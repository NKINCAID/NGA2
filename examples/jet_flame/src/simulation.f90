!> Various definitions and tools for running an NGA2 simulation
module simulation
   use precision,         only: WP
   use geometry,          only: cfg
   use ddadi_class,       only: ddadi
   use hypre_str_class,   only: hypre_str
   use lowmach_class,     only: lowmach
   use vdscalar_class,    only: vdscalar
   use sgsmodel_class,    only: sgsmodel
   use flamelet_class,    only: flamelet
   use timetracker_class, only: timetracker
   use ensight_class,     only: ensight
   use event_class,       only: event
   use monitor_class,     only: monitor
   implicit none
   private

   !> Single low Mach flow solver and scalar solver and corresponding time tracker
   type(hypre_str),   public :: ps
   type(ddadi),       public :: vs,ss
   type(lowmach),     public :: fs
   type(vdscalar),    public :: sc    
   type(timetracker), public :: time
   type(sgsmodel),    public :: sgs
   type(flamelet),    public :: flm

   !> Ensight postprocessing
   type(ensight) :: ens_out
   type(event)   :: ens_evt

   !> Simulation monitor file
   type(monitor) :: mfile,cflfile,consfile

   public :: simulation_init,simulation_run,simulation_final

   !> Private work arrays
   real(WP), dimension(:,:,:),     allocatable :: resU,resV,resW,resSC
   real(WP), dimension(:,:,:),     allocatable :: Ui,Vi,Wi
   real(WP), dimension(:,:,:,:,:), allocatable :: gradU

   !> Inlet
   real(WP) :: Z_jet,Z_cof
   real(WP) :: D_jet,D_cof
   real(WP) :: U_jet,U_cof

   !> Integral of pressure residual
   real(WP) :: int_RP=0.0_WP

   !> Combustion
   integer  :: nfilter,ncells
   logical  :: rho_limiter
   real(WP) :: rho_min,rho_max
   real(WP), dimension(:,:,:), allocatable :: drho
   real(WP), dimension(:,:,:), allocatable :: ZgradMagSq
   real(WP), dimension(:,:,:), allocatable :: Y_OH

   !> Turbulence
   real(WP) :: SchmidtSGS

contains


   !> Function that localizes y- boundary
   function ym_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (j.eq.pg%jmin) isIn=.true.
   end function ym_locator


   !> Function that localizes y+ boundary
   function yp_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (j.eq.pg%jmax+1) isIn=.true.
   end function yp_locator


   !> Function that localizes z- boundary
   function zm_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (k.eq.pg%kmin) isIn=.true.
   end function zm_locator


   !> Function that localizes z+ boundary
   function zp_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (k.eq.pg%kmax+1) isIn=.true.
   end function zp_locator


   !> Function that localizes the x+ boundary
   function xp_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imax+1) isIn=.true.
   end function xp_locator


   !> Function that localizes jet at -x
   function jet(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      real(WP) :: radius
      logical :: isIn
      isIn=.false.
      ! Jet in yz plane
      radius=norm2([pg%ym(j),pg%zm(k)]-[0.0_WP,0.0_WP])
      if (radius.le.0.5_WP*D_jet.and.i.eq.pg%imin) isIn=.true.
   end function jet


   !> Function that localizes coflow at -x
   function coflow(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      real(WP) :: radius
      logical :: isIn
      isIn=.false.
      ! Coflow in yz plane
      radius=norm2([pg%ym(j),pg%zm(k)]-[0.0_WP,0.0_WP])
      if (radius.gt.0.5_WP*D_jet.and.radius.le.0.5_WP*D_cof.and.i.eq.pg%imin) isIn=.true.
   end function coflow


   !> Function that localizes jet at -x
   function jetsc(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      real(WP) :: radius
      logical :: isIn
      isIn=.false.
      ! Jet in yz plane
      radius=norm2([pg%ym(j),pg%zm(k)]-[0.0_WP,0.0_WP])
      if (radius.le.0.5_WP*D_jet.and.i.eq.pg%imin-1) isIn=.true.
   end function jetsc


   !> Function that localizes coflow at -x
   function coflowsc(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      real(WP) :: radius
      logical :: isIn
      isIn=.false.
      ! Coflow in yz plane
      radius=norm2([pg%ym(j),pg%zm(k)]-[0.0_WP,0.0_WP])
      if (radius.gt.0.5_WP*D_jet.and.radius.le.0.5_WP*D_cof.and.i.eq.pg%imin-1) isIn=.true.
   end function coflowsc


   !> Spatially filter density changes
   subroutine filter_drho()
      use mpi_f08 , only: MPI_SUM,MPI_ALLREDUCE
      use parallel, only: MPI_REAL_WP
      implicit none
      integer  :: i,j,k,ierr,n
      real(WP) :: vol,my_vol,volume,my_drho,my_drho2,drho_mean,drho2_mean,sdt
      real(WP), dimension(sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_) :: Fdrho

      ! Find the deviation from mean
      my_vol  =0.0_WP
      my_drho =0.0_WP
      my_drho2=0.0_WP
      do k=sc%cfg%kmin_,sc%cfg%kmax_
         do j=sc%cfg%jmin_,sc%cfg%jmax_
            do i=sc%cfg%imin_,sc%cfg%imax_
               vol=sc%cfg%dx(i)*sc%cfg%dy(j)*sc%cfg%dz(k)*sc%cfg%VF(i,j,k)
               my_vol=my_vol+vol
               my_drho=my_drho+vol*drho(i,j,k)
               my_drho2=my_drho2+vol*drho(i,j,k)**2
            end do
         end do
      end do
      call MPI_ALLREDUCE(my_vol,volume,1,MPI_REAL_WP,MPI_SUM,sc%cfg%comm,ierr)
      call MPI_ALLREDUCE(my_drho,drho_mean,1,MPI_REAL_WP,MPI_SUM,sc%cfg%comm,ierr)
      call MPI_ALLREDUCE(my_drho2,drho2_mean,1,MPI_REAL_WP,MPI_SUM,sc%cfg%comm,ierr)
      drho_mean =drho_mean /volume
      drho2_mean=drho2_mean/volume
      sdt=sqrt(abs(drho2_mean-drho_mean**2))

      ! Perform the filtering
      do n=1,nfilter
         Fdrho=drho
         do k=sc%cfg%kmin_,sc%cfg%kmax_
            do j=sc%cfg%jmin_,sc%cfg%jmax_
               do i=sc%cfg%imin_,sc%cfg%imax_
                  if ((fs%mask(i,j,k).eq.0).and.(abs(drho(i,j,k)-drho_mean).gt.5.0_WP*sdt)) then
                     Fdrho(i,j,k)=sum(sgs%filtern(:,:,:,i,j,k)*drho(i-1:i+1,j-1:j+1,k-1:k+1))
                  end if
               end do
            end do
         end do
         drho(sc%cfg%imin_:sc%cfg%imax_,sc%cfg%jmin_:sc%cfg%jmax_,sc%cfg%kmin_:sc%cfg%kmax_)=Fdrho(sc%cfg%imin_:sc%cfg%imax_,sc%cfg%jmin_:sc%cfg%jmax_,sc%cfg%kmin_:sc%cfg%kmax_)
         ! Sync drho
         call sc%cfg%sync(drho)
      end do

   end subroutine filter_drho


   !> Initialization of problem solver
   subroutine simulation_init
      use param, only: param_read
      implicit none


      ! Read in inlet information
      call param_read('Z jet',Z_jet)
      call param_read('D jet',D_jet)
      call param_read('U jet',U_jet)
      call param_read('Z coflow',Z_cof)
      call param_read('D coflow',D_cof)
      call param_read('U coflow',U_cof)


      ! Create a low-Mach flow solver with bconds
      create_velocity_solver: block
         use hypre_str_class, only: pcg_pfmg2
         use lowmach_class,   only: dirichlet,clipped_neumann,slip
         real(WP) :: visc
         ! Create flow solver
         fs=lowmach(cfg=cfg,name='Variable density low Mach NS')
         ! Define jet and coflow boundary conditions
         call fs%add_bcond(name='jet'   ,type=dirichlet,face='x',dir=-1,canCorrect=.false.,locator=jet   )
         call fs%add_bcond(name='coflow',type=dirichlet,face='x',dir=-1,canCorrect=.false.,locator=coflow)
         ! Outflow on the right
         call fs%add_bcond(name='outflow',type=clipped_neumann,face='x',dir=+1,canCorrect=.true.,locator=xp_locator)
         ! Configure pressure solver
         ps=hypre_str(cfg=cfg,name='Pressure',method=pcg_pfmg2,nst=7)
         ps%maxlevel=18
         call param_read('Pressure iteration',ps%maxit)
         call param_read('Pressure tolerance',ps%rcvg)
         ! Configure implicit velocity solver
         vs=ddadi(cfg=cfg,name='Velocity',nst=7)
         ! Setup the solver
         call fs%setup(pressure_solver=ps,implicit_solver=vs)
      end block create_velocity_solver


      ! Create a scalar solver
      create_scalar: block
         use vdscalar_class, only: dirichlet,neumann,quick
         real(WP) :: diffusivity
         ! Create scalar solver
         sc=vdscalar(cfg=cfg,scheme=quick,name='MixFrac')
         ! Define jet and coflow boundary conditions
         call sc%add_bcond(name='jet'   ,type=dirichlet,locator=jetsc   )
         call sc%add_bcond(name='coflow',type=dirichlet,locator=coflowsc)
         ! Outflow on the right
         call sc%add_bcond(name='outflow',type=neumann,locator=xp_locator,dir='+x')
         ! Configure implicit scalar solver
         ss=ddadi(cfg=cfg,name='Scalar',nst=13)
         ! Setup the solver
         call sc%setup(implicit_solver=ss)
      end block create_scalar


      ! Create a combustion model
      create_combustion_model: block
         use string,            only: str_medium
         use flameletLib_class, only: sfm
         use tabulation,        only: tabulate_flamelet
         character(len=str_medium) :: chfname
         logical :: mkchmtbl
         ! Create the chemtable
         call param_read('Chemtable file name',chfname)
         call param_read('Create chemtable',mkchmtbl)
         if (cfg%amRoot) then
            if (mkchmtbl) call tabulate_flamelet(model=sfm,chfname=chfname)
         end if
         ! Construct the flamelet object
         flm=flamelet(cfg=cfg,flmModel=sfm,tablefile=trim(chfname),name='Steady flamelet model')
         call flm%print()
         ! Read in control parameters for density
         call param_read('Filtering levels',nfilter)
         call param_read('Density limiter',rho_limiter)
      end block create_combustion_model


      ! Create a SGS model
      create_sgs: block
         sgs=sgsmodel(cfg=fs%cfg,umask=fs%umask,vmask=fs%vmask,wmask=fs%wmask)
         call param_read('SGS Schmidt number',SchmidtSGS)
      end block create_sgs


      ! Allocate work arrays
      allocate_work_arrays: block
         ! Flow solver
         allocate(resU(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(resV(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(resW(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(Ui  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(Vi  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(Wi  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         ! Scalar solver
         allocate(resSC(sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_))
         ! Combustion
         allocate(drho      (sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_)); drho=0.0_WP
         allocate(ZgradMagSq(sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_)); ZgradMagSq=0.0_WP
         allocate(Y_OH      (sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_)); Y_OH=0.0_WP
         ! Turbulence
         allocate(gradU(1:3,1:3,cfg%imino_:cfg%imaxo_,cfg%jmino_:cfg%jmaxo_,cfg%kmino_:cfg%kmaxo_))
      end block allocate_work_arrays


      ! Initialize time tracker with subiterations
      initialize_timetracker: block
         time=timetracker(amRoot=fs%cfg%amRoot,name='jet_flame')
         call param_read('Max timestep size',time%dtmax)
         call param_read('Max cfl number',time%cflmax)
         call param_read('Max time',time%tmax)
         call param_read('Sub iterations',time%itmax)
         time%dt=time%dtmax
      end block initialize_timetracker


      ! Initialize our mixture fraction field
      initialize_scalar: block
         use vdscalar_class, only: bcond
         integer :: n,i,j,k
         type(bcond), pointer :: mybc
         ! Zero initial field
         sc%SC=0.0_WP
         ! Apply BCs
         call sc%get_bcond('jet',mybc)
         do n=1,mybc%itr%no_
            i=mybc%itr%map(1,n); j=mybc%itr%map(2,n); k=mybc%itr%map(3,n)
            sc%SC(i,j,k)=2.0_WP*Z_jet-sc%SC(i+1,j,k)
         end do
         call sc%get_bcond('coflow',mybc)
         do n=1,mybc%itr%no_
            i=mybc%itr%map(1,n); j=mybc%itr%map(2,n); k=mybc%itr%map(3,n)
            sc%SC(i,j,k)=2.0_WP*Z_cof-sc%SC(i+1,j,k)
         end do
      end block initialize_scalar


      initialize_combustion: block
         ! Number of cells
         ncells=cfg%nxo_*cfg%nyo_*cfg%nzo_
         ! Lookup viscosity
         call flm%chmtbl%lookup('viscosity',fs%visc,sc%SC,flm%Zvar,flm%chi,ncells)
         ! Lookup diffusivity
         call flm%chmtbl%lookup('diffusivity',sc%diff,sc%SC,flm%Zvar,flm%chi,ncells)
         ! Mixture fraction gradient
         ZgradMagSq=sc%grad_mag_sq(itpr_x=fs%itpr_x,itpr_y=fs%itpr_y,itpr_z=fs%itpr_z)
         ! Mixture fraction variance
         call flm%get_Zvar(delta=sgs%delta,ZgradMagSq=ZgradMagSq,Z=sc%SC)
         ! Lookup density
         call flm%chmtbl%lookup('density',sc%rho,sc%SC,flm%Zvar,flm%chi,ncells)
         ! Find the min and max for rho
         call flm%chmtbl%lookup_max('density',rho_max)
         call flm%chmtbl%lookup_min('density',rho_min)
      end block initialize_combustion


      ! Initialize our velocity field
      initialize_velocity: block
         use lowmach_class, only: bcond
         integer :: n,i,j,k
         type(bcond), pointer :: mybc
         ! Zero initial field
         fs%U=0.0_WP; fs%V=0.0_WP; fs%W=0.0_WP
         ! Apply all boundary conditions
         call fs%get_bcond('jet',mybc)
         do n=1,mybc%itr%no_
            i=mybc%itr%map(1,n); j=mybc%itr%map(2,n); k=mybc%itr%map(3,n)
            fs%U(i,j,k)   =U_jet
         end do
         call fs%get_bcond('coflow',mybc)
         do n=1,mybc%itr%no_
            i=mybc%itr%map(1,n); j=mybc%itr%map(2,n); k=mybc%itr%map(3,n)
            fs%U(i,j,k)   =U_cof
         end do
         ! Set density from scalar
         fs%rho=sc%rho
         ! Form momentum
         call fs%rho_multiply()
         call fs%apply_bcond(time%t,time%dt)
         call fs%interp_vel(Ui,Vi,Wi)
         resSC=0.0_WP
         call fs%get_div(drhodt=resSC)
         ! Compute MFR through all boundary conditions
         call fs%get_mfr()
      end block initialize_velocity


      ! Add Ensight output
      create_ensight: block
         ! Create Ensight output from cfg
         ens_out=ensight(cfg=cfg,name='jet_flame')
         ! Create event for Ensight output
         ens_evt=event(time=time,name='Ensight output')
         call param_read('Ensight output period',ens_evt%tper)
         ! Add variables to output
         call ens_out%add_scalar('pressure',fs%P)
         call ens_out%add_vector('velocity',Ui,Vi,Wi)
         call ens_out%add_scalar('divergence',fs%div)
         call ens_out%add_scalar('density',sc%rho)
         call ens_out%add_scalar('mixfrac',sc%SC)
         call ens_out%add_scalar('chi',flm%chi)
         call ens_out%add_scalar('Zvar',flm%Zvar)
         call ens_out%add_scalar('Y_OH',Y_OH)
         ! Output to ensight
         if (ens_evt%occurs()) call ens_out%write_data(time%t)
      end block create_ensight


      ! Create a monitor file
      create_monitor: block
         ! Prepare some info about fields
         call fs%get_cfl(time%dt,time%cfl)
         call fs%get_max()
         call sc%get_max()
         call sc%get_int()
         ! Create simulation monitor
         mfile=monitor(fs%cfg%amRoot,'simulation')
         call mfile%add_column(time%n,'Timestep number')
         call mfile%add_column(time%t,'Time')
         call mfile%add_column(time%dt,'Timestep size')
         call mfile%add_column(time%cfl,'Maximum CFL')
         call mfile%add_column(fs%Umax,'Umax')
         call mfile%add_column(fs%Vmax,'Vmax')
         call mfile%add_column(fs%Wmax,'Wmax')
         call mfile%add_column(fs%Pmax,'Pmax')
         call mfile%add_column(sc%SCmax,'Zmax')
         call mfile%add_column(sc%SCmin,'Zmin')
         call mfile%add_column(sc%rhomax,'RHOmax')
         call mfile%add_column(sc%rhomin,'RHOmin')
         call mfile%add_column(int_RP,'Int(RP)')
         call mfile%add_column(fs%divmax,'Maximum divergence')
         call mfile%add_column(fs%psolv%it,'Pressure iteration')
         call mfile%add_column(fs%psolv%rerr,'Pressure error')
         call mfile%write()
         ! Create CFL monitor
         cflfile=monitor(fs%cfg%amRoot,'cfl')
         call cflfile%add_column(time%n,'Timestep number')
         call cflfile%add_column(time%t,'Time')
         call cflfile%add_column(fs%CFLc_x,'Convective xCFL')
         call cflfile%add_column(fs%CFLc_y,'Convective yCFL')
         call cflfile%add_column(fs%CFLc_z,'Convective zCFL')
         call cflfile%add_column(fs%CFLv_x,'Viscous xCFL')
         call cflfile%add_column(fs%CFLv_y,'Viscous yCFL')
         call cflfile%add_column(fs%CFLv_z,'Viscous zCFL')
         call cflfile%write()
         ! Create conservation monitor
         consfile=monitor(fs%cfg%amRoot,'conservation')
         call consfile%add_column(time%n,'Timestep number')
         call consfile%add_column(time%t,'Time')
         call consfile%add_column(sc%SCint,'SC integral')
         call consfile%add_column(sc%rhoint,'RHO integral')
         call consfile%add_column(sc%rhoSCint,'rhoSC integral')
         call consfile%write()
      end block create_monitor


   end subroutine simulation_init


   !> Perform an NGA2 simulation
   subroutine simulation_run
      implicit none


      ! Perform time integration
      do while (.not.time%done())

         ! Increment time
         call fs%get_cfl(time%dt,time%cfl)
         call time%adjust_dt()
         call time%increment()

         ! Remember old scalar
         sc%rhoold=sc%rho
         sc%SCold =sc%SC

         ! Remember old velocity and momentum
         fs%rhoold=fs%rho
         fs%Uold=fs%U; fs%rhoUold=fs%rhoU
         fs%Vold=fs%V; fs%rhoVold=fs%rhoV
         fs%Wold=fs%W; fs%rhoWold=fs%rhoW

         ! Lookup viscosity
         call flm%chmtbl%lookup('viscosity',fs%visc,sc%SC,flm%Zvar,flm%chi,ncells)
         ! Lookup diffusivity
         call flm%chmtbl%lookup('diffusivity',sc%diff,sc%SC,flm%Zvar,flm%chi,ncells)
         ! Mixture fraction gradient
         ZgradMagSq=sc%grad_mag_sq(itpr_x=fs%itpr_x,itpr_y=fs%itpr_y,itpr_z=fs%itpr_z)
         ! Mixture fraction variance
         call flm%get_Zvar(delta=sgs%delta,ZgradMagSq=ZgradMagSq,Z=sc%SC)
         ! Scalar dissipation rate
         call flm%get_chi(mueff=fs%visc,rho=sc%rho,ZgradMagSq=ZgradMagSq)
         
         ! Turbulence modeling
         sgs_modeling: block
            use sgsmodel_class, only: vreman
            resU=fs%rho
            call fs%get_gradu(gradU)
            call sgs%get_visc(type=vreman,dt=time%dtold,rho=resU,gradu=gradU)
            fs%visc=fs%visc+sgs%visc
            sc%diff=sc%diff+sgs%visc/SchmidtSGS
         end block sgs_modeling

         ! Perform sub-iterations
         do while (time%it.le.time%itmax)

            ! ============= SCALAR SOLVER ============= !
            ! Build mid-time scalar
            sc%SC=0.5_WP*(sc%SC+sc%SCold)

            ! Explicit calculation of drhoSC/dt from scalar equation
            call sc%get_drhoSCdt(resSC,fs%rhoU,fs%rhoV,fs%rhoW)

            ! Assemble explicit residual
            resSC=time%dt*resSC-(2.0_WP*sc%rho*sc%SC-(sc%rho+sc%rhoold)*sc%SCold)

            ! Form implicit residual
            call sc%solve_implicit(time%dt,resSC,fs%rhoU,fs%rhoV,fs%rhoW)

            ! Apply these residuals
            sc%SC=2.0_WP*sc%SC-sc%SCold+resSC

            ! Apply all boundary conditions on the resulting field
            call sc%apply_bcond(time%t,time%dt)
            dirichlet_scalar: block
               use vdscalar_class, only: bcond
               type(bcond), pointer :: mybc
               integer :: n,i,j,k
               call sc%get_bcond('jet',mybc)
               do n=1,mybc%itr%no_
                  i=mybc%itr%map(1,n); j=mybc%itr%map(2,n); k=mybc%itr%map(3,n)
                  sc%SC(i,j,k)=2.0_WP*Z_jet-sc%SC(i+1,j,k)
               end do
               call sc%get_bcond('coflow',mybc)
               do n=1,mybc%itr%no_
                  i=mybc%itr%map(1,n); j=mybc%itr%map(2,n); k=mybc%itr%map(3,n)
                  sc%SC(i,j,k)=2.0_WP*Z_cof-sc%SC(i+1,j,k)
               end do
            end block dirichlet_scalar
            ! ========================================= !

            ! ============ UPDATE DENSITY ============ !
            ! Lookup density
            call flm%chmtbl%lookup('density',sc%rho,sc%SC,flm%Zvar,flm%chi,ncells)
            ! Smooth density
            if (nfilter.gt.0) then
               ! Compute density changes
               drho=sc%rho-sc%rhoold
               ! Filter density changes
               call filter_drho()
               ! Recompute new density
               sc%rho=sc%rhoold+drho
               ! Bound density
               if (rho_limiter) then
                  sc%rho=max(min(sc%rho,rho_max),rho_min)
                  call sc%cfg%sync(sc%rho)
               end if
            end if
            ! ========================================= !

            ! ============ VELOCITY SOLVER ============ !
            ! Build n+1 density
            fs%rho=0.5_WP*(sc%rho+sc%rhoold)

            ! Build mid-time velocity and momentum
            fs%U=0.5_WP*(fs%U+fs%Uold); fs%rhoU=0.5_WP*(fs%rhoU+fs%rhoUold)
            fs%V=0.5_WP*(fs%V+fs%Vold); fs%rhoV=0.5_WP*(fs%rhoV+fs%rhoVold)
            fs%W=0.5_WP*(fs%W+fs%Wold); fs%rhoW=0.5_WP*(fs%rhoW+fs%rhoWold)

            ! Explicit calculation of drho*u/dt from NS
            call fs%get_dmomdt(resU,resV,resW)

            ! Assemble explicit residual
            resU=time%dtmid*resU-(2.0_WP*fs%rhoU-2.0_WP*fs%rhoUold)
            resV=time%dtmid*resV-(2.0_WP*fs%rhoV-2.0_WP*fs%rhoVold)
            resW=time%dtmid*resW-(2.0_WP*fs%rhoW-2.0_WP*fs%rhoWold)

            ! Form implicit residuals
            call fs%solve_implicit(time%dtmid,resU,resV,resW)

            ! Apply these residuals
            fs%U=2.0_WP*fs%U-fs%Uold+resU
            fs%V=2.0_WP*fs%V-fs%Vold+resV
            fs%W=2.0_WP*fs%W-fs%Wold+resW

            ! Apply boundary conditions and update momentum
            call fs%apply_bcond(time%tmid,time%dtmid)
            call fs%rho_multiply()
            call fs%apply_bcond(time%tmid,time%dtmid)
            dirichlet_velocity: block
               use lowmach_class, only: bcond
               type(bcond), pointer :: mybc
               integer :: n,i,j,k
               call fs%get_bcond('jet',mybc)
               do n=1,mybc%itr%no_
                  i=mybc%itr%map(1,n); j=mybc%itr%map(2,n); k=mybc%itr%map(3,n)
                  fs%U(i,j,k)=U_jet
               end do
               call fs%get_bcond('coflow',mybc)
               do n=1,mybc%itr%no_
                  i=mybc%itr%map(1,n); j=mybc%itr%map(2,n); k=mybc%itr%map(3,n)
                  fs%U(i,j,k)=U_cof
               end do
               call fs%rho_multiply()
            end block dirichlet_velocity

            ! Solve Poisson equation
            call sc%get_drhodt(dt=time%dt,drhodt=resSC)
            call fs%correct_mfr(drhodt=resSC)
            call fs%get_div(drhodt=resSC)
            fs%psolv%rhs=-fs%cfg%vol*fs%div/time%dtmid
            call cfg%integrate(A=fs%psolv%rhs,integral=int_RP)
            fs%psolv%sol=0.0_WP
            call fs%psolv%solve()
            call fs%shift_p(fs%psolv%sol)

            ! Correct momentum and rebuild velocity
            call fs%get_pgrad(fs%psolv%sol,resU,resV,resW)
            fs%P=fs%P+fs%psolv%sol
            fs%rhoU=fs%rhoU-time%dtmid*resU
            fs%rhoV=fs%rhoV-time%dtmid*resV
            fs%rhoW=fs%rhoW-time%dtmid*resW
            call fs%rho_divide
            ! ========================================= !

            ! Increment sub-iteration counter
            time%it=time%it+1

         end do

         ! Recompute interpolated velocity and divergence
         call fs%interp_vel(Ui,Vi,Wi)
         call sc%get_drhodt(dt=time%dt,drhodt=resSC)
         call fs%get_div(drhodt=resSC)

         ! Combustion post-process
         call flm%chmtbl%lookup('Y_OH',Y_OH,sc%SC,flm%Zvar,flm%chi,ncells)

         ! Output to ensight
         if (ens_evt%occurs()) call ens_out%write_data(time%t)

         ! Perform and output monitoring
         call fs%get_max()
         call sc%get_max()
         call sc%get_int()
         call mfile%write()
         call cflfile%write()
         call consfile%write()

      end do

      
   end subroutine simulation_run


   !> Finalize the NGA2 simulation
   subroutine simulation_final
      implicit none


      ! Get rid of all objects - need destructors
      ! monitor
      ! ensight
      ! bcond
      ! timetracker

      ! Deallocate work arrays
      deallocate(resSC,resU,resV,resW,Ui,Vi,Wi,drho,gradU,ZgradMagSq)


   end subroutine simulation_final


end module simulation
