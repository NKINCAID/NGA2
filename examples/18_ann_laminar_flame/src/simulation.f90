!> Various definitions and tools for running an NGA2 simulation
module simulation
   use precision,           only: WP
   use string,              only: str_medium
   use geometry,            only: cfg,Lx
   use hypre_str_class,     only: hypre_str
   use ddadi_class, only: ddadi
   use lowmach_class,       only: lowmach
   use multivdscalar_class, only: multivdscalar
   use aencodernet_class,   only: aencodernet
   use chsourcenet_class,   only: chsourcenet
   use trnsportnet_class,   only: trnsportnet
   use timetracker_class,   only: timetracker
   use ensight_class,       only: ensight
   use event_class,         only: event
   use monitor_class,       only: monitor

   implicit none
   private

   !> Single phase low Mach flow solver, scalar solver, and corresponding time trackerf
   type(hypre_str),     public :: ps
   type(lowmach),       public :: fs
   type(multivdscalar), public :: sc
   type(timetracker),   public :: time
   type(ddadi), public :: vs, ss


   !> Artificial neural networks
   type(aencodernet) :: aen
   type(chsourcenet) :: csn
   type(trnsportnet) :: trn
   integer :: nY_sub

   !> Chemistry
   character(len=str_medium), dimension(:), allocatable :: spec_name

   !> Ensight postprocessing
   type(ensight) :: ens_out
   type(event)   :: ens_evt

   !> Simulation monitor file
   type(monitor) :: mfile,cflfile,statfile

   !> Private work arrays
   real(WP), dimension(:,:,:,:), allocatable :: resSC,SCtmp,SC_src    !< Scalar solver arrays
   logical , dimension(:,:,:,:), allocatable :: bqflag                !< Flag for bquick scheme
   real(WP), dimension(:,:,:),   allocatable :: resU,resV,resW,resRHO !< Residuals
   real(WP), dimension(:,:,:),   allocatable :: Ui,Vi,Wi              !< Cell-centred velocity components
   real(WP), dimension(:,:,:),   allocatable :: SC_init,U_init,V_init !< Initial condition for scalar and velocity fields
   real(WP), dimension(:,:,:),   allocatable :: T                     !< Temperature
   real(WP), dimension(:,:,:),   allocatable :: tmpfield              !< Temporary field for statistics
   real(WP), dimension(:),       allocatable :: Y_sub                 !< Mass fraction of species that are used by networks
   real(WP), dimension(:),       allocatable :: Y_init                !< Initial mass fractions
   real(WP), dimension(:),       allocatable :: Yu                !< Initial mass fractions
   real(WP), dimension(:),       allocatable :: Yb                !< Initial mass fractions

   real(WP), dimension(:),       allocatable :: hY                    !< Enthalpy and mass fractions of sub species
   real(WP), dimension(:),       allocatable :: TYS                   !< Temperature, mass fractions, and source terms of sub species
   real(WP), dimension(4)                    :: trnprop,trnprop_tmp   !< Transport properties: Temperature, logarithm of density, viscosity, and scalar diffusivity

   !> Post process for species mass fractions
   integer :: n_Y                                                     !< Number of output species
   character(len=str_medium), dimension(:), allocatable :: Y_name     !< Names of output species
   real(WP), dimension(:,:,:,:), allocatable            :: Y          !< Output species mass fractions (must exist in Y_sub)
   integer, dimension(:), allocatable                   :: iY_in_sub  !< The indices of post-process species in the sub-species list

   !> Scalar and species indices
   integer :: isc,iY

   !> Statistics
   real(WP) :: rhomean,Tmean,Trms,Tmax, Uin

   !> Simulation sub-routines
   public :: simulation_init,simulation_run,simulation_final

      !> Time stepping
   character(len=str_medium) :: time_stepping

contains


   !> Function that localizes y- boundary
   function ym_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (j.eq.pg%jmin) isIn=.true.
   end function ym_locator


   !> Function that localizes y+ boundary
   function yp_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (j.eq.pg%jmax+1) isIn=.true.
   end function yp_locator


   !> Function that localizes the x+ boundary
   function xm_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imin) isIn=.true.
   end function xm_locator


   !> Function that localizes the x+ boundary
   function xp_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imax+1) isIn=.true.
   end function xp_locator


   !> Function that localizes y- boundary
   function ym_locator_sc(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (j.eq.pg%jmin-1) isIn=.true.
   end function ym_locator_sc


   !> Function that localizes y+ boundary
   function yp_locator_sc(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (j.eq.pg%jmax+1) isIn=.true.
   end function yp_locator_sc


   !> Function that localizes jet at -x
   function xm_locator_sc(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imin-1) isIn=.true.
   end function xm_locator_sc


   !> Function that localizes the right domain boundary
   function xp_locator_sc(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imax+1) isIn=.true.
   end function xp_locator_sc

      !> Function that localizes jet at -x
   function xmsc_locator(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      real(WP) :: radius
      logical :: isIn
      isIn = .false.
      ! Jet in yz plane
      if (i .eq. pg%imin - 1) isIn = .true.
   end function xmsc_locator


   !> Initialization of problem solver
   subroutine simulation_init
      use param, only: param_read,param_getsize,param_exists
      use messager, only: die
      use fcmech, only: nspec
      implicit none
      real(WP) :: L_buffer


      ! Read-in the inputs
      call param_read('Buffer region length',L_buffer)
      n_Y=param_getsize('Ensight output species')

      
      ! Create a low-Mach flow solver with bconds
      create_velocity_solver: block
         use hypre_str_class, only: pcg_pfmg,smg
         use lowmach_class,   only: dirichlet,neumann
         real(WP) :: visc
         ! Create flow solver
         fs=lowmach(cfg=cfg,name='Variable density low Mach NS')
         ! Boundary conditions
         ! call fs%add_bcond(name='ym_outflow',type=neumann,face='y',dir=-1,canCorrect=.True.,locator=ym_locator)
         ! call fs%add_bcond(name='yp_outflow',type=neumann,face='y',dir=+1,canCorrect=.True.,locator=yp_locator)
         call fs%add_bcond(name='inflow', type=dirichlet, face='x', dir=-1, canCorrect=.false., locator=xm_locator)

         ! call fs%add_bcond(name='xm_outflow',type=neumann,face='x',dir=-1,canCorrect=.True.,locator=xm_locator)
         call fs%add_bcond(name='xp_outflow',type=neumann,face='x',dir=+1,canCorrect=.True.,locator=xp_locator)
         ! Configure pressure solver
         ps = hypre_str(cfg=cfg, name='Pressure', method=smg, nst=7)
         ps%maxlevel = 12
         call param_read('Pressure iteration',ps%maxit)
         call param_read('Pressure tolerance',ps%rcvg)
         ! Setup the flow solver
         ! call fs%setup(pressure_solver=ps)
         vs = ddadi(cfg=cfg, name='Velocity', nst=7)
         call fs%setup(pressure_solver=ps, implicit_solver=vs)


      end block create_velocity_solver


      ! Create neural networks
      create_ann: block
         use string, only: str_medium
         character(len=str_medium) :: aenfname,csnfname,trnfname
         ! Read-in the data file names
         call param_read('Auto encoder'        ,aenfname)
         call param_read('Chemical source'     ,csnfname)
         call param_read('Transport properties',trnfname)
         ! The auto encoder network
         aen=aencodernet(cfg=cfg,fdata=aenfname,name='Auto encoder network')
         call aen%print()
         ! The chemical source network
         csn=chsourcenet(cfg=cfg,fdata=csnfname,name='Chemical source network')
         call csn%print()
         ! The transport properties network
         trn=trnsportnet(cfg=cfg,fdata=trnfname,name='Transport properties network')
         call trn%print()
         ! Species sub-array size
         nY_sub=size(aen%vectors(aen%ivec_spec_inds)%vector)
      end block create_ann


      ! Create a multi scalar solver
      create_scalar_solver: block
         use multivdscalar_class, only: dirichlet,neumann,quick,bquick
         ! Create scalar solver
         sc=multivdscalar(cfg=cfg,scheme=bquick,nscalar=aen%nvar,name='Variable density multi scalar')
         ! Boundary conditions
         ! call sc%add_bcond(name='ym_outflow',type=neumann,locator=ym_locator_sc,dir='-y')
         ! call sc%add_bcond(name='yp_outflow',type=neumann,locator=yp_locator_sc,dir='+y')
         ! call sc%add_bcond(name='xm_outflow',type=neumann,locator=xm_locator_sc,dir='-x')
         ! call sc%add_bcond(name='xp_outflow',type=neumann,locator=xp_locator_sc,dir='+x')

         call sc%add_bcond(name='inflow', type=dirichlet, locator=xmsc_locator)
         call sc%add_bcond(name='xp_outflow',type=neumann,locator=xp_locator_sc,dir='+x')

         ss = ddadi(cfg=cfg, name='Scalar', nst=13)


         ! Setup the solver
         ! call sc%setup()
         call sc%setup(implicit_solver=ss)

      end block create_scalar_solver


      ! Allocate work arrays
      allocate_work_arrays: block
         ! Flow solver
         allocate(resU  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(resV  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(resW  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(resRHO(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(Ui    (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(Vi    (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(Wi    (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         ! Scalar solver
         allocate(resSC (sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_,sc%nscalar))
         allocate(SCtmp (sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_,sc%nscalar))
         allocate(bqflag(cfg%imino_:cfg%imaxo_,cfg%jmino_:cfg%jmaxo_,cfg%kmino_:cfg%kmaxo_,sc%nscalar))
         allocate(SC_src(sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_,aen%nvar)); SC_src=0.0_WP
         ! Temporary arrays
         allocate(SC_init (sc%cfg%imin:sc%cfg%imax,sc%cfg%jmin:sc%cfg%jmax,sc%cfg%kmin:sc%cfg%kmax))
         allocate(U_init  (fs%cfg%imin:fs%cfg%imax,fs%cfg%jmin:fs%cfg%jmax,fs%cfg%kmin:fs%cfg%kmax))
         allocate(V_init  (fs%cfg%imin:fs%cfg%imax,fs%cfg%jmin:fs%cfg%jmax,fs%cfg%kmin:fs%cfg%kmax))
         allocate(tmpfield(sc%cfg%imin:sc%cfg%imax,sc%cfg%jmin:sc%cfg%jmax,sc%cfg%kmin:sc%cfg%kmax)); tmpfield=0.0_WP
         ! Combustion
         allocate(T(sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_))
         allocate(Y(sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_,n_Y))
         allocate(Y_sub(nY_sub))  ; Y_sub=0.0_WP
         allocate(hY(nY_sub+1))   ; hY=0.0_WP
         allocate(TYS(2*nY_sub+1)); TYS=0.0_WP
         allocate(iY_in_sub(n_Y)) ; iY_in_sub=0
         allocate(Y_init(nspec))  ; Y_init=0.0_WP
         allocate(Yu(nspec))  ; Yu=0.0_WP
         allocate(Yb(nspec))  ; Yb=0.0_WP

         allocate(spec_name(nspec))
         allocate(Y_name(n_Y))
      end block allocate_work_arrays


      ! Initialize time tracker with 2 subiterations
      initialize_timetracker: block
         time=timetracker(amRoot=fs%cfg%amRoot,name='reactor2d_ann')
         call param_read('Max timestep size',time%dtmax)
         call param_read('Max cfl number',time%cflmax)
         call param_read('Max time',time%tmax)
         call param_read('Inner iterations',time%itmax)
         call param_read('Time stepping',time_stepping)
         if ((trim(time_stepping).ne.'explicit').and.(trim(time_stepping).ne.'implicit')) call die('Time stepping must be either explicit or implicit.')

         time%dt=time%dtmax
      end block initialize_timetracker


      ! Initialize species
      initialize_species: block
         use string, only: str_long
         use, intrinsic :: iso_fortran_env, only: output_unit
         integer  :: iY_sub,ispec
         character(len=str_long) :: errmsg

         ! Get all the species names in the mechanism
         call fcmech_get_speciesnames(spec_name)
         ! Read-in the names of post processed species
         call param_read('Ensight output species',Y_name)

         ! Print species information
         if (cfg%amRoot) then

            ! Species info
            write(output_unit,'(" >  Species information")')

            ! Sub-array used in ANN
            write(output_unit,'(" Sub-array species used in the ann:")',advance='No')
            do iY_sub=1,nY_sub
               write(output_unit,'(a)',advance='No') ' '//trim(spec_name(int(aen%vectors(aen%ivec_spec_inds)%vector(iY_sub))))
            end do
            write(output_unit,'(" ")')

            ! Post processed species
            write(output_unit,'(" Post processed species:")',advance='No')
            do iY=1,n_Y
               write(output_unit,'(a)',advance='No') ' '//trim(Y_name(iY))
            end do
            write(output_unit,'(" ")')

         end if

         ! Localize post processed species inside the ann species sub-array
         do iY=1,n_Y
            do iY_sub=1,nY_sub
               ! Global species index
               ispec=aen%vectors(aen%ivec_spec_inds)%vector(iY_sub)
               ! Local species index in the sub-array species
               if (trim(spec_name(ispec)).eq.trim(Y_name(iY))) then
                  iY_in_sub(iY)=iY_sub
                  exit
               end if
            end do
         end do

         ! Check if we have found all the required species
         do iY=1,n_Y
            if (iY_in_sub(iY).eq.0) then
               errmsg='Could not find '//trim(Y_name(iY))//' in the sub-species list. Make sure that '//trim(Y_name(iY))//' exists in the sub-array species used by the ANN.'
               call die(trim(errmsg))
            end if
         end do

      end block initialize_species


      ! Initialize our scalar fields
      initialize_scalar: block
         use messager, only: die
         use parallel, only: MPI_REAL_WP
         use fcmech,   only: hsp, cpsp, W_sp
         integer  :: i,j,k,ierr,ispec, iY_sub
         integer  :: imin,imax,jmin,jmax,kmin,kmax
         real(WP) :: h_init,T_buf,T_range,T_min, Tb, Tu, xloc
         real(WP) :: SC_init_min,SC_init_max


         call param_read('Tu', Tu)
         call param_read('Tb', Tb)

         call param_read('Flame location', xloc)
         ! call param_read('Pressure', sc%Pthermo)

         print *, "X location of initial flame front", xloc

         ! Get initial composition
         Yu = 0.0_WP
         do i = 1, nspec
            ! Check if species is defined
            if (param_exists('Inflow '//trim(spec_name(i)))) then
               call param_read('Inflow '//trim(spec_name(i)), Yu(i))
               print *, 'Inflow species ', trim(spec_name(i)), Yu(i)
            end if
         end do

         Yu = Yu/sum(Yu)

         ! Get initial composition
         Yb = 0.0_WP
         do i = 1, nspec
            ! Check if species is defined
            if (param_exists('Burnt '//trim(spec_name(i)))) then
               call param_read('Burnt '//trim(spec_name(i)), Yb(i))
               print *, 'Burnt species ', trim(spec_name(i)), Yb(i)
            end if
         end do

         Yb = Yb/sum(Yb)


         ! tmp_sc = 1.0_WP
         do k = sc%cfg%kmino_, sc%cfg%kmaxo_
            do j = sc%cfg%jmino_, sc%cfg%jmaxo_
               do i = sc%cfg%imino_, sc%cfg%imaxo_
                  Y_init= Yu + (Yb - Yu)*0.5_WP*(1.0_WP + tanh((sc%cfg%xm(i) - xloc)/(Lx/20.0_WP)))
                  T(i,j,k)= Tu + (Tb - Tu)*0.5_WP*(1.0_WP + tanh((sc%cfg%xm(i) - xloc)/(Lx/20.0_WP)))

                  call fcmech_thermodata(T(i,j,k))
                  h_init=0.0_WP
                  do ispec=1, nspec
                     h_init=h_init+hsp(ispec)* Y_init(ispec)
                  end do

                  ! Process the species indices and set initial mass fractions
                  do iY_sub=1,nY_sub
                     ! Global species index
                     ispec=aen%vectors(aen%ivec_spec_inds)%vector(iY_sub)
                     Y_sub(iY_sub)=Y_init(ispec)
                  end do
         
                  
                  ! Map Y and h to the neural network scalars
                  call aen%transform_inputs([h_init,Y_sub],hY)
                  call aen%encode(hY,sc%SC(i,j,k,:))

                  ! Get transport properties
                  call trn%get_transport(sc%SC(i,j,k,:),trnprop_tmp)
                  call trn%inverse_transform_outputs(trnprop_tmp,trnprop)
                  sc%rho(i,j,k)   =exp(trnprop(1))
                  fs%visc(i,j,k)  =trnprop(2)
                  sc%diff(i,j,k,:)=trnprop(3)

                  ! Map the neural network scalars to Y (for visualization purposes at t=0)
                  call aen%decode(sc%SC(i,j,k,:),TYS)
                  call aen%inverse_transform_outputs(TYS,hY,nY_sub+1)
                  do iY=1,n_Y
                     Y(i,j,k,iY)=hY(iY_in_sub(iY)+1)
                  end do

                  test_diff: block
                  use messager, only: die
                     real(WP) :: Wmix, sum1, sum2, cp, lambda, diff
                     real(WP), dimension(nspec)  :: eta, cond


                     ! ---- Thermal diffusivity ---- !
                     ! Mixture molar mass and temperature
                     print *, "Tinit: ", T(i,j,k)

                     Wmix = 1.0_WP/sum(Y_init/W_sp)
                     print *, "Wmix", Wmix

                     ! Individual compounds viscosity
                     call fcmech_get_viscosity(eta, T(i,j,k))

                     ! Individual compounds viscosity
                     call fcmech_get_conductivity(cond, T(i,j,k), eta)

                     ! Mixture averaged thermal conductivity
                     sum1 = Wmix*sum(Y_init/(cond*W_sp))
                     sum2 = Wmix*sum(Y_init*cond/W_sp)
                     lambda = 0.5_WP*(sum2 + 1.0_WP/sum1)

                     print *, "conductivity", lambda
                     ! Average Cp based on scalar field
                     call fcmech_thermodata(T(i,j,k))
                     cp  = sum(Y_init*Cpsp)
                     print *, "cp", cp

               

                  ! Thermal diffusivity for enthalpy
                  !  this%diff(i,j,k,isc_ENTH)=this%lambda(i,j,k)/this%cp(i,j,k)

                  ! Thermal diffusivity for temperature
                  ! this%diff(i, j, k, nspec1) = this%lambda(i, j, k)/this%cp(i, j, k)
                     diff =  lambda  / cp
                     print *, "FC Diff", diff
                     print *, "ANN Diff", (sc%diff(i,j,k, 1))
                     print *, "----------------------------------"
                     print *, ""


                  end block test_diff

               end do
            end do
         end do

         ! call die("HERE")

         ! call sc%get_bcond('inflow', mybc)
         ! do n = 1, mybc%itr%no_
         !    i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
         !    sc%SC(i, j, k, 1:nspec) = 2.0_WP*Yinit - sc%SC(i + 1, j, k, 1:nspec)
         !    sc%SC(i, j, k, nspec + 1) = 2.0_WP*Tinit - sc%SC(i + 1, j, k, nspec + 1)

         ! end do


         ! Sync fields
         call sc%cfg%sync(T)
         call sc%cfg%sync(sc%rho)
         call sc%cfg%sync(fs%visc)
         do isc=1,sc%nscalar
            call sc%cfg%sync(sc%SC  (:,:,:,isc))
            call sc%cfg%sync(sc%diff(:,:,:,isc))
         end do
         do iY=1,n_Y
            call sc%cfg%sync(Y(:,:,:,iY))
         end do

         ! Release unused memory
         deallocate(SC_init)
         deallocate(Y_init)

      end block initialize_scalar

      ! Initialize our velocity field
      initialize_velocity: block
         use lowmach_class, only: bcond
         use parallel, only: MPI_REAL_WP
         integer :: n, i, j, k, ierr
         type(bcond), pointer :: mybc
         ! Velocity fluctuation, length scales, epsilon
         real(WP) :: Ut, le, ld, epsilon

         call param_read("U velocity", Uin)
         fs%U = Uin
         ! fs%U = 0.0_WP
         fs%V = 0.0_WP
         fs%W = 0.0_WP

         call fs%get_bcond('inflow', mybc)
         do n = 1, mybc%itr%no_
            i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
            fs%U(i, j, k) = Uin
         end do

         ! Set density from scalar
         fs%rho = sc%rho
         ! fs%visc = sc%visc
         ! Form momentum
         call fs%rho_multiply
         ! Apply all other boundary conditions
         call fs%apply_bcond(time%t, time%dt)
         call fs%interp_vel(Ui, Vi, Wi)
         resRHO = 0.0_WP
         call fs%get_div(drhodt=resRHO)
         ! Compute MFR through all boundary conditions
         call fs%get_mfr()
      end block initialize_velocity


      ! Initialize turbulence statistics
      initialize_stats: block
         ! Mean density
         call cfg%integrate(sc%rho,rhomean)
         rhomean=rhomean/cfg%vol_total
         ! Favre-averaged temperature
         tmpfield=sc%rho*T
         call cfg%integrate(tmpfield,Tmean)
         Tmean=Tmean/(rhomean*cfg%vol_total)
         ! Favre-averaged temperature fluctuations
         tmpfield=sc%rho*(T-Tmean)**2
         call cfg%integrate(tmpfield,Trms)
         Trms=sqrt(Trms/(rhomean*cfg%vol_total))
         ! Maximum temperature
         call cfg%maximum(T,Tmax)
      end block initialize_stats


      ! Add Ensight output
      create_ensight: block
         ! Create Ensight output from cfg
         ens_out=ensight(cfg=cfg,name='reactor2d_ann')
         ! Create event for Ensight output
         ens_evt=event(time=time,name='Ensight output')
         call param_read('Ensight output period',ens_evt%tper)
         ! Add variables to output
         call ens_out%add_scalar('pressure'   ,fs%P)
         call ens_out%add_vector('velocity'   ,Ui,Vi,Wi)
         call ens_out%add_scalar('divergence' ,fs%div)
         call ens_out%add_scalar('density'    ,sc%rho)
         call ens_out%add_scalar('viscosity'  ,fs%visc)
         call ens_out%add_scalar('T',T)
         call ens_out%add_scalar('thermal_diff',sc%diff(:, :, :, 1))
         do iY=1,n_Y
            call ens_out%add_scalar('Y_'//Y_name(iY),Y(:,:,:,iY))
         end do
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
         call mfile%add_column(sc%rhomax,'RHOmax')
         call mfile%add_column(sc%rhomin,'RHOmin')
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
         ! Creat statistics monitor
         statfile=monitor(fs%cfg%amRoot,'stats')
         call statfile%add_column(time%t,'Time')
         call statfile%add_column(Tmean,'Tmean')
         call statfile%add_column(Trms,'Trms')
         call statfile%add_column(Tmax,'Tmax')
         call statfile%write()
      end block create_monitor


   end subroutine simulation_init


   !> Perform an NGA2 simulation
   subroutine simulation_run
      implicit none
      integer :: i,j,k, nsc


      ! Perform time integration
      do while (.not. time%done())

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

         SC_src = 0.0_WP
         ! Get the RHS from chsourcenet
            do k=sc%cfg%kmino_,sc%cfg%kmaxo_
               do j=sc%cfg%jmino_,sc%cfg%jmaxo_
                  do i=sc%cfg%imino_,sc%cfg%imaxo_
                     call csn%get_src(sc%SC(i,j,k,:),SC_src(i,j,k,:))
                  end do
               end do
            end do


         ! Perform sub-iterations
         do while (time%it.le.time%itmax)

            ! =============SCALAR SOLVER =============!
            


            ! Reset metric for bquick
            call sc%metric_reset()

            ! Build mid-time scalar
            sc%SC=0.5_WP*(sc%SC+sc%SCold)

            ! Explicit calculation of drhoSC/dt from scalar equation
            ! call sc%get_drhoSCdt(resSC,fs%rhoU,fs%rhoV,fs%rhoW)

            ! ! Assemble explicit residual
            ! do isc=1,sc%nscalar
            !    resSC(:,:,:,isc)=time%dt*resSC(:,:,:,isc)-2.0_WP*sc%rho*sc%SC(:,:,:,isc)+(sc%rho+sc%rhoold)*sc%SCold(:,:,:,isc)+time%dt*sc%rho*SC_src(:,:,:,isc)
            !    SCtmp(:,:,:,isc)=2.0_WP*sc%SC(:,:,:,isc)-sc%SCold(:,:,:,isc)+resSC(:,:,:,isc)/sc%rho
            ! end do

            ! do isc = 1, sc%nscalar
            !    ! Assemble explicit residual
            !    resSC(:, :, :, isc) = time%dt*resSC(:, :, :, isc) - 2.0_WP*sc%rho*sc%SC(:, :, :, isc) + (sc%rho + sc%rhoold)*sc%SCold(:, :, :, isc) + sc%rho*SC_src(:, :, :, isc) * time%dt
            !    SCtmp(:, :, :, isc) = 2.0_WP*sc%SC(:, :, :, isc) - sc%SCold(:, :, :, isc) + resSC(:, :, :, isc)/sc%rho
            ! end do

            ! ! Apply it to get explicit scalar prediction
            ! do isc=1,sc%nscalar
            !    do k=sc%cfg%kmino_,sc%cfg%kmaxo_
            !       do j=sc%cfg%jmino_,sc%cfg%jmaxo_
            !          do i=sc%cfg%imino_,sc%cfg%imaxo_
            !             if ((SCtmp(i,j,k,isc).le.0.0_WP).or.(SCtmp(i,j,k,isc).ge.1.0_WP)) then
            !                bqflag(i,j,k,isc)=.false.
            !             else
            !                bqflag(i,j,k,isc)=.false.
            !             end if
            !          end do
            !       end do
            !    end do
            ! end do

            ! ! Adjust metrics
            ! call sc%metric_adjust(SCtmp,bqflag)

            call sc%get_drhoSCdt(resSC, fs%rhoU, fs%rhoV, fs%rhoW)
            ! resSC = -2.0_WP*(sc%SC - sc%SCold) + time%dt*resSC
            do nsc = 1, sc%nscalar
               ! ============= SCALAR SOLVER =======================
               ! Assemble explicit residual
               resSC(:, :, :, nsc) = time%dt*resSC(:, :, :, nsc) - 2.0_WP*sc%rho*sc%SC(:, :, :, nsc) + (sc%rho + sc%rhoold)*sc%SCold(:, :, :, nsc) + sc%rho *SC_src(:, :, :, nsc) * time%dt
            end do
            !    resSC(:,:,:,nsc)=time%dt*resSC(:,:,:,nsc)-2.0_WP*sc%rho*sc%SC(:,:,:,nsc) + (sc%rho+sc%rhoold)*sc%SCold(:,:,:,nsc) + sc%rho * sc%SRCchem(:,:,:,nsc)
            ! Get the residual
            if (time_stepping.eq.'implicit') then
               ! Form implicit residual
               call sc%solve_implicit(time%dt,resSC,fs%rhoU,fs%rhoV,fs%rhoW)
            else
               ! Divide by density
               do nsc=1,sc%nscalar
                  resSC(:,:,:,nsc)=resSC(:,:,:,nsc)/sc%rho
               end do
            end if
            ! Apply these residuals
            sc%SC = 2.0_WP*sc%SC - sc%SCold + resSC
            ! Apply all boundary conditions on the resulting field
            call sc%apply_bcond(time%t, time%dt)
           

            ! call sc%get_bcond('inflow', mybc)
            ! do n = 1, mybc%itr%no_
            !    i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
            !    sc%SC(i, j, k, 1:nspec) = 2.0_WP*Yinit - sc%SC(i + 1, j, k, 1:nspec)
            !    sc%SC(i, j, k, nspec + 1) = 2.0_WP*Tinit - sc%SC(i + 1, j, k, nspec + 1)
   
            ! end do

            ! =============================================

            ! ============UPDATE PROPERTIES ====================

            ! Get transport properties
            do k=sc%cfg%kmino_,sc%cfg%kmaxo_
               do j=sc%cfg%jmino_,sc%cfg%jmaxo_
                  do i=sc%cfg%imino_,sc%cfg%imaxo_
                     call trn%get_transport(sc%SC(i,j,k,:),trnprop_tmp)
                     call trn%inverse_transform_outputs(trnprop_tmp,trnprop)
                     sc%rho(i,j,k)   =exp(trnprop(1))
                     fs%visc(i,j,k)  =trnprop(2)
                     sc%diff(i,j,k,:)=trnprop(3)  
                  end do
               end do
            end do

            ! ===================================================

            ! ============VELOCITY SOLVER ======================

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

            ! Divide by density to get velocity residuals
            ! do k=cfg%kmin_,cfg%kmax_+1
            !    do j=cfg%jmin_,cfg%jmax_+1
            !       do i=cfg%imin_,cfg%imax_+1
            !          resU(i,j,k)=resU(i,j,k)/sum(fs%itpr_x(:,i,j,k)*fs%rho(i-1:i,j,k))
            !          resV(i,j,k)=resV(i,j,k)/sum(fs%itpr_y(:,i,j,k)*fs%rho(i,j-1:j,k))
            !          resW(i,j,k)=resW(i,j,k)/sum(fs%itpr_z(:,i,j,k)*fs%rho(i,j,k-1:k))
            !       end do
            !    end do
            ! end do
            call fs%cfg%sync(resU)
            call fs%cfg%sync(resV)
            call fs%cfg%sync(resW)


            ! Form implicit residuals
            call fs%solve_implicit(time%dtmid, resU, resV, resW)


            ! Apply these residuals
            fs%U=2.0_WP*fs%U-fs%Uold+resU
            fs%V=2.0_WP*fs%V-fs%Vold+resV
            fs%W=2.0_WP*fs%W-fs%Wold+resW

            ! Apply other boundary conditions and update momentum
            call fs%apply_bcond(time%tmid, time%dtmid)
            dirichlet_velocity: block
               use lowmach_class, only: bcond
               type(bcond), pointer :: mybc
               integer :: n, i, j, k
               call fs%get_bcond('inflow', mybc)
               do n = 1, mybc%itr%no_
                  i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
                  fs%U(i, j, k) = Uin
               end do
            end block dirichlet_velocity

            call fs%rho_multiply()

            ! Solve Poisson equation
            call sc%get_drhodt(dt=time%dt, drhodt=resRHO)
            call fs%correct_mfr(drhodt=resRHO)
            call fs%get_div(drhodt=resRHO)
            fs%psolv%rhs=-fs%cfg%vol*fs%div/time%dtmid
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

            ! ===================================================

            ! Increment sub-iteration counter
            time%it=time%it+1

         end do

         ! Recompute interpolated velocity and divergence
         call fs%interp_vel(Ui,Vi,Wi)
         call sc%get_drhodt(dt=time%dt,drhodt=resRHO)
         call fs%get_div(drhodt=resRHO)

         ! Post process
         if (ens_evt%occurs()) then
            ! Map the neural network scalars to T and Y
            do k=sc%cfg%kmino_,sc%cfg%kmaxo_
               do j=sc%cfg%jmino_,sc%cfg%jmaxo_
                  do i=sc%cfg%imino_,sc%cfg%imaxo_
                     call aen%decode(sc%SC(i,j,k,:),TYS)
                     call aen%inverse_transform_outputs(TYS,hY,nY_sub+1)
                     T(i,j,k)=hY(1)
                     do iY=1,n_Y
                        Y(i,j,k,iY)=hY(iY_in_sub(iY)+1)
                     end do
                  end do
               end do
            end do
            ! Mean density
            call cfg%integrate(sc%rho,rhomean)
            rhomean=rhomean/cfg%vol_total
            ! Favre-averaged temperature
            tmpfield=sc%rho*T
            call cfg%integrate(tmpfield,Tmean)
            Tmean=Tmean/(rhomean*cfg%vol_total)
            ! Favre-averaged temperature fluctuations
            tmpfield=sc%rho*(T-Tmean)**2
            call cfg%integrate(tmpfield,Trms)
            Trms=sqrt(Trms/(rhomean*cfg%vol_total))
            ! Maximum temperature
            call cfg%maximum(T,Tmax)
            ! Output to ensight
            call ens_out%write_data(time%t)
            ! Output monitoring
            call statfile%write()
         end if

         ! Perform and output monitoring
         call fs%get_max()
         call sc%get_max()
         call sc%get_int()
         call mfile%write()
         call cflfile%write()

      end do


   end subroutine simulation_run


   !> Finalize the NGA2 simulation
   subroutine simulation_final
      implicit none


      ! Get rid of all objects-need destructors
      ! monitor
      ! ensight
      ! bcond
      ! timetracker

      ! Deallocate work arrays
      deallocate(spec_name,Y_sub,hY,TYS,iY_in_sub,Y_name)
      deallocate(resU,resV,resW,resRHO,Ui,Vi,Wi,T)
      deallocate(resSC,SCtmp,SC_src,bqflag,Y)


   end subroutine simulation_final


end module simulation
