!> Various definitions and tools for running an NGA2 simulation
module simulation
   use precision,           only: WP
   use string,              only: str_medium
   use geometry,            only: cfg,Lx
   use ddadi_class,         only: ddadi
   use hypre_str_class,     only: hypre_str
   use lowmach_class,       only: lowmach
   use vdscalar_class,      only: vdscalar
   use multivdscalar_class, only: multivdscalar
   use finitechem_class,    only: finitechem
   use timetracker_class,   only: timetracker
   use ensight_class,       only: ensight
   use event_class,         only: event
   use monitor_class,       only: monitor
   use fcmech
   implicit none
   private

   !> Single low Mach flow solver and scalar solver and corresponding time tracker
   type(hypre_str),   public :: ps
   type(ddadi),       public :: vs,ss
   type(lowmach),     public :: fs
   type(finitechem),  public :: fc
   type(timetracker), public :: time

   !> Ensight postprocessing
   type(ensight) :: ens_out
   type(event)   :: ens_evt

   !> Simulation monitor file
   type(monitor) :: mfile,cflfile,consfile

   character(len=str_medium), dimension(:), allocatable :: spec_name


   public :: simulation_init,simulation_run,simulation_final

   !> Private work arrays
   real(WP), dimension(:,:,:),   allocatable :: resU,resV,resW,resRHO
   real(WP), dimension(:,:,:),   allocatable :: Ui,Vi,Wi
   real(WP), dimension(:,:,:,:), allocatable :: resSC,SCtmp
   logical,  dimension(:,:,:,:), allocatable :: bqflag

   !> Initial conditions
   real(WP) :: L_buffer

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

   !> Function that localizes jet at -x
   function xm_scalar(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imin-1) isIn=.true.
   end function xm_scalar

   !> Function that localizes y- boundary
   function ym_scalar(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid),intent(in) :: pg
      integer,intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (j.eq.pg%jmin-1) isIn=.true.
   end function ym_scalar

   !> Find the border indices of the initialization region
   subroutine get_borders(Lbu,imin,imax,jmin,jmax,kmin,kmax)
      implicit none
      real(WP),intent(in) :: Lbu
      integer,intent(out) :: imin,imax,jmin,jmax,kmin,kmax
      integer :: i,j,k

      ! Find the x bounds
      imin=cfg%imin
      do i=cfg%imin,cfg%imax
         if (cfg%xm(i).gt.cfg%x(cfg%imin)+Lbu) then
            imin=i
            exit
         end if
      end do
      imax=cfg%imax
      do i=cfg%imax,cfg%imin,-1
         if (cfg%xm(i).lt.cfg%x(cfg%imax+1)-Lbu) then
            imax=i
            exit
         end if
      end do

      ! Find the y bounds
      jmin=cfg%jmin
      do j=cfg%jmin,cfg%jmax
         if (cfg%ym(j).gt.cfg%y(cfg%jmin)+Lbu) then
            jmin=j
            exit
         end if
      end do
      jmax=cfg%jmax
      do j=cfg%jmax,cfg%jmin,-1
         if (cfg%ym(j).lt.cfg%y(cfg%jmax+1)-Lbu) then
            jmax=j
            exit
         end if
      end do

      ! Find the z bounds
      kmin=cfg%kmin
      do k=cfg%kmin,cfg%kmax
         if (cfg%zm(k).gt.cfg%z(cfg%kmin)+Lbu) then
            kmin=k
            exit
         end if
      end do
      kmax=cfg%kmax
      do k=cfg%kmax,cfg%kmin,-1
         if (cfg%zm(k).lt.cfg%z(cfg%kmax+1)-Lbu) then
            kmax=k
            exit
         end if
      end do
   end subroutine get_borders

   !> Initialization of problem solver
   subroutine simulation_init
      use param, only: param_read,param_getsize,param_exists
      implicit none
      
      ! Create a low-Mach flow solver with bconds
      create_velocity_solver: block
         use hypre_str_class, only: pcg_pfmg,smg
         use lowmach_class, only: dirichlet,clipped_neumann,slip,neumann
         real(WP) :: visc
         ! Create flow solver
         fs=lowmach(cfg=cfg,name='Variable density low Mach NS')
         ! BCs
         call fs%add_bcond(name='ym_outflow',type=neumann,face='y',dir=-1,canCorrect=.True.,locator=ym_locator)
         call fs%add_bcond(name='yp_outflow',type=neumann,face='y',dir=+1,canCorrect=.True.,locator=yp_locator)
         call fs%add_bcond(name='xm_outflow',type=neumann,face='x',dir=-1,canCorrect=.True.,locator=xm_locator)
         call fs%add_bcond(name='xp_outflow',type=neumann,face='x',dir=+1,canCorrect=.True.,locator=xp_locator)
         ! Configure pressure solver
         ps=hypre_str(cfg=cfg,name='Pressure',method=smg,nst=7)
         ps%maxlevel=18
         call param_read('Pressure iteration',ps%maxit)
         call param_read('Pressure tolerance',ps%rcvg)
         ! Configure implicit velocity solver
         vs=ddadi(cfg=cfg,name='Velocity',nst=7)
         ! Setup the solver
         call fs%setup(pressure_solver=ps,implicit_solver=vs)
      end block create_velocity_solver

      ! Create a scalar solver
      create_fc: block
         use multivdscalar_class, only: dirichlet,neumann,quick,bquick
         real(WP) :: bundleref
         ! Create scalar solver
         fc=finitechem(cfg=cfg,scheme=bquick,name='fc')
         ! Scheduler
         call param_read('Use scheduler',fc%use_scheduler)
         call param_read('Bundle Refinement',bundleref)
         call fc%scheduler_init(bundleref)
         ! BCs
         call fc%add_bcond(name='xm_outflow',type=neumann,locator=xm_scalar,dir='-x')
         call fc%add_bcond(name='xp_outflow',type=neumann,locator=xp_locator,dir='+x')
         call fc%add_bcond(name='ym_outflow',type=neumann,locator=ym_scalar,dir='-y')
         call fc%add_bcond(name='yp_outflow',type=neumann,locator=yp_locator,dir='+y')
         ! Configure implicit scalar solver
         ss=ddadi(cfg=cfg,name='Scalar',nst=13)
         ! Setup the solver
         call fc%setup(implicit_solver=ss)
      end block create_fc

      ! Allocate work arrays
      allocate_work_arrays: block
         ! Flow solver
         allocate (resU(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resV(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resW(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resRHO(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Ui(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Vi(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Wi(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         ! Scalar solver
         allocate (resSC(fc%cfg%imino_:fc%cfg%imaxo_,fc%cfg%jmino_:fc%cfg%jmaxo_,fc%cfg%kmino_:fc%cfg%kmaxo_,fc%nscalar))
         allocate (SCtmp(fc%cfg%imino_:fc%cfg%imaxo_,fc%cfg%jmino_:fc%cfg%jmaxo_,fc%cfg%kmino_:fc%cfg%kmaxo_,fc%nscalar))
         allocate (bqflag(cfg%imino_:cfg%imaxo_,cfg%jmino_:cfg%jmaxo_,cfg%kmino_:cfg%kmaxo_,fc%nscalar))
         ! Chemistry
         allocate(spec_name(nspec))
      end block allocate_work_arrays

      ! Initialize time tracker
      initialize_timetracker: block
         use messager, only: die
         time=timetracker(amRoot=fs%cfg%amRoot,name='scheduler_test')
         call param_read('Max timestep size',time%dtmax)
         call param_read('Max cfl number',time%cflmax)
         call param_read('Max time',time%tmax)
         call param_read('Sub-iterations',time%itmax)
         call param_read('Time stepping',time_stepping)
         if ((trim(time_stepping).ne.'explicit').and.(trim(time_stepping).ne.'implicit')) call die('Time stepping must be either explicit or implicit.')
         time%dt=time%dtmax
      end block initialize_timetracker

      ! Initialize our mixture fraction field
      initialize_fc: block
         use multivdscalar_class, only: bcond
         use parallel, only: MPI_REAL_WP
         integer :: n,i,j,k,ierr
         real(WP) :: T_init,T_buf
         integer  :: imin,imax,jmin,jmax,kmin,kmax
         real(WP) :: tmpY
         type(bcond),pointer :: mybc

         ! Read-in the inputs
         call param_read('Buffer region length',L_buffer)
         call param_read('Pressure',fc%Pthermo)
         call param_read('Initial temperature',T_init)
         call param_read('Buffer temperature',T_buf)

         ! Get all the species names in the mechanism
         call fcmech_get_speciesnames(spec_name)

         ! Initial values
         do i=1,nspec
            if (param_exists('Initial '//trim(spec_name(i)))) then
               call param_read('Initial '//trim(spec_name(i)),tmpY)
               fc%SC(:,:,:,i)=tmpY
               if (cfg%amRoot) then
                  print *,"Initial ",trim(spec_name(i)),tmpY
               end if
            end if
         end do
         print*,''

         ! Set initial conditions
         call get_borders(L_buffer,imin,imax,jmin,jmax,kmin,kmax)
         do k=fc%cfg%kmino_,fc%cfg%kmaxo_
            do j=fc%cfg%jmino_,fc%cfg%jmaxo_
               do i=fc%cfg%imino_,fc%cfg%imaxo_
                  if ((i.ge.imin).and.(i.le.imax).and.(j.ge.jmin).and.(j.le.jmax).and.(k.ge.kmin).and.(k.le.kmax)) then
                     fc%SC(i,j,k,nspec+1)=T_init
                  else
                     fc%SC(i,j,k,nspec+1)=T_buf
                  end if
               end do
            end do
         end do

         ! Get properties
         call fc%get_density()
         call fc%get_viscosity()
         call fc%get_diffusivity()
         call fc%get_max()
      end block initialize_fc

      ! Initialize our velocity field
      initialize_velocity: block
         use lowmach_class, only: bcond
         use parallel,     only: MPI_REAL_WP
         integer  :: n,i,j,k
         ! Set the initial velocity field
         fs%U=0.0_WP
         fs%V=0.0_WP
         ! Set density from scalar
         fs%rho=fc%rho
         ! Form momentum
         call fs%rho_multiply()
         ! Apply all other boundary conditions
         call fs%apply_bcond(time%t,time%dt)
         call fs%interp_vel(Ui,Vi,Wi)
         resRHO=0.0_WP
         call fs%get_div(drhodt=resRHO)
         ! Compute MFR through all boundary conditions
         call fs%get_mfr()
      end block initialize_velocity

      ! Add Ensight output
      create_ensight: block
         ! Create Ensight output from cfg
         ens_out=ensight(cfg=cfg,name='scheduler_test')
         ! Create event for Ensight output
         ens_evt=event(time=time,name='Ensight output')
         call param_read('Ensight output period',ens_evt%tper)
         ! Add variables to output
         call ens_out%add_scalar('pressure',fs%P)
         call ens_out%add_vector('velocity',Ui,Vi,Wi)
         call ens_out%add_scalar('divergence',fs%div)
         call ens_out%add_scalar('density',fc%rho)
         call ens_out%add_scalar('YNXC12H26',fc%SC(:,:,:,sNXC12H26))
         call ens_out%add_scalar('T',fc%SC(:,:,:,nspec+1))
         call ens_out%add_scalar('SRC_T',fc%SRCchem(:,:,:,nspec+1))
         ! Output to ensight
         if (ens_evt%occurs()) call ens_out%write_data(time%t)
      end block create_ensight

      ! Create a monitor file
      create_monitor: block
         ! Prepare some info about fields
         call fs%get_cfl(time%dt,time%cfl)
         call fs%get_max()
         call fc%get_max()
         call fc%get_int()
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
         call mfile%add_column(fc%rhomax,'RHOmax')
         call mfile%add_column(fc%rhomin,'RHOmin')
         call mfile%add_column(fc%SCmax(nspec+1),'Temperature')
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
         call consfile%add_column(fc%rhoint,'RHO integral')
         call consfile%write()
      end block create_monitor

   end subroutine simulation_init

   !> Perform an NGA2 simulation
   subroutine simulation_run
      implicit none
      integer :: i,j,k,isc

      ! Perform time integration
      do while (.not. time%done())

         ! Increment time
         call fs%get_cfl(time%dt,time%cfl)
         call time%adjust_dt()
         call time%increment()

         ! Remember old scalar
         fc%rhoold=fc%rho
         fc%SCold=fc%SC

         ! Remember old velocity and momentum
         fs%rhoold=fs%rho
         fs%Uold=fs%U; fs%rhoUold=fs%rhoU
         fs%Vold=fs%V; fs%rhoVold=fs%rhoV
         fs%Wold=fs%W; fs%rhoWold=fs%rhoW

         ! Update chemical source terms
         fc%SRCchem=0.0_WP
         call fc%react(time%dt)

         ! Perform sub-iterations
         do while (time%it.le.time%itmax)

            ! ============= SCALAR SOLVER =======================
            ! Reset scalar solver metric
            call fc%metric_reset()
            
            ! Build mid-time scalar
            fc%SC=0.5_WP*(fc%SC+fc%SCold)
            
            ! Diffusive source terms
            fc%SRC=0.0_WP
            call fc%diffusive_source(time%dt)

            ! Explicit calculation of drhoSC/dt from scalar equation
            call fc%get_drhoSCdt(resSC,fs%rhoU,fs%rhoV,fs%rhoW)

            ! Assemble explicit residual
            do isc=1,fc%nscalar
               resSC(:,:,:,isc)=time%dt*resSC(:,:,:,isc)-2.0_WP*fc%rho*fc%SC(:,:,:,isc)+(fc%rho+fc%rhoold)*fc%SCold(:,:,:,isc)+fc%rho*fc%SRCchem(:,:,:,isc)+fc%SRC(:,:,:,isc)
               SCtmp(:,:,:,isc)=2.0_WP*fc%SC(:,:,:,isc)-fc%SCold(:,:,:,isc)+resSC(:,:,:,isc)/fc%rho
            end do

            ! Apply it to get explicit scalar prediction
            do isc=1,fc%nscalar
               do k=fc%cfg%kmino_,fc%cfg%kmaxo_
                  do j=fc%cfg%jmino_,fc%cfg%jmaxo_
                     do i=fc%cfg%imino_,fc%cfg%imaxo_
                        if (isc.eq.nspec+1) then
                           if (SCtmp(i,j,k,isc).le.250.0_WP.or.SCtmp(i,j,k,isc).ge.4000.0_WP) then
                              bqflag(i,j,k,isc)=.true.
                           else
                              bqflag(i,j,k,isc)=.false.
                           end if
                        else
                           if (SCtmp(i,j,k,isc).le.0.0_WP.or.SCtmp(i,j,k,isc).ge.1.0_WP) then
                              bqflag(i,j,k,isc)=.true.
                           else
                              bqflag(i,j,k,isc)=.false.
                           end if
                        end if
                     end do
                  end do
               end do
            end do

            ! Adjust metrics
            call fc%metric_adjust(SCtmp,bqflag)

            ! Recompute drhoSC/dt
            call fc%get_drhoSCdt(resSC,fs%rhoU,fs%rhoV,fs%rhoW)

            ! Assemble explicit residual
            do isc=1,fc%nscalar
               resSC(:,:,:,isc)=time%dt*resSC(:,:,:,isc)-2.0_WP*fc%rho*fc%SC(:,:,:,isc)+(fc%rho+fc%rhoold)*fc%SCold(:,:,:,isc)+fc%rho*fc%SRCchem(:,:,:,isc)+fc%SRC(:,:,:,isc)
            end do

            ! Get the residuals
            if (time_stepping.eq.'implicit') then
               ! Form implicit residual
               call fc%solve_implicit(time%dt,resSC,fs%rhoU,fs%rhoV,fs%rhoW)
            else
               ! Divide by density
               do isc=1,fc%nscalar
                  resSC(:,:,:,isc)=resSC(:,:,:,isc)/fc%rho
               end do
            end if

            ! Apply these residuals
            fc%SC=2.0_WP*fc%SC-fc%SCold+resSC

            ! Apply all boundary conditions on the resulting field
            call fc%apply_bcond(time%t,time%dt)
            ! =============================================

            ! ============ UPDATE PROPERTIES ====================
            call fc%get_density()
            call fc%get_viscosity()
            fs%visc=fc%visc
            call fc%get_diffusivity()
            ! ===================================================

            ! ============ VELOCITY SOLVER ======================
            ! Build n+1 density
            fs%rho=0.5_WP*(fc%rho+fc%rhoold)

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

            ! Get the residuals
            if (time_stepping.eq.'implicit') then
               ! Form implicit residuals
               call fs%solve_implicit(time%dtmid,resU,resV,resW)
            else
               ! Divide by density
               do k=cfg%kmin_,cfg%kmax_+1
                  do j=cfg%jmin_,cfg%jmax_+1
                     do i=cfg%imin_,cfg%imax_+1
                        resU(i,j,k)=resU(i,j,k)/sum(fs%itpr_x(:,i,j,k)*fs%rho(i-1:i,j,k))
                        resV(i,j,k)=resV(i,j,k)/sum(fs%itpr_y(:,i,j,k)*fs%rho(i,j-1:j,k))
                        resW(i,j,k)=resW(i,j,k)/sum(fs%itpr_z(:,i,j,k)*fs%rho(i,j,k-1:k))
                     end do
                  end do
               end do
               call fs%cfg%sync(resU)
               call fs%cfg%sync(resV)
               call fs%cfg%sync(resW)
            end if

            ! Apply these residuals
            fs%U=2.0_WP*fs%U-fs%Uold+resU
            fs%V=2.0_WP*fs%V-fs%Vold+resV
            fs%W=2.0_WP*fs%W-fs%Wold+resW

            ! Apply boundary conditions and update momentum
            call fs%apply_bcond(time%tmid,time%dtmid)
            call fs%rho_multiply()

            ! Solve Poisson equation
            call fc%get_drhodt(dt=time%dt,drhodt=resRHO)
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
         call fc%get_drhodt(dt=time%dt,drhodt=resRHO)
         call fs%get_div(drhodt=resRHO)

         ! Output to ensight
         if (ens_evt%occurs()) call ens_out%write_data(time%t)

         ! Perform and output monitoring
         call fs%get_max()
         call fc%get_max()
         call fc%get_int()
         call mfile%write()
         call cflfile%write()
         call consfile%write()

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
      deallocate (resU,resV,resW,resRHO,Ui,Vi,Wi,resSC,SCtmp,bqflag)

   end subroutine simulation_final

end module simulation
