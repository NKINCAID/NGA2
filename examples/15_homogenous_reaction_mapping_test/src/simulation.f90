!> Various definitions and tools for running an NGA2 simulation
module simulation
   use precision, only: WP
   use string, only: str_medium
   use geometry, only: cfg, Lx, Ly, Lz
   use ddadi_class, only: ddadi
   use hypre_str_class, only: hypre_str
   use lowmach_class, only: lowmach
   use vdscalar_class, only: vdscalar
   use multivdscalar_class, only: multivdscalar
   use finitechem_class, only: finitechem
   use timetracker_class, only: timetracker
   use ensight_class, only: ensight
   use event_class, only: event
   use monitor_class, only: monitor
   use parallel, only: parallel_time
   use fcmech
   implicit none
   private

   !> Single low Mach flow solver and scalar solver and corresponding time tracker
   type(hypre_str), public :: ps
   type(ddadi), public :: vs, ss
   type(lowmach), public :: fs
   type(finitechem), public :: fc
   type(timetracker), public :: time

   !> Ensight postprocessing
   type(ensight) :: ens_out
   type(event)   :: ens_evt

   !> Simulation monitor file
   type(monitor) :: mfile, cflfile, consfile, fcfile

   character(len=str_medium), dimension(:), allocatable :: spec_name


   public :: simulation_init, simulation_run, simulation_final

   !> Private work arrays
   real(WP), dimension(:, :, :), allocatable :: resU, resV, resW, resRHO
   real(WP), dimension(:, :, :), allocatable :: Ui, Vi, Wi
   real(WP), dimension(:,:,:),   allocatable :: SC_init               !< Initial condition for scalar field
   real(WP), dimension(:, :, :, :), allocatable :: SR
   real(WP), dimension(:, :, :, :, :), allocatable :: gradU
   real(WP), dimension(:, :, :), allocatable :: tmp_sc
   real(WP), dimension(:, :, :, :), allocatable :: resSC, SCtmp

   logical, dimension(:, :, :, :), allocatable :: flag

   !> Fluid, forcing, and particle parameters
   real(WP) :: visc, meanU, meanV, meanW
   real(WP) :: Urms0, TKE0, EPS0, Re_max
   real(WP) :: TKE, URMS
   real(WP) :: tauinf, G, Gdtau, Gdtaui, dx
   real(WP) :: L_buffer

   !> For monitoring
   real(WP) :: EPS
   real(WP) :: Re_L, Re_lambda
   real(WP) :: eta, ell
   real(WP) :: dx_eta, ell_Lx, Re_ratio, eps_ratio, tke_ratio, nondtime

   real(WP) :: tmp_sc_min, tmp_sc_max

   real(WP) :: t1, t2, t3, t4, t5, t6, t7

   !> Time stepping
   character(len=str_medium) :: time_stepping

contains

   !> Function that localizes y- boundary
   function ym_locator(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      isIn = .false.
      if (j .eq. pg%jmin) isIn = .true.
   end function ym_locator

   !> Function that localizes y+ boundary
   function yp_locator(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      isIn = .false.
      if (j .eq. pg%jmax + 1) isIn = .true.
   end function yp_locator

   !> Function that localizes the x+ boundary
   function xm_locator(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      isIn = .false.
      if (i .eq. pg%imin) isIn = .true.
   end function xm_locator

   !> Function that localizes the x+ boundary
   function xp_locator(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      isIn = .false.
      if (i .eq. pg%imax + 1) isIn = .true.
   end function xp_locator

   !> Function that localizes jet at -x
   function xm_scalar(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      isIn = .false.
      if (i .eq. pg%imin - 1) isIn = .true.
   end function xm_scalar

   !> Function that localizes the right domain boundary
   ! function xp_scalar(pg, i, j, k) result(isIn)
   !     use pgrid_class, only: pgrid
   !     class(pgrid), intent(in) :: pg
   !     integer, intent(in) :: i, j, k
   !     logical :: isIn
   !     isIn = .false.
   !     ! if (i .ge. pg%imax) isIn = .true.
   !     if (i .eq. pg%imax) isIn = .true.
   ! end function xp_scalar

   !> Function that localizes y- boundary
   function ym_scalar(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      isIn = .false.
      if (j .eq. pg%jmin - 1) isIn = .true.
   end function ym_scalar

   !> Function that localizes y+ boundary
   ! function yp_scalar(pg, i, j, k) result(isIn)
   !     use pgrid_class, only: pgrid
   !     class(pgrid), intent(in) :: pg
   !     integer, intent(in) :: i, j, k
   !     logical :: isIn
   !     isIn = .false.
   !     if (j .eq. pg%jmax) isIn = .true.
   ! end function yp_scalar


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

   !> Initialize a double delta scalar field
   subroutine doubledelta_SCinit(Lbu,imin,imax,jmin,jmax,kmin,kmax)
      use precision
      use param,   only: param_read
      use random,  only: random_normal,random_uniform
      use, intrinsic :: iso_c_binding
      implicit none

      ! Buffer and faded region lenght
      real(WP),intent(in) :: Lbu
      integer, intent(in) :: imin,imax,jmin,jmax,kmin,kmax
      real(WP) :: pi,ke,dk,kc,ks,ksk0ratio,kcksratio,kx,ky,kz,kk,f_phi,kk2
      ! Complex and real buffer
      complex(WP), dimension(:,:,:), pointer :: Cbuf
      real(WP),    dimension(:,:,:), pointer :: Rbuf
      ! Spectrum computation
      real(WP) :: spec_amp,eps,amp_disc,energy_spec
      complex(WP), dimension(:,:,:), pointer :: ak,bk
      ! Other
      integer     :: i,j,k,nk,nx,ny,nz
      complex(WP) :: ii=(0.0_WP,1.0_WP)
      real(WP)    :: rand
      ! Fourier coefficients
      integer(KIND=8) :: plan_r2c,plan_c2r

      include 'fftw3.f03'

      ! Create pi
      pi=acos(-1.0_WP)

      ! Step size for wave number
      dk=2.0_WP*pi/(Lx-2.0_WP*Lbu)

      ! Number of cells iniside the initialization region
      nx=imax-imin+1
      ny=jmax-jmin+1
      nz=kmax-kmin+1
      nk=nx/2+1

      ! Initialize in similar manner to Eswaran and Pope 1988
      call param_read('ks/ko',ksk0ratio)
      ks=ksk0ratio*dk
      call param_read('kc/ks',kcksratio)
      kc=kcksratio*ks

      ! Allocate Cbuf and Rbuf
      allocate(Cbuf(nk,ny,nz))
      allocate(Rbuf(nx,ny,nz))

      ! Compute the Fourier coefficients
      do k=1,nz
         do j=1,ny
            do i=1,nk
               ! Wavenumbers
               kx=real(i-1,WP)*dk
               ky=real(j-1,WP)*dk
               if (j.gt.nk) ky=-real(nx+1-j,WP)*dk
               kz=real(k-1,WP)*dk
               if (k.gt.nk) kz=-real(nx+1-k,WP)*dk
               kk =sqrt(kx**2+ky**2+kz**2)
               kk2=sqrt(kx**2+ky**2)

               ! Compute the Fourier coefficients
               if ((ks-dk/2.0_WP.le.kk).and.(kk.le.ks+dk/2.0_WP)) then
                  f_phi=1.0_WP
               else
                  f_phi=0.0_WP
               end if
               call random_number(rand)
               if (kk.lt.1e-10) then
                  Cbuf(i,j,k)=0.0_WP
               else
                  Cbuf(i,j,k)=sqrt(f_phi/(4.0_WP*pi*kk**2))*exp(ii*2.0_WP*pi*rand)
               end if
            end do
         end do
      end do

      ! Oddball and setting up plans based on geometry
      do j=nk+1,ny
         Cbuf(1,j,1)=conjg(Cbuf(1,ny+2-j,1))
      end do
      call dfftw_plan_dft_c2r_2d(plan_c2r,nx,ny,Cbuf,Rbuf,FFTW_ESTIMATE)
      call dfftw_plan_dft_r2c_2d(plan_r2c,nx,ny,Rbuf,Cbuf,FFTW_ESTIMATE)

      ! Inverse Fourier transform
      call dfftw_execute(plan_c2r)

      ! Force 'double-delta' pdf on scalar field
      do k=1,nz
         do j=1,ny
            do i=1,nx
               if (Rbuf(i,j,k).le.0.0_WP) then
                  Rbuf(i,j,k)=0.0_WP
               else
                  Rbuf(i,j,k)=1.0_WP
               end if
            end do
         end do
      end do

      ! Fourier Transform and filter to smooth
      call dfftw_execute(plan_r2c)

      do k=1,nz
         do j=1,ny
            do i=1,nk
               ! Wavenumbers
               kx=real(i-1,WP)*dk
               ky=real(j-1,WP)*dk
               if (j.gt.nk) ky=-real(nx+1-j,WP)*dk
               kz=real(k-1,WP)*dk
               if (k.gt.nk) kz=-real(nx+1-k,WP)*dk
               kk =sqrt(kx**2+ky**2+kz**2)
               kk2=sqrt(kx**2+ky**2)

               ! Filter to remove high wavenumber components
               if (kk.le.kc) then
                  Cbuf(i,j,k)=Cbuf(i,j,k)*1.0_WP
               else
                  Cbuf(i,j,k)=Cbuf(i,j,k)*(kc/kk)**2
               end if

            end do
         end do
      end do

      ! Oddball
      do j=nk+1,ny
         Cbuf(1,j,1)=conjg(Cbuf(1,ny+2-j,1))
      end do

      ! Fourier Transform back to real
      call dfftw_execute(plan_c2r)

      ! Set zero in the buffer region
      SC_init=0.0_WP
      ! Set the internal scalar field
      SC_init(imin:imax,jmin:jmax,kmin:kmax)=Rbuf/real(nx*ny*nz,WP)

      ! Destroy the plans
      call dfftw_destroy_plan(plan_c2r)
      call dfftw_destroy_plan(plan_r2c)

      ! Clean up
      deallocate(Cbuf)
      deallocate(Rbuf)

      ! Fade to zero in the buffer region
      ! call fade_borders(SC_init,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
   end subroutine doubledelta_SCinit

   !> Initialization of problem solver
   subroutine simulation_init
      use param, only: param_read,param_getsize,param_exists
      implicit none

      ! Create a low-Mach flow solver with bconds
      create_velocity_solver: block
         use hypre_str_class, only: pcg_pfmg, smg
         use lowmach_class, only: dirichlet, clipped_neumann, slip, neumann
         real(WP) :: visc
         ! Create flow solver
         fs = lowmach(cfg=cfg, name='Variable density low Mach NS')
         ! Assign constant viscosity
         ! call param_read('Dynamic viscosity', visc); fs%visc = visc
         ! Use slip on the sides with correction
         call fs%add_bcond(name='ym_outflow', type=neumann, face='y', dir=-1, canCorrect=.True., locator=ym_locator)
         call fs%add_bcond(name='yp_outflow', type=neumann, face='y', dir=+1, canCorrect=.True., locator=yp_locator)
         ! Outflow on the right
         call fs%add_bcond(name='xm_outflow', type=neumann, face='x', dir=-1, canCorrect=.True., locator=xm_locator)
         call fs%add_bcond(name='xp_outflow', type=neumann, face='x', dir=+1, canCorrect=.True., locator=xp_locator)
         ! ! Configure pressure solver
         ps = hypre_str(cfg=cfg, name='Pressure', method=smg, nst=7)
         ps%maxlevel = 18
         call param_read('Pressure iteration', ps%maxit)
         call param_read('Pressure tolerance', ps%rcvg)
         ! Configure implicit velocity solver
         vs = ddadi(cfg=cfg, name='Velocity', nst=7)
         ! Setup the solver
         call fs%setup(pressure_solver=ps, implicit_solver=vs)
      end block create_velocity_solver

      ! Create a scalar solver
      create_fc: block
         use multivdscalar_class, only: dirichlet, neumann, quick, bquick
         real(WP) :: diffusivity
         ! Create scalar solver
         fc = finitechem(cfg=cfg, scheme=bquick, name='fc')
         ! Outflow on the right
         call fc%add_bcond(name='xm_outflow', type=neumann, locator=xm_scalar, dir='-x')
         call fc%add_bcond(name='xp_outflow', type=neumann, locator=xp_locator, dir='+x')
         call fc%add_bcond(name='ym_outflow', type=neumann, locator=ym_scalar, dir='-y')
         call fc%add_bcond(name='yp_outflow', type=neumann, locator=yp_locator, dir='+y')
         ! Assign constant diffusivity
         ! call param_read('Dynamic diffusivity', diffusivity)
         ! fc%diff = diffusivity
         ! Configure implicit scalar solver
         ss = ddadi(cfg=cfg, name='Scalar', nst=13)
         ! Setup the solver
         call fc%setup(implicit_solver=ss)

      end block create_fc

      ! Allocate work arrays
      allocate_work_arrays: block
         ! Flow solver
         allocate (resU(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resV(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resW(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resRHO(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Ui(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Vi(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Wi(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (SR(1:6, cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))
         allocate(SC_init(fc%cfg%imin:fc%cfg%imax,fc%cfg%jmin:fc%cfg%jmax,fc%cfg%kmin:fc%cfg%kmax))
         allocate (gradU(1:3, 1:3, cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))
         ! Scalar solver
         allocate (resSC(fc%cfg%imino_:fc%cfg%imaxo_, fc%cfg%jmino_:fc%cfg%jmaxo_, fc%cfg%kmino_:fc%cfg%kmaxo_, fc%nscalar))
         allocate (SCtmp(fc%cfg%imino_:fc%cfg%imaxo_, fc%cfg%jmino_:fc%cfg%jmaxo_, fc%cfg%kmino_:fc%cfg%kmaxo_, fc%nscalar))

         allocate (flag(cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_, fc%nscalar))

         allocate(spec_name(nspec))

         ! Temporary scalar field for initialization
         allocate (tmp_sc(fc%cfg%imin:fc%cfg%imax, fc%cfg%jmin:fc%cfg%jmax, fc%cfg%kmin:fc%cfg%kmax))
      end block allocate_work_arrays

      ! Initialize time tracker
      initialize_timetracker: block
         use messager, only: die
         time = timetracker(amRoot=fs%cfg%amRoot,name='FC_test')
         call param_read('Max timestep size', time%dtmax)
         call param_read('Max cfl number', time%cflmax)
         call param_read('Max time', time%tmax)
         call param_read('Max iterations', time%nmax)
         call param_read('Sub-iterations', time%itmax)
         call param_read('Time stepping',time_stepping)
         if ((trim(time_stepping).ne.'explicit').and.(trim(time_stepping).ne.'implicit')) call die('Time stepping must be either explicit or implicit.')
         time%dt = time%dtmax
      end block initialize_timetracker

      ! Initialize our mixture fraction field
      initialize_fc: block
         use multivdscalar_class, only: bcond
         use parallel, only: MPI_REAL_WP
         integer :: n, i, j, k, ierr
         character(len=str_medium) :: fuel, oxidizer
         real(WP) :: moles_fuel
         real(WP) :: T_init, L_buffer, T_buf
         integer  :: imin,imax,jmin,jmax,kmin,kmax
         real(WP) :: tmpY
         type(bcond), pointer :: mybc


         ! Get all the species names in the mechanism
         call fcmech_get_speciesnames(spec_name)

         do i = 1, nspec
            ! ! Global species index
            ! ispec=aen%vectors(aen%ivec_spec_inds)%vector(iY_sub)
            ! Initial values
            if (param_exists('Initial '//trim(spec_name(i)))) then
               call param_read('Initial '//trim(spec_name(i)), tmpY)
               fc%SC(:,:,:,i) = tmpY
               if (cfg%amRoot) then
                  print *, "Initial ", trim(spec_name(i)), tmpY
               end if
            end if
         end do
         print*,''

         call param_read('Pressure', fc%Pthermo)

         call param_read('Buffer region length',L_buffer)
         call param_read('Buffer temperature',T_buf)

         call get_borders(L_buffer,imin,imax,jmin,jmax,kmin,kmax)

         ! Initialize the global scalar field
         if (fc%cfg%amRoot) call doubledelta_SCinit(L_buffer,imin,imax,jmin,jmax,kmin,kmax)

         ! Communicate information
         call MPI_BCAST(SC_init, fc%cfg%nx*fc%cfg%ny*fc%cfg%nz, MPI_REAL_WP, 0, fc%cfg%comm, ierr)

         tmp_sc_min = minval(SC_init)
         tmp_sc_max = maxval(SC_init)
         

         fc%SC(:,:,:, nspec+1) = 1000.0_WP


         call fc%get_density()
         call fc%get_viscosity()
         call fc%get_diffusivity()
         ! print *, maxval(fc%visc), minval(fc%visc)

         call fc%get_max()
         if (cfg%amRoot) print *, fc%visc_min, fc%visc_max
      end block initialize_fc

      ! Initialize our velocity field
      initialize_velocity: block
         use lowmach_class, only: bcond
         use random, only: random_normal
         use mathtools, only: Pi
         integer :: n, i, j, k
         type(bcond), pointer :: mybc
         ! Zero initial field
         fs%U = 0.0_WP; fs%V = 0.0_WP; fs%W = 0.0_WP

         ! Set density from scalar
         fs%rho = fc%rho
         fs%visc = fc%visc
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

      ! Add Ensight output
      create_ensight: block
         ! Create Ensight output from cfg
         ens_out = ensight(cfg=cfg, name='FC_test')
         ! Create event for Ensight output
         ens_evt = event(time=time, name='Ensight output')
         call param_read('Ensight output period', ens_evt%tper)
         ! Add variables to output
         call ens_out%add_scalar('pressure', fs%P)
         call ens_out%add_vector('velocity', Ui, Vi, Wi)
         call ens_out%add_scalar('divergence', fs%div)
         call ens_out%add_scalar('density', fc%rho)
         call ens_out%add_scalar('viscosity', fc%visc)
         call ens_out%add_scalar('thermal_diff', fc%diff(:, :, :, nspec+1))

         ! call ens_out%add_scalar('YNC12H26', fc%SC(:, :, :, sXC12H26))
         ! call ens_out%add_scalar('HMN', fc%SC(:, :, :, sHMN))

         call ens_out%add_scalar('YOH', fc%SC(:, :, :, 5))
         call ens_out%add_scalar('YO2', fc%SC(:, :, :, sO2))
         call ens_out%add_scalar('YN2', fc%SC(:, :, :, sN2))
         call ens_out%add_scalar('T', fc%SC(:, :, :, nspec+1))
         call ens_out%add_scalar('YHO2', fc%SC(:, :, :, sHO2))

         ! call ens_out%add_scalar('SRC_YNC12H26', fc%SRCchem(:, :, :, sXC12H26))
         call ens_out%add_scalar('SRC_YO2', fc%SRCchem(:, :, :, sO2))
         call ens_out%add_scalar('SRC_YHO2', fc%SRCchem(:, :, :, sHO2))
         call ens_out%add_scalar('SRC_T', fc%SRCchem(:, :, :, nspec+1))

         ! Output to ensight
         if (ens_evt%occurs()) call ens_out%write_data(time%t)
      end block create_ensight

      ! Create a monitor file
      create_monitor: block
         ! Prepare some info about fields
         call fs%get_cfl(time%dt, time%cfl)
         call fs%get_max()
         call fc%get_max()
         call fc%get_int()
         ! Create simulation monitor
         mfile = monitor(fs%cfg%amRoot, 'simulation')
         call mfile%add_column(time%n, 'Timestep number')
         call mfile%add_column(time%t, 'Time')
         call mfile%add_column(time%dt, 'Timestep size')
         call mfile%add_column(time%cfl, 'Maximum CFL')
         call mfile%add_column(fs%Umax, 'Umax')
         call mfile%add_column(fs%Vmax, 'Vmax')
         call mfile%add_column(fs%Wmax, 'Wmax')
         call mfile%add_column(fs%Pmax, 'Pmax')
         ! call mfile%add_column(fc%SCmax, 'Zmax')
         ! call mfile%add_column(fc%SCmin, 'Zmin')
         call mfile%add_column(fc%SCmax(nspec + 1), 'Tmax')
         call mfile%add_column(fc%rhomax, 'RHOmax')
         call mfile%add_column(fc%rhomin, 'RHOmin')
         call mfile%add_column(fs%divmax, 'Maximum divergence')
         call mfile%add_column(fs%psolv%it, 'Pressure iteration')
         call mfile%add_column(fs%psolv%rerr, 'Pressure error')
         call mfile%write()
         ! Create CFL monitor
         cflfile = monitor(fs%cfg%amRoot, 'cfl')
         call cflfile%add_column(time%n, 'Timestep number')
         call cflfile%add_column(time%t, 'Time')
         call cflfile%add_column(fs%CFLc_x, 'Convective xCFL')
         call cflfile%add_column(fs%CFLc_y, 'Convective yCFL')
         call cflfile%add_column(fs%CFLc_z, 'Convective zCFL')
         call cflfile%add_column(fs%CFLv_x, 'Viscous xCFL')
         call cflfile%add_column(fs%CFLv_y, 'Viscous yCFL')
         call cflfile%add_column(fs%CFLv_z, 'Viscous zCFL')
         call cflfile%write()
         ! Create CFL monitor
         fcfile = monitor(fs%cfg%amRoot, 'fc')
         call fcfile%add_column(time%n, 'Timestep number')
         call fcfile%add_column(time%t, 'Time')
         call fcfile%add_column(fs%CFLc_x, 'Min Temperature')
         call fcfile%add_column(fs%CFLc_y, 'Max Temperature')
         call fcfile%add_column(fs%CFLc_z, 'Min sumY')
         call fcfile%add_column(fs%CFLv_x, 'Max sumY')
         call fcfile%add_column(fc%rhomax, 'RHOmax')
         call fcfile%add_column(fc%rhomin, 'RHOmin')
         call fcfile%write()
         ! Create conservation monitor
         consfile = monitor(fs%cfg%amRoot, 'conservation')
         call consfile%add_column(time%n, 'Timestep number')
         call consfile%add_column(time%t, 'Time')
         ! call consfile%add_column(fc%SCint, 'fc integral')
         call consfile%add_column(fc%rhoint, 'RHO integral')
         ! call consfile%add_column(fc%rhoSCint, 'rhoSC integral')
         call consfile%write()
      end block create_monitor

   end subroutine simulation_init

   !> Perform an NGA2 simulation
   subroutine simulation_run
      implicit none
      integer :: i,j,k
      real(WP), dimension(nspec) :: wdot_test

      ! Perform time integration
      do while (.not. time%done())

         ! Increment time
         call fs%get_cfl(time%dt, time%cfl)
         call time%adjust_dt()
         call time%increment()

         ! Remember old scalar

         fc%rhoold = fc%rho
         fc%SCold = fc%SC

         ! Remember old velocity and momentum
         fs%rhoold = fs%rho
         fs%Uold = fs%U; fs%rhoUold = fs%rhoU
         fs%Vold = fs%V; fs%rhoVold = fs%rhoV
         fs%Wold = fs%W; fs%rhoWold = fs%rhoW

         ! Apply time-varying Dirichlet conditions
         ! This is where time-dpt Dirichlet would be enforced
         ! t1 = parallel_time()
         
         fc%SRCchem = 0.0_WP
         call fc%react(time%dt)

         fc%SC = fc%SCold + fc%SRCchem 

         print *, ""
         print *, time%n, time%t
         print *, "T",fc%SC(1,1,1,nspec+1)
         print *, "Tsrc",fc%SRCchem(1,1,1,nspec+1)
         ! print *,"Fuel",fc%SC(1,1,1,sNXC12H26)
         ! print *,"Fuelsrc",fc%SRCchem(1,1,1,sNXC12H26)
         print *, "O2",fc%SC(1,1,1,sO2)
         print *, ""
         

         ! call fc%get_density()
         ! call fc%get_viscosity()
         ! call fc%get_diffusivity()

         call fc%get_max()

       
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
      deallocate (resSC, resU, resV, resW, Ui, Vi, Wi)

   end subroutine simulation_final

end module simulation
