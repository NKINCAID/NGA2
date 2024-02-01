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
   type(monitor) :: mfile, cflfile, consfile, fcfile,statfile

   character(len=str_medium), dimension(:), allocatable :: spec_name


   public :: simulation_init, simulation_run, simulation_final

   !> Private work arrays
   real(WP), dimension(:, :, :), allocatable :: resU, resV, resW, resRHO
   real(WP), dimension(:, :, :), allocatable :: Ui, Vi, Wi
   real(WP), dimension(:,:,:),   allocatable :: SC_init,U_init,V_init !< Initial condition for scalar and velocity fields
   real(WP), dimension(:,:,:),   allocatable :: tmpfield              !< Temporary field for statistics
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

   !> Statistics
   real(WP) :: rhomean,Tmean,Trms,Tmax

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

   !> Initialize PP spectrum for velocity
   subroutine PP_spectrum(Lbu,Lfd,Ut,le)
      use precision
      use param,    only: param_read
      use random,   only: random_normal,random_uniform
      use, intrinsic :: iso_c_binding
      implicit none

      ! Buffer and faded region lenght
      real(WP), intent(in) :: Lbu,Lfd
      ! Turbulent velocity
      real(WP) :: Ut
      ! Spectrum type
      real(WP) :: le
      ! Spectrum computation
      real(WP) :: psr,ps1,ps2,ke,dk,kc,kk,kx,ky,kz,kk2
      real(WP) :: spec_amp,eps,amp_disc,energy_spec
      complex(WP), dimension(:,:,:), pointer :: ak,bk
      ! Cutoff wave number
      integer  :: nk
      ! Complex buffer
      complex(WP), dimension(:,:,:), pointer :: Cbuf
      ! Real buffer
      real(WP), dimension(:,:,:), pointer :: Rbuf
      ! Other
      integer :: i,j,k
      integer :: imin,imax,jmin,jmax,kmin,kmax,nx,ny,nz
      complex(WP) :: ii=(0.0_WP,1.0_WP)
      real(WP) :: rand,pi
      ! Fourier coefficients
      integer(KIND=8) :: plan_r2c,plan_c2r
      complex(WP), dimension(:,:,:), pointer :: Uk,Vk

      include 'fftw3.f03'

      ! Create pi
      pi=acos(-1.0_WP)

      ! Find bounds of the region to be initialized
      call get_borders(Lbu,imin,imax,jmin,jmax,kmin,kmax)

      ! Number of cells iniside the initialization region
      nx=imax-imin+1
      ny=jmax-jmin+1
      nz=kmax-kmin+1
      nk=nx/2+1

      ! Spectrum computation
      ke=2.0_WP*pi/le
      dk=2.0_WP*pi/(Lx-2.0_WP*Lbu)
      kc=real(nx/2,WP)*dk

      eps=ke/1000000.0_WP
      spec_amp=(32.0_WP/3.0_WP)*sqrt(2.0_WP/pi)*Ut**2/ke
      amp_disc=sqrt(dk)**3

      ! Compute spectrum
      allocate(ak(nk,ny,nz),bk(nk,ny,nz))
      do k=1,nz
         do j=1,ny
            do i=1,nk
               ! Random numbers
               call random_number(rand)
               psr=2.0_WP*pi*(rand-0.5_WP)
               call random_number(rand)
               ps1=2.0_WP*pi*(rand-0.5_WP)
               call random_number(rand)
               ps2=2.0_WP*pi*(rand-0.5_WP)
               ! Wavenumbers
               kx=real(i-1,WP)*dk
               ky=real(j-1,WP)*dk
               if (j.gt.nk) ky=-real(nx+1-j,WP)*dk
               kz=real(k-1,WP)*dk
               if (k.gt.nk) kz=-real(nx+1-k,WP)*dk
               kk=sqrt(kx**2+ky**2+kz**2)
               ! Spectrums
               energy_spec=spec_amp*(kk/ke)**4*exp(-2.0_WP*(kk/ke)**2)
               ! Coeff
               ak(i,j,k)=0.0_WP
               bk(i,j,k)=0.0_WP
               if ((kk.gt.eps).and.(kk.le.kc)) then
                  ak(i,j,k)=dk*sqrt(energy_spec/(1.0_WP*pi*kk**1))*exp(ii*ps1)
               end if
            end do
         end do
      end do

      ! Compute 3D field
      allocate(Uk(nk,ny,nz))
      allocate(Vk(nk,ny,nz))
      Uk=(0.0_WP,0.0_WP)
      Vk=(0.0_WP,0.0_WP)

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

               if ((kk.gt.eps).and.(kk.le.kc)) then
                  if (kk2.lt.eps) then
                     Uk(i,j,k)=(ak(i,j,k)+bk(i,j,k))/sqrt(2.0_WP)
                     Vk(i,j,k)=(bk(i,j,k)-ak(i,j,k))/sqrt(2.0_WP)
                  else
                     Uk(i,j,k)=(ak(i,j,k)*kk*ky+bk(i,j,k)*kx*kz)/(kk*kk2)
                     Vk(i,j,k)=(bk(i,j,k)*ky*kz-ak(i,j,k)*kk*kx)/(kk*kk2)
                  end if
               end if
            end do
         end do
      end do

      ! Oddball
      do j=nk+1,ny
         Uk(1,j,1)=conjg(Uk(1,ny+2-j,1))
         Vk(1,j,1)=conjg(Vk(1,ny+2-j,1))
      end do

      ! Inverse Fourier transform
      allocate(Cbuf(nk,ny,nz))
      allocate(Rbuf(nx,ny,nz))
      call dfftw_plan_dft_c2r_2d(plan_c2r,nx,ny,Cbuf,Rbuf,FFTW_ESTIMATE)
      call dfftw_plan_dft_r2c_2d(plan_r2c,nx,ny,Rbuf,Cbuf,FFTW_ESTIMATE)

      ! Set zero in the buffer region
      U_init=0.0_WP
      V_init=0.0_WP

      ! Execute the plans
      Cbuf=Uk
      call dfftw_execute(plan_c2r)
      U_init(imin:imax,jmin:jmax,kmin:kmax)=Rbuf
      Cbuf=Vk
      call dfftw_execute(plan_c2r)
      V_init(imin:imax,jmin:jmax,kmin:kmax)=Rbuf

      ! Clean up
      deallocate(Uk)
      deallocate(Vk)
      deallocate(ak)
      deallocate(bk)

      ! Fade to zero in the buffer region
      ! call fade_borders(U_init,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
      ! call fade_borders(V_init,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
   end subroutine PP_spectrum

   !> Initialization of problem solver
   subroutine simulation_init
      use param, only: param_read,param_getsize,param_exists
      implicit none

      call param_read('Buffer region length',L_buffer)
      
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
         real(WP) :: diffusivity,bundleref
         ! Create scalar solver
         fc = finitechem(cfg=cfg, scheme=bquick, name='fc')
         ! Scheduler
         call param_read('Use scheduler',fc%use_scheduler)
         call param_read('Bundle Refinement',bundleref)
         call fc%scheduler_init(bundleref)
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
         allocate(U_init (fs%cfg%imin:fs%cfg%imax,fs%cfg%jmin:fs%cfg%jmax,fs%cfg%kmin:fs%cfg%kmax))
         allocate(V_init (fs%cfg%imin:fs%cfg%imax,fs%cfg%jmin:fs%cfg%jmax,fs%cfg%kmin:fs%cfg%kmax))
         allocate (gradU(1:3, 1:3, cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))
         ! Scalar solver
         allocate (resSC(fc%cfg%imino_:fc%cfg%imaxo_, fc%cfg%jmino_:fc%cfg%jmaxo_, fc%cfg%kmino_:fc%cfg%kmaxo_, fc%nscalar))
         allocate (SCtmp(fc%cfg%imino_:fc%cfg%imaxo_, fc%cfg%jmino_:fc%cfg%jmaxo_, fc%cfg%kmino_:fc%cfg%kmaxo_, fc%nscalar))

         allocate (flag(cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_, fc%nscalar))

         allocate(spec_name(nspec))

         ! Temporary arrays
         allocate (tmp_sc(fc%cfg%imin:fc%cfg%imax, fc%cfg%jmin:fc%cfg%jmax, fc%cfg%kmin:fc%cfg%kmax))
         allocate(tmpfield(fc%cfg%imin:fc%cfg%imax,fc%cfg%jmin:fc%cfg%jmax,fc%cfg%kmin:fc%cfg%kmax)); tmpfield=0.0_WP
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
         integer  :: imin,imax,jmin,jmax,kmin,kmax
         real(WP) :: tmpY
         real(WP) :: h_init,T_buf,T_range,T_min
         type(bcond), pointer :: mybc


         ! Read-in inputs
         call param_read('Buffer temperature',T_buf)
         call param_read('Temperature range',T_range)
         call param_read('Temperature lower bound',T_min)

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

         call param_read('Buffer temperature',T_buf)

         call get_borders(L_buffer,imin,imax,jmin,jmax,kmin,kmax)

         ! Initialize the global scalar field
         if (fc%cfg%amRoot) call doubledelta_SCinit(L_buffer,imin,imax,jmin,jmax,kmin,kmax)

         ! Communicate information
         call MPI_BCAST(SC_init, fc%cfg%nx*fc%cfg%ny*fc%cfg%nz, MPI_REAL_WP, 0, fc%cfg%comm, ierr)

         tmp_sc_min = minval(SC_init)
         tmp_sc_max = maxval(SC_init)
         

         ! Set initial conditions
         do k = fc%cfg%kmino_, fc%cfg%kmaxo_
            do j = fc%cfg%jmino_, fc%cfg%jmaxo_
               do i = fc%cfg%imino_, fc%cfg%imaxo_

                  if ((i.ge.imin).and.(i.le.imax).and.(j.ge.jmin).and.(j.le.jmax).and.(k.ge.kmin).and.(k.le.kmax)) then
                     fc%SC(i,j,k, nspec+1)=(SC_init(i,j,k) - tmp_sc_min) / (tmp_sc_max - tmp_sc_min) *T_range+T_min
                  else
                     fc%SC(i,j,k, nspec+1)=T_buf
                  end if
                  ! print *, i, j, k, SC_init(i,j,k), fc%SC(i,j,k, nspec+1)

               end do
            end do
         end do

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
         use parallel,      only: MPI_REAL_WP
         integer  :: n,i,j,k,ierr
         real(WP) :: Ut,le
         type(bcond), pointer :: mybc
         ! Read-in the inputs
         call param_read('Velocity fluctuation',Ut)
         call param_read('Energetic scale',le)
         ! Initialize the global velocity field
         if (fs%cfg%amRoot) call PP_spectrum(L_buffer,0.0_WP,Ut,le)
         ! Communicate information
         call MPI_BCAST(U_init,fs%cfg%nx*fs%cfg%ny*fs%cfg%nz,MPI_REAL_WP,0,fs%cfg%comm,ierr)
         call MPI_BCAST(V_init,fs%cfg%nx*fs%cfg%ny*fs%cfg%nz,MPI_REAL_WP,0,fs%cfg%comm,ierr)
         ! Set the local velocity field
         fs%U(fs%cfg%imin_:fs%cfg%imax_,fs%cfg%jmin_:fs%cfg%jmax_,fs%cfg%kmin_:fs%cfg%kmax_)=U_init(fs%cfg%imin_:fs%cfg%imax_,fs%cfg%jmin_:fs%cfg%jmax_,fs%cfg%kmin_:fs%cfg%kmax_)
         fs%V(fs%cfg%imin_:fs%cfg%imax_,fs%cfg%jmin_:fs%cfg%jmax_,fs%cfg%kmin_:fs%cfg%kmax_)=V_init(fs%cfg%imin_:fs%cfg%imax_,fs%cfg%jmin_:fs%cfg%jmax_,fs%cfg%kmin_:fs%cfg%kmax_)
         ! Sync it
         call fs%cfg%sync(fs%U)
         call fs%cfg%sync(fs%V)
         ! Release memory
         deallocate(U_init)
         deallocate(V_init)
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

      ! Initialize turbulence statistics
      initialize_stats: block
         ! Mean density
         call cfg%integrate(fc%rho,rhomean)
         rhomean=rhomean/cfg%vol_total
         ! Favre-averaged temperature
         tmpfield=fc%rho*fc%SC(:,:,:,nspec+1)
         call cfg%integrate(tmpfield,Tmean)
         Tmean=Tmean/(rhomean*cfg%vol_total)
         ! Favre-averaged temperature fluctuations
         tmpfield=fc%rho*(fc%SC(:,:,:,nspec+1)-Tmean)**2
         call cfg%integrate(tmpfield,Trms)
         Trms=sqrt(Trms/(rhomean*cfg%vol_total))
         ! Maximum temperature
         call cfg%maximum(fc%SC(:,:,:,nspec+1),Tmax)
      end block initialize_stats

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

         call ens_out%add_scalar('YNC12H26', fc%SC(:, :, :, sXC12H26))
         ! call ens_out%add_scalar('HMN', fc%SC(:, :, :, sHMN))

         call ens_out%add_scalar('YOH', fc%SC(:, :, :, 5))
         call ens_out%add_scalar('YO2', fc%SC(:, :, :, sO2))
         call ens_out%add_scalar('YN2', fc%SC(:, :, :, sN2))
         call ens_out%add_scalar('T', fc%SC(:, :, :, nspec+1))
         call ens_out%add_scalar('YHO2', fc%SC(:, :, :, sHO2))

         call ens_out%add_scalar('SRC_YNC12H26', fc%SRCchem(:, :, :, sXC12H26))
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
      integer :: i,j,k

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
         ! t2 = parallel_time()
         ! Perform sub-iterations
         do while (time%it .le. time%itmax)

            ! t3 = parallel_time()
            ! ===================================================
            scalar_solver: block
               use messager, only: die
               integer :: nsc

               call fc%metric_reset()

               fc%SRC = 0.0_WP
               ! call fc%pressure_source()

               ! Build mid-time scalar
               fc%SC = 0.5_WP*(fc%SC + fc%SCold)

               call fc%diffusive_source(time%dt)
               ! Explicit calculation of drhoSC/dt from scalar equation
               call fc%get_drhoSCdt(resSC, fs%rhoU, fs%rhoV, fs%rhoW)

               do nsc = 1, fc%nscalar
                  ! Assemble explicit residual
                  resSC(:, :, :, nsc) = time%dt*resSC(:, :, :, nsc) - 2.0_WP*fc%rho*fc%SC(:, :, :, nsc) + (fc%rho + fc%rhoold)*fc%SCold(:, :, :, nsc) + fc%rho*fc%SRCchem(:, :, :, nsc) + fc%SRC(:, :, :, nsc)
                  SCtmp(:, :, :, nsc) = 2.0_WP*fc%SC(:, :, :, nsc) - fc%SCold(:, :, :, nsc) + resSC(:, :, :, nsc)/fc%rho

               end do

               ! Apply it to get explicit scalar prediction
               do nsc = 1, fc%nscalar
                  do k = fc%cfg%kmino_, fc%cfg%kmaxo_
                     do j = fc%cfg%jmino_, fc%cfg%jmaxo_
                        do i = fc%cfg%imino_, fc%cfg%imaxo_
                           if (nsc .eq. nspec + 1) then
                              if (SCtmp(i, j, k, nsc) .le. 250.0_WP .or. SCtmp(i, j, k, nsc) .ge. 4000.0_WP) then
                                 flag(i, j, k, nsc) = .true.
                              else
                                 flag(i, j, k, nsc) = .false.
                              end if
                           else
                              if (SCtmp(i, j, k, nsc) .le. 0.0_WP .or. SCtmp(i, j, k, nsc) .ge. 1.0_WP) then
                                 flag(i, j, k, nsc) = .true.
                              else
                                 flag(i, j, k, nsc) = .false.
                              end if
                           end if
                        end do
                     end do
                  end do
               end do
               ! Adjust metrics
               call fc%metric_adjust(SCtmp, flag)

               ! Recompute drhoSC/dt
               call fc%get_drhoSCdt(resSC, fs%rhoU, fs%rhoV, fs%rhoW)
               ! resSC = -2.0_WP*(fc%SC - fc%SCold) + time%dt*resSC
               do nsc = 1, fc%nscalar
                  ! ============= SCALAR SOLVER =======================
                  ! Assemble explicit residual
                  resSC(:, :, :, nsc) = time%dt*resSC(:, :, :, nsc) - 2.0_WP*fc%rho*fc%SC(:, :, :, nsc) + (fc%rho + fc%rhoold)*fc%SCold(:, :, :, nsc) + fc%rho*fc%SRCchem(:, :, :, nsc) + fc%SRC(:, :, :, nsc)
               end do
               !    resSC(:,:,:,nsc)=time%dt*resSC(:,:,:,nsc)-2.0_WP*fc%rho*fc%SC(:,:,:,nsc) + (fc%rho+fc%rhoold)*fc%SCold(:,:,:,nsc) + fc%rho * fc%SRCchem(:,:,:,nsc)
               ! Get the residual
               if (time_stepping.eq.'implicit') then
                  ! Form implicit residual
                  call fc%solve_implicit(time%dt,resSC,fs%rhoU,fs%rhoV,fs%rhoW)
               else
                  ! Divide by density
                  do nsc=1,fc%nscalar
                     resSC(:,:,:,nsc)=resSC(:,:,:,nsc)/fc%rho
                  end do
               end if
               ! Apply these residuals
               fc%SC = 2.0_WP*fc%SC - fc%SCold + resSC
               ! Apply all boundary conditions on the resulting field
               call fc%apply_bcond(time%t, time%dt)
            end block scalar_solver
            ! =============================================
            ! ============ UPDATE PROPERTIES ====================
            ! t4 = parallel_time()
            call fc%get_density()
            ! t5 = parallel_time()
            ! call fc%rescale_density()
            call fc%get_viscosity()
            ! t6 = parallel_time()
            call fc%get_diffusivity()
            ! t7 = parallel_time()

            ! print *, " "
            ! print *, "================================================="
            ! print *, "Reaction mapping    : ", t2 - t1
            ! print *, "Scalar solver block : ", t4 - t3
            ! print *, "get_density         : ", t5 - t4
            ! print *, "get_viscosity       : ", t6 - t5
            ! print *, "get_diffusivity     : ", t7 - t6
            ! print *, "================================================="
            fs%visc = fc%visc

            ! ===================================================

            ! ============ VELOCITY SOLVER ======================

            ! Build n+1 density
            fs%rho = 0.5_WP*(fc%rho + fc%rhoold)

            ! Build mid-time velocity and momentum
            fs%U = 0.5_WP*(fs%U + fs%Uold); fs%rhoU = 0.5_WP*(fs%rhoU + fs%rhoUold)
            fs%V = 0.5_WP*(fs%V + fs%Vold); fs%rhoV = 0.5_WP*(fs%rhoV + fs%rhoVold)
            fs%W = 0.5_WP*(fs%W + fs%Wold); fs%rhoW = 0.5_WP*(fs%rhoW + fs%rhoWold)

            ! Explicit calculation of drho*u/dt from NS
            call fs%get_dmomdt(resU, resV, resW)

            ! Assemble explicit residual
            resU = time%dtmid*resU - (2.0_WP*fs%rhoU - 2.0_WP*fs%rhoUold)
            resV = time%dtmid*resV - (2.0_WP*fs%rhoV - 2.0_WP*fs%rhoVold)
            resW = time%dtmid*resW - (2.0_WP*fs%rhoW - 2.0_WP*fs%rhoWold)

            ! Get the residual
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
            fs%U = 2.0_WP*fs%U - fs%Uold + resU
            fs%V = 2.0_WP*fs%V - fs%Vold + resV
            fs%W = 2.0_WP*fs%W - fs%Wold + resW

            ! Apply other boundary conditions and update momentum
            call fs%apply_bcond(time%tmid, time%dtmid)
            call fs%rho_multiply()

            ! Solve Poisson equation
            call fc%get_drhodt(dt=time%dt, drhodt=resRHO)
            call fs%correct_mfr(drhodt=resRHO)
            call fs%get_div(drhodt=resRHO)
            fs%psolv%rhs = -fs%cfg%vol*fs%div/time%dtmid
            fs%psolv%sol = 0.0_WP
            call fs%psolv%solve()
            call fs%shift_p(fs%psolv%sol)

            ! Correct momentum and rebuild velocity
            call fs%get_pgrad(fs%psolv%sol, resU, resV, resW)
            fs%P = fs%P + fs%psolv%sol
            fs%rhoU = fs%rhoU - time%dtmid*resU
            fs%rhoV = fs%rhoV - time%dtmid*resV
            fs%rhoW = fs%rhoW - time%dtmid*resW
            call fs%rho_divide
            ! ===================================================

            ! Increment sub-iteration counter
            time%it = time%it + 1

         end do

         ! Recompute interpolated velocity and divergence
         call fs%interp_vel(Ui, Vi, Wi)
         call fc%get_drhodt(dt=time%dt,drhodt=resRHO)
         call fs%get_div(drhodt=resRHO)

         ! Post process
         if (ens_evt%occurs()) then
            ! Mean density
            call cfg%integrate(fc%rho,rhomean)
            rhomean=rhomean/cfg%vol_total
            ! Favre-averaged temperature
            tmpfield=fc%rho*fc%SC(:,:,:,nspec+1)
            call cfg%integrate(tmpfield,Tmean)
            Tmean=Tmean/(rhomean*cfg%vol_total)
            ! Favre-averaged temperature fluctuations
            tmpfield=fc%rho*(fc%SC(:,:,:,nspec+1)-Tmean)**2
            call cfg%integrate(tmpfield,Trms)
            Trms=sqrt(Trms/(rhomean*cfg%vol_total))
            ! Maximum temperature
            call cfg%maximum(fc%SC(:,:,:,nspec+1),Tmax)
            ! Output to ensight
            call ens_out%write_data(time%t)
            ! Output monitoring
            call statfile%write()
         end if

         ! Perform and output monitoring
         call fs%get_max()
         call fc%get_max()
         call fc%get_int()

         ! test: block
         !     use messager, only: die

         !     integer:: i, j, k
         !     do k = fc%cfg%kmino_, fc%cfg%kmaxo_
         !         do j = fc%cfg%jmino_, fc%cfg%jmaxo_
         !             do i = fc%cfg%imino_, fc%cfg%imaxo_
         ! print *, i, j, k, fc%rho(i, j, k), fc%SC(i, j, k, nspec + 1), fc%SC(i, j, k, sN2), fc%SC(i, j, k, sO2), fc%mask(i, j, k)
         !             end do
         !         end do
         !     end do
         !     call die("Merp")
         ! end block test
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
