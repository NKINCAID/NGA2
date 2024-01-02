!> Various definitions and tools for running an NGA2 simulation
module simulation
   use precision,         only: WP
   use geometry,          only: cfg,Lx,Ly,Lz,boundary_condition
   use ddadi_class,       only: ddadi
   use hypre_str_class,   only: hypre_str
   use lowmach_class,     only: lowmach
   use vdscalar_class,    only: vdscalar
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

   !> Ensight postprocessing
   type(ensight) :: ens_out
   type(event)   :: ens_evt

   !> Simulation monitor file
   type(monitor) :: mfile,cflfile,consfile

   public :: simulation_init,simulation_run,simulation_final

   !> Private work arrays
   real(WP), dimension(:,:,:),     allocatable :: resU,resV,resW,resSC
   real(WP), dimension(:,:,:),     allocatable :: Ui,Vi,Wi
   real(WP), dimension(:,:,:,:),   allocatable :: SR
   real(WP), dimension(:,:,:,:,:), allocatable :: gradU
   real(WP), dimension(:,:,:),     allocatable :: tmp_sc,tmp_U,tmp_V

   !> Equation of state
   real(WP) :: rho0,rho1
   real(WP) :: Z_spot,Z_air
   real(WP) :: radius,shift

   !> Integral of pressure residual
   real(WP) :: int_RP=0.0_WP

   !> Fluid, forcing, and particle parameters
   real(WP) :: visc,meanU,meanV,meanW
   real(WP) :: Urms0,TKE0,EPS0,Re_max
   real(WP) :: TKE,URMS
   real(WP) :: tauinf,G,Gdtau,Gdtaui
   real(WP) :: L_buffer,L_faded

   !> For monitoring
   real(WP) :: EPS
   real(WP) :: Re_L,Re_lambda
   real(WP) :: eta,ell
   real(WP) :: dx_eta,ell_Lx,Re_ratio,eps_ratio,tke_ratio,nondtime

   ! Debug
   character(len=8) :: time_stepping


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


   !> Function that localizes the x+ boundary
   function xm_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imin) isIn=.true.
   end function xm_locator


   !> Function that localizes the x+ boundary
   function xp_locator(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imax+1) isIn=.true.
   end function xp_locator


   !> Function that localizes y- boundary
   function ym_locator_sc(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (j.eq.pg%jmin-1) isIn=.true.
   end function ym_locator_sc


   !> Function that localizes y+ boundary
   function yp_locator_sc(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (j.eq.pg%jmax+1) isIn=.true.
   end function yp_locator_sc


   !> Function that localizes jet at -x
   function xm_locator_sc(pg,i,j,k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i,j,k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imin-1) isIn=.true.
   end function xm_locator_sc


   !> Function that localizes the right domain boundary
   function xp_locator_sc(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      isIn=.false.
      if (i.eq.pg%imax+1) isIn=.true.
   end function xp_locator_sc


   !> Define here our equation of state
   subroutine get_rho()
      implicit none
      integer :: i,j,k
      real(WP) :: Z
      ! Calculate density
      do k=sc%cfg%kmino_,sc%cfg%kmaxo_
         do j=sc%cfg%jmino_,sc%cfg%jmaxo_
            do i=sc%cfg%imino_,sc%cfg%imaxo_
               Z=min(max(sc%SC(i,j,k),0.0_WP),1.0_WP)
               sc%rho(i,j,k)=rho0*rho1/((1.0_WP-Z)*rho1+Z*rho0)
            end do
         end do
      end do
   end subroutine get_rho


   !> Initialize a double delta field
   subroutine doubledelta_SCinit(Lbu,Lfd)
      use precision
      use param,    only: param_read
      use random,   only: random_normal,random_uniform
      use, intrinsic :: iso_c_binding
      implicit none

      ! Buffer and faded region lenght
      real(WP), intent(in) :: Lbu,Lfd
      real(WP) :: pi,ke,dk,kc,ks,ksk0ratio,kcksratio,kx,ky,kz,kk,f_phi,kk2
      ! Complex and real buffer
      complex(WP), dimension(:,:,:), pointer :: Cbuf
      real(WP), dimension(:,:,:), pointer :: Rbuf
      ! Spectrum computation
      real(WP) :: spec_amp,eps,amp_disc,energy_spec
      complex(WP), dimension(:,:,:), pointer :: ak,bk
      ! Other
      integer     :: i,j,k,nk,imin,imax,jmin,jmax,kmin,kmax,nx,ny,nz
      complex(WP) :: ii=(0.0_WP,1.0_WP)
      real(WP)    :: rand
      ! Fourier coefficients
      integer(KIND=8) :: plan_r2c,plan_c2r

      include 'fftw3.f03'

      ! Create pi
      pi=acos(-1.0_WP)

      ! Step size for wave number
      dk=2.0_WP*pi/(Lx-2.0_WP*Lbu)

      ! Find bounds of the region to be initialized
      call get_borders(Lbu,imin,imax,jmin,jmax,kmin,kmax)

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
                  Cbuf(i,j,k)=Cbuf(i,j,k) * 1.0_WP
               else
                  Cbuf(i,j,k)=Cbuf(i,j,k) * (kc/kk)**2
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
      tmp_sc=0.0_WP
      ! Set the internal scalar field
      tmp_sc(imin:imax,jmin:jmax,kmin:kmax)=Rbuf/real(nx*ny*nz,WP)
      ! tmp_sc(imin:imax,jmin:jmax,kmin:kmax)=1.0_WP

      ! Destroy the plans
      call dfftw_destroy_plan(plan_c2r)
      call dfftw_destroy_plan(plan_r2c)

      ! Clean up
      deallocate(Cbuf)
      deallocate(Rbuf)

      ! Fade to zero in the buffer region
      call fade_borders(tmp_sc,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
   end subroutine doubledelta_SCinit


   !> Initialize PP spectrum for velocity
   subroutine PP_spectrum(Lbu,Lfd,Ut,le,ld,epsilon)
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
      real(WP) :: le,ld,epsilon
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
      complex(WP), dimension(:,:,:), pointer :: Uk,Vk!,Wk

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
      ! allocate(Wk(nk,ny,nz))
      Uk=(0.0_WP,0.0_WP)
      Vk=(0.0_WP,0.0_WP)
      ! Wk=(0.0_WP,0.0_WP)

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
      tmp_U=0.0_WP
      tmp_V=0.0_WP
      ! tmp_W=0.0_WP

      ! Execute the plans
      Cbuf=Uk
      call dfftw_execute(plan_c2r)
      tmp_U(imin:imax,jmin:jmax,kmin:kmax)=Rbuf
      Cbuf=Vk
      call dfftw_execute(plan_c2r)
      tmp_V(imin:imax,jmin:jmax,kmin:kmax)=Rbuf
      ! Cbuf=Wk
      ! call dfftw_execute(plan_c2r)
      ! tmp_W(imin:imax,jmin:jmax,kmin:kmax)=Rbuf

      ! Clean up
      deallocate(Uk)
      deallocate(Vk)
      ! deallocate(Wk)
      deallocate(ak)
      deallocate(bk)

      ! Fade to zero in the buffer region
      call fade_borders(tmp_U,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
      call fade_borders(tmp_V,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
   end subroutine PP_spectrum


   !> Find the border indices of the initialization region
   subroutine get_borders(Lbu,imin,imax,jmin,jmax,kmin,kmax)
      implicit none
      real(WP), intent(in) :: Lbu
      integer, intent(out) :: imin,imax,jmin,jmax,kmin,kmax
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


   !> Fade borders of the initialized region for smooth transition to ambient
   subroutine fade_borders(field,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
      implicit none
      real(WP), intent(inout) :: field(cfg%imin:cfg%imax,cfg%jmin:cfg%jmax,cfg%kmin:cfg%kmax)
      real(WP), intent(in)    :: Lbu,Lfd
      integer, intent(in)     :: imin,imax,jmin,jmax,kmin,kmax
      integer  :: i,j,k
      real(WP) :: dx,dy

      ! Check the lengths
      if ((Lbu.gt.0.0_WP).and.(Lfd.gt.0.0_WP)) then

         ! Left side
         do i=cfg%imin,imin-1
            dx=abs(cfg%xm(i)-cfg%xm(imin))
            if (dx.le.Lfd) then
               do j=jmin,jmax
                  field(i,j,kmin:kmax)=field(imin,j,kmin:kmax)*exp(-(dx/(dx-Lfd))**2)
               end do
            end if
         end do

         ! Right side
         do i=imax+1,cfg%imax
            dx=abs(cfg%xm(i)-cfg%xm(imax))
            if (dx.le.Lfd) then
               do j=jmin,jmax
                  field(i,j,kmin:kmax)=field(imax,j,kmin:kmax)*exp(-(dx/(dx-Lfd))**2)
               end do
            end if
         end do

         ! Bottom side
         do j=cfg%jmin,jmin-1
            dy=abs(cfg%ym(j)-cfg%ym(jmin))
            if (dy.le.Lfd) then
               do i=imin,imax
                  field(i,j,kmin:kmax)=field(i,jmin,kmin:kmax)*exp(-(dy/(dy-Lfd))**2)
               end do
            end if
         end do

         ! Top side
         do j=jmax+1,cfg%jmax
            dy=abs(cfg%ym(j)-cfg%ym(jmax))
            if (dy.le.Lfd) then
               do i=imin,imax
                  field(i,j,kmin:kmax)=field(i,jmax,kmin:kmax)*exp(-(dy/(dy-Lfd))**2)
               end do
            end if
         end do

         ! Left bottom corner
         do j=cfg%jmin,jmin-1
            dy=abs(cfg%ym(j)-cfg%ym(jmin))
            if (dy.le.Lfd) then
               do i=cfg%imin,imin-1
                  dx=abs(cfg%xm(i)-cfg%xm(imin))
                  if (dx.le.Lfd) then
                     field(i,j,kmin:kmax)=(dy*field(imin,j,kmin:kmax)*exp(-(dx/(dx-Lfd))**2)+dx*field(i,jmin,kmin:kmax)*exp(-(dy/(dy-Lfd))**2))/(dx+dy)
                  end if
               end do
            end if
         end do

         ! Left top corner
         do j=jmax+1,cfg%jmax
            dy=abs(cfg%ym(j)-cfg%ym(jmax))
            if (dy.le.Lfd) then
               do i=cfg%imin,imin-1
                  dx=abs(cfg%xm(i)-cfg%xm(imin))
                  if (dx.le.Lfd) then
                     field(i,j,kmin:kmax)=(dy*field(imin,j,kmin:kmax)*exp(-(dx/(dx-Lfd))**2)+dx*field(i,jmax,kmin:kmax)*exp(-(dy/(dy-Lfd))**2))/(dx+dy)
                  end if
               end do
            end if
         end do

         ! Right top corner
         do j=jmax+1,cfg%jmax
            dy=abs(cfg%ym(j)-cfg%ym(jmax))
            if (dy.le.Lfd) then
               do i=imax+1,cfg%imax
                  dx=abs(cfg%xm(i)-cfg%xm(imax))
                  if (dx.le.Lfd) then
                     field(i,j,kmin:kmax)=(dy*field(imax,j,kmin:kmax)*exp(-(dx/(dx-Lfd))**2)+dx*field(i,jmax,kmin:kmax)*exp(-(dy/(dy-Lfd))**2))/(dx+dy)
                  end if
               end do
            end if
         end do

         ! Right bottom corner
         do j=cfg%jmin,jmin-1
            dy=abs(cfg%ym(j)-cfg%ym(jmin))
            if (dy.le.Lfd) then
               do i=imax+1,cfg%imax
                  dx=abs(cfg%xm(i)-cfg%xm(imax))
                  if (dx.le.Lfd) then
                     field(i,j,kmin:kmax)=(dy*field(imax,j,kmin:kmax)*exp(-(dx/(dx-Lfd))**2)+dx*field(i,jmin,kmin:kmax)*exp(-(dy/(dy-Lfd))**2))/(dx+dy)
                  end if
               end do
            end if
         end do

      end if
   end subroutine fade_borders


   !> Initialization of problem solver
   subroutine simulation_init
      use param,    only: param_read
      use messager, only: die
      implicit none


      ! Debug
      call param_read('Time stepping',time_stepping)
      if ((time_stepping.ne.'explicit').and.(time_stepping.ne.'implicit')) call die('Incorrect time stepping option.')


      ! Read in the buffer region length
      call param_read('Buffer region length',L_buffer)
      ! Read in the faded region length
      call param_read('Faded region length',L_faded)

      ! Read in the EOS info
      call param_read('rho0',rho0)
      call param_read('rho1',rho1)

      ! Read in inlet information
      call param_read('Spot radius',radius)
      call param_read('Spot shift',shift)
      call param_read('Z spot',Z_spot)
      call param_read('Z air',Z_air)


      ! Create a low-Mach flow solver with bconds
      create_velocity_solver: block
         use hypre_str_class, only: pcg_pfmg
         use lowmach_class,   only: dirichlet,clipped_neumann,neumann,slip
         use messager,        only: die
         real(WP) :: visc
         ! Create flow solver
         fs=lowmach(cfg=cfg,name='Variable density low Mach NS')
         ! Assign constant viscosity
         call param_read('Dynamic viscosity',visc); fs%visc=visc
         ! Boundary conditions
         select case (boundary_condition)
          case ('neumann')
            call fs%add_bcond(name='ym_outflow',type=neumann,face='y',dir=-1,canCorrect=.true.,locator=ym_locator)
            call fs%add_bcond(name='yp_outflow',type=neumann,face='y',dir=+1,canCorrect=.true.,locator=yp_locator)
            call fs%add_bcond(name='xm_outflow',type=neumann,face='x',dir=-1,canCorrect=.true.,locator=xm_locator)
            call fs%add_bcond(name='xp_outflow',type=neumann,face='x',dir=+1,canCorrect=.true.,locator=xp_locator)
          case ('clipped_neumann')
            call fs%add_bcond(name='ym_outflow',type=clipped_neumann,face='y',dir=-1,canCorrect=.true.,locator=ym_locator)
            call fs%add_bcond(name='yp_outflow',type=clipped_neumann,face='y',dir=+1,canCorrect=.true.,locator=yp_locator)
            call fs%add_bcond(name='xm_outflow',type=clipped_neumann,face='x',dir=-1,canCorrect=.true.,locator=xm_locator)
            call fs%add_bcond(name='xp_outflow',type=clipped_neumann,face='x',dir=+1,canCorrect=.true.,locator=xp_locator)
          case ('slip')
            call fs%add_bcond(name='ym_outflow',type=slip,face='y',dir=-1,canCorrect=.true.,locator=ym_locator)
            call fs%add_bcond(name='yp_outflow',type=slip,face='y',dir=+1,canCorrect=.true.,locator=yp_locator)
            call fs%add_bcond(name='xm_outflow',type=slip,face='x',dir=-1,canCorrect=.true.,locator=xm_locator)
            call fs%add_bcond(name='xp_outflow',type=slip,face='x',dir=+1,canCorrect=.true.,locator=xp_locator)
          case ('periodic')
          case default
            call die('Unknown BC')
         end select
         ! Configure pressure solver
         ps=hypre_str(cfg=cfg,name='Pressure',method=pcg_pfmg,nst=7)
         ps%maxlevel=18
         call param_read('Pressure iteration',ps%maxit)
         call param_read('Pressure tolerance',ps%rcvg)
         ! Configure implicit velocity solver
         vs=ddadi(cfg=cfg,name='Velocity',nst=7)
         vs%maxit=100
         vs%rcvg=1e-6
         ! Setup the solver
         call fs%setup(pressure_solver=ps,implicit_solver=vs)
      end block create_velocity_solver


      ! Create a scalar solver
      create_scalar: block
         use vdscalar_class, only: dirichlet,neumann,quick
         use messager,       only: die
         real(WP) :: diffusivity
         ! Create scalar solver
         sc=vdscalar(cfg=cfg,scheme=quick,name='MixFrac')
         ! Boundary conditions
         select case (boundary_condition)
          case ('neumann','clipped_neumann','slip')
            call sc%add_bcond(name='xm_outflow',type=neumann,locator=xm_locator_sc,dir='-x')
            call sc%add_bcond(name='xp_outflow',type=neumann,locator=xp_locator_sc,dir='+x')
            call sc%add_bcond(name='ym_outflow',type=neumann,locator=ym_locator_sc,dir='-y')
            call sc%add_bcond(name='yp_outflow',type=neumann,locator=yp_locator_sc,dir='+y')
          case ('periodic')
          case default
            call die('Unknown BC')
         end select
         ! Assign constant diffusivity
         call param_read('Dynamic diffusivity',diffusivity)
         sc%diff=diffusivity
         ! Configure implicit scalar solver
         ss=ddadi(cfg=cfg,name='Scalar',nst=13)
         ! Setup the solver
         call sc%setup(implicit_solver=ss)
      end block create_scalar


      ! Allocate work arrays
      allocate_work_arrays: block
         ! Flow solver
         allocate(resU(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(resV(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(resW(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(Ui  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(Vi  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(Wi  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate(SR  (1:6,cfg%imino_:cfg%imaxo_,cfg%jmino_:cfg%jmaxo_,cfg%kmino_:cfg%kmaxo_))
         allocate(gradU(1:3,1:3,cfg%imino_:cfg%imaxo_,cfg%jmino_:cfg%jmaxo_,cfg%kmino_:cfg%kmaxo_))
         ! Scalar solver
         allocate(resSC (sc%cfg%imino_:sc%cfg%imaxo_,sc%cfg%jmino_:sc%cfg%jmaxo_,sc%cfg%kmino_:sc%cfg%kmaxo_))
         ! Temporary scalar field for initialization
         allocate(tmp_sc(sc%cfg%imin:sc%cfg%imax,sc%cfg%jmin:sc%cfg%jmax,sc%cfg%kmin:sc%cfg%kmax))
         ! Temporary velocity field for initialization
         allocate(tmp_U(fs%cfg%imin:fs%cfg%imax,fs%cfg%jmin:fs%cfg%jmax,fs%cfg%kmin:fs%cfg%kmax))
         allocate(tmp_V(fs%cfg%imin:fs%cfg%imax,fs%cfg%jmin:fs%cfg%jmax,fs%cfg%kmin:fs%cfg%kmax))
      end block allocate_work_arrays


      ! Initialize time tracker with 2 subiterations
      initialize_timetracker: block
         time=timetracker(amRoot=fs%cfg%amRoot,name='ignition')
         call param_read('Max timestep size',time%dtmax)
         call param_read('Max cfl number',time%cflmax)
         call param_read('Max time',time%tmax)
         call param_read('Inner iterations',time%itmax)
         time%dt=time%dtmax
      end block initialize_timetracker


      ! Initialize our mixture fraction field
      initialize_scalar: block
         use vdscalar_class, only: bcond
         use parallel,       only: MPI_REAL_WP
         integer :: n,i,j,k,ierr
         type(bcond), pointer :: mybc
         ! Initialize the global scalar field
         if (sc%cfg%amRoot) call doubledelta_SCinit(L_buffer,L_faded)
         ! Communicate information
         call MPI_BCAST(tmp_sc,sc%cfg%nx*sc%cfg%ny*sc%cfg%nz,MPI_REAL_WP,0,sc%cfg%comm,ierr)
         ! Set the local scalar field
         sc%SC(sc%cfg%imin_:sc%cfg%imax_,sc%cfg%jmin_:sc%cfg%jmax_,sc%cfg%kmin_:sc%cfg%kmax_)=tmp_sc(sc%cfg%imin_:sc%cfg%imax_,sc%cfg%jmin_:sc%cfg%jmax_,sc%cfg%kmin_:sc%cfg%kmax_)
         ! Sync it
         call sc%cfg%sync(sc%SC)
         ! Release memory
         deallocate(tmp_sc)
         ! Compute density
         call get_rho()
      end block initialize_scalar


      ! Initialize our velocity field
      initialize_velocity: block
         use lowmach_class, only: bcond
         use parallel,      only: MPI_REAL_WP
         integer :: n,i,j,k,ierr
         type(bcond), pointer :: mybc
         ! Velocity fluctuation, length scales, epsilon
         real(WP) :: Ut,le,ld,epsilon
         ! Read in the inputs
         call param_read('Velocity fluctuation',Ut)
         call param_read('Energetic scale',le)
         call param_read('Dissipative scale',ld)
         call param_read('Dissipation',epsilon)
         ! Initialize the global velocity field
         if (fs%cfg%amRoot) call PP_spectrum(L_buffer,L_faded,Ut,le,ld,epsilon)
         ! Communicate information
         call MPI_BCAST(tmp_U,fs%cfg%nx*fs%cfg%ny*fs%cfg%nz,MPI_REAL_WP,0,fs%cfg%comm,ierr)
         call MPI_BCAST(tmp_V,fs%cfg%nx*fs%cfg%ny*fs%cfg%nz,MPI_REAL_WP,0,fs%cfg%comm,ierr)
         ! Set the local velocity field
         fs%U(fs%cfg%imin_:fs%cfg%imax_,fs%cfg%jmin_:fs%cfg%jmax_,fs%cfg%kmin_:fs%cfg%kmax_)=tmp_U(fs%cfg%imin_:fs%cfg%imax_,fs%cfg%jmin_:fs%cfg%jmax_,fs%cfg%kmin_:fs%cfg%kmax_)
         fs%V(fs%cfg%imin_:fs%cfg%imax_,fs%cfg%jmin_:fs%cfg%jmax_,fs%cfg%kmin_:fs%cfg%kmax_)=tmp_V(fs%cfg%imin_:fs%cfg%imax_,fs%cfg%jmin_:fs%cfg%jmax_,fs%cfg%kmin_:fs%cfg%kmax_)
         ! fs%U=0.0_WP
         ! fs%V=0.0_WP
         ! fs%W=0.0_WP
         ! Sync it
         call fs%cfg%sync(fs%U)
         call fs%cfg%sync(fs%V)
         call fs%cfg%sync(fs%W)
         ! Release memory
         deallocate(tmp_U)
         deallocate(tmp_V)
         ! Set density from scalar
         fs%rho=sc%rho
         ! Form momentum
         call fs%rho_multiply()
         ! Apply BCs
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
         ens_out=ensight(cfg=cfg,name='ignition')
         ! Create event for Ensight output
         ens_evt=event(time=time,name='Ensight output')
         call param_read('Ensight output period',ens_evt%tper)
         ! Add variables to output
         call ens_out%add_scalar('pressure',fs%P)
         call ens_out%add_vector('velocity',Ui,Vi,Wi)
         call ens_out%add_scalar('divergence',fs%div)
         call ens_out%add_scalar('density',sc%rho)
         call ens_out%add_scalar('mixfrac',sc%SC)
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
      ! Debug
      integer :: i,j,k

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

         ! Apply time-varying Dirichlet conditions
         ! This is where time-dpt Dirichlet would be enforced

         ! Perform sub-iterations
         do while (time%it.le.time%itmax)

            ! ============= SCALAR SOLVER =======================
            ! Build mid-time scalar
            sc%SC=0.5_WP*(sc%SC+sc%SCold)

            ! Explicit calculation of drhoSC/dt from scalar equation
            call sc%get_drhoSCdt(resSC,fs%rhoU,fs%rhoV,fs%rhoW)

            ! Assemble explicit residual
            resSC=time%dt*resSC-(2.0_WP*sc%rho*sc%SC-(sc%rho+sc%rhoold)*sc%SCold)

            ! Get the residual
            if (time_stepping.eq.'implicit') then
               ! Form implicit residual
               call sc%solve_implicit(time%dt,resSC,fs%rhoU,fs%rhoV,fs%rhoW)
            else
               ! Divide by density
               resSC=resSC/sc%rho
            end if

            ! Advance scalar field
            sc%SC=2.0_WP*sc%SC-sc%SCold+resSC

            ! Apply boundary conditions
            call sc%apply_bcond(time%t,time%dt)

            ! ===================================================

            ! ============ UPDATE PROPERTIES ====================
            ! Backup rhoSC
            ! resSC=sc%rho*sc%SC
            ! Update density
            call get_rho()
            ! Rescale scalar for conservation
            ! sc%SC=resSC/sc%rho
            ! UPDATE THE VISCOSITY
            ! UPDATE THE DIFFUSIVITY
            ! ===================================================

            ! ============ VELOCITY SOLVER ======================

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
            fs%U=2.0_WP*fs%U-fs%Uold+resU
            fs%V=2.0_WP*fs%V-fs%Vold+resV
            fs%W=2.0_WP*fs%W-fs%Wold+resW

            ! Update momentum
            call fs%rho_multiply()
            ! Apply boundary conditions
            call fs%apply_bcond(time%tmid,time%dtmid)
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
            call fs%rho_divide()

            ! ===================================================

            ! Increment sub-iteration counter
            time%it=time%it+1

         end do

         ! Recompute interpolated velocity and divergence
         call fs%interp_vel(Ui,Vi,Wi)
         ! Debug
         if (cfg%iproc.eq.1) print*,'Ui left = ',Ui(cfg%imin,201,1)
         if (cfg%iproc.eq.cfg%npx) print*,'Ui right = ',Ui(cfg%imax,201,1)
         call sc%get_drhodt(dt=time%dt,drhodt=resSC)
         call fs%get_div(drhodt=resSC)

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
      deallocate(resSC,resU,resV,resW,Ui,Vi,Wi)

   end subroutine simulation_final


end module simulation
