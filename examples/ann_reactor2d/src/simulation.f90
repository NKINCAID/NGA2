!> Various definitions and tools for running an NGA2 simulation
module simulation
   use precision,           only: WP
   use geometry,            only: cfg,Lx,Ly,Lz
   use ddadi_class,         only: ddadi
   use hypre_str_class,     only: hypre_str
   use lowmach_class,       only: lowmach
   use vdscalar_class,      only: vdscalar
   use multivdscalar_class, only: multivdscalar
   use aencodernet_class,   only: aencodernet
   use chsourcenet_class,   only: chsourcenet
   use trnsportnet_class,   only: trnsportnet
   use timetracker_class,   only: timetracker
   use ensight_class,       only: ensight
   use event_class,         only: event
   use monitor_class,       only: monitor
   use parallel,            only: parallel_time
   use fcmech
   implicit none
   private

   !> Single low Mach flow solver and scalar solver and corresponding time tracker
   type(hypre_str),     public :: ps
   type(ddadi),         public :: vs,ss
   type(lowmach),       public :: fs
   type(multivdscalar), public :: msc
   type(timetracker),   public :: time

   !> Machine learning interface
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
   type(monitor) :: mfile,cflfile!,consfile

   !> Private work arrays
   real(WP), dimension(:,:,:),   allocatable :: resU,resV,resW,resRHO
   real(WP), dimension(:,:,:),   allocatable :: Ui,Vi,Wi
   real(WP), dimension(:,:,:),   allocatable :: SC_init
   real(WP), dimension(:,:,:,:), allocatable :: resSC,SCtmp,SC_src
   real(WP), dimension(:,:,:),   allocatable :: T                     !< Temperature
   real(WP), dimension(:),       allocatable :: Y_sub                 !< Mass fraction of species that are used by networks
   real(WP), dimension(:),       allocatable :: hY                    !< Enthalpy and mass fractions of sub species
   real(WP), dimension(:),       allocatable :: TYS                   !< Temperature, mass fractions, and source terms of sub species
   real(WP), dimension(4)                    :: trnprop               !< Transport properties: Temperature, logarithm of density, viscosity, and scalar diffusivity

   !> Flag for bquick scheme
   logical, dimension(:,:,:,:), allocatable :: bqflag

   !> Initialization zone
   real(WP) :: L_buffer,L_faded

   !> Post process for species mass fractions
   integer :: nY                                                      !< Number of output species
   character(len=str_medium), dimension(:), allocatable :: Y_name     !< Output species names
   real(WP), dimension(:,:,:,:), allocatable :: Y                     !< Output species mass fractions
   integer, dimension(:), allocatable :: iY_in_sub                    !< The indices of post-process species in the sub-species list

   !> Simulation sub-routines
   public :: simulation_init,simulation_run,simulation_final


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


   !> Initialize a double delta scalar field
   subroutine ignition_doubledelta(Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
      use precision
      use param,   only: param_read
      use random,  only: random_normal,random_uniform
      use,intrinsic :: iso_c_binding
      implicit none

      ! Buffer and faded region lenght
      real(WP),intent(in) :: Lbu,Lfd
      integer, intent(in) :: imin,imax,jmin,jmax,kmin,kmax
      real(WP) :: pi,ke, dk,kc,ks,ksk0ratio,kcksratio,kx,ky,kz,kk,f_phi,kk2
      ! Complex and real buffer
      complex(WP), dimension(:,:,:), pointer :: Cbuf
      real(WP),    dimension(:,:,:), pointer :: Rbuf
      ! Spectrum computation
      real(WP) :: spec_amp,eps, amp_disc,energy_spec
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
      call fade_borders(SC_init,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
   end subroutine ignition_doubledelta


   !> Initialize PP spectrum for velocity
   ! subroutine ignition_spectrum(Lbu,Lfd,Ut,le,ld,epsilon)
   !    use precision
   !    use param,   only: param_read
   !    use random,  only: random_normal,random_uniform
   !    use,intrinsic :: iso_c_binding
   !    implicit none

   !    ! Buffer and faded region lenght
   !    real(WP),intent(in) :: Lbu,Lfd
   !    ! Turbulent velocity
   !    real(WP) :: Ut
   !    ! Spectrum type
   !    real(WP) :: le,ld,epsilon
   !    ! Spectrum computation
   !    real(WP) :: psr,ps1,ps2,ke, dk,kc,kk,kx,ky,kz,kk2
   !    real(WP) :: spec_amp,eps, amp_disc,energy_spec
   !    complex(WP), dimension(:,:,:), pointer :: ak,bk
   !    ! Cutoff wave number
   !    integer  :: nk
   !    ! Complex buffer
   !    complex(WP), dimension(:,:,:), pointer :: Cbuf
   !    ! Real buffer
   !    real(WP), dimension(:,:,:), pointer :: Rbuf
   !    ! Other
   !    integer :: i,j,k
   !    integer :: imin,imax,jmin,jmax,kmin,kmax,nx,ny,nz
   !    complex(WP) :: ii=(0.0_WP,1.0_WP)
   !    real(WP) :: rand,pi
   !    ! Fourier coefficients
   !    integer(KIND=8) :: plan_r2c,plan_c2r
   !    complex(WP), dimension(:,:,:), pointer :: Uk,Vk

   !    include 'fftw3.f03'

   !    ! Create pi
   !    pi=acos(-1.0_WP)

   !    ! Find bounds of the region to be initialized
   !    call get_borders(Lbu,imin,imax,jmin,jmax,kmin,kmax)

   !    ! Number of cells iniside the initialization region
   !    nx=imax-imin+1
   !    ny=jmax-jmin+1
   !    nz=kmax-kmin+1
   !    nk=nx/2+1

   !    ! Spectrum computation
   !    ke=2.0_WP*pi/le
   !    dk=2.0_WP*pi/(Lx-2.0_WP*Lbu)
   !    kc=real(nx/2,WP)*dk

   !    eps=ke/1000000.0_WP
   !    spec_amp=(32.0_WP/3.0_WP)*sqrt(2.0_WP/pi)*Ut**2/ke
   !    amp_disc=sqrt(dk)**3

   !    ! Compute spectrum
   !    allocate(ak(nk,ny,nz),bk(nk,ny,nz))
   !    do k=1,nz
   !       do j=1,ny
   !          do i=1,nk
   !             ! Random numbers
   !             call random_number(rand)
   !             psr=2.0_WP*pi*(rand-0.5_WP)
   !             call random_number(rand)
   !             ps1=2.0_WP*pi*(rand-0.5_WP)
   !             call random_number(rand)
   !             ps2=2.0_WP*pi*(rand-0.5_WP)
   !             ! Wavenumbers
   !             kx=real(i-1,WP)*dk
   !             ky=real(j-1,WP)*dk
   !             if (j.gt.nk) ky=-real(nx+1-j,WP)*dk
   !             kz=real(k-1,WP)*dk
   !             if (k.gt.nk) kz=-real(nx+1-k,WP)*dk
   !             kk=sqrt(kx**2+ky**2+kz**2)
   !             ! Spectrums
   !             energy_spec=spec_amp*(kk/ke)**4*exp(-2.0_WP*(kk/ke)**2)
   !             ! Coeff
   !             ak(i,j,k)=0.0_WP
   !             bk(i,j,k)=0.0_WP
   !             if ((kk.gt.eps).and.(kk.le.kc)) then
   !                ak(i,j,k)=dk*sqrt(energy_spec/(1.0_WP*pi*kk**1))*exp(ii*ps1)
   !             end if
   !          end do
   !       end do
   !    end do

   !    ! Compute 3D field
   !    allocate(Uk(nk,ny,nz))
   !    allocate(Vk(nk,ny,nz))
   !    ! allocate(Wk(nk,ny,nz))
   !    Uk=(0.0_WP,0.0_WP)
   !    Vk=(0.0_WP,0.0_WP)

   !    ! Compute the Fourier coefficients
   !    do k=1,nz
   !       do j=1,ny
   !          do i=1,nk
   !             ! Wavenumbers
   !             kx=real(i-1,WP)*dk
   !             ky=real(j-1,WP)*dk
   !             if (j.gt.nk) ky=-real(nx+1-j,WP)*dk
   !             kz=real(k-1,WP)*dk
   !             if (k.gt.nk) kz=-real(nx+1-k,WP)*dk
   !             kk =sqrt(kx**2+ky**2+kz**2)
   !             kk2=sqrt(kx**2+ky**2)

   !             if ((kk.gt.eps).and.(kk.le.kc)) then
   !                if (kk2.lt.eps) then
   !                   Uk(i,j,k)=(ak(i,j,k)+bk(i,j,k))/sqrt(2.0_WP)
   !                   Vk(i,j,k)=(bk(i,j,k)-ak(i,j,k))/sqrt(2.0_WP)
   !                else
   !                   Uk(i,j,k)=(ak(i,j,k)*kk*ky+bk(i,j,k)*kx*kz)/(kk*kk2)
   !                   Vk(i,j,k)=(bk(i,j,k)*ky*kz-ak(i,j,k)*kk*kx)/(kk*kk2)
   !                end if
   !             end if
   !          end do
   !       end do
   !    end do

   !    ! Oddball
   !    do j=nk+1,ny
   !       Uk(1,j,1)=conjg(Uk(1,ny+2-j,1))
   !       Vk(1,j,1)=conjg(Vk(1,ny+2-j,1))
   !    end do

   !    ! Inverse Fourier transform
   !    allocate(Cbuf(nk,ny,nz))
   !    allocate(Rbuf(nx,ny,nz))
   !    call dfftw_plan_dft_c2r_2d(plan_c2r,nx,ny,Cbuf,Rbuf,FFTW_ESTIMATE)
   !    call dfftw_plan_dft_r2c_2d(plan_r2c,nx,ny,Rbuf,Cbuf,FFTW_ESTIMATE)

   !    ! Set zero in the buffer region
   !    tmp_U=0.0_WP
   !    tmp_V=0.0_WP

   !    ! Execute the plans
   !    Cbuf=Uk
   !    call dfftw_execute(plan_c2r)
   !    tmp_U(imin:imax,jmin:jmax,kmin:kmax)=Rbuf
   !    Cbuf=Vk
   !    call dfftw_execute(plan_c2r)
   !    tmp_V(imin:imax,jmin:jmax,kmin:kmax)=Rbuf

   !    ! Clean up
   !    deallocate(Uk)
   !    deallocate(Vk)
   !    deallocate(ak)
   !    deallocate(bk)

   !    ! Fade to zero in the buffer region
   !    call fade_borders(tmp_U,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
   !    call fade_borders(tmp_V,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
   ! end subroutine ignition_spectrum


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


   !> Fade borders of the initialized region for smooth transition to ambient
   subroutine fade_borders(field,Lbu,Lfd,imin,imax,jmin,jmax,kmin,kmax)
      implicit none
      real(WP),intent(inout) :: field(cfg%imin:cfg%imax,cfg%jmin:cfg%jmax,cfg%kmin:cfg%kmax)
      real(WP),intent(in)    :: Lbu,Lfd
      integer,intent(in)     :: imin,imax,jmin,jmax,kmin,kmax
      integer  :: i,j,k
      real(WP) :: dx, dy

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
      use param, only: param_read
      implicit none


      read_input: block
      use param,   only: param_read,param_getsize
         ! Thermodynamic pressure
         ! call param_read('Pressure',Pthermo)
         ! Buffer region length
         call param_read('Buffer region length',L_buffer)
         ! Faded region length
         call param_read('Faded region length',L_faded)
         ! Species mass fractions
         nY=param_getsize('Post process species')
      end block read_input


      ! Create a low-Mach flow solver with bconds
      create_velocity_solver: block
         use hypre_str_class, only: pcg_pfmg,smg
         use lowmach_class,   only: dirichlet,neumann
         real(WP) :: visc
         ! Create flow solver
         fs=lowmach(cfg=cfg,name='Variable density low Mach NS')
         ! Boundary conditions
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


      ! Create neural networks
      create_ann: block
         use string, only: str_medium
         character(len=str_medium) :: aenfname,csnfname,trnfname
         ! Read in the data file names
         call param_read('Auto encoder'        ,aenfname)
         call param_read('Chemical source'     ,csnfname)
         call param_read('Transport properties',trnfname)
         ! The auto encoder network object
         aen=aencodernet(cfg=cfg,fdata=aenfname,name='Auto encoder network')
         call aen%print()
         ! The chemical source network object
         csn=chsourcenet(cfg=cfg,fdata=csnfname,name='Chemical source network')
         call csn%print()
         ! The transport properties network object
         trn=trnsportnet(cfg=cfg,fdata=csnfname,name='Chemical source network')
         call csn%print()
         ! Species sub-array size
         nY_sub=size(aen%vectors(aen%ivec_spec_inds)%vector)
      end block create_ann


      ! Create a multi scalar solver
      create_scalar_solver: block
         use multivdscalar_class, only: dirichlet,neumann,quick,bquick
         ! Create scalar solver
         msc=multivdscalar(cfg=cfg,scheme=bquick,nscalar=aen%nvar,name='Variable density multi scalar')
         ! Boundary conditions
         call msc%add_bcond(name='ym_outflow',type=neumann,locator=ym_locator_sc,dir='-y')
         call msc%add_bcond(name='yp_outflow',type=neumann,locator=yp_locator_sc,dir='+y')
         call msc%add_bcond(name='xm_outflow',type=neumann,locator=xm_locator_sc,dir='-x')
         call msc%add_bcond(name='xp_outflow',type=neumann,locator=xp_locator_sc,dir='+x')
         ! Configure implicit scalar solver
         ss=ddadi(cfg=cfg,name='Scalar',nst=13)
         ! Setup the solver
         call msc%setup(implicit_solver=ss)
      end block create_scalar_solver


      ! Allocate work arrays
      allocate_work_arrays: block
         ! Flow solver
         allocate (resU  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resV  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resW  (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resRHO(fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Ui    (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Vi    (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Wi    (fs%cfg%imino_:fs%cfg%imaxo_,fs%cfg%jmino_:fs%cfg%jmaxo_,fs%cfg%kmino_:fs%cfg%kmaxo_))
         ! Scalar solver
         allocate (resSC (msc%cfg%imino_:msc%cfg%imaxo_,msc%cfg%jmino_:msc%cfg%jmaxo_,msc%cfg%kmino_:msc%cfg%kmaxo_,msc%nscalar))
         allocate (SCtmp (msc%cfg%imino_:msc%cfg%imaxo_,msc%cfg%jmino_:msc%cfg%jmaxo_,msc%cfg%kmino_:msc%cfg%kmaxo_,msc%nscalar))
         allocate (bqflag(cfg%imino_:cfg%imaxo_,cfg%jmino_:cfg%jmaxo_,cfg%kmino_:cfg%kmaxo_,msc%nscalar))
         ! Initialization
         allocate (SC_init(msc%cfg%imin:msc%cfg%imax,msc%cfg%jmin:msc%cfg%jmax,msc%cfg%kmin:msc%cfg%kmax))
         ! Combustion and ANN
         allocate(T(msc%cfg%imino_:msc%cfg%imaxo_,msc%cfg%jmino_:msc%cfg%jmaxo_,msc%cfg%kmino_:msc%cfg%kmaxo_))
         allocate(SC_src(msc%cfg%imino_:msc%cfg%imaxo_,msc%cfg%jmino_:msc%cfg%jmaxo_,msc%cfg%kmino_:msc%cfg%kmaxo_,aen%nvar)); SC_src=0.0_WP
         allocate(Y_sub(nY_sub))  ; Y_sub=0.0_WP
         allocate(hY(nY_sub+1))   ; hY   =0.0_WP
         allocate(TYS(2*nY_sub+1)); TYS  =0.0_WP
         allocate(spec_name(nspec))
         allocate(Y(msc%cfg%imino_:msc%cfg%imaxo_,msc%cfg%jmino_:msc%cfg%jmaxo_,msc%cfg%kmino_:msc%cfg%kmaxo_,nY))
         allocate(Y_name(nY))
         allocate(iY_in_sub(nY)); iY_in_sub=0
      end block allocate_work_arrays


      ! Initialize time tracker with 2 subiterations
      initialize_timetracker: block
         time=timetracker(amRoot=fs%cfg%amRoot,name='ann_rector2d')
         call param_read('Max timestep size',time%dtmax)
         call param_read('Max cfl number',time%cflmax)
         call param_read('Max time',time%tmax)
         call param_read('Inner iterations',time%itmax)
         time%dt=time%dtmax
      end block initialize_timetracker


      ! Initialize our scalar fields
      initialize_scalar: block
         use param,               only: param_exists,param_read
         use string,              only: str_long
         use messager,            only: die
         use multivdscalar_class, only: bcond
         use parallel,            only: MPI_REAL_WP
         integer  :: i,j,k,ierr
         integer  :: imin,imax,jmin,jmax,kmin,kmax
         integer  :: iY,isub,ispec,isub_n2
         real(WP) :: T_init,h_init,Y_HMN_init,Y_XC12H26_init,Y_N2_init,Y_O2_init,Y_n2_buf,T_buf
         real(WP), dimension(:), allocatable :: Y_init
         type(bcond), pointer :: mybc
         character(len=str_long) :: errmsg

         ! Allocate mamory for initial mass fractions
         allocate(Y_init(nY_sub)); Y_init=0.0_WP

         ! Read in initial conditions
         call param_read('Initial temperature',T_init)
         call param_read('Initial enthalpy'   ,h_init)
         call param_read('Buffer N2'          ,Y_n2_buf)
         call param_read('Buffer temperature' ,T_buf)
         do isub=1,nY_sub
            ispec=aen%vectors(aen%ivec_spec_inds)%vector(isub)
            if (trim(spec_name(isub)).eq.'N2') isub_n2=isub
            if (param_exists('Initial '//trim(spec_name(ispec)))) then
               call param_read('Initial '//trim(spec_name(ispec)),Y_init(isub))
            end if
         end do 

         ! Get the species names
         call fcmech_get_speciesnames(spec_name)
         call param_read('Post process species',Y_name)

         ! Get species indices
         do iY=1,nY
            do isub=1,nY_sub
               ispec=aen%vectors(aen%ivec_spec_inds)%vector(isub)
               if (spec_name(ispec).eq.Y_name(iY)) then
                  iY_in_sub(iY)=isub
                  exit
               end if
            end do
         end do
         do iY=1,nY
            if (iY_in_sub(iY).eq.0) then
               errmsg='Could not find '//Y_name(iY)//' in the sub-species list.'
               call die(trim(errmsg))
            end if
         end do

         ! Find bounds of the region to be initialized
         call get_borders(L_buffer,imin,imax,jmin,jmax,kmin,kmax)
         ! Initialize the global double delta scalar field
         if (msc%cfg%amRoot) call ignition_doubledelta(L_buffer,L_faded,imin,imax,jmin,jmax,kmin,kmax)
         ! Communicate information
         call MPI_BCAST(SC_init,msc%cfg%nx*msc%cfg%ny*msc%cfg%nz,MPI_REAL_WP,0,msc%cfg%comm,ierr)

         ! Set initial conditions
         do k=msc%cfg%kmin_,msc%cfg%kmax_
            do j=msc%cfg%jmin_,msc%cfg%jmax_
               do i=msc%cfg%imin_,msc%cfg%imax_

                  ! Initialize thrmochemical variables
                  h_init=SC_init(i,j,k)*h_init ! I'm not sure if this good for enthalpy + What value should buffer zone have?
                  Y_sub =SC_init(i,j,k)*Y_init
                  ! The following if statement would spoil the fading for N2 and T
                  if ((i.ge.imin).and.(i.le.imax).and.(j.ge.jmin).and.(j.le.jmax).and.(k.ge.kmin).and.(k.le.kmax)) then
                     T(i,j,k)      =T_init
                     Y_sub(isub_n2)=SC_init(i,j,k)*Y_N2_init
                  else
                     T(i,j,k)      =T_buf
                     Y_sub(isub_n2)=Y_n2_buf
                  end if

                  ! Map Y and h to the neural network scalars
                  call aen%transform_inputs([h_init,Y_sub],hY)
                  call aen%encode(hY,msc%SC(i,j,k,:))

                  ! Get transport properties
                  call trn%get_transport(msc%SC(i,j,k,:),trnprop)
                  call trn%inverse_transform_outputs(trnprop,trnprop)
                  T(i,j,k)         =trnprop(1)
                  msc%rho(i,j,k)   =trnprop(2)
                  fs%visc(i,j,k)   =trnprop(3)
                  msc%diff(i,j,k,:)=trnprop(4)
                  msc%rho(i,j,k)=exp(msc%rho(i,j,k))

                  ! Map the neural network scalars to Y (for visualization purposes)
                  call aen%decode(msc%SC(i,j,k,:),TYS)
                  call aen%inverse_transform_outputs(TYS(1:nY_sub+1),hY)
                  do iY=1,nY
                     Y(i,j,k,iY)=hY(iY_in_sub(iY)+1)
                  end do

               end do
            end do
         end do

         ! Sync fields
         call msc%cfg%sync(msc%SC)
         call msc%cfg%sync(T)
         call msc%cfg%sync(msc%rho)
         call msc%cfg%sync(msc%diff)
         call msc%cfg%sync(fs%visc)
         do iY=1,nY
            call msc%cfg%sync(Y(:,:,:,iY))
         end do

         ! Release unused memory
         deallocate(SC_init)
         deallocate(Y_init)
      end block initialize_scalar


      ! Initialize our velocity field
      initialize_velocity: block
         use lowmach_class, only: bcond
         use random, only: random_normal
         use mathtools, only: Pi
         integer :: n,i,j,k
         type(bcond), pointer :: mybc
         ! Zero initial field
         fs%U=0.0_WP; fs%V=0.0_WP; fs%W=0.0_WP
         ! Set density from scalar
         fs%rho=msc%rho
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
         integer :: iY
         ! Create Ensight output from cfg
         ens_out=ensight(cfg=cfg,name='ann_rector2d')
         ! Create event for Ensight output
         ens_evt=event(time=time,name='Ensight output')
         call param_read('Ensight output period',ens_evt%tper)
         ! Add variables to output
         call ens_out%add_scalar('pressure'   ,fs%P)
         call ens_out%add_vector('velocity'   ,Ui,Vi,Wi)
         call ens_out%add_scalar('divergence' ,fs%div)
         call ens_out%add_scalar('density'    ,msc%rho)
         call ens_out%add_scalar('viscosity'  ,fs%visc)
         call ens_out%add_scalar('temperature',T)
         do iY=1,nY
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
         call msc%get_max()
         call msc%get_int()
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
         call mfile%add_column(msc%rhomax,'RHOmax')
         call mfile%add_column(msc%rhomin,'RHOmin')
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
         ! consfile=monitor(fs%cfg%amRoot,'conservation')
         ! call consfile%add_column(time%n,'Timestep number')
         ! call consfile%add_column(time%t,'Time')
         ! call consfile%add_column(msc%SCint,'SC integral')
         ! call consfile%add_column(msc%rhoint,'RHO integral')
         ! call consfile%add_column(msc%rhoSCint,'rhoSC integral')
         ! call consfile%write()
      end block create_monitor


   end subroutine simulation_init

   !> Perform an NGA2 simulation
   subroutine simulation_run
      implicit none
      integer :: i,j,k
      integer :: isc,iY


      ! Perform time integration
      do while (.not. time%done())

         ! Increment time
         call fs%get_cfl(time%dt,time%cfl)
         call time%adjust_dt()
         call time%increment()

         ! Remember old scalar
         msc%rhoold=msc%rho
         msc%SCold=msc%SC

         ! Remember old velocity and momentum
         fs%rhoold=fs%rho
         fs%Uold=fs%U; fs%rhoUold=fs%rhoU
         fs%Vold=fs%V; fs%rhoVold=fs%rhoV
         fs%Wold=fs%W; fs%rhoWold=fs%rhoW

         ! Get the RHS from chsourcenet
         do k=msc%cfg%kmino_,msc%cfg%kmaxo_
            do j=msc%cfg%jmino_,msc%cfg%jmaxo_
               do i=msc%cfg%imino_,msc%cfg%imaxo_
                  call csn%get_src(msc%SC(i,j,k,:),SC_src(i,j,k,:))
               end do
            end do
         end do

         ! Perform sub-iterations
         do while (time%it.le.time%itmax)

            ! ============= SCALAR SOLVER ============= !

            ! Reset metric for bquick
            call msc%metric_reset()

            ! Build mid-time scalar
            msc%SC=0.5_WP*(msc%SC+msc%SCold)

            ! Explicit calculation of drhoSC/dt from scalar equation
            call msc%get_drhoSCdt(resSC,fs%rhoU,fs%rhoV,fs%rhoW)

            ! Assemble explicit residual
            do isc=1,msc%nscalar
               resSC(:,:,:,isc)=time%dt*resSC(:,:,:,isc)-2.0_WP*msc%rho*msc%SC(:,:,:,isc)+(msc%rho+msc%rhoold)*msc%SCold(:,:,:,isc)+time%dt*msc%rho*SC_src(:,:,:,isc)
               SCtmp(:,:,:,isc)=2.0_WP*msc%SC(:,:,:,isc)-msc%SCold(:,:,:,isc)+resSC(:,:,:,isc)/msc%rho
            end do

            ! Apply it to get explicit scalar prediction
            do isc=1,msc%nscalar
               do k=msc%cfg%kmino_,msc%cfg%kmaxo_
                  do j=msc%cfg%jmino_,msc%cfg%jmaxo_
                     do i=msc%cfg%imino_,msc%cfg%imaxo_
                        if ((SCtmp(i,j,k,isc).le.0.0_WP).or.(SCtmp(i,j,k,isc).ge.1.0_WP)) then
                           bqflag(i,j,k,isc)=.true.
                        else
                           bqflag(i,j,k,isc)=.false.
                        end if
                     end do
                  end do
               end do
            end do

            ! Adjust metrics
            call msc%metric_adjust(SCtmp,bqflag)

            ! Recompute drhoSC/dt
            call msc%get_drhoSCdt(resSC,fs%rhoU,fs%rhoV,fs%rhoW)

            ! Assemble explicit residual
            do isc=1,msc%nscalar
               resSC(:,:,:,isc)=time%dt*resSC(:,:,:,isc)-2.0_WP*msc%rho*msc%SC(:,:,:,isc)+(msc%rho+msc%rhoold)*msc%SCold(:,:,:,isc)+time%dt*msc%rho*SC_src(:,:,:,isc)
            end do

            ! Form implicit residual
            call msc%solve_implicit(time%dt,resSC,fs%rhoU,fs%rhoV,fs%rhoW)

            ! Apply these residuals
            msc%SC=2.0_WP*msc%SC-msc%SCold+resSC

            ! Apply boundary conditions on the resulting field
            call msc%apply_bcond(time%t,time%dt)
            ! =============================================

            ! ============ UPDATE PROPERTIES ====================

            ! Get transport properties
            do k=msc%cfg%kmino_,msc%cfg%kmaxo_
               do j=msc%cfg%jmino_,msc%cfg%jmaxo_
                  do i=msc%cfg%imino_,msc%cfg%imaxo_
                     call trn%get_transport(msc%SC(i,j,k,:),trnprop)
                     call trn%inverse_transform_outputs(trnprop,trnprop)
                     T(i,j,k)         =trnprop(1)
                     msc%rho(i,j,k)   =trnprop(2)
                     fs%visc(i,j,k)   =trnprop(3)
                     msc%diff(i,j,k,:)=trnprop(4)
                     msc%rho(i,j,k)=exp(msc%rho(i,j,k))
                  end do
               end do
            end do
            ! ===================================================

            ! ============ VELOCITY SOLVER ======================

            ! Build n+1 density
            fs%rho=0.5_WP*(msc%rho+msc%rhoold)

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

            ! Apply other boundary conditions and update momentum
            call fs%apply_bcond(time%tmid,time%dtmid)
            call fs%rho_multiply()

            ! Solve Poisson equation
            call msc%get_drhodt(dt=time%dt, drhodt=resRHO)
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
         call msc%get_drhodt(dt=time%dt, drhodt=resRHO)
         call fs%get_div(drhodt=resRHO)

         ! Map the neural network scalars to T and Y
         do k=msc%cfg%kmino_,msc%cfg%kmaxo_
            do j=msc%cfg%jmino_,msc%cfg%jmaxo_
               do i=msc%cfg%imino_,msc%cfg%imaxo_
                  call aen%decode(msc%SC(i,j,k,:),TYS)
                  call aen%inverse_transform_outputs(TYS(1:nY_sub+1),hY)
                  T(i,j,k)=hY(1)
                  do iY=1,nY
                     Y(i,j,k,iY)=hY(iY_in_sub(iY)+1)
                  end do
               end do
            end do
         end do

         ! Output to ensight
         if (ens_evt%occurs()) call ens_out%write_data(time%t)

         ! Perform and output monitoring
         call fs%get_max()
         call msc%get_max()
         call msc%get_int()
         call mfile%write()
         call cflfile%write()
         ! call consfile%write()

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
      deallocate(Y_sub,hY,TYS)
      deallocate (resSC,resU,resV,resW,resRHO,Ui,Vi,Wi,SC_init,T)
      deallocate(resSC,SCtmp,SC_src)


   end subroutine simulation_final

end module simulation
