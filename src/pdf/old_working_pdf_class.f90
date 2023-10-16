!> Basic Lagrangian particle solver class:
!> Provides support for Lagrangian-transported objects
module pdf_class
  use precision, only: WP
  use string, only: str_medium
  use config_class, only: config
  use dvode_f90_m
  ! use reactor_class, only: reactor
  use pdf_mech
  use mpi_f08, only: MPI_Datatype, MPI_INTEGER8, MPI_INTEGER, MPI_DOUBLE_PRECISION
  implicit none
  private

  ! Expose type/constructor/methods
  public :: pdf

  type(vode_opts), save :: opts
  real(WP), parameter :: tscale = 1.0e5_WP

  !> Memory adaptation parameter
  real(WP), parameter :: coeff_up = 1.3_WP      !< Particle array size increase factor
  real(WP), parameter :: coeff_dn = 0.7_WP      !< Particle array size decrease factor

  !> I/O chunk size to read at a time
  integer, parameter :: part_chunk_size = 1000  !< Read 1000 particles at a time before redistributing

  ! Clipping values for temperature
  real(WP), parameter :: T_min = 50.0_WP
  real(WP), parameter :: Tmax = 5000.0_WP

  ! Offset for PLP
  integer :: PLPoffset

  ! Newton parameters
  real(WP) :: newton_tol = 1.0e-4_WP
  integer :: nnewton = 20

  ! DVODE variables
  real(WP) :: atol = 1.0e-8_WP
  real(WP) :: rtol = 1.0e-8_WP
  integer :: istate, itask

  ! Solver options
  logical :: use_jacanal = .true.
  logical :: use_scheduler = .false.

  ! Jacobian parameters
  integer :: njac, njac_max, idisp, del_store, actual_njac
  integer, dimension(:), allocatable :: ind_jac, ind_disp
  real(WP), dimension(:), allocatable :: dt_array
  real(WP), dimension(:, :, :), allocatable :: jac_array

  !> Basic particle object definition
  type :: pmc
    real(WP), dimension(3) :: pos    !< Particle center coordinates
    real(WP) :: vol                  !< Volume
    real(WP) :: m                    !< Mass
    real(WP) :: h                    !< Enthalpy
    real(WP) :: T                    !< Temperature
    real(WP) :: Zmix                 !< Mixture fraction
    real(WP), dimension(npS) :: Yi   !< Composition
    integer, dimension(3) :: ind     !< Index of cell containing particle center
    integer :: multi                 !< Multiplicity
  end type pmc
  !> Number of blocks, block length, and block types in a particle
  integer, parameter                         :: part_nblock = 2
  integer, dimension(part_nblock) :: part_lblock = [8 + npS, 4]
  type(MPI_Datatype), dimension(part_nblock) :: part_tblock = [MPI_DOUBLE_PRECISION, MPI_INTEGER]
  !> MPI_PART derived datatype and size
  type(MPI_Datatype) :: MPI_PART
  integer :: MPI_PART_SIZE

  !> Lagrangian particle tracking solver object definition
  type :: pdf

    ! This is our underlying config
    class(config), pointer :: cfg                       !< This is the config the solver is build for

    ! This is the name of the solver
    character(len=str_medium) :: name = 'UNNAMED_pdf'     !< Solver name (default=UNNAMED_pdf)

    ! Particle data
    integer :: np                                       !< Global number of particles
    integer :: np_                                      !< Local number of particles
    integer, dimension(:), allocatable :: np_proc       !< Number of particles on each processor
    type(pmc), dimension(:), allocatable :: p          !< Array of particles of type pmc

    ! Particle density
    real(WP) :: rho                                     !< Density of particle

    real(WP) :: Pthermo, Pthermo_old     !< Thermodynamic pressure

    ! Flow variables
    real(WP), dimension(:, :, :), allocatable :: T      !< Temperature array
    real(WP), dimension(:, :, :), allocatable :: H     !< Enthalpy array
    real(WP), dimension(:, :, :, :), allocatable :: Yi      !< Mass Fractions
    real(WP), dimension(:, :, :), allocatable :: Z     !< Mixture fraction array
    real(WP), dimension(:, :, :), allocatable :: SV     !< Specific volume array
    real(WP), dimension(:, :, :), allocatable :: V        !< Volume array
    real(WP), dimension(:, :, :), allocatable :: srcSV        !< Specific volume source array
    real(WP), dimension(:, :, :), allocatable :: multi        !< Multiplicity array
    real(WP), dimension(:, :, :), allocatable :: gammat        !< Multiplicity array

    ! Control variables
    integer :: nppc                                       !< Desired number of particles per cell
    integer, dimension(:, :, :, :), allocatable :: pincell        !< Multiplicity array
    integer, dimension(:, :, :), allocatable :: npincell        !< Multiplicity array
    integer, dimension(:, :, :), allocatable :: npincell_inject        !< Multiplicity array
    integer :: nppc_min, nppc_max, nppc_maxo, nppcm, nppcp
    real(WP) :: eta
    real(WP) :: wtmax
    real(WP), dimension(:), allocatable :: wt
    integer, dimension(:), allocatable :: iwt

    ! Solver parameters
    real(WP) :: nstep = 1                                 !< Number of substeps (default=1)
    logical :: use_reactions = .false.

    ! Monitoring info
    real(WP) :: Tmin, Tmax, Tmean, Tvar                    !< Diameter info
    integer  :: np_min, np_max, np_mean, np_max_inject, np_in, np_out, np_total                          !< Number of new and removed particles

    ! Reactor parameters
    ! Scheduler variables
    ! Identities
    integer :: ihead, inewhead
    integer, dimension(:), allocatable :: ihead_list, ihead_aware, iproc_waiting
    integer :: nbundles_max, nbundles, nproc_waiting
    ! Buffers
    real(WP), dimension(:), allocatable :: bufferR, bufferS
    integer :: nwhere_buf
    ! Locations
    integer, dimension(:, :), allocatable :: iwhere
    integer, dimension(:), allocatable :: iwhere_buf
    integer, dimension(:), allocatable :: nwhere

  contains
    procedure :: control_init
    procedure :: inject
    procedure :: control
    procedure :: react
    procedure :: mix
    procedure :: transport_step
    procedure :: transport_solve
    procedure :: get_eulerian_fields
    procedure :: get_eulerian_fields_mid

    procedure :: get_density
    procedure :: get_viscosity
    procedure :: get_diffusivity

    procedure :: get_Wmix
    procedure :: get_sv
    procedure :: get_thermodata
    procedure :: H2T
    procedure :: clip

    procedure :: get_max                                !< Extract various monitoring data
    procedure :: update_partmesh                        !< Update a partmesh object using current particles
    procedure :: sync                                   !< Synchronize particles across interprocessor boundaries
    procedure :: resize                                 !< Resize particle array to given size
    procedure :: recycle                                !< Recycle particle array by removing flagged particles
    procedure :: write                                  !< Parallel write particles to file
    procedure :: read                                   !< Parallel read particles from file

  end type pdf

  !> Declare pdf solver constructor
  interface pdf
    procedure constructor
  end interface pdf

contains

  !> Default constructor for pdf solver
  function constructor(cfg, name) result(self)
    implicit none
    type(pdf) :: self
    class(config), target, intent(in) :: cfg
    ! type(reactor), intent(in) :: reac
    character(len=*), optional :: name

    ! Set the name for the solver
    if (present(name)) self%name = trim(adjustl(name))

    ! Point to pgrid object
    self%cfg => cfg

    ! self%reactor => reac

    ! Allocate variables
    allocate (self%np_proc(1:self%cfg%nproc)); self%np_proc = 0
    self%np_ = 0; self%np = 0
    call self%resize(2)

    ! Allocate flow variables
    allocate (self%T(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%T = 0.0_WP
    allocate (self%H(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%H = 0.0_WP
    allocate (self%Z(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%Z = 0.0_WP
    allocate (self%SV(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%SV = 0.0_WP
    allocate (self%V(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%V = 0.0_WP
    allocate (self%srcSV(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%srcSV = 0.0_WP
    allocate (self%multi(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%multi = 0.0_WP
    allocate (self%gammat(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%gammat = 0.0_WP
    allocate (self%Yi(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_, npS)); self%Yi = 0.0_WP

    allocate (self%npincell(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%npincell = 0.0_WP
  allocate (self%npincell_inject(self%cfg%imino_:self%cfg%imaxo_, self%cfg%jmino_:self%cfg%jmaxo_, self%cfg%kmino_:self%cfg%kmaxo_)); self%npincell_inject = 0.0_WP

    ! Initialize MPI derived datatype for a particle
    call prepare_mpi_part()

    ! Log/screen output
    logging: block
      use, intrinsic :: iso_fortran_env, only: output_unit
      use param, only: verbose
      use messager, only: log
      use string, only: str_long
      character(len=str_long) :: message
      if (self%cfg%amRoot) then
        write (message, '("Particle solver [",a,"] on partitioned grid [",a,"]")') trim(self%name), trim(self%cfg%name)
        if (verbose .gt. 1) write (output_unit, '(a)') trim(message)
        if (verbose .gt. 0) call log(message)
        print *, "Number of species: ", npS
      end if
    end block logging

  end function constructor

  subroutine control_init(this, nppc)
    use mpi_f08, only: MPI_ALLREDUCE, MPI_MAX, MPI_MIN, MPI_SUM
    use parallel, only: MPI_REAL_WP
    implicit none
    class(pdf), intent(inout) :: this
    integer, intent(in) :: nppc
    real(WP) :: buf, safe_np
    integer :: i, j, k, ierr, bufint

    ! real(WP), parameter :: alpha = 0.70710678_WP
    ! real(WP), parameter :: beta = 1.41421356_WP

        real(WP), parameter :: alpha = 0.8_WP
    real(WP), parameter :: beta = 1.3_WP

    ! Desired number of particles per cell
    this%nppc = nppc

    ! Minimum and maximum number of particles in cell

    this%nppc_min = int(alpha*real(this%nppc))
    this%nppc_max = int(beta*real(this%nppc))

    ! Estimate instantaneaous max number of particles per cell
    this%nppc_maxo = 2*this%nppc_max
    buf = 1.0_WP

    ! Check current max number of particle-per-cell and adjust if necessary
    this%npincell = 0
    do i = 1, this%np_
         this%npincell(this%p(i)%ind(1), this%p(i)%ind(2), this%p(i)%ind(3)) = this%npincell(this%p(i)%ind(1), this%p(i)%ind(2), this%p(i)%ind(3)) + 1
    end do

    ! ! Adjust estimate of max particle-per-cell
    ! call MPI_ALLREDUCE(maxval(this%npincell), bufint, 1, MPI_INTEGER, MPI_MAX, this%cfg%comm, ierr); this%nppc_maxo = bufint

    ! Allocate particle-per-cell storage
      allocate(this%pincell(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_, this%nppc_maxo))

    ! Allocate additional temp arrays
    allocate (this%wt(this%nppc_maxo))
    allocate (this%iwt(this%nppc_maxo))
  end subroutine control_init

  !> Advance the particle equations by a specified time step dt
  subroutine transport_step(this, dt, U, V, W, rho, visc, diff)
    use messager, only: die
    implicit none
    class(pdf), intent(inout) :: this
    real(WP), intent(inout) :: dt  !< Timestep size over which to advance
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: U     !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: V     !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: W     !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: rho   !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: visc  !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: diff  !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)

    integer :: i
    real(WP) :: mydt, dt_done
    real(WP), dimension(3) :: rhs
    type(pmc) :: pold

    this%gammat = diff*this%SV
    ! Advance the equations
    do i = 1, this%np_
      ! Remember the particle
      pold = this%p(i)
      ! Advance with Euler prediction
      call this%transport_solve(dt=dt, U=U, V=V, W=W, rho=rho, visc=visc, gammat=this%gammat, p=this%p(i), rhs=rhs)
      this%p(i)%pos = pold%pos + rhs

      ! Relocalize
      this%p(i)%ind = this%cfg%get_ijk_global(this%p(i)%pos, this%p(i)%ind)
      ! Increment
      dt_done = dt_done + mydt

      ! Correct the position to take into account periodicity
      if (this%cfg%xper) then
        this%p(i)%pos(1) = this%cfg%x(this%cfg%imin) + modulo(this%p(i)%pos(1) - this%cfg%x(this%cfg%imin), this%cfg%xL)
      end if
      if (this%cfg%yper) then
        this%p(i)%pos(2) = this%cfg%y(this%cfg%jmin) + modulo(this%p(i)%pos(2) - this%cfg%y(this%cfg%jmin), this%cfg%yL)
      end if
      if (this%cfg%zper) then
        this%p(i)%pos(3) = this%cfg%z(this%cfg%kmin) + modulo(this%p(i)%pos(3) - this%cfg%z(this%cfg%kmin), this%cfg%zL)
      end if

      ! Handle particles that have left the domain
      if (this%p(i)%pos(1) .lt. this%cfg%x(this%cfg%imin) .or. this%p(i)%pos(1) .gt. this%cfg%x(this%cfg%imax + 1)) then
        this%p(i)%multi = 0
      end if
      if (this%p(i)%pos(2) .lt. this%cfg%y(this%cfg%jmin) .or. this%p(i)%pos(2) .gt. this%cfg%y(this%cfg%jmax + 1)) then
        this%p(i)%multi = 0
      end if
      if (this%p(i)%pos(3) .lt. this%cfg%z(this%cfg%kmin) .or. this%p(i)%pos(3) .gt. this%cfg%z(this%cfg%kmax + 1)) then
        this%p(i)%multi = 0
      end if
      ! Relocalize the particle
      this%p(i)%ind = this%cfg%get_ijk_global(this%p(i)%pos, this%p(i)%ind)
    end do

    ! Communicate particles
    call this%sync()

    ! Log/screen output
    logging: block
      use, intrinsic :: iso_fortran_env, only: output_unit
      use param, only: verbose
      use messager, only: log
      use string, only: str_long
      character(len=str_long) :: message
      if (this%cfg%amRoot) then
          write(message,'("Particle solver [",a,"] on partitioned grid [",a,"]: ",i0," particles were advanced")') trim(this%name),trim(this%cfg%name),this%np
        if (verbose .gt. 1) write (output_unit, '(a)') trim(message)
        if (verbose .gt. 0) call log(message)
      end if
    end block logging

  end subroutine transport_step

  !> Calculate RHS of the particle ODEs
  subroutine transport_solve(this, dt, U, V, W, rho, visc, gammat, p, rhs)
    use random, only: random_normal
    use messager, only: die

    implicit none
    class(pdf), intent(inout) :: this
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: U     !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: V     !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: W     !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: rho   !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: visc  !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: gammat  !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    type(pmc), intent(in) :: p
    real(WP), dimension(3), intent(out) :: rhs
    real(WP), intent(in) :: dt
    real(WP) :: Re, corr, tau
    real(WP) :: fvisc, frho, fgammat
    real(WP), dimension(3) :: fvel
    real(WP), dimension(3) :: random_motion
    integer :: i

    do i = 1, 3
      random_motion(i) = random_normal(m=0.0_WP, sd=1.0_WP)
    end do
    ! Interpolate the fluid phase velocity to the particle location
    fvel = this%cfg%get_velocity(pos=p%pos, i0=p%ind(1), j0=p%ind(2), k0=p%ind(3), U=U, V=V, W=W)
    ! Interpolate the fluid phase viscosity to the particle location
    fvisc = this%cfg%get_scalar(pos=p%pos, i0=p%ind(1), j0=p%ind(2), k0=p%ind(3), S=visc, bc='n')
    ! Interpolate the fluid phase density to the particle location
    frho = this%cfg%get_scalar(pos=p%pos, i0=p%ind(1), j0=p%ind(2), k0=p%ind(3), S=rho, bc='n')
    ! Interpolate the fluid phase diffusivity to the particle location
    fgammat = this%cfg%get_scalar(pos=p%pos, i0=p%ind(1), j0=p%ind(2), k0=p%ind(3), S=gammat, bc='n')

    ! print *, p%ind(1), p%ind(2), p%ind(3), gammat(p%ind(1), p%ind(2), p%ind(3)), gammat(p%ind(1), p%ind(2), p%ind(3) + 1)

    ! Return rhs
    rhs = (fvel*dt) + sqrt(2.0_WP*fgammat*dt)*random_motion
!
    if (this%cfg%nz .eq. 1) then
      rhs(3) = 0.0_WP
    end if

  end subroutine transport_solve

  subroutine inject(this, dt, itr, U, V, W, Tin, Hin, Yin, svin)
    use random, only: random_uniform
    use iterator_class, only: iterator

    implicit none
    type(iterator), intent(in) :: itr
    class(pdf), intent(inout) :: this
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(in) :: U     !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(in) :: V     !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(in) :: W     !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    real(WP), intent(in) :: dt, Tin, Hin, svin
    real(WP), dimension(npS) :: Yin

    real(WP) :: vgoal, rand, vadded, Vp, previous_error
    integer :: mynpmc_in, count, npmc_in, i, j, k, n

    count = this%np_

    ! Re-initialize number of particles injected in j,k cell
    mynpmc_in = 0
    npmc_in = 0

    vgoal = 0.0_WP

    do n = 1, itr%n_
      i = itr%map(1, n); j = itr%map(2, n); k = itr%map(3, n)

      ! Determine volume of particles to inject
      vgoal = U(i, j, k)*(this%cfg%y(j + 1) - this%cfg%y(j))*(this%cfg%z(k + 1) - this%cfg%z(k))*dt

      ! Initialize number of particles added
      vadded = 0.0_WP

      ! Initialize volume of particle for that cell
      Vp = this%cfg%dx(i)*this%cfg%dy(j)*this%cfg%dz(k)/real(this%nppc, WP)

      ! Add new particles until desired number is achieved
      loop1: do while (vadded .lt. vgoal)
        ! ------------------------------------------------------------ !
        ! Deal with the case vadded very close to vgoal: carry over
        if (vgoal - vadded .lt. 0.1_WP*Vp) exit loop1

        ! ------------------------------------------------------------ !
        ! Deal with the case vadded less than Vp from vgoal
        if (vgoal - vadded .lt. Vp) then

          ! Change volume of the added particle to match vgoal exactly
          Vp = vgoal - vadded

          ! Increment counter
          vadded = vadded + Vp
          count = count + 1
          mynpmc_in = mynpmc_in + 1

          ! Create space for new particle
          call this%resize(count)

          ! Set various parameters for the particle
          this%p(count)%multi = 1
          this%p(count)%vol = Vp

          ! Inflow-specific properties
          this%p(count)%h = Hin
          this%p(count)%T = Tin
          this%p(count)%Yi = Yin(:)
          this%p(count)%m = Vp/svin

          this%p(count)%ind = 0

          ! Give a position at the injector to the particle

          ! Uniform distribution in cartesian coordinates
          if (this%cfg%nz .eq. 1) then
            this%p(count)%pos = [random_uniform(this%cfg%x(i), this%cfg%x(i + 1)),&
            &            random_uniform(this%cfg%y(j), this%cfg%y(j + 1)),&
            &            0.0_WP]
          else
            this%p(count)%pos = [random_uniform(this%cfg%x(i), this%cfg%x(i + 1)),&
            &            random_uniform(this%cfg%y(j), this%cfg%y(j + 1)),&
            &            random_uniform(this%cfg%z(k), this%cfg%z(k + 1))]
          end if
          ! Localize the particle
          this%p(count)%ind = this%cfg%get_ijk_global(this%p(count)%pos, this%p(count)%ind)

          ! Update the added number for the timestep
          npmc_in = npmc_in + 1

          ! Done, no carry-over in this case
          exit loop1
        end if

        ! ------------------------------------------------------------ !
        ! Regular case: still particles to add, increment by full Vp
        vadded = vadded + Vp
        count = count + 1
        mynpmc_in = mynpmc_in + 1

        ! Create space for new particle
        call this%resize(count)

        ! Set various parameters for the particle
        this%p(count)%multi = 1
        this%p(count)%vol = Vp

        ! Inflow-specific properties
        this%p(count)%h = Hin
        this%p(count)%T = Tin
        this%p(count)%Yi = Yin(:)
        this%p(count)%m = Vp/svin

        ! Reset index location
        this%p(count)%ind = 0

        ! Uniform distribution in cartesian coordinates
        if (this%cfg%nz .eq. 1) then
          this%p(count)%pos = [random_uniform(this%cfg%x(i), this%cfg%x(i + 1)),&
          &            random_uniform(this%cfg%y(j), this%cfg%y(j + 1)),&
          &            0.0_WP]
        else
          this%p(count)%pos = [random_uniform(this%cfg%x(i), this%cfg%x(i + 1)),&
          &            random_uniform(this%cfg%y(j), this%cfg%y(j + 1)),&
          &            random_uniform(this%cfg%z(k), this%cfg%z(k + 1))]
        end if
        ! Localize the particle
        this%p(count)%ind = this%cfg%get_ijk_global(this%p(count)%pos, this%p(count)%ind)

        ! Update the added number for the timestep
        npmc_in = npmc_in + 1

        ! print *, i, j, k, this%cfg%x(i), this%cfg%y(j), this%cfg%z(k), this%p(count)%T
      end do loop1
      this%npincell_inject(i, j, k) = npmc_in
    end do
    ! Communicate particles
    ! call this%sync()

  end subroutine inject

  subroutine control(this)
    use messager, only: die

    implicit none
    class(pdf), intent(inout) :: this
    real(WP) :: eta
    real(WP) :: wtmax

    integer :: i, j, k, n, ic, ip, jf, jl, jlast, nc, jj, kk
    integer :: mync, multi, myip, myn, mynp, nu

    real(WP) :: cm, pm
    real(WP) :: volt, vol0, vol1, volc
    real(WP) :: mt, mass1, mass0, massc, w_max, wtc, wte
    real(WP) :: rand, mytotmass

    integer :: npmc_new_
    logical :: doit
    real(WP) :: myT, myV, myH, myM

    eta = 0.5_WP
    wtmax = 1.5_WP

    ! --------------------------------------------------- !
    ! Cell localization
    ! --------------------------------------------------- !
    call cell_localize(this)

    ! --------------------------------------------------- !
    ! Main loop, treat each cell one by one
    ! --------------------------------------------------- !
    ! Go through each cell in processor
    do k = this%cfg%kmin_, this%cfg%kmax_
      do j = this%cfg%jmin_, this%cfg%jmax_
        do i = this%cfg%imin_, this%cfg%imax_
          ! Skip masked cells
          if (this%cfg%VF(i, j, k) .eq. 0.0_WP) cycle
          ! Short-hand notation for number of pmc in cell
          mynp = this%npincell(i, j, k)
          this%nppcm = min(this%nppcm, mynp)
          this%nppcp = max(this%nppcp, mynp)

          ! --------------------------------------------------- !
          ! Empty cells
          ! --------------------------------------------------- !

          ! Catch empty cell exception ! PP IMPLEMENT
          if (mynp .eq. 0) then
            ! print *, 'i', i, 'j', j, 'k', k, 'No particle in cell, need treatment for that!'
            ! Skip the rest
            cycle
          end if

          ! --------------------------------------------------- !
          ! Multiplicity
          ! --------------------------------------------------- !

          ! Get total mass and volume in cell
          mt = 0.0_WP
          volt = 0.0_WP
          do myip = 1, mynp
            ip = this%pincell(i, j, k, myip)
            if (this%p(ip)%multi .ne. 1) call die('SOMETHING WRONG HERE!')
            mt = mt + this%p(ip)%m
            volt = volt + this%p(ip)%vol
          end do
          mass0 = mt
          vol0 = volt

          ! Check values
          if (mt .le. 0.0_WP .or. volt .le. 0.0_WP) then
            print *, 'Cell [', i, j, k, '] has zero or negative mass or volume:', mt, volt, '[', mynp, ' particles]'
            call die('Stopping there')
          end if

          ! Normalized inverse of total m and vol
          mt = eta*real(this%nppc, WP)/mt
          volt = (1.0_WP - eta)*real(this%nppc, WP)/volt

          ! Particle importance
          do myip = 1, mynp
            ip = this%pincell(i, j, k, myip)
            this%wt(myip) = this%p(ip)%m*mt + this%p(ip)%vol*volt
          end do

          ! All particles with wt>wtmax will be cloned
          myn = mynp
          do myip = 1, mynp
            ! Get particle index
            ip = this%pincell(i, j, k, myip)
            ! If above maximum weight, increase multiplicity
            if (this%wt(myip) .gt. this%wtmax) then
              !print*,'particle cloned because w>wmax',ip,wt(myip),this%p(ip)%multi,mynp,'i',i,'j',j,'k',k
              multi = int(this%wt(myip)/wtmax) + 1
              ! Update number of particles
              myn = myn + multi - 1
              eta = 1.0_WP/real(multi, WP)
              this%wt(myip) = this%wt(myip)*eta
              ! Update particle data
              this%p(ip)%multi = multi
              this%p(ip)%m = this%p(ip)%m*eta ! update particle mass
              this%p(ip)%vol = this%p(ip)%vol*eta ! update particle volume
            else
              ! Do nothing
              this%p(ip)%multi = 1
            end if
          end do

          ! ------------------------------------------ !
          ! Right number of particles in the cell
          if (myn .ge. this%nppc_min .and. myn .le. this%nppc_max) cycle

          ! ------------------------------------------ !
          ! Clone if too few particles
          if (myn .lt. this%nppc_min) then
            ! Set minimum number of particle per cell to nppc_min
            this%nppcm = this%nppc_min
            ! Number of cloned particles needed
            mync = this%nppc_min - myn
            !print*,'particle cloned because too few in cell!',myn,nppc_min,mync,'i',i,'j',j,'k',k
            ! Identify them
            do jj = 1, mync
              ! Particle with largest weight
              myip = maxloc(this%wt(1:mynp), 1)
              ip = this%pincell(i, j, k, myip)
              ! Mark it as cloned
              multi = this%p(ip)%multi
              eta = real(multi, WP)/(real(multi, WP) + 1.0_WP)
              this%wt(myip) = this%wt(myip)*eta
              this%p(ip)%multi = multi + 1
              this%p(ip)%m = this%p(ip)%m*eta ! update particle mass
              this%p(ip)%vol = this%p(ip)%vol*eta ! update particle volume
              myn = myn + 1
            end do
            if (myn .ne. this%nppc_min) call die('Attempt to increase pmc numbers above min failed')
            cycle
          end if

          ! Cluster if too many particles
          nu = myn    ! number of unclustered particles
          wtc = 0.0_WP ! sum of weights of particles to be clustered

          ! Set maximum number of particle per cell to nppc_max
          this%nppcp = this%nppc_max

          ! Sort particles by increasing weight
          do ip = 1, mynp
            this%iwt(ip) = ip
          end do
          call quick_sort(this%wt(1:mynp), this%iwt(1:mynp))

          jj = 0
          ! Loop through particles, and set multi to zero if clustering
          do while (myn .gt. this%nppc_max .and. jj .lt. mynp)
            ! Increase number of particles in clusters
            jj = jj + 1
            ! Increase cluster total weight
            wtc = wtc + this%wt(jj)
            ! Get actual particle index
            ip = this%pincell(i, j, k, this%iwt(jj))
            nu = nu - this%p(ip)%multi
            ! Adjust mass and volume to account for multi>1
            this%p(ip)%m = this%p(ip)%m*this%p(ip)%multi
            this%p(ip)%vol = this%p(ip)%vol*this%p(ip)%multi
            ! Particle will be removed (unless chosen as cluster head)
            this%p(ip)%multi = 0

            if (this%wt(jj) .gt. 0) then
              ! Estimate max cluster weight
              wte = this%wt(jj) + wtc/real(jj, WP)
              ! Number of clusters
              nc = int(wtc/wte) + 1
              myn = nu + nc
            end if
          end do

          !  target number of clusters
          nc = this%nppc_max - nu
          if (nc .le. 0) call die('Unsuccessful at identifying clusters to raise npm in cell')
          ! Particles to be clustered are iwt(jj), jj=1:jlast
          jlast = jj

          ! Maximum importance weight of clusters
          w_max = wtc/real(nc, WP) + 0.8_WP*wtc/(real(jlast, WP))

          jj = 1
          ic = 0   ! cluster index
          mytotmass = 0.0_WP
          do while (jj .le. jlast)  !  loop over clusters (may be more or less than nc)
            ic = ic + 1
            wtc = 0.0_WP     !  current weight of cluster
            massc = 0.0_WP     !  mass of particles in cluster
            volc = 0.0_WP     !  volume of particles in cluster
            jf = jj          !  first particle in cluster
            jl = jj          !  last particle in cluster

            ! Continue adding to the cluster until adding a further particle would make the
            ! cluster weight exceed w_max

            inner: do while (jj .le. jlast)   ! identify cluster [jf,jl]
              ! Check if cluster is full
              if (wtc + this%wt(jj) .gt. w_max .and. jj .ne. jlast) exit inner
              ! Else, add particle to it
              ip = this%pincell(i, j, k, this%iwt(jj))
              wtc = wtc + this%wt(jj)
              massc = massc + this%p(ip)%m
              !volc  = volc  + this%p(ip)%vol
              jl = jj
              jj = jj + 1
            end do inner

            ! Select particle in the cluster
            call random_number(rand)
            pm = rand*wtc                ! cumulative mass within particle to be selected
            cm = 0.0_WP                    ! cumulative mass of particles in cluster
            do kk = jf, jl
              ip = this%pincell(i, j, k, this%iwt(kk))
              cm = cm + this%wt(kk); 
              if (cm .ge. pm) then            ! ip is the selected particle

                this%p(ip)%multi = 1
                volc = this%p(ip)%m/this%p(ip)%vol ! density of the selected particle
                this%p(ip)%m = massc
                this%p(ip)%vol = massc/volc
                mytotmass = mytotmass + massc

                exit
              end if
            end do
          end do

          ! Check total mass and volume
          mass1 = 0.0_WP
          vol1 = 0.0_WP
          do myip = 1, mynp
            ip = this%pincell(i, j, k, myip)
            mass1 = mass1 + this%p(ip)%m*this%p(ip)%multi
            vol1 = vol1 + this%p(ip)%vol*this%p(ip)%multi
          end do

          if (abs(mass1 - mass0)/mass1 .gt. 1.0e-10_WP) then
            print *, 'mass check:', mass1, mass0, abs(mass1 - mass0)/mass1, i, j, k, mynp
            do myip = 1, mynp
              print *, myip, this%p(ip)%m, this%p(ip)%vol, this%p(ip)%multi
            end do
            call die('Inconsistent mass after particle number control')
          end if

        end do
      end do
    end do
    ! --------------------------------------------------- !
    ! Effectively adjust particle number
    ! --------------------------------------------------- !

    ! Update number of particles
    npmc_new_ = 0 ! with multiplicity
    ! Loop through all cells
    do k = this%cfg%kmin_, this%cfg%kmax_
      do j = this%cfg%jmin_, this%cfg%jmax_
        do i = this%cfg%imin_, this%cfg%imax_
          ! Loop through all particles

          do n = 1, this%npincell(i, j, k)
            npmc_new_ = npmc_new_ + max(1, this%p(this%pincell(i, j, k, n))%multi)
          end do
        end do
      end do
    end do
    doit = .false.
    if (npmc_new_ .gt. this%np_) doit = .true.

    ! Resize particle array accounting for multiplicity
    call this%resize(npmc_new_)
    ! Clone particles with multiplicity and remove those with multi 0
    call clone(this, this%np_, doit)

    call cell_localize(this)


  contains

    ! ================================================ !
    ! Cloning particle with multiplicity larger than 1 !
    ! ================================================ !
    subroutine clone(this, ip, doit)
      implicit none

      class(pdf), intent(inout) :: this
      integer :: i, j, multi, myi, myj, myk
      integer :: ip ! End of current list of particles
      real(WP) :: rand
      logical :: doit

      !print*,this%cfg%rank,'CLONING PARTICLES!',ip,size(pmc,dim=1)
      if (doit) then
        do i = 1, size(this%p, dim=1)
          ! Save multiplicity
          multi = this%p(i)%multi
          ! If multi greater than one, clone and reset multi
          if (this%p(i)%multi .gt. 1) then
            ! Reset multi
            this%p(i)%multi = 1
            ! Clone particle
            do j = 1, multi - 1
              ! Increment particle counter
              ip = ip + 1
              ! Duplicate particle
              this%p(ip) = this%p(i)
            end do
          end if
        end do
      end if

      ! Update sizes
      call this%recycle()

      ! Check
      !print*,'ip',ip,np_
      if (ip .ne. this%np_) call die('issue with cloning of particles: numbers do not match')

    end subroutine clone
    ! --------------------------------- !
    ! Localization of particles in cell !
    ! --------------------------------- !
    subroutine cell_localize(this)
      implicit none
      class(pdf), intent(inout) :: this

      integer :: ip, i, j, k
      integer :: buf

      ! Initialize arrays for particle localization
      this%npincell = 0
      this%pincell = -1

      ! Map normal particle to cell
      do ip = 1, this%np_
        ! Localize particles in cells
        i = this%p(ip)%ind(1)
        j = this%p(ip)%ind(2)
        k = this%p(ip)%ind(3)

        ! Number of particles in cell ijk
        this%npincell(i, j, k) = this%npincell(i, j, k) + 1

        ! Index of nth particle in cell ijk
        this%pincell(i, j, k, this%npincell(i, j, k)) = ip

      end do

      return
    end subroutine cell_localize

    ! ------------------------------------------------ !
    ! Recursive QuickSort algorithm : this routine     !
    ! recursively sorts a real(WP) array by increasing !
    ! order and sorts a integer array at the same time !
    ! ------------------------------------------------ !
    recursive subroutine quick_sort(A, B)
      use precision
      implicit none

      real(WP), dimension(:) :: A
      integer, dimension(:)  :: B
      integer :: imark

      if (size(A) .gt. 1) then
        call qs_partition(A, B, imark)
        call quick_sort(A(:imark - 1), B(:imark - 1))
        call quick_sort(A(imark:), B(imark:))
      end if

      return
    end subroutine quick_sort

    ! Real and integer arrays
    subroutine qs_partition(A, B, marker)
      use precision
      implicit none

      real(WP), dimension(:) :: A
      integer, dimension(:) :: B
      integer, intent(out) :: marker
      integer :: i, j
      real(WP) :: dtmp
      integer  :: itmp
      real(WP) :: x

      x = A(1)
      i = 0
      j = size(A) + 1

      do
        j = j - 1
        do
          if (A(j) .le. x) exit
          j = j - 1
        end do
        i = i + 1
        do
          if (A(i) .ge. x) exit
          i = i + 1
        end do
        if (i .lt. j) then
          ! Exchange A(i) and A(j)
          dtmp = A(i)
          A(i) = A(j)
          A(j) = dtmp
          ! Also exchange B(i) and B(j)
          itmp = B(i)
          B(i) = B(j)
          B(j) = itmp
        else if (i .eq. j) then
          marker = i + 1
          return
        else
          marker = i
          return
        end if
      end do

      return
    end subroutine qs_partition
  end subroutine control

  subroutine react(this, dt)
    use mpi_f08
    use messager, only: die
    implicit none
    class(pdf), intent(inout) :: this
    real(WP), intent(in) :: dt  !< Timestep size over which to advance

    integer :: i, myi
    real(WP), dimension(npS + 1) :: sol

    ! Local scheduler variables
    integer :: ndata
    ! Tags
    integer :: itag_ndata, itag_data, itag_ihead, itag_idle, itag_done
    integer :: idata, ibuf
    logical :: ldone
    ! Misc
    integer :: ipmc_, istatus, iworker, itag, ip
    ! MPI
    integer, dimension(MPI_STATUS_SIZE) :: status
    integer :: iexit_head
    ! List of particles to send out for direct integration
    integer :: nDI
    integer, dimension(:), pointer :: iDI
    ! Temp variables to update particle properties
    real(WP) :: rbuf, myw, mysv, myh

    ! If only one processor, or if beginning of simulation, just do the work for all particles
    if (.not. use_scheduler .or. this%cfg%nproc .eq. 1) then

      do i = 1, this%np_
        ! Package initial solution vector
        sol(1:npS) = this%p(i)%Yi
        sol(npS1) = this%p(i)%T
        ! Advance the chemical equations for each particle
        call pdf_reaction_compute_sol(sol, this%pthermo, dt)

        ! Transfer back to particle structure
        this%p(i)%Yi = sol(1:npS)
        this%p(i)%T = sol(npS1)
        ! Update other particle variables
        ! Enthalpy - Should be constant since reaction step is at constant pressure
        myh = this%p(i)%h
        call pdf_reaction_thermodata(this%p(i)%Yi, this%p(i)%T, rbuf, this%p(i)%h)
if (abs(this%p(i)%h-myh)/myh.gt.0.01_WP) print*,'===== P1 - ENTHALPY LIKELY WRONG!!!',i,this%cfg%rank,' - H = ',this%p(i)%h,myh,(this%p(i)%h-myh)/myh
        ! Adjust volume (assume constant mass)
        call this%get_Wmix(this%p(i), myw)
        call this%get_sv(this%p(i), myw, mysv)
        this%p(i)%vol = mysv*this%p(i)%m
      end do

      return
    end if

    ! ! If no ISAT, all particles treated with DI
    ! do i = 1, np_
    !   iDI(i) = i
    ! end do

    ! ! Total number of particles in DI
    ! nDI = np_

    ! ! Redefine bundle size to correspond to at least to twice the number of processors
    ! nbundles = min(nbundles_max, int(real(nDI, WP)/real(2*nproc, WP)) + 1)

    ! ! Make sure the same number applies to everybody (take maximum, so nbundles_max better be adequate)
    ! call parallel_max(nbundles, ibuf); nbundles = ibuf

    ! ! Transfer it to pdf module for monitor output
    ! nppbundle = nbundles

    ! ! ------------------------------------------- !
    ! ! Dynamic scheduler here for load balancing
    ! ! ------------------------------------------- !

    ! ! Tag meanings:
    ! ! TAG=1: sending number of data
    ! itag_ndata = 1
    ! ! TAG=2: sending data
    ! itag_data = 2
    ! ! TAG=3: identity of head
    ! itag_ihead = 3
    ! ! TAG=4: idle
    ! itag_idle = 4
    ! ! TAG=5: quit signal
    ! itag_done = 5

    ! ! Initialize head/worker identities
    ! ! Initial head id
    ! ihead = iroot
    ! ! flag that a new head has been promoted
    ! inewhead = -1
    ! ! Processors that have been head already
    ! ihead_list = 0
    ! ihead_list(ihead) = 1
    ! ! Flag indicating that data need to be sent back to head
    ! idata = 0
    ! ! Extra integer buffer
    ! ibuf = 1
    ! ! Not done to start with
    ! ldone = .false.
    ! ! first particle considered will have index 1
    ! ipmc_ = 1

    ! ! Loop until all work is done
    ! scheduler_loop: do while (.not. ldone)

    !   ! head loop
    !   if (this%cfg%rank .eq. ihead) then

    !     ! Initializing processor roles and buffers
    !     nproc_waiting = 0
    !     iproc_waiting = 0
    !     bufferR = 0.0_WP
    !     bufferS = 0.0_WP
    !     ! Initialize ndata
    !     ndata = 0
    !     ! Initialize loop condition
    !     iexit_head = 0

    !     ! Let's send out all my data
    !     head_loop: do while (iexit_head .eq. 0) ! No explicit exit conditions here, automatically handled below

    !       ! ------------------------------------------- !
    !       ! Gather compositions until there is a bundle of ndata particles ready to send
    !       ! Skip locally processed compositions
    !       do while (ndata .lt. nbundles .and. ipmc_ .le. nDI)
    !         ! Need to gather some more
    !         ! Initialize solution vector
    !         sol(1:npS) = pmc(iDI(ipmc_))%Yi
    !         sol(1:npS) = sol(1:npS)/sum(sol(1:npS))
    !         sol(npS + 1) = pmc(iDI(ipmc_))%T
    !         ! Buffer composition to send out to other processors
    !         ndata = ndata + 1
    !         bufferS((ndata - 1)*npS1 + 1:ndata*npS1) = sol
    !         iwhere_buf(ndata) = iDI(ipmc_) ! link between bundle and pmc array
    !         nwhere_buf = ndata ! Keep track of actual number of compos in bundle
    !         ! Increment index of next composition to consider
    !         ipmc_ = ipmc_ + 1
    !       end do

    !       ! ------------------------------------------- !
    !       ! Listen to the workers until one raises its hand
    !       call MPI_probe(MPI_ANY_SOURCE, MPI_ANY_TAG, comm, status, ierr)
    !       ! Got something: which worker is talking?
    !       iworker = status(MPI_SOURCE) + 1
    !       ! What message is it sending?
    !       itag = status(MPI_TAG)

    !       ! ------------------------------------------- !
    !       ! Does this worker have results to send?
    !       if (itag .eq. itag_data) then
    !         ! itag = 1: worker has data to send back, receive them!
    !    call MPI_recv(bufferR(1:nwhere(iworker)*npS1), nwhere(iworker)*npS1, MPI_REAL_WP, iworker - 1, itag_data, comm, status, ierr)
    !         ! Store each bufferR compo at correct location in pmc array
    !         do i = 1, nwhere(iworker)
    !           myi = iwhere(iworker, i)
    !           pmc(myi)%Yi = bufferR((i - 1)*npS1 + 1:i*npS1 - 1)
    !           pmc(myi)%T = bufferR(i*npS1)
    !           ! Update other particle variables
    !           ! Enthalpy - Should be constant since reaction step is at constant pressure
    !           myh = pmc(myi)%h
    !           call pdf_reaction_thermodata(pmc(myi)%Yi, pmc(myi)%T, rbuf, pmc(myi)%h)
    !              if (abs(pmc(myi)%h-myh)/myh.gt.0.01_WP) print*,'===== P3 - ENTHALPY LIKELY WRONG!!!',i,this%cfg%rank,' - H = ',pmc(myi)%h,myh,(pmc(myi)%h-myh)/myh
    !           ! Adjust volume (assume constant mass)
    !           call pdf_reaction_Wmix(pmc(myi)%Yi, 'Y', myw)
    !           call pdf_reaction_sv(pmc(myi)%T, myw, mysv)
    !           pmc(myi)%vol = mysv*pmc(myi)%m
    !         end do
    !       else
    !         ! Just receive its empty message
    !         call MPI_recv(ibuf, 1, MPI_INTEGER, iworker - 1, itag_idle, comm, status, ierr)
    !       end if

    !       ! ------------------------------------------- !
    !       ! Do I have more work to do?
    !       if (ndata .gt. 0) then ! ndata not 0: last bundle has not been sent yet, more work to do!
    !         ! Keep a note of what was sent to iworker
    !         iwhere(iworker, :) = iwhere_buf
    !         nwhere(iworker) = nwhere_buf
    !         ! If yes, send the number of data that will be sent
    !         call MPI_send(nwhere(iworker), 1, MPI_INTEGER, iworker - 1, itag_ndata, comm, ierr)
    !         ! Send the next chunk to this worker
    !         call MPI_send(bufferS(1:nwhere(iworker)*npS1), ndata*npS1, MPI_REAL_WP, iworker - 1, itag_data, comm, ierr)
    !         ! Reset buffer to start accumulating more composition for next call
    !         ndata = 0
    !         iwhere_buf = 0
    !         nwhere_buf = 0
    !         ! Go back to beginning of loop
    !         cycle head_loop
    !       end if

    !       ! ------------------------------------------- !
    !       ! If reaching here, no more work to do,
    !       ! ready to finish work as head and switch to worker status

    !       ! ------------------------------------------- !
    !       ! Has a new head being promoted?
    !       if (inewhead .eq. -1) then !no, no new head yet

    !         ! Reset ihead_aware to 0
    !         ihead_aware = 0
    !         ! But I am aware already, that counts for someting
    !         ihead_aware(this%cfg%rank) = 1

    !         ! Has this worker been a head yet?
    !         if (ihead_list(iworker) .eq. 0) then ! no, not yet
    !           ! Promote the worker to head status
    !           inewhead = iworker
    !           ! Tell the current worker that it is the new head and update aware list
    !           call MPI_send(inewhead, 1, MPI_INTEGER, iworker - 1, itag_ihead, comm, ierr)
    !           ihead_aware(iworker) = 1
    !           ! Send id of new head to all waiting workers and update aware list
    !           do ip = 1, nproc_waiting
    !             call MPI_send(inewhead, 1, MPI_INTEGER, iproc_waiting(ip) - 1, itag_ihead, comm, ierr)
    !             ihead_aware(iproc_waiting(ip)) = 1
    !           end do

    !         else ! Yes, this proc has been head already
    !           ! Add identity of this worker to the waiting list
    !           nproc_waiting = nproc_waiting + 1
    !           iproc_waiting(nproc_waiting) = iworker
    !           ! Check if we are fully done (all procs have been heads already)
    !           if (nproc_waiting .eq. nproc - 1) then
    !             exit head_loop
    !           end if
    !         end if

    !         ! ------------------------------------------- !
    !       else ! Yes, a new head has been promoted
    !         ! Send the id of the new head to the current worker and update aware list
    !         call MPI_send(inewhead, 1, MPI_INTEGER, iworker - 1, itag_ihead, comm, ierr)
    !         ihead_aware(iworker) = 1
    !       end if

    !       ! ------------------------------------------- !
    !       ! Continue waiting for signals till all workers have received new head id
    !       if (sum(ihead_aware) .eq. nproc) iexit_head = 1

    !     end do head_loop

    !     ! ------------------------------------------- !
    !     ! Check if I am the last head
    !     if (sum(ihead_list) .eq. nproc) then
    !       ! If so, update done
    !       ldone = .true.
    !       ! Send a quit signal to every body
    !       do ip = 1, nproc
    !         if (ip .eq. this%cfg%rank) cycle
    !         call MPI_send(ldone, 1, MPI_LOGICAL, ip - 1, itag_done, comm, ierr)
    !       end do
    !     else ! I am not the last head
    !       ! Switching the head id to somebody else
    !       ihead = inewhead
    !       ! Keep it rolling
    !       cycle scheduler_loop
    !     end if

    !     ! ------------------------------------------- !
    !     ! ------------------------------------------- !
    !     ! worker loop
    !   else

    !     worker_loop: do while (.true.) ! No explicit exit conditions here, automatically handled below
    !       ! ------------------------------------------- !
    !       ! Do I have results to send to the head?
    !       if (idata .eq. 1) then ! yes, I have data to send
    !         ! Send them to head
    !         call MPI_send(bufferR(1:ndata*npS1), ndata*npS1, MPI_REAL_WP, ihead - 1, itag_data, comm, ierr)
    !         ! No more data to send for now
    !         idata = 0
    !       else ! No, no data to send
    !         ! Just tell the head I am available
    !         call MPI_send(ibuf, 1, MPI_INTEGER, ihead - 1, itag_idle, comm, ierr)
    !       end if

    !       ! ------------------------------------------- !
    !       ! Listening to the head to see what is coming next
    !       call MPI_probe(ihead - 1, MPI_ANY_TAG, comm, status, ierr)
    !       ! What message is it sending?
    !       itag = status(MPI_TAG)

    !       ! ------------------------------------------- !
    !       ! What is the message?
    !       if (itag .eq. itag_done) then ! Quit message
    !         ! Receive it (we just tested the tag here, still need to actually receive the integer)
    !         call MPI_recv(ldone, 1, MPI_LOGICAL, ihead - 1, itag_done, comm, status, ierr)
    !         cycle scheduler_loop

    !         ! ------------------------------------------- !
    !       elseif (itag .eq. itag_ndata) then ! Work message
    !         ! Receiving number of data expected
    !         call MPI_recv(ndata, 1, MPI_integer, ihead - 1, itag_ndata, comm, status, ierr)
    !         ! Receiving chunk of data to process
    !         call MPI_recv(bufferR(1:ndata*npS1), ndata*npS1, MPI_REAL_WP, ihead - 1, itag_data, comm, status, ierr)

    !         ! Do the work
    !         do i = 1, ndata
    !           call pdf_reaction_compute_sol(bufferR((i - 1)*npS1 + 1:i*npS1), Pthermo, dt)
    !         end do
    !         ! Now I have data to send
    !         idata = 1
    !         ! Cycle the worker loop again
    !         cycle worker_loop

    !         ! ------------------------------------------- !
    !       elseif (itag .eq. itag_ihead) then ! head has changed
    !         ! Receive the id of the new head
    !         call MPI_recv(ibuf, 1, MPI_integer, ihead - 1, itag_ihead, comm, status, ierr)
    !         ! Update the id of head
    !         ihead = ibuf
    !         ihead_list(ihead) = 1
    !         ! Cycle the scheduler loop
    !         cycle scheduler_loop

    !       end if
    !     end do worker_loop
    !   end if
    ! end do scheduler_loop

    ! ! Deallocate working array
    ! deallocate (iDI)
    ! return
  contains
! ---------------------------------------------------------------------------------- !
! ======================================== !
! Compute solution after time step delta t !
! ======================================== !
    subroutine pdf_reaction_compute_sol(sol, mypressure, dt)
      use random
      use messager, only: die
      implicit none

      real(WP), dimension(npS1) :: sol, dsol, solcheck
      real(WP) :: dt
      real(WP) :: mypressure
      real(WP) :: tstop, tstart
      integer :: ii
      ! external :: pdf_reaction_compute_rhs
      ! external :: pdf_reaction_compute_jac
      ! external :: pdfrmap1
      real(WP), dimension(22) :: rstats
      integer, dimension(31) :: istats

      ! ISAT parameters
      real(WP) :: xx(npS2), f(npS1), dfdx(npS1, npS2), hvar(1), stats(100)
      integer :: iusr(1)

      ! Get correct pressure
      pressure = mypressure

      ! Save solution
      solcheck = sol

      ! Set values for DVODE
      itask = 1; istate = 1; tstart = 0.0_WP; tstop = dt

      ! -------------------------------- !
      ! ------ Direct integration ------ !
      ! -------------------------------- !

      ! Integrate using DVODE
      if (use_jacanal) then

        opts = set_opts(dense_j=.true., mxstep=500000, abserr=atol, relerr=rtol, tcrit=tstop, user_supplied_jacobian=.true.)
        call dvode_f90(pdf_reaction_compute_rhs, npS1, sol, tstart, tstop, itask, istate, opts, j_fcn=pdf_reaction_compute_jac)

      else

        opts = set_opts(method_flag=22, mxstep=500000, abserr=atol, relerr=rtol, tcrit=tstop)
        call dvode_f90(pdf_reaction_compute_rhs, npS1, sol, tstart, tstop, itask, istate, opts)

      end if
      call get_stats(rstats, istats)
      call release_opts_arrays(opts)

      ! Error handling
      if (istate .ne. 2) then
        print *, 'NCF: ', istats(21) ! No. of convergence failures of the nonlinear solver so far.
        print *, 'NEF: ', istats(22) ! No. of error test failures of the integrator so far.
        print *, 'tstop: ', tstop, ' tstart:', tstart, ' itask: ', itask, ' istate: ', istate
        print *, 'solold---------'
        do ii = 1, npS
          print *, 'sol(', ii, ') = ', solcheck(ii), '_WP !'
        end do
        print *, 'sol(NT) = ', solcheck(npS + 1), '_WP !', 'T'
        print *, '-------------------------------'
        print *, 'sol', sol
        print *, '-------------------------------'
        print *, 'T', 'dsol(ii)', dsol(npS + 1)
        call die('pdf_reaction_source: Direct integration - DVODE failed to converge.')
      end if

      ! Clip and renormalize, just in case
      call this%clip(sol(1:npS))

      return
    end subroutine pdf_reaction_compute_sol
! ================================================================== !
! Computes the chemical source term of the system (called by solver) !
! ================================================================== !
    subroutine pdf_reaction_compute_rhs(n_, t_, sol, rhs)

      implicit none

      integer, intent(in) :: n_
      real(WP), intent(in) :: t_
      real(WP), dimension(n_), intent(in)  :: sol
      real(WP), dimension(n_), intent(out) :: rhs
      real(WP) :: Cp_mix, Wmix, RHOmix
      real(WP), dimension(1:npS + npTB) :: myCM
      real(WP), dimension(npR) :: myW, myK

      ! Reset rhs
      rhs = 0.0_WP

      ! Get W of mixture
      call pdf_reaction_Wmix(sol(1:npS), 'Y', Wmix)

      ! Get Cp of mixture and update hsp
      call pdf_reaction_Cpmix(sol(1:npS), sol(npS1), Cp_mix)

      ! Get RHO of mixture
      RHOmix = pressure*Wmix/(R_cst*sol(npS1))

      ! Calculate concentrations of unsteady species
      myCM(1:npS) = RHOmix*sol(1:npS)/Wsp(1:npS)

      ! --- Get thirdbodies --- !
      call pdf_mech_thirdbodies(myCM(1:npS), myCM(npS + 1:npS + npTB))

      ! --- Rate coefficients --- !
      call pdf_reaction_ratecoefficients(myK, sol(npS1), myCM(npS + 1:npS + npTB))

      ! --- Reaction rates --- !
      call pdf_reaction_reactionrates(myW, myK, myCM)

      ! --- Production rates --- !
      call pdf_reaction_prodrates(rhs, myW)

      ! Transform concentration into mass fraction
      rhs(1:npS) = rhs(1:npS)*Wsp(1:npS)/RHOmix

      ! Temperature rhs from change in concentration
      rhs(npS1) = -sum(hsp(1:npS)*rhs(1:npS))/Cp_mix

      return
    end subroutine pdf_reaction_compute_rhs
    ! ------------------------------------------------------------------------------------------------ !
! ================================================ !
! Compute the average Cp in the mixture (J/(kg.K)) !
! ================================================ !
    subroutine pdf_reaction_Cpmix(scalar, Tmix, Cp_mix)
      implicit none
      real(WP), dimension(npS), intent(in) :: scalar
      real(WP), intent(in) :: Tmix
      real(WP), intent(out) :: Cp_mix

      call pdf_mech_thermodata(Tmix)
      Cp_mix = sum(scalar*Cpsp)

      return
    end subroutine pdf_reaction_Cpmix
    ! ------------------------------------------------------------------------------------ !
! ============================================= !
! Compute the average Molecular weight (kg/mol) !
! ============================================= !
    subroutine pdf_reaction_Wmix(scalar, type, Wmix)
      implicit none

      character :: type
      real(WP) :: Wmix
      real(WP), dimension(npS) :: scalar, scalar_t

      ! Ensure scalar is between 0 and 1
      scalar_t = min(max(scalar, 0.0_WP), 1.0_WP)

      ! Check if scalar is mole or mass based
      if (type .eq. 'X') then
        Wmix = sum(scalar_t*Wsp)
      elseif (type .eq. 'Y') then
        Wmix = sum(scalar_t)/sum(scalar_t/Wsp)
      else
        call die('Unknown datatype to compute mixture molar mass')
      end if

      return
    end subroutine pdf_reaction_Wmix

    subroutine pdf_reaction_thermodata(sol, temp, cpm, hm)
      implicit none

      real(WP) :: temp, cpm, hm
      real(WP), dimension(npS) :: sol

      ! Function in mechanism.f file
      call pdf_mech_thermodata(temp)
      cpm = sum(sol*Cpsp)
      hm = sum(sol*hsp)

      return
    end subroutine pdf_reaction_thermodata

! ------------------------------------------------------------------------------------------------ !
! ======================================= !
! Obtain rate constants for each reaction !
! kloc = A T^n exp(-Ea/RT)                !
! ======================================= !
    subroutine pdf_reaction_ratecoefficients(myK, myT, myM)

      implicit none

      integer  :: i, j
      real(WP), intent(in) :: myT
      real(WP), dimension(npS + 1:npS + npTB), intent(in) :: myM
      real(WP), dimension(npR), intent(out) :: myK
      real(WP) :: oneoverRT, lnT, oneoverT
      real(WP) :: FC, conc, klind
      real(WP), dimension(npRL + 1:npR) :: k0, kInf
      real(WP), parameter :: eps = 2.0_WP*epsilon(1.0_WP)

      oneoverT = 1.0_WP/myT
      oneoverRT = oneoverT/R_cst
      lnT = log(myT)

      ! Get k for simple reactions and third body reactions without pressure dependence
      !myK(1:npRL) = Acoeff(1:npRL)*exp(ncoeff(1:npRL)*lnT - Eact(1:npRL)*oneoverRT)
      myK(1:npRL) = Acoeff1(1:npRL)*exp(Acoeff2(1:npRL)*log(10.0_WP) + ncoeff(1:npRL)*lnT - Eact(1:npRL)*oneoverRT)

      ! Pressure dependent reactions with fall off parameters
      ! Get k0, kInf values if there are some
      do i = 1, nPDR
        !k0(npRL+i)   = Acoeff(npRL+i) * exp(ncoeff(npRL+i)*lnT - Eact(npRL+i)*oneoverRT)
        !kInf(npRL+i) = Acoeff(npR+i) * exp(ncoeff(npR+i)*lnT - Eact(npR+i)*oneoverRT)
        k0(npRL + i) = Acoeff1(npRL + i)*exp(Acoeff2(npRL + i)*log(10.0_WP) + ncoeff(npRL + i)*lnT - Eact(npRL + i)*oneoverRT)
        kInf(npRL + i) = Acoeff1(npR + i)*exp(Acoeff2(npR + i)*log(10.0_WP) + ncoeff(npR + i)*lnT - Eact(npR + i)*oneoverRT)
      end do

      ! Find lindemann rate constant
      do i = npRL + 1, npR
        ! Get Fcent values
        FC = 0.0_WP
        if (abs(fcta(i)) .gt. eps) FC = FC + fca(i)*exp(-myT/fcta(i))
        if (abs(fctb(i)) .gt. eps) FC = FC + fcb(i)*exp(-myT/fctb(i))
        FC = FC + fcc(i)*exp(-fctc(i)*oneoverT)

        ! Get appropriate M
        conc = -1.0_WP ! getlindratecoeff will use conc = p/RT in the absence of a positive value
        j = nreactants(i)
        if (reactants(j, i) .gt. npS) conc = myM(reactants(j, i))
        call pdf_mech_getlindratecoeff(myT, pressure, k0(i), kInf(i), FC, conc, klind)
        myK(i) = klind
      end do

      return
    end subroutine pdf_reaction_ratecoefficients

! ------------------------------------------------------------------------------------------------ !
! ================================================================= !
! Obtain rates for each reaction                                    !
! wloc_j = kloc_j*[C_i]^{nu_reactant_ij}*[C_i]^{nu_reactant_ij}*... !
! ================================================================= !
    subroutine pdf_reaction_reactionrates(myW, myK, myCM)

      implicit none

      integer  :: i, j
      real(WP), dimension(npR), intent(in) :: myK
      real(WP), dimension(npS + npTB), intent(in) :: myCM
      real(WP), dimension(npR), intent(out) :: myW

      ! Initialize myW - Same for all reactions
      myW = myK

      ! Multiply by concentrations raised to stoich coeff power
      do i = 1, npR
        ! Loop through all species reactants
        do j = 1, nreactants_inW(i)
          ! Take no powers when exponent = 1
          if (abs(nuR(j, i) - 1.0_WP) .lt. epsilon(1.0_WP)) then
            myW(i) = myW(i)*myCM(reactants(j, i))
          else
            myW(i) = myW(i)*myCM(reactants(j, i))**nuR(j, i)
          end if
        end do
      end do

      return
    end subroutine pdf_reaction_reactionrates

! ------------------------------------------------------------------------------------------------ !
! ================================================ !
! Obtain production rates for each species
! cdot = sum_{j=1}^{j=npR} [sumNu(i,j)*wdot(j)]
! ================================================ !
    subroutine pdf_reaction_prodrates(myCdot, myW)

      implicit none
      real(WP), dimension(npR), intent(in) :: myW
      real(WP), dimension(npS), intent(out) :: myCdot
      integer :: i, k

      ! Initialize
      myCdot = 0.0_WP

      ! Loop through reactions
      do k = 1, npR

        ! Loop through reactants and add to Cdot
        do i = 1, nreactants_noM(k)
          myCdot(reactants(i, k)) = myCdot(reactants(i, k)) - nuR(i, k)*myW(k)
        end do
        ! Loop through products and add to Cdot
        do i = 1, nproducts_noM(k)
          myCdot(products(i, k)) = myCdot(products(i, k)) + nuP(i, k)*myW(k)
        end do

      end do

      return
    end subroutine pdf_reaction_prodrates

    ! ---------------------------------------------------------------------------------- !
! ========================================================== !
! Computes the approximate analytical jacobian of the system !
! ========================================================== !
    subroutine pdf_reaction_compute_jac(n_, t_, sol, ml, mu, jac, nrpd)
      implicit none

      ! Input
      integer :: n_, ml, mu, nrpd
      real(WP) :: t_
      real(WP), dimension(n_) :: sol
      real(WP), dimension(nrpd, n_) :: jac

      ! Local variables
      real(WP) :: Cp_mix, Wmix, RHOmix
      real(WP), dimension(npS) :: myCdot
      real(WP), dimension(1:npS + npTB) :: myCM
      real(WP), dimension(npR) :: myW, myK

      ! Get W of mixture
      call pdf_reaction_Wmix(sol(1:npS), 'Y', Wmix)

      ! Get Cp of mixture and update hsp
      call pdf_reaction_Cpmix(sol(1:npS), sol(npS + 1), Cp_mix)

      ! Get RHO of mixture
      RHOmix = pressure*Wmix/(R_cst*sol(npS + 1))

      ! Calculate concentrations of unsteady species
      myCM(1:npS) = RHOmix*sol(1:npS)/Wsp(1:npS)

      ! Get thirdbodies
      call pdf_mech_thirdbodies(myCM(1:npS), myCM(npS + 1:npS + npTB))

      ! --- Rate coefficients --- !
      call pdf_reaction_ratecoefficients(myK, sol(npS + 1), myCM(npS + 1:npS + npTB))

      ! --- Reaction rates --- !
      call pdf_reaction_reactionrates(myW, myK, myCM)

      ! --- Production rates --- !
      call pdf_reaction_prodrates(myCdot, myW)

      ! Get analytical Jacobian from mechanism file
      call pdf_reaction_getjacobian(sol(1:npS), sol(npS + 1), myCM, myCdot, myK, RHOmix, Cp_mix, Wmix, jac)

      return
    end subroutine pdf_reaction_compute_jac

! ------------------------------------------------------------------------------------------------ !
! ====================================== !
! New calculation of analytical jacobian !
! ====================================== !
    subroutine pdf_reaction_getjacobian(myY, myT, myCM, myCdot, myK, myrho, myCpmix, myWmix, jac)

      implicit none

      ! Mass fraction/concentration vector
      real(WP), dimension(npS) :: myY
      real(WP), dimension(npS + npTB) :: myCM
      ! Temperature
      real(WP) :: myT
      ! Cdot
      real(WP), dimension(npS) :: myCdot, myYdot
      ! Reaction coefficients
      real(WP), dimension(npR) :: myK
      ! Mixture density, heat capacity, molar mass
      real(WP) :: myrho, myCpmix, myWmix
      ! Jacobian
      real(WP), dimension(npS1, npS1), intent(out) :: jac

      ! Working arrays
      real(WP) :: myTdot, FC, dlnFCdT, dGdT, dlnFdT, k0overkInf
      real(WP), dimension(npS1, npS1) :: jacC
      real(WP), dimension(npS1, npS1) :: dCdY
      integer :: i, myi, j, myj, jj, myjj, k, ireac, iM
      real(WP), dimension(nreactants_max) :: dW_dC
      real(WP), dimension(npS, npS) :: dCdot_dC
      real(WP), dimension(npS) :: dCdot_dT
      real(WP), dimension(npS) :: dCpsdT
      real(WP), dimension(npR) :: dk_dT
      real(WP) :: oneoverT, oneoverRT, lnT, Mval, Pr, dW_dT
      real(WP), dimension(npRL + 1:npR) :: k0, kinf, dk0_dT, dkinf_dT

      ! ------------------------------------------------------------------------------ !
      ! Variables are {Y,T}, Y being mass fraction vector
      ! Jacobian is therefore Jac(i,j) = [dYdot_i/dY_j dYdot_j/dT;dTdot/dY_j dTdot/dT]
      ! ------------------------------------------------------------------------------ !

      ! Initialize
      jac = 0.0_WP
      jacC = 0.0_WP
      dCdot_dC = 0.0_WP
      dCdot_dT = 0.0_WP

      ! Shorthand notations
      oneoverT = 1.0_WP/myT
      oneoverRT = oneoverT/R_cst
      lnT = log(myT)

      ! ---------------------------------------------------------- !
      ! Temperature source term
      myYdot = Wsp*myCdot/myrho
      myTdot = -sum(hsp*myYdot)/myCpmix

      ! ---------------------------------------------------------- !
      ! Jacobian is more easily written in terms of concentrations
      ! A change of variables from (Y,T) to (C,T) is done. The jacobian corresponding to
      ! the change of variables is dC/dY (and the identity for T), so that
      ! dYdot/dY = dYdot/dC * dC/dY
      ! dCdY(i,j) = dC_i/dY_j
      dCdY = 0.0_WP
      do i = 1, npS
        do j = 1, npS
          dCdY(i, j) = -myCM(i)*myWmix/Wsp(j)
        end do
        dCdY(i, i) = dCdY(i, i) + myrho/Wsp(i)
      end do
      dCdY(npS1, npS1) = 1.0_WP

      ! ---------------------------------------------------------- !
      ! dk_dT: easier to evaluate all at once
      dk_dT = 0.0_WP
      ! Simple reactions
      dk_dT(1:npRL) = oneoverT*(ncoeff(1:npRL) + Eact(1:npRL)*oneoverRT)*myK(1:npRL)
      ! Pressure dependent reactions if any
      do i = 1, nPDR
        ! k0 for pressure dependent reactions
        !k0(npRL+i) = Acoeff(npRL+i)*exp(ncoeff(npRL+i)*lnT - Eact(npRL+i)*oneoverRT)
        k0(npRL + i) = Acoeff1(npRL + i)*exp(Acoeff2(npRL + i)*log(10.0_WP) + ncoeff(npRL + i)*lnT - Eact(npRL + i)*oneoverRT)
        dk0_dT(npRL + i) = oneoverT*(ncoeff(npRL + i) + Eact(npRL + i)*oneoverRT)*k0(npRL + i)
        ! kinf for pressure dependent reactions
        !kInf(npRL+i) = Acoeff(npR+i) * exp(ncoeff(npR+i)*lnT - Eact(npR+i)*oneoverRT)
        kInf(npRL + i) = Acoeff1(npR + i)*exp(Acoeff2(npR + i)*log(10.0_WP) + ncoeff(npR + i)*lnT - Eact(npR + i)*oneoverRT)
        dkinf_dT(npRL + i) = oneoverT*(ncoeff(npR + i) + Eact(npR + i)*oneoverRT)*kInf(npRL + i)
      end do

      ! Get dk_dT for pressure-dependent reactions, with k = kinf*(Pr/1+Pr)*F
      do k = npRL + 1, npR

        ! Reduced pressure
        Mval = myCM(reactants(nreactants(k), k))
        !k0overkInf = Acoeff(k)/Acoeff(k+nPDR)*exp((ncoeff(k)-ncoeff(k+nPDR))*lnT - (Eact(k)-Eact(k+nPDR))*oneoverRT)
     k0overkInf = Acoeff1(k)/Acoeff1(k+nPDR)*exp( (Acoeff2(k)-Acoeff2(k+nPDR))*log(10.0_WP) + (ncoeff(k)-ncoeff(k+nPDR))*lnT - (Eact(k)-Eact(k+nPDR))*oneoverRT)
        Pr = k0overkInf*Mval

        ! F function
        call pdf_reaction_compute_dlnFCdT(fca(k), fcta(k), fcb(k), fctb(k), fcc(k), fctc(k), myT, FC, dlnFCdT)
        call pdf_reaction_compute_dGdT(Pr, dk0_dT(k) - dkinf_dT(k), myT, FC, dlnFCdT, dGdT)
        dlnFdT = dlnFCdT + log(FC)*dGdT

        ! Assemble term
        dk_dT(k) = myK(k)*(dkinf_dT(k)/kInf(k) + 1.0_WP)/(1.0_WP + Pr)*(dk0_dT(k)/k0(k) - dkinf_dT(k)/kinf(k)) + dlnFdT

      end do

      ! ============================== !
      ! dYdot_i/dC_j and dYdot_i/dT
      ! ============================== !

      ! dCdot_dC(i,j) = sum_{reac k} nu(i,k) * ( k(k) dW(j)/dCk + dk(j)/dCk*dW_dT(j) )

      ! First, go through all reactions and accumulate part common to all reactions (k(k) dP(j)/dCk)
      jac1: do ireac = 1, npR

        ! Initialize dP_dC array to K, P to 1.0
        dW_dC(1:nreactants_noM(ireac)) = myK(ireac)
        dW_dT = 1.0_WP

        ! Multiply by thirdbody concentration for Lindemann reactions (not pressure dependent ones)
        iM = -1
        if (ireac .gt. npRN .and. ireac .le. npRL) then
          iM = reactants(nreactants(ireac), ireac) ! Index of thirdbody in reaction

          dW_dC(1:nreactants_noM(ireac)) = dW_dC(1:nreactants_noM(ireac))*myCM(iM)
          dW_dT = dW_dT*myCM(iM)

        end if

        ! Then, fill it, considering d/dCj
        do j = 1, nreactants_noM(ireac)

          ! Global index of j
          myj = reactants(j, ireac)

          ! Form the source term
          do jj = 1, nreactants_noM(ireac)

            ! Global index of jj
            myjj = reactants(jj, ireac)

            ! Multiply by correct term for jj
            if (abs(nuR(jj, ireac) - 1.0_WP) .gt. epsilon(1.0_WP)) then
              if (j .eq. jj) dW_dC(j) = dW_dC(j)*nuR(jj, ireac)*myCM(myjj)**(nuR(jj, ireac) - 1.0_WP)
              if (j .ne. jj) dW_dC(j) = dW_dC(j)*myCM(myjj)**nuR(jj, ireac)
            else
              if (j .ne. jj) dW_dC(j) = dW_dC(j)*myCM(myjj)
            end if

          end do

          ! dW_dT term: include all concentrations
          if (nuR(j, ireac) .ne. 1.0_WP) then
            dW_dT = dW_dT*myCM(myj)**nuR(j, ireac)
          else
            dW_dT = dW_dT*myCM(myj)
          end if
        end do

        ! Add terms to the dCdot_i/dC_j matrix, looping over differentiation species j
        do j = 1, nreactants_noM(ireac)
          myj = reactants(j, ireac)

          ! i on reactant side first
          do i = 1, nreactants_noM(ireac)
            myi = reactants(i, ireac)

            ! Add derivative wrt Cj to Cdot_i
            dCdot_dC(myi, myj) = dCdot_dC(myi, myj) - nuR(i, ireac)*dW_dC(j)

            ! Add thirdbody term if needed (at this stage, dW_dT does not contain rate coefficient info)
            if (iM .gt. 0) then
              dCdot_dC(myi, myj) = dCdot_dC(myi, myj) - nuR(i, ireac)*myK(ireac)*dW_dT*Meff(myj, iM)
            end if
          end do

          ! On product side next
          do i = 1, nproducts_noM(ireac)
            myi = products(i, ireac)

            ! Add derivative wrt Cj to Cdot_i
            dCdot_dC(myi, myj) = dCdot_dC(myi, myj) + nuP(i, ireac)*dW_dC(j)

            ! Add thirdbody term if needed
            if (iM .gt. 0) then
              dCdot_dC(myi, myj) = dCdot_dC(myi, myj) + nuP(i, ireac)*myK(ireac)*dW_dT*Meff(myj, iM)
            end if
          end do
        end do

        ! Final dW_dT
        dW_dT = dW_dT*dk_dT(ireac)

        ! Add terms to the dCdot_i/dT, i on reactant side first
        do i = 1, nreactants_noM(ireac)
          myi = reactants(i, ireac)
          dCdot_dT(myi) = dCdot_dT(myi) - nuR(i, ireac)*dW_dT
        end do
        ! On the product side
        do i = 1, nproducts_noM(ireac)
          myi = products(i, ireac)
          dCdot_dT(myi) = dCdot_dT(myi) + nuP(i, ireac)*dW_dT
        end do

      end do jac1

      ! Assemble full terms
      do i = 1, npS
        ! dYdot_i/dC_j
        do j = 1, npS
          jac(i, j) = Wsp(i)*dCdot_dC(i, j)/myrho - myYdot(i)*Wsp(j)/myrho
        end do
        ! dYdot_i/dT
        jac(i, npS1) = Wsp(i)*(dCdot_dT(i)/myrho - myCdot(i)/(myrho*myT))
      end do

      ! Accounting for the change of variables (T terms work better in Y space)
      jac(1:npS, 1:npS) = matmul(jac(1:npS, 1:npS), dCdY(1:npS, 1:npS))

      ! ==================== !
      ! dTdot/dY_j
      ! ==================== !

      jac(npS1, 1:npS) = Cpsp*myTdot
      do j = 1, npS
        jac(npS1, j) = jac(npS1, j) + sum(hsp*jac(1:npS, j))
      end do
      jac(npS1, :) = -jac(npS1, :)/myCpmix

      ! ==================== !
      ! dTdot/dT
      ! ==================== !

      call pdf_mech_compute_dCpsdT(myT, dCpsdT)
      jac(npS1, npS1) = -(sum(dCpsdT*myY)*myTdot + sum(Cpsp*myCdot) + sum(hsp*jac(1:npS, npS1)))/myCpmix

      return
    end subroutine pdf_reaction_getjacobian

! ------------------------------------------------------------------------------------------------ !
! ================================================ !
! Used by finitechem_getjacobian
! to compute one of the more involved terms
! dealing with pressure dependent rate coefficients
! ================================================ !
    subroutine pdf_reaction_compute_dlnFCdT(fca_i, fcta_i, fcb_i, fctb_i, fcc_i, fctc_i, Tloc, FC, dlnFCdT)

      implicit none

      real(WP) :: fca_i, fcta_i, fcb_i, fctb_i, fcc_i, fctc_i
      real(WP) :: Tloc, FC, dlnFCdT
      real(WP) :: tmp
      real(WP), parameter :: eps = 2.0_WP*epsilon(1.0_WP)

      ! fca_i = 1-alpha, fcta_i = T***, fcb_i = alpha, fctb_i = T*, fcc_i = beta, fctc_i = T**
      FC = 0.0_WP
      dlnFCdT = 0.0_WP
      if (abs(fcta_i) .gt. eps) then
        tmp = fca_i*exp(-Tloc/fcta_i)
        FC = FC + tmp
        dlnFCdT = dlnFCdT - tmp/fcta_i
      end if
      if (abs(fctb_i) .gt. eps) then
        tmp = fcb_i*exp(-Tloc/fctb_i)
        FC = FC + tmp
        dlnFCdT = dlnFCdT - tmp/fctb_i
      end if
      tmp = fcc_i*exp(-fctc_i/Tloc)
      FC = FC + tmp
      dlnFCdT = dlnFCdT + tmp*fctc_i/(Tloc*Tloc)
      dlnFCdT = dlnFCdT/FC

      return
    end subroutine pdf_reaction_compute_dlnFCdT

! ------------------------------------------------------------------------------------------------ !
! ================================================ !
! Used by finitechem_getjacobian
! to compute one of the more involved terms
! dealing with pressure dependent rate coefficients
! ================================================ !
    subroutine pdf_reaction_compute_dGdT(redP, kc_oInf, Tloc, FC, dlnFCdT, dGdT)

      implicit none

      real(WP), intent(in) :: redP, kc_oInf, Tloc, FC
      real(WP) :: J, H
      real(WP) :: ctmp, ntmp, dtmp
      real(WP), intent(out) :: dGdT, dlnFCdT

      ntmp = 0.75_WP - 1.27_WP*dlog10(FC)
      ctmp = -0.4_WP - 0.67_WP*dlog10(FC)
      dtmp = 0.14_WP

      J = log(redP) + ctmp
      H = J/(ntmp - dtmp*J)
      dlnFCdT = dlnFCdT/(1 + H*H)
      dGdT = -2*H/((1 + H*H)**2)*(ntmp/((ntmp - dtmp*J)**2))*(kc_oInf - 1/Tloc)

      return
    end subroutine pdf_reaction_compute_dGdT
  end subroutine react

  subroutine mix(this, dt, delta)
    use messager, only: die
    implicit none

    class(pdf), intent(inout) :: this
    real(WP), intent(inout) :: dt  !< Timestep size over which to advance
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: delta  !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    integer :: i, j, k, ip, ii

    real(WP) :: Wmix, sv
    ! Parameters
    real(WP) :: cphi, omeg

    real(WP) :: decay_f

    cphi = 4.0_WP

    ! Loop though all particles
    do ip = 1, this%np_

      i = this%p(ip)%ind(1)
      j = this%p(ip)%ind(2)
      k = this%p(ip)%ind(3)

      ! Evaluate omega (take all euler data at cell center)
      omeg = this%gammat(i, j, k)*cphi/(delta(i, j, k)**2)

      ! Compute decay factor
      decay_f = 1 - exp(-1.0_WP*omeg*dt)

      ! Advance compositions
      this%p(ip)%Yi = this%p(ip)%Yi - decay_f*(this%p(ip)%Yi - this%Yi(i, j, k, :))
      this%p(ip)%H = this%p(ip)%H - decay_f*(this%p(ip)%H - this%H(i, j, k))

      ! ! Evaluate new temperature from enthalpy
      call this%H2T(p=this%p(ip))

      ! Adjust volume (assume constant mass)
      call this%get_Wmix(p=this%p(ip), Wmix=Wmix)
      call this%get_sv(p=this%p(ip), Wmix=Wmix, sv=sv)
      this%p(ip)%vol = sv*this%p(ip)%m

    end do

  end subroutine mix

  subroutine get_eulerian_fields(this)
    use messager, only: die
    implicit none
    class(pdf), intent(inout) :: this

    integer :: i, j, k, n, myi, myj, myk, is
    integer :: i1, i2, j1, j2, k1, k2
    ! Store source term if needed
    logical :: isrc

    ! Zero out fields
    this%H = 0.0_WP
    this%Yi = 0.0_WP
    this%T = 0.0_WP
    this%Z = 0.0_WP
    this%V = 0.0_WP
    this%SV = 0.0_WP
    this%multi = 0.0_WP

    ! ---------------------------------------------- !
    ! Compute mass-weighted values on Eulerian mesh -!
    ! ---------------------------------------------- !

    ! Loop though all particles
    do i = 1, this%np_
      ! Particle indices on Eulerian mesh
      myi = this%p(i)%ind(1); myj = this%p(i)%ind(2); myk = this%p(i)%ind(3)

      ! Transfer mass-weighted enthalpy
      this%H(myi, myj, myk) = this%H(myi, myj, myk) + this%p(i)%multi*this%p(i)%m*this%p(i)%H
      ! Transfer mass-weighted max fractions
      this%Yi(myi, myj, myk, 1:npS) = this%Yi(myi, myj, myk, 1:npS) + this%p(i)%multi*this%p(i)%m*this%p(i)%Yi
      ! Transfer mass-weighted temperature
      this%T(myi, myj, myk) = this%T(myi, myj, myk) + this%p(i)%multi*this%p(i)%m*this%p(i)%T
      ! Transfer mass-weighted mixture fraction
      this%Z(myi, myj, myk) = this%Z(myi, myj, myk) + this%p(i)%multi*this%p(i)%m*this%p(i)%Zmix
      ! Transfer mass-weighted specific volume (i.e. volume itself)
      this%V(myi, myj, myk) = this%V(myi, myj, myk) + this%p(i)%multi*this%p(i)%vol
      ! Transfer mass-weighted particle multiplicity (i.e. mass itself)
      this%multi(myi, myj, myk) = this%multi(myi, myj, myk) + this%p(i)%multi*this%p(i)%m
    end do

    ! Need to update boundaries!
    ! ---------------------------------------------- !
    ! Take care of empty cells inside domain ------- !
    ! ---------------------------------------------- !
    ! Populate empty cells with surrounding values
    do k = this%cfg%kmino_, this%cfg%kmaxo_
      do j = this%cfg%jmino_, this%cfg%jmaxo_
        do i = this%cfg%imino_, this%cfg%imaxo_
          ! Skip masked cells
          if (this%cfg%VF(i, j, k) .eq. 0.0_WP) cycle

          ! Populate empty cells by sum over neighboring ones
          if (this%multi(i, j, k) .eq. 0.0_WP) then
            ! Averaging bounds
            k1 = 1
            k2 = 1
            j1 = 1
            j2 = 1
            i1 = 1
            i2 = 1
            if (k .eq. this%cfg%kmaxo_) k2 = 0
            if (k .eq. this%cfg%kmino_) k1 = 0
            if (j .eq. this%cfg%jmaxo_) j2 = 0
            if (j .eq. this%cfg%jmino_) j1 = 0
            if (i .eq. this%cfg%imaxo_) i2 = 0
            if (i .eq. this%cfg%imino_) i1 = 0
            ! Species
            do is = 1, npS
              this%Yi(i, j, k, is) = sum(this%Yi(i - i1:i + i2, j - j1:j + j2, k - k1:k + k2, is))
            end do
            ! Others
            this%H(i, j, k) = sum(this%H(i - i1:i + i2, j - j1:j + j2, k - k1:k + k2))
            this%T(i, j, k) = sum(this%T(i - i1:i + i2, j - j1:j + j2, k - k1:k + k2))
            this%multi(i, j, k) = sum(this%multi(i - i1:i + i2, j - j1:j + j2, k - k1:k + k2))

            ! ! For volume, take volume of cell itself
            this%V(i, j, k) = this%cfg%dx(i)*this%cfg%dy(j)*this%cfg%dz(k)

          end if
        end do
      end do
    end do
    ! ---------------------------------------------- !
    ! Normailize mass-weighted fields ------ ------- !
    ! ---------------------------------------------- !
    call update_boundary(this)

    ! Loop through Eulerian mesh
    do k = this%cfg%kmino_, this%cfg%kmaxo_
      do j = this%cfg%jmino_, this%cfg%jmaxo_
        do i = this%cfg%imino_, this%cfg%imaxo_
          if (this%cfg%VF(i, j, k) .eq. 0) then
            cycle
          end if
          this%H(i, j, k) = this%H(i, j, k)/this%multi(i, j, k)
          this%Yi(i, j, k, 1:npS) = this%Yi(i, j, k, 1:npS)/this%multi(i, j, k)
          this%T(i, j, k) = this%T(i, j, k)/this%multi(i, j, k)
          this%Z(i, j, k) = this%Z(i, j, k)/this%multi(i, j, k)
          this%SV(i, j, k) = this%V(i, j, k)/this%multi(i, j, k)
        end do
      end do
    end do

  contains
    subroutine update_boundary(this)
      implicit none
      class(pdf), intent(inout) :: this
      integer :: i, j, k

      ! -x
      if (this%cfg%iproc .eq. 1) then
        do i = this%cfg%imino_, this%cfg%imin_ - 1
          this%multi(i, :, :) = this%multi(this%cfg%imin_, :, :)
          this%H(i, :, :) = this%H(this%cfg%imin_, :, :)
          this%Yi(i, :, :, 1:npS) = this%Yi(this%cfg%imin_, :, :, 1:npS)
          this%T(i, :, :) = this%T(this%cfg%imin_, :, :)
          this%Z(i, :, :) = this%Z(this%cfg%imin_, :, :)
          this%V(i, :, :) = this%V(this%cfg%imin_, :, :)
        end do
      end if

      ! +x
      if (this%cfg%iproc .eq. this%cfg%npx) then
        do i = this%cfg%imax_ + 1, this%cfg%imaxo_
          this%multi(i, :, :) = this%multi(this%cfg%imax_, :, :)
          this%H(i, :, :) = this%H(this%cfg%imax_, :, :)
          this%Yi(i, :, :, 1:npS) = this%Yi(this%cfg%imax_, :, :, 1:npS)
          this%T(i, :, :) = this%T(this%cfg%imax_, :, :)
          this%Z(i, :, :) = this%Z(this%cfg%imax_, :, :)
          this%V(i, :, :) = this%V(this%cfg%imax_, :, :)
        end do
      end if

      ! -y
      if (this%cfg%jproc .eq. 1) then
        do j = this%cfg%jmino_, this%cfg%jmin_ - 1
          this%multi(:, j, :) = this%multi(:, this%cfg%jmin_, :)
          this%H(:, j, :) = this%H(:, this%cfg%jmin_, :)
          this%Yi(:, j, :, 1:npS) = this%Yi(:, this%cfg%jmin_, :, 1:npS)
          this%T(:, j, :) = this%T(:, this%cfg%jmin_, :)
          this%Z(:, j, :) = this%Z(:, this%cfg%jmin_, :)
          this%V(:, j, :) = this%V(:, this%cfg%jmin_, :)
        end do
      end if
      ! +y
      if (this%cfg%jproc .eq. this%cfg%npy) then
        do j = this%cfg%jmax_ + 1, this%cfg%jmaxo_
          this%multi(:, j, :) = this%multi(:, this%cfg%jmax_, :)
          this%H(:, j, :) = this%H(:, this%cfg%jmax_, :)
          this%Yi(:, j, :, 1:npS) = this%Yi(:, this%cfg%jmax_, :, 1:npS)
          this%T(:, j, :) = this%T(:, this%cfg%jmax_, :)
          this%Z(:, j, :) = this%Z(:, this%cfg%jmax_, :)
          this%V(:, j, :) = this%V(:, this%cfg%jmax_, :)
        end do
      end if

      ! -z
      if (this%cfg%kproc .eq. 1) then
        do k = this%cfg%kmino_, this%cfg%kmin_ - 1
          this%multi(:, :, k) = this%multi(:, :, this%cfg%kmin_)
          this%H(:, :, k) = this%H(:, :, this%cfg%kmin_)
          this%Yi(:, :, k, 1:npS) = this%Yi(:, :, this%cfg%kmin_, 1:npS)
          this%T(:, :, k) = this%T(:, :, this%cfg%kmin_)
          this%Z(:, :, k) = this%Z(:, :, this%cfg%kmin_)
          this%V(:, :, k) = this%V(:, :, this%cfg%kmin_)
        end do
      end if

      ! +z
      if (this%cfg%kproc .eq. this%cfg%npz) then
        do k = this%cfg%kmax_ + 1, this%cfg%kmaxo_
          this%multi(:, :, k) = this%multi(:, :, this%cfg%kmax_)
          this%H(:, :, k) = this%H(:, :, this%cfg%kmax_)
          this%Yi(:, :, k, 1:npS) = this%Yi(:, :, this%cfg%kmax_, 1:npS)
          this%T(:, :, k) = this%T(:, :, this%cfg%kmax_)
          this%Z(:, :, k) = this%Z(:, :, this%cfg%kmax_)
          this%V(:, :, k) = this%V(:, :, this%cfg%kmax_)
        end do
      end if

      call this%cfg%sync(this%multi)
      call this%cfg%sync(this%H)
      do i = 1, npS
        call this%cfg%sync(this%Yi(:, :, :, i))
      end do
      call this%cfg%sync(this%T)
      call this%cfg%sync(this%Z)
      call this%cfg%sync(this%V)

    end subroutine update_boundary
  end subroutine get_eulerian_fields

  subroutine get_eulerian_fields_mid(this)
    implicit none
    class(pdf), intent(inout) :: this

    integer :: i, j, k, n, myi, myj, myk, is
    integer :: i1, i2, j1, j2, k1, k2
    ! Store source term if needed
    logical :: isrc

    ! Zero out fields
    this%H = 0.0_WP
    this%Yi = 0.0_WP
    this%T = 0.0_WP
    this%Z = 0.0_WP
    this%V = 0.0_WP
    this%SV = 0.0_WP
    this%multi = 0.0_WP

    ! ---------------------------------------------- !
    ! Compute mass-weighted values on Eulerian mesh -!
    ! ---------------------------------------------- !

    ! Loop though all particles
    do i = 1, this%np_
      ! Particle indices on Eulerian mesh
      myi = this%p(i)%ind(1); myj = this%p(i)%ind(2); myk = this%p(i)%ind(3)

      ! Transfer mass-weighted enthalpy
      this%H(myi, myj, myk) = this%H(myi, myj, myk) + this%p(i)%multi*this%p(i)%m*this%p(i)%H
      ! Transfer mass-weighted max fractions
      this%Yi(myi, myj, myk, 1:npS) = this%Yi(myi, myj, myk, 1:npS) + this%p(i)%multi*this%p(i)%m*this%p(i)%Yi
      ! Transfer mass-weighted temperature
      this%T(myi, myj, myk) = this%T(myi, myj, myk) + this%p(i)%multi*this%p(i)%m*this%p(i)%T
      ! Transfer mass-weighted mixture fraction
      this%Z(myi, myj, myk) = this%Z(myi, myj, myk) + this%p(i)%multi*this%p(i)%m*this%p(i)%Zmix
      ! Transfer mass-weighted specific volume (i.e. volume itself)
      this%V(myi, myj, myk) = this%V(myi, myj, myk) + this%p(i)%multi*this%p(i)%vol
      ! Transfer mass-weighted particle multiplicity (i.e. mass itself)
      this%multi(myi, myj, myk) = this%multi(myi, myj, myk) + this%p(i)%multi*this%p(i)%m
    end do

    ! No need to worry about ghost or empty cells here since data are
    ! used in mixing only and empty cells are not used in mixing
    ! Normalize all necessary variables

    ! ---------------------------------------------- !
    ! Normailize mass-weighted fields ------ ------- !
    ! ---------------------------------------------- !

    ! Loop through Eulerian mesh
    do k = this%cfg%kmin_, this%cfg%kmax_
      do j = this%cfg%jmin_, this%cfg%jmax_
        do i = this%cfg%imin_, this%cfg%imax_
          if (this%cfg%VF(i, j, k) .eq. 0) cycle
          if (this%multi(i, j, k) .eq. 0) cycle

          this%H(i, j, k) = this%H(i, j, k)/this%multi(i, j, k)
          this%Yi(i, j, k, 1:npS) = this%Yi(i, j, k, 1:npS)/this%multi(i, j, k)
          this%T(i, j, k) = this%T(i, j, k)/this%multi(i, j, k)
          this%Z(i, j, k) = this%Z(i, j, k)/this%multi(i, j, k)
          this%SV(i, j, k) = this%V(i, j, k)/this%multi(i, j, k)
        end do
      end do
    end do
  end subroutine get_eulerian_fields_mid

  subroutine get_density(this, rho)
    use messager, only: die
    implicit none
    class(pdf), intent(inout) :: this
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(inout) :: rho !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    integer :: i, j, k
    ! Loop through Eulerian mesh
    do k = this%cfg%kmino_, this%cfg%kmaxo_
      do j = this%cfg%jmino_, this%cfg%jmaxo_
        do i = this%cfg%imino_, this%cfg%imaxo_
          ! Skip masked cells
          if (this%cfg%VF(i, j, k) .eq. 0.0_WP) then
            rho(i, j, k) = 1.0_WP
            cycle
          end if
          rho(i, j, k) = 1.0_WP/this%SV(i, j, k)
          ! rho(i, j, k) = 1.0_WP
        end do
      end do
    end do
  end subroutine get_density

  subroutine get_viscosity(this, visc)
    implicit none
    class(pdf), intent(inout) :: this
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(out) :: visc !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    integer :: i, j, k
    real(WP), parameter :: T0 = 500.0_WP
    real(WP), parameter :: visc0 = 0.000038275_WP
    real(WP), parameter :: alpha = 1.67_WP
    real(WP) :: Tmix

    ! Loop through Eulerian mesh
    do k = this%cfg%kmino_, this%cfg%kmaxo_
      do j = this%cfg%jmino_, this%cfg%jmaxo_
        do i = this%cfg%imino_, this%cfg%imaxo_
          ! Skip masked cells
          if (this%cfg%VF(i, j, k) .eq. 0.0_WP) then
            visc(i, j, k) = 1.0_WP
            cycle
          end if
          ! Temperature
          Tmix = min(max(this%T(i, j, k), T_min), Tmax)
          ! Viscosity
          visc(i, j, k) = visc0*(Tmix/T0)**alpha/this%SV(i, j, k)
          ! visc(i, j, k) = 1.0e-5_WP
        end do
      end do
    end do
  end subroutine get_viscosity

  subroutine get_diffusivity(this, diff)
    implicit none
    class(pdf), intent(inout) :: this
    real(WP), dimension(this%cfg%imino_:this%cfg%imaxo_, this%cfg%jmino_:this%cfg%jmaxo_, this%cfg%kmino_:this%cfg%kmaxo_), intent(out) :: diff !< Needs to be (imino_:imaxo_,jmino_:jmaxo_,kmino_:kmaxo_)
    integer :: i, j, k
    real(WP), parameter :: T0 = 500.0_WP
    real(WP), parameter :: diff0 = 0.000038275_WP
    real(WP), parameter :: alpha = 0.6550_WP
    real(WP), parameter :: beta = 1.4156_WP
    real(WP) :: Tmix

    ! Loop through Eulerian mesh
    do k = this%cfg%kmino_, this%cfg%kmaxo_
      do j = this%cfg%jmino_, this%cfg%jmaxo_
        do i = this%cfg%imino_, this%cfg%imaxo_
          ! Skip masked cells
          if (this%cfg%VF(i, j, k) .eq. 0.0_WP) cycle
          ! Temperature
          Tmix = min(max(this%T(i, j, k), T_min), Tmax)
          ! Viscosity
          diff(i, j, k) = beta*diff0*(Tmix/T0)**alpha/this%SV(i, j, k)
        end do
      end do
    end do
  end subroutine get_diffusivity

  subroutine clip(this, myY)
    implicit none
    class(pdf), intent(inout) :: this
    real(WP), intent(inout), dimension(npS) :: myY

    myY = min(max(myY, 0.0_WP), 1.0_WP)
    myY = myY/sum(myY)
    return
  end subroutine clip

  subroutine get_Wmix(this, p, Wmix)
    implicit none
    class(pdf), intent(inout) :: this
    class(pmc), intent(inout) :: p
    real(WP), intent(out) :: Wmix
    real(WP), dimension(npS) :: scalar_t

    scalar_t = min(max(p%Yi, 0.0_WP), 1.0_WP)

    Wmix = sum(scalar_t)/sum(scalar_t/Wsp)

    return
  end subroutine get_Wmix

  subroutine get_sv(this, p, Wmix, sv)
    implicit none
    class(pdf), intent(inout) :: this
    class(pmc), intent(inout) :: p
    real(WP), intent(in) :: Wmix
    real(WP), intent(out) :: sv

    sv = R_cst*p%T/(this%Pthermo*Wmix)

  end subroutine get_sv

  subroutine get_thermodata(this, p, cpm, hm)
    implicit none
    class(pdf), intent(inout) :: this
    class(pmc), intent(inout) :: p

    real(WP), intent(out):: cpm, hm
    real(WP), dimension(npS) :: sol

    ! Function in mechanism.f file
    call pdf_mech_thermodata(p%T)
    cpm = sum(sol*Cpsp)
    hm = sum(sol*hsp)

    return
  end subroutine get_thermodata

  subroutine H2T(this, p)
    use messager, only: die
    implicit none
    class(pdf), intent(inout) :: this
    type(pmc), intent(inout) :: p
    real(WP) :: f, fprime, T_tmp, cp_tmp, h_tmp, newton_tol
    integer :: n, nnewton

    newton_tol = 1.0e-4_WP
    nnewton = 20

    ! Newton iterations
    T_tmp = 0.0_WP
    n = 0
    do while (n .le. nnewton .and. abs(T_tmp - p%T)/p%T .gt. newton_tol)

      ! Reset initial guess
      T_tmp = p%T
      ! Update enthalpy
      call pdf_mech_thermodata(T_tmp)

      cp_tmp = sum(p%Yi*Cpsp)
      h_tmp = sum(p%Yi*hsp)

      ! Advance newton
      f = p%H - h_tmp
      fprime = -cp_tmp + tiny(1.0_WP)
      p%T = T_tmp - f/fprime
      n = n + 1

    end do
  end subroutine H2T

  !> Extract various monitoring data from particle field
  subroutine get_max(this)
    use messager, only: die
    use mpi_f08, only: MPI_ALLREDUCE, MPI_MAX, MPI_MIN, MPI_SUM
    use parallel, only: MPI_REAL_WP
    implicit none
    class(pdf), intent(inout) :: this
    real(WP) :: buf, safe_np
    integer :: mynp_min, mynp_max, mynp_mean, mynp_total
    integer :: i, j, k, ierr

    ! Create safe np
    safe_np = real(max(this%np, 1), WP)

    ! Diameter and velocity min/max/mean
    this%Tmin = huge(1.0_WP); this%Tmax = -huge(1.0_WP); this%Tmean = 0.0_WP
    mynp_min = huge(1); mynp_max = -huge(1); mynp_mean = 0; mynp_total = this%np_

    do i = 1, this%np_
      this%Tmin = min(this%Tmin, this%p(i)%T); this%Tmax = max(this%Tmax, this%p(i)%T); this%Tmean = this%Tmean + this%p(i)%T
    end do

    do k = this%cfg%kmin_, this%cfg%kmax_
      do j = this%cfg%jmin_, this%cfg%jmax_
        do i = this%cfg%imin_, this%cfg%imax_
          mynp_min = min(mynp_min, this%npincell(i, j, k))
          mynp_max = max(mynp_max, this%npincell(i, j, k))
          mynp_mean = mynp_mean + this%npincell(i, j, k)
        end do
      end do
    end do

    call MPI_ALLREDUCE(this%Tmin, buf, 1, MPI_REAL_WP, MPI_MIN, this%cfg%comm, ierr); this%Tmin = buf
    call MPI_ALLREDUCE(this%Tmax, buf, 1, MPI_REAL_WP, MPI_MAX, this%cfg%comm, ierr); this%Tmax = buf
    call MPI_ALLREDUCE(this%Tmean, buf, 1, MPI_REAL_WP, MPI_SUM, this%cfg%comm, ierr); this%Tmean = buf/safe_np

    call MPI_ALLREDUCE(mynp_min, this%np_min, 1, MPI_INTEGER, MPI_MIN, this%cfg%comm, ierr); 
    call MPI_ALLREDUCE(mynp_max, this%np_max, 1, MPI_INTEGER, MPI_MAX, this%cfg%comm, ierr); 
    call MPI_ALLREDUCE(mynp_mean, this%np_mean, 1, MPI_INTEGER, MPI_SUM, this%cfg%comm, ierr); this%np_mean = this%np_mean/(this%cfg%nx*this%cfg%ny*this%cfg%nz)
    call MPI_ALLREDUCE(mynp_total, this%np_total, 1, MPI_INTEGER, MPI_SUM, this%cfg%comm, ierr)

    ! Diameter and velocity variance
    this%Tvar = 0.0_WP

    do i = 1, this%np_
      this%Tvar = this%Tvar + (this%p(i)%T - this%Tmean)**2.0_WP
    end do
    call MPI_ALLREDUCE(this%Tvar, buf, 1, MPI_REAL_WP, MPI_SUM, this%cfg%comm, ierr); this%Tvar = buf/safe_np

  end subroutine get_max

  !> Update particle mesh using our current particles
  subroutine update_partmesh(this, pmesh)
    use partmesh_class, only: partmesh
    implicit none
    class(pdf), intent(inout) :: this
    class(partmesh), intent(inout) :: pmesh
    integer :: i
    ! Reset particle mesh storage
    call pmesh%reset()
    ! Nothing else to do if no particle is present
    if (this%np_ .eq. 0) return
    ! Copy particle info
    call pmesh%set_size(this%np_)
    do i = 1, this%np_
      pmesh%pos(:, i) = this%p(i)%pos
    end do
  end subroutine update_partmesh

  !> Creation of the MPI datatype for particle
  subroutine prepare_mpi_part()
    use mpi_f08
    use messager, only: die
    implicit none
    integer(MPI_ADDRESS_KIND), dimension(part_nblock) :: disp
    integer(MPI_ADDRESS_KIND) :: lb, extent
    type(MPI_Datatype) :: MPI_PART_TMP
    integer :: i, mysize, ierr
    ! Prepare the displacement array
    disp(1) = 0
    do i = 2, part_nblock
      call MPI_Type_size(part_tblock(i - 1), mysize, ierr)
      disp(i) = disp(i - 1) + int(mysize, MPI_ADDRESS_KIND)*int(part_lblock(i - 1), MPI_ADDRESS_KIND)
    end do
    ! Create and commit the new type
    call MPI_Type_create_struct(part_nblock, part_lblock, disp, part_tblock, MPI_PART_TMP, ierr)
    call MPI_Type_get_extent(MPI_PART_TMP, lb, extent, ierr)
    call MPI_Type_create_resized(MPI_PART_TMP, lb, extent, MPI_PART, ierr)
    call MPI_Type_commit(MPI_PART, ierr)
    ! If a problem was encountered, say it
    if (ierr .ne. 0) call die('[pdf prepare_mpi_part] MPI Particle type creation failed')
    ! Get the size of this type
    call MPI_type_size(MPI_PART, MPI_PART_SIZE, ierr)
  end subroutine prepare_mpi_part

  !> Synchronize particle arrays across processors
  subroutine sync(this)
    use mpi_f08
    implicit none
    class(pdf), intent(inout) :: this
    integer, dimension(0:this%cfg%nproc - 1) :: nsend_proc, nrecv_proc
    integer, dimension(0:this%cfg%nproc - 1) :: nsend_disp, nrecv_disp
    integer :: n, prank, ierr
    type(pmc), dimension(:), allocatable :: buf_send
    ! Recycle first to minimize communication load
    call this%recycle()
    ! Prepare information about what to send
    nsend_proc = 0
    do n = 1, this%np_
      prank = this%cfg%get_rank(this%p(n)%ind)
      nsend_proc(prank) = nsend_proc(prank) + 1
    end do
    nsend_proc(this%cfg%rank) = 0
    ! Inform processors of what they will receive
    call MPI_ALLtoALL(nsend_proc, 1, MPI_INTEGER, nrecv_proc, 1, MPI_INTEGER, this%cfg%comm, ierr)
    ! Prepare displacements for all-to-all
    nsend_disp(0) = 0
    nrecv_disp(0) = this%np_   !< Directly add particles at the end of main array
    do n = 1, this%cfg%nproc - 1
      nsend_disp(n) = nsend_disp(n - 1) + nsend_proc(n - 1)
      nrecv_disp(n) = nrecv_disp(n - 1) + nrecv_proc(n - 1)
    end do
    ! Allocate buffer to send particles
    allocate (buf_send(sum(nsend_proc)))
    ! Pack the particles in the send buffer
    nsend_proc = 0
    do n = 1, this%np_
      ! Get the rank
      prank = this%cfg%get_rank(this%p(n)%ind)
      ! Skip particles still inside
      if (prank .eq. this%cfg%rank) cycle
      ! Pack up for sending
      nsend_proc(prank) = nsend_proc(prank) + 1
      buf_send(nsend_disp(prank) + nsend_proc(prank)) = this%p(n)
      ! Flag particle for removal
      this%p(n)%multi = 0
    end do
    ! Allocate buffer for receiving particles
    call this%resize(this%np_ + sum(nrecv_proc))
    ! Perform communication
    call MPI_ALLtoALLv(buf_send, nsend_proc, nsend_disp, MPI_PART, this%p, nrecv_proc, nrecv_disp, MPI_PART, this%cfg%comm, ierr)
    ! Deallocate buffer
    deallocate (buf_send)
    ! Recycle to remove duplicate particles
    call this%recycle()
  end subroutine sync

  !> Adaptation of particle array size
  subroutine resize(this, n)
    implicit none
    class(pdf), intent(inout) :: this
    integer, intent(in) :: n
    type(pmc), dimension(:), allocatable :: tmp
    integer :: size_now, size_new
    ! Resize particle array to size n
    if (.not. allocated(this%p)) then
      ! Allocate directly to size n
      allocate (this%p(n))
      this%p(1:n)%multi = 0
    else
      ! Update from a non-zero size to another non-zero size
      size_now = size(this%p, dim=1)
      if (n .gt. size_now) then
        size_new = max(n, int(real(size_now, WP)*coeff_up))
        allocate (tmp(size_new))
        tmp(1:size_now) = this%p
        tmp(size_now + 1:)%multi = 0
        call move_alloc(tmp, this%p)
      else if (n .lt. int(real(size_now, WP)*coeff_dn)) then
        allocate (tmp(n))
        tmp(1:n) = this%p(1:n)
        call move_alloc(tmp, this%p)
      end if
    end if
  end subroutine resize

  !> Clean-up of particle array by removing flag=1 particles
  subroutine recycle(this)
    implicit none
    class(pdf), intent(inout) :: this
    integer :: new_size, i, ierr
    ! Compact all active particles at the beginning of the array
    new_size = 0
    if (allocated(this%p)) then
      do i = 1, size(this%p, dim=1)
        if (this%p(i)%multi .ne. 0) then
          new_size = new_size + 1
          if (i .ne. new_size) then
            this%p(new_size) = this%p(i)
            this%p(i)%multi = 0
          end if
        end if
      end do
    end if
    ! Resize to new size
    call this%resize(new_size)
    ! Update number of particles
    this%np_ = new_size
    call MPI_ALLGATHER(this%np_, 1, MPI_INTEGER, this%np_proc, 1, MPI_INTEGER, this%cfg%comm, ierr)
    this%np = sum(this%np_proc)
  end subroutine recycle

  !> Parallel write particles to file
  subroutine write (this, filename)
    use mpi_f08
    use messager, only: die
    use parallel, only: info_mpiio
    implicit none
    class(pdf), intent(inout) :: this
    character(len=*), intent(in) :: filename
    type(MPI_File) :: ifile
    type(MPI_Status):: status
    integer(kind=MPI_OFFSET_KIND) :: offset
    integer :: i, ierr, iunit

    ! Root serial-writes the file header
    if (this%cfg%amRoot) then
      ! Open the file
      open (newunit=iunit, file=trim(filename), form='unformatted', status='replace', access='stream', iostat=ierr)
      if (ierr .ne. 0) call die('[pdf write] Problem encountered while serial-opening data file: '//trim(filename))
      ! Number of particles and particle object size
      write (iunit) this%np, MPI_PART_SIZE
      ! Done with the header
      close (iunit)
    end if

    ! The rest is done in parallel
    call MPI_FILE_OPEN(this%cfg%comm, trim(filename), IOR(MPI_MODE_WRONLY, MPI_MODE_APPEND), info_mpiio, ifile, ierr)
    if (ierr .ne. 0) call die('[pdf write] Problem encountered while parallel-opening data file: '//trim(filename))

    ! Get current position
    call MPI_FILE_GET_POSITION(ifile, offset, ierr)

    ! Compute the offset and write
    do i = 1, this%cfg%rank
      offset = offset + int(this%np_proc(i), MPI_OFFSET_KIND)*int(MPI_PART_SIZE, MPI_OFFSET_KIND)
    end do
    if (this%np_ .gt. 0) call MPI_FILE_WRITE_AT(ifile, offset, this%p, this%np_, MPI_PART, status, ierr)

    ! Close the file
    call MPI_FILE_CLOSE(ifile, ierr)

    ! Log/screen output
    logging: block
      use, intrinsic :: iso_fortran_env, only: output_unit
      use param, only: verbose
      use messager, only: log
      use string, only: str_long
      character(len=str_long) :: message
      if (this%cfg%amRoot) then
  write(message,'("Wrote ",i0," particles to file [",a,"] on partitioned grid [",a,"]")') this%np,trim(filename),trim(this%cfg%name)
        if (verbose .gt. 2) write (output_unit, '(a)') trim(message)
        if (verbose .gt. 1) call log(message)
      end if
    end block logging

  end subroutine write

  !> Parallel read particles to file
  subroutine read (this, filename)
    use mpi_f08
    use messager, only: die
    use parallel, only: info_mpiio
    implicit none
    class(pdf), intent(inout) :: this
    character(len=*), intent(in) :: filename
    type(MPI_File) :: ifile
    type(MPI_Status):: status
    integer(kind=MPI_OFFSET_KIND) :: offset, header_offset
    integer :: i, j, ierr, npadd, psize, nchunk, cnt
    integer, dimension(:, :), allocatable :: ppp

    ! First open the file in parallel
    call MPI_FILE_OPEN(this%cfg%comm, trim(filename), MPI_MODE_RDONLY, info_mpiio, ifile, ierr)
    if (ierr .ne. 0) call die('[pdf read] Problem encountered while reading data file: '//trim(filename))

    ! Read file header first
    call MPI_FILE_READ_ALL(ifile, npadd, 1, MPI_INTEGER, status, ierr)
    call MPI_FILE_READ_ALL(ifile, psize, 1, MPI_INTEGER, status, ierr)

    ! Remember current position
    call MPI_FILE_GET_POSITION(ifile, header_offset, ierr)

    ! Check compatibility of particle type
    if (psize .ne. MPI_PART_SIZE) call die('[pdf read] Particle type unreadable')

    ! Naively share reading task among all processors
    nchunk = int(npadd/(this%cfg%nproc*part_chunk_size)) + 1
    allocate (ppp(this%cfg%nproc, nchunk))
    ppp = int(npadd/(this%cfg%nproc*nchunk))
    cnt = 0
    out: do j = 1, nchunk
      do i = 1, this%cfg%nproc
        cnt = cnt + 1
        if (cnt .gt. mod(npadd, this%cfg%nproc*nchunk)) exit out
        ppp(i, j) = ppp(i, j) + 1
      end do
    end do out

    ! Read by chunk
    do j = 1, nchunk
      ! Find offset
         offset=header_offset+int(MPI_PART_SIZE,MPI_OFFSET_KIND)*int(sum(ppp(1:this%cfg%rank,:))+sum(ppp(this%cfg%rank+1,1:j-1)),MPI_OFFSET_KIND)
      ! Resize particle array
      call this%resize(this%np_ + ppp(this%cfg%rank + 1, j))
      ! Read this file
  call MPI_FILE_READ_AT(ifile,offset,this%p(this%np_+1:this%np_+ppp(this%cfg%rank+1,j)),ppp(this%cfg%rank+1,j),MPI_PART,status,ierr)
      ! Most general case: relocate every droplet
      do i = this%np_ + 1, this%np_ + ppp(this%cfg%rank + 1, j)
        this%p(i)%ind = this%cfg%get_ijk_global(this%p(i)%pos, this%p(i)%ind)
      end do
      ! Exchange all that
      call this%sync()
    end do

    ! Close the file
    call MPI_FILE_CLOSE(ifile, ierr)

    ! Log/screen output
    logging: block
      use, intrinsic :: iso_fortran_env, only: output_unit
      use param, only: verbose
      use messager, only: log
      use string, only: str_long
      character(len=str_long) :: message
      if (this%cfg%amRoot) then
   write(message,'("Read ",i0," particles from file [",a,"] on partitioned grid [",a,"]")') npadd,trim(filename),trim(this%cfg%name)
        if (verbose .gt. 2) write (output_unit, '(a)') trim(message)
        if (verbose .gt. 1) call log(message)
      end if
    end block logging

  end subroutine read
end module pdf_class
