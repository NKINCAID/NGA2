!> Finite chem class
!> Extends multiscalar class for finite chem related functions
module finitechem_class
    use multivdscalar_class, only: multivdscalar
    use config_class, only: config
    use dvode_f90_m
    use precision, only: WP
    use fcmech
    implicit none
    private

    ! Expose type/constructor/methods
    public :: finitechem

    type(vode_opts), save :: opts

    ! Clipping values for temperature
    real(WP), parameter :: T_min = 50.0_WP
    real(WP), parameter :: T_max = 5000.0_WP

    integer :: nspec1 = nspec + 1
    integer :: nspec2 = nspec + 2

    ! DVODE variables
    real(WP) :: atol = 1.0e-8_WP
    real(WP) :: rtol = 1.0e-8_WP
    integer :: istate, itask

    ! Solver options
    logical :: use_jacanal = .false.
    logical :: use_scheduler = .false.

    logical :: use_lewis = .false.

    !> Finite chemistry solver object definition
    type, extends(multivdscalar) :: finitechem
        real(WP) :: vol                  !< Volume
        real(WP) :: m                    !< Mass
        real(WP) :: h                    !< Enthalpy
        real(WP) :: T                    !< Temperature
        real(WP) :: Zmix                 !< Mixture fraction
        real(WP), dimension(nspec) :: Y   !< Composition

        real(WP) :: Pthermo, Pthermo_old     !< Thermodynamic pressure

        real(WP), dimension(:, :, :), allocatable :: visc     !< Viscosity field

        real(WP), dimension(:, :, :), allocatable :: lambda     !< Thermal conductivity
        real(WP), dimension(:, :, :), allocatable :: cp     !< Heat capacity

        logical :: use_explicit_try = .false.

    contains
        ! procedure :: react

        ! procedure :: get_density
        procedure :: get_viscosity
        procedure :: get_diffusivity
        procedure :: get_cpmix
        procedure :: get_Wmix

        ! procedure :: get_Wmix
        ! procedure :: get_sv
        ! procedure :: get_thermodata
        ! procedure :: H2T
        procedure :: react

        procedure :: clip

        procedure :: test
    end type finitechem

    !> Declare fc model constructor
    interface finitechem
        procedure constructor
    end interface finitechem

contains

    !> FC model constructor from multivdscalar
    function constructor(cfg, scheme, name) result(self)
        implicit none
        type(finitechem) :: self
        class(config), target, intent(in) :: cfg
        integer, intent(in) :: scheme
        character(len=*), optional :: name
        character(len=str_medium), dimension(nspec) :: names
        integer :: i
        ! Create a six-scalar solver for conformation tensor
        self%multivdscalar = multivdscalar(cfg=cfg, scheme=scheme, nscalar=nspec + 1, name=name)
        call fcmech_get_speciesnames(names)
        do i = 1, nspec
            self%SCname(i) = names(i)
        end do
        self%SCname(nspec + 1) = "T"

    end function constructor

    subroutine test(this, Wmix)
        implicit none
        class(finitechem), intent(inout) :: this
        real(WP), intent(out) :: Wmix
        real(WP), dimension(nspec) :: scalar_t

        print *, nspec

        return
    end subroutine test

    subroutine clip(this, myY)
        implicit none
        class(finitechem), intent(inout) :: this
        real(WP), intent(inout), dimension(nspec) :: myY

        myY = min(max(myY, 0.0_WP), 1.0_WP)
        myY = myY/sum(myY)
        return
    end subroutine clip

    subroutine react(this, dt)
        use mpi_f08
        use messager, only: die
        implicit none
        class(finitechem), intent(inout) :: this
        real(WP), intent(in) :: dt  !< Timestep size over which to advance

        integer :: i, j, k, myi
        real(WP), dimension(nspec + 1) :: sol

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

            do k = this%cfg%kmino_, this%cfg%kmaxo_
                do j = this%cfg%jmino_, this%cfg%jmaxo_
                    do i = this%cfg%imino_, this%cfg%imaxo_
                        ! Package initial solution vector
                        sol(1:nspec) = this%SC(i, j, k, 1:nspec)
                        sol(nspec1) = this%SC(i, j, k, nspec1)
                        ! Advance the chemical equations for each particle
                        call fc_reaction_compute_sol(sol, this%pthermo, dt)
                        ! Transfer back to particle structure
                        this%SC(i, j, k, 1:nspec) = sol(1:nspec)
                        this%SC(i, j, k, nspec1) = sol(nspec1)

                    end do
                end do
            end do

            return
        end if

    contains
        ! ---------------------------------------------------------------------------------- !
        ! ======================================== !
        ! Compute solution after time step delta t !
        ! ======================================== !
        subroutine fc_reaction_compute_sol(sol, mypressure, dt)
            use random
            use messager, only: die
            implicit none

            real(WP), dimension(nspec1) :: sol, dsol, solcheck
            real(WP) :: dt, t_chem
            real(WP) :: mypressure
            real(WP) :: tstop, tstart
            integer :: ii
            ! external :: fc_reaction_compute_rhs
            ! external :: fc_reaction_compute_jac
            ! external :: pdfrmap1
            real(WP), dimension(22) :: rstats
            integer, dimension(31) :: istats

            ! ISAT parameters
            real(WP) :: xx(nspec2), f(nspec1), dfdx(nspec1, nspec2), hvar(1), stats(100)
            integer :: iusr(1)

            ! Save solution
            solcheck = sol

            ! Set values for DVODE
            itask = 1; istate = 1; tstart = 0.0_WP; tstop = dt

            ! -------------------------------- !
            ! ------ Direct integration ------ !
            ! -------------------------------- !
            call fc_reaction_compute_rhs(nspec1, 0.0_WP, solcheck, dsol)
            t_chem = sol(nspec1)/(abs(dsol(nspec1)) + epsilon(1.0_WP))

            if (dt .lt. 0.0001_WP*t_chem .and. this%use_explicit_try) then
                sol = sol + dsol*dt
            else
                ! Integrate using DVODE
                if (use_jacanal) then

                opts = set_opts(dense_j=.true., mxstep=500000, abserr=atol, relerr=rtol, tcrit=tstop, user_supplied_jacobian=.true.)
             call dvode_f90(fc_reaction_compute_rhs, nspec1, sol, tstart, tstop, itask, istate, opts, j_fcn=fc_reaction_compute_jac)

                else

                    opts = set_opts(method_flag=22, mxstep=500000, abserr=atol, relerr=rtol, tcrit=tstop)
                    call dvode_f90(fc_reaction_compute_rhs, nspec1, sol, tstart, tstop, itask, istate, opts)

                end if
                call get_stats(rstats, istats)
                call release_opts_arrays(opts)

                ! Error handling
                if (istate .ne. 2) then
                    print *, 'NCF: ', istats(21) ! No. of convergence failures of the nonlinear solver so far.
                    print *, 'NEF: ', istats(22) ! No. of error test failures of the integrator so far.
                    print *, 'tstop: ', tstop, ' tstart:', tstart, ' itask: ', itask, ' istate: ', istate
                    print *, 'solold---------'
                    do ii = 1, nspec
                        print *, 'sol(', ii, ') = ', solcheck(ii), '_WP !'
                    end do
                    print *, 'sol(NT) = ', solcheck(nspec + 1), '_WP !', 'T'
                    print *, '-------------------------------'
                    print *, 'sol', sol
                    print *, '-------------------------------'
                    do ii = 1, nspec
                        print *, 'dsol(', ii, ') = ', dsol(ii), '_WP !'
                    end do
                    print *, 'T', 'dsol(ii)', dsol(nspec + 1)
                    call die('fc_reaction_source: Direct integration - DVODE failed to converge.')
                end if
            end if

            ! Clip and renormalize, just in case
            call this%clip(sol(1:nspec))

            return
        end subroutine fc_reaction_compute_sol
        ! ================================================================== !
        ! Computes the chemical source term of the system (called by solver) !
        ! ================================================================== !
        subroutine fc_reaction_compute_rhs(n_, t_, sol, rhs)

            implicit none

            integer, intent(in) :: n_
            real(WP), intent(in) :: t_
            real(WP), dimension(n_), intent(in)  :: sol
            real(WP), dimension(n_), intent(out) :: rhs
            real(WP) :: Cp_mix, Wmix, RHOmix
            real(WP), dimension(nspec) :: C
            real(WP), dimension(nTB + nFO) :: M
            real(WP), dimension(nreac + nreac_reverse) :: W, K

            ! Reset rhs
            rhs = 0.0_WP

            ! Get W of mixture
            call this%get_Wmix(sol(1:nspec), Wmix)

            ! Get Cp of mixture and update hsp
            call this%get_cpmix(sol(1:nspec), sol(nspec1), Cp_mix)

            ! Get RHO of mixture
            RHOmix = this%Pthermo*Wmix/(Rcst*sol(nspec1))

            ! Calculate concentrations of unsteady species
            c = RHOmix*sol(1:nspec)/W_sp(1:nspec)

            call get_thirdbodies(M, c)

            call get_rate_coefficients(k, M, sol(nspec1), this%Pthermo)

            call get_reaction_rates(w, k, M, c)

            call get_production_rates(rhs, w)

            ! Transform concentration into mass fraction
            rhs(1:nspec) = rhs(1:nspec)*W_sp(1:nspec)/RHOmix

            ! Temperature rhs from change in concentration
            rhs(nspec1) = -sum(hsp(1:nspec)*rhs(1:nspec))/Cp_mix

            return
        end subroutine fc_reaction_compute_rhs

        subroutine fc_reaction_thermodata(sol, temp, cpm, hm)
            implicit none

            real(WP) :: temp, cpm, hm
            real(WP), dimension(nspec) :: sol

            ! Function in mechanism.f file
            call fcmech_thermodata(temp)
            cpm = sum(sol*Cpsp)
            hm = sum(sol*hsp)

            return
        end subroutine fc_reaction_thermodata

        ! ---------------------------------------------------------------------------------- !
        ! ========================================================== !
        ! Computes the approximate analytical jacobian of the system !
        ! ========================================================== !
        subroutine fc_reaction_compute_jac(n_, t_, sol, ml, mu, jac, nrpd)
            implicit none

            ! ! Input
            integer :: n_, ml, mu, nrpd
            real(WP) :: t_
            real(WP), dimension(n_) :: sol
            real(WP), dimension(nrpd, n_) :: jac

            ! Local variables
            real(WP) :: Cp_mix, Wmix, RHOmix
            real(WP), dimension(nspec) :: C, Cdot
            real(WP), dimension(nTB + nFO) :: M
            real(WP), dimension(nreac + nreac_reverse) :: W, K

            ! Get W of mixture
            call this%get_Wmix(sol(1:nspec), Wmix)

            ! Get Cp of mixture and update hsp
            call this%get_cpmix(sol(1:nspec), sol(nspec1), Cp_mix)

            ! Get RHO of mixture
            RHOmix = this%Pthermo*Wmix/(Rcst*sol(nspec1))

            call get_thirdbodies(M, c)

            call get_rate_coefficients(k, M, sol(nspec1), this%Pthermo)

            call get_reaction_rates(w, k, M, c)

            call get_production_rates(Cdot, w)

            ! Get analytical Jacobian from mechanism file
            ! call fc_reaction_getjacobian(sol(1:nspec), sol(nspec + 1), M, Cdot, K, RHOmix, Cp_mix, Wmix, jac)

            return
        end subroutine fc_reaction_compute_jac

        ! ------------------------------------------------------------------------------------------------ !
        ! ================================================ !
        ! Used by finitechem_getjacobian
        ! to compute one of the more involved terms
        ! dealing with pressure dependent rate coefficients
        ! ================================================ !
        subroutine fc_reaction_compute_dlnFCdT(fca_i, fcta_i, fcb_i, fctb_i, fcc_i, fctc_i, Tloc, FC, dlnFCdT)

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
        end subroutine fc_reaction_compute_dlnFCdT

        ! ------------------------------------------------------------------------------------------------ !
        ! ================================================ !
        ! Used by finitechem_getjacobian
        ! to compute one of the more involved terms
        ! dealing with pressure dependent rate coefficients
        ! ================================================ !
        subroutine fc_reaction_compute_dGdT(redP, kc_oInf, Tloc, FC, dlnFCdT, dGdT)

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
        end subroutine fc_reaction_compute_dGdT
    end subroutine react

! --------------------------------------------------------------------------------------------!
! =================================== !
! Compute density from mass fractions !
! =================================== !
    subroutine get_density(this)
        implicit none
        class(finitechem), intent(inout) :: this

        integer :: i, j, k
        real(WP):: Tmix, Wmix
        real(WP), dimension(nspec) :: Ys

        ! Original version
        ! Compute the new density from the equation of state
        do k = this%cfg%kmino_, this%cfg%kmaxo_
            do j = this%cfg%jmino_, this%cfg%jmaxo_
                do i = this%cfg%imino_, this%cfg%imaxo_
                    if (this%mask(i, j, k) .ne. 0) then
                        this%rho(i, j, k) = 1.0_WP
                    else
                        Tmix = min(max(this%SC(i, j, k, nspec1), T_min), T_max)
                        Ys = this%SC(i, j, k, 1:nspec)
                        call fcsubs_Wmix(Ys, Wmix)
                        this%rho(i, j, k) = this%Pthermo*Wmix/(Rcst*Tmix)
                    end if
                end do
            end do
        end do

        return
    end subroutine get_density

! --------------------------------------------------------------------------------------------!
! =========================================== !
! Compute viscosity by fitting transport data !
! =========================================== !
    subroutine get_viscosity(this)
        implicit none

        class(finitechem), intent(inout) :: this

        integer  :: i, j, k, sc1, sc2
        real(WP) :: Tmix, buf
        real(WP), dimension(nspec) :: eta
        real(WP), dimension(nspec, nspec) :: phi

        ! Compute the new viscosity from Wilke's method
        do k = this%cfg%kmino_, this%cfg%kmaxo_
            do j = this%cfg%jmino_, this%cfg%jmaxo_
                do i = this%cfg%imino_, this%cfg%imaxo_
                    if (this%mask(i, j, k) .ne. 0) cycle

                    ! Pure compounds viscosity
                    Tmix = min(max(this%SC(i, j, k, nspec1), T_min), T_max)

                    call fcmech_get_viscosity(eta, Tmix)

                    ! Mixing coefficients
                    do sc2 = 1, nspec
                        do sc1 = 1, nspec
                            if (sc1 .eq. sc2) then
                                phi(sc1, sc2) = 1.0_WP
                            else
                                buf = sqrt(eta(sc1)/eta(sc2))*(W_sp(sc2)/W_sp(sc1))**0.25_WP
                                phi(sc1, sc2) = (1.0_WP + buf)**2/sqrt(8.0_WP + 8.0_WP*W_sp(sc1)/W_sp(sc2))
                            end if
                        end do
                    end do

                    ! Mixing rule
                    this%visc(i, j, k) = 0.0_WP
                    do sc1 = 1, nspec
                        if (this%SC(i, j, k, 1 + sc1 - 1) .le. 0.0_WP) cycle
                        buf = sum(this%SC(i, j, k, 1:1 + nspec - 1)*phi(sc1, :)/W_sp)
                        this%visc(i, j, k) = this%visc(i, j, k) + this%SC(i, j, k, 1 + sc1 - 1)*eta(sc1)/(W_sp(sc1)*buf)
                    end do

                end do
            end do
        end do

        return
    end subroutine get_viscosity

! --------------------------------------------------------------------------------------------!
! ============================================= !
! Compute diffusivity by fitting transport data !
! ============================================= !
    subroutine get_diffusivity(this)
        implicit none
        class(finitechem), intent(inout) :: this

        integer  :: i, j, k, n
        real(WP) :: Wmix, Tmix
        real(WP) :: sum1, sum2, sumY, sumDiff
        real(WP), dimension(nspec) :: eta, cond, YoverW, Ys
        real(WP), dimension(nspec, nspec) :: invDij

        ! Compute the new diffusivity
        do k = this%cfg%kmino_, this%cfg%kmaxo_
            do j = this%cfg%jmino_, this%cfg%jmaxo_
                do i = this%cfg%imino_, this%cfg%imaxo_
                    if (this%mask(i, j, k) .ne. 0) cycle

                    ! ---- Thermal diffusivity ---- !
                    ! Mixture molar mass and temperature
                    Ys = this%SC(i, j, k, 1:1 + nspec - 1)
                    call this%get_Wmix(Ys, Wmix)
                    Tmix = min(max(this%SC(i, j, k, nspec1), T_min), T_max)

                    ! Individual compounds viscosity
                    call fcmech_get_viscosity(eta, Tmix)

                    ! Individual compounds viscosity
                    call fcmech_get_conductivity(cond, Tmix, eta)

                    ! Mixture averaged thermal conductivity
                    sum1 = Wmix*sum(Ys/(cond*W_sp))
                    sum2 = Wmix*sum(Ys*cond/W_sp)
                    this%lambda(i, j, k) = 0.5_WP*(sum2 + 1.0_WP/sum1)

                    ! Average Cp based on scalar field
                    call this%get_cpmix(Ys, Tmix, this%cp(i, j, k))

                    ! Thermal diffusivity for enthalpy
                    !  this%diff(i,j,k,isc_ENTH)=this%lambda(i,j,k)/this%cp(i,j,k)

                    ! Thermal diffusivity for temperature
                    this%diff(i, j, k, nspec1) = this%lambda(i, j, k)/this%cp(i, j, k)

                    ! ---- Species diffusivity ---- !
                    ! Inverse of binary diffusion coefficients
                    call fcmech_get_invDij(invDij, Tmix, this%Pthermo)

                    ! Constant terms
                    Ys = this%SC(i, j, k, 1:nspec)
                    YOverW = Ys/W_sp
                    sumY = sum(Ys)

                    ! Compute mixture-average diffusion coefficient for each species
                    do n = 1, nspec

                        ! Denominator
                        sumDiff = sum(YOverW*invDij(n, :))

                        if (sumDiff .gt. 1.0e-15_WP) then
                            ! Diffusion is well defined
                            this%diff(i, j, k, n) = (sumY - Ys(n))/(Wmix*sumDiff); 
                        else
                            ! Diffusion is ill defined
                            sumDiff = sum(invDij(n, :)/W_sp)
                            this%diff(i, j, k, n) = (real(nspec - 1, WP))/(Wmix*sumDiff)
                        end if
                        this%diff(i, j, k, n) = this%rho(i, j, k)*this%diff(i, j, k, n)
                    end do

                end do
            end do
        end do
        return
    end subroutine get_diffusivity

    ! ------------------------------------------------------------------------------------------------ !
    ! ================================================ !
    ! Compute the average Cp in the mixture (J/(kg.K)) !
    ! ================================================ !
    subroutine get_cpmix(this, scalar, Tmix, Cp_mix)
        implicit none
        class(finitechem), intent(inout) :: this

        real(WP), dimension(nspec), intent(in) :: scalar
        real(WP), intent(in) :: Tmix
        real(WP), intent(out) :: Cp_mix

        call fcmech_thermodata(Tmix)
        Cp_mix = sum(scalar*Cpsp)

        return
    end subroutine get_cpmix

! ========================================== !
! Compute average molecular weight (kg/kmol) !
! ========================================== !
    subroutine get_Wmix(this, scalar, Wmix)
        implicit none
        class(finitechem), intent(inout) :: this

        real(WP), dimension(nspec), intent(in) :: scalar
        real(WP), intent(out) :: Wmix
        real(WP), dimension(nspec) :: scalar_clip

        scalar_clip = scalar!min(max(scalar,0.0_WP),1.0_WP)
        Wmix = 1.0_WP/sum(scalar_clip/W_sp)

        return
    end subroutine get_Wmix
end module finitechem_class
