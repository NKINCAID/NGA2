!> Various definitions and tools for running an NGA2 simulation
module simulation
    use precision, only: WP
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

    public :: simulation_init, simulation_run, simulation_final

    !> Private work arrays
    real(WP), dimension(:, :, :), allocatable :: resU, resV, resW, resRHO
    real(WP), dimension(:, :, :), allocatable :: Ui, Vi, Wi
    real(WP), dimension(:, :, :, :), allocatable :: SR
    real(WP), dimension(:, :, :, :, :), allocatable :: gradU
    real(WP), dimension(:, :, :), allocatable :: tmp_sc
    real(WP), dimension(:, :, :, :), allocatable :: resSC

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
    integer :: isc_fuel, isc_o2, isc_n2, isc_T
    integer :: imin, imax, jmin, jmax, kmin, kmax, nx, ny, nz

    real(WP) :: t1, t2, t3, t4, t5, t6, t7
    real(WP) :: int_RP = 0.0_WP

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

    !> Initialization of problem solver
    subroutine simulation_init
        use param, only: param_read
        implicit none

        ! Create a low-Mach flow solver with bconds
        create_velocity_solver: block
            use hypre_str_class, only: pcg_pfmg
            use lowmach_class, only: dirichlet, neumann, clipped_neumann, slip
            real(WP) :: visc
            ! Create flow solver
            fs = lowmach(cfg=cfg, name='Variable density low Mach NS')
            ! Assign constant viscosity
            ! call param_read('Dynamic viscosity', visc); fs%visc = visc
            ! Use slip on the sides with correction
            call fs%add_bcond(name='ym_outflow', type=neumann, face='y', dir=-1, canCorrect=.False., locator=ym_locator)
            call fs%add_bcond(name='yp_outflow', type=neumann, face='y', dir=+1, canCorrect=.False., locator=yp_locator)
            ! Outflow on the right
            call fs%add_bcond(name='xm_outflow', type=neumann, face='x', dir=-1, canCorrect=.False., locator=xm_locator)
            call fs%add_bcond(name='xp_outflow', type=neumann, face='x', dir=+1, canCorrect=.False., locator=xp_locator)
            ! ! Configure pressure solver
            ps = hypre_str(cfg=cfg, name='Pressure', method=pcg_pfmg, nst=7)
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
            use multivdscalar_class, only: dirichlet, neumann, quick
            real(WP) :: diffusivity
            ! Create scalar solver
            fc = finitechem(cfg=cfg, scheme=quick, name='fc')

            ! Assign constant diffusivity
            ! call param_read('Dynamic diffusivity', diffusivity)
            ! fc%diff = diffusivity
            ! Configure implicit scalar solver
            ss = ddadi(cfg=cfg, name='Scalar', nst=13)
            ! Setup the solver
            call fc%setup(implicit_solver=ss)

            print *, sCO2, fc%SCname(sCO2)
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
            allocate (gradU(1:3, 1:3, cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))
            ! Scalar solver
            allocate (resSC(fc%cfg%imino_:fc%cfg%imaxo_, fc%cfg%jmino_:fc%cfg%jmaxo_, fc%cfg%kmino_:fc%cfg%kmaxo_, fc%nscalar))
            ! Temporary scalar field for initialization
            allocate (tmp_sc(fc%cfg%imin:fc%cfg%imax, fc%cfg%jmin:fc%cfg%jmax, fc%cfg%kmin:fc%cfg%kmax))
        end block allocate_work_arrays

        ! Initialize time tracker with 2 subiterations
        initialize_timetracker: block
            time = timetracker(amRoot=fs%cfg%amRoot)
            call param_read('Max timestep size', time%dtmax)
            call param_read('Max cfl number', time%cflmax)
            call param_read('Max time', time%tmax)
            call param_read('Max iterations', time%nmax)

            time%dt = time%dtmax
            time%itmax = 5
        end block initialize_timetracker

        ! Initialize our mixture fraction field
        initialize_fc: block
            use multivdscalar_class, only: bcond
            use parallel, only: MPI_REAL_WP
            integer :: n, i, j, k, ierr
            character(len=str_medium) :: fuel, oxidizer
            real(WP) :: moles_fuel
            real(WP) :: T_init
            type(bcond), pointer :: mybc

            call param_read('Stoich moles fuel', moles_fuel)
            call param_read('Fuel', fuel)
            call param_read('Oxidizer', oxidizer)
            call param_read('T init', T_init)

            call param_read('Pressure', fc%Pthermo)

            ! Read in the buffer region length
            call param_read('Buffer region length', L_buffer)

            do i = 1, nspec
                if (fc%SCname(i) .eq. fuel) then
                    isc_fuel = i
                    print *, "Fuel: ", trim(fc%SCname(i)), isc_fuel
                elseif (fc%SCname(i) .eq. 'O2') then
                    isc_o2 = i
                elseif (fc%SCname(i) .eq. 'N2') then
                    isc_n2 = i
                end if
            end do

            isc_T = nspec + 1
            tmp_sc = 1.0_WP
            do k = fc%cfg%kmino_, fc%cfg%kmaxo_
                do j = fc%cfg%jmino_, fc%cfg%jmaxo_
                    do i = fc%cfg%imino_, fc%cfg%imaxo_
                        ! Set mass fraction of fuel
                        fc%SC(i, j, k, isc_fuel) = (W_sp(isc_fuel)*tmp_sc(i, j, 1)*moles_fuel)/ &
                                                   (W_sp(isc_o2) + 3.76_WP*W_sp(isc_n2) + &
                                                    (W_sp(isc_fuel)*tmp_sc(i, j, 1)*moles_fuel))
                        ! Set mass fraction of O2
                        fc%SC(i, j, k, isc_o2) = W_sp(isc_o2)/ &
                                                 (W_sp(isc_o2) + 3.76_WP*W_sp(isc_n2) + &
                                                  (W_sp(isc_fuel)*tmp_sc(i, j, 1)*moles_fuel))
                        ! Set mass fraction of N2
                        fc%SC(i, j, k, isc_n2) = 3.76_WP*W_sp(isc_n2)/ &
                                                 (W_sp(isc_o2) + 3.76_WP*W_sp(isc_n2) + &
                                                  (W_sp(isc_fuel)*tmp_sc(i, j, 1)*moles_fuel))

                        fc%SC(i, j, k, isc_T) = T_init
                    end do
                end do
            end do
            call fc%get_density()
            call fc%get_viscosity()
            call fc%get_diffusivity()
            ! print *, maxval(fc%visc), minval(fc%visc)

            call fc%get_max()
            print *, fc%visc_min, fc%visc_max
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
            ens_out = ensight(cfg=cfg, name='vdjet')
            ! Create event for Ensight output
            ens_evt = event(time=time, name='Ensight output')
            call param_read('Ensight output period', ens_evt%tper)
            ! Add variables to output
            call ens_out%add_scalar('pressure', fs%P)
            call ens_out%add_vector('velocity', Ui, Vi, Wi)
            call ens_out%add_scalar('divergence', fs%div)
            call ens_out%add_scalar('density', fc%rho)
            call ens_out%add_scalar('viscosity', fc%visc)
            call ens_out%add_scalar('thermal_diff', fc%diff(:, :, :, isc_T))

            call ens_out%add_scalar('YNC12H26', fc%SC(:, :, :, isc_fuel))
            call ens_out%add_scalar('YOH', fc%SC(:, :, :, 5))
            call ens_out%add_scalar('YO2', fc%SC(:, :, :, isc_o2))
            call ens_out%add_scalar('YN2', fc%SC(:, :, :, isc_n2))
            call ens_out%add_scalar('T', fc%SC(:, :, :, isc_T))
            call ens_out%add_scalar('YHO2', fc%SC(:, :, :, sHO2))
            call ens_out%add_scalar('YSXC12H25', fc%SC(:, :, :, sSXC12H25))

            call ens_out%add_scalar('SRC_YNC12H26', fc%SRCchem(:, :, :, isc_fuel))
            call ens_out%add_scalar('SRC_YO2', fc%SRCchem(:, :, :, isc_o2))
            call ens_out%add_scalar('SRC_YHO2', fc%SRCchem(:, :, :, sHO2))
            call ens_out%add_scalar('SRC_YSXC12H25', fc%SRCchem(:, :, :, sSXC12H25))
            call ens_out%add_scalar('SRC_T', fc%SRCchem(:, :, :, isc_T))

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
            call mfile%add_column(int_RP, 'Int(RP)')

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
            fs%rhoold = fc%rho
            fs%Uold = fs%U; fs%rhoUold = fs%rhoU
            fs%Vold = fs%V; fs%rhoVold = fs%rhoV
            fs%Wold = fs%W; fs%rhoWold = fs%rhoW

            ! Apply time-varying Dirichlet conditions
            ! This is where time-dpt Dirichlet would be enforced
            ! t1 = parallel_time()
            call fc%react(time%dt)
            ! t2 = parallel_time()
            ! Perform sub-iterations
            do while (time%it .le. time%itmax)

                ! t3 = parallel_time()
                ! ===================================================
                scalar_solver: block
                    use messager, only: die
                    integer :: nsc
                    ! Build mid-time scalar
                    fc%SC = 0.5_WP*(fc%SC + fc%SCold)
                    ! Explicit calculation of drhoSC/dt from scalar equation
                    call fc%get_drhoSCdt(resSC, fs%rhoU, fs%rhoV, fs%rhoW)
                    do nsc = 1, fc%nscalar
                        ! ============= SCALAR SOLVER =======================
                        ! Assemble explicit residual
                        !    resSC(:,:,:,nsc)=time%dt*resSC(:,:,:,nsc)-2.0_WP*fc%rho*fc%SC(:,:,:,nsc) + (fc%rho+fc%rhoold)*fc%SCold(:,:,:,nsc) + fc%rho*fc%SRCchem(:,:,:,nsc)
                        resSC(:, :, :, nsc) = fc%rho*fc%SRCchem(:, :, :, nsc)

                    end do
                    ! resSC = -2.0_WP*(fc%SC - fc%SCold) + time%dt*resSC

                    !    resSC(:,:,:,nsc)=time%dt*resSC(:,:,:,nsc)-2.0_WP*fc%rho*fc%SC(:,:,:,nsc) + (fc%rho+fc%rhoold)*fc%SCold(:,:,:,nsc) + fc%rho * fc%SRCchem(:,:,:,nsc)
                    ! Form implicit residual
                    call fc%solve_implicit(time%dt, resSC, fs%rhoU, fs%rhoV, fs%rhoW)
                    ! Re-apply Dirichlet BCs
                    fc%SC = 2.0_WP*fc%SC - fc%SCold + resSC
                    ! Apply all other boundary conditions on the resulting field
                    call fc%apply_bcond(time%t, time%dt)
                end block scalar_solver

                test_fc: block
                    use messager, only: die
                    character(len=*), parameter :: FM1 = '(A10,A20, A20, A20, A20, A20)'
                    character(len=*), parameter :: FM3 = '(A10,ES20.7,ES20.7,ES20.7,ES20.7,ES20.7)'
                    character(len=*), parameter :: FM4 = '(A10,ES20.7,ES20.7,ES20.7)'

                    real(WP), dimension(nspec + 1) :: sol, solold, delta, newsol, newdelta

                    solold = fc%SCold(16, 16, 1, :)
                    delta = fc%SRCchem(16, 16, 1, :)

                    sol = solold + delta
                    newsol = fc%SC(16, 16, 1, :)
                    newdelta = fc%SC(16, 16, 1, :) - fc%SCold(16, 16, 1, :)
                    print *, time%n, time%t
                    !                 write (unit=*, fmt=FM1) " ", "Old", "New", "Delta", "SC Solve", "SC Solve Delta"
                    !      write (unit=*, fmt=FM3) "Temp", solold(nspec + 1), sol(nspec + 1), delta(nspec + 1), newsol(nspec + 1), newdelta(nspec + 1)
                    !                 write (unit=*, fmt=FM3) "O2", solold(sO2), sol(sO2), delta(sO2), newsol(sO2), newdelta(SO2)
                    !                 write (unit=*, fmt=FM3) "HO2", solold(sHO2), sol(sHO2), delta(sHO2), newsol(sHO2), newdelta(sHO2)
                    !  write (unit=*, fmt=FM3) "NXC12H26", solold(sNXC12H26), sol(sNXC12H26), delta(sNXC12H26), newsol(sNXC12H26), newdelta(sNXC12H26)
                    !  write (unit=*, fmt=FM3) "SXC12H25", solold(sSXC12H25), sol(sSXC12H25), delta(sSXC12H25), newsol(sSXC12H25), newdelta(sSXC12H25)
                    write (unit=*, fmt=FM1) " ", "resSC ", "New", "Delta", "SC Solve", "SC Solve Delta"
         write (unit=*, fmt=FM3) "Temp", solold(nspec + 1), sol(nspec + 1), delta(nspec + 1), newsol(nspec + 1), newdelta(nspec + 1)
                    write (unit=*, fmt=FM3) "O2", solold(sO2), sol(sO2), delta(sO2), newsol(sO2), newdelta(SO2)
                    write (unit=*, fmt=FM3) "HO2", solold(sHO2), sol(sHO2), delta(sHO2), newsol(sHO2), newdelta(sHO2)
     write (unit=*, fmt=FM3) "NXC12H26", solold(sNXC12H26), sol(sNXC12H26), delta(sNXC12H26), newsol(sNXC12H26), newdelta(sNXC12H26)
     write (unit=*, fmt=FM3) "SXC12H25", solold(sSXC12H25), sol(sSXC12H25), delta(sSXC12H25), newsol(sSXC12H25), newdelta(sSXC12H25)
                    print *, " "
                    write (unit=*, fmt=FM1) " ", "rho ", "rho old", "delta rho", " ", " "
                write (unit=*, fmt=FM4) "Density", fc%rho(16, 16, 1), fc%rhoold(16, 16, 1), fc%rho(16, 16, 1) - fc%rhoold(16, 16, 1)
                    write (unit=*, fmt=FM4) "Viscosity", fc%visc(16, 16, 1)
                    print *, "------------------------------------------------------------------------------"

                    ! call die("Merp")
                end block test_fc

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

                ! Form implicit residuals
                call fs%solve_implicit(time%dtmid, resU, resV, resW)

                ! Apply these residuals
                fs%U = 2.0_WP*fs%U - fs%Uold + resU
                fs%V = 2.0_WP*fs%V - fs%Vold + resV
                fs%W = 2.0_WP*fs%W - fs%Wold + resW

                ! Apply other boundary conditions and update momentum
                call fs%rho_multiply()
                call fs%apply_bcond(time%tmid, time%dtmid)

                ! Solve Poisson equation
                call fc%get_drhodt(dt=time%dt, drhodt=resRHO)
                call fs%correct_mfr(drhodt=resRHO)
                call fs%get_div(drhodt=resRHO)
                fs%psolv%rhs = -fs%cfg%vol*fs%div/time%dtmid
                call cfg%integrate(A=fs%psolv%rhs, integral=int_RP)
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
            call fc%get_drhodt(dt=time%dt, drhodt=resRHO)
            call fs%get_div(drhodt=resRHO)

            ! Output to ensight
            if (ens_evt%occurs()) call ens_out%write_data(time%t)

            ! Perform and output monitoring
            call fs%get_max()
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
