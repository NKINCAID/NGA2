!> Various definitions and tools for running an NGA2 simulation
module simulation
    use precision, only: WP
    use geometry, only: cfg, nx, ny, nz, Lx, Ly, Lz
    use ddadi_class, only: ddadi
    use hypre_str_class, only: hypre_str
    use lowmach_class, only: lowmach
    use multivdscalar_class, only: multivdscalar
    use timetracker_class, only: timetracker
    use ensight_class, only: ensight
    use event_class, only: event
    use monitor_class, only: monitor
    use parallel, only: parallel_time
    implicit none
    private

    !> Single low Mach flow solver and scalar solver and corresponding time tracker
    type(hypre_str), public :: ps
    type(ddadi), public :: vs, ss
    type(lowmach), public :: fs
    type(multivdscalar), public :: sc
    type(timetracker), public :: time

    !> Ensight postprocessing
    type(ensight) :: ens_out
    type(event)   :: ens_evt

    !> Simulation monitor file
    type(monitor) :: mfile, cflfile, consfile

    public :: simulation_init, simulation_run, simulation_final

    !> Private work arrays
    real(WP), dimension(:, :, :), allocatable :: resU, resV, resW, resRHO
    real(WP), dimension(:, :, :, :), allocatable :: resSC, tmp_sc
    real(WP), dimension(:, :, :), allocatable :: resSC2, tmpSC, tmpDIFF
    real(WP), dimension(:, :, :), allocatable :: Ui, Vi, Wi
    real(WP), dimension(:, :, :, :), allocatable :: SR
    real(WP), dimension(:, :, :, :, :), allocatable :: gradU

    !> Equation of state
    real(WP) :: rho0, rho1
    real(WP) :: Z_spot, Z_air
    real(WP) :: radius, shift

    !> Fluid, forcing, and particle parameters
    real(WP) :: visc, meanU, meanV, meanW
    real(WP) :: Urms0, TKE0, EPS0, Re_max
    real(WP) :: TKE, URMS
    real(WP) :: tauinf, G, Gdtau, Gdtaui, dx

    !> For monitoring
    real(WP) :: EPS
    real(WP) :: Re_L, Re_lambda
    real(WP) :: eta, ell
    real(WP) :: dx_eta, ell_Lx, Re_ratio, eps_ratio, tke_ratio, nondtime, t1, t2

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
    function xp_scalar(pg, i, j, k) result(isIn)
        use pgrid_class, only: pgrid
        class(pgrid), intent(in) :: pg
        integer, intent(in) :: i, j, k
        logical :: isIn
        isIn = .false.
        ! if (i .ge. pg%imax) isIn = .true.
        if (i .eq. pg%imax) isIn = .true.

    end function xp_scalar

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
    function yp_scalar(pg, i, j, k) result(isIn)
        use pgrid_class, only: pgrid
        class(pgrid), intent(in) :: pg
        integer, intent(in) :: i, j, k
        logical :: isIn
        isIn = .false.
        if (j .eq. pg%jmax) isIn = .true.
    end function yp_scalar
    !> Define here our equation of state
    subroutine get_rho()
        implicit none
        integer :: i, j, k
        real(WP) :: Z
        ! Calculate density
        do k = sc%cfg%kmino_, sc%cfg%kmaxo_
            do j = sc%cfg%jmino_, sc%cfg%jmaxo_
                do i = sc%cfg%imino_, sc%cfg%imaxo_
                    Z = min(max(sc%SC(i, j, k, 1), 0.0_WP), 1.0_WP)
                    sc%rho(i, j, k) = rho0*rho1/((1.0_WP - Z)*rho1 + Z*rho0)
                end do
            end do
        end do
    end subroutine get_rho

    !> Initialization of problem solver
    subroutine simulation_init
        use param, only: param_read
        implicit none

        ! Read in the EOS info
        call param_read('rho0', rho0)
        call param_read('rho1', rho1)

        ! Read in inlet information
        call param_read('Spot radius', radius)
        call param_read('Spot shift', shift)
        call param_read('Z spot', Z_spot)
        call param_read('Z air', Z_air)

        ! Create a low-Mach flow solver with bconds
        create_velocity_solver: block
            use hypre_str_class, only: pcg_pfmg
            use lowmach_class, only: dirichlet, clipped_neumann, slip
            real(WP) :: visc
            ! Create flow solver
            fs = lowmach(cfg=cfg, name='Variable density low Mach NS')
            ! Assign constant viscosity
            call param_read('Dynamic viscosity', visc); fs%visc = visc

            ! Use slip on the sides with correction
            call fs%add_bcond(name='ym_outflow', type=clipped_neumann, face='y', dir=-1, canCorrect=.true., locator=ym_locator)
            call fs%add_bcond(name='yp_outflow', type=clipped_neumann, face='y', dir=+1, canCorrect=.true., locator=yp_locator)
            ! Outflow on the right
            call fs%add_bcond(name='xm_outflow', type=clipped_neumann, face='x', dir=-1, canCorrect=.true., locator=xm_locator)
            call fs%add_bcond(name='xp_outflow', type=clipped_neumann, face='x', dir=+1, canCorrect=.true., locator=xp_locator)
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
        create_scalar: block
            use multivdscalar_class, only: dirichlet, neumann, quick
            real(WP) :: diffusivity
            ! Create scalar solver
            sc = multivdscalar(cfg=cfg, scheme=quick, nscalar=1, name='MultiScalar')
            sc%SCname(1) = "MixtureFraction"
            ! Outflow on the right
            call sc%add_bcond(name='xm_outflow', type=neumann, locator=xm_scalar, dir='-x')
            call sc%add_bcond(name='xp_outflow', type=neumann, locator=xp_scalar, dir='+x')
            call sc%add_bcond(name='ym_outflow', type=neumann, locator=ym_scalar, dir='-y')
            call sc%add_bcond(name='yp_outflow', type=neumann, locator=yp_scalar, dir='+y')
            ! Assign constant diffusivity
            call param_read('Dynamic diffusivity', diffusivity)
            sc%diff = diffusivity
            ! Configure implicit scalar solver
            ss = ddadi(cfg=cfg, name='Scalar', nst=13)
            ! Setup the solver
            call sc%setup(implicit_solver=ss)
        end block create_scalar

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
            allocate (resSC(sc%cfg%imino_:sc%cfg%imaxo_, sc%cfg%jmino_:sc%cfg%jmaxo_, sc%cfg%kmino_:sc%cfg%kmaxo_, 1))
            allocate (tmp_sc(sc%cfg%imino_:sc%cfg%imaxo_, sc%cfg%jmino_:sc%cfg%jmaxo_, sc%cfg%kmino_:sc%cfg%kmaxo_, 1))
            allocate (resSC2(sc%cfg%imino_:sc%cfg%imaxo_, sc%cfg%jmino_:sc%cfg%jmaxo_, sc%cfg%kmino_:sc%cfg%kmaxo_))
            allocate (tmpSC(sc%cfg%imino_:sc%cfg%imaxo_, sc%cfg%jmino_:sc%cfg%jmaxo_, sc%cfg%kmino_:sc%cfg%kmaxo_))
            allocate (tmpDIFF(sc%cfg%imino_:sc%cfg%imaxo_, sc%cfg%jmino_:sc%cfg%jmaxo_, sc%cfg%kmino_:sc%cfg%kmaxo_))

        end block allocate_work_arrays

        ! Initialize time tracker with 2 subiterations
        initialize_timetracker: block
            time = timetracker(amRoot=fs%cfg%amRoot)
            call param_read('Max timestep size', time%dtmax)
            call param_read('Max cfl number', time%cflmax)
            call param_read('Max iterations', time%nmax)
            time%dt = time%dtmax
            time%itmax = 2
        end block initialize_timetracker

        ! Initialize our mixture fraction field
        initialize_scalar: block
            use vdscalar_class, only: bcond
            integer :: n, i, j, k
            type(bcond), pointer :: mybc
            ! Zero initial field
            sc%SC = 0.0_WP
            ! Apply BCs

            do k = fs%cfg%kmino_, fs%cfg%kmaxo_
                do j = fs%cfg%jmino_, fs%cfg%jmaxo_
                    do i = fs%cfg%imino_, fs%cfg%imaxo_
                        if (sqrt((fs%cfg%xm(i) - shift)**2 + fs%cfg%ym(j)**2 + fs%cfg%zm(k)**2) .le. radius) then
                            sc%SC(i, j, k, :) = Z_spot
                        else
                            sc%SC(i, j, k, :) = Z_air
                        end if
                    end do
                end do
            end do

            ! Compute density
            call get_rho()

        end block initialize_scalar

        ! Initialize our velocity field
        initialize_velocity: block
            use lowmach_class, only: bcond
            use random, only: random_normal
            use mathtools, only: Pi
            integer :: n, i, j, k
            type(bcond), pointer :: mybc
            ! Zero initial field
            fs%U = 0.0_WP; fs%V = 0.0_WP; fs%W = 0.0_WP
            ! Gaussian initial field
            do k = fs%cfg%kmin_, fs%cfg%kmax_
                do j = fs%cfg%jmin_, fs%cfg%jmax_
                    do i = fs%cfg%imin_, fs%cfg%imax_
                        fs%U(i, j, k) = random_normal(m=0.0_WP, sd=0.1_WP)
                        fs%V(i, j, k) = random_normal(m=0.0_WP, sd=0.1_WP)
                        ! fs%W(i,j,k)=random_normal(m=0.0_WP,sd=Urms0)
                    end do
                end do
            end do
            ! Set density from scalar
            fs%rho = sc%rho
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
            integer :: nsc
            ! Create Ensight output from cfg
            ens_out = ensight(cfg=cfg, name='vdjet')
            ! Create event for Ensight output
            ens_evt = event(time=time, name='Ensight output')
            call param_read('Ensight output period', ens_evt%tper)
            ! Add variables to output
            call ens_out%add_scalar('pressure', fs%P)
            call ens_out%add_vector('velocity', Ui, Vi, Wi)
            call ens_out%add_scalar('divergence', fs%div)
            call ens_out%add_scalar('density', sc%rho)
            do nsc = 1, sc%nscalar
                call ens_out%add_scalar(trim(sc%SCname(nsc)), sc%SC(:, :, :, nsc))
            end do
            ! Output to ensight
            if (ens_evt%occurs()) call ens_out%write_data(time%t)
        end block create_ensight

        ! Create a monitor file
        create_monitor: block
            integer :: nsc
            ! Prepare some info about fields
            call fs%get_cfl(time%dt, time%cfl)
            call fs%get_max()
            call sc%get_max()
            call sc%get_int()

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
            ! Create conservation monitor
            consfile = monitor(fs%cfg%amRoot, 'conservation')
            call consfile%add_column(time%n, 'Timestep number')
            call consfile%add_column(time%t, 'Time')
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
            sc%rhoold = sc%rho
            sc%SCold = sc%SC

            ! Remember old velocity and momentum
            fs%rhoold = fs%rho
            fs%Uold = fs%U; fs%rhoUold = fs%rhoU
            fs%Vold = fs%V; fs%rhoVold = fs%rhoV
            fs%Wold = fs%W; fs%rhoWold = fs%rhoW

            ! Apply time-varying Dirichlet conditions
            ! This is where time-dpt Dirichlet would be enforced
            ! Perform sub-iterations
            do while (time%it .le. time%itmax)

                t1 = parallel_time()
                scalar_solver: block
                    use messager, only: die
                    integer :: nsc

                    ! Build mid-time scalar
                    sc%SC = 0.5_WP*(sc%SC + sc%SCold)
                    ! Explicit calculation of drhoSC/dt from scalar equation
                    tmpSC = sc%SC(:, :, :, 1)
                    tmpDIFF = sc%diff(:, :, :, 1)

                    call sc%get_drhoSCdt(resSC2, tmpSC, tmpDIFF, fs%rhoU, fs%rhoV, fs%rhoW)
                    do nsc = 1, sc%nscalar
                        ! ============= SCALAR SOLVER =======================
                        ! Assemble explicit residual
                   resSC(:,:,:,nsc)=time%dt*resSC(:,:,:,nsc)-(2.0_WP*sc%rho*sc%SC(:,:,:,nsc)-(sc%rho+sc%rhoold)*sc%SCold(:,:,:,nsc))
                    end do
                    ! Form implicit residual
                    call sc%solve_implicit(time%dt, resSC, fs%rhoU, fs%rhoV, fs%rhoW)
                    ! Re-apply Dirichlet BCs
                    sc%SC = 2.0_WP*sc%SC - sc%SCold + resSC
                    ! Apply all other boundary conditions on the resulting field
                    call sc%apply_bcond(time%t, time%dt)
                end block scalar_solver
                t2 = parallel_time()

                print *, t2 - t1
                ! ===================================================

                ! ============ UPDATE PROPERTIES ====================
                ! Backup rhoSC
                !resSC=sc%rho*sc%SC
                ! Update density
                call get_rho()
                ! Rescale scalar for conservation
                !sc%SC=resSC/sc%rho
                ! UPDATE THE VISCOSITY
                ! UPDATE THE DIFFUSIVITY
                ! ===================================================

                ! ============ VELOCITY SOLVER ======================

                ! Build n+1 density
                fs%rho = 0.5_WP*(sc%rho + sc%rhoold)

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
                call fs%apply_bcond(time%tmid, time%dtmid)
                call fs%rho_multiply()
                call fs%apply_bcond(time%tmid, time%dtmid)

                ! Solve Poisson equation
                call sc%get_drhodt(dt=time%dt, drhodt=resRHO)
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
            call sc%get_drhodt(dt=time%dt, drhodt=resRHO)
            call fs%get_div(drhodt=resRHO)

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
        deallocate (resSC, resU, resV, resW, Ui, Vi, Wi)

    end subroutine simulation_final

end module simulation
