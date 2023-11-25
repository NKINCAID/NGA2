!> Various definitions and tools for running an NGA2 simulation
module simulation
   use precision, only: WP
   use geometry, only: cfg, jet_diam, pilot_diam
   use ddadi_class, only: ddadi
   use hypre_str_class, only: hypre_str
   use lowmach_class, only: lowmach
   use sgsmodel_class, only: sgsmodel
   use vdscalar_class, only: vdscalar
   use pdf_class, only: pdf
   use partmesh_class, only: partmesh
   use timetracker_class, only: timetracker
   use ensight_class, only: ensight
   use event_class, only: event
   use monitor_class, only: monitor
   use pdf_mech
   implicit none
   private

   !> Single low Mach flow solver and scalar solver and corresponding time tracker
   type(lowmach), public :: fs
   type(vdscalar), public :: sc
   type(pdf), public :: pd
   type(sgsmodel), public :: sgs
   type(timetracker), public :: time

   type(hypre_str), public :: ps
   type(ddadi), public :: vs, ss

   !> Ensight postprocessing
   type(partmesh) :: pmesh
   type(ensight) :: ens_out
   type(event)   :: ens_evt

   !> Simulation monitor file
   type(monitor) :: mfile, cflfile, consfile, pdffile

   public :: simulation_init, simulation_run, simulation_final

   !> Private work arrays
   real(WP), dimension(:, :, :, :, :), allocatable :: gradU
   real(WP), dimension(:, :, :), allocatable :: resU, resV, resW, resSC, srcSC
   real(WP), dimension(:, :, :), allocatable :: Ui, Vi, Wi, dRHOdt, dRHO
   real(WP), dimension(:, :, :, :), allocatable :: SR

   real(WP), dimension(nspec) :: Yin_jet, Yin_co, Yin_pi
   real(WP) :: Win_jet, Tin_jet, Hin_jet, svin_jet
   real(WP) :: Win_co, Tin_co, Hin_co, svin_co
   real(WP) :: Win_pi, Tin_pi, Hin_pi, svin_pi

   real(WP) :: tauSV

   ! Backup of viscosity and diffusivity
   real(WP), dimension(:, :, :), allocatable :: viscmol, diffmol
   real(WP), dimension(:, :, :), allocatable :: diff

   real(WP), parameter:: sigma1 = 1.8e-3_WP
   real(WP), parameter:: sigma2 = 1.4e-3_WP
   real(WP), parameter:: mu = 1.7e-3_WP
   real(WP), parameter :: t_factor = 0.20_WP
   real(WP) :: Ujet, Ucoflow, Upilot

   integer :: counter

   integer :: nfilter

contains
   subroutine get_rho()
      implicit none

      integer :: i, j, k
      real(WP) :: one_over_T
      ! Integrate 1/T
      ! Calculate density
      do k = sc%cfg%kmino_, sc%cfg%kmaxo_
         do j = sc%cfg%jmino_, sc%cfg%jmaxo_
            do i = sc%cfg%imino_, sc%cfg%imaxo_
               if (sc%cfg%VF(i, j, k) .eq. 0.0_WP) then
                  sc%RHO(i, j, k) = 1000.0_WP
                  cycle
               end if
               ! Take data from specific volume scalar field
               sc%RHO(i, j, k) = 1.0_WP/sc%SC(i, j, k)
               ! sc%RHO(i, j, k) = 1.0_WP
            end do
         end do
      end do
      ! print *, one_over_T, minval(sc%rho), maxval(sc%rho)
      ! sc%rho = 1.0_WP
   end subroutine get_rho
   !> Function that localizes the right domain boundary
   function right_boundary(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      isIn = .false.
      if (i .eq. pg%imax + 1) isIn = .true.
   end function right_boundary

   !> Function that localizes the right domain boundary
   function scalar_right_boundary(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      isIn = .false.
      ! if (i .ge. pg%imax) isIn = .true.
      if (i .eq. pg%imax) isIn = .true.

   end function scalar_right_boundary

   !> Function that localizes liquid stream at -x
   function jet_inj(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      real(WP) :: rad
      isIn = .false.
      rad = sqrt(pg%ym(j)**2 + pg%zm(k)**2)
      if (rad .le. jet_diam/2.0_WP .and. i .eq. pg%imin) isIn = .true.
   end function jet_inj

   !> Function that localizes liquid stream at -x
   function pilot_inj(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      real(WP) :: rad
      isIn = .false.
      rad = sqrt(pg%ym(j)**2 + pg%zm(k)**2)
      if (rad .gt. jet_diam/2.0_WP .and. rad .le. Pilot_diam/2.0_WP .and. i .eq. pg%imin) isIn = .true.
   end function pilot_inj

   !> Function that localizes gas stream at -x
   function coflow_inj(pg, i, j, k) result(isIn)
      use pgrid_class, only: pgrid
      class(pgrid), intent(in) :: pg
      integer, intent(in) :: i, j, k
      logical :: isIn
      real(WP) :: rad
      isIn = .false.
      rad = sqrt(pg%ym(j)**2 + pg%zm(k)**2)
      if (rad .gt. pilot_diam/2.0_WP .and. i .eq. pg%imin) isIn = .true.
   end function coflow_inj

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

   !> Initialization of problem solver
   subroutine simulation_init
      use param, only: param_read, param_exists
      use messager, only: die
      implicit none
      print *, counter; counter = counter + 1; 
      ! Create an incompressible flow solver with bconds
      create_solver: block
         use hypre_str_class, only: pcg_pfmg, smg
         use lowmach_class, only: dirichlet, clipped_neumann, slip, neumann
         real(WP) :: visc
         print *, counter; counter = counter + 1; 
         ! Create flow solver
         fs = lowmach(cfg=cfg, name='Variable density low Mach NS')
         print *, counter; counter = counter + 1; 
         ! Assign constant viscosity
         call param_read('Dynamic viscosity', visc); fs%visc = visc
         print *, counter; counter = counter + 1; 
         ! Define direction gas/liquid stream boundary conditions
         call fs%add_bcond(name='coflow_inj', type=dirichlet, face='x', dir=-1, canCorrect=.false., locator=coflow_inj)
         call fs%add_bcond(name='jet_inj', type=dirichlet, face='x', dir=-1, canCorrect=.false., locator=jet_inj)
         call fs%add_bcond(name='pilot_inj', type=dirichlet, face='x', dir=-1, canCorrect=.false., locator=pilot_inj)
         print *, counter; counter = counter + 1; 
         ! Outflow on the right
         call fs%add_bcond(name='outflow', type=clipped_neumann, face='x', dir=+1, canCorrect=.true., locator=right_boundary)
         print *, counter; counter = counter + 1; 
         call fs%add_bcond(name='ym_outflow', type=clipped_neumann, face='y', dir=-1, canCorrect=.False., locator=ym_locator)
         call fs%add_bcond(name='yp_outflow', type=clipped_neumann, face='y', dir=+1, canCorrect=.False., locator=yp_locator)
         print *, counter; counter = counter + 1; 
         ! Configure pressure solver
         ps = hypre_str(cfg=cfg, name='Pressure', method=smg, nst=7)
         ps%maxlevel = 18
         call param_read('Pressure iteration', ps%maxit)
         call param_read('Pressure tolerance', ps%rcvg)
         ! Configure implicit velocity solver
         vs = ddadi(cfg=cfg, name='Velocity', nst=7)
         ! Setup the solver
         call fs%setup(pressure_solver=ps, implicit_solver=vs)
      end block create_solver
      print *, counter; counter = counter + 1; 
      ! Create a scalar solver
      create_scalar: block
         use vdscalar_class, only: dirichlet, neumann, quick
         real(WP) :: diffusivity
         ! Create scalar solver
         sc = vdscalar(cfg=cfg, scheme=quick, name='Temperature')
         ! Assign constant diffusivity
         call param_read('Dynamic diffusivity', diffusivity)
         sc%diff = diffusivity

         ! call sc%add_bcond(name='coflow_inj', type=dirichlet, locator=coflow_inj)
         ! call sc%add_bcond(name='jet_inj', type=dirichlet, locator=jet_inj)
         ! call sc%add_bcond(name='pilot_inj', type=dirichlet, locator=pilot_inj)
         call sc%add_bcond(name='sc_outflow', type=neumann, locator=scalar_right_boundary, dir='+x')

         ss = ddadi(cfg=cfg, name='Scalar', nst=13)

         ! Setup the solver
         call sc%setup(implicit_solver=ss)
      end block create_scalar
      print *, counter; counter = counter + 1; 
      ! Allocate work arrays
      allocate_work_arrays: block
         ! Flow solver
         allocate (resU(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resV(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (resW(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Ui(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Vi(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (Wi(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (SR(6, cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))

         allocate (resSC(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))
         allocate (srcSC(fs%cfg%imino_:fs%cfg%imaxo_, fs%cfg%jmino_:fs%cfg%jmaxo_, fs%cfg%kmino_:fs%cfg%kmaxo_))

         allocate (dRHOdt(cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))
         allocate (dRHO(cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))
         allocate (diff(cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))

         allocate (gradU(1:3, 1:3, cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))

      end block allocate_work_arrays
      print *, counter; counter = counter + 1; 
      ! Initialize time tracker with 2 subiterations
      initialize_timetracker: block
         time = timetracker(amRoot=fs%cfg%amRoot)
         call param_read('Max timestep size', time%dtmax)
         call param_read('Max cfl number', time%cflmax)
         call param_read('Max time', time%tmax)
         call param_read('Sub-iterations', time%itmax)
         time%dt = time%dtmax
      end block initialize_timetracker

      ! Initialize our pdf
      initialize_pdf: block
         use random, only: random_uniform
         real(WP):: rho0, rho_use
         integer :: i, j, k, ii, jj, kk, ierr, iunit, n, np, nppc, ip
         real(WP), dimension(nspec) :: myhsp, myCpsp
         real(WP) :: sum_y
         real(WP) :: rand, Vp
         real(WP) :: Pinit
         real(WP) :: Winit, Tinit, svinit, Hinit, Zinit, s

         real(WP), dimension(:), allocatable :: Yinit, Yinflow
         character(len=str_medium), dimension(:), allocatable :: spname

         logical :: isdefined
         print *, counter; counter = counter + 1; 
         ! Create solver
         pd = pdf(cfg=cfg, name='PDF')

         call param_read("Number of particles per cell", nppc)

         call param_read("Pressure", pd%Pthermo)

         call param_read("Use reactions", pd%use_reactions)
         call param_read("Use explicit try", pd%use_explicit_try)

         ! ------------------- Chemistry variables ------------------- !
         ! Allocate space
         allocate (spname(nspec))
         allocate (Yinit(nspec))

         ! Get species names and molecular masses
         call pdf_mech_get_speciesnames(spname)

         call param_read('Initial temperature', Tinit)
         ! Get particles initial composition
         Yinit = 0.0_WP
         do i = 1, nspec
            ! Check if species is defined
            if (param_exists('Initial gas '//trim(spname(i)))) then
               call param_read('Initial gas '//trim(spname(i)), Yinit(i))
               print *, 'species ', trim(spname(i)), Yinit(i)
            end if
         end do

         Yinit = Yinit/sum(Yinit)

         ! Get initial molecular weight
         Winit = 0.0_WP
         do i = 1, nspec
            Winit = Winit + Yinit(i)/W_sp(i)
         end do
         Winit = 1/Winit

         print *, 'Initial W mixture : ', Winit
         print *, 'Initial temperature : ', Tinit

         np = pd%cfg%nx*pd%cfg%ny*pd%cfg%nz*nppc
         ! Root process initializes 1000 particles randomly
         ip = 0
         if (pd%cfg%amRoot) then
            call pd%resize(np)
            do i = 1, pd%cfg%nx
               do j = 1, pd%cfg%ny
                  do k = 1, pd%cfg%nz
                     Vp = pd%cfg%dx(i)*pd%cfg%dy(j)*pd%cfg%dz(k)/real(nppc, WP)
                     do n = 1, nppc

                        ip = ip + 1
                        ! Assign random position in the domain
                        if (fs%cfg%nz .eq. 1) then
                           pd%p(ip)%pos = [random_uniform(pd%cfg%x(i), pd%cfg%x(i + 1)),&
                           &            random_uniform(pd%cfg%y(j), pd%cfg%y(j + 1)),&
                           &            0.0_WP]
                        else
                           pd%p(ip)%pos = [random_uniform(pd%cfg%x(i), pd%cfg%x(i + 1)),&
                           &            random_uniform(pd%cfg%y(j), pd%cfg%y(j + 1)),&
                           &            random_uniform(pd%cfg%z(k), pd%cfg%z(k + 1))]
                        end if

                        ! Locate the particle on the mesh
                        pd%p(ip)%ind = pd%cfg%get_ijk_global(pd%p(ip)%pos, [pd%cfg%imin, pd%cfg%jmin, pd%cfg%kmin])

                        pd%p(ip)%vol = Vp
                        ! Stratified initial field
                        pd%p(ip)%T = Tinit

                        ! Initialize enthalpy
                        call pdf_mech_get_thermodata(myhsp, myCpsp, pd%p(ip)%T)
                        Hinit = sum(Yinit(:)*myhsp)

                        pd%p(ip)%h = Hinit

                        ! Get initial particle specific volume
                        svinit = Rcst*pd%p(ip)%T/(pd%Pthermo*Winit)
                        pd%p(ip)%m = pd%p(ip)%vol/svinit

                        pd%p(ip)%Zmix = 0.0_WP
                        pd%p(ip)%Yi = Yinit
                        pd%p(ip)%multi = 1

                     end do
                  end do
               end do
            end do
         end if
         ! Distribute particles
         call pd%sync()

         call pd%control_init(nppc=nppc)

         call pd%get_eulerian_fields()

         call pd%get_density(rho=fs%rho)

         call pd%get_viscosity(visc=fs%visc)

         call pd%get_diffusivity(diff=diff)

      end block initialize_pdf

      initialize_pdf_injection: block
         character(len=str_medium), dimension(:), allocatable :: spname
         integer :: i
         real(WP), dimension(nspec) :: myhsp, myCpsp

         allocate (spname(nspec))

         ! Get species names and molecular masses
         call pdf_mech_get_speciesnames(spname)

         ! Get particles initial composition
         Yin_jet = 0.0_WP
         do i = 1, nspec
            ! Check if species is defined
            if (param_exists('Jet gas '//trim(spname(i)))) then
               call param_read('Jet gas '//trim(spname(i)), Yin_jet(i))
               print *, 'species ', trim(spname(i)), Yin_jet(i)
            end if
         end do

         Yin_jet = Yin_jet/sum(Yin_jet)

         ! Get initial molecular weight
         Win_jet = 0.0_WP
         do i = 1, nspec
            Win_jet = Win_jet + Yin_jet(i)/W_sp(i)
         end do
         Win_jet = 1/Win_jet

         call param_read('Jet temperature', Tin_jet)
         call pdf_mech_get_thermodata(myhsp, myCpsp, Tin_jet)
         Hin_jet = sum(Yin_jet(:)*myhsp)

         svin_jet = Rcst*Tin_jet/(pd%Pthermo*Win_jet)

         if (fs%cfg%amRoot) then
            print *, 'Jet W mixture : ', Win_jet
            print *, 'Jet temperature : ', Tin_jet
            print *, 'Jet enthalpy : ', Hin_jet
         end if
         ! Get particles initial composition
         Yin_co = 0.0_WP
         do i = 1, nspec
            ! Check if species is defined
            if (param_exists('Coflow gas '//trim(spname(i)))) then
               call param_read('Coflow gas '//trim(spname(i)), Yin_co(i))
               print *, 'species ', trim(spname(i)), Yin_co(i)
            end if
         end do

         Yin_co = Yin_co/sum(Yin_co)

         ! Get initial molecular weight
         Win_co = 0.0_WP
         do i = 1, nspec
            Win_co = Win_co + Yin_co(i)/W_sp(i)
         end do
         Win_co = 1/Win_co

         call param_read('Coflow temperature', Tin_co)
         call pdf_mech_get_thermodata(myhsp, myCpsp, Tin_co)
         Hin_co = sum(Yin_co(:)*myhsp)
         svin_co = Rcst*Tin_co/(pd%Pthermo*Win_co)

         if (fs%cfg%amRoot) then
            print *, 'Coflow W mixture : ', Win_co
            print *, 'Coflow temperature : ', Tin_co
            print *, 'Coflow enthalpy : ', Hin_co
         end if
         ! Get particles initial composition
         Yin_pi = 0.0_WP
         do i = 1, nspec
            ! Check if species is defined
            if (param_exists('Pilot gas '//trim(spname(i)))) then
               call param_read('Pilot gas '//trim(spname(i)), Yin_pi(i))
               print *, 'species ', trim(spname(i)), Yin_pi(i)
            end if
         end do

         Yin_pi = Yin_pi/sum(Yin_pi)

         ! Get initial molecular weight
         Win_pi = 0.0_WP
         do i = 1, nspec
            Win_pi = Win_pi + Yin_pi(i)/W_sp(i)
         end do
         Win_pi = 1/Win_pi

         call param_read('Pilot temperature', Tin_pi)
         call pdf_mech_get_thermodata(myhsp, myCpsp, Tin_pi)
         Hin_pi = sum(Yin_pi(:)*myhsp)

         svin_pi = Rcst*Tin_pi/(pd%Pthermo*Win_pi)

         if (fs%cfg%amRoot) then
            print *, 'Pilot W mixture : ', Win_pi
            print *, 'Pilot temperature : ', Tin_pi
            print *, 'Pilot enthalpy : ', Hin_pi
         end if
      end block initialize_pdf_injection

      ! Initialize our scalar field
      initialize_scalar: block
         use vdscalar_class, only: bcond
         type(bcond), pointer :: mybc
         integer :: i, j, k, n

         call param_read('SV relaxation tau', tauSV)

         sc%SC = pd%SV

         ! call sc%get_bcond('jet_inj', mybc)
         ! do n = 1, mybc%itr%no_
         !   i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
         !   sc%SC(i, j, k) = svin_jet
         ! end do
         ! call sc%get_bcond('pilot_inj', mybc)
         ! do n = 1, mybc%itr%no_
         !   i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
         !   sc%SC(i, j, k) = svin_pi
         ! end do
         ! call sc%get_bcond('coflow_inj', mybc)
         ! do n = 1, mybc%itr%no_
         !   i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
         !   sc%SC(i, j, k) = svin_co
         ! end do

         ! Build corresponding density
         call get_rho()
         fs%RHO = sc%RHO
         ! dRHOdt = 0.0_WP
         print *, minval(sc%rho), maxval(sc%rho)
      end block initialize_scalar
      ! Create partmesh object for Lagrangian particle output
      create_pmesh: block
         integer :: i
         pmesh = partmesh(nvar=5, nvec=0, name='pdf')
         pmesh%varname(1) = 'pT'
         pmesh%varname(2) = 'pO2'
         pmesh%varname(3) = 'pNXC12H26'
         pmesh%varname(4) = 'pOH'
         pmesh%varname(5) = 'multi'

         call pd%update_partmesh(pmesh)
         do i = 1, pd%np_
            pmesh%var(1, i) = pd%p(i)%T
            pmesh%var(2, i) = pd%p(i)%Yi(8)
            pmesh%var(3, i) = pd%p(i)%Yi(27)
            pmesh%var(4, i) = pd%p(i)%Yi(5)
            pmesh%var(5, i) = pd%p(i)%multi
         end do
      end block create_pmesh

      ! Initialize our velocity field
      initialize_velocity: block
         use lowmach_class, only: bcond
         integer :: n, i, j, k
         real(WP) :: rad, R
         type(bcond), pointer :: mybc

         call param_read('Coflow velocity', Ucoflow)
         ! Zero initial field
         fs%U = Ucoflow; fs%V = 0.0_WP; fs%W = 0.0_WP
         ! Set density from scalar
         ! fs%rho = fs%rho

         ! Apply Dirichlet for the coflow
         call fs%get_bcond('coflow_inj', mybc)
         do n = 1, mybc%itr%no_
            i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
            fs%U(i, j, k) = Ucoflow
            fs%rhoU(i, j, k) = Ucoflow*fs%rho(i, j, k)
         end do

         ! Apply Dirichlet for the jet
         call param_read('Jet velocity', Ujet)
         call fs%get_bcond('jet_inj', mybc)
         do n = 1, mybc%itr%no_
            i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
            fs%U(i, j, k) = Ujet
            fs%rhoU(i, j, k) = fs%U(i, j, k)*fs%rho(i, j, k)
         end do

         call param_read('Pilot velocity', Upilot)
         call fs%get_bcond('pilot_inj', mybc)
         do n = 1, mybc%itr%no_
            i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
            fs%U(i, j, k) = Upilot
            fs%rhoU(i, j, k) = fs%U(i, j, k)*fs%rho(i, j, k)
         end do

         ! Form momentum
         call fs%rho_multiply()
         ! Apply all other boundary conditions
         call fs%apply_bcond(time%t, time%dt)
         call fs%interp_vel(Ui, Vi, Wi)
         call fs%get_div(drhodt=dRHOdt)
         ! Compute MFR through all boundary conditions
         call fs%get_mfr()
      end block initialize_velocity

      ! Create an LES model
      create_sgs: block
         allocate (viscmol(cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))
         allocate (diffmol(cfg%imino_:cfg%imaxo_, cfg%jmino_:cfg%jmaxo_, cfg%kmino_:cfg%kmaxo_))
         sgs = sgsmodel(cfg=fs%cfg, umask=fs%umask, vmask=fs%vmask, wmask=fs%wmask)
      end block create_sgs

      ! Add Ensight output
      create_ensight: block
         ! Create Ensight output from cfg
         ens_out = ensight(cfg=cfg, name='test')
         ! Create event for Ensight output
         ens_evt = event(time=time, name='Ensight output')
         call param_read('Ensight output period', ens_evt%tper)
         ! Add variables to output
         call ens_out%add_scalar('pressure', fs%P)
         call ens_out%add_vector('velocity', Ui, Vi, Wi)
         call ens_out%add_scalar('divergence', fs%div)
         call ens_out%add_scalar('density', fs%rho)
         call ens_out%add_scalar('temperature', pd%T)
         call ens_out%add_scalar('npincell', pd%npincell)
         call ens_out%add_scalar('gammat', pd%gammat)
         call ens_out%add_scalar('pSV', pd%SV)
         call ens_out%add_scalar('SV', sc%SC)

         ! Add variables to output
         call ens_out%add_particle('particles', pmesh)
         ! Output to ensight
         if (ens_evt%occurs()) call ens_out%write_data(time%t)
      end block create_ensight
      ! print *, "Here"

      ! Create a monitor file
      create_monitor: block
         ! Prepare some info about fields
         call fs%get_cfl(time%dt, time%cfl)
         call fs%get_max()

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

         ! Create PDF monitor
         pdffile = monitor(fs%cfg%amRoot, 'pdf')
         call pdffile%add_column(time%n, 'Timestep number')
         call pdffile%add_column(time%t, 'Time')
         call pdffile%add_column(pd%Tmin, 'Tmin')
         call pdffile%add_column(pd%Tmax, 'Tmax')
         call pdffile%add_column(pd%Tmean, 'Tmean')
         call pdffile%add_column(pd%np_min, 'Np_min')
         call pdffile%add_column(pd%np_max, 'Np_max')
         call pdffile%add_column(pd%np_mean, 'Np_mean')
         call pdffile%add_column(pd%np_total, 'Np_total')

         call pdffile%write()

         call consfile%write()
      end block create_monitor

   end subroutine simulation_init

   !> Perform an NGA2 simulation
   subroutine simulation_run
      use messager, only: die
      implicit none
      integer i, j, k

      ! Perform time integration
      do while (.not. time%done())

         do k = sc%cfg%kmin_, sc%cfg%kmax_
            do j = sc%cfg%jmin_, sc%cfg%jmax_
               do i = sc%cfg%imin_, sc%cfg%imax_
                  if (pd%npincell(i, j, k) .eq. 0) then
                     print *, i, j, k, "Oh no, we don't have any particles..."
                  end if
               end do
            end do
         end do
         ! Increment time
         call fs%get_cfl(time%dt, time%cfl)
         call time%adjust_dt()
         call time%increment()

         ! Remember old scalar
         sc%rhoold = sc%rho
         sc%SCold = sc%SC

         ! Remember old velocity and momentum
         pd%Pthermo_old = pd%Pthermo
         fs%rhoold = fs%rho
         fs%Uold = fs%U; fs%rhoUold = fs%rhoU
         fs%Vold = fs%V; fs%rhoVold = fs%rhoV
         fs%Wold = fs%W; fs%rhoWold = fs%rhoW

         ! ============ UPDATE PROPERTIES ====================

         ! Turbulence modeling
         sgs_modeling: block
            use sgsmodel_class, only: vreman
            resU = fs%rho
            call fs%get_gradu(gradU)
            call sgs%get_visc(type=vreman, dt=time%dtold, rho=resU, gradu=gradU)
         end block sgs_modeling

         ! Get fluid properties from PDF
         call pd%get_viscosity(visc=fs%visc)
         call pd%get_diffusivity(diff=diff)

         diff = diff + sgs%visc
         fs%visc = fs%visc + sgs%visc

         ! Advance particles by dt
         call pd%transport_step(dt=time%dt, U=fs%U, V=fs%V, W=fs%W, rho=fs%rho, visc=fs%visc, diff=diff)

         ! Injection routine
         pdf_injection: block
            use random, only: random_uniform
            use lowmach_class, only: bcond
            use param, only: param_read

            type(bcond), pointer :: mybc

            call fs%get_bcond('jet_inj', mybc)
            call pd%inject(dt=time%dt, itr=mybc%itr, U=fs%U, V=fs%V, W=fs%W, Tin=Tin_jet, Hin=Hin_jet, Yin=Yin_jet, svin=svin_jet)
            call pd%sync()

            call fs%get_bcond('coflow_inj', mybc)
            call pd%inject(dt=time%dt, itr=mybc%itr, U=fs%U, V=fs%V, W=fs%W, Tin=Tin_co, Hin=Hin_co, Yin=Yin_co, svin=svin_co)
            call pd%sync()

            call fs%get_bcond('pilot_inj', mybc)
            call pd%inject(dt=time%dt, itr=mybc%itr, U=fs%U, V=fs%V, W=fs%W, Tin=Tin_co, Hin=Hin_co, Yin=Yin_co, svin=svin_co)
            ! call pd%inject(dt=time%dt, itr=mybc%itr, U=fs%U, V=fs%V, W=fs%W, Tin=Tin_jet, Hin=Hin_jet, Yin=Yin_jet, svin=svin_jet)
            call pd%sync()

         end block pdf_injection

         call pd%control()

         call pd%get_eulerian_fields_mid()

         call pd%mix(dt=time%dt, delta=sgs%delta)

         if (time%t .gt. 2.0e-2) then
            call pd%react(dt=time%dt)
         end if

         call pd%get_eulerian_fields()

         ! Get fluid properties from PDF
         call pd%get_viscosity(visc=fs%visc)
         call pd%get_diffusivity(diff=diff)

         diff = diff + sgs%visc
         fs%visc = fs%visc + sgs%visc

         sc%diff = diff

         filter_pSV: block
            use vdscalar_class, only: bcond
            type(bcond), pointer :: mybc
            integer :: i, j, k, n

            do n = 1, nfilter
               do k = sc%cfg%kmin_, sc%cfg%kmax_
                  do j = sc%cfg%jmin_, sc%cfg%jmax_
                     do i = sc%cfg%imin_, sc%cfg%imax_
                        pd%SV(i, j, k) = sum(sgs%filtern(:, :, :, i, j, k)*pd%SV(i - 1:i + 1, j - 1:j + 1, k - 1:k + 1))
                     end do
                  end do
               end do
            end do
         end block filter_pSV

         ! Apply time-varying Dirichlet conditions
         ! This is where time-dpt Dirichlet would be enforced

         ! Perform sub-iterations
         do while (time%it .le. time%itmax)

            ! ============= SCALAR SOLVER =======================
            ! Build mid-time scalar
            sc%SC = 0.5_WP*(sc%SC + sc%SCold)

            ! Explicit calculation of drhoSC/dt from scalar equation
            call sc%get_drhoSCdt(resSC, fs%rhoU, fs%rhoV, fs%rhoW)

            ! SV relaxation term
            srcSC = 0.5_WP*(sc%RHO + sc%RHOold)*(pd%SV - sc%SC)/(tauSV)

            ! Assemble explicit residual
            resSC = time%dt*resSC + srcSC - (2.0_WP*sc%rho*sc%SC - (sc%rho + sc%rhoold)*sc%SCold)

            ! Form implicit residual
            call sc%solve_implicit(time%dt, resSC, fs%rhoU, fs%rhoV, fs%rhoW)

            ! Apply this residual
            sc%SC = 2.0_WP*sc%SC - sc%SCold + resSC

            ! Apply other boundary conditions on the resulting field
            call sc%apply_bcond(time%t, time%dt)

            ! Backup rhoSC
            call get_rho()

            drho = sc%rho - sc%rhoold

            ! test: block
            !   use vdscalar_class, only: bcond
            !   type(bcond), pointer :: mybc
            !   integer :: i, j, k, n

            !   do n = 1, nfilter
            !     do k = sc%cfg%kmin_, sc%cfg%kmax_
            !       do j = sc%cfg%jmin_, sc%cfg%jmax_
            !         do i = sc%cfg%imin_, sc%cfg%imax_
            !           drho(i, j, k) = sum(sgs%filtern(:, :, :, i, j, k)*drho(i - 1:i + 1, j - 1:j + 1, k - 1:k + 1))
            !         end do
            !       end do
            !     end do
            !   end do

            ! sc%rho = sc%rhoold + drho
            ! end block test

            dRHOdt = (sc%rho - sc%rhoold)/time%dt
            ! ============ VELOCITY SOLVER ======================
            ! Build n+1 density
            fs%rho = 0.5_WP*(sc%rho + sc%rhoold)

            ! Build mid-time velocity and momentum
            fs%U = 0.5_WP*(fs%U + fs%Uold); fs%rhoU = 0.5_WP*(fs%rhoU + fs%rhoUold)
            fs%V = 0.5_WP*(fs%V + fs%Vold); fs%rhoV = 0.5_WP*(fs%rhoV + fs%rhoVold)
            fs%W = 0.5_WP*(fs%W + fs%Wold); fs%rhoW = 0.5_WP*(fs%rhoW + fs%rhoWold)

            ! Explicit calculation of drho*u/dt from NS
            call fs%get_dmomdt(resU, resV, resW)

            ! Add momentum source terms
            call fs%addsrc_gravity(resU, resV, resW)

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

            ! Update time-varying Dirichlet conditions
            update_inlet_conditions: block
               use random, only: random_uniform
               use lowmach_class, only: bcond
               use param, only: param_read

               type(bcond), pointer :: mybc
               real(WP) :: rand, R
               integer :: i, j, k, n

               call fs%get_bcond('jet_inj', mybc)
               do n = 1, mybc%itr%no_
                  i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
                  fs%U(i, j, k) = Ujet
                  fs%rhoU(i, j, k) = fs%U(i, j, k)*fs%rho(i, j, k)

               end do

               call fs%get_bcond('coflow_inj', mybc)
               do n = 1, mybc%itr%no_
                  i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
                  fs%U(i, j, k) = Ucoflow
                  fs%rhoU(i, j, k) = fs%U(i, j, k)*fs%rho(i, j, k)
               end do

               call fs%get_bcond('pilot_inj', mybc)
               do n = 1, mybc%itr%no_
                  i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
                  fs%U(i, j, k) = Upilot
                  fs%rhoU(i, j, k) = fs%U(i, j, k)*fs%rho(i, j, k)
               end do
            end block update_inlet_conditions

            ! Solve Poisson equation
            call fs%correct_mfr(drhodt=dRHOdt)
            call fs%get_div(drhodt=dRHOdt)
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
         call fs%get_div(drhodt=dRHOdt)

         !    test_block: block
         !    use lowmach_class, only: bcond
         !    use param, only: param_read

         !    type(bcond), pointer :: mybc
         !    real(WP) :: rand, R
         !    integer :: i, j, k, n

         !    call fs%get_bcond('coflow_inj', mybc)
         !    do n = 1, mybc%itr%no_
         !       i = mybc%itr%map(1, n); j = mybc%itr%map(2, n); k = mybc%itr%map(3, n)
         !       fs%U(i,j,k) = 1.0_WP
         !       print *, i, j, k, fs%U(i,j,k)
         !    end do
         ! end block test_block

         ! Output to ensight
         if (ens_evt%occurs()) then
            update_pmesh: block
               integer:: i
               ! Transfer particles to pmesh
               call pd%update_partmesh(pmesh)
               ! Also populate diameter variable
               do i = 1, pd%np_
                  pmesh%var(1, i) = pd%p(i)%T
                  pmesh%var(2, i) = pd%p(i)%Yi(8)
                  pmesh%var(3, i) = pd%p(i)%Yi(27)
                  pmesh%var(4, i) = pd%p(i)%Yi(5)
                  pmesh%var(5, i) = pd%p(i)%multi
               end do
            end block update_pmesh
            call ens_out%write_data(time%t)
         end if

         ! Perform and output monitoring
         call fs%get_max()
         call pd%get_max()
         call mfile%write()
         call cflfile%write()
         call consfile%write()
         call pdffile%write()
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
      deallocate (dRHOdt, resU, resV, resW, Ui, Vi, Wi)

   end subroutine simulation_final

end module simulation
