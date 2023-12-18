!> Various definitions and tools for running an NGA2 simulation
module simulation
   use precision,         only: WP
   use geometry,          only: cfg
   use aencodernet_class, only: aencodernet
   use chsourcenet_class, only: chsourcenet
   use timetracker_class, only: timetracker
   use monitor_class,     only: monitor
   use fcmech
   implicit none
   private

   !> Time tracker
   type(timetracker), public :: time

   !> Machine learning interface
   type(aencodernet) :: aen
   type(chsourcenet) :: csn

   !> Simulation monitor files
   type(monitor) :: mfile

   !> Scalars in the neural network representation of the mechanism
   integer :: nZ,nY_sub
   real(WP), dimension(:), allocatable :: Z,Zold
   real(WP), dimension(:), allocatable :: Z_src   !< RHS of the scalar ODEs

   !< Thermochemical quantites
   real(WP), dimension(:), allocatable :: Y       !< Mass fractions
   real(WP), dimension(:), allocatable :: hY      !< Enthalpy and mass fractions of sub species
   real(WP), dimension(:), allocatable :: TYS     !< Temperature, mass fractions, and source terms of sub species
   real(WP) :: T,h                                !< Temperature and enthalpy
   character(len=str_medium), dimension(:), allocatable :: spec_name  

   ! Indices of species
   integer :: isc_dodecane,isc_isocetane,isc_o2,isc_n2


   public :: simulation_init,simulation_run,simulation_final


contains


   !> Initialization of problem solver
   subroutine simulation_init
      use param, only: param_read
      implicit none


      ! Initialize neural networks
      create_networks: block
         use string, only: str_medium
         character(len=str_medium) :: aenfname,csnfname
         ! Read in the data file names
         call param_read('Auto encoder',aenfname)
         call param_read('Chemical source',csnfname)
         ! The auto encoder network object
         aen=aencodernet(cfg=cfg,fdata=aenfname,name='Auto encoder network')
         call aen%print()
         ! The chemical source network object
         csn=chsourcenet(cfg=cfg,fdata=csnfname,name='Chemical source network')
         call csn%print()
         ! Array sizes
         nZ=size(aen%matrices(aen%imat_proj_weight)%matrix,dim=1)
         nY_sub=size(aen%vectors(aen%ivec_spec_inds)%vector)
      end block create_networks


      ! Initialize time tracker
      initialize_timetracker: block
         time=timetracker(amRoot=cfg%amRoot)
         call param_read('Max timestep size',time%dtmax)
         call param_read('Max time',time%tmax)
         time%dt=time%dtmax
         time%itmax=1
      end block initialize_timetracker


      ! Allocate work arrays
      allocate_work_arrays: block
         allocate(Z(nZ))          ; Z    =0.0_WP
         allocate(Zold(nZ))       ; Zold =0.0_WP
         allocate(Z_src(nZ))      ; Z_src=0.0_WP
         allocate(Y(nspec))       ; Y    =0.0_WP
         allocate(hY(nY_sub+1))   ; hY   =0.0_WP
         allocate(TYS(2*nY_sub+1)); TYS  =0.0_WP
         allocate(spec_name(nspec))
      end block allocate_work_arrays


      ! Initialize scalars
      initialize_scalar: block
         integer :: ispec
         ! Get the species names
         call fcmech_get_speciesnames(spec_name)
         ! Get the indices
         do ispec=1,nspec
            if (spec_name(ispec).eq.'XC12H26') then
               isc_dodecane=ispec
            elseif (spec_name(ispec).eq.'HMN') then
               isc_isocetane=ispec
            elseif (spec_name(ispec).eq.'O2') then
               isc_o2=ispec
            elseif (spec_name(ispec).eq.'N2') then
               isc_n2=ispec
            end if
         end do
         ! Initial values
         call param_read('Initial temperature',T)
         call param_read('Initial enthalpy',h)
         call param_read('Initial HMN',Y(isc_isocetane))
         call param_read('Initial XC12H26',Y(isc_dodecane))
         call param_read('Initial N2',Y(isc_n2))
         call param_read('Initial O2',Y(isc_o2))
         ! Map Y and T to the neural network scalars
         call aen%transform_inputs([h,Y(int(aen%vectors(aen%ivec_spec_inds)%vector))],hY)
         call aen%encode(hY,Z)
      end block initialize_scalar


      ! Create a monitor files
      create_monitor: block
         mfile=monitor(cfg%amRoot,'simulation')
         call mfile%add_column(time%t,'Time')
         call mfile%add_column(Y(isc_dodecane),'Dodecane mass fraction')
         call mfile%add_column(Y(isc_isocetane),'Iso-cetane mass fraction')
         call mfile%add_column(Y(isc_o2),'O2 mass fraction')
         call mfile%add_column(Y(isc_n2),'N2 mass fraction')
         call mfile%add_column(T,'Temperature')
         call mfile%write()
      end block create_monitor


   end subroutine simulation_init


   !> Perform an NGA2 simulation
   subroutine simulation_run
      implicit none
      integer :: ispec


      ! Perform time integration
      do while (.not.time%done())

         ! Increment time
         call time%increment()

         ! Remember old scalar
         Zold=Z

         ! Perform sub-iterations
         do while (time%it.le.time%itmax)

            ! Get the rhs from chsourcenet
            call csn%get_src(Z,Z_src)

            ! Perform the explicit first order time integration
            Z=Zold+Z_src*time%dt

            ! Increment sub-iteration counter
            time%it=time%it+1

         end do

         ! Map the neural network scalars to Y and T
         call aen%decode(Z,TYS)
         call aen%inverse_transform_outputs(TYS(1:nY_sub+1),hY)
         T=hY(1)
         do ispec=1,nY_sub
            Y(int(aen%vectors(aen%ivec_spec_inds)%vector(ispec)))=hY(ispec+1)
         end do

         ! Output
         call mfile%write()

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
      deallocate(Z,Zold,Z_src,Y,hY,TYS)

      
   end subroutine simulation_final


end module simulation
