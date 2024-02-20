!> Various definitions and tools for running an NGA2 simulation
module simulation
   use precision,         only: WP
   use string,            only: str_medium
   use geometry,          only: cfg
   use timetracker_class, only: timetracker
   use monitor_class,     only: monitor
   use finitechem_class,  only: finitechem
   use fcmech
   implicit none
   private

   !> Finite rate chemistry solver
   type(finitechem), public :: fc

   !> Time tracker
   type(timetracker), public :: time

   !> Simulation monitor files
   type(monitor) :: mfile

   !> Simulation sub-routines
   public :: simulation_init,simulation_run,simulation_final


contains


   !> Initialization of problem solver
   subroutine simulation_init
      use param, only: param_read
      implicit none


      ! Modify indices for a faster homogeneous run
      cfg%imino_=cfg%imin_; cfg%imaxo_=cfg%imax_
      cfg%jmino_=cfg%jmin_; cfg%jmaxo_=cfg%jmax_
      cfg%kmino_=cfg%kmin_; cfg%kmaxo_=cfg%kmax_


      ! Create a scalar solver
      create_fc: block
         use multivdscalar_class, only: quick
         ! Create scalar solver
         fc=finitechem(cfg=cfg,scheme=quick,name='fc')
         fc%use_scheduler=.false.
      end block create_fc


      ! Initialize time tracker
      initialize_timetracker: block
         time=timetracker(amRoot=cfg%amRoot,name='homogeneous_reactor_ann')
         call param_read('Max timestep size',time%dtmax)
         call param_read('Max time',time%tmax)
         time%dt=time%dtmax
         time%itmax=1
      end block initialize_timetracker


      ! Initialize finitechem solver
      initialize_fc: block
         use param, only: param_exists
         real(WP) :: tmpSC
         integer  :: ispec
         ! Set the initial conditions
         do ispec=1,nspec
            if (param_exists('Initial '//trim(fc%SCname(ispec)))) then
               call param_read('Initial '//trim(fc%SCname(ispec)),tmpSC)
               fc%SC(:,:,:,ispec)=tmpSC
               if (cfg%amRoot) then
                  print*,"Initial ",trim(fc%SCname(ispec)),tmpSC
               end if
            end if
         end do
         call param_read('Temperature',tmpSC)
         fc%SC(:,:,:,nspec+1)=tmpSC
         call param_read('Pressure',fc%Pthermo)
         call fc%get_density()
         call fc%get_max()
      end block initialize_fc


      ! Create a monitor files
      create_monitor: block
         mfile=monitor(cfg%amRoot,'simulation')
         call mfile%add_column(time%t,'Time')
         call mfile%add_column(fc%SC(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,sHMN),'Y_HMN')
         call mfile%add_column(fc%SC(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,sN2),'Y_N2')
         call mfile%add_column(fc%SC(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,sO2),'Y_O2')
         call mfile%add_column(fc%SC(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,nspec+1),'Temperature')
         call mfile%write()
      end block create_monitor


   end subroutine simulation_init


   !> Perform an NGA2 simulation
   subroutine simulation_run
      implicit none
      integer :: isc


      ! Perform time integration
      do while (.not.time%done())

         ! Increment time
         call time%increment()

         ! Remember old scalars
         fc%rhoold=fc%rho
         fc%SCold =fc%SC

         ! Get the reaction source terms
         fc%SRCchem=0.0_WP
         call fc%react(time%dt)

         ! Perform sub-iterations
         do while (time%it.le.time%itmax)

            ! Perform Euler time integration
            do isc=1,fc%nscalar
               ! fc%SC(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,isc)=fc%SCold(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,isc)+fc%rho(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_)*fc%SRCchem(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,isc)
               fc%SC(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,isc)=fc%SCold(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,isc)+fc%SRCchem(fc%cfg%imin_,fc%cfg%jmin_,fc%cfg%kmin_,isc)
            end do

            ! Update density
            call fc%get_density()

            ! Increment sub-iteration counter
            time%it=time%it+1

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


   end subroutine simulation_final


end module simulation
