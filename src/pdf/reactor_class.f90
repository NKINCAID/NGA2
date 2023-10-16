!> Reactor solver class:

module reactor_class
  use precision, only: WP
  use string, only: str_medium

  type :: reactor

  contains
    procedure :: step
  end type

  interface reactor
    procedure constructor
  end interface reactor

contains

  function constructor() result(this, p)
    implicit none
    class(reactor), intent(inout) :: this
    ! Local variables
    real(WP) :: tempref, yref, bundlesize
    integer :: ibuf

    ! Kinetic solver-related input parameters
    call parser_read('Finite chem acvg', atol, 1.0e-8_WP)
    call parser_read('Finite chem rcvg', rtol, 1.0e-8_WP)

    ! Newton iterations parameters (H2T)
    call parser_read('Newton iterations', nnewton, 20)
    call parser_read('Newton tolerance', newton_tol, 1.0e-4_WP)

    ! Use scheduler
    call parser_read('Use scheduler', use_scheduler, .false.)

    ! Use ISAT or not
    call parser_read('Use ISAT', use_isat, .false.)

    ! Use analytical jacobian or not
    call parser_read('Use analytical jac', use_jacanal, .false.)

    ! Further initialization
    if (use_isat) then
      call parser_read('Max jacobians', njac_max, 10)
      allocate (ind_jac(njac_max), ind_disp(njac_max), dt_array(njac_max), jac_array(njac_max, npS1, npS1))

      ! info
      idtab = 1
      mode = 0
      nxx = npS2
      nff = npS1
      nhh = 0
      info = 0
      info(1) = 1
      info(12) = 2
      info(13) = 5000 ! error output

      ! rinfo
      allocate (rinfo(50 + nxx + npS2))
      rinfo = 0.0_WP
      rinfo(1:50) = 0.0_WP

      ! ISAT error tolerance
      call parser_read('ISAT error tolerance', rinfo(1), 1e-5_WP)

      ! Storage allowed
      call parser_read('Table size in MB', rinfo(8), 500.0_WP)

      ! Scaling for x
      yref = 0.1_WP
      tempref = 1000.0_WP
      rinfo(51:50 + npS1) = yref
      rinfo(50 + npS1) = tempref
      rinfo(50 + npS2) = 1.0_WP

      ! Scaling for f
      rinfo(50 + nxx + 1:50 + nxx + npS) = yref
      rinfo(50 + nxx + npS1) = tempref
      rinfo(50 + nxx + npS2) = 1.0_WP

      rrusr(1) = 1.0_WP

      ! Sensitivity parameters
      sens_lim = 10.0_WP

    end if

    ! Bundle size for scheduler - in terms of number of real numbers
    call parser_read('Bundle size', bundlesize, 1000.0_WP)
    ! Corresponding number of particles per bundle for this specific problem
    nbundles_max = int(bundlesize/(real(pmc_kind, WP)/8.0_WP)) + 1
    nppbundle = nbundles_max
    call parallel_max(nbundles_max, ibuf); nbundles_max = ibuf
    print *, 'Maximum number of compositions per bundles:', nbundles_max, '[proc: ', irank, ']'

    ! Allocate space for scheduler variables
    allocate (ihead_list(nproc), ihead_aware(nproc))
    allocate (bufferS(nbundles_max*npS1), bufferR(nbundles_max*npS1))
    allocate (iwhere_buf(nbundles_max))
    allocate (iwhere(nproc, nbundles_max))
    allocate (nwhere(nproc))
    allocate (iproc_waiting(nproc))

    ! Offset for ISAT to perform PLP
    call parser_read('PLP offset', PLPoffset, 20)
  end function constructor

  subroutine step()
    implicit none

    integer :: i, myi
    real(WP) :: deltat
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
    if (.not. use_scheduler .or. PLPoffset .gt. ntime .or. nproc .eq. 1) then

      do i = 1, npmc_
        ! Package initial solution vector
        sol(1:npS) = pmc(i)%Yi
        sol(npS1) = pmc(i)%T
        ! Advance the chemical equations for each particle
        call pdf_reaction_compute_sol(sol, Pthermo, deltat)
        ! Transfer back to particle structure
        pmc(i)%Yi = sol(1:npS)
        pmc(i)%T = sol(npS1)
        ! Update other particle variables
        ! Enthalpy - Should be constant since reaction step is at constant pressure
        myh = pmc(i)%h
        call pdf_reaction_thermodata(pmc(i)%Yi, pmc(i)%T, rbuf, pmc(i)%h)
if (abs(pmc(i)%h-myh)/myh.gt.0.01_WP) print*,'===== P1 - ENTHALPY LIKELY WRONG!!!',i,irank,' - H = ',pmc(i)%h,myh,(pmc(i)%h-myh)/myh
        ! Adjust volume (assume constant mass)
        call pdf_reaction_Wmix(pmc(i)%Yi, 'Y', myw)
        call pdf_reaction_sv(pmc(i)%T, myw, mysv)
        pmc(i)%vol = mysv*pmc(i)%m
      end do

      return
    end if

    ! Total number of particles in DI
    nDI = npmc_

    ! Redefine bundle size to correspond to at least to twice the number of processors
    nbundles = min(nbundles_max, int(real(nDI, WP)/real(2*nproc, WP)) + 1)

    ! Make sure the same number applies to everybody (take maximum, so nbundles_max better be adequate)
    call parallel_max(nbundles, ibuf); nbundles = ibuf

    ! Transfer it to pdf module for monitor output
    nppbundle = nbundles

    ! ------------------------------------------- !
    ! Dynamic scheduler here for load balancing
    ! ------------------------------------------- !

    ! Tag meanings:
    ! TAG=1: sending number of data
    itag_ndata = 1
    ! TAG=2: sending data
    itag_data = 2
    ! TAG=3: identity of head
    itag_ihead = 3
    ! TAG=4: idle
    itag_idle = 4
    ! TAG=5: quit signal
    itag_done = 5

    ! Initialize head/worker identities
    ! Initial head id
    ihead = iroot
    ! flag that a new head has been promoted
    inewhead = -1
    ! Processors that have been head already
    ihead_list = 0
    ihead_list(ihead) = 1
    ! Flag indicating that data need to be sent back to head
    idata = 0
    ! Extra integer buffer
    ibuf = 1
    ! Not done to start with
    ldone = .false.
    ! first particle considered will have index 1
    ipmc_ = 1

    ! Loop until all work is done
    scheduler_loop: do while (.not. ldone)

      ! head loop
      if (irank .eq. ihead) then

        ! Initializing processor roles and buffers
        nproc_waiting = 0
        iproc_waiting = 0
        bufferR = 0.0_WP
        bufferS = 0.0_WP
        ! Initialize ndata
        ndata = 0
        ! Initialize loop condition
        iexit_head = 0

        ! Let's send out all my data
        head_loop: do while (iexit_head .eq. 0) ! No explicit exit conditions here, automatically handled below

          ! ------------------------------------------- !
          ! Gather compositions until there is a bundle of ndata particles ready to send
          ! Skip locally processed compositions
          do while (ndata .lt. nbundles .and. ipmc_ .le. nDI)
            ! Need to gather some more
            ! Initialize solution vector
            sol(1:npS) = pmc(iDI(ipmc_))%Yi
            sol(1:npS) = sol(1:npS)/sum(sol(1:npS))
            sol(npS + 1) = pmc(iDI(ipmc_))%T
            ! Buffer composition to send out to other processors
            ndata = ndata + 1
            bufferS((ndata - 1)*npS1 + 1:ndata*npS1) = sol
            iwhere_buf(ndata) = iDI(ipmc_) ! link between bundle and pmc array
            nwhere_buf = ndata ! Keep track of actual number of compos in bundle
            ! Increment index of next composition to consider
            ipmc_ = ipmc_ + 1
          end do

          ! ------------------------------------------- !
          ! Listen to the workers until one raises its hand
          call MPI_probe(MPI_ANY_SOURCE, MPI_ANY_TAG, comm, status, ierr)
          ! Got something: which worker is talking?
          iworker = status(MPI_SOURCE) + 1
          ! What message is it sending?
          itag = status(MPI_TAG)

          ! ------------------------------------------- !
          ! Does this worker have results to send?
          if (itag .eq. itag_data) then
            ! itag = 1: worker has data to send back, receive them!
       call MPI_recv(bufferR(1:nwhere(iworker)*npS1), nwhere(iworker)*npS1, MPI_REAL_WP, iworker - 1, itag_data, comm, status, ierr)
            ! Store each bufferR compo at correct location in pmc array
            do i = 1, nwhere(iworker)
              myi = iwhere(iworker, i)
              pmc(myi)%Yi = bufferR((i - 1)*npS1 + 1:i*npS1 - 1)
              pmc(myi)%T = bufferR(i*npS1)
              ! Update other particle variables
              ! Enthalpy - Should be constant since reaction step is at constant pressure
              myh = pmc(myi)%h
              call pdf_reaction_thermodata(pmc(myi)%Yi, pmc(myi)%T, rbuf, pmc(myi)%h)
                   if (abs(pmc(myi)%h-myh)/myh.gt.0.01_WP) print*,'===== P3 - ENTHALPY LIKELY WRONG!!!',i,irank,' - H = ',pmc(myi)%h,myh,(pmc(myi)%h-myh)/myh
              ! Adjust volume (assume constant mass)
              call pdf_reaction_Wmix(pmc(myi)%Yi, 'Y', myw)
              call pdf_reaction_sv(pmc(myi)%T, myw, mysv)
              pmc(myi)%vol = mysv*pmc(myi)%m
            end do
          else
            ! Just receive its empty message
            call MPI_recv(ibuf, 1, MPI_INTEGER, iworker - 1, itag_idle, comm, status, ierr)
          end if

          ! ------------------------------------------- !
          ! Do I have more work to do?
          if (ndata .gt. 0) then ! ndata not 0: last bundle has not been sent yet, more work to do!
            ! Keep a note of what was sent to iworker
            iwhere(iworker, :) = iwhere_buf
            nwhere(iworker) = nwhere_buf
            ! If yes, send the number of data that will be sent
            call MPI_send(nwhere(iworker), 1, MPI_INTEGER, iworker - 1, itag_ndata, comm, ierr)
            ! Send the next chunk to this worker
            call MPI_send(bufferS(1:nwhere(iworker)*npS1), ndata*npS1, MPI_REAL_WP, iworker - 1, itag_data, comm, ierr)
            ! Reset buffer to start accumulating more composition for next call
            ndata = 0
            iwhere_buf = 0
            nwhere_buf = 0
            ! Go back to beginning of loop
            cycle head_loop
          end if

          ! ------------------------------------------- !
          ! If reaching here, no more work to do,
          ! ready to finish work as head and switch to worker status

          ! ------------------------------------------- !
          ! Has a new head being promoted?
          if (inewhead .eq. -1) then !no, no new head yet

            ! Reset ihead_aware to 0
            ihead_aware = 0
            ! But I am aware already, that counts for someting
            ihead_aware(irank) = 1

            ! Has this worker been a head yet?
            if (ihead_list(iworker) .eq. 0) then ! no, not yet
              ! Promote the worker to head status
              inewhead = iworker
              ! Tell the current worker that it is the new head and update aware list
              call MPI_send(inewhead, 1, MPI_INTEGER, iworker - 1, itag_ihead, comm, ierr)
              ihead_aware(iworker) = 1
              ! Send id of new head to all waiting workers and update aware list
              do ip = 1, nproc_waiting
                call MPI_send(inewhead, 1, MPI_INTEGER, iproc_waiting(ip) - 1, itag_ihead, comm, ierr)
                ihead_aware(iproc_waiting(ip)) = 1
              end do

            else ! Yes, this proc has been head already
              ! Add identity of this worker to the waiting list
              nproc_waiting = nproc_waiting + 1
              iproc_waiting(nproc_waiting) = iworker
              ! Check if we are fully done (all procs have been heads already)
              if (nproc_waiting .eq. nproc - 1) then
                exit head_loop
              end if
            end if

            ! ------------------------------------------- !
          else ! Yes, a new head has been promoted
            ! Send the id of the new head to the current worker and update aware list
            call MPI_send(inewhead, 1, MPI_INTEGER, iworker - 1, itag_ihead, comm, ierr)
            ihead_aware(iworker) = 1
          end if

          ! ------------------------------------------- !
          ! Continue waiting for signals till all workers have received new head id
          if (sum(ihead_aware) .eq. nproc) iexit_head = 1

        end do head_loop

        ! ------------------------------------------- !
        ! Check if I am the last head
        if (sum(ihead_list) .eq. nproc) then
          ! If so, update done
          ldone = .true.
          ! Send a quit signal to every body
          do ip = 1, nproc
            if (ip .eq. irank) cycle
            call MPI_send(ldone, 1, MPI_LOGICAL, ip - 1, itag_done, comm, ierr)
          end do
        else ! I am not the last head
          ! Switching the head id to somebody else
          ihead = inewhead
          ! Keep it rolling
          cycle scheduler_loop
        end if

        ! ------------------------------------------- !
        ! ------------------------------------------- !
        ! worker loop
      else

        worker_loop: do while (.true.) ! No explicit exit conditions here, automatically handled below
          ! ------------------------------------------- !
          ! Do I have results to send to the head?
          if (idata .eq. 1) then ! yes, I have data to send
            ! Send them to head
            call MPI_send(bufferR(1:ndata*npS1), ndata*npS1, MPI_REAL_WP, ihead - 1, itag_data, comm, ierr)
            ! No more data to send for now
            idata = 0
          else ! No, no data to send
            ! Just tell the head I am available
            call MPI_send(ibuf, 1, MPI_INTEGER, ihead - 1, itag_idle, comm, ierr)
          end if

          ! ------------------------------------------- !
          ! Listening to the head to see what is coming next
          call MPI_probe(ihead - 1, MPI_ANY_TAG, comm, status, ierr)
          ! What message is it sending?
          itag = status(MPI_TAG)

          ! ------------------------------------------- !
          ! What is the message?
          if (itag .eq. itag_done) then ! Quit message
            ! Receive it (we just tested the tag here, still need to actually receive the integer)
            call MPI_recv(ldone, 1, MPI_LOGICAL, ihead - 1, itag_done, comm, status, ierr)
            cycle scheduler_loop

            ! ------------------------------------------- !
          elseif (itag .eq. itag_ndata) then ! Work message
            ! Receiving number of data expected
            call MPI_recv(ndata, 1, MPI_integer, ihead - 1, itag_ndata, comm, status, ierr)
            ! Receiving chunk of data to process
            call MPI_recv(bufferR(1:ndata*npS1), ndata*npS1, MPI_REAL_WP, ihead - 1, itag_data, comm, status, ierr)

            ! Do the work
            do i = 1, ndata
              call pdf_reaction_compute_sol(bufferR((i - 1)*npS1 + 1:i*npS1), Pthermo, deltat)
            end do
            ! Now I have data to send
            idata = 1
            ! Cycle the worker loop again
            cycle worker_loop

            ! ------------------------------------------- !
          elseif (itag .eq. itag_ihead) then ! head has changed
            ! Receive the id of the new head
            call MPI_recv(ibuf, 1, MPI_integer, ihead - 1, itag_ihead, comm, status, ierr)
            ! Update the id of head
            ihead = ibuf
            ihead_list(ihead) = 1
            ! Cycle the scheduler loop
            cycle scheduler_loop

          end if
        end do worker_loop
      end if
    end do scheduler_loop

    return
  end subroutine step
end module
