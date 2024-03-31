!> Artificial neural network class:
module ann_class
   use precision,    only: WP
   use config_class, only: config
   use mathtools,    only: ReLU
   implicit none
   private

   public :: ann


   !> Hidden layer for the ANN
   type :: hidden_layer
      type(hidden_layer), pointer           :: next   !< Singly-connected linked list of hidden_layers
      real(WP), dimension(:,:), allocatable :: weight !< Weight matrix
      real(WP), dimension(:),   allocatable :: bias   !< Bias vector
   end type hidden_layer


   !> ANN object definition
   type :: ann

      ! Input manipulators
      logical :: is_inp_man
      real(WP), dimension(:), allocatable :: inp_shift,inp_scale

      ! Input sub-array indices
      logical :: is_sub_ind
      integer, dimension(:), allocatable :: inp_sub_ind

      ! Hidden layers
      integer :: n_hid_lay
      type(hidden_layer) :: first_hid_lay

      ! Output layer
      real(WP), dimension(:,:), allocatable :: out_weight
      real(WP), dimension(:),   allocatable :: out_bias

      ! Output manipulators
      logical :: is_out_man
      real(WP), dimension(:), allocatable :: out_shift,out_scale

   contains

      procedure                                   :: add_layer    !< Add a new hidden layer to the network
      procedure(forward_pass_interface), deferred :: forward_pass !< Take input to forward pass and get the output

   end type ann


   !> Declare ANN constructor
   interface ann
      procedure constructor
   end interface ann


   !> Interface
   abstract interface
      subroutine forward_pass_interface(this)
         import ann
         class(ann), intent(in) :: this
      end subroutine forward_pass_interface
   end interface


contains


   !> Default constructor for ANN
   function constructor(cfg,fdata,name) result(self)
      use messager, only: die
      use parallel, only: info_mpiio,MPI_INTEGER,MPI_LOGICAL,MPI_REAL_WP
      use mpi_f08
      implicit none
      type(ann)                              :: self
      class(config), target, intent(in)      :: cfg
      character(len=*), intent(in)           :: fdata
      character(len=*), intent(in), optional :: name
      type(hidden_layer)                     :: first_hid_lay
      real(WP), dimension(:), allocatable    :: weight,bias
      integer                                :: ihid,n_row,n_col
      integer                                :: ierr
      type(MPI_File)                         :: ifile
      type(MPI_Status)                       :: status

      ! Point to pgrid object
      self%cfg=>cfg

      ! Set the name for the multi matrix object
      if (present(name)) self%name=trim(adjustl(name))

      ! Set the file name
      self%filename=trim(adjustl(fdata))

      ! Initialize hidden layers
      self%n_hid_lay=0
      self%first_hid_lay=>NULL()

      ! Open the file
      call MPI_FILE_OPEN(self%cfg%comm,trim(self%filename),MPI_MODE_RDONLY,info_mpiio,ifile,ierr)
      if (ierr.ne.0) call die('[ann constructor] Problem encountered while reading file: '//trim(self%filename))

      ! Read input manipulators
      call MPI_FILE_READ_ALL(ifile,is_inp_man,1,MPI_LOGICAL,status,ierr)
      if (is_inp_man) then
         ! Shift vector
            ! Size
            call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
            allocate(self%inp_shift(n_row))
            ! Entries
            call MPI_FILE_READ_ALL(ifile,self%inp_shift,n_row,MPI_REAL_WP,status,ierr)
         ! Scale vector
            ! Size
            call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
            allocate(self%inp_scale(n_row))
            ! Entries
            call MPI_FILE_READ_ALL(ifile,self%inp_scale,n_row,MPI_REAL_WP,status,ierr)
      end if

      ! Read input sub-array indices
      call MPI_FILE_READ_ALL(ifile,is_sub_ind,1,MPI_LOGICAL,status,ierr)
      if (is_sub_ind) then
            ! Size
            call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
            allocate(self%inp_sub_ind(n_row))
            ! Entries
            call MPI_FILE_READ_ALL(ifile,self%inp_sub_ind,n_row,MPI_INTEGER,status,ierr)
      end if

      ! Read hidden layers
      call MPI_FILE_READ_ALL(ifile,self%n_hid_lay,1,MPI_INTEGER,status,ierr)
      do ihid=1,self%n_hid_lay
         ! Read the bias vector
         call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
         allocate(bias(n_row))
         call MPI_FILE_READ_ALL(ifile,bias,n_row,MPI_REAL_WP,status,ierr)
         ! Read the weight matrix
         call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
         call MPI_FILE_READ_ALL(ifile,n_col,1,MPI_INTEGER,status,ierr)
         allocate(weight(n_row,n_col))
         call MPI_FILE_READ_ALL(ifile,weight,n_row*n_col,MPI_REAL_WP,status,ierr)
         ! Add the hidden layer
         call self%add_layer(weight,bias)
      end do

      ! Read output layer
      call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
      allocate(self%out_bias(n_row))
      call MPI_FILE_READ_ALL(ifile,self%out_bias,n_row,MPI_REAL_WP,status,ierr)
      call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
      call MPI_FILE_READ_ALL(ifile,n_col,1,MPI_INTEGER,status,ierr)
      allocate(self%out_weight(n_row,n_col))
      call MPI_FILE_READ_ALL(ifile,self%out_weight,n_row*n_col,MPI_REAL_WP,status,ierr)

      ! Read output manipulators
      call MPI_FILE_READ_ALL(ifile,is_out_man,1,MPI_LOGICAL,status,ierr)
      if (is_out_man) then
         ! Shift vector
            ! Size
            call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
            allocate(self%out_shift(n_row))
            ! Entries
            call MPI_FILE_READ_ALL(ifile,self%out_shift,n_row,MPI_REAL_WP,status,ierr)
         ! Scale vector
            ! Size
            call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
            allocate(self%out_scale(n_row))
            ! Entries
            call MPI_FILE_READ_ALL(ifile,self%out_scale,n_row,MPI_REAL_WP,status,ierr)
      end if

      ! Close the file
      call MPI_FILE_CLOSE(ifile,ierr)

      ! Point to the appropriate forward pass
      ! T/T/T
      if ((self%is_inp_man).and.(is_out_man).and.(is_sub_ind)) then
         self%forward_pass=>forward_pass_TTT
      ! T/T/F
      ! T/F/T
      ! T/F/F
      ! F/T/T
      ! F/T/F
      else if ((.not.self%is_inp_man).and.(is_out_man).and.(.not.is_sub_ind))
         self%forward_pass=>forward_pass_FTF
      ! F/F/T
      ! F/F/F
      else if ((.not.self%is_inp_man).and.(.not.is_out_man).and.(.not.is_sub_ind))
         self%forward_pass=>forward_pass_FFF
      ! Throw error for the combinations not implemented yet
      else
         call die('[ann constructor] Unknown network.')
      end if
   end function constructor


   ! Apend a new hidden layer at the end of the hidden layer list
   subroutine add_layer(this,weight,bias)
      implicit none
      class(ann), intent(inout)            :: this
      real(WP), dimension(:,:), intent(in) :: weight
      real(WP), dimension(:),   intent(in) :: bias
      type(hidden_layer)                   :: last_hid_lay,new_hid_lay

      ! Make the first hidden layer if it does not exist
      if (.not. associated(this%first_hid_lay)) then
         this%first_hid_lay%weight=weight
         this%first_hid_lay%bias=bias
         this%first_hid_lay%next=>NULL()
      else
         ! Prepare new hidden layer
         allocate(new_hid_lay)
         new_hid_lay%weight=weight
         new_hid_lay%bias=bias
         new_hid_lay%next=>NULL()
         ! Traverse hidden layers
         last_hid_lay=>this%first_hid_lay
         do while (associated(last_hid_lay%next))
            ! Move on to the next hidden layer
            last_hid_lay=>last_hid_lay%next
         end do
      end if
      ! Append it to the end of the list
      last_hid_lay=>new_hid_lay
   end subroutine add_layer

   
   !> Forward pass with input manipulation, output manipulation, and input sub-array
   subroutine forward_pass_TTT(this,input,output)
      implicit none
      class(ann), intent(inout)           :: this
      type(hidden_layer)                  :: my_hid_lay
      real(WP), dimension(:), intent(in)  :: input
      real(WP), dimension(:), intent(out) :: output
      real(WP), dimension(:), allocatable :: tmparr

      ! Allocate temporary array
      allocate(tmparr(size(this%first_hid_lay,dim=1)))
      tmparr=ReLU(matmul((input(self%inp_sub_ind)-self%inp_shift)/self%inp_scale,this%first_hid_lay%weight)+this%first_hid_lay%bias)
      ! Traverse hidden layers
      my_hid_lay=>this%first_hid_lay%next
      do while (associated(my_hid_lay))
         ! Hidden layer calculations
         tmparr=ReLU(matmul(tmparr,my_hid_lay%weight)+my_hid_lay%bias)
         ! Move on to the next hidden layer
         my_hid_lay=>my_hid_lay%next
      end do
      ! Output layer calculations
      output=(matmul(tmparr,this%output_weight)+this%output_bias)*self%out_scale+self%out_scale
      ! Deallocate temporary array
      deallocate(tmparr)
   end subroutine forward_pass


   !> Forward pass with output manipulation
   subroutine forward_pass_FTF(this,input,output)
      implicit none
      class(ann), intent(inout)           :: this
      type(hidden_layer)                  :: my_hid_lay
      real(WP), dimension(:), intent(in)  :: input
      real(WP), dimension(:), intent(out) :: output
      real(WP), dimension(:), allocatable :: tmparr

      ! Allocate temporary array
      allocate(tmparr(size(this%first_hid_lay,dim=1)))
      tmparr=ReLU(matmul(input,this%first_hid_lay%weight)+this%first_hid_lay%bias)
      ! Traverse hidden layers
      my_hid_lay=>this%first_hid_lay%next
      do while (associated(my_hid_lay))
         ! Hidden layer calculations
         tmparr=ReLU(matmul(tmparr,my_hid_lay%weight)+my_hid_lay%bias)
         ! Move on to the next hidden layer
         my_hid_lay=>my_hid_lay%next
      end do
      ! Output layer calculations
      output=(matmul(tmparr,this%output_weight)+this%output_bias)*self%out_scale+self%out_scale
      ! Deallocate temporary array
      deallocate(tmparr)
   end subroutine forward_pass


   !> Forward pass without input manipulation, output manipulation, or input sub-array
   subroutine forward_pass_FFF(this,input,output)
      implicit none
      class(ann), intent(inout)           :: this
      type(hidden_layer)                  :: my_hid_lay
      real(WP), dimension(:), intent(in)  :: input
      real(WP), dimension(:), intent(out) :: output
      real(WP), dimension(:), allocatable :: tmparr

      ! Allocate temporary array
      allocate(tmparr(size(this%first_hid_lay,dim=1)))
      tmparr=ReLU(matmul(input,this%first_hid_lay%weight)+this%first_hid_lay%bias)
      ! Traverse hidden layers
      my_hid_lay=>this%first_hid_lay%next
      do while (associated(my_hid_lay))
         ! Hidden layer calculations
         tmparr=ReLU(matmul(tmparr,my_hid_lay%weight)+my_hid_lay%bias)
         ! Move on to the next hidden layer
         my_hid_lay=>my_hid_lay%next
      end do
      ! Output layer calculations
      output=matmul(tmparr,this%output_weight)+this%output_bias
      ! Deallocate temporary array
      deallocate(tmparr)
   end subroutine forward_pass

   
end module ann_class