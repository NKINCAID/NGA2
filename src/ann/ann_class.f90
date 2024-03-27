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

      ! Hidden layers
      integer :: nhid
      type(hidden_layer) :: first_hid_lay

   contains

      procedure :: add_layer                        !< Add a new hidden layer to the network
      procedure :: forward_pass                     !< Take input to forward pass and get the output

   end type ann


   !> Declare ANN constructor
   interface ann
      procedure constructor
   end interface ann


contains


   !> Default constructor for ANN
   function constructor(cfg,fdata,name) result(self)
      use messager, only: die
      implicit none
      type(ann)                              :: self
      class(config), target, intent(in)      :: cfg
      character(len=*), intent(in)           :: fdata
      character(len=*), intent(in), optional :: name
      type(hidden_layer) :: first_hid_lay

      ! Initialize hidden layers
      self%nhid=0
      self%first_hid_lay=>NULL()
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

   
   !> Take input to forward pass and get the output
   subroutine forward_pass(this,input,output)
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