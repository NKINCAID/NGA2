!> Neural network class:
module nnetwork_class
   use precision,         only: WP
   use multimatrix_class, only: multimatrix
   use mathtools,         only: ReLU
   implicit none
   private

   public :: nnetwork


   !> Neural network object definition
   type, extends(multimatrix) :: nnetwork

      ! Indices of matrices
      integer :: imat_lay0_bias,imat_lay1_bias,imat_outp_bias       ! Bias
      integer :: imat_lay0_weight,imat_lay1_weight,imat_outp_weight ! Weight
      integer :: imat_x_scale,imat_y_scale                          ! Scale
      integer :: imat_x_shift,imat_y_shift                          ! Shift

      ! Transposed matrices
      real(WP), dimension(:,:), allocatable :: lay0_weight_T,lay1_weight_T
      real(WP), dimension(:,:), allocatable :: outp_weight_T

   contains
      procedure :: call
   end type nnetwork

   !> Declare neural network constructor
   interface nnetwork
      procedure constructor
   end interface nnetwork

contains


   !> Default constructor for neural network object
   function constructor() result(self)
      implicit none
      type(nnetwork) :: self

      ! Set the matrix indices
      do imat=1,self%nmatrix
         select case (trim(self%matnames(imat)))
         end select
      end do
      if(                                                                                             &
         max(                                                                                         &
             
            )                                                                                         & 
         .ne.                                                                                         & 
         self%nmatrix                                                                                 &
        ) then
          call die('[aencoder constructor] Inconsistent number of matrices')
      end if

      ! Allocate matrices

      ! Get the transposed
   end function constructor


   subroutine call(this,input,output)
      implicit none
      class(nnetwork), intent(inout) :: this
      real(WP), dimension(:,:), intent(in)    :: input
      real(WP), dimension(:,:), intent(inout) :: output
      output=ReLU(matmul(input ,this%lay0_weight_T)+this%matrices(this%imat_lay0_bias)%matrix)
      output=ReLU(matmul(output,this%lay1_weight_T)+this%matrices(this%imat_lay1_bias)%matrix)
      output = matmul(output,this%outp_weight_T)+this%matrices(this%imat_outp_bias)%matrix
   end subroutine call


end module nnetwork_class
