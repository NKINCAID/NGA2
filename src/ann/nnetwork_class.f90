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

      ! Indices of vectors
      integer :: ivec_x_scale,ivec_y_scale                          ! Scale
      integer :: ivec_x_shift,ivec_y_shift                          ! Shift

      ! Indices of matrices
      integer :: imat_lay0_bias,imat_lay1_bias,imat_outp_bias       ! Bias
      integer :: imat_lay0_weight,imat_lay1_weight,imat_outp_weight ! Weight

      ! Transposed matrices
      real(WP), dimension(:,:), allocatable :: lay0_weight_T,lay1_weight_T
      real(WP), dimension(:,:), allocatable :: outp_weight_T

   contains
      procedure :: init    !< Initialization of the neural network
      procedure :: predict !<
   end type nnetwork


contains


   !> Initialization for neural network object
   subroutine init(this,fdata,name)
      use messager, only: die
      implicit none
      class(nnetwork) , intent(inout)        :: this
      character(len=*), intent(in)           :: fdata
      character(len=*), intent(in), optional :: name
      integer :: imatrix

      ! Call the initialization from parrent
      call this%initialize(fdata=fdata,name=name)

      ! Set the vector indices
      do ivector=1,this%nvector
         select case (trim(this%matnames(ivector)))
         case ('x_scale')
            this%ivec_x_scale=ivector
          case ('y_scale')
            this%ivec_y_scale=ivector
          case ('x_shift')
            this%ivec_x_shift=ivector
          case ('y_shift')
            this%ivec_y_shift=ivector
         end select
      end do
      if(                                                                            &
         max(                                                                        &
             this%ivec_x_scale,this%ivec_y_scale,this%ivec_x_shift,this%ivec_y_shift &
            )                                                                        & 
         .ne.                                                                        & 
         this%nvector                                                                &
        ) then
          call die('[aencoder init] Inconsistent number of vectors')
      end if

      ! Set the matrix indices
      do imatrix=1,this%nmatrix
         select case (trim(this%matnames(imatrix)))
         case ('layers_0_bias')
            this%imat_lay0_bias=imatrix
         case ('layers_1_bias')
            this%imat_lay1_bias=imatrix
         case ('output_bias')
            this%imat_outp_bias=imatrix
         case ('layers_0_weight')
            this%imat_lay0_weight=imatrix
         case ('layers_1_weight')
            this%imat_lay1_weight=imatrix
         case ('output_weight')
            this%imat_outp_weight=imatrix
         end select
      end do
      if(                                                                      &
         max(                                                                  &
             this%imat_lay0_bias,this%imat_lay1_bias,this%imat_outp_bias,      &
             this%imat_lay0_weight,this%imat_lay1_weight,this%imat_outp_weight &
            )                                                                  & 
         .ne.                                                                  & 
         this%nmatrix                                                          &
        ) then
          call die('[aencoder init] Inconsistent number of matrices')
      end if

      ! Allocate transposed matrices
      allocate(this%lay0_weight_T(size(this%matrices(this%imat_lay0_weight)%matrix,dim=2),size(this%matrices(this%imat_lay0_weight)%matrix,dim=1)))
      allocate(this%lay1_weight_T(size(this%matrices(this%imat_lay1_weight)%matrix,dim=2),size(this%matrices(this%imat_lay1_weight)%matrix,dim=1)))
      allocate(this%outp_weight_T(size(this%matrices(this%imat_outp_weight)%matrix,dim=2),size(this%matrices(this%imat_outp_weight)%matrix,dim=1)))

      ! Get the transposed matrices
      this%lay0_weight_T=transpose(this%matrices(this%imat_lay0_weight)%matrix)
      this%lay1_weight_T=transpose(this%matrices(this%imat_lay1_weight)%matrix)
      this%outp_weight_T=transpose(this%matrices(this%imat_outp_weight)%matrix)
   end subroutine init


   subroutine predict(this,input,output)
      implicit none
      class(nnetwork), intent(inout) :: this
      real(WP), dimension(:,:), intent(in)    :: input
      real(WP), dimension(:,:), intent(inout) :: output
      output=ReLU(matmul(input ,this%lay0_weight_T)+this%matrices(this%imat_lay0_bias)%matrix)
      output=ReLU(matmul(output,this%lay1_weight_T)+this%matrices(this%imat_lay1_bias)%matrix)
      output=matmul(output,this%outp_weight_T)+this%matrices(this%imat_outp_bias)%matrix
   end subroutine predict


end module nnetwork_class
