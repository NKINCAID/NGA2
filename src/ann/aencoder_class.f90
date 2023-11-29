!> Auto encoder class:
module aencoder_class
   use precision,         only: WP
   use multimatrix_class, only: multimatrix
   use mathtools,         only: ReLU
   implicit none
   private

   public :: aencoder


   !> Auto encoder object definition
   type, extends(multimatrix) :: aencoder

      ! Indices of matrices
      integer :: imat_hid1_bias,imat_hid2_bias,imat_outp_bias                        ! Bias
      integer :: imat_hid1_weight,imat_hid2_weight,imat_outp_weight,imat_proj_weight ! Weight
      integer :: imat_x_scale,imat_y_scale                                           ! Scale
      integer :: imat_x_shift,imat_y_shift                                           ! Shift

      ! Transposed matrices
      real(WP), dimension(:,:), allocatable :: proj_weight_T
      real(WP), dimension(:,:), allocatable :: hid1_weight_T,hid2_weight_T
      real(WP), dimension(:,:), allocatable :: outp_weight_T

   contains
      procedure :: encode                    !< Encode
      procedure :: decode                    !< Decode
      procedure :: transform_inputs          !< Transform inputs
      procedure :: inverse_transform_outputs !< Inverse transform outputs
   end type aencoder


   !> Declare auto encoder constructor
   interface aencoder
      procedure constructor
   end interface aencoder

contains


   !> Default constructor for auto encoder object
   function constructor() result(self)
      implicit none
      type(aencoder) :: self
      integer :: imat

      ! Set the matrix indices
      do imat=1,self%nmatrix
         select case (trim(self%matnames(imat)))
          case ('hidden1_bias')
            self%imat_hid1_bias=imat
          case ('hidden2_bias')
            self%imat_hid2_bias=imat
          case ('output_bias')
            self%imat_outp_bias=imat
          case ('hidden1_weight')
            self%imat_hid1_weight=imat
          case ('hidden2_weight')
            self%imat_hid2_weight=imat
          case ('output_weight')
            self%imat_outp_weight=imat
          case ('project_weight')
            self%imat_proj_weight=imat
          case ('x_scale')
            self%imat_x_scale=imat
          case ('y_scale')
            self%imat_y_scale=imat
          case ('x_shift')
            self%imat_x_shift=imat
          case ('y_shift')
            self%imat_y_shift=imat
         end select
      end do
      if(                                                                                             &
         max(                                                                                         &
             self%imat_hid1_bias,self%imat_hid2_bias,self%imat_outp_bias,                             &
             self%imat_hid1_weight,self%imat_hid2_weight,self%imat_outp_weight,self%imat_proj_weight, $
             self%imat_x_scale,self%imat_y_scale,self%imat_x_shift,self%imat_y_shift                  &
            )                                                                                         & 
         .ne.                                                                                         & 
         self%nmatrix                                                                                 &
        ) then
          call die('[aencoder constructor] Inconsistent number of matrices')
      end if

      ! Allocate matrices
      allocate(self%proj_weight_T(size(self%matrices(self%imat_proj_weight)%matrix,dim=2),size(self%matrices(self%imat_proj_weight)%matrix,dim=1)))
      allocate(self%hid1_weight_T(size(self%matrices(self%imat_hid1_weight)%matrix,dim=2),size(self%matrices(self%imat_hid1_weight)%matrix,dim=1)))
      allocate(self%hid2_weight_T(size(self%matrices(self%imat_hid2_weight)%matrix,dim=2),size(self%matrices(self%imat_hid2_weight)%matrix,dim=1)))
      allocate(self%outp_weight_T(size(self%matrices(self%imat_outp_weight)%matrix,dim=2),size(self%matrices(self%imat_outp_weight)%matrix,dim=1)))

      ! Get the transposed
      self%proj_weight_T=transpose(self%matrices(self%imat_proj_weight)%matrix)
      self%hid1_weight_T=transpose(self%matrices(self%imat_hid1_weight)%matrix)
      self%hid2_weight_T=transpose(self%matrices(self%imat_hid2_weight)%matrix)
      self%outp_weight_T=transpose(self%matrices(self%imat_outp_weight)%matrix)
   end function constructor

   !> Encode
   subroutine encode(this,input,output)
      implicit none
      class(aencoder), intent(in) :: this
      real(WP), dimension(:,:), intent(in)    :: input
      real(WP), dimension(:,:), intent(inout) :: output
      output=matmul(input,this%proj_weight_T)
   end subroutine encode


   !> Decode
   subroutine decode(this,input,output)
      implicit none
      class(aencoder), intent(inout) :: this
      real(WP), dimension(:,:), intent(in)    :: input
      real(WP), dimension(:,:), intent(inout) :: output
      output=ReLU(matmul(input ,this%hid1_weight_T)+this%matrices(this%imat_hid1_bias)%matrix)
      output=ReLU(matmul(output,this%hid2_weight_T)+this%matrices(this%imat_hid2_bias)%matrix)
      output=matmul(output,this%outp_weight_T)+this%matrices(this%imat_outp_bias)%matrix
   end subroutine decode


   !> Transform inputs
   subroutine transform_inputs(this,input,output)
      implicit none
      class(aencoder), intent(inout) :: this
      real(WP), dimension(:,:), intent(in)    :: input
      real(WP), dimension(:,:), intent(inout) :: output
      output=(input-this%matrices(this%imat_x_shift)%matrix)/this%matrices(this%imat_x_scale)%matrix
   end subroutine transform_inputs


   !> Inverse transform outputs
   subroutine inverse_transform_outputs(this,input,output)
      implicit none
      class(aencoder), intent(inout) :: this
      real(WP), dimension(:,:), intent(in)    :: input
      real(WP), dimension(:,:), intent(inout) :: output
      output=input*this%matrices(this%imat_y_scale)%matrix+this%matrices(this%imat_y_shift)%matrix
   end subroutine inverse_transform_outputs


end module aencoder_class
