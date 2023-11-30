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

      ! Indices of vectors
      integer :: ivec_x_scale,ivec_y_scale                                           ! Scale
      integer :: ivec_x_shift,ivec_y_shift                                           ! Shift

      ! Indices of matrices
      integer :: imat_hid1_bias,imat_hid2_bias,imat_outp_bias                        ! Bias
      integer :: imat_hid1_weight,imat_hid2_weight,imat_outp_weight,imat_proj_weight ! Weight

      ! Transposed matrices
      real(WP), dimension(:,:), allocatable :: proj_weight_T
      real(WP), dimension(:,:), allocatable :: hid1_weight_T,hid2_weight_T
      real(WP), dimension(:,:), allocatable :: outp_weight_T

   contains
      procedure :: init                      !< Initialization of the auto encoder
      procedure :: encode                    !< Encode
      procedure :: decode                    !< Decode
      procedure :: transform_inputs          !< Transform inputs
      procedure :: inverse_transform_outputs !< Inverse transform outputs
   end type aencoder


contains


   !> Initialization for auto encoder object
   subroutine init(this,fdata,name)
      use messager, only: die
      implicit none
      class(aencoder) , intent(inout)        :: this
      character(len=*), intent(in)           :: fdata
      character(len=*), intent(in), optional :: name
      integer :: imatrix,ivector

      ! Call the initialization from parrent
      call this%initialize(fdata=fdata,name=name)

      ! Set the vector indices
      do ivector=1,this%nvector
        select case (trim(this%vecnames(ivector)))
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
      if(                                                                             &
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
          case ('hidden1_bias')
            this%imat_hid1_bias=imatrix
          case ('hidden2_bias')
            this%imat_hid2_bias=imatrix
          case ('output_bias')
            this%imat_outp_bias=imatrix
          case ('hidden1_weight')
            this%imat_hid1_weight=imatrix
          case ('hidden2_weight')
            this%imat_hid2_weight=imatrix
          case ('output_weight')
            this%imat_outp_weight=imatrix
          case ('project_weight')
            this%imat_proj_weight=imatrix
         end select
      end do
      if(                                                                                            &
         max(                                                                                        &
             this%imat_hid1_bias,this%imat_hid2_bias,this%imat_outp_bias,                            &
             this%imat_hid1_weight,this%imat_hid2_weight,this%imat_outp_weight,this%imat_proj_weight &
            )                                                                                        & 
         .ne.                                                                                        & 
         this%nmatrix                                                                                &
        ) then
          call die('[aencoder init] Inconsistent number of matrices')
      end if

      ! Allocate transposed matrices
      allocate(this%proj_weight_T(size(this%matrices(this%imat_proj_weight)%matrix,dim=2),size(this%matrices(this%imat_proj_weight)%matrix,dim=1)))
      allocate(this%hid1_weight_T(size(this%matrices(this%imat_hid1_weight)%matrix,dim=2),size(this%matrices(this%imat_hid1_weight)%matrix,dim=1)))
      allocate(this%hid2_weight_T(size(this%matrices(this%imat_hid2_weight)%matrix,dim=2),size(this%matrices(this%imat_hid2_weight)%matrix,dim=1)))
      allocate(this%outp_weight_T(size(this%matrices(this%imat_outp_weight)%matrix,dim=2),size(this%matrices(this%imat_outp_weight)%matrix,dim=1)))

      ! Get the transposed matrices
      this%proj_weight_T=transpose(this%matrices(this%imat_proj_weight)%matrix)
      this%hid1_weight_T=transpose(this%matrices(this%imat_hid1_weight)%matrix)
      this%hid2_weight_T=transpose(this%matrices(this%imat_hid2_weight)%matrix)
      this%outp_weight_T=transpose(this%matrices(this%imat_outp_weight)%matrix)
   end subroutine init

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
      output=(input-this%matrices(this%ivec_x_shift)%matrix)/this%matrices(this%ivec_x_scale)%matrix
   end subroutine transform_inputs


   !> Inverse transform outputs
   subroutine inverse_transform_outputs(this,input,output)
      implicit none
      class(aencoder), intent(inout) :: this
      real(WP), dimension(:,:), intent(in)    :: input
      real(WP), dimension(:,:), intent(inout) :: output
      output=input*this%matrices(this%ivec_y_scale)%matrix+this%matrices(this%ivec_y_shift)%matrix
   end subroutine inverse_transform_outputs


end module aencoder_class
