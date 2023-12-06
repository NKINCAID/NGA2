!> Auto encoder class:
module aencoder_class
   use precision,         only: WP
   use config_class,      only: config
   use multimatrix_class, only: multimatrix
   use mathtools,         only: ReLU
   implicit none
   private

   public :: aencoder


   !> Auto encoder object definition
   type, extends(multimatrix) :: aencoder

      ! Indices of vectors
      integer :: ivec_hid1_bias,ivec_hid2_bias,ivec_outp_bias ! Bias
      integer :: ivec_x_scale,ivec_y_scale                    ! Scale
      integer :: ivec_x_shift,ivec_y_shift                    ! Shift

      ! Indices of matrices
      integer :: imat_hid1_weight,imat_hid2_weight,imat_outp_weight,imat_proj_weight ! Weight

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


   !> Declare aencoder constructor
   interface aencoder
      procedure constructor
   end interface aencoder


contains


   !> Default constructor for aencoder
   function constructor(cfg,fdata,name) result(self)
      use messager, only: die
      implicit none
      type(aencoder)                         :: self
      class(config), target, intent(in)      :: cfg
      character(len=*), intent(in)           :: fdata
      character(len=*), intent(in), optional :: name
      integer :: imatrix,ivector

      ! Construct the parent object
      self%multimatrix=multimatrix(cfg=cfg,fdata=fdata,name=name)

      ! Set the vector indices
      do ivector=1,self%nvector
         select case (trim(self%vectors(ivector)%name))
            case ('hidden1_bias')
               self%ivec_hid1_bias=ivector
            case ('hidden2_bias')
               self%ivec_hid2_bias=ivector
            case ('output_bias')
               self%ivec_outp_bias=ivector
            case ('x_scale')
               self%ivec_x_scale=ivector
            case ('y_scale')
               self%ivec_y_scale=ivector
            case ('x_shift')
               self%ivec_x_shift=ivector
            case ('y_shift')
               self%ivec_y_shift=ivector
         end select
      end do
      if(                                                                             &
          max(                                                                        &
              self%ivec_hid1_bias,self%ivec_hid2_bias,self%ivec_outp_bias,            &
              self%ivec_x_scale,self%ivec_y_scale,self%ivec_x_shift,self%ivec_y_shift &
             )                                                                        & 
          .ne.                                                                        & 
          self%nvector                                                                &
         ) then
           call die('[aencoder constructor] Inconsistent number of vectors')
      end if

      ! Set the matrix indices
      do imatrix=1,self%nmatrix
         select case (trim(self%matrices(imatrix)%name))
          case ('hidden1_weight')
            self%imat_hid1_weight=imatrix
          case ('hidden2_weight')
            self%imat_hid2_weight=imatrix
          case ('output_weight')
            self%imat_outp_weight=imatrix
          case ('project_weight')
            self%imat_proj_weight=imatrix
         end select
      end do
      if(                                                                                            &
         max(                                                                                        &
             self%imat_hid1_weight,self%imat_hid2_weight,self%imat_outp_weight,self%imat_proj_weight &
            )                                                                                        & 
         .ne.                                                                                        & 
         self%nmatrix                                                                                &
        ) then
          call die('[aencoder constructor] Inconsistent number of matrices')
      end if

      ! Allocate transposed matrices
      allocate(self%proj_weight_T(size(self%matrices(self%imat_proj_weight)%matrix,dim=2),size(self%matrices(self%imat_proj_weight)%matrix,dim=1)))
      allocate(self%hid1_weight_T(size(self%matrices(self%imat_hid1_weight)%matrix,dim=2),size(self%matrices(self%imat_hid1_weight)%matrix,dim=1)))
      allocate(self%hid2_weight_T(size(self%matrices(self%imat_hid2_weight)%matrix,dim=2),size(self%matrices(self%imat_hid2_weight)%matrix,dim=1)))
      allocate(self%outp_weight_T(size(self%matrices(self%imat_outp_weight)%matrix,dim=2),size(self%matrices(self%imat_outp_weight)%matrix,dim=1)))

      ! Get the transposed matrices
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
      real(WP), dimension(:), intent(in)    :: input
      real(WP), dimension(:), intent(inout) :: output
      output=ReLU(matmul(input ,this%hid1_weight_T)+this%vectors(this%ivec_hid1_bias)%vector)
      output=ReLU(matmul(output,this%hid2_weight_T)+this%vectors(this%ivec_hid2_bias)%vector)
      output=matmul(output,this%outp_weight_T)+this%vectors(this%ivec_outp_bias)%vector
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
