!> Auto encoder network class:
module aencodernet_class
   use precision,         only: WP
   use config_class,      only: config
   use multimatrix_class, only: multimatrix
   use mathtools,         only: ReLU
   implicit none
   private

   public :: aencodernet


   !> Auto encoder network object definition
   type, extends(multimatrix) :: aencodernet

      ! Number of ANN variables
      integer :: nvar

      ! Indices of vectors
      integer :: ivec_spec_inds                                                      !< Indices of species used to construct reduced space
      integer :: ivec_hid1_bias,ivec_hid2_bias,ivec_outp_bias                        !< Bias
      integer :: ivec_x_scale,ivec_y_scale                                           !< Scale
      integer :: ivec_x_shift,ivec_y_shift                                           !< Shift

      ! Indices of matrices
      integer :: imat_hid1_weight,imat_hid2_weight,imat_outp_weight,imat_proj_weight !< Weight

      ! Transposed matrices
      real(WP), dimension(:,:), allocatable :: proj_weight_T
      real(WP), dimension(:,:), allocatable :: hid1_weight_T,hid2_weight_T
      real(WP), dimension(:,:), allocatable :: outp_weight_T

   contains

      procedure :: encode                                                            !< Encode from actual to ANN variables
      procedure :: decode                                                            !< Decode from ANN to actual variables
      procedure :: transform_inputs                                                  !< Transform inputs
      procedure :: inverse_transform_outputs                                         !< Inverse transform outputs

   end type aencodernet


   !> Declare aencodernet constructor
   interface aencodernet
      procedure constructor
   end interface aencodernet


contains


   !> Default constructor for aencodernet
   function constructor(cfg,fdata,name) result(self)
      use messager, only: die
      implicit none
      type(aencodernet)                      :: self
      class(config), target, intent(in)      :: cfg
      character(len=*), intent(in)           :: fdata
      character(len=*), intent(in), optional :: name
      integer :: imatrix,ivector

      ! Construct the parent object
      self%multimatrix=multimatrix(cfg=cfg,fdata=fdata,name=name)

      ! Set the vector indices
      do ivector=1,self%nvector
         select case (trim(self%vectors(ivector)%name))
            case ('spec_inds')
               self%ivec_spec_inds=ivector
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
              self%ivec_spec_inds,                                                    &
              self%ivec_hid1_bias,self%ivec_hid2_bias,self%ivec_outp_bias,            &
              self%ivec_x_scale,self%ivec_y_scale,self%ivec_x_shift,self%ivec_y_shift &
             )                                                                        & 
          .ne.                                                                        & 
          self%nvector                                                                &
         ) then
           call die('[aencodernet constructor] Inconsistent number of vectors')
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
          call die('[aencodernet constructor] Inconsistent number of matrices')
      end if

      ! Get the number of state variables
      self%nvar=size(self%matrices(self%imat_proj_weight)%matrix,dim=1)

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


   !> Map the actual variables to the ANN representation of variables
   subroutine encode(this,actvar,annvar)
      implicit none
      class(aencodernet), intent(inout)   :: this
      real(WP), dimension(:), intent(in)  :: actvar
      real(WP), dimension(:), intent(out) :: annvar

      annvar=matmul(actvar,this%proj_weight_T)
   end subroutine encode


   !> Map the ANN variables to the actual variables
   subroutine decode(this,annvar,actvar)
      implicit none
      class(aencodernet), intent(inout)   :: this
      real(WP), dimension(:), intent(in)  :: annvar
      real(WP), dimension(:), intent(out) :: actvar
      real(WP), dimension(:), allocatable :: tmparr

      allocate(tmparr(size(this%hid2_weight_T,dim=1)))
      tmparr=ReLU(matmul(annvar,this%hid1_weight_T)+this%vectors(this%ivec_hid1_bias)%vector)
      tmparr=ReLU(matmul(tmparr,this%hid2_weight_T)+this%vectors(this%ivec_hid2_bias)%vector)
      actvar=     matmul(tmparr,this%outp_weight_T)+this%vectors(this%ivec_outp_bias)%vector
      deallocate(tmparr)
   end subroutine decode


   !> Transform inputs to match the ANN variables
   subroutine transform_inputs(this,xraw,xtrf)
      implicit none
      class(aencodernet), intent(inout)   :: this
      real(WP), dimension(:), intent(in)  :: xraw
      real(WP), dimension(:), intent(out) :: xtrf

      xtrf=(xraw-this%vectors(this%ivec_x_shift)%vector)/this%vectors(this%ivec_x_scale)%vector
   end subroutine transform_inputs


   !> Inverse transform outputs to match the actual variables
   subroutine inverse_transform_outputs(this,yraw,ytrf,nargs)
      implicit none
      class(aencodernet), intent(inout)   :: this
      real(WP), dimension(:), intent(in)  :: yraw
      real(WP), dimension(:), intent(out) :: ytrf
      integer, intent(in) :: nargs

      ytrf=yraw(1:nargs)*this%vectors(this%ivec_y_scale)%vector(1:nargs)+this%vectors(this%ivec_y_shift)%vector(1:nargs)
   end subroutine inverse_transform_outputs


end module aencodernet_class
