!> Transport properties network class:
module trnsportnet_class
   use precision,         only: WP
   use config_class,      only: config
   use multimatrix_class, only: multimatrix
   use mathtools,         only: ReLU
   implicit none
   private

   public :: trnsportnet


   !> Transport properties network object definition
   type, extends(multimatrix) :: trnsportnet

      ! Indices of vectors
      integer :: ivec_lay0_bias,ivec_lay1_bias,ivec_lay2_bias,ivec_outp_bias              !< Bias
      integer :: ivec_y_scale                                              !< Scale
      integer :: ivec_y_shift                                              !< Shift

      ! Indices of matrices
      integer :: imat_lay0_weight,imat_lay1_weight,imat_lay2_weight,imat_outp_weight      !< Weight

      ! Transposed matrices
      real(WP), dimension(:,:), allocatable :: lay0_weight_T,lay1_weight_T,lay2_weight_T
      real(WP), dimension(:,:), allocatable :: outp_weight_T

      contains

      procedure :: get_transport                                           !< Get the transport properties of the fluid
      procedure :: inverse_transform_outputs                               !< Inverse transform outputs

   end type trnsportnet


   !> Declare trnsportnet constructor
   interface trnsportnet
      procedure constructor
   end interface trnsportnet


contains


   !> Default constructor for trnsportnet
   function constructor(cfg,fdata,name) result(self)
      use messager, only: die
      implicit none
      type(trnsportnet)                      :: self
      class(config), target, intent(in)      :: cfg
      character(len=*), intent(in)           :: fdata
      character(len=*), intent(in), optional :: name
      integer :: ivector,imatrix

      ! Construct the parent object
      self%multimatrix=multimatrix(cfg=cfg,fdata='networks/'//fdata,name=name)

      ! Set the vector indices
      do ivector=1,self%nvector
         select case (trim(self%vectors(ivector)%name))
            case ('layers_0_bias')
               self%ivec_lay0_bias=ivector
            case ('layers_1_bias')
               self%ivec_lay1_bias=ivector
            case ('layers_2_bias')
               self%ivec_lay2_bias=ivector
            case ('output_bias')
               self%ivec_outp_bias=ivector
            case ('y_shift')
               self%ivec_y_shift=ivector
            case ('y_scale')
               self%ivec_y_scale=ivector
         end select
      end do
      if(                                                                                    &
         max(                                                                                &
             self%ivec_lay0_bias,self%ivec_lay1_bias,self%ivec_lay2_bias,self%ivec_outp_bias,self%ivec_y_scale,self%ivec_y_shift  &
            
             )                                                                                & 
         .ne.                                                                                & 
         self%nvector                                                                        &
        ) then
          call die('[trnsportnet constructor] Inconsistent number of vectors')
      end if

      ! Set the matrix indices
      do imatrix=1,self%nmatrix
         select case (trim(self%matrices(imatrix)%name))
         case ('layers_0_weight')
            self%imat_lay0_weight=imatrix
         case ('layers_1_weight')
            self%imat_lay1_weight=imatrix
         case ('layers_2_weight')
            self%imat_lay2_weight=imatrix
         case ('output_weight')
            self%imat_outp_weight=imatrix
         end select
      end do
      if(                                                                                            &
         max(                                                                                        &
             self%imat_lay0_weight,self%imat_lay1_weight,self%imat_lay2_weight,self%imat_outp_weight &
            )                                                                                        & 
         .ne.                                                                                        & 
         self%nmatrix                                                                                &
        ) then
          call die('[trnsportnet constructor] Inconsistent number of matrices')
      end if

      ! Allocate memory for the transposed matrices
      allocate(self%lay0_weight_T(size(self%matrices(self%imat_lay0_weight)%matrix,dim=2),size(self%matrices(self%imat_lay0_weight)%matrix,dim=1)))
      allocate(self%lay1_weight_T(size(self%matrices(self%imat_lay1_weight)%matrix,dim=2),size(self%matrices(self%imat_lay1_weight)%matrix,dim=1)))
      allocate(self%lay2_weight_T(size(self%matrices(self%imat_lay2_weight)%matrix,dim=2),size(self%matrices(self%imat_lay2_weight)%matrix,dim=1)))
      allocate(self%outp_weight_T(size(self%matrices(self%imat_outp_weight)%matrix,dim=2),size(self%matrices(self%imat_outp_weight)%matrix,dim=1)))

      ! Get the transposed matrices
      self%lay0_weight_T=transpose(self%matrices(self%imat_lay0_weight)%matrix)
      self%lay1_weight_T=transpose(self%matrices(self%imat_lay1_weight)%matrix)
      self%lay2_weight_T=transpose(self%matrices(self%imat_lay2_weight)%matrix)
      self%outp_weight_T=transpose(self%matrices(self%imat_outp_weight)%matrix)
   end function constructor


   !< Get the transport properties of the fluid
   subroutine get_transport(this,annvar,trnvar)
      implicit none
      class(trnsportnet), intent(inout)   :: this
      real(WP), dimension(:), intent(in)  :: annvar
      real(WP), dimension(:), intent(out) :: trnvar
      real(WP), dimension(:), allocatable :: tmparr

      allocate(tmparr(size(this%lay1_weight_T,dim=1)))
      tmparr=ReLU(matmul(annvar,this%lay0_weight_T)+this%vectors(this%ivec_lay0_bias)%vector)
      tmparr=ReLU(matmul(tmparr,this%lay1_weight_T)+this%vectors(this%ivec_lay1_bias)%vector)
      tmparr=ReLU(matmul(tmparr,this%lay2_weight_T)+this%vectors(this%ivec_lay2_bias)%vector)
      trnvar=     matmul(tmparr,this%outp_weight_T)+this%vectors(this%ivec_outp_bias)%vector
      deallocate(tmparr)
   end subroutine get_transport


   !> Inverse transform outputs to match the actual variables
   subroutine inverse_transform_outputs(this,yraw,ytrf)
      implicit none
      class(trnsportnet), intent(inout)   :: this
      real(WP), dimension(:), intent(in)  :: yraw
      real(WP), dimension(:), intent(out) :: ytrf
      
      ytrf=yraw*this%vectors(this%ivec_y_scale)%vector+this%vectors(this%ivec_y_shift)%vector
   end subroutine inverse_transform_outputs


end module trnsportnet_class
