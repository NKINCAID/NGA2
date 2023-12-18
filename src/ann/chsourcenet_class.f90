!> Chemical source terms network class:
module chsourcenet_class
   use precision,         only: WP
   use config_class,      only: config
   use multimatrix_class, only: multimatrix
   use mathtools,         only: ReLU
   implicit none
   private

   public :: chsourcenet


   !> Chemical source terms network object definition
   type, extends(multimatrix) :: chsourcenet

      ! Indices of vectors
      integer :: ivec_lay0_bias,ivec_lay1_bias,ivec_lay2_bias,ivec_outp_bias              !< Bias
      integer :: ivec_x_scale,ivec_y_scale                                                !< Scale
      integer :: ivec_x_shift,ivec_y_shift                                                !< Shift

      ! Indices of matrices
      integer :: imat_lay0_weight,imat_lay1_weight,imat_lay2_weight,imat_outp_weight      !< Weight

      ! Transposed matrices
      real(WP), dimension(:,:), allocatable :: lay0_weight_T,lay1_weight_T,lay2_weight_T
      real(WP), dimension(:,:), allocatable :: outp_weight_T

   contains

      procedure :: get_src                                                                !< Get the source terms of the ODEs

   end type chsourcenet


   !> Declare chsourcenet constructor
   interface chsourcenet
      procedure constructor
   end interface chsourcenet


contains


   !> Default constructor for chsourcenet
   function constructor(cfg,fdata,name) result(self)
      use messager, only: die
      implicit none
      type(chsourcenet)                      :: self
      class(config), target, intent(in)      :: cfg
      character(len=*), intent(in)           :: fdata
      character(len=*), intent(in), optional :: name
      integer :: ivector,imatrix

      ! Construct the parent object
      self%multimatrix=multimatrix(cfg=cfg,fdata=fdata,name=name)

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
      if(                                                                                     &
         max(                                                                                 &
             self%ivec_lay0_bias,self%ivec_lay1_bias,self%ivec_lay2_bias,self%ivec_outp_bias, &
             self%ivec_x_scale,self%ivec_y_scale,self%ivec_x_shift,self%ivec_y_shift          &
            )                                                                                 & 
         .ne.                                                                                 & 
         self%nvector                                                                         &
        ) then
          call die('[chsourcenet constructor] Inconsistent number of vectors')
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
          call die('[chsourcenet constructor] Inconsistent number of matrices')
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


   subroutine get_src(this,input,output)
      implicit none
      class(chsourcenet), intent(inout)     :: this
      real(WP), dimension(:), intent(in)    :: input
      real(WP), dimension(:), intent(inout) :: output
      output=ReLU(matmul(input ,this%lay0_weight_T)+this%vectors(this%ivec_lay0_bias)%vector)
      output=ReLU(matmul(output,this%lay1_weight_T)+this%vectors(this%ivec_lay1_bias)%vector)
      output=ReLU(matmul(output,this%lay2_weight_T)+this%vectors(this%ivec_lay2_bias)%vector)
      output=matmul(output,this%outp_weight_T)+this%vectors(this%ivec_outp_bias)%vector
   end subroutine get_src


end module chsourcenet_class
