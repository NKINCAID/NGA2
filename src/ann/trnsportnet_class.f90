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
      integer :: ivec_lay0_bias,ivec_lay1_bias,ivec_outp_bias       !< Bias
      integer :: ivec_y_scale                                       !< Scale
      integer :: ivec_y_shift                                       !< Shift

      ! Indices of matrices
      integer :: imat_lay0_weight,imat_lay1_weight,imat_outp_weight !< Weight

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
      self%multimatrix=multimatrix(cfg=cfg,fdata=fdata,name=name)

      ! Set the vector indices
      do ivector=1,self%nvector
         select case (trim(self%vectors(ivector)%name))
            case ('layers_0_bias')
               self%ivec_lay0_bias=ivector
            case ('layers_1_bias')
               self%ivec_lay1_bias=ivector
            case ('output_bias')
               self%ivec_outp_bias=ivector
            case ('y_scale')
               self%ivec_y_scale=ivector
            case ('y_shift')
               self%ivec_y_shift=ivector
         end select
      end do
      if(                                                                 &
         max(                                                             &
             self%ivec_lay0_bias,self%ivec_lay1_bias,self%ivec_outp_bias, &
             self%ivec_y_scale,self%ivec_y_shift                          &
            )                                                             & 
         .ne.                                                             & 
         self%nvector                                                     &
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
         case ('output_weight')
            self%imat_outp_weight=imatrix
         end select
      end do
      if(                                                                      &
         max(                                                                  &
             self%imat_lay0_weight,self%imat_lay1_weight,self%imat_outp_weight &
            )                                                                  & 
         .ne.                                                                  & 
         self%nmatrix                                                          &
        ) then
          call die('[trnsportnet constructor] Inconsistent number of matrices')
      end if
   end function constructor


end module trnsportnet_class
