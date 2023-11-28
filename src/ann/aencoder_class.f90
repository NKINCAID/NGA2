!> Auto encoder class:
module aencoder_class
   use multimatrix_class, only: multimatrix
   implicit none
   private

   public :: aencoder


   !> Auto encoder object definition
   type, extends(multimatrix) :: aencoder

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
   end function constructor


   !> Encode
   subroutine encode(this)
      implicit none
      class(aencoder), intent(inout) :: this
   end subroutine encode


   !> Decode
   subroutine decode(this)
      implicit none
      class(aencoder), intent(inout) :: this
   end subroutine decode


   !> Transform inputs
   subroutine transform_inputs(this)
      implicit none
      class(aencoder), intent(inout) :: this
   end subroutine transform_inputs


   !> Inverse transform outputs
   subroutine inverse_transform_outputs(this)
      implicit none
      class(aencoder), intent(inout) :: this
   end subroutine inverse_transform_outputs


end module aencoder_class
