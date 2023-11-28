!> Neural network class:
module nnetwork_class
   use multimatrix_class, only: multimatrix
   implicit none
   private

   public :: nnetwork


   !> Neural network object definition
   type, extends(multimatrix) :: nnetwork

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
   end function constructor


end module nnetwork_class
