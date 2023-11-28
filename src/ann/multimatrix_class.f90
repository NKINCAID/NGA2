!> Multi matrix class:
module multimatrix_class
   use precision, only: WP
   use string,    only: str_medium
   implicit none
   private

   public :: multimatrix


   !> 2D multi matrix for the multi matrix
   type :: mmat2d
      real(WP), dimension(:,:), allocatable :: matrix
   end type mmat2d

   !> Multi matrix object definition
   type :: multimatrix

      ! This is the name of the multi matrix
      character(len=str_medium) :: name='UNNAMED_MULTIMATRIX'          !< Multi matrix name (default=UNNAMED_MULTIMATRIX)

      ! Matrices file name
      character(len=str_medium) :: filename                            !< Name of datafile to read

      ! Matrices
      integer :: nmatrix                                               !< Number of matrices
      character(len=str_medium), dimension(:), allocatable :: matname  !< Names of matrices
      type(mmat2d), dimension(:), allocatable :: matrices              !< 2D multi matrix contains entries of all the matrices

   end type multimatrix

   !> Declare multi matrix constructor
   interface multimatrix
      procedure constructor
   end interface multimatrix

contains


   !> Default constructor for multi matrix object
   function constructor(fdata,name) result(self)
      use parallel, only: info_mpiio,MPI_REAL_WP
      use mpi_f08
      implicit none
      type(multimatrix) :: self
      character(len=*), intent(in) :: fdata
      character(len=*), optional :: name
      integer :: ierr,imat,n_row,n_col
      type(MPI_File) :: ifile
      type(MPI_Status) :: status

      ! Set the name for the multi matrix object
      if (present(name)) self%name=trim(adjustl(name))

      ! Open the input file
      self%filename=trim(adjustl(fdata))
      call MPI_FILE_OPEN(self%cfg%comm,trim(self%filename),MPI_MODE_RDONLY,info_mpiio,ifile,ierr)
      if (ierr.ne.0) call die('[multimatrix constructor] Problem encountered while reading matrices file: '//trim(self%filename))

      ! Read the number of matrices
      call MPI_FILE_READ_ALL(ifile,self%nmatrix,1,MPI_INTEGER,status,ierr)
      if (self%nmatrix.le.0) call die('[multimatrix constructor] multimatrix object requires at least 1 matrix')
      allocate(self%matname(1:self%nmatrix))
      allocate(self%matrices(1:self%nmatrix))

      ! Read the matrices
      do imat=1,self%nmatrix
         call MPI_FILE_READ_ALL(ifile,self%matname(imat),str_medium,MPI_CHARACTER,status,ierr)
         call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
         call MPI_FILE_READ_ALL(ifile,n_col,1,MPI_INTEGER,status,ierr)
         allocate(self%matrices(imat)%matrix(n_row,n_col))
         call MPI_FILE_READ_ALL(ifile,self%matrices(imat)%matrix,n_row*n_col,MPI_REAL_WP,status,ierr)
      end do
   end function constructor


end module multimatrix_class
