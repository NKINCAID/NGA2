!> Multi matrix class:
module multimatrix_class
   use precision, only: WP
   use string,    only: str_medium
   implicit none
   private

   public :: multimatrix


   !> 1D multi array for the multi matrix
   type :: marr1D
      real(WP), dimension(:), allocatable :: vector
   end type marr1D

   !> 2D multi array for the multi matrix
   type :: marr2D
      real(WP), dimension(:,:), allocatable :: matrix
   end type marr2D


   !> Multi matrix object definition
   type :: multimatrix

      ! This is the name of the multi matrix
      character(len=str_medium) :: name='UNNAMED_MULTIMATRIX'           !< Multi matrix name (default=UNNAMED_MULTIMATRIX)

      ! Data file name
      character(len=str_medium) :: filename                             !< Name of datafile to read

      ! 1D arrays
      integer :: nvector                                                !< Number of vectors
      character(len=str_medium), dimension(:), allocatable :: vecnames  !< Names of vectors
      type(marr1D), dimension(:), allocatable :: vectors                !< 1D multi array contains entries of all the 1D arrays
      
      ! 2D arrays
      integer :: nmatrix                                                !< Number of matrices
      character(len=str_medium), dimension(:), allocatable :: matnames  !< Names of matrices
      type(marr2D), dimension(:), allocatable :: matrices               !< 2D multi array contains entries of all the 2D arrays

   contains
      procedure :: initialize                                           !< Initialization of the multi matrix
   end type multimatrix


contains


   !> Initialization of the multi matrix object
   subroutine initialize(this,fdata,name)
      use messager, only: die
      use parallel, only: info_mpiio,MPI_REAL_WP
      use mpi_f08
      implicit none
      class(multimatrix), intent(inout)        :: this
      character(len=*)  , intent(in)           :: fdata
      character(len=*)  , intent(in), optional :: name
      integer :: ierr,imatrix,ivector,n_row,n_col
      type(MPI_File) :: ifile
      type(MPI_Status) :: status

      ! Set the name for the multi matrix object
      if (present(name)) this%name=trim(adjustl(name))

      ! Open the file
      this%filename=trim(adjustl(fdata))
      call MPI_FILE_OPEN(this%cfg%comm,trim(this%filename),MPI_MODE_RDONLY,info_mpiio,ifile,ierr)
      if (ierr.ne.0) call die('[multimatrix constructor] Problem encountered while reading matrices file: '//trim(this%filename))

      ! Read the number of vectors
      call MPI_FILE_READ_ALL(ifile,this%nvector,1,MPI_INTEGER,status,ierr)
      if (this%nvector.le.0) call die('[multimatrix constructor] multimatrix object requires at least 1 vector')

      ! Allocate memory for vectors
      allocate(this%vecnames(1:this%nvector))
      allocate(this%vectors (1:this%nvector))

      ! Read the vectors
      do ivector=1,this%nvector
         call MPI_FILE_READ_ALL(ifile,this%vecnames(ivector),str_medium,MPI_CHARACTER,status,ierr)
         call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
         allocate(this%vectors(ivector)%vector(n_row))
         call MPI_FILE_READ_ALL(ifile,this%vectors(ivector)%vector,n_row,MPI_REAL_WP,status,ierr)
      end do

      ! Read the number of matrices
      call MPI_FILE_READ_ALL(ifile,this%nmatrix,1,MPI_INTEGER,status,ierr)
      if (this%nmatrix.le.0) call die('[multimatrix constructor] multimatrix object requires at least 1 matrix')

      ! Allocate memory for matrices
      allocate(this%matnames(1:this%nmatrix))
      allocate(this%matrices(1:this%nmatrix))

      ! Read the matrices
      do imatrix=1,this%nmatrix
         call MPI_FILE_READ_ALL(ifile,this%matnames(imatrix),str_medium,MPI_CHARACTER,status,ierr)
         call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
         call MPI_FILE_READ_ALL(ifile,n_col,1,MPI_INTEGER,status,ierr)
         allocate(this%matrices(imatrix)%matrix(n_row,n_col))
         call MPI_FILE_READ_ALL(ifile,this%matrices(imatrix)%matrix,n_row*n_col,MPI_REAL_WP,status,ierr)
      end do

      ! Close the file
      call MPI_FILE_CLOSE(ifile,ierr)
   end subroutine initialize


end module multimatrix_class
