!> Multi matrix class:
module multimatrix_class
   use precision,    only: WP
   use string,       only: str_medium
   use config_class, only: config
   implicit none
   private

   public :: multimatrix


   !> 1D multi array for the multi matrix
   type :: marr1D
      real(WP), dimension(:), allocatable :: vector
      character(:), allocatable :: name
   end type marr1D

   !> 2D multi array for the multi matrix
   type :: marr2D
      real(WP), dimension(:,:), allocatable :: matrix
      character(:), allocatable :: name
      end type marr2D


   !> Multi matrix object definition
   type :: multimatrix

      ! This is our config
      class(config), pointer :: cfg                                     !< This is the config the solver is build for

      ! This is the name of the multi matrix
      character(len=str_medium) :: name='UNNAMED_MULTIMATRIX'           !< Multi matrix name (default=UNNAMED_MULTIMATRIX)

      ! Data file name
      character(len=str_medium) :: filename                             !< Name of datafile to read

      ! 1D arrays
      integer :: nvector                                                !< Number of vectors
      type(marr1D), dimension(:), allocatable :: vectors                !< 1D multi array contains entries of all the 1D arrays
      
      ! 2D arrays
      integer :: nmatrix                                                !< Number of matrices
      type(marr2D), dimension(:), allocatable :: matrices               !< 2D multi array contains entries of all the 2D arrays

   contains
      procedure :: print=>multimatrix_print                             !< Output solver to the screen
   end type multimatrix


   !> Declare multimatrix constructor
   interface multimatrix
      procedure constructor
   end interface multimatrix


contains


   !> Default constructor for multimatrix
   function constructor(cfg,fdata,name) result(self)
      use messager, only: die
      use parallel, only: info_mpiio,MPI_REAL_WP
      use mpi_f08
      implicit none
      type(multimatrix)                        :: self
      class(config), target, intent(in)        :: cfg
      character(len=*)  , intent(in)           :: fdata
      character(len=*)  , intent(in), optional :: name
      integer :: ierr
      type(MPI_File) :: ifile
      type(MPI_Status) :: status
      integer :: imatrix,ivector,n_row,n_col,n_char,i

      ! Point to pgrid object
      self%cfg=>cfg

      ! Set the name for the multi matrix object
      if (present(name)) self%name=trim(adjustl(name))

      ! Set the file name
      self%filename=trim(adjustl(fdata))

      ! Open the file
      call MPI_FILE_OPEN(self%cfg%comm,trim(self%filename),MPI_MODE_RDONLY,info_mpiio,ifile,ierr)
      if (ierr.ne.0) call die('[multimatrix constructor] Problem encountered while reading file: '//trim(self%filename))

      ! Read the number of vectors
      call MPI_FILE_READ_ALL(ifile,self%nvector,1,MPI_INTEGER,status,ierr)
      if (self%nvector.le.0) call die('[multimatrix constructor] multimatrix object requires at least 1 vector')

      ! Allocate memory for vectors
      allocate(self%vectors(1:self%nvector))

      ! Read the vector data
      do ivector=1,self%nvector
         ! Name
         call MPI_FILE_READ_ALL(ifile,n_char,1,MPI_INTEGER,status,ierr)
         allocate(character(n_char) :: self%vectors(ivector)%name)
         call MPI_FILE_READ_ALL(ifile,self%vectors(ivector)%name,n_char,MPI_CHARACTER,status,ierr)
         ! Size
         call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
         allocate(self%vectors(ivector)%vector(n_row))
         ! Entries
         call MPI_FILE_READ_ALL(ifile,self%vectors(ivector)%vector,n_row,MPI_REAL_WP,status,ierr)
      end do

      ! Read the number of matrices
      call MPI_FILE_READ_ALL(ifile,self%nmatrix,1,MPI_INTEGER,status,ierr)
      if (self%nmatrix.le.0) call die('[multimatrix constructor] multimatrix object requires at least 1 matrix')

      ! Allocate memory for matrices
      allocate(self%matrices(1:self%nmatrix))

      ! Read the matrix data
      do imatrix=1,self%nmatrix
         ! Name
         call MPI_FILE_READ_ALL(ifile,n_char,1,MPI_INTEGER,status,ierr)
         allocate(character(n_char) :: self%matrices(imatrix)%name)
         call MPI_FILE_READ_ALL(ifile,self%matrices(imatrix)%name,n_char,MPI_CHARACTER,status,ierr)
         ! Size
         call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
         call MPI_FILE_READ_ALL(ifile,n_col,1,MPI_INTEGER,status,ierr)
         allocate(self%matrices(imatrix)%matrix(n_row,n_col))
         ! Entries
         call MPI_FILE_READ_ALL(ifile,self%matrices(imatrix)%matrix,n_row*n_col,MPI_REAL_WP,status,ierr)
      end do

      ! Close the file
      call MPI_FILE_CLOSE(ifile,ierr)
   end function constructor


   !> Print out info for multi matrix object
   subroutine multimatrix_print(this)
      use, intrinsic :: iso_fortran_env, only: output_unit
      implicit none
      class(multimatrix), intent(in) :: this
      
      ! Output
      if (this%cfg%amRoot) then
         write(output_unit,'("Multi matrix object [",a,"] containing [",i5,"] vectors and [",i5,"] matrices was read from file [",a,"].")') trim(this%name),this%nvector,this%nmatrix,trim(this%filename)
      end if
      
   end subroutine multimatrix_print


end module multimatrix_class
