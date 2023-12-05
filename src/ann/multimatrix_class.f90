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
      procedure :: initialize                                           !< Initialization of the multi matrix
      procedure :: print=>multimatrix_print                             !< Output solver to the screen
   end type multimatrix


contains


   !> Initialization of the multi matrix object
   subroutine initialize(this,cfg,fdata,name)
      use messager, only: die
      use parallel, only: info_mpiio,MPI_REAL_WP
      use mpi_f08
      implicit none
      class(multimatrix), intent(inout)        :: this
      class(config), target, intent(in)        :: cfg
      character(len=*)  , intent(in)           :: fdata
      character(len=*)  , intent(in), optional :: name
      integer :: ierr
      type(MPI_File) :: ifile
      type(MPI_Status) :: status
      integer :: imatrix,ivector,n_row,n_col,n_char,i

      ! Point to pgrid object
      this%cfg=>cfg

      ! Set the name for the multi matrix object
      if (present(name)) this%name=trim(adjustl(name))

      ! Set the file name
      this%filename=trim(adjustl(fdata))

      ! Open the file
      call MPI_FILE_OPEN(this%cfg%comm,trim(this%filename),MPI_MODE_RDONLY,info_mpiio,ifile,ierr)
      if (ierr.ne.0) call die('[multimatrix constructor] Problem encountered while reading file: '//trim(this%filename))

      ! Read the number of vectors
      call MPI_FILE_READ_ALL(ifile,this%nvector,1,MPI_INTEGER,status,ierr)
      if (this%nvector.le.0) call die('[multimatrix constructor] multimatrix object requires at least 1 vector')

      ! Allocate memory for vectors
      allocate(this%vectors(1:this%nvector))

      ! Read the vector data
      do ivector=1,this%nvector
         ! Name
         call MPI_FILE_READ_ALL(ifile,n_char,1,MPI_INTEGER,status,ierr)
         allocate(character(n_char) :: this%vectors(ivector)%name)
         call MPI_FILE_READ_ALL(ifile,this%vectors(ivector)%name,n_char,MPI_CHARACTER,status,ierr)
         ! Size
         call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
         allocate(this%vectors(ivector)%vector(n_row))
         ! Entries
         call MPI_FILE_READ_ALL(ifile,this%vectors(ivector)%vector,n_row,MPI_REAL_WP,status,ierr)
      end do

      ! Read the number of matrices
      call MPI_FILE_READ_ALL(ifile,this%nmatrix,1,MPI_INTEGER,status,ierr)
      if (this%nmatrix.le.0) call die('[multimatrix constructor] multimatrix object requires at least 1 matrix')

      ! Allocate memory for matrices
      allocate(this%matrices(1:this%nmatrix))

      ! Read the matrix data
      do imatrix=1,this%nmatrix
         ! Name
         call MPI_FILE_READ_ALL(ifile,n_char,1,MPI_INTEGER,status,ierr)
         allocate(character(n_char) :: this%matrices(imatrix)%name)
         call MPI_FILE_READ_ALL(ifile,this%matrices(imatrix)%name,n_char,MPI_CHARACTER,status,ierr)
         ! Size
         call MPI_FILE_READ_ALL(ifile,n_row,1,MPI_INTEGER,status,ierr)
         call MPI_FILE_READ_ALL(ifile,n_col,1,MPI_INTEGER,status,ierr)
         allocate(this%matrices(imatrix)%matrix(n_row,n_col))
         ! Entries
         call MPI_FILE_READ_ALL(ifile,this%matrices(imatrix)%matrix,n_row*n_col,MPI_REAL_WP,status,ierr)
      end do

      ! Close the file
      call MPI_FILE_CLOSE(ifile,ierr)
   end subroutine initialize


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
