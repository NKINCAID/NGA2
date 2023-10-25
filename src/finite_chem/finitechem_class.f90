!> Finite chem class
!> Extends multiscalar class for finite chem related functions
module finitechem_class
    use multivdscalar_class, only: multivdscalar
    use config_class,      only: config
    use precision,         only: WP
    use fc_mech
    implicit none
    private
    
    ! Expose type/constructor/methods
    public :: fc
    
  ! Clipping values for temperature
    real(WP), parameter :: T_min = 50.0_WP
    real(WP), parameter :: Tmax = 5000.0_WP
    
    !> Finite chemistry solver object definition
    type, extends(multivdscalar) :: fc
      real(WP) :: vol                  !< Volume
      real(WP) :: m                    !< Mass
      real(WP) :: h                    !< Enthalpy
      real(WP) :: T                    !< Temperature
      real(WP) :: Zmix                 !< Mixture fraction
      real(WP), dimension(nspec) :: Yi   !< Composition
    contains
       procedure :: react

      procedure :: get_density
      procedure :: get_viscosity
      procedure :: get_diffusivity    

      procedure :: get_Wmix
      procedure :: get_sv
      procedure :: get_thermodata
      procedure :: H2T
      procedure :: clip
    end type fc
    
    !> Declare fc model constructor
    interface fc
       procedure constructor
    end interface fc
    
    
 contains
    
    
    !> FC model constructor from multivdscalar
    function constructor(cfg,model,scheme,name) result(self)
       implicit none
       type(fene) :: self
       class(config), target, intent(in) :: cfg
       integer, intent(in) :: model
       integer, intent(in) :: scheme
       character(len=*), optional :: name
       ! Create a six-scalar solver for conformation tensor
       self%multiscalar=multiscalar(cfg=cfg,scheme=scheme,nscalar=6,name=name)


    end function construct_fc_from_args
    
   
 