module orbital
    implicit none

    ! by default, stuff in this module is not visible outside
    private
    ! only wave functions and the basis states are visible outside (and only their public members)
    public :: orbital_t

    !------------------------------------------------------------------------------------!
    ! Orbital type
    !------------------------------------------------------------------------------------!    

    type orbital_t
        private
        integer :: orb_index
    contains
        ! An orbital has an index, return it
        procedure, public :: index
    end type orbital_t

contains
    
    !------------------------------------------------------------------------------------!
    ! Orbital methods
    !------------------------------------------------------------------------------------!

    function index(this) result(orb_index)
        class(orbital_t), intent(in) :: this
        integer :: orb_index

        ! This defines how the orbital class internally stores its orbitals
        orb_index = this%orb_index
    end function index

end module orbital

!------------------------------------------------------------------------------------!

module basis_state
    
    use orbital
    implicit none
    
    private
    public :: basis_state_t, determinant_t, csf_t

    !------------------------------------------------------------------------------------!
    ! Basis state type
    !------------------------------------------------------------------------------------!
    
    type, abstract :: basis_state_t
        private
    contains
        ! A determinant must be able to tell us about occupation

        ! The deferred procedures have to have an abstract interface
        ! It only has to be specified in the base class
        procedure(occ_orbs_t), deferred :: occ_orbs
        procedure(is_occ_t), deferred :: is_occ
    end type basis_state_t

    ! Implement two types of basis states: determinants and csfs
    type, extends(basis_state_t) :: determinant_t
        private

        ! A determinant is uniquely given by a list of occ. orbs
        type(orbital_t), allocatable :: orbitals(:)       
    contains
        procedure :: occ_orbs => occ_orbs_det
        procedure :: is_occ => is_occ_det
    end type determinant_t

    type, extends(basis_state_t) :: csf_t
        private

        ! A csf is uniquely given by a list of occ orbs and a list of spin couplings
        type(orbital_t), allocatable :: orbitals(:)
        integer, allocatable :: spin_coupling(:)
    contains
        procedure :: occ_orbs => occ_orbs_csf
        procedure :: is_occ => is_occ_csf        
    end type csf_t

    !------------------------------------------------------------------------------------!
    ! Interfaces of deferred functions
    !------------------------------------------------------------------------------------!    

    ! Deferred procedures require an interface
    abstract interface
        subroutine occ_orbs_t(this, orbs)
            import :: basis_state_t, orbital_t
            ! This is the interface of the occ_orbs function
            class(basis_state_t), intent(in) :: this
            type(orbital_t), intent(out) :: orbs(:)
        end subroutine occ_orbs_t

        function is_occ_t(this, orb) result(occ)
            import :: basis_state_t, orbital_t
            ! This is the signature of the occ_orbs function
            class(basis_state_t), intent(in) :: this
            type(orbital_t), intent(in) :: orb
            logical :: occ
        end function is_occ_t

    end interface
    
contains
    
    !------------------------------------------------------------------------------------!
    ! Determinant methods
    !------------------------------------------------------------------------------------!

    subroutine occ_orbs_det(this, orbs)
        ! This is the signature of the occ_orbs function
        class(determinant_t), intent(in) :: this
        type(orbital_t), intent(out) :: orbs(:)

        ! Implementation goes here
    end subroutine occ_orbs_det

    function is_occ_det(this, orb) result(occ)
        ! This is the signature of the occ_orbs function
        class(determinant_t), intent(in) :: this
        type(orbital_t), intent(in) :: orb
        logical :: occ

        ! Implementation goes here
    end function is_occ_det

    !------------------------------------------------------------------------------------!
    ! CSF methods
    !------------------------------------------------------------------------------------!    

    subroutine occ_orbs_csf(this, orbs)
        ! This is the signature of the occ_orbs function
        class(csf_t), intent(in) :: this
        type(orbital_t), intent(out) :: orbs(:)

        ! Implementation goes here
    end subroutine occ_orbs_csf

    function is_occ_csf(this, orb) result(occ)
        ! This is the signature of the occ_orbs function
        class(csf_t), intent(in) :: this
        type(orbital_t), intent(in) :: orb
        logical :: occ

        ! Implementation goes here
    end function is_occ_csf

end module basis_state

!------------------------------------------------------------------------------------!

module wave_function
    use basis_state
    implicit none

    private
    public :: wave_function_t

    !------------------------------------------------------------------------------------!
    ! Wave function type
    !------------------------------------------------------------------------------------!
    
    type :: wave_function_t
        ! by default, stuff in this class is not visible outside
        private

        class(basis_state_t), allocatable :: basis_states(:)
        real, allocatable :: ci_amplitudes(:)

    contains
        ! A wave function must know the coefficient of a given basis state
        ! This is public outside the module
        procedure :: get_coeff
    end type wave_function_t

contains

    !------------------------------------------------------------------------------------!
    ! Wave function methods
    !------------------------------------------------------------------------------------!

    function get_coeff(this, det) result(coeff)
        ! This is the signature of the get_coeff function
        class(wave_function_t), intent(in) :: this
        class(basis_state_t), intent(in) :: det
        real :: coeff

        ! Implementation goes here
    end function get_coeff    
        
end module 
