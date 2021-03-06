module orbital
    implicit none

    ! by default, stuff in this module is not visible outside
    private
    ! only wave functions are visible outside (and only their public members)
    public :: orbital_t

    !------------------------------------------------------------------------------------!
    ! Orbital type
    !------------------------------------------------------------------------------------!    

    type orbital_t
        private
        integer :: orb_index
    contains
        ! An orbital has an index, return it
        procedure :: index
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
    public :: determinant_t
    
    !------------------------------------------------------------------------------------!
    ! Determinant type
    !------------------------------------------------------------------------------------!
    
    type :: determinant_t
        private

        type(orbital_t), allocatable :: orbitals(:)

    contains
        ! A determinant must be able to tell us about occupation
        procedure :: occ_orbs
        procedure :: is_occ
    end type determinant_t

contains

    !------------------------------------------------------------------------------------!
    ! Determinant methods
    !------------------------------------------------------------------------------------!

    subroutine occ_orbs(this, orbs)
        ! This is the signature of the occ_orbs function
        class(determinant_t), intent(in) :: this
        type(orbital_t), intent(out) :: orbs(:)

        ! Implementation goes here
    end subroutine occ_orbs

    function is_occ(this, orb) result(occ)
        ! This is the signature of the occ_orbs function
        class(determinant_t), intent(in) :: this
        type(orbital_t), intent(in) :: orb
        logical :: occ

        integer :: index

        ! Better: rely on the interfacing function
        index = orb%index()
        ! Further implementation
    end function is_occ

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

        type(determinant_t), allocatable :: basis_states(:)
        real, allocatable :: ci_amplitudes(:)

    contains
        ! A wave function must know the coefficient of a given basis state
        ! This is public outside the module (public is default, so not required)
        procedure :: get_coeff
    end type wave_function_t

contains

    !------------------------------------------------------------------------------------!
    ! Wave function methods
    !------------------------------------------------------------------------------------!

    function get_coeff(this, det) result(coeff)
        ! This is the signature of the get_coeff function
        class(wave_function_t), intent(in) :: this
        type(determinant_t), intent(in) :: det
        real :: coeff

        ! Implementation goes here
    end function get_coeff
    
end module wave_function
