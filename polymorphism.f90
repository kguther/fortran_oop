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

    ! by default, stuff in this module is not visible outside
    private
    ! only wave functions and the basis states are visible outside (and only their public members)
    public :: determinant_t, csf_t

    !------------------------------------------------------------------------------------!
    ! Basis state type
    !------------------------------------------------------------------------------------!

    ! Implement two types of basis states: determinants and csfs
    type :: determinant_t
        private

        ! A determinant is uniquely given by a list of occ. orbs
        type(orbital_t), allocatable :: orbitals(:)       
    contains
        private
        procedure :: occ_orbs => occ_orbs_det
        procedure :: is_occ => is_occ_det
    end type determinant_t

    type, extends(determinant_t) :: csf_t
        private

        ! A csf is uniquely given by a list of occ orbs and a list of spin couplings
        integer, allocatable :: spin_coupling(:)
        ! orbitals are inherited
    contains
        private
        procedure :: occ_orbs => occ_orbs_csf
        procedure :: is_occ => is_occ_csf        
    end type csf_t
        
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

program polymorphism
    use basis_state
    implicit none

    ! A polymoprhic object (declared with class) can be either
    ! a) allocatable
    class(determinant_t), allocatable, target :: det
    ! b) a pointer
    class(determinant_t), pointer :: det_ptr    
    ! c) a dummy argument (see above)

    ! The type is determined at runtime, by
    ! a) allocation
    allocate(csf_t :: det)
    ! b) assignment
    det_ptr => det
    ! c) call of function/subroutine

    ! Intrinsic assignment to a non-allocatable polymorphic object is not allowed
    ! det_ptr = det
    ! This is allowed, since det is allocatable
    det = det_ptr    
end program polymorphism
