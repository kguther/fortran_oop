module shape_types
    ! This is a bunch of shape types
    implicit none
    private
    public :: shape, square, circle, triangle, round, polygon

    ! Base type for shapes
    type :: shape
    end type shape

    ! Base type for polygons
    type, extends(shape) :: polygon
    end type polygon

    ! Square type
    type, extends(polygon) :: square
        integer :: size
    end type square

    ! Triangle type
    type, extends(polygon) :: triangle
    end type triangle

    ! Base type for round shapes
    type, extends(shape) :: round
    end type round

    ! Circle type
    type, extends(round) :: circle
    end type circle

end module shape_types

program assign_polymorphic
    use shape_types
    implicit none

    type(square) :: square_one, square_two

    square_two%size = 5
    call copy_shape(square_one, square_two)

    print *, "Square one is of size", square_one%size
    
contains

    subroutine copy_shape(a, b)
        class(shape), intent(inout) :: a
        class(shape), intent(in) :: b

        a = b
    end subroutine copy_shape
end program assign_polymorphic
