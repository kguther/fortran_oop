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

program select_type
    use shape_types
    use line_types
    implicit none
    type(triangle), target :: my_triangle
    ! This is the polymorphic object whose type we want to identify
    class(shape), pointer :: my_shape

    my_shape => my_triangle
    ! Differentiate between polygons and round shapes
    ! the argument has to be polymorphic
    select type(my_shape)
    ! 'Class is' asks if the object is an extension of a type
    class is (polygon)
        print *, "This is a polygon"
    class is (round)
        print *, "This is a round shape"
    ! As in 'select case' there can be a default
    class default
        print *, "This is another shape"
    end select

    ! Differentiate between the possible subtypes (not subclasses)
    select type(my_shape)
    ! With 'type is', we ask for exact type match
    type is(polygon)
        print *, "This is an abstract polygon"
    type is(triangle)
        print *, "This is a triangle"        
        
    ! We can mix them in a single 'select type'
    class is(round)
        print *, "This is a round shape"
    ! A 'type is' match takes precedence (this does not depend on the order)
    class is(polygon)
        print *, "This is another polygon"
    ! It is always class default, regardless of what the checks are
    class default
        print *, "This is another shape"
    end select
end program select_type
