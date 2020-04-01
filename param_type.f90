module points
    implicit none

    private
    public :: point_t

    type :: point_t(dim)
        ! This is a point. It has coordinates and a dimension
        private
        real, len :: dim
        real :: coords(dim)
    contains
        ! You can ask for its coordinates
        procedure, public :: get_coords => get_coords_cartesian
    end type point_t

contains

    function get_coords_cartesian(this) result(x)
        class(point_t(*)) :: this
        real :: x(this%dim)

        x = this%coords
    end function get_coords_cartesian
    
end module points

program param
    use points, only: point_t
    implicit none

    type(point_t(2)) :: cartesian_point
end program param
