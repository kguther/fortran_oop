module points
    ! Lattice points module
    implicit none

    private
    public :: point_t

    type :: point_t(dim)
        ! This is a point. It has coordinates and a dimension
        private
        integer, len :: dim
        integer :: coords(dim)
    contains
        ! You can ask for its coordinates
        procedure, public :: get_coords => get_coords_cartesian
    end type point_t

contains

    function get_coords_cartesian(this) result(x)
        class(point_t(*)) :: this
        integer :: x(this%dim)

        x = this%coords
    end function get_coords_cartesian
    
end module points

module lattices
    use points, only: point_t
    implicit none

    private
    public :: square_lattice_t

    ! A lattice is somewhat a graph: It contains a number of nodes and edges.
    ! This is the information needed. Hence, require a function that, given two nodes,
    ! tells us if there is an edge. Nodes are represented as points in N-D space (for
    ! the sake of simpler adjacency functions and to demonstrate parametrized types)
    type, abstract :: lattice_t(dim)
        ! dimension of the lattice
        integer, len :: dim
        ! size of the lattice per dimension
        integer :: lengths(dim)

    contains
        procedure(adjacent_t), deferred :: adjacent
    end type lattice_t

    abstract interface
        function adjacent_t(this, p_1, p_2) result(adj)
            ! Input: this - instance of the lattice used to define adjacency
            !        p_1, p_2 - points to determine adjacency
            ! Ouput: adj - true if and only if p_1 and p_2 are adjacent in this lattice
            import :: lattice_t, point_t
            class(lattice_t(*)) :: this
            type(point_t(this%dim)), intent(in) :: p_1, p_2
            logical :: adj
        end function adjacent_t
    end interface    

    ! A square lattice - it defines which points are adjacent
    type, extends(lattice_t) :: square_lattice_t
    contains
        procedure :: adjacent => adjacent_square
    end type square_lattice_t

contains

    function adjacent_square(this, p_1, p_2) result(adj)
        ! Use the adjacent_t interface
        class(square_lattice_t(*)) :: this
        type(point_t(this%dim)), intent(in) :: p_1, p_2
        logical :: adj

        ! Coordinates of p_1 and p_2
        integer :: x_1(this%dim), x_2(this%dim)
        ! difference of x_1,x_2 (1-norm): |x_1 - x_2|_1
        integer :: diff
        
        x_1 = p_1%get_coords()
        x_2 = p_2%get_coords()

        diff = sum(abs(x_1 - x_2))

        ! Two sites are adjacent if and only if they are one apart in exactly one dimension
        if(diff == 1) then
            adj = .true.
        else
            adj = .false.
        end if
    end function adjacent_square
end module lattices

program build_lattice
end program build_lattice
