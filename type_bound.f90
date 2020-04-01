module geometry
    ! Here, we draw squares and circles
    implicit none

    ! This is the draw object. It can be called upon squares and circles to draw them
    type :: draw_t
        ! This is the io stream we send the output to - each draw object can have
        ! another stream associated
        integer :: io_stream = 6
    contains
        ! Type-bound procedures.
        ! The declaration works like this:
    !   procedure, (<attribute list optional>) :: <binding_name> (=> <procedure_name>)
        ! Where attribute list can contain pass, nopass, deferred, non_overridable, public
        ! and private. 
        procedure :: draw_circle
        procedure :: draw_square => draw_square_impl
        procedure, nopass :: static_draw
    end type draw_t

    type :: circle_t
        real :: radius = 0.0
    end type circle_t

    type :: square_t
        real :: length = 0.0
    end type square_t

contains

    ! Here goes the implementation of the draws
    subroutine draw_circle(this, circle)
        class(draw_t) :: this
        type(circle_t), intent(in) :: circle

        write(this%io_stream, *) "Drawing a circle of radius", circle%radius
    end subroutine draw_circle

    subroutine draw_square_impl(this, square)
        class(draw_t) :: this
        type(square_t), intent(in) :: square

        write(this%io_stream, *) "Drawing a square of length", square%length
    end subroutine draw_square_impl

    !------------------------------------------------------------------------!

    subroutine static_draw(number)
        ! This is nopass, it does not get passed the calling object as 'this'
        ! Works like a normal function, but is called from an object
        integer, intent(in) :: number

        print *, "Drawing to stdout the number", number
    end subroutine static_draw

end module geometry

program geo
    use geometry, only: circle_t, square_t, draw_t
    implicit none

    type(draw_t) :: drawer
    type(circle_t) :: circle = circle_t(1.0)
    type(square_t) :: square = square_t(2.0)

    call drawer%draw_circle(circle)
    call drawer%draw_square(square)

    call drawer%static_draw(5)
    
end program geo
