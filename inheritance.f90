module simple_inheritance
    ! Types can be extended - the new type is inherited from the old one
    implicit none

    ! Good practice: Everything defaults to private
    private
    public :: base, child

    ! Our base type consists of one function
    type :: base
        integer :: io_stream = 6
    contains
        ! Do some io
        procedure :: do_io
    end type base

    ! The child type is an extension that adds another do_io procedure
    type, extends(base) :: child
        real :: timer = 0.0
    contains
        procedure :: do_io_child
    end type child

contains

    ! All subroutines just print an output to signal being called

    !--------------------------------------------------------------------!
    ! Base class functions
    !--------------------------------------------------------------------!

    subroutine do_io(this)
        ! Now we need 'class' instead of 'type' to catch child objects, too
        class(base) :: this

        write(this%io_stream,*) "Calling do_io"
    end subroutine do_io
    
    !--------------------------------------------------------------------!
    ! Derived class functions
    !--------------------------------------------------------------------!

    subroutine do_io_child(this)
        class(child) :: this

        write(this%io_stream,*) "Calling do_io_child"
    end subroutine do_io_child
end module simple_inheritance

program inherit
    use simple_inheritance
    implicit none

    type(base) :: base_object
    type(child) :: child_object

    ! Compare base/child do_io() output
    call base_object%do_io()
    call child_object%do_io()

    ! child has additional functionality
    call child_object%do_io_child()

    ! Both, the base and the child have an io_stream
    base_object%io_stream = 5
    child_object%io_stream = 5

    ! But only child has a timer
    child_object%timer = 1.0
    ! Compiler error
    ! base_object%timer = 0.0
end program inherit
