module simple_override
    ! Extended types can override procedures - this is a simple example
    implicit none

    ! Good practice: Everything defaults to private
    private
    public :: base, child

    ! Our base type consists of two funcions
    type :: base
        integer :: io_stream = 6
    contains
        ! One that can be overriden
        procedure :: do_io
        ! And one that cannot
        procedure, non_overridable :: do_base
    end type base

    ! The child type is an extension that uses another do_io procedure
    type, extends(base) :: child
    contains
        ! To override, specify the same binding and different procedure name
        ! The signature and the pass attribute must be the same
        procedure :: do_io => do_io_child
        ! This yields a compiler error: do_base cannot be overriden
        ! procedure :: do_base => do_io_child
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

    subroutine do_base(this)
        class(base) :: this

        write(this%io_stream,*) "This is called from all instances"
    end subroutine do_base

    !--------------------------------------------------------------------!
    ! Derived class functions
    !--------------------------------------------------------------------!    

    subroutine do_io_child(this)
        class(child) :: this

        write(this%io_stream,*) "Calling do_io_child"
    end subroutine do_io_child
end module simple_override

program override
    use simple_override
    implicit none

    type(base) :: base_object
    type(child) :: child_object

    ! Compare base/child do_base() output
    call base_object%do_base()
    call child_object%do_base()

    ! Compare base/child do_io() output
    call base_object%do_io()
    call child_object%do_io()

    ! We can also call the base classes functions from a child object
    call child_object%base%do_io()
end program override
