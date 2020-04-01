module simple_override
    ! Extended types can override procedures - this is a simple example
    implicit none

    ! Good practice: Everything defaults to private
    private
    public :: base, child

    ! Our base type consists of three funcions
    type :: base
        integer :: io_stream = 6
    contains
        ! One that can be overriden
        procedure :: do_io
        ! And one that cannot
        procedure, non_overridable :: do_base

        ! And an overridable, static function
        procedure, nopass :: static_io
    end type base

    ! The child type is an extension that uses another do_io procedure
    type, extends(base) :: child
    contains
        ! To override, specify the same binding and different procedure name
        ! The signature and the pass attribute must be the same        
        procedure :: do_io => do_io_child
        ! This yields a compiler error: do_base cannot be overriden
        ! procedure :: do_base => do_io_child

        ! We can also override static functions - the override has to have the same attribute
        procedure, nopass :: static_io => static_io_child
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

    subroutine static_io()
        ! This is a static function, this is not passed
        
        print *, "Calling static_io"
    end subroutine static_io

    !--------------------------------------------------------------------!
    ! Derived class functions
    !--------------------------------------------------------------------!    

    subroutine do_io_child(this)
        class(child) :: this

        write(this%io_stream,*) "Calling do_io_child"
    end subroutine do_io_child

    subroutine static_io_child()
        ! This is a static function, this is not passed
        
        print *, "Calling static_io_child"
    end subroutine static_io_child
    
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

    ! Compare basic/child static_io output
    call base_object%static_io()
    call child_object%static_io()
    
end program override
