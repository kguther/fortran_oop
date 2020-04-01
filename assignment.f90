module resources
    implicit none
    ! This is a type that manages a resource
    ! Since the default allocation already does a good job, this
    ! has more exotic applications

    type :: resource_manager_t
        ! For example, manage an integer
        integer, pointer :: resource => null()

    contains
        procedure :: get

        ! A final routine is called when the object goes out of scope
        final :: destructor
    end type resource_manager_t

    ! Introduce a function to create objects by name
    interface resource_manager_t
        module procedure factory
    end interface resource_manager_t

    ! If a final routine is required, we almost always need to override the copy
    ! assignment
    interface assignment(=)
        module procedure copy
    end interface assignment(=)

contains

    function factory(val) result(new_obj)
        ! Create a new resource manager and assign val to its resource
        integer, intent(in) :: val
        type(resource_manager_t) :: new_obj

        allocate(new_obj%resource, source = val)
    end function factory

    ! A copy assignment has to interface a subroutine that looks like this
    subroutine copy(new_obj, old_obj)
        ! Two (and only two) arguments are required: the rhs and the lhs of the assignment
        class(resource_manager_t), intent(inout) :: new_obj        
        class(resource_manager_t), intent(in) :: old_obj

        ! make sure the target has the resource allocated
        if(.not. associated(new_obj%resource)) allocate(new_obj%resource)
        ! assign the content
        new_obj%resource = old_obj%resource
    end subroutine copy

    !------------------------------------------------------------------------------------!
    ! Type-bound procedures
    !------------------------------------------------------------------------------------!    

    function get(this) result(val)
        class(resource_manager_t) :: this
        integer :: val

        val = this%resource
    end function get
        
    subroutine destructor(this)
        ! A final subroutine is never called with a polymorphic argument
        type(resource_manager_t) :: this
        ! When going out of scope, deallocate the resource
        if(associated(this%resource)) deallocate(this%resource)
    end subroutine destructor

end module resources

program construction
    use resources
    implicit none
    type(resource_manager_t) :: resource_manager

    ! We got a problem here: Assigning a pointer to a temporary
    resource_manager = resource_manager_t(5)

    ! The resource is now some junk
    print *, "The managed resource is", resource_manager%get()
end program construction
