module resources
    implicit none
    ! This is a type that manages a resource
    ! Since the default allocation already does a good job, this
    ! has more exotic applications

    type :: resource_manager_t
        ! For example, manage an integer
        private
        integer, allocatable :: resource

    contains
        procedure :: get
    end type resource_manager_t

    ! Introduce a function to create objects by name
    interface resource_manager_t
        module procedure factory
    end interface resource_manager_t

contains

    function factory(val) result(new_obj)
        ! Create a new resource manager and assign val to its resource
        integer, intent(in) :: val
        type(resource_manager_t) :: new_obj

        allocate(new_obj%resource, source = val)
    end function factory

    !------------------------------------------------------------------------------------!
    ! Type-bound procedures
    !------------------------------------------------------------------------------------!

    function get(this) result(val)
        class(resource_manager_t) :: this
        integer :: val

        val = this%resource
    end function get    
    
end module resources

program construction
    use resources
    implicit none
    type(resource_manager_t) :: resource_manager

    resource_manager = resource_manager_t(5)

    print *, "The managed resource is", resource_manager%get()    
end program construction
