program Greeter
    implicit none
    integer :: count
    
    count = 5
    call greet_times(count)
    
contains
    subroutine greet_times(n)
        integer, intent(in) :: n
        integer :: i
        do i = 1, n
            print *, "Hello, World!"
        end do
    end subroutine greet_times
end program Greeter

