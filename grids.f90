module coordinates
    use movement
    implicit none
    integer:: grid(10*60+10, 2*10*60) = 0, available(4)=1, findable

    !Grid is for SAW with maximum travel time of 600 minutes
    !available() is a list the index of which represents direction: 1=right, 2=left, 3=forward, 4=backward

    !This module contains everything related to the grid that the sailor lives in.


contains

    !Saves the path of the sailor to the grid:
    !The matrix is full of zeros in the beginning and subroutine update_grid() changes the 
    !current position coordinate  to 1.

    subroutine update_grid(grid, position)
        type(position_vector), intent(in):: position
        integer, intent(inout) :: grid(10*60+10, 2*10*60)
        
        grid(10*60 + position%x, 10*60 - position%y) = 1    !The sailor starts from origin so grid(1,1) is actually
                                                            !(-599, 599) in our coordinate system

    end subroutine

    
    !subroutine available_test() sees if the four squares next to the current one are available,
    !i.e. if the sailor has already been there, available_test() returns a 1 for that index of available

    subroutine available_test(position, grid, available)
        type(position_vector), intent(in):: position
        integer, intent(in) :: grid(610, 1200)
        integer, dimension(4), intent(out):: available 
        
        available = 1 !we start from the assumption that there are no available squares
 
            if (grid(600 + position%x + 1, 600 - position%y) == 0) then !checks if can go right
                available(1) = 0
            end if

            if (grid(600 + position%x - 1, 600 - position%y) == 0) then !checks if can go left
                available(2) = 0
            end if

            if (grid(600 + position%x, 600 - position%y - 1) == 0) then !checks if can go forwards
                available(3) = 0
            end if

            if (grid(600 + position%x, 600 - position%y + 1) == 0) then !checks if can go backwards
                available(4) = 0
            end if

    end subroutine available_test

    !find() is a function that returns the index of the first values in a list for which the value is some
    !specified number findable

    integer function find(available, findable)
        integer, dimension(4), intent(in) :: available
        integer, intent(in) :: findable
        integer:: i

        do i=1, 4
            if (available(i) == findable) then
                find = i
                exit
            else if(i==4 .and. available(i) /= findable) then
                print*, 'No such value'
            end if
        end do
        
    end function find
    
end module coordinates 