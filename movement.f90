module movement
    use mtmod   !random number generator
    implicit none
    !This module contains the basic coordinate change functions for both the regular walk as well as the SAW.

    !Define a position vector type for easy coordinate maninpulation
    type:: position_vector 
        integer:: x
        integer:: y
    end type

contains

    !update_position() is a function that chooses a random direction and tells the new coordinate in that direction.
    !It's used for regular random walks in the main program.

    type(position_vector) function update_position(old_position, x)
        type(position_vector), intent(in) :: old_position
        real, intent(out) :: x

        x = 0.001*igrnd(0,1000) !Randomizes the chosen direction

        update_position = old_position  !First set the new position equal to the current one
 
        if (x < 0.25) then
            update_position%x = old_position%x + 1 !If x < 0.25, sailor goes to the right
        else if (x >= 0.25 .and. x < 0.5) then
            update_position%x = old_position%x - 1 !If x in [0.25,0.5[ sailor goes to the left
        else if (x >= 0.5 .and. x < 0.75) then
            update_position%y = old_position%y + 1 !If x in [0.5,0.75[ sailor goes forwards
        else
            update_position%y = old_position%y - 1 !If x >= 0.75, sailor goes to the right
        end if

    end function update_position


    !update_position_saw() is the same function as update_position() except it doesnt't randomize anything itself.
    !The randomisation in SAW is done in the main loop

    type(position_vector) function update_position_saw(old_position, chosen_direction)
        type(position_vector), intent(in) :: old_position
        integer, intent(in) :: chosen_direction

        update_position_saw = old_position
 
        if (chosen_direction == 1) then
            update_position_saw%x = old_position%x + 1
        else if (chosen_direction == 2) then
            update_position_saw%x = old_position%x - 1
        else if (chosen_direction == 3) then
            update_position_saw%y = old_position%y + 1
        else
            update_position_saw%y = old_position%y - 1
        end if

    end function update_position_saw


end module movement