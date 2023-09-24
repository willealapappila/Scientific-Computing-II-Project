program testprog
    use coordinates
    implicit none
    integer:: gg(610, 1200) = 0

    gg(600, 598) = 1

    print*, available
    call available_test(position_vector(0,1), gg, available)

    print*, grid(602, 598)

    call update_grid(grid, position_vector(2,2))

    print*, grid(602, 598)

    print*, available

    available = 0

    call available_test(position_vector(0,1), gg, available)

    print*, available
    
end program testprog