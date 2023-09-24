program sailor
    use coordinates
    use timemod
    implicit none
    real :: intime_probability, death_probability, ranvar = 0.66, drunk_probability, dis = 0
    real, allocatable :: distance(:), time(:)
    integer::i=0, t=0, number_of_in_time=0, number_of_deaths=0, number_of_drinks = 0
    integer :: k=1, dir1, dir2, dir3, stat1, stat2, stat3
    logical :: in_time
    type(position_vector):: position !position vector with x- and y-coordinates
    character(len=100) :: walk_n, print, saw

    call get_command_argument(1, walk_n, status = stat1)
    call get_command_argument(2, print, status = stat2)
    call get_command_argument(3, saw, status = stat3)


    !Error message if invalid command arguments
    if (stat1 /= 0 .or. stat2 /= 0 .or. stat3 /= 0) then
        print*, 'Error: Input command arguments, "number of walks (integer)", "print walks? (y/n)", "SAW or normal? (saw/normal)"'
    end if

    read(walk_n, '(i100)') k

    !allocate sizes of lists for iteration count
    allocate(distance(k), time(k))

    if (print == 'y') then
        open(unit = 1, file = 'output.dat', form = 'formatted', status = 'replace') !Open file for writing
    end if

    if (saw == 'normal') then !Regular random walk
        do i=1, k
            position = position_vector(0, 0)    !Reset position to (0,0) and time to 0 for each walk
            t = 0
            dis = 0
            do
                if (t >= 50*365*24*60) then !If the walk takes 50 years, exit loop and add to death counter
                    number_of_deaths = number_of_deaths + 1
                    exit
                end if
                if (position%x >= 10) then  !If the sailor gets to shore, exit loop
                    exit
                else                        !If time not out and not in shore, update position
                    position = update_position(position, x)
                    dis = dis + 0.1
                    t = t + 1
                    if (print == 'y') then
                        write(1, '(i0,a,i0)') position%x,' ',position%y !Write to file
                    end if
                end if
            end do

            if (t < 10*60 .and. position%x >= 10) then   !If finish in under 10 hours, add to 'in time' counter
                in_time = .true.
                number_of_in_time = number_of_in_time + 1
            end if
            distance(i) = dis
            time(i) = t
        end do
    else if (saw == 'saw') then      !SAW
        do i=1, k
            position = position_vector(0, 0)
            call update_grid(grid, position)
            grid = 0
            call available_test(position, grid, available)
            t = 0
            dis = 0
            do
                if (print == 'y') then  
                    write(1, '(i0,a,i0)') position%x,' ',position%y
                end if
                if (t >= 10*60) then     !If the walk takes over 10 hours, exit loop
                    number_of_drinks = number_of_drinks + 1
                    exit
                end if
                if (position%x >= 10) then  !If sailor gets to shore, exit loop
                    exit
                else       !Update position now with SAW conditions
                    call update_grid(grid, position)    !Mark that the sailor has been in current position
                    call available_test(position, grid, available)  !Check which squares next to current one are available
                    if (sum(available) == 4) then   !If sum(available) == 4, no squares are available
                        number_of_drinks = number_of_drinks + 1
                        exit
                    else if (sum(available) == 3) then  
                        position = update_position_saw(position, find(available, 0)) !if there is only one available direction, we know the direction right away  
                    else if(sum(available) == 2) then   !If two directions are available, find directions and then randomize between them
                        dir1 = find(available, 0) !find first available direction
                        available(dir1) = 1         !set component to one to find the next zero
                        dir2 = find(available, 0)   !find second zero component
                        ranvar = 0.001*igrnd(0,1000)  !randomize which one the sailor takes
                        if (ranvar < 0.5) then
                            position = update_position_saw(position, dir1)
                        else
                            position = update_position_saw(position, dir2)
                        end if
                    else if(sum(available) == 1) then   !Same as with two directions but now with one extra step
                        dir1 = find(available, 0) !find first available direction
                        available(dir1) = 1         !set component to one to find the next zero
                        dir2 = find(available, 0)   !find second zero component
                        available(dir2) = 1             !set component to one
                        dir3 = find(available, 0)    !find third zero
                        ranvar = 0.001*igrnd(0, 1000)  !randomize which one the sailor takes
                        if (ranvar <= 0.33) then
                            position = update_position_saw(position, dir1)
                        else if (ranvar > 0.33 .and. ranvar <= 0.66) then
                            position = update_position_saw(position, dir2)
                        else
                            position = update_position_saw(position, dir3)
                        end if
                    else
                        if (t == 0) then
                            position = update_position(position, x)
                        else
                            print*, 'Error in saw loop'     !Just to help with debugging
                        end if
                    end if
                    distance = distance + 0.1
                    t = t + 1 !Update time
                    dis = dis + 0.1
                end if
                time(i) = t
                distance(i) = dis
            end do

            if (t < 10*60 .and. position%x >= 10) then
                in_time = .true.
                number_of_in_time = number_of_in_time + 1
            end if
        end do
    else
        print*, 'ERROR'
    end if

    if (print == 'y') then
        close(1)
    end if

    intime_probability = (1.0*number_of_in_time/(1.0*k))
    death_probability = (1.0*number_of_deaths/(1.0*k))
    drunk_probability = (1.0*number_of_drinks/(1.0*k))

    print '(a, i0)', 'Number of simulations: ', k

    print '(40(''-''))'

    print '(a, f12.2, a)', 'Average distance walked: ', sum(distance)/k, 'km'
    print '(a, f12.2, a)', 'Minimum distance walked: ', minval(distance), 'km'
    print '(a, f12.2, a)', 'Maximum distance walked: ', maxval(distance), 'km'

    print '(a, a)', 'Average time: ', convert_time(nint(sum(time)/k))
    print '(a, a)', 'Minimum time: ', convert_time(nint(minval(time)))
    print '(a, a)', 'Maximum time: ', convert_time(nint(maxval(time)))

    print*,

    print '(a,f6.2,a)', 'Arrived in time: ', intime_probability*100, '%'
    if (saw == 'normal') then
        print '(a, f7.3,a)', 'Died: ', death_probability*100, '%'
    else
        print '(a,f6.2)', 'Probability of dead end: ', drunk_probability*100, '%'
    end if

end program