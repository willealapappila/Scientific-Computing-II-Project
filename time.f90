module timemod
    implicit none
    integer:: years, days, hours, minutes
    real :: x

contains 

character(len=80) function convert_time(time)
    integer, intent(in):: time
    character(len=80) :: midtime
    !character(len=80), intent(inout) :: newtime
    if (time > 365*24*60) then
        years = time/60/24/365
        if ((time - years*365*24*60) > 24*60) then
        days = (time -years*365*24*60)/60/24
            if ((time - years*365*24*60 - days*24*60) > 60) then
                hours = (time - years*365*24*60 - days*24*60)/60
                if (time - years*365*24*60 - days*24*60 - hours*60 >= 1) then
                    minutes = time - years*365*24*60 - days*24*60 - hours*60
                    write (midtime, '(a, i0, a, i0, a, i0, a, i0, a)') 'Time: ', years, ' years, ', days, ' days, ',&
                    hours, ' hours, ', minutes, ' minutes, '
                else
                    write (midtime, '(a, i0, a, i0, a, i0, a)') 'Time: ', years, ' years, ' ,days, ' days, ', hours, ' hours'
                end if
            else if ((time - years*365*24*60 - days*24*60) >= 1) then
                minutes = time - years*365*24*60 - days*24*60
                write (midtime, '(a, i0, a, i0, a, i0, a)') 'Time: ', years, ' years, ' ,days, ' days, ', minutes, ' minutes'
            else
                write (midtime, '(a, i0, a, i0, a)') 'Time: ', years, ' years, ', days, 'days'
            end if
        else if ((time - years*365*24*60) > 60) then
            hours = (time - years*365*24*60)/60
            if (time - years*365*24*60 - hours*60 >= 1) then
                write (midtime, '(a, i0, a, i0, a, i0, a)') 'Time: ', years, ' years, ', hours, 'hours, ', minutes, ' minutes'
            else
                write (midtime, '(a, i0, a, i0, a)') 'Time: ', years, 'years, ', hours, 'hours'
            end if
        else if ((time - years*365*24*60 ) >= 1) then
            minutes = (time - years*365*24*60)
            write (midtime, '(a, i0, a, i0, a)') 'Time: ', years, ' years, ', minutes, ' minutes'
        else
            write (midtime, '(a, i0, a)') 'Time: ', years, ' years'
        end if
    else
        if (time - 24*60 >= 1) then
            days = time/(24*60)
            if ((time - days*24*60) >= 60) then
                hours = (time - days*24*60)/60
                if ((time - days*24*60 - hours*60) >=1) then
                    minutes = (time - days*24*60 - hours*60)
                    write (midtime, '(a, i0, a, i0, a, i0, a)') 'Time: ', days, ' days, ', hours, ' hours, ', minutes, ' minutes'
                else
                    write (midtime, '(a, i0, a, i0, a)') 'Time: ', days, ' days, ', hours, ' hours'
                end if
            else
                minutes = time - days*24*60
                write (midtime, '(a, i0, a, i0, a)') 'Time: ', days, ' days, ', minutes, ' minutes'
            end if
        else if (time - 60 >= 0) then
            hours = time/60
            if (time - hours*60 >= 1) then
                minutes = time - hours*60
                write (midtime, '(a, i0, a, i0, a)') 'Time: ', hours, ' hours, ', minutes, ' minutes'
            else
                write (midtime, '(a, i0, a)') 'Time: ', hours, ' hours'
            end if
        else
            write (midtime, '(a, i0, a)') 'Time: ', time, ' minutes'
        end if
    end if

    convert_time = midtime

end function convert_time

end module timemod