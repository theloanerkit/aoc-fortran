program day01
    use readfile, only : readlines
    use isadigit, only : get_numbers
    implicit none
    integer :: num_lines
    open(1,file="2018-01f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)

    contains

    subroutine solve(num_lines)
        integer :: answer_one
        integer :: answer_two
        integer :: num_lines
        character(len=8) :: lines(num_lines)

        lines=readlines(num_lines,8,1)

        answer_one = part_one(lines,num_lines)
        answer_two = part_two(lines,num_lines,answer_one)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(lines,num_lines) result (frequency)
        integer :: frequency
        integer :: num_lines
        integer :: i
        character(len=8) :: lines(num_lines)
        frequency = 0
        do i=1,num_lines
            frequency = change_frequency(frequency,lines(i))
        end do
    end function part_one

    function part_two(lines,num_lines,freq_increment) result (frequency_twice)
        integer :: frequency
        integer :: num_lines
        integer :: freq_increment
        integer :: i,j,x,y,k,idx
        character(len=8) :: lines(num_lines)
        integer :: prev_freqs(num_lines)
        integer :: frequency_twice
        integer :: shifts,newshifts
        ! frequency will be seen again if:
        ! x = y + frequency_increment*shifts
        ! both x and y must be previously seen frequencies
        ! we need to find y and shifts
        shifts = -1
        idx = -1
        prev_freqs = 0
        frequency_twice = 0
        frequency = 0
        do i=1,num_lines
            frequency = change_frequency(frequency,lines(i))
            prev_freqs(i) = frequency
        end do
        do i=1,num_lines
            x = prev_freqs(i)
            y = prev_freqs(i)
            newshifts = 0
            do while (y.gt.freq_increment)
                y = y - freq_increment
                newshifts = newshifts + 1
                k = -1
                do j=1,num_lines
                    if (prev_freqs(j).eq.y) then
                        k=j
                    end if
                end do
                if (k.ne.-1) then
                    if (newshifts.le.shifts.or.shifts.eq.-1) then
                        if (shifts.eq.-1) then
                            shifts = newshifts
                            frequency_twice = x
                            idx = k
                        else 
                            if (k.lt.idx) then
                                shifts = newshifts
                                frequency_twice = x
                                idx=k
                            end if
                        end if
                    end if
                end if
            end do
        end do
    end function part_two

    function change_frequency(oldfreq,line) result (freq)
        integer :: freq, oldfreq
        character(len=8) :: line
        integer :: num(1)
        num = get_numbers(line,8,1)
        if (line(1:1).eq."+") then
            freq = oldfreq + num(1)
        else if (line(1:1).eq."-") then
            freq = oldfreq - num(1)
        end if
    end function change_frequency
end program day01