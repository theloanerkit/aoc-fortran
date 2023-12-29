module readfile
    implicit none

    contains

    function readlines(num_lines,num_chars,file) result (lines)
        integer :: num_lines
        integer :: num_chars
        integer :: file
        character(len=num_chars) :: line
        character(len=num_chars) :: lines(num_lines)
        integer :: i
        do i=1,num_lines
            read(file,'(a)') line
            lines(i) = line
        end do
    end function readlines

    function remove_whitespace(line,length) result (newline)
        integer :: length
        character(len=length) :: line
        character(len=length) :: newline
        integer :: i,j
        character :: char
        newline = ""
        j = 1
        do i=1,length
            char = line(i:i)
            if (char.ne." ") then
                newline(j:j) = char
                j = j+1
            end if
        end do
    end function remove_whitespace

    function separate_by_comma(line,length) result (array_vars)
        integer :: length
        character(len=length) :: line
        integer :: array_vars
        character :: char
        integer :: i
        array_vars = 1
        do i=1,length
            char = line(i:i)
            if (char.eq."`") then
                array_vars = array_vars + 1
            end if
        end do
    end function separate_by_comma

end module readfile