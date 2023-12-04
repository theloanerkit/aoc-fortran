module readfile
    implicit none

    contains

    function readlines(num_lines,num_chars) result (lines)
        integer :: num_lines
        integer :: num_chars
        character(len=num_chars) :: line
        character(len=num_chars) :: lines(num_lines)
        integer :: i
        do i=1,num_lines
            read(1,'(a)') line
            lines(i) = line
        end do
    end function readlines
end module readfile