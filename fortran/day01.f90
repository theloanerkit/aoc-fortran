program day01
    implicit none
    integer :: num_lines
    integer :: answer
    open(1,file="01f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)

    contains

    subroutine solve(num_lines)
        integer :: answer_one,answer_two
        integer :: num_lines
        character(len=100) :: lines(num_lines)

        lines = read_file()

        answer_one=part_one(lines,num_lines)
        print*,"part one: "
        print*,answer_one
    end subroutine solve

    function read_file() result (lines)
        character(len=100) :: line
        character(len=100) :: lines(num_lines)
        integer :: i
        do i=1,num_lines
            read(1,*) line
            lines(i)=line
        end do
    end function read_file

    function part_one(lines,num_lines) result (answer_one)
        integer :: answer_one
        integer :: num_lines
        integer :: i
        character(len=100) :: lines(num_lines)
        answer_one=0
        do i=1,num_lines
            answer_one = answer_one + two_digit_number(lines(i))
        end do
    end function part_one

    function two_digit_number(line) result (number)
        integer :: number
        integer :: str_length = 100
        character(len=100) :: line
        integer :: digits
        integer :: i
        character :: char
        integer :: tens,units
        logical :: digit_or_not
        number = 0
        tens = 0
        units = 0
        digits = digits_in_string(line)
        ! only one digit in string
        if (digits==1) then
            do i=1,str_length
                char = line(i:i)
                digit_or_not = is_char_a_digit(char)
                if (digit_or_not) then
                    read(char,'(i1)') tens
                    read(char,'(i1)') units
                end if
            end do
        ! two digits in string
        else if (digits>1) then
            do i=1,str_length
                char = line(i:i)
                digit_or_not = is_char_a_digit(char)
                if (digit_or_not) then
                    read(char,'(i1)') tens
                    exit
                end if
            end do
            do i=str_length,1,-1
                char = line(i:i)
                digit_or_not = is_char_a_digit(char)
                if (digit_or_not) then
                    read(char,'(i1)') units
                    exit
                end if
            end do
        end if
        number = number + (10*tens) + units
    end function two_digit_number

    function digits_in_string(line) result (digit_count)
        integer :: digit_count
        integer :: str_length = 100
        character(len=100) :: line
        integer :: i
        character :: char
        logical :: digit_or_not

        digit_count = 0

        do i=1,str_length
            char = line(i:i)
            digit_or_not = is_char_a_digit(char)
            if (digit_or_not) then
                digit_count = digit_count + 1
            end if
        end do
    end function digits_in_string

    function is_char_a_digit(char) result (digit_or_not)
        character :: char
        logical :: digit_or_not
        if (char=="1".or.char=="2".or.char=="3".or.char=="4".or.char=="5".or.char=="6".or.char=="7".or.char=="8".or.&
            char=="9") then
            digit_or_not = .TRUE.
        else
            digit_or_not = .FALSE.
        end if
    end function is_char_a_digit
end program day01