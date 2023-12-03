program day01
    implicit none
    integer :: num_lines
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
        answer_two=part_two(lines,num_lines)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
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

    function part_two(lines,num_lines) result (answer_two)
        integer :: answer_two
        integer :: num_lines
        integer :: i
        character(len=100) :: lines(num_lines)
        character(len=100) :: newline
        answer_two = 0
        do i=1,num_lines
            newline = words_to_numbers(lines(i))
            answer_two = answer_two + two_digit_number(newline)
        end do
    end function part_two

    function words_to_numbers(line) result (newline)
        character(len=100) :: line
        character(len=100) :: newline
        character(len=3) :: three
        character(len=4) :: four
        character(len=5) :: five
        integer :: str_length = 100
        integer :: i

        ! check 3 character numbers
        do i=1,str_length-2
            three = line(i:i+2)
            if (three=="one") then
                line(i:i+2) = "o1e"
            else if (three=="two") then
                line(i:i+2) = "t2o"
            else if (three=="six") then
                line(i:i+2) = "s6x"
            end if
        end do

        ! check 4 character numbers
        do i=1,str_length-3
            four = line(i:i+3)
            if (four=="four") then
                line(i:i+3) = "fo4r"
            else if (four=="five") then
                line(i:i+3) = "fi5e"
            else if (four=="nine") then
                line(i:i+3) = "ni9e"
            end if
        end do

        ! check 5 character numbers
        do i=1,str_length-4
            five = line(i:i+4)
            if (five=="three") then
                line(i:i+4) = "thr3e"
            else if (five=="seven") then
                line(i:i+4) = "sev7n"
            else if (five=="eight") then
                line(i:i+4) = "eig8t"
            end if
        end do
        newline=line
    end function words_to_numbers

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