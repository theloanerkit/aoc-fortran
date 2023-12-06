module isadigit
    implicit none
    contains
    function is_char_a_digit(char) result (digit_or_not)
        character :: char
        logical :: digit_or_not
        if (char=="1".or.char=="2".or.char=="3".or.char=="4".or.char=="5".or.char=="6".or.char=="7".or.char=="8".or.&
            char=="9".or.char=="0") then
            digit_or_not = .TRUE.
        else
            digit_or_not = .FALSE.
        end if
    end function is_char_a_digit

    function how_many_numbers(line,length) result (count)
        integer :: count
        integer :: length
        character(len=length) :: line
        character :: char
        integer :: i
        logical :: in_a_num
        in_a_num = .FALSE.
        count = 0
        do i=1,length
            char = line(i:i)
            if (is_char_a_digit(char)) then
                in_a_num = .TRUE.
            else
                if (in_a_num) then
                    count = count + 1
                    in_a_num = .FALSE.
                end if
            end if
        end do
    end function how_many_numbers

    function get_numbers(line,length,nums_count) result (nums)
        integer :: length
        integer :: nums_count
        character(len=length) :: line
        integer :: nums(nums_count)
        character :: char
        integer :: current_num
        integer :: i
        integer :: digit
        integer :: arr_idx
        arr_idx = 1
        print*,"yo"
        current_num = 0
        do i=1,length
            char = line(i:i)
            if (is_char_a_digit(char)) then
                current_num = current_num * 10
                read(char,'(i1)') digit
                current_num = current_num + digit
            else if (current_num.ne.0) then
                nums(arr_idx) = current_num
                current_num = 0
                arr_idx = arr_idx + 1
            end if
        end do
        print*,nums
    end function get_numbers
end module isadigit