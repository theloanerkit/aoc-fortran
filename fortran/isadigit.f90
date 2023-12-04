module isadigit
    implicit none
    contains
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
end module isadigit