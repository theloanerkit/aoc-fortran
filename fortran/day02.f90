program day02
    use readfile, only : readlines
    use isadigit, only : is_char_a_digit 
    implicit none
    integer :: num_lines
    open(1,file="02f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)
    contains

    subroutine solve(num_lines)
        integer :: num_lines
        integer :: answer_one
        integer :: answer_two
        character(len=200) :: lines(num_lines)
        lines = readlines(num_lines,200)
        !print*,lines

        answer_one=part_one(lines,num_lines)
        answer_two=part_two(lines,num_lines)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two

    end subroutine solve

    function part_one(lines, num_lines) result (answer_one)
        integer :: answer_one
        integer :: num_lines
        character(len=200) :: lines(num_lines)
        integer :: gameid
        integer :: i
        answer_one = 0
        do i=1,num_lines
            gameid = game_one(lines(i))
            if (gameid.ne.0) then
                answer_one = answer_one + gameid
            end if
        end do
        
    end function part_one

    function part_two(lines, num_lines) result (answer_two)
        integer :: answer_two
        integer :: num_lines
        character(len=200) :: lines(num_lines)
        integer :: i
        integer :: draws
        integer :: answer
        integer :: start_index
        answer_two = 0
        do i=1,num_lines
            draws = get_draws(lines(i))+1
            start_index = get_start_index(lines(i))
            answer = build_game(lines(i),draws,start_index,2)
            answer_two = answer_two + answer
        end do
    end function part_two

    function game_one(line) result (gameid)
        integer :: gameid
        character(len=200) :: line
        integer :: draws
        integer :: start_index
        character :: char
        integer :: place
        integer :: digit
        integer :: i
        integer :: answer
        gameid=0
        place = 0
        draws = get_draws(line) + 1
        start_index = get_start_index(line)
        answer = build_game(line,draws,start_index,1)
        if (answer == 1) then
            do i=start_index,1,-1
                char = line(i:i)
                if (is_char_a_digit(char)) then
                    read(char,'(i1)') digit
                    gameid = gameid + (digit * (10**place))
                    place = place+1
                end if
            end do
        end if
    end function game_one

    function valid_game(game, draws) result (valid)
        logical :: valid
        integer :: draws
        character(len=10) :: game(draws)
        !character :: char
        integer :: i!,j
        integer :: red = 12
        integer :: green = 13
        integer :: blue = 14
        integer :: amount(draws)
        character :: colour(draws)
        integer :: valid_draws
        valid_draws = 0
        amount = get_amount(game,draws)
        colour = get_char(game,draws)
        do i=1,draws
            if (colour(i)=="r") then
                if (amount(i).le.red) then
                    valid_draws = valid_draws + 1
                end if
            else if (colour(i)=="g") then
                if (amount(i).le.green) then
                    valid_draws = valid_draws + 1
                end if
            else if (colour(i)=="b") then
                if (amount(i).le.blue) then
                    valid_draws = valid_draws + 1
                end if 
            end if
        end do
        if (valid_draws==draws) then
            valid = .TRUE.
        else 
            valid = .FALSE.
        end if
    end function valid_game

    function get_amount(game, draws) result (amount)
        integer :: draws
        character(len=10) :: game(draws)
        integer :: amount(draws)
        integer :: i,j
        character :: char
        integer :: digit
        amount = 0
        do i=1,draws
            do j=1,10
                char = game(i)(j:j)
                if (is_char_a_digit(char)) then
                    read(char,'(i1)') digit
                    amount(i) = amount(i) * 10
                    amount(i) = amount(i) + digit
                    digit = 0
                end if
            end do
        end do
    end function get_amount

    function get_char(game, draws) result (chars)
        integer :: draws
        character(len=10) :: game(draws)
        character :: chars(draws)
        integer :: i,j
        character :: char
        character :: colour
        char = ""
        colour = " "
        do i=1,draws
            do j=1,10
                char = game(i)(j:j)
                if (.not.is_char_a_digit(char).and.char.ne." ".and.colour==" ") then
                    colour = char
                end if
            end do
            chars(i) = colour
            colour = " "
        end do
    end function get_char

    function build_game(line,draws,start_index,part) result (answer)
        character(len=200) :: line
        integer :: draws
        integer :: start_index
        character(len=10) :: game(draws)
        character(len=10) :: temp
        integer :: i
        character :: char
        integer :: stringidx
        integer :: arr_idx
        logical :: valid
        integer :: part
        integer :: answer
        stringidx=1
        arr_idx=1
        temp=""
        do i=1,draws
            game(i)=""
        end do
        do i=start_index+1,200
            char = line(i:i)
            if (is_char_a_digit(char)) then ! if character is a digit
                temp(stringidx:stringidx) = char ! set current character to the digit
                stringidx = stringidx + 1 ! increment the string index
            else if (char==",".or.char==";") then ! if character is a , or a ;
                game(arr_idx) = temp ! add the temporary string to the array
                temp = "" ! reset the temporary string
                arr_idx = arr_idx + 1 ! increment the array index
                stringidx = 1 ! reset the string index to 1
            else if (char==" ".and.line(i-1:i-1).ne." ") then ! if current character is a space, and the one before is not
                temp(stringidx:stringidx) = char ! add character to string
                stringidx = stringidx+1 ! increment the string index
            else 
                if (stringidx <= 10) then
                    temp(stringidx:stringidx) = char
                    stringidx = stringidx + 1
                end if
            end if
        end do
        game(arr_idx) = temp
        if (part==1) then
            valid = valid_game(game, draws)
            if (valid) then
                answer = 1
            else
                answer = 0
            end if
        else if (part==2) then
            answer = how_many_cubes(game, draws)
        end if
    end function build_game

    function how_many_cubes(game, draws) result (cubes)
        integer :: cubes
        integer :: draws
        character(len=10) :: game(draws)
        integer :: i
        integer :: red
        integer :: green
        integer :: blue
        integer :: amount(draws)
        character :: colour(draws)
        red = 0 
        green = 0
        blue = 0
        amount = get_amount(game,draws)
        colour = get_char(game,draws)
        do i=1,draws
            print*,colour(i),amount(i)
            if (colour(i)=="r") then
                if (amount(i)>red) then
                    red = amount(i)
                end if
            else if (colour(i)=="g") then
                if (amount(i)>green) then
                    green = amount(i)
                end if
            else if (colour(i)=="b") then
                if (amount(i)>blue) then
                    blue = amount(i)
                end if
            end if
        end do
        cubes = red * blue * green
    end function

    function get_start_index(line) result (index)
        integer :: index
        character(len=200) :: line
        integer :: j
        character :: char
        j=1
        index = -1
        do while (index == -1)
            char = line(j:j)
            if (char==":") then
                index = j
            end if
            j = j+1
        end do
    end function get_start_index

    function get_draws(line) result (draws)
        integer :: draws
        character(len=200) :: line
        character :: char
        integer :: i
        draws = 0
        do i=1,200
            char = line(i:i)
            if (char==",".or.char==";") then
                draws = draws + 1
            end if
        end do
    end function get_draws
end program day02