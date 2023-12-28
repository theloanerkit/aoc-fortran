program day01
    use isadigit, only : get_digit
    implicit none
    integer :: num_lines
    character(len=2014) :: line
    open(1,file="2017-01f.txt",status="old")
    read(1,*) num_lines
    read(1,*) line

    call solve(line)

    contains

    subroutine solve(line)
        integer :: answer_one
        integer :: answer_two
        character(len=2014) :: line

        answer_one = part_one(line)
        answer_two = part_two(line)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(line) result (captcha)
        integer :: captcha
        character(len=2014) :: line
        captcha = solve_captcha(line,1)
    end function part_one

    function part_two(line) result (captcha)
        integer :: captcha
        character(len=2014) :: line
        captcha = solve_captcha(line,2014/2)
    end function part_two

    function solve_captcha(line, next_idx) result (captcha)
        character(len=2014) :: line
        integer :: next_idx
        integer :: captcha
        integer :: i,j
        character :: char, nextchar
        integer :: digit
        captcha = 0
        do i=1,2014
            char = line(i:i)
            j = i + next_idx
            if (j.gt.2014) then
                j = j - 2014
            end if
            nextchar = line(j:j)
            if (char.ne." ") then
                if (char.eq.nextchar) then
                    read(char,'(i1)') digit
                    captcha = captcha + digit
                end if
            end if
        end do
    end function solve_captcha

end program day01