program day02
    use readfile, only : readlines
    implicit none
    integer :: num_lines
    open(1,file="2016-02f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)
    contains

    subroutine solve(num_lines)
        character(len=5) :: answer_one
        character(len=5) :: answer_two
        integer :: num_lines
        character(len=525) :: lines(num_lines)

        lines = readlines(num_lines,525,1)

        answer_one = part_one(lines,num_lines)
        answer_two = part_two(lines,num_lines)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(lines,num_lines) result (code)
        character(len=5) :: code
        integer :: num_lines
        character(len=525) :: lines(num_lines)
        integer :: i,j,idx
        character :: char
        integer :: startpos(2),endpos(2)
        character :: numpad(9)
        numpad=(/"1","2","3","4","5","6","7","8","9"/)
        startpos=(/1,1/)
        code = "    "
        do i=1,num_lines
            do j=1,525
                char = lines(i)(j:j)
                if (char.ne." ") then
                    endpos = move(char,startpos)
                    if (endpos(1).ge.0.and.endpos(2).ge.0.and.endpos(1).le.2.and.endpos(2).le.2) then
                        startpos = endpos
                    end if
                end if
            end do
            idx = 3*startpos(1) + startpos(2) + 1
            code(i:i) = numpad(idx)
        end do
    end function part_one

    function part_two(lines,num_lines) result (code)
        character(len=5) :: code
        integer :: num_lines
        character(len=525) :: lines(num_lines)
        integer :: i,j,idx
        character :: char
        integer :: startpos(2),endpos(2)
        character :: numpad(13)
        numpad=(/"1","2","3","4","5","6","7","8","9","A","B","C","D"/)
        startpos=(/0,-2/)
        code = "     "
        do i=1,num_lines
            do j=1,525
                char = lines(i)(j:j)
                if (char.ne." ") then
                    endpos = move(char,startpos)
                    if (endpos(1).ge.-2.and.endpos(2).ge.-2.and.endpos(1).le.2.and.endpos(2).le.2) then
                        if (abs(endpos(1))+abs(endpos(2)).ge.-2.and.abs(endpos(1))+abs(endpos(2)).le.2) then
                            startpos = endpos
                        end if
                    end if
                end if
            end do
            select case(startpos(1))
            case(-2)
                idx = 1
            case(-1)
                idx = 3
            case(0)
                idx = 7
            case(1)
                idx = 11
            case(2)
                idx = 13
            end select
            idx = idx + startpos(2)
            code(i:i) = numpad(idx)
        end do
    end function part_two

    function move(char, startpos) result (endpos)
        character :: char
        integer :: startpos(2),endpos(2) ! (row,col)
        select case (char)
        case("U")
            endpos=(/startpos(1)-1,startpos(2)/)
        case("D")
            endpos=(/startpos(1)+1,startpos(2)/)
        case("L")
            endpos=(/startpos(1),startpos(2)-1/)
        case("R")
            endpos=(/startpos(1),startpos(2)+1/)
        end select
    end function move
end program day02