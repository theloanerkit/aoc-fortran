program day01
    implicit none
    integer :: num_lines
    character(len=7000) :: line
    open(1,file="2015-01f.txt",status="old")
    read(1,*) num_lines ! only 1 line
    read(1,*) line
    
    call solve(line)

    contains

    subroutine solve(line)
        integer :: answer_one
        integer :: answer_two
        character(len=7000) :: line

        answer_one=part_one(line)
        answer_two=part_two(line)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(line) result (floor_number)
        integer :: floor_number
        integer :: i
        character(len=7000) :: line
        character :: char
        floor_number=0
        do i=1,7000
            char = line(i:i)
            if (char == "(") then
                floor_number = floor_number + 1
            else if (char == ")") then 
                floor_number = floor_number - 1
            end if
        end do
    end function part_one

    function part_two(line) result (basement_idx)
        integer :: basement_idx
        integer :: floor_number
        integer :: i
        character(len=7000) :: line
        character :: char
        floor_number = 0
        basement_idx = -1
        do i=1,7000
            char = line(i:i)
            if (char == "(") then
                floor_number = floor_number + 1
            else if (char == ")") then 
                floor_number = floor_number - 1
            end if
            if (floor_number.eq.-1 .and. basement_idx.eq.-1) then
                basement_idx = i
            end if
        end do
    end function part_two

    
end program day01