program day01
    use readfile, only : readlines
    use isadigit, only : get_numbers
    implicit none
    integer :: num_lines
    open(1,file="2021-01f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)
    contains

    subroutine solve(num_lines)
        integer :: answer_one
        integer :: answer_two
        integer :: num_lines
        character(len=5) :: lines(num_lines)
        integer :: ints(num_lines)
        integer :: i
        integer :: temp(1)
        lines = readlines(num_lines,5,1)
        do i=1,num_lines
            temp = get_numbers(lines(i),5,1)
            ints(i) = temp(1)
        end do

        answer_one = part_one(ints,num_lines)
        answer_two = part_two(ints,num_lines)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(ints,num_ints) result (increase_times)
        integer :: increase_times
        integer :: num_ints
        integer :: ints(num_ints)
        integer :: i
        increase_times = 0
        do i=2,num_ints
            if (ints(i).gt.ints(i-1)) then
                increase_times = increase_times + 1
            end if
        end do
    end function part_one

    function part_two(ints,num_ints) result (increase_times)
        integer :: increase_times
        integer :: num_ints
        integer :: ints(num_ints)
        integer :: i,a,b
        increase_times = 0
        do i=4,num_ints
            a = ints(i-3) + ints(i-2) + ints(i-1)
            b = ints(i-2) + ints(i-1) + ints(i)
            if (b.gt.a) then
                increase_times = increase_times + 1
            end if
        end do
    end function part_two
end program day01