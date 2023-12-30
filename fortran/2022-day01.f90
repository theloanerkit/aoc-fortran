program day01
    use readfile, only : readlines
    use isadigit, only : get_numbers
    use listfuncs, only : maximum_values
    implicit none
    integer :: num_lines
    open(1,file="2022-01f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)
    contains

    subroutine solve(num_lines)
        integer :: answer_one
        integer :: answer_two
        integer :: num_lines
        character(len=6) :: lines(num_lines)
        integer :: ints(num_lines)
        integer :: i
        integer :: temp(1)
        integer :: elf_count
        elf_count = 1
        lines = readlines(num_lines,6,1)
        do i=1,num_lines
            temp = get_numbers(lines(i),6,1)
            ints(i) = temp(1)
            if (ints(i).eq.0) then
                elf_count = elf_count + 1
            end if
        end do
        answer_one = part_one(ints,num_lines,elf_count)
        answer_two = part_two(ints,num_lines,elf_count)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(ints,num_ints,elf_count) result (calories)
        integer :: calories
        integer :: num_ints,elf_count
        integer :: ints(num_ints)
        integer :: elves(elf_count)
        integer :: temp(1)
        elves = elf_carry(num_ints,elf_count,ints)
        temp = maximum_values(elves,elf_count,1)
        calories = temp(1)
    end function part_one

    function part_two(ints,num_ints,elf_count) result (calories)
        integer :: calories
        integer :: num_ints,elf_count
        integer :: ints(num_ints)
        integer :: elves(elf_count)
        integer :: temp(3)
        integer :: i
        calories = 0
        elves = elf_carry(num_ints,elf_count,ints)
        temp = maximum_values(elves,elf_count,3)
        do i=1,3
            calories = calories + temp(i)
        end do
    end function part_two

    function elf_carry(num_ints,elf_count,ints) result (elves)
        integer :: num_ints,elf_count
        integer :: ints(num_ints)
        integer :: elves(elf_count)
        integer :: i,j
        elves = 0
        j = 1
        do i=1,num_ints
            elves(j) = elves(j) + ints(i)
            if (ints(i).eq.0) then
                j = j + 1
            end if
        end do
    end function elf_carry
end program day01