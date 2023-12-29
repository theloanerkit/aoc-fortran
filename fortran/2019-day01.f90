program day01
    use readfile, only : readlines
    use isadigit, only : get_numbers
    implicit none
    integer :: num_lines
    open(1,file="2019-01f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)
    contains

    subroutine solve(num_lines)
        integer :: answer_one
        integer :: answer_two
        integer :: num_lines
        character(len=7) :: lines(num_lines)
        lines = readlines(num_lines,7,1)
        answer_one = part_one(lines,num_lines)
        answer_two = part_two(lines,num_lines)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(lines,num_lines) result (total_fuel)
        integer :: total_fuel
        integer :: num_lines
        character(len=7) :: lines(num_lines)
        integer :: i
        integer :: mass(1)
        integer :: fuel
        total_fuel = 0
        do i=1,num_lines
            mass = get_numbers(lines(i),7,1)
            fuel = fuel_required(mass(1))
            total_fuel = total_fuel + fuel
        end do
    end function part_one

    function part_two(lines,num_lines) result (total_fuel)
        integer :: total_fuel
        integer :: num_lines
        character(len=7) :: lines(num_lines)
        integer :: i
        integer :: mass(1)
        integer :: fuel
        total_fuel = 0
        do i=1,num_lines
            mass = get_numbers(lines(i),7,1)
            fuel = fuel_required(mass(1))
            total_fuel = total_fuel + fuel
            do while (fuel.gt.0)
                fuel = fuel_required(fuel)
                if (fuel.gt.0) then
                    total_fuel = total_fuel + fuel
                end if
            end do
        end do
    end function part_two

    function fuel_required(mass) result (fuel)
        integer :: mass
        integer :: fuel
        real :: step1
        integer :: step2, step3
        step1 = mass/3
        step2 = floor(step1)
        step3 = step2 - 2
        fuel = step3
    end function fuel_required
end program day01