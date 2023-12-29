program day01
    use readfile, only : readlines  
    use isadigit, only : get_numbers  
    implicit none
    integer :: num_lines
    open(1,file="2020-01f.txt",status="old")
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
        ints = 0
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

    function part_one(ints,num_ints) result (answer)
        integer :: answer
        integer :: num_ints
        integer :: ints(num_ints)
        answer = sum_to_then_multiply(ints,num_ints,2020,1)
    end function part_one

    function part_two(ints,num_ints) result (answer)
        integer :: answer
        integer :: num_ints
        integer :: ints(num_ints)
        answer = sum_to_then_multiply(ints,num_ints,2020,2)
    end function part_two

    recursive function sum_to_then_multiply(list_of_nums,list_len,sum_to,part) result (num)
        integer :: num,temp
        integer :: list_len
        integer :: list_of_nums(list_len)
        integer :: sum_to
        integer :: i,y,x
        integer :: part
        num = 0
        do i=1,list_len
            x = list_of_nums(i)
            y = sum_to - x
            if (part.eq.1) then
                if (any(list_of_nums.eq.y)) then
                    num = x * y
                end if
            else if (part.eq.2) then
                temp = sum_to_then_multiply(list_of_nums,list_len,y,1)
                if (temp.ne.0) then
                    num = temp * x
                end if
            end if
        end do
    end function sum_to_then_multiply
end program day01