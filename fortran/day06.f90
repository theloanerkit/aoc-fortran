program day06
    use readfile, only : readlines,remove_whitespace
    use isadigit, only : how_many_numbers, get_numbers
    implicit none
    integer :: num_lines
    open(1,file="06f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)

    contains

    subroutine solve(num_lines)
        integer :: num_lines
        integer :: answer_one
        integer :: answer_two
        character(len=40) :: lines(num_lines)
        lines = readlines(num_lines,40)

        answer_one=part_one(lines,num_lines)
        answer_two=part_two(lines,num_lines)

        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function how_long_to_button(time,distance) result (options)
        integer, parameter :: k = selected_int_kind(16)
        integer :: options
        integer (kind=k) :: time
        integer (kind=k):: distance
        integer (kind=k):: bt,mt,d
        integer (kind=k):: min_button
        real (kind=k):: temp
        options = 0
        temp = (time*time)-(4*distance)
        min_button = floor((time-sqrt(temp))/2)
        do bt=min_button,time
            mt = time - bt
            d = mt * bt
            if (d > distance) then
                options = options + 1
            else if (d <= distance.and.options > 0) then
                exit
            end if
        end do
    end function how_long_to_button

    function part_one(lines,num_lines) result (answer_one)
        integer :: num_lines
        integer :: answer_one
        character(len=40) :: lines(num_lines)
        integer :: nums_count

        nums_count = how_many_numbers(lines(1),40)

        answer_one = run_part_one(lines,num_lines,nums_count)
    end function part_one

    function run_part_one(lines,num_lines,nums_count) result (answer_one)
        integer, parameter :: k = selected_int_kind(16)
        integer :: num_lines
        integer :: nums_count
        integer :: answer_one
        character(len=40) :: lines(num_lines)
        integer (kind=k):: times(nums_count)
        integer (kind=k):: distances(nums_count)
        integer :: options(nums_count)
        integer :: i
        times = get_numbers(lines(1),40,nums_count)
        distances = get_numbers(lines(2),40,nums_count)
        do i=1,nums_count
            options(i) = how_long_to_button(times(i),distances(i))
        end do
        answer_one = 1
        do i=1,nums_count
            answer_one = answer_one*options(i)
        end do
    end function run_part_one

    function part_two(lines,num_lines) result (answer_two)
        integer, parameter :: k = selected_int_kind(16)
        integer :: num_lines
        integer :: answer_two
        character(len=40) :: lines(num_lines)
        character(len=40) :: line1,line2
        integer(kind=k) :: time(1)
        integer(kind=k) :: distance(1)
        line1 = remove_whitespace(lines(1),40)
        line2 = remove_whitespace(lines(2),40)
        time = get_numbers(line1,40,1)
        distance = get_numbers(line2,40,1)
        answer_two = how_long_to_button(time(1),distance(1))
    end function part_two

end program day06