program day02
    use readfile, only : readlines
    use listfuncs, only : minimum_vaules
    use isadigit, only : get_numbers
    implicit none
    integer :: num_lines
    open(1,file="2015-02f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)
    contains

    subroutine solve(num_lines)
        integer :: answer_one
        integer :: answer_two
        integer :: num_lines
        character(len=9) :: lines(num_lines)
        lines = readlines(num_lines,9,1)

        answer_one = part_one(lines,num_lines)
        answer_two = part_two(lines,num_lines)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(lines,num_lines) result (area)
        integer :: area
        integer :: num_lines
        character(len=9) :: lines(num_lines)
        integer :: sides(3)
        integer :: i
        area = 0
        do i=1,num_lines
            sides = get_numbers(lines(i),9,3)
            area = area + surface_area(sides(1),sides(2),sides(3))
        end do
    end function part_one

    function part_two(lines,num_lines) result (ribbon)
        integer :: ribbon
        integer :: num_lines
        character(len=9) :: lines(num_lines)
        integer :: sides(3)
        integer :: i
        ribbon = 0
        do i=1,num_lines
            sides = get_numbers(lines(i),9,3)
            ribbon = ribbon + ribbon_length(sides(1),sides(2),sides(3))
        end do
    end function part_two

    function surface_area(length,width,height) result (area)
        integer :: length,width,height,area
        integer :: small(2)
        small = minimum_vaules((/length,width,height/),3,2)
        area = 0
        area = area + (2 * length * width)
        area = area + (2 * width * height)
        area = area + (2 * height * length)
        area = area + (small(1) * small(2))
    end function surface_area

    function ribbon_length(length,width,height) result (ribbon)
        integer :: length,width,height,ribbon
        integer :: small(2)
        small = minimum_vaules((/length,width,height/),3,2)
        ribbon = 0
        ribbon = ribbon + (length*width*height)
        ribbon = ribbon + (2*small(1)) + (2*small(2))
    end function ribbon_length
end program day02