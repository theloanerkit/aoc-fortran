program day01
    use readfile, only : separate_by_comma
    use isadigit, only : get_numbers
    use getdistance, only : manhattan_distance
    implicit none
    integer :: num_lines
    character(len=660) :: line
    open(1,file="2016-01f.txt",status="old")
    read(1,*) num_lines
    read(1,*) line
    
    call solve(line)

    contains

    subroutine solve(line)
        integer :: answer_one
        integer :: answer_two
        character(len=660) :: line
        integer :: instructions
        instructions = separate_by_comma(line,660)

        answer_one = part_one(line,instructions)
        answer_two = part_two(line,instructions)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(line,instructions) result (distance)
        integer :: distance
        character(len=660) :: line
        integer :: instructions
        character :: dirns(instructions)
        integer :: dists(instructions)
        integer :: facing(2)
        integer :: position(2)
        integer :: i
        dirns = build_directions(line,instructions)
        dists = build_distances(line,instructions)
        facing = (/0,1/) ! north is x=0, y=1
        position = (/0,0/)
        do i=1,instructions
            facing = new_direction(facing,dirns(i))
            position = new_position(position, facing, dists(i))
        end do
        distance = manhattan_distance((/0,0/),position)
    end function part_one

    function part_two(line,instructions) result (distance)
        integer :: distance
        character(len=660) :: line
        integer :: instructions
        character :: dirns(instructions)
        integer :: dists(instructions)
        integer :: facing(2)
        integer :: position(2)
        integer :: i,j,k
        integer :: previous_positions_x(10000)
        integer :: previous_positions_y(10000)
        integer :: visit_twice(2)
        previous_positions_x = 0
        previous_positions_y = 0
        visit_twice = 0
        dirns = build_directions(line,instructions)
        dists = build_distances(line,instructions)
        facing = (/0,1/) ! north is x=0, y=1
        position = (/0,0/)
        do i=1,instructions
            facing = new_direction(facing,dirns(i))
            do k=1,dists(i)
                position = new_position(position, facing, 1)
                do j=1,10000
                    if (previous_positions_x(j).eq.position(1).and.previous_positions_y(j).eq.position(2)) then
                        if (visit_twice(1).eq.0.and.visit_twice(2).eq.0) then
                            visit_twice = position
                        end if
                    end if
                end do
                previous_positions_x((i-1)*dists(i)+k) = position(1)
                previous_positions_y((i-1)*dists(i)+k) = position(2)
            end do
        end do
        distance = manhattan_distance((/0,0/), visit_twice)
    end function part_two

    function new_direction(previous_direction, turn) result (direction)
        integer :: previous_direction(2)
        character :: turn
        integer :: direction(2)
        integer :: x,y,nx,ny
        x = previous_direction(1)
        y = previous_direction(2)
        if (turn.eq."R") then
            if (x.eq.0) then
                nx = y
                ny = 0
            else if (x.ne.0) then
                ny = -x
                nx = 0
            end if
        else if (turn.eq."L") then
            if (y.eq.0) then
                ny = x
                nx = 0
            else if (y.ne.0) then
                nx = -y
                ny = 0
            end if
        end if
        direction = (/nx,ny/)
    end function new_direction

    function new_position(old_position, direction, distance) result (position)
        integer :: old_position(2)
        integer :: position(2)
        integer :: direction(2)
        integer :: distance
        position(1) = old_position(1) + (direction(1) * distance)
        position(2) = old_position(2) + (direction(2) * distance)
    end function new_position

    function build_directions(line,instructions) result (dirns)
        integer :: instructions
        character(len=660) :: line
        character :: dirns(instructions)
        integer :: i,j
        character :: char
        dirns = " "
        j = 1
        do i=1,660
            char = line(i:i)
            if (char.eq."R".or.char.eq."L") then
                dirns(j) = char
                j = j + 1
            end if
        end do
    end function build_directions

    function build_distances(line,instructions) result (dists)
        integer :: instructions
        character(len=660) :: line
        integer :: dists(instructions)
        dists = get_numbers(line,660,instructions)
    end function build_distances
end program day01