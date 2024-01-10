program day02
    use readfile, only : readlines, separate_by_comma
    use isadigit, only : get_numbers
    use listfuncs, only : maximum_values, minimum_vaules
    implicit none
    integer :: num_lines
    open(1,file="2017-02f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines)
    contains

    subroutine solve(num_lines)
        integer :: answer_one
        integer :: answer_two
        integer :: num_lines
        character(len=76) :: lines(num_lines)

        lines = readlines(num_lines,76,1)
        answer_one = part_one(lines,num_lines)
        answer_two = part_two(lines,num_lines)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(lines,num_lines) result (checksum)
        integer :: checksum
        integer :: num_lines
        character(len=76) :: lines(num_lines)
        integer :: i
        integer :: big(1), small(1)
        integer :: nums_count
        integer :: nums(16)
        checksum = 0
        do i=1,num_lines
            nums_count = separate_by_comma(lines(i),76)
            nums = get_numbers(lines(i),76,nums_count)
            big = maximum_values(nums,nums_count,1)
            small = minimum_vaules(nums,nums_count,1)
            checksum = checksum + (big(1)-small(1))
        end do
    end function part_one

    function part_two(lines,num_lines) result (checksum)
        integer :: checksum
        integer :: num_lines
        character(len=76) :: lines(num_lines)
        integer :: i,j,k
        integer :: nums_count
        integer :: nums(16)
        checksum = 0
        do i=1,num_lines
            nums_count = separate_by_comma(lines(i),76)
            nums = get_numbers(lines(i),76,nums_count)
            do j=1,nums_count
                do k=1,nums_count
                    if (j.ne.k) then
                        if (mod(nums(j),nums(k)).eq.0) then
                            checksum = checksum + (nums(j)/nums(k))
                        end if
                    end if
                end do
            end do
        end do
    end function part_two
end program day02