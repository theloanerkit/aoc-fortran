program day02
    use readfile, only : readlines, count_chars
    implicit none
    integer :: num_lines
    character :: alphabet_lower(26)
    alphabet_lower=(/"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"/)
    open(1,file="2018-02f.txt",status="old")
    read(1,*) num_lines

    call solve(num_lines,alphabet_lower)

    contains

    subroutine solve(num_lines,alphabet_lower)
        integer :: answer_one
        character(len=27) :: answer_two
        integer :: num_lines
        character(len=27) :: lines(num_lines)
        character :: alphabet_lower(26)

        lines = readlines(num_lines,27,1)
        answer_one = part_one(lines,num_lines,alphabet_lower)
        answer_two = part_two(lines,num_lines)
        print*,"part one: "
        print*,answer_one
        print*,"----------"
        print*,"part two: "
        print*,answer_two
    end subroutine solve

    function part_one(lines,num_lines,alphabet_lower) result (checksum)
        integer :: checksum
        integer :: num_lines
        character(len=27) :: lines(num_lines)
        integer :: char_count_2, char_count_3, count
        logical :: two,three
        integer :: i,j
        character :: alphabet_lower(26)
        char_count_2 = 0
        char_count_3 = 0
        do i = 1,num_lines
            two = .FALSE.
            three = .FALSE.
            do j=1,26
                count = count_chars(lines(i),27,alphabet_lower(j))
                if (count.eq.2) then
                    two = .TRUE.
                else if (count.eq.3) then
                    three = .TRUE.
                end if
            end do
            if (two.eqv..TRUE.) then
                char_count_2 = char_count_2 + 1
            end if
            if (three.eqv..TRUE.) then
                char_count_3 = char_count_3 + 1
            end if
        end do
        checksum = char_count_2 * char_count_3
    end function part_one

    function part_two(lines,num_lines) result (box_id)
        character(len=27) :: box_id
        integer :: num_lines
        character(len=27) :: lines(num_lines)
        integer :: i,j,k
        character(len=27) :: line1,line2,id1,id2
        character :: char1,char2
        integer :: dif_count
        id1=""
        id2=""
        box_id=""
        do i=1,num_lines
            line1 = lines(i)
            do j=i+1,num_lines
                line2 = lines(j)
                dif_count = 0
                do k=1,27
                    char1 = line1(k:k)
                    char2 = line2(k:k)
                    if (char1.ne.char2) then
                        dif_count = dif_count + 1
                    end if
                end do
                if (dif_count.eq.1) then
                    id1 = line1
                    id2 = line2
                end if
            end do
        end do
        j=1
        do i=1,27
            if (id1(i:i).eq.id2(i:i)) then
                box_id(j:j) = id1(i:i)
                j = j+1
            end if
        end do
    end function part_two
end program day02

