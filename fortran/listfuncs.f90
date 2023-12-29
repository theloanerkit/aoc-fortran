module listfuncs
    implicit none

    contains

    recursive function maximum_values(list,list_length,n) result (nums)
        integer :: list_length,n
        integer :: list(list_length)
        integer :: newlist(list_length-1)
        integer :: nums(n)
        integer :: i,j
        logical :: seen
        seen = .FALSE.
        j=1
        if (n.eq.1) then
            nums(1) = maxval(list)
        else
            nums(n) = maxval(list)
            do i=1,list_length
                if (list(i).ne.nums(n).or.seen.eqv..TRUE.) then
                    newlist(j) = list(i)
                    j = j + 1
                else if (list(i).eq.nums(n)) then
                    seen = .TRUE.
                end if
            end do
            nums(1:n-1) = maximum_values(newlist,list_length-1,n-1)
        end if
    end function maximum_values

    recursive function minimum_vaules(list, list_length, n) result (nums)
        integer :: list_length,n
        integer :: list(list_length)
        integer :: newlist(list_length-1)
        integer :: nums(n)
        integer :: i,j
        logical :: seen
        seen = .FALSE.
        j = 1
        if (n.eq.1) then
            nums(1) = minval(list)
        else
            nums(n) = minval(list)
            do i=1,list_length
                if (list(i).ne.nums(n).or.seen.eqv..TRUE.) then
                    newlist(j) = list(i)
                    j = j + 1
                else if (list(i).eq.nums(n)) then
                    seen = .TRUE.
                end if
            end do
            nums(1:n-1) = minimum_vaules(newlist,list_length-1,n-1)
        end if
    end function minimum_vaules

end module listfuncs