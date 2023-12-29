module getdistance
    implicit none

    contains

    function manhattan_distance(start,end) result (distance)
        integer :: distance
        integer :: start(2)
        integer :: end(2)
        distance = abs(start(1)-end(1)) + abs(start(2)-end(2))
    end function manhattan_distance

end module getdistance