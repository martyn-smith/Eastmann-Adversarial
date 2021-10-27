!==============================================================================
! random_dist default values:
!  1  0.485
!  2  0.005
!  3  45.0
!  4  45.0
!  5  35.0
!  6  40.0
!  7  1
!  8  1
!  9  0
! 10  0
! 11  0
! 12  0
!
! no deviation has been observed within 48 hours
!==============================================================================

subroutine perturb_xmv(time)

    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)

    real(kind=8), intent(in) :: time
    logical :: init = .false.
    integer :: i = 0, j = 0
    real(kind=8) :: period, amp
    character(len=10) :: tmp, mode = ""

        if (.not. init) then
            do j = 1, command_argument_count()
                call get_command_argument(j, tmp)
                if (tmp == "--xmv") exit
            end do
            call get_command_argument(j+1, tmp)
            read(tmp, *) i
            call get_command_argument(j+2, tmp)
            read(tmp, *) mode
        end if
        select case (mode)
            case ("MAX")
                xmv(i) = 100.
            case ("MIN")
                xmv(i) = 0.
            case ("STICK")
            case ("SINE")
                call get_command_argument(j+3, tmp)
                read(tmp, *) period
                call get_command_argument(j+4, tmp)
                read(tmp, *) amp
                xmv(i) = xmv(i) + amp * sin(2 * 3.14159 * (time * 3600.) / period)
                xmv(i) = max(0., min(100.,xmv(i)))
            case ("SQUARE")
                call get_command_argument(j+3, tmp)
                read(tmp, *) period
                call get_command_argument(j+4, tmp)
                read(tmp, *) amp
                xmv(i) = xmv(i) + amp * (-1) ** floor(2 * (time * 3600.) / period)
                xmv(i) = max(0., min(100.,xmv(i)))
            !case ("DRIFT")
        end select
    init = .true.
end subroutine perturb_xmv

subroutine perturb_xmeas(time)

    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)

    real(kind=8), intent(in) :: time
    logical :: init = .false.
    integer :: i = 0, j = 1
    real(kind=8) :: period, amp, stick = 0., aggression = 0.9999, r
    character(len=10) :: tmp, mode = ""

    if (.not. init) then
        do j = 1, command_argument_count()
            call get_command_argument(j, tmp)
            if (tmp == "--xmeas") exit
        end do
        call get_command_argument(j+1, tmp)
        read(tmp, *) i
        call get_command_argument(j+2, tmp)
        read(tmp, *) mode
    end if
    select case (mode)
        case ("MAX")
            xmeas(i) = 100.
        case ("MIN")
            xmeas(i) = 0.
        case ("STICK")
            call random_number(r)
            if (r > aggression) then
                stick = xmeas(i)
            end if
            if (stick > 0.0) then
                xmeas(i) = stick
            end if
        case ("SINE")
            call get_command_argument(j+3, tmp)
            read(tmp, *) period
            call get_command_argument(j+4, tmp)
            read(tmp, *) amp
            xmeas(i) = xmeas(i) + amp * sin(2 * 3.14159 * (time * 3600.) / period)
            xmeas(i) = max(0., min(100.,xmeas(i)))
        case ("SQUARE")
            call get_command_argument(j+3, tmp)
            read(tmp, *) period
            call get_command_argument(j+4, tmp)
            read(tmp, *) amp
            xmeas(i) = xmeas(i) + amp * (-1) ** floor(2 * (time * 3600.) / period)
            xmeas(i) = max(0., min(100.,xmeas(i)))
    end select ! xmeas variations
    init = .true.
end subroutine perturb_xmeas

subroutine set_mode
!   controller common block
    real(kind=8) :: setpt, gain, taui, errold
    common /ctrl/ setpt(20), gain(20), taui(20), errold(20)

    integer :: i, j
    character(len=6) :: tmp

    do j = 1, command_argument_count()
        call get_command_argument(j, tmp)
        if (tmp == "--mode") exit
    end do
    call get_command_argument(j+1, tmp)
    read(tmp, *) i
    select case (i)
        case (1)
            ! default, do nothing
        case (2)
            setpt(6) = 0.1
        case (3)
            setpt(6) = 9.0
        case (4)
            ! default, do nothing
        case (5)
            setpt(6) = 0.1
        case (6)
            setpt(6) = 9.0
    end select

end subroutine set_mode

subroutine perturb_mode(time)
!   not actually used/tested
    real(kind=8) :: setpt, gain, taui, errold
    common /ctrl/ setpt(20), gain(20), taui(20), errold(20)

    real(kind=8), intent(in) :: time
    logical :: init = .false.
    integer :: i = 0, j = 1
    real(kind=8) :: period, amp, set
    character(len=10) :: tmp, mode = ""

    if (.not. init) then
        do j = 1, command_argument_count()
            call get_command_argument(j, tmp)
            if (tmp == "--mode") exit
        end do
        call get_command_argument(j+1, tmp)
        read(tmp, *) i
        call get_command_argument(j+2, tmp)
        read(tmp, *) mode
    end if
    select case (mode)
        !case ("MAX")
        !    setpt(i) = setpt_max(i)
        !case ("MIN")
        !    setpt(i) = setpt_min(i)
        case ("SET")
            call get_command_argument(j+3, tmp)
            read(tmp, *) set
            setpt(i) = set
        case ("SINE")
            call get_command_argument(j+3, tmp)
            read(tmp, *) period
            call get_command_argument(j+4, tmp)
            read(tmp, *) amp
            setpt(i) = setpt(i) + amp * sin(2 * 3.14159 * (time * 3600.) / period)
            setpt(i) = max(0., min(100., setpt(i)))
        case ("SQUARE")
            call get_command_argument(j+3, tmp)
            read(tmp, *) period
            call get_command_argument(j+4, tmp)
            read(tmp, *) amp
            setpt(i) = setpt(i) + amp * (-1) ** floor(2 * (time * 3600.) / period)
            setpt(i) = max(0., min(100.,setpt(i)))
    end select ! setpt variations
    init = .true.
end subroutine perturb_mode

subroutine zero_idvs()
    logical :: auto
    integer :: idv
    common /dvec/ idv(24), auto

    idv = 0
end subroutine zero_idvs

subroutine set_idv()
    logical :: auto
    integer :: idv
    common /dvec/ idv(24), auto

    logical :: init = .false.
    integer :: i, active
    character(len=2) :: tmp, active_param

    if (.not. init) then
        do i = 1, command_argument_count()
            call get_command_argument(i, tmp)
            if (tmp == "-d") exit
        end do
        call get_command_argument(i+1, active_param)
        if (active_param /= "") read(active_param, *) active
        init = .true.
    end if

    idv = 0
    idv(active) = 1
end subroutine set_idv

subroutine set_idvs()
    logical :: auto
    integer :: idv
    common /dvec/ idv(24), auto

    logical :: init = .false.
    integer :: i, j !, k=0
    real(kind=8) :: rand
    real(kind=8) :: aggression = 0.01
    character(len=10) :: tmp, aggression_param
    !character(len=255) :: filename

    idv = 0
    if (.not. init) then
        do j = 1, command_argument_count()
            call get_command_argument(j, tmp)
            if (tmp == "-a" .or. tmp == "--aggression") exit
        end do
        call get_command_argument(j+1, aggression_param)
        if (aggression_param /= "") read(aggression_param, *) aggression
        init = .true.
    else
        do i=1,size(idv)
            call random_number(rand)
            if (rand + aggression > 1.) idv(i) = 1
        end do
    end if
end subroutine set_idvs

module tewalk
    type walk
        sequence
        integer, dimension(12) :: idvwlk
        real(kind=8), dimension(12) :: adist, bdist, cdist, ddist, tlast, tnext, &
                                       hspan, hzero, sspan, szero, spspan
    end type walk
    contains
    subroutine walker_init()
        type(walk) :: wlk
        common /wlk/ wlk

        wlk%bdist = 0.
        wlk%cdist = 0.
        wlk%ddist = 0.
        wlk%tlast = 0.
        wlk%tnext = 0.1
        wlk%hspan = [0.2, 0.7, 0.25, 0.7, 0.15, 0.15, &
                 1.0, 1.0, 0.4, 1.5, 2.0, 1.5]
        wlk%hzero = [0.5, 1.0, 0.5, 1.0, 0.25, 0.25, &
                 2.0, 2.0, 0.5, 2.0, 3.0, 2.0]
        wlk%sspan = [0.03, 0.003, 10.0, 10.0, 10.0, 10.0, &
                 0.25, 0.25, 0.25, 0.0, 0.0, 0.0]
        wlk%szero = [0.485, 0.005, 45.0, 45.0, 35.0, 40.0, &
                 1.0, 1.0, 0.0, 0.0, 0.0, 0.0]
        wlk%spspan = 0.
!       label 550
        wlk%adist = wlk%szero
!       idvwlk never set?
    end subroutine walker_init

    subroutine next_walk(time)
!       common block
        type(walk) :: wlk
        common /wlk/ wlk
!       common block

        real(kind=8), intent(in) :: time
        integer :: i
        real(kind=8) :: hwlk, swlk, spwlk, rand

        call set_idvwlk()
        do i=1,9 !label 900
            if(time >= wlk%tnext(i)) then
!               TODO: this part frequently denormalises. Really, is it necessary?
                hwlk = wlk%tnext(i)-wlk%tlast(i)
                swlk = wlk%adist(i) + hwlk &
                                      * (wlk%bdist(i) + hwlk &
                                                    * (wlk%cdist(i) + hwlk * wlk%ddist(i)))
                spwlk = wlk%bdist(i) + hwlk &
                                    * (2. * wlk%cdist(i) + 3. * hwlk * wlk%ddist(i))
                wlk%tlast(i) = wlk%tnext(i)
                !set_dists calls random(), mutating cdist, ddist, tnext
                call set_dists(swlk, spwlk, i)
            end if
        end do
        do i=10,12 !label 910
            if(time >= wlk%tnext(i)) then
                hwlk = wlk%tnext(i)-wlk%tlast(i)
                swlk = wlk%adist(i)+hwlk &
                                * (wlk%bdist(i) + hwlk &
                                                * (wlk%cdist(i) + hwlk * wlk%ddist(i)))
                spwlk = wlk%bdist(i) + hwlk &
                                    * (2. * wlk%cdist(i) + 3. * hwlk * wlk%ddist(i))
                wlk%tlast(i) = wlk%tnext(i)
                if(swlk > 0.1) then
                    wlk%adist(i) = swlk
                    wlk%bdist(i) = spwlk
                    wlk%cdist(i) = -(3.*swlk+0.2*spwlk)/0.01
                    wlk%ddist(i) = (2.*swlk+0.1*spwlk)/0.001
                    wlk%tnext(i) = wlk%tlast(i)+0.1
                else
                    !calls random(-1,1) on hwlk (used to be tesub7(isd))
                    call random_number(rand)
                    hwlk = wlk%hspan(i) * (2*rand-1) + wlk%hzero(i)
                    wlk%adist(i) = 0.
                    wlk%bdist(i) = 0.
                    wlk%cdist(i) = wlk%idvwlk(i)/hwlk**2
                    wlk%ddist(i) = 0.
                    wlk%tnext(i) = wlk%tlast(i)+hwlk
                end if
            end if
        end do
        if(time == 0.) then
            !label 950 vectorised
            wlk%adist = wlk%szero
            wlk%bdist = 0.
            wlk%cdist = 0.
            wlk%ddist = 0.
            wlk%tlast = 0.
            wlk%tnext = .1
        end if
    end subroutine next_walk

    subroutine set_dists(s, sp, i)
!       was tesub5
!       common block
        type(walk) :: wlk
        common /wlk/ wlk
!       common block
    
        integer, intent(in) :: i
        real(kind=8), intent(in) :: s, sp
        real(kind=8) ::  h, s1, s1p, rand(3)
    
        call random_number(rand)
        h = wlk%hspan(i) * (2*rand(1)-1) + wlk%hzero(i)
        s1 = wlk%sspan(i) * (2*rand(2)-1) * wlk%idvwlk(i) + wlk%szero(i)
        s1p = wlk%spspan(i) * (2*rand(3)-1) * wlk%idvwlk(i)
        wlk%adist(i) = s
        wlk%bdist(i) = sp
        wlk%cdist(i) = (3.*(s1-s) - h*(s1p + 2.*sp))/h**2
        wlk%ddist(i) = (2.*(s-s1) + h*(s1p + sp))/h**3
        wlk%tnext(i) = wlk%tlast(i) + h
        return
    end subroutine set_dists
        
    subroutine set_idvwlk()
    !   common block
        logical :: auto
        integer :: idv
        common /dvec/ idv(24), auto
        type(walk) :: wlk
        common /wlk/ wlk
    !   common block
    
        wlk%idvwlk(1)=idv(8)
        wlk%idvwlk(2)=idv(8)
        wlk%idvwlk(3)=idv(9)
        wlk%idvwlk(4)=idv(10)
        wlk%idvwlk(5)=idv(11)
        wlk%idvwlk(6)=idv(12)
        wlk%idvwlk(7)=idv(13)
        wlk%idvwlk(8)=idv(13)
        wlk%idvwlk(9)=idv(16)
        wlk%idvwlk(10)=idv(17)
        wlk%idvwlk(11)=idv(18)
        wlk%idvwlk(12)=idv(24)
        return
    end subroutine set_idvwlk

    real(kind=8) function random_xmeas_noise(xns) 
    !   replaces tesub6
        real(kind=8) :: xns, rand

        call random_number(rand)
        random_xmeas_noise = (12*rand - 6.) * xns
        return
    end function random_xmeas_noise

    real(kind=8) function random_dist(i, time) 
!       was named tesub8
!       sets: sm(4)%x(1,2) (1,2), sm(1)%T (3), sm(4)%T (4), tcwr (5), tcws (6), r1f (7), r2f (8), 
!       uac (9), sm(8)%ftm (12), R%qu (10), S%qu (11)
!       common block
        type(walk) :: wlk
        common /wlk/ wlk
!       common block

        real(kind=8), intent(in) :: time
        integer :: i
        real(kind=8) :: h

        h = time - wlk%tlast(i)
        random_dist = wlk%adist(i) + h*(wlk%bdist(i) + h*(wlk%cdist(i) + h*wlk%ddist(i)))
        !print *, time, i, random_dist
        return
    end function random_dist
end module tewalk
