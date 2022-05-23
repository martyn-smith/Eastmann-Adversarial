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

module tewalk

contains
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
       
real(kind=8) function random_xmeas_noise(i)
!   replaces tesub6
    integer, intent(in) :: i
    real(kind=8) :: rand


    real, parameter :: xns(41) = [0.0012, 18.000, 22.000, 0.0500, 0.2000, &
                              0.2100, 0.3000, 0.5000, 0.0100, 0.0017, &
                              0.0100, 1.0000, 0.3000, 0.1250, 1.0000, &
                              0.3000, 0.1150, 0.0100, 1.1500, 0.2000, &
                              0.0100, 0.0100, 0.250, 0.100, 0.250, &
                              0.100, 0.250, 0.025, 0.250, 0.100, &
                              0.250, 0.100, 0.250, 0.025, 0.050, &
                              0.050, 0.010, 0.010, 0.010, 0.500, &
                              0.500]

    call random_number(rand)
    random_xmeas_noise = (12*rand - 6.) * xns(i)
    return
end function random_xmeas_noise

real(kind=8) function random_dist(i)
    integer, intent(in) :: i
    
    real, parameter :: dst(12) = [0.485, 0.005, 45., 45., 35., 40., &
                              1., 1., 0., 0., 0., 0.]

    random_dist = dst(i)
end function random_dist

end module tewalk