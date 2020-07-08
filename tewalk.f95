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
        integer :: idv
        common /dvec/ idv(24)
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