!module controller
!end module

subroutine contrlinit
!   measurement and valve common block
    real(kind=8) :: xmeas, xmv, fxmeas, taufil, alpha
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22)

!   controller common block
    real(kind=8) :: setpt, gain, taui, errold
    common /ctrl/ setpt(20), gain(20), taui(20), errold(20)

!   set controller parameters
!   reactor temperature control
    setpt(1)=120.40
    errold(1)=0.
    gain(1)=-10.3
    taui(1)=1.9
!   reactor level control:
    setpt(2)=75.0
    errold(2)=0.
    gain(2)=0.8
    taui(2)=23
!   product separator level control:
    setpt(3)=50.0
    errold(3)=0.
    gain(3)=1.4
    taui(3)= 96
!   stripper level control:
    setpt(4)=50.0
    errold(4)=0.
    gain(4)= 5
    taui(4)= 50
!   stripper underflow control:
    setpt(5)=22.949
    errold(5)=0.
    gain(5)=1.2
    taui(5)=50.
!   g/h ratio control:
    setpt(6)=1.0
    errold(6)=0.
    gain(6)=7.1
    taui(6)=39.
!   reactor pressure control:
    setpt(7)=2705.0
    errold(7)=0.
    gain(7)=1.1
    taui(7)=49.5
!   purge gas b component control:
    setpt(8)=13.823
    errold(8)=0.
    gain(8)=-14.5
    taui(8)=30.1
!   reactor feed a component control:
    setpt(9)=32.188
    errold(9)=0.
    gain(9)=4.1
    taui(9)=81.9
end subroutine contrlinit

subroutine contrl(delta_t)
    save 
!   discrete control algorithms

!   measurement and valve common block
    real(kind=8) :: xmeas, xmv, fxmeas, taufil, alpha
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22)

!   controller common block
    real(kind=8) :: setpt, gain, taui, errold
    common /ctrl/ setpt(20), gain(20), taui(20), errold(20)

    real(kind=8), intent(in) :: delta_t
    integer :: target
    real(kind=8) :: err
    character(len=5) :: flag

!   example PI controller:
!     stripper level controller
!
!     calculate error
!       err = setpt - xmeas(15)
!
!     proportional-integral controller (velocity form)
!         gain = controller gain
!         taui = reset time (min)
!
!     dxmv = gain * ( ( err - errold ) + err * delta_t * 60. / taui )
!
!     xmv(8) = xmv(8) - dxmv
!
!     errold = err
!
!     impose integral desaturation
!
!       xmv(8)=max(0.,min(100.,xmv(8)))      

!   reactor pressure control (reactor pressure -> A and C feed)
    err=setpt(7)-xmeas(7)
    xmv(4)=xmv(4) &
            + gain(7)*((err-errold(7))+err*delta_t*60./taui(7))
    errold(7)=err
    xmv(4)=max(0.,min(100.,xmv(4)))
!   reactor temperature control (reactor temperature -> reactor coolant flow)
    err=setpt(1)-xmeas(9)
    xmv(10) = xmv(10) &
            + gain(1)*((err-errold(1))+err*delta_t*60./taui(1))
    errold(1)=err
    xmv(10)=max(0.,min(100.,xmv(10)))
!   reactor level control (reactor level -> D feed)
    err=setpt(2)-fxmeas(8)
    xmv(2)=xmv(2) &
            + gain(2)*((err-errold(2))+err*delta_t*60./taui(2))
    errold(2)=err
    xmv(2)=max(0.,min(100.,xmv(2)))
!   product separator level control (sep level -> condensor coolant flow)
    err=setpt(3)-fxmeas(12)
    xmv(11)=xmv(11) &
            + gain(3)*((err-errold(3))+err*delta_t*60./taui(3))
    errold(3)=err
    xmv(11)=max(0.,min(100.,xmv(11)))
!   stripper level control (strip level -> sep pot flow)
    err=setpt(4)-fxmeas(15)
    xmv(7)=xmv(7) &
            + gain(4)*((err-errold(4))+err*delta_t*60./taui(4))
    errold(4)=err
    xmv(7)=max(0.,min(100.,xmv(7)))
!   stripper underflow control (strip underflow -> product flow)
    err=setpt(5)-xmeas(17)
    xmv(8)=xmv(8) &
                + gain(5)*((err-errold(5))+err*delta_t*60./taui(5))
    errold(5)=err
    xmv(8)=max(0.,min(100.,xmv(8)))
!   g/h ratio control (ratio -> a feed)
    err=setpt(6)-xmeas(42)
    xmv(1)=xmv(1) &
            + gain(6)*((err-errold(6))+err*delta_t*60./taui(6))
    errold(6)=err
    xmv(1)=max(0.,min(100.,xmv(1)))
!   purge gas b component control (b -> reactor feed rate)
    err=setpt(8)-xmeas(30)
    xmv(6)=xmv(6) &
            + gain(8)*((err-errold(8))+err*delta_t*60./taui(8))
    errold(8)=err
    xmv(6)=max(0.,min(100.,xmv(6)))
!   reactor feed a component control (reactor feed A -> E feed)
    err=setpt(9)-xmeas(23)
    xmv(3)=xmv(3) &
            + gain(9)*((err-errold(9))+err*delta_t*60./taui(9))
    errold(9)=err
    xmv(3)=max(0.,min(100.,xmv(3)))
!   set attacks
    call get_command_argument(1, flag)
    if (flag == "-xmv") then
        call get_command_argument(2, flag)
        read(flag, *) target
        if (target <= size(xmv)) then
            call get_command_argument(3, flag)
            select case (flag)
                case ("MAX")
                    xmv(target) = 100.0
                case ("MIN")
                    xmv(target) = 0.0
                case default
            end select
        else !invalid target
            print *, "invalid target"
        endif
    endif
    return
end subroutine contrl

subroutine check_safety(has_failed, err_msg)
!   common block
    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)
!   common block

    logical, intent(inout) :: has_failed
    character(len=25), intent(inout) :: err_msg

    if(xmeas(7) > 3000.0) then
        has_failed = .true.
        err_msg = "Reactor pressure high"
    end if
    if(xmeas(8) > 114.4 ) then ! 24.0 m^3
           has_failed = .true.
           err_msg = "Reactor level high"
    end if
    if(xmeas(8) < -2.0) then ! 2.0m^3
           has_failed = .true.
           err_msg = "Reactor level low"
    end if
    if(xmeas(9) > 175.0) then
           has_failed = .true.
           err_msg = "Reactor temp high"
    end if
    if(xmeas(12) > 137.0) then ! 12.0 m^3
           has_failed = .true.
           err_msg = "Sep level high"
    end if
    if(xmeas(12) < 2.7) then ! 1.0 m^3
           has_failed = .true.
           err_msg = "Sep level low"
    end if
    if(xmeas(15) > 131.0) then ! 8.0 m^3
          has_failed = .true.
           err_msg = "Stripper level high"
    end if
    if(xmeas(15) < -2.7) then ! 1.0 m^3
           has_failed = .true.
           err_msg = "Stripper level low"
    end if
end subroutine check_safety

subroutine check_danger(has_failed, err_msg)

    use entities
!   common block
    integer :: ivst
    real(kind=8) :: &
    delta_xr, reaction_rate, reaction_heat,  &
    vcv, vrng, vtau, &
    sfr, &
    xdel, xns, &
    t_gas, t_prod, vst
    type(agitator) :: agtatr
    type(vessel) :: r, s, c, v
    type(stream) :: sm
    type(compressor) :: cmpsr
    common /teproc/ &
    r, s, c, v, &
    delta_xr(8), reaction_rate(4), reaction_heat, &
    vcv(12), vrng(12), vtau(12), &
    sm(13), &
    sfr(8), &
    cmpsr, &
    agtatr, xdel(41), xns(41), &
    t_gas, t_prod, vst(12), ivst(12)
    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)
!   common block

    logical, intent(inout) :: has_failed
    character(len=25), intent(inout) :: err_msg

    if (((R%pt-760.0)/760.0*101.325) > 12000.) then
        has_failed = .true.
        err_msg = "Reactor has exploded"
    end if
    if (sm(10)%ftm < 0.) then
        has_failed = .true.
        err_msg = "purge reverse flow"
    end if
end subroutine check_danger