subroutine measure
    use constants
    use entities
    use tewalk

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

    logical :: auto
    integer :: idv
    common /dvec/ idv(24), auto

    type(walk) :: wlk
    common /wlk/ wlk
!   common block

   
!   sm(3) -> stream 1, sm(1) -> stream 2, sm(2) -> stream 3
!   note vessel "C", apparently part of the stripper, is never pressurised (the original code has no PTC slot)
!   print *, time, sm(1)%ftm, sm(2)%ftm, sm(3)%ftm, sm(4)%ftm, sm(5)%ftm, sm(6)%ftm, sm(7)%ftm, &
!                sm(8)%ftm, sm(9)%ftm, sm(10)%ftm, sm(11)%ftm, sm(12)%ftm, sm(13)%ftm, &
!                R%pt, S%pt, C%pt, V%pt, S%vl
    xmeas(1) = sm(3)%ftm*0.359/35.3145 ! A Feed  (stream 1)                    kscmh , 
    xmeas(2) = sm(1)%ftm*sm(1)%xmws*0.454 ! D Feed  (stream 2)                 kg/hr from lbmol/hr
    xmeas(3) = sm(2)%ftm*sm(2)%xmws*0.454 ! E Feed  (stream 3)                 kg/hr
    xmeas(4) = sm(4)%ftm*0.359/35.3145 ! A and C Feed  (stream 4)              kscmh
    xmeas(5) = sm(9)%ftm*0.359/35.3145 ! Recycle Flow  (stream 8)              kscmh
    xmeas(6) = sm(6)%ftm*0.359/35.3145 ! Reactor Feed Rate  (stream 6)         kscmh
    xmeas(7) = (R%pt-760.0)/760.0*101.325 ! Reactor Pressure                   kPa gauge
    xmeas(8) = (R%vl-84.6)/666.7*100.0 ! Reactor Level                         %
    xmeas(9) = R%tc ! Reactor Temperature                                      deg C
    xmeas(10) = sm(10)%ftm*0.359/35.3145 ! purge rate (stream 9)               kscmh
    xmeas(11) = S%tc ! product sep temp                                        deg c
    xmeas(12) = (S%vl-27.5)/290.0*100.0 ! product sep level                    %
    xmeas(13) = (S%pt-760.0)/760.0*101.325 ! sep pressure                      kpa gauge
    xmeas(14) = sm(11)%ftm/S%density/35.3145 ! sep underflow (stream 10)       m3/hr
    xmeas(15) = (C%vl-78.25)/C%vt*100.0 ! stripper level                       %
    xmeas(16) = (V%pt-760.0)/760.0*101.325 ! stripper pressure                 kpa gauge
    xmeas(17) = sm(13)%ftm/C%density/35.3145 ! stripper underflow (stream 11)  m3/hr
    xmeas(18) = C%tc ! stripper temperature                                    deg c
    xmeas(19) = C%qu*1.04e3*0.454 ! stripper steam flow                        kg/hr
    xmeas(20) = cmpsr%work*0.29307e3 ! compressor work, again??                kwh
    xmeas(21) = R%cl%T_out ! reactor cooling water outlet temp                 deg c
    xmeas(22) = S%cl%T_out ! separator cooling water outlet temp               deg c

    if(time > 0.) then
        do i=1,22 !label 6500
            !call tesub6(xns(i),xmns)
            xmeas(i) = xmeas(i)+random_xmeas_noise(xns(i))
        end do
    end if
    if (idv(16) == 1) then
        if (xmeas_tgt == 0) then
            xmeas_tgt = ceiling(rand() * 42.0)
        end if
        if (time > 0.012 .and. time < 0.027 ) then
            xmeas(xmeas_tgt) = 0.0
        end if
    end if
    if (idv(21) == 1) then
        if (time > 0.012 .and. time < 0.027 ) then
            xmeas(9) = 500.0
        end if
    end if
    

!    print *, metric_gauge(R%pt), S%pt, C%pt, metric_gauge(V%pt)

    if(time == 0.) then
        do i=23,41 !label 7010
            xdel(i) = xcmp(i)
            xmeas(i) = xcmp(i)
        end do
        t_gas = 0.1
        t_prod = 0.25
    end if
    if(time >= t_gas) then !purge gas and reactor feed analysis
        do i=23,36 !label 7020
            xmeas(i) = xdel(i)
            !call tesub6(xns(i),xmns)
            xmeas(i) = xmeas(i)+random_xmeas_noise(xns(i))
            xdel(i) = xcmp(i)
        end do
        t_gas=t_gas+0.1
    end if
    if(time >= t_prod) then !product feed analysis
        do i=37,41 !label 7030
            xmeas(i) = xdel(i)
            !call tesub6(xns(i),xmns)
            xmeas(i) = xmeas(i)+random_xmeas_noise(xns(i))
            xdel(i) = xcmp(i)
        end do
        t_prod = t_prod+0.25
    end if
!   mass flow g/ mass flow h =
!        (molecular weight*mol%g/molecular weight*mol%h)
    xmeas(42) = (62.0*xmeas(40))/(76*xmeas(41))
end subroutine