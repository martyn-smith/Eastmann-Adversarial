subroutine outputinit
    open(unit=1, file="state.dat", status="unknown")
    open(unit=2, file="xmeas.dat", status="unknown")
    open(unit=3, file="xmv.dat", status="unknown")
    open(unit=4, file="idvs.dat", status="unknown")


    write(1,"(8(a23))") "Time (hrs)", "Reactor", "Separator", "Stripper", "Junction", "TWR", "TWS", "VCV"
    ! ) "Time (hrs) |24 --- 93||93 --- 207|208||231 - 299||300 - 414|| 415 ||438 - 621|| 622 ||28 - 35|| 36 |", &
    !            "| R.ucv || R.ucl ||  R.et || S.ucv || S.ucl ||S.et|| C.ucl ||C.et|| V.ucv ||V.et|",&
    !   "| 37|| 38||   39-50   |",&
    !   "|twr||tws|| vcv/vpos? |"
!   attack / disturbance times (one-hot)
    write(4,*) "time  ", "a_c_ratio  ", "b_comp  ", "d_feed  ", "reactor_cool_step  ", "condensor_cool_step  ", &
                "a_feed_loss  ", "c_Pressure_loss  ", "a_b_c_feed  ", "d_T  ", "c_T  ", "reactor_cool_T  ", "condensor_cool_T  ", &
                "reaction_kinetics  ", "reactor_cool_stick  ", "condensor_cool_stick  ", &
                "unknown  ", "unknown  ", "unknown  ", "unknown  ", "unknown  ", &
                "Reactor_Integrity  ", "DDoS_1  ", "D_feed_DDoS  ", "Noise_1"
end subroutine outputinit

subroutine output(time, state)
!   measurement and valve common block
    logical :: auto
    integer :: idv, k
    real(kind=8) :: xmeas, xmv, fxmeas, taufil, alpha
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22)
    common /dvec/ idv(24), auto

!   local variables
    real(kind=8), intent(in) :: time, state(50)

!   write output
    write(1,"(51e23.15)") time, (state(k), k=1,50)
    write(2,"(43e23.15)") time, (xmeas(k),k=1,42)
    write(3,"(13e23.15)") time, (xmv(k), k=1,12)
    write(4,"(e23.15, 24i3)") time, (idv(k), k=1,24)

    return
end subroutine output