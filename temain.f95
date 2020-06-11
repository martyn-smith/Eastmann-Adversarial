!
!               tennessee eastman process control test problem
!
!                    james j. downs and ernest f. vogel
!
!                  process and control systems engineering
!                        tennessee eastman company
!                              p.o. box 511
!                          kingsport, tn  37662
!
!
!  reference:
!    "a plant-wide industrial process control problem"
!    presented at the aiche 1990 annual meeting
!    industrial challenge problems in process control, paper #24a
!    chicago, illinois, november 14, 1990
!
!
!  main program for demonstrating application of the tennessee eastman 
!  process control test problem
!
!
!===============================================================================
!
include "teprob.f95"
include "tecontrol.f95"

program TEmain
    implicit none
!   measurement and valve common block
    real(kind=8) :: xmeas, xmv, fxmeas, xmv0, taufil, alpha
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22), xmv0(12)

!   disturbance vector common block
    integer :: idv
    common /dvec/ idv(24)

!   controller common block
    real(kind=8) :: setpt, gain, taui, errold, delta_t
    common /ctrl/ setpt(20), gain(20), taui(20), errold(20), delta_t

!   specify the relay common block
    integer :: rlout, rlin
    real(kind=8) :: dumm, pert, signp, rlset
    common /relay/ dumm, pert, rlset, signp, rlout, rlin

!   local variables
    integer :: i, npts
    real(kind=8) :: time, state(50), derivative(50)
    real(kind=8) :: dummy, rand1

!   initialise filter times (seconds)
    taufil = 5.d0
                ! 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, &
                ! 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, &
                ! 5.d0, 5.d0, 5.d0, 5.d0/
!   open files for output
    call outputinit

!   set the number of points to simulate
!   48 hours of simulated time
    npts = 48*3600

!   integrator step size:  1 second converted to hours (time-base of simulation)
    delta_t = 1. / 3600.00001

!   specify some parameters for the relay identification plus some
!   parameters for the filter
!   references: i)seborg ii) astrom, automatica, 1984, 20, 645-651
!   taufil  = filter constant for xmeas(i)
    do i=1,22
         alpha(i) = delta_t*3600./taufil(i)
    end do

!   initialise random number generator
    dummy = rand1()

!   initialize process (sets time to zero)
    call teinit(size(state), time, state, derivative)

!   mass flow g/ mass flow h =
!        (molecular weight*mol%g/molecular weight*mol%h)
    xmeas(42)=(62.0*xmeas(40))/(76*xmeas(41))

!   set the filtered variables equal to the steady-state values
    call filter_xmeas(time)
!   ?
    do i=1,size(xmv)
        xmv0(i)=xmv(i)
    end do
    call contrlinit
!   set all disturbance flags to off
    call set_idvs

!   simulation loop
    do i = 1, npts
        call contrl
        if (mod(i, 3600) == 0) then
            call set_idvs
        end if
        call tefunc(size(state), time, state, derivative)
        call output(time)
        call intgtr(size(state), time, delta_t, state, derivative)
        call filter_xmeas(time)
    end do
!   stop
end program TEmain

!===============================================================================
subroutine outputinit
    open(unit=30,file='out.dat',status='unknown')
    write(30, *) "time  ", "reactor_level  ", "reactor_temperature  ", "product_sep_level  ", &
                "strip_level  ", "reactor_pressure  ", "strip_underflow  ", "b_purge_prct  ", &
                "prod_g/h  ", "d_feed  ", "reactor_cool  ", "condensor_cool  ", & 
                "sep_prod_flow  ", "a_c_feed  ", "strip_prod_flow  ", "purge_valve  ", &
                "a_feed  ", "reactor_a/c  ", "e_feed" 
!   filtered measurements
    open(unit=40,file='fout.dat',status='unknown')
    write(40,*) "time  ", &
                "(f)a_feed  ", "(f)d_feed  ", "(f)e_feed  ", "(f)a_c_feed  ", "(f)recycle  ", &
                "(f)reactor_feed  ", "(f)reactor_pressure  ", "(f)reactor_level  ", "(f)reactor_temperature  ", &
                "(f)purge  ", &
                "(f)product_temp  ",  "(f)product_level  ", "(f)product_pressure  ", "(f)prod_underflow  ", &
                "(f)strip_level  ", "(f)strip_pressure  ", "(f)strip_underflow  ", "(f)strip_temp  ", &
                "(f)strip_steam  ", "(f)compress_work  ", "(f)reactor_cool_out_temp  ", "(f)sep_cool_out_temp  ", &
                "reactor_feed_a_prct  ", "reactor_feed_b_prct  ", "reactor_feed_c_prct  ","reactor_feed_d_prct  ", &
                "reactor_feed_e_prct  ","reactor_feed_f_prct  ", &
                "purge_a_prct  ", "purge_b_prct  ", "purge_c_prct  ", "purge_d_prct  ", &
                "purge_e_prct  ", "purge_f_prct  ", "purge_g_prct  ", "purge_h_prct  ", &
                "prod_d_prct  ", "prod_e_prct  ", "prod_f_prct  ", "prod_g_prct  ",  "prod_h_prct"
!   manipulated variables
    open(unit=50,file='inpt.dat',status='unknown')
    write(50, *) "time  ", "a_feed  ", "d_feed  ", "e_feed  ", "a_c_feed  ", "compressor_valve  ", &
                 "purge_valve  ", "sep_underflow  ", "strip_underflow  ", "strip_valve  ", &
                 "reactor_cool  ", "condensor_cool  ", "agitator_spd"
!   attack / disturbance times (one-hot)
    open(unit=60,file='dvec.dat')
    write(60, *) "time  ", "a_c_ratio  ", "b_comp  ", "d_feed  ", "reactor_cool_step  ", "condensor_cool_step  ", &
                 "a_feed_loss  ", "c_Pressure_loss  ", "a_b_c_feed  ", "d_T  ", "c_T  ", "reactor_cool_T  ", "condensor_cool_T  ", &
                 "reaction_kinetics  ", "reactor_cool_stick  ", "condensor_cool_stick  ", &
                 "unknown", "unknown", "unknown", "unknown", "unknown", &
                 "Reactor Integrity", "DDoS 1", "D feed DDoS", "Noise 1"
end subroutine outputinit

subroutine output(time)
    save icount
!   measurement and valve common block
    integer :: icount, idv, k, n
    real(kind=8) :: xmeas, xmv, time, fxmeas, xmv0, taufil, alpha
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22), xmv0(12)
    common /dvec/ idv(24)
    data icount /0/

!   write matlab output every n samples
    n=1
    if(mod(icount,n) == 0) then
!       matlab output
        write(30,101) time, xmeas(8),xmeas(9),xmeas(12),xmeas(15), &
        xmeas(7),xmeas(17),xmeas(30),xmeas(40)/xmeas(41),xmv(2), &
        xmv(10),xmv(11),xmv(7),xmv(4),xmv(8),xmv(6),xmv(1), &
        xmeas(23)/xmeas(25),xmv(3)
        write(40,102) time, (fxmeas(k), k=1,22),(xmeas(k),k=23,42)
        write(50,103) time, (xmv(k),k=1,12)
        write(60,104) time, (idv(k), k=1,24)
    end if
    icount=icount+1
!   use this sort of format statement for output for matlab (all data on a single
!   line), the first number is the number of data entries
101 format(21e23.15)
102 format(43e23.15)
103 format(13e23.15)
104 format(e23.15, 24i3)
    return
end subroutine output
!
!===============================================================================
!
subroutine intgtr(nn, time, delta_t, state, derivative)
!   euler integration algorithm
    integer :: i, nn
    real(kind=8) :: time, delta_t, state(nn), derivative(nn)
    
    time = time + delta_t
    do i = 1, nn
        state(i) = state(i) + derivative(i) * delta_t
    end do
    return
end subroutine intgtr

subroutine filter_xmeas(time)
    !       filter measurements
    !       alpha = filter constant, see e.g. seeborg pp. 539-540 for more details.  Currently set to ~0.2)
    !       alpha = 1,   no filtering
    !       alpha = 0,   complete filtering (measurement is ignored)
    integer :: k
    real(kind=8) :: alpha, fxmeas, taufil, xmv0, time
    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22), xmv0(12)

    if (time /= 0) then
        do k=1,22
            fxmeas(k)=alpha(k)*xmeas(k)+(1-alpha(k))*fxmeas(k)
        end do 
    else
        do k=1,22
            fxmeas(k)=xmeas(k)
        end do 
    end if
end subroutine filter_xmeas

subroutine set_idvs
    
    integer :: idv
    common /dvec/ idv(24)

    logical :: init
    integer :: i
    real(kind=8) :: rand1, aggression
    data init /.FALSE./
    data aggression /0.9/
    
    do i=1,size(idv)
        idv(i) = 0
    end do
    if (init) then
        do i=1,size(idv)
            call random_number(rand1)
            if (rand1 > aggression) idv(i) = 1
        end do
    else
        !init = .TRUE.
    end if
end subroutine set_idvs

real(kind=8) function rand1()
!   see numerical recipes in fortran, p.196
!   returns 0.0 to 1.0 random deviate. set idum to any negative value to initialise
    logical :: init
    integer :: ia1, ia2, ia3, ic1, ic2, ic3, ix1, ix2, ix3, idum, j, m1, m2, m3
    real(kind=8) :: r(97), rm1, rm2
    save ix1, ix2, ix3, r
    parameter(m1=259200, ia1=7141, ic1=54773, rm1=1./m1)
    parameter(m2=134456, ia2=8121, ic2=28411, rm2=1./m2)
    parameter(m3=243000, ia3=4561, ic3=51349)
    data init /.FALSE./
    data idum /-1/
    if(.NOT. init) then ! idum < 0 .or. iff == 0) then
        init = .TRUE.
        ix1=mod(ic1-idum,m1)
        ix1=mod(ia1*ix1+ic1,m1)
        ix2=mod(ix1,m2)
        ix1=mod(ia1*ix1+ic1,m1)
        ix3=mod(ix1,m3)
        do j=1,97
            ix1=mod(ia1*ix1+ic1,m1)
            ix2=mod(ia2*ix2+ic2,m2)
            r(j)=(ix1+ix2*rm2)*rm1
        end do
        idum=1 !???
    endif
    ix1=mod(ia1*ix1+ic1, m1)
    ix2=mod(ia2*ix2+ic2, m2)
    ix3=mod(ia3*ix3+ic3, m3)
    j=mod(1+(97*ix3)/m3, size(r))
    rand1=r(j)
    r(j)=(ix1+ix2*rm2)*rm1
    return
end function rand1