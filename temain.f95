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

program TEmain
!   measurement and valve common block
    double precision xmeas, xmv, fxmeas, xmv0, taufil, alpha
    common/pv/ xmeas(42), xmv(12)
    common/filter/ fxmeas(22), xmv0(12),taufil(22),alpha(22)

!   disturbance vector common block
    integer idv
    common/dvec/ idv(20)

!   controller common block
    double precision setpt, gain, taui, errold,deltat
    common/ctrl/ setpt(20), gain(20), taui(20), errold(20),deltat

!   specify the relay common block
    double precision dumm, pert, signp, rlset
    integer rlout, rlin
    common/relay/ dumm, pert, rlset, signp, rlout, rlin

!   local variables
    integer i, nn, npts
    double precision time, state(50), derivative(50)
!    real(kind=8) dummy
    double precision dummy

!   initialise filter times (seconds)
    data taufil/ 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, &
                5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, &
                5.d0, 5.d0, 5.d0, 5.d0/
!   open files for output
    open(unit=30,file='out.dat',status='unknown')
    write(30, *) "time  ", "reactor_level  ", "reactor_temperature  ", "product_sep_level  ", &
                "strip_level  ", "reactor_pressure  ", "strip_underflow  ", "b_purge_prct  ", &
                "prod_g/h  ", "d_feed  ", "reactor_cool  ", "condensor_cool  ", & 
                "sep_prod_flow  ", "a_c_feed  ", "strip_prod_flow  ", "purge_valve  ", &
                "a_feed  ", "reactor_a/c  ", "e_feed" 
!   filteres measurements
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
!   set the number of differential equations (states).  the process has 50
!   states.  If the user wishes to integrate additional states, nn must be 
!   increased by the number of additional differential equations.
    nn = 50

!   set the number of points to simulate
!   48 hours of simulated time
    npts = 48*3600

!   integrator step size:  1 second converted to hours (time-base of simulation
    deltat = 1. / 3600.00001

!   specify some parameters for the relay identification plus some
!   parameters for the filter
!
!   references: i)seborg ii) astrom, automatica, 1984, 20, 645-651
!
!   taufil  = filter constant for xmeas(i)
    do i=1,22
         alpha(i) = deltat*3600./taufil(i)
    end do

!   initialise random number generator
    idum=-1
    dummy=ran1(idum)

!   initialize process (sets time to zero)
    call teinit(nn,time,state,derivative)

!   mass flow g/ mass flow h =
!        (molecular weight*mol%g/molecular weight*mol%h)
    xmeas(42)=(62.0*xmeas(40))/(76*xmeas(41))

!   set the filtered variables equal to the steady-state values
    do i=1,22
        fxmeas(i)=xmeas(i)
    end do
    do i=1,12
        xmv0(i)=xmv(i)
    end do
!   set up the relay if desired.
!   dumm = dummy variable...
!   dumm= 1 ----> relay is active
!   dumm = 0 ----> relay is inactive
    dumm = 0.
!   and the perturbation size
    pert = 5.
!   sign of steady-state process gain (if increasing manipulated variable
!   increases measured variable then signp=1. else signp=-1.)
    signp = -1.
!   measured variable
    rlout = 9
!   manipulated variable
    rlin = 10
    rlset = fxmeas(rlout)

!   set controller parameters
!   reactor temperature control
    setpt(1)=120.40
    errold(1)=0.d0
    gain(1)=-10.3
    taui(1)=1.9
!   reactor level control:
    setpt(2)=75.0
    errold(2)=0.d0
    gain(2)=0.8
    taui(2)=23
!   product separator level control:
    setpt(3)=50.0
    errold(3)=0.d0
    gain(3)=1.4
    taui(3)= 96
!   stripper level control:
    setpt(4)=50.0
    errold(4)=0.d0
    gain(4)= 5
    taui(4)= 50
!   stripper underflow control:
    setpt(5)=22.949
    errold(5)=0.d0
    gain(5)=1.2
    taui(5)=50.
!   g/h ratio control:
    setpt(6)=1.0
    errold(6)=0.d0
    gain(6)=7.1
    taui(6)=39.
!   reactor pressure control:
    setpt(7)=2705.0
    errold(7)=0.d0
    gain(7)=1.1
    taui(7)=49.5
!   purge gas b component control:
    setpt(8)=13.823
    errold(8)=0.d0
    gain(8)=-14.5
    taui(8)=30.1
!   reactor feed a component control:
    setpt(9)=32.188
    errold(9)=0.d0
    gain(9)=4.1
    taui(9)=81.9

!  set all disturbance flags to off
    do i = 1, 20
        idv(i) = 0
    end do

!   set disturbance 1 on 
!   idv(1)=1
!   idv(4)=1
!   idv(8)=1
!   idv(12)=1
!   idv(15)=1

!   simulation loop
    do i = 1, npts
!        increase setpoints after 5 hours
!        if (time > 5) setpt(1)=120.4
!        if (time > 5) setpt(2)=75
!        if (time > 5) setpt(3)=50
!        if (time > 5) setpt(4)=50
!        if (time > 5) setpt(5)=22.949*0.75
!        if (time > 5) setpt(6)=1*0.67
!        if (time > 5) setpt(7)=2705-60
        if (time > 5) setpt(8)=15.82
!        if (time > 5) setpt(9)=32.188
        call contrl
        call output(time)
        call intgtr(nn,time,deltat,state,derivative)

!       filter measurements
!       alpha = filter constant, see e.g. seeborg pp. 539-540 for more details
!       alpha = 1,   no filtering
!       alpha = 0,   complete filtering (measurement is ignored)
        do k=1,22
            fxmeas(k)=alpha(k)*xmeas(k)+(1-alpha(k))*fxmeas(k)
        end do   
    end do
    stop
end program
!
!===============================================================================
!
subroutine contrl
    save 
!   discrete control algorithms

!   measurement and valve common block
    double precision xmeas, xmv, fxmeas,xmv0, taufil, alpha
    common/pv/ xmeas(42), xmv(12)
    common/filter/ fxmeas(22), xmv0(12),taufil(22),alpha(22)

!   relay common block
    double precision dumm, rlset, pert, signp
    integer rlout, rlin
    common/relay/ dumm, pert, rlset, signp, rlout, rlin

!   controller common block
    double precision setpt, gain, taui, errold,deltat
    common/ctrl/ setpt(20), gain(20), taui(20), errold(20),deltat

    double precision err
    double precision dummy
    dimension icount(20)
    data icount /20*0/
    data istart /0/
!
!   example pi controller:
!     stripper level controller
!
!     calculate error
!       err = setpt - xmeas(15)
!
!     proportional-integral controller (velocity form)
!         gain = controller gain
!         taui = reset time (min)
!
!     dxmv = gain * ( ( err - errold ) + err * deltat * 60. / taui )
!
!     xmv(8) = xmv(8) - dxmv
!
!     errold = err
!
!     impose integral desaturation
!
!       xmv(8)=max(0.,min(100.,xmv(8)))      
!
! measured variable 42 has to be calculated here
! because it isn't calculated in the teprob.f
!
    xmeas(42)=(62.0*xmeas(40))/(76*xmeas(41))

!   relay testing
    if(dumm > 0.5)then
        if(fxmeas(rlout) > rlset)then
            xmv(rlin)=xmv0(rlin)-signp*pert         
        elseif(fxmeas(rlout) < rlset)then
            xmv(rlin)=xmv0(rlin)+signp*pert          
        endif
    endif

!   reactor temperature control (reactor temperature to cooling water flow)
    err=setpt(1)-xmeas(9)
    xmv(10) = xmv(10) &
            + gain(1)*((err-errold(1))+err*deltat*60./taui(1))
    errold(1)=err
    xmv(10)=max(0.d0,min(100.d0,xmv(10)))
!   reactor level control:
    err=setpt(2)-fxmeas(8)
    xmv(2)=xmv(2) &
            + gain(2)*((err-errold(2))+err*deltat*60./taui(2))
    errold(2)=err
    xmv(2)=max(0.d0,min(100.d0,xmv(2)))
!   product separator level control:
    err=setpt(3)-fxmeas(12)
    xmv(11)=xmv(11) &
            + gain(3)*((err-errold(3))+err*deltat*60./taui(3))
    errold(3)=err
    xmv(11)=max(0.d0,min(100.d0,xmv(11)))
!   stripper level control:
    err=setpt(4)-fxmeas(15)
    xmv(7)=xmv(7) &
            + gain(4)*((err-errold(4))+err*deltat*60./taui(4))
    errold(4)=err
    xmv(7)=max(0.d0,min(100.d0,xmv(7)))
!   stripper underflow control:
    err=setpt(5)-xmeas(17)
    xmv(8)=xmv(8) &
                + gain(5)*((err-errold(5))+err*deltat*60./taui(5))
    errold(5)=err
    xmv(8)=max(0.d0,min(100.d0,xmv(8)))
!   g/h ratio control:
    err=setpt(6)-xmeas(42)
    xmv(1)=xmv(1) &
            + gain(6)*((err-errold(6))+err*deltat*60./taui(6))
    errold(6)=err
    xmv(1)=max(0.d0,min(100.d0,xmv(1)))
!   reactor pressure control:
    err=setpt(7)-xmeas(7)
    xmv(4)=xmv(4) &
            + gain(7)*((err-errold(7))+err*deltat*60./taui(7))
    errold(7)=err
    xmv(4)=max(0.d0,min(100.d0,xmv(4)))
!   purge gas b component control:
    err=setpt(8)-xmeas(30)
    xmv(6)=xmv(6) &
            + gain(8)*((err-errold(8))+err*deltat*60./taui(8))
    errold(8)=err
    xmv(6)=max(0.d0,min(100.d0,xmv(6)))
!   reactor feed a component control:
    err=setpt(9)-xmeas(23)
    xmv(3)=xmv(3) &
            + gain(9)*((err-errold(9))+err*deltat*60./taui(9))
    errold(9)=err
    xmv(3)=max(0.d0,min(100.d0,xmv(3)))

!   gives a bounded random variable dummy
    idum=1
    dummy=dble(ran1(idum))

!   if want to use a random binary signal of  clockwidth equal to n
!   samples and upper limit 1, lower limit 0 could add
!     icount(1)=icount(1)+1
!     n=10
!     if(mod(icount(1),n) ==  0)bdummy=dnint(dummy)
!     write(6,*) dummy,bdummy
    istart=1
    return
end

!===============================================================================
!
subroutine output(time)
    save icount
!   measurement and valve common block
    double precision xmeas,xmv,time,fxmeas,xmv0,taufil,alpha
    common/pv/ xmeas(42), xmv(12)
    common/filter/ fxmeas(22), xmv0(12),taufil(22),alpha(22)
    data icount /0/

!   write matlab output every n samples
    n=1
    if(mod(icount,n) ==  0)then
!       matlab output
        write(30,101) time, xmeas(8),xmeas(9),xmeas(12),xmeas(15), &
        xmeas(7),xmeas(17),xmeas(30),xmeas(40)/xmeas(41),xmv(2), &
        xmv(10),xmv(11),xmv(7),xmv(4),xmv(8),xmv(6),xmv(1), &
        xmeas(23)/xmeas(25),xmv(3)
        write(40,102) time, (fxmeas(k), k=1,22),(xmeas(k),k=23,42)
        write(50,103) time, (xmv(k),k=1,12)
    endif
    icount=icount+1
!   use this sort of format statement for output for matlab (all data on a single
!   line), the first number is the number of data entries
101 format(21e23.15)
102 format(43e23.15)
103 format(13e23.15)
    return
end
!
!===============================================================================
!
subroutine intgtr(nn,time,deltat,state,derivative)
!   euler integration algorithm
    integer i, nn
    double precision time, deltat, state(nn), derivative(nn)
    call tefunc(nn,time,state,derivative)
    time = time + deltat
    do i = 1, nn
        state(i) = state(i) + derivative(i) * deltat
    end do
    return
end

real function ran1(idum)
!   FIXME: coerced to real(4), and actually breaks if set to double.  Why? 
!   see numerical recipes in fortran, p.196
!   returns 0.0 to 1.0 random deviate. set idum to any negative value to
!   initialise
    double precision r(97)
    !dimension r(97) !real(4)
    save ix1,ix2,ix3,r
    parameter(m1=259200,ia1=7141,ic1=54773,rm1=1./m1)
    parameter(m2=134456,ia2=8121,ic2=28411,rm2=1./m2)
    parameter(m3=243000,ia3=4561,ic3=51349)
    data iff /0/
    if(idum < 0 .or. iff == 0) then
        iff=1
        ix1=mod(ic1-idum,m1)
        ix1=mod(ia1*ix1+ic1,m1)
        ix2=mod(ix1,m2)
        ix1=mod(ia1*ix1+ic1,m1)
        ix3=mod(ix1,m3)
        do j=1,97
            ix1=mod(ia1*ix1+ic1,m1)
            ix2=mod(ia2*ix2+ic2,m2)
            r(j)=(float(ix1)+float(ix2)*rm2)*rm1
        end do
        idum=1
    endif
    ix1=mod(ia1*ix1+ic1, m1)
    ix2=mod(ia2*ix2+ic2, m2)
    ix3=mod(ia3*ix3+ic3, m3)
    j=1+(97*ix3)/m3
    !if(j > 97 .or. j < 1) pause
    ran1=r(j)
    r(j)=(float(ix1)+float(ix2)*rm2)*rm1
    return
end