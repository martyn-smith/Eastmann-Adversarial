!==============================================================================
!               tennessee eastman process control test problem
!
!                    james j. downs and ernest f. vogel
!
!                  process and control systems engineering
!                        tennessee eastman company
!                              p.o. box 511
!                          kingsport,tn  37662
!
!  reference:
!    "a plant-wide industrial process control problem"
!    presented at the aiche 1990 annual meeting
!    industrial challenge problems in process control,paper #24a
!    chicago,illinois,november 14,1990
!
!  subroutines:
!
!    tefunc - function evaluator to be called by integrator
!    teinit - initialization
!    tesubi - utility subroutines, i=1,2,..,8
!
!
!  the process simulation has 50 states (nn=50).  if the user wishes to 
!  integrate additional states, nn must be increased accordingly in the calling
!  program.  the additional states should be appended to the end of the state 
!  vector, e.g. state(51),...  the additional derivatives should be appended to 
!  the end of the derivative vector, e.g. derivative(51),...  to initialize the new states 
!  and to calculate derivatives for them, we suggest creating new function 
!  evaluator and initialization routines as follows.
!
!          c-----------------------------------------------
!          c
!                subroutine func(nn,time,state,derivative)
!          c
!                integer nn
!                real(kind=8) time, state(nn), derivative(nn)
!          c
!          c  call the function evaluator for the process
!          c 
!                call tefunc(nn,time,state,derivative)
!          c
!          c  calculate derivatives for additional states
!          c
!                derivative(51) = ....
!                derivative(52) = ....
!                   .
!                   .
!                   .
!                derivative(nn) = ....
!          c
!                return
!                end
!          c
!          c-----------------------------------------------
!          c
!                subroutine init(nn,time,state,derivative)
!          c
!                integer nn
!                real(kind=8) time, state(nn), derivative(nn)
!          c
!          c  call the initialization for the process
!          c 
!                call teinit(nn,time,state,derivative)
!          c
!          c  initialize additional states
!          c
!                state(51) = ....
!                state(52) = ....
!                   .
!                   .
!                   .
!                state(nn) = ....
!          c
!                return
!                end
!          c
!          c-----------------------------------------------
!
!  differences between the code and its description in the paper:
!
!  1.  subroutine teinit has time in the argument list.  teinit sets time
!      to zero.
!
!  2.  there are 8 utility subroutines (tesubi) rather than 5.
!
!  3.  process disturbances 14 through 20 do not need to be used in
!      conjunction  with another 
!      disturbance as stated in the paper.  all disturbances can
!      be used alone or in any combination.
!
!  manipulated variables
!
!    xmv(1)     a feed flow (stream 1)
!    xmv(2)     d feed flow (stream 2)
!    xmv(3)     e feed flow (stream 3)
!    xmv(4)     a and c feed flow (stream 4)
!    xmv(5)     compressor recycle valve
!    xmv(6)     purge valve (stream 9)
!    xmv(7)     separator pot liquid flow (stream 10)
!    xmv(8)     stripper liquid product flow (stream 11)
!    xmv(9)     stripper steam valve
!    xmv(10)    reactor cooling water flow
!    xmv(11)    condenser cooling water flow
!    xmv(12)    agitator speed
!
!  continuous process measurements
!
!    xmeas(1)   a feed  (stream 1)                    kscmh
!    xmeas(2)   d feed  (stream 2)                    kg/hr
!    xmeas(3)   e feed  (stream 3)                    kg/hr
!    xmeas(4)   a and c feed  (stream 4)              kscmh
!    xmeas(5)   recycle flow  (stream 8)              kscmh
!    xmeas(6)   reactor feed rate  (stream 6)         kscmh
!    xmeas(7)   reactor pressure                      kpa gauge
!    xmeas(8)   reactor level                         %
!    xmeas(9)   reactor temperature                   deg c
!    xmeas(10)  purge rate (stream 9)                 kscmh
!    xmeas(11)  product sep temp                      deg c
!    xmeas(12)  product sep level                     %
!    xmeas(13)  prod sep pressure                     kpa gauge
!    xmeas(14)  prod sep underflow (stream 10)        m3/hr
!    xmeas(15)  stripper level                        %
!    xmeas(16)  stripper pressure                     kpa gauge
!    xmeas(17)  stripper underflow (stream 11)        m3/hr
!    xmeas(18)  stripper temperature                  deg c
!    xmeas(19)  stripper steam flow                   kg/hr
!    xmeas(20)  compressor work                       kw
!    xmeas(21)  reactor cooling water outlet temp     deg c
!    xmeas(22)  separator cooling water outlet temp   deg c
!
!  sampled process measurements
!
!    reactor feed analysis (stream 6)
!        sampling frequency = 0.1 hr
!        dead time = 0.1 hr
!        mole %
!    xmeas(23)   component a   
!    xmeas(24)   component b   
!    xmeas(25)   component c   
!    xmeas(26)   component d   
!    xmeas(27)   component e   
!    xmeas(28)   component f   
!
!    purge gas analysis (stream 9)
!        sampling frequency = 0.1 hr
!        dead time = 0.1 hr
!        mole %
!    xmeas(29)   component a   
!    xmeas(30)   component b   
!    xmeas(31)   component c   
!    xmeas(32)   component d   
!    xmeas(33)   component e   
!    xmeas(34)   component f   
!    xmeas(35)   component g   
!    xmeas(36)   component h   
!
!    product analysis (stream 11)
!        sampling frequency = 0.25 hr
!        dead time = 0.25 hr
!        mole %
!    xmeas(37)   component d   
!    xmeas(38)   component e   
!    xmeas(39)   component f   
!    xmeas(40)   component g   
!    xmeas(41)   component h   
!
!  process disturbances
!
!    idv(1)   a/c feed ratio, b composition constant (stream 4)          step
!    idv(2)   b composition, a/c ratio constant (stream 4)               step
!    idv(3)   d feed temperature (stream 2)                              step
!    idv(4)   reactor cooling water inlet temperature                    step
!    idv(5)   condenser cooling water inlet temperature                  step
!    idv(6)   a feed loss (stream 1)                                     step
!    idv(7)   c header pressure loss - reduced availability (stream 4)   step
!    idv(8)   a, b, c feed composition (stream 4)            random variation
!    idv(9)   d feed temperature (stream 2)                  random variation
!    idv(10)  c feed temperature (stream 4)                  random variation
!    idv(11)  reactor cooling water inlet temperature        random variation
!    idv(12)  condenser cooling water inlet temperature      random variation
!    idv(13)  reaction kinetics                                    slow drift
!    idv(14)  reactor cooling water valve                            sticking
!    idv(15)  condenser cooling water valve                          sticking
!    idv(16)  random xmeas                                   Failure, 0.012 to 0.027 hr
!    idv(17)  unknown
!    idv(18)  unknown
!    idv(19)  unknown
!    idv(20)  unknown
!    idv(21)  Reactor T (°C),                                 Integrity attack, 0.012 to 0.027 hr
!    idv(22)  mv 7, xmeas(14), xmeas(16)                      DDoS, 663 to 25019 hr
!    idv(23)  D feed flow (mv(0))                             DDoS, 10 hr
!    idv(24)  C feed (mv(3)), Purge flow (mv(5)), Stripper underflow (meas(16)),
!             Stripper steam (xmeas(8))                       Noise, 7,727 to 71,291 h.
!
!===============================================================================
!
subroutine teinit(nn, time, state, derivative)
!
!   initialization
!
!   inputs:
!        nn = number of differential equations
!
!   mutates:
!       time = current time(hrs)
!       state = current state values
!       derivative = current derivative values

!   common block
    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)

    integer :: idv
    common /dvec/ idv(24)

    real(kind=8) :: g
    common /randsd/ g

    integer :: ivst
    real(kind=8) :: &
    uclr, ucvr, utlr, utvr, xlr, xvr, etr, esr, tcr, tkr, dlr, vlr, &
    vvr, vtr, ptr, ppr, fwr, twr, qur, hwr, &
    crxr, rr, rh, uar, &
    ucls, ucvs, utls, utvs, xls, xvs, ets, ess, tcs, tks, dls, vls, &
    vvs, vts, pts, pps, fws, tws, qus, hws, &
    uclc, utlc, xlc, etc, esc, tcc, dlc, vlc, vtc, quc, &
    ucvv, utvv, xvv, etv, esv, tcv, tkv, vtv, ptv, &
    vcv, vrng, vtau, &
    ftm, fcm, xst, xmws, hst, tst, sfr, &
    cpflmx, cpprmx, cpdh, tcwr, tcws, &
    htr, agsp, xdel, xns, t_gas, t_prod, vst
!   Possible bug in f2py - does not respect bang comments
!   Reactor properties (24)
!   Seperator properties (20)
!   Stripper properties? (10)
!   Condensor properties? (9)
    common /teproc/ &
    uclr(8), ucvr(8), utlr, utvr, xlr(8), xvr(8), etr, esr, tcr, tkr, dlr, vlr, &
    vvr, vtr, ptr, ppr(8), fwr, twr, qur, hwr, &
    crxr(8), rr(4), rh, uar, &
    ucls(8), ucvs(8), utls, utvs, xls(8), xvs(8), ets, ess, tcs, tks, dls, vls, &
    vvs, vts, pts, pps(8), fws, tws, qus, hws, &
    uclc(8), utlc, xlc(8), etc, esc, tcc, dlc, vlc, vtc, quc, &
    ucvv(8), utvv, xvv(8), etv, esv, tcv, tkv, vtv, ptv, &
!   XMV
    vcv(12), vrng(12), vtau(12), &
!   stream and component properties
    ftm(13), fcm(8,13), xst(8,13), xmws(13), hst(13), tst(13), sfr(8), &
    cpflmx, cpprmx, cpdh, tcwr, tcws, &
    htr(3), agsp, xdel(41), xns(41), &
    t_gas, t_prod, vst(12), ivst(12)
    
    integer :: idvwlk
    real(kind=8) :: &
    adist, bdist, cdist, ddist, &
    tlast, tnext, &
    hspan, hzero, sspan, szero, &
    spspan
    common /wlk/ &
    adist(12), bdist(12), cdist(12), ddist(12), &
    tlast(12), tnext(12), &
    hspan(12), hzero(12), sspan(12), szero(12), &
    spspan(12), idvwlk(12)

    real(kind=8) :: &
    avp,bvp,cvp, &
    ah,bh,ch, &
    ag,bg,cg, &
    av, &
    ad,bd,cd, &
    xmw
    common /const/ &
    avp(8),bvp(8),cvp(8), &
    ah(8),bh(8),ch(8), &
    ag(8),bg(8),cg(8), &
    av(8), &
    ad(8),bd(8),cd(8), &
    xmw(8)
!   common block

    integer :: i, nn
    real(kind=8) :: state(nn), derivative(nn), time

    ! fortran to python headers
    ! f2py intent(in) nn, time, state, derivative
    ! f2py intent(out) state, derivative

    ! pub const AG : [f64; 8] = [3.411e-6, 0.3799e-6, 0.2491e-6, 0.3567e-6, 0.3463e-6, 0.393e-6, 0.17e-6, 0.150e-6];
    ! pub const BG : [f64; 8] = [7.18e-10, 1.08e-9, 1.36e-11, 8.51e-10, 8.96e-10, 1.02e-9, 0., 0.];
    ! pub const CG : [f64; 8] = [6.0e-13, -3.98e-13, -3.93e-14, -3.12e-13, -3.27e-13,-3.12e-13, 0., 0.];
    
    ! pub const P_REACTOR_HIGH : f64 = 2985.0;
    ! pub const T_REACTOR_HIGH : f64 = 150.0;
    ! pub const P_REACTOR_HIGH_HIGH : f64 = 3000.0;
    ! pub const T_REACTOR_HIGH_HIGH : f64 = 175.0; // degC
    
    ! pub const valve_ranges : [f64; 12] = 
    !                           [400.00, 400.00, 100.00, 1500.00,
    !                            NAN, NAN, 1500.00, 1000.00,
    !                            0.03, 1000.0, 1200.0, NAN];
    ! pub const s_zero : [f64; 12] = 
    !                           [0.485, 0.005, 45.0, 45.0, 
    !                            35.0, 40.0, 1.0, 1.0,
    !                            0.0, 0.0, 0.0, 0.0];
    ! pub const h_span : [f64; 12] = 
    !                           [0.2, 0.7, 0.25, 0.7,
    !                            0.15, 0.15, 1.0, 1.0,
    !                            0.4, 1.5, 2.0, 1.5];
    ! pub const h_zero : [f64; 12] = 
    !                           [0.5, 1.0, 0.5, 1.0,
    !                           0.25, 0.25, 2.0, 2.0,
    !                           0.5, 2.0, 3.0, 2.0];
    ! pub const s_span : [f64; 12] = 
    !                           [0.03, 0.003, 10.0, 10.0,
    !                            10.0, 10.0, 0.25, 0.25,
    !                            0.25, 0.0, 0.0, 0.0];
    ! pub const sp_span : [f64; 12] = 
    !                           [0.0, 0.0, 0.0, 0.0,
    !                            0.0, 0.0, 0.0, 0.0,
    !                            0.0, 0.0, 0.0, 0.0];
    xmw = [2.0, 25.4, 28.0, 32.0, 46.0, 48.0, 62.0, 76.0]
    avp = [0.0, 0.0, 0.0, 15.92, 16.35, 16.35, 16.43, 17.21]
    bvp = [0.0, 0.0, 0.0, -1444.0, -2114.0, -2114.0, -2748.0, -3318.0]
    cvp = [0.0, 0.0, 0.0, 259.0, 265.5, 265.5, 232.9, 249.6]
    ad = [1.0, 1.0, 1.0, 23.3, 33.9, 32.8, 49.9, 50.5]
    bd = [0.0, 0.0, 0.0, -0.0700, -0.0957, -0.0995, -0.0191, -0.0541]
    cd = [0.0, 0.0, 0.0, -0.0002, -0.000152, -0.000233, -0.000425, -0.000150]
    ah = [1.0e-6, 1.0e-6, 1.0e-6, 0.96e-6, 0.573e-6, 0.652e-6, 0.515e-6, 0.471e-6]
    bh = [0., 0., 0., 8.70e-9, 2.41e-9, 2.18e-9, 5.65e-10, 8.70e-10]
    ch = [0., 0., 0., 4.81e-11, 1.82e-11, 1.94e-11, 3.82e-12, 2.62e-12]
    av = [1.0e-6, 1.0e-6, 1.0e-6, 86.7e-6, 160.0e-6, 160.0e-6, 225.0e-6, 209.0e-6]

    ag(1)=3.411e-6
    ag(2)=0.3799e-6
    ag(3)=0.2491e-6
    ag(4)=0.3567e-6
    ag(5)=0.3463e-6
    ag(6)=0.3930e-6
    ag(7)=0.170e-6
    ag(8)=0.150e-6
    bg(1)=7.18e-10
    bg(2)=1.08e-9
    bg(3)=1.36e-11
    bg(4)=8.51e-10
    bg(5)=8.96e-10
    bg(6)=1.02e-9
    bg(7)=0.
    bg(8)=0.
    cg(1)=6.0e-13
    cg(2)=-3.98e-13
    cg(3)=-3.93e-14
    cg(4)=-3.12e-13
    cg(5)=-3.27e-13
    cg(6)=-3.12e-13
    cg(7)=0.
    cg(8)=0.
    state(1)=10.40491389
    state(2)=4.363996017
    state(3)=7.570059737
    state(4)=0.4230042431
    state(5)=24.15513437
    state(6)=2.942597645
    state(7)=154.3770655
    state(8)=159.1865960
    state(9)=2.808522723
    state(10)=63.75581199
    state(11)=26.74026066
    state(12)=46.38532432
    state(13)=0.2464521543
    state(14)=15.20484404
    state(15)=1.852266172
    state(16)=52.44639459
    state(17)=41.20394008
    state(18)=0.5699317760
    state(19)=0.4306056376
    state(20)=7.9906200783e-03
    state(21)=0.9056036089
    state(22)=1.6054258216e-02
    state(23)=0.7509759687
    state(24)=8.8582855955e-02
    state(25)=48.27726193
    state(26)=39.38459028
    state(27)=0.3755297257
    state(28)=107.7562698
    state(29)=29.77250546
    state(30)=88.32481135
    state(31)=23.03929507
    state(32)=62.85848794
    state(33)=5.546318688
    state(34)=11.92244772
    state(35)=5.555448243 
    state(36)=0.9218489762
    state(37)=94.59927549
    state(38)=77.29698353
    state(39)=63.05263039
    state(40)=53.97970677
    state(41)=24.64355755
    state(42)=61.30192144
    state(43)=22.21000000
    state(44)=40.06374673
    state(45)=38.10034370
    state(46)=46.53415582
    state(47)=47.44573456
    state(48)=41.10581288
    state(49)=18.11349055
    state(50)=50.00000000
    do i=1,12 !label 200
        xmv(i)=state(i+38)
        vcv(i)=xmv(i)
        vst(i)=2.0
        ivst(i)=0
    end do
    vrng(1)=400.00
    vrng(2)=400.00
    vrng(3)=100.00
    vrng(4)=1500.00
    vrng(7)=1500.00
    vrng(8)=1000.00
    vrng(9)=0.03
    vrng(10)=1000.
    vrng(11)=1200.0
    vtr=1300.0
    vts=3500.0
    vtc=156.5
    vtv=5000.0
    htr(1)=0.06899381054
    htr(2)=0.05
    hwr=7060.
    hws=11138.
    sfr(1)=0.99500
    sfr(2)=0.99100
    sfr(3)=0.99000
    sfr(4)=0.91600
    sfr(5)=0.93600
    sfr(6)=0.93800
    sfr(7)=5.80000e-02
    sfr(8)=3.01000e-02
    xst(1,1)=0.0
    xst(2,1)=0.0001
    xst(3,1)=0.0
    xst(4,1)=0.9999
    xst(5,1)=0.0
    xst(6,1)=0.0
    xst(7,1)=0.0
    xst(8,1)=0.0
    tst(1)=45.
    xst(1,2)=0.0
    xst(2,2)=0.0
    xst(3,2)=0.0
    xst(4,2)=0.0
    xst(5,2)=0.9999
    xst(6,2)=0.0001
    xst(7,2)=0.0
    xst(8,2)=0.0
    tst(2)=45.
    xst(1,3)=0.9999
    xst(2,3)=0.0001
    xst(3,3)=0.0
    xst(4,3)=0.0
    xst(5,3)=0.0
    xst(6,3)=0.0
    xst(7,3)=0.0
    xst(8,3)=0.0
    tst(3)=45.
    xst(1,4)=0.4850
    xst(2,4)=0.0050
    xst(3,4)=0.5100
    xst(4,4)=0.0
    xst(5,4)=0.0
    xst(6,4)=0.0
    xst(7,4)=0.0
    xst(8,4)=0.0
    tst(4)=45.
    cpflmx=280275.
    cpprmx=1.3
    vtau(1)=8.
    vtau(2)=8.
    vtau(3)=6.
    vtau(4)=9.
    vtau(5)=7.
    vtau(6)=5.
    vtau(7)=5.
    vtau(8)=5.
    vtau(9)=120.
    vtau(10)=5.
    vtau(11)=5.
    vtau(12)=5.
    do i=1,size(vtau) !label 300
        vtau(i)=vtau(i)/3600.
    end do
    g=1431655765.
    xns(1)=0.0012
    xns(2)=18.000
    xns(3)=22.000
    xns(4)=0.0500
    xns(5)=0.2000
    xns(6)=0.2100
    xns(7)=0.3000
    xns(8)=0.5000
    xns(9)=0.0100
    xns(10)=0.0017
    xns(11)=0.0100
    xns(12)=1.0000
    xns(13)=0.3000
    xns(14)=0.1250
    xns(15)=1.0000
    xns(16)=0.3000
    xns(17)=0.1150
    xns(18)=0.0100
    xns(19)=1.1500
    xns(20)=0.2000
    xns(21)=0.0100
    xns(22)=0.0100
    xns(23)=0.250
    xns(24)=0.100
    xns(25)=0.250
    xns(26)=0.100
    xns(27)=0.250
    xns(28)=0.025
    xns(29)=0.250
    xns(30)=0.100
    xns(31)=0.250
    xns(32)=0.100
    xns(33)=0.250
    xns(34)=0.025
    xns(35)=0.050
    xns(36)=0.050
    xns(37)=0.010
    xns(38)=0.010
    xns(39)=0.010
    xns(40)=0.500
    xns(41)=0.500
    do i=1,size(idv) !label 500
        idv(i)=0
    end do
    hspan(1)=0.2
    hzero(1)=0.5
    sspan(1)=0.03
    szero(1)=0.485
    spspan(1)=0.
    hspan(2)=0.7
    hzero(2)=1.0
    sspan(2)=.003
    szero(2)=.005
    spspan(2)=0.
    hspan(3)=0.25
    hzero(3)=0.5
    sspan(3)=10.
    szero(3)=45.
    spspan(3)=0.
    hspan(4)=0.7
    hzero(4)=1.0
    sspan(4)=10.
    szero(4)=45.
    spspan(4)=0.
    hspan(5)=0.15
    hzero(5)=0.25
    sspan(5)=10.
    szero(5)=35.
    spspan(5)=0.
    hspan(6)=0.15
    hzero(6)=0.25
    sspan(6)=10.
    szero(6)=40.
    spspan(6)=0.
    hspan(7)=1.
    hzero(7)=2.
    sspan(7)=0.25
    szero(7)=1.0
    spspan(7)=0.
    hspan(8)=1.
    hzero(8)=2.
    sspan(8)=0.25
    szero(8)=1.0
    spspan(8)=0.
    hspan(9)=0.4
    hzero(9)=0.5
    sspan(9)=0.25
    szero(9)=0.0
    spspan(9)=0.
    hspan(10)=1.5
    hzero(10)=2.0
    sspan(10)=0.0
    szero(10)=0.0
    spspan(10)=0.
    hspan(11)=2.0
    hzero(11)=3.0
    sspan(11)=0.0
    szero(11)=0.0
    spspan(11)=0.
    hspan(12)=1.5
    hzero(12)=2.0
    sspan(12)=0.0
    szero(12)=0.0
    spspan(12)=0.
    do i=1,12 !label 550
        tlast(i)=0.
        tnext(i)=0.1
        adist(i)=szero(i)
        bdist(i)=0.
        cdist(i)=0.
        ddist(i)=0.
    end do
    time = 0.0
    call tefunc(nn, time, state, derivative)
    return
end !teinit
!
!==============================================================================
!
subroutine tefunc(nn, time, state, derivative)
!   function evaluator
!
!   inputs:
!     nn = number of differential equations
!     time = current time(hrs)
!     state = current state values
!
!   mutates:
!     derivative = current derivative values

!   common block
    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)

    integer :: idv
    common /dvec/ idv(24)

    integer :: ivst
    real(kind=8) :: &
    uclr, ucvr, utlr, utvr, xlr, xvr, etr, esr, tcr, tkr, dlr, vlr, &
    vvr, vtr, ptr, ppr, fwr, twr, qur, hwr, &
    crxr, rr, rh, uar, &
    ucls, ucvs, utls, utvs, xls, xvs, ets, ess, tcs, tks, dls, vls, &
    vvs, vts, pts, pps, fws, tws, qus, hws, &
    uclc, utlc, xlc, etc, esc, tcc, dlc, vlc, vtc, quc, &
    ucvv, utvv, xvv, etv, esv, tcv, tkv, vtv, ptv, &
    vcv, vrng, vtau, &
    ftm, fcm, xst, xmws, hst, tst, sfr, &
    cpflmx, cpprmx, cpdh, tcwr, tcws, &
    htr, agsp, xdel, xns, t_gas, t_prod, vst
    common /teproc/ &
    uclr(8), ucvr(8), utlr, utvr, xlr(8), xvr(8), etr, esr, tcr, tkr, dlr, vlr, & 
    vvr, vtr, ptr, ppr(8), fwr, twr, qur, hwr, &
    crxr(8), rr(4), rh, uar, &
    ucls(8), ucvs(8), utls, utvs, xls(8), xvs(8), ets, ess, tcs, tks, dls, vls, &
    vvs, vts, pts, pps(8), fws, tws, qus, hws, &
    uclc(8), utlc, xlc(8), etc, esc, tcc, dlc, vlc, vtc, quc, &
    ucvv(8), utvv, xvv(8), etv, esv, tcv, tkv, vtv, ptv, &
    vcv(12),vrng(12),vtau(12), &
    ftm(13), fcm(8,13), xst(8,13), xmws(13), hst(13),tst(13), sfr(8), &
    cpflmx, cpprmx, cpdh, tcwr, tcws, &
    htr(3), agsp, xdel(41), xns(41), &
    t_gas, t_prod, vst(12), ivst(12)

    integer :: idvwlk
    real(kind=8) :: &
    adist, bdist, cdist, ddist, &
    tlast, tnext, &
    hspan, hzero, sspan, szero, &
    spspan
    common /wlk/ &
    adist(12), bdist(12), cdist(12), ddist(12), &
    tlast(12), tnext(12), &
    hspan(12), hzero(12), sspan(12), szero(12), &
    spspan(12), idvwlk(12)

    real(kind=8) :: &
    avp,bvp,cvp, &
    ad,bd,cd, &
    av, &
    ah,bh,ch, &
    ag,bg,cg, &
    xmw
    common /const/ &
    avp(8), bvp(8), cvp(8), &
    ah(8), bh(8), ch(8), &
    ag(8), bg(8), cg(8), &
    av(8), &
    ad(8), bd(8), cd(8), &
    xmw(8)
!   common block

    integer :: i, nn, isd
    real(kind=8) :: rg, &
    vpr, &
    fin(8), &
    time, state(nn), derivative(nn), &
    flms, dlp, &
    pr, &
    flcoef, &
    uas, uac, &
    vovrl, &
    uarlev, &
    vpos(12), &
    xmns, &
    xcmp(41), &
    tmpfac, &
    r1f, r2f, &
    hwlk, swlk, spwlk, &
    tesub7, tesub8

    do i=1,size(idv) !label 500
        if(idv(i) > 0) then
            idv(i)=1
        else
            idv(i)=0
        end if
    end do
    idvwlk(1)=idv(8)
    idvwlk(2)=idv(8)
    idvwlk(3)=idv(9)
    idvwlk(4)=idv(10)
    idvwlk(5)=idv(11)
    idvwlk(6)=idv(12)
    idvwlk(7)=idv(13)
    idvwlk(8)=idv(13)
    idvwlk(9)=idv(16)
    idvwlk(10)=idv(17)
    idvwlk(11)=idv(18)
    idvwlk(12)=idv(24)
    do  i=1,9 !label 900
        if(time >= tnext(i)) then
            hwlk=tnext(i)-tlast(i)
            swlk=adist(i)+hwlk*(bdist(i)+hwlk* &
            (cdist(i)+hwlk*ddist(i)))
            spwlk=bdist(i)+hwlk* &
            (2.*cdist(i)+3.*hwlk*ddist(i))
            tlast(i)=tnext(i)
            call tesub5(swlk,spwlk,adist(i),bdist(i),cdist(i), &
            ddist(i),tlast(i),tnext(i),hspan(i),hzero(i), &
            sspan(i),szero(i),spspan(i),idvwlk(i))
        end if
    end do
    do i=10,12 !label 910
        if(time >= tnext(i)) then
            hwlk = tnext(i)-tlast(i)
            swlk = adist(i)+hwlk*(bdist(i)+hwlk* &
            (cdist(i)+hwlk*ddist(i)))
            spwlk=bdist(i)+hwlk* &
            (2.*cdist(i)+3.*hwlk*ddist(i))
            tlast(i)=tnext(i)
            if(swlk > 0.1) then
                adist(i)=swlk
                bdist(i)=spwlk
                cdist(i)=-(3.*swlk+0.2*spwlk)/0.01
                ddist(i)=(2.*swlk+0.1*spwlk)/0.001
                tnext(i)=tlast(i)+0.1
            else
                isd=-1
                hwlk=hspan(i)*tesub7(isd)+hzero(i)
                adist(i)=0.
                bdist(i)=0.
                cdist(i)=idvwlk(i)/hwlk**2
                ddist(i)=0.
                tnext(i)=tlast(i)+hwlk
            end if
        end if
    end do
    if(time == 0.) then
        do i=1,12 !label 950
            adist(i)=szero(i)
            bdist(i)=0.
            cdist(i)=0.
            ddist(i)=0.
            tlast(i)=0.0
            tnext(i)=0.1
        end do
    end if
    xst(1,4)=tesub8(1, time)-idv(1)*0.03 -idv(2)*2.43719e-3
    xst(2,4)=tesub8(2, time)+idv(2)*0.005
    xst(3,4)=1.-xst(1,4)-xst(2,4)
    tst(1)=tesub8(3, time)+idv(3)*5.
    tst(4)=tesub8(4, time)
    tcwr=tesub8(5, time)+idv(4)*5.
    tcws=tesub8(6, time)+idv(5)*5.
    r1f=tesub8(7, time)
    r2f=tesub8(8, time)
    do i=1,3 !label 1010
        ucvr(i)=state(i)
        ucvs(i)=state(i+9)
        uclr(i)=0.0
        ucls(i)=0.0
    end do
    do i=4,8 !label 1020
        uclr(i)=state(i)
        ucls(i)=state(i+9)
    end do
    do  i=1,8 !label 1030
        uclc(i)=state(i+18)
        ucvv(i)=state(i+27)
    end do
    etr=state(9)
    ets=state(18)
    etc=state(27)
    etv=state(36)
    twr=state(37)
    tws=state(38)
    do i=1,size(vpos) !label 1035
        vpos(i)=state(i+38)
    end do
    utlr=0.0
    utls=0.0
    utlc=0.0
    utvv=0.0
    do i=1,8 !label 1040
        utlr=utlr+uclr(i)
        utls=utls+ucls(i)
        utlc=utlc+uclc(i)
        utvv=utvv+ucvv(i)
    end do
    do i=1,8 !label 1050
        xlr(i)=uclr(i)/utlr
        xls(i)=ucls(i)/utls
        xlc(i)=uclc(i)/utlc
        xvv(i)=ucvv(i)/utvv
    end do
    esr=etr/utlr
    ess=ets/utls
    esc=etc/utlc
    esv=etv/utvv
    call tesub2(xlr,tcr,esr,0) !first point TCR is set?
    tkr=tcr+273.15
    call tesub2(xls,tcs,ess,0)
    tks=tcs+273.15
    call tesub2(xlc,tcc,esc,0)
    call tesub2(xvv,tcv,esv,2)
    tkv=tcv+273.15
    call tesub4(xlr,tcr,dlr)
    call tesub4(xls,tcs,dls)
    call tesub4(xlc,tcc,dlc)
    vlr=utlr/dlr
    vls=utls/dls
    vlc=utlc/dlc
    vvr=vtr-vlr
    vvs=vts-vls
    rg=998.9
    ptr=0.0
    pts=0.0
    do i=1,3 !label 1110
        ppr(i)=ucvr(i)*rg*tkr/vvr
        ptr=ptr+ppr(i)
        pps(i)=ucvs(i)*rg*tks/vvs
        pts=pts+pps(i)
    end do
    do i=4,8 !label 1120
        vpr=exp(avp(i)+bvp(i)/(tcr+cvp(i)))
        ppr(i)=vpr*xlr(i)
        ptr=ptr+ppr(i)
        vpr=exp(avp(i)+bvp(i)/(tcs+cvp(i)))
        pps(i)=vpr*xls(i)
        pts=pts+pps(i)
    end do
    ptv=utvv*rg*tkv/vtv
    do i=1,8 !label 1130
        xvr(i)=ppr(i)/ptr
        xvs(i)=pps(i)/pts
    end do
    utvr=ptr*vvr/rg/tkr
    utvs=pts*vvs/rg/tks
    do i=4,8 !label 1140
        ucvr(i)=utvr*xvr(i)
        ucvs(i)=utvs*xvs(i)
    end do
    rr(1)=exp(31.5859536-40000.0/1.987/tkr)*r1f
    rr(2)=exp(3.00094014-20000.0/1.987/tkr)*r2f
    rr(3)=exp(53.4060443-60000.0/1.987/tkr)
    rr(4)=rr(3)*0.767488334
    if(ppr(1) > 0.0.and.ppr(3) > 0.0) then
        r1f=ppr(1)**1.1544
        r2f=ppr(3)**0.3735
        rr(1)=rr(1)*r1f*r2f*ppr(4)
        rr(2)=rr(2)*r1f*r2f*ppr(5)
    else
        rr(1)=0.0
        rr(2)=0.0
    end if
    rr(3)=rr(3)*ppr(1)*ppr(5)
    rr(4)=rr(4)*ppr(1)*ppr(4)
    do i=1,4 !label 1200
        rr(i)=rr(i)*vvr
    end do
    crxr(1)=-rr(1)-rr(2)-rr(3)
    crxr(3)=-rr(1)-rr(2)
    crxr(4)=-rr(1)-1.5*rr(4)
    crxr(5)=-rr(2)-rr(3)
    crxr(6)=rr(3)+rr(4)
    crxr(7)=rr(1)
    crxr(8)=rr(2)
    rh=rr(1)*htr(1)+rr(2)*htr(2)
    xmws(1)=0.0
    xmws(2)=0.0
    xmws(6)=0.0
    xmws(8)=0.0
    xmws(9)=0.0
    xmws(10)=0.0
    do i=1,8 !label 2010
        xst(i,6)=xvv(i)
        xst(i,8)=xvr(i)
        xst(i,9)=xvs(i)
        xst(i,10)=xvs(i)
        xst(i,11)=xls(i)
        xst(i,13)=xlc(i)
        xmws(1)=xmws(1)+xst(i,1)*xmw(i)
        xmws(2)=xmws(2)+xst(i,2)*xmw(i)
        xmws(6)=xmws(6)+xst(i,6)*xmw(i)
        xmws(8)=xmws(8)+xst(i,8)*xmw(i)
        xmws(9)=xmws(9)+xst(i,9)*xmw(i)
        xmws(10)=xmws(10)+xst(i,10)*xmw(i)
    end do
    tst(6)=tcv
    tst(8)=tcr
    tst(9)=tcs
    tst(10)=tcs
    tst(11)=tcs
    tst(13)=tcc
    call tesub1(xst(1,1),tst(1),hst(1),1)
    call tesub1(xst(1,2),tst(2),hst(2),1)
    call tesub1(xst(1,3),tst(3),hst(3),1)
    call tesub1(xst(1,4),tst(4),hst(4),1)
    call tesub1(xst(1,6),tst(6),hst(6),1)
    call tesub1(xst(1,8),tst(8),hst(8),1)
    call tesub1(xst(1,9),tst(9),hst(9),1)
    hst(10)=hst(9)
    call tesub1(xst(1,11),tst(11),hst(11),0)
    call tesub1(xst(1,13),tst(13),hst(13),0)
    ftm(1)=vpos(1)*vrng(1)/100.0
    ftm(2)=vpos(2)*vrng(2)/100.0
    ftm(3)=vpos(3)*(1.-idv(6))*vrng(3)/100.0
    ftm(4)=vpos(4)*(1.-idv(7)*0.2)*vrng(4)/100.0+1.e-10
    ftm(11)=vpos(7)*vrng(7)/100.0
    ftm(13)=vpos(8)*vrng(8)/100.0
    uac=vpos(9)*vrng(9)*(1.+tesub8(9,time))/100.0
    fwr=vpos(10)*vrng(10)/100.0
    fws=vpos(11)*vrng(11)/100.0
    agsp=(vpos(12)+150.0)/100.0
    dlp=max(ptv-ptr, 0.)
    flms=1937.6*sqrt(dlp)
    ftm(6)=flms/xmws(6)
    dlp=max(ptr-pts, 0.)
    flms=4574.21*sqrt(dlp)*(1.-0.25*tesub8(12,time))
    ftm(8)=flms/xmws(8)
    dlp=max(pts-760.0, 0.)
    flms=vpos(6)*0.151169*sqrt(dlp)
    ftm(10)=flms/xmws(10)
    pr=min(max(ptv/pts, 1.), cpprmx)
    flcoef=cpflmx/1.197
    flms=cpflmx+flcoef*(1.0-pr**3)
    cpdh=flms*(tcs+273.15)*1.8e-6*1.9872 *(ptv-pts)/(xmws(9)*pts)
    dlp=max(ptv-pts, 0.)
    flms=max(flms-vpos(5)*53.349*sqrt(dlp), 1.e-3)
    ftm(9)=flms/xmws(9)
    hst(9)=hst(9)+cpdh/ftm(9)
    do i=1,8 !label 5020
        fcm(i,1)=xst(i,1)*ftm(1)
        fcm(i,2)=xst(i,2)*ftm(2)
        fcm(i,3)=xst(i,3)*ftm(3)
        fcm(i,4)=xst(i,4)*ftm(4)
        fcm(i,6)=xst(i,6)*ftm(6)
        fcm(i,8)=xst(i,8)*ftm(8)
        fcm(i,9)=xst(i,9)*ftm(9)
        fcm(i,10)=xst(i,10)*ftm(10)
        fcm(i,11)=xst(i,11)*ftm(11)
        fcm(i,13)=xst(i,13)*ftm(13)
    end do
    if(ftm(11) > 0.1) then
        if(tcc > 170.) then
            tmpfac=tcc-120.262
        elseif(tcc < 5.292) then
            tmpfac=0.1
        else
            tmpfac=363.744/(177.-tcc)-2.22579488
        end if
        vovrl=ftm(4)/ftm(11)*tmpfac
        sfr(4)=8.5010*vovrl/(1.0+8.5010*vovrl)
        sfr(5)=11.402*vovrl/(1.0+11.402*vovrl)
        sfr(6)=11.795*vovrl/(1.0+11.795*vovrl)
        sfr(7)=0.0480*vovrl/(1.0+0.0480*vovrl)
        sfr(8)=0.0242*vovrl/(1.0+0.0242*vovrl)
    else
        sfr(4)=0.9999
        sfr(5)=0.999
        sfr(6)=0.999
        sfr(7)=0.99
        sfr(8)=0.98
    end if
    do i=1,8 !label 6010
        fin(i)=0.0
        fin(i)=fin(i)+fcm(i,4)
        fin(i)=fin(i)+fcm(i,11)
    end do
    ftm(5)=0.0
    ftm(12)=0.0
    do i=1,8 !label 6020
        fcm(i,5)=sfr(i)*fin(i)
        fcm(i,12)=fin(i)-fcm(i,5)
        ftm(5)=ftm(5)+fcm(i,5)
        ftm(12)=ftm(12)+fcm(i,12)
    end do
    do i=1,8 !label 6030
        xst(i,5)=fcm(i,5)/ftm(5)
        xst(i,12)=fcm(i,12)/ftm(12)
    end do
    tst(5)=tcc
    tst(12)=tcc
    call tesub1(xst(1,5),tst(5),hst(5),1)
    call tesub1(xst(1,12),tst(12),hst(12),0)
    ftm(7)=ftm(6)
    hst(7)=hst(6)
    tst(7)=tst(6)
    do i=1,8 !label 6130
        xst(i,7)=xst(i,6)
        fcm(i,7)=fcm(i,6)
    end do
    if(vlr/7.8 > 50.0) then
        uarlev=1.0
    elseif(vlr/7.8 < 10.0) then
        uarlev=0.0
    else
        uarlev=0.025*vlr/7.8-0.25
    end if
    uar=uarlev*(-0.5*agsp**2 + 2.75*agsp-2.5)*855490.e-6
    qur=uar*(twr-tcr) *(1.-0.35*tesub8(10,time))
    uas=0.404655*(1.0-1.0/(1.0+(ftm(8)/3528.73)**4))
    qus=uas*(tws-tst(8)) *(1.-0.25*tesub8(11,time))
    quc=0.
    if(tcc < 100.)quc=uac*(100.0-tcc)
    xmeas(1)=ftm(3)*0.359/35.3145
    xmeas(2)=ftm(1)*xmws(1)*0.454
    xmeas(3)=ftm(2)*xmws(2)*0.454
    xmeas(4)=ftm(4)*0.359/35.3145
    xmeas(5)=ftm(9)*0.359/35.3145
    xmeas(6)=ftm(6)*0.359/35.3145
    xmeas(7)=(ptr-760.0)/760.0*101.325
    xmeas(8)=(vlr-84.6)/666.7*100.0
    xmeas(9)=tcr
    xmeas(10)=ftm(10)*0.359/35.3145
    xmeas(11)=tcs
    xmeas(12)=(vls-27.5)/290.0*100.0
    xmeas(13)=(pts-760.0)/760.0*101.325
    xmeas(14)=ftm(11)/dls/35.3145
    xmeas(15)=(vlc-78.25)/vtc*100.0
    xmeas(16)=(ptv-760.0)/760.0*101.325
    xmeas(17)=ftm(13)/dlc/35.3145
    xmeas(18)=tcc
    xmeas(19)=quc*1.04e3*0.454
    xmeas(20)=cpdh*0.0003927e6
    xmeas(20)=cpdh*0.29307e3
    xmeas(21)=twr
    xmeas(22)=tws
    isd=0
    if(xmeas(7) > 3000.0)isd=1
    if(vlr/35.3145 > 24.0)isd=1
    if(vlr/35.3145 < 2.0)isd=1
    if(xmeas(9) > 175.0)isd=1
    if(vls/35.3145 > 12.0)isd=1
    if(vls/35.3145 < 1.0)isd=1
    if(vlc/35.3145 > 8.0)isd=1
    if(vlc/35.3145 < 1.0)isd=1
    if(isd == 1) then
        write(6,*) 'plant has tripped'
        stop
    end if    
    if(time > 0.0 .and. isd == 0) then
        do i=1,22 !label 6500
            call tesub6(xns(i),xmns)
            xmeas(i)=xmeas(i)+xmns
        end do
    end if
    xcmp(23)=xst(1,7)*100.0
    xcmp(24)=xst(2,7)*100.0
    xcmp(25)=xst(3,7)*100.0
    xcmp(26)=xst(4,7)*100.0
    xcmp(27)=xst(5,7)*100.0
    xcmp(28)=xst(6,7)*100.0
    xcmp(29)=xst(1,10)*100.0
    xcmp(30)=xst(2,10)*100.0
    xcmp(31)=xst(3,10)*100.0
    xcmp(32)=xst(4,10)*100.0
    xcmp(33)=xst(5,10)*100.0
    xcmp(34)=xst(6,10)*100.0
    xcmp(35)=xst(7,10)*100.0
    xcmp(36)=xst(8,10)*100.0
    xcmp(37)=xst(4,13)*100.0
    xcmp(38)=xst(5,13)*100.0
    xcmp(39)=xst(6,13)*100.0
    xcmp(40)=xst(7,13)*100.0
    xcmp(41)=xst(8,13)*100.0
    
    if(time == 0.) then
        do i=23,41 !label 7010
            xdel(i)=xcmp(i)
            xmeas(i)=xcmp(i)
        end do
        t_gas=0.1
        t_prod=0.25
    end if
    if(time >= t_gas) then
        do i=23,36 !label 7020
            xmeas(i)=xdel(i)
            call tesub6(xns(i),xmns)
            xmeas(i)=xmeas(i)+xmns
            xdel(i)=xcmp(i)
        end do
        t_gas=t_gas+0.1
    end if
    if(time >= t_prod) then
        do i=37,41 !label 7030
            xmeas(i)=xdel(i)
            call tesub6(xns(i),xmns)
            xmeas(i)=xmeas(i)+xmns
            xdel(i)=xcmp(i)
        end do
        t_prod=t_prod+0.25
    end if
    do i=1,8 !label 9010
        derivative(i)=fcm(i,7)-fcm(i,8)+crxr(i)
        derivative(i+9)=fcm(i,8)-fcm(i,9)- fcm(i,10)-fcm(i,11)
        derivative(i+18)=fcm(i,12)-fcm(i,13)
        derivative(i+27)=fcm(i,1) + fcm(i,2) + fcm(i,3) + fcm(i,5) + fcm(i,9)- fcm(i,6)
    end do
    derivative(9)=hst(7)*ftm(7)- hst(8)*ftm(8)+rh+qur
    derivative(18)=hst(8)*ftm(8)- hst(9)*ftm(9)- hst(10)*ftm(10)- hst(11)*ftm(11)+ qus
    derivative(27)=hst(4)*ftm(4)+ hst(11)*ftm(11)- hst(5)*ftm(5)- hst(13)*ftm(13)+ quc
    derivative(36)=hst(1)*ftm(1)+ &
                    hst(2)*ftm(2)+ &
                    hst(3)*ftm(3)+ &
                    hst(5)*ftm(5)+ &
                    hst(9)*ftm(9)- &
                    hst(6)*ftm(6)
    derivative(37)=(fwr*500.53* (tcwr-twr)-qur*1.e6/1.8)/hwr
    derivative(38)=(fws*500.53* (tcws-tws)-qus*1.e6/1.8)/hws 
    ivst(10)=idv(14)
    ivst(11)=idv(15)
    ivst(5)=idv(19)
    ivst(7)=idv(19)
    ivst(8)=idv(19)
    ivst(9)=idv(19)
    do i=1,12 !label 9020
        if(time == 0. .or. abs(vcv(i)-xmv(i)) > vst(i)*ivst(i)) vcv(i)=xmv(i)
        if(vcv(i) < 0.0) vcv(i)=0.0
        if(vcv(i) > 100.0) vcv(i)=100.0
        derivative(i+38)=(vcv(i)-vpos(i))/vtau(i)
    end do
    if(isd /= 0) then
        do i=1, size(derivative) !label 9030
            derivative(i)=0.0
        end do
    end if
    return
end !tefunc    
!
!===============================================================================
!
subroutine tesub1(z, t, h, ity)
!   common block
    real(kind=8) :: &
    avp,bvp,cvp, &
    ah,bh,ch, &
    ag,bg,cg, &
    av, &
    ad,bd,cd, &
    xmw
    common /const/ &
    avp(8),bvp(8),cvp(8), &
    ah(8),bh(8),ch(8), &
    ag(8),bg(8),cg(8), &
    av(8), &
    ad(8),bd(8),cd(8), &
    xmw(8)
!   common block

    integer :: i, ity
    real(kind=8) :: h, hi, r, t, z(8)

    if(ity == 0) then
        h = 0.0
        do i=1,8 !label 100
            hi=t*(ah(i)+bh(i)*t/2.+ch(i)*t**2/3.)
            hi=1.8 * hi
            h=h+z(i)*xmw(i)*hi
        end do
    else
        h=0.0
        do i=1,8 !label 200
            hi=t*(ag(i) + bg(i)*t/2. + cg(i)*t**2/3.)
            hi=1.8 * hi
            hi=hi+av(i)
            h=h+z(i)*xmw(i)*hi
        end do
    end if
    if(ity == 2) then
        r=3.57696/1.e6
        h=h-r*(t+273.15)
    end if
    return
end

subroutine tesub2(z, t, h, ity)
!   common block
    real(kind=8) :: &
    avp,bvp,cvp, &
    ah,bh,ch, &
    ag,bg,cg, &
    av, &
    ad,bd,cd, &
    xmw
    common /const/ &
    avp(8),bvp(8),cvp(8), &
    ah(8),bh(8),ch(8), &
    ag(8),bg(8),cg(8), &
    av(8), &
    ad(8),bd(8),cd(8), &
    xmw(8)
!   common block

    integer :: ity, j
    real(kind=8) :: err, dh, dt, h, htest, t, tin, z(8)

    tin=t
    do j=1,100 !label 250
        call tesub1(z, t, htest, ity)
        err = htest - h
        call tesub3(z, t, dh, ity)
        dt=-err/dh
        t=t+dt !main mutator of T
        if(abs(dt) < 1.d-12) return
    end do
    t=tin !this appears to be correct..? i.e. t is reset if error never converges.
    return
end

subroutine tesub3(z, t, dh, ity)
!   common block
    real(kind=8) &
    avp,bvp,cvp, &
    ah,bh,ch, &
    ag,bg,cg, &
    av, &
    ad,bd,cd, &
    xmw
    common /const/ &
    avp(8),bvp(8),cvp(8), &
    ah(8),bh(8),ch(8), &
    ag(8),bg(8),cg(8), &
    av(8), &
    ad(8),bd(8),cd(8), &
    xmw(8)
!   common block

    integer :: i, ity
    real(kind=8) :: dh, dhi, r, t, z(8)

    if(ity == 0) then
        dh=0.0
        do i=1,8 !label 100...again
            dhi=ah(i)+bh(i)*t+ch(i)*t**2
            dhi=1.8*dhi
            dh=dh+z(i)*xmw(i)*dhi
        end do
    else
        dh=0.0
        do i=1,8
            dhi=ag(i)+bg(i)*t+cg(i)*t**2
            dhi=1.8*dhi
            dh=dh+z(i)*xmw(i)*dhi
        end do
    end if
    if(ity == 2) then
        r=3.57696/1.e6
        dh=dh-r
    end if
    return
end

subroutine tesub4(x, t, r)
!   common block
    real(kind=8) &
    avp,bvp,cvp, &
    ah,bh,ch, &
    ag,bg,cg, &
    av, &
    ad,bd,cd, &
    xmw
    common /const/ &
    avp(8),bvp(8),cvp(8), &
    ah(8),bh(8),ch(8), &
    ag(8),bg(8),cg(8), &
    av(8), &
    ad(8),bd(8),cd(8), &
    xmw(8)
!   common block

    integer :: i
    real(kind=8) :: r, t, v, x(8)

    v=0.0
    do i=1,8
        v=v+x(i)*xmw(i)/ (ad(i)+(bd(i)+cd(i)*t)*t)
    end do
    r = 1.0/v
    return
end

subroutine tesub5(s, sp, adist, bdist, cdist, ddist, tlast, tnext, &
                  hspan, hzero, sspan, szero, spspan, idvflag)
    integer :: i, idvflag
    real(kind=8) :: &
    s, sp, h, s1, s1p, &
    adist, bdist, cdist, ddist, &
    tlast, tnext, &
    hspan, hzero, sspan, szero, spspan, &
    tesub7

    i=-1
    h=hspan*tesub7(i)+hzero
    s1=sspan*tesub7(i)*idvflag+szero
    s1p=spspan*tesub7(i)*idvflag
    adist=s
    bdist=sp
    cdist=(3.*(s1-s)-h*(s1p+2.*sp))/h**2
    ddist=(2.*(s-s1)+h*(s1p+sp))/h**3
    tnext=tlast+h
    return
end

subroutine tesub6(std,x)
    integer :: i
    real(kind=8)::  std, x, tesub7
    x=0.
    do i=1,12
        x = x+tesub7(i)
    end do
    x=(x-6.)*std
    return
end

real(kind=8) function tesub7(i)
    integer :: i
    real(kind=8) :: g

    common /randsd/ g
    g=mod(g*9228907.,4294967296.)
    if(i >=0)tesub7=g/4294967296.
    if(i < 0)tesub7=2.*g/4294967296.-1.
    return
end

real(kind=8) function tesub8(i, t) !returns a double, as it should
!   common block
    integer idvwlk
    real(kind=8) &
    adist, bdist, cdist, ddist, &
    tlast, tnext, &
    hspan, hzero, sspan, szero, spspan
    common /wlk/ &
    adist(12), bdist(12), cdist(12), ddist(12), &
    tlast(12), tnext(12), &
    hspan(12), hzero(12), sspan(12), szero(12), &
    spspan(12), idvwlk(12)
!   common block

    integer :: i
    real(kind=8) :: h, t
    h = t - tlast(i)
    tesub8 = adist(i) + h*(bdist(i) + h*(cdist(i) + h*ddist(i)))
    return
end
