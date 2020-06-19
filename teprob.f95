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
!  Model of the TE (Tennessee Eastmann) challenge reactor.
!
!  The Plant takes four inputs, A, C, D, E, and produces two outputs,
!  G and H.
!
!  Primary reactions:
!
!  A(g) + C(g) + D(g) -> G(l)
!  A(g) + D(g) + E(g) -> H(l)
!
!  Byproduct reactions:
!
!  A(g) + E(g) -> F(l)
!  3D(g) -> 2F(l)
!
!  All are exothermic, reversible, and first-order
!  (rates follow an Arrhenius relation.)
!
!  Product flow is: 
!  a,d,e ->                                  c ->
!     reactor -> condensor -> separator -> stripper -> product
!           <- compressor <- purge <-
!           <---------------------------------

! High-level overview of run():

! 1 ) set IDVs
! 2 ) set wlk values
! 3 ) set Reactor ES, stream[3] properties (the latter by calling sub8)
! 4 ) load some state into Vessels
! 5 ) calculate XL from UCL
! 6 ) sub2?
! 7 ) sub4? 
! 8 ) set VL
! 9 ) calculate Pressure of A,B,C
! 10) calculate Pressure of D,E,F,G,H
! 11) calculate XVs from PPs
! 12) calculate whatever RRs are 
! 13) ditto delta_xr, XMWs
! 14) Set temps
! 15) set_heat on streams()
! 16) calculate flows
! 17) calculate flms
! 18) calculate flow mass fracs
! 19) calculate flow concs
! 20) temp and heat conservation
! 21) underflow
! 22) XMEAS update
! 23) errors (based on GROUND TRUTH - change to XMEAS)
! 24) Separator Energy Balance?
! 25) VCVs, whatever they are 
! 26) update derivatives

! ############################################################################### 
!
! Four core loops:

!     Reactor.T <- Coolant flow
!     Reactor.level <- E feed flow
!     Separator.level <- Condensor Coolant flow
!     Stripper.level <- Stripper liquid efflux valve
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
!    extras
!
!    xmeas(42)   g/h ratio
!    xmeas(43)   TODO: cost
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

module constants
    real, parameter :: r_gas = 8.314472
    real, parameter :: r_gas_kcal = 1.987e-3
    real, parameter :: rg = 998.9 ! ?. I have no idea.
    real, parameter :: avp(8) = [0.0, 0.0, 0.0, 15.92, 16.35, 16.35, 16.43, 17.21]
    real, parameter :: bvp(8) = [0.0, 0.0, 0.0, -1444.0, -2114.0, -2114.0, -2748.0, -3318.0]
    real, parameter :: cvp(8) = [0.0, 0.0, 0.0, 259.0, 265.5, 265.5, 232.9, 249.6]
    real, parameter :: ad(8) = [1.0, 1.0, 1.0, 23.3, 33.9, 32.8, 49.9, 50.5]
    real, parameter :: bd(8) = [0.0, 0.0, 0.0, -0.0700, -0.0957, -0.0995, -0.0191, -0.0541]
    real, parameter :: cd(8) = [0.0, 0.0, 0.0, -0.0002, -0.000152, -0.000233, -0.000425, -0.000150]
    real, parameter :: ah(8) = [1.0e-6, 1.0e-6, 1.0e-6, 0.96e-6, 0.573e-6, 0.652e-6, 0.515e-6, 0.471e-6]
    real, parameter :: bh(8) = [0., 0., 0., 8.70e-9, 2.41e-9, 2.18e-9, 5.65e-10, 8.70e-10]
    real, parameter :: ch(8) = [0., 0., 0., 4.81e-11, 1.82e-11, 1.94e-11, 3.82e-12, 2.62e-12]
    real, parameter :: av(8) = [1.0e-6, 1.0e-6, 1.0e-6, 86.7e-6, 160.0e-6, 160.0e-6, 225.0e-6, 209.0e-6]
    real, parameter :: ag(8) = [3.411e-6, 0.3799e-6, 0.2491e-6, 0.3567e-6, 0.3463e-6, 0.393e-6, 0.17e-6, 0.150e-6]
    real, parameter :: bg(8) = [7.18e-10, 1.08e-9, 1.36e-11, 8.51e-10, 8.96e-10, 1.02e-9, 0., 0.]
    real, parameter :: cg(8) = [6.0e-13, -3.98e-13, -3.93e-14, -3.12e-13, -3.27e-13,-3.12e-13, 0., 0.]
    real, parameter :: xmw(8) = [2.0, 25.4, 28.0, 32.0, 46.0, 48.0, 62.0, 76.0]
    real, parameter :: htr(2) = [0.06899381054, 0.05] !in calmol-1 !
end module constants

module types
    use iso_c_binding
    type Vessel
        sequence
        real(kind=8) :: utl, utv, &    ! total molar amounts in liquid and gas phase
                        et, es, &      ! total and liquid phase heat?
                        tc, tk, &      ! temps (C and K)
                        density, &          ! ??
                        vt, vl, vv, &  ! total, liquid and vapour volumes
                        pt, &          ! total pressure
                        qu, hw ! ?? ?? ?? ??
        real(kind=8), dimension(8) :: ucl, ucv, & ! molar components amounts in liquid and gas phase
                                      xl, xv, &      ! concentrations in liquid and gas phase
                                      pp             ! partial pressure
        !contains
        !   procedure, pass :: set_temperature
    end type Vessel
    type Stream
        sequence
        real(kind=8) :: ftm, fcm(8), x(8), xmws, h, T
    end type Stream
    contains
        real(kind=8) function calc_vessel_heat(this, ity) result(H)
!           was tesub1. Split out for when it's used for vessels.
            use constants
            integer, intent(in) :: ity
            integer :: i
            real(kind=8) :: hi
            type(vessel) :: this

            H = 0.
            if(ity == 0) then ! R, S, C
                do i=1,8 !label 100
                    hi = 1.8 * this%Tc * (ah(i) + bh(i)*this%Tc/2. + ch(i)*this%Tc**2/3.)
                    H = H + this%xl(i)*xmw(i)*hi !xl
                end do
            else if (ity==2) then ! V only
                do i=1,8 !label 200
                    hi = 1.8 * this%Tc * (ag(i) + bg(i)*this%Tc/2. + cg(i)*this%Tc**2/3.) + av(i)
                    H = H + this%xv(i)*xmw(i)*hi !xv
                end do
                H = H - 3.57696e-6 * (this%tc+273.15)
            end if
            return
        end function calc_vessel_heat

        real(kind=8) function new_calc_delta_h(this, ity) result(dh)
!           was tesub3
            use constants
            integer, intent(in) :: ity
            integer :: i
            real(kind=8) :: dhi
            type(vessel), intent(in) :: this

            dh = 0.
            if(ity == 0) then
                do i=1,8 !label 100...again
                    dhi = 1.8 * (ah(i) + bh(i)*this%Tc + ch(i)*this%tc**2)
                    dh = dh + this%xl(i)*xmw(i)*dhi
                end do
            else
                do i=1,8
                    dhi = 1.8 * (ag(i) + bg(i)*this%tc + cg(i)*this%tc**2)
                    dh = dh + this%xv(i)*xmw(i)*dhi
                end do
                dh = dh - 3.57696e-6
            end if
            return
        end function new_calc_delta_h

        subroutine set_temperature(this, ity)
!           replaces tesub2
!           for R, S, C, called with xl, tc, es, 0
            logical :: converged
            integer, intent(in) :: ity
            integer :: i
            real(kind=8) :: dh, dT, err, h_test, T_in
            type(vessel) :: this

            T_in = this%tc
            converged = .FALSE.
            do i=1,100 !label 250
                h_test = calc_vessel_heat(this, ity)
                err = h_test - this%es
                dh = new_calc_delta_h(this, ity) 
                dT = -err/dh
                this%tc = this%tc + dT !main mutator of T
                if(abs(dT) < 1.e-12) then
                    converged = .TRUE.
                    exit
                end if
            end do
!           this appears to be correct..? i.e. T is reset if error never converges.
            if (.not. converged) this%tc = T_in 
            this%tk = this%tc + 273.15
            return
        end subroutine set_temperature

        subroutine set_pressures(this)
            use constants
            integer :: i
            type(vessel) :: this

            do i=1,3 !label 1110 - for R and S only
                this%pp(i) = this%ucv(i) * rg * this%tk / this%vv
            end do
            do i=4,8 !label 1120 - for R and S only
                this%pp(i) = this%xl(i) * exp(avp(i) + bvp(i)/(this%tc + cvp(i))) !Antoine eq.
            end do
            this%pt = sum(this%pp)
            do i=1,8 !label 1130
                this%xv(i) = this%pp(i) / this%pt
            end do
            this%utv = this%pt * this%vv/rg/this%tk
            do i=4,8 !label 1140
                this%ucv(i) = this%utv * this%xv(i)
            end do
        end subroutine set_pressures

        subroutine set_density(this) 
!           was tesub4
            use constants
            integer :: i
            real(kind=8) :: v
            type(vessel), intent(inout) :: this

            v=0.
            do i=1,8
                v = v + this%xl(i) * xmw(i) / (ad(i) + (bd(i)+cd(i) * this%tc) * this%tc)
            end do
            this%density = 1.0/v
            return
        end subroutine set_density

        real(kind=8) function metric_gauge(Pa) result(Pg)
!           converts mmHga to kPag
            real(kind=8), intent(in) :: Pa

            Pg = (Pa-760.0)/760.0*101.325
            return
        end function metric_gauge
end module types

subroutine teinit(state, nn, derivative, time)
!   initialization
!
!   inputs:
!      nn = number of differential equations
!
!   mutates:
!     time = current time(hrs)
!     state = current state values
!     derivative = current derivative values
    use types
    use constants

    integer :: ivst
    real(kind=8) :: &
    twr, tws, fwr, fws, &
    delta_xr, reaction_rate, reaction_heat, &
    vcv, vrng, vtau, &
    sfr, &
    cpflmx, cpprmx, cpdh, tcwr, tcws, &
    agsp, xdel, xns, t_gas, t_prod, vst
!   Possible bug in f2py - does not respect bang comments
!   Reactor properties (24)
!   Seperator properties (20)
!   Stripper properties? (10)
!   Condensor properties? (9)
    type(vessel) :: r, s, c, v
    type(stream) :: sm
    common /teproc/ &
    r, s, c, v, &
    twr, tws, fwr, fws, &
    delta_xr(8), reaction_rate(4), reaction_heat, &
!   XMV
    vcv(12), vrng(12), vtau(12), &
!   stream and component properties 
    sm(13), &
    sfr(8), &
    cpflmx, cpprmx, cpdh, tcwr, tcws, &
    agsp, xdel(41), xns(41), &
    t_gas, t_prod, vst(12), ivst(12)

    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)

    integer :: idv
    common /dvec/ idv(24)

    integer :: idvwlk
    real(kind=8) :: &
    adist, bdist, cdist, ddist, tlast, tnext, &
    hspan, hzero, sspan, szero, spspan
    common /wlk/ &
    adist(12), bdist(12), cdist(12), ddist(12), tlast(12), tnext(12), &
    hspan(12), hzero(12), sspan(12), szero(12), spspan(12), idvwlk(12)
!   common block

    integer, intent(in) :: nn
    integer :: i
    real :: delta_t
    real(kind=8), intent(out) :: state(nn), derivative(nn), time

!   fortran to python headers
!   f2py intent(in) nn, time, state, derivative
!   f2py intent(out) state, derivative

!   common /teproc/ assignments. TODO: not all are present, investigate.
    R%hw=7060.
    S%hw=11138.

    R%vt=1300.0
    S%vt=3500.0
    C%vt=156.5
    V%vt=5000.0

!   vrng(5,6,12) not assigned. VCV assigned later from state.
    vrng(1)=400.00
    vrng(2)=400.00
    vrng(3)=100.00
    vrng(4)=1500.00
    vrng(7)=1500.00
    vrng(8)=1000.00
    vrng(9)=0.03
    vrng(10)=1000.
    vrng(11)=1200.0
    vtau = [8., 8., 6., 9., &
            7., 5., 5., 5., &
            120., 5., 5., 5.]
    do i=1,size(vtau) !label 300
        vtau(i)=vtau(i)/3600.
    end do

    !was xst(:,1) etc
    sm(1)%x = [0., 0.0001, 0., 0.9999, 0., 0., 0., 0.]
    sm(2)%x = [0., 0., 0., 0., 0.9999, 0.0001, 0., 0.]
    sm(3)%x = [0.9999, 0.0001, 0., 0., 0., 0., 0., 0.]
    sm(4)%x = [0.4850, 0.0050, 0.5100, 0., 0., 0., 0., 0.]
    sm(1:4)%T = 45. !why just 1:4?
    sfr = [0.995, 0.991, 0.99, 0.916, 0.936, 0.938, 0.058, 0.0301]

    cpflmx=280275.
    cpprmx=1.3

    xns = [0.0012, 18.000, 22.000, 0.0500, 0.2000, &
           0.2100, 0.3000, 0.5000, 0.0100, 0.0017, &
           0.0100, 1.0000, 0.3000, 0.1250, 1.0000, &
           0.3000, 0.1150, 0.0100, 1.1500, 0.2000, &
           0.0100, 0.0100, 0.250, 0.100, 0.250, &
           0.100, 0.250, 0.025, 0.250, 0.100, &
           0.250, 0.100, 0.250, 0.025, 0.050, &
           0.050, 0.010, 0.010, 0.010, 0.500, &
           0.500]
!   array assignment!
    vst = 2.
    ivst = 0

!   state init to happen before /pv/ and /teproc/ vcv

!   layout of state vector:

!  |1 --- 3||4 --- 8||  9 ||10 - 12||13 - 17|| 18 ||19 - 26|| 27 ||28 - 35|| 36 |,
!  | R.ucv || R.ucl ||R.et|| S.ucv || S.ucl ||S.et|| C.ucl ||C.et|| V.ucv ||V.et|,

!  | 37|| 38||   39-50   |,
!  |twr||tws|| vcv/vpos? |,

    state = [10.40491389,  4.363996017,    7.570059737, .4230042431,   24.15513437, &
             2.942597645,  154.3770655,    159.186596,  2.808522723,   63.75581199, &
             26.74026066,  46.38532432,   .2464521543,  15.20484404,   1.852266172, & 
             52.44639459,  41.20394008,   .569931776,   .4306056376,  .0079906200783, &
             .9056036089,  .016054258216, .7509759687,  .088582855955, 48.27726193, &
             39.38459028,  .3755297257,    107.7562698, 29.77250546,   88.32481135, &
             23.03929507,  62.85848794,    5.546318688, 11.92244772,   5.555448243, &
             .9218489762,  94.59927549,    77.29698353, 63.05263039,   53.97970677, &
             24.64355755,  61.30192144,    22.21,       40.06374673,   38.1003437, &
             46.53415582,  47.44573456,    41.10581288, 18.11349055,   50.]

!   integrator step size:  1 second converted to hours (time-base of simulation)
    delta_t = 1. / 3600.00001

!   common /pv/ init
    do i=1,size(xmv) !label 200
        xmv(i)=state(i+38)
        vcv(i)=xmv(i) !not a /pv/ !
    end do

!   common /idv/ init
    idv=0

!   common /wlk/ init 
    bdist=0.
    cdist=0.
    ddist=0.
    tlast=0.
    tnext=0.1
    hspan = [0.2, 0.7, 0.25, 0.7, 0.15, 0.15, &
             1.0, 1.0, 0.4, 1.5, 2.0, 1.5]
    hzero = [0.5, 1.0, 0.5, 1.0, 0.25, 0.25, &
             2.0, 2.0, 0.5, 2.0, 3.0, 2.0]
    sspan = [0.03, 0.003, 10.0, 10.0, 10.0, 10.0, &
             0.25, 0.25, 0.25, 0.0, 0.0, 0.0]
    szero = [0.485, 0.005, 45.0, 45.0, 35.0, 40.0, &
             1.0, 1.0, 0.0, 0.0, 0.0, 0.0]
    spspan = 0.
    do i=1,12 !label 550
        adist(i)=szero(i)
    end do
    !idvwlk?

    time = 0.
    call tefunc(state, nn, derivative, time)
    return
end subroutine teinit

!==============================================================================
subroutine tefunc(state, nn, derivative, time)
!   function evaluator
!
!   inputs:
!     nn = number of differential equations
!     time = current time(hrs)
!     state = current state values
!
!   mutates:
!     derivative = current derivative values
    use constants
    use types

!   common block
    integer :: ivst
    real(kind=8) :: &
    twr, tws, fwr, fws, &
    delta_xr, reaction_rate, reaction_heat,  &
    vcv, vrng, vtau, &
    sfr, &
    cpflmx, cpprmx, cpdh, tcwr, tcws, &
    agsp, xdel, xns, &
    t_gas, t_prod, vst
    type(vessel) :: r, s, c, v
    type(stream) :: sm
    common /teproc/ &
    r, s, c, v, &
    twr, tws, fwr, fws, &
    delta_xr(8), reaction_rate(4), reaction_heat, &
    vcv(12), vrng(12), vtau(12), &
    sm(13), &
    sfr(8), &
    cpflmx, cpprmx, cpdh, tcwr, tcws, &
    agsp, xdel(41), xns(41), &
    t_gas, t_prod, vst(12), ivst(12)

    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)

    integer :: idv
    common /dvec/ idv(24)

    integer :: idvwlk
    real(kind=8) :: &
    adist, bdist, cdist, ddist, tlast, tnext, &
    hspan, hzero, sspan, szero, spspan
    common /wlk/ &
    adist(12), bdist(12), cdist(12), ddist(12), tlast(12), tnext(12), &
    hspan(12), hzero(12), sspan(12), szero(12), spspan(12), idvwlk(12)
!   common block

    integer, intent(in) :: nn
    integer :: i, isd
    real(kind=8) :: &
    delta_p, flcoef, flms, pr, &
    r1f, r2f, &
    tmpfac, &
    uar, uas, uac, uarlev, &
    vovrl, &
    hwlk, swlk, spwlk, &
    fin(8), &
    vpos(12), &
    xcmp(41), &
    random_dist, rand, random_xmeas_noise
    character(len=25) :: err_msg
    real(kind=8), intent(inout) :: state(nn), derivative(nn), time

!   label 500 abstracted, idv is a logical now.
!   futzing with wlk block
    call set_idvwlk()
    do i=1,9 !label 900
        if(time >= tnext(i)) then
!   TODO: this part frequently denormalises. Really, is it necessary?
            hwlk = tnext(i)-tlast(i)
            swlk = adist(i) + hwlk &
                               * (bdist(i) + hwlk &
                                              * (cdist(i) + hwlk * ddist(i)))
            spwlk = bdist(i) + hwlk &
                                * (2. * cdist(i) + 3. * hwlk * ddist(i))
            tlast(i)=tnext(i)
            !set_dists calls random(), mutating cdist, ddist, tnext
            call set_dists(swlk, spwlk, i)
        end if
    end do
    do i=10,12 !label 910
        if(time >= tnext(i)) then
            hwlk = tnext(i)-tlast(i)
            swlk = adist(i)+hwlk &
                             * (bdist(i) + hwlk &
                                            * (cdist(i) + hwlk * ddist(i)))
            spwlk = bdist(i) + hwlk &
                                * (2. * cdist(i) + 3. * hwlk * ddist(i))
            tlast(i)=tnext(i)
            if(swlk > 0.1) then
                adist(i)=swlk
                bdist(i)=spwlk
                cdist(i)=-(3.*swlk+0.2*spwlk)/0.01
                ddist(i)=(2.*swlk+0.1*spwlk)/0.001
                tnext(i)=tlast(i)+0.1
            else
                !isd=-1
                !calls random(-1,1) on hwlk (used to be tesub7(isd))
                call random_number(rand)
                hwlk=hspan(i)*(2*rand-1)+hzero(i)
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
        end do
        bdist=0.
        cdist=0.
        ddist=0.
        tlast=0.
        tnext=.1
    end if
!   stream and idvs
    sm(4)%x(1) = random_dist(1, time)-idv(1)*0.03 -idv(2)*2.43719e-3
    sm(4)%x(2) = random_dist(2, time)+idv(2)*0.005
    sm(4)%x(3) = 1. - sm(4)%x(1) - sm(4)%x(2)
    sm(1)%T = random_dist(3, time) + idv(3)*5.
    sm(4)%T =random_dist(4, time)
    tcwr=random_dist(5, time)+idv(4)*5.
    tcws=random_dist(6, time)+idv(5)*5.

!   download new molar amounts from state vector.
    do i=1,3 !label 1010
        R%ucv(i)=state(i)
        S%ucv(i)=state(i+9)
        R%ucl(i)=0.  
        S%ucl(i)=0.
    end do
    do i=4,8 !label 1020
        R%ucl(i)=state(i)
        S%ucl(i)=state(i+9)
    end do
    do  i=1,8 !label 1030
        C%ucl(i)=state(i+18)
        V%ucv(i)=state(i+27)
    end do
    R%et=state(9)
    S%et=state(18)
    C%et=state(27)
    V%et=state(36)
    twr=state(37)
    tws=state(38)
    do i=1,size(vpos) !label 1035
        vpos(i)=state(i+38)
    end do
!   label 1040 abstracted into sum()
    R%utl = sum(R%ucl) 
    S%utl = sum(S%ucl)
    C%utl = sum(C%ucl)
    V%utv = sum(V%ucv)
    do i=1,8 !label 1050
        R%xl(i) = R%ucl(i)/R%utl
        S%xl(i) = S%ucl(i)/S%utl
        C%xl(i) = C%ucl(i)/C%utl
        V%xv(i) = V%ucv(i)/V%utv
    end do

    R%es = R%et / R%utl
    S%es = S%et / S%utl
    C%es = C%et / C%utl
    V%es = V%et / V%utv

    call set_temperature(R, 0)
    call set_temperature(S, 0)
    call set_temperature(C, 0) !ecce
    call set_temperature(V, 2)

    call set_density(R)
    call set_density(S)
    call set_density(C)

    R%vl = R%utl / R%density
    S%vl = S%utl / S%density
    C%vl = C%utl / C%density
    R%vv = R%vt-R%vl
    S%vv = S%vt-S%vl

!   setting pressures
    call set_pressures(R)
    call set_pressures(S)
    V%pt = V%utv * rg * V%tk / V%vt ! P = n R T / V
!   setting reactions
!   rr's 1, 2, 3 consume A, 4 does not
!   R is in cal.K-1mol-1! first Ea works out to about 167 kJmol-1
    r1f=random_dist(7, time)
    r2f=random_dist(8, time)
    reaction_rate(1) = r1f * 5.219217002265e+13 * exp(-40./(1.987e-3 * R%tk))
    reaction_rate(2) = r2f * 20.27525952163 * exp(-20./(1.987e-3 * R%tk))
    reaction_rate(3) = 1.5629689117665e+23 * exp(-60./(1.987e-3 * R%tk))
    reaction_rate(4) = reaction_rate(3)*0.767488334
    if(R%pp(1) > 0.0 .and. R%pp(3) > 0.0) then
        r1f=R%pp(1)**1.1544
        r2f=R%pp(3)**0.3735
        reaction_rate(1)=reaction_rate(1)*r1f*r2f*R%pp(4)
        reaction_rate(2)=reaction_rate(2)*r1f*r2f*R%pp(5)
    else
        reaction_rate(1)=0.0
        reaction_rate(2)=0.0
    end if
    reaction_rate(3)=reaction_rate(3)*R%pp(1)*R%pp(5)
    reaction_rate(4)=reaction_rate(4)*R%pp(1)*R%pp(4)
    do i=1,4 !label 1200
        reaction_rate(i)=reaction_rate(i)*R%vv
    end do
!   consumption / generation
    delta_xr(1)=-reaction_rate(1)-reaction_rate(2)-reaction_rate(3) ! A consumption
    delta_xr(3)=-reaction_rate(1)-reaction_rate(2) ! C consumption.  Should be by rr(1) only!?
    delta_xr(4)=-reaction_rate(1)-1.5*reaction_rate(4) ! D consumed by rr(1), rr(2), rr(4)
    delta_xr(5)=-reaction_rate(2)-reaction_rate(3) ! E consumed by rr(2), rr(3)
    delta_xr(6)=reaction_rate(3)+reaction_rate(4) ! F created by rr(3), rr(4)
    delta_xr(7)=reaction_rate(1) ! A + C + D -> G
    delta_xr(8)=reaction_rate(2) ! A + D + E -> H
    reaction_heat=reaction_rate(1)*htr(1)+reaction_rate(2)*htr(2)

!   label 2010 vectorised
    sm(6)%x = V%xv
    sm(8)%x = R%xv
    sm(9)%x = S%xv
    sm(10)%x = S%xv
    sm(11)%x = S%xl
    sm(13)%x = C%xl

    sm(1)%xmws = sum(sm(1)%x * xmw)
    sm(2)%xmws = sum(sm(2)%x * xmw)
    sm(6)%xmws = sum(sm(6)%x * xmw)
    sm(8)%xmws = sum(sm(8)%x * xmw)
    sm(9)%xmws = sum(sm(9)%x * xmw)
    sm(10)%xmws = sum(sm(10)%x * xmw)

!   setting stream temps
    sm(6)%T = V%tc
    sm(8)%T = R%tc
    sm(9)%T = S%tc !
    sm(10)%T = S%tc
    sm(11)%T = S%tc
    sm(13)%T = C%tc
!   setting stream heats
    call set_stream_heat(sm(1),1) 
    call set_stream_heat(sm(2),1)
    call set_stream_heat(sm(3),1)
    call set_stream_heat(sm(4),1)
    call set_stream_heat(sm(6),1)
    call set_stream_heat(sm(8),1)
    call set_stream_heat(sm(9),1)
    sm(1)%H = sm(9)%H ! ???
    call set_stream_heat(sm(11),0)
    call set_stream_heat(sm(13),0)

!   setting stream mass flows
    sm(3)%ftm = vpos(3)*(1.-idv(6))*vrng(3)/100.0 ! A feed
    sm(1)%ftm = vpos(1)*vrng(1)/100.0 ! D feed
    sm(2)%ftm = vpos(2)*vrng(2)/100.0 ! E feed
    sm(4)%ftm = vpos(4)*(1.-idv(7)*0.2)*vrng(4)/100.0+1.e-10 ! A and C feed
    sm(11)%ftm = vpos(7)*vrng(7)/100.0 ! separator underflow
    sm(13)%ftm = vpos(8)*vrng(8)/100.0 ! stripper underflow
    uac = vpos(9)*vrng(9)*(1. + random_dist(9,time))/100.0
    fwr = vpos(10)*vrng(10)/100.0
    fws = vpos(11)*vrng(11)/100.0 !fws, interesting
    agsp = (vpos(12)+150.0)/100.0
    delta_p=max(V%pt-R%pt, 0.) 
    flms=1937.6*sqrt(delta_p)
    sm(6)%ftm = flms / sm(6)%xmws
    delta_p=max(R%pt-S%pt, 0.) ! reactor - condensor?
    flms=4574.21*sqrt(delta_p)*(1.-0.25*random_dist(12,time))
    sm(8)%ftm = flms / sm(8)%xmws ! mass flow = volume / mean mass?
    delta_p=max(S%pt-760.0, 0.) ! sep? - atmosphere
    flms=vpos(6)*0.151169*sqrt(delta_p)
    sm(10)%ftm = flms / sm(10)%xmws
    pr=min(max(V%pt/S%pt, 1.), cpprmx)
    flcoef=cpflmx/1.197
    flms=cpflmx+flcoef*(1.0-pr**3)
    cpdh=flms*(S%tk)*1.8e-6*1.9872 *(V%pt-S%pt)/(sm(9)%xmws*S%pt)
    delta_p=max(V%pt - S%pt, 0.)
    flms=max(flms-vpos(5)*53.349*sqrt(delta_p), 1.e-3)
    sm(9)%ftm = flms/sm(9)%xmws
    sm(9)%h = sm(9)%h + cpdh/sm(9)%ftm

    ! label 5020 vectorised (yes, this also works)
    sm(1)%fcm = sm(1)%x * sm(1)%ftm
    sm(2)%fcm = sm(2)%x * sm(2)%ftm
    sm(3)%fcm = sm(3)%x * sm(3)%ftm
    sm(4)%fcm = sm(4)%x * sm(4)%ftm
    sm(6)%fcm = sm(6)%x * sm(6)%ftm
    sm(8)%fcm = sm(8)%x * sm(8)%ftm
    sm(9)%fcm = sm(9)%x * sm(9)%ftm
    sm(10)%fcm = sm(10)%x * sm(10)%ftm
    sm(11)%fcm = sm(11)%x * sm(11)%ftm
    sm(13)%fcm = sm(13)%x * sm(13)%ftm

    if(sm(11)%ftm > 0.1) then
        if(C%tc > 170.) then
            tmpfac = C%tc-120.262
        elseif(C%tc < 5.292) then
            tmpfac = 0.1
        else
            tmpfac = 363.744/(177. - C%tc) - 2.22579488
        end if
        vovrl = sm(4)%ftm / sm(11)%ftm*tmpfac
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
!   label 6010
    fin = sm(4)%fcm + sm(11)%fcm

!   setting streams 5 and 12 properties
!   label 6020 and 6030
    sm(5)%fcm = sfr * fin ! only place that consumes sfr
    sm(12)%fcm = fin - sm(5)%fcm
    sm(5)%ftm = sum(sm(5)%fcm)
    sm(12)%ftm = sum(sm(12)%fcm)
    sm(5)%x = sm(5)%fcm / sm(5)%ftm
    sm(12)%x = sm(12)%fcm / sm(12)%ftm

    sm(5)%T = C%tc  !C%tc
    sm(12)%T = C%tc !C%tc
    call set_stream_heat(sm(5), 1)
    call set_stream_heat(sm(12), 0)

    sm(7)%ftm = sm(6)%ftm
    sm(7)%H = sm(6)%H
    sm(7)%T = sm(6)%T
!   label 6130
    sm(7)%x = sm(6)%x
    sm(7)%fcm = sm(6)%fcm

!   calculate cooling from water feeds
!    call set_heat_transfer(R) , (S), (C)
    if(R%vl/7.8 > 50.0) then
        uarlev=1.0
    elseif(R%vl/7.8 < 10.0) then
        uarlev=0.0
    else
        uarlev=0.025*R%vl/7.8-0.25
    end if
    uar = uarlev * (-0.5*agsp**2 + 2.75*agsp-2.5) * 855490.e-6
    R%qu = uar * (twr-R%tc) * (1. - 0.35*random_dist(10,time))
    uas = 0.404655 * (1. - 1./(1. + (sm(8)%ftm/3528.73)**4)) ! condensor?
    S%qu = uas * (tws-sm(8)%T) *(1. - 0.25*random_dist(11,time))
    C%qu=0.
    if(C%tc < 100.) C%qu = uac*(100.0-C%tc)
!   sm(3) -> stream 1, sm(1) -> stream 2, sm(2) -> stream 3
!   note vessel "C", apparently part of the stripper, is never pressurised (the original code has no PTC slot)

!   print *, S%vl
    xmeas(1) = sm(3)%ftm*0.359/35.3145 ! A Feed  (stream 1)                    kscmh
    xmeas(2) = sm(1)%ftm*sm(1)%xmws*0.454 ! D Feed  (stream 2)                 kg/hr
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
    xmeas(20) = cpdh*0.0003927e6 ! compressor work                             kwh
    xmeas(20) = cpdh*0.29307e3 ! compressor work, again??                      kwh
    xmeas(21) = twr ! reactor cooling water outlet temp                        deg c
    xmeas(22) = tws ! separator cooling water outlet temp                      deg c

    isd=0
    if(xmeas(7) > 3000.0) then
     isd=1
     err_msg = "reactor pressure high"
    end if
    if (((R%pt-760.0)/760.0*101.325) > 12000.) then
        print *, "plant has exploded"
    end if
    if(R%vl/35.3145 > 24.0) then 
        isd=1
        print *, "Reactor level high"
    end if
    if(R%vl/35.3145 < 2.0) then
        isd=1
        err_msg = "Reactor level low"
    end if
    if(xmeas(9) > 175.0) then
        isd=1
        err_msg = "Reactor temp high"
    end if
    if(S%vl/35.3145 > 12.0) then 
        isd=1
        err_msg = "Sep level high"
    end if
    if(S%vl/35.3145 < 1.0) then
        isd=1
        err_msg = "Sep level low"
    end if
    if(C%vl/35.3145 > 8.0) then
        isd=1
        err_msg = "C level high"
    end if
    if(C%vl/35.3145 < 1.0) then
        isd=1
        err_msg = "C level low"
    end if
    if(isd == 1) then
        print *, err_msg, " at t = ", time
        stop
    end if    
    if(time > 0.0 .and. isd == 0) then
        do i=1,22 !label 6500
            !call tesub6(xns(i),xmns)
            xmeas(i)=xmeas(i)+random_xmeas_noise(xns(i))
        end do
    end if
    xcmp(23)=sm(7)%x(1)*100.0
    xcmp(24)=sm(7)%x(2)*100.0
    xcmp(25)=sm(7)%x(3)*100.0
    xcmp(26)=sm(7)%x(4)*100.0
    xcmp(27)=sm(7)%x(5)*100.0
    xcmp(28)=sm(7)%x(6)*100.0
    xcmp(29)=sm(10)%x(1)*100.0
    xcmp(30)=sm(10)%x(2)*100.0
    xcmp(31)=sm(10)%x(3)*100.0
    xcmp(32)=sm(10)%x(4)*100.0
    xcmp(33)=sm(10)%x(5)*100.0
    xcmp(34)=sm(10)%x(6)*100.0
    xcmp(35)=sm(10)%x(7)*100.0
    xcmp(36)=sm(10)%x(8)*100.0
    xcmp(37)=sm(13)%x(4)*100.0
    xcmp(38)=sm(13)%x(5)*100.0
    xcmp(39)=sm(13)%x(6)*100.0
    xcmp(40)=sm(13)%x(7)*100.0
    xcmp(41)=sm(13)%x(8)*100.0

!    print *, metric_gauge(R%pt), S%pt, C%pt, metric_gauge(V%pt)

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
            !call tesub6(xns(i),xmns)
            xmeas(i)=xmeas(i)+random_xmeas_noise(xns(i))
            xdel(i)=xcmp(i)
        end do
        t_gas=t_gas+0.1
    end if
    if(time >= t_prod) then
        do i=37,41 !label 7030
            xmeas(i)=xdel(i)
            !call tesub6(xns(i),xmns)
            xmeas(i)=xmeas(i)+random_xmeas_noise(xns(i))
            xdel(i)=xcmp(i)
        end do
        t_prod=t_prod+0.25
    end if
!   product cost?
    xmeas(42)=(62.0*xmeas(40))/(76*xmeas(41))

    do i=1,8 !label 9010
!       delta reactor UCV and UCL = flow in (7) - flow out (8) plus delta_xr
        derivative(i) = sm(7)%fcm(i) - sm(8)%fcm(i) + delta_xr(i)
!       delta separator UCV and UCL
!                         React out      recycle        purge           underflow
        derivative(i+9) = sm(8)%fcm(i) - sm(9)%fcm(i) - sm(10)%fcm(i) - sm(11)%fcm(i)
!       delta C UCV and UCL
        derivative(i+18) = sm(12)%fcm(i) - sm(13)%fcm(i)
!       delta V UCV only
        derivative(i+27) = sm(1)%fcm(i) + sm(2)%fcm(i) + sm(3)%fcm(i) &
                           + sm(5)%fcm(i) + sm(9)%fcm(i) - sm(6)%fcm(i)
    end do
!   delta R.et
    derivative(9) = sm(7)%H*sm(7)%ftm - sm(8)%H*sm(8)%ftm + reaction_heat + R%qu
!   delta S.et
    derivative(18) = sm(8)%h*sm(8)%ftm - (sm(9)%H*sm(9)%ftm - cpdh) - sm(10)%H*sm(10)%ftm &
                     - sm(11)%H*sm(11)%ftm + S%qu
!   delta C.et
    derivative(27) = sm(4)%H*sm(4)%ftm + sm(11)%H*sm(11)%ftm - sm(5)%H*sm(5)%ftm- sm(13)%H*sm(13)%ftm &
                     + C%qu
!   delta V.et
    derivative(36) = sm(1)%H*sm(1)%ftm + sm(2)%H*sm(2)%ftm + sm(3)%H*sm(3)%ftm  &
                     + sm(5)%H*sm(5)%ftm + sm(9)%H*sm(9)%ftm - sm(6)%H*sm(6)%ftm

    derivative(37)=(fwr*500.53* (tcwr-twr)-R%qu*1.e6/1.8)/R%hw !twr
    derivative(38)=(fws*500.53* (tcws-tws)-S%qu*1.e6/1.8)/S%hw !tws
    ivst(10)=idv(14)
    ivst(11)=idv(15)
    ivst(5)=idv(19)
    ivst(7)=idv(19)
    ivst(8)=idv(19)
    ivst(9)=idv(19)
    do i=1,12 !label 9020
        if(time == 0. .or. abs(vcv(i)-xmv(i)) > vst(i)*ivst(i)) then
            vcv(i)=xmv(i)
        end if 
        if(vcv(i) < 0.0) vcv(i)=0.0
        if(vcv(i) > 100.0) vcv(i)=100.0
        derivative(i+38)=(vcv(i)-vpos(i))/vtau(i)
    end do
    if(isd /= 0) then
        !label 9030
        derivative = 0.
    end if
    return
end subroutine tefunc   

subroutine intgtr(state, nn, derivative, time, delta_t)
    !   euler integration algorithm
    integer :: i
    integer, intent(in) :: nn
    real(kind=8), intent(in) :: delta_t, derivative(nn)
    real(kind=8), intent(out) :: time, state(nn) 

    time = time + delta_t
    do i = 1, nn
        state(i) = state(i) + derivative(i) * delta_t
    end do
    return
end subroutine intgtr
!
!===============================================================================
!

subroutine set_stream_heat(sm, ity) 
!   was tesub1
!   called as set_stream_heat(xst(:,12) -> z(8), tst(12)->T, hst(12)->H, 0)

    use constants
    use types
    integer, intent(in) :: ity
    integer :: i
    real(kind=8) :: hi
    type(stream), intent(inout) :: sm

    sm%H = 0.
    do i =1,8
        if (ity == 0) hi = 1.8 * sm%T * (ah(i) + bh(i)*sm%T/2. + ch(i)*sm%T**2/3.)
        if (ity == 1) hi = 1.8 * sm%T * (ag(i) + bg(i)*sm%T/2. + cg(i)*sm%T**2/3.) + av(i)
        sm%H = sm%H + sm%x(i)*xmw(i)*hi
    end do
    return
end subroutine set_stream_heat

subroutine set_dists(s, sp, i)
!   was tesub5
!   common block
    integer :: idvwlk
    real(kind=8) :: &
    adist, bdist, cdist, ddist, tlast, tnext, &
    hspan, hzero, sspan, szero, spspan
    common /wlk/ &
    adist(12), bdist(12), cdist(12), ddist(12), tlast(12), tnext(12), &
    hspan(12), hzero(12), sspan(12), szero(12), spspan(12), idvwlk(12)
!   common block

    integer, intent(in) :: i
    real(kind=8), intent(in) :: s, sp
    real(kind=8) ::  h, s1, s1p, rand(3)

    call random_number(rand)
    h = hspan(i)*(2*rand(1)-1)+hzero(i)
    s1 = sspan(i)*(2*rand(2)-1)*idvwlk(i)+szero(i)
    s1p = spspan(i)*(2*rand(3)-1)*idvwlk(i)
    adist(i) = s
    bdist(i) = sp
    cdist(i) = (3.*(s1-s) - h*(s1p + 2.*sp))/h**2
    ddist(i) = (2.*(s-s1) + h*(s1p + sp))/h**3
    tnext(i) = tlast(i)+h
    return
end subroutine set_dists

subroutine set_idvwlk()
!   common block
    integer :: idv
    common /dvec/ idv(24)

    integer :: idvwlk
    real(kind=8) :: &
    adist, bdist, cdist, ddist, tlast, tnext, &
    hspan, hzero, sspan, szero, spspan
    common /wlk/ &
    adist(12), bdist(12), cdist(12), ddist(12), tlast(12), tnext(12), &
    hspan(12), hzero(12), sspan(12), szero(12), spspan(12), idvwlk(12)
!   common block

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
!   was named tesub8
!   sets: sm(4)%x(1,2) (1,2), sm(1)%T (3), sm(4)%T (4), tcwr (5), tcws (6), r1f (7), r2f (8), 
!   uac (9), sm(8)%ftm (12), R%qu (10), S%qu (11)
!   common block
    integer :: idvwlk
    real(kind=8) :: &
    adist, bdist, cdist, ddist, tlast, tnext, &
    hspan, hzero, sspan, szero, spspan
    common /wlk/ &
    adist(12), bdist(12), cdist(12), ddist(12), tlast(12), tnext(12), &
    hspan(12), hzero(12), sspan(12), szero(12), spspan(12), &
    idvwlk(12)
!   common block

    integer :: i
    real(kind=8) :: time, h

    h = time - tlast(i)
    random_dist = adist(i) + h*(bdist(i) + h*(cdist(i) + h*ddist(i)))
!    print *, time, i, random_dist
    return
end function random_dist