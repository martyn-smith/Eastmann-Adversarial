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
include "tecontrol.f95"
include "teout.f95"
include "tewalk.f95"
include "teprob.f95"

!  layout of state vector:

!  |1 --- 3||4 --- 8||  9 ||10 - 12||13 - 17|| 18 ||19 - 26|| 27 ||28 - 35|| 36 |,
!  | R.ucv || R.ucl ||R.et|| S.ucv || S.ucl ||S.et|| C.ucl ||C.et|| V.ucv ||V.et|,

!  | 37|| 38||   39-50   |,
!  |twr||tws|| vcv/vpos? |,

! module filtering
!     type filter
!         real(kind=8) :: alpha(22), fxmeas(22), taufil(22)
!     end type filter
!     contains
!         subroutine filterinit
!             taufil = 5.0
!         end subroutine filterinit

! end module filtering

program temain
!   TODO: fix delta_t, xmeas(42), general output format, move in argparse
    implicit none
!   measurement and valve common block
    real(kind=8) :: xmeas, xmv, fxmeas, taufil, alpha
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22)

!   disturbance vector common block
    integer :: idv
    common /dvec/ idv(24)

!   controller common block
    real(kind=8) :: setpt, gain, taui, errold
    common /ctrl/ setpt(20), gain(20), taui(20), errold(20)

!   local variables
    logical :: realtime = .false., verbose = .false.
    integer :: i, npts 
    real(kind=8) :: time, delta_t, state(50), derivative(50)
    character(len=20) :: flag

!   display help, if necessary
    do i = 1, command_argument_count()
        call get_command_argument(i, flag)
        if (flag == "-h" .or. flag == "--help") call helptext
        if (flag == "-r") realtime = .true.
        if (flag == "-v") then 
            call outputinit
            verbose = .true.
        end if
        if (any([flag == "--xmeas", flag == "--xmv", flag == "-a", flag == "--aggression", &
                 flag == "--mode"])) exit
    end do

!   integrator step size:  1 second converted to hours (time-base of simulation)
    delta_t = 1. / 3600.0

!   set the number of points to simulate
    npts = 48*3600

    call filterinit(delta_t)
    call teinit(state, size(state), derivative, time)
    call filter_xmeas(time)
    call contrlinit

!   simulation loop
    do i = 1, npts
        if (flag == "--xmeas") call perturb_xmeas(time)
        if (flag == "--mode") call perturb_mode(time)
        call contrl(delta_t)
        if (flag == "--xmv") call perturb_xmv(time)
        if (mod(i, 3600) == 0 .and. (flag == "-a" .or. flag == "--aggression")) call set_idvs()
        call tefunc(state, size(state), derivative, time)
        if (verbose) call output(time)
        if (realtime) then 
            call sleep(1)
            print *, xmeas(8), xmeas(9), xmeas(7), xmeas(12), xmeas(11), xmeas(13)
        end if
        call intgtr(state, size(state), derivative, time, delta_t)
        call filter_xmeas(time)
    end do
end program temain

!===============================================================================
subroutine helptext
    print *, "usage: te [OPTIONS]", char(10), &
        "Options:", char(10), &
        "  -h, --help                  Display this message",  char(10), &
        "  -a AGGR, --aggression AGGR  Run with aggression parameter AGGR (default 0.01)",  char(10), &
        "  -p [FNAME]                  Run with disturbances predetermined by FNAME",  char(10), &
        "  -v                          outputs to file (slow)", char(10), &
        "  -r                          Run realtime and print state to STDOUT", char(10), &
        "  --xmeas [X] [OPTIONS]       Run with XMEAS set to [options], or ",  char(10), &
        "  --xmv [X] [OPTIONS]         Run with XMV set to [options]", char(10), &
        "  --mode [X] [OPTIONS]        Run with setpoints set to [options]", char(10), &
        "  xmeas/xmv options:", char(10), &
        "    MAX, MIN                  Maximum or minimum values respectively", char(10), &
        "    STICK                     Sticks at random point in time", char(10), &
        "    SINE [PERIOD, AMP]        Sinusoidal deviation", char(10), &
        "    SQUARE [PERIOD, AMP]      Square wave deviation", &
        "  mode options:", char(10), &
        "    SET [VALUE]               New set value", char(10), &
        "    SINE, ", char(10), &
        "    SQUARE [PERIOD, AMP]      As with xmeas/xmv" 
    call exit(0)
end subroutine helptext

subroutine filterinit(delta_t)
!   specify some parameters for the relay identification plus some
!   parameters for the filter
!   references: i)seborg ii) astrom, automatica, 1984, 20, 645-651
!   taufil  = filter constant for xmeas(i)
!   initialise filter times (seconds)

    real(kind=8) :: alpha, fxmeas, taufil
    common /filter/ alpha(22), fxmeas(22), taufil(22)
    real(kind=8), intent(in) :: delta_t
    integer :: i

    taufil = 5.0
    do i=1,22
         alpha(i) = delta_t*3600./taufil(i)
    end do
end subroutine

subroutine filter_xmeas(time)
!   filter measurements
!   alpha = filter constant, see e.g. seeborg pp. 539-540 for more details.  Currently set to ~0.2)
!   alpha = 1,   no filtering
!   alpha = 0,   complete filtering (measurement is ignored)
    integer :: k
    real(kind=8) :: alpha, fxmeas, taufil, time
    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22)

    if (time /= 0) then
        do k=1,22
            fxmeas(k) = alpha(k)*xmeas(k)+(1-alpha(k))*fxmeas(k)
        end do 
    else
        do k=1,22
            fxmeas(k) = xmeas(k)
        end do 
    end if
end subroutine filter_xmeas