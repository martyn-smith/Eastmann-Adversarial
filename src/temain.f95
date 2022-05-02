!==============================================================================
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

include "entities.f95"
include "tecontrol.f95"
include "teout.f95"
include "tewalk.f95"
include "teprob.f95"
!include "tesense.f95"

!  layout of state vector:
! 0                     time
! [1..3]                R.ucv
! [4..8]                R.ucl
! 9                     R.et
! [10..12]              S.ucv
! [13..17]              S.ucl
! 18                    S.et
! [19..26]              C.ucl
! 27                    C.et
! [28..35]              V.ucv
! 36                    V.et
! 37                    twr
! 38                    tws
! [39..50]              vpos
!

program temain
!   CONSIDER:
!   change delta_t to use one second by default (requires changing some hard-coded constants!)
    use tewalk
    implicit none

!   measurement and valve common block
    real(kind=8) :: xmeas, xmv, fxmeas, taufil, alpha
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22)

!   disturbance vector common block
    logical :: auto
    integer :: idv
    common /dvec/ idv(24), auto

!   controller common block
    real(kind=8) :: setpt, gain, taui, errold
    common /ctrl/ setpt(20), gain(20), taui(20), errold(20)

!   local variables
    logical :: aggression = .false., danger = .false., dvec = .false., fileout = .false., finalout = .false., &
               has_failed = .false., load = .false., mode = .false., open = .false., realtime = .false., &
               verbose = .false., veryverbose = .false.
    integer :: i, k
    integer :: npts = 48*3600
    real(kind=8) :: time, delta_t, state(50), derivative(50)
    character(len=20) :: flag
    character(len=25) :: err_msg

!   display help, if necessary
    do i = 1, command_argument_count()
        call get_command_argument(i, flag)
        if (flag == "-a" .or. flag == "--aggression") aggression = .true.
        if (flag == "-d") dvec = .true.
        if (flag == "--danger") danger = .true.
        if (flag == "-f") finalout = .true.
        if (flag == "-h" .or. flag == "--help") call helptext
        if (flag == "-l") load = .true.
        if (flag == "--long") npts = huge(npts)
        if (flag == "--mode") mode = .true.
        if (flag == "--open") open = .true.
        if (flag == "-o") fileout = .true.
        if (flag == "-r") realtime = .true.
        if (flag == "-t") call set_timer(npts, i)
        if (flag == "-v") verbose = .true.
        if (flag == "-vv") veryverbose = .true.
        if (any([flag == "--xmeas", flag == "--xmv", flag == "-a", flag == "--aggression", &
                 flag == "--mode"])) exit
    end do

!   integrator step size:  1 second converted to hours (time-base of simulation)
    delta_t = 1. / 3600.0

    call filterinit(delta_t)
    call teinit(state, size(state), derivative, time)
    call filter_xmeas(time)
    call contrlinit(open)
    if (fileout) call outputinit
    if (load) call teload(state, idv)
    if (mode) call set_mode

!   simulation loop
    do i = 1, npts

        if (flag == "--xmeas") call perturb_xmeas(time)
        if (.not. open) call contrl(delta_t)
        if (flag == "--xmv") call perturb_xmv(time)

        if (mod(i, 3600) == 0 .and. aggression) call set_idvs()
!       if (mod(i, 3600) == 0) print *, i

        if (dvec) call set_idv()

        call tefunc(state, size(state), derivative, time)

!        call measure()

        if (.not. danger) then
            call check_safety(has_failed, err_msg)
        end if
        call check_danger(has_failed, err_msg)

    !   check the true Reactor Pressure has not exceeded yield
        if(has_failed) then
            print *, err_msg, " at t = ", time, " hrs"
            stop
        end if

        if (fileout) call output(time, state)

        if (realtime) then
            call sleep(1)
        end if
        if (verbose) then
            print "(51e23.15,24i3)", time, (state(k), k=1,50), (idv(k), k=1,24)
        else if (veryverbose) then
            print "(55e23.15)", time, (xmeas(k), k=1,42), (xmv(k), k=1,12)
        end if

        call intgtr(state, size(state), derivative, time, delta_t)
        call filter_xmeas(time)
    end do
    if (finalout) then
        if (verbose) then
            print "(51e23.15,24i3)", time, (state(k), k=1,50), (idv(k), k=1,24)
        else
            print "(55e23.15)", time, (xmeas(k), k=1,42), (xmv(k), k=1,12)
        end if
    end if
end program temain

!===============================================================================
subroutine helptext
!   TODO: danger mode
!   TODO: better testing of these options,
!   TODO: argparse (debtable)
    print *, "usage: te [OPTIONS]", char(10), &
        "Options:", char(10), &
        "  -h, --help                  Display this message", char(10), &
        "  -a AGGR, --aggression AGGR  Run with aggression parameter AGGR (default 0.01)", char(10), &
        "  -d DIST                     Permanently set disturbance number DIST", char(10), &
        "  -l                          Load state from STDIN (!) before running", char(10), &
        "  --long                      Runs forever", char(10), &
        "  -o                          outputs to file (slow)", char(10), &
        "  -f                          print final measurement or state to STDOUT", char(10), &
        "  -r                          Run realtime and prints measurements to STDOUT", char(10), &
        "  -t HOURS                    Set timer (default 48 hr) in hours", char(10), &
        "     -s SECONDS               in seconds", char(10), &
        "     inf                      Runs forever", char(10), &
        "  -v                          Outputs more information", char(10), &
        "  --danger                    Danger mode: safety cutouts disabled", char(10), &
        "  --open                      Runs open loop", char(10), &
        "  --xmeas [X] [OPTIONS]       Run with XMEAS set to [options], or ",  char(10), &
        "  --xmv [X] [OPTIONS]         Run with XMV set to [options]", char(10), &
        "  --mode [X]                  Run with modes specified in Downs and Vogel", char(10), &
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

!==============================================================================

subroutine set_timer(npts, j)
    integer, intent(in) :: j
    integer :: ierr, n
    integer, intent(inout) :: npts
    character(3) :: tmp

    call get_command_argument(j+1, tmp)
    if (tmp == "inf") then
        npts = huge(npts)
    else if (tmp == "-s") then
        call get_command_argument(j+2, tmp)
        read(tmp, *, iostat=ierr) n
         if (ierr == 0) npts = n
    else
        read(tmp, *, iostat=ierr) n
        if (ierr == 0) npts = n * 3600
    end if
end subroutine set_timer

subroutine filterinit(delta_t)
!   specify some parameters for the relay identification plus some
!   parameters for the filter
!   references: i)seborg ii) astrom, automatica, 1984, 20, 645-651
!   taufil  = filter constant for xmeas(i)
!   initialise filter times (seconds)

    real(kind=8) :: alpha, fxmeas, taufil
    common /filter/ alpha(22), fxmeas(22), taufil(22)

    real(kind=8), intent(in) :: delta_t

    taufil = 5.0
    alpha = delta_t*3600. / taufil
end subroutine

subroutine filter_xmeas(time)
!   filter measurements
!   alpha = filter constant, see e.g. seeborg pp. 539-540 for more details.
!   alpha = 1,   no filtering
!   alpha = 0,   complete filtering (measurement is ignored)
!   currently set to ~0.2
    real(kind=8) :: alpha, fxmeas, taufil, time
    real(kind=8) :: xmeas, xmv
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22)

    if (time /= 0) then
        fxmeas = alpha*xmeas(1:22)+(1-alpha)*fxmeas
    else
        fxmeas = xmeas(1:22)
    end if
end subroutine filter_xmeas
