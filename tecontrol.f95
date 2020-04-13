subroutine contrlinit
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

end

subroutine contrl
    save 
!   discrete control algorithms

!   measurement and valve common block
    real(kind=8) :: xmeas, xmv, fxmeas, xmv0, taufil, alpha
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22), xmv0(12)

!   relay common block
    integer :: rlout, rlin
    real(kind=8) :: dumm, rlset, pert, signp
    common /relay/ dumm, pert, rlset, signp, rlout, rlin

!   controller common block
    real(kind=8) :: setpt, gain, taui, errold, delta_t
    common /ctrl/ setpt(20), gain(20), taui(20), errold(20), delta_t

    real(kind=8) :: dummy, err, rand1
!    dimension icount(20)
!    data icount /20*0/
!    data istart /0/

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

!   measured variable 42 has to be calculated here
!   because it isn't calculated in teprob
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
            + gain(1)*((err-errold(1))+err*delta_t*60./taui(1))
    errold(1)=err
    xmv(10)=max(0.,min(100.,xmv(10)))
!   reactor level control:
    err=setpt(2)-fxmeas(8)
    xmv(2)=xmv(2) &
            + gain(2)*((err-errold(2))+err*delta_t*60./taui(2))
    errold(2)=err
    xmv(2)=max(0.,min(100.,xmv(2)))
!   product separator level control:
    err=setpt(3)-fxmeas(12)
    xmv(11)=xmv(11) &
            + gain(3)*((err-errold(3))+err*delta_t*60./taui(3))
    errold(3)=err
    xmv(11)=max(0.,min(100.,xmv(11)))
!   stripper level control:
    err=setpt(4)-fxmeas(15)
    xmv(7)=xmv(7) &
            + gain(4)*((err-errold(4))+err*delta_t*60./taui(4))
    errold(4)=err
    xmv(7)=max(0.,min(100.,xmv(7)))
!   stripper underflow control:
    err=setpt(5)-xmeas(17)
    xmv(8)=xmv(8) &
                + gain(5)*((err-errold(5))+err*delta_t*60./taui(5))
    errold(5)=err
    xmv(8)=max(0.,min(100.,xmv(8)))
!   g/h ratio control:
    err=setpt(6)-xmeas(42)
    xmv(1)=xmv(1) &
            + gain(6)*((err-errold(6))+err*delta_t*60./taui(6))
    errold(6)=err
    xmv(1)=max(0.,min(100.,xmv(1)))
!   reactor pressure control:
    err=setpt(7)-xmeas(7)
    xmv(4)=xmv(4) &
            + gain(7)*((err-errold(7))+err*delta_t*60./taui(7))
    errold(7)=err
    xmv(4)=max(0.,min(100.,xmv(4)))
!   purge gas b component control:
    err=setpt(8)-xmeas(30)
    xmv(6)=xmv(6) &
            + gain(8)*((err-errold(8))+err*delta_t*60./taui(8))
    errold(8)=err
    xmv(6)=max(0.,min(100.,xmv(6)))
!   reactor feed a component control:
    err=setpt(9)-xmeas(23)
    xmv(3)=xmv(3) &
            + gain(9)*((err-errold(9))+err*delta_t*60./taui(9))
    errold(9)=err
    xmv(3)=max(0.,min(100.,xmv(3)))

!   gives a bounded random variable dummy
    dummy=rand1()

!   if want to use a random binary signal of clockwidth equal to n
!   samples and upper limit 1, lower limit 0 could add
!     icount(1)=icount(1)+1
!     n=10
!     if(mod(icount(1),n) ==  0)bdummy=dnint(dummy)
!     write(6,*) dummy,bdummy
!    istart=1
    return
end
