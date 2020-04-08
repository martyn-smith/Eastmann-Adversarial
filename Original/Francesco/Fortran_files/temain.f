C
C               Tennessee Eastman Process Control Test Problem
C
C                    James J. Downs and Ernest F. Vogel
C
C                  Process and Control Systems Engineering
C                        Tennessee Eastman Company
C                              P.O. Box 511
C                          Kingsport, TN  37662
C
C
C  Reference:
C    "A Plant-Wide Industrial Process Control Problem"
C    Presented at the AIChE 1990 Annual Meeting
C    Industrial Challenge Problems in Process Control, Paper #24a
C    Chicago, Illinois, November 14, 1990
C
C
C  Main program for demonstrating application of the Tennessee Eastman 
C  Process Control Test Problem
C
C
C===============================================================================
C
      include "teprob.f"
C
C  MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV, FXMEAS, xmv0, taufil, alpha
      COMMON/PV/ XMEAS(42), XMV(12)
      COMMON/FILTER/ FXMEAS(22), xmv0(12),taufil(22),alpha(22)
C
C   DISTURBANCE VECTOR COMMON BLOCK
C
      INTEGER IDV
      COMMON/DVEC/ IDV(20)
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, GAIN, TAUI, ERROLD,DELTAT
      COMMON/CTRL/ SETPT(20), GAIN(20), TAUI(20), ERROLD(20),DELTAT
C 
C   SPECIFY THE RELAY COMMON BLOCK
C
      DOUBLE PRECISION DUMM, PERT, SIGNP, RLSET
      INTEGER RLOUT, RLIN
      COMMON/RELAY/ DUMM, PERT, RLSET, SIGNP, RLOUT, RLIN
C
C  Local Variables
C
      INTEGER I, NN, NPTS
C
      DOUBLE PRECISION TIME, YY(50), YP(50)
C
      DOUBLE PRECISION dummy
C.....initialise filter times (seconds)
      DATA taufil/ 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0,
     +             5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0, 5.d0,
     +             5.d0, 5.d0, 5.d0, 5.d0/
c.....open files for OUTPUT
      OPEN(unit=30,file='out.dat',status='unknown')
c.....Filteres measurements
      OPEN(unit=40,file='Fout.dat',status='unknown')
c......Manipulated variables.......
      OPEN(unit=50,file='Inpt.dat',status='unknown')
C  Set the number of differential equations (states).  The process has 50
C  states.  If the user wishes to integrate additional states, NN must be 
C  increased by the number of additional differential equations.
C
      NN = 50
C
C  Set the number of points to simulate
C
C    48 hours of simulated time
      NPTS = 48*3600
C
C  Integrator Step Size:  1 Second Converted to Hours (time-base of simulation
C
      DELTAT = 1. / 3600.00001
C
C
C     Specify some parameters for the relay identification plus some
c     parameters for the filter
c
C   References: i)Seborg ii) Astrom, Automatica, 1984, 20, 645-651
C
C    TAUFIL  = filter constant for XMEAS(I)
C
      do 117 i=1,22
         ALPHA(i) = deltat*3600./taufil(i)
 117  continue
c.....initialise random number generator
      idum=-1
      dummy=ran1(idum)
C
C  Initialize Process
C  (Sets TIME to zero)
C
      CALL TEINIT(NN,TIME,YY,YP)
C
C      mass flow G/ mass flow H =
C       = (molecular weight*mol%G/molecular weight*mol%H)

      XMEAS(42)=(62.0*XMEAS(40))/(76*XMEAS(41))
C
C  Set the filtered variables equal to the steady-state values
c
      do 301 i=1,22
         FXMEAS(I)=XMEAS(I)
 301  CONTINUE
      do 302 i=1,12
         xmv0(i)=xmv(i)
 302  CONTINUE
C      set up the relay if desired.
c   dumm = Dummy variable...
c    dumm= 1 ----> relay is active
c    dumm = 0 ----> relay is inactive
      dumm = 0.
c..... d the perturbation size
      PERT = 5.
c..... sign of steady-state process gain (if increasing manipulated variable
c      increases measured variable then signp=1. else signp=-1.)
      SIGNP = -1.
c..... measured variable
      RLOUT = 9
c..... manipulated variable
      RLIN = 10
      RLSET = FXMEAS(RLOUT)
C
C  Set Controller Parameters
c.....reactor temperature control
      setpt(1)=120.40
      errold(1)=0.d0
      gain(1)=-10.3
      taui(1)=1.9
c.....reactor level control:
      setpt(2)=75.0
      errold(2)=0.d0
      gain(2)=0.8
      taui(2)=23
c.....product separator level control:
      setpt(3)=50.0
      errold(3)=0.d0
      gain(3)=1.4
      taui(3)= 96
c.....stripper level control:
      setpt(4)=50.0
      errold(4)=0.d0
      gain(4)= 5
      taui(4)= 50
c.....stripper underflow control:
      setpt(5)=22.949
      errold(5)=0.d0
      gain(5)=1.2
      taui(5)=50.
c.....G/H ratio control:
      setpt(6)=1.0
      errold(6)=0.d0
      gain(6)=7.1
      taui(6)=39.
c.....reactor pressure control:
      setpt(7)=2705.0
      errold(7)=0.d0
      gain(7)=1.1
      taui(7)=49.5
c.....purge gas B component control:
      setpt(8)=13.823
      errold(8)=0.d0
      gain(8)=-14.5
      taui(8)=30.1
c.....reactor feed A component control:
      setpt(9)=32.188
      errold(9)=0.d0
      gain(9)=4.1
      taui(9)=81.9
C  Set all Disturbance Flags to OFF
C
      DO 100 I = 1, 20
          IDV(I) = 0
 100  CONTINUE
c
c       set disturbance 1 on 
c       idv(1)=1
c       idv(4)=1
c       idv(8)=1
c       idv(12)=1
c       idv(15)=1
C
C  Simulation Loop
C
      DO 1000 I = 1, NPTS
C
C        increase setpoints after 5 hours
C        IF (TIME.GT.5) setpt(1)=120.4
C        IF (TIME.GT.5) setpt(2)=75
C        IF (TIME.GT.5) setpt(3)=50
C        IF (TIME.GT.5) setpt(4)=50
C        IF (TIME.GT.5) setpt(5)=22.949*0.75
C        IF (TIME.GT.5) setpt(6)=1*0.67
c        IF (TIME.GT.5) setpt(7)=2705-60
        IF (TIME.GT.5) setpt(8)=15.82
C        IF (TIME.GT.5) setpt(9)=32.188
        
          CALL CONTRL
C
          CALL OUTPUT(TIME)
C
          CALL INTGTR(NN,TIME,DELTAT,YY,YP)
C
C       Filter measurements
C
C  ALPHA = Filter Constant, See e.g. Seeborg pp. 539-540 for more details
C
C  ALPHA = 1,   No filtering
C  ALPHA = 0,   Complete filtering (Measurement is ignored)
C
          do 2001 k=1,22
             FXMEAS(k)=ALPHA(k)*XMEAS(k)+(1-ALPHA(k))*FXMEAS(k)
 2001     CONTINUE
C
 1000 CONTINUE
C
      STOP
      END
C
C===============================================================================
C
      SUBROUTINE CONTRL
      SAVE 
C
C  Discrete control algorithms
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS, XMV, FXMEAS,xmv0, taufil, alpha
      COMMON/PV/ XMEAS(42), XMV(12)
      COMMON/FILTER/ FXMEAS(22), xmv0(12),taufil(22),alpha(22)
C 
C  RELAY COMMON BLOCK
C
      DOUBLE PRECISION DUMM, RLSET, PERT, SIGNP
      INTEGER RLOUT, RLIN
      COMMON/RELAY/ DUMM, PERT, RLSET, SIGNP, RLOUT, RLIN
C
C
C   CONTROLLER COMMON BLOCK
C
      DOUBLE PRECISION SETPT, GAIN, TAUI, ERROLD,DELTAT
      COMMON/CTRL/ SETPT(20), GAIN(20), TAUI(20), ERROLD(20),DELTAT
C
      DOUBLE PRECISION ERR
C
      DOUBLE PRECISION DUMMY
C
      dimension icount(20)
      data icount /20*0/
      data istart /0/
C
C  Example PI Controller:
C    Stripper Level Controller
C
C    Calculate Error
C
C      ERR = SETPT - XMEAS(15)
C
C    Proportional-Integral Controller (Velocity Form)
C         GAIN = Controller Gain
C         TAUI = Reset Time (min)
C
C      DXMV = GAIN * ( ( ERR - ERROLD ) + ERR * DELTAT * 60. / TAUI )
C
C      XMV(8) = XMV(8) - DXMV
C
C      ERROLD = ERR
C
C......IMPOSE INTEGRAL DESATURATION
C
C       XMV(8)=MAX(0.,MIN(100.,XMV(8)))      
C
C Measured variable 42 has to be calculated here
C because it isn't calculated in the teprob.f
C
      XMEAS(42)=(62.0*XMEAS(40))/(76*XMEAS(41))
C
C
C      relay testing
C
        if(dumm.gt.0.5)then
           if(Fxmeas(RLOUT).gt.RLSET)then
              xmv(RLIN)=xmv0(RLIN)-signp*pert         
           elseif(Fxmeas(RLOUT).lt.RLSET)then
              xmv(RLIN)=xmv0(RLIN)+signp*pert          
           endif
        endif
C
c.......reactor temperature control (reactor temperature to cooling water flow)
        err=setpt(1)-xmeas(9)
        xmv(10)=xmv(10)+
     +       gain(1)*((err-errold(1))+err*deltat*60./taui(1))
        errold(1)=err
        xmv(10)=max(0.d0,min(100.d0,xmv(10)))
c.......reactor level control:
        err=setpt(2)-fxmeas(8)
        xmv(2)=xmv(2)+
     +       gain(2)*((err-errold(2))+err*deltat*60./taui(2))
        errold(2)=err
        xmv(2)=max(0.d0,min(100.d0,xmv(2)))
c.......product separator level control:
        err=setpt(3)-fxmeas(12)
        xmv(11)=xmv(11)+
     +       gain(3)*((err-errold(3))+err*deltat*60./taui(3))
        errold(3)=err
        xmv(11)=max(0.d0,min(100.d0,xmv(11)))
c.......stripper level control:
        err=setpt(4)-fxmeas(15)
        xmv(7)=xmv(7)+
     +       gain(4)*((err-errold(4))+err*deltat*60./taui(4))
        errold(4)=err
        xmv(7)=max(0.d0,min(100.d0,xmv(7)))
c.......stripper underflow control:
        err=setpt(5)-xmeas(17)
        xmv(8)=xmv(8)+
     +       gain(5)*((err-errold(5))+err*deltat*60./taui(5))
        errold(5)=err
        xmv(8)=max(0.d0,min(100.d0,xmv(8)))
c.......G/H ratio control:
        err=setpt(6)-xmeas(42)
        xmv(1)=xmv(1)+
     +       gain(6)*((err-errold(6))+err*deltat*60./taui(6))
        errold(6)=err
        xmv(1)=max(0.d0,min(100.d0,xmv(1)))
c.......reactor pressure control:
        err=setpt(7)-xmeas(7)
        xmv(4)=xmv(4)+
     +       gain(7)*((err-errold(7))+err*deltat*60./taui(7))
        errold(7)=err
        xmv(4)=max(0.d0,min(100.d0,xmv(4)))
c.......purge gas B component control:
        err=setpt(8)-xmeas(30)
        xmv(6)=xmv(6)+
     +       gain(8)*((err-errold(8))+err*deltat*60./taui(8))
        errold(8)=err
        xmv(6)=max(0.d0,min(100.d0,xmv(6)))
c.......reactor feed A component control:
        err=setpt(9)-xmeas(23)
        xmv(3)=xmv(3)+
     +       gain(9)*((err-errold(9))+err*deltat*60./taui(9))
        errold(9)=err
        xmv(3)=max(0.d0,min(100.d0,xmv(3)))
C
c.....gives a bounded random variable dummy
c
      idum=1
      dummy=ran1(idum)
c
c.....if want to use a random binary signal of  clockwidth equal to n
c     samples and upper limit 1, lower limit 0 could add
c      icount(1)=icount(1)+1
c      n=10
c      if(mod(icount(1),n).eq.0)bdummy=dnint(dummy)
c      write(6,*) dummy,bdummy
      istart=1
c
      RETURN
      END
C
C===============================================================================
C
      SUBROUTINE OUTPUT(time)
      save icount
C
C
C   MEASUREMENT AND VALVE COMMON BLOCK
C
      DOUBLE PRECISION XMEAS,XMV,time,FXMEAS,xmv0,taufil,alpha
      COMMON/PV/ XMEAS(42), XMV(12)
      COMMON/FILTER/ FXMEAS(22), xmv0(12),taufil(22),alpha(22)
      data icount /0/
C
c.....write MATLAB output every n samples
      n=30
      if(mod(icount,n).eq.0)then
c.........matlab output
         write(30,101) time,xmeas(8),xmeas(9),xmeas(12),xmeas(15),
     +  xmeas(7),xmeas(17),xmeas(30),xmeas(40)/xmeas(41),xmv(2),
     +  xmv(10),xmv(11),xmv(7),xmv(4),xmv(8),xmv(6),xmv(1),
     +  xmeas(23)/xmeas(25),xmv(3)
       write(40,102) time,(FXMEAS(k), k=1,22),(XMEAS(k),k=23,42)
       write(50,103) time,(xmv(k),k=1,12)
      endif
      icount=icount+1
c Use this sort of format statement for output for matlab (all data on a single
c line), the first number is the number of data entries
101     format(21e23.15)
 102    format(43e23.15)
 103    format(13e23.15)
C
      RETURN
      END
C
C===============================================================================
C
      SUBROUTINE INTGTR(NN,TIME,DELTAT,YY,YP)
C
C  Euler Integration Algorithm
C
C
      INTEGER I, NN
C
      DOUBLE PRECISION TIME, DELTAT, YY(NN), YP(NN)
C
      CALL TEFUNC(NN,TIME,YY,YP)
C
      TIME = TIME + DELTAT
C
      DO 100 I = 1, NN
C
          YY(I) = YY(I) + YP(I) * DELTAT
C
 100  CONTINUE
C
      RETURN
      END
c
      real function ran1(idum)
c.....see Numerical Recipes in FORTRAN, p.196
c     Returns 0.0 to 1.0 random deviate. Set idum to any negative value to
c     initialise
      dimension r(97)
      save ix1,ix2,ix3,r
      parameter(m1=259200,ia1=7141,ic1=54773,rm1=1./m1)
      parameter(m2=134456,ia2=8121,ic2=28411,rm2=1./m2)
      parameter(m3=243000,ia3=4561,ic3=51349)
      data iff /0/
      if(idum.lt.0 .or. iff.eq.0)then
        iff=1
        ix1=mod(ic1-idum,m1)
        ix1=mod(ia1*ix1+ic1,m1)
        ix2=mod(ix1,m2)
        ix1=mod(ia1*ix1+ic1,m1)
        ix3=mod(ix1,m3)
        do 11 j=1,97
           ix1=mod(ia1*ix1+ic1,m1)
           ix2=mod(ia2*ix2+ic2,m2)
           r(j)=(float(ix1)+float(ix2)*rm2)*rm1
 11     continue
        idum=1
      endif
      ix1=mod(ia1*ix1+ic1,m1)
      ix2=mod(ia2*ix2+ic2,m2)
      ix3=mod(ia3*ix3+ic3,m3)
      j=1+(97*ix3)/m3
      if(j.gt.97 .or. j.lt.1)pause
      ran1=dble(r(j))
      r(j)=(float(ix1)+float(ix2)*rm2)*rm1
      return
      end
