"""
###############################################################################

Model of the TE (Tennessee Eastmann) challenge reactor.

The Plant takes four inputs, A, C, D, E, and produces two outputs,
G and H.

Primary reactions:

A(g) + C(g) + D(g) -> G(l)
A(g) + D(g) + E(g) -> H(l)

Byproduct reactions:

A(g) + E(g) -> F(l)
3D(g) -> 2F(l)

All are exothermic, reversible, and first-order
(rates follow an Arrhenius relation.)

Product flow is: 
a,d,e ->                                  c ->
    reactor -> condensor -> separator -> stripper -> product
          <- compressor <- purge <-
          <---------------------------------

  Differences between the code and its description in the paper:

  1.  Subroutine TEINIT has time in the argument list.  TEINIT sS.et time
      to zero.

  3.  Process disturbances 14 through 20 do NOT need to be used in
      conjunction with another disturbance as stated in the paper.  All
      disturbances can be used alone or in any combination.

###############################################################################

Notes on C to Python conversion:

    Lots more OO and abstraction, in particular Stream() (13x) and Vessel() (4x).
    Also, add Sensors().  Maybe some JSON too.  Everyone like json.

    Indexing is all messed-up.  There are off-by-one errors, sure, but also
    off-by-TWO errors.  MUST replace with a dict.

    self.derivatives is zeroed?

High-level overview of run():

1 ) set IDVs
2 ) set wlk values
3 ) set Reactor ES, stream[3] properties (the latter by calling sub8)
4 ) load some state into Vessels
5 ) calculate XL from UCL
6 ) sub2?
7 ) sub4? 
8 ) set VL
9 ) calculate Pressure of A,B,C
10) calculate Pressure of D,E,F,G,H
11) calculate XVs from PPs
12) calculate whatever RRs are 
13) ditto CRXR, XMWs
14) Set temps
15) set_heat on streams()
16) calculate flows
17) calculate flms
18) calculate flow mass fracs
19) calculate flow concs
20) temp and heat conservation
21) underflow
22) XMEAS update
23) errors (based on GROUND TRUTH - change to XMEAS)
24) Separator Energy Balance?
25) VCVs, whatever they are 
26) update derivatives

############################################################################### 

    50 State Variables

#   |0|,
#   |?|,

#   |1 --- 3||4 --- 8||  9 ||10 - 12||13 - 17|| 18 ||19 - 26|| 27 ||28 - 35|| 36 |,
#   | R.ucv || R.ucl ||R.et|| S.ucv || S.ucl ||S.et|| C.ucl ||C.et|| V.ucv ||V.et|,

#   | 37|| 38|| 39-50|,
#   |twr||tws|| vpos |,

"""

"""
Four core loops:

    Reactor.T <- Coolant flow
    Reactor.level <- E feed flow
    Separator.level <- Condensor Coolant flow
    Stripper.level <- Stripper liquid efflux valve
"""
import numpy as np
from math import exp, sqrt
import constants

REACTOR_P_HIGH = 3e3
REACTOR_LEVEL_HIGH = 24
REACTOR_LEVEL_LOW = 2
REACTOR_T_HIGH = 175
SEP_LEVEL_HIGH = 12
SEP_LEVEL_LOW = 1
STRIP_LEVEL_HIGH = 8
STRIP_LEVEL_LOW = 1

class TEprob():

    #static int teinit(const integer *nn, doublereal *time, doublereal *yy, doublereal *yp)
    def __init__(self):
        # the original C matches the following regex:
        # constants.[a-z]{2,3}\[[0-7]\] = (\(float\))?
        self.state = [10.40491389, 4.363996017, 7.570059737, .4230042431, 24.15513437,
                        2.942597645, 154.3770655, 159.186596, 2.808522723, 63.75581199, 
                        26.74026066, 46.38532432, .2464521543, 15.20484404, 1.852266172, 
                        52.44639459, 41.20394008, .569931776, .4306056376, .0079906200783, 
                        .9056036089, .016054258216, .7509759687, .088582855955, 48.27726193,
                        39.38459028, .3755297257, 107.7562698, 29.77250546, 88.32481135,
                        23.03929507, 62.85848794, 5.546318688, 11.92244772, 5.555448243, 
                        .9218489762, 94.59927549, 77.29698353, 63.05263039, 53.97970677, 
                        24.64355755, 61.30192144, 22.21, 40.06374673, 38.1003437, 
                        46.53415582, 47.44573456, 41.10581288, 18.11349055, 50.]
        self.derivatives = [None] * len(self.state)

        #L200 
        # Removed a weird TEproc alteration that occurs BEFORE init, or PV.__init__
        self.dvec = DVEC()   
        self.pv = PV(self.state)
        self = TEproc(self.pv)
        self.wlk = WLK()
        self.time = 0.

    #def update(self, state):
        #will need this to accept states from a http server
        #self.state = state

    # called as run(integer *nn=len(state), time, &state[1]=*state, &derivatives[1]=*derivatives)
    def run(self):
        #locals: flms, hwlk, vovrl, vpr, uas,flcoef, pr, tmpfac, uarlev, r1f, r2f, uac, fin[8]
        xcmp = [None] * 41 #?

        #L500
        self.dvec.set_IDVs_0_or_1() #why though
        self.wlk.set_idvwlk(self.dvec)

        #L900
        for i in range(9): #(i = 1; i <= 9; ++i)
            if (self.time >= self.wlk.tnext[i]):
                hwlk, swlk, spwlk = self.wlk.get_h_s_spwlk(i)
                self.wlk.tlast[i] = self.wlk.tnext[i]   
                self.wlk.sub5(i, swlk, spwlk)

        #L910
        for i in range(9, 12): #(i = 10; i < 12; ++i) #why though
            if (self.time >= self.wlk.tnext[i]): 
                hwlk, swlk, spwlk = self.wlk.get_h_s_spwlk(i)
                self.wlk.tlast[i] = self.wlk.tnext[i]
                if (swlk > .1):
                    self.wlk.adist[i] = swlk
                    self.wlk.bdist[i] = spwlk
                    self.wlk.cdist[i] = -(swlk * 3. + spwlk * .2) / .01
                    self.wlk.ddist[i] = (swlk * 2. + spwlk * .1) / .001
                    self.wlk.tnext[i] = self.wlk.tlast[i] + .1
                else:
                    self.dvec.isd = -1
                    hwlk = self.wlk.hspan[i] * tesub7(self.dvec.isd) + self.wlk.hzero[i]
                    self.wlk.adist[i] = 0.
                    self.wlk.bdist[i] = 0.
                    # Computing 2nd power
                    self.wlk.cdist[i] = self.wlk.idvwlk[i] / (hwlk **2)
                    self.wlk.ddist[i] = 0.
                    self.wlk.tnext[i] = self.wlk.tlast[i] + hwlk

        #L950
        if (self.time == 0.): #why though
            self.wlk.reset()

        #stream num and IDV matches
        self.stream["Strp"].x[0] = (self.wlk.sub8(1, self.time) 
                                           - self.dvec.idv[0] * .03 #A/C feed
                                           - self.dvec.idv[1] * .00243719) #B composition
        self.stream["Strp"].x[1] = self.wlk.sub8(2, self.time) + self.dvec.idv[1] * .005
        self.stream["Strp"].x[2] = (1 - (self.stream["Strp"].x[0] 
                                                + self.stream["Strp"].x[1]))  #xC = 1 - xA +xB

        #ambiguous if A or D.  Also sub8 is weird.  Also, D and A feed messed up.
        self.stream["D"].T = self.wlk.sub8(3, self.time) + self.dvec.idv[2] * 5 #D feed.  Yes, it is Temp.
        self.stream["C"].T = self.wlk.sub8(4, self.time)

        self.tcwr = self.wlk.sub8(5, self.time) + self.dvec.idv[3] * 5. #Reactor coolant
        self.tcws = self.wlk.sub8(6, self.time) + self.dvec.idv[4] * 5. #Condensor coolant

        r1f = self.wlk.sub8(7, self.time)
        r2f = self.wlk.sub8(8, self.time)

        #L1010
        self.C.load(self.state)
        self.R.load(self.state)
        self.S.load(self.state)
        self.V.load(self.state)

        self.twr = self.state[37]
        self.tws = self.state[38]

        #L1035
        for i in range(0, 12):
            self.vpos[i] = self.state[i + 38]

        #L1040 abstracted into earlier load() method

        #L1050 (conc. calculations, or xl) abstracted into Vessel property

        #es calculation abstracted into property methods

        self.R.sub2()
        self.S.sub2()
        self.C.sub2()
        self.V.sub2(2)
        self.R.sub4()
        self.S.sub4()
        self.C.sub4()

        # vl[x] = level
        # level and vv abstracted into property methods

        #L1110
        for i in range(3): #(i = 1; i <= 3; ++i)
            self.R.pp[i] = (self.R.ucv[i] * constants.rg * self.R.T_K / 
                self.R.vv)
            self.S.pp[i] = (self.S.ucv[i] * constants.rg * self.S.T_K / 
                self.S.vv)

        #L1120
        for i in range(4, 8): #(i = 4; i <= 8; ++i) 
            #calculating vapour pressures, D-H only.  Antoine.
            vpr = exp(constants.avp[i] + constants.bvp[i] 
                         / (self.R.T + constants.cvp[i]))
            self.R.pp[i] = vpr * self.R.xl[i]
            vpr = exp(constants.avp[i] + constants.bvp[i] 
                         / (self.S.T + constants.cvp[i]))
            self.S.pp[i] = vpr * self.S.xl[i]

        #L1130 (xv, or gaseous concs) abstracted into property

        #L1140
        for i in range(4, 8):
            self.R.ucv[i] = self.R.utv * self.R.xv[i]
            self.S.ucv[i] = self.S.utv * self.S.xv[i]

        #L1200
        self.set_reaction_rates(r1f, r2f)
        self.set_crxrs()

        self.rh = (self.rr[0] * self.htr[0] 
                         + self.rr[1] * self.htr[1])

        #self.xmws zeroing removed.
        #why is recirc stream set to same properties as S only?
        #L2010
        #was (5,7,8,9,10,12)
        self.stream["Strp"].x = self.V.xv
        self.stream["R out"].x = self.R.xv
        self.stream["Recirc"].x = self.S.xv
        self.stream["Purge"].x = self.S.xv #was 8/9
        self.stream["S out"].x = self.S.xl #was 9/10
        self.stream["Product"].x = self.C.xl #was 11

        for i in (0,1,5,7,8,9): #why just these
            self.xmws[i] = sum(x * xmw for x, xmw in 
                                      zip(self.stream[i].x, constants.xmw))

        #self.set_stream_temps() 
        #ERR: Sep has two outputs not three!
        self.stream["Strp"].T = self.V.T
        self.stream["R out"].T = self.R.T
        self.stream["Recirc"].T = self.S.T
        self.stream["Purge"].T = self.S.T
        self.stream["S out"].T = self.S.T 
        self.stream["Product"].T = self.C.T

        for i in (0, 1, 2, 3, 5, 7, 8):
            self.stream[i].set_heat(1)
        self.stream[9].H = self.stream[8].H
        self.stream[10].set_heat(0) 
        self.stream[12].set_heat(0) 

        self.stream[0].flow = self.vpos[0] * self.vrng[0] / 100
        self.stream[1].flow = self.vpos[1] * self.vrng[1] / 100
        self.stream[2].flow = (self.vpos[2] 
                              * (1. - self.dvec.idv[5]) # A Feed Loss (Stream 1) 
                              * self.vrng[2] / 100)
        self.stream["C"].flow = (self.vpos[3] 
                              * (1. - self.dvec.idv[6] * .2)  #C Header Pressure Loss - Reduced Availability (Stream 4)
                              * self.vrng[3] / 100.
                              + 1e-10)
        self.stream[10].flow = self.vpos[6] * self.vrng[6] / 100.
        self.stream[12].flow = self.vpos[7] * self.vrng[7] / 100.
        uac = (self.vpos[8] * self.vrng[8] * (self.wlk.sub8(9, self.time) + 1.) / 100.)
        self.fwr = self.vpos[9] * self.vrng[9] / 100.
        self.fws = self.vpos[10] * self.vrng[10] / 100.
        self.agsp = (self.vpos[11] + 150.) / 100.

        # calculating flow from V to Reactor, as stream 5
        #NOTHING SHOULD FLOW FROM V TO REACTOR.  But whatever, this is DEFINITELY going to reactor.
        self.stream[5].set_flow(self.V, self.R)
        # calculating flow from Reactor to Separator, as stream 6
        self.stream[6].set_flow()
        self.stream[9].set_flow()

        dlp = max(self.V.P - self.R.P, 0)
        flms = sqrt(dlp) * 1937.6
        self.stream[5].flow = flms / self.xmws[5]


        dlp = max(self.R.P - self.S.P, 0)
        flms = sqrt(dlp) * 4574.21 * (1 - self.wlk.sub8(12, self.time) * .25)
        self.stream[6].flow = flms / self.xmws[7]

        # calculating flow from Separator to Purge?
        dlp = max(self.S.P - 760. , 0)
        flms = sqrt(dlp) * self.vpos[5] * .151169
        self.stream[9].flow = flms / self.xmws[9]

        pr = min(max(self.V.P / self.S.P, 1), 
                self.cpprmx)
        flcoef = self.cpflmx / 1.197
        # Computing 3rd power 
        flms = self.cpflmx + flcoef * (1 - pr * (pr**2))

        self.compressor_work = (flms 
                            * (self.S.T_K) * 1.8e-6 * 1.9872 
                            * (self.V.P - self.S.P) 
                            / (self.xmws[8] * self.S.P))

        self.stream[8].set_flow(self.V, self.S)
        dlp = max(self.V.P - self.S.P, 0)
        flms -= self.vpos[4] * 53.349 * sqrt(dlp)
        flms = max(flms, .001)
        self.stream[8].flow = flms / self.xmws[8]

        self.stream[8].H += self.compressor_work / self.stream[8].flow
        #L5020
        self.update_fractional_flows()
        
        if (self.stream[10].flow > .1):
            if (self.C.T > 170.):
                tmpfac = self.C.T - 120.262
            elif (self.C.T < 5.292):
                tmpfac = .1
            else:
                tmpfac = (363.744 / (177. - self.C.T) 
                          - 2.22579488)
            #vovrl = stripper flow mass balance ()
            vovrl = self.stream["C"].flow / self.stream["Product"].flow * tmpfac
            self.sfr[3] = vovrl * 8.501 / (vovrl * 8.501 + 1)
            self.sfr[4] = vovrl * 11.402 / (vovrl * 11.402 + 1)
            self.sfr[5] = vovrl * 11.795 / (vovrl * 11.795 + 1)
            self.sfr[6] = vovrl * .048 / (vovrl * .048 +  1)
            self.sfr[7] = vovrl * .0242 / (vovrl * .0242 + 1)
        else:
            self.sfr[3] = .9999
            self.sfr[4] = .999
            self.sfr[5] = .999
            self.sfr[6] = .99
            self.sfr[7] = .98

        #L6010
        fin = self.stream[3].flow_frac + self.stream[10].flow_frac

        #L6020
        self.stream[4].flow_frac = [i * j for i, j in zip(fin, self.sfr)]
        self.stream[11].flow_frac = [i - j  for i, j in zip(fin, self.stream[4].flow_frac)]
        self.stream[4].flow = sum(self.stream[4].flow_frac)
        self.stream[11].flow = sum(self.stream[11].flow_frac)

        #L6030
        for i in range(8):
            self.stream[4].x[i] = self.stream[4].flow_frac[i] / self.stream[4].flow
            self.stream[11].x[i] = self.stream[11].flow_frac[i] / self.stream[11].flow

        self.stream[3].T = self.C.T
        self.stream[10].T = self.C.T
        self.stream[3].set_heat(1)
        self.stream[10].set_heat(0)
        self.stream[6].flow = self.stream[5].flow
        self.stream[6].H = self.stream[5].H
        self.stream[6].T = self.stream[5].T

        #L6130
        self.stream[6].x = self.stream[5].x
        self.stream[6].flow_frac = self.stream[5].flow_frac

        if (self.R.level / 7.8 > 50.):
            uarlev = 1.
        elif (self.R.level / 7.8 < 10.):
            uarlev = 0.
        else:
            uarlev = self.R.level * .025 / 7.8 - .25

        self.uar = (uarlev 
                          * (self.agsp**2 * -0.5 
                             + self.agsp * 2.75 - 2.5) 
                          * .85549)
        self.R.underflow = (self.uar 
                           * (self.twr - self.R.T) 
                           * (1 - self.wlk.sub8(10, self.time) * .35))

        uas = .404655 * (1 - 1 / ((self.stream[7].flow / 3528.73)**4 + 1)) 
        self.S.underflow = (uas 
                           * (self.tws - self.stream[7].T) 
                           * (1 - self.wlk.sub8(11, self.time) * .25))

        self.C.underflow = (uac * (100. - self.C.T) 
                            if self.C.T < 100 else 0)

        self.dvec.isd = 0
        self.pv.update_continuous_xmeas(self)
        self.pv.check_criticals(self, self.dvec)

        if (self.time > 0. and self.dvec.isd == 0):
            #L6500
            for i, xn in enumerate(self.xns): #1-22 inclusive
                self.pv.xmeas[i] += tesub6(xn) #wtf?
        
        # Temp store for sampled process measurements
        # reactor (stream 6) mol%
        xcmp[22] = self.stream[6].x[0] * 100  # A
        xcmp[23] = self.stream[6].x[1] * 100  # B
        xcmp[24] = self.stream[6].x[2] * 100  # C
        xcmp[25] = self.stream[6].x[3] * 100  # D
        xcmp[26] = self.stream[6].x[4] * 100  # E
        xcmp[27] = self.stream[6].x[5] * 100  # F
        # purge (stream 9) mol%
        xcmp[28] = self.stream[8].x[0] * 100  # A
        xcmp[29] = self.stream[8].x[1] * 100  # B
        xcmp[30] = self.stream[8].x[2] * 100  # C
        xcmp[31] = self.stream[8].x[3] * 100  # D
        xcmp[32] = self.stream[8].x[4] * 100  # E
        xcmp[33] = self.stream[8].x[5] * 100  # F
        xcmp[34] = self.stream[8].x[6] * 100  # G
        xcmp[35] = self.stream[8].x[7] * 100  # H
        # product (stream 10) mol%.  Fixed off-by-TWO error
        xcmp[36] = self.stream[10].x[3] * 100 # D
        xcmp[37] = self.stream[10].x[4] * 100 # E
        xcmp[38] = self.stream[10].x[5] * 100 # F
        xcmp[39] = self.stream[10].x[6] * 100 # G
        xcmp[40] = self.stream[10].x[7] * 100 # H

        if (self.time == 0.): #init?
            #L7010
            #self.pv.update_sampled_xmeas(xcmp)
            for i in range(22, 41): # was 23
                #WHY?????
                self.xdel[i] = xcmp[i]
                self.pv.xmeas[i] = xcmp[i]
            self.tgas = .1
            self.tprod = .25

        if (self.time >= self.tgas):
            #L7020
            #reactor feed and purge analysis
            #self.pv.update_sampled_xmeas(xcmp)
            for i in range(22, 36):
                self.pv.xmeas[i] = self.xdel[i] + tesub6(self.xns[i])
                self.xdel[i] = xcmp[i]
            self.tgas += .1

        if (self.time >= self.tprod):
            #L7030
            #self.pv.update_product()
            #self.pv.update_sampled_xmeas("product", xcmp, self)
            #PRODUCT FEED
            for i in range(36, 41):
                self.pv.xmeas[i] = self.xdel[i] + tesub6(self.xns[i])
                self.xdel[i] = xcmp[i]
            self.tprod += .25

        #L9010
        for i in range(8):
            self.derivatives[i] = (self.stream[6].flow_frac[i] 
                     - self.stream[7].flow_frac[i]
                     + self.crxr[i])
            self.derivatives[i + 9] = (self.stream[7].flow_frac[i] - self.stream[8].flow_frac[i] 
                         - self.stream[9].flow_frac[i] - self.stream[10].flow_frac[i])
            self.derivatives[i + 18] = self.stream[11].flow_frac[i] - self.stream[12].flow_frac[i]
            self.derivatives[i + 27] = (self.stream[0].flow_frac[i] + self.stream[1].flow_frac[i]  
                          + self.stream[2].flow_frac[i] + self.stream[4].flow_frac[i]  
                          + self.stream[8].flow_frac[i] - self.stream[5].flow_frac[i])

        self.derivatives[9] = (self.stream[6].H * self.stream[6].flow 
                         - self.stream[7].H * self.stream[7].flow 
                         + self.rh + self.R.underflow)
        #     Here is the "correct" version of the separator energy balance:
        #     Original off-by-two.
        self.derivatives[18] = (self.stream[6].H * self.stream[6].ftm
                                - (self.stream[7].H * self.stream[7].ftm - self.compressor_work)
                                - self.stream[8].H * self.stream[8].ftm
                                - self.stream[9].H * self.stream[9].ftm
                                - self.S.underflow)
        self.derivatives[27] = (self.stream[3].H 
                  * self.stream[3].flow + self.stream[10].H 
                  * self.stream[10].flow - self.stream[4].H 
                  * self.stream[4].flow - self.stream[12].H 
                  * self.stream[12].flow + self.C.underflow)
        self.derivatives[36] = (self.stream[0].H
                  * self.stream[0].flow + self.stream[1].H 
                  * self.stream[1].flow + self.stream[2].H 
                  * self.stream[2].flow + self.stream[4].H 
                  * self.stream[4].flow + self.stream[8].H 
                  * self.stream[8].flow - self.stream[5].H 
                  * self.stream[5].flow)
        self.derivatives[37] = ((self.fwr * 500.53 
                   * (self.tcwr - self.twr) 
                   - self.R.underflow * 1e6 / 1.8) 
                   / self.hwr)
        self.derivatives[38] = ((self.fws * 500.53 
                   * (self.tcws - self.tws) 
                   -  self.S.underflow * 1e6 / 1.8) 
                   / self.hws)
        self.ivst[9] = self.dvec.idv[13]
        self.ivst[10] = self.dvec.idv[14]
        self.ivst[4] = self.dvec.idv[18]
        self.ivst[6] = self.dvec.idv[18]
        self.ivst[7] = self.dvec.idv[18]
        self.ivst[8] = self.dvec.idv[18]
        #L9020
        for i in range(12):
            if ((self.time == 0.) or 
               (abs(self.vcv[i] - self.pv.xmv[i]) 
                > self.vst[i] * self.ivst[i])):
                self.vcv[i] = self.pv.xmv[i]
            if (self.vcv[i] < 0.):
                self.vcv[i] = 0.
            if (self.vcv[i] > 100.):
                self.vcv[i] = 100.
            self.derivatives[i + 38] = ((self.vcv[i] - self.vpos[i]) 
                           / self.vtau[i])
        if (self.time > 0. and self.dvec.isd != 0):
            #L9030.
            #WHAT?!
            self.derivatives = [0.] * len(self.state)

###############################################################################
"""
    41 Process Measurements (XMEAS) 
    Continuous Process Measurements

    XMEAS(0)   A Feed  (stream 0)                    kscmh
    XMEAS(1)   D Feed  (stream 1)                    kg/hr
    XMEAS(2)   E Feed  (stream 2)                    kg/hr
    XMEAS(3)   A and C Feed  (stream 3)              kscmh
    XMEAS(4)   Recycle Flow  (stream 7)              kscmh
    XMEAS(5)   Reactor Feed Rate  (stream 5)         kscmh
    XMEAS(6)   Reactor Pressure                      kPa gauge
    XMEAS(7)   Reactor Level                         %
    XMEAS(8)   Reactor Temperature                   Deg C
    XMEAS(9)   Purge Rate (stream 8)                 kscmh
    XMEAS(10)  Product Sep Temp                      Deg C
    XMEAS(11)  Product Sep Level                     %
    XMEAS(12)  Prod Sep Pressure                     kPa gauge
    XMEAS(13)  Prod Sep Underflow (stream 9)         m3/hr
    XMEAS(14)  Stripper Level                        %
    XMEAS(15)  Stripper Pressure                     kPa gauge
    XMEAS(16)  Stripper Underflow (stream 10)        m3/hr
    XMEAS(17)  Stripper Temperature                  Deg C
    XMEAS(18)  Stripper Steam Flow                   kg/hr
    XMEAS(19)  Compressor Work                       kW
    XMEAS(20)  Reactor Cooling Water Outlet Temp     Deg C
    XMEAS(21)  Separator Cooling Water Outlet Temp   Deg C

  Sampled Process Measurements

    Reactor Feed Analysis (Stream 5)
        Sampling Frequency = 0.1 hr
        Dead Time = 0.1 hr
        Mole %

    XMEAS(22)   Component A
    XMEAS(23)   Component B
    XMEAS(24)   Component C
    XMEAS(25)   Component D
    XMEAS(26)   Component E
    XMEAS(27)   Component F

    Purge Gas Analysis (Stream 8)
        Sampling Frequency = 0.1 hr
        Dead Time = 0.1 hr
        Mole %

    XMEAS(28)   Component A
    XMEAS(29)   Component B
    XMEAS(30)   Component C
    XMEAS(31)   Component D
    XMEAS(32)   Component E
    XMEAS(33)   Component F
    XMEAS(34)   Component G
    XMEAS(35)   Component H

    Product Analysis (Stream 10)
        Sampling Frequency = 0.25 hr
        Dead Time = 0.25 hr
        Mole %

    XMEAS(36)   Component D
    XMEAS(37)   Component E
    XMEAS(38)   Component F
    XMEAS(39)   Component G
    XMEAS(40)   Component H

    Added XMEAS in C version

    XMEAS(41) is for cost [cents/kmol product]. 
    XMEAS(42) is production rate of G [kmol G generated/h] 
    XMEAS(43) is production rate of H [kmol H generated/h] 
    XMEAS(44) is production rate of F [kmol F generated/h] 
    XMEAS(45) is partial pressure of A in reactor [kPa] 
    XMEAS(46) is partial pressure of C in reactor [kPa] 
    XMEAS(47) is partial pressure of D in reactor [kPa] 
    XMEAS(48) is partial pressure of E in reactor [kPa] 
    XMEAS(49) is true (delay free) mole % G in product 
    XMEAS(50) is true (delay free) mole % H in product 

    12 Manipulated Variables - 11 valves plus one stirrer (1-indexed!)

    XMV(1)     A Feed Flow (stream 1)
    XMV(2)     D Feed Flow (stream 2)
    XMV(3)     E Feed Flow (stream 3)
    XMV(4)     A and C Feed Flow (stream 4)
    XMV(5)     Compressor Recycle Valve
    XMV(6)     Purge Valve (stream 9)
    XMV(7)     Separator Pot Liquid Flow (stream 10)
    XMV(8)     Stripper Liquid Product Flow (stream 11)
    XMV(9)     Stripper Steam Valve
    XMV(10)    Reactor Cooling Water Flow
    XMV(11)    Condenser Cooling Water Flow
    XMV(12)    Agitator Speed
"""

class PV():

    def __init__(self, state):
        self.xmeas = [None] *41
        #self.xmeas = [Sensor("A feed"), Sensor("D feed"), Sensor("E feed"), Sensor()]
        self.xmv = state[38:50]

    def update_continuous_xmeas(self, teproc):
        self.xmeas[0] = teproc.stream[0].flow_kscmh               # A feed stream 0 kscmh
        self.xmeas[1] = teproc.stream[1].flow * teproc.xmws[0] * .454     # D feed stream 1 kg/hr
        self.xmeas[2] = teproc.stream[2].flow * teproc.xmws[1] * .454     # E feed stream 2 kg/hr
        self.xmeas[3] = teproc.stream[3].flow_kscmh               # A and C stream 4 kscmh
        self.xmeas[4] = teproc.stream[8].flow_kscmh               # recycle stream 8 kscmh
        self.xmeas[5] = teproc.stream[5].flow_kscmh               # reactor feed stream 6 kscmh
        self.xmeas[6] = teproc.R.P_kPag                     # reactor P kPa gauge
        self.xmeas[7] = (teproc.R.level - 84.6) / 666.7 * 100.    # reactor level %
        self.xmeas[8] = teproc.R.T                                # reactor T deg C
        self.xmeas[9] = teproc.stream[9].flow_kscmh               # purge stream 9 kscmh
        self.xmeas[10] = teproc.S.T                               # separator T deg C
        self.xmeas[11] = (teproc.S.level - 27.5) / 290. * 100.    # separator level %
        self.xmeas[12] = teproc.S.P_kPag                           # separator P kPa gauge
        self.xmeas[13] = teproc.flow[10] / teproc.dls / 35.3145    # separator underflow stream 10 m3/hr
        self.xmeas[14] = (teproc.C.level - 78.25) / teproc.V.Volume * 100. # stripper level %.  What?!!!!
        self.xmeas[15] = teproc.C.P_kPag                          # stripper P kPa gauge
        self.xmeas[16] = teproc.flow[12] / teproc.dlc / 35.3145    # stripper underflow stream 11 m3/hr
        self.xmeas[17] = teproc.C.T                               # stripper T deg C
        self.xmeas[18] = teproc.C.underflow* 1040. * .454               # stripper flow kg/hr
        #self.xmeas[19] = teproc.cpdh * 392.7                     # compressor work kW?
        self.xmeas[19] = teproc.compressor_work * 293.07          # compressor work kW 
        self.xmeas[20] = teproc.twr                               # reactor coolant outlet T deg C
        self.xmeas[21] = teproc.tws                               # separator coolant outlet T deg C

    def update_gas(self):
        pass

    def update_product_xmeas(self):
        pass

    def check_criticals(self, teproc, dvec):
        if (self.xmeas[6] > REACTOR_P_HIGH): 
            raise ProcessException(1, "High Reactor Pressure")
        if (self.xmeas[7] / 35.3145 > REACTOR_LEVEL_HIGH): #check units
            raise ProcessException(2, "High Reactor Liquid Level")
        if (self.xmeas[7] / 35.3145 < REACTOR_LEVEL_LOW):
            raise ProcessException(3, "Low Reactor Liquid Level")
        if (self.xmeas[8] > REACTOR_T_HIGH):
            raise ProcessException(4, "High Reactor Temperature")
        if (self.xmeas[11] / 35.3145 > SEP_LEVEL_HIGH):
            raise ProcessException(5, "High Separator Liquid Level")
        if (self.xmeas[11] / 35.3145 < SEP_LEVEL_LOW):
            raise ProcessException(6, "Low Separator Liquid Level")
        if (self.xmeas[14] / 35.3145 > STRIP_LEVEL_HIGH):
            raise ProcessException(7, "High Stripper Liquid Level")
        if (self.xmeas[14] / 35.3145 < STRIP_LEVEL_LOW):
            raise ProcessException(8, "Low Stripper Liquid Level")

class ProcessException(Exception):
    #processException will need to set DVEC.isd, and print f"{msg}!! Shutting Down".
    pass

"""
    20 Process Disturbances

    IDV(0)   A/C Feed Ratio, B Composition Constant (Stream 4)          Step
    IDV(1)   B Composition, A/C Ratio Constant (Stream 4)               Step
    IDV(2)   D Feed Temperature (Stream 2)                              Step
    IDV(3)   Reactor Cooling Water Inlet Temperature                    Step
    IDV(4)   Condenser Cooling Water Inlet Temperature                  Step
    IDV(5)   A Feed Loss (Stream 1)                                     Step
    IDV(6)   C Header Pressure Loss - Reduced Availability (Stream 4)   Step
    IDV(7)   A, B, C Feed Composition (Stream 4)            Random Variation
    IDV(8)   D Feed Temperature (Stream 2)                  Random Variation
    IDV(9)  C Feed Temperature (Stream 4)                  Random Variation
    IDV(10)  Reactor Cooling Water Inlet Temperature        Random Variation
    IDV(11)  Condenser Cooling Water Inlet Temperature      Random Variation
    IDV(12)  Reaction Kinetics                                    Slow Drift
    IDV(13)  Reactor Cooling Water Valve                            Sticking    affects IVST[10]
    IDV(14)  Condenser Cooling Water Valve                          Sticking    affects IVST[11]
    IDV(15)  Unknown
    IDV(16)  Unknown
    IDV(17)  Unknown
    IDV(18)  Unknown              affects IVSTs 5,7,8,9   (Implied) Sticking
    IDV(19)  Unknown  
    IDV(21)  Cyberattack?
"""

class DVEC():
    #L500
    def __init__(self):
        self.idv = [0] * 20
        self.isd = 0 #general isError flag

    def set_IDVs_0_or_1(self):
        self.idv = [1 if i > 0 else 0 for i in self.idv] 

class TEproc():

    def __init__(self, pv):
        # self.[a-z]{3,4}\[[0-9]{1,2}\] = (\(float\))?
        self.vpos = [None] * 12
        self.vrng = [400., 400., 100., 1500.,
                     None, None, 1500., 1e3,
                     .03, 1e3, 1200., None]

        self.R = Vessel("Reactor", 1300.) #src=5, dst=6
        self.S = Vessel("Separator", 3500.) #src=6? dst=[8,9]
        self.C = Vessel("Stripper", 156.5) #src=9, dst=10
        self.V = Vessel("V", 5e3) # Condensor?

        self.rr = [0.] * 4 #? so, per vessel.  Reaction rate???
        self.crxr = {"A": None, "B": None, "C": None, "D": None, 
                     "E": None, "F": None, "G": None, "H": None}

        self.htr = [.06899381054, .05]

        self.hwr = 7060.
        self.hws = 11138.

        self.sfr = [.995, .991, .99, .916, 
                    .936, .938, .058, .0301]

        self.stream = {"A": Stream("A", 5, "gas", [.9999, 1e-4, 0., 0., 0., 0., 0., 0.], 45.), 
                       "D": Stream("D", 5, "gas", [0., 1e-4, 0., .9999, 0., 0., 0., 0.], 45.), 
                       "E": Stream("E", 5, "gas", [0., 0., 0., 0., .9999, 1e-4, 0., 0.], 45.), 
                       "C": Stream("C", "Stripper", "gas", [.485, .005, .51, 0., 0., 0., 0., 0.], 45.), #3
                       "Strp": Stream("Stripper", 7, "gas"), #4
                       "R in": Stream(["A","D","E",7], "Reactor", "gas"), #5
                       "R out": Stream("Reactor","Condensor","gas"), #6
                       "Recirc": Stream([4,"Compressor"], 5, "gas"), #7
                       "Purge": Stream("Separator", "Purge", "gas"), #8
                       "S out": Stream("Separator", "Stripper", "liquid"), #9
                       "Product": Stream("Stripper", "Product", "liquid"), #10
                       } #11, 12 removed as coolant

        self.cpflmx = 280275.
        self.cpprmx = 1.3

        #L300
        self.vtau = [i / 3600 for i in [8., 8., 6., 9., 
                                        7., 5., 5., 5., 
                                        120., 5., 5., 5.]]

        self.xns = [.0012, 18., 22., .05, .2, .21, .3, .5,
                    .01, .0017, .01, 1., .3, .125, 1., .3,
                    .115, .01, 1.15, .2, .01, .01, .25, .1, 
                    .25, .1, .25, .025, .25, .1, .25, .1,
                    .25, .025, .05, .05, .01, .01, .01, .5, 
                    .5] #41, so... xmeas property?
        self.xdel = [None] * 41
        self.xmws = [None] * 13 #make stream property

        self.vcv = pv.xmv
        self.vst = [2.] * 12
        self.ivst = [0] * 12
        self.stream_include = (0,1,2,3,5,7,8,9,10,12) #makes no sense

    def set_reaction_rates(self, r1f, r2f):
        #Arrhenius variant.  should be A*exp(-E_act/RT)
        self.rr = [exp(31.5859536 - 20130.85052843482 / self.R.T_K) * r1f, #A(g) + C(g) + D(g) -> G(l
                   exp(3.00094014 - 10065.42526421741 / self.R.T_K) * r2f, #A(g) + D(g) + E(g) -> H(l)
                   exp(53.4060443 - 30196.27579265224 / self.R.T_K),       #A(g) + E(g) -> F(l)
                   self.rr[2] * .767488334]                                #3D(g) -> 2F(l)

        if (self.R.pp[0] > 0. and self.R.pp[2] > 0.): #so, usually.
            r1f = self.R.pp[0] ** constants.b73
            r2f = self.R.pp[2] ** constants.b74
            self.rr[0] *=  r1f * r2f * self.R.pp[3]
            self.rr[1] *=  r1f * r2f * self.R.pp[4]
        else:
            self.rr[0] = 0.
            self.rr[1] = 0.

        self.rr[2] *=  self.R.pp[0] * self.R.pp[4]
        self.rr[3] *=  self.R.pp[0] * self.R.pp[3]

        for i in range(len(self.rr)):
            self.rr[i] *= self.R.vv

    def set_crxrs(self):
        #why no B? Why D consumption not affected by rr[1]?
        self.crxr["A"] = -self.rr[0] - self.rr[1] - self.rr[2] # was [0]. A consumed by [0,1,2]
        self.crxr["C"] = -self.rr[0] - self.rr[1] # was [2].  D consumed by [0,1]
        self.crxr["D"] = -self.rr[0] - self.rr[3] * 1.5 # was [2].  Should include rr[1]?
        self.crxr["E"] = -self.rr[1] - self.rr[2] # E consumed by [1,2]
        self.crxr["F"] = self.rr[2] + self.rr[3] # was [5]
        self.crxr["G"] = self.rr[0]
        self.crxr["H"] = self.rr[1]

    def update_fractional_flows(self):
        for s in self.stream_include:
            self.stream[s].set_fractional_flow()

class WLK():

    def __init__(self):
        self.hspan = [.2, .7, .25, .7, 
                      .15, .15, 1., 1.,
                      .4, 1.5, 2., 1.5]
        self.hzero = [.5, 1., .5, 1., 
                      .25, .25, 2., 2., 
                      .5, 2., 3., 2.]
        self.sspan = [.03, .003, 10.,10., 
                      10., 10., .25, .25, 
                      .25, 0., 0., 0.]
        self.szero = [.485, .005, 45., 45., 
                      35., 40., 1., 1., 
                      0., 0., 0., 0.]
        self.spspan = [0.] * 12
        self.tlast = [0.] * 12
        self.tnext = [.1] * 12
        #L550
        self.idvwlk = np.zeros(12)
        self.reset()

    def reset(self):
        #do we need this method at all?
        self.adist = self.szero
        self.bdist = [0.] * 12
        self.cdist = [0.] * 12
        self.ddist = [0.] * 12
        #self.tlast = [0.] * 12
        #self.tnext = [.1] * 12

    def set_idvwlk(self, dvec):
        self.idvwlk[0] = self.idvwlk[1] = dvec.idv[7]
        self.idvwlk[2] = dvec.idv[8]
        self.idvwlk[3] = dvec.idv[9]
        self.idvwlk[4] = dvec.idv[10]
        self.idvwlk[5] = dvec.idv[11]
        self.idvwlk[6] = self.idvwlk[7] = dvec.idv[12]
        self.idvwlk[8] = dvec.idv[15]
        self.idvwlk[9] = dvec.idv[16]
        self.idvwlk[10] = dvec.idv[17]
        self.idvwlk[11] = dvec.idv[19]

    def get_h_s_spwlk(self, i):
        hwlk = self.tnext[i] - self.tlast[i]
        swlk = (self.adist[i] 
                + hwlk * (self.bdist[i] + hwlk 
                          * (self.cdist[i] + hwlk * self.ddist[i])))
        spwlk = (self.bdist[i] 
                 + hwlk * (self.cdist[i] * 2. 
                            + hwlk * 3. * self.ddist[i]))
        return (hwlk, swlk, spwlk)
        
    def sub5(self, i, s, sp):
        idvflag = self.idvwlk[i]
        h = self.hspan[i] * tesub7(i) + self.hzero[i]
        s1 = self.sspan[i] * tesub7(i) * idvflag + self.szero[i]
        s1p = self.spspan[i] * tesub7(i) * idvflag
        self.adist[i] = s
        self.bdist[i] = sp
        # Computing 2nd power 
        self.cdist[i] = ((s1 - s) * 3 - h * (s1p + sp * 2)) / (h**2)
        # Computing 3rd power 
        self.ddist[i] = ((s - s1) * 2 + h * (s1p + sp)) / (h**3)
        self.tnext[i] = self.tlast[i] + h

    # called as (1=i, time=t)
    def sub8(self, i, t):
        h = t - self.tlast[i]
        return (self.adist[i] 
                + h * (self.bdist[i] 
                    + h * (self.cdist[i] + h * self.ddist[i])))
###############################################################################
#  new classes

class Sensor():
    pass

class Coolant():
    pass

"""
13 Streams:

    0 -> A input to 5                  / 0/     A feed
    1 -> D input to 5                  / 1/     D feed
    2 -> E input to 5                  / 2/     E feed
    3 -> C input (to stripper)         / ?/     
    4 -> stripper to recirc            / -/     Stripper output
    5 -> A + D + E + recirc to reactor / /      V output.  Wrong?!
    6 -> reactor to condensor          / /      linked to 5.  Wrong?!
    7 -> recirc + compressor to 5      / /      Reactor output.  Wrong?!
    8 -> separator to purge            / /      Seperator, Recycle
    9 -> separator to stripper (l)     / /      Sep
    10 -> stripper to product  (l)     / /      Sep
    11 -> reactor coolant out          / /      Stripper output
    12 -> condenser coolant out        / /      Stripper output

"""

class Stream():

    def __init__(self, src, dst, state, component_fractions = None, T = None):
        self.source = src
        self.sink = dst
        self.state = state #(liquid or gas)
        self.flow = None #was ftm,
        self.H = None
        self.T = T #was tst, units degC
        if component_fractions is None:
            component_fractions = [None] * 8
        self.x = component_fractions #unitless
        #self.flow_frac = [None] * 8 #@property?

    @property
    def T_K(self):
        return self.T + 273.15 

    #called as &self.stream[1]=z, &self.tst[1]=t, &self.stream[1]=h, &1=ity
    def set_heat(self, ity): #was sub1
        if (ity == 0):
            self.H = sum(self.T 
                         * constants.xmw[i] 
                         * (self.T * (constants.ah[i] + constants.bh[i] * self.T / 2 
                                     + constants.ch[i] * (self.T**2) / 3) 
                                     * 1.8) 
                        for i in range(8))
            #L100
        else:
            self.H = sum(self.T 
                         * constants.xmw[i]
                         * ((self.T * (constants.ag[i] + constants.bg[i] * self.T / 2 
                            + constants.cg[i] * (self.T**2) / 3) * 1.8) + constants.av[i])
                         for i in range(8))
                #L200
        if (ity == 2):
            self.H -= 3.57696e-6 * self.T_K

    @property
    def flow_frac(self): #was fcm, units same as flow
        return self.x * self.flow

    @property
    def flow_kscmh(self):
        return self.flow * .359 / 35.3145 #1m3 = 35.3145 ft3

class Vessel():

    """ 
    A note on name schemes:

    [type][units][vessel]

    e.g. TCR = "temp, Celsius, Reactor"

    First char:

    F = Flow
    P = Pressure
    T = Temp
    V = Volume
    U = Molar Amounts
    X = Concentration

    Second char:

    C = Celsius
    K = Kelvin

    First two chars:

    vl = Level
    qu = underflow

    Last character:

    C = Stripper, R = Reactor, S = Separator, V = ???

    """

    def __init__(self, name, V):
        self.name = name
        self.dl = None
        self.et = None       # primitive
        self.pp = [None] * 8       # partial pressure
        self.underflow = None       # underflow related
        #TODO: where is T initialised? Not clear in Fortran code
        self.T = None        # temp in degC
        self.Volume = V      # total
        self.ucl = [None] * 8
        self.ucv = [None] * 8 # V only?

    def load(self, state):
        if self.name == "Reactor":
            self.ucv = state[1:4] 
            self.ucl = state[4:9]
            self.et = state[9]
        elif self.name == "Separator":
            self.ucv = state[10:13]
            self.ucl = state[13:18]
            self.et = state[18]
        elif self.name == "Stripper":
            self.ucl = state[19:27]
            self.et = state[27]
        elif self.name == "V":
            self.ucv = state[28:36]
            self.et = state[36]

    @property
    def es(self):
        return (self.et / self.utv if self.name == "V" else self.et / self.utl)

    @property
    def level(self):
        return self.utl / self.dl

    @property
    def P(self):
        return (self.utv * constants.rg * self.T_K / self.Volume if self.name == "V" 
                 else sum(self.pp))

    @property
    def P_kPag(self): #check operator precendece
        return (self.P  - 760) / 760 * 101.325

    @property
    def T_K(self): #returns T in K
        return self.T + 273.15

    @property
    def utl(self):
        return sum(self.ucl)

    @property
    def utv(self): #only used for V?
        return (sum(self.ucv) if self.name == "V"
                else self.P * self.vv * constants.rg / self.T_K)

    @property
    def vv(self):
        return self.Volume - self.level

    @property
    def xl(self):
        return [x / self.utl for x in self.ucl]

    @property
    def xv(self):
        return [p / self.P for p in self.pp]

    #both of these methods only associated with sub3.  Move back in?
    def get_htest(self, ity): #was sub3
        if (ity == 0):
            return sum(self.T 
                        * constants.xmw[i] 
                        * (self.T * (constants.ah[i] + constants.bh[i] * self.T / 2 
                                    + constants.ch[i] * (self.T**2) / 3) * 1.8) 
                        for i in range(8))
        else:
            return sum(self.T 
                        * constants.xmw[i]
                        * ((self.T * (constants.ag[i] + constants.bg[i] * self.T / 2 
                            + constants.cg[i] * (self.T**2) / 3) * 1.8) + constants.av[i])
                        for i in range(8))
        if (ity == 2):
            # WOAH what's this weirdness
            return  - (3.57696e-6 * self.T_K)

    def get_dh(self, dh, ity):
            if (ity == 0):
                return sum(self.xl[i] 
                        * constants.xmw[i]
                        * (1.8 * (constants.ah[i] 
                                + constants.bh[i] * self.T 
                                + constants.ch[i] * (self.T ** 2)))
                        for i in range(8))
            #L100
            elif (ity != 2): #check these switches with returns
                return sum(self.xl[i] 
                        * constants.xmw[i]
                        * (1.8 * (constants.ag[i] 
                                + constants.bg[i] * self.T 
                                + constants.cg[i] * (self.T ** 2))) 
                        for i in range(8))
            #L200
            if (ity == 2):
                return dh - 3.57696e-6

    # called with args xl[c|r|s|v]=z, tc[x]=*t, es[x]=*h, ity).
    # V ONLY calls with ity==2
    def sub2(self, ity=0):
        #mutates self.T.  INCLUDING when initialised with T=0
        tin = self.T
        dh = 0
        #L250
        for _ in range(100): #needs a Stream() to connect to
            htest = self.get_htest(ity) #sub1
            err = htest - self.es #recall we think es is heat!
            dh = self.get_dh(dh, ity) #sub3
            dt = -err / dh
            #mutate T
            self.T += dt
            if (abs(dt) < 1e-12):
                break #L300
        # this makes no sense.  Spend all that time mutating T, then just reassign it?
        self.T = tin

    # called as: (self.xl[rcsv]=x, &self.R.T=t, &self.dlr=r)
    def sub4(self):
        #L10
        v = sum(self.xl[i] * constants.xmw[i] 
                / (constants.ad[i] + (constants.bd[i] + constants.cd[i] * self.T)
                * self.T) for i in range(8))
        self.dl = 1 / v #r__

###############################################################################
# subfuncs

# called as (&self.xns[i]=*std, &xmns = *x)
def tesub6(xn):
    return (sum(tesub7(i) for i in range(12)) - 6) * xn

# called as (self.dvec.isd=i) OR with raw ints.  
# TODO: Global randsd is mutated! consider chucking this whole method for random()
def tesub7(i):
    global randsd
    randsd = (randsd * 9228907.) % constants.b78
    return (randsd / constants.b78 if i >= 0 
            else randsd * 2 / constants.b78 - 1)


if __name__ == "__main__":
    te = TEprob()
    te.run()