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

Notes on Fortran to Python conversion:

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
13) ditto delta_xr, XMWs
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


    50 State vector

#   |0|,
#   |?|,

#   |1 --- 3||4 --- 8||  9 ||10 - 12||13 - 17|| 18 ||19 - 26|| 27 ||28 - 35|| 36 |,
#   | R.ucv || R.ucl ||R.et|| S.ucv || S.ucl ||S.et|| C.ucl ||C.et|| V.ucv ||V.et|,

#   | 37|| 38|| 39-50|,
#   |twr||tws|| vpos |,

So: [R, C, S].ucv, ucl, et, twr, tws, vpos are INHERENT. All others can be derived?
"""

import numpy as np
from math import exp, sqrt
from random import random
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

        #L200 
        #self.dvec = DVEC()   
        self.time = 0.
        self.delta_t = 1. / 3600.0
        self.R, self.S, self.C, self.V = (Reactor(self.state[0:9],
                                                  Coolant(self.state[36], 7060.), 1300.), 
                                          Separator(self.state[9:18],
                                                  Coolant(self.state[37], 11138.), 3500.), 
                                          C(self.state[18:27], 256.5), 
                                          V(self.state[27:36], 5000.))
        self.streams = ( FeedStream([.9999, 1e-4, 0., 0., 0., 0., 0., 0.], 45.), 
                         FeedStream([0., 1e-4, 0., .9999, 0., 0., 0., 0.], 45.), 
                         FeedStream([0., 0., 0., 0., .9999, 1e-4, 0., 0.], 45.), 
                         FeedStream(),
                         Stream([]),
                         PurgeStream(),
                         LiquidStream(),
                         LiquidStream(),
                         LiquidStream())
        self.sfr = [0.995, 0.991, 0.99, 0.916, 0.936, 0.938, 0.058, 0.0301]

        self.compressor = Compressor(280275, 1.3)
        self.vpos = [None] * 12
        self.vrng = [400.0, 400.0, 100.0, 1500.0, 
                    None, None, 1500.0, 1500.0,
                    0.03, 1000.0, 1200.0, None]
        self.vtau = [i / 3600. for i in [8., 8., 6., 9., 
                                         7., 5., 5., 5., 
                                         120., 5., 5., 5.]] #L300
        
        self.pv = PV()
        
        self.rr = [0.] * 4 #reaction rates.
        self.htr = [.06899381054, .05] #reaction heats
        self.delta_xr = {"A": None, "B": None, "C": None, "D": None, 
                     "E": None, "F": None, "G": None, "H": None}

    def run(self):
        #locals: hwlk, vovrl, vpr, tmpfac, r1f, r2f, fin[8]
        #xcmp = [None] * 41 #?
        #L500, L900, L910, L950 gone
        #stream num and IDV matches
        self.streams[3].x[0] = (random()
                                           - self.dvec.idv[0] * .03        #A/C feed
                                           - self.dvec.idv[1] * .00243719) #B composition
        self.streams[3].x[1] = self.wlk.sub8(2, self.time) + self.dvec.idv[1] * .005
        self.streams[3].x[2] = (1 - (self.streams[3].x[0] 
                                                + self.streams[3].x[1]))   #xC = 1 - xA +xB

        #ambiguous if A or D.  Also sub8 is weird.  Also, D and A feed messed up.
        self.streams[0].T = self.wlk.sub8(3, self.time) + self.dvec.idv[2] * 5 #D feed.  Yes, it is Temp.
        self.streams[3].T = self.wlk.sub8(4, self.time)

        self.R.coolant.T_in = self.wlk.sub8(5, self.time) + self.dvec.idv[3] * 5. #Reactor coolant
        self.S.coolant.T_in = self.wlk.sub8(6, self.time) + self.dvec.idv[4] * 5. #Condensor coolant

        r1f = self.wlk.sub8(7, self.time)
        r2f = self.wlk.sub8(8, self.time)

        #L1010
        self.C.load(self.state)
        self.R.load(self.state)
        self.S.load(self.state)
        self.V.load(self.state)

        self.R.coolant.T_out = self.state[37]
        self.S.coolant.T_out = self.state[38]

        #L1035
        for i in range(0, 12):
            self.vpos[i] = self.state[i + 38]

        #L1040 abstracted into earlier load() method
        #L1050 (conc. calculations, or xl) abstracted into Vessel property
        #es calculation abstracted into property methods

        self.R.set_vessel_temperature()
        self.S.set_vessel_temperature()
        self.C.set_vessel_temperature()
        self.V.set_vessel_temperature()
        self.R.set_vessel_density()
        self.S.set_vessel_density()
        self.C.set_vessel_density()
        self.R.set_pressure()
        self.S.set_pressure()

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
        self.set_delta_xrs()
        self.rh = (self.rr[0] * self.htr[0] + self.rr[1] * self.htr[1])

        #self.xmws zeroing removed.
        #why is recirc stream set to same properties as S only?
        #L2010
        #was (5,7,8,9,10,12)
        self.streams[5].x = self.V.xv
        self.streams[7].x = self.R.xv
        self.streams[8].x = self.S.xv
        self.streams[9].x = self.S.xv
        self.streams[10].x = self.S.xl
        self.streams[12].x = self.C.xl

        for i in (0,1,5,7,8,9): #why just these
            self.xmws[i] = sum(x * xmw for x, xmw in 
                                      zip(self.streams[i].x, constants.xmw))

        self.streams[5].T = self.V.T
        self.streams[7].T = self.R.T
        self.streams[8].T = self.S.T
        self.streams[9].T = self.S.T
        self.streams[10].T = self.S.T 
        self.streams[12].T = self.C.T

        for i in (0, 1, 2, 3, 5, 7, 8):
            self.streams[i].set_heat()
        self.streams[9].H = self.streams[8].H 
        self.streams[10].set_heat() 
        self.streams[12].set_heat() 

        #2,0,1,3,10,12
        self.streams[0].set_flow(self.vpos[0], self.vrng[0])
        self.streams[1].set_flow(self.vpos[1],  self.vrng[1])
        self.streams[2].set_flow(self.vpos[2], self.vrng[2]) # (1. - self.dvec.idv[5]) # A Feed Loss (Stream 1) 
        self.streams[3].set_flow(self.vpos[3], self.vrng[3]) #C Header Pressure Loss - Reduced Availability (Stream 4)
        self.streams[10].set_flow(self.vpos[6], self.vrng[6])
        self.streams[12].set_flow(self.vpos[7], self.vrng[7])

        self.R,coolant.flow = self.vpos[9] * self.vrng[9] / 100.
        self.S.coolant.flow = self.vpos[10] * self.vrng[10] / 100.
        self.R.agitator_speed = (self.vpos[11] + 150.) / 100.

        # calculating flow from V to Reactor, as stream 5
        self.streams[5].set_flow(self.V, self.R, 1937.6)
        self.streams[7].set_flow(self,R, self.S, 4574.21)   #plus random_dist
        self.streams[9].set_flow(self.S, 0.151169)          #discharges to atmosphere, mult flcoef by vpos[5]
        self.compressor.set_work(self.S, self.V, self.xmws)
        self.streams[8].set_flow(self.V, self.S, 53.349)    #plus max thing, check fortran
        self.streams[8].H += self.compressor.work / self.streams[8].flow
        #L5020 - as property?
        #self.update_fractional_flows()
        
        if (self.streams[10].flow > .1):
            if (self.C.T > 170.):
                tmpfac = self.C.T - 120.262
            elif (self.C.T < 5.292):
                tmpfac = .1
            else:
                tmpfac = (363.744 / (177. - self.C.T) 
                          - 2.22579488)
            #vovrl = stripper flow mass balance ()
            vovrl = self.streams["C"].flow / self.streams["Product"].flow * tmpfac
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
        fin = self.streams[3].flow_frac + self.streams[10].flow_frac

        #L6020
        self.streams[4].flow_frac = [i * j for i, j in zip(fin, self.sfr)]
        self.streams[11].flow_frac = [i - j  for i, j in zip(fin, self.streams[4].flow_frac)]
        self.streams[4].flow = sum(self.streams[4].flow_frac)
        self.streams[11].flow = sum(self.streams[11].flow_frac)

        #L6030
        for i in range(8):
            self.streams[4].x[i] = self.streams[4].flow_frac[i] / self.streams[4].flow
            self.streams[11].x[i] = self.streams[11].flow_frac[i] / self.streams[11].flow

        self.streams[3].T = self.C.T
        self.streams[10].T = self.C.T
        self.streams[3].set_heat() #why again?
        self.streams[10].set_heat()
        self.streams[6].flow = self.streams[5].flow
        self.streams[6].H = self.streams[5].H
        self.streams[6].T = self.streams[5].T

        #L6130
        self.streams[6].x = self.streams[5].x
        self.streams[6].flow_frac = self.streams[5].flow_frac

        self.R.set_heat_transfer()
        self.S.set_heat_transfer()
        self.C.set_heat_transfer()

        self.dvec.isd = 0
        self.pv.update_continuous_xmeas(self)
        self.check_criticals()

        if (self.time > 0. and self.dvec.isd == 0):
            #L6500
            for i, xn in enumerate(self.xns): #1-22 inclusive
                self.pv.xmeas[i] += tesub6(xn) #wtf?

        # Temp store for sampled process measurements
        # reactor (stream 6) mol%
        xcmp[22:28] = [i * 100 for i in self.streams[6].x[0:6]]
        # purge (stream 9) mol%
        xcmp[28:36] = [i * 100 for i in self.streams[8].x]
        # product (stream 10) mol%.  Fixed off-by-TWO error
        xcmp[36:] = [i * 100 for i in self.streams[10].x[3:]] 

        if (self.time == 0.): #init?
            #L7010
            self.pv.update_sampled_xmeas(xcmp)
            for i in range(22, 41): # was 23
                #WHY?????
                self.xdel[i] = xcmp[i]
                self.pv.xmeas[i] = xcmp[i]
            self.tgas = .1
            self.tprod = .25

        if (self.time >= self.tgas):
            #L7020
            #reactor feed and purge analysis
            #self.update_sampled_xmeas()
            #self.update_gas_xmeas()
            for i in range(22, 36):
                self.pv.xmeas[i] = self.xdel[i] + tesub6(self.xns[i])
                self.xdel[i] = xcmp[i]
            self.tgas += .1

        if (self.time >= self.tprod):
            #L7030
            #self.update_product_xmeas()
            #self.update_sampled_xmeas()
            #product analysis
            for i in range(36, 41):
                self.pv.xmeas[i] = self.xdel[i] + tesub6(self.xns[i])
                self.xdel[i] = xcmp[i]
            self.tprod += .25

        #L9010
        for i in range(8):
            self.derivatives[i] = (self.streams[6].flow_frac[i] 
                     - self.streams[7].flow_frac[i]
                     + self.delta_xr[i])
            self.derivatives[i + 9] = (self.streams[7].flow_frac[i] - self.streams[8].flow_frac[i] 
                         - self.streams[9].flow_frac[i] - self.streams[10].flow_frac[i])
            self.derivatives[i + 18] = self.streams[11].flow_frac[i] - self.streams[12].flow_frac[i]
            self.derivatives[i + 27] = (self.streams[0].flow_frac[i] + self.streams[1].flow_frac[i]  
                          + self.streams[2].flow_frac[i] + self.streams[4].flow_frac[i]  
                          + self.streams[8].flow_frac[i] - self.streams[5].flow_frac[i])

        self.derivatives[9] = (self.streams[6].H * self.streams[6].flow 
                         - self.streams[7].H * self.streams[7].flow 
                         + self.rh + self.R.underflow)
        #     Here is the "correct" version of the separator energy balance:
        #     Original off-by-two.
        self.derivatives[18] = (self.streams[6].H * self.streams[6].ftm
                                - (self.streams[7].H * self.streams[7].ftm - self.compressor.work)
                                - self.streams[8].H * self.streams[8].ftm
                                - self.streams[9].H * self.streams[9].ftm
                                - self.S.underflow)
        self.derivatives[27] = (self.streams[3].H 
                  * self.streams[3].flow + self.streams[10].H 
                  * self.streams[10].flow - self.streams[4].H 
                  * self.streams[4].flow - self.streams[12].H 
                  * self.streams[12].flow + self.C.underflow)
        self.derivatives[36] = (self.streams[0].H
                  * self.streams[0].flow + self.streams[1].H 
                  * self.streams[1].flow + self.streams[2].H 
                  * self.streams[2].flow + self.streams[4].H 
                  * self.streams[4].flow + self.streams[8].H 
                  * self.streams[8].flow - self.streams[5].H 
                  * self.streams[5].flow)
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
            self.derivatives = [0.] * len(self.state)

# end (run)
####################################################################################################

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

    def set_delta_xrs(self):
        #why no B? Why D consumption not affected by rr[1]?
        self.delta_xr["A"] = -self.rr[0] - self.rr[1] - self.rr[2] # was [0]. A consumed by [0,1,2]
        self.delta_xr["C"] = -self.rr[0] - self.rr[1] # was [2].  D consumed by [0,1]
        self.delta_xr["D"] = -self.rr[0] - self.rr[3] * 1.5 # was [2].  Should include rr[1]?
        self.delta_xr["E"] = -self.rr[1] - self.rr[2] # E consumed by [1,2]
        self.delta_xr["F"] = self.rr[2] + self.rr[3] # was [5]
        self.delta_xr["G"] = self.rr[0]
        self.delta_xr["H"] = self.rr[1]

    def check_criticals(self):
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

class PV:

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

    def __init__(self):
        self.xmeas = [None] * 41
        self.xmv = [None] * 12

    def update_continuous_xmeas(self):
        self.xmeas[0] = self.streams[0].flow_kscmh              # A feed stream 0 kscmh
        self.xmeas[1] = self.streams[1].flow * self.xmws[0] * .454     # D feed stream 1 kg/hr
        self.xmeas[2] = self.streams[2].flow * self.xmws[1] * .454   # E feed stream 2 kg/hr
        self.xmeas[3] = self.streams[3].flow_kscmh              # A and C stream 4 kscmh
        self.xmeas[4] = self.streams[8].flow_kscmh              # recycle stream 8 kscmh
        self.xmeas[5] = self.streams[5].flow_kscmh              # reactor feed stream 6 kscmh
        self.xmeas[6] = self.R.P_kPag                           # reactor P kPa gauge
        self.xmeas[7] = (self.R.level - 84.6) / 666.7 * 100.    # reactor level %
        self.xmeas[8] = self.R.T                                # reactor T deg C
        self.xmeas[9] = self.streams[9].flow_kscmh              # purge stream 9 kscmh
        self.xmeas[10] = self.S.T                               # separator T deg C
        self.xmeas[11] = (self.S.level - 27.5) / 290. * 100.    # separator level %
        self.xmeas[12] = self.S.P_kPag                          # separator P kPa gauge
        self.xmeas[13] = self.flow[10] / self.dls / 35.3145     # separator underflow stream 10 m3/hr
        self.xmeas[14] = (self.C.level - 78.25) / self.V.Volume * 100. # stripper level %.  What?!!!!
        self.xmeas[15] = self.C.P_kPag                          # stripper P kPa gauge
        self.xmeas[16] = self.flow[12] / self.dlc / 35.3145     # stripper underflow stream 11 m3/hr
        self.xmeas[17] = self.C.T                               # stripper T deg C
        self.xmeas[18] = self.C.underflow* 1040. * .454         # stripper flow kg/hr
        self.xmeas[19] = self.compressor.work * 293.07          # compressor work kWh
        self.xmeas[20] = self.twr                               # reactor coolant outlet T deg C
        self.xmeas[21] = self.tws                               # separator coolant outlet T deg C

    def update_gas_xmeas(self):
        pass

    def update_product_xmeas(self):
        pass

class ProcessException(Exception):
    #processException will need to set DVEC.isd, and print f"{msg}!! Shutting Down".
    pass

class DVEC():
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
    #L500
    def __init__(self):
        self.idv = [0] * 20
        self.isd = 0 #general isError flag

####################################################################################################

#     def __init__(self, pv):
        # self.[a-z]{3,4}\[[0-9]{1,2}\] = (\(float\))?

    def update_fractional_flows(self):
        for s in self.streams_include:
            self.streams[s].set_fractional_flow()

class Sensor():
    pass

class Coolant():
    pass

####################################################################################################

"""
13 Streams:

    R -> 7 -> S -> 8 ----------------------------> |
    |         | -> 10 ->  |                        |
    |         |     3 ->  C -> 4 ----------------> V <- 0, 1, 2
    |         |           | -> 11 -> C             |
    |         |           | -> 12 -> product       |
    |         -> 9 -> purge                        |
    <- 6 <------------------------------------- 5 <-

C is more like:

    11 -> C -> 12

     4 -> |
    11 -> fin -> 12
          fin * sfr -> 5

TODO: check these

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
class Stream:

    def __init__(self, component_fractions = None, T = None):
        #self.source = src
        #self.sink = dst
        #self.state = state #(liquid or gas)
        self.flow = None #was ftm,
        self.H = None
        self.T = T #was tst, units degC
        if component_fractions is None:
            component_fractions = [None] * 8
        self.x = component_fractions #unitless
        #self.flow_frac = [None] * 8 #@property?

    #called as &self.streams[1]=z, &self.tst[1]=t, &self.streams[1]=h, &1=ity
    #for streams 0,1,2,3,4,5,7,8, ITY = 1. For streams 10, 11, 12 ITY = 0 (liquid?)
    def set_heat(self, ity): #was sub1
        #L100
        self.H = sum(self.T 
                        * constants.xmw[i]
                        * ((self.T * (constants.ag[i] + constants.bg[i] * self.T / 2 
                        + constants.cg[i] * (self.T**2) / 3) * 1.8) + constants.av[i])
                        for i in range(8))

    def set_flow(self, src, dst, flcoef):
        dlp = max(self.src.P - self.dst.P, 0)
        flms = sqrt(dlp) * flcoef
        self.flow = flms / self.xmws

    @property
    def T_K(self):
        return self.T + 273.15

    @property
    def flow_frac(self): #was fcm, units same as flow
        return self.x * self.flow

    @property
    def flow_kscmh(self):
        return self.flow * .359 / 35.3145 #1m3 = 35.3145 ft3

class FeedStream(Stream):

    #CAN accept teprob as argument
    def set_flow(self, vpos, vrng):
        self.flow = vpos * vrng / 100

class LiquidStream(Stream):

    def set_heat(self):
        self.H = sum(self.T 
                         * constants.xmw[i] 
                         * (self.T * (constants.ah[i] + constants.bh[i] * self.T / 2 
                                     + constants.ch[i] * (self.T**2) / 3) 
                                     * 1.8) 
                        for i in range(8))

class PurgeStream(Stream):

    def set_flow(self, src, flcoef): #dst is atmosphere
        dlp = max(self.src.P - self.dst.P, 0)
        flms = sqrt(dlp) * flcoef
        self.flow = flms / self.xmws

####################################################################################################
class Compressor:

    def __init__(self, max_flow, max_PR):
        self.max_flow = max_flow
        self.max_PR = max_PR

    def set_work(self, S, V, xmws):
        pr = min(max(1, self.V.P / self.S.P), self.max_PR)
        flcoef = self.max_flow / 1.197 
        flow = self.max_flow + flcoef * (1 - pr * (pr**2))
        self.work = (flow * S.T_K * 1.8e-6 * 1.9872 
                            * (V.P - S.P) 
                            / (xmws[8] * S.P))

class Coolant:

    def __init__(self, T_out, hw):
        # self.T_in = 
        self.T_out = T_out
        # self.flow = 
        self.hw = hw

###############################################################################

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

    @property
    def es(self):
        return self.et / self.utl

    @property
    def level(self):
        return self.utl / self.dl

    @property
    def P(self):
        return sum(self.pp)

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
        return self.P * self.vv * constants.rg / self.T_K

    @property
    def vv(self):
        return self.Volume - self.level

    @property
    def xl(self):
        return [x / self.utl for x in self.ucl]

    @property
    def xv(self):
        return [p / self.P for p in self.pp]

    # was tesub1.
    def calc_vessel_heat(self):
        return sum(self.T 
                    * constants.xmw[i] 
                    * (self.T * (constants.ah[i] + constants.bh[i] * self.T / 2 
                                + constants.ch[i] * (self.T**2) / 3) * 1.8) 
                    for i in range(8))

    # was tesub3.
    def calc_delta_h(self, dh):
        return sum(self.xl[i] 
                * constants.xmw[i]
                * (1.8 * (constants.ah[i] 
                        + constants.bh[i] * self.T 
                        + constants.ch[i] * (self.T ** 2)))
                for i in range(8))

    # was tesub2.
    def set_vessel_temperature(self):
        #mutates self.T, INCLUDING when initialised with T=0
        T_in = self.T
        converged = False
        #L250
        for _ in range(100): #needs a Stream() to connect to
            htest = self.calc_vessel_heat() #sub1
            err = htest - self.es #recall we think es is heat!
            dh = self.calc_delta_h(dh)
            dt = -err / dh
            #mutate T
            self.T += dt
            if (abs(dt) < 1e-12):
                converged = True
                break #L300
        if not (converged):
            self.T = T_in

    # was tesub4. called as: (self.xl[rcsv]=x, &self.R.T=t, &self.dlr=r)
    def set_vessel_density(self):
        #L10
        v = sum(self.xl[i] * constants.xmw[i] 
                / (constants.ad[i] + (constants.bd[i] + constants.cd[i] * self.T)
                * self.T) for i in range(8))
        self.dl = 1 / v #r__

class Reactor(Vessel):

    def __init__(self, state, coolant, vt):
        self.ucv = state[0:4]
        self.ucl = state[4:8]
        self.et = state[8]
        self.vt = vt
        self.coolant = coolant

    def set_pressure(self):
        pass

    def set_heat_transfer(self):
        if (self.level / 7.8 > 50.):
            uarlev = 1.
        elif (self.level / 7.8 < 10.):
            uarlev = 0.
        else:
            uarlev = self.level * .025 / 7.8 - .25
        uar = (uarlev * (self.agitator_speed**2 * -0.5 
                         + self.agitator_speed * 2.75 - 2.5) 
                      * .85549)
        self.qu = (uar * (self.T_out - self.T) 
                       * (1 - self.wlk.sub8(10, self.time) * .35))

class Separator(Vessel):

    def __init__(self, state, coolant, vt):
        self.ucv = state[0:4]
        self.ucl = state[4:8]
        self.et = state[8]
        self.vt = vt

    def set_heat_transfer(self, streams):
         uas = .404655 * (1 - 1 / ((streams[7].flow / 3528.73)**4 + 1)) 
         self.qu = (uas * (self.T_out - self.streams[7].T) 
                        * (1 - self.wlk.sub8(11, self.time) * .25))

class C(Vessel):

    def __init__(self, state, vt):
        self.ucl = state[0:8]
        self.et = state[8]
        self.vt = vt

    def set_heat_transfer(self):
        uac = (self.vpos[8] * self.vrng[8] * (self.wlk.sub8(9, self.time) + 1.) / 100.)
        self.qu = (uac * (100. - self.C.T) if self.C.T < 100 else 0)

class V(Vessel):

    def __init__(self, state, vt):
        self.ucv = state[0:8]
        self.et = state[8]
        self.vt = vt

    @property
    def es(self):
        return self.et / self.utv

    @property
    def P(self):
        return self.utv * constants.rg * self.T_K / self.Volume

    @property
    def utv(self):
        return sum(self.ucv)

    def calc_vessel_heat(self):
        return -(3.57696e-6 * self.T_K)

    def calc_delta_h(self):
        return sum(self.xv[i] 
                * constants.xmw[i]
                * (1.8 * (constants.ag[i] 
                        + constants.bg[i] * self.T 
                        + constants.cg[i] * (self.T ** 2)))
                for i in range(8)) - 3.57696e-6

    # was tesub2. V calculates vessel heat and delta_h uniquely.
    def set_vessel_temperature(self):
        #mutates self.T, INCLUDING when initialised with T=0
        T_in = self.T
        converged = False
        #L250
        for _ in range(100):
            htest = self.calc_vessel_heat() #sub1
            err = htest - self.es #recall we think es is heat!
            dh = self.calc_delta_h(dh)
            dt = -err / dh
            #mutate T
            self.T += dt
            if (abs(dt) < 1e-12):
                converged = True
                break #L300
        if not (converged):
            self.T = T_in

###############################################################################
# subfuncs

# called as (&self.xns[i]=*std, &xmns = *x)
def tesub6(xn):
    return (sum(random() for i in range(12)) - 6) * xn

if __name__ == "__main__":
    te = TEprob()
    te.run()