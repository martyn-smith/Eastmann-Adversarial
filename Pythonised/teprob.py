"""
#==============================================================================
#               tennessee eastman process control test problem
#
#                    james j. downs and ernest f. vogel
#
#                  process and control systems engineering
#                        tennessee eastman company
#                              p.o. box 511
#                          kingsport,tn  37662
#
#  reference:
#    "a plant-wide industrial process control problem"
#    presented at the aiche 1990 annual meeting
#    industrial challenge problems in process control,paper #24a
#    chicago,illinois,november 14,1990
#
#  Model of the TE (Tennessee Eastmann) challenge reactor.
#
#  The Plant takes four inputs, A, C, D, E, and produces two outputs,
#  G and H.
#
#  Primary reactions:
#
#  A(g) + C(g) + D(g) -> G(l)
#  A(g) + D(g) + E(g) -> H(l)
#
#  Byproduct reactions:
#
#  A(g) + E(g) -> F(l)
#  3D(g) -> 2F(l)
#
#  All are exothermic, reversible, and first-order
#  (rates follow an Arrhenius relation.)
#
#  Product flow is:
#  a,d,e ->                                  c ->
#     reactor -> condensor -> separator -> stripper -> product
#           <- compressor <- purge <-
#           <---------------------------------
#
#  manipulated variables
#
#    xmv[0]     a feed flow (stream 0) -> clearly should be different??
#    xmv[1]     d feed flow (stream 1)
#    xmv[2]     e feed flow (stream 2)
#    xmv[3]     a and c feed flow (stream 3)
#    xmv[4]     compressor recycle valve
#    xmv[5]     purge valve (stream 8)
#    xmv[6]     separator pot liquid flow (stream 9)
#    xmv[7]     stripper liquid product flow (stream 10)
#    xmv[8]     stripper steam valve
#    xmv[9]    reactor cooling water flow
#    xmv[10]    condenser cooling water flow
#    xmv[11]    agitator speed
#
#  continuous process measurements
#
#    xmeas[0]   a feed  (stream 1)                    kscmh
#    xmeas[1]   d feed  (stream 2)                    kg/hr
#    xmeas[2]   e feed  (stream 3)                    kg/hr
#    xmeas[4]   a and c feed  (stream 4)              kscmh
#    xmeas[5]   recycle flow  (stream 8)              kscmh
#    xmeas[6]   reactor feed rate  (stream 6)         kscmh
#    xmeas[7]   reactor pressure                      kpa gauge
#    xmeas[8]   reactor level                         %
#    xmeas[9]   reactor temperature                   deg c
#    xmeas[10]  purge rate (stream 9)                 kscmh
#    xmeas[11]  product sep temp                      deg c
#    xmeas[12]  product sep level                     %
#    xmeas[13]  prod sep pressure                     kpa gauge
#    xmeas[14]  prod sep underflow (stream 10)        m3/hr
#    xmeas[15]  stripper level                        %
#    xmeas[16]  stripper pressure                     kpa gauge
#    xmeas[17]  stripper underflow (stream 11)        m3/hr
#    xmeas[18]  stripper temperature                  deg c
#    xmeas[19]  stripper steam flow                   kg/hr
#    xmeas[20]  compressor work                       kw
#    xmeas[21]  reactor cooling water outlet temp     deg c
#    xmeas[22]  separator cooling water outlet temp   deg c
#
#  sampled process measurements
#
#    reactor feed analysis (stream 6)
#        sampling frequency = 0.1 hr
#        dead time = 0.1 hr
#        mole %
#    xmeas[23]   component a
#    xmeas[24]   component b
#    xmeas[25]   component c
#    xmeas[26]   component d
#    xmeas[27]   component e
#    xmeas[28]   component f
#
#    purge gas analysis (stream 9)
#        sampling frequency = 0.1 hr
#        dead time = 0.1 hr
#        mole %
#    xmeas[29]   component a
#    xmeas[30]   component b
#    xmeas[31]   component c
#    xmeas[32]   component d
#    xmeas[33]   component e
#    xmeas[34]   component f
#    xmeas[35]   component g
#    xmeas[36]   component h
#
#    product analysis (stream 11)
#        sampling frequency = 0.25 hr
#        dead time = 0.25 hr
#        mole %
#    xmeas[37]   component d
#    xmeas[38]   component e
#    xmeas[39]   component f
#    xmeas[40]   component g
#    xmeas[41]   component h
#
#    extras (43 onwards: TODO. Or... not?)
#
#    xmeas[42]   g/h ratio
#    xmeas[43]   cost
#    xmeas[42]   production rate of G [kmol G generated/h]
#    xmeas[43]   production rate of H [kmol H generated/h]
#    xmeas[44]   production rate of F [kmol F generated/h]
#
#  process disturbances
#
#    idv(1)   a/c feed ratio, b composition constant (stream 4)          step
#    idv(2)   b composition, a/c ratio constant (stream 4)               step
#    idv(3)   d feed temperature (stream 2)                              step
#    idv(4)   reactor cooling water inlet temperature                    step
#    idv(5)   condenser cooling water inlet temperature                  step
#    idv(6)   a feed loss (stream 1)                                     step
#    idv(7)   c header pressure loss - reduced availability (stream 4)   step
#    idv(8)   a, b, c feed composition (stream 4)            random variation
#    idv(9)   d feed temperature (stream 2)                  random variation
#    idv(10)  c feed temperature (stream 4)                  random variation
#    idv(11)  reactor cooling water inlet temperature        random variation
#    idv(12)  condenser cooling water inlet temperature      random variation
#    idv(13)  reaction kinetics                                    slow drift
#    idv(14)  reactor cooling water valve                            sticking
#    idv(15)  condenser cooling water valve                          sticking
#    idv(16)  random xmeas                                  Failure, 0.012 to 0.027 hr
#    idv(17)  none
#    idv(18)  none
#    idv(19)  multiple valves stick
#    idv(20)  none
#    idv(21)  Reactor T (°C),                                 Integrity attack, 0.012 to 0.027 hr
#    idv(22)  xmv 7, xmeas[14], xmeas[16]                     DDoS, 663 to 25019 hr
#    idv(23)  D feed flow (mv(0))                             DDoS, 10 hr
#    idv(24)  C feed (mv(3)), Purge flow (mv(5)), Stripper underflow (meas(16)),
#             Stripper steam (xmeas[8])                       Noise, 7,727 to 71,291 h.
#
#    Stream mappings
#
#    sm[0]  D feed -> J
#    sm[1]  E feed -> J
#    sm[2]  A feed -> J
#    sm[3]  A & C feed -> sm[11]
#    sm[4]  C (Stripper) -> J (junction)
#    sm[5]  J (junction) -> sm[6]
#    sm[6]  sm[5] -> R (Reactor)
#    sm[7]  R (Reactor) -> S (Separator)
#    sm[8]  S (Separator) -> V (compressor?)
#    sm[9] S (Separator) -> purge
#    sm[10] S (Separator) -> sm[11]
#    sm[11] sm[3] + S (Separator) -> C (Stripper)
#    sm[12] C (Stripper) -> prod
#===============================================================================
"""
from agent import DummyAgent
from blue import TEprobManager
from colorpy.blackbody import blackbody_color
from constants import *
import copy
import control
import enum
import gym
from matplotlib import pyplot as plt
import loss
import numpy as np
np.seterr(all="raise")
from random import choice, uniform
from red import ThreatAgent
import sys
if "--render" in sys.argv:
    from gym.envs.classic_control import rendering

DELTA_t = 1. / 3600.

log = []

class Action(enum.Enum):
    CONTINUE = 0
    MANUAL = 1
    RESET = 2

class ControlMode(enum.Enum):
     OPEN = 0
     CLOSED = 1

def idv(i):
    return 0

class Vessel:

    @property
    def delta_H(self):
    #   was tesub3
        # dh = 0.
        # for i in range(8):  #label 100...again
        #     dhi = 1.8 * (ah(i) + bh(i)*self.Tc + ch(i)*self.tc**2)
        #     dh += self.xl(i)*xmw(i)*dhi
        # #return dh
        return sum(self.xl * xmw * (1.8 * (ah + bh*self.tc + ch*self.tc**2)))

    @property
    def H(self):
    #   was tesub1. Split out for when it's used for vessels.
        # H = 0.
        # for i in range(8):  #label 100
        #     hi = 1.8 * self.Tc * (ah(i) + bh(i)*self.Tc/2. + ch(i)*self.Tc**2/3.)
        #     H += self.xl(i)*xmw(i)*hi
        # return H
        return sum(self.xl * xmw * (1.8 * self.tc * (ah + bh*self.tc/2. + ch*self.tc**2/3.)))

    @property
    def es(self):
        return self.et / self.utl

    @property
    def pg(self):
        #clearly, converting to mmHgg then to atmg then to kPag
        return (self.pt-760.0) / 760.0*101.325

    @property
    def tk(self):
        return self.tc + 273.15

    @property
    def utl(self):
        return sum(self.ucl)

    @property
    def vl(self):
        return self.utl / self.density

    @property
    def vv(self):
        return self.vt - self.vl

    @property
    def xl(self):
        return self.ucl / self.utl

    def set_density(self):
        v = sum(self.xl * xmw / (ad + (bd+cd * self.tc) * self.tc))
        self.density = 1.0 / v

    def set_P(self):
        global log
        self.pp = np.zeros(8)
        for i in range(3):  #label 1110 - for R and S only
            self.pp[i] = self.ucv[i] * rg * self.tk / self.vv

        for i in range(3,8): #label 1120 - for R and S only
            self.pp[i] = self.xl[i] * np.exp(avp[i] + bvp[i]/(self.tc + cvp[i])) #Antoine eq.
        self.pt = sum(self.pp) # in mmHga (!)
        #label 1130
        self.xv = self.pp / self.pt
        self.utv = self.pt * self.vv / rg / self.tk
        for i in range(3,8):#label 1140
            self.ucv[i] = self.utv * self.xv[i]

    def set_T(self):
        #note: this slightly weird way of doing it is necessary so H, dH,
        #es are correct

        if not hasattr(self, "tc"):
            self.tc = 0.
        T_in = self.tc
        for i in range(100):  #label 250
            err = self.H - self.es
            dh = self.delta_H
            dT = -err/dh
            self.tc += dT #main mutator of T
            if abs(dT) < 1.e-12:
                return
        self.tc = T_in

class GasVessel(Vessel):

    @property
    def delta_H(self):
        return (sum(self.xv * xmw
                    * 1.8
                    * (ag + bg * self.tc + cg * self.tc**2))
                - 3.57696e-6)

    @property
    def H(self):
        return (sum(self.xv * xmw
                   * (1.8 * self.tc
                     * (ag + bg * self.tc/2. + cg * self.tc**2/3.)
                     + av))
                - 3.57696e-6 * self.tk)

    @property
    def es(self):
        return self.et / self.utv

    @property
    def utv(self):
        return sum(self.ucv)

    @property
    def xv(self):
        return self.ucv/self.utv

    def set_P(self):
        self.pt = self.utv * rg * self.tk / self.vt # P = n R T / V

class Stream:

    #all streams weirdness:
    # set_H() initially called normally for 1,2,3,5,7,8 only
    # 9.H then copies 8.H so set_H() is never called
    # (but that means we can't evaluate lazily)
    # 10 and 12 are calculated differently as liquid streams
    # 4 (gas) and 11 (liquid) set_H are called, but later
    # fcm is calculated the same for all but 4, 6, and 11
    # 6 copies five, 4 and 11 are dependent on Sfr and Fin

    @property
    def fcm(self):
        return self.x * self.ftm

    @property
    def xmws(self):
        return sum(self.x * xmw)

    def set_H(self):
        self.H = sum(self.x * xmw
                     * (1.8 * self.T
                       * (ag + bg*self.T/2. + cg*self.T**2/3.)
                     + av))

class FeedStream(Stream):
    def __init__(self, seed):
        self.x = np.array(seed[:-1])
        self.T = seed[-1]

class FCMStream():
    #stream 4 only
    def __init__(self, seed = None):
        if seed is not None:
            self.x = np.array(seed[:-1])
            self.T = seed[-1]

    #inherits only this from regular streams
    def set_H(self):
        self.H = sum(self.x * xmw
                     * (1.8 * self.T
                       * (ag + bg*self.T/2. + cg*self.T**2/3.)
                     + av))

class FCMLiquidStream():
    #stream 11 only

    #inherits only this from LiquidStream
    def set_H(self):
        self.H = sum(self.x * xmw
                    * (1.8 * self.T
                    * (ah + bh*self.T/2. + ch*self.T**2/3.)))

class LiquidStream(Stream):
    #streams 10, 12

    def set_H(self):
        self.H = sum(self.x * xmw
                    * (1.8 * self.T
                      * (ah + bh*self.T/2. + ch*self.T**2/3.)))

class Valve():

    def __init__(self, id, pos, rng, tau):
        self.id = id
        self.pos = pos
        self.rng = rng
        self.tau = tau / 3600.

    def fail(self):
        return False

    def is_stuck(self):
        return False

    def flow(self):
        if self.fail():
            return 0.
        return self.pos * self.rng / 100.0 * uniform(0.95, 1.05)

    def set(self, mv):
        derivative = (mv - self.pos) / self.tau
        if not self.is_stuck():
            self.pos += derivative * DELTA_t

class Agitator:

    def __init__(self):
        self.speed = 0.

class Coolant:

    def __init__(self, h, T_in, T_out):
        self.h = h
        self.T_in = T_in
        self.T_out = T_out

class Sensor:

    def __init__(self, period):
        self.period = period

class Reactor(Vessel):

    def __init__(self, seed):
        self.vt = 1300.0
        self.cl = Coolant(h=7060., T_in=35., T_out=94.59927549)
        self.ucv = np.array([seed[0], seed[1], seed[2], 0., 0., 0., 0., 0.])
        self.ucl = np.array([0., 0., 0., seed[3], seed[4], seed[5], seed[6], seed[7]])
        self.et = seed[8]

    def __str__(self):
        return (f"(utl = {self.utl=}, utv = {self.utv}, et = {self.et}, es = {self.es}, tc = {self.tc}, tk = {self.tk}, density = {self.density}, "
                 + f"vt = {self.vt=}, vl = {self.vl}, vv = {self.vv}, pt = {self.pt}, qu = {self.qu}, ucl = {self.ucl}, ucv = {self.ucv}"
                 + f"xl = {self.xl}, xv = {self.xv}, pp = {self.pp}")

    @property
    def level(self):
        return (self.vl - 84.6) / 666.7

    def set_et(self, in_stream, out_stream, reaction_heat):
        derivative = ((in_stream.H * in_stream.ftm)
                      - (out_stream.H * out_stream.ftm)
                      + reaction_heat
                      + self.qu)
        self.et += derivative * DELTA_t

    def set_heat_transfer(self, agtatr):
        uarlev = np.clip((1. / 312.) * (self.vl - 78.), 0.0, 1.0)
        ua = uarlev * (-0.5 * agtatr.speed**2 + 2.75 * agtatr.speed - 2.5) * 855490.e-6
        self.qu = ua * (self.cl.T_out - self.tc)

    def react(self):
        reaction_rate = np.zeros(4)
        #A + C + D -> G
        reaction_rate[0] = 5.219217002265e+13 * np.exp(-40./(1.987e-3 * self.tk))
        #A + D + E -> H
        reaction_rate[1] = 20.27525952163 * np.exp(-20./(1.987e-3 * self.tk))
        #A + E -> F
        reaction_rate[2] = 1.5629689117665e+23 * np.exp(-60./(1.987e-3 * self.tk))
        #3D -> 2F
        reaction_rate[3] = reaction_rate[2] * 0.767488334
        if self.pp[0] > 0.0 and self.pp[2] > 0.0:
            rf = self.pp[0]**1.1544 * self.pp[2]**0.3735 #??? rr[1] should not be dependent on C
            reaction_rate[0] *= rf * self.pp[3] #D dependence
            reaction_rate[1] *= rf * self.pp[4] #E dependence
        else:
            reaction_rate[0] = 0.
            reaction_rate[1] = 0.

        reaction_rate[2] *= self.pp[0]*self.pp[4] #E dependence
        reaction_rate[3] *= self.pp[0]*self.pp[3] #D dependence

        reaction_rate *= self.vv

    #   consumption / generation
        delta_xr = np.zeros(8)
        delta_xr[0] = -reaction_rate[0]-reaction_rate[1]-reaction_rate[2] # A consumption
        delta_xr[2] = -reaction_rate[0]-reaction_rate[1] # C consumption.  Should be by rr(1) only!?
        delta_xr[3] = -reaction_rate[0]-1.5*reaction_rate[3] # D consumed by rr(1), rr(2), rr(4)
        delta_xr[4] = -reaction_rate[1]-reaction_rate[2] # E consumed by rr(2), rr(3)
        delta_xr[5] = reaction_rate[2]+reaction_rate[3] # F created by rr(3), rr(4)
        delta_xr[6] = reaction_rate[0] # A + C + D -> G
        delta_xr[7] = reaction_rate[1] # A + D + E -> H
        reaction_heat = reaction_rate[0]*htr[0]+reaction_rate[1]*htr[1]
        return delta_xr, reaction_heat

    def set_tw(self):
        derivative = (self.cl.flow * 500.53 * (self.cl.T_in - self.cl.T_out) - self.qu*1.e6/1.8) / self.cl.h
        self.cl.T_out += derivative * DELTA_t

    def set_uc(self, in_stream, out_stream, delta_xr):
        derivative = in_stream.fcm - out_stream.fcm + delta_xr
        self.ucv[:3] += np.array([*(derivative[:3] * DELTA_t)])
        self.ucl += np.array([*(np.zeros(3)), *(derivative[3:] * DELTA_t)])

    def set_P(self):
        #debug function, it doesn't need to be separate from super()
        old_pp = self.pp if hasattr(self, "pp") else np.zeros(8)
        self.pp = np.zeros(8)
        for i in range(3):  #label 1110 - for R and S only
            self.pp[i] = self.ucv[i] * rg * self.tk / self.vv

        for i in range(3,8): #label 1120 - for R and S only
            self.pp[i] = self.xl[i] * np.exp(avp[i] + bvp[i]/(self.tc + cvp[i])) #Antoine eq.
        self.pt = sum(self.pp) # in mmHga (!)
        #label 1130
        self.xv = self.pp / self.pt
        self.utv = self.pt * self.vv / rg / self.tk
        for i in range(3,8):#label 1140
            self.ucv[i] = self.utv * self.xv[i]

class Separator(Vessel):

    def __init__(self, seed):
        self.vt = 3500.0
        self.cl = Coolant(h=11138., T_in=40., T_out=77.29698353)
        self.ucv = np.array([seed[0], seed[1], seed[2], 0., 0., 0., 0., 0.])
        self.ucl = np.array([0., 0., 0., seed[3], seed[4], seed[5], seed[6], seed[7]])
        self.et = seed[8]

    def set_et(self, in_stream, out_streams, cmpsr): #out => 8,9,10
        derivative = ((in_stream.H*in_stream.ftm)
                       - (out_streams[0].H*out_streams[0].ftm - cmpsr.work)
                       - out_streams[1].H*out_streams[1].ftm
                       - out_streams[2].H*out_streams[2].ftm
                       + self.qu)
        self.et += derivative * DELTA_t

    def set_heat_transfer(self, sm):
        ua = 0.404655 * (1. - 1./(1. + (sm.ftm / 3528.73)**4))
        self.qu = ua * (self.cl.T_out - sm.T)

    def set_tw(self):
        derivative = ((self.cl.flow
                       * 500.53
                       * (self.cl.T_in - self.cl.T_out)
                       - self.qu*1.e6/1.8)
                     / self.cl.h)
        self.cl.T_out += derivative * DELTA_t

    def set_uc(self, in_stream, out_streams):
        #Separator, as reminder
        #E is being depleted
        derivative = in_stream.fcm - sum(sm.fcm for sm in out_streams)
        self.ucv += np.array([*(derivative[:3] * DELTA_t), *np.zeros(5)])
        self.ucl += np.array([*np.zeros(3), *(derivative[3:] * DELTA_t)])

    @property
    def level(self):
        return (self.vl-27.5) / 290.0

class Stripper(Vessel):

    def __init__(self, seed):
        self.vt = 156.5
        self.ucv = np.zeros(3)
        self.ucl = np.array(seed[:-1])
        self.et = np.array(seed[-1])

    def set_et(self, in_streams, out_streams):
        derivative = (sum(sm.H * sm.ftm for sm in in_streams)
                     - sum(sm.H * sm.ftm for sm in out_streams)
                     + self.qu)
        self.et += derivative * DELTA_t

    def set_heat_transfer(self, flow):
        ua = flow
        self.qu = 0.
        if(self.tc < 100.):
            self.qu = ua*(100.0-self.tc)

    def set_uc(self, in_stream, out_stream):
        derivative = in_stream.fcm - out_stream.fcm
        #self.ucv += derivative[:3] * DELTA_t
        self.ucl += derivative * DELTA_t

    @property
    def level(self):
        return (self.vl-78.25) / self.vt

class Compressor(Vessel):

    def __init__(self):
        self.max_flow = 280275.
        self.max_PR = 1.3

    def set_work(self, flms, s, v, sm):
        self.work = flms * (s.tk) * 1.8e-6 * 1.9872 * (v.pt-s.pt)/(sm.xmws*s.pt)

class Junction(GasVessel):

    def __init__(self, seed):
        self.vt = 5000.0
        self.ucv = np.array(seed[:-1])
        self.et = seed[-1]

    def set_et(self, in_streams, out_stream):
        derivative = (sum(sm.H * sm.ftm for sm in in_streams)
                      - (out_stream.H * out_stream.ftm))
        self.et += derivative * DELTA_t

    def set_uc(self, in_streams, out_stream):
        derivative = (sum(sm.fcm for sm in in_streams)
                      - out_stream.fcm)
        self.ucv += derivative * DELTA_t

class Sfr:

    def __init__(self, seed):
        self.fcm = np.array(seed)

    @property
    def ftm(self):
        return sum(self.fcm)

    def set_fcm(self, c, sm):
        if sm[10].ftm > 0.1:
            if c.tc > 170.:
                tmpfac = c.tc - 120.262
            elif c.tc < 5.292:
                tmpfac = 0.1
            else:
                tmpfac = 363.744/(177. - c.tc) - 2.22579488
            vovrl = sm[3].ftm / sm[10].ftm * tmpfac
            self.fcm[3] = 8.5010 * vovrl / (1.0+8.5010*vovrl)
            self.fcm[4] = 11.402 * vovrl / (1.0+11.402*vovrl)
            self.fcm[5] = 11.795 * vovrl / (1.0+11.795*vovrl)
            self.fcm[6] = 0.0480 * vovrl / (1.0+0.0480*vovrl)
            self.fcm[7] = 0.0242 * vovrl / (1.0+0.0242*vovrl)
        else:
            self.fcm[3] = 0.9999
            self.fcm[4] = 0.999
            self.fcm[5] = 0.999
            self.fcm[6] = 0.99
            self.fcm[7] = 0.98

class TEproc(gym.Env):

    def __init__(self, ctrl_mode=None):
        """
        initialization

        inputs:
            nn = number of differential equations

        mutates:
            time = current time(hrs)
            state = current state values
            derivative = current derivative values


        """
        #TODO:
        #valve_stick loop
        #xmv/vcv loop
        seed = {
            "R": [10.40491389, 4.363996017, 7.570059737, .4230042431, 24.15513437,
                  2.942597645, 154.3770655, 159.186596, 2.808522723],
            "S": [63.75581199, 26.74026066, 46.38532432, .2464521543, 15.20484404,
                  1.852266172, 52.44639459, 41.20394008, .569931776,],
            "C": [.4306056376, .0079906200783, .9056036089, .016054258216, .7509759687,
                  .088582855955, 48.27726193, 39.38459028, .3755297257],
            "V": [107.7562698, 29.77250546, 88.32481135, 23.03929507, 62.85848794,
                  5.546318688, 11.92244772, 5.555448243, .9218489762],
            "Twr" : 94.59927549,
            "Tws" : 77.29698353,
            "vpos": [63.05263039, 53.97970677, 24.64355755, 61.30192144, 22.21, 40.06374673,
                     38.1003437, 46.53415582, 47.44573456, 41.10581288, 18.11349055, 50.],
            "vrng" : [400.00, 400.00, 100.00, 1500.00, None, None,
                      1500.00, 1000.00, 0.03, 1000., 1200.0, None],
            "vtau" : [8., 8., 6., 9., 7., 5.,
                      5., 5., 120., 5., 5., 5.],
            "sfr" : [0.995, 0.991, 0.99, 0.916, 0.936, 0.938, 0.058, 0.0301],
            0 : [0., 0.0001, 0., 0.9999, 0., 0., 0., 0., 45.], #D feed
            1 : [0., 0., 0., 0., 0.9999, 0.0001, 0., 0., 45.], #E feed
            2 : [0.9999, 0.0001, 0., 0., 0., 0., 0., 0., 45.], #A feed
            3 : [0.4850, 0.0050, 0.5100, 0., 0., 0., 0., 0., 45.] #A and C feed
        }

        self.time = 0.
        self.T_GAS = 0.1
        self.T_PROD = 0.25
        self.time_since_gas = 0.
        self.time_since_prod = 0.
        self.r = Reactor(seed["R"])
        self.s = Separator(seed["S"])
        self.c = Stripper(seed["C"])
        self.j = Junction(seed["V"])
        self.sm = ([FeedStream(seed[0]), FeedStream(seed[1]), FeedStream(seed[2]), FeedStream(seed[3])]
                   + [FCMStream()]
                   + [Stream() for _ in range(5)]
                   + [LiquidStream()]
                   + [FCMLiquidStream()]
                   + [LiquidStream()])
        self.sfr = Sfr(seed["sfr"])
        self.valves = [Valve(i, p, r, t) for i, p, r, t in zip(
                                                               list(range(12)),
                                                               seed["vpos"],
                                                               seed["vrng"],
                                                               seed["vtau"])]
        self.cmpsr = Compressor()
        self.agtatr = Agitator()

        self.prod_mode = 1
        if "--open" in sys.argv:
            self.ctrlr = control.Dummy()
        else:
            self.ctrlr = control.Controller(seed["vpos"])
        self.faults = [0] * 20
        self.attacks = [0] * 20

    def step(self, action: list[Action]):
        """
        function evaluator
        """

        global log
        reset = False

        red_action, blue_action = action[0], action[1]
        if blue_action == "reset_all": #reset signal
            reset = True
            self.reset()
            red_action = None
        #action: blue can reset control loops 0-8
        #action: red can perturb xmvs
        if red_action is not None:
            if "xmv" in red_action:
                self.ctrlr.perturb_xmv(red_action["xmv"])
        for mv, valve in zip(self.ctrlr.xmv, self.valves):
            valve.set(mv)

        self.r.set_T()
        self.s.set_T()
        self.c.set_T()
        self.j.set_T()

        self.r.set_density()
        self.s.set_density()
        self.c.set_density()

    #   setting pressures
        self.r.set_P()
        self.s.set_P()
        self.j.set_P()

    #   setting reactions
    #   R is in cal.K-1mol-1! first Ea works out to about 167 kJmol-1
        delta_xr, reaction_heat = self.r.react()

    #   label 2010 vectorised
        self.sm[5].x = self.j.xv
        self.sm[7].x = self.r.xv
        self.sm[8].x = self.s.xv
        self.sm[9].x = self.s.xv
        self.sm[10].x = self.s.xl
        self.sm[12].x = self.c.xl

    #   setting stream temps
        self.sm[5].T = self.j.tc
        self.sm[7].T = self.r.tc
        self.sm[8].T = self.s.tc
        self.sm[9].T = self.s.tc
        self.sm[10].T = self.s.tc
        self.sm[12].T = self.c.tc
    #   setting stream heats
        self.sm[0].set_H()
        self.sm[1].set_H()
        self.sm[2].set_H()
        self.sm[3].set_H()
        self.sm[5].set_H()
        self.sm[7].set_H()
        self.sm[8].set_H()
        self.sm[9].H = self.sm[8].H
        self.sm[10].set_H()
        self.sm[12].set_H()

    #   setting stream mass flows
        self.sm[2].ftm = self.valves[2].flow() # A feed
        self.sm[0].ftm = self.valves[0].flow() # D feed
        self.sm[1].ftm = self.valves[1].flow() # E feed
        self.sm[3].ftm = self.valves[3].flow() # A and C feed
        self.sm[10].ftm = self.valves[6].flow() # separator underflow
        self.sm[12].ftm = self.valves[7].flow() # stripper underflow
        self.r.cl.flow = self.valves[9].flow()
        self.s.cl.flow = self.valves[10].flow()
        self.agtatr.speed = (self.valves[11].pos + 150.0)/100.0

        delta_p = max(self.j.pt - self.r.pt, 0.)
        flms = 1937.6 * np.sqrt(delta_p)
        self.sm[5].ftm = flms / self.sm[5].xmws # volume flow per time / m weight = moles / s / density?

        delta_p = max(self.r.pt - self.s.pt, 0.) # reactor - separator
        flms = 4574.21 * np.sqrt(delta_p)
        self.sm[7].ftm = flms / self.sm[7].xmws # mass flow = volume / mean mass?

        delta_p = self.s.pt - 760.0 # sep - atmosphere
        flms = self.valves[5].pos * 0.151169*np.sqrt(abs(delta_p)) * np.sign(delta_p)
        self.sm[9].ftm = flms / self.sm[9].xmws

        pr = np.clip(self.j.pt / self.s.pt, 1., self.cmpsr.max_PR)
        flcoef = self.cmpsr.max_flow/1.197
        flms = self.cmpsr.max_flow + flcoef * (1.0 - pr**3)
    #   later conversion implies this is in BTU
        self.cmpsr.set_work(flms, self.s, self.j, self.sm[8])

        delta_p = max(self.j.pt - self.s.pt, 0.)
        flms = max(flms - self.valves[4].pos * 53.349 * np.sqrt(delta_p), 1.e-3)
        self.sm[8].ftm = flms / self.sm[8].xmws
        self.sm[8].H += self.cmpsr.work / self.sm[8].ftm

    #   label 5020 abstracted into property

        self.sfr.set_fcm(self.c, self.sm)
    #   label 6010
        fin = self.sm[3].fcm + self.sm[10].fcm
    #   label 6020 and 6030
        self.sm[4].fcm = self.sfr.fcm * fin # only place that consumes sfr
        self.sm[11].fcm = fin - self.sm[4].fcm
        self.sm[4].ftm = sum(self.sm[4].fcm)
        self.sm[11].ftm = sum(self.sm[11].fcm)
        self.sm[4].x = self.sm[4].fcm / self.sm[4].ftm
        self.sm[11].x = self.sm[11].fcm / self.sm[11].ftm

        self.sm[4].T = self.c.tc
        self.sm[11].T = self.c.tc
        self.sm[4].set_H()
        self.sm[11].set_H()

        self.sm[6] = copy.deepcopy(self.sm[5])

    #   calculate cooling from water feeds
        self.r.set_heat_transfer(self.agtatr)
        self.s.set_heat_transfer(self.sm[7])
        self.c.set_heat_transfer(self.valves[8].flow())

        self.r.set_et( self.sm[6], self.sm[7], reaction_heat)
        self.s.set_et( self.sm[7], (self.sm[8], self.sm[9], self.sm[10]), self.cmpsr)
        self.c.set_et( (self.sm[3], self.sm[10]), (self.sm[4], self.sm[12]))
        self.j.set_et( (self.sm[0], self.sm[1], self.sm[2], self.sm[4], self.sm[8]), self.sm[5] )
    #   twr and tws
        self.r.set_tw()  #delta_T in degC, 1.8 is F->C
        self.s.set_tw()

        #label 9010
        self.r.set_uc( self.sm[6], self.sm[7], delta_xr)
        self.s.set_uc( self.sm[7], (self.sm[8], self.sm[9], self.sm[10]))
        self.c.set_uc( self.sm[11], self.sm[12])
        self.j.set_uc( (self.sm[0], self.sm[1], self.sm[2], self.sm[4], self.sm[8]), self.sm[5])

    #   sticking idvs
    #    valve_stick(10) = idv(14) #reactor cooling valve sticks
    #    valve_stick(11) = idv(15) #condensor cooling valve sticks
    #    valve_stick(5) = idv(19)  #recycle flow valve sticks
    #    valve_stick(7) = idv(19)  #separator underflow sticks
    #    valve_stick(8) = idv(19)  #stripper underflow sticks
    #    valve_stick(9) = idv(19)  #stripper recirc?

        xmeas = self.measure()
        true_xmeas = xmeas
        if red_action is not None:
            if "xmeas" in red_action:
                xmeas[red_action["xmeas"]] = 0.
        done = self.has_failed(xmeas, self.time)
        l = loss.loss(reset, done, true_xmeas, self.ctrlr.xmv)
        if red_action is not None:
            if "setpt" in red_action:
                self.ctrlr.perturb_setpt(red_action["setpt"])
        if blue_action is not None:
            if "reset" in blue_action:
                self.ctrlr.reset_single(blue_action["reset"])
        self.ctrlr.control(xmeas, self.time) #here?
        # update valves.xmv is control signal, translated to vcv if no stick.
        # returns: (possibly false) xmeas, loss based on true state, done if failed
        log += [self.s.level]
        self.time += DELTA_t
        return (true_xmeas, xmeas), l, bool(done), {"failures": done}

    def measure(self):
        xmeas = np.zeros(43)
        xmeas[0] = self.time
        xmeas[1] = self.sm[2].ftm*0.359/35.3145 # A Feed  (stream 1)                    kscmh ,
        xmeas[2] = self.sm[0].ftm*self.sm[0].xmws*0.454 # D Feed  (stream 2)                 kg/hr from lbmol/hr
        xmeas[3] = self.sm[1].ftm*self.sm[1].xmws*0.454 # E Feed  (stream 3)                 kg/hr
        xmeas[4] = self.sm[3].ftm*0.359/35.3145 # A and C Feed  (stream 4)              kscmh
        xmeas[5] = self.sm[8].ftm*0.359/35.3145 # Recycle Flow  (stream 8)              kscmh
        xmeas[6] = self.sm[5].ftm*0.359/35.3145 # Reactor Feed Rate  (stream 6)         kscmh
        xmeas[7] = self.r.pg # Reactor Pressure                   kPa gauge
        xmeas[8] = self.r.level * 100.0 #                     %
        xmeas[9] = self.r.tc # Reactor Temperature                                      deg C
        xmeas[10] = self.sm[9].ftm*0.359/35.3145 # purge rate (stream 9)               kscmh
        xmeas[11] = self.s.tc # product sep temp                                        deg c
        xmeas[12] = self.s.level * 100.0 # product sep level                    %
        xmeas[13] = (self.s.pt-760.0)/760.0*101.325 # sep pressure                      kpa gauge
        xmeas[14] = self.sm[10].ftm/self.s.density/35.3145 # sep underflow (stream 10)       m3/hr
        xmeas[15] = (self.c.vl-78.25)/self.c.vt*100.0 # stripper level                       %
        xmeas[16] = (self.j.pt-760.0)/760.0*101.325 # stripper pressure                 kpa gauge
        xmeas[17] = self.sm[12].ftm/self.c.density/35.3145 # stripper underflow (stream 11, aka production) m3/hr
        xmeas[18] = self.c.tc # stripper temperature                                    deg c
        xmeas[19] = self.c.qu*1.04e3*0.454 # stripper steam flow                        kg/hr
        xmeas[20] = self.cmpsr.work*0.29307e3 # compressor work, again??                kwh
        xmeas[21] = self.r.cl.T_out # reactor cooling water outlet temp                 deg c
        xmeas[22] = self.s.cl.T_out # separator cooling water outlet temp               deg c

        if idv(16):
            if xmeas_tgt == 0:
                xmeas_tgt = np.ceiling(rand() * 42.0)

            if time > 0.012 and time < 0.027:
                xmeas[xmeas_tgt] = 0.0

        if idv(21):
            if time > 0.012 and time < 0.027:
                xmeas[9] = 500.0

        if self.time_since_gas >= self.T_GAS or not hasattr(self, "gas"): #purge gas and reactor feed analysis
            self.gas = (self.sm[5].x[:-2], self.sm[8].x)
            self.time_since_gas = 0.
        xmeas[23:29] = self.gas[0] * 100
        xmeas[29:37] = self.gas[1] * 100
        self.time_since_gas += DELTA_t

        if self.time_since_prod >= self.T_PROD or not hasattr(self, "prod"): #product feed analysis
            self.prod = self.sm[12].x[3:]
            self.time_since_prod = 0.
        xmeas[37:42] = self.prod * 100
        self.time_since_prod += DELTA_t

        xmeas[42] = (xmw[6] * xmeas[40]) / (xmw[7] * xmeas[41])
        return xmeas

    @property
    def state(self):
        """
        layout of state vector:

            |1--- 3||4 --- 8||  9 ||10 - 12||13 - 17|| 18 ||19 - 26|| 27 ||28 - 35|| 36 |,
            | R.ucv || R.ucl ||R.et|| S.ucv || S.ucl ||S.et|| C.ucl ||C.et|| V.ucv ||V.et|,

            | 37|| 38||   39-50   |,
            |twr||tws|| vcv/vpos  |,
        """
        return np.array([*self.r.ucv[:3], *self.r.ucl[3:], self.r.et,
                         *self.s.ucv[:3], *self.s.ucl[3:], self.s.et,
                         *self.c.ucl, self.c.et,
                         *self.j.ucv, self.j.et,
                         self.r.cl.T_out, self.s.cl.T_out,
                         *[v.pos for v in self.valves]])

    def has_failed(self, xmeas, time):
        if self.r.pg > 3000:
            return "Reactor pressure high"
        elif self.r.pg < 2700:
            return "Reactor pressure low"
        elif self.r.level > 1.144:
            return "Reactor level high"
        elif self.s.level > 1.37:
            return "Separator level high"
        elif self.s.level < 0.27:
            return "Separator level low"
        else:
            return False

    def reset(self):
        self.__init__()
        #run for one hour
        self.ctrlr.reset()
        for i in range(3600):
            self.step([None, None])
        return self.step([None, None])

    def render(self, mode="human"):
        screen_width = 640
        screen_height = 480
        vessel_width = 50.0
        vessel_height = 30.0
        color_offset = 800.0
        sep_space = 200
        b = -vessel_height / 2

        if not hasattr(self, "viewer") and mode=="human":
            self.viewer = rendering.Viewer(screen_width, screen_height)

        l = -vessel_width / 2
        t = self.r.level * 200
        r = self.r.pg / 30.
        reactor = rendering.FilledPolygon([(l, b), (l, t), (r, t), (r, b)])
        reactor.set_color(*blackbody_color(self.r.tk + 800))
        self.reactrans = rendering.Transform()
        reactor.add_attr(self.reactrans)
        self.viewer.add_geom(reactor)

        l += sep_space
        t = self.s.level * 200
        r = sep_space + self.s.pg / 40.
        separator = rendering.FilledPolygon([(l, b), (l, t), (r, t), (r, b)])
        separator.set_color(*blackbody_color(self.s.tk + 800))
        self.septrans = rendering.Transform()
        separator.add_attr(self.septrans)
        self.viewer.add_geom(separator)

        l += sep_space
        t = self.c.level * 200
        r = (sep_space * 2) + 100.
        stripper = rendering.FilledPolygon([(l, b), (l, t), (r, t), (r, b)])
        stripper.set_color(*blackbody_color(self.c.tk + 800))
        self.striptrans = rendering.Transform()
        stripper.add_attr(self.striptrans)
        self.viewer.add_geom(stripper)

        return self.viewer.render(return_rgb_array=mode == "rgb_array")

    def close(self):
        if hasattr(self, "viewer"):
            self.viewer.close()
            self.viewer = None

def help():
    print(
"""
usage: python3 teprob.py [OPTIONS]

options:

    --fast            runs for one hour, not 48
    --v               prints reward
    --render          visualisation (slow)
    --peaceful        no agents
    --open            no control loops
"""
    )

if __name__ == "__main__":

    if "--fast" in sys.argv:
        hrs = 0.1
    else:
        hrs = 48

    gym.envs.registration.register(
        id="TennesseeEastmann-v1",
        entry_point="__main__:TEproc",
        max_episode_steps=int(hrs*3600),
        reward_threshold=195.0
    )

    env = gym.make("TennesseeEastmann-v1")
    if "--peaceful" in sys.argv:
        red, blue = DummyAgent(), DummyAgent()
        num_episodes = 1
    else:
        red = ThreatAgent(intent=0)
        blue = TEprobManager()
        num_episodes = 100
    observations, _, __, ___ = env.reset()
    blue_action = blue.get_action(observations[0][1:])
    red_action = red.get_action(observations[1][1:])
    action = (blue_action, red_action)
    for i in range(num_episodes):
        env.reset()
        for t in range(env._max_episode_steps):
            prev_obs = observations
            blue_action = blue.get_action(prev_obs[0][1:])
            red_action = red.get_action(prev_obs[1][1:])
            action = (blue_action, red_action)
            if "-v" in sys.argv:
                print(action)
            observations, blue_reward, done, info = env.step(action)
            red_obs = observations[0]
            blue_obs = observations[1]
            #blue_reward = loss.loss(blue_obs)
            red_reward = - red.intent(red_obs)
            if "-v" in sys.argv:
                print(f"{blue_reward=}, {red_reward=}")
            if "--render" in sys.argv:
                env.render()
            red.remember(prev_obs[0][1:], red_action, blue_reward, red_obs[1:], done)
            blue.remember(prev_obs[1][1:], blue_action, red_reward, blue_obs[1:], done)
            print("reactor P, t, and control value: ", env.r.pg, env.r.level, env.r.tc, env.ctrlr.xmv[3])
            if done:
                print(f"Episode finished after {t} timesteps: "
                      + f"{'red' if info['failures'] else 'blue'} team wins")
                break
        env.close()
        if "--render" in sys.argv:
            plt.plot(log)
            plt.show()
        red.replay()
        blue.replay()
