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
#    xmv(1)     a feed flow (stream 1)
#    xmv(2)     d feed flow (stream 2)
#    xmv(3)     e feed flow (stream 3)
#    xmv(4)     a and c feed flow (stream 4)
#    xmv(5)     compressor recycle valve
#    xmv(6)     purge valve (stream 9)
#    xmv(7)     separator pot liquid flow (stream 10)
#    xmv(8)     stripper liquid product flow (stream 11)
#    xmv(9)     stripper steam valve
#    xmv(10)    reactor cooling water flow
#    xmv(11)    condenser cooling water flow
#    xmv(12)    agitator speed
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
#    sm[0]  D feed -> V
#    sm[1]  E feed -> V
#    sm[2]  A feed -> V
#    sm[3]  A & C feed -> sm[11]
#    sm[4]  C (Stripper) -> V (compressor?)
#    sm[5]  V (compressor) -> sm[6]
#    sm[6]  V -> R (Reactor)
#    sm[7]  R (Reactor) -> S (Separator)
#    sm[8]  S (Separator) -> V (compressor?)
#    sm[9] S (Separator) -> purge
#    sm[10] S (Separator) -> sm[11]
#    sm[11] sm[4-1] + S (Separator) -> C (Stripper)
#    sm[12] C (Stripper) -> prod
#===============================================================================
"""
from blue import TEprobManager
from colorpy.blackbody import blackbody_color
from constants import *
import control
import enum
import gym
from gym.envs.classic_control import rendering
from matplotlib import pyplot as plt
import loss
import numpy as np
np.seterr(all="raise")
from random import choice, uniform
from red import ThreatAgent
from sys import exit

DELTA_t = 1. / 3600.

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

    @property
    def level(self):
        return (self.vl - 84.6 / 666.7)    

    def set_density(self):
        v = sum(self.xl * xmw / (ad + (bd+cd * self.tc) * self.tc))
        self.density = 1.0 / v

    def set_P(self):
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
        # if debug:
        #     print("setting T for reactor")
        T_in = self.tc
        for i in range(100):  #label 250
            err = self.H - self.es
            dh = self.delta_H
            dT = -err/dh
            self.tc += dT #main mutator of T
            # if (debug):
            #     print(self.tc, self.H, self.delta_H)
            if abs(dT) < 1.e-12:
                # if debug:
                #     print("converged")
                return
        self.tc = T_in

class GasVessel(Vessel):

    @property
    def delta_H(self):
        # dh = 0.
        # for i in range(8): 
        #     dhi = 1.8 * (ag(i) + bg(i)*self.tc + cg(i)*self.tc**2)
        #     dh = dh + self.xv(i)*xmw(i)*dhi
        # dh = dh 
        return (sum(self.xv * xmw 
                    * 1.8 
                    * (ag + bg * self.tc + cg * self.tc**2)) 
                - 3.57696e-6)

    @property
    def H(self):
    #   was tesub1. Split out for when it's used for vessels.
        # H = 0.
        # for i in range(8):  #label 200
        #     hi = 1.8 * self.Tc * (ag(i) + bg(i)*self.Tc/2. + cg(i)*self.Tc**2/3.) + av(i)
        #     H += self.xv(i)*xmw(i)*hi 
        # H -= 3.57696e-6 * self.tk
        # return H
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
    # 9 then copies 8
    # then for 10 and 12 with ity = 0 (i.e. as liquid?)
    # 4 (ity=1) and 11 (ity = 0) are called later
    # fcm is calculated the same for all but 4, 6, and 11
    # 6 copies five, 4 and 11 are dependent on Sfr and Fin

    def __init__(self, seed = None):
        if seed is not None:   
            self.x = np.array(seed[:-1])
            self.T = seed[-1]

    @property
    def xmws(self):
        return sum(self.x * xmw)

    def set_H(self):
        self.H = sum(self.x * xmw
                     * (1.8 * self.T 
                       * (ag + bg*self.T/2. + cg*self.T**2/3.) 
                     + av))

class LiquidStream(Stream):

    def __init__(self):
        pass

    #special calc for streams 10, 11, 12
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
                   
    def set_et(self, in_stream, out_stream, reaction_heat):
        #in_stream ftm is twice as high as should be, out_stream H is twice as low
        #but otherwise correct
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
        #r1f, r2f removed (== 1)
        reaction_rate = np.zeros(4)
        reaction_rate[0] = 5.219217002265e+13 * np.exp(-40./(1.987e-3 * self.tk)) * uniform(0.99, 1.01)
        reaction_rate[1] = 20.27525952163 * np.exp(-20./(1.987e-3 * self.tk)) * uniform(0.99, 1.01)
        reaction_rate[2] = 1.5629689117665e+23 * np.exp(-60./(1.987e-3 * self.tk)) * uniform(0.99, 1.01)
        reaction_rate[3] = reaction_rate[2] * 0.767488334
        if self.pp[0] > 0.0 and self.pp[2] > 0.0:
            rf = self.pp[0]**1.1544 * self.pp[2]**0.3735
            reaction_rate[0] *= rf * self.pp[3] 
            reaction_rate[1] *= rf * self.pp[4]
        else:
            reaction_rate[0] = 0.
            reaction_rate[1] = 0.
        
        reaction_rate[2] *= self.pp[0]*self.pp[4]
        reaction_rate[3] *= self.pp[0]*self.pp[3]

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
        #order of operations funkiness
        derivative = (self.cl.flow * 500.53 * (self.cl.T_in - self.cl.T_out) - self.qu*1.e6/1.8) / self.cl.h
        self.cl.T_out += derivative * DELTA_t 

    def set_uc(self, in_stream, out_stream, delta_xr):
        derivative = in_stream.fcm - out_stream.fcm + delta_xr
        self.ucv += np.array([*(derivative[:3] * DELTA_t), *np.zeros(5)])
        self.ucl += np.array([*(derivative[3:] * DELTA_t), *np.zeros(3)])

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
        #       delta separator UCV and UCL
        #                         React out      recycle        purge           underflow
        derivative = in_stream.fcm - sum(sm.fcm for sm in out_streams)
        self.ucv += np.array([*(derivative[:3] * DELTA_t), *np.zeros(5)])
        self.ucl += np.array([*(derivative[3:] * DELTA_t), *np.zeros(3)])

    @property
    def level(self):
        return (self.vl-27.5)/290.0

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
    #       delta C UCV and UCL
    #TODO: dims of this func
        derivative = in_stream.fcm - out_stream.fcm
        self.ucv += derivative[:3] * DELTA_t
        self.ucl += derivative * DELTA_t

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
        print(f"junction {derivative=}")
        self.et += derivative * DELTA_t

    def set_uc(self, in_streams, out_stream):
        derivative = (sum(sm.fcm for sm in in_streams)
                      - out_stream.fcm)
        self.ucv += derivative * DELTA_t

class Sfr:

    def __init__(self, seed):
        self.fcm = np.array(seed)

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

    def __init__(self, ctrl_mode = ControlMode.CLOSED):
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
            0 : [0., 0.0001, 0., 0.9999, 0., 0., 0., 0., 45.],
            1 : [0., 0., 0., 0., 0.9999, 0.0001, 0., 0., 45.],
            2 : [0.9999, 0.0001, 0., 0., 0., 0., 0., 0., 45.],
            3 : [0.4850, 0.0050, 0.5100, 0., 0., 0., 0., 0., 45.]
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
        self.sm = ([Stream(seed[0]), Stream(seed[1]), Stream(seed[2]), Stream(seed[3])]
                   + [Stream() for _ in range(6)]
                   + [LiquidStream() for _ in range(3)])
        self.sfr = Sfr(seed["sfr"])
        self.valves = [Valve(i, p, r, t) for i, p, r, t in zip(
                                                               list(range(12)), 
                                                               seed["vpos"], 
                                                               seed["vrng"], 
                                                               seed["vtau"])]
        self.cmpsr = Compressor()
        self.agtatr = Agitator()

        self.prod_mode = 1
        self.ctrlr = (control.Controller(seed["vpos"], self.prod_mode)
                        if ctrl_mode == ControlMode.CLOSED
                        else control.Dummy())
        self.faults = [0] * 20
        self.attacks = [0] * 20

    def step(self, action: list[Action]):
        """
        function evaluator
        
        inputs:
            action (XMVs only for this one)
        
            state = self.state
            next_walk(time)
            stream and idvs
        """

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
        self.sm[2].ftm = self.valves[2].flow() #vpos[2] * (1.-idv(6))*self.vrng[2]/100.0 # A feed
        self.sm[0].ftm = self.valves[0].flow() # D feed
        self.sm[1].ftm = self.valves[1].flow() # E feed
        self.sm[3].ftm = self.valves[3].flow() # A and C feed
        self.sm[10].ftm = self.valves[6].flow() # separator underflow
        self.sm[12].ftm = self.valves[7].flow() # stripper underflow
        self.r.cl.flow = self.valves[9].flow()
        self.s.cl.flow = self.valves[10].flow()
        self.agtatr.speed = (self.valves[11].pos + 150.0)/100.0

        #J is still 4% too hot, and thus 4% too high P.
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

        # label 5020 vectorised (yes, this also works)
        self.sm[0].fcm = self.sm[0].x * self.sm[0].ftm
        self.sm[1].fcm = self.sm[1].x * self.sm[1].ftm
        self.sm[2].fcm = self.sm[2].x * self.sm[2].ftm
        self.sm[3].fcm = self.sm[3].x * self.sm[3].ftm
        self.sm[5].fcm = self.sm[5].x * self.sm[5].ftm
        self.sm[7].fcm = self.sm[7].x * self.sm[7].ftm
        self.sm[8].fcm = self.sm[8].x * self.sm[8].ftm
        self.sm[9].fcm = self.sm[9].x * self.sm[9].ftm
        self.sm[10].fcm = self.sm[10].x * self.sm[10].ftm
        self.sm[12].fcm = self.sm[12].x * self.sm[12].ftm

        self.sfr.set_fcm(self.c, self.sm)
    #   label 6010
        fin = self.sm[3].fcm + self.sm[10].fcm
        #check fin
        print(f"{fin=}")

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

        self.sm[6].ftm = self.sm[5].ftm
        self.sm[6].H = self.sm[5].H
        self.sm[6].T = self.sm[5].T
    #   label 6130
        self.sm[6].x = self.sm[5].x
        self.sm[6].fcm = self.sm[5].fcm

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
        self.r.set_uc( self.sm[6], self.sm[8], delta_xr)
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
        
        xmeas = self.measure(False)
        true_xmeas = self.measure(True)
        done = self.has_failed()
        l = loss.loss(None, done, true_xmeas, None)
        self.control(xmeas) #here?
        # update valves.xmv is control signal, translated to vcv if no stick.
        # returns: (possibly false) xmeas, loss based on true state, done if failed 
        return xmeas, l, done, {}

    def measure(self, reliable):
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
        xmeas[17] = self.sm[12].ftm/self.c.density/35.3145 # stripper underflow (stream 11)  m3/hr
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
        xmeas[23:29] = self.gas[0]
        xmeas[29:37] = self.gas[1]
        self.time_since_gas += DELTA_t
        
        if self.time_since_prod >= self.T_PROD or not hasattr(self, "prod"): #product feed analysis
            self.prod = self.sm[12].x[3:]
            self.time_since_prod = 0.
        xmeas[37:42] = self.prod
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

    def control(self, xmeas):
        if hasattr(self.ctrlr, "control"):
            self.ctrlr.control(xmeas)

    def has_failed(self):
        return (self.r.pg > 3000.
                or False)

    def reset(self):
        self.__init__()
        #reset MUST return an initial observation
        return np.zeros(43)

    def render(self, mode="human"):
        screen_width = 640
        screen_height = 480
        vessel_width = 50.0
        vessel_height = 30.0
        color_offset = 800.0
        sep_space = 300

        if not hasattr(self, "viewer") and mode=="human":
            self.viewer = rendering.Viewer(screen_width, screen_height)
        l, b = -vessel_width / 2, -vessel_height / 2
        t = self.r.level / 5
        r = self.r.pg / 30.
        reactor = rendering.FilledPolygon([(l, b), (l, t), (r, t), (r, b)])
        reactor.set_color(*blackbody_color(self.r.tk + 800))
        self.reactrans = rendering.Transform()
        reactor.add_attr(self.reactrans)
        self.viewer.add_geom(reactor)

        l += sep_space
        t = self.s.level 
        r = sep_space + self.s.pt / 100.
        separator = rendering.FilledPolygon([(l, b), (l, t), (r, t), (r, b)])
        separator.set_color(*blackbody_color(self.s.tk + 600))
        self.septrans = rendering.Transform()
        reactor.add_attr(self.septrans)
        self.viewer.add_geom(separator)
        
        return self.viewer.render(return_rgb_array=mode == "rgb_array")

    def close(self):
        if self.viewer:
            self.viewer.close()
            self.viewer = None

if __name__ == "__main__":

    gym.envs.registration.register(
        id="TennesseeEastmann-v1",
        entry_point="__main__:TEproc",
        max_episode_steps=200,
        reward_threshold=195.0
    )

    env = gym.make("TennesseeEastmann-v1")
    red = ThreatAgent()
    blue = TEprobManager()
    observation = env.reset()
    blue_action = [Action.CONTINUE] * 12
    red_action = None
    action = (blue_action, red_action)
    #TODO:
    #select random threat agent (random loss function)
    #with open(f):
    for t in range( 48*3600):
        observation, reward, done, info = env.step(action)
        print(f"{reward=}")
        #f.write(info)
        env.render()
        #print(observation)
        # if model.predict(observation) == failure:
        #     env.reset()
        #     reward -= 24*3600
        xmeas, xmv = observation[0], observation[1]
        print("reactor P, t, and control value: ", env.r.pg, env.r.tc, env.ctrlr.xmv[3])
        if done:
            print("Episode finished after {} timesteps".format(t+1))
            break
    env.close()