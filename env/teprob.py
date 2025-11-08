"""
==============================================================================
               tennessee eastman process control test problem

                    james j. downs and ernest f. vogel

                  process and control systems engineering
                        tennessee eastman company
                              p.o. box 511
                          kingsport,tn  37662

  reference:
    "a plant-wide industrial process control problem"
    presented at the aiche 1990 annual meeting
    industrial challenge problems in process control,paper #24a
    chicago,illinois,november 14,1990

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

  manipulated variables

    xmv[0]     a feed flow (stream 0) -> clearly should be different??
    xmv[1]     d feed flow (stream 1)
    xmv[2]     e feed flow (stream 2)
    xmv[3]     a and c feed flow (stream 3)
    xmv[4]     compressor recycle valve
    xmv[5]     purge valve (stream 8)
    xmv[6]     separator pot liquid flow (stream 9)
    xmv[7]     stripper liquid product flow (stream 10)
    xmv[8]     stripper steam valve
    xmv[9]     reactor cooling water flow
    xmv[10]    condenser cooling water flow
    xmv[11]    agitator speed

  continuous process measurements

    xmeas[1]   a feed  (stream 1)                    kscmh
    xmeas[2]   d feed  (stream 2)                    kg/hr
    xmeas[3]   e feed  (stream 3)                    kg/hr
    xmeas[4]   a and c feed  (stream 4)              kscmh
    xmeas[5]   recycle flow  (stream 8)              kscmh
    xmeas[6]   reactor feed rate  (stream 6)         kscmh
    xmeas[7]   reactor pressure                      kpa gauge
    xmeas[8]   reactor level                         %
    xmeas[9]   reactor temperature                   deg c
    xmeas[10]  purge rate (stream 9)                 kscmh
    xmeas[11]  product sep temp                      deg c
    xmeas[12]  product sep level                     %
    xmeas[13]  prod sep pressure                     kpa gauge
    xmeas[14]  prod sep underflow (stream 10)        m3/hr
    xmeas[15]  stripper level                        %
    xmeas[16]  stripper pressure                     kpa gauge
    xmeas[17]  stripper underflow (stream 11)        m3/hr
    xmeas[18]  stripper temperature                  deg c
    xmeas[19]  stripper steam flow                   kg/hr
    xmeas[20]  compressor work                       kw
    xmeas[21]  reactor cooling water outlet temp     deg c
    xmeas[22]  separator cooling water outlet temp   deg c

  sampled process measurements

    reactor feed analysis (stream 6)
        sampling frequency = 0.1 hr
        dead time = 0.1 hr
        mole %
    xmeas[23]   component a
    xmeas[24]   component b
    xmeas[25]   component c
    xmeas[26]   component d
    xmeas[27]   component e
    xmeas[28]   component f

    purge gas analysis (stream 9)
        sampling frequency = 0.1 hr
        dead time = 0.1 hr
        mole %
    xmeas[29]   component a
    xmeas[30]   component b
    xmeas[31]   component c
    xmeas[32]   component d
    xmeas[33]   component e
    xmeas[34]   component f
    xmeas[35]   component g
    xmeas[36]   component h

    product analysis (stream 11)
        sampling frequency = 0.25 hr
        dead time = 0.25 hr
        mole %
    xmeas[37]   component d
    xmeas[38]   component e
    xmeas[39]   component f
    xmeas[40]   component g
    xmeas[41]   component h

    extras (43 onwards - not implemented)

    xmeas[42]   g/h ratio
    xmeas[43]   cost
    xmeas[42]   production rate of G [kmol G generated/h]
    xmeas[43]   production rate of H [kmol H generated/h]
    xmeas[44]   production rate of F [kmol F generated/h]

  process disturbances

    idv(1)   a/c feed ratio, b composition constant (stream 4)          step
    idv(2)   b composition, a/c ratio constant (stream 4)               step
    idv(3)   d feed temperature (stream 2)                              step
    idv(4)   reactor cooling water inlet temperature                    step
    idv(5)   condenser cooling water inlet temperature                  step
    idv(6)   a feed loss (stream 1)                                     step
    idv(7)   c header pressure loss - reduced availability (stream 4)   step
    idv(8)   a, b, c feed composition (stream 4)            random variation
    idv(9)   d feed temperature (stream 2)                  random variation
    idv(10)  c feed temperature (stream 4)                  random variation
    idv(11)  reactor cooling water inlet temperature        random variation
    idv(12)  condenser cooling water inlet temperature      random variation
    idv(13)  reaction kinetics                                    slow drift
    idv(14)  reactor cooling water valve                            sticking
    idv(15)  condenser cooling water valve                          sticking
    idv(16)  random xmeas                                  Failure, 0.012 to 0.027 hr
    idv(17)  none
    idv(18)  none
    idv(19)  multiple valves stick
    idv(20)  none
    idv(21)  Reactor T (°C),                                 Integrity attack, 0.012 to 0.027 hr
    idv(22)  xmv 7, xmeas[14], xmeas[16]                     DDoS, 663 to 25019 hr
    idv(23)  D feed flow (mv(0))                             DDoS, 10 hr
    idv(24)  C feed (mv(3)), Purge flow (mv(5)), Stripper underflow (meas(16)),
             Stripper steam (xmeas[8])                       Noise, 7,727 to 71,291 h.

    Stream mappings

    sm[0]  D feed -> J
    sm[1]  E feed -> J
    sm[2]  A feed -> J
    sm[3]  A & C feed -> sm[11]
    sm[4]  C (Stripper) -> J (junction)
    sm[5]  J (junction) -> sm[6]
    sm[6]  sm[5] -> R (Reactor)
    sm[7]  R (Reactor) -> S (Separator)
    sm[8]  S (Separator) -> V (compressor?)
    sm[9] S (Separator) -> purge
    sm[10] S (Separator) -> sm[11]
    sm[11] sm[3] + S (Separator) -> C (Stripper)
    sm[12] C (Stripper) -> prod

===============================================================================
"""

from colorpy.blackbody import blackbody_color
import control
from ctypes import byref, c_double, c_int, cdll
from copy import deepcopy
import gym
from gym import spaces

# from gym.envs.classic_control import rendering
from matplotlib import pyplot as plt
import numpy as np
from random import choice  # , uniform

# from sense import Sensors
import sys

np.seterr(all="raise")

log = []

TELIB_PATH = "../Eastmann-95"


class ProcessError(Exception):
    """
    Catches a situation where the plant model has reached an implausible state
    due to modelling imperfections.
    (The modelled error checks may not catch an implausible state, since they represent
    checks on a physical plant).
    """

    def __init__(self, msg, log):
        print(f"Plant has reached an implausible state: {msg}")
        plt.plot([l[0] for l in log], label="s.level")
        plt.plot([l[1] for l in log], label="xmv[10]")
        plt.legend()
        plt.show()
        print("goodbye")

def idv(i):
    return 0

class Vessel:
    """
    Base class for any liquid-solid vessel.
    """

    @property
    def delta_H(self):
        """
        was tesub3
        dh = 0.
        for i in range(8):  #label 100...again
            dhi = 1.8 * (ah(i) + bh(i)*self.Tc + ch(i)*self.tc**2)
            dh += self.xl(i)*xmw(i)*dhi
        return dh
        """
        return sum(self.xl * xmw * (1.8 * (ah + bh * self.tc + ch * self.tc**2)))

    @property
    def H(self):
        """
        was tesub1. Split out for when it's used for vessels.
        H = 0.
        for i in range(8):  #label 100
            hi = 1.8 * self.Tc * (ah(i) + bh(i)*self.Tc/2. + ch(i)*self.Tc**2/3.)
            H += self.xl(i)*xmw(i)*hi
        return H
        """
        return sum(
            self.xl
            * xmw
            * (1.8 * self.tc * (ah + bh * self.tc / 2.0 + ch * self.tc**2 / 3.0))
        )

    @property
    def es(self):
        return self.et / self.utl

    @property
    def pg(self):
        """
        converts vessel pressure to mmHgg (!) then to atmg then to kPag
        """
        return (self.pt - 760.0) / 760.0 * 101.325

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
        v = sum(self.xl * xmw / (ad + (bd + cd * self.tc) * self.tc))
        self.density = 1.0 / v

    def set_P(self):
        self.pp = np.zeros(8)
        # label 1110 - for R and S only
        for i in range(3):
            self.pp[i] = self.ucv[i] * rg * self.tk / self.vv
        # label 1120 - for R and S only
        for i in range(3, 8):
            # Antoine eq.
            self.pp[i] = self.xl[i] * np.exp(avp[i] + bvp[i] / (self.tc + cvp[i]))
        self.pt = sum(self.pp)  # in mmHga (!)
        # label 1130
        self.xv = self.pp / self.pt
        self.utv = self.pt * self.vv / rg / self.tk
        for i in range(3, 8):  # label 1140
            self.ucv[i] = self.utv * self.xv[i]

    def set_T(self):
        """
        Iterative temperature calculation routine.

        note: this slightly weird way of doing it is necessary so H, dH,
              es are correct
        """
        if not hasattr(self, "tc"):
            self.tc = 0.0
        T_in = self.tc
        # label 250
        for i in range(100):
            err = self.H - self.es
            dh = self.delta_H
            dT = -err / dh
            self.tc += dT  # main mutator of T
            if abs(dT) < 1.0e-12:
                return
        raise FloatingPointError("failed to converge")
        self.tc = T_in

    def __str__(self):
        out = ""
        for u in self.ucv[:4]:
            out += f"  {u:.15E}"
        for u in self.ucl[4:]:
            out += f"  {u:.15E}"
        out += f"  {self.et:.15E}"
        return out


class GasVessel(Vessel):
    @property
    def delta_H(self):
        return (
            sum(self.xv * xmw * 1.8 * (ag + bg * self.tc + cg * self.tc**2))
            - 3.57696e-6
        )

    @property
    def H(self):
        return (
            sum(
                self.xv
                * xmw
                * (
                    1.8 * self.tc * (ag + bg * self.tc / 2.0 + cg * self.tc**2 / 3.0)
                    + av
                )
            )
            - 3.57696e-6 * self.tk
        )

    @property
    def es(self):
        return self.et / self.utv

    @property
    def utv(self):
        return sum(self.ucv)

    @property
    def xv(self):
        return self.ucv / self.utv

    def set_P(self):
        self.pt = self.utv * rg * self.tk / self.vt  # P = n R T / V


class Stream:
    # all streams weirdness:
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
        self.H = sum(
            self.x
            * xmw
            * (1.8 * self.T * (ag + bg * self.T / 2.0 + cg * self.T**2 / 3.0) + av)
        )

    def __str__(self):
        return f"  {self.fcm}"


class FeedStream(Stream):
    def __init__(self, seed):
        self.x = np.array(seed["x"])
        self.T = seed["T"]


class FCMStream:
    # stream 4 only
    def __init__(self, seed=None):
        if seed is not None:
            self.x = np.array(seed["x"])
            self.T = seed["T"]

    # inherits only this from regular streams
    def set_H(self):
        self.H = sum(
            self.x
            * xmw
            * (1.8 * self.T * (ag + bg * self.T / 2.0 + cg * self.T**2 / 3.0) + av)
        )

    def __str__(self):
        return f"  {self.x:.15E}  {self.T:.15E}"


class FCMLiquidStream:
    # stream 11 only

    # inherits only this from LiquidStream
    def set_H(self):
        self.H = sum(
            self.x
            * xmw
            * (1.8 * self.T * (ah + bh * self.T / 2.0 + ch * self.T**2 / 3.0))
        )


class LiquidStream(Stream):
    # streams 10, 12

    def set_H(self):
        self.H = sum(
            self.x
            * xmw
            * (1.8 * self.T * (ah + bh * self.T / 2.0 + ch * self.T**2 / 3.0))
        )


class Valve:
    def __init__(self, id, pos, rng, tau):
        self.id = id
        self.pos = pos
        self.rng = rng
        self.tau = tau / 3600.0

    def __str__(self):
        return f"  {self.pos:.15E}"

    def fail(self):
        return False

    def is_stuck(self):
        return False

    def flow(self):
        if self.fail():
            return 0.0
        return self.pos * self.rng / 100.0  # * uniform(0.95, 1.05)

    def set(self, mv):
        derivative = (mv - self.pos) / self.tau
        if not self.is_stuck():
            self.pos += derivative * DELTA_t


class Agitator:
    def __init__(self):
        self.speed = 0.0


class Coolant:
    def __init__(self, h, T_in, T_out):
        self.h = h
        self.T_in = T_in
        self.T_out = T_out

    def __str__(self):
        return f"  {self.T_out:.15E}"


class Sensor:
    def __init__(self, period):
        self.period = period


class Reactor(Vessel):
    def __init__(self, seed):
        self.vt = 1300.0
        self.cl = Coolant(h=7060.0, T_in=35.0, T_out=94.59927549)
        self.ucv = np.array([seed[0], seed[1], seed[2], 0.0, 0.0, 0.0, 0.0, 0.0])
        self.ucl = np.array(
            [0.0, 0.0, 0.0, seed[3], seed[4], seed[5], seed[6], seed[7]]
        )
        self.et = seed[8]

    @property
    def level(self):
        return (self.vl - 84.6) / 666.7

    def set_et(self, in_stream, out_stream, reaction_heat):
        derivative = (
            (in_stream.H * in_stream.ftm)
            - (out_stream.H * out_stream.ftm)
            + reaction_heat
            + self.qu
        )
        self.et += derivative * DELTA_t

    def set_heat_transfer(self, agtatr):
        uarlev = np.clip((1.0 / 312.0) * (self.vl - 78.0), 0.0, 1.0)
        ua = uarlev * (-0.5 * agtatr.speed**2 + 2.75 * agtatr.speed - 2.5) * 855490.0e-6
        self.qu = ua * (self.cl.T_out - self.tc)

    def react(self):
        reaction_rate = np.zeros(4)
        # A + C + D -> G
        reaction_rate[0] = 5.219217002265e13 * np.exp(-40.0 / (1.987e-3 * self.tk))
        # A + D + E -> H
        reaction_rate[1] = 20.27525952163 * np.exp(-20.0 / (1.987e-3 * self.tk))
        # A + E -> F
        reaction_rate[2] = 1.5629689117665e23 * np.exp(-60.0 / (1.987e-3 * self.tk))
        # 3D -> 2F
        reaction_rate[3] = reaction_rate[2] * 0.767488334
        if self.pp[0] > 0.0 and self.pp[2] > 0.0:
            rf = (
                self.pp[0] ** 1.1544 * self.pp[2] ** 0.3735
            )  # ??? rr[1] should not be dependent on C
            reaction_rate[0] *= rf * self.pp[3]  # D dependence
            reaction_rate[1] *= rf * self.pp[4]  # E dependence
        else:
            reaction_rate[0] = 0.0
            reaction_rate[1] = 0.0

        reaction_rate[2] *= self.pp[0] * self.pp[4]  # E dependence
        reaction_rate[3] *= self.pp[0] * self.pp[3]  # D dependence

        reaction_rate *= self.vv

        #   consumption / generation
        delta_xr = np.zeros(8)
        delta_xr[0] = (
            -reaction_rate[0] - reaction_rate[1] - reaction_rate[2]
        )  # A consumption
        delta_xr[2] = (
            -reaction_rate[0] - reaction_rate[1]
        )  # C consumption.  Should be by rr(1) only!?
        delta_xr[3] = (
            -reaction_rate[0] - 1.5 * reaction_rate[3]
        )  # D consumed by rr(1), rr(2), rr(4)
        delta_xr[4] = -reaction_rate[1] - reaction_rate[2]  # E consumed by rr(2), rr(3)
        delta_xr[5] = reaction_rate[2] + reaction_rate[3]  # F created by rr(3), rr(4)
        delta_xr[6] = reaction_rate[0]  # A + C + D -> G
        delta_xr[7] = reaction_rate[1]  # A + D + E -> H
        reaction_heat = reaction_rate[0] * htr[0] + reaction_rate[1] * htr[1]
        return delta_xr, reaction_heat

    def set_tw(self):
        derivative = (
            self.cl.flow * 500.53 * (self.cl.T_in - self.cl.T_out)
            - self.qu * 1.0e6 / 1.8
        ) / self.cl.h
        self.cl.T_out += derivative * DELTA_t

    def set_uc(self, in_stream, out_stream, delta_xr):
        derivative = in_stream.fcm - out_stream.fcm + delta_xr
        self.ucv[:3] += np.array([*(derivative[:3] * DELTA_t)])
        self.ucl += np.array([*(np.zeros(3)), *(derivative[3:] * DELTA_t)])


class Separator(Vessel):
    def __init__(self, seed):
        self.vt = 3500.0
        self.cl = Coolant(h=11138.0, T_in=40.0, T_out=77.29698353)
        self.ucv = np.array([seed[0], seed[1], seed[2], 0.0, 0.0, 0.0, 0.0, 0.0])
        self.ucl = np.array(
            [0.0, 0.0, 0.0, seed[3], seed[4], seed[5], seed[6], seed[7]]
        )
        self.et = seed[8]

    def set_et(self, in_stream, out_streams, cmpsr):  # out => 8,9,10
        derivative = (
            (in_stream.H * in_stream.ftm)
            - (out_streams[0].H * out_streams[0].ftm - cmpsr.work)
            - out_streams[1].H * out_streams[1].ftm
            - out_streams[2].H * out_streams[2].ftm
            + self.qu
        )
        self.et += derivative * DELTA_t

    def set_heat_transfer(self, sm):
        ua = 0.404655 * (1.0 - 1.0 / (1.0 + (sm.ftm / 3528.73) ** 4))
        self.qu = ua * (self.cl.T_out - sm.T)

    def set_tw(self):
        derivative = (
            self.cl.flow * 500.53 * (self.cl.T_in - self.cl.T_out)
            - self.qu * 1.0e6 / 1.8
        ) / self.cl.h
        self.cl.T_out += derivative * DELTA_t

    def set_uc(self, in_stream, out_streams):
        """
        inputs 7,8,9,10
        """
        derivative = in_stream.fcm - sum(sm.fcm for sm in out_streams)
        self.ucv += np.array([*(derivative[:3] * DELTA_t), *np.zeros(5)])
        self.ucl += np.array([*np.zeros(3), *(derivative[3:] * DELTA_t)])

    @property
    def level(self):
        return (self.vl - 27.5) / 290.0


class Stripper(Vessel):
    def __init__(self, seed):
        self.vt = 156.5
        self.ucv = np.zeros(3)
        self.ucl = np.array(seed[:-1])
        self.et = np.array(seed[-1])

    def __str__(self):
        out = ""
        for u in self.ucl:
            out += f"  {u:.15E}"
        out += f"  {self.et:.15E}"
        return out

    def set_et(self, in_streams, out_streams):
        derivative = (
            sum(sm.H * sm.ftm for sm in in_streams)
            - sum(sm.H * sm.ftm for sm in out_streams)
            + self.qu
        )
        self.et += derivative * DELTA_t

    def set_heat_transfer(self, flow):
        ua = flow
        self.qu = 0.0
        if self.tc < 100.0:
            self.qu = ua * (100.0 - self.tc)

    def set_uc(self, in_stream, out_stream):
        derivative = in_stream.fcm - out_stream.fcm
        # self.ucv += derivative[:3] * DELTA_t
        self.ucl += derivative * DELTA_t

    @property
    def level(self):
        return (self.vl - 78.25) / self.vt


class Compressor(Vessel):
    def __init__(self):
        self.max_flow = 280275.0
        self.max_PR = 1.3
        self.cycles = 0.0
        self.max_cycles = 1.0e6
        self.work = 0.0

    def set_work(self, flms, s, v, sm):
        work = flms * (s.tk) * 1.8e-6 * 1.9872 * (v.pt - s.pt) / (sm.xmws * s.pt)
        delta = abs(work - self.work)
        self.work = work
        self.cycles += delta / 100.0

    def has_fatigued(self):
        return self.cycles > self.max_cycles


class Junction(GasVessel):
    def __init__(self, seed):
        self.vt = 5000.0
        self.ucv = np.array(seed[:-1])
        self.et = seed[-1]

    def __str__(self):
        out = ""
        for u in self.ucv:
            out += f"  {u:.15E}"
        out += f"  {self.et:.15E}"
        return out

    def set_et(self, in_streams, out_stream):
        derivative = sum(sm.H * sm.ftm for sm in in_streams) - (
            out_stream.H * out_stream.ftm
        )
        self.et += derivative * DELTA_t

    def set_uc(self, in_streams, out_stream):
        derivative = sum(sm.fcm for sm in in_streams) - out_stream.fcm
        self.ucv += derivative * DELTA_t


class Sfr:
    def __init__(self, seed):
        self.fcm = np.array(seed)

    def __str__(self):
        return f"  {self.fcm:.15E}"

    @property
    def ftm(self):
        return sum(self.fcm)

    def set_fcm(self, c, sm):
        if sm[10].ftm > 0.1:
            if c.tc > 170.0:
                tmpfac = c.tc - 120.262
            elif c.tc < 5.292:
                tmpfac = 0.1
            else:
                tmpfac = 363.744 / (177.0 - c.tc) - 2.22579488
            vovrl = sm[3].ftm / sm[10].ftm * tmpfac
            self.fcm[3] = 8.5010 * vovrl / (1.0 + 8.5010 * vovrl)
            self.fcm[4] = 11.402 * vovrl / (1.0 + 11.402 * vovrl)
            self.fcm[5] = 11.795 * vovrl / (1.0 + 11.795 * vovrl)
            self.fcm[6] = 0.0480 * vovrl / (1.0 + 0.0480 * vovrl)
            self.fcm[7] = 0.0242 * vovrl / (1.0 + 0.0242 * vovrl)
        else:
            self.fcm[3] = 0.9999
            self.fcm[4] = 0.999
            self.fcm[5] = 0.999
            self.fcm[6] = 0.99
            self.fcm[7] = 0.98
