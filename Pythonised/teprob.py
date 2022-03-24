#!/usr/bin/env python3
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
    xmv[9]    reactor cooling water flow
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

    Red team action

    adjust xmeas[7]

    Blue team action

    adjust xmv[3]

===============================================================================
"""
from argparse import Action as ArgAction, ArgumentParser, RawTextHelpFormatter
from red import SCENARIOS
from agent import DummyAgent
from blue import DefendAgent
from collections import deque
from colorpy.blackbody import blackbody_color
import control
from constants import *
from copy import deepcopy
from datetime import datetime
import gym
from gym import spaces
from gym.envs.classic_control import rendering
from matplotlib import pyplot as plt
import numpy as np
from os import system
from random import choice  # , uniform
from red import ThreatAgent
from statistics import mode
import sys

np.seterr(all="raise")

DELTA_t = 1.0 / 3600.0

log = []


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
        return f"{self.fcm}"


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
        return f"{self.x}, {self.T}\n"


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
        return f"{self.id}, {self.pos}\n"

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

    def __str__(self):
        return f"R: {[*self.ucv[0:3], *self.ucl[3:8], self.et]}\n"

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
        ua = (
            uarlev
            * (-0.5 * agtatr.speed**2 + 2.75 * agtatr.speed - 2.5)
            * 855490.0e-6
        )
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

    def __str__(self):
        return f"S: {[*self.ucv[0:3], *self.ucl[3:8], self.et]}\n"

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
        return f"{[*self.ucl, self.et]}\n"

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
        return f"{self.ucv}, {self.et}\n"

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
        return f"{self.fcm}\n"

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


class TEproc(gym.Env):
    def __init__(self, red_intent, open_control=False):
        # TODO:
        # valve_stick loop
        seed = deepcopy(
            {
            # fmt: off
                "R": [10.40491389, 4.363996017, 7.570059737, 0.4230042431, 24.15513437, 2.942597645, 154.3770655, 159.186596, 2.808522723],
                "S": [63.75581199, 26.74026066, 46.38532432, 0.2464521543, 15.20484404, 1.852266172, 52.44639459, 41.20394008, 0.569931776],
                "C": [0.4306056376, 0.0079906200783, 0.9056036089, 0.016054258216, 0.7509759687, 0.088582855955, 48.27726193, 39.38459028, 0.3755297257],
                "V": [107.7562698, 29.77250546, 88.32481135, 23.03929507, 62.85848794, 5.546318688, 11.92244772, 5.555448243, 0.9218489762],
                "Twr": 94.59927549,
                "Tws": 77.29698353,
                "vpos": [63.05263039, 53.97970677, 24.64355755, 61.30192144, 22.21, 40.06374673, 38.1003437, 46.53415582, 47.44573456, 41.10581288, 18.11349055, 50.0],
                "vrng": [400.00, 400.00, 100.00, 1500.00, None, None, 1500.00, 1000.00, 0.03, 1000.0, 1200.0, None],
                "vtau": [8.0, 8.0, 6.0, 9.0, 7.0, 5.0, 5.0, 5.0, 120.0, 5.0, 5.0, 5.0],
                "sfr": [0.995, 0.991, 0.99, 0.916, 0.936, 0.938, 0.058, 0.0301],
                # D feed
                0: {
                    "x": [0.0, 0.0001, 0.0, 0.9999, 0.0, 0.0, 0.0, 0.0],
                    "T": 45.0,
                },
                # E feed
                1: {
                    "x": [0.0, 0.0, 0.0, 0.0, 0.9999, 0.0001, 0.0, 0.0],
                    "T": 45.0,
                },
                # A feed
                2: {
                    "x": [0.9999, 0.0001, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                    "T": 45.0,
                },
                # A and C feed
                3: {
                    "x": [0.4850, 0.0050, 0.5100, 0.0, 0.0, 0.0, 0.0, 0.0],
                    "T": 45.0,
                },
                # fmt: on
            }
        )

        self.time = 0.0
        self.T_GAS = 0.1
        self.T_PROD = 0.25
        self.time_since_gas = 0.0
        self.time_since_prod = 0.0
        self.r = Reactor(seed["R"])
        self.s = Separator(seed["S"])
        self.c = Stripper(seed["C"])
        self.j = Junction(seed["V"])
        self.sm = (
            [
                FeedStream(seed[0]),
                FeedStream(seed[1]),
                FeedStream(seed[2]),
                FeedStream(seed[3]),
            ]
            + [FCMStream()]
            + [Stream() for _ in range(5)]
            + [LiquidStream()]
            + [FCMLiquidStream()]
            + [LiquidStream()]
        )
        self.sfr = Sfr(seed["sfr"])
        self.valves = [
            Valve(i, p, r, t)
            for i, p, r, t in zip(
                list(range(12)), seed["vpos"], seed["vrng"], seed["vtau"]
            )
        ]
        self.cmpsr = Compressor()
        self.agtatr = Agitator()

        if open_control:
            self.ctrlr = control.Dummy()
        else:
            self.ctrlr = control.Controller(seed["vpos"], delta_t=DELTA_t)
        # stub for if we implement Sensors as a separate module
        # self.sensors = Sensors()
        self.blue_action_space = spaces.Box(low=0.0, high=100.0, shape=(1,))
        self.red_action_space = spaces.Box(low=-np.inf, high=np.inf, shape=(1,))
        self.red_intent = red_intent

        self.faults = [0] * 20
        self.xmeas = None

    def __str__(self) -> str:
        """
        Convenience representation function. Should be harmonised with the Fortran output.
        """
        return f"{sum(self.measure()[40:])}"

    def step(
        self, action: tuple[np.ndarray, np.ndarray]
    ) -> tuple[tuple[np.ndarray, np.ndarray], tuple[float, float], bool, dict]:
        """
        main loop for the TE process
        """

        global log
        reset = False

        ###########################################################################################
        # Control
        ###########################################################################################

        xmv = self.ctrlr.control(self.xmeas, self.time)

        blue_action, red_action = (action[0], action[1])
        if blue_action is not None:
            assert self.blue_action_space.contains(blue_action), blue_action
        if red_action is not None:
            assert self.red_action_space.contains(red_action), red_action
        if blue_action is not None:
            self.ctrlr.xmv[3] += blue_action[0]

        # setting valves
        for mv, valve in zip(xmv, self.valves):
            valve.set(mv)

        ###########################################################################################
        # Reaction / process
        ###########################################################################################

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
        self.sm[2].ftm = self.valves[2].flow()  # A feed
        self.sm[0].ftm = self.valves[0].flow()  # D feed
        self.sm[1].ftm = self.valves[1].flow()  # E feed
        self.sm[3].ftm = self.valves[3].flow()  # A and C feed
        self.sm[10].ftm = self.valves[6].flow()  # separator underflow
        self.sm[12].ftm = self.valves[7].flow()  # stripper underflow
        self.r.cl.flow = self.valves[9].flow()
        self.s.cl.flow = self.valves[10].flow()
        self.agtatr.speed = (self.valves[11].pos + 150.0) / 100.0

        delta_p = max(self.j.pt - self.r.pt, 0.0)
        flms = 1937.6 * np.sqrt(delta_p)
        self.sm[5].ftm = (
            flms / self.sm[5].xmws
        )  # volume flow per time / m weight = moles / s / density?

        delta_p = max(self.r.pt - self.s.pt, 0.0)  # reactor - separator
        flms = 4574.21 * np.sqrt(delta_p)
        self.sm[7].ftm = flms / self.sm[7].xmws  # mass flow = volume / mean mass?

        delta_p = self.s.pt - 760.0  # sep - atmosphere
        flms = self.valves[5].pos * 0.151169 * np.sqrt(abs(delta_p)) * np.sign(delta_p)
        self.sm[9].ftm = flms / self.sm[9].xmws

        pr = np.clip(self.j.pt / self.s.pt, 1.0, self.cmpsr.max_PR)
        flcoef = self.cmpsr.max_flow / 1.197
        flms = self.cmpsr.max_flow + flcoef * (1.0 - pr**3)
        #   later conversion implies this is in BTU
        self.cmpsr.set_work(flms, self.s, self.j, self.sm[8])

        delta_p = max(self.j.pt - self.s.pt, 0.0)
        flms = max(flms - self.valves[4].pos * 53.349 * np.sqrt(delta_p), 1.0e-3)
        self.sm[8].ftm = flms / self.sm[8].xmws
        self.sm[8].H += self.cmpsr.work / self.sm[8].ftm

        #   label 5020 abstracted into property

        self.sfr.set_fcm(self.c, self.sm)
        #   label 6010
        fin = self.sm[3].fcm + self.sm[10].fcm
        #   label 6020 and 6030
        self.sm[4].fcm = self.sfr.fcm * fin  # only place that consumes sfr
        self.sm[11].fcm = fin - self.sm[4].fcm
        self.sm[4].ftm = sum(self.sm[4].fcm)
        self.sm[11].ftm = sum(self.sm[11].fcm)
        self.sm[4].x = self.sm[4].fcm / self.sm[4].ftm
        self.sm[11].x = self.sm[11].fcm / self.sm[11].ftm

        self.sm[4].T = self.c.tc
        self.sm[11].T = self.c.tc
        self.sm[4].set_H()
        self.sm[11].set_H()

        self.sm[6] = deepcopy(self.sm[5])

        #   calculate cooling from water feeds
        self.r.set_heat_transfer(self.agtatr)
        self.s.set_heat_transfer(self.sm[7])
        self.c.set_heat_transfer(self.valves[8].flow())

        self.r.set_et(self.sm[6], self.sm[7], reaction_heat)
        self.s.set_et(self.sm[7], (self.sm[8], self.sm[9], self.sm[10]), self.cmpsr)
        self.c.set_et((self.sm[3], self.sm[10]), (self.sm[4], self.sm[12]))
        self.j.set_et(
            (self.sm[0], self.sm[1], self.sm[2], self.sm[4], self.sm[8]), self.sm[5]
        )
        #   twr and tws
        self.r.set_tw()  # delta_T in degC, 1.8 is F->C
        self.s.set_tw()

        # label 9010
        self.r.set_uc(self.sm[6], self.sm[7], delta_xr)
        self.s.set_uc(self.sm[7], (self.sm[8], self.sm[9], self.sm[10]))
        self.c.set_uc(self.sm[11], self.sm[12])
        self.j.set_uc(
            (self.sm[0], self.sm[1], self.sm[2], self.sm[4], self.sm[8]), self.sm[5]
        )

        ###########################################################################################
        # Measurement
        ###########################################################################################
        # xmeas = self.sensors.measure(self.sm, self.r, self.s, self.c, self.j, self.cmpsr, self.time, red_action)

        # fmt: off
        xmeas = np.zeros(43)
        xmeas[0] = self.time
        xmeas[1] = self.sm[2].ftm * 0.359 / 35.3145          # A Feed  (stream 1)                             kscmh
        xmeas[2] = self.sm[0].ftm * self.sm[0].xmws * 0.454  # D Feed  (stream 2)                             kg/hr from lbmol/hr
        xmeas[3] = self.sm[1].ftm * self.sm[1].xmws * 0.454  # E Feed  (stream 3)                             kg/hr
        xmeas[4] = self.sm[3].ftm * 0.359 / 35.3145          # A and C Feed  (stream 4)                       kscmh
        xmeas[5] = self.sm[8].ftm * 0.359 / 35.3145          # Recycle Flow  (stream 8)                       kscmh
        xmeas[6] = self.sm[5].ftm * 0.359 / 35.3145          # Reactor Feed Rate  (stream 6)                  kscmh
        xmeas[7] = self.r.pg                                 # Reactor Pressure                               kPa gauge
        xmeas[8] = self.r.level * 100.0                      # Reactor level                                  %
        xmeas[9] = self.r.tc                                 # Reactor Temperature                            deg C
        xmeas[10] = self.sm[9].ftm * 0.359 / 35.3145         # purge rate (stream 9)                          kscmh
        xmeas[11] = self.s.tc                                # product sep temp                               deg c
        xmeas[12] = self.s.level * 100.0                     # product sep level                              %
        xmeas[13] = (self.s.pt - 760.0) / 760.0 * 101.325    # sep pressure                                   kpa gauge
        xmeas[14] = (self.sm[10].ftm
                     / self.s.density
                     / 35.3145)                              # sep underflow (stream 10)                      m3/hr
        xmeas[15] = (self.c.vl - 78.25) / self.c.vt * 100.0  # stripper level                                 %
        xmeas[16] = (self.j.pt - 760.0) / 760.0 * 101.325    # stripper pressure                              kpa gauge
        xmeas[17] = (self.sm[12].ftm
                     / self.c.density
                     / 35.3145)                              # stripper underflow (stream 11, aka production) m3/hr
        xmeas[18] = self.c.tc                                # stripper temperature                           deg c
        xmeas[19] = self.c.qu * 1.04e3 * 0.454               # stripper steam flow                            kg/hr
        xmeas[20] = self.cmpsr.work * 0.29307e3              # compressor work, again??                       kwh
        xmeas[21] = self.r.cl.T_out                          # reactor cooling water outlet temp              deg c
        xmeas[22] = self.s.cl.T_out                          # separator cooling water outlet temp            deg c
        # fmt: on

        if idv(16):
            if xmeas_tgt == 0:
                xmeas_tgt = np.ceiling(rand() * 42.0)

            if time > 0.012 and time < 0.027:
                xmeas[xmeas_tgt] = 0.0

        if idv(21):
            if time > 0.012 and time < 0.027:
                xmeas[9] = 500.0

        if self.time_since_gas >= self.T_GAS or not hasattr(
            self, "gas"
        ):  # purge gas and reactor feed analysis
            self.gas = (self.sm[5].x[:-2], self.sm[8].x)
            self.time_since_gas = 0.0

        xmeas[23:29] = self.gas[0] * 100
        xmeas[29:37] = self.gas[1] * 100

        if self.time_since_prod >= self.T_PROD or not hasattr(
            self, "prod"
        ):  # product feed analysis
            self.prod = self.sm[12].x[3:]
            self.time_since_prod = 0.0
        xmeas[37:42] = self.prod * 100
        self.time_since_prod += DELTA_t

        # G/H ratio as a convenience measurement
        xmeas[42] = (xmw[6] * xmeas[40]) / (xmw[7] * xmeas[41])

        # Final red action: alter measured values. Red team still gets the real ones.
        red_xmeas = xmeas
        blue_xmeas = xmeas

        if red_action is not None:
            blue_xmeas[7] += red_action[0]

        self.xmeas = blue_xmeas

        ###########################################################################################
        # Cleanup and return
        ###########################################################################################

        done = self.has_failed(xmeas, self.time)
        blue_reward = self.reward(reset, done, xmeas, self.ctrlr.xmv)
        if self.red_intent == "downtime":
            red_reward = -self.downtime(reset)
        elif self.red_intent == "recipe":
            red_reward = -(self.production(red_xmeas) - self.utilities(red_xmeas))
        elif self.red_intent == "destruction":
            self.red_intent = -self.mechanical(red_xmeas)
        self.time += DELTA_t
        self.time_since_gas += DELTA_t
        return (blue_xmeas, red_xmeas), (blue_reward, red_reward), bool(done), {"failures": done}

    def reward(self, reset, failed, true_xmeas, xmv):
        return sum(
            [
                self.production(true_xmeas),
                self.downtime(reset),
                self.mechanical(true_xmeas),
                self.environmental(true_xmeas),
                self.utilities(true_xmeas),
            ]
        )

    def downtime(self, reset):
        """
        Expected downtime loss per hour
        """
        PRODUCTIVITY_HR = 24_000
        return -24 * PRODUCTIVITY_HR if reset else 0

    def utilities(self, true_xmeas):
        """
        cost of compressor work and steam (inflation adjusted from 1993)
        """
        COST_KWH = 0.1
        COST_STEAM = 0.065
        return -(true_xmeas[20] * COST_KWH + true_xmeas[19] * COST_STEAM)

    def production(self, true_xmeas):
        """
        value of product
        """
        G_H_LOWER = 0.95
        G_H_UPPER = 1.05
        if G_H_LOWER < true_xmeas[42] < G_H_UPPER:
            return 20_000 * true_xmeas[17]
        else:
            return 0

    def mechanical(self, true_xmeas):
        R_MAX_STRESS = 12_000
        if true_xmeas[7] > R_MAX_STRESS:
            reward = -1e6
        else:
            reward = 0
        return reward

    def environmental(self, true_xmeas):
        """
        Assumed fines for any environmental damagage occurred, assumed to be excess G in purge
        """
        G_TOLERANCE = 0.01
        if true_xmeas[35] > G_TOLERANCE:
            reward = -1e3 * true_xmeas[10] * true_xmeas[35]
        else:
            reward = 0
        return reward

    @property
    def state(self):
        """
        layout of state vector:

            0                     time
            [1..3]                R.ucv
            [4..8]                R.ucl
            9                     R.et
            [10..12]              S.ucv
            [13..17]              S.ucl
            18                    S.et
            [19..26]              C.ucl
            27                    C.et
            [28..35]              V.ucv
            36                    V.et
            37                    twr
            38                    tws
            [39..50]              vpos
        """
        return np.array(
            [
                *self.r.ucv[:3],
                *self.r.ucl[3:],
                self.r.et,
                *self.s.ucv[:3],
                *self.s.ucl[3:],
                self.s.et,
                *self.c.ucl,
                self.c.et,
                *self.j.ucv,
                self.j.et,
                self.r.cl.T_out,
                self.s.cl.T_out,
                *[v.pos for v in self.valves],
            ]
        )

    def has_failed(self, xmeas, time):
        """
        Safety checks implemented by a functioning Safety Instrumented System (SIS)
        """
        # TODO: red team should be able to disable these
        if self.r.pg > 3000:
            return "Reactor pressure high"
        elif self.r.pg < 2700:
            return "Reactor pressure low"
        elif self.r.tc > 175.0:
            return "Reactor temp high"
        elif self.r.level > 1.144:
            return "Reactor level high"
        elif xmeas[8] < -2.0:
            return "Reactor level low"
        elif self.s.level > 1.37:
            return "Separator level high"
        elif self.s.level < 0.27:
            return "Separator level low"
        elif xmeas[15] > 131.0:
            return "Stripper level high"
        elif xmeas[15] < -2.7:
            return "Stripper level low"
        elif self.cmpsr.has_fatigued():
            return "compressor has fatigued"
        else:
            return False

    def has_failed_extra(self):
        """
        Extra sanity checks on floating points. Raises FloatingPointError if not true
        """
        for i in range(12):
            if self.sm[i].ftm < 0:
                raise FloatingPointError(f"Stream {i} below 0")
        if self.r.pg < 0:
            raise FloatingPointError("R pressure below 0")
        if self.r.tc < 0:
            raise FloatingPointError("R temperature below 0")
        if self.s.level < 0:
            raise FloatingPointError("S level below 0")

        """
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
        """

    def reset(self):
        """
        Resets the plant and burns in for one hour with no actions
        """
        print("#" * 80 + "\n\n  RESETTING  \n\n" + "#" * 80)
        self.__init__(self.red_intent)
        global log
        log = []
        try:
            try:
                for i in range(3600):
                    observations, _, done, info = env.step((None, None))
                    log.append([self.s.level * 100, self.ctrlr.xmv[10]])
                    self.has_failed_extra()
            except FloatingPointError as e:
                raise ProcessError(
                    f" plant failed after {i}/{self.time} timesteps due to numerical instability ({e})! this should never occur.",
                    log,
                )
            except AssertionError as e:
                raise ProcessError(
                    f" plant failed after {i}/{self.time} timesteps due to {e}!", log
                )
            return self.step([None, None])
        except ProcessError as e:
            exit()

    def render(self, mode="human"):
        """
        Rendering utility.
        Note: the base doesn't have the ability to scale x and y independently,
        hence the add_onetime
        """
        screen_width = 640
        screen_height = 480
        vessel_width = 50.0
        vessel_height = 30.0
        color_offset = 800.0
        sep_space = 200
        b = -vessel_height / 2

        if not hasattr(self, "viewer") and mode == "human":
            self.viewer = rendering.Viewer(screen_width, screen_height)

        l = -vessel_width / 2
        t = self.r.level * 200
        r = self.r.pg / 30.0
        reactor = rendering.FilledPolygon([(l, b), (l, t), (r, t), (r, b)])
        reactor.set_color(*blackbody_color(self.r.tk + 800))
        self.reactrans = rendering.Transform()
        reactor.add_attr(self.reactrans)
        self.viewer.add_onetime(reactor)

        l += sep_space
        t = self.s.level * 200
        r = sep_space + self.s.pg / 40.0
        separator = rendering.FilledPolygon([(l, b), (l, t), (r, t), (r, b)])
        separator.set_color(*blackbody_color(self.s.tk + 800))
        self.septrans = rendering.Transform()
        separator.add_attr(self.septrans)
        self.viewer.add_onetime(separator)

        l += sep_space
        t = self.c.level * 200
        r = (sep_space * 2) + 100.0
        stripper = rendering.FilledPolygon([(l, b), (l, t), (r, t), (r, b)])
        stripper.set_color(*blackbody_color(self.c.tk + 800))
        self.striptrans = rendering.Transform()
        stripper.add_attr(self.striptrans)
        return self.viewer.render(return_rgb_array=mode == "rgb_array")

    def close(self):
        if hasattr(self, "viewer"):
            self.viewer.close()
            self.viewer = None


description = """
    Tennessee Eastmann Adversarial Control Challenge - Single Continunous Control version.

    Scenarios:

    "default" [default] - red team attempts to maximise downtime
    "chaos" - red team acts at random
    "nored" - red team takes no action
"""

action_txt = """
    Red team action:   adjust xmeas[7]

    Blue team action:  adjust xmv[3]
"""

parser = ArgumentParser(
    description=description + "\n" + action_txt, formatter_class=RawTextHelpFormatter
)
parser.add_argument(
    "--fast", help="runs for fewer timesteps per episode", action="store_true"
)
parser.add_argument(
    "--intent",
    help="sets red team intent",
    default="downtime",
    choices=["downtime", "recipe", "destruction"],
)
parser.add_argument(
    "-n",
    "--num_episodes",
    help="number of episodes (default 100)",
    type=int,
    default=100,
)
parser.add_argument(
    "--peaceful",
    help="no red or blue team action (overrides scenario)",
    action="store_true",
)
parser.add_argument("--render", help="live visualisations (slow)", action="store_true")
parser.add_argument("--report", help="generates report template", action="store_true")
parser.add_argument("--scenario", help="select from scenarios:", default="default")
parser.add_argument(
    "-v", "--verbose", help="displays debug info", action="count", default=0
)

if __name__ == "teprob":
    gym.envs.registration.register(
        id="TennesseeEastmannContinuous-v1",
        entry_point="teprob:TEproc",
        max_episode_steps=int(48 * 3600),
        reward_threshold=195.0,
    )
elif __name__ == "__main__":

    args = parser.parse_args()
    d = str(datetime.now().date())

    if args.report:
        memory = []

    gym.envs.registration.register(
        id="TennesseeEastmannContinous-v1",
        entry_point="__main__:TEproc",
        max_episode_steps=int(48 * 3600),
        reward_threshold=195.0,
    )

    env = gym.make("TennesseeEastmannContinous-v1", red_intent = args.intent)
    if args.fast:
        env._max_episode_steps = 3600 + int(0.1 * 3600)

    # logging stuff
    wins = deque(maxlen=10)
    summary = []
    losses = []

    if args.peaceful:
        red, blue = DummyAgent(), DummyAgent()
        args.num_episodes = 3
    else:
        blue = DefendAgent()
        # TODO: plug in scenarios
        red = ThreatAgent()

    observations, _, __, ___ = env.reset()
    blue_action = None
    red_action = None
    actions = (blue_action, red_action)

    for i in range(args.num_episodes):
        env.reset()
        episode_memory = []
        for t in range(env._max_episode_steps):
            # separating observations. We also strip out time.
            blue_observation = observations[0][1:]
            red_observation = observations[1][1:]
            blue_action = blue.get_action(blue_observation.reshape(1, 42))[0]
            red_action = red.get_action(red_observation.reshape(1, 42))[0]
            blue_previous = blue_observation
            red_previous = red_observation
            actions = (blue_action, red_action)
            if args.verbose >= 1 and not args.peaceful:
                print(f"{blue_action=}, {red_action=}")
            observations, rewards, done, info = env.step(actions)
            red_observation = observations[0][1:]
            blue_obervation = observations[1][1:]
            blue_reward = rewards[0]
            red_reward = rewards[1]
            blue_loss = blue.learn(blue_previous, blue_reward, blue_observation, done)
            red_loss = red.learn(red_previous, red_reward, red_observation, done)
            if args.render:
                env.render()
            if args.report and i % 10 == 0:
                episode_memory.append(
                    {
                        "episodes": i,
                        "time": t,
                        "blue action": blue_action,
                        "red action": red_action,
                        "true reactor pressure": env.r.pg,
                        "true reactor temperature": env.r.tc,
                        "reported reactor pressure": blue_observation[7],
                        "reported reactor temperature": blue_observation[9],
                        "true separator temperature": env.s.tc,
                        "true separator level": env.s.level,
                        "reported separator temperature": blue_observation[11],
                        "reported separator level": blue_observation[12],
                        "compressor cycles": env.cmpsr.cycles
                    }
                )
            if args.verbose >= 1:
                print(
                    f"time = {env.time}: reactor P, T, PVs = {env.r.pg}, {env.r.tc}, {info['failures']}"
                )
                print(f"{blue_reward=}, {red_reward=}")
                print(f"{red_loss=}")
                print(f"{blue_loss=}")
            if done:
                print(
                    f"Episode {i} finished after {t/3600.:1f} hrs ({t} timesteps): "
                    + (
                        f"red team wins: {info['failures']}"
                        if info["failures"]
                        else "blue team wins"
                    )
                )
                wins.append((0, 1) if info["failures"] else (1, 0))
                if args.report and i % 10 == 0:
                    if wins:
                        try:
                            win_rate = sum(1 for w in wins if w[0]) / sum(
                                1 for w in wins if w[1]
                            )
                        except ZeroDivisionError:
                            win_rate = 1 if wins[0][0] else 0
                    else:
                        win_rate = "n/a"
                    summary.append(
                        f"blue team win rate from last ten episodes: {win_rate}\n\n"
                        + f"last failure condition: {info['failures']}\n\n"
                    )
                break
        env.close()
        if args.report and i % 10 == 0:
            fig, ax = plt.subplots()
            ax.plot(
                [m["blue action"] for m in episode_memory],
                label="blue team",
                color="blue",
            )
            ax.plot(
                [m["red action"] for m in episode_memory], label="red team", color="red"
            )
            ax.set_title(f"actions at episode {i}")
            ax.set_xlabel("time")
            ax.set_ylabel("actions")
            plt.legend()
            plt.savefig(f"actions_{d}_ep{i}.png")

            fig, ax1 = plt.subplots()
            ax1.plot(
                [m["true reactor pressure"] for m in episode_memory],
                label="real pressure",
                color="red",
            )
            ax1.plot(
                [m["reported reactor pressure"] for m in episode_memory],
                label="reported pressure",
                color="blue",
            )
            ax1.set_ylabel("pressure (kPag)")
            ax1.set_ylim(2700, 3000)
            ax2 = ax1.twinx()
            ax2.plot(
                [m["true reactor temperature"] for m in episode_memory],
                label="real temperature",
                color="red",
                linestyle="dashed",
            )
            ax2.plot(
                [m["reported reactor temperature"] for m in episode_memory],
                label="reported temperature",
                color="blue",
                linestyle="dashed",
            )
            ax2.set_ylabel("temperature (degC)")
            ax2.set_ylim(90, 180)
            ax2.set_title(f"reactor parameters at episode {i}")
            ax2.set_xlabel("time")
            fig.legend(bbox_to_anchor=(0.85, 0.9))
            fig.tight_layout()
            plt.savefig(f"r_parameters_{d}_ep{i}.png")

            fig, ax1 = plt.subplots()
            ax1.plot(
                [m["true separator temperature"] for m in episode_memory],
                label="real temperature",
                color="red",
            )
            ax1.plot(
                [m["reported separator temperature"] for m in episode_memory],
                label="reported temperature",
                color="blue",
            )
            ax2 = ax1.twinx()
            ax2.plot(
                [m["true separator level"] for m in episode_memory],
                label="real level",
                color="red",
                linestyle="dashed",
            )
            ax2.plot(
                [m["reported separator level"] for m in episode_memory],
                label="reported level",
                color="blue",
                linestyle="dashed",
            )
            ax2.set_title(f"separator parameters at episode {i}")
            ax2.set_xlabel("time")
            fig.legend(bbox_to_anchor=(0.85, 0.9))
            fig.tight_layout()
            plt.savefig(f"s_parameters_{d}_ep{i}.png")
            plt.close("all")

            losses.append((blue_loss, red_loss))


    ###############################################################################################
    # Report generation
    ###############################################################################################

    if args.report:
        with open(f"report_{d}.md", "w") as f:
            f.write(f"wargame of TE process generated on {d}\n===\n")
            f.write(action_txt + "\n\n")
            for i in range(10):
                f.write(
                    f"![Actions at episode {10*i}](actions_{d}_ep{10*i}.png){{margin=auto}}\n"
                )
                f.write(
                    f"![Reactor parameters at episode {10*i}](r_parameters_{d}_ep{10*i}.png){{margin=auto}}\n"
                )
                f.write(
                    f"![Separator parameters at episode {10*i}](s_parameters_{d}_ep{10*i}.png){{margin=auto}}\n"
                )
                # f.write(f"{summary[i]}\n\nblue and red training losses: {losses[i]}\n\\newpage")
            # f.write(input("closing remarks?"))
