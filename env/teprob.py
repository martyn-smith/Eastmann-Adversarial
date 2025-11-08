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


class TEproc(gym.Env):
    def __init__(self, blue_type, red_type, red_intent):
        self.te = cdll.LoadLibrary(f"{TELIB_PATH}/telib.so")
        self.te.init()

    def __str__(self) -> str:
        """
        Convenience representation function. Should be harmonised with the Fortran output.
        """
        return f"{self.state}"

    def step(
        self, action, burn_in=False
    ) -> tuple[tuple[np.ndarray, np.ndarray], tuple[float, float], bool, dict]:

        global log
        reset = False

        state = (c_double * len(state))(*state)
        time = c_double(1.0)

        self.te.step(
            byref(state), byref(c_int(len(state))), byref(derivate), byref(time)
        )

        # control?
