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

    Blue team actions

    [0..11] => reset PLC 0-11 (TEproc will resort to open-loop for that PLC for one hour)
    12 => restart entire plant (no production for 24 hours)
    13 => continue (no action, no reward)

    Red team actions

    [0..11] => set xmv[i] to MAX
    [12..53] => set xmeas[i-12] to 0.
    [54..62] => setpt[i-54] to HUGE_VAL
    63 => no action

===============================================================================
"""

from argparse import Action as ArgAction, ArgumentParser, RawTextHelpFormatter
from agent import DummyAgent
from blue import BlueAgent
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
from red import RedAgent
from teprob import TEproc
from report import make_figures, make_report

# from sense import Sensors
from statistics import mode
import sys

np.seterr(all="raise")

DELTA_t = 1.0 / 3600.0

log = []

##################################################################################################
# ArgParse preamble
##################################################################################################

description = """
    Tennessee Eastmann Adversarial Control Challenge - Single Continunous Control version.
    Scenarios:
    "default" [default] - red team attempts to maximise downtime
    "chaos" - red team acts at random
    "nored" - red team takes no action
"""

action_txt = """
    Blue team actions

        [0..11] => reset PLC 0-11 (TEproc will resort to open-loop for that PLC for one hour)
        12 => restart entire plant (no production for 24 hours)
        13 => continue (no action, no reward)

    Red team actions

        [0..11] => set xmv[i] to MAX
        [12..53] => set xmeas[i-12] to 0.
        [54..62] => setpt[i-54] to HUGE_VAL
        63 => no action

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
group = parser.add_mutually_exclusive_group()
group.add_argument("--nored", help="no red team actions", action="store_true")
group.add_argument("--noblue", help="no blue team actions", action="store_true")
group.add_argument("--peaceful", help="no blue or red team actions", action="store_true")

parser.add_argument("--render", help="live visualisations (slow)", action="store_true")
parser.add_argument("--report", help="generates report template", action="store_true")
parser.add_argument(
    "-v", "--verbose", help="displays debug info", action="count", default=0
)

if __name__ == "teprob":
    gym.envs.registration.register(
        id="TennesseeEastmann-v1",
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
        id="TennesseeEastmann-v1",
        entry_point="__main__:TEproc",
        max_episode_steps=int(48 * 3600),
        reward_threshold=195.0,
    )

    env = gym.make("TennesseeEastmann-v1", red_intent=args.intent)
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
        blue = BlueAgent()
        red = RedAgent()

    observations, _, __, ___ = env.reset()
    blue_action = blue.get_action(observations[0][1:])
    red_action = red.get_action(observations[1][1:])
    action = (blue_action, red_action)

    for i in range(args.num_episodes):
        env.reset()
        episode_memory = []
        for t in range(env._max_episode_steps):
            prev_obs = observations
            blue_action = blue(prev_obs[0][1:])
            red_action = red(prev_obs[1][1:])
            action = (blue_action, red_action)
            if args.verbose >= 1 and "--peaceful" not in sys.argv:
                print(blue.encode(action[0]), red.encode(action[1]))
            observations, rewards, done, info = env.step(action)
            blue_obs = observations[0]
            red_obs = observations[1]
            blue_reward = rewards[0]
            red_reward = rewards[1]
            blue.remember(prev_obs[1][1:], blue_action, blue_reward, blue_obs[1:], done)
            red.remember(prev_obs[0][1:], red_action, red_reward, red_obs[1:], done)
            if args.render:
                env.render()
            if args.report and i % 10 == 0:
                episode_memory.append(
                    {
                        "episode": i,
                        "time": t,
                        "blue action": blue_action,
                        "red action": red_action,
                        "true reactor pressure": env.r.pg,
                        "true reactor temperature": env.r.tc,
                        "reported reactor pressure": blue_obs[7],
                        "reported reactor temperature": blue_obs[9],
                        "true separator temperature": env.s.tc,
                        "true separator level": env.s.level,
                        "reported separator temperature": blue_obs[11],
                        "reported separator level": blue_obs[12],
                        "compressor cycles": env.cmpsr.cycles,
                    }
                )
            if args.verbose >= 1:
                print(
                    f"time = {env.time}: reactor P, T, PVs = {env.r.pg}, {env.r.tc}, {info['failures']}"
                )
                print(f"{blue_reward=}, {red_reward=}")
            if done:
                print(
                    f"Episode {i} finished after {t/3600.:1f} hrs ({t} timesteps): "
                    + (
                        f"red team wins: {info['failures']}"
                        if info["failures"]
                        else "blue team wins"
                    )
                )
                wins.append(True if info["failures"] else False)
                if args.report and i % 10 == 0:
                    if wins:
                        try:
                            win_rate = sum(1 for w in wins if w) / 10
                        except ZeroDivisionError:
                            win_rate = 0
                    else:
                        win_rate = "n/a"
                    blue_modal = blue.encode(
                        mode([i["blue action"] for i in episode_memory])
                    )
                    red_modal = red.encode(
                        mode([i["red action"] for i in episode_memory])
                    )
                    summary.append(
                        f"blue team win rate from last ten episodes: {win_rate}\n\n"
                        + f"last failure condition: {info['failures']}\n\n"
                        + f"most common blue team action: {blue_modal}\n\n"
                        + f"most common red team action: {red_modal}\n\n"
                    )
                break
        env.close()
        blue_loss = blue.learn()
        red_loss = red.learn()
        if args.report and i % 10 == 0:
            make_figures(episode_memory, i, d)
            losses.append((blue_loss, red_loss))

    ###############################################################################################
    # Report generation
    ###############################################################################################

    # TODO: and add a timed-out opportunity to write closing remarks
    if args.report:
        make_report(d, action_txt, args.intent)
