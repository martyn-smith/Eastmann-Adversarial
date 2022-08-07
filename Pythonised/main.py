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

    Red team actions

    i = [0..8] => set setpt[i]
    i = [9..49] => set xmeas[i-9]

    Blue team actions

    i = [0..11] => set xmv[i]

===============================================================================
"""

from argparse import Action as ArgAction, ArgumentParser, RawTextHelpFormatter
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
from teprob import TEproc
from report import make_figures, make_report

# from sense import Sensors
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


##################################################################################################
# ArgParse preamble
##################################################################################################

description = """
    Tennessee Eastmann Adversarial Control Challenge - Single Continunous Control version.
"""

action_txt = """

Blue team action:  adjust xmv[3]
Red team action:   adjust xmeas[7]
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
group.add_argument(
    "--peaceful", help="no blue or red team actions", action="store_true"
)
group.add_argument("--loadblue", help="loads a pretrained blue team", action="store_true")
group.add_argument("--saveblue", help="saves the blue team policy", action="store_true")
    
parser.add_argument("--render", help="live visualisations (slow)", action="store_true")
parser.add_argument("--report", help="generates report template", action="store_true")
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
        entry_point="teprob:TEproc",
        max_episode_steps=int(48 * 3600),
        reward_threshold=195.0,
    )

    env = gym.make("TennesseeEastmannContinous-v1", red_intent=args.intent)
    if args.fast:
        env._max_episode_steps = 3600 + int(0.1 * 3600)

    # logging stuff
    wins = deque(maxlen=10)
    summary = []
    losses = []

    blue = DummyAgent() if args.peaceful or args.noblue else DefendAgent(args.loadblue, args.saveblue)
    red = DummyAgent() if args.peaceful or args.nored else ThreatAgent()

    if args.loadblue:
        blue.actor_critic.load_weights("./Pythonised/actorcritic")

    observations, _, __, ___ = env.reset()
    blue_action = None
    red_action = None
    actions = (blue_action, red_action)

    for i in range(args.num_episodes):
        env.reset()
        episode_memory = []
        for t in range(env._max_episode_steps):
            # separating observations. We also strip out time.
            blue_observation = observations[0]
            red_observation = observations[1]
            blue_action = blue(blue_observation[1:].reshape(1, 42))[0]
            red_action = red(red_observation[1:].reshape(1, 42))[0]
            blue_previous = blue_observation
            red_previous = red_observation
            actions = (blue_action, red_action)
            if args.verbose >= 1 and not args.peaceful:
                print(actions)
            observations, rewards, done, info = env.step(actions)
            blue_observation = observations[0]
            red_observation = observations[1]
            blue_reward = rewards[0]
            red_reward = rewards[1]
            blue_loss = blue.learn(
                blue_previous[1:], blue_reward, blue_observation[1:], done
            )
            red_loss = red.learn(
                red_previous[1:], red_reward, red_observation[1:], done
            )
            if args.render:
                env.render()
            if args.report and i % 10 == 0:
                episode_memory.append(
                    {
                        "episode": i,
                        "time": t,
                        "blue action": blue_action,
                        "red action": red_action,
                        "blue reward": blue_reward,
                        "red reward": red_reward,
                        "blue loss": blue_loss,
                        "red loss": red_loss,
                        "reported reactor pressure": blue_observation[7],
                        "reported reactor temperature": blue_observation[9],
                        "true reactor pressure": red_observation[7],
                        "true reactor temperature": red_observation[9],
                        "reported separator temperature": blue_observation[11],
                        "reported separator level": blue_observation[12],
                        "true separator temperature": red_observation[11],
                        "true separator level": red_observation[12],
                        "real inflows": (red_observation[1] * 22.32)
                        + red_observation[2]
                        + red_observation[3],
                        "real outflows": red_observation[17],
                        "compressor work": red_observation[20],
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
        if args.report and i % 10 == 0:
            make_figures(episode_memory, i, d)

    if args.saveblue:
        blue.actor_critic.save_weights("./Pythonised/actorcritic")

    ###############################################################################################
    # Report generation
    ###############################################################################################

    if args.report:
        make_report(d, action_txt, args.intent, summary)

    ###############################################################################################
    # Cleanup
    ###############################################################################################

    env.close()
