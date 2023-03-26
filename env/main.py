#!/usr/bin/env python3
"""
"""

from argparse import Action as ArgAction, ArgumentParser, RawTextHelpFormatter
from collections import deque
from copy import deepcopy
from datetime import datetime
import logging
import os
from os import system
from random import choice  # , uniform
from statistics import mode
import sys

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"  # FATAL only
logging.getLogger("tensorflow").setLevel(logging.FATAL)

import numpy as np
from colorpy.blackbody import blackbody_color
from matplotlib import pyplot as plt

from constants import *
from teprob import TEproc
from report import Logger
import gym

from agents.discrete.blue import DefendAgent as DiscreteDefendAgent
from agents.discrete.red import ThreatAgent as DiscreteThreatAgent
from agents.continuous.blue import DefendAgent as ContinuousDefendAgent
from agents.continuous.red import ThreatAgent as ContinuousThreatAgent
from agents.twin.blue import DefendAgent as TwinBasedDefendAgent
from agents.dummy import DummyAgent

np.seterr(all="raise")

##################################################################################################
# ArgParse preamble
##################################################################################################

description = """
    Tennessee Eastmann Adversarial Control Challenge - Single Continunous Control version.
"""

action_txt = """
Actions
---

For the Discrete variant:

Blue team actions

[0..12) => reset PLC 0-11 (TEproc will resort to open-loop for that PLC for one hour)
12 => restart entire plant (no production for 24 hours)
13 => continue (no action, no reward)

Red team actions

[0..41) => set xmeas[i] to 0.
[41..50) => setpt[i-41] *= 10
50 => no action

For the Single Continuous variant:

Blue team actions:

adjust xmv[3] (A and C feed)

Red team actions:

adjust setpt[6] (Reactor pressure)

For the Continuous variant:

Blue team actions:

[0..12) => adjust xmv[i]

Red team actions:

[0..9) => adjust setpoint[i]
"""

reward_txt = """
Blue team reward

  - production (valued at $20,000 per cubic metre of within-specification product,\n (G:H between 0.95 and 1.05)\n
  - utilities (valued at $ 0.1 per kWh for power and $ 0.065 per kg steam)\n
  - $ 1 M penalty if reactor overstress occurs;\n
  - $ 1000 penalty if the purge stream contains excess G.\n
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
    default="oppose",
    choices=["oppose", "recipe", "destruction", "environmental"],
)
parser.add_argument(
    "-n",
    "--num_episodes",
    help="number of episodes (default 100)",
    type=int,
    default=100,
)

parser.add_argument(
    "--red",
    help="red team agent type",
    default="continuous",
    choices=["none", "discrete", "singlecontinuous", "continuous"],
)
parser.add_argument(
    "--blue",
    help="blue team agent type",
    default="none",
    choices=["none", "discrete", "singlecontinuous", "continuous", "twin"],
)

parser.add_argument("--render", help="live visualisations (slow)", action="store_true")
parser.add_argument("--report", help="generates report template", action="store_true")
parser.add_argument("--adddate", help="add date to report name", action="store_true")
parser.add_argument("--data", help="outputs structured data", action="store_true")
parser.add_argument(
    "-v", "--verbose", help="displays debug info", action="count", default=0
)

if __name__ == "__main__":
    args = parser.parse_args()
    d = str(datetime.now().date())

    if args.report:
        memory = []

    gym.envs.registration.register(
        id="TennesseeEastmannContinous-v1",
        entry_point="teprob:TEproc",
        max_episode_steps=int(DEFAULT_RUNTIME),
        reward_threshold=195.0,
    )

    env = gym.make(
        "TennesseeEastmannContinous-v1",
        blue_type=args.blue,
        red_type=args.red,
        red_intent=args.intent,
    )
    if args.fast:
        env._max_episode_steps = 3600 + int(0.1 * 3600)
    log = Logger(args)

    # logging stuff

    if args.blue == "none":
        blue = DummyAgent()
    elif args.blue == "discrete":
        blue = DiscreteDefendAgent()
    elif args.blue == "singlecontinuous":
        blue = ContinuousDefendAgent(n_actions=1)
    elif args.blue == "continuous":
        blue = ContinuousDefendAgent()
    elif args.blue == "twin":
        blue = TwinBasedDefendAgent()

    if args.red == "none":
        red = DummyAgent()
    elif args.red == "discrete":
        red = DiscreteDefendAgent()
    elif args.red == "singlecontinuous":
        red = ContinuousDefendAgent(n_actions=1)
    elif args.red == "continuous":
        red = ContinuousThreatAgent()

    observations, _, __, ___ = env.reset()
    actions = (None, None)

    for i in range(args.num_episodes):
        prev_obvs = observations
        env.reset()
        for t in range(env._max_episode_steps):
            # separating observations. We also strip out time.
            blue_observation, red_observation = (observations[0], observations[1])
            blue_action, red_action = (
                blue(blue_observation[1:].reshape(1, 42))[0],
                red(red_observation[1:].reshape(1, 42))[0],
            )
            blue_previous, red_previous = (blue_observation, red_observation)
            actions = (blue_action, red_action)
            if args.verbose >= 2:
                print(actions)
            if args.data:
                log.log_to_file(env, t, blue_observation, red_observation)
            observations, rewards, done, info = env.step(actions)
            blue_observation, red_observation = (observations[0], observations[1])
            blue_reward, red_reward = (rewards[0], rewards[1])

            if args.blue == "discrete":
                blue.remember(
                    prev_obvs[1][1:],
                    blue_action,
                    blue_reward,
                    blue_observation[1:],
                    done,
                )
            elif args.blue == "continuous":
                blue_loss = blue.learn(
                    blue_previous[1:], blue_reward, blue_observation[1:], done
                )
            if args.red == "discrete":
                red.remember(
                    prev_obvs[1][1:], red_action, red_reward, red_observation[1:], done
                )
            elif args.red == "continuous":
                red_loss = red.learn(
                    red_previous[1:], red_reward, red_observation[1:], done
                )
            if args.render:
                env.render()
            if args.report and i % 10 == 0:
                log.log(
                    i,
                    t,
                    blue_action,
                    red_action,
                    blue_observation,
                    red_observation,
                    blue_reward,
                    red_reward,
                    env,
                )
            if args.verbose >= 1:
                print(
                    f"time = {env.time}: reactor P, T, PVs = {env.r.pg}, {env.r.tc}, {info['failures']}, {blue_reward=}, {red_reward=}"
                )
            if done:
                print(
                    f"Episode {i} finished after {t/3600.:1f} hrs ({t} timesteps): "
                    + (
                        f"red team wins: {info['failures']}"
                        if info["failures"]
                        else "blue team wins"
                    )
                )
                log.wins.append((0, 1) if info["failures"] else (1, 0))
                break
        if args.report and i % 10 == 0:
            log.summary(t, info)
            log.make_figures(i, d, args.blue, args.red)
        if args.blue == "discrete":
            blue.learn()
        # print(env.ctrlr)

    ###############################################################################################
    # Report generation
    ###############################################################################################

    if args.report:
        name = f"report_{d}.md" if args.adddate else "report.md"
        log.make_report(name, d, action_txt, reward_txt, args.intent)
        log.close()

    ###############################################################################################
    # Cleanup
    ###############################################################################################

    env.close()
