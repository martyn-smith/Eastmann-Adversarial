"""
#Goal randomly pick the high-level loss goal
#given a selected number of control loops under control,
#respond to gym under
#TODO: find an optimiser, ideally NN-based but also Pyomo
# classes: aims for
# environmental damage (G in purge),
# mechanical damage
# downtime or lost profits
"""

from .agent import Agent
from random import choice, randint, random
import numpy as np


class ThreatAgent(Agent):
    def __init__(self):
        self.id = "red"
        super().__init__(50)

    def encode(self, action):
        if action == 0:
            return None
        elif action < 13:
            return {"xmv": action - 1}
        elif action < 55:
            return {"xmeas": action - 13}
        else:
            return {"setpt": action - 55}

    def decode(self, action):
        if action is None:
            return 0
        elif "xmv" in action:
            return action["xmv"] + 1
        elif "xmeas" in action:
            return action["xmeas"] + 13
        elif "setpt" in action:
            return action["setpt"] + 55
