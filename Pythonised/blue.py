from agent import Agent
from gym.spaces import MultiDiscrete

class BlueTeamSpace(MultiDiscrete):
    """
    Blue team actions:

     continue (no action, no reward)
     reset PLC 0-12 (TEproc will resort to open-loop for that PLC for one hour)
     restart entire plant (no production for 24 hours)
    """
    def __init__(self, nvec=[12,2], seed=None):
        self.nvec = nvec


class TEprobManager(Agent):
    pass
