#TODO:
#Goal randomly pick the high-level loss goal
#given a selected number of control loops under control,
#respond to gym under 
#TODO: find an optimiser, ideally NN-based but also Pyomo
# classes: aims for 
# environmental damage (G in purge), 
# mechanical damage
# downtime or lost profits

import enum
import loss
from agent import Agent
from gym.spaces import Space
from random import choice, randint

class RedTeamSpace(Space):
    """
    A composite Box/Discrete space.

    The RedTeam may choose one action out of (perturb_xmv, perturb_xmeas, perturb_setpt)
    Given this single point, red team may overwite the process variable.
    """

    def __init__(self):
        """
        0 => perturb xmv 
        1 => perturb xmeas
        2 => perturb setpt
        """
        self.target = Space.Discrete(3)
        """
        number of choices is dependent on target:
        xmv => 12
        xmeas => 42
        setpt => 9

        so will have to take max and discard invalid at runtime
        """
        self.variable = Space.Discrete(42)
        """
        easiest part, we will handle the scaling at runtime
        """
        self.value = Space.Box(0., 1.0, shape=(1))


class ThreatAgent(Agent):
    """
    ENVIRONMENTAL = 0
    MECHANICAL = 1
    DOWNTIME = 2
    """
        
    loss_func = {
        0: loss.environmental,
        1: loss.mechanical,
        2: loss.downtime
    }

    def __init__(self, intent):
        self.intent = self.loss_func[intent]
        super().__init__()

    def get_action(self, _):
        return [choice([0,1]), randint(0,11), 100.]
