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

import enum
import loss
from agent import Agent
from gym.spaces import Space
from random import choice, randint
import numpy as np

class RedTeamSpace(Space):
    """
    A composite Box/Discrete space.

    The RedTeam may choose one action out of (perturb_xmv, perturb_xmeas, perturb_setpt)
    Given this single point, red team may overwite the process variable.
    """

    def __init__(self):
        self.target = Space.Discrete(3)
        """
        number of choices is dependent on target:
        xmv => 12
        xmeas => 42
        setpt => 9

        so will have to take max and discard invalid at runtime

        total of (12 + 42 + 9) = 63 d.o.f

        We can't particularly easily use Continous with DQN (the outputs of a model are Q-values),
        so discrete for now.

        actions are:
        [0:12] => set xmv to MAX
        [12:54] => set xmeas to 0.
        [54:63] => setpt *= 10
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
        import logging
        import os
        os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  #FATAL only
        logging.getLogger('tensorflow').setLevel(logging.FATAL)
        
        from tensorflow.keras import Sequential
        from tensorflow.keras.layers import Dense, Dropout, Input
        from tensorflow.keras.layers.experimental.preprocessing import Normalization
        
        super().__init__()
        self.intent = self.loss_func[intent]
        model = Sequential()
        model.add(Input(shape=(42,)))
        model.add(Dense(54, activation="tanh"))
        model.add(Dropout(0.4))
        model.add(Dense(64, activation="relu"))
        model.compile(loss="mae",
                      optimizer="adam")
        self.model = model

    def remember(self, previous, action, reward, observation, done):
        #TODO: somehow passed an int here. Should always be (None, dict)
        if action is None:
            action = 0
        elif "xmv" in action:
            action = action["xmv"]
        elif "xmeas" in action:
            action = action["xmeas"] + 12
        elif "setpt" in action:
            action = action["setpt"] + 54
        self.memory.append((previous, action, reward, observation, done))

    def get_action(self, observation):
        q = self.model.predict(observation.reshape(1,42))[0]
        action = choice(np.where(q == np.amax(q))[0])
        if action == 0:
            return None
        elif action < 13:
            return {"xmv": action}
        elif action < 54:
            return {"xmeas": action - 12}
        else:
            return {"setpt": action - 54}

