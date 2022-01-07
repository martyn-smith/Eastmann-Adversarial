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
from random import choice, randint, random
import numpy as np

class ThreatAgent(Agent):
    """
    ENVIRONMENTAL = 0
    MECHANICAL = 1
    DOWNTIME = 2
    """

    loss_func = {
        "environmental": loss.environmental,
        "mechanical": loss.mechanical,
        "downtime": loss.downtime
    }

    def __init__(self, intent="downtime"):
        import logging
        import os
        os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  #FATAL only
        logging.getLogger('tensorflow').setLevel(logging.FATAL)

        self.id = "red"
        from tensorflow.keras import Sequential
        from tensorflow.keras.layers import Dense, Dropout, Input
        from tensorflow.keras.layers.experimental.preprocessing import Normalization
        from tensorflow.keras.optimizers import Adam

        super().__init__()
        self.intent = self.loss_func[intent]
        model = Sequential()
        model.add(Input(shape=(42,)))
        model.add(Dense(54, activation="tanh"))
        model.add(Dropout(0.4))
        model.add(Dense(64, activation="relu"))
        opt = Adam(learning_rate=0.01)
        model.compile(loss="mae",
                      optimizer=opt)
        self.model = model

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

    def get_action(self, observation):
        #TODO: pick action_space
        if random() >= self.epsilon:
            q = self.model.predict(observation.reshape(1,42))[0]
            try:
                action = np.nanargmax(q)
            except ValueError:
                action = randint(0,63)
        else:
            action = randint(0,63)
        return action
