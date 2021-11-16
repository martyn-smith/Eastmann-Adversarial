from agent import Agent
from gym.spaces import MultiDiscrete
import numpy as np
from random import choice

class BlueTeamSpace(MultiDiscrete):
    """
    Blue team actions:

     continue (no action, no reward)
     reset PLC 0-12 (TEproc will resort to open-loop for that PLC for one hour)
     restart entire plant (no production for 24 hours)

     total of 13 degrees of freedoe
    """
    def __init__(self, nvec=[12,2], seed=None):
        self.nvec = nvec

class TEDummy(Agent):
    """
    Dummy class if we want to run the process without all the awkward setup.
    """
    pass

class TEprobManager(Agent):
    def __init__(self):
        import logging
        import os
        os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  #FATAL only
        logging.getLogger('tensorflow').setLevel(logging.FATAL)
        from tensorflow.keras import Sequential
        from tensorflow.keras.layers import Dense, Dropout, Input
        from tensorflow.keras.layers.experimental.preprocessing import Normalization

        super().__init__()
        model = Sequential()
        model.add(Input(shape=(42,)))
        model.add(Dense(54, activation="tanh"))
        model.add(Dense(14, activation="relu"))
        model.compile(loss="mae",
                        optimizer="adam")
        self.model = model

    def remember(self, state, action, reward, observation, done):
        if action is None:
            action = 0
        elif type(action) is dict:
            action = action["reset"]
        else:
            action = 13
        self.memory.append((state, action, reward, observation, done))

    def get_action(self, observation):
        q = self.model.predict(observation.reshape(1,42))[0]
        action = choice(np.where(q == np.amax(q))[0])
        if action == 0:
            return None
        elif action < 13:
            return {"reset": action}
        else:
            return "reset_all"

