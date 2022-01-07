from agent import Agent
from gym.spaces import MultiDiscrete
import numpy as np
from random import choice, randint, random

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
        from tensorflow.keras.optimizers import Adam

        self.id = "blue"
        super().__init__()
        model = Sequential()
        model.add(Input(shape=(42,)))
        model.add(Dense(54, activation="tanh"))
        model.add(Dense(14, activation="relu"))
        opt = Adam(learning_rate=0.01)
        model.compile(loss="mae",
                        optimizer="adam")
        self.model = model

    def encode(self, action):
        if action == 0:
            return None
        elif action < 13:
            return {"reset": action - 1}
        else:
            return "reset_all"

    def decode(self, action):
        if action is None:
            return 0
        elif type(action) is dict:
            return action["reset"] + 1
        else:
            return 13

    def get_action(self, observation):
        if random() >= self.epsilon:
            q = self.model.predict(observation.reshape(1,42))[0]
            try:
                action = np.nanargmax(q)
            except ValueError:
                action = randint(0,13)
        else:
            action = randint(0,13)
        return action
