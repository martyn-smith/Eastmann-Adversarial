from agent import Agent
from gym.spaces import MultiDiscrete
import numpy as np
from random import choice, randint, random


class BlueAgent(Agent):
    def __init__(self):
        self.id = "blue"
        super().__init__(14)

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

    def __call__(self, observation):
        if random() >= self.epsilon:
            q = self.model.predict(observation.reshape(1, 42))[0]
            try:
                action = np.nanargmax(q)
            except ValueError:
                action = randint(0, 13)
        else:
            action = randint(0, 13)
        return action
