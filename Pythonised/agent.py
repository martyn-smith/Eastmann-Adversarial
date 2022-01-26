"""
A2C-based policy gradient solver.

Inspired by:
https://adventuresinmachinelearning.com/a2c-advantage-actor-critic-tensorflow-2/

"""

#import logging
#import os
#os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  #FATAL only
#logging.getLogger('tensorflow').setLevel(logging.FATAL)
#from tensorflow.keras import Sequential
from collections import deque
import numpy as np
from random import choice, sample

class Agent:

    def __init__(self):
        #create model here in child process
        self.memory = deque(maxlen=100_000)
        self.batch_size = 64
        self.gamma = 1.0
        self.epsilon = 1.0
        self.epsilon_min=0.01
        self.epsilon_decay=0.095
        #self.model = Sequential()

    def remember(self, state, action, reward, observation, done):
        #self.memory.append((state, action, reward, observation, done))
        self.memory.append((state, action, reward, observation, done))

    def get_action(self, observation):
        #this could only be inheritable if model shape is known.
        pass

    def replay(self):
        #x is state (dims (42,1)). y is Q-value of all possible actions (14 for blue, ..? for red)
        x_batch, y_batch = [], []
        #memory here is [(state, action, reward, observation, done)]
        for i, (state, action, reward, observation, done) in enumerate(self.memory):
            #y_target = np.zeros(self.model.layers[-1].output_shape[1])
            y_target = self.model.predict(state.reshape(1,42))[0] * self.advantage(i)
            x_batch.append(state)
            y_batch.append(y_target)

        # combine the actions and advantages into a combined array for passing to
        # actor_loss function
        combined = np.zeros((len(actions), 2))
        combined[:, 0] = actions
        combined[:, 1] = advantages

        y_batch = [discounted_rewards, combined]
        loss = self.model.train_on_batch(np.array(x_batch), np.array(y_batch))
        print(f"{self.id} training {loss=}")
        return loss

    def advantage(self, i):
        return sum(m[2]**i for i, m in enumerate(self.memory))

class DummyAgent(Agent):
    def __init__(self):
        pass

    def intent(self, *args):
        return 0.

    def remember(self, *args):
        pass

    def replay(self, *args):
        pass

    def get_action(self, *args):
        pass


