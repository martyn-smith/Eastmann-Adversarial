"""
DQN-based solver.

Inspired by n1try's writeup
(https://gym.openai.com/evaluations/eval_EIcM1ZBnQW2LBaFN6FY65g/) and
https://keon.io/deep-q-learning
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
        minibatch = sample(
            self.memory, min(len(self.memory), self.batch_size))
        for state, action, reward, observation, done in minibatch:
            #y_target = np.zeros(self.model.layers[-1].output_shape[1])
            y_target = self.model.predict(state.reshape(1,42))[0]
            y_target[action] = (reward if done else
                                   reward + self.gamma
                                            * np.max(self.model.predict(observation.reshape(1,42))[0]))
            x_batch.append(state)
            #print(f"{y_target=}")
            y_batch.append(y_target)

        loss = self.model.train_on_batch(np.array(x_batch), np.array(y_batch))
        print(f"{self.id} training {loss=}")
        if self.epsilon > self.epsilon_min:
            self.epsilon *= self.epsilon_decay
        return loss

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


