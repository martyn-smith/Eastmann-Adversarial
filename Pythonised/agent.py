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
        #self.model = Sequential()

    def remember(self, state, action, reward, observation, done):
        self.memory.append((state, action, reward, observation, done))

    def get_action(self, observation):
        x = self.model.predict(observation[1:])[0]
        idx = choice(np.where(x == np.amax(x)))
        return idx

    def replay(self):
        #x is state (dims (42,1)). y is Q-value of all possible actions (14 for blue, ..? for red)
        x_batch, y_batch = [], []
        #memory here is [(state, action, reward, observation, done)]
        minibatch = sample(
            self.memory, min(len(self.memory), self.batch_size))
        for state, action, reward, observation, done in minibatch:
            #TODO: replace with np.zeros()
            y_target = self.model.predict(state.reshape(1,42))[0]
            y_target[action] = (reward if done else
                                   reward + self.gamma
                                            * np.max(self.model.predict(observation.reshape(1,42))[0]))
            x_batch.append(state)
            #print(f"{y_target=}")
            y_batch.append(y_target)

        loss = self.model.train_on_batch(np.array(x_batch), np.array(y_batch))
        print(loss)
        #TODO: epsilon
        #if self.epsilon > self.epsilon_min:
        #    self.epsilon *= self.epsilon_decay

class DummyAgent(Agent):
    def __init__(self):
        pass

    def remember(self, *args):
        pass
    def replay(self, *args):
        pass

    def get_action(self, *args):
        pass


