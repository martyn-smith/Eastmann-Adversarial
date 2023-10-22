"""
DQN-based solver.

References:
n1try on cartpole (https://gym.openai.com/evaluations/eval_EIcM1ZBnQW2LBaFN6FY65g/),
https://keon.io/deep-q-learning
"""

import logging
import os

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"  # FATAL only
logging.getLogger("tensorflow").setLevel(logging.FATAL)
from tensorflow.keras import Sequential
from tensorflow.keras.layers import Dense, Dropout, Input
from tensorflow.keras.layers.experimental.preprocessing import Normalization
from tensorflow.keras.optimizers import Adam
from collections import deque
import numpy as np
from random import choice, sample, randint, random


class Agent:
    def __init__(self, n_actions):
        # create model here in child process
        self.memory = deque(maxlen=100_000)
        self.batch_size = 100
        self.gamma = 1.0
        self.epsilon = 0.99
        self.epsilon_min = 0.00001
        self.epsilon_decay = 0.1
        self.n_actions = n_actions
        model = Sequential()
        model.add(Input(shape=(42,)))
        model.add(Dense(256, activation="tanh"))
        model.add(Dense(256, activation="tanh"))
        model.add(Dense(n_actions, activation="relu"))
        opt = Adam(learning_rate=0.01)
        model.compile(loss="mae", optimizer="adam")
        self.model = model

    def remember(self, state, action, reward, observation, done):
        self.memory.append((state, action, reward, observation, done))

    def __call__(self, observation):
        if random() >= self.epsilon:
            q = self.model.predict(observation.reshape(1, 42), verbose=0)[0]
            try:
                action = np.nanargmax(q)
            except ValueError:
                action = randint(0, self.n_actions - 1)
        else:
            action = randint(0, self.n_actions - 1)
        return [action]

    def learn(self):
        # x is state (dims (42,1)). y is Q-value of all possible actions (14 for blue, ..? for red)
        x_batch, y_batch = [], []
        # memory here is [(state, action, reward, observation, done)]
        minibatch = sample(self.memory, min(len(self.memory), self.batch_size))
        for state, action, reward, observation, done in minibatch:
            # y_target = np.zeros(self.model.layers[-1].output_shape[1])
            y_target = self.model.predict(state.reshape(1, 42), verbose=0)[0]
            y_target[action] = (
                reward
                if done
                else reward
                + self.gamma
                * np.max(self.model.predict(observation.reshape(1, 42), verbose=0)[0])
            )
            x_batch.append(state)
            # print(f"{y_target=}")
            y_batch.append(y_target)

        loss = self.model.train_on_batch(np.array(x_batch), np.array(y_batch))
        print(f"{self.id} training {loss=}")
        if self.epsilon > self.epsilon_min:
            self.epsilon *= self.epsilon_decay
        return loss
