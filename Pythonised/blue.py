from gym.spaces import Discrete
import numpy as np
import logging
import os
from random import choice, sample, randint, randint, random
from collections import deque

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"  # FATAL only
logging.getLogger("tensorflow").setLevel(logging.FATAL)
from tensorflow.keras import Sequential
from tensorflow.keras.layers import Dense, Dropout, Input
from tensorflow.keras.layers.experimental.preprocessing import Normalization
from tensorflow.keras.optimizers import Adam

class Agent:
    def __init__(self, n_actions):
        # create model here in child process
        self.memory = deque(maxlen=100_000)
        self.batch_size = 64
        self.gamma = 1.0
        self.epsilon = 1.0
        self.epsilon_min = 0.01
        self.epsilon_decay = 0.095
        self.n_actions = n_actions
        model = Sequential()
        model.add(Input(shape=(42,)))
        model.add(Dense(12, activation="softmax"))
        model.add(Dense(n_actions, activation="relu"))
        opt = Adam(learning_rate=0.01)
        model.compile(loss="mae", optimizer="adam")
        self.model = model

    def remember(self, state, action, reward, observation, done):
        self.memory.append((state, action, reward, observation, done))

    def get_action(self, observation):
        # this could only be inheritable if model shape is known.
        pass

    def __call__(self, observation):
        # TODO: pick action_space
        if random() >= self.epsilon:
            q = self.model.predict(observation.reshape(1, 42))[0]
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
            y_target = self.model.predict(state.reshape(1, 42))[0]
            y_target[action] = (
                reward
                if done
                else reward
                + self.gamma * np.max(self.model.predict(observation.reshape(1, 42))[0])
            )
            x_batch.append(state)
            # print(f"{y_target=}")
            y_batch.append(y_target)

        loss = self.model.train_on_batch(np.array(x_batch), np.array(y_batch))
        print(f"{self.id} training {loss=}")
        if self.epsilon > self.epsilon_min:
            self.epsilon *= self.epsilon_decay
        return loss


class DummyAgent(Agent):
    def __init__(self):
        pass

    def remember(self, *args):
        pass

    def __call__(self, *args):
        return 13

    def learn(self, *args):
        pass

class DefendAgent(Agent):
    def __init__(self):
        self.id = "blue"
        super().__init__(12)

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
