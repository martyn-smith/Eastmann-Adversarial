"""
A2C-based policy gradient solver.

Inspired by:
https://adventuresinmachinelearning.com/a2c-advantage-actor-critic-tensorflow-2/

"""

import logging
import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'  #FATAL only
logging.getLogger('tensorflow').setLevel(logging.FATAL)
from tensorflow.keras import Sequential
from collections import deque
import numpy as np
from random import choice, sample
import tensorflow as tf
import tensorflow.keras as keras
from tensorflow.keras.layers import Dense

class DummyAgent():
    def __init__(self):
        pass

    def __call__(self, _):
        return [None]

    def learn(self, *_):
        return 0.

class Agent:
    def __init__(self, n_out):
        # create model here in child process
        self.memory = deque(maxlen=100_000)
        self.batch_size = 64
        self.gamma = 0.98
        self.epsilon = 0.0001
        self.rng = np.random.random
        self.scale = 100.0
        self.actor_critic = ActorCriticNetwork(n_out)
        self.actor_critic.compile(optimizer="adam")

    def __call__(self, observation):
        observation = tf.convert_to_tensor([observation])
        value, actions = self.actor_critic(observation)
        return np.clip(actions.numpy()[0] + (self.epsilon * self.rng()) * self.scale,
                       0.,
                       self.scale)

    def learn(self, previous, reward, observation, done):
        previous = tf.convert_to_tensor([previous], dtype=tf.float32)
        observation = tf.convert_to_tensor([observation], dtype=tf.float32)
        reward = tf.convert_to_tensor(reward, dtype=tf.float32)  # not fed to NN
        with tf.GradientTape(persistent=True) as tape:
            prev_value, actions = self.actor_critic(previous)
            obs_value, _ = self.actor_critic(observation)
            prev_value = tf.squeeze(prev_value)
            obs_value = tf.squeeze(obs_value)

            delta = reward + self.gamma * obs_value * (1 - int(done)) - prev_value
            actor_loss = -actions * delta
            critic_loss = delta**2
            total_loss = actor_loss + critic_loss

        gradient = tape.gradient(total_loss, self.actor_critic.trainable_variables)
        self.actor_critic.optimizer.apply_gradients(
            zip(gradient, self.actor_critic.trainable_variables)
        )
        return total_loss.numpy().sum()

    # def remember(self, state, action, reward, observation, done):
    #     self.memory.append((state, action, reward, observation, done))
    #
    # def replay(self):
    #     # x is state (dims (42,1)). y is Q-value of all possible actions (14 for blue, ..? for red)
    #     x_batch, y_batch = [], []
    #     # memory here is [(state, action, reward, observation, done)]
    #     for i, (state, action, reward, observation, done) in enumerate(self.memory):
    #         # y_target = np.zeros(self.model.layers[-1].output_shape[1])
    #         y_target = self.model.predict(state.reshape(1, 42))[0] * self.advantage(i)
    #         x_batch.append(state)
    #         y_batch.append(y_target)
    #
    #     # combine the actions and advantages into a combined array for passing to
    #     # actor_loss function
    #     combined = np.zeros((len(actions), 2))
    #     combined[:, 0] = actions
    #     combined[:, 1] = advantages
    #
    #     y_batch = [discounted_rewards, combined]
    #     loss = self.model.train_on_batch(np.array(x_batch), np.array(y_batch))
    #     print(f"{self.id} training {loss=}")
    #     return loss
    #
    # def advantage(self, i):
    #     return sum(m[2] ** i for i, m in enumerate(self.memory))
    #
class ActorCriticNetwork(keras.Model):
    def __init__(
        self,
        n_actions,
        fc1_dims=1024,
        fc2_dims=512,
        name="actor_critic",
    ):
        super().__init__()
        self.fc1_dims = fc1_dims
        self.fc2_dims = fc2_dims
        self.n_actions = n_actions
        self.model_name = name

        self.fc1 = Dense(self.fc1_dims, activation="relu")
        self.fc2 = Dense(self.fc2_dims, activation="relu")
        #self.lstm = LSTM(16)
        self.v = Dense(1, activation=None)
        self.pi = Dense(n_actions, activation="sigmoid")

    def __call__(self, state):
        value = self.fc1(state)
        value = self.fc2(value)

        v = self.v(value)
        pi = self.pi(value)

        return v, pi
