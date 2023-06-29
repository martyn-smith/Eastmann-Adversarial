"""
DDPG-based policy gradient solver.

Inspired by:
https://adventuresinmachinelearning.com/a2c-advantage-actor-critic-tensorflow-2/

"""

import logging
import os

from tensorflow.keras import Sequential
from collections import deque
import numpy as np
from random import choices
import tensorflow as tf
import tensorflow.keras as keras
from tensorflow.keras.layers import Dense
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.losses import MSE
from copy import deepcopy

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"  # FATAL only
logging.getLogger("tensorflow").setLevel(logging.FATAL)


class Agent:
    def __init__(self, n_out):
        # learning parameters
        self.gamma = 0.98
        self.tau = 0.01
        self.alpha = 0.03

        # exploration noise and scaling
        self.rng = np.random.random
        self.epsilon = 0.01
        self.epsilon_min = 0.000001
        self.epsilon_decay = 0.095
        self.scale = 1.0

        # replay parameters
        self.memory = deque(maxlen=100_000)
        self.batch_size = 100

        # networks
        self.actor = Actor(n_out)
        self.critic = Critic()
        self.actor.compile(optimizer=Adam(learning_rate=self.alpha))
        self.critic.compile(optimizer=Adam(learning_rate=self.alpha))
        self.target_actor = deepcopy(self.actor)
        self.target_critic = deepcopy(self.critic)

    def __call__(self, observation):
        observation = tf.convert_to_tensor([observation])
        action = self.actor(observation)
        self.action = action[0][0]
        if self.epsilon >= self.epsilon_min:
            self.epsilon *= self.epsilon_decay
        return np.clip(
            (action.numpy()[0] + (self.epsilon * self.rng()) * 2.0 * self.scale),
            -self.scale,
            self.scale,
        )

    def update(self):
        weights = [
            w * self.tau + t * (1 - self.tau)
            for w, t in zip(self.actor.weights, self.target_actor.weights)
        ]
        self.target_actor.set_weights(weights)

    def learn(self, previous, reward, observation, done):
        self.memory += [
            {
                "previous": previous,
                "action": self.action,
                "reward": reward,
                "observation": observation,
                "done": done,
            }
        ]

        batch = choices(self.memory, k=self.batch_size)

        previous = tf.convert_to_tensor(
            [b["previous"] for b in batch], dtype=tf.float32
        )
        observation = tf.convert_to_tensor(
            [b["observation"] for b in batch], dtype=tf.float32
        )
        reward = tf.convert_to_tensor([b["reward"] for b in batch], dtype=tf.float32)
        action = tf.convert_to_tensor([b["action"] for b in batch], dtype=tf.float32)
        done = tf.convert_to_tensor([int(b["done"]) for b in batch], dtype=tf.float32)

        with tf.GradientTape() as tape:
            target_actions = self.target_actor(observation)
            critic_value = tf.squeeze(
                self.target_critic(observation, target_actions), 1
            )
            prev_critic_value = tf.squeeze(self.critic(previous, action), 1)
            target = reward + self.gamma * critic_value * (1 - done)
            critic_loss = MSE(target, prev_critic_value)

        critic_network_gradient = tape.gradient(
            critic_loss, self.critic.trainable_variables
        )
        self.critic.optimizer.apply_gradients(
            zip(critic_network_gradient, self.critic.trainable_variables)
        )

        with tf.GradientTape() as tape:
            policy_actions = self.actor(previous)
            actor_loss = tf.math.reduce_mean(-self.critic(previous, policy_actions))

        actor_network_gradient = tape.gradient(
            actor_loss, self.actor.trainable_variables
        )
        self.actor.optimizer.apply_gradients(
            zip(actor_network_gradient, self.actor.trainable_variables)
        )

        self.update()
        return actor_loss.numpy().sum() + critic_loss.numpy().sum()


class Actor(keras.Model):
    def __init__(self, n_actions):
        super().__init__()
        self.layer_1 = Dense(512, activation="relu")
        self.layer_2 = Dense(512, activation="sigmoid")
        self.layer_2 = Dense(512, activation="relu")
        self.mu = Dense(n_actions, activation="sigmoid")

    def __call__(self, observation):
        return self.mu(self.layer_2(self.layer_1(observation)))


class Critic(keras.Model):
    def __init__(self):
        super().__init__()
        self.layer_1 = Dense(512, activation="relu")
        self.layer_2 = Dense(512, activation="sigmoid")
        self.layer_3 = Dense(512, activation="relu")
        self.layer_4 = Dense(512, activation="sigmoid")
        self.q = Dense(1, activation=None)

    def __call__(self, observation, action):
        return self.q(
            self.layer_4(
                self.layer_3(
                    self.layer_2(self.layer_1(tf.concat([observation, action], axis=1)))
                )
            )
        )
