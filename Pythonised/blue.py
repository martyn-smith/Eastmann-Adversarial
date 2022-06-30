import numpy as np
import os
import sys
from agent import Agent
from subprocess import run
import tensorflow as tf
from tensorflow.keras.models import load_model

class DefendAgent(Agent):
    def __init__(self):
        self.id = "blue"
        self.stategenerator = load_model("./Pythonised/stategenerator")
        np.set_printoptions(
            linewidth=1300,
            formatter={
                "float": lambda x: " " + f"{x:.15E}",
                "int": lambda x: " " + str(x),
            },
        )
        super().__init__(1)

    def value(self, observation):
        observation = observation.reshape(1,53)
        state = self.stategenerator.predict([observation])[0] # np.ndarray, 74 inputs
        state = bytes(str(state[:50])[2:-1] + " " + str(state[50:].astype(int))[1:-1], "utf-8")
        try:
            output = run(["./te", "-l", "-v"], input=state, capture_output=True)
            assert output.stderr == b""
            xmeas = output.stdout.decode("utf-8")
        except AssertionError as e:
            print(f"twin output error: {e}", file=sys.stderr)
            exit(1)
        n = xmeas.count('\n')
        xmeas = np.fromstring(xmeas, sep=' ').reshape(n, -1)
        try:
            value = sum(
                (self.gamma**i) * 20_000 * xmeas[17] for i, xmeas in enumerate(xmeas)
            )
        except FloatingPointError:
            value = 0.
        return value

    def learn(self, previous, reward, observation, done):
        value = self.value(observation)
        value = tf.convert_to_tensor([value], dtype=tf.float32)
        # return super().learn(previous, reward, observation, done)
        # print(value)
        previous = tf.convert_to_tensor([previous], dtype=tf.float32)
        observation = observation[:42]
        observation = tf.convert_to_tensor([observation], dtype=tf.float32)
        reward = tf.convert_to_tensor(reward, dtype=tf.float32)  # not fed to NN
        with tf.GradientTape(persistent=True) as tape:
            prev_value, actions = self.actor_critic(previous)
            # obs_value, _ = self.actor_critic(observation)
            obs_value = value
            prev_value = tf.squeeze(prev_value)
            obs_value = tf.squeeze(obs_value)

            delta = reward + self.gamma * obs_value * (1 - int(done)) - prev_value
            # ecce here
            actor_loss = -actions * delta
            critic_loss = delta**2
            total_loss = actor_loss + critic_loss

        gradient = tape.gradient(total_loss, self.actor_critic.trainable_variables)
        self.actor_critic.optimizer.apply_gradients(
            zip(gradient, self.actor_critic.trainable_variables)
        )
        return total_loss.numpy()[0][0]
