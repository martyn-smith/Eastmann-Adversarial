import numpy as np
import os
import sys
from .agent import Agent
from subprocess import run
import tensorflow as tf
from tensorflow.keras.models import load_model


class DefendAgent(Agent):
    def __init__(self, load):
        self.id = "blue"
        self.load = load
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
        """
        Deriving the value from a digital twin.

        Method:

        We receive an observation - an array of 41 (?) measured variables. We then feed that
        into a neural net, generating a full state vector of 50 floating point variables
        plus 24 fault flags. We then feed that to the twin, and capture
        its output at t+48 hours, discarding the zeroth variable (time).
        (Note that we capture output at t+48 hours *only*, for speed).

        The twin returns time, plus xmeas (42 floating point variables, the last being a convenience
        variable) + xmv (12 floating point variables) if -verbose was not enabled,
        otherwise it returns time, plus the same state vector (plus fault flags) as before.
        We obtain the value from those (currently set to NOT verbose).

        Note that the Twin outputs to stderr ONLY if it fails to load the seed state.
        A successful load that results in plant failure still outputs to stdout.
        """
        observation = observation.reshape(1, 53)
        state = self.stategenerator([observation]).numpy()[0]
        state = bytes(
            str(state[:50])[2:-1] + " " + str(state[50:].astype(int))[1:-1], "utf-8"
        )
        try:
            output = run(["./te", "-l", "-t", "-f"], input=state, capture_output=True)
            assert output.stderr == b""
            output = output.stdout.decode("utf-8")
            # print(output)
            # n = output.count('\n')
            # xmeas = np.fromstring(output, sep=" ").reshape(n, -1)
            xmeas = np.fromstring(output, sep=" ")[1:42]
            if xmeas.shape == (41,):
                return 20_000.0 * xmeas[17]
            else:
                return -999.0
        except AssertionError as e:
            print(f"twin output error: {e}", file=sys.stderr)
            exit(1)

    def learn(self, previous, reward, observation, done):
        if not self.load:
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
            return total_loss.numpy().sum()
        else:
            return 0.0
