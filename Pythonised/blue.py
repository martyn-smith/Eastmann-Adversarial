from agent import Agent
import logging
import os
import numpy as np

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"  # FATAL only
logging.getLogger("tensorflow").setLevel(logging.FATAL)
import tensorflow as tf
from tensorflow.keras.models import load_model

# import tensorflow_probability as tfp
from actorcritic import ActorCriticNetwork

GAMMA = 0.95


class DefendAgent(Agent):
    def __init__(self):
        self.id = "blue"
        self.gamma = GAMMA
        self.actor_critic = ActorCriticNetwork(n_actions=1)
        self.actor_critic.compile(optimizer="adam")
        self.stategenerator = load_model("./Pythonised/stategenerator")

    def __call__(self, observation):
        state = self.stategenerator.predict([observation])[0]
        with open("teststates/state.in") as f:
            state = f.read()
        xmeas = np.frombuffer(run(["../te", "-l", "-v"], input=bytes(state, "utf-8"), capture_output=True).stdout)
        print(xmeas)
        value = sum((self.gamma ** i) * 20_000 * xmeas[17] for i, xmeas in enumerate(xmeas))
        # print(f"{state=}, {value=}")
        v = value
        pi = self.pi(value)
        return v, pi

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
            # ecce here
            actor_loss = -actions * delta
            critic_loss = delta**2
            total_loss = actor_loss + critic_loss

        gradient = tape.gradient(total_loss, self.actor_critic.trainable_variables)
        self.actor_critic.optimizer.apply_gradients(
            zip(gradient, self.actor_critic.trainable_variables)
        )
        return total_loss.numpy()[0][0]

    def get_action(self, observation):
        observation = tf.convert_to_tensor([observation])
        value, actions = self(observation)
        return actions.numpy()[0]
