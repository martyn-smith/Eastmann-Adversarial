from agent import Agent
import logging
import os

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"  # FATAL only
logging.getLogger("tensorflow").setLevel(logging.FATAL)
import tensorflow as tf

# import tensorflow_probability as tfp
from actorcritic import ActorCriticNetwork

GAMMA = 0.95


class DefendAgent(Agent):
    def __init__(self):
        self.id = "blue"
        self.gamma = GAMMA
        self.actor_critic = ActorCriticNetwork(n_actions=1)
        self.actor_critic.compile(optimizer="adam")

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
        value, actions = self.actor_critic(observation)
        return actions.numpy()[0]
