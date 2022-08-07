from agent import Agent, ActorCriticNetwork
from collections import deque
import numpy as np

class DefendAgent(Agent):
    def __init__(self, load, save):
        self.id = "blue"
        self.memory = deque(maxlen=100_000)
        self.batch_size = 64
        self.gamma = 0.98
        self.epsilon = 0.0001
        self.rng = np.random.random
        self.scale = 100.0
        self.load = load
        self.save = save
        self.actor_critic = ActorCriticNetwork(1)
        self.actor_critic.compile(optimizer="adam")

    def learn(self, previous, reward, observation, done):
        if self.load:
            return 0.
        else:
            return super().learn(previous, reward, observation, done)
