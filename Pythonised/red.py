# import tensorflow_probability as tfp
from agent import Agent

class ThreatAgent(Agent):
    def __init__(self):
        self.id = "red"
        super().__init__(51)
