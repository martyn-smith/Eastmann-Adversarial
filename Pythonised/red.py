# import tensorflow_probability as tfp
from agent import Agent


class ThreatAgent(Agent):
    def __init__(self):
        self.id = "red"
        super().__init__(9)

    def __call__(self, observation):
        return 100.0 * super(ThreatAgent, self).__call__(observation)
