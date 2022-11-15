from .agent import Agent


class DefendAgent(Agent):
    def __init__(self, n_actions=9):
        # self.id = "red"
        super().__init__(n_actions)

    def __call__(self, observation):
        return 100.0 * super(ThreatAgent, self).__call__(observation)
