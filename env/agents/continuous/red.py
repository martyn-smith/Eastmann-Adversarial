from .agent import Agent


class ThreatAgent(Agent):
    def __init__(self, n_actions=9):
        # self.id = "blue"
        super().__init__(n_actions)

    def __call__(self, observation):
        return 100.0 * super().__call__(observation)
