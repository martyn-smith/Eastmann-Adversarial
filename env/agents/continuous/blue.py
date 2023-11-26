from .agent import Agent


class DefendAgent(Agent):
    def __init__(self, n_actions=12):
        # self.id = "red"
        super().__init__(n_actions)
        # high epsilon variant
        self.epsilon = 0.5

    def __call__(self, observation):
        return 100.0 * super().__call__(observation)
