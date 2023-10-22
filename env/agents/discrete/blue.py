from .agent import Agent


class DefendAgent(Agent):
    def __init__(self):
        self.id = "blue"
        super().__init__(12)
