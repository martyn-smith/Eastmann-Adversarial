class DummyAgent:
    def __init__(self):
        pass

    def __call__(self, *_):
        return [None]

    def learn(self, *_):
        return 0.0

    def remember(self, *_):
        pass
