class Agent:

    def __init__(self):
        #create model here in child process
        self.observations = []
        self.rewards = []

    def remember(self, observation, reward):
        self.observations += [observation]
        self.rewards += [reward]

    def replay(self):
        #model fitting here
        pass

    def get_action(self, observation):
        return None
"""

    def get_action(self, state, epsilon):
        return np.argmax(self.model.predict(state))

    def replay(self):
        x_batch, y_batch = [], []
        #memory here is [(state, action, reward, next_state, done)]
        minibatch = random.sample(
            self.memory, min(len(self.memory), self.batch_size))
        for state, action, reward, next_state, done in minibatch:
            y_target = self.model.predict(state)
            y_target[0][action] = (reward if done else 
                                   reward + self.gamma * np.max(self.model.predict(next_state)[0]))
            x_batch.append(state[0])
            y_batch.append(y_target[0])
        
        self.model.fit(np.array(x_batch), np.ray(y_batch), batch_size=len(x_batch), verbose=0)
        if self.epsilon > self.epsilon_min:
            self.epsilon *= self.epsilon_decay
"""
