import numpy as np
from agent import Agent
from subprocess import run

class DefendAgent(Agent):
    def __init__(self):
        self.id = "blue"
        self.stategenerator = load_model("./Pythonised/stategenerator")
        super().__init(1)

    def __call__(self, observation):
         print(f"{state=}, {value=}")
        v = self.value()
        pi = self.pi()
        return v, pi

    def value(self, observation):
        state = self.stategenerator.predict([observation])[0]
        xmeas = np.frombuffer(run(["../te", "-l", "-v"], input=bytes(state, "utf-8"), capture_output=True).stdout)
        #print(xmeas)
        value = sum((self.gamma ** i) * 20_000 * xmeas[17] for i, xmeas in enumerate(xmeas))
        return value

    def learn(self, previous, reward, observation, done):
        previous = tf.convert_to_tensor([previous], dtype=tf.float32)
        observation = tf.convert_to_tensor([observation], dtype=tf.float32)
        reward = tf.convert_to_tensor(reward, dtype=tf.float32)  # not fed to NN
        with tf.GradientTape(persistent=True) as tape:
            prev_value, actions = self.actor_critic(previous)
            obs_value, _ = self.value(observation)
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
