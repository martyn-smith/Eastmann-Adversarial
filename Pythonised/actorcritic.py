import os
import tensorflow.keras as keras
from tensorflow.keras.layers import BatchNormalization, Dense
from tensorflow.keras import regularizers


class ActorCriticNetwork(keras.Model):
    def __init__(
        self,
        n_actions,
        fc1_dims=1024,
        fc2_dims=512,
        activation="softmax",
    ):
        super().__init__()
        self.fc1_dims = fc1_dims
        self.fc2_dims = fc2_dims
        self.n_actions = n_actions

        self.fc1 = Dense(
            self.fc1_dims,
            activation="relu",
            kernel_regularizer=regularizers.l2(0.1),
            bias_regularizer=regularizers.l2(0.1),
            activity_regularizer=regularizers.l2(0.1),
        )
        self.fc2 = Dense(
            self.fc2_dims,
            activation="softmax",
            kernel_regularizer=regularizers.l2(1e-3),
            bias_regularizer=regularizers.l2(1e-3),
            activity_regularizer=regularizers.l2(1e-3),
        )
        self.v = Dense(1, activation=None)
        self.pi = Dense(n_actions, activation=activation)

    def __call__(self, state):
        value = self.fc1(state)
        # print(f"{state=}, {value=}")
        value = self.fc2(value)
        v = self.v(value)
        pi = self.pi(value)
        return v, pi
