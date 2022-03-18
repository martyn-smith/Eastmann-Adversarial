import logging
import os

os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"  # FATAL only
logging.getLogger("tensorflow").setLevel(logging.FATAL)
from tensorflow import keras
import numpy as np
import subprocess

if not os.path.exists("blue.h5"):
    subprocess.run("./teprob.py")
model = keras.models.load_model("blue.h5")
test_state = np.loadtxt("../teststates/xmeas.dat")[1:]
print(model.predict(test_state.reshape(1, 42))[0])
