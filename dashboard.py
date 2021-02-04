import matplotlib.pyplot as plt
import numpy as np
from subprocess import Popen, PIPE
from time import sleep

with Popen(["builds/te_140920", "-r"], stdout=PIPE) as p:
    while (True):
        sleep(1)
        out = p.stdout.readline().decode("utf-8").strip(" \n")
        print(out)
        #print(np.fromstring(out, dtype=float, sep=" ", count=7))
