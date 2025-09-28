"""
Generates a random seed in the correct format for the TE process. Nothing more,
and it will almost certainly fail given it is random input.
"""

from datetime import date
from random import random, randint
from subprocess import Popen, run, PIPE

seed = "".join(f"{random()*(10**randint(1,2)):.15E}  " for _ in range(50))


def score(seed):
    """
    Scores an input seed for time-to-failure. Returns the inverse, so that scipy.minimise works.
    Returns 0 if no failure is observed
    TODO: add setpt err to score
    """
    date = date.today().strftime("%d%m%y")
    p = run(["./te_" + date, "-l"], stdout=PIPE, input=seed, encoding="ascii")
    try:
        print(p.stdout)
        time = float(p.stdout[36:53])
        return 1.0 / time
    except TypeError:
        return 0.0
    except ZeroDivisionError:
        # The minimum time-delta is 1/ 3600, which approximates to 0.0002. 0.0001 is always lower.
        return 1.0 / 0.0001


print(score(seed))
