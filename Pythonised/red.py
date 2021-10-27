#TODO:
#Goal randomly pick the high-level loss goal
#given a selected number of control loops under control,
#respond to gym under 
#TODO: find an optimiser, ideally NN-based but also Pyomo
# classes: aims for 
# environmental damage (G in purge), 
# mechanical damage
# downtime or lost profits

import enum
import loss

class ThreatAgent():
    """
    ENVIRONMENTAL = 0
    MECHANICAL = 1
    DOWNTIME = 2
    """

    loss_func = {
        0: loss.environmental,
        1: loss.mechanical,
        2: loss.downtime
    }
    
    def __init__(self):
        pass

    def peturb():
        pass
