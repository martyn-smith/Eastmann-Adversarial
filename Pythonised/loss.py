steps = 0
G_TOLERANCE = 0.001
G_H_LOWER = 0.95
G_H_UPPER = 1.05
COST_KWH = 0.1
COST_STEAM = 0.065

def loss(reset, failed, true_xmeas, xmv):
    #should start at -[cost of utilities]
    return sum([production(true_xmeas),
               downtime(reset),
               mechanical(true_xmeas),
               environmental(true_xmeas),
               utilities(true_xmeas)])

def utilities(true_xmeas):
    """
    cost of compressor work and steam (inflation adjusted from 1993)
    """
    return -(true_xmeas[20] * COST_KWH
            + true_xmeas[19] * COST_STEAM)

def production(true_xmeas):
    """
    value of product
    """
    if G_H_LOWER < true_xmeas[42] < G_H_UPPER:
        return  20_000 * true_xmeas[17]
    else:
        return 0

def downtime(reset):
    """
    simple cost of assumed downtime, rather than running the simulation
    """
    return (-24*20_000 if reset else 0)

def mechanical(true_xmeas):
    if true_xmeas[7] > 12_000:
        reward = - 1e6
    else:
        reward = 0
    return reward

def environmental(true_xmeas):
    """
    TODO: check normal components of purge
    """
    if true_xmeas[35] > G_TOLERANCE:
        reward = - 1e3 * true_xmeas[10] * true_xmeas[35]
    else:
        reward = 0
    return reward
