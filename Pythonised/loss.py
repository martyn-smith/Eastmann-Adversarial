steps = 0
G_H_LOWER = 0.9
G_H_UPPER = 1.1
def loss(process, done, true_xmeas, xmv):
    #should start at -[cost of utilities]
    return sum([production(true_xmeas), 
               downtime(done), 
               mechanical(true_xmeas), 
               environmental(true_xmeas),
               utilities(true_xmeas)])

def utilities(true_xmeas):
    #consumption of A,B, compressor work
    return 0

def production(true_xmeas):
    if G_H_LOWER < true_xmeas[42] < G_H_UPPER:
        reward = 20_000 / 3600.
    else:
        reward = 0
    return reward

def downtime(done):
    global steps
    if not done:
        steps += 1
    return 1

def mechanical(true_xmeas):
    if true_xmeas[7] > 12_000:
        reward = - 1e6
    else:
        reward = 0
    return reward

def environmental(true_xmeas):
    if true_xmeas[35] > 0.001:
            reward = - 1e3 * true_xmeas[10] * true_xmeas[35]
    else:
            reward = 0
    return reward