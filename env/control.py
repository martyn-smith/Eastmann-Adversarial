from copy import deepcopy
import numpy as np
from constants import FXMEAS, SETPT


class Dummy:
    def __init__(self):
        self.xmv = np.array([0.5] * 12)

    def control(self, *args):
        return self.xmv

    def perturb_xmv(self, *args):
        pass

    def reset_single(self, *args):
        pass


class Controller:
    def __init__(self, seed, delta_t):
        #   set controller parameters
        #   r.tc, r.level, s.level, c.level, s.under, g/h, r.pg, purge.b, r.a
        #   xmvs actually used: 0,1,2, 3,5, 6,7, 9,10
        #   so 4, 8, 11 unused (compressor recycle, stripper steam, agitator respectively)
        self.setpt = deepcopy(SETPT)
        self.gain = np.array([-10.3, 0.8, 1.4, 5.0, 1.2, 7.1, 1.1, -14.5, 4.1])
        self.taui = np.array([1.9, 23.0, 96.0, 50.0, 50.0, 39.0, 49.5, 30.1, 81.9])
        self.err = np.zeros(9)
        self.xmv = np.array(seed)
        self.fxmeas = deepcopy(FXMEAS)
        self.alpha = delta_t * 3600.0 / 5.0
        self.reset_times = np.zeros(12)
        self.xmv_map = {0: 5, 1: 1, 2: 8, 3: 6, 5: 7, 6: 3, 7: 4, 9: 0, 10: 2}
        self.delta_t = delta_t

    def __str__(self):
        return f"{self.xmv}\n"

    def control(self, xmeas, time):
        """
        discrete control algorithms

        example PI controller:
          stripper level controller

          calculate error
            err  = self.setpt - xmeas[15]

          proportional-integral controller (velocity form)
              gain  =  controller gain
              taui  =  reset time (min)

          dxmv  =  gain * ( ( err - errold ) + err * DELTA_t * 60. / taui )

          xmv[8]  =  xmv[8] - dxmv

          errold  =  err

          impose integral desaturation

          xmv[8] = np.clip(self.xmv[8], 0., 100.)

         loops

         #    xmeas    setpt    xmv
         0    7        6        3
         1    9        0        9
         2    8        1        1
         3    12       2        10
         4    15       3        6
         5    17       4        7
         6    42       5        0
         7    30       7        5
         8    23       8        2

        """
        if xmeas is None:
            return self.xmv
        #    reactor temperature control (reactor temperature -> reactor coolant flow)
        if self.reset_times[9] == 0 or time - self.reset_times[9] > 1.0:
            err = self.setpt[0] - xmeas[9]
            self.xmv[9] += self.gain[0] * (
                (err - self.err[0]) + err * self.delta_t * 60.0 / self.taui[0]
            )
            self.err[0] = err
            self.xmv[9] = np.clip(self.xmv[9], 0.0, 100.0)
        # else:
        #     print("xmv[9] open")
        #    reactor level control (reactor level -> E feed?)
        if self.reset_times[1] == 0 or time - self.reset_times[1] > 1.0:
            err = self.setpt[1] - self.fxmeas[8]
            self.xmv[1] += self.gain[1] * (
                (err - self.err[1]) + err * self.delta_t * 60.0 / self.taui[1]
            )
            self.err[1] = err
            self.xmv[1] = np.clip(self.xmv[1], 0.0, 100.0)
        # else:
        #     print("xmv[1] open")
        #    product separator level control (sep level -> condensor coolant flow)
        if self.reset_times[10] == 0 or time - self.reset_times[10] > 1.0:
            err = self.setpt[2] - self.fxmeas[12]
            self.xmv[10] += self.gain[2] * (
                (err - self.err[2]) + err * self.delta_t * 60.0 / self.taui[2]
            )
            self.err[2] = err
            self.xmv[10] = np.clip(self.xmv[10], 0.0, 100.0)
        # else:
        #     print("xmv[10] open")
        #    stripper level control (strip level -> sep pot flow)
        if self.reset_times[6] == 0 or time - self.reset_times[6] > 1.0:
            err = self.setpt[3] - self.fxmeas[15]
            self.xmv[6] += self.gain[3] * (
                (err - self.err[3]) + err * self.delta_t * 60.0 / self.taui[3]
            )
            self.err[3] = err
            self.xmv[6] = np.clip(self.xmv[6], 0.0, 100.0)
        # else:
        #     print("xmv[6] open")
        #    stripper underflow control (strip underflow -> product flow)
        if self.reset_times[7] == 0 or time - self.reset_times[7] > 1.0:
            err = self.setpt[4] - xmeas[17]
            self.xmv[7] += self.gain[4] * (
                (err - self.err[4]) + err * self.delta_t * 60.0 / self.taui[4]
            )
            self.err[4] = err
            self.xmv[7] = np.clip(self.xmv[7], 0.0, 100.0)
        # else:
        #     print("xmv[7] open")
        #    g/h ratio control (ratio -> D feed?)
        if self.reset_times[0] == 0 or time - self.reset_times[0] > 1.0:
            err = self.setpt[5] - xmeas[42]
            self.xmv[0] += self.gain[5] * (
                (err - self.err[5]) + err * self.delta_t * 60.0 / self.taui[5]
            )
            self.err[5] = err
            self.xmv[0] = np.clip(self.xmv[0], 0.0, 100.0)
        # else:
        #     print("xmv[0] open")
        #    reactor pressure control (reactor pressure -> A and C feed)
        if self.reset_times[3] == 0 or time - self.reset_times[3] > 1.0:
            err = self.setpt[6] - xmeas[7]
            self.xmv[3] += self.gain[6] * (
                (err - self.err[6]) + err * self.delta_t * 60.0 / self.taui[6]
            )
            self.err[6] = err
            self.xmv[3] = np.clip(self.xmv[3], 0.0, 100.0)
        # else:
        #     print("xmv[3] open")
        #    purge gas b component control (b -> purge)
        if self.reset_times[5] == 0 or time - self.reset_times[5] > 1.0:
            err = self.setpt[7] - xmeas[30]
            self.xmv[5] += self.gain[7] * (
                (err - self.err[7]) + err * self.delta_t * 60.0 / self.taui[7]
            )
            self.err[7] = err
            self.xmv[5] = np.clip(self.xmv[5], 0.0, 100.0)
        # else:
        #     print("xmv[5] open")
        #    reactor feed a component control (reactor feed A -> A feed?)
        if self.reset_times[2] == 0 or time - self.reset_times[2] > 1.0:
            err = self.setpt[8] - xmeas[23]
            self.xmv[2] += self.gain[8] * (
                (err - self.err[8]) + err * self.delta_t * 60.0 / self.taui[8]
            )
            self.err[8] = err
            self.xmv[2] = np.clip(self.xmv[2], 0.0, 100.0)
        # else:
        #     print("xmv[2] open")

        try:
            self.fxmeas = (self.alpha * xmeas[:22]) + ((1 - self.alpha) * self.fxmeas)
        except FloatingPointError:
            pass
        return self.xmv

    def perturb_xmv(self, idx):
        self.xmv[idx] = 100.0

    def perturb_setpt(self, idx):
        try:
            self.setpt[idx] *= 10.0
        except KeyError:
            pass

    def reset_single(self, idx, time):
        self.reset_times[idx] = time
        try:
            setpt_idx = self.xmv_map[idx]
            self.setpt[setpt_idx] = SETPT[setpt_idx]
        except KeyError:
            # 4, 8, 11 have no control loops - this is fine if so
            pass
