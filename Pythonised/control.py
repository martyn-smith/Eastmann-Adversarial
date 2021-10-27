import numpy as np
from teprob import DELTA_t
from enum import Enum

GH_MODES = {
     1 : 1.0,
     2 : 0.1,
     3 : 9.0
}


class Dummy:
     xmv = np.array([0.5] * 12)

class Controller:

    def __init__(self, seed, prod_mode):
    #   set controller parameters
    #   reactor temperature control
        self.setpt = np.array([120.40, 75.0, 50.0, 50.0, 
                               22.949, GH_MODES[prod_mode], 2705.0, 13.823,
                               32.188])
        self.gain = np.array([-10.3, 0.8, 1.4, 5.,
                              1.2, 7.1, 1.1, -14.5,
                              4.1])
        self.taui = np.array([1.9, 23., 96., 50.,
                              50., 39., 49.5, 30.1,
                              81.9])
        self.err = np.zeros(9)
        self.xmv = np.array(seed)
        self.fxmeas = np.zeros(22)
        self.alpha = DELTA_t*3600. / 5.0

    def control(self, xmeas):#TODO: prod_mode)
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
        """     
    #   reactor pressure control (reactor pressure -> A and C feed)
        err = self.setpt[6] - xmeas[7]
        self.xmv[3] += self.gain[6] * ((err - self.err[6]) + err * DELTA_t * 60. / self.taui[6])
        self.err[6] = err
        self.xmv[3] = np.clip(self.xmv[3], 0., 100.)  
   #    reactor temperature control (reactor temperature -> reactor coolant flow)
        err = self.setpt[0]-xmeas[9]
        self.xmv[9] += self.gain[0]*((err-self.err[0])+err*DELTA_t*60./self.taui[0])
        self.err[0] = err
        self.xmv[9] = np.clip(self.xmv[9], 0., 100.)
   #    reactor level control (reactor level -> D feed)
        err = self.setpt[1]-self.fxmeas[8]
        self.xmv[1] += self.gain[1]*((err-self.err[1])+err*DELTA_t*60./self.taui[1])
        self.err[1] = err
        self.xmv[1] = np.clip(self.xmv[1], 0., 100.)
   #    product separator level control (sep level -> condensor coolant flow)
        err = self.setpt[2]-self.fxmeas[12]
        self.xmv[10] += self.gain[2]*((err-self.err[2])+err*DELTA_t*60./self.taui[2])
        self.err[2] = err
        
        self.xmv[10] = np.clip(self.xmv[10], 0., 100.)
   #    stripper level control (strip level -> sep pot flow)
        err = self.setpt[3]-self.fxmeas[15]
        self.xmv[6] += self.gain[3]*((err-self.err[3])+err*DELTA_t*60./self.taui[3])
        self.err[3] = err
        self.xmv[6] = np.clip(self.xmv[6], 0., 100.)
   #    stripper underflow control (strip underflow -> product flow)
        err = self.setpt[4]-xmeas[17]
        self.xmv[7] += self.gain[4]*((err-self.err[4])+err*DELTA_t*60./self.taui[4])
        self.err[4] = err
        self.xmv[7] = np.clip(self.xmv[7], 0., 100.)
   #    g/h ratio control (ratio -> A feed)
        err = self.setpt[5]-xmeas[42]
        self.xmv[0] += self.gain[5]*((err-self.err[5])+err*DELTA_t*60./self.taui[5])
        self.err[5] = err
        self.xmv[0] = np.clip(self.xmv[0], 0., 100.)
   #    purge gas b component control (b -> reactor feed rate)
        err = self.setpt[7]-xmeas[30]
        self.xmv[5] += self.gain[7]*((err-self.err[7])+err*DELTA_t*60./self.taui[7])
        self.err[7] = err
        self.xmv[5] = np.clip(self.xmv[5], 0., 100.)
   #    reactor feed a component control (reactor feed A -> E feed)
        err = self.setpt[8]-xmeas[23]
        self.xmv[2] += self.gain[8]*((err-self.err[8])+err*DELTA_t*60./self.taui[8])
        self.err[8] = err
        self.xmv[2] = np.clip(self.xmv[2], 0., 100.)

        self.fxmeas = self.alpha * xmeas[1:23] + (1-self.alpha) * self.fxmeas

