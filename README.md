"""
###############################################################################

Model of the TE (Tennessee Eastmann) challenge reactor.

The Plant takes four inputs, A, C, D, E, and produces two outputs,
G and H.

Primary reactions:

A(g) + C(g) + D(g) -> G(l)
A(g) + D(g) + E(g) -> H(l)

Byproduct reactions:

A(g) + E(g) -> F(l)
3D(g) -> 2F(l)

All are exothermic, reversible, and first-order
(rates follow an Arrhenius relation.)

Product flow is: 
a,d,e ->                                  c ->
    reactor -> condensor -> separator -> stripper -> product
          <- compressor <- purge <-
          <---------------------------------

  Differences between the code and its description in the paper:

  1.  Subroutine TEINIT has time in the argument list.  TEINIT sS.et time
      to zero.

  3.  Process disturbances 14 through 20 do NOT need to be used in
      conjunction with another disturbance as stated in the paper.  All
      disturbances can be used alone or in any combination.

###############################################################################

Notes on Fortran to Python conversion:

    Lots more OO and abstraction, in particular Stream() (13x) and Vessel() (4x).
    Also, add Sensors().  Maybe some JSON too.  Everyone like json.

    Indexing is all messed-up.  There are off-by-one errors, sure, but also
    off-by-TWO errors.  MUST replace with a dict.

    self.derivatives is zeroed?

High-level overview of run():

1 ) set IDVs
2 ) set wlk values
3 ) set Reactor ES, stream[3] properties (the latter by calling sub8)
4 ) load some state into Vessels
5 ) calculate XL from UCL
6 ) sub2?
7 ) sub4? 
8 ) set VL
9 ) calculate Pressure of A,B,C
10) calculate Pressure of D,E,F,G,H
11) calculate XVs from PPs
12) calculate whatever RRs are 
13) ditto delta_xr, XMWs
14) Set temps
15) set_heat on streams()
16) calculate flows
17) calculate flms
18) calculate flow mass fracs
19) calculate flow concs
20) temp and heat conservation
21) underflow
22) XMEAS update
23) errors (based on GROUND TRUTH - change to XMEAS)
24) Separator Energy Balance?
25) VCVs, whatever they are 
26) update derivatives

############################################################################### 


    50 State vector

#   |0|,
#   |?|,

#   |1 --- 3||4 --- 8||  9 ||10 - 12||13 - 17|| 18 ||19 - 26|| 27 ||28 - 35|| 36 |,
#   | R.ucv || R.ucl ||R.et|| S.ucv || S.ucl ||S.et|| C.ucl ||C.et|| V.ucv ||V.et|,

#   | 37|| 38|| 39-50|,
#   |twr||tws|| vpos |,

So: [R, C, S].ucv, ucl, et, twr, tws, vpos are INHERENT. All others can be derived?
"""