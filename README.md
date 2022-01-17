*Note: this README, as it stands, is primarily documenting the internal approach of the TE model itself, not the interface it exposes.*

Model of the TE (Tennessee Eastmann) challenge reactor
===

The Plant takes four inputs, A, C, D, E, and produces two outputs, G and H.

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

The plant has unreliable sensors, and unreliable actuators. It may, from time to time, suffer DoS attacks. All of the above are configurable, and the process can run realtime or fast, verbose or not. The plant can also load an initial state at startup.

Environment
---

For the Q-network (which requires discrete actions) variant, the following actions are exposed to red and blue team:

Red team actions

0..=11 => set xmv[i] to MAX
12..=53 => set xmeas[i-12] to 0.
54..=62 => setpt[i-54] *= 10
63 => no action

Blue team actions

0..=11 => reset PLC 0-11 (TEproc will resort to open-loop for that PLC for one hour)
12 => restart entire plant (no production for 24 hours)
13 => continue (no action, no reward)

Notes
---

Folder structure is as follows:

- builds - old builds we're unlikely to need 
- utils - cleaning and seeding utilities
- studies - experiment scripts
- *.mod - autogenerated, ignore/autoremove
- reference - src for even older builds
- teststates - contains test states

- constants.f95 - constants, as implied
- temain.f95 - driver and integrator
- teprob.f95 - process loop
- tewalk.f95 - random variations and attacks
- tesense.f95 - sensor loop
- tecontrol.f95 - control loop
- teout.f95 - formats output

- teloss.f95 sketches out loss measurement, but is not included.

Major changes occurred:

080420 - first major refactor, adjustable disturbances, played with csv log format and then reverted.
260521 - breaking change, new log format
xx1221 - Pythonised version with support for discrete inputs added

Building
---

Fortran:

Simply run make in the implementations directory to compile. The makefile compiles and datestamps the binary in debug and release mode, then updates the symlink (so invoking ./te always invokes the latest build), and test runs. Keeping older binaries is for use in case of regression, though this probably isn't that useful.

Python:

Simply run teprob.py in the Pythonised directory.

Program Loop
---

The program operates with temain as the driver, teprob as the process loop, and tecontrol generating control signals. Work in progress: adding a tesense.

High-level overview of teprob():

1. set IDVs
2. set wlk values
3. set Reactor ES, stream[3] properties (the latter by calling sub8)
4. load some state into Vessels
5. calculate XL from UCL
6. removed
7. removed
8. set VL
9. calculate Pressure of A,B,C
10. calculate Pressure of D,E,F,G,H
11. calculate XVs from PPs
12. calculate reaction rate coefficients
13. calculate change in reactant concentrations in Reactor
14. Set temps
15. set_heat on streams()
16. calculate flows
17. calculate flms
18. calculate flow mass fracs
19. calculate flow concs
20. temp and heat conservation
21. underflow
22. XMEAS update
23. errors (based on GROUND TRUTH - change to XMEAS)
24. Separator Energy Balance?
25. VCVs, whatever they are 
26. update derivatives

The process represents internal state with 50 floating points, plus 24 booleans. The 50 states are:

```
|1 --- 3||4 --- 8||  9 ||10 - 12||13 - 17|| 18 ||19 - 26|| 27 ||28 - 35|| 36 | 37|| 38|| 39-50|
| R.ucv || R.ucl ||R.et|| S.ucv || S.ucl ||S.et|| C.ucl ||C.et|| V.ucv ||V.et|twr||tws|| vpos |
```

These elements of the model are inherent, all others should be derivable.

Logging
---

The process logs to whatever directory it is invoked in. The state vector in textual format is represented in 23 chars, 15 point precision, so a simple state vector is 1.15 kB.

The full output is:

state.dat (50*23) 
idvs.dat (24*3)
xmeas.dat (41*23)
xmv.dat (12*23)

This represents 2.44 kB per timestep. (A binary representation would need only 632 B). Looks like this:

```
0.000000000000000E+00  0.104049138900000E+02  0.436399601700000E+01  0.757005973700000E+01  0.423004243100000E+00  0.241551343700000E+02  0.294259764500000E+01  0.154377065500000E+03  0.159186596000000E+03  0.280852272300000E+01  0.637558119900000E+02  0.267402606600000E+02  0.463853243200000E+02  0.246452154300000E+00  0.152048440400000E+02  0.185226617200000E+01  0.524463945900000E+02  0.412039400800000E+02  0.569931776000000E+00  0.430605637600000E+00  0.799062007830000E-02  0.905603608900000E+00  0.160542582160000E-01  0.750975968700000E+00  0.885828559550000E-01  0.482772619300000E+02  0.393845902800000E+02  0.375529725700000E+00  0.107756269800000E+03  0.297725054600000E+02  0.883248113500000E+02  0.230392950700000E+02  0.628584879400000E+02  0.554631868800000E+01  0.119224477200000E+02  0.555544824300000E+01  0.921848976200000E+00  0.945992754900000E+02  0.772969835300000E+02  0.630526303900000E+02  0.539797067700000E+02  0.246435575500000E+02  0.613019214400000E+02  0.222100000000000E+02  0.400637467300000E+02  0.381003437000000E+02  0.465341558200000E+02  0.474457345600000E+02  0.411058128800000E+02  0.181134905500000E+02  0.500000000000000E+02
```
