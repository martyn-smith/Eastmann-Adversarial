from teprob import TEProc


class Sensors:
    def measure(t: TEProc):
        xmeas = np.zeros(43)
        xmeas[0] = t.time
        xmeas[1] = (
            t.sm[2].ftm * 0.359 / 35.3145
        )  # A Feed  (stream 1)                    kscmh ,
        xmeas[2] = (
            t.sm[0].ftm * t.sm[0].xmws * 0.454
        )  # D Feed  (stream 2)                 kg/hr from lbmol/hr
        xmeas[3] = (
            t.sm[1].ftm * t.sm[1].xmws * 0.454
        )  # E Feed  (stream 3)                 kg/hr
        xmeas[4] = (
            t.sm[3].ftm * 0.359 / 35.3145
        )  # A and C Feed  (stream 4)              kscmh
        xmeas[5] = (
            t.sm[8].ftm * 0.359 / 35.3145
        )  # Recycle Flow  (stream 8)              kscmh
        xmeas[6] = (
            t.sm[5].ftm * 0.359 / 35.3145
        )  # Reactor Feed Rate  (stream 6)         kscmh
        xmeas[7] = t.r.pg  # Reactor Pressure                   kPa gauge
        xmeas[8] = t.r.level * 100.0  #                     %
        xmeas[
            9
        ] = t.r.tc  # Reactor Temperature                                      deg C
        xmeas[10] = (
            t.sm[9].ftm * 0.359 / 35.3145
        )  # purge rate (stream 9)               kscmh
        xmeas[
            11
        ] = t.s.tc  # product sep temp                                        deg c
        xmeas[12] = t.s.level * 100.0  # product sep level                    %
        xmeas[13] = (
            (t.s.pt - 760.0) / 760.0 * 101.325
        )  # sep pressure                      kpa gauge
        xmeas[14] = (
            t.sm[10].ftm / t.s.density / 35.3145
        )  # sep underflow (stream 10)       m3/hr
        xmeas[15] = (
            (t.c.vl - 78.25) / t.c.vt * 100.0
        )  # stripper level                       %
        xmeas[16] = (
            (t.j.pt - 760.0) / 760.0 * 101.325
        )  # stripper pressure                 kpa gauge
        xmeas[17] = (
            t.sm[12].ftm / t.c.density / 35.3145
        )  # stripper underflow (stream 11, aka production) m3/hr
        xmeas[
            18
        ] = t.c.tc  # stripper temperature                                    deg c
        xmeas[19] = (
            t.c.qu * 1.04e3 * 0.454
        )  # stripper steam flow                        kg/hr
        xmeas[20] = (
            t.cmpsr.work * 0.29307e3
        )  # compressor work, again??                kwh
        xmeas[
            21
        ] = t.r.cl.T_out  # reactor cooling water outlet temp                 deg c
        xmeas[
            22
        ] = t.s.cl.T_out  # separator cooling water outlet temp               deg c

        if idv(16):
            if xmeas_tgt == 0:
                xmeas_tgt = np.ceiling(rand() * 42.0)

            if time > 0.012 and time < 0.027:
                xmeas[xmeas_tgt] = 0.0

        if idv(21):
            if time > 0.012 and time < 0.027:
                xmeas[9] = 500.0

        if t.time_since_gas >= t.T_GAS or not hasattr(
            t, "gas"
        ):  # purge gas and reactor feed analysis
            t.gas = (t.sm[5].x[:-2], t.sm[8].x)
            t.time_since_gas = 0.0
        xmeas[23:29] = t.gas[0] * 100
        xmeas[29:37] = t.gas[1] * 100
        t.time_since_gas += DELTA_t

        if t.time_since_prod >= t.T_PROD or not hasattr(
            t, "prod"
        ):  # product feed analysis
            t.prod = t.sm[12].x[3:]
            t.time_since_prod = 0.0
        xmeas[37:42] = t.prod * 100
        t.time_since_prod += DELTA_t

        xmeas[42] = (xmw[6] * xmeas[40]) / (xmw[7] * xmeas[41])
        return xmeas
