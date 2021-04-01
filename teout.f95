subroutine outputinit
    open(unit=30,file='out.dat',status='unknown')
    write(30,*) "time  ", "reactor_level  ", "reactor_temperature  ", "product_sep_level  ", &
                "strip_level  ", "reactor_pressure  ", "strip_underflow  ", "b_purge_prct  ", &
                "prod_g/h  ", "d_feed  ", "reactor_cool  ", "condensor_cool  ", & 
                "sep_prod_flow  ", "a_c_feed  ", "strip_prod_flow  ", "purge_valve  ", &
                "a_feed  ", "reactor_a/c  ", "e_feed", "  compressor_work"
!   filtered measurements
    open(unit=40,file='fout.dat',status='unknown')
    write(40,*) "time  ", &
                "(f)a_feed  ", "(f)d_feed  ", "(f)e_feed  ", "(f)a_c_feed  ", "(f)recycle  ", &
                "(f)reactor_feed  ", "(f)reactor_pressure  ", "(f)reactor_level  ", "(f)reactor_temperature  ", &
                "(f)purge  ", &
                "(f)product_temp  ",  "(f)product_level  ", "(f)product_pressure  ", "(f)prod_underflow  ", &
                "(f)strip_level  ", "(f)strip_pressure  ", "(f)strip_underflow  ", "(f)strip_temp  ", &
                "(f)strip_steam  ", "(f)compress_work  ", "(f)reactor_cool_out_temp  ", "(f)sep_cool_out_temp  ", &
                "reactor_feed_a_prct  ", "reactor_feed_b_prct  ", "reactor_feed_c_prct  ","reactor_feed_d_prct  ", &
                "reactor_feed_e_prct  ","reactor_feed_f_prct  ", &
                "purge_a_prct  ", "purge_b_prct  ", "purge_c_prct  ", "purge_d_prct  ", &
                "purge_e_prct  ", "purge_f_prct  ", "purge_g_prct  ", "purge_h_prct  ", &
                "prod_d_prct  ", "prod_e_prct  ", "prod_f_prct  ", "prod_g_prct  ",  "prod_h_prct  ", &
                "prod_g_h_ratio"
!   manipulated variables
    open(unit=50,file='inpt.dat',status='unknown')
    write(50,*) "time  ", "a_feed  ", "d_feed  ", "e_feed  ", "a_c_feed  ", "compressor_valve  ", &
                "purge_valve  ", "sep_underflow  ", "strip_underflow  ", "strip_valve  ", &
                "reactor_cool  ", "condensor_cool  ", "agitator_spd"
!   attack / disturbance times (one-hot)
    open(unit=60,file='dvec.dat')
    write(60,*) "time  ", "a_c_ratio  ", "b_comp  ", "d_feed  ", "reactor_cool_step  ", "condensor_cool_step  ", &
                "a_feed_loss  ", "c_Pressure_loss  ", "a_b_c_feed  ", "d_T  ", "c_T  ", "reactor_cool_T  ", "condensor_cool_T  ", &
                "reaction_kinetics  ", "reactor_cool_stick  ", "condensor_cool_stick  ", &
                "unknown  ", "unknown  ", "unknown  ", "unknown  ", "unknown  ", &
                "Reactor_Integrity  ", "DDoS_1  ", "D_feed_DDoS  ", "Noise_1"
end subroutine outputinit

subroutine output(time)
!   measurement and valve common block
    integer :: idv, k
    real(kind=8) :: xmeas, xmv, time, fxmeas, taufil, alpha
    common /pv/ xmeas(42), xmv(12)
    common /filter/ alpha(22), fxmeas(22), taufil(22)
    common /dvec/ idv(24)

!   write output every n samples
    write(30,101) time, xmeas(8), xmeas(9), xmeas(12), &
        xmeas(15), xmeas(7), xmeas(17), xmeas(30), &
        xmeas(42), xmv(2), xmv(10), xmv(11), &
        xmv(7), xmv(4), xmv(8), xmv(6), &
        xmv(1), xmeas(23)/xmeas(25), xmv(3), xmeas(20)
    write(40,102) time, (fxmeas(k), k=1,22),(xmeas(k),k=23,42)
    write(50,103) time, (xmv(k),k=1,12)
    write(60,104) time, (idv(k), k=1,24)

!   format statements for output.
101 format(21e23.15)
102 format(43e23.15)
103 format(13e23.15)
104 format(e23.15, 24i3)
    return
end subroutine output