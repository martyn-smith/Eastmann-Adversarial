module entities
!   CHALLENGE: C binding conflicts with sequence, which is more necessary.
    type agitator
         sequence
         real(kind=8) :: speed
    end type agitator

    type Compressor
        sequence
        real(kind=8) :: max_PR, max_flow, work
    end type Compressor

    type Coolant
            sequence
            real(kind=8) :: flow, T_in, T_out, h ! T_out set by state vector.  h is heat cap?
    end type Coolant

    type Vessel
        sequence
        real(kind=8) :: utl, utv, &    ! total molar amounts in liquid and gas phase
                        et, &          ! PRIMARY: total heat?
                        es, &          ! total and liquid phase heat?
                        tc, tk, &      ! temps (C and K)
                        density, &     ! self-explanatory
                        vt, vl, vv, &  ! total, liquid and vapour volumes
                        pt, &          ! total pressure
                        qu             ! work?
        real(kind=8), dimension(8) :: ucl, ucv, &    ! PRIMARY: molar components amounts in liquid and gas phase 
                                                     ! (only liquid for C, only gas for V)
                                      xl, xv, &      ! concentrations in liquid and gas phase
                                      pp             ! partial pressure
        type(Coolant) :: cl
        !contains
        !   procedure, pass :: set_temperature
    end type Vessel

    type Stream
        sequence
        real(kind=8) :: ftm, fcm(8), x(8), xmws, h, T
    end type Stream

    type Actuator
        sequence
        logical :: is_stuck
        real(kind=8) :: pos, delta_pos, commanded
    end type Actuator

    type Sensor
    end type Sensor
end module entities