module types
type Vessel
    real(kind=8) :: utl, utv
end type Vessel
end module types

program test
    use types
    type(Vessel) :: foo
    foo%utl = 1.
    print *, foo%utl
end program

