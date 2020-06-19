module types
type Vessel
    real(kind=8) :: utl, utv
end type Vessel
end module types

program test
    real, parameter :: a(8) = [1., 2., 3., 4., 5., 6., 7., 8.]
    real, parameter :: b = 2.
    real :: c(8)
    c = a * b
    print *, c
end program

