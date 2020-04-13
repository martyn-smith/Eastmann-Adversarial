program test
    double precision foo(8)
    call bar(foo)
end program

subroutine bar(foo)
    double precision foo
    print *,* size(foo)
end