function f(x,n,myp,mdim)
    use types    
    implicit none
    integer ( kind = 4 ) mdim
    real ( kind = 8 ) myp(mdim)
    integer ( kind = 4 ) n
    real ( kind = 8 ) f
    real ( kind = 8 ) x(n)
    real ( kind = 8 ) :: t13, dmee, chi
    
    t13  = myp(1)
    dmee = myp(2)
    call db_chi_from_reactor_flux(t13,dmee,x,chi)
    f = chi
    !f = x(1)**2 + x(2)**2 + 5.0d0

    return
end function f


