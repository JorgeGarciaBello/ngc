subroutine db_chi_square_constant_reactor_flux(t13,dmee,chi)
    use types    
    implicit none
    real(dp) :: dmee,t13,chi
    real(dp) :: pr
    integer ( kind = 4 ), parameter :: mdim=2
    real ( kind = 8 ) myp(mdim)
    
    myp=0.0_dp
    myp(1) = t13
    myp(2) = dmee
    call db_minimization_with_PRAXIS(pr,myp,mdim)
    chi=pr
    return
end subroutine db_chi_square_constant_reactor_flux