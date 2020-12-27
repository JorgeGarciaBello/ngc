subroutine reno_finding_out_the_reactor_flux()
    use types
    use reno_data, only: NBIN, nearObs, farObs
    implicit none
    integer  :: i, r, bin
    real(dp) :: bin_constant_flux
    real(dp) :: bin_constant_reactor_flux(NBIN)
    real(dp) :: result, bin_semi_spectrum, f_spectrum
    real(dp) :: reno_near_integral_per_reactor_bin,reno_far_integral_per_reactor_bin
    real(dp) :: reno_far_events
    real(dp) :: t13, dmee
    real(dp) :: Y(12)
    r=1
    !bin=11
    Y=0.0_dp    
    t13  = asin(sqrt(0.0856))/2.0_dp
    dmee = 2.5d-3
    do bin=1,NBIN
     print*, bin, reno_near_integral_per_reactor_bin(r,bin,t13,dmee), &
                  reno_far_integral_per_reactor_bin(r,bin,t13,dmee),  &
                  reno_far_events(bin,t13,dmee),                      &
                  farObs(bin)
    enddo

    return
end subroutine reno_finding_out_the_reactor_flux