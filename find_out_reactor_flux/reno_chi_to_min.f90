function reno_chi_to_min(X,XX)
    use types    
    use reno_data, only: NDIM, NBIN, nearObs, detector_efficiency, TP_r,TP_d, LT_d, bkg, farObs
    implicit none
    real(dp) :: reno_chi_to_min
    integer  :: bin,r
    real(dp) :: t13,dmee
    real(dp) :: X(NDIM)
    real(dp) :: XX(*)
    real(dp) :: far_events
    real(dp) :: reno_far_reactor_flux
    real(dp) :: reno_near_reactor_flux    
    real(dp) :: reno_far_integral_per_reactor_bin
    real(dp) :: reno_near_integral_per_reactor_bin    
    real(dp) :: result
    
    t13  = XX(1)
    dmee = XX(2)
    
    result = 0.0_dp
    do bin=1,NBIN
        reno_far_reactor_flux=0.0_dp
        reno_near_reactor_flux=0.0_dp
        do r=1,6
            !reno_far_reactor_flux  = reno_far_reactor_flux  + TP_r(r)*reno_far_integral_per_reactor_bin(r,bin,t13,dmee)
            !reno_near_reactor_flux = reno_near_reactor_flux + TP_r(r)*reno_near_integral_per_reactor_bin(r,bin,t13,dmee)
            reno_far_reactor_flux  = reno_far_reactor_flux  + reno_far_integral_per_reactor_bin(r,bin,t13,dmee)
            reno_near_reactor_flux = reno_near_reactor_flux + reno_near_integral_per_reactor_bin(r,bin,t13,dmee)
        enddo
        far_events = ( LT_d(2)/LT_d(1) )*( nearObs(bin) - X(1)*bkg(bin,1) )*( reno_far_reactor_flux/reno_near_reactor_flux )        
        result = result + ( farObs(bin) - 1.166_dp*far_events*(1.0_dp + X(3)) - X(2)*bkg(bin,2) )**2/(farObs(bin))        
    enddo    
    result=result+(X(1)/(9.5_dp*3.26_dp/100.0_dp))**2+(X(2)/(5.0d0*5.61_dp/100.0_dp))**2+( X(3)/(0.4_dp/100.0_dp) )**2
    reno_chi_to_min=result
    return
end function reno_chi_to_min