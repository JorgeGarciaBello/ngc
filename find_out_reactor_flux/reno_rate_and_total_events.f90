subroutine reno_rate_and_total_events(t13,dmee,plt,chi)
    use types
    use neu_osc_parameters, only: Y
    use reno_data, only: NDIM, NBIN, nearObs, detector_efficiency, TP_d, LT_d, bkg, farObs
    implicit none
    
    integer  :: bin,r
    real(dp) :: t13,dmee,plt,chi
    real(dp) :: X(NDIM)
    real(dp) :: far_events
    real(dp) :: reno_far_reactor_flux
    real(dp) :: reno_near_reactor_flux    
    real(dp) :: reno_far_integral_per_reactor_bin
    real(dp) :: reno_near_integral_per_reactor_bin    
    real(dp) :: result
    real(dp) :: t_exp_events, t_obs_events
    !sdmee = Y(2)
    !t13  = Y(5)
    result = 0.0_dp
    t_exp_events=0.0_dp
    t_obs_events=0.0_dp
    do bin=1,NBIN
        reno_far_reactor_flux=0.0_dp
        reno_near_reactor_flux=0.0_dp
        do r=1,6
            reno_far_reactor_flux  = reno_far_reactor_flux  + reno_far_integral_per_reactor_bin(r,bin,t13,dmee)
            reno_near_reactor_flux = reno_near_reactor_flux + reno_near_integral_per_reactor_bin(r,bin,t13,dmee)        
        enddo
        far_events = ( LT_d(2)/LT_d(1) )*( nearObs(bin) )*( reno_far_reactor_flux/reno_near_reactor_flux )
        t_exp_events = t_exp_events + far_events
        t_obs_events = t_obs_events + farObs(bin)
        !result = result + ( farObs(bin) - 1.1671_dp*far_events )**2/farObs(bin)
        result = result + ( farObs(bin) - 1.16_dp*far_events )**2/farObs(bin)
        !result = result + ( farObs(bin) - far_events )**2/farObs(bin)
    enddo
    !result = result + ( X(1)/(3.26_dp/100.0_dp) )**2 + ( X(2)/(5.61_dp/100.0_dp) )**2
    !print*, t_obs_events, t_exp_events 
    result = result + ( t_obs_events - 1.16_dp*t_exp_events )**2 / t_obs_events
    !result = result + ( t_obs_events - t_exp_events )**2/t_obs_events
    chi = result
    return
end subroutine reno_rate_and_total_events