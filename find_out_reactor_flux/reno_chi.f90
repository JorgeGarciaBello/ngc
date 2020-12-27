!!subroutine reno_chi(t13,dmee,alp,BN,result)
!subroutine reno_chi(t13,dmee,result)
!    use types    
!    use reno_data, only: NDIM, NBIN, nearObs, detector_efficiency, TP_d, LT_d, bkg
!    implicit none
!    integer  :: bin,r
!    real(dp) :: t13,dmee,BN
!    real(dp) :: X(NDIM)
!    real(dp) :: reno_far_events
!    real(dp) :: reno_far_reactor_flux
!    real(dp) :: reno_near_reactor_flux    
!    real(dp) :: reno_far_integral_per_reactor_bin
!    real(dp) :: reno_near_integral_per_reactor_bin
    !
!    real(dp) :: reno_far_events
!    integer  :: bin,j 
!    real(dp) :: result
!
!    reno_far_reactor_flux=0.0_dp
!    reno_near_reactor_flux=0.0_dp    
!    do r=1,6
!        reno_far_reactor_flux  = reno_far_reactor_flux  + reno_far_integral_per_reactor_bin(r,bin,t13,dmee)
!        reno_near_reactor_flux = reno_near_reactor_flux + reno_near_integral_per_reactor_bin(r,bin,t13,dmee)        
!    enddo
!    reno_far_events = ( LT_d(2)/LT_d(1) )*( nearObs(bin) )*( reno_far_reactor_flux/reno_near_reactor_flux )    
!
!    result = 0.0_dp
!    do bin=1,NBIN
!        result = result + (farObs(bin) - 1.1671_dp*reno_far_events(bin,t13,dmee) )**2/farObs(bin)        
!    enddo
!    return
!end subroutine reno_chi