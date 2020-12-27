function db_expected_far_events_from_near_H1_data_P2(bin,t13,dmee,x)
    use types
    use db_data, only: NDIM, NBIN, RCTS, TP_d, LT_d, N_obs_H1_8AD, Bkg_H1_8AD, Bkg_H3_8AD
    implicit none
    integer, parameter :: PRD=2   ! Period of data taking
    real(dp) :: db_expected_far_events_from_near_H1_data_P2
    integer  :: bin
    real(dp) :: t13,dmee,x(NDIM)
    real(dp) :: db_near_integral_per_detector_reactor_bin_H1    
    real(dp) :: db_far_integral_per_detector_reactor_bin_H3
    real(dp) :: r1, r3
    integer  :: d,r
    r3 = 0.0_dp
    do r=1,RCTS
        r3 = r3 + TP_d(5)*LT_d(PRD,5)*db_far_integral_per_detector_reactor_bin_H3(5,r,bin,t13,dmee) + &
                  TP_d(6)*LT_d(PRD,6)*db_far_integral_per_detector_reactor_bin_H3(6,r,bin,t13,dmee) + &
                  TP_d(7)*LT_d(PRD,7)*db_far_integral_per_detector_reactor_bin_H3(7,r,bin,t13,dmee) + &
                  TP_d(8)*LT_d(PRD,8)*db_far_integral_per_detector_reactor_bin_H3(8,r,bin,t13,dmee)
    enddo
    r3 = r3*(N_obs_H1_8AD(bin) - Bkg_H1_8AD(bin)*x(3))
    !r3 = r3*( N_obs_H1_8AD(bin) )
    r1 = 0.0_dp
    do r=1,RCTS
        r1 = r1 + TP_d(1)*LT_d(PRD,1)*db_near_integral_per_detector_reactor_bin_H1(1,r,bin,t13,dmee) + &
                  TP_d(2)*LT_d(PRD,2)*db_near_integral_per_detector_reactor_bin_H1(2,r,bin,t13,dmee)
    enddo
    db_expected_far_events_from_near_H1_data_P2 = 0.98*(r3/r1) - Bkg_H3_8AD(bin)*x(4)
    !db_expected_far_events_from_near_H1_data_P2 = (r3/r1)
    return
end function db_expected_far_events_from_near_H1_data_P2