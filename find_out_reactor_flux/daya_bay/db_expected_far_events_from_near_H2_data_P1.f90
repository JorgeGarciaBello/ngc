function db_expected_far_events_from_near_H2_data_P1(bin,t13,dmee,X)
    use types
    use db_data, only: NDIM, NBIN, RCTS, TP_d, LT_d, N_obs_H2_6AD, Bkg_H2_6AD, Bkg_H3_6AD, &
                       Bkg_sigma2_6AD_H2, Bkg_sigma2_6AD_H3
    implicit none
    integer, parameter :: PRD=1   ! Period of data taking
    real(dp) :: db_expected_far_events_from_near_H2_data_P1
    integer  :: bin
    real(dp) :: t13,dmee,X(NDIM)
    real(dp) :: db_near_integral_per_detector_reactor_bin_H2_P1
    real(dp) :: db_far_integral_per_detector_reactor_bin_H3_P1
    real(dp) :: r2, r3
    integer  :: d,r
    r3 = 0.0_dp
    do r=1,RCTS
        r3 = r3 + TP_d(5)*LT_d(PRD,5)*db_far_integral_per_detector_reactor_bin_H3_P1(5,r,bin,t13,dmee,X) + &
                  TP_d(6)*LT_d(PRD,6)*db_far_integral_per_detector_reactor_bin_H3_P1(6,r,bin,t13,dmee,X) + &
                  TP_d(7)*LT_d(PRD,7)*db_far_integral_per_detector_reactor_bin_H3_P1(7,r,bin,t13,dmee,X)
    enddo
    r3 = r3*( N_obs_H2_6AD(bin) - Bkg_H2_6AD(bin)*X(2) )
    r2 = 0.0_dp
    do r=1,RCTS
        r2 = r2 + TP_d(3)*LT_d(PRD,1)*db_near_integral_per_detector_reactor_bin_H2_P1(3,r,bin,t13,dmee,X)
    enddo
    db_expected_far_events_from_near_H2_data_P1 = 0.9735*(1.0_dp+X(4))*(r3/(r2)) - X(3)*Bkg_H3_6AD(bin)
    return
end function db_expected_far_events_from_near_H2_data_P1