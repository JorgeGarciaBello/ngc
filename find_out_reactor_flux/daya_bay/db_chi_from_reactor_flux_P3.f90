subroutine db_chi_from_reactor_flux_P3(t13,dmee,X,chi)
    use types
    use db_data, only: NDIM, NBIN, N_obs_H3_7AD
    implicit none
    real(dp) :: t13,dmee,X(NDIM),chi
    real(dp) :: db_expected_far_events_from_near_H1_data_P3
    real(dp) :: db_expected_far_events_from_near_H2_data_P3
    real(dp) :: alp=1.3_dp
    integer  :: bin
    chi=0.0_dp
    do bin=1,NBIN
        chi = chi + &
        (N_obs_H3_7AD(bin) - db_expected_far_events_from_near_H1_data_P3(bin,t13,dmee,X))**2/(N_obs_H3_7AD(bin)*alp)
    enddo
    do bin=1,NBIN
       chi = chi + &
       (N_obs_H3_7AD(bin) - db_expected_far_events_from_near_H2_data_P3(bin,t13,dmee,X))**2/(N_obs_H3_7AD(bin)*alp)
    enddo   
    chi=chi + (X(1)/(5.5_dp/100.0_dp))**2 + (X(2)/(5.5_dp/100.0_dp))**2 + (X(3)/(8.0_dp/100.0_dp))**2  + &
              (X(4)/(0.33_dp/100.0_dp))**2 + (X(5)/(4.5_dp/100.0_dp))**2
    return
end subroutine db_chi_from_reactor_flux_P3