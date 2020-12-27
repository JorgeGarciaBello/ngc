subroutine db_chi_from_reactor_flux(t13,dmee,X,chi)
    use types
    use db_data, only: NDIM, NBIN, N_obs_H3_6AD, N_obs_H3_8AD, N_obs_H3_7AD
    implicit none
    real(dp) :: t13,dmee,X(NDIM),chi
    real(dp) :: db_expected_far_events_from_near_H1_data_P1
    real(dp) :: db_expected_far_events_from_near_H2_data_P1
    real(dp) :: db_expected_far_events_from_near_H1_data_P2
    real(dp) :: db_expected_far_events_from_near_H2_data_P2
    real(dp) :: db_expected_far_events_from_near_H1_data_P3
    real(dp) :: db_expected_far_events_from_near_H2_data_P3
    integer  :: bin
    chi=0.0_dp
    do bin=1,NBIN
        chi = chi + (N_obs_H3_6AD(bin) - db_expected_far_events_from_near_H1_data_P1(bin,t13,dmee,X) )**2/N_obs_H3_6AD(bin)
    enddo
    do bin=1,NBIN
        chi = chi + (N_obs_H3_6AD(bin) - db_expected_far_events_from_near_H2_data_P1(bin,t13,dmee,X) )**2/N_obs_H3_6AD(bin)
    enddo

    do bin=1,NBIN
        chi = chi + (N_obs_H3_8AD(bin) - db_expected_far_events_from_near_H1_data_P2(bin,t13,dmee,X))**2/N_obs_H3_8AD(bin)
    enddo
    do bin=1,NBIN
        chi = chi + (N_obs_H3_8AD(bin) - db_expected_far_events_from_near_H2_data_P2(bin,t13,dmee,X))**2/N_obs_H3_8AD(bin)
    enddo


    !do bin=1,NBIN
    !    chi = chi + (N_obs_H3_7AD(bin) - db_expected_far_events_from_near_H1_data_P3(bin,t13,dmee,X))**2/N_obs_H3_7AD(bin)    
    !enddo
    do bin=1,NBIN
       chi = chi + (N_obs_H3_7AD(bin) - db_expected_far_events_from_near_H2_data_P3(bin,t13,dmee,X))**2/N_obs_H3_7AD(bin)
    enddo   
    chi = chi + (X(1)/250.0_dp)**2 + (X(2)/250.0_dp)**2     + &
                (X(3)/(250.0_dp))**2 + (X(4)/(250.0_dp))**2 + &
                (X(5)/(500.5_dp))**2 + (X(6)/(500.75_dp))**2! + &    
                !(X(5)/(400.5_dp/100.0_dp))**2 + (X(6)/(300.5_dp/100.0_dp))**2
    return
end subroutine db_chi_from_reactor_flux