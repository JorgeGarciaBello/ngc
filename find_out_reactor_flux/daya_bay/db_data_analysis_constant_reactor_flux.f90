subroutine db_data_analysis_constant_reactor_flux()
    use types
    use db_data, only: NBIN
    use neu_osc_parameters, only: Y
    implicit none
    integer, parameter :: n=18
    real(dp) :: dmee, t13, bkg_f, bkg_n, chi    
    real(dp) :: data(n,n,n)
    real(dp) :: dmee_shift, t13_shift, alpha_shift
    real(dp) :: dmee_data(n), t13_data(n), alpha_data(n)
    real(dp) :: t13_i,   t13_f
    real(dp) :: dmee_i,  dmee_f
    real(dp) :: alpha_i, alpha_f    
    real(dp) :: alpha_i_j(n,n)    
    integer  :: i, j, k, l, u, bin, d, r

    real(dp) :: db_near_integral_per_detector_reactor_bin_H1
    real(dp) :: db_near_integral_per_detector_reactor_bin_H2
    real(dp) :: db_far_integral_per_detector_reactor_bin_H3
    real(dp) :: db_expected_far_events_from_near_data
    real(dp) :: result 

    print*, 'Grid db: ', n
    t13_i=asin(sqrt(0.075))/2.0_dp
    t13_f=asin(sqrt(0.10))/2.0_dp
    dmee_i=2.2d-3
    dmee_f=2.8d-3
!    alpha_i=0.75_dp
!    alpha_f=1.25_dp
    t13_shift   = (t13_f   - t13_i)/real(n)
    dmee_shift  = (dmee_f  - dmee_i)/real(n)
    !alpha_shift = (alpha_f - alpha_i)/real(n)
    do i=1,n
        t13_data(i)   = t13_i   +  t13_shift*i
        dmee_data(i)  = dmee_i  +  dmee_shift*i        
    enddo    
    do i=1,n
        !$omp parallel do private (t13,dmee,chi)
        do j=1,n
            t13   = t13_data(i)
            dmee  = dmee_data(j)            
            !call db_chi_square_constant_reactor_flux(t13,dmee,chi)
            call db_chi_square_constant_reactor_flux_newoua(t13,dmee,chi)
            data(i,j,1) = chi
        enddo
        !$omp end parallel do
        print*,i
    enddo
    open(newunit=u, file='find_out_reactor_flux/daya_bay/db_data.dat')
    do i=1,n
        do j=1,n            
            write(u,*) sin(2.0_dp*t13_data(i))**2, dmee_data(j), data(i,j,1)                 
        enddo
    enddo
    close(u)
    return
end subroutine db_data_analysis_constant_reactor_flux