subroutine reno_data_analysis_constant_reactor_flux()
    use types
    use reno_data, only: NBIN, alp
    use neu_osc_parameters, only: Y
    implicit none
    integer, parameter :: n=200
    real(dp) :: dmee, t13, alpha, bkg_f, bkg_n, chi
    !real(dp) :: data(n,n,n,n)
    real(dp) :: data(n,n,n)
    real(dp) :: dmee_shift, t13_shift, alpha_shift, BF_shift, BN_shift
    real(dp) :: dmee_data(n), t13_data(n), alpha_data(n), BF_data(n), BN_data(n)
    real(dp) :: t13_i,   t13_f
    real(dp) :: dmee_i,  dmee_f
    real(dp) :: alpha_i, alpha_f
    real(dp) :: BF_i, BF_f, BN_i, BN_f
    real(dp) :: alpha_i_j(n,n)    
    integer  :: i, j, k, l, u
    print*, 'Grid: ', n

    t13_i=asin(sqrt(0.080))/2.0_dp
    t13_f=asin(sqrt(0.10))/2.0_dp

    dmee_i=2.0d-3
    dmee_f=3.3d-3

    alpha_i=0.75_dp
    alpha_f=1.25_dp

    BF_i =-5.61_dp/100.0_dp
    BF_f = 5.61_dp/100.0_dp
    
    BN_i = -3.26_dp/100.0_dp 
    BN_f =  3.26_dp/100.0_dp

    t13_shift   = (t13_f   - t13_i)/real(n)
    dmee_shift  = (dmee_f  - dmee_i)/real(n)
    alpha_shift = (alpha_f - alpha_i)/real(n)

    BF_shift = (BF_f - BF_i)/real(n)
    BN_shift = (BN_f - BN_i)/real(n)

    do i=1,n
        t13_data(i)   = t13_i   +  t13_shift*i
        dmee_data(i)  = dmee_i  +  dmee_shift*i
        alpha_data(i) = alpha_i +  alpha_shift*i
        BF_data(i)    = BF_i    +  BF_shift*i
        BN_data(i)    = BN_i    +  BN_shift*i        
    enddo    
    

    do i=1,n
        do j=1,n
            !!$omp parallel do private (t13,dmee,alpha,chi,Y)
            !!$omp parallel do private (t13,dmee,alpha,chi)
            !do k=1,n                
                    t13   = t13_data(i)
                    dmee  = dmee_data(j)
                    !alpha = alpha_data(k)
                    call reno_chi_square_constant_reactor_flux(dmee,t13,alpha,chi)                    
                    data(i,j,1) = chi

                    !###################################################################
                    !
                    !                   Code for RENO minimazing the 5 pulls
                    !
                    !       t13   = t13_data(i)
                    !       dmee  = dmee_data(j)            
                    !       alpha = alpha_data(k)
                    !       call reno_chi_square_constant_reactor_flux(dmee,t13,alpha,chi)
                    !       data(i,j,k) = chi
                    !
                    !###################################################################


                    !call reno_rate_and_total_events(t13,dmee,alpha,chi)
                    
                    !data(i,j,k,l) = chi                
                !enddo
                !!$omp end parallel do
            !enddo
            !!$omp end parallel do
        enddo
        print*,i
    enddo
    open(newunit=u, file='find_out_reactor_flux/only_data_sigma_as_exp_1.dat')
    !open(newunit=u, file='find_out_reactor_flux/only_data_rate_and_total.dat')
    !open(newunit=u, file='find_out_reactor_flux/only_bgk_data.dat')
    !open(newunit=u, file='find_out_reactor_flux/only_reactor_flux_data.dat')
    !open(newunit=u, file='find_out_reactor_flux/all_systematics_data.dat')
    !open(newunit=u, file='find_out_reactor_flux/only_norma_data.dat')
    !open(newunit=u, file='find_out_reactor_flux/norma_and_all_systematics_data.dat')
    do i=1,n
        do j=1,n
            !do k=1,n
                !do l=1,n
                 write(u,*) sin(2.0_dp*t13_data(i))**2, dmee_data(j), data(i,j,1)
                 !write(u,*) sin(2.0_dp*t13_data(i))**2, dmee_data(j), BF_data(k), BN_data(l), data(i,j,k,l)
                 !write(u,*) sin(2.0_dp*t13_data(i))**2, dmee_data(j), alpha_i_j(i,j), data(i,j,1)                 
                 !write(u,*) sin(2.0_dp*t13_data(i))**2, dmee_data(j), data(i,j,1)
                !enddo
            !enddo
        enddo
    enddo
    close(u)
    return
end subroutine reno_data_analysis_constant_reactor_flux




