function reno_chi_from_near_constant_flux(X)
    use types
    use reno_data, only: NDIM, NBIN, farObs, bkg, alp
    use neu_osc_parameters, only: Y
    implicit none
    real(dp) :: reno_chi_from_near_constant_flux
    real(dp) :: alpha, X(NDIM)
    real(dp) :: chi
    integer  :: bin,j,r
    real(dp) :: f_spectrum, result    
    chi=0.0_dp
    do bin=1,NBIN
        !call reno_semi_far_spectrum_per_bin(bin,Y,f_spectrum)
        f_spectrum=0.0_dp
        do r=1,6
            call reno_semi_far_spectrum_per_reactor_and_bin(r,bin,Y,X,result)
            !f_spectrum = f_spectrum + result
            f_spectrum = f_spectrum + result*(1.0_dp + X(1))
        enddo
        !chi = chi + ( farObs(bin) - f_spectrum )**2/farObs(bin)
        !chi = chi + ( farObs(bin) - 1.1671_dp*f_spectrum*(1.0_dp + X(2) + X(3)) - bkg(bin,2)*X(4) )**2/farObs(bin)
        !chi = chi + ( farObs(bin) - f_spectrum*X(6)*(1.0_dp + X(2) + X(3)) - bkg(bin,2)*X(4) )**2/farObs(bin)
        chi = chi + ( farObs(bin) - f_spectrum*1.158*(1.0_dp + X(2) + X(3)) )**2/(farObs(bin)*1.6_dp)
        !chi = chi + ( farObs(bin) - f_spectrum*alp*(1.0_dp + X(2) + X(3)) )**2/farObs(bin)
        !chi = chi + ( farObs(bin) - f_spectrum*alp*(1.0_dp + X(2)) ) **2/farObs(bin)
    end do
    do r=1,6
        chi = chi + ( X(1) / ( 0.9_dp/100.0_dp  ) )**2          ! Flujo de Reactor
    enddo
    chi = chi +  (X(2)  / ( 0.21_dp/100.0_dp ))**2 +  &         ! Detection undcertaintt
                 (X(3)  / ( 0.15_dp/100.0_dp ))**2 +  &         ! Energy scale
                 (X(4)  / ( 1.5_dp*5.61_dp/100.0_dp ))**2 +  &  ! Far bkg
                 (X(5)  / ( 1.5_dp*3.26_dp/100.0_dp ))**2       ! Near bkg
    reno_chi_from_near_constant_flux = chi
    return
end function reno_chi_from_near_constant_flux