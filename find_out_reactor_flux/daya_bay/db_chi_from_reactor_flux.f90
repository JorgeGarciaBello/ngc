subroutine db_chi_from_reactor_flux(t13,dmee,X,chi)
    use types
    use db_data, only: NDIM, NBIN, N_obs_H3_6AD, N_obs_H3_8AD, N_obs_H3_7AD
    implicit none
    real(dp) :: t13,dmee,X(NDIM),chi,chi1,chi2,chi3
    integer  :: bin
    chi=0.0_dp
    call db_chi_from_reactor_flux_P1(t13,dmee,X,chi1)
    call db_chi_from_reactor_flux_P2(t13,dmee,X,chi2)
    call db_chi_from_reactor_flux_P3(t13,dmee,X,chi3)
    chi = chi + chi1 + chi2 + chi3
    !chi = chi + chi2
    return
end subroutine db_chi_from_reactor_flux