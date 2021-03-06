!subroutine reno_chi_square_constant_reactor_flux(dmee,t13,chi)
subroutine reno_chi_square_constant_reactor_flux(dmee,t13,alpha,chi)
    use types
    use reno_data, only: NDIM
    use neu_osc_parameters, only: Y
    implicit none
    real(dp) :: dmee,t13, alpha,chi
    real(dp) :: X(NDIM),W(10000)
    integer  :: IPRINT=2
    integer  :: MAXFUN=5000
    real(dp) :: RHOBEG
    real(dp) :: RHOEND=1.0D-6
    integer  :: N,NPT
    real(dp) :: reno_chi_from_near_constant_flux
    real(dp) :: XX(2),RMIN
    
    XX=0.0_dp
    XX(1) = t13
    XX(2) = dmee    

    N=NDIM
    NPT=2*N+1
    X(1)=DFLOAT(1)/DFLOAT(N+1)
    RHOBEG=0.2D0*X(1)  
    CALL NEWUOA (N,NPT,X,RHOBEG,RHOEND,IPRINT,MAXFUN,W,XX,RMIN)
    chi=RMIN
    return
end subroutine reno_chi_square_constant_reactor_flux