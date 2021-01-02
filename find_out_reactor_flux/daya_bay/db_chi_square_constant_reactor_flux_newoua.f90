subroutine db_chi_square_constant_reactor_flux_newoua(t13,dm,chi)
    use types
    use db_data, only: NDIM
    implicit none
    real(dp) :: t13,dm,chi
    real(dp) :: YX
    real(dp) :: X(NDIM),W(10000),XX(2),RHOEND,RHOBEG
    INTEGER  :: IPRINT,MAXFUN,N,NPT,I
    IPRINT=1
    MAXFUN=5000
    RHOEND=1.0D-8
    N=NDIM
    NPT=2*N+1

      DO 10 I=1,N
   10 X(I)=DFLOAT(I)/DFLOAT(N+1)
      RHOBEG=0.2D0*X(1)
      XX(1)=t13
      XX(2)=dm
      CALL NEWUOA_M (N,NPT,X,RHOBEG,RHOEND,IPRINT,MAXFUN,W,XX,YX)
      chi=YX
    return
end subroutine db_chi_square_constant_reactor_flux_newoua
