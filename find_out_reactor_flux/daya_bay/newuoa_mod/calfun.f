      SUBROUTINE CALFUN_M (N,X,F,XX)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION X(*),XX(*)
      call db_chi_from_reactor_flux(XX(1),XX(2),X,chi)
      !F=(X(1)-XX(1))**2 + (X(2)-XX(2))**2 !+ XX(2)!+XX(2)
      F=chi
      RETURN
      END

