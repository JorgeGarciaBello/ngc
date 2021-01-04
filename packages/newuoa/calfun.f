      SUBROUTINE CALFUN (N,X,F,XX)
      use types    
      use neu_osc_parameters, only: Y  
      implicit none
      integer :: N            ! Dimension of the pull array
      real(dp) :: X(N)        ! Vector of pull's
      real(dp) :: F
      real(dp) :: reno_chi_from_near_constant_flux      
      real(dp) :: reno_chi_to_min,chi     
      real(dp) :: XX(*)    
       F = reno_chi_to_min(X,XX)
      RETURN
      END

