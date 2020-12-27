      SUBROUTINE CALFUN (N,X,F)
      use types    
      use neu_osc_parameters, only: Y  
      implicit none
      integer :: N            ! Dimension of the pull array
      real(dp) :: X(N)        ! Vector of pull's
      real(dp) :: F
      real(dp) :: reno_chi_from_near_constant_flux      
      real(dp) :: reno_chi_to_min,chi     
      
       
       !F = reno_chi_from_near_constant_flux(X)
       F = reno_chi_to_min(X)
       !call reno_chi(Y(5),Y(2),0.0d0,X,chi)
       !F = chi
       
      RETURN
      END

