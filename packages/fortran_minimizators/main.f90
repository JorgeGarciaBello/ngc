!program main
!    implicit none    
!    real ( kind = 8 ) praxis    
!    real ( kind = 8 ) pr    
!    real ( kind = 8 ) t0
!    real ( kind = 8 ) h0
!    integer ( kind = 4 ), parameter ::  n=2     
!    integer ( kind = 4 ) prin
!    real ( kind = 8 ) x(n)
!    real ( kind = 8 ), external :: f
!
!    t0=0.00000001d0
!    h0=0.1d0    
!    prin = 0
!    x(1) = 1.1d0
!    x(2) = 0.1d0
!    print*, 'x', x
!    pr = praxis ( t0, h0, n, prin, x, f)
!    print*, 'x', x
!    print*, 'resilt: ', pr
!    return
!end program main