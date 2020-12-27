subroutine db_minimization_with_PRAXIS(pr,myp,mdim)
    use types
    use db_data, only: NDIM
    implicit none    
    integer ( kind = 4 ) mdim
    real ( kind = 8 ) myp(mdim)

    real ( kind = 8 ) praxis    
    real ( kind = 8 ) pr    
    real ( kind = 8 ) t0
    real ( kind = 8 ) h0    
    integer ( kind = 4 ) prin
    real ( kind = 8 ) x(NDIM)
    real ( kind = 8 ), external :: f

    t0=0.00000001d0
    h0=56.0d0
    prin = 0
    x(1) = 0.126004D-01
    x(2) = -0.150118D-01
    x(3) = 0.126004D-01
    x(4) = -0.150118D-01
    x(5) = 0.126004D-01
    x(6) = -0.150118D-01
    !x(3) = 1.0d0    
    pr = praxis ( t0, h0, NDIM, prin, x, f, myp, mdim )    
    return
end subroutine db_minimization_with_PRAXIS