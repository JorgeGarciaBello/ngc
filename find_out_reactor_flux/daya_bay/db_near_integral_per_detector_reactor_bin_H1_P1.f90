function db_near_integral_per_detector_reactor_bin_H1_P1(d,r,bin,t13,dmee,xy)
    use types
    use db_data, only: NDIM,bines, length_d_r
    implicit none
    integer, parameter :: n=5
    real(dp) :: db_near_integral_per_detector_reactor_bin_H1_P1
    integer  :: d, r, bin
    real(dp) :: t13, dmee    
    real(dp) :: a, b, h, x,xy(NDIM)
    integer  :: k
    real(dp) :: probability, crossSection, result    
    if ((d==1).or.(d==2)) then        
        a=bines(bin,1); b=bines(bin,2); 
        a=a*0.92
        b=b*0.92
        h=(b-a)/real(n)
        result=0.0d0
        do k=1,n
            x = a + h*real(k-1)
            x = x*(1.0_dp + xy(5))
            if (x < 1.8010001) x=1.8010001            
            result=result + h*( probability(t13,dmee,length_d_r(d,r),x)*crossSection(x) + &
                                probability(t13,dmee,length_d_r(d,r),x+h)*crossSection(x+h) )/2.0d0
        enddo
        db_near_integral_per_detector_reactor_bin_H1_P1 = result/( length_d_r(d,r)**2 )
    else
       print*, 'El detector para la sala H1 no coincide con los indices: 1,2' 
       stop
    end if    
    return
end function db_near_integral_per_detector_reactor_bin_H1_P1