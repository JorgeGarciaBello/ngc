function db_far_integral_per_detector_reactor_bin_H3_P2(d,r,bin,t13,dmee,xy)
    use types
    use db_data, only: NDIM, bines, length_d_r
    implicit none
    integer, parameter :: n=5
    real(dp) :: db_far_integral_per_detector_reactor_bin_H3_P2
    integer  :: d, r, bin
    real(dp) :: t13, dmee    
    real(dp) :: a, b, h, x,xy(NDIM)
    integer  :: k
    real(dp) :: probability, crossSection, result    
    if((d>=5).or.(d<=8)) then
        a=bines(bin,1); b=bines(bin,2); 
        a=a*0.985
        b=b*0.985
        h=(b-a)/real(n)
        result=0.0d0
        do k=1,n
            x = a + h*real(k-1)
            x = x*(1.0_dp + xy(5))
            if (x < 1.8010001) x=1.8010001
            result=result + h*( probability(t13,dmee,length_d_r(d,r),x)*crossSection(x) + &
                                probability(t13,dmee,length_d_r(d,r),x+h)*crossSection(x+h) )/2.0d0
        enddo
        db_far_integral_per_detector_reactor_bin_H3_P2 = result/( length_d_r(d,r)**2 )
    else
        print*, 'El indice del detector para la sala H3 no se encuentra dentro del rango 5 <= d <= 8'
    end if
    return
end function db_far_integral_per_detector_reactor_bin_H3_P2