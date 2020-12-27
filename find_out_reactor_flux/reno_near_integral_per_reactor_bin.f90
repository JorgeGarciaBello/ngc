function reno_near_integral_per_reactor_bin(r,bin,t13,dmee)
    use types
    use reno_data, only: NBIN, bines, length_d_r
    implicit none
    integer, parameter :: d=1
    real(dp) :: reno_near_integral_per_reactor_bin
    integer  :: r, bin, t13, dmee
    real(dp) :: a, b, h              ! limits of the bin    
    integer  :: k, n=5
    real(dp) :: x, result            ! Neutrino energy in [MeV]    
    real(dp) :: reno_probability_of_survival
    real(dp) :: crossSection         ! Is the cross section for the inverse beta decay [m^2]    

    a=bines(bin,1); b=bines(bin,2); h=(b-a)/real(n)
    result=0.0d0
    do k=1,n
        x = a + h*real(k-1)
        x=x*0.9992d0
        if (x < 1.8010001) x=1.8010001
        result=result + h*( reno_probability_of_survival(x,  length_d_r(d,r),t13,dmee)*crossSection(x) + & 
                            reno_probability_of_survival(x+h,length_d_r(d,r),t13,dmee)*crossSection(x+h))/2.0d0
    enddo    
    reno_near_integral_per_reactor_bin = result/((b-a)*length_d_r(d,r)**2)
    return
end function reno_near_integral_per_reactor_bin