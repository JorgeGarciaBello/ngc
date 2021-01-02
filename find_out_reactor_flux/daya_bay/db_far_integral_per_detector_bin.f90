!function db_far_integral_per_detector_bin(d,bin,t13,dmee)
!    use types
!    use db_data, only: bines, RCTS, length_d_r
!    implicit none    
!    real(dp) :: db_far_integral_per_detector_bin
!    real(dp) :: db_far_integral_per_detector_reactor_bin_H3_P1
!    integer  :: d, r, bin
!    real(dp) :: t13, dmee
!    real(dp) :: result
!
!    result=0.0_dp
!    do r = 1, RCTS
!        result = db_far_integral_per_detector_reactor_bin_H3_P1(d,r,bin,t13,dmee)
!    enddo
!    return
!end function db_far_integral_per_detector_bin
