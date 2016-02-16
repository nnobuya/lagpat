subroutine new_raph(gel, rho, ye, rmu_p_lg, rmu_n_lg, x, n_conv)
  !------------------------------------------------------------------- !
  !                                                                    !
  !    Newton-Raphson Method (2D)                                      !
  !                                                                    !
  !------------------------------------------------------------------- !

  use mod_const, only: nel, pi, x_min
  use mod_set  , only: rn, rz, ra

  implicit none

  !..param
  integer, parameter:: item = 100
  !real*8 , parameter:: eps  = 2.d-14
  !real*8 , parameter:: eps  = 5.d-14
  real*8 , parameter:: eps  = 1.d-13

  real*8, external:: coul_mu

  !..io
  real*8 , intent(in)   :: gel(nel)
  real*8 , intent(inout):: rmu_p_lg, rmu_n_lg, rho, ye
  integer, intent(out)  :: n_conv
  real*8 , intent(out)  :: x(nel)

  !..local
  real*8 :: xr(nel), rho_lg_in, ye_in, delta, total
  integer:: iter


  rho_lg_in = log10(rho)
  ye_in     = ye

  loop_out: do iter = 0, item

     !..nse abundance
     xr(1:nel) = &
          & 10.d0**( gel(1:nel) + rz(1:nel) *rmu_p_lg + rn(1:nel) *rmu_n_lg )

     !..rho & Ye
     rho = sum( xr(1:nel) )
     ye  = sum( rz(1:nel) /ra(1:nel) *xr(1:nel) ) /rho

     delta = sqrt( (log10(rho) - rho_lg_in)**2 + ((ye - ye_in) /0.05d0)**2 )

     if(delta <= eps) exit loop_out

     call nr_retry(xr(:), rho, rho_lg_in, ye, ye_in, rmu_p_lg, rmu_n_lg)
     !  in: xr(nel), rho, rho_lg_in, ye, ye_in
     ! out: rmu_p_lg, rmu_n_lg

  end do loop_out


  x(1:nel) = max( xr(1:nel) /rho, x_min )

  total = sum(x(1:nel))
  x(1:nel) = x(1:nel) /total


  if( iter >= item ) then
     if( delta <= 1.d-10 ) then
        n_conv = 1
     else
        n_conv = -1
     end if
  else
     n_conv = 0
  end if


  return
  
end subroutine new_raph
