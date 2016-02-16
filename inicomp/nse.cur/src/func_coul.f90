real*8 function func_coul( na, nz, ye, rho, t9 )

  use mod_const, only: rm_u, rkb, rkb_mev, h, e_esu, pi

  implicit none

  !..param.
  real*8, parameter:: a1 = - 0.898004d0
  real*8, parameter:: a2 =   0.96786d0
  real*8, parameter:: a3 =   0.220703d0
  real*8, parameter:: a4 = - 0.86097d0
  real*8, parameter:: a5 = - 2.52692d0
  real*8, parameter:: bet =  0.295614d0
  real*8, parameter:: r   =  1.98848d0

  !..io
  integer, intent(in):: na, nz
  real*8 , intent(in):: ye, rho, t9

  real*8:: fc, z, t, a_e, gam, gam2


  z = dble(nz)
  t = t9 *1.d9


  a_e = ( ( 3.d0 *rm_u ) /( 4.d0 *pi *rho *ye ) )**( 1.d0 /3.d0 )
  gam = ( z**( 5.d0 /3.d0 ) *e_esu *e_esu ) /( a_e *rkb *t)

! Ichimaru 1996 eq. (1.13b)
!  gam = 36.d0 *( dble(nz) /6.d0 )**2 &
!       & *( dble(na) /12.d0 )**( - 1.d0 /3.d0 ) &
!       & *( 1.d-6 *rho )**(   1.d0 /3.d0 ) &
!       & *( 1.d-7 *t   )**( - 1.d0 )


  if ( gam > 1.d0 ) then
     fc = a1 *gam + 4.d0 *a2 *gam**0.25d0 &
          & - 4.d0 *a3 *gam**(-0.25d0) + a4 *log( gam ) + a5
  else
     fc = - 1.d0 /sqrt( 3.d0 ) *gam**1.5d0 + bet /r * gam**r
  end if

  func_coul = rkb_mev *t *fc


  return

end function func_coul
