subroutine hokan ( nd, y, fac, f )

  use mod_set , only: int_x, nin, nou

  implicit none

  !..io
  integer, intent(in):: nd

  double precision, intent(in) :: y(nin:nou,nin:nou,nin:nou), fac(1:nd)
  double precision, intent(out):: f

  !..local only
  integer:: i1, i2, i3
  double precision:: y1, y2, y11, y12, y21, y22


  if      ( int_x == 0 ) then   !! nearest neighbor
     i1 = 1 + int( 2.d0 *fac(1) )
     i2 = 1 + int( 2.d0 *fac(2) )
     i3 = 1 + int( 2.d0 *fac(3) )

     f = y(i1,i2,i3)

  else if ( int_x == 1 ) then   !! bi-linear
     y11 = ( 1.d0 - fac(1) ) *y(1,1,1) + fac(1) *y(2,1,1)
     y12 = ( 1.d0 - fac(1) ) *y(1,2,1) + fac(1) *y(2,2,1)
     y21 = ( 1.d0 - fac(1) ) *y(1,1,2) + fac(1) *y(2,1,2)
     y22 = ( 1.d0 - fac(1) ) *y(1,2,2) + fac(1) *y(2,2,2)

     y1  = ( 1.d0 - fac(2) ) *y11 + fac(2) *y12
     y2  = ( 1.d0 - fac(2) ) *y21 + fac(2) *y22

     f   = ( 1.d0 - fac(3) ) *y1 + fac(3) *y2

  else
     stop 'hokan(): error no mapping method'
  end if


  return

end subroutine
