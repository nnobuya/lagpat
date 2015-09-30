subroutine move( dt, dt0, ist_pt, v_pt, v_pt_p, x_pt )

  use mod_cnst, only: npt, ndim
  use mod_set , only: nx1, nx2, nx3, int_t, x_fld

  implicit none

  integer, intent(in):: ist_pt(1:npt)
  double precision, intent(in)   :: dt, dt0
  double precision, intent(in)   :: v_pt(ndim,npt)
  double precision, intent(in)   :: v_pt_p(1:ndim,0:4,1:npt)
  double precision, intent(inout):: x_pt(ndim,npt)

  double precision:: c1, c2, c3, c4
  double precision:: the_min, the_max, v_comb(1:ndim)
  integer:: i


  the_min = minval( x_fld(2,1:nx1,1:nx2,1:nx3) )
  the_max = maxval( x_fld(2,1:nx1,1:nx2,1:nx3) )

  do i = 1, npt

     if ( ist_pt(i) /= 0 ) cycle

     !..time evol.
     if      ( int_t == 0 ) then
        v_comb(1:ndim) = v_pt(1:ndim,i)
     else if ( int_t == 1 ) then
        v_comb(1:ndim) = 0.5d0 *( v_pt(1:ndim,i) + v_pt_p(1:ndim,0,i) )
     else if ( int_t == 2 ) then

        c1 = dt /dt0 - 0.d5
        c2 = 0.d5
        c3 = 1.d0 - dt /dt0
        c4 = 0.d0

        v_comb(1:ndim) &
             & = c1 *v_pt_p(1:ndim,1,i) + c2 *v_pt_p(1:ndim,2,i) &
               + c3 *v_pt_p(1:ndim,3,i) + c4 *v_pt_p(1:ndim,4,i)
     else
        stop 'error: no method'
     end if

     x_pt(1,i) = x_pt(1,i) + v_comb(1) *dt
     if ( nx2 >= 2 ) x_pt(2,i) = x_pt(2,i) + v_comb(2) /x_pt(1,i) *dt
     if ( nx3 >= 2 ) x_pt(3,i) = x_pt(3,i) + v_comb(3) /x_pt(1,i) *dt

     !..avoid negative r
     x_pt(1,i) = max(0.d0, x_pt(1,i))

     if     ( x_pt(2,i) < the_min ) then
        x_pt(2,i) = ( the_min - x_pt(2,i) )
     else if( x_pt(2,i) > the_max ) then
        x_pt(2,i) = 2.d0 *the_max - x_pt(2,i)
     end if


  end do


  return

end subroutine move
