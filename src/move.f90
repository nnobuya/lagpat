subroutine move( dt, dt0, ist_pt, v_pt, v_pt_p, x_pt )

  use mod_cnst, only: pi
  use mod_set , only: nx1, nx2, nx3, int_t, x_fld, npt, ndim, mode_run

  implicit none

  integer, intent(in):: ist_pt(1:npt)
  double precision, intent(in)   :: dt, dt0
  double precision, intent(in)   :: v_pt(ndim,npt)
  double precision, intent(in)   :: v_pt_p(1:ndim,0:4,1:npt)
  double precision, intent(inout):: x_pt(ndim,npt)

  double precision, save:: the_min, the_max
  double precision:: c1, c2, c3, c4, v_comb(1:ndim), theta, rd
  integer:: i


  if (mode_run == 1 .or. mode_run == 3) then
     the_min = minval( x_fld(2,1:nx1,1:nx2,1:nx3) )
     the_max = maxval( x_fld(2,1:nx1,1:nx2,1:nx3) )
  else if (mode_run == 2) then
     the_min = 0.d0
     the_max = pi /2.d0
  else
     write(*,*) 'ERROR: bad mode_run =', mode_run
     stop
  end if

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

     if (mode_run == 1 .or. mode_run == 3) then
        x_pt(1,i) = x_pt(1,i) + v_comb(1) *dt
        if ( nx2 >= 2 ) x_pt(2,i) = x_pt(2,i) + v_comb(2) /x_pt(1,i) *dt
        if ( nx3 >= 2 ) x_pt(3,i) = x_pt(3,i) + v_comb(3) /x_pt(1,i) *dt
     else if (mode_run == 2) then
        x_pt(1,i) = x_pt(1,i) + v_comb(1) *dt
        if ( nx2 >= 2 ) x_pt(2,i) = x_pt(2,i) + v_comb(2) *dt
        if ( nx3 >= 2 ) x_pt(3,i) = x_pt(3,i) + v_comb(3) *dt
     end if


     !..avoid negative r
     if (mode_run == 1 .or. mode_run == 3) then
        x_pt(1,i) = max(0.d0, x_pt(1,i))

        if     ( x_pt(2,i) < the_min ) then
           x_pt(2,i) = ( the_min - x_pt(2,i) )
        else if( x_pt(2,i) > the_max ) then
           x_pt(2,i) = 2.d0 *the_max - x_pt(2,i)
        end if
     else if (mode_run == 2) then
        !write(*,*) 'WARNING: please check this part once more ' &
        !     & // 'to use fujib model'
        
        theta = atan(x_pt(1,i) /x_pt(3,i))
        !rd    = sqrt(x_pt(1,i) *x_pt(1,i) + x_pt(3,i) *x_pt(3,i))
        if (theta < the_min) then
           theta = the_min - theta
        else if (theta > the_max) then
           theta = 2.d0 *the_max - theta
        end if

     end if

  end do


  return

end subroutine move
