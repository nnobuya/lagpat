subroutine status( x_pt, ist_pt )

  use mod_cnst, only: npt, ndim
  use mod_set , only: bound_out, bound_in

  implicit none

  !..io
  double precision, intent(in)   :: x_pt(ndim,npt)
  integer         , intent(inout):: ist_pt(npt)

  !..tmp
  integer:: i

  !! status now
  do i = 1, npt
     if( ist_pt(i) /= 0 ) cycle

     if      ( x_pt(1,i) > bound_out ) then
        ist_pt(i) =  1
     else if ( x_pt(1,i) < bound_in  ) then
        ist_pt(i) = - 1
     end if
  end do



  return

end subroutine status
