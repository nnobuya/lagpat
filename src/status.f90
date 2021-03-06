subroutine status(x_pt, ist_pt)

  use mod_set , only: bound_out, bound_in, npt, ndim, mode_run

  implicit none

  !..io
  double precision, intent(in)   :: x_pt(ndim,npt)
  integer         , intent(inout):: ist_pt(npt)

  !..tmp
  double precision:: rd
  integer:: i


  !! current status
  do i = 1, npt
     if( ist_pt(i) /= 0 ) cycle

     if (mode_run == 1 .or. mode_run == 3) then
        rd = x_pt(1,i)
     else if (mode_run == 2) then
        rd = sqrt(x_pt(1,i) *x_pt(1,i) + x_pt(3,i) *x_pt(3,i))
     else
        write(*,*) 'ERROR: undefined mode_run =', mode_run
        stop
     end if

     if      ( rd > bound_out ) then
        ist_pt(i) =  1
     else if ( rd < bound_in  ) then
        ist_pt(i) = - 1
     end if
  end do

  return

end subroutine status
