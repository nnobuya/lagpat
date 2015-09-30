subroutine pt_status(istat_pt, npt_in, npt_out, npt_num)

  use mod_cnst, only: npt

  implicit none

  !..io
  integer, intent(in) :: istat_pt(1:npt)
  integer, intent(out):: npt_in, npt_out, npt_num

  !..tmp
  integer:: i

  npt_in  = 0
  npt_out = 0
  npt_num = 0

  do i = 1, npt
     if     ( istat_pt(i) ==  0 ) then
        npt_num = npt_num + 1
     else if( istat_pt(i) ==  1 ) then 
        npt_out = npt_out + 1
     else if( istat_pt(i) == -1 ) then 
        npt_in  = npt_in  + 1
     else
        stop 'status(): error'
     end if
  end do


  return

end subroutine pt_status
