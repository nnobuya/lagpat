subroutine dt_chk( time, dt, istat_pt, ipt, v_pt, x_pt )

  use mod_set , only: dx_fld, npt, ndim

  implicit none

  !..io
  integer, intent(in):: istat_pt(npt)
  integer, intent(in):: ipt(1:ndim,1:npt)
  double precision, intent(in):: time, dt, v_pt(ndim,npt), x_pt(ndim,npt)

  double precision:: di_pt(npt),  dj_pt(npt)

  integer:: i


  di_pt(1:npt) = 0.d0
  dj_pt(1:npt) = 0.d0

  do i = 1, npt
     if( istat_pt(i) /= 0 ) cycle

     di_pt(i) = abs(v_pt(1,i) *dt) /dx_fld(1,ipt(1,i))
     if ( ndim == 1 ) then
        dj_pt(i) = 0.d0
     else
        dj_pt(i) = abs(v_pt(2,i) /x_pt(1,i) *dt) /dx_fld(2,ipt(2,i))
     end if

  end do

  write(71,'(1p3e14.5)') &
       & time, maxval(di_pt(1:npt)), maxval(dj_pt(1:npt))


  return

end subroutine dt_chk
