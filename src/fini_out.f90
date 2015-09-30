subroutine fini_out( istg, ti, ist_pt, id, x_pt, v_pt )

  use mod_cnst, only: npt, ndim
  use mod_set , only: dma

  implicit none

  !..io
  integer         , intent(in):: istg, ist_pt(npt), id(1:ndim,1:npt)
  double precision, intent(in):: ti, x_pt(ndim,npt), v_pt(ndim,npt)

  !..local
  integer:: i


  !..for next stage
  write(90,*) '#'
  write(90,*) ti, istg
  write(90,*)
  write(90,*)

  do i = 1, npt
     write(90,'(i10,3i5,1p7e18.10,i5)') &
          & i, id(1:ndim,i), dma(i), x_pt(1:ndim,i), v_pt(1:ndim,i), ist_pt(i)
  end do


  return

end subroutine fini_out
