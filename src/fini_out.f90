subroutine fini_out(io, istg, ti, ist_pt, id, dma, &
     & x_pt, v_pt, d_pt, t_pt, ye_pt, en_pt)

  use mod_cnst, only: npt, ndim

  implicit none

  !..io
  integer, intent(in):: io, istg, ist_pt(npt), id(1:ndim,1:npt)
  real(8), intent(in):: ti, dma(1:npt), x_pt(ndim,npt), v_pt(ndim,npt), &
       & d_pt(1:npt), t_pt(1:npt), ye_pt(1:npt), en_pt(1:npt)

  !..local
  integer:: ipt


  !..for next stage
  write(io,'("#", 10x, "time", 6x, "step")')
  write(io,'("#", 1p, e14.7, i10)') ti, istg
  write(io,'(a1, a9, 4a7, *(a18))') &
       & '#', 'npt', 'x1', 'x2', 'x3', 'stat', &
       & 'Mass', 'x(1)', 'x(2)', 'x(3)', 'v(1)', 'v(2)', 'v(3)',&
       & 'Density',  'Temperature', 'Ye', 'Entropy'

  do ipt = 1, npt
     write(io,'(i10, 4i7, 1p, *(e18.10))') &
          & ipt, id(1:ndim,ipt), ist_pt(ipt), &
          & dma(ipt), x_pt(1:ndim,ipt), v_pt(1:ndim,ipt), &
          & d_pt(ipt), t_pt(ipt), ye_pt(ipt), en_pt(ipt)
  end do


  return

end subroutine fini_out
