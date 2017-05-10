subroutine init_part2(d_fld, id, dma, x_pt)

  use mod_cnst, only: pi, rm_sol
  use mod_set , only: r_in, r_out, nx1, nx2, nx3, x_fld, npt, &
       & ndim, npt_rad, npt_the

  implicit none

  !..io
  double precision, intent(in) :: d_fld(1:nx1,1:nx2,1:nx3)
  integer, intent(out):: id(1:ndim,1:npt)
  double precision, intent(out):: dma(1:npt), x_pt(1:ndim,1:npt)

  !..local
  integer:: ipt(1:ndim)
  double precision:: fld_in(1:2,1:2,1:2), d_pt(1:npt)
  double precision:: rd0, th0, vol(1:npt), vel_pt(1:npt), fac(1:ndim)
  double precision:: rd(0:npt_rad), th(0:npt_the), drd, dth
  integer:: i, j, k, l
  integer:: i1(1:npt), i2(1:npt), i3(1:npt)

  drd = (r_out - r_in) /dble(npt_rad)
  do i = 0, npt_rad
     rd(i) = r_in + dble(i) *drd
  end do

  dth = pi/2.d0 /dble(npt_the)
  do j = 0, npt_the
     th(j) = dble(j) *dth
  end do

  l = 0
  do j = 1, npt_the
     do i = 1, npt_rad
        l = l + 1

        id(1,l) = i
        id(2,l) = 1
        id(3,l) = j

        rd0 = 0.5d0 *(rd(i-1) + rd(i))
        th0 = 0.5d0 *(th(j-1) + th(j))

        x_pt(1,l) = rd0 *cos(th0)
        x_pt(2,l) = 0.d0
        x_pt(3,l) = rd0 *sin(th0)

        vol(l) = 4.d0 *pi *rd0 *rd0 *(rd(i) - rd(i-1)) /dble(npt_the)
     end do
  end do

  !print *, sum(vol), 4.d0 /3.d0 *pi *r_out**3, 4.d0 /3.d0 *pi *r_in**3



  vel_pt(1:ndim) = 0.d0
  do i = 1, npt

     call search(1, x_pt(1:ndim,i), vel_pt(1:ndim), fac(1:ndim), ipt(1:ndim))

     do k = 1, 2
        l = k - 1
        i1(k) = max( min( ipt(1) + l, nx1 ), 1 )
        i2(k) = max( min( ipt(2) + l, nx2 ), 1 )
        i3(k) = max( min( ipt(3) + l, nx3 ), 1 )
     end do

     fld_in(1:2,1:2,1:2) = d_fld(i1(1:2), i2(1:2), i3(1:2))
     call hokan(ndim, fld_in, fac(1:ndim), d_pt(i))

  end do

  !do i = 1, npt
  !   write(100,'(*(es12.3))') x_pt(1:ndim, i), log10(d_pt(i))
  !end do

  dma(1:npt) = vol(1:npt) *d_pt(1:npt)




  return

end subroutine init_part2
