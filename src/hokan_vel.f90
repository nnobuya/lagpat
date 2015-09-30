subroutine hokan_vel( ipt, fac, v_fld, vpt )

  use mod_cnst, only: npt, ndim
  use mod_set , only: nx1, nx2, nx3, nin, nou

  implicit none

  !..io
  integer         , intent(in) :: ipt(1:ndim)
  double precision, intent(in) :: fac(1:ndim), v_fld(ndim,nx1,nx2,nx3)
  double precision, intent(out):: vpt(1:ndim)

  !..local
  integer:: idim, i, j, i1(nin:nou), i2(nin:nou), i3(nin:nou)
  double precision:: y(nin:nou,nin:nou,nin:nou)


  ! ------------------------------------------------------------ !
  !     hokan                                                    !
  ! ------------------------------------------------------------ !

  do idim = 1, ndim

     do i = nin, nou
        j = i - 1
        i1(i) = max( min( ipt(1) + j, nx1 ), 1 )
        i2(i) = max( min( ipt(2) + j, nx2 ), 1 )
        i3(i) = max( min( ipt(3) + j, nx3 ), 1 )
     end do

     y(nin:nou,nin:nou,nin:nou) &
          & = v_fld( idim, i1(nin:nou), i2(nin:nou), i3(nin:nou) )

     call hokan ( ndim, y(:,:,:), fac(:), vpt(idim) )

  end do


  return

end subroutine hokan_vel
