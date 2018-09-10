subroutine hokan_rhotye(istat_pt, ipt, fac, &
     & d_fld, t_fld, s_fld, ye_fld, pr_fld, f_fld, &
     & d_pt, t_pt, s_pt, ye_pt, pr_pt, f_pt)

  use mod_set , only: nx1, nx2, nx3, nin, nou, npt, ndim

  implicit none

  !..io
  integer, intent(in) :: istat_pt(1:npt), ipt(1:ndim,1:npt)
  double precision, intent(in) :: fac(1:ndim,1:npt), &
       &  d_fld(1:nx1,1:nx2,1:nx3), t_fld(1:nx1,1:nx2,1:nx3) , &
       &  s_fld(1:nx1,1:nx2,1:nx3), ye_fld(1:nx1,1:nx2,1:nx3), &
       & pr_fld(1:nx1,1:nx2,1:nx3), f_fld(1:3,1:nx1,1:nx2,1:nx3)
  double precision, intent(out):: d_pt(1:npt), t_pt(1:npt), &
       & s_pt(1:npt), ye_pt(1:npt), pr_pt(1:npt), f_pt(1:3,1:npt)

  !..local
  integer:: ip, i, j, i1(nin:nou), i2(nin:nou), i3(nin:nou)
  double precision:: y(nin:nou,nin:nou,nin:nou)


  ! ------------------------------------------------------------ !
  !     hokan                                                    !
  ! ------------------------------------------------------------ !

  do ip = 1, npt

     if ( istat_pt(ip) /= 0 ) cycle

     do i = nin, nou
        j = i - 1
        i1(i) = max( min( ipt(1,ip) + j, nx1 ), 1 )
        i2(i) = max( min( ipt(2,ip) + j, nx2 ), 1 )
        i3(i) = max( min( ipt(3,ip) + j, nx3 ), 1 )
     end do

     !! density
     y(nin:nou,nin:nou,nin:nou) &
          & = d_fld( i1(nin:nou), i2(nin:nou), i3(nin:nou) )
     call hokan(ndim, y(:,:,:), fac(:,ip), d_pt(ip))

     !! temperature
     y(nin:nou,nin:nou,nin:nou) &
          & = t_fld(i1(nin:nou), i2(nin:nou), i3(nin:nou))
     call hokan(ndim, y(:,:,:), fac(:,ip), t_pt(ip))

     !! entropy
     y(nin:nou,nin:nou,nin:nou) &
          & = s_fld(i1(nin:nou), i2(nin:nou), i3(nin:nou))
     call hokan(ndim, y(:,:,:), fac(:,ip), s_pt(ip))

     !! ye
     y(nin:nou,nin:nou,nin:nou) &
          & = ye_fld(i1(nin:nou), i2(nin:nou), i3(nin:nou))
     call hokan(ndim, y(:,:,:), fac(:,ip), ye_pt(ip)) 


     !! pressure
     y(nin:nou,nin:nou,nin:nou) &
          & = pr_fld(i1(nin:nou), i2(nin:nou), i3(nin:nou))
     call hokan(ndim, y(:,:,:), fac(:,ip), pr_pt(ip)) 

     !! Marker tracer
     do j = 1, 3
        y(nin:nou,nin:nou,nin:nou) &
             & = f_fld(j, i1(nin:nou), i2(nin:nou), i3(nin:nou))
        call hokan(ndim, y(:,:,:), fac(:,ip), f_pt(j,ip)) 
     end do

  end do

  return

end subroutine hokan_rhotye
