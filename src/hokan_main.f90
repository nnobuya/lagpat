subroutine hokan_main(mode, dt, ist_pt, ipt, x_pt, &
     & d_fld, t_fld, s_fld, ye_fld, v0_fld, v_fld, &
     & d_pt, t_pt, s_pt, ye_pt, v_pt, v_pt_p)

  use mod_cnst, only: npt, ndim
  use mod_set , only: nx1, nx2, nx3, int_t

  implicit none

  !..io
  integer, intent(in)         :: mode, ist_pt(npt)
  real(8), intent(in):: dt, x_pt(1:ndim,1:npt)
  real(8), dimension(1:nx1,1:nx2,1:nx3), intent(in):: &
       & d_fld, t_fld, s_fld, ye_fld
  real(8), dimension(1:ndim,1:nx1,1:nx2,1:nx3), intent(in):: &
       & v0_fld, v_fld
  real(8), intent(out):: d_pt(1:npt), t_pt(1:npt), s_pt(1:npt), ye_pt(1:npt), &
       & v_pt(1:ndim,1:npt), v_pt_p(1:ndim,0:4,1:npt)
  integer, intent(inout):: ipt(1:ndim,1:npt)

  !..local
  real(8):: x_pt_p(1:ndim,1:npt)
  real(8), dimension(1:ndim,1:npt):: fac, fac_p
  integer:: ipt_p(1:ndim,1:npt)
  integer:: i


  !! particle velosity
  do i = 1, npt

     if ( ist_pt(i) /= 0 ) cycle

     !..position
     call search( mode, x_pt(:,i), v_pt(:,i), fac(:,i), ipt(:,i) )
     !  out: fac;     inout: ipt

     call hokan_vel( ipt(:,i), fac(:,i), v0_fld(:,:,:,:), v_pt(:,i) )
     ! out: v_pt


     ! ------------------------------------------------------------------ !
     !..for Heun's
     ipt_p(1:ndim,i) = ipt(1:ndim,i)


     if ( int_t == 1 ) then

        x_pt_p(1,i) = x_pt(1,i) + dt *v_pt(1,i)
        x_pt_p(2,i) = x_pt(2,i) + dt *v_pt(2,i) /x_pt_p(1,i)

        call search( mode, x_pt_p(:,i), v_pt(:,i), fac_p(:,i), ipt_p(:,i) )
        !  out: fac_p;     inout: ipt_p

        call hokan_vel(ipt_p(:,i), fac_p(:,i), v_fld(:,:,:,:), v_pt_p(:,0,i))
        ! out: v_pt_p

     else if ( int_t == 2 ) then

        x_pt_p(1,i) = x_pt(1,i) + 0.5d0 *dt *v_pt(1,i)
        x_pt_p(2,i) = x_pt(2,i) + 0.5d0 *dt *v_pt(2,i) /x_pt_p(1,i)

        call search( mode, x_pt_p(:,i), v_pt(:,i), fac_p(:,i), ipt_p(:,i) )
        !  out: fac_p;     inout: ipt_p

        call hokan_vel(ipt_p(:,i), fac_p(:,i), v0_fld(:,:,:,:), v_pt_p(:,1,i))
        call hokan_vel(ipt_p(:,i), fac_p(:,i),  v_fld(:,:,:,:), v_pt_p(:,2,i))
        v_pt_p(1:ndim,3,i) = v_pt(1:ndim,i)
        call hokan_vel(ipt(:,i)  ,   fac(:,i),  v_fld(:,:,:,:), v_pt_p(:,4,i))
        ! out: v_pt_p

     end if

     ! for Heun's
     ! ------------------------------------------------------------------ !

  end do


  call hokan_rhotye(ist_pt(:), ipt(:,:), fac(:,:), &
       & d_fld(:,:,:), t_fld(:,:,:), s_fld(:,:,:), ye_fld(:,:,:), &
       & d_pt(:), t_pt(:), s_pt(:), ye_pt(:))
  ! out: t_pt, d_pt

  !     hokan                                                    !
  ! ------------------------------------------------------------ !


  return

end subroutine hokan_main
