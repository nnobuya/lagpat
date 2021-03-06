subroutine hokan_main(mode, dt, ist_pt, ipt, x_pt, &
     & d_fld, t_fld, s_fld, ye_fld, pr_fld, f_fld, v0_fld, v_fld, &
     & d_pt, t_pt, s_pt, ye_pt, pr_pt, f_pt, v_pt, v_pt_p)

  use mod_set , only: nx1, nx2, nx3, int_t, npt, ndim, mode_run, x_fld


  implicit none

  !..io
  integer, intent(in):: mode, ist_pt(npt)
  double precision, intent(in):: dt, x_pt(1:ndim,1:npt)
  double precision, dimension(1:nx1,1:nx2,1:nx3), intent(in):: &
       & d_fld, t_fld, s_fld, ye_fld, pr_fld
  double precision, intent(in):: f_fld(1:3,1:nx1,1:nx2,1:nx3), &
       & v0_fld(1:ndim,1:nx1,1:nx2,1:nx3), v_fld(1:ndim,1:nx1,1:nx2,1:nx3)
  double precision, intent(out):: &
       & d_pt(1:npt), t_pt(1:npt), s_pt(1:npt), ye_pt(1:npt), pr_pt(1:npt), &
       & f_pt(1:3,1:npt), v_pt(1:ndim,1:npt), v_pt_p(1:ndim,0:4,1:npt)
  integer, intent(inout):: ipt(1:ndim,1:npt)

  !..local
  double precision:: x_pt_p(1:ndim,1:npt)
  double precision, dimension(1:ndim,1:npt):: fac, fac_p
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

        if      (mode_run == 1 .or. mode_run == 3) then
           x_pt_p(1,i) = x_pt(1,i) + 0.5d0 *dt *v_pt(1,i)
           x_pt_p(2,i) = x_pt(2,i) + 0.5d0 *dt *v_pt(2,i) /x_pt_p(1,i)
           x_pt_p(3,i) = 0d0
        else if (mode_run == 2) then
           x_pt_p(1,i) = x_pt(1,i) + 0.5d0 *dt *v_pt(1,i)
           x_pt_p(2,i) = 0.d0
           x_pt_p(3,i) = x_pt(3,i) + 0.5d0 *dt *v_pt(3,i)
        else
           write(*,*) 'ERROR: bad mode_run =', mode_run
           stop
        end if

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
       & d_fld(:,:,:), t_fld(:,:,:), s_fld(:,:,:), &
       & ye_fld(:,:,:), pr_fld(:,:,:), f_fld(:,:,:,:), &
       & d_pt(:), t_pt(:), s_pt(:), ye_pt(:), pr_pt(:), f_pt(:,:))
  ! out: t_pt, d_pt

  !     hokan                                                    !
  ! ------------------------------------------------------------ !


  return

end subroutine hokan_main
