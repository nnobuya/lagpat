! -------------------------------------------------------------------- !
!                                                                      !
! Lag_rangian Pa_rticle T_racer for 3-D                                !
!                                                                      !
! Purpose: to trace motions of particles from hydro results            !
! time stamp: 2005/09/24                                               !
!             2006/01/04                                               !
!             2009/04/16                                               !
!             2009/08/25                                               !
!             2010/01/24                                               !
!             2012/07/24                                               !
!             2015/10/13                                               !
!                                                                      !
! input : hydro Result: LAGPAT/lag_*.dat                               !
!         ./in.dat                                                     !
!                                                                      !
! output: ./part/*.dat                                                 !
!                                                                      !
! Coded by Nishimura Nobuya                                            !
!                                                                      !
! -------------------------------------------------------------------- !

program lagpat

  use mod_set , only: npt, ndim, &
       & int_t, i_test, last_lp, n_init, n_fini, &
       & d_fld, t_fld, s_fld, ye_fld, v_fld, v0_fld, set_data
  use mod_fld , only: dt_max, fld

  implicit none

  !..main
  integer, allocatable:: ist_pt(:), ipt(:,:), id(:,:)
  real(8), allocatable, dimension(:):: d_pt, t_pt, s_pt, ye_pt, dma
  real(8), allocatable:: x_pt(:,:), v_pt(:,:), v_pt_p(:,:,:)

  !..local
  integer:: istg, ihyd, n_anim_out = 0
  integer:: i, ier
  real(8):: ti, dt0, dt_in, dt = 0.d0


  ti = 0.d0

  ! ------------------------------------------------------------------ !
  !                                                                    !
  !     pre-process                                                    !
  !                                                                    !
  ! ------------------------------------------------------------------ !

  !..logo
  call logo

  !..open files
  call ofile

  allocate(ist_pt(1:npt), ipt(1:ndim,1:npt), id(1:ndim,1:npt), &
       & d_pt(1:npt), t_pt(1:npt), s_pt(1:npt), ye_pt(1:npt), &
       & dma(1:npt), x_pt(1:ndim,1:npt), v_pt(1:ndim,1:npt), &
       & v_pt_p(1:ndim,0:4,1:npt))

  !..set hydro data
  call set_data

  call set_part(istg, ti, ist_pt(:), id(:,:), x_pt(:,:), v_pt(:,:), dma(:))
  !call set_part(istg, ti, ist_pt, id, x_pt, v_pt, dma)

  !  out: all

  !     pre-process                                                    !
  ! ------------------------------------------------------------------ !



  ! ------------------------------------------------------------------ !
  !                                                                    !
  !     first-step                                                     !
  !                                                                    !
  ! ------------------------------------------------------------------ !

  call fld(n_init, d_fld(:,:,:), t_fld(:,:,:), s_fld(:,:,:), &
       & ye_fld(:,:,:), v_fld(:,:,:,:), v0_fld(:,:,:,:) )
  ! out: all

  call hokan_main(1, dt_max, ist_pt(:), ipt(:,:), x_pt(:,:), &
       & d_fld(:,:,:), t_fld(:,:,:), s_fld(:,:,:), ye_fld(:,:,:), &
       & v0_fld(:,:,:,:), v_fld(:,:,:,:), &
       & d_pt(:), t_pt(:), s_pt(:), ye_pt(:), v_pt(:,:), v_pt_p(:,:,:) )
  !  in: mode, ist_pt, rad_pt, the_pt, d_fld, t_fld, v_fld
  !  in: d_pt, t_pt, v_pt
  !  inout: mode, ipt, jpt

  dt = 0.d0

  call output(n_init, ti, dt, ipt(:,:), ist_pt(:), x_pt(:,:), &
       & d_pt(:), t_pt(:), s_pt(:), ye_pt(:), v_pt(:,:), n_anim_out)

  dt0 = dt_max
  dt  = dt_max

  !..temporary
  !total = sum(dma(1:npt))
  !do i = 1, npt
  !   write(100,'(1p, *(e15.7))') ye_pt(i), dma(i), dma(i)/total
  !end do

  call fini_out(40, istg + 1, ti, ist_pt(:), id(:,:), dma(:), &
       & x_pt(:,:), v_pt(:,:), d_pt(:), t_pt(:), ye_pt(:), s_pt(:) )
  close(40)

  if (i_test == 1) stop '### finish test  ###'

  !     first-step                                                     !
  ! ------------------------------------------------------------------ !

  ! ------------------------------------------------------------------ !
  !                                                                    !
  !     particle tracing: LagPaT main                                  !
  !                                                                    !
  ! ------------------------------------------------------------------ !


  main_lp: do ihyd = n_init + 1, n_fini - 1

     ! --------------------------------------------------------------- !
     !     update                                                      !
     ! --------------------------------------------------------------- !

     call dt_chk( ti, dt, ist_pt(:), ipt(:,:), v_pt(:,:), x_pt(:,:) )
     !  in: all

     call move( dt, dt0, ist_pt(:), v_pt(:,:), v_pt_p(:,:,:), x_pt(:,:) )
     !  in : dt, ist_pt, ipt, jpt, v_pt
     !  out: rad_pt, the_pt

     call status( x_pt(:,:), ist_pt(:) )
     !  in: rad_pt
     ! i&o: ist_pt

     !     update                                                      !
     ! --------------------------------------------------------------- !


     ! --------------------------------------------------------------- !
     !     read                                                        !
     ! --------------------------------------------------------------- !

     call fld(ihyd, d_fld(:,:,:), t_fld(:,:,:), s_fld(:,:,:), &
          & ye_fld(:,:,:), v_fld(:,:,:,:), v0_fld(:,:,:,:))
     !  out: all

     !if( ier /= 0 .or. (last_lp > 0 .and. istg > last_lp) ) exit main_lp

     !     read                                                        !
     ! --------------------------------------------------------------- !

     dt0 = dt
     dt  = dt_max

     ti = ti + dt

     ! --------------------------------------------------------------- !
     !     hokan                                                       !
     ! --------------------------------------------------------------- !

     if (int_t == 2) then
        dt_in = dt0
     else
        dt_in = dt
     end if

     call hokan_main(2, dt_in, ist_pt(:), ipt(:,:), x_pt(:,:), &
          & d_fld(:,:,:), t_fld(:,:,:), s_fld(:,:,:), ye_fld(:,:,:), &
          & v0_fld(:,:,:,:), v_fld(:,:,:,:), &
          & d_pt(:), t_pt(:), s_pt(:), ye_pt(:), v_pt(:,:), v_pt_p(:,:,:) )

     !    in: others
     !   out: d_pt, t_pt, v_pt
     ! inout: ipt, jpt

     !     hokan                                                       !
     ! --------------------------------------------------------------- !


     ! --------------------------------------------------------------- !
     !     output                                                      !
     ! --------------------------------------------------------------- !

     call output(ihyd, ti, dt, ipt(:,:), ist_pt(:), x_pt(:,:), &
          & d_pt(:), t_pt(:), s_pt(:), ye_pt(:), v_pt(:,:), n_anim_out)
     !    in: others
     ! inout: n_anim_out


     !     output                                                      !
     ! --------------------------------------------------------------- !
     
  end do main_lp

  close(61)
  close(62)
  close(63)
  close(64)

  close(71)

  !     particle tracing: LagPaT main                                  !
  ! ------------------------------------------------------------------ !



  ! ------------------------------------------------------------------ !
  !     closing                                                        !
  ! ------------------------------------------------------------------ !

  write(*,'(" -----------------------------------", &
  & "----------------------------------")')

  !write(65,'(a20,i10)') 'calculation step:', istg
  !write(65,'(a20,i10)') 'output:', n_anim_out
  close(65)

  write(*,'(a20,i10)') 'calculation step:', istg
  write(*,'(a20,i10)') 'output:', n_anim_out

  call fini_out(41, ihyd - 1, ti, ist_pt(:), id(:,:), dma(:), &
       & x_pt(:,:), v_pt(:,:), d_pt(:), t_pt(:), ye_pt(:), s_pt(:))

  close(41)

  write(*,'(" -----------------------------------", &
  & "----------------------------------")')


  !     closing                                                        !
  ! ------------------------------------------------------------------ !


  stop 'lagpat: Normal Termination'


end program lagpat
