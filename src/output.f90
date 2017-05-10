subroutine output(istg, ti, dt, ipt, &
     & ist_pt, x_pt, d_pt, t_pt, s_pt, ye_pt, v_pt, n_anim_out)

  use mod_set , only: nout_lpt, n_anim, npt, ndim, mode_run

  implicit none

  !..io
  integer, intent(in):: istg, ist_pt(1:npt), ipt(1:ndim,1:npt)
  real(8), intent(in):: ti, dt, &
       & x_pt(1:ndim,1:npt), v_pt(1:ndim,1:npt), &
       & d_pt(1:npt), t_pt(1:npt), s_pt(1:npt), ye_pt(1:npt)
  integer, intent(inout):: n_anim_out

  !..local
  integer, save:: iout = 0
  integer:: npt_in, npt_out, npt_num
  integer:: i
  real(8):: x, y, vr
  character:: f_name*100



  !..physical quantities
  out_cond: if (istg == 1 .or. mod(istg,nout_lpt) == 0) then

     close(60)

     write(f_name, '("./res/lpt/lpt_", i6.6, ".dat")') istg
     open(60, file = f_name, form = 'unformatted', action = 'write')

     !..lpt data
     write(60) istg, real(ti), real(dt)
     write(60) ipt(1:ndim,1:npt)
     write(60) ist_pt(1:npt)
     write(60) real(x_pt(1:ndim,1:npt)), real(v_pt(1:ndim,1:npt))
     write(60) real(d_pt(1:npt)), real(t_pt(1:npt)), &
          & real(s_pt(1:npt)), real(ye_pt(1:npt))

  end if out_cond

  if (istg == 1 .or. mod(istg,n_anim) == 0) then

     close(66)

     write(f_name, '("./res/anim/anim_", i4.4, ".dat")') istg
     open(66, file = f_name, action = 'write')

     write(66,'("#", 3x, "X", 11x, "Y",11x,  "de", 10x, &
          & "te", 10x, "s", 10x, "ye")')
     do i = 1, npt
        if (ist_pt(i) /= 0) cycle

        if (mode_run == 1) then
           x = x_pt(1,i) *sin(x_pt(2,i))
           y = x_pt(1,i) *cos(x_pt(2,i))
           vr = v_pt(1,i)
        else if (mode_run == 2) then
           x = x_pt(1,i)
           y = x_pt(3,i)
           vr = sqrt(v_pt(1,i) *v_pt(1,i) + v_pt(3,i) *v_pt(3,i))
           if (v_pt(1,i) < 0 .and. v_pt(3,i) < 0) vr = - vr
        end if

        write(66,'(*(es12.4))') &
             & x, y, d_pt(i), t_pt(i), s_pt(i), ye_pt(i), vr
     end do

     write(65,'(i10, 1p, e15.7)') istg, ti *1000.0
     n_anim_out = n_anim_out + 1

  end if


  call pt_status( ist_pt(:), npt_in, npt_out, npt_num )

  if( istg == 1 ) then
     write(*,*) &
          & '======================= tracing particles  ======================='
     write(*,'("#", 5x, "step", 8x, "ti", 10x, "dt", &
          & 3x, "pt(move)", 4x, "pt(in)", 3x, "pt(out)")')
  end if

  iout = iout + 1
  if (iout == 1) then
     write(*,*)
     write(*,'(" -----------------------------------", &
     & "----------------------------------")')
  end if
  if( mod(iout,100) == 0 .or. iout <= 10 ) &
       & write(*,'(i10,1p2e12.4,3i10)') &
       & istg, ti, dt, npt_num, npt_in, npt_out

  return

end subroutine output
