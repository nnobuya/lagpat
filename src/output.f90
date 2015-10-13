subroutine output(istg, ti, dt, ipt, &
     & ist_pt, x_pt, d_pt, t_pt, ye_pt, v_pt, n_anim_out )

  use mod_cnst, only: npt, ndim
  use mod_set , only: nout_lpt, n_anim

  implicit none

  !..io
  integer, intent(in):: istg, ist_pt(1:npt), ipt(1:ndim,1:npt)
  double precision, intent(in):: ti, dt, &
       & x_pt(1:ndim,npt), v_pt(ndim,npt), d_pt(npt), t_pt(npt), ye_pt(npt)
  integer, intent(inout):: n_anim_out

  !..local
  integer:: npt_in, npt_out, npt_num
  integer:: i
  real(8):: x, y
  character:: f_name*100



  !..physical quantities
  out_cond: if( istg == 1 .or. mod(istg,nout_lpt) == 0 ) then

     !..position
     write(61) istg, real(ti), real(dt)
     write(61) ipt(1:ndim,1:npt)

     !..motion
     write(62) istg, real(ti), real(dt)
     write(62) real(x_pt(1:ndim,1:npt)), real(v_pt(1:ndim,1:npt))

     !..rhotye
     write(63) istg, real(ti), real(dt)
     write(63) real(d_pt(1:npt)), real(t_pt(1:npt)), real(ye_pt(1:npt))

     !..status
     write(64) istg, real(ti), real(dt)
     write(64) ist_pt(1:npt)

  end if out_cond


  if ( mod(istg,n_anim) == 0 ) then

     close(66)

     write(f_name, '("../res/anim/anim_", i4.4, ".dat")') istg
     open(66, file = f_name, action = 'write')

     do i = 1, npt
        if (ist_pt(i) /= 0) cycle
        x = x_pt(1,i) *sin(x_pt(2,i))
        y = x_pt(1,i) *cos(x_pt(2,i))
        write(66,'(1p, *(2e11.3))') x, y
     end do

     n_anim_out = n_anim_out + 1

  end if


  call pt_status( ist_pt(:), npt_in, npt_out, npt_num )

  if( istg == 1 ) then
     write(*,*) &
          & '======================= tracing particles  ======================='
     write(*,'("#", 5x, "step", 8x, "ti", 10x, "dt", &
          & 3x, "pt(move)", 4x, "pt(in)", 3x, "pt(out)")')
  end if

  if( mod(istg,100) == 0 .or. istg <= 5 ) &
       & write(*,'(i10,1p2e12.4,3i10)') &
       & istg, ti, dt, npt_num, npt_in, npt_out

  return

end subroutine output
