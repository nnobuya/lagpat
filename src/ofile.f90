subroutine ofile

  use mod_set , only: k_zoku, n_init, set_param, path, mode_run

  implicit none

  character:: no*10, f_name*100

  ! ------------------------------------------------------------------ !
  !     input                                                          !
  ! ------------------------------------------------------------------ !

  open(15, file = './in/lagpat.in', action = 'read')

  call set_param(15)

  close(15)

  if (mode_run == 1) then
     write(no,'(i4.4)') n_init
     f_name = trim(adjustl(path)) // '/rpr'  // trim(adjustl(no)) // '.dat'
  else if (mode_run == 2) then
     write(no,'(i6.6)') n_init
     f_name = trim(adjustl(path)) // '/hydro_' // trim(adjustl(no)) // '.dat'
  end if

  !..hydro results (initial)
  open(50, file = f_name, form = 'unformatted', convert = 'big_endian', &
       & action = 'read')

  !     input                                                          !
  ! ------------------------------------------------------------------ !


  ! ------------------------------------------------------------------ !
  !     output                                                         !
  ! ------------------------------------------------------------------ !

  !..settings
  open(40, file = './res/part_init.dat', action = 'write')
  open(41, file = './res/part_fini.dat', action = 'write')

  !..movie
  if      (mode_run == 1) then
     write(f_name,'("./res/lpt/lpt_", i4.4, ".dat")') n_init
  else if (mode_run == 2) then
     write(f_name,'("./res/lpt/lpt_", i6.6, ".dat")') n_init
  end if
  open(60, file = f_name, form = 'unformatted', action = 'write')

  open(65, file = './res/anim_set.dat', action = 'write')

  if      (mode_run == 1) then
     write(f_name,'("./res/anim/anim_", i4.4, ".dat")') n_init
  else if (mode_run == 2) then
     write(f_name,'("./res/anim/anim_", i6.6, ".dat")') n_init
  end if
  open(66, file = f_name, action = 'write')


  !..log files
  open(70, file = './res/part_mass.log', action='write')
  open(71, file = './res/condition.log', action = 'write')

  !     output                                                         !
  ! ------------------------------------------------------------------ !

  !! for keizoku
  if( k_zoku == 1 ) open(91, file = './in/fini.dat' , action = 'read')


  return

end subroutine ofile
