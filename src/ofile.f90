subroutine ofile

  use mod_cnst, only: set_cnst
  use mod_set , only: k_zoku, n_init, set_param, path

  implicit none

  character:: no*4, f_name*100

  ! ------------------------------------------------------------------ !
  !     input                                                          !
  ! ------------------------------------------------------------------ !

  open(15, file = './in/lagpat.in', action = 'read')

  call set_param(15)

  call set_cnst(15)

  close(15)

  write(no,'(i4.4)') n_init
  f_name = trim(adjustl(path)) // '/rpr' // no // '.dat'

  !..hydro results (initial)
  open(50, file = f_name, form = 'unformatted', action = 'read')

  !     input                                                          !
  ! ------------------------------------------------------------------ !


  ! ------------------------------------------------------------------ !
  !     output                                                         !
  ! ------------------------------------------------------------------ !

  !..settings
  open(40, file = './res/part_init.dat', action = 'write')
  open(41, file = './res/part_fini.dat', action = 'write')

  !..movie
  write(f_name,'("./res/lpt/lpt_", i4.4, ".dat")') n_init
  open(60, file = f_name, form = 'unformatted', action = 'write')

  open(65, file = './res/anim_set.dat', action = 'write')

  write(f_name,'("./res/anim/anim_", i4.4, ".dat")') n_init
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
