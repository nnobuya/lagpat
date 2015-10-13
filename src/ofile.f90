subroutine ofile

  use mod_set, only: k_zoku, n_init, set_param, path

  implicit none

  character:: no*4, f_name*100

  ! ------------------------------------------------------------------ !
  !     input                                                          !
  ! ------------------------------------------------------------------ !

  call set_param(15)

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
  open(41, file = '../res/part_mass.log', action='write')
  open(42, file = '../res/init_part.dat', action='write')

  !..particle motion
  open(60, file = '../res/set.dat'  , action = 'write')

  !! position
  open(61, file = '../res/posi_lpt.dat', &
       & form = 'unformatted', action = 'write')

  !! moving
  open(62, file = '../res/move.dat', &
       & form = 'unformatted', action = 'write')

  !! Density, Temperature, Ye
  open(63, file = '../res/hydro.dat', &
       & form = 'unformatted', action = 'write')

  !! Status
  open(64, file = '../res/stat_lpt.dat', &
       & form = 'unformatted', action = 'write')

  !..movie
  open(65, file = '../res/anim_set.dat', action = 'write')
  write(f_name,'("../res/anim/anim_", i4.4, ".dat")') n_init
  open(66, file = f_name, action = 'write')


  !..log files
  open(70, file = '../res/condition.log', action = 'write')

  !     output                                                         !
  ! ------------------------------------------------------------------ !


  !! final status
  open(90, file = '../res/fini.dat', action = 'write')

  !! for keizoku
  if( k_zoku == 1 ) open(91, file = '../in/fini.dat' , action = 'read')


  return

end subroutine ofile
