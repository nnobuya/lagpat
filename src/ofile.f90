subroutine ofile

  use mod_set, only: k_zoku, set_param

  implicit none

  character:: path*50

  character*60:: grid, vr, vt, vp, de, te, ye

  ! ------------------------------------------------------------------ !
  !     input                                                          !
  ! ------------------------------------------------------------------ !

  open(15, file = '../lagpat.in', action = 'read')

  read(15,*)
  read(15,*) path
  read(15,*)

  call set_param(15)


  !..set file name
  write(grid,*) adjustr(path)//'grid.lpt'
  write(vr,*)   adjustr(path)//'vr.lpt'
  write(vt,*)   adjustr(path)//'vt.lpt'
  write(vp,*)   adjustr(path)//'vp.lpt'
  write(de,*)   adjustr(path)//'de.lpt'
  write(te,*)   adjustr(path)//'te.lpt'
  write(ye,*)   adjustr(path)//'ye.lpt'

  !..hydro result
  open(50, file = grid, form = 'unformatted', action = 'read')
  open(51, file = vr  , form = 'unformatted', action = 'read')
  open(52, file = vt  , form = 'unformatted', action = 'read')
  open(53, file = vp  , form = 'unformatted', action = 'read')
  open(54, file = de  , form = 'unformatted', action = 'read')
  open(55, file = te  , form = 'unformatted', action = 'read')
  open(56, file = ye  , form = 'unformatted', action = 'read')

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
  open(66, file = '../res/anim.dat'    , action = 'write')


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
