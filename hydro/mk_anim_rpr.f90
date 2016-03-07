program mk_anim_rpr

  implicit none

  !..param.
  integer  :: ndt_in, ndt_ou, nstp, nrd, nth
  character:: path*100
  real(8), parameter:: sol_m = 1.989d33

  !..main
  integer:: idt
  logical:: initial = .true.

  real(8):: ti, lnue, lnue_, enue, enue_
  real(8):: b_st
  integer, allocatable:: flgexp(:,:)
  real(8), allocatable, dimension(:)  :: rd, th
  real(8), allocatable, dimension(:,:):: x1, x2, vo, de, dvol, &
       & v1, v2, v3, b1, b2, b3, ye, te, s, eg, etot, rnu0

  !..tmp
  integer  :: i, j, i1, i2, ier
  character:: op_file*100, no*10


  ! ---------------------------------------------------------------- !
  !     preparation                                                  !
  ! ---------------------------------------------------------------- !

  open(50, file = './mk_anim.in', action = 'read')

  !..time step data for anime
  open(60, file = './res/time_anime.dat', action = 'write')


  read(50,*)
  read(50,*) ndt_in, ndt_ou, nstp
  read(50,*)
  read(50,*) nrd, nth
  read(50,*)
  read(50,*) path

  allocate( x1(1:nrd,1:nth), x2(1:nrd,1:nth), rd(1:nrd), th(1:nth), &
       & vo(1:nrd,1:nth), de(1:nrd,1:nth), dvol(1:nrd,1:nth), &
       & v1(1:nrd,1:nth), v2(1:nrd,1:nth), v3(1:nrd,1:nth), &
       & b1(1:nrd,1:nth), b2(1:nrd,1:nth), b3(1:nrd,1:nth), &
       & ye(1:nrd,1:nth), te(1:nrd,1:nth),  s(1:nrd,1:nth), &
       & eg(1:nrd,1:nth), etot(1:nrd,1:nth), rnu0(1:nrd,1:nth), &
       & flgexp(1:nrd,1:nth) )


  !     preparation                                                  !
  ! ---------------------------------------------------------------- !



  ! ---------------------------------------------------------------- !
  !     main loop                                                    !
  ! ---------------------------------------------------------------- !

  lp_main: do idt = ndt_in, ndt_ou, nstp

     !..open
     write(no,'(i4.4)') idt
     op_file = trim(path) // trim(no) // '.dat'
     open(50, file = op_file, form = 'unformatted', action = 'read')

     op_file = './res/anim_' // trim(no) // '.dat'
     open(61, file = op_file, action = 'write')

     op_file = './res/hist_' // trim(no) // '.dat'
     open(62, file = op_file, action = 'write')


     !..read data
     read(50) ti, lnue, lnue_, enue, enue_

     ti = ti *1.e3
     
     write(*,'(i10,1pe14.3)') idt, ti

     do j = 1, nth
        do i = 1, nrd
           read(50) &
                & i1, i2, rd(i), th(j), de(i,j),  dvol(i,j), &
                & v1(i,j), v2(i,j), v3(i,j), b1(i,j), b2(i,j), b3(i,j), &
                & ye(i,j), te(i,j),  s(i,j), eg(i,j), etot(i,j)  , &
                & rnu0(i,j), flgexp(i,j)
           x1(i,j) = rd(i) *sin(th(j))
           x2(i,j) = rd(i) *cos(th(j))
        end do
     end do

     close(50)

     !print *, sum(dvol(1:nrd,1:nth) *de(1:nrd,1:nth))/sol_m

     write(60,'(f7.1)') ti
     do j = 1, nth
        do i = 1, nrd
           b_st = sqrt(b1(i,j)**2 + b2(i,j)**2 + b3(i,j)**2)
           write(61,'(1p, *(e10.2))') &
                & x1(i,j), x2(i,j), &
                & log10(de(i,j)), log10(te(i,j)), &
                & s(i,j), ye(i,j), log10(b_st), eg(i,j)
           write(62,'(1p, *(e12.4))') &
                & rd(i), th(j), s(i,j), ye(i,j), dvol(i,j) *de(i,j)/sol_m
        end do
        write(61,*)
     end do

     close(61)
     close(62)

  end do lp_main


  ! ------------------------------------------------------------------ !
  !     open                                                           !
  ! ------------------------------------------------------------------ !


  close(51)
  close(70)
  close(71)



  stop 'normal termination'

end program mk_anim_rpr
