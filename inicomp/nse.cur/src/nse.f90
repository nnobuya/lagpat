program nse

  use mod_const, only: nel
  use mod_set  , only: ndt, no, ti, de, te, ye, set_nuclei, set_rhot

  implicit none

  !..main
  integer:: istp, len, istat, istp_st, istp_en
  real*8 :: rmu_p_lg, rmu_n_lg
  real*8 :: x(1:nel)
  character*10:: arg1, arg2

  intrinsic:: command_argument_count, get_command_argument

  call get_command_argument(1, arg1, status = istat)
  call get_command_argument(2, arg2, status = istat)

  read(arg1,*) istp_st
  read(arg2,*) istp_en


  !..open
  open(40,file='./in/part.ame.fz4421', action = 'read')
  open(41,file='./in/part.z4071'     , action = 'read')

  open(51,file='./table.in', action = 'read')

  call set_nuclei
  call set_rhot(51)


  !main_loop: do istp = 1, ndt
  main_loop: do istp = istp_st, istp_en

     if ( mod(istp,1000) == 1 ) write(*,'(*(i10))') no(istp), istp, ndt

     call eq_nse( istp, te(istp) /1.d9, ye(istp), de(istp), &
          & rmu_p_lg, rmu_n_lg, x(1:nel) )
     ! i&o: rmu_p_lg, rmu_n_lg
     ! out: x(:)


     call output ( no(istp), x(1:nel) )


  end do main_loop

  !    main loop                                                       !
  ! ================================================================== !


  stop 'normal temination'

end program nse
