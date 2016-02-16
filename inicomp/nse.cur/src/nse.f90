program nse

  use mod_const, only: nel
  use mod_set  , only: ndt, ti, de, te, ye, set_nuclei, set_rhot

  implicit none

  !..main
  integer:: istp
  real*8 :: rmu_p_lg, rmu_n_lg
  real*8 :: x(1:nel)



  !..open
  open(40,file='./in/part.ame.fz4421', action = 'read')
  open(41,file='./in/part.z4071'     , action = 'read')

  open(51,file='./table.in', action = 'read')

  call set_nuclei
  call set_rhot(51)


  main_loop: do istp = 1, ndt

     if ( mod(istp,100) == 1 ) write(*,'(2i10)') istp, ndt

     call eq_nse( istp, te(istp) /1.d9, ye(istp), de(istp), &
          & rmu_p_lg, rmu_n_lg, x(1:nel) )
     ! i&o: rmu_p_lg, rmu_n_lg
     ! out: x(:)


     call output ( istp, x(1:nel) )


  end do main_loop

  !    main loop                                                       !
  ! ================================================================== !


  stop 'normal temination'

end program nse
