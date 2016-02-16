subroutine eq_nse( istep, t9, ye, de, rmu_p_lg, rmu_n_lg, x )

  use mod_const, only: nel
  use mod_set  , only: nz

  implicit none

  real*8, external:: coul_mu

  integer, intent(in)   :: istep
  real*8 , intent(in)   :: t9, ye, de
  real*8 , intent(out)  :: x(1:nel)
  real*8 , intent(inout):: rmu_p_lg, rmu_n_lg

  integer:: i, i_try, n_conv
  real*8 :: gel(1:nel), rmu_coul(1:nel)
  real*8 :: de_nr, ye_nr   !! newton raphson



  call part_func( ye, de, t9, gel(:) )
  !  in: t9, ye, de
  ! out: g(nel), gel(nel)


  !..initial
  de_nr = de
  ye_nr = ye

  iter_loop: do i_try = 1, 7

     !..set initial chemical potential
     if( ( istep == 1 .and. i_try == 1 ) .or. i_try == 2 ) then

        !..initial rmu_p_lg, rmu_n_lg
        call ini_set( de, t9, ye, gel(:), rmu_p_lg, rmu_n_lg )
        !  in: de, t9, ye, gel
        ! out: rmu_p_lg, rmu_n_lg

     else if ( i_try == 3 ) then
        rmu_p_lg = 0.d0
        rmu_n_lg = 0.d0
     else if ( i_try == 4 ) then
        rmu_p_lg = 0.5d0
        rmu_n_lg = 0.5d0
     else if ( i_try == 5 ) then
        rmu_p_lg = - 0.5d0
        rmu_n_lg = - 0.5d0
     else if ( i_try == 6 ) then
        rmu_p_lg = 5.d0
        rmu_n_lg = 5.d0
     else if ( i_try == 7 ) then
        rmu_p_lg = - 5.d0
        rmu_n_lg = - 5.d0
     end if


     call new_raph( gel(:), de_nr, ye_nr, rmu_p_lg, rmu_n_lg, x(:), n_conv )
     ! in   : gel(nel)
     ! inout: de_nr, ye_nr, rmu_p_lg, rmu_n_lg
     ! out  : xrl(nel)


     if( n_conv == 0 .or. n_conv == 1 ) exit iter_loop

     de_nr = de
     ye_nr = ye

  end do iter_loop


  if( i_try >= 4 ) write(*,'("re-try",i5)') i_try



  return

end subroutine eq_nse
