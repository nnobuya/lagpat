subroutine part_func( ye, rho, t9, gel )

  use mod_const, only: nel, pi, rm_u, rkb, h, beta
  use mod_set  , only: ra, rn, rz, qm, qm_n, qm_p, npt, ct9, cpt, spin, nz, na

  implicit none

  !..io
  real*8, intent(in) :: t9, rho, ye
  real*8, intent(out):: gel(1:nel)

  !..func.
  real*8, external:: fitlag, func_coul

  integer:: i, ierr
  real*8 :: g(1:nel)
  real*8 :: const, yerr

  real*8 :: rm_coul(1:nel), q_c(1:nel), rm_c_p


  do i = 1, nel
     g(i) = fitlag(t9, npt, 3, ct9(:), cpt(:,i), 1.d-3, ierr, yerr)
  end do

  do i = 1, nel
     rm_coul(i) = func_coul( na(i), nz(i), ye, rho, t9 )
  end do


  do i = 1, nel
     if ( nz(i) == 1 .and. na(i) == 1 ) then
        rm_c_p = rm_coul(i)
        exit
     end if
  end do


  q_c(1:nel) = dble( nz(1:nel) ) *rm_c_p - rm_coul(1:nel)

  const = &
       &   2.5d0 *log10(rm_u) &
       & + 1.5d0 *( log10(t9) + 9.d0 + log10(rkb) + log10(2.d0 *pi) ) &
       & - 3.d0 *log10(h)

  gel(1:nel) = &
       &   g(1:nel) &
       & + log10( 2.d0 *spin(1:nel) + 1.d0 ) &
       & + 2.5d0 *log10(ra(1:nel)) &
       & - ( qm(1:nel) + rn(1:nel) *qm_n + rz(1:nel) *qm_p - q_c(1:nel) ) &
       & *beta /t9 /log(10.d0)

  gel(1:nel) = gel(1:nel) + const


  return

end subroutine part_func
