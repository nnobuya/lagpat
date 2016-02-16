subroutine ini_set(rho, t9, ye, gel, zlp, zln)

  use mod_const, only: nel, beta, pi
  use mod_set  , only: na, nz, nn, name

  implicit none

  !..io
  real*8, intent(in) :: rho, t9, ye, gel(nel)
  real*8, intent(out):: zlp, zln

  integer:: iel_n, iel_p, iel_s32
  real*8 :: zsum1, zsum4, zsum, dez
  real*8 :: zdif, t9i, rhol


  !..get element number
  do iel_n = 1, nel
     if(nz(iel_n) == 0 .and. na(iel_n) == 1) exit
  end do

  do iel_p = 1, nel
     if(nz(iel_p) == 1 .and. na(iel_p) == 1) exit
  end do

  do iel_s32 = 1, nel
     if(nz(iel_s32) == 16 .and. na(iel_s32) == 32) exit
  end do

  if(iel_n > nel .or. iel_p > nel .or. iel_s32 > nel) then 
     stop 'ini_set: error'
  end if


  !..begin
  t9i   = 1.d0 /t9
  rhol  = log10(rho)

  zsum1 = (rhol - gel(iel_n))
  zsum4 = (rhol - gel(iel_s32)) /32.d0
  zsum  = min(zsum1,zsum4)
  zsum  = zsum4

  if(zsum == zsum1) then
     zdif = - 782.397d0 * beta *1.d-3 *t9i *0.5d0 /log(10.d0) /ye
  else
     dez  = 56.d0 *ye - 26.d0
     zdif = (340.75d0 + 3010.45d0 *dez) *beta *1.d-3 **t9i *0.5d0 /log(10.d0)
  end if

  zdif = (gel(iel_n) - gel(iel_p)) *0.5d0

  !..setting
  zlp = zsum + zdif
  zln = zsum - zdif


  return

end subroutine ini_set
