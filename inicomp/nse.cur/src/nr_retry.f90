subroutine nr_retry(xr, rho, rho_lg_in, ye, ye_in, zlp, zln)

  use mod_const, only: nel
  use mod_set  , only: rn, ra, rz

  implicit none

  real*8, intent(in) :: xr(nel), rho, rho_lg_in, ye, ye_in
  real*8, intent(out):: zlp, zln

  real*8:: rhol, zai(nel)
  real*8:: det, dr, de, drdp, drdn, dedp, dedn, dzlp, dzln, dzl2, fact


  zai(1:nel) = rz(1:nel) /ra(1:nel)

  !..re-trial
  rhol = log10(rho)

  drdp = sum( rz(1:nel) *xr(1:nel) )
  drdn = sum( rn(1:nel) *xr(1:nel) )

  dedp = sum( rz(1:nel) *zai(1:nel) *xr(1:nel) )
  dedn = sum( rn(1:nel) *zai(1:nel) *xr(1:nel) )

  drdp = drdp /rho
  drdn = drdn /rho

  dedp = dedp /ye /rho - drdp
  dedn = dedn /ye /rho - drdn

  det  = drdp *dedn - drdn *dedp
  dr   = rho_lg_in - rhol
  de   = log10(ye_in /ye)
  dzlp = ( dr *dedn - de *drdn ) /det
  dzln = ( - dr *dedp + de *drdp ) /det
  dzl2 = sqrt( dzlp *dzlp + dzln *dzln )
  fact = 1.d0

  if( dzl2 > 0.5 ) fact = 0.5d0 /dzl2

  !..reset
  zlp = zlp + fact *dzlp
  zln = zln + fact *dzln

  return

end subroutine nr_retry
