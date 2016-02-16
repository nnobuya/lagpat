module mod_const

  implicit none

  private

  public:: nel, nel_ou, rm_u, rkb, rkb_mev, h, e_esu, beta, pi, x_min

  !..element
  !integer, parameter:: nel = 471
  integer, parameter:: nel    = 4421
  integer, parameter:: nel_ou = 4071
  real*8, parameter:: x_min = 1.d-60  !! minimum value of X_i

  !..physical const.
  real*8, parameter:: beta    = 11.604505d0     !! 1 MeV in G Kelvin [GK]
  real*8, parameter:: rm_u    = 1.660538782d-24 !! atomic mass unit  [g]
  real*8, parameter:: rkb     = 1.3806504d-16   !! Boltzman const.   [erg/K]
  real*8, parameter:: rkb_mev = 8.6173324d-11   !! Boltzman const.   [MeV/K]
  real*8, parameter:: h       = 6.62606896d-27  !! Plank const.      [erg s]
  real*8, parameter:: e_c     = 1.602176487d-19 !! electron charge   [C]
  real*8, parameter:: e_esu   = e_c *3.d9       !! electron charge   [esu]

  !..mathematical const.
  real*8, parameter:: pi = 3.141592653589793d0


end module mod_const
