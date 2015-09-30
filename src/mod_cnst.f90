module mod_cnst

  implicit none

  private
  public:: npt, npt_rad, npt_the, ndim, pi, rm_sol


  !..part. num. and dimension
  integer, parameter:: npt     = 10000
  integer, parameter:: npt_rad = 100, npt_the = 100, ndim = 3
  !integer, parameter:: nin = 1, nou = 2  !! for hokan grid


  !..const.
  double precision, parameter:: pi     = 3.141592653589793d0
  double precision, parameter:: rm_sol = 1.9891d33

end module mod_cnst
