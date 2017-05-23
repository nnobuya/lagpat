module mod_cnst

  implicit none

  private
  public:: pi, rm_sol, v_c, r_mev

  !..const.
  double precision, parameter:: &
       & pi     = 3.141592653589793d0, &
       & rm_sol = 1.9891d33          , &
       & v_c    = 2.99792458d10      , &
       & r_mev  = 1.1604505d10 

end module mod_cnst
