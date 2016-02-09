module mod_cnst

  implicit none

  private
  public:: npt, npt_rad, npt_the, ndim, pi, rm_sol, set_cnst


  !..part. num. and dimension
  integer:: npt, npt_rad, npt_the, ndim

  !..const.
  real(8), parameter:: pi     = 3.141592653589793d0
  real(8), parameter:: rm_sol = 1.9891d33

contains

  subroutine set_cnst(io)

    implicit none

    !..io
    integer,intent(in):: io

    read(io,*)
    read(io,*) npt_rad, npt_the, ndim

    npt     = npt_rad *npt_the

    return

  end subroutine set_cnst

end module mod_cnst
