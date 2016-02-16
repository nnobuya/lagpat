module mod_set

  use mod_const, only: nel, nel_ou

  implicit none

  private
  public:: ndt, &
       & na, nz, nn, ra, rz, rn, na_ou, nn_ou, nz_ou, &
       & name, be, qm, spin, npt, cpt, ct9, &
       & ti, de, te, ye, qm_n, qm_p, &
       & set_nuclei, set_rhot

  !..rhot
  integer:: ndt
  real*8 :: ti_init, dt_init, ye_init
  real*8 :: temp_lim, t_min !! limit value
  real*8, allocatable :: ti(:), de(:), te(:), ye(:)


  integer:: nn(1:nel), na(1:nel), nz(1:nel)
  integer:: nn_ou(1:nel), na_ou(1:nel), nz_ou(1:nel)
  real*8 :: qm_n, qm_p
  real*8 :: rn(1:nel), ra(1:nel), rz(1:nel)
  real*8 :: be(1:nel), qm(1:nel), spin(1:nel)

  integer:: npt
  real*8, allocatable:: cpt(:,:), ct9(:)
  character :: name(1:nel)*14


contains

  subroutine set_rhot(io)

    implicit none

    integer, intent(in):: io

    real*8 :: r1
    integer:: i, j, ier


    lp_ndt: do
       read(io,*,iostat = ier )

       if(ier /= 0) exit lp_ndt

       ndt = ndt + 1
    end do lp_ndt


    allocate ( ti(1:ndt), de(1:ndt), te(1:ndt), ye(1:ndt), stat = ier)
    if(ier /= 0) stop 'qnse(): error'

    rewind(io)

    do i = 1, ndt
       read(io,*) ti(i), de(i), te(i), ye(i)
    end do



    close(io)

    return

  end subroutine set_rhot


  subroutine set_nuclei

    implicit none

    double precision:: r1, r2
    integer :: i, j, k, js, je, ier
    character:: ch*100

    qm_n = 0.d0
    qm_p = 0.d0

    read(40,*) npt

    allocate( ct9(1:npt), cpt(1:npt,1:nel), stat = ier)
    if(ier /= 0) stop 'set_nuclei(): error '


    !..t9 grid
    do i = 1, npt, 12
       read(40,*) ( ct9(j), j = i, i + 11 )
    end do


    !..pt func.
    read(40,*) j
    if (j /= nel) stop 'error'

    do i = 1, nel
       read(40,*) name(i), na(i), nz(i), nn(i), spin(i), qm(i)
       do k = 1, npt, 8
          js = k
          je = k + 7
          read(40,*) ( cpt(j,i), j =  js,  je )
       end do
    end do
    close(40)

    cpt(1:npt,1:nel) = log10(cpt(1:npt,1:nel))

    ra(1:nel) = dble(na(1:nel))
    rz(1:nel) = dble(nz(1:nel))
    rn(1:nel) = dble(nn(1:nel))

    do i = 1, nel
       if(na(i) == 1 .and. nz(i) == 0) qm_n = qm(i)
       if(na(i) == 1 .and. nz(i) == 1) qm_p = qm(i)

       if( qm_n /= 0 .and. qm_p /= 0 ) exit

    end do

    if(qm_n == 0 .and. qm_p == 0) stop 'set_nuclei(): no p or n'

    be(1:nel) = rn(1:nel) *qm_n + rz(1:nel) *qm_p - qm(1:nel)


    read(41,*) j
    if ( j /= nel_ou ) stop 'error'

    do i = 1, nel_ou
       read(41,*) ch, na_ou(i), nz_ou(i), nn_ou(i), r1, r2
       read(41,*)
       read(41,*)
       read(41,*)
    end do

    close(41)




    return

  end subroutine set_nuclei



end module mod_set
