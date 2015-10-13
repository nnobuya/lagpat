module mod_fld

  implicit none

  private
  public:: ihyd0, dt_max, fld, fld_set, rd_fld

  double precision, allocatable, dimension(:,:,:):: &
       & d_fld0, t_fld0, ye_fld0, d_fld1, t_fld1, ye_fld1
  double precision, allocatable, dimension(:,:,:,:):: v_fld0, v_fld1

  integer:: ihyd0, ihyd1
  double precision:: ti1, ti0
  double precision:: dt_max

contains

  subroutine fld_set

    use mod_cnst, only: ndim
    use mod_set , only: nx1, nx2, nx3

    implicit none

    !..io
    integer:: ier

    allocate ( &

         &  d_fld0(1:nx1,1:nx2,1:nx3),  d_fld1(1:nx1,1:nx2,1:nx3), &
         &  t_fld0(1:nx1,1:nx2,1:nx3),  t_fld1(1:nx1,1:nx2,1:nx3), &
         & ye_fld0(1:nx1,1:nx2,1:nx3), ye_fld1(1:nx1,1:nx2,1:nx3), &
         & v_fld0(1:ndim,1:nx1,1:nx2,1:nx3), &
         & v_fld1(1:ndim,1:nx1,1:nx2,1:nx3), &
         & stat = ier )
    if( ier /= 0 ) stop 'fld_set(): error'


    return

  end subroutine fld_set


  subroutine fld(ihyd, d_fld, t_fld, ye_fld, v_fld, v0_fld)

    use mod_cnst, only: ndim
    use mod_set  , only: nx1, nx2, nx3, path

    implicit none

    integer, intent(in):: ihyd

    !..io
    real(8), intent(out):: &
         &  d_fld(1:nx1,1:nx2,1:nx3), t_fld(1:nx1,1:nx2,1:nx3), &
         & ye_fld(1:nx1,1:nx2,1:nx3), &
         & v_fld(1:ndim,1:nx1,1:nx2,1:nx3), v0_fld(1:ndim,1:nx1,1:nx2,1:nx3)
    
    !..local
    character:: f_name*100, no*4
    logical, save:: initial = .true.


    if (initial) then
       !..read field data (1st step)
       call fld_set
       call rd_fld(ti0, &
            & d_fld0(:,:,:), t_fld0(:,:,:), ye_fld0(:,:,:), v_fld0(:,:,:,:) )
       initial = .false.
    else
       ihyd0 = ihyd1
       ti0 = ti1
       d_fld0(1:nx1,1:nx2,1:nx3)        = d_fld1(1:nx1,1:nx2,1:nx3)
       t_fld0(1:nx1,1:nx2,1:nx3)        = t_fld1(1:nx1,1:nx2,1:nx3)
       ye_fld0(1:nx1,1:nx2,1:nx3)       = ye_fld1(1:nx1,1:nx2,1:nx3)
       v_fld0(1:ndim,1:nx1,1:nx2,1:nx3) = v_fld1(1:ndim,1:nx1,1:nx2,1:nx3)
    end if


    write(no,'(i4.4)') ihyd + 1
    f_name = trim(adjustl(path)) // '/rpr' // no // '.dat'
    open(50, file = f_name, form = 'unformatted', action = 'read')

    !..density & temperature
    call rd_fld(ti1, &
         & d_fld1(:,:,:), t_fld1(:,:,:), ye_fld1(:,:,:), v_fld1(:,:,:,:) )

    dt_max = ti1 - ti0



    !..set fld

!    if( ier == 0 ) then
       d_fld(1:nx1,1:nx2,1:nx3)          = d_fld0(1:nx1,1:nx2,1:nx3)
       t_fld(1:nx1,1:nx2,1:nx3)          = t_fld0(1:nx1,1:nx2,1:nx3)
       ye_fld(1:nx1,1:nx2,1:nx3)         = ye_fld0(1:nx1,1:nx2,1:nx3)
       v_fld(1:ndim,1:nx1,1:nx2,1:nx3)   = v_fld0(1:ndim,1:nx1,1:nx2,1:nx3)
       v0_fld(1:ndim,1:nx1,1:nx2,1:nx3)  = v_fld0(1:ndim,1:nx1,1:nx2,1:nx3)
!    else
!       d_fld(1:nx1,1:nx2,1:nx3)         = d_fld1(1:nx1,1:nx2,1:nx3)
!       t_fld(1:nx1,1:nx2,1:nx3)         = t_fld1(1:nx1,1:nx2,1:nx3)
!       ye_fld(1:nx1,1:nx2,1:nx3)        = ye_fld1(1:nx1,1:nx2,1:nx3)
!       v_fld(1:ndim,1:nx1,1:nx2,1:nx3)  = v_fld1(1:ndim,1:nx1,1:nx2,1:nx3)
!       v0_fld(1:ndim,1:nx1,1:nx2,1:nx3) = v_fld1(1:ndim,1:nx1,1:nx2,1:nx3)
!    end if


    return

  end subroutine fld



  subroutine rd_fld(ti, d_fld, t_fld, ye_fld, v_fld)

    use mod_cnst, only: ndim
    use mod_set , only: nx1, nx2, nx3

    implicit none

    real(8), intent(out)::  ti, &
         & d_fld(nx1,nx2,nx3), t_fld(nx1,nx2,nx3), &
         & ye_fld(nx1,nx2,nx3), v_fld(ndim,nx1,nx2,nx3)

    real(8):: lnue, lnue_, enue, enue_, dummy(1:9)
    integer:: i, j, k, i1, i2, i3


    read(50) ti, lnue, lnue_, enue, enue_

    do k = 1, nx3
       do j = 1, nx2
          do i = 1, nx1
             read(50) &
                  & i1, i2, dummy(1:2), d_fld(i,j,k), dummy(3), &
                  & v_fld(1:ndim,i,j,k), dummy(4:6), &
                  & ye_fld(i,j,k), t_fld(i,j,k), &
                  & dummy(6:9), i3
          end do
       end do
    end do


    close(50)

    return

  end subroutine rd_fld


end module mod_fld
