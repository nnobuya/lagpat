module mod_fld

  implicit none

  private
  public:: ihyd0, dt_max, fld, fld_set, rd_fld

  integer:: nrd = 0
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


  subroutine fld( ier, d_fld, t_fld, ye_fld, v_fld, v0_fld )

    use mod_cnst, only: ndim
    use mod_set  , only: nx1, nx2, nx3

    implicit none

    !..io
    integer, intent(out):: ier
    double precision, intent(out):: &
         &  d_fld(1:nx1,1:nx2,1:nx3), t_fld(1:nx1,1:nx2,1:nx3), &
         & ye_fld(1:nx1,1:nx2,1:nx3), &
         & v_fld(1:ndim,1:nx1,1:nx2,1:nx3), v0_fld(1:ndim,1:nx1,1:nx2,1:nx3)

    if( nrd == 0 ) then
       !..read field data (1st step)
       call fld_set
       call rd_fld( ier, ihyd0, ti0, &
            & d_fld0(:,:,:), t_fld0(:,:,:), ye_fld0(:,:,:), v_fld0(:,:,:,:) )
    else
       ihyd0 = ihyd1
       ti0 = ti1
       d_fld0(1:nx1,1:nx2,1:nx3)        = d_fld1(1:nx1,1:nx2,1:nx3)
       t_fld0(1:nx1,1:nx2,1:nx3)        = t_fld1(1:nx1,1:nx2,1:nx3)
       ye_fld0(1:nx1,1:nx2,1:nx3)       = ye_fld1(1:nx1,1:nx2,1:nx3)
       v_fld0(1:ndim,1:nx1,1:nx2,1:nx3) = v_fld1(1:ndim,1:nx1,1:nx2,1:nx3)
    end if

    nrd = nrd + 1


    !..density & temperature
    call rd_fld( ier, ihyd1, ti1, &
         & d_fld1(:,:,:), t_fld1(:,:,:), ye_fld1(:,:,:), v_fld1(:,:,:,:) )

    dt_max = ti1 - ti0

    !..set fld

    if( ier == 0 ) then
       d_fld(1:nx1,1:nx2,1:nx3)          = d_fld0(1:nx1,1:nx2,1:nx3)
       t_fld(1:nx1,1:nx2,1:nx3)          = t_fld0(1:nx1,1:nx2,1:nx3)
       ye_fld(1:nx1,1:nx2,1:nx3)         = ye_fld0(1:nx1,1:nx2,1:nx3)
       v_fld(1:ndim,1:nx1,1:nx2,1:nx3)   = v_fld1(1:ndim,1:nx1,1:nx2,1:nx3)
       v0_fld(1:ndim,1:nx1,1:nx2,1:nx3)  = v_fld0(1:ndim,1:nx1,1:nx2,1:nx3)
    else
       d_fld(1:nx1,1:nx2,1:nx3)         = d_fld1(1:nx1,1:nx2,1:nx3)
       t_fld(1:nx1,1:nx2,1:nx3)         = t_fld1(1:nx1,1:nx2,1:nx3)
       ye_fld(1:nx1,1:nx2,1:nx3)        = ye_fld1(1:nx1,1:nx2,1:nx3)
       v_fld(1:ndim,1:nx1,1:nx2,1:nx3)  = v_fld0(1:ndim,1:nx1,1:nx2,1:nx3)
       v0_fld(1:ndim,1:nx1,1:nx2,1:nx3) = v_fld0(1:ndim,1:nx1,1:nx2,1:nx3)
    end if


    return

  end subroutine fld


  subroutine rd_fld( ier, istep, ti, d_fld, t_fld, ye_fld, v_fld )

    use mod_cnst, only: ndim
    use mod_set , only: nx1, nx2, nx3

    implicit none

    integer, intent(out):: ier, istep
    double precision, intent(out):: ti, &
         & d_fld(nx1,nx2,nx3), t_fld(nx1,nx2,nx3), &
         & ye_fld(nx1,nx2,nx3), v_fld(ndim,nx1,nx2,nx3)

    real   :: ti_in, fld_in(1:nx1,1:nx2,1:nx3)
    integer:: i


    read(54,iostat=ier) istep, ti_in
    if (ier /= 0) return

    read(54) fld_in(1:nx1,1:nx2,1:nx3)
    ti = dble( ti_in )
    d_fld(1:nx1,1:nx2,1:nx3) = dble( fld_in(1:nx1,1:nx2,1:nx3) )


    read(55,iostat=ier) istep, ti_in
    if ( ier /= 0 ) return
    read(55) fld_in(1:nx1,1:nx2,1:nx3)
    ti = dble( ti_in )
    t_fld(1:nx1,1:nx2,1:nx3) = dble( fld_in(1:nx1,1:nx2,1:nx3) )


    do i = 1, ndim
       read(50+i,iostat=ier) istep, ti_in
       if ( ier /= 0 ) return
       read(50+i) fld_in(1:nx1,1:nx2,1:nx3)
       ti = dble( ti_in )
       v_fld(i,1:nx1,1:nx2,1:nx3) = dble( fld_in(1:nx1,1:nx2,1:nx3) )
    end do

    read(56,iostat=ier) istep, ti_in
    if ( ier /= 0 ) return
    read(56) fld_in(1:nx1,1:nx2,1:nx3)
    ti = dble( ti_in )
    ye_fld(1:nx1,1:nx2,1:nx3) = dble( fld_in(1:nx1,1:nx2,1:nx3) )


    return

  end subroutine rd_fld


end module mod_fld
