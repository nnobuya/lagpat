module mod_fld

  implicit none

  private
  public:: ihyd0, dt_max, fld, fld_set, rd_fld

  double precision, allocatable, dimension(:,:,:):: &
       & d_fld0, t_fld0, s_fld0, ye_fld0, pr_fld0, &
       & d_fld1, t_fld1, s_fld1, ye_fld1, pr_fld1
  double precision, allocatable, dimension(:,:,:,:):: &
       & v_fld0, v_fld1, f_fld0, f_fld1

  integer:: ihyd0, ihyd1
  double precision:: ti1, ti0, dt_max

contains

  subroutine fld_set

    use mod_set , only: nx1, nx2, nx3, ndim

    implicit none

    !..io
    integer:: ier

    allocate ( &
         &  d_fld0(1:nx1,1:nx2,1:nx3),  d_fld1(1:nx1,1:nx2,1:nx3), &
         &  t_fld0(1:nx1,1:nx2,1:nx3),  t_fld1(1:nx1,1:nx2,1:nx3), &
         &  s_fld0(1:nx1,1:nx2,1:nx3),  s_fld1(1:nx1,1:nx2,1:nx3), &
         & ye_fld0(1:nx1,1:nx2,1:nx3), ye_fld1(1:nx1,1:nx2,1:nx3), &
         & pr_fld0(1:nx1,1:nx2,1:nx3), pr_fld1(1:nx1,1:nx2,1:nx3), &
         & f_fld0(1:3,1:nx1,1:nx2,1:nx3), &
         & f_fld1(1:3,1:nx1,1:nx2,1:nx3), &
         & v_fld0(1:ndim,1:nx1,1:nx2,1:nx3), &
         & v_fld1(1:ndim,1:nx1,1:nx2,1:nx3), &
         & stat = ier )
    if( ier /= 0 ) stop 'fld_set(): error'


    return

  end subroutine fld_set


  subroutine fld(ihyd, d_fld, t_fld, s_fld, ye_fld, pr_fld, f_fld, &
       & v_fld, v0_fld)

    use mod_set , only: nx1, nx2, nx3, path, ndim, mode_run

    implicit none

    integer, intent(in):: ihyd

    !..io
    double precision, intent(out):: &
         & d_fld(1:nx1,1:nx2,1:nx3),  t_fld(1:nx1,1:nx2,1:nx3), &
         & s_fld(1:nx1,1:nx2,1:nx3), ye_fld(1:nx1,1:nx2,1:nx3), &
         & pr_fld(1:nx1,1:nx2,1:nx3), &
         &  f_fld(1:3,1:nx1,1:nx2,1:nx3), &
         &  v_fld(1:ndim,1:nx1,1:nx2,1:nx3), &
         & v0_fld(1:ndim,1:nx1,1:nx2,1:nx3)
     
    !..local
    character:: f_name*100, no*10
    logical, save:: initial = .true.


    if (initial) then
       !..read field data (1st step)
       call fld_set
       call rd_fld(ti0, d_fld0(:,:,:), t_fld0(:,:,:), s_fld0(:,:,:), &
            & ye_fld0(:,:,:), pr_fld0(:,:,:), f_fld0(:,:,:,:), v_fld0(:,:,:,:))
       initial = .false.
    else
       ihyd0 = ihyd1
       ti0 = ti1
        d_fld0(1:nx1,1:nx2,1:nx3) =  d_fld1(1:nx1,1:nx2,1:nx3)
        t_fld0(1:nx1,1:nx2,1:nx3) =  t_fld1(1:nx1,1:nx2,1:nx3)
        s_fld0(1:nx1,1:nx2,1:nx3) =  s_fld1(1:nx1,1:nx2,1:nx3)
       ye_fld0(1:nx1,1:nx2,1:nx3) = ye_fld1(1:nx1,1:nx2,1:nx3)
       pr_fld0(1:nx1,1:nx2,1:nx3) = pr_fld1(1:nx1,1:nx2,1:nx3)
        f_fld0(1:3,1:nx1,1:nx2,1:nx3)    =  f_fld1(1:3,1:nx1,1:nx2,1:nx3)
        v_fld0(1:ndim,1:nx1,1:nx2,1:nx3) =  v_fld1(1:ndim,1:nx1,1:nx2,1:nx3)
    end if

    if      (mode_run == 1) then
       write(no,'(i4.4)') ihyd + 1
       f_name = trim(adjustl(path)) // '/rpr' // trim(adjustl(no)) // '.dat'
       open(50, file = f_name, form = 'unformatted', action = 'read')
    else if (mode_run == 2) then
       write(no,'(i6.6)') ihyd + 1
       f_name = trim(adjustl(path)) // '/hydro_' // trim(adjustl(no)) // '.dat'
       open(50, file = f_name, form = 'unformatted', &
            & convert = 'big_endian', action = 'read')
    else if (mode_run == 3) then
       write(no,'(i5.5)') ihyd + 1
       f_name = trim(adjustl(path)) // '/file' // trim(adjustl(no)) // '.data'
       open(50, file = f_name, form = 'unformatted', action = 'read')
    else
       write(*,*) 'ERROR: bad mode_run =', mode_run
       stop
    end if

    !..density & temperature
    call rd_fld(ti1, &
         & d_fld1(:,:,:), t_fld1(:,:,:), s_fld1(:,:,:), &
         & ye_fld1(:,:,:), pr_fld1(:,:,:), f_fld1(:,:,:,:) , v_fld1(:,:,:,:) )

    dt_max = ti1 - ti0



    !..set fld
     d_fld(1:nx1,1:nx2,1:nx3) =  d_fld0(1:nx1,1:nx2,1:nx3)
     t_fld(1:nx1,1:nx2,1:nx3) =  t_fld0(1:nx1,1:nx2,1:nx3)
     s_fld(1:nx1,1:nx2,1:nx3) =  s_fld0(1:nx1,1:nx2,1:nx3)
    ye_fld(1:nx1,1:nx2,1:nx3) = ye_fld0(1:nx1,1:nx2,1:nx3)
    pr_fld(1:nx1,1:nx2,1:nx3) = pr_fld0(1:nx1,1:nx2,1:nx3)

    f_fld(1:3,1:nx1,1:nx2,1:nx3) = f_fld0(1:3,1:nx1,1:nx2,1:nx3)

    v0_fld(1:ndim,1:nx1,1:nx2,1:nx3) = v_fld0(1:ndim,1:nx1,1:nx2,1:nx3)
     v_fld(1:ndim,1:nx1,1:nx2,1:nx3) = v_fld1(1:ndim,1:nx1,1:nx2,1:nx3)


    return

  end subroutine fld



  subroutine rd_fld(ti, d_fld, t_fld, s_fld, ye_fld, pr_fld, f_fld, v_fld)

    use mod_cnst, only: v_c, r_mev
    use mod_set , only: nx1, nx2, nx3, ndim, mode_run

    implicit none

    double precision, intent(out)::  ti, &
         & d_fld(nx1,nx2,nx3), t_fld(nx1,nx2,nx3), s_fld(nx1,nx2,nx3), &
         & ye_fld(nx1,nx2,nx3), pr_fld(nx1,nx2,nx3), f_fld(1:3,nx1,nx2,nx3), &
         & v_fld(ndim,nx1,nx2,nx3)

    double precision:: lnue, lnue_, enue, enue_, dummy(1:9)
    integer:: i, j, k, i1, i2, i3

    real:: ti_in, x1(1:nx1), x2(1:nx2), x3(1:nx3), &
         & de_in(1:nx1,1:nx2,1:nx3), ye_in(1:nx1,1:nx2,1:nx3), &
         & te_in(1:nx1,1:nx2,1:nx3), ut_in(1:nx1,1:nx2,1:nx3), &
         & qb_in(1:nx1,1:nx2,1:nx3), en_in(1:nx1,1:nx2,1:nx3), &
         & pr_in(1:nx1,1:nx2,1:nx3), f_in(1:3,1:nx1,1:nx2,1:nx3), &         
         & v1(1:nx1,1:nx2,1:nx3)   , v2(1:nx1,1:nx2,1:nx3)   , &
         & v3(1:nx1,1:nx2,1:nx3)   , vol_in(1:nx1,1:nx2,1:nx3)


    if (mode_run == 1) then
       read(50) ti, lnue, lnue_, enue, enue_
       do k = 1, nx3
          do j = 1, nx2
             do i = 1, nx1
                read(50) &
                     & i1, i2, dummy(1:2), d_fld(i,j,k), dummy(3), &
                     & v_fld(1:ndim,i,j,k), dummy(4:6), &
                     & ye_fld(i,j,k), t_fld(i,j,k), s_fld(i,j,k), &
                     & dummy(6:8), i3
             end do
          end do
       end do
       close(50)
    else if (mode_run == 2) then
       if (nx2 /= 1 ) then
          write(*,*) 'ERROR: check nx2'
          stop
       end if

       read(50) ti_in, x1(1:nx1), x3(1:nx3), &
            & de_in(1:nx1,1:nx2,1:nx3), ye_in(1:nx1,1:nx2,1:nx3), &
            & te_in(1:nx1,1:nx2,1:nx3), ut_in(1:nx1,1:nx2,1:nx3), &
            & qb_in(1:nx1,1:nx2,1:nx3), en_in(1:nx1,1:nx2,1:nx3), &
            & v1(1:nx1,1:nx2,1:nx3)   , v2(1:nx1,1:nx2,1:nx3)   , &
            & v3(1:nx1,1:nx2,1:nx3)

       ti = 1.d-3 *dble(ti_in) !! ms --> s

       j = 1
       d_fld( 1:nx1,j,1:nx3) = dble(de_in(1:nx1,j,1:nx3))
       ye_fld(1:nx1,j,1:nx3) = dble(ye_in(1:nx1,j,1:nx3))
       t_fld( 1:nx1,j,1:nx3) = dble(te_in(1:nx1,j,1:nx3))
       s_fld( 1:nx1,j,1:nx3) = dble(en_in(1:nx1,j,1:nx3))

       v_fld(1,1:nx1,j,1:nx3)  = dble(v1(1:nx1,j,1:nx3))
       v_fld(2,1:nx1,j,1:nx3)  = dble(v2(1:nx1,j,1:nx3))
       v_fld(3,1:nx1,j,1:nx3)  = dble(v3(1:nx1,j,1:nx3))

       t_fld(1:nx1,j,1:nx3)        = r_mev *t_fld(1:nx1,j,1:nx3)
       v_fld(1:ndim,1:nx1,j,1:nx3) =   v_c *v_fld(1:ndim,1:nx1,j,1:nx3)

    else if (mode_run == 3) then

       !..majin


       en_in(1:nx1,1:nx2,1) = 10.0
       ye_in(1:nx1,1:nx2,1) = 0.5

       read(50) ti_in, x1(1:nx1), x2(1:nx2), vol_in(1:nx1,1:nx2,1), &
            & v1(1:nx1,1:nx2,1), v2(1:nx1,1:nx2,1), &
            & de_in(1:nx1,1:nx2,1), pr_in(1:nx1,1:nx2,1), &
            & te_in(1:nx1,1:nx2,1), &
            & ( f_in(j,1:nx1,1:nx2,1), j = 1, 3 )

       ti = dble(ti_in)
       
       k = 1
       d_fld( 1:nx1, 1:nx2, k) = dble(de_in(1:nx1,1:nx2,k))
       ye_fld(1:nx1, 1:nx2, k) = dble(ye_in(1:nx1,1:nx2,k))
       t_fld( 1:nx1, 1:nx2, k) = dble(te_in(1:nx1,1:nx2,k))
       s_fld( 1:nx1, 1:nx2, k) = dble(en_in(1:nx1,1:nx2,k))
       pr_fld(1:nx1, 1:nx2, k) = dble(pr_in(1:nx1,1:nx2,k))
       f_fld(1:3, 1:nx1, 1:nx2, k) = dble(f_in(1:3,1:nx1,1:nx2,k))

       v_fld(1, 1:nx1, 1:nx2, k)  = dble(v1(1:nx1,1:nx2,k))
       v_fld(2, 1:nx1, 1:nx2, k)  = dble(v2(1:nx1,1:nx2,k))
       v_fld(3, 1:nx1, 1:nx2, k)  = 0.d0

    else
       write(*,*) 'ERROR: bad mode_run =', mode_run
       stop
    end if

    return

  end subroutine rd_fld


end module mod_fld
