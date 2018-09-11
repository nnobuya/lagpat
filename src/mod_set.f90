module mod_set

  implicit none

  private
  public:: k_zoku, i_test, last_lp, int_t, int_x, nin, nou, &
       & nout_lpt, n_anim, n_init, n_fini, &
       & r_in, r_out, bound_in, bound_out, set_param, nx1, nx2, nx3, &
       & d_fld, t_fld, s_fld, ye_fld, pr_fld, x_fld, dx_fld, &
       & f_fld, v_fld, v0_fld, &
       & set_data, path, mode_run, npt, ndim, npt_rad, npt_the

  integer:: n_init, n_fini, mode_run
  integer:: k_zoku, i_test, last_lp, int_t, int_x, nin, nou
  integer:: nout_lpt, n_anim
  double precision:: r_in, r_out, bound_in, bound_out
  character:: path*100

  !..main
  integer:: nx1, nx2, nx3
  integer:: npt, npt_rad, npt_the, ndim

  !..Grid & field data (hydro results)

  double precision, dimension(:,:,:), allocatable:: &
       & d_fld, t_fld, s_fld, ye_fld, pr_fld
  double precision, allocatable:: dx_fld(:,:)
  double precision, dimension(:,:,:,:), allocatable:: &
       & x_fld, f_fld, v_fld, v0_fld
  real, allocatable:: x1(:), x2(:), x3(:)
  real, dimension(:,:), allocatable:: de_in, ye_in, te_in, ut_in, qb_in, &
       & en_in, v1, v2, v3

contains


  subroutine set_param(io)

    implicit none

    integer, intent(in):: io


    !..calculation Parametar form './in.dat'
    read(io,*)
    read(io,*) mode_run
    read(io,*)
    read(io,*)
    read(io,*) path
    read(io,*)
    read(io,*)
    read(io,*) nx1, nx2, nx3
    read(io,*)
    read(io,*)
    read(io,*) n_init, n_fini
    read(io,*)
    read(io,*)
    read(io,*) k_zoku, i_test, last_lp, int_t, int_x, nin, nou
    read(io,*)
    read(io,*)
    read(io,*) r_in, r_out, bound_in, bound_out
    read(io,*)
    read(io,*)
    read(io,*) nout_lpt, n_anim
    read(io,*)
    read(io,*)
    read(io,*) npt_rad, npt_the, ndim

    npt = npt_rad *npt_the

    !..check
    if(     r_in > r_out     ) stop 'error: bad data #1 set_param @mod_set'
    if( bound_in > bound_out ) stop 'error: bad data #2 set_param @mod_set'

    !..message
    write(*,'(a20,i10)') 'npt = :', npt
    write(*,'(a20,1pe10.2,a3,e10.2)') &
         & 'set particle range :', r_in, '->', r_out
    write(*,'(a20,1pe10.2,a3,e10.2)') &
         & 'partilce move area :', bound_in, '->', bound_out
    write(*,*)


    return

  end subroutine set_param



  subroutine set_data

    use mod_cnst, only: pi

    implicit none

    double precision:: dummy(1:14)
    integer:: nx_max, i, j, k, ier, i1, i2, i3
    real:: ti_in

    !..grid for hydro result
    !..set grid
    !nx1 = 576
    !nx2 = 128
    !nx3 = 1

    nx_max = max(max( nx1, nx2 ), nx3)

    allocate(x1(1:nx1), x2(1:nx2), x3(1:nx3), &
         & de_in(1:nx1,1:nx3), ye_in(1:nx1,1:nx3), &
         & te_in(1:nx1,1:nx3), ut_in(1:nx1,1:nx3), qb_in(1:nx1,1:nx3), &
         & en_in(1:nx1,1:nx3), &
         & v1(1:nx1,1:nx3) , v2(1:nx1,1:nx3), v3(1:nx1,1:nx3))

    allocate(x_fld(ndim,nx1,nx2,nx3), dx_fld(ndim,nx_max), &
         & d_fld(nx1,nx2,nx3), t_fld(nx1,nx2,nx3), &
         & s_fld(nx1,nx2,nx3), ye_fld(nx1,nx2,nx3), &
         & pr_fld(nx1,nx2,nx3), f_fld(1:3,nx1,nx2,nx3), &
         & v_fld(ndim,nx1,nx2,nx3), v0_fld(ndim,nx1,nx2,nx3), &
         & stat = ier)

    if(ier /= 0) stop '### main: error  #3 ###'


    x_fld(1:3,1:nx1,1:nx2,1:nx3) = 0.d0
    if (mode_run == 1) then
       !..Sawai format
       !..ti, lnue, lnue_, enue, enue_
       read(50)
       do k = 1, nx3
          do j = 1, nx2
             do i = 1, nx1
                read(50) i1, i2, x_fld(1,i,j,k), x_fld(2,i,j,k), dummy(1:14), i3
             end do
          end do
       end do
    else if (mode_run == 2) then
       !..fujib format
       read(50) ti_in, x1(1:nx1), x3(1:nx3), &
            & de_in(1:nx1,1:nx3), ye_in(1:nx1,1:nx3), te_in(1:nx1,1:nx3), &
            & ut_in(1:nx1,1:nx3), qb_in(1:nx1,1:nx3), en_in(1:nx1,1:nx3), &
            & v1(1:nx1,1:nx3) , v2(1:nx1,1:nx3), v3(1:nx1,1:nx3)
       do k = 1, nx3
          do j = 1, nx2
             do i = 1, nx1
                x_fld(1,i,j,k) = dble(x1(i))
                x_fld(2,i,j,k) = 0.d0
                x_fld(3,i,j,k) = dble(x3(k))
             end do
          end do
       end do
    else if (mode_run == 3) then
       !..majin format
       read(50) ti_in, x1(1:nx1), x2(1:nx2)

       k = 1
       do j = 1, nx2
          do i = 1, nx1
             x_fld(1,i,j,k) = dble(x1(i))
             x_fld(2,i,j,k) = dble(x2(j))
          end do
       end do
       x_fld(3,1:nx1,1:nx2,1:nx3) = 0.d0
    else
       write(*,*) 'ERROR: invalid mode_run =', mode_run
       stop
    end if

    rewind(50)

    dx_fld(1,1) = x_fld(1,1,1,1)
    dx_fld(2,1) = x_fld(2,1,1,1)
    dx_fld(3,1) = x_fld(3,1,1,1)


    if( nx1 >= 2 ) then
       dx_fld(1,2:nx1) = x_fld(1,2:nx1,1,1) - x_fld(1,1:nx1-1,1,1)
    else
       dx_fld(1,1:nx1) = 0.d0
    end if

    if( nx2 >= 2 ) then
       dx_fld(2,2:nx2) = x_fld(2,1,2:nx2,1) - x_fld(2,1,1:nx2-1,1)
    else
       dx_fld(2,1:nx2) = 0.d0
    end if

    if( nx3 >= 2 ) then
       dx_fld(3,2:nx3) = x_fld(3,1,2:nx2,1) - x_fld(3,1,1:nx2-1,1)
    else
       dx_fld(3,1:nx3) = 0.d0
    end if

    write(42,'("#", 3a10)') 'nx1,', 'nx2,', 'nx3,'
    write(42,'(3i10)') nx1, nx2, nx3
    write(42,'(*(es18.10))') x_fld(1,1:nx1,1,1)
    write(42,'(*(es18.10))') x_fld(2,1,1:nx2,1)
    write(42,'(*(es18.10))') x_fld(3,1,1,1:nx3)

    close(42)

    !..message
    write(*,'(" --------------------- grid information", &
         & " ----------------------")')

    if      (mode_run == 1) then
       write(*,'("   Spherical Coordinate")')
    else if (mode_run == 2) then
       write(*,'("   (log) Cartesian Coordinates")')
    end if

    write(*,'(a20,i5,2(a2,i5))') &
         & 'x1 x x2 x n3 :', nx1, ' x', nx2, ' x', nx3

    write(*,'(a20,1pe10.2,a3,e10.2)') &
         & 'x1-range :'    , x_fld(1,1,1,1), '->',  x_fld(1,nx1,nx2,nx3)
    write(*,'(a20,1pe10.2,a3,e10.2)') &
         & 'x2-range :'    , x_fld(2,1,1,1), '->',  x_fld(2,nx1,nx2,nx3)
    if (mode_run == 1) then
       write(*,'(19x,a1,f10.2,a3,f10.2,1x,a1)') &
            & '(', 180.0 /pi *x_fld(2,1,1,1), &
            & '->', 180.0/pi *x_fld(2,nx1,nx2,nx3), ')'
    end if
    write(*,'(a20,1pe10.2,a3,e10.2)') &
         & 'x3-range :'    , x_fld(3,1,1,1), '->',  x_fld(3,nx1,nx2,nx3)
    if (mode_run == 1) then
       write(*,'(19x,a1,f10.2,a3,f10.2,1x,a1)') &
            & '(', 180.0 /pi *x_fld(3,1,1,1), &
            & '->', 180.0/pi *x_fld(3,nx1,nx2,nx3), ')'
    end if
    write(*,'(" --------------------------------", &
         & "-----------------------------")')
    write(*,*)


    return

  end subroutine set_data



end module mod_set
