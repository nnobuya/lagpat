module mod_set

  use mod_cnst, only: npt, ndim

  implicit none

  private
  public:: k_zoku, i_test, last_lp, int_t, int_x, nin, nou, &
       & nout_lpt, n_anim, n_init, n_fini, &
       & r_in, r_out, bound_in, bound_out, set_param, nx1, nx2, nx3, &
       & d_fld, t_fld, s_fld, ye_fld, x_fld, dx_fld, v_fld, v0_fld, &
       & set_data, path

  integer:: n_init, n_fini
  integer:: k_zoku, i_test, last_lp, int_t, int_x, nin, nou
  integer:: nout_lpt, n_anim
  real(8):: r_in, r_out, bound_in, bound_out
  character:: path*100

  !..main
  integer:: nx1, nx2, nx3

  !..Grid & field data (hydro results)

  real(8), allocatable:: d_fld(:,:,:), t_fld(:,:,:), s_fld(:,:,:), ye_fld(:,:,:)
  real(8), allocatable:: dx_fld(:,:)
  real(8), dimension(:,:,:,:), allocatable:: x_fld, v_fld, v0_fld

contains


  subroutine set_param(io)

    implicit none

    integer, intent(in):: io


    !..calculation Parametar form './in.dat'
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

    real(8):: dummy(1:14)
    integer:: nx_max, i, j, k, ier, i1, i2, i3


    !..grid for hydro result
    !..set grid
    !nx1 = 576
    !nx2 = 128
    !nx3 = 1

    nx_max = max(max( nx1, nx2 ), nx3)

    allocate(x_fld(ndim,nx1,nx2,nx3), dx_fld(ndim,nx_max), &
         & d_fld(nx1,nx2,nx3), t_fld(nx1,nx2,nx3), &
         & s_fld(nx1,nx2,nx3), ye_fld(nx1,nx2,nx3), &
         & v_fld(ndim,nx1,nx2,nx3), v0_fld(ndim,nx1,nx2,nx3), &
         & stat = ier)

    if(ier /= 0) stop '### main: error  #3 ###'


    !..ti, lnue, lnue_, enue, enue_
    read(50)


    x_fld(1:3,1:nx1,1:nx2,1:nx3) = 0.d0
    do k = 1, nx3
       do j = 1, nx2
          do i = 1, nx1
             read(50) i1, i2, x_fld(1,i,j,k), x_fld(2,i,j,k), dummy(1:14), i3
          end do
       end do
    end do

    rewind(50)


    !..check
    !do k = 1, nx3
    !   do j = 1, nx2
    !      do i = 1, nx1
    !         write(100,'(1p, *(e14.5))') &
    !              & x_fld(1,i,j,k) *cos(x_fld(2,i,j,k)), &
    !              & x_fld(1,i,j,k) *sin(x_fld(2,i,j,k)), &
    !              & x_fld(3,i,j,k)
    !      end do
    !   end do
    !end do
    !..end check


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


    !..message
    write(*,'(" --------------------- grid information ----------------------")')
    write(*,'("   Spherical Coordinate")')
    write(*,'(a20,i5,2(a2,i5))') &
         & 'x1 x x2 x n3 :', nx1, ' x', nx2, ' x', nx3

    write(*,'(a20,1pe10.2,a3,e10.2)') &
         & 'x1-range :'    , x_fld(1,1,1,1), '->',  x_fld(1,nx1,nx2,nx3)
    write(*,'(a20,1pe10.2,a3,e10.2)') &
         & 'x2-range :'    , x_fld(2,1,1,1), '->',  x_fld(2,nx1,nx2,nx3)
    write(*,'(19x,a1,f10.2,a3,f10.2,1x,a1)') &
         & '(', 180.0 /pi *x_fld(2,1,1,1), &
         & '->', 180.0/pi *x_fld(2,nx1,nx2,nx3), ')'
    write(*,'(a20,1pe10.2,a3,e10.2)') &
         & 'x3-range :'    , x_fld(3,1,1,1), '->',  x_fld(3,nx1,nx2,nx3)
    write(*,'(19x,a1,f10.2,a3,f10.2,1x,a1)') &
         & '(', 180.0 /pi *x_fld(3,1,1,1), &
         & '->', 180.0/pi *x_fld(3,nx1,nx2,nx3), ')'
    write(*,'(" -------------------------------------------------------------")')
    write(*,*)


    return

  end subroutine set_data



end module mod_set
