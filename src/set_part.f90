subroutine set_part(istage, time, istat_pt, id, x_pt, v_pt, dma)

  use mod_set , only: k_zoku, nx1, nx2, nx3, mode_run, npt, ndim

  implicit none

  !..io
  integer, intent(out):: istage, id(1:ndim,1:npt), istat_pt(npt)
  real(8), intent(out):: time, x_pt(1:ndim,1:npt), v_pt(1:ndim,1:npt), dma(1:npt)

  !..local
  real   :: ti_in, dvol_in(1:nx1,1:nx2,1:nx3)
  double precision:: total_mass
  double precision:: dummy(1:14), &
       & d_fld(1:nx1,1:nx2,1:nx3), dvol(1:nx1,1:nx2,1:nx3)

  real, allocatable:: x1(:), x2(:), x3(:)
  real, dimension(:,:), allocatable:: de_in, ye_in, te_in, ut_in, &
       & qb_in, en_in, v1, v2, v3

  double precision:: dr(1:nx1), dr_vol(1:nx1),pi
  integer:: i, j, k, i_tmp, j_tmp, ier, i1, i2, i3


  !..initial position
  if (k_zoku == 0) then

     !! for first calculation

     if (mode_run == 1) then
        read(50, iostat = ier) time, dummy(1:4)
        if (ier /= 0) stop 'fld_set(): bad in data'

        do k = 1, nx3
           do j = 1, nx2
              do i = 1, nx1
                 read(50) i1, i2, dummy(1:2), d_fld(i,j,k), dvol(i,j,k), &
                      & dummy(3:14), i3
              end do
           end do
        end do

     else if (mode_run == 2) then

        allocate(x1(1:nx1), x3(1:nx3), &
             & de_in(1:nx1,1:nx3), ye_in(1:nx1,1:nx3), &
             & te_in(1:nx1,1:nx3), ut_in(1:nx1,1:nx3), qb_in(1:nx1,1:nx3), &
             & en_in(1:nx1,1:nx3), v1(1:nx1,1:nx3) , &
             & v2(1:nx1,1:nx3), v3(1:nx1,1:nx3))

        read(50) ti_in, x1(1:nx1), x3(1:nx3), &
             & de_in(1:nx1,1:nx3), ye_in(1:nx1,1:nx3), te_in(1:nx1,1:nx3), &
             & ut_in(1:nx1,1:nx3), qb_in(1:nx1,1:nx3), en_in(1:nx1,1:nx3), &
             & v1(1:nx1,1:nx3) , v2(1:nx1,1:nx3), v3(1:nx1,1:nx3)

        time = 1.d-3 *dble(ti_in)

        d_fld(1:nx1,1,1:nx3) = qb_in(1:nx1,1:nx3)

        deallocate(x1, x3, de_in, ye_in, te_in, ut_in, qb_in, en_in, v1, v2, v3)
     else if (mode_run == 3) then
        !..majin

        allocate(x1(1:nx1), x2(1:nx2), &
             & de_in(1:nx1,1:nx2), ye_in(1:nx1,1:nx2), &
             & te_in(1:nx1,1:nx2), &
             & en_in(1:nx1,1:nx2), v1(1:nx1,1:nx2) , &
             & v2(1:nx1,1:nx2), v3(1:nx1,1:nx2))

        read(50) ti_in, x1(1:nx1), x2(1:nx2), dvol_in(1:nx1,1:nx2,1:nx3),&
             & v1(1:nx1,1:nx2), v2(1:nx1,1:nx2), &
             & de_in(1:nx1,1:nx2), te_in(1:nx1,1:nx2)

        d_fld(1:nx1,1:nx2,1) = dble(de_in(1:nx1,1:nx2))

        pi = 4.d0 *atan(1.d0)

        dr(1) = x1(1) - 1.d8
        do i = 2, nx1
           dr(i) = x1(i) - x1(i-1)
        end do

        do i = 1, nx1
           dr_vol(i) = 4.d0 *pi * x1(i)**2 *dr(i)
        end do

        do k = 1, nx3
           do j = 1, nx2
              do i = 1, nx1
                 dvol(i,j,k) = dr_vol(i) /dble(nx2)
                 write(200,'(*(es14.5))') &
                      & x1(i) *sin(x2(j)), x1(i) *cos(x2(j)), &
                      & dvol(i,j,k), dvol_in(i,j,k)*1.e30
              end do
           end do
        end do

        !write(*,'(*(es14.5))') 4.d0 /3.d0 *pi *(x1(nx1)**3 - x1(1)**3), &
        !     & sum(dvol), sum(dvol_in*1.e30)

        !print *, sum(dr_vol(1:nx1)), sum(dvol), 4.d0 /3.d0 *pi *x1(nx1)**3

        deallocate(x1, x2, de_in, ye_in, te_in, en_in, v1, v2, v3)
     else
        write(*,*) 'ERROR: undefined mode_run =', mode_run
        stop
     end if

     rewind(50)

     !! initial stage
     istage = 0
     istat_pt(1:npt) = 0

     if (mode_run == 1 .or. mode_run == 3) then
        call init_part(d_fld(:,:,:), dvol(:,:,:), id(:,:), dma(:), x_pt(:,:))
        !   in: d_fld, dvol
        !  out: dma, rad_pt, the_pt
     else if (mode_run == 2) then
        call init_part2(d_fld(:,:,:), id(:,:), dma(:), x_pt(:,:))
     end if

  else if( k_zoku == 1 ) then
     !! continue

     read(91,*)
     read(91,*) time, istage
     do i = 1, npt
        read(91,*) &
             & i_tmp, j_tmp, &
             & dma(i), x_pt(1:2,i), v_pt(i,1:2), istat_pt(i)
     end do

  else
     !! error
     stop '### Error: "k_zoku" isn''t 0 or 1.  ###'
  end if


  return

end subroutine set_part
