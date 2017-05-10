subroutine set_part(istage, time, istat_pt, id, x_pt, v_pt, dma)

  use mod_set , only: k_zoku, nx1, nx2, nx3, mode_run, npt, ndim

  implicit none

  !..io
  integer, intent(out):: istage, id(1:ndim,1:npt), istat_pt(npt)
  real(8), intent(out):: time, x_pt(1:ndim,1:npt), v_pt(1:ndim,1:npt), dma(1:npt)

  !..local
  real   :: ti_in
  real(8):: total_mass
  real(8):: d_fld(1:nx1,1:nx2,1:nx3), dvol(1:nx1,1:nx2,1:nx3), dummy(1:14)

  real, allocatable:: x1(:), x3(:)
  real, dimension(:,:), allocatable:: de_in, ye_in, te_in, ut_in, qb_in, en_in, v1, v2, v3

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

        time = dble(ti_in)

        d_fld(1:nx1,1,1:nx3) = qb_in(1:nx1,1:nx3)

        deallocate(x1, x3, de_in, ye_in, te_in, ut_in, qb_in, en_in, v1, v2, v3)

     end if

     rewind(50)

     !! initial stage
     istage = 0
     istat_pt(1:npt) = 0

     if (mode_run == 1) then
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
