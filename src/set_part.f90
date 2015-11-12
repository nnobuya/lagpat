subroutine set_part( istage, time, istat_pt, id, x_pt, v_pt )

  use mod_cnst, only: npt, ndim
  use mod_set , only: k_zoku, dma, nx1, nx2, nx3

  implicit none

  !..io
  integer, intent(out):: istage, id(1:ndim,1:npt), istat_pt(npt)
  double precision, intent(out):: time, x_pt(1:ndim,1:npt), v_pt(1:ndim,1:npt)

  !..local
  real(8):: total_mass
  real(8):: d_fld(1:nx1,1:nx2,1:nx3), dvol(1:nx1,1:nx2,1:nx3), dummy(1:14)
  integer:: i, j, k, i_tmp, j_tmp, ier, i1, i2, i3


  !..initial position
  if ( k_zoku == 0 ) then

     !! for first calculation
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

     rewind(50)

     !! initial stage
     istage = 0
     istat_pt(1:npt) = 0

     call init_part( d_fld(:,:,:), dvol(:,:,:), id(:,:), dma(:), x_pt(:,:) )
     !   in: d_fld, dvol
     !  out: dma, rad_pt, the_pt

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
