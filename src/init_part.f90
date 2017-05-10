subroutine init_part(d_fld, dvol, id, dma, x_pt)

  use mod_cnst, only: pi, rm_sol
  use mod_set , only: r_in, r_out, nx1, nx2, nx3, x_fld,npt, &
       & ndim, npt_rad, npt_the

  implicit none

  !..io
  real(8), intent(in) :: d_fld(1:nx1,1:nx2,1:nx3), dvol(1:nx1,1:nx2,1:nx3)
  integer, intent(out):: id(1:ndim,1:npt)
  real(8), intent(out):: dma(1:npt), x_pt(1:ndim,1:npt)

  !..local
  real(8):: &
       & dma_in(npt_rad,npt_the), rad_pt(npt_rad,npt_the), &
       & the_pt(npt_rad, npt_the),&
       & vol_pt(npt_rad,npt_the), d_pt(npt_rad,npt_the), &
       & cell_rd(npt_rad,npt_the,1:4), cell_the(npt_rad,npt_the,1:4)
  real(8):: vol_dr(1:npt_rad)
  real(8):: rad_pt0(0:npt_rad), rma_pt0(0:npt_rad), &
       & the_in, the_out, rma_in, rma_ou, dr, dthe, total_mass, &
       & rad_in(1:nx1), dens(1:nx1), rma(1:nx1), &
       & x, y, h
  integer:: ipt(1:ndim), i1(1:2), i2(1:2), i3(1:2)
  real(8):: x_cell(1:4), y_cell(1:4), total_mass_grid, &
       & vel_pt(1:ndim), fac(1:ndim), xpt(1:ndim), fld_in(1:2,1:2,1:2)

  !..temporaly
  integer :: i, j, k, l



  ! ------------------------------------------------------------------ !
  !     set the initial position and cell of particle                  !
  ! ------------------------------------------------------------------ !

  !..Radial
  if (r_out == r_in) then
     rad_pt(1:npt_rad,1:npt_the) = r_out
  else if (r_out > r_in) then

     dr = (r_out - r_in) /dble(npt_rad)
     do j = 1, npt_the
        rad_pt(1,j) = r_in + 0.5 *dr
        do i = 2, npt_rad
           rad_pt(i,j) = rad_pt(i-1,j) + dr
        end do
     end do
  else
     write(*,*) 'Rin > Rout'
     stop 'error'
  end if


  !..Theta
  the_in  = x_fld(2,  1,  1,  1)
  the_out = x_fld(2,nx1,nx2,nx3)
  if (the_in == the_out) then
     the_pt(1:npt_rad,1:npt_the) = the_in
  else if (the_in < the_out) then
     dthe = ( the_out - the_in ) /dble(npt_the)
     do i = 1, npt_rad
        the_pt(i,1) = the_in + 0.5 *dthe
        do j = 2, npt_the
           the_pt(i,j) = the_pt(i,j-1) + dthe
        end do
     end do
  else
     write(*,*) 'Rin > Rout'
     stop 'error'
  end if


  !..cell
  cell_rd(1:npt_rad,1:npt_the,1) = rad_pt(1:npt_rad,1:npt_the) - 0.5d0 *dr
  cell_rd(1:npt_rad,1:npt_the,2) = rad_pt(1:npt_rad,1:npt_the) - 0.5d0 *dr
  cell_rd(1:npt_rad,1:npt_the,3) = rad_pt(1:npt_rad,1:npt_the) + 0.5d0 *dr
  cell_rd(1:npt_rad,1:npt_the,4) = rad_pt(1:npt_rad,1:npt_the) + 0.5d0 *dr

  cell_the(1:npt_rad,1:npt_the,1) = the_pt(1:npt_rad,1:npt_the) - 0.5d0 *dthe
  cell_the(1:npt_rad,1:npt_the,2) = the_pt(1:npt_rad,1:npt_the) + 0.5d0 *dthe
  cell_the(1:npt_rad,1:npt_the,3) = the_pt(1:npt_rad,1:npt_the) - 0.5d0 *dthe
  cell_the(1:npt_rad,1:npt_the,4) = the_pt(1:npt_rad,1:npt_the) + 0.5d0 *dthe

  !..volume

  do i = 1, npt_rad
     vol_dr(i) = 4.0/3.0 *pi *(cell_rd(i,1,4)**3 - cell_rd(i,1,1)**3)
  end do



  do i = 1, npt_rad
     do j = 1, npt_the
        vol_pt(i,j) = vol_dr(i)/dble(npt_the)
     end do
  end do

  !print *, sum(vol_pt)/(4.0/3.0 *pi *(r_out**3 - r_in**3)),sum(vol_dr)/(4.0/3.0 *pi *(r_out**3 - r_in**3))

  !!check
  write(70,*) '# initial position and cell'
  do j = 1, npt_the, 10
     do i = 1, npt_rad, 10
        x = rad_pt(i,j) *sin(the_pt(i,j))
        y = rad_pt(i,j) *cos(the_pt(i,j))
        x_cell(1:4) = cell_rd(i,j,1:4) *sin(cell_the(i,j,1:4))
        y_cell(1:4) = cell_rd(i,j,1:4) *cos(cell_the(i,j,1:4))
        write(70,'(1p, *(e14.5))') x, y, (x_cell(k), y_cell(k), k = 1, 4)
     end do
  end do

  ! ------------------------------------------------------------------ !



  ! ------------------------------------------------------------------ !
  !     set particle's mass                                            !
  ! ------------------------------------------------------------------ !

  vel_pt(1:ndim) = 0.d0
  do j = 1, npt_the
     do i = 1, npt_rad
        xpt(1) = rad_pt(i,j)
        xpt(2) = the_pt(i,j)
        xpt(3) = 0.d0
        call search(1, xpt(1:ndim), vel_pt(1:ndim), fac(1:ndim), ipt(1:ndim))

        do k = 1, 2
           l = k - 1
           i1(k) = max( min( ipt(1) + l, nx1 ), 1 )
           i2(k) = max( min( ipt(2) + l, nx2 ), 1 )
           i3(k) = max( min( ipt(3) + l, nx3 ), 1 )
        end do

        fld_in(1:2,1:2,1:2) &
             & = d_fld(i1(1:2), i2(1:2), i3(1:2))
        call hokan(ndim, fld_in, fac(1:ndim), d_pt(i,j))

        !write(100,'(1p, *(e14.5))') d_pt(i,j), d_fld(ipt(1),ipt(2),ipt(3))

     end do
  end do


  !..dma
  dma_in(1:npt_rad,1:npt_the) = &
       & d_pt(1:npt_rad,1:npt_the) *vol_pt(1:npt_rad,1:npt_the)/rm_sol
  total_mass = sum(dma_in)

  !                                                                    !
  ! ------------------------------------------------------------------ !



  ! ------------------------------------------------------------------ !
  !     check                                                          !
  ! ------------------------------------------------------------------ !

  write(70,*) '# initial position'
  do j = 1, npt_the
     do i = 1, npt_rad
        x = rad_pt(i,j) *sin(the_pt(i,j))
        y = rad_pt(i,j) *cos(the_pt(i,j))
        write(70,'(1p2e14.5)') x, y
     end do
  end do
  close(70)


  !..total mass by grid
  total_mass_grid = 0.d0
  do j = 1, nx2
     do i = 1, nx1
        if (r_in <= x_fld(1,i,j,1) .and. x_fld(1,i,j,1) <= r_out) then
           total_mass_grid = total_mass_grid + d_fld(i,j,1) *dvol(i,j,1)
        end if
     end do
  end do
  total_mass_grid = total_mass_grid /rm_sol


  write(*,'("Total mass (particle)  :",1p, e14.5)') total_mass
  write(*,'("Total mass (grid-based):",1pe14.5)')   total_mass_grid
  write(*,'(10x, "ratio:",f10.5)') total_mass /total_mass_grid


  !..check output
  !write(41,'(a15,1p2e14.5)') 'range:  cm ',  r_in, r_out
  !write(41,'(a15,1p2e14.5)') 'range: Msol', rma_in, rma_ou
  !write(41,*) '   #     Mr            rad           Density'
  !do i = 1, nx1
  !   write(41,'(i5,1p3e14.5)') i, rma(i), rad_in(i), dens(i)
  !end do


  !                                                                    !
  ! ------------------------------------------------------------------ !



  ! ------------------------------------------------------------------ !
  !     set particle variable                                          !
  ! ------------------------------------------------------------------ !

  k = 0
  do j = 1, npt_the
     do i = 1, npt_rad
        k = k + 1

        id(1,k) = i
        id(2,k) = j
        id(3,k) = 1

        dma(k) = dma_in(i,j)

        x_pt(1,k) = rad_pt(i,j)
        x_pt(2,k) = the_pt(i,j)
        x_pt(3,k) = 0.d0
     end do
  end do

  if ( k /= npt ) stop 'ini_part: error'

  if (.false.) then
     !..write
     write(60,'(a26)') '#      npt, nx1, nx2, nx3,'
     write(60,'(i10,3i5)') npt, npt_rad, npt_the, 1
     write(60,*)
     write(60,*)

     write(60,'(a,6x,a3,3x,a2,3x,a2,3x,a2,4x,a4,12x,a6,10x,a5,11x,a3)') &
     & '#', 'npt', 'x1', 'x2', 'x3', 'mass', 'radius', 'theta', 'phi'

     l = 0
     do k = 1, 1
        do j = 1, npt_the
           do i = 1, npt_rad
              l = l + 1
              write(60,'(i10, 3i5, 1p, *(e16.8))') &
              & l, i, j, k, dma_in(i,j), x_pt(1:ndim,l)
           end do
        end do
     end do
  end if
  !                                                                    !
  ! ------------------------------------------------------------------ !


  return

end subroutine init_part
