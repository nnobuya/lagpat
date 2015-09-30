subroutine init_part( d_fld, id, dma, x_pt )

  use mod_cnst, only: npt, ndim, npt_rad, npt_the, pi, rm_sol
  use mod_set , only: r_in, r_out, nx1, nx2, nx3, x_fld

  implicit none

  !..io
  double precision, intent(in) :: d_fld(1:nx1,1:nx2,1:nx3)
  integer, intent(out)         :: id(1:ndim,1:npt)
  double precision, intent(out):: dma(1:npt), x_pt(1:ndim,1:npt)

  !..local
  double precision:: &
       & dma_in(npt_rad,npt_the), rad_pt(npt_rad,npt_the), &
       & the_pt(npt_rad, npt_the)
  double precision:: rad_pt0(0:npt_rad), rma_pt0(0:npt_rad), &
       & the_in, the_out, rma_in, rma_ou, dr, dthe, total_mass, &
       & rad_in(1:nx1), dens(1:nx1), rma(1:nx1), &
       & x, y, h

  !..temporaly
  integer :: i, j, k, l


  rad_in(1:nx1) = x_fld( 1, 1:nx1, max(1,nx2/2), max(1,nx3/2) )
  dens(1:nx1)   = d_fld(    1:nx1, max(1,nx2/2), max(1,nx3/2) )


  !..mass coordinate
  rma(1) = dens(1) *4.d0 /3.d0 *pi *rad_in(1)**3
  do i = 2, nx1
     dr = rad_in(i) - rad_in(i-1)
     rma(i) = rma(i-1) + 4.d0 *pi *rad_in(i)**2 *dens(i) *dr
  end do

  rma(1:nx1) = rma(1:nx1) /rm_sol

  !..set initial particle position
  !!rad
  if ( r_out == r_in ) then
     rad_pt(1:npt_rad,1:npt_the) = r_out
  else
     dr = ( r_out - r_in ) /dble(npt_rad)
     do j = 1, npt_the
        rad_pt(1,j) = r_in + 0.5 *dr
        do i = 2, npt_rad
           rad_pt(i,j) = rad_pt(i-1,j) + dr
        end do
     end do
  end if

  rad_pt0(0)       = r_in
  rad_pt0(npt_rad) = r_out
  do i = 1, npt_rad - 1
     rad_pt0(i) = rad_pt0(i-1) + dr
  end do


  !!theta
  the_in  = x_fld(2,  1,  1,  1)
  the_out = x_fld(2,nx1,nx2,nx3)

  if ( the_in == the_out) then
     the_pt(1:npt_rad,1:npt_the) = the_in
  else
     dthe = ( the_out - the_in ) /dble(npt_the)
     do i = 1, npt_rad
        the_pt(i,1) = the_in + 0.5 *dthe
        do j = 2, npt_the
           the_pt(i,j) = the_pt(i,j-1) + dthe
        end do
     end do
  end if


  !!check
  write(42,*) '# initial position'
  do j = 1, npt_the
     do i = 1, npt_rad
        x = rad_pt(i,j) *sin(the_pt(i,j))
        y = rad_pt(i,j) *cos(the_pt(i,j))
        write(42,'(1p2e14.5)') x, y
     end do
  end do


  !..set particle mass
  rad_pt0_lp: do i = 0, npt_rad
     if      ( rad_pt0(i) <     rad_in(1) ) then
!        stop 'r_in is too small'
     else if ( rad_in(nx1)  <= rad_pt0(i) ) then
        h = ( rma(nx1) - rma(nx1-1) ) /( rad_in(nx1) - rad_in(nx1-1) )
        rma_pt0(i) = rma(nx1) + h *( rad_pt0(i) - rad_in(nx1) )
     else
        do j = 1, nx1-1
           if ( rad_in(j) <= rad_pt0(i) .and. rad_pt0(i) < rad_in(j+1) ) then
              h = ( rma(j+1) - rma(j) ) /( rad_in(j+1) - rad_in(j) )
              rma_pt0(i) = rma(j) + h *( rad_pt0(i) - rad_in(j) )
           end if
        end do
     end if
  end do rad_pt0_lp

  rma_in = rma_pt0(0)
  rma_ou = rma_pt0(npt_rad)


  do i = 1, npt_rad
     dma_in(i,1) = ( rma_pt0(i) - rma_pt0(i-1) ) /dble(npt_the)
  end do

  do j = 2, npt_the
     dma_in(1:npt_rad,j) = dma_in(1:npt_rad,1)
  end do


  rma_in_lp: do i = 1, nx1 - 1
     if ( rad_in(i) <= r_in .and. r_in < rad_in(i+1) ) then
        h = ( rma(i+1) - rma(i) ) /( rad_in(i+1) - rad_in(i) )
        rma_in = rma(i) + h *( r_in - rad_in(i) )
        exit rma_in_lp
     end if
  end do rma_in_lp



  total_mass = sum( dma_in(1:npt_rad,1:npt_the) )


  !..massage
  write(*,'(a20,1pe14.5)') 'total mass part:', total_mass
  write(*,'(a20,1pe14.5)') 'total mass grid:', rma_ou - rma_in
  write(*,'(a20,f10.5)') 'ratio :', total_mass /( rma_ou - rma_in )

  !..check output
  write(41,'(a15,1p2e14.5)') 'range:  cm ',  r_in, r_out
  write(41,'(a15,1p2e14.5)') 'range: Msol', rma_in, rma_ou
  write(41,*) '   #     Mr            rad           Density'
  do i = 1, nx1
     write(41,'(i5,1p3e14.5)') i, rma(i), rad_in(i), dens(i)
  end do


  !..set
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

  !..write
  write(60,'(a26)') '#      npt, nx1, nx2, nx3,'
  write(60,'(i10,3i5)') npt, npt_rad, npt_the, 1
  write(60,*)
  write(60,*)

  write(60,'(a,6x,a3,3x,a2,3x,a2,3x,a2,4x,a4,12x,a6,10x,a5,11x,a3)') &
       & '#', 'npt', 'x1', 'x2', 'x3', 'mass', 'radius', 'theta', 'phi'

  k = 0

  do k = 1, 1
     do j = 1, npt_the
        do i = 1, npt_rad
           l = l + 1
           write(60,'(i10,3i5,1p4e16.8)') l, i, j, k, dma_in(i,j), x_pt(1:ndim,l)
        end do
     end do
  end do



  return

end subroutine init_part
