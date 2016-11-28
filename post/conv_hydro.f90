program conv

  implicit none

  !..param.
  integer, parameter:: nrd = 576, nth = 128
  integer, parameter:: nx  = 150
  real(8), parameter:: radius = 1.d8

  !..main
  real(8), external:: f_intpol
  real(8):: rd(1:nrd), th(1:nth), r_shock(1:nx), r_old
  real(8), dimension(1:nrd,1:nth):: x, y, s, ye, bet, de
  real(8), dimension(1:nx,1:nx):: x1, y1, rd1, th1, s1, ye1, bet1, de1

  real(8):: dx, t1, t2, h
  real(8):: r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, &
       & r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25 
  integer:: i, j, i1, i2, ii, jj


  ! ------------------------------------------------------------------ !
  !     open                                                           !
  ! ------------------------------------------------------------------ !

  open(50, file = './in.dat', action = 'read')

  open(60, file = './res/hydro_org.dat', action = 'write')
  open(61, file = './res/hydro_new.dat', action = 'write')
  open(62, file = './res/shock.dat'    , action = 'write')

  !                                                                    !
  ! ------------------------------------------------------------------ !



  ! ------------------------------------------------------------------ !
  !     input data                                                     !
  ! ------------------------------------------------------------------ !

  do j = 1, nth
     do i = 1, nrd
        read(50,*) i1, i2, rd(i), th(j), &
             & r5, r6, r7, r8, r9, r10, s(i,j), ye(i,j), r13, r14, r15, &
             & r16, r17, r18, r19, r20, r21, r22, r23, r24, r25
        x(i,j) = rd(i) *sin(th(j))
        y(i,j) = rd(i) *cos(th(j))
        de(i,j)  = r5
        bet(i,j) = log10(r10 /(r20 + r21 + r24))
     end do
     read(50,*)
  end do

  close(50)


  !..out
  do j = 1, nth
     do i = 1, nrd
        write(60,'(1p, *(e14.5))') &
             & 1.d-5 *x(i,j), 1.d-5 *y(i,j), s(i,j), ye(i,j), bet(i,j), de(i,j)
     end do
     write(60,*)
  end do

  close(60)

  !     read data                                                      !
  ! ------------------------------------------------------------------ !



  ! ------------------------------------------------------------------ !
  !     convert on new grids                                           !
  ! ------------------------------------------------------------------ !

  !..grid setting
  dx = radius /dble(nx-1)
  do j = 1, nx
     do i = 1, nx
        x1(i,j) = dble(i-1) *dx
        y1(i,j) = dble(j-1) *dx
     end do
  end do

  !..grid (x,y) --> (r,theta)
  rd1(1:nx,1:nx) = sqrt(x1(1:nx,1:nx)**2 + y1(1:nx,1:nx)**2)
  do j = 1, nx
     do i = 1, nx
        if (y1(i,j) == 0.d0) then
           th1(i,j) = 0.d0
        else
           th1(i,j) = atan(x1(i,j) /y1(i,j))
        end if
     end do
  end do


  !..search
  do j = 1, nx
     do i = 1, nx
        if (rd1(i,j) <= rd(1)) then
           ii = 1
        else
           lp_search_rd: do ii = 1, nrd-1
              if ( rd(ii) <= rd1(i,j) .and. rd1(i,j) < rd(ii+1)) then
                 t1 = (rd1(i,j) - rd(ii)) /(rd(ii+1) - rd(ii))
                 exit
              end if
           end do lp_search_rd
        end if

        if (th1(i,j) <= th(1)) then
           jj = 1
        else
           lp_search_th: do jj = 1, nth-1
              if ( th(jj) <= th1(i,j) .and. th1(i,j) < th(jj+1)) then
                 t2 = (th1(i,j) - th(jj)) /(th(jj+1) - th(jj))
                 exit
              end if
           end do lp_search_th
        end if

        if (ii == 1 .or. jj == 1) then
           s1(i,j)   = s(ii,jj)
           ye1(i,j)  = ye(ii,jj)
           bet1(i,j) = bet(ii,jj)
           de1(i,j)  = de1(ii,jj)
        else

           s1(i,j)  = f_intpol(s(ii,jj), s(ii+1,jj), s(ii,jj+1), &
                & s(ii+1,jj+1), t1, t2)

           ye1(i,j) = f_intpol(ye(ii,jj), ye(ii+1,jj), ye(ii,jj+1), &
                & ye(ii+1,jj+1), t1, t2)

           bet1(i,j) = f_intpol(bet(ii,jj), bet(ii+1,jj), bet(ii,jj+1), &
                & bet(ii+1,jj+1), t1, t2)

           de1(i,j)  = f_intpol(de(ii,jj), de(ii+1,jj), de(ii,jj+1), &
                & de(ii+1,jj+1), t1, t2)

        end if

     end do
  end do


  !..shock
  do i = 1, nx
     r_shock(i) = r_old

     if (s1(i,nx) >= 5.d0) then
        r_shock(i) = y1(i,nx)
     else
        do j = nx, 1, -1
           if (s1(i,j) > 5.d0) then

              h = (y1(i,j+1) - y1(i,j)) /(s1(i,j+1) - s1(i,j))
              r_shock(i) = y1(i,j) + h *(5.d0 - s1(i,j))
              r_old      = y1(i,j)
              exit
           end if
        end do
     end if
  end do

  do i = 1, nx
     write(62,'(1p, *(e14.5))') 1.d-5 *x1(i,1), 1.d-5 *r_shock(i)
  end do


  write(61,'("#", i5)') nx
  do j = 1, nx
     do i = 1, nx
        write(61,'(1p, *(e14.5))') &
             & 1.d-5 *x1(i,j), 1.d-5 *y1(i,j), &
             & s1(i,j), ye1(i,j), bet1(i,j), de1(i,j)
     end do
     write(61,*)
  end do

  !     convert on new grids                                           !
  ! ------------------------------------------------------------------ !



  stop 'conv: normal termination'

end program conv





real(8) function f_intpol(x11, x12, x21, x22, t1, t2)

  implicit none

  !..io
  real(8), intent(in):: x11, x12, x21, x22, t1, t2

  !..local
  real(8):: x1, x2


  x1 = t1 *x11 + (1.d0 - t1) *x12
  x2 = t1 *x21 + (1.d0 - t1) *x22

  f_intpol = t2 *x1 + (1.d0 - t2) *x2

  return

end function f_intpol
