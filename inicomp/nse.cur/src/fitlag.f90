! -------------------------------------------------------------------- !
!     fitlag()                                                         !
!         function to compute iterated Lagrange interpolation          !
!         at x based on data given in a(i) and f(i)                    !
!                                                                      !
!     === input data for common block ===                              !
!         n    ...total number of data (a(i),f(i)) given in the table  !
!         xa(i)...i-th sampling point                                  !
!         ya(i)...data at i-th point                                   !
!         m    ...number of sampling points used in both sides of x    !
!                for interpolation (m<=5)                              !
!         eps  ...absolute error tolerance                             !
!                                                                      !
!     === work arrays ===                                              !
!         b(j)  ...xa(i)-x                                             !
!         v(i,j)...triangular table for interpolation                  !
! -------------------------------------------------------------------- !

real*8 function fitlag( x, n, m, xa, ya, eps, ierr, yerr )

  implicit none

  !..io data
  integer,  intent(in) :: n, m
  real*8 ,  intent(in) :: x, xa(1000), ya(1000), eps
  integer, intent(out) :: ierr
  real*8 , intent(out) :: yerr

  !..local
  integer :: i, j, k, is, im, ib, ir, il, m2
  real*8  :: v(10,10), b(10)
  real*8  :: bv, vv, vdif, vadd


  ! --- check the value of m,n ---
  if(n > 1000) then
     stop 'fitlag.f: too many sampling points to fitlag!'
  else if(m > 10) then
     stop 'fitlag.f: redefine the value of m to fitlag!'
  end if

  m2   = 2 *m
  ierr = 0
  yerr = 0.d0

  ! --- find the nearest sampling point to x by bisection method ---
  !******************************** ct9(is) < t9 < ct9(ib) *******
  is = 1
  ib = n

  do
     if(is >= ib-1) exit
     im = (is+ib) /2

     if(x < xa(im)) then
        ib = im
     else if(x > xa(im)) then
        is = im
     else
        fitlag = ya(im)
        return
     end if
  end do

  !*************************************************************
  ! --- set sampling points to use ---
  il = is - m + 1
  ir = ib + m - 1

  if(il < 1) then
     il = 1
  else if(ir > n) then
     il = n - m2 + 1
  end if

  do i = 1, m2
     b(i)   = xa(i+il-1) - x
     v(1,i) = ya(i+il-1)
  end do


  !..sort abs(b(i)) in asending order
  do i = 2, m2
     k = i - 1
     do j = i, m2
        if(abs(b(j)) < abs(b(k))) k = j
     end do

     if(k /= i-1) then
        bv       = b(k)
        b(k)     = b(i-1)
        b(i-1)   = bv

        vv       = v(1,k)
        v(1,k)   = v(1,i-1)
        v(1,i-1) = vv
     end if
  end do


  !..compute iterated interpolation
  do i = 2, m2
     do j = 2, i
        v(j,i) = (v(j-1,j-1) *b(i) - v(j-1,i) *b(j-1)) /(b(i) - b(j-1))
     end do

     !! converged
     vdif = abs(v(i,i) - v(i-1,i-1))
     vadd = abs(v(i,i) + v(i-1,i-1))
     if(vdif <= eps *vadd) then
        fitlag = v(i,i)
        return
     end if
  end do

  !! not converged
  fitlag = v(m2,m2)

  !! NN
  fitlag = ( ya(is) + ya(ib) )/2.d0


  ierr   = 1
  yerr   = vdif/vadd


  return

end function fitlag
