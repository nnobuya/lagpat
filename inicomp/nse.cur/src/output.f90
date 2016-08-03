subroutine output( no, x )

  use mod_const, only: nel, nel_ou
  use mod_set  , only: nz, nn, na, nz_ou, nn_ou, na_ou

  implicit none

  !..io
  integer         , intent(in):: no
  double precision, intent(in):: x(1:nel)

  double precision:: tot, x_ou(1:nel)
  integer:: i, j
  character:: fl*100


  write(fl,'("./abund/abund_", i7.7, ".dat")') no
  open(60, file = fl, action = 'write')

  x_ou(1:nel_ou) = 0.d0

  do j = 1, nel_ou
     lp_chk: do i = 1, nel
        if ( nn(i) == nn_ou(j) .and. nz(i) == nz_ou(j) ) then
           x_ou(j) = x(i)
           exit lp_chk
        end if
     end do lp_chk
  end do


  tot = sum( x_ou(1:nel_ou) )
  x_ou(1:nel_ou) = x_ou(1:nel_ou) /tot


  write(60,'("#   A    Z    N     X")')
  do i = 1, nel_ou
     write(60,'(3i5,1p10e16.8)') na_ou(i), nz_ou(i), nn_ou(i), x_ou(i)
  end do


!  write(61,'("#   A    Z    N     X")')
!  do i = 1, nel
!     write(61,'(3i5,1p10e16.8)') na(i), nz(i), nn(i), x(i)
!  end do

  close(60)
!  close(61)


  return

end subroutine output
