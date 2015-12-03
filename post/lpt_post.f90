program lpt_post

  implicit none

  integer, parameter:: ndim = 3
  integer, parameter:: ndt_ex = 100
  real(8), parameter:: ti_ini = 1.d-2, ti_fin = 1.d4
  real(8), parameter:: te_nse = 9.d9

  !..main
  integer:: npt = 10000
  integer:: istg, ipt, ndt, idt
  real(8):: dt, dt_ex
  integer, allocatable:: ist_pt(:,:), ndt_pt(:)
  real(8), allocatable:: ti(:), &
       & de_pt(:,:), te_pt(:,:), en_pt(:,:), &
       & ye_pt(:,:), x_pt(:,:,:), v_pt(:,:,:)
  real(8):: ti_ex(1:ndt_ex), de_ex, te_ex, en_ex, ye_ex

  integer:: n_nse
  real(8):: ti_nse, de_nse, en_nse, ye_nse

  real(8):: h
  real(4):: dt_in, ti_in
  real(4), allocatable:: de_in(:), te_in(:), en_in(:), ye_in(:), &
       & x_in(:,:), v_in(:,:)

  character:: ofile*100
  integer:: i, ier


  !..make dt ext
  dt_ex = (log10(ti_fin) - log10(ti_ini)) /real(ndt_ex - 1)

  ti_ex(1) = log10(ti_ini)
  do i = 2, ndt_ex
     ti_ex(i) = ti_ex(i-1) + dt_ex
  end do

  ti_ex(1:ndt_ex) = 10.d0**ti_ex(1:ndt_ex)

  do i = 1, ndt_ex
     write(100,'(i5, e14.5)') i, ti_ex(i)
  end do


  !..open
  open(50, file = '../res/move.dat', &
       & form = 'unformatted', action = 'read')
  open(51, file = '../res/hydro.dat', &
       & form = 'unformatted', action = 'read')
  open(52, file = '../res/stat_lpt.dat', &
       & form = 'unformatted', action = 'read')


  open(61, file = 'peak.dat', action = 'write')


  ndt = 0
  do
     read(52,iostat=ier)
     if (ier /= 0) exit
     read(52)
     ndt = ndt + 1
  end do
  rewind(52)


  allocate(de_in(1:npt), te_in(1:npt), en_in(1:npt), ye_in(1:npt), &
       & x_in(1:ndim,1:npt), v_in(1:ndim,1:npt))

  allocate(ti(1:ndt), ist_pt(1:npt,1:ndt), ndt_pt(1:npt), &
       & de_pt(1:npt,1:ndt), te_pt(1:npt,1:ndt), &
       & en_pt(1:npt,1:ndt), ye_pt(1:npt,1:ndt), &
       & x_pt(1:ndim,1:npt,1:ndt), v_pt(1:ndim,1:npt,1:ndt))

  write(*,'("- reading tracer data")')

  do idt = 1, ndt

     read(50) istg, ti_in, dt_in
     read(50) x_in(1:ndim,1:npt), v_in(1:ndim,1:npt)

     read(51) istg, ti_in, dt_in
     read(51) de_in(1:npt), te_in(1:npt), en_in(1:npt), ye_in(1:npt)

     read(52) istg, ti_in, dt_in
     read(52) ist_pt(1:npt,idt)

     !..convert real(4) to real(8)
     ti(idt) = dble(ti_in)
     x_pt(1:ndim,1:npt,idt) = dble(x_in(1:ndim,1:npt))
     v_pt(1:ndim,1:npt,idt) = dble(v_in(1:ndim,1:npt))

     de_pt(1:npt,idt) = dble(de_in(1:npt))
     te_pt(1:npt,idt) = dble(te_in(1:npt))
     en_pt(1:npt,idt) = dble(en_in(1:npt))
     ye_pt(1:npt,idt) = dble(ye_in(1:npt))

     if (mod(istg,100) == 0) write(*,*) istg, ti(idt)

  end do
  close(51)

  deallocate(de_in, te_in, en_in, ye_in)


  ndt_pt(1:npt) = ndt
  do ipt = 1, npt
     do idt = 1, ndt
        if (ist_pt(ipt,idt) /= 0) then
           ndt_pt(ipt) = idt
           cycle
        end if
     end do
  end do


  do ipt = 1, npt
     write(61,'(1p, *(e14.5))') &
          & de_pt(ipt,1)  , te_pt(ipt,1)  , ye_pt(ipt,1), &
          & de_pt(ipt,ndt), te_pt(ipt,ndt), ye_pt(ipt,ndt),&
          & maxval(de_pt(ipt,1:ndt)), maxval(te_pt(ipt,1:ndt))
  end do
  close(61)


  write(*,'("- writing tracer data")')

  do ipt = 1, npt

     if (mod(ipt,1000) == 0) write(*,'(2i10)') ipt, npt

     !..original hydro data
     write(ofile,'("./tracer/hydro_", i5.5, ".org")') ipt
     open(60, file = ofile, action = 'write')

     do idt = 1, ndt_pt(ipt)
        write(60,'(1p, *(e15.7))') ti(idt), &
        & de_pt(ipt,idt), te_pt(ipt,idt), ye_pt(ipt,idt)
     end do
     close(60)


     if (maxval(te_pt(ipt,1:ndt)) < te_nse) then
        print *, 'check later'
        cycle
     else

        do idt = ndt_pt(ipt), 1, -1
           if (te_pt(ipt,idt) >= te_nse) then
              n_nse = idt
           end if
        end do

        h = (te_nse - te_pt(ipt,idt)) &
        & /(te_pt(ipt,n_nse + 1) - te_pt(ipt,n_nse))

        ti_nse = ti(n_nse)        + h *(ti(n_nse + 1)      - ti(n_nse))
        de_nse = de_pt(ipt,n_nse) &
             & + h *(de_pt(ipt,n_nse + 1) - de_pt(ipt,n_nse))
        ye_nse = ye_pt(ipt,n_nse) &
             & + h *(ye_pt(ipt,n_nse + 1) - ye_pt(ipt,n_nse))

     end if

  end do



  stop 'normal termination'

end program lpt_post
