program lpt_post

  implicit none

  integer, parameter:: ndim = 3

  integer:: npt = 10000
  integer:: istg, ipt, ndt, idt
  real(4):: dt
  integer, allocatable:: ist_pt(:,:)
  real(4), allocatable:: ti(:), &
       & de_pt(:,:), te_pt(:,:), en_pt(:,:), &
       & ye_pt(:,:), x_pt(:,:,:), v_pt(:,:,:)

  character:: ofile*100
  integer:: ier



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


  allocate(ti(1:ndt), ist_pt(1:npt,1:ndt), &
       & de_pt(1:npt,1:ndt), te_pt(1:npt,1:ndt), &
       & en_pt(1:npt,1:ndt), ye_pt(1:npt,1:ndt), &
       & x_pt(1:ndim,1:npt,1:ndt), v_pt(1:ndim,1:npt,1:ndt))

  write(*,'("- reading tracer data")')

  do idt = 1, ndt

     read(50) istg, ti(idt), dt
     read(50) x_pt(1:ndim,1:npt,idt), v_pt(1:ndim,1:npt,idt)

     read(51) istg, ti(idt), dt
     read(51) de_pt(1:npt,idt), te_pt(1:npt,idt), &
          & en_pt(1:npt,idt), ye_pt(1:npt,idt)

     read(52) istg, ti(idt), dt
     read(52) ist_pt(1:npt,idt)

     if (mod(istg,100) == 0) write(*,*) istg, ti(idt)

  end do
  close(51)

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

     write(ofile,'("./tracer/hydro_", i5.5, ".dat")') ipt
     open(60, file = ofile, action = 'write')

     do idt = 1, ndt
        if (ist_pt(ipt,idt) == 0) then
           write(60,'(1p, *(e15.7))') ti(idt), &
                & de_pt(ipt,idt), te_pt(ipt,idt), ye_pt(ipt,idt)
        end if
     end do

     close(60)
  end do



  stop 'normal termination'

end program lpt_post
