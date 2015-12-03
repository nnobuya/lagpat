program lpt_post

  implicit none

  integer, parameter:: ndim = 3
  integer, parameter:: ndt_ex = 100
  real(8), parameter:: ti_ini = 1.d-2, ti_fin = 1.d4
  real(8), parameter:: te_nse_cond = 9.d9

  !..main
  integer:: istg, ndt, npt
  real(8):: dt, dt_ex
  integer, allocatable:: ist_pt(:,:), ndt_pt(:)
  real(8), allocatable:: ti(:), &
       & de_pt(:,:), te_pt(:,:), en_pt(:,:), &
       & ye_pt(:,:), x_pt(:,:,:), v_pt(:,:,:), rma_pt(:)
  real(8):: ti0, de0, te0, en0, ye0, rd0, vr0
  real(8):: ti_ex(1:ndt_ex), de_ex, te_ex, en_ex, ye_ex, rd_ex

  integer:: n_nse, num_nse
  real(8):: ti_nse, de_nse, te_nse, en_nse, ye_nse, rd_nse, vr_nse

  real(8):: h
  real(4):: dt_in, ti_in
  real(4), allocatable:: de_in(:), te_in(:), en_in(:), ye_in(:), &
       & x_in(:,:), v_in(:,:)

  character:: ofile*100
  real(8):: r1, r2, r3, r4
  integer:: i, ier, ipt, idt, i1, i2, i3, i4


  !..make dt ext
  dt_ex = (log10(ti_fin) - log10(ti_ini)) /real(ndt_ex - 1)

  ti_ex(1) = log10(ti_ini)
  do i = 2, ndt_ex
     ti_ex(i) = ti_ex(i-1) + dt_ex
  end do

  ti_ex(1:ndt_ex) = 10.d0**ti_ex(1:ndt_ex)


  !..open
  open(50, file = '../res/move.dat', &
       & form = 'unformatted', action = 'read')
  open(51, file = '../res/hydro.dat', &
       & form = 'unformatted', action = 'read')
  open(52, file = '../res/stat_lpt.dat', &
       & form = 'unformatted', action = 'read')
  open(53, file = '../res/set.dat', action = 'read')

  open(61, file = 'peak.dat'    , action = 'write')
  open(62, file = 'pt_eject.dat', action = 'write')


  read(53,*)
  read(53,*) npt, i1, i2, i3
  read(53,*)
  read(53,*)
  read(53,*)


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
       & x_pt(1:ndim,1:npt,1:ndt), v_pt(1:ndim,1:npt,1:ndt), rma_pt(1:npt))

  do ipt = 1, npt
     read(53,*) i1, i2, i3, i4, rma_pt(ipt), r1, r2, r3
  end do

  write(*,'("- reading tracer data", i10, "steps")') ndt

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

     if (mod(istg,500) == 0 .or. idt <= 10) &
          & write(*,'(i10, f8.1, "  ms")') istg, ti(idt) *1000.0

  end do
  close(51)


  deallocate(de_in, te_in, en_in, ye_in)

  write(*,'("- end reading tracer data")')

  ndt_pt(1:npt) = ndt
  do ipt = 1, npt
     do idt = 1, ndt
        if (ist_pt(ipt,idt) /= 0) exit        
        ndt_pt(ipt) = idt
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

  num_nse = 0

  do ipt = 1, npt

     if (mod(ipt,1000) == 0) write(*,'(2i10)') ipt, npt

     !..original hydro data
     write(ofile,'("./tracer/hydro_", i5.5, ".org")') ipt
     open(60, file = ofile, action = 'write')

     do idt = 1, ndt_pt(ipt)
        write(60,'(1p, *(e15.7))') ti(idt), &
             & de_pt(ipt,idt), te_pt(ipt,idt), ye_pt(ipt,idt),&
             & x_pt(1,ipt,idt)
     end do
     close(60)


     if (maxval(te_pt(ipt,1:ndt_pt(idt))) < te_nse_cond) then


        lp_te_search: do idt = ndt_pt(ipt), 1, -1
           if (te_pt(ipt,idt) >= maxval(te_pt(ipt,1:ndt_pt(idt)))) then
              n_nse = idt
              exit lp_te_search
           end if
        end do lp_te_search


        !if (n_nse == 1)then
        !   print *, 'okashiinodeha'
        !end if

        ti_nse = ti(n_nse)
        de_nse = de_pt(ipt,n_nse)
        te_nse = te_pt(ipt,n_nse)
        en_nse = en_pt(ipt,n_nse)
        ye_nse = ye_pt(ipt,n_nse)
        rd_nse = x_pt(1,ipt,n_nse)
     else

        num_nse = num_nse + 1

        lp_nse_search: do idt = ndt_pt(ipt), 1, -1
           if (te_pt(ipt,idt) >= te_nse_cond) then
              n_nse = idt
              exit lp_nse_search
           end if
        end do lp_nse_search

        te_nse = te_nse_cond

        h = (te_nse - te_pt(ipt,idt)) &
             & /(te_pt(ipt,n_nse + 1) - te_pt(ipt,n_nse))

        ti_nse = ti(n_nse) + h *(ti(n_nse + 1) - ti(n_nse))
        de_nse = de_pt(ipt,n_nse) &
             & + h *(de_pt(ipt,n_nse + 1) - de_pt(ipt,n_nse))
        en_nse = en_pt(ipt,n_nse) &
             & + h *(en_pt(ipt,n_nse + 1) - en_pt(ipt,n_nse))
        ye_nse = ye_pt(ipt,n_nse) &
             & + h *(ye_pt(ipt,n_nse + 1) - ye_pt(ipt,n_nse))

        rd_nse = x_pt(1,ipt,n_nse) &
             & + h *(x_pt(1,ipt,n_nse + 1) - x_pt(1,ipt,n_nse))
        vr_nse = v_pt(1,ipt,n_nse) &
             & + h *(v_pt(1,ipt,n_nse + 1) - v_pt(1,ipt,n_nse))

     end if


     write(62,'(1p, *(e15.7))') ye_nse, en_nse, rma_pt(ipt)


     write(ofile,'("./tracer/hydro_", i5.5, ".dat")') ipt
     open(60, file = ofile, action = 'write')

     write(60,'(1p, *(e15.7))') ti_nse, de_nse, te_nse, ye_nse, rd_nse
     do idt = n_nse + 1, ndt_pt(ipt)
        write(60,'(1p, *(e15.7))') ti(idt), &
             & de_pt(ipt,idt), te_pt(ipt,idt), ye_pt(ipt,idt), x_pt(1,ipt,idt)
     end do

     ti0 = ti(ndt_pt(ipt))
     de0 = de_pt(ipt,ndt_pt(ipt))
     te0 = te_pt(ipt,ndt_pt(ipt))
     en0 = en_pt(ipt,ndt_pt(ipt))
     ye0 = ye_pt(ipt,ndt_pt(ipt))
     rd0 = x_pt(1,ipt,ndt_pt(ipt))
     vr0 = v_pt(1,ipt,ndt_pt(ipt))

     do idt = 1, ndt_ex

        rd_ex = rd0 + vr0*ti_ex(idt)

        de_ex = max(2.e0, de0 *(rd0/rd_ex)**3)
        te_ex = max(2.e8, te0 *(rd0/rd_ex))
        ye_ex = ye0 *exp(-(ti_ex(idt) - ti0))
        write(60,'(1p, *(e15.7))') &
             & ti0 + ti_ex(idt), de_ex, te_ex, ye_ex, rd_ex
     end do

     close(60)

  end do

  print *, num_nse, npt

  stop 'normal termination'

end program lpt_post
