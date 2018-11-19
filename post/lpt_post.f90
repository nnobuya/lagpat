program lpt_post

  implicit none

  !..param.
  integer, parameter:: ndim = 3
  integer, parameter:: ndt_ex = 100
  integer, parameter:: ndt_lim = 1000, npt_lim = 1000
  double precision, parameter:: ti_ini = 1.d-2, ti_fin = 1.d4
  double precision, parameter:: te_nse_cond = 9.d9
  logical, parameter:: debug = .false.

  !..main
  integer:: istg, ndt, npt
  double precision:: dt, dt_ex, tau_exp

  integer, allocatable:: i_pt(:,:), ist_pt(:,:), ndt_pt(:)
  double precision, allocatable:: ti(:), &
       & de_pt(:,:), te_pt(:,:), en_pt(:,:), &
       & ye_pt(:,:), x_pt(:,:,:), v_pt(:,:,:), rma_pt(:)

  double precision:: ti0, de0, te0, en0, ye0, rd0, vr0
  double precision:: ti_ex(1:ndt_ex), de_ex, te_ex, en_ex, ye_ex, rd_ex

  integer:: n_nse, num_nse
  double precision:: ti_nse, de_nse, te_nse, en_nse, ye_nse, rd_nse, vr_nse

  double precision:: h, t9_chk
  real(4):: dt_in, ti_in
  real(4), allocatable:: de_in(:), te_in(:), en_in(:), ye_in(:), &
       & x_in(:,:), v_in(:,:)

  character:: ch*10, ofile*100
  double precision:: rr(1:10), r1, r2
  integer:: i, ier, ipt, idt, ii(1:5), i1, i2

  if (debug) then
     write(*,*) '----------------------------------------'
     write(*,*) '     Debug Mode!!!'
     write(*,*) '----------------------------------------'
  end if

  !..make dt ext
  dt_ex = (log10(ti_fin) - log10(ti_ini)) /real(ndt_ex - 1)

  ti_ex(1) = log10(ti_ini)
  do i = 2, ndt_ex
     ti_ex(i) = ti_ex(i-1) + dt_ex
  end do

  ti_ex(1:ndt_ex) = 10.d0**ti_ex(1:ndt_ex)


  !..open
  !open(50, file = './lpt/move.dat', &
  !     & form = 'unformatted', action = 'read')
  !open(51, file = './lpt/hydro.dat', &
  !     & form = 'unformatted', action = 'read')
  !open(52, file = './lpt/stat_lpt.dat', &
  !     & form = 'unformatted', action = 'read')
  open(51, file = './lpt/part_init.dat', action = 'read')
  open(52, file = './lpt/part_fini.dat', action = 'read')

  open(61, file = './res/peak.dat'     , action = 'write')
  open(62, file = './res/pt_eject_nse.dat' , action = 'write')
  open(64, file = './res/bad_traj.dat' , action = 'write')

  ! ------------------------------------------------------------------ !
  !     set parameters                                                 !
  ! ------------------------------------------------------------------ !

  read(51,*)
  read(51,*) ch, r1, i1
  read(52,*)
  read(52,*) ch, r2, i2

  ndt = i2 - i1 + 1

  npt = 0
  read(51,*)
  do
     read(51,*,iostat=ier) i1
     if (ier /= 0) exit
     npt = npt + 1
  end do

  write(*,'("npt =", i10, 5x, "ndt =", i10)') npt, ndt

  rewind(51)
  rewind(52)

  allocate(de_in(1:npt), te_in(1:npt), en_in(1:npt), ye_in(1:npt), &
       & x_in(1:ndim,1:npt), v_in(1:ndim,1:npt))

  allocate(ti(1:ndt), i_pt(1:ndim,1:npt), ist_pt(1:npt,1:ndt), ndt_pt(1:npt), &
       & de_pt(1:npt,1:ndt), te_pt(1:npt,1:ndt), &
       & en_pt(1:npt,1:ndt), ye_pt(1:npt,1:ndt), &
       & x_pt(1:ndim,1:npt,1:ndt), v_pt(1:ndim,1:npt,1:ndt), rma_pt(1:npt))

  !     set parameters                                                 !
  ! ------------------------------------------------------------------ !

  read(51,*)
  read(51,*)
  read(51,*)
  do ipt = 1, npt
     read(51,*) ii(1:5), rma_pt(ipt), rr(1:10)
  end do

  write(*,'(/, "--- reading tracer data", i10, " Steps ---", /)') ndt

  lp_step: do idt = 1, ndt

     if (debug .and. idt >= ndt_lim) exit lp_step

     write(ofile,"('./lpt/lpt/traj_', i6.6, '.lpt')") idt
     open(50, file = ofile, form = 'unformatted', action = 'read')

     read(50) istg, ti_in, dt_in
     read(50) i_pt(1:ndim,1:npt), ist_pt(1:npt,idt), &
          & x_in(1:ndim,1:npt), v_in(1:ndim,1:npt), &
          & de_in(1:npt), te_in(1:npt), en_in(1:npt), ye_in(1:npt)
     close(50)

     !..convert real(4) to double precision
     ti(idt) = dble(ti_in)
     x_pt(1:ndim,1:npt,idt) = dble(x_in(1:ndim,1:npt))
     v_pt(1:ndim,1:npt,idt) = dble(v_in(1:ndim,1:npt))

     de_pt(1:npt,idt) = dble(de_in(1:npt))
     te_pt(1:npt,idt) = dble(te_in(1:npt))
     en_pt(1:npt,idt) = dble(en_in(1:npt))
     ye_pt(1:npt,idt) = dble(ye_in(1:npt))

     if (mod(idt,500) == 0 .or. idt <= 5) &
          & write(*,'(i7, "/", i7, i10, f8.1, "  ms")') &
          & idt, ndt, istg, ti(idt) *1000.0

  end do lp_step

  deallocate(de_in, te_in, en_in, ye_in)

  write(*,'(/, "--- end reading tracer data ---", /)')


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


  write(*,'(/, "--- writing tracer data ---", /)')

  num_nse = 0

  loop_tp_write: do ipt = 1, npt

     if (debug .and. idt >= npt_lim) exit loop_tp_write

     if (mod(ipt,500) == 0 .or. ( ipt <= 5 )) &
          & write(*,'(i10, "/", i10)') ipt, npt

     !..original hydro data
     write(ofile,'("./traj/hydro_", i7.7, ".org")') ipt
     open(60, file = ofile, action = 'write')

     write(60,'("#", 3x, "ti [s]", 9x, "de [g/cc]", &
          & 6x, "te [k]", 9x, "Ye", 13x, "ra [cm]")')

     do idt = 1, ndt_pt(ipt)
        write(60,'(*(es15.7))') ti(idt), &
             & de_pt(ipt,idt), te_pt(ipt,idt), ye_pt(ipt,idt),&
             & x_pt(1,ipt,idt)
     end do

     close(60)


     if (maxval(te_pt(ipt,1:ndt_pt(ipt))) < te_nse_cond) then

        lp_te_search: do idt = 1, ndt_pt(ipt)
           if (te_pt(ipt,idt) >= maxval(te_pt(ipt,1:ndt_pt(idt)))) then
              n_nse = idt
              exit lp_te_search
           end if
        end do lp_te_search

        !if (n_nse == 1) then
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


     write(62,'(1p, i10, *(e18.10))') ipt, rma_pt(ipt), &
          & ti_nse, de_nse, te_nse, en_nse, ye_nse, rd_nse

     write(ofile,'("./traj/hydro_", i7.7, ".dat")') ipt
     open(60, file = ofile, action = 'write')

     write(60,'("#", 3x, "ti [s]", 9x, "de [g/cc]", &
          & 6x, "te [k]", 9x, "ra [cm]")')
     write(60,'(1p, *(e15.7))') ti_nse, de_nse, te_nse, rd_nse

     do idt = n_nse + 1, ndt_pt(ipt)
        write(60,'(*(es15.7))') ti(idt), &
             & de_pt(ipt,idt), te_pt(ipt,idt), x_pt(1,ipt,idt)
     end do

     ti0 = ti(ndt_pt(ipt))
     de0 = de_pt(ipt,ndt_pt(ipt))
     te0 = te_pt(ipt,ndt_pt(ipt))
     en0 = en_pt(ipt,ndt_pt(ipt))
     ye0 = ye_pt(ipt,ndt_pt(ipt))
     rd0 = x_pt(1,ipt,ndt_pt(ipt))
     vr0 = v_pt(1,ipt,ndt_pt(ipt))     

     t9_chk = 0.d0

     tau_exp = 446.d0 /sqrt(de0)

     do idt = 1, ndt_ex

        rd_ex = rd0 + vr0 *ti_ex(idt)

        !de_ex = max(2.e1, de0 *(rd0 /rd_ex)**3)
        !te_ex = max(2.e6, te0 *(rd0 /rd_ex))

        de_ex = max(2.e1, de0 *exp(-ti_ex(idt) /tau_exp) )
        te_ex = max(2.e1, te0 *exp(-ti_ex(idt) /tau_exp /3.d0) )

        ye_ex = ye0 *exp(-(ti_ex(idt) - ti0))

        write(60,'(1p, *(e15.7))') &
             & min(ti_fin,ti0 + ti_ex(idt)), de_ex, te_ex, rd_ex

        t9_chk = max(t9_chk,te_ex)

        if (ti0 + ti_ex(idt) >= ti_fin) exit

     end do

     if (t9_chk > te_nse_cond) &
          & write(64,'(i10, 1p, 2e14.5)') ipt, t9_chk, te_ex

     close(60)

  end do loop_tp_write

  close(64)

  write(*,'(/, "--- writing tracer data ---", /)')
  print *, num_nse, npt

  stop 'normal termination'

end program lpt_post
