!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file blkcom.f90
!

module comthl
  !  common /comthl/ angtpe, nswtpe
  integer(4) :: nswtpe
  real(8) :: angtpe
end module comthl

module comld
  !  common /comld/ newtac
  integer(4) :: newtac
end module comld

module blkcom
  implicit none
  !     flag-1.   begin class-1  /blank/  variables
  !               (alphanumeric variables, capable of storing  a6  info).
  character(80) :: abuff
  character(8) :: bus1, bus2, bus3, bus4, bus5, bus6, texcol(160)
  character(8) :: trash, blank, terra, userid, branch, chcopy
  character(8) :: chcont, csepar
  character(8) :: vstacs(24), texta6(15)
  !
  ! common abuff
  ! common bus1, bus2, bus3, bus4, bus5, bus6
  ! common trash, blank, terra, userid, branch, copy
  ! common texcol
  ! common csepar, chcont, texta6
  ! common date1, tclock, vstacs
  !     flag-2.   begin class-2  /blank/  variables
  !               (floating-point numeric usage only, with scalars
  !               preceding arrays).

  real(8) :: ci1, ck1, deltat, delta2, freqcs
  real(8) :: xunits, aincr, xmaxmx
  real(8) :: znolim(2), epstop, t, tolmat
  real(8) :: tmax, omega, copt, xopt, szplt
  real(8) :: szbed, sigmax
  real(8) :: fminfs, delffs, fmaxfs, tenerg, begmax(6)
  real(8) :: unity, onehaf
  real(8) :: fltinf, degmin, degmax, statfr, voltbc(50)
  real(8) :: flstat(20)
  real(8) :: angle, pu, dltinv, speedl
  real(8) :: epsuba, epomeg, epdgel, epsiln, epszno, epwarn
  real(8) :: flzero
  real(8) :: peaknd(3)
  real(8) :: sglfir
  real(8) :: tenm3, tenm6, twopi
  ! common ci1, ck1, deltat, delta2, freqcs
  ! common epsiln, xunits, aincr, xmaxmx
  ! common znolim, epszno, epwarn, epstop, t, tolmat
  ! common twopi, tmax, omega, copt, xopt, szplt
  ! common szbed, sglfir, sigmax,  epsuba,  epdgel,  epomeg
  ! common fminfs, delffs, fmaxfs, tenerg, begmax
  ! common tenm3, tenm6, unity, onehaf, peaknd
  ! common fltinf, flzero, degmin, degmax, statfr, voltbc
  ! common flstat
  ! common angle, pu, dltinv, speedl
  !     flag-3.   begin class-3  /blank/  variables
  !               (integer-numeric usage only, with arrays
  !               preceding scalars).
  integer(4), private :: karg
  integer(4), dimension(10) :: indtv
  integer(4), dimension(11) :: ipntv
  integer(4), dimension(6) :: kprchg
  integer(4) :: lunit0, lunit1, lunit2, lunit3, lunit4, lunit5, lunit6, lunit7
  integer(4) :: lunit8, lunit9, lunt10, lunt11, lunt12, lunt13, lunt14, lunt15
  integer(4), dimension(15) :: lunsav
  integer(4), dimension(5) :: multpr
  integer(4) :: nright, nfrfld, kolbeg, max99m
  integer(4) :: lstat(80)
  integer(4), dimension(10) :: moncar
  integer(4), dimension(6) :: nbyte
  integer(4) :: ia, ioutcs, iprsov(39)
  integer(4) :: icheck, iline, inonl, iout, ipunch, iread, istep, itype, it1
  integer(4) :: it2, izero
  integer(4) :: kol132, kpar, kunit6, kwtspy, kxic
  integer(4) :: kcount, istead, ldata, lbrnch
  integer(4) :: lexct, lbus, lymat, lswtch, lnonl, lchar, m4plot
  integer(4) :: lpast, lsize7, iplot, ncomp, nv, lcomp, numsm
  integer(4) :: ifdep, ltails, lfdep, lwt, last, npower, maxpe
  integer(4) :: lsiz12, lsmout, limass, locz11, iv
  integer(4) :: ktrlsw(8), num99, kpartb, llbuff, kanal, nsmth
  integer(4) :: ntcsex, nstacs, maxbus, lastov, ltacst
  integer(4) :: lhist, ifx, isubc1, inecho, noutpr
  integer(4) :: ktab, jflsos, numdcd, numum, lspcum
  integer(4) :: nphcas, ialter, i_char, ktref, memsav, lisoff
  integer(4) :: kburro, iaverg, lsiz23, lsiz26, numout, moldat
  integer(4) :: lsiz27, lsiz28, ltlabl, iwt, ifdep2, idoubl, ioutin
  integer(4) :: ipun, jst, jst1, muntsv(2), numsub, maxzno
  integer(4) :: ifsem, lfsem, iadd, lfd, nexout
  integer(4) :: iofgnd, iofbnd, modout, lint
  integer(4) :: iftail, ncurr, ioffd, isplot, isprin, maxout
  integer(4) :: kill, ivolt, nchain, iprsup
  integer(4) :: intinf, kconst, kswtch, it, ntot, ibr
  integer(4) :: lsyn,  kssout, loopss(13)
  integer(4) :: numref, nword1, nword2, iloaep
  integer(4) :: ntot1, limstp, indstp, nc
  integer(4) :: icat, numnvo, nenerg
  integer(4) :: angtpe, nswtpe
  integer(4) :: newtac, niu, nsu, nsudv, nsup, nuk
  real(8) :: lnpin
  !  dimension iprsov(39), ktrlsw(8)
  !  dimension muntsv(2), loopss(13), lstat(80)
  equivalence (kunit6, lunit6)
  ! common moncar
  ! common lunit0, lunit1, lunit2, lunit3, lunit4, lunit5, lunit6, lunit7
  ! common lunit8, lunit9, lunt10, lunt11, lunt12, lunt13, lunt14, lunt15
  ! common nright, nfrfld, kolbeg, max99m
  ! common kprchg, multpr, ipntv, indtv
  ! common lstat, nbyte, lunsav, iprsov
  ! common icheck, iline, inonl, iout, ipunch, iread
  ! common kol132, istep, kwtspy,itype, it1, it2, izero
  ! common kcount, istead, ldata, lbrnch
  ! common lexct, lbus, lymat, lswtch, lnonl, lchar, m4plot
  ! common lpast, lsize7, iplot, ncomp, nv, lcomp, numsm
  ! common ifdep, ltails, lfdep, lwt, last, npower, maxpe
  ! common lsiz12, lsmout, limass, iv
  ! common ktrlsw, num99, kpartb, llbuff, kanal, nsmth
  ! common ntcsex, nstacs, maxbus, lastov, ltacst
  ! common lhist, ifx, isubc1, inecho, noutpr
  ! common ktab, jflsos, numdcd, numum, lspcum
  ! common nphcas, ialter, i_char, ktref, memsav, lisoff
  ! common kburro, iaverg, lsiz23, lsiz26, numout, moldat
  ! common lsiz27, lsiz28, ltlabl, iwt, ifdep2, idoubl, ioutin
  ! common ipun, jst, jst1, muntsv, numsub, maxzno
  ! common ifsem, lfsem, iadd, lfd, nexout(17)
  ! common iofgnd, iofbnd, modout, lint
  ! common iftail, ncurr, ioffd, isplot, isprin, maxout
  ! common kill, ivolt, nchain, iprsup
  ! common intinf, kconst, kswtch, it, ntot, ibr
  ! common lsyn,  kssout, loopss
  ! common numref, nword1, nword2, iloaep, lnpin
  ! common ntot1, limstp, indstp, nc
  ! common icat, numnvo, nenerg
  !  common /comthl/ angtpe, nswtpe
  !  common /comld/ newtac
  ! Other modules equivalences:
  ! from tacsar
  equivalence (nuk, lstat(51)), (ia, lstat(52))
  equivalence (nsu, lstat(53)), (niu, lstat(54))
  equivalence (nsup, lstat(55)), (karg, lstat(56))
  equivalence (kpar, lstat(57)), (kxic, lstat(58))
  equivalence (ioutcs, lstat(59)), (nsudv, lstat(60))
  ! from ntacs1
  integer(4) :: maxniu, maxnuk
  equivalence (lstat(67), maxniu)
  equivalence (lstat(68), maxnuk)
  ! from main00
  integer(4) :: knt
  character(8) :: buff10(10), aupper(14), alower(14)
  equivalence (buff10(1), abuff)
  equivalence (moncar(1), knt)
  equivalence (aupper(1), texcol(1))
  ! from main10
  character(8) :: busone(1)
  integer(4) :: idistx(1), iprsav(4)
  equivalence (idistx(1), nenerg)
  equivalence (busone(1), idistx(1))
  equivalence (bus1, busone(1))
  equivalence (nenerg, idistx(1))
  equivalence (kpen(1), bus1)
  equivalence (moncar(2), kbase)
  ! from over1
  integer(4) :: idist, isw, itest, intbus(1), jseedr, kbase, kloaep, kpen(5)
  integer(4) :: ltdelt, mtape, nmauto
  real(8) :: anglex(1)
  equivalence (moncar(1), knt)
  equivalence (moncar(2), kbase)
  equivalence (moncar(3), ltdelt)
  equivalence (moncar(4), isw)
  equivalence (moncar(5), idist)
  equivalence (moncar(6), itest)
  equivalence (moncar(8), jseedr)
  equivalence (moncar(9), kloaep)
  equivalence (moncar(10), mtape)
  equivalence (anglex(1), angle)
  equivalence (iprsov(39), nmauto)
  equivalence (bus1, kpen(1))
  equivalence (intbus(1), bus1)
  ! from over2
  equivalence (iprsov(36), locz11)
  ! from over7
  integer(4) :: iofkol, iofkor, next
  equivalence (loopss(11), next)
  equivalence (iofkol, iofgnd)
  equivalence (iofkor, iofbnd)
  ! from over10

  ! from over16
  integer(4) :: ipoint, iupper, n20
  equivalence (iprsov(35), ipoint)
  equivalence (iprsov(36), iupper)
  equivalence (ktrlsw(1), n20)
  ! data
  data terra / 'terra ' /
end module blkcom

!
! end of file blkcom.f90
!
