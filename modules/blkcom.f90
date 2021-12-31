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
  !  character(8) :: buff10(10)
  character(1) :: texcol(80)
  character(8) :: busone(1)
  character(8) :: bus1, bus2, bus3, bus4, bus5, bus6
  character(8) :: trash, blank, terra, userid, branch, chcopy
  character(8) :: chcont, csepar
  character(8) :: vstacs(24), texta6(15)
  character(80) :: abuff, buff10
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

  real(8) :: ci1, ck1
  real(8) :: deltat, delta2
  real(8) :: freqcs
  real(8) :: xunits, aincr, xmaxmx
  real(8) :: znolim(2), epstop, t, tolmat
  real(8) :: tmax, omega, copt, xopt, szplt
  real(8) :: szbed, sigmax
  real(8) :: fminfs, delffs, fmaxfs, tenerg, begmax(6)
  real(8) :: unity, onehaf
  real(8) :: fltinf, degmin, degmax, statfr, voltbc(50)
  real(8) :: flstat(20)
  real(8) :: angle
  real(8) :: pu, dltinv, speedl
  real(8) :: epsuba, epomeg, epdgel, epsiln, epszno, epwarn
  real(8) :: flzero
  real(8) :: peaknd(3)
  real(8) :: sglfir
  real(8) :: tenm3, tenm6, twopi

  !     flag-3.   begin class-3  /blank/  variables
  !               (integer-numeric usage only, with arrays
  !               preceding scalars).

  integer(4) :: indtv(10)
  integer(4) :: ipntv(11)
  integer(4) :: kprchg(6)
  integer(4) :: lunit0, lunit1, lunit2, lunit3, lunit4, lunit5, lunit6, lunit7
  integer(4) :: lunit8, lunit9, lunt10, lunt11, lunt12, lunt13, lunt14, lunt15
  integer(4) :: lunsav(15)
  integer(4) :: multpr(5)
  integer(4) :: nright, nfrfld, kolbeg, max99m
  integer(4) :: lstat(80)
  integer(4) :: moncar(90)
  integer(4) :: nbyte(6)
  integer(4) :: ia
  integer(4) :: iprsov(39)
  integer(4) :: icheck, iline, inonl, iout, ipunch, iread, istep, itype, it1
  integer(4) :: it2, izero
  integer(4) :: kol132, kpar, kpen(5), kunit6, kwtspy, kxic
  integer(4) :: kcount, istead, ldata, lbrnch
  integer(4) :: lexct, lbus, lymat, lswtch, lnonl, lchar, m4plot
  integer(4) :: lpast, lsize7, iplot, ncomp, nv, lcomp, numsm
  integer(4) :: ifdep, ltails, lfdep, lwt, last, npower, maxpe
  integer(4) :: lsiz12, lsmout, limass, iv
  integer(4) :: ktrlsw(8), num99, kpartb, llbuff, kanal, nsmth
  integer(4) :: nstacs, maxbus, lastov
  integer(4) :: lhist, ifx, isubc1, inecho, noutpr
  integer(4) :: ktab, jflsos, numdcd, numum, lspcum
  integer(4) :: nphcas, ialter, i_char, ktref, memsav, lisoff
  integer(4) :: kburro, iaverg, lsiz23, lsiz26, numout, moldat
  integer(4) :: lsiz27, lsiz28, ltlabl, iwt, ifdep2, idoubl, ioutin
  integer(4) :: ipun, jst, jst1, muntsv(2), numsub, maxzno
  integer(4) :: ifsem, lfsem, iadd, lfd, nexout
  integer(4) :: iofgnd
  integer(4) :: iofbnd
  integer(4) :: modout, lint
  integer(4) :: iftail, ncurr, ioffd, isplot, isprin, maxout
  integer(4) :: kill, ivolt, nchain, iprsup
  integer(4) :: intinf, kconst, kswtch, it, ntot, ibr
  integer(4) :: lsyn,  kssout
  integer(4) :: loopss(13)
  integer(4) :: numref, nword1, nword2, iloaep
  integer(4) :: ntot1, limstp, indstp, nc
  integer(4) :: icat, numnvo
  integer(4) :: nenerg
  integer(4) :: swtpe
  integer(4) :: newtac, niu, nsu, nsudv, nsup, nuk
  real(8) :: lnpin

  ! Equivalences
  character(8) :: alower(14), aupper(14)
  integer(4) :: iaddrs
  integer(4) :: ichtr2
  integer(4) :: idist
  integer(4) :: idistx(1)
  integer(4) :: iofkol
  integer(4) :: iofkor
  integer(4) :: ioutcs
  integer(4) :: ipoint
  integer(4) :: isw
  integer(4) :: itest
  integer(4) :: itranm
  integer(4) :: ityold
  integer(4) :: iupper
  integer(4) :: jseedr
  integer(4) :: kbase
  integer(4) :: kloaep
  integer(4) :: knt
  integer(4) :: knum
  integer(4) :: locz11
  integer(4) :: ltacst
  integer(4) :: ltdelt
  integer(4) :: maxniu
  integer(4) :: maxnuk
  integer(4) :: mdrive
  integer(4) :: mtape
  integer(4) :: next
  integer(4) :: nmauto
  integer(4) :: ntcsex
  ! integer(4) :: idistx(1), iofkol, iofkor
  ! integer(4) :: iaddrs, ichtr2, itranm, ityold, iupper
  ! integer(4) :: jseedr
  ! real(8) :: anglex(1)
  ! real(8) :: xlong1
  !
  !  equivalence (anglex(1), angle)
  !  equivalence (buff10(1), abuff(1))
  equivalence (buff10, abuff)
  !  equivalence (aupper(1), texcol(1))
  equivalence (bus1, busone(1))
  ! equivalence (bus1, kpen(1))
  equivalence (indtv(1), iaddrs)
  equivalence (indtv(2), itranm)
  equivalence (indtv(3), ityold)
  equivalence (indtv(4), ichtr2)
  equivalence (iofkol, iofgnd)
  equivalence (iofkor, iofbnd)
  equivalence (iprsov(35), ipoint)
  equivalence (iprsov(36), iupper)
  equivalence (iprsov(36), locz11)
  equivalence (iprsov(39), nmauto)
  equivalence (nenerg, idistx(1))
  !  equivalence (kpen(1), bus1)
  !  equivalence (kunit6, lunit6)
  equivalence (loopss(11), next)
  equivalence (lstat(14), knum)
  equivalence (lstat(14), mdrive)
  equivalence (lstat(67), maxniu)
  equivalence (lstat(68), maxnuk)
  equivalence (moncar(1), knt)
  equivalence (moncar(2), kbase)
  equivalence (moncar(3), ltdelt)
  equivalence (moncar(4), isw)
  equivalence (moncar(5), idist)
  equivalence (moncar(5), ioutcs)
  equivalence (moncar(6), itest)
  equivalence (moncar(8), jseedr)
  equivalence (moncar(9), kloaep)
  equivalence (moncar(10), mtape)
  equivalence (moncar(83), ntcsex)
  !  equivalence (omega, xlong1)

  ! data

  data terra / 'terra ' /
end module blkcom

!
! end of file blkcom.f90
!
