!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file dekspy.f90
!

!
!     This deck is used only by interactive execution modules
!     which begin with "emtspy".  Note "deck29" working space:

module dekspy
  integer(4) :: iascii(1000), ibegcl, iendcl, imax(55), imin(55), inchlp
  integer(4) :: indbeg, indbuf, indxrp(20), intout(55), iprspy, itexp
  integer(4) :: ivec(1000)
  integer(4) :: jjroll, jword
  integer(4) :: kar1(1), kar2(2), karray(3), kbegtx(85), kbreak, kbrser, kerase, kfile5
  integer(4) :: kilper, kolout, komadd, konadd(14), kontac(14), kopyit, kserlc
  integer(4) :: kslowr, ksmspy(3), kspsav, ksymbl, kverfy, kyramp(20)
  integer(4) :: labels(15), lidnt1, lidnt2, limarr(4), limbuf, limcrd, linnow
  integer(4) :: linspn, locate(1000), lockbr, locout(55), looprp(20), lserlc
  integer(4) :: luntsp
  integer(4) :: maxarg, maxflg, memkar, memrmp(20), mflush, monitr, monits
!  integer(4) :: numnam
  integer(4) :: munit5
  integer(4) :: n10rmp(20), nbreak, newvec, nexmod, nline(1000), noback, numcrd, numex, numkey
  integer(4) :: numrmp, numsym
  real(8) :: epskon(20) !, evmx, evmxf
  real(8) :: fbegrp(20), fendrp(20), fkar1, fkar2
!  real(8) :: gymax, gymin
  real(8) :: rampcn(20), rampsl(20)
  real(8) :: tbegrp(20), tbreak, tendrp(20), tmaxrp, tminrp
  !  common /c29b01/ karray(3)                    ! "deck29" stores plot points
  real(8) :: pltbuf(1), forbyt(600)               ! real*4 plot storage
  equivalence (pltbuf(1), karray(1))              ! /c29b01/ plot points
  logical :: logvar                               ! logical variable for "inquire" usage
  ! common /spycom/ rampcn(20), rampsl(20), kyramp(20)
  ! common /spycom/ fendrp(20), tminrp, tmaxrp
  ! common /spycom/ tbegrp(20), tendrp(20), fbegrp(20)
  ! common /spycom/ tbreak, epskon(14)
  !     end reals, next come integers, in new common block:
  ! common /spykom/ indxrp(20), ivec(1000), iascii(1000)
  ! common /spykom/ numsym,  jjroll, itexp, labels(15)
  ! common /spykom/ maxarg, kilper, kfile5, kverfy, jword
  ! common /spykom/ ibegcl, iendcl, lidnt1, lidnt2, nbreak
  ! common /spykom/ linnow, linspn, numcrd, munit5, numkey
  ! common /spykom/ indbuf, indbeg, mflush, newvec, maxflg
  ! common /spykom/ kspsav, memkar, noback, ksmspy(3)
  ! common /spykom/ lserlc, kserlc, kbrser, lockbr, kerase
  ! common /spykom/ komadd
  ! common /spykom/ iprspy,  monitr, monits, locate(1000)
  ! common /spykom/ nline(1000),  kbreak, limbuf, kolout
  ! common /spykom/ limarr(4), imin(55), imax(55), numex
  ! common /spykom/ locout(55), intout(55), nexmod, nextsn
  !  common /spykom/ inchlp, ksymbl, kopyit, kslowr, limcrd
  ! common /spykom/ looprp(20), n10rmp(20), memrmp(20)
  ! common /spykom/ kontac(14), konadd(14), kbegtx(85)
  !  common /spykom/ kar1(1), kar2(2), numrmp, luntsp, logvar
  dimension fkar1(1), fkar2(1)
  equivalence (kar1, fkar1), (kar2, fkar2)
  !     character*1 char1, filbyt(1), col, filext, digit
  integer(4) nextsn
  character char1, filext(10), digit(10)
  character(8) symb(1000), symbrp(20), junker, texpar(10), spykwd(75)
  character(8) ansi8, brobus
  character(16) ansi16
  character(20) bytfnd
  !character(25) col
  character(32) ansi32
  character(35) spycd2
  character(80) bytbuf, buff77, file6(3000), file6b(20), blan80
  character(80) prom80, answ80, texspy(1250), abufsv
  character(132) munit6, outlin, outsav, heding
  !  common /spyf77/ filext(10), symb(1000)
  !  common /spyf77/ bytfnd, char1, symbrp(20), abufsv, junker
  !  common /spyf77/ bytbuf, buff77, file6b(20), file6(30000)
  !  common /spyf77/ blan80, prom80, digit(10), texpar(10)
  !  common /spyf77/ spykwd(75), ansi8, ansi16, ansi32
  !  common /spyf77/ spycd2, answ80, brobus, munit6
  !  common /spyf77/ outlin, outsav, heding, texspy(1250)
   data symb(  1) / 'bus1  '/,  ivec(  1) /  0/,  iascii(  1) /1/
   data symb(  2) / 'bus2  '/,  ivec(  2) /  0/,  iascii(  2) /1/
   data symb(  3) / 'bus3  '/,  ivec(  3) /  0/,  iascii(  3) /1/
   data symb(  4) / 'bus4  '/,  ivec(  4) /  0/,  iascii(  4) /1/
   data symb(  5) / 'bus5  '/,  ivec(  5) /  0/,  iascii(  5) /1/
   data symb(  6) / 'bus6  '/,  ivec(  6) /  0/,  iascii(  6) /1/
   data symb(  7) / 'trash '/,  ivec(  7) /  0/,  iascii(  7) /1/
   data symb(  8) / 'blank '/,  ivec(  8) /  0/,  iascii(  8) /1/
   data symb(  9) / 'terra '/,  ivec(  9) /  0/,  iascii(  9) /1/
   data symb( 10) / 'userid'/,  ivec( 10) /  0/,  iascii( 10) /1/
   data symb( 11) / 'branch'/,  ivec( 11) /  0/,  iascii( 11) /1/
   data symb( 12) / 'copy  '/,  ivec( 12) /  0/,  iascii( 12) /1/
   data symb( 13) / 'csepar'/,  ivec( 13) /  0/,  iascii( 13) /1/
   data symb( 14) / 'chcont'/,  ivec( 14) /  0/,  iascii( 14) /1/
   data symb( 15) / 'texcol'/,  ivec( 15) /  1/,  iascii( 15) /1/
   data symb( 16) / 'texta6'/,  ivec( 16) /  1/,  iascii( 16) /1/
   data symb( 17) / 'date1 '/,  ivec( 17) /  1/,  iascii( 17) /1/
   data symb( 18) / 'tclock'/,  ivec( 18) /  1/,  iascii( 18) /1/
   data symb( 19) / 'vstacs'/,  ivec( 19) /  1/,  iascii( 19) /1/
   data symb( 20) / 'abuff '/,  ivec( 20) /  1/,  iascii( 20) /1/
   data symb( 21) / 'ci1   '/,  ivec( 21) /  0/,  iascii( 21) /0/
   data symb( 22) / 'ck1   '/,  ivec( 22) /  0/,  iascii( 22) /0/
   data symb( 23) / 'deltat'/,  ivec( 23) /  0/,  iascii( 23) /0/
   data symb( 24) / 'delta2'/,  ivec( 24) /  0/,  iascii( 24) /0/
   data symb( 25) / 'freqcs'/,  ivec( 25) /  0/,  iascii( 25) /0/
   data symb( 26) / 'epsiln'/,  ivec( 26) /  0/,  iascii( 26) /0/
   data symb( 27) / 'xunits'/,  ivec( 27) /  0/,  iascii( 27) /0/
   data symb( 28) / 'aincr '/,  ivec( 28) /  0/,  iascii( 28) /0/
   data symb( 29) / 'xmaxmx'/,  ivec( 29) /  0/,  iascii( 29) /0/
   data symb( 30) / 'znvref'/,  ivec( 30) /  0/,  iascii( 30) /0/
   data symb( 31) / 'epszno'/,  ivec( 31) /  0/,  iascii( 31) /0/
   data symb( 32) / 'epwarn'/,  ivec( 32) /  0/,  iascii( 32) /0/
   data symb( 33) / 'epstop'/,  ivec( 33) /  0/,  iascii( 33) /0/
   data symb( 34) / 't     '/,  ivec( 34) /  0/,  iascii( 34) /0/
   data symb( 35) / 'hertz '/,  ivec( 35) /  0/,  iascii( 35) /0/
   data symb( 36) / 'tolmat'/,  ivec( 36) /  0/,  iascii( 36) /0/
   data symb( 37) / 'twopi '/,  ivec( 37) /  0/,  iascii( 37) /0/
   data symb( 38) / 'tmax  '/,  ivec( 38) /  0/,  iascii( 38) /0/
   data symb( 39) / 'omega '/,  ivec( 39) /  0/,  iascii( 39) /0/
   data symb( 40) / 'copt  '/,  ivec( 40) /  0/,  iascii( 40) /0/
   data symb( 41) / 'xopt  '/,  ivec( 41) /  0/,  iascii( 41) /0/
   data symb( 42) / 'szplt '/,  ivec( 42) /  0/,  iascii( 42) /0/
   data symb( 43) / 'szbed '/,  ivec( 43) /  0/,  iascii( 43) /0/
   data symb( 44) / 'sglfir'/,  ivec( 44) /  0/,  iascii( 44) /0/
   data symb( 45) / 'sigmax'/,  ivec( 45) /  0/,  iascii( 45) /0/
   data symb( 46) / 'epsuba'/,  ivec( 46) /  0/,  iascii( 46) /0/
   data symb( 47) / 'epdgel'/,  ivec( 47) /  0/,  iascii( 47) /0/
   data symb( 48) / 'epomeg'/,  ivec( 48) /  0/,  iascii( 48) /0/
   data symb( 49) / 'fminfs'/,  ivec( 49) /  0/,  iascii( 49) /0/
   data symb( 50) / 'delffs'/,  ivec( 50) /  0/,  iascii( 50) /0/
   data symb( 51) / 'fmaxfs'/,  ivec( 51) /  0/,  iascii( 51) /0/
   data symb( 52) / 'tenerg'/,  ivec( 52) /  0/,  iascii( 52) /0/
   data symb( 53) / 'begmax'/,  ivec( 53) /  1/,  iascii( 53) /0/
   data symb( 54) / 'tenm3 '/,  ivec( 54) /  0/,  iascii( 54) /0/
   data symb( 55) / 'tenm6 '/,  ivec( 55) /  0/,  iascii( 55) /0/
   data symb( 56) / 'unity '/,  ivec( 56) /  0/,  iascii( 56) /0/
   data symb( 57) / 'onehaf'/,  ivec( 57) /  0/,  iascii( 57) /0/
   data symb( 58) / 'peaknd'/,  ivec( 58) /  1/,  iascii( 58) /0/
   data symb( 59) / 'fltinf'/,  ivec( 59) /  0/,  iascii( 59) /0/
   data symb( 60) / 'flzero'/,  ivec( 60) /  0/,  iascii( 60) /0/
   data symb( 61) / 'degmin'/,  ivec( 61) /  0/,  iascii( 61) /0/
   data symb( 62) / 'degmax'/,  ivec( 62) /  0/,  iascii( 62) /0/
   data symb( 63) / 'statfr'/,  ivec( 63) /  0/,  iascii( 63) /0/
   data symb( 64) / 'voltbc'/,  ivec( 64) /  1/,  iascii( 64) /0/
   data symb( 65) / 'flstat'/,  ivec( 65) /  1/,  iascii( 65) /0/
   data symb( 66) / 'dtnext'/,  ivec( 66) /  1/,  iascii( 66) /0/
   data symb( 67) / 'angle '/,  ivec( 67) /  0/,  iascii( 67) /0/
   data symb( 68) / 'pu    '/,  ivec( 68) /  0/,  iascii( 68) /0/
   data symb( 69) / 'seedr '/,  ivec( 69) /  0/,  iascii( 69) /0/
   data symb( 70) / 'speedl'/,  ivec( 70) /  0/,  iascii( 70) /0/
   data symb( 71) / 'kstart'/,  ivec( 71) /  0/,  iascii( 71) /0/
   data symb( 72) / 'knt   '/,  ivec( 72) /  0/,  iascii( 72) /0/
   data symb( 73) / 'kbase '/,  ivec( 73) /  0/,  iascii( 73) /0/
   data symb( 74) / 'ltdelt'/,  ivec( 74) /  0/,  iascii( 74) /0/
   data symb( 75) / 'unused'/,  ivec( 75) /  0/,  iascii( 75) /0/
   data symb( 76) / 'mtape '/,  ivec( 76) /  0/,  iascii( 76) /0/
   data symb( 77) / 'lunit1'/,  ivec( 77) /  0/,  iascii( 77) /0/
   data symb( 78) / 'lunit2'/,  ivec( 78) /  0/,  iascii( 78) /0/
   data symb( 79) / 'lunit3'/,  ivec( 79) /  0/,  iascii( 79) /0/
   data symb( 80) / 'lunit4'/,  ivec( 80) /  0/,  iascii( 80) /0/
   data symb( 81) / 'lunit5'/,  ivec( 81) /  0/,  iascii( 81) /0/
   data symb( 82) / 'lunit6'/,  ivec( 82) /  0/,  iascii( 82) /0/
   data symb( 83) / 'lunit7'/,  ivec( 83) /  0/,  iascii( 83) /0/
   data symb( 84) / 'lunit8'/,  ivec( 84) /  0/,  iascii( 84) /0/
   data symb( 85) / 'lunit9'/,  ivec( 85) /  0/,  iascii( 85) /0/
   data symb( 86) / 'lunt10'/,  ivec( 86) /  0/,  iascii( 86) /0/
   data symb( 87) / 'lunt11'/,  ivec( 87) /  0/,  iascii( 87) /0/
   data symb( 88) / 'lunt12'/,  ivec( 88) /  0/,  iascii( 88) /0/
   data symb( 89) / 'lunt13'/,  ivec( 89) /  0/,  iascii( 89) /0/
   data symb( 90) / 'lunt14'/,  ivec( 90) /  0/,  iascii( 90) /0/
   data symb( 91) / 'lunt15'/,  ivec( 91) /  0/,  iascii( 91) /0/
   data symb( 92) / 'nexout'/,  ivec( 92) /  1/,  iascii( 92) /0/
   data symb( 93) / 'nright'/,  ivec( 93) /  0/,  iascii( 93) /0/
   data symb( 94) / 'nfrfld'/,  ivec( 94) /  0/,  iascii( 94) /0/
   data symb( 95) / 'kolbeg'/,  ivec( 95) /  0/,  iascii( 95) /0/
   data symb( 96) / 'kprchg'/,  ivec( 96) /  1/,  iascii( 96) /0/
   data symb( 97) / 'multpr'/,  ivec( 97) /  1/,  iascii( 97) /0/
   data symb( 98) / 'ipntv '/,  ivec( 98) /  1/,  iascii( 98) /0/
   data symb( 99) / 'indtv '/,  ivec( 99) /  1/,  iascii( 99) /0/
   data symb(100) / 'lstat '/,  ivec(100) /  1/,  iascii(100) /0/
   data symb(101) / 'nbyte '/,  ivec(101) /  1/,  iascii(101) /0/
   data symb(102) / 'lunsav'/,  ivec(102) /  1/,  iascii(102) /0/
   data symb(103) / 'iprsov'/,  ivec(103) /  1/,  iascii(103) /0/
   data symb(104) / 'icheck'/,  ivec(104) /  0/,  iascii(104) /0/
   data symb(105) / 'unused'/,  ivec(105) /  0/,  iascii(105) /0/
   data symb(106) / 'iend  '/,  ivec(106) /  0/,  iascii(106) /0/
   data symb(107) / 'iline '/,  ivec(107) /  0/,  iascii(107) /0/
   data symb(108) / 'inonl '/,  ivec(108) /  0/,  iascii(108) /0/
   data symb(109) / 'iold  '/,  ivec(109) /  0/,  iascii(109) /0/
   data symb(110) / 'iout  '/,  ivec(110) /  0/,  iascii(110) /0/
   data symb(111) / 'iprint'/,  ivec(111) /  0/,  iascii(111) /0/
   data symb(112) / 'ipunch'/,  ivec(112) /  0/,  iascii(112) /0/
   data symb(113) / 'iread '/,  ivec(113) /  0/,  iascii(113) /0/
   data symb(114) / 'kol132'/,  ivec(114) /  0/,  iascii(114) /0/
   data symb(115) / 'istep '/,  ivec(115) /  0/,  iascii(115) /0/
   data symb(116) / 'unused'/,  ivec(116) /  0/,  iascii(116) /0/
   data symb(117) / 'itype '/,  ivec(117) /  0/,  iascii(117) /0/
   data symb(118) / 'it1   '/,  ivec(118) /  0/,  iascii(118) /0/
   data symb(119) / 'it2   '/,  ivec(119) /  0/,  iascii(119) /0/
   data symb(120) / 'iupper'/,  ivec(120) /  0/,  iascii(120) /0/
   data symb(121) / 'izero '/,  ivec(121) /  0/,  iascii(121) /0/
   data symb(122) / 'kcount'/,  ivec(122) /  0/,  iascii(122) /0/
   data symb(123) / 'istead'/,  ivec(123) /  0/,  iascii(123) /0/
   data symb(124) / 'unused'/,  ivec(124) /  0/,  iascii(124) /0/
   data symb(125) / 'ldata '/,  ivec(125) /  0/,  iascii(125) /0/
   data symb(126) / 'lbrnch'/,  ivec(126) /  0/,  iascii(126) /0/
   data symb(127) / 'limtxf'/,  ivec(127) /  0/,  iascii(127) /0/
   data symb(128) / 'mdebug'/,  ivec(128) /  0/,  iascii(128) /0/
   data symb(129) / 'lexct '/,  ivec(129) /  0/,  iascii(129) /0/
   data symb(130) / 'lbus  '/,  ivec(130) /  0/,  iascii(130) /0/
   data symb(131) / 'lymat '/,  ivec(131) /  0/,  iascii(131) /0/
   data symb(132) / 'lswtch'/,  ivec(132) /  0/,  iascii(132) /0/
   data symb(133) / 'lnonl '/,  ivec(133) /  0/,  iascii(133) /0/
   data symb(134) / 'lchar '/,  ivec(134) /  0/,  iascii(134) /0/
   data symb(135) / 'm4plot'/,  ivec(135) /  0/,  iascii(135) /0/
   data symb(136) / 'lpast '/,  ivec(136) /  0/,  iascii(136) /0/
   data symb(137) / 'lsmat '/,  ivec(137) /  0/,  iascii(137) /0/
   data symb(138) / 'iplot '/,  ivec(138) /  0/,  iascii(138) /0/
   data symb(139) / 'ncomp '/,  ivec(139) /  0/,  iascii(139) /0/
   data symb(140) / 'nv    '/,  ivec(140) /  0/,  iascii(140) /0/
   data symb(141) / 'lcomp '/,  ivec(141) /  0/,  iascii(141) /0/
   data symb(142) / 'numsm '/,  ivec(142) /  0/,  iascii(142) /0/
   data symb(143) / 'ifdep '/,  ivec(143) /  0/,  iascii(143) /0/
   data symb(144) / 'ltails'/,  ivec(144) /  0/,  iascii(144) /0/
   data symb(145) / 'lfdep '/,  ivec(145) /  0/,  iascii(145) /0/
   data symb(146) / 'lwt   '/,  ivec(146) /  0/,  iascii(146) /0/
   data symb(147) / 'last  '/,  ivec(147) /  0/,  iascii(147) /0/
   data symb(148) / 'npower'/,  ivec(148) /  0/,  iascii(148) /0/
   data symb(149) / 'maxpe '/,  ivec(149) /  0/,  iascii(149) /0/
   data symb(150) / 'lpeak '/,  ivec(150) /  0/,  iascii(150) /0/
   data symb(151) / 'nout  '/,  ivec(151) /  0/,  iascii(151) /0/
   data symb(152) / 'iv    '/,  ivec(152) /  0/,  iascii(152) /0/
   data symb(153) / 'ineof '/,  ivec(153) /  0/,  iascii(153) /0/
   data symb(154) / 'ktrlsw'/,  ivec(154) /  1/,  iascii(154) /0/
   data symb(155) / 'num99 '/,  ivec(155) /  0/,  iascii(155) /0/
   data symb(156) / 'kpartb'/,  ivec(156) /  0/,  iascii(156) /0/
   data symb(157) / 'llbuff'/,  ivec(157) /  0/,  iascii(157) /0/
   data symb(158) / 'kanal '/,  ivec(158) /  0/,  iascii(158) /0/
   data symb(159) / 'nsmth '/,  ivec(159) /  0/,  iascii(159) /0/
   data symb(160) / 'ntcsex'/,  ivec(160) /  0/,  iascii(160) /0/
   data symb(161) / 'nstacs'/,  ivec(161) /  0/,  iascii(161) /0/
   data symb(162) / 'kloaep'/,  ivec(162) /  0/,  iascii(162) /0/
   data symb(163) / 'lastov'/,  ivec(163) /  0/,  iascii(163) /0/
   data symb(164) / 'ltacst'/,  ivec(164) /  0/,  iascii(164) /0/
   data symb(165) / 'lhist '/,  ivec(165) /  0/,  iascii(165) /0/
   data symb(166) / 'ifx   '/,  ivec(166) /  0/,  iascii(166) /0/
   data symb(167) / 'ndelta'/,  ivec(167) /  0/,  iascii(167) /0/
   data symb(168) / 'idelta'/,  ivec(168) /  0/,  iascii(168) /0/
   data symb(169) / 'inecho'/,  ivec(169) /  0/,  iascii(169) /0/
   data symb(170) / 'noutpr'/,  ivec(170) /  0/,  iascii(170) /0/
   data symb(171) / 'ktab  '/,  ivec(171) /  0/,  iascii(171) /0/
   data symb(172) / 'jflsos'/,  ivec(172) /  0/,  iascii(172) /0/
   data symb(173) / 'numdcd'/,  ivec(173) /  0/,  iascii(173) /0/
   data symb(174) / 'numum '/,  ivec(174) /  0/,  iascii(174) /0/
   data symb(175) / 'lspcum'/,  ivec(175) /  0/,  iascii(175) /0/
   data symb(176) / 'nphcas'/,  ivec(176) /  0/,  iascii(176) /0/
   data symb(177) / 'locz11'/,  ivec(177) /  0/,  iascii(177) /0/
   data symb(178) / 'locbr1'/,  ivec(178) /  0/,  iascii(178) /0/
   data symb(179) / 'ialter'/,  ivec(179) /  0/,  iascii(179) /0/
   data symb(180) / 'i_char'/,  ivec(180) /  0/,  iascii(180) /0/
   data symb(181) / 'ktref '/,  ivec(181) /  0/,  iascii(181) /0/
   data symb(182) / 'kph   '/,  ivec(182) /  0/,  iascii(182) /0/
   data symb(183) / 'kreqab'/,  ivec(183) /  0/,  iascii(183) /0/
   data symb(184) / 'ksat  '/,  ivec(184) /  0/,  iascii(184) /0/
   data symb(185) / 'memsav'/,  ivec(185) /  0/,  iascii(185) /0/
   data symb(186) / 'lisoff'/,  ivec(186) /  0/,  iascii(186) /0/
   data symb(187) / 'lspov4'/,  ivec(187) /  0/,  iascii(187) /0/
   data symb(188) / 'kburro'/,  ivec(188) /  0/,  iascii(188) /0/
   data symb(189) / 'iaverg'/,  ivec(189) /  0/,  iascii(189) /0/
   data symb(190) / 'lsiz23'/,  ivec(190) /  0/,  iascii(190) /0/
   data symb(191) / 'lsiz26'/,  ivec(191) /  0/,  iascii(191) /0/
   data symb(192) / 'numout'/,  ivec(192) /  0/,  iascii(192) /0/
   data symb(193) / 'moldat'/,  ivec(193) /  0/,  iascii(193) /0/
   data symb(194) / 'lsiz27'/,  ivec(194) /  0/,  iascii(194) /0/
   data symb(195) / 'ltlabl'/,  ivec(195) /  0/,  iascii(195) /0/
   data symb(196) / 'iwt   '/,  ivec(196) /  0/,  iascii(196) /0/
   data symb(197) / 'ifdep2'/,  ivec(197) /  0/,  iascii(197) /0/
   data symb(198) / 'idoubl'/,  ivec(198) /  0/,  iascii(198) /0/
   data symb(199) / 'ioutin'/,  ivec(199) /  0/,  iascii(199) /0/
   data symb(200) / 'ipun  '/,  ivec(200) /  0/,  iascii(200) /0/
   data symb(201) / 'jst   '/,  ivec(201) /  0/,  iascii(201) /0/
   data symb(202) / 'jst1  '/,  ivec(202) /  0/,  iascii(202) /0/
   data symb(203) / 'unused'/,  ivec(203) /  0/,  iascii(203) /0/
   data symb(204) / 'numsub'/,  ivec(204) /  0/,  iascii(204) /0/
   data symb(205) / 'maxzno'/,  ivec(205) /  0/,  iascii(205) /0/
   data symb(206) / 'kalplt'/,  ivec(206) /  0/,  iascii(206) /0/
   data symb(207) / 'niomax'/,  ivec(207) /  0/,  iascii(207) /0/
   data symb(208) / 'niamax'/,  ivec(208) /  0/,  iascii(208) /0/
   data symb(209) / 'ibr1  '/,  ivec(209) /  0/,  iascii(209) /0/
   data symb(210) / 'ifsem '/,  ivec(210) /  0/,  iascii(210) /0/
   data symb(211) / 'lfsem '/,  ivec(211) /  0/,  iascii(211) /0/
   data symb(212) / 'iadd  '/,  ivec(212) /  0/,  iascii(212) /0/
   data symb(213) / 'lfd   '/,  ivec(213) /  0/,  iascii(213) /0/
   data symb(214) / 'laux  '/,  ivec(214) /  0/,  iascii(214) /0/
   data symb(215) / 'iofgnd'/,  ivec(215) /  0/,  iascii(215) /0/
   data symb(216) / 'iofbnd'/,  ivec(216) /  0/,  iascii(216) /0/
   data symb(217) / 'unused'/,  ivec(217) /  0/,  iascii(217) /0/
   data symb(218) / 'jseedr'/,  ivec(218) /  0/,  iascii(218) /0/
   data symb(219) / 'modout'/,  ivec(219) /  0/,  iascii(219) /0/
   data symb(220) / 'iftail'/,  ivec(220) /  0/,  iascii(220) /0/
   data symb(221) / 'ipoint'/,  ivec(221) /  0/,  iascii(221) /0/
   data symb(222) / 'lpast2'/,  ivec(222) /  0/,  iascii(222) /0/
   data symb(223) / 'ncurr '/,  ivec(223) /  0/,  iascii(223) /0/
   data symb(224) / 'ioffd '/,  ivec(224) /  0/,  iascii(224) /0/
   data symb(225) / 'isplot'/,  ivec(225) /  0/,  iascii(225) /0/
   data symb(226) / 'isprin'/,  ivec(226) /  0/,  iascii(226) /0/
   data symb(227) / 'maxout'/,  ivec(227) /  0/,  iascii(227) /0/
   data symb(228) / 'ipos  '/,  ivec(228) /  0/,  iascii(228) /0/
   data symb(229) / 'unused'/,  ivec(229) /  0/,  iascii(229) /0/
   data symb(230) / 'unused'/,  ivec(230) /  0/,  iascii(230) /0/
   data symb(231) / 'kill  '/,  ivec(231) /  0/,  iascii(231) /0/
   data symb(232) / 'ivolt '/,  ivec(232) /  0/,  iascii(232) /0/
   data symb(233) / 'nchain'/,  ivec(233) /  0/,  iascii(233) /0/
   data symb(234) / 'iprsup'/,  ivec(234) /  0/,  iascii(234) /0/
   data symb(235) / 'unused'/,  ivec(235) /  0/,  iascii(235) /0/
   data symb(236) / 'intinf'/,  ivec(236) /  0/,  iascii(236) /0/
   data symb(237) / 'kconst'/,  ivec(237) /  0/,  iascii(237) /0/
   data symb(238) / 'kswtch'/,  ivec(238) /  0/,  iascii(238) /0/
   data symb(239) / 'it    '/,  ivec(239) /  0/,  iascii(239) /0/
   data symb(240) / 'ntot  '/,  ivec(240) /  0/,  iascii(240) /0/
   data symb(241) / 'ibr   '/,  ivec(241) /  0/,  iascii(241) /0/
   data symb(242) / 'lcom10'/,  ivec(242) /  0/,  iascii(242) /0/
   data symb(243) / 'ltrnst'/,  ivec(243) /  0/,  iascii(243) /0/
   data symb(244) / 'lsyn  '/,  ivec(244) /  0/,  iascii(244) /0/
   data symb(245) / 'kssout'/,  ivec(245) /  0/,  iascii(245) /0/
   data symb(246) / 'loopss'/,  ivec(246) /  1/,  iascii(246) /0/
   data symb(247) / 'infexp'/,  ivec(247) /  0/,  iascii(247) /0/
   data symb(248) / 'numref'/,  ivec(248) /  0/,  iascii(248) /0/
   data symb(249) / 'nword1'/,  ivec(249) /  0/,  iascii(249) /0/
   data symb(250) / 'nword2'/,  ivec(250) /  0/,  iascii(250) /0/
   data symb(251) / 'iloaep'/,  ivec(251) /  0/,  iascii(251) /0/
   data symb(252) / 'lnpin '/,  ivec(252) /  0/,  iascii(252) /0/
   data symb(253) / 'ntot1 '/,  ivec(253) /  0/,  iascii(253) /0/
   data symb(254) / 'limstp'/,  ivec(254) /  0/,  iascii(254) /0/
   data symb(255) / 'indstp'/,  ivec(255) /  0/,  iascii(255) /0/
   data symb(256) / 'nc    '/,  ivec(256) /  0/,  iascii(256) /0/
   data symb(257) / 'unused'/,  ivec(257) /  0/,  iascii(257) /0/
   data symb(258) / 'unused'/,  ivec(258) /  0/,  iascii(258) /0/
   data symb(259) / 'icat  '/,  ivec(259) /  0/,  iascii(259) /0/
   data symb(260) / 'numnvo'/,  ivec(260) /  0/,  iascii(260) /0/
   data symb(261) / 'unused'/,  ivec(261) /  0/,  iascii(261) /0/
   data symb(262) / 'nenerg'/,  ivec(262) /  0/,  iascii(262) /0/
   data symb(263) / 'isw   '/,  ivec(263) /  0/,  iascii(263) /0/
   data symb(264) / 'itest '/,  ivec(264) /  0/,  iascii(264) /0/
   data symb(265) / 'idist '/,  ivec(265) /  0/,  iascii(265) /0/
   data symb(266) / 'x     '/,  ivec(266) /  1/,  iascii(266) /0/
   data symb(267) / 'ykm   '/,  ivec(267) /  1/,  iascii(267) /0/
   data symb(268) / 'km    '/,  ivec(268) /  1/,  iascii(268) /0/
   data symb(269) / 'xk    '/,  ivec(269) /  1/,  iascii(269) /0/
   data symb(270) / 'xm    '/,  ivec(270) /  1/,  iascii(270) /0/
   data symb(271) / 'weight'/,  ivec(271) /  1/,  iascii(271) /0/
   data symb(272) / 'iwtent'/,  ivec(272) /  1/,  iascii(272) /0/
   data symb(273) / 'con1  '/,  ivec(273) /  1/,  iascii(273) /0/
   data symb(274) / 'iskip '/,  ivec(274) /  1/,  iascii(274) /0/
   data symb(275) / 'zinf  '/,  ivec(275) /  1/,  iascii(275) /0/
   data symb(276) / 'eta   '/,  ivec(276) /  1/,  iascii(276) /0/
   data symb(277) / 'nhist '/,  ivec(277) /  1/,  iascii(277) /0/
   data symb(278) / 'stailm'/,  ivec(278) /  1/,  iascii(278) /0/
   data symb(279) / 'stailk'/,  ivec(279) /  1/,  iascii(279) /0/
   data symb(280) / 'xmax  '/,  ivec(280) /  1/,  iascii(280) /0/
   data symb(281) / 'koutvp'/,  ivec(281) /  1/,  iascii(281) /0/
   data symb(282) / 'bnrg  '/,  ivec(282) /  1/,  iascii(282) /0/
   data symb(283) / 'sconst'/,  ivec(283) /  1/,  iascii(283) /0/
   data symb(284) / 'cnvhst'/,  ivec(284) /  1/,  iascii(284) /0/
   data symb(285) / 'sfd   '/,  ivec(285) /  1/,  iascii(285) /0/
   data symb(286) / 'qfd   '/,  ivec(286) /  1/,  iascii(286) /0/
   data symb(287) / 'semaux'/,  ivec(287) /  1/,  iascii(287) /0/
   data symb(288) / 'ibsout'/,  ivec(288) /  1/,  iascii(288) /0/
   data symb(289) / 'bvalue'/,  ivec(289) /  1/,  iascii(289) /0/
   data symb(290) / 'sptacs'/,  ivec(290) /  1/,  iascii(290) /0/
   data symb(291) / 'kswtyp'/,  ivec(291) /  1/,  iascii(291) /0/
   data symb(292) / 'modswt'/,  ivec(292) /  1/,  iascii(292) /0/
   data symb(293) / 'kbegsw'/,  ivec(293) /  1/,  iascii(293) /0/
   data symb(294) / 'lastsw'/,  ivec(294) /  1/,  iascii(294) /0/
   data symb(295) / 'kentnb'/,  ivec(295) /  1/,  iascii(295) /0/
   data symb(296) / 'nbhdsw'/,  ivec(296) /  1/,  iascii(296) /0/
   data symb(297) / 'topen '/,  ivec(297) /  1/,  iascii(297) /0/
   data symb(298) / 'crit  '/,  ivec(298) /  1/,  iascii(298) /0/
   data symb(299) / 'kdepsw'/,  ivec(299) /  1/,  iascii(299) /0/
   data symb(300) / 'tdns  '/,  ivec(300) /  1/,  iascii(300) /0/
   data symb(301) / 'isourc'/,  ivec(301) /  1/,  iascii(301) /0/
   data symb(302) / 'energy'/,  ivec(302) /  1/,  iascii(302) /0/
   data symb(303) / 'iardub'/,  ivec(303) /  1/,  iascii(303) /0/
   data symb(304) / 'ardube'/,  ivec(304) /  1/,  iascii(304) /0/
   data symb(305) / 'nonlad'/,  ivec(305) /  1/,  iascii(305) /0/
   data symb(306) / 'nonle '/,  ivec(306) /  1/,  iascii(306) /0/
   data symb(307) / 'vnonl '/,  ivec(307) /  1/,  iascii(307) /0/
   data symb(308) / 'curr  '/,  ivec(308) /  1/,  iascii(308) /0/
   data symb(309) / 'anonl '/,  ivec(309) /  1/,  iascii(309) /0/
   data symb(310) / 'vecnl1'/,  ivec(310) /  1/,  iascii(310) /0/
   data symb(311) / 'vecnl2'/,  ivec(311) /  1/,  iascii(311) /0/
   data symb(312) / 'brnonl'/,  ivec(312) /  1/,  iascii(312) /1/
   data symb(313) / 'vzero '/,  ivec(313) /  1/,  iascii(313) /0/
   data symb(314) / 'ilast '/,  ivec(314) /  1/,  iascii(314) /0/
   data symb(315) / 'nltype'/,  ivec(315) /  1/,  iascii(315) /0/
   data symb(316) / 'kupl  '/,  ivec(316) /  1/,  iascii(316) /0/
   data symb(317) / 'nlsub '/,  ivec(317) /  1/,  iascii(317) /0/
   data symb(318) / 'cursub'/,  ivec(318) /  1/,  iascii(318) /0/
   data symb(319) / 'cchar '/,  ivec(319) /  1/,  iascii(319) /0/
   data symb(320) / 'vchar '/,  ivec(320) /  1/,  iascii(320) /0/
   data symb(321) / 'gslope'/,  ivec(321) /  1/,  iascii(321) /0/
   data symb(322) / 'kk    '/,  ivec(322) /  1/,  iascii(322) /0/
   data symb(323) / 'c     '/,  ivec(323) /  1/,  iascii(323) /0/
   data symb(324) / 'tr    '/,  ivec(324) /  1/,  iascii(324) /0/
   data symb(325) / 'tx    '/,  ivec(325) /  1/,  iascii(325) /0/
   data symb(326) / 'r     '/,  ivec(326) /  1/,  iascii(326) /0/
   data symb(327) / 'nr    '/,  ivec(327) /  1/,  iascii(327) /0/
   data symb(328) / 'length'/,  ivec(328) /  1/,  iascii(328) /0/
   data symb(329) / 'cik   '/,  ivec(329) /  1/,  iascii(329) /0/
   data symb(330) / 'ci    '/,  ivec(330) /  1/,  iascii(330) /0/
   data symb(331) / 'ck    '/,  ivec(331) /  1/,  iascii(331) /0/
   data symb(332) / 'swname'/,  ivec(332) /  1/,  iascii(332) /1/
   data symb(333) / 'ibrnch'/,  ivec(333) /  1/,  iascii(333) /0/
   data symb(334) / 'jbrnch'/,  ivec(334) /  1/,  iascii(334) /0/
   data symb(335) / 'tstop '/,  ivec(335) /  1/,  iascii(335) /0/
   data symb(336) / 'nonlk '/,  ivec(336) /  1/,  iascii(336) /0/
   data symb(337) / 'nonlm '/,  ivec(337) /  1/,  iascii(337) /0/
   data symb(338) / 'spum  '/,  ivec(338) /  1/,  iascii(338) /0/
   data symb(339) / 'kks   '/,  ivec(339) /  1/,  iascii(339) /0/
   data symb(340) / 'kknonl'/,  ivec(340) /  1/,  iascii(340) /0/
   data symb(341) / 'znonl '/,  ivec(341) /  1/,  iascii(341) /0/
   data symb(342) / 'znonlb'/,  ivec(342) /  1/,  iascii(342) /0/
   data symb(343) / 'znonlc'/,  ivec(343) /  1/,  iascii(343) /0/
   data symb(344) / 'finit '/,  ivec(344) /  1/,  iascii(344) /0/
   data symb(345) / 'ksub  '/,  ivec(345) /  1/,  iascii(345) /0/
   data symb(346) / 'msub  '/,  ivec(346) /  1/,  iascii(346) /0/
   data symb(347) / 'isubeg'/,  ivec(347) /  1/,  iascii(347) /0/
   data symb(348) / 'litype'/,  ivec(348) /  1/,  iascii(348) /0/
   data symb(349) / 'imodel'/,  ivec(349) /  1/,  iascii(349) /0/
   data symb(350) / 'kbus  '/,  ivec(350) /  1/,  iascii(350) /0/
   data symb(351) / 'mbus  '/,  ivec(351) /  1/,  iascii(351) /0/
   data symb(352) / 'kodebr'/,  ivec(352) /  1/,  iascii(352) /0/
   data symb(353) / 'cki   '/,  ivec(353) /  1/,  iascii(353) /0/
   data symb(354) / 'ckkjm '/,  ivec(354) /  1/,  iascii(354) /0/
   data symb(355) / 'indhst'/,  ivec(355) /  1/,  iascii(355) /0/
   data symb(356) / 'kodsem'/,  ivec(356) /  1/,  iascii(356) /0/
   data symb(357) / 'brname'/,  ivec(357) /  1/,  iascii(357) /1/
   data symb(358) / 'iform '/,  ivec(358) /  1/,  iascii(358) /0/
   data symb(359) / 'node  '/,  ivec(359) /  1/,  iascii(359) /0/
   data symb(360) / 'crest '/,  ivec(360) /  1/,  iascii(360) /0/
   data symb(361) / 'time1 '/,  ivec(361) /  1/,  iascii(361) /0/
   data symb(362) / 'time2 '/,  ivec(362) /  1/,  iascii(362) /0/
   data symb(363) / 'tstart'/,  ivec(363) /  1/,  iascii(363) /0/
   data symb(364) / 'sfreq '/,  ivec(364) /  1/,  iascii(364) /0/
   data symb(365) / 'kmswit'/,  ivec(365) /  1/,  iascii(365) /0/
   data symb(366) / 'nextsw'/,  ivec(366) /  1/,  iascii(366) /0/
   data symb(367) / 'rmfd  '/,  ivec(367) /  1/,  iascii(367) /0/
   data symb(368) / 'cikfd '/,  ivec(368) /  1/,  iascii(368) /0/
   data symb(369) / 'imfd  '/,  ivec(369) /  1/,  iascii(369) /0/
   data symb(370) / 'tclose'/,  ivec(370) /  1/,  iascii(370) /0/
   data symb(371) / 'adelay'/,  ivec(371) /  1/,  iascii(371) /0/
   data symb(372) / 'kpos  '/,  ivec(372) /  1/,  iascii(372) /0/
   data symb(373) / 'e     '/,  ivec(373) /  1/,  iascii(373) /0/
   data symb(374) / 'f     '/,  ivec(374) /  1/,  iascii(374) /0/
   data symb(375) / 'kssfrq'/,  ivec(375) /  1/,  iascii(375) /0/
   data symb(376) / 'kode  '/,  ivec(376) /  1/,  iascii(376) /0/
   data symb(377) / 'kpsour'/,  ivec(377) /  1/,  iascii(377) /0/
   data symb(378) / 'volti '/,  ivec(378) /  1/,  iascii(378) /0/
   data symb(379) / 'voltk '/,  ivec(379) /  1/,  iascii(379) /0/
   data symb(380) / 'volt  '/,  ivec(380) /  1/,  iascii(380) /0/
   data symb(381) / 'bus   '/,  ivec(381) /  1/,  iascii(381) /1/
   data symb(382) / 'eld   '/,  ivec(382) /  1/,  iascii(382) /0/
   data symb(383) / 'elaf  '/,  ivec(383) /  1/,  iascii(383) /0/
   data symb(384) / 'elf   '/,  ivec(384) /  1/,  iascii(384) /0/
   data symb(385) / 'elakd '/,  ivec(385) /  1/,  iascii(385) /0/
   data symb(386) / 'elfkd '/,  ivec(386) /  1/,  iascii(386) /0/
   data symb(387) / 'elkd  '/,  ivec(387) /  1/,  iascii(387) /0/
   data symb(388) / 'elq   '/,  ivec(388) /  1/,  iascii(388) /0/
   data symb(389) / 'elag  '/,  ivec(389) /  1/,  iascii(389) /0/
   data symb(390) / 'elg   '/,  ivec(390) /  1/,  iascii(390) /0/
   data symb(391) / 'elakq '/,  ivec(391) /  1/,  iascii(391) /0/
   data symb(392) / 'elgkq '/,  ivec(392) /  1/,  iascii(392) /0/
   data symb(393) / 'elkq  '/,  ivec(393) /  1/,  iascii(393) /0/
   data symb(394) / 'el0   '/,  ivec(394) /  1/,  iascii(394) /0/
   data symb(395) / 'ra    '/,  ivec(395) /  1/,  iascii(395) /0/
   data symb(396) / 'rf    '/,  ivec(396) /  1/,  iascii(396) /0/
   data symb(397) / 'rkd   '/,  ivec(397) /  1/,  iascii(397) /0/
   data symb(398) / 'rg    '/,  ivec(398) /  1/,  iascii(398) /0/
   data symb(399) / 'rkq   '/,  ivec(399) /  1/,  iascii(399) /0/
   data symb(400) / 'r0    '/,  ivec(400) /  1/,  iascii(400) /0/
   data symb(401) / 'agline'/,  ivec(401) /  1/,  iascii(401) /0/
   data symb(402) / 'rat1  '/,  ivec(402) /  1/,  iascii(402) /0/
   data symb(403) / 'smoutp'/,  ivec(403) /  1/,  iascii(403) /0/
   data symb(404) / 'smoutq'/,  ivec(404) /  1/,  iascii(404) /0/
   data symb(405) / 'teg   '/,  ivec(405) /  1/,  iascii(405) /0/
   data symb(406) / 'texc  '/,  ivec(406) /  1/,  iascii(406) /0/
   data symb(407) / 'cnp   '/,  ivec(407) /  1/,  iascii(407) /0/
   data symb(408) / 'a22   '/,  ivec(408) /  1/,  iascii(408) /0/
   data symb(409) / 'a12   '/,  ivec(409) /  1/,  iascii(409) /0/
   data symb(410) / 'a21   '/,  ivec(410) /  1/,  iascii(410) /0/
   data symb(411) / 'ac    '/,  ivec(411) /  1/,  iascii(411) /0/
   data symb(412) / 'ai    '/,  ivec(412) /  1/,  iascii(412) /0/
   data symb(413) / 'at    '/,  ivec(413) /  1/,  iascii(413) /0/
   data symb(414) / 'ah    '/,  ivec(414) /  1/,  iascii(414) /0/
   data symb(415) / 'xay   '/,  ivec(415) /  1/,  iascii(415) /0/
   data symb(416) / 'cu    '/,  ivec(416) /  1/,  iascii(416) /0/
   data symb(417) / 'cv    '/,  ivec(417) /  1/,  iascii(417) /0/
   data symb(418) / 'dsat  '/,  ivec(418) /  1/,  iascii(418) /0/
   data symb(419) / 'qsat  '/,  ivec(419) /  1/,  iascii(419) /0/
   data symb(420) / 'acr   '/,  ivec(420) /  1/,  iascii(420) /0/
   data symb(421) / 'ce    '/,  ivec(421) /  1/,  iascii(421) /0/
   data symb(422) / 'dsr   '/,  ivec(422) /  1/,  iascii(422) /0/
   data symb(423) / 'dsd   '/,  ivec(423) /  1/,  iascii(423) /0/
   data symb(424) / 'hico  '/,  ivec(424) /  1/,  iascii(424) /0/
   data symb(425) / 'dsm   '/,  ivec(425) /  1/,  iascii(425) /0/
   data symb(426) / 'hsp   '/,  ivec(426) /  1/,  iascii(426) /0/
   data symb(427) / 'power '/,  ivec(427) /  1/,  iascii(427) /0/
   data symb(428) / 'extrs '/,  ivec(428) /  1/,  iascii(428) /0/
   data symb(429) / 'histq '/,  ivec(429) /  1/,  iascii(429) /0/
   data symb(430) / 'histr '/,  ivec(430) /  1/,  iascii(430) /0/
   data symb(431) / 'yfor  '/,  ivec(431) /  1/,  iascii(431) /0/
   data symb(432) / 'zsk   '/,  ivec(432) /  1/,  iascii(432) /0/
   data symb(433) / 'y     '/,  ivec(433) /  1/,  iascii(433) /0/
   data symb(434) / 'tork  '/,  ivec(434) /  1/,  iascii(434) /0/
   data symb(435) / 'temp  '/,  ivec(435) /  1/,  iascii(435) /0/
   data symb(436) / 'z     '/,  ivec(436) /  1/,  iascii(436) /0/
   data symb(437) / 'x1    '/,  ivec(437) /  1/,  iascii(437) /0/
   data symb(438) / 'sqrt3 '/,  ivec(438) /  0/,  iascii(438) /0/
   data symb(439) / 'asqrt3'/,  ivec(439) /  0/,  iascii(439) /0/
   data symb(440) / 'sqrt32'/,  ivec(440) /  0/,  iascii(440) /0/
   data symb(441) / 'thtw  '/,  ivec(441) /  0/,  iascii(441) /0/
   data symb(442) / 'athtw '/,  ivec(442) /  0/,  iascii(442) /0/
   data symb(443) / 'radeg '/,  ivec(443) /  0/,  iascii(443) /0/
   data symb(444) / 'omdt  '/,  ivec(444) /  0/,  iascii(444) /0/
   data symb(445) / 'factom'/,  ivec(445) /  0/,  iascii(445) /0/
   data symb(446) / 'damrat'/,  ivec(446) /  0/,  iascii(446) /0/
   data symb(447) / 'isat  '/,  ivec(447) /  1/,  iascii(447) /0/
   data symb(448) / 'ised  '/,  ivec(448) /  1/,  iascii(448) /0/
   data symb(449) / 'iseq  '/,  ivec(449) /  1/,  iascii(449) /0/
   data symb(450) / 'imdual'/,  ivec(450) /  1/,  iascii(450) /0/
   data symb(451) / 'iconfg'/,  ivec(451) /  1/,  iascii(451) /0/
   data symb(452) / 'kmac  '/,  ivec(452) /  1/,  iascii(452) /0/
   data symb(453) / 'kexc  '/,  ivec(453) /  1/,  iascii(453) /0/
   data symb(454) / 'numas '/,  ivec(454) /  1/,  iascii(454) /0/
   data symb(455) / 'nodma '/,  ivec(455) /  1/,  iascii(455) /0/
   data symb(456) / 'nodmb '/,  ivec(456) /  1/,  iascii(456) /0/
   data symb(457) / 'nodmc '/,  ivec(457) /  1/,  iascii(457) /0/
   data symb(458) / 'jasmit'/,  ivec(458) /  1/,  iascii(458) /0/
   data symb(459) / 'jsmtor'/,  ivec(459) /  1/,  iascii(459) /0/
   data symb(460) / 'jexcit'/,  ivec(460) /  1/,  iascii(460) /0/
   data symb(461) / 'isloc '/,  ivec(461) /  1/,  iascii(461) /0/
   data symb(462) / 'noutsm'/,  ivec(462) /  1/,  iascii(462) /0/
   data symb(463) / 'ismout'/,  ivec(463) /  1/,  iascii(463) /0/
   data symb(464) / 'mfirst'/,  ivec(464) /  0/,  iascii(464) /0/
   data symb(465) / 'limass'/,  ivec(465) /  0/,  iascii(465) /0/
   data symb(466) / 'nst   '/,  ivec(466) /  0/,  iascii(466) /0/
   data symb(467) / 'itold '/,  ivec(467) /  0/,  iascii(467) /0/
   data symb(468) / 'ibrold'/,  ivec(468) /  0/,  iascii(468) /0/
   data symb(469) / 'busum '/,  ivec(469) /  1/,  iascii(469) /1/
   data symb(470) / 'ptheta'/,  ivec(470) /  1/,  iascii(470) /0/
   data symb(471) / 'zthevr'/,  ivec(471) /  1/,  iascii(471) /0/
   data symb(472) / 'vinp  '/,  ivec(472) /  1/,  iascii(472) /0/
   data symb(473) / 'zthevs'/,  ivec(473) /  1/,  iascii(473) /0/
   data symb(474) / 'umcur '/,  ivec(474) /  1/,  iascii(474) /0/
   data symb(475) / 'con   '/,  ivec(475) /  1/,  iascii(475) /0/
   data symb(476) / 'dumvec'/,  ivec(476) /  1/,  iascii(476) /0/
   data symb(477) / 'dummat'/,  ivec(477) /  1/,  iascii(477) /0/
   data symb(478) / 'date  '/,  ivec(478) /  1/,  iascii(478) /0/
   data symb(479) / 'clock '/,  ivec(479) /  1/,  iascii(479) /0/
   data symb(480) / 'pi    '/,  ivec(480) /  0/,  iascii(480) /0/
   data symb(481) / 'sroot2'/,  ivec(481) /  0/,  iascii(481) /0/
   data symb(482) / 'sroot3'/,  ivec(482) /  0/,  iascii(482) /0/
   data symb(483) / 'omegrf'/,  ivec(483) /  0/,  iascii(483) /0/
   data symb(484) / 'inpu  '/,  ivec(484) /  0/,  iascii(484) /0/
   data symb(485) / 'numbus'/,  ivec(485) /  0/,  iascii(485) /0/
   data symb(486) / 'ncltot'/,  ivec(486) /  0/,  iascii(486) /0/
   data symb(487) / 'ndum  '/,  ivec(487) /  1/,  iascii(487) /0/
   data symb(488) / 'initum'/,  ivec(488) /  0/,  iascii(488) /0/
   data symb(489) / 'iureac'/,  ivec(489) /  0/,  iascii(489) /0/
   data symb(490) / 'iugpar'/,  ivec(490) /  0/,  iascii(490) /0/
   data symb(491) / 'iufpar'/,  ivec(491) /  0/,  iascii(491) /0/
   data symb(492) / 'iuhist'/,  ivec(492) /  0/,  iascii(492) /0/
   data symb(493) / 'iuumrp'/,  ivec(493) /  0/,  iascii(493) /0/
   data symb(494) / 'iunod1'/,  ivec(494) /  0/,  iascii(494) /0/
   data symb(495) / 'iunod2'/,  ivec(495) /  0/,  iascii(495) /0/
   data symb(496) / 'iujclt'/,  ivec(496) /  0/,  iascii(496) /0/
   data symb(497) / 'iujclo'/,  ivec(497) /  0/,  iascii(497) /0/
   data symb(498) / 'iujtyp'/,  ivec(498) /  0/,  iascii(498) /0/
   data symb(499) / 'iunodo'/,  ivec(499) /  0/,  iascii(499) /0/
   data symb(500) / 'iujtmt'/,  ivec(500) /  0/,  iascii(500) /0/
   data symb(501) / 'iuhism'/,  ivec(501) /  0/,  iascii(501) /0/
   data symb(502) / 'iuomgm'/,  ivec(502) /  0/,  iascii(502) /0/
   data symb(503) / 'iuomld'/,  ivec(503) /  0/,  iascii(503) /0/
   data symb(504) / 'iutham'/,  ivec(504) /  0/,  iascii(504) /0/
   data symb(505) / 'iuredu'/,  ivec(505) /  0/,  iascii(505) /0/
   data symb(506) / 'iureds'/,  ivec(506) /  0/,  iascii(506) /0/
   data symb(507) / 'iuflds'/,  ivec(507) /  0/,  iascii(507) /0/
   data symb(508) / 'iufldr'/,  ivec(508) /  0/,  iascii(508) /0/
   data symb(509) / 'iurequ'/,  ivec(509) /  0/,  iascii(509) /0/
   data symb(510) / 'iuflqs'/,  ivec(510) /  0/,  iascii(510) /0/
   data symb(511) / 'iuflqr'/,  ivec(511) /  0/,  iascii(511) /0/
   data symb(512) / 'iujcds'/,  ivec(512) /  0/,  iascii(512) /0/
   data symb(513) / 'iujcqs'/,  ivec(513) /  0/,  iascii(513) /0/
   data symb(514) / 'iuflxd'/,  ivec(514) /  0/,  iascii(514) /0/
   data symb(515) / 'iuflxq'/,  ivec(515) /  0/,  iascii(515) /0/
   data symb(516) / 'iunppa'/,  ivec(516) /  0/,  iascii(516) /0/
   data symb(517) / 'iurotm'/,  ivec(517) /  0/,  iascii(517) /0/
   data symb(518) / 'iuncld'/,  ivec(518) /  0/,  iascii(518) /0/
   data symb(519) / 'iunclq'/,  ivec(519) /  0/,  iascii(519) /0/
   data symb(520) / 'iujtqo'/,  ivec(520) /  0/,  iascii(520) /0/
   data symb(521) / 'iujomo'/,  ivec(521) /  0/,  iascii(521) /0/
   data symb(522) / 'iujtho'/,  ivec(522) /  0/,  iascii(522) /0/
   data symb(523) / 'iureqs'/,  ivec(523) /  0/,  iascii(523) /0/
   data symb(524) / 'iuepso'/,  ivec(524) /  0/,  iascii(524) /0/
   data symb(525) / 'iudcoe'/,  ivec(525) /  0/,  iascii(525) /0/
   data symb(526) / 'iukcoi'/,  ivec(526) /  0/,  iascii(526) /0/
   data symb(527) / 'iuvolt'/,  ivec(527) /  0/,  iascii(527) /0/
   data symb(528) / 'iuangl'/,  ivec(528) /  0/,  iascii(528) /0/
   data symb(529) / 'iunodf'/,  ivec(529) /  0/,  iascii(529) /0/
   data symb(530) / 'iunodm'/,  ivec(530) /  0/,  iascii(530) /0/
   data symb(531) / 'iukumo'/,  ivec(531) /  0/,  iascii(531) /0/
   data symb(532) / 'iujumo'/,  ivec(532) /  0/,  iascii(532) /0/
   data symb(533) / 'iuumou'/,  ivec(533) /  0/,  iascii(533) /0/
   data symb(534) / 'nclfix'/,  ivec(534) /  0/,  iascii(534) /0/
   data symb(535) / 'numfix'/,  ivec(535) /  0/,  iascii(535) /0/
   data symb(536) / 'iotfix'/,  ivec(536) /  0/,  iascii(536) /0/
   data symb(537) / 'ibsfix'/,  ivec(537) /  0/,  iascii(537) /0/
   data symb(538) / 'ksubum'/,  ivec(538) /  0/,  iascii(538) /0/
   data symb(539) / 'nsmach'/,  ivec(539) /  0/,  iascii(539) /0/
   data symb(540) / 'istart'/,  ivec(540) /  0/,  iascii(540) /0/
   data symb(541) / 'karray'/,  ivec(541) /  1/,  iascii(541) /0/
   data symb(542) / 'rampcn'/,  ivec(542) /  1/,  iascii(542) /0/
   data symb(543) / 'rampsl'/,  ivec(543) /  1/,  iascii(543) /0/
   data symb(544) / 'kyramp'/,  ivec(544) /  1/,  iascii(544) /0/
   data symb(545) / 'texpar'/,  ivec(545) /  1/,  iascii(545) /0/
   data symb(546) / 'fendrp'/,  ivec(546) /  1/,  iascii(546) /0/
   data symb(547) / 'tminrp'/,  ivec(547) /  0/,  iascii(547) /0/
   data symb(548) / 'tmaxrp'/,  ivec(548) /  0/,  iascii(548) /0/
   data symb(549) / 'tbegrp'/,  ivec(549) /  1/,  iascii(549) /0/
   data symb(550) / 'tendrp'/,  ivec(550) /  1/,  iascii(550) /0/
   data symb(551) / 'fbegrp'/,  ivec(551) /  1/,  iascii(551) /0/
   data symb(552) / 'tbreak'/,  ivec(552) /  0/,  iascii(552) /0/
   data symb(553) / 'indxrp'/,  ivec(553) /  1/,  iascii(553) /0/
   data symb(554) / 'ivec  '/,  ivec(554) /  1/,  iascii(554) /0/
   data symb(555) / 'iascii'/,  ivec(555) /  1/,  iascii(555) /0/
   data symb(556) / 'numsym'/,  ivec(556) /  0/,  iascii(556) /0/
   data symb(557) / 'jjroll'/,  ivec(557) /  0/,  iascii(557) /0/
   data symb(558) / 'itexp '/,  ivec(558) /  0/,  iascii(558) /0/
   data symb(559) / 'labels'/,  ivec(559) /  1/,  iascii(559) /0/
   data symb(560) / 'maxarg'/,  ivec(560) /  0/,  iascii(560) /0/
   data symb(561) / 'kilper'/,  ivec(561) /  0/,  iascii(561) /0/
   data symb(562) / 'kfile5'/,  ivec(562) /  0/,  iascii(562) /0/
   data symb(563) / 'kverfy'/,  ivec(563) /  0/,  iascii(563) /0/
   data symb(564) / 'ibegcl'/,  ivec(564) /  0/,  iascii(564) /0/
   data symb(565) / 'iendcl'/,  ivec(565) /  0/,  iascii(565) /0/
   data symb(566) / 'lidnt1'/,  ivec(566) /  0/,  iascii(566) /0/
   data symb(567) / 'lidnt2'/,  ivec(567) /  0/,  iascii(567) /0/
   data symb(568) / 'linnow'/,  ivec(568) /  0/,  iascii(568) /0/
   data symb(569) / 'linspn'/,  ivec(569) /  0/,  iascii(569) /0/
   data symb(570) / 'numcrd'/,  ivec(570) /  0/,  iascii(570) /0/
   data symb(571) / 'munit5'/,  ivec(571) /  0/,  iascii(571) /0/
   data symb(572) / 'indbuf'/,  ivec(572) /  0/,  iascii(572) /0/
   data symb(573) / 'indbeg'/,  ivec(573) /  0/,  iascii(573) /0/
   data symb(574) / 'mflush'/,  ivec(574) /  0/,  iascii(574) /0/
   data symb(575) / 'newvec'/,  ivec(575) /  0/,  iascii(575) /0/
   data symb(576) / 'munit6'/,  ivec(576) /  0/,  iascii(576) /0/
   data symb(577) / 'lserlc'/,  ivec(577) /  0/,  iascii(577) /0/
   data symb(578) / 'kserlc'/,  ivec(578) /  0/,  iascii(578) /0/
   data symb(579) / 'kbrser'/,  ivec(579) /  0/,  iascii(579) /0/
   data symb(580) / 'lockbr'/,  ivec(580) /  0/,  iascii(580) /0/
   data symb(581) / 'iprspy'/,  ivec(581) /  0/,  iascii(581) /0/
   data symb(582) / 'monitr'/,  ivec(582) /  0/,  iascii(582) /0/
   data symb(583) / 'monits'/,  ivec(583) /  0/,  iascii(583) /0/
   data symb(584) / 'locate'/,  ivec(584) /  1/,  iascii(584) /0/
   data symb(585) / 'nline '/,  ivec(585) /  1/,  iascii(585) /0/
   data symb(586) / 'kwtspy'/,  ivec(586) /  0/,  iascii(586) /0/
   data symb(587) / 'kbreak'/,  ivec(587) /  0/,  iascii(587) /0/
   data symb(588) / 'limbuf'/,  ivec(588) /  0/,  iascii(588) /0/
   data symb(589) / 'inchlp'/,  ivec(589) /  0/,  iascii(589) /0/
   data symb(590) / 'ksymbl'/,  ivec(590) /  0/,  iascii(590) /0/
   data symb(591) / 'kopyit'/,  ivec(591) /  0/,  iascii(591) /0/
   data symb(592) / 'kslowr'/,  ivec(592) /  0/,  iascii(592) /0/
   data symb(593) / 'limcrd'/,  ivec(593) /  0/,  iascii(593) /0/
   data symb(594) / 'looprp'/,  ivec(594) /  1/,  iascii(594) /0/
   data symb(595) / 'n10rmp'/,  ivec(595) /  1/,  iascii(595) /0/
   data symb(596) / 'memrmp'/,  ivec(596) /  1/,  iascii(596) /0/
   data symb(597) / 'kar1  '/,  ivec(597) /  1/,  iascii(597) /0/
   data symb(598) / 'kar2  '/,  ivec(598) /  1/,  iascii(598) /0/
   data symb(599) / 'numrmp'/,  ivec(599) /  0/,  iascii(599) /0/
   data symb(600) / 'luntsp'/,  ivec(600) /  0/,  iascii(600) /0/
   data symb(601) / 'logvar'/,  ivec(601) /  0/,  iascii(601) /0/
   data symb(602) / 'filext'/,  ivec(602) /  1/,  iascii(602) /0/
   data symb(603) / 'symb  '/,  ivec(603) /  1/,  iascii(603) /0/
   data symb(604) / 'col   '/,  ivec(604) /  1/,  iascii(604) /0/
   data symb(605) / 'bytfnd'/,  ivec(605) /  0/,  iascii(605) /0/
   data symb(606) / 'char1 '/,  ivec(606) /  0/,  iascii(606) /0/
   data symb(607) / 'symbrp'/,  ivec(607) /  1/,  iascii(607) /0/
   data symb(608) / 'chard4'/,  ivec(608) /  0/,  iascii(608) /0/
   data symb(609) / 'bytbuf'/,  ivec(609) /  0/,  iascii(609) /0/
   data symb(610) / 'buff77'/,  ivec(610) /  0/,  iascii(610) /0/
   data symb(611) / 'file6b'/,  ivec(611) /  1/,  iascii(611) /0/
   data symb(612) / 'file6 '/,  ivec(612) /  1/,  iascii(612) /0/
   data symb(613) / 'blan80'/,  ivec(613) /  0/,  iascii(613) /0/
   data symb(614) / 'prom80'/,  ivec(614) /  0/,  iascii(614) /0/
   data symb(615) / 'digit '/,  ivec(615) /  1/,  iascii(615) /0/
   data symb(616) / 'iac   '/,  ivec(616) /  0/,  iascii(616) /0/
   data symb(617) / 'idctcs'/,  ivec(617) /  0/,  iascii(617) /0/
   data symb(618) / 'ipl   '/,  ivec(618) /  0/,  iascii(618) /0/
   data symb(619) / 'ipr   '/,  ivec(619) /  0/,  iascii(619) /0/
   data symb(620) / 'ixr   '/,  ivec(620) /  0/,  iascii(620) /0/
   data symb(621) / 'jpl   '/,  ivec(621) /  0/,  iascii(621) /0/
   data symb(622) / 'jpr   '/,  ivec(622) /  0/,  iascii(622) /0/
   data symb(623) / 'kint  '/,  ivec(623) /  0/,  iascii(623) /0/
   data symb(624) / 'kout  '/,  ivec(624) /  0/,  iascii(624) /0/
   data symb(625) / 'nds   '/,  ivec(625) /  0/,  iascii(625) /0/
   data symb(626) / 'nkn   '/,  ivec(626) /  0/,  iascii(626) /0/
   data symb(627) / 'nmax  '/,  ivec(627) /  0/,  iascii(627) /0/
   data symb(628) / 'nuk   '/,  ivec(628) /  0/,  iascii(628) /0/
   data symb(629) / 'kwrite'/,  ivec(629) /  0/,  iascii(629) /0/
   data symb(630) / 'kpr   '/,  ivec(630) /  0/,  iascii(630) /0/
   data symb(631) / 'kpl   '/,  ivec(631) /  0/,  iascii(631) /0/
   data symb(632) / 'mxtacw'/,  ivec(632) /  0/,  iascii(632) /0/
   data symb(633) / 'iptacw'/,  ivec(633) /  0/,  iascii(633) /0/
   data symb(634) / 'nhst  '/,  ivec(634) /  0/,  iascii(634) /0/
   data symb(635) / 'kvin  '/,  ivec(635) /  0/,  iascii(635) /0/
   data symb(636) / 'kvou  '/,  ivec(636) /  0/,  iascii(636) /0/
   data symb(637) / 'kvxx  '/,  ivec(637) /  0/,  iascii(637) /0/
   data symb(638) / 'icsup '/,  ivec(638) /  0/,  iascii(638) /0/
   data symb(639) / 'nxic  '/,  ivec(639) /  0/,  iascii(639) /0/
   data symb(640) / 'kksj  '/,  ivec(640) /  0/,  iascii(640) /0/
   data symb(641) / 'kksk  '/,  ivec(641) /  0/,  iascii(641) /0/
   data symb(642) / 'kkfst '/,  ivec(642) /  0/,  iascii(642) /0/
   data symb(643) / 'kkni  '/,  ivec(643) /  0/,  iascii(643) /0/
   data symb(644) / 'kkhst '/,  ivec(644) /  0/,  iascii(644) /0/
   data symb(645) / 'kifls '/,  ivec(645) /  0/,  iascii(645) /0/
   data symb(646) / 'kidum '/,  ivec(646) /  0/,  iascii(646) /0/
   data symb(647) / 'kslim1'/,  ivec(647) /  0/,  iascii(647) /0/
   data symb(648) / 'kslim2'/,  ivec(648) /  0/,  iascii(648) /0/
   data symb(649) / 'kslim3'/,  ivec(649) /  0/,  iascii(649) /0/
   data symb(650) / 'kpac1r'/,  ivec(650) /  0/,  iascii(650) /0/
   data symb(651) / 'kpac1i'/,  ivec(651) /  0/,  iascii(651) /0/
   data symb(652) / 'kpac2r'/,  ivec(652) /  0/,  iascii(652) /0/
   data symb(653) / 'kpac2i'/,  ivec(653) /  0/,  iascii(653) /0/
   data symb(654) / 'kalksx'/,  ivec(654) /  0/,  iascii(654) /0/
   data symb(655) / 'kilms1'/,  ivec(655) /  0/,  iascii(655) /0/
   data symb(656) / 'kilms2'/,  ivec(656) /  0/,  iascii(656) /0/
   data symb(657) / 'kdumj '/,  ivec(657) /  0/,  iascii(657) /0/
   data symb(658) / 'kdumk '/,  ivec(658) /  0/,  iascii(658) /0/
   data symb(659) / 'kkzj  '/,  ivec(659) /  0/,  iascii(659) /0/
   data symb(660) / 'kkzk  '/,  ivec(660) /  0/,  iascii(660) /0/
   data symb(661) / 'kiflz '/,  ivec(661) /  0/,  iascii(661) /0/
   data symb(662) / 'kgnz  '/,  ivec(662) /  0/,  iascii(662) /0/
   data symb(663) / 'kzlim1'/,  ivec(663) /  0/,  iascii(663) /0/
   data symb(664) / 'kzlim2'/,  ivec(664) /  0/,  iascii(664) /0/
   data symb(665) / 'kalkzx'/,  ivec(665) /  0/,  iascii(665) /0/
   data symb(666) / 'kilmz1'/,  ivec(666) /  0/,  iascii(666) /0/
   data symb(667) / 'kilmz2'/,  ivec(667) /  0/,  iascii(667) /0/
   data symb(668) / 'kksus '/,  ivec(668) /  0/,  iascii(668) /0/
   data symb(669) / 'kalksu'/,  ivec(669) /  0/,  iascii(669) /0/
   data symb(670) / 'kiuty '/,  ivec(670) /  0/,  iascii(670) /0/
   data symb(671) / 'kud1  '/,  ivec(671) /  0/,  iascii(671) /0/
   data symb(672) / 'kud2  '/,  ivec(672) /  0/,  iascii(672) /0/
   data symb(673) / 'kud3  '/,  ivec(673) /  0/,  iascii(673) /0/
   data symb(674) / 'kud4  '/,  ivec(674) /  0/,  iascii(674) /0/
   data symb(675) / 'kud5  '/,  ivec(675) /  0/,  iascii(675) /0/
   data symb(676) / 'kaliu '/,  ivec(676) /  0/,  iascii(676) /0/
   data symb(677) / 'ktysup'/,  ivec(677) /  0/,  iascii(677) /0/
   data symb(678) / 'kjsup '/,  ivec(678) /  0/,  iascii(678) /0/
   data symb(679) / 'kksup '/,  ivec(679) /  0/,  iascii(679) /0/
   data symb(680) / 'kspvar'/,  ivec(680) /  0/,  iascii(680) /0/
   data symb(681) / 'kopsup'/,  ivec(681) /  0/,  iascii(681) /0/
   data symb(682) / 'kfnsup'/,  ivec(682) /  0/,  iascii(682) /0/
   data symb(683) / 'krgsup'/,  ivec(683) /  0/,  iascii(683) /0/
   data symb(684) / 'kprsup'/,  ivec(684) /  0/,  iascii(684) /0/
   data symb(685) / 'ktypdv'/,  ivec(685) /  0/,  iascii(685) /0/
   data symb(686) / 'kkdj  '/,  ivec(686) /  0/,  iascii(686) /0/
   data symb(687) / 'kkdk  '/,  ivec(687) /  0/,  iascii(687) /0/
   data symb(688) / 'kgndev'/,  ivec(688) /  0/,  iascii(688) /0/
   data symb(689) / 'kdev1 '/,  ivec(689) /  0/,  iascii(689) /0/
   data symb(690) / 'kdev2 '/,  ivec(690) /  0/,  iascii(690) /0/
   data symb(691) / 'kldev1'/,  ivec(691) /  0/,  iascii(691) /0/
   data symb(692) / 'kldev2'/,  ivec(692) /  0/,  iascii(692) /0/
   data symb(693) / 'kkdus '/,  ivec(693) /  0/,  iascii(693) /0/
   data symb(694) / 'kalkdu'/,  ivec(694) /  0/,  iascii(694) /0/
   data symb(695) / 'ktbdev'/,  ivec(695) /  0/,  iascii(695) /0/
   data symb(696) / 'kpn   '/,  ivec(696) /  0/,  iascii(696) /0/
   data symb(697) / 'kpd   '/,  ivec(697) /  0/,  iascii(697) /0/
   data symb(698) / 'kxhst '/,  ivec(698) /  0/,  iascii(698) /0/
   data symb(699) / 'khscr '/,  ivec(699) /  0/,  iascii(699) /0/
   data symb(700) / 'khsci '/,  ivec(700) /  0/,  iascii(700) /0/
   data symb(701) / 'kilim1'/,  ivec(701) /  0/,  iascii(701) /0/
   data symb(702) / 'kilim2'/,  ivec(702) /  0/,  iascii(702) /0/
   data symb(703) / 'krowcs'/,  ivec(703) /  0/,  iascii(703) /0/
   data symb(704) / 'krhsde'/,  ivec(704) /  0/,  iascii(704) /0/
   data symb(705) / 'kvlim1'/,  ivec(705) /  0/,  iascii(705) /0/
   data symb(706) / 'kvlim2'/,  ivec(706) /  0/,  iascii(706) /0/
   data symb(707) / 'kkxic '/,  ivec(707) /  0/,  iascii(707) /0/
   data symb(708) / 'kawkcs'/,  ivec(708) /  0/,  iascii(708) /0/
   data symb(709) / 'kxar  '/,  ivec(709) /  0/,  iascii(709) /0/
   data symb(710) / 'kxai  '/,  ivec(710) /  0/,  iascii(710) /0/
   data symb(711) / 'kbwkcs'/,  ivec(711) /  0/,  iascii(711) /0/
   data symb(712) / 'kxtcs '/,  ivec(712) /  0/,  iascii(712) /0/
   data symb(713) / 'klntab'/,  ivec(713) /  0/,  iascii(713) /0/
   data symb(714) / 'klmxic'/,  ivec(714) /  0/,  iascii(714) /0/
   data symb(715) / 'kcolcs'/,  ivec(715) /  0/,  iascii(715) /0/
   data symb(716) / 'katcs '/,  ivec(716) /  0/,  iascii(716) /0/
   data symb(717) / 'kbtcs '/,  ivec(717) /  0/,  iascii(717) /0/
   data symb(718) / 'kjout '/,  ivec(718) /  0/,  iascii(718) /0/
   data symb(719) / 'kkout '/,  ivec(719) /  0/,  iascii(719) /0/
   data symb(720) / 'kxmncs'/,  ivec(720) /  0/,  iascii(720) /0/
   data symb(721) / 'ktxmn '/,  ivec(721) /  0/,  iascii(721) /0/
   data symb(722) / 'kxmxcs'/,  ivec(722) /  0/,  iascii(722) /0/
   data symb(723) / 'ktxmx '/,  ivec(723) /  0/,  iascii(723) /0/
   data symb(724) / 'klnout'/,  ivec(724) /  0/,  iascii(724) /0/
   data symb(725) / 'ekbuf '/,  ivec(725) /  1/,  iascii(725) /0/
   data symb(726) / 'ektemp'/,  ivec(726) /  1/,  iascii(726) /0/
   data symb(727) / 'errchk'/,  ivec(727) /  0/,  iascii(727) /0/
   data symb(728) / 'solrsv'/,  ivec(728) /  1/,  iascii(728) /0/
   data symb(729) / 'solisv'/,  ivec(729) /  1/,  iascii(729) /0/
   data symb(730) / 'nitera'/,  ivec(730) /  0/,  iascii(730) /0/
   data symb(731) / 'nekreq'/,  ivec(731) /  0/,  iascii(731) /0/
   data symb(732) / 'nekcod'/,  ivec(732) /  1/,  iascii(732) /0/
   data  numsym   / 732 /

   data texspy (   1 )  /  'key word no.  1:  "heading"     ----  ----  ----                                '  /
   data texspy (   2 )  /  '  response will be the printing of the previously-defined heading of  "examine",'  /
   data texspy (   3 )  /  '  followed by current values of all variables (as for the 1st  "examine"  use). '  /
   data texspy (   4 )  /  'key word no.  2:  "stop"        ----  ----  ----                                '  /
   data texspy (   5 )  /  '  this command will terminate interactive EMP execution immediately,  by  means '  /
   data texspy (   6 )  /  '  of a fortran "stop" statement.  There will be no automatic saving  of  tables,'  /
   data texspy (   7 )  /  '  or  of plot data points before such a termination  (use prior  "sleep"  and/or'  /
   data texspy (   8 )  /  '  "lunit4"  commands, if such preservation is desired).                         '  /
   data texspy (   9 )  /  'key word no.  3:  "plot"        ----  ----  ----                                '  /
   data texspy (  10 )  /  '  issue this command to transfer control to the  "outer:"  prompt of interactive'  /
   data texspy (  11 )  /  '  emtp plotting (the former separate EMTP crt plotting program  "tpplot").   due'  /
   data texspy (  12 )  /  '  to the absorbtion into spy,  several changes have been made.  first,  no plot-'  /
   data texspy (  13 )  /  '  file specification is required of the user  (he should subsequently send  "go"'  /
   data texspy (  14 )  /  '  if no other outer-level response is desired).   at the  "middle:"  level,  the'  /
   data texspy (  15 )  /  '  "timespan"  computation is now automatically performed,  internally.   at  any'  /
   data texspy (  16 )  /  '  level,  "stop"  no longer terminates program execution, but instead it returns'  /
   data texspy (  17 )  /  '  control to the  "spy:"  prompt.   continuous,  automatic plotting  of  the on-'  /
   data texspy (  18 )  /  '  going solution  (like a strip-chart) is  based  on the use of either  "rollc" '  /
   data texspy (  19 )  /  '  (for character plotting)  or  "rollv" (for vector plotting)  as  commands  at '  /
   data texspy (  20 )  /  '  the  "inner:"  level.   for  a  detailed explanation of all  "plot"  commands,'  /
   data texspy (  21 )  /  '  send  "help"  at any of the three levels within  "plot".                      '  /
   data texspy (  22 )  /  'key word no.  4:  "help"        ----  ----  ----                                '  /
   data texspy (  23 )  /  '  education for the ignorant, such as the user is now being subjected to (joke).'  /
   data texspy (  24 )  /  '  a  carriage return  <cr>  will  produce text for the next key word  in  order,'  /
   data texspy (  25 )  /  '  while  a  return  to the  "spy:"  prompt is accomplished by  "spy", or  "end".'  /
   data texspy (  26 )  /  '  "all"  will loop over  all  explanations  (subject  to  user-keyed interrupt).'  /
   data texspy (  27 )  /  '  sending  "top"  will rewind to the first message, while  "bot"  will give the '  /
   data texspy (  28 )  /  '  last.   use  "back"  to back up one message.                                  '  /
   data texspy (  29 )  /  'key word no.  5:  "examine"     ----  ----  ----                                '  /
   data texspy (  30 )  /  '  issue this command  to  examine the contents of any EMTP common variables  (of'  /
   data texspy (  31 )  /  '  solution overlays).   integer  scalars require 6 columns;  all other variables'  /
   data texspy (  32 )  /  '  require 15.   subsequent prompts willallow  the  user  to specify scalars and '  /
   data texspy (  33 )  /  '  vector ranges (e.g.,  "kbus(3:8)"  for  cells  3  through  8 of kbus).   "end"'  /
   data texspy (  34 )  /  '  terminates the list,  resulting  in adisplay of heading and numerical values. '  /
   data texspy (  35 )  /  '  any later striking of the  "return" key  will  then display current numerical '  /
   data texspy (  36 )  /  '  values only.   send  "heading"  for a  refresh of the variable names.  for  a '  /
   data texspy (  37 )  /  '  rolling display,  send  "roll"  (also,  see  separate  instructions  for  this'  /
   data texspy (  38 )  /  '  command).  for  rolling, the  output vector  is  re-formed at each spy chance,'  /
   data texspy (  39 )  /  '  but is only output when one or more variables has changed.  to  terminate  the'  /
   data texspy (  40 )  /  '  roll-loop,  use the regular user-keyed interrupt.                             '  /
   data texspy (  41 )  /  'key word no.  6:  "deposit"     ----  ----  ----                                '  /
   data texspy (  42 )  /  '  issue this command to modify the contents of any  EMTP  common  variables  (of'  /
   data texspy (  43 )  /  '  solution overlays).   subsequent prompts  will  permit  the  user  to  specify'  /
   data texspy (  44 )  /  '  scalars  and vector ranges  (e.g.,  "kbus(3:8)"  for  cells  3  through  8  of'  /
   data texspy (  45 )  /  '  kbus).   "end"  terminates the list, returning to the  "spy:"  prompt.  after '  /
   data texspy (  46 )  /  '  each  variable,  there will be a prompt for the desired new value (free-format'  /
   data texspy (  47 )  /  '  number),  if no  "="  is used.   the separately-prompted input  is  rigorously'  /
   data texspy (  48 )  /  '  free-format  (so  that any  i, f, or e-field number is permissible).  but  for'  /
   data texspy (  49 )  /  '  simple numeric values which  can  be read  using  f15.0,  or  for  text (a6), '  /
   data texspy (  50 )  /  '  follow the scalar or vector by an equal sign and then the number.  the case of'  /
   data texspy (  51 )  /  '  alphanumeric  must  not  have imbedded blanks after the  "=",  but numbers can'  /
   data texspy (  52 )  /  '  (within the span of 15 columns).                                              '  /
   data texspy (  53 )  /  'key word no.  7:  "switch"      ----  ----  ----                                '  /
   data texspy (  54 )  /  '  this response to the  "spy:"  prompt is  issued  for  a  display  of the EMTP '  /
   data texspy (  55 )  /  '  switch  table.   subsequently  send an  additional  "extra"  to change to the '  /
   data texspy (  56 )  /  '  next sub-table (as of february 1984, there are two),  if different columns are'  /
   data texspy (  57 )  /  '  desired.  within any one choice as to sub-table,  there is a loop in which the'  /
   data texspy (  58 )  /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy (  59 )  /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy (  60 )  /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage '  /
   data texspy (  61 )  /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy (  62 )  /  '  and return to the  "spy:"  prompt.  to refresh the heading, use either  "swit"'  /
   data texspy (  63 )  /  '  or  "head"  (only 4 characters of "switch" or "heading" are checked).         '  /
   data texspy (  64 )  /  'key word no.  8:  "append"      ----  ----  ----                                '  /
   data texspy (  65 )  /  '  this command is the gateway to installation-dependent commands which are non- '  /
   data texspy (  66 )  /  '  standard,  and which have been installed locally.   the  utpf carries a dummy '  /
   data texspy (  67 )  /  '  subroutine append,  by  definition. one clean  way  of adding a functional    '  /
   data texspy (  68 )  /  '  module is at translation time,  using a submod  request.                      '  /
   data texspy (  69 )  /  'key word no.  9:  "save"        ----  ----  ----                                '  /
   data texspy (  70 )  /  '  when the EMTP is in the time-step loop, this command will force an exit to    '  /
   data texspy (  71 )  /  '  overlay 20 upon completion of present time-step.   there,  "katalg"  saves    '  /
   data texspy (  72 )  /  '  EMTP tables as rapidly as possible (may not be permanent).   the simulation   '  /
   data texspy (  73 )  /  '  then recommences by an automatic transfer back to the beginning of the        '  /
   data texspy (  74 )  /  '  time-step loop ("over16").   since plot data points are not part of the       '  /
   data texspy (  75 )  /  '  saving, if the user wants these to be preserved, too, he must be careful.     '  /
   data texspy (  76 )  /  '  provided EMTP storage for plot data points does not fill up before the        '  /
   data texspy (  77 )  /  '  subsequent  restore  operation (to back up time to the  save  point), there   '  /
   data texspy (  78 )  /  '  is no problem.  otherwise, consider use of  "space"  and  "lunit4"  commands. '  /
   data texspy (  79 )  /  'key word no. 10:  "restore"     ----  ----  ----                                '  /
   data texspy (  80 )  /  '  the  "restore"  command is the reverse of  "save".  when within the deltat    '  /
   data texspy (  81 )  /  '  loop, use of  "restore"  will rapidly transfer to overlay 20.  there, module  '  /
   data texspy (  82 )  /  '  "katalg" restores former tables, after which control is transferred to spy.   '  /
   data texspy (  83 )  /  '  possibly the  "space"  and  "lunit4" would also be appropriate, to restore    '  /
   data texspy (  84 )  /  '  any separately plot files.   also after tables have been restored, the user   '  /
   data texspy (  85 )  /  '  can modify any EMTP variables  desired,  and exit  "deposit"  with  "end";    '  /
   data texspy (  86 )  /  '  finally, send  "go"  in response to the  "spy:"  prompt, to transfer back to  '  /
   data texspy (  87 )  /  '  the top of the time-step loop ("over16").                                     '  /
   data texspy (  88 )  /  'key word no. 11:  "go"          ----  ----  ----                                '  /
   data texspy (  89 )  /  '  this command is issued in response to the  "spy:"  prompt to terminate        '  /
   data texspy (  90 )  /  '  several sequences, such as one which might have begun with  "restore".  in    '  /
   data texspy (  91 )  /  '  this case, there would be the preceding transfers of control from overlay 16  '  /
   data texspy (  92 )  /  '  to overlay 20 ("katalg"), and then back to overlay 16 again.  as a second     '  /
   data texspy (  93 )  /  '  example,  "go"  cancels the  "rest" command.  third, it can be used to        '  /
   data texspy (  94 )  /  '  begin the EMTP solution following the  "data"  command (for data card input   '  /
   data texspy (  95 )  /  '  at the beginning of program execution).   finally,  "go"  cancels the         '  /
   data texspy (  96 )  /  '  suspension   of execution which accompanies the  "sleep"  command.            '  /
   data texspy (  97 )  /  'key word no. 12:  "echo"        ----  ----  ----                                '  /
   data texspy (  98 )  /  '  users who are interested in keeping a history of spy commands which are to    '  /
   data texspy (  99 )  /  '  be issued can use the  "echo"  command.   a subsequent prompt then allows     '  /
   data texspy ( 100 )  /  '  several choices.  to begin such accumulation, send  "begin";  to end it,      '  /
   data texspy ( 101 )  /  '  send  "file",  which will result in the dumping of all accumulation onto      '  /
   data texspy ( 102 )  /  '  disk as a permanent file of the users choice (a subsequent prompt will        '  /
   data texspy ( 103 )  /  '  ask for the desired file name).   the history consists of 80-column card      '  /
   data texspy ( 104 )  /  '  images,  stored from the bottom of  file6  upward  (with cell  kspsav         '  /
   data texspy ( 105 )  /  '  storing the last, and  limcrd  storing the first).  to view the accumulation  '  /
   data texspy ( 106 )  /  '  to date, use  "show".   when  "file" is used,  not only is a copy sent to     '  /
   data texspy ( 107 )  /  '  disk, but those in memory are erased. hence, if  echoing  is to continue,     '  /
   data texspy ( 108 )  /  '  the user must send  "begin"  again, immediately after  "file"  is complete.   '  /
   data texspy ( 109 )  /  '  in order to erase an erroneous, immediately-preceding command, use  "cancel"  '  /
   data texspy ( 110 )  /  '  at any point.  this is intercepted by the input routine  "flager",  so is     '  /
   data texspy ( 111 )  /  '  not a spy command per se.   there will be confirmation of the erasure.        '  /
   data texspy ( 112 )  /  'key word no. 13:  "find"        ----  ----  ----                                '  /
   data texspy ( 113 )  /  '  the sending of  "find"  will result in the printing of a heading, followed    '  /
   data texspy ( 114 )  /  '  by a pause, as  spy  waits for the user to supply a 6-character EMTP symbol   '  /
   data texspy ( 115 )  /  '  name.   this is a closed loop: after receiving a name,  spy  will display     '  /
   data texspy ( 116 )  /  '  the memory address, and then wait foranother such symbol.   exit by  "end".   '  /
   data texspy ( 117 )  /  '  wild cards ("*") of  vax/vms  are honored here, although the candidate        '  /
   data texspy ( 118 )  /  '  string is limited to eight characters maximum (only the first 8 are read).    '  /
   data texspy ( 119 )  /  'key word no. 14:  "list"        ----  ----  ----                                '  /
   data texspy ( 120 )  /  '  this command will result in the printing of a heading, followed by a pause.   '  /
   data texspy ( 121 )  /  '  at this point the user is in a loop, supplying row numbers (beginning and     '  /
   data texspy ( 122 )  /  '  ending rows as a pair of free-format integers).   spy responds to each with   '  /
   data texspy ( 123 )  /  '  a display of those rows of the spy symbol table, and then waits for the next  '  /
   data texspy ( 124 )  /  '  request.   the sending of  "0,0"  will break out, returning to the  "spy:"    '  /
   data texspy ( 125 )  /  '  prompt.   the user can interrupt any excessively long display with his keyed  '  /
   data texspy ( 126 )  /  '  interrupt (details depend upon computer).  instead of  "0,0"  to break out,   '  /
   data texspy ( 127 )  /  '  "end"  or  "spy:"  can alternatively be used.  sending nothing (just a        '  /
   data texspy ( 128 )  /  '  carriage return <cr>) is interpreted by spy as a request for "more of the     '  /
   data texspy ( 129 )  /  '  same" ---- the same number of rows as just displayed, beginning where the     '  /
   data texspy ( 130 )  /  '  last display left off.   special trickery is required if an argument of  "@"  '  /
   data texspy ( 131 )  /  '  usage is to respond to the  "list"  prompt,  since a comma must not be used   '  /
   data texspy ( 132 )  /  '  as the free-format separator (due to confusion with use of the same symbol    '  /
   data texspy ( 133 )  /  '  for argument separation by "@").  instead, a pounds sign  "#"  must be used   '  /
   data texspy ( 134 )  /  '  rather than a blank (due to extraction of blanks by  "@"  logic).  finally,   '  /
   data texspy ( 135 )  /  '  sending  "all"  instead of two row numbers displays the whole table.          '  /
   data texspy ( 136 )  /  'key word no. 15:  "spy"         ----  ----  ----                                '  /
   data texspy ( 137 )  /  '  this text, supplied almost anywhere that the program is looking for text,     '  /
   data texspy ( 138 )  /  '  will cause an internal interruption of whatever was happening, and a return   '  /
   data texspy ( 139 )  /  '  to the  "spy:"  prompt.   one exception is the  "help"  prompt which is now   '  /
   data texspy ( 140 )  /  '  being serviced.                                                               '  /
   data texspy ( 141 )  /  'key word no. 16:  "break"       ----  ----  ----                                '  /
   data texspy ( 142 )  /  '  this response to the  "spy:"  prompt is appropriate if the user wants the     '  /
   data texspy ( 143 )  /  '  simulation to continue uninterrupted until some pre-specified time, when a    '  /
   data texspy ( 144 )  /  '  clean break at the start of "subts1" will be made.  a subsequent prompt will  '  /
   data texspy ( 145 )  /  '  allow the user to specify the future break time  t-break  in seconds.  if a   '  /
   data texspy ( 146 )  /  '  minus sign is appended, then the input is taken to be a step number of the    '  /
   data texspy ( 147 )  /  '  time-step loop, and the program will calculate  t-break   by multiplying by   '  /
   data texspy ( 148 )  /  '  deltat.  oh, in case "subts1" means nothing to the user, this is the first of '  /
   data texspy ( 149 )  /  '  four pieces of overlay 16 (the time-step loop).  in case the user sends zero  '  /
   data texspy ( 150 )  /  '  for  t-break, an added prompt will seek clarification as to which utpf overlay'  /
   data texspy ( 151 )  /  '  is the desired stopping point (immediately prior to entry).  common usage     '  /
   data texspy ( 152 )  /  '  involves overlays numbered 6  (after all sources have been read),  12  (when  '  /
   data texspy ( 153 )  /  '  the   phasor solution is complete), or  16  (the time-step loop).             '  /
   data texspy ( 154 )  /  'key word no. 17:  "when"        ----  ----  ----                                '  /
   data texspy ( 155 )  /  '  this response to the  "spy:"  prompt will redefine the EMTP table-handling    '  /
   data texspy ( 156 )  /  '  time of  "save"  and  "restore" .   that is,  if so defined,  this overrides  '  /
   data texspy ( 157 )  /  '  the otherwise immediate exit of the time-step loop for table-handling.        '  /
   data texspy ( 158 )  /  'key word no. 18:  "comment"     ----  ----  ----                                '  /
   data texspy ( 159 )  /  '  this response to the  "spy:"  prompt will toggle the switch which controls the'  /
   data texspy ( 160 )  /  '  printing of comment cards ("c ") which may be contained within command files. '  /
   data texspy ( 161 )  /  '  the default (starting value) is to show comment cards during execution ("@"). '  /
   data texspy ( 162 )  /  'key word no. 19:  "@?"          ----  ----  ----                                '  /
   data texspy ( 163 )  /  '  this response to the  "spy:"  prompt will result in the internal  opening  of '  /
   data texspy ( 164 )  /  '  an arbitary disk file, and the connection of this file as replacement for     '  /
   data texspy ( 165 )  /  '  keyboard input to spy.  if the file name consists of just a single digit      '  /
   data texspy ( 166 )  /  '  (?=1-9),  inclspy?.dat  is the disk file name to be used.  reading from the   '  /
   data texspy ( 167 )  /  '  disk file continues until an end-of-file is hit, at which point the spy input '  /
   data texspy ( 168 )  /  '  channel is connected once again to the keyboard.  such usage can not be nested'  /
   data texspy ( 169 )  /  '  (i.e., no such disk file can itself contain an  "@"  statement).   EMTP       '  /
   data texspy ( 170 )  /  '  comment cards ("c ") are permitted within such disk files, however,  with the '  /
   data texspy ( 171 )  /  '  "comment"  switch controlling whether or not they are written to the screen   '  /
   data texspy ( 172 )  /  '  (the default is for such writing).other lines of an executed disk file are    '  /
   data texspy ( 173 )  /  '  generally not seen by the user as they are read during execution, and neither '  /
   data texspy ( 174 )  /  '  are the repetitive  "spy:"  prompts. parameters are possible, to substitute   '  /
   data texspy ( 175 )  /  '  for 8-column   "%%%%%%%%"   fields of the disk file.   blanks are ignored,    '  /
   data texspy ( 176 )  /  '  and arguments are to be separated by commas.  an opening parenthesis  "("  and'  /
   data texspy ( 177 )  /  '  a closing parenthesis  ")"  are optional delimiters.   each command line is   '  /
   data texspy ( 178 )  /  '  limited to 80 columns (no continuation), and a maximum of 10 arguments.   the '  /
   data texspy ( 179 )  /  '  left-to-right list is applied to the file %-fields from top to bottom, in     '  /
   data texspy ( 180 )  /  '  order.  the %-fields can be built into any line of the disk file which        '  /
   data texspy ( 181 )  /  '  is read by  spy  proper (not plotting).   finally, there is the use of a pound'  /
   data texspy ( 182 )  /  '  sign "#" for reserved blanks, which are otherwise ignored.   the classic case '  /
   data texspy ( 183 )  /  '  where it is needed is for a response to  "list"  (e.g.,  "2#4");   this is    '  /
   data texspy ( 184 )  /  '  free-format information, with a comma impossible due to the conflict with "@".'  /
   data texspy ( 185 )  /  '  although not a part of the interactive  spy  code per se, it should also be   '  /
   data texspy ( 186 )  /  '  remembered that commands which could be built into  "@"  files can also be    '  /
   data texspy ( 187 )  /  '  executed as part of the regular EMTP data.  the key to such usage is  "$spy", '  /
   data texspy ( 188 )  /  '  which is processed by  "cimage"  and/or  "erexit".   if all spy commands are  '  /
   data texspy ( 189 )  /  '  to be built in-line as part of the lunit5 EMTP input data, then precede such  '  /
   data texspy ( 190 )  /  '  data by a card reading  "$spy",  with columns 5 onward blank.  in this        '  /
   data texspy ( 191 )  /  '  case,  "erexit"  reads and removes such data cards, creating special reserved '  /
   data texspy ( 192 )  /  '  disk files named  spyfile?.dat,  where  "?"  is a single digit between one    '  /
   data texspy ( 193 )  /  '  and nine (allowing a maximum of nine such distinct groups of spy commands).   '  /
   data texspy ( 194 )  /  '  the last spy command of each such group is to be followed by  "$endspy"  as a '  /
   data texspy ( 195 )  /  '  special marker record.  on the other hand, if the user does not care about    '  /
   data texspy ( 196 )  /  '  unifying all such EMTP data in a single disk file, then a single line of EMTP '  /
   data texspy ( 197 )  /  '  data,  "$spy, filename",  is all that is required to provide the connection to'  /
   data texspy ( 198 )  /  '  spy.  in this second card,  filename can be any legal, full file name of the  '  /
   data texspy ( 199 )  /  '  computer system being considered (if the user supplies the file name, and     '  /
   data texspy ( 200 )  /  '  puts commands in the disk file, then there is no limit on the number of       '  /
   data texspy ( 201 )  /  '  such usages.   the role of  "cimage" is to treat  "$spy"  like   "$include".  '  /
   data texspy ( 202 )  /  'key word no. 20:  "roll"        ----  ----  ----                                '  /
   data texspy ( 203 )  /  '  this response to the  "spy:"  prompt will result in a "rolling" of previously-'  /
   data texspy ( 204 )  /  '  defined  "examine"  request.   while this happens, no other spy activity is   '  /
   data texspy ( 205 )  /  '  permitted (any <cr> will abort the loop, and return to the  "spy:"  prompt).  '  /
   data texspy ( 206 )  /  '  following  "roll",  the simulation will recommence, with spy called at each   '  /
   data texspy ( 207 )  /  '  opportunity for a spy break (within frequency  maxflg  of  "dekspy",  which   '  /
   data texspy ( 208 )  /  '  nominally has the value one, implying four checks per time step).   the very  '  /
   data texspy ( 209 )  /  '  first check, the heading and current value of the  "examine"  vector will be  '  /
   data texspy ( 210 )  /  '  displayed;  thereafter, output will be produced if and only if a change in    '  /
   data texspy ( 211 )  /  '  the output (compared with the previous evaluation) has occurred.              '  /
   data texspy ( 212 )  /  '  a final thought is about other commands which function much like the  "roll"  '  /
   data texspy ( 213 )  /  '  commands, although in fact these are not issued at the  "spy:" level.  for    '  /
   data texspy ( 214 )  /  '  "plot"  use,  at the  "inner:"  level,  "rollc"  will produce continuous      '  /
   data texspy ( 215 )  /  '  character plotting, while  "rollv"  does the same for vector plotting.   it   '  /
   data texspy ( 216 )  /  '  is the  "noroll"  command of spy which will cancel either or both of these.   '  /
   data texspy ( 217 )  /  'key word no. 21:  "type?"       ----  ----  ----                                '  /
   data texspy ( 218 )  /  '  this response to the  "spy:"  prompt will result in the listing of command    '  /
   data texspy ( 219 )  /  '  file  inclspy?.dat  of  "@?"  usage. for any specific numerical digit         '  /
   data texspy ( 220 )  /  '  "?",  just that one file will be listed.   but if symbolic "?" is retained, or'  /
   data texspy ( 221 )  /  '  if column 5 onward is blank, then all possible  "@?"  files will be listed    '  /
   data texspy ( 222 )  /  '  in natural order, preceded by an identifying heading.  also, for arbitrary    '  /
   data texspy ( 223 )  /  '  disk files of card images with names of 2 or more characters, this command    '  /
   data texspy ( 224 )  /  '  will display them.  "type filename" is the form of this more general          '  /
   data texspy ( 225 )  /  '  command, where  filename  is any legal file name of 32 or fewer characters.   '  /
   data texspy ( 226 )  /  '  if no such file exists, there will be a warning message, so this command      '  /
   data texspy ( 227 )  /  '  can be used to check on the existence of arbitary disk files of card images.  '  /
   data texspy ( 228 )  /  'key word no. 22:  "verify"      ----  ----  ----                                '  /
   data texspy ( 229 )  /  '  this response to the  "spy:"  prompt will toggle the switch that controls the '  /
   data texspy ( 230 )  /  '  echoing of data cards within a disk file which is read via  "@"  usage.  the  '  /
   data texspy ( 231 )  /  '  default (beginning) setting is to have such echoing.   a related command is   '  /
   data texspy ( 232 )  /  '  "comment",  which can separately control the display of comment ("c ") cards  '  /
   data texspy ( 233 )  /  '  as they are encountered during processing of the  "@"  file.  if there is no  '  /
   data texspy ( 234 )  /  '  echoing of spy data, then comment cards might likewise not be displayed;  or, '  /
   data texspy ( 235 )  /  '  they can be used as an absolute reference (if they are displayed), to mark the'  /
   data texspy ( 236 )  /  '  beginning or ending of invisible operations due to no-echoing of "verify".    '  /
   data texspy ( 237 )  /  'key word no. 23:  "files"       ----  ----  ----                                '  /
   data texspy ( 238 )  /  '  this  response  to  the  "spy:"  prompt will result in a display of all of the'  /
   data texspy ( 239 )  /  '  inclspy?.dat  files which exist,  based on the fortran "inquire" operation  at'  /
   data texspy ( 240 )  /  '  the time program execution began.  an "x"  means that the file exists (in the '  /
   data texspy ( 241 )  /  '  display),  whereas a blank means that it does not.  there are 9 columns.      '  /
   data texspy ( 242 )  /  'key word no. 24:  "sleep"       ----  ----  ----                                '  /
   data texspy ( 243 )  /  '  this response to the  "spy:"  prompt will put the EMTP to sleep in such a way '  /
   data texspy ( 244 )  /  '  that the simulation can be continued at any later time.  the interactive      '  /
   data texspy ( 245 )  /  '  "sleep"  command is comparable to the batch-mode use of miscellaneous data    '  /
   data texspy ( 246 )  /  '  parameter   memsav = 1   for the saving of EMTP tables on disk.  to service a '  /
   data texspy ( 247 )  /  '  "sleep"  command, spy exits the time-step loop and jumps to  "over20"  for    '  /
   data texspy ( 248 )  /  '  table dumping to disk.  subsequent awakening is via  "wake",  which reads     '  /
   data texspy ( 249 )  /  '  EMTP tables back from disk into EMTP memory.  any plot data must be separately'  /
   data texspy ( 250 )  /  '  and manually provided for by the user (using  "space", "lunit4"),  if it, too,'  /
   data texspy ( 251 )  /  '  is to be permanently saved.                                                   '  /
   data texspy ( 252 )  /  'key word no. 25:  "source"      ----  ----  ----                                '  /
   data texspy ( 253 )  /  '  this response to the  "spy:"  prompt will allow the user to look at either    '  /
   data texspy ( 254 )  /  '  the electric network or the tacs source table.   there will be a pause after  '  /
   data texspy ( 255 )  /  '  spy receives  "source",  as it waits to receive either  "tacs"  or  "elec"  as'  /
   data texspy ( 256 )  /  '  an indication of source-table choice. then  spy  waits for a pair of free-    '  /
   data texspy ( 257 )  /  '  format integer row numbers, to define the limits of the table display.   a few'  /
   data texspy ( 258 )  /  '  key words are also accepted here:  "all"  to display entire table,  "end"  or '  /
   data texspy ( 259 )  /  '  "stop"  or  "spy"  to return to  "spy:"  prompt,  and  "tacs"  or  "elec"  to '  /
   data texspy ( 260 )  /  '  produce a new table heading (or switch between the two tables).   the tacs    '  /
   data texspy ( 261 )  /  '  table displays offsets of  "sptacs" which are used with  "deposit/examine",   '  /
   data texspy ( 262 )  /  '  in case the user wants to redefine such quantities.                           '  /
   data texspy ( 263 )  /  'key word no. 26:  "edit"        ----  ----  ----                                '  /
   data texspy ( 264 )  /  '  this response to the  "spy:"  prompt will allow the user to examine and modify'  /
   data texspy ( 265 )  /  '  card images which are currently stored in memory (see the  "data"  command).  '  /
   data texspy ( 266 )  /  '  a  "*"  prompt will next appear, at which point vax sos-like editing commands '  /
   data texspy ( 267 )  /  '  can be issued.  the only bothersome changes of notation are the use of  "#"   '  /
   data texspy ( 268 )  /  '  in place of vax"s  "!"  (because of in-line comment problems with e/ts),  and '  /
   data texspy ( 269 )  /  '  use of  "@"  in place of vax"s  "esc" key as a character string delimiter.    '  /
   data texspy ( 270 )  /  '  for an explanation of sos editing rules, see dec vax-11 user documentation.   '  /
   data texspy ( 271 )  /  '  examples of printing include:  "*p^:*",  "*p5",  "*p^:18",  "*p5#10",  "*p",  '  /
   data texspy ( 272 )  /  '  and  "p.-20:."   there also is  "*f" usage,  only with  "@"  replacing  <esc> '  /
   data texspy ( 273 )  /  '  of  vax/vms  sos.  to exit the  "edit"  command and return to the  "spy:"     '  /
   data texspy ( 274 )  /  '  prompt, use  "*e"  (analogous to the sos exit).  additional user commands     '  /
   data texspy ( 275 )  /  '  include  "*d"  (for deletion of lines),  "*r"  (for replacement of lines),    '  /
   data texspy ( 276 )  /  '  "*i"  (for insertion of new lines), and  "*s"  (for the substitution of one   '  /
   data texspy ( 277 )  /  '  character string by another).  concerning  "*s",  however, once again  "@"    '  /
   data texspy ( 278 )  /  '  is used as a delimiter rather than  <esc>,  and no qualifiers (e.g., ",d" for '  /
   data texspy ( 279 )  /  '  "decide mode") are allowed.   finally, there are special EMTP-designed        '  /
   data texspy ( 280 )  /  '  commands.   the first of these is  "(8)",  which initiates a search for the   '  /
   data texspy ( 281 )  /  '  next card which has a non-blank column 80.   after display of this record,    '  /
   data texspy ( 282 )  /  '  spy awaits a user decision regarding disposition:  <cr>  will leave the card  '  /
   data texspy ( 283 )  /  '  unchanged and initiate a search for the following one;  digits 0, 1, 2, 3, 4  '  /
   data texspy ( 284 )  /  '  will result in the punching of this value into column 80 before searching for '  /
   data texspy ( 285 )  /  '  the next such record (with "0" internally changed to a blank before punching).'  /
   data texspy ( 286 )  /  '  the  "*"  prompt will reappear automatically when the search hits the bottom  '  /
   data texspy ( 287 )  /  '  of the file.  or,  "*"  can be reached at any point of the search-display loop'  /
   data texspy ( 288 )  /  '  by sending  "e" .   even if not in the  "(8)"  loop, column-80 deposits are   '  /
   data texspy ( 289 )  /  '  possible by use of  "(8),?"  where  "?"  is the desired col.-80 content of the'  /
   data texspy ( 290 )  /  '  current line.   finally,  the command "*col"  will produce a heading of       '  /
   data texspy ( 291 )  /  '  column numbers, which is useful when inserting new data records using  "*i" . '  /
   data texspy ( 292 )  /  '  this ruler heading will be shifted to line up with  "*p"  displays.   if an   '  /
   data texspy ( 293 )  /  '  unshifted display is desired,  use  "*col8" .                                 '  /
   data texspy ( 294 )  /  'key word no. 27:  "wake"        ----  ----  ----                                '  /
   data texspy ( 295 )  /  '  this response to the  "spy:"  prompt will awaken a hibernating solution (one  '  /
   data texspy ( 296 )  /  '  which was put to bed with an earlier "sleep"  command).   altered program     '  /
   data texspy ( 297 )  /  '  dimensions are not allowed  (both programs must be dimensioned identically),  '  /
   data texspy ( 298 )  /  '  just as with the batch-mode  "start again".  the  "wake"  command is the exact'  /
   data texspy ( 299 )  /  '  interactive equivalent of the batch-mode request  "start again".  there is a  '  /
   data texspy ( 300 )  /  '  related  "wake4"  command for regeneration of the lunit4 plot-file header     '  /
   data texspy ( 301 )  /  '  information, in case this is wanted by the user (it is in fact needed, if the '  /
   data texspy ( 302 )  /  '  "plot"  command is to be used, as of 26 feb 1984).                            '  /
   data texspy ( 303 )  /  'key word no. 28:  "language"    ----  ----  ----                                '  /
   data texspy ( 304 )  /  '  this response to the  "spy:"  prompt allows the user to either examine or     '  /
   data texspy ( 305 )  /  '  modify "spy:"-level command words (e.g., "plot", "data", etc.).  a loop will  '  /
   data texspy ( 306 )  /  '  be entered, in which there are only 4 legal responses  ("single",  "entire",  '  /
   data texspy ( 307 )  /  '  "show",  and  "spy")  to the prompt. sending  "single"  will lead to an       '  /
   data texspy ( 308 )  /  '  inner input loop in which old and new symbol pairs are to be redefined one at '  /
   data texspy ( 309 )  /  '  a time, terminated by  "end"  (to return to outer  "language"  loop) or  "spy"'  /
   data texspy ( 310 )  /  '  (to return to the  "spy:"  prompt).  the second outer response,  "entire",    '  /
   data texspy ( 311 )  /  '  must be followed by an unabridged dictionary of symbols using 10a8 format.    '  /
   data texspy ( 312 )  /  '  sending the third response  "show"  will result in an unabridged display of   '  /
   data texspy ( 313 )  /  '  all such current commands.  finally, sending  "spy"  will exit the outer loop,'  /
   data texspy ( 314 )  /  '  and return to the  "spy:"  prompt.  the language of  "plot"  usage does not   '  /
   data texspy ( 315 )  /  '  occur at the command level with prompt  "spy:",  so it can only be redefined  '  /
   data texspy ( 316 )  /  '  within that utility (by means of the "set data"  command).                    '  /
   data texspy ( 317 )  /  'key word no. 29:  "catalog"     ----  ----  ----                                '  /
   data texspy ( 318 )  /  '  this response to the  "spy:"  prompt will create a new permanent disk file, it'  /
   data texspy ( 319 )  /  '  will dump a copy of the data case which is presently contained within the     '  /
   data texspy ( 320 )  /  '  EMTP into that disk file.   the name of this new disk file is user-supplied   '  /
   data texspy ( 321 )  /  '  (in response to a subsequent prompt for such a name, which must, naturally, be'  /
   data texspy ( 322 )  /  '  legal for the computer system being used).   the EMTP data case in question   '  /
   data texspy ( 323 )  /  '  generally will differ from that originally read in using the  "data"  command,'  /
   data texspy ( 324 )  /  '  of course  (assuming  "edit"  operations have produced alterations).          '  /
   data texspy ( 325 )  /  'key word no. 30:  "begin"       ----  ----  ----                                '  /
   data texspy ( 326 )  /  '  this response to the  "spy:"  prompt will abort the current EMTP solution,    '  /
   data texspy ( 327 )  /  '  and initiate a complete new solution. it is the simulator-stored EMTP data    '  /
   data texspy ( 328 )  /  '  cards which are used as input for the new solution, and these will usually    '  /
   data texspy ( 329 )  /  '  have just been modified using  "edit" operations.  the transition is          '  /
   data texspy ( 330 )  /  '  "instantaneous" for cases which have been put to bed via  "rest"  or  "sleep",'  /
   data texspy ( 331 )  /  '  or which are cycling the time-step loop.   for execution in earlier overlays, '  /
   data texspy ( 332 )  /  '  there may be a delay until the present overlay is exited.                     '  /
   data texspy ( 333 )  /  'key word no. 31:  "step"        ----  ----  ----                                '  /
   data texspy ( 334 )  /  '  this response to the  "spy:"  prompt represents a request to toggle the binary'  /
   data texspy ( 335 )  /  '  switch that  forces  a  spy  interrupt at each and every possible opportunity.'  /
   data texspy ( 336 )  /  '  either  there  are  such  ever-present forced interrupts, or  there  are none.'  /
   data texspy ( 337 )  /  '  interrupt opportunities exist at the start of each overlay,  at  the start  of'  /
   data texspy ( 338 )  /  '  each of the four pieces of overlay 16 (the time-step loop),  and  finally, as '  /
   data texspy ( 339 )  /  '  each new data card is read by input module  "cimage".                         '  /
   data texspy ( 340 )  /  'key word no. 32:  "debug"       ----  ----  ----                                '  /
   data texspy ( 341 )  /  '  this response to the  "spy:"  prompt is used only for debugging of interactive'  /
   data texspy ( 342 )  /  '  EMTP execution itself, in case of faulty operation.   it is not used for cases'  /
   data texspy ( 343 )  /  '  of suspected EMTP error (do not confuse with  "iprsup"  of the EMTP).  anyway,'  /
   data texspy ( 344 )  /  '  there will be a subsequent prompt for diagnostic control variable  iprspy.    '  /
   data texspy ( 345 )  /  'key word no. 33:  "data"       ----  ----  ----                                 '  /
   data texspy ( 346 )  /  '  this response to the  "spy:"  prompt is appropriate when the user wants to    '  /
   data texspy ( 347 )  /  '  specify a disk file of EMTP data which is to be solved next.  the program     '  /
   data texspy ( 348 )  /  '  will then prompt for the desired file name, which will generally be computer- '  /
   data texspy ( 349 )  /  '  dependent.  a full, legal file name for the computer being used must then be  '  /
   data texspy ( 350 )  /  '  provided.  before giving the real file name, however, the user could send     '  /
   data texspy ( 351 )  /  '  "control",  should he want to input only a portion of the disk file, or should'  /
   data texspy ( 352 )  /  '  it be desired to have the data placed in the EMTP cache with an offset, or    '  /
   data texspy ( 353 )  /  '  should it be desired to have the  lunit5  usage pointer set to other than the '  /
   data texspy ( 354 )  /  '  default value of unity (offset zero). a second non-file response is  "spy",   '  /
   data texspy ( 355 )  /  '  to abort the file-name prompt after the  "control"  definitions (in case the  '  /
   data texspy ( 356 )  /  '  user only wants to redefine the  lunit5  usage pointer, for example).   a     '  /
   data texspy ( 357 )  /  '  final point concerns the avoidance of non-existent files (which would result  '  /
   data texspy ( 358 )  /  '  in an error stop of the EMTP by the operating system).  if in doubt about a   '  /
   data texspy ( 359 )  /  '  file"s existence, try to  type  it first (the  "type"  command warns of non-  '  /
   data texspy ( 360 )  /  '  existent files, without any termination of execution).                        '  /
   data texspy ( 361 )  /  'key word no. 34:  "ramp"        ----  ----  ----                                '  /
   data texspy ( 362 )  /  '  this response to the  "spy:"  prompt will result in prompts which allow the   '  /
   data texspy ( 363 )  /  '  user to modify any variables in EMTP common blocks as linear functions of     '  /
   data texspy ( 364 )  /  '  time.   that is, the the user can ramp the values between beginning and ending'  /
   data texspy ( 365 )  /  '  limits,  as EMTP simulation time moves between beginning and ending times --- '  /
   data texspy ( 366 )  /  '  with all control parameters being user-defined.  any variable which can be    '  /
   data texspy ( 367 )  /  '  seen via the  "examine"  command can also be  ramped.  in specifying parameter'  /
   data texspy ( 368 )  /  '  values, there are three nested loops. the outer loop is for time, the middle  '  /
   data texspy ( 369 )  /  '  one is for variable values, and the inner one is for variable names.   the    '  /
   data texspy ( 370 )  /  '  user can stay inside (middle or inner loops) as long as the outer quantity is '  /
   data texspy ( 371 )  /  '  not altered.   "end" will exit any one level,  and head outward to the next   '  /
   data texspy ( 372 )  /  '  one.   at the outer-most level, the optional  "show"  command will display all'  /
   data texspy ( 373 )  /  '  ramps which the user has defined thus far,  while  "end"  or  "spy"  will     '  /
   data texspy ( 374 )  /  '  return to the  "spy:"  prompt.  actual variable redefinition is handled within'  /
   data texspy ( 375 )  /  '  module  "analyt"  of overlay 20 of the EMTP, where ramping logic has been     '  /
   data texspy ( 376 )  /  '  built (this is automatic, as the simulation progresses).                      '  /
   data texspy ( 377 )  /  'key word no. 35:  "time"        ----  ----  ----                                '  /
   data texspy ( 378 )  /  '  this response to the  "spy:"  prompt will result in the display of  both  the '  /
   data texspy ( 379 )  /  '  wall-clock time  (the date, too)  and also  the  current EMTP simulation time '  /
   data texspy ( 380 )  /  '  parameters (t, tmax, deltat).                                                 '  /
   data texspy ( 381 )  /  'key word no. 36:  "tek"         ----  ----  ----                                '  /
   data texspy ( 382 )  /  '  this response to the  "spy:"  prompt allows modification to an ongoing vector-'  /
   data texspy ( 383 )  /  '  graphic display which is currently  rolling.  yes, the user could simply      '  /
   data texspy ( 384 )  /  '  regenerate the plot from the beginning (by use of a  "plot"  command following'  /
   data texspy ( 385 )  /  '  a user-keyed interrupt), but minor changes can often be made more easily, and '  /
   data texspy ( 386 )  /  '  with less disruption, using  "tek". a subsequent prompt will then list a      '  /
   data texspy ( 387 )  /  '  menu of alternatives which are available for such "on-the-fly"  rolling       '  /
   data texspy ( 388 )  /  '  "plot"  tampering.  for completeness, that menu is repeated here:             '  /
   data texspy ( 389 )  /  '       spy:tek                                                                  '  /
   data texspy ( 390 )  /  '        >< to tamper with the rolling vector plot,  send choice.                '  /
   data texspy ( 391 )  /  '        >< option (mark, delay, inner, overlap, end, help) :                    '  /
   data texspy ( 392 )  /  '           mark  ---- for instantaneous marking of curves on tek screen;        '  /
   data texspy ( 393 )  /  '           delay  --- to control how simultaneous the rolling is to be;         '  /
   data texspy ( 394 )  /  '           inner  ---- to  call timval (the "inner:" level of ploting);         '  /
   data texspy ( 395 )  /  '           overlap  -- to modify the percent overlap for new-page plot;         '  /
   data texspy ( 396 )  /  '           end   ---- for return to  "spy:"  prompt.                            '  /
   data texspy ( 397 )  /  '  unless the user requests curve identification via  "mark",  the  rolling plot '  /
   data texspy ( 398 )  /  '  will only have it for the regenerated portion (the overlap from the previous  '  /
   data texspy ( 399 )  /  '  page, as controlled by  "overlap"). the  "delay"  command allows the user     '  /
   data texspy ( 400 )  /  '  to control the time-step multiplicity between plotting sessions.  there is a  '  /
   data texspy ( 401 )  /  '  trade off between solution efficiency (maximized if plotting is infrequent)   '  /
   data texspy ( 402 )  /  '  and simultaneous observation (maximized if plotting occurs on each new time   '  /
   data texspy ( 403 )  /  '  step).   so much for the concept.  but the details of counting are in fact a  '  /
   data texspy ( 404 )  /  '  little different, since it is not time steps, but rather plot points, which   '  /
   data texspy ( 405 )  /  '  are counted.   only if every solution point becomes a plot point (only if the '  /
   data texspy ( 406 )  /  '  miscellaneous data parameter  iplot is equal to unity) are these two equal,   '  /
   data texspy ( 407 )  /  '  note.  so, if  iout = 3  and the user wants to see plotting progress about    '  /
   data texspy ( 408 )  /  '  every twentieth time-step, he would send "7" in response to the prompt for a  '  /
   data texspy ( 409 )  /  '  multiplicity after his  "delay"  was accepted.   the result would then be     '  /
   data texspy ( 410 )  /  '  incremental plotting every 21st step (21 = 3*7).   concerning  "overlap",  it '  /
   data texspy ( 411 )  /  '  is generally recommended that this only be used for terminals without memory, '  /
   data texspy ( 412 )  /  '  since any percentage greater than zero does increase the plotting burden.   in'  /
   data texspy ( 413 )  /  '  the case of displays with near-infinite storage (e.g., apollo windows),  it is'  /
   data texspy ( 414 )  /  '  better to leave the overlap at the default value of zero.   use of  "inner"   '  /
   data texspy ( 415 )  /  '  both very powerful and potentially very tricky, since it results in a  call to'  /
   data texspy ( 416 )  /  '  subroutine timval,  which is responsible for the  "inner:"  level of  "plot"  '  /
   data texspy ( 417 )  /  '  dialogue.   all  "inner:"  level definitions are therefore available to the   '  /
   data texspy ( 418 )  /  '  user, although effects may or may not be as expected (either trial and error  '  /
   data texspy ( 419 )  /  '  experience, or understanding of "tpplot" fortran, are required in order to    '  /
   data texspy ( 420 )  /  '  predict the results of any given operation).   the one big difference is that '  /
   data texspy ( 421 )  /  '  the use of  "time"  or just  <cr>  do not result in the production of a new   '  /
   data texspy ( 422 )  /  '  plot.  to return back to the  "tek" prompt from the  "inner:"  prompt,        '  /
   data texspy ( 423 )  /  '  send  "spy".   as for return to the "spy"  prompt,  either the listed  "end"  '  /
   data texspy ( 424 )  /  '  or  "spy"  will work.                                                         '  /
   data texspy ( 425 )  /  'key word no. 37:  "branch"      ----  ----  ----                                '  /
   data texspy ( 426 )  /  '  this response to the  "spy:"  prompt is  issued  for  a  display  of the EMTP '  /
   data texspy ( 427 )  /  '  branch table.  subsequently send an additional  "extra"  to change to the next'  /
   data texspy ( 428 )  /  '  sub-table  (as of february 1984, there  are  two),  if  different columns  are'  /
   data texspy ( 429 )  /  '  desired.  within any one choice as to sub-table,  there is a loop in which the'  /
   data texspy ( 430 )  /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy ( 431 )  /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy ( 432 )  /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage '  /
   data texspy ( 433 )  /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy ( 434 )  /  '  and return to the  "spy:"  prompt.  to refresh the heading, use either  "bran"'  /
   data texspy ( 435 )  /  '  or  "head"  (only 4 characters of  "branch"  or  "heading"  are checked).     '  /
   data texspy ( 436 )  /  'key word no. 38:  "yform"       ----  ----  ----                                '  /
   data texspy ( 437 )  /  '  this  response  to  the  "spy:"  prompt  will result in continuous (every time'  /
   data texspy ( 438 )  /  '  step)  re-formation of  [y],  followed by re-triangularization.   it is turned'  /
   data texspy ( 439 )  /  '  off (no more [y]-forming)  by sending "noy"  after  "spy:"  prompt.  ???  ??? '  /
   data texspy ( 440 )  /  '  warning:  like  the earlier batch-mode  "modify deltat",  this looks better in'  /
   data texspy ( 441 )  /  '  theory than  it  does in practice.  for  important elements such as frequency-'  /
   data texspy ( 442 )  /  '  dependent transmission lines, the original data which is needed to reform  [y]'  /
   data texspy ( 443 )  /  '  has been destroyed by the solution, so the result  will  be erroneous.  unless'  /
   data texspy ( 444 )  /  '  the user has a particularly degenerate problem,  and  he is sure that he knows'  /
   data texspy ( 445 )  /  '  what he is doing, the general recommendation is to avoid all such use.  ??? ??'  /
   data texspy ( 446 )  /  'key word no. 39:  "noy"         ----  ----  ----                                '  /
   data texspy ( 447 )  /  '  this response to the  "spy:"  prompt will cancel a preceding  "yform"  request'  /
   data texspy ( 448 )  /  '  for continuous [y]-formation.                                                 '  /
   data texspy ( 449 )  /  'key word no. 40:  "factor"      ----  ----  ----                                '  /
   data texspy ( 450 )  /  '  this  response  to  the  "spy:"  prompt  will result in continuous (every time'  /
   data texspy ( 451 )  /  '  step) triangularization.   it is turned off  (no more automatic factoring)  by'  /
   data texspy ( 452 )  /  '  sending  "nof"  in response to the  "spy:"  prompt.                           '  /
   data texspy ( 453 )  /  'key word no. 41:  "nof"         ----  ----  ----                                '  /
   data texspy ( 454 )  /  '  this  response  to  the  "spy:"  prompt  will  cancel  a  preceding   "factor"'  /
   data texspy ( 455 )  /  '  request for the continuous re-triangularization of [y].                       '  /
   data texspy ( 456 )  /  'key word no. 42:  "rlc"         ----  ----  ----                                '  /
   data texspy ( 457 )  /  '  this response to the  "spy:"  prompt will produce a display of the EMTP  R-L-C'  /
   data texspy ( 458 )  /  '  tables.  subsequently  send  an  additional  "extra"  to  change to  the  next'  /
   data texspy ( 459 )  /  '  sub-table  (as of february 1984, there  are  two),  if  different columns  are'  /
   data texspy ( 460 )  /  '  desired.  within any one choice as to sub-table,  there is a loop in which the'  /
   data texspy ( 461 )  /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy ( 462 )  /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy ( 463 )  /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage '  /
   data texspy ( 464 )  /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy ( 465 )  /  '  and return to the  "spy:"  prompt.  to refresh the heading,  use either  "rlc"'  /
   data texspy ( 466 )  /  '  or  "head"  (only 4 characters of  "heading"  are checked).                   '  /
   data texspy ( 467 )  /  'key word no. 43:  "width"       ----  ----  ----                                '  /
   data texspy ( 468 )  /  '  this response to the  "spy:"  prompt will toggle the output line length  (if  '  /
   data texspy ( 469 )  /  '  equal to 132 at the time the command is issued, it will be changed to 80,  and'  /
   data texspy ( 470 )  /  '  vice-versa).  this is for EMTP line printer output (channel lunit6) only.     '  /
   data texspy ( 471 )  /  'key word no. 44:  "bus"         ----  ----  ----                                '  /
   data texspy ( 472 )  /  '  this response to the  "spy:"  prompt will produce a display  of  the  EMTP bus'  /
   data texspy ( 473 )  /  '  vectors.   subsequently  send  an  additional  "extra"  to  change to the next'  /
   data texspy ( 474 )  /  '  next sub-table (as of february 1984, there are two),  if different columns are'  /
   data texspy ( 475 )  /  '  desired.  within any one choice as to sub-table,  there is a loop in which the'  /
   data texspy ( 476 )  /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy ( 477 )  /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy ( 478 )  /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage '  /
   data texspy ( 479 )  /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy ( 480 )  /  '  and return to the  "spy:"  prompt.  to refresh the heading,  use either  "bus"'  /
   data texspy ( 481 )  /  '  or  "head"  (only 4 characters of "heading" are checked).                     '  /
   data texspy ( 482 )  /  'key word no. 45:  "size"        ----  ----  ----                                '  /
   data texspy ( 483 )  /  '  this response  to  the  "spy:"  prompt  will  produce a display of actual EMTP'  /
   data texspy ( 484 )  /  '  data requirements  ----  the  "present figure"  list sizes which are  seen  at'  /
   data texspy ( 485 )  /  '  the end of batch-mode EMTP printout, in the case-summary statistics.          '  /
   data texspy ( 486 )  /  'key word no. 46:  "limit"       ----  ----  ----                                '  /
   data texspy ( 487 )  /  '  this response  to  the  "spy:"  prompt  will produce a display of the limiting'  /
   data texspy ( 488 )  /  '  EMTP table sizes  ----  the  "program limit"  list sizes which are seen at the'  /
   data texspy ( 489 )  /  '  end of batch-mode EMTP printout,  in the case-summary statistics.             '  /
   data texspy ( 490 )  /  'key word no. 47:  "iout"        ----  ----  ----                                '  /
   data texspy ( 491 )  /  '  this response  to  the  "spy:"  prompt  will alter the EMTP printout frequency'  /
   data texspy ( 492 )  /  '  according  to  the user"s latest desire.   the response will be instantaneous,'  /
   data texspy ( 493 )  /  '  with  a  later  minor adjustment at the first round step number  (the next one'  /
   data texspy ( 494 )  /  '  which  is  divisible  by  iout  with zero  remainder).   this command cancels '  /
   data texspy ( 495 )  /  '  any  previous  batch-mode requests  for  modification  of the output frequency'  /
   data texspy ( 496 )  /  '  (e.g., the special-request word  "change printout frequency").                '  /
   data texspy ( 497 )  /  'key word no. 48:  "node"        ----  ----  ----                                '  /
   data texspy ( 498 )  /  '  this response  to  the  "spy:"  prompt  will yield a question/answer loop with'  /
   data texspy ( 499 )  /  '  input being a user-supplied 6-character bus name, and output (the spy display)'  /
   data texspy ( 500 )  /  '  being the corresponding EMTP node number.   after any one node number has been'  /
   data texspy ( 501 )  /  '  displayed,  the  user can send  "connect"  to obtain a list of all row numbers'  /
   data texspy ( 502 )  /  '  of all connected branches/switches/nonlinear elements.                        '  /
   data texspy ( 503 )  /  'key word no. 49:  "nonlin"      ----  ----  ----                                '  /
   data texspy ( 504 )  /  '  this  response  to  the  "spy:"  prompt  is  issued  for a display of the EMTP'  /
   data texspy ( 505 )  /  '  nonlinear element table.  subsequently  send an additional  "extra"  to change'  /
   data texspy ( 506 )  /  '  to the next sub-table (as of february 1984, there are just two),  if different'  /
   data texspy ( 507 )  /  '  columns are desired.  within any one sub-table,  there is a loop in which  the'  /
   data texspy ( 508 )  /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy ( 509 )  /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy ( 510 )  /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage '  /
   data texspy ( 511 )  /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy ( 512 )  /  '  and return to the  "spy:"  prompt.  to refresh the heading, use either  "nonl"'  /
   data texspy ( 513 )  /  '  or  "head"  (only 4 characters of "nonlin" or "heading" are checked).         '  /
   data texspy ( 514 )  /  'key word no. 50:  "space"       ----  ----  ----                                '  /
   data texspy ( 515 )  /  '  this response to the  "spy:"  prompt begins dialogue which can rearrange      '  /
   data texspy ( 516 )  /  '  storage of either data cards or plot data points.  such use is mandatory if   '  /
   data texspy ( 517 )  /  '  no additional room for plot data remains (logic of "pltfil" will deny passage '  /
   data texspy ( 518 )  /  '  until more storage has so been freed, in such a case).  the management of card'  /
   data texspy ( 519 )  /  '  images will generally be performed only in conjunction with  "edit"  use, to  '  /
   data texspy ( 520 )  /  '  modify EMTP data.  parameters related to these two storages will be displayed:'  /
   data texspy ( 521 )  /  '     indbeg --- beginning location (index to /c29b01/) of plot data points;     '  /
   data texspy ( 522 )  /  '     indbuf --- next free cell (index to /c29b01/) for storage of plot data;    '  /
   data texspy ( 523 )  /  '     limbuf --- limit on indbuf (end of /c29b01/);                              '  /
   data texspy ( 524 )  /  '     numdcd --- last card read by EMTP (index to character(8)0 file6 storage)   '  /
   data texspy ( 525 )  /  '     numcrd --- largest index for card storage in file6 (as read by data);      '  /
   data texspy ( 526 )  /  '     limcrd --- limiting index for file6 card storage (dimensioned limit).      '  /
   data texspy ( 527 )  /  '  immediately after this display there will be a choice among the possible      '  /
   data texspy ( 528 )  /  '  responses  "cards",  "plot",  and  "spy".  the third of these will abort      '  /
   data texspy ( 529 )  /  '  the command, while the first two select between the two major classes of      '  /
   data texspy ( 530 )  /  '  space management.                                                             '  /
   data texspy ( 531 )  /  '       (1) for the  "plot"  case,  the user must next choose among  "write",    '  /
   data texspy ( 532 )  /  '  "thin",  "delete",  and  "read",  basically.   the first of these writes plot '  /
   data texspy ( 533 )  /  '  points to disk just as a non-interactive program would, using i/o channel     '  /
   data texspy ( 534 )  /  '  number lunit4.   the only change is that the time span  [tbeg, tend]  to be   '  /
   data texspy ( 535 )  /  '  dumped,  as well as the frequency of the output (the effective iplot), are    '  /
   data texspy ( 536 )  /  '  under interactive control (chosen by responses to subsequent prompts).  the   '  /
   data texspy ( 537 )  /  '  "thin"  command discards points within the  [tbeg, tend]  window according to '  /
   data texspy ( 538 )  /  '  a user-specified frequency.  the  "delete"  option destroys all points        '  /
   data texspy ( 539 )  /  '  (the effective iplot is infinite) within the user-specified time range        '  /
   data texspy ( 540 )  /  '  [tbeg,  tend].   the  "read"  option is the reverse of  "write",  allowing the'  /
   data texspy ( 541 )  /  '  restoration of lunit4 disk-stored points to the working interactive memory.   '  /
   data texspy ( 542 )  /  '  this is done with user-specified frequency within the time range [tbeg, tend].'  /
   data texspy ( 543 )  /  '  finally, there is a hybrid command  "flush"  which combines  "write"  and     '  /
   data texspy ( 544 )  /  '  "thin"  (assuming that the user wants to use a common multiplicity for both   '  /
   data texspy ( 545 )  /  '  operations).  after compacting operations, there will be printout of available'  /
   data texspy ( 546 )  /  '  free space.  the normal EMTP line printer output (i/o channel lunit6) will    '  /
   data texspy ( 547 )  /  '  contain a record of the plot-file manipulations, to remind the user of what   '  /
   data texspy ( 548 )  /  '  he has done.  an extra command is  "auto",  which gives an automatic, full    '  /
   data texspy ( 549 )  /  '  "flush"  from that point onward.                                              '  /
   data texspy ( 550 )  /  '       (2) for the  cards  branch, the user is prompted to choose among  "move",'  /
   data texspy ( 551 )  /  '  "copy",  and  "blank".   the first of these is for block transfers (leaving   '  /
   data texspy ( 552 )  /  '  blanks in vacated locations), while the second is for block reproductions     '  /
   data texspy ( 553 )  /  '  (leaving the original locations unaltered).  the  "blank"  command is to      '  /
   data texspy ( 554 )  /  '  erase a block of cards.  in all three cases, beginning locations and total    '  /
   data texspy ( 555 )  /  '  number of cards processed are used to define the blocks involved.             '  /
   data texspy ( 556 )  /  '       many sub-commands of the large "space"  command allow the abort option   '  /
   data texspy ( 557 )  /  '  "spy" (to return to the  "spy:"  prompt), or the option of backing up one     '  /
   data texspy ( 558 )  /  '  level (via  "out").                                                           '  /
   data texspy ( 559 )  /  'key word no. 51:  "lunit4"      ----  ----  ----                                '  /
   data texspy ( 560 )  /  '  issue  this  command  to  connect  (fortran open), disconnect (fortran close),'  /
   data texspy ( 561 )  /  '  position,  or  inspect  the  contents of  the disk file of plot data which is '  /
   data texspy ( 562 )  /  '  connected  to  i/o unit number  lunit4.   recall that a disk file can be given'  /
   data texspy ( 563 )  /  '  solution  points  via  the  "disk"  or  "flush"  subcommands  of  the  "space"'  /
   data texspy ( 564 )  /  '  command.  subcommand choices are as follows: "open",  "close",  "top",  "bot",'  /
   data texspy ( 565 )  /  '  "next",  "back",  and  "time".   in order,  these shall now                   '  /
   data texspy ( 566 )  /  '  be summarized:                                                                '  /
   data texspy ( 567 )  /  '       open -- subsequent prompts will allow for a user-supplied file name      '  /
   data texspy ( 568 )  /  '               (not to exceed 32 characters), and the  "status"  choice         '  /
   data texspy ( 569 )  /  '               between  "new"  and  "old"  (vacuous or pre-defined files).      '  /
   data texspy ( 570 )  /  '       close -- there are no parameters; the distinction between  "keep"        '  /
   data texspy ( 571 )  /  '                and  "delete"  is implicit (all opened files should be saved).  '  /
   data texspy ( 572 )  /  '       top  --- to position the lunit4 disk file ready to read the beginning    '  /
   data texspy ( 573 )  /  '                plot data (for smallest time);                                  '  /
   data texspy ( 574 )  /  '       bot  --- to position the lunit4 disk file after reading the final plot   '  /
   data texspy ( 575 )  /  '                data (there is a read until an end-of-file).  certain computers '  /
   data texspy ( 576 )  /  '                (e.g., vax) allow writeing at this point, so old plot files can '  /
   data texspy ( 577 )  /  '                be directly added to after such an initial positioning.         '  /
   data texspy ( 578 )  /  '       next --- to read and display the plot points of the next time instant.   '  /
   data texspy ( 579 )  /  '                this forward read is fast for all known computers.              '  /
   data texspy ( 580 )  /  '       back --- to read and display plot points of the preceding time step.     '  /
   data texspy ( 581 )  /  '                since this uses backspace, be aware that for some computers     '  /
   data texspy ( 582 )  /  '                (prime, apollo, etc.), this command is internally converted to  '  /
   data texspy ( 583 )  /  '                a  "top"  and  "time" command which backs up two steps.         '  /
   data texspy ( 584 )  /  '       time --- to position the lunit4 plot file immediately after having read  '  /
   data texspy ( 585 )  /  '                points for the first step at or beyond the user-specified time; '  /
   data texspy ( 586 )  /  'key word no. 52:  "series"      ----  ----  ----                                '  /
   data texspy ( 587 )  /  '  issue this command in order to modify the values of series R-L-C branches     '  /
   data texspy ( 588 )  /  '  within the time-step loop.  such usage requires one unused row of both the    '  /
   data texspy ( 589 )  /  '  branch table (list 2) and the branch-parameter table (list 7) for each series '  /
   data texspy ( 590 )  /  '  R-L-C branch of interest.  the  "series"  command is coupled to the  "ramp"   '  /
   data texspy ( 591 )  /  '  command in that  "series"  must be used to select the branches of interest    '  /
   data texspy ( 592 )  /  '  or potential interest ahead of time, while it is  "ramp"  that actually varies'  /
   data texspy ( 593 )  /  '  them when the time comes.  this would be for continuous variation (ramping),  '  /
   data texspy ( 594 )  /  '  which is the most common case of interest.  for step changes (single-time     '  /
   data texspy ( 595 )  /  '  occurances),  "ramp"  is not used.  in either case,  actual changes to [y]    '  /
   data texspy ( 596 )  /  '  occur just prior to factoring of [y] in  "subts1"  of overlay 16.  previously,'  /
   data texspy ( 597 )  /  '  the user must issue a spy call for  "series"  ----   at any point prior to    '  /
   data texspy ( 598 )  /  '  overlay 12.  this is remembered, so that the EMTP will later automatically    '  /
   data texspy ( 599 )  /  '  break in the middle of  "over12"  with a message that this is the time to     '  /
   data texspy ( 600 )  /  '  define the table of series R-L-C branches which might later be tampered with. '  /
   data texspy ( 601 )  /  '  at the automatic break in  "over12", the user chooses among the commands      '  /
   data texspy ( 602 )  /  '  "show",  "extra",  "change",  "step", "rewind",  and  "spy".   "show"  will   '  /
   data texspy ( 603 )  /  '  display the table of series R-L-C branches which have thus far been considered'  /
   data texspy ( 604 )  /  '  for possible later change,  with  "extra"  yielding an extension to the       '  /
   data texspy ( 605 )  /  '  display (a second table,  giving starting and next values).   "change"  is the'  /
   data texspy ( 606 )  /  '  gateway to various further choices  ("data",  "move",  "blank",  "use",       '  /
   data texspy ( 607 )  /  '  "value",  "end",  and  "spy")  for the definition and manipulation of the     '  /
   data texspy ( 608 )  /  '  table of  "show".   quickly summarizing these,  "data"  is used to copy series'  /
   data texspy ( 609 )  /  '  R-L-C branches into the tamper table, while  "move"  will copy from one row of'  /
   data texspy ( 610 )  /  '  the tamper table to another, and  "blank"  will erase any such row.   "use"   '  /
   data texspy ( 611 )  /  '  allows the user to toggle the activity status of any entry in the tamper      '  /
   data texspy ( 612 )  /  '  table,  while  "value"  allows modification of R-L-C parameters in case this  '  /
   data texspy ( 613 )  /  '  is to be done discontinuously.   "end"  moves outward, back to the preceding  '  /
   data texspy ( 614 )  /  '  prompt,  while  "spy"  aborts  "show" and returns to the  "spy:"  prompt.     '  /
   data texspy ( 615 )  /  '  this completes  "change"  usage.   next is  "step",  which is the command for '  /
   data texspy ( 616 )  /  '  a manual, discontinuous change at the next available opportunity (within one  '  /
   data texspy ( 617 )  /  '  time step).   "rewind"  will erase the tamper table completely, so it is only '  /
   data texspy ( 618 )  /  '  recommended if the existing table is unsalvageable.   for details of the      '  /
   data texspy ( 619 )  /  '  behind the ramping of series R-L-C elements, see vladimir"s article in the    '  /
   data texspy ( 620 )  /  '  EMTP newsletter, vol. 4, no. 2, november, 1983.                               '  /
   data texspy ( 621 )  /  'key word no. 53:  "lock"        ----  ----  ----                                '  /
   data texspy ( 622 )  /  '  issue this command to disable time-sharing between  spy  and the ongoing EMTP '  /
   data texspy ( 623 )  /  '  simulation.  subsequent cancellation of the command is by  "go".   since this '  /
   data texspy ( 624 )  /  '  command is absolute, it can be repeated any number of times.   besides this   '  /
   data texspy ( 625 )  /  '  user-defined  "lock"  command,  there are hidden, internal ones, such as the  '  /
   data texspy ( 626 )  /  '  one by module  "pltfil"  when plot data space has been exhausted.  but a word '  /
   data texspy ( 627 )  /  '  of caution:  all of this talk about time-sharing has meaning only for those   '  /
   data texspy ( 628 )  /  '  computers which allow time-sharing (with type-ahead that is not erased by the '  /
   data texspy ( 629 )  /  '  user-keyed interrupt).                                                        '  /
   data texspy ( 630 )  /  'key word no. 54:  "[y]"         ----  ----  ----                                '  /
   data texspy ( 631 )  /  '  issue this command to display one or more rows of the nodal admittance matrix '  /
   data texspy ( 632 )  /  '  [y] of the time-step loop.   a subsequent prompt will request the desired row,'  /
   data texspy ( 633 )  /  '  which may either be specified by bus name (assumed to begin with a letter), or'  /
   data texspy ( 634 )  /  '  by a node number.  alternate responses are  "spy"  or  "end"  (to return to   '  /
   data texspy ( 635 )  /  '  the  "spy:"  prompt),  "top"  (to display the row for node 2),  "bot"  (to    '  /
   data texspy ( 636 )  /  '  display the row for node  kpartb),  and  "next"  or  nothing  (to display the '  /
   data texspy ( 637 )  /  '  following row).   each nonzero term of the row will be displayed,  next to    '  /
   data texspy ( 638 )  /  '  the associated column number.   a minus sign applied to the column number is  '  /
   data texspy ( 639 )  /  '  used to mark the largest column of the row.   in case separate  "examine"     '  /
   data texspy ( 640 )  /  '  usage might be contemplated,  kks  points to the  (km, ykm)  pairs.           '  /
   data texspy ( 641 )  /  'key word no. 55:  "[f]"         ----  ----  ----                                '  /
   data texspy ( 642 )  /  '  issue this command to display one or more rows of the table of factors (the   '  /
   data texspy ( 643 )  /  '  triangularized [y] of the time-step loop).   display options are generally    '  /
   data texspy ( 644 )  /  '  identical to the immediately-preceding  "[y]"  command,  except that here     '  /
   data texspy ( 645 )  /  '  only the upper triangle exists,  a minus sign marks the diagonal entry,  and  '  /
   data texspy ( 646 )  /  '  kk  is the pointer (which can be observed  via the  "bus"  command).          '  /
   data texspy ( 647 )  /  'key word no. 55:  "noroll"      ----  ----  ----                                '  /
   data texspy ( 648 )  /  '  use this command to cancel  rolling character and/or vector plots.   these    '  /
   data texspy ( 649 )  /  '  began,  it should be recalled,  via "rollc"  and/or  "rollv"  commands        '  /
   data texspy ( 650 )  /  '  (independent of each other, and also independent of the "tek" setting) at the '  /
   data texspy ( 651 )  /  '  "inner:"  level of plotting.   the user-keyed interrupt will by itself stop   '  /
   data texspy ( 652 )  /  '  the rolling only if control is within "chrplt"  or  "tekplt"  plotting        '  /
   data texspy ( 653 )  /  '  modules when it is spotted.  otherwise, this added manual request is needed.  '  /
   data texspy ( 654 )  /  '  the only alternative would be to go back into the  "inner:"  level of         '  /
   data texspy ( 655 )  /  '  plotting to toggle the appropriate switch (via  "rollc"  and/or  "rollv"      '  /
   data texspy ( 656 )  /  '  requests).                                                                    '  /
   data texspy ( 657 )  /  'key word no. 57:  "open"        ----  ----  ----                                '  /
   data texspy ( 658 )  /  '  issue this command to  open  a formatted disk file.  a loop will be entered in'  /
   data texspy ( 659 )  /  '  which there will be a prompt for the next i/o unit number, and the disk       '  /
   data texspy ( 660 )  /  '  file which is to be connected thereto.  to exit the loop, send a blank line.  '  /
   data texspy ( 661 )  /  'key word no. 58:  "close"       ----  ----  ----                                '  /
   data texspy ( 662 )  /  '  issue this command to  close  a disk file.  a loop will be entered in         '  /
   data texspy ( 663 )  /  '  which there will be a prompt for the next i/o unit number, and the  status    '  /
   data texspy ( 664 )  /  '  (either  keep  or  delete  are ok). to exit the loop, send a blank line.      '  /
   data texspy ( 665 )  /  'key word no. 59:  "sm"          ----  ----  ----                                '  /
   data texspy ( 666 )  /  '  issue this command to display the various electrical, mechanical, and solution'  /
   data texspy ( 667 )  /  '  parameters of any type-59 synchronous machine (s.m.).  time-sharing by spy    '  /
   data texspy ( 668 )  /  '  is automatically suspended during this display, due to algorithmic limitations'  /
   data texspy ( 669 )  /  '  of the type-59 model (examination is only possible as that machine is being   '  /
   data texspy ( 670 )  /  '  processed within subroutine update, which is called by "subts1" (the first    '  /
   data texspy ( 671 )  /  '  piece of the time-step loop.  the user can issue the  "sm"  request at any    '  /
   data texspy ( 672 )  /  '  time, but in fact the EMTP simulation will stop within the processing loop of '  /
   data texspy ( 673 )  /  '  "update", and allow the user to accept or reject (y or n responses) each      '  /
   data texspy ( 674 )  /  '  machine in turn.  actually, only  "n" vetos the opportunity (a simple <cr>    '  /
   data texspy ( 675 )  /  '  will accept the next machine).  once considering any one type-59 s.m., there  '  /
   data texspy ( 676 )  /  '  are subsequent choices as to which parameters the user wants to see:  "elec"  '  /
   data texspy ( 677 )  /  '  for electrical parameters,  "mech"  for mechanical parameters, etc.           '  /
   data texspy ( 678 )  /  '  ?????????   as of 26 feb 84, command is not complete   ?????????????????????  '  /
   data texspy ( 679 )  /  'key word no. 60:  "honk"        ----  ----  ----                                '  /
   data texspy ( 680 )  /  '  issue this command to ring the terminal bell (i.e., honk its horn).  there    '  /
   data texspy ( 681 )  /  '  will then be a prompt for an integer severity level, which should be between  '  /
   data texspy ( 682 )  /  '  one and ten (zero produces no response, while 10 indicates a disaster).       '  /
   data texspy ( 683 )  /  'key word no. 61:  "choice"      ----  ----  ----                                '  /
   data texspy ( 684 )  /  '  issue this command to see output variable names of any of the five classes    '  /
   data texspy ( 685 )  /  '  making up the EMTP output vector (1=node voltages,  2=branch voltages,  etc.).'  /
   data texspy ( 686 )  /  '  order of the five classes corresponds to the order of concatenation in the    '  /
   data texspy ( 687 )  /  '  EMTP output vector;  and within each class, the order corresponds, too.  the  '  /
   data texspy ( 688 )  /  '  display is dynamic,  so it always reflects the current output variables       '  /
   data texspy ( 689 )  /  '  (unlike  "choice"  at the  "middle:" level of plotting,  which only displays  '  /
   data texspy ( 690 )  /  '  the original status of overlay 15).                                           '  /
   data texspy ( 691 )  /  'key word no. 62:  "tacs"        ----  ----  ----                                '  /
   data texspy ( 692 )  /  '  issue this command as the gateway to concurrent sequential processing (csp),  '  /
   data texspy ( 693 )  /  '  which is based on special, spy-defined tacs supplemental variable usage.  then'  /
   data texspy ( 694 )  /  '  one enters a loop over csp control commands.   if  "rewind"  is requested,    '  /
   data texspy ( 695 )  /  '  all previous definitions are erased, so definitions begin from level zero.  if'  /
   data texspy ( 696 )  /  '  "source"  is sent,  all following input is assumed to be tacs source cards,   '  /
   data texspy ( 697 )  /  '  until terminated by an  "end"  card. if  "supplemental"  is sent,  then all   '  /
   data texspy ( 698 )  /  '  following data cards (spy input) is assumed to be tacs supplemental variable/ '  /
   data texspy ( 699 )  /  '  device data cards,  also until an  "end"  card is encountered.  the subcommand'  /
   data texspy ( 700 )  /  '  "patch"  allows the user to connect any variables to his tacs sources  (to    '  /
   data texspy ( 701 )  /  '  define the inputs),  and apply any supplemental variable results anywhere.    '  /
   data texspy ( 702 )  /  '  the "any" comes from the use of a memory address of the spy commands  "list"  '  /
   data texspy ( 703 )  /  '  or  "find",  just as  "examine"  or "deposit"  are quite unrestricted.  there '  /
   data texspy ( 704 )  /  '  will be separate prompts for all such tacs input and output connections.  when'  /
   data texspy ( 705 )  /  '  all such connections are complete,  "show"  can be used to display a summary  '  /
   data texspy ( 706 )  /  '  table of all tacs csp input/output connections.   working room for such usage '  /
   data texspy ( 707 )  /  '  can be controlled by the user in several ways, if there are shortages.  first,'  /
   data texspy ( 708 )  /  '  if a user wants dummy output channels for printing/plotting, reserve dummy    '  /
   data texspy ( 709 )  /  '  node voltage outputs with  "chan01" punched in cols. 3-8, and the desired     '  /
   data texspy ( 710 )  /  '  number punched in columns 9-16 with i8format.   resultant names will be       '  /
   data texspy ( 711 )  /  '  serialized from the aforementioned root, and added to the output vector at the'  /
   data texspy ( 712 )  /  '  point of definition (if this card is first, all such dummy names will come    '  /
   data texspy ( 713 )  /  '  first, before other node voltages). second, there is array dimensioning for   '  /
   data texspy ( 714 )  /  '  all vectors of the  "choice"  table ---  presently fixed in deck "dekspy", at '  /
   data texspy ( 715 )  /  '  the moment.  finally, in addition to the familiar tacs table limits, there is '  /
   data texspy ( 716 )  /  '  an automatic reservation of space for all user-defined tacs csp sources.  this'  /
   data texspy ( 717 )  /  '  is maximized automatically, within existing tacs dimensions (as assigned by   '  /
   data texspy ( 718 )  /  '  absolute tacs dimensions, for example).  as usual,  "spy"  will break out of  '  /
   data texspy ( 719 )  /  '  any loop, in case an abortion is desired.                                     '  /
   data texspy ( 720 )  /  'key word no. 63:  "wait"        ----  ----  ----                                '  /
   data texspy ( 721 )  /  '  the response to this spy command will be a prompt for the desired delay       '  /
   data texspy ( 722 )  /  '  (hibernation) time in seconds.  once this is accepted, the installation-      '  /
   data texspy ( 723 )  /  '  dependent subroutine tdelay  is called, to return control only after the      '  /
   data texspy ( 724 )  /  '  requested nap.                                                                '  /
   data texspy ( 725 )  /  'key word no. 64:  "v-i""        ----  ----  ----                                '  /
   data texspy ( 726 )  /  '  this spy command is used to examine the list-10 characteristics of all        '  /
   data texspy ( 727 )  /  '  nonlinear and pseudo-nonlinear elements.  the name  "v-i"  is symbolic        '  /
   data texspy ( 728 )  /  '  only, since flux-current or resistance-time characteristics are also          '  /
   data texspy ( 729 )  /  '  included.  immediately upon entry, there will be a display of the number      '  /
   data texspy ( 730 )  /  '  of nonlinear elements (list 9) and data points of the characteristics         '  /
   data texspy ( 731 )  /  '  (list 10).  also shown will be a listof options to a pair of free-format      '  /
   data texspy ( 732 )  /  '  row numbers:                                                                  '  /
   data texspy ( 733 )  /  '     next or <cr>  ---- to display the characteristic of the next element       '  /
   data texspy ( 734 )  /  '                        of the nonlinear element table.  there is wrap around   '  /
   data texspy ( 735 )  /  '                        from beginning to end.  no initialization required.     '  /
   data texspy ( 736 )  /  '     last  ---- to display the characteristic of the preceding element of       '  /
   data texspy ( 737 )  /  '                the nonlinear element table.  there is wrap around from the     '  /
   data texspy ( 738 )  /  '                beginning to the end. no initialization is required.            '  /
   data texspy ( 739 )  /  '     all  --- to display all rows of the list 10 characteristics.               '  /
   data texspy ( 740 )  /  '     spy, end, or stop ---- to abort display loop, and return to  "spy:"        '  /
   data texspy ( 741 )  /  '     mode  ---- to toggle the binary switch which selects between a display     '  /
   data texspy ( 742 )  /  '                by elements (value n8=0), and a display by rows of list 10.     '  /
   data texspy ( 743 )  /  '                initially, the display by elements, only one at a time, is      '  /
   data texspy ( 744 )  /  '                assumed.                                                        '  /
   data texspy ( 745 )  /  '  in case the use inputs a pair of numbers instead of one of these key          '  /
   data texspy ( 746 )  /  '  words, then the response depends on mode.   for the display by elements,      '  /
   data texspy ( 747 )  /  '  the first list-9 entry with a characteristic contained within the range       '  /
   data texspy ( 748 )  /  '  [n1, n2]  is identified, and displayed (where n1 and n2 are the ordered       '  /
   data texspy ( 749 )  /  '  pair of free-format numbers which the user has inputted.  on the other hand,  '  /
   data texspy ( 750 )  /  '  with  mode  toggled for the absolute, unlimited display of list 10 rows,      '  /
   data texspy ( 751 )  /  '  then these rows (within limits of availability) are displayed.                '  /
   data texspy ( 752 )  /  '  the following plot commands are honored at all three levels (either  "outer:",'  /
   data texspy ( 753 )  /  '  "middle",  or  "inner:")  of plot dialogue:                                   '  /
   data texspy ( 754 )  /  '  stop  ---  to abort  "plot"  activity,  and return to the  "spy:"  prompt.    '  /
   data texspy ( 755 )  /  '  spy   ---  to abort  "plot"  activity,  and return to the  "spy:"  prompt.    '  /
   data texspy ( 756 )  /  '  in   ----  to transfer one level inward  (either from  "outer"  to  "middle:" '  /
   data texspy ( 757 )  /  '             or from  "middle"  to  "inner")  in the plot dialogue.             '  /
   data texspy ( 758 )  /  '  help  ---  to produce the display now being viewed.  if unqualified, messages '  /
   data texspy ( 759 )  /  '             will be restricted to the level of current operation.  but one     '  /
   data texspy ( 760 )  /  '             blank and then a qualifier ("all", "outer", "middle", or "inner")  '  /
   data texspy ( 761 )  /  '             provide the opportunity to see messages of other levels, too.      '  /
   data texspy ( 762 )  /  '  debug  --  to alter the level of diagnostic printout.  if none is now being   '  /
   data texspy ( 763 )  /  '             used, this command will turn it on.  in fact, the control is       '  /
   data texspy ( 764 )  /  '             identical to that used in response to the  "spy:"  prompt (iprspy).'  /
   data texspy ( 765 )  /  '               < < <  =======   end of any-level  commands   =======  > > >     '  /
   data texspy ( 766 )  /  '  set data -- to over-ride default data values with user-established choices.   '  /
   data texspy ( 767 )  /  '              the associated user-defined data file must be pre-stored on disk, '  /
   data texspy ( 768 )  /  '              and must have the file name  "tpparam.dat".                       '  /
   data texspy ( 769 )  /  '  tek  ---- to toggle the switch which chooses between character plotting and   '  /
   data texspy ( 770 )  /  '            vector-graphic (e.g., tektronix) plotting.  the default may be      '  /
   data texspy ( 771 )  /  '            computer dependent, or come from  "set data"  usage.                '  /
   data texspy ( 772 )  /  '  column -- to toggle the switch that chooses between 80 and 132-column widths  '  /
   data texspy ( 773 )  /  '            for character plotting.                                             '  /
   data texspy ( 774 )  /  '  set column -- to set character plot widths to any value.  but if either 80 or '  /
   data texspy ( 775 )  /  '                132 is desired (common choices), use the simpler  "column".     '  /
   data texspy ( 776 )  /  '               < < <  =======   end of outer-level  commands   =======  > > >   '  /
   data texspy ( 777 )  /  '  timespan -- to be shown the time range  [t-min, t-max]  of the plot data.     '  /
   data texspy ( 778 )  /  '  choice -- to produce a tabulation of plotable EMTP variables.   but be very   '  /
   data texspy ( 779 )  /  '            careful, since the resultant tabulation corresponds to the status   '  /
   data texspy ( 780 )  /  '            before the time-step loop was entered.  for a dynamic tabulation,   '  /
   data texspy ( 781 )  /  '            use  "choice"  in response to the  "spy:"  prompt.                  '  /
   data texspy ( 782 )  /  '  time units -- to specify the integer which characterizes the time units used  '  /
   data texspy ( 783 )  /  '                in the plotting.  this is just as with batch-mode EMTP plotting,'  /
   data texspy ( 784 )  /  '                where:  "1"  -- for degrees based on the power frequency;       '  /
   data texspy ( 785 )  /  '                        "2"  -- for cycles  based on the power frequency;       '  /
   data texspy ( 786 )  /  '                        "3,4,5"  -- for seconds, milliseconds, and microseconds '  /
   data texspy ( 787 )  /  '                        "6,7" -- for  "frequency scan"  data cases, using either'  /
   data texspy ( 788 )  /  '                                 hz or the log to base 10 of hz,  rather than t.'  /
   data texspy ( 789 )  /  '  x   ----- any garbage character (including blank) represents a request for the'  /
   data texspy ( 790 )  /  '            program to begin the interrogation for plot variables and labeling. '  /
   data texspy ( 791 )  /  '            hereafter, in response to the different prompts, several special    '  /
   data texspy ( 792 )  /  '            key words are applicable (immediately below).  upon the completion  '  /
   data texspy ( 793 )  /  '            of all such information, there is an automatic transfer to the      '  /
   data texspy ( 794 )  /  '            "inner:"  level of program dialogue.  but first, options are:       '  /
   data texspy ( 795 )  /  '    repeat >> to reuse all former plot labels, send  "repeat"  when first asked '  /
   data texspy ( 796 )  /  '              for label information (which begins with the super title).  for   '  /
   data texspy ( 797 )  /  '              the first plot, "former labels" are all blank (initialization).   '  /
   data texspy ( 798 )  /  '    back >>>> to abort input of plot variables, and return to "middle:" prompt. '  /
   data texspy ( 799 )  /  '    end  >>>> to terminate all indeterminate inputs.  included are the three    '  /
   data texspy ( 800 )  /  '              classes of plot variables, and lines of case-title text.          '  /
   data texspy ( 801 )  /  '    last >>>> to terminate all plot-variable input, and begin specifying labels.'  /
   data texspy ( 802 )  /  '    blank >>> sending nothing (just a carriage return) will usually reuse the   '  /
   data texspy ( 803 )  /  '              old datum (e.g., node name, or line of case-title text).  old data'  /
   data texspy ( 804 )  /  '              are usually displayed within parentheses for this reason.         '  /
   data texspy ( 805 )  /  '    flush >>> to rewind the pointer which counts the lines of case-title text.  '  /
   data texspy ( 806 )  /  '              the text then remains, to be accepted or overwritten line by line.'  /
   data texspy ( 807 )  /  '    playback> to display the entire present case title.  this command is legal  '  /
   data texspy ( 808 )  /  '              at any point before the "end"  which freezes the case title.      '  /
   data texspy ( 809 )  /  '  label  -- to skip around the plot-variable specification (see "x" above), and '  /
   data texspy ( 810 )  /  '            begin the input of plot labels.  plot variables remain unchanged.   '  /
   data texspy ( 811 )  /  '               < < <  =======   end of middle-level  commands   =======  > > >  '  /
   data texspy ( 812 )  /  '  extrema -- to toggle the switch that decides whether or not variable extrema  '  /
   data texspy ( 813 )  /  '             of subsequent plots are to be displayed.  such output precedes the '  /
   data texspy ( 814 )  /  '             associated plot (as does that of  "level"  below).  the program    '  /
   data texspy ( 815 )  /  '             then pauses before drawing the graph, waiting for the user to send '  /
   data texspy ( 816 )  /  '             a blank.  if the user wants to skip the plot and return to the     '  /
   data texspy ( 817 )  /  '             "middle:"  level of dialogue, send  "no plot".                     '  /
   data texspy ( 818 )  /  '  level -- to toggle the switch that decides whether or not level-triggers for  '  /
   data texspy ( 819 )  /  '           variables are to be activated.  if such triggers are being turned on,'  /
   data texspy ( 820 )  /  '           the program will next request a level vector.  the response is using '  /
   data texspy ( 821 )  /  '           free-format.  the same pause exists as with  "extrema".              '  /
   data texspy ( 822 )  /  '  smooth -- to change the tolerance which is used to discard plot points.       '  /
   data texspy ( 823 )  /  '  size   -- to change the length of the time axis of character plots.           '  /
   data texspy ( 824 )  /  '  show   -- to display the current values of many important plot parameters.    '  /
   data texspy ( 825 )  /  '  factor -- to specify a new vector of multiplicative scaling factors for plot  '  /
   data texspy ( 826 )  /  '            variables (the "a" of  z = a*y + b ).   zero is taken to mean unity,'  /
   data texspy ( 827 )  /  '            and free-format is used.                                            '  /
   data texspy ( 828 )  /  '  offset -- like  "factor",  only to specify the vector of constants  "b".      '  /
   data texspy ( 829 )  /  '  rescale-- to return to natural scaling (i.e.,  a=1.0 ,  b=0.0 ) for all plot  '  /
   data texspy ( 830 )  /  '            variables.  this is used to erase previous  "factor"  or  "offset". '  /
   data texspy ( 831 )  /  '  limits -- to specify new minimum and maximum values for the vertical axis of  '  /
   data texspy ( 832 )  /  '            plots.  send  "0,0"  to cancel previous manually-specified limits,  '  /
   data texspy ( 833 )  /  '            and return to the automatic scaling of the computer.                '  /
   data texspy ( 834 )  /  '  average-- to change the limit on the number of consecutive oscillations after '  /
   data texspy ( 835 )  /  '            which the averaging of successive ordinates is to be instituted.    '  /
   data texspy ( 836 )  /  '  time   -- to specify time-axis limits t-min  and  t-max  of the plot.  this   '  /
   data texspy ( 837 )  /  '            must be done at least once, for the first plot, unless  "timespan"  '  /
   data texspy ( 838 )  /  '            was issued at the  "middle:"  level (resulting in full time range). '  /
   data texspy ( 839 )  /  '  all time--this is a request for a plot over the full time range of the data.  '  /
   data texspy ( 840 )  /  '            it functions correctly only if  "timespan"  was previously used at  '  /
   data texspy ( 841 )  /  '            the  "middle:"  level.                                              '  /
   data texspy ( 842 )  /  '  blank  -- a blank (just a <cr>) is interpreted as a request for a plot using  '  /
   data texspy ( 843 )  /  '            time-axis scaling as for the preceding plot (or the full range, if  '  /
   data texspy ( 844 )  /  '            no previous plots existed, but  "timespan"  was ordered).           '  /
   data texspy ( 845 )  /  '  cursor -- to toggle the switch that indicates whether or not cursor input is  '  /
   data texspy ( 846 )  /  '            expected after the next vector plot.  when the cursor is switched to'  /
   data texspy ( 847 )  /  '            "on",  following the plot there will be keyboard input following the'  /
   data texspy ( 848 )  /  '            positioning of the cursor:                                          '  /
   data texspy ( 849 )  /  '     p  >>>>> to mark another cursor point (wherever the cursor is sitting)     '  /
   data texspy ( 850 )  /  '     e  >>>>> to terminate such marking input (end of cursor use, actually).    '  /
   data texspy ( 851 )  /  '     show >>> to produce a tabulation of all cursor points (previous "p" usage).'  /
   data texspy ( 852 )  /  '     slope >> to produce  dx,  dy,  f, and  dy/dx  of any pair of points that   '  /
   data texspy ( 853 )  /  '              the user is interested in.  after the last such  (m,k)  pair of   '  /
   data texspy ( 854 )  /  '              point numbers which are of interest,  send  "0,0"  to terminate.  '  /
   data texspy ( 855 )  /  '     end  >>> to terminate all cursor displays, and return to  "inner:"  prompt.'  /
   data texspy ( 856 )  /  '  x-y plot--to toggle the switch which chooses between regular plotting as a    '  /
   data texspy ( 857 )  /  '            function of time (the default), and x-y plotting.                   '  /
   data texspy ( 858 )  /  '  rollv  -- to toggle the switch which produces a  rolling  vector plot.   it is'  /
   data texspy ( 859 )  /  '            normal to turn on such rolling after the basic plot is in place, and'  /
   data texspy ( 860 )  /  '            the axis scaling is judged to be appropriate.  then, after  "rollv",'  /
   data texspy ( 861 )  /  '            the user will normally send "spy"  to return to the  "spy:"  prompt '  /
   data texspy ( 862 )  /  '            (from which  "go"  will be required to actually make the plot  roll,'  /
   data texspy ( 863 )  /  '            if  "break"  or  "lock"  were in effect at that point).   should the'  /
   data texspy ( 864 )  /  '            user wish to cancel a rolling vector plot from spy, send  "noroll"  '  /
   data texspy ( 865 )  /  '            (which will stop both  rolling  vector and character plots).        '  /
   data texspy ( 866 )  /  '  rollc  -- to toggle the switch which produces a  rolling  character plot.  see'  /
   data texspy ( 867 )  /  '            preceding  "rollv"  for details (functioning is comparable).        '  /
   data texspy ( 868 )  /  '               < < <  =======   end of inner-level  commands   =======  > > >   '  /

   data kbegtx (  1 )   /     1   /
   data kbegtx (  2 )   /     4   /
   data kbegtx (  3 )   /     9   /
   data kbegtx (  4 )   /    22   /
   data kbegtx (  5 )   /    29   /
   data kbegtx (  6 )   /    41   /
   data kbegtx (  7 )   /    53   /
   data kbegtx (  8 )   /    64   /
   data kbegtx (  9 )   /    69   /
   data kbegtx ( 10 )   /    79   /
   data kbegtx ( 11 )   /    88   /
   data kbegtx ( 12 )   /    97   /
   data kbegtx ( 13 )   /   112   /
   data kbegtx ( 14 )   /   119   /
   data kbegtx ( 15 )   /   136   /
   data kbegtx ( 16 )   /   141   /
   data kbegtx ( 17 )   /   154   /
   data kbegtx ( 18 )   /   158   /
   data kbegtx ( 19 )   /   162   /
   data kbegtx ( 20 )   /   202   /
   data kbegtx ( 21 )   /   217   /
   data kbegtx ( 22 )   /   228   /
   data kbegtx ( 23 )   /   237   /
   data kbegtx ( 24 )   /   242   /
   data kbegtx ( 25 )   /   252   /
   data kbegtx ( 26 )   /   263   /
   data kbegtx ( 27 )   /   294   /
   data kbegtx ( 28 )   /   303   /
   data kbegtx ( 29 )   /   317   /
   data kbegtx ( 30 )   /   325   /
   data kbegtx ( 31 )   /   333   /
   data kbegtx ( 32 )   /   340   /
   data kbegtx ( 33 )   /   345   /
   data kbegtx ( 34 )   /   361   /
   data kbegtx ( 35 )   /   377   /
   data kbegtx ( 36 )   /   381   /
   data kbegtx ( 37 )   /   425   /
   data kbegtx ( 38 )   /   436   /
   data kbegtx ( 39 )   /   446   /
   data kbegtx ( 40 )   /   449   /
   data kbegtx ( 41 )   /   453   /
   data kbegtx ( 42 )   /   456   /
   data kbegtx ( 43 )   /   467   /
   data kbegtx ( 44 )   /   471   /
   data kbegtx ( 45 )   /   482   /
   data kbegtx ( 46 )   /   486   /
   data kbegtx ( 47 )   /   490   /
   data kbegtx ( 48 )   /   497   /
   data kbegtx ( 49 )   /   503   /
   data kbegtx ( 50 )   /   514   /
   data kbegtx ( 51 )   /   559   /
   data kbegtx ( 52 )   /   586   /
   data kbegtx ( 53 )   /   621   /
   data kbegtx ( 54 )   /   630   /
   data kbegtx ( 55 )   /   641   /
   data kbegtx ( 56 )   /   647   /
   data kbegtx ( 57 )   /   657   /
   data kbegtx ( 58 )   /   661   /
   data kbegtx ( 59 )   /   665   /
   data kbegtx ( 60 )   /   679   /
   data kbegtx ( 61 )   /   683   /
   data kbegtx ( 62 )   /   691   /
   data kbegtx ( 63 )   /   720   /
   data kbegtx ( 64 )   /   725   /
   data kbegtx ( 65 )   /   752   /
   data kbegtx ( 66 )   /   766   /
   data kbegtx ( 67 )   /   777   /
   data kbegtx ( 68 )   /   812   /
   data kbegtx ( 69 )   /   869   /
   data  kslowr  /   3   /
 end module dekspy

!
! end of file dekspy.f90
!
