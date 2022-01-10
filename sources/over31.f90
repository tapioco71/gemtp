!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over31.f90
!

!
! subroutine subr31.
!

subroutine subr31
  use blkcom
  use labcom, only : buslst, ibsout, ibrnch, jbrnch, karray, ev
  use deck31
  use tracom
  use bcddat
  use bcdtim
  use movcop
  use strcom
  use freedom
  use pckcom
  implicit none
  !     flag-1.   begin class-1  /blank/  variables
  !     flag-2.   begin class-2  /blank/  variables
  !               (floating-point numeric usage only, with scalars
  !               preceding arrays).
  !     flag-3.   begin class-3  /blank/  variables
  !               (integer-numeric usage only, with arrays
  !                preceding scalars).
  character(8) :: arch10(2), alpha(52)
  character(8) :: blanka(1)
  character(8) :: cstxt
  character(8) :: daytim
  character(8) :: headl(3), horzl(4)
  character(8) :: pltle
  character(8) :: sext(13), slot(8)
  character(8) :: text1, text2, text3, text4, text5, text6, text7, text8
  character(8) :: text9, text10, text11, text12, text13, text14, text15
  character(8) :: text16, text18, text19, text20, text22, text23, text24
  character(8) :: text25, text26, text27, text28, text29, text30, text31
  character(8) :: text32, text33, textax
  character(8) :: vertl(3)
  integer(4) :: i, ialf, ib, ibase, icp, iend, ihs, il, indx, intd8(150)
  integer(4) :: intd9(150), iofbus, iofibr, iofjbr, iovfl, ip1, ipl, ipl1, ipl2, ipontr
  integer(4) :: istore, isww, iswx, itimes, itp
  integer(4) :: j, j1, j2, jalf, jbegbc, jchar, jcnt, jcol, jdumy, jend, jhms
  integer(4) :: jhmsp, jk, jovfl, jplt, jplt2, jpntr, jpts, jsl, jslot, jstp
  integer(4) :: k, kalcom, kbound, kend, kikoy, klm, kprhd, kold, kpgrid
  integer(4) :: kpl, kplt, kpltq, kscale, ktrm
  integer(4) :: l, lcnt, limbin, linlim, ll0, ll1, ll2, ll3, ll6, ll10, ll16, ll18
  integer(4) :: ll24, ll78, llmin1, llmin3, lltemp, lnck, lnflg, long1, long2, long3
  integer(4) :: lplt
  integer(4) :: m, maxev, maxevk, mdpt, mhoriz, mm0, mm1, mm2, mm3, mm4, mm6, mm7, mm8
  integer(4) :: mm9, mm11, mm13, mmmin1, mpage, mplot(4), mrgn, mulplt(5)
  integer(4) :: n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, ncrv, ndx1, nfour
  integer(4) :: numbco, numbvo, numnam
  real(8) :: a
  real(8) :: c1e12, countp
  real(8) :: bxsing(150), tstep, xyplot, bx(150), fl90
  real(8) :: d1, d2, d3, d4, d4fact, d5, d6, d7, d8, d9, d23, denom, disqr, dlen
  real(8) :: dstrt, dx, dxl, dxl2, dy, dyl
  real(8) :: enumr, evbasx, evbasy, evp, evdh, evdp, evh, evmx, evmxf, expnt
  real(8) :: fl10, fl1p5, fl2p5, fl3, fl3p5, flong1, fltwo, fourth
  real(8) :: ha, half, hdif, hgt1, hgt2, hhi, hhpt, hlo, hmax, hmin, hms, hpi
  real(8) :: hpil, hpt, hvec
  real(8) :: one, ricp
  real(8) :: stp
  real(8) :: taxmax, term, tmult, tolrce, tsing, xyshor
  real(8) :: vchnge, vdif, vh, vhs, vhs1, vmax, vmaxl, vmaxr, vmin, vminl, vminr, vnew
  real(8) :: vold, vploff, vrtnum, vs, vsnew, vvec, zero
  real(8) :: xin(150)
  !
  !     declaration2   intd8, intd9, maxev, karray, lltemp
  !     declaration2   long1, long2, long3, jhmsp
  !     declaration2   mmmin1, mm0, mm1, mm2, mm3, mm4, mm6
  !     declaration2   mm7, mm8, mm9, mm11, mm13
  !  common /ldec31/  kalcom
  !  dimension array(1), evdoub(1)
  !  dimension intd8(150), intd9(150),  bxsing(150)
  !  dimension arch10(2), mulplt(5), blanka(1), busvec(6)
  !  dimension alpha(52), mplot(4), headl(3), vertl(3)
  !  dimension bx(150), slot(8), horzl(4), sext(13)
  !  dimension buslst(1)
  !  dimension ibsout(1)
  !  dimension ibrnch(1), jbrnch(1)
  !  dimension xin(150)
  dimension daytim(3), textax(32), xyplot(9)
  dimension iswx(4), evh(4), evdh(4), isww(4), itimes(4), kpltq(4)
  dimension cstxt(13), pltle(52), xyshor(8)
  dimension vrtnum(14), jpntr(4)
  dimension text14(2), text16(2)
  dimension lltemp(20)
  equivalence (tstep, xin(1)), (bx(1), xin(2))
  !
  !  equivalence (karray(1), array(1), ev(1), buslst(1), evdoub(1), ibsout(1), ibrnch(1), jbrnch(1))
  !  equivalence (busvec(1), bus1)
  !  equivalence (moncar(2), kbase)
  !
  !
  data text1      / 'print ' /
  data text2      / 'head o' /
  data text3      / 'n     ' /
  data text4      / 'ff    ' /
  data text5      / 'smooth' /
  data text6      / 'branch' /
  data text7      / 'height' /
  data text8      / 'margin' /
  data text9      / 'calcom' /
  data text10     / 'p plot' /
  data text11     / 'printe' /
  data text12     / 'r plot' /
  data text13     / 'p prin' /
  data text14(1)  / 'plot t' /
  data text14(2)  / 'ype   ' /
  data text15     / 'page  ' /
  data text16(1)  / 'node n' /
  data text16(2)  / 'ames  ' /
  data text18     / 'pen ch' /
  data text19     / 'oice  ' /
  data text20     / 'a1    ' /
  data text22     / 'plot l' /
  data text23     / 'ine li' /
  data text24     / 'mit   ' /
  data text25     / 'superi' /
  data text26     / 'mpose ' /
  data text27     / 'scale ' /
  data text28     / 'x-y pl' /
  data text29     / 'ot    ' /
  data text30     / 'fourie' /
  data text31     / 'r on  ' /
  data text32     / 'r off ' /
  data text33     / 'bounds' /
  data textax(1)  / 'degree' /
  data textax(2)  / 's base' /
  data textax(3)  / 'd on 6' /
  data textax(4)  / '0 hz  ' /
  data textax(5)  / 'cycles' /
  data textax(6)  / ' based' /
  data textax(7)  / ' on 60' /
  data textax(8)  / ' hz   ' /
  data textax(9)  / 'second' /
  data textax(10) / 's     ' /
  data textax(11) / '      ' /
  data textax(12) / '      ' /
  data textax(13) / 'millis' /
  data textax(14) / 'econds' /
  data textax(15) / '      ' /
  data textax(16) / '      ' /
  data textax(17) / 'micros' /
  data textax(18) / 'econds' /
  data textax(19) / '      ' /
  data textax(20) / '      ' /
  data textax(21) / 'freque' /
  data textax(22) / 'ncy in' /
  data textax(23) / ' hertz' /
  data textax(24) / '      ' /
  data textax(25) / 'log10 ' /
  data textax(26) / 'freque' /
  data textax(27) / 'ncy in' /
  data textax(28) / ' hertz' /
  data textax(29) / 'space ' /
  data textax(30) / 'for   ' /
  data textax(31) / 'x-y   ' /
  data textax(32) / 'plot  ' /
  data kscale     / 0 /
  data llmin3     / -3 /
  data llmin1     / -1 /
  data ll0        / 0 /
  data ll1        / 1 /
  data ll2        / 2 /
  data ll3        / 3 /
  data ll6        / 6 /
  data ll10       / 10 /
  data ll16       / 16 /
  data ll18       / 18 /
  data ll24       / 24 /
  data ll78       / 78 /
  data mplot / 1, 1, 1, 1 /
  !
  blanka(1) = blank
  long1 = nchain
  if (kburro .eq. 1) long1 = 29
  call dimens (lltemp(1), long1, trash, trash)
  do i = 1, 9999, 2
     if (lltemp(i) .ne. 0) exit
     maxev = lltemp(i + 1) * nbyte(6) / nbyte(5)
     go to 5655
  end do
  call stoptp
5655 maxev = maxev - 8
  c1e12 = 1.e36
  !     The following variables are used as floating-point arguments of
  !     calcomp subroutine calls.   They are just constants, which are
  !     defined using equal signs so either single or double precision
  !     is possible without program changes.
  zero = 0.d0
  one = 1.0d0
  hgt1 = 0.12d0
  hgt2 = 0.2d0
  fourth = .25d0
  half = 0.5d0
  fl1p5 = 1.5d0
  fltwo = 2.0d0
  fl2p5 = 2.5d0
  fl3 = 3.0d0
  fl3p5 = 3.5d0
  fl10 = 10.d0
  fl90 = 90.
  !     Other calcomp arguments are listed here just for convenience ....
  !     countp, dlen, dstrt, dy, hpi, ricp, szbed, szplt, vmin, vhs, vhs1
  !     the following are long integers (may be double length),
  !     Used as arguments of non-calcomp subroutine calls.  "mm"
  mmmin1 = -1
  mm0 = 0
  mm1 = 1
  mm2 = 2
  mm3 = 3
  mm4 = 4
  mm6 = 6
  mm7 = 7
  mm8 = 8
  mm9 = 9
  mm11 = 11
  mm13 = 13
  xyplot(1) = 0.0d0
  tolrce = .0001d0
  vs = 1.0d0
  vhs = 8.0d0
  vh = 10.0d0
  taxmax = 0.0d0
  vploff = 0.0d0
  mrgn = 2
  kbound = 0
  iout = 1
  mpage = 0
  kpgrid = 0
  mulplt(1) = 0
  linlim = 100
  !     dimension of buffer integer arrays   'intd8'  and  'intd9' .
  limbin = 150
  jdumy = 999
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 962) llbuff, kill, lnpin, nsmth, szbed, szplt,  date1, tclock
962 format (/, "  Begin  'subr31' .  llbuff    kill   lnpin   nsmth", 10x, 'szbed', 10x, 'szplt', 5x, 'date1', 4x, 'tclock', /, 18x, 4i8, 2e15.5, 2(2x, 2a4))
  do j = 1, 4
     kpen(j) = 1
  end do
  kprhd = 1
  nfour = 0
  kalcom = 0
  if (kill .eq. 9999) go to 2740
  long1 = 6
  long2 = 1
  long3 = 1
  do i = 1, 13
     cstxt(i) = blank
     !     call packch (blanka(1), sext(1), long1, long2, long3)
     call pack (blanka(1), sext(1), long1, long2, long3)
     long2 = long2 + 6
  end do
  do j = 1, 52
     pltle(j) = blank
  end do
  countp = 0.0d0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 982)  sext
982 format (/, " Vector  'sext'  as  13a6 .", 13a6, /, 1x, 13a6)
  !     segmented, 1, vax e/t can skip translation of rewind:
1000 rewind lunit(4)
  if (iout .ge. 2) write (unit = lunit(6), fmt = 1005)
1005 format (///, 1x)
  tstep = 0.0d0
  ialf = 0
  !     read input card using cimage
1050 call cimage
  read (unit = abuff, fmt = 1060) itp, (aupper(i), i = 1, 13)
1060 format (i2, 13a6)
  if (iplot .lt. 0) go to 1070
  if (lunit(4) .le. 0) go to 1070
  if (to_lower (aupper(1)) .ne. text15) go to 10452
  mpage = 1
  go to 1050
10452 if (to_lower (aupper(1)) .ne. text9) go to 1052
  if (to_lower (aupper(2)) .ne. text10) go to 1054
  iout = 1
  write (unit = kunit6, fmt = 1051)
1051 format ('+Request for calcomp plot.')
  go to 1050
1052 if (to_lower (aupper(1)) .ne. text11) go to 1061
  if (to_lower (aupper(2)) .ne. text12) go to 1061
  iout = 2
  write (unit = kunit6, fmt = 1053)
1053 format ('+Request for line printer plot.')
  go to 1050
1054 if (to_lower (aupper(2)) .ne. text13) go to 1061
  iout = 3
  write (unit = kunit6, fmt = 1055)
1055 format ('+Request for calcomp and line printer plots.')
  go to 1050
1061 if (to_lower (aupper(1)) .ne. text1) go to 1067
  if (to_lower (aupper(2)) .ne. text2) go to 1067
  if (to_lower (aupper(3)) .ne. text3) go to 1064
  kprhd = 1
  write (unit = kunit6, fmt = 1062)
1062 format ('+Request for typing of plot characters.')
  go to 1050
1064 if (to_lower (aupper(3)) .ne. text4) go to 1067
  kprhd = 0
  write (unit = kunit6, fmt = 1065)
1065 format('+Request for drawing of plot characters.')
  go to 1050
1067 if (to_lower (aupper(1)) .ne. text33) go to 5843
  kbound = 1
  write (unit = kunit6, fmt = 4865)
4865 format('+Scale y axis so no curve exceeds limits.')
  go to 1050
5843 if (to_lower (aupper(1)) .ne. text18) go to 1079
  if (to_lower (aupper(2)) .ne. text19) go to 1079
  read (unit = abuff, fmt = 1072) kpgrid, isww
1072 format (16x, 5i8)
  write (unit = kunit6, fmt = 1073) kpgrid, isww
1073 format ('+Grid & pen choices.', 5i5)
  do i = 1, 4
     if (isww(i) .gt. 0) kpen(i) = isww(i)
  end do
  go to 1050
1079 if (to_lower (aupper(1)) .ne. text22) go to 7392
  if (to_lower (aupper(2)) .ne. text23) go to 7392
  if (to_lower (aupper(3)) .ne. text24) go to 7392
  read (unit = abuff, fmt = 7381) linlim
7381 format (24x, 3i8)
  write (unit = kunit6, fmt = 7386) linlim
7386 format ('+Line limit for sparse printer plots =', i8)
  go to 1050
7392 if (to_lower (aupper(1)) .ne. text25) go to 7406
  if (to_lower (aupper(2)) .ne. text26) go to 7406
  read (unit = abuff, fmt = 7381) mulplt(1), mulplt(4), mulplt(5)
  if (mulplt(1) .le. 0) mulplt(1) = 1
  write (unit = kunit6, fmt = 7398)  mulplt(1), mulplt(4), mulplt(5)
7398 format ('+Graph superposition.', 3i8)
  mulplt(2) = 0
  mulplt(3) = 0
  go to 1050
7406 if (to_lower (aupper(1)) .ne. text27) go to 7627
  read (unit = abuff, fmt = 7613) d4
7613 format (24x, e8.0)
  write (unit = kunit6, fmt = 7618) d4
7618 format ('+Scaling factor for plots.', f10.5)
  d4 = 1.0d0 / d4
  if (kalcom .ne. 1) go to 7622
  call factor (d4)
  go to 1050
7622 kscale = 1
  d4fact = d4
  go to 1050
7627 if (to_lower (aupper(1)) .ne. text28) go to 7661
  if (to_lower (aupper(2)) .ne. text29) go to 7661
  write (unit = kunit6, fmt = 7636)
7636 format ('+Plot one emtp variable against another.')
  textax(29) = aupper(3)
  textax(30) = aupper(4)
  textax(31) = aupper(5)
  textax(32) = aupper(6)
  !     read input card using cimage
  call cimage
  if (kolbeg .gt. 0) go to 7644
  read (unit = abuff, fmt = 7641) (xyplot(i), i = 1, 3)
7641 format (10e8.0)
  go to 7648
7644 nfrfld = 3
  !  call frefld (xyplot)
  call free (xyplot)
7648 if (xyplot(1) .le. 0.0d0) xyplot(1) = 8.0d0
  if (xyplot(1) .eq. 9999.0d0) xyplot(1) = 0.0d0
  write (unit = kunit6, fmt = 7649) (xyplot(i), i = 1, 3)
7649 format ('+x-axis.', 3e13.4)
  !     read input card using cimage
  call cimage
  if (kolbeg .gt. 0) go to 7651
  read (unit = abuff, fmt = 7641) (xyplot(i), i = 4, 9)
  go to 7653
7651 nfrfld = 6
  !  call frefld (xyplot(4 :))
  call free (xyplot(4 :))
7653 if (xyplot(4) .le. 0.0d0) xyplot(4) = 8.0d0
  if (xyplot(7) .le. 0.0d0) xyplot(7) = 10.0d0
  if (xyplot(9) .le. 0.0d0) xyplot(9) = 10.0d0
  write (unit = kunit6, fmt = 7654) (xyplot(i), i = 4, 6)
7654 format ('+y-axis.', 3e13.4)
  go to 1050
7661 if (aupper(1) .ne. text30) go to 7679
  if (aupper(2) .ne. text31) go to 7671
  read (unit = abuff, fmt = 7381) nfour
  if (nfour .le. 1) nfour = 30
  write (unit = kunit6, fmt = 7664) nfour
7664 format ('+Fourier series started.  nfour =', i8)
  maxevk = maxevk * nbyte(5) / nbyte(3)
  go to 1050
7671 if (to_lower (aupper(2)) .ne. text32) go to 7679
  nfour = 0
  write (unit = kunit6, fmt = 7674)
7674 format ('+Fourier series ended.  Back to plots.')
  maxevk = maxevk * nbyte(3) / nbyte(5)
  go to 1050
7679 select case (itp)
  case (0)
     go to 1070

  case (1)
     go to 1160

  case (2)
     go to 1550
  end select
  lstat(16) = itp
  kill = 76
  lstat(19) = 1070
  go to 9200
1070 do i = 1, 13
     if (aupper(i) .ne. blank) go to 1106
  end do
  write (unit = kunit6, fmt = 1100)
1100 format ('+Blank card terminating plot spec. cards.')
  call interp
  if (taxmax .eq. 0.0d0) go to 2720
  if (kalcom .eq. 0) go to 2720
  n8 = -6666
  go to 2656
1106 if (iplot .ge. 0 .and. lunit(4) .gt. 0) go to 1120
  write (unit = kunit6, fmt = 1108)
1108 format ('+Plot card ignored in quest for start of new case.')
  go to 1050
1120 if (ialf .lt. 52) go to 1126
  write (unit = kunit6, fmt = 1122)
1122 format ('+Overflow subtitle card discarded.')
  go to 1050
1126 ialf = ialf + 13
  !  call packch (aupper(1), alpha(ialf - 12), mm6, mm1, mm13)
  call pack (aupper(1), alpha(ialf - 12), mm6, mm1, mm13)
  j1 = ialf - 12
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 126) (alpha(j), j = j1, ialf)
126 format (' At 1126 packed subtitle text.', /, 1x, 13a10)
  k = 0
  do j = j1, ialf
     k = k + 1
     pltle(j) = aupper(k)
  end do
  write (unit = kunit6, fmt = 1140)
1140 format ('+Plot subtitle card.')
  go to 1050
  !1550 call packch (aupper(1), sext(1), mm6, mm1, mm13)
1550 call pack (aupper(1), sext(1), mm6, mm1, mm13)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 155) sext
155 format (' At 1550 packed case title text.', /, 1x, 13a10)
  do j = 1, 13
     cstxt(j) = aupper(j)
  end do
  write (unit = kunit6, fmt = 1155)
1155 format ('+Plot case-title text.')
  go to 1050
  !1160 call copya ( blank, slot(1), mm8 )
1160 call copy (blank, slot(1 :), mm8)
  jslot = 3
  if (kolbeg .gt. 0) go to 7402
  read (unit = abuff, fmt = 1180) itp, icp, ihs, hpi, hmin, hmax, vmin, vmax, (slot(j), j = 1, 4), bus1, bus2, bus3, bus4, bus5, bus6
1180 format (i2, 2i1, e3.0, 2e4.0, e5.0, e4.0, 4a6, 2(2a6, a4))
  go to 7405
7402 nfrfld = 1
  !  call freone (flong1)
  call free (flong1)
  itp = int (flong1, kind (itp))
  !  call freone (flong1)
  call free (flong1)
  icp = int (flong1, kind (icp))
  !  call freone (flong1)
  call free (flong1)
  ihs = int (flong1, kind (ihs))
  nfrfld = 5
  !  call frefld (voltbc)
  call free (voltbc)
  hpi = voltbc(1)
  hmin = voltbc(2)
  hmax = voltbc(3)
  vmin = voltbc(4)
  vmax = voltbc(5)
  nfrfld = 4
  nright = -1
  !  call freone (flong1)
  call free (flong1)
  if (kill .gt. 0) go to 9200
  nright = 0
  do j = 1, 4
     slot(j) = texta6(j)
  end do
  !     temporarily blank out the titles in free-format case
  bus1 = blank
  bus2 = blank
  bus3 = blank
  bus4 = blank
  bus5 = blank
  bus6 = blank
7405 if (isprin .ne. 43) go to 7408
  if (degmax .eq. 0.0d0) go to 7408
  ihs = 4
  icp = 4
  hpi = tmax / hmin
  hmin = 0.0d0
  hmax = tmax
  if (slot(1) .eq. text20) go to 7408
  vmin = degmin
  vmax = degmax
7408 hmin = hmin * (1.0d0 - flzero)
  hmax = hmax * (1.0d0 + flzero)
  if (xyplot(1) .eq. 0.0d0) go to 7413
  hpi = (hmax - hmin) * 0.1d0
7413 ncrv = icp
  if (to_lower (slot(1)) .ne. text8) go to 83
  write (unit = kunit6, fmt = 80) ihs
80 format ("+Graph separation card.   'ksep' =", i3 )
  if (ihs .ge. 2) go to 82
  write (unit = lunit(6), fmt = 81) ihs, mrgn
81 format (' The specified graph separation of ', i2, ' inches is considered too small and is reset to ', i1, ' inches. ')
  ihs = mrgn
82 mrgn = ihs
  go to 1050
83 if (slot(1) .ne. text7) go to 1185
  write (unit = kunit6, fmt = 1181) hmin, hmax
1181 format ('+Graph size adjustment card.', 2e11.3)
  d1 = hmin + hmax + vploff
  if (d1 .le. szplt) go to 11182
  write (unit = lunit(6), fmt = 11181) hmin, hmax, d1, szplt
11181 format (5x, 'Note ---- The requested bottom margin of ', e9.3, ' and graph height of ', e9.3, ' requires a total', /, 5x, 'paper height of ', e9.3, '.  This is greater than the height specified in =call paprsz=, which is ', e9.3, /, 5x, 'the requested values will be ignored and the last specified (or default if no height values were ever given)', /, 5x, 'will be used.')
  if (taxmax .gt. 0.0d0) write (unit = lunit(6), fmt = 21181) vploff
21181 format (5x, 'Actually, the just-quoted paper height is the paper height minus the offset height of ', e13.4, '   for', /, 5x, 'the one or more graphs which have already been drawn vertically below the upcoming plot.   The user should not', /, 5x, 'increase vertical dimensions unless he is at the bottom of the paper, or unless he is sure that he has sufficient', /, 5x, 'vertical space left on the paper for at least one more plot.   The size adjustment remains cancelled.')
  go to 1050
11182 vsnew = hmin - vs
  if (kalcom .eq. 1) call plot (zero, vsnew, llmin3)
  vs = hmin
  vhs = hmax
  vh = vhs + vs + 1.0
  go to 1050
1185 if (to_lower (slot(1)) .ne. text6) go to 1188
  jslot = 7
  write (unit = kunit6, fmt = 1182)  hpi, hmin, hmax
1182 format ('+** Plot card. ', 2x, 3e11.3)
  call interp
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 1187) ncrv, slot
1187 format (2x, i1, 21x, 8a6)
  if (ncrv .eq. 0) ncrv = 4
  write (unit = kunit6, fmt = 1184)
1184 format ('+Continuation to read branch node-pair names.')
  call interp
  go to 1195
1188 if (to_lower (slot(1)) .ne. text5) go to 1195
  tolrce = vmin * vmin
  write (unit = kunit6, fmt = 1189) vmin
1189 format ('+Redefinition of smoothing tolerance.', 2x, e10.2)
  d1 = 0.1d0
  if (vmin .gt. d1) write (unit = lunit(6), fmt = 1190) vmin
1190 format (' ****WARNING****  Requested smoothing tolerance of ', e11.3, ' may cause inaccurate plot.')
  go to 1050
1195 if (icp .eq. 8) go to 7412
  if (icp .eq. 9) go to 7412
  if (icp .gt. 0 .and. icp .le. 4) go to 7412
  write (unit = lunit(6), fmt = 7407) icp
7407 format (' Illegal plot-variable type code  "icp" =', i3, ' .   This plot request is cancelled.')
  go to 1000
  !7412 call packch (busvec(1), headl(1), mm6, mm1, mm2)
7412 call pack (busvec(1), headl(1), mm6, mm1, mm2)
  !  call packch (busvec(3), headl(1), mm4, mm13, mm1)
  call pack (busvec(3), headl(1), mm4, mm13, mm1)
  !  call packch (busvec(4), vertl(1), mm6, mm1, mm3)
  call pack (busvec(4), vertl(1), mm6, mm1, mm3)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1196) headl, vertl
1196 format (' At 1195 packed plot title text.', 3a10, /, ' at 1195 packed vert axis label.', 3a10 )
  if (jslot .ne. 3) go to 4589
  write (unit = kunit6, fmt = 1182)  hpi, hmin, hmax
  call interp
4589 if (hpi .ne. 0.0d0) go to 1192
  !     read input card using cimage
  call cimage
  if (kolbeg .gt. 0) go to 8212
  read (unit = abuff, fmt = 1183) hpi, hmin, hmax, vmin, vmax
1183 format (5e16.0)
  go to 8215
8212 nfrfld = 5
  !  call frefld (voltbc)
  call free (voltbc)
  hpi  = voltbc(1)
  hmin = voltbc(2)
  hmax = voltbc(3)
  vmin = voltbc(4)
  vmax = voltbc(5)
  if (kill .gt. 0) go to 9200
8215 write (unit = kunit6, fmt = 1186)
1186 format ('+Re-read of floating-point fields for accuracy.')
  call interp
1192 if (nfour .eq. 0) go to 21192
  d23 = hpi - 7.6d0
  if (d23 .gt. -.001d0 .and. d23 .lt. 0.001d0) nfour = -nfour
  go to 21220
21192 if (hpi .le. 0.0d0) go to 1193
  if (hmax .le. 0.0d0) go to 1193
  if (hmax .gt. hmin) go to 1197
1193 write (unit = lunit(6), fmt = 1194) hpi, hmin, hmax
1194 format (5x, 'Plot card error.   Time-axis specification is illegal.   This plot request is being skipped.', /, 20x, 3e15.5)
  go to 1000
1197 d1 = (hmax - hmin) / hpi
  hpil = hpi
  if (iout .eq. 2) go to 1227
  if (d1 .le. szbed) go to 1227
  write (unit = lunit(6), fmt = 1224) hpi, hmin, hmax, d1
1224 format (5x, 'note ----- a time scale of ', e13.4, ' was read from columns 5-7 of the preceding plot-request card.', /, 5x, 'together with the requested minimum of ', e13.4, ' units (read from columns 8-11) and the requested', /, 5x, 'maximum of ', e13.4, ' units (read from columns 12-15), this implies a plot of length ', e13.4)
  write (unit = lunit(6), fmt = 1225) szbed
1225 format ('+', 106x, ', which exceeds', /, 5x, 'the currently imposed flat-bed limit of ', e13.4, ' inches.  The scale on the time axis will be changed by the EMTP so', /, 5x, 'that the specified range covers exactly 12 inches of paper.   If the user has questions, call program maintenance.')
  hpi = (hmax - hmin) / 12.0d0
1227 if (iout .eq. 1) go to 1229
  if (d1 .ge. 3.0d0) go to 1229
  write (unit = lunit(6), fmt = 1224) hpi, hmin, hmax, d1
  hmax = 3.0d0 * hpi + hmin
  write (unit = lunit(6), fmt = 1228) hmax
1228 format ('+', 106x, ', which is considered', /, 5x, 'too short for the requested lineprinter plot.  The requested maximum will be reset by the t. p. to ', e13.6, /, 5x, 'to give a plot length of 3 inches.')
1229 countp = countp + 1.0d0
  kikoy = 0
  if ((iout .eq. 2) .and. (vmin .ne. 0.0d0 .or. vmax .ne. 0.0d0)) kikoy = 99
  if (vmin .ne. 0.0d0 .or. vmax .ne. 0.0d0) go to 1220
  vmin = -c1e12
  vmax = c1e12
1220 vmaxr = vmin + (vmax - vmin) * (vh - vs) / vhs
  vminr = vmin - (vmax - vmin) * vs / vhs
21220 if (ihs .ge. 1 .and. ihs .le. 7) go to 1230
  write (unit = lunit(6), fmt = 1221) ihs
1221 format (5x, 'The number specifying the units on the horizontal scale, punched in column 4 of the plot request card, must be', /, 5x, 'between 1 and 7 (inclusive).  The number read from the last such card was', i2, '.', /, 5x, 'this plot request is cancelled.')
  go to 1000
!1230 go to (1240, 1260, 1280, 1300, 1320, 1280, 1280) , ihs
1230 select case (ihs)
  case (1)
     go to 1240

  case (2)
     go to 1260

  case (3, 6, 7)
     go to 1280

  case (4)
     go to 1300

  case (5)
     go to 1320

  end select
1240 tmult = 360.0d0 * statfr
  go to 1340
1260 tmult = statfr
  go to 1340
1280 tmult = 1.0d0
  go to 1340
1300 tmult = 1000.0d0
  go to 1340
1320 tmult = 1000000.0d0
1340 hmin = hmin / tmult
  hmax = hmax / tmult
  mhoriz = -22
  if (xyplot(1) .gt. 0.0d0) ihs = 8
  j = 4 * (ihs - 1) + 1
  !  call packch (textax(j), horzl(1), mm6, mm1, mm4)
  call pack (textax(j), horzl(1), mm6, mm1, mm4)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1341) horzl
1341 format (' At 1341 packed time axis label.', 3a10)
  if (vmin .ge. 0.0d0) go to 1460
  if (vmax .le. 0.0d0) go to 1480
  ha = -vmin * vhs / (vmax - vmin)
  go to 1500
1460 ha = 0.0d0
  go to 1500
1480 ha = vhs
1500 read (unit = lunit(4)) date1, tclock, (intd8(j), j = 1, 4)
  numnam = intd8(1)
  numnvo = intd8(2)
  numbco = intd8(3)
  nc = intd8(4)
  jcnt = numnam + numnvo + 2 * nc
  jcnt = jcnt * nbyte(1) / nbyte(4)
  if (maxev .ge. jcnt)  go to 1520
  lstat(14) = maxev
  lstat(15) = jcnt
  lstat(19) = 1500
  kill = 96
  go to 9200
1520 if (mulplt(1) .gt. 0) go to 1523
  mulplt(2) = 0
  mulplt(3) = 0
  !     segmented, 1, vax e/t can skip translation of rewind:
1523 rewind lunit(4)
  read (unit = lunit(4)) date1, tclock, (intd8(j), j = 1, 4), (buslst(j), j = 1, numnam)
  if (numnvo .gt. limbin) go to 34231
  if (nc .le. limbin) go to 44231
34231 write (unit = lunit(6), fmt = 24231) limbin, numnvo, nc
24231 format (/, ' Overflow error stop in  ', "'", 'subr31', "'", '.  limbin  numnvo      nc', /,  35x, 3i8)
  call stoptp
44231 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4231) date1, tclock, numnam, numbco, nc, (buslst(j), j = 1, numnam)
4231 format (/, ' First third of plot-file header info read from logical 4 in subr31.', 7x, 'date1(2)', 6x, 'tclock(2)', '  numnam  numbco      nc', /, 68x, 7x, 2a4, 7x, 2a4, 3i8, /, ( ' (buslst(j), j=1, numnam)',  1x, 10a7))
  iofibr = numnvo
  iofjbr = numnvo + nc
  iofbus = numnvo + 2 * nc
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1525) iofibr, iofjbr, iofbus
1525 format (/, ' offsets.  iofibr  iofjbr  iofbus', /, 9x, 3i8)
  j = numnam
4224 n4 = j + iofbus
  buslst(n4) = buslst(j)
  j = j - 1
  if (j .gt. 0) go to 4224
  if (numnvo .gt. 0) read (unit = lunit(4)) (intd8(j), j = 1, numnvo)
  do j = 1, numnvo
     ibsout(j) = intd8(j)
  end do
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 64225) numnvo, (ibsout(j), j = 1, numnvo)
64225 format (/, ' numnvo=', i4, 10x, '(ibsout(j), j=1, numnvo)  follow ....', /, (1x, 20i6))
  n1 = numnvo + 1
  n2 = numnvo + nc
  n5 = n1 + nc
  n6 = n2 + nc
  if (nc .eq. 0) go to 54224
  read (unit = lunit(4)) (intd8(j), j = 1, nc), (intd9(j), j = 1, nc)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4219) (intd8(j), j = 1, nc), (intd9(j), j = 1, nc)
4219 format (/, ' (intd8(j), j = 1, nc), (intd9(j), j = 1, nc)', /, (1x, 20i6))
  n11 = n1
  n12 = n5
  do j = 1, nc
     ibrnch(n11) = intd8(j)
     jbrnch(n12) = intd9(j)
     n11 = n11 + 1
     n12 = n12 + 1
  end do
  if (iprsup .lt. 1) go to 54224
  write (unit = lunit(6), fmt = 4222) n1, n2
4222 format (' n1=', i10, 'n2=', i10, /, ' ibrnch(j) and jbrnch(j+nc), j=n1, n2 at 4222....')
  do j = n1, n2
     n6 = j + nc
     write (unit = lunit(6), fmt = 64223) ibrnch(j), jbrnch(n6)
  end do
64223 format (1x, 2i10)
  !54224 call packch (date1(1), daytim(1), mm4, mm1, mm2)
54224 call pack (date1(1), daytim(1), mm4, mm1, mm2)
  !  call packch (blanka(1), daytim(1), mm4, mm9, mm1)
  call pack (blanka(1), daytim(1), mm4, mm9, mm1)
  !  call packch (tclock(1), daytim(1), mm4, mm11, mm2)
  call pack (tclock(1), daytim(1), mm4, mm11, mm2)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4223) daytim
4223 format (' At 4223 packed daytim.', 8a10)
  numbvo = nc - numbco
  i = 0
  jplt = 0
  if (icp .gt. 7) go to 1660
1560 i = i + 1
  if (i .ne. 5) go to 1580
  if (jplt .eq. 0) go to 1900
  go to 1880
1580 if (slot(i) .eq. blank) go to 1560
  k = 0
1600 k = k + 1
  if (k .le. numnvo) go to 1640
  write (unit = lunit(6), fmt = 1620) slot(i)
1620 format (5x, 'bus name =', a6, '= of the user=s last-read plot card is not the name of a bus having node voltage', /, 5x, 'output.   Hence this field will be ignored by the EMTP (treated as if it had been blank).')
  write(unit = lunit(6), fmt = 1621)
1621 format (5x, 'The user is reminded that correct spelling and the consistent positioning of all blanks within the data', /, 5x, 'fields of width 6 for all bus names is required.')
1627 slot(i) = blank
  go to 1560
1640 l = ibsout(k) + iofbus
  if (slot(i) .ne. buslst(l)) go to 1600
  jplt = jplt + 1
  mplot(jplt) = k
  slot(jplt) = buslst(l)
  if (jplt .lt. i) go to 1627
  go to 1560
1660 do j = 1, jslot, 2
     if (slot(j) .eq. blank .and. slot(j + 1) .eq. blank) exit
     slot(jplt + 1) = slot(j)
     slot(jplt + 2) = slot(j + 1)
     jplt = jplt + 2
  end do
  if (jplt .eq. 0) go to 1900
  lplt = 0
  jbegbc = numbvo + 1
  do i = 1, jplt, 2
     if (icp .eq. 9) go to 1720
     ib = 1
     il = jbegbc
     n1 = numnvo + 1
     go to 1740
1720 ib = jbegbc
     il = nc + 1
     n1 = numnvo + jbegbc
1740 do
        if (ib .lt. il) go to 1780
        write (unit = lunit(6), fmt = 1760) trim (slot(i)), trim (slot(i + 1))
1760    format (5x, 'The user=s last-read plot card requests a plot for a branch-variable which is identified by terminal', /, 5x, 'names =', a, '= and =', a, '=.   But the EMTP cannot find this requested variable in the list of output', /, 5x, 'variables, so this particular plot request must be ignored.')
        write (unit = lunit(6), fmt = 1621)
        write (unit = lunit(6), fmt = 1763)
1763    format (5x, 'Also, the user should be reminded that branch-output requests are made using column-80 punches on the', /, 5x, 'branch cards in question.   The user should double-check that he really has requested the output variable which', /, 5x, 'he is trying to plot (and which got him in trouble).   One common error is to request only branch-current output', /, 5x, '(a 1-punch in column 80) and then try to plot branch voltage ---- or vice versa.   Finally, the user should check', /, 5x, 'that branch output is even possible for the component in question, since column-80 punches may be ignored if the')
        write (unit = lunit(6), fmt = 1764)
1764    format (5x, 'component in question does not provide for such output.   Any branch-output request for a multi-phase', /, 5x,  'distributed line falls into this class, it will be noted.')
        slot(i) = blank
        slot(i + 1) = blank
        goto 1860
1780    n7 = ib + iofibr
        n8 = ib + iofjbr
        n2 = ibrnch(n7) + iofbus
        n3 = jbrnch(n8) + iofbus
        if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1781) n2, n3, buslst(n2), buslst(n3)
1781    format (' At 1780', 2i10, 5x, 2a8)
        if (slot(i) .ne. buslst(n2)) go to 1800
        if (slot(i + 1) .ne. buslst(n3)) go to 1820
        !               node pair found - sign correct
        mplot(lplt + 1) = n1
        go to 1840
1800    if (slot(i + 1) .ne. buslst(n2)) go to 1820
        if (slot(i) .ne. buslst(n3)) go to 1820
        !               node pair found - sign negative
        mplot(lplt + 1) = -n1
        go to 1840
1820    ib = ib + 1
        n1 = n1 + 1
     end do
1840 lplt = lplt + 1
  end do
1860 continue
  jplt = lplt
  if (jplt .eq. 0) go to 1900
  jslot = jslot + 1
1880 if (iprsup .eq. 0) go to 1929
  write (unit = lunit(6), fmt = 1883) jplt, (slot(i), i = 1, jslot)
1883 format (/, ' At 1883 of ov31. ', i10, /, (1x, a6))
  write (unit = lunit(6), fmt = 1884) (mplot(i), i = 1, 4)
1884 format (4i10)
  go to 1929
1900 write (unit = lunit(6), fmt = 1920)
1920 format (5x, 'No valid plot-variable name or names was punched on the user=s last-read plot card.   Hence the EMTP will', /, 5x, 'ignore this plot card completely, and go on to read the next one (fun and games continue).')
  go to 1000
1929 k = numnvo + nc
  jplt2 = jplt * 2
  maxevk = (maxev / jplt2 ) * jplt2 - (4 * jplt)
  maxevk = maxevk * nbyte(4) / nbyte(5)
  j = jplt2 + 4 * jplt
  if (maxevk .ge. j) go to 1935
  write (unit = lunit(6), fmt = 1930) maxevk, j
1930 format (5x, ' *** The plotting array ev has been dimensioned such that the working size is ', i10, '.  This is less than', i10, /, 5x, 'which is the size required for this plot.  This request is cancelled.')
  go to 1000
1935 kpl = 0
  kplt = 0
  iovfl = 0
  iend = 0
  evmx = 0.0d0
  vmaxl = 0.0d0
  vminl = 0.0d0
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunit(13)
  do j = 1, jplt
     iswx(j) = 0
     evh(j) = 0.0d0
     evdh(j) = 0.0d0
  end do
  n9 = 0
  if (xyplot(1) .ne. 0 .or. nfour .ne. 0) n9 = 1
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 2117) k, jplt, n9, maxevk, mplot
2117 format (/, ' Before read of plot points.       k    jplt      n9  maxevk', /, 28x, 4i8, /, "'", 'mplot', "'",  'vector follows.', 16i5)
  ndx1 = 2 * linlim
  !  call mover0 ( ev(1), ndx1 )
  call move0 (ev(1 :), ndx1)
2115 if (m4plot .eq. 0) go to 2102
  read (unit = lunit(4)) tsing, (bxsing(j), j = 1, k)
  tstep = tsing
  do j = 1, k
     bx(j) = bxsing(j)
  end do
  go to 2105
2102 read (unit = lunit(4)) tstep, (bx(j), j = 1, k)
2105 if (tstep .eq. -9999. .or. tstep .gt. hmax) go to 2200
  if (tstep .lt. hmin) go to 2115
  do j = 1, jplt
     j1 = mplot(j)
     if (j1 .ge. 0) go to 2120
     j1 = -j1
     bx(j1) = -bx(j1)
2120 if (n9 .eq. 0) go to 2127
     kplt = kplt + 1
     kpl = kpl + 1
     if (nfour .ne. 0) go to 2123
     ev(kpl) = bx(j1)
     go to 2140
2123 evdoub(kpl) = bx(j1)
     go to 2140
2127 kpl = kpl + 2
     ev(kpl - 1) = tstep
     ev(kpl) = bx(j1)
     kplt = kplt + 2
2140 end do
  if (kpl .ge. maxevk) go to 2150
  go to 2115
2150 iovfl = 1
  go to 2210
2200 iend = 1
  if (lastov .eq. 1) tmax = ev(kpl - 1)
  if (kplt .ne. 0) go to 2210
  write (unit = lunit(6), fmt = 2205) hmin, hmax
2205 format (' No plot points found between the requested time limits ', e12.3, ' to ', e12.3,    ' .   This plot request cancelled.')
  go to 1000
2210 if (iprsup .eq. 0) go to 2215
  jk = kplt / jplt2
  write (unit = lunit(6), fmt = 2211) jk
2211 format (/, ' at 2211, after reading from 4, the number of points per curve are', i10)
  if (iprsup .lt. 3) go to 2215
  j1 = 8
  if (iprsup .gt. 39) j1 = kpl
  write (unit = lunit(6), fmt = 2213) (ev(j), j = 1, j1)
2213 format (/, ' ev(j) plot points.', /, (1x, 10e13.4))
2215 if (xyplot(1) .gt. 0.0d0) go to 2540
  if (nfour .eq. 0) go to 7682
  !     calculate fourier series in module  "series" .
  call series (nfour, kpl, jplt, maxevk)
  go to 1000
7682 do j=1, jplt
     j1 = j * 2
     do i = j1, kpl, jplt2
        evp = ev(i)
        if (iswx(j) .eq. 1) go to 2240
        evdp = evp - evh(j)
        if (evdp * evdh(j) .ge. 0.0d0) go to 2250
        if (isww(j) .eq. 1) go to 2255
        itimes(j) = itimes(j) + 1
        if (itimes(j) .le. nsmth) go to 2260
        iswx(j) = 1
        write (unit = lunit(6), fmt = 2220) j, ev(i - 1), nsmth
2220    format (5x, 'While scanning the data points for curves of the last-read plot card, a need for smoothing of curve number', /, 5x, i2, ' beyond time ', e12.4, ' seconds has been determined.   At this point,', i3, ' successive, uninterrupted,', /, 5x, 'alternating relative maxima and relative minima have been observed.   This is taken as a sign of a spurious', /, 5x, 'mathematical oscillation, something which should not exist physically (at least not for an intelligent user', /, 5x, 'who has picked the time-step size =deltat= and the output frequency "iplot" properly).   At this point, the')
        write (unit = lunit(6), fmt = 2221)
2221    format (5x, 'omniscient and omnipotent EMTP (otherwise affectionately referred to as =big brother= by those users', /, 5x, 'who are accustomed to his modus operandi) has decided to smooth this curve for all later time.   This smoothing', /, 5x, 'involves simply the averaging of successive ordinates in the output file of plot-variable points for this curve,', /, 5x, 'before plotting beyond this point in time.')
2240    ip1 = i + jplt2
        if (ip1 .le. kpl) go to 2241
        ip1 = i - jplt2
        ev(i) = ev(ip1)
        go to 2242
2241    ev(i) = (ev(ip1) + evp) / 2.0
2242    evp = ev(i)
        go to 2260
2250    isww(j) = 1
        go to 2260
2255    itimes(j) = 1
        isww(j) = 0
2260    evdh(j) = evdp
        evh(j) = evp
        d1 = abs(evp)
        if (d1 .gt. evmx) evmx = d1
        if (evp .lt. vminr) go to 2280
        if (evp .gt. vmaxr) go to 2300
        go to 2310
2280    ev(i) = vminr
        go to 2310
2300    ev(i) = vmaxr
2310    if (iout .eq. 1) go to 2320
        if (ev(i) .gt. vmaxl) vmaxl = ev(i)
        if (ev(i) .lt. vminl) vminl = ev(i)
     end do
2320 continue
  end do
  if (iovfl .eq. 0) go to 2350
  write (unit = lunit(13)) (ev(j), j = 1, kpl)
  if (iend .eq. 1) go to 2350
  kpl = 0
  go to 2115
2350 stp = 1.0d0 + (hpi / tmult) / (ev(jplt2 + 1) - ev(1))
  if (iout .eq. 1) go to 2346
  if (vmaxl .ne. 0.0d0) go to 2346
  if (vminl .ne. 0.0d0) go to 2346
  write (unit = lunit(6), fmt = 2345)
2345 format (/, ' Abort this plot request, because all variables are identically zero over the requested time span.')
  go to 1000
2346 jstp = int (stp, kind (jstp))
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2342) jplt, kplt, vmax, vmin, evmx, vminr, vmaxr
2342 format (/, ' Plot parameters at 2342. ', 2i10, 5e15.5)
  if (vmax .ne. c1e12 .or. vmin .ne. (-c1e12)) go to 2540
  if (kikoy .eq. 99) go to 2540
  if (evmx .eq. 0.0) evmx = 1.0
  a = 8.0
  evmxf = evmx
  if (kbound .eq. 1) go to 2360
  evmxf = evmx * 4.0
  evmxf = evmxf / 5.0
2360 d1 = evmxf - a
  if (d1 .eq. 0.0d0) go to 2400
  if (d1 .gt. 0.0d0) go to 2380
  a = a / 10.
  go to 2360
2380 a = a * 10.
  d1 = evmxf - a
  if (d1 .eq. 0.0d0) go to 2400
  if (d1 .gt. 0.0d0) go to 2380
  d1 = evmxf - a / 2.0d0
  if (d1 .eq. 0.0d0) go to 2440
  if (d1 .lt. 0.0d0) go to 2420
2400 vmax = a
  go to 2520
2420 d1 = evmxf - a / 4.0d0
  if (d1 .lt. 0.0d0) go to 2460
  if (d1 .eq. 0.0d0) go to 2480
2440 vmax = a / 2.0d0
  go to 2520
2460 if (evmxf - a / 8.0d0 .le. 0.0d0) go to 2500
2480 vmax = a / 4.0d0
  go to 2520
2500 vmax = a / 8.0d0
2520 vmin = -vmax
2540 continue
  do j = 1, 13
     aupper(j) = blank
  end do
  long1 = 1
  jsl = jplt
  if (icp .ge. 8) jsl = jplt2
  do i = 1, jsl
     if (slot(i) .eq. blank) slot(i) = terra
     !     call packch (slot(i), aupper(1), mm6, long1, mm1)
     call pack (slot(i), aupper(1), mm6, long1, mm1)
     long1 = long1 + 6
     !     call packch (blanka(1), aupper(1), mm4, long1, mm1)
     call pack (blanka(1), aupper(1), mm4, long1, mm1)
     long1 = long1 + 2
  end do
  jchar = long1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2569) aupper
2569 format (' At 2569 packed node names.', 8a10)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4100) tolrce
4100 format (' Tolerance=', e12.6)
  if (xyplot(1) .eq. 0.0d0) go to 2570
  !     if ( iout  .eq.  1 )   go to 12581  vb removal (june 82)
  vminl = xyplot(5)
  vmaxl = xyplot(6)
  jplt2 = 1
2570 dx = hpi / tmult
  dy = (vmax - vmin) / vhs
  d1 = lnpin
  dxl = (hpil / tmult) / d1
  dxl2 = dxl / 2.0
  dyl = (vmaxl - vminl) / 130.
  if (kikoy .eq. 99) dyl = (vmax - vmin) / 130.0d0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2571) dxl, vmaxl, vminl, dyl
2571 format (' At 2580', 4e15.6)
  jpts = kplt / jplt2
  if (jpts .lt. linlim) jpts = linlim
  lnflg = 0
  ipl1 = 1
2580 ipl2 = ipl1 + ncrv - 1
  if (ipl2 .gt. jplt) ipl2 = jplt
  kpl = kplt
  jovfl = iovfl
  lnck = 0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2583) ipl1, ipl2
2583 format (' At 2580, ipl1,2', 2i10)
  if (iout .eq. 1) go to 12581
  if (lnflg .eq. 1) go to 3260
  if (mpage .eq. 1) write (unit = lunit(6), fmt = 3179)
3179 format ('1')
  if (mpage .eq. 0) write (unit = lunit(6), fmt = 3180)
3180 format (////, 1x)
  if (bus1 .ne. blank) go to 3190
  if (bus2 .ne. blank) go to 3190
  if (bus3 .ne. blank) go to 3190
  go to 3205
3190 write (unit = lunit(6), fmt = 3200) bus1, bus2, bus3
3200 format (58x, 2a6, a4, //, 1x)
3205 do j = 1, 13
     if (cstxt(j) .ne. blank) go to 3207
  end do
  go to 3209
3207 write (unit = lunit(6), fmt = 3208) cstxt
3208 format (27x, 13a6, /, 1x)
3209 if (ialf .eq. 0) go to 3212
  do j = 1, ialf, 13
     j1 = j + 12
     write (unit = lunit(6), fmt = 3211) (pltle(k), k = j, j1)
3211 format (27x, 13a6)
     do k = j, j1
        pltle(k) = blank
     end do
  end do
3212 j = int (countp, kind (j))
  write (unit = lunit(6), fmt = 3220) date1, tclock, j, icp, slot
3220 format (/, 1x, 2a4, 2x, 2a4, 6x, i2, /, ' plot type', 5x, i1, /,  ' node names', 5x, 8(a6, 3x), /, 1x)
  k = 4 * ( ihs - 1 )  +  1
  j1 = k + 3
  write (unit = lunit(6), fmt = 3230) (textax(j), j = k, j1)
3230 format (1x, 4a6, /, 1x)
  d1 = abs (vmaxl)
  d2 = abs (vminl)
  if (kikoy .eq. 99) d1 = abs(vmax)
  if (kikoy .eq. 99) d2 = abs(vmin)
  if (d1 .lt. d2) d1 = d2
  k = 0
  if (d1 .lt. 1.0d0) go to 3232
3231 if (d1 .lt. 10.0d0) go to 3235
  k = k + 1
  d1 = d1 / 10.0d0
  go to 3231
3232 if (d1 * fltinf .lt. 1.0d0) go to 3235
43232 if (d1 .ge. 1.0d0) go to 3235
  k = k - 1
  d1 = d1 * 10.0d0
  go to 43232
3235 if (k .ne. 0) go to 3237
  write (unit = lunit(6), fmt = 3236) bus4, bus5, bus6
3236 format (51x, 2a6, a4)
  expnt = 1.0
  go to 3240
3237 write (unit = lunit(6), fmt = 3238) bus4, bus5, bus6, k
3238 format (51x, 2a6, a4, '  (  x 10**(', i3, ')  )')
  k = - k
  expnt = 10.0d0 ** k
3240 d3 = vminl
  if (kikoy .eq. 99) d3 = vmin
  do j = 1, 14
     vrtnum(j) = d3 * expnt
     d3 = d3 + (dyl * 10.0d0)
  end do
  write (unit = lunit(6), fmt = 3255) vrtnum
3255 format (f7.3, f8.3, f9.3, 10f10.3, f8.3)
  long1 = jdumy
  call linplt (mm0, long1)
  if (xyplot(1) .ne. 0.0) go to 2581
  lcnt = -1
  hpt = hmin
  jcol = 0
  mdpt = 0
  if ((vminl .gt. 0.0d0 .and. kikoy .ne. 99) .or. (vmin .gt. 0.0d0 .and. kikoy .eq. 99)) go to 3260
  if ((vmaxl .lt. 0.0d0 .and. kikoy .ne. 99) .or. (vmax .lt. 0.0d0 .and. kikoy .eq. 99)) go to 3260
  d3 = -vminl / dyl + 1.5d0
  if (kikoy .eq. 99) d3 = -vmin / dyl + 1.5d0
  mdpt = int (d3, kind (mdpt))
3260 if (iout .eq. 2) go to 5975
  go to 12581
  !     begin x-y printer plot code  &&&&&&&&&&&&&&&&&&&&&&&&&&&
2581 n1 = int (xyplot(1) * lnpin, kind (n1))
  n2 = int (xyplot(4) * xyplot(9), kind (n2))
  n3 = n1 * n2
  n4 = maxevk - n3
  n11 = kpl * nbyte(5) / nbyte(4)
  if (n4 .gt. n11) go to 2739
  write (unit = lunit(6), fmt = 2734) n11, maxevk, n3, kpl
2734 format (5x, ' x-y printer plot cancelled due to insufficient working space.', 4i8)
  go to 2819
2739 n5 = n11 + 1
  n6 = n5 + n3
  do j = n5, n6
     karray(j) = 0
  end do
  d1 = n1 / (xyplot(3) - xyplot(2))
  d2 = n2 / (xyplot(6) - xyplot(5))
  n10 = int (-xyplot(5) * d2 + 0.5d0, kind (n10))
  if (n10 .gt. 131) n10 = 0
  if (n10 .lt. 0) n10 = 0
  n5 = 1
  n9 = jplt / 2
  do i=1, maxevk
     do j=1, n9
        k = int ((ev(n5) - xyplot(2)) * d1 + 0.5d0, kind (k))
        if (k .gt. n1) k = n1
        if (k .le. 0) k = 1
        l = int ((ev(n5 + 1) - xyplot(5)) * d2 + 0.5d0, kind (l))
        if (l .gt. n2) l = n2
        if (l .le. 0) l = 1
        n8 = n11 + (k - 1) * n2 + l
        if (karray(n8) .gt. 0) go to 2756
        karray(n8) = j
        go to 2763
2756    if (karray(n8) .ne. j) karray(n8) = 99
2763    n5 = n5 + 2
        if (n5 .gt. kpl) go to 2781
     end do
  end do
  call stoptp
2781 n8 = n11
  n7 = int (lnpin, kind (n7))
  n9 = 77
  d3 = xyplot(2)
  d4 = (xyplot(3) - xyplot(2)) / xyplot(1)
  call linplt (ll0, ll1)
  do 2816  k = 1, n1
     if (n10 .gt. 0) call linplt (llmin1, n10)
     n7 = n7 - 1
     do j = 1, n2
        n8 = n8 + 1
        m = karray(n8)
        if (m .eq. 0) go to 2807
        call linplt (m, j)
2807 end do
     call linplt (n9, llmin1)
     if (n7 .gt. 0) go to 2816
     d3 = d3 + d4
     write (unit = kunit6, fmt = 2811) d3
2811 format ('+', e12.3)
     n7 = int (lnpin, kind (n7))
2816 end do
2819 d1 = 1.0d0 / d1
  d2 = 1.0d0 / d2
  write (unit = lunit(6), fmt = 2822) d1, d2
2822 format (' Just-completed x-y plot: dx/line =', e14.6, 5x, 'dy/column =', e14.6)
  !     x-y printer plot ends here.   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  if (iout .eq. 2) go to 1000
12581 if (kalcom .eq. 1) go to 2582
  kalcom = 1
  call begplt
  if (kscale .eq. 1) call factor (d4fact)
  call plot (one, vs, llmin3)
2582 iprsov(38) = iprsov(38) + 1
  if (mulplt(2) .gt. 0) go to 5975
  jalf = 1
  call newpen (ll1)
  call prnthd (kprhd)
  d7 = vhs
  if (kpgrid .gt. 0) d7 = d7 + 0.1d0
  call symbol (half, d7, hgt1, sext(1), zero, ll78)
  call symbol (fl2p5, vhs+half, hgt2, headl(1), zero, ll16)
  if (mulplt(1) .eq. 0) go to 2593
  call symbol (half, fourth, hgt1, daytim(1), zero, ll18)
  call symbol (half, fl3p5, hgt1, userid, zero, ll6)
  if (mulplt(4) .eq. 0) go to 5962
  call plot (zero, vhs, ll3)
  call plot (zero, zero, ll2)
  d4 = -.08d0
  d6 = .08d0
  d5 = -.0001d0
2589 call plot (d4, d5, ll3)
  call plot (d6, d5, ll2)
  d5 = d5 + 1.0d0
  if (d5 .le. vhs) go to 2589
  go to 5963
2593 call symbol (half, half, hgt1, daytim(1), zero, ll18)
  call number (fl3, half, hgt1, countp, zero, llmin1)
  !  call packch (text14(1), arch10(1), mm6, mm1, mm2)
  call pack (text14(1), arch10(1), mm6, mm1, mm2)
  call symbol (half, fourth, hgt1, arch10(1) , zero, ll10)
  ricp = icp
  call number (fltwo, fourth, hgt1, ricp, zero, llmin1)
  !  call packch (text16(1), arch10(1), mm6, mm1, mm2)
  call pack (text16(1), arch10(1), mm6, mm1, mm2)
  call symbol (half, zero, hgt1, arch10(1), zero, ll10)
  call symbol (fltwo, zero, hgt1, aupper(1), zero, jchar)
5962 vhs1 = d7 - half
2600 if (jalf.gt.ialf) go to 2620
  call symbol (fl1p5, vhs1, hgt1, alpha(jalf), zero, ll78)
  vhs1 = vhs1 - half
  jalf = jalf + 13
  go to 2600
2620 if (xyplot(1) .eq. 0.0d0) go to 3496
  n7 = kpl / 2
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3413) kpl, jplt, ev(1), ev(2)
3413 format (/, " Before call to  'scale'  for  x-y  plot.     kpl    jplt",  11x, 'x(1)', 11x,  'y(1)', /, 41x,  2i8,  2e15.6)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3416) xyplot
3416 format (/, " Vector  'xyplot'  follows ...", /, 1x, 8e15.5)
  !     now transfer to single-precision vector for calcomp
  !     calls.   xyplot  interfaced with  "frefld", recall.
  do i = 1, 8
     xyshor(i) = xyplot(i)
  end do
  if (xyplot(2) .eq. xyplot(3)) go to 3419
  ev(kpl + 1) = xyplot(2)
  ev(kpl + 3) = (xyplot(3) - xyplot(2)) / xyplot(1)
  go to 3422
3419 call scale (ev(1), xyshor(1), n7, ll2, xyshor(7))
3422 if (xyplot(5) .eq. xyplot(6)) go to 3426
  ev(kpl + 2) = xyplot(5)
  ev(kpl + 4) = (xyplot(6) - xyplot(5)) / xyplot(4)
  go to 3429
3426 call scale (ev(2), xyshor(4), n7, ll2, xyshor(7))
3429 d4 = ev(kpl + 3)
  d5 = ev(kpl + 4)
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 3432) ev(kpl + 1), ev(kpl + 2), d4, d5, ev(1), ev(2)
3432 format (/, ' Scaling parameters.', 9x, 'x-bias', 9x, 'y-bias', 10x, 'dx/in', 10x, 'dy/in',  11x, 'x(1)', 11x, 'y(1)', /, 20x, 6e15.6)
  do i = 1, jplt, 2
     n5 = kpl + i
     ev(n5) = ev(kpl + 1)
     ev(n5 + 1) = ev(kpl + 2)
     n5 = n5 + jplt
     ev(n5) = d4
     ev(n5 + 1) = d5
  end do
  n5 = n5 + 1
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 3440)  (ev(i), i = kpl, n5)
3440 format (/, ' (ev(i), i=kpl, n5)  ....', /, (1x, 10e13.4))
  call axis (zero, zero, horzl(1), ll24, xyshor(1), zero, ev(kpl + 1), d4)
  call axis (zero, zero, vertl(1), ll16, xyshor(4), fl90, ev(kpl + 2), d5)
  if (kpgrid .eq. 0) go to 3442
  j1 = int (xyplot(1), kind (j1))
  j2 = int (xyplot(4), kind (j2))
  call newpen (kpgrid)
  call grid (zero, zero, one, one, j1, j2)
3442 j = 0
  if (xyplot(8) .gt. 0.0) j = int (n7 / xyplot(8), kind (j))
  n5 = kpl / jplt
  n12 = 0
  do  i=1, jplt, 2
     n12 = n12 + 1
     call newpen (kpen(n12))
     if (iprsup .ge. 2) write (unit = lunit(6), fmt = 3444) i, j, jplt, n5
3444 format (/, " Ready to call  'line'  again.       i       j    jplt      n5", /, 30x, 4i8)
     call line (ev(i), ev(i + 1), n5, jplt, j, i)
  end do
  taxmax = xyplot(1) + 1.0
  xyplot(1) = 0.0d0
  ipl2 = jplt
  go to 2656
3496 if (kprhd .eq. 0) go to 2624
  call prnthd (ll2)
2624 call axis (zero, zero, vertl(1), ll16, vhs, fl90, vmin, dy)
5963 if (kprhd .gt. 0) call prnthd (ll1)
  if (ipl1 .ne. 1) go to 5970
  dlen = (hmax - hmin) * tmult / hpi
  if (dlen .lt. 9.0d0) dlen = 9.0d0
  dstrt = hmin * tmult
5970 if (mulplt(1) .eq. 0) go to 7435
  if (mulplt(5) .eq. 0) go to 7435
  call plot (zero, ha, ll3)
  call plot (dlen, ha, ll2)
  d4 = ha - .08d0
  d6 = ha + .08d0
  d5 = .9999
7433 call plot (d5, d4, ll3)
  call plot (d5, d6, ll2)
  d5 = d5 + 1.0d0
  if (d5 .le. dlen) go to 7433
  go to 7438
7435 call axis (zero, ha, horzl(1), mhoriz, dlen, zero, dstrt, hpi)
7438 if (dlen .gt. taxmax) taxmax = dlen
  if (kpgrid .eq. 0) go to 5975
  j1 = int (dlen + flzero, kind (j1))
  j2 = int (vhs  + flzero, kind (j2))
  call newpen (kpgrid)
  call grid (zero, zero, one, one, j1, j2)
5975 iend = kplt
  !     &&&&&&&&&&&&&&&&&   remove  call mover0  &&&&&&&&&&&&&
  if (iovfl .eq. 0) go to 5980
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunit(13)
  iend = maxevk
  read (unit = lunit(13)) (ev(j), j = 1, iend)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 5979) iend
5979 format (' ev read from logical 9', i10)
5980 if (iout .eq. 1) go to 5981
  if (lnflg .eq. 1) go to 3115
  jend = 0
  kend = iend - jplt2
  do i = ipl1, ipl2
     jpntr(i) = 2 * i - 1
  end do
  hlo = hpt - dxl2
  hhi = hpt + dxl2
3010 if (iprsup .ge. 5) write (unit = lunit(6), fmt = 3015)  hlo, hpt, hhi, tmax
3015 format (13x, 'hlo', 12x, 'hpt', 12x, 'hhi', 11x, 'tmax', /, 1x, 4e15.6)
  do i = ipl1, ipl2
     kold = 0
3020 j = jpntr(i)
     evp = ev(j)
     if (evp .lt. hlo) go to 3100
     if (evp .ge. hhi) go to 3100
     klm = int ((ev(j + 1) - vminl) / dyl + 1.5d0, kind (klm))
     if (kikoy .eq. 99) klm = int ((ev(j + 1) - vmin) / dyl + 1.5d0, kind (klm))
     if (1 .gt. klm) klm = 1
     if (131 .lt. klm) klm = 131
     if (klm .eq. kold) go to 3040
     if (klm .le. 9) jcol = 1
     long1 = i
     long2 = klm
     call linplt (long1, long2)
     kold = klm
     if (iprsup .ge. 5) write (unit = lunit(6), fmt = 3035) j, ev(j), ev(j + 1), i,klm
3035 format (' At 3040 ', i10, 2e15.6, 2i10)
3040 jpntr(i) = jpntr(i) + jplt2
     d4 = tmax - evp
     if (d4 .gt. 0.0d0) go to 3020
     jend = 1
3100 end do
  if (iprsup .ge. 6) write (unit = lunit(6), fmt = 13100) lnck, jpts, lcnt, lnpin, jcol, jovfl, jend, mdpt, ipl1, ipl2, llmin1
13100 format (1x, '    lnck    jpts    lcnt   lnpin    jcol   jovfl    jend    mdpt    ipl1    ipl2  llmin1', /, 1x, 11i8)
  if (jend .eq. 1) go to 3103
  do j = ipl1, ipl2
     ktrm = (j * 2 - 1) + kend
     if (jpntr(j) .gt. ktrm) go to 3101
     go to 3102
3101 end do
  jend = 1
3103 if (jovfl .ne. 0) go to 3115
3102 lcnt = lcnt + 1
  long1 = jdumy
  call linplt (long1, mmmin1)
  lnck = lnck + 1
  if (lnck .gt. jpts) go to 3111
  if (lcnt .ne. lnpin) go to 3110
  if (jcol .ne. 0) go to 3109
  hhpt = hpt * tmult
  if (hhpt .ge. 0.9999d0) go to 3106
  write (unit = kunit6, fmt = 3105) hhpt
3105 format ('+', 1x, f8.6)
  go to 3109
3106 d4 = hhpt * .001d0
  if (d4 .ge. 0.9999d0) go to 3108
  write (unit = kunit6, fmt = 3107) hhpt
3107 format ('+', 2x, f7.3)
  go to 3109
3108 d4 = hhpt * 1.e-6
  if (d4 .ge. 1.0d0) go to 3119
  write (unit = kunit6, fmt = 3104) hhpt
3104 format ('+', 2x, f7.0)
  go to 3109
3119 write (unit = kunit6, fmt = 3121) hhpt
3121 format ('+', 2x, e9.3)
3109 lcnt = 0
3110 if (jend .eq. 1) go to 3115
  if (mdpt .eq. 0) go to 31
  if (mdpt .gt. 9) go to 30
  j1 = lcnt + 1
  if (j1 .eq. lnpin) go to 31
30 long1 = mdpt
  call linplt (mmmin1, long1)
31 jcol = 0
  hlo = hhi
  hpt = hlo + dxl2
  hhi = hlo + dxl
  go to 3010
3111 if (iout .eq. 2) go to 3112
  lnflg = 1
  write (unit = lunit(6), fmt = 310)
310 format (' ****The number of lines printed for this plot request now exceeds the total number of data points.')
  write (unit = lunit(6), fmt = 311)
311 format (' ****Since calcomp plot was also specified, only printer plot is cancelled for this request.')
  go to 3115
3112 write (unit = lunit(6), fmt = 310)
  write (unit = lunit(6), fmt = 312)
312 format (' ****Since only printer plot was specified, this plot request is cancelled.')
  go to 1000
3115 if (iout .eq. 2) go to 2645
5981 do i = ipl1, ipl2
     j = 2 * i - 1
     if (tolrce .gt. flzero) go to 5991
     kpltq(i) = iend / jplt2
     j1 = j + iend
     go to 6145
5991 kpltq(i) = 0
     ibase = j
     istore = j
     indx = j + 1 + jplt2
     vold = ev(indx)
6000 evbasx = ev(ibase)
     evbasy = ev(ibase + 1)
     ev(istore) = evbasx
     ev(istore + 1) = evbasy
     kpltq(i) = kpltq(i) + 1
     istore = istore + jplt2
     ipontr = ibase + jplt2
     hvec =  (ev(ipontr) - evbasx) / dx
     vvec = (ev(ipontr + 1) - evbasy) / dy
     denom = hvec * hvec + vvec * vvec
6020 ipontr = ipontr + jplt2
     if (iovfl .eq. 0) go to 6025
     if (ipontr .gt. iend) go to 6140
     go to 6030
6025 if (ipontr .gt. kplt) go to 6140
6030 hdif = (ev(ipontr) - evbasx) / dx
     vnew = ev(ipontr + 1)
     vdif = (vnew - evbasy) / dy
     term = hdif * hdif + vdif * vdif
     enumr = hvec * hdif + vvec * vdif
     disqr = term - enumr * enumr / denom
     if (ipontr .le. 10000 .and. i .eq. 1 .and. iprsup .ge. 5) write (unit = lunit(6), fmt = 16030) i, istore, ipontr, ev(ipontr), evbasx, dx, vnew, evbasy, dy, denom, enumr, disqr, hdif, vdif, hvec, vvec, term
16030 format (/, ' At 16030  ', 3i10, 6e15.5, /, (1x, 8e15.5))
     if (disqr .gt. tolrce) go to 6040
     vchnge = vnew - vold
     vold = vnew
     if (vvec * vchnge .gt. -flzero) go to 6020
6040 vold = vnew
     ibase = ipontr - jplt2
     go to 6000
6140 ipontr = ipontr - jplt2
     ev(istore) = ev(ipontr)
     ev(istore + 1) = ev(ipontr + 1)
     kpltq(i) = kpltq(i) + 1
     j1 = istore + jplt2
6145 ev(j1) = hmin
     ev(j1 + 1) = vmin
     j1 = j1 + jplt2
     ev(j1) = dx
     ev(j1 + 1) = dy
  end do
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2630) (kpltq(i), i = ipl1, ipl2)
2630 format (/, ' At 2630,after smoothing, the number of points per curve are', /, 10x, 4i10)
  d6 = vhs - fourth * (1.0d0 + mulplt(3))
  d9 = d6 + hgt1 / 2.0d0
  n5 = ipl1 + mulplt(3) - 1
  do ipl = ipl1, ipl2
     j1 = 2 * ipl - 1
     call newpen (kpen(ipl))
     n5 = n5 + 1
     call line (ev(j1), ev(j1 + 1), kpltq(ipl), jplt2, jstp, n5)
     if (iovfl .eq. 0) go to 2641
     if (jovfl .gt. 0) go to 2642
2641 if (mulplt(1) .gt. 0) go to 7452
     d7 = dlen - 1.2d0
     d8 = ipl
     call number (d7, vhs1, hgt1, d8, zero, llmin1)
     d7 = dlen - 1.0d0
     call plot (d7, vhs1, ll3)
     call plot (dlen, vhs1, ll2)
     d7 = dlen - 0.5d0
     d8 = vhs1 + 0.1d0
     call symbol (d7, d8, hgt1, char (ipl), zero, llmin1)
     vhs1 = vhs1 - half
     go to 2642
7452 d7 = dlen + 0.5d0
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 7502) ipl, icp, mulplt, taxmax, vhs, d6, slot
7502 format (/, ' Legend.     ipl     icp', /, 8x,  6i8,  3e20.6, /, (1x, 20a6))
     d8 = n5
     call number (d7, d6, hgt1, d8, zero, llmin1)
     d7 = d7 + 0.35d0
     call symbol (d7, d9, hgt1, char (n5), zero, llmin1)
     d7 = d7 + 0.35d0
     n6 = ipl
     if (icp .ge. 8) n6 = 2 * ipl - 1
     call symbol (d7, d6, hgt1, slot(n6), zero, ll6)
     if (mulplt(3) .eq. 0) taxmax = taxmax + 2.0d0
     mulplt(3) = mulplt(3) + 1
     if (icp .lt. 8) go to 7468
     d7 = d7 + 0.8d0
     n6 = n6 + 1
     call symbol (d7, d6, hgt1, slot(n6), zero, ll6)
7468 d6 = d6 - 0.25d0
     d9 = d9 - 0.25d0
2642 end do
2645 if (iovfl .eq. 0) go to 2650
  kpl = kpl - iend
  if (kpl .eq. 0) go to 2650
  iend = kpl
  if (iend .gt. maxevk) iend = maxevk
  read (unit = lunit(13)) (ev(j), j = 1, iend)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 5979) iend
  j1 = kpl - iend
  if (j1 .eq. 0) jovfl = 0
  go to 5980
2650 if (iout .eq. 1) go to 2653
  if (lcnt .eq. 0) go to 2651
  if (jcol .eq. 1) go to 2651
  if (lnflg .eq. 1) go to 2651
  hhpt = hpt * tmult
  if (hhpt .ge. 1.0d0) go to 2648
  write (unit = kunit6, fmt = 3105) hhpt
  go to 2651
2648 write (unit = kunit6, fmt = 3121) hhpt
2651 long1 = 1
  long2 = 75
  call linplt (long1, long2)
  write (unit = lunit(6), fmt = 2652)
2652 format (/, ' End of graph.', /, 1x)
  if (iout .eq. 2) go to 2680
2653 d7 = vh + 1.0d0
  mulplt(2) = mulplt(2) + 1
  if (mulplt(2) .lt. mulplt(1)) go to 1000
  mulplt(1) = 0
  d8 = vploff + d7
  d9 = d8 + vh - 0.001d0
  if (d9 .le. szplt) go to 2657
2656 call plot (zero, -vploff, llmin3)
  go to 2659
2657 vploff = d8
  call plot (zero, d7, llmin3)
  go to 2680
2659 hms = (taxmax + mrgn) * 1.001d0
  taxmax = 0.0d0
  vploff = 0.0d0
  jhmsp = int (hms, kind (jhmsp))
  hms = jhmsp
  jhms = jhmsp
2646 if (jhmsp .gt. 60) jhmsp = 60
  call advanz (jhmsp)
  if (jhmsp .eq. 0) go to 2647
  jhms = jhms - 60
  if (jhms .le. 0) go to 2680
  jhmsp = jhms
  go to 2646
2647 call plot (hms, zero, llmin3)
2680 if (n8 .eq. -6666) go to 2720
  if (ipl2 .eq. jplt) go to 1000
  ipl1 = ipl2 + 1
  go to 2580
2720 if (kalcom .eq. 1) call plot (zero, -vs, llmin3)
  long1 = kalcom
  call endplt (long1)
  if (isprin .ne. 43) go to 7416
  lastov = nchain
  nchain = 43
  go to 9999
7416 lastov = nchain
  if (kbase .ne. 1) go to 2724
  kbase = 2
  nchain = 12
  go to 9999
2724 nchain = 51
  go to 9999
2740 call fintp
  call stoptp
9200 lstat(18) = 31
  lastov = nchain
  nchain = 51
9999 return
end subroutine subr31

!
! subroutine series.
!

subroutine series (nfour, kpl, jplt, maxevk)
  use blkcom
  use deck31
  use tracom
  implicit none
  !  dimension evdoub(2)
  !  equivalence (evdoub(1), karray(1))
  integer(4), intent(out) :: kpl, nfour
  integer(4), intent(in) :: jplt, maxevk
  !
  integer(4) :: ioffa, ioffb
  integer(4) :: j
  integer(4) :: k
  integer(4) :: l
  integer(4) :: m
  integer(4) :: n1, n2, n4, ndx1, ndx2
  real(8) :: an, ap
  real(8) :: bp
  real(8) :: c1, cp
  real(8) :: d1, d2, d4, d5, d6, d7, d12
  real(8) :: gk, gk1, gk2
  real(8) :: pi
  real(8) :: s1, sp
  !
  if (nfour .gt. 0) go to 3184
  nfour = -nfour
  n2 = kpl
  d1 = evdoub(kpl) - evdoub(1)
3163 n2 = n2 - 1
  d2 = evdoub(n2)  - evdoub(1)
  if (d1 * d2 .gt. 0.0d0) go to 3175
  if (absz (d1) .gt. absz (d2)) n2 = n2 - 1
  write (unit = lunit(6), fmt = 3168) kpl, n2, evdoub(n2 + 1)
3168 format (/, ' Back up final time from point number', i5, '   to point number', i5, ' ,   so that signal will be nearly periodic.', /, ' The following unused point, which has value closest to point number 1, equals ', e15.6)
  kpl = n2
  go to 3184
3175 d1 = d2
  go to 3163
3184 n4 = kpl / 2  + 2
  n1 = kpl + 2 * n4
  if (n1 .le. maxevk) go to 3209
  write (unit = lunit(6), fmt = 3203) n1, maxevk
3203 format (/, ' Sorry, no fourier series is possible due to lack of emtp table space.', i8, '   Floating-point cells are needed,', /, ' while only', i8, '   are available.   On to next request.')
  go to 9000
3209 ioffa = kpl
  ioffb = kpl + n4
  if (kpl .ge. 2) go to 3216
  write (unit = lunit(6), fmt = 3213) kpl
3213 format (/, ' Sorry, no fourier series is possible due to lack of data.   The number of data points found is only', i5,'  .', /, ' Two or more are needed. On to the next request.')
  go to 9000
3216 if (jplt .eq. 1) go to 3226
  write (unit = lunit(6), fmt = 3219) jplt
3219 format (/, ' Sorry, no fourier series is possible due to the naming of', i4, '   variables.   Only one is allowed.  On to next request.')
  go to 9000
3226 write (unit = lunit(6), fmt = 3231) kpl, evdoub(1), evdoub(2), evdoub(kpl - 1), evdoub(kpl)
3231 format (/, ' Begin fourier series calculation using ', i6, '   equidistant points.   Beginning two points =', /, 2e20.10, 9x, 'ending two points =', 2e20.10)
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 3238) (evdoub(j), j = 1, kpl)
3238 format (/, ' List of all data points follows ....', /, (1x, 10e13.4))
  pi = twopi * onehaf
  an = kpl
  an = an * onehaf
  j = int (an + 1.6d0, kind (j))
  m = int (an + 1.0d0, kind (m))
  if (nfour .gt. m) nfour = m
  d12 = 1.0d0 / an
  d7 = pi * d12
  c1 = cosz (d7)
  s1 = sinz (d7)
  cp = 1.0d0
  sp = 0.0d0
  ndx1 = ioffa
  ndx2 = ioffb
  do l = 1, m
     gk2 = 0.0d0
     gk1 = 0.0d0
     k = kpl
4020 gk = evdoub(k) + cp * gk1 * 2.0d0 - gk2
     gk2 = gk1
     gk1 = gk
     k = k - 1
     if (k .gt. 1) go to 4020
     ap = (evdoub(1) + gk1 * cp - gk2) * d12
     bp = sp * gk1 * d12
     if (l .eq. 1) go to 4030
     if (l .ne. j) go to 4040
     bp = 0.0d0
4030 ap = ap * onehaf
4040 ndx1 = ndx1 + 1
     ndx2 = ndx2 + 1
     evdoub(ndx1) = ap
     evdoub(ndx2) = bp
     ap = c1 * cp - s1 * sp
     sp = c1 * sp + s1 * cp
     cp = ap
  end do
  write (unit = lunit(6), fmt = 4317)
4317 format (/, ' Coefficients of resultant fourier series, with  "complex amplitude"  being the square root', /, ' of the sum of the squares of the two preceding entries.   The final column applies to this amplitude.', /, 2x, 'harmonic', 14x, 'cosine', 16x, 'sine', 13x, 'complex', 9x, 'fraction of', /, 4x, 'number', 9x, 'coefficient', 9x, 'coefficient', 11x, 'amplitude', 9x, 'fundamental')
  ndx1 = ioffa
  ndx2 = ioffb
  d7 = evdoub(ndx1 + 2) ** 2 + evdoub(ndx2 + 2) ** 2
  d7 = sqrtz (d7)
  do k = 1, nfour
     l = k - 1
     ndx1 = ndx1 + 1
     ndx2 = ndx2 + 1
     d5 = evdoub(ndx1) ** 2 + evdoub(ndx2) ** 2
     d6 = sqrtz (d5)
     d4 = d6 / d7
     write (unit = lunit(6), fmt = 4345) l, evdoub(ndx1), evdoub(ndx2), d6, d4
  end do
4345 format (1x, i9, 3e20.10, f20.8)
  write (unit = lunit(6), fmt = 4352)
4352 format (//, 1x)
9000 return
end subroutine series

!
! subroutine begplt.
!

subroutine begplt
  use blkcom
  implicit none
  !
  if (iprsov(38) .eq. 0) call plots (0, 0, 0)
  return
end subroutine begplt

!
! subroutine endplt.
!

subroutine endplt (kalcom)
  implicit none
  !     This system-dependent module is always called once as control
  !     leaves  'subr31'  during a normal exit (no error stop).
  !     Argument  "kalcom"  is zero if the data case did no
  !     calcomp plotting, or unity if there were one or more
  !     such plots.   For most computers, the calcomp buffer
  !     is not closed here, but rather in  "fintp" .
  integer(4), intent(in) :: kalcom
  !
  write (unit = *, fmt = *) kalcom
  return
end subroutine endplt

!
! subroutine fintp.
!

subroutine fintp
  use blkcom
  implicit none
  !
  if (iprsov(38) .gt. 0) call plot (0.0d0, 0.0d0, 999)
  if (ivolt .eq. 7777) close (unit = 20, status = 'delete')
  return
end subroutine fintp

!
! subroutine axis.
!

subroutine axis (xx, yy, title, numch, size, ang, begin, scale)
  use blkcom
  implicit none
  !     Module  'axis'  was written by W. Scott Meyer of BPA in June of
  !     1976, especially for BPA EMTP usage only.   Anyone having the real
  !     calcomp module of the same name should discard this present
  !     approximate equivalent.   Unfortunately, BPA does not fall into
  !     this class (our system programmers thought they were being clever
  !     by adding two more arguments to the module which is available in
  !     our system library).
  character(8), intent(in) :: title
  integer(4), intent(in) :: numch
  real(8), intent(in) :: ang, begin, scale, size, xx, yy
  character(8) :: text1
  integer(4) :: length, n3, nchabs, ndig
  real(8) :: axmid, beg1, d1, d2, d3, d4, hgt1, hgt2, hgt3, scal1
  real(8) :: tic, ticd2, xmax
  !
  data tic   / 0.075d0 /
  data ticd2 / 0.03d0 /
  data hgt1  / 0.2d0 /
  data hgt2  / 0.15d0 /
  data hgt3  / 0.12d0 /
  data text1 / '( 10**' /
  d1 = abs (begin)
  d2 = abs (begin + size * scale)
  if (d2 .gt. d1) d1 = d2
  if (d1 .le. 0.0) go to 9000
  ndig = 0
2313 if (d1 .le. 1.000001d0) go to 2321
  ndig = ndig - 1
  d1 = d1 * 0.1d0
  go to 2313
2321 if (d1 .ge. 0.0999999d0) go to 2336
  ndig = ndig + 1
  d1 = d1 * 10.0d0
  go to 2321
2336 if (ang .gt. 0.0d0) go to 2345
  if (ndig .ge. 2) go to 2345
  if (ndig .lt. -3) go to 2345
  ndig = 0
2345 d3 = 10.0d0 ** ndig
  scal1 = scale * d3
  beg1 = begin * d3
  length = int (size + 0.000001d0, kind (length))
  axmid = size * 0.5d0 - 2.0d0
  d3 = -ndig
  nchabs = iabs (numch)
  if (ang .gt. 0.0d0) go to 2506
  call plot (xx, yy, 3)
  xmax = xx + size
  call plot (xmax, yy, 2)
  d2 = beg1 + length * scal1
2427 d1 = xx + length
  call plot (d1, yy + tic, 3)
  call plot (d1, yy - tic, 2)
  call number (d1 - 0.2d0, yy - 0.3d0, hgt2, d2, ang, 2)
  length = length - 1
  d2 = d2 - scal1
  if (length .gt. 0) go to 2427
  d1 = yy - 0.6d0
  d2 = xx + axmid
  call symbol (d2, d1, hgt1, title, ang, nchabs)
  if (ndig .eq. 0) go to 2438
  d2 = d2 + 5.0d0
  call symbol (d2, d1, hgt1, text1, ang, 6)
  d2 = d2 + 1.2
  call number (d2, d1 + .07, hgt1, d3, ang, -1)
  d2 = d2 + 0.4d0
  call symbol (d2, d1, hgt1, ')', ang, 1)
2438 go to 2569
2506 call plot (xx, yy + size, 3)
  call plot (xx, yy, 2)
  d1 = yy
  d2 = beg1
  n3 = 0
2554 call plot (xx + tic, d1, 3)
  call plot (xx - ticd2, d1, 2)
  if (iprsup .ge. 1) write (unit = 6, fmt = 4216) xx, d1, hgt3, d2, ang
4216 format (' Before y-axis call to  ', "'", 'number', "'", ' in  ', '"', 'axis', "'", ' .', 13x, 'xx', 13x, 'd1', 11x, 'hgt3', 13x, 'd2', 12x, 'ang', /, 46x, 4e15.6)
  call number (xx - 0.1d0, d1 - 0.2d0, hgt3, d2, ang, 3)
  d2 = d2 + scal1
  d1 = d1 + 1.0d0
  n3 = n3 + 1
  if (n3 .le. length) go to 2554
  d1 = yy + axmid
  d4 = xx - 0.3d0
  call symbol (d4, d1, hgt1, title, ang, nchabs)
  if (ndig .eq. 0) go to 2569
  d1 = d1 + 4.0d0
  call symbol (d4, d1, hgt1, text1, ang, 6)
  d1 = d1 + 1.2d0
  call number (d4 - 0.07d0, d1, hgt1, d3, ang, -1)
  d1 = d1 + 0.4d0
  call symbol (d4, d1, hgt1, ')', ang, 1)
2569 return
9000 write (unit = lunit(6), fmt = 9006)
9006 format (//, " Error stop within bpa cdc module  'axis'  of overlay 31.     Go see program maintenance at once.")
  call stoptp
end subroutine axis

!
! subroutine grid.
!

subroutine grid (d1, d2, d3, d4, n1, n2)
  implicit none
  !     This module has been written for use only on the BPA CDC-6500
  !     computer installation, which does not have legitimate calcomp.
  !     It simulates the calcomp module of the same name.   Anyone who
  !     has real calcomp plotting software with these subroutines should
  !     destroy this module.        W. Scott Meyer, April 1977.
  integer(4) :: i, j, ll2, ll3, n1, n2
  real(8) :: d1, d2, d3, d4, d5, d6, d8
  !
  ll2 = 2
  ll3 = 3
  d8 = n2 * d4 + d2
  d5 = d1
  d6 = d2
  i = 0
1878 i = i + 1
  if (i .gt. n1) go to 1972
  d5 = d5 + d3
  call plot (d5, d6, ll3)
  if (d6 .gt. d2) go to 1912
  d6 = d8
  go to 1941
1912 d6 = d2
1941 call plot (d5, d6, ll2)
  go to 1878
1972 d6 = d2
  d5 = d1
  d8 = d1 + n1 * d3
  j = -1
1988 j = j + 1
  if (j .gt. n2) go to 2029
  call plot (d5, d6, ll3)
  if (d5 .gt. d1) go to 2005
  d5 = d8
  go to 2013
2005 d5 = d1
2013 call plot (d5, d6, ll2)
  d6 = d6 + d4
  go to 1988
2029 return
end subroutine grid

!
! subroutine linplt.
!

subroutine linplt (krv, klm)
  use blkcom
  implicit none
  !
  !c****** this is the standard version of linplt ******
  !
  !)       linplt is the line printer plot subroutine.  it fills and
  !)    prints an output array,  kut , with the necessary symbols from the
  !)    array ktsin.  the position and type of symbol is determined in the
  !)    calling subroutine,  =subr31= .
  !        before printing, for each curve,  linplt  fills the columns be-
  !     tween the minimum and maximum points with the corresponding sym-
  !     bol.  if a different curve symbol is encountered, the common
  !     symbol  *  is inserted.  this procedure insures that =spikes= will
  !     appear as a line of symbols.   arrays  =max=  and  =min=  are used
  !     for this purpose.   but x-y printer plot bypasses this,
  !     via  krv = 77  special flag.
  !        immediately after printing, the array is blanked, to be ready
  !     for the next line of data points, before returning.
  !        the normal range of values for the calling arguments are --
  !             krv  1 to 4   ( inclusive )
  !             klm  1 to 131 ( inclusive )
  !     when called with these values,  linplt  will place the =krv= curve
  !     symbol in the =klm= column of the output array, unless that column
  !     already contains a different krv symbol, in which case the common
  !     symbol is used.
  !        the special values of krv and klm are --
  !          krv = -1   place the time axis line symbol, 1, in the klm
  !                     column of the array.  this symbol is erased by any
  !                     subsequent curve symbol.
  !          krv = 0    the first call to linplt uses this and a dummy
  !                     argument for klm to initialize linplt.  the ordi-
  !                     nate axis and tic marks are printed.  the first
  !                     output line of data will overwrite the axis.
  !          klm = -1   this and a dummy argument for krv causes the out-
  !                     put array to be printed.
  !          krv = 99   places the intersection symbol  "*"
  !                     (variable kom) in column  klm.
  !          krv = 77   bypasses the vertical fill between min
  !                     and max, just before printing.
  !
  integer(4), intent(in) :: krv, klm
  integer(4) :: i, j, jgo, jmax, jmin, k, max(4), min(4)
  character(1) :: klank, kline, kom, ktsin(4), kut(131), kxline
  !  dimension kut(131), ktsin(4)
  !  dimension max(4), min(4)
  !
  data ktsin(1)   / 'a' /
  data ktsin(2)   / 'b' /
  data ktsin(3)   / 'c' /
  data ktsin(4)   / 'd' /
  data kom        / '*' /
  data kline      / '1' /
  data klank      / ' ' /
  data kxline     / '-' /
  !    Burroughs: preserve local variable between module calls:
  data kut(1)  / ' ' /
  !    Burroughs: preserve local variable between module calls:
  data min(1) / 0 /,    max(1) / 0 /,    k / 0 /
  if (krv .eq. 99) go to 407
  jgo = 4
  if (krv .eq. -1) jgo = 1
  if (krv .eq. 0) jgo = 2
  if (klm .eq. -1) jgo = 3
  go to (100, 200, 300, 400), jgo
100 kut(klm) = kline
  go to 999
200 k = 1
  kut(1 : 131) = klank
  do j = 1, 131, 10
     kut(j) = kline
  end do
  write (unit = lunit(6), fmt = 230) kut
230 format (1x, 131a1)
  kut(1 : 131) = kxline
  write (unit = lunit(6), fmt = 250) kut
250 format ('+', 131a1)
  go to 350
300 if (krv .eq. 77) go to 334
  do j = 1, 4
     jmin = min(j)
     if (jmin .eq. 132) go to 330
     jmax = max(j)
     if (jmin .eq. jmax) go to 330
     do i = jmin, jmax
        if (kut(i) .eq. ktsin(j)) go to 320
        if (kut(i) .eq. klank) go to 310
        if (kut(i) .eq. kline) go to 310
        kut(i) = kom
        go to 320
310     kut(i) = ktsin(j)
     end do
320  continue
  end do
330 continue
334 if (k .eq. 0) go to 340
  k = 0
  write (unit = lunit(6), fmt = 250) kut
  go to 350
340 write (unit = lunit(6), fmt = 230) kut
350 do j = 1, 4
     max(j) = 1
     min(j) = 132
  end do
  kut(1 : 131) = klank
  go to 999
400 if (klm .gt. max(krv)) max(krv) = klm
  if (klm  .lt.  min(krv)) min(krv) = klm
  if (kut(klm) .eq. ktsin(krv)) go to 999
  if (kut(klm) .eq. klank) go to 410
  if (kut(klm) .eq. kline) go to 410
407 kut(klm) = kom
  go to 999
410 kut(klm) = ktsin(krv)
999 continue
  return
end subroutine linplt

!
! subroutine paprsz.
!

subroutine paprsz (horiz, vert)
  implicit none
  !)    The real (i.e., non-dummy) subroutine paprsz is used with BPA's
  !)    eai flatbed plotter.   This is the way the dimensions of the table
  !)    upon which the emtp can legally draw plots is communicated to the
  !)    system.   The plotting software then will protect the hardware,
  !)    not allowing the pen to exceed the  x  and  y  coordinates which
  !)    are specified by the arguments in the call to this subroutine.
  !)    for regular calcomp drum plotting (where there is no table as such
  !)    ), leave in this dummy subroutine as it is written here.   calling
  !)    it will do nothing, of course.
  real(8), intent(in) :: horiz, vert
  !
  write (unit = *, fmt = *) horiz, vert
  return
end subroutine paprsz

!
! subroutine advanz.
!

subroutine advanz (jhmsp)
  implicit none
  !)    The real (i.e., non-dummy) subroutine advanz is used with BPA's
  !)    eai flatbed plotter, to roll  'jhmsp'  inches of new paper onto
  !)    the plotting table, making room for the upcoming plot to be
  !)    drawn.   But with regular calcomp drum plotting (where there is
  !)    no table, as such), the present replacement module should be used,
  !)    which has two effects.   First, the rolling of the paper is
  !)    bypassed, of course.   Also, by setting  'jhmsp'  to zero, the
  !)    associated plot origin-changing is effectively disabled.
  integer(4), intent(out) :: jhmsp
  !
  jhmsp = 0
  return
end subroutine advanz

!
! subroutine prnthhd.
!

subroutine prnthd (n1)
  implicit none
  !)    The real (i.e., non-dummy) subroutine prnthd is used with BPA's
  !)    eai flatbed plotter.   It is associated with the printhead feature
  !)    of the eai plotter, where alphanumeric characters may be typed
  !)    rather than drawn with the pen, in order to speed up the plotting
  !)    operation.   subroutine prnthd is called with different values
  !)    for the argument  'n1'  in order to change modes on this
  !)    printhead usage, as follows ......
  !)           0  -----  drawn characters, using the pen.
  !)           1  -----  horizontally-typed lettering
  !)           2  -----  vertically-typed lettering
  !)    for regular calcomp drum plotting where all characters must be
  !)    drawn with the pen, leave in this dummy subroutine as it is
  !)    written here.   Calling it will do nothing, of course.
  integer(4), intent(in) :: n1
  !
  write (unit = *, fmt = *) n1
  return
end subroutine prnthd

!
! end of file over31.f90
!
