!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over42.f90
!

! Copyright 1977-2021 Bonneville Power Administration
! Copyright 2019-2021 Angelo Rossi <angelo.rossi.homelab@gmail.com>
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice,
!    this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! 3. Neither the name of the copyright holder nor the names of its contributors
!    may be used to endorse or promote products derived from this software
!    without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.

!
! subroutine over42.
!

subroutine over42
  use blkcom
  use tracom
  use freedom
  implicit none
  integer(4) :: i, ipnch
  integer(4) :: j
  integer(4) :: k, kthird
  integer(4) :: l
  integer(4) :: m, maxpt
  integer(4) :: n, n1
  real(8) :: a1, a2, a3
  real(8) :: bb, bbb
  real(8) :: c1, c1old, cc, cfact, check, cnew, currr(101)
  real(8) :: d1, d2, d11, dflux, dltat
  real(8) :: flux(101), freq
  real(8) :: oldc, oldf, omegah
  real(8) :: pbase, pi, picon, psi, psiold
  real(8) :: ratio
  real(8) :: slope1, slope2, sq2, sss(90)
  real(8) :: tee
  real(8) :: v, vbase, vfact, vold
  real(8) :: xx
  real(8) :: y
  !  dimension  currr(101), flux(101), sss(90)
  !
  !     the following limit checks overflow of  curr, flux
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module over42."')
  maxpt = 101
  pi = twopi / 2.0d0
  picon = pi / 180.0d0
  do i = 1, 90
     sss(i) = sinz (i * picon)
  end do
  d1 = 2.0d0
  sq2 = sqrtz (d1) * 1000.0d0
  !     check for "convert zno" connection of "reques" (overlay 1):
  if (iofbnd .ne. 99876) go to 5
  call zinold
  go to 9200
5 n = 1
  !     read input card using cimage
  call cimage
  if (kolbeg .gt. 0) go to 2103
  read (unit = abuff, fmt = 10) freq, vbase, pbase, ipnch, kthird
10 format (3e8.0, 2i8)
  go to 2105
2103 nfrfld = 1
  !  call freone (freq)
  call ffree (freq)
  !  call freone (vbase)
  call ffree (vbase)
  !  call freone (pbase)
  call ffree (pbase)
  !  call freone (d11)
  call ffree (d11)
  ipnch = int (d11, kind (ipnch))
  !  call freone (d11)
  call ffree (d11)
  kthird = int (d11, kind (kthird))
  if (kill .gt. 0) go to 9200
2105 if (freq .eq. 0.0d0) go to 2050
  if (freq .ne. 88.0d0) go to 7219
  !     connect with routine written by prof. ned mohan of the
  !     university of minnesota.   this generates the current
  !     vs. flux points needed for  type-96  hysteretic inductor
  !     modeling.
  write (unit = kunit6, fmt = 7215)
7215 format ('+freq=88 requests hysteresis data.')
  call hysdat
  if (kill .gt. 0) go to 9200
  go to 5
7219 if (freq .ne. 77.0d0) go to 7228
  !     connect with routine written by dr. vladimir brandwajn
  !     of ontario hydro.  this generates emtp data cards for
  !     zno  surge arresters using least squares fitting.
  write (unit = kunit6, fmt = 7221)
7221 format ('+freq=77 requests  Zno  data generator.')
  call arrdat
  if (kill .gt. 0) go to 9200
  go to 5
7228 write (unit = kunit6, fmt = 15) freq, vbase, pbase, ipnch
15 format ('+Misc. const.  ', 2x, 3e10.3, i5)
  if (freq .eq. -1.0d0) go to 19
  if (vbase .gt. 0.0d0) go to 18
16 kill = 90
  flstat(14) = vbase
  flstat(15) = pbase
  lstat(19) = 16
  go to 9200
18 if (pbase .le. 0.0d0) go to 16
  flux(1) = 0.0d0
  currr(1) = 0.0d0
  omegah = twopi * freq
  vfact = sq2 * vbase / omegah
  cfact = pbase * 1000.0d0 / vbase
  dltat = picon
  slope1 = fltinf
  c1old = 0.0d0
  vold = 0.0d0
  go to 20
19 if (pbase .le. 0.0d0) pbase = 1.0d0
  if (vbase .le. 0.0d0) vbase = 1.0d0
  vfact = pbase
  cfact = vbase
  c1old = -1.0d0 / fltinf
  n = 0
  !     read input card using cimage
20 call cimage
  if (kolbeg .gt. 0) go to 2113
  read (unit = abuff, fmt = 23) c1, v
23 format (2e16.0)
  go to 2115
2113 nfrfld = 1
  !  call freone (c1)
  call ffree (c1)
  if (c1 .eq. 9999.0d0)   go to 2115
  !  call freone ( v )
  call ffree (v)
  if (kill .gt. 0) go to 9200
2115 if (c1 .eq. 9999.0d0) go to 100
  if (freq .gt. 0.0d0) go to 41
  write (unit = kunit6, fmt = 38) c1, v
38 format ('+(i, l) point.  ', 2x, 2e13.5)
  go to 49
41 write (unit = kunit6, fmt = 45) c1, v
45 format ('+(i, v) point.  ', 2x, 2e13.5)
49 n = n + 1
  if (n .le. maxpt) go to 70
  kill = 69
  lstat(12) = maxpt
  lstat(19) = 70
  go to 9200
70 if (freq .gt. 0.0d0) go to 74
  if (c1 .le. c1old) go to 75
  if (v .le. 0.0d0) go to 75
  go to 90
74 if (v .gt. vold) go to 80
75 lstat(12) = n - 1
  flstat(13) = c1
  flstat(14) = c1old
  flstat(15) = v
  flstat(16) = vold
  kill = 71
  lstat(19) = 75
  go to 9200
80 if (c1 .le. c1old) go to 75
  slope2 = (v - vold) / (c1 - c1old)
  if (slope1 .ge. slope2) go to 90
  write (unit = lunit(6), fmt = 82)
82 format (/, ' ************************************************************************************************************************ ', /, 1x)
  write (unit = lunit(6), fmt = 86)
86 format(' Warning.  The last data point causes the v-i curve to deviate from the expected convex form.  That is the slope  ', /, &
        ' of this section is greater than that of the previous section.  Although this is not considered an error, this    ', /, &
        ' condition warrents double checking the data for accuracy. ')
  write (unit = lunit(6), fmt = 82)
90 slope1 = slope2
  c1old = c1
  vold = v
  flux(n) = v * vfact
  currr(n) = c1 * cfact
  go to 20
100 if (n .gt. 1) go to 1784
  if (freq .lt. 0.0d0) go to 1784
  write (unit = kunit6, fmt = 45) c1, v
  write (unit = lunit(6), fmt = 1742)
1742 format (' Problem ----- the very first point of the characteristic has just been read, and it has been observed that this   ', /, &
          15x, 'has voltage equal to zero.   This is illegal.   The first section of the curve must have positive slope,     ', /, &
          15x, 'and the origin (i=0, v=0) is an implied point, not to be inputted by the user.   The EMTP will discard the   ', /, &
          15x, 'just-read card, and begin reading the characteristic points once again, under the assumption that the user   ', /, &
          15x, 'has overlooked this minor detail.   Talk about a forgiving program (sometimes).    ')
  go to 20
1784 write (unit = kunit6, fmt = 65)
65 format ('+special termination-of-points card.')
  call interp
  write (unit = lunit(6), fmt = 105)
105 format(/, ' Derived saturation curve giving peak current vs. flux   ', /, 7x, 'row', 17x, 'current (amp)', 15x, &
         'flux (volt-sec)   ')
  if (freq .gt. 0.0d0) go to 195
  psiold = 0.0d0
  if (n .ge. 2) go to 124
  write (unit = lunit(6), fmt = 117)
117 format ( //, ' Note. ---- the just-read current vs. incremental inductance characteristic does not contain two or more    ', /, &
         12x, 'points.   No curve is thus defined, and the EMTP can not integrate inductance to produce flux.   This    ',/, &
         12x, "'saturation'  data case is thus being aborted in overlay 42, and control will pass on to read the next    ", /, &
         12x,  "such 'saturation' data case.   ", //, 1x)
  go to 5
124 if (currr(1) .eq. 0.0d0) go to 138
  d1 = flux(1) / vfact
  write (unit = lunit(6), fmt = 128) d1
128 format ( //, ' Warning. ---- the just-read current vs. incremental inductance characteristic does not begin with a point for  ', /, &
         15x, 'zero current.   This missing point is being automatically supplied by the emtp, with associated    ',/, &
         15x,  'inductance value taken to equal the first-inputed value, which was ', e15.5, '  .   Has the user    ', /, &
         15x,  'ever seen such an intelligent, benevolent, considerate, forgiving program as this.    ', //, 1x)
  do j = 1, n
     l = n + 1 - j
     currr(l + 1) = currr(l)
     flux(l + 1) = flux(l)
  end do
  currr(1) = 0.0d0
  n = n + 1
138 psiold = 0.0d0
  do j = 2, n
     d1 = (flux(j - 1) + flux(j)) / 2.0d0
     d2 = currr(j) - currr(j-1)
     flux(j - 1) = psiold
     psiold = psiold + d1 * d2
  end do
  flux(n) = psiold
  write (unit = lunit(6), fmt = 147)
147 format (10x, 100('-'), /, 10x, 'Rremember.   The just-completed conversion began with a current vs. incremental inductance     ', /, &
         10x, "characteristic, due to miscellaneous data parameter  'freq'  of columns 1-8 being punched with a value of      ", /, &
         10x, ' -1.0  .    Trapezoidal rule integration of the inductance curve was used, to produce flux. ', /, &
         10x, 100('-'))
  go to 500
195 currr(2) = currr(2) * sq2 / 1000.0d0
  i = 2
200 if (i .ge. n) go to 500
  i = i + 1
  y = flux(i)
  check = flux(2)
  tee = 0.0d0
  dflux = check
  cnew = currr(2)
  ratio = cnew / check
  oldc = 0.0d0
  oldf = 0.0d0
  a1 = 0.0d0
  a2 = 0.0d0
  a3 = 0.0d0
  k = 2
  m = 0
215 m = m + 1
  tee = tee + dltat
  psi = y * sinz (tee)
  if (psi .le. check) go to 230
  k = k + 1
  oldf = check
  check = flux(k)
  dflux = check - oldf
  oldc = cnew
  cnew = currr(k)
  ratio = (cnew - oldc) / dflux
  if (k .le. i) go to 224
  kill = 70
  lstat(19) = 224
  goto 9200
224 if (k .eq. i) go to 350
230 a1 = a1 + (oldc + (psi - oldf) * ratio) ** 2
  go to 215
350 cfact = oldc * 2.0d0
  vfact = oldc ** 2
310 a1 = a1 + vfact
  ratio = (psi - oldf) / dflux
  a2 = a2 + cfact * ratio
  a3 = a3 + ratio ** 2
  if (m .ge. 90) go to 400
  tee = tee + dltat
  m = m + 1
  psi = y * sinz (tee)
  go to 310
400 a1 = a1 - vfact / 2.0d0 - 90.0d0 * currr(i) ** 2
  a2 = a2 - cfact * ratio / 2.0d0
  a3 = a3 - ratio ** 2 / 2.0d0
  c1 = (-a2 + sqrtz (a2 ** 2 - 4.0d0 * a1 * a3)) / (2.0d0 * a3)
  currr(i) = oldc + c1
  go to 200
500 if (kthird .ne. 1) go to 503
  n1 = n - 1
  do j = 1, n1
     l = n + 1 - j
     d1 = -currr(l)
     d2 = -flux(l)
     i = -l
     write (unit = lunit(6), fmt = 504) i, d1, d2
     if (ipnch .eq. 0) write (unit = lunit(7), fmt = 505) d1, d2
  end do
503 do i = 1, n
     if (i .ne. 1) go to 1503
     if (kthird .eq. 1) go to 510
1503 write (unit = lunit(6), fmt = 504) i, currr(i), flux(i)
504  format (i10, 2f30.10)
     if (i .eq. 1) go to 510
     if (ipnch .eq. 0) write (unit = lunit(7), fmt = 505) currr(i), flux(i)
505  format (2e16.8)
510 end do
  write (unit = lunit(6), fmt = 527)
527 format (36x, 4h9999)
  if (ipnch .eq. 0) write (unit = lunit(7), fmt = 531)
531 format (12x, '9999')
  if (freq .eq. -1.0d0) go to 1029
  write (unit = lunit(6), fmt = 800)
800 format (/, ' Check of derived curve by independent reverse computation.  Assuming sinusoidal voltage (flux) at level of each point,   ', /, &
         ' rms current is found numerically.  This curve should be equal to the original i-v points inputted.')
  write (unit = lunit(6), fmt = 17)
17 format (2x, 'row', 5x, 'current in p.u.', 5x, 'voltage in p.u.')
  i = 1
  bb = flux(1)
  do l = 2,n
     cc = 0.0d0
     xx = flux(l)
     do m = 1, 90
        v = xx * sss(m)
        if (v .gt. bb) go to 960
940     j = i - 1
        bbb = flux(j)
        if (bbb .le. v) go to 980
        i = j
        bb = bbb
        go to 940
960     i = i + 1
        bb = flux(i)
        if (bb .lt. v) go to 960
980     a1 = (v - flux(i - 1)) * (currr(i) - currr(i - 1)) / (flux(i) - flux(i - 1))
        a2 = (currr(i - 1) + a1) ** 2
        cc = cc + a2
     end do
     c1 = sqrtz ((cc - a2 / 2.0d0) / 90.0d0) * vbase / (pbase * 1000.0d0)
     v = flux(l) * omegah / (sq2 * vbase)
     write (unit = lunit(6), fmt = 1010) l, c1, v
1010 format (i5, 2f20.8)
  end do
1029 write (unit = lunit(6), fmt = 1031)
1031 format (//, 1x)
  go to 5
2050 write (unit = kunit6, fmt = 2055)
2055 format ('+Blank card terminating all saturation cases.')
  call interp
  lastov = nchain
  nchain = 51
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module over42."')
  go to 99999
9200 lastov = nchain
  nchain = 51
  lstat(18) = 42
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
99999 return
end subroutine over42

!
! subroutine hysdat.
!

subroutine hysdat
  use blkcom
  use freedom
  implicit none
  integer(4) :: i, ibot(1, 4), itop(1, 4)
  integer(4) :: level
  integer(4) :: n1, n2
  real(8) :: b(50)
  real(8) :: cursat
  real(8) :: d1
  real(8) :: flxsat
  real(8) :: h(50)
  real(8) :: scalef, scalei
  !  dimension b(50),h(50),itop(1,4),ibot(1,4)
  !     set the top and bottom pointers.
  !
  data itop / 1, 5, 15, 29 /
  data ibot / 4, 14, 28, 50/
  !
  !     set the b points (kilogauss)
  b(1)  =  -15.6
  b(2)  =   14.6
  b(3)  =   17.0
  b(4)  =   17.1
  b(5)  =  -16.5
  b(6)  =  -15.63
  b(7)  =  -14.4
  b(8)  =  -11.2
  b(9)  =    9.7
  b(10) =   12.8
  b(11) =   14.7
  b(12) =   16.0
  b(13) =   17.0
  b(14) =   17.1
  b(15) =  -16.6
  b(16) =  -16.4
  b(17) =  -15.9
  b(18) =  -15.4
  b(19) =  -14.2
  b(20) =  -12.0
  b(21) =    8.6
  b(22) =   12.3
  b(23) =   13.8
  b(24) =   15.0
  b(25) =   15.8
  b(26) =   16.4
  b(27) =   17.0
  b(28) =   17.1
  b(29) =  -16.6
  b(30) =  -16.5
  b(31) =  -16.15
  b(32) =  -15.8
  b(33) =  -15.5
  b(34) =  -14.9
  b(35) =  -14.2
  b(36) =  -13.0
  b(37) =  -11.0
  b(38) =  - 8.0
  b(39) =    5.35
  b(40) =    7.4
  b(41) =   10.0
  b(42) =   12.0
  b(43) =   13.0
  b(44) =   14.0
  b(45) =   14.9
  b(46) =   15.6
  b(47) =   16.1
  b(48) =   16.6
  b(49) =   17.0
  b(50) =   17.1
  !     set the h points (oersteds)
  h( 1) =   .04
  h( 2) =   .17
  h( 3) =  1.60
  h( 4) =  2.20
  h( 5) =  -.40
  h( 6) =  -.05
  h( 7) =   .03
  h( 8) =   .07
  h( 9) =   .135
  h(10) =   .21
  h(11) =   .36
  h(12) =   .665
  h(13) =  1.60
  h(14) =  2.20
  h(15) =  -.60
  h(16) =  -.30
  h(17) =  -.10
  h(18) =  -.02
  h(19) =   .035
  h(20) =   .066
  h(21) =   .12
  h(22) =   .19
  h(23) =   .27
  h(24) =   .40
  h(25) =   .59
  h(26) =   .92
  h(27) =  1.60
  h(28) =  2.20
  h(29) =  -.60
  h(30) =  -.40
  h(31) =  -.18
  h(32) =  -.08
  h(33) =  -.03
  h(34) =   .01
  h(35) =   .035
  h(36) =   .058
  h(37) =   .07
  h(38) =   .08
  h(39) =   .10
  h(40) =   .11
  h(41) =   .14
  h(42) =   .18
  h(43) =   .218
  h(44) =   .285
  h(45) =   .39
  h(46) =   .535
  h(47) =   .70
  h(48) =  1.00
  h(49) =  1.60
  h(50) =  2.20
3548 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 90)
90 format (1x, 'enter itype and level in 2i1 format')
  !     read input card using cimage.
  call cimage
  if (kolbeg .gt. 0) go to 3628
  read (unit = abuff, fmt = 100) itype, level, ipunch
100 format (10i8)
  if (itype .ne. 0) go to 3645
  if (level .ne. 0) go to 3645
  if (ipunch .ne. 0) go to 3645
  write (unit = kunit6, fmt = 3622)
3622 format ('+Blank card ending hysteresis-curve requests.')
  call interp
  go to 9200
3628 nfrfld = 3
  !  call frefld (voltbc(1))
  call ffree (voltbc)
  itype = int (voltbc(1), kind (itype))
  level = int (voltbc(2), kind (level))
  ipunch = int (voltbc(3), kind (ipunch))
3645 write (unit = kunit6, fmt = 3648) itype, level, ipunch
3648 format ('+Begin B-H.  (itype, level, ipunch) =', 3i4)
  if (itype .eq. 1) go to 125
  write (unit = lunit(6), fmt = 110) itype
110 format (1x, 'job is being halted- the value of itype=', i3, 2x, 'is illegal')
  go to 3548
125 if ((level .ge. 1) .and. (level .le. 4)) go to 150
  write (unit = lunit(6), fmt = 130) level
130 format (1x, 'job is being halted- the value of level=', i3, 2x, 'is illegal')
  go to 3548
150 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 155)
155 format (1x, 'enter isat(amps) and flxsat(wb-turns) 2f10.4 format')
  !     read input card using cimage
  call cimage
  if (kolbeg .gt. 0) go to 3682
  read (unit = abuff, fmt = 160) cursat, flxsat
160 format (10e8.0)
  go to 3695
3682 nfrfld = 1
  !  call freone (cursat)
  call ffree (cursat)
  !  call freone (flxsat)
  call ffree (flxsat)
  if (kill .gt. 0) go to 9200
3695 write (unit = kunit6, fmt = 3703) cursat, flxsat
3703 format ('+cursat, flxsat =', 2e13.4)
  n1 = itop(itype, level)
  n2 = ibot(itype, level)
  scalef = flxsat / b(n2 - 1)
  scalei = cursat / h(n2 - 1)
  !     rescale the loop and write to the punch unit.
  write (unit = lunit(6), fmt = 3718)
3718 format (/, 20x, 'Derived type-96 characteristic follows:', /, 20x, 9x, 'current', 12x, 'flux')
  do i = n1, n2
     h(i) = scalei * h(i)
     b(i) = scalef * b(i)
     write (unit = lunit(6), fmt = 175) h(i), b(i)
175  format (20x, 2e16.7)
     if (ipunch .eq. 0) write (unit = lunit(7), fmt = 178) h(i), b(i)
178  format (2e16.8)
  end do
  d1 = 9999.0d0
  write (unit = lunit(6), fmt = 175) d1
  if (ipunch .eq. 0) write (unit = lunit(7), fmt = 178) d1
  write (unit = lunit(6), fmt = 175)
  go to 3548
9200 return
end subroutine hysdat

!
! subroutine arrdat.
!

subroutine arrdat
  use blkcom
  use tracom
  use movcop
  implicit none
  !     program to perform least-squares fit of straight-line segments to
  !     a set of data points. the number of segments is limited to maxexp
  character(8) :: text1, text2, text3, text4
  character(8) :: text92, text55, textb, textc, textna(60)
  integer(4) :: i, i1, icar, ifit, ij, ikb, ip, iphase, iprzno
  integer(4) :: j, jcase, jd, jk, jkb, jkp, ju, jv
  integer(4) :: k, ki, kp, ks, ku
  integer(4) :: maxexp
  integer(4) :: n4, n6, nb, nbran, nd(2), ndat, nexp, nk, nk1, nkc, nt(100)
  real(8) :: a, a1, a2, a3, a4, a5, ainf, ala5, alb, alvref, amin
  real(8) :: b, bg(200), bk, bu, bv, bx, by
  real(8) :: c(200), cc(200), ck(200), cref
  real(8) :: d, d1, d66, derlim, dq
  real(8) :: e, err, errlim
  real(8) :: f
  real(8) :: g
  real(8) :: s(200)
  real(8) :: v(200), vc(200), vcor1, vcorr, vflash, vmin, vref, vs(200)
  !  dimension v(200),c(200),vc(200),cc(200),ck(200)
  !  dimension nt(100),s(200),bg(200)
  !  dimension vs(200),nd(2)
  !
  data text1 / 'gapp' /
  data text2 / 'ed  ' /
  data text3 / 'gapl' /
  data text4 / 'ess ' /
  data textb / 'branch' /
  data text92 / '92' /
  data text55 / '5555.' /
  !
  !     define constants*************************************************
  maxexp = 100
  nbran = 60
  derlim = 0.05d0
  ainf = alog1z (fltinf)
  jcase = 0
1200 icar = 0
  ndat = 0
  !  call copya( blank, textna( 1 ), nbran )
  call copy (blank, textna(1 :), nbran)
1202 continue
  !     read input card using cimage
  call cimage
  !     check for "branch" request
  read (unit = abuff, fmt = 1208) textc
1208 format (a6)
  if (textc .ne. textb) go to 1212
  !     process terminal nodes input*************************************
  nkc = ndat + 12
  if (nkc .le. nbran) go to 1308
  lstat(19) = 1308
  write (unit = lunit(6), fmt = 1302) nkc, nbran
1302 format (/, ' required number of nodes equal', i10, 'is greater than maximum which equals', i10)
  go to 9200
1308 continue
  nk1 = ndat + 1
  ndat = ndat + 12
  read (unit = abuff, fmt = 1304) (textna(jv), jv = nk1, ndat )
1304 format (8x, 12a6)
  write (unit = kunit6, fmt = 1305)
1305 format ('+bus names for each phase.')
  go to 1202
  !     process miscellaneous data card**********************************
1212 continue
  read (unit = abuff, fmt = 1220) nexp, iphase, d66, iprzno, vref, vflash
1220 format (2i12, e12.0, i12, 2e12.0)
  if (nexp .ne. 0 .or. iphase .ne. 0) go  to  1232
  write (unit = kunit6, fmt = 1226)
1226 format ('+Blank card ends arrester cases.')
  go to 9900
1232 write (unit = kunit6, fmt = 1235) nexp, iphase, iprzno, vref, vflash
1235 format ('+arrester.', 3i4, 2e12.3)
  if (iprzno .ge. 3) write (unit = lunit(6), fmt = 1240)
1240 format (//, ' begin next data case. **********  **********  **********  **********')
  jcase = jcase + 1
  ifit = 1
  if (nexp .gt. 0) go to 1260
  ifit = -1
1260 if (vref .gt. 0.0d0) go to 1280
  vref = 0.0d0
1280 if (iphase .le. 0) iphase = 1
  jkp = 0
  nd(2) = 0
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 1540) a1, a2, a3, a4, a5, amin
  write (unit = kunit6, fmt = 1324) a1, a2, a3, a4
1324 format ('+ratings.', 4e10.2)
  if (a1 .gt. 0.0d0) go to 1340
  lstat(19) = 1328
  write (unit = lunit(6), fmt = 1328) a1
1328 format (/, ' illegal non-positive  "a1"  equals', e15.5)
  go to 9200
1340 if (a2 .gt. 0.0d0) go to 1360
  lstat(19) = 1348
  write (unit = lunit(6), fmt = 1348) a2
1348 format (/, ' illegal non-positive  "a2"  equals', e15.5)
  go to 9200
1360 if (a3 .gt. 0.0d0) go to 1380
  lstat(19) = 1366
  write (unit = lunit(6), fmt = 1366) a3
1366 format (/, ' illegal non-positive  "a3"  equals', e15.5)
  go to 9200
1380 if (a4 .gt. 0.0d0) go to 1400
  lstat(19) = 1387
  write (unit = lunit(6), fmt = 1387) a4
1387 format (/, ' illegal non-positive  "a4"  equals', e15.5)
  go to 9200
1400 if (amin .gt. 0.0d0) go to 1420
  lstat(19) = 1402
  write (unit = lunit(6), fmt = 1402) amin
1402 format (/, ' illegal non-positive  "amin"  equals', e15.5)
  go to 9200
1420 if (a5 .eq. 0.0d0) a5 = 1.0d0
  if (vref .gt. 0.0d0) go to 1421
  vref = 2.0d0 * a2
1421 if (a5 .eq. 1.0d0 .and. vflash .eq. 0.0d0) go to 1425
  if (a5 .ne. 1.0d0 .and. vflash .gt. 0.0d0) go to 1425
  lstat(19) = 1421
  write (unit = lunit(6), fmt = 1422) a5, vflash
1422 format (/, ' illegal "a5"-"vflash" combination', 2e15.5)
  go to 9200
1425 if (iprzno .lt. 3) go to 1451
  write (unit = lunit(6), fmt = 1320) nexp, vref, iphase, vflash
1320 format (5x, ' number of segments', 5x, ' reference voltage', 5x, ' number of phases', &
          5x, ' flashover voltage', /, 5x, i19, 5x, e18.8, 5x, i17, 5x, e18.8)
  write (unit = lunit(6), fmt = 1440) a1, a2, a3, a4, a5
1440 format (2x, 'constants for conversion of input data', /,  7x, &
          'original rating', 12x, 'new rating', 4x, &
          'voltage multiplier', 4x, 'current multiplier', 6x, &
          'additional blocks factor', /, 4e22.8, e30.8)
1451 amin = alog1z (amin * a4)
  jd = 0
  alvref = alog1z (vref)
  ala5 = 0.0d0
  if (a5 .gt. 0.0d0) ala5 = alog1z (a5)
1455 if (ifit .gt. 0) go to 1460
  write (unit = lunit(6), fmt = 1456) d66
1456 format (2x, ' tolerance "errlim".', 5x, e15.6)
  if (d66 .le. 0.0d0) d66 = derlim
  errlim = d66
  nexp = 1
  !     process cards with arrester data*********************************
1460 if (nexp .le. maxexp) go to  1465
  write (unit = lunit(6), fmt = 3278) maxexp
  lstat(19) = 1460
  go to 9200
1465 icar = 0
  jkp = jkp + 1
  dq = (a2 / a1) * a3
  j = 0
  nk = 0
1480 i = 0
  ck1 = 0.0d0
  ci1 = 0.0d0
  d = 0.0d0
  e = 0.0d0
  f = 0.0d0
  g = 0.0d0
  n6 = nk + 1
  !     read input card using cimage.
1520 call cimage
  read (unit = abuff, fmt = 1540) a, b
1540 format ( 6e12.0 )
  icar = icar + 1
  if (a .eq. 0.0d0 .and. icar .gt. 2) go to 1620
  write (unit = kunit6, fmt = 1546) a, b
1546 format ('+(i,v) point.', 2e15.6)
  nk = nk + 1
  if (a .gt. ck1 .and. b .gt. ci1) go to  1590
  write (unit = lunit(6), fmt = 1587) a, b
1587 format (/, ' nonpositve or nonmomtonic "a" and/or "b" the read-in values are', 2e15.6)
  lstat(19) = 1580
  go to 9200
1590 ck1 = a
  ci1 = b
  i = i + 1
  a = a * a4
  cc(nk) = a
  vc(nk) = b *  dq
  a = alog1z (a)
  b = alog1z (vc(nk)) - alvref
  v(nk) = b
  c(nk) = a
  if (iprzno .ge. 8) write (unit = lunit(6), fmt = 1600) a, b
1600 format (5x, 4e25.10)
  e = e + a
  d = d + b
  f = f + b * b
  g = g + a * b
  go  to  1520
1620 write (unit = kunit6, fmt = 1621)
1621 format ('+Blank card ends characteristic.')
  call interp
  if (iprzno  .ge.  2) write (unit = lunit(6), fmt = 1622) (c(ip), v(ip), ip = n6, nk)
1622 format (' Input data after conversion to a log-log plane  ==================  ', /, 23x,  'current', &
          18x, 'voltage', /, (5x, 2e25.15))
  if (ifit .lt. 0) go to 3100
  !     code for user defined segments***********************************
  if (iprsup .gt. 1)  write (unit = lunit(6), fmt = 1640) d, e, f, g
1640 format (1x, 'constants for the fitting procedure', /, ' d =', e16.7, 5x, 'e =', e16.7, 5x, &
          'f =', e16.7, 5x, 'g =', e16.7)
  j = j + 1
  a = (g - (d * e) / i) / (f - (d * d) / i)
  ju = j + jd
  s(ju) = a
  bv = (e - a * d) / i
  by = bv
  bx = absz (by)
  if (ala5 .eq. 0.0d0) go to 1642
  bu = bv - a * ala5
  if (absz (bu) .gt. bx) by = bu
1642 if (absz (by) .le. ainf) go to 1650
  vcor1 = ainf * bx / bv - by
  vcorr = vcor1 / a
  do ku = 1, nk
     v(ku) = v(ku) - vcorr
  end do
  cref = vref
  vref = vref * 10.0d0 ** vcorr
  write (unit = lunit(6), fmt = 3354) cref, vref
3354 format (2x, 'vreference reset to prevent numerical overflow. The old and new values follow....', 2x, 2e15.6)
  alvref = alvref + vcorr
  bv = bv + vcor1
  do ku = 1, ju
     bg (ku) = bg(ku) + vcor1
  end do
  if (jd .eq. 0) go to 1650
  do ku = 1, jd
     vs(ku) = vs(ku) - vcorr
  end do
1650 bg(ju) = bv
  if (iprzno .gt. 1) write (unit = lunit(6), fmt = 1660) j, i, s(ju), bg(ju)
1660 format (/, ' calculated values.  j, i =', 2i5, 5x, 's(ju), bg(ju) =', 2e20.10)
  nt(j) = i
  if (j .lt. nexp) go to 1480
  go to 1680
  !     start automatic determination of segments************************
3100 jkb = n6 - 1
  nexp = 0
3200 jkb = jkb + 1
  if (jkb .gt. nk) go to 1680
  if (jkb .lt. nk) go to 3250
  nt (j) = nt(j) - 1
  jkb = jkb - 1
3250 j = j + 1
  i1 = 1
  nexp = nexp + 1
  if (nexp .le. maxexp) go to 3280
  lstat(19) = 3280
  write (unit = lunit(6), fmt = 3278) maxexp
3278 format (/, 'The present case requires more than the present limit of ', i4, ' exponentials  ')
  go to 9200
3280 ju = j + jd
  e = c(jkb)
  d = v(jkb)
  f = d * d
  g = e * d
  ikb = jkb
  !     start internal fitting loop**************************************
3300 ikb = ikb + 1
  if (ikb .le. nk) go to 3350
  nt(j) = nt(j) - 1
  go to 1680
3350 i1 = i1 + 1
  a = c(ikb)
  b = v(ikb)
  e = e + a
  d = d + b
  f = f + b * b
  g = g + a * b
  d1 = d / i1
  a = (g - d1 * e) / (f - d1 * d)
  bv = (e - a * d) / i1
  by = bv
  bx = absz (by)
  if (ala5 .eq. 0.0d0) go to 3342
  bu = bv - a * ala5
  if (absz (bu) .gt. bx) by = bu
3342 if (absz (by) .le. ainf) go to 3352
  vcor1 = ainf * bx / bv  -  by
  vcorr = vcor1 / a
  do ku = 1, nk
     v(ku) = v(ku) - vcorr
  end do
  cref = vref
  vref = vref * 10.0d0 ** vcorr
  write (unit = lunit(6), fmt = 3354) cref, vref
  alvref = alvref + vcorr
  bv = bv + vcor1
  do ku = 1, ju
     bg(ku) = bg(ku) + vcor1
  end do
  if (jd .eq. 0) go to 3352
  do ku = 1, jd
     vs(ku) = vs(ku) - vcorr
  end do
3352 alb = 10.0d0 ** bv
  !     check local fitting errors after expansion***********************
  do ks = jkb, ikb
     ck(ks) = alb * (vc(ks) / vref) ** a
     err = absz (ck(ks) - cc(ks)) / cc(ks)
     if (err .gt. errlim) go to 3500
  end do
  bg(ju) = bv
  s(ju) = a
  nt(j) = i1
  if (iprzno .gt. 1) write (unit = lunit(6), fmt = 1660) j, i1, s(ju), bg(ju)
  go to 3300
  !     remove last point (error too large)******************************
3500 jkb = ikb - 2
  if (nexp .gt. 1) nt(j) = nt(j) - 1
  go to 3200
1680 vmin = (amin - bg(jd + 1)) / s(jd + 1)
  vs(jd + 1) = vmin
  nd(jkp) = nexp
  if (nexp .lt. 2) go to 1720
  nb = nexp - 1
  do i = 1, nb
     k = i + jd
     j = k + 1
     a = (bg(j) - bg(k)) / (s(k) - s(j))
     vs(j) = a
  end do
1720 if (iprzno .lt. 2) go to 1780
  do i = 1, nexp
     ju = i + jd
     write (unit = lunit(6), fmt = 1740) i, bg(ju), vs(ju)
1740 format (i5, 4e20.10)
  end do
1780 write (unit = lunit(6), fmt = 1800)
1800 format (/, 3x, '**********  error statistics  **********', /, &
          3x, 'segment', 14x, 'local error', 8x, 'accumulated error')
  jk = 0
  b = 0.0d0
  do j = 1, nexp
     a = 0.0d0
     ju = jd + j
     bv = 10.0d0 ** bg(ju)
     i = nt(j)
     ki = jk + 1
     jk = jk + i
     do k = ki, jk
        ck(k) = bv * (vc(k) / vref) ** s(ju)
        a = a + (c(k) - s(ju) * v(k) - bg(ju)) ** 2
     end do
     b = b + a
     write (unit = lunit(6), fmt = 1860) j, a, b
  end do
1860 format (i10, 2x, 2e20.10)
  if (iprzno .lt. 1) go to 1940
  write (unit = lunit(6), fmt = 1880)
1880 format (/, '  comparison between input data and the recalculated points', /, &
          8x,  'input voltage', 7x, 'input current', 2x, 'calculated current  &&&&&&&&')
  do  k = 1, jk
     write (unit = lunit(6), fmt = 1900) vc(k), cc(k), ck(k)
1900 format (3e20.10)
  end do
1940 if (a5 .eq. 0.0d0 .or. a5 .eq. 1.0d0) go to 2080
  jd = jd + nexp
  if (a5 .lt. 0.0d0) go to 2020
  if (iprzno .ge. 1) write (unit = lunit(6), fmt = 1960) a5
1960 format (/, '  reprocess input data.  Multiplier "a5" =', e20.10)
  if (iprzno .ge. 2) write (unit = lunit(6), fmt = 1622)
  jk = 0
  jkp = jkp + 1
  do j = 1, nexp
     i = nt(j)
     ki = jk + 1
     jk = jk + i
     kp = 0
     do k = ki, jk
        kp = kp + 1
        vc(k) = vc(k) * a5
        v(k) = v(k) + ala5
     end do
     ju = j + jd
     a = s( j )
     s(ju) = a
     bg(ju) = bg(j) - a * ala5
     if (iprzno .gt. 1) write (unit = lunit(6), fmt = 1660) j, kp, s(ju), bg(ju)
  end do
  a5 = 0.0d0
  go to 1680
2020 continue
  write (unit = lunit(6), fmt = 2040) jcase
2040 format ('  process second part of case no.', i6, '   &&&&&&&&&&&&&&&&&')
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 1220) nexp
  ifit = 1
  if (nexp .le. maxexp) go to 2060
  ifit = -1
2060 a5 = 0.0d0
  go to 1455
2080 continue
  vflash = vflash / vref
  if (vflash .eq. 0.0d0) vflash = -100.0d0
  write (unit = lunit(6), fmt = 2240)
2240 format (//, '  **********   80-column card-image listing of unit-7 punched cards.   *******')
  write (unit = lunit(6), fmt = 2243)
2243 format (1x, '--------------------------------------------------------------------------------')
  write (unit = lunit(6), fmt = 2245) (ip, ip = 1, 8)
  n4 = 0
  write (unit = lunit(6), fmt = 2245) (n4, ip = 1, 8)
2245 format (1x, 8i10)
  write (unit = lunit(6), fmt = 2243)
  if (a5 .eq. 0.0d0) go to 2260
  write (unit = lunit(6), fmt = 2248) a2, a3, a4, text3, text4
2248 format (' c rating =', f9.2, '  v-mult =', e13.5, '  i-mult =', e13.5, 2x, 2a4)
  write (unit = lunit(7), fmt = 2252) a2, a3, a4, text3, text4
2252 format (' c rating =', f9.2, '  v-mult =', e13.5, '  i-mult =', e13.5, 2x, 2a4)
  go to 2320
2260 write (unit = lunit(6), fmt = 2248) a2, a3, a4, text1, text2
  write (unit = lunit(7), fmt = 2252) a2, a3, a4, text1, text2
2320 continue
  nkc = 2
  do k = 1, iphase
     if (k .gt. 1) go to 2422
     write (unit = lunit(6), fmt = 2262) text92, textna(1), textna(2), text55
2262 format (1x, a2, 2a6, 25x, a5)
     write (unit = lunit(7), fmt = 2263) text92, textna(1), textna(2), text55
2263 format (a2, 2a6, 25x, a5)
     write (unit = lunit(6), fmt = 2341)
2341 format (1x, 'c', 12x, '  vreference', 15x, 'vflashover')
     write (unit = lunit(6), fmt = 2381) vref, vflash
2381 format (1x, 2e25.15)
     write (unit = lunit(7), fmt = 2343)
2343 format ('c', 12x, '  vreference', 15x, 'vflashover')
     write (unit = lunit(7), fmt = 2382) vref, vflash
2382 format (2e25.15)
     j = 0
     write (unit = lunit(6), fmt = 2340)
2340 format (1x, 'c', 12x, '  multiplier', 17x, 'exponent', 21x, 'vmin')
     write (unit = lunit(7), fmt = 2344)
2344 format ('c', 12x, '  multiplier', 17x, 'exponent', 21x, 'vmin')
     do ij = 1, jkp
        ju = j + 1
        jk = j + nd(ij)
        do i = ju, jk
           bv = 10.0d0 ** vs(i)
           bk = 10.0d0 ** bg(i)
           write (unit = lunit(6), fmt = 2347) bk, s(i), bv
2347       format (1x, 3e25.15)
           write (unit = lunit(7), fmt = 2360) bk, s(i), bv
2360       format (3e25.15)
        end do
        write (unit = lunit(6), fmt = 2383)
2383    format (1x, 21x, '9999')
        write (unit = lunit(7), fmt = 2400)
2400    format (21x, '9999')
        j = jk
     end do
     cycle
2422 continue
     write (unit = lunit(6), fmt = 2264) text92, textna(nkc + 1), textna(nkc + 2), textna(1), textna(2), text55
2264 format (1x, a2, 4a6, 13x, a5)
     write (unit = lunit(7), fmt = 2265) text92, textna(nkc + 1), textna(nkc + 2), textna(1), textna(2), text55
2265 format (a2, 4a6, 13x, a5)
     nkc = nkc + 2
  end do
  write (unit = lunit(6), fmt = 2243)
  write (unit = lunit(6), fmt = 2447)
2447 format (/, 1x)
  go to 1200
9200 kill = 1000
9900 return
end subroutine arrdat

!
! subroutine zinold.
!

subroutine zinold
  use strcom
  use blkcom
  implicit none
  !     this module is used to convert old  ("m36." or earlier vintage )
  !     zinc oxide arrester data into the new form as required for the
  !     m37 (and later emtp versions).
  !     this module uses the ansi 77 fortran features and may, therefore,
  !     not run on systems that do not have those capabilities.
  !     it is assumed that the data case to be processed is a valid one,
  !     i.e., it did execute correctly on the older program versions
  character(80) :: cbuff, bbuff(35)
  character(30) :: cblank
  character(8) :: text(14)
  integer(4) :: iarest, iblank, icond, ik, iphase
  integer(4) :: k, k1, k2, ki
  integer(4) :: l
  integer(4) :: n19, noup
  real(8) :: vflash, vref, vref1, vstart, vzero
  real(8) :: zk(20)
  !  dimension zk(20)
  !
  write (unit = lunit(6), fmt = 3408)
3408 format (' Begin execution of "zinold", to convert from old to new Zno formats.')
  iarest = 0
  noup = noutpr
  noutpr = 1
  cblank = '                              '
  !     process branch cards ********************************************
  !     read input card using cimage
100 call cimage
  read (unit = abuff, fmt = 3417) text
3417 format (13a6, a2)
  write (unit = cbuff, fmt = 3417) text
3450 format (i2, 24x, e6.0)
  if (cbuff(1 : 2) .ne. '92') go to 150
  if (cbuff(28 : 32) .ne. '5555.' .and. cbuff(27 : 31) .ne. '5555.') go to 150
  cbuff(27 : 32) = '      '
  cbuff(40 : 44) = '5555.'
  cbuff(15 : 20) = '      '
  cbuff(21 : 26) = '      '
3418 format (a80)
  iarest = iarest + 1
  bbuff(iarest) = cbuff
  go to 100
150 if (toLower (cbuff(1 : 6)) .eq. 'blank ' .or. cbuff(1 : 30) .eq. cblank) then
     if (iarest .gt. 0) go to 200
  else
     go to  100
  end if
  if (iarest .gt. 0) go to 200
  stop
  !     process switch data cards ***************************************
  !     read cards using cimage
200 call cimage
  read (unit = abuff, fmt = 3417) text
  write (unit = cbuff, fmt = 3417) text
  if (toLower (cbuff(1 : 6)) .eq. 'blank ' .or. cbuff(1 : 30) .eq. cblank) go to 300
  go to 200
  !     process source cards ********************************************
  !     read cards using cimage
300 call cimage
  read (unit = abuff, fmt = 3417) text
  write (unit = cbuff, fmt = 3417) text
  if (cbuff(1 : 1) .eq. '5') then
     !     read cards using cimage
340  call cimage
     read (unit = abuff, fmt = 3417) text
     write (unit = cbuff, fmt = 3417) text
     if (cbuff(3 : 8) .eq. 'finish') go to 300
     go to 340
  else if (toLower (cbuff(1 : 6)) .eq. 'blank ' .or. cbuff(1 : 30) .eq. cblank) then
     go to 400
  else if (cbuff(1 : 2) .eq. '19') then
     iblank = 0
     !     read cards using cimage
350  call cimage
     read (unit = abuff, fmt = 3417) text
     write (unit = cbuff, fmt = 3417) text
     if (toLower (cbuff(1 : 6)) .ne. 'blank ' .and. cbuff(1 : 30) .ne. cblank) go to 350
     iblank = iblank + 1
     if (iblank .eq. 1) go to 350
  end if
  go to 300
400 continue
  !     remove initial conditions, if any *******************************
  !     read cards using cimage
  call cimage
  read (unit = abuff, fmt = 3420) icond
3420 format (i2)
  if (icond .gt. 1 .and. icond .lt. 6) go to 400
  ik = 0
  go to 510
  !     process arrester data *******************************************
  !     read cards using cimage
500 call cimage
510 continue
  read (unit = abuff, fmt = 3421) iphase, (zk(k), k = 1, 5)
3421 format (i8, 4e16.0, e8.0)
  ik = ik + 1
  write (unit = lunit(7), fmt = 3418) bbuff(ik)
  if (iphase .gt. 0) go to 530
  vref = zk(5)
  if (vref .eq. 0.0d0) vref = vref1
  vref1 = vref
  vflash = -100.0d0
  vzero = zk(4)
  write (unit = lunit(7), fmt = 3422) vref, vflash, vzero
3422 format (3e25.15)
  write (unit = lunit(6), fmt = 3423) vref, vflash, vzero
3423 format (1x, 3e25.15)
  write (unit = lunit(7), fmt = 3422) (zk(l), l = 1, 3)
  write (unit = lunit(6), fmt = 3423)   ( zk( l ), l = 1, 3 )
  write (unit = lunit(7), fmt = 3424)
3424 format (21x, '9999')
  write (unit = lunit(6), fmt = 3425)
3425 format (1x, 21x, '9999')
  go to 590
530 k2 = 4
  do k = 1, 3
     k1 = k2 + 1
     k2 = k2 + 4
     !     read cards using cimage
     call cimage
     read (unit = abuff, fmt = 3421) iphase, (zk(ki), ki = k1, k2)
     if (k .eq. 1 .or. k .eq. 3) go to 555
     go to 560
555  call cimage
     read (unit = abuff, fmt = 3426) n19
3426 format(i16)
560 end do
  !     read cards using cimage
  call cimage
  read (unit = abuff, fmt = 3427) vref
3427 format (e16.0)
  vref = vref * 2.0d0
  vflash = zk(16) * vref
  if (vflash .gt. (1.5d0 * vref)) vflash = -100.0d0
  if (vflash .eq. 0.0d0) vflash = -100.0d0
  if (zk(11) .eq. zk(3) .and. zk(7) .eq. zk(15)) vflash = -100.0d0
  vzero = zk(4)
  write (unit = lunit(7), fmt = 3422) vref, vflash, vzero
  write (unit = lunit(6), fmt = 3423) vref, vflash, vzero
  write (unit = lunit(7), fmt = 3422) (zk(l), l = 1, 3)
  write (unit = lunit(6), fmt = 3423) (zk(l), l = 1, 3)
  if (zk(7) .eq. vstart) go to 570
  write (unit = lunit(7), fmt = 3422) (zk(l), l = 5, 7)
  write (unit = lunit(6), fmt = 3423) (zk(l), l = 5, 7)
570 write (unit = lunit(7), fmt = 3424)
  write (unit = lunit(6), fmt = 3425)
  !     check whether gapless arrester **********************************
  if (vflash .lt. 0.0d0) go to 590
  write (unit = lunit(7), fmt = 3422) (zk(l), l = 9, 11)
  write (unit = lunit(6), fmt = 3423) (zk(l), l = 9, 11)
  if (zk(11) .eq. zk(15)) go to 580
  write (unit = lunit(7), fmt = 3422) (zk(l), l = 13, 15)
  write (unit = lunit(6), fmt = 3423) (zk(l), l = 13, 15)
580 write (unit = lunit(7), fmt = 3424)
  write (unit = lunit(6), fmt = 3425)
590 iarest = iarest - 1
  if (iarest .gt. 0) go to 500
  !     check for 'begin new data case' *********************************
  !     read cards using cimage
600 call cimage
  read (unit = abuff, fmt = 3417) text
  write (unit = cbuff, fmt = 3417) text
  if (cbuff(1 : 7) .eq. 'begin n') go to 620
  go to 600
620 noutpr = noup
  return
end subroutine zinold

!
! end of file over42.f90
!
