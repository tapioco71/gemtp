!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over41.f90
!

module zprint
  implicit none
  !  common /zprint/ zoutr(120), zoutx(120)
  real(8) :: zoutr(120), zoutx(120)
end module zprint

module newt1
  implicit none
  !  common /newt1/ rwin(10), zhl, zht, zlt, k, m, idelt, logsix
  integer(4) :: idelt
  integer(4) :: k
  integer(4) :: logsix
  integer(4) :: m
  real(8) :: rwin(10)
  real(8) :: zhl, zht, zlt
end module newt1

module over41mod
  implicit none

contains

  !
  ! subroutine newton.
  !

  subroutine newton (x, maxit, err)
    use newt1
    use blkcom
    use tracom
    implicit none
    !  common /newt1/ rwin(10),zhl,zht,zlt,k,m,idelt, logsix
    !  dimension x(1)
    real(8), intent(out) :: x(:)
    integer(4), intent(out) :: err
    integer(4), intent(in) :: maxit
    integer(4) :: i
    integer(4) :: kcode
    real(8) :: a, a1, a2, a3, a4
    real(8) :: b
    real(8) :: c, df, dx
    real(8) :: f
    real(8) :: rtot
    real(8) :: zdif
    !
    i = 0
2   i = i + 1
    if (i .ge. maxit) go to 10
    a4 = rwin(k)
    a3 = x(1)
    a2 = rwin(m)
    a1 = rwin(idelt)
    rtot = a2 + a1
    zdif = zlt - zht
    a = a3 ** 2 - 2.0d0 * zht * a3 + a4 * rtot + a2 * a1 - zht * zdif
    b = 2.0d0 * a1 * a3 + a4 * zlt + a1 * zdif + a2 * zht
    c = zhl * zhl * (rtot ** 2 + zlt ** 2)
    f = a ** 2 + b ** 2 - c
    df = 2.0d0 * a * (2.0d0 * x(1) - 2.0d0 * zht) + 2.0d0 * 2.0d0 * a1 * b
    dx = -f / df
    if (absz (dx) .le. err) go to 1
    x(1) = a3 + dx
    go to 2
10  kcode = 5
    write (unit = lunit6, fmt = 12) k, m, kcode
12  format (' Modification of zero sequence short-circuit test between', i3, ' and', i3, ' not possible. Error code =', i3)
    call stoptp
1   return
  end subroutine newton

  !
  ! subroutine cxred1.
  !

  subroutine cxred1 (as, cs, n, m)
    implicit none
    !)    for an explanation  as to the calling sequence of this module,
    !)    refer to the same subroutine of overlay 3.
    !     Define double precision for single precision UNIVAC
    integer(4), intent(in) :: n, m
    double precision, intent(out) :: as(:), cs(:)
    integer(4) :: i, i1, i2, ij, ik
    integer(4) :: j
    integer(4) :: k
    integer(4) :: l
    integer(4) :: nnn
    real(8) :: a10
    real(8) :: w
    double precision :: a(100), b(100), c(100), d(100)
    double precision :: g1, g2, h1, h2, x, y
    !  dimension  as(1), cs(1)
    nnn = n * (n + 1) / 2
    do j = 1, nnn
       a(j) = as(j)
50     c(j) = cs(j)
    end do
    j = n + 1
    w = 1.0d0
    if (m .gt. 0) w = -w
    ij = n * j / 2
3   j = j - 1
    if (j .eq. m) go to 60
    h1 = a(ij)
    g1 = c(ij)
    x = 1.0d0 / (h1 * h1 + g1 * g1)
    h1 = -h1 * x
    g1 = g1 * x
    b(j) = h1
    d(j) = g1
    ij = ij - j
    k = 0
    ik = 0
    !                                   begin k-loop
4   ik = ik + k
    i1 = ik + 1
    k = k + 1
    if (k .gt. n) go to 3
    if (k .lt. j) go to 9
    if (w .lt. 0.0d0) go to 3
    if (k .eq. j) go to 7
    i = ik + j
5   h2 = a(i)
    g2 = c(i)
    b(k) = h2 * h1 - g2 * g1
    d(k) = h2 * g1 + g2 * h1
    !                                   begin i-loop
    i2 = ik + k
    l = 0
    do i = i1, i2
       l = l + 1
       x = b(l)
       y = d(l)
       a(i) = a(i) + x * h2 - y * g2
6      c(i) = c(i) + x * g2 + y * h2
    end do
    if (k .lt. j) go to 4
    i = ik + j
    a(i) = b(k)
    c(i) = d(k)
    go to 4
    !                                   end i-loop
7   i = ij
    do l = 1, j
       i = i + 1
       c(i) = d(l)
8      a(i) = b(l)
    end do
    go to 4
    !                                   end k-loop
9   i = ij + k
    go to 5
    !     nnn = n * (n+1) / 2 ! defined at top, so unneded here. wsm
60  do j = 1, nnn
       as(j) = a(j)
70     cs(j) = c(j)
    end do
    return
  end subroutine cxred1

end module over41mod

!
! subroutine over41.
!

subroutine over41
  use over41mod
  use blkcom
  use tracom
  use strcom
  implicit none
  character(8) :: text1(3), text2(3), text3(3), t1, t2, t3
  character(8) :: text6, text7(3), text8(3), text9
  character(8) :: text10, text11, text12, typec(20)
  integer(4) :: i, ichck, iwind
  integer(4) :: j
  integer(4) :: k, ki, kk
  integer(4) :: ll0, ll2, ll3
  integer(4) :: m
  integer(4) :: n, n1, n4
  real(8) :: ai, ar
  real(8) :: cii, cmag, cr, cur1
  real(8) :: d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14
  real(8) :: henry(20)
  real(8) :: ohm(20)
  real(8) :: ploss, pmag, pratg
  real(8) :: resist, rik(3), rr(6), rsum
  real(8) :: temp1(3), temp2(3), temp3(3)
  real(8) :: v(3), v1, v2, volt(20), volti(20), voltk(20), vtap
  real(8) :: xik(3), xsum, xx(6)
  real(8) :: z
  !
  data t1 / 'high  ' /
  data t2 / 'medium' /
  data t3 / 'low   ' /
  data text9  / 'branch' /
  data text10 / '0     ' /
  data text11 / '1     ' /
  data text12 / 'transf' /
  data typec(1)  / '51    ' /
  data typec(2)  / '52    ' /
  data typec(3)  / '53    ' /
  data typec(4)  / '54    ' /
  data typec(5)  / '55    ' /
  data typec(6)  / '56    ' /
  data typec(7)  / '57    ' /
  data typec(8)  / '58    ' /
  data typec(9)  / '59    ' /
  data typec(10) / '60    ' /
  data typec(11) / '61    ' /
  data typec(12) / '62    ' /
  data typec(13) / '63    ' /
  data typec(14) / '64    ' /
  data typec(15) / '65    ' /
  data typec(16) / '66    ' /
  data typec(17) / '67    ' /
  data typec(18) / '68    ' /
  data typec(19) / '69    ' /
  data typec(20) / '70    ' /
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4567)
4567 format ( '  "Begin module over41."')
  if ( iofbnd .ne. 33666 ) go to 4574
  call crdchg
  go to 9200
4574 text1(1) = t1
  text2(1) = t1
  text2(2) = t1
  text1(2) = t2
  text2(3) = t2
  text3(1) = t2
  text1(3) = t3
  text3(2) = t3
  text3(3) = t3
  ll0 = 0
  ll2 = 2
  ll3 = 3
  read (unit = abuff, fmt = 7108) statfr, xopt
7108 format (32x, 2e8.0)
  if (statfr .ne. 44.0d0) go to 7119
  !     Enter hermann's ubc 3-phase transformer routine  "bctran"
  call bctran
  if (kill .gt. 0) go to 9200
  go to 3872
7119 omega = twopi * statfr
  if (statfr .gt. 0.0d0) go to 7122
5 do i = 1, 3
     text7(i) = blank
3854 text8(i) = blank
  end do
  !     Read input card using cimage
3857 call cimage
  n4 = kolbeg
  if (kolbeg .gt. 0) go to 1723
  read (unit = abuff, fmt = 3861) bus1
3861 format (a6, 2x, 12a6)
  go to 1725
1723 nfrfld = 1
  nright = -1
  call freone (d1)
  nright = 0
  bus1 = texta6(1)
1725 if (bus1 .ne. text9) go to 3869
  write (unit = kunit6, fmt = 3864)
3864 format ('+Node names for punched branch cards.')
  if (kolbeg .gt. 0) go to 1733
  read (unit = abuff, fmt = 3861) bus1, (text7(i), text8(i), i = 1, 3)
  go to 1735
1733 nfrfld = 6
  nright = -1
  call freone ( d1 )
  nright = 0
  if (iprsup  .ge.  1) write (unit = lunit6, fmt = 4568)
4568 format ('  "Exit  module over41."')
  if (kill .gt. 0) go to 99999
  text7(1) = texta6(1)
  text8(1) = texta6(2)
  text7(2) = texta6(3)
  text8(2) = texta6(4)
  text7(3) = texta6(5)
  text8(3) = texta6(6)
1735 go to 3857
3869 kolbeg = n4
  if (kolbeg .gt. 0) go to 1743
  read (unit = abuff, fmt = 6) iwind, cmag, pmag, ipunch
6 format (i1, e9.0, e10.0, i10)
  go to 1745
1743 nfrfld = 1
  call freone (d11)
  iwind = d11
  call freone (cmag)
  call freone (pmag)
  call freone (d11)
  ipunch = d11
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
  if (kill .gt. 0) go to 99999
1745 if (iwind .eq. 2) go to 7
  if (iwind .eq. 3) go to 7
3870 write (unit = kunit6, fmt = 3871)
3871 format ("+Blank card terminating  'xformer'  cases.")
  call interp
  call interp
3872 lastov = nchain
  nchain = 51
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
  go to 99999
7 write (unit = kunit6, fmt = 3892) iwind, cmag, pmag, ipunch
3892 format ('+New device.', i6, 2e12.3, i7)
  if (iwind .eq. 2) go to 500
  text1(2) = t2
  rsum = 0.0d0
  xsum = 0.0d0
  do i = 1, 3
     call cimage
     !     Read input card using cimage
     if (kolbeg .gt. 0) go to 1753
     read (unit = abuff, fmt = 10) vtap, ploss, z, pratg
10   format (5e10.0)
     go to 1755
1753 nfrfld = 1
     call freone (vtap)
     call freone (ploss)
     call freone (z)
     call freone (pratg)
     if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
     if (kill .gt. 0) go to 99999
1755 write (unit = kunit6, fmt = 3904) vtap, ploss, z, pratg
3904 format ('+Wind.', 4e11.3)
     call interp
     temp1(i) = ploss
     temp2(i) = z
     temp3(i) = pratg
     resist = ploss / (pratg * 1000.0d0)
     v(i) = vtap
     rik(i) = resist / pratg
     xik(i) = sqrtz (z ** 2 / 10000.0d0 - resist ** 2) / pratg
     if (z .lt. 0.0d0) xik(i) = -xik(i)
     rsum = rsum + rik(i)
30   xsum = xsum + xik(i)
  end do
  write (unit = lunit6, fmt = 15) iwind, cmag, pmag
15 format (/, ' Single-phase', i2, "-winding transformer.   'imagn' = ", f8.5, ' per cent based on ', f8.3, ' MVA', /, ' voltage across winding ', 15x, 'losses  impedance based on ', /, 9x, '(kV)', 26x, '(kW)   (per cent) (MVA)')
  do i = 1, 3
3907 write (unit = lunit6, fmt = 20) text1(i), v(i), text2(i), text3(i), temp1(i), temp2(i), temp3(i)
  end do
20 format (1x, a6, f7.2, 6x, a6, ' to ', a6, f8.2, f10.4, f10.3)
  cmag = cmag * pmag / 300.0d0
  rsum = rsum / 2.0d0
  xsum = xsum / 2.0d0
  ar = rsum - rik(1)
  ai = xsum - xik(1)
  cr = rik(2) * rik(3) - xik(2) * xik(3) - ar ** 2 + ai ** 2
  cii = rik(2) * xik(3) + xik(2) * rik(3) - 2.0d0 * ar * ai
  ai = cr ** 2 + cii ** 2
  ar = cr / ai
  ai = -cii / ai
  ki = 0
  i = 0
  j = 0
40 i = i + 1
  if (i .gt. 3) go to 70
  k = 0
50 k = k + 1
  ki = ki + 1
  vtap = v(i) * v(k)
  if (k .eq. i) go to 60
  j = j + 1
  cr = (rik(j) - rsum) / vtap
  cii = (xik(j) - xsum) / vtap
  rr(ki) = cii * ai - cr * ar
  xx(ki) = -cii * ar - cr * ai
  go to 50
60 n = 4 - i
  cr = rik(n) / vtap
  cii = xik(n) / vtap
  rr(ki) = cii * ai - cr * ar
  cr = -cii * ar - cr * ai
  cii = cmag / vtap
  if (cii .gt. absz (cr) * epsiln) go to 65
62 write (unit = lunit6, fmt = 61) epsiln, cii, cr
61 format (/, ' Problem.  ----  admittance matrix for transformer is too close to being singular to be invertible.   Hence', /, 17x, 'the inversion will be skipped.   Parameters which document this breakdown follow.', /, 20x, 9x, 'epsiln', 12x,  'cii',  13x, 'cr' , /, 20x, 3e15.6)
  go to 600
65 xx(ki) = cr + cii
  go to 40
70 call cxred1 (rr(1 :), xx(1 :), ll3, ll0)
75 write (unit = lunit6, fmt = 90)
90 format (/, " Impedance matrix as required for EMTP studies (with  'X'  in Ohms at the power frequency)", /, 3(14x, 'R', 14x, 'X'))
  i = 0
  n = 1
95 i = i + 1
  if (i .gt. iwind) go to 110
  ki = n + i - 1
  write (unit = lunit6, fmt = 100) text1(i), (rr(k), xx(k), k = n, ki)
100 format(' ', a6, 6e15.7)
  n = n + i
  go to 95
110 write (unit = lunit6, fmt = 113)
113 format (/, 1x)
120 format (' Warning. ----- If nothing is connected to windings which are delta-connected, either add capacitances or ground', /, 16x, 'one terminal of the delta.   Otherwise the delta connection is floating, with voltage indeterminate.')
  if (ipunch .gt. 0) go to 4273
  write (unit = lunit6, fmt = 4201) (i, i = 1, 8)
4201 format (//, ' 80-column card-image listing of punched-card output follows (type-51-53 EMTP branch cards).', /, 1x, 80('-'), /, 1x, 8(9x, i1), /, 1x, 8(9x, '0'), /, 1x, 80('-'))
  do i = 1, iwind
     if (i .gt. 1) go to 4228
     write (unit = lunit7, fmt = 4219) text7(i), text8(i), rr(1), xx(1)
4219 format ('51,', a6, ',', a6, ',  ,  ,   ', 2(e22.13, ' ,'), 1x, ',,,, ')
     write (unit = lunit6, fmt = 4220) text7(i), text8(i), rr(1), xx(1)
4220 format (1x, '51,', a6, ',', a6, ',  ,  ,   ', 2(e22.13, ' ,'), 1x, ',,,, ')
     go to 4270
4228 if (i .gt. 2) go to 4238
     write (unit = lunit7, fmt = 4229) text7(i), text8(i), rr(2), xx(2), rr(3), xx(3)
4229 format ('52,', a6, ',', a6, ',  ,  ,   ', e22.13, ' ,', e22.13, ' $', /, 26x, 2(e22.13, ' ,'), 1x, ',,,, ')
     write (unit = lunit6, fmt = 4230) text7(i), text8(i), rr(2), xx(2), rr(3), xx(3)
4230 format (1x, '52,', a6, ',', a6, ',  ,  ,   ', e22.13, ' ,', e22.13, ' $', /, 27x, 2(e22.13, ' ,'), 1x, ',,,, ')
     go to 4270
4238 write (unit = lunit7, fmt = 4239) text7(i), text8(i), (rr(j), xx(j), j = 4, 6)
4239 format ('53,', a6, ',', a6, ',  ,  ,   ', e22.13, ' ,', e22.13, ' $', /, 26x, e22.13, ' ,', e22.13, ' $', /, 26x, 2(e22.13, ' ,'), 1x, ',,,, ')
     write (unit = lunit6, fmt = 4240) text7(i), text8(i), ( rr(j), xx(j), j = 4, 6)
4240 format (1x, '53,', a6, ',', a6, ',  ,  ,   ', e22.13, ' ,', e22.13, ' $', /, 27x, e22.13, ' ,', e22.13, ' $', /, 27x, 2(e22.13, ' ,'), 1x, ',,,, ')
  end do
4270 continue
  write (unit = lunit6, fmt = 4271)
4271 format (1x, 80('-'), //, 1x)
4273 ichck = 0
  write (unit = lunit6, fmt = 130)
130 format (/, ' Short-circuit input impedances which are obtained from the just-printed impedance matrix, by reverse', /, ' computation.   This is sort of a check on the computation.')
135 ar = rr(2) ** 2 - xx(2) ** 2
  ai = 2.0d0 * rr(2) * xx(2)
  cr = rr(3) ** 2 + xx(3) ** 2
  rsum = rr(3) / cr
  xsum = -xx(3) / cr
  cr = rr(1) - ar * rsum + ai * xsum
  cii = xx(1) - ar * xsum - ai * rsum
  if (iwind .eq. 2) go to 145
  write (unit = lunit6, fmt = 140) text2(1), text3(1), cr, cii
140 format (1x, a6, ' to ', a6, 2f15.5)
  cr = rr(6) ** 2 + xx(6) ** 2
  rsum = rr(6) / cr
  xsum = -xx(6) / cr
  ar = rr(4) ** 2 - xx(4) ** 2
  ai = 2.0d0 * rr(4) * xx(4)
  cr = rr(1) - ar * rsum + ai * xsum
  cii = xx(1) - ar * xsum - ai * rsum
  write (unit = lunit6, fmt = 140) text2(2), text3(2), cr, cii
  ar = rr(5) ** 2 - xx(5) ** 2
  ai = 2.0d0 * rr(5) * xx(5)
  cr = rr(3) - ar * rsum + ai * xsum
  cii = xx(3) - ar * xsum - ai * rsum
  write (unit = lunit6, fmt = 140) text2(3), text3(3), cr, cii
  go to 146
145 write (unit = lunit6, fmt = 140) text1(1), text1(3), cr, cii
146 if (ichck .gt. 0) go to 600
  j = (iwind - 1) * 3
  do i = 1, j
     if (xx(i) .lt. 0.0d0) write (unit = lunit6, fmt = 155)
155  format (' Input data looks suspicious, because one or more inductances is negative.')
     call round (rr(i))
150  call round (xx(i))
  end do
  write (unit = lunit6, fmt = 160)
160 format (' Repeat of preceding calculation, only this time the starting point will be the impedance matrix with all', /, ' elements rounded to five decimal digits.')
  ichck = 1
  go to 135
  !     Read input card using cimage
500 call cimage
  if (kolbeg .gt. 0) go to 1763
  read (unit = abuff, fmt = 10) v1, v2, ploss, z, pratg
  go to 1765
1763 nfrfld = 1
  call freone (v1)
  call freone (v2)
  call freone (ploss)
  call freone (z)
  call freone (pratg)
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
  if (kill .gt. 0) go to 99999
1765 write (unit = kunit6, fmt = 3904) v1, v2, ploss, z
  call interp
  write (unit = lunit6, fmt = 15) iwind, cmag, pmag
  write (unit = lunit6, fmt = 20) text1(1), v1, text1(1), text1(3), ploss, z, pratg
  text1(2) = t3
  cmag = cmag  * pmag / 200.0d0
  rsum = ploss / (pratg * 1000.0d0)
  xsum = sqrtz (z ** 2 / 10000.0d0 - rsum ** 2)
  if (z .lt. 0.0d0) xsum = -xsum
  cii = (rsum ** 2 + xsum ** 2) / pratg
  ar = rsum / cii
  ai = -xsum / cii
  if (cmag .gt. absz (ai) * epsiln) go to 3926
  cii = cmag
  cr = ai
  go to 62
3926 vtap = v1 ** 2
  rr(1) = -ar / vtap
  xx(1) = (cmag - ai) / vtap
  vtap = v1 * v2
  rr(2) = ar / vtap
  xx(2) = ai / vtap
  vtap = v2 ** 2
  rr(3) = -ar / vtap
  xx(3) = (cmag - ai) / vtap
  call cxred1 (rr(1 :), xx(1 :), ll2, ll0)
  go to 75
600 write (unit = lunit6, fmt = 601)
601 format (/, 1x)
  go to 5
  !     Begin code for the conversion of saturable  'transformer'
  !     data into  (r)  and  (l)  specification (type 51, 52, .. ).
  !     Let  'Zk'  denote the complex (phasor) leakage impedance for
  !     winding  'k' ,   and let  'zmag'  denote the complex
  !     magnetizing impedance.   Further, let  'tk'  be the relative
  !     number of turns for winding  'k' ,   and let  (Z)  be the
  !     desired output matrix, where    (Z) = (R) + jw(L)    at
  !     sinusoidal frequence  'w' .   Then the relevant conversion
  !     formulas are  .....
  !          Z(k,k)  =  Zk  +  Zmag * (tk/t1)**2
  !          Z(k,j)  =  Zmag * tk * tj / t1**2      (for  j .ne. k )
  !     read input card using cimage
7122 call cimage
7123 continue
  read (unit = abuff, fmt = 7129) bus5, d1, d2, d3
7129 format (2x, a6, 18x, 2e6.2, 6x, e6.2)
  if (bus5 .eq. text12) go to 7132
  lstat(19) = 7132
  lstat(14) = 1
  go to 9000
7132 write (unit = kunit6, fmt = 7134) d1, d2, d3
7134 format ('+Sat. xformer.  ', 3e11.3)
  d4 = d2 / d1
  d5 = d4 * omega
  do i = 1, 99
     !     Read input card using cimage
     call cimage
     read (unit = abuff, fmt = 7142) bus5, bus6
7142 format (2a1)
     if (to_lower (bus6) .ne. text11) go to 7151
     if (to_lower (bus5) .eq. text10) go to 7157
     if (bus5 .eq. blank) go to 7157
7151 write (unit = kunit6, fmt = 7152)
  end do
7152 format ('+Ignore characteristic in search for windings.')
  lstat(19) = 7152
  lstat(14) = 2
  go to 9000
7157 do i = 1, 20
     if (i .eq. 1) go to 7161
     !     Read input card using cimage
     call cimage
7161 continue
     read (unit = abuff, fmt = 7163) kk, aupper(i), alower(i), ohm(i), henry(i), volt(i)
7163 format (i2, 2a6, 12x, 3e6.2)
     if (kk .ne. i) go to 7175
     iwind = kk
     write (unit = kunit6, fmt = 7171) i, ohm(i), henry(i), volt(i)
7171 format ('+  Winding', i2, '. ', 3e12.4)
  end do
7172 continue
  lstat(19) = 7175
  lstat(14) = 3
  go to 9000
7175 write (unit = kunit6, fmt = 7178)
7178 format ('+Data card for following transformer, if any.')
  write (unit = lunit6, fmt = 7182) iwind, statfr, xopt, d4, d5
7182 format (//,' Saturable transformer input data is now complete. Relevant scalar parameters may be summarized as follows ...' , /, 1x, i19, '  =  number of transformer windings ', /, e20.6, '  =  power frequency in Hz (equivalence will be exact at this frequency)', /, e20.6, "  =  'xopt'  (EMTP inductance/reactance specification parameter)", /, e20.8, '  =  assumed linear magnetizing inductance in H', /, e20.8, '  =  magnetizing reactance at the power frequency in Ohms')
  if (d3 .gt. 0.0d0) go to 7206
  d9 = 0.0d0
  d10 = d5
  go to 7216
7206 d6 = d3 ** 2
  d7 = d5 ** 2
  d8 = d6 + d7
  d9 = d3 * d7 / d8
  d10 = d6 * d4 / d8
7216 if (xopt .eq. 0.0d0) d10 = d10 * 1000.0d0 / omega
  if (xopt .gt. 0.0d0) d10 = d10 * xopt / statfr
  write (unit = lunit6, fmt = 4201) (i, i = 1, 8)
  do i = 1, iwind
     n1 = i - 1
     d11 = volt(i) / volt(1)
     text6 = typec(i)
     bus1 = aupper(i)
     bus2 = alower(i)
     if (n1 .le. 0) go to 7253
     do j = 1, n1
        d12 = d11 * volt(j) / volt(1)
        d13 = d12 * d9
        d14 = d12 * d10
        write (unit = lunit7, fmt = 7231) text6, bus1, bus2, d13, csepar, d14, chcont
7231    format (a2,  2a6,  12x,  2(e22.13, 1x, a1),  1x,  a1,  6x)
        write (unit = lunit6, fmt = 7234) text6, bus1, bus2, d13, csepar, d14, chcont
7234    format (1x,  a2,  2a6,  12x,  2(e22.13, 1x, a1),  1x,  a1,  6x)
        bus1 = blank
        bus2 = blank
7245    text6 = blank
     end do
7253 d12 = d11 ** 2
     volti(i) = d12 * d9
     voltk(i) = d12 * d10
     d13 = ohm(i) + volti(i)
     d14 = henry(i) + voltk(i)
     write (unit = lunit7, fmt = 7262) text6, bus1, bus2, d13, csepar, d14, (csepar, m = 1, 5)
7262 format (a2, 2a6, 12x, 2(e22.13, 1x, a1), 1x, 5a1, 2x)
     write (unit = lunit6, fmt = 7265) text6, bus1, bus2, d13, csepar, d14, (csepar, m = 1, 5)
7265 format (1x, a2, 2a6, 12x, 2(e22.13, 1x, a1), 1x, 5a1, 2x)
  end do
7274 continue
  write (unit = lunit6, fmt = 4271)
  write (unit = lunit6, fmt = 7277) (i, ohm(i), henry(i), volti(i), voltk(i), i = 1, iwind)
7277 format (' Components which are added to form diagonals of the matrix.   Computer word-length should be able to accurately', /, ' handle this addition without the leakage being lost in the roundoff.   If not, the resulting  (r)  and  (l)  are of', /, ' questionable value for purposes of EMTP simulation.   Column 1 is to be added to column 3,   and column 2  is', /, ' to be added to column 4 to produce the diagonals.', /, 7x, 'row', 11x, 'leakage R', 6x, 'leakage L or X', 12x, 'magnetizing R', 6x, 'magnet. L or X', 16x, /, (i10, 2e20.10, 5x, 2e20.10))
  read (unit = abuff, fmt = 7284) (aupper(i), i = 1, 14)
7284 format (13a6, a2)
  write (unit = lunit6, fmt = 7288)  ( aupper(i), i=1, 14 )
7288 format (///, ' Another saturable transformer has now been successfully converted to  (r), (l)  format.   that computation', /, ' is now complete.  The EMTP now begins to process the following case (if any) by re-listing the last-read data', /, ' card (which did not belong to the last case, recall).', /, 1x, 131('-'), /, '+', 50x, '1', /, 51x, '1', 13a6, a2)
  read (unit = abuff, fmt = 7281) bus5
7281 format (2x, a6)
  if (bus5 .eq. text12) go to 7123
  if (bus5 .eq. blank) go to 3870
  lstat(14) = 1
  lstat(19) = 7288
9000 kill = 158
9200 lstat(18) = nchain
  lastov = nchain
  nchain = 51
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
99999 return
end subroutine over41

!
! subroutine crdchg.
!

subroutine crdchg
  use blkcom
  use dekspy
  implicit none
  !
  !     Fortran77 used to provide a subroutine to
  !     input old data cards and change their format;
  !     this includes doing any appropriate calculations.
  !     Old 91,92,93 types changed to new 91,99,98 types
  !     respectively.
  !
  character(80) :: card, blanks, work
  character(8) :: text(14)
  integer(4) :: i, icount
  integer(4) :: n6
  real(8) :: cur1, cur2
  real(8) :: d7, d8
  real(8) :: flash
  real(8) :: resist
  real(8) :: seal
  real(8) :: wl1, wl2
  real(8) :: y, ys
  !
  blanks = ' '
  work = ' '
  !
  write (unit = lunit6, fmt = 20)
20 format (' This code will read data types 91-switched resistor, 92-switched resistance, and 93-switched inductor.')
  write (unit = lunit6, fmt = 26)
26 format (" The data will be converted, formated, and written out to meet the new EMTP rule book's requirements.")
  !
  !open(unit = 7, status = 'new', err = 40)
  open (unit = 7, err = 40)
  !
  go to 50
  !
40 continue
  stop ' Error opening output file '
  !
50 continue
  !
  noutpr = 1
  do icount = 1, 9999
     !
     call cimage
     read (unit = abuff, fmt = 100) text
     write (unit = card, fmt = 100) text
100  format (13a6, a2)
     !
     !     *** case '91' ***
     !
     if (card(1 : 2) .eq. '91') then
        if (card(39 : 44) .eq. ' 3333.') go to 200
        write (unit = lunit6, fmt = 625) card
625     format (' Type-91:  piece-wise linear resistor', 14x, '1', a80)
1000    continue
        write (unit = 7, fmt = 1175) card(1 : 78)
1175    format ('c ', a78)
        write (unit = 7, fmt = 1100) card(1 : 26), card(80 : 80)
1100    format (a26, 12x, ' 3333.', 37x, a1)
        write (unit = lunit9, fmt = 1150) card(1 : 26), card(80 : 80)
1150    format (a26, 12x, ' 3333.', 37x, a1)
        !
        work(1 : 25) = blanks(1 : 19)//card(27 : 32)
        !
        write (unit = 7, fmt = 1160)
1160    format ('c', 18x, 'vstart')
        write (unit = 7, fmt = 1200) work
1200    format (a80)
        write (unit = lunit9, fmt = 1225) work
1225    format (a80)
        !
        write (unit = 7, fmt = 1185)
1185    format ('c ', 14x, 'tr in sec.', 4x, 'R(tr) in Ohms')
        !
1250    continue
        if (card(13 : 16) .ne. '9999') then
           call cimage
           read (unit = abuff, fmt = 1400) text
           write (unit = card, fmt = 1400) text
1400       format (13a6, a2)
           work(1 : 25) = blanks(1 : 9)//card(1 : 16)
           work(26 : 50) = blanks(1 : 9)//card(17 : 32)
           !
           write (unit = 7, fmt = 1415) card(1 : 78)
1415       format ('c ', a78)
           write (unit = 7, fmt = 1413) work
1413       format (a80)
           write (unit = lunit6, fmt = 1300) card(1 : 32), work
1300       format (1x, 'Characteristics', a32, 3x, '1', a80)
           write (unit = lunit9, fmt = 1325) work
1325       format (a80)
           !
           go to 1250
        end if
     end if
     !
     if (card .eq. blanks) go to 500
     if (card(1 : 5) .eq. 'blank') go to 500
     !
  end do
200 continue
  !
500 do kcount = 1, 9999
     !
     call cimage
     read (unit = abuff, fmt = 600) text
     write (unit = card, fmt = 600) text
600  format (13a6, a2)
     if (card .eq. blanks) go to 10000
     if (card(1 : 5) .eq. 'blank') go to 10000
     !
     !     *** case '92' ***
     !
     if (card(1 : 2) .eq. '92') then
        write (unit = lunit6, fmt = 650) card
650     format (' Type-92:  switched resistance', 21x, '1', a80)
        work = ' '
        write (unit = 7, fmt = 3100) card(1 : 78)
3100    format ('c ', a78)
        !
        read (unit = card, fmt = 3110) d7, d8
3110    format (34x, 2e10.6)
        n6 = 6
        call fltopt (d7, n6)
        work(1 : 6) = ansi32(27 : 32)
        n6 = 6
        call fltopt (d8, n6)
        write (unit = 7, fmt = 3300) card(3 : 14), work(1 : 6), ansi32(27 : 32), work(13 : 79), card(80 : 80)
3300    format ('99', a12, 12x, a6, 6x, '     2', a6, a30, a1)
        !
        write (unit = 7, fmt = 3400)
3400    format ('c', 8x, 'current', 12x, 'volt', 50x)
        write (unit = 7, fmt = 3500) card(35 : 44)
3500    format (7x, '0.0000001', 6x, a10, 50x)
        write (unit = lunit9, fmt = 3525) card(3 : 14), work(1 : 6), ansi32(27 : 32), work(13 : 79), card(80 : 80)
3525    format ('99', a12, 12x, a6, 6x, '     2', a6, a30, a1)
        !
        write (unit = lunit9, fmt = 3535) card(35 : 44)
3535    format (7x, '0.0000001', 6x, a10, 50x)
        !
        read (unit = card(15 : 24), fmt = 3550) resist
3550    format (e10.6)
        read (unit = card(39 : 44), fmt = 3575) seal
3575    format (e10.6)
        flash = resist + seal
        write (unit = work(13 : 44), fmt = 3600) flash
3600    format (13x, '1.0', e16.9)
        write (unit = 7, fmt = 3650) work(13 : 80)
3650    format (a64)
        write (unit = 7, fmt = 3700)
3700    format (12x, '9999')
        write (unit = lunit9, fmt = 3725) work(13 : 80)
3725    format (a64)
        write (unit = lunit9, fmt = 3750)
3750    format (12x, '9999')
        go to 700
     end if
     !
     !     *** case '93' ***
     !
     if (card(1 : 2) .eq. '93') then
        write (unit = lunit6, fmt = 675) card
675     format (' Type-93:  switched inductance', 21x, '1', a80)
        work = ' '
        write (unit = 7, fmt = 5025) card(1 : 78)
5025    format ('c ', a78)
        !
        read (unit = card, fmt = 5100) wl1, wl2, ys
5100    format (24x, 3e10.6)
        if (xopt .eq. 0.0d0) then
           wl1 = wl1 / 1000.0d0
           wl2 = wl2 / 1000.0d0
        else
           wl1 = wl1 / (twopi * xopt)
           wl2 = wl2 / (twopi * xopt)
        end if
        n6 = 6
        call fltopt (ys, n6)
        write (unit = 7, fmt = 5250) card(3 : 14), ansi32(27 : 32), card(80 : 80)
5250    format ('98', a12, 12x, '   1.0', a6, 42x, a1)
        write (unit = lunit9, fmt = 5275) card(3 : 14), ansi32(27 : 32), card(80 : 80)
5275    format ('98', a12, 12x, '   1.0', a6, 42x, a1)
        write (unit = 7, fmt = 5150)
5150    format ('c', 8x, 'current', 12x, 'flux', 50x)
        cur1 = ys / wl1
        cur2 = cur1 * 2
        y = ys + (wl2 * (cur2 - cur1))
        write (unit = work(23 : 70), fmt = 5300) cur1, cur2, y
5300    format (3e16.7)
        write (unit = 7, fmt = 5400) work(23 : 38), card(45 : 54)
5400    format (a16, 6x, a10, 47x)
        write (unit = 7, fmt = 5450) work(39 : 70)
5450    format (a32, 47x)
        write (unit = 7, fmt = 5500)
5500    format (12x, '9999', 65x)
        write (unit = lunit9, fmt = 5600) work(23 : 38), card(45 : 54)
5600    format (a16, 6x, a10, 47x)
        write (unit = lunit9, fmt = 5700) work(39 : 70)
5700    format (a32, 47x)
        write (unit = lunit9, fmt = 5800)
5800    format (12x, '9999', 65x)
     end if
     !
  end do
700 continue
  !
10000 rewind lunit9
  write (unit = lunit6, fmt = 425)
425 format (/, 27x, 'Sequential list of punched output.', /, 81('-'), /, 1x, '         1         2         3         4         5         6         7         8', /, 1x, '12345678901234567890123456789012345678901234567890123456789012345678901234567890', /, 1x, 81('-'), /, 1x)
  do i = 1, 9999
     read(unit = lunit9, fmt = 10050, end = 10200) card
10050 format (a80)
     write (unit = lunit6, fmt = 10100) card
10100 format (1x, a80)
  end do
10150 continue
10200 stop
end subroutine crdchg

!
! subroutine fltopt.
!

subroutine fltopt (d8, n14)
  use blkcom
  use dekspy
  use tracom
  implicit none
  !       This module services         , and is designed to optimally
  !       encode the floating number  d8  into  ansi32  of  "dekspy".  By
  !       optimal, we mean a showing of the greatest precision within the
  !       minimum space.  On input,  n14  is the maximum column width, but
  !       on output, it signals the beginning column number.  That is, the
  !       character string  ansi32(n14:32)  is to be displayed.
  integer(4), intent(out) :: n14
  real(8), intent(in) :: d8
  character(1) :: onebyt
  character(5) :: ansi5
  integer(4) :: j
  integer(4) :: k
  integer(4) :: n3, n4, n5, n7, n15, n17, n18
  real(8) :: a, a1, a2, a3, aq, az
  real(8) :: b
  real(8) :: d9
  !
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' Top "fltopt".  d8, n14 =', d8, n14
  n15 = n14
  if (d8 .ne. 0.0d0) go to 4118
  ansi32(30 : 32) = '0.0'
  n14 = 30
  go to 4321
4118 d9 = absz (d8)
  if (d8 .lt. 0.0d0) n15 = n15 - 1
  n5 = 0
  if (d9 .lt. 1.e6 .and. d9 .ge. 1.e-3) go to 4169
  !       Use of e-field requires massage and removal of exponent first:
  write (unit = spycd2, fmt = 4126) d9
4126 format (e35.22)
  if (iprsup .ge. 1) write (unit = *, fmt = *)  ' E-field spycd2 before massage:', spycd2
  n7 = index (spycd2, 'e')
  if (n7 .gt. 0) go to 4133
  ansi32(26 : 32) = 'garbage'
  n14 = 26
  go to 4321
4133 ansi32 = spycd2(4 : 35)
  k = index (spycd2, 'e+')
  if (k .eq. 0) go to 4145
4138 spycd2(4 : k + 1) = ' '//ansi32(1 : k - 3)
  go to 4133
4145 k = index (spycd2, 'e0')
  if (k .ne. 0 .and. k .lt. 34) go to 4138
  k = index (spycd2, 'e-0')
  if (k .eq. 0) go to 4157
  spycd2(4 : k + 2) = ' '//ansi32(1 : k - 2)
  go to 4133
  !     we reach s.n.4157 having fully simplified e-field exponent:
4157 k = index (spycd2, 'e')
  ansi5 = spycd2(k : 35)//blan80(1 : k - 31)
  n5 = 36 - k
  n15 = n15 - n5
  ansi32 = spycd2(1 : k - 1)
  spycd2 = '   '//ansi32
  read (unit = spycd2, fmt = 4172) d9
  go to 4184
  !     Use of f-field begins with encoding of absolute value:
4169 write (unit = spycd2, fmt = 4172) d9
4172 format (f35.24)
4184 if (iprsup .ge. 1) write (unit = *, fmt = 4188) spycd2
4188 format ( ' After f35.24 encode, spycd2(a35) =', a35)
  n17 = 0
  if (d9 .lt. 1.0) n17 = index (spycd2, '.')
  n18 = 0
  do j = 1, 35
     k = 36 - j
     if (n18 .gt. 0) go to 4245
     if (spycd2(k : k) .eq. '0') go to 4252
     n18 = k
     if (n17 .gt. 0) go to 4256
     go to 4252
4245 if (spycd2(k : k) .ne. ' ') go to 4252
     n17 = k + 1
     go to 4256
4252 end do
  call stoptp
4256 n4 = n18 - n17 + 1
  if (n4 .le. n15) go to 4288
  n18 = n17 + n15 - 1
  !     Having chopped number, we must check for "0000.." or "9999..":
  onebyt = spycd2(n18 + 1 : n18 + 1)
  read (unit = onebyt, fmt = 4262) n3
4262 format (i1)
  if (n3 .ge. 5) go to 4273
  !     Enter loop for case of "0000.." (search left for non-zero):
4264 if (spycd2(n18 : n18) .ne. '0') go to 4256
  n18 = n18 - 1
  go to 4264
  !     Enter loop for case of "9999.." (move left, rounding upward):
4273 onebyt = spycd2(n18 : n18)
  if (onebyt .eq. '.') go to 4286
  read (unit = onebyt, fmt = 4262) n3
  n3 = n3 + 1
  spycd2(n18 : n18) = digit(n3)
  if (n3 .eq. 10) go to 4286
  n3 = index (spycd2, '.')
  if (n17 .gt. n18) n17 = n18
  if (n18 .lt. n3) n18 = n3
  go to 4293
4286 n18 = n18 - 1
  go to 4273
4288 if (n4 .ge. n15) go to 4293
  n15 = n4
4293 if (iprsup .ge. 1) write (unit = *, fmt = 4296)  n4, n17, n18, n15, n14, ansi32(n14 : 32)
4296 format (' f-field.  n4, n17, n18, n15, n14 =', 5i6, '    ansi32(n14:32) =', a)
  if (n5 .eq. 0) go to 4306
  if (n18 + n5 .gt. 35) call stoptp
  n4 = n18 + n5
  spycd2(n18 + 1 : n4) = ansi5(1 : n5)
  n18 = n4
4306 if (d8 .ge. 0.0d0) go to 4314
  n17 = n17 - 1
  spycd2(n17 : n17) = '-'
4314 n17 = n17 - 1
  spycd2(n17 : n17) = ' '
  n14 = 32 - (n18 - n17)
  ansi32(n14 : 32) = spycd2(n17 : n18)
4321 ansi32(1 : n14 - 1) = blan80(1 : n14 - 1)
  if (iprsup .ge. 1)  write (unit = *, fmt = 4387) n14, ansi32(n14 : 32)
4387 format (' Exit "fltopt".  n14 =', i3, '   ansi32(n14:32) =', a)
  return
end subroutine fltopt

!
! subroutine store.
!

subroutine store (i, j, n, d2r, d2x, d1r, d1x)
  use zprint
  use blkcom
  implicit none
  !  common /zprint/ zoutr(120), zoutx(120)
  integer(4), intent(in) :: i
  integer(4), intent(in) :: j
  integer(4), intent(in) :: n
  real(8), intent(in) :: d2r
  real(8), intent(in) :: d2x
  real(8), intent(in) :: d1r
  real(8), intent(in) :: d1x
  integer(4) :: k1, k2, k16, kij, km1, km2, km3, km4, km5, km6
  real(8) :: br, bx
  !
  if (iprsup .ge. 5) write (unit = lunit6, fmt = 2472) i, j, n, d2r, d2x, d1r, d1x
2472 format (' i, j, n, d2r, d2x, d1r, d1x =', 3i5, 4e18.8)
  kij = j + i * (i - 1) / 2
  k1 = (n + i - 1) * (n + i) / 2 + n + j
  k16 = 2 * n
  k2 = (k16 + i - 1) * (k16 + i) / 2 + k16 + j
  km1 = (n + j - 1) * (n + j) / 2 + i
  km2 = (k16 + j - 1) * (k16 + j) / 2 + i
  km3 = km2 + n
  km4 = (n + i - 1) * (n + i) / 2 + j
  km5 = (k16 + i - 1) * (k16 + i) / 2 + j
  km6 = km5 + n
  br = (d2r + 2.0d0 * d1r) / 3.0d0
  bx = (d2x + 2.0d0 * d1x) / 3.0d0
  zoutr(kij) = br
  zoutr(k1) = br
  zoutr(k2) = br
  zoutx(kij) = bx
  zoutx(k1) = bx
  zoutx(k2) = bx
  br = (d2r - d1r) / 3.0d0
  bx = (d2x - d1x) / 3.0d0
  zoutr(km1) = br
  zoutr(km2) = br
  zoutr(km3) = br
  zoutr(km4) = br
  zoutr(km5) = br
  zoutr(km6) = br
  zoutx(km1) = bx
  zoutx(km2) = bx
  zoutx(km3) = bx
  zoutx(km4) = bx
  zoutx(km5) = bx
  zoutx(km6) = bx
  return
end subroutine store

!
! subroutine round.
!

subroutine round (x)
  use tracom
  implicit none
  !
  !)    The purpose of this module is to numerically round the argument
  !)    'x'  to five decimal digits (or thereabouts), and then pass this
  !)    rounded number back to the calling module.   This version will
  !)    work on any computer system.   Yet if  encode  and  decode  are
  !     available, it is perhaps preferable to use these, as per  ....
  !                  dimension  adum(5)
  !                  encode (20, 5540, adum(1) )  x
  !                  decode (20, 5540, adum(1) )  x
  !             5540 format ( 1e20.5 )
  real(8), intent(out) :: x
  integer(4) :: i10
  integer(4) :: n1
  real(8) :: a, a10
  !
  a = absz (x)
  if (a .eq. 0.0d0) go to 1492
  i10 = alog1z (a)
  a10 = 10.0d0 ** (i10 - 5)
  a = a / a10
  n1 = a + 0.5d0
  a = n1
  a = a * a10
  if (x .lt. 0.0d0) a = -a
1492 x = a
  return
end subroutine round

!
! subroutine bctran.
!

subroutine bctran
  use over41mod
  use newt1
  use blkcom
  use tracom
  implicit none
  !     Multi-phase transformer data generator that was handed over
  !     to us at BPA by prof. Hermann W. Dommel of the University of
  !     British Columbia (Vancouver, b.c., Canada) on December
  !     18, 1980.   This uses  [a], [b]  representation.   Original
  !     file is  [scott]hwd.for;1 ,   as read from Hermann's cards.
  !     If number of windings is to be increased from 10 to n,
  !     then simply change 10 to n and 55 to n*(n+1)/2, 30 to 3*n
  !     and 60 to 6*n in next 6 lines and in one line between statement
  !     number 1 and 4.
  character(8) :: name(60), iname(6)
  integer(4) :: err
  integer(4) :: i, i1, i2, iad, iagain, ib, ibg, icount, ie, ien, ii
  integer(4) :: ii50, ij, ik, iloss, imagn, in, ip, iprint, iput, iss, iw, iw1
  integer(4) :: iw2, iwin
  integer(4) :: j, jj, jp, jw
  integer(4) :: k1, k2, k3, kcode, kdelta, kk2, kk3
  integer(4) :: l
  integer(4) :: m1, m5, maxit, mdelta, mk, mz
  integer(4) :: n, n1, n2, nad(55), nf, nn1, nn2, nphase, nt
  integer(4) :: kb(55), ke(55)
  real(8) :: a, a1, a2, a3, aq, az
  real(8) :: b, b1, b2, b3, bb, bexm, bexs
  real(8) :: c
  real(8) :: d, d1, dm2, dm3, dm4, dnom
  real(8) :: e, ethree, etwo
  real(8) :: freq
  real(8) :: gexm, gexs
  real(8) :: h1, h2, h11, h22, hfreq
  real(8) :: othree
  real(8) :: q
  real(8) :: r, ratpos, ratzer, rtot, rout(30), rmva(55), rwin2(10)
  real(8) :: s
  real(8) :: three
  real(8) :: v(10)
  real(8) :: var1
  real(8) :: ww
  real(8) :: x(3), xo(55), xp(55), xpos, xtot, xzero
  real(8) :: y(30)
  real(8) :: z1, zmm, zo, zpos, zs(55), zss, zm(55), zsd(10)
  real(8) :: zmd(10)
  !
  !  common /newt1/ rwin(10), zhl, zht, zlt, k, m, idelt, logsix
  data maxit / 200 /
  logsix = lunit6
  err = flzero * 10.0d0
  aq = unity / 3.0d0
  d1 = 3.0d0
  othree = unity / sqrtz (d1)
  etwo = 1.0d-2
  ethree = tenm3
  three = 3.0d0
  !     read input card using cimage.
100 call cimage
  read (unit = abuff, fmt = 4001) iwin, freq, bexs, ratpos, gexs, bexm, ratzer, gexm, nphase, itest, iput, iprint
4001 format (i2, 7e10.2, 4i2)
  if (iwin .le. 0) go to 312
  if (iwin .gt. 10 .or. iwin .eq. 1) go to 120
  if (nphase .ne. 1) go to 36
  bexm = bexs
  ratzer = ratpos
  gexm = gexs
  write (unit = lunit6, fmt = 37)
37 format (' Three phase bank consists of single phase units')
36 write (unit = lunit6, fmt = 4004) iwin, freq, iprint, bexs, ratpos, gexs, bexm, ratzer, gexm
4004 format ('0', i6, '-Winding transformer. freq.=', f6.2, 'Hz', 10x, 'output option =', i3, //, 27x, 'Exciting current(percent) rating(MVA) excitation loss(kW)', /, ' pos. seq.', f13.5, f18.2, f19.4, /, ' Zero seq.', f13.5, f18.2, f19.4, /, 1x, 'Winding no. voltage(kV)   R(Ohm)', 16x, 'Node name pairs for legs i, ii, iii')
  hfreq = freq
  freq = freq * twopi
  k1 = iwin - 1
  in = (iwin * k1) / 2
  do j = 1, in
155  nad(j) = 0
  end do
  do k = 1, iwin
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 701) m1, z1, zo, (iname(i), i = 1, 6)
701  format (i3, 2e10.2, 1x, 6a6)
     write (unit = lunit6, fmt = 4009) m1, z1, zo, (iname(i), i = 1, 6)
4009 format (' ', i6, f16.3, f10.5, 6(4x, a6))
     m5 = (m1 - 1) * 6
     do i = 1, 6
        m5 = m5 + 1
860     name(m5) = iname(i)
     end do
     rwin(m1) = zo * aq / z1 ** 2
     rwin2(k) = 0.0d0
11   v(m1) = othree / z1
  end do
  write (unit = lunit6, fmt = 4008) itest, iput
4008 format (' Excitation test made from winding', i3, '. Magn. impedance placed across winding', i3)
  gexs = gexs * ethree
  gexm = gexm * ethree
  bexs = (bexs * ratpos * etwo) ** 2 - gexs ** 2
  if (bexs .lt. 0.0d0) go to 850
  bexs = sqrtz (bexs)
  bexm = (bexm * ratzer * etwo) ** 2 - gexm ** 2
  if (bexm .lt. 0.0d0) go to 850
  bexm = sqrtz (bexm)
  nn1 = itest
  nn2 = iput
  imagn = 0
  if (itest .lt. iput) go to 12
  nn1 = iput
  nn2 = itest
12 i = 0
  if (nn1 .le. 0 .and. nn2 .gt. 0) go to 320
  write (unit = lunit6, fmt = 4005)
4005 format ('0', 24x, 'positive sequence', 25x, 'zero sequence', 10x, ' closed delta', /, ' from  to  load loss(kw)  impedance(percent)  rating(MVA)   impedance(percent) rating(MVA)', 5x, 'in')
  !     Read input card using cimage.
25 call cimage
  read (unit = abuff, fmt = 4202) ibg, ien, h1, xpos, ratpos, xzero, ratzer, idelt, k3
4202 format (2i2, 5e10.2, 2i2)
  if (i .eq. 0) iloss = k3
  if (ibg .le. 0 .or. ien .le. 0) go to 10
  if (nphase .ne. 1) go to 38
  idelt = 0
  xzero = xpos
  ratzer = ratpos
38 write (unit = lunit6, fmt = 4007) ibg, ien, h1, xpos, ratpos, xzero, ratzer, idelt
4007 format (' ', 2i4, 2f15.5, f17.2, f16.5, f14.2, 7x, i3)
  if (ibg .eq. ien) go to 300
  if (ibg .gt. iwin .or. ien .gt. iwin) go to 300
  if (ibg .lt. ien) go to 20
  idelt = -idelt
  k3 = ibg
  ibg = ien
  ien = k3
20 if (i .eq. 0) go to 15
  do m = 1, i
     if (kb(m) .eq. ibg .and. ke(m) .eq. ien) go to 300
35 end do
15 i = i + 1
  kb(i) = ibg
  ke(i) = ien
  nad(i) = idelt
  rmva(i) = ratzer / etwo
  if (ibg .eq. nn1 .and. ien .eq. nn2) imagn = i
  z1 = (rwin(ibg) + rwin(ien)) ** 2
  if (h1 .lt. epsiln) go to 16
  h1 = h1 * ethree / (ratpos ** 2)
  z1 = h1 ** 2
  if (iloss .eq. 0 .or. iwin .gt. 3) go to 18
  h1 = h1 * onehaf
  rwin2(ibg) = rwin2(ibg) + h1
  rwin2(ien) = rwin2(ien) + h1
  k3 = 6 - ibg - ien
  if (k3 .le. 0) k3 = 4
  rwin2(k3) = rwin2(k3) - h1
  go to 18
16 iloss = 0
18 xpos = (xpos * etwo / ratpos) ** 2 - z1
  if (xpos .lt. 0.0d0) go to 301
  xp(i) = -sqrtz (xpos)
  xo(i) = -xzero * etwo / ratzer
  if (iabs (idelt) .gt. 0) go to 25
  xzero = (xzero * etwo / ratzer) ** 2 - z1
  if (xzero .lt. 0.0d0) go to 301
  xo(i) = -sqrtz (xzero)
  go to 25
10 if (in .ne. i) go to 310
  write (unit = lunit6, fmt = 901) iloss
901 format (' iloss =', i3)
  if (iloss .eq. 0) go to 303
  if (iwin .gt. 3) go to 303
  do i = 1, iwin
     b = rwin2(i)
     if (b.gt. 0.0d0) go to 181
     write (unit = lunit6, fmt = 890) i, b
890  format (' Resistance of winding ', i3, ' was calculated as ', e15.6, /, 'and is set equal to zero')
     b = 0.0d0
181  rwin(i) = b
  end do
  write (unit = lunit6, fmt = 182)
182 format(' Resistance matrix values calculated from load losses')
  !     Process input data to account for the closed delta effect in the
  !     zero  sequence  short-circuit tests******************************
19 i = 0
800 i = i + 1
  if (i .gt. in) go to 888
  iad = nad(i)
  if (iad .eq. 0) go to 800
  idelt = iabs (iad)
  k = kb(i)
  m = ke(i)
  if (idelt .eq. k .or. idelt .eq. m) go to 830
  kk2 = 0
  kk3 = 0
  kdelta = idelt
  mdelta = idelt
  k3 = k
  m1 = m
  if(k.lt.idelt) go to 801
  k3 = idelt
  kdelta = k
801 if (m .lt. idelt) go to 802
  m1 = idelt
  mdelta = m
802 do j = 1, in
     if (kb(j) .eq. k3 .and. ke(j) .eq. kdelta) kk2 = j
     if (kb(j) .eq. m1 .and. ke(j) .eq. mdelta) kk3 = j
805 end do
  if (kk2 .eq. 0 .or. kk3 .eq. 0) go to 830
  if (iad .gt. 0) go to 222
  k3 = k
  k = m
  m = k3
  k3 = kk2
  kk2 = kk3
  kk3 = k3
222 zht = -xo(kk2)
  zlt = -xo(kk3)
  zhl = -xo(i)
  x(1) = onehaf * (zhl + zht - zlt)
  k2 = nad(kk2)
  k3 = nad(kk3)
  if (k2 .eq. 0 .and. k3 .eq. 0) go to 210
  if (k2 .eq. 0) go to 833
  if (k3 .ne. 0) go to 832
  ik = kb(kk2)
  if (k2 .lt. 0) ik = ke(kk2)
  if (ik .ne. k) go to 833
  go to 212
210 call newton (x(1 :), maxit, err)
  x(2) = zlt - zht + x(1)
  x(3) = zht - x(1)
  go to 180
212 a = onehaf * zlt
  x(2) = a
  x(3) = a
  a1 = rwin(idelt)
  a2 = rwin(m)
  rtot = a1 + a2
  dnom = rtot ** 2 + zlt ** 2
  d = a1 * a2 - a ** 2
  e = a * rtot
  b = (d * rtot + e * zlt) / dnom
  c = (rtot * e - d * zlt) / dnom
  a = zhl ** 2 - (rwin(k) + b) ** 2
  if (a .lt. 0.0d0) go to 837
  x(1) = sqrtz (a) - c
172 format ('0zero sequence test data between', i3, ' and', i3, ' is modified for open delta in', i3, /, 'with delta closed again, modified data produces Z=', e14.6, ' percent, which should agree with input value.')
  !     To check impedances
180 a1 = rwin(k)
  a2 = rwin(m)
  a3 = rwin(idelt)
  b1 = x(1)
  b2 = x(2)
  b3 = x(3)
  rtot = a2 + a3
  xtot = b2 + b3
  a = (a1 * rtot - b1 * xtot + a2 * a3 - b2 * b3) ** 2 + (a1 * xtot + b1 * rtot + a2 * b3 + a3 * b2) ** 2
  b = rtot ** 2 + xtot ** 2
  zhl = a / b
  if (zhl .lt. 0.0d0) go to 950
  zhl = sqrtz (zhl) * rmva(i)
  write (unit = lunit6, fmt = 172) k, m, idelt, zhl
  xo(i) = -b1 - b2
  nad(i) = 0
  if (k2 .eq. 0) go to 800
  write (unit = lunit6, fmt = 173) k, idelt, k, m, m, idelt
173 format ('0input value of zero sequence short-circuit impedance from', i3, ' to', i3, ' is ignored and set equal to value from', i3, ' to', i3, /, 'because both impedances must be equal if there are closed deltas in', i3, ' and', i3)
  xo(kk2) = -b1 - b3
  nad(kk2) = 0
  go to 800
888 do j = 1, in
156  nad(j) = 0
  end do
  !     End  of  input  data  processing *********************************
  if (nn1 .eq. 0) go to 154
  a = rwin(itest)
  z1 = 0.0d0
  zo = 0.0d0
  if (imagn .eq. 0) go to 158
  z1 = -xp(imagn)
  zo = -xo(imagn)
158 h1 = (gexs ** 2 + bexs ** 2) * a
  if (gexs .ge. h1) go to 845
  gexs = h1
  write (unit = lunit6, fmt = 846) gexs
846 format ('0pos. seq. excitation losses raised to', e15.6, ' MW')
845 h1 = gexs ** 2 + bexs ** 2
  if (h1 .lt. epsiln) go to 157
  h11 = gexs / h1 - a
  h1 = bexs / h1 - z1
  ww = h11 ** 2 + h1 ** 2
  gexs = h11 / ww
  bexs = h1 / ww
157 h1 = (gexm ** 2 + bexm ** 2) * a
  if (gexm .ge. h1) go to 847
  gexm = h1
  write (unit = lunit6, fmt = 848) gexm
848 format ('0zero seq. excitation losses raised to', e15.6, ' MW')
847 h1 = gexm ** 2 + bexm ** 2
  if (h1 .lt. epsiln) go to 154
  h11 = gexm / h1 - a
  h1 = bexm / h1 - zo
  ww = h11 ** 2 + h1 ** 2
  gexm = h11 / ww
  bexm = h1 / ww
154 write (unit = lunit6, fmt = 853)
853 format ('0', 15x, 'Shunt resistances for representation of excitation losses%')
  if (gexm .ge. gexs)  go to 861
  gexm = gexs
  write (unit = lunit6, fmt = 862)
862 format (' Zero sequence shunt resistance reduced to be equal to positive sequence value.')
861 if (absz (gexm) .lt. epsiln .or. absz (gexs) .lt. epsiln) go to 852
  gexm = gexs / gexm
  gexs = 1.0d0 / gexs
  gexm = (gexm - unity) * gexs * aq
  gexs = gexm + gexs
  if (nn1 .eq. 0) go to 854
  q = v(iput) ** 2
  b = gexs / q
  bb = gexm / q
  write (unit = lunit6, fmt = 855) iput, b, bb
855 format (' Place shunt resistance matrix across winding', i3, ' with R(self/Ohm)=', e15.6, 5x, ' and R(mutual/Ohm)=', e15.6)
  go to 153
854 write (unit = lunit6, fmt = 858)
858 format (' Place shunt resistance matrix across all terminals with the following values%', /, 'winding no.', 10x, 'R(self/Ohm)', 10x, 'R(mutual/Ohm)')
  do i = 1, iwin
     q = v(i) ** 2
     b = gexs * iwin / q
     bb = gexm * iwin / q
856  write (unit = lunit6, fmt = 857) i, b, bb
  end do
857 format (i6, e28.6, e21.6)
  go to 153
852 write (unit = lunit6, fmt = 859)
859 format (' Leave off, because series resistances already produce losses which are greater than input values of excitation losses.')
153 continue
  do i = 1, in
     z1 = xp(i) * aq
     zo = xo(i) * aq
     zss = zo + 2.0d0 * z1
     zmm = zo - z1
     if (ke(i) .eq. iwin) go to 85
     xp(i) = -zss
     xo(i) = -zmm
     go to 80
85   m = kb(i)
     zsd(m) = zss
     zmd(m) = zmm
     mz = (m * (m + 1)) / 2
     zs(mz) = zss
     zm(mz) = zmm
     nad(m) = mz
80 end do
  do n = 1, in
     if (ke(n) .eq. iwin) go to 90
     ib = kb(n)
     ie = ke(n)
     zss = (zsd(ib) + xp(n) + zsd(ie)) * onehaf
     zmm = (zmd(ib) + xo(n) + zmd(ie)) * onehaf
     ia = (ie * (ie - 1)) / 2
     mk = ia + ib
     zs(mk) = zss
     zm(mk) = zmm
90 end do
  j = iwin
  ij = (k1 * j) / 2
  iagain = 0
33 j = j - 1
  if (j .eq. 0) go to 200
  h1 = zs(ij)
  h11 = zm(ij)
  if (absz (h1) .lt. epsiln) go to 110
  zpos = h1 - h11
  ww = 2.0d0 * h11
  az = zpos / (h1 + ww)
  zpos = 1.0d0 / zpos
  var1 = zpos * aq
  h1 = -(2.0d0 + az) * var1
  h11 =  - (az - 1.0d0) * var1
  xp(j) = h1
  xo(j) = h11
  ij = ij - j
  k = 0
  ik = 0
44 ik = ik + k
  i1 = ik + 1
  k = k + 1
  if (k .gt. k1) go to 33
  if (k .lt. j) go to 99
  if (k .eq. j) go to 77
  i = ik + j
55 h2 = zs(i)
  h22 = zm(i)
  dm2 = h22 * h11
  xp(k) = h2 * h1 + 2.0d0 * dm2
  xo(k) = h2 * h11 + h1 * h22 + dm2
  i2 = ik + k
  l = 0
  do i = i1, i2
     l = l + 1
     dm3 = xp(l)
     dm4 = xo(l) * h22
     zs(i) = zs(i) + dm3 * h2 + 2.0d0 * dm4
6    zm(i) = zm(i) + dm3 * h22 + xo(l) * h2 + dm4
  end do
  if (k .lt. j) go to 44
  i = ik + j
  zs(i) = xp(k)
  zm(i) = xo(k)
  go to 44
77 i = ij
  do l = 1, j
     i = i + 1
     zs(i) = xp(l)
81   zm(i) = xo(l)
  end do
  go to 44
99 i = ij + k
  go to 55
200 j = 0
  if (iagain .eq. 1) go to 503
  k = 0
  n2 = in
  n1 = in + 1
51 k = k + 1
  i = nad(k)
  r = -zs(i)
  s = -zm(i)
  if (k .eq. 1) go to 48
  do i = n1, n2
     j = j + 1
     a = zs(j)
     b = zm(j)
     zs(i) = zs(i) - a
     zm(i) = zm(i) - b
     r = r - a
49   s = s - b
  end do
48 j = j + 1
  n2 = n2 + 1
  zs(n2) = r
  zm(n2) = s
  if (k .lt. k1) go to 51
  a = 0.0d0
  b = 0.0d0
  do i = n1, n2
     a = a - zs(i)
52   b = b - zm(i)
  end do
  n2 = n2 + 1
  zs(n2) = a
  zm(n2) = b
  bexm = (bexm - bexs) * aq
  bexs = bexm + bexs
  if (iput .eq. 0) go to 201
  nn1 = iput * (iput + 1) / 2
  zs(nn1) = zs(nn1) + bexs
  zm(nn1) = zm(nn1) + bexm
  go to 203
201 nn1 = 0
  a = unity / iwin
  do i = 1, iwin
     nn1 = nn1 + i
     zs(nn1) = zs(nn1) + bexs * a
202  zm(nn1) = zm(nn1) + bexm * a
  end do
203 m = 0
  do k = 1, iwin
     a = v(k) * freq
     do i=1, k
        m = m + 1
        c = v(i) * a
        zs(m) = zs(m) * c
72      zm(m) = zm(m) * c
     end do
71 end do
  iss = 0
716 if (iprint .gt. 0) go to 752
  write (unit = lunit6, fmt = 715)
715 format ('0 branch data - resistance matrix (Ohms) and inverse inductance matrix (1/Henries)')
  if (iss .le. 0) go to 730
506 write (unit = lunit6, fmt = 709) hfreq
709 format('0 branch data - resistance matrix (Ohms) and reactance matrix (Ohms) at', f6.2, ' Hz')
730 ib = 3 * iwin
  icount = 0
  if (nphase .eq. 1 )  ib = iwin
952 do i = 1, ib
508  rout(i) = 0.0d0
  end do
  do i = 1, iwin
     a = rwin(i) / v(i) ** 2
     rout(i) = a
     iw1 = iwin + i
     iw2 = iwin + iw1
     rout(iw1) = a
     rout(iw2) = a
502 end do
  do ii = 1, ib
     ip = (ii - 1) / iwin + 1
     iw = ii - ((ii - 1) / iwin) * iwin
     nf = (iw - 1) * 6 + (ip - 1) * 2 + 1 + icount
     nt = nf + 1
     do i = 1, ib
708     y(i) = 0.0d0
     end do
     do jj = 1, ii
        jp = (jj - 1) / iwin + 1
        jw = jj - ((jj - 1) / iwin) * iwin
        if (iw .lt. jw) go to 710
        k = (iw - 1) * iw / 2 + jw
        go to 711
710     k = (jw - 1) * jw / 2 + iw
711     if (ip .eq. jp) go to 703
        y(jj) = zm(k)
        go to 705
703     y(jj) = zs(k)
705  end do
     ii50 = ii
     if (ii .eq. 1) go to 722
     write (unit = lunit6, fmt = 721) ii50, name(nf), name(nt), (rout(i), y(i), i = 1, ii)
721  format (' ', i2, 2a6, 12x, 2e17.10, /, (' ', 26x, 2e17.10))
     write (unit = lunit7, fmt = 726) ii50, name(nf), name(nt), (rout(i), y(i), i = 1, ii)
726  format (i2, 2a6, 12x, 2e16.10, /, (26x, 2e16.10))
     go to 724
722  write (unit = lunit6, fmt = 723) ii50, name(nf), name(nt), (rout(i), y(i), i = 1, ii)
723  format (' ', i2, 2a6, 12x, 2e17.10)
     write (unit = lunit7, fmt = 725) ii50, name(nf), name(nt), (rout(i), y(i), i = 1, ii)
725  format (i2, 2a6, 12x, 2e16.10)
724  rout(ii) = 0.0d0
706 end do
  if (nphase .ne. 1) icount = 5
  icount = icount + 2
  if (icount .le. 4) go to 952
  if (iss .eq. 1 .or. iprint .eq. 0) go to 100
752 iagain = 1
  j = iwin + 1
  ij = iwin * j / 2
  k1 = iwin
  go to 33
503 m = 0
  do k = 1, iwin
     do i = 1, k
        m = m + 1
        zs(m) = -zs(m) * freq
504     zm(m) = -zm(m) * freq
     end do
505 end do
  iss = 1
  go to 506
120 write (unit = lunit6, fmt = 121) iwin
121 format (' Number of windings =', i3)
  call stoptp
300 write (unit = lunit6, fmt = 309) ibg, ien
309 format (' ', 2i3, ' Wrong winding numbers')
  call stoptp
301 write (unit = lunit6, fmt = 302) ibg, ien
302 format (' Load losses or winding resistances too large', 2i3)
  call stoptp
303 write (unit = lunit6, fmt = 304)
304 format (' Resistance matrix values are those which were read in')
  go to 19
310 write (unit = lunit6, fmt = 311) i, in
311 format (' only', i3, ' short-circuit tests specified but', i3, ' are needed')
  call stoptp
320 write (unit = lunit6, fmt = 321) itest, iput
321 format (' either itest =', i3, ' or iput =', i3, ' not permitted')
  call stoptp
110 write (unit = lunit6, fmt = 101) j
101 format (' diagonal element in row', i3, ' close to zero')
  call stoptp
830 write (unit = lunit6, fmt = 840) idelt, k, m
840 format (' idelta=', i2, ' wrong in short-circuit test between', i3, ' and', i3)
  call stoptp
832 kcode = 1
835 write (unit = lunit6, fmt = 834) k, m, kcode
  call stoptp
833 kcode = 2
  go to 835
837 kcode = 3
  go to 835
  ! 839 kcode = 4   15 feb 1983 "m34" ibm emtp feedback from
  !     go to 835   pacific power: unreachable s.n. 839.  wsm.
950 kcode = 5
  go to 835
850 write (unit = lunit6, fmt = 851)
851 format (' p.u. excitation loss larger than p.u. exciting current (either in pos. or zero sequence)')
834 format (' Modification of zero sequence short-circuit test between', i3, ' and', i3, ' not possible. Error code =', i3)
312 call stoptp
end subroutine bctran

!
! end of file over41.f90
!
