!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over13.f90
!

module ovr13mod
  implicit none
contains

  !
  ! subroutine redu13.
  !

  subroutine redu13 (a, n, m)
    implicit none
    !     This subroutine can be used for either partial reduction or for
    !     complete inversion of a real  n by n  symmetric matrix  'a' .
    !     Storage for matrix elements is in the implied order   (1,1),
    !     (1,2), (2,2), (1,3), (2,3), (3,3), (1,4),  etc.,  for the
    !     complete upper triangle.   Variables  m+1, ...., n  are to be
    !     eliminated (which is the definition of variable  'm', note).
    !     Result is reduced matrix in columns 1,...m in case of reduction
    !     (m unequal 0) or negative inverse matrix in columns 1,...n in case
    !     of inversion (m=0).
    real(8), intent(out) :: a(:)
    integer(4), intent(in) :: m, n
    integer(4) :: i, i1, i2, ij, ik
    integer(4) :: j
    integer(4) :: k
    integer(4) :: l
    real(8) :: b(100)
    real(8) :: h1, h2
    real(8) :: w
    !
    j = n + 1
    w = 1.0d0
    if (m .gt. 0) w = -w
    ij = n * j / 2
3   j = j - 1
    if (j .eq. m) return
    h1 = -1.0 / a(ij)
    b(j) = h1
    ij = ij - j
    k = 0
    ik = 0
    !                                   begin k-loop
4   ik = ik + k
    i1 = ik + 1
    k = k + 1
    if (k .gt. n) go to 3
    if (k .lt. j) go to 9
    if (w .lt. 0.) go to 3
    if (k .eq. j) go to 7
    i = ik + j
5   h2 = a(i)
    b(k) = h2 * h1
    !                                   begin i-loop
    i2 = ik + k
    l = 0
    do i = i1, i2
       l = l + 1
       a(i) = a(i) + b(l) * h2
    end do
    if (k .lt. j) go to 4
    i = ik + j
    a(i) = b(k)
    go to 4
    !                                   end i-loop
7   i = ij
    do l = 1, j
       i = i + 1
       a(i) = b(l)
    end do
    go to 4
    !                                   end k-loop
9   i = ij + k
    go to 5
  end subroutine redu13

  !
  ! function funp13.
  !

  function funp13 (y, x, twopi) result(valueOut)
    use tracom
    implicit none
    real(8), intent(in) :: twopi
    real(8), intent(in) :: x
    real(8), intent(in) :: y
    real(8) :: valueOut
    !
    if (x .ne. 0.0d0) go to 101
    if (y .ne. 0.0d0) go to 102
    valueOut = 0.0d0
    go to 110
102 if (y .gt. 0.0d0) go to 103
    valueOut = -twopi / 4.0d0
    go to 110
103 valueOut = twopi / 4.0d0
    go to 110
101 if (y .ne. 0.0d0) go to 105
    if (x .gt. 0.0d0) go to 104
    valueOut = twopi / 2.0d0
    go to 110
104 valueOut = 0.0d0
    go to 110
105 valueOut = atan2z (y, x)
110 continue
  end function funp13

end module ovr13mod

!
! subroutine over13.
!

subroutine over13
  use blkcom
  use labcom
  use tracom
  use movcop
  use ovr44c, only: cominv
  use freedom
  use strcom
  use ovr13mod
  use fdqlcl
  implicit none
  !  dimension cblhst(1)
  !  dimension cmr(1), cmi(1), vim(1)
  !  dimension infdli(1)
  !  dimension ekreal(50), ekimag(50), emreal(50), emimag(50)
  !  dimension closev(50), farv(50), closei(50), fari(50)
  !  dimension w1(1)
  !  dimension wk1(1)
  !
  !  equivalence (cnvhst(1), cblhst(1))
  !  equivalence (namebr(1), infdli(1))
  !  equivalence (volt(1), vim(1))
  !  equivalence (kks(1), cmr(1))
  !  equivalence (kknonl(1), cmi(1))
  !  equivalence (lstat(14), mdrive)
  !  equivalence (ykm(1), w1(1))
  !  equivalence (semaux(1), wk1(1))
  !
  character(8) :: text1, text2
  integer(4) :: i, i1, iadrs, ibadd, ibf, icbr, idt, ier, ifa, ifd
  integer(4) :: iflag, ikf, ii, iip, im, ioff1, ioff2, ioff3, ioff4, ioff5
  integer(4) :: ioff6, ioff7, ioff8, ip, ipp, iprcbl, iprint, iq, iqa, isd
  integer(4) :: isd1, isecti, isfd, itadd, itam, ivbr, ix, iy, iz
  integer(4) :: j, j0, j1, j2, jadd, jglnn, jh, jip, jj, jkl, jm, jpl, jqa, jv
  integer(4) :: k, ka, kadt, kcbl, kf, kj, kmode, kmv, koff11, koff12, kq, kqk
  integer(4) :: kqv, kqvq, kv, kvd, ky
  integer(4) :: l, la, lg, lj, lj1, ll, ll0, ll1, lq, lqv
  integer(4) :: m, marti
  integer(4) :: n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15
  integer(4) :: n16, n17, n18, n19, n20, ndx1, ndx2, nk1, nk7, nk8, nkp, nl, nl1
  integer(4) :: nn17, nn4, nn5, nn6, nphs, nphs2, nq1, nq2, nq3, nra, nrf, nrz, ns1
  integer(4) :: ntaui, nteq
  real(8) :: a, absfk, absfm, ai, aki, akr, amagk, amagm, angk, angm, aph
  real(8) :: api, apr, ar
  real(8) :: bi3, bi4
  real(8) :: c2i, c2r, c3i, c3r, ccc3, ccc4, closev(50), farv(50), closei(50)
  real(8) :: ckimag, ckreal, cmimag, cmreal, csl
  real(8) :: csli, csr, csri, curi1, curi2, curr1, curr2, cziim, czire
  real(8) :: d, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14
  real(8) :: ddd3, ddd4, djk3, djk4, dki, dkr, dqq3
  real(8) :: ejwti, ejwtr, ekreal(50), ekimag(50), emreal(50), emimag(50)
  real(8) :: fari(50), fzero
  real(8) :: gus1, gus3, gus4
  real(8) :: h1, h2, hf
  real(8) :: omegs1, omg
  real(8) :: phf, phfk, phfm
  real(8) :: steady, sumki, sumkr, summi, summr
  real(8) :: taua, taui, tpi
  real(8) :: vi1, vi2, vr1, vr2, vsl, vsli, vsr, vsri
  real(8) :: wd, wdt, winic
  real(8) :: yx
  !
  integer(4), pointer :: infdli(:) => namebr(1 :)
  integer(4), pointer :: mdrive => lstat(14)
  real(8), pointer :: cblhst(:) => cnvhst(1 :)
  real(8), allocatable :: cmr(:)
  real(8), pointer :: wk1(:) => semaux(1 :)
  !
  data text1 / 'parame' /
  data text2 / 'ters  ' /
  !
  ll0 = size (transfer (kks, cmr))
  allocate (cmr(ll0))
  cmr = transfer (kks, cmr)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567) istead, ibr, ntot
4567 format (' Begin module "over13".  istead     ibr    ntot', /, 23x, 8i8)
  jglnn = 0
  isecti = 400
  ll0 = 0
  kcbl = 0
  kmode=0
  ll1 = 1
  mdrive = 0
  marti = 0
  ! initialize counters for the -666 branches*   *   *   *   *   *   *
  ibf = 0
  ikf = 0
  isfd = 0
  fzero = 0.0d0
  if (istead .gt. 0) go to 546
  do i = 1, ibr
     if (kbus(i) .lt. 0) cycle
     ci(i) = 0.0d0
     cik(i) = 0.0d0
     ck(i) = 0.0d0
  end do
  call move0 (e(1 :), ntot)
  call move0 (f(1 :), ntot)
  call move0 (kode(1 :), ntot)
  call move0 (xk(1 :), lpast)
  call move0 (xm(1 :), lpast)
  call move0 (stailk(1 :), ltails)
  call move0 (stailm(1 :), ltails)
  go to 545
546 l = iline + 1
  k = lpast - iline
  call move0 (xk(l :), k)
  call move0 (xm(l :), k)
  k = 0
  itadd = it + 1
  marti = 1
547 k = k + 1
  if (imodel(k) .ne. -4) go to 4202
  jpl = 0
  do i = 1, ibr
     if (kodsem(i) .eq. 0) cycle
     kv = infdli(inoff1 + i)
     if (kv .eq. 0) cycle
     if (kv .eq. 1) go to 2024
     go to 2025
2024 nr(i) = lpast + kv
     it2 = length(i)
     nphs2 = it2 * it2
     do j = 1, nphs2
        jpl = jpl + wk1(koff20 + kv)
        kv = kv + 1
     end do
     cycle
2025 nr(i) = lpast + 2 * jpl + 1
  end do
4202 continue
  if (kill .gt. 0) go to 9200
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 110) k, ibr, kbus(k), nr(k), istead, kodebr(k)
110 format (/, " In  'over13' ,   next coupled branch group.         k       ibr   kbus(k)     nr(k)    istead kodebr(k)", /, 44x, 6i10)
  if (k .gt. ibr) go to 521
  l = kbus(k)
  n15 = iabs(l)
  if (n15 .eq. 1) n15 = iabs (mbus(k))
  n16 = iabs (kssfrq(n15))
  omega = twopi * sfreq(n16)
  steady = omega * deltat
  if (l .lt. 0) go to 575
  m = iabs (mbus(k))
  go to 560
521 if (inonl .eq. 0) go to 545
  ibadd = ibr
  do i = 1, inonl
     if (nltype(i) .ne. -99) go to 3684
     if (anonl(i) .ge. 0.0d0) cycle
     anonl(i) = -anonl(i)
     k = nonlk(i)
     m = iabs (nonlm(i))
     d6 = e(k) - e(m)
     n4 = nonlad(i)
     curr(i) = 1.0
     if (d6 .lt. 0.0) curr(i) = -1.0
     if (absz (d6) .le. vchar(i)) go to 3676
     if (noutpr .eq. 0) write (unit = lunit(6), fmt = 3671) bus(k), bus(m), d6
3671 format (' type-99 element from ', "'", a6,  "'", ' to ', "'", a6,  "'", ' has initial condition voltage of', e15.6, &
          '   which is beyond segment-1 breakpoint.', /, 70x, 'The user should expect spurious transients at time zero.')
3676 cycle
3684 j0 = nonle(i)
     if (j0 .gt. 0) cycle
     if (nltype(i) .eq. -98) curr(i) = 1.0
     k = nonlk(i)
     m = iabs (nonlm(i))
     if (vzero(i) .eq. 0.) go to 522
     ibadd = ibadd + 1
     if (nltype(i) .eq. -98) go to 522
     curr(i) = -ci(ibadd)
522  ipp = k
     if (ipp .eq. 1) ipp = m
     ipp = iabs (kssfrq(ipp))
     omega = twopi * sfreq(ipp)
     ci1 = (f(k) - f(m)) / omega
     if (iprsup .ge. 2) write (unit = lunit(6), fmt = 4275) i, nltype(i), k, m, ci1, omega, f(k), f(m)
4275 format (/, ' Flux calc.       i  nltype       k       m',  12x,  'ci1', 10x, 'omega', 11x, 'f(k)', 11x,  'f(m)', /, 11x, 4i8, 4e15.6)
     if (nltype(i) .ne. -96) go to 7115
     if (ci1 .ne. 0) go to 710
     ci1 = vecnl1(i)
     n11 = nonlad(i)
     cchar(n11 + 3) = curr(i)
     go to 7122
     ! check that the initial flux-current point lies within the major
     ! hysteresis loop.
710  n10 = ilast(i)
     n11 = nonlad(i)
     n11 = cchar(n11)
     n12 = nonlad(i) + 3
     cchar(n12) = curr(i)
     n12 = n10 + n11
     if (curr(i) .gt. cchar(n10)) go to 720
     d12 = vchar(n12 + 1) * curr(i) + cchar(n12 + 1)
     go to 726
720  if (curr(i) .lt. cchar(n12 - 1)) go to 730
     n13 = n12 + n11 + 1
     d12 = vchar(n13) * curr(i) + cchar(n13)
726  d13 = d12
     d14 = d12
     go to 760
730  n18 = n10 + 1
     n19 = n12 - 1
     do n14 = n18, n19
        if (curr(i) .gt. cchar(n14)) cycle
        n15 = n14
        go to 745
     end do
745  n16 = n15 + n11 + 1
     d13 = vchar(n16) * curr(i) + cchar(n16)
     do n14 = n18, n19
        if (-curr(i) .gt. cchar(n14)) cycle
        n15 = n14
        go to 755
     end do
755  n16 = n15 + n11 + 1
     d14 = vchar(n16) * curr(i) - cchar(n16)
     if (d14 + flzero .ge. ci1 .and. ci1 .ge. d13 - flzero) go to 7122
     d12 = onehaf * (d13 + d14)
760  if (d12 .eq. ci1) go to 7122
     write (unit = lunit(6), fmt = 773) i, bus(k), bus(m), ci1, curr(i)
773  format (/, 10x, 'Note  ---- Nonlinear element number', i4, '  is a type-96 hysteretic inductor which is connected', /, 21x, 'between busses  ',  "'", a6,  "'", '  and  ',  "'", a6, "'", ' .   The initial flux-current point as found by the', /, 21x, 'phasor steady-state solution has been observed to lie outside the user-defined major hysteresis loop, however.', /, 21x, 'The initial flux is', e14.4, '   and the initial current is', e14.4, ' .    The EMTP shall now alter this')
     write (unit = lunit(6), fmt = 776) d14, d13, d12
776  format (21x, 'just-printed flux so as to make it legal, while holding the current constant.   The line of constant current', /, 21x, 'intersects the user-supplied major hysteresis loop at two points (possibly equal, if the current is large', /, 21x, "enough).   The  'upper'  is cut at flux value", e14.5,  " ,   and the  'lower'  at flux value", e14.5, ' .', /, 21x, 'The initial flux shall be taken by the EMTP to be the average of these, which has flux value', e15.5, ' .')
     ci1 = d12
     if (iprsup .ge. 3) write (unit = lunit(6), fmt = 4288) n10, n11, n12, n18, n19, nonle(i), curr(i), ci1, d13, d14, e(k), e(m)
4288 format (/, ' Type-96 process.     n10     n11     n12     n18     n19', 12x, 'nonle(i)', /, 17x, 5i8, i10, /, 1x, 13x, 'curr(i)', 17x, 'ci1', 17x, 'd13', 17x, 'd14', 16x, 'e(k)', 16x, 'e(m)', /, 1x, 6e20.11)
7122 n6 = nonlad(i)
     n7 = cchar(n6)
     n12 = n6 + 2
     n15 = ilast(i)
     n10 = n15 + n7 - 1
     vnonl(i) = ci1
     vchar(n6 + 4) = vnonl(i)
     gslope(n6 + 4) = curr(i)
     cchar(n6 + 4) = -1
     if (e(k) .lt. e(m)) go to 920
     ! an upper trajectory is initially assumed
     cchar(n6 + 1) = 1
     vchar(n6 + 5) = vchar(n10)
     gslope(n6 + 5) = cchar(n10)
     d6 = 0.0
     if (absz (vchar(n6 + 5) - vchar(n6 + 4)) .gt. flzero) go to 1895
     d12 = 0.0
     vchar(n6) = 0.0
     vchar(n6 + 1) = 0.0
     cchar(n12) = n15 +n7
     go to 1312
1895 do n11 = n15, n10
        if (gslope(n6 + 4) .gt. cchar(n11)) cycle
        cchar(n12) = n11
        go to 1910
     end do
     cchar(n12) = n10 + 1
1910 n13 = cchar(n12) + n7 + 1
     d9 = vchar(n13) * gslope(n6 + 4) + cchar(n13)
     d12= vchar(n6+4) - d9
     go to 1700
     ! a downer trajectory is initially assumed
920  cchar(n6 + 1) = -1
     vchar(n6 + 5) = vchar(n15)
     gslope(n6 + 5) = cchar(n15)
     d6 = 0.0
     if (absz (vchar(n6 + 5) - vchar(n6 + 4)) .gt. flzero) go to 1945
     d12 = 0.0
     vchar(n6) = 0.0
     vchar(n6 + 1) = 0.0
     cchar(n12) = n15 + n7
     go to 1212
1945 do n11 = n15, n10
        if (-gslope(n6 + 4) .gt. cchar(n11)) cycle
        cchar(n12) = n11
        go to 1960
     end do
     cchar(n12) = n10 + 1
1960 n13 = cchar(n12) + n7 + 1
     d9 = vchar(n13) * gslope(n6 + 4) - cchar(n13)
     d12 = d9 - vchar(n6 + 4)
1700 vchar(n6) = (d12 - d6) / (vchar(n6 + 4) - vchar(n6 + 5))
     vchar(n6 + 1) = d12 - vchar(n6) * vchar(n6 + 4)
     if (cchar(n6 + 1) .eq. 1) go to 1312
1212 n14 = cchar(n12)
     n13 = n14 + n7 + 1
     d13 = gslope(n14) * (1.0 + vchar(n6))
     d13 = 1.0 / d13
     d14 = gslope(n13) - gslope(n14) * vchar(n6 + 1)
     d14= d14 * d13
     go to 1315
1312 n14 = cchar(n12)
     n13 = n14 + n7 + 1
     d13 = gslope(n14) * (1.0 - vchar(n6))
     d13 = 1.0 / d13
     d14 = gslope(n14) * vchar(n6 + 1) - gslope(n13)
     d14 = d14 * d13
1315 gslope(n6 + 1) = delta2 / d13
     gslope(n6) = curr(i) - (e(k) - e(m)) * gslope(n6 + 1)
     vchar(n12) = vnonl(i)
     vchar(n6 + 3) = curr(i)
     gslope(n6 + 2) = cchar(n6 + 1)
     gslope(n6 + 3) = cchar(n6 + 2)
     n8 = n6 + 5
     if (iprsup .ge. 3) write (unit = lunit(6), fmt = 4295) n13, (ipp, cchar(ipp), vchar(ipp), gslope(ipp), ipp = n6, n8)
4295 format (/, ' Done type-96 init.', 5x, 'n13 =', i4, /, 7x, 'row', 15x, 'cchar', 15x, 'vchar', 14x, 'gslope', /, (1x, i9, 3e20.11))
     go to 7117
7115 if (anonl(i) .eq. 0.0d0) cycle
7117 if (noutpr .eq. 0) write (unit = lunit(6), fmt = 427) bus(k), bus(m), ci1
427  format (' Initial flux in coil ', "'", a6, "'", ' to ', "'", a6, "'", ' =', e13.5)
     if (absz (ci1) .gt. absz (anonl(i)) .and. noutpr .eq. 0) write (unit = lunit(6), fmt = 426)
426  format (' Warning.  Assumption that AC steady state has fundamental frequency only is questionable with preceding flux outside linear region')
     vnonl(i) = ci1 - (e(k) - e(m)) * delta2
     if (nltype(i) .ne. -98) cycle
     if (vnonl(i) .lt. 0.0) curr(i) = -1.0d0
  end do
545 istead = 0
  iprint = -1
  itadd = it + 1
  !
  ! read input card using cimage.
  !
544 call cimage
5758 if (kolbeg .gt. 0) go to 4623
  read (unit = abuff, fmt = 7011) ivolt
  go to 4625
4623 nfrfld = 1
  !  call freone (d11)
  call free (d11)
  ivolt = d11
4625 if (ivolt .le. 1) go to 590
  mdrive = 1
  if (ivolt .eq. 2) go to 550
  iprint = 1
  if (ivolt .eq. 3) go to 553
  if (ivolt .eq. 4) go to 570
  if (ivolt .eq. 5) go to 5600
  kill = 30
  lstat(19) = 550
  lstat(16) = ivolt
  go to 9200
550 if (kolbeg .gt. 0) go to 4633
  read (unit = abuff, fmt = 7011) ll, bus1, a, ci1, ck1, k
7011 format (i2, a6, 3e15.8, 21x, i6)
  go to 4635
4633 k = ivolt
  nright = -1
  nfrfld = 1
  !  call freone (d1)
  call free (d1)
  bus1 = texta6(1)
  nright = 0
  !  call freone (a)
  call free (a)
  !  call freone (ci1)
  call free (ci1)
  !  call freone (ck1)
  call free (ck1)
4635 if (iprint .eq. 0) go to 54157
  if (iprint .eq. 0) write (unit = * , fmt = *) 'over13-cable.    ck1, tpi=', ck1, tpi
  winic = ck1 * tpi
  iprint = 0
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54153) a, ci1, ck1
54153 format ('+node volt init cond.  ', 2e11.3, f7.1)
  steady = ck1 * twopi * deltat
  go to 4463
54157 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54158) a, ci1
54158 format ('+node volt init cond.  ', 2e11.3)
4463 if (k .gt. 0) go to 7005
  do k = 2, ntot
     if (bus(k).eq.bus1) go to 552
  end do
  kill = 31
  lstat(19) = 551
  go to 9200
7005 if (bus(k) .eq. bus1) go to 552
  kill = 31
  lstat(19) = 7005
  go to 9200
552 e(k) = a
  f(k) = ci1
  go to 544
553 if (kolbeg .gt. 0) go to 4643
  read (unit = abuff, fmt = 7020) ll, bus1, bus2, ci1, ck1, a, d2, k
7020 format (i2, 2a6, 4e15.8, i6)
  go to 4645
4643 ll = ivolt
  nright = -1
  nfrfld = 2
  !  call freone (d1)
  call free (d1)
  bus1 = texta6(1)
  bus2 = texta6(2)
  nright = 0
  nfrfld = 1
  !  call freone (ci1)
  call free (ci1)
  !  call freone (ck1)
  call free (ck1)
  !  call freone (a)
  call free (a)
  !  call freone (d2)
  call free (d2)
  !  call freone (d7)
  call free (d7)
  k = d7
4645 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54159) ci1, ck1, a
54159 format ('+linear i.', 4x, 3e12.4)
  if (k .gt. 0) go to 7000
  do k = 1, ibr
     l = kbus(k)
     ll = iabs (l)
     m = iabs (mbus(k))
     if (bus(ll) .ne. bus1) cycle
     if (bus(m) .ne. bus2) cycle
     if (l .lt. 0) go to 575
     go to 560
  end do
  kill = 32
  lstat(19) = 554
  go to 9200
7000 l = kbus(k)
  ll = iabs(l)
  m = iabs (mbus(k))
  if (bus(ll) .ne. bus1 .or. bus(m) .ne. bus2) go to 7001
  if (l .lt. 0) go to 575
  go to 560
7001 kill = 32
  lstat(19) = 7000
  go to 9200
560 n3 = nr(k)
  if (n3 .gt. 0) go to 565
  if (istead .gt. 0) go to 561
  ci(k) = -ci1
  ck(k) = ck1
561 n3 = -n3
  cik(k) = -ci(k) - x(n3) * (e(l) - e(m))
  if (istead .gt. 0) go to 547
  go to 544
565 it2 = iabs (length(k))
  if (istead .gt. 0) go to 568
  cik(k) = ci1
  ci(k) = ck1
  ck(k) = a
568 ci1 = e(l)
  ck1 = e(m)
  volti(1) = -ci1 / 2.0
  voltk(1) = -ck1 / 2.0
  volt(1) = ck1 - ci1
  if (length(k + 1) .eq. -666) voltk(1) = f(m) - f(l)
  n1 = k + it2 - 1
  if (it2 .eq. 1) go to 567
  it1 = k + 1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 111) ci1, ck1, it1, n1, n3
111 format (17x, 'ci1', 17x, 'ck1', 2x, 'it1', 3x, 'n1', 3x, 'n3', /, 2(2x, e18.9), 3i5)
  do i = it1, n1
     l = kbus(i)
     m = iabs (mbus(i))
     if (istead .gt. 0) go to 569
     ! read input card using cimage.
     call cimage
     if (kolbeg .gt. 0) go to 4653
     read (unit = abuff, fmt = 7020) n2, bus1, bus2, cik(i), ci(i), ck(i)
     go to 4655
4653 nfrfld = 1
     !     call freone (d11)
     call free (d11)
     n2 = d11
     nright = -1
     !     call freone (d1)
     call free (d1)
     bus1 = texta6(1)
     !     call freone (d1)
     call free (d1)
     bus2 = texta6(1)
     nright = 0
     !     call frefld (cik(i :))
     call free (cik(i :))
     !     call frefld (ci(i :))
     call free (ci(i :))
     !     call frefld (ck(i :))
     call free (ck(i :))
4655 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54159) cik(i), ci(i), ck(i)
     if (bus(l) .eq. bus1 .and. bus(m) .eq. bus2) go to 569
     kill = 32
     lstat(19) = 569
     go to 9200
569  ci1 = e(l)
     ck1 = e(m)
     nkp = l
     l = i - k + 1
     volti(l) = -ci1 / 2.0d0
     voltk(l) = -ck1 / 2.0d0
     volt(l) = ck1 - ci1
     if (length(k + 1) .eq. -666) voltk(l) = f(m) - f(nkp)
  end do
  if (kodebr(k) .le. 0) go to 567
  call mult(x(n3), volt(1), cik(k), it2, ll1)
  l = k
  do i = 1, it2
     if (iprsup .ge. 3) write (unit = lunit(6), fmt = 38567) i, volt(i), cik(l)
38567 format (/, ' At 38567, init  i(t-deltat) ....   ', i10, 2e25.15)
     l = l + 1
  end do
  go to 28567
567 call mult (tx(n3), volt, cik(k), it2, ll1)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 111) ci1, ck1, it1, n1, n3
  if (length(k + 1) .ne. -666) go to 28567
  ! process next set of modal branches   *   *   *   *   *   *   *   *
  omg = steady / deltat
  call fdint (ikf, isfd, ibf, omg)
28567 continue
  call mult (c(n3), volti, ci(k), it2, ll1)
  call mult (c(n3), voltk, ck(k), it2, ll1)
  k = n1
  if (istead .gt. 0) go to 547
  go to 544
570 if (kolbeg .gt. 0) go to 4663
  read (unit = abuff, fmt = 7020) k, bus1, bus2, ci1, ck1
  go to 4665
4663 k = ivolt
  nright = -1
  nfrfld = 2
  !  call freone (d1)
  call free (d1)
  bus1 = texta6(1)
  bus2 = texta6(2)
  nright = 0
  nfrfld = 1
  !  call freone (ci1)
  call free (ci1)
  !  call freone (ck1)
  call free (ck1)
4665 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54162) ci1, ck1
54162 format ('+Nonlin. branch init cond.', 2x, 2e11.3)
  do k = 1, inonl
     l = nonlk(k)
     m = iabs (nonlm(k))
     if (bus(l) .ne. bus1) cycle
     if (bus(m) .eq. bus2) go to 572
  end do
  kill = 33
  lstat(19) = 571
  go to 9200
572 n1 = nonlad(k)
  n2 = nonle(k)
  if (n2 .gt. 0) go to 544
  n2 = -n2
  if (nltype(k) .ne. -96) go to 7361
  vnonl(k) = ci1 - (e(l) -e(m)) * delta2
  n17 = nonlad(k)
  n18 = n17 + 1
  n19 = n17 + 2
  n20 = n17 + 3
  cchar(n20) = ck1
  curr(k) = cchar(n20)
  ! read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 7018) n15, n16, (vchar(ipp), ipp = n17, n20)
7018 format (2i10, 4e15.8)
  cchar(n18) = n15
  cchar(n19) = n16
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 7017) n15, n16, vchar(n17), vchar(n17 + 1)
7017 format ('+  type-96.', 2i4, 2e15.6)
  gslope(n19) = cchar(n18)
  gslope(n20) = cchar(n19)
  n16 = cchar(n19)
  n15 = n16 + cchar(n17) + 1
  if (cchar(n18) .ne. 1) go to 4667
  d13 = gslope(n16) * (1.0 - vchar(n17))
  d13 = 1.0 / d13
  d14 = gslope(n16) * vchar(n18) - gslope(n15)
  d14 = d14 * d13
  go to 4668
4667 d13 = gslope(n16) * (1.0 + vchar(n17))
  d13 = 1.0 / d13
  d14 = gslope(n15) - gslope(n16) * vchar(n18)
  d14 = d14 * d13
4668 gslope(n18) = delta2 / d13
  gslope(n17) = vchar(n20)  -  gslope(n18) * ( e(l) - e(m) )
  n18 = n17 + 4
  n19 = n17 + 5
  ! read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 7026) n20, vchar(n18), vchar(n19), gslope(n18), gslope(n19)
7026 format (i10, 4e15.8)
  cchar(n18) = n20
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 7025) n20, vchar(n18), vchar(n19)
7025 format ('+  type-96.', i8, 2e14.6)
  go to 544
7361 do i = n1, n2
     if (cchar(i) .gt. ci1) go to 574
  end do
  kill = 34
  lstat(19) = 573
  flstat(16) = ci1
  flstat(15) = cchar(n2)
  lstat(14) = n2
  lstat(13) = k
  go to 9200
574 n1 = i - 1
  vr1 = vchar(n1)
  curr1 = cchar(n1)
  curr(k) = ci1
  ci1 = vr1 + (vchar(i) - vr1) * (ci1 - curr1) / (cchar(i) - curr1)
  if (ck1 .ne. 0.) ci1 = ck1
  vnonl(k) = ci1 - (e(l) - e(m)) * delta2
  go to 544
  ! 3456789012345678901234567890123456789012345678901234567890123456789012
575 if (imodel(k) .ne. -3) go to 3300
  !
  !
3300 n1 = -kbus(k)
  n2 = iabs (mbus(k))
  if (imodel(k) .eq. -4) go to 7550
  ! if (kodsem(k) .ne. 0
  ! 1 .and. imodel(k) .ne. -2) go to 5750
  ! for semlyen or cas. pi
  if (kodsem(k) .ne. 0 .and. imodel(k) .ge. 0) go to 5750
7550 n4 = length(k)
  if (n4 .gt. 0) n4 = 1
  isd = itadd
  if (imodel(k) .eq. -2) go to 60001
  if (imodel(k) .eq. -4) go to 60006
  if (istead.eq.0) go to 578
  curr1 = tr(itadd)
  curi1 = tx(itadd)
  curr2 = r(itadd)
  curi2 = c(itadd)
  itadd = itadd + 1
  go to 579
  !l
  !     initialization of history vectors for marti's  f.d. lines
  !     non-zero initial conditions
  !
  !
60006 it2 = iabs (n4)
  aph = it2
  nl = litype(k)
  if (istead .eq. 0) go to 60005
  n5 = nl - 1
  iy = k
  iz = itadd
  !
  ! call incdrv ( k, nphs, deltat, o
  !
  n7 = iy
  n8 = iz
  steady = omega * deltat
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = *) 'steady = omega * deltat', omega, deltat, steady
  wdt = -deltat * omega
  !
  ! evaluate: vm(to), vm(to-dt), vk(to), vk(
  !
  if (istead .eq. 0) omegs1 = 0.0
  n1 = kodsem(k)
  nrz = cki(k)
  nra = ckkjm(k)
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 8833) n1, nra, nrz
8833 format (1x, i6, 1x, i6, 1x, i6, ' i       vkdt(i)       vmdt(i)       nn5        n6')
  ivbr = k
  do i = 1, it2
     ns1 = kodsem(ivbr)
     nrz = cki(ivbr)
     nra = ckkjm(ivbr)
     n1 = -kbus(ivbr)
     n2 = iabs (mbus(ivbr))
     nk1 = ns1 + 5 * nra + 5 * nrz + 4 + 1
     nn17 = sconst(nk1)
     nn5 = ns1 + 5 * nrz + 5 * nra + 4 + 1 + 1 + nn17
     phf = atan2z (f(n2), e(n2))
     hf = dsqrt (e(n2) ** 2 + f(n2) ** 2)
     sconst(nn5) = hf * dcos (wdt + phf)
     n6 = nn5 + 1
     phf = atan2z (f(n1), e(n1))
     hf = dsqrt (e(n1) ** 2 + f(n1) ** 2)
     sconst(n6) = hf * dcos (wdt + phf)
     if (iprsup .gt. 0) write (unit = lunit(6), fmt = 5333) i, sconst(nn5), sconst(n6), nn5, n6
5333 format (9x, i5, 1x, 2e14.5, 1x, 1x, i6, 1x, i6)
     ivbr = ivbr + 1
  end do
  ! single phase and constant q
  ! evaluate im' = q**-1 * im
  ! ik' = q**-1 * ik
  ! 125   call qinv(qr,qi,qk0,w,nphs,iadrsq,ntermq) !q matrix and converts
  !
  ioff1 = 0
  ioff2 = ioff1 + isecti
  ioff3 = ioff2 + isecti
  ioff4 = ioff3 + isecti
  ioff5 = ioff4 + isecti
  ioff6 = ioff5 + isecti
  ioff7 = ioff6 + isecti
  ioff8 = ioff7 + isecti
  if (ioff8 .le. lymat) go to 5337
  write (unit = lunit(6), fmt = 5338) lymat, ioff8
5338 format ( ' In over13, new Marti line solution logic overflowed storage list no. 5: lymat =', i5, ' needed space here ioff8=', i5, '.', /, &
       ' Execution is aborted, and redimension with larger value of list no. 5 is required.')
  stop
5337 koff25 = koff24 + 288
  koff11 = koff25 + 288
  if (koff11 .lt. lhist) go to 5339
  write (unit = lunit(6), fmt = 5340) lhist, koff11
5340 format ( ' In over13, new Marti line solution logic overflowed storage list no. 22: lhist =', i5,' needed space here koff11=',i5, '.', /, &
       ' Execution is aborted, and redimension with larger value of list no. 22 is required.')
  stop
5339 nq1 = infdli(inoff2 + k)
  nphs2 = it2 * it2
  kqvq = infdli(inoff1 + k)
  kqv = litype(k)
  do iq = 1, nphs2
     nteq = wk1(koff20 + kqvq)
     qfd(kqv) = sconst(nq1)
     sfd(kqv) = 0.d0
     if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5692)
5692 format (1x, 'kqv nteq    nq1    nq2    nq3   sconst(nq1)   sconst(nq2)   sconst(nq3)       qr(kqv)')
     do lq = 1, nteq
        nq2 = nq1 + lq
        nq3 = nq2 + nteq
        if (absz (sconst(nq2)) .ge. 1.e+13) go to 3000
        d = omega ** 2 + sconst(nq3) ** 2
        qfd(kqv) = qfd(kqv) + sconst(nq2) * sconst(nq3) / d
        sfd(kqv) = sfd(kqv) - sconst(nq2) * omega / d
        go to 3001
3000    jkl = jkl + 1
        if (jkl .eq. 2) go to 3002
        akr = sconst(nq2) / 1.e+15
        aki = sconst(nq2 + 1) / 1.e+15
        apr = sconst(nq3)   / 1.e+15
        api = sconst(nq3 + 1) / 1.e+15
        dqq3 = apr ** 2 + (omega + api) ** 2
        ddd3 = (akr * apr + aki * (omega + api)) / dqq3
        ddd4 = (aki * apr - akr * (omega + api)) / dqq3
        xk(kqv) = xk(kqv) + 2 * ddd3
        if (jkl .eq. 1) go to 3001
3002    jkl = 0
3001    if (iprsup .ge. 2) write (unit = lunit(6), fmt = 1333) kqv, nteq, nq1, nq2, nq3, sconst(nq1), sconst(nq2), sconst(nq3), qfd(kqv)
1333    format (1x, i6, 3x, i6, 2x, i5, 2x, i5, 2x, i5, 4e14.5)
        nq2 = nq2 + 1
        nq3 = nq3 + 1
     end do
     nq1 = nq1 + 2 * nteq + 1
     kqv = kqv + 1
     kqvq = kqvq + 1
  end do
  j1 = litype(k)
  jadd = j - 1 + nphs2
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = *) 'Following are qr(k) by rows____________'
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 4515) (qfd(j), j = j1, jadd)
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = *) 'Following are qi(k) by rows____________'
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 4516) (sfd(j), j = j1, jadd)
4515 format ('+ qr-i:', 3e14.5)
4516 format ('+ qi-i:', 3e14.5)
  icbr = itadd
  do i = 1, it2
     r(icbr) = -r(icbr)
     c(icbr) = -c(icbr)
     if (iprcbl .gt. 0) write (unit = lunit(6), fmt = *) '  aimr(i)=-aimr(i) , aimi(i)=-aimi(i) ', r(icbr), c(icbr)
     icbr = icbr + 1
  end do
  !
  !        invert matrix q
  !
  !     kq = iadrsu(k)
  !      kq = infdli(inoff1+k)
  kq = litype(k)
  !      call cinvl(qfd(kq),sfd(kq),it2)                            !inver
  ll = 1
  nphs2 = it2 * it2
  !   wsm & thl  comment out following do loop on 14 sept 88.   it
  !              seems to us that initialization is not required.
  !     do 7003 ifd = 1, 72
  !     wk1(koff24+ll) = 0.0
  !     wk1(koff24+ll+1)=0.0
  !     ll=ll+2                              !skip two celles for next fre
  !7003 continue
  ll = 1
  nphs2 = it2 * it2
  do ifd = 1, nphs2
     wk1(koff24 + ll) = qfd(kq)
     wk1(koff24 + ll + 1) = sfd(kq)
     ll = ll + 2
     kq = kq + 1
  end do
  call cominv (wk1(koff24 + 1 :), wk1(koff25 + 1 :), it2, 60.0d0)
  t = 0.0000000d0
  kq = infdli(inoff1 + k)
  kq = litype(k)
  ll = 1
  do ifd = 1, nphs2
     qfd(kq) = wk1(koff25 + ll)
     sfd(kq) = wk1(koff25 + ll + 1)
     ll = ll + 2
     kq = kq + 1
  end do
  kmv = k
  itam = itadd
  ! kq = infdli(inoff1+k)
  kq = litype(k)
  ! call mvecc(qfd(kq),sfd(kq),r(itadd),c(itadd),it2,
  ! 1    w1( ioff1 + k ), w1( ioff2 + k ) )         !modal current im'=
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 4933)
4933 format ('  i    j   it2', '    qfd(kq)    sfd(kq)   r(itadd)   c(itadd)w1(ioff1+k)w1(ioff2+k)  tr(itadd)  tx(itadd)w1(ioff7+k)w1(ioff8+k)')
  do im = 1, it2
     w1(ioff1 + kmv - 1 + im) = 0.0d0
     w1(ioff2 + kmv - 1 + im) = 0.0d0
     w1(ioff7 + kmv - 1 + im) = 0.0d0
     w1(ioff8 + kmv - 1 + im) = 0.0d0
  end do
  do jm = 1, it2
     do im = 1, it2
        w1(ioff1 + kmv - 1 + im) = w1(ioff1 + kmv - 1 + im) + qfd(kq) * r(itam) - sfd(kq) * c(itam)
        w1(ioff2 + kmv - 1 + im) = w1(ioff2 + kmv - 1 + im) + qfd(kq) * c(itam) + sfd(kq) * r(itam)
        w1(ioff7 + kmv - 1 + im) = w1(ioff7 + kmv - 1 + im) + qfd(kq) * tr(itam) - sfd(kq) * tx(itam)
        w1(ioff8 + kmv - 1 + im) = w1(ioff8 + kmv - 1 + im) + qfd(kq) * tx(itam) + sfd(kq) * tr(itam)
        if (iprsup .gt. 0) write (unit = lunit(6), fmt = 4333) im, jm, it2, qfd(kq), sfd(kq), r(itam), c(itam), w1(ioff1 + kmv - 1 + im), w1(ioff2 + kmv - 1 + im), &
             tr(itam), tx(itam), w1(ioff7 + kmv - 1 + im), w1(ioff8 + kmv - 1 + im)
4333    format (1x, i2, 3x, i2, 3x, i3, 10e11.4)
        kq = kq + 1
     end do
     itam = itam + 1
  end do
  ! call mvecc(qfd(kq),sfd(kq),tr(itadd),tx(itadd),it2,
  ! 1    w1( ioff7 + k ), w1( ioff8 + k ) )         !modal current ik'=
  icbr = itadd
  do i = 1, nphs
     r(icbr) = -r(icbr)
     c(icbr) = -c(icbr)
     icbr = icbr + 1
  end do
  !
  ! evaluate:   im'(to-dt)  ,  ik'(to-dt)
  !
  kadt = k
  do idt = 1, it2
     n1 = kodsem(kadt)
     nrz = cki(kadt)
     nra = ckkjm(kadt)
     nk1 = n1 + 5*nra + 5*nrz + 4 + 1
     nn17 = sconst(nk1)
     nn5 = n1 + 5*nrz + 5*nra + 4 + 2 + 1 + 1 + nn17
     n6 = nn5 + 1
     phf = atan2z (w1(ioff2 + kadt), w1(ioff1 + kadt))
     hf = dsqrt (w1(ioff2 + kadt) ** 2 + w1(ioff1 + kadt) ** 2)
     sconst(nn5) = hf * dcos (wdt + phf)
     phf = atan2z (w1(ioff8 + kadt), w1(ioff7 + kadt))
     hf = dsqrt (w1(ioff7 + kadt) ** 2 + w1(ioff8 + kadt) ** 2)
     sconst(n6) = hf * dcos (wdt + phf)
     if (iprsup .gt. 0) write (unit = lunit(6), fmt = *) '   aim5dt,      aik5dt ', sconst(nn5), sconst(n6)
     kadt = kadt + 1
  end do
  ! evaluate    vm'    vk'
  !             vmj'   vkj'
  ka = k
  do jh = 1, it2
     n4 = indhst(ka)
     cnvhst(n4 + 5) = 0.
     cnvhst(n4 + 6) = 0.
     cnvhst(n4 + 7) = 0.
     cnvhst(n4 + 8) = 0.
     ka = ka + 1
  end do
  kvd = k
  kv = infdli(inoff1 + k)
  nq1 = infdli(inoff2 + k)
  !      lj = kv                              ! 1st pole of qii for the br
  lj = nr(k)
  !      if ( kv .gt. 1) lj = ( kv-1 ) * 2 + 1
  !      kiline = lpast
  !      lj = kiline + lj
  !      nr(k) = lj
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 5467) k, ibr, ntot
5467 format (' Begin qt * vk, m = vk, m in mode domain        k     ibr    ntot', /, 23x, 3i8)
  do jv = 1, it2
     n4 = indhst(kvd)
     do iv = 1, it2
        n1 = -kbus(n7)
        n2 = iabs (mbus(n7))
        nteq = wk1(koff20 + kv)
        sumkr = sconst(nq1) * e(n1)
        sumki = sconst(nq1) * f(n1)
        summr = sconst(nq1) * e(n2)
        summi = sconst(nq1) * f(n2)
        if (iprsup .gt. 0) write (unit = lunit(6), fmt = 4839)
4839    format (' iq  nteq    nq1    nq2    nq3     s(nq1)     s(nq2)     s(nq3)      e(n1)      e(n2)      hk2()      hk3()      sumkr      summr')
        jkl = 0
        do iq = 1, nteq
           nq2 = nq1 + iq
           nq3 = nq2 + nteq
           if (absz (sconst(nq2)) .ge. 1.e+13) go to 3003
           d = omega ** 2 + sconst(nq3) ** 2
           ar = sconst(nq2) * sconst(nq3) / d
           ai = - sconst(nq2) * omega / d
           go to 3004
3003       jkl = jkl + 1
           if (jkl .eq. 2) go to 3005
           akr = sconst(nq2) / 1.e+15
           aki = sconst(nq2 + 1) / 1.e+15
           apr = sconst(nq3)   / 1.e+15
           api = sconst(nq3 + 1) / 1.e+15
           dqq3 = apr ** 2 + (omega + api) ** 2
           ar = 2 * (akr * apr + aki * (omega + api)) / dqq3
           ai = 0.
3004       w1(ioff3 + iq) = ar * e(n1) - ai * f(n1)
           w1(ioff4 + iq) = ar * f(n1) + ai * e(n1)
           w1(ioff5 + iq) = ar * e(n2) - ai * f(n2)
           w1(ioff6 + iq) = ar * f(n2) + ai * e(n2)
           sumkr = sumkr + w1(ioff3 + iq)
           sumki = sumki + w1(ioff4 + iq)
           summr = summr + w1(ioff5 + iq)
           summi = summi + w1(ioff6 + iq)
           if (jkl .eq. 1) go to 3006
3005       jkl = 0
3006       if (iprsup .gt. 0) write (unit = lunit(6), fmt = 9333) iq, nteq, nq1, nq2, nq3, sconst(nq1), sconst(nq2), sconst(nq3), e(n1), &
                e(n2), w1(ioff3 + iq), w1(ioff4 + iq), sumkr, summr
9333       format (1x, i2, 3x, i3, 2x, i5, 2x, i5, 2x, i5, 9e11.4)
           nq2 = nq2 + 1
           nq3 = nq3 + 1
        end do
        nq1 = nq1 + 2 * nteq + 1
        if (iprsup .gt. 0) write (unit = lunit(6), fmt = 5433) iv, jv, cnvhst(n4 + 5), cnvhst(n4 + 7)
        cnvhst(n4 + 7) = cnvhst(n4 + 7) + sumkr
        cnvhst(n4 + 8) = cnvhst(n4 + 8) + sumki
        cnvhst(n4 + 5) = cnvhst(n4 + 5) + summr
        cnvhst(n4 + 6) = cnvhst(n4 + 6) + summi
        if (iprsup .gt. 0) write (unit = lunit(6), fmt = 5433) kvd, jv, cnvhst(n4 + 5), cnvhst(n4 + 7)
5433    format (' kvd, jv, vmr5(kvd), vkr5(kvd)=', 1x, i5, 1x, i5, 2x, 2e14.5)
        !
        !        evaluate vmj'(to-dt)
        !
        !     lj = iadrsdt  !keep parallel pointer as tranformation matrix for v
        lj1 = lj + nteq
        if (iprsup .gt. 0) write (unit = lunit(6), fmt = 4733)
4733    format ('   lj   lj1  nteq    vmj5dt(lj)    vkj5dt(lj)')
        do lqv = 1, nteq
           phf = atan2z (w1(ioff6 + lqv), w1(ioff5+lqv))
           hf = sqrtz (w1(ioff5 + lqv) ** 2 + w1(ioff6 + lqv) ** 2)
           xm(lj) = hf * cosz (wdt + phf)
           phf = atan2z (w1(ioff4 + lqv), w1(ioff3 + lqv))
           hf = sqrtz (w1(ioff3 + lqv) ** 2 + w1(ioff4 + lqv) ** 2)
           xm(lj1) = hf * cosz (wdt + phf)
           if (iprsup .ge. 1) write (unit = lunit(6), fmt = 9733) lj, lj1, nteq, xm(lj), xm(lj1)
9733       format (2x, i8, 2x, i8, 2x, i4, 2e14.3)
           lj = lj + 1
           lj1 = lj1 + 1
        end do
        lj = lj + nteq
        n7 = n7 + 1
        if (iv .eq. it2) n7 = n7 - it2
        kv = kv + 1
     end do
     phf = atan2z (cnvhst(n4 + 6), cnvhst(n4 + 5))
     hf = sqrtz (cnvhst(n4 + 5) ** 2 + cnvhst(n4 + 6) ** 2)
     wk1(koff1 + kvd) = hf * cosz (wdt + phf)
     phf = atan2z (cnvhst(n4 + 8), cnvhst(n4 + 7))
     hf = sqrtz (cnvhst(n4 + 7) ** 2 + cnvhst(n4 + 8) ** 2)
     wk1(koff2 + kvd) = hf * cosz (wdt + phf)
     !      write (*,*) 'vm5dt(kvd), vk5dt(kvd) =,',
     !     1            wk1(koff1+kvd), wk1(koff2+kvd)
     kvd = kvd + 1
  end do
  !
  !        evaluate:    gmj = ycj' * vm'   gkj' = ycj' * vk'
  !                     fk = gk + ik'
  !                     bm = gm - im'
  ky = k
  do iv = 1, it2
     !     call fj(vkr5(iv),vki5(iv),xr,xi,gkr,gki
     !     1       ,vmr5(iv),vmi5(iv),eja,ejy,gmr,gmi
     !     1       ,ypi(iadrs),yki(iadrs),w,ntey,0.d0,yk0(iv))
     n1 = kodsem(ky)
     n4 = indhst(ky)
     sumkr = sconst(n1) * cnvhst(n4 + 7)
     sumki = sconst(n1) * cnvhst(n4 + 8)
     summr = sconst(n1) * cnvhst(n4 + 5)
     summi = sconst(n1) * cnvhst(n4 + 6)
     nrz = cki(ky)
     nra = ckkjm(ky)
     jkl = 0
     do ii = 1, nrz
        n2 = n1 + ii
        n3 = n2 + nrz
        if (absz (sconst(n2)) .ge. 1.0e+13) go to 5326
        d1 = sconst(n3) ** 2 + omega ** 2
        czire = sconst(n2) * sconst(n3) / d1
        cziim = - sconst(n2) * omega / d1
        go to 5328
5326    jkl = jkl + 1
        if (jkl .eq. 2) go to 5327
        akr = sconst(n2) / 1.e+15
        aki = sconst(n2 + 1) / 1.e+15
        apr = sconst(n3) / 1.e+15
        api = sconst(n3 + 1) / 1.e+15
        djk3 = apr ** 2 + (omega + api) ** 2
        ddd3 = (akr * apr + aki * (omega + api)) / djk3
        !     ddd4 = ( aki * apr - akr * ( omega + api ) ) / djk3
        czire = 2 * ddd3
        cziim = 0.0d0
5328    continue
        w1(ioff3 + ii) = czire*cnvhst(n4 + 7) - cziim * cnvhst(n4 + 8)
        w1(ioff4 + ii) = czire*cnvhst(n4 + 8) + cziim * cnvhst(n4 + 7)
        w1(ioff5 + ii) = czire*cnvhst(n4 + 5) - cziim * cnvhst(n4 + 6)
        w1(ioff6 + ii) = czire*cnvhst(n4 + 6) + cziim * cnvhst(n4 + 5)
        sumkr = sumkr + w1(ioff3 + ii)
        sumki = sumki + w1(ioff4 + ii)
        summr = summr + w1(ioff5 + ii)
        summi = summi + w1(ioff6 + ii)
        if (iprsup .gt. 0) write (unit = lunit(6), fmt = 5343) n1, ii, n2, n3, sconst(n2), sconst(n3)
5343    format (1x, 'n1, ii, n2, n3, ypi, yki=', 1x, i6, 2x, i6, 3x, i6, 3x, i6, 3x, 2e14.5)
        if (jkl .eq. 1) cycle
5327    jkl = 0
     end do
     if (iprsup .gt. 0) write (unit = lunit(6), fmt = *) 'The voltage vm & vk. summr, summi, sumkr, sumki', summr, summi, sumkr, sumki
     !
     !        evaluate:   gmj(to-dt)
     !
     if (iprsup .gt. 0) write (unit = lunit(6), fmt = 5443)
5443 format ('ky kodsem(ky) cki(ky) ckkjm(ky)  nn5   n6    gmjdt(nn5)     gkjdt(n6)')
     nk1 = n1 + 5 * nra + 5 * nrz + 4 + 1
     nn17 = sconst(nk1)
     jkl = 0
     do lg = 1, nrz
        nn5 = n1 + 5 * nrz + 5 * nra + 4 + 2 + 2 + lg + 1 + nn17
        n6 = nn5 + nrz
        n2 = n1 + lg
        if (absz (sconst(n2)) .ge. 1.0e+13) jkl = jkl + 1
        if (jkl .eq. 2) go to 5829
        phf = atan2z (w1(ioff6 + lg), w1(ioff5 + lg))
        hf = dsqrt (w1(ioff5 + lg) ** 2 + w1(ioff6 + lg) ** 2)
        sconst(nn5) = hf * dcos (wdt + phf)
        phf = atan2z (w1(ioff4 + lg), w1(ioff3 + lg))
        hf = dsqrt (w1(ioff3 + lg) ** 2 + w1(ioff4 + lg) ** 2)
        sconst(n6) = hf * dcos (wdt + phf)
        if (iprsup .gt. 0) write (unit = lunit(6), fmt = 5943) ky, kodsem(ky), nrz, nra, nn5, n6, sconst(nn5), sconst(n6)
5943    format (1x, i5, 8x, i6, 5x, i6, 7x, i6, 2x, i6, 2x, i6, 2e14.5)
        if (jkl .eq. 1) cycle
5829    jkl = 0
     end do
     !
     !        evaluate:   bm'
     !
     cnvhst(n4 + 9)  = summr - w1(ioff1 + ky)
     cnvhst(n4 + 10) = summi - w1(ioff2 + ky)
     cnvhst(n4 + 11) = sumkr + w1(ioff7 + ky)
     cnvhst(n4 + 12) = sumki + w1(ioff8 + ky)
     if (iprsup .gt. 0) write (unit = lunit(6), fmt = 5444) ky, cnvhst(n4 + 9), cnvhst(n4 + 10), cnvhst(n4 + 11), cnvhst(n4 + 12)
5444 format ('ky, bmr5(ky), bmi5, fkr5, fki5=', 1x, i2,2x, 4e14.5)
     ky = ky + 1
  end do
  !
  !        evaluate:     imj(to-dt)  ikj(to-dt)
  !
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 4576) k, ibr, ntot
4576 format (' Begin qt * ik, m = ik, m in phase domain       k     ibr    ntot', /, 23x, 3i8)
  nq1 = infdli(inoff2 + k)
  ka = k
  kq = infdli(inoff1 + k)
  kqk = infdli(inoff1 + ka)
  kv = infdli(inoff1 + k)
  !      lj = kv                              ! 1st pole of qii for the br
  !      if ( kv .gt. 1) lj = ( kv-1 ) * 2 + 1
  !      kiline = lpast                                  !next cell is fre
  !      lj = kiline + lj
  !      nr(k) = lj
  lj = nr(k)
  do jqa = 1, it2
     do iqa = 1, it2
        nteq = wk1(koff20 + kq)
        sumkr = sconst(nq1) * w1(ioff7 + ka)
        sumki = sconst(nq1) * w1(ioff8 + ka)
        summr = sconst(nq1) * w1(ioff1 + ka)
        summi = sconst(nq1) * w1(ioff2 + ka)
        if (iprsup .gt. 0) write (unit = lunit(6), fmt = 4433)
4433    format ('jqa nteq    nq1    nq2    nq3 snt(nq1) snt(nq2) sst(nq3) aimr(ka) aimi(ka)  hm1(ka)  hk1(ka)    sumkr    sumki    summr    summi')
        do iq = 1, nteq
           nq2 = nq1 + iq
           nq3 = nq2 + nteq
           if (absz (sconst(nq2)) .ge. 1.e+13) go to 3007
           d = omega ** 2 + sconst(nq3) ** 2
           ar = sconst(nq2) * sconst(nq3) / d
           ai = -sconst(nq2) * omega / d
           go to 3008
3007       jkl = jkl + 1
           if (jkl .eq. 2) go to 3009
           akr = sconst(nq2) / 1.e+15
           aki = sconst(nq2 + 1) / 1.e+15
           apr = sconst(nq3) / 1.e+15
           api = sconst(nq3 + 1) / 1.e+15
           dqq3 = apr ** 2 + (omega + api) ** 2
           ar = 2 * (akr * apr + aki * (omega + api)) / dqq3
           ai = 0.
3008       w1(ioff3 + iq) = ar * w1(ioff7 + ka) - ai * w1(ioff8 + ka)
           w1(ioff4 + iq) = ar * w1(ioff8 + ka) + ai * w1(ioff7 + ka)
           w1(ioff5 + iq) = ar * w1(ioff1 + ka) - ai * w1(ioff2 + ka)
           w1(ioff6 + iq) = ar * w1(ioff2 + ka) + ai * w1(ioff1 + ka)
           sumkr = sumkr + w1(ioff3 + iq)
           sumki = sumki + w1(ioff4 + iq)
           summr = summr + w1(ioff5 + iq)
           summi = summi + w1(ioff6 + iq)
           if (jkl .eq. 1) go to 3010
3009       jkl = 0
3010       if (iprsup .gt. 0) write (unit = lunit(6), fmt = 4773) jqa, nteq, nq1, nq2, nq3, sconst(nq1), sconst(nq2), sconst(nq3), w1(ioff7 + ka), w1(ioff8 + ka), &
                w1(ioff1 + ka), w1(ioff2 + ka), sumkr, sumki, summr, summi
4773       format (1x, i2, 3x, i2, 2x, i5, 2x, i5, 2x, i5, 11e9.3)
           nq2 = nq2 + 1
           nq3 = nq3 + 1
        end do
        nq1 = nq1 + 2 * nteq + 1
        nteq = wk1(koff20 + kq)
        lj1 = lj + nteq
        do la = 1, nteq
           phf = atan2z (w1(ioff4 + la), w1(ioff3 + la))
           hf = dsqrt (w1(ioff3 + la) **2  + w1(ioff4 + la) ** 2)
           xk(lj1) = hf * dcos (wdt + phf)
           phf = atan2z (w1(ioff6 + la), w1(ioff5 + la))
           hf = dsqrt (w1(ioff5 + la) ** 2 + w1(ioff6 + la) ** 2)
           xk(lj) = hf * dcos (wdt + phf)
           lj = lj + 1
           lj1 = lj1 + 1
        end do
        kq = kq + 1
        lj = lj + nteq
     end do
     ka = ka + 1
  end do
  !
  !        evaluate:   fmj' = a1j * fk'  => fmj'(to), fm'(to)
  !                    bkj' = a1j * bm'  => bkj'(to), bk'(to)
  !
  !      call fj(fkr5(i),fki5(i),fmj5(iadrs),fmji5(iadrs),fm5(i),sumki
  !     1       ,bmr5(i),bmi5(i),bkj5(iadrs),bkji5(iadrs),bk5(i),summi
  !     2       ,api(iadrs),aki(iadrs),w,ntea,tau(i),0.d0)
  ka = k
  iadrs = 1
  jkl = 0
  do ifa=1, it2
     n4 = indhst(ka)
     taua = cnvhst(n4)
     ejwtr = dcos(omega * taua)
     ejwti = -dsin(omega * taua)
     sumkr = 0.d0
     sumki = 0.d0
     summr = 0.d0
     summi = 0.d0
     n1 = kodsem(ka)
     nrz = cki(ka)
     nra = ckkjm(ka)
     if (iprsup .gt. 0) write (unit = lunit(6), fmt = 4833)
4833 format (' nk7  nra  api(iadrs)       czire       cziim              aki(iadrs)    fkr5(ka)    fki5(ka)', /, &
          '    bmr5(ka)    bmi5(ka) fmj5(iadrs) bkj5(iadrs)     fm5(ii)     bk5(ii)')
     do ii = 1, nra
        n2 = n1 + 2 * nrz + ii
        n3 = n2 + nra
        nk1 = n1 + 5 * nra + 5*nrz + 4 + 1
        nn17 = sconst(nk1)
        nn5 = n1 + 7 * nrz + 5*nra + 8 + ii + 1 + nn17
        nn6 = nn5 + nra
        if (abs (sconst(n2)) .ge. 1.e13) go to 4338
        d1 = sconst(n3) ** 2 + omega ** 2
        czire = sconst(n2) * sconst(n3) / d1
        cziim = - sconst(n2) * omega / d1
        go to 4337
4338    jkl = jkl + 1
        if (jkl .eq. 2) go to 5486
        nk7 = n1 + 7 * nra + 7*nrz + 9 + nn17 + ii
        nk8 = nk7 + nra
        akr = sconst(n2) / 1.e+15
        aki = sconst(n2+1) / 1.e+15
        apr = sconst(n3) / 1.e+15
        api = sconst(n3+1) / 1.e+15
        dkr = apr * akr - api * aki
        dki = apr * aki + api * akr
        akr = dkr
        aki = dki
        djk3 = apr ** 2 + (omega + api) ** 2
        ddd3 = (akr * apr + aki * (omega + api)) / djk3
        ddd4 = (aki * apr - akr * (omega + api)) / djk3
        djk4 = apr ** 2 + (omega - api) ** 2
        ccc3 = (akr * apr - aki * (omega - api)) / djk4
        ccc4 = (akr * (omega - api) + aki * apr) / djk4
        czire = ddd3 + ccc3
        cziim = ddd4 - ccc4
        czire = ddd3
        cziim = ddd4
4337    continue
        w1(ioff3 + ii) = czire * cnvhst(n4 + 11) - cziim * cnvhst(n4 + 12)
        w1(ioff4 + ii) = czire * cnvhst(n4 + 12) + cziim * cnvhst(n4 + 11)
        w1(ioff5 + ii) = czire * cnvhst(n4 + 9) - cziim * cnvhst(n4 + 10)
        w1(ioff6 + ii) = czire * cnvhst(n4 + 10) + cziim * cnvhst(n4 + 9)
        ar = w1(ioff3 + ii) * ejwtr - w1(ioff4 + ii) * ejwti
        ai = w1(ioff3 + ii) * ejwti + w1(ioff4 + ii) * ejwtr
        w1(ioff3 + ii) = ar
        w1(ioff4 + ii) = ai
        sconst(nn5) = ar
        ar = w1(ioff5 + ii) * ejwtr - w1(ioff6 + ii) * ejwti
        ai = w1(ioff5 + ii) * ejwti + w1(ioff6 + ii) * ejwtr
        w1(ioff5 + ii) = ar
        w1(ioff6 + ii) = ai
        sconst(nn6) = ar
        sumkr = sumkr + w1(ioff3 + ii)
        sumki = sumki + w1(ioff4 + ii)
        summr = summr + w1(ioff5 + ii)
        summi = summi + w1(ioff6 + ii)
        if (iprsup .gt. 0) write (unit = lunit(6), fmt = 4334) nk7, nra, sconst(n3), sconst(n2), czire, cziim, cnvhst(n4 + 11), cnvhst(n4 + 12), &
             cnvhst(n4 + 9), cnvhst(n4 + 10), sconst(nn5), sconst(nn6), sumkr, summr
4334    format (1x, i3, 2x, i3, 6e16.7, /, 9x, 6e16.7)
        if (jkl .eq. 1) cycle
5486    jkl = 0
     end do
     wk1(koff3 + ka) = sumkr
     wk1(koff4 + ka) = summr
     ka = ka + 1
     if (iprsup .gt. 0) write (unit = lunit(6), fmt = *) ' fmj = a1j * fk; bkj = a1j * bm'
     if (iprsup .gt. 0) write (unit = lunit(6), fmt = *)' sumkr, jsumki, summr, jsummi ', sumkr,  sumki, summr,  summi
  end do
  !
  !        fill up history vectors: bm'(history)
  !                                 fk'(history)
  !
  ka = k
  if (kmode .eq. 5) koff11 = kcbl
  if (koff11 .lt. lhist) go to 1114
  write (unit = lunit(6), fmt = 5340) lhist, koff11
  write (unit = lunit(6), fmt = *) ' Nearby statement number is 1114. '
  stop
1114 do ifa = 1, it2
     nn4 = indhst(ka)
     taui = cnvhst(nn4)
     ntaui = taui / deltat + 1
     !      call fillup(cnvhst(nn4+11),cnvhst(nn4+12),fkh5(iadrs)
     !     1           ,cnvhst(nn4+9), cnvhst(nn4+10),bmh5(iadrs)
     !     2           ,omega,wdt,ntaui,taui)
     !      call fillup(fkr5(ifa),fki5(ifa),fkh5(iadrs)
     !     1           ,bmr5(ifa),bmi5(ifa),bmh5(iadrs)
     !     2           ,w,wdt,ntaui,taui)
     n1    =  ntaui - 1
     amagk = dsqrt (cnvhst(nn4 + 11) ** 2 + cnvhst(nn4 + 12) ** 2)
     amagm = dsqrt (cnvhst(nn4 + 9) ** 2 + cnvhst(nn4 + 10) ** 2)
     angk  = atan2z (cnvhst(nn4 + 12), cnvhst(nn4 + 11))
     angm  = atan2z (cnvhst(nn4 + 10), cnvhst(nn4 + 9))
     wd = wdt
     koff11 = koff11 + 0
     koff12 = koff11 + ntaui
     do i = 1, n1
        wk1(koff11 + i) = amagk * dcos (wd + angk)
        wk1(koff12 + i) = amagm * dcos (wd + angm)
        wd = wd + wdt
     end do
     wk1(koff11+ntaui) = amagk * dcos (-omega * taui + angk)
     wk1(koff12+ntaui) = amagm * dcos (-omega * taui + angm)
     if (iprsup .gt. 0) write (unit = lunit(6), fmt = 80150) ntaui
80150 format (/, ' Forward functions fkhist and bmhist; positions     1  to', i6)
     if (iprsup .gt. 0) write (unit = 6, fmt = 80145) (wk1(koff11 + i), i = 1, ntaui), (wk1(koff12 + i), i = 1, ntaui)
80145 format (1x, 10e12.5)
     if (iprcbl .gt. 0) write (unit = lunit(6), fmt = *) 'ka, fkr5(ka), fki5(ka), fkh5(iadrs)=', ka, cnvhst(nn4 + 11), cnvhst(nn4 + 12), wk1(koff11 + ntaui)
     if (iprcbl .gt. 0) write (unit = lunit(6), fmt = *) 'bmr5(ka), bmi5(ka), bmh5(iadrs), wdt, ntaui=', cnvhst(nn4 + 9), cnvhst(nn4 + 10), wk1(koff12 + ntaui), wdt, ntaui
     ka = ka + 1
     koff12 = koff12 + ntaui
     koff11 = koff12
  end do
  kcbl = koff11
  if (koff11 .lt. lhist) go to 6258
  write (unit = lunit(6), fmt = 5340) lhist, koff11
  write (unit = lunit(6), fmt = *) ' Nearby statement number is 6258. '
  stop
6258 if (koff11 .gt. koff25 + 288) kmode = 5
  k = k + it2 -1
  itadd = itadd + it2 + ( it2 - 1 ) * it2 / 2
  go to 547
  !
60001 it2 = iabs (n4)
  aph = it2
  nl = litype(k)
  if (istead .eq. 0) go to 60005
  !     initial conditions from steady-state
  !        convert to mode quantities
61594 n9 = it2 * it2
  j = nl -1
  do i = 1, n9
     j = j + 1
     volt(i) =  qfd(j)
  end do
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3438) (volt(l), l = 1, n9)
3438 format (/, ' matrix [t] follows.  volt(1 : n9) ....', /, (1x, 5e25.16))
  call move0 (volti(1 :), n9)
  n10 = 1
  n12 = it2 + 1
  do l = 1, it2
     volti(n10) = 1.0
     n10 = n10 + n12
  end do
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3466) (volti(l), l = 1, n9)
3466 format (' unit matrix.  volti(1 : n9) ...', /, (1x, 5e25.16))
  call dgelg ( volti(1), volt(1), it2, it2, epsiln, ier )
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3471) ier, (volti(m), m = 1, n9)
3471 format (/, ' inverse of  (zthev).  ier =,  i2', /, (1x, 5e25.16))
  n5 = nl - 1
  iy = k
  iz = itadd
  do i = 1, it2
     n7 = iy
     n8 = iz
     vsl = 0.0
     vsli = 0.0
     vsr = 0.0
     vsri = 0.0
     csl = 0.0
     csli = 0.0
     csr = 0.0
     csri = 0.0
     do j = 1, it2
        n1 = iabs (kbus(n7))
        n2 = iabs (mbus(n7))
        n5 = n5 + 1
        h1 = qfd(n5)
        vsl = vsl + h1 * e(n1)
        vsli = vsli + h1 * f(n1)
        vsr = vsr + h1 * e(n2)
        vsri = vsri + h1 * f(n2)
        n6 = i + (j-1) * it2
        h2 = volti(n6)
        csl = csl + h2 * tr(n8)
        csli = csli + h2 * tx(n8)
        csr = csr + h2 * r(n8)
        csri = csri + h2 * c(n8)
        n7 = n7 + 1
        n8 = n8 + 1
        if (i .ne. 1) cycle
        if (iprsup .le. 5) cycle
        write (unit = lunit(6), fmt = 72595) j, e(n1), f(n1), e(n2), f(n2), tr(n8), tx(n8), r(n8), c(n8)
72595   format(/, " Initial phase values, Marti's branch", i4, 8e11.4)
     end do
     if (iprsup .ge. 5) write (unit = lunit(6), fmt = 72597) i, vsl, vsli, vsr, vsri, csl, csli, csr, csri
72597 format(/, " Initial modal values, Marti's branch", i4, 8e11.4)
     omegs1 = omega
     if (istead .eq. 0) omegs1 = 0.0
     n4 = indhst(k)
     cnvhst(n4 + 5) = csl
     cnvhst(n4 + 6) = csr
     n1 = kodsem(k)
     nrz = cki(k)
     nra = ckkjm(k)
     do ii = 1, nrz
        n2 = n1 + ii
        n3 = n2 + nrz
        d1 = sconst(n3) ** 2 + omegs1 ** 2
        czire = sconst(n2) * sconst(n3) / d1
        cziim = - sconst(n2) * omegs1 / d1
        nn5 = n1 + 5*nrz + 5*nra + 4 + ii
        sconst(nn5) = csl * czire - csli * cziim
        n6 = nn5 + nrz
        sconst(n6) = csr * czire - csri * cziim
     end do
     if (iprsup .lt. 1) go to 81108
     write (unit = lunit(6), fmt = 81106) i
81106 format (/, " Initialization of history vectors of Marti's branch", i4, '  node k, node m partial equiv. voltages')
     j1 = nn5 - nrz + 1
     j2 = n6 - nrz + 1
     write (unit = lunit(6), fmt = 81107) (sconst(ii), ii = j1, nn5)
81107 format (1x, 10e12.5)
     write (unit = lunit(6), fmt = 81107) (sconst(ii), ii = j2, n6)
     ! forward perturbation functions  (f)
81108 ckreal = vsl + cnvhst(n4 + 1) * csl - cnvhst(n4 + 2) * csli
     ckimag = vsli + cnvhst(n4 + 1) * csli + cnvhst(n4 + 2) * csl
     absfk = sqrtz (ckreal ** 2 + ckimag ** 2)
     phfk = funp13 (ckimag, ckreal, twopi)
     cmreal = vsr + cnvhst(n4 + 1) * csr - cnvhst(n4 + 2) * csri
     cmimag = vsri + cnvhst(n4 + 1) * csri + cnvhst(n4 + 2) * csr
     absfm = sqrtz (cmreal ** 2 + cmimag ** 2)
     phfm = funp13 (cmimag, cmreal, twopi)
     nrf = cnvhst(n4) / deltat + 2.
     nr(k) = iline + 1
     iline = iline + 1 + nrf
     if (iline .le. lpast) go to 8109
     iprint = 8
     lstat(19) = 8109
     go to 9000
8109 do ii = 1, nrf
        ix = nr(k) + ii - 1
        xk(ix) = absfk * cosz ((-nrf + ii) * omegs1 * deltat + phfk)
        xm(ix) = absfm * cosz ((-nrf + ii) * omegs1 * deltat + phfm)
     end do
     if (iprsup .lt. 1) go to 81113
     j1 = ix - nrf + 1
     write (unit = lunit(6), fmt = 81112) j1, ix
81112 format (/, ' Forward functions xk and xm; positions', i6, '  to', i6)
     write (unit = lunit(6), fmt = 81107) (xk(ii), ii = j1,ix), (xm(ii),ii = j1, ix)
     !     history of partial equivalent current sources  (b)
81113 d9 = -omegs1 * cnvhst(n4)
     c2r = cosz (d9)
     c2i = sinz (d9)
     do ii = 1, nra
        n2 = n1 + 2*nrz + ii
        n3 = n2 + nra
        d1 = sconst(n3) ** 2 + omegs1 ** 2
        c3r = sconst(n2) * sconst(n3) / d1
        c3i =-sconst(n2) * omegs1 / d1
        n7 = n1 + 7*nrz + 5*nra + 4 + ii
        d2 = c2r * c3r - c2i * c3i
        d3 = c2r * c3i + c2i * c3r
        sconst(n7) = d2 * cmreal - d3 * cmimag
        n8 = n7 + nra
        sconst(n8) = d2 * ckreal - d3 * ckimag
     end do
     if (iprsup .lt. 1) go to 63593
     j1 = n7 - nra + 1
     j2 = n8 - nra + 1
     write (unit = lunit(6), fmt = 81120)
81120 format (/, ' Partial equiv. current sources')
     write (unit = lunit(6), fmt = 81107) (sconst(ii),ii = j1, n7), (sconst(ii), ii = j2, n8)
63593 k = k + 1
     itadd = itadd + 1
  end do
  itadd = itadd + it2 * (it2 - 1) / 2
  it1 = 0
  k = k - 1
  if (istead .gt. 0) go to  547
  go to 544
  !
  !        user-supplied d.c. conditions
  !        single phase lines
60005 n1 = iabs (kbus(k))
  n2 = iabs (mbus(k))
  tr(itadd) = ci1
  tx(itadd) = 0.0
  r(itadd) = a
  c(itadd) = 0.0
  f(n1) = 0.0
  f(n2) = 0.0
  omegs1 = 0.0
  if (n4 .ge. 0) go to 61594
  ! multiphase lines
  ix = itadd
  do i1 = 2, it2
     ii = k + i1 - 1
     if (nr(ii) .lt. 0) it1 = 1
     l = -kbus(ii)
     m = mbus(ii)
     ! read input card using cimage
     call cimage
     read (unit = abuff, fmt = 17020) n3, bus1, bus2, h1, h2, ci1, ck1
17020 format (i2, 2a6,4e15.8)
17021 format (' ', a6, 1x, a6, 4e13.5)
     write (unit = lunit(6), fmt = 17021) bus1, bus2, h1, h2, ci1, ck1
     if (bus(l) .ne. bus1 .or. bus(m) .ne. bus2) call stoptp
     ix = ix + 1
     tr(ix) = h1
     tx(ix) = 0.0
     r(ix) = ci1
     c(ix) = 0.0
     f(l) = 0.0
     f(m) = 0.0
  end do
  marti = 1
  go to 61594
578 curr1 = ci1
  curi1 = ck1
  curr2 = a
  curi2 = d2
  !
  ! meyer_line with groung wire fd goes through he
  !
579 vr1 = e(n1)
  vi1 = f(n1)
  vr2 = e(n2)
  vi2 = f(n2)
  it2 = iabsz (n4)
  yx = it2
  gus3 = vr1
  bi3 = vi1
  gus4 = vr2
  bi4 = vi2
  vsl = curr1
  vsli = curi1
  vsr = curr2
  vsri = curi2
  it1 = 0
  ! transformation of initial conditions from phase to modal
  ! quantities
  nl = litype(k)
  do i = 1, it2
     n7 = k + i - 1
     l = kbus(n7)
     l = iabs(l)
     m = mbus(n7)
     ii = itadd + i - 2
     if (iprsup .ge. 2) write (unit = lunit(6), fmt = 34572) i, l, m, k, yx, ci(k), e(l), e(m), tr(ii), r(ii)
34572 format (/, ' next conductor.       i       l       m       k', /, 16x, 4i8, /, &
          16x, 14x, 'yx', 11x, 'ci(k)', 12x, 'e(l)', 12x, 'e(m)', 10x, 'tr(ii)', 11x, 'r(ii)', /, 16x, 6e16.7)
  end do
  iprint = 3
  itadd = isd
  nl1 = nl + it2 * it2 - 1
  do i = 1, it2
     ii = k + i - 1
     if (nr(ii) .lt. 0) it1 = 1
     l = -kbus(ii)
     m = mbus(ii)
     if (istead .gt. 0) go to 594
     if (i .eq. 1) go to 599
     ! read input card using cimage
     call cimage
     read (unit = abuff, fmt = 7020) n3, bus1, bus2, ci1, ck1, a, d2
     write (unit = kunit6, fmt = 7021)
7021 format ('+steady state initial conditions')
     if (bus(l) .ne. bus1 .or. bus(m) .ne. bus2) go to 9020
599  if (itadd .gt. ldata) go to 9000
     tr(itadd) = ci1
     tx(itadd) = ck1
     r(itadd) = a
     c(itadd) = d2
594  itadd = itadd + 1
     a = ci(ii)
     d2 = absz (ck(ii))
     if (a .lt. 0.) a = -a * d2
     d2 = d2 / yx
     volti(i) = -a / d2
     ekreal(i) = e(m)
     ekimag(i) = f(m)
     emreal(i) = e(l)
     emimag(i) = f(l)
  end do
  if (iprsup .lt. 1) go to 1594
  n2 = itadd - 1
  write (unit = lunit(6), fmt = 1593) nl, isd, n2, it2
1593 format (' nl, isd, n2 and it2 at 1593 are', 4i10)
  write (unit = lunit(6), fmt = 2593) (tr(i), tx(i), r(i), c(i), i = isd, n2)
2593 format (' (tr(i), tx(i), r(i), c(i), i = isd, n2) are', /, (1x, 8e15.6))
  write (unit = lunit(6), fmt = 3593) (ekreal(i), ekimag(i), emreal(i), emimag(i), i = 1, it2)
3593 format (' (ekreal(i), ekimag(i), emreal(i), emimag(i), i = 1, it2) are', /, (1x,8e15.6))
1594 do jj = 1, it2
     ii = k + jj - 1
     if (ck(ii) .lt. 0.0) go to 21594
  end do
  go to 4594
21594 n9 = it2 * it2
  j = nl -1
  do i = 1, n9
     j = j + 1
     volt(i) =  qfd(j)
  end do
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3438) (volt(l), l = 1, n9)
  call move0 (voltk(1 :), n9)
  n10 = 1
  n12 = it2 + 1
  do l = 1, it2
     voltk(n10) = 1.0
     n10 = n10 + n12
  end do
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3467) (voltk(l), l = 1, n9)
3467 format (' unit maxtrix.  voltk(1 : n9) ...', /, (1x, 5e25.16))
  call dgelg (voltk(1), volt(1), it2, it2, epsiln, ier)
  if (iprsup  .ge.  1) write (unit = lunit(6), fmt = 3471) ier, (voltk(m), m = 1, n9)
  n5 = nl - 1
  n7 = isd
  do i = 1, it2
     vsl = 0.0d0
     vsr = 0.0d0
     csl = 0.0d0
     csr = 0.0d0
     do j = 1, it2
        n5 = n5 + 1
        h1 = qfd(n5)
        vsl = vsl + h1 * ekreal(j)
        vsr = vsr + h1 * emreal(j)
        n6 = i + (j-1) * it2
        h2 = voltk(n6)
        csl = csl + h2 * r(n7)
        csr = csr + h2 * tr(n7)
        n7 = n7 + 1
     end do
     closev(i) = vsr
     farv(i) = vsl
     closei(i) = csr
     fari(i) = csl
     n7 = isd
  end do
  !**** complex voltage & current vectors are stored in ekreal,ekimag,
  !     emreal,emimag and tr,tx,r,c
4594 n6 = isd - 1
  do i = 1, it2
     do j = 1, i
        gus1 = 0.
        n5 = nl - 1
        do ip = 1, it2
           jip = n5 + j
           iip = n5 + i
           gus1 = gus1 + qfd(jip) * volti(ip) * qfd(iip)
           n5 = n5 + it2
        end do
        n6 = n6 + 1
        x(n6) = gus1
     end do
  end do
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 1595) isd, n6, (x(i), i = isd, n6)
1595 format (' at 1595 x(i) from i = ', i8, 3x, 'to ', i8, 3x, 'are', /, (1x, 8e15.6))
  isd1 = isd + it2 * (it2 + 1) / 2 - 1
  ll0 = 0
  call redu13 (x(isd :), it2, ll0)
  ll1 = 1
  call mult (x(isd), tr(isd), emreal, it2, ll1)
  call mult (x(isd), tx(isd), emimag, it2, ll1)
  call mult (x(isd), r(isd), ekreal, it2, ll1)
  call mult (x(isd), c(isd), ekimag, it2, ll1)
  !**** y=ax+y is obtained for side m,k in real and imag
  n5 = nl - 1
  do i = 1, it2
     vsl = 0.
     vsli = 0.
     vsr = 0.
     vsri = 0.
     ii = k + i - 1
     a = ci(ii)
     if (a .lt. 0.) a = -a * ck(ii)
     a = a * yx
     do j = 1, it2
        n5 = n5 + 1
        h1 = qfd(n5)
        vsl = vsl + h1 * ekreal(j)
        vsli = vsli + h1 * ekimag(j)
        vsr = vsr + h1 * emreal(j)
        vsri = vsri + h1 * emimag(j)
     end do
     volti(i) = sqrtz (vsl * vsl + vsli * vsli) * a
     if (volti(i) .eq. 0.) vsl = 1.
     volt(i) = atan2z (vsli, vsl)
     voltk(i) = sqrtz (vsr * vsr + vsri * vsri) * a
     if (voltk(i) .eq. 0.) vsr = 1.
     ndx1 = lsiz26 + i
     vim(ndx1) = atan2z (vsri, vsr)
  end do
  ! *** end of transformation for initial conditions
  ii = length(k + 1)
  if (length(k) .gt. 0) ii = length(k)
  itadd = itadd + it2 * (it2 - 1) / 2
  if (istead .le. 0) itadd = it + 1
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 1592) (volti(i), volt(i), voltk(i), i = 1, it2)
1592 format (' (volti(i), volt(i), voltk(i),             i = 1, it2) are', /, (1x, 6e20.10))
  ndx1 = lsiz26 + 1
  ndx2 = lsiz26 + it2
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 21592) (vim(i), i = ndx1, ndx2)
21592 format (' vim(lsiz26 + i):', 6e19.10)
  k = k - 1
  i = 0
5019 i = i + 1
  k = k + 1
  h1 = volti(i)
  h2 = voltk(i)
  ci1 = volt(i)
  ndx1 = lsiz26 + i
  ck1 = vim(ndx1)
  n4 = nr(k) + 1
  n2 = ii + n4
  gus1 = -n4
  gus1 = gus1 * steady
  gus4 = gus1 + (1.0 - cik(k)) * steady
  do n1 = ii, n2
     a = h1 * cosz (gus1 + ci1)
     d2 = h2 * cosz (gus1 + ck1)
     gus1 = gus1 + steady
     xk(n1) = a
     xm(n1) = d2
  end do
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 44582) k, kbus(k), mbus(k), ii, n2, it2, (xk(n1), xm(n1), n1 = ii, n2)
44582 format (/, ' one mode past history  (xk(j), xm(j), j = ii, n2)  follows.       k kbus(k) mbus(k)      ii      n2     it2', /, &
       58x, 6i8, /, (1x, 8e16.7))
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 44583) h1, h2, ci1, ck1, steady
44583 format (' phasor quantities used.', 14x, 'h1', 14x, 'h2', 13x, 'ci1', 13x, 'ck1', 10x, 'steady', /, 24x, 5e16.7)
  if (ck(k) .lt. 0) go to 20750
  a = h1 * cosz (gus4 + ci1)
  d2 = h2 * cosz (gus4 + ck1)
  if (ci(k) .lt. 0.) go to 584
  ci1 = (ck(k) + 1.0) / 2.0
  ck1 = 1.0 - ci1
  xk(n2) = ci1 * a + ck1 * d2
  xm(n2) = ci1 * d2 + ck1 * a
  go to 583
20750 kf = cik(k)
  ifdep2 = kf + kf - 1
  a = 0.0
  d7 = h2
  d8 = ck1
  d9 = h1
  d10 = ci1
20755 d3 = con1(ifdep2)
  d4 = con1(ifdep2 + 2 * lfdep)
  d5 = con1(ifdep2 + 4 * lfdep)
  iftail = iftail + 1
  if (iftail .le. ltails) go to 20762
  iprint = 15
  lstat(19) = 20762
  go to 9000
20762 d6 = omega * d5
  d2 = d3 / (d4 * d4 + omega * omega)
  d3 = d2 * omega
  d4 = d2 * d4
  d8 = d8 - d6
  d10 = d10 - d6
  stailk(iftail) = d7 * (d4 * cosz (d8) + d3 * sinz (d8))
  stailm(iftail) = d9 * (d4 * cosz (d10) + d3 * sinz (d10))
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 20767) ifdep2, iftail, k, stailk(iftail), stailm(iftail), d2, d3, d4, d5, d6, d7, d8, d9, d10
20767 format (/, ' exp. tail history.  ifdep2  iftail       k', 6x, 'stailk(iftail)', &
       6x, 'stailm(iftail)', 14x, 'd2', 14x, 'd3', /, &
       19x, 3i8, 2e20.11, 2e16.7, /, 19x, 14x, 'd4', &
       14x, 'd5', 14x, 'd6', 14x, 'd7', 14x, 'd8', &
       14x, 'd9', 13x, 'd10', /, 19x, 7e16.7)
  if (a .gt. 0.0) go to 20775
  a = 1.0
  d7 = h1
  d8 = ci1
  d9 = h2
  d10 = ck1
  ifdep2 = ifdep2 + 1
  go to 20755
  ! m22.452220775 if ( it2  .eq.  1 )   go to 20700
20775 d7 = ci(k) * yx / eta(kf)
  a = d7 * closev(i) - closei(i)
  d2 = d7 * farv(i)  -  fari(i)
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 20776) k, i, ifdep2, iftail, closev(i), farv(i), closei(i), fari(i), ci(k), a, d2
20776 format (/, ' freq. dep. last history point.       k       i  ifdep2  iftail', 7x, 'closev(i)', &
       9x,  'farv(i)',  7x, 'closei(i)', /, 31x, 4i8, 3e16.7, /, 31x, 9x, 'fari(i)', 11x, 'ci(k)', &
       15x, 'a', 14x, 'd2', /, 31x, 4e16.7)
584 xk(n2) = a
  xm(n2) = d2
583 ii = n2 + 1
  if (i .lt. it2) go to 5019
  if (istead .gt. 0) go to 547
  go to 544
5750 it2 = iabs (kodebr(k))
  n11 = k + it2 - 1
  if (istead .gt. 0) go to 5862
  itadd = nr(k) - 1
  it = itadd
  !     read input card using cimage
  call cimage
  do i = 1, it2
     ii = k + i - 1
     itadd = itadd + 1
     if (i .ne. 1) go to 5763
     tr(itadd) = ci1
     tx(itadd) = ck1
     r(itadd) = a
     c(itadd) = d2
     go to 5759
5763 m = iabs(mbus(ii))
     l = iabs(kbus(ii))
     if (kolbeg .gt. 0) go to 4693
     if (kolbeg .gt. 0) go to 4693
     read (unit = abuff, fmt = 7020) n3, bus1, bus2, tr(itadd), tx(itadd), r(itadd), c(itadd)
     go to 4695
4693 nfrfld = 1
     kolbeg = 1
     !     call freone (d11)
     call free (d11)
     n3 = d11
     nright = -1
     !     call freone (d1)
     call free (d1)
     bus1 = texta6(1)
     !     call freone (d1)
     call free (d1)
     bus2 = texta6(1)
     nright = 0
     !     call frefld (tr(itadd :))
     call free (tr(itadd :))
     !     call frefld (tx(itadd :))
     call free (tx(itadd :))
     !     call frefld (r(itadd :))
     call free (r(itadd :))
     !     call frefld (c(itadd :))
     call free (c(itadd :))
4695 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54159) tr(itadd), tx(itadd), r(itadd)
     if (bus1 .eq. bus(l) .and. bus2 .eq. bus(m) .and. n3 .eq. 3) go to 5767
5766 lstat(19) = 5766
     kill = 32
     go to 9200
     ! read input card using cimage
5767 call cimage
5759 if (kolbeg .gt. 0) go to 4703
     read (unit = abuff, fmt = 7020) n3, bus1, bus2
     go to 4705
4703 nfrfld = 1
     !     call freone (d11)
     call free (d11)
     n3 = d11
     nright = -1
     !     call freone (d1)
     call free (d1)
     bus1 = texta6(1)
     !     call freone (d1)
     call free (d1)
     bus2 = texta6(1)
     nright = 0
4705 if (to_lower (bus1) .ne. text1) cycle
     if (to_lower (bus2) .ne. text2) cycle
     if (n3 .ne. i) go to 5766
     read (unit = abuff, fmt = 5764) ci1, ck1, a, d2
5764 format (14x,4e15.8)
     write (unit = kunit6, fmt = 5761) ci1, ck1, d2
5761 format ('+Modal z and y.', 3e11.4)
     ! read input card using cimage
     call cimage
     n3 = iabs (indhst(ii))
     if (ci(ii) .gt. 0.0) n3 = n3 + 1
     cnvhst(n3 + 0) = ci1
     cnvhst(n3 + 1) = ck1
     cnvhst(n3 + 2) = a
     cnvhst(n3 + 3) = d2
     cnvhst(n3 + 4) = steady / deltat
  end do
  itadd = it + 1
5862 do i = k, n11
     if (indhst(i) .gt. 0) indhst(i) = - indhst(i)
  end do
  itadd = itadd + it2 * (it2 + 1) / 2
5809 if ( cki(k) .lt. 0.0) go to 5810
  k = k + 1
  go to 5809
5810 if (istead .eq. 0) go to 5758
  go to 547
  ! initial condition input for EMTP switch component.
5600 if (kolbeg .gt. 0) go to 4713
  read (unit = abuff, fmt = 5610) bus1, bus2, n1, n2, n3, d1, d2, d3, d4
5610 format (2x, 2a6, 3i4, 4e13.6)
  go to 4715
4713 nfrfld = 1
  nright = -1
  !  call freone (d1)
  call free (d1)
  bus1 = texta6(1)
  !  call freone (d1)
  call free (d1)
  bus2 = texta6(1)
  nright = 0
  !  call freone (d11)
  call free (d11)
  n1 = d11
  !  call freone (d11)
  call free (d11)
  n2 = d11
  !  call freone (d11)
  call free (d11)
  n3 = d11
  !  call freone (d1)
  call free (d1)
  !  call freone (d2)
  call free (d2)
  !  call freone (d3)
  call free (d3)
  !  call freone (d4)
  call free (d4)
4715 if (noutpr .eq. 0) write (unit = kunit6, fmt = 5620)  n1, n2, n3, d1
5620 format ('+Switch init. cond.', 3i6, e13.4)
  do k = 1, kswtch
     l = iabs (kmswit(k))
     ndx1 = lswtch + k
     m = iabs (kmswit(ndx1))
     if (bus1 .ne. bus(l)) cycle
     if (bus2 .eq. bus(m)) go to 5640
  end do
  kill = 31
  lstat(19) = 5630
  go to 9200
5640 tclose(k) = d1
  iflag = 0
  adelay(k) = d2
  energy(k) = d3
  crit(k) = d4
  kpos(k) = n1
  kode(l) = n2
  kode(m) = n3
  ! read input card using cimage
  call cimage
  read (unit = abuff, fmt = 5656) nextsw(k)
5656 format (14x, i4)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 5643) nextsw(k)
5643 format ('+  Continue.  nextsw = ', i4)
  go to 544
  ! transfer control to 2nd half of overlay 13, in "last13".
590 if (marti .eq. 1) go to 4590
  do kj = 1, ibr
     if (imodel(kj) .ne. -2) cycle
     n4 = indhst(kj)
     nrf = cnvhst(n4) / deltat + 2.0d0
     nr(kj) = iline + 1
     iline = iline + 1 + nrf
     if (iline .le. lpast) cycle
     iprint = 8
     lstat(19) = 3590
     go to 9000
  end do
4590 call last13
  if (kill .gt. 0) go to 9200
  lastov = nchain
  nchain = nchain + 1
  go to 9800
9020 write (unit = lunit(6), fmt = 9021)
9021 format (' Error in identification of next branch. Job terminated')
  go to 9200
9000 lstat(16) = iprint
  kill = 1
9200 lastov = nchain
  nchain = 51
  lstat(18) = lastov
9800 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 9806)
9806 format (' Exit module  "over13".')
  if (allocated (cmr)) then
     kks = transfer (cmr, kks)
     deallocate (cmr)
  end if
  return
end subroutine over13

!
! subroutine fdint.
!

subroutine fdint (ikf, isfd, ibf, omg)
  use blkcom
  use labcom
  implicit none
  integer(4), intent(out) :: ibf
  integer(4), intent(out) :: ikf
  integer(4), intent(out) :: isfd
  real(8), intent(in) :: omg
  integer(4) :: i, idk, isc, isk, ist, isu, isv, isw
  integer(4) :: ka, kb
  real(8) :: ac1, ai, al1, ar, ar1, arl, aui, aur, azi, azr
  real(8) :: cz
  real(8) :: den
  !  dimension ur(40), ui(40)
  !     this routine initializes a)capacitor voltages, b)branch currents**
  !
  idk = 2 * ikf
  ikf = ikf + 1
  isc = ibf + 1
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 1) ikf, isfd, ibf, imfd(idk + 1), imfd(idk + 2)
1 format (' Integer counters at start of fdint......', 7x, 'ikf', 6x, 'isfd', 7x, 'ibf', 6x, 'izfd', 6x, 'ipfd', /, 41x, 5i10)
  !     calculate modal voltages from the phase values   *   *   *   *   *
  cz = it2
  cz = 1.0 / cz
  ur(1) = volt(1)
  ui(1) = voltk(1)
  do ka = 2, it2
     ur(1) = ur(1) + volt(ka)
     ui(1) = ui(1) + voltk(ka)
     ur(ka) = (volt(1) - volt(ka)) * cz
     ui(ka) = (voltk(1) - voltk(ka)) * cz
  end do
  ar = ur(1) * cz
  ai = ui(1) * cz
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 3) ikf, ar, ai, (ur(i), ui(i), i = 2, it2)
3 format (1x, 'Modal voltages (set no.)', i5, 2x, 'real and imaginary in pairs', /, (2x, 6e20.11))
  !     Initialize  branch  quantities   *   *   *   *   *   *   *   *   *
  !     Process first the zero sequence data *   *   *   *   *   *   *   *
  ist = isfd + 1
  isu = isfd + imfd(idk + 1) * 5
  isv = ibf
  do ka = ist, isu, 5
     isv = isv + 1
     ar1 = rmfd(ka)
     al1 = rmfd(ka + 1) * omg
     arl = rmfd(ka + 3)
     ac1 = rmfd(ka + 2)
     if (ac1 .gt. 0.0d0) ac1 = 1.0d0 / (ac1 * omg)
     azr = ar1
     azi = al1
     if (arl .eq. 0.0d0 .or. al1 .eq. 0.0d0) go to 4
     den = 1.0d0 / (arl * arl + al1 * al1)
     azr = azr + arl * (al1 * al1) * den
     azi = al1 * (arl * arl) * den
4    azi = azi - ac1
     ! Invert branch impedance to obtain admittance *   *   *   *   *   *
     den = 1.0d0 / (azr * azr + azi * azi)
     azr = azr * den
     azi = -azi * den
     ! Calculate and store branch current   *   *   *   *   *   *   *   *
     aur = ar * azr - ai * azi
     aui = ar * azi + ai * azr
     cikfd(isv + 1) = aur
     ! Calculate capacitor voltage  *   *   *   *   *   *   *   *   *   *
     cikfd(isv + 2) = -aui * ac1
     isv = isv + 2
  end do
  ibf = isv
  isfd = isu
  ist = isfd + 1
  isu = isfd + imfd(idk + 2) * 5
  isk = imfd(idk + 2) * 3
  isv = isv - 2
  ! process the remaining modes  *   *   *   *   *   *   *   *   *   *
  ! start loop across all modal branches *   *   *   *   *   *   *   *
  do ka = ist, isu, 5
     ar1 = rmfd(ka)
     al1 = rmfd(ka + 1) * omg
     arl = rmfd(ka + 3)
     ac1 = rmfd(ka + 2)
     if (ac1 .gt. 0.0d0) ac1 = 1.0d0 / (ac1 * omg)
     azi = al1
     azr = ar1
     if (arl .eq. 0.0d0 .or. al1 .eq. 0.0d0) go to 6
     den = 1.0d0 / (arl * arl + al1 * al1)
     azr = azr + arl * (al1 * al1) * den
     azi = al1 * (arl * arl) * den
6    azi = azi - ac1
     ! invert branch impedance to obtain admittance *   *   *   *   *   *
     den = 1.0d0 / (azr * azr + azi * azi)
     azr = azr * den
     azi = -azi * den
     ! start the internal loop across modes *   *   *   *   *   *   *   *
     isv = isv + 3
     isw = isv
     do kb = 2, it2
        ! calculate and store branch current   *   *   *   *   *   *   *   *
        aur = ur(kb) * azr - ui(kb) * azi
        aui = ur(kb) * azi + ui(kb) * azr
        cikfd(isw + 1) = aur
        ! calculate capacitor voltage  *   *   *   *   *   *   *   *   *   *
        cikfd(isw + 2) = -aui * ac1
        isw = isw + isk
     end do
  end do
  ibf = ibf + (it2 - 1) * isk
  isfd = isu
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 9) isc, ibf, (cikfd(ka), ka = isc, ibf)
9 format (' Array cikfd from', i6, '  to', i6, /, (2x, 6e21.11))
  return
end subroutine fdint

!
! subroutine redinv.
!

subroutine redinv (x1, m, n)
  use movcop
  implicit none
  real(8), intent(out) :: x1(:)
  integer(4), intent(in) :: m, n
  integer(4) :: i, ik
  integer(4) :: j
  integer(4) :: k
  integer(4) :: mk
  integer(4) :: n9, nk
  real(8) :: a1(20)
  real(8) :: b
  real(8) :: c
  !
  !     Gauss-Jordan elimination process performed on a square matrix x
  !)    this routine can also be used for matrix inversion * * * * * * * *
  !     maximum order of matrix x is 20
  j = m
  ik = m ** 2
  nk = ik - m
1 c = x1(ik)
  c = 1.0 / c
  call move (x1(nk + 1 :), a1(1 :), m)
  k = 1
4 mk = (k - 1) * m
  n9 = mk + j
  b = -x1(n9) * c
  i = 1
3 mk = mk + 1
  x1(mk) = x1(mk) + b * a1(i)
  i = i + 1
  if (i .le. m) go to 3
  n9 = mk + j - m
  x1(n9) = b
  k = k + 1
  if (k .eq. j) k = k + 1
  if (k .le. m) go to 4
  do k = 1, m
     n9 = nk + k
     x1(n9) = a1(k) * c
  end do
  x1(ik) = c
  j = j - 1
  ik = ik - m - 1
  nk = nk - m
  if (j .gt. n) go to 1
  return
end subroutine redinv

!
! subroutine last13.
!

subroutine last13
  use blkcom
  use labcom
  use tracom
  use movcop
  implicit none
  !
  !  equivalence (lstat(14), mdrive), (volt(1), vim(1))
  !
  integer(4) :: i, i0, i1, i2, i3, i4, i5, i6, i7, i8, i10, i11, iflag, ii, iq2
  integer(4) :: iq3, itadd
  integer(4) :: j
  integer(4) :: k
  integer(4) :: l
  integer(4) :: moon, mxstrt
  integer(4) :: n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n15, n16, n23, ndx1
  real(8) :: d1, d2, d3, d4, d5, d5save, d6, d7, d8, d9, d10, d11, d12, d13, d14
  real(8) :: d15, d16, d17, d18, d19, d1save, d20, d21, d22, dthaf, dxp, dxq
  real(8) :: eeeta, eoutk, eoutm
  real(8) :: fac
  real(8) :: resi, resr
  real(8) :: soutk, soutm, spole, steady, stype
  real(8) :: tdi, tdr, tii, tir, tni, tnr, tri
  real(8) :: trisum, trr, trrsum
  real(8) :: w, wdthaf, wfac, wpole, wshz
  double precision :: dblpr1, dblpr2, dblpr3, dblpr4
  !
  integer(4), pointer :: mdrive => lstat(14)
  !
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format (' Begin module "last13".')
  i = ibr + 1
610 i = i - 1
  if (i .le. 0) go to 650
  if (kbus(i) .ge. 0 .or. kodsem(i) .eq. 0) go to 610
  if (imodel(i) .eq. -2) go to 610
  if (imodel(i) .eq. -4) go to 610
  i = i - length(i) + 1
  n15 = iabs (kbus(i))
  if (n15 .eq. 1) n15 = iabs (mbus(i))
  n16 = iabs (kssfrq(n15))
  steady = twopi * sfreq(n16) * deltat
  it2 = iabs (kodebr(i))
  if (iprsup .lt. 6) go to 14009
  k = i
  n7 = 1
14000 write (unit = lunit(6), fmt = 14999) k, kbus(k), mbus(k), nr(k), length(k), kodsem(k), indhst(k), kodebr(k), ci(k), ck(k), cik(k), cki(k)
  n1 = iabs (kodsem(k))
  n2 = iabs (indhst(k))
  n3 = n1 - 1
  n4 = n2 - 1
  n8 = n4
  if (n7 .le. iabs (kodebr(k))) n8 = n8 + 6
  if (kodsem(k) .gt. 0) go to 14002
  n3 = n3 + 2 - ck(k) - ck(k)
  n4 = n4 + 2
  if (ck(k) .lt. 0.0) go to 14001
  n3 = n3 + 6 * ck(k)
  n4 = n4 + ck(k) - 1
  go to 14005
14001 continue
  go to 14005
14002 n3 = n3 + 1 - ci(k) - ci(k)
  n4 = n4 + 2
  if (ci(k) .lt. 0.0) go to 14003
  n3 = n3 + 7 * ci(k)
  n4 = n4 + 2 * ci(k) - 2
14003 n3 = n3 - ck(k) - ck(k)
  n4 = n4 + 2
  if (ck(k) .lt. 0.0) go to 14004
  n3 = n3 + 6 * ck(k)
  n4 = n4 + 2 * ck(k) - 2
14004 continue
14005 write (unit = lunit(6), fmt = 14996) n1, n3, (sconst(l), l = n1, n3)
  if (n8 .gt. n4) n4 = n8
  write (unit = lunit(6), fmt = 14995) n2, n4, (cnvhst(l), l = n2, n4)
  k = k + 1
  n7 = n7 + 1
  if (cki(k - 1) .gt. 0.0) go to 14000
14009 if (indhst(i) .gt. 0 .or. nr(i) .lt. 0) go to 14100
  !
  !  compute phasor terminal conditions(voltage and current) and voltage
  !  travelling waves(phase frame if cik(.) .lt. 0 else modal).
  !
  !     volt       +j*volti        kbus terminal voltage
  !     voltk      +j*vim          mbus terminal voltage
  !     semaux(0)  +j*semaux(i1)   kbus terminal current towards mbus
  !     semaux(i2) +j*semaux(i3)   mbus terminal current towards kbus
  !     semaux(i4) +j*semaux(i5)   outgoing voltage travelling wave from k
  !     semaux(i6) +j*semaux(i7)   outgoing voltage travelling wave from m
  !     semaux(i8) +j*semaux(n23)   incoming voltage travelling wave at k
  !     semaux(i10)+j*semaux(i11)  incoming voltage travelling wave at m
  !
  i1 = it2
  i2 = i1 + it2
  i3 = i2 + it2
  i4 = i3 + it2
  i5 = i4 + it2
  i6 = i5 + it2
  i7 = i6 + it2
  i8 = i7 + it2
  n23 = i8 + it2
  i10 = n23 + it2
  i11 = i10 + it2
  n1 = absz (cik(i))
  n2 = n1 + it2 * it2
  n3 = nr(i)
  n4 = n3 + it2 - 1
  wdthaf = onehaf * steady
  w = steady / deltat
  if (cik(i) .gt. 0.0d0) go to 14010
  call move0 (volt(1 :), it2)
  call move0 (volti(1 :), it2)
  call move0 (voltk(1 :), it2)
  call move0 (vim(lsiz26 + 1 :), it2)
  call move0 (semaux(1 :), 12 * it2)
14010 do k = 1, it2
     if (kodsem(i) .lt. 0 .and. cik(i) .lt. 0.0) go to 14060
     d1 = 0.0d0
     d2 = 0.0d0
     d3 = 0.0d0
     d4 = 0.0d0
     d5 = 0.0d0
     d6 = 0.0d0
     d7 = 0.0d0
     d8 = 0.0d0
     do j = n3, n4
        ii = i + j - n3
        n9 = -kbus(ii)
        d1 = d1 + qfd(n1) * e(n9) - qfd(n2) * f(n9)
        d2 = d2 + qfd(n1) * f(n9) + qfd(n2) * e(n9)
        n9 = iabs(mbus(ii))
        d3 = d3 + qfd(n1) * e(n9) - qfd(n2) * f(n9)
        d4 = d4 + qfd(n1) * f(n9) + qfd(n2) * e(n9)
        d5 = d5 + sfd(n1) * tr(j) - sfd(n2) * tx(j)
        d6 = d6 + sfd(n1) * tx(j) + sfd(n2) * tr(j)
        d7 = d7 + sfd(n1) * r(j) - sfd(n2) * c(j)
        d8 = d8 + sfd(n1) * c(j) + sfd(n2) * r(j)
        n1 = n1 + 1
        n2 = n2 + 1
     end do
     if (kodsem(i) .lt. 0) go to 14050
     ii = k + i - 1
     n5 = -indhst(ii)
     if (ci(ii) .gt. 0.0) n5 = n5 + 1
     if (absz (w - cnvhst(n5 + 4)) .lt. 0.0001) go to 14040
     n6 = -kbus(ii)
     n7 = iabs (mbus(ii))
     write (unit = lunit(6), fmt = 14030) bus(n6), bus(n7), k, cnvhst(n5 + 4), w
14030 format(//, ' Warning...  Steady state modal parameters for recursive-convolution component connecting nodes ', "'", a6, "'", ' and ', "'", a6, "'", /, &
          13x, 'for mode ', i2, ' are determined at angular frequency of', e12.5, ' radians/sec.  The steady-state solution frequency', /, &
          13x, 'being used is ', e12.5, ' radians/sec.')
14040 d17 = cnvhst(n5 + 2) ** 2 +  cnvhst(n5 + 3) ** 2
     d19 = (cnvhst(n5 + 2) * cnvhst(n5 + 0) + cnvhst(n5 + 3) * cnvhst(n5 + 1)) / d17
     d20 = (cnvhst(n5 + 2) * cnvhst(n5 + 1) - cnvhst(n5 + 3) * cnvhst(n5 + 0)) / d17
     d17 = sqrtz (d19 * d19 + d20 * d20)
     if (d19 .lt. 0.0) go to 14043
     d17 = sqrtz ((d19 + d17) * onehaf)
     d18 = onehaf * d20 / d17
     go to 14047
14043 d18 = sqrtz ((- d19 + d17) * onehaf)
     if (d20 .lt. 0.0) d18 = -d18
     d17 = onehaf * d20 / d18
     ! d17 + j*d18 is the characteristic impedance of mode k at ss frequency
14047 d9 = onehaf * (d1 + d17 * d5 - d18 * d6)
     d13 = d1 - d9
     d10 = onehaf * (d2 + d17 * d6 + d18 * d5)
     d14 = d2 - d10
     d11 = onehaf * (d3 + d17 * d7 - d18 * d8)
     d15 = d3 - d11
     d12 = onehaf * (d4 + d17 * d8 + d18 * d7)
     d16 = d4 - d12
     if (cik(i) .lt. 0.0) go to 14060
14050 volt(k) = d1
     volti(k) = d2
     voltk(k) = d3
     ndx1 = lsiz26 + k
     vim(ndx1) = d4
     n6 = k
     semaux(n6) = d5
     n6 = n6 + it2
     semaux(n6) = d6
     n6 = n6 + it2
     semaux(n6) = d7
     n6 = n6 + it2
     semaux(n6) = d8
     if (kodsem(i) .lt. 0) cycle
     n6 = n6 + it2
     semaux(n6) = d9
     n6 = n6 + it2
     semaux(n6) = d10
     n6 = n6 + it2
     semaux(n6) = d11
     n6 = n6 + it2
     semaux(n6) = d12
     n6 = n6 + it2
     semaux(n6) = d13
     n6 = n6 + it2
     semaux(n6) = d14
     n6 = n6 + it2
     semaux(n6) = d15
     n6 = n6 + it2
     semaux(n6) = d16
     cycle
14060 n6 = -kbus(ii)
     volt(k) = e(n9)
     volti(k) = f(n9)
     n9 = iabs(mbus(ii))
     voltk(k) = e(n9)
     ndx1 = lsiz26 + k
     vim(ndx1) = f(n9)
     n9 = n3 + k - 1
     n6 = k
     semaux(n6) = tr(n9)
     n6 = n6 + it2
     semaux(n6) = tx(n9)
     n6 = n6 + it2
     semaux(n6) = r(n9)
     n6 = n6 + it2
     semaux(n6) = c(n9)
     if (kodsem(i) .lt. 0) cycle
     n7 = -cik(ii) + (k - 1) * it2
     n8 = n8 + it2 * it2
     do j=1, it2
        n6 = i4 + j
        semaux(n6) = semaux(n6) + sfd(n7) * d9 - sfd(n8) * d10
        n6 = i5 + j
        semaux(n6) = semaux(n6) + sfd(n7) * d10 + sfd(n8) * d9
        n6 = i6 + j
        semaux(n6) = semaux(n6) + sfd(n7) * d11 - sfd(n8) * d12
        n6 = i7 + j
        semaux(n6) = semaux(n6) + sfd(n7) * d12 + sfd(n8) * d11
        n6 = i8 + j
        semaux(n6) = semaux(n6) + sfd(n7) * d13 - sfd(n8) * d14
        n6 = n23 + j
        semaux(n6) = semaux(n6) + sfd(n7) * d14 + sfd(n8) * d13
        n6 = i10 + j
        semaux(n6) = semaux(n6) + sfd(n7) * d15 - sfd(n8) * d16
        n6 = i11 + j
        semaux(n6) = semaux(n6) + sfd(n7) * d16 + sfd(n8) * d15
        n7 = n7 + 1
        n8 = n8 + 1
     end do
  end do
  if (iprsup .lt. 6) go to 14085
  write (unit = lunit(6), fmt = 14081) d17, d18
14081 format(///, 5x, 'Characteristic impedance of last mode = ', e15.7, '+ j*', e15.7, //, 5x, 'mode', 6x, 'k-voltage', 6x, &
       'm-voltage', 6x, 'k-current', 6x, 'm-current', 7x, 'k v out', 8x, 'm v out', 9x, 'k v in', 9x, 'm v in', //)
  do l = 1, it2
     n3 = i2 + l
     n4 = i4 + l
     n5 = i6 + l
     n6 = i8 + l
     n7 = i10 + l
     write (unit = lunit(6), fmt = 14082) l, volt(l), voltk(l), semaux(l), semaux(n3), semaux(n4), semaux(n5), semaux(n6), semaux(n7)
14082 format (/, 6x, i2, 1x, 8e15.7)
     n2 = i1 + l
     n3 = i3 + l
     n4 = i5 + l
     n5 = i7 + l
     n6 = n23 + l
     n7 = i11 + l
     ndx1 = lsiz26 + l
     write (unit = lunit(6), fmt = 14083) volti(l), vim(ndx1), semaux(n2), semaux(n3), semaux(n4), semaux(n5), semaux(n6), semaux(n7)
14083 format (9x, 8e15.7)
  end do
14085 if (iflag .eq. 1) go to 14100
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14090)
14090 format (//, 5x, 'Table of discrepancies between the results of time-domain convolution(anlaytic) and steady-state phasor solution.', //, &
       2x, ' kbus ', 5x, ' mbus ', 5x, 'row', 5x, 'col', 7x, 'kbus voltage', 8x, 'mbus voltage', 8x, 'kbus current', 8x, 'mbus current', //)
  iflag = 1
14100 itadd = nr(i)
  dthaf = onehaf * deltat
  mxstrt = 0
  k = i
  ! begin loop over convolutions ....
14105 n1 = absz (cki(k)) - 1
  n2 = n1 / it2 + 1
  n1 = n1 - (n2 - 1) * it2 + 1
  if (itadd .lt. 0) go to 14290
  n3 = kodsem(k)
  n4 = iabs (indhst(k))
  if (n3 .gt. 0) go to 14190
  n5 = ck(k)
  if (n5 .ge. 0) go to 14290
  ! allocate and initialize past history of ametani lumped element.
  n5 = -n3 - n5 - n5
  n6 = sconst(n5) / deltat + 2.0
  length(k) = n6
  if (mxstrt .ne. 0) go to 14150
  nr(k) = iline
  mxstrt = iline
  iline = iline + n6
  if (iline .le. lpast) go to 14120
  lstat(19) = 14110
14110 lstat(16) = 8
  lstat(10) = n3
  lstat(11) = n4
  lstat(12) = n5
  flstat(15) = deltat
  flstat(16) = cnvhst(n4)
  flstat(17) = sconst(n5)
  flstat(18) = sconst(n3 - 2)
  kill = 1
  go to 9200
14120 if (indhst(k) .lt. 0) go to 14130
  n7 = nr(k)
  call move0 (xk(n7 :), n6 - 1)
  go to 14290
14130 d1 = volt(n2) - voltk(n2)
  ndx1 = lsiz26 + n2
  d2 = voltk(n2) - vim(ndx1)
  do j = 2, n6
     d3 = (j - n6) * steady
     n8 = n7 + j
     xk(n8) = d1 * cosz(d3) - d2 * sinz(d3)
  end do
  go to 14290
14150 nr(k) = - mxstrt
  if (iline .lt. mxstrt + n6) iline = mxstrt + n6
  mxstrt = 0
  if (iline .le. lpast) go to 14160
  lstat(19) = 14160
  go to 14110
14160 if (indhst(k) .lt. 0) go to 14170
  n7 = -nr(k)
  call move0 (xm(n7 :), n6 - 1)
  go to 14290
14170 n7 = -nr(k) - 2
  d1 = volt(n2) - voltk(n2)
  ndx1 = lsiz26 + n2
  d2 = voltk(n2) - vim(ndx1)
  do j = 2, n6
     d3 = (j - n6) * steady
     n8 = n7 + j
     xm(n8) = d1 * cosz(d3) - d2 * sinz(d3)
  end do
  go to 14290
  ! allocate and initialize past history for propagation convolutions.
14190 n5 = ci(k)
  !  if (n5) 14195, 14250, 14200
  if (n5 .lt. 0) then
     go to 14195
  else if (n5 .eq. 0) then
     go to 14250
  else
     go to 14200
  end if
14195 n3 = n3 - n5 - n5 + 1
  n6 = sconst(n3 - 2) / deltat + 2.0
  go to 14210
14200 n6 = cnvhst(n4) / deltat + 2.0
  n3 = n3 + 5 * n5 + 1
14210 length(k) = n6
  nr(k) = iline +1
  iline = iline + n6 +1
  if (iline .le. lpast) go to 14220
  lstat(19) = 14220
  go to 14110
14220 if (indhst(k) .lt. 0) go to 14230
  n7 = nr(k)
  call move0 (xk(n7 :), n6 - 1)
  call move0 (xm(n7 :), n6 - 1)
  go to 14250
14230 n7 = nr(k) - 2
  n8 = i4 + n2
  d1 = semaux(n8)
  n8 = i5 + n2
  d2 = semaux(n8)
  n8 = i6 + n2
  d3 = semaux(n8)
  n8 = i7 + n2
  d4 = semaux(n8)
  do j = 2, n6
     d6 = (j - n6 - 1) * steady
     d5 = cosz(d6)
     d6 = sinz(d6)
     n8 = n7 + j
     xk(n8) = d1 * d5 - d2 * d6
     xm(n8) = d3 * d5 - d4 * d6
  end do
  ! allocate and initialize past history for char. admittance convolution
14250 n5 = ck(k)
  if (n5 .ge. 0) go to 14290
  n3 = n3 - n5 - n5
  n6 = sconst(n3 - 2) / deltat + 2.0
  n7 = iline
  iline = iline + n6
  if (iline .le. lpast) go to 14260
  lstat(19) = 14260
  go to 14110
14260 if (indhst(k) .lt. 0) go to 14270
  call move0 (xk(n7 :), n6 - 1)
  call move0 (xm(n7 :), n6 - 1)
  go to 14290
14270 n7 = n7 - 2
  n8 = i8 + n2
  d1 = volt(n2) - 2.0 * semaux(n8)
  n8 = n23 + n2
  d2 = volti(n2) - 2.0 * semaux(n8)
  n8 = i10 + n2
  d3 = voltk(n2) - 2.0 * semaux(n8)
  n8 = i11 + n2
  ndx1 = lsiz26 + n2
  d4 = vim(ndx1) - 2.0 * semaux(n8)
  do j = 2, n6
     d6 = (j - n6) * steady
     d5 = cosz(d6)
     d6 = sinz(d6)
     n8 = n7 + j
     xk(n8) = d1 * d5 - d2 * d6
     xm(n8) = d3 * d5 - d4 * d6
  end do
14290 n3 = kodsem(k)
  if (n3 .gt. 0) go to 14430
  ! initialize history term of lumped element parallel capacitor
  n5 = ck(k)
  n3 = -n3
  sconst(n4 + 1) = sconst(n4 + 1) / dthaf
  if (itadd .lt. 0) go to 14310
  eeeta = cnvhst(n4)
  if (indhst(k) .lt. 0) go to 14300
  cnvhst(n4) = 0.0
  go to 14310
14300 d1 = volt(n2) - voltk(n2)
  ndx1 = lsiz26 + n2
  d2 = volti(n2) - vim(ndx1)
  d3 = cosz (steady)
  d4 = -sinz (steady)
  d5 = d1 * d3 - d2 * d4
  d6 = d1 * d4 + d2 * d3
  cnvhst(n4) = - sconst(n3 + 1) * (d5 + wdthaf * d6)
  d3 = d1 * (sconst(n3) + sconst(n3 + 1)) - cnvhst(n4)
  if (n5 .ge. 0) go to 14310
  d9 = -d1 * sconst(n3 + 2) * dthaf
  !14310 if (n5) 14320, 14340, 14370
14310 if (n5 .lt. 0) then
     go to 14320
  else if (n5 .eq. 0) then
     go to 14340
  else
     go to 14370
  end if
14320 n5 = n3 - n5 - n5
  n3 = n3 + 2
  n4 = n4 + 1
  i0 = -1
  !  assign 14340 to moon
  moon = 14340
  go to 14700
  ! ... Ametani initialization ...
14340 if (itadd .lt. 0 .or. indhst(k) .ge. 0) go to 14670
  go to 14410
  ! initialize semlyen lumped element convolution
14370 n3 = n3 + 2
  n5 = n3 + 4 * (n5 - 1)
  n4 = n4 + 1
  !  assign 14390 to moon
  moon = 14390
  i0 = -1
  go to 14800
  ! ...  semlyen initialization ...
14390 if (itadd .lt. 0 .or. indhst(k) .ge. 0) go to 14670
14410 d4 = semaux(n2)
  d5 = - d3
  d6 = - d4
  n8 = i + n1 - 1
  n7 = -kbus(n8)
  n8 = iabs (mbus(n8))
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14420) bus(n7), bus(n8), n1, n2, d3, d5, d4, d6
14420 format (/, 2x, a6, 5x, a6, 5x, i2, 6x, i2, 46x, e15.8, 5x, e15.8, /, 75x, 2(5x, e15.8))
  go to 14670
  ! transmission line propagation impulse convolution initialization
14430 n5 = ci(k)
  n3 = n3 + 1
  eeeta = cnvhst(n4)
  if (itadd .lt. 0 .or. indhst(k) .ge. 0) go to 14440
  n8 = i4 + n2
  d5 = semaux(n8)
  n8 = i5 + n2
  d6 = semaux(n8)
  n8 = i6 + n2
  d1 = semaux(n8)
  n8 = i7 + n2
  d2 = semaux(n8)
  d3 = 0.0
  d4 = 0.0
  n8 = i8 + n2
  n9 = i10 + n2
  soutk = semaux(n8)
  soutm = semaux(n9)
  trrsum = 0.0
  trisum = 0.0
  if (n5 .ge. 0) go to 14440
  d9 = 0.0
  d10 = 0.0
  !14440 if (n5) 14450, 14525, 14500
14440 if (n5 .lt. 0) then
     go to 14450
  else if (n5 .eq. 0) then
     go to 14525
  else
     go to 14500
  end if
  ! Ametani propagation convolution initialization
14450 n5 = n3 - n5 - n5 - 2
  i0 = 1
  !  assign 14470 to moon
  moon = 14470
  go to 14700
  ! ... Ametani initialization ...
14470 n3 = n5 + 2
  go to 14525
  ! Semlyen propagation convolution initialization.
14500 n5 = n3 + 5 * n5 - 5
  i0 = 2
  !  assign 14520 to moon
  moon = 14520
  go to 14800
  ! ... Semlyen initialization ...
14520 n3 = n5 + 5
14525 if (itadd .lt. 0 .or. indhst(k) .ge. 0) go to 14550
  n8 = n1 + i - 1
  n7 = -kbus(n8)
  n8 = iabs (mbus(n8))
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14540) bus(n7), bus(n8), n1, n2, d3, d4
14540 format (/, 2x, a6, 5x, a6, 5x, i2, 6x, i2, 6x, e15.8, 5x, e15.8)
  ! Transmission line characteristic admittance convolution initializatio
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14415) trrsum, trisum
14550 n5 = ck(k)
  if (itadd .lt. 0 .or. indhst(k) .ge. 0) go to 14560
  if (itadd .ne. 11911) go to 14555
  d1save = d1
  d1 = trrsum * d1save - trisum * d2
  d2 = trisum * d1 + trrsum * d2
  d5save = d5
  d5 = trrsum * d5save - trisum * d6
  d6 = trisum * d5 + trrsum * d6
14555 continue
  n8 = i8 + n2
  d1 = volt(n2) - 2.0 * semaux(n8)
  n8 = n23 + n2
  d2 = volti(n2) - 2.0 * semaux(n8)
  n8 = i10 + n2
  d5 = voltk(n2) - 2.0 * semaux(n8)
  n8 = i11 + n2
  ndx1 = lsiz26 + n2
  d6 = vim(ndx1) - 2.0 * semaux(n8)
  n8 = kodsem(k)
  d3 = sconst(n8) * d1
  d4 = sconst(n8) * d5
  trrsum = sconst(n8)
  trisum = 0.0
  n8 = i2 + n2
  soutk = semaux(n2)
  soutm = semaux(n8)
  if (n5 .ge. 0) go to 14560
  d9 = -sconst(n3 + 1) * volt(n2) * dthaf
  d10 = -sconst(n3 + 1) * voltk(n2) * dthaf
  !14560 if (n5) 14570, 14580, 14610
14560 if (n5 .lt. 0) then
     go to 14570
  else if (n5 .eq. 0) then
     go to 14580
  else
     go to 14610
  end if
  !  Ametani characteristic admittance convolution initialization
14570 n5 = n3 - n5 - n5 - 2
  i0 = 1
  !  assign 14580 to moon
  moon = 14580
  go to 14700
  !     ...  Ametani initialization  ...
14580 if (itadd .lt. 0 .or. indhst(k) .ge. 0) go to 14670
  go to 14650
  !  Semlyen characteristic admittance convolution initialization
14610 n5 = n3 + 4 * (n5 - 1)
  i0 = 1
  !  assign 14630 to moon
  moon = 14630
  go to 14800
  !     ...  semlyen initialization  ...
14630 if (itadd .lt. 0 .or. indhst(k) .ge. 0) go to 14670
14650 n3 = i2 + n2
  n4 = i8 + n2
  n5 = i10 + n2
  if (iprsup  .ge. 1) write (unit = lunit(6), fmt = 14660) d3, d4, semaux(n4), semaux(n5), semaux(n2), semaux(n3)
14660 format (' ', 79x, e15.8, 5x, e15.8, /, 35x, 4(5x, e15.8))
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14415) trrsum, trisum
14415 format (' trrsum and trisum are', 5x, 2e15.7)
  !
14670 indhst(k) = iabs(indhst(k))
  nr(k) = iabs(nr(k))
  k = k + 1
  if (cki(k - 1) .ge. 0.0) go to 14105
  if (iprsup .lt. 6) go to 610
  k = i
  n7 = 1
14680 write (unit = lunit(6), fmt = 14999) k, kbus(k), mbus(k), nr(k), length(k), kodsem(k), indhst(k), kodebr(k), ci(k), ck(k), cik(k), cki(k)
  n1 = iabs (kodsem(k))
  n2 = iabs (indhst(k))
  n3 = n1 - 1
  n4 = n2 - 1
  n8 = n4
  if (n7 .le. iabs (kodebr(k))) n8 = n8 + 6
  if (kodsem(k) .gt. 0) go to 14682
  n3 = n3 + 2 - ck(k) - ck(k)
  n4 = n4 + 2
  if (ck(k) .lt. 0.0) go to 14681
  n3 = n3 + 6 * ck(k)
  n4 = n4 + ck(k) - 1
  go to 14685
14681 n5 = nr(k)
  n6 = n6 + n5 + length(k) - 1
  if (nr(k) .gt. 0) write (unit = lunit(6), fmt = 14998) n5, n6, (xk(l), l = n5, n6)
  if (nr(k) .lt. 0) write (unit = lunit(6), fmt = 14997) n5, n6, (xm(l), l = n5, n6)
  go to 14685
14682 n3 = n3 + 1 - ci(k) - ci(k)
  n4 = n4 + 2
  if (ci(k) .lt. 0.0) go to 14683
  n3 = n3 + 7 * ci(k)
  n4 = n4 + 2 * ci(k) - 2
14683 n3 = n3 - ck(k) - ck(k)
  n4 = n4 + 2
  if (ck(k) .lt. 0.0d0) go to 14684
  n3 = int (n3 + 6 * ck(k))
  n4 = int (n4 + 2 * ck(k) - 2)
14684 n5 = nr(k)
  n6 = n5 + length(k) - 1
  if (ck(k) .lt. 0.0) n6 = n6 + int (sconst(n3 - 1) / deltat + 2.0d0)
  write (unit = lunit(6), fmt = 14998) n5, n6, (xk(l), l = n5, n6)
  write (unit = lunit(6), fmt = 14997) n5, n6, (xm(l), l = n5, n6)
14685 write (unit = lunit(6), fmt = 14996) n1, n3, (sconst(l), l = n1, n3)
  if (n8 .gt. n4) n4 = n8
  write (unit = lunit(6), fmt = 14995) n2, n4, (cnvhst(l), l = n2, n4)
  k = k + 1
  n7 = n7 + 1
  if (cki(k - 1) .gt. 0.0) go to 14680
14999 format (//, 1x, 'k = ', i5, 5x, 'kbus = ', i5, 5x, 'mbus = ', i5, 5x, 'nr = ', i5, 5x, 'length = ', i5, 5x, 'kodsem = ', i5, /, 1x, 'indhst = ', &
       i5, 5x, 'kodebr = ', i5, 5x, 'ci = ', f5.0, 5x, 'ck = ', f5.0, 5x, 'cik = ', f5.0, 5x, 'cki = ', f5.0)
14998 format (//, 5x, '(xk    (l), l=', i10, ', ', i10, ')..', /, (8(1x, e15.7)))
14997 format (//, 5x, '(xm    (l), l=', i10, ', ', i10, ')..', /, (8(1x, e15.7)))
14996 format (//, 5x, '(sconst(l), l=', i10, ', ', i10, ')..', /, (8(1x, e15.7)))
14995 format (//, 5x, '(cnvhst(l), l=', i10, ', ', i10, ')..', /, (8(1x, e15.7)))
  go to 610
  ! Ametani piecewise-linear convolution initialization
14700 if (itadd .lt. 0 .or. indhst(k) .ge. 0) go to 14710
  d1 = d1 / wdthaf
  d2 = d2 / wdthaf
  if (i0 .lt. 0) go to 14710
  d5 = d5 / wdthaf
  d6 = d6 / wdthaf
14710 do j = n3, n5, 2
     d15 = dthaf * sconst(j + 1)
     if (kodebr(k) .gt. 0) sconst(j + 1) = d15
     if (itadd .lt. 0 .or. indhst(k) .ge. 0) cycle
     d8 = w * sconst(j)
     d7 = cosz ( d8 )
     d8 = sinz ( d8 )
     if (j .eq. n3) go to 14720
     d11 = d16 * (d1 * (d8 - d14) - d2 * (d7 - d13))
     d3 = d3 + d11
     d9 = d9 + d11
     if (i0 .lt. 0) go to 14720
     d12 = d16 * (d5 * (d8 - d14) - d6 * (d7 - d13))
     d4 = d4 + d12
     d10 = d10 + d12
14720 d13 = d7
     d14 = d8
     d16 = d15
  end do
  if (itadd .lt. 0) go to 14750
  if (indhst(k) .le. 0) go to 14740
  cnvhst(n4) = 0.0
  n4 = n4 + 1
  if (i0 .lt. 0) go to 14750
  cnvhst(n4) = 0.0
  n4 = n4 + 1
  go to 14750
14740 d8 = w * tmax
  d7 = cosz (d8)
  d8 = sinz (d8)
  d11 = d16 * (d1 * (d8 - d14) - d2 * (d7 - d13))
  d3 = d3 + d11
  cnvhst(n4) = d9 + d11
  n4 = n4 + 1
  if (i0 .lt. 0) go to 14750
  d12 = d16 * (d5 * (d8 - d14) - d6 * (d7 - d13))
  d4 = d4 + d12
  cnvhst(n4) = d10 + d12
  n4 = n4 + 1
  !14750 go to moon, (14340, 14470, 14580)
14750 select case (moon)
  case (14340)
     go to 14340

  case (14390)
     go to 14390

  case (14470)
     go to 14470

  case (14520)
     go to 14520

  case (14580)
     go to 14580

  case (14630)
     go to 14630
  end select
  ! Semlyen exponential convolution initialization
14800 n10 = 4
  dxp = 1.0d0
  dxq = 0.0d0
  if (iabs (i0) .le. 1) go to 14810
  d7 = eeeta / deltat
  n7 = int (d7)
  dxq = d7 - n7
  dxp = unity - dxq
  n10 = 5
  if (itadd .lt. 0 .or. indhst(k) .ge. 0) go to 14810
  d8 = w * eeeta
  d7 = cosz (d8)
  d8 = -sinz (d8)
  d9 = d1 * d7 - d2 * d8
  d2 = d1 * d8 + d2 * d7
  d1 = d9
  if (i0 .lt. 0) go to 14810
  d9 = d5 * d7 - d6 * d8
  d6 = d5 * d8 + d6 * d7
  d5 = d9
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14802) d1, d2, d5, d6
14802 format (' values of d1, d2, d5, d6 before do 14940 loop', 4e17.8)
14810 do j = n3, n5, n10
     stype = sconst(j)
     !     if (stype) 14940, 14820, 14830
     if (stype .lt. 0) then
        cycle
     else if (stype .eq. 0) then
        go to 14820
     else
        go to 14830
     end if
14820 d7 = sconst(j + 1)
     d8 = 0.0d0
     d11 = sconst(j + 2)
     d12 = 0.0d0
     if (d7 .ne. 0.0d0) go to 14826
     sconst(j+2) = dxp*d11
     sconst(j+3) = dxq*d11
     if (i0.eq.2) sconst(j+4) =0.0
     cnvhst(n4) = 0.0
     cnvhst(n4+1) = 0.0
     n4 = n4 + 2
     if (indhst(k) .ge. 0) cycle
     cnvhst(n4-2) = d1*d11
     d3 = d3 + d1*d11
     cnvhst(n4-1) = d5*d11
     d4 = d4 + d5*d11
     cycle
14826 d9  = d11 * d7
     d10 = 0.0
     go to 14840
14830 n9 = j + n10
     d7 = sconst(j + 1)
     d8 = sconst(n9 + 1)
     d11 = sconst(j + 2)
     d12 = sconst(n9 + 2)
     d9  = d11 * d7  -  d12 * d8
     d10 = d11 * d8  +  d12 * d7
     !
14840 d13 = deltat * d7
     d14 = deltat * d8
     if (d13 * d13 + d14 * d14 .gt. 0.0001) go to 14880
     !
     n6 = 0
     n8 = 1
     d20 = epsiln * epsiln
     dblpr1 = 0.0
     dblpr2 = 0.0
     dblpr3 = 0.0
     dblpr4 = 0.0
     d15 = - unity
     d16 = 0.0
14850 n6 = n6 + 1
     if (n6 .lt. 15) go to 14870
     n8 = i + n1 - 1
     n7 = - kbus(n8)
     n8 = iabs (mbus(n8))
     write (unit = lunit(6), fmt = 14860) bus(n7), bus(n8), n1, n2, i0, d17, d18, flzero
14860 format (//, " Warning...  The Taylor's series used to calculate the Semlyen convolution coeffiecient connected between ", '"' a6, '"', ' and ', "'", a6, "'", /, 13x, 'has failed to converge.  The matrix entry is (', i2, ',', i2,  '), type ', i2, '.  Relative error is ', e15.8, ' and ', e12.5, '.', / ,13x,'The convergence tollerance is ', e12.5, ".  Computation will continue using the 'dexpz / dsinz / dcosz' routines.")
     go to 14880
14870 n7 = n8
     n8 = n8 * (n6 + 1)
     d17 = -d15 * d13 + d16 * d14
     d16 = - d15 * d14 - d16 * d13
     d15 = d17
     d17 = d15 / n8
     d18 = d16 / n8
     dblpr1 = dblpr1 + d17
     dblpr2 = dblpr2 + d18
     d17 = (d17 * d17  + d18 * d18) / (dblpr1*dblpr1 + dblpr2 * dblpr2)
     d18 = n8 - n7
     d18 = d18 / n8 / n7
     d19 = d15 * d18
     d18 = d16 * d18
     dblpr3 = dblpr3 + d19
     dblpr4 = dblpr4 + d18
     d18 = (d19 * d19 + d18 * d18) / (dblpr3 * dblpr3 + dblpr4 * dblpr4)
     if (d18 .gt. d20 .or. d17 .gt. d20) go to 14850
     d15 = d11 * dblpr1 - d12 * dblpr2
     d16 = d11 * dblpr2 + d12 * dblpr1
     if (kodebr(k) .lt. 0) go to 14890
     d13 = expz (-d13)
     sconst(j + 1) = d13 * cosz (d14)
     sconst(j + 2) = d15
     sconst(j + 3) = d11 * dblpr3 - d12 * dblpr4
     if (stype .le. 0.0) go to 14890
     sconst(n9 + 1) = -d13 * sinz (d14)
     sconst(n9 + 2) = d16
     sconst(n9 + 3) = d11 * dblpr4 + d12 * dblpr3
     go to 14890
14880 dblpr1 = - d13
     call dexpz (dblpr1, dblpr2)
     dblpr1 = -d14
     call dcosz (dblpr1, dblpr3)
     call dsinz (dblpr1, dblpr4)
     dblpr1 = dblpr2 * dblpr3
     dblpr2 = dblpr2 * dblpr4
     d17 = d13 * d13  +  d14 * d14
     dblpr3 = ((unity - dblpr1) * d13 - dblpr2 * d14) / d17
     dblpr4 = (-(unity - dblpr1) * d14 - dblpr2 * d13) / d17
     d15 = d11 * (unity - dblpr3) + d12 * dblpr4
     d16 = -d11 * dblpr4 + d12 * (unity - dblpr3)
     if (kodebr(k) .lt. 0) go to 14890
     sconst(j + 1) = dblpr1
     sconst(j + 2) = d15
     sconst(j + 3) = d11 * (dblpr3 - dblpr1) - d12 * (dblpr4 - dblpr2)
     if (stype .le. 0.0) go to 14890
     sconst(n9 + 1) = dblpr2
     sconst(n9 + 2) = d16
     sconst(n9 + 3) = d11 * (dblpr4 - dblpr2) + d12 * (dblpr3 - dblpr1)
14890 if (iabs (i0) .gt. 1) go to 14895
     if (i0 .gt. 0) go to 14893
     cnvhst(n4 + 0) = d15 * (volt(n2) - voltk(n2))
     if (stype .le. 0.0) go to 14900
     cnvhst(n4 + 1) = d16 * (volt(n2) - voltk(n2))
     go to 14900
14893 cnvhst(n4 + 0) = d15 * volt(n2)
     cnvhst(n4 + 1) = d15 * voltk(n2)
     if (stype .le. 0.0) go to 14900
     cnvhst(n4 + 2) = d16 * volt(n2)
     cnvhst(n4 + 3) = d16 * voltk(n2)
     go to 14900
14895 cnvhst(n4 + 0) = 0.0
     cnvhst(n4 + 1) = 0.0
     if (kodebr(k) .lt. 0) go to 14897
     d16 = sconst(j + 2)
     d17 = sconst(j + 3)
     sconst(j + 2) = dxp * d16
     sconst(j + 3) = dxq * d16 + dxp * d17
     sconst(j + 4) = dxq * d17
14897 if (stype .le. 0.0) go to 14900
     cnvhst(n4 + 2) = 0.0
     cnvhst(n4 + 3) = 0.0
     if (kodebr(k) .lt. 0) go to 14900
     d16 = sconst(n9 + 2)
     d17 = sconst(n9 + 3)
     sconst(n9 + 2) = dxp * d16
     sconst(n9 + 3) = dxq * d16 + dxp * d17
     sconst(n9 + 4) = dxq * d17
14900 if (itadd .lt. 0) cycle
     if (indhst(k) .lt. 0) go to 14920
     cnvhst(n4) = 0.0
     n4 = n4 + 1
     if (i0 .lt. 0) go to 14910
     cnvhst(n4) = 0.0
     n4 = n4 + 1
14910 if (stype .le. 0.0) cycle
     cnvhst(n4) = 0.0
     n4 = n4 + 1
     if (i0 .lt. 0) cycle
     cnvhst(n4) = 0.0
     n4 = n4 + 1
     cycle
14920 wfac = tenm6
     wshz = w*wfac
     spole = d7*wfac
     wpole = -d8*wfac
     resr = d9*wfac
     resi = d10*wfac
     tnr = resr * spole - resi * wpole
     tni = resr * wshz
     tdr = spole * spole + wpole * wpole - wshz * wshz
     tdi = 2. * spole * wshz
     fac = 1. / (tdr * tdr + tdi * tdi)
     tdr = tdr * fac
     tdi = tdi * fac
     trr = tdr * tnr + tdi * tni
     tri = tdr * tni - tdi * tnr
     trrsum=trrsum+trr
     trisum=trisum+tri
     d15 = trr * d1 - tri * d2
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14922) d3, trr, tri, d15
14922 format (' Values of d3, trr, tri, d15 before d3 update', 4e17.8)
     d3 = d3 + d15
     if (j .lt. n5 .or. stype .gt. 0.0) go to 14924
     eoutk = d3 - soutk
     d3 = soutk
     d15 = d15 - eoutk
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14923) d15, eoutk
14923 format (' d15 reset to ', e15.8, ' to correct output k error = ', e15.8)
14924 cnvhst(n4) = d15 - cnvhst(n4)
     n4 = n4 + 1
     if (i0 .lt. 0) go to 14930
     d21 = trr * d5 - tri * d6
     if (iprsup.ge.1) write (unit = lunit(6), fmt = 14926) d4, trr, tri, d21
14926 format (' Values of d4, trr, tri, d21 before d4 update', 4e17.8)
     d4 = d4 + d21
     if (j .lt. n5 .or. stype .gt. 0.0) go to 14928
     eoutm = d4 - soutm
     d4 = soutm
     d21 = d21 - eoutm
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14927) d21, eoutm
14927 format (' d21 reset to ', e15.8, ' to correct output m error = ', e15.8)
14928 cnvhst(n4) = d21 - cnvhst(n4)
     n4 = n4 + 1
14930 if (stype .le. 0.0) cycle
     trrsum = trrsum + trr
     trisum = trisum + tri
     d3 = d3 + d15
     tnr = resi * spole + resr * wpole
     tni = resi * wshz
     tir = tdr * tnr + tdi * tni
     tii = tdr * tni - tdi * tnr
     d16 = tir * d1 + tii * d2
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 14932) d16, tir, tii, d16
14932 format (' Values of d16, tir, tii, d16 before d16 update', 4e17.8)
     cnvhst(n4) = d16 - cnvhst(n4)
     n4 = n4 + 1
     if (i0 .lt. 0) cycle
     d4 = d4 + d21
     d22 = tir * d5 + tii * d6
     cnvhst(n4) = d22 - cnvhst(n4)
     n4 = n4 + 1
  end do
  !  go to moon, (14390, 14520, 14630)
  select case (moon)
  case (14390)
     go to 14390

  case (14520)
     go to 14520

  case (14630)
     go to 14630
  end select
650 k = 2 * ifdep
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 653) ( kodsem(i), i = 1, ibr)
653 format (/, ' (kodsem(i), i = 1, ibr)', /, (1x, 10i10))
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 654) (cnvhst(i), i = 1, ifx)
  lstat(28) = iline - 1
  lstat(42) = ifx
654 format (/, ' (cnvhst(i), i = 1, ifx)', /, (1x, 8e16.6))
  i = 0
  if (k .le. 0) go to 6679
  if (iprsup .le. 0) go to 6675
  write (unit = lunit(6), fmt = 2307)
2307 format (/, " Exponential-tail constants ready for deltat-loop convolution, at end  'over13' .", /, 10x, 'i', &
       13x, 'con1(i)', 13x, 'con2(i)', 13x, 'con3(i)')
6675 i = i + 1
  iq2 = 2 * lfdep + i
  iq3 = 4 * lfdep + i
  if (i .gt. k) go to 6679
  d3 = con1(i)
  d4 = con1(iq2)
  d5 = con1(iq3)
  if (d5 .eq. 0.0) go to 2314
  d6 = d4 * deltat
  d1 = expz (-d6)
  d6 = (d1 - 1.0) / d6
  d2 = d3 / d4
  con1(i) = d2 * (1.0 + d6)
  con1(iq2) = -d2 * (d6 + d1)
  con1(iq3) = d1
2314 if (iprsup .ge. 2) write (unit = lunit(6), fmt = 55426) i, con1(i), con1(iq2), con1(iq3)
55426 format (1x, i10, 3e20.11)
  go to 6675
6679 lstat(35) = iftail
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format (' at 4568.')
  if (numsm .gt. 0) go to 9200
  if (kconst .gt. 0) go to 9200
  if (mdrive .gt. 0) go to 9200
  kill = 191
  lstat(19) = 1423
9200 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 9207) kill, lstat(19)
9207 format (' Exit  module "last13".  kill, lstat(19) =', 2i8)
  return
end subroutine last13

!
! end of file over13.f90
!
