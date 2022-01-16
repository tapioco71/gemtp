!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over11.f90
!

module over11mod
  implicit none
  save

contains

  !
  ! subroutine reducn.
  !

  subroutine reducn (x, y, m, n)
    implicit none
    real(8), intent(out) :: x(:)
    real(8), intent(out) :: y(:)
    integer(4), intent(in) :: m
    integer(4), intent(in) :: n
    integer(4) :: i, ik
    integer(4) :: j
    integer(4) :: k
    integer(4) :: mk, mkj, mkmj
    integer(4) :: nk, nkk
    integer(4) :: m1
    real(8) :: a, a1(18)
    real(8) :: b, b1(18)
    real(8) :: c
    real(8) :: d
    !
    j = m
    ik = m * m
    nk = ik - m
    m1 = m + 1
1   c = 1.d0 / (x(ik) ** 2 + y(ik) ** 2)
    d = -y(ik) * c
    c = x(ik) * c
    do k = 1, m
       nkk = nk + k
       b1(k) = y(nkk)
       a1(k) = x(nkk)
    end do
    k = 1
4   mk = (k - 1) * m
    mkj = mk + j
    a = -(x(mkj) * c - y(mkj) * d)
    b = -(x(mkj) * d + y(mkj) * c)
    i = 1
3   mk = mk + 1
    x(mk) = x(mk) + (a * a1(i) - b * b1(i))
    y(mk) = y(mk) + (a * b1(i) + b * a1(i))
    i = i + 1
    if (i .lt. m1) go to 3
    mkmj = mk - m + j
    x(mkmj) = a
    y(mkmj) = b
    k = k + 1
    if (k .eq. j) k = k + 1
    if (k .lt. m1) go to 4
    do k = 1, m
       nkk = nk + k
       x(nkk) = a1(k) * c - b1(k) * d
       y(nkk) = a1(k) * d + b1(k) * c
    end do
    x(ik) = c
    y(ik) = d
    j = j - 1
    ik = ik - m1
    nk = nk - m
    if (j .gt. n) go to 1
    return
  end subroutine reducn

  !
  ! subroutine uncor.
  !

  subroutine uncor (sci, i)
    use blkcom
    use labcom
    use smach
    implicit none
    !     This module is used only by Brandwajn (type-59) s.m. model
    integer(4), intent(in) :: i
    real(8), intent(out) :: sci(:)
    integer(4) :: i26, iw, iwd, iwd1, iwd2
    integer(4) :: j
    integer(4) :: k
    integer(4) :: n, nb, nk
    real(8) :: a1, a2, a3, a4, a5, a18, a19, acde, acdf, ax(9), ay(9)
    real(8) :: cni, cnr
    real(8) :: tra2
    real(8) :: vnr(3), vni(3)
    !
    cni = sci(1)
    cnr = sci(2)
    acde = sci(3)
    acdf = sci(4)
    a19 = sci( 5 )
    tra2 = sci( 6 )
    !     process first the d - axis       *   *   *   *   *   *   *   *   *
    i26 = 101 * i - 100
    n = 0
    iwd1 = 2
    iwd = iwd1
    if (elp(i26 + 7) .eq. 0.0d0) iwd = iwd - 1
    iw = iwd * iwd
    do j = 1, iw
       ax(j) = 0.0d0
       ay(j) = 0.0d0
    end do
    a18 = (1.0d0 - acde) * elp(i26 + 20)
    ax(1) = -elp(i26 + 6)
    ay(1) = (elp(i26 + 2) - elp(i26 + 1) * a18) * tra2
    a1 = elp(i26 + 1) * acde * 2.0d0
    a2 = a19 * a19
    a5 = 1.0d0 / (a2 + 1.0d0)
    a2 = a2 * a5
    a19 = a19 * a5
    a3 = a1 * a19
    a4 = a1 * a2
    vnr(1) = -(a3 * cnr - a4 * cni)
    vni(1) = -(a3 * cni + a4 * cnr)
    if (iwd .eq. 1) go to 3
    ay(2) = elp(i26 + 4) * acde * tra2
    ay(3) = ay(2)
    ax(4) = -elp(i26 + 7)
    ay(4) = (elp(i26 + 5) - elp(i26 + 3) * a18) * tra2
    a1 = elp(i26 + 3) * acde * 2.0d0
    a3 = a1 * a19
    a4 = a1 * a2
    vnr(2) = -(a3 * cnr - a4 * cni)
    vni(2) = -(a3 * cni + a4 * cnr)
3   call reducn (ax, ay, iwd, n)
    nk = 2
    do j = 1, iwd1
       a3 = 0.0d0
       a4 = 0.0d0
       nb = j - iwd
       nk = nk + 1
       if (j .gt. iwd) go to 14
       do k = 1, iwd
          nb = nb + iwd
          a3 = a3 + ax(nb) * vnr(k) - ay(nb) * vni(k)
          a4 = a4 + ax(nb) * vni(k) + ay(nb) * vnr(k)
       end do
14     if (iprsup .gt. 1) write (unit = lunit(6), fmt = 25) j, a3, a4
25     format ('  Negative sequence correction for winding number j =', i5, '   of the d-axis.  a3, a4 follow:', /, 2x, 2e25.15)
       sci(nk) = a4
    end do
    !     process the q - axis     *   *   *   *   *   *   *   *   *   *   *
    iwd2 = 2
    iwd = iwd2
    if (elp(i26 + 15) .eq. 0.0d0) iwd = iwd - 1
    if (elp(i26 + 14) .eq. 0.0d0) iwd = iwd - 1
    if (iwd .eq. 0) go to 13
    iw = iwd * iwd
    do j = 1, iw
       ax(j) = 0.0d0
       ay(j) = 0.0d0
    end do
    a18 = (1.0d0 - acdf) * elp(i26 + 20)
    ax(1) = -elp(i26 + 14)
    ay(1) = (elp(i26 + 10) - elp(i26 + 9) * a18) * tra2
    a1 = elp(i26 + 9) * acdf * 2.0d0
    a3 = a1 * a19
    a4 = a1 * a2
    vnr(1) = -(a3 * cnr - a4 * cni)
    vni(1) = -(a3 * cni + a4 * cnr)
    if (iwd .eq. 1) go to  8
    ay(2) = elp(i26 + 12) * acdf * tra2
    ay(3) = ay(2)
    ax(4) = -elp(i26 + 15)
    ay(4) = (elp(i26 + 13) - elp(i26 + 11) * a18) * tra2
    a1 = elp(i26 + 11) * acdf * 2.0d0
    a3 = a1 * a19
    a4 = a1 * a2
    vnr(2) = -(a3 * cnr - a4 * cni)
    vni(2) = -(a3 * cni + a4 * cnr)
8   call reducn (ax, ay, iwd, n)
13  do j = 1, iwd2
       a3 = 0.0d0
       a4 = 0.0d0
       nb = j - iwd
       nk = nk + 1
       if (j .gt. iwd) go to 15
       do k = 1, iwd
          nb = nb + iwd
          a3 = a3 + ax(nb) * vnr(k) - ay(nb) * vni(k)
          a4 = a4 + ax(nb) * vni(k) + ay(nb) * vnr(k)
       end do
15     if (iprsup .gt. 1) write (unit = lunit(6), fmt = 26) j, a3, a4
26     format ('  Negative sequence correction for winding number j =', i5, '   of the q-axis.  a3, a4 follow:', /, 2x, 2e25.15)
       sci(nk) = a3
    end do
    if (iprsup .gt. 1) write (unit = lunit(6), fmt = 30) (sci(k), k = 1, nk)
30  format ('  Negative sequence corrections upon exit from "uncor".   sci(1:nk)  follow ....', /,  (2x, 6e21.12))
    return
  end subroutine uncor

end module over11mod

!
! subroutine over11.
!

subroutine over11
  use a8swmod
  use linemodel
  use blkcom
  use labcom
  use space2
  use dekspy
  use tracom
  use bcddat
  use bcdtim
  use movcop
  use veccom
  implicit none
  !
  character(6) ::  chhold
  character(6) :: text1, text2, text3, text4, text5, text6
  character(8) :: aupper(13)
  integer(4) :: i, ib, ibf, ihalf, ij, ijk, ik, in, ip, iq, isubs1, isubs2, ix
  integer(4) :: iy, iz
  integer(4) :: j, ja, jb, jj, jn, jpp, js, jtemp
  integer(4) :: k, k1, kd, kib, kkk, kt
  integer(4) :: l, ld, ldd, ll0, ll1, ll2, llm1, ltemp
  integer(4) :: m, mib, mk, mt
  integer(4) :: n1, n2, n5, n6, n7, n8, n9, n13, n15, n16, n18, n19, n22, n28
  integer(4) :: n29, n71, n74, n77, n78, n85, n899, ndum, ndx1, ndx2, ndx3, ndx4
  integer(4) :: ndx5, nf, nfsout, nn, nref, nrow1, nt1, nt2, num
  real(8) :: a, ang
  real(8) :: clii, clir, clji, cljr, csi, csr, cti, ctr, curik, curim, currk
  real(8) :: currm
  real(8) :: d1, d2, d3, d4, d5, d6, d26, dd, dei, der, dum123
  real(8) :: eik, eim, erk, erm
  real(8) :: fshold(30000)
  real(8) :: gus1, gus2, gus3, gus4
  real(8) :: h1, h2, h3
  real(8) :: omegac, omegal
  real(8) :: power
  real(8) :: rd, react
  real(8) :: sfrold
  real(8) :: vi, vr
  real(8) :: xd, xxx
  real(8) :: yi, yis, yr, yrs, yy
  !
  !  dimension aupper(13)
  !  equivalence (volt(1), vim(1))
  !  equivalence (imfd(1), jch2(1))
  !  equivalence (moncar(1), knt)
  !      Preceding "jch2" uses "imfd" just for "frequency scan".
  !      as such, there must be no freq-depend sources present.
  !
  integer(4), pointer :: jch2(:)
  integer(4), pointer :: knt => moncar(1)
  real(8), pointer :: vim(:)
  !
  data text1 / 'mag   ' /
  data text2 / 'angle ' /
  data text3 / 'real  ' /
  data text4 / 'imag  ' /
  data text5 / 'pctmag' /
  data text6 / 'pctang' /
  data nfsout / 0 /
  !
  jch2 => imfd
  vim => volt
  locatn(i, j) = (j * j - j) / 2 + i
  ll2 = 2
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module over11.')
  if (iprsup .ge. 3) kssout = 1
  llm1 = -1
  ll0 = 0
  ll1 = 1
  ll2 = 2
  ib = 1
  sfrold = -fltinf
  if ((kssout .ne. 1) .and. (kssout .ne. 3)) go to 6010
  n6 = 0
  do k = 1, ibr
     j = iabs (kbus(k))
     if (j .eq. 1) j = iabs (mbus(k))
     if (j .le. 1) go to 1854
     m = iabs (kssfrq(j))
     if (n6 .le. 0) go to 1846
     ndx1 = lsiz26
     do i = 1, n6
        ndx1 = ndx1 + 1
        if (sfreq(m) .eq. vim(ndx1)) go to 1854
     end do
1846 n6 = n6 + 1
     ndx1 = lsiz26 + n6
     vim(ndx1) = sfreq(m)
     if (n6 .lt. lsiz26) go to 1854
     write (unit = lunit(6), fmt = 1851) lsiz26
1851 format (' Number of phasor frequencies exceeds list size 26 (working vector usage).')
     kill = 1
     lstat(19) = 1854
     lstat(17) = 26
     go to 9800
1854 continue
  end do
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 6003) vim(lsiz26 + 1)
6003 format (//, ' Sinusoidal steady state solution, branch by branch.  All flows are away from bus, and real part, magnitude, or p', /, ' is printed above the imaginary part, the angle, or q.   First solution frequency =', e18.9, '   Hertz.')
  if (kol132 .ne. 132) write (unit = lunit(6), fmt = 6004) vim(lsiz26 + 1)
6004 format (/, ' 80-column phasor branch flows.  Reverse flows omitted.  1st freq =',  e13.5, /, '  bus k', 13x, '   vk-amplitude', '   vm-amplitude  ikm-amplitude   pkm (Watts)', /, 15x, 'bus m   vk-degrees     vm-degrees     ikm-degrees    qkm (vars)')
  ndx1 = lsiz26 + 2
  ndx2 = lsiz26 + n6
  if (n6 .ge. 2) write (unit = lunit(6), fmt = 6005) (vim(i), i = ndx1, ndx2)
6005 format (' Added subnetwork frequencies :', 6e14.5)
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 6007)
6007 format (2x, 'Bus k', 37x, 'Node voltage', 21x, 'Branch current', 10x, 'Power flow', 5x, 'Power loss', /, 16x, 'Bus m', 2(9x, 'Rectangular', 10x, 'Polar'), 5x,  2(8x, 'p and q)'))
6010 if (ibr .lt. ib) go to 6200
  xopt = xoptbr(ib)
  copt = coptbr(ib)
  d3 = tenm3
  d4 = unity / 1000000.0d0
  if (xopt .gt. 0.0d0) d3 = 1.0d0 / (twopi * xopt)
  if (copt .gt. 0.0d0) d4 = d4 / (twopi * copt)
  jb = iabs (kbus(ib))
  if (jb .eq. 1) jb = iabs (mbus(ib))
  n16 = iabs (kssfrq(jb))
  omegal = d3 * twopi * sfreq(n16)
  omegac = d4 * twopi * sfreq(n16)
  if (imodel(ib) .eq. -4) omegac = 1.0d0
  if (sfreq(n16) .eq. sfrold) go to 6017
  if ((kssout .ne. 1) .and. (kssout .ne. 3)) go to 6016
  if (ib .eq. 1) go to 6016
  write (unit = lunit(6), fmt = 6014) sfrold, sfreq(n16)
6014 format ('      %%%%%  Frequency discontinuity:   end', e13.4, '  Hz.,   begin branches excited at', e18.9, '   Hertz.   %%%%%')
6016 sfrold = sfreq(n16)
6017 jb = iabs (nr(ib))
  l = iabs (length(ib))
  if (kodebr(ib) .ne. -2) go to 6030
  if ((kodsem(ib) .ne. 0) .and. (imodel(ib) .ne. -2)) go to 6030
  nphcas = iabs (length(ib))
  do ix = 1, nphcas
     iy = ib + ix - 1
     nrow1 = kbus(iy)
     nrow1 = norder(nrow1)
     e(ix) = solr(nrow1)
     f(ix) = soli(nrow1)
     nrow1 = mbus(iy)
     nrow1 = norder(nrow1)
     n1 = ix + nphcas
     e(n1) = solr(nrow1)
     f(n1) = soli(nrow1)
  end do
  call mult (tx(jb), f(1), diag(1), ll2 * nphcas, ll0)
  call mult (tr(jb), e(1), diag(1), ll2 * nphcas, llm1)
  call mult (tr(jb), f(1), diab(1), ll2 * nphcas, ll0)
  call mult (tx(jb), e(1), diab(1), ll2 * nphcas, ll1)
  if (kssout .ne. 1 .and. kssout .ne. 3) go to 7003
  do ix = 1, nphcas
     iy = ib + ix - 1
     iz = ix + nphcas
     call ssout (iy, e(ix), f(ix), e(iz), f(iz), diag(ix), diab(ix), diag(iz), diab(iz))
  end do
7003 ib = ib + nphcas
  go to 6010
6030 if (nr(ib) .ge. 0) go to 6060
  !              Series   R - L - C   calculations
  in = iabs (kbus(ib))
  in = norder(in)
  jn = iabs (mbus(ib))
  jn = norder(jn)
  if (c(jb) .eq. 0.0d0) go to 6044
  xxx = tx(jb) * omegal - 1.0d0 / (c(jb) * omegac)
  go to 6048
6044 xxx = tx(jb) * omegal
6048 dd = tr(jb) * tr(jb) + xxx * xxx
  rd = tr(jb) / dd
  xd = -xxx / dd
  erk = solr(in)
  eik = soli(in)
  erm = solr(jn)
  eim = soli(jn)
  der = erk - erm
  dei = eik - eim
  currk = rd * der - xd * dei
  curik = rd * dei + xd * der
  ci(ib) = -currk
  ck(ib) = 0.0d0
  if (c(jb) .ne. 0.0d0) ck(ib) = curik / (c(jb) * omegac)
  currm = -currk
  curim = -curik
  if (kssout .eq. 1 .or. kssout .eq. 3) call ssout (ib, erk, eik, erm, eim, currk, curik, currm, curim)
  cik(ib) = 0.0d0
  ib = ib + 1
  go to 6010
6060 if (l .gt. 1) go to 6080
  !              Single  pi  section  calculations
  kib = iabs (kbus(ib))
  in = norder(kib)
  mib = iabs(mbus(ib))
  jn = norder(mib)
  yr = tr(jb)
  yi = tx(jb)
  if (iprsup .ge. 4) write (unit = *, fmt = *) 'i, jb, tr(jb), tx(jb), imodel(ib)= ',i, jb, tr(jb), tx(jb), imodel(ib)
  if (imodel(ib) .eq. -4) go to 6067
  yi = tx(jb) * omegal
  yy = tr(jb) * tr(jb) +yi *yi
  yr = tr(jb) / yy
  yi = -yi / yy
6067 erk = solr(in)
  eik = soli(in)
  erm = solr(jn)
  eim = soli(jn)
  der = erk - erm
  dei = eik - eim
  ctr = yr * der - yi * dei
  cti = yr * dei + yi * der
  yis = c(jb) * omegac
  if (kbus(ib) .ge. 0) go to 6076
  yrs = r(jb)
  csr = yrs * erk - yis * eik
  csi = yrs * eik + yis * erk
  currk = ctr + csr
  curik = cti + csi
  tr(jb) = currk
  tx(jb) = curik
  csr = yrs*erm - yis*eim
  csi = yrs*eim + yis*erm
  currm = csr - ctr
  curim = csi - cti
  r(jb) = currm
  c(jb) = curim
  go to 6078
6076 cik(ib) = ctr
  currk = -yis * eik
  curik = yis*erk
  currm = - yis*eim
  curim = yis * erm
  ci(ib) = currk
  ck(ib) = currm
  currk = currk + ctr
  curik = curik + cti
  currm = currm - ctr
  curim = curim - cti
6078 if (kssout .eq. 1  .or. kssout .eq. 3) call ssout (ib, erk, eik, erm, eim, currk, curik, currm, curim)
  ib = ib + 1
  go to 6094
  !              multiple  pi  section  calculations
6080 ibf = ib - 1
  ld = jb -1
  ldd = jb -1
6090 jb = ibf
  clir = 0.0d0
  clii = 0.0d0
  cljr = 0.0d0
  clji = 0.0d0
  ctr = 0.0d0
  cti = 0.0d0
6100 ld = ld +1
  jb = jb +1
  ia = iabs (kbus(jb))
  ia = norder(ia)
  ja = iabs (mbus(jb))
  ja = norder(ja)
  der = solr(ia) - solr(ja)
  dei = soli(ia) - soli(ja)
  csr = omegac * c(ld)
  ctr = ctr + tr(ld) * der - tx(ld) * dei
  cti = cti + tr(ld) * dei + tx(ld) * der
  clir = clir - csr * soli(ia)
  clii = clii + csr * solr(ia)
  cljr = cljr - csr * soli(ja)
  clji = clji + csr * solr(ja)
  if (kbus(jb) .ge. 0) go to 6110
  clir = clir + r(ld) * solr(ia)
  clii = clii + r(ld) * soli(ia)
  cljr = cljr + r(ld) * solr(ja)
  clji = clji + r(ld) * soli(ja)
6110 if (jb .lt. ib) go to 6100
  kd = ld
  erk = solr(ia)
  eik = soli(ia)
  erm = solr(ja)
  eim = soli(ja)
6140 if (jb .eq. ibf + l) go to 6160
  kd = kd + jb - ibf
  jb = jb + 1
  ia = iabs(kbus(jb))
  ia = norder(ia)
  ja = iabs(mbus(jb))
  ja = norder(ja)
  der = solr(ia) - solr(ja)
  dei = soli(ia) - soli(ja)
  csr = omegac * c(kd)
  ctr = ctr + tr(kd) * der - tx(kd) * dei
  cti = cti + tr(kd) * dei + tx(kd) * der
  clir = clir - csr * soli(ia)
  clii = clii + csr * solr(ia)
  cljr = cljr - csr * soli(ja)
  clji = clji + csr * solr(ja)
  if (kbus(jb) .ge. 0) go to 6140
  clir = clir  +  r(kd) * solr(ia)
  clii = clii  +  r(kd) * soli(ia)
  cljr = cljr  +  r(kd) * solr(ja)
  clji = clji  +  r(kd) * soli(ja)
  go to 6140
6160 currk = ctr + clir
  curik = cti + clii
  currm = -ctr + cljr
  curim = -cti + clji
  if (kbus(ib) .ge. 0) go to 6164
  ldd = ldd + 1
  tr(ldd) = currk
  tx(ldd) = curik
  r(ldd) = currm
  c(ldd) = curim
  go to 6166
6164 cik(ib) = ctr
  ci(ib) = clir
  ck(ib) = cljr
6166 if (kssout .eq. 1 .or. kssout .eq. 3) call ssout (ib, erk, eik, erm, eim, currk, curik, currm, curim)
  ib = ib + 1
  if (ib .le. ibf + l) go to 6090
6094 if (kodsem(ib - 1) .eq. 0 .or. imodel(ib - 1) .eq. -2) go to 6010
  if (imodel(ib - 1) .eq. -4) go to 6010
6095 if (cki(ib - 1) .lt. 0.0d0) go to 6010
  ib = ib + 1
  go to 6095
6200 n1 = 2
  call vecsav (diag, ntot, n1)
  call vecsav (diab, ntot, n1)
  call vecsav (gnd(iofgnd + 1 :), ioffd, n1)
  call vecsav (bnd(iofbnd + 1 :), ioffd, n1)
  if (ncurr .le. 0) ncurr = 1
  call move0 (e(1 :), ntot)
  call move0 (f(1 :), ntot)
  do i = 1, ntot
     vr = solr(i)
     vi = soli(i)
     jj = index(i)
     kkk = index(i + 1)
     e(i) = e(i) + diag(i) * vr - diab(i) * vi
     f(i) = f(i) + diag(i) * vi + diab(i) * vr
     go to 7370
     do
        jj = jj + 1
7370    if (jj .ge. kkk) go to 7380
        j = iloc(jj)
        isubs1 = iofgnd + jj
        isubs2 = iofbnd + jj
        der = gnd(isubs1) * vr - bnd(isubs2) * vi
        dei = gnd(isubs1) * vi + bnd(isubs2) * vr
        e(j) = e(j) + der
        f(j) = f(j) + dei
        e(i) = e(i) + gnd(isubs1) * solr(j) - bnd(isubs2) * soli(j)
        f(i) = f(i) + gnd(isubs1) * soli(j) + bnd(isubs2) * solr(j)
     end do
7380 continue
  end do
  if (kssout .eq. 0) go to 7394
  d26 = 0.0d0
  do i = 1, ntot
     d26 = d26 + e(i) * solr(i) + f(i) * soli(i)
  end do
  d26 = d26 * onehaf
  write (unit = lunit(6), fmt = 2008) d26
2008 format (/, 8x, 'Total network loss "ploss" by summing nodal injections =', e20.10)
7394 if (kswtch .le. 0) go to 7396
  do i = 1, kconst
     if (node(i) .gt. 0) cycle
     if (tstart(i) .ge. 0) cycle
     if (iabs (iform(i)) .ne. 14) cycle
     kt = iabs (node(i))
     k = norder(kt)
     e(k) = e(k) - crest(i) * cosz (time1(i))
     f(k) = f(k) - crest(i) * sinz (time1(i))
  end do
  call move0 (nextsw(1 :), kswtch)
  call move0 (energy(1 :), kswtch)
  do i = 1, kswtch
     if (adelay(i) .eq. 44444.0d0) cycle
     if (tclose(i) .lt. 0.0d0) nextsw(i) = 88
  end do
  n8 = 0
8516 n9 = 0
  n5 = 0
  do i = 1, kswtch
     if (nextsw(i) .eq. 87) cycle
     if (nextsw(i) .eq. 88) go to 8521
     n9 = n9 + 1
     cycle
8521 kt = kmswit(i)
     k = norder(kt)
     ndx5 = lswtch + i
     mt = kmswit(ndx5)
     m = norder(mt)
     n13 = k
4511 if (n13 .eq. norder(1)) go to 4544
     do j = 1, kswtch
        if (nextsw(j) .lt. 88) cycle
        if (i .eq. j) cycle
        nt1 = kmswit(j)
        n1 = norder(nt1)
        ndx3 = lswtch + j
        nt2 =kmswit(ndx3)
        n2 = norder(nt2)
        if (n13 .eq. n1) go to 4544
        if (n13 .eq. n2) go to 4544
     end do
     do j = 1, kconst
        if (node(j) .lt. 0) cycle
        js = norder(node(j))
        if (js .ne. n13) cycle
        if (tstart(j) .ge. 0.0d0) cycle
        if (iabs (iform(j)) .ne. 14) cycle
        go to 4544
     end do
     if (iprsup .ge. 4) write (unit = lunit(6), fmt = 5515) i, k, n13
5515 format (' Current.  i, k, n13 =', 3i8)
     tclose(i) = e(n13)
     energy(i) = f(n13)
     e(n13) = 0.0d0
     f(n13) = 0.0d0
     nextsw(i) = 87
     n8 = n8 + 1
     n5 = 1
     if (n13 .eq. m) m = k
     e(m) = e(m) + tclose(i)
     f(m) = f(m) + energy(i)
     m = norder(mt)
     if (n13 .eq. m) go to 7657
     tclose(i) = -tclose(i)
     energy(i) = -energy(i)
7657 k1 = kdepsw( lswtch + i )
     if (k1 .ne. 8888) cycle
     mk = int (-adelay(i))
     if (mk .le. 0.0d0) cycle
     num = kssfrq(kt)
     ang = twopi * sfreq(num) * deltat
     a8sw(mk + 6) = tclose(i) * cosz(ang) - energy(i) * sinz(ang)
     cycle
4544 if (n13 .eq. m) cycle
     n13 = m
     go to 4511
  end do
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5525) n5, n8, n9
5525 format (' Below 8657.  n5, n8, n9 =', 3i8)
  if (n5 .eq. 0) go to 8669
  go to 8516
8669 n8 = n8 + n9
  if (n8 .eq. kswtch) go to 8681
  write (unit = lunit(6), fmt = 8674) n8, kswtch
8674 format ('Illegal switch/source connection. n8, kswtch =', 2i8)
  kill = 64
  lstat(19) = 8669
  bus1 = trash
  go to 9800
8681 if (kssout .eq. 0) go to 8715
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 8685)
8685 format (' Output for steady state switch current', /, 7x, 'node-k', 4x, 'node-m', 10x, 'i-real', 12x, 'i-imag', 12x, 'i-magn', 10x, 'degrees', 8x, 'power ', 10x, 'reactive')
  if (kol132 .eq. 80) write (unit = lunit(6), fmt = 8686)
8686 format (' Output for steady state switch current', /, 1x, 'node-k', 2x, 'node-m', 10x, 'i-real', 10x, 'i-imag', 10x, 'i-magn', 9x, 'degrees')
  do i = 1, kswtch
     ndx4 = lswtch + i
     k = kmswit(i)
     m = kmswit(ndx4)
     if (nextsw(i) .ne. 87) go to 8695
     d6 = tclose(i) ** 2 + energy(i) ** 2
     d4 = sqrtz (d6)
     d5 = atan2z (energy(i), tclose(i))
     d5 = d5 * 360.0d0 / twopi
     nn = norder(k)
     power = onehaf * (solr(nn) * tclose(i) + soli(nn) * energy(i))
     react = onehaf * (soli(nn) * tclose(i) - solr(nn) * energy(i))
     if (kol132 .eq. 132) write (unit = lunit(6), fmt = 8692) bus(k), bus(m), tclose(i), energy(i), d4, d5, power, react
8692 format (5x, 2a10, 3e18.8, f13.4, 2e18.8)
     if (kol132 .eq. 80) write (unit = lunit(6), fmt = 8693) bus(k), bus(m), tclose(i), energy(i), d4, d5
8693 format (1x, a6, 2x, a6, 3e16.7, f16.6)
     cycle
8695 if (adelay(i) .eq. 44444.0d0) go to 8703
     if (tclose(i) .gt. 0.0d0) go to 8703
     if (kol132 .eq. 132) write (unit = lunit(6), fmt = 8699) bus(k), bus(m)
8699 format (5x, 2a10, 2(5x, 'close at t=0+ '))
     if (kol132 .eq. 80) write (unit = lunit(6), fmt = 8700) bus(k), bus(m)
8700 format (1x, a6, 2x, a6, 2(5x, 'close at t=0+ '))
     cycle
8703 if (kol132 .eq. 132) write (unit = lunit(6), fmt = 8705) bus(k), bus(m)
8705 format (5x, 2a10, 4(10x, 'open', 6x), 8x, 'open')
     if (kol132 .eq. 80) write (unit = lunit(6), fmt = 8706) bus(k), bus(m)
8706 format (1x, a6, 2x, a6, 3(12x, 'open'))
  end do
8715 ktrlsw(1) = 8877
7396 if (kssout .le. 0) go to 8350
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 7343)
7343 format (/, ' Solution at nodes with known voltage.     Nodes shorted together by switches are shown as a group of names, with', /, " the printed result applying to the composite group.   The entry 'mva' is  sqrt(p**2 + q**2)  in units of power,", /, " while  'p.f.'  is the associated power factor.", /, 17x, 'node', 16x, 'source node voltage', 12x, 'injected source current', 14x, 'injected source power', /, 17x, 'name', 2(9x, 'rectangular', 10x, 'polar'), 13x, 'P and Q', 3x, 'MVA and p.f.')
  if (kol132 .ne. 132) write (unit = lunit(6), fmt = 7346)
7346 format (' 80-column injection printout at nodes with known voltage.', /, 16x, 'node', '  vk-magnitude   ik-magnitude     pk (Watts)   MVA (Watts)', /, 16x, 'name', '  vk-degrees     ik-degrees       qk (vars)    power factor')
8350 do k = 1, ntot
     j = norder(k)
     index(j) = k
  end do
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 7401) (i, norder(i), index(i), kode(i), e(i), f(i), diag(i), diab(i), i = 1, ntot)
7401 format (/, 1x, '     row  norder   index    kode', 14x, 'e',  14x, 'f',  11x, 'diag', 11x, 'diab', /, (1x, 4i8, 4e15.6))
  i = ncurr + 1
7405 if (i .gt. ntot) go to 7456
  j = kode(i)
  if (j .le. i) go to 7410
  e(j) = e(j) + e(i)
  f(j) = f(j) + f(i)
  go to 7440
7410 l = 1
  kkk = index(i)
  if (kkk .eq. 1) go to 7440
  aupper(1) = bus(kkk)
7420 if (j .ge. i) go to 7430
  if (l .lt. 13) l = l + 1
  kkk = index(j)
  aupper(l) = bus(kkk)
  if (aupper(l) .eq. blank) aupper(l) = terra
  j = kode(j)
  go to 7420
7430 if (kssout .eq. 0) go to 7440
  write (unit = lunit(6), fmt = 7431)
  n13 = l - 1
  if (n13 .gt. 0) write (unit = lunit(6), fmt = 7431) (aupper(j), j = 1, n13)
7431 format (15x, a6)
  bus5 = aupper(l)
  ib = -1
  rd = 1.0d0
  call ssout (ib, solr(i), soli(i), rd, rd, e(i), f(i), rd, rd)
7440 i = i + 1
  go to 7405
7456 if (loopss(1) .le. 0) go to 7452
  lastov = nchain
  nchain = 8
  write (unit = lunit(6), fmt = 8457)
8457 format (' Complete another internal phasor solution for u.m.   exit "over11", back to "over8".')
  go to 9900
7452 if (loopss(1) .eq. 0) go to 7453
  kconst = loopss(3)
  ibr = loopss(5)
  !     Work on synchronous machine steady-state solution output, plus
  !     calculation of initial conditions for time  t = 0.0   .
7453 if (numsm .eq. 0) go to 6207
  call smint
  if (kill .gt.  0) go to 9800
6207 do i = 1, ntot
     j = norder(i)
     e(i) = solr(j)
     f(i) = soli(j)
  end do
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 5581)
5581 format (' Steady-state phasor network solution now complete.')
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 5504) (bus(i), i = 1, ntot)
5504 format (' (bus(i), i=1, ntot)  in  over11.', /, (1x, 10a8))
  if (tmax .gt. 0.0d0) go to 9700
  if (fmaxfs .eq. 0.0d0) go to 7027
  if ((knt .eq. 1) .and. (nsolve .eq. 0)) go to 7027
  go to 8102
7027 numnvo = 0
  !     Read input card using cimage.
7030 call cimage
  read (unit = abuff, fmt = 22) ivolt, aupper
22 format (i2, 13a6)
  if (ivolt .eq. 1) go to 7050
  if (ivolt .ne. 0) go to 7030
  n1 = 0
  do k = 1, 13
     bus1 = aupper(k)
     if (bus1 .ne. blank) go to 6847
  end do
  write (unit = lunit(6), fmt = 5416)
5416 format ('+Blank card ending node names for voltage output.')
  go to 8102
6847 write (unit = lunit(6), fmt = 54163)
54163 format ('+Card of bus names for node-voltage output.')
  k = 1
7048 if (k .gt. 13) go to 7030
  bus1 = aupper(k)
  if (bus1 .eq. blank) go to 7040
  do i = 2, ntot
     if (bus1 .eq. bus(i)) go to 7043
  end do
  write (unit = lunit(6), fmt = 7042) bus1
7042 format (5x, "Requested output for nonexistent node  '", a6, "'  will be ignored.")
  go to 7040
7043 numnvo = numnvo + 1
  if ((numnvo .gt. lsiz27) .or. (numnvo .gt. lsiz12)) go to 8080
  jch2(numnvo) = i
7040 k = k + 1
  go to 7048
8080 write (unit = lunit(6), fmt = 8090) numnvo, lsiz12, lsiz27
8090 format (' The number of requests for node voltage outputs exceeds the maximum number  of node voltage outputs ("lsiz12"), or the size', /, ' of list 27 ("lsiz27").  Temporary error,  stop at s.n. 8080 of "over11".', 3i8)
  call stoptp
7050 write (unit = lunit(6), fmt = 54166)
54166 format ('+Request for output of all node voltages.')
  if ((ntot1 .gt. lsiz27) .or. (ntot1 .gt. lsiz12)) go to 8080
  do i = 2, ntot
     jch2(i - 1) = i
  end do
  numnvo = ntot1
  if (aupper(1) .ne. blank) go to 7030
8102 if (fmaxfs .eq. 0.0d0) write (unit = lunit(6), fmt = 8103)
8103 format (//, ' Begin steady-state printout of EMTP output variables.   Node voltage output follows.',/, 7x, 'bus', 14x, 'phasor', 7x, 'angle in', 16x, 'real', 11x, 'imaginary', /, 6x, 'name', 11x, 'magnitude', 8x, 'degrees',  16x, 'part', 16x, 'part')
  if (istead .eq. 0) go to 8100
  if (numnvo .eq. 0) go to 8100
  do k = 1, numnvo
     i = jch2(k)
     h1 = e(i)
     h2 = f(i)
     a = sqrtz (h1 ** 2 + h2 ** 2)
     gus1 = h1
     if (a .eq. 0.0d0) gus1 = 1.0d0
     ck1 = atan2z (h2, gus1) * 360.0d0 / twopi
     if (fmaxfs .eq. 0) write (unit = lunit(6), fmt = 8104) bus(i), a, ck1, h1, h2
8104 format (4x, a6, e20.8, f15.6, 2e20.8)
     solr(k) = a
     soli(k) = ck1
     voltk(k) = h1
     volt(k) = h2
     if (k .lt. lsiz26) cycle
     write (unit = lunit(6), fmt = 9104) lsiz26
9104 format (/, ' Permanent error stop due to overflow of list-26 vectors "voltk", "volt".', i8)
     kill = 1
     lstat(19) = 8105
     lstat(17) = 26
     go to 9800
  end do
8100 if (fmaxfs .gt. 0.0d0) go to 8120
  write (unit = lunit(6), fmt = 8106)
8106 format (/, ' EMTP branch-current output follows', /, 8x, 'from', 10x, 'to', 11x, 'magnitude', 7x, 'angle in', 16x, 'real', 11x, 'imaginary', /, 9x, 'bus', 9x, 'bus', 11x, 'of phasor', 8x, 'degrees', 2(16x, 'part'))
  do i = 1, ibr
     icheck = mbus(i)
     if (icheck .ge. 0) cycle
     n1 = kbus(i)
     n2 = iabs (nr(i))
     h1 = tr(n2)
     icheck = -icheck
     n15 = icheck
     if (n15 .eq. 1) n15 = iabs (n1)
     xopt = xoptbr(i)
     copt = coptbr(i)
     n16 = iabs (kssfrq(n15))
     omega = twopi * sfreq(n16)
     d2 = omega * tenm3
     h3 = 1000.0d0 / d2
     if (xopt .gt. 0.0d0) d2 = omega / (xopt * twopi)
     if (copt .gt. 0.0d0) h3 =  1000000.0d0 * copt * twopi / omega
     if (n1 .gt. 0) go to 8107
     n1 = -n1
     h2 = tx(n2)
     go to 8108
8107 h2 = tx(n2) * d2
     a = c(n2)
     if (a .gt. 0.0d0) h2 = h2 - h3 / a
     a = h1 ** 2 + h2 ** 2
     gus1 = h1 / a
     gus2 = -h2 / a
     gus3 = e(n1) - e(icheck)
     gus4 = f(n1) - f(icheck)
     h1 = gus1 * gus3 - gus2 * gus4
     h2 = gus1 * gus4 + gus2 * gus3
8108 a = sqrtz (h1 ** 2 + h2 ** 2)
     gus1 = h1
     if (a .eq. 0.0d0) gus1 = 1.0d0
     ck1 = atan2z (h2, gus1) * 360.0d0 / twopi
     write (unit = lunit(6), fmt = 8109) bus(n1), bus(icheck), a, ck1, h1, h2
8109 format (6x, a6,  6x, a6, e20.8, f15.6, 2e20.8)
  end do
  if (nv .eq. 0) go to 8120
  write (unit = lunit(6), fmt = 8111)
8111 format (/, ' EMTP branch-voltage output follows (column-80 punches only)', /, 8x, 'from', 10x, 'to', 11x, 'magnitude', 7x, 'angle in', 16x, 'real', 11x, 'imaginary', /,  9x, 'bus', 9x, 'bus', 11x, 'of phasor', 8x, 'degrees', 2(16x, 'part'))
  do i = 1, nv
     n1 = ibrnch(i)
     n2 = jbrnch(i)
     d1 = e(n1) - e(n2)
     d2 = f(n1) - f(n2)
     d3 = sqrtz (d1 ** 2 + d2 ** 2)
     d4 = d1
     if (d3 .eq. 0.0d0) d4 = 1.0d0
     d5 = atan2z (d2, d4) * 360.0d0 / twopi
     write (unit = lunit(6), fmt = 8116) bus(n1), bus(n2), d3, d5, d1, d2
  end do
8116 format (6x, a6, 6x, a6, e20.8, f15.6, 2e20.8)
8120 if (fmaxfs .eq. 0.0d0) go to 7481
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 7457) knt, kconst, fminfs, delffs, fmaxfs
7457 format (/, " End freq. scan loop in  'over11' .     knt  kconst", 9x, 'fminfs', 9x, 'delffs', 9x, 'fmaxfs', /, 35x, 2i8, 3e15.6)
  d4 = fminfs
  if (delffs .lt. 0) d4 = alog1z (d4)
  if (knt .gt. 1 .or. kexact .eq. 88333) go to 7473
  write (unit = lunit(6), fmt = 7467)
7467 format (/, " The   'frequency scan'   output vector has the following format.   Cell number one contains the frequency", /, ' of the phasor solution (or the base-10 logarithm of this, inthe case of geometric frequency spacing).   Cells', /, ' numbered two onward contain the pairs of magnitude and angle of the phasor node voltages.   These pairs are in the', /, ' order requested by the user on the card for selective node voltage output.   For plotting purposes, these output')
  write (unit = lunit(6), fmt = 7468)
7468 format (" Variables are treated as though they were ordinary EMTP branch currents (plot type-code  '9' ).   Both the", /, " magnitude and the angle use the actual node name for the first identifying variable,  and then either   'mag   '   or", /, " 'angle'   for the second.        In response to an ireq request during may of 1981,", /, " rectangular outputs (using 'real' and 'imag' as second identifying names for plotting) have", /, ' been appended.   In the printed output, this alternate rectangular output vector follows the')
  write (unit = lunit(6), fmt = 7466)
7466 format (" Original polar one, with the same node ordering (only with 'real' taking the place of 'mag'", /, " and 'imag' taking the place of 'angle').   There is column alignment (the rectangular", /, ' output for any node is located vertically below the corresponding polar output).')
  if (ivolt .eq. 1) write (unit = lunit(6), fmt = 7469) (bus(i), i = ll2, ntot)
7469 format ( " Actually, the user requested the output of all node voltages by means of a  '1'-punch in column number two.", /, ' In this case, the ordering of nodes for output purposes is as follows ....', /, (1x, 20a6))
  write (unit = lunit(6), fmt = 7470)
7470 format (1x)
7473 if (iplot .lt. 0) go to 7462
  if (knt .gt. 1) go to 7459
  if (nsolve .eq. 1) go to 7459
  n7 = ntot + 4
  if (kexact .eq. 88333) n7 = n7 + 2
  n6 = 0
  n5 = 4 * numnvo
  n85 = n5
  if (kexact .eq. 88333) n85 = 2 * n5
  n71 = 2 * numnvo
  n74 = 0
  if (kexact .eq. 88333) n74 = numnvo
  lstat(32) = n5
  if (kexact .eq. 88333) lstat(32) = n85 + n71
  begmax(1) = 0.0d0
  rewind lunit(4)
  if (kexact .ne. 88333) then
     write (unit = lunit(4)) date1, tclock, n7, n6, n5, n85, (bus(i), i = 1, ntot), text1, text2, text3, text4
  else
     write (unit = lunit(4)) date1, tclock, n7, n6, n5 + n71, n85 + n71, (bus(i), i = 1, ntot), text1, text2, text3, text4, text5, text6
  end if
  n8 = ntot + 1
  n9 = ntot + 2
  n18 = ntot + 3
  n19 = ntot + 4
  n28 = ntot + 5
  n29 = ntot + 6
  ltemp = 1
  if (kexact .eq. 88333) ltemp = 2
  do jj = 1, ltemp
     write (unit = lunit(4)) (jch2(i), jch2(i), i = 1, numnvo)
  end do
  do jj = 1, ltemp
     write (unit = lunit(4)) (jch2(i), jch2(i), i = 1, numnvo)
  end do
  write (unit = lunit(4)) (jch2(i), jch2(i), i = 1, n74)
  do jj = 1, ltemp
     write (unit = lunit(4)) (n8, n9, i = 1, numnvo), (n18, n19, i = 1, numnvo)
  end do
  write (unit = lunit(4)) (n28, n29, i = 1, n74), (n28, n29, i = 1, n74)
  if (iprsup .ge. 1 ) write (unit = lunit(6), fmt = 7477) n5, n6, n7, n8, n9, numnvo, (jch2(i), i = 1, numnvo)
7477 format (/, ' lunit(4) header record.      n5      n6      n7      n8      n9  numnvo', /, 22x,  6i8,  5x, '(jch2(i), i=1, numnvo)  follow ...', /, (1x, 20i6))
7459 if (m4plot .eq. 0) go to 7461
  if (kexact .eq. 88333) go to 3010
3070 volti(1) = d4
  ip = 1
  if (kexact .ne. 88333) go to 3075
  iq = (knt - 1) * 4 * numnvo
  ihalf = iq + 2 * numnvo
  j = 1
  do i = 1, numnvo
     volti(ip + 1) = fshold (iq + j)
     volti(ip + 2) = fshold (iq + j + 1)
     j = j + 2
     ip = ip + 2
  end do
  k = 1
  do i = 1, numnvo
     volti(ip + 1) = fshold (ihalf + k)
     volti(ip + 2) = fshold (ihalf + k + 1)
     k = k + 2
     ip = ip + 2
  end do
3075 do i = 1, numnvo
     volti(ip + 1) = solr(i)
     volti(ip + 2) = soli(i)
     ip = ip + 2
  end do
  do i = 1, numnvo
     volti(ip + 1) = voltk(i)
     volti(ip + 2) = volt(i)
     ip = ip + 2
  end do
  if (kexact .ne. 88333) go to 3066
  n78 = n77 * 2 * n899 + (knt - 1) * 2 * numnvo
  j = 1
  do i = 1, numnvo
     volti(ip + 1) = (volti(j + 1) / volti(j + n77 + 1) - 1.0d0) * 100.0d0
     fshold(n78 + j) = volti(ip + 1)
     volti(ip + 2) = (volti(j + 2) / volti(j + n77 + 2) - 1.0d0) * 100.0d0
     fshold(n78 + j + 1) = volti(ip + 2)
     j = j + 2
     ip = ip + 2
  end do
3066 if (iprsup .ge. 3) write (unit = lunit(6), fmt = *) ' After do 17460, ip,volti, fshold=', ip, (volti(ij), ij = 1, ip), (fshold(ij), ij = n78 + 1, n78 + j)
  call pltfil (ip)
  go to 7462
3010 jtemp = knt
  if (nsolve .eq. 1 .and. knt .eq. 1) n899 = int (alogz (fmaxfs / fminsv) / alogz (-delffs) + 2.5d0)
  if (nsolve .eq. 1) jtemp = knt + n899
  n77 = 4 * numnvo
  ik = (jtemp - 1) * n77
  do i = 1, numnvo
     fshold(ik + 1) = solr(i)
     fshold(ik + 2) = soli(i)
     ik = ik + 2
  end do
  do i = 1 , numnvo
     fshold(ik + 1) = voltk(i)
     fshold(ik + 2) = volt(i)
     ik = ik + 2
  end do
  if (nsolve .eq. 0)  go to 7462
  go to 3070
7461 write (unit = lunit(4)) d4, ( solr(i), soli(i), i = 1, numnvo), (voltk(i), volt(i), i = 1, numnvo)
7462 if (kexact .eq. 88333 .and. iprsup .lt. 1) go to 375
  if (iout .eq. 1) write (unit = lunit(6), fmt = 7463) knt, fminfs, (solr(i), soli(i), i = 1, numnvo)
7463 format (/, ' Output vector for step number', i5, ' .   Frequency =', e15.6, ' Hz.', /, (1x, 8e16.7))
  if (iout .eq. 1) write (unit = lunit(6), fmt = 7464) (voltk(i), volt(i), i = 1, numnvo)
7464 format (1x,  8e16.7)
375 knt = knt + 1
  lastov = nchain
  nchain = 8
  if (kexact.ne. 88333 .or. nsolve .eq. 0) go to 9900
  if (nfsout.eq. 3179) go to 9750
  if (knt .eq. 2) dum123 = fminsv * ((-delffs) ** (n899 - 2))
  if ((fminfs / dum123 + epsiln  -1.0d0) .ge. 0) go to 3080
  go to 9900
3080 write (unit = lunit(6), fmt = 3081)
3081 format (/////)
  write (unit = lunit(6), fmt = 3082)
3082 format( 20x, 90('*'))
  write (unit = lunit(6), fmt = 3083) date1
3083 format (20x, '*', 89x, '*', /, 20x, '*', 89x, '*', /, 20x, '*',18x, 'Line model frequency scan comparison table', 12x, 2a4, 9x, '*')
  if (numrun .eq. 1) write (unit = lunit(6), fmt = 3110)
3110 format (20x, '*', 25x, 'Zero-sequence impedance test', 36x, '*')
  if (numrun .eq. 2) write (unit = lunit(6), fmt = 3112)
3112 format (20x, '*', 23x, 'Positive-sequence impedance test', 34x,'*')
  if (numrun .eq. 3) write (unit = lunit(6), fmt = 3114)
3114 format (20x, '*', 28x, 'Mutual impedance test', 47x, '*')
  write (unit = lunit(6), fmt = 3116)
3116 format (20x, '*', 89x, '*', /, 20x, '*', 89x, '*')
  write (unit = lunit(6), fmt = 3082)
  write (unit = lunit(6), fmt = 3085)
3085 format (///, 31x, 'Line model being tested', 29x, 'exact-pi model', 22x, ' % error ', ///, '  frequency', 2x, 'node', 6x, 'mag', 8x, 'ang', 8x, 'real', 8x, 'imag',10x, 'mag', 8x, 'ang', 8x, 'real', 7x, 'imag',9x,'pctmag  pctang')
  nfsout = 3179
  fminfs = fminsv
  nf = n899 - 1
  nref = 2 * numnvo
  ijk = 0
  do i = 1, nf
     k = (i - 1) * 4 * numnvo
     l = k + n899 * 4 * numnvo
     jpp = n899 * 4 * nref + (i-1) * nref
     if (i .eq. 1) go to  3086
     fminfs = fminfs * (-delffs)
     !     n22 = jmod (i - 1, iout)
     n22 = mod (i - 1, iout)
     if (n22 .ne. 0) cycle    !  thl, 10/30/89
3086 ijk = ijk + 1
     if (ijk .gt. 101) go to 9900
     do j = 1, numnvo
        ndum = jch2(j)
        n6 = 6
        call fltopt (fshold(jpp + 1), n6)
        chhold = ansi32(27 : 32)
        n6 = 6
        call fltopt (fshold(jpp + 2), n6)
        if (j .ne. 1) go to 3089
        write (unit = lunit(6), fmt = 3087) fminfs, bus(ndum), (fshold(k + m), m = 1, 2), (fshold(k + nref + m), m = 1, 2), (fshold(l + m), m = 1, 2), (fshold(l + nref + m), m = 1, 2), chhold, ansi32(27 : 32)
3087    format (//, 1p, 1x, e10.3, 1x, a6, 1x, e10.3, 1x, e10.3, 2x, e10.3, 1x, e10.3, 4x, e10.3, 1x, e10.3, 2x, e10.3, 1x, e10.3, 5x, 0p, a6, 3x, a6)
        go to 3092
3089    write (unit = lunit(6), fmt = 3091) bus(ndum), (fshold(k + m), m = 1, 2), (fshold(k + nref + m), m = 1, 2), (fshold(l + m), m = 1, 2), (fshold(l + nref + m), m = 1,2), chhold, ansi32(27 : 32)
3091    format (1p, 12x, a6, 1x, e10.3, 1x, e10.3, 2x, e10.3, 1x, e10.3, 4x, e10.3, 1x, e10.3, 2x, e10.3, 1x, e10.3, 5x, 0p, a6, 3x, a6)
3092    k = k + 2
        l = l + 2
        if (iprsup .ge. 3) write (unit = lunit(6), fmt = *) ' fshold for % error:', jpp, (fshold(jpp + m), m = 1, 2)
        jpp = jpp + 2
     end do
  end do
  go to 9900
7481 icheck = 0
  if ((kexact .eq. 88333) .and. (nsolve .eq. 1)) go to 3080
  !     read input card using cimage
8121 call cimage
  read (unit = abuff, fmt = 8134) aupper
8134 format (13a6)
  do i = 1, 13
     if (aupper(i) .ne. blank) go to 8123
  end do
  write (unit = lunit(6), fmt = 8124)
8124 format ('+Blank card terminating plot spec. cards.')
  go to 9205
8123 icheck = 1
  write (unit = lunit(6), fmt = 8127)
8127 format ('+Plot card discarded in search for blank card.')
  go to 8121
9205 lastov = nchain
  nchain = 51
  call runtym (d1, d2)
  flstat(3) = flstat(3) + d1
  flstat(4) = flstat(4) + d2
  flstat(9) = d1
  flstat(10) = d2
  go to 9900
9700 lastov = nchain
  nchain = 8
  loopss(1) = 7766
  go to 9900
9750 nfsout = 0     ! reset nfsout for 2nd and 3rd lmfs data cases
  go to 9900
9800 lastov = nchain
  nchain = 51
  lstat(18) = 11
9900 if (iprsup  .ge.  1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module over11."')
  return
end subroutine over11

!
! subroutine smint.
!

subroutine smint
  use blkcom
  use labcom
  use space2
  use tacsar
  use smtacs
  use smach
  use tracom
  use movcop
  use over11mod
  implicit none
  !     This module is used only by Brandwajn (type-59) s.m. model
  character(8) :: text1
  integer(4) :: i, i26, i30, i75, ib, ic, idd, idelta, ii, iiter, ik, il, ilb
  integer(4) :: ilk, ilmass, ilstor, in, ineg, ipb, ipp, isact
  integer(4) :: j, j30, jmset
  integer(4) :: k, k1, k14, karc, kmset, ksm
  integer(4) :: l, lmset
  integer(4) :: n1, n2, n3, n4, n5, n6, n7, n9, n10, n12, n15, n20, n21, n31, n33, nloce
  integer(4) :: nloce1, nlocg, nlocg1, nn10, num1, num2, num4, num6, numask, nwd
  real(8) :: a, ac1, ac2, acde, acdf, acee, acef, adeld, adk, adl, adm, ads
  real(8) :: aimia1, aip, ajj, akm, akn, arp, aye, ayr
  real(8) :: b, b1, b2, b6, bs
  real(8) :: c1, ca, ca1, caang, caang1, cai, camag, camag1, camneg, can
  real(8) :: caneg, car, cazer, cb, cb2, cbi, cbr, cc2, cci, ccr, ci0, cid, cid1
  real(8) :: cif, cif1, cig, cikd, cikq, ciq, ciq1, cmneg, cmzer, cs, cz
  real(8) :: d1, d2, d3, d4, d5, d6, d7, d8, d20, d22, ds
  real(8) :: es
  real(8) :: pt, pt0, ptn
  real(8) :: q1, q22, qt, qt0, qtn
  real(8) :: reia1
  real(8) :: sa1, sb2, sc2, scn, sf2, sf3, sft, ssad, ssaq, ssd, ssg, ssld, sslq
  real(8) :: tot, totdmp, totet, tsc1, tsd1
  real(8) :: vaang, vai, vamag, vamag1, var, vbi, vbr, vci, vcr, vd, vf, vq
  real(8) :: xl
  !
  data  text1 / '  1st ' /
  !
  karc = 0
  lmset = 0
  n10 = 0
  nwd = 24
  n33 = 0
  ilmass = 0
  ilstor = 0
  nn10 = ismdat(25)
  iiter = 100
  akm = epsuba
  if (akm .lt. 2.0d0) akm = 100.0d0
  akn = 2.0d0 / (1.0d0 + delta2 / akm)
  akm = akm / omdt
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6250) numsm, nst, delta2
6250 format (/, " Begin  'smint' .   numsm     nst  delta2", /, 17x, 2i8, e15.6)
  j30 = 1
  do k = 1, numsm
     in = 0
     ia = ismdat(j30 + 2)
     ia = norder(ia)
2    l = kode(ia)
     if (l .le. ia) go to 3
     ia = l
     go to 2
3    car = e(ia)
     cai = f(ia)
     ib = ismdat(j30 + 3)
     ib = norder(ib)
4    l = kode(ib)
     if (l .le. ib) go to 5
     ib = l
     go to 4
5    cbr = e(ib)
     cbi = f(ib)
     ic = ismdat(j30 + 4)
     ic = norder(ic)
6    l = kode(ic)
     if (l .le. ic) go to 7
     ic = l
     go to 6
7    ccr = e(ic)
     cci = f(ic)
     !     compute positive sequence currents    ****************************
     z(karc + 1) = car
     z(karc + 2) = cai
     z(karc + 3) = cbr
     z(karc + 4) = cbi
     z(karc + 5) = ccr
     z(karc + 6) = cci
     a = (cbr + ccr) * onehaf
     b = (cci - cbi) * sqrt32
     c1 = (cci + cbi) * onehaf
     d1 = (cbr - ccr) * sqrt32
     reia1 = (car - a + b) / 3.0d0
     aimia1 = (cai - c1 + d1) / 3.0d0
     adk = 1.0d0
     ads = 0.0d0
     idelta = ismdat(j30 + 1)
     if (idelta .eq. 0) go to 423
     adk = asqrt3
     ads = twopi / 12.0d0
423  camag = (sqrtz (reia1 ** 2 + aimia1 ** 2)) * adk
     caang = atan2z (aimia1, reia1) + ads
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6251) camag, caang
6251 format ('  Positive sequence:', 4x, 'ampl=', 2x, e15.8, 4x, 'angle=', 2x, e15.8)
     !     Compute zero and negative sequence currents *********************
     ca = (car + cbr + ccr) / 3.0d0
     cb = (cai + cbi + cci) / 3.0d0
     cmzer = sqrtz (ca ** 2 + cb ** 2)
     cazer = atan2z (cb, ca)
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6252) cmzer, cazer
6252 format (6x, 'zero sequence:', 4x, 'ampl=', 2x, e15.8, 4x, 'angle=', 2x, e15.8)
     ca = (car - a - b) / 3.0d0
     cb = (cai - c1 - d1) / 3.0d0
     cmneg = (sqrtz (ca ** 2 + cb ** 2)) * adk
     caneg = atan2z (cb, ca) - ads
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6253) cmneg, caneg
6253 format ('  Negative sequence:', 4x, 'ampl=', 2x, e15.8, 4x, 'angle=', 2x, e15.8)
     !     Calculate positive sequence terminal voltage ********************
     ksm = kconst + 3 * (k - numsm) - 2
     vamag = crest (ksm)
     vaang = time1 (ksm)
     var = vamag * cosz (vaang)
     vai = vamag * sinz (vaang)
     vamag = crest(ksm + 1)
     vaang = time1(ksm + 1)
     vbr = vamag * cosz (vaang)
     vbi = vamag * sinz (vaang)
     vamag = crest(ksm + 2)
     vaang = time1(ksm + 2)
     vcr = vamag * cosz (vaang)
     vci = vamag * sinz (vaang)
     if (idelta .eq. 0) go to 427
     a = var
     b = vai
     var = var - vbr
     vai = vai - vbi
     vbr = vbr - vcr
     vbi = vbi - vci
     vcr = vcr - a
     vci = vci - b
427  a = (vbr + vcr) * onehaf
     b = (vci - vbi) * sqrt32
     c1 = (vci + vbi) * onehaf
     d1 = (vbr - vcr) * sqrt32
     a = (var - a + b) / 3.0d0
     b = (vai - c1 + d1) / 3.0d0
     vamag = sqrtz (a ** 2 + b ** 2)
     vaang = atan2z (b, a)
     vamag1 = vamag * thtw
     ilk = ismdat(j30)
     if (ilk .eq. 1) go to 12
     if (iprsup .ge. 2) write (unit = lunit(6), fmt = 6254) k, ilk, vamag, vaang
6254 format (/, " In  'smint' ,  b4 dual current split.       k     ilk", 10x, 'vamag', 10x, 'vaang', /, 38x, 2i8, 2e15.6)
     !     Calculate load angles for multiple generators ********************
     d1 = 0.0d0
     q1 = 0.0d0
     n2 = n10 + 1
     n3 = n10 + ilk
     do n1 = n2, n3
        i75 = 101 * k - 74
        d1 = d1 + elp(i75 + 2)
        q1 = q1 + elp(i75 + 3)
     end do
     if (d1 .ne. 0.0d0 .and. q1 .ne. 0.0d0) go to 351
     kill = 106
     k1 = ismdat(j30 + 2)
     bus1 = bus(k1)
     lstat(14) = k
     lstat(18) = nchain
     lstat(19) =  300
     return
351  d2 = vaang - caang
     pt = camag * cosz (d2) / d1
     qt = camag * sinz (d2) / q1
     !     Calculate negative and zero sequence current split coefficients***
     scn = vaang - caneg
     ptn = cmneg * cosz (scn) / d1
     qtn = cmneg * sinz (scn) / q1
     scn = vaang - cazer
     pt0 = cmzer * cosz (scn) / d1
     qt0 = cmzer * sinz (scn) / q1
11   ik = n10 + 1
     k14 = 4 * ik
     i75 = 101 * ik - 74
     arp = elp(i75 + 2)
     aip = elp(i75 + 3)
     d1 = pt * arp
     q1 = qt * aip
     camag1 = sqrtz (d1 ** 2 + q1 ** 2)
     caang1 = vaang - atan2z (q1, d1)
     if (iprsup .ge. 2) write (unit = lunit(6), fmt = 6255) n1, ilk, camag1, caang1
6255 format (/, ' After current split.      n1     ilk', 10x, 'camag1', 10x, 'caang1', /, 21x, 2i8, 2e16.6)
     !     split the negative and zero sequence currents ********************
     d1 = ptn * arp
     q1 = qtn * aip
     cmneg = sqrtz (d1 ** 2 + q1 ** 2)
     caneg = vaang - atan2z (q1, d1)
     d1 = pt0 * arp
     q1 = qt0 * aip
     cmzer = sqrtz (d1 ** 2 + q1 ** 2)
     cazer = vaang - atan2z (q1, d1)
     go to 13
12   camag1 = camag
     caang1 = caang
13   ccr = camag1 * cosz (caang1)
     cci = camag1 * sinz (caang1)
     write (unit = lunit(6), fmt = 6256) cmneg, caneg, cmzer, cazer
6256 format (2x, 'Tabulation of neg. and zero sequ. currents', /, 2x, 'neg. seq.', 2e20.7, /, 2x, 'zer. seq.', 2e20.7)
     !     Calculate electrical position of the d-axis   ********************
     cai = vamag * sinz (vaang)
     car = vamag * cosz (vaang)
     n1 = n10 + 1
     i26 = 101 * n1 - 100
     i75 = i26 + 26
     i30 = 30 * n1 - 29
     es = elp(i26)
     ds = elp(i26 + 1)
     cs = elp(i26 + 8)
     xl = elp(i26 + 18)
     bs = elp(i26 + 19)
     ssld = elp(i26 + 21)
     ssad = elp(i26 + 22)
     sslq = elp(i26 + 23)
     ssaq = elp(i26 + 24)
     ssd = es - xl
     ssg = cs - xl
     b6 = ssg / ssd
     acdf = 1.0d0
     acde = 1.0d0
     isact = ismdat(i30 + 8)
     d1 = car + bs * ccr - cs * cci
     q1 = cai + bs * cci + cs * ccr
     d1 = atan2z (q1, d1)
     !     Calculate Park's currents and voltages   *************************
     q1 = camag1 * thtw
     d2 = caang1 - d1
     cid = q1 * sinz (d2)
     ciq = q1 * cosz (d2)
     d2 = vaang - d1
     vq = vamag1 * cosz (d2)
     cif = (vq + bs * ciq - es * cid) / ds
     !     Add negative sequence corrections, if needed *   *   *   *   *   *
     adeld = cmneg * cosz (caneg) / camag1
     ineg = 0
     if (absz (adeld) .gt. (10.0d0 * epsiln)) ineg = 2
     camneg = cmneg * thtw
     n3 = 6
     call move0 (x1(1 :), n3)
     can = -caneg - d1
     if (ineg .lt. 1) go to 300
     x1(1) = camneg * sinz (can)
     x1(2) = camneg * cosz (can)
     x1(3) = 1.0d0
     x1(4) = 1.0d0
     x1(5) = akm
     x1(6) = akn
     call uncor (x1(1 :), n1)
300  acde = 1.0d0
     acdf = 1.0d0
     !     calculate the total flux in the air-gap      *   *   *   *   *   *
     sf2 = (cid + x1(1)) / elp(i26 + 20) + cif + x1(3) + x1(4)
     sf3 = (ciq + x1(2)) / elp(i26 + 20) + x1(5) + x1(6)
     sft = sqrtz (sf2 ** 2 + (sf3 * b6) ** 2)
     if (isact .eq. 0) go to 306
     ipp = 0
     idd = 0
     acee = 1.0d0
     acef = 1.0d0
     if (ssld .ge. sft .and. sslq .ge. sft) go to 306
     if (ssld .ge. sft) go to 301
     acde = (1.0d0 - ssad * sft) / (1.0d0 - ssad * ssld)
     if (ineg .gt. 1) call slope (sft, ssld, ssad, acee)
301  if (sslq .ge. sft) go to 302
     acdf = (1.0d0 - ssaq * sft) / (1.0d0 - ssaq * sslq)
     if (ineg .gt. 1) call slope (sft, sslq, ssaq, acef)
     go to 302
304  cif1 = cif
     cid1 = d1
     ipp = ipp + 1
     sf2 = (cid + x1(1)) / elp(i26 + 20) + cif + x1(3) + x1(4)
     sf3 = (ciq + x1(2)) / elp(i26 + 20) + x1(5) + x1(6)
     sft = sqrtz (sf2 ** 2 + (sf3 * b6) ** 2)
     acee = 1.0d0
     acde = 1.0d0
     if (sft .le. ssld) go to 311
     acde = 1.0d0 / (1.0d0 + ssad * (sft - ssld))
     if (ineg .gt. 1) call slope (sft, ssld, ssad, acee)
311  acdf = 1.0d0
     acef = 1.0d0
     if (sft .le. sslq) go to 302
     acdf = 1.0d0 / (1.0d0 + ssaq * (sft - sslq))
     if (ineg .gt. 1) call slope (sft, sslq, ssaq, acef)
302  ds = elp(i26 + 1) * acde
     es = ssd * acde + xl
     cs = ssg * acdf + xl
     b1 = car + bs * ccr - cs * cci
     b2 = cai + bs * cci + cs * ccr
     d1 = atan2z (b2, b1)
     d2 = caang1 - d1
     cid = q1 * sinz (d2)
     ciq = q1 * cosz (d2)
     d2 = vaang - d1
     vq = vamag1 * cosz (d2)
     cif = (vq + bs * ciq - es * cid) / ds
     can = -caneg - d1
     if (ineg .eq. 0) go to 303
     x1(1) = camneg * sinz (can)
     x1(2) = camneg * cosz (can)
     x1(3) = acee
     x1(4) = acef
     x1(5) = akm
     x1(6) = akn
     call uncor (x1(1 :), n1)
303  if (ipp .eq. 0) go to 304
     if (idd .gt. 0) go to 306
     adeld = absz ((d1 - cid1) / cid1) + absz ((cif1 - cif) / cif1)
     if (adeld .le. epsiln) go to 305
     if (ipp .le. iiter) go to 304
     kill = 210
     lstat(19) = 306
     lstat(14) = n1
     return
305  idd = 2
     go to 304
306  vf = -elp(i26 + 6) * cif
     vd = vamag1 * sinz (d2)
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6257) n1, d1, vd, vq, vf, cid, ciq, cif
6257 format (/, 8x, 'terminal', 10x, 'delta', 13x, 'vd', 13x, 'vq', 13x, 'vf', 12x, 'cid', 12x, 'ciq', 12x, 'cif', /, 8x, i8, 7e15.6)
     !     Store the electrical variables in corresponding arrays ***********
     cid1 = cid
     ciq1 = ciq
     cif1 = cif
     !     Store coefficients for the calculation of equivalent saturated****
     !     inductances. The values stored here will be necessry for the first
     !     time step even for an unsaturated machine  ***********************
     elp(i75 + 2) = acde
     elp(i75 + 3) = acdf
     cid = cid + x1(1)
     cu(n33 + 8) = cid
     can = can - 2.0d0 * omdt
     if (ineg .eq. 2) cu(n33 + 8) = cid1 + camneg * sinz (can)
     cu(n33 + 1) = cid
     ciq = ciq + x1(2)
     cu(n33 + 9) = ciq
     if (ineg .eq. 2) cu(n33 + 9) = ciq1 + camneg * cosz (can)
     cu(n33 + 2) = ciq
     ci0 = cmzer * cosz (cazer) * sqrt3
     cu(n33 + 3) = ci0
     cu(n33 + 11) = vf
     cif = cif + x1(3)
     cu(n33 + 4) = cif
     cikd = x1(4)
     cu(n33 + 5) = cikd
     cig = x1(5)
     cu(n33 + 6) = cig
     cikq = x1(6)
     cu(n33 + 7) = cikq
     !     Convert Park's currents to phase quantities **********************
     d1 = d1 + twopi / 4.0d0
     ci0 = ci0 * asqrt3
     ca1 = cosz (d1)
     sa1 = sinz (d1)
     tsc1 = -ca1 * onehaf
     tsd1 = sa1 * sqrt32
     cb2 = tsc1 + tsd1
     cc2 = tsc1 - tsd1
     tsc1 = -sa1 * onehaf
     tsd1 = ca1 * sqrt32
     sb2 = tsc1 - tsd1
     sc2 = tsc1 + tsd1
     cu(n33 + 12) = (ca1 * cid + sa1 * ciq) / thtw + ci0
     cu(n33 + 13) = (cb2 * cid + sb2 * ciq) / thtw + ci0
     cu(n33 + 14) = (cc2 * cid + sc2 * ciq) / thtw + ci0
     !     Initialize the mechanical variables.   ***************************
     cz = elp(i26 + 25)
     d3 = d1 / cz
     d4 = omega / cz
     ac1 = (es * cid + ds * cif + elp(i26 + 3) * acde * cikd) / omega
     ac2 = (elp(i26 + 9) * cig + elp(i26 + 11) * cikq ) * acdf
     ac2 = (ac2 + cs * ciq) / omega
     d22 = (ac1 * ciq - ac2 * cid)  * cz * tenm6
     q22 = -(vf * cz * tenm6 * cif) / d4
     d2 = ((es - cs) * cid1 + ds * cif1) * ciq1
     d2 = d2 * tenm6 / d4
     numask = ismdat(i30 + 11)
     nlocg = ismdat(i30 + 12)
     nloce = ismdat(i30 + 13)
     q1 = -(vf * cif1 * cz * tenm6) / d4
     if (nloce .eq. 0) q1 = 0.0d0
     totet = d2 + q1
     num1 = numask - 1
     num2 = numask + numask
     num4 = num2 + num2
     num6 = num4 + num2
     n2 = ilmass * 12 + num2
     n4 = ilstor + nlocg
     histq(n4) = d3
     totdmp = 0.0d0
     d5 = 0.0d0
     n6 = n2 + 1
     do n3 = 1, numask
        d5 = d5 + shp(n6)
        n7 = n6 + num4
        totdmp = totdmp + shp(n7)
        n6 = n6 + 1
     end do
     tot = (totet + totdmp * d4) / d5
     !     Initialize torques for generator no.n10+1   **********************
     ilb = ilstor + num4 + numask
     do il = 1, numask
        histq(il + ilb) = shp(il + n2) * tot
     end do
     if (iprsup .lt. 2) go to 8818
     write (unit = lunit(6), fmt = 6258) n10, nlocg, nloce, numask
6258 format (/, ' After torque initialization.', 12x, 'n10', 10x, 'nlocg', 10x, 'nloce', 9x, 'numask', /, 29x, 4(10x, i5))
     n6 = ilb + 1
     n7 = n6 + num1
     write (unit = lunit(6), fmt = 6259) (histq(i), i = n6, n7)
6259 format (/, (1x, 5e21.12))
8818 nlocg1 = ilb + nlocg
     if (nloce .eq. 0) go to 1500
     nloce1 = ilb + nloce
     histq(nloce1) = histq(nloce1) - q1
     aye = histq(nloce1)
1500 histq(nlocg1) = histq(nlocg1) - d2
     ayr = histq(nlocg1)
     il = nlocg
     n4 = n4 - nlocg
     n20 = n2 + 3 * numask
     if (numask .eq. 1) go to 25
     if (il .eq. 1) go to 18
     !     Calculate  angles to the left of the rotor ***********************
     n6 = n2 + num4 + 1
     n7 = n6 + nlocg - 1
     d6 = 0.0d0
     do n3 = n6, n7
        d6 = d6 - shp(n3)
     end do
     d6 = d6 * d4
     n6 = ilb
     do ik = 1, il
        n6 = n6 + 1
        d6 = d6 + histq(n6)
     end do
21   ik = il
     il = il - 1
     if (il .lt. 1) go to 18
     n3 = n2 + num4 + ik
     n6 = n4 + ik
     n21 = n20 + ik
     d6 = d6 + shp(n3) * d4 - histq(ilb + ik)
     histq(n6 - 1) = histq(n6) + d6 / shp(n21 - 1)
     go to 21
18   il = nlocg
     if (il .eq. numask) go to 24
     !     calculate  angles to the right of the rotor  *********************
     d6 = 0.0d0
     if (il .eq. 1) go to 26
     n6 = n2 + num4 + 1
     n7 = n6 + nlocg - 2
     do n3 = n6, n7
        d6 = d6 - shp(n3)
     end do
     d6 = d6 * d4
     n7 = nlocg - 1
     n6 = ilb
     do ik = 1, n7
        n6 = n6 + 1
        d6 = d6 + histq( n6 )
     end do
26   ik = il
     il = il + 1
     if (il .gt. numask) go to 24
     n3 = n2 + num4 + ik
     d6 = d6 + histq(ilb + ik) - shp(n3) * d4
     n6 = n4 + ik
     n3 = n20 + ik
     histq(n6 + 1) = histq(n6) - d6 / shp(n3)
     go to 26
24   if (nloce .gt. 0) histq(nloce1) = aye + q1
25   histq(nlocg1) = ayr + d2
     d20 = d2
     !     Store initial (synchronous) speeds ******************************
     n5 = n4 + numask + 1
     n3 = n5 + num1
     do ik = n5, n3
        histq(ik) = d4
     end do
     if (numask .eq. 1) go to 30
     !     calculate torques on the shaft  **********************************
     n6 = ilb - numask
     do ik = 1, num1
        n3 = n20 + ik
        n5 = n4 + ik
        n6 = n6 + 1
        histq(n6) = (histq(n5) - histq(n5 + 1)) * shp(n3)
     end do
     if (iprsup .lt. 2) go to 30
     write (unit = lunit(6), fmt = 6258) n10, nlocg, nloce, numask
     n7 = ilb - 1
     n6 = n7 - num1 + 1
     write (unit = lunit(6), fmt = 6259) in, (histq(i), i = n6, n7)
30   n10 = n10 + 1
     in = in + 1
     !     Load initial conditions into tacs arrays ************************
     kmset = ismdat(i30 + 14)
     if (kmset .eq. 0) go to 970
     ipb = ilstor + numask
     n31 = num2 + num1
     do i = 1, kmset
        lmset = lmset + 1
        jmset = ismtac(lmset)
        if (jmset .gt. 0) go to 1696
        jmset = -jmset
        if (jmset .gt. 7) go to 1680
        etac(lmset) = cu(n33 + jmset)
        cycle
1680    if (jmset .gt. 8) go to 1681
        etac(lmset) = vd
        cycle
1681    if (jmset .gt. 9) go to 1682
        etac(lmset) = vq
        cycle
1682    if (jmset .gt. 10) go to 1683
        etac(lmset) = (var + vbr + vcr) * asqrt3
        cycle
1683    if (jmset .gt. 11) go to 1684
        etac(lmset) = vf
        cycle
1684    if (jmset .gt. 12) go to 1685
        etac(lmset) = sft
        cycle
1685    if (jmset .gt. 13) go to 1686
        etac(lmset) = atan2z (sf3, sf2)
        cycle
1686    if (jmset .gt. 14) go to 1687
        etac(lmset) = d22
        cycle
1687    if (jmset .gt. 15) go to 1688
        etac(lmset) = q22
        cycle
1688    if (jmset .eq. 17) go to 1689
        etac(lmset) = ac1
        cycle
1689    etac(lmset) = ac2
        cycle
1696    if (jmset .gt. num2) go to 1697
        n9 = ilstor + jmset
        etac(lmset) = histq(n9)
        cycle
1697    jmset = ilstor + jmset + num2
        etac(lmset) = histq(jmset)
     end do
     !     the following routine prints info requested by user which exists
     !     immediately after machine initialization
970  bus4 = text1
     if (elp(i75) .eq. 0.0d0) go to 1201
     !     Print inductance and restance values of mach. n10 in physical unit
     k1 = ismdat(j30 + 2)
     write (unit = lunit(6), fmt = 6260) n10, bus(k1), in
6260 format (//, ' mach ', i2, 10x, 'Data parameters and initial conditions of next machine follow.     ', 48('-'), /, " '",  a6, "'   unit", i3)
     write (unit = lunit(6), fmt = 91993) elp(i26 + 2)
91993 format (/, ' Machine reactances and resistances, in Ohms  (quantities labeled as inductances are actually reactances).', /, 3x, e15.7, 5x, 'Lf   = d-axis field self inductance')
     write (unit = lunit(6), fmt = 91994) elp(i26 + 1)
91994 format (3x, e15.7, 5x, 'Laf  = d-axis field-armature mutual inductance')
     write (unit = lunit(6), fmt = 91995) elp(i26 + 4)
91995 format (3x, e15.7, 5x, 'Lfkd = d-axis field-damper mutual inductance')
     write (unit = lunit(6), fmt = 91996) elp(i26)
91996 format (3x, e15.7, 5x, 'Ld   = d-axis armature self inductance (synchronous reactance)')
     write (unit = lunit(6), fmt = 91997) elp(i26 + 3)
91997 format (3x, e15.7, 5x, 'Lakd = d-axis armature-damper mutual inductance')
     write (unit = lunit(6), fmt = 91998) elp(i26 + 5)
91998 format (3x, e15.7, 5x, 'Lkd  = d-axis damper self inductance')
     write (unit = lunit(6), fmt = 91999) elp(i26 + 10)
91999 format (3x, e15.7, 5x, 'Lg   = q-axis circuit 1 self inductance')
     write (unit = lunit(6), fmt = 92991) elp(i26 + 9)
92991 format (3x, e15.7, 5x, 'Lag  = q-axis circuit 1-armature mutual inductance')
     write (unit = lunit(6), fmt = 92992) elp(i26 + 12)
92992 format (3x, e15.7, 5x, 'Lgkq = q-axis circuit 1-circuit 2 mutual inductance')
     write (unit = lunit(6), fmt = 92993) elp(i26 + 8)
92993 format (3x, e15.7, 5x, 'Lq   = q-axis armature self inductance')
     write (unit = lunit(6), fmt = 92994) elp(i26 + 11)
92994 format (3x, e15.7, 5x, 'Lakq = q-axis circuit 2-armature mutual inductance')
     write (unit = lunit(6), fmt = 92995) elp(i26 + 13)
92995 format (3x, e15.7, 5x, 'Lkq  = q-axis circuit 2-self inductance')
     write (unit = lunit(6), fmt = 92996) elp(i26 + 16)
92996 format (3x, e15.7, 5x, 'L0   = zero sequence reactance')
     write (unit = lunit(6), fmt = 92997) elp(i26 + 17)
92997 format (3x, e15.7, 5x, 'R0   = zero sequence resistance')
     write (unit = lunit(6), fmt = 92999) elp(i26 + 6)
92999 format (3x, e15.7, 5x, 'Rf   = resistance of field winding')
     write (unit = lunit(6), fmt = 93991) elp(i26 + 19)
93991 format (3x, e15.7, 5x, 'Ra   = armature resistance')
     write (unit = lunit(6), fmt = 93992) elp(i26 + 7)
93992 format (3x, e15.7, 5x, 'Rkd  = d-axis damper resistance')
     write (unit = lunit(6), fmt = 93993) elp(i26 + 14)
93993 format (3x, e15.7, 5x, 'Rg   = q-axis circuit-1 resistance')
     write (unit = lunit(6), fmt = 93994) elp(i26 + 15)
93994 format (3x, e15.7, 5x, 'Rkq  = q-axis circuit-2 resistance')
     !     print mechanical data of mach. n10
     write (unit = lunit(6), fmt = 6261)
6261 format (/,' Mechanical parameters of generator, in physical units as shown by column headings.')
     write (unit = lunit(6), fmt = 6262)
6262 format (3x, 'Moment of inertia', 12x, 'Self-damping coefficients of mass', 9x, 'Mutual-damping coeff.', 5x, 'Torsional spring constant', /, 7x, 'of rotor mass', 10x, 'Speed-deviation', 6x, 'Absolute-speed', 9x, '(with following mass)', 9x, '(with following mass)')
     write (unit = lunit(6), fmt = 6263)
6263 format (' Million (n - m) / (rad / sec ** 2)', 17x, 'Million (n - m) / (rad / sec)', 7x, 'Million (n - m) / (rad / sec)', 11x, 'Million (n - m) / (rad)')
     n15 = n2 + numask
     do ii = 1, numask
        n15 = n15 + 1
        ajj = shp(n15)
        adl = shp(n15 + num4)
        adm = shp(n15 + numask)
        adk = shp(n15 + num2)
        ads = shp(n15 + 3 * numask)
        write (unit = lunit(6), fmt = 6264) ajj, adl, ads, adm, adk
     end do
6264 format (1x, e21.7, 5x, 2e20.7, 2e30.7)
1201 if (elp(i75 + 1) .eq. 0.0d0) go to 1218
     write (unit = lunit(6), fmt = 6265)
6265 format (/, ' Total current injected into network at generator bus, in phase coordinates.   For a dual-machine bus,', /, " this is the total injection ( 'a' + 'b' ).   The first line displays the currents as found by the phasor network", /, ' solution (which may be unbalanced, if the network is).   The 2nd line shows only the positive-sequence component,', /, '  magnitudes of the currents are in units of (Amps) .')
     adk = 1.0d0
     ads = 0.0d0
     if (idelta .eq. 0) go to 429
     adk = sqrt3
     ads = twopi / 12.0d0
429  d7 = z(karc + 1)
     d8 = z(karc + 2)
     d1 = sqrtz (d7 ** 2 + d8 ** 2)
     d2 = atan2z (d8, d7) * radeg
     d7 = z(karc + 3)
     d8 = z(karc + 4)
     d3 = sqrtz (d7 ** 2 + d8 ** 2)
     d4 = atan2z (d8, d7) * radeg
     d7 = z(karc + 5)
     d8 = z(karc + 6)
     d5 = sqrtz (d7 ** 2 + d8 ** 2)
     d6 = atan2z (d8, d7) * radeg
     write (unit = lunit(6), fmt = 6290) d1, d2, d3, d4, d5, d6
6290 format (18x, " Phase  'a'  injection", 18x, " Phase  'b'  injection", 18x, " Phase  'c'  injection", /, 3(17x, 'Magnitude', 7x, 'Degrees'), /, 3(11x, e15.7, 2x, f12.7))
     write (unit = lunit(6), fmt = 6266)
6266 format ('+Actual')
     d4 = camag * adk
     d1 = (caang - ads) * radeg
     d2 = d1 + 240.0d0
     d5 = d2 - 360.0d0
     if (d5 .gt. -180.0d0) d2 = d5
     d3 = d1 + 120.0d0
     d5 = d3 - 360.0d0
     if (d5 .gt. -180.0d0) d3 = d5
     write (unit = lunit(6), fmt = 6267) d4, d1, d4, d2, d4, d3
6267 format (' pos. seq. ', e15.7, 2x, f12.7, 2(11x, e15.7, 2x, f12.7))
     !     Print d,q, and 0 currents
     write (unit = lunit(6), fmt = 6268)
6268 format (/, ' Armature currents of generator in rotating reference frame (d-q-0 coordinates), in units of  (amps) .')
     write (unit = lunit(6), fmt = 6269) cu(n33 + 1), cu(n33 + 2), cu(n33 + 3)
6269 format (20x, 'id', 13x, 'iq', 13x, 'i0', /, 7x, 3(e15.7))
     !     Print a,b,c phase currents (pos. seq. component)
     write (unit = lunit(6), fmt = 6270)
6270 format (/, ' Positive-sequence component of generator armature current in phase coordinates, in units of (Amps) .')
     d4 = camag1
     d1 = caang1 * radeg
     d2 = d1 + 240.0d0
     d5 = d2 - 360.0d0
     if (d5 .gt. -180.0d0) d2 = d5
     d3 = d1 + 120.0d0
     d5 = d3 - 360.0d0
     if (d5 .gt. -180.0d0) d3 = d5
     write (unit = lunit(6), fmt = 6271) d4, d1, d4, d2, d4, d3
6271 format (18x, "Armature of phase  'a'", 18x, "Armature of phase  'b'", 18x, "Armature of phase  'c'", /, 3(17x, 'Magnitude', 7x, 'Degrees'), /, 3(11x, e15.7, 2x, f12.7))
     !     Print field current
     write (unit = lunit(6), fmt = 6272)
6272 format (/, ' Field current of generator in units of  (Amps) .')
     write (unit = lunit(6), fmt = 6273) cu(n33 + 4), cif1
6273 format (9x, 'Total', 15x, 'DC-component', /, 2(1x,e19.7))
     !     Print torque on generator rotor
     write (unit = lunit(6), fmt = 6274)
6274 format (/, ' Electromechanical torque of generator, in units of  million (n - m).')
     write (unit = lunit(6), fmt = 6273) d22, d20
     if ( nloce .eq. 0 ) go to 1960
     write (unit = lunit(6), fmt = 6275)
6275 format (/, ' Electromechanical torque of exciter, in units of  million (n - m).')
     write (unit = lunit(6), fmt = 6273) q22, q1
1960 write (unit = lunit(6), fmt = 6276) elp(i26 + 21), elp(i26 + 23)
6276 format (/, ' Critical level of total air gap mmf at which saturati on begins, in units of  (Amperes) .', /, 1x, 2e25.9)
     write (unit = lunit(6), fmt = 6277)
6277 format (/, ' Mechanical angles of rotor masses, in units of  (Degrees)  .')
     do ii = 1, numask
        n12 = ilstor + ii
        d4 = histq(n12) * radeg
        write (unit = lunit(6), fmt = 6278) d4,  ii
6278    format (15x, f12.7, 4x, "'theta'  for mass no.", i3)
     end do
     !     print angular velocities
     write (unit = lunit(6), fmt = 6279)
6279 format (/, ' Angular velocities of rotor masses, in units of (rad/sec) .')
     n12 = ilstor + numask
     d4 = histq(n12 + 1)
     do i = 1, numask
        write (unit = lunit(6), fmt = 6280) d4, i
6280    format (15x , f12.7, 4x, "'omega'  for mass no.", i3)
     end do
     if ( num1 .eq. 0 )  go  to  1218
     !     print shaft torques
     write (unit = lunit(6), fmt = 6281)
6281 format (/, ' Shaft torques between masses, in units of  million (n - m).')
     !     note- transient shaft torque includes a damping torque term also
     !           but above is correct for steady state
     n6 = ilstor + num4
     do i = 1, num1
        n6 = n6 + 1
        ajj = histq( n6 )
        n1 = i + 1
        write (unit = lunit(6), fmt = 6282)  ajj, i, i, n1
6282    format (16x, e15.7, 4x, "'t", i2, "' --- Torque on shaft between mass", i3, '  and mass', i3)
     end do
1218 ilmass = ilmass + numask
     ilstor = ilstor + num6
     n33 = n33 + nwd
     if ( iprsup  .ge.  2 ) write ( lunit(6), 6283 )  k, n10, numsm, ismdat( j30 )
6283 format (/, " After shaft-torque calc. in  'smint ' .       k     n10   numsm  imdual", /, 40x, 5i8)
     if (in .lt. ilk) go to 11
     j30 = j30 + 30
  end do
  !     erase synchronous machines from source tables  * * * * * * * * * *
  do i = 1, kconst
     j = i
     if (tstart(i) .eq. -9988.0d0) go to 915
  end do
  go to 918
915 kconst = j - 1
918 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6284) kconst
6284 format (/, " Upon exit  'sminit' ,  kconst =", i3)
  return
end subroutine smint

!
! subroutine slope.
!

subroutine slope (sft, ssld, ssad, acee)
  implicit none
  !
  real(8), intent(in) :: sft
  real(8), intent(in) :: ssld
  real(8), intent(in) :: ssad
  real(8), intent(out) :: acee
  integer(4) :: n5
  real(8) :: adeld
  real(8) :: sb, sf4, sf5, sf6, sf7
  !
  adeld = sft / ssld - 0.9d0
  n5 = int (10.0d0 * adeld)
  n5 = (n5 + 1) / 2
  sb = n5
  sb = sb  * 0.1d0
  sf4 = ssld * (0.9d0 + sb)
  sf5 = ssld * (1.1d0 + sb)
  sf6 = sf4 / (1.0d0 + ssad * (sf4 - ssld))
  sf7 = sf5 / (1.0d0 + ssad * (sf5 - ssld))
  acee = (sf7 - sf6) / (sf5 - sf4)
  return
end subroutine slope

!
! subroutine ssout.
!

subroutine ssout (l, erk, eik, erm, eim, currk, curik, currm, curim)
  use blkcom
  use labcom
  use tracom
  implicit none
  !
  integer(4), intent(in) :: l
  real(8), intent(in) :: curik, curim, currk, currm
  real(8), intent(in) :: eik, eim, erk, erm
  integer(4) :: k
  integer(4) :: m
  real(8) :: cak, cam, cmk, cmm
  real(8) :: picon, pk, ploss, pm
  real(8) :: qk, qloss, qm
  real(8) :: thk, thm
  real(8) :: vk, vm
  !
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1006) l, erk, eik, erm, eim
1006 format (' Top of "ssout".  l, erk, eik, erm, eim =', i6, 4e16.5)
  if (m4plot .eq. 1) call emtspy
  picon = 360.0d0 / twopi
  if (l .gt. 0) go to 2020
  bus1 = blank
  go to 2030
2020 k = iabs (kbus(l))
  m = iabs (mbus(l))
  if (mbus(l) .gt. 0 .and. kssout .eq. 3) go to 4500
  bus1 = bus(k)
  if (k .eq. 1) bus1 = terra
  bus2 = bus(m)
  if (m .eq. 1) bus2 = terra
  if (bus1 .eq. trash) go to 4500
  if (bus2 .eq. trash) go to 4500
2030 vk = sqrtz (erk ** 2 + eik ** 2)
  vm = sqrtz (erm ** 2 + eim ** 2)
  thk = 0.0d0
  thm = 0.0d0
  if (vk .gt. 0.0d0) thk = picon * atan2z (eik, erk)
  if (vm .gt. 0.0d0) thm = picon * atan2z (eim, erm)
  cmk = sqrtz (currk ** 2 + curik ** 2)
  cmm = sqrtz (currm ** 2 + curim ** 2)
  cak = 0.0d0
  cam = 0.0d0
  if (cmk .gt. 0.0d0) cak = picon * atan2z (curik, currk)
  if (cmm .gt. 0.0d0) cam = picon * atan2z (curim, currm)
  pk = (erk * currk + eik * curik ) / 2.0d0
  pm = (erm * currm + eim * curim ) / 2.0d0
  qk = (-erk * curik + eik * currk ) / 2.0d0
  qm = (-erm * curim + eim * currm ) / 2.0d0
  ploss = pk + pm
  qloss = qk + qm
  if (kssout .eq. 1) go to 2032
  if (vk .gt. 0.0d0) go to 2032
  if (vm .gt. 0.0d0) go to 2032
  if (cmk .gt. epsiln) go to 2032
  if (cmm .gt. epsiln) go to 2032
  go to 4500
2032 if (l .gt. 0) go to 2040
  !     Begin code for generator injection (80 or 132 columns):
  ploss = sqrtz (pk ** 2 + qk ** 2)
  qloss = 0.0d0
  if (ploss .gt. 0.0d0) qloss = pk / ploss
  if (kol132 .eq. 132) go to 3214
  !     Begin 80-column code for generator injection printout:
  write (unit = lunit(6), fmt = 3207) bus5, vk, cmk, pk, ploss
3207 format (1x, a6, 13x, 4e15.7)
  write (unit = lunit(6), fmt = 3208) thk, cak, qk, qloss
3208 format (20x, 2f15.4, e15.7, f15.10)
  go to 4500
  !     Begin 132-column code for generator injection printout:
3214 write (unit = lunit(6), fmt = 2034) bus5, erk, vk, currk, cmk, pk, ploss
2034 format (15x, a6, 3(5x, 2e15.7))
  go to 2051
  !     Begin code for branch flows (80 or 132 columns):
2040 if (kol132 .eq. 132) go to 2045
  !     Begin special code dedicated to 80-col. branch flow:
  write (unit = lunit(6), fmt = 3225) bus1, vk, vm, cmk, pk
3225 format (/, 1x, a6, 13x, 4e15.7)
  write (unit = lunit(6), fmt = 3229) bus2, thk, thm, cak, qk
3229 format (14x, a6, 3f15.4, e15.7)
  go to 4500
  !     Begin 132-col. code for forward branch flow printout:
2045 write (unit = lunit(6), fmt = 2050) bus1, erk, vk, currk, cmk, pk, ploss
2050 format (/, 1x, a6, 14x, 3(5x, 2e15.7))
2051 write (unit = lunit(6), fmt = 2052) eik, thk, curik, cak, qk, qloss
2052 format (21x, 2(5x, e15.7, f15.4), 5x, 2e15.7)
  if (l .lt. 0) go to 4500
  !     Begin code for reverse branch flow, 132-column display:
  write (unit = lunit(6), fmt = 2054) bus2, erm, vm, currm, cmm, pm
2054 format (/, 15x, a6, 3(5x, 2e15.7))
  write (unit = lunit(6), fmt = 2052) eim, thm, curim, cam, qm
  write (unit = lunit(6), fmt = 2052)
4500 return
end subroutine ssout

!
! end of file over11.f90
!
