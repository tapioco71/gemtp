!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over12.f90
!

module over12mod
  implicit none
  save

contains

  !
  ! subroutine reduce.
  !

  subroutine reduce (x1, m, n)
    use movcop
    implicit none
    real(8), intent(out) :: x1(:)
    integer(4), intent(in) :: m
    integer(4), intent(in) :: n
    integer(4) :: i, ik
    integer(4) :: j
    integer(4) :: k
    integer(4) :: mk
    integer(4) :: n9, nk
    real(8) :: a1(20)
    real(8) :: b
    real(8) :: c
    !     Gauss-Jordan elimination process performed on a square matrix x
    !     this routine can also be used for matrix inversion * * * * * * * *
    !     maximum order of matrix x is 20
    j = m
    ik = m ** 2
    nk = ik - m
1   c = x1(ik)
    c = 1.0d0 / c
    call move (x1(nk + 1 :), a1(1 :), m)
    k = 1
4   mk = (k - 1) * m
    n9 = mk + j
    b = -x1(n9) * c
    i = 1
3   mk = mk + 1
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
  end subroutine reduce

  !
  ! subroutine reduct.
  !

  subroutine reduct(a, n, m)
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
    h1 = -1.0d0 / a(ij)
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
    if (w .lt. 0.0d0) go to 3
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
  end subroutine reduct

end module over12mod

!
! subroutine over12.
!

subroutine over12
  use blkcom
  use labcom
  use tacsar
  use smtacs
  use space2
  use dekspy
  use tracom
  use indcom
  use random
  use movcop
  use bcdtim
  use comthl
  use over12mod
  use fdqlcl
  use systematic
  implicit none
  !     dc-48 had "lastsw" clobbered on 2nd energization; change
  !     dummy usage of this vector to "lastxx" temporarily (12 sep
  character(4) :: atim(2)
  integer(4) :: i, ii, ikf, ilorow, iofcnt, ioftab, ip, iprint, iprout, iq
  integer(4) :: irbase, isecti, isfd, itadd
  integer(4) :: j, j1, jgl, jglnn, jlk, jt
  integer(4) :: k, k1, k2, k3, k4, ka, kb, kf, klorow, ksw
  integer(4) :: l, l1, lastxx(50), lb, ll0, ll4, ll6
  integer(4) :: m, mxpair
  integer(4) :: n, n1, n2, n3, n4, n5, n6, n7, n8, n9, n15, n16, n17, n24
  integer(4) :: ncompt, ndx1, ndx2, ndx3, ndx4, ndx5, ndxi, nhalf, ni, nj, nk1
  integer(4) :: nn1, nn8, nn9, nn10, nn11, nn12, nn13, nn14, nn15, npair, nph, nra, nrz
  integer(4) :: ns1, ns2, nwww
  real(8) :: a, az, azi, azr
  real(8) :: bias, bias2
  real(8) :: cz
  real(8) :: d1, d2, d22, d7, d8, d9, d14, d18
  real(8) :: dblpr1, dblpr2, dblpr3, dblpr4, dum9, eh
  real(8) :: fac1, fac2, fac3, fac4, fac5, fsigma
  real(8) :: gus1, gus2, h2, h3, hi, one
  real(8) :: ranoff, rll, rng
  real(8) :: secfrq, seed, seedr, sk1i, sk1r, sll, sumdt
  real(8) :: tdiff, timev, tma, tmb, tmc, tmean, tmt, total, tstati
  real(8) :: tstatj, tstbeg
  real(8) :: window
  real(8) :: xd, xll, xn, xq
  real(8) :: yll, yx
  real(8) :: zerofl
  !  dimension cmi(1), cmr(1)
  !  dimension akey(1), tstat(1)
  !  dimension wk1(1)
  !
  !  equivalence (semaux(1), wk1(1))
  !  equivalence (akey(1), adelay(1))
  !  equivalence (tstat(1), crit(1))
  !  equivalence (kks(1), cmr(1))
  !  equivalence (kknonl(1), cmi(1))
  !  equivalence (moncar(1), knt)
  !  equivalence (moncar(2), kbase)
  !  equivalence (moncar(3), ltdelt)
  !  equivalence (moncar(4), isw)
  !  equivalence (moncar(5), idist)
  !  equivalence (moncar(6), itest)
  !  equivalence (moncar(8), jseedr)
  !  equivalence (moncar(9), kloaep)
  !  equivalence (moncar(10), mtape)
  !
  integer(4), pointer :: idist => moncar(5)
  integer(4), pointer :: isw => moncar(4)
  integer(4), pointer :: itest => moncar(6)
  integer(4), pointer :: jseedr => moncar(8)
  integer(4), pointer :: kbase => moncar(2)
  integer(4), pointer :: kloaep => moncar(9)
  integer(4), pointer :: knt => moncar(1)
  integer(4), pointer :: ltdelt => moncar(3)
  integer(4), pointer :: mtape => moncar(10)
  real(8), pointer :: akey(:) => adelay(1 :)
  real(8), allocatable :: cmi(:)
  real(8), allocatable :: cmr(:)
  real(8), pointer :: tstat(:) => crit(1 :)
  real(8), pointer :: wk1(:) => semaux(1 :)
  !
  ll0 = size (transfer (kknonl, cmi))
  allocate (cmi(ll0))
  cmi = transfer (kknonl, cmi)
  ll0 = size (transfer (kks, cmr))
  allocate (cmr(ll0))
  cmr = transfer (kks, cmr)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module over12."')
  nph = 0
  nqtt = 0
  nqtw = 0
  jglnn = 0
  isecti  = 400    ! this variable should be in common,thl.3/21/90
  !  seedr = 7 * twopi
  seedr = int (7 * twopi)
  inecho = 0
  ll0 = 0
  ll4 = 4
  ll6 = 6
  one = 1.0d0
  !     initialize counters for the -666 branches*   *   *   *   *   *   *
  ikf = 0
  isfd = 0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2716) kswtch, kbase, isw, nenerg, (kdepsw(i), i = 1, kswtch)
2716 format (/, ' At beginning  ', "'", 'over12', "' .", '  kswtch   kbase     isw  nenerg', /, 25x, 4i8, /, ' (kdepsw(i), i=1, kswtch)  follow ...', /, (1x, 20i6))
  !     Define lstat(15) as a flag for omit base case in a stat. run
  if (kbase .eq. intinf) lstat(15) = intinf
  if (lastov .eq. 1) go to 13000
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4624) knt, kbase, kloaep, mtape, nenerg, jseedr, ltdelt, kswtch
  if (kbase .eq. 2) go to 164
  !163 call runtym(d1, d2)
  call runtym(d1, d2)
  flstat(3) = flstat(3) + d1
  flstat(4) = flstat(4) + d2
  flstat(5) = flstat(5) - d1
  flstat(6) = flstat(6) - d2
164 if (isw .eq. 4444) go to 4088
  if (kbase .ne. 2) go to 76403
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2721) n4, nword1, nword2, ltlabl
  call tables
  rewind lunit(10)
  read (unit = lunit(10)) n8
  if (n8 .le. 0) go to 7246
  do i = 1, n8
     read (unit = lunit(10)) j, akey(j), tstat(j), topen(j + lswtch)
     if (iprsup .ge. 2) write (unit = lunit(6), fmt = 7242) i, j, akey(j), tstat(j), topen(j + lswtch)
7242 format (' stat. open.  i, j =', 2i5, 5x, 3e15.6)
  end do
  !7245 continue
7246 inecho = 0
  rewind lunit(12)
  if (knt .eq. 1) mtape = lunit(5)
  lunit(5) = lunit(12)
  rewind lunit(5)
  if (knt .gt. 1)   go to  409
166 iplot = -1
  rewind lunit(3)
  do i = 1, kswtch
     j = lswtch + i
     write (unit = lunit(3)) kmswit(i), kmswit(j), akey(i), tstat(i), topen(j), kdepsw(i)
  end do
  write (unit = lunit(3)) kloaep
  go to 409
76403 if (loopss(1) .ne. 7766) go to 13000
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 7474) numum, it1, ibr
7474 format (' In "over12".  b4 sneaky restoration.    numum, it1, ibr =', 3i8)
  if (numum .gt. 0) it1 = ibr
  if (numum .eq. 0) ibr = it1
  lstat(22) = ibr
13000 n1 = nchain
  if (kbase .eq. 0) go to 13010
  rewind lunit(9)
  write (unit = lunit(9)) ntot, nenerg
  if (jflsos .gt. 0) write (unit = lunit(9)) pu
  inecho = lunit(12)
  rewind lunit(12)
13010 if ((kbase .ge. 2) .and. (kbase .ne. intinf)) go to 156
  if (ktab .eq. 0) go to 156
  indstp = 1
  write (unit = *, fmt = *) ' over12 ready for TACS.  newtac =', newtac
  if (newtac .ne. 1) call tacs2
  if (newtac .eq. 1) call ntacs2
  write (*, *) ' Done with TACS, back in over12.  ioutin =', ioutin
  iout = ioutin
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2719) iout, indstp, multpr, kprchg
2719 format(/, ' in ', "'", 'over12', "'", ' after ', "'", 'tacs2', "'",  '    iout  indstp  multpr  kprchg', /, 31x, 4i8)
  if (kill .gt. 0) go to 9200
  if (nchain .ne. n1) go to 9800
156 iftail = 0
  if (kbase .eq. 0) go to 598
  if (kbase .ne. intinf) call tables
  if (ktab .eq. 0) iprout = 0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2721) n4, nword1, nword2, ltlabl, ktab, isw, lunit(2), iv, lbrnch
2721 format (/, ' Unit 2 read/write indices.      n4  nword1  nword2  ltlabl    ktab     isw  lunit2      iv  lbrnch', /, 27x, 9i8)
  rewind lunit(10)
  n8 = 0
  do i = 1, kswtch
     if (akey(i) .ne. -44444.0d0) cycle
     n8 = n8 + 1
  end do
  write (unit = lunit(10)) n8
  do i = 1, kswtch
     if (akey(i) .ne. -44444.0d0) cycle
     j = i + lswtch
     write (unit = lunit(10)) i, akey(i), tstat(i), topen(j)
  end do
  if (kbase .ne. intinf) go to 7263
  knt = 1
  go to 166
7263 nenerg = 0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4624) knt, kbase, kloaep, mtape
  if (iprsup .ge. 4) write (unit = lunit(6), fmt = 76) kswtch, kloaep, (kdepsw(ip), ip = 1, ll6)
76 format (/, ' At 76', 2i10, /, 1x, 6i5)
  do j = 1, kswtch
     if (absz (akey(j)) .ne. 44444.0d0) cycle
     n = kdepsw(j)
     if (n  .eq.  0) go to 42300
     tma = tclose(n)
     if (akey(n) .eq. -44444.0d0) tma = topen(n)
     tmb = tclose(j)
     if (akey(j) .eq. -44444.0d0) tmb = topen(j)
     tmc = tma + tmb
     if (akey(j) .eq.  44444.0d0) tclose(j) = tmc
     if (akey(j) .eq. -44444.0d0) topen(j) = tmc
     go to 42500
42300 if (kloaep .le. 0) go to 42500
     tmt = tclose(kloaep)
     if (akey(kloaep) .eq. -44444.0d0) tmt = topen(kloaep)
     if (akey(j) .eq. 44444.0d0) tclose(j) = tmt
     if (akey (j) .eq. -44444.0d0) topen(j) = tmt
42500 crit(j) = 0.0
     if (akey(j) .eq. -44444.0d0) cycle
     topen(j) = fltinf
     akey(j) = 0.0d0
  end do
  if (iprsup .lt. 1)  go to  598
  write (unit = lunit(6), fmt = 52630)  (tclose(j), j = 1, kswtch)
52630 format (' at 52630, tclose after sorting', /, (1x, 8e15.6))
  write (unit = lunit(6), fmt = 5263) (topen(j), j = 1, kswtch)
5263 format (' at 5263, topen after sorting', /, (1x, 8e15.6))
  go to 598
4088 ii = 0
  write (unit = lunit(3)) (bus(i), i = 1, ntot)
  do i = 1, kswtch
     j = lswtch + i
     write (unit = lunit(3)) kmswit(i), kmswit(j), akey(i), tstat(i), topen(j), kdepsw(i)
  end do
  write (unit = lunit(3)) kloaep
  !  ncompt = 2 * (sigmax / aincr + flzero)
  ncompt = int (2 * (sigmax / aincr + flzero), kind (ncompt))
  !  nhalf = ncompt / 2
  nhalf = int (ncompt / 2, kind (nhalf))
  n3 = 0
  do i = 1, kswtch
     if (absz (akey(i)) .ne. 44444.0d0) cycle
     n3 = n3 + 1
  end do
  n4 = nbyte(3) / nbyte(4)
  iofcnt = n4 * ncompt
  ioftab = n4 * 3 * ncompt
  mxpair = n3 * (n3 - 1) / 2
  !  n2 = (3 + mxpair) * ncompt
  n2 = int ((3 + mxpair) * ncompt, kind (n2))
  if (kburro .eq. 1) go to 4093
  n1 = location (kswtyp(1)) - location (irandn(1))
  call move0 (irandn(1 :), n1)
  go to 4097
4093 call move0 (irandn(1 :), lsiz23)
  n1 = lsiz23
4097 if (iprsup .ge. 1) go to 4095
  go to 4099
4095 write (unit = lunit(6), fmt = 2345)
2345 format (/, ' At 2345 of over12')
  do i = 1 , kswtch
     j = lswtch + i
     !4096 write (unit = lunit(6), fmt = 61234) kmswit(i), kmswit(j), akey(i), tstat(i), topen(j)
     write (unit = lunit(6), fmt = 61234) kmswit(i), kmswit(j), akey(i), tstat(i), topen(j)
  end do
61234 format (1x,2i6,3e15.6)
4099 if (n1 .ge. n2)   go to 4100
  kill = 151
  lstat(19) = 4090
  lstat(13) = ncompt
  lstat(14) = n1
  lstat(15) = n2
  lstat(16) = n3
  go to 9200
4100 do l = 1, ncompt
     if (l .gt. nhalf) go to 4110
     frandn(l) = -aincr * (nhalf - l)
     cycle
4110 frandn(l) = aincr * (l - nhalf)
  end do
409 if (nenerg .lt. 0) go to 188
  call time44 (atim)
  call runtym (d1, d2)
  seed = seedy (atim(1 :)) + 1000.0d0 * (d1 + d2)
  seed = 2.0d0 * seed
  if (jseedr .ge. nenerg) seed = seedr
  d8 = randnm (seed)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 646) knt, kswtch, jseedr, nenerg, itest, idist, seed, d8
646 format (/, ' in  =over12=  just after 1st use of  =randnm= .', /, 1x, '     knt  kswtch  jseedr  nenerg   itest   idist', 16x, 'seed', 18x, 'd8', /, 1x, 6i8, 2e20.10)
  bias = 0.0d0
  if (itest .eq. 1) go to 1646
  if (knt .gt. 1) go to 1615
  nswtpe = 0
  do i = 1, kswtch
     if (absz (akey(i)) .ne. 44444.0d0) cycle
     if (akey (i) .eq. 44444.0d0) go to 1607
  end do
  go to 1608
1607 nswtpe = nswtpe + 1
1608 do i = 1, kswtch
     if (absz (akey(i)) .ne. 44444.0d0) cycle
     if (akey (i) .eq. -44444.0d0) go to 1612
  end do
  go to 1615
1612 nswtpe = nswtpe + 1
1615 secfrq = (1.0d0 / statfr) / 360.0d0
  window = (degmax - degmin) * secfrq
  if (window .lt. 0.0d0) window = 360.0d0 * secfrq
  zerofl = 0.0d0
  d14 = knt
  d14 = d14 / nenerg
  do j = 1, nswtpe
     if (linsys .ne. 1) d14 = randnm (zerofl)
     ranoff = d14 * window + degmin * secfrq
     !     write (*,*) ' knt, nenerg, window, degmin, secfrq, d14, ranoff =',
     !     1              knt, nenerg, window, degmin, secfrq, d14, ranoff
     if (j .gt. 1) go to 1619
     !     bias = ranoff
     dum9 = ranoff
     angle = ranoff * 360.0d0 * statfr
     cycle
1619 bias2 = ranoff
     angtpe = ranoff * 360.0d0 * statfr
  end do
1646 do ksw = 1, kswtch
     if (absz (akey(ksw)) .ne. 44444.0d0) cycle
     ndxi = ksw + lswtch
     if (itest .eq. 1) go to 664
     if (itest .ge. 2) go to 1663
     if (itest .eq. 0 .and. nswtpe .lt. 2) go to 1630
     if (akey(ksw) .eq. 44444.0d0) go to 1630
     bias = bias2
     go to 664
1630 bias = dum9
     go to 664
1663 if (itest .gt. 2) go to  662
     if (akey(ksw) .eq. +44444.0d0) go to 663
     bias = 0.0d0
     go to 664
663  bias = ranoff
     go to 664
662  if (akey(ksw) .eq. -44444.0d0) go to 663
     bias = 0.0d0
664  timev = tstat(ksw)
     n = kdepsw(ksw)
     if (kloaep .eq. 0) go to 665
     if (n .gt. 0) go to 665
     timev = tstat(kloaep)
665  rng = randnm(zerofl)
     if (iprsup .ge. 2) write (unit = lunit(6), fmt = 666) ksw, rng, bias, angle
666  format (/, ' At 666  of  "over12" .   ksw, rng, bias, angle =', 1i10, 3e16.6)
     if (idist .gt. 0)  go to 675
     if (topen(ndxi) .lt. 0.0d0) go to 675
     d9 = rng
     if (d9 .eq. 0.0d0) go to 665
     if (d9 .eq. 1.0d0) go to 665
     if (2.0d0 * d9 .gt. 1.0d0) d9 = 1.0d0 - d9
     d7 = alogz (1.0d0 / (d9 * d9))
     eh = sqrtz (d7)
     xn = 2.515517d0 + eh * (0.802853d0 + 0.010328d0 * eh)
     xd = 1.0d0 + eh * (1.432788d0 + eh * (0.189269d0 + 0.001308d0 * eh))
     xq = eh - xn / xd
     if (xq .gt. sigmax) xq = sigmax
     if (2.0d0 * rng .lt. 1.0d0) xq = -xq
     rng = timev + topen(ndxi) * xq + bias
     go to 680
675  d1 = 3.0d0
     rng = timev + bias + sqrtz (d1) * absz (topen(ndxi)) * (2.0d0 * rng - 1.0d0)
680  if (akey(ksw) .eq. -44444.0d0) go to 7680
     tclose(ksw) = rng
     if (n .eq. 0) cycle
     if (akey(n) .eq. 44444.0d0) tclose(ksw) = tclose(n) + tclose(ksw) - bias
     if (akey(n) .eq. -44444.0d0) tclose(ksw) = topen(n) + tclose(ksw) - bias
     if (tclose(ksw) .lt. deltat) go to 690
     cycle
7680 topen(ksw) = rng
     if (n .eq. 0) cycle
     if (akey(n) .eq. 44444.0d0) topen(ksw) = tclose(n) + topen(ksw) - bias
     if (akey(n) .eq. -44444.0d0) topen(ksw) = topen(n) + topen(ksw) - bias
     if (topen(ksw) .lt. deltat) go to 690
     cycle
690  call time44 (atim(1 :))
     call runtym (d1, d2)
     !     seed = seedy(atim(1)) + 1000.*(d1+d2)
     seed = int (seedy (atim(1 :)) + 1000.0d0 * (d1 + d2))
     !     seed = 2.0*seed
     seed = int (2.0 * seed, kind (seed))
     d8 = randnm (seed)
     rng = randnm (zerofl)
     ltdelt = ltdelt + 1
     if (akey(ksw) .eq. 44444.0d0) tclose(ksw) = 0.0d0
     if (akey(ksw) .eq. -44444.0d0) topen(ksw) = 0.0d0
  end do
  if (moncar(7) .le. 0) go to 300
  !     Now read user-supplied tclose of "user supplied switch times"
  n17 = moncar(7)
  read (unit = n17, fmt = 4372) (tclose(ksw), ksw = 1, kswtch)
  read (unit = n17, fmt = 4372) (topen(ksw), ksw = 1, kswtch)
4372 format (5e15.6)
300 write (unit = lunit(3)) ranoff, (tclose(k1), k1 = 1, kswtch)
  write (unit = lunit(3)) ranoff, (topen(k1), k1 = 1, kswtch)
  if (iprsup .ge. 1 ) write (unit = lunit(6), fmt = 4624) knt, kbase, kloaep, mtape, nenerg, jseedr, ltdelt, kswtch,  seedr, ranoff, (tclose(k1), k1 = 1, kswtch), (topen(k), k = 1, kswtch)
4624 format (/, " at 4624 of  'over12' .  ", '     knt   kbase  kloaep   mtape  nenerg  jseedr  lt             delt  kswtch   ', 10x, 'seedr', 9x, 'ranoff', /, 23x, 8i8, 2e15.5, /, (4x, 8e16.6))
  if (isw .ne. 4444) go to 703
  npair = 0
  irbase = ioftab
  if (idist .gt. 0) go to 840
  do j = 1 , kswtch
     if (absz (akey(j)) .ne. 44444.0d0) cycle
     k = j + lswtch
     if (topen(k) .lt. 0) cycle
     if (kloaep .eq. 0) go to 752
     if (kdepsw(j) .eq. 0) go to 751
     tstatj = tstat(j) + tstat(kloaep)
     go to 754
751  tstatj = tstat(kloaep)
     go to 754
752  if (kdepsw(j) .eq. 0) go to 753
     n = kdepsw(j)
     tstatj = tstat(j) + tstat(n)
     go to 754
753  tstatj = tstat(j)
754  j1 = j - 1
     do i=1, j1
        if (absz (akey(i)) .ne. 44444.0d0) cycle
        if (i .eq. j) cycle
        ni = i + lswtch
        if (topen(ni) .lt. 0) cycle
        npair = npair + 1
        tdiff = tclose(i) - tclose(j)
        if (kloaep .eq. 0) go to 762
        if (kdepsw(i) .eq. 0) go to 761
        tstati = tstat(i) + tstat(kloaep)
        go to 764
761     tstati = tstat(kloaep)
        go to 764
762     if (kdepsw(i) .eq. 0) go to 763
        n = kdepsw(i)
        tstati = tstat(i) + tstat(n)
        go to 764
763     tstati = tstat(i)
764     tmean = tstati - tstatj
        tdiff = tdiff - tmean
        sumdt = sqrtz (topen(ni) * topen(ni) + topen(k) * topen(k))
        fsigma = sigmax * sumdt
        if (absz (tdiff) .le. fsigma) go to 800
        ii = ii + 1
        cycle
800     sumdt = sumdt * aincr
        !       k1 = tdiff / sumdt
        k1 = int (tdiff / sumdt)
        if (iprsup .ge. 1) write (unit = lunit(6), fmt = 803) npair, tdiff, sumdt, k1
803     format (' npair,tdiff,sumdt,k1=', 10x, i6, 2e15.6, i6)
        if (tdiff .lt. 0.0d0) go to 805
        k1 = k1 + nhalf + 1
        go to 810
805     k1 = k1 + nhalf
810     k1 = k1 * n4 - 1
        n6 = irbase + k1
        irandn(n6) = irandn(n6) + 1
        irbase = irbase + iofcnt
     end do
  end do
  knt = knt + 1
  if (knt .le. nenerg) go to 409
840 continue
  npair = 0
  irbase = ioftab
  do j = 1, kswtch
     if (absz (akey(j)) .ne. 44444.0d0) cycle
     j1 = j - 1
     do i = 1, j1
        if (absz (akey(i)) .ne. 44444.0d0) cycle
        if (i .eq. j) cycle
        npair = npair + 1
        k1 = kmswit(j)
        ndx1 = lswtch + j
        k2 = kmswit(ndx1)
        k3 = kmswit(i)
        ndx2 = lswtch + i
        k4 = kmswit(ndx2)
        bus1 = bus(k1)
        bus2 = bus(k2)
        bus3 = bus(k3)
        bus4 = bus(k4)
        write (unit = lunit(6), fmt = 845) npair, bus1, bus2, bus3, bus4
845     format (//, ' switch pair', i4, 15x, "  '", a6, "'  to  '", a6, "'   and   '", a6, "'  to  '", a6, "'")
        if (idist .gt. 0) go to 850
        if (topen(ndx1) .lt. 0) go to 850
        if (topen(ndx2) .lt. 0) go to 850
        go to 858
850     write (unit = lunit(6), fmt = 855)
855     format(' In this pair either one or both switch closing times are generated according to the uniform distribution and it will be skipped.')
        cycle
858     n3 = ncompt- 1
        do l = 1, n3
           k = l + 1
           k1 = k * n4 - 1
           l1 = l * n4 - 1
           n7 = irbase + k1
           n8 = irbase + l1
           irandn(n7) = irandn(n8) + irandn(n7)
           total = nenerg
           m = ncompt + k
           frandn(m) = irandn(n7) / total
           d9 = (k - nhalf) * aincr
           az = absz ( d9 )
           t = 1.0d0 / (1.0d0 + 0.2316419d0 * az)
           d1 = 0.3989423d0 * expz (-d9 * d9 * onehaf)
           d8 = 1.0d0 - d1 * t * ((((1.330274d0 * t - 1.821256d0) * t + 1.781478d0) * t - 0.3565638d0) * t + 0.3193815d0)
           if (d9 .ge. 0.0d0) go to 864
           d8 = 1.0d0 - d8
864        n9 = m + ncompt
           frandn(n9) = d8
        end do
        !868     continue
        irbase = irbase + iofcnt
        n1 = 1
        n2 = 14
        n3 = 2 * ncompt
869     if (n2 .gt. ncompt) n2 = ncompt
        write (unit = lunit(6), fmt = 870) (frandn(l), l = n1, n2)
870     format (1x, 'time', 10x, 14f8.4)
        n6 = n1 + ncompt
        n7 = n2 + ncompt
        write (unit = lunit(6), fmt = 871) (frandn(l), l = n6, n7)
871     format (1x, 'sample', 8x, 14f8.4)
        n8 = n1 + n3
        n9 = n2 + n3
        write (unit = lunit(6), fmt = 872) (frandn(l), l = n8, n9)
872     format (1x, 'theoretical', 3x, 14f8.4, /, 1x)
        n1 = n1 + 14
        if (n1 .gt. ncompt) cycle
        n2 = n2 + 14
        go to 869
     end do
     if (ii .eq. 0) cycle
     write (unit = lunit(6), fmt = 893) ii, nenerg, sigmax
893  format (/, i6, ' out of', i6, ' switch closing times falling beyond', f8.4, ' times the standard deviation.')
  end do
  lstat(32) = kswtch
  xmaxmx = 1.0d0
  aincr = 1.0d0
  numnvo = 1
  nc = 1
  call runtym (d1, d2)
  flstat(5) = flstat(5) + d1
  flstat(6) = flstat(6) + d2
  flstat(9) = d1
  flstat(10) = d2
  lastov = 12
  nchain = 29
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
  go to 99999
703 if (isw .eq. 0) go to 702
  if (knt .eq. 1) go to 702
  n6 = 0
  do k1 = 1, kswtch
     if (iabs (kpos(k1)) .eq. 2) go to 4208
     if (akey(k1) .ne. 44444.0d0) cycle
     volti(n6 + 1) = tclose(k1)
     go to 4213
4208 if (akey(k1) .ne. -44444.0d0) cycle
     volti(n6 + 1) = topen(k1)
4213 n6 = n6 + 1
     if (n6 .lt. 2 * lsiz26) go to 4215
     lstat(19) = 4213
     iprint = 26
     go to 9200
4215 lastxx(n6) = k1
  end do
  write (unit = lunit(6), fmt = 701) knt, (lastxx(k1), volti(k1), k1 = 1, n6 )
701 format (/, 32x, 'Random switching times for energization number', i4, /, (32x, 5(i4, e16.6)))
702 if (knt .eq. 1) go to 598
  n9 = ipntv(11)
  if (n9 .le. 0) go to 6666
  do i = 1, n9
     read (unit = lunit(5), fmt = 1702) d1
1702 format (a1)
  end do
  !2702 continue
6666 lastov = 12
  nchain = 16
  go to 9800
188 klorow = ipntv(1)
  ilorow = 1
  if (kloaep .eq. 0)   go to 190
  klorow = kloaep
  ilorow = iloaep
190 nj = klorow + lswtch
  ns2 = int (tdns(klorow), kind (ns2))
  if (numref .le. 1) go to 201
  if (knt .eq. 1) indtv(ilorow) = 1
  j = 0
198 j = j + 1
  if (j .ne. ilorow) go to 195
  if (j .eq. numref) go to 201
  go to 199
195 indtv(j) = indtv(j) + 1
  n1 = ipntv(j)
  ns1 = int (tdns(n1), kind (ns1))
  if (indtv(j) .le. ns1) go to 200
  indtv(j) = 1
  if (j .eq. numref) go to 201
  go to 199
201 indtv(ilorow) = indtv(ilorow) + 1
  if (indtv(ilorow) .gt. ns2)   go to 702
  go to 200
199 if (j .lt. numref) go to 198
200 tstbeg = tstat(klorow)
  if (itest .gt. 0) go to 203
  tstbeg = tstbeg - 0.5d0 * (tdns(klorow) - 1.0d0) * topen(nj)
203 if (akey(klorow) .eq. 44444.0d0) tclose(klorow) = tstbeg + (indtv(ilorow) - 1) * topen(nj)
  if (akey(klorow) .eq. -44444.0d0) topen(klorow) = tstbeg + (indtv(ilorow) - 1) * topen(nj)
  k = 1
  do i = 1, kswtch
     if (k .ne. ilorow) go to 207
     k = k + 1
     if (indtv(k) .eq. 0) indtv(k) = 1
207  if (i .eq. klorow) cycle
     if (ipntv(k) .eq. 0) go to 210
     if (i .eq. ipntv(k)) go to 220
210  if (absz (akey(i)) .ne. 44444.0d0) cycle
     n = kdepsw(i)
     if (akey(i) .eq. 44444.0d0) tclose(i) = tclose(n) + tstat(i)
     if (akey(i) .eq. -44444.0d0) topen(i) = topen(n) + tstat(i)
     cycle
220  j = i + lswtch
     if (kloaep .eq. 0) go to 225
     n2 = ipntv(k)
     if (akey(i) .eq. 44444.0d0) tclose(i) = tclose(kloaep) + topen(j)*(indtv(k) - 0.5d0 - 0.5d0 * tdns(n2))
     if (akey(i) .eq. -44444.0d0) topen(i) = topen(kloaep) + topen(j) * (indtv(k) - 0.5d0 - 0.5d0 * tdns(n2))
     go to 228
225  tstbeg = tstat(i)
     if (itest .gt. 0) go to 227
     tstbeg = tstat(i) - 0.5d0 * (tdns(i) - 1.0d0) * topen(j)
227  if (akey(i) .eq. 44444.0d0) tclose(i) = tstbeg + (indtv(k) - 1) * topen(j)
     if (akey(i) .eq. -44444.0d0) topen(i) = tstbeg + (indtv(k) - 1) * topen(j)
228  k = k + 1
     if (indtv(k) .eq. 0) indtv(k) = 1
  end do
  go to 300
  !     Preparing r,x,
598 d2 = 1.0d0 / (deltat * 500.0d0)
  if (m4plot .ne. 1) go to 5763
  if (kbrser .ne. 1) go to 5763
  n24 = kbreak
  kbrser = 2
  kbreak = 1
  call emtspy
  kbreak = n24
5763 if (iprsup .lt. 2) go to 6300
  write (unit = lunit(6), fmt = 6) nenerg
6 format (/, ' At s.n. 6 of over12.   nenerg =', i3)
  do i = 1 , kswtch
     ndx3 = lswtch + i
     write (unit = lunit(6), fmt = 9) kmswit(i), kmswit(ndx3), akey(i), tstat(i), topen(ndx3)
  end do
9 format (1x, 2i6, 3e15.6)
6300 if (numsm .le. 0) go to 901
  call elecyy
  call premec
901 if (it .eq. 0) go to 433
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 65435) (tr(i), tx(i), r(i), c(i), i = 1, it)
65435 format (/, ' Lumped-element branch-parameter values in "over12", prior to processing.', /, 18x, 'tr', 18x, 'tx', 19x, 'R', 19x, 'C', /, (1x, 4e20.10))
  k = 1
5527 j = length(k)
  xopt = xoptbr(k)
  copt = coptbr(k)
  d22 = d2
  ci1 = d2 / 1000.0d0
  if (xopt .gt. 0.0d0) d22 = d2 * 1000.0d0 / (twopi * xopt)
  if (copt .gt. 0.0d0) ci1 = ci1 / (twopi * copt)
  !     if (kodsem(k) .ne. 0  .and. imodel(k) .ne. -2
  !     1                      .or.  imodel(k) .ne. -4) j = kodebr(k)
  if (kodsem(k) .ne. 0 .and. imodel(k) .ge. 0) j = kodebr(k)
  if (j .eq. 0) j = 1
  j = iabs (j)
  if (kbus(k) .lt. 0) go to 5539
  if (kodebr(k) .gt. 0) go to 5539
  if (length(k) .lt. 0) go to 5539
  n1 = iabs (nr(k))
  n2 = n1 - 1 + j * (j + 1) / 2
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5528) k, length(k), nr(k), n1, n2, d22
5528 format (/, ' At 5528.   ', 5i10, e15.5)
  !     Check for the presence of the -666 branches  *   *   *   *   *   *
  if (length(k + 1) .ne. -666) go to 905
  !903 call dteqiv( ikf, isfd, d22, azr, azi )
  call dteqiv (ikf, isfd, d22, azr, azi)
  !     Convert modal impedances to phase frame of reference *   *   *   *
  cz = j
  azr = (azr - azi) / cz
  lb = n1
  ka = 1
  !     Load the equivalent impedances into the  tr  tables  *   *   *   *
  do kb = n1, n2
     tr(kb) = azr
     tx(kb) = 0.0d0
     if (lb .ne. kb) cycle
     tr(lb) = tr(lb) + azi
     ka = ka + 1
     lb = lb + ka
  end do
905 do i = n1, n2
     c(i) = c(i) * ci1
     x(i) = tr(i) + tx(i) * d22
     tx(i) = -x(i)
  end do
5539 k = k + j
  if (k .le. ibr) go to 5527
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 5543) (x(i), c(i), i = 1, it)
5543 format ( /, ' (X(i), c(i), i=1, it)   after conversion to  R + 2*L/deltat .', /, (1x, 8e16.7))
433 if (kswtch .eq. 0) go to 430
  if (iprsup .ge. 1) go to 455
  go to 477
455 write (unit = lunit(6), fmt = 2666)
2666 format (/, ' Switch table vectors, before switch table processing in  ', "'", 'over12', "' .", /, '     row   bus1    bus2     kpos  isourc', 14x, 'tclose',  15x,  'topen',  16x, 'crit', 14x, 'fkposi')
  do k = 1, kswtch
     ndx4 = lswtch + k
     write (unit = lunit(6), fmt = 2713) k, kmswit(k), kmswit(ndx4), kpos(k), isourc(k), kswtyp(k), tclose(k), topen(k), crit(k)
  end do
2713 format (6i8, 3e20.10)
477 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2714) (k, energy(k), adelay(k), k = 1, kswtch)
2714 format (/, ' continuation of switch table vectors.', /, 5x, 'row', 9x, 'energy',  9x, 9x, 'adelay', /, (i8, 2e15.6))
  do k = 1, kswtch
     n1 = kmswit(k)
     ndx5 = lswtch + k
     n2 = kmswit(ndx5)
     icheck = kswtyp(k)
     if (absz (adelay(k)) .eq. 44444.0d0) cycle
     if (adelay(k) .le. 0.0d0) go to 428
     adelay(ndx5) = adelay(k)
     cycle
428  if (icheck .le. 0) cycle
     if (istead .eq. 0) cycle
     if (icheck .eq. 9976) cycle
     nn1 = kbus(icheck)
     n15 = nn1
     if (n15 .eq. 1) n15 = n2
     n16 = iabs (kssfrq(n15))
     omega = twopi * sfreq(n16)
     ck1 = (f(nn1) - f(n2)) / omega
     if (noutpr .eq. 0) write (unit = lunit(6), fmt = 427) bus(nn1), bus(n2), ck1
427  format (' Initial flux in coil ', "'", a6, "'", ' to ', "'", a6, "'", ' =', e13.5)
     ck(icheck) = ck1 - (e(nn1) - e(n2)) * delta2
     if (absz (ck1) .gt. topen(k) .and. noutpr .eq. 0) write (unit = lunit(6), fmt = 426)
426  format (' Warning.  Assumption that AC steady state has fundamental frequency only is questionable with preceding flux outside linear region')
  end do
  !     Inversion of matrices of coupled
  !     Presetting tables for lossle
430 k = 1
  itadd = it + 1
  n1 = ibr+1
  if (n1 .lt. 10) n1 = 10
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5306) (i, kbus(i), mbus(i), length(i), kodebr(i), kodsem(i), indhst(i), cik(i), cki(i),ckkjm(i), i = 1, n1)
5306 format (/, '     row    kbus    mbus  length  kodebr  kodsem     indsem', 12x, 'cik', 12x, 'cki', 10x, 'ckkjm', /, (7i8, 3e15.6))
  n5 = k
  iline = 1
  iprint = 8
500 if (kbus(k) .lt. 0) go to 535
  it2 = length(k)
  if (it2 .lt. 0) go to 520
  i = nr(k)
  if (it2 .le. 1) go to 510
  if (kodebr(k) .le. 0) go to 4460
  l = nr(k)
  jt = it2 * (it2 + 1) / 2
  call mover (tx(l :), cmr(1 :), jt)
  d1 = -1.0d0 / delta2
  call addmxd (cmr(1 :), d1, cmr(1 :), it2)
  call reduct (cmr(1 :), it2, ll0)
  ii = l + jt - 1
  if (iprsup .ge. 2) write(unit = lunit(6), fmt = 4435) (cmr(i), i = 1, jt)
4435 format(/, ' At 4435.  Intermed. matrix (d)', /, (1x, 6e20.10))
  call multmx (cmr(1 :), tr(l :), x(l :), cmi(1 :), it2)
  call multmx (cmr(1 :), tx(l :), r(l :), cmi(1 :), it2)
  call addmxd (r(l :), one , r(l :), it2)
  do i = l, ii
     r(i) = 2.0d0 * r(i)
  end do
  if (iprsup .ge. 2) write(unit = lunit(6), fmt = 4451) k, l, it2, (x(i), r(i), i = l, ii)
4451 format(/, ' At 4451.  Final (g), (h) from (a), (b).   ', 3i10, /, (1x, 6e20.10))
  go to 520
4460 call reduct (tx(i :), it2, ll0)
  ii = i - 1
  n2 = 1
501 do l = 1, it2
     if (l .le. n2) go to 504
     n1 = n1 + l - 1
     go to 502
504  n1 = ii + l
502  volt(l) = tx(n1)
  end do
  call mult (tr(i :), volt, volti, it2, ll0)
  call mult (tx(i :), volti, volt, it2, ll0)
  do l = 1, n2
     ii = ii + 1
     r(ii) = (tx(ii) - volt(l)) * 2.0d0
  end do
  n2 = n2 + 1
  if (n2 .le. it2) go to 501
  go to 520
530 i = iabs(i)
  gus1 = c(i)
  if (gus1 .ne. 0.0d0) gus1 = 1.0d0 / gus1
  gus2 = 1.0d0 / (x(i) + gus1)
  r(i) = gus2 * (tr(i) * 2.0d0 - x(i) + gus1)
  x(i) = gus2
  c(i) = gus1
  go to 520
535 it2 = iabs(length(k))
  if (kodsem(k) .eq. 0 .or. imodel(k) .eq. -2) go to 5349
  if (kodsem(k) .ne. 0 .and. imodel(k) .eq. -4) go to 5349
  n1 = k
  n3 = 1
5350 if (cki(n1) .lt. 0.0d0) go to 5353
  n1 = n1 + 1
  n3 = n3 + 1
  go to 5350
5353 n2 = k + n3 - 1
  length(n2) = n3
  it2 = iabs(kodebr(k))
  n1 = k + it2 - 1
  n2 = itadd
  do i = k, n1
     nr(i) = n2
     n2 = n2 + 1
  end do
  !5348 continue
  k = k + n3 - it2
  go to 520
5349 a = it2
  ii = 1
534 if (ck(k) .ge. 0.0d0) go to 6482
  kf = nr(k)
  cik(k) = kf
  i = nhist(kf)
  nr(k) = i
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 20601) kf, i, k, ii, iline, it2, kbus(k), mbus(k)
20601 format (/, ' Freq. dependent mode.      kf       i       k      ii   iline     it2    kbus    mbus', /, 22x, 8i8)
  if (iskip(kf) .gt. 0) go to 537
  h2 = 0.0d0
  go to 11536
6482 h2 = cik(k)
  h3 = h2 / deltat
  !  i = h3
  i = int (h3)
  !     l
  if (imodel(k) .eq. -2) go to 50001
  if (imodel(k) .eq. -4) go to 50006
  go to 50005
  !
50006 i = -2
  inoff1 = lbrnch
  inoff2 = 2 * lbrnch
  inoff3 = 3 * lbrnch
  inoff4 = 4 * lbrnch
  inoff5 = 5 * lbrnch
  koff1 = 900
  koff2 = koff1 + isecti
  koff3 = koff2 + isecti
  koff4 = koff3 + isecti
  koff5 = koff4 + isecti
  koff6 = koff5 + isecti
  koff7 = koff6 + isecti
  koff8 = koff7 + isecti
  koff9 = koff8 + isecti
  koff10 = koff9 + isecti
  koff13 = koff10 + isecti
  koff14 = koff13 + isecti
  koff15 = koff14 + isecti
  koff16 = koff15 + isecti
  koff17 = koff16 + isecti
  koff18 = koff17 + isecti
  koff19 = koff18 + isecti
  koff20 = koff19
  if (jglnn .ne. 0) go to 8893
  do jlk = 1, ibr
     if (kodsem(jlk) .eq. 0) cycle
     nwww = length(jlk)
     nqtw = nqtw + nwww ** 2
     nph = nph + nwww
     jglnn = jglnn + 1
  end do
  if (nph .le. isecti) go to 8891
  write(unit = lunit(6), fmt = 8892) isecti, nph
8892 format(' in over12, offset variable isecti =', i5, ' is too samll.  It should be increased to the value of ',i5, /, 'which is the number of coupled phases currently modelled with new marti frequency-dependent format.     Stop.')
  stop
8891 do iq = 1, nqtw
     nqtt = int (nqtt + wk1(koff20 + iq), kind (nqtt))
  end do
  !1988 continue
8893 koff21 = koff20 + nqtt
  koff22 = koff21 + lbrnch
  koff23 = koff22 + nqtw
  koff24 = koff23 + nqtw
  koff25 = koff24 + 288
  nr(k) = i
  n1 = kodsem(k)
  nrz = int (cki(k), kind (nrz))
  nra = int (ckkjm(k), kind (nra))
  n8 = n1 + 5 * nrz + 2 * nra + 1
  n9 = n8 + 1
  sconst(n9) = sconst(n1)
  sconst(n8) = 0.0d0
  nn8 = n1 + 5 * nrz + 5 * nra + 3
  nn9 = nn8 + 1
  nk1 = nn9 + 1
  sconst(nn8) = 0.0d0
  sconst(nn9) = 0.0d0
  do j = 1, nrz
     n2 = n1 + j
     n3 = n2 + nrz
     fac1 = sconst(n3) * deltat
     fac2 = sconst(n2) / sconst(n3)
     n5 = n1 + 2 * nrz + 2 * nra + j
     sconst(n5) = expz (-fac1)
     hi = (one - sconst(n5)) / fac1
     n6 = n5 + nrz
     sconst(n6) = fac2 * (one - hi)
     n7 = n6 + nrz
     sconst(n7) = -fac2 * (sconst(n5) - hi)
     sconst(n8) = sconst(n8) + sconst(n7)
     sconst(n9) = sconst(n9) + sconst(n6)
     wk1(koff21 + k) = sconst(n9)
     if (iprsup .gt. 0) write(unit = *, fmt = 5487) j, nrz, sconst(n5), sconst(n6), sconst(n7), sconst(n8), sconst(n9)
5487 format(' j,nrz,cj,dj,ej,esum,d0sum=y05', /, 1x, i3, 2x, i3, 3x, e14.5, 3x, e14.5, 3x, e14.5, 3x, e14.5, 3x, e14.5)
  end do
  !50007 continue
  jgl = 0
  !     if ( iprsup .gt. 0 )
  !    1 write(lunit(6),4445)
  !  445  format(19h   j  nra  scst(n5),
  !    130h  scst(n6)  scst(n7)  sst(nn8),
  !    330h  sct(nn9)  st(nn10)  st(nn11),
  !    430h  st(nn12)  st(nn13)  st(nn14),
  !    530h  st(nn15)      fac3      fac4,
  !    630h      fac5      sk1r      sk1i,
  !    710h       swd)
  do j = 1, nra
     n2 = n1 + 2 * nrz + j
     n3 = n2 + nra
     n5 = n1 + 5*nrz + 2 *nra + 2 + j
     n6 = n5 + nra
     n7 = n6 + nra
     if (absz (sconst(n2)) .ge. 1.e+13) go to 5324
     if (absz (sconst(n2)) .eq. 0.0d0) go to 5324
     fac1 = sconst(n3) * deltat
     fac2 = sconst(n2) / sconst(n3)
     sconst(n5) = expz (-fac1)
     hi = (one - sconst(n5)) / fac1
     sconst(n6) = fac2 * (one - hi)
     sconst(n7) = -fac2 * (sconst(n5) - hi)
     sconst(nn8) = sconst(nn8) + sconst(n7)
     sconst(nn9) = sconst(nn9) + sconst(n6)
     go to 5325
5324 jgl = jgl + 1
     if (jgl .eq. 2) go to 5486
     fac1 = sconst(n3) * deltat / 1.e15
     sconst(n5) = expz (-fac1)
     dblpr2 = sconst(n5)
     fac3 = -sconst(n3 + 1) * deltat / 1.e15
     fac4 = sconst(n3 + 1) / 1.e15
     fac5 = sconst(n3) / 1.e15
     call dcosz (fac3, dblpr3)
     call dsinz (fac3, dblpr4)
     dblpr1 = dblpr2 * dblpr3
     dblpr2 = dblpr2 * dblpr4
     d18 = fac4*fac4 + fac5*fac5
     nn10 = nk1 + 1
     nn11 = nn10 + 1
     nn12 = nn11 + 1
     nn13 = nn12 + 1
     nn14 = nn13 + 1
     nn15 = nn14 + 1
     nk1 = nk1 + 6
     sk1r = sconst(n2)/1.e+15
     sk1i = sconst(n2+1)/1.e+15
     rll = ((unity - dblpr1) * fac5 - dblpr2 * fac4)
     sll = -(dblpr2 * fac5 + (unity - dblpr1) * fac4)
     yll = sk1r * rll - sk1i * sll
     xll = sk1r * dblpr1 - sk1i * dblpr2
     sconst(n5) = dblpr1
     sconst(n6) = sk1r - yll / (deltat * d18)
     sconst(n7) = -(xll - yll / (deltat * d18))
     sconst(nn8) = sconst(nn8) + sconst(n7)
     sconst(nn9) = sconst(nn9) + sconst(n6)
     ! 5325 if ( iprsup .gt. 0 )
     !    1 write(*,5488)j,nra,sconst(n5),sconst(n6),
     !    2       sconst(n7),sconst(nn8),sconst(nn9),
     !    3   sconst(nn10), sconst(nn11), sconst(nn12),
     !    4   sconst(nn13), sconst(nn14), sconst(nn15),
     !    5   fac3, fac4, fac5, sk1r, sk1i, swd
     !  488  format(1x,i3,2x,i3, 17e10.3 )
5325 if (absz (sconst(n2)) .le. 1.e+14) cycle
     if (jgl .eq. 1) cycle
5486 jgl = 0
  end do
  go to 5335
  !
50001 i = -2
  nr(k) = i
  n1 = kodsem(k)
  nrz = int (cki(k), kind (nrz))
  nra = int (ckkjm(k), kind (nra))
  n8 = n1 + 5 * nrz + 2 * nra + 1
  n9 = n8 + 1
  sconst(n9) = sconst(n1)
  sconst(n8) = 0.0d0
  nn8 = n1 + 5 * nrz + 5 * nra + 3
  nn9 = nn8 + 1
  sconst(nn8) = 0.0d0
  sconst(nn9) = 0.0d0
  do j = 1, nrz
     n2 = n1 + j
     n3 = n2 + nrz
     fac1 = sconst(n3) * deltat
     fac2 = sconst(n2) / sconst(n3)
     n5 = n1 + 2 * nrz + 2 * nra + j
     sconst(n5) = expz (-fac1)
     hi = (one - sconst(n5)) / fac1
     n6 = n5 + nrz
     sconst(n6) = fac2 * (one - hi)
     n7 = n6 + nrz
     sconst(n7) = -fac2 * (sconst(n5) - hi)
     sconst(n8) = sconst(n8) + sconst(n7)
     sconst(n9) = sconst(n9) + sconst(n6)
  end do
  !50003 continue
  do j = 1, nra
     n2 = n1 + 2*nrz + j
     n3 = n2 + nra
     fac1 = sconst(n3) * deltat
     fac2 = sconst(n2) / sconst(n3)
     n5 = n1 + 5 * nrz + 2 * nra + 2 + j
     sconst(n5) = expz (-fac1)
     hi = (one - sconst(n5)) / fac1
     n6 = n5 + nra
     sconst(n6) = fac2 * (one - hi)
     n7 = n6 + nra
     sconst(n7) = -fac2 * (sconst(n5) - hi)
     sconst(nn8) = sconst(nn8) + sconst(n6)
     sconst(nn9) = sconst(nn9) + sconst(n7)
  end do
  !50004 continue
  !
  !     should put the calculation of ti integer here using
  !     same way as zc & a1 had been done above
  !                   ljg. april 20, 1987
  !
  ci(k)=one/(sconst(n9)*a)
  ck(k)=1.0
  cik(k)=1.0
  go to 5335
50005 if(istead.gt.0) go to 531
  if (h2 .le. tmax) go to 531
  i = -2
531 n4 = int (h3, kind (n4))
  d1 = n4
  cik(k) = h3 - d1
  if(i .ne. 0) go to 537
11536 kill = 29
  lstat(19) = 531
  flstat(16) = h2
12536 n1 = iabs( kbus(k) )
  n2 = iabs( mbus(k) )
  bus1 = bus(n1)
  bus2 = bus(n2)
  go to 9200
537 if(ii.eq.1) length(k)=iline
  iline=iline+i+2
  nr(k) = i
  !5537 if (iline .lt. lpast) go to 536
  if (iline .lt. lpast) go to 536
  lstat(19) = 537
  go to 9000
536 if (ck(k) .ge. 0.0) go to 5074
  ck(k) = -1.0
  yx = zinf(kf)
  go to 533
5074 ck1 = ck(k) * onehaf
  yx=ci(k)
  if(yx.lt.0.) go to 532
  yx=yx+ck1 / 2.0
  ck(k)=(yx-ck1)/yx
  if ( ck(k)  .gt.  0.0 )   go to 533
  kill = 102
  lstat(19) = 536
  lstat(15) = k
  flstat(14) = ck1 * 2.0
  flstat(15) = yx - 0.5*ck1
  go to 12536
532 ck(k)=expz(ck1/yx)
533 ci(k)=1.0/(yx*a)
  !5331 if ( iprsup  .lt.  4 )   go to 5335
  if (iprsup .lt. 4) go to 5335
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 5334) k, ii, length(k), iline, it2, i, n3, h3, cik(k)
5334 format (/, ' after processing new mode of line.', /, '       k      ii  length   iline     it2       i      n3', 13x, 'h3', 9x, 'cik(k)', /, 7i8, 2e15.6)
5335 if (ii .lt. it2) go to 5375
  it2 = 1
  go to 520
5375 if ( ii  .gt.  1 ) go to 538
  length(k+1)=length(k)
  length(k)=-it2
538 ii=ii+1
  k=k+1
  go to 534
510 if(i.lt.0) go to 530
  tx(i)=1.0/x(i)
  r(i)=2.0*(x(i)-tr(i))/x(i)**2
  !
  !
520 if ( kbus(k) .ge. 0 )  go to 81520
  if ( kodsem(k) .ne. 0  .and. imodel(k) .eq. -4)  go to 5698
  if ( kodsem(k) .ne. 0  .and. imodel(k) .ne. -2)  go to 81515
5698 itadd = itadd + ii * (ii+1) / 2
  go to 81520
81515 itadd = itadd + it2 * (it2+1)/2
81520 k = k + iabs(it2)
  n5 = k
  if(k.le.ibr) go to 500
  lastov = 12
  nchain = 13
  if ( iprsup  .le.  2 )   go to 9800
  write (lunit(6), 208)   ( k, kbus(k), mbus(k), nr(k), kodebr(k), length(k), kodsem(k), indhst(k), k=1, ibr )
208 format (/, " branch-table integer vectors at end 'over12' . ", /, &
       '     row    kbus    mbus      nr  kodebr  length  kodsem    indhst   ', /,( 8i8 ))
  write (lunit(6), 209)   ( k, bus(k), ci(k), ck(k), cik(k), cki(k), ckkjm(k), k=1, ibr )
209 format ( /, 5x, 3hrow, 7x, 3hbus, 13x, 2hci, 13x, 2hck, 12x, 3hcik, 12x, 3hcki,10x,5hckkjm,/,( i8,4x,a6,5e15.6 ) )
  write (lunit(6), 4394) (sconst(i), i=1, ifsem)
4394 format(/, 24h (sconst(i), i=1, ifsem)   ,/, 8(1x,e15.7))
  write (lunit(6), 4395) (cnvhst(i), i=1, ifx)
4395 format(/,22h (cnvhst(i), i=1, ifx)   ,/, 8(1x,e15.7))
  go to 9800
9000 lstat(16) = iprint
  kill = 1
9200 lastov = 12
  nchain = 51
  lstat(18) = 12
9800 if (iprsup .ge. 1)  write (unit = lunit(6), fmt = 4568)
4568 format (' Exit module "over12".')
99999 if (allocated (cmi)) then
     kknonl = transfer (cmi, kknonl)
     deallocate (cmi)
  end if
  if (allocated (cmr)) then
     kks = transfer (cmr, kks)
     deallocate (cmr)
  end if
  return
end subroutine over12

!
! subroutine dteqiv.
!

subroutine dteqiv (ikf, isfd, d2, azr, azi)
  use blkcom
  use labcom
  implicit none
  integer(4), intent(out) :: ikf, isfd
  real(8), intent(out) :: azi, azr
  real(8), intent(in) :: d2
  integer(4) :: idk, isf, ist, isu
  integer(4) :: ka, kb, kc
  real(8) :: a1, ac1, al1, ar1, arl
  real(8) :: d21
  real(8) :: ur(2)
  !
  !     This routine produces the companion model to be inserted into the
  !     tr tables. it also normalizes the l and c data to henry and farad
  idk = 2 * ikf
  ikf = ikf + 1
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 1) ikf, isfd, imfd(idk + 1), imfd(idk + 2)
1 format (' Integer counters at start of dteqiv.....', 17x, 'ikf', 6x, 'isfd', 6x, 'izkf', 6x, 'ipkf', /, 51x, 4i10)
  d21 = d2 * delta2
  do kb = 1, 2
     ur(kb) = 0.0d0
     kc = kb + idk
     isf = imfd(kc)
     ist = isfd + 1
     isu = isfd + isf * 5
     !     Process branch data for the appropriate mode *   *   *   *   *   *
     do ka = ist, isu, 5
        ar1 = rmfd(ka)
        arl = rmfd(ka + 3)
        !!     Normalize l and c data   *   *   *   *   *   *   *   *   *   *   *
        ac1 = rmfd(ka + 2) * ci1
        al1 = rmfd(ka + 1) * d2
        rmfd(ka + 1) = rmfd(ka + 1) * d21
        rmfd(ka + 2) = ac1 * delta2
        if (ac1 .ne. 0.0d0) ac1 = 1.0d0 / ac1
        !     Calculate branch impedance   *   *   *   *   *   *   *   *   *   *
        !     Process the parallel R and L connection  *   *   *   *   *   *   *
        a1 = al1
        if (arl .eq. 0.0d0 .or. al1 .eq. 0.0d0) go to 3
        a1 = (al1 * arl) / (al1 + arl)
        !     Accumulate branch admittance *   *   *   *   *   *   *   *   *   *
3       ur(kb) = ur(kb) + 1.0d0 / (ar1 + ac1 + a1)
     end do
     isfd = isu
     !     Invert the equivalent branch admittance  *   *   *   *   *   *   *
     !4    ur(kb) = 1.0d0 / ur(kb)
     ur(kb) = 1.0d0 / ur(kb)
  end do
  azr = ur(1)
  azi = ur(2)
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 5) azr, azi
5 format (' Equivalent branch impedances (modal)', 4x, 2e22.10)
  return
end subroutine dteqiv

!
! subroutine tacs2.
!

subroutine  tacs2
  use blkcom
  use labcom
  use tacsar
  use smtacs
  use tracom
  use movcop
  implicit none
  character(8) :: dumj(9), texnam(5)
  integer(4) :: i, i1, i2, i3, i30, iac, idctcs, ilk, ioutcs, ip, ipass, ite
  integer(4) :: j, j1, j2, jcm, jlk, jmset
  integer(4) :: k, k1, k2, k3, kbtcs, kbwkcs, kh, kj, kjsup, kksup, krowcs
  integer(4) :: kxai
  integer(4) :: l, ll
  integer(4) :: m, ma1, ma2, ma3, ma4, mdy5, mj, mjm, mk, mm, mrm, mtot
  integer(4) :: n, n1, n1tttt, n2, n3, n5, n6, n7, n14, n22, n33, ndx1, ndx2
  integer(4) :: ndx3, ndx4, ndx5, ndx6, ndxi, ndxj, ndy0, ndy1, ndy2, ndy5
  integer(4) :: niunrs, nj, nkk, nkk1, nn, ntcsex, nuki, nukj, nukr, nuks
  real(8) :: a, amax, angl
  real(8) :: b, bb, bi, br
  real(8) :: critia
  real(8) :: d1, d1tttt, d2, d3, d4, d5, d6, d7, d8, defi, defr, dpd, dum9(10)
  real(8) :: epslon
  real(8) :: f1, f2, freqhz
  real(8) :: omegar
  real(8) :: picon, pru, prx
  real(8) :: real, rima
  real(8) :: xaisav, xarsav, xtcsav
  !
  !  equivalence (moncar(2), kbase)
  !  equivalence (moncar(3), ltdelt)
  !
  integer(4), pointer :: kbase => moncar(2)
  integer(4), pointer :: ltdelt => moncar(3)
  !
  !1000 if (iprsup .ge. 1) write (lunit(6), 1001)  lastov, m4plot
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1001) lastov, m4plot
1001 format (' enter "tacs2".  lastov, m4plot =', 2i6)
  do i = 1, 8
     lstat( i) = lstat(i + 60)
  end do
  picon = 360. / twopi
  epslon = epsiln * 100.
  ite = 0
  niunrs = iuty( kiuty + 1 )
  ma1 = iuty(kiuty+7)
  ma2 = iuty(kiuty+8)
  if (iprsup .lt. 1) go to 6219
  write (unit = lunit(6), fmt = 6213) ioutcs, iplot, iprsup, ntcsex, nstacs, ktab, niu, nuk, nenerg, iuty(kiuty + 11)
6213 format (/, 1x, '  ioutcs   iplot  iprsup  ntcsex  nstacs    ktab     niu     nuk  nenerg  infexp', /, 10i8)
  write (unit = lunit(6), fmt = 6215) t, twopi, fltinf, xopt, copt
6215 format (1x, 14x, 't', 10x, 'twopi', 9x, 'fltinf', 11x, 'xopt', 11x, 'copt', /, 1x, 8e15.6)
6219 kxai = kxar + lstat(68)
  kbtcs = katcs + lstat(62)
  kbwkcs = kawkcs + lstat(68)
  kjsup = kinsup + lstat(65)
  kksup = kjsup  + lstat(65)
  ndy5 = kud1 - 5
  ndx1 = kxtcs + nuk
  xtcs(ndx1+ 3) =  deltat
  xtcs(ndx1+ 6) =  0.0
  xtcs(ndx1+ 7) =  1.0
  xtcs(ndx1+ 8) = -1.0
  xtcs(ndx1+ 9) =  1.0
  xtcs(ndx1+10) =  fltinf
  xtcs(ndx1+11) =  twopi / 2.0
  do i = niunrs, niu
     ndy5 = ndy5 + 5
     if (iuty(kiuty + i) .ne. 14) cycle
     xtcs(ndx1 + 4) = ud1(ndy5 + 3)
     xtcs(ndx1 + 5) = twopi * ud1(ndy5 + 3)
     exit
  end do
  !                                               $$$  steady - state  $$$
  d1tttt = t
  n1tttt = istep
  istep = 0
  t = 0.0d0
  !     ******  linkage for  'tacs emtp sources'  names   ********
  if (nstacs .eq. 0) go to 480
  do i = 1, nstacs
     if (vstacs(i) .eq. blank) go to 483
     do j = 1, ktab
        ndx1 = ilntab(klntab + j)
        if (vstacs(i) .eq. texvec(ndx1)) go to 490
     end do
     !484  continue
     kill = 124
     bus1 = vstacs(i)
     lstat(14) = i
     lstat(19) = 490
     write (unit = lunit(6), fmt = 166) vstacs(i), i
166  format ('0error.  emtp source =', a6, '= (type ',i2, ') is not a recognizable variable name in tacs.')
     go to 9000
490  vstacs(i) = char(j)
     cycle
483  vstacs(i) = char(0)
  end do
480 nuki = kisblk - 8
  nukr = krsblk - 4
  do i = 1, nuk
     nuki = nuki + 8
     nukr = nukr + 4
     if (isblk(nuki + 2) .eq. 1) cycle
     j = isblk(nuki + 3)
     k = isblk(nuki + 2)
     n1 = j + 8
     n2 = j - 1 + k * 6
     f2 = 2.0d0 / deltat
     f1 = 1.0d0
     ndx2 = kprsup + j + 2
     if (parsup(ndx2) .eq. 0.0d0) go to 4061
     rsblk(nukr+1) = parsup(ndx2 + 1) / parsup(ndx2)
4061 do ll = n1, n2, 6
        f1 = f1 * f2
        ndx1 = kprsup  + ll
        !        parsup(ndx1) = parsup(ndx1) * f1
        parsup(ndx1) = int(parsup(ndx1) * f1)
        parsup(ndx2) = parsup(ndx2) + parsup(ndx1)
        !        parsup(ndx1+1) = parsup(ndx1+1) * f1
        parsup(ndx1 + 1) = int(parsup(ndx1 + 1) * f1)
        parsup(ndx2 + 1) = parsup(ndx2 + 1) + parsup(ndx1 + 1)
        n3 = (ll - j) / 6
        dum9(n3) = 1.0d0
     end do
     f1 = 1.0d0
     k2 = j + 2
     k1 = j + 8
     i1 = k
4072 i1 = i1 - 1
     if (i1 .eq. 0) cycle
     f1 = -2.0d0 * f1
     k3 = k2
     k2 = k1
     k1 = k1 + 6
     ndx1 = kprsup  + k2
     if (i1 .eq. 1) go to 4091
     do ll = 2, i1
        dum9(ll) = dum9(ll) + dum9(ll - 1)
     end do
     do kj= k1, n2, 6
        mm = (kj - k3) / 6
        ndx3 = kprsup  + kj
        !        parsup(ndx1) = parsup(ndx1) + dum9(mm) * parsup(ndx3)
        parsup(ndx1) = int(parsup(ndx1) + dum9(mm) * parsup(ndx3))
        !4088    parsup(ndx1+1) = parsup(ndx1+1) + dum9(mm) * parsup(ndx3+1)
        !        parsup(ndx1 + 1) = parsup(ndx1 + 1) + dum9(mm) * parsup(ndx3 + 1)
        parsup(ndx1 + 1) = int(parsup(ndx1 + 1) + dum9(mm) * parsup(ndx3 + 1))
     end do
4091 ndx2 = kprsup + k3
     !     parsup(ndx1) = parsup(ndx2) + f1 * parsup(ndx1)
     parsup(ndx1) = int(parsup(ndx2) + f1 * parsup(ndx1))
     !     parsup(ndx1+1) = parsup(ndx2+1) + f1 * parsup(ndx1+1)
     parsup(ndx1 + 1) = int(parsup(ndx2 + 1) + f1 * parsup(ndx1 + 1))
     go to 4072
  end do
  !     ******  t a c s   s o u r c e s   90  and  91  ******
  if ( iprsup  .ge.  1 ) write (lunit(6), 6523)  ( vstacs(i), i=1, nstacs )
6523 format ( /,  40h (vstacs(i), i=1, nstacs)  in  'tacs2' .   ,/, ( 1x, 10e13.4 ) )
  idctcs = 0
  iac = 0
  if ( niu .lt. niunrs )  go to 405
  if ( iprsup  .ge.  2 ) write (lunit(6), 3678)  ntotac, (ismtac(n6), n6=1, ntotac)
3678 format (/, ' ntotac =', i4, ' .   ( ismtac( j ),  j = 1, ntotac )  follows .', /, (12(1x, i5)))
  ndy5 = kud1
  do i = niunrs, niu
     ndx1 = ilntab( kaliu + i )
     bus1 = texvec( ndx1 )
     n1 = iuty( kiuty + i )
     if ( n1  .ne. 90 )  go to 475
     do j=2,ntot
        if ( bus1  .eq. bus(j) )  go to 479
     end do
     !476  continue
     kill = 125
     lstat(19) = 475
     write(lunit(6),168) bus1
168  format(25h0error.  type 90 source =,a6,42h= is not a recognizable node name in emtp.)
     go to 9000
475  if ( n1 .ne. 91  .and.  n1 .ne. 93 )  go to 54474
     do j=1,kswtch
        k = iabs(kmswit(j))
        ndx2 = lswtch + j
        m = iabs( kmswit(ndx2))
        if ( bus1  .eq.  bus(k) )   go to 479
        if ( bus1  .eq.  bus(m) )   go to 479
     end do
     !478  continue
     kill = 126
     if ( n1 .eq. 93 ) kill = 144
     lstat(19) = 479
     go to 9000
54474 go to 474
479  ud1( ndy5+2 ) = j
474  ndy5 = ndy5 + 5
  end do
  !     000  check for active sources in steady-state  0000000000000000000
  ndy5 = kud1
  do i = niunrs, niu
     ndx1 = kiuty  +  i
     j = iuty(ndx1)
     if ( ud1(ndy5+4) .ge. 0.0 )  go to 407
     if ( j.eq.11  .or.  j.eq.92  .or.  j.eq.93 ) idctcs = 1
     if (j .eq. 14 )  iac = 1
     if ( j .ne. 90  .and.  j .ne. 91 ) go to 407
     if ( ud1(ndy5+1)  .eq.  0.0 )  iac = 1
     if ( ud1(ndy5+1)  .eq.  1.0 )  idctcs = 1
407  ndy5 = ndy5 + 5
  end do
  if ( iac .eq. 1 )  go to 3000
405 if ( iac .eq. 0  .or.  nuk .eq. 0 ) go to 414
  nuki = kisblk
  do i = 1, nuk
     if ( isblk(nuki+2)  .eq.  1 )  go to 3738
     n1 = kprsup + isblk( nuki + 3 )
     if ( parsup(n1+1)  .ne.  0.0 )  go to 3738
     j = isblk( nuki + 4 )
     xtcs(kxtcs+j) = xtcs(kxtcs+j) - xar(kxar+j)
3738 nuki = nuki + 8
  end do
414 if ( idctcs .eq. 1 )  go to 256
  !     000  form coefficients to s-blocks for transients  000000000000000
406 if ( nuk .eq. 0 )  go to 271
  nuki = kisblk - 8
  do i = 1, nuk
     nuki = nuki + 8
     if (isblk(nuki + 2) .eq. 1) cycle
     j1 = kprsup + isblk(nuki + 3)
     k = isblk(nuki + 2) - 1
     j2 = j1 + k * 6
     do j = j1, j2, 6
        parsup(j) = parsup( j + 2 )
        !5454    parsup(j+1) = parsup( j + 3 )
        parsup(j + 1) = parsup(j + 3)
     end do
  end do
  if (idctcs .eq. 0 .and. kxic .eq. 0 .and. iac .eq. 0) go to 271
  !     000  initialize history on s-blocks for dc steady-state  000000000
  idctcs = 0
  nuki = kisblk - 8
  nukr = krsblk - 4
  do i = 1, nuk
     nuki = nuki + 8
     nukr = nukr + 4
     if (isblk(nuki + 2) .eq. 1) cycle
     j = isblk(nuki + 4)
     prx = xtcs(kxtcs + j)
     pru = rsblk(nukr + 1) * prx
     if (rsblk(nukr + 1) .ne. 999999.99d0) go to 245
     prx = 0.0d0
     pru = 0.0d0
     j = iabs(isblk(nuki + 1))
     k = iabs(isblk(nuki + 9)) - 1
     if (i .eq. nuk) k = nsu
     do m = j, k
        ndx1 = kalksu + m
        ndx2 = kxtcs + ksus(ndx1)
        ndx1 = kksus + m
        pru = pru + xtcs(ndx2) * ksus( ndx1)
     end do
     !239  continue
245  k = isblk(nuki + 2) - 1
     j = kprsup + isblk(nuki + 3) + k * 6
     !     parsup(j-2) = parsup(j) * pru - parsup(j + 1) * prx
     parsup(j - 2) = int(parsup(j) * pru - parsup(j + 1) * prx)
4510 if (k .eq. 1) go to 270
     k = k - 1
     j = j - 6
     !     parsup(j-2) = (parsup(j)*pru - parsup(j+1)*prx + parsup(j+4)) / 2
     parsup(j - 2) = int((parsup(j) * pru - parsup(j + 1) * prx + parsup(j + 4)) / 2)
     go to 4510
270  if (iprsup .lt. 2) cycle
     n1 = kprsup + isblk(nuki + 3)
     n2 = n1 + isblk(nuki + 2) * 6 - 6
     write (unit = lunit(6), fmt = 4412) i, (parsup(n), n = n1, n2, 6)
4412 format('   Function number', i8 ,/, '  pn    ', 8e15.6)
     write (unit = lunit(6), fmt = 4414) (parsup(n + 1), n = n1, n2, 6)
4414 format ('  pd    ', 8e15.6)
     n2 = n2 - 6
     write (unit = lunit(6), fmt = 4416) (parsup(n + 2), n = n1, n2, 6)
4416 format ('  hst   ', 8e15.6)
  end do
271 kxic = kivarb
  if (nsup .eq. 0) go to 2222
  do i = 1, nsup
     n1 = -insup(kjsup + i)
     if (n1 .lt. 0)  cycle
     n2 = insup(kksup + i)
     if (n2 .ne. 66) cycle
     nn = ivarb(n1)
     n = ivarb(n1 + 3)
     b = 0.0d0
     do j = 1, n
        j1 = nn + j
        parsup(j1) = parsup(j1) * parsup(j1)
        b = b + parsup(j1)
     end do
     n = kxtcs + nuk + i + lstat(64)
     xtcs(n) = sqrtz(b * parsup(nn))
  end do
2222 if (iprsup .lt. 1 .and. kssout .eq. 0) go to 2566
  write (unit = lunit(6), fmt = 4949)
4949 format (/, ' TACS DC steady-state solution follows ...', /, 5(3x, '-name-', 11x, 'value')  )
  mtot = ioutcs
  if ( kssout .eq. 1 )  mtot = ktab
  j1 = nuk + niu
  j2 = nuk + lstat(64)
  n14 = 0
  do i = 1, mtot
     j = jout( kjout + i )
     if (kssout .eq. 1) j = i
     if (j .gt. j1 .and. j .le. j2) cycle
     i1 = ilntab(klntab + j)
     n14 = n14 + 1
     texnam(n14) = texvec(i1)
     volti(n14) = xtcs(kxtcs + j)
     if (n14 .lt. 5) cycle
     write (unit = lunit(6), fmt = 4737) (texnam(ip), volti(ip), ip = 1, n14)
4737 format( 5( 3x, a6, e16.7 ) )
     n14 = 0
  end do
  if (n14 .gt. 0) write (unit = lunit(6), fmt = 4737) (texnam(ip), volti(ip), ip = 1, n14)
2566 if (lastov .gt. 1) go to 256
  n = 9
  if (ioutcs .lt. 9) n = ioutcs
  do i = 1, n
     k1 = jout(kjout+i)
     k1 = ilntab( klntab + k1 )
     dumj(i) = texvec(k1)
  end do
  write (unit = lunit(6), fmt = 234) (dumj(i), i = 1, n)
234 format (/, ' step', 5x, 'time', 3x, 9(6x, a6))
32201 if ( n .ge. ioutcs ) go to 233
  k = n + 1
  n = n + 9
  if ( n .gt. ioutcs )  n = ioutcs
  j = 0
  do i = k, n
     j = j + 1
     k1 = jout( kjout + i )
     k1 = ilntab( klntab + k1 )
     dumj(j) = texvec(k1)
  end do
  write (unit = lunit(6), fmt = 235) (dumj(i), i = 1, j)
235 format (/, 17x, 9(6x, a6))
  go to 32201
233 write (unit = lunit(6), fmt = 235)
  !                                         $$$  form  sparse  matrix  $$$
256 if (iac .eq. 1 .and. idctcs .eq. 0) go to 3042
  ia = 0
  nuki = kisblk - 8
  nukr = krsblk - 4
  do i = 1, nuk
     nuki = nuki + 8
     nukr = nukr + 4
     isblk(nuki+8) = ia + 1
     ndx1 = kawkcs + 1
     call move0 (awkcs(ndx1 :), ktab)
     nukj = kisblk - 8
     nuks = krsblk - 4
     do j = 1, nuk
        nukj = nukj + 8
        nuks = nuks + 4
        if (isblk(nukj + 4) .ne. i) cycle
        j1 = iabs(isblk(nukj + 1))
        k1 = iabs(isblk(nukj + 9)) - 1
        if (j .eq. nuk) k1 = nsu
        if (isblk(nukj + 2) .eq. 1) go to 218
        n = kprsup + isblk(nukj + 3)
        ndx1 = kawkcs + i
        dpd = parsup(n + 1)
        if (dpd .ne. 0.0d0 .or. idctcs .eq. 0)  go to 2911
        ndx1 = krowcs + i
        isblk(nuki + 8) = 0
        cycle
2911    awkcs(ndx1) = dpd
        do m = j1, k1
           l = ksus(kalksu + m)
           ndx1 = kawkcs + l
           ndx3 = kksus + m
           !219        awkcs(ndx1) = -parsup(n) * ksus(ndx3)
           awkcs(ndx1) = -parsup(n) * ksus(ndx3)
        end do
        go to 221
218     ndx1 = kawkcs + i
        awkcs(ndx1) = 1.0d0
        do m = j1, k1
           l = ksus(kalksu + m)
           ndx1 = kawkcs + l
           ndx3 = kksus  + m
           awkcs(ndx1) = -rsblk(nuks+1) * ksus(ndx3)
        end do
        go to 221
     end do
221  if (iprsup .ge. 4) write (unit = lunit(6), fmt = 2021) i, (j, awkcs(kawkcs + j), j = 1, ktab)
2021 format ('0row ', i3, (1x, 6(i3, ': ', e13.6,2x)))
     !                                    $$$  triangularize  each  row  $$$
     d1 = awkcs(kawkcs + i)
     if (d1 .eq. 0.0d0) go to 7261
     if (i .eq.  1) go to 261
     m = i - 1
     do k = 1, m
        ndx1 = kawkcs + k
        if (awkcs(ndx1) .eq. 0.0d0) cycle
        n = k + 1
        ndx1 = kisblk + k * 8
        k1 = isblk(ndx1) - 1
        if (k1 .lt. 0) cycle
4545    k2 = isblk(ndx1 + 8)
        if (k2 .gt. 0)  go to 263
        ndx1 = ndx1 + 8
        go to 4545
263     k1 = k1 + 1
        if (k1 .eq. k2) cycle
        j = icolcs(kcolcs + k1)
        jlk = isblk(kisblk + j * 8)
        if (j .lt. n .and. jlk .ne. 0)  go to 263
        !264     ndx1 = kawkcs + j
        ndx1 = kawkcs + j
        ndx3 = kawkcs + k
        ndx4 = katcs  + k1
        awkcs(ndx1) = awkcs(ndx1) - awkcs(ndx3) * atcs(ndx4)
        go to 263
     end do
261  ndx1 = kawkcs + i
     d2 = awkcs(ndx1)
     if ( absz(d2/d1)   .gt.   epsiln )   go to 265
7261 kill = 128
     ndx1 = ilntab( klntab + i )
     bus1 = texvec(ndx1)
     lstat(14) = i
     flstat(14) = d1
     flstat(15) = d2
     lstat(19) = 7261
     write ( lunit(6), 267 ) bus1
267  format ( 95h0error.  your system is unstable.  you will find the error in a feedback loop involving block =, a6, 2h=.   )
     go to 9000
265  ndx3 = kawkcs + i
     awkcs(ndx3) = 1.0 / awkcs(ndx3)
     !                               $$$  append  to  compacted  vector  $$$
     do j = 1, ktab
        if (awkcs(kawkcs + j) .eq. 0.0d0) cycle
        ia = ia + 1
        if (ia .le. lstat(62)) go to 8122
        kill = 122
        lstat(16) = lstat(62)
        lstat(17) = 2
        lstat(19) = 8122
        go to 9000
8122    ndx1 = kcolcs + ia
        icolcs(ndx1) = j
        ndx1 = katcs  + ia
        ndx2 = kawkcs + j
        atcs(ndx1) = awkcs(ndx2)
        jlk = isblk( kisblk + j * 8 )
        if (j .le. i .and. jlk .ne. 0) cycle
        atcs(ndx1) = atcs(ndx1) * awkcs(ndx3)
     end do
  end do
  if (iprsup .le. 1) go to 250
  write (unit = lunit(6), fmt = 2023) ia, (isblk(kisblk + n * 8), n = 1, nuk)
2023 format ('0', /, ' ia=', i4, 8x, 'next is irowcs(1,2,...,nuk)/(1x,30i4)')
  write (unit = lunit(6), fmt = 2024) (icolcs(kcolcs + m), atcs(katcs + m), m = 1, ia)
2024 format ('0icolcs and atcs', /, (1x, 6(i3, e15.6,2x)))
  !                                       $$$  update  input  sources  $$$
250 ndx1 = kxtcs + nuk
  xtcs(ndx1 + 1) = t
  xtcs(ndx1 + 2) = istep
  if (idctcs .eq. 0 .and. t .eq. 0.0d0) go to 317
  !     ---  i.c. on devices  ---
  if (istep .ne. 1 .or. nsup .eq. 0) go to 25050
  do i = 1, nsup
     n1 = - insup(kjsup + i)
     if (n1 .lt. 0) go to 5051
     n2 = insup(kksup + i) - 49
     nn = ivarb(n1)
     j = nuk + lstat(64) + i
     ! go to (5051, 5051, 5051, 5051, 5051, 5051, 5051, 5051, 658 , 659, 5051, 5051, 662, 5051, 664, 664, 5051, 5051 ), n2
     select case (n2)
     case (1 : 8, 11 : 12, 14, 17 : 18)
        go to 5051

     case (9)
        go to 658

     case (10)
        go to 659

     case (13)
        go to 662

     case (15 : 16)
        go to 664
     end select
     !658  parsup(nn) = (parsup(nn+1) - parsup(nn+2)) /2.0 * xtcs(kxtcs+j)
658  parsup(nn) = int((parsup(nn + 1) - parsup(nn + 2)) / 2.0d0 * xtcs(kxtcs + j))
     go to  5051
659  j = ivarb(n1 + 1)
     k = ivarb(n1 + 2)
     b = 0.0d0
     do n = j, k
        m = kxtcs + ksus( kalksu + n )
        !5061    b = b + xtcs(m)  *  ksus(kksus+n)
        b = b + xtcs(m) * ksus(kksus + n)
     end do
     !     parsup(nn + 1) = b
     parsup(nn + 1) = int (b)
     go to 5051
662  parsup(nn + 2) = xtcs(kxtcs + j)
     go to  5051
664  parsup(nn) = xtcs(kxtcs + j)
5051 continue
  end do
25050 if (idctcs .eq. 1) go to 6513
  if (ntcsex + nstacs .le. 0) go to 6513
  t = d1tttt
  istep = n1tttt
  indstp = 1
  xtcs(ndx1 + 1) = 0.0d0
  xtcs(ndx1 + 2) = 0.0d0
  if (nsup .eq. 0 .or. numsm .eq. 0) go to 9000
  n33 = 0
  do i = 1, numsm
     i30 = 30 * i - 29
     jmset = ismdat( i30 + 15 )
     if ( jmset .le. 0 ) go to 909
     jmset = -insup( kinsup + lstat(65) + jmset - lstat(64) - nuk )
     jmset = ivarb( jmset )
     parsup(jmset) = cu( n33 + 11 )
909  n33 = n33 + 24
  end do
  do i = 1, nsup
     n1 = - insup( kjsup + i )
     if (n1 .le. 0) cycle
     n2 = insup( kksup + i )
     if (n2 .ne. 67) cycle
     nn = ivarb( n1 )
     if (parsup(nn) .ge. parsup(nn + 1)) go to 4499
4477 kill = 500
     write (unit = lunit(6), fmt = 4488)
4488 format (' ***** The crest of field winding voltage source of #59 machine is beyond its limit *****', /, ' stop at tacs2 of over12 near statement 4499')
     go to 9000
4499 if (parsup(nn) .gt. parsup(nn + 2)) go to 4477
     b = 0.0
     j = ivarb( n1 + 1 )
     k = ivarb( n1 + 2 )
     do mj = j, k
        n = kksus + mj
        if (ksus(n) .eq. 9) cycle
        m = kalksu + mj
        bb = xtcs(kxtcs + ksus(m)) * ksus(n)
        nj = mj
1144    nj = nj - 1
        n = n - 1
        m = m - 1
        if (nj .lt. j) go to 1155
        if (ksus(n) .ne. 9) go to 1155
        bb = bb * xtcs( kxtcs + ksus(m) )
        go to 1144
1155    b = b + bb
     end do
     parsup(nn+3) = 1.0 / b
     ndx1 = kxtcs + nuk + lstat(64) + i
     xtcs(ndx1) = 1.0
  end do
  go to 9000
6513 continue
  if (niu .lt. niunrs) go to 505
  if (ltdelt .ne. -6789) go to 1768
  !     postprocessing usage defines tacs sources from old
  !     plot file that is connected to unit "lunit2" .
  do i = 1, iofbnd
     if (m4plot .eq. 0) go to 1761
     call pltlu2 (d1, volti(1))
     go to 1762
1761 read (unit = lunit(2)) d1, (volti(j), j = 1, iofgnd)
1762 if (d1 .eq. -9999.) go to 900
  end do
  !1763 continue
  t = d1
  ndx1 = kxtcs + nuk
  xtcs(ndx1+1) = t
  xtcs(ndx1+3) = t - epsuba
  epsuba = t
  ndx1 = ndx1 + niunrs
  call move (volti(1 :), xtcs(ndx1 :), iofgnd)
  go to 505
1768 ndy5 = kud1 - 5
  do i = niunrs, niu
     ndy5 = ndy5 + 5
     j = i + nuk
510  ndxi = kxtcs + j
     xtcs(ndxi) = 0.0d0
     ndx1 = kiuty  +  i
     n1 = iuty(ndx1)
     if (t .lt. ud1(ndy5 + 4) - flzero * 10.0d0) cycle
     if (t .ge. ud1(ndy5 + 5) - flzero * 10.0d0) cycle
     if (t .eq. 0.0d0 .and. ud1(ndy5 + 4) .ge. 0.0d0) cycle
     if (iac .eq. 1 .and. n1 .eq. 14) cycle
     if (n1 .ge. 90 .and. t .gt. 0.0d0) cycle
     if (n1 .lt. 90) go to 501
     k = int (ud1(ndy5 + 2))
     if (n1 .gt. 93) cycle
     n2 = n1 - 89
     ! go to ( 502, 508, 504, 506), n2
     select case (n2)
     case (1)
        go to 502

     case (2)
        go to 508

     case (3)
        go to 504

     case (4)
        go to 506
     end select
502  if (ud1(ndy5 + 1) .eq. 1.0d0) xtcs(ndxi) = e(k)
     cycle
504  xtcs(ndxi) = etac( k)
     cycle
506  if (nextsw(k) .eq. 87) xtcs(ndxi) = 1.0d0
     cycle
508  if (nextsw(k) .eq. 87 .and. ud1(ndy5 + 1) .eq. 1.0d0) xtcs(ndxi) = tclose(k)
     cycle
501  xtcs(ndxi) = ud1(ndy5 + 1)
     if (n1 .eq. 11) cycle
     if (n1 .ne. 14) go to 511
     xtcs(ndxi) = xtcs(ndxi) * cosz(twopi * ud1(ndy5 + 3) * t + ud1(ndy5 + 2))
511  if (t .eq. 0.0d0) cycle
     if (n1 .ne. 23) go to 512
     if (t .lt. ud1(ndy5+4) +ud1(ndy5+2) -flzero*10.) cycle
     ud1(ndy5 + 4) = ud1(ndy5 + 4) + ud1(ndy5 + 3)
     go to 510
512  if (n1 .ne. 24) cycle
     if (t .lt. ud1(ndy5 + 4) + ud1(ndy5 + 3) - flzero * 10.0d0) go to 513
     ud1(ndy5 + 4) = ud1(ndy5 + 4) + ud1(ndy5 + 3)
     go to 512
513  xtcs(ndxi) = xtcs(ndxi) * (t - ud1(ndy5 + 4)) / ud1(ndy5 + 3)
     if (absz(xtcs(ndxi)) .le. 10.0d0 * flzero) xtcs(ndxi) = 0.0d0
  end do
505 l = iuty(kiuty + 4)
  if (t .gt. 0.0d0) go to 5555
  !555 if ( l  .gt.  0 ) call csupdc(l)
  if (l .gt. 0) call csupdc(l)
  ipass = 0
  ite = ite + 1
  if (ite .lt. 100) go to 327
  write (unit = lunit(6), fmt = 7798) ite
7798 format (' ---- Iteration is beyond ', i4, ' times, but the result is not convergent yet, program will continue  ----')
  go to 3177
5555 if (l .gt. 0) call csup(l)
  !                                                 $$$  form  rhside  $$$
  nukr = krsblk - 4
  do i = 1, nuk
     nukr = nukr + 4
     !2324 rsblk(nukr+4) = 0.0
     rsblk(nukr + 4) = 0.0
  end do
  nuki = kisblk - 8
  do i = 1, nuk
     nuki = nuki + 8
     if (isblk(nuki + 2) .eq. 1) cycle
     j = krsblk + isblk(nuki + 4) * 4
     k = kprsup + isblk(nuki + 3) + 4
     rsblk(j) = rsblk(j) + parsup(k)
  end do
  !                                          $$$  forward  on  rhside  $$$
  nuki = kisblk - 8
  nukr = krsblk - 4
  do i = 1, nuk
     nuki = nuki + 8
     nukr = nukr + 4
     m = isblk(nuki+8)
301  ndx1 = kcolcs + m
     k = icolcs(ndx1)
     if (k .ge. i) go to 306
     ndx3 = katcs  + m
     ndx4 = krsblk + k * 4
     rsblk(nukr+4) = rsblk(nukr+4) - atcs(ndx3) * rsblk(ndx4)
     m = m + 1
     go to 301
306  ndx3 = katcs  + m
     !305  rsblk(nukr+4) = rsblk(nukr+4) * atcs(ndx3)
     rsblk(nukr + 4) = rsblk(nukr + 4) * atcs(ndx3)
  end do
  !                                           $$$  backward  on  xtcs  $$$
327 mm = 1
  nuki = kisblk - 8
  nukr = krsblk - 4
  do i = 1, nuk
     nuki = nuki + 8
     nukr = nukr + 4
     j = nuk + 1 - i
     nukj = kisblk + j * 8 - 8
     nuks = krsblk + j * 4 - 4
     m = isblk( nukj + 8 )
     ndx1 = kxtcs + j
     xtcsav = xtcs(ndx1)
     if (m .eq. 0) go to 308
     n = ia
     xtcs(ndx1) = rsblk(nuks + 4)
     if (j .eq. nuk) go to 309
     nkk = isblk(nukj + 16)
     nkk1 = 1
     if (nkk .gt. 0) go to 3091
2850 if (j .eq. nuk - nkk1) go to 309
     nkk = isblk(nukj + nkk1 * 8 + 16)
     if (nkk .gt. 0) go to 3091
     nkk1 = nkk1 + 1
     go to 2850
3091 n = nkk - 1
309  if (m .gt. n) go to 324
     k = icolcs(kcolcs + m)
     ilk = isblk(kisblk + k * 8)
     if (k .le. j .and. ilk .ne. 0) go to 334
     ndx3 = katcs + m
     ndx4 = kxtcs + k
     xtcs(ndx1) = xtcs(ndx1) - atcs(ndx3) * xtcs(ndx4)
334  m = m + 1
     go to 309
324  if (iac .eq. 1) go to 308
     j1 = isblk(nukj + 5)
     k1 = isblk(nukj + 6)
     ndx2 = kxtcs  + j1
     if (j1 .gt. 0) rsblk(nuks + 2) = xtcs(ndx2)
     ndx2 = kxtcs + k1
     if (k1 .gt. 0) rsblk(nuks + 3) = xtcs(ndx2)
     if (t .gt. 0.0d0) go to 337
     if(xtcs(ndx1) .ge. rsblk(nuks + 2) .and. xtcs(ndx1) .le. rsblk(nuks + 3)) go to 308
     if (iuty(kiuty + 3) .eq. 0) go to 337
     iuty(kiuty + 3) = iuty(kiuty + 3) - 1
     ndx2 = ilntab(klntab + j)
     write (unit = lunit(6), fmt = 3041) texvec(ndx2)
3041 format ('0warning.  Block ', "'", a6, "'", 'has its limiter operating during the TACS DC steady-state solution.', /, 11x, 'double-check the program output for  t = 0.0  for any misunderstanding between the program and the user.', /, 1x)
337  if (xtcs(ndx1) .lt. rsblk(nuks + 2)) xtcs(ndx1) = rsblk(nuks + 2)
     if (xtcs(ndx1) .gt. rsblk(nuks + 3)) xtcs(ndx1) = rsblk(nuks + 3)
     if (rsblk(nuks + 2) .le. rsblk(nuks + 3)) go to 308
     if (iuty(kiuty + 3) .eq. 0) go to 308
     ndx2 = ilntab(klntab + j)
     write (unit = lunit(6), fmt = 198) texvec(ndx2)
198  format ('0warning. limits at block =', a6, '= have criss-crossed. Expect puzzling results.',/, " this warning message will not be repeated.  Refer to TACS user's manual.")
     iuty(kiuty + 3) = iuty(kiuty + 3) - 1
308  l = isblk(nukj + 7)
     if (t .gt. 0.0d0) go to 3077
     if (l .gt. 0)  call csupdc(l)
     defr = absz (xtcsav - xtcs(ndx1))
     critia = absz (xtcs(ndx1)) * 0.001d0
     if (critia .lt. epslon) critia = epslon
     if (defr .gt. critia) ipass = 1
     cycle
3077 if (l .gt. 0) call csup(l)
  end do
  if (ipass .eq. 1 .and. iuty(kiuty + 9) .eq. 1) go to 505
  !                                                       $$$  output  $$$
3177 if (t .eq. 0.0d0 .and. iac .eq. 1) go to 4066
317 do i = 1, nsup
     n1 = -insup(kjsup + i)
     if (n1 .lt. 0) cycle
     if (insup(kksup + i) .ne. 53) cycle
     j = ivarb(n1 + 1)
     k = ivarb(n1 + 2)
     b = 0.0d0
     do n = j, k
        m = ksus(kalksu + n)
        !601     b = b + xtcs(kxtcs+m) * ksus(kksus+n)
        b = b + xtcs(kxtcs + m) * ksus(kksus + n)
     end do
     nn = ivarb(n1)
     !     n5 = parsup(nn)
     n5 = int (parsup(nn))
     !     n6 = parsup(nn + 2)
     n6 = int (parsup(nn + 2))
     n7 = ivarb(n1 + 4)
     ndx6 = kprsup + n7
     parsup(ndx6) = b
     n7 = n7 + 1
     if (n7 .eq. n5 + n6) n7 = n5
     ivarb(n1 + 4) = n7
  end do
  if (ioutcs .eq. 0) go to 310
  if (t .gt. deltat) go to 335
  if (t .gt. 0.0) go to 3335
  if (begmax(1) .eq. 1.0d0) go to 316
  if (isprin .eq. 0) go to 316
  if (isplot .gt. 0) go to 310
316 do i = 1, ioutcs
     j = jout(kjout + i)
     ndx1 = kbtcs + i
     !336  atcs(ndx1) = xtcs( kxtcs + j )
     atcs(ndx1) = xtcs(kxtcs + j)
  end do
  go to 3318
3335 do i = 1, ioutcs
     ndx1 = kbtcs + i
     ndx2 = kxar  + i
     ndx3 = kxai  + i
     ndx4 = kawkcs + i
     ndx5 = kbwkcs + i
     xar(ndx2) = atcs(ndx1)
     xar(ndx3) = atcs(ndx1)
     awkcs(ndx4) = 0.0
     awkcs(ndx5) = 0.0
  end do
335 do i = 1, ioutcs
     j = jout(kjout + i)
     ndx1 = kbtcs + i
     ndx2 = kxar + i
     ndx3 = kxai + i
     atcs(ndx1) = xtcs(kxtcs + j)
     if (atcs(ndx1) .ge. xar(ndx3)) go to 319
     xar(ndx3) = atcs(ndx1)
     ndx4 = kbwkcs + i
     go to 3333
319  if (atcs(ndx1) .le. xar(ndx2)) cycle
     xar(ndx2) = atcs(ndx1)
     ndx4 = kawkcs + i
3333 awkcs(ndx4) = t
  end do
3318 if (istep .lt. limstp) go to 4321
  isprin = 0
  iout = multpr(indstp)
  indstp = indstp + 1
  limstp = kprchg(indstp)
4321 if (isprin .gt. 0) go to 320
  if (lastov .gt. 1) go to 320
  isprin = iout
  ndx1 = kbtcs + 1
  ndx2 = kbtcs + 9
  if (ioutcs .lt. 9) ndx2 = kbtcs + ioutcs
  write (unit = lunit(6), fmt = 321) istep, t, (atcs(i), i = ndx1, ndx2)
321 format (1x, i5, e13.5, 9e12.5)
  ndx1 = kbtcs + ioutcs
32819 if (ndx2 .ge. ndx1) go to 320
  ndx3 = ndx2 + 1
  ndx2 = ndx2 + 9
  if (ndx2 .gt. ndx1) ndx2 = ndx1
  write (unit = lunit(6), fmt = 322) (atcs(i), i = ndx3, ndx2)
322 format (19x, 9e12.5)
  go to 32819
320 if (isplot .gt. 0) go to 310
  if (ntot .ge. 2) go to 310
  ndx1 = kbtcs + 1
  if (m4plot .eq. 0) go to 1648
  volti(1) = t
  call move (atcs(ndx1 :), volti(2 :), ioutcs)
  ndx2 = ioutcs + 1
  call pltfil ( ndx2 )
  go to 1649
1648 ndx2 = kbtcs + ioutcs
  write (unit = lunit(4)) t, (atcs(i), i = ndx1, ndx2)
1649 isplot = iplot
  !                                                $$$  time  control  $$$
310 if (t .gt. tmax) go to 900
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 8461) istep, isprin, isplot, iac, idctcs
8461 format (/, " In  'tacs2' ,   increment  'istep' .   istep  isprin  isplot     iac  idctcs", /, 3x, 7i8)
  istep = istep + 1
  t = istep * deltat
  if (t .lt. sptacs(29)) go to 1945
  sptacs(29) = fltinf
  iuty(kiuty + 3) = iuty(kiuty + 2)
1945 isprin = isprin - 1
  isplot = isplot - 1
  if (iac .eq. 1) go to 350
  if (idctcs .eq. 0) go to 328
4066 if (nsup .eq. 0) go to 406
  do i = 1, nsup
     n1 = -insup(kjsup + i)
     if (n1 .lt. 0) cycle
     mjm = insup(kksup + i)
     if (mjm .ne. 53 .and. mjm .ne. 66) cycle
     j = ivarb(n1 + 1)
     k = ivarb(n1 + 2)
     b = 0.0d0
     do n = j, k
        m = ksus(kalksu + n)
        n2 = ksus(kksus + n)
        b = b + xtcs(kxtcs + m) * n2
     end do
     nn = ivarb(n1)
     !     m = parsup(nn)
     m = int (parsup(nn))
     !     n = parsup( nn + 2 )
     n = int (parsup(nn + 2))
     if ( mjm  .eq.  53 )  go to 4058
     n = ivarb( n1 + 3 )
4058 do j = 1, n
        ndx1 = kprsup + m + n - j
        if ( mjm .eq. 66 ) ndx1 = nn + n - j + 1
        !3039    parsup(ndx1) = parsup(ndx1) + b
        parsup(ndx1) = parsup(ndx1) + b
     end do
  end do
  go to 406
350 iac = 0
  go to 256
  !                                                  $$$  update  hst  $$$
328 nuki = kisblk - 8
  nukr = krsblk - 4
  do i = 1, nuk
     nuki = nuki + 8
     nukr = nukr + 4
     if (isblk(nuki + 2) .eq. 1) cycle
     l = isblk(nuki + 4)
     prx = xtcs(kxtcs + l)
     j = iabs (isblk(nuki + 1))
     k = iabs (isblk(nuki + 9)) - 1
     if (i .eq. nuk) k = nsu
     if (isblk(nuki + 1) .gt. 0) go to 2345
     do i1 = ma1, ma2
        if (ivarb(i1) .ne. -i) cycle
        ma3 = i1 + 1
        do i2 = ma3, ma2
           if (ivarb(i2) .lt. 0) go to 2345
           ma4 = i2
        end do
        go to 2345
     end do
2345 pru = 0.0d0
     do m = j, k
        n = ksus(kalksu + m)
        if (isblk(nuki + 1) .gt. 0) go to 5678
        n1 = n - lstat(64) - nuk
        do i3 = ma3, ma4
           if (ivarb(i3) .ne. n1) cycle
           n = n + lstat(68)
           go to 5678
        end do
5678    ndx1 = kxtcs  + n
        ndx2 = kksus  + m
        pru = pru + xtcs(ndx1) * ksus(ndx2)
     end do
     !312  continue
     jcm = kprsup + isblk( nuki + 3 )
     k = isblk(nuki+2) - 1
     j = jcm + k * 6 - 2
     ndx1 = krsblk + l * 4 - 2
     ndx2 = ndx1 + 1
     if ( prx .eq. rsblk( ndx1)  .or.  prx .eq. rsblk( ndx2) ) go to 2810
     if ( k .eq. 1 )  go to 330
     n22 = jcm + 6
     do n = n22, j, 6
        parsup(n - 2) = parsup(n) * pru - parsup(n + 1) * prx - parsup(n - 2) + parsup(n + 4)
     end do
330  parsup(j) = parsup(j + 2) * pru - parsup(j + 3) * prx
     go to 311
     !                               $$$  dynamic  limiter  hst  $$$
2810 n = j
     parsup(j) = parsup(j + 2) * pru - parsup(j + 3) * prx
2840 if (k .eq. 1) go to 311
     k = k - 1
     n = n - 6
     parsup(n) = (parsup(n + 2) * pru - parsup(n + 3) * prx + parsup(n + 6)) / 2.0d0
     go to 2840
311  if (iprsup .ge. 4)  write (unit = lunit(6), fmt = 2020) i, (parsup(n + 4), n = jcm, j, 6)
2020 format ('  Function ', i5, '  hst ', 7e14.6)
  end do
  ndx1 = kxtcs + nuk + lstat(64) + 1
  ndx2 = ndx1 + lstat(68)
  call move (xtcs(ndx1 :), xtcs(ndx2 :), nsup)
  go to 250
  !                                                  $$$  termination  $$$
900 if ( iprsup  .ge.  1 ) write(lunit(6),901)
901 format ("0normal termination within  'tacs2' .")
  if (istep .eq. 1) go to 903
  if (ioutcs .eq. 0) go to 903
  if (begmax(1) .le. 0.0d0) go to 903
  if (noutpr .ne. 0) go to 903
  if (begmax(1) .eq. 0.0d0) write (unit = lunit(6), fmt = 904)
904 format (/, ' Minima and maxima of all TACS output variables')
  if (begmax(1) .eq. 1.0d0) write (unit = lunit(6), fmt = 42905)
42905 format (/, " Minima and maxima of all TACS output variables  (extrema over all solution points, since  'maxout'  is 1)")
  if (begmax(1) .eq. 2.0d0) write (unit = lunit(6), fmt = 52905)
52905 format (/, ' Minima and maxima of all TACS output variables  (extrema over just printed or plotted points, since "maxout"  is 2)')
  write (unit = lunit(6), fmt = 62905)
62905 format (7x, 'row', 6x, 'name', 8x, 'maximum', 8x, 'minimum', 9x, 'time of max', 4x, 'time of min')
  do i = 1, ioutcs
     ndx1 = jout(kjout + i)
     ndx1 = ilntab(klntab + ndx1)
     ndx2 = kxar + i
     ndx3 = kxai + i
     ndx4 = kawkcs + i
     ndx5 = kbwkcs + i
     !905  write(lunit(6),906) i, texvec(ndx1),xar(ndx2),xar(ndx3),awkcs(ndx4),awkcs(ndx5)
     write(unit = lunit(6), fmt = 906) i, texvec(ndx1), xar(ndx2), xar(ndx3), awkcs(ndx4), awkcs(ndx5)
  end do
906 format (i10, 4x, a6, 2e15.6, 5x, 2f15.6)
903 lastov = nchain
  nchain = 20
  lstat(32) = ioutcs
  nenerg = 0
  kbase = 0
  begmax(1) = 0.0d0
  ipunch = 0
  go to 9000
  !                                           $$$  ac  steady - state  $$$
  !     ***  form  pac  ***
3000 mdy5 = kud1 - 5
  do mjm = niunrs, niu
     mdy5 = mdy5 + 5
     if (ud1(mdy5 + 4) .ge. 0.0d0) go to 8888
     if (ud1(mdy5 + 3) .le. 0.0d0) go to 8888
     mrm = iuty( kiuty + mjm )
     if (mrm .ne. 14 .and. mrm .ne. 90 .and. mrm .ne. 91) go to 8888
     freqhz = ud1( mdy5 + 3 )
     omegar = freqhz * twopi
     ndx1 = kxar   + 1
     ndx2 = lstat(68) + lstat(68)
     call move0 (xar(ndx1 :), ndx2)
     nuki = kisblk - 8
     nukr = krsblk - 4
     do i = 1, nuk
        nuki = nuki + 8
        nukr = nukr + 4
        if (isblk(nuki + 2) .eq. 1) cycle
        j = kprsup + isblk(nuki + 3)
        m = isblk(nuki + 2) * 6 + j - 4
        ndx1 = kxar + i
        xar(ndx1) = parsup(j)
        ndx3 = kxai + i
        xar(ndx3) = 0.0d0
        ndx2 = ndx1 + nuk
        xar(ndx2) = parsup(j + 1)
        ndx4 = ndx3 + nuk
        xar(ndx4) = 0.0d0
        n1 = 0
        d1 = 1.0d0
        n22 = j + 6
        do k = n22, m, 6
           d1 = d1 * omegar
           if (n1 .eq. 0) go to 13000
           n1 = 0
           d1 = -d1
           xar(ndx1) = xar(ndx1) + parsup(k) * d1
           xar(ndx2) = xar(ndx2) + parsup(k+1) * d1
           cycle
13000      n1 = 1
           xar(ndx3) = xar(ndx3) + parsup(k) * d1
           xar(ndx4) = xar(ndx4) + parsup(k+1) * d1
        end do
     end do
     if (iprsup .le. 1) go to 3004
     write (unit = lunit(6), fmt = 1372) nuk, (i, isblk(nuki + 3), isblk(nuki + 2), xar(kxar + i), xar(kxai + i), xar(kxar + nuk + i), xar(kxai + nuk + i), i = 1, nuk)
1372 format (/, " 'tacs2' tables before ac steady-state.  nuk =", i4, /, 1x, '     row    kfst     kni', 10x, 'xar-1', 10x, 'xai-1', 10x, 'xar-2', 10x, 'xai-2', /, (1x, 3i8, 4e15.6))
     !     ***  form  sparse  matrix  ***
3004 ia = 0
     nuki = kisblk - 8
     do i = 1, nuk
        nuki = nuki + 8
        isblk(nuki+8) = ia + 1
        ndx1 = kawkcs + 1
        call move0 (awkcs(ndx1 :), ktab)
        ndx1 = kbwkcs + 1
        call move0 (awkcs(ndx1 :), ktab)
        nukj = kisblk - 8
        nuks = krsblk - 4
        do j = 1, nuk
           nukj = nukj + 8
           nuks = nuks + 4
           k1 = isblk( nukj + 4 )
           if (k1 .ne. i) cycle
           j1 = iabs (isblk(nukj + 1))
           k1 = iabs (isblk(nukj + 9)) - 1
           if (j .eq. nuk) k1 = nsu
           if (isblk(nukj + 2) .eq. 1) go to 3218
           ndx1 = kawkcs + i
           ndx2 = kxar + nuk +  j
           awkcs(ndx1) = xar(ndx2)
           ndx1 = kbwkcs + i
           ndx2 = kxai + nuk + j
           awkcs(ndx1) = xar(ndx2)
           ndy1 = kxar + j
           ndy2 = kxai + j
           do m = j1, k1
              l = ksus( kalksu + m )
              d1 = ksus( kksus + m )
              ndx1 = kawkcs + l
              awkcs(ndx1) = -xar(ndy1)*d1
              ndx1 = kbwkcs + l
              awkcs(ndx1) = -xar(ndy2)*d1
           end do
           go to 3221
3218       ndx1 = kawkcs + i
           awkcs(ndx1) = 1.0
           ndx1 = kbwkcs + i
           awkcs(ndx1) = 0.0
           d1 = rsblk( nuks + 1 )
           do m=j1, k1
              l = ksus( kalksu + m )
              ndx1 = kawkcs + l
              ndx2 = kksus  + m
              awkcs(ndx1) = -d1 * ksus(ndx2)
              ndx1 = kbwkcs + l
              awkcs(ndx1) = 0.0
           end do
           go to 3221
        end do
3221    if (iprsup .ge. 2) write (unit = lunit(6), fmt = 43219) i, j, ia, ktab, nuk, j1, k1
43219   format (/, ' at 43219       i       j      ia    ktab       nuk      j1      k1', /, 9x, 7i8)
        if (iprsup .ge. 2) write (unit = lunit(6), fmt = 2022) i, (j, awkcs(kawkcs + j), awkcs(kbwkcs + j), j = 1, ktab)
2022    format ('0row ', i3, (1x, 3(i3, ': ', e13.6, '  + j ', e13.6)))
        !     ***  triangularize each row  ***
        !3224    ndx1 = kawkcs + i
        ndx1 = kawkcs + i
        ndx2 = kbwkcs + i
        d4 = awkcs(ndx1) ** 2 + awkcs(ndx2) ** 2
        if (i .eq. 1) go to 3261
        m = i - 1
        do k = 1, m
           ndx1 = kawkcs + k
           if (awkcs(ndx1) .ne. 0.0d0) go to 13263
           ndx1 = kbwkcs + k
           if (awkcs(ndx1) .eq. 0.0d0) cycle
13263      n = k + 1
           ndx1 = kisblk + k * 8
           k1 = isblk(ndx1) - 1
           k2 = isblk(ndx1+8)
3263       k1 = k1 + 1
           if (k1 .eq. k2) cycle
           ndx1 = kcolcs + k1
           if (icolcs(ndx1) .lt. n) go to 3263
           ndx1 = kcolcs + k1
           j = icolcs(ndx1)
           ndx1 = kawkcs + k
           d1 = awkcs(ndx1)
           ndx1 = kbwkcs + k
           d2 = awkcs(ndx1)
3264       ndx1 = kawkcs + j
           ndx3 = katcs + k1
           ndx4 = kbtcs + k1
           awkcs(ndx1) = awkcs(ndx2) - (d1 * atcs(ndx3) - d2 * atcs(ndx4))
           ndx1 = kbwkcs + j
           awkcs(ndx1)  =  awkcs(ndx1)  -  ( d1*atcs(ndx3) + d2*atcs(ndx4) )
           k1 = k1 + 1
           if (k1 .eq. k2) cycle
           ndx1 = kcolcs + k1
           j = icolcs(ndx1)
           go to 3264
        end do
3261    ndx1 = kawkcs + i
        ndx2 = kbwkcs + i
        d2 = awkcs(ndx1) ** 2 + awkcs(ndx2) ** 2
        if (d4 .eq. 0.0) go to 13261
        if (d2 / d4 .gt. tolmat) go to 3265
13261   kill = 129
        ndx1 = ilntab( klntab + i )
        bus1 = texvec(ndx1)
        lstat(14) = i
        flstat(14) = d4
        flstat(15) = d2
        lstat(19) = 3265
        write (lunit(6), 3267)  bus1
3267    format (111h0error.  your system is unstable in steady-state.  you will find the error in a feedback loop involving block =,a6,2h=.)
        go to 9000
3265    ndx1 = kawkcs + i
        d3 =  awkcs(ndx1) / d2
        ndx1 = kbwkcs + i
        d4 = -awkcs(ndx1) / d2
        ndx1 = kawkcs + i
        awkcs(ndx1) = d3
        ndx1 = kbwkcs + i
        awkcs(ndx1) = d4
        !     ***  append to compacted vector  ***
        do j = 1, ktab
           ndx1 = kawkcs + j
           if (awkcs(ndx1) .ne. 0.0d0) go to 13265
           ndx1 = kbwkcs + j
           if (awkcs(ndx1) .eq. 0.0d0) cycle
13265      ia = ia + 1
           if (ia .le. lstat(62)) go to 8127
           kill = 122
           lstat(16) = lstat(62)
           lstat(17) = 2
           lstat(19) = 8127
           go to 9000
8127       ndx1 = kcolcs + ia
           icolcs(ndx1) = j
           ndx1 = katcs  + ia
           ndx2 = kawkcs + j
           atcs(ndx1) = awkcs(ndx2)
           ndx1 = kbtcs  + ia
           ndx2 = kbwkcs + j
           atcs(ndx1) = awkcs(ndx2)
           if (j .le. i) cycle
           ndx1 = katcs  + ia
           ndx2 = kbtcs  + ia
           d1 = atcs(ndx1) * d3 - atcs(ndx2) * d4
           atcs(ndx2) = atcs(ndx1) * d4 + atcs(ndx2) * d3
           atcs(ndx1) = d1
        end do
     end do
     !3260 continue
     if (iprsup .le. 1) go to 3019
     write (unit = lunit(6), fmt = 2023) ia, (isblk(kisblk + n * 8), n = 1, nuk)
     write (unit = lunit(6), fmt = 2025) (icolcs(kcolcs + m), atcs(katcs + m), atcs(kbtcs + m), m = 1, ia)
2025 format (/, ' icolcs, atcs, btcs', /, (1x, 3(i3, ': ', e13.6, '  + j', e13.6)))
     write (unit = lunit(6), fmt = 13019)
13019 format (//, ' calc. of phasor forcing functions.', /, 1x, '     row       j  idctcs     niu', 9x, 'xar(j)', 9x, 'xai(j)')
     !     ***  set  inputs  ***
3019 ndx1 = kxar   + 1
     call move0 (xar(ndx1 :), ktab)
     ndx1 = kxai + 1
     call move0 (xar(ndx1 :), ktab)
     ndy5 = kud1 - 5
     do i = niunrs, niu
        ndy5 = ndy5 + 5
        if (ud1(ndy5 + 4) .ge. 0.0d0) cycle
        if (ud1(ndy5 + 3) .ne. freqhz) cycle
        ud1(ndy5 + 3) = -ud1(ndy5 + 3)
        j = i + nuk
        ndxi = iuty(kiuty + i)
        ndx2 = kxai + j
        ndx3 = kxar + j
        if (ndxi .eq. 14) go to 3003
        if (ndxi .eq. 90 .or. ndxi .eq. 91) go to 3001
        cycle
3001    k = int (ud1(ndy5 + 2))
        if (ndxi .eq. 91) go to 3131
        xar(ndx3) = e(k)
        xar(ndx2) = f(k)
        go to 3030
3131    if (nextsw(k) .ne. 87) cycle
        xar(ndx3) = tclose(k)
        xar(ndx2) = energy(k)
        go to 3030
3003    d1 = ud1(ndy5 + 1)
        d2 = ud1(ndy5 + 2)
        xar(ndx3) = d1 * cosz (d2)
        xar(ndx2) = d1 * sinz (d2)
3030    if (iprsup .ge. 1)  write (unit = lunit(6), fmt = 13001) i, j, idctcs, niu, xar(ndx3), xar(ndx2)
13001   format (1x, 4i8, 2e15.6)
     end do
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 13020) (xar(kxar + i), xar(kxai + i), i = 1, ktab)
13020 format (/, ' (xar(i), xai(i), i=1, ktab)', /, (1x, 8e16.6))
3412 ite = ite + 1
     ipass = 0
     if (ite .le. 100) go to 3515
     write (unit = lunit(6), fmt = 7798) ite
     go to 3618
3515 l = iuty(kiuty + 4)
     if (l .gt. 0) call csupac (l, omegar)
     !     ***  Backward on xa  ***
     nuki = kisblk - 8
     do i = 1, nuk
        nuki = nuki + 8
        j = nuk + 1 - i
        nukj = kisblk + j * 8 - 8
        !3329    ndx1 = kxar   + j
        ndx1 = kxar + j
        xarsav = xar(ndx1)
        xar(ndx1) = 0.0d0
        ndx2 = kxai + j
        xaisav = xar(ndx2)
        xar(ndx2) = 0.0d0
        m = isblk(nukj + 8)
        n = ia
        if (j .ne. nuk) n = isblk(nukj + 16) - 1
3309    ndx3 = kcolcs + m
        if (icolcs(ndx3) .eq. j) go to 3334
        m = m + 1
        go to 3309
3334    m = m + 1
        if (m .gt. n) go to 3024
        k = icolcs(kcolcs + m)
        if (k .gt. nuk + niu) go to 3024
        d1 = xar(kxar + k)
        d2 = xar(kxai + k)
        ndx3 = katcs + m
        ndx4 = kbtcs + m
        xar(ndx1) = xar(ndx1) - (atcs(ndx3) * d1 - atcs(ndx4) * d2)
        xar(ndx2) = xar(ndx2) - (atcs(ndx3) * d2 + atcs(ndx4) * d1)
        if (iprsup .ge. 2) write (unit = lunit(6), fmt = 13334) i, j, k, m, n, d1, d2, xar(ndx1), xar(ndx2)
13334   format (' At 13334', 5i8, 4e20.6)
        go to 3334
3024    l = isblk(nukj + 7)
        if (l .gt. 0) call csupac (l, omegar)
        defr = absz (xarsav - xar(ndx1))
        defi = absz (xaisav - xar(ndx2))
        critia = 0.001 * sqrtz( xar(ndx1)**2 + xar(ndx2)**2 )
        if (critia .lt. epslon) critia = epslon
        if (defr .le. critia .and. defi .le. critia) cycle
        ipass = 1
     end do
     if (ipass .eq. 1 .and. iuty(kiuty + 9) .eq. 1 ) go to 3412
3618 if (iprsup .lt. 1 .and. kssout .eq. 0) go to 2389
     write (unit = lunit(6), fmt = 1357) freqhz
1357 format (/, '   AC steady state solution,  frequency = ', e16.6, /, '    name         real part  imaginary part       magnitude           angle')
     mtot = ioutcs
     if ( kssout .eq. 1 ) mtot = ktab
     j1 = nuk + niu
     j2 = nuk + lstat(64)
     do i = 1, mtot
        j = jout(kjout + i)
        if (kssout .eq. 1) j = i
        if (j .gt. j1  .and. j .le. j2) cycle
        i1 = ilntab( klntab + j )
        real = xar(kxar + j)
        rima = xar(kxai + j)
        amax = sqrtz (real ** 2 + rima ** 2)
        angl = 0.0d0
        if (amax .gt. 0.0 ) angl = picon * atan2z( rima, real )
        write (unit = lunit(6), fmt = 2489) texvec(i1), real, rima, amax, angl
2489    format ( 2x, a6, 3e16.6, f16.6 )
     end do
2389 ndx1 = kxtcs + lstat(68)
     do i = 1, ktab
        ndx2 = ndx1 + i
        xtcs(ndx2) = xtcs(ndx2) + xar(kxar + i)
     end do
     !     ***  histories  ***
     d1 = omegar * deltat
     d2 = cosz(d1)
     d3 =-sinz(d1)
     d1 = d2 + 1.0
     d4 = d1**2  +  d3**2
     d5 = d1/d4
     d6 = -d3/d4
     if ( iprsup  .ge.  1 ) write (unit = lunit(6), fmt = 1304) ktab, omegar, deltat, d2, d3, d5, d6
1304 format (/, 5x, 'ktab', 9x, 'omegar', 9x, 'deltat', 13x, 'd2', 13x, 'd3', 13x, 'd5', 13x, 'd6', /, 1x, i8, 6e15.6, /, 1x)
     nuki = kisblk - 8
     do i = 1,nuk
        nuki = nuki + 8
        if (isblk(nuki + 2) .eq. 1) cycle
        k1 = isblk( nuki + 4 )
        j = iabs (isblk(nuki + 1))
        k = iabs (isblk(nuki + 9)) - 1
        if (i .eq. nuk) k = nsu
        ndxi = kawkcs + i
        awkcs(ndxi) = 0.0d0
        ndxj = kbwkcs + i
        awkcs(ndxj) = 0.0d0
        if (iprsup .ge. 4) write (unit = lunit(6), fmt = 43033)
43033   format (/, 9x, '       i       m       j       k       n', 18x, 'd1', 12x, 'awkcs(i)', 12x, 'bwkcs(i)')
        do m = j, k
           n = ksus(kalksu + m)
           d1 = ksus(kksus + m)
           awkcs(ndxi) = awkcs(ndxi) + xar(kxar+n) * d1
           awkcs(ndxj) = awkcs(ndxj) + xar(kxai+n) * d1
           if (iprsup .ge. 4) write (unit = lunit(6), fmt = 13033) i, m, j, k, n, d1, awkcs(ndxi), awkcs(ndxj)
13033      format (' at 13033', 5i8, 3e20.6)
        end do
        !3033    continue
        kh = isblk( nuki + 2 )
        k = kh - 1
        j = isblk( nuki + 3 ) + k * 6
        if ( iprsup  .ge.  4 )  write ( lunit(6), 53033 )
53033   format (/, 9x, '       i       m       j       k      j1      m1', 12x, 'hstr(m1)', 12x, 'hsti(m1)')
        do m = 1, k
           ndy1 = kprsup + j
           d7 = parsup(ndy1 + 2)
           d8 = parsup(ndy1 + 3)
           ndx1 = katcs + kh - m
           ndx3 = kxar + k1
           atcs(ndx1) = d7 * awkcs(ndxi) - d8 * xar(ndx3)
           ndx4 = kbtcs  + kh - m
           ndx3 = kxai + k1
           atcs(ndx4) = d7 * awkcs(ndxj) - d8 * xar(ndx3)
           if (iprsup .ge. 4) write (unit = lunit(6), fmt = 23033) i, j, k, kh, atcs(ndx1), atcs(ndx4)
23033      format (' at 23033', 4i8, 2e20.6)
           j = j - 6
           if (m .eq. 1)  go to 3034
           ndx2 = ndx1 + 1
           d7 = atcs(ndx2)
           ndx3 = ndx4 + 1
           d8 = atcs(ndx3)
           d1 =  atcs(ndx1)  +  d7*d2  -  d8*d3
           d4 =  atcs(ndx4)  +  d7*d3  +  d8*d2
           atcs(ndx1)  =  d1*d5  -  d4*d6
           atcs(ndx4)  =  d1*d6  +  d4*d5
           if ( iprsup  .ge.  4 ) write (lunit(6), 33033)  atcs(ndx1), atcs(ndx4)
33033      format ( 57x, 2e20.6 )
3034       parsup(ndy1-1) = parsup(ndy1-1) + atcs(ndx1)
        end do
     end do
     if (nsup .eq. 0) go to 8888
     do i = 1, nsup
        n1 = -insup(kjsup + i)
        if (n1 .lt. 0) cycle
        mk = insup(kksup + i)
        if (mk .ne. 53 .and. mk .ne. 66) cycle
        j = ivarb(n1 + 1)
        k = ivarb(n1 + 2)
        br = 0.0d0
        bi = 0.0d0
        do n = j, k
           m = ksus(kalksu + n)
           n2 = ksus(kksus + n)
           br = br + xar(kxar + m) * n2
           bi = bi + xar(kxai + m) * n2
        end do
        nn = ivarb(n1)
        !        m = parsup(nn)
        m = int (parsup(nn))
        !        n = parsup( nn + 2 )
        n = int (parsup(nn + 2))
        a = omegar * deltat
        if (mk .eq. 66) n = ivarb(n1 + 3)
        do j = 1, n
           ndx1 = kprsup + m + n - j
           if ( mk .eq. 66 ) ndx1 = nn + n - j + 1
           !7039       parsup(ndx1) = parsup(ndx1) + br*cosz(a*j) + bi*sinz(a*j)
           parsup(ndx1) = parsup(ndx1) + br * cosz(a * j) + bi * sinz(a * j)
        end do
     end do
8888 ite = 0
  end do
  mdy5 = kud1 - 5
  do i = niunrs, niu
     mdy5 = mdy5 + 5
     if (ud1(mdy5 + 3).lt. 0.0d0) ud1(mdy5 + 3) = -ud1(mdy5 + 3)
  end do
  go to 405
  !     ***  superimposing dc steady-state  ***
3042 ndx1 = kxtcs + lstat(68)
  do i = 1, ktab
     ndx2 = kxtcs + i
     xtcs(ndx2) = xtcs(ndx2) + xtcs(ndx1 + i)
  end do
  nuki = kisblk - 8
  do i = 1, nuk
     nuki = nuki + 8
     k = isblk(nuki+2) - 1
     if (k .eq. 0) cycle
     ndy1 = kprsup + isblk(nuki + 3)
     ndy0 = ndy1 + 4
     do j = 1, k
        parsup(ndy1+4) = parsup(ndy1+4) + parsup(ndy1+5)
        ndy1 = ndy1 + 6
     end do
     if ( iprsup .gt. 2 ) write ( lunit(6), 6890 ) j, ( parsup(n), n = ndy0, ndy1, 6 )
6890 format ('  function ', i5, '  hst ', 7e14.6)
  end do
  nukr = krsblk - 4
  do i = 1, nuk
     nukr = nukr + 4
     ndx1 = kxtcs + i
     if (xtcs(ndx1) .ge. rsblk(nukr + 2) .and. xtcs(ndx1) .le. rsblk(nukr + 3)) cycle
     ndx1 = ilntab(klntab + i)
     if (iuty(kiuty + 3) .eq. 0) cycle
     iuty(kiuty + 3) = iuty(kiuty + 3) - 1
     if (noutpr .eq. 0) write (unit = lunit(6), fmt = 3040) texvec(ndx1)
3040 format ('0warning.  TACS variable ', "'", a6, "'", ' is allowed to operate outside its limits during the steady-state solution.', /, 1x)
  end do
  go to 317
9000 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6518) istep, nchain, ioutcs, t
6518 format (/, " Exit  'tacs2' .   istep  nchain  ioutcs",  14x,  't', /, 16x, 3i8, e15.5)
  return
end subroutine tacs2

!
! subroutine csupdc.
!

subroutine csupdc (l)
  use blkcom
  use labcom
  use tacsar
  use tracom
  implicit none
  integer(4), intent(in) :: l
  integer(4) :: i
  integer(4) :: j, ji
  integer(4) :: k, karg, kjsup, kksup
  integer(4) :: m
  integer(4) :: n, n1, n2, n7, ndx1, ndx2, ndx3, ndx4, nn, nnn
  real(8) :: a
  real(8) :: b
  real(8) :: contss
  real(8) :: d9
  !
  kjsup = kinsup + lstat(65)
  kksup = kjsup  + lstat(65)
  !2001 if ( iprsup .lt. 6 )  go to 1000
  if (iprsup .lt. 6) go to 1000
  write (unit = lunit(6), fmt = 1001) t, nsup, karg, kpar
1001 format ('0entering subroutine  csupdc  at  t=', e13.6, /, '0e nsup=', i6, '   karg=', i8, '   kpar=', i6)
  write (unit = lunit(6), fmt = 1002) (i, ilntab(i + kspvar), insup(i + kjsup), insup(i + kksup), i = 1, nsup)
1002 format ('  number  supvar    jsup    ksup', /, (4i8))
  write (unit = lunit(6), fmt = 1033) karg
1033 format ('  karg =', i8, /, '       n  iopsup  ifnsup  irgsup    idev     kdj     kdk  ildev1  ildev2')
  do i = 1, nsup
     n1 = insup(kjsup + i)
     if (n1 .lt. 0) go to 2014
     n2 = insup(kksup + i)
     write (unit = lunit(6), fmt = 2008) (n, ivarb(n + 1), ivarb(n + 2), ivarb(n + 2), n = n1, n2, 3)
2008 format (4i8)
     cycle
2014 n1 = -n1
     write (unit = lunit(6), fmt = 2022) n1, ivarb(n1), ivarb(n1 + 1), ivarb(n1 + 2), ivarb(n1 + 3), ivarb(n1 + 4)
2022 format (i8, 24x, 5i8)
  end do
  if (kpar .ne. 0) write (unit = lunit(6), fmt = 1004) (i, parsup(i + kprsup), i = 1, kpar)
1004 format ('0e', 5x, 'parsup ...', /, (' e ', 5(i3, 1x, e15.6, 3x)))
1000 nnn = kxtcs + nuk + lstat(64)
  i = l
1234 a = 0.0d0
  n1 = -insup(kjsup + i)
  n2 = insup(kksup + i)
  if (n1 .lt. 0) go to 10
  nn = ivarb(n1)
  if (n2 .eq. 60 .or. n2 .eq. 61) go to 602
  if (n2 .eq. 63 .or. n2 .eq. 67) go to 602
  j = ivarb(n1 + 1)
  k = ivarb(n1 + 2)
  b = 0.0d0
  do n = j, k
     m = kxtcs + ksus(kalksu + n)
     b = b + xtcs(m) * ksus(kksus + n)
  end do
602 n2 = n2 - 49
  !  go to (10, 651,651,653, 10,655,656, 10, 10, 10, 660 ,661,662, 10,664, 10,666,667 ), n2
  select case (n2)
  case (1, 5, 8, 9, 10, 16)
     go to 10

  case (2, 3)
     go to 651

  case (4)
     go to 653

  case (6)
     go to 655

  case (7)
     go to 656

  case (11)
     go to 660

  case (12)
     go to 661

  case (13)
     go to 662

  case (14)
     go to 10

  case (15)
     go to 664

  case (17)
     go to 666

  case (18)
     go to 667
  end select
  !     ---  Relays and level-triggers  ---
651 if (parsup(nn) .ne. 0.0d0) b = b * parsup(nn)
  if (parsup(nn + 2) .gt. -1.0d0) go to 10
  a = b
  go to 11
  !     ---  Variable transport delay  ---
653 if (parsup(nn + 3) .eq. 0.0d0) go to 10
  a = b
  go to 11
  !     ---  Digitizer  ---
655 if (parsup(nn + 1) .ne. 1.0d0) go to 10
  m = ivarb(n1 + 3)
  n = ivarb(n1 + 4)
  if (parsup(nn) .ne. 0.0d0) b = b * parsup(nn)
  ndx1 = kprsup + m
  a = parsup(ndx1)
  if (m .eq. n) go to 11
  j = m + 1
  do k = j, n
     m = n - k + j
     ndx1 = kprsup +  m
     if (b .ge. parsup(ndx1)) go to 65501
  end do
  !65500 continue
  go to 11
65501 ndx1 = kprsup + m
  a = parsup(ndx1)
  go to 11
  !     ---  point-by-point non-linearity  ---
656 if (parsup(nn + 1) .ne. 1.0d0) go to 10
  m = ivarb(n1 + 3)
  n = ivarb(n1 + 4)
  if (parsup(nn) .ne. 0.0d0) b = b * parsup(nn)
  ndx1 = kprsup +  m + 1
  a = parsup(ndx1)
  if (n .le. m + 1) go to 11
  ndx1 = kprsup +  m
  if (b .le. parsup(ndx1)) go to 11
  j = m + 2
  do k = j, n, 2
     ndx1 = kprsup + k
     if (b .le. parsup(ndx1)) go to 65601
  end do
  !65600 continue
  ndx1 = kprsup +  n
  a = parsup(ndx1)
  go to 11
65601 ndx1 = kprsup + k+1
  ndx2 = kprsup + k - 1
  ndx3 = kprsup + k
  ndx4 = kprsup + k - 2
  d9 = (parsup(ndx1) - parsup(ndx2)) / (parsup(ndx3) - parsup(ndx4))
  a = parsup(ndx2) + d9 * (b - parsup(ndx4))
  go to 11
  !     ---  input  if - block  ---
660 n7 = int (parsup(nn + 1))
  if (n7 .lt. 1 .or. n7 .gt. 3) go to 10
  j = ivarb(n1 + 1) - n7 + 3
  ndx4 = kxtcs + ksus(kalksu + j)
  a = xtcs(ndx4) * ksus(kksus + j)
  go to 11
  !     ---  input  signal  selector  ---
661 n7 = int (parsup(nn))
  if (n7 .le. 0 .or. n7 .gt. 8) go to 10
  !  go to ( 6611, 6611, 6611, 6611, 6611, 6616, 6617, 6618 ), n7
  select case (n7)
  case (1, 2, 3, 4, 5)
     go to 6611

  case (6)
     go to 6616

  case (7)
     go to 6617

  case (8)
     go to 6618
  end select
6611 j = ivarb(n1 + 1) - n7 + 5
  m = ksus(kalksu + j)
  if (m .eq. 0) go to 11
  a = xtcs(kxtcs + m) * ksus(kksus + j)
  go to 11
6616 ndx3 = ivarb(n1 + 3)
  if (ndx3 .eq. 0) go to 11
  a = xtcs(kxtcs + ndx3)
  go to 11
6617 a = parsup(nn + 2)
  go to 11
6618 a = parsup(nn + 1)
  go to 11
  !     ---  Track  and  sample  ---
662 contss = parsup(nn + 1)
  if (contss .lt. 0.5d0) go to 10
  a = parsup(nn + 2)
  if (contss .gt. 1.5d0) a = b
  go to 11
  !     --- min/max tracking, controlled accumulator or couhter ---
664 if (parsup(nn) .eq. 0.0d0) go to 10
  if (parsup(nn) .eq. 99999.0d0) parsup(nn) = 0.0d0
  a = b + parsup(nn) * parsup(nn + 1)
  go to 11
666 do ji = 1, k
     a = a + parsup(nn + ji)
  end do
  a = sqrtz (a * parsup(nn) + b * b)
  go to 11
667 a = 1.0d0
11 ndx1 = nnn + i
  xtcs(ndx1) = a
10 i = insup(kinsup + i)
  if (i .gt. 0) go to 1234
  !9999 return
  return
end subroutine csupdc

!
! subroutine cuspac.
!

subroutine csupac (l, omegar)
  use blkcom
  use labcom
  use tacsar
  use tracom
  implicit none
  integer(4), intent(in) :: l
  real(8), intent(in) :: omegar
  integer(4) :: i
  integer(4) :: j
  integer(4) :: k, kjsup, kksup
  integer(4) :: m, mi
  integer(4) :: n, n1, n2, n7, ndx1, ndx2, ndx3, ndx4, ndx5, nn, nnn
  real(8) :: a, aa, ai
  real(8) :: b, bi
  real(8) :: contss
  !
  kjsup = kinsup + lstat(65)
  kksup = kjsup  + lstat(65)
  !1000 nnn = kxar + nuk + lstat(64)
  nnn = kxar + nuk + lstat(64)
  i = l
1234 a = 0.0d0
  ai = 0.0
  n1 = -insup(kjsup + i)
  n2 = insup(kksup + i)
  if (n1 .lt. 0) go to 10
  nn = ivarb(n1)
  if (n2 .eq. 60 .or. n2 .eq. 61) go to 602
  if (n2 .eq. 63 .or. n2 .eq. 67) go to 602
  j = ivarb(n1 + 1)
  k = ivarb(n1 + 2)
  b = 0.0
  bi = 0.0
  do n = j, k
     m = kxar + ksus(kalksu + n)
     mi = m + lstat(68)
     b = b + xar(m) * ksus(kksus + n)
     bi = bi + xar(mi) * ksus(kksus + n)
  end do
602 n2 = n2 - 49
  !  go to (10, 651, 651, 653, 10, 10, 10, 10, 10, 659, 660, 661, 662, 10, 664, 10, 10, 10), n2
  select case (n2)
  case (1, 5, 6, 7, 8, 9, 14, 16, 17, 18)
     go to 10

  case (2, 3)
     go to 651

  case (4)
     go to 653

  case (10)
     go to 659

  case (11)
     go to 660

  case (12)
     go to 661

  case (13)
     go to 662

  case (15)
     go to 664
  end select
  !     ---  relays and level-triggers  ---
651 if (parsup(nn + 2) .gt. -1.) go to 10
  if (parsup(nn) .eq. 0.0) go to 6511
  b = b * parsup(nn)
  bi = bi * parsup(nn)
6511 a = b
  ai = bi
  go to 11
  !     ---  variable transport delay  ---
653 if (parsup(nn + 3) .eq. 0.0) go to 10
  aa = omegar * deltat * parsup(nn+2)
  a  = b  * cosz (aa) + bi * sinz (aa)
  ai = bi * cosz (aa) - b  * sinz (aa)
  go to 11
  !       --- simple derivative ---
659 if (parsup(nn + 2) .ne. 1.0) go to 10
  a = - bi * omegar
  ai =  b  * omegar
  go to 11
  !     ---  input  if - block  ---
  !660 n7 = parsup(nn + 1)
660 n7 = int (parsup(nn + 1))
  if (n7 .lt. 1 .or. n7 .gt. 3) go to 10
  j = ivarb(n1 + 1) - n7 + 3
  ndx4 = kxar + ksus(kalksu + j)
  ndx5 = ndx4 + lstat(68)
  a = xar(ndx4) * ksus(kksus + j)
  ai = xar(ndx5) * ksus(kksus + j)
  go to 11
  !     ---  input  signal  selector  ---
  !661 n7 = parsup(nn)
661 n7 = int (parsup(nn))
  if (n7 .le. 0 .or. n7 .gt. 8) go to 10
  go to (6611, 6611, 6611, 6611, 6611, 6616, 6617, 6618), n7
6611 j = ivarb(n1 + 1) - n7 + 5
  m = ksus(kalksu + j)
  if (m .eq. 0) go to 11
  a = xar(kxar + m) * ksus(kksus + j)
  ai = xar(kxar + m + lstat(68)) * ksus(kksus + j)
  go to 11
6616 ndx3 = ivarb(n1 + 3)
  if (ndx3 .eq. 0) go to 11
  a = xar(kxar + ndx3)
  ai = xar(kxar + ndx3 + lstat(68))
  go to 11
6617 a = parsup( nn + 2 )
  go to 11
6618 a = parsup( nn + 1 )
  go to 11
  !     ---  track  and  sample  ---
662 contss = parsup(nn + 1)
  if (contss .lt. 1.5) go to 10
  a = b
  ai = bi
11 ndx1 = nnn + i
  ndx2 = ndx1 + lstat(68)
  xar(ndx1) = a
  xar(ndx2) = ai
  go to 10
  !     --- min/max tracking, controlled accumulator or couhter ---
664 if (parsup(nn) .ne. 99999.) go to 10
  parsup(nn) = sqrtz(b * b + bi * bi)
10 i = insup(kinsup + i)
  if (i .gt. 0) go to 1234
  !9999 return
  return
end subroutine csupac

!
! subroutine premec.
!

subroutine premec
  use blkcom
  use labcom
  use tacsar
  use smach
  use movcop
  implicit none
  !     This module is used only by Brandwajn (type-59) s.m. model
  integer(4) :: i26, i30, ic, id, ii, ik, ilk, imk, iml
  integer(4) :: k
  integer(4) :: l2, l4
  integer(4) :: n2, n3, n4, n6, n7, n8, n17, n21, n22, n23, n24, n25, num, num2
  integer(4) :: num4, num8, numask
  real(8) :: csp
  real(8) :: zyn
  !     construct the constant matrices of coefficients  ****************
  imk = 0
  n17 = 0
  do ilk = 1, nst
     if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5289) ilk
  end do
5289 format (/, ' begin next gen in  ', "'", 'premec', "'", ' .', 'ilk', /, 30x, i8)
  i30 = 30 * ilk - 29
  numask = ismdat( i30+11 )
  num2 = numask  +  numask
  num4 = num2 + num2
  num = num2 + num4
  num8 = num4 + num4
  iml = imk + 1
  call move0 (shp(iml :), num2)
  iml = iml + num8
  call move0 (shp(iml :), num4)
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 90) numask, num2, num4, imk, nst
90 format (2x, 'in premec b4 708', /, 2x, 13i10)
  !     lower subdiagonal and diagonal   * * * * * * * * * * * * * * * * *
  l2 = imk
  l4 = l2 + num8
  ic = l4
  n2 = l2 + num2
  n21 = n2 + numask
  n22 = n21 + num2
  n24 = n21 + num4
  if (numask .eq. 1) go to 710
  n4 = n22 - 1
  do k = 2, numask
     l4 = l4 + 4
     l2 = l2 + 2
     n3 = n4 + k
     zyn = -(shp(n3) * delta2 + shp(n3 - numask))
     shp(l2) = zyn
     shp(l2 + 1) = -zyn
     shp(l4 - 2) = -2.0 * shp(n3)
     shp(l4) = zyn
     shp(l4 + 1) = -shp(l4 - 2)
     shp(l4 + 3) = -zyn
  end do
  !708 continue
  !     diagonal   ******************************************************
  n6 = numask - 1
  l2 = imk
  l4 = ic
  do k = 1, n6
     l2 = l2 + 2
     l4 = l4 + 4
     shp(l2 - 1) = shp(l2 - 1) - shp(l2)
     shp(l4 - 3) = shp(l4 - 3) - shp(l4 - 2)
     shp(l4 - 1) = shp(l4 - 1) - shp(l4)
  end do
  !709 continue
  !     complete the calculation of the diagonal elements  * * * * * * * *
710 l2 = imk
  l4 = ic
  do k = 1, numask
     n24 = n24 + 1
     n22 = n21 + k
     n23 = n24 - numask
     l2 = l2 + 2
     l4 = l4 + 4
     shp( l2-1 ) = shp( l2-1 ) + shp( n23 ) + shp( n24 )  + shp( n22 ) / delta2
     shp( l4-1 ) = shp( l4-1 ) + shp( n23 ) + shp( n24 )  - shp( n22 ) / delta2
  end do
  !711 continue
  if (iprsup .le. 0) go to 712
  write (unit = lunit(6), fmt = 901) ilk
901 format (2x, 'constant matrices for generator no.', i5)
  id = ic + 1
  ik = ic + num4
  write (unit = lunit(6), fmt = 902) id, ik, (shp(ii), ii = id, ik)
902 format (2x, 'matrix y(i), i =', i5, 2x, 'to', i5, /, 2x, (2x, 6e21.12))
  id = imk + 1
  ik = imk + num2
  write( lunit(6), 902 )  id, ik, ( shp(ii), ii = id, ik )
712 n7  = n17 + numask + num4
  n25 = n24 - numask
  i26 = 101 * ilk - 75
  csp = omega / elp(i26)
  do k = 1, numask
     n8 = n7 + k
     histq( n8 ) = histq(n8) * csp
     n8 = n25 + k
     !713  shp( n8 ) = shp( n8 ) * 2.0 * csp
     shp(n8) = shp(n8) * 2.0d0 * csp
  end do
  imk = imk + num + num
  n17 = n17 + num
  if (iprsup .lt. 1) go to 1
  n7 = n17 - numask + 1
  write(unit = lunit(6), fmt = 905) n7, n17, (histq(k), k = n7, n17)
905 format(2x, 'array power from', i6, '  to', i6, /, (2x, 6e20.11))
1 continue
  return
end subroutine premec

!
! subroutine mul.
!

subroutine mul (y, z, xay, l1, m, n)
  implicit none
  !     Multiplication of two matrices stored as vectors by columns
  real(8), intent(out) :: xay(:)
  integer(4), intent(in) :: l1
  integer(4), intent(in) :: m
  integer(4), intent(in) :: n
  real(8), intent(in) :: y(:)
  real(8), intent(in) :: z(:)
  integer(4) :: i
  integer(4) :: j
  integer(4) :: l
  integer(4) :: n1, n2, nl, nl1, nm
  real(8) :: a
  !
  l = 1
2 j = 1
  nm = -n
4 nm = nm + n
  nl = nm + l
  a = 0.0
  i = 1
  n1 = -m
  n2 = nm
3 n1 = n1 + m
  nl1 = n1 + l
  n2 = n2 + 1
  a = a + y(nl1) * z(n2)
  i = i + 1
  if (i .le. m) go to 3
  xay(nl) = a
  j = j + 1
  if (j .le. n) go to 4
  l = l + 1
  if (l .le. l1) go to 2
  return
end subroutine mul

!
! subroutine elecyy.
!

subroutine elecyy
  use blkcom
  use labcom
  use tacsar
  use smach
  use tracom
  use movcop
  use over12mod
  implicit none
  !     This module is used only by brandwajn (type-59) s.m. model
  integer(4) :: i, i26, i30, i75, ibk, ibl, idelta, ids, idt, ii, ik, ilk, imech, in, is, iu
  integer(4) :: j30, juk
  integer(4) :: k, k1
  integer(4) :: ll1, ll3, ll7, ll36
  integer(4) :: n07, n1, n2, nwd
  real(8) :: a, acde, acdf, afd, afq, akm, akn
  real(8) :: b, b6, d7
  real(8) :: etot
  real(8) :: fact
  real(8) :: ra
  real(8) :: rat1, sb, sf2, sf3, sf4, sf5, sf6, sf7, sf8, sf9, sum
  real(8) :: xl
  !
  !     This routine calculates the matrices of the electrical part  *****
  ibr = ibrold
  it = itold
  imech = 0
  ilk = 0
  n07 = 0
  nwd = 24
  ll36 = 36
  fact = 1.0 / (omega * delta2)
  akm = epsuba
  if (akm .lt. 2.0d0) akm = 100.0d0
  akm = 1.0d0 / akm
  akn = 1.0d0 / (1.0d0 + akm)
  akm = 1.0d0 - 2.0d0 * akn * akm
  fact = fact * akn
  damrat = akm
  factom = fact * omega
  om2 = omega * onehaf
  j30 = 1
  do juk = 1, numsm
     k1 = ismdat(j30)
     !     build branch terminal information *******************************
     idelta = ismdat(j30 + 1)
     if (idelta .eq. 1) go to 401
     ismdat(j30 + 5) = 1
     ismdat(j30 + 6) = 1
     ismdat(j30 + 7) = 1
     go to 402
401  ismdat(j30 + 5) = ismdat(j30 + 3)
     ismdat(j30 + 6) = ismdat(j30 + 4)
     ismdat(j30 + 7) = ismdat(j30 + 2)
402  ilk = ilk + 1
     i26 = ilk * 101 - 100
     i75 = i26 + 26
     i30 = ilk * 30 - 29
     k1  = k1 - 1
     !     fill-in the a11 submatrix of generator no. ilk   *****************
     call move0 (z(1 :), ll36)
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6701) ilk, numsm
6701 format (/, ' Companion network model for generator no.', i5, i8)
     !     Change  storage  arrangement   for  the   inductances, i.e. store*
     !     separately leakages and main reactances *************************
     xl = elp(i26 + 18)
     elp(i26) = elp(i26) - xl
     elp(i26 + 8) = elp(i26 + 8) - xl
     z(65) = elp(i26 + 1) * fact
     z(66) = elp(i26 + 3) * fact
     elp(i26 + 4) = elp(i26 + 4) * fact
     z(67) = elp(i26 + 9) * fact
     z(68) = elp(i26 + 11) * fact
     elp(i26 + 12) = elp(i26 + 12) * fact
     rat1 = elp(i26 + 20)
     z(61) = z(65) * rat1
     z(62) = z(66) * rat1
     z(63) = z(67) * rat1
     z(64) = z(68) * rat1
     elp(i26 + 2) = elp(i26 + 2) * fact - z(61)
     elp(i26 + 5) = elp(i26 + 5) * fact - z(62)
     elp(i26 + 10) = elp(i26 + 10) * fact - z(63)
     elp(i26 + 13) = elp(i26 + 13) * fact - z(64)
     a = elp(i26 + 16) * fact
     elp(i26 + 16) = a + elp(i26 + 17)
     elp(i26 + 17) = a - elp(i26 + 17) * akm
     acde = 1.0
     acdf = 1.0
     rat1 = 1.0 / rat1
     elp(i26 + 20) = rat1
     b6 = elp(i26 + 8) / elp(i26)
     ismdat(i30 + 9) = 0
     ismdat(i30 + 10) = 0
     if (ismdat(i30 + 8) .eq. 0) go to 404
     sf3 = cu(n07 + 1) * rat1 + cu(n07 + 4) + cu(n07 + 5)
     sf2 = (cu(n07 + 2) * rat1 + cu(n07 + 6) + cu(n07 + 7)) * b6
     sf3 = sqrtz(sf3 ** 2 + sf2 ** 2)
     sf8 = elp(i26 + 21)
     if (sf8 .ge. sf3) go to 403
     !     d - axis saturated  **********************************************
     d7 = sf3 / sf8 - 0.9
     !     is = 10.0 * d7
     is = int (10.0 * d7)
     !     is = (is + 1) / 2
     is = int ((is + 1) / 2)
     ismdat(i30 + 9) = is
     sb = is
     sb = sb * 0.1
     sf4 = sf8 * ( 0.9 + sb )
     sf5 = sf8 * ( 1.1 + sb )
     sf9 = elp( i26+22 )
     sf6 = sf4 / ( 1.0 + sf9 * ( sf4 - sf8 ) )
     sf7 = sf5 / ( 1.0 + sf9 * ( sf5 - sf8 ) )
     acde = ( sf7 - sf6  ) / ( sf5 - sf4 )
403  sf8 = elp( i26+23 )
     if( sf8 .ge. sf3 )  go  to  404
     !     q - axis saturated  **********************************************
     d7 = sf3 / sf8 - 0.9
     !     is = 10.0 * d7
     is = int (10.0 * d7)
     !     is = ( is + 1 ) / 2
     is = int ((is + 1) / 2)
     ismdat( i30+10 ) = is
     sb = is
     sb = sb * 0.1
     sf4 = sf8 * ( 0.9 + sb )
     sf5 = sf8 * ( 1.1 + sb )
     sf9 = elp( i26+24 )
     sf6 = sf4 / ( 1.0 + sf9 * ( sf4 - sf8 ) )
     sf7 = sf5 / ( 1.0 + sf9 * ( sf5 - sf8 ) )
     acdf = ( sf7 - sf6  ) / ( sf5 - sf4 )
404  write( lunit(6), 6702 )  ilk, ismdat( i30+9 ), ismdat( i30+10 )
6702 format( 1x, 3h***, 10x, 11hmachine no., i6, 10x,31hbegin operation on segments no., 2x, 2i10 )
     z( 50 ) = ( elp( i26 ) * acde + xl ) * fact
     z( 51 ) = ( elp( i26+8 ) * acdf + xl )  * fact
     z( 52 ) =  elp( i26+2 ) + z( 61 ) * acde
     z( 53 ) =  elp( i26+5 ) + z( 62 ) * acde
     z( 54 ) =  elp( i26+10 ) + z( 63 ) * acdf
     z( 55 ) =  elp( i26+13 ) + z( 64 ) * acdf
     !     fill in  the  the  transformer terms   of  the resistive history**
     !     matrix of  the  generator*****************************************
     ra = elp( i26+19 )
     z( 1 ) = z( 50 ) + ra
     z( 8 ) = z( 51 ) + ra
     z( 15 ) = z( 52 ) + elp( i26+6 )
     z( 22 ) = z( 53 ) + elp( i26+7 )
     z( 29 ) = z( 54 ) + elp( i26+14 )
     z( 36 ) = z( 55 ) + elp( i26+15 )
     z( 11 ) =  z( 67 ) * acdf
     z( 26 ) = z( 11 )
     z( 12 ) = z( 68 ) *  acdf
     z( 32 ) = z( 12 )
     z( 30 ) = elp( i26+12 ) * acdf
     z( 35 ) = z( 30 )
     z( 3 ) = z( 65 ) * acde
     z( 13 ) = z( 3 )
     z( 4 ) = z( 66 ) * acde
     z( 19 ) = z( 4 )
     z( 16 ) = elp( i26+4 ) * acde
     z( 21 ) = z( 16 )
     !     change reactances (at statfr hz ) to  inductances ****************
     elp(  i26  ) = elp(  i26  ) / omega
     elp( i26+1 ) = elp( i26+1 ) / omega
     elp( i26+3 ) = elp( i26+3 ) / omega
     elp( i26+8 ) = elp( i26+8 ) / omega
     elp( i26+9 ) = elp( i26+9 ) / omega
     elp( i26+11 ) = elp( i26+11 ) / omega
     xl = xl / omega
     elp( i26+18 ) = xl
     if(  iprsup  .lt.  1  )   go  to  406
     do k = 1, 6
        n2 = 6 * k
        n1 = n2 - 5
        write( lunit(6), 6703 )   k,  ( z(i), i=n1, n2 )
     end do
6703 format (/, ' new column', i6, /, (1x, 7e17.8))
406  call move (z(1 :), x1(1 :), ll36)
     ll7 = 6
     ll3 = 2
     call reduce (x1(1 :), ll7, ll3)
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6704) ilk
6704 format(/, ' reduced network model.', i10)
     if( iprsup .lt. 1 )   go  to  408
     do k = 1, 6
        n2 = 6 * k
        n1 = n2 - 5
        !407     write( lunit(6), 6703 )   k,  ( x1(i), i=n1, n2 )
        write (unit = lunit(6), fmt = 6703) k, (x1(i), i = n1, n2)
     end do
     !     store constant terms for insertion into the transients program
408  a = x1( 1 )
     b = x1( 8 )
     sf8 = a
     sf9 = b
     sum = a
     x1( 8 ) = b - a
     !     sum=(a+b) * onehaf
     !     a=a-sum
     !     x1(1)=a
     !     x1( 8 ) = -a
     etot = elp( i26+16 )
     elp( i26+16 ) = 1.0 / etot
     !     the values ''a'' and ''b'' are to be inserted into the transients
     b = ( etot-sum ) / 3.0
     a = ( etot+2.0*sum ) / 3.0
     if ( iprsup  .ge.  2 ) write( lunit(6), 6705 )  a, b
6705 format( /,  18h correction terms.   ,/,  1x,  6e20.12  )
     !     add to the branch tables tr,tx,c,kbus,mbus,length,nr * * * * * * *
     !     note that the inverse matrix ai has been calculated in park's
     !     dq0  coordinates.this simplifies the calculations
     it=it+1
     tr(it)=a
     tx(it)=0.0
     c(it)=0.0
     length( ibr+1 ) = 3
     nr( ibr+1 ) = it
     cik( ibr+1 ) = 0.0
     ci( ibr+1 ) = 0.0
     ck( ibr+1 ) = 0.0
     kbus( ibr+1 ) = ismdat( j30+2 )
     mbus( ibr+1 ) = ismdat( j30+5 )
     it=it+1
     kbus( ibr+2 ) = ismdat( j30+3 )
     mbus( ibr+2 ) = ismdat( j30+6 )
     nr( ibr+2 ) = it
     length( ibr+2 ) = -555
     ck( ibr+2 ) = 0.0
     ci( ibr+2 ) = 0.0
     cik( ibr+2 ) = 0.0
     tr(it)=b
     tx(it)=0.0
     c(it)=0.0
     it=it+1
     tr(it)=a
     tx(it)=0.0
     c(it)=0.0
     it=it+1
     kbus( ibr+3 ) = ismdat( j30+4 )
     mbus( ibr+3 ) = ismdat( j30+7 )
     nr( ibr+3 ) = it
     cik( ibr+3 ) = 0.0
     ck( ibr+3 ) = 0.0
     ci( ibr+3 ) = 0.0
     length( ibr+3 ) = -2
     ibr = ibr + 3
     tr(it)=b
     tx(it)=0.0
     c(it)=0.0
     it=it+1
     tr(it)=b
     tx(it) = 0.0
     c(it) = 0.0d0
     it = it + 1
     tr(it) = a
     tx(it) = 0.0d0
     c(it) = 0.0d0
     if( iprsup .lt. 1 )  go  to  1408
     ll1 = ibr - 2
     write( lunit(6), 6706 )  ( nr( i ), i = ll1, ibr )
6706 format( 8x, 5i10 )
     write (unit = lunit(6), fmt = 6706) (length(i), i = ll1, ibr)
     ll1 = it - 5
     write (unit = lunit(6), fmt = 6707) (tr(i), i = ll1, it)
6707 format( 3x, 8e15.7 )
1408 a = 1.0 / sum
     b = 1.0 / etot
     sum = ( b + 2.0 * a ) / 3.0
     etot = ( b - a ) / 3.0
     elp(  i75  ) = sum
     elp( i75+1 ) = etot
     elp( i75+53 ) = a * athtw
     elp( i75+54 ) = b * asqrt3
     if( iprsup .gt. 0 ) write( lunit(6), 6707 )   elp(i75), elp(i75+1), elp(i75+53), elp(i75+54)
     !     correction terms for voltage and current calculations*************
     !     note that only the nonzero terms are to be stored ***************
     elp( i75+4 ) = b6
     elp( i75+5 ) = x1( 3 )
     elp( i75+6 ) = x1( 4 )
     elp( i75+7 ) = x1( 11 )
     elp( i75+8 ) = x1( 12 )
     elp( i75+9 ) = x1( 15 )
     elp( i75+10 ) = x1( 21 )
     elp( i75+11 ) = x1( 16 )
     elp( i75+12 ) = x1( 22 )
     elp( i75+13 ) = x1( 29 )
     elp( i75+14 ) = x1( 35 )
     elp( i75+15 ) = x1( 30 )
     elp( i75+16 ) = x1( 36 )
     elp( i75+43 ) = x1( 13 )
     elp( i75+44 ) = x1( 19 )
     elp( i75+45 ) = x1( 26 )
     elp( i75+46 ) = x1( 32 )
     elp( i75+47 ) = 1.0 / sf8
     elp( i75+48 ) = xl  / sf8
     elp( i75+49 ) = sf9
     elp( i75+50 ) = 0.0
     elp( i75+51 ) = 0.0
     elp( i75+52 ) = x1( 8 )
     if( iprsup .lt. 2 )  go  to  409
     write (unit = lunit(6), fmt = 6708) ilk
6708 format (5x, ' Arrays for mach no.', i5, '  in order of ac ', 1x, 'a21 ', 1x, 'a22 ', 1x)
     write (unit = lunit(6), fmt = 6707) elp(i75 + 2), elp(i75 + 3), elp(i75 + 51), elp(i75 + 52)
     idt = i75 + 5
     ids = idt + 3
     write (unit = lunit(6), fmt = 6707) (elp(iu), iu = idt, ids)
     ids = ids + 1
     idt = ids + 7
     write (unit = lunit(6), fmt = 6707) (elp(iu), iu = ids, idt)
     !     calculate the resistive history matrix **************************
409  elp( i75+17 ) = z( 50 ) - ra * akm
     elp( i75+18 ) = z( 13 )
     elp( i75+19 ) = z( 19 )
     elp( i75+20 ) = z( 51 ) - ra * akm
     elp( i75+21 ) = z( 26 )
     elp( i75+22 ) = z( 32 )
     elp( i75+23 ) = z( 3  )
     elp( i75+24 ) = z( 52 ) - elp( i26+6 ) * akm
     elp( i75+25 ) = z( 21 )
     elp( i75+26 ) = z( 4  )
     elp( i75+27 ) = z( 16 )
     elp( i75+28 ) = z( 53 ) - elp( i26+7 ) * akm
     elp( i75+29 ) = z( 11 )
     elp( i75+30 ) = z( 54 ) - elp(i26+14)  * akm
     elp( i75+31 ) = z( 35 )
     elp( i75+32 ) = z( 12 )
     elp( i75+33 ) = z( 30 )
     elp( i75+34 ) = z( 55 ) - elp(i26+15)  * akm
     !     store additional elements for future calcultions of incremental***
     !     inductances and updating of the matrix /ys/ **********************
     do ii = 35, 42
        elp( i75+ii ) = z( ii+26 )
     end do
     !     store flux linkages for synchronous frame*************************
     a = elp( i26 ) * cu( n07+1 ) + elp( i26+1 ) * cu( n07+4 ) + elp( i26+3 ) * cu( n07+5 )
     afd = a * elp( i75+2 ) + xl * cu( n07+1 )
     a = elp( i26+8 ) * cu( n07+2 ) + elp( i26+9 ) * cu( n07+6 ) + elp( i26+11 ) * cu( n07+7 )
     afq = a * elp( i75+3 ) + xl * cu( n07+2 )
     cu( n07+15 ) = afd
     cu( n07+16 ) = afd
     cu( n07+17 ) = afq
     cu( n07+18 ) = afq
     !     store mechanical variables for the iteration loop ***************
     idt = imech + ismdat( i30+12 )
     cu( n07+19 ) = histq( idt ) * elp( i26+25 )
     cu( n07+20 ) = cosz( cu( n07+19 ) )
     cu( n07+21 ) = sinz( cu( n07+19 ) )
     imech = imech + 6 * ismdat( i30+11 )
     !     store rotor position 1 and 2 time-steps back in time**************
     cu( n07+22 ) = cu( n07+19 ) - omdt
     cu( n07+23 ) = cu( n07+22 ) - omdt
     cu( n07+24 ) = omega
     n07 = n07 + nwd
     if (iprsup .lt. 1) go to 412
     write (unit = lunit(6), fmt = 6709) ilk
6709 format (/, ' History matrix for generator no.', i5)
     in = i75 + 9
     do k = 1, 5
        in = in + 7
        n2 = in + 7
        n1 = in + 1
        write (unit = lunit(6), fmt = 6710) in, (elp(i), i = n1, n2)
     end do
6710 format (1x, 'New  row    ', i6, /, (1x, 7e17.9))
412  if( k1 .gt. 0 )    go  to  402
     j30 = j30 + 30
  end do
  if( iprsup .lt. 2 )    go to 2500
  ibk = 0
  do ik = 1, ilk
     ibl = ibk + 1
     ibk = ibk + 101
     write (unit = lunit(6), fmt = 6711) (elp(i), i = ibl, ibk)
6711 format (/, (1x, 6e20.12))
  end do
  !2400 continue
2500 return
end subroutine elecyy

!
! end of file over12.f90
!
