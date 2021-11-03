!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file ovr44c.f90
!

module ovr44c
  use linemodel
  use blkcom
  use deck44
  use com44
  use tracom
  implicit none

contains

  !
  ! subroutine redu44.
  !

  subroutine redu44 (a, b, n, m)
    implicit none
    !)    Elimination of variables m+1,...n in symmetric matrix a. A is
    !)    stored as triangle (1 element for 1.column,2 for 2.column etc.).
    !)    Result is reduced matrix in columns 1,...m in case of reduction
    !)    (m unequal 0) or negative inverse matrix in columns 1,...n in case
    !)    of inversion (m=0).
    real(8), intent(out) :: a(:)
    real(8), intent(out) :: b(:)
    integer(4), intent(in) :: m, n
    integer(4) :: i, i1, i2, ij, ik
    integer(4) :: j
    integer(4) :: k
    integer(4) :: l
    real(8) :: h1, h2
    real(8) :: w
    !  dimension a(1), b(1)
    !
    j = n + 1
    w = unity
    if (m .gt. 0) w = -w
    ij = n * j / 2
    if (iprsup  .ge.  4) write (unit = lunit6, fmt = 2692) n, m, ij,  (a(k), k = 1, ij)
2692 format (/, " At start of  'redu44' .       n       m      ij  ", /, 24x, 3i8, /, ' (a(k), k = 1, ij)  follow ....', /, (1x, 8e16.7))
3   j = j - 1
    if (j .eq. m) return
    h1 = a(ij)
    if (absz (h1) .gt. epsiln) go to 6421
    kill = 86
    lstat(19) = 6421
    lstat(18) = 51
    lstat(13) = n
    lstat(14) = m
    lstat(15) = j
    flstat(15) = epsiln
    flstat(16) = h1
    return
6421 h1 = -unity / h1
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
6      a(i) = a(i) + b(l) * h2
    end do
    if (k .lt. j) go to 4
    i = ik + j
    a(i) = b(k)
    go to 4
    !                                   end i-loop
7   i = ij
    do l = 1, j
       i = i + 1
8      a(i) = b(l)
    end do
    go to 4
    !                                   end k-loop
9   i = ij + k
    go to 5
  end subroutine redu44

  !
  ! subroutine wrte.
  !

  subroutine wrte (p, i2, i3, iflag, l, unit, lunit6)
    implicit none
    integer(4), intent(in) :: i2, i3, iflag
    integer(4), intent(in) :: l, lunit6
    real(8), intent(in) :: p(:)
    real(8), intent(in) :: unit
    !  dimension p(1),r(10)
    integer(4) :: i
    integer(4) :: j
    real(8) :: r(10)
    !
    j = 0
    do i = i2, i3
       j = j + 1
30     r(j) = p(i) * unit
    end do
    !  goto (1,2,3),iflag
    select case (iflag)
    case (1)
       write (unit = lunit6, fmt = 11) (r(i), i = 1, j)
       return

    case (2)
       write (unit = lunit6, fmt = 22) (r(i), i = 1, j)
       return

    case (3)
       write (unit = lunit6, fmt = 33) l, (r(i), i = 1, j)
       return
    end select
11  format ('0', 3x, 9e14.5)
22  format (4x, 9e14.5)
33  format ('0', i3, 9e14.5)
  end subroutine wrte

  !
  ! subroutine output.
  !

  subroutine output (metrik, p, z, switch, kmax, is, k2)
    implicit none
    integer(4), intent(in) :: is
    integer(4), intent(in) :: metrik
    integer(4), intent(in) :: k2
    integer(4), intent(in) :: kmax
    real(8), intent(in) :: p(:)
    real(8), intent(in) :: switch
    real(8), intent(in) :: z(:)
    !  dimension p(1), z(1)
    character(8) :: txtmi, txtkm, txtunt
    integer(4) :: i2, i3, icount
    integer(4) :: k, ki, kk
    integer(4) :: l
    real(8) :: check
    real(8) :: unit
    !
    data txtmi / 'miles ' /
    data txtkm / '  km  ' /
    if ( jpralt  .eq.  0 )   return
    ll1 = 1
    ll2 = 2
    ll3 = 3
    txtunt = txtmi
    unit = 1.0d0
    if (metrik .eq. 1) txtunt = txtkm
    if (metrik .eq. 1) unit = 5280.0d0 * 12.0d0 * 2.54d0 / 100000.0d0
    check = switch
    write (unit = lunit6, fmt = 66)
66  format ('0')
    !  go to (305, 306, 307, 308, 332, 333, 336, 337, 340, 341), is
    select case (is)
    case (1)
       write (unit = lunit6, fmt = 301) txtunt
301    format ('0', 12x, 'Inverted capacitance matrix (daraf-', a4, ')')

    case (2)
       write (unit = lunit6, fmt = 302) txtunt
302    format ('0', 14x, 'Inverted susceptance matrix (Ohm-', a4, ')')

    case (3)
       write (unit = lunit6, fmt = 303) txtunt
303    format ('0', 21x, 'Capacitance matrix (Farad/',a4, ')')
       unit = 1.0d0 / unit

    case (4)
       write (unit = lunit6, fmt = 304) txtunt
304    format ('0', 23x, 'Susceptance matrix (mho/', a4, ')')
       unit = 1.0d0 / unit

    case (5)
       write (unit = lunit6, fmt = 330) txtunt
330    format ('0', 16x, 'Inverted impedance matrix (mho-', a4, ')')

    case (6)
       write (unit = lunit6, fmt = 331) txtunt
331    format ('0', 25x, 'Impedance matrix (Ohm/', a4, ')')
       unit = 1.0d0 / unit

    case (7)
       write (unit = lunit6, fmt = 334)
334    format ('0', 19x, 'Transfer admittance matrix (mhos)')

    case (8)
       write (unit = lunit6, fmt = 335)
335    format ('0', 16x, 'Twice shunt admittance matrix (mhos)')

    case (9)
       write (unit = lunit6, fmt = 338)
338    format ('0', 20x, 'Transfer impedance matrix (Ohms)')

    case (10)
       write (unit = lunit6, fmt = 339)
339    format ('0', 11x, 'One half of shunt impedance matrix (Ohms)')
    end select
    !5 go to (320,321,322),k2
5   select case (k2)
    case (1)
       write (unit = lunit6, fmt = 309)
309    format ('+', 53x, 'For the system of physical conductors')

    case (2)
       write (unit = lunit6, fmt = 310)
310    format ('+', 53x, 'For the system of equivalent phase conductors')
       go to 6

    case (3)
       write (unit = lunit6, fmt = 311)
311    format ('+', 53x, 'For the symmetrical components of the equivalent phase conductors')
    end select
6   if (k2 .eq. 3) go to 3
    write (unit = lunit6, fmt = 312)
312 format (' ', 30x, 'Rows and columns proceed in same order as sorted input')
4   k = 0
    if (is .eq. 8) write (unit = lunit6, fmt = 350)
    if (is .eq.10) write (unit = lunit6, fmt = 350)
350 format (' Sum of two equal shunt admittances at both terminals or its inverse, printed to conform to transients program input format')
    kk = 0
    ki = 0
1   k = k + 1
    if (k .gt. kmax) return
    icount = 0
    i2 = ki + 1
    i3 = i2 + 8
    ki = ki + k
7   if (i3 .gt. ki) i3=ki
    if (icount .eq. 0) go to 8
    call wrte (p(1 :), i2, i3, ll1, l, unit, lunit6)
10  if (check .gt. 0.0d0) call wrte (z(1 :), i2, i3, ll2, l, unit, lunit6)
    if (i3 .eq. ki) go to 1
    i2 = i3 + 1
    i3 = i2 + 8
    go to 7
8   icount = 1
    if (k2 .eq. 3) go to 20
    l = k
21  call wrte (p(1 :), i2, i3, ll3, l, unit, lunit6)
    go to 10
3   if (kmax .eq. 2) go to 400
    write (unit = lunit6, fmt = 313)
313 format (' ', 25x, 'Rows proceed in sequence 0,1,2, 0,1,2 etc. and columns proceed in sequence 0,2,1, 0,2,1 etc.')
    go to 4
400 write (unit = lunit6, fmt = 401)
401 format (' ', 38x, 'This is a two-phase line. Rows and columns proceed in sequence 0,1')
    if (is .le. 4) check = -1.0d0
    go to 4
20  l = kk
    kk = kk + 1
    if (kk .eq. 3) kk = 0
    go to 21
  end subroutine output

  !
  ! subroutine cxred2.
  !

  subroutine cxred2 (a, c, b, d, n, m)
    implicit none
    !)    elimination of variables m+1,...n in symmetric complex matrix with
    !)    a=real part, c=imaginary part. a and c are
    !)    stored as triangle (1 element for 1.column,2 for 2.column etc.).
    !)    result is reduced matrix in columns 1,...m in case of reduction
    !)    (m unequal 0) or negative inverse matrix in columns 1,...n in case
    !)    of inversion (m=0).
    real(8), intent(out) :: a(:)
    real(8), intent(out) :: b(:)
    real(8), intent(out) :: c(:)
    real(8), intent(out) :: d(:)
    integer(4), intent(in) :: m, n
    !  dimension a(1), c(1), b(1), d(1)
    integer(4) :: i, i1, i2, ij, ik
    integer(4) :: j
    integer(4) :: k
    integer(4) :: l
    real(8) :: g1, g2
    real(8) :: h1, h2
    real(8) :: w
    real(8) :: x
    real(8) :: y
    !
    j = n + 1
    w = 1.0d0
    if (m .gt. 0) w = -w
    ij = n * j / 2
3   j = j - 1
    if (j .eq. m) return
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
  end subroutine cxred2

  !
  ! subroutine cominv.
  !

  subroutine cominv (a, b, m, freq)
    implicit none
    !   This subroutine performs inversion of a complex matrix
    integer(4), intent(in) :: m
    real(8), intent(in) :: a(:)
    real(8), intent(out) :: b(:)
    real(8), intent(in) :: freq
    !  dimension a(1), b(1), ij(50)
    integer(4) :: i, ij(50)
    integer(4) :: j
    integer(4) :: k
    integer(4) :: l
    integer(4) :: n1, n2, n3, n4, n22, nph2, nphpi2
    real(8) :: d9
    real(8) :: epspv2
    real(8) :: u
    real(8) :: v
    !
    epspv2 = 1.0d-16
    do i = 1, m
110    ij(i) = 0
    end do
    nph2 = m + m
    n22 = 2 * m * m
    nphpi2 = nph2 + 2
    do j=1, n22
120    b(j) = a(j)
    end do
    l = 0
130 l = l + 1
    if (l .gt. m) return
    t = 0.0d0
    n1 = 1
    do j = 1, m
       if (ij(j) .gt. 0) go to 140
       u = b(n1) * b(n1) + b(n1 + 1) * b(n1 + 1)
       if (u .le. t) go to 140
       t = u
       n2 = j
       k = n1
140    n1 = n1 + nphpi2
    end do
    ij(n2) = 1
    if (t .gt. epspv2) go to  145
    kill = 199
    flstat(15) = freq
    flstat(13) = t
    flstat(14) = epspv2
    lstat(13) = l
    lstat(14) = m
    lstat(19) = 145
    return
145 u = b(k) / t
    t = -b(k + 1) / t
    b(k) = u
    b(k + 1) = t
    n3 = (n2 - 1)*nph2 + 1
    n2 = 2 * n2 - 1
    do i = 1, nph2, 2
       if (i .eq. n2) go to 160
       n1 = i + (n2 - 1) * m
       v = u * b(n1) - t * b(n1 + 1)
       d9 = u * b(n1 + 1) + t * b(n1)
       do j = 1, n22, nph2
          if (j .eq. n3) go to 150
          k = i + j
          n4 = j + n2
          b(k-1) = b(k - 1) - b(n4 - 1) * v + b(n4) * d9
          b(k) = b(k) - b(n4 - 1) * d9 - b(n4) * v
150    end do
160 end do
    k = n2
    do i = 1, nph2, 2
       if (i .eq. n2) go to 170
       n4 = n3 + i
       v = b(n4 - 1) * u - b(n4) * t
       b(n4) = b(n4 - 1) * t + b(n4) * u
       b(n4 - 1) = v
       v = b(k + 1) * t - b(k) * u
       b(k + 1) = -b(k) * t - b(k + 1) * u
       b(k) = v
170    k = k + nph2
    end do
    go to 130
  end subroutine cominv

  !
  ! subroutine modal.
  !

  subroutine modal (array, xwc, xwy, yzr, yzi, tii, tir, tvi, tvr, er, ei, theta2, xtir, xtii, zsurge, dummi, dummr, tixf, work1, freq, m, iw, dist, metrik, fmipkm, ndim, ntri, nsqr2, itrnsf, kfull, mrr, nrp, ntol, conduc)
    implicit none
    integer(4), intent(in) :: iw
    integer(4), intent(in) :: m
    integer(4), intent(in) :: metrik
    integer(4), intent(in) :: ndim
    integer(4), intent(in) :: nsqr2
    integer(4), intent(in) :: ntri
    real(8), intent(in) :: array(:)
    real(8), intent(out) :: dist
    real(8), intent(out) :: dummi(ndim)
    real(8), intent(out) :: dummr(ndim)
    real(8), intent(out) :: ei(ndim)
    real(8), intent(out) :: er(ndim)
    real(8), intent(in) :: fmipkm
    real(8), intent(in) :: freq
    real(8), intent(out) :: theta2(ndim)
    real(8), intent(out) :: tii(ndim, ndim)
    real(8), intent(out) :: tir(ndim, ndim)
    real(8), intent(out) :: tixf(nsqr2)
    real(8), intent(out) :: tvi(ndim, ndim)
    real(8), intent(out) :: tvr(ndim, ndim)
    real(8), intent(out) :: work1(nsqr2)
    real(8), intent(out) :: xtii(ndim)
    real(8), intent(out) :: xtir(ndim)
    real(8), intent(out) :: xwc(ntri)
    real(8), intent(out) :: xwy(ntri)
    real(8), intent(out) :: yzi(ndim, ndim)
    real(8), intent(out) :: yzr(ndim, ndim)
    real(8), intent(out) :: zsurge(ndim)
    character(8) :: text1, text2, text3, text4, text5, text6
    character(8) :: text7, text8, text9, text10
    !  dimension array(1)
    !  dimension xwc(ntri), xwy(ntri), yzi(ndim, ndim), yzr(ndim, ndim)
    !  dimension tii(ndim, ndim), tir(ndim, ndim), tvi(ndim, ndim)
    !  dimension er(ndim), ei(ndim), theta2(ndim), tvr(ndim, ndim)
    !  dimension xtir(ndim), xtii(ndim), zsurge(ndim), dummi(ndim)
    !  dimension dummr(ndim), tixf(nsqr2), work1(nsqr2)
    !  dimension ping(200), iseq(15), kmax(15)
    !  dimension pp1(20, 20), pp2(20, 20), ee1(15), ee2(15)
    !  dimension tempr(20, 20), tempi(20, 20)
    integer(4) :: i, ibk, ierror, ig, ij, iseq(15), itrnsf
    integer(4) :: j, jj
    integer(4) :: k, kfull, ki1, kk, kk1, kmax(15), kthl
    integer(4) :: l, ll
    integer(4) :: mm, mrr
    integer(4) :: n, n5, n9sq, n12, nrp
    integer(4) :: ntol
    real(8) :: alpha
    real(8) :: b, beta
    real(8) :: c, cc, ci, cl, conduc, cr
    real(8) :: d1, d9, d13, d14, d55, d56, da, date1, dd, di, distm, dr, dumm1, dv
    real(8) :: ea, ee1(15), ee2(15), em
    real(8) :: fnorm
    real(8) :: g, gamma
    real(8) :: ping(200)
    real(8) :: pp1(20, 20), pp2(20, 20)
    real(8) :: tclock, tempi(20, 20), tempr(20, 20), theta
    real(8) :: vmode
    real(8) :: xa, xb
    real(8) :: ya, ychara, ycharm, ym
    real(8) :: zchari, zcharr, zi, zr
    !
    data text2  / 'a' /
    data text3  / 'b' /
    data text4  / 'c' /
    data text5  / 'd' /
    data text6  / 'e' /
    data text7  / 'f' /
    data text8  / 'g' /
    data text9  / 'h' /
    data text10 / 'i' /
    data iseq   / 15 * 0 /
    if (kexact .eq. 88333) go to 100
    if (itrnsf .eq. 1  .and. nfreq .eq. 1) return
    if (itrnsf .eq. 1  .and. nfreq .eq. 2) return
    if (lastov .eq. 39 .and. nfreq .eq. 2) return
100 if ( iprsup .ge. 1 ) write (lunit6,*) ' top of modal.   ndim =',  ndim
    ll0 = 0
    mm=m*(m+1)/2
    c=unity
    if (iprsup .ge. 1 ) write (lunit6, 300) m, mm, (p(i),z(i), i=1, mm)
300 format (' At the top of modal, m and mm are', 2i10, /, ' (p(i), z (i), i = 1, mm) are', /, (1x, 8e15.6))
    d13 = twopi * freq
    if ( iw .eq. 1 ) c = c / d13
    do i=1,mm
       xwc(i) = xwc(i) * c
27     xwy(i)=-xwc(i)
    end do
    call redu44 (xwy(1 :), workr1(1 :), m, ll0)
    !    Const. [t], Marti setup will bypass the exact diagonalization
    !    at each looping frequency
    if (nfreq .ne. 1  .and.  lastov .eq. 39 .and. itrnsf .ne. 1) go to 2520
    if (lastov .eq. 39 .and. nfreq .eq. 3) go to 7030
    if (kexact .eq. 88333) go to 7030
    write (unit = lunit6, fmt = 45) freq
    write (unit = lunit6, fmt = 44)
    if (metrik .eq. 1) go to 7029
    write (unit = lunit6, fmt = 65)
    go to 7030
7029 continue
    write (unit = lunit6, fmt = 7031)
7030 continue
45  format (//, ' Modal parameters at freq = ', e13.5, ' Hz')
    ! ***** create y * z matrix from array from lcp main
    k = 0
3   k = k + 1
    do j = 1, m
       !     n=max0z(j,k)
       n = j
       if (n .lt. k) n = k
       n = n * (n - 3) / 2 + j + k
       er(j) = p(n)
50     ei(j) = z(n)
    end do
    ! **** the k-th column of r+xwl is stored
    i = 0
2   i = i + 1
    cr = 0.0d0
    cl = 0.0d0
    do j = 1, m
       !     n=max0z(j,i)
       n = j
       if (n .lt. i) n = i
       n = n * (n - 3) / 2 + j + i
       cc = xwy(n)
       if (lastov .eq. 1) conduc = 0.0d0
       cr = cr + cc * er(j) + conduc * ei(j)
1      cl = cl - cc * ei(j) + conduc * er(j)
    end do
    ! **** the element of row i and column k of yz is obtained
    yzr(i, k) = cl
    yzi(i, k) = cr
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 350) i, k, yzr(i, k), yzi(i, k)
350 format ('       i       k       yzr(i,k)       yzi(i,k)', /, 2i8, 2e15.6)
    if (i .lt. m)go to 2
    ! **** yz is obtained in order of (1,1),(2,1),...,(1,2),(2,2),columnwise
    if (k.lt.m)go to 3
    ! *** find eigenvalues + eigenvectors ***
    if ( kexact .eq. 88333 )  go to 6999
    if ( itrnsf .eq. -1 )  go to 459
    if ( lastov .eq. 1 .or. itrnsf .ne. 1)  go to 7011
    ! $$$$  change to zy-matrix i.e. transformation matrix is tv   $$$$
6999 do i = 1, m
       do j = 1, m
          tempr(i, j) = yzr(j, i)
          tempi(i, j) = yzi(j, i)
       end do
7001   continue
    end do
    do i = 1, m
       do j = 1, m
          yzr(i, j) = tempr(i, j)
          yzi(i, j) = tempi(i, j)
       end do
7008   continue
    end do
    call dceign(yzr, yzi, tvi, tvr, er, ei, m, m, ierror, ll1, ll0, lunit6, iprsup, ndim)
    !      write (*,*)(er(i), i=1,m)
    !      write (*,*)(ei(i), i=1,m)
    go to 7012
7011 call dceign(yzr, yzi, tii, tir, er, ei, m, m, ierror, ll1, ll0, lunit6, iprsup, ndim)
    !      write (*,*)(er(i), i=1,m)
    !      write (*,*)(ei(i), i=1,m)
    go to 7072
7012 kthl = 1
    ping(kthl) = alog1z (freq)
    d55 = 1.0d0 / freq
    d56 = 1.0d0 / sqrtz (freq)
    do i = 1, m
       em = sqrtz (er(i) * er(i) + ei(i) * ei(i))
       ea = atan2z (ei(i), er(i))
       p(i) = sqrtz (em)
       z(i) = ea / 2
       if (kexact .eq. 88333) go to 5600
       cc = sqrtz (em) * cosz (ea / 2.0d0)
       d14 = sqrtz (em) * sinz (ea / 2.0d0)
       vmode = d13 / d14
       ! $$$$  put data in ping vector  $$$$
       ping(kthl+1) = em*d55
       ping(kthl+2) = ea/(twopi/360.)
       ping(kthl+3) = vmode
       ping(kthl+4) = cc*d56
       kthl = kthl + 4
5600 end do
    if ( iprsup .lt. 1 )  go to 5602
    write (lunit6,*) ' tv before unwind. '
    do i = 1, m
       write (lunit6,59) ( tvr(i,j), j=1,m )
       write ( lunit6,59) ( tvi(i,j), j=1,m )
2411 end do
5602 if (kexact .eq. 88333) go to 5620
    call unwind (ping, kthl, mrr, nrp, ntol, iseq)
    if ( iprsup .lt. 1 )  go to 5605
    write (lunit6, *) ' iseq after unwind are, ', (iseq(i),i=1,m)
    write (lunit6, *) ' ping after unwind are, ', (ping(i),i=1,kthl)
5605 continue
    ! $$$$  begin the process of eigenvectors  $$$$
    do i=1,m
       ee1(i)=p(i)
       ee2(i)=z(i)
       do j=1,m
          ! $$$$  use tv  $$$$
          pp1(i,j)=tvr(i,j)
          pp2(i,j)=tvi(i,j)
5613   end do
    end do
    do i=1,m
       p(i)=ee1(iseq(i))
       z(i)=ee2(iseq(i))
       do j=1,m
          ! $$$$ use tv  $$$$
          tvr(i,j)=pp1(i,iseq(j))
          tvi(i,j)=pp2(i,iseq(j))
5612   end do
    end do
    ! $$$$ new code to get ti from tv-t $$$$
5620 ll=1
    do i=1,m
       do j=1,m
          tixf(ll)=tvr(i,j)
          tixf(ll+1)=tvi(i,j)
          ll=ll+2
7003   end do
7002 end do
    call cominv (tixf(1 :), work1(1 :), m, freq)
    ll = 1
7766 if (iprsup .ge. 3) write (unit = lunit6, fmt = *) ' Before do 7004.  m, ndim, freq =', m, ndim, freq
    do j = 1, m
       do i = 1, m
          tir(i, j) = work1(ll)
          tii(i, j) = work1(ll+1)
          ll = ll + 2
7005   end do
7004 end do
    if (kexact .eq. 88333) go to 7072
    do i = 1, m
       do j = 1, m
          pp1(i, j) = sqrtz ((tir(i, j)) ** 2 + (tii(i, j)) ** 2)
          pp2(i, j) = atan2z (tii(i, j), tir(i, j))
5621   end do
5611 end do
    !
    if (ntol .ne. 1) go to 5812
    do j = 1, m
       kmax(j) = 1
       do i = 1, m
          if (pp1(i, j) .le. pp1(kmax(j), j)) go to 5802
          kmax(j) = i
5802   end do
5801 end do
    write (unit = lunit9) (kmax(j), j = 1, m)
5812 do j = 1, m
       if (pp1(kmax(j), j) .eq. 0.0d0) go to 5614
       dv = 1.0d0 / pp1(kmax(j), j)
       da = pp2(kmax(j), j)
       if (iprsup .ge. 3) write (unit = *, fmt = *) 'in do loop 5614, j=, kmax(j)=,dv=,da=', j, kmax(j), dv, da
       do i = 1, m
          if (iprsup .ge. 3) write (unit = *, fmt = *) 'in do loop 5624, i, j, pp1(i,j), pp2(i,j)', i, j, pp1(i, j), pp2(i, j)
          pp1(i, j) = pp1(i, j) * dv
          pp2(i, j) = pp2(i, j) - da
          !     *** to keep angles within principal value region
          if (pp2(i, j) .gt. twopi / 2.0d0) pp2(i, j) = pp2(i, j) - twopi
          if (pp2(i, j) .lt.-twopi / 2.0d0) pp2(i, j) = pp2(i, j) + twopi
          if (iprsup .ge. 3) write (unit = *, fmt = *) 'Just before 5624, i, j, pp1(i,j), pp2(i,j)', i, j, pp1(i, j), pp2(i, j)
5624   end do
5614 end do
    !!    ** fix the error of using  ti as tv  **
    do i = 1, m
       do j = 1, m
          tir(i, j) = (pp1(i, j)) * cosz (pp2(i, j))
          tii(i, j) = (pp1(i, j)) * sinz (pp2(i, j))
6001   end do
    end do
7120 if (iprsup .ge. 1) write (unit = lunit6, fmt = 400) freq, ((tir(i, j), tii(i, j), j = 1, m), i = 1, m)
400 format (' At ', e12.5, 'Hz, eigenvectors tir and tii after scaling are', /, (1x, 8e15.6))
7072 if (ierror .eq. 0) go to 463
    kill = 221
    lstat(19) = 72
    lstat(14) = ierror
    go to 9200
459 do j = 1, m
       do i = 1, m, 3
          ij = i + 2
          call cimage
          read (unit = abuff, fmt = 469) (tir(j, ibk), tii(j, ibk), ibk = i, ij)
469       format (6e13.0)
462    end do
    end do
    !     !   $$$$ skip the rotation and normalization  for freq-dep [t] ??
463 if (lastov .eq. 39 .and. itrnsf .eq. 1 .or. kexact .eq. 88333) go to 2500
    if (itrnsf .eq. -9 .or. itrnsf .eq. -1) go to 464
    if (iprsup .lt. 4) go to 88
    write (unit = lunit6, fmt = 1462)
1462 format (' Current transformation matrix before zeroing the imaginary part:')
    write (unit = lunit6, fmt = 39)
    do  k = 1, m
       write (unit = lunit6, fmt = 9) (tir(k, j), j = 1, m)
1472 end do
    write (unit = lunit6, fmt = 60)
    do k = 1, m
       write (unit = lunit6, fmt = 9) (tii(k, j), j = 1, m)
1482 end do
88  continue
    do j = 1, m
       cr = 0.0d0
       fnorm = 0.0d0
       do  i = 1, m
          fnorm = fnorm + (tir(i, j) ** 2 - tii(i, j) ** 2)
          cr = cr + tir(i, j) * tii(i, j)
466    end do
       cr = -2.0d0 * cr
       theta = atan2z (cr, fnorm) / 2.0d0
       cr = cosz (theta)
       cl = sinz (theta)
       do i = 1, m
          tir(i, j) = tir(i, j) * cr - tii(i, j) * cl
467       tii(i, j) = 0.0d0
       end do
465 end do
464 do i = 1, m
       fnorm = 0.0d0
       do j = 1, m
26        fnorm = fnorm + tir(j, i) ** 2 + tii(j, i) ** 2
       end do
       fnorm = 1.0d0 / sqrtz (fnorm)
       do j = 1, m
          tii(j, i) = tii(j, i) * fnorm
          tir(j, i) = tir(j, i) * fnorm
5      end do
    end do
    ! $$$$  calculation of tv from ti  $$$$
2500 ll = 1
    do i = 1, m
       do j = 1, m
          tixf(ll) = tir(i, j)
          tixf(ll + 1) = tii(i, j)
          ll = ll + 2
6003   end do
6002 end do
    call cominv (tixf(1 :), work1(1 :), m, freq)
    ll = 1
    do j = 1, m
       do i = 1, m
          tvr(i, j) = work1(ll)
          tvi(i, j) = work1(ll + 1)
          ll = ll + 2
6005   end do
6004 end do
    !!    ** now, we have both ti and tv **
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 403) ((tvr(i, j), tvi(i, j), j = 1, m), i = 1, m)
403 format (' Eigenvectors tvr and tvi are', /, (1x, 8e15.6))
    if (lastov .eq. 39 .and. itrnsf .ne. 1) go to 166
    if (lastov .ne. 1) go to 2520
    write (unit = lunit7, fmt = 8769) tclock, date1
8769 format ("!    Punched output of K. C. Lee's line which began at ", 2x, 2a4, 2x, 2a4)
    write (unit = lunit7, fmt = 8770) freq
8770 format ( "c  ***** Untransposed K. C. Lee line segment calculated at ", 2x, e10.3,  ' Hz. *****')
    rewind lunit2
    n5 = kfull + nfreq + 1
    do n12 = 1, n5
       read (unit = lunit2, fmt = 8771, end = 8775) (texta6(i), i = 1, 14)
8771   format (13a6, a2)
       write (unit = lunit7, fmt = 8772) (texta6(i), i = 1, 13)
8772   format ('c ', 13a6)
8774 end do
8775 write (unit = lunit7, fmt = 28)
28  format ('$vintage, 1')
    ! ****** to get modal y *********
2520 do i = 1, m
       do j = 1, m
          !     $$$ now, using tv to get ymode $$$
          xtir(j) = tvr(j, i)
          xtii(j) = tvi(j, i)
41     end do
       call mult (xwy, xtir, dummi, m, ll0)
       b = 0.0d0
       call mult (xwy, xtii, dummr, m, ll0)
       ! *** xwy  is j(wc) ******
       do j = 1, m
          dummr(j) = -dummr(j)
3141   end do
       do j = 1, m
          dummi(j) = dummi(j) + conduc * xtii(j)
          dummr(j) = dummr(j) + conduc * xtir(j)
3151   end do
       g = 0.0d0
       do ig = 1, m
          c = dummr(ig)
          d1 = dummi(ig)
          xa = xtir(ig)
          xb = xtii(ig)
          b = b + xa * d1 + xb * c
43        g = g + xa * c - xb * d1
       end do
       if (iprsup .ge. 1) write (unit = lunit6, fmt = *) ' Modal admittance ym for mode ', i, g, b
44     format (//, ' mode   resistance  reactance  susceptance surge impedance(Ohm)          velocity  attenuation')
65     format ('          Ohm/mile    Ohm/mile   S/mile        real     imag       lossless     mile/sec  neper/mile')
7031   format ('          Ohm/km      Ohm/km     S/km         real     imag      lossless     km/sec     neper/km')
33     format (i5, 3x, 9e12.5)
       ! to get rotation for  y=0+xwc
       ya = atan2z (b, g)
       theta = pi * onehaf - ya
       theta2(i) = -theta * onehaf
       dumm1 = sqrtz (g * g + b * b)
       continue
       ! **** to get zmodal, zsurge, vmode from ymodal & eigenvalues ****
       if ( lastov .eq. 1 )  go to 2900
       !!  ** new coding **
       if ( itrnsf .ne. 1 .and. kexact .ne. 88333 )  go to 7041
       gamma = p(i)
       ea = z(i)
1232   ycharm = dumm1 / gamma
       ychara = ya - ea
       !  ***********************************
       !     comment the following two lines if ych is to be fitted
       !      ycharm = 1./ycharm    !  zch = 1 / ych
       !      ychara = - ychara
       !   ********************************
       alpha = gamma * cosz(ea)
       beta = gamma * sinz(ea)
       if ( kexact .ne. 88333 )  go to 7033
       if ( metrik .eq. 1 )  dist = dist * fmipkm
       alpha = alpha * dist
       beta = beta * dist
       go to 155
7033   alpha = alpha * fmipkm
       beta = beta * fmipkm
       go to 155
7041   zr = 0.0
       zi = 0.0
       kk1 = 2 * m
       ki1 = 2 * i - kk1 - 1
       do jj = 1, m
          kk = kk1 * jj + ki1
          do l = 1, m
             cr = 0.0
             ci = 0.0
             do j = 1, m
                !     n = max0z(j,l)
                n = j
                if ( n .lt. l )  n = l
                n = n * (n-3) / 2 + j + l
                er(j) = p(n)
                ei(j) = z(n)
                k = kk1 * j + ki1
                cr = cr + er(j)*tixf(k) - ei(j)*tixf(k+1)
                ci = ci + er(j)*tixf(k+1) + ei(j)*tixf(k)
2600         end do
             dummr(l) = cr
             dummi(l) = ci
2700      end do
          zr = zr + tixf(kk)*dummr(jj) - tixf(kk+1)*dummi(jj)
          zi = zi + tixf(kk)*dummi(jj) + tixf(kk+1)*dummr(jj)
2800   end do
2899   dr = zr * g - zi * b
       di = zr * b + zi * g
       em = sqrtz( dr*dr + di*di )
       gamma = sqrtz( em )
       ea = atan2z( di, dr ) / 2.
       ym=sqrtz( g**2 + b**2 )
       ya=atan2z( b, g )
       go to 1232
2900   g=ei(i)/dumm1
       b=-er(i)/dumm1
58     format (2x, 10e11.4)
2950   zsurge(i)=sqrtz(b/dumm1)
       cc=sqrtz(g**2+b**2)
       theta=atan2z(-g,b)*onehaf
       zcharr=sqrtz(cc/dumm1)
       zchari=zcharr*sinz(theta)
       zcharr=zcharr*cosz(theta)
       theta=theta+pi*onehaf
       cc=sqrtz(cc*dumm1)
       d9 = cc * sinz(theta)
       vmode = d13 / d9
       cc=cc*cosz(theta)
       if (metrik .eq. 0)  go to 137
       g = g * fmipkm
       b = b * fmipkm
       dumm1 = dumm1 * fmipkm
       vmode = vmode / fmipkm
       cc = cc * fmipkm
       d9 = d9 * fmipkm
137    write (lunit6,33)i,g,b,dumm1,zcharr,zchari,zsurge(i),vmode,cc
       distm = -dist
       if ( m  .lt.  10     .and.     lastov  .eq.  1 ) write (lunit7,140) i, brname(2*i-1), brname(2*i), g,zsurge(i),vmode,distm,m
140    format ('-', i1, 2a6, 12x, 4e12.5, ' 1', 2x, i1)
       if ( m .ge. 10  .and.  lastov .eq. 1)  go to 141
       go to 40
141    if ( m .eq. 10 )  text1 = text2
       if ( m .eq. 11 )  text1 = text3
       if ( m .eq. 12 )  text1 = text4
       if ( m .eq. 13 )  text1 = text5
       if ( m .eq. 14 )  text1 = text6
       if ( m .eq. 15 )  text1 = text7
       if ( m .eq. 16 )  text1 = text8
       if ( m .eq. 17 )  text1 = text9
       if ( m .eq. 18 )  text1 = text10
       if ( i .le. 9 ) write (lunit7,143) i, brname(2*i-1), brname(2*i), g,zsurge(i),vmode,distm,text1
143    format ('-', i1, 2a6, 12x, 4e12.5, ' 1', 2x, a1)
       if ( i  .gt.  9 ) write (lunit7,145) i, brname(2*i-1), brname(2*i), g,zsurge(i),vmode,distm,text1
145    format (i2, 2a6, 12x, 4e12.5, ' 1', 2x, a1)
       go to 40
155    write (lunit9) d13,ycharm,ychara,alpha,beta
       if ( iprsup .ge. 1 ) write (lunit6, 158) d13, i, ycharm,ychara,alpha,beta
158    format (' at ', e12.5,' Hz,ycharm,ychara,alpha,beta for mode',  i5, ' are', 5x, 4e12.5)
       if ( kexact .eq. 88333 )  go to 40
       if ( itrnsf .ne. 1 )  go to 40
       write (lunit9) (pp1(j,i),pp2(j,i), j=1,m )
40  end do
    if ( kexact .ne. 88333 )  go to 119
    ll = 1
    do j = 1, m
       do ij = 1, m
          tixf(ll) = tir(ij,j)
          work1(ll) = tii(ij,j)
          ll = ll + 1
115    end do
    end do
    n9sq = m * m
    write (lunit9) ( tixf(k),k=1,n9sq), (work1(k),k=1,n9sq)
    go to 9900
119 if ( lastov .eq. 1 )  write (lunit7,240)
240 format ('$vintage, 0')
    if ( nfreq .ne. 1  .and.  lastov .eq. 39 )  go to 9900
    do i=1,m
       c=cosz(theta2(i))
       d1=sinz(theta2(i))
       do j=1,m
          cc=tir(j,i)
          dd=tii(j,i)
          tir(j,i)=cc*c-dd*d1
          tii(j,i)=cc*d1+dd*c
37     end do
    end do
    ! ***** end of rotation of ti matrix for k.c.lee's const.param.*******
39  format (//, '      Real components, row by row')
166 write (lunit6,66)
66  format (////, ' Eigenvector matrix ti for current transformation i (phase) = ti * i(mode)')
    write (lunit6,39)
9   format (2x, 6e12.5)
    do k=1,m
       write (lunit6,9)(tir(k,j),j=1,m)
59     format (6f12.8)
57  end do
    write (lunit6,60)
60  format (//, '     Imaginary components, row by row')
    do k=1,m
       write (lunit6,9)(tii(k,j),j=1,m)
    end do
    do i =1, m
       if ( lastov .eq. 39 )  go to 161
       write (lunit7,59) ( tir(i,j), j=1,m )
       write (lunit7,59) ( tii(i,j), j=1,m )
       go to 1204
161    write (lunit9)   (tir(i,j), tii(i,j), j=1, m )
1204 end do
    if ( lastov .eq. 39 )  go to 9900
    ! to get zsurge matrix in phase domain
75  do i=1,m
       c = 1./zsurge(i)
       do j=1,m
80        yzr(i,j)=tir(j,i) * c
       end do
    end do
    n=0
    do i=1,m
       do k=1,i
          c = 0.0
          do j=1,m
81           c=c-tir(i,j)*yzr(j,k)
          end do
          n=n+1
82        xwc(n)=c
       end do
    end do
    call redu44 (xwc(1 :), workr1(1 :), m, ll0)
    if (kill .ne. 0) go to 9200
    write (unit = lunit6, fmt = 83)
83  format (/,' Phase domain zsurge (real; the imag. of ti ignored)')
    n = 1
    do i = 1, m
       k = n + i - 1
       write (unit = lunit6, fmt = 9) (xwc(j), j = n, k)
84     n = n + i
    end do
    go to 9900
9200 lstat(18) = nchain
    lastov = nchain
    nchain = 51
9900 return
  end subroutine modal

  !
  ! subroutine guts44.
  !

  subroutine guts44 (array, xwc, xwy, yzr, yzi, tii, tir, tvi, tvr, er, ei, theta2, xtir, xtii, zsurge, dummi, dummr, tixf, work1, ndim, ntri,nsqr2)
    use linemodel
    use deck44
    use volpri
    use tracom
    use indcom
    use movcop
    implicit none
    integer(4), intent(in) :: ndim
    integer(4), intent(in) :: nsqr2
    integer(4), intent(in) :: ntri
    real(8), intent(in) :: array(:)
    real(8), intent(out), dimension(ndim) :: dummi
    real(8), intent(out), dimension(ndim) :: dummr
    real(8), intent(out), dimension(ndim) :: ei
    real(8), intent(out), dimension(ndim) :: er
    real(8), intent(out), dimension(ndim) :: theta2
    real(8), intent(out), dimension(ndim, ndim) :: tii
    real(8), intent(out), dimension(ndim, ndim) :: tir
    real(8), intent(out), dimension(nsqr2) :: tixf
    real(8), intent(out), dimension(ndim, ndim) :: tvi
    real(8), intent(out), dimension(ndim, ndim) :: tvr
    real(8), intent(out), dimension(nsqr2) :: work1
    real(8), intent(out), dimension(ndim) :: xtii
    real(8), intent(out), dimension(ndim) :: xtir
    real(8), intent(out), dimension(ntri) :: xwc
    real(8), intent(out), dimension(ntri) :: xwy
    real(8), intent(out), dimension(ndim, ndim) :: yzi
    real(8), intent(out), dimension(ndim, ndim) :: yzr
    real(8), intent(out), dimension(ndim) :: zsurge
    character(8) :: text1, text2, fmetrc, englis, bufsem(14)
    character(8) :: text3, text4, text5, text6, text7
    character(8) :: text8, text9
    integer(4) :: i, i1, i2, i3, i4, i5, icount, idebug, iii, ik, imax, imodal, ip, iprint, ips
    integer(4) :: isec, isegm, isn, itrnsf, iw, ix
    integer(4) :: j, jprmat(16), j1516, j1, j2, j2out, j4out, j3, j4, j5, j56
    integer(4) :: j5out, j6, j6out, j7, j8, j8out, j9, j9out, j10, j11, j12, j13
    integer(4) :: j14, j15, j16, j17, jb, jj, jm, jp, jspecl
    integer(4) :: k, k2, k5, kcir2, kcirct, kfull, kk, kkk, kount, kp, ktot
    integer(4) :: l, l1, l2, l3, l4, l5save, ldisfr, liu
    integer(4) :: m, m5, metrik, mfrqpr, moon, mrr, mspedb, mtrnsp, muntrn, mutual
    integer(4) :: n1, n2, n5, n8, n9sq, n12, n55, nbundl, nfqpl1, nmode, nrp, ntol
    real(8) :: alpha1, alphao, angl
    real(8) :: bet1, beto
    real(8) :: c1, c2, ccoup, cdiag, coff, conduc, corr, cpos, cs, czero
    real(8) :: d1, d2, d3, d4, d8, d9, d9r, d11, d13, dangl, date1, deltad, deltam
    real(8) :: deltap, deltaq, dist, distkm, distm, distsv, dj, dx
    real(8) :: error
    real(8) :: f1, f2, factor, fdecad, finpcm, fmipkm, freq, fspac, ftpm
    real(8) :: g1, gmdj, gmode
    real(8) :: h1, h2
    real(8) :: phi, pkkk, prop1, propo
    real(8) :: r1, radius, rcoup, rdiag, rearth, rj, rm, roff, rpos, rpos1, rtio
    real(8) :: rzero, rzero1
    real(8) :: s, s1, s2, sn, switch
    real(8) :: tclock, temp
    real(8) :: v1l, valu14, vol
    real(8) :: wave1, waveo
    real(8) :: x1, xcoup, xdiag, xj, xm, xoff, xpos, xpos1, xs, xx, xzero, xzero1
    real(8) :: y1, yj, ymang, ymmag, ypos, ypos1, yy, yzero, yzero1
    real(8) :: z1, z1ang, zero, zl, zmang, zmmag, zo, zoang, zs1, zs1drg, zso
    real(8) :: zsodrg
    !  dimension bufsem(14), jprmat(16)
    !  dimension array(1)
    !  dimension xwc(ntri), xwy(ntri), yzi(ndim,ndim), yzr(ndim,ndim)
    !  dimension tii(ndim, ndim), tir(ndim, ndim)
    !  dimension er(ndim), ei(ndim), theta2(ndim)
    !  dimension xtir(ndim), xtii(ndim), zsurge(ndim), dummi(ndim)
    !  dimension dummr(ndim), tixf(nsqr2), work1(nsqr2)
    equivalence (jprmat(1), j1 ), (jprmat(2), j2)
    equivalence (jprmat(3), j3 ), (jprmat(4), j4)
    equivalence (jprmat(5), j5 ), (jprmat(6), j6)
    equivalence (jprmat(7), j7 ), (jprmat(8), j8)
    equivalence (jprmat(9), j9 ), (jprmat(10), j10)
    equivalence (jprmat(11), j11 ), (jprmat(12), j12)
    equivalence (jprmat(13), j13 ), (jprmat(14), j14)
    equivalence (jprmat(15), j15 ), (jprmat(16), j16)
    !
    data text1  / 'change' /
    data text2  / 'branch' /
    data text3  / 'freque' /
    data text4  / 'specia' /
    data text5  / 'l doub' /
    data text6  / 'untran' /
    data text7  / 'sposed' /
    data text8  / 'transp' /
    data text9  / 'osed  ' /
    data fmetrc / 'metric' /
    data englis / 'englis' /
    data nrp / 0 /
    data mrr / 0 /
    mfrqpr = 0
    if (kexact .ne. 88333) go to 423
    nfqpl1 = 0
    close (unit = lunit2, status = 'delete')
    !open (unit = lunit2, status = 'scratch', form = 'formatted')
    open (unit = lunit2, form = 'formatted')
423 rewind  lunit9
    rewind lunit2
    rewind lunit3
    rewind lunt13
    if (ialter .ne. 2) go to 7407
    l5save = lunit5
    lunit5 = lunit2
7407 ldisfr = location (flstat(1)) - location (voltbc(1))
    metrik = 0
    finpcm = unity / 2.5400d0
    ftpm = 100.0d0 * finpcm / 12.0d0
    fmipkm = ftpm * 1000.0d0 / 5280.0d0
    d8 = 0.3048d0
    fspac = .024d0 * twopi * alogz (d8)
    !     fspac needed for conversion of reactance at 1 meter
    !     spacing to 1 foot spacing.
    !              start of constants definition.
    valu1 = 5584596.2d0
    valu2 = .00085646541d0
    valu3 = .00064373888d0
    valu4 = .61593152d0
    valu5 = 8.68588964d0
    valu6 = .0005000000d0
    valu7 = 3.0
    valu7 = onehaf * sqrtz( valu7 )
    valu8 = .0040447306d0
    valu9 = .21139217d0
    valu1 = .5772156649015328606d0
    valu11 = .3926991d0
    valu12 = .398942280401433d0
    valu13 = 1.2533141373155d0
    valu14 = 2.302585093000d0
    aaa1 = .785398163397448d0
    aaa2 = .318309886183791d0
    sqrt2 = 1.4142135623730950488d0
    ccars(2) = 1.3659315156584124488d0
    ll0 = 0
    ll1 = 1
    ll2 = 2
    ll3 = 3
    ll5 = 5
    ll6 = 6
    ll7 = 7
    ll8 = 8
    ll9 = 9
    ll10 = 10
    liu = 0
    nfreq = 0
    nbundl = 0
    pi = twopi * onehaf
    picon = 360. / twopi
    corchk = unity - tenm6/10.
    !              begin calculate constants for carson % bessel.
    bcars(1) = sqrt2 / 6.
    bcars(2) = unity / 16.
    dcars(2) = bcars(2) * pi / 4.
    do i = 3, 30
       isn = (-1) ** ((i - 1) / 2)
       bcars(i) = -bcars(i - 2) / (i * i + 2. * i) * isn
6603   dcars(i) = bcars(i) * pi / 4.
    end do
    ccars(1) = unity / sqrt2
    ccars(3) = ccars(1)
    ccars(5) = 3. * sqrt2 / 2.
    ccars(7) = -45. * onehaf * sqrt2
    do i = 1, 29, 2
       if (i .gt. 8) ccars(i) = 0.0
64     dcars(i) = ccars(i)
    end do
    dcars(3) = -dcars(3)
    dcars(7) = -dcars(7)
    do i = 4, 30, 2
46     ccars(i) = ccars(i - 2) + unity / i + unity / (i + 2.)
    end do
    fbe(1) = 16.
    fbed(1) = -4.
    d1 = unity - valu10
    fke(1) = fbe(1) * d1
    do i = 2, 14
       isn = (-1) ** i
       fbe(i) = fbe(i - 1) * (16. / (i * i)) * (-isn)
       fbed(i) = fbe(i) / (2. * i + 2.) * isn
       d1 = d1 + unity / i
       fke(i) = fbe(i) * d1
811    fked(i - 1) = fke(i) * i / 32.
    end do
    fked(14) = 0.0
    valu9=fke(1)/32.
    fbe(15) = .01104860d0
    fbe(16) = 0.0
    fbe(17) = -.00009060d0
    fbe(18) = -.00002520d0
    fbe(19) = -.00000340d0
    fbe(20) =  .00000060d0
    fbed(15) = -.01104850d0
    fbed(16) = -.00097650d0
    fbed(17) = -.00009010d0
    fbed(18) =  .0
    fbed(19) = .00000510d0
    fbed(20) = .00000190d0
    fke(15) = -.06250010d0
    fke(16) = -.00138130d0
    fke(17) =  .00000050d0
    fke(18) =  .00003460d0
    fke(19) =  .00001170d0
    fke(20) = .00000160d0
    fked(15) = -.00000010d0
    fked(16) =  .00138110d0
    fked(17) =  .00024520d0
    fked(18) =  .00003380d0
    fked(19) = -.00000240d0
    fked(20) = -.00000320d0
    if (iprsup .ge. 2) write (lunit6, 333) (fbe(i), i = 1, 20), (fbed(i), i = 1, 20), (fke(i), i = 1, 20), (fked(i), i = 1, 20), (bcars(i), i = 1, 30), &
         (dcars(i), i = 1, 30), (ccars(i), i = 1, 30)
333 format (1x, 'data fbe', /, /, 4(1x, 5e25.15, /), /, ' data fbed', /, /, 4(1x, 5e25.15, /), /, ' data fke', /, /, 4(1x,5e25.15, /), /, &
         ' data fked', /, /, 4(1x, 5e25.15, /), /, ' data bcars', /, /, 6(1x, 5e25.15, /), /, ' data dcars', /, /, 6(1x, 5e25.15, /), &
         /, ' data ccars', /, /, 6(1x, 5e25.15, /), /, 1x)
    if (iprsup .ge. 1) write (lunit6, 3866) pi, picon, sqrt2
3866 format (/, ' at 3866 ', 3e25.15)
2   m = 1
    do i = 1, 40
13866  brname(i) = blank
    end do
    mspedb = 0
    muntrn = 0
    mtrnsp = 0
    !     segmented, 1, vax e/t can skip translation of rewind:
    if (lastov .eq. 1) rewind lunit2
    rewind lunt13
    !     read input card using cimage
7403 call cimage
7413 continue
    read (unit = abuff, fmt = 4230) bus1
    if (bus1 .ne. text2) go to 37403
    !     optional "branch" card, which species  a6  branch names
    n1 = m + 11
    read (unit = abuff, fmt = 17403) (brname(i), i = m, n1)
17403 format (8x, 12a6)
    m = m + 12
    write (kunit6, 27403)
27403 format ('+Bus names for each phase.')
    go to 7403
37403 continue
    read (unit = abuff, fmt = 4230) bufsem
4230 format (13a6, a2)
    if (ialter .ne. 2) write (lunit2, 4230) bufsem
    bus1 = bufsem(1)
    if (bus1 .ne. fmetrc) goto 4258
    metrik = 1
    write (kunit6, 4252)
4252 format ('+Request card for metric  units on all data.')
    go to 7403
4258 if (bus1 .ne. englis) go to 24258
    metrik = 0
    write (kunit6, 14258)
14258 format ('+Request card for english units on all data.')
    go to 7403
24258 if (bus1 .ne. text3) go to 34258
    write (kunit6, 44258)
44258 format ('+Request for frequency-loop printout.')
    mfrqpr = 1
    go to 7403
34258 if (bufsem(1) .ne. text4) go to 54258
    if (bufsem(2) .ne. text5) go to 54258
    mspedb = 1
    write (kunit6, 5010)
5010 format ('+Request for special double circuit transposed.')
    go to 7403
54258 if (bufsem(1) .ne. text6) go to 5020
    if (bufsem(2) .ne. text7) go to 5020
    muntrn = 1
    write (kunit6, 5015)
5015 format ('+Request for untransposed line modeling.')
    go to 7403
5020 if (bufsem(1) .ne. text8) go to 5029
    if (bufsem(2) .ne. text9) go to 5029
    mtrnsp = 1
    write (kunit6,5027)
5027 format ('+Request for transposed line modeling.')
    go to 7403
5029 do j = 1, 14
       if (bufsem(j)  .ne.  blank) go to 4251
7436 end do
    write (kunit6, 4244)
4244 format ('+Blank card terminating line-constants cases.')
    call interp
    if (lastov .eq. 1) go to 7439
    n1 = lastov
    lastov = nchain
    nchain = n1
    if (ialter .eq. 2) lunit5 = l5save
    if (ipunch .eq. 0) go to 7496
    d1 = 0.0
    write (lunit9) d1, d1, d1
    if (iprsup .ge. 1 .or. lastov .eq. 39 ) write (lunit6, 14244) d1, d1, d1, ipunch
14244 format (' Last record on unit9', 3e15.6, /, 1x, 'ipunch =', i10)
    !     segmented, 1, vax e/t can skip translation of rewind:
    rewind lunit9
7496 go to 9900
7439 lastov = nchain
    nchain = 51
    go to 9900
    !     segmented, 1, vax e/t can skip translation of rewind:
4251 rewind lunt13
    ik = 0
    if (bus1 .ne. text1) go to 4260
    write (kunit6, 4257)
4257 format ('+Request card for change-case option.')
    go to 500
4260 i = 1
    !  assign 4236 to moon
    moon = 4236
    go to 4164
5   i = i + 1
    !     read input card using cimage
    call cimage
    read (unit = abuff, fmt = 4230) bufsem
    if (ialter .ne. 2 ) write (lunit2, 4230) bufsem
4164 if (kolbeg .gt. 0) go to 8712
    read (unit = abuff, fmt = 11) itbic(i), tbtb2(i), tbr(i),itbtb3(i),tbg(i),tbd(i), tbx(i), h1, h2, d8, d9, tbtext(i), i3
11  format (i3, f5.4, f8.5, i2, 2f8.5, 3f8.3, f8.5, f6.2, a6, i2)
    go to 8715
8712 nfrfld = 1
    call freone (d11)
    itbic(i) = d11
    call frefld (tbtb2(i))
    call frefld (tbr(i))
    call freone (d11)
    itbtb3(i) = d11
    call frefld (tbg(i))
    call frefld (tbd(i))
    call frefld (tbx(i))
    call freone (h1)
    call freone (h2)
    call freone (d8)
    call freone (d9)
    nright = -1
    call freone (d1)
    nright = 0
    tbtext(i) = texta6(1)
    call freone (d11)
    i3 = d11
    if (kill .gt. 0) go to 9200
    go to 4235
8715 if (i .eq. 1) go to 4320
4280 if (itbic(i) .ne. 0) go to 4285
    read (unit = abuff, fmt = 4281) bus1
4281 format (a3)
    if (bus1 .ne. blank) go to 4285
    itbic(i) = itbic(i - 1)
4285 if (tbtb2(i) .ne. 0.0) go to 4291
    read (unit = abuff, fmt = 4286) bus1
4286 format (3x, a5)
    if (bus1 .ne. blank) go to 4291
    tbtb2(i) = tbtb2(i - 1)
4291 if (tbr(i) .ne. 0.0) go to 4296
    read (unit = abuff, fmt = 4292) bus1, bus2
4292 format (8x, 2a4)
    if (bus1 .ne. blank) go to 4296
    if (bus2 .ne. blank) go to 4296
    tbr(i) = tbr(i - 1)
4296 if (itbtb3(i) .ne. 0) go to 4305
    read (unit = abuff, fmt = 4297) bus1
4297 format (16x, a2)
    if (bus1 .ne. blank) go to 4305
    itbtb3(i) = itbtb3(i - 1)
4305 if (tbg(i) .ne. 0.0) go to 4312
    read (unit = abuff, fmt = 4306) bus1, bus2
4306 format (18x, 2a4)
    if (bus1 .ne. blank) go to 4312
    if (bus2 .ne. blank) go to 4312
    tbg(i) = tbg(i - 1)
4312 if (tbd(i) .ne. 0.0) go to 4320
    read (unit = abuff, fmt = 4313) bus1, bus2
4313 format (26x, 2a4)
    if (bus1 .ne. blank) go to 4320
    if (bus2 .ne. blank) go to 4320
    tbd(i) = tbd(i - 1)
4320 if (h1 .ne. 0.0) go to 4227
    read (unit = abuff, fmt = 4321) bus1, bus2
4321 format (42x, 2a4)
    if ( bus1 .ne. blank) go to 4227
    if ( bus2 .ne. blank) go to 4227
    h1 = h2
4227 if (h2 .ne. 0.0) go to 4235
    read (unit = abuff, fmt = 4228) bus1, bus2
4228 format (50x, 2a4)
    if (bus1 .ne. blank) go to 4235
    if (bus2 .ne. blank) go to 4235
    h2 = h1
4235 tby(i) = (h1 + h2 + h2) / 3.0
    !  go to moon, (4236, 711)
    select case (moon)
    case (711)
       go to 711

    case (4236)
       go to 4236
    end select
4236 if (tbx(i) .ne. 0.0) go to 4239
    if (tby(i) .ne. 0.0) go to 4239
    write (kunit6, 4268)
4268 format ('+Blank card terminating conductor cards.')
    go to 6
4239 write (kunit6, 4240) tbtb2(i), tbr(i), itbtb3(i)
4240 format ('+Line-conductor card.', 2e11.3, i6)
    if (i3 .le. 1) go to 706
    nbundl = 1
    i4 = 1
    xx = tbx(i)
    yy = tby(i)
    angl = (pi - twopi / i3) / 2.0
    dangl = twopi / i3
    radius = d8 / (24.0 * cosz (angl))
    if (metrik .eq. 1) radius = radius * .12d0
    d9r = d9 / picon
703 tbx(i) = xx + radius * cosz (d9r - dangl * i4)
    tby(i) = yy + radius * sinz (d9r - dangl * i4)
    if (i4 .eq. i3) go to 706
    i4 = i4 + 1
    i = i + 1
    itbic(i) = itbic(i - 1)
    tbtb2(i) = tbtb2(i - 1)
    tbr(i) = tbr(i - 1)
    itbtb3(i) = itbtb3(i - 1)
    tbg(i) = tbg(i - 1)
    tbd(i) = tbd(i - 1)
    tbtext(i) = tbtext(i - 1)
    go to 703
706 if (i .le. lphase) go to 5
7   kill = 82
    lstat(15) = lphase
    lstat(19) = 7
    go to 9200
    !                                         sorting of conductor data
6   tbd(i) = 0.0
    if (iprsup .ge. 1) write (lunit6, 3625) (itbic(k), itbtb3(k), tbtb2(k), tbr(k), tbg(k), tbd(k), tbx(k), tby(k), tbtext(k), k = 1, i)
3625 format (//, ' Unsorted conductor table', /, (1x, 2i6, 6e15.5, 5x, a6))
    !              '      '      '    convert from si units for calculation.
    if (metrik .ne. 1) goto 981
    do k = 1, i
       tbd(k) = tbd(k) * finpcm
       tbx(k) = tbx (k) * ftpm
       tby(k) = tby(k) * ftpm
       tbr(k) = tbr(k) / fmipkm
       jb = itbtb3(k)
       if (jb .lt. 2) tbg(k) = (tbg(k) + fspac) / fmipkm
       if (jb .eq. 2) tbg(k) = tbg(k) * finpcm
982 end do
981 call move0 (ic(1 :), lphase)
    ktot = 0
    kcirct = 0
    k = 0
10  k = k + 1
    if (tbd(k) .eq. 0.) go to 15
    i1 = itbic(k)
    if (i1 .eq. 0) go to 12
    if (i1 .lt. 0) go to 10
    if (i1 .gt. kcirct) kcirct = i1
    i = lphpl1 - i1
    if (ic(i) .eq. 0) go to 13
12  ktot = ktot + 1
    i = ktot
    if (i1 .eq. 0) go to 13
    ip = ktot
18  j = i - 1
    if (j .eq. 0) go to 13
    if (ic(j) .gt. 0) go to 13
    i = j
    ic(ip) = ic(i)
    tb2(ip) = tb2(i)
    itb3(ip) = itb3(i)
    r(ip) = r(i)
    d(ip) = d(i)
    gmd(ip) = gmd(i)
    x(ip) = x(i)
    y(ip) = y(i)
    text(ip) = text(i)
    ip=ip - 1
    go to 18
13  kfull = ktot + kcirct
    if (kfull .gt. lphase) go to 7
    ic(i) = i1
    tb2(i) = tbtb2(k)
    itb3(i) = itbtb3(k)
    r(i) = tbr(k)
    d(i) = tbd(k)
    gmd(i) = tbg(k)
    x(i) = tbx(k)
    y(i) = tby(k)
    text(i) = tbtext(k)
    go to 10
15  iprint = 0
    ntol = 0
16  if (ik .gt. 0) go to 3006
    !     read input card using cimage
33316 call cimage
    !     segmented, 1, vax e/t can skip translation of rewind:
    rewind lunt13
    read (unit = abuff, fmt = 4230) bufsem
    if (ialter .ne. 2) write (lunit2, 4230) bufsem
    read (unit = abuff, fmt = 17) rearth, freq, corr, j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, &
         j11, j12, iw, dist, j13, j14, j15, j16, isegm, mutual, ik, ips, j17, imodal, itrnsf, conduc
17  format (f8.2, f10.2, f10.6, i2, 5i1, i2, 5i1, i2, f8.3, i2, 5i1, 3i3, 2i2, e8.2)
    if (metrik .eq. 1) conduc = conduc / fmipkm
    if (conduc .eq. 0) conduc = 3.22d-9
    if (muntrn .eq. 1) imodal = 1
    if (mtrnsp .eq. 1 .or. mspedb .eq. 1) imodal = 0
    if (kexact .ne. 88333) go to 3168
    freq = fminfs
    ik = alog1z (fmaxfs / fminfs) + epsiln
    ips = valu14 / alogz (-delffs) + epsiln
    distsv = dist
    go to 217
3168 if (lastov .eq. 1 .or. imodal .eq. 1) go to 217
    if (lastov .ne. 39) go to 217
    gmode = conduc
    if (metrik .eq. 1) gmode = gmode * fmipkm
217 jspecl = j1 + j2 + j3 + j4 + j5 + j6 + j7 + j8 + j9 +j10 + j11 + j12
    nfreq = nfreq + 1
    if (lastov .ne. 1 .and. imodal .eq. 0 .and. nfreq .eq. 2 .and. ik .eq. 0 .and. ips .eq. 0 .and. lastov .ne. 45) go to 33316
    j5out = j5
    j8out = j8
    j2out = j2
    j4out = j4
    j6out = j6
    j9out = j9
    if (imodal .ne. 0) go to 117
    j5 = 1
    j9 = 1
117 if (j17 .ne. 44)  go to 28725
    j5 = 1
    j8 = 1
    iw = 1
28725 if (imodal .le. 0) go to 18725
    j2 = 1
    j8 = 1
18725 if (ik .le. 0) go to 8726
    j6 = 1
    j9 = 1
8726 if (ck1 .ne. -fltinf) go to 7446
    if (dist .le. 0.0) go to 7446
    ck1 = dist
    if (metrik .eq. 1) ck1 = ck1 * fmipkm
7446 if (ci1 .ne. -fltinf) go to 7449
    if (rearth .le. 0.0) go to 7449
    ci1 = rearth
7449 if ( mutual  .gt.  0 )   j8 = 1
    if ( kolbeg  .gt.  0 )   go to 4264
    if ( corr  .ne.  0.0 )  go to 4264
    read (unit = abuff, fmt = 4256) bus1, bus2
4256 format ( 18x, a6, a4 )
    if (bus1 .ne. blank) go to 4264
    if (bus2 .ne. blank) go to 4264
    corr = tenm6
4264 iprint = iprint + 1
    if ( rearth  .ne.  0.0 )   go to 4267
    write (kunit6, 4266)
4266 format ('+Blank card terminating frequency cards.')
    if (kexact .ne. 88333) go to 2
    lastov = 44
    !      nchain = 8
    nchain = 2
    go to 9900
4267 if (ialter .eq. 2) rearth = ci1
    write (kunit6, 3618) rearth, freq, dist
3618 format ('+Frequency card.', 3e11.3)
    call interp
    if (metrik .eq. 1) dist = dist * fmipkm
    ipunch = j17
    jpralt = 1
    if (ialter .eq. 0) go to 3729
    do jj = 1, 16
       if (jprmat (jj) .ne. 0) go to 13721
3721 end do
    jpralt = 0
    if (ialter .eq. 3) go to 13723
3723 jprmat(5) = 1
    jprmat(8) = 1
    iw = 0
    go to 3729
13721 if (ialter .ne. 3) go to 3723
13723 jprmat (6) = 1
    jprmat(9) = 1
3729 kkk = 0
    if (iprsup .ge. 1) write (lunit6, 7454) iprint, ldisfr, ialter
7454 format (/, ' New frequency card.  iprint  ldisfr  ialter', /, 20x, 3i8)
    do jj = 1, 16
       if (jprmat (jj) .ne. 0) go to 3733
3731 end do
    go to 33316
3733 if (freq .eq. 0.0) freq = statfr
    if (ik .gt. 0)   go to 7456
    if (ialter .eq. 3) go to 7453
    if (lastov .eq. 1) go to 7453
    volti(iprint) = rearth
    if (ialter .eq. 2) go to 7453
    voltk(iprint) = freq
    if (iprint .le. ldisfr) go to 7453
    kill = 170
    lstat(14) = ldisfr
    lstat(19) = 7456
    go to 9200
7453 go to 3007
7456 if (ips  .eq.  0) ips = 1
    if (ips * ik .le. 399) go to 7458
    !     this check really belongs in "semlyen setup" (overlay 45):
    if (lastov .ne. 45) go to 7458
    kill = 160
    lstat(14) = 399
    lstat(19) = 7458
    go to 9200
7458 continue
    factor = ips
    factor = valu14 / factor
    fdecad=freq
    if (lastov .eq. 1 .and. imodal .ne. 0) go to 7459
    dist = 0.
7459 iii = 0
    voltbc(1) = freq
    voltbc(2) = freq * 10. ** ik
    voltbc(3) = expz (factor)
    d13 = voltbc(2) / voltbc(1)
    voltbc(4) = alogz (d13) / alogz (voltbc(3)) + 1.5
    voltbc(5) = ik
    voltbc(6) = ips
    icheck = iprint - 1
    iprint = icheck
    ktab = kcirct
    nmode = 2
    if (kcirct .lt. 2)  nmode = 1
    if (iprsup .ge. 1) write (lunit6, 7457) icheck, ktab, (voltbc(jj), jj = 1, 3)
7457 format (/, ' Logarithmic frequency.  icheck    ktab', 8x,  'volt(1)', 8x, 'volt(2)', 8x, 'volt(3)', /, 23x, 2i8, 3e15.6)
    if (mfrqpr .gt. 0) write (lunit6, 3004)
    go to 3006
3005 iii = iii + 1
!!!!      if ( iii  .ge.  ik )   go to 33316
    if (iii .ge. ik) go to 8801
    go to 8802
8801 if (kexact .ne. 88333) go to 8701
    if (nfqpl1 .eq. 1) go to 8701
    nfqpl1 = 1
    go to 8802
8701 if (nfreq .ne. 3) go to 8800
    !!    ** identical eigenvalue ? **
    if (nrp .gt. 0 .or. itrnsf .eq. 0) go to 8800
    write (*, *) '$$  mrr, ntol, nfreq are  : ', mrr, ntol, nfreq
    rtio = (mrr * 1.0) / ntol
    if (rtio .lt. 0.75) go to 8800
    nrp = 1
    freq = fdecad
    do i = 1, ntol * ktab
       backspace lunit9
8804 end do
    ntol = 0
    go to 16
8800 go to 33316
8802 fdecad = fdecad * 10.0
    kkk = 1
3006 pkkk = kkk
    freq = fdecad * expz (pkkk * factor)
    iprint = iprint + 1
    if (iprsup .ge. 1) write (lunit6, 23006) iprint, kkk, iii, freq, ci1, ck1
23006 format (/, ' Next logarithmically-spaced frequency.  iprint     kkk     iii', 11x, 'freq', 12x, 'ci1', 12x, 'ck1', /, 39x, 3i8, 3e15.6)
    if (kkk .gt. ips) go to 3005
    if (freq + epsiln .lt. fmaxfs .or. kexact .ne. 88333) go to 8806
    if (nfqpl1 .eq. 1) kkk = ips
8806 kkk = kkk + 1
3007 omega = twopi * freq
    if (ik .gt. 0) go to 3008
    write (lunit6, 4)
4   format (//, ' Line-conductor table after sorting and initial processing.', /, 1x, &
         'table', 3x, 'phase', 3x, 'skin effect', 4x, 'resistance', 3x, 'reactance-data specification', 5x, &
         'diameter', 3x, 'horizontal', 3x, 'avg height', 3x, 'conductor')
8   format (3x, 'row', 2x, 'number', 8x, 'r-type', 4x, 'r (ohm/mi)', 3x, 'x-type', 6x, 'x(ohm/mi) or gmr', 5x, '(inches)', 5x, 'x (feet)', 5x, 'y (feet)', 8x, 'name')
    if (metrik .eq. 0) write (lunit6, 8)
    if (metrik .eq. 1) write (lunit6, 9)
9   format (3x, 'row', 2x, 'number', 8x, 'r-type', 4x, 'r (ohm/km)', 3x, 'x-type', 6x, 'x(ohm/km) or gmr', 5x, '(  cm  )', 5x, 'x (mtrs)', 5x, 'y (mtrs)', 8x, 'name')
3008 idist = 1
    if (dist .eq. 0.0 .and. ipunch .ne. 44) go to 4271
    if (kcirct .eq. 0) go to 4271
    if (kcirct .lt. lphd2) go to 4275
4271 idist = 0
4275 j56 = j5 + j6 + idist
    !
    !                                                  formation of p-matrix
    ip = 0
    k = 0
    f1 = unity / omega
    if (iw .gt. 0) f1 = unity
    f1 = f1 * valu1
    f2 = f1 * 2.0
20  k = k + 1
    if (k .gt. kfull) go to 30
    if (k .le. kcirct) go to 28
    j = k - kcirct
21  x1 = x(j)
    y1 = y(j)
    d1 = d(j)
    if (iprint .ne. 1) go to 222
    if (metrik .eq. 0) goto 221
    rj = r(j) * fmipkm
    yj = y(j) / ftpm
    xj = x(j) / ftpm
    dj = d(j) / finpcm
    gmdj = gmd(j)
    jb = itb3(j)
    if (jb .lt. 2) gmdj = gmdj * fmipkm - fspac
    if (jb .eq. 2) gmdj = gmdj / finpcm
    write (lunit6, 22) k, ic(j), tb2(j), rj, jb, gmdj, dj, xj, yj, text(j)
    goto 225
221 write (lunit6, 22) k, ic(j), tb2(j), r(j), itb3(j), gmd(j), d(j), x(j), y(j), text(j)
22  format (1x, i5, i8, 2f14.5, i9, f22.6, f13.5, 2f13.3, 6x, a6)
225 if (itb3(j) .ne. 4) go to 222
    if (tb2(j) .gt. 0.) go to 224
    kill = 83
    lstat(15) = j
    lstat(19) = 224
    go to 9200
224 gmd(j) = d(j)
222 i = 0
23  i = i + 1
    ip = ip + 1
    if (i .eq. k) go to 24
    j = i - kcirct
    if (i .le. kcirct) j = lphpl1 - i
    dx = (x(j) - x1) ** 2
    h1 = y(j) - y1
    h2 = y(j) + y1
    if (dx .eq. 0.0 .and. h1 .eq. 0.0) go to 26
    r1 = alogz ((dx + h2 * h2) / (dx + h1 * h1))
    p(ip) = r1 * f1
    z(ip) = r1
    go to 23
24  p(ip) = alogz (48. * y1 / d1) * f2
    go to 20
26  kill = 84
    lstat(15) = i
    lstat(19) = 28
    go to 9200
28  j = lphpl1 - k
    if (ic(j) .ne. 0) go to 21
    kill = 85
    lstat(19) = 30
    lstat(15) = k
    go to 9200
30  if (kkk .gt. 0 .and. lastov .ne. 1) go to 3020
    if (jspecl .eq. 0) go to 7025
    write (lunit6, 25) rearth, freq, corr
25  format (//, '0following matrices are for earth resistivity=', f8.2, ' Ohm-m and frequency=', f13.2, ' Hz. correction factor=', f10.6)
7025 if (kkk .gt. 1) go to 3020
    if (isegm .gt. 0) write (lunit6,12345)
12345 format (' ', 30x, '************Earth wires will be segmented************')
    if (ik .gt. 0) write (lunit6,3004)
3004 format ('0', 6x, '-----------------Zero sequence-----------------', 13x, '---------------Positive sequence---------------', /, 4x, &
         'alpha', 8x, 'beta', 8x, 'R', 8x, 'L(milli-', 4x, 'C(micro-', 6x, 'alpha', 8x, 'beta', 8x, 'R', 8x, 'L(milli-', 4x, 'C(micro-', 4x, 'frequency', /, &
         ' neper/mile  radian/mile  Ohm/mile  Henry/mile) Farad/mile)  neper/mile  radian/mile  Ohm/mile   Henry/mile) Farad/mile)', 5x, 'Hz')
3020 write (lunt13) (z(i), i = 1, ip)
    if (iprsup .ge. 2) write (lunit6, 4427) (z(i), i = 1, ip)
4427 format (/, ' at 4427.  (z(i), i = 1, ip) .', /, (1x, 8e16.6))
    if (iprsup .ge. 1) write (lunit6, 4428) j1, j4, kcirct, j2, j3, j56, kfull
4428 format (/, ' at 4428.', 7i12)
    !
    !                                   reductions and inversions p-matrix
    switch=-unity
    if (j1 .gt. 0) go to 36
31  if (j4 .gt. 0) go to 37
32  if (kcirct.eq.0) go to 80
    if (j2+j3+j56  .eq.0) go to 80
    !                          begin elimination of earth wires and bundling
49  k=kcirct
50  k=k+1
    if (k .gt. kfull) go to 56
    i=k-kcirct
    i=ic(i)
    if (i.eq.0) go to 50
    i2=i*(i-1)/2
    k2=k*(k-1)/2
    kk=k2+i
    h1=p(kk)
    h2=z(kk)
    kk=k2+k
    l=0
51  if (l.lt.i) go to 54
    i2=i2+l
52  if (l.lt.k) go to 55
    k2=k2+l
53  l=l+1
    p(k2)=p(k2)-p(i2)
    if (switch .gt. 0.)z(k2)=z(k2)-z(i2)
    if (l.ne.kfull) go to 51
    p(kk)=p(kk)-h1
    if (switch .gt. 0.) z(kk)=z(kk)-h2
    go to 50
54  i2=i2+1
    go to 52
55  k2=k2+1
    go to 53
56  kp=kcirct*(kcirct+1)/2
    if (switch .gt. 0.) go to 202
    call redu44 (p(1 :), workr1(1 :), kfull, kcirct)
!!!!      write (*,*) ' after redu44 on c.  p(1:6) =',
!!!!     1                                ( p(i), i=1, 6 )
    if (kill .gt. 0) go to 9200
    !                            end elimination of earth wires and bundling
!!!!      write (*,*) ' branch to 38?  j2, j3 =',  j2, j3
    if (j2 + j3 .gt. 0) go to 38
33  if (j56 .gt. 0) go to 39
    !
    !                                         formation of z-matrix
80  switch = +unity
    j56 = j8 + j9 + j11 + j12 + idist
    if (j56 + j7 + j10 .eq. 0) go to 600
    !     segmented, 1, vax e/t can skip translation of rewind:
    rewind lunt13
    k = 0
    if (rearth .eq. 0.0d0) go to 100
    f1 = valu2 * sqrtz (freq / rearth)
100 f2 = omega * valu3
    imax = 30
    if (corr .eq. 0.0d0 .or. rearth .eq. 0.0d0) imax = -1
    if (corr .gt. corchk) imax = corr - onehaf
    if (imax .gt. 31) imax = 30
    read (unit = lunt13) (p(i), i = 1, ip)
    !     segmented, 1, vax e/t can skip translation of rewind:
    rewind lunt13
    ip = 0
120 k = k + 1
    if (k .gt. kfull) go to 200
    if (k .le. kcirct) go to 129
    j = k - kcirct
    go to 121
129 j = lphpl1 - k
121 x1 = x(j)
    y1 = y(j)
    r1 = r(j)
    h1 = tb2(j)
    h2 = unity - 2.0d0 * h1
    if (h1 .gt. 0.) call skin (h2, r1, freq, r1, h1)
    g1 = gmd(j)
    ix = itb3(j)
    izero = 1
    xm = 0.0d0
    if (ic(j) .eq. 0 .and. isegm .gt. 0) izero = 0
    if (ix .lt. 2) go to 150
    if (ix .eq. 3) g1 = g1 * d(j) / 2.0d0
    xs = alogz (24.0d0 * y1 / g1) * onehaf
122 if (ix .ne. 4) go to 125
    xs = xs + h1 / f2 - onehaf * alogz (d(j) * onehaf / g1)
125 i = 0
123 i = i + 1
    ip = ip + 1
    if (i .eq. k) go to 124
    j = i - kcirct
    if (i .le. kcirct) j = lphpl1 - i
    rm = 0.0d0
    if (izero .eq. 0) go to 126
    xm = p(ip) / 4.0d0
    if (imax .lt. 0) go to 126
    dx = absz (x(j) - x1)
    h2 = y(j) + y1
    s = sqrtz (h2 * h2 + dx * dx)
    z1 = s * f1
    if (z1 .gt. 5.0d0) go to 151
    !                    begin carson series for mutual impedance if z.le.5.
    rm = twopi / 16.0d0
    zl = alogz (z1)
    xm = xm + (valu4 - zl) * onehaf
    if (imax .eq. 0) go to 126
    s1 = dx / s
    c1 = h2 / s
    cs = c1 * z1
    sn = s1 * z1
    if (imax .gt. 1) phi = asinz (s1)
    m5 = 0
    error = 0.0d0
1110 k5 = 0
    i5 = m5 * 4
1111 k5 = k5 + 1
    i5 = i5 + 1
    if (k5 .eq. 1 .or. k5 .eq. 3) go to 1113
    deltap = ((ccars(i5) - zl) * cs + phi * sn) * bcars(i5)
    h1 = -dcars(i5) * cs
    if (k5 .eq. 4) go to 1114
    deltaq = h1
1004 rm = rm + deltap
    xm = xm + deltaq
    if (i5 .eq. imax) go to 126
    if (corr .gt. corchk) go to 1005
    if (absz (deltap) .lt. corr .and. absz (deltaq) .lt. corr) go to 1115
    error = 0.0d0
1005 h1 = sn * c1 + cs * s1
    cs = (cs * c1 - sn * s1) * z1
    sn = h1 * z1
    if (k5 .lt. 4) go to 1111
    m5 = m5 + 1
    go to 1110
1113 deltaq = bcars(i5) * cs
    deltap = deltaq
    if (k5 .eq. 1) deltap = -deltap
    go to 1004
1114 deltaq = -deltap
    deltap = h1
    go to 1004
1115 if (error .gt. onehaf) go to 126
    error = unity
    go to 1005
    !                      end carson series for mutual impedance if z.le.5.
    !
    !                    begin carson series for mutual impedance if z .gt. 5.
151 s = s * z1
    sn = dx / s
    cs = h2 / s
    s2 = sn * cs * 2.0d0
    c2 = cs * cs - sn * sn
    rm = -c2
    do i5 = 1, 7, 2
       rm = rm + ccars(i5) * cs
       xm = xm + dcars(i5) * cs
       h1 = cs * s2 + sn * c2
       cs = cs * c2 - sn * s2
1006   sn = h1
    end do
    !                      end carson series for mutual impedance if z .gt. 5.
126 p(ip) = rm * f2
    z(ip) = xm * f2
    if (iprsup .ge. 4) write (unit = lunit6, fmt = 4439) i, k, kfull, kcirct, i5, ip, p(ip), z(ip)
4439 format (' At 4439 ', 6i10, 2e16.6)
    go to 123
124 if (imax .lt. 0) go to 128
    r1 = r1 / f2
    z1 = y1 * f1 * 2.0d0
    if (z1 .gt. 5.0d0) go to 152
    !                    begin carson series for self impedance if z.le.5.0
    r1 = r1 + twopi / 16.0d0
    zl = alogz (z1)
    xs = xs + (valu4 - zl) * onehaf
    if (imax .eq. 0) go to 127
    m5 = 0
    cs = z1
    error = 0.0d0
2110 k5 = 0
    i5 = m5 * 4
2111 k5 = k5 + 1
    i5 = i5 + 1
    if (k5 .eq. 1 .or. k5 .eq. 3) go to 2113
    deltap = (ccars(i5) - zl) * bcars(i5) * cs
    h1 = -dcars(i5) * cs
    if (k5 .eq. 4) go to 2114
    deltaq = h1
2004 r1 = r1 + deltap
    xs = xs + deltaq
    if (i5 .eq. imax) go to 127
    if (corr .gt. corchk) go to 2005
    if (absz (deltap) .lt. corr .and. absz (deltaq) .lt. corr) go to 2115
    error = 0.0d0
2005 cs = cs * z1
    if (k5 .lt. 4) go to 2111
    m5 = m5 + 1
    go to 2110
2113 deltaq = bcars(i5) * cs
    deltap = deltaq
    if (k5 .eq. 1) deltap = -deltap
    go to 2004
2114 deltaq = -deltap
    deltap = h1
    go to 2004
2115 if (error .gt. onehaf) go to 127
    error = unity
    go to 2005
    !                      End Carson series for self impedance if z.le.5.0
    !
    !                    Begin Carson series for self impedance if z .gt. 5.0
152 cs = unity / z1
    c2 = cs * cs
    r1 = r1 - c2
    do i5 = 1, 7, 2
       r1 = r1 + ccars(i5) * cs
       xs = xs + dcars(i5) * cs
2006   cs = cs * c2
    end do
    !                      End Carson series for self impedance if z .gt. 5.0
127 r1 = r1 * f2
128 z(ip) = xs * f2
    p(ip) = r1
    if (iprsup .ge. 4) write (unit = lunit6, fmt = 4440) i, k, kfull, kcirct, i5, ip, p(ip), z(ip)
4440 format (' At 4440 ', 6i10, 2e16.6)
    go to 120
150 xs = g1 / f2
    if (ix .eq. 1) xs = xs * freq / 60.0d0
    xs = xs + alogz (y1 * 2.0d0) * onehaf
    g1 = 24.0d0 * y1 / expz (2.0d0 * xs)
    go to 122
    !
    !                                  routines for z-printing and inversion
    !     impedance matrix for physical conductors is in p(1),...p(ip) (real
    !     part) and z(1)....z(ip) (imaginary part).
200 if (j7 .gt. 0) call output (metrik, p(1 :), z(1 :), unity, kfull, ll6, ll1)
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 4434) j7, j10, j56, kcirct, j8, mutual, j11, j12, idist, j9, kp, j2, j3, j5, j6
4434 format (/, ' at 4234.   misc. integers.', /, (1x, 10i12))
    if (j10 .gt. 0) go to 250
    if (j56 .eq. 0) go to 600
201 if (kcirct .eq. 0) go to 600
    go to 49
202 call cxred2 (p(1 :), z(1 :), workr1(1 :), workr2(1 :), kfull, kcirct)
    !     Impedance matrix for equivalent phase conductors is in p(1),...
    !     p(kp) (real part) and z(1),...z(kp) (imaginary part).
    if (j8out .gt. 0) call output (metrik, p(1 :), z(1 :), unity, kcirct, ll6, ll2)
    if (ipunch .ne. 44) go to 8202
    call punpie (kcirct)
8202 if (iprsup .ge. 1) write (unit = lunit6, fmt = 3202) imodal, dist
3202 format (' imodal, dist =', i5, e17.5)
    if (lastov .ne. 39) go to 3203
    if (nfreq .ne. 1) go to 3203
    !     "Marti setup" assumes distance in kilometers is on 9:
    distkm = dist / fmipkm
    write (unit = lunit9) imodal, metrik, distkm, mspedb, itrnsf
    if (iprsup  .gt.  1) write (unit = lunit6, fmt = *) ' mspedb written on lunit9 =', mspedb
3203 if (imodal .le. 0) go to 2202
    if (lastov .eq. 1 .and. itrnsf .ne. -1 .and. itrnsf .ne. -9) itrnsf = 0
    n8 = kfull
    if (nbundl .eq. 1) n8 = kcirct
    if (kcirct .le. ndim) go to 1202
    kill = 221
    lstat(19) = 1202
    lstat(14) = kcirct
    go to 9200
1202 if (nfreq .eq. 3) ntol = ntol + 1
    if (kexact .eq. 88333) dist = distsv
    call modal (array, xwc, xwy, yzr, yzi, tii, tir, tvi, tvr, er, ei, theta2, xtir, xtii, zsurge, dummi, dummr, tixf, work1, freq, kcirct, iw, dist, metrik, fmipkm, ndim, ntri, nsqr2, itrnsf,  kfull, mrr, nrp, ntol, conduc)
    if (kill .ne. 0) go to 9900
2202 if (lastov .eq. 39 .or. lastov .eq. 45 .or. imodal .ne. 0) go to 8777
    if (kexact .eq. 88333) go to 45454
    kount = 0
    rewind lunit1
    if (ipunch .eq. 0) write (unit = lunit7, fmt = 8769) tclock, date1
8769 format ('c   Punched card output of transposed line which began at ', 2x, 2a4, 2x, 2a4)
    if (ipunch .ne. 0) go to 8770
    if (imodal .eq. 0 .and. mspedb .eq. 0) write (unit = lunit7, fmt = 8780) freq
8780 format ('c   ***** Transposed K. C. Lee line segment calculated at', 2x, e10.3, ' Hz. *****')
    if (imodal .eq. 0 .and. mspedb .eq. 1) write (unit = lunit7, fmt = 8785) freq
8785 format ('c **** Special double circuit transposed line calculated at', 2x, e10.3, ' Hz. ****')
8770 rewind lunit2
    n5 = 0
    !     if ( jdatcs  .gt.  0 )  go to 8777
    do n12 = 1, 9999
       read (unit = lunit2, fmt = 8771) (texta6(i), i = 1, 14)
8771   format (13a6, a2)
       if (ipunch .eq. 0) write (unit = lunit7, fmt = 8772) (texta6(i), i = 1, 13)
8772   format ('c ', 13a6)
       if (idebug .eq. 0) go to 8100
       write (unit = lunit1, fmt = 8773) (texta6(i), i = 1, 14)
8773   format (' c ', 13a6, a2)
       kount = kount + 1
8100   if (texta6(1) .eq. blank .and. texta6(2) .eq. blank) n5 = n5 + 1
       if (n5 .ge. 1) go to 8777
8774 end do
8777 if (ialter .le. 0) go to 3734
    if (lastov .eq. 39) go to 3734
    write (unit = lunit3) (p(jj), jj = 1, kp)
    write (unit = lunit3) (z(jj), jj = 1, kp)
    if (iprsup .eq. 0) go to 3734
    write (unit = lunit6, fmt = 7423) kp, (p(jj), jj = 1, kp), (z(jj), jj = 1, kp)
7423 format (/, ' Output of  (R)  and  (L)  for Semlyen,    ', i6, '  cells each.', /, (1x, 8e16.4))
3734 if (mutual .gt. 0) call outspc (p(1 :), z(1 :), kcirct, metrik, fmipkm)
    j56 = j11 + j12 + idist
    if (lastov .eq. 39) go to 253
    if (j9 .gt. 0 .and. kcirct .ge. 2) go to 253
203 if (j56 .eq. 0) go to 600
    do i = 1, kp
       p(i) = -p(i)
204    z(i) = -z(i)
    end do
    call cxred2 (p(1 :), z(1 :), workr1(1 :), workr2(1 :), kcirct, ll0)
    !     Inverted impedance matrix for equivalent phase conductors is in
    !     p(1),...p(kp) (real part) and z(1),...z(kp) (imaginary part).
    if (j11 .gt. 0) call output (metrik, p(1 :), z(1 :), unity, kcirct, ll5, ll2)
    if (idist .eq. 0) go to 207
    do i = 1, kp
       gd(i) = p(i)
206    bd(i) = z(i)
    end do
207 if (j12 .eq. 0 .or. kcirct .lt. 2) go to 600
    call symm (p(1 :), z(1 :), unity, kcirct, kk)
    !     Inverted impedance matrix for symmetrical components of equivalent
    !     phase conductors is in p(1),...p(kk*(kk+1)/2) (real part) and z(1)
    !     ,...z(kk*(kk+1)/2) (imaginary part). Definition of kk same as for
    !     inverse susceptance matrix
    call output (metrik, p(1 :), z(1 :), unity, kk, ll5, ll3)
    go to 600
250 if (j56 .eq. 0) go to 251
    write (unit = lunt13) (p(i), z(i), i = 1, ip)
    !     segmented, 1, vax e/t can skip translation of rewind:
    rewind lunt13
251 do i = 1, ip
       p(i) = -p(i)
252    z(i) = -z(i)
    end do
    call cxred2 (p(1 :), z(1 :), workr1(1 :), workr2(1 :), kfull, ll0)
    !     Inverted impedance matrix for physical conductors is in p(1),...
    !     p(ip) (real part) and z(1),...z(ip) (imaginary part).
    call output (metrik, p(1 :), z(1 :), unity, kfull, ll5, ll1)
    if (j56 .eq. 0) go to 600
    read (unit = lunt13) (p(i), z(i), i = 1, ip)
    !     Segmented, 1, vax e/t can skip translation of rewind:
    rewind lunt13
    go to 201
253 if (j56 .eq. 0) go to 254
    write (unit = lunt13) (p(i), z(i), i = 1, kp)
    !     Segmented, 1, vax e/t can skip translation of rewind:
    rewind lunt13
254 go to 45454
    !  254 if ( lastov .eq. 39  .or.
    !     1          kcirct  .gt.  3 ) go to 45454
256 call symm (p(1 :), z(1 :), unity, kcirct, kk)
    !     Impedance matrix for symmetrical components of equivalent phase
    !     conductors is in p(1),...p(kk*(kk+1)/2) (real part) and z(1),...
    !     z(kk*(kk+1)/2) (imaginary part). Definition of kk same as for
    !     inverse susceptance matrix.
    if (j9out .gt. 0 ) call output (metrik, p(1 :), z(1 :), unity, kk, ll6, ll3)
    if (j6 .eq. 0) go to 9998
    rzero = p(1)
    xzero = z(1)
    rpos = p(ll5)
    xpos = z(ll5)
    zo = sqrtz (rzero ** 2 + xzero ** 2)
    z1 = sqrtz (rpos ** 2 + xpos ** 2)
    zso = sqrtz (zo / yzero)
    zs1 = sqrtz (z1 / ypos)
    zoang = atan2z (xzero, rzero) * onehaf
    z1ang = atan2z (xpos, rpos) * onehaf
    propo = sqrtz (zo * yzero)
    prop1 = sqrtz (z1 * ypos)
    zsodrg = zoang * picon - 45.0d0
    zs1drg = z1ang * picon - 45.0d0
    d2 = twopi / 8.0d0
    alphao = propo * cosz (zoang + d2)
    alpha1 = prop1 * cosz (z1ang + d2)
    beto = propo * sinz (zoang + d2)
    bet1 = prop1 * sinz (z1ang + d2)
    go to 2254
45454 d13 = twopi * freq
!!!!      write (*,*) ' ready for z avg.  p(1:6) =',
!!!!     1                              ( p(i), i=1, 6 )
!!!!      write (*,*) ' ready for z avg.  z(1:6) =',
!!!!     1                              ( z(i), i=1, 6 )
!!!!      write (*,*) ' imodal, nfreq =',  imodal, nfreq
    !      if (imodal .ne. 0)    go to 2254
    if (lastov .eq. 39) go to 6256
    if (imodal .ne. 0) go to 256
    go to 6276
6256 if (nfreq .eq. 1) go to 9998
    if (imodal .eq. 0) go to 6276
    if (nfreq .lt. 3) go to 9998
    go to 3006
    !     if (lastov .eq. 39  .and.  nfreq  .eq. 1)    go to 9998
!!!c  dor = p(1)
!!!c  doi = z(1)
!!!c  d1r = p(ll5)
!!!c  d1i = z(ll5)
6276 rdiag = 0
    xdiag = 0
    roff = 0
    xoff = 0
    n55 = 1
    if (kcirct .lt. 2) go to 5059
    if (mspedb .ne. 1) go to 5090
    rcoup = 0.0d0
    xcoup = 0.0d0
    do 5030  i = 1, 6
       rdiag = rdiag + p(n55)
       xdiag = xdiag + z(n55)
       if (n55 .eq. 10) go to 5028
       jm = n55 - i + 1
       if (n55 .ge. 15) jm = jm + 3
       do jp = jm, n55 - 1
          roff = roff + p(jp)
5025      xoff = xoff + z(jp)
       end do
5028   n55 = n55 + i + 1
5030 end do
    rdiag = rdiag / 6
    xdiag = xdiag / 6
    roff = roff / 6
    xoff = xoff / 6
    n55 = 6
    do jp = 4, 6
       j = n55 + 1
       do i = j, j + 2
          rcoup = rcoup + p(i)
5040      xcoup = xcoup + z(i)
       end do
       n55 = n55 + jp
5050 end do
    rcoup = rcoup / 9
    xcoup = xcoup / 9
    xtir(1) = rdiag + 2 * roff + 3 * rcoup
    xtii(1) = xdiag + 2 * xoff + 3 * xcoup
    xtir(2) = xtir(1) - 6 * rcoup
    xtii(2) = xtii(1) - 6 * xcoup
    xtir(3) = rdiag - roff
    xtii(3) = xdiag - xoff
    if (lastov .eq. 1) go to 5080
    if (lastov .ne. 39 .and. kexact .ne. 88333) go to 9998
    do i = 1, 3
       zmmag = sqrtz (xtir(i) ** 2 + xtii(i) ** 2)
       zmang = atan2z (xtii(i), xtir(i))
       ymmag = sqrtz (gmode ** 2 + dummi(i) ** 2)
       ymang = atan2z (dummi(i), gmode)
       zsurge(i) = sqrtz (ymmag / zmmag)
       theta2(i) = (ymang - zmang) / 2.0d0
       d1 = sqrtz (zmmag * ymmag )
       d2 = (zmang + ymang) / 2.0d0
       er(i) = d1 * cosz (d2) * fmipkm
       ei(i) = d1 * sinz (d2) * fmipkm
       if (kexact .ne. 88333) go to 5053
       er(i) = d1 * cosz (d2) * distsv
       ei(i) = d1 * sinz (d2) * distsv
    end do
5053 continue
    do i = 1, kcirct
       if (i .gt. 2) go to 5058
       write (unit = lunit9) d13, zsurge(i), theta2(i), er(i), ei(i)
       go to 5060
5058   write (unit = lunit9) d13, zsurge(3), theta2(3), er(3), ei(3)
5060 end do
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 5065) freq, kcirct, (zsurge(i), theta2(i), er(i), ei(i), i = 1, 3)
5065 format ('  at 5065, freq,kcirct,zsurge,theta2,er,ei are: ', /, e10.3, i3, 12e9.2)
    go to 9998
5080 write (unit = lunit7, fmt = 228)
    do i = 1, 3
       zsurge(i) = sqrtz (xtii(i) / dummi(i))
       er(i) = d13 / sqrtz (xtii(i) * dummi(i))
5081 end do
    if (iprsup .ge. 1) write (unit = lunit6, fmt = *) ' zsurge, er = ', (zsurge(i), er(i), i = 1, 3)
    do i = 1, kcirct
       if (i .gt. 1) go to 5082
       write (unit = lunit7, fmt = 140) i, brname(2 * i - 1), brname(2 * i), xtir(i), zsurge(i), er(i), dist
       go to 5088
5082   if (i .gt. 2) go to 5084
       write (unit = lunit7, fmt = 140) i, brname(2 * i - 1), brname(2 * i), xtir(3), zsurge(3), er(3), dist
       go to 5088
5084   if (i .gt. 3) go to 5086
       write (unit = lunit7, fmt = 140) i, brname(2 * i - 1), brname(2 * i), xtir(2), zsurge(2), er(2), dist
       go to 5088
5086   if (i .le. 9) write (unit = lunit7, fmt = 1475) i, brname(2 * i - 1), brname(2 * i)
       if (i .gt. 9) write (unit = lunit7, fmt = 9475) i, brname(2 * i - 1), brname(2 * i)
9475   format (i2, 2a6)
5088 end do
    write (unit = lunit7, fmt = 29)
    go to 9998
5059 xtir(1) = p(1)
    xtii(1) = z(1)
    go to 480
!!!!      write (*,*) ' enter do 478 loop.   kcirct =',  kcirct
5090 do i = 1, kcirct
       rdiag = rdiag + p(n55)
       xdiag = xdiag + z(n55)
       do jp = n55 - i + 1, n55 - 1
          roff = roff + p(jp)
479       xoff = xoff + z(jp)
       end do
478    n55 = n55 + i + 1
    end do
    rdiag = rdiag / kcirct
    xdiag = xdiag / kcirct
    roff = roff / ( kcirct * ( kcirct - 1 ) / 2 )
    xoff = xoff / ( kcirct * ( kcirct - 1 ) / 2 )
!!!!      write (*,*) ' average of diagonals,  rdiag, xdiag =',  rdiag,
!!!!      write (*,*) ' average of off-diag,  roff, xoff =',  roff, xoff
    xtir(1) = rdiag + (kcirct - 1) * roff
    xtii(1) = xdiag + (kcirct - 1) * xoff
    xtir(2) = rdiag - roff
    xtii(2) = xdiag - xoff
    if (lastov .eq. 39 .or. lastov .eq. 45 .or. kexact .eq. 88333) go to 480
    if (iprsup .lt. 1) go to 1228
    write (unit = lunit6, fmt = *) ' gmode,xtir, xtii, , dummi =', gmode, (xtir(i), xtii(i), dummi(i), i = 1, 2)
1228 write (unit = lunit7, fmt = 228)
228 format ('$vintage, 1')
    do i = 1, 2
       zsurge(i) = sqrtz (xtii(i) / dummi(i))
       er(i) = d13 / sqrtz(xtii(i) * dummi(i))
5092 end do
    do i = 1, kcirct
       if (i .gt. 2) go to 1473
       write (unit = lunit7, fmt = 140) i, brname(2 * i - 1), brname(2 * i), xtir(i), zsurge(i), er(i), dist
140    format ('-', i1, 2a6, 12x, 4e12.5, ' 1', 2x)
       go to 1480
1473   if (i .le. 9) write (unit = lunit7, fmt = 1475) i, brname(2 * i - 1), brname(2 * i)
       if (i .gt. 9) write (unit = lunit7, fmt = 9475) i, brname(2 * i - 1), brname(2 * i)
1475   format ('-', i1, 2a6)
1480 end do
    write (unit = lunit7, fmt = 29)
29  format ('$vintage, 0')
    go to 256
480 do i = 1, nmode
       zmmag = sqrtz (xtir(i) ** 2 + xtii(i) ** 2)
       zmang = atan2z (xtii(i), xtir(i))
       ymmag = sqrtz (gmode ** 2 + dummi(i) ** 2)
       ymang = atan2z (dummi(i), gmode)
       zsurge(i) = sqrtz (ymmag / zmmag)
       theta2(i) = (ymang - zmang) / 2.0d0
       d1 = sqrtz (zmmag * ymmag)
       d2 = (zmang + ymang) / 2.0d0
       er(i) = d1 * cosz (d2) * fmipkm
       ei(i) = d1 * sinz (d2) * fmipkm
       if (kexact .ne. 88333) go to 5093
       er(i) = d1 * cosz (d2) * distsv
       ei(i) = d1 * sinz (d2) * distsv
5093 end do
    do i = 1, kcirct
       if (i .gt. 1) go to 5098
       write (unit = lunit9) d13, zsurge(i), theta2(i), er(i), ei(i)
       go to 5094
5098   write (unit = lunit9) d13, zsurge(2), theta2(2), er(2), ei(2)
5094 end do
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 5095) freq, kcirct, (zsurge(i), theta2(i), er(i), ei(i), i = 1, 2)
5095 format ('  At 5095, freq,kcirct,zsurge,theta2,er,ei are: ', /, e10.3, i3, 8e12.3)
    go to 9998
2254 rzero1 = rzero
    rpos1 = rpos
    xzero1 = xzero
    xpos1 = xpos
    yzero1 = yzero
    ypos1 = ypos
    zo=1000.0d0 / omega
    z1=1000000.0d0 / omega
    xzero = xzero * zo
    xpos = xpos * zo
    yzero = yzero * z1
    ypos = ypos * z1
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 13012) ipunch
13012 format (' At 13012, ipunch =', i10)
    if (ipunch .eq. 0 .or. ipunch .eq. 44) go to 13039
    if (ipunch .ne. 1) go to 13011
    write (unit = lunit7, fmt = 3011) alphao, beto, alpha1, bet1, freq
3011 format (5e15.5)
    go to 13039
13011 if (mfrqpr .gt. 0) write (unit = lunit6, fmt = 3012) alphao, beto, rzero, xzero, yzero, alpha1, bet1, rpos, xpos, ypos, freq
3012 format (1x, e11.4, 9e12.4, e12.5)
    if (ipunch .ne. 2) go to 13019
    write (unit = lunit7, fmt = 13015) rzero, xzero, freq
13015 format (3e16.8)
    go to 13039
13019 if (ipunch .ne. 3) go to 13024
    write (unit = lunit7, fmt = 13015) rpos, xpos, freq
    go to 13039
13024 if (ipunch .ne. 88) go to 13031
    if (metrik .eq. 0) go to 13026
    rzero = rzero * fmipkm
    xzero = xzero * fmipkm
13026 write (unit = lunit9) rzero, xzero, freq
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 23024) rzero, xzero, freq
23024 format (' Rzero, Xzero and freq on unit9', 3e15.6)
    go to 13039
13031 if (ipunch .eq. 89) go to 13035
    kill = 162
    lstat(19) = 13031
    go to 9200
13035 if (metrik .eq. 0) go to 13037
    rpos = rpos * fmipkm
    xpos = xpos * fmipkm
13037 write (unit = lunit9) rpos, xpos, freq
13039 if (ik .gt. 0) go to 3006
3010 vol = omega / beto
    alphao = alphao * valu5
    alpha1 = alpha1 * valu5
    v1l = omega / bet1
    waveo = twopi / beto
    wave1 = twopi / bet1
    if (metrik .eq. 1) go to 9992
    write (unit = lunit6, fmt = 9991) zso, zsodrg, alphao, vol, waveo, rzero, xzero1, yzero1, zs1, zs1drg, alpha1, v1l, wave1, rpos, xpos1, ypos1
9991 format ('0sequence', 6x, 'surge impedance', 7x, 'attenuation   velocity    wavelength   resistance    reactance   susceptance', /, 9x, 'magnitude(Ohm) angle(degr.)   dB/mile      miles/s       miles Ohm/mile     Ohm/mile     mho/mile', /, '   zero  ', 8e13.5, /, ' positive', 8e13.5)
    go to 9998
9992 alphao = alphao * fmipkm
    alpha1 = alpha1 * fmipkm
    vol = vol / fmipkm
    v1l = v1l / fmipkm
    waveo = waveo / fmipkm
    wave1 = wave1 / fmipkm
    rzero1 = rzero1 * fmipkm
    rpos1  = rpos1  * fmipkm
    xzero1 = xzero1 * fmipkm
    xpos1 = xpos1 * fmipkm
    yzero1 = yzero1 * fmipkm
    ypos1 = ypos1 * fmipkm
    write (unit = lunit6, fmt = 9993) zso, zsodrg, alphao, vol, waveo, rzero1, xzero1, yzero1, zs1, zs1drg, alpha1, v1l, wave1, rpos1, xpos1, ypos1
9993 format ('0sequence', 6x, 'surge impedance', 7x, 'attenuation   velocity    wavelength   resistance    reactance   susceptance', /, 9x, 'magnitude(Ohm) angle(degr.)    dB/km         km/s          km Ohm/km       Ohm/km       mho/km ', /, '   zero  ', 8e13.5, /, ' positive', 8e13.5)
9998 if (kexact .ne. 88333) go to 9996
    n1 = 1
    n9sq = kcirct ** 2
    if (mspedb .eq. 1) go to 9540
    k = 1
    do j=1, kcirct
       do i=1, kcirct
          dummr(k) = 1.0d0
          if (i .eq. j .and. i .ne. 1) dummr(k) = 1 - i
          if (i .gt. j .and. j .ne. 1) dummr(k) = 0.0d0
5955      k = k + 1
       end do
5960 end do
    do i =1, n9sq, kcirct
       k = i + kcirct - 1
       temp = 0.0d0
       do j = i, k
5965      temp = temp + dummr(j) ** 2
       end do
       temp = sqrtz (temp)
       do  j = i, k
          tixf(n1) = dummr(j) / temp
          work1(n1) = 0.0d0
5968      n1 = n1 + 1
       end do
5970 end do
    go to 9994
9540 d1 = 2.0d0
    d2 = 6.0d0
    d3 = 1.0d0 / sqrtz (d1)
    d4 = 1.0d0 / sqrtz (d2)
    do j = 1, 36
       if (j .gt. 9) go to 15975
       tixf(n1) = d4
       go to 25960
15975  if (j .gt. 12) go to 15980
       tixf(n1) = -d4
       go to 25960
15980  if (j .gt. 13) go to 15985
       tixf(n1) = d3
       go to 25960
15985  if (j .gt. 14) go to 15990
       tixf(n1) = -d3
       go to 25960
15990  if (j .gt. 18) go to 15991
       tixf(n1) = 0.0d0
       go to 25960
15991  if (j .gt. 20) go to 15992
       tixf(n1) = d4
       go to 25960
15992  if (j .gt. 21) go to 15993
       tixf(n1) = -d1 * d4
       go to 25960
15993  if (j .gt. 27) go to 15994
       tixf(n1) = 0.0d0
       go to 25960
15994  if (j .gt. 28) go to 15995
       tixf(n1) = d3
       go to 25960
15995  if (j .gt. 29) go to 15996
       tixf(n1) = -d3
       go to 25960
15996  if ( j  .gt.  33 )  go to 15997
       tixf(n1) = 0.0
       go to 25960
15997  if (j .gt. 35) go to 15999
       tixf(n1) = d4
       go to 25960
15999  tixf(n1) = -d1 * d4
25960  work1(n1) = 0.0d0
       n1 = n1 + 1
25965 end do
9994 write (unit = lunit9) (tixf(i), i = 1, n9sq), (work1(i), i = 1, n9sq)
9996 if (j56 .eq. 0) go to 600
    read (unit = lunt13) (p(i), z(i), i = 1, kp)
    !     segmented, 1, vax e/t can skip translation of rewind:
    rewind lunt13
    go to 203
    !                          End of routines for z-printing and inversions
    !
    !                                Routines for p-printing and inversions
36  i2 = 1
    !     Inverse susceptance or capacitance matrix for physical conductors
    !     is in p(1),...p(ip).
    if (iw .eq. 0) i2 = i2 + 1
    call output (metrik, p(1 :), z(1 :), switch, kfull, i2, ll1)
    go to 31
37  i2 = 3
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 4444) iw, ip, ll0, ll1
4444 format (/, ' At 4444.  iw, ip, ll0, ll1=', 4i10)
    if (iw .eq. 0) i2 = i2 + 1
    do i = 1, ip
371    z(i) = -p(i)
    end do
    call redu44 (z(1 :), workr1(1 :), kfull, ll0)
    if (kill .gt. 0) go to 9200
    !     Susceptance or capacitance matrix for physical conductors is in
    !     z(1),...z(ip).
    call output (metrik, z(1 :), p(1 :), switch, kfull, i2, ll1)
    go to 32
38  i2 = 1
    !     inverse susceptance or capacitance matrix for equivalent phase
    !     conductors is in p(1),...p(kp)
    !     to store wc(inv) or c(inv) for modal analysis
    if (iprsup .ge. 1) write (unit = *, fmt = *) ' At s.n. 38,  p(1) =', p(1)
    if (imodal .le. 0) go to 403
    do i = 1, kp
404    xwc(i) = p(i)
    end do
403 if (iw .eq. 0) i2 = i2 + 1
    if (j2out .gt. 0) call output (metrik, p(1 :), z(1 :), switch, kcirct, i2, ll2)
    if (iprsup .ge. 4) write (unit = lunit6, fmt = 2654) j3, kcirct, j56, kp, iw, i2, j2, (p(i), i = 1, kp)
2654 format (/, " Before 'C'-matrix call to  'symm' .      j3  kcirct     j56      kp,      iw      i2      j2", /, 36x, 7i8, /, ' (p(i), i = 1, kp)  follow ....', /, (1x, 8e16.7))
    if (j3 .eq. 0 .or. kcirct .lt. 2) go to 33
    if (j56 .eq. 0) go to 40
    write (unit = lunt13) (p(i), i = 1, kp)
    !     Segmented, 1, vax e/t can skip translation of rewind:
    rewind lunt13
40  call symm (p(1 :), z(1 :), switch, kcirct, kk)
    i2 = 1
    if (iw .eq. 0) i2 = i2 + 1
    !     Inverse susceptance or capacitance matrix for symmetrical compo-
    !     nents of equivalent phase conductors (grouped as 3-phase circuits
    !     in order 1-2-3, 4-5-6 etc.) is in p(1),...p(kk*(kk+1)/2) for real
    !     part and z(1),...z(kk*(kk+1)/2) for imaginary part with kk=highest
    !     number of equivalent conductor being a multiple of 3.
    if (ik .le. 0) call output (metrik, p(1 :), z(1 :), unity, kk, i2, ll3)
    if (j56 .eq. 0) go to 33
    read (unit = lunt13) d1
    read (unit = lunt13) (p(i),i = 1, kp)
    go to 33
39  i2 = 3
    if (iw .eq. 0) i2 = i2 + 1
    do i = 1, kp
391    p(i) = -p(i)
    end do
    call redu44 (p(1 :), workr1(1 :), kcirct, ll0)
    if (kill .gt. 0) go to 9200
    !     Susceptance or capacitance matrix for equivalent phase conductors
    !     is in p(1),...p(kp).
    if (imodal .eq. 1) go to 1378
    if (iprsup .lt. 1) go to 5124
    write (unit = *, fmt = *) ' Ready for [c] avg.  p(1:6) =', (p(i), i = 1, 6)
    write (unit = lunit6, fmt = *) ' Begin averaging of capacitance.'
    write (unit = lunit6, fmt = *) ' 1st diagonal,  p(1) =', p(1)
    write (unit = lunit6, fmt = *) ' 1st off-diagonal,  p(2) =', p(2)
    write (unit = lunit6, fmt = *) ' =======  iw, kcirct, freq =', iw, kcirct, freq
5124 cdiag = 0
    coff = 0
    n55 = 1
    if (mspedb .ne. 1) go to 5190
    ccoup = 0.0d0
    do i = 1, 6
       cdiag = cdiag + p(n55)
       if (n55 .eq. 10) go to 5128
       jm = n55 - i + 1
       if (n55 .ge. 15) jm = jm + 3
       do jp = jm, n55 - 1
          coff = coff + p(jp)
5125   end do
5128   n55 = n55 + i + 1
5130 end do
    cdiag = cdiag / 6
    coff = coff / 6
    if (iprsup .ge. 1) write (unit = lunit6, fmt = *) ' cdiag, coff are ', cdiag, coff
    n55 = 6
    do jp = 4, 6
       j = n55 + 1
       do i = j, j + 2
          ccoup = ccoup + p(i)
5140   end do
       n55 = n55 + jp
5150 end do
    ccoup = ccoup / 9
    dummi(1) = cdiag + 2 * coff + 3 * ccoup
    dummi(2) = dummi(1) - 6 * ccoup
    dummi(3) = cdiag - coff
    if (iprsup .ge. 1) write (unit = lunit6, fmt = *) ' At 5150, dummi =', (dummi(i), i = 1, 3)
    if (iw .eq. 0) go to 1378
    d13 = twopi * freq
    do i = 1,3
       dummi(i) = dummi(i) * d13
1260 end do
    go to 1378
5190 if (kcirct .lt. 2) go to 1379
    do i = 1, kcirct
       cdiag = cdiag + p(n55)
       do jp = n55 - i + 1, n55 - 1
379       coff = coff + p(jp)
       end do
378    n55 = n55 + i + 1
    end do
    cdiag = cdiag / kcirct
    coff = coff / (kcirct * (kcirct - 1) / 2)
    !     write (*,*) ' average of diagonals,  cdiag =',  cdiag
    !     write (*,*) ' average of off-diag,   coff =',   coff
    dummi(1) = cdiag + (kcirct - 1) * coff
    dummi(2) = cdiag -  coff
    go to 2379
1379 dummi(1) = p(1)
    dummi(2) = 0.0d0
2379 if (iw .eq. 0) go to 1378
    d13 = twopi * freq
    dummi(1) = dummi(1) * d13
    dummi(2) = dummi(2) * d13
1378 if (j5out .gt. 0) call output (metrik, p(1 :), z(1 :), switch, kcirct, i2, ll2)
    zero = 0.0d0
    if (ialter .le. 0) go to 3741
    write (unit = lunit3) (zero, jj = 1, kp)
    write (unit = lunit3) (p(jj), jj = 1, kp)
    if (iprsup .le. 0) go to 3741
    write (unit = lunit6, fmt = 7428) (p(jj), jj = 1, kp)
7428 format (/, ' Output  (c)  for Semlyen.', /, (1x, 8e16.4))
3741 if (idist .eq. 0) go to 393
    do i = 1, kp
392    yd(i) = p(i)
    end do
393 if (j6 .eq. 0 .or. kcirct .lt. 2) go to 80
    call symm (p(1), z(1), switch, kcirct, kk)
    i2 = 3
    if (iw .eq. 0) i2 = i2 + 1
    !     Susceptance or capacitance matrix for symmetrical components of
    !     equivalent phase conductors is in p(1), ..., p(kk * (kk + 1) / 2) for real
    !     part and z(1), ..., z(kk * (kk + 1) / 2) for imaginary part. Definition of
    !     kk same as for inverse susceptance matrix
    if (j6out .gt. 0) call output (metrik, p(1 :), z(1 :), unity, kk, i2, ll3)
    if (ipunch .lt. 88) go to 399
    if (ipunch .gt. 89) go to 399
    if (ik .gt. 0) go to 399
    if (liu .ne. 0) go to 399
    czero = p(1) / tenm6
    cpos = p(ll5) / tenm6
    if (iw .ne. 0) go to 394
    czero = czero / omega
    cpos = cpos / omega
394 if (ipunch .ne. 88)  go to 395
    if (metrik .eq. 1) czero = czero * fmipkm
    write (lunit9) czero
    if (iprsup .ge. 1) write (lunit6, 98765) czero
98765 format (' Czero on unit9', e15.6)
    go to 396
395 if (metrik .eq. 1) cpos = cpos * fmipkm
    write (lunit9) cpos
    if (iprsup .ge. 1) write (lunit6, 397) cpos
397 format (' Cpos on unit9', e15.6)
396 liu = 1
399 yzero = p(1)
    ypos = p(ll5)
    if (iw .eq. 0) go to 80
    yzero = yzero * omega
    ypos = ypos * omega
    go to 80
    !                          end of routines for p-printing and inversions
    !
    !                                        changes in conductor data
500 icount = 0
    k = 0
    if (kcirct .eq. 0) go to 503
    i2 = lphpl1 - kcirct
    i = lphase
    kk = -1
501 k = k + 1
    itbic(k) = ic(i)
    tbtb2(k) = tb2(i)
    itbtb3(k) = itb3(i)
    tbr(k) = r(i)
    tbd(k) = d(i)
    tbg(k) = gmd(i)
    tbx(k) = x(i)
    tby(k) = y(i)
    tbtext(k) = text(i)
    if (i .eq. i2) go to 502
    i = i + kk
    go to 501
502 if (icount .gt. 0) go to 504
503 icount = 1
    i = 1
    i2 = ktot
    kk = 1
    if (ktot .gt. 0) go to 501
    !     read input card using cimage
504 call cimage
    read (unit = abuff, fmt = 4230) bufsem
    if (ialter .ne. 2) write (lunit2, 4230) bufsem
    read (unit = abuff, fmt = 11) i1, y1, r1, i2, g1, d1, x1, h1, h2, d8, d9, bus1, i3
    if (i1 .ne. 0) go to 4423
    if (y1 .ne. 0) go to 4423
    if (r1 .ne. 0) go to 4423
    if (i2 .ne. 0) go to 4423
    if (g1 .ne. 0.0) go to 4423
    if (d1 .ne. 0.0) go to 4423
    if (x1 .ne. 0.0) go to 4423
    if (h1 .ne. 0.0) go to 4423
    if (h2 .ne. 0.0) go to 4423
    if (d8 .ne. 0.0) go to 4423
    if (d9 .ne. 0.0) go to 4423
    if (bus1 .eq. blank) go to 507
4423 isw = 1
    i = 0
1505 i = i + 1
    if (i .gt. kfull) go to 505
    if (tbtext(i) .eq. bus1) go to 506
    go to 1505
505 if (isw .eq. 2) go to 504
    kfull = kfull + 1
    if (kfull .gt. lphase) go to 7
    i = kfull
    itbic(i) = i1
    tbtb2(i) = y1
    tbr(i) = r1
    itbtb3(i) = i2
    tbg(i) = g1
    tbd(i) = d1
    tbx(i) = x1
    tbtext(i) = bus1
    !  assign 711 to moon
    moon = 711
    go to 4280
506 continue
    read (unit = abuff, fmt = 4281) bus1
    if ( bus1  .ne.  blank )  itbic(i) = i1
    read (unit = abuff, fmt = 4286) bus1
    if ( bus1  .ne.  blank )  tbtb2(i) = y1
    read (unit = abuff, fmt = 4292) bus1, bus2
    if ( bus1  .ne.  blank )  tbr(i) = r1
    if ( bus2  .ne.  blank )  tbr(i) = r1
    read (unit = abuff, fmt = 4297) bus1
    if ( bus1  .ne.  blank )  itbtb3(i) = i2
    read (unit = abuff, fmt = 4306) bus1, bus2
    if ( bus1  .ne.  blank )  tbg(i) = g1
    if ( bus2  .ne.  blank )  tbg(i) = g1
    read (unit = abuff, fmt = 4313) bus1, bus2
    if ( bus1  .ne.  blank )  tbd(i) = d1
    if ( bus2  .ne.  blank )  tbd(i) = d1
    isw = 2
    go to 1505
711 if (i3 .le. 1)   go to 716
    i4 = 1
    xx = tbx(i)
    yy = tby(i)
    angl = (pi - twopi  / i3) / 2.0
    dangl = twopi / i3
    radius = d8 / (24.0 * cosz(angl))
    d9r = d9 / picon
713 tbx(i) = xx +radius * cosz(d9r -dangl * i4)
    tby(i) = yy +radius * sinz(d9r -dangl * i4)
    if (i4 .eq. i3)   go to 716
    i4 = i4 +1
    i = i +1
    itbic(i) = itbic(i-1)
    tbtb2(i) = tbtb2(i-1)
    tbr(i) = tbr(i-1)
    itbtb3(i) = itbtb3(i-1)
    tbg(i) = tbg(i-1)
    tbd(i) = tbd(i-1)
    tbtext(i) = tbtext(i-1)
    go to 713
716 kfull = i
    go to 504
507 i = kfull + 1
    go to 6
    !
    !                            extending the matrices for specified length
600 if (dist.eq.0.) go to 16
    j1516=j15+j16
    if (j13+j14+j1516.eq.0) go to 16
    if ( metrik .eq. 1 ) go to 610
    write (lunit6,601) dist
601 format (/, ' Matrices for line length =', e13.6, ' miles.')
    go to 612
610 distm = dist / fmipkm
    write ( lunit6,611 ) distm
611 format (/, ' Matrices for line length =', f8.3, ' km.')
612 continue
    if (idist.eq.1) go to 603
    write (lunit6,602) kcirct
602 format ('+', 41x, 'Cannot be calculated with number of equivalent conductors=', i3)
    go to 16
603 ip=0
    d1 = fltinf
    k=0
604 k=k+1
    if (k .gt. kcirct) go to 605
    ip=ip+k
    h1= valu6*bd(ip)/yd(ip)
    if (absz(h1).lt.d1) d1=absz(h1)
    go to 604
605 if (iw .gt. 0) d1=d1/omega
    d1 = sqrtz(d1 * 2.0)
    i1 = dist / d1 + onehaf
    i2 = 1
    do isec = 1, 34
       if (i1 .le. i2) go to 608
606    i2 = i2 * 2
    end do
    write (unit = lunit6, fmt = 607)
607 format ('+', 41x, 'Cannot be calculated with number of necessary sections=2**33')
    go to 16
608 x1 = i2
    i1 = isec - 1
    deltad = dist / x1
    if (metrik .eq. 1) go to 613
    write (unit = lunit6, fmt = 609) i1, deltad
609 format (' Computed by connecting in cascade 2**', i2, ' equal sections of', e13.6, ' miles each.')
    go to 615
613 deltam = deltad / fmipkm
    write (unit = lunit6, fmt = 614) i1, deltam
614 format ('+', 41x, 'computed by connecting in series 2**', i2, ' equal sections of', f9.4, ' km each.')
615 continue
    f1 = unity / deltad
    f2 = deltad * onehaf
    if (iw .gt. 0) f2 = omega * f2
    kp = kcirct * (kcirct + 1) / 2
    kcir2 = kcirct + kcirct
    if (i1 .eq. 0) go to 670
    do i = 1, kp
       r1 = f1 * gd(i)
       x1 = f1 * bd(i)
       g1 = f2 * yd(i)
       d1 = x1 + g1
       p(i) = r1
       z(i) = d1
       gd(i) = r1 + r1
621    bd(i) = x1 + d1
    end do
    !                                  begin of loop for connecting sections
    icount = 0
622 icount = icount + 1
    if (icount .eq. isec) go to 650
    !                                       expanding matrix
    ip = 0
    i3 = kp
    k = 0
623 k = k + 1
    if (k .gt. kcirct) go to 626
    i2 = i3 + kcirct
    l4 = kp + k
    i = 0
624 i = i + 1
    l1 = ip + i
    l2 = i2 + i
    l3 = i3 + i
    r1 = p(l1)
    x1 = z(l1)
    h1 = r1 - gd(l1)
    h2 = x1 - bd(l1)
    p(l2) = r1 * 2.0
    z(l2) = x1 * 2.0
    p(l3) = h1
    z(l3) = h2
    gd(l1) = r1
    bd(l1) = x1
    if (i .eq. k) go to 625
    p(l4) = h1
    z(l4) = h2
    l4 = l4 + i + kcirct
    go to 624
625 ip = ip + k
    i3 = i2 + k
    go to 623
626 call cxred2 (p(1 :), z(1 :), workr1(1 :), workr2(1 :), kcir2, kcirct)
    go to 622
    !                                       output of matrices for length
650 do i = 1, kp
       gd(i) = gd(i) - p(i)
       bd(i) = bd(i) - z(i)
       p(i) = (p(i) - gd(i)) * 2.0d0
651    z(i) = (z(i) - bd(i)) * 2.0d0
    end do
673 if (j1516 .gt. 0) go to 660
652 if (j13 .eq. 0) go to 653
    !     transfer admittance matrix for equivalent cond. is in gd+j*bd.
    call output (ll0, gd(1 :), bd(1 :), unity, kcirct, ll7, ll2)
    !     shunt admittance matrix for equivalent cond. is in p+j*z.
    call output (ll0, p(1 :), z(1 :), unity, kcirct, ll8, ll2)
653 if (j14 .eq. 0) go to 654
    call symm (gd(1 :), bd(1 :), unity, kcirct, kk)
    !     transfer admittance matrix for symmetrical comp. is in gd+j*bd.
    call output (ll0, gd(1 :), bd(1 :), unity, kk, ll7, ll3)
    call symm (p(1 :), z(1 :), unity, kcirct, kk)
    !     shunt admittance matrix for symmetrical comp. is in p+j*z.
    call output (ll0, p(1 :), z(1 :), unity, kk, ll8 , ll3)
654 if (j1516 .eq. 0) go to 16
    do i = 1, kp
       n1 = 2 * lgdbd + 1 + i
       n2 = i + lgdbd
       gd(i) = -p(n2)
       bd(i) = -z(n2)
       p(i) = -p(n1)
655    z(i) = -z(n1)
    end do
    call cxred2 (gd(1 :), bd(1 :), workr1(1 :), workr2(1 :), kcirct, ll0)
    call cxred2 (p(1 :), z(1 :), workr1(1 :), workr2(1 :), kcirct, ll0)
    if (j15 .eq. 0) go to 656
    !     Transfer impedance matrix for equivalent conductors is in gd+j*bd.
    call output (ll0, gd(1 :), bd(1 :), unity, kcirct, ll9, ll2)
    !     Shunt impedance matrix for equivalent conductors is in p+j*z.
    call output (ll0,  p(1 :),  z(1 :), unity, kcirct, ll10, ll2)
656 if (j16 .eq. 0) go to 16
    call symm (gd(1 :), bd(1 :), unity, kcirct, kk)
    !     Transfer impedance matrix for symmetrical comp. is in gd+j*bd.
    call output (ll0, gd(1 :), bd(1 :), unity, kk, ll9, ll3)
    call symm (p(1 :), z(1 :), unity, kcirct, kk)
    !     shunt impedance matrix for symmetrical comp. is in p+j*z.
    call output (ll0, p(1 :), z(1 :), unity, kk, ll10, ll3)
    go to 16
660 i = lgdbd + 1
    call mover (gd(1 :), p(i :), kp)
    call mover (bd(1 :), z(i :), kp)
    i = 2 * i
    call mover (p(1 :), p(i :), kp)
    call mover (z(1 :), z(i :), kp)
    go to 652
670 f2 = f2 * 2.0d0
    do i=1, kp
       p(i) = 0.0d0
       z(i) = f2 * yd(i)
       gd(i) = f1 * gd(i)
671    bd(i) = f1 * bd(i)
    end do
    go to 673
9200 lstat(18) = nchain
    if (ialter .eq. 2) lunit5 = l5save
    lastov = nchain
    nchain = 51
9900 return
  end subroutine guts44

end module ovr44c

!
! end of file ovr44c.f90
!
