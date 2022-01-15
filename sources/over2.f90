!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over2.f90
!

module redcom
  implicit none

contains

  !
  ! subroutine cxred.
  !

  subroutine cxred (a, c, n, m)
    implicit none
    !    Elimination of variables m+1,...n in symmetric complex matrix with
    !    a=real part, c=imaginary part. a and c are
    !    stored as triangle (1 element for 1.column,2 for 2.column etc.).
    !    result is reduced matrix in columns 1,...m in case of reduction
    !    (m unequal 0) or negative inverse matrix in columns 1,...n in case
    !    of inversion (m=0).
    integer(4), intent(in) :: n, m
    real(8), intent(out) :: a(:), c(:)
    integer(4) :: i, i1, i2, ij, ik
    integer(4) :: j
    integer(4) :: k
    integer(4) :: l
    real(8) :: b(30), d(30)
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
       c(i) = c(i) + x * g2 + y * h2
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
       a(i) = b(l)
    end do
    go to 4
    !                                   end k-loop
9   i = ij + k
    go to 5
  end subroutine cxred

end module redcom

!
! subroutine over2.
!

subroutine over2
  use linemodel
  use blkcom
  use labcom
  use dekspy
  use com2
  use movcop
  use strcom
  use tracom
  use freedom
  implicit none
  !     %include  '//c/tsu/cables.ftn'
  character(6) :: chrpad(18)
  character(8) :: text1, text2, text3, text4, text5, text6
  character(8) :: text7, text8, text9, text10, text11, text12
  character(8) :: text13, text14, text15, text16, text17
  !
  !  dimension wk1(1)
  !  dimension infdli(1)
  !  dimension icrit(1)
  !
  !  equivalence (semaux(1), wk1(1))
  !  equivalence (namebr(1), infdli(1))
  !  equivalence (icrit(1), crit(1))
  !  equivalence (indtv(1), iaddrs)
  !  equivalence (indtv(2), itranm)
  !  equivalence (indtv(3), ityold)
  !  equivalence (indtv(4), ichtr2)
  !  equivalence (iprsov(39), nmauto)
  !     character*6   char6
  !     character*26  alphan
  !     data alphan  / 'abcdefghijklmnopqrstuvwxyz' /
  !
  integer(4) :: i, ibf, ibr2, ibr3, ibrnam, ibrter, icas
  integer(4) :: ideal, idumy, iend, ifk, ii, iihst, ikf, iml, inew, inoff1
  integer(4) :: inoff2, inoff3, inoff4, inoff5, inonam, interm, iold, ios
  integer(4) :: ioutmg, ips1, iref, isec, isecti, isfd, isgfd, iprint, it3, it32
  integer(4) :: itemq, itrans, itym1
  integer(4) :: j, j1, j2, jj, jj1, jj2, jk
  integer(4) :: k, keepb, keept, koff1, koff2, koff3, koff4, koff5
  integer(4) :: koff6, koff7, koff8, koff9, koff10, koff13, koff14, koff15
  integer(4) :: koff16, koff17, koff18, koff19, koff20, kph, kq, kq1
  integer(4) :: kreqab, ksat
  integer(4) :: l, ll, ll0, ll2, ll3, ll9
  integer(4) :: m, mpower, mread2, mxphas
  integer(4) :: n, n5, n6, n8, n9, n9sq, n14, n16, n24, ncoil, ncount, nfir, nfscan, nkq
  integer(4) :: nn11, nn13, nn17, nnn1, nodtop, np, nphs, nphs2, nphsu
  integer(4) :: nq, nsec, ntlin, nycmp
  real(8) :: cut1, cut2, cut3
  real(8) :: d1, d2, d3, d4, d5, d6
  real(8) :: rmag
  real(8) :: temp, turn1
  real(8) :: yzero
  !
  integer(4), pointer :: ichtr2 => indtv(4)
  integer(4), allocatable :: icrit(:)
  integer(4), pointer :: iaddrs => indtv(1)
  integer(4), pointer :: infdli(:) => namebr(1 :)
  integer(4), pointer :: itranm => indtv(2)
  integer(4), pointer :: ityold => indtv(3)
  integer(4), pointer :: nmauto => iprsov(39)
  real(8), pointer :: wk1(:) => semaux(1 :)
  !
  data text1   / 'stop c' /
  data text2   / 'ascade' /
  data text3   / 'use ab' /
  data text4   / 'use rl' /
  data text5   / 'cascad' /
  data text6   / 'ed pi ' /
  data text7   / 'transf' /
  data text8   / 'ormer ' /
  data text9   / 'three ' /
  data text10  / 'phase ' /
  data text11  / 'branch' /
  data text12  / ' name:' /
  data text13  / 'nonlin' /
  data text14  / 'es    ' /
  data text16  / 'lin000' /
  data text17  / 'nln000' /
  data ibrnam  / 0 /
  data inonam  / 0 /
  data nfscan  / 0 /
  !
  ll0 = size (transfer (crit, icrit))
  allocate (icrit(ll0))
  icrit = transfer (crit, icrit)
  if (lastov .ne. 44) go to 2000
  k = nphlmt  + 1                                           ! ready for next branch, lmfs case
  ibr = nphlmt
  ntot = 2 * nphlmt + 1
  it = 1
  ibr1 = 1
  do jk = 1, nphlmt
     m = jk + nphlmt
     l = 2 * jk
     !write (unit = bus(l), fmt = 2689) chlmfs(jk)(1 : 6)
     read (unit = chlmfs(jk)(1 : 6), fmt = 2689) bus(l)
2689 format (a6)
     !write (unit = bus(l+1), fmt = 2689) chlmfs(m)(1:6)
     read (unit = chlmfs(m)(1 : 6), fmt = 2689) bus(l + 1)
  end do
  go to 4199                                                ! go directly to use $include for lmfs pi model half
  ! following used rather than data statements due to common:
2000 ityold = 0
  kreqab = 0
  ichtr2 = 0
  iold = 0
  kph = 0
  ksat = 0
  ibr1 = 0
  nn17 = 0
  nycmp = 0
  isecti = 400
1000 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format (' Begin module "over2".')
  l27dep = lbrnch * lsiz27 / 2
  ll2 = 2
  ll3 = 3
  ll9 = 9
  mxphas = 0
  ifk = 0
  lcount = 0
  ! initialize counters for the -666 branches  * * * * * * * * * * * *
  ikf = 0
  isfd = 0
  ibf = 0
  isgfd = -100
  mread2 = 0
  icas = 0
  nrecur = 0
  model = 0
  if (lastov .eq. 4) go to 100
  ! do i = 1, lfsem
  !    sconst(i) = 0.0d0
  ! end do
  ! do i = 1, lbrnch
  !    imodel(i) = 0
  ! end do
  ! do i = 1, lbrnch
  !    litype(i) = 0
  ! end do
  call move0 (sconst, lfsem)
  call move0 (imodel, lbrnch)
  call move0 (litype, lbrnch)
  iaddrs = 1
100 it2 = it + 2
  if (kill .gt. 0) go to 9200
  iprint = 3
  lstat(19) = 100
  if (it2 .gt. ldata) go to 9000
  if (ksat .ge. 0) go to 3986
  ksat = 1
  go to 100
3986 if (ntot .gt. 1) go to 3988
  if (mread2 .eq. 0) go to 3989
  ! read input card using cimage
3988 call cimage
3989 mread2 = 1
  read (unit = abuff, fmt = 2005) char80
2005 format (a)
  if (to_lower (char80(1 : 8)) .ne. 'no. of p') go to 2100  ! not lmfs case
  do jk = 16, 80
     if (char80(jk : jk) .ne. ' ') go to 2008
  end do
  write (unit = lunit(6), fmt = 2006)
2006 format (' No integer value (format i1) was inputted for number of phases in this LMFS case, run will be aborted. Stop.')
  stop
2008 read (unit = char80(jk : jk), fmt = 2189) nphlmt
2189 format (i1)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 2010) nphlmt
2010 format ('+LMFS of a ', i1, '-phase circuit')
  call cimage
  read (unit = abuff, fmt = 2005) char80
  if (to_lower (char80(1 : 7)) .eq. 'sending') go to 2020
  write (unit = lunit(6), fmt = 2015)
2015 format ('  Card of sending end nodes:  of this LMFS case is not inputted, and case is to be aborted. Stop.')
  stop
2020 read (unit = abuff, fmt = 2025) (chlmfs(jk), jk = 1, nphlmt)
2025 format (18x, 9a6)
  do jk = 1, nphlmt
     if (chlmfs(jk)(1 : 6) .eq. '      ') go to 2028
  end do
  go to 2030
2028 write (unit = lunit(6), fmt = 2029) nphlmt
2029 format (' Program observed an invalid blank node name for sending end of this ', i1, '-phase LMFS circuit. Case is to be stopped.')
  stop
2030 if (noutpr .eq. 0) write (unit = kunit6, fmt = 2035)
2035 format ('+Sending end node names (9a6 format)')
  call cimage
  read (unit = abuff, fmt = 2005) char80
  if (to_lower (char80(1 : 9)) .eq. 'receiving') go to 2040
  write (unit = lunit(6), fmt = 2038)
2038 format ('  Card of receiving end nodes: of this LMFS case is not inputted, and case is to be aborted. Stop.')
  stop
2040 read (unit = abuff, fmt = 2045) (chlmfs(jk), jk = nphlmt + 1, 2 * nphlmt)
2045 format (20x, 9a6)
  do jk = 1, nphlmt
     if (chlmfs(jk) (1 : 6) .eq. '      ') go to 2048
  end do
  go to 2050
2048 write (unit = lunit(6), fmt = 2049) nphlmt
2049 format (' Program observed an invalid blank name for receiving end of this ',i1, '-phase lmfs circuit. Case is to be stopped.')
  stop
2050 if (noutpr .eq. 0) write (unit = kunit6, fmt = 2055)
2055 format ('+Receiving end node names (9a6 format)')
  go to 100                                       ! end of three "/" cards for lmfs data case
2100 if (kolbeg .gt. 0) go to 6618
  read (unit = abuff, fmt = 1) itype, bus1, bus2, bus3, bus4
1 format (i2, 4a6, 9e6.2)
  go to 6621
6618 nfrfld = 1
  call free (voltbc)
  itype = int (voltbc(1), kind (itype))
  nfrfld = 4
  nright = -1
  call free (d1)
  bus1 = texta6(1)
  bus2 = texta6(2)
  bus3 = texta6(3)
  bus4 = texta6(4)
  nright = 0
6621 if ((iprsup .ge. 3) .and. (ibr1 .ge. 1)) write (unit = lunit(6), fmt = 7621) ibr1, ityold, kodsem(ibr1), itranm, itype, length(ibr1)
7621 format (' Cont. transp.?,  ibr1, ityold, kodsem(ibr1), itranm, itype, length(ibr1) =', 6i7)
  if (to_lower (bus2) .ne. text12) go to 7682
  n24 = 0
  if (to_lower (bus1) .ne. text11) go to 7677
  n1 = ibr + 1
  call namea6 (bus3, n24)
  namebr(n1) = n24
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 7674) bus3, n1
7674 format ('+Moniker ', '"', a6, '"', ' is for next branch  +', i6, ' .')
7675 ibrnam = ibrnam + 1
  if (ibrnam .gt. ibr) go to 7676
  namebr(ibrnam) = 1
  if (nmauto .eq. 0) go to 7675
  call nmincr (text16, ibrnam)
  n24 = 0
  call namea6 (text16, n24)
  namebr(ibrnam) = n24
  go to 7675
7676 go to 3988
7677 if (to_lower (bus1) .ne. text13) go to 7682
  n1 = inonl + 1
  call namea6 (bus3, n24)
  namesw(n1) = n24
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 7679) bus3, n1
7679 format ('+Moniker ', '"', a6, '"', ' is for next nonlinear  +', i6, ' .')
7680 inonam = inonam + 1
  if (inonam .gt. inonl) go to 7681
  namebr(inonam) = 1
  if (nmauto .eq. 0) go to 7680
  call nmincr (text17, inonam)
  n24 = 0
  call namea6 (text17, n24)
  namenl(inonam) = n24
  go to 7680
7681 go to 3988
7682 if (ibr1 .eq. 0 )  go to 36621
  if (ityold .ge. 0) go to 36621
  if (model .ge. 1) go to 36621                             ! model phases untransposed
  if ((kodsem(ibr1) .ne. 0) .and. (imodel(ibr1) .eq. 0)) go to 36621
  if ((kodsem(ibr1) .ne. 0) .and. (n13 .ne. 0)) go to 36621
  if (itranm .gt. 0) go to 36621
  if ((n13 .eq. 0) .and. (imodel(ibr1) .eq. -4)) ktrlsw(3) = 0
  if (itype .ge. 0) go to 26621
  itym1 = iabsz (itype) - 1
  if (imodel(ibr1) .eq. -3) go to 36621
  if (itym1 .eq. iabsz (length (ibr1))) go to 36621
26621 if ((length (ibr1) .ne. 3) .or. (ktrlsw(3) .eq. 0)) go to 8200
  litype(ibr1) = ktrlsw(3)
  go to  36621
8200 k = 1
  n9 = iabsz (length (ibr1))
  iadd = iaddrs + 2 * n9 * n9 - 1
  if (iadd .lt. lfd) go to 5950
  iprint = 21
  lstat(19) = 5950
  go to 9000
5950 n9sq = n9 * n9
  litype(ibr1) = -iaddrs
  if (kodebr(ibr1) .eq. -1) go to 15970
  do j = 1, n9
     do i = 1, n9
        volti(k) = 1.0d0
        if ((i .eq. j) .and. (i .ne. 1)) volti(k) = 1 - i
        if ((i .gt. j) .and. (j .ne. 1)) volti(k) = 0.0d0
        k = k + 1
     end do
  end do
  n1 = iaddrs
  do i = 1, n9sq, n9
     k = i + n9 - 1
     temp = 0.0d0
     do j = i, k
        temp = temp + volti(j) ** 2
     end do
     temp = sqrtz (temp)
     do j = i, k
        qfd(n1) = volti(j) / temp
        sfd(n1) = 0.0d0
        n1 = n1 + 1
     end do
  end do
  if ((ipunch .ne. -4) .and. (n13 .eq. 0.0d0)) go to 50050
  ifq = ifsem
  inoff1 = lbrnch
  inoff2 = 2 * lbrnch
  inoff3 = 3 * lbrnch
  inoff4 = 4 * lbrnch
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
  if (kgroup .ne. 1) wk1(koff19) = 1
  wk1(koff19) = wk1(koff19) + kgroup
  ncount = idm
  nphs = n9
  nphs2 = n9 ** 2
  nphsu = (nphs + 1) * nphs / 2
  ibr2 = ibr - nphs + 1
  ibr3 = ibr - nphs + 3
  ifq = ifq + 1
  infdli(inoff2 + ibr2) = ifq
  infdli(inoff1 + ibr2) = idm
  infdli(inoff4 + ibr2) = idu
  if (iprsup .ge. 4) write (unit = *, fmt = *) ' Begin to read qi. ntlin,iq,idm,idu,idt,idq= ', ntlin, iq, idm, idu, idt, idq
  nnn1 = iaddrs
  do i = 1, nphs2
     wk1(koff20 + ncount) = idq
     np = 1
     sconst(ifq) = qfd(nnn1)
     nnn1 = nnn1 + 1
     !     if (noutpr .eq. 0)
     !     1 write (*, 8262) np , sconst(ifq)
     !     8262 format ( 24h+ ti.  np, sconst(ifq) =, i5,  e15.5  )
     wk1(koff20 + ncount) = np
     j1 = 1
4888 j2 = j1 + 2
     m = np
     if (j2 .gt. m) j2 = m
     jj1 = j1 + ifq
     jj2 = j2 + ifq
     iml = j1
     do jj = jj1, jj2
        sconst(jj) = 0.0
        iml = iml + 1
     end do
     !     if (noutpr .eq. 0)
     !     1 write (*, 4413)   ( sconst(j), j=jj1, jj2 )
     !     4413 format ( 7h+ qk-i:, 3e14.5 )
     j1 = j1 + 3
     if (j1 .le. m) go to 4888
     !
     j1 = 1
4847 j2 = j1 + 2
     m = np
     if (j2 .gt. m) j2 = m
     nn11 = jj2 + j1
     nn13 = jj2 + j2
     iml = j1
     do jj = nn11, nn13
        sconst(jj) = 1.0d0
        iml = iml + 1
     end do
     !     if (noutpr .eq. 0)
     !     1 write (*, 8414)  ( sconst(j), j=j1, j2 )
     !     8414 format ( 7h+ qp-i:, 3e14.5 )
     j1 = j1 + 3
     if (j1 .le. m) go to 4847
     ifq = nn13 + 1
     ncount = ncount + 1
     idq = idq + np
  end do
  idq = idq - 1
  idq = 2 * idq
  idq = idq + 1
  ifq = nn13 + (nn13 - ifsem - nphs2 ) * 3 / 2 + 2 * nphs2
  ifsem = ifq
  infdli(inoff3 + ibr2) = nn13
  idm = idm + nphs2
  idu = idu + nphsu
  ifkc = ifkc + n9
  if (iprsup .ge. 4) write (unit = *, fmt = *) 'Next bunch of coupled branches. ntlin=, kcount=', ntlin, kcount
  itranm = model
  model = 0
  kgroup = 1
50050 if (length(ibr1) .eq. 3) ktrlsw(3) = -iaddrs
  go to 25970
15970 d1 = 2.0d0
  d2 = 6.0d0
  d3 = 1.0d0 / sqrtz (d1)
  d4 = 1.0d0 / sqrtz (d2)
  n1 = iaddrs
  do j = 1, 36
     if (j .gt. 9) go to 15975
     qfd(n1) = d4
     go to 25960
15975 if (j .gt. 12) go to 15980
     qfd(n1) = -d4
     go to 25960
15980 if (j .gt. 13) go to 15985
     qfd(n1) = d3
     go to 25960
15985 if (j .gt. 14) go to 15990
     qfd(n1) = -d3
     go to 25960
15990 if (j .gt. 18) go to 15991
     qfd(n1) = 0.0
     go to 25960
15991 if (j .gt. 20) go to 15992
     qfd(n1) = d4
     go to 25960
15992 if (j .gt. 21) go to 15993
     qfd(n1) = -d1 * d4
     go to 25960
15993 if (j .gt. 27) go to 15994
     qfd(n1) = 0.0d0
     go to 25960
15994 if (j .gt. 28) go to 15995
     qfd(n1) = d3
     go to 25960
15995 if (j .gt. 29) go to 15996
     qfd(n1) = -d3
     go to 25960
15996 if (j .gt. 33) go to 15997
     qfd(n1) = 0.0d0
     go to 25960
15997 if (j .gt. 35) go to 15999
     qfd(n1) = d4
     go to 25960
15999 qfd(n1) = -d1 * d4
25960 sfd(n1) = 0.0d0
     n1 = n1 + 1
  end do
25970 if (iprsup .lt. 1) go to 5971
  n1 = iaddrs
  n2 = n1 + n9sq - 1
  write (unit = lunit(6), fmt = 15968) n1, n2, (qfd(i), i = n1, n2), (sfd(i), i = n1, n2)
15968 format (' qfd(i) and sfd(i) for i =', i6, ' to ', i6, ' are', /, (1x, 8e15.7))
  write (unit = lunit(6), fmt = 25968) ibr1, iaddrs, litype(ibr1)
25968 format (' At 25968, ibr1, iaddrs and litype(ibr1) are', 3i10)
5971 iaddrs = iadd + 1
36621 itranm = 0
  if (bus1 .ne. blank) go to 110
  if (bus2 .ne. blank) go to 110
  if (itype .eq. 0) goto 4199
  if (itype .lt. 0 ) go to 110
  if (icas .eq. 0) nphcas = itype - 51
  icas = -1
  go to 147
4199 continue
  ! =================  line-model f-scan building of network ===
  if (kexact .ne. 88333) go to 5823
  ! write (*,*) ' check f-scan data.  ntot, ibr =',  ntot, ibr
  if (nfscan .gt. 0) go to 5823
  if (lastov .ne. 44) numrun = numrun + 1
  nfscan = nphlmt
  ! if ( nfscan*2 .eq. ntot - 1  .and.
  ! 1     nfscan .eq. ibr ) go to 1583
  ! if ( nfscan*2 .eq. ntot-1 )  go to 1583
  ! write (*,*) ' error f-scan data.  ntot, ibr =',  ntot, ibr
  ! stop
  ! 1583 do 5281  m= 2, ntot, 2
  ! n9 = m / 2
  ! char6 = 'send_'//alphan(n9:n9)
  ! read (char6,4207) bus(m)
  ! char6 = 'recv_'//alphan(n9:n9)
  ! read (char6,4207) bus(m+1)
  ! 4207 format ( a6 )
  ! 5281 continue
  do m = 1, 2 * nphlmt
     chrpad(m)(1 : 6) = chlmfs(m)(1 : 6)
     do k = 1, 6
        if (chlmfs(m)(k : k) .eq. ' ') chrpad(m)(k : k) = '#'
     end do
  end do
  if (numrun .ge. 2) go to 2540
  if (nphlmt .gt. 3) go to 2510
  write (unit = buff77, fmt = 3629) nphlmt, (chrpad(m)(1 : 6), m = 1, 2 * nphlmt)
3629 format ('$include, [tsu]linescanz', i1, '.thl ', 6(a6,1x))
  go to 2580
2510 do i = 1, 2
     if (i .eq. 1) write (unit = buff77, fmt = 2515) nphlmt, (chrpad(m)(1 : 6), m = 1, 7)
2515 format ('$include,[tsu]linescanz', i1, '.thl ', 7(a6,1x), '$$')
     if (i .eq. 2) write (unit = buff77, fmt = 2516) (chrpad(m)(1 : 6), m = 8, 2 * nphlmt)
2516 format ('c ', 11(a6, 1x))
     numdcd = numdcd  +  1
     call inlmfs
  end do
  go to 3988
2540 if (numrun .gt. 2) go to 2560
  if (nphlmt .gt. 3) go to 2545
  write (unit = buff77, fmt = 3639) nphlmt, (chrpad(m)(1 : 6), m = 1, 2 * nphlmt)
3639 format ('$include, [tsu]linescanp', i1, '.thl ', 6(a6, 1x))
  go to 2580
2545 do i = 1, 2
     if (i .eq. 1) write (unit = buff77, fmt = 2550) nphlmt, (chrpad(m)(1 : 6), m = 1, 7)
2550 format ('$include,[tsu]linescanp', i1, '.thl ', 7(a6, 1x), '$$')
     if (i .eq. 2) write (unit = buff77, fmt = 2516) (chrpad(m)(1 : 6), m = 8, 2 * nphlmt)
     numdcd = numdcd  +  1
     call inlmfs
  end do
  go to 3988
2560 do i = 1, 2
     if (i .eq. 1) write (unit = buff77, fmt = 3649) nphlmt, (chrpad(m)(1 : 6), m = 1, 7)
3649 format ('$include,[tsu]linescanm', i1, '.thl ', 7(a6,1x), '$$')
     if (i .eq. 2) write (unit = buff77, fmt = 2516) (chrpad(m)(1 : 6), m = 8, 2 * nphlmt)
     numdcd = numdcd  +  1
     call inlmfs
  end do
  go to 3988
  ! write (*,*) ' internally encoded card '
2580 numdcd = numdcd  +  1
  call inlmfs
  go to 3988
  ! =================  end line-model f-scan building of network
5823 if (noutpr .eq. 0) write (unit = kunit6, fmt = 4198)
4198 format ('+Blank card terminating branch cards.')
  nfscan = 0                                                ! reset nfscan for 2nd and 3rd lmfs cases
  if (ntot .le. lbus) go to 4192
  iprint = 1
  lstat(19) = 110
  go to 9000
4192 if (ksat .eq. 0) go to 200
110 n1 = 0
  n2 = 0
  n3 = 0
  n4 = 0
  ityold = itype
  do i = 1, ntot
     if (bus1 .eq. bus(i)) n1 = i
     if (bus2 .eq. bus(i)) n2 = i
     if (bus3 .eq. bus(i)) n3 = i
     if (bus4 .eq. bus(i)) n4 = i
  end do
  if (model .eq. 0) go to 4005
  ! itype gets negative sign internally for 2nd to mth phase
  ! of a untransposed line
  if (icheck .eq. 3) itype = -iabsz (itype)
4005 if (to_lower (bus1) .ne. text7) go to 4110
  if (to_lower (bus2) .ne. text8) go to 4110
  if (ksat .gt. 0) go to 4040
  if (to_lower (bus3) .ne. text9) go to 4003
  if (to_lower (bus4) .ne. text10) go to 4003
  read (unit = abuff, fmt = 44002) bus6, yzero
44002 format (26x, a6, e6.2)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 3996) bus6, yzero
3996 format ('+3-phase xformer request.  ', "'", a6, "'", '  ', e12.4)
  if (yzero .gt. 0.0d0) go to 3998
  kill = 52
  lstat(16) = 1
  lstat(19) = 3998
  go to 9200
3998 kph = 1
  go to 100
4003 continue
  read (unit = abuff, fmt = 4013) tr(it), tx(it), bus5, rmag, ioutmg
4013 format (26x, 2e6.2, a6, e6.2, 29x, i1)
  if (ioutmg .lt. 2) go to 3400
  nv = nv + 1
  if (nv .le. lsiz12) go to 3400
  iprint = 11
  lstat(19) = 3400
  go to 9000
3400 ntot = ntot + 1
  bus(ntot) = bus5
  if (bus5 .ne. blank) go to 3401
  kill = 161
  lstat(19) = 3401
  go to 9200
3401 nodtop = ntot
  kswtch = kswtch + 1
  if (kswtch .le. lswtch) go to 4004
  kill = 47
  lstat(19) = 4010
  lstat(16) = lswtch
  go to 9200
4004 nextsw(kswtch) = ntot
  if (kph .gt. 0) kph = kph + 1
  if (n3 .ne. 1) go to 4002
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 4010) tr(it), tx(it), rmag
4010 format ('+Sat. xformer.', 2x, 3e11.3)
  ksat = -1
  inonl = inonl + 1
  if (inonl .le. lnonl) go to 4011
4012 iprint = 9
  lstat(19) = 4012
  go to 9000
4011 vzero(inonl) = tr(it)
  anonl(inonl) = tx(it)
  ci1 = 0.0d0
  ck1 = 0.0d0
  nonlad(inonl) = i_char + 1
  itype = 98
  nltype(inonl) = -98
  curr(inonl) = 1.0d0
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 4014) ksat, it, i_char, ntot, it3, inonl
4014 format(/, ' At 4014', 7i10)
  lstat(18) = 187
  go to 186
4002 if (noutpr .eq. 0) write (unit = kunit6, fmt = 74003) bus3
74003 format ('+Transformer copy using reference name ', "'", a6, "'", '. ')
  ktref = 1
4006 if (ktref .le. kswtch) go to 4007
  kill = 48
  lstat(19) = 4007
  go to 9200
4007 if (n3 .eq. nextsw(ktref)) go to 4008
  ktref = ktref + 1
  go to 4006
4008 i = isourc(ktref)
  tclose(kswtch) = tclose(ktref)
  topen(kswtch) = topen(ktref)
  if (i .le. 0) go to 4027
  inonl = inonl + 1
  if (inonl .gt. lnonl) go to 4012
  nltype(inonl) = nltype(i)
  nonlad(inonl) = nonlad(i)
  nonle(inonl) = nonle(i)
  vzero(inonl) = vzero(i)
  anonl(inonl) = anonl(i)
  vnonl(inonl) = 0.0d0
  curr(inonl) = curr(i)
  isourc(kswtch) = inonl
4027 ksat = 1
  go to 100
4040 if (itype .eq. ksat) go to 4045
  kmswit(kswtch) = ibr
  icrit(kswtch) = ksat - 1
  if (ksat .ge. 3) go to 4093
  bus1 = trash
  lstat(19) = 4093
  go to 4067
4093 ksat = 0
  if (isourc(kswtch) .gt. 0) num99 = num99 + 1
  if (ktref .eq. 0) go to 4041
  lstat(19) = 4040
  go to 4067
4041 if (kph .eq. 4) kph = 0
  if (iprsup .lt. 3) go to 84041
  write (unit = lunit(6), fmt = 34041) kswtch, ibr, it, inonl, i_char
34041 format (/, ' Done with sat. xformer at 34041.  kswtch, ibr, it, inonl, ichar=  ', 5i10)
  write (unit = lunit(6), fmt = 44041) (i, kbus(i), mbus(i), nr(i), length(i), kodebr(i), bus(i), i = 1, ibr)
44041 format(/, ' Linear branch table.', /, 13x, 'row', 11x, 'kbus', 11x, 'mbus', 13x, 'nr', 9x, 'length', 9x, 'kodebr', 12x, 'bus', /, (1x, 6i15, a15))
  n = i_char
  if (inonl .gt. n) n = inonl
  write (unit = lunit(6), fmt = 54041) (i, nonlk(i), nonlm(i), nltype(i), nonlad(i), nonle(i), anonl(i), vzero(i), vchar(i), gslope(i), cchar(i), i = 1, n)
54041 format (/, ' n.l. elem arrays.', /, 6x, 'row', 3x, 'nonlk', 3x, 'nonlm', 2x, 'nltype', 2x, 'nonlad', 3x, 'nonle', 10x, 'anonl', 10x, 'vzero', 10x, 'vchar', 9x, 'gslope', 10x, 'cchar', /, (1x, 6i8, 5e15.5))
  write (unit = lunit(6), fmt = 64041) (modswt(i), kmswit(i), isourc(i), nextsw(i), icrit(i), kpos(i), tclose(i), topen(i), i = 1, kswtch)
64041 format (/, ' Switch arrays with xformer info.', /, 11x, 'modswt', 4x, 'kmswit', 4x, 'isourc', 4x, 'nextsw', 5x, 'icrit', 6x, 'kpos', 9x, 'tclose', 9x, ' topen', /, (7x, 6i10, 2e15.5))
  write (unit = lunit(6), fmt = 74041) (i, tr(i), tx(i), c(i), i = 1, it)
74041 format (/, ' Lumped param storage tr, tx, c', /, (1x, i10, 3e20.6))
84041 if (n1 .ne. 1) go to 4005
  if (n2 .ne. 1) go to 4005
  go to 200
4045 ksat = ksat + 1
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 74046) itype
74046 format ('+Winding', i2, '.')
  if (itype .gt. 1) go to 4034
  modswt(kswtch) = ibr + 1
  if (ioutmg .le. 1) go to 14045
  ibrnch(nv) = nodtop
  jbrnch(nv) = n2
  if (ioutmg .eq. 2) ioutmg = 0
14045 if (kph .le. 0) go to 4034
  if (kph .gt. 2) go to 4031
  keept = nodtop
  keepb = n2
  go to 4034
4031 call ibrinc
  kbus(ibr) = nodtop
  mbus(ibr) = n2
  if (kph .gt. 3) go to 4033
  nr(ibr) = it
  kodebr(ibr) = 1
  length(ibr) = 2
  tr(it) = yzero
  ideal = it
  call move0 (tx(it :), ll3)
  call move0 (c(it :), ll3)
  it = it + 1
  call ibrinc
  nr(ibr) = it
  tr(it) = -yzero
  it = it + 1
  tr(it) = yzero
  it = it + 1
  kbus(ibr) = keepb
  ntot = ntot + 1
  bus(ntot ) = bus6
  mbus(ibr) = ntot
  keepb = ntot
64033 modswt(kswtch) = ibr + 1
  kodebr(ibr) = 1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 54033) yzero, kswtch, ibr, it, inonl, i_char, kph, ksat, ideal, keepb, keept, ntot, nodtop, n1, n2
54033 format (/, ' Ideal xformer interconnect, at 54033.  yzero =', e15.6, 5i12, /, (1x, 10i12))
  go to 4034
4033 nr(ibr) = ideal
  length(ibr) = -2
  kodebr(ibr) = 1
  call ibrinc
  kbus(ibr) = keepb
  mbus(ibr) = keept
  nr(ibr) = ideal + 1
  go to 64033
4034 if (ktref .eq. 0) go to 4044
  if (ksat .gt. 2) go to 4065
  if (kph .eq. 2) yzero = 2.0d0 * yzero / tclose(ktref) ** 2
  if (isourc(ktref) .le. 0) go to 34035
  nonlk(inonl) = nodtop
  nonlm(inonl) = n2
  if (ioutmg .gt. 0) nonlm(inonl) = -nonlm(inonl)
34035 iref = modswt(ktref)
  call ibrinc
  nr(ibr) = nr(iref)
  length(ibr) = -1
  kbus(ibr) = n1
  mbus(ibr) = nodtop
  read (unit = abuff, fmt = 142) iout
  if (iout .gt. 0) mbus(ibr) = -mbus(ibr)
  kpos(kswtch) = n2
44035 iref = iref + 1
  n5 = iabs (length(iref))
  if (iprsup .gt. 3) write (unit = lunit(6), fmt = 74035) ibr, it, ktref, iref, n5, kbus(ibr), mbus(ibr)
74035 format (/, ' Ref. comp. copy of r1-l1 and mag. branch, at 74035', /, 1x, 8i15)
  if (n5 .eq. 2) go to 100
  call ibrinc
  nr(ibr) = nr(iref)
  length(ibr) = -1
  kbus(ibr) = nodtop
  mbus(ibr) = n2
  i = iabs (nr(ibr))
  if (tr(i) .ne. 0.0d0) go to 44035
  if (ioutmg .gt. 0) mbus(ibr) = -mbus(ibr)
  go to 44035
4044 continue
  read (unit = abuff, fmt = 4046) tr(it), tx(it), c(it), iout
4046 format (26x, 3e6.2, 35x, i1)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54047) itype, tr(it), tx(it), c(it)
54047 format ('+Winding', i2, '. ',  3e12.4)
  i = it + 1
  call move0 (tr(i :), ll2)
  call move0 (tx(i :), ll2)
  call move0 (c(i :), ll2)
  if (c(it) .gt. 0.0d0) go to 4047
  kill = 52
  lstat(19) = 4047
  lstat(16) = 3
  go to 9200
4047 if (ksat .gt. 2) go to 4070
  turn1 = c(it)
  if (kph .eq. 2) yzero = 2.0d0 * yzero / turn1 ** 2
  kpos(kswtch) = n2
  tclose(kswtch) = turn1
  c(it) = 0.0d0
  if (tr(it) .ne. 0.0d0) go to 4049
  if (tx(it) .ne. 0.0d0) go to 4049
  lstat(19) = 4049
  kill = 52
  lstat(16) = 2
  go to 9200
4049 call ibrinc
  kbus(ibr) = n1
  mbus(ibr) = nodtop
  if (iout .gt. 0) mbus(ibr) = -mbus(ibr)
  length(ibr) = 1
  nr(ibr) = -it
  it = it + 1
  if (rmag .eq. 0.0d0) go to 4052
  tr(it) = rmag
4048 call ibrinc
  kbus(ibr) = nodtop
  mbus(ibr) = n2
  length(ibr) = 1
  nr(ibr) = -it
  it = it + 1
  if (tx(it - 1) .ne. 0.0d0) go to 100
4052 it3 = nonlad(inonl)
  if (kph .le. 2) go to 4054
  if (tclose(kswtch) .eq. tclose(kswtch - 1)) go to 4054
24054 kill = 52
  lstat(19) = 4054
  lstat(16) = 4
  go to 9200
4054 ncoil = -nonle(inonl) - it3
  if (ncoil .gt. 0) go to 4056
  i_char = it3 - 1
  inonl = inonl - 1
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 4053) it, ibr, i_char, inonl, kswtch, ncoil, ksat, kph
4053 format (/, ' mag. br. not actually n.l., at 4053.', /, 1x, 8i15)
  if (ncoil .lt. 0) go to 100
  tx(it) = delta2 / gslope(it3) * xunits
  if (ioutmg .gt. 0) n2 = -n2
  go to 4048
4056 nonlk(inonl) = nodtop
  nonlm(inonl) = n2
  if (ioutmg .gt. 0) nonlm(inonl) = -nonlm(inonl)
  isourc(kswtch) = inonl
  go to 100
4065 call ibrinc
  kbus(ibr) = n1
  mbus(ibr) = n2
  nr(ibr) = nr(iref)
  kodebr(ibr) = 1
  length(ibr) = -iabs (length(iref))
  call ibrinc
  kbus(ibr) = nextsw(kswtch)
  mbus(ibr) = kpos(kswtch)
  kodebr(ibr) = 1
  iref = iref + 1
  nr(ibr) = nr(iref)
  iref = iref + 1
  if (itype .le. icrit(ktref)) go to 4068
  lstat(19) = 4067
4067 kill = 49
  j = nextsw(ktref)
  bus1 = bus(j)
  lstat(13) = ksat
  lstat(16) = icrit(ktref)
  go to 9200
4068 if (itype .lt. icrit(ktref)) go to 100
  ktref = 0
  go to 100
4070 d1 = c(it) / turn1
  if (ksat .gt. 3) go to 4073
  topen(kswtch) = c(it)
  if (kph .le. 2) go to 4073
  if (topen(kswtch) .ne. topen(kswtch - 1)) go to 24054
4073 if (tx(it) .ne. 0.0d0) go to 4071
  kill = 52
  lstat(19) = 4071
  lstat(16) = 2
  go to 9200
4071 c(it) = 0.0d0
  tx(it) = tx(it) / xunits
  cut1 = 1.0d0 / tx(it)
  cut2 = cut1 * d1
  cut3 = -tr(it) * cut1
  tr(it) = cut1
  tx(it) = cut3
  call ibrinc
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 4076) it, d1, cut1, cut2, tr(it), tx(it)
4076 format (/, ' Row 1 of 2x2 at 4076', i10, 5e15.4, /, 1x)
  kbus(ibr) = n1
  mbus(ibr) = n2
  length(ibr) = 2
  nr(ibr) = it
  kodebr(ibr) = 1
  call ibrinc
  it = it + 1
  kbus(ibr) = nextsw(kswtch)
  mbus(ibr) = kpos(kswtch)
  kodebr(ibr) = 1
  nr(ibr) = it
  tr(it) = -cut2
  it = it + 1
  tr(it) = d1 * cut2
  tx(it) = cut3
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 4084) it
4084 format (/, ' Row 2 of 2x2 at 4084', i10, /, 1x)
  it = it + 1
  go to 100
4110 if (to_lower (bus1) .ne. text3) go to 4112
  kreqab = 1
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 4111)
4111 format ('+Begin coupled, lumped elements using (A), (B).')
  go to 100
4112 if (to_lower (bus1) .ne. text4) go to 4114
  kreqab = 0
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 4113)
4113 format ('+Begin coupled, lumped elements using (R), (L).')
  go to 100
4114 if (to_lower (bus1) .ne. text5) go to 7642
  if (to_lower (bus2) .ne. text6) go to 7642
  icas = 1
  iprsov(36) = it
  iprsov(35) = ibr + 1
  read (unit = abuff, fmt = 764) nphcas, freqcs
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 76701) nphcas, freqcs
76701 format ('+Cascaded-pi header card.', i10, 2x, e12.4)
764 format (26x, i6, e6.2)
  idumy = 3 * nphcas
  idumy = idumy * (idumy + 1) / 2
  if (idumy .lt. lpast) go to 7671
  lstat(19) = 764
  lstat(15) = 1
  go to 76724
7671 idumy = nphcas * (nphcas + 1) / 2
  if (idumy .lt. lbus) go to 100
  lstat(19) = 7671
  lstat(15) = 2
76724 kill = 53
  lstat(16) = idumy
  lstat(12) = nphcas
  go to 9200
7642 if (isgfd .lt. 0) go to 15
  if ((to_lower (bus1) .ne. text11) .or. (to_lower (bus2) .ne. text14)) go to 15
  length(ibr1 + 1) = -666
  call fddata (ikf, isfd, ibf)
  if (kill .gt. 0) go to 9200
  isgfd = -100
  go to 100
15 iprint = 1
  if (n1 .ne. 0) go to 130
  ntot = ntot + 1
  lstat(19) = 7642
  if (ntot .gt. lbus) go to 9000
  bus(ntot) = bus1
  n1 = ntot
130 if (n2 .ne. 0) go to 140
  ntot = ntot + 1
  lstat(19) = 130
  if (ntot .gt. lbus) go to 9000
  bus(ntot) = bus2
  n2 = ntot
140 if (ksat .gt. 0) go to 4040
  if (itype .lt. 0) go to 64117
  if ((itype .gt. 50) .and. (itype .le. 90)) go to 64117
  if (kolbeg .gt. 0) go to 132
  if (moldat .gt. 0) go to 64117
  read (unit = abuff, fmt = 64114, iostat = ios) (tr(i), tx(i), c(i), i = it, it2)
64114 format (26x, 9e6.2)
  go to 64117
132 kolbeg = 27
  nfrfld = 3 * (it2 - it + 1)
  call free (voltbc)
  n9 = 1
  do i = it, it2
     tr(i) = voltbc(n9)
     tx(i) = voltbc(n9 + 1)
     c(i) = voltbc(n9 + 2)
     n9 = n9 + 3
  end do
64117 if (kph .eq. 0) go to 4115
  kill = 50
  lstat(15) = kph - 1
  lstat(19) = 4115
  go to 9200
4115 iprint = 2
  itrans = 0
  if (itype .lt. 51) go to 144
  if (itype .gt. 90) go to 144
147 itrans = itype
  itype = itype - 50
  n8 = it2 - it + 1
  if (kolbeg .gt. 0) go to 4243
  read (unit = abuff, fmt = 145) (tr(i), tx(i), i = it, it2)
145 format (26x, 3(e6.2, e12.2))
  go to 4246
4243 kolbeg = 27
  nfrfld = 2 * n8
  n9 = 1
  call free (voltbc)
  do i = it, it2
     tr(i) = voltbc(n9)
     tx(i) = voltbc(n9 + 1)
     n9 = n9 + 2
  end do
4246 call move0 (c(it :), n8)
144 if ((itype .gt. 2) .and. (itype .le. 50)) go to 143
  read (unit = abuff, fmt = 142) iout
142 format (79x, i1)
  if (iout .le. 3) go to 54208
  iout = 3
  if (npower .lt. maxpe) go to 54201
  iprint = 18
  lstat(19) = 142
  go to 9000
54201 npower = npower + 1
  mpower = npower + maxpe
  koutvp(npower) = nv + 1
  if (itype .lt. 90) go to 54203
  koutvp(mpower) = -(inonl + 1)
  go to 54205
54203 koutvp(mpower) = ibr + 1
54205 if (iprsup .ge. 2) write (unit = lunit(6), fmt = 54207) npower, maxpe, koutvp(npower), koutvp(mpower)
54207 format (/, ' Power output request', 4i10)
54208 continue
  if (iout .lt. 2) go to 143
  nv = nv + 1
  ibrnch(nv) = n1
  jbrnch(nv) = n2
  if (itype .ne. 99) go to 138
  if (n1 .le. n2) go to 138
  ibrnch(nv) = n2
  jbrnch(nv) = n1
138 if (nv .le. lsiz12) go to 143
  iprint = 11
  lstat(19) = 143
  go to 9000
143 if (iout .eq. 2) iout = 0
  if (itype .lt. 0) n1 = -n1
  call ibrinc
  lstat(19) = 141
  if (ibr .gt. lbrnch) go to 9000
  if (itype .eq. 92) icheck = 1
  !  go to (141, 162, 190, 177, 190, 8520, 8130, 8178), icheck
  select case (icheck)
  case (1)
     go to 141

  case (2)
     go to 162

  case (3, 5)
     go to 190

  case (4)
     go to 177

  case (6)
     go to 8520

  case (7)
     go to 8130

  case (8)
     go to 8178
  end select
141 icheck = 1
  if (itype .gt. 50) go to 186
  if (iout .gt. 0 .and. itype .le. 0) n2 = -n2
  if (bus3 .ne. branch) go to 6835
  n16 = 1
  text15 = bus4
  call namea6 (text15, n16)
  if (n16 .eq. -intinf) go to 6824
  lstat(19) = 6829
  kill = 167
  lstat(14) = n16
  go to 9200
6824 n16 = 0
  call namea6 (text5, n16)
  namebr(ibr) = n16
  bus3 = blank
  bus4 = blank
  n3 = 1
  n4 = 1
6835 iold = 0
  if (n3 .eq. 1 .and. n4 .eq. 1) go to 156
  iold = 1
  go to 155
150 ll = iabs (length(iold))
  if (bus3 .eq. chcopy) go to 6841
  if (iabs (kbus(iold)) .ne. n3) go to 28150
  if (iabs (mbus(iold)) .eq. n4) go to 156
  go to 28150
6841 n14 = namebr(iold)
  if (bus4 .eq. texvec(n14)) go to 156
28150 iold = iold + ll
155 if (iold .lt. ibr) go to 150
  lstat(19) = 150
  kill = 6
  go to 9200
156 kbus(ibr) = n1
  mbus(ibr) = n2
  if (iold .ne. 0) go to 175
  length(ibr) = 1
  if (itype .eq. 0) go to 170
  ibr1 = ibr
  kcount = 1
  if (itype .eq. 1) go to 160
  if (itype .eq. (-1)) go to 190
  lstat(19) = 160
4218 kill = 3
  lstat(16) = itype
  go to 9200
  !     pi-equivalent ****************************************************
160 nr(ibr) = it
  if (kreqab .gt. 0) kodebr(ibr) = 1
  if (moldat .eq. 0) go to 171
  if (kolbeg .gt. 0) go to 171
  read (unit = abuff, fmt = 8331) tr(it), tx(it), c(it)
171 if (noutpr .ne. 0) go to 5411
  if (itrans .gt. 0) go to 54108
  write (unit = kunit6, fmt = 54777) tr(it), tx(it), c(it)
54777 format ('+1st of pi-ckt.', 1x, 3e11.3)
  go to 5411
54108 write (unit = kunit6, fmt = 54109) tr(it), tx(it)
54109 format ('+1st of coupled R-L.', 5x, 2e11.3)
5411 icheck = 2
  if (tr(it) .eq. -6666. .and. tx(it) .eq. -6666.) isgfd = 100
  go to 8383
162 kcount = kcount + 1
  if (itype .ne. kcount) go to 141
  length(ibr1) = kcount
  if (kcount .gt. mxphas) mxphas = kcount
  it2 = it + itype - 1
  if (moldat .eq. 0) go to 8352
  if (kolbeg .gt. 0) go to 8352
  do it32 = it, it2
     !     read input card using cimage.
     if (it32 .gt. it) call cimage
     read (unit = abuff, fmt = 8331) tr(it32), tx(it32), c(it32)
8331 format (26x, 3e16.0)
     if (noutpr .ne. 0) cycle
     if (it32 .gt. it) go to 8338
     write (unit = kunit6, fmt = 8335) itype, tr(it32), tx(it32), c(it32)
8335 format ('+phase', i3, 3e13.4)
     cycle
8338 write (unit = kunit6, fmt = 8341) tr(it32), tx(it32), c(it32)
8341 format ('+', 3x, 'cont.', 3e13.4)
  end do
  go to 163
8352 if (itype .ne. 3) go to 47634
  d3 = absz (tr(it)) + absz (tx(it)) + absz (tr(it + 1)) + absz (tx(it + 1)) + absz(tr(it + 2)) + absz(tx(it + 2))
  if (d3 .gt. 0.0d0) go to 47634
  d3 = tr(it - 3)
  d4 = tr(it - 2)
  d5 = (d3 + 2.0d0 * d4) / 3.
  d6 = (d3 - d4) / 3.
  tr(it - 3) = d5
  tr(it - 1) = d5
  tr(it + 2) = d5
  tr(it - 2) = d6
  tr(it) = d6
  tr(it + 1) = d6
  d3 = tx(it - 3)
  d4 = tx(it - 2)
  d5 = (d3 + 2.0d0 * d4) / 3.
  d6 = (d3 - d4) / 3.
  tx(it - 3) = d5
  tx(it - 1) = d5
  tx(it + 2) = d5
  tx(it - 2) = d6
  tx(it) = d6
  tx(it + 1) = d6
47634 if (itrans .gt. 0) go to 21693
  it3 = it + 1
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54104) tr(it), tx(it), c(it), tr(it3), tx(it3)
54104 format ('+', 5e10.3)
  go to 21694
21693 it3 = itype + (it - 1)
  n3 = it + 2
  if (it3 .lt. n3) n3 = it3
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54104) tr(it), tx(it), tr(it + 1), tx(it + 1), tr(n3)
21694 continue
  if (itype .le. 3) go to 163
  it3 = it + 3
165 it32 = it3 + 2
  if (it2 .lt. it32) it32 = it2
  ! read input card using cimage.
  call cimage
  if (itrans .eq. 0) go to 168
  n8 = it32 - it3 + 1
  if (kolbeg .gt. 0) go to 4254
  read (unit = abuff, fmt = 166) (tr(i), tx(i), i = it3, it32)
166 format (26x, 3(e6.2, e12.2))
  go to 4256
4254 kolbeg = 27
  nfrfld = 2 * n8
  n9 = 1
  call free (voltbc)
  do i = it3, it32
     tr(i) = voltbc(n9)
     tx(i) = voltbc(n9 + 1)
     n9 = n9 + 2
  end do
4256 call move0 (c(it3 :), n8)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54104) tr(it3), tx(it3), tr(it3 + 1), tx(it3 + 1), tr(it32)
  go to 169
168 if (kolbeg .gt. 0) go to 4262
  read (unit = abuff, fmt = 2) (tr(i), tx(i), c(i), i = it3, it32)
  go to 4264
4262 nfrfld = 3 * (it32 - it3 + 1)
  kolbeg = 27
  n9 = 1
  call free (voltbc)
  do i = it3, it32
     tr(i) = voltbc(n9)
     tx(i) = voltbc(n9 + 1)
     c(i) = voltbc(n9 + 2)
     n9 = n9 + 3
  end do
4264 i = it3
  if (it32 .gt. i) i = i + 1
  n3 = i
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54104) tr(it3), tx(it3), c(it3), tr(n3), tx(n3)
169 if (it32 .eq. it2) go to 163
  it3 = it3 + 3
  go to 165
2 format (26x, 9e6.2)
163 kbus(ibr) = n1
  mbus(ibr) = n2
  nr(ibr) = it
  if (kreqab .gt. 0) kodebr(ibr) = 1
  it = it + itype
  if (icas .eq. -1 .and. itype .eq. 2 * nphcas) go to 148
  if (icas .le. 0) go to 100
  if (itype .lt. nphcas) go to 100
  call over3
  go to 1000
148 iend = ibr1 + nphcas - 1
  do i = ibr1, iend
     kodebr(i) = -2
  end do
  n3 = ibr1 + nphcas
  n4 = ibr1 + ibr
  do i = n3,n4
     kbus(i) = 0
     mbus(i) = 0
  end do
  icas = 0
  ibr = ibr - nphcas
  length(ibr1) = nphcas
  if (nphcas .gt. mxphas) mxphas = nphcas
  go to 100
  ! series R-L-C  ****************************************************
170 nr(ibr) = -it
  icheck = 1
  if (moldat .eq. 0) go to 8371
  if (kolbeg .gt. 0) go to 8371
  read (unit = abuff, fmt = 8331) tr(it), tx(it), c(it)
8371 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54107) tr(it), tx(it), c(it)
54107 format ('+Series R-L-C.', 2x, 3e11.3)
8383 d1 = absz (tr(it)) + absz (tx(it)) + absz (c(it))
  if (d1 .ne. 0.0d0) go to 4220
  kill = 4
  lstat(19) = 5411
  go to 9200
4220 it = it + 1
  go to 100
  ! data already available in reference branch ***********************
175 if (imodel(iold) .eq. -3 .or. imodel(iold) .eq. -4) go to 8100
  if (imodel(iold) .eq. -2) go to 8100
  if (kodsem(iold) .eq. 0) go to 8600
  ipunch = iabs (kodebr(iold))
  ips1 = 1
  icheck = 6
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 21696) bus3, bus4
  go to 8560
8520 n6 = 5
  kbus(ibr) = n1
  mbus(ibr) = n2
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54111)
  iold = iold + 1
  ips1 = ips1 + 1
8540 if (itype .eq. -1) go to 8560
  kill = 169
  lstat(19) = 8550
  go to 9200
8560 kodebr(ibr) = -ipunch
  kodsem(ibr) = kodsem(iold)
  length(ibr) = length(iold)
  ci(ibr) = ci(iold)
  ck(ibr) = ck(iold)
  cik(ibr) = cik(iold)
  cki(ibr) = cki(iold)
  n3 = nr(iold)
  nr(ibr) = n3
  n5 = ifx + 1
  ifx = ifx + n3
  if (ifx .le. lhist) go to 8570
  iprint = 22
  lstat(19) = 8570
  go to 9000
8570 indhst(ibr) = n5
  n3 = indhst(iold)
  if (ci(iold) .gt. 0.0d0) n6 = n6 + 1
  if (kodsem(iold) .gt. 0) n6 = 6
  if (n6 .gt. 0) call mover (cnvhst(n3 :), cnvhst(n5 :), n6)
  if (ips1 .lt. ipunch) go to 100
  if (cki(iold) .gt. 0.0) go to 8580
  icheck = 1
  go to 100
8580 call ibrinc
  if (ibr .le. lbrnch) go to 8590
  iprint = 2
  lstat(19) = 8590
  go to 9000
8590 n6 = 0
  kbus(ibr) = -1
  mbus(ibr) =  1
  go to 8540
8100 if (imodel(iold) .eq. -2) go to 8987
  if (itype .lt. 0 ) write (unit = kunit6, fmt = 8710) itype, bus1, bus2, bus3, bus4
8710 format ('+Reference cable branch:', i2, 4a6)
  call interp
  icheck = 8
  model = length(iold)
  ibr1 = ibr
  length(ibr1) = length(iold)
8187 litype(ibr) = litype(iold)
  ! imodl(ibr) = imodl(iold)
  imodel(ibr) = imodel(iold)
  ! matcon(ibr) = matcon(iold)
  ! ifbrc(ibr) = ibr
  ! icrbf(ibr) = ibr
  ifx = ifx + 1
  if (ifx .gt. lhist) stop
  ifsem = ifsem + 1
  if (ifsem .gt. lfsem) stop
  cki(ibr) = cki(iold)
  itemq = kodsem(iold)
  sconst(ifsem) = sconst(itemq)
  kodsem(ibr) = ifsem
  indhst(ibr) = ifx
  ckkjm(ibr) = ckkjm(iold)
  iihst = indhst(iold)
  cnvhst(ifx) = cnvhst(iihst)
  inoff5 = 5 * lbrnch
  infdli(inoff5 + ibr) = int (cnvhst(ifx) / deltat) + 1
  itemq = itemq + int (7 * cki(iold)) + int (11 * ckkjm(iold)) + 9 + nn17
  ifsem = ifsem + int (7 * cki(ibr)) + int (11 * ckkjm(ibr)) + 9 +nn17
  if (ifsem .gt. lfsem) stop
  nfir = kodsem(iold) + 1
  nsec = kodsem(ibr) + 1
  call mover (sconst(nfir :), sconst(nsec :), itemq - 1)
  !      ifx = ifx + 14
  ifx = ifx + 19 + 1 + nycmp
  if (model .ne. -itype) go to 100
  if (-itype .eq. model) go to 50087
50087 ifq = ifsem
  !                                                   !the refernce branch
  inoff1 = lbrnch
  inoff2 = 2 * lbrnch
  inoff3 = 3 * lbrnch
  inoff4 = 4 * lbrnch
  icheck = 1
  nphs = model
  nphs2 = model ** 2
  nphsu = (model + 1) * model / 2
  ibr2 = ibr - nphs + 1
  ibr3 = ibr - nphs + 3
  ifq = ifq + 1
  infdli(inoff2 + ibr2) = ifq
  infdli(inoff1 + ibr2) = idm
  infdli(inoff4 + ibr2) = idu
  ! iadrsm(iold) contains the index for first cell of qii(1,1), for
  ! double six phases circuits it should be 1*6+7(16+9+10+11+12+12)
  ! +7(21+16+17+10+10+17)+6*8=1181 which been added 1 gives 1st cell of qii
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
  if (kgroup .ne. 1) wk1(koff19) = 1
  wk1(koff19) = wk1(koff19) + kgroup
  nq = 0
  interm = iold - model + 1
  ibrter = ibr - model + 1
  nphs2 = length( interm ) ** 2
  ! kq = iadrsu( interm ) !index of q - 'm'atrix for referenced branch
  kq = infdli(inoff1 + interm)
  ! nkq = iadrsu( ibrter ) !index of q - 'm'atrix for reference branch
  nkq = infdli(inoff1 + ibrter)
  do i = 1, nphs2
     wk1(koff20 + nkq) = wk1(koff20 + kq)
     ! ntermq(nkq) = ntermq(kq)
     ! load # terms form refd brch to ref brch
     ! qk0(nkq)    = qk0(kq)
     !load feedforward coefficient qk0 of qii
     nkq = nkq + 1
     kq = kq + 1
  end do
  iold = iold - model + 1
  isec = iold + 1
  kq =  infdli(inoff2 + iold)
  kq1 = infdli(inoff3 + iold)
  inew = ibr - model + 1
  nkq  = infdli(inoff2 + inew)
  kq1  = kq1 - kq + 1
  call mover (sconst(kq :), sconst(nkq :), kq1)
  nkq = nkq + kq1 - 1
  ifq = nkq + (nkq - infdli(inoff2 + inew) + 1 - nphs2) * 3 / 2 + 2 * nphs2
  ifsem = ifq
  infdli(inoff3 + ibr2) = nkq
  idm = idm + nphs2
  idu = idu + nphsu
  if (model .ne. -itype) go to 100
  icheck = 1
  kgroup = 1
  go to 100
8178 iold = iold + 1
  kbus(ibr) = n1
  mbus(ibr) = n2
  go to 8187
8987 icheck = 7
  model = length(iold)
  ibr1 = ibr
  length(ibr1) = 0
  litype(ibr) = litype(iold)
8110 if (itype .lt. 0 .and. noutpr .eq. 0) write (unit = kunit6, fmt = 8810) itype, bus1, bus2, bus3, bus4
8810 format ("+Reference Jose's branch: ", i2, 4a6)
  call interp
  cki(ibr) = cki(iold)
  ckkjm(ibr) = ckkjm(iold)
  imodel(ibr) = imodel(iold)
  n5 = ifsem + 1
  n6 = int (7 * cki(ibr)) + int (7 * ckkjm(ibr)) + 5
  ifsem = ifsem + n6
  if (ifsem .gt. lfsem) go to 8170
  kodsem(ibr) = n5
  n3 = kodsem(iold)
  call mover (sconst(n3 :), sconst(n5 :), n6)
  n5 = ifx + 1
  ifx = ifx + 15
  if (ifx .gt. lhist) go to 8150
  indhst(ibr) = n5
  n3 = indhst(iold)
  cnvhst(n5) = cnvhst(n3)
  length(ibr1) = length(ibr1) + 1
  if (model .gt. length(ibr1)) go to 100
  icheck = 1
  itranm = model
  model = 0
  go to 100
8130 iold = iold + 1
  kbus(ibr) = n1
  mbus(ibr) = n2
  go to 8110
8150 iprint = 22
  lstat(19) = 8150
  go to 9000
8170 iprint = 20
  lstat(19) = 8170
  go to 9000
8600 nr(ibr) = nr(iold)
  namebr(ibr) = namebr(iold)
  length(ibr) = -iabs (length(iold))
  litype(ibr) = litype(iold)
  kodebr(ibr) = kodebr(iold)
  cik(ibr) = cik(iold)
  ci(ibr) = ci(iold)
  ck(ibr) = ck(iold)
  cki(ibr) = cki(iold)
  ibr1 = ibr
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 21696) bus3, bus4
21696 format ('+Reference branch.   Copy  ', "'", a6, "'", ' to ', a6, "'")
  if (ll.eq.1) go to 178
  mbus(ibr) = iabs (n2)
  icheck = 4
  ii = 2
  !     read input card using cimage.
176 call cimage
  if (kolbeg .gt. 0) go to 6634
  read (unit = abuff, fmt = 1) itype, bus1, bus2
  go to 6637
6634 nfrfld = 1
  call free (voltbc)
  itype = int (voltbc(1))
  nfrfld = 2
  nright = -1
  call free (d1)
  bus1 = texta6(1)
  bus2 = texta6(2)
  nright = 0
6637 if (itype .gt. 50 .and. itype .lt. 91) itype = itype - 50
  if (iabs (itype) .eq. ii) go to 110
  lstat(19) = 176
  go to 4218
177 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54111)
54111 format ('+Reference branch.   Copy cont.')
  iold = iold + 1
  kbus(ibr) = n1
  mbus(ibr) = n2
  nr(ibr) = nr(iold)
  kodebr(ibr) = kodebr(iold)
  cki(ibr) = cki(iold)
  cik(ibr) = cik(iold)
  ci(ibr) = ci(iold)
  ck(ibr) = ck(iold)
  ii = ii + 1
  if (ii .le. ll) go to 176
178 icheck = 1
  itranm = 1
  if (icas .ne. 1) go to 100
  icas = 0
  call cimage
  read (unit = abuff, fmt = 76621) bus1, bus2
76621 format (2x, 2a6)
  if (to_lower (bus1) .ne. text1) go to 7662
  if (to_lower (bus2) .eq. text2) go to 76617
7662 lstat(19) = 7662
  kill = 58
  goto 9200
76617 if (noutpr .eq. 0) write (unit = kunit6, fmt = 76620)
76620 format ('+Termination of cascaded pi.')
  go to 100
  !     process nonlinear element inside subroutine  "nonln2" .
186 call nonln2
  if (kill .gt. 0) go to 9200
  go to 100
  !     process distributed line inside subroutine  "distr2" .
190 call distr2
  if (kill .gt. 0) go to 9200
  if (nchain .ne. 4) go to 197
  call over4
  go to 1000
197 i = lstat(18)
  if (i .eq. 100) go to 100
  if (i .eq. 141) go to 141
  call stoptp
200 it = it - 1
  if (mxphas*mxphas .lt. lsiz26) go to 1641
  kill = 1
  iprint = 26
  lstat(19) = 200
  lstat(13) = mxphas
  go to 9000
1641 lastov = nchain
  nchain = 5
  ktrlsw(3) = 0
  do i = 1, ibr
     if (litype(i) .ge. 0) cycle
     litype(i) = -litype(i)
  end do
8675 ibrnam = ibrnam + 1
  if (ibrnam .gt. ibr) go to 8680
  namebr(ibrnam) = 1
  if (nmauto .eq. 0) go to 8675
  call nmincr (text16, ibrnam)
  n24 = 0
  call namea6 (text16, n24)
  namebr(ibrnam) = n24
  go to 8675
8680 inonam = inonam + 1
  if (inonam .gt. inonl) go to 8681
  namebr(inonam) = 1
  if (nmauto .eq. 0) go to 8680
  call nmincr (text17, inonam)
  n24 = 0
  call namea6 (text17, n24)
  namenl(inonam) = n24
  go to 8680
8681 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1644) ntot, ibr, it, inonl, mxphas
1644 format (' Bottom of "over2".    ntot     ibr      it   inonl  mxphas ', /, 19x, 10i8)
  if (iprsup .le. 2) go to 9900
  write (unit = lunit(6), fmt = 208) (k, kbus(k), mbus(k), nr(k), kodebr(k), length(k), kodsem(k), indhst(k), litype(k), imodel(k), namebr(k), k = 1, ibr)
208 format (/, " Branch-table integer vectors at end  'over2' .", /, '     row    kbus    mbus      nr  kodebr  length  kodsem indhst  litype  imodel  namebr', /, (11i8))
  write (unit = lunit(6), fmt = 209) (k, bus(k), ci(k), ck(k), cik(k), cki(k), ckkjm(k), k = 1, ibr)
209 format (/, 5x, 'row', 7x, 'bus', 13x, 'ci', 13x, 'ck', 12x, 'cik', 12x, 'cki', 10x, 'ckkjm', /, (i8, 4x, a6, 5e15.6))
  write (unit = lunit(6), fmt = 2584) (i, tr(i), tx(i), r(i), c(i), i = 1, it)
2584 format (/, ' Rows 1 through it of parameters follow:', /, 7x, 'row',  13x, 'tr',  13x, 'tx',  14x, 'r',  14x, 'c', /, (i10, 4e15.5))
  go to 9900
9000 lstat(16) = iprint
  kill = 1
9200 nchain = 51
  lstat(18) = 2
9900 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 9906) ibr, inonl, ntot, kill
9906 format (' Exit module "over2".     ibr   inonl    ntot    kill', /, 21x, 10i8)
  if (allocated (icrit)) then
     crit = transfer (icrit, crit)
     deallocate (icrit)
  end if
  return
end subroutine over2

!
! subroutine fddata.
!

subroutine fddata (ikf, isfd, ibf)
  use com2
  use blkcom
  use labcom
  implicit none
  ! overlay 2 module used only for frequency-dependent
  ! representation of generator equivalents.
  !
  ! This routine reads-in the branch data for the modes  * * * * * * *
  ! i n i t i a l i z e   c o u n t e r s    *   *   *   *   *   *   *
  integer(4), intent(out) :: ibf, ikf, isfd
  integer(4) :: ibk, idk, ifk, igk, ikk, isk
  integer(4) :: ka, kb
  real(8) :: ac1, al1, ar1, arl
  real(8) :: d3
  !
  idk = 2 * ikf
  ikk = isfd
  ikf = ikf + 1
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 1) ikf, idk, ikk
1 format (' At start of fddata ikf, idk, ikk =', 3i10)
  ! proces modal branch data         *   *   *   *   *   *   *   *   *
  do ka = 1, 2
     ibk = 0
     ! read input card using cimage
3    call cimage
     read (unit = abuff, fmt = 4) ar1, al1, ac1, arl
4    format (4e16.0)
     ifk = 3
     if (ar1 .eq. 9999.0d0) go to 10
     d3 = ar1 + al1 + ac1 + arl
     if (d3 .gt. 0.) go to 6
     lstat(19) = 6
     go to 3719
6    if (al1 .gt. 0.0d0) go to 7
     ar1 = ar1 + arl
     arl = 0.
7    ibk = ibk + 1
     ikk = ikk + 1
     igk = 7
     if (ikk+4 .le. l27dep) go to 8
     lstat(19) = 8
     go to 3742
8    rmfd(ikk) = ar1
     rmfd(ikk+1) = al1
     rmfd(ikk+2) = ac1
     rmfd(ikk+3) = arl
     rmfd(ikk+4) = 0.
     ikk = ikk + 4
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 9) ar1, al1, ac1
9    format ('+Next branch', 8x, 3e10.3)
     go to 3
10   if (noutpr .eq. 0) write (unit = kunit6, fmt = 11)
11   format ('+Special termination of branches')
     ifk = 10
     if (ibk .gt. 0) go to 15
     lstat(19) = 15
     go to 3719
15   kb = idk + ka
     igk = 12
     if (kb .le. lsiz27) go to 16
     lstat(19) = 16
     go to 3742
16   imfd(kb) = ibk
  end do
  if (iprsup .lt. 1) go to 14
  isk = isfd + 1
  write (unit = lunit(6), fmt = 13) isk, ikk, (rmfd(ka), ka = isk, ikk)
13 format (' At end of fddata  rmfd from', i6, '  to', i6, /, (2x,6e21.12))
14 isfd = ikk
  igk = 14
  ibf = ibf + (imfd(idk + 1) + imfd(idk + 2) * (itype - 1)) * 3
  if (ibf .le. l27dep) go to 9999
  lstat(19) = 14
3742 kill = 1
  go to 9999
3719 kill = 37
9999 return
end subroutine fddata

!
! subroutine nonln2.
!

subroutine nonln2
  use blkcom
  use labcom
  use com2
  use tracom
  use strcom
  use movcop
  use freedom
  implicit none
  character(8) :: text1, text2, text3
  integer(4) :: i, iaw, ibk, icard, ichr, ii, iprint, iseg
  integer(4) :: j, jb, jk
  integer(4) :: ll9
  integer(4) :: n6, n7, n10, n11, n12, n14, n15, n16, n17
  real(8) :: a2
  real(8) :: c1
  real(8) :: d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12
  real(8) :: xlong
  !
  data text1 / 'single' /
  data text2 / ' flash' /
  !
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module nonln2."')
  ll9 = 9
  ! following check is for saturable transformer char.
  if (lstat(18) .eq. 187) go to 187
  if (itype .ge. 91) go to 4221
  kill = 3
  lstat(19) = 4221
  lstat(16) = itype
  go to 9999
4221 iprint = 9
  inonl = inonl + 1
  lstat(19) = 6862
  if (inonl .gt. lnonl) go to 9000
  if (moldat .eq. 0) go to 4236
  read (unit = abuff, fmt = 5432) tr(it), tx(it), c(it), tr(it + 1)
5432 format (26x, 4e12.0)
4236 if (bus3 .ne. branch) go to 6874
  n16 = 1
  text3 = bus4
  call namea6 (text3, n16)
  if (n16 .eq. -intinf) go to 6824
  kill = 167
  lstat(19) = 4236
  lstat(14) = n16
  go to 9999
6824 n16 = 0
  call namea6 (text3, n16)
  namenl(inonl) = n16
  bus3 = blank
  bus4 = blank
  n4 = 1
  n3 = 1
6874 ibr = ibr - 1
  nltype(inonl) = itype
  nonlk(inonl) = n1
  if (iout .gt. 0) n2 = -n2
  nonlm(inonl) = n2
  vnonl(inonl) = tr(it)
  if (itype .ne. 93 .and. itype .ne. 96 .and. itype .ne. 98) go to 185
  vzero(inonl) = tr(it)
  anonl(inonl) = tx(it)
  vnonl(inonl) = 0.0d0
185 curr(inonl) = 0.0d0
  if (itype .eq. 94) go to 270
  if (itype .ne. 99) go to 73412
  d3 = c(it)
  if (moldat .gt. 0) go to 4257
  read (unit = abuff, fmt = 73400) d3
73400 format (38x, e6.0)
4257 if (d3 .eq. 0.0) d3 = 1.0
  anonl(inonl) = d3
  n10 = int (d3)
  vecnl1(inonl) = tx(it)
  vecnl2(inonl) = tx(it)
  nltype(inonl) = -99
  num99 = num99 + 1
  curr(inonl) = 0.0d0
  vzero(inonl) = tr(it + 1)
  if (vzero(inonl) .gt. vnonl(inonl)) vzero(inonl) = 0.0
  if (n3 * n4 .ne. 1) go to 182
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 73406) tr(it), tx(it), n10
73406 format ('+Type-99 nonlinear R.', 2x, 2e11.3, i5)
73408 ci1 = 0.0d0
  go to 73420
73412 if (itype .ne. 98) go to 73416
  nltype(inonl) = -98
  num99 = num99 + 1
  curr(inonl) = 1.0
  if (n3 * n4 .ne. 1) go to 182
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 73413) tr(it), tx(it)
73413 format ('+Type-98 pseudo-nonlinear L.', 2e11.3)
  go to 73408
73416 if (itype .ne. 97) go to 73418
  nltype(inonl) = -97
  num99 = num99 + 1
  anonl(inonl) = tx(it)
  if (n3 * n4 .ne. 1) go to 182
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 73417) tr(it), tx(it)
73417 format ('+Type-97 R(t).', 10x, 2e12.4)
73418 if (itype .ne. 96) go to 4741
  nltype(inonl) = -96
  nonlad(inonl) = i_char + 1
  ilast(inonl) = nonlad(inonl) + 6
  num99 = num99 + 1
  vecnl1(inonl) = c(it)
  if (n3 * n4 .eq. 1) go to 4736
  i_char = i_char + 6
  nonle(inonl) = -i_char
  go to 182
4736 if (noutpr .eq. 0) write (unit = kunit6, fmt = 4738) tr(it), tx(it), c(it)
4738 format ('+Hysteresis.', 3e12.4)
4741 continue
  if (n3 .ne. 1 .or. n4 .ne. 1) go to 182
  if (itype .eq. 93 .and. noutpr .eq. 0) write (unit = kunit6, fmt = 54113) tr(it), tx(it)
54113 format ('+N.l. inductance, type 93.', 2x, 2e11.3)
  if (itype .gt. 91) go to 19
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54114) tr(it), c(it)
54114 format ('+Time-varying R, type 91.', 3x, 2e11.3)
  if (c(it) .eq. 3333.0d0) go  to 12
  kill = 5
  lstat(19) = 20
  go to 9999
12 nltype(inonl) = 923
  go to 21
19 if (itype .ne. 92) go to 80
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54115)  tr(it), tx(it), c(it)
54115 format ('+Gap, type 92.', 1x, 3e11.3)
  if (tr(it) .ne. 5555.0d0) go to 20
  kill = 28
  lstat(19) = 20
  go to 9999
20 if (c(it) .eq. 4444.0d0 .or. c(it) .eq. 5555.0d0) go to 21
  kill = 5
  lstat(19) = 20
  go to 9999
21 iprint = i_char + 1
  nonlad(inonl) = iprint
  iprint = 10
  !     read cards using cimage
  call cimage
  if (kolbeg .gt. 0) go to 22
  read (unit = abuff, fmt = 149) d2, d3, d4
149 format (3e25.0)
  go to 23
22 nfrfld = 1
  nright = 0
  !  call freone (d2)
  call free (d2)
  !  call freone (d3)
  call free (d3)
  !  call freone (d4)
  call free (d4)
23 ck1 = -fltinf
  a2 = 0.0d0
  vzero(inonl) = d4
  if (c(it) .eq. 4444.0d0) go to 40
  ! enter ZnO data code**********************************************
  if (d2 .gt. 0.0d0) go to 25
  kill = 28
  lstat(19) = 23
  go to 9999
25 anonl(inonl) = d2
  if (itype .eq. 91) go to  45
  if (d3 .le. 0.0d0) d3 = fltinf
  vnonl(inonl) = d3
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 57) d2, d3, d4
57 format ('+Vref, Vgap, Vinit', 1x, 3e11.3)
  ! read-in arrester characteristics*********************************
  iseg = 2
  if (d3 .eq. fltinf) iseg = 1
  if (iseg .eq. 2) vnonl(inonl) = d3 * d2
  do ibk = 1, iseg
     i_char = i_char + 1
     ichr = i_char
     icard = 0
29   icard = icard + 1
     ! read cards using cimage
     call cimage
     if (kolbeg .gt. 0) go to 30
     read (unit = abuff, fmt = 149) d2, d3, d4
     go to 31
30   nfrfld = 1
     nright = 0
     call free (d2)
     call free (d3)
     call free (d4)
31   if (d2 .eq. 9999.0d0) go to 37
     i_char = i_char + 1
     if (i_char .le. lchar) go to 36
     lstat(19) = 36
     go to 9000
36   continue
     cchar(i_char) = d2
     gslope(i_char) = d3
     vchar(i_char) = d4
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 59) d2, d3, d4
59   format ('+Breakpoint', 4x, 3e11.4)
     go to 29
37   if (icard .gt. 1) go to 38
     ! no valid data encountered, terminate run
     kill = 28
     lstat(19) = 37
     go to 9999
38   if (noutpr .eq. 0) write (unit = kunit6, fmt = 54118)
     if (ibk .eq. 1) ilast(inonl) = i_char
     ! calculate initial ( linear ) slope
     d11 = cchar(ichr + 1) * (vchar(ichr + 1) ** gslope(ichr + 1))
     cchar(ichr) = d11 / (vchar(ichr + 1) * anonl(inonl))
     vchar(ichr) = 0.0
  end do
  nonle(inonl) = i_char
  nltype(inonl) = 921
  go to 100
  ! enter code for piecewise linear and time varying resistance******
40 a2 = d2
  if (d3 .lt. 0.0d0) d3 = fltinf
  vnonl(inonl) = d3
  vecnl1(inonl) = tr(it)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 58) d2, d3, d4
58 format ('+Rlin, Vflash, Nflash', 1x, 3e10.3)
  go to 46
45 if (noutpr .eq. 0) write (unit = kunit6, fmt = 60) d2
60 format ('+V start=', e15.5)
  anonl(inonl) = 2.0 * d2
  vnonl(inonl) = d2
  ilast(inonl) = 1
  ! read-in nonlinear characteristic (mininmum two data points)******
46 icard = 0
47 icard = icard + 1
  ! read cards using cimage
  call cimage
  if (kolbeg .gt. 0) go  to  48
  read (unit = abuff, fmt = 149) d2, d3
  go to 49
48 nfrfld = 1
  nright = 0
  call free (d2)
  call free (d3)
49 if (d2 .eq. 9999.0d0) go to  53
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 59) d2, d3
  if (itype .eq. 91) go to 51
  if (d3 .gt. ck1) go to 51
  kill = 5
  lstat(19) = 49
  go to 9999
51 i_char = i_char + 1
  if (i_char .le. lchar) go  to  52
  lstat(19) = 51
  go to 9000
52 vchar(i_char) = d3 + a2 * d2
  ck1 = d3
  cchar(i_char) = d2
  go to 47
53 if (icard .gt. 2) go  to  54
  kill = 28
  lstat(19) = 53
  go to 9999
54 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54118)
  ! process input data (calculate slope + constant, extend data
  ! points to infinity , determine vreference************************
  ichr = nonlad(inonl)
  if (itype .eq. 91) go to 74
  d5 = absz (vchar(ichr))
  d3 = absz (vchar(i_char))
  if (d5 .gt. d3)  d3 = d5
  anonl(inonl) = d3
  ! determine whether only upper half of data specified *************
  if (vchar(ichr) .gt. 0.0 .and. cchar(ichr) .gt. 0.0) go to 55
  go to 74
  ! add ( 0.0, 0.0 ) point to the user's data ***********************
55 jk = i_char + 1
  if (jk .le. lchar) go to 70
  lstat(19) = 55
  go to 9000
70 do jb = ichr, i_char
     vchar(jk) = vchar(jk - 1)
     cchar(jk) = cchar(jk - 1)
     jk = jk - 1
  end do
  vchar(ichr) = 0.0
  cchar(ichr) = 0.0
  i_char = i_char + 1
74 ichr = ichr + 1
  do jb = ichr, i_char
     d5 = cchar(jb) - cchar(jb - 1)
     if (jb .eq. i_char .and. vchar(jb) .eq. vchar(jb - 1)) go to 77
     d5 = d5 / (vchar(jb) - vchar(jb - 1))
     go to 78
77   d5 = 0.0d0
78   d6 = cchar(jb) - vchar(jb) * d5
     gslope(jb - 1) = d5
     cchar(jb - 1) = d6
  end do
  i_char = i_char - 1
  nonle(inonl) = i_char
  if (itype .eq. 91) go to 76
  if (vchar(ichr - 1) .lt. 0.0) vchar(ichr - 1) = -fltinf
  ilast(inonl) = 1
  if (vnonl(inonl) .eq. fltinf .or. vnonl(inonl) .eq. 0.0) ilast( inonl ) = -1
  nltype(inonl) = 922
  go to 100
76 ilast(inonl) = 1
  vecnl1(inonl) = -1.0d0
  go to 100
80 ci1 = -fltinf
73420 ck1 = ci1
  if (itype .ne. 96) go to 73421
  i_char = i_char + 7
  go to 73422
73421 iprint = i_char + 1
  nonlad(inonl) = iprint
73422 iprint = 10
  !     read input card using cimage.
187 call cimage
  if (kolbeg .gt. 0) go to 73423
  read (unit = abuff, fmt = 188) d2, xlong
188 format (3e16.0)
  go to 73424
73423 nfrfld = 1
  nright = 0
  call free (d2)
  call free (xlong)
73424 if (d2 .eq. 9999.0d0) go to 189
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 181) d2, xlong
181 format ('+Breakpoint.', 2e15.5)
  if (d2 .gt. ci1) go to 4225
4224 kill = 5
  lstat(19) = 181
  go to 9999
4225 if (itype .ne. 96 .and. itype .ne. 98) go to 4227
  if (xlong .gt. ck1) go to 4226
  go to 4224
4227 if (xlong .ge. ck1) go to 4226
  if (itype .eq. 97) go to 4226
  go to 4224
4226 i_char = i_char + 1
  if (i_char .le. lchar) go to 6472
  iprint = 10
  lstat(19) = 4226
  go to 9000
6472 d3 = d2 - ci1
  d4 = xlong - ck1
  if (itype .ne. 99) go to 73430
  gslope(i_char) = d3 / d4
  cchar(i_char) = ci1 - gslope(i_char) * ck1
  go to 73436
73430 if (itype .ne. 98) go to 73437
  d1 = d4 / d3
  gslope(i_char) = delta2 / d1
  cchar(i_char) = ck1 - d1 * ci1
  go to 73436
73437 if (itype .ne. 97) go to 73441
  if (xlong .gt. 0.0) go to 7344
  kill = 72
  lstat(19) = 7344
  flstat(16) = xlong
  go to 9999
7344 gslope(i_char) = 1.0 / xlong
73441 if (itype .ne. 96) go to 73442
  cchar(i_char) = d2
  vchar(i_char) = xlong
  c1 = d2
  ck1 = xlong
  if (iprsup .ge. 4) write (unit = lunit(6), fmt = 4232) i_char, inonl, nonlad(inonl), d2, xlong
4232 format (/, ' Type-96  point.   i_char   inonl', 3x, 'nonlad(inonl)', 13x, 'd2', 10x, 'xlong', /, 16x,  2i8,  8x,  i8, 2e15.6)
  go to 187
73442 cchar(i_char) = d2
73436 ci1 = d2
  ck1 = xlong
  vchar(i_char) = xlong
  go to 187
189 if (itype .ne. 96) go to 4758
  n11 = nonlad(inonl)
  n12 = ilast(inonl)
  cchar(n12) = -cchar(i_char - 1)
  vchar(n12) = -vchar(i_char - 1)
  cchar(n11) = i_char - ilast(inonl)
  n13 = int (cchar(n11))
  ! calculate the slope and intercept for major hysteresis loop
  ! segments,  for  type-96  pseudononlinear element.
  n14 = i_char + 2
  n15 = n14 + n13 - 1
  do n16 = n14, n15
     n17 = n16 - n13 - 1
     vchar(n16) = (vchar(n17) - vchar(n17 - 1)) / (cchar(n17) - cchar(n17 - 1))
     cchar(n16) = vchar(n17) - vchar(n16) * cchar(n17)
     gslope(n17) = 1.0d0 / vchar(n16)
     gslope(n16) = -cchar(n16) / vchar(n16)
  end do
  n16 = i_char + n13 + 1
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 4234) inonl, i_char, n11, n13, tr(it), vecnl1(inonl), anonl(inonl)
4234 format (/, ' Process type-96.   inonl   i_char     n11     n13', 14x, 'tr(it)', 7x, 'vecnl1(inonl)', 8x, 'anonl(inonl)', /, 17x, 4i8, 3e20.11)
  cchar(i_char + 1) = -cchar(n16)
  vchar(i_char + 1) = vchar(n16)
  gslope(n12) = gslope(i_char)
  gslope(i_char + 1) = -gslope(n16)
  i_char = i_char + n13 + 1
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 4239) i_char, (cchar(j), vchar(j), gslope(j), j = n11, i_char)
4239 format (/, ' i_char =', i3, 5x, '(cchar(j), vchar(j), gslope(j), j = n11, i_char)  follow ...', /, (1x, 6e20.11))
  ! handling of steady-state current and flux conditions.
5309 n12 = ilast(inonl)
  n13 = nonlad(inonl)
  n13 = int (cchar(n13))
  n14 = n12 + n13 - 1
  if (tr(it) .eq. 8888.0d0) go to 5330
  ! make sure that the user-specified point lies within the major
  ! hysteresis loop.
  do n15 = n12, n14
     if (tr(it) .gt. cchar(n15)) cycle
     n16 = n15
     go to 5313
  end do
  n16 = n14 + 1
5313 n17 = n16 + n13 + 1
  d7 = vchar(n17) * tr(it) + cchar(n17) - flzero
  do n15 = n12, n14
     if (-tr(it) .gt. cchar(n15)) cycle
     n16 = n15
     go to 5323
  end do
  n16 = n14 + 1
5323 n17 = n16 + n13 + 1
  d8 = -vchar(n17) * (-tr(it)) - cchar(n17) + flzero
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5327) it, n16, n17, d7, d8, anonl(inonl), flzero
5327 format (/, ' Type-96 s.s. check.      it     n16     n17', 13x, 'd7', 13x, 'd8', 3x, 'anonl(inonl)', 9x, 'flzero', /, 20x, 3i8, 4e15.6)
  if (anonl(inonl) .le. d8 .and. anonl(inonl) .ge. d7) go to 5360
  kill = 204
  lstat(19) = 5323
  flstat(14) = tr(it)
  flstat(15) = tx(it)
  go to 9999
  !     it is desired to have the program calculate the values for
  !     steady-state flux and current based on 70 percent of saturation.
5330 d12 = 0.7d0
  anonl(inonl) = d12 * vchar(n14)
  do n15 = n12, n14
     if (cchar(n15) .lt. 0.0) cycle
     n16 = n15
     go to 5343
  end do
  n16 = n14 + 1
5343 n17 = n16 + n13 + 1
  d9 = -cchar(n17)
  d12 = 0.3d0
  d10 = anonl(inonl) - d12 * d9
  do n15 = n12, n14
     if (d10 .gt. vchar(n15)) cycle
     n16 = n15
     go to 5353
  end do
  n16 = n14 + 1
5353 n17 = n16 + n13 + 1
  vzero(inonl) = gslope(n16) * d10 + gslope(n17)
  if (iprsup  .ge.  2) write (unit = lunit(6), fmt = 5355) n12, n14, n16, n17, d9, anonl(inonl), vzero(inonl), gslope(n16), gslope(n17)
5355 format (/, ' Program calculated ss pt.  n12  n14  n16  n17 d9       anonl       vzero    gslope(n16)    gslope(n17)', /, 26x, 4i5, 3(1x, e15.6), 2e15.6)
  go to 5380
  !     make sure that the user-specified residual flux is within the
  !     major hysteresis loop.
5360 do n15 = n12, n14
     if (cchar(n15) .lt. 0.0) cycle
     n16 = n15
     go to 5373
  end do
  n16 = n14 +1
5373 n17 = n16 + n13 + 1
  d9 = -cchar(n17)
5380 d9 = absz(d9)
  d12 = vecnl1(inonl)
  d12 = absz(d12)
  if (d12 .le. d9 + flzero) go to 4758
  kill = 205
  lstat(19) = 5380
  flstat(16) = vecnl1(inonl)
  flstat(17) = d9
  go to 9999
4758 if (n3 * n4 .ne. 1) go to 100
  nonle(inonl) = i_char
  if (i_char .le. lchar) go to 4763
  lstat(19) = 4758
  iprint = 10
  go to 9000
4763 if (itype .ne. 99) go to 54127
  read (unit = abuff, fmt = 54125) bus1, bus2
54125 format (32x, 2a6)
  if (to_lower (bus1) .ne. text1) go to 54127
  if (to_lower (bus2) .ne. text2) go to 54127
  j = nonlad(inonl)
  sglfir = vchar(j)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54126)
54126 format ('+', 39x, "'1-flash'")
54127 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54118)
54118 format ('+Special termination-of-points card.')
  if (itype .ne. 93 .and. itype .ne. 96 .and. itype .ne. 98) go to 100
  nonle(inonl) = -i_char
  vnonl(inonl) = 0.
  go to 100
182 ii = inonl - 1
  if (ii .gt. 0) go to 4231
  lstat(19) = 182
  go to 4230
4231 if (noutpr .eq. 0) write (unit = kunit6, fmt = 21696) bus3, bus4
21696 format ('+Reference branch.   Copy  ', '"', a6,  '"', ' to ', '"', a6, '"')
  do i = 1, ii
     if (bus3 .eq. chcopy) go to 6881
     if (nonlk(i) .ne. n3) cycle
     if (iabs (nonlm(i)) .ne. n4) cycle
     go to 184
6881 n13 = namenl(i)
     if (bus4 .eq. texvec(n13)) go to 184
  end do
  lstat(19) = 183
4230 kill = 6
  go to 9999
184 if (itype .ne. 96) go to 269
  ilast(inonl) = ilast(i)
  n16 = nonlad(inonl)
  n17 = nonlad(i)
  cchar(n16) = cchar(n17)
  go to 5309
269 nonlad(inonl) = nonlad(i)
  if (itype .eq. 94) go to 100
  nonle(inonl)=nonle(i)
  if (itype .eq. 99) go to 100
  vzero(inonl) = vzero(i)
  anonl(inonl) = anonl(i)
  if (itype .ne. 91) go to 72
  if (c(it) .ne. 3333.) lstat(19) = 269
  go to 73
72 if (itype .ne. 92) go to 100
73 if (c(it) .ne. 4444. .and. c(it) .ne. 5555.) lstat( 19 ) = 269
  if (lstat(19) .eq. 269) go to 4230
  vnonl(inonl) = vnonl(i)
  vecnl1(inonl) = vecnl1(i)
  ilast(inonl) = ilast(i)
  nltype(inonl) = nltype(i)
  go to 100
270 if (i_char + 11 .le. lchar) go to 271
1270 iprint = 10
  lstat(19) = 270
  go to 9000
271 iaw = i_char + 1
  nonle(inonl) = iaw
  call move0 (vchar(iaw :), ll9)
  if (tx(it) .eq. 0.0) tx(it) = 1.0
  vchar(i_char + 10) = tx(it)
  if (c(it) .eq. 0.0) c(it) = 1.0
  vchar(i_char + 11) = c(it)
  if (n3 .ne. 1) go to 4271
  if (n4 .eq. 1) go to 3271
4271 i_char = i_char + 11
  go to 182
3271 nonlad(inonl) = -iaw
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 2271) tr(it), tx(it), c(it)
2271 format ('+Type-94 arrester.', e10.3, 2e11.4)
  if (i_char + 18 .gt. lchar) go to 1270
  j = 1
  ii = 4
  !     read input card using cimage
  call cimage
  n6 = j + i_char
  n7 = ii + i_char
  read (unit = abuff, fmt = 1272) (cchar(i), i = n6, n7)
1272 format (5e16.0)
  ii = 3
  n7 = ii + i_char
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 2272) j, ii, (cchar(i), i = n6, n7)
2272 format ('+consts. ', i2, '-', i2, '.', 3e11.3)
  do j = 5, 17, 3
     ii = j + 2
     if (ii .gt. 18) ii = 18
     !     read input card using cimage
     call cimage
     n6 = j + i_char
     n7 = ii + i_char
     read (unit = abuff, fmt = 1272) (cchar(i), i = n6, n7)
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 2272)  j, ii, (cchar(i), i = n6, n7)
  end do
  i_char = i_char + 18
100 go to 9999
9000 lstat(16) = iprint
  kill = 1
9999 if (iprsup .ge. 3 .or. kill .gt. 0) write (unit = lunit(6), fmt = 9998)  kill, inonl, num99, i_char, itype
9998 format (/, ' Exit  "nonln2" .    kill   inonl   num99   ichar   itype', /, 17x, 10i8)
  lstat(18) = 0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module nonln2."')
  return
end subroutine nonln2

!
! subroutine distr2.
!

subroutine distr2
  use blkcom
  use labcom
  use com2
  use indcom
  use tracom
  use strcom
  use freedom
  implicit none
  !  dimension cblhst(1)
  !  dimension  wk1(1)
  !  dimension infdli(1)
  !
  !  equivalence (cnvhst(1), cblhst(1))
  !  equivalence (semaux(1), wk1(1))
  !  equivalence (namebr(1), infdli(1))
  !  equivalence (omega, xlong1)
  !  equivalence (indtv(4), ichtr2)
  !  equivalence (indtv(1), iaddrs)
  !  equivalence (indtv(2), itranm)
  !
  character(8) :: text1, text2, text3, text4, text5, textmx(2)
  character(8) :: text6, text7, text8, text9, text10, text11
  character(8) :: text12, text13, text14
  integer(4) :: i, ibr2, ibr3, ibr15, ibrm, ibrm1, ifqq, ifqt
  integer(4) :: ii, ik, iluis, imarti, iml, inoff1, inoff2, inoff3, inoff4
  integer(4) :: inoff5, intcpl, ip, iprint, ips1, ips2, ips3, ipsem, irow
  integer(4) :: isecti
  integer(4) :: j, j1, j2, jj, jj1, jj2, jkl
  integer(4) :: k, kl, koff1, koff2, koff3, koff4, koff5, koff6, koff7, koff8
  integer(4) :: koff9, koff10, koff13, koff14, koff15, koff16, koff17, koff18
  integer(4) :: koff19, koff20
  integer(4) :: m, mxphas
  integer(4) :: n5, n6, n7, n8, n9, n9old, n10, n11, n12, n45, n46, ncount, nk1
  integer(4) :: nn11, nn12, nn13, nn17, np, npa, nphs, nphs2, nphsu, npll, npz
  integer(4) :: npza, nrowt, nt1, nt2, nt13, ntemp, nticpl, ntlin, numaki
  integer(4) :: nycmp
  real(8) :: a, aa, ak0zc, aki, akr
  real(8) :: d1, d2, d3, d4, d17, dddd
  real(8) :: h1, h2, h3
  real(8) :: pdt, pdt0
  real(8) :: tauo
  real(8) :: xlong, xsum
  real(8) :: ysum
  !
  integer(4), pointer :: iaddrs => indtv(1)
  integer(4), pointer :: ichtr2 => indtv(4)
  integer(4), pointer :: infdli(:)
  integer(4), pointer :: itranm => indtv(2)
  real(8), pointer :: cblhst(:)
  real(8), pointer :: wk1(:)
  real(8), pointer :: xlong1 => omega
  !
  data text2  / '   con' /
  data text3  / 'stant ' /
  data text4  / 'steady' /
  data text5  / ' state' /
  data text6  / 'a' /
  data text7  / 'b' /
  data text8  / 'c' /
  data text9  / 'd' /
  data text10 / 'e' /
  data text11 / 'f' /
  data text12 / 'g' /
  data text13 / 'h' /
  data text14 / 'i' /
  data textmx(1) / '+tr(  ' /
  data textmx(2) / '+tx(  ' /
  !     Burroughs: preserve local variable between module calls:
  data ipsem  / 0 /
  !
  infdli => namebr
  cblhst => cnvhst
  wk1 => semaux
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module distr2."')
  n45 = location (ida)
  n46 = location (ifkc)
  isecti = 400
  if (iprsup .ge. 4) write (unit = *, fmt = *) ' top distr2.  n45, n46, ida, ifkc =', n45, n46, ida, ifkc
  numaki = 0
  lstat(18) = 0
  pdt0 = 0
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 3213) icheck, moldat, ibr, itype
3213 format (/, ' Begin  "distr2" .  icheck  moldat     ibr   itype', /, 18x, 4i8, /, 1x)
  n13 = 0
  if (icheck .eq. 3) go to 196
  if (icheck .eq. 5) go to 20000
190 if (moldat .eq. 0) go to 3265
  ! decode (80, 3241, abuff(1))  text1
  ! 3241 format ( 75x,  a1 )
  ! if ( text1  .eq.  blank )   go to 3265
  read (unit = abuff, fmt = 3256) h1, aa, h3, xlong, iline, ipunch
3256 format (26x, 4e12.0, 2i2)
  if (xlong .gt. 0.) go to 21633
  xlong = absz (xlong)
  read (unit = abuff, fmt = 3257) text1
3257 format (78x, a1)
  if (to_lower (text1) .eq. text6) n13 = 10
  if (to_lower (text1) .eq. text7) n13 = 11
  if (to_lower (text1) .eq. text8) n13 = 12
  if (to_lower (text1) .eq. text9) n13 = 13
  if (to_lower (text1) .eq. text10) n13 = 14
  if (to_lower (text1) .eq. text11) n13 = 15
  if (to_lower (text1) .eq. text12) n13 = 16
  if (to_lower (text1) .eq. text13) n13 = 17
  if (to_lower (text1) .eq. text14) n13 = 18
  if (n13 .ne. 0) go to 21633
  read (unit = abuff, fmt = 3258) n13
3258 format (78x, i1)
  go to 21633
3265 continue
  read (unit = abuff, fmt = 191) ipsem
191 format (75x, i3)
  ! if (ipsem .ne. 0) go to 21633
  if (ipsem .ne. 0) go to 8010
  read (unit = abuff, fmt = 1920) h1, aa, h3, xlong, iline, ipunch, n13, lint
1920 format (26x, 4e6.2, 4i2)
  ! 2345678901234567890123456789012345678901234567890123456789012345678901
  if (iprsup .ge. 4) write (unit = *, fmt = *) ' distr2, extract from col 55-56  n13 =',  n13
  go to 21633
  ! recursive convolution branch processing begins here for semlyem (ljg's
8010 ipunch = iabs (ipsem)
  if (ipunch .le. 50) go to 8030
  kill = 117
  lstat(19) = 8030
  go to 9999
8030 ips1 = -ipunch
  ips2 = 0
  ips3 = 0
  n5 = ipsem
8040 continue
  read (unit = abuff, fmt = 8045) d1, d2, d3, n1, n2, n3, n4
8045 format (26x, 3e12.5, 4i3)
  if (ipsem .ne. n5) go to 8050
  if (n2 .le. 0 .or. n2 .gt. ipunch) go to 8050
  n5 = n1 + (n2 - 1) * ipunch
  if (n5 .le. ips1) go to 8050
  if (n5 .ne. ips1 + ipunch + 1) ips3 = 1
  if (ips2 .le. ipunch .or. iabs (mbus(ibr)) .eq. 1) go to 8060
8050 kill = 169
  lstat(19) = 8050
  go to 9999
8060 kodebr(ibr) = ipunch
  n6 = ifx
  n7 = ifx
  ifsem = ifsem + 1
  if (ifsem .le. lfsem) go to 8100
8090 iprint = 20
  lstat(19) = 8090
  go to 9000
8100 sconst(ifsem) = d1
  ci(ibr) = n3
  ck(ibr) = n4
  cik(ibr) = iaddrs
  cki(ibr) = n5
  indhst(ibr) = ifx + 1
  ips1 = n5
  ips2 = ips2 + 1
  ! make length(ibr) conditional for toronto.  6 oct 1981:
  if (ips3 .eq. 0) length(ibr) = ipunch
  if (ipunch .gt. mxphas) mxphas = ipunch
  bus5 = text2
  bus6 = text3
  if (ipsem .lt. 0) go to 8140
  kodsem(ibr) = ifsem
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 8110) d1, d2, n1, n2
8110 format ('+Convolution.', 2e13.4, 2i4)
  if (n3 .le. 0) go to 8160
  n6 = n6 + 1
  if (n6 .le. lhist) go to 8130
8120 iprint = 22
  lstat(19) = 8120
  go to 9000
8130 cnvhst(n6) = d2
  if (d2 .gt. deltat) go to 8160
  kill = 197
  lstat(19) = 8130
  go to 9999
8140 kodsem(ibr) = -ifsem
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 8150) n1, n2, d1
8150 format ('+Lumped element convolution (', i2, ',', i2, ')', 1x, e10.3)
  ifsem = ifsem + 1
  if (ifsem .gt. lfsem) go to 8090
  ifx = ifx + 1
  if (ifx .gt. lhist) go to 8120
  sconst(ifsem) = d2
8160 if (ips2 .gt. ipunch) go to 8190
  ! read input card using cimage
  call cimage
  n8 = n6 + 1
  n6 = n6 + 5
  if (n6 .gt. lhist) go to 8120
  if (kolbeg .le. 0) go to 8161
  nfrfld = 5
  !  call frefld (cnvhst(n8 :))
  call free (cnvhst(n8 :))
  go to 8162
8161 if (moldat .eq. 1) go to 18161
  read (unit = abuff, fmt = 18170) (cnvhst(i), i = n8, n6)
  go to 8162
18161 continue
  read (unit = abuff, fmt = 8170) (cnvhst(i), i = n8, n6)
8162 d1 = cnvhst(n6)
18170 format (2x, 5e15.8)
8170 format (2x, 6e12.5)
  cnvhst(n6) = cnvhst(n6) * twopi
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 8180) cnvhst(n8), cnvhst(n8 + 1), cnvhst(n8 + 2)
8180 format ('+Phasor Z, Y.', 3e12.4)
  !8190 if (n3) 8200, 8270, 8240
8190 if (n3 .lt. 0) then
     go to 8200
  else if (n3 .eq. 0) then
     go to 8270
  else
     go to 8240
  end if
8200 n9 = 0
  numaki = 1
8205 n8 = ifsem + 1
  n10 = - n3 - n9
  if (n10 .gt. 3) n10 = 3
  ifsem = ifsem + n10 + n10
  if (ifsem .gt. lfsem) go to 8090
  ! read input card using cimage
  call cimage
  if (kolbeg .le. 0) go to 8206
  nfrfld = ifsem - n8 + 1
  call free (sconst(n8 :))
  go to 8208
8206 continue
  read (unit = abuff, fmt = 8170) (sconst(i), i = n8, ifsem)
8208 if (n9 .eq. 0) go to 8207
  if (sconst(n8) - sconst(n8 - 2) .lt. deltat) go to 8210
8207 if (n9 .eq. 0 .and. sconst(n8) .lt. deltat) go to 8210
  if (n10 .le. 1) go to 8220
  if (sconst(n8 + 2) - sconst(n8) .lt. deltat) go to 8210
  if (n10 .le. 2) go to 8220
  if (sconst(n8 + 4) - sconst(n8 + 2) .gt. deltat) go to 8220
8210 kill = 197
  lstat(19) = 8210
  go to 9999
8220 n8 = n9 + 1
  n9 = n9 + n10
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 8230) n8, n9
8230 format ('+Ametani propagation impulse, segments ', i3, ' thru ', i3)
  if (n9 .lt. - n3) go to 8205
  if (d2 .le. 0.0) d2 = tmax
  if (d2 - sconst(ifsem - 1) .lt. deltat .and. d2 .gt. sconst(ifsem - 1)) go to 8210
  sconst(ifsem) = (unity - sconst(ifsem))/(d2 - sconst(ifsem - 1))
  if (sconst(ifsem) .lt. 0.0) sconst(ifsem) = 0.0
  ifx = ifx + 2
  if (ifx .gt. lhist) go to 8120
  go to 8270
8240 n9 = 0
8250 n8 = ifsem + 1
  n10 = n3 - n9
  if (n10 .gt. 2) n10 = 2
  n11 = 3 * n10
  ifsem = ifsem + 5 * n10
  if (ifsem .gt. lfsem) go to 8090
  !     read input card using cimage
  call cimage
  if (kolbeg .le. 0) go to 8251
  nfrfld = n11
  call free (voltbc)
  go to 8252
8251 continue
  read (unit = abuff, fmt = 8170) (voltbc(i), i = 1, n11)
8252 n11 = 1
  do i = n8, ifsem, 5
     sconst(i + 0) = voltbc(n11 + 0)
     sconst(i + 1) = voltbc(n11 + 1)
     sconst(i + 2) = voltbc(n11 + 2)
     n11 = n11 + 3
     if (sconst(i) .ge. 0.0) cycle
  end do
  n8 = n9 + 1
  n9 = n9 + n10
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 8260) n8, n9, voltbc(1), voltbc(2)
8260 format ('+Propagation exp.', i3, '-', i2, '. ', 2e12.3)
  if (n9 .lt. n3) go to 8250
  ifx = ifx + n3 + n3
  if (ifx .gt. lhist) go to 8120
  !8270 if (n4) 8280, 8340, 8310
8270 if (n4 .lt. 0) then
     go to 8280
  else if (n4 .eq. 0) then
     go to 8340
  else
     go to 8310
  end if
8280 n9 = 0
8290 n8 = ifsem + 1
  n10 = - n4 - n9
  if (n10 .gt. 3) n10 = 3
  ifsem = ifsem + n10 + n10
  if (ifsem .gt. lfsem) go to 8090
  !     read input card using cimage
  call cimage
  if (kolbeg .le. 0) go to 8291
  nfrfld = ifsem - n8 + 1
  call free (sconst(n8 :))
  go to 8293
8291 continue
  read (unit = abuff, fmt = 8170) (sconst(i), i = n8, ifsem)
8293 if (n9 .eq. 0) go to 8292
  if (sconst(n8) - sconst(n8 - 2) .lt. deltat) go to 8210
8292 if (n9 .eq. 0 .and. sconst(n8) .ne. 0.0) go to 8210
  if (n10 .le. 1) go to 8294
  if (sconst(n8 + 2) - sconst(n8) .lt. deltat) go to 8210
  if (n10 .le. 2) go to 8294
  if (sconst(n8 + 4) - sconst(n8 + 2) .lt. deltat) go to 8210
8294 n8 = n9 + 1
  n9 = n9 + n10
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 8300) n8, n9
8300 format ('+Ametani admittance impulse, segment ', i3, ' thru ', i3)
  if (n9 .lt. -n4) go to 8290
  if (d3 .le. 0.0) d3 = tmax
  if (d3 - sconst(ifsem - 1) .lt. deltat .and. d3 .gt. sconst(ifsem - 1)) go to 8210
  sconst(ifsem) = sconst(ifsem) / (d3 - sconst(ifsem - 1))
  if (d3 .lt. sconst(ifsem - 1)) sconst(ifsem) = 0.0
  ifx = ifx + 1
  if (ipsem .gt. 0) ifx = ifx + 1
  if (ifx .gt. lhist) go to 8120
  go to 8340
8310 n9 = 0
8320 n8 = ifsem + 1
  n10 = n4 - n9
  if (n10 .gt. 2) n10 = 2
  n11 = 3 * n10
  ifsem = ifsem + 4 * n10
  if (ifsem .gt. lfsem) go to 8090
  ! read input card using cimage
  call cimage
  if (kolbeg .le. 0) go to 8321
  nfrfld = n11
  call free (voltbc)
  go to 8322
8321 continue
  read (unit = abuff, fmt = 8170) (voltbc(i), i = 1, n11)
8322 n11 = 1
  do i = n8, ifsem, 4
     sconst(i + 0) = voltbc(n11 + 0)
     sconst(i + 1) = voltbc(n11 + 1)
     sconst(i + 2) = voltbc(n11 + 2)
     n11 = n11 + 3
     if (sconst(i) .ge. 0.0) cycle
  end do
  n8 = n9 + 1
  n9 = n9 + n10
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 8330) n8, n9, voltbc(1), voltbc(2)
8330 format ('+Admittance  exp.', i3, '-', i2, '. ', 2e12.3)
  if (n9 .lt. n4) go to 8320
  ifx = ifx + n4
  if (ipsem .gt. 0) ifx = ifx + n4
  if (ifx .gt. lhist) go to 8120
8340 if (n6 .gt. ifx) ifx = n6
  nr(ibr) = ifx - n7
  icheck = 5
  nrecur = nrecur + 1
  if (nrecur .eq. ipunch) go to 8351
  go to 100
8350 continue
  if (iabs (mbus(ibr)) .eq. ntot) ntot = ntot - 1
  if (iabs (kbus(ibr)) .eq. ntot) ntot = ntot - 1
  ibr = ibr - 1
8351 cki(ibr) = -cki(ibr)
  if (ips3 .eq. 0) go to 8357
  n1 = -iadd - 1
  n2 = ibr - ips2 + 1
  do i = n2, ibr
     cik(i) = n1
  end do
  bus5 = text4
  bus6 = text5
8357 n1 = iaddrs - 1
  n3 = ipunch * ipunch
  iadd = iaddrs + n3 + n3 - 1
  if (iadd .le. lfd) go to 8360
  iprint = 21
  lstat(19) = 8360
  go to 9000
8360 n2 = n1 + n3
  n4 = 1
  n5 = 1
  n8 = ipunch * 2
  !                   !total # of elements in matrix for the coupled group
  irow = 0
  !     read input card using cimage
8370 call cimage
  if (kolbeg .le. 0) go to 8376
18375 irow = irow + 1
  n9old = (irow - 1 ) * 6
  n9 = irow * 6
  if (n8 .lt. n9) go to 38376
  nfrfld = 6
  go to 18377
38376 nfrfld = n8 - n9old
  if (nfrfld .gt. 0) go to 18377
  irow = 0
  go to 18375
18377 call free (voltbc)
  go to 8377
8376 continue
  read (unit = abuff, fmt = 18376) (voltbc(i), i = 1, 6)
18376 format (2x, 6e12.5)
8377 if (noutpr .eq. 0) write (unit = kunit6, fmt = 8380) n5, (voltbc(i), i = 1, 3)
8380 format ('+[tv] row', i3, '.', 3e12.3)
  do i = 1, 5, 2
     n6 = n4 + n1
     sfd(n6) = voltbc(i)
     if (numaki .eq. 0) go to 8381
     n4 = n4 + ipunch
     if (n4 .gt. n3 ) go to 8400
     sfd(n4) = voltbc(i + 1)
     go to 8382
8381 n6 = n4 + n2
     sfd(n6) = voltbc(i + 1)
8382 n4 = n4 + ipunch
     if (n4 .gt. n3) go to 8400
  end do
  go to 8370
8400 n5 = n5 + 1
  n4 = n5
  if (n5 .le. ipunch) go to 8370
  n4 = 1
  n5 = 1
  irow = 0
  ! read input card using cimage
8410 call cimage
  if (kolbeg .le. 0) go to 8415
28375 irow = irow + 1
  n9old = (irow - 1) * 6
  n9 = irow * 6
  if (n8 .lt. n9) go to 28376
  nfrfld = 6
  go to 28377
28376 nfrfld = n8 - n9old
  if (nfrfld .gt. 0) go to 28377
  irow = 0
  go to 28375
28377 call free (voltbc)
  go to 8417
8415 continue
  read (unit = abuff, fmt = 18376) (voltbc(i), i = 1, 6)
8417 if (noutpr .eq. 0) write (unit = kunit6, fmt = 8420) n5, (voltbc(i), i = 1, 3)
8420 format ('+[ti] row', i3, '.', 3e12.3)
  do i = 1, 5, 2
     n6 = n4 + n1
     qfd(n6) = voltbc(i)
     if (numaki .eq. 0) go to 8421
     n4 = n4 + ipunch
     if (n4 .gt. n3 ) go to 8440
     qfd(n4) = voltbc(i + 1)
     go to 8422
8421 n6 = n4 + n2
     qfd(n6) = voltbc(i + 1)
8422 n4 = n4 + ipunch
     if (n4 .gt. n3) go to 8440
  end do
  go to 8410
8440 n5 = n5 + 1
  n4 = n5
  if (n5 .le. ipunch) go to 8410
  icheck = 1
  nrecur = 0
  iaddrs = iadd + 1
  go to 100
20000 kbus(ibr) = n1
  mbus(ibr) = n2
  read (unit = abuff, fmt = 191) n5
  if (n5 .eq. 0) go to 8350
  go to 8040
  !     end of recursive convolution branch input ************************
21633 continue
  if (ipsem .ne. 0) imodel(ibr) = -4
  if (ipsem .eq. 0) go to 1923
  read (unit = abuff, fmt = 8045) d1, d2, d3, nt1, nt2, n3, n4
1923 if (ipsem .eq. 0) imodel(ibr) = ipunch
  if (imodel(ibr) .ne. -4) go to 76893
  !     1 .and.imodel(ibr).ne.-2)go to 76893    !if not marti model bapass
  pdt0 = aa
  if (pdt0 .ge. 1. .or. pdt0 .eq. 0.) go to 6100
  pdt0 = 1.
  write (unit = lunit(6), fmt = 1006)
1006 format (//, ' A too low value of p*deltat was read. p*deltat changed to 1.', //)
6100 pdt = pdt0
  ifx = ifx + 1
  if (ifx .gt. lhist) go to 8120
  ifsem = ifsem + 1
  if (ifsem .gt. lfsem) go to 8090
  if (ipsem .ne. 0) itype = itype - 1
  if (ipsem .eq. 0) go to 1989
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 1990) nt1
1990 format (" Semlyen's  mode", i3, '  parameters begin')
  cki(ibr) = n4
  npz = n4
  sconst(ifsem) = d1
  kodsem(ibr) = ifsem
  indhst(ibr) = ifx
  go to 1988
1989 n11 = -itype
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 6729)  n11
6729 format (' Cable mode', i3, '  parameters begin')
  aa = noutpr
  if (h1 .eq. 2.0) noutpr = 1
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 6733) npz, ak0zc
6733 format (i8, e32.20)
  cki(ibr) = npz
  sconst(ifsem) = ak0zc
  kodsem(ibr) = ifsem
  indhst(ibr) = ifx
  if (noutpr .ne. 0) npll = noutpr
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 6735) npz, ak0zc
6735 format ('+Yc begins. Order, Yc(infinity) =', i5, e11.3)
  if (h1 .eq. 1.0) noutpr = 1
  n11 = 1
6738 n12 = n11 + 2
  if (n12 .gt. npz) n12 = npz
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 6741) (voltbc(ii), ii = n11, n12)
6741 format (3e26.0)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 6744) n11, n12, (voltbc(ii), ii = n11, n12)
6744 format ('+Residuals', i3, '-', i2, '.', 3e11.3)
  nn11 = n11 + ifsem
  nn12 = n12 + ifsem
  i = n11
  do jj = nn11, nn12
     sconst(jj) = voltbc(i)
     i = i + 1
  end do
  n11 = n11 + 3
  if (n11 .le. npz) go to 6738
  nycmp = 0
  n11 = 1
6753 n12 = n11 + 2
  if (n12 .gt. npz) n12 = npz
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 6741) (voltbc(ii), ii = n11, n12)
  if (noutpr .eq. 0 ) write (unit = lunit(6), fmt = 6755) n11, n12, (voltbc(ii), ii = n11, n12)
6755 format ('+Poles', i3, '-', i2, '.', 3e12.4)
  nn11 = nn12 + n11
  nn13 = nn12 + n12
  i = n11
  do jj = nn11, nn13
     sconst(jj) = voltbc(i)
     if (sconst(jj) .ge. 1.e+14 .or. sconst(jj) .le. -1.e+14) nycmp = nycmp + 1
     i = i + 1
  end do
  n11 = n11 + 3
  if (n11 .le. npz) go to 6753
  !     go to 3567
  nycmp = nycmp * 3
  nk1 = ifx + 19 + 1
  cnvhst(nk1) = nycmp
  if (h1 .eq. 1.0) noutpr = int (aa)
  if (pdt0 .eq. 0.) go to 45000
  npza = npz
  ii = 0
40021 n11 = ifsem + npza + ii + 1
  n12 = n11 + npza - 1
  xsum = 0.0d0
  ysum = 0.0d0
  do jj = n11, n12
     if (sconst(jj) * deltat .lt. pdt) cycle
     go to 40001
  end do
  go to 40007
40001 if (jj .eq. n11) jj = jj + 1
  if (jj .eq. n12) go to 40007
  do i = jj, n12
     nn11 = i - npza
     xsum = xsum + sconst(nn11) / sconst(i)
     ysum = ysum + sconst(nn11) / sconst(i) ** 2
  end do
  ysum = xsum / ysum
  if (ysum * deltat .ge. pdt) go to 40030
  pdt = pdt + 1.
  go to 40021
40030 nn11 = jj - npza
  if (ii .ne. 0) go to 40003
  pdt = pdt0
  npz = npz- n12 + jj
  cki(ibr) = npz
  go to 40004
40003 npa = npa - n12 + jj
  ckkjm(ibr) = npa
40004 sconst(nn11) = ysum * xsum
  sconst(jj) = ysum
  i = n12 - jj + 1
  if (ii .eq. 0 .and. noutpr .eq. 0) write (unit = lunit(6), fmt = 40005) i, sconst(nn11), sconst(jj)
40005 format (' Z0 equivalence.  I, R * P, P =', i5, 2e20.13)
  if (ii .ne. 0 .and. noutpr .eq. 0) write (unit = lunit(6), fmt = 40006) i, sconst(nn11), sconst(jj)
40006 format ( ' A1 equivalence.  I, R * P, P =', i5, 2e20.13)
  !      if (pdt.ne.pdt0)
  !     1 write(lunit(6),40031)pdt
  !40031 format(//' *** note - pdt changed to *** ',f7.2//)
  n12 = n12 - i + 1
  jj = nn11
  do i = n11, n12
     jj = jj + 1
     sconst(jj) = sconst(i)
  end do
  nn13 = jj
  !     pointer for no. of complex poles must be reset after pole
  !     reduction, thl.  march 20, 1990
  !     40007 if (ii.ne.0)go to 40022
40007 if (ii .eq. 0) go to 3179
  ntemp = int (sconst(nk1))                                 ! save no. of complex poles
  nk1 = ifsem + 5 * (npa + npz + 1)                         ! define the new pointer
  sconst(nk1 ) = ntemp                                      ! store the number in the new cell
  go to 40022
  !     end of thl's correction, march 20, 1990
  !     read input card using cimage
3179 if (ipsem .eq. 0) go to 45000
1988 tauo = d2
  npa = n3
  go to 1992
45000 call cimage
  ! 3456789012345678901234567890123456789012345678901234567890123456789012
  read (unit = abuff, fmt = 6733) npa, tauo
1992 if (tauo .gt. deltat) go to 6756
  kill = 197
  flstat(14) = tauo
  lstat(19) = 6756
  go to 9999
6756 ckkjm(ibr) = npa
  if (ipsem .ne. 0) go to 2016
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 6759) npa, tauo
6759 format ('+A1 begins. Order, tau(sec) =', i6, e14.5)
  if (h1 .eq. 1.0) noutpr = 1
2016 if (ipsem .eq. 0) go to 2002
  call cimage
  n8 = ifx + 15
  n6 = ifx + 19
  read (unit = abuff, fmt = 18170) (cnvhst(i), i = n8, n6)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 2019) (cnvhst(i), i = n8, n6)
2019 format (' Phasor Z-Y.', 3e12.3)
  n11 = npz * 2 + ifsem + 1
  nt13 = n11 + npa
  nn17 = 0
  do j = 1, npa / 2
     call cimage
     nfrfld = 6
     if (kolbeg .le. 0) go to 1993
     call free (voltbc)
     go to 1994
1993 continue
     read (unit = abuff, fmt = 8170) (voltbc(i), i = 1, nfrfld)
1994 n12 = 1
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 2017) (voltbc(ii), ii = 1, nfrfld)
2017 format (' Propagation exp.', 2e12.3)
     do i = 1, 6, 3
        if (voltbc(n12) .eq. -1) go to 2022
        if (voltbc(n12) .ne. 1) go to 2021
        nn17 = nn17 + 1
        akr = voltbc(n12 + 2) * voltbc(n12 + 1) - voltbc(n12 + 5) * voltbc(n12 + 4)
        akr = voltbc(n12 + 2)
        akr = akr * 1.e+15
        aki = voltbc(n12 + 2) * voltbc(n12 + 4) + voltbc(n12 + 5) * voltbc(n12 + 1)
        aki = voltbc(n12 + 5)
        aki = aki * 1.e+15
        voltbc(n12 + 2) = akr
        voltbc(n12 + 5) = aki
        go to 2022
2021    dddd = voltbc(n12 + 1) * voltbc(n12 + 2)
        sconst(n11) = dddd
        if (voltbc(n12 + 1) .eq. 0) voltbc(n12 + 1) = 1.
        sconst(nt13) = voltbc(n12 + 1)
        go to 2023
2022    sconst(n11) = voltbc(n12 + 2)
        sconst(nt13) = voltbc(n12 + 1) * 1.e+15
2023    n12 = n12 + 3
        n11 = n11 + 1
        nt13 = nt13 + 1
     end do
  end do
  n11 = ifsem + 1
  n13 = n11 + npz
  do j = 1, npz / 2
     call cimage
     nfrfld = 6
     if (kolbeg .le. 0) go to 1998
     call free (voltbc)
     go to 1999
1998 continue
     read (unit = abuff, fmt = 8170) (voltbc(i), i = 1, nfrfld)
1999 n12 = 1
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 2018) (voltbc(ii), ii = 1, nfrfld)
2018 format (' Char. admt. exp.', 2e12.3)
     do i = 1, 6, 3
        dddd = voltbc(n12 + 1) * voltbc(n12 + 2)
        sconst(n11) = dddd
        if (voltbc(n12 + 1) .eq. 0) voltbc(n12 + 1) = 1.
        sconst(n13) = voltbc(n12 + 1)
        n12 = n12 + 3
        n11 = n11 + 1
        n13 = n13 + 1
     end do
  end do
  nn17 = nn17 * 2
  go to 2001
2002 n11 = 1
6764 n12 = n11 + 2
  if (n12 .gt. npa) n12 = npa
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 6741) (voltbc(ii), ii = n11, n12)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 6744) n11, n12, (voltbc(ii), ii = n11, n12)
  nn11 = nn13 + n11
  nn12 = nn13 + n12
  i = n11
  do jj = nn11, nn12
     sconst(jj) = voltbc(i)
     i = i + 1
  end do
  n11 = n11 + 3
  if (n11 .le. npa) go to 6764
  nn17 = 0
  n11 = 1
6772 n12 = n11 + 2
  if (n12 .gt. npa) n12 = npa
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 6741) (voltbc(ii), ii = n11, n12)
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 6755) n11, n12, (voltbc(ii), ii = n11, n12)
  nn11 = nn12 + n11
  nn13 = nn12 + n12
  i = n11
  do jj = nn11, nn13
     sconst(jj) = voltbc(i)
     if (sconst(jj) .ge. 1.e+14 .or. sconst(jj) .le. -1.e+14) nn17 = nn17 + 1
     i = i + 1
  end do
  n11 = n11 + 3
  if (n11 .le. npa) go to 6772
2001 nn17 = nn17 * 3
  nk1 = ifsem + 5 * npa + 5 * npz + 4 + 1
  sconst(nk1) = nn17
  !      if ( imodel(ibr) .eq. -4 )    go to 40022
  if (pdt .eq. 0.) go to 40022
  ii = 2 * npz
  npza = npa
  go to 40021
40022 noutpr = int (aa)
  cnvhst(ifx) = tauo
  !
  inoff5 = 5 * lbrnch
  infdli(inoff5 + ibr) = int (cnvhst(ifx) / deltat) + 1
  !
  ifx = ifx + 19 + 1 + nycmp
  if (ifx .gt. lhist) go to 8120
  ifsem = ifsem + 11 * npa + nn17 + 7 * npz + 9
  if (kcount .eq. model .or. nt1 .eq. ipsem) ifq = ifsem
  if (ifsem .gt. lfsem)  go to 8090
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4834) ifx, npz, npa, ibr, ifsem
4834 format (' Done Marti cards.  ifx, npz, npa, ibr,ifsem =', 5i6)
  icheck = 3
  length(ibr1)=kcount
  if (kcount .gt. mxphas) mxphas = kcount
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6178) itype, n13, model, ipunch
6178 format (' Below 21105. itype, n13, model, ipunch =', 4i6)
  !     if ( ipunch  .ne.  -4
  !    1    .and. ipsem .eq. 0)   go to 100          !flag for jose's case
  if (ipsem .ne.0 .and. nt1 .eq. ipsem) go to 50050
  if (kcount .eq. model) go to 50050
  if (kcount .ne. 1) go to 100
  if (n13 .lt. 1) go to 100
  model = n13
  if (ipsem .ne. 0) model = ipsem
  litype(ibr) = iaddrs
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 62078) model, iaddrs, ibr1
62078 format ('   Cable model for untransposed line, model, iaddrs, ib r1 =', 3i10)
  if (n13 .eq. 1) go to 50050
  go to 100
  !
  !
76893 imodel(ibr) = ipunch
  if (ipunch .ne. -2) go to 10105
  !     beginning of input for jose marti's branches
  if (ipunch .ne. -2) imarti = imarti + 1
  if (iluis .ne. 0) write (unit = lunit(6), fmt = 1067)
1067 format ( ' Attention. You have mixed Luis- with Jose- model')
  pdt0 = aa
  if (pdt0 .ge. 1. .or. pdt0 .eq. 0.) go to 6000
  pdt0 = 1.
  write (unit = lunit(6), fmt = 6001)
6001 format (//, ' A too low value of P * DeltaT was read. P * DeltaT changed to 1.', //)
6000 pdt = pdt0
  ! 3456789012345678901234567890123456789012345678901234567890123456789012
  ifx = ifx + 1
  if (ifx .gt. lhist) go to 8120
  ifsem = ifsem + 1
  if (ifsem .gt. lfsem) go to 8090
  n11 = -itype
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 7629)  n11
7629 format ('+marti transmission mode', i3, '  parameters begin.')
  aa = noutpr
  if (h1 .eq. 2.0) noutpr = 1
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 7633) npz, ak0zc
7633 format (i8, e32.20)
  cki(ibr) = npz
  sconst(ifsem) = ak0zc
  kodsem(ibr) = ifsem
  indhst(ibr) = ifx
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 7635) npz, ak0zc
7635 format ('+zc begins. Order, Zc(infinity) =', i5, e11.3)
  if (h1 .eq. 1.0) noutpr = 1
  n11 = 1
7638 n12 = n11 + 2
  if (n12 .gt. npz) n12 = npz
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 7641) (voltbc(ii), ii = n11, n12)
7641 format (3e26.0)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 7644) n11, n12, (voltbc(ii), ii = n11, n12)
7644 format ('+Residuals', i3, '-', i2, '.', 3e11.3)
  nn11 = n11 + ifsem
  nn12 = n12 + ifsem
  i = n11
  do jj = nn11, nn12
     sconst(jj) = voltbc(i)
     i = i + 1
  end do
  n11 = n11 + 3
  if (n11 .le. npz) go to 7638
  n11 = 1
7653 n12 = n11 + 2
  if (n12 .gt. npz) n12 = npz
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 7641) (voltbc(ii), ii = n11, n12)
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 7655) n11, n12, (voltbc(ii), ii = n11, n12)
7655 format ('+Poles', i3, '-', i2, '.', 3e12.4)
  nn11 = nn12 + n11
  nn13 = nn12 + n12
  i = n11
  do jj = nn11, nn13
     sconst(jj) = voltbc(i)
     i = i + 1
  end do
  n11 = n11 + 3
  if (n11 .le. npz) go to 7653
  if (h1 .eq. 1.0) noutpr = int (aa)
  if (pdt0.eq.0.) go to 55000
  npza = npz
  ii = 0
50021 n11 = ifsem + npza + ii + 1
  n12 = n11 + npza - 1
  xsum = 0.
  ysum = 0.
  do jj = n11, n12
     if (sconst(jj) * deltat .lt. pdt) cycle
     go to 50001
  end do
  go to 50007
50001 if (jj .eq. n11) jj = jj + 1
  if (jj .eq. n12) go to 50007
  do i = jj, n12
     nn11 = i - npza
     xsum = xsum + sconst(nn11) / sconst(i)
     ysum = ysum + sconst(nn11) / sconst(i) ** 2
  end do
  ysum = xsum / ysum
  if (ysum * deltat .ge. pdt) go to 50030
  pdt = pdt + 1.
  go to 50021
50030 nn11 = jj - npza
  if (ii .ne. 0) go to 50003
  pdt = pdt0
  npz = npz - n12 + jj
  cki(ibr) = npz
  go to 50004
50003 npa = npa - n12 + jj
  ckkjm(ibr) = npa
50004 sconst(nn11) = ysum * xsum
  sconst(jj) = ysum
  i = n12 - jj + 1
  if (ii .eq. 0 .and. noutpr .eq. 0) write (unit = lunit(6), fmt = 50005) i, sconst(nn11), sconst(jj)
50005 format (' Z0 equivalence.  I, R * P, P =', i5, 2e20.13)
  if (ii .ne. 0 .and. noutpr .eq. 0) write(unit = lunit(6), fmt = 50006) i, sconst(nn11), sconst(jj)
50006 format (' A1 equivalence.  I, R * P, P =', i5, 2e20.13)
  !      if (pdt.ne.pdt0 .and. noutpr .eq. 0)
  !     1 write(lunit(6),50031)pdt
  !50031 format(//' *** note - pdt changed to *** ',f7.2//)
  n12 = n12 - i + 1
  jj = nn11
  do i = n11, n12
     jj = jj + 1
     sconst(jj) = sconst(i)
  end do
  nn13 = jj
50007 if (ii .ne. 0) go to 50022
  !     read input card using cimage
55000 call cimage
  ! 3456789012345678901234567890123456789012345678901234567890123456789012
  read (unit = abuff, fmt = 7633) npa, tauo
  if (tauo .gt. deltat) go to 7656
  kill = 197
  flstat(14) = tauo
  lstat(19) = 7656
  go to 9999
7656 ckkjm(ibr) = npa
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 7659) npa, tauo
7659 format ('+A1 begins. Order, tau(sec) =', i6, e14.5)
  if (h1 .eq. 1.0) noutpr = 1
  n11 = 1
7664 n12 = n11 + 2
  if (n12 .gt. npa) n12 = npa
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 7641) (voltbc(ii), ii = n11, n12)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 7644) n11, n12, (voltbc(ii), ii = n11, n12)
  nn11 = nn13 + n11
  nn12 = nn13 + n12
  i = n11
  do jj = nn11, nn12
     sconst(jj) = voltbc(i)
     i = i + 1
  end do
  n11 = n11 + 3
  if (n11 .le. npa) go to 7664
  n11 = 1
7672 n12 = n11 + 2
  if (n12 .gt. npa) n12 = npa
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 7641) (voltbc(ii), ii = n11, n12)
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 7655) n11, n12, (voltbc(ii), ii = n11, n12)
  nn11 = nn12 + n11
  nn13 = nn12 + n12
  i = n11
  do jj = nn11, nn13
     sconst(jj) = voltbc(i)
     i = i + 1
  end do
  n11 = n11 + 3
  if (n11 .le. npa) go to 7672
  if (pdt .eq. 0.) go to 50022
  ii = 2 * npz
  npza = npa
  go to 50021
50022 noutpr = int (aa)
  cnvhst(ifx) = tauo
  ifx = ifx + 14
  if (ifx .gt. lhist) go to 8120
  ifsem = ifsem + 7 * npa + 7 * npz + 8
  if (ifsem .gt. lfsem)  go to 8090
  if (iprsup  .ge. 1) write (unit = lunit(6), fmt = 8434) ifx, npz, npa, ibr, ifsem
8434 format (' Done Marti cards.  ifx, npz, npa, ibr,ifsem =', 5i6)
  icheck = 3
  length(ibr1) = kcount
  if (kcount .gt. mxphas) mxphas = kcount
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6078) itype, n13, model, ipunch
6078 format (' Below 20105. itype, n13, model, ipunch =', 4i6)
  if (ipunch .ne. -2) go to 100
  if (kcount .eq. model) go to 10010
  if (kcount .ne. 1) go to 100
  if (n13 .lt. 1) go to 100
  model = n13
  litype(ibr) = iaddrs
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 26078) model, iaddrs, ibr1
26078 format (" Marti's model for untransposed line, model, iaddrs, ib r1 =", 3i10)
  if (n13 .eq. 1) go to 10010
  go to 100
10105 if (ipunch .ne. -3) go to 6552
6552 if (kcount .gt. 1) go to 5874
  xlong1 = xlong
  if (xlong .gt. 0.0) go to 5877
  if (iline .eq. 2) go to 5877
  kill = 78
  flstat(16) = xlong
  lstat(19) = 5874
  go to 9999
5874 if (xlong .eq. xlong1) go to 5877
  if (xlong .eq. 0.0) go to 5877
  kill = 77
  lstat(19) = 191
  flstat(16) = xlong
  flstat(15) = xlong1
  lstat(14) = kcount
  go to 9999
5877 d17 = h1 + aa + h3 + xlong
  ichtr2 = 0
  if (d17 .ne. 0.0) ichtr2 = 1
  xlong = xlong1
  if (kcount .ne. 3) go to 198
  if (n13 .gt. 0) go to 198
  if (ichtr2 .eq. 0) go to 197
  if (aa .le. 0.0) go to 5878
  if (h3 .gt. 0.0) go to 5879
5878 kill = 93
  lstat(19) = 5878
  flstat(14) = aa
  flstat(15) = h3
  go to 9999
5879 ibr15 = ibr1 + 5
  do i = ibr1, ibr15
     kodebr(i)=-1
  end do
198 if (iline.gt.0) go to 192
  d2 = aa * tenm3
  ci1 = h3 / 1000000.0d0
  if (xopt .gt. 0.0d0) d2 = d2 * 1000. / (twopi * xopt)
  if (copt .gt. 0.0d0) ci1 = ci1 / (twopi * copt)
  a = sqrtz (d2 / ci1)
  h2 = xlong * a * ci1
  go to 193
192 a = aa
  h2 = h3
  if (iline .eq. 1) h2 = xlong / h2
193 cik(ibr) = h2
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 4104) h1, aa, h3, a, h2
4104 format ('+', 5e10.3)
  if (iprsup .ge. 4) write (unit = lunit(6), fmt = 5487) ipunch, kcount, model, n13
5487 format (' Below 4104.  ipunch, kcount, model, n13 =', i5)
  if (h1 .eq. 0. .or. ipunch .gt. 0) a = -a
  ck(ibr) = h1 * xlong
  ci(ibr) = a
  if (ipunch .lt. 0) go to 20160
  icheck = 3
  length(ibr1) = kcount
  if (kcount .gt. mxphas) mxphas = kcount
  if (kcount .eq. model) go to 10010
  if (kcount .ne. 1) go to 100
  if (n13 .eq. 0) model = 0
  if (n13 .le. 1) go to 100
  model = n13
  litype(ibr) = iaddrs
  go to 100
196 kcount = kcount + 1
  if (iprsup .ge. 1) write (unit = 6, fmt = 7693) kcount, itype, model
7693 format (' Ready for [ti]?  kcount, itype, model =', 3i6)
  if (ipsem .ne. 0) go to 2011
  if ((-itype) .ne. kcount) go to 141
2011 if (kcount .gt. 18) go to 9175
  if (iprsup .ge. 4) write (unit = *, fmt = *) ' cable cont.?  ntlin, ibr,  n1, n2 =', ntlin, ibr, n1, n2
  if (iprsup .ge. 4) write (unit = *, fmt = *) ' cable check?      model= , kcount=', model, kcount
  if (-ipunch .ne. 4) go to 5202
  ipunch = imodel(ibr - 1)
  mbus(ibr1) = iabs (mbus(ibr1))
  kbus(ibr) = n1
  mbus(ibr) = n2
  !      if ( n13 .eq. 0 ) n13q = n13 + 1
  !      if (kcount .nq. n13q ) go to 190
  !      if (model.le.1)go to 10004
  if (-itype .le. model) go to 190
  if (ipunch .eq. -4 .and. n13 .eq. 0) go to 190
  !
  !
50050 inoff1 = lbrnch
  inoff2 = 2 * lbrnch
  noutpr = npll
  inoff3 = 3 * lbrnch
  inoff4 = 4 * lbrnch
  koff1 = 900
  koff2 = koff1 +isecti
  koff3  = koff2 +isecti
  koff4  = koff3 +isecti
  koff5  = koff4 +isecti
  koff6  = koff5 +isecti
  koff7  = koff6 +isecti
  koff8  = koff7 +isecti
  koff9  = koff8 +isecti
  koff10 = koff9 +isecti
  koff13 = koff10 + isecti
  koff14 = koff13 + isecti
  koff15 = koff14 + isecti
  koff16 = koff15 + isecti
  koff17 = koff16 + isecti
  koff18 = koff17 + isecti
  koff19 = koff18 + isecti
  koff20 = koff19
  if (kgroup .ne. 1) wk1(koff19) = 1
  wk1(koff19) = wk1(koff19) + kgroup
  ncount = idm
  if (ipsem .ne. 0) n13 = ipsem
  nphs = n13
  nphs2 = n13 ** 2
  nphsu = (nphs + 1) * nphs / 2
  ibr2 = ibr - nphs + 1
  ibr3 = ibr - nphs + 3
  ifq = ifq + 1
  infdli(inoff2 + ibr2) = ifq
  !      infdli( inoff1 + ibr2 ) =
  !    1              idm    !remember intial adress for this group, 1--10
  infdli(inoff1 + ibr2) = iaddrs
  infdli(inoff1 + ibr2) = idm
  !      ncount = iaddrs
  iaddrs = iaddrs + nphs2
  infdli(inoff4 + ibr2) = idu
  if (iprsup .ge. 4) write (unit = *, fmt = *) ' Begin to read qi. ntlin, iq, idm, idu, idt, idq= ', ntlin, iq, idm, idu, idt, idq
  if (ipsem .ne. 0) go to 2003
  do i = 1, nphs2
     wk1(koff20 + ncount) = idq
     call cimage
     read (unit = abuff, fmt = 6366) np, sconst(ifq)
6366 format (i8, e32.0)
     if (noutpr .ne. -7777 .and. noutpr .ne. 1) write (unit = kunit6, fmt = 8262) np, sconst(ifq)
8262 format ('+ti.  np, sconst(ifq) =', i5, e15.5)
     call interp
     wk1(koff20 + ncount) = np
     if (np .ge. 0) go to 1441
1441 j1 = 1
     nticpl = 0
4888 j2 = j1 + 2
     m =  np
     if (j2 .gt. m) j2 = m
     !     read input card using cimage
     call cimage
     read (unit = abuff, fmt = 105) (voltbc(j), j = j1, j2)
105  format (3e26.0)
     if ((noutpr .ne. -7777) .and. (noutpr .ne. 1)) write (unit = kunit6, fmt = 4413) (voltbc(j), j = j1, j2)
4413 format ('+qk-i:', 3e14.5)
     jj1 = j1 + ifq
     jj2 = j2 + ifq
     iml = j1
     do jj = jj1, jj2
        if (abs (sconst(jj)) .ge. 1.e+13) nticpl = nticpl + 1
        sconst(jj) = voltbc(iml)
        iml = iml + 1
     end do
     j1 = j1 + 3
     if (j1 .le. m) go to 4888
     call interp
     intcpl = lhist + i
     cblhst(intcpl) = nticpl
     j1 = 1
4847 j2 = j1 + 2
     m = np
     if (j2 .gt. m) j2 = m
     !     read input card using cimage
     call cimage
     read (unit = abuff, fmt = 105) (voltbc(j), j = j1, j2)
     if (noutpr .ne. -7777 .and. noutpr .ne. 1) write (unit = kunit6, fmt = 8414) (voltbc(j), j = j1, j2)
8414 format ('+qp-i:', 3e14.5)
     nn11 = jj2 + j1
     nn13 = jj2 + j2
     iml = j1
     do jj = nn11, nn13
        sconst(jj) = voltbc(iml)
        iml = iml + 1
     end do
     j1 = j1 + 3
     if (j1 .le. m) go to 4847
     ifq = nn13 + 1
     call interp
     ncount = ncount + 1
     idq = idq + np
  end do
  idq = idq - 1
  idq = 2 * idq
  idq = idq + 1
  ifq = nn13 + (nn13 - ifsem - nphs2) * 3 / 2 + 2 * nphs2
  ifsem = ifq
  infdli(inoff3 + ibr2) = nn13
  idm = idm + nphs2
  idu = idu + nphsu
  go to 2009
2003 jkl = 0
  if (ipsem .lt. 3) jkl = 1
  do j = 1, nphs2
     wk1(koff20 + ncount) = 1
     ncount = ncount + 1
  end do
  if (ipsem .le. 3) ip = ipsem *2
  if (ipsem .gt. 3 .and. ipsem .lt. 6) ip = ipsem*4
  if (ipsem .eq. 6) ip = ipsem * 4
  kl = 0
  ifqt = ifq
  ifqq = ifq
  do i = 1, ip
     call cimage
     if (ipsem .le. 3) go to 2113
     if (i .le. ipsem * 2) cycle
     go to 2114
2113 if (i .le. ipsem) cycle
2114 if (ipsem .eq. 6) nrowt = 6
     if (ipsem .eq. 6) go to 2014
     if (ipsem .gt. 3 .and. jkl .eq. 0) nrowt = 6
     if (ipsem .le. 3) nrowt = 2 * ipsem
     if (ipsem .le. 3) go to 2014
     if (jkl .eq. 1) nrowt = ipsem * 2 - 6
     if (nrowt .eq. 6) jkl = 1
     if (nrowt .lt. 6) jkl = 0
     if (ipsem .gt. 3 .and. ipsem .eq. 6) nrowt = 6
2014 nfrfld = nrowt
     if (kolbeg .le. 0) go to 2005
     !     call frefld (voltbc)
     call free (voltbc)
     go to 2006
2005 continue
     read (unit = abuff, fmt = 8170) (voltbc(ik), ik = 1, nfrfld)
2006 n12 = 1
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 2020) (voltbc(ii), ii = 1,nfrfld)
2020 format (' Ti - Semlyen.', 3e12.3)
     do k = 1, nrowt, 2
        kl = kl + 1
        sconst(ifq) = voltbc(n12)
        sconst(ifq + 1) = 0.0d0
        sconst(ifq + 2) = 1.0d0
        n12 = n12 + 2
        if (kl .lt. ipsem) go to 2015
        kl = 0
        ifqq = ifqq + 3
        ifq = ifqq
        cycle
2015    ifq = ifq + ipsem * 3
     end do
  end do
  ifq = ifqt + ipsem ** 2 * 3
  nn13 = ifq - 1
  ifq = nn13 + (nn13 - ifsem - nphs2 ) * 3 / 2 + 2 * nphs2
  ifsem = ifq
  infdli(inoff3 + ibr2) = nn13
  idm = idm + nphs2
  idu = idu + nphsu
2009 if (-itype .eq. nphs .or. nt1 .eq. ipsem) icheck = 1
  if (-itype .eq. nphs .or. nt1 .eq. ipsem) ifkc = ifkc + n13
  if (iprsup .ge. 4) write (unit = *, fmt = *) 'next bunch of coupled branches. ntlin=, kcount=', ntlin, kcount
  itranm = model
  model = 0
  icheck = 1
  kgroup = 1
  noutpr = int (aa)
  go to 100
  !
5202 length(ibr1) = kcount
  if (kcount .gt. mxphas) mxphas = kcount
  mbus(ibr1) = iabs (mbus(ibr1))
  kbus(ibr)=n1
  mbus(ibr)=n2
  if (ipunch .eq. -2 .and. n13 .eq. 0) go to 190
  if (kcount .le. 3) go to 190
  if (model .le. 1) go to 10004
  if (kcount .le. model) go to 190
10010 iadd = iaddrs + 2 * model * model - 1
  !   constant and frequence-dependent modal, 2xnpsxnps cells sre required
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 27693)
27693 format (' at s.n. 10010, ready to read [ti]')
  if (iadd .lt. lfd) go to 10007
  ! check for overflow for ti array
  iprint = 21
  lstat(19) = 10010
  go to 9000
9175 write (unit = lunit(6), fmt = 9176)
9176 format (' Last distributed param. line has more than 18 phases')
  go to 9000
  !     next read in complex transformation matrix  [tr] + j[ti] :
10007 n9 = 1
2711 n7 = 1
2716 n1 = n9 + iaddrs - 1
  i = 1
  !     read input card using cimage
2722 call cimage
  nfrfld = 6
  if (i + 5 .gt. model) nfrfld = model - i + 1
  if (kolbeg .gt. 0) go to 2728
  read (unit = abuff, fmt = 2721) (voltbc(j), j = 1, nfrfld)
2721 format (6e12.0)
  go to 2733
2728 call free (voltbc)
2733 if (noutpr .eq. 0) write (unit = kunit6, fmt = 2736) textmx(n7), n9, i, (voltbc(j), j = 1, nfrfld)
2736 format (a4, i2, ',', i2, ')...', 6f6.3)
  do n8 = 1, nfrfld
     if (n7 .eq. 1) qfd(n1) = voltbc(n8)
     if (n7 .eq. 2) sfd(n1) = voltbc(n8)
     i = i + 1
     n1 = n1 + model
  end do
  if (i .le. model) go to 2722
  n7 = n7 + 1
  if (n7 .le. 2) go to 2716
  n9 = n9 + 1
  if (n9 .le. model) go to 2711
  if (iprsup .lt. 2) go to 10005
  n9 = itranm * itranm
  write (unit = lunit(6), fmt = 10003) n13, model, iadd, n9, (qfd(i), i = iaddrs, iadd)
10003 format (' For this untransposed line, n13, model, iadd, n9, (qfd(i), i = (iadd - n9), iadd) =', i7, /, (1x, 6e15.6))
  !     define new address for following untransposed line
10005 iaddrs = iadd + 1
  itranm = model
  model = 0
  icheck = 1
  go to 100
10004 cik(ibr) = cik(ibr1 + 1)
  imodel(ibr) = imodel(ibr1 + 1)
  if (imodel(ibr) .ne. -2) go to 10106
  ktrlsw(5) = 1
  go to 100
10106 continue
197 if (ichtr2 .ne. 0 .and. kcount .eq. 4) go to 5891
  ibrm = ibr1 + 1
  if (kodebr(ibr1) .eq. -1) ibrm = ibr1 + 2
  cik(ibr) = cik(ibrm)
  ci(ibr) = ci(ibrm)
  ck(ibr) = ck(ibrm)
  nr(ibr) = nr(ibrm)
  go to 5892
5891 ibrm = ibr - 1
  ibrm1 = ibr - 2
  if (kodebr(ibr1) .ne. -1) go to 15891
  d1 = cik(ibrm)
  d2 = ci(ibrm)
  d3 = ck(ibrm)
  d4 = nr(ibrm)
15891 cik(ibrm) = cik(ibrm1)
  ci(ibrm) = ci(ibrm1)
  ck(ibrm) = ck(ibrm1)
  nr(ibrm) = nr(ibrm1)
  if (kodebr(ibr1) .ne. -1) go to 15895
  cik(ibrm1) = d1
  ci(ibrm1) = d2
  ck(ibrm1) = d3
  nr(ibrm1) = int (d4)
15895 cik(ibr) = cik(ibrm)
  ci(ibr) = ci(ibrm)
  ck(ibr) = ck(ibrm)
  nr(ibr) = nr(ibrm)
5892 if (kodebr(ibr1) .eq. -1) go to 5895
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 54121)
54121 format ('+3rd or later unif.-transposed distributed cond.')
  go to 100
5895 if (noutpr .eq. 0) write (unit = kunit6, fmt = 5900)
5900 format ('+4th or later double ckt distributed conductor.')
100 lstat(18) = 100
  go to 9999
141 lstat(18) = 141
  go to 9999
20160 nchain = 4
  go to 9999
9000 lstat(16) = iprint
  kill = 1
9999 if (iprsup .ge. 3) write (unit = lunit(6), fmt = 9998) kill, nchain, lstat(18), ibr, ifsem
9998 format (/, ' Exit  "distr2" .    kill  nchain lstat18     ibr   ifsem ', /, 17x, 10i8)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module distr2."')
  return
end subroutine distr2

!
! subroutine over3.
!

subroutine over3
  use blkcom
  use labcom
  use tracom
  use movcop
  use redcom
  implicit none
  !  dimension trser(1),txser(1)
  !  dimension mapcas(1), mapinv(1), node1(1), node2(1)
  !  dimension caslnr(1), caslnx(1)
  !  dimension cser(1)
  !  dimension trshun(1), txshun(1), cshun(1)
  !
  !  equivalence (trser(1), e(1))
  !  equivalence (txser(1), f(1))
  !  equivalence (volti(1), node1(1))
  !  equivalence (voltk(1), node2(1))
  !  equivalence (volt(1), mapinv(1))
  !  equivalence (mapcas(1), ykm(1))
  !  equivalence (caslnr(1), xk(1))
  !  equivalence (caslnx(1), xm(1))
  !  equivalence (cser(1), kode(1))
  !  equivalence (trshun(1), kk(1))
  !  equivalence (txshun(1), kks(1))
  !  equivalence (cshun(1), kknonl(1))
  !  equivalence (iprsov(35), ipoint)
  !  equivalence (iprsov(36), locz11)
  character(8) :: text1, text2, text3, text4, text5, text6, text7
  integer(4) :: i, icas, idumy, idumy2, iend, iendd, ii, iph, iphase, istart
  integer(4) :: j, jbr
  integer(4) :: ll0, ll3, locznn
  integer(4) :: mbr, mbr1, msect, mser, multip
  integer(4) :: nnpos, npos, npos2, npos3, nredct, nrow1, nrow2
  real(8) :: dsectj
  real(8) :: freqc, freqx
  real(8) :: ymag2, yserr, yserx, yshunr, yshunx
  !
  integer(4), pointer :: ipoint => iprsov(35)
  integer(4), pointer :: locz11 => iprsov(36)
  integer(4), allocatable :: mapcas(:)
  integer(4), allocatable :: mapinv(:)
  integer(4), allocatable :: node1(:)
  integer(4), allocatable :: node2(:)
  real(8), pointer :: caslnr(:)
  real(8), pointer :: caslnx(:)
  real(8), allocatable :: cser(:)
  real(8), allocatable :: cshun(:)
  real(8), pointer :: trser(:)
  real(8), allocatable :: trshun(:)
  real(8), pointer :: txser(:)
  real(8), allocatable :: txshun(:)
  !
  data text1  / 'stop' /
  data text2  / ' cas' /
  data text3  / 'cade' /
  data text4  / '    ' /
  !
  caslnr => xk
  caslnx => xm
  trser => e
  txser => f
  ll0 = size (transfer (kode, cser))
  allocate (cser(ll0))
  cser = transfer (kode, cser)
  ll0 = size (transfer (ykm, mapcas))
  allocate (mapcas(ll0))
  mapcas = transfer (ykm, mapcas)
  ll0 = size (transfer (volt, mapinv))
  allocate (mapinv(ll0))
  mapinv = transfer (volt, mapinv)
  ll0 = size (transfer (volti, node1))
  allocate (node1(ll0))
  node1 = transfer (volti, node1)
  ll0 = size (transfer (voltk, node2))
  allocate (node2(ll0))
  node2 = transfer (voltk, node1)
  ll0 = size (transfer (kknonl, cshun))
  allocate (cshun(ll0))
  cshun = transfer (kknonl, cshun)
  ll0 = size (transfer (kk, trshun))
  allocate (trshun(ll0))
  trshun = transfer (kk, trshun)
  ll0 = size (transfer (kks, txshun))
  allocate (txshun(ll0))
  txshun = transfer (kks, txshun)
  locatn(i, j) = (j * j - j) / 2 + i
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module over3."')
  ll0 = 0
  ll3 = 3
  iend = nphcas
  idumy = nphcas
  !     read input card using cimage.
  call cimage
  if (iend .gt. 14) idumy = 14
  read (unit = abuff, fmt = 76501) dsectj, multip, mser, mbr, msect, (mapcas(i), i = 1, idumy)
76501 format (2x, e6.2, 18i4)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 76702) dsectj, multip, mser, mbr, msect, (mapcas(i), i = 1, ll3)
76702 format ('+line pos.', 1x, e11.3, 7i4)
  if (dsectj .eq. 0.0) dsectj = 1.0
  if (multip .eq. 0) multip = 1
  if (multip .gt. 0) go to 7671
  kill = 55
  lstat(19) = 7671
  lstat(12) = multip
  go to 9200
7671 istart = 15
76504 iend = iend - 14
  if (iend .le. 0) go to 76503
  !     read input card using cimage.
  call cimage
  idumy = istart + iend - 1
  if (iend .gt. 14) idumy = istart + 13
  read (unit = abuff, fmt = 76502) (mapcas(i), i = istart, idumy)
76502 format (24x, 14i4)
  if (idumy .gt. istart + 11) idumy = istart + 11
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 76703) (mapcas(i), i = istart, idumy)
76703 format ('+', 12i4)
  istart = istart + 14
  go to 76504
76503 npos2 = 2 * nphcas
  npos3 = 3 * nphcas
  freqcs = twopi * freqcs
  locznn = locz11 + locatn(nphcas, nphcas) - 1
  freqx = xopt * twopi
  freqc = copt * twopi * 1000000.0d0
  if (xopt .eq. 0.0d0) freqx = 1000.0d0
  if (copt .eq. 0.0d0) freqc = 1000000.0d0
  do i = locz11, locznn
     tx(i) = freqcs * tx(i) / freqx
     c(i) = freqcs * c(i) / (2.0 * freqc)
  end do
  call cxred (tr(locz11 :), tx(locz11 :), nphcas, ll0)
  do i = locz11, locznn
     tr(i) = -tr(i)
     tx(i) = -tx(i)
  end do
  iend = locatn(npos3, npos3)
  call move0 (caslnr(1 :), iend)
  call move0 (caslnx(1 :), iend)
  do i = 1, nphcas
     do j = i, nphcas
        if (mapcas(i) .le. nphcas) go to 76711
        kill = 54
        lstat(19) = 7655
        lstat(12) = mapcas(i)
        lstat(13) = nphcas
        go to 9200
76711   npos = locatn(mapcas(i), mapcas(j)) + locz11 - 1
        if (mapcas(i) .gt. mapcas(j)) npos = locatn(mapcas(j), mapcas(i)) + locz11 - 1
        nnpos = locatn(i, j)
        caslnr(nnpos) = tr(npos) / dsectj
        caslnx(nnpos) = tx(npos) / dsectj + c(npos) * dsectj
        nnpos = locatn(i, j + nphcas)
        caslnr(nnpos) = -tr(npos) / dsectj
        caslnx(nnpos) = -tx(npos) / dsectj
        if (i .eq. j) go to 76433
        nnpos = locatn(j, i + nphcas)
        caslnr(nnpos) = -tr(npos) / dsectj
        caslnx(nnpos) = -tx(npos) / dsectj
76433   nnpos = locatn(i + nphcas, j + nphcas)
        caslnr(nnpos) = tr(npos) / dsectj
        caslnx(nnpos) = tx(npos) / dsectj + c(npos) * dsectj
     end do
  end do
76521 multip = multip - 1
  if (multip .gt. 0) go to 76505
  iend = nphcas
  idumy = nphcas
  !     read input card using cimage.
  call cimage
  if (iend .gt. 14) idumy = 14
  read (unit = abuff, fmt = 4012) text5, text6, text7
4012 format (2x, 3a4)
  if (text5 .ne. text1) go to 4013
  if (text6 .ne. text2) go to 4013
  if (text7 .eq. text3) go to 76600
4013 continue
  read (unit = abuff, fmt = 76501) dsectj, multip, mser, mbr, msect, (mapcas(i), i = 1, idumy)
  if (noutpr .eq.  0) write (unit = kunit6, fmt = 76702)  dsectj, multip, mser, mbr, msect, (mapcas(i), i = 1, ll3)
  if (dsectj .eq. 0.0) dsectj = 1.0
  if (multip .eq. 0) multip = 1
  if (multip .gt. 0) go to 76713
  lstat(19) = 4013
  kill = 55
  lstat(12) = multip
  go to 9200
76713 istart = 15
76506 iend = iend - 14
  if (iend .le. 0) go to 76507
  !     read input card using cimage.
  call cimage
  idumy = istart + iend - 1
  if (iend .gt. 14) idumy = istart + 13
  read (unit = abuff, fmt = 76502) (mapcas(i), i = istart, idumy)
  if (idumy .gt. istart + 12) idumy = istart + 12
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 76703) (mapcas(i), i = istart, idumy)
  go to 76506
76507 do i = 1, nphcas
     idumy = mapcas(i)
     mapinv(idumy) = i
  end do
  if (mser .le. 0) go to 76510
  call move0 (trser(1 :), nphcas)
  call move0 (txser(1 :), nphcas)
  call move0 (cser(1 :), nphcas)
  !     read input card using cimage.
76511 call cimage
  read (unit = abuff, fmt = 7651) itype
7651 format (i2)
  if (itype .ge. 0 .and. itype .le. nphcas) go to 76714
  lstat(19) = 7651
  kill = 54
  lstat(12) = itype
  go to 9200
76714 if (itype .eq. 0 .and. noutpr .eq. 0) write (unit = kunit6, fmt = 76705)
76705 format ('+Blank card terminating series R-L-C.')
  if (itype .eq. 0) go to 76510
  read (unit = abuff, fmt = 76530) trser(itype), txser(itype), cser(itype)
76530 format (26x, 3e6.0)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 76704) trser(itype), txser(itype), cser(itype)
76704 format ('+series R-L-C.', 2x, 3e11.3)
  go to 76511
76510 if (mbr .le. 0) go to 76513
  mbr1 = 0
76516 mbr1 = mbr1 + 1
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 4012) text5, text6, text7
  if (text5 .ne. text4) go to 4018
  if (text6 .ne. text4) go to 4018
  if (text7 .ne. text4) go to 4018
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 4017)
4017 format ('+Blank card terminating shunt R-L-C branches.')
  go to 76540
4018 continue
  read (unit = abuff, fmt = 76515) node1(mbr1), node2(mbr1), trshun(mbr1), txshun(mbr1), cshun(mbr1)
76515 format (2x, 2i6, 12x, 3e6.2)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 76706) node1(mbr1), node2(mbr1), trshun(mbr1), txshun(mbr1), cshun(mbr1)
76706 format ('+Shunt.', 2x, 2i4, 3e11.3)
  go to 76516
76540 mbr1 = mbr1 - 1
76513 if (msect .le. 0) go to 76505
  istart = locz11
  do i = 1, nphcas
     idumy = i
     if (idumy .gt. 3) idumy=3
     iend = istart + idumy - 1
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 76518) itype, (tr(j), tx(j), c(j), j = istart, iend)
76518 format (i2, 24x, 9e6.2)
     if (i .gt. 1) go to 5264
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 54777) tr(istart), tx(istart), c(istart)
54777 format ('+1st of pi-ckt.', 1x, 3e11.3)
     go to 5271
5264 idumy = istart + 1
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 54104) tr(idumy), tx(idumy), c(idumy), tr(idumy + 1), tx(idumy + 1)
54104 format ('+', 5e10.3)
5271 istart = iend + 1
     idumy = i
76520 idumy = idumy - 3
     if (idumy .le. 0) cycle
     iend = istart + idumy - 1
     if (idumy .gt. 3) iend = istart + 2
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 76519) (tr(j), tx(j), c(j), j = istart, iend)
76519 format (26x, 9e6.2)
     iendd = iend
     if (idumy .gt. 3) iendd = istart + 1
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 54104) tr(istart), tx(istart), c(istart), tr(istart + 1), tx(istart + 1)
     istart = iend + 1
     go to 76520
  end do
  do i = locz11, locznn
     tx(i) = freqcs * tx(i) / freqx
     c(i) = freqcs * c(i) / (2.0 * freqc)
  end do
  call cxred (tr(locz11 :), tx(locz11 :), nphcas, ll0)
  do i = locz11, locznn
     tr(i) = -tr(i)
     tx(i) = -tx(i)
  end do
76505 if (mser .eq. 0) go to 6434
  do i = 1, nphcas
     do j = 1, nphcas
        npos = locatn(i, j + nphcas)
        nnpos = locatn(i, j + npos2)
        caslnr(nnpos) = caslnr(npos)
        caslnr(npos) = 0.0
        caslnx(nnpos) = caslnx(npos)
        caslnx(npos) = 0.0
     end do
  end do
  do i = 1, nphcas
     do j = i, nphcas
        npos = locatn(i + nphcas, j + nphcas)
        nnpos = locatn(i + npos2, j + npos2)
        caslnr(nnpos) = caslnr(npos)
        caslnr(npos) = 0.0
        caslnx(nnpos) = caslnx(npos)
        caslnx(npos) = 0.0
     end do
  end do
  nredct = nphcas
  do iph = 1, nphcas
     iphase = mapcas(iph)
     if ((trser(iphase) .eq. 0.0d0) .and. (txser(iphase) .eq. 0.0d0) .and. (cser(iphase) .eq. 0.0d0)) go to 76438
     if (trser(iphase) .eq. 999999.0d0) go to 76441
     if (cser(iphase) .eq. 0.0d0) go to 76439
     ymag2 = trser(iphase) ** 2 + (freqcs * txser(iphase) / freqx - 1.0d0 / (freqcs * cser(iphase) / freqc)) ** 2
     yserr = trser(iphase) / ymag2
     yserx = (-freqcs * txser(iphase) / freqx + 1.0d0 / (freqcs * cser(iphase) / freqc)) / ymag2
     go to 76440
76439 yserr = trser(iphase) / (trser(iphase) ** 2 + (freqcs * txser(iphase) / freqx) ** 2)
     yserx = (-freqcs * txser(iphase) / freqx) / (trser(iphase) ** 2 + (freqcs * txser(iphase) / freqx) ** 2)
     go to 76440
76441 yserr = 0.0d0
     yserx = 0.0d0
76440 nnpos = locatn(iph + nphcas, iph + nphcas)
     caslnr(nnpos) = yserr
     caslnx(nnpos) = yserx
     nnpos = locatn(iph + nphcas, iph + nphcas + nredct)
     caslnr(nnpos) = -yserr
     caslnx(nnpos) = -yserx
     nnpos = locatn(iph + nphcas + nredct, iph + nphcas + nredct)
     caslnr(nnpos) = caslnr(nnpos) + yserr
     caslnx(nnpos) = caslnx(nnpos) + yserx
     cycle
76438 idumy = nphcas + iph
     do i = 1, idumy
        nnpos = locatn(i, idumy)
        npos = locatn(i, idumy + nredct)
        caslnr(nnpos) = caslnr(npos)
        caslnx(nnpos) = caslnx(npos)
     end do
     nnpos = locatn(idumy, idumy)
     npos = locatn(idumy + nredct, idumy + nredct)
     caslnr(nnpos) = caslnr(npos)
     caslnx(nnpos) = caslnx(npos)
     iend = npos2 + nredct
     istart = idumy + nredct
     do i = istart, iend
        nnpos = locatn(idumy, i)
        npos = locatn(idumy + nredct, i)
        caslnr(nnpos) = caslnr(npos)
        caslnx(nnpos) = caslnx(npos)
     end do
     istart = npos2 + 1
     iend = idumy + nredct
     do i = istart, iend
        nnpos = locatn(idumy, i)
        npos = locatn(i, idumy + nredct)
        caslnr(nnpos) = caslnr(npos)
        caslnx(nnpos) = caslnx(npos)
     end do
     istart = idumy + nredct + 1
     iend = npos2 + nredct
     do i = istart, iend
        do j = i, iend
           npos = locatn(i, j)
           nnpos = npos - 1
           caslnr(nnpos) = caslnr(npos)
           caslnx(nnpos) = caslnx(npos)
        end do
     end do
     do j = istart, iend
        iendd = j - 1
        do i = 1, iendd
           nnpos = locatn(i, iendd)
           npos = locatn(i, j)
           caslnr(nnpos) = caslnr(npos)
           caslnx(nnpos) = caslnx(npos)
        end do
     end do
     nredct = nredct - 1
  end do
  if (iprsup .lt. 9) go to 76630
  write (unit = lunit(6), fmt = 76605)
  write (unit = lunit(6), fmt = 76631)
76631 format (5x, 'Y-matrix before series R-L-C elimination.')
  do i = 1, npos3
     write (unit = lunit(6), fmt = 76605)
     istart = locatn(i - 1, i - 1) + 1
     idumy = i + 9
76633 idumy = idumy - 9
     iend = idumy + istart - 1
     if (idumy .gt. 9) iend = istart + 8
     write (unit = lunit(6), fmt = 76604) (caslnr(ii), ii = istart, iend)
     write (unit = lunit(6), fmt = 76604) (caslnx(ii), ii = istart, iend)
     istart = istart + 9
     if (idumy .gt. 9) go to 76633
  end do
76630 continue
  call cxred (caslnr, caslnx, npos2 + nredct, npos2)
  if (iprsup .lt. 9) go to 76650
  write (unit = lunit(6), fmt = 76605)
  write (unit = lunit(6), fmt = 76653)
76653 format (5x, 'Y-matrix after series R-L-C elimination.')
  do i = 1, npos2
     write (unit = lunit(6), fmt = 76605)
     istart = locatn (i - 1, i - 1) + 1
     idumy = i + 9
76659 idumy = idumy - 9
     iend = idumy + istart - 1
     if (idumy .gt. 9) iend = istart + 8
     write (unit = lunit(6), fmt = 76604) (caslnr(ii), ii = istart, iend)
     write (unit = lunit(6), fmt = 76604) (caslnx(ii), ii = istart, iend)
     istart = istart + 9
     if (idumy .gt. 9) go to 76659
  end do
76650 continue
  istart = locatn (npos2, npos2) + 1
  iend = locatn (npos3, npos3)
  i = iend - istart + 1
  call move0 (caslnr(istart :), i)
  call move0 (caslnx(istart :), i)
6434 if (mbr .eq. 0) go to 76450
  do jbr = 1, mbr1
     if (node1(jbr) .ne. node2(jbr)) go to 6715
     lstat(19) = 6434
     kill = 56
     lstat(12) = node1(jbr)
     lstat(13) = mbr1
     lstat(14) = jbr
     go to 9200
6715 if (iabs (node1(jbr)) .le. nphcas) go to 6716
     lstat(19) = 6715
     kill = 54
     lstat(12) = node1(jbr)
     go to 9200
6716 if (iabs (node2(jbr)) .le. nphcas) go to 76717
     lstat(19) = 6716
     kill = 54
     lstat(12) = node2(jbr)
     go to 9200
76717 if (trshun(jbr) .ne. 0.0 .or. txshun(jbr) .ne. 0.0 .or. cshun(jbr) .ne. 0.0) go to 6718
     lstat(19) = 6718
     kill = 57
     lstat(12) = node1(jbr)
     lstat(13) = node2(jbr)
     go to 9200
6718 if (cshun(jbr) .eq. 0.0) go to 76452
     ymag2 = trshun(jbr) ** 2 + (freqcs * txshun(jbr) / freqx - 1.0 / (cshun(jbr) * freqcs / freqc)) ** 2
     yshunr = trshun(jbr) / ymag2
     yshunx = (-freqcs * txshun(jbr) / freqx + 1.0 / (freqcs * cshun(jbr) / freqc)) / ymag2
     if (iprsup .ge. 6) write (unit = lunit(6), fmt = 76721) jbr, mbr1, node1(jbr), node2(jbr), trshun(jbr), txshun(jbr), cshun(jbr), freqx, freqc, freqcs, ymag2, yshunr, yshunx
76721 format (/, ' Shunt branch admittance calculation at 76721.', 4i10, /, (1x, 3e30.8))
     go to 76453
76452 ymag2 = trshun(jbr) ** 2 + (freqcs * txshun(jbr) / freqx) ** 2
     yshunr = trshun(jbr) / ymag2
     yshunx = (-freqcs * txshun(jbr) / freqx) / ymag2
     if (iprsup .ge. 6) write (unit = lunit(6), fmt = 76721) jbr, mbr1, node1(jbr), node2(jbr), trshun(jbr), txshun(jbr), cshun(jbr), freqx, freqc, freqcs, ymag2, yshunr, yshunx
76453 if (node1(jbr) .ge. 0 .and. node2(jbr) .ge. 0) go to 76454
     if (node1(jbr) .le. 0 .and. node2(jbr) .le. 0) go to 76455
     if (node1(jbr) .gt. 0) go to 76456
     idumy = node1(jbr)
     node1(jbr) = node2(jbr)
     node2(jbr) = idumy
76456 node2(jbr) = -node2(jbr)
     idumy = node1(jbr)
     nrow1 = nphcas + mapinv(idumy)
     nrow2 = npos2 + node2(jbr)
     nnpos = locatn (nrow1, nrow1)
     caslnr(nnpos) = caslnr(nnpos) + yshunr
     caslnx(nnpos) = caslnx(nnpos) + yshunx
     nnpos = locatn (nrow1, nrow2)
     caslnr(nnpos) = -yshunr
     caslnx(nnpos) = -yshunx
     nnpos = locatn(nrow2, nrow2)
     caslnr(nnpos) = caslnr(nnpos) + yshunr
     caslnx(nnpos) = caslnx(nnpos) + yshunx
     node2(jbr) = -node2(jbr)
     cycle
76454 if (node1(jbr) .eq. 0 .or. node2(jbr) .eq. 0) go to 76457
     idumy = node1(jbr)
     idumy2 = node2(jbr)
     if (mapinv(idumy) .lt. mapinv(idumy2)) go to 76458
     idumy = node2(jbr)
     idumy2 = node1(jbr)
76458 nrow1 = nphcas + mapinv(idumy)
     nrow2 = nphcas + mapinv(idumy2)
     nnpos = locatn (nrow1, nrow1)
     caslnr(nnpos) = caslnr(nnpos) + yshunr
     caslnx(nnpos) = caslnx(nnpos) + yshunx
     nnpos = locatn (nrow1, nrow2)
     caslnr(nnpos) = caslnr(nnpos) - yshunr
     caslnx(nnpos) = caslnx(nnpos) - yshunx
     nnpos = locatn (nrow2, nrow2)
     caslnr(nnpos) = caslnr(nnpos) + yshunr
     caslnx(nnpos) = caslnx(nnpos) + yshunx
     cycle
76457 idumy = node1(jbr) + node2(jbr)
     nrow1 = nphcas + mapinv(idumy)
     nnpos = locatn(nrow1, nrow1)
     caslnr(nnpos) = caslnr(nnpos) + yshunr
     caslnx(nnpos) = caslnx(nnpos) + yshunx
     cycle
76455 node1(jbr) = -node1(jbr)
     node2(jbr) = -node2(jbr)
     if (node1(jbr) .eq. 0 .or. node2(jbr) .eq. 0) go to 76460
     if (node1(jbr) .lt. node2(jbr)) go to 76459
     idumy = node1(jbr)
     node1(jbr) = node2(jbr)
     node2(jbr) = idumy
76459 nrow1 = npos2 + node1(jbr)
     nrow2 = npos2 + node2(jbr)
     nnpos = locatn(nrow1, nrow1)
     caslnr(nnpos) = caslnr(nnpos) + yshunr
     caslnx(nnpos) = caslnx(nnpos) + yshunx
     nnpos = locatn (nrow1, nrow2)
     caslnr(nnpos) = caslnr(nnpos) - yshunr
     caslnx(nnpos) = caslnx(nnpos) - yshunx
     nnpos = locatn (nrow2, nrow2)
     caslnr(nnpos) = caslnr(nnpos) + yshunr
     caslnx(nnpos) = caslnx(nnpos) + yshunx
     node1(jbr) = -node1(jbr)
     node2(jbr) = -node2(jbr)
     cycle
76460 nrow1 = npos2 + node1(jbr) + node2(jbr)
     nnpos = locatn (nrow1, nrow1)
     caslnr(nnpos) = caslnr(nnpos) + yshunr
     caslnx(nnpos) = caslnx(nnpos) + yshunx
     node1(jbr) = -node1(jbr)
     node2(jbr) = -node2(jbr)
  end do
  nredct = 0
  do i = 1, nphcas
     npos = locatn (npos2 + i - 1, npos2 + i - 1)
     nnpos = locatn (npos2 + i, npos2 + i)
     ymag2 = absz (caslnr(nnpos)) + absz (caslnx(nnpos))
     if (ymag2 .eq. 0.0d0) cycle
     ymag2 = absz (caslnr(npos)) + absz (caslnx(npos))
     if (ymag2 .ne. 0.0d0) go to 6470
     lstat(19) = 6470
     kill = 60
     lstat(12) = -i
     go to 9200
6470 nredct = nredct + 1
  end do
  if (iprsup .lt. 9) go to 76640
  write (unit = lunit(6), fmt = 76605)
  write (unit = lunit(6), fmt = 76634)
76634 format (5x, 'Y-matrix before shunt R-L-C elimation.')
  do i = 1, npos3
     write (unit = lunit(6), fmt = 76605)
     istart = locatn (i - 1, i - 1) + 1
     idumy = i + 9
76638 idumy = idumy - 9
     iend = idumy + istart - 1
     if (idumy .gt. 9) iend = istart + 8
     write (unit = lunit(6), fmt = 76604) (caslnr(ii), ii = istart, iend)
     write (unit = lunit(6), fmt = 76604) (caslnx(ii), ii = istart, iend)
     istart = istart + 9
     if (idumy .gt. 9) go to 76638
  end do
76640 continue
  call cxred (caslnr, caslnx, npos2 + nredct, npos2)
  if (iprsup .lt. 9) go to 76651
  write (unit = lunit(6), fmt = 76605)
  write (unit = lunit(6), fmt = 76654)
76654 format (5x, 'Y-matrix after shunt R-L-C elimation.')
  do i = 1, npos2
     write (unit = lunit(6), fmt = 76605)
     istart = locatn (i - 1, i - 1) + 1
     idumy = i + 9
76660 idumy = idumy - 9
     iend = idumy + istart - 1
     if (idumy .gt. 9) iend = istart + 8
     write (unit = lunit(6), fmt = 76604) (caslnr(ii), ii = istart, iend)
     write (unit = lunit(6), fmt = 76604) (caslnx(ii), ii = istart, iend)
     istart = istart + 9
     if (idumy .gt. 9) go to 76660
  end do
76651 continue
  istart = locatn(npos2, npos2) + 1
  iend = locatn(npos3, npos3)
  i = iend - istart + 1
  call move0 (caslnr(istart :), i)
  call move0 (caslnx(istart :), i)
76450 do i = 1, nphcas
     do j = 1, nphcas
        npos = locatn (i, j + nphcas)
        nnpos = locatn (i, j + npos2)
        caslnr(nnpos) = caslnr(npos)
        caslnr(npos) = 0.0d0
        caslnx(nnpos) = caslnx(npos)
        caslnx(npos) = 0.0d0
     end do
  end do
  do i = 1, nphcas
     do j = i, nphcas
        npos = locatn (i + nphcas, j + nphcas)
        nnpos = locatn (i + npos2, j + npos2)
        caslnr(nnpos) = caslnr(npos)
        caslnr(npos) = 0.0d0
        caslnx(nnpos) = caslnx(npos)
        caslnx(npos) = 0.0d0
     end do
  end do
  do i = 1, nphcas
     do j = i, nphcas
        npos = locatn (mapcas(i), mapcas(j)) + locz11 - 1
        if (mapcas(i) .gt. mapcas(j)) npos = locatn (mapcas(j), mapcas(i)) + locz11 - 1
        nnpos=locatn (i + nphcas, j + nphcas)
        caslnr(nnpos) = tr(npos) / dsectj
        caslnx(nnpos) = tx(npos) / dsectj + c(npos) * dsectj
        nnpos = locatn (i + nphcas, j + npos2)
        caslnr(nnpos) = -tr(npos) / dsectj
        caslnx(nnpos) = -tx(npos) / dsectj
        if (i .eq. j) go to 76466
        nnpos = locatn(j + nphcas, i + npos2)
        caslnr(nnpos) = -tr(npos) / dsectj
        caslnx(nnpos) = -tx(npos) / dsectj
76466   nnpos = locatn (i + npos2, j + npos2)
        caslnr(nnpos) = tr(npos) / dsectj + caslnr(nnpos)
        caslnx(nnpos) = tx(npos) / dsectj + c(npos) * dsectj + caslnx(nnpos)
     end do
  end do
  if (iprsup .lt. 9) go to 76641
  write (unit = lunit(6), fmt = 76605)
  write (unit = lunit(6), fmt = 76635)
76635 format (5x, 'Y-matrix before section elimination.')
  do i = 1, npos3
     write (unit = lunit(6), fmt = 76605)
     istart = locatn (i - 1, i - 1) + 1
     idumy = i + 9
76639 idumy = idumy - 9
     iend = idumy + istart - 1
     if (idumy .gt. 9) iend = istart + 8
     write (unit = lunit(6), fmt = 76604) (caslnr(ii), ii = istart, iend)
     write (unit = lunit(6), fmt = 76604) (caslnx(ii), ii = istart, iend)
     istart = istart + 9
     if (idumy .gt. 9) go to 76639
  end do
76641 continue
  call cxred (caslnr, caslnx, npos3, npos2)
  if (iprsup .lt. 9) go to 76652
  write (unit = lunit(6), fmt = 76605)
  write (unit = lunit(6), fmt = 76655)
76655 format (5x, 'Y-matrix after section elimination.')
  do i = 1, npos2
     write (unit = lunit(6), fmt = 76605)
     istart = locatn (i - 1, i - 1) + 1
     idumy = i + 9
76661 idumy = idumy - 9
     iend = idumy + istart - 1
     if (idumy .gt. 9) iend = istart + 8
     write (unit = lunit(6), fmt = 76604) (caslnr(ii), ii = istart, iend)
     write (unit = lunit(6), fmt = 76604) (caslnx(ii), ii = istart, iend)
     istart = istart + 9
     if (idumy .gt. 9) go to 76661
  end do
76652 continue
  istart = locatn (npos2, npos2) + 1
  iend = locatn (npos3, npos3)
  i = iend - istart + 1
  call move0 (caslnr(istart :), i)
  call move0 (caslnx(istart :), i)
  go to 76521
76600 if (noutpr .eq. 0 ) write (unit = kunit6, fmt = 76620)
76620 format ('+Termination of cascaded pi.')
  locznn = locatn (npos2, npos2) + locz11 - 1
  it = locz11 + locatn (npos2, npos2)
  tmax = -1.0
  icas = 0
  do i = locz11, locznn
     j = i - locz11 + 1
     tr(i) = caslnr(j)
     tx(i) = caslnx(j)
     c(i) = 0.0d0
  end do
  do i = ipoint, ibr
     kodebr(i) = -2
  end do
  if (iprsup .lt. 2) go to 100
  write (unit = lunit(6), fmt = 76607)
76607 format (5x, 'Y-matrix for line represented by cascaded-pi.')
  do i = 1, npos2
     write (unit = lunit(6), fmt = 76605)
76605 format (//, 1x)
     istart = locatn(i - 1, i - 1) + 1
     idumy = i + 9
76603 idumy = idumy - 9
     iend = idumy + istart - 1
     if (idumy .gt. 9) iend = istart + 8
     write (unit = lunit(6), fmt = 76604) (caslnr(ii), ii = istart, iend)
     write (unit = lunit(6), fmt = 76604) (caslnx(ii), ii = istart, iend)
76604 format (1x, 9e13.5)
     istart = istart + 9
     if (idumy .gt. 9) go to 76603
  end do
100 lastov = nchain
  nchain = 2
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module over3."')
  go to 99999
9200 lastov = nchain
  nchain = 51
  lstat(18) = 3
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
99999 if (allocated (trshun)) then
     kk = transfer (trshun, kk)
     deallocate (trshun)
  end if
  if (allocated (txshun)) then
     kks = transfer (txshun, kks)
     deallocate (txshun)
  end if
  if (allocated (cshun)) then
     kknonl = transfer (cshun, kknonl)
     deallocate (cshun)
  end if
  if (allocated (node2)) then
     voltk = transfer (node2, voltk)
     deallocate (node2)
  end if
  if (allocated (node1)) then
     volti = transfer (node1, volti)
     deallocate (node1)
  end if
  if (allocated (mapinv)) then
     volt = transfer (mapinv, volt)
     deallocate (mapinv)
  end if
  if (allocated (mapcas)) then
     ykm = transfer (mapcas, ykm)
     deallocate (mapcas)
  end if
  if (allocated (cser)) then
     kode = transfer (cser, kode)
     deallocate (cser)
  end if
  return
end subroutine over3

!
! subroutine over4.
!

subroutine over4
  use blkcom
  use labcom
  use space1
  use indcom
  use tracom
  implicit none
  integer(4) :: i, iend, ik, ikp, iofa1p, ioftm1, ioftm2
  integer(4) :: j, jfdep2
  integer(4) :: kfdep2, kfin, kflop
  integer(4) :: locmax, locmin
  integer(4) :: mode
  integer(4) :: n, n1, n2, n3, n4, n5, n6, n7, n9, n10, n12, nii, npoint
  integer(4) :: ntime1, ntime2
  real(8) :: a, a1, aamp, aditn, atheo
  real(8) :: bct, buffin(8)
  real(8) :: cut1, cut2
  real(8) :: d1, d2, d3, d4, d5, d6, d12, d17, d18, d1tttt, dzero
  real(8) :: fder, fstar
  real(8) :: rtotal
  real(8) :: t1, tails, tbound, tcor, tend
  real(8) :: z1
  !
  !     Frequency dependent line. ********************************
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module over4."')
  jfdep2 = 2 * lfdep + ifdep2
  kfdep2 = 4 * lfdep + ifdep2
  d1tttt = t
  if (kburro .eq. 1) go to 8716
  iofa1p = (location (weight(1)) - location (ykm(1))) / 4
  go to 8721
8716 iofa1p = lsiz23 / 4
8721 ioftm2 = 2 * iofa1p
  ioftm1 = 3 * iofa1p
  ifdep = ifdep + 1
  if (ifdep .le. lfdep) go to 20170
  kill = 1
  lstat(16) = 13
  lstat(19) = 20170
  go to 9200
20170 nr(ibr) = ifdep
  length(ibr) = kcount
  ck(ibr) = -ck(ibr)
  d1 = -tenm3 / 100.0d0
  if (ck(ibr) .gt. d1) ck(ibr) = d1
  ci(ibr) = absz (ci(ibr))
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 6105) ntime1, ntime2, locmax, locmin, z1, cut1, cut2, n10, rtotal, nii
6105 format (4i8, 3f8.0, i8, f8.0, i8)
  if (cut1 .le. 0.0d0) cut1 = 10. * tenm3
  if (cut2 .le. 0.0d0) cut2 = 100. * tenm3
  if (nii .le. 0) nii = 100
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 6110)
6110 format ('+Frequency-dependence misc. Data values.')
  n9 = noutpr
  noutpr = n10
  n1 = ntime1
  if (ntime2 .gt. n1) n1 = ntime2
  if (n1 + 3 .le. iofa1p) go to 6103
  kill = 145
  lstat(14) = ntime1
  lstat(15) = ntime2
  lstat(16) = iofa1p
  lstat(19) = 6103
  go to 9200
6103 n2 = ioftm1 + 1
  n3 = iofa1p + 1
  n12 = ioftm1 + ntime1
  go to 5736
  !     read input card using cimage.
6111 call cimage
  read (unit = abuff, fmt = 6115) buffin
  do j = 1, 7, 2
     tim1(n2) = buffin(j)
     a1p(n3) = buffin(j + 1)
     n2 = n2 + 1
     n3 = n3 + 1
  end do
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 6112)
6112 format ('+Weighting function a1 points.')
5736 if (n2 .le. n12) go to 6111
  n4 = ioftm2 + 1
  n5 = 1
  go to 5741
  !     read input card using cimage.
6116 call cimage
  read (unit = abuff, fmt = 6115) buffin
  do j = 1, 7, 2
     tim2(n4) = buffin(j)
     a2p(n5) = buffin(j + 1)
     n4 = n4 + 1
     n5 = n5 + 1
  end do
6115 format (8f10.0)
  if (noutpr .eq. 0 .and. n12 .gt. 0) write (unit = kunit6, fmt = 6117)
6117 format ('+Weighting function a2 points.')
5741 n12 = ntime2 - n5
  if (n12 .le. 7) noutpr = n9
  if (n12 .ge. 0) go to 6116
  do i = 1, ntime1
     n6 = ioftm1 + i
     tim1(n6) = tim1(n6) / 1000000.0d0
  end do
  do i = 1, ntime2
     n7 = ioftm2 + i
     tim2(n7) = tim2(n7) / 1000000.0d0
  end do
  d3 = 0.0d0
  do i = 2, ntime1
     n2 = ioftm1 + i
     n4 = iofa1p + i
     d1 = tim1(n2) - tim1(n2 - 1)
     d2 = a1p(n4) + a1p(n4 - 1)
     if (d1 .gt. 0.0d0) go to 6125
     kill = 45
     lstat(19) = 6125
     lstat(17) = 1
     lstat(14) = i
     flstat(15) = tim1(n2)
     flstat(16) = tim1(n2 - 1)
     go to 9200
6125 d3 = d3 + d1 * d2
  end do
  do i = 2, ntime2
     n6 = ioftm2 + i
     d1 = tim2(n6) - tim2(n6 - 1)
     d2 = a2p(i) + a2p(i - 1)
     if (d1 .gt. 0.0d0) go to 6126
     kill = 45
     lstat(19) = 6126
     lstat(17) = 2
     lstat(14) = i
     flstat(15) = tim2(n6)
     flstat(16) = tim2(n6 - 1)
     go to 9200
6126 d3 = d3 + d1 * d2
  end do
  d3 = d3 * onehaf
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 6129) d3
6129 format ('+Weighting function a2 points.', f16.8)
  if (rtotal .gt. 0.0) go to 7820
  d1 = 1.0 / d3
  do i = 1, ntime1
     n2 = iofa1p + i
     a1p(n2) = a1p(n2) * d1
  end do
  do i = 1, ntime2
     a2p(i) = a2p(i) * d1
  end do
7820 cut2 = cut2 * a2p(locmin)
  atheo = rtotal / (2.0d0 * z1 + rtotal)
  iend = ntime2
  dzero = 0.0d0
  i = 1
  kfin = 0
  n = 0
  tend = 0.0d0
6200 a1 = ap(i)
  aditn = 0.0d0
  n3 = ioftm2 + i
  n4 = ioftm2 + iend
  t1 = tp(n3)
  tbound = tp(n4)
  iwtent(ifdep2) = jst1
  weight(jst1) = 0.0d0
  n = n - 1
  kflop = 1
  mode  = 0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6213) ifdep, ifdep2, jst, i, ioftm2, iend, dzero, deltat
6213 format (/, ' Begin  A(t)  processing.   ifdep  ifdep2     jst       i  ioftm2    iend', 11x, 'dzero', 10x, 'deltat',/, 25x, 6i8, 2e16.7)
  go to 6450
6220 if (mode .gt. 0) go to 6240
  if (t .le. tend) go to 6240
  kflop = 1
  d1 = (a - a1) / (t - t1)
  a = a1 + d1 * (tend - t1)
  i = i - 1
  t = tend
  go to 6250
6240 if (i .lt. iend) go to 6250
  if (iprsup .le. 0) go to 6245
  write (unit = lunit(6), fmt = 6243) i, iend, jst, mode, kflop, lwt, kfin, n, ifdep, ifdep2, ioftm2, locmin, locmax
6243 format (/, ' End raw data points.       i    iend     jst    mode   kflop     lwt,     kfin       n   ifdep  ifdep2  ioftm2  locmin  locmax  ', /, 21x, 13i8)
  write (unit = lunit(6), fmt = 6244)  t1, a1, t, a, d5, d6, tend, d3
6244 format (/, 15x, 't1', 14x, 'a1',  15x, 't', 15x, 'a',  14x, 'd5', 14x, 'd6', 12x, 'tend', 14x, 'd3', /, 1x, 8e16.7)
6245 i = -1
  kflop = 1
6250 d1 = (t - t1) * onehaf
  d18 = d1 * (a1 + a)
  d5 = d5 + d18
  d6 = d6 + d1 * (t1 * a1 + t * a)
  t1 = t
  a1 = a
  if (mode .eq. 0) go to 6260
  if (i .gt. 0) go to 6450
  go to 6465
6260 aditn = aditn + d18
  if (kflop .eq. 0) go to 6450
  d6 = d6 / deltat
  if (iprsup .ge. 4) write (unit = lunit(6), fmt = 6316) i, jst, mode, n, t, a, d5, d6
6316 format (/, ' Done with panel.       i     jst    mode       n', 15x, 't', 15x, 'a', 14x, 'd5', 14x, 'd6', /, 17x, 4i8, 4e16.7)
  if (dzero .ne. 0.0) go to 6320
  dzero = d5
  d3 = dzero - d6
  d4 = 1.0 + d3
  d5 = 1.0 - d3
  eta(ifdep) = d4 / d5
  d3 = 1.0 / d4
  weight(jst) = d6 * d3
  go to 6455
  !     the following changes restore weighting functions to the
  !     way they were in  "m21." .   remove the  "m22."  change
  !     which has (we suspect) lead to instability for al's cases.
6320 d1 = d6 - n * d5
  d2 = (d5 - d1) * d3
  d1 = d1 * d3
  weight(jst) = weight(jst) + d2
  if (n .le. 5 .and. iprsup .ge. 2) write (unit = lunit(6), fmt = 6323) d1, d2, d3, weight(jst)
6323 format (/, " Accumulate  'weight' .", 4x, 'weight(jst1)', 14x, 'd2', 14x, 'd3', 5x, 'weight(jst)', /, 23x, 4e16.7)
  weight(jst1) = d1
  if (kfin .gt. 0) go to 6340
  if (i .le. locmin) go to 6450
  if (a .lt. cut2) go to 6450
  go to 6410
6340 if (i .le. locmax) go to 6450
  if (a .gt. cut1) go to 6450
6410 mode  = 1
  tails  = t
  aamp = a
6450 if (kflop .eq. 0) go to 6460
  jst = jst1
  jst1 = jst1 + 1
  if (jst .le. lwt) go to 6455
  kill = 1
  lstat(16) = 14
  lstat(19) = 6455
  go to 9200
6455 if (i .lt. 0) go to 6470
  kflop = 0
  d5 = 0.0d0
  d6 = 0.0d0
  tend = tend + deltat
  n = n + 1
6460 i = i + 1
  n5 = ioftm2 + i
  t = tp(n5)
  a = ap(i)
  go to 6220
6465 d1 = 1.0d0 / (d6 / d5 - tails)
  d2 = d1 * d5
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 6466) ifdep2, d2, d1, tails, d5, d6
6466 format (/, ' Tail integral complete.  ifdep2', 14x, 'd2', 14x, 'd1', 11x, 'tails', 14x, 'd5', 14x, 'd6', /, 24x, i8, 5e16.7)
  con1(ifdep2) = d2 * d3
  con1(jfdep2) = d1
  con1(kfdep2) = tails
  if (rtotal .le. 0.0d0)  go to 6475
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 7812) d3, aamp, aditn, con1(ifdep2), d1, tails, ifdep2
7812 format ('  Before tail adjustment.', /, 18x, 'd3', 16x,'aamp', 15x, 'aditn', 15x, 'tailp', 15x, 'tailq', 15x, 'tails', 4x, 'ifdep2', /, x, 6e20.11, i10)
  tcor = atheo - aditn
  !     start Newton Raphson iterations on tailq * * * * * * * * * * * * *
  bct = absz (tcor / aamp)
  bct = alogz (bct)
  d18 = 0.0d0
  ik = 0
7843 ik = ik + 1
  if (ik .gt. nii) go to 7904
  d12 = d1 + d18
  if (ik .eq. 1) go to 7910
  d17 = absz (d18) / d1
  if (d17 .le. epsiln) go to 7905
7910 d1 = d12
  fstar = -alogz (d1) - d1 * tails - bct
  fder = 1.0d0 / d1 + tails
  d18 = fstar / fder
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 7912) ik, fstar, fder, d18, d1, bct
7912 format (1x, 'On iteration no.', i5, /, 5e22.12)
  do ikp = 1, 6
     if (absz (d18) .lt. d1) go to 7843
     d18 = d18 / 5.0d0
  end do
  if (absz (d18) .lt. d1) go to 7843
7904 kill = 46
  lstat(19) = 7904
  lstat(14) = nii
  lstat(15) = ifdep
  flstat(15) = rtotal
  go to 9200
7905 con1(jfdep2) = d1
  con1(ifdep2) = tcor * d1 * d3
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 7913) ifdep2, con1(ifdep2), con1(jfdep2)
7913 format (' Successful termination.  ifdep2', 15x, 'tailp', 15x, 'tailq', /, 24x, i8, 2e20.12)
  go to 6475
6470 con1(ifdep2) = 0.0d0
  con1(jfdep2) = 0.0d0
  con1(kfdep2) = 0.0d0
6475 npoint = jst - iwtent(ifdep2)
  ifdep2 = ifdep2 + 1
  jfdep2 = jfdep2 + 1
  kfdep2 = kfdep2 + 1
  if (kfin .gt. 0) go to 6600
  kfin = 1
  n6 = iofa1p + locmax
  cut1 = cut1 * a1p(n6)
  n = int (tim1(ioftm1 + 1) / deltat)
  iskip(ifdep) = n
  tend = n * deltat
  nhist(ifdep) = npoint
  i = iofa1p
  iend = ntime1 + i
  locmax = locmax + i
  i = i + 1
  atheo = (2.0d0 * z1) / (2.0d0 * z1 + rtotal)
  go to 6200
6600 zinf(ifdep) = z1
  npoint = npoint + (iskip(ifdep) - 1)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 6601) ibr, jst, npoint, iskip(ifdep), nr(ibr), nhist(ifdep)
6601 format (/, " Ready to exit  'over4' .", 12x, 'ibr', 12x, 'jst', 9x, 'npoint', 3x, 'iskip(ifdep)', 8x, 'nr(ibr)', 3x, 'nhist(ifdep)', /, 25x, 6i15)
  if (npoint .gt. nhist(ifdep)) nhist(ifdep) = npoint
  nhist(ifdep) = nhist(ifdep) + 1
  icheck = 3
  lastov = nchain
  nchain = 2
  t = d1tttt
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module over4."')
  go to 99999
9200 lastov = nchain
  nchain = 51
  lstat(18) = 4
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
99999 return
end subroutine over4

!
! end of file over2.f90
!
