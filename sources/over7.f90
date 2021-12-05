!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over7.f90
!

!
! subroutine over7.
!

subroutine over7
  use blkcom
  use labcom
  use space2
  use movcop
  implicit none
  !  dimension lorder(1), ndex(20)
  !
  !  equivalence (e(1), ndex(1))
  !  equivalence (ich2(1), lorder(1))
  !  equivalence (iofkol, iofgnd)
  !  equivalence (iofkor, iofbnd)
  !
  !  Following carries "next" among over6, insert, over7, & over9:
  !
  !  equivalence (loopss(11), next)
  integer(4) :: i, ib, icon, ii, ischm, ist, isubs1, isubs2
  integer(4) :: j, jb, jbs, jbt, js, jsub
  integer(4) :: k, kb, ks
  integer(4) :: l, lastxx, ll0, ls
  integer(4) :: m, mext
  integer(4) :: n1, n2, n13, ncn, ndx1, nelim, nz
  real(8) :: td
  real(8) :: zzza
  !
  integer(4), pointer :: iofkol
  integer(4), pointer :: iofkor
  integer(4), pointer :: lorder(:)
  integer(4), allocatable :: ndex(:)
  integer(4), pointer :: next
  !
  ll0 = size (transfer (e, ndex))
  allocate (ndex(ll0))
  ndex = transfer (e, ndex)
  lorder(1 :) => ich2(1 :)
  iofkol => iofgnd
  iofkor => iofbnd
  next => loopss(11)
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4567)
4567 format ('  "Begin module over7."')
  ischm = 2
  if(iprsup .lt. 4) go to 4054
  write (unit = lunit6, fmt = 4050) ischm, ntot, next, iofkol, iofkor, ncurr
4050 format (/, " Scalars upon entry into renumbering,  'over7' .ischm    ntot    next  iofkol  iofkor   ncurr  ", /, 50x, 6i8)
  write (unit = lunit6, fmt = 4053)
4053 format (" Integer arrays upon entry into renumbering,  'over7' .  ", /,  '     row   kolum  korder   kownt     loc   kode kbus    mbus      nr  length   bus1     bus2')
  do i = 1, next
     n1 = i + iofkol
     n2 = i + iofkor
     ndx1 = lswtch + i
     write (unit = lunit6, fmt = 4055) i, kolum(n1), korder(n2), kownt(i), loc(i), kode(i), kbus(i), mbus(i), nr(i), length(i), kmswit(i), kmswit(ndx1)
4055 format (12i8)
  end do
4054 ioffd = 0
  td = 0.0d0
  zzza = 0.0d0
  lastxx = last
  ! n1 = lbus + 1  // use lbus, not this n1,  in following:
  call move0 (ndex, lbus)
  nz = ncurr
  i = 0
140 i = i + 1
  if (ischm .eq. 3) go to 1510
  j = kownt(i)
  call subscr (i, lbus, 150, 1)
  if (j .eq. (-1)) go to 170
155 k =  int (ndex(j + 1))
  jsub = j + 1
  call subscr (jsub, lbus, 155, 1)
  ich1(i) = k
  call subscr (i, lbus, 155, 2)
  if (k .gt. 0) ich2(k) = i
  call subscr (k, lbus, 155, 99)
  ndex(j + 1) = i
  ich2(i) = 0
  norder(i) = 0
  go to 180
170 nz = nz + 1
  norder(i) = nz
  call subscr (i, lbus, 170, 1)
180 if (i .lt. ntot) go to 140
  if (nz .eq. ntot) go to 190
  lstat(19) = 190
  nelim = 0
  go to 453
190 ncn = 0
  ! next delete obviously zero-cell zeroing of vectors:
  nelim = 1
200 do
     if (ncn .ge. lbus) exit
     jsub = ncn + 1
     call subscr (jsub, lbus, 200, 1)
     if (ndex(ncn + 1) .ne. 0) exit
     ncn = ncn +1
  end do
220 if (ncurr .lt. nelim)   go to 229
  jsub = ncn + 1
  call subscr (jsub, lbus, 220, 1)
  i = int (ndex(ncn + 1))
  index(nelim) = ioffd + 1
  call subscr (nelim, lbus, 220, 2)
  if (iprsup .le. 25) go to 222
  write (unit = lunit6, fmt = 4061) nelim, ncn, ioffd, ntot, ncurr, next, mext, icon
4061 format (" Prepare to renumber next network node in  'over7'.     nelim     ncn   ioffd    ntot   ncurr    next    mext    icon   ", /, 54x, 8i8)
  if (iprsup .le. 34) go to 222
  write (unit = lunit6, fmt = 4062)
4062 format (' Renumbering arrays.       row   kolum  korder   kownt     loc    ich1    ich2    ndex')
  do ii = 1, 15
     n1 = ii + iofkol
     n2 = ii + iofkor
     write (unit = lunit6, fmt = 4064) ii, kolum(n1), korder(n2), kownt(ii), loc(ii), ich1(ii), ich2(ii), ndex(ii)
4064 format (22x, 8i8)
  end do
222 j = ich1(i)
  call subscr (i, lbus, 222, 1)
  ndex(ncn + 1) = j
  jsub = ncn + 1
  call subscr (jsub, lbus, 222, 2)
  if (j .gt. 0) ich2(j) = 0
  call subscr (j, lbus, 222, 99)
  norder(i) = nelim
  go to 240
229 do i = 1, ntot
     j = norder(i)
     call subscr (j, lbus, 229, 1)
     lorder(j) = i
  end do
  if (iprsup .ge. 3) write (unit = lunit6, fmt = 5231)
5231 format (/, " Final renumbering arrays at the end of  'over7' .    row  norder   index    iloc  lorder   kownt     loc   kolum    korder")
  do i = 1, 20
     n1 = i + iofkol
     n2 = i + iofkor
     if (iprsup  .ge.  3) write (unit = lunit6, fmt = 5233) i, norder(i), index(i), iloc(i), lorder(i), kownt(i), loc(i), kolum(n1), korder(n2)
5233 format (50x, 9i8)
  end do
  if (ioffd .le. 0) go to 233
  do i = 1, ioffd
     call subscr (i, lsiz23, 5235, 1)
     j = iloc(i)
     call subscr (j, lbus, 5235, 2)
     j = norder(j)
     iloc(i) = j
  end do
233 if (nelim .gt. ntot) go to 5236
  do i = nelim , ntot
     call subscr (i, lbus, 233, 1)
     index(i) = ioffd +1
     k = lorder(i)
     call subscr (k, lbus, 233, 2)
     j = loc(k)
234  if (j .eq. 0)   go to 236
     call subscr (j, lsiz23, 234, 1)
     isubs1 = iofkol + j
     k = kolum(isubs1)
     call subscr (k, lbus, 234, 2)
     l = norder(k)
     if (i .gt. l)   go to 235
     ioffd = ioffd + 1
     call subscr (ioffd, lsiz23, 234, 3)
     iloc(ioffd) = l
235  isubs1 =  iofkor + j
     call subscr (j, lsiz23, 235, 1)
     j = korder(isubs1)
     go to 234
  end do
236 continue
5236 index(ntot + 1) = ioffd + 1
  jsub = ntot + 1
  call subscr (jsub, lbus, 5236, 1)
  if (iprsup .ge. 3) write (unit = lunit6, fmt = 5241) (i, norder(i), index(i), iloc(i), i = 1, 20)
5241 format ( /, " Arrays upon exit from  'over7' .     row  norder index    iloc", /, (33x, 4i8))
  if (ioffd .gt. lstat(43)) lstat(43) = ioffd
5276 if (lastov .gt. nchain) go to 5283
  if (iprsup .gt. 0) write (unit = lunit6, fmt = 47881) (norder(i), i = 1, ntot)
47881 format (/, ' (norder(i), i = 1, ntot)   after transient-network renumbering .', /, (1x, 20i6))
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 54230) next, last, ntot, kswtch, kconst, nv, ibr, inonl, npower, it, istead, norder(1), ncurr
54230 format (/, " Before 'over8' .    Next    last    ntot  kswtch  kconst      nv     ibr   inonl  npower      it  istead  nor(1) ncurr", /, 17x, 13i8)
  if (norder(1) .eq. 1) go to 14
  if (noutpr .eq. 0) write (unit = lunit6, fmt = 47881) (norder(i), i = 1, ntot)
  if (iprsup .gt. 0) write (unit = lunit6, fmt = 6045)
6045 format (' Note ---- The preceding printout shows that ground was not renumbered first in the transient-network renumbering.', /, 11x, 'We will swap the new numbers between ground and whatever node was renumbered first, in order to get a legitimate', /, 11x, 'numbering for the rest of the emtp calculations.')
  do i = 1, ntot
     if (norder(i) .eq. 1) go to 32
  end do
  call stoptp
32 norder(i) = norder(1)
  norder(1) = 1
14 do i = 1, ntot
     j = norder(i)
     ich1(j) = i
  end do
  if (kconst .eq. 0) go to 73820
  n1 = 0
  j = 1
  do i = 2, ntot
     k = ich1(i)
     if (kode(k) .eq. 0) go to 73775
     do l = 1, kconst
        if (iprsup .ge. 3) write (unit = lunit6, fmt = 6049) i, k, kode(k), n1, j, l, node(l), tstart(l)
6049    format (' At 6049 of "over7".  i, k, kode(k), etc. =', 7i8, e20.10)
        ! s.m.  source node must precede regular voltage source nodes.
        ! alter renumbering map to produce this, if  tstart  .eq.  -9988.
        if (tstart(l) .ne. -9988.) go to 6051
        if (node(l) .eq. k) go to 73775
     end do
6051 continue
     n1 = n1 + 1
     norder(n1) = k
     go to 73780
73775 j = j + 1
     ich1(j) = k
  end do
73780 continue
  kpartb = j
  if (n1 .le. 0) go to 73820
  do i = 1, n1
     j = j + 1
     k = norder(i)
     ich1(j) = k
  end do
  do i = 1, ntot
     j = ich1(i)
     norder(j) = i
  end do
73820 continue
  lastov = nchain
  nchain = nchain + 1
  go to 5294
5283 n1 = nchain
  nchain = lastov + 1
  lastov = n1
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
4568 format ('  "Exit  module over7."')
5294 go to 99999
240 nelim = nelim +1
  if (ischm .eq. 1) go to 200
  call subscr (i, lbus, 240, 1)
  if (loc(i) .eq. 0)   go to 200
  ist = loc(i)
  call subscr (i, lbus, 260, 1)
  jst = ist
  go to 290
270 isubs1 =  iofkor+jst
  call subscr (jst, lsiz23, 270, 1)
  jst = korder(isubs1)
  if (jst .ne. 0) go to 290
  if (ischm .eq. 3) go to 1110
  go to 200
290 ib = ist
  call subscr (jst, lsiz23, 290, 1)
  isubs1 = iofkol + jst
  j = kolum(isubs1)
  call subscr (j, lbus, 290, 2)
  jb = loc(j)
  icon = -1
  jbs = 0
  if (jb .eq. 0) go to 200
  ioffd = ioffd + 1
  call subscr (ioffd, lsiz23, 300, 1)
  iloc(ioffd) = j
310 isubs1 = iofkol + ib
  isubs2 = iofkol + jb
  call subscr (ib, lsiz23, 310, 1)
  call subscr (jb, lsiz23, 310, 2)
  n13 = kolum(isubs1) - kolum(isubs2)
  if (n13 .gt. 0) go to 500
  if (n13 .lt. 0) go to 360
  ! if (kolum(isubs1) - kolum(isubs2)) 360, 320, 500
  ! 320 isubs1 =  iofkor+ib
  isubs1 = iofkor + ib
  if (korder(isubs1) .eq. 0) go to 430
  isubs1 = iofkor + ib
  call subscr (ib, lsiz23, 330, 1)
  ib = korder(isubs1)
  isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 340, 1)
  if (korder(isubs1) .eq. 0) go to 700
  jbs = jb
  isubs1 =  iofkor + jb
  call subscr (jb, lsiz23, 350, 1)
  jb = korder(isubs1)
  go to 310
360 isubs1 = iofkol + ib
  call subscr (ib, lsiz23, 360, 1)
  if (kolum(isubs1) .eq. j) go to 410
  isubs1 = iofkol+next
  call subscr (next, lsiz23, 370, 1)
  isubs2 = iofkol + ib
  call subscr (ib, lsiz23, 370, 2)
  kolum(isubs1) = kolum(isubs2)
  mext = next
  if (next .eq. lastxx) go to 375
  call subscr (next, lsiz23, 370, 3)
  isubs1 = iofkor + next
  next = korder(isubs1)
  isubs1 = iofkor + mext
  call subscr (mext, lsiz23, 370, 4)
  korder(isubs1) = jb
  jbt = jbs
  jbs = mext
  icon = icon + 1
  go to 380
375 kill = 16
  lstat(19) = 375
  lstat(16) = last
  lstat(15) = nelim
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
  go to 9999
380 if (jbt .eq. 0) go to 400
  isubs1 = iofkor + jbt
  call subscr (jbt, lsiz23, 390, 1)
  korder(isubs1) = mext
  go to 410
400 loc(j) = mext
  call subscr (j, lbus, 400, 1)
410 isubs1 = iofkor + ib
  call subscr (ib, lsiz23, 410, 1)
  ib = korder(isubs1)
  if (ib .ne. 0) go to 310
430 isubs1 =  iofkol + jb
  call subscr (jb, lsiz23, 430, 1)
  ! if (kolum(isubs1)    -i) 440, 470, 790
  ! 440 jbs = jb
  n13 = kolum(isubs1) - i
  if (n13 .eq. 0) go to 470
  if (n13 .gt. 0) go to 790
  jbs = jb
  isubs1 = iofkor + jb
  jb = korder(isubs1)
  if (jb .ne. 0) go to 430
  lstat(19) = 450
453 kill = 25
  lstat(16) = nelim
  lstat(15) = jbs
  lstat(14) = i
  lstat(13) = ib
  lstat(12) = mext
  lstat(11) = next
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
  go to 9999
470 isubs1 = iofkor + lastxx
  call subscr (lastxx, lsiz23, 470, 1)
  korder(isubs1) = jb
  lastxx = jb
  if (jbs .eq. 0) go to 490
  isubs1 = iofkor + jbs
  isubs2 = iofkor + jb
  call subscr (jbs, lsiz23, 480, 1)
  call subscr (jb, lsiz23, 480, 2)
  korder(isubs1) = korder(isubs2)
  go to 790
490 isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 490, 1)
  loc(j) = korder(isubs1)
  call subscr (j, lbus, 490, 2)
  go to 790
500 isubs1 = iofkol + jb
  call subscr (jb, lsiz23, 500, 1)
  !     if (kolum(isubs1)    -i) 510, 540, 520
  ! 510 jbs = jb
  n13 = kolum(isubs1) - i
  if (n13 .eq. 0) go to 540
  if (n13 .gt. 0) go to 520
  jbs = jb
  isubs1 =  iofkor+jb
  jb = korder(isubs1)
  go to 310
520 isubs1 = iofkor+jb
  call subscr (jb, lsiz23, 520, 1)
  if (korder(isubs1) .eq. 0) go to 700
  jbs = jb
  isubs1 = iofkor+jb
  call subscr (jb, lsiz23, 530, 1)
  jb = korder(isubs1)
  go to 310
540 if (jbs .eq. 0)   go to 580
  isubs1 = iofkor + jbs
  isubs2 =  iofkor + jb
  call subscr (jbs, lsiz23, 550, 1)
  call subscr (jb, lsiz23, 550, 2)
  korder(isubs1) = korder(isubs2)
  isubs1 = iofkor + lastxx
  call subscr (lastxx, lsiz23, 550, 3)
  korder(isubs1) = jb
  lastxx = jb
  isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 560, 1)
  if (korder(isubs1) .ne. 0) go to 575
  jb = jbs
  go to 700
575 isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 575, 1)
  jb = korder(isubs1)
  go to 310
580 isubs1 = iofkor + lastxx
  call subscr (lastxx, lsiz23, 580, 1)
  korder(isubs1) = jb
  lastxx = jb
  isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 580, 2)
  if (korder(isubs1) .eq. 0) go to 600
  isubs1 =  iofkor + jb
  call subscr (jb, lsiz23, 590, 1)
  call subscr (j, lbus, 590, 2)
  loc(j) = korder(isubs1)
  isubs1 = iofkor + jb
  jb = korder(isubs1)
  go to 310
600 isubs1 = iofkol + ib
  call subscr (ib, lsiz23, 600, 1)
  if (kolum(isubs1) .eq. j) go to 620
610 loc(j) = next
  call subscr (j, lbus, 610, 1)
  jb = next
  mext = next
  !     if (next .eq. lastxx)   go to 615
  if (next .eq. lastxx)   go to 375
  isubs1 = iofkor + next
  call subscr (next, lsiz23, 610, 2)
  next = korder(isubs1)
  icon = icon + 1
  isubs1 = iofkol + jb
  isubs2 = iofkol + ib
  call subscr (jb, lsiz23, 610, 3)
  call subscr (ib, lsiz23, 610, 4)
  kolum(isubs1) = kolum(isubs2)
  !     go to 650     eliminate "continue";  use next line:
  go to 660
  ! 615 go to 375    there was only one transfer to 615
620 isubs1 = iofkor + ib
  call subscr (ib, lsiz23, 620, 1)
  ib = korder(isubs1)
  if (ib .ne. 0)   go to 610
  loc(j) = 0
  call subscr (j, lbus, 640, 1)
  go to 790
  ! 650 continue      eliminate "continue" not end of do loop
660 isubs1 = iofkor + ib
  call subscr (ib, lsiz23, 660, 1)
  if (korder(isubs1) .eq. 0) go to 690
  isubs1 = iofkor + ib
  call subscr (ib, lsiz23, 670, 1)
  ib = korder(isubs1)
  isubs1 = iofkol + next
  isubs2 = iofkol + ib
  call subscr (next, lsiz23, 670, 2)
  call subscr (ib, lsiz23, 670, 3)
  kolum(isubs1) = kolum(isubs2)
  go to 680
  ! 675 go to 375        convert "go to 675"  to  "go to 375"
680 mext = next
  !     if (next .eq. lastxx)   go to 675    eliminate 675 go to 375
  if (next .eq. lastxx) go to 375
  isubs1 = iofkor + next
  call subscr (next, lsiz23, 680, 1)
  next = korder(isubs1)
  icon = icon + 1
  !     go to 650     eliminate "650 continue";  use next line:
  go to 660
690 isubs1 = iofkor + mext
  call subscr (mext, lsiz23, 690, 1)
  korder(isubs1) = 0
  go to 790
700 isubs1 = iofkol + ib
  call subscr (ib, lsiz23, 700, 1)
  !     if (kolum(isubs1)    -j) 710, 760, 780
  ! 710 isubs1 =  iofkol+next
  n13 = kolum(isubs1) - j
  if (n13 .eq. 0) go to 760
  if (n13 .gt. 0) go to 780
  isubs1 = iofkol + next
  call subscr (next, lsiz23, 700, 2)
  isubs2 = iofkol + ib
  kolum(isubs1)      = kolum(isubs2)
  isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 700, 3)
  korder(isubs1) = next
720 mext = next
  !     if (next .eq. lastxx)   go to 725       "725 go to 375"
  if (next .eq. lastxx) go to 375
  isubs1 = iofkor + next
  call subscr (next, lsiz23, 720, 1)
  next = korder(isubs1)
  icon = icon + 1
  isubs1 = iofkor + ib
  call subscr (ib, lsiz23, 720, 2)
  ib = korder(isubs1)
  go to 730
  ! 725 go to 375    convert  "go to 725"  to  "go to 375"
730 isubs1 =  iofkol + ib
  call subscr (ib, lsiz23, 730, 1)
  !     if (kolum(isubs1)    -j) 750, 650, 740    with 650>>>660
  ! 740 kill = 26
  n13 = kolum(isubs1) - j
  if (n13 .lt. 0) go to 750
  if (n13 .eq. 0) go to 660
  kill = 26
  lstat(19) = 730
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
  go to 9999
750 isubs1 = iofkol + next
  isubs2 = iofkol + ib
  call subscr (next, lsiz23, 750, 1)
  call subscr (ib, lsiz23, 750, 2)
  kolum(isubs1) = kolum(isubs2)
  go to 720
760 isubs1 = iofkor + ib
  call subscr (ib, lsiz23, 760, 1)
  if (korder(isubs1) .eq. 0) go to 790
  isubs1 = iofkor + ib
  call subscr (ib, lsiz23, 770, 1)
  ib = korder(isubs1)
780 isubs1 = iofkol + next
  isubs2 = iofkol + ib
  call subscr (next, lsiz23, 780, 1)
  call subscr (ib, lsiz23, 780, 2)
  kolum(isubs1) = kolum(isubs2)
  isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 780, 3)
  korder(isubs1) = next
  go to 680
790 if (ischm .ne. 2) go to 270
  if (kownt(j) .eq. (-1)) go to 270
  ! 800 if (icon) 810, 270, 830
  ! 810 if (kownt(j) +icon .ge. ncn)   go to 830
  if (icon .eq.  0) go to 270
  call subscr (j, lbus, 800, 1)
  !     this check here also ensure "j" at s.n. 830 is ok:
  if (icon .gt. 0) go to 830
  n13 = kownt(j) + icon
  if (n13 .ge. ncn) go to 830
  ncn = kownt(j) + icon
  call subscr (j, lbus, 820, 1)
  !     call subscr  not needed here (j is ok;  see below 800):
830 if (ich2(j) .eq. 0) go to 850
  l = ich2(j)
  k = ich1(j)
  call subscr (j, lbus, 840, 1)
  call subscr (k, lbus, 840, 99)
  call subscr (l, lbus, 840, 3)
  ich1(l) = k
  if (k .gt. 0) ich2(k) = l
  m = kownt(j) + icon
  go to 860
850 k = ich1(j)
  call subscr (j, lbus, 850, 1)
  call subscr (k, lbus, 850, 99)
  if (k .gt. 0) ich2(k) = 0
  m = kownt(j)
  jsub = m + 1
  call subscr (jsub, lbus, 850, 3)
  ndex(m + 1) = k
  m = m + icon
860 kownt(j) = m
  call subscr (j, lbus, 860, 1)
  jsub = m + 1
  call subscr (jsub, lbus, 860, 2)
  k = int (ndex(m + 1))
  ndex(m+1) = j
  ich1(j) = k
  call subscr (k, lbus, 860, 99)
  if (k .gt. 0) ich2(k) = j
  ich2(j) = 0
  go to 270
1110 js = loc(i)
  call subscr (i, lbus, 1110, 1)
  go to 1130
1120 isubs1 =  iofkor + js
  call subscr (js, lsiz23, 1120, 1)
  js = korder(isubs1)
1130 icon = 0
  if (js .eq. 0)   go to 200
  isubs1 = iofkol + js
  call subscr (js, lsiz23, 1150, 1)
  j = kolum(isubs1)
  call subscr (j, lbus, 1150, 2)
  if (kownt(j) .lt. 0)   go to 1120
  ks = loc(j)
  ls = ks
  go to 1170
1160 isubs1 = iofkor + ks
  call subscr (ks, lsiz23, 1160, 1)
  ks = korder(isubs1)
1170 if (ks .ne. 0) go to 1190
  icon = icon / 2
  go to 1410
1190 isubs1 = iofkol + ks
  call subscr (ks, lsiz23, 1190, 1)
  k = kolum(isubs1)
  jb = ls
  call subscr (k, lbus, 1190, 2)
  kb = loc(k)
1200 isubs1 =  iofkol + jb
  isubs2 = iofkol + kb
  !     if (kolum(isubs1)    -kolum(isubs2)   ) 1210, 1250, 1270
  !1210 isubs1 =  iofkol+jb
  call subscr (jb, lsiz23, 1200, 1)
  call subscr (kb, lsiz23, 1200, 2)
  n13 = kolum(isubs1) - kolum(isubs2)
  if (n13 .eq. 0) go to 1250
  if (n13 .gt. 0) go to 1270
  isubs1 = iofkol + jb
  if (kolum(isubs1) .eq. k)   go to 1230
  icon = icon +1
1230 isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 1230, 1)
  jb = korder(isubs1)
  if (jb .ne. 0) go to 1200
  go to 1160
1250 isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 1250, 1)
  if (korder(isubs1) .eq. 0) go to 1160
  isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 1260, 1)
  jb = korder(isubs1)
1270 isubs1 = iofkor + kb
  call subscr (kb, lsiz23, 1270, 1)
  if (korder(isubs1) .eq. 0) go to 1290
  isubs1 = iofkor + kb
  call subscr (kb, lsiz23, 1280, 1)
  kb = korder(isubs1)
  go to 1200
1290 isubs1 = iofkol + jb
  call subscr (jb, lsiz23, 1290, 1)
  if (kolum(isubs1) .eq. k) go to 1310
  icon = icon +1
1310 isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 1310, 1)
  if (korder(isubs1) .eq. 0) go to 1160
  isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 1320, 1)
  jb = korder(isubs1)
  go to 1290
  !1410 if (kownt(j) -icon) 1440, 1120, 1420
  !1420 if (ncn .le. icon)   go to 1440
1410 call subscr (j, lbus, 1410, 1)
  n13 = kownt(j) - icon
  if (n13 .lt. 0) go to 1440
  if (n13 .eq. 0) go to 1120
  if (ncn .le. icon) go to 1440
  ncn = icon
1440 if (ich2(j) .eq. 0) go to 1460
  !     no check of "j" needed at 1440 above, since only thru 1410
  !     call subscr ( j, lbus,  ----- not needed.
  l = ich2(j)
  k = ich1(j)
  call subscr (j, lbus, 1450, 1)
  call subscr (l, lbus, 1450, 2)
  call subscr (k, lbus, 1450, 3)
  ich1(l) = k
  ich2(k) = l
  m = icon
  go to 1470
1460 k = ich1(j)
  call subscr (j, lbus, 1460, 1)
  call subscr (k, lbus, 1460, 2)
  ich2(k) = 0
  m = kownt(j)
  jsub = m + 1
  call subscr (jsub, lbus, 1460, 3)
  ndex(m + 1) = k
  m = icon
1470 kownt(j) = m
  jsub = m + 1
  call subscr (jsub, lbus, 1470, 1)
  call subscr (j, lbus, 1470, 2)
  k =  int (ndex(m + 1))
  ndex(m + 1) = j
  ich1(j) = k
  ich2(k) = j
  call subscr (k, lbus, 1470, 3)
  ich2(j) = 0
  go to 1120
1510 icon = 0
  call subscr (i,lbus, 1510, 1)
  if (kownt(i) .lt. 0) go to 170
  ks = loc(i)
  ls = ks
1520 if (ks .ne. 0) go to 1540
  j = icon / 2
  call subscr (i, lbus, 1530, 1)
  kownt(i) = j
  go to 155
1540 isubs1 = iofkol + ks
  call subscr (ks, lsiz23, 1540, 1)
  k = kolum(isubs1)
  jb = ls
  kb = loc(k)
  call subscr (k,  lbus, 1540, 2)
1550 isubs1 = iofkol + jb
  isubs2 = iofkol + kb
  call subscr (jb, lsiz23, 1550, 1)
  call subscr (kb, lsiz23, 1550, 2)
  !     if (kolum(isubs1)    -kolum(isubs2)   ) 1600, 1560, 1580
  !1560 isubs1 =  iofkor+jb
  n13 = kolum(isubs1) - kolum(isubs2)
  if (n13 .lt. 0) go to 1600
  if (n13 .gt. 0) go to 1580
  isubs1 = iofkor + jb
  if (korder(isubs1) .eq. 0) go to 1680
  isubs1 =  iofkor + jb
  call subscr (jb, lsiz23, 1570, 1)
  jb = korder(isubs1)
1580 isubs1 = iofkor + kb
  call subscr (kb, lsiz23, 1580, 1)
  if (korder(isubs1) .eq. 0) go to 1640
  isubs1 = iofkor+kb
  call subscr (kb, lsiz23, 1590, 1)
  kb = korder(isubs1)
  go to 1550
1600 isubs1 = iofkol + jb
  call subscr (jb, lsiz23, 1600, 1)
  if (kolum(isubs1) .eq. k) go to 1620
  icon = icon + 1
1620 isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 1620, 1)
  jb = korder(isubs1)
  if (jb .ne. 0)   go to 1550
  go to 1680
1640 isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 1640, 1)
  if (korder(isubs1) .eq. k) go to 1660
  icon = icon + 1
1660 isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 1660, 1)
  if (korder(isubs1) .eq. 0) go to 1680
  isubs1 = iofkor + jb
  call subscr (jb, lsiz23, 1670, 1)
  jb = korder(isubs1)
  go to 1640
1680 isubs1 = iofkor + ks
  call subscr (ks, lsiz23, 1680, 1)
  ks = korder(isubs1)
  go to 1520
9999 if (lastov .gt. nchain) go to 9996
  kill = 0
  write (unit = lunit6, fmt = 9998)
9998 format (/, ' Warning ---- Node renumbering of the transients network has broken down, presumably due to table overflow', /, ' (i.e., the network is too big and/or too dense for the present EMTP dimensioning).   the next time that the user', /, ' redimensions the emtp, he is advised to increase the size of list number  5  and/or  8 .   Both of these lists', /, ' contribute fully (100 per cent) to dependent list number  99,   which is what has actually overflowed at this point.')
  nelim = nelim - 1
  write (unit = lunit6, fmt = 9997) nelim, ntot, ncurr
9997 format (1x, i4, ' Nodes out of total of  ', i4, ' were renumbered before breakdown in the renumbering overlay.', /, ' Had we made it to', i6, '   nodes, the operation would have terminated normally (since the remaining ones', /, ' are always forced last without regard to sparsity considerations).')
  do i = 1, ntot
     if (norder(i) .gt. 0) go to 2236
     nelim = nelim + 1
     norder(i) = nelim
  end do
2236 continue
  write (unit = lunit6, fmt = 2240)
2240 format (/, ' Anyway, the EMTP will try to continue with execution of this data case, as best it can.   Nodes which were', /, ' not renumbered before the overflow limit was reached will now simply be renumbered in their original relative', /, ' order, without regard to sparsity considerations.   Recall that the original node order comes from the order of data', /, ' input (the order in which node names are encountered, as the EMTP data cards are read).')
  write (unit = lunit6, fmt = 2241) (norder(i), i = 1, ntot)
2241 format (' The final renumbering map   (norder(i), i = 1, ntot) will then appear as follows ....', /, (1x, 20i6))
  write (unit = lunit6, fmt = 2247)
2247 format (' The  k-th  such entry gives the row (and column) position of old variable  k  in the reordered matrix.', /, 1x)
  go to 5276
9996 lstat(18) = 7
  lastov = nchain
  nchain = 51
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
99999 if (allocated (ndex)) then
     e = transfer (ndex, e)
     deallocate (ndex)
  end if
  return
end subroutine over7

!
! subroutine subscr.
!

subroutine subscr (j, limit, istat, n1)
  use blkcom
  implicit none
  !     Module used to check out-of-bounds subscripts for "over7"
  !     special n1=99 case is for subscripts which are used only
  !     if positive (wsm changes to walker's code).
  integer(4), intent(in) :: j, istat, limit, n1
  !
  if ((j .ge. 1 .and. j .le. limit) .or. (j .le. 0 .and. n1 .eq. 99)) go to 9000
  write (unit = lunit6, fmt = 1487) j, limit, istat, n1
1487 format (' Out-of-bounds subscript =', i6, ' .   limit =', i6, ' .   Below s.n.', i6, '   with sequence no.', i2, ' .')
9000 if (iprsup .ge. 99) write (unit = lunit6, fmt = 9006) j, limit, istat, n1
9006 format (' Trace.  j, limit, istat, n1 =', 4i8)
  return
end subroutine subscr

!
! end of file over7.f90
!
