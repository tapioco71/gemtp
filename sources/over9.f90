!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file: over9.f90
!

!
! subroutine over9.
!

subroutine over9
  implicit none
  !  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'space2.ftn'
  include 'deck10.ftn'
  character :: fixbu3
  equivalence ( iofkol, iofgnd ), ( iofkor, iofbnd )
  !     following carries "next" among over6, insert, over7, & over9:
  equivalence  ( loopss(11), next )
  locatn(i,j)=(j*j-j)/2+i
  if ( iprsup  .ge.  1 ) write ( lunit6, 4567 )
4567 format ('  "Begin module over9."')
  if( iprsup .gt. 0 ) write(lunit6, 1001)  kconst, last
1001 format ( 36h steady-state phasor solution begins,35h (top of "over9").   kconst, last =,  2i8 )
  if ( istep  .ne. -4567 )  go to 1024
  nkr = 0
  lstat(70) = kconst
1111 call cimage
  read (unit = abuff, fmt = 1133) nk, bus1
1133 format ( i2, a6 )
  if ( bus1  .eq.  blank )  go to 2200
  nkr = nkr + 1
  if ( nkr  .ge.  lexct )  go to 9393
  read (unit = abuff, fmt = 1983) nekfix(nkr), fixbu1(nkr),fixbu2(nkr), fixbu3(nkr), fixbu5(nkr), fixbu8(nkr), &
       fixbu7(nkr), fixbu9(nkr), fixbu4(nkr), fixbu6(nkr)
1983 format (  i2, 3a6, 2e16.0, 2e8.0, 2e6.0 )
  if ( fixbu9(nkr) .eq. 0.0 ) fixbu9(nkr) = fltinf
  if ( fixbu4(nkr) .eq. 0.0 ) fixbu4(nkr) = - fltinf
  if ( fixbu6(nkr) .eq. 0.0 ) fixbu6(nkr) = fltinf
  write (kunit6, 1984) fixbu5(nkr), fixbu8(nkr),fixbu7(nkr), fixbu9(nkr)
1984 format ( 5h+fix.,  4e11.3 )
  go to 1111
9393 kill = 227
  write ( lunit6, 8944)  nkr
8944 format( 26h the number of fix sources, i5, 2x,46h should be less than the number of all sources  )
  go to 9999
2200 lstat(69) = nkr
  if ( tmax .gt. 0.0 )      go to 1024
  do i = 1, nkr
     texvec(1) = fixbu1(i)
     do k = 1, kconst
        k1 = iabs( node(k) )
        if ( bus(k1) .eq. texvec(1) ) go to 3131
1016 end do
     n1 = 1
     kconst = kconst + 1
     crest(kconst) = ( fixbu7(i) + fixbu9(i) ) * 0.5
     time1(kconst) = ( fixbu4(i) + fixbu6(i) ) * twopi / 720.
1911 do j = 2, ntot
        if ( bus(j) .eq. texvec(1) ) go to 5757
4949 end do
     kill = 500
     go to 9999
5757 node(kconst) = j
     j1 = kssfrq(j)
     sfreq(kconst) = sfreq(j1)
     iform(kconst) = 14
     tstop(kconst) = 1.0
     tstart(kconst) = -1.0
     go to ( 3131, 3333, 3535 ), n1
3131 texvec(1) = fixbu2(i)
     if ( texvec(1) .eq. blank ) go to 3535
     do k = 1, kconst
        k1 = iabs( node(k) )
        if ( bus(k1) .eq. fixbu2(i) ) go to 3333
1036 end do
     n1 = 2
     kconst = kconst + 1
     crest(kconst) = crest(kconst-1)
     time1(kconst) = time1(kconst-1) - twopi/3.0
     go to 1911
3333 texvec(1) = fixbu3(i)
     if ( texvec(1) .eq. blank ) go to 3535
     do k = 1, kconst
        k1 = iabs( node(k) )
        if ( bus(k1) .eq. fixbu3(i) ) go to 3535
1056 end do
     n1 = 3
     kconst = kconst + 1
     crest(kconst) = crest(kconst-1)
     time1(kconst) = time1(kconst-1) - twopi/3.0
     go to 1911
3535 end do
1024 if ( kconst  .eq.  0 )   go to 1034
  do i = 1, kconst
     if ( iabs( iform(i) )  .ne.  14 )   go to 1030
     if ( tstart(i)  .ge.  0.0 )   go to 1030
     go to 1050
1030 end do
1034 write (lunit6, 1041)
1041 format ( /, 106h comment ---- no sinusoidal sources requested for steady-state solution.   thus this solution is bypassed.   )
  lastov = nchain
  nchain = 12
  go to 9990
1050 continue
  next = 1
  jbrt = last
  isubs1 =  iofkor+1
  call move0 ( korder(isubs1)  , jbrt )
  call move0 ( loc(1), ntot )
  i = 1
  go to 1160
1150 i = ii + 1
1160 if ( i  .gt.  ibr )   go to 1500
  ii = i +iabs(length(i)) -1
  !      if (kodsem(i) .ne. 0  .and. imodel(i) .ne. -2
  !     1                      .and. imodel(i) .ne. -4)
  if (kodsem(i) .ne. 0  .and. imodel(i) .ge. 0) ii = i + iabs(kodebr(i)) - 1
  iswbob = 1
  j = i
  go to 1250
1220 j = j + 1
  if(iswbob.eq.0) go to 1320
  if (j .gt. ii) go to 1330
1250 k = iabs(kbus(j))
1260 if (k .eq. 0) go to 1220
  jsw = 1
  l = i
  go to 1290
1280 l = l + 1
1290 if (l .gt. ii) go to 1350
  m = iabs(kbus(l))
  if (m .eq. k  .or.  m .eq. 0)   go to 1280
  go to 1400
1320 if (j .gt. ii) go to 1150
  go to 1340
1330 iswbob = 0
  j = i
1340 k = iabs(mbus(j))
  go to 1260
1350 jsw = 2
  l = i
  go to 1370
1360 l = l + 1
1370 if (l .gt. ii) go to 1220
  m = iabs(mbus(l))
  if (m .eq. k  .or.  m .lt. 1)   go to 1360
1400 kf = loc(k)
  if (kf .eq. 0) go to 1420
  go to 1440
1420 loc(k) = next
  go to 1480
1430 isubs1 =  iofkor+kf
  kf = korder(isubs1)
1440 isubs1 =  iofkol+kf
  n1 = m - kolum(isubs1)
  if( n1 .gt. 0 )  go to 1460
  if( n1 .eq. 0 )  go to 1490
  isubs1 =  iofkol+kf
  ma = kolum(isubs1)
  isubs1 =  iofkol+kf
  kolum(isubs1)    = m
  m = ma
1460 isubs1 =  iofkor+kf
  if (korder(isubs1)    .ne. 0) go to 1430
  isubs1 =  iofkor+kf
  korder(isubs1)    = next
1480 isubs1 =  iofkol+next
  kolum(isubs1)      = m
  if (next .lt. last)   go to 1495
  kill = 220
  lstat(19) = 1495
  lstat(15) = i
  go to 9999
1495 next = next + 1
1490 go to (1280, 1360),jsw
1500 do i = 2,  ntot
     ik = 0
     il = loc(i)
1530 if (il .eq. 0) go to 1550
     ik = ik + 1
     isubs1 =  iofkor+il
     il = korder(isubs1)
     go to 1530
1550 kownt(i) = ik
  end do
  !     prevent vax i/o interrupt on kownt(1) garbage (nov, 81):
  if ( iabs(kownt(1))  .gt.  99999 )  kownt(1) = 0
  if( iprsup .ge. 3 ) write(lunit6, 1554)  ( kownt(i), i=1, ntot )
1554 format ( /, 109h (kownt(i), i=1, ntot)   in  'over9' ,  after branch table has been put into steady-state renumbering arrays.  ,/, &
       ( 1x, 20i6 ) )
  lastm1 = last -1
  do i = next, lastm1
     isubs1 =  iofkor+i
1570 korder(isubs1)   = i + 1
  end do
  isubs1 =  iofkor+last
  korder(isubs1)      = 0
  call move0 ( kode(1), ntot )
  if (kswtch .eq. 0) go to 1940
  i = 0
1631 i = i + 1
  if ( adelay(i)  .eq.  -44444. )   go to 1650
  if (tclose(i) .ge. 0.) go to 1640
  if ( adelay(i)   .eq.  44444. )   go to 1640
  go to 1650
1640 if ( i  .lt.  kswtch )   go to 1631
  go to 1940
1650 j = kmswit(i)
  ndx1 = lswtch + i
  k = kmswit(ndx1)
  if ( k .gt. j) go to 1655
  ndx2 = k
  k = j
  j = ndx2
1655 if( kode(j) .ne. 0 )  go to 1760
  if( kode(k) .ne. 0 )  go to 1690
  kode(j) = k
  kode(k) = j
  go to 1640
1690 l = k
  go to 1710
1700 l = kode(l)
1710 if( kode(l) .gt. l )  go to 1700
  m = kode(l)
1730 if( m .gt. j )  go to 1750
  l = m
  m = kode(m)
  go to 1730
1750 kode(j) = m
  kode(l) = j
  go to 1640
1760 if( kode(k) .ne. 0 )  go to 1820
  l = j
  go to 1790
1780 l = kode(l)
1790 if( kode(l) .ge. k )  go to 1810
  if( kode(l) .gt. l )  go to 1780
1810 m = kode(l)
  kode(l) = k
  kode(k) = m
  go to 1640
1820 if( kode(j) .le. j )  go to 1840
  j = kode(j)
  go to 1820
1840 if( kode(k) .le. k )  go to 1860
  k = kode(k)
  go to 1840
1860 l = j
  if( l .lt. k )  l = k
  j = kode(j)
  k = kode(k)
1870 if( j .gt. k )  go to 1910
  if( j .eq. k )  go to 1640
  kode(l) = j
  l = j
  j = kode(j)
  if (j .gt. l) go to 1870
  j = 10000
  go to 1870
1910 kode(l) = k
  l = k
  k = kode(k)
  if (k .gt. l) go to 1870
  k = 10000
  go to 1870
1940 i = 1
  if( iprsup .ge. 3 ) write(lunit6, 1943)  ( kode(k), node(k), k=1, ntot )
1943 format ( /,  84h (kode(i), node(i), i=1, ntot)   after steady-state switch-chain setup in  'over9' .   ,/, ( 1x, 20i6 ) )
  k = 0
1950 if ( iabs( iform(i) )  .ne.  14 )   go to 2040
  if ( iform(i+1)  .eq.  18 )         go to 2040
  if ( tstart(i)  .ne.  5432. )   go to 1972
  !     current source treated as voltage source for s.s. :
  j = iabs( node(i) )
  if ( iprsup  .ge.  1 ) write (lunit6, 1967)  i, k, j, kode(j), kownt(j)
1967 format ( 37h charge. i, k, j, kode(j), kownt(j) =, 5i6 )
  go to 1980
1972 if( tstart(i).ge. 0.0) go to 2040
  j = iabs(node(i))
  if( node(i) .le. 0 )  go to 2040
1980 if( kode(j) .gt. 0 )  go to 2020
  if( kode(j) .lt. 0 )  go to 2000
  kode(j) = -j
  kownt(j) = -1
  k = k + 1
  go to 2040
2000 istate = 2000
  jj = node(i)
  write (lunit6,2001)   bus(jj)
2001 format( /,  " notice. ---- two or more sinusoidal voltage sources are present on node '", a6,  "' during the steady-state  ",/, &
       14x, "solution.   As per the user's manual, these voltages will all be added together to get a total node value.")
  go to 2040
2010 j=l
2020 l = kode(j)
  kode(j) = -l
  kownt(j) = -1
  k = k + 1
  if( kode(l) .gt. 0 )  go to 2010
2040 if( i .gt. kconst )  go to 2050
  i = i +1
  if( i .le. kconst )  go to 1950
  j = 1
  go to 1980
2050 ncurr = ntot - k
  if( iprsup .ge. 3 ) write (lunit6, 2052)  ( kownt(i), i=1, ntot )
2052 format ( /,  82h (kownt(i), i=1, ntot)   in  'over9' ,  after known voltage nodes are forced last.   ,/,  ( 1x, 20i6 ) )
  i = 3 * lbus
  if( i .lt. intinf )  go to 2054
  kill = 80
  lstat(19) = 2054
  go to 9999
2054 ii = 2 * lbus
  if ( kconst  .eq.  0 )   go to 2113
  do i = 1, kconst
     if ( iabs( iform(i) )  .ne.  14 )   go to 2110
     if( tstart(i).ge. 0.0) go to 2110
     j = iabs(node(i))
     if( node(i) .gt. 0 )  go to 2110
     if( kode(j) .ge. 0 )  go to 2110
     jj = iabs(node(i))
     write (lunit6,2101)   bus(jj)
2101 format( /,  18h note. ---- node ', a6,  80h' has both voltage and current sources on it.   the current source has no effect ,/, &
          12x,  44hon the solution, then, and could be omitted.   )
     node(i) = node(i) - ii
2110 end do
2113 if ( kswtch  .eq.  0 )   go to 3000
  do ii = 1, kswtch
     if ( adelay(ii)  .eq.  -44444. )   go to 2523
     if (tclose(ii) .ge. 0.0) go to 2730
     if ( adelay(ii)  .eq.  44444. )   go to 2730
2523 i = kmswit(ii)
     ndx3 = lswtch + ii
     j = kmswit(ndx3)
     if ( i .gt. j )  i = j
2530 j = iabs(kode(i))
2540 l = loc(i)
     if (l .ne. 0)   go to 2560
     isubs1 =  iofkor+next
     nx = korder(isubs1)
     isubs1 =  iofkol+next
     kolum(isubs1)      = j
     isubs1 =  iofkor+next
     korder(isubs1)      = 0
     loc(i) = next
     next = nx
     if (kode(i) .ge. 0)   kownt(i) = 1
     go to 2610
2550 isubs1 =  iofkor+l
     l = korder(isubs1)
2560 isubs1 =  iofkol+l
     k = kolum(isubs1)
     if( k .gt. j )  go to 2590
     if( k .eq. j )  go to 2690
     isubs1 =  iofkor+l
     if (korder(isubs1)   .ne. 0)   go to 2550
     isubs1 =  iofkor+next
     nx = korder(isubs1)
     isubs1 =  iofkol+next
     kolum(isubs1)      = j
     isubs1 =  iofkor+next
     korder(isubs1)      = 0
     isubs1 =  iofkor+l
     korder(isubs1)   = next
     next = nx
     go to 2600
2590 isubs1 =  iofkor+next
     nx = korder(isubs1)
     isubs1 =  iofkol+next
     isubs2 =  iofkol+l
     kolum(isubs1)      = kolum(isubs2)
     isubs1 =  iofkor+next
     isubs2 =  iofkor+l
     korder(isubs1)      = korder(isubs2)
     isubs1 =  iofkor+l
     korder(isubs1)   = next
     isubs1 =  iofkol+l
     kolum(isubs1)   = j
     next = nx
2600 if( kownt(i)  .ne.  (-1) )  kownt(i) = kownt(i) + 1
2610 l = loc(j)
     if (l .ne. 0)   go to 2630
     isubs1 =  iofkor+next
     nx = korder(isubs1)
     isubs1 =  iofkol+next
     kolum(isubs1)      = i
     isubs1 =  iofkor+next
     korder(isubs1)      = 0
     loc(j) = next
     next = nx
     if (kode(j) .ge. 0)   kownt(j) = 1
     go to 2690
2620 isubs1 =  iofkor+l
     l = korder(isubs1)
2630 isubs1 =  iofkol+l
     k = kolum(isubs1)
     if( k .gt. i )  go to 2670
     if( k .lt. i )  go to 2650
     kill = 15
     lstat(19) = 2650
     lstat(16) = ii
     lstat(15) = next
     go to 9999
2650 isubs1 =  iofkor+l
     if (korder(isubs1)   .ne. 0)   go to 2620
     isubs1 =  iofkor+next
     nx = korder(isubs1)
     isubs1 =  iofkol+next
     kolum(isubs1)      = i
     isubs1 =  iofkor+next
     korder(isubs1)      = 0
     isubs1 =  iofkor+l
     korder(isubs1)   = next
     next = nx
     go to 2680
2670 isubs1 =  iofkor+next
     nx = korder(isubs1)
     isubs1 =  iofkol+next
     isubs2 =  iofkol+l
     kolum(isubs1)      = kolum(isubs2)
     isubs1 =  iofkor+next
     isubs2 =  iofkor+l
     korder(isubs1)      = korder(isubs2)
     isubs1 =  iofkor+l
     korder(isubs1)   = next
     isubs1 =  iofkol+l
     kolum(isubs1)   = i
     next = nx
2680 if( kownt(j)  .ne.  (-1) )  kownt(j) = kownt(j) + 1
2690 if (iabs(kode(j)) .le. j )  go to 2710
     j = iabs(kode(j))
     go to 2540
2710 i = iabs(kode(i))
     if (i .ne. j) go to 2530
2730 end do
  if( iprsup .gt. 0 ) write(lunit6, 47889)  next, last
47889 format ( /,  60h at end of  'over9' ,  just before steady-state renumbering,      , i5, &
       13h of total of  , i5,   34h cells of kolum, korder are filled)
3000 icas = 1
  lastov = nchain
  nchain = 7
  go to 9990
9999 lastov = nchain
  nchain = 51
  lstat(18) = 9
9990 if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
4568 format ( 23h  "exit  module over9." )
99999 return
end subroutine over9

!
! end of file: over9.f90
!
