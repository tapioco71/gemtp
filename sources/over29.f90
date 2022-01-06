!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over29.f90
!

!
! subroutine subr29.
!

subroutine subr29
  use blkcom
  use deck29
  use labl29
  implicit none
  character(8) :: text
  dimension text(1), array(1)
  equivalence (karray(1), text(1), array(1))
  !
  if ( iprsup .ge. 1) write (lunit6, 2371) lswtch, lsiz12, lbus
2371 format (' At top of "subr29".  lswtch, lsiz12, lbus =', 3i8)
  nvar = lswtch
  if (lsiz12 .gt. lswtch) nvar = lsiz12
  call dimens ( karray(1), nchain, trash, trash )
  do j=1, 9999, 2
     if ( karray(j)  .ne.  0 )   go to 2364
     maxo29 = karray(j+1)
     go to 2368
2364 end do
  call stoptp
2368 iofbus = 1
  iofkms  =  ( iofbus + 2 * lbus ) * nbyte(1) / nbyte(4)
  iofkde  =  iofkms  +  2 * lswtch
  iofibr  =  iofkde  +  lswtch
  iofjbr  =  iofibr  +  lsiz12
  iofake  =  ( iofjbr + lsiz12 ) * nbyte(4) / nbyte(3)  +  1
  ioftst  =  iofake  +  lswtch
  ioftcl  =  ioftst  +  lswtch
  ioftop  =  ioftcl  +  lswtch
  iofarr  =  ioftop  +  2*lswtch
  n9  =  iofarr * nbyte(3) / nbyte(4)
  if ( iprsup  .ge.  2 ) write (lunit6, 2388)  nvar,  ( nbyte(j), j=1, 4 ),  maxo29, &
       iofbus, iofkms, iofkde, iofibr, iofjbr, iofake, &
       ioftst, ioftcl, ioftop, iofarr, n9
2388 format (' After offset computation.  nvar, nbyte(1:4) =', 5i5, &
          5x, 'maxo29 =', i8, /, '   iofbus  iofkms  iofkde  iofibr  iofjbr', &
          '  iofake  ioftst  ioftcl  ioftop  iofarr', /, 1x, 10i8)
  if (n9 .lt. maxo29) go to 3462
  write (lunit6, 3457)  n9, maxo29
3457 format (/, ' Temporary error stop in "subr29" of overlay 29.', &
          '   working storage of /c29b01/ ("vardim" output) must equal at least', i7, '   words', /, ' to solve the problem, although user-dimensioning only provided for', i7, '   words (both integer figures).   Redimension the EMTP.')
  call stoptp
3462 call guts29 ( text(iofbus), karray(iofkms), karray(iofkde), karray(iofibr), karray(iofjbr), array(iofake), array(ioftst),  array(ioftcl), array(ioftop))
  if (iprsup .ge. 1) write (lunit6, 3489)  iofarr, kill
3489 format (' At exit of "subr29".  iofarr, kill =', 2i8)
  return
end subroutine subr29

!
! subroutine guts29.
!

subroutine guts29  ( bus, kmswit, kdepsw, ibrnch, jbrnch, akey, tstat, tclose, topen  )
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  character(8) :: bus
  dimension bus(1), kmswit(1), kdepsw(1), ibrnch(1), jbrnch(1)
  dimension akey(1), tstat(1), tclose(1), topen(1)
  include 'blkcom.ftn'
  include 'deck29.ftn'
  include 'labl29.ftn'
  dimension array(1)
  equivalence ( array(1), karray(1) )
  equivalence  (moncar(1),    knt),   (moncar(2),  kbase)
  equivalence  (moncar(5),  idist),   (moncar(6),  itest)
  equivalence  (moncar(9), kloaep)
  dimension  lltemp(20), mmtemp(50)
  character(8) :: text1, text2, text3
  character(8) :: text5, text6, text7
  data  text1  / 'misc. ' /
  data  text2  / 'statis' /
  data  text3  / 'tics d' /
  data  text5  / 'begin ' /
  data  text6  / 'new da' /
  data  text7  / 'bndc  ' /
  if ( iprsup  .ge.  1 ) write (lunit6, 4823)  iofbnd, kburro, istep
4823 format (' Top of "guts29".  iofbnd, kburro, istep =', 3i8)
  n18 =  maxo29 * nbyte(4)/nbyte(3)  -  iofarr
  if ( istep  .ne.  -6633 )  go to 5627
  !     begin "fault data usage" (from "reques" of overlay 1):
  !     next solve for maximum dimension x:  2(x)**2 + 4(x) - n18 = 0
  d18 = 1 + n18 / 2
  n3 = sqrtz ( d18 )  -  1.0
  d7 = n3
  n2 = sqrtz ( d7 )
  if ( iofbnd .le. 0 )  iofbnd = n2
  if ( iofbnd  .le.  n2 )  go to 4839
  write (6, 4832)  iofbnd, n2
4832 format (' Insufficient working space from "vardim" dimensioning to honor user-requested maximum number of busses equal to', i5, ' .', /, ' user can rerun with reduced maximum equal to', i5, ' .')
  call stoptp
4839 n3 = iofbnd**2
  n7 = n3**2
  write (lunit6, 4847)  iofbnd
4847 format (  1h+, 40x,   5hmax =,  i3  )
  iofray = 1
  iofsav = iofray + n7
  iofsol = iofsav + n7
  iofrhs = iofsol + n3
  iofmat = iofrhs + n3
  iofksp = iofmat + 2*n3
  iofksp = iofksp * nbyte(4) / nbyte(3) + 1
  n8  =  iofksp + n7
  lspars = 1
  if ( n8 .lt. loopss(2) ) lspars = n3
  n4 = 2 * iofbnd
  if ( iprsup .ge. 1 ) write (lunit6, 4851)  iofsav, iofsol, iofrhs, iofmat, iofksp,  n3, n4, iofbnd, lspars
4851 format (' iofsav, iofsol, iofrhs, iofmat, iofksp =', 5i6, /, ' array dimensions n3, n4, n2, lspars =', 4i6)
  call fltdat ( array(iofray), array(iofsav), array(iofsol), array(iofrhs), array(iofmat), karray(iofksp), n3, n4, iofbnd, lspars )
  go to 9800
  !     end "fault data usage" servicing;  begin monte carlo:
  !     remove possible minus sign from  'xmaxmx'  (indicating
  !     user-request for built-in random numbers of  "sandnm" ).
5627 xmaxmx = absz ( xmaxmx )
  !     reset 'aincr' if bias of  55  is present (indicating
  !     statistics just for minima?).
  if (aincr .ge. 55.0)  aincr = aincr - 55.0
  if (aincr .eq. 0.0)  aincr = .05
  knt = 0
  nstat = lstat(32)
  if ( lastov  .gt.  1 )   go to 6309
  knt = 1
  nenerg = 0
  n6 = 0
  n7 = 0
  n8 = 0
  !     read input card using cimage.
6227 call cimage
  if ( kolbeg  .gt.  0 )   go to 6263
  read (unit = abuff, fmt = 6234) (lltemp(i), i = 1, 10)
6234 format ( 10i8 )
  do i=1, 10
     if ( lltemp(i)  .eq.  0 )   go to 6239
     if ( lltemp(i)  .ne.  9999 )   go to 6236
     n8 = 9999
     go to 6243
6236 n7 = n7 + 1
     if ( n7 .gt. 50 )  go to 6270
     mmtemp(n7) = lltemp(i)
6239 end do
6243 write (kunit6, 6249)  ( lltemp(i), i=1, 9 )
6249 format ( 12h+file names.,  9i4  )
  if ( n8  .eq.  9999 )   go to 6294
  go to 6227
6263 nfrfld = 1
  !6269 call freone ( d7 )
6269 call free (d7)
  if (d7 .eq. 9999.0d0) go to 6282
  if (d7 .eq. 0.0d0) go to 6273
  n7 = n7 + 1
  if (n7 .le. 50) go to 6272
6270 write (unit = lunit6, fmt = 6271)
6271 format (' Temporary error stop in "guts29".   overflow  mmtemp(50).   Stop.')
  call stoptp
6272 mmtemp(n7) = d7
6273 go to 6269
6282 do i = 1, 9
6286 lltemp(i) = mmtemp(i)
  end do
  write (unit = kunit6, fmt = 6249) (lltemp(i), i = 1, 9)
6294 do m = 1, n7
     n13 = mmtemp(m)
     n2 = n13/100
     n5 = n13 - 100 * n2
     n3 = n5 / 10
     n4 = n5 - 10 * n3
     if (iprsup .ge. 1) write (unit = lunit6, fmt = 6299) m, n7, n13, n2, n3, n4, lunit3, lunit6, lunit9
6299 format (/, " in  'guts29' ,  ready to attach.       m      n7     n13      n2      n3      n4  lunit3  lunit6  lunit9", /, 33x, 9i8)
     lstat(14) = n2
     lstat(15) = n3
     lstat(16) = n4
     lstat(13) = 9
     call statrs
     read (unit = lunit1) ntot, neni
     read (unit = lunit1) pu
     npni = iabsz (neni)
     nenerg = nenerg + neni
     npnerg = iabsz (nenerg)
     if (iprsup .ge. 3) write (unit = lunit6, fmt = 1848) ntot, neni
1848 format (/, ' after 2-integer read from lunit1.    ntot    neni', /, 34x, 2i8)
     nexout(m) = ntot
     if (m .ne. 1)  go to 260
     write(unit = lunit9) ntot
260  read (unit = lunit1) date1, tclock, ivolt, n6, ndx1, ndx2, n16, nsmout, ioutcs, numout, n18, numnvo, numbco, nc, kswtch
     nv = nc - numbco
     if (m .ne. 1) go to 270
     write (unit = lunit9) date1, tclock, ivolt, n6, ndx1, ndx2, n16, nsmout, ioutcs, numout, n18, numnvo, numbco, nc, kswtch
270  j1 = ntot + 1
     j2 = ntot + n6
     j3 = j2 + ndx1
     j4 = j2 + ndx2
     j5 = j4 + 1
     j6 = j4 + n16
     read (unit = lunit1) (bus(i), i = 1, ntot), (bus(i),i = j1, j2), (bus(i), i = j3, j4), (bus(i), i = j5, j6)
     if (m .ne. 1) go to 275
     write (unit = lunit9) (bus(i), i = 1, ntot), (bus(i), i = j1, j2), (bus(i), i = j3, j4), (bus(i), i = j5, j6)
275  if (iprsup .ge. 1) write (unit = lunit6, fmt = 3207) nc, nsmout, ioutcs, ivolt, ndx1, ndx2, n16, numout, n18, numnvo, numbco, nc
3207 format (' Integers written on lunit9', 5x, 12i8)
     texta6(m) = bus(ntot)
     if (ivolt .ne. 1) go to 4103
     read (unit = lunit1) (m13, i = 2, ntot)
     if (m .ne. 1) go to 4104
     write (unit = lunit9) (i, i = 2, ntot)
     go to 4104
4103 if (numnvo .gt. 0) read (unit = lunit1) (karray(i), i = 1, numnvo)
     if (m .ne. 1) go to 4104
     if (numnvo .gt. 0) write (unit = lunit9)  (karray(i), i = 1, numnvo)
4104 if (nc .gt. 0) read (unit = lunit1) (ibrnch(i), i = 1, nc), (jbrnch(i), i = 1, nc)
     if (m .ne. 1) go to 285
     if (nc .gt. 0) write (unit = lunit9) (ibrnch(i), i = 1, nc), (jbrnch(i), i = 1, nc)
285  nstat = nc + numnvo
     if (m .eq. 1) lstat(32) = nstat
     ipntv(m) = nstat
     indtv(m) = kswtch
     if (m .eq. 1)  go to 310
     n2 = m - 1
     if (nexout(m) .ne. nexout(n2)) go to 9200
     if (texta6(m) .ne. texta6(n2)) go to 9200
     if (ipntv(m) .ne. ipntv(n2)) go to 9200
     if (indtv(m) .ne. indtv(n2)) go to 9200
310  do jj = 1, npni
        read (unit = lunit1) (voltbc(j), j = 1, nstat)
        if (iprsup .ge. 3) write (unit = lunit6, fmt = 1310) (voltbc(j), j = 1, nstat)
1310    format ('  voltbc  from lunit1.', /, (1x, 8e15.6))
        write (unit = lunit9) (voltbc(j), j = 1, nstat)
320  end do
330  lstat(13) = 3
     call statrs
     do i=1, kswtch
        j = lswtch + i
        read (unit = lunit1) kmswit(i), kmswit(j), akey(i), tstat(i), topen(j), kdepsw(i)
        if (m .ne. 1)  go to 340
        write (unit = lunit3) kmswit(i), kmswit(j), akey(i), tstat(i), topen(j), kdepsw(i)
340  end do
     read (unit = lunit1) kloaep
     if (m .ne. 1) go to 410
     write (unit = lunit3) kloaep
410  do jj = 1, npni
        read (unit = lunit1) ranoff, (tclose(i), i = 1, kswtch)
        write (unit = lunit3) ranoff, (tclose(i), i = 1, kswtch)
        read (unit = lunit1) ranoff, (topen(i), i = 1, kswtch)
        write (unit = lunit3) ranoff, (topen(i), i = 1, kswtch)
420  end do
1320 end do
  lstat(13) = 0
  call statrs
6309 if ( kbase .eq. 29 )  go to 1621
  !     read input card using cimage
6315 call cimage
6319 continue
  !     check for truncated statistics misc. data card:
  read (unit = abuff, fmt = 1620) bus1, bus2, bus3, bus4
1620 format ( 40x, 4a6 )
  if (bus1 .ne. text1) go to 6369
  if (bus2 .ne. text2) go to 6369
  if (bus3 .ne. text3) go to 6369
  kbase = 29
1621 continue
  read (unit = abuff, fmt = 9620) d1, d2
9620 format ( 24x, 2e8.0 )
  if ( d1 .ne. 0.0 )  aincr = d1
  if ( d2 .ne. 0.0 )  xmaxmx = d2
  write (unit = kunit6, fmt = 9630) itest, idist, aincr, xmaxmx
9630 format ('+Statistics data.', 2i7, 2f9.4)
  go to 6315
  !     check for "begin new data case" or "bndc" card:
6369 continue
  read (unit = abuff, fmt = 1605) bus1, bus2, bus3, bus4
1605 format (13a6)
  if (bus1 .eq. text5  .and.  bus2 .eq. text6) go to 16379
  if (bus1 .eq. text7  .and.  bus2 .eq. blank) go to 16379
  if (bus1 .eq. blank  .and. bus2 .eq. blank .and. kbase .eq. 28) go to 16379
  go to 6379
16379 write (unit = kunit6, fmt = 1608)
1608 format ('+New-case marker card bounds this-case data.')
  go to 9900
6379 ntemp = nstat
  if (nstat .lt. kswtch) ntemp = kswtch
  narray = iabs(nenerg) * (ntemp + 1 ) * 2
  if ( aincr .ge. 0.0 )  go to 6389
  liminc = absz(aincr)
  go to 6399
6389 xmaxmx = 5.0 * xmaxmx
  liminc = xmaxmx/aincr + 1.0
6399 iofibs = (iofarr + narray) * nbyte(3)/nbyte(4)
  iofkou = iofibs + numnvo + nc + nc
  iofpnt = iofkou + liminc
  iofnsu = iofpnt + liminc
  n2 = iofnsu + liminc - 1
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 5672) maxo29, nstat, kswtch, nenerg, numnvo, nc, liminc, n2, xmaxmx, aincr
5672 format (/, ' Before call innr29.  maxo29   nstat  kswtch  nenerg,   numnvo      nc  liminc      n2',  9x,  'xmaxmx',  10x, 'aincr', /, 20x, 8i8, 2e15.6)
  if (n2 .le. maxo29) go to 20
  kill=88
  lstat(15) = n2
  lstat(16) = maxo29
  lstat(13) = iabs(nenerg) + 3
  lstat(14) = liminc
  lstat(19) = 20
  go to 9800
20 call innr29 (array(iofarr), karray(iofibs), karray(iofkou), karray(iofpnt), karray(iofnsu), &
        bus(1), kmswit(1), kdepsw(1), ibrnch(1), jbrnch(1), akey(1), tstat(1), tclose(1), topen(1)  )
  if (kbase .eq. 29) go to 6319
  if (kbase .eq. 28) go to 6315
9200 kill = 206
  bus1 = texta6(m)
  bus2 = texta6(n2)
  lstat(11) = nexout(n2)
  lstat(12) = ipntv(n2)
  lstat(13) = indtv(n2)
  lstat(14) = nstat
  lstat(19) = 300
9800 lstat(18) = nchain
  lastov = nchain
  nchain = 51
9900 if (iprsup .ge. 1) write (unit = lunit6, fmt = 9903) kill, nchain, n18
9903 format (' Exit "guts29".  kill, nchain, n18 =', 3i6)
  return
end subroutine guts29

!
! subroutine innr29.
!

subroutine innr29 (array, ibsout, kount, kpoint, nsum, bus, kmswit, kdepsw, ibrnch, jbrnch, akey, tstat, tclose, topen)
  use blkcom
  use deck29
  use labl29
  implicit none
  dimension  array(1), ibsout(1), kount(1), kpoint(1)
  dimension  nsum(1), bus(1), kmswit(1), kdepsw(1)
  dimension  ibrnch(1), jbrnch(1), akey(1)
  dimension  tstat(1), tclose(1), topen(1)
  character(8) :: bus
  dimension  kount1(25), kount2(25), kount3(25)
  dimension  normal(25), itemp(264), texnam(10)
  equivalence  (moncar(1),    knt),   (moncar(2),  kbase)
  equivalence                         (moncar(4),    isw)
  equivalence  (moncar(5),  idist),   (moncar(6),  itest)
  equivalence  (moncar(9), kloaep)
  character(8) :: text1, text2, text3, text4, text5, text6
  character(8) :: text7, text8, texnam, aupper
  dimension aupper(264)
  data  text1  / 'misc. ' /
  data  text2  / 'statis' /
  data  text3  / 'tics d' /
  data  text4  / 'ata   ' /
  data  text5  / 'begin ' /
  data  text6  / 'new da' /
  data  text7  / 'bndc  ' /
  data  text8  / ' cont.' /
  data  texnam(1)   / 'voltag' /
  data  texnam(2)   / 'e     ' /
  data  texnam(3)   / 'curren' /
  data  texnam(4)   / 't     ' /
  data  texnam(5)   / ' power' /
  data  texnam(6)   / '      ' /
  data  texnam(7)   / 'energy' /
  data  texnam(8)   / '      ' /
  data  texnam(9)   / ' node ' /
  data  texnam(10)  / 'branch' /
  data normal(1), normal(2), normal(3),normal(4) / 0, 0, 0, 0 /
  data normal(5), normal(6), normal(7), normal(8) / 1, 2, 4, 8 /
  data normal(9),normal(10),normal(11),normal(12) / 12,18,24,28 /
  data normal(13),normal(14),normal(15),normal(16) / 30,28,24,18 /
  data normal(17), normal(18), normal(19), normal(20) / 12,8,4,2 /
  data normal(21), normal(22), normal(23), normal(24) / 1, 0, 0, 0 /
  data normal(25) / 0 /
4380 if (iprsup .ge. 1) write (unit = lunit6, fmt = 4381) isw, lastov, lunit9
4381 format (' Top of "innr29".  isw, lastov, lunit9 =', 3i8)
  llm1 = -1
  jdummy = 999
  in1 = 1
  in5 = 5
  in10 = 10
  in15 = 15
  in20 = 20
  in25 = 25
  in30 = 30
  ll264 = 264
  icnovf = 0
  npnerg = iabs(nenerg)
  total = npnerg
  icont = 0
  n22 = 0
  n9 = nv + 1
  dd1 = alog1z(fltinf)
  infexp = dd1 + 0.5
  if (isw .eq. 4444)  go to 399
  kntout = 0
  k9 = 0
  rewind lunit9
  if ( knt .eq. 1 ) go to 7
  read(lunit9)  ntot, nenerg
  go to 9
7 read (lunit9)  ntot
9 if ( jflsos .gt. 0 )  read (lunit9)  pu
!     read (lunit9)  date1, tclock, ivolt, n6, ndx1, ndx2, n16,
!     1        nsmout, ioutcs, numout, n18, numnvo, numbco, nc, kswtch
!     read (lunit9)  (bus(i), i=1, ntot),
!     1               (bus(i), i=j1, j2),
!     2      ( bus(i),i=j3,j4), (bus(i), i=j5,j6 )
!     write (*,*) ' innr29.  ready for lunit9 read.  j2 =', j2
  read (lunit9)  date1, tclock, ivolt, maxbus, ndx1, ndx2, n16, &
       nsmout, ioutcs, numout, n18, numnvo, numbco, nc, kswtch
  if ( iprsup .ge. 3) write (lunit6,*) ' innr29.  ntot, maxbus, n16 =', &
       ntot, maxbus, n16
  j2 = ntot + maxbus + n16
  read (lunit9)  ( bus(i), i=1, j2 )
  if ( iprsup  .ge.  1 ) write (lunit6, 3207)  nc,  nsmout, ioutcs, ivolt, ndx1, &
       ndx2, n16, numout, n18, numnvo, numbco, nc
3207 format ( 26h integers read from lunit9, 5x, 12i8 )
  if ( ivolt  .ne.  1 )  go to 4103
  read (lunit9)   ( m13, i=2, ntot )
  go to 4104
4103 if( numnvo .gt. 0 ) read (lunit9)  ( ibsout(i), i=1, numnvo )
4104 if( nc .gt. 0 ) read (lunit9)  (ibrnch(i), i = 1, nc), (jbrnch(i), i = 1, nc)
  nstat = lstat(32)
  n1 = 1
  n2 = nstat
  i0 = 0
35 i0 = i0 + 1
  if ( i0 .gt. npnerg )  go to 4108
  read (lunit9) ( array(j), j=n1, n2 )
  if ( iprsup .ge. 6 ) write (lunit6, 36)  n1, n2, ( array(j), j=n1, n2 )
36 format ( /,  34h read  array(i) ,   between limits ,  2i5, /, ( 1x, 10e13.4  ) )
  n2end = n2
  n1 = n1 + nstat
  n2 = n2 + nstat
  go to 35
4108 basev = pu
  n13 = 0
  go to 1600
6543 if ( m4plot .eq. 1 )  call emtspy
  icont = 0
  n22 = 0
  !     read input card using cimage
16543 call cimage
  !     next check for truncated statistics misc. data card:
1600 call move0 (itemp(1), ll264)
  read (unit = abuff, fmt = 1620) bus1, bus2, bus3, bus4
1620 format ( 40x, 4a6 )
  if ( bus1 .ne. text1 ) go to 2620
  if ( bus2 .ne. text2 ) go to 2620
  kbase = 29
  go to 9200
2620 n13 = n13 + 1
  icont = icont + 1
!!!!  n11 = (icont-1) * 11 + 1
  n11 = n22 + 1
  if ( n11 .le. ll264 )  go to 2630
  icont = 24
  icnovf = 1
  go to 4727
  !     2630 n22 = icont * 11
2630 n22 = n22 + 11
  if ( kolbeg .gt. 0 )  go to 4723
  read (unit = abuff, fmt = 7610) n8, c8, (aupper(k), k = n11, n22)
7610 format (i2,e12.3,11a6)
  if ( icont .gt. 1 )  go to 4725
  ibropt = n8
  basev = c8
  go to 4725
4723 nfrfld = 1
  kolbeg = 1
  !  call freone (d11)
  call free (d11)
  ibropt = d11
  !  call freone (basev)
  call free (basev)
  nright = -1
  nfrfld = 11
  !  call freone (d1)
  call free (d1)
  nright = 0
  do k = n11, n22
4724 aupper(k) = texta6(k)
  end do
4725 if ( aupper (n22) .ne. text8 ) go to 4727
  n22 = n22 - 1
  write (unit = kunit6, fmt = 9615)
9615 format ('+  Continued dice request.  See last card:')
  go to 16543
4727 vmaxsv = 0.0d0
  vminsv = fltinf
  iall = 0
  totmax = 0.0d0
  if (ibropt .lt. 0) go to 7612
  if (ibropt .ne. 1) go to 1725
  kill = 91
  lstat(19) = 1725
  go to 9200
1725 kend = n22
7611 ivolt = 0
  mpr = 0
  kfl = 0
  go to 1615
7612 continue
  kend = n22
  go to 7611
1615 if (ibropt .eq. 0  .or.  ibropt .eq. -1) go to 7615
  if (basev .eq. 0 )  basev = 1.0
  if (ibropt .eq. -2) n16 = 3
  if (ibropt .eq. -3) n16 = 5
  if (ibropt .eq. -4) n16 = 7
  write (kunit6, 4615) texnam(10), texnam(n16), texnam(n16 + 1)
4615 format ('+Statistical output of ', a6, 1x, a6, a1, e13.4)
  go to 2800
7615 if (basev .eq. 0.0d0) go to 7616
  if (basev .eq. pu) go to 7640
  n16 = 1
  if (ibropt .ge. 0) n17 =  9
  if (ibropt .lt. 0) n17 = 10
  write (unit = kunit6, fmt = 4615) texnam(n17), texnam(n16), texnam(n16 + 1)
  go to 2800
7616 basev = pu
  mpr=1
7620 do k=1, kend
     if ( aupper(k) .eq. blank ) go to 7625
     kfl = 1
     go to 7630
7625 end do
7630 if ( kfl .eq. 0 ) go to 7635
  n16 = 1
  if ( ibropt .ge. 0 )   n17 =  9
  if ( ibropt .lt. 0 )   n17 = 10
  write (kunit6, 4615)  texnam(n17), texnam(n16), texnam(n16+1), basev
  go to 7615
7635 write (kunit6, 7636)
7636 format('+Blank card terminating statistics output cards.')
  call interp
  kbase = 28
  if ( n13 .eq. 1 )  go to 2800
  go to 180
7640 if ( mpr  .eq.  1 )   go to 2800
  n17 = 9
  if ( ibropt .lt. 0 )  n17 = 10
  n16 = 1
  write (kunit6, 4615)  texnam(n17), texnam(n16), texnam(n16+1)
2800 numsrt = 0
  if ( icnovf .eq. 0 )  go to  801
  write (lunit6, 2625)
2625 format (' Overflow maximum limit of 24 cont. cards, current request card is ignored.')
  icnovf = 0
801 if ( ibropt .ne. 0 )  n33 = kend/2
  i = 0
  do k = 1, n22
     if ( kbase .ne. 28 )  go to 290
     k9 = k * nstat
     ibropt = 0
210  kntout = kntout + 1
     if ( kntout .gt. nstat )  go to 15071
     if ( ibropt .eq. -1 )  go to 230
     if ( ivolt .ne. 1 )  go to 220
     if ( kntout .eq. 1 )  go to 210
     if ( kntout .gt. ntot )  go to 15071
     bus1 = bus(kntout)
     go to 40
220  if ( kntout .gt. numnvo )  go to 15071
     n1 = ibsout(kntout)
     bus1 = bus(n1)
     go to 40
230  i = i + 1
     n1 = ibrnch(i)
     n2 = jbrnch(i)
     bus1 = bus(n1)
     bus2 = bus(n2)
     go to 40
290  if ( ibropt .eq. 0 )  go to 7645
     if ( k .gt. n33)  go to 6543
     !     c    k1 = k * 2 + ( icont - 2 )
     k1 = 2 * k - 1
     k2 = k1 + 1
     bus1  =  aupper(k1)
     bus2  =  aupper(k2)
     if ( bus1 .eq. blank .and. bus2 .eq. blank )  go to 5070
     n1 = 0
     n2 = 0
     do j=1, j2
        if ( n1 .eq. 0  .and.  bus(j) .eq. bus1 )  n1 = j
        if ( n2 .eq. 0  .and.  bus(j) .eq. bus2 )  n2 = j
7813 end do
     n3 = n1 * n2
     if ( n3 .gt. 1 )  go to 7835
     if ( n3 .eq. 0 ) write (lunit6, 817)  bus1, bus2
817  format (' one or more nonexistent node names in branch', ' output  "', a6, 8h"  to  ", a6, &
          '" .   This request ignored.')
     go to 5070
7835 if (ibropt .eq. -2 .or. ibropt .eq. -4) go to 1845
     do i=1, nv
        if ( ibrnch(i) .ne. n1  .or. jbrnch(i) .ne. n2 )  go to 1835
        go to 1855
1835 end do
     go to 1852
1845 do  i=n9, nc
        if ( ibrnch(i) .ne. n1  .or. jbrnch(i) .ne. n2 )  go to 1850
        go to 1855
1850 end do
1852 write (lunit6, 817)  bus1, bus2
     go to 5070
1855 kntout = numnvo + i
     go to 40
7645 bus1 = aupper(k)
     if(bus1.eq.blank) go to 5070
     do i=2,ntot
        if(bus1.eq.bus(i)) go to 7660
7650 end do
     write (lunit6,3053) bus1
3053 format(5x, 48hrequest for voltage output of nonexistent node ', a6, 18h' will be ignored.)
     go to 5070
7660 do j = 1, numnvo
        if ( ibsout(j) .eq. i )  kntout = j
1653 end do
40   vmax = absz ( array(kntout) )
     numsrt = numsrt + 1
     itemp(k) = kntout
     ainsav = aincr
     if ( aincr .lt. 0.)   vmin = absz(array(kntout))
     n2end = kntout + (npnerg - 1) * nstat
     do j = kntout, n2end, nstat
        array(j) = absz(array(j))
        if (array(j) .gt. vmax)  vmax = array(j)
        if (aincr .ge. 0.)  go to 45
        if (array(j) .lt. vmin)  vmin = array(j)
45   end do
     if ( vmaxsv .lt. vmax )  vmaxsv = vmax
     if ( vminsv .gt. vmin )  vminsv = vmin
145  if (aincr .ge. 0.)  go to 46
     ainsav = vmax/vmin/(liminc-1)
     basev = vmin
46   call move0 (kount(1), liminc)
     call move0 ( nsum(1), liminc)
     i1=0
     sxi=0.0
     sxisq=0.0
     n88 = kntout
59   i1 = i1 + 1
     if ( iprsup  .ge.  2 ) write (lunit6, 5714)  i1, npnerg, basev
5714 format ( /,  9h at 5714.,   24h      i1  npnerg        , 12x,  5hbasev  ,/,  9x, 2i8,  e15.6  )
     if ( i1 .gt. npnerg )   go to 3860
     if ( iall .eq. 0 )  go to 5724
     totmax = 0.0
     if ( k9 .eq. 0 )  go to 5730
     if ( ibropt .eq. -1 )  go to 5722
     do l6 =1, numnvo
        n1 = l6 + (i1-1) * nstat
        if (totmax .lt. array(n1) )  totmax = array(n1)
5720 end do
     go to 5744
5722 do l7 = numnvo+1, nstat
        n1 = l7 + (i1-1) * nstat
        if (totmax .lt. array(n1) )  totmax = array(n1)
5723 end do
     go to 5744
5730 do kj = 1, n22
        i = itemp(kj)
        if ( i .eq. 0 )  go to 5734
        n1 = i + (i1-1) * nstat
        if (totmax .lt.  array(n1) )  totmax = array(n1)
5734 end do
     go to 5744
5724 totmax = array(n88)
     n88 = n88 + nstat
5744 totmax = totmax / basev
     xi = totmax
     xisq = xi * xi
     sxi = sxi + xi
     sxisq = sxisq + xisq
     if ( ainsav .le. 0.0 ) go to 6543
     dex = totmax / ainsav + 1.0
     index = dex
     if (index .le. liminc )   go to 6020
     write (lunit6, 3)
     go to 46901
6020 kount(index) = kount(index) + 1
     go to 59
3860 call interp
     write (lunit6, 6426)
6426 format ( ///,1x)
     write (lunit6, 6434 )
6434 format (' ----------------------------------------------------------------------------------------------------------------------------------- ')
     if ( iall .eq. 0 )  go to 6464
     write (lunit6, 16434)
16434 format ( 1x, 13( 10hsummary    ) )
     write (lunit6, 6434 )
     if ( ibropt .ne. 0 )  go to 6439
     write (lunit6, 95)  basev
95   format(' A distribution of peak overvoltages among all output nodes on the last card having the same base voltage follows.   This', /, ' statistical distribution is for the maximum of the peaks at all of these output nodes having base voltage = ', e14.5)
     if (k9 .ne. 0)  ibropt = -1
     go to 68
6439 write (lunit6, 6440)
6440 format(' a distribution of peak values among all output branches on the last card having the same base value follows.   This')
     if ( ibropt .eq. -1 )   n16 = 1
     if ( ibropt .eq. -2 )   n16 = 3
     if ( ibropt .eq. -3 )   n16 = 5
     if ( ibropt .eq. -4 )   n16 = 7
     write (lunit6, 6798)  texnam(n16), texnam(n16+1), basev
6798 format ( 44h statistical distribution is for the maximum, 36h of the peaks at all of these output, &
          22h branches having base ,  a6, a1, 2h =, e14.5 )
     go to 68
6464 if ( ibropt .ne. 0 )  go to 2167
     write (lunit6, 65 )   bus1, basev
65   format(' statistical distribution of peak voltage at node  ', a6, "' .",  5x,  'base voltage for per-unit printout =', e14.5)
     go to 68
2167 if ( ibropt .eq. -1 )   n16 = 1
     if ( ibropt .eq. -2 )   n16 = 3
     if ( ibropt .eq. -3 )   n16 = 5
     if ( ibropt .eq. -4 )   n16 = 7
     write (lunit6, 3167)   texnam(n16), texnam(n16+1), bus1, bus2, texnam(n16), texnam(n16+1), basev
3167 format (' Statistical distribution of peak ', a6, a1, ' at branch  "', a6, '"  to  "', a6, '".    Base ', a6, a1, ' for per-unit output =', e14.5)
68   key = 0
     if ( iprsup  .ge.  1 ) write (lunit6, 69) sxi, total, sxisq, basev, vmax, ainsav
69   format (/, ' at 69   ', 6e17.5)
     term2 = sxi * sxi
     term1 = total * sxisq
     xmean1 = sxi / total
     if(total.eq.1.0) go to 100
     xvar1 = (term1-term2)/((total)*(total-1.0))
     if (xvar1 .lt. 0.0)   xvar1 = 0.0
     go to 102
100  key = 1
     xvar1 = 0.0
102  stdev1 = sqrtz(xvar1)
46900 sxifi=0.0
     sx2fi=0.0
     is = 0
     txmax = vmax/basev
     xint = txmax/ainsav + 1.0
     maxint = xint
     if ( aincr .lt. 0.0 )  go to 5010
     if(maxint .le. liminc)   go to 5010
     write (lunit6,3)
3    format (/, ' Notice ----- Overvoltage tabulation for this voltage variable can not continue, due to insufficient working ', /, &
          14x, "space.   Statistics miscellaneous data parameter  'xmaxmx'  has been exceeded by the peak per unit ", /, &
          14x, 'overvoltage (actually, exceeded by  5.0  times or more).')
     go to 46901
5010 if ( ibropt .eq. -1 )   n16 = 1
     if ( ibropt .eq. -2 )   n16 = 3
     if ( ibropt .eq. -3 )   n16 = 5
     if ( ibropt .eq. -4 )   n16 = 7
     write (lunit6, 6010)  texnam(n16), texnam(n16+1), texnam(n16), texnam(n16+1)
6010 format (2x, 'interval', 13x,  a6, a1,  10x, a6, a1,  ' in', 6x, 'frequency', 5x, 'cumulative', 12x, 'per cent')
     write (lunit6, 6018)
6018 format (4x, 'number', 9x, 'in per unit', 6x, 'physical units', 6x, '(density)', 6x, 'frequency', 5x, '.ge. current value')
     do m =1, maxint
        mm=m-1
        if(m.eq.1) mm=1
        nsum(m)=nsum(mm) + kount(m)
        r = ainsav * (m-1)
        xxi = r
        fi = kount(m)
        xifi = xxi * fi
        x2fi = xxi * xxi * fi
        sxifi=sxifi + xifi
        sx2fi=sx2fi + x2fi
        if (iprsup .ge. 1) write (lunit6, 5013)   sxifi, total, sx2fi
5013    format (/, ' at 5013', 3e17.5)
        r = r * basev
        percen = ((npnerg - nsum(m))/total)*100
        if(nsum(m).gt.0) go to 5011
        is = 1
        m1 = m
        xi1 =xxi
        r1 = r
        kont1 = kount(m)
        nsum1 = nsum(m)
        prcen1 = percen
        go to 5050
5011    if ( is .gt. 0 ) write (lunit6, 5020 ) m1, xi1, r1,  kont1, nsum1, prcen1
        is = 0
        if(nsum(m) .eq. npnerg) go to 5015
        write (lunit6,5020)  m,xxi, r, kount(m), nsum(m), percen
5020    format ( 1x, i9, f20.5, e20.6, 2i15, f20.3 )
        go to 5050
5015    mend = m
        go to 5055
     end do
5050 continue
5055 write (lunit6,5020) mend,xxi,r,kount(mend),nsum(mend),percen
     if (iprsup .ge. 1) write (lunit6, 5014)   sxifi, total, sx2fi
5014 format (/, ' at 5014', 3e17.5)
     xmean = sxifi/total
     if ( key .ne. 0 )   go to 5066
     xvar = ((total*sx2fi)-(sxifi*sxifi))/(total*(total-1.0))
     if( xvar .lt. 0.0 ) xvar = 0.0
     stddev = sqrtz(xvar)
     write (lunit6, 5064) xmean, xmean1, xvar, xvar1, stddev, stdev1
5064 format(/, ' Distribution parameters for the above data. Grouped data   ungrouped data', /, 38x, 'mean = ', 3x, f10.7, &
          6x,f10.7,/,34x, 'variance = ', 3x, f10.7, 6x, f10.7, /, 29x, 'std deviation = ', 3x, f10.7, 6x, f10.7)
     go to 46901
5066 write (lunit6, 5069)
5069 format(/, ' The number of energizations for this statistics run is 1.  Variance and standard deviation cannot be computed.')
46901 call move0 (kount(1), liminc)
     call move0 ( nsum(1), liminc )
     if ( k9 .eq. 0 )  go to 15070
     if ( iall .eq. 0 )   go to 210
     iall = 0
     if (kntout .ge. nstat)  go to 1840
     go to 210
15070 if ( iall .eq. 1 )  go to 1840
5070 if ( k .eq. n22  .and. ibropt .eq. 0 )  go to 5071
     if ( k .eq.  n33  .and. ibropt .lt. 0 )  go to 5071
     go to 840
15071 if ( ibropt .eq. -1 )  go to 5071
     kntout = kntout - 1
     if ( numsrt .ne. 1 )  go to 15073
     numsrt = 0
     go to 210
15073 numsrt = 0
     go to 15072
5071 if ( numsrt .eq. 1 )  go to 1840
15072 iall = 1
     vmax = vmaxsv
     vmin = vminsv
     go to 145
840 end do
1840 write (lunit6, 2840)
2840 format ( /, 1x )
  if ( kbase .ne. 28 )  go to 6543
180 if ( nenerg  .lt.  0 )   go to 9200
399 write (lunit6, 400)
400 format (///, 49x, 29h sample curve symbols---a,b,c)
  write (lunit6, 402)
402 format (49x, ' Theoretical curve symbol---d')
  write (lunit6, 404)
404 format (49x, ' Common curve symbol--------*')
  nsw = 0
  nssw = 0
  rewind  lunit3
  if ( iprsup  .ge.  1 ) write (6, 3447)  isw, kswtch, lswtch, ntot
3447 format (' isw, kswtch, lswtch, not =', 4i6)
  if ( isw  .eq.  4444 ) read (lunit3)  ( bus(i),  i = 1, ntot )
  if ( iprsup  .ge.  1 ) write (lunit6, 406)
406 format (" Switch vectors read from  'lunit3' . ", /, '     row   bus1     bus2', 11x, 'akey', 10x, 'tstat', 13x, 'dt', 2x, 'kdepsw')
  do i=1, kswtch
     j = lswtch + i
     read (lunit3) kmswit(i), kmswit(j), akey(i), tstat(i), topen(j), kdepsw(i)
     if ( iprsup  .ge.  1 ) write (lunit6, 1406)  i, kmswit(i), kmswit(j), akey(i), &
          tstat(i), topen(j), kdepsw(i)
1406 format ( 1x, 3i8, 3e15.6, i8 )
1405 end do
  read (lunit3) kloaep
410 nsw = nsw + 1
  if (nsw .gt. kswtch)   go to 415
  if (absz(akey(nsw))  .ne. 44444. )   go to 410
  nssw = nssw + 1
  if (kloaep .eq. 0)   go to 412
  if (kdepsw(nsw) .eq. 0)   go to 411
  tclose(nssw) = tstat(nsw) + tstat(kloaep)
  n = kdepsw(nsw)
  vartot = topen(nsw+lswtch) ** 2 + topen(n+lswtch) ** 2
1411 if (kdepsw(n) .eq. 0)   go to 414
  tclose(nssw) = tclose(nssw) + tstat(n)
  n = kdepsw(n)
  vartot = vartot + topen(n+lswtch) ** 2
  go to 1411
411 tclose(nssw) = tstat(kloaep)
  go to 414
412 if (kdepsw(nsw) .eq. 0)   go to 413
  n = kdepsw(nsw)
  tclose(nssw) = tstat(nsw) + tstat(n)
  vartot = topen(nsw+lswtch) ** 2 + topen(n+lswtch) ** 2
1412 if (kdepsw(n) .eq. 0)   go to 414
  n = kdepsw(n)
  tclose(nssw) = tclose(nssw) + tstat(n)
  vartot = vartot + topen(n+lswtch) ** 2
  go to 1412
413 tclose(nssw) = tstat(nsw)
414 topen(nssw + lswtch) = topen(nsw + lswtch)
  if ( kdepsw(nsw) .ne. 0 ) topen(nssw+lswtch) = sqrtz(vartot)
  kmswit(nssw) = kmswit(nsw)
  kmswit(nssw + lswtch) = kmswit(nsw + lswtch)
  kpoint(nssw) = nsw
  go to 410
415 do i = 1, nssw
     tstat(i) = tclose(i)
1415 end do
  if (iprsup .ge. 1) write (lunit6, 1710)  (tstat(j), j=1, nssw)
1710 format(14h tstat at 1710, (1x, 6e15.6) )
  n1 = 0
  n9 = nenerg * kswtch
  do i = 1, nenerg
     n5 = n1 + 1
     n6 = n1 + kswtch
     read (lunit3)  bias, (array(i1), i1 = n5, n6)
     m5 = n9 + n5
     m6 = n9 + n6
     read (lunit3)  bias, (array(i1), i1 = m5, m6)
     ranoff = bias
     if ( itest .eq. 1 )  ranoff = 0.0
     if ( iprsup  .ge.  1 ) write (lunit6, 4629)  kswtch, nenerg, n1, bias, (array(i1) , i1 = n5, n6), (array(i1),i1=m5,m6)
4629 format (/, ' Read logical 3.  kswtch  nenerg      n1', 11x, 'bias', /, 16x, 3i8, e15.5, /, (1x, 8e16.6 ) )
     do j = 1, nssw
        k = kpoint(j)
        if ( itest .eq. 0  .or. itest .eq. 1 )  go to 2417
        if ( itest .gt. 2 )  go to 1417
        if ( akey(k) .eq. +44444. )  go to  2410
        ranoff = 0.0
        go to 2417
2410    ranoff = bias
        go to 2417
1417    if ( akey(k) .eq. -44444. )  go to 2410
        ranoff = 0.0
2417    n7 = n1 + k
        if ( akey(k) .eq. -44444.)  n7 = n1+k+n9
        n8 = n1 + j
        array(n8) = array(n7) - ranoff
417  end do
     n1 = n1 + nssw
418 end do
  sqrt3 = 3.0
  sqrt3 = sqrtz(sqrt3)
  i = 1
419 continue
  call move0  (kount1(1), in25)
  call move0 (kount2(1), in25)
  call move0  (kount3(1), in25)
  sdt = absz(topen(i + lswtch))
  deltsc = sdt/3.0
  fclosc = nenerg * .13242/30
  deltsu = sqrt3*sdt/12.0
  fclosu = nenerg/720.0
  i2 = i + 1
  i3 = i + 2
  if (i2 .gt. nssw)   go to 430
  if (topen(i2+lswtch) .ne. topen(i+lswtch))  go to 430
  if (i3 .gt. nssw)   go to  530
  if (topen(i3+lswtch) .ne. topen(i+lswtch) )  go to 530
  go to 630
430 k1 = kmswit(i)
  k2 = kmswit(i + lswtch)
  bus1 = bus(k1)
  bus2 = bus(k2)
  write (lunit6, 432)  bus1, bus2
432 format (///, 6x, " switch '", a6, "'  to  '", a6, "'")
  if (idist .gt. 0)   go to 436
  if (topen(i + lswtch) .lt. 0.0)   go to 436
  write (lunit6, 434)   fclosc
434 format (5x, e12.4, ' Switch closings/col')
  go to 440
436 write (lunit6, 434)   fclosu
440 write (lunit6, 442)
442 format ( 17x, 8h columns)
  write (lunit6, 444)  in5, in10, in15, in20, in25, in30
444 format (10x, i1, 5(3x, i2))
  write(lunit6, 446)
446 format (5x, '+----+----+----+----+----+----+')
  go to 660
530 k1 = kmswit(i)
  k2 = kmswit(i + lswtch)
  k3 = kmswit(i2)
  k4 = kmswit(i2 + lswtch)
  bus1 = bus(k1)
  bus2 = bus(k2)
  bus3 = bus(k3)
  bus4 = bus(k4)
  write (lunit6, 532)  bus1, bus2, bus3, bus4
532 format (///, 6x, 2(" Switch '", a6, "'  to  '", a6, "'", 12x))
  if (idist .gt. 0)   go to 536
  if (topen(i + lswtch) .lt. 0.0)   go to 536
  write (lunit6, 534)   fclosc, fclosc
534 format ( 5x, 2(e12.4, ' Switch closings/col', 10x))
  go to 540
536 write (lunit6, 534)   fclosu, fclosu
540 write (lunit6, 542)
542 format (17x, ' Columns', 36x, ' Columns')
  write (lunit6, 544)  in5, in10, in15, in20, in25, in30, in5, in10, in15, in20, in25, in30
544 format( 10x, i1, 5(3x, i2), 16x, i1, 5(3x, i2))
  write (lunit6, 546)
546 format (5x, 2('+----+----+----+----+----+----+', 11x))
  go to 660
630 k1 = kmswit(i)
  k2 = kmswit(i + lswtch)
  k3 = kmswit(i2)
  k4 = kmswit(i2 + lswtch)
  k5 = kmswit(i3)
  k6 = kmswit(i3 + lswtch)
  bus1 = bus(k1)
  bus2 = bus(k2)
  bus3 = bus(k3)
  bus4 = bus(k4)
  bus5 = bus(k5)
  bus6 = bus(k6)
  write (lunit6, 632)  bus1, bus2, bus3, bus4, bus5, bus6
632 format (///, 6x, " switch '", a6, "'  to  '", a6, "'", 2(12x, " switch '", a6, "'  to  '", a6, "'"))
  if (idist.gt.0)   go to 636
  if (topen(i +lswtch) .lt. 0.0)   go to 636
  write (lunit6, 634)   fclosc, fclosc, fclosc
634 format (5x, 3(e12.4, ' Switch closings/col', 10x))
  go to 640
636 write (lunit6, 634)   fclosu, fclosu, fclosu
640 write (lunit6, 642)
642 format (17x, ' Columns', 2(36x, ' Columns'))
  write (lunit6, 644)  in5, in10, in15, in20, in25, in30, in5, in10, in15, in20, in25, in30, &
       in5, in10, in15, in20, in25, in30
644 format (10x, i1, 5(3x, i2), 2(16x, i1, 5(3x, i2)))
  write (lunit6, 646)
646 format (5x, 3('+----+----+----+----+----+----+', 11x))
660 do n = 1, nenerg
     l = (n-1) *nssw
     n3 = i + l
     twdep = array(n3) - tstat(i)
     if (kdepsw(i) .eq. 0)   go to 1660
     m = kdepsw(i)
     nn3 = m + l
     twdep = twdep - array(nn3) + tstat(m)
1660 if (idist .gt. 0)   go to 662
     if (topen(i + lswtch) .lt. 0.0)   go to 662
     l1 = 13 + (twdep + deltsc/2.) / deltsc
     kount1(l1) = kount1(l1) + 1
     if (iprsup .ge. 1) write (lunit6, 661)  n3, array(n3), l1, kount1(l1)
661  format(/, ' at 661 of subr29,n3, array(n3), l1 and kount1(l1) are', 10x, i10, e15.6, 2i10)
     go to 665
662  l1 = 13 + (twdep + deltsu/2.) / deltsu
     kount1(l1) = kount1(l1) + 1
     if (iprsup .ge. 1) write (lunit6, 661) n3, array(n3), l1, kount1(l1)
665 end do
  if (i2 .gt. nssw)   go to  700
  if (topen(i2+lswtch) .ne. topen(i+lswtch))  go to 700
  do n = 1, nenerg
     l = (n-1) * nssw
     n4 = i2 + l
     twdep = array(n4) - tstat(i2)
     if (kdepsw(i2) .eq. 0)   go to 1666
     m = kdepsw(i2)
     nn4 = m + l
     twdep = twdep - array(nn4) + tstat(m)
1666 if (idist .gt. 0)   go to 668
     if (topen(i2 + lswtch) .lt. 0.0  )   go to 668
     l2 = 13 + (twdep + deltsc/2.) / deltsc
     kount2(l2) = kount2(l2) + 1
     go to 669
668  l2 = 13 + (twdep + deltsu / 2.) / deltsu
     kount2(l2) = kount2(l2) + 1
669  if (iprsup .ge. 1) write (lunit6, 1669) n4, array(n4), l2, kount2(l2)
1669 format (' At 1669 of subr29,n4, array(n4), l2 and kount2(l2) are ', 10x, i10, e15.6, 2i10)
670 end do
  if (iprsup .ge. 1) write (lunit6, 671)  i, i2, i3, topen( i + lswtch ), topen(i2+lswtch), topen(i3+lswtch)
671 format (21h i, i2, i3 and dt are, 10x, 3i5, 3e15.6)
  if (i3 .gt. nssw)   go to 700
  if (topen(i3+lswtch) .ne. topen(i+lswtch))  go to 700
  do n=1, nenerg
     l = (n-1) * nssw
     n5 = i3 + l
     twdep = array(n5) - tstat(i3)
     if (kdepsw(i3) .eq. 0)   go to 1671
     m = kdepsw(i3)
     nn5 = m + l
     twdep = twdep - array(nn5) + tstat(m)
1671 if (idist .gt. 0)   go to 672
     if(topen(i3+lswtch) .lt. 0.0 )   go to 672
     l3 = 13 + (twdep + deltsc/2.) / deltsc
     kount3(l3) = kount3(l3) + 1
     go to 673
672  l3 = 13+ (twdep + deltsu/2.) / deltsu
     kount3(l3) = kount3(l3) + 1
673  if (iprsup .ge. 1) write (lunit6, 674) n5, array(n5), l3, kount3(l3)
674  format (' At 674 of subr29, n5, array(n5), l3 and kount3(l3) are ', 10x, i10, e15.6, 2i10)
675 end do
700 kurve1 = 1
  kurve2 = 2
  kurve3 = 3
  kurve4 = 4
  n13 = -5678
  call plotng ( llm1, n13 )
  do line = 1, 25
     kolum1 = 0
     kolum2 = 0
     kolum3 = 0
     if (idist .gt. 0)   go to 753
     if (topen(i+lswtch) .lt. 0.0)   go to 753
     kolum1 = 5 + kount1(line)/fclosc + 0.5
     kolnum = 5 + normal(line)
     if (iprsup .ge. 1) write(lunit6, 751)  line, kount1(line), normal(line)
751  format(/, ' At 751, line, kount1 and normal are', 10x, 3i10)
     go to 770
753  kolum1 = 5 + kount1(line)/fclosu + 0.5
     if (iprsup .ge. 1) write (lunit6, 1754)  line, kount1(line)
1754 format (' Line and kount1', 10x, 2i10)
     kolnum = 35
     go to 770
754  if (idist .gt. 0)   go to 758
     if (topen(i+lswtch) .lt. 0.0)   go to 758
     kolum2 = 47 + kount2(line)/fclosc + 0.5
     kolnum = 47 + normal(line)
     go to 772
758  kolum2 = 47 + kount2(line)/fclosu + 0.5
     if (iprsup .ge. 1) write (lunit6, 1758)  line, kount2(line)
1758 format (' Line and kount2', 10x, 2i10)
     kolnum = 77
     go to 772
759  if(idist .gt. 0)   go to 763
     if (topen(i+lswtch) .lt. 0.0)   go to 763
     kolum3 = 89 + kount3(line)/fclosc + 0.5
     kolnum = 89 + normal(line)
     go to 774
763  kolum3 = 89 + kount3(line)/fclosu + 0.5
     if (iprsup .ge. 1) write (lunit6, 1763)  line, kount3(line)
1763 format (' Line and kount3', 10x, 2i10)
     kolnum = 119
     go to 774
770  call  plotng (kurve4, kolnum)
     call  plotng (kurve1, kolum1)
     if (topen(i2+lswtch) .ne. topen(i+lswtch) )  go to 779
     go to 754
772  call plotng (kurve4, kolnum)
     call plotng (kurve2, kolum2)
     if ( topen(i3+lswtch) .ne. topen(i+lswtch) )  go to 779
     go to 759
774  call plotng (kurve4, kolnum)
     call plotng (kurve3, kolum3)
779  if (line .ne. 13)   go to 705
     tolmat = tstat(i)
     n13 = -4321
     call plotng ( kurve1, n13 )
733  if (i2 .gt. nssw)   go to 705
     if ( topen(i2+lswtch) .ne. topen(i+lswtch) )  go to 705
     tolmat = tstat(i2)
     call plotng ( kurve2, n13 )
738  if (i3 .gt. nssw)   go to 705
     if ( topen(i3+lswtch) .ne. topen(i+lswtch) )  go to 705
     tolmat = tstat(i3)
     call plotng ( kurve3, n13 )
705  call plotng ( jdummy, llm1 )
780 end do
  if (i2 .gt. nssw)   go to 799
  if (topen(i2+lswtch) .ne. topen(i+lswtch))   go to 799
  if (i3 .gt. nssw)   go to 809
  if (topen(i3+lswtch) .ne. topen(i+lswtch))   go to 809
  go to 813
799 write (lunit6,805)
805 format (3x, ' Time')
  if (idist .gt. 0 )   go to 821
  if (topen(i + lswtch) .lt. 0.0) go to 821
  write (lunit6, 825)  deltsc
825 format (e12.4)
  go to 826
821 write (lunit6, 825)   deltsu
826 write (lunit6, 865)
865 format (2x, ' sec/line')
  go to 991
809 write (lunit6, 810)
810 format (3x, ' Time', 37x, ' Time')
  if (idist .gt. 0 )   go to 831
  if (topen(i + lswtch) .lt. 0.0)   go to 831
  write (lunit6, 835)   deltsc, deltsc
835 format (e12.4, 30x, e12.4)
  go to 836
831 write (lunit6, 835)   deltsu, deltsu
836 write (lunit6, 875)
875 format( 2x, ' sec/line', 34x, ' sec/line')
  go to 992
813 write (lunit6, 814)
814 format (3x, ' Time', 2(37x, ' Time'))
  if (idist .gt. 0)   go to 841
  if (topen(i + lswtch)  .lt. 0.0)  go to 841
  write (lunit6, 845)   deltsc, deltsc, deltsc
845 format (e12.4, 2(30x, e12.4))
  go to 848
841 write (lunit6, 845)   deltsu, deltsu, deltsu
848 write (lunit6, 850)
850 format (2x,  ' sec/line', 2(34x, ' sec/line'))
  i = i + 3
  go to 1000
991 i = i + 1
  go to 1000
992 i = i + 2
1000 if (i .le. nssw)   go to 419
  nm1 = nenerg - 1
  do i = 1, nssw
     if ( nm1  .le.  0 )   go to 1905
     do j = 1, nm1
        m = j * nssw + i
        mm1 = m - nssw
        if (array(m)  .ge.  array(mm1))   go to 905
        temp = array(m)
        do jn = 1, j
           jn1 = jn * nssw
           l = m - jn1
           n6 = l + nssw
           if (temp .ge. array(l))   go to 904
           array(n6) = array(l)
903     end do
        array(i) = temp
        go to 905
904     array(n6) = temp
905  end do
1905 if (idist .eq. 0)   go to 900
901  abc = absz(topen(i+lswtch)) * sqrt3
     b = tstat(i) -abc
     c = tstat(i) + abc
     go to 902
900  if (topen(i + lswtch) .lt. 0.0)   go to 901
902  dn = 0.0
     fs = 0.0
     il = 1
906  if (il .le. nm1)   go to 1906
     jb = i
     go to 908
1906 do k = il, nm1
        j = k
        jb = j * nssw + i
        ja = jb - nssw
        if (array(ja) .ne. array(jb))   go to 909
907  end do
908  j = nenerg
     ja = jb
909  il = j + 1
     fi = fs
     fs = j
     fs = fs / total
     if (idist .gt. 0 )   go to 917
     if (topen(i + lswtch) .lt. 0.0 )   go to 917
     if (topen(i +lswtch) .gt. 0.0)   go to 912
     go to 929
912  z = (array(ja) - tstat(i))/topen(i+lswtch)
     az = absz(z)
     t = 1.0/(1.0 + .2316419 * az )
     d = .3989423 * expz(-z*z/2.0)
     y = 1.0 - d*t*((((1.330274 * t - 1.821256) * t + 1.781478) * t - .3565638) * t + .3193815)
     if (z .ge. 0.0)   go to 927
     y = 1.0 - y
     go to 927
917  if (array(ja) .gt. b)   go to 923
     y = 0.0
     go to 927
923  if (array(ja) .le. c)   go to 925
     y = 1.0
     go to 927
925  y = (array(ja) - b)/(2 * abc)
927  ei = absz(y - fi)
     es = absz(y - fs)
     if (dn .lt. es)   dn = es
     if (il - nenerg)  906, 908, 928
928  z = dn * sqrtz(total)
     if (iprsup .ge. 1) write (lunit6, 1927)  az, y, es, dn, z
1927 format(/, 33h at 1927, az, y, es, dn and z are, 10x, 5e15.6)
     if ( z .gt. 0.27 )   go to 2
     prob = 0.0
     go to 950
2    if ( z .ge. 1.0 )   go to 6
     c1 = expz(-1.233701/z**2)
     n1 = alog1z(c1) * 32.
     if (-n1 .lt. infexp)   go to 1928
     c8 = 0.0
     go to 5
1928 c2 = c1*c1
     c4 = c2 *c2
     c8 = c4 * c4
5    prob = (2.506628/z) * c1 * ( 1.0 + c8 * (1.0 + c8 * c8))
     go to 950
6    if (z .lt. 3.1)   go to 8
     prob = 1.0
     go to 950
8    c1 = expz(-2.0 * z * z)
     c2 = c1 * c1
     c4 = c2 *c2
     c8 = c4 *c4
     prob = 1.0 - 2.0 * (c1 -c4 + c8 *(c1-c8))
950  prob = 1.0 - prob
     write (lunit6, 940)   prob
940  format (75x, ' Kolmogorov-Smirnov test result', 10x, e15.6)
929 end do
9200 if ( isw .eq. 4444 )  kbase = 28
  lstat(18) = 29
  lastov = nchain
  nchain = 51
  return
end subroutine innr29

!
! subroutine plotng.
!

subroutine plotng (kkrv, kklm)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     universal module called only by "innr29" of overlay 29.
  !     some logic is similar to "linplt" of overlay 31.
  include 'blkcom.ftn'
  dimension  kut(131), ktsin(4), kdig(10)
  !     Burroughs: preserve local variable between module calls:
  data  kut(1)  / ' ' /
  data  ktsin(1) / 'a' /
  data  ktsin(2) / 'b' /
  data  ktsin(3) / 'c' /
  data  ktsin(4) / 'd' /
  data  kom        /  1h*  /
  data  klank      /  1h   /
  data  kdig(1)  / '1' /
  data  kdig(2)  / '2' /
  data  kdig(3)  / '3' /
  data  kdig(4)  / '4' /
  data  kdig(5)  / '5' /
  data  kdig(6)  / '6' /
  data  kdig(7)  / '7' /
  data  kdig(8)  / '8' /
  data  kdig(9)  / '9' /
  data  kdig(10) / '0' /
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 1349) kkrv, kklm
1349 format (' Top of "plotng".  kkrv, kklm =', 2i8)
  krv = kkrv
  klm = kklm
  if (klm .eq. -5678) go to 350
  if (klm .gt. 131) go to 5831
  if (klm .eq. -4321) go to 4832
  if (klm .ge. 0) go to 3652
  write (unit = lunit6, fmt = 230) kut
230 format (1x, 131a1)
350 do j = 1, 131
370  kut(j) = klank
  end do
  kut(5) = kdig(1)
  go to 9000
  !     begin code to add curve "krv" to column "klm" of kut(131)
3652 if (krv .ne. 2) go to 3664
  if (kut(47) .eq. klank) kut(47) = kdig(1)
3664 if (krv .ne. 3) go to 3671
  if (kut(89) .eq. klank) kut(89) = kdig(1)
3671 if (kut(klm) .eq. klank) go to 3688
  if (kut(klm) .eq. kdig(1)) go to 3688
  kut(klm) = kom
  go to 9000
3688 kut(klm) = ktsin(krv)
  go to 9000
  !     begin code to encode mean closing time stored in tolmat:
4832 n4 = 6
  if (krv .eq. 2) n4 = 48
  if (krv .eq. 3) n4 = 90
  n5 = n4 + 11
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4834) n4, tolmat
4834 format (' lunt12 write.  n4, tolmat =', i8, e15.3)
  rewind lunt12
  write (unit = lunt12, fmt = 4837) tolmat
4837 format (e12.3)
  rewind lunt12
  read  (unit = lunt12, fmt = 4844) (kut(j), j = n4, n5)
4844 format (12a1)
  go to 9000
  !     begin code to encode 3-digit klm on right edge of kut:
5831 if (krv .eq. 1) klm = klm - 5
  if (krv .eq. 2) klm = klm - 47
  if (krv .eq. 3) klm = klm - 89
  if (klm .lt. 1000) go to 5856
  write (unit = lunit6, fmt = 5845) krv, klm
5845 format (' Too large klm in "plotng".  Stop.   krv, klm =', 2i8, 8x, 'kut(1:131) ...', /,  1x, 131a1)
5856 n6 = klm / 100
  n1 = klm - 100 * n6
  n7 = n1 / 10
  n8 = n1 - 10 * n7
  if (n7 .eq. 0) n7 = 10
  if (n8 .eq. 0) n8 = 10
  n14 = 120
  if (krv .eq. 2) n14 = 124
  if (krv .eq. 3) n14 = 128
  kut(n14) = kdig(n6)
  kut(n14 + 1) = kdig(n7)
  kut(n14 + 2) = kdig(n8)
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 9045) krv, klm, n14, n6, n7, n8, kut
9045 format (' Encode klm.   krv, klm, n14, n6, n7, n8 =', 6i6, 8x, 'kut(1:131) ....', /, 1x, 131a1)
9000 if (iprsup .ge. 3) write (unit = lunit6, fmt = 9004) krv, klm, kut
9004 format (' Exit "plotng",  krv, klm =', 2i6, 8x, 'kut(1:131) follows .....', /, 1x, 131a1)
  return
end subroutine plotng

!
! subroutine fltdat.
!

subroutine fltdat (array, arrsav, soln, rhs, ymat, kspars, maxsq, nmax2, nmax, lspars)
  implicit none
  use blkcom
  dimension array(maxsq, maxsq), arrsav(maxsq, maxsq)
  dimension soln(maxsq), rhs(maxsq), ymat(nmax, nmax2)
  dimension kspars(lspars,lspars)
  character(8) :: texta, textb, text1, text2, chara
  character(8) :: charb, charc, charq, text80, text9
  !     Following dimensioned vector with "35" really should be
  !     variably-dimensioned to "iofbnd" user-requested size.  But
  !     it is easier to leave them fixed, bigger than ever needed:
  dimension  rthev(35),  output(35), negate(35), xthev(35)
  dimension  texta(35), textb(35), rzero(35), xzero(35)
  dimension  ichara(35), icharb(35), admit(35)
  dimension  text80(14), text1(6), text2(6), text9(1)
  data  korig  / 1hx  /
  data  kfill  / 1hf  /
  data kblank  /  1h  /
  data  chara   / 6ha        /
  data  charb   / 6hb        /
  data  charc   / 6hc        /
  data  charq   / 6h?        /
  ll35 = 35
  drive = 1.0
  limit = maxsq**2
5270 loop = 1
  call move0 ( negate(1), ll35 )
  !     enter loop over sequence networks (zero first, then positive):
5286 n13 = 2 * maxsq
  call mover0 ( ymat(1,1), n13 )
  iprsup = iprsov(nchain)
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 5306) nitmax, n13, epsiln, percen, ipunch, ipncom, bus1, bus2, bus3, bus4
5306 format ( 2i8, 2e8.0, 2i8, 4x, 4a1 )
  if (nitmax .gt. 0 .or. epsiln .gt. 0.0d0) go to 5311
  write (unit = lunit6, fmt = 5308)
5308 format ('+Blank card terminates "fdu" subcases.')
  go to 9900
5311 write (lunit6, 5314)  nitmax, n13, epsiln, percen, ipncom, bus1, bus2, bus3, bus4
5314 format (  6h+misc.,  2i5, 2e12.3, i4,  1x,  4a1  )
  if ( iprsup .le. 0 ) iprsup = n13
  if (iprsup .le. 0) iprsup = 1
  if (bus1 .ne. blank) chara = bus1
  if (bus2 .ne. blank) charb = bus2
  if (bus3 .ne. blank) charc = bus3
  if (bus4 .ne. blank) charq = bus4
  scale = percen
  ntot = 0
  !     enter loop with top at 5321 where another branch card is read:
  !     read input card using cimage.
5321 call cimage
  read (unit = abuff, fmt = 5326) k, m, d6
5326 format ( 2i8, e8.0 )
  if ( k  .le.  0     .or. m  .le.  0     .or. d6  .le.  0 )   go to 5347
  if ( k  .gt.  ntot )   ntot = k
  if ( m  .gt.  ntot )   ntot = m
  if ( ntot  .le.  nmax )  go to 5335
  write (lunit6, 5331)  ntot
5331 format ( /,  45h  @@@@@@@  overflow error stop.  generator or, 35h node number is too large.   ntot =,   i5  )
  kill = 1
  lstat(19) = 5331
  lstat(16) = 23
  go to 9900
5335 write (lunit6, 5336)  k, m, d6
5336 format (  22h+branch card.  nodes =,  2i4,  6h   x =,  e13.4 )
  d6 = 1.0 / d6
  ymat(k,m) = -d6
  ymat(m,k) = -d6
  ymat(k,k) = ymat(k,k) + d6
  ymat(m,m) = ymat(m,m) + d6
  go to 5321
5347 write (unit = lunit6, fmt = 5348)
5348 format ('+Blank card terminating line cards.')
  ntot2 = 2 * ntot
  if ( iprsup  .ge.  2 ) write (lunit6, 5354)  ntot
5354 format (   36h admittance matrix follows.   ntot =,  i3  )
  ntotsq = ntot * ntot
  ntotp1 = ntot + 1
  ntotm1 = ntot - 1
  maxcol = 2 * ntot - 1
  do j=1, ntot
     n12 = ntot + j
     ymat(j,n12) = 1.0
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 5357) l, texta(l), textb(l), rthev(l), xthev(l), n13
5357 format ( i2, 2a6, 2e10.0, i6 )
     read (unit = abuff, fmt = 5358) text1, text2
5358 format ( 2x, 12a1 )
     write (lunit6, 5360)  l, rthev(l), xthev(l), n13
5360 format ('+Thevenin.',  i6, 2e13.4, i6  )
     if ( negate(l) .eq. 0 )  negate(l) = n13
     n4 = 0
     do m=1, 6
        if ( text1(m) .ne. charq )  go to 5361
        ichara(l) = m
        n4 = n4 + 1
5361    if ( text2(m) .ne. charq )  go to 5363
        icharb(l) = m
        n4 = n4 + 1
5363 end do
     if ( n4 .ge. 2 )  go to 5368
     ichara(l) = 6
     icharb(l) = 6
     do 5366  k=2, 6
        if ( text1(k) .eq. blank   .and. text1(k-1) .eq. chara )  ichara(l) = k-1
        if ( text2(k) .eq. blank   .and. text2(k-1) .eq. chara )  icharb(l) = k-1
5366 end do
5368 admit(l) = 1.0 / xthev(l)
     if ( iprsup  .ge.  2 )  write (lunit6, 5371) l, rthev(l), xthev(l),   l, (ymat(l,l), l=1, ntot)
5371 format (' row', i2, '.   X/R =', f11.3, '   Xthev =', f11.3, '   y(', i2, ',1:ntot) =', 3e13.5, /, 1x, 10e13.5)
5375 end do
  !     Next assemble constant part of Jacobian in arrsav array.  This
  !     is top of loop over admittance level (for sequential adjustment
  !     of [y] to minimize shock to newton solution):
5379 n7 = 0
  call mover0 ( arrsav(1,1), limit )
  do kk=1, ntot
     n8 = n7
     do i=1, ntot
        n7 = n7 + 1
        if ( scale .gt. percen )   go to 5385
        soln(n7) = 0.0
        if ( i .eq. kk ) soln(n7) = admit(kk)
5385    n4 = ( i - 1 ) * ntot  +  i
        !     arrsav(n7,n4) = -gen(i)          ! df(i)/dys(i) = -vs(i)
        if ( lspars .eq. 1 )  go to 5395
        do l=1, ntotsq
5394       kspars(n7,l) = kblank
        end do
        kspars(n7,n4) = korig
5395    if ( i .eq. kk ) arrsav(n7,n7) = arrsav(n7,n7) + drive
        do j=1, ntot
           if ( j .eq. kk ) go to 5402
           arrsav(n7,n8+j) = scale*ymat(i,j)
           if ( lspars .ne. 1 ) kspars(n7,n8+j) = korig
5402    end do
5408 end do
5422 end do
  if ( iprsup  .ge.  2 ) write (lunit6, 5425)  ( soln(j), j=1, ntotsq )
5425 format (' Initial guess soln(1:ntotsq) =', 6e15.6, /, 1x,  8e15.6)
  if ( iprsup  .ge.  2 ) write (lunit6, 5437)
5437 format (/, ' Partial constant Jacobian follows.')
  do i=1, ntotsq
     if ( iprsup  .ge.  3 ) write (lunit6, 5443)  ( arrsav(i,j), j=1, ntotsq )
5443 format ( 1x, 10e13.5 )
5447 end do
  if ( iprsup  .le. 1 )  go to 5458
  if ( lspars .eq. 1 )   go to 5458
  write (lunit6, 5753)
  do k=1, ntotsq
     write (lunit6, 5755)  k,  ( kspars(k,l), l=1, ntotsq )
5457 end do
5458 niter = 0
  write (lunit6, 5443)
  go to 5723
  !     top of loop for next newton iteration:
5481 call mover ( arrsav(1,1), array(1,1), limit )
  n7 = 0
  !     inside do 5506, rhs(i) is function f(i);  newton "-" added at 5529
  do kk=1, ntot
     n8 = n7
     do i=1, ntot
        n7 = n7 + 1
        n4 = ( i - 1 ) * ntot + i
        if ( i .eq. kk ) rhs(n7) = ( soln(n7) - admit(i) ) * drive
        if ( i .ne. kk ) rhs(n7) = soln(n4) * soln(n7)
        !     enter loop to evaluate sum over j of y(i,j)*v(j) for j .ne. i:
        d13 = ymat(i,kk)*drive
        n2 = n8
        do j=1, ntot
           n2 = n2 + 1
           if ( j .eq. kk )  go to 5492
           d13 = d13 + ymat(i,j) * soln(n2)
5492    end do
        if ( iprsup .ge. 6 ) write (lunit6, 5496)  kk, i, n4, n7, n2,  scale, rhs(n7), d13
5496    format(' Below 5492.  kk, i, n4, n7, n2 =', 5i5 ,/, 'scale, rhs(n7), d13 =', 3e15.5)
        rhs(n7) = rhs(n7) + scale * d13
        if ( i .eq. kk )  go to 5502
        array(n7,n7) = array(n7,n7) + soln(n4)
        array(n7,n4) = array(n7,n4) + soln(n7)
        if ( iprsup .le. 1 )  go to 5502
        if ( lspars .eq. 1 )  go to 5502
        kspars(n7,n7) = korig
        kspars(n7,n4) = korig
5502 end do
5506 end do
  if ( iprsup  .ge.  2 ) write (lunit6, 5517)
5517 format (/, ' Final Jacobian after variable part has been added.  rhs = function evaluation (also done).')
  do i=1, ntotsq
     if (iprsup .ge. 4) write (lunit6, 5527) i, rhs(i), (array(i,j), j = 1, ntotsq)
5527 format (   i3,   7h. rhs =,   e15.6,   18h   y(i,1:ntotsq) =, 6e15.6   ,/,  1x, 8e15.6 )
5529 rhs(i) = -rhs(i)
  end do
  if ( iprsup  .le.  1 )  go to 5538
  if ( lspars  .eq.  1 )  go to 5538
  write (lunit6, 5753)
  do k=1, ntotsq
     write (lunit6, 5755)  k,  ( kspars(k,l), l=1, ntotsq )
5537 end do
5538 do i=1, ntotsq
     if ( i .eq. 1 )  go to 5563
     n6 = i - 1
     do j=1, n6
        if ( array(i,j) .eq. 0.0 )  go to 5561
        if ( lspars .eq. 1 )  go to 5541
        if ( kspars(i,j) .eq. kblank ) kspars(i,j) = kfill
5541    d3 = array(i,j)
        array(i,j) = 0.0
        n8 = j + 1
        do l=n8, ntotsq
5548       array(i,l) = array(i,l) - d3 * array(j,l)
        end do
        rhs(i) = rhs(i) - d3 * rhs(j)
        if ( iprsup  .ge.  5 ) write (lunit6, 5549) i, j, rhs(i), (array(i,l), l = 1, ntotsq)
5549    format(' zero (', i2, ',', i2, '). rhs(i) =', e15.6, '   a(i,1:n9) =', 5e15.6, /, 1x, 8e15.6)
5561 end do
5563 if ( iprsup  .ge.  4 ) write (lunit6, 5564)  i, i, array(i,i)
5564 format (' Diagonal ready to be reciprocated,  a(', i2, ',',  i2, ') =', e15.6  )
     d4 = 1.0 / array(i,i)
     array(i,i) = 1.0
     if ( i .eq. ntotsq )  go to 5574
     n8 = i + 1
     do l=n8, ntotsq
        if ( array(i,l) .eq. 0.0 )  go to 5572
        array(i,l) = d4 * array(i,l)
        if ( lspars .eq. 1 )  go to 5572
        if ( kspars(i,l) .eq. kblank ) kspars(i,l) = kfill
5572 end do
5574 rhs(i) = d4 * rhs(i)
     if ( iprsup  .ge.  3 ) write (lunit6, 5576) i, rhs(i), (array(i,j), j=1, ntotsq)
5576 format (' Final row', i3, '.  rhs =', e15.6, '   a(i,1:ntotsq) =', 5e15.6, /, 1x, 8e15.6)
5585 end do
  if ( iprsup  .ge.  2 ) write (lunit6, 5594)
5594 format (' Done with downward, begin backsubstitution.')
  i = ntotsq
5614 i = i - 1
  n8 = i + 1
  do j=n8, ntotsq
     if ( array(i,j) .eq. 0.0 )  go to 5627
     rhs(i) = rhs(i) - array(i,j) * rhs(j)
5627 end do
  if ( i .gt. 1 )  go to 5614
  if ( iprsup  .ge.  2 ) write (lunit6, 5697)  ( rhs(j), j=1, ntotsq )
5697 format (' Complete correction vector:', 6e15.6, /, 1x, 8e15.6)
  d4 = 0.0
  do j=1, ntotsq
     if ( absz( rhs(j) ) .gt. d4 ) d4 = absz( rhs(j) )
5706 soln(j) = soln(j) + rhs(j)
  end do
  if ( iprsup  .ge.  1 ) write (lunit6, 5714)  niter, d4
5714 format (' Largest correction at Newton iteration  niter =', i3, '  is  d4 =', e15.6)
  if ( iprsup  .ge.  2 ) write (lunit6, 5716)  ( soln(i), i=1, ntotsq )
5716 format (' Corrected solution estimate:', 6e15.6, /, 1x, 8e15.6)
  if ( d4 .lt. epsiln )  go to 5729
5723 niter = niter + 1
  if ( niter .le. nitmax )  go to 5481
  write (lunit6, 5727)  nitmax,  d4,  scale
5727 format (/, ' ***********  Nonconvergence error.  Iteration limit has been reached.   nitmax =', i3, /,  '   peak correction (abs) =', e15.6, '   for admittance level "scale" =', f6.3)
  go to 5778
5729 if ( absz( scale - 1.0 )  .gt.  epsiln )   go to 5782
  !     problem is solved;  only conversion from "y" to "x" remains:
  write (lunit6, 5737)
5737 format (/, ' Final generator equivalent impedances, plus confirmation of original generator parameters.', /, ' Resistance is ignored during the internal computation, and added as fixed % only at end.', /, ' X-fault is short circuit reactance computed after removal of user-flagged generators.', /,  3x, 'gen',  10x, 'X-new', 10x, 'R-new', 8x, 'X-fault', 9x, 'X-thev', 7x, 'X/R-thev')
  n18 = 0
  rewind lunit1
  !     next we add y-gen to [y], to calculate short circuit current:
  do j=1, ntot
     if ( negate(j) .ne. 0 )  go to 5734
     n8 = ( j - 1 ) * ntot  +  j
     ymat(j,j) = ymat(j,j) + soln(n8)
5734 end do
  !     next we invert total [y], to give s.s. currents as diagonals:
  do k=1, ntot
     d3 = 1.0 / ymat(k,k)
     n6 = k + 1
     do l=n6, ntot2
6716    ymat(k,l) = ymat(k,l) * d3
     end do
     do m=1, ntot
        if ( m .eq. k )  go to 6748
        d2 = ymat(m,k)
        do l=n6, ntot2
6737       ymat(m,l) = ymat(m,l) - d2 * ymat(k,l)
        end do
6748 end do
6753 end do
  !     done with ymat inversion;  output generator goodies:
  do j=1, ntot
     n8 = ( j - 1 ) * ntot  +  j
     output(j) = fltinf
     if ( soln(n8) .ne. 0.0 ) output(j) = 1.0 / soln(n8)
     d7 = output(j) / rthev(j)
     n2 = ntot + j
     write (lunit6, 5739) j, output(j), d7, ymat(j,n2), xthev(j), rthev(j)
5739 format ( 1x, i5, 4e15.6,  f15.3  )
     if ( negate(j) .eq. 1 ) output(j) = fltinf
     d7 = output(j) / rthev(j)
     if ( loop .eq. 2 )  go to 5740
     rzero(j) = d7
     xzero(j) = output(j)
     go to 5746
     !     with both z-ground and z-sky modes now known, punch branch cards:
5740 if ( negate(j) .ne. 0 )  go to 5746
     n6 = 51
     n4 = ichara(j)
     n5 = icharb(j)
     text9(1) = chara
     call packa1 ( text9(1), texta(j), n4 )
     call packa1 ( text9(1), textb(j), n5 )
     if ( ipncom .eq. 0 )  go to 5743
     write (lunit7, 5741)
5741 format ('c')
     write (lunit1, 5742)
5742 format (' c')
     n18 = n18 + 1
5743 if (ipunch .eq. 0) write (unit = lunit7, fmt = 5744) n6, texta(j), textb(j), rzero(j), xzero(j)
5744 format (i2, ',',  2(a6, ','), 6x, ',,,', e23.14, ',', e23.14, ',,,,,,')
     write (unit = lunit1, fmt = 5745) n6, texta(j), textb(j), rzero(j), xzero(j)
5745 format (i3, ',', 2(a6, ','), 6x, ',,,', e23.14, ',', e23.14, ',,,,,,')
     n18 = n18 + 1
     n4 = ichara(j)
     n5 = icharb(j)
     text9(1) = charb
     call packa1 ( text9(1), texta(j), n4 )
     call packa1 ( text9(1), textb(j), n5 )
     n6 = 52
     if ( ipunch  .eq.  0 ) write (lunit7, 5744)  n6, texta(j), textb(j), d7, output(j)
     write (lunit1, 5745)  n6, texta(j), textb(j), d7, output(j)
     n18 = n18 + 1
     text9(1) = charc
     call packa1 ( text9(1), texta(j), n4 )
     call packa1 ( text9(1), textb(j), n5 )
     n6 = 53
     if ( ipunch  .eq.  0 ) write (lunit7, 2744)  n6, texta(j), textb(j)
2744 format (i2, ',',  2(a6, ','), 6x, ',,,,,,,,,,')
     write (unit = lunit1, fmt = 2745)  n6, texta(j), textb(j)
2745 format (i3, ',', 2(a6, ','), 6x, ',,,,,,,,,,')
     n18 = n18 + 1
5746 end do
  if ( iprsup .le. 1 )  go to 5764
  if ( lspars .eq. 1 )  go to 5764
  write (lunit6, 5753)
5753 format (/, ' Sparsity pattern of triangularized Jacobian follows  .....', /, 6x, '1234567890123456789012345678901234567890123456789012345678901234567890')
  do k=1, ntotsq
     write (lunit6, 5755)  k,  ( kspars(k,l), l=1, ntotsq )
5755 format (  i3,   ' : ',   120a1  )
5756 end do
5764 loop = loop + 1
  write (lunit6, 5443)
  if ( loop .le. 2 )  go to 5286
  if ( ipunch  .ne.  0 )  go to 5778
  write (lunit6, 5767)
5767 format (' 80-column card images of coupled R-L branches that may be punched on lunit7 (if ipunch = 0).', /, &
          ' 12345678901234567890123456789012345678901234567890123456789012345678901234567890', /, &
          ' --------------------------------------------------------------------------------')
  rewind lunit1
  do j=1, n18
     read (lunit1, 5770)  text80
5770 format ( 13a6, a3 )
5773 write (lunit6, 5770)  text80
  end do
5778 go to 5270
5782 scale = scale + percen
  if ( scale .gt. 1.0 )  scale = 1.0
  go to 5379
9900 if (iprsov(nchain) .ge. 1) write (unit = lunit6, fmt = 9904) kill
9904 format (' Exit "fltdat".  kill =', i5)
  return
end subroutine fltdat

!
! subroutine statrs.
!

subroutine statrs
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     installation-dependent module of overlay 29,  called only
  !     for  "tabulate energization results"  usage.
  include 'blkcom.ftn'
  character :: c1
  character(20) :: filn20
  character :: filnam
  dimension filnam(20)
  equivalence (filn20, filnam(1))
  data c1 / ' ' /
  if ( iprsup  .ge.  1 ) write (lunit6, 1793)  ( lstat(i), i=13, 16 )
1793 format ( ' top of "statrs" .   lstat(13:16) =',  4i5  )
  write (unit = filnam(1), fmt = 1804) (lstat(i), i = 13, 16)
1804 format ( 2hst, i1, 3hlog, 3i1, 5h.dat  )
  do j = 15, 20
4256 filnam(j) = c1
  end do
  close (unit = lunit1)
  if (lstat(13) .eq. 0) go to 9000
  if (iprsup .ge. 1) write (lunit6, 1808) filnam
1808 format ( ' in "statrs", b4 open of unit lunit1 .' , 20a1)
  open (unit = lunit1, status = 'old', file = filn20, form = 'unformatted')
  write (lunit6, 1822)  lstat(13), filnam
1822 format (20x, '----  successful  open   of  lunit',  i1,  '  data', ' on disk.   permanent file name = ', 20a1, 2h .   )
9000 return
end subroutine statrs

!
! end of file over29.f90
!
