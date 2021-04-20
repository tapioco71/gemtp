!-*- mode: f90; syntax: free-format; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over5.for
!
!
!     subroutine over5.
!
subroutine over5
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'tacsar.ftn'
  common /a8sw/ a8sw(400)
  dimension akey(1), tstat(1)
  equivalence (akey(1), adelay(1)), (tstat(1), crit(1))
  equivalence (moncar(1), knt), (moncar(4), isw)
  equivalence (moncar(5), idist), (moncar(6), itest)
  equivalence (moncar(9), kloaep)
  character*8 text1, text2, text5, text6, text7
  character*8 text8, text13, text14, text15, text16
  character*8 text17
  character*80  buffer
  data  text1    / 6hname   /
  data  text2    / 6hswt001 /
  data  text5    / 6hstatis /
  data  text6    / 4htics   /
  data  text7    / 6hsystem /
  data  text8    / 4hatic   /
  data  text13   / 6hmeasur /
  data  text14   / 6hing    /
  data  text15   / 6hclosed /
  data  text16   / 6hsame   /
  data  text17   / 6htarget /
  if (iprsup .ge. 1) write (lunit6, 4567)
4567 format ( 22h begin module "over5".  )
  lstat(29) = inonl
  lstat(30) = ichar
  lstat(33) = ifdep
  lstat(34) = jst
  lstat(40) = ifsem
  lstat(41) = iadd
  call mover0 ( crest(1), lexct )
  call mover0 ( time1(1), lexct )
  call mover0 ( sfreq(1), lexct )
  call mover0 ( xk(1), lpast )
  call mover0 ( xm(1), lpast )
  omega = 0.0
  iwtent(ifdep2) = jst1
  if ( iprsup .le. 0 )  go to 6693
  if ( ifdep .eq. 0 )  go to 6693
  if ( iprsup  .ge.  9 ) write (lunit6, 6656)  ( weight(i), i=1, jst )
6656 format (' weight ---- array of weighting function storage  ', /, (1x,10e13.4))
  write (lunit6, 6648)
6648 format (/, 2x, 3hrow, 3x, 5hiskip, 2x, 6hiwtent,12x, 3heta, 11x, 4hzinf, 10x, 5htailp, 10x, 5htailq, 10x, 5htailt, 4x, &
       11hsum weights)
  do i = 1, ifdep
     n1 = 2 * i - 1
     n3 = iwtent(n1)
     n4 = iwtent(n1+2) - 1
     d1 = 0.0
     ip2 = 2 * lfdep + i
     ip3 = 2 * ip2   - i
     do j = n3, n4
        d1 = d1 + weight(j)
     end do
6664 write (lunit6, 6671)  i, iskip(i), iwtent(i), eta(i), zinf(i), con1(i), con1(ip2), con1(ip3), d1
  end do
6671 format ( 1x, i4, 2i8, 6e15.6 )
  do i = 1, ifdep
     n1 = ifdep + i
     n2 = 2 * lfdep + n1
     n3 = 4 * lfdep + n1
1983 write (lunit6, 6678)   n1, iwtent(n1), con1(n1), con1(n2), con1(n3)
  end do
6678 format ( 1x, i4, 8x, i8, 10x, 3e15.6 )
6693 continue
  !                                                reading in switch data
  kswtch=0
  kconst=0
  n4=0
  kpass = 0
  idube1 = 0
  mk = 0
  ll10 = 10
  lsw2 = lswtch + lswtch
  call move0 (indtv(1), ll10)
  call move0 ( kdepsw(1), lsw2 )
  call mover0 ( adelay(1), lsw2 )
209 if ( kill  .gt.  0 )   go to 9200
  if ( iprsup  .ge.  5 ) write (lunit6, 76)  kswtch, kloaep, (kdepsw(ip), ip = 1, kswtch)
76 format ( /,  39h read next switch card.  kswtch  kloaep  ,/, 23x,  2i8  ,/,  ( 1x,  20i6 ) )
  !     read input card using cimage.
3483 call cimage
  if ( kolbeg  .gt.  0 )   go to 3503
  read (unit = abuff(1), fmt = 35) it2, bus1, bus2, gus3, gus4, ck1, a, bus3, bus4, bus5, bus6, jdube1, j
35 format (i2,2a6,4e10.0,a6,a4,2a6,2x,2i1)
  go to 3506
3503 nfrfld = 1
  call frefld ( voltbc(1) )
  it2 = voltbc(1)
  nright = -1
  nfrfld = 2
  call freone ( d1 )
  nright = 0
  bus1 = texta6(1)
  bus2 = texta6(2)
  nfrfld = 4
  call frefld ( voltbc(1) )
  gus3 = voltbc(1)
  gus4 = voltbc(2)
  ck1 = voltbc(3)
  a   = voltbc(4)
  nright = -2
  nfrfld = 3
  call freone ( d1 )
  nright = 0
  bus3 = texta6(1)
  bus4 = texta6(2)
  bus5 = texta6(3)
  bus6 = texta6(4)
  nfrfld = 2
  call frefld ( voltbc(1) )
  jdube1 = voltbc(1)
  j      = voltbc(2)
3506 if ( bus1  .ne.  blank )   go to 3510
  if ( bus2 .eq. blank )  go to 213
3510 kswtch = kswtch + 1
  kswt2  = lswtch + kswtch
  if ( kswtch  .le.  lswtch )   go to 3517
  lstat(19) = 3510
  iprint = 6
  go to 9000
3517 crit(lswtch+1-kswtch) = 0.0
  if ( bus1  .ne.  text1 )  go to 8212
  if ( noutpr .eq. 0 )  write( lunit6, 8233 )  bus2, kswtch
8233 format (10h moniker ", a6, 21h" is for next switch , i6, 2h .)
  call namea6 ( bus2, n24 )
  namesw(kswtch) = n24
  go to 3483
8212 if ( bus1  .ne.  bus2 )   go to 3519
  kill = 224
  lstat(19) = 3517
  go to 9200
3519 call nmincr ( text2, kswtch )
  call namea6 ( text2, n24 )
  namesw(kswtch) = n24
  adelay(kswtch)=0.
  atemp = a
  n1=0
  n2=0
  do i = 1, ntot
     if (bus1.eq.bus(i)) n1=i
     if ( bus2  .eq.  bus(i) )   n2 = i
210 end do
  iprint = 1
  if (n1.ne.0) go to 211
  ntot=ntot+1
  lstat(19)=  210
  if (ntot.gt.lbus)go to 9000
  n1=ntot
  bus(n1)=bus1
211 if (n2.ne.0) go to 212
  ntot=ntot+1
  lstat(19)=211
  if (ntot.gt.lbus)go to 9000
  n2=ntot
  bus(n2)=bus2
212 isourc(kswtch)=0
  if ( j .le. 3 )  go to 4228
  j = 3
  if ( npower  .lt.  maxpe )   go to 54201
  iprint = 18
  lstat(19) = 54201
  go to 9000
54201 npower = npower + 1
  mpower = npower + maxpe
  koutvp(npower) = -( nv+1)
  koutvp(mpower) = kswtch
  if ( iprsup .ge. 2 ) write(lunit6, 4227)  npower, maxpe, koutvp(npower), koutvp(mpower)
4227 format ( /, 21h power output request   , 4i10 )
4228 continue
  if (j.lt.2) go to 224
  nv =nv+1
  ibrnch(nv) = n1
  jbrnch(nv) = n2
  if ( nv  .le.  lsiz12 )   go to 224
  iprint =11
  lstat(19) =  224
  go to 9000
224 if ( j.eq.2 ) j=0
  if ( it2 .ge. 11  .and.  it2 .le. 13 )   go to 250
  if (it2.eq.0) go to 218
  if (it2 .ne. 76)  go to 4235
  it2 = 9976
  go to 218
4235 kill = 7
  lstat(19) = 224
  lstat(16) = it2
  go to 9200
  !     *****   interpreting   ******************************************
  !     *****     diode  or  tacs-controlled valve  (type-11)  **********
  !     *****     tacs-controlled spark gap         (type-12)  **********
  !     *****     tacs-controlled ordinary switch   (type-13)  **********
250 if ( bus4 .ne. text16 )   go to 262
  if ( it2 .ne. 11 )  go to 262
  if ( idube1 .ne. 0 )  go to 257
  kill = 146
  lstat(19) = 250
  go to 9200
262 adube1 = gus3
  adube2 = gus4
  adube3 = ck1
  idube1 = 1
257 idube2 = 0
  idube3 = 0
  if ( bus5 .eq. blank )  go to 264
  if ( it2 .eq. 13 )  go to 264
  if ( ktab .eq. 0 )  go to 259
  do i = 1, ktab
     ndx1 = ilntab( klntab + i )
     if ( bus5 .eq. texvec( ndx1) )  go to 261
258 end do
259 kill = 147
  lstat(19) = 259
  go to 9200
261 idube2 = i
264 if ( bus6 .eq. blank )  go to 251
  if ( ktab .eq. 0 )  go to 265
  do i = 1, ktab
     ndx1 = ilntab( klntab + i )
     if ( bus6 .eq. texvec( ndx1) )  go to 267
266 end do
265 kill = 119
  lstat(19) = 265
  go to 9200
267 idube3 = i
251 if ( noutpr .ne. 0 )  go to 25199
  if ( it2 .ne. 11 )  go to 25100
  if ( idube2 .ne. 0 )  go to 25110
  write (kunit6, 252)
252 format (1h+,  20hdiode,  no tacs grid  )
  go to 25150
25110 ndx1 = ilntab( klntab + idube2 )
  write (kunit6, 253)  texvec( ndx1)
253 format ( 1h+, 19hvalve, tacs grid= ',  a6,  1h'  )
  go to 25150
25100 if ( it2 .ne. 12 )  go to 25120
  if ( idube2 .ne. 0 )  go to 25105
  write (kunit6, 25101)
25101 format ( 1h+, 19hgap,  no tacs spark  )
  go to 25150
25105 ndx1 = ilntab( klntab + idube2 )
  write (kunit6, 25106)  texvec( ndx1)
25106 format ( 1h+,  19hgap, tacs spark = ',  a6,  1h'  )
25150 if ( idube3 .eq. 0 )  go to 25199
  ndx1 = ilntab( klntab + idube3 )
  write (kunit6, 25151)  texvec( ndx1)
25151 format (1h+, 26x, 16h, tacs clamp = ',  a6,  1h'  )
  go to 25199
25120 if ( idube3 .ne. 0 )  go to 25125
  write (kunit6, 25121)
25121 format (1h+, 49hswitch, tacs control signal missing  ????????????)
  go to 25199
25125 ndx1 = ilntab( klntab + idube3 )
  write (kunit6, 25126)  texvec( ndx1)
25126 format ( 1h+, 31hswitch, tacs control signal = ', a6, 1h' )
25199 continue
  adelay(kswtch) = a
  energy(kswtch) = 0.0
  kdepsw(kswt2) = 8888
  if ( it2 .eq. 12 )  kdepsw(kswt2) = 8890
  if ( it2 .eq. 13 )  kdepsw(kswt2) = 8891
  topen(kswtch) = fltinf
  tclose(kswtch) = fltinf
  if ( bus3  .eq.  text15 ) tclose(kswtch) = -1.0
  kpos(kswtch) = 11
  if ( j  .gt.  0 ) kpos(kswtch) = -11
  kswtyp(kswtch) = 0
  ndx1 = lswtch + kswtch
  ndx2 = lswtch + ndx1
  ndx3 = lswtch + ndx2
  ardube(ndx1) = adube1
  ardube(ndx2) = adube2
  ardube(ndx3) = adube3
  ardube(kswtch) = -fltinf
  iardub(kswtch) = idube2
  iardub(ndx1) = idube3
  iardub(ndx2) = jdube1
  if ( a .ne. 7777. ) go to 220
  call cimage
  read (unit = abuff(1), fmt = 3355) a8sw(mk + 1), a8sw(mk + 2), a8sw(mk + 3), a8sw(mk + 8)
3355 format ( 4e16.6 )
  a8sw(mk+7) = 0.0
  mk = mk + 8
  adelay(kswtch) = 7 - mk
  if ( mk .le. 400 ) go to 220
  kill = 1
  write ( lunit6, 3434 )
3434 format( ' ***** overflow the temporary storage 300 cells for', ' vector a6sw near statement 218 at over5 *****')
  go to 9200
218 if ( bus3  .ne.  text13 )  go to 70218
  if ( bus4  .ne.  text14 )  go to 70218
  if ( noutpr .eq. 0 ) write (kunit6, 10218 )
10218 format ( 45h+permanently-closed switch used for metering.   )
  gus3 = -1.0
  gus4 = fltinf
  go to 220
70218 if ( noutpr  .eq.  0 ) write (kunit6, 36 )  gus3, gus4, ck1, a
36 format ( 8h+switch.  ,  2x, 4e10.2  )
  if ( a      .eq.  0.0   )   go to 220
  if ( atemp  .eq.  3333. )   go to 220
222 adelay(kswtch) = absz(gus4)
220 kmswit(kswtch) = n1
  ndx2 = kswtch + lswtch
  kmswit(ndx2) = n2
  if ( it2 .ge. 11  .and.  it2 .le. 13 )  go to 209
  icheck=11
  if ( a      .eq.  0.0   )   go to 216
  if ( atemp  .eq.  3333. )   go to 216
  gus4 = absz(a)
  icheck=10
  if (gus3.ge.0.) go to 216
  gus3=0.
  if ( iprsup .ge. 1 )   write(lunit6, 217)
217 format ( 16x, 22htclose changed to zero  )
216 if (j.gt.0) icheck=-icheck
  kpos(kswtch) = icheck
  kswtyp(kswtch) = it2
  tclose(kswtch) =gus3
  topen(kswtch) =gus4
  crit(kswtch) =ck1
  read (unit = abuff, fmt = 3898) buffer
3898 format ( a80 )
  n98 = index ( buffer, '   statistics' )
  n99 = index ( buffer, '   st'         )
  if ( n98 .eq. 52  .or. n99 .eq. 0 ) go to 3920
  if ( n99 .lt. 30  .or. n99 .gt. 60 ) go to 3920
  write (lunit6, 3906)  buffer
3906 format ( ' temporary error stop in  over5.   the user"s', ' data is for a monte carlo simulation,'     ,/, &
       ' but  "statistics"  in columns 55-64 of the', ' last-read data card has been misspelled.'  ,/, &
       ' the defective 80-column card image =',  a80  )
  stop
3920 if ( bus3 .ne. text5 )   go to 3260
  if ( bus4 .eq. text6 )   go to 3360
3260 if ( bus3 .ne. text7 )   go to 605
  if ( bus4 .ne. text8 )   go to 605
  sigmax = 0.0
  idist = 0
  if (nenerg .lt. 0)  go to 615
  kill = 141
  lstat(19) = 3260
  go to 9200
605 go to 209
3360 if ( nenerg .gt. 0 ) go to 615
  kill = 141
  lstat(19) = 3360
  go to 9200
615 akey(kswtch) = +44444.
  if ( a  .ne.  3333. )   go to 3344
  akey(kswtch) = -44444.
  ndx1 = lswtch + 1 - kswtch
  if ( ck1 .le. 0.0 ) go to 4674
  !     patch to allow current margin with random opening (subts1):
  if ( ndx1 .gt. kswtch )   go to 4668
  write (lunit6, 4661)  ndx1, kswtch, lswtch
4661 format (  37h overflow error stop, list 6.  random, 29h opening with current margin., 25h   ndx1, kswtch, lswtch =,  3i8  )
  call stoptp
4668 crit(ndx1) = ck1
  if ( iprsup .ge. 1 )  write (lunit6, 4669) i, kswtch, ck1
4669 format ( 5x, 16hi, kswtch, ck1 =,  2i8, e15.5  )
4674 continue
  topen(kswtch) = tclose(kswtch)
  tclose(kswtch) = -1.0
3344 kpass = kpass + 1
  if (bus5 .ne. text17)   go to 34605
  kloaep = kswtch
  go to 225
34605 n5 = 0
  n6 = 0
  do i = 1, ntot
     if (bus5 .eq. bus(i))  n5 = i
     if (bus6 .eq. bus(i))  n6 = i
240 end do
  if (n5*n6 .eq. 1)   go to 225
241 n7 = kswtch - 1
  if ( n7  .le.  0 )   go to 7245
  do i = 1, n7
     if (n5 .ne.  kmswit(i))  go to 245
     ndx3 = lswtch + i
     if (n6 .ne.  kmswit(ndx3))  go to 245
     kdepsw(kswtch) = i
     go to 225
245 end do
7245 kill = 110
  lstat(19) = 245
  lstat(15) = n5
  lstat(16) = n6
  go to 9200
225 tdns(kswtch) = ck1
  tstat(kswtch) = gus3
  if ( gus4 .lt. 0.0 )  go to  620
  ndxi = kswtch + lswtch
  topen( ndxi ) = gus4
  if (it2 .eq. 9976 .or. idist .eq. 1)  topen(ndxi) = - gus4
  go to 209
617 kill = 94
  flstat(14) = ststat
  flstat(15) = ssigma
  go to 9200
618 kill = 95
  flstat(14) = ststat
  flstat(15) = ssigma
  go to 9200
620 kill = 89
  lstat(19) = 620
  flstat(15) = tstat(kswtch)
  flstat(16) = gus4
  go to 9200
213 if ( nenerg  .eq.  0 )  go to 8299
  do i = 1, kswtch
     if ( absz ( akey(i) )  .ne.  44444. )  go to 8211
     j = i
     ststat = 0.0
     ssigma = 0.0
4632 ndxj = j + lswtch
     targ = tstat(j)
     jk = kdepsw(j)
     if ( kloaep .ne. 0 .and. jk .eq. 0 ) targ = tstat(kloaep)
     ststat = ststat + targ
     ssigma = sqrtz ( ssigma*ssigma + topen(ndxj) * topen(ndxj) )
     if (jk .eq. 0)  go to 4682
     j = jk
     go to 4632
4682 if ( ststat .ge. 0.0 )   go to 4688
     lstat(14) = i
     lstat(15) = 4433
     go to 620
4688 if ( nenerg  .lt.  0 )  go to 8222
     if ( idist .eq. 1  .or. kswtyp(i) .eq. 9976 )  go to 616
     timchk = ststat -sigmax * ssigma
     if ( timchk .lt. 0.0 ) go to 617
     go to 614
8222 abc = 1.0 - itest
     if ( kloaep .eq. 0 .or.  kloaep .eq. i )  go to 8303
     ndxk = lswtch + kloaep
     ststat = ststat - 0.5 * abc * topen(ndxk) * (tdns(kloaep) - 1.)
     abc = 1.0
8303 timchk = ststat - 0.5 * abc * topen(ndxj) * (tdns(j) - 1.)
     if ( timchk  .lt.  0. )  go to 618
     go to 614
616  timchk = ststat - 1.732050808 * ssigma
     if (timchk .lt. 0.0) go to 618
614  if ( timchk  .lt.  tenerg )  tenerg = timchk
8211 end do
8299 if ( noutpr  .eq.  0 ) write (kunit6, 54133)
54133 format (  '+blank card terminating switch cards.'  )
  call move0 ( ipntv(1), ll10 )
  if ( tenerg  .ge.  1.e+19 )   go to 1642
  if ( tenerg  .lt.  tmax )     go to 1642
  if ( nenerg  .eq.  1 )        go to 1642
  kill = 207
  lstat(19) = 1642
  go to 9200
1642 numref = 0
  if ( nenerg  .eq.  0 )   go to 640
  n1 = 1
  do i = 1, kswtch
     if ( absz ( akey(i) )   .ne.   44444. )   go to 625
     if (kdepsw(i) .ne. 0)   go to 625
     numref = numref + 1
     if (numref .le. 10)   go to 500
     kill = 112
     lstat(14) = i
     lstat(19) = 500
     n1 = kmswit(i)
     ndx4 = lswtch + i
     n2 = kmswit(ndx4)
     bus1 = bus(n1)
     bus2 = bus(n2)
     go to 9200
500  if (nenerg .gt. 0)   go to 510
     n1 = n1 *tdns(i)
510  ipntv(numref) = i
     if (kloaep .ne. i )   go to  625
     iloaep = numref
625 end do
  if ( nenerg .gt. 0 )   go to 640
  n2 = iabs(nenerg)
  if (n1 .eq. n2)   go to 640
  kill = 113
  lstat(19) = 625
  lstat(12) = n1
  j = 0
  do i = 1, kswtch
     if ( absz (akey(i) ) .ne. 44444.)   go to 520
     if ( kdepsw(i) .ne. 0 )   go to 520
     j = j+1
     ipntv(j) = tdns(i)
520 end do
  go to 9200
640 if (nenerg .le. 0)   go to 643
  if ( isw  .ne.  4444 )   go to 643
  call runtym ( d1, d2 )
  flstat(1) = flstat(1) + d1
  flstat(2) = flstat(2) + d2
  flstat(5) = flstat(5) - d1
  flstat(6) = flstat(6) - d2
  lastov = nchain
  nchain = 12
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
4568 format ( 23h  "exit  module over5." )
  go to 99999
643 lstat(21) = ntot
  lstat(26) = kswtch
  lstat(38) = npower
  if ( iprsup .ge. 2 ) write(lunit6, 8025)  ibr, inonl, kswtch, nv, npower, ntot, it, ifdep, num99, ichar, knt
8025 format ( /,  59h misc. scalars, in  'over5'  after completing switch input.   ,/,  1x, &
       88h     ibr   inonl  kswtch      nv  npower ntot      it   ifdep   num99   ichar     knt   ,/, 1x, 11i8 )
  if ( iprsup .ge. 2 ) write(lunit6, 8026)  ( koutvp(i), i = 1, npower )
8026 format ( /, 15h koutvp, koutie   ,/, ( 1x, 10i10 ) )
  ndx1 = maxpe + 1
  ndx2 = maxpe + npower
  if ( iprsup .ge. 2 ) write(lunit6, 8026)  ( koutvp(i), i=ndx1, ndx2 )
  ipunch = ipun
  iout = ioutin
  !     code for input of emtp sources is in  "over5a" .
  call over5a
  if ( ktab .eq. 0 ) go to 3636
  ndx1 = kxtcs + nuk
  xtcs(ndx1+5) = omega
  xtcs(ndx1+4) = omega / twopi
3636 if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
  if ( kill  .eq.  0 )   go to 99999
  go to 9200
9000 lstat(16) = iprint
  kill = 1
9200 lastov = nchain
  nchain = 51
  lstat(18) = 5
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
99999 return
end subroutine over5
!
!     subroutine over5a.
!
subroutine over5a
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'tacsar.ftn'
  include 'synmac.ftn'
  include 'syncom.ftn'
  include 'umdeck.ftn'
  include 'dekspy.ftn'
  dimension  ispum(1), kpen(4)
  equivalence  ( spum(1), ispum(1) )
  character*8 text12
  data  text12   / 6htyp-16 /
  ll2 = 2
  ll3 = 3
  ll4 = 4
  iread=0
  istead=0
  iprint=4
  kpu=0
  nsmout = 0
  machfl = 0
  numum = 0
  call move0 ( ksmspy(1), ll3 )
  call move0 ( kode(1), ntot )
  kode(1) = -1
  !     read input card using cimage.
310 call cimage
  if ( kill  .gt.  0 )   return
  if ( machfl.eq.1 ) go to 110
  if ( machfl.eq.2 ) go to 110
  if ( machfl.eq.3 ) machfl=0
  if ( kolbeg  .gt.  0 )   go to 313
  read (unit = abuff, fmt = 6304) n2, bus1, n1
6304 format ( i2, a6, i2 )
  if ( n2  .ne.  18 )   go to 6347
  !     special type-18 input extends preceding source to allow
  !     for ungrounded 2nd terminal and ideal transformer:
  kconst = kconst + 1
  lstat(19) = 6304
  iprint = 4
  if ( kconst  .gt.  lexct )   go to 9000
  read (unit = abuff, fmt = 6309) crest(kconst), bus2, bus3, bus4
6309 format ( 10x,  e10.0,  3a6  )
  call move0 ( kpen(1), ll4 )
  do j = 1, ntot
     if ( bus(j) .ne. bus1 )  go to 6313
     kpen(1) = j
     node(kconst) = j
     iform(kconst) = 18
6313 if ( bus(j) .ne. bus2 )  go to 6317
     kpen(2) = j
     time2(kconst) = j
6317 if ( bus(j) .ne. bus3 )  go to 6322
     kpen(3) = j
     sfreq(kconst) = j
6322 end do
  if ( kpen(1)  .ne.  0 )  go to 6325
  bus6 = bus1
  go to 6330
6325 if ( kpen(2)  .ne.  0 )  go to 6326
  bus6 = bus2
  go to 6330
6326 if ( kpen(3)  .ne.  0 )  go to 6335
  bus6 = bus3
6330 write (lunit6, 6331)  bus6
6331 format ( 35h  ++++++  warning.  ++++++  type-18, 39h source has unrecognizable node name  ", &
       a6, 21h" .   device ignored.     )
  kconst = kconst - 1
  go to 310
6335 if ( n1  .ne.  -1 )   go to 6339
  !     ungrounded current source (no transformer allowed):
  iform(kconst) = iform(kconst-1)
  crest(kconst) = -crest(kconst-1)
  node(kconst) = kpen(1)
  sfreq(kconst) = sfreq(kconst-1)
  tstart(kconst) = tstart(kconst-1)
  tstop(kconst) = tstop(kconst-1)
  time1(kconst) = time1(kconst-1)
  go to 310
6339 n14 = iabs ( node(kconst-1) )
  kode(n14) = 0
  ntot = ntot + 1
  ktrans(ntot)=-664422
  iprint = 1
  lstat(19) = 6335
  if ( ntot  .gt.  lbus )   go to 9000
  bus(ntot) = bus4
  time1(kconst) = ntot
  iprint = 2
  lstat(19) = 6343
  if ( ibr+8  .gt.  lbrnch )   go to 9000
  iprint = 3
  lstat(19) = 6343
  if ( it+8  .gt.  ldata )   go to 9000
  kpen(4) = iabs ( node(kconst-1) )
  volti(ll4) = -1.0
  volti(1) = +1.0
  volti(ll2) = +1.0 / crest(kconst)
  volti(ll3) = - volti(ll2)
  do j = 1, 4
     if ( volti(j)  .eq.  0.0 )   go to 6343
     ibr = ibr + 1
     kbus(ibr) = kpen(j)
     mbus(ibr) = ntot
     length(ibr) = 1
     nr(ibr) = it + 1
     it = it + 1
     tr(it) = volti(j)
     tx(it) = 0.0
     c(it)  = 0.0
     r(it)  = 0.0
     ibr = ibr + 1
     kbus(ibr) = kpen(j)
     mbus(ibr) = 1
     length(ibr) = 1
     nr(ibr) = it + 1
     it = it + 1
     tr(it) = -volti(j)
     tx(it) = 0.0
     c(it) = 0.0
     r(it) = 0.0
6343 end do
  go to 310
  !     end of type-18 (ideal transformer) code;  consider non-18:
6347 continue
  read (unit = abuff, fmt = 6358) a, d1, gus2, h1, h2, gus3, gus4
6358 format ( 10x, 7e10.0 )
  if ( n2  .ne.  19 )   go to 314
  !     universal machine of hian lauw --- type-19 source.
  call umoffs
  call umdata ( spum(iureac), spum(iugpar), spum(iufpar), spum(iuhist), spum(iuumrp), ispum(iunod1), ispum(iunod2), &
       ispum(iujclt), ispum(iujclo), ispum(iujtyp), ispum(iunodo), ispum(iujtmt), spum(iuhism), spum(iuomgm), &
       spum(iuomld), spum(iutham), spum(iuredu), spum(iureds), spum(iuflds), spum(iufldr), spum(iurequ), spum(iuflqs), &
       spum(iuflqr), ispum(iujcds), ispum(iujcqs), spum(iuflxd), spum(iuflxq), ispum(iunppa), spum(iurotm), ispum(iuncld), &
       ispum(iunclq), ispum(iujtqo), ispum(iujomo), ispum(iujtho), spum(iureqs), spum(iuepso), spum(iudcoe), ispum(iukcoi), &
       spum(iuvolt), spum(iuangl), ispum(iunodf), ispum(iunodm), ispum(iukumo), ispum(iujumo), spum(iuumou))
  if ( kill  .gt.  0 )   return
  go to 310
313 continue
  read (unit = abuff(1), fmt = 6304) n2, bus1
  nfrfld = 1
  nright = 6
  call freone ( h2 )
  n1 = h2
  call freone ( a )
  call freone ( d1 )
  call freone ( gus2 )
  call freone ( h1 )
  call freone ( h2 )
  call freone ( gus3 )
  call freone ( gus4 )
314 smang = gus2
  if ( n2  .gt.  0 )   go to 4568
  if ( bus1  .ne.  blank )   go to 4562
  if ( a  .eq.  0.0 )   go to 400
4562 kill = 10
  lstat(19) = 4562
  lstat(16) = n2
  return
4568 if ( n2  .ge.  50 .and. n2  .le.  59 )   go to 114
  !     check that conventional sources dont follow synch.machines
  if(numsm .eq. 0) go to 114
  kill= 177
  lstat(14) = n2
  lstat(19)= 8
  return
110 if ( kolbeg  .gt.  0 )   go to 7642
  read (unit = abuff(1), fmt = 111) bus1
111 format(2x,a6)
  go to 7646
7642 nfrfld = 1
  nright = -1
  call freone ( d1 )
  nright = 0
  bus1 = texta6(1)
7646 smang = smang - 120.
  read (unit = abuff(1), fmt = 102) smamp, smangl
102 format(10x,e10.6,10x,e10.6)
  if( smamp .eq. 0.  .and. smangl .eq. 0.  )  go  to  103
  smang = smangl
  a = smamp
103 n2 = 14
  gus2=smang
  if ( machfl  .eq.  2 )   go to 130
  if ( noutpr  .eq.  0 ) write (kunit6, 3128)  a, smang
3128 format( 19h+2nd phase of s.m. , e12.5, 2x, f8.2 )
  go to 113
130 if ( noutpr  .eq.  0 ) write (kunit6, 3129)  a, smang
3129 format( 19h+3rd phase of s.m. , e12.5, 2x, f8.2 )
  !     transfer control to synchronous machine data-input subroutine.
  call smdat ( n2mach )
  if ( kill  .gt.  0 )   return
113 machfl = machfl + 1
114 kconst = kconst + 1
  kpu = kpu+1
  if(n2 .lt. 50 ) go to 115
  if(n2 .gt. 59) go to 115
  !     this source is phase a of a synchronous machine
  numsm = numsm + 1
  j30 = 30 * numsm - 28
  if(numsm.le. lsyn) go to 125
  kill= 1
  lstat(16) = 17
  lstat(19)=125
  return
125 machfl = 1
  n2mach=n2
  n2=14
  gus3 = -9988.
115 if( kconst .le. lexct )  go to 27
  lstat(19) = 310
  go to 9000
27 if( gus4 .eq. 0.0 )  gus4 = fltinf
  if (kpu .eq. 1) pu = a
  sfreq(kconst) = d1
  if ( machfl  .le.  1 .and. noutpr  .eq.  0 ) write (kunit6, 7)  a, sfreq(kconst), gus2, gus3
7 format ( 8h+source., 2x, 4e10.2  )
  if ( n2  .eq.  17 )  go to 4259
  if ( n2  .ne.  14 )   go to 317
  if ( sfreq(kconst)  .gt.  0.0 )   go to 317
  kill = 149
  flstat(14) = sfreq(kconst)
  lstat(19) = 317
  return
317 do i = 1, ntot
     if( bus1 .eq. bus(i) )  go to 330
320 end do
  lstat(19) = 320
4258 kill = 12
  return
330 if ( n2  .le.  16 )   go to 4250
  lstat(19) = 330
  if ( n2  .lt.  60 )   go to 4249
4259 do m = 1, ktab
     ndx1 = ilntab( klntab + m )
     if ( bus1  .eq.  texvec(ndx1) )   go to 3435
3429 end do
  lstat(19) = 3429
  go to 4249
3435 sfreq(kconst) = m
  go to 362
4249 kill = 10
  lstat(16) = n2
  return
4250 if( n2 .ne. 16 )  go to 349
  if( n1 .ge. 1  .and.  n1 .le. 3 )  go to 4254
  kill = 11
  lstat(19) = 4250
  lstat(16) = n1
  return
4254 d2 = gus2 + h2
  h3=gus2*h2
  yx=gus3
  gus3=1.0+d2/delta2+h3/delta2/delta2
  gus4=(1.0+h1/delta2)*a
  ck1=sfreq(kconst)
  sfreq(kconst)=a
  time1(kconst) = gus3
  tstart(kconst)=4.0*h3/deltat/gus3
  iform(kconst)=16
  node(kconst)  =-i
  kconst=kconst+1
  lstat(19)= 4254
  if(kconst.gt.lexct) go to 9000
  iform(kconst)=n1
  !     read input card using cimage.
  call cimage
  if ( kolbeg  .gt.  0 )   go to 7653
  read (unit = abuff(1), fmt = 4255) bus1, n3, a, gus2, time1(kconst), tstart(kconst), reps, dcfreq, loutbr
4255 format ( 2x, a6, i2, 6e10.6,  9x, i1 )
  go to 7658
7653 nfrfld = 1
  call frefld ( voltbc(1) )
  n3 = voltbc(1)
  nright = -1
  call freone ( d1 )
  nright = 0
  bus1 = texta6(1)
  call frefld ( voltbc(1) )
  n3 = voltbc(1)
  call freone ( a )
  call freone ( gus2 )
  call frefld ( time1(kconst) )
  call frefld ( tstart(kconst) )
  call freone ( reps )
  call freone ( dcfreq )
  call frefld ( voltbc(1) )
  loutbr = voltbc(1)
7658 if ( noutpr  .eq.  0 ) write (kunit6, 54137)
54137 format(  26h+second dc simulator card.  )
  if ( dcfreq .eq. 0.) dcfreq = tenm3
  crest(kconst-1)=a*gus3/gus2/gus4
  gus3=gus4*gus2/gus3
  do k = 1, ntot
     if(bus1.eq.bus(k))go to 342
341 end do
  lstat(19) = 341
  go to 4258
342 n3=4
  if(yx.lt.0.) yx=0.
  ntot = ntot + 1
  bus(ntot) = text12
  kode(ntot) = 0
  if ( loutbr  .lt.  2 )   go to 4257
  nv = nv + 1
  if ( nv  .le.  lsiz12 )   go to 4256
  iprint = 11
  lstat(19) = 4256
  go to 9000
4256 ibrnch(nv) = k
  jbrnch(nv) = i
  if ( loutbr  .eq.  2 )   loutbr = 0
4257 ntot = ntot + 1
  iprint=1
  lstat(19)=  342
  if(ntot.gt.lbus) go to 9000
  bus(ntot)=trash
  kode(ntot) = 0
  iprint=6
  kswtch=kswtch+1
  if( kswtch .le. lswtch )  go to 2342
  lstat(19) = 2342
  go to 9000
2342 kmswit(kswtch) = ntot
  ndx1 = kswtch + lswtch
  kmswit(ndx1) = ntot - 1
  kpos(kswtch) = n3
  kswtyp(kswtch) = 0
  tclose(kswtch)=-yx
  topen(kswtch)= deltat +  deltat / 2.0
  adelay(kswtch)=0.
  crit(kswtch)=0.
  energy(kswtch)=0.
  isourc(kswtch)=0
  ibr = ibr + 1
  kbus(ibr) = k
  mbus(ibr) = ntot - 1
  if ( loutbr  .gt.  0 )   mbus(ibr) = -mbus(ibr)
  length(ibr) = 1
  it = it + 1
  if ( reps  .eq.  0.0 )   reps = sqrtz(epsiln)
  tr(it) = reps
  tx(it) = 0.0
  c(it) = 0.0
  nr(ibr) = -it
  k=ntot
  node(kconst)=-k
  ibr=ibr+1
  iprint=2
  if( ibr .le. lbrnch )  go to 2343
  lstat(19) = 2343
  go to 9000
2343 kbus(ibr) = i
  mbus(ibr)=k
  it=it+1
  iprint=3
  if( it .le. ldata )  go to 2344
  lstat(19) = 2344
  go to 9000
2344 iprint = 4
  nr(ibr)=-it
  length(ibr)=1
  c(it)=0.
  tr(it)=gus3
  tx(it)=0.
  if ( noutpr  .eq.  0 ) write (kunit6, 340)  gus3
340 format ( 1h+, 30x, 8hequiv r=, f10.6  )
  tstop(kconst-1)=1.0/gus3
  crest(kconst)=gus4
  tstop(kconst)=ck1
  if(n1.le.0) go to 310
  kconst=kconst+2
  lstat(19)=  340
  if(kconst.gt.lexct) go to 9000
  if ( istead .eq. 0)   omega = dcfreq * twopi
  istead=1
  node(kconst-1)=-i
  node(kconst)=-k
  iform(kconst-1) = 14
  iform(kconst) = 14
  if(n1.ne.1)go to 343
  if( ck1  .le.  tstart(kconst-2) )  go to 4265
4264 kill = 13
  lstat(19) = 4264
  return
4265 if( ck1 .lt. time1(kconst - 2 ) ) go to 4264
343 if(n1.eq.2) ck1=tstart(kconst-2)
  if(n1 .eq. 3 ) ck1 = time1(kconst - 2 )
  gus2=ck1/gus3+yx
  crest(kconst-1)= gus2
  crest(kconst)=-gus2
  tstart(kconst - 1 ) = -1.0
  tstart(kconst) = -1.0
  tstop(kconst - 1) = delta2
  tstop(kconst) = delta2
  sfreq(kconst) = dcfreq
  sfreq(kconst - 1 ) = dcfreq
  time1(kconst - 1 ) = 0.0
  time1(kconst) = 0.0
  go to 310
349 if( n2 .le. 10 )  go to 362
  if( a .ne. 0.0 )  go to 362
  kill = 73
  lstat(19) = 349
  lstat(16) = n2
  return
362 if( n1 .ge. 0 )  go to 336
  i=-i
331 node(kconst)=i
  k13 = j30 + machfl
  ismdat( k13 ) = i
  if( n2 .gt. 0 )  go to 4266
  lstat(19) = 331
  go to 4249
4266 tstop(kconst) = gus4
  iform(kconst)=n2
  crest(kconst)=a
  if(n2.eq.14) go to 332
  if(gus3.lt.0.) gus3=0.
  if(n2.le.10  .and.  crest(kconst) .eq. 0.0)iread=1
  if(n2.eq.13)sfreq(kconst)=(h1-a)/(h2-gus2)
  go to 333
332 gus2 = gus2 * twopi / 360.
  if(h1.gt. 0.) gus2=gus2*sfreq(kconst)*360.0
  if(gus3.ge.0.) go to 333
  if( omega .eq. 0.0    .and.    istead .eq. 0 ) omega = twopi * sfreq(kconst)
  istead=1
333 time1(kconst) = gus2
  tstart(kconst)=gus3
  if(i.lt.0) go to 310
  k=0
334 k=k+1
  if(k.eq.kconst) go to 310
  if(node(k).eq.i) go to 335
  go to 334
335 iform(kconst)=-n2
  go to 310
336 kode(i) = -i
  go to 331
400 if ( noutpr  .eq.  0 ) write (kunit6, 54139)
54139 format(  37h+blank card terminating source cards.   )
  if( numsm .gt. 0 )    mfirst = ismdat( 24 )
  call interp
  if ( kconst  .le.  1 )   go to 4145
  do j = 1, kconst
     if ( iabs ( iform(j) )  .ne.  17 )   go to 4142
     iform(j) = 17
     crest(j) = crest(j+1)
     write (lunit6, 6616)  j, crest(j)
6616 format ( 32h type-17 reassign. j, crest(j) =, i5, e15.4 )
4142 end do
4145 if ( t  .gt.  0.0 )   go to 5737
  if ( istead  .ne.  0 )   go to 5737
  if ( inonl .le. 0 )  go to 5737
  do j = 1, inonl
     if ( nltype(j)  .ne.  -96 )   go to 5732
     kill = 203
     lstat(19) = 5732
     return
5732 end do
5737 if ( ncomp  .gt.  0 )   go to 419
  if( inonl .gt. num99 )  ncomp = 1
419 if ( fmaxfs  .eq.  0.0 )   go to 447
  tmax = 0.0
  n1 = 0
  n2 = 0
  if ( kconst  .eq.  0 )   go to 433
  do j = 1, kconst
     if ( iform(j)  .ne.  14 )   go to 431
     n1 = n1 + 1
     if ( tstart(j)  .lt.  0.0 )   go to 424
     n2 = n2 + 1
424  sfreq(j) = fminfs
431 end do
  if ( n1  .gt.  n2 )   go to 437
433 kill = 194
  lstat(13) = n1
  lstat(14) = n1 - n2
  lstat(19) = 433
  return
437 if ( n2  .eq.  0 )   go to 447
  write (lunit6, 441)  kconst, n1, n2
441 format ( /,  57h notice  ----  this  'frequency scan'  data case contains,  i5,  25h   emtp sources, of which,  i5,  &
       6h   are    ,/, 15x, &
       102hsinusoidal (type-code 14).   But of these sinusoidal sources, not all have field  'tstart'  of columns      ,/, &
       15x, 98h61-70 punched negative (which would indicate that such a source is present during the steady-state          , &
       /, 15x, 31hphasor solutions).   There were,  i5, 60h   exceptions of this sort.   The user is reminded that only       )
  write (lunit6, 442)
442 format (15x, &
         "sinusoidal sources which have negative  'tstart' will affect the EMTP solution.   By definition of", &
         /, 15x, &
         99h 'frequency scan' ,   this data case will involve only steady-state phasor solutions (there will be         ,/, &
         15x,           26hno transient simulations).         )
447 if ( ibr  .gt.  0 )   go to 2436
  !     degenerate case without linear branches; add infinite r:
  write (lunit6, 2430)
2430 format (  /,  39h this is a degenerate data case without, 35h any linear branches.  add infinite, &
       34h resistance from node 2 to ground. ,/ )
  ibr = 1
  kbus(1) = 2
  mbus(1) = 1
  length(1) = 1
  nr(1) = -1
  it = 1
  tr(1) = 1.e10
  tx(1) = 0.0
  r(1) = 0.0
  c(1) = 0.0
2436 if (iprsup .lt. 3)   go to 1416
  write (lunit6, 12416)
12416 format ( /,  21h switch table vectors  ,/, 56h     row   bus1     bus2    kpos  kdepsm  isourc  kswtyp, &
       9x, 6htclose, 9x, 6hadelay, 10x, 5htopen, 11x, 4hcrit )
  if ( kswtch  .le.  0 )   go to 2428
  do i = 1, kswtch
     ndx2 = lswtch + i
2406 write (lunit6, 2426) i, kmswit(i),kmswit(ndx2), kpos(i), kdepsw(i),isourc(i),kswtyp(i),tclose(i),adelay(i), &
          topen(i), crit(i)
  end do
2426 format (7i8,  4e15.6 )
2428 write (lunit6, 13416)  (i, kbus(i), mbus(i), nr(i), length(i), kodebr(i), kodsem(i), i = 1, ibr)
13416 format (/, 21h branch table vectors,/,  70h       row      kbus      mbus        nr    length    kodebr    kodsem,/, ( 7i10))
  write (lunit6, 14416)  (i, iform(i), node(i), crest(i), time1(i), tstart(i), sfreq(i), i = 1, kconst)
14416 format (/, 21h source table vectors,/, 30h       row     iform node, 15x, 5hcrest, 15x, 5htime1, 14x, 6htstart,15x,5hsfreq,&
       /, (3i10, 4e20.10)   )
  write (lunit6,2584) (i, tr(i), tx(i), r(i), c(i), i=1, it)
2584 format ( /, 40h rows 1 through it of parameters follow: ,/,    7x,  3hrow,  13x,  2htr,  13x,  2htx,  14x, &
       1hr,  14x,  1hc  ,/,  ( i10,  4e15.5  ) )
1416 if (iprsup .ge. 1) write (lunit6, 1417)  kconst, ibr, inonl, kswtch, istead, omega, xopt, copt, twopi
1417 format ( /,  22h normal exit "over5a"., 40h  kconst     ibr   inonl  kswtch  istead, &
       8x, 5homega, 9x, 4hxopt, 9x, 4hcopt, 8x, 5htwopi,  /, 22x, 5i8, 4e13.4  )
  lastov = nchain
  nchain = nchain + 1
  go to 9999
9000 lstat(16) = iprint
  kill = 1
9999 return
end subroutine over5a
!
! subroutine umoffs.
!
subroutine umoffs
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     overlay-5  u.m.  module called by "over5a".
  include 'blkcom.ftn'
  include 'umdeck.ftn'
  if ( nclfix  .gt.  0 )   go to 1758
  nclfix = 20
  numfix = 3
  iotfix = 50
  ibsfix = 60
1758 if ( iprsup  .ge.  1 ) write (lunit6, 1853)   (nbyte(i), i=1, 4), nclfix, numfix, iotfix, ibsfix
1853 format ( /,  18h begin  "umoffs" ., 32h  nbyte1  nbyte2  nbyte3  nbyte4, &
       32h  nclfix  numfix  iotfix  ibsfix  ,/, 18x,  10i8  )
  d5 = nbyte(4)
  d5 = d5 / nbyte(3)
  iureac = 1
  !     step over all coil-table vectors next.
  iugpar = iureac + nclfix
  iufpar = iugpar + nclfix
  iuhist = iufpar + nclfix
  iuumrp = iuhist + nclfix
  iunod1 = ( iuumrp + nclfix ) / d5  +  1
  iunod2 = iunod1 + nclfix
  iujclt = iunod2 + nclfix
  iujclo = iujclt + nclfix
  iujtyp = iujclo + nclfix
  !     step over all machine-table vectors next.
  iunodo = iujtyp + numfix
  iujtmt = iunodo + numfix
  iuhism = ( iujtmt + numfix ) * d5  +  1
  iuomgm = iuhism + numfix
  iuomld = iuomgm + numfix
  iutham = iuomld + numfix
  iuredu = iutham + numfix
  iureds = iuredu + numfix
  iuflds = iureds + numfix
  iufldr = iuflds + numfix
  iurequ = iufldr + numfix
  iuflqs = iurequ + numfix
  iuflqr = iuflqs + numfix
  iujcds = ( iuflqr + numfix ) / d5  +  1
  iujcqs = iujcds + numfix
  iuflxd = ( iujcqs + numfix ) * d5  +  1
  iuflxq = iuflxd + numfix
  iunppa = ( iuflxq + numfix ) / d5  +  1
  iurotm = ( iunppa + numfix ) * d5  +  1
  iuncld = ( iurotm + numfix ) / d5  +  1
  iunclq = iuncld + numfix
  iujtqo = iunclq + numfix
  iujomo = iujtqo + numfix
  iujtho = iujomo + numfix
  iureqs = ( iujtho + numfix ) * d5  +  1
  iuepso = iureqs + numfix
  iudcoe = iuepso + numfix
  iukcoi = ( iudcoe + numfix ) / d5  +  1
  iuvolt = ( iukcoi + numfix ) * d5  +  1
  iuangl = iuvolt + numfix
  iunodf = ( iuangl + numfix ) / d5  +  1
  iunodm = iunodf + numfix
  iukumo = iunodm + numfix
  !     step over all output-table vectors next.
  iujumo = iukumo + iotfix
  iuumou = ( iujumo + iotfix ) * d5  +  1
  !     finally, step over last vector, and check for overflow.
  n5     = iuumou + ibsfix
  lstat(45) = n5
  if ( n5  .le.  lspcum )   go to 3458
  !     following is temporary message, to be moved to error overlays
  !     later, when we have time.
  write (lunit6, 3451)  n5, lspcum, nclfix, numfix, iotfix, ibsfix
3451 format ( /,  35h overflow error stop in  "umoffs" . , 36h   insufficient total working space.  ,/, &
       1x,  10i8  )
  call stoptp
3458 if ( iprsup  .ge.  1 ) write (lunit6, 3465)  n5, lspcum
3465 format ( /,  32h exit  "umoffs" .   n5, lspcum =,  2i6,/)
  if ( noutpr  .eq.  0 ) write (kunit6, 3472)  n5
3472 format ( 40h+u.m. data begins.  list-25 cells used =, i5 )
  return
end subroutine umoffs
!
!     subroutine umdata.
!
subroutine umdata(reacl, gpar, fpar, hist,umcurp, nodvo1, nodvo2, jcltac, jclout,jtype,nodom, jtmtac, histom, omegm, omold, &
     thetam, reamdu, reamds, flxds, flxdr, reamqu, flxqs, flxqr, jcdsat, jcqsat, flxd, flxq, nppair, rotmom, ncld, nclq, &
     jtqout, jomout, jthout, reamqs, epsom, dcoef, kcoil, voltum, anglum, nodfum, nodmum, kumout, jumout, umoutp)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension  reacl(1), gpar(1), fpar(1), hist(1),umcurp(1)
  dimension  nodvo1(1), nodvo2(1), jcltac(1), jclout(1)
  dimension  jtype(1), nodom(1), jtmtac(1), histom(1)
  dimension  omegm(1), omold(1), thetam(1)
  dimension  reamdu(1), reamds(1), flxds(1), flxdr(1)
  dimension  reamqu(1), flxqs(1), flxqr(1)
  dimension  jcdsat(1), jcqsat(1), flxd(1), flxq(1)
  dimension  nppair(1), rotmom(1), ncld(1)
  dimension  nclq(1), jtqout(1), jomout(1), jthout(1)
  dimension  reamqs(1), epsom(1), dcoef(1), kcoil(1)
  dimension  voltum(1), anglum(1), nodfum(1), nodmum(1)
  dimension  kumout(1), jumout(1), umoutp(1)
  common /umlocal/ texta, d1,d2,d3,d17, stat59, fmum, rmvaum, rkvum, s1um, s2um, zlsbum, s1qum, s2qum, aglqum, raum, xdum, &
       squm, xdpum, xqpum, xdppum, xqppum, tdpum, tdppum, x0um, rnum, xnum, xfum, xdfum, xdkdum, xkdum, xkqum, xqkqum, &
       xgkqum, xqum, xqgum, xgum, distrf, hjum, dsynum, dmutum, spring, dabsum, tqppum, agldum, xlum, n1, n2, n3, n4, n5, n6, &
       n7, n8, n9, n10, n11, n12, n14, n15, n16, n17, n18, n19, n20, jr,jf, nexc, kconex, ibrexc, nstan, numasu, nmgen, nmexc, &
       ntypsm, netrun, netrum, nsmtpr, nsmtac, nrsyn, ntorq, mlum, nparum, ngroup, nall, nangre, nexcsw, limasu, lopss2, &
       lopss1, lopss8, lopss9, lopss10, lopss4, nshare
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'umdeck.ftn'
  include 'tacsar.ftn'
  include 'syncom.ftn'
  character*8 texta(101), text3,text4,text5
  character*8 textm, tesm1, tesm2, tesm3, tesm4, tesm5
  character*8 tesm6, tesm7, tesm8, tesm9, text6, text7
  !. data initialization:
  !     be careful with fpar(kcl) and umcurp(kcl). in umdata and
  !      and in umrenu they are used differently than in solvum.
  !     fpar(kcl) in umdata and umrenu : adress ibr of leakage ind
  !     fpar(kcl+1) in umdata and umrenu : adress kconst of excit
  !       sources to represent im excit coils (umdata creation)
  !     umcurp in umdata and umrenu : used to store shift in power
  !      nodes if load-flow is requested. then after load-flow is
  !      is completed, these same cells of umcurp are used as
  !      pointers for the shift of power resistances to network.
  data texta(1),texta(2),texta(3),texta(4),texta(5),texta(6),texta(7),texta(8),texta(9),texta(10) &
       /6hum-1  ,6hum-2  ,6hum-3  ,6hum-4  ,6hum-5  ,6hum-6  , 6hum-7  ,6hum-8  ,6hum-9  ,6hum-10 /
  data texta(11),texta(12),texta(13),texta(14),texta(15), texta(16),texta(17),texta(18),texta(19),texta(20) &
       /6hum-11 ,6hum-12 ,6hum-13 ,6hum-14 ,6hum-15 ,6hum-16 , 6hum-17 ,6hum-18 ,6hum-19 ,6hum-20 /
  data texta(21),texta(22),texta(23),texta(24),texta(25), texta(26),texta(27),texta(28),texta(29),texta(30) &
       /6htqgen ,6homegm ,6hthetam,6hipa   ,6hipb   ,6hipc   , 6hie1   ,6hie2   ,6hie3   ,6hie4   /
  data texta(31),texta(32),texta(33),texta(34),texta(35), texta(36),texta(37),texta(38),texta(39),texta(40) &
       /6hie5   ,6hie6   ,6hie7   ,6hie8   ,6hie9   ,6hie10  , 6hie11  ,6hie12  ,6hie13  ,6hie14  /
  data texta(41), texta(42), texta(43), texta(44), texta(45), texta(46), texta(47), texta(48) &
       /6hum1mxx, 6hum1e1x, 6hum1e2x, 6hum1e3x, 6hum2mxx, 6hum2e1x, 6hum2e2x, 6hum2e3x /
  data texta(49), texta(50), texta(51), texta(52), texta(53), texta(54), texta(55), texta(56) &
       /6hum3mxx, 6hum3e1x, 6hum3e2x, 6hum3e3x, 6hum4mxx, 6hum4e1x, 6hum4e2x, 6hum4e3x /
  data texta(57), texta(58), texta(59), texta(60) /6hum5mxx, 6hum5e1x, 6hum5e2x, 6hum5e3x/
  data texta(61), texta(62), texta(63), texta(64), texta(65) /6hum1ntr, 6hum2ntr, 6hum3ntr, 6hum4ntr, 6hum5ntr/
  data texta(66), texta(67), texta(68), texta(69), texta(70) /6hum1ms1, 6hum2ms1, 6hum3ms1, 6hum4ms1, 6hum5ms1/
  data texta(71), texta(72), texta(73), texta(74), texta(75) /6hum1ms2, 6hum2ms2, 6hum3ms2, 6hum4ms2, 6hum5ms2/
  data texta(76), texta(77), texta(78), texta(79), texta(80) /6hum1tla, 6hum1tlb, 6hum1tlc, 6hum2tla, 6hum2tlb/
  data texta(81), texta(82), texta(83), texta(84), texta(85) /6hum2tlc, 6hum3tla, 6hum3tlb, 6hum3tlc, 6hum4tla/
  data texta(86), texta(87), texta(88), texta(89), texta(90) /6hum4tlb, 6hum4tlc, 6hum5tla, 6hum5tlb, 6hum5tlc/
  data texta(91), texta(92), texta(93), texta(94) /6hfluxmd, 6himd   , 6hfluxmq, 6himq   /
  data texta(95), texta(96), texta(97), texta(98) /6hip0   , 6hipd   , 6hipq   , 6htqexc /
  data texta(99),texta(100),texta(101) /6hum1mcc, 6hum2mcc, 6hum3mcc/
  data textm / 6hmore  /
  data tesm1,tesm2,tesm3,tesm4,tesm5,tesm6,tesm7,tesm8,tesm9 /6hsmdata,6htolera,6hparame,6hall   ,6hnone  ,6hextend, &
       6hfinish,6h part ,6hshare /
  !   if mech network option is used, rotmom is used to store
  !      frequency of network connected to power coil. initiali -
  !      zation is in umrenu, except if sm type-59 is used.
  if ( iprsup  .ge.  1 ) write ( lunit6, 4567 )
4567 format ( 24h  "begin module umdata." )
  if (numum .eq. 0) go to 40
  write (lunit6,30)
30 format(/,45h um error stop. you have requested another um, 44h to be included to this data case. the rules, &
       29h regarding the ordering of um)
  write (lunit6,31)
31 format(  39h data cards have however been violated., 43h the data cards of this um should be placed, &
       32h immediately behind those of the ,/, 35h previous um in accordance with the, 18h following rules :)
  write (lunit6,32)
32 format(/,43h (1). the data cards of the next um have to, 44h follow immediately those of the previous um, &
       25h without the insertion of)
  write(lunit6,33)
33 format(34h a blank card and without repeated, 40h insertion of the class-1 um data cards., &
       40h  these class-1 cards, consisting of two )
  write(lunit6,34)
34 format(36h cards and a blank termination card,, 43h contains initial information regarding the, &
       41h reading of all um cards to follow. it is)
  write(lunit6,35)
35 format(' therefore to be placed on top and never to', ' be repeated between the cards of the', " different um's.")
  write(lunit6,36)
36 format(/,41h (2). the data cards of the last um to be, 36h included to this data case is to be, &
       41h terminated with a blank card, indicating ,/, 33h the ending of all um data cards.)
  go to 9600
  !     . busum adjustment due to variable numfix :
40 do n1 = 1, numfix
42   busum(n1) = texta(n1)
  end do
  do n1 = 1, 28
     n2 = n1 + numfix
     n3 = n1 + 12
     if (n1 .gt. 11) go to 50
     n3 = n1 + 87
     if (n1 .gt. 3) go to 50
     n3 = n1 + 20
50   busum(n2) = texta(n3)
  end do
  pi = twopi * onehaf
  d2 = 2.0
  d3 = 3.0
  sroot2 = sqrtz(d2)
  sroot3 = sqrtz(d3)
  omegrf = twopi * statfr
  !     start reading um data ***************************************
  !     note : the following variable names are reserved for
  !     looping during reading of the data input :
  numum = 0
  ncltot = 0
  nsmtac = 0
  nsmach = 0
  !     nsmtac is the total number of additional entries to
  !     umoutp(numout) because of sm tacs transfer.
  n1 = 0
  !     .user1 : general specifications (class 1 um data cards)
  !     read input card using cimage
  call cimage
  read (unit = abuff(1), fmt = 60) inpu, initum, bus3, limasu, loopss(8)
60 format(2i1,a6,i6,i1)
  if (bus3 .eq. tesm1) initum = 1
  if (limasu .eq. 0) limasu = 10
  if (loopss(8) .eq. 0) go to 105
  if (noutpr .eq. 0) write (kunit6, 102)
102 format(40h+um power coil interfacing by prediction)
  go to 150
105 if ( noutpr  .eq.  0 ) write (kunit6, 110)
110 format (21h+um fully compensated)
  !     read(lunit5,203) jtype(n1),ncld(n1),nclq(n1),jtqout(n1),
  !     vjomout(n1),jthout(n1),bum3(n1),bum4(n1),nppair(n1),
  !     vrotmom(n1),dcoef(n1),epsom(n1),omegm(n1),reamdu(n1),
  !     vjcdsat(n1),reamds(n1),flxds(n1),flxdr(n1),thetam(n1),
  !     vreamqu(n1),jcqsat(n1),reamqs(n1),flxqs(n1),flxqr(n1),noread
203 format(3i2,3i1,2a6,i2,3e14.5/2e14.5,i1,3e14.5/2e14.5,i1, 3e14.5,i1)
  !     decision on m31 or m32 data input ***************************
  !     with m32 , the input is modular.
150 call cimage
  read (unit = abuff(1), fmt = 1207) text3, text4, text5
  if (text3 .ne. blank) go to 3202
  if (text4 .ne. blank) go to 3202
  if (text5 .ne. blank) go to 3202
  if (noutpr .eq. 0) write (kunit6, 1001)
1001 format ( 40h+blank card ending class-1 um data cards  )
  go to 11202
  !     data input for m31 and older versions ***********************
  !. user2 : mach-table input for m31 and older
  !     read input card using cimage
1202 call cimage
1203 n6 = 0
  read (unit = abuff(1), fmt = 1207) text3, text4, text5
1207 format ( 13a6, a2 )
  if ( text3  .ne.  blank )   go to 3202
  if ( text4  .ne.  blank )   go to 3202
  if ( text5  .ne.  blank )   go to 3202
  if ( noutpr  .eq.  0 ) write (kunit6, 2202)
2202 format (  33h+blank card ending machine table.  )
  go to 300
3202 numum = numum + 1
  if (iprsup .ge. 1) write(lunit6,170) numum
170 format(37h0the machine-table input of um number,i3,1h:)
  if ( numum  .le.  numfix )   go to 5202
  write (lunit6, 4202)  numum
4202 format ( /,  66h overflow u.m. machine table in module "umdata". Current case has, 2x, i4, 2x, &
       52hu.m.,  numfix on the card for absolute um dimensions, /,28 h must be set to no less than, 2x, i4, 1h.  )
  go to 9600
5202 n1 = numum
  read (unit = abuff(1), fmt = 1204) nclq(n1),jtqout(n1), jomout(n1), jthout(n1), bus3, bus4, nppair(n1), rotmom(n1), &
       dcoef(n1), epsom(n1)
1204 format ( 3i2, 3i1, 2a6, i2, 3e14.5 )
  if ( noutpr  .eq.  0 ) write (kunit6, 3203)  jtype(n1), ncld(n1), nclq(n1)
3203 format (  21h+mach-table card   1.,  3i5  )
  do n10 = 1, ntot
     if (bus3 .eq. blank ) go to 205
     if (bus3 .ne. bus(n10)) go to 204
     nodom(n1) = n10
     rotmom(n1) = twopi * statfr
     go to 205
204 end do
  write (lunit6, 3204)  bus3
3204 format ( /,   36h error stop.   the node name  "bus3", 34h on the last-read card is not that &
       ,/, 30h of any electric network node., 6h bus3=,  a6  )
  go to 9600
205 jtmtac(n1) = 0
  if (bus3 .ne. blank ) go to 213
  do j = 1, ktab
     ndx1 = ilntab( klntab + j )
     if (bus4 .eq. texvec(ndx1)) go to 209
207 end do
  if ( bus4  .eq.  blank )   go to 213
  write (lunit6, 3207)  bus4
3207 format ( /,   36h error stop.   the tacs name  "bus4", 25h on the last-read card is, 22h unrecognized.   bus4=,  a6  )
  go to 9600
209 ndx1 = kxtcs + j
  jtmtac(n1) = ndx1
  !     read input card using cimage
213 call cimage
  read (unit = abuff(1), fmt = 4203) omegm(n1), reamdu(n1), jcdsat(n1), reamds(n1), flxds(n1), flxdr(n1)
4203 format ( 2e14.5,  i1,  3e14.5 )
  if ( noutpr  .eq.  0 ) write (kunit6, 5203)  omegm(n1), reamdu(n1)
5203 format (  21h+mach-table card   2., 2e12.4)
  !     read input card using cimage
  call cimage
  read (unit = abuff(1), fmt = 4203) thetam(n1), reamqu(n1), jcqsat(n1), reamqs(n1), flxqs(n1), flxqr(n1)
  if (reamqu(n1) .gt. 0.0) go to 6199
  write(lunit6,6198) n1
6198 format( /, 41h error stop. incorrect um data input. the, 42h result is that the q-axis main inductance, /,7x, &
       39h is either zero or negative for machine, 7h number, i4, 1h.)
  go to 9600
6199 if ( noutpr  .eq.  0 ) write (kunit6, 6203)  thetam(n1), reamqu(n1)
6203 format (  21h+mach-table card   3., 2e12.4)
  if ( iprsup  .ge.  1 ) write(lunit6,214) jtype(n1),ncld(n1),nclq(n1),jtqout(n1),jomout(n1),jthout(n1), bus3, bus4, &
       nppair(n1),rotmom(n1),dcoef(n1),epsom(n1),omegm(n1),reamdu(n1),jcdsat(n1),reamds(n1),flxds(n1),flxdr(n1),thetam(n1), &
       reamqu(n1),jcqsat(n1),reamqs(n1),flxqs(n1),flxqr(n1)
214 format(1h0,3i2,3i1,2a6,i2,3e14.5/1h ,2e14.5,i1,3e14.5, 1h ,2e14.5,i1,3e14.5)
  if (initum .eq. 0) go to 249
  call cimage
  d1 = 1.0e+3
  n5 = 4
  n6 = 0
  read (unit = abuff(1), fmt = 6204) voltum(n1), anglum(n1), bus5, bus6, distrf
6204 format(2e14.5, 2a6, e14.5)
  if (noutpr .eq. 0) write (kunit6, 6205)  voltum(n1), anglum(n1)
6205 format(   21h+mach-table card   4., 2e12.4)
  if (jtype(n1) .eq. 3) go to 230
  if (jtype(n1) .eq. 4) go to 230
  if (jtype(n1) .eq. 5) go to 230
  do n10 = 1,ntot
     if (bus5 .ne. bus(n10)) go to 220
     nodfum(n1) = n10
     do n11 = 1,kconst
        n12 = node(n11)
        if (n12 .lt. 0) n12 = - n12
        if (nodfum(n1) .ne. n12) go to 219
        nodfum(n1) = n11
        go to 230
219  end do
220 end do
  write (lunit6, 222)  bus5
222 format ( /,   36h error stop.   the node name  "busf", 34h on the last-read card is not that, &
       /, 37h of any electric network source node., 6h busf=,  a6  )
  go to 9600
230 do n10 = 1,ntot
     if (bus6 .ne. bus(n10)) go to 233
     do n11 = 1,kconst
        n12 = node(n11)
        if (n12 .lt. 0) n12 = - n12
        if (n10 .ne. n12) go to 232
        crest(n11) = d1 * distrf
        if (n6 .ne. 0) go to 231
        nodmum(n1) = n11
        if (distrf .eq. 0.0) crest(n11) = d1
231     tstart(n11) = - 7777.
        go to 240
232  end do
233 end do
  write (lunit6, 234) bus6
234 format ( /,   36h error stop.   the node name  "busm", 34h on the last-read card is not that, &
       /, 37h of any electric network source node., 6h busm=,  a6  )
  go to 9600
240 call cimage
  read (unit = abuff(1), fmt = 6240) text3
6240 format(a6)
  if (text3 .ne. textm) go to 248
  n5 = n5 + 1
  n6 = 1
  read (unit = abuff(1), fmt = 6241) text3, bus6, distrf
6241 format(a6, 28x, a6, e14.5)
  if (noutpr .eq. 0) write (kunit6, 6242)  n5, distrf
6242 format( 16h+mach-table card, i4, 1h., e12.4)
  go to 230
248 go to 1203
249 go to 1202
  !     . if flux output is desired :
  !     . user3 : coil-table input for m31 and older
300 if ( iprsup  .ge.  1 ) write(lunit6,301)
301 format(22h0the coil-table input:  ,/,  1x  )
  !     . avoid conflict blank termination card with dummy coil
  !     . and adjustment of ncld and nclq for um type 4 :
  n5 = 0
  do n10 = 1, numum
     if (jtype(n10) .ne. 4) go to 302
     n5 = n5 + 1
     ncld(n10) = 1
     nclq(n10) = 1
302  n5 = n5 + ncld(n10) + nclq(n10)
  end do
  n5 = n5 + 3 * numum
  !     read(lunit5,303) gpar(n1),reacl(n1),bum1(n1),bum2(n1),
  !     vbum6(n1),jclout(n1),hist(n1),noread
303 format(2e14.5,3a6,i1,e14.5 )
  !     read input card using cimage
3302 call cimage
  read (unit = abuff(1), fmt = 1207) text3, text4, text5
  if ( text3  .ne.  blank )   go to 4303
  if ( text4  .ne.  blank )   go to 4303
  if ( text5  .ne.  blank )   go to 4303
  if ( ncltot .lt. n5 ) go to 4303
  if ( noutpr  .eq.  0 ) write (kunit6, 4302)
4302 format (   30h+blank card ending coil table.  )
  go to 500
4303 ncltot = ncltot + 1
  fpar(ncltot) = 0.0
  if ( ncltot  .le.  nclfix )   go to 6302
  write (lunit6, 5302)  ncltot
5302 format ( /,   26h overflow u.m. coil table., 11h   ncltot =,  i5  )
  go to 9600
6302 n1 = ncltot
  read (unit = abuff(1), fmt = 303) gpar(n1), reacl(n1), bus1, bus2, bus6, jclout(n1), hist(n1)
  if ( noutpr  .eq.  0 ) write (kunit6, 2203) ncltot,   gpar(n1), reacl(n1), jclout(n1)
2203 format (  5h+coil,  i4,  1h.,  2e14.5,  i5  )
  n11 = 0
  n12 = 0
  do n10 = 1,ntot
     if (n11 .ne. 0) go to 304
     if (bus1 .ne. bus(n10)) go to 304
     nodvo1(n1) = n10
     n11 = 1
     if (n12 .ne. 0) go to 305
304  if (bus2 .ne. bus(n10)) go to 305
     nodvo2(n1) = n10
     n12 = 1
305  n13 = n11 * n12
     if (n13 .ne. 0) go to 307
306 end do
  if ( n11  .eq.  0 ) write (lunit6, 3206)  bus1
3206 format ( /,    33h error stop.  just-read coil card, 25h bears illegal node name:,  a6  )
  if ( n12  .eq.  0 ) write (lunit6, 3206)  bus2
  go to 9600
307 jcltac(n1) = 0
  if (bus6 .eq. blank) go to 313
  do j = 1, ktab
     ndx1 = ilntab( klntab + j )
     if (bus6 .eq. texvec(ndx1)) go to 311
309 end do
  write (lunit6, 3207)  bus6
  go to 9600
311 ndx1 = kxtcs + j
  jcltac(n1) = ndx1
313 if (iprsup .ge. 1) write(lunit6,314) gpar(n1),reacl(n1), bus1, bus2, bus6, jclout(n1),hist(n1)
314 format(1h ,2e14.5,3a6,i1,e14.5 )
  go to 3302
  !  data input for m32 and newer versions ********************
  !. user2 : mach-table input for m32 and newer
  !     class 2 and class 3 um data cards
11202 call cimage
  read (unit = abuff(1), fmt = 11207) text3, text4, text5
11207 format ( 13a6, a2 )
  if ( text3  .ne.  blank )   go to 13202
  if ( text4  .ne.  blank )   go to 13202
  if ( text5  .ne.  blank )   go to 13202
  if ( noutpr  .eq.  0 ) write (kunit6, 12202)
12202 format (  27h+blank card ending um data.)
  go to 500
13202 numum = numum + 1
  if ( numum  .le.  numfix )   go to 17000
  write (lunit6, 14202)  numum
14202 format ( /,  37hoverflow u.m. machine table.  numum =, i4, 36hincrease numfix in card for absolute ,/, 15h um dimensions.)
  go to 9600
  !  checking of data input format ******************************
17000 n10 = 0
  read (unit = abuff(1), fmt = 17002) n10
17002 format(i2)
  if (n10 .ne. 19) go to 17004
  write (lunit6,30)
  write (lunit6,31)
  write (lunit6,32)
  write (lunit6,33)
  write (lunit6,34)
  write (lunit6,35)
  write (lunit6,36)
  go to 9600
17004 if (n10 .ge. 50 .and. n10 .lt. 60) go to 17040
  if (n10 .lt. 50) go to 15202
  !  start reading sm type -50 to 59 data input format **********
  jf = 0
  jr = 0
17040 call umdatb (reacl,gpar,fpar,nodvo1,nodvo2,jcltac,jtype,nodom,ncld,jtmtac,reamdu,reamds,flxds,flxdr,reamqu, &
       flxqs,flxqr,jcdsat,jcqsat,nppair,rotmom,nclq,jtqout, jthout,reamqs,voltum,anglum,nodfum,nodmum,kumout,jumout, &
       jclout, dcoef, jomout, umoutp )
  if (jf .eq. 1) goto 11202
  if (jr .eq. 1) return
  !  end reading sm type-59 data input $$$$$$$$$$$$$$$$$$$$$$$$$$
  !    start reading pure um data input format ******************
15202 n1 = numum
  read (unit = abuff(1), fmt = 11203) jtype(n1), ncld(n1), nclq(n1), jtqout(n1), jomout(n1), jthout(n1), bus3, bus4, &
       nppair(n1), rotmom(n1), dcoef(n1), epsom(n1), d10
11203 format ( 3i2, 3i1, 2a6, i2, 4e14.5 )
  if ( noutpr  .eq.  0 ) write (kunit6, 13203)  numum, jtype(n1)
13203 format(5h+um -, i3, 26h   mach card  1.   type = ,i2)
  do n10 = 1, ntot
     if (bus3 .eq. blank ) go to 10205
     if (bus3 .ne. bus(n10)) go to 10204
     nodom(n1) = n10
     if (d10 .eq. 0.0) d10 = statfr
     rotmom(n1) = twopi * d10
     go to 10205
10204 end do
  write (lunit6, 13204)  bus3
13204 format ( /,   36h error stop.   the node name  "bus3", 34h on the last-read card is not that ,/, &
       30h of any electric network node., 6h bus3=,  a6  )
  go to 9600
10205 jtmtac(n1) = 0
  if (bus3 .ne. blank ) go to 10213
  do j = 1, ktab
     ndx1 = ilntab( klntab + j )
     if (bus4 .eq. texvec(ndx1)) go to 10209
10207 end do
  if ( bus4  .eq.  blank )   go to 10213
  write (lunit6, 13207)  bus4
13207 format ( /,   36h error stop.   the tacs name  "bus4", 25h on the last-read card is, &
       /, 22h unrecognized.   bus4=,  a6  )
  go to 9600
10209 ndx1 = kxtcs + j
  jtmtac(n1) = ndx1
  !     read input card using cimage
10213 call cimage
  read (unit = abuff(1), fmt = 14203) omegm(n1), reamdu(n1), jcdsat(n1), reamds(n1), flxds(n1), flxdr(n1)
14203 format ( 2e14.5,  i1,  3e14.5 )
  if ( noutpr  .eq.  0 ) write (kunit6, 15203)  numum, omegm(n1), reamdu(n1)
15203 format(5h+um -, i3, 16h   mach card  2.,2e10.2)
  !     read input card using cimage
  call cimage
  read (unit = abuff(1), fmt = 14203) thetam(n1), reamqu(n1), jcqsat(n1), reamqs(n1), flxqs(n1), flxqr(n1)
  if (reamqu(n1) .gt. 0.0) go to 15910
  write (lunit6,6198) n1
  go to 9600
15910 if ( noutpr  .eq.  0 ) write (kunit6, 15903)  numum, thetam(n1), reamqu(n1)
15903 format(5h+um -, i3, 16h   mach card  3.,2e10.2)
  if (iprsup .ge. 4) write (lunit6,15912) n1
15912 format (/,40h ***************************************, 37h the machine-table input of um number,i4,2h :)
  if ( iprsup  .ge.  4 ) write(lunit6,10214) jtype(n1),ncld(n1),nclq(n1),jtqout(n1), jomout(n1),jthout(n1), bus3, bus4, &
       nppair(n1), rotmom(n1),dcoef(n1),epsom(n1),omegm(n1),reamdu(n1), jcdsat(n1),reamds(n1),flxds(n1),flxdr(n1),thetam(n1), &
       reamqu(n1),jcqsat(n1),reamqs(n1),flxqs(n1),flxqr(n1)
10214 format(10h *********,3i2,3i1,2a6,i2,3e14.5,10h *********,2e14.5,i1,3e14.5, 10h *********,2e14.5,i1,3e14.5)
  !  read share card(if sharing of mech netw is requested) :
  nshare = 0
  call cimage
  read (unit = abuff(1), fmt = 15800) text3
15800 format(a6)
  if (text3 .ne. tesm9) go to 15830
  nshare = 1
  read (unit = abuff(1), fmt = 15810) n5, n6
15810 format(6x,2i6)
  if (n6 .eq. 0) go to 15822
  if (noutpr .eq. 0) write(kunit6,15820) numum,n5,n6
15820 format(5h+um -,i3,26h shares mech netw with um-,i3,3hand,i3)
  go to 15826
15822 if (noutpr .eq. 0) write(kunit6,15824) numum,n5
15824 format(5h+um -,i3,26h shares mech netw with um-,i3)
15826 if (n1 .ne. n5 .and. n1 .ne. n6) go to 15828
  write(lunit6,15827) n1
15827 format(/,21h error stop. this um-,i3,15h is supposed to, 45h share its mech network with other um's. what, &
       41h should be specified on this card are the, /, 36h numbers of these other um's without, &
       40h including the number of the um which is, 27h being processed right now.)
  call stoptp
15828 n10 = n1
  if (n10 .gt. n5) n10 = n5
  if (n6 .ne. 0 .and. n10 .gt. n6) n10 = n6
  if (n1 .ne. n10) nshare = 2
15830 if (initum .eq. 0) go to 10300
  if (nshare .eq. 0) go to 15880
  call cimage
15880 d1 = 1.0e+3
  !  make sure that user sets up automatic ss-calc if ldflow :
  if (istep .ne. -4567) go to 15890
  if (nshare .eq. 2) go to 15890
  read (unit = abuff(1), fmt = 15882) bus6
15882 format(34x,a6)
  if (bus6 .ne. blank) go to 15890
  write(lunit6,80502)
  call stoptp
15890 n5 = 4
  n6 = 0
  read (unit = abuff(1), fmt = 15904) voltum(n1), anglum(n1), bus5, bus6, distrf
15904 format(2e14.5, 2a6, e14.5)
  if (noutpr .eq. 0) write (kunit6, 15905)  numum, voltum(n1), anglum(n1)
15905 format(5h+um -, i3, 16h   mach card  4.,2e10.2)
  if (jtype(n1) .eq. 3) go to 10230
  if (jtype(n1) .eq. 4) go to 10230
  if (jtype(n1) .eq. 5) go to 10230
  do n10 = 1, ntot
     if (bus5 .ne. bus(n10)) go to 10220
     nodfum(n1) = n10
     do n11 = 1, kconst
        n12 = node(n11)
        if (n12 .lt. 0) n12 = - n12
        if (nodfum(n1) .ne. n12) go to 10219
        nodfum(n1) = n11
        go to 10230
10219 end do
10220 end do
  write (lunit6, 10222)  bus5
10222 format ( /,   36h error stop.   the node name  "busf", 34h on the last-read card is not that, &
       /, 37h of any electric network source node., 6h busf=,  a6  )
  go to 9600
10230 do n10 = 1,ntot
     if (nshare .eq. 2 .and. bus6 .eq. blank) go to 10250
     if (bus6 .ne. bus(n10)) go to 10233
     do n11 = 1,kconst
        n12 = node(n11)
        if (n12 .lt. 0) n12 = - n12
        if (n10 .ne. n12) go to 10232
        crest(n11) = d1 * distrf
        if (n6 .ne. 0) go to 10231
        nodmum(n1) = n11
        if (distrf .eq. 0.0) crest(n11) = d1
10231   tstart(n11) = - 7777.
        go to 10250
10232 end do
10233 end do
  if (nshare .ne. 2) go to 90233
  write(lunit6,90200) bus6
90200 format(/,28h error stop. the node name ",a6, 8h" on the, 37h last-read card should be left blank.)
  write (lunit6,90202) n1
90202 format(10h this um -,i3,26h is requested to share its, 41h mechanical network with other um's.  the, &
       45h specification of all applied torques in this, /, 42h mech network should have been placed with, &
       45h the data input for the lowest-numbered um of, 43h the set of um's sharing this mech network.)
  call stoptp
90233 write (lunit6, 10234) bus6
10234 format ( /,   36h error stop.   the node name  "busm", 34h on the last-read card is not that, &
       /, 37h of any electric network source node., 6h busm=,  a6  )
  go to 9600
10250 call cimage
  read (unit = abuff(1), fmt = 10260) text3
10260 format(a6)
  if (text3 .ne. textm) go to 10300
  if (nshare .ne. 2) go to 90260
  write(lunit6,90250) n1
90250 format(/,43h error stop. this card 5 of class 2 um data, 19h cards of this um -,i3,19h should be removed.)
  write(lunit6,90202) n1
  call stoptp
90260 n5 = n5 + 1
  n6 = 1
  read (unit = abuff(1), fmt = 10261) text3, bus6, distrf
10261 format(a6, 28x, a6, e14.5)
  if (noutpr .eq. 0) write (kunit6, 10262)  numum, n5, distrf
10262 format( 5h+um -, i3, 12h   mach card, i3, 1h., e14.5)
  go to 10230
  !     . if flux output is desired :
  !     . user3 : coil-table input for m32 and newer
10300 if ( iprsup  .ge.  4 ) write(lunit6,10301) numum
10301 format(/,40h ***************************************, 34h the coil-table input of um number,i4,2h :)
  n8 = 0
  ncldq = ncld(n1) + nclq(n1) + 3
  !. adjustment of ncld and nclq for um type 4 :
  if (jtype(n1) .ne. 4) go to 10304
  ncld(n1) = 1
  nclq(n1) = 1
  ncldq = ncld(n1) + nclq(n1) + 4
10304 if (initum .ne. 0) go to 14303
13302 if (n8 .ge. ncldq) go to 11202
  if (n8 .eq. 0 .and. nshare .eq. 0) go to 14303
  call cimage
14303 ncltot = ncltot + 1
  fpar(ncltot) = 0.0
  if ( ncltot  .le.  nclfix )   go to 14305
  write (lunit6, 14304)  ncltot
14304 format ( /,   26h overflow u.m. coil table., 11h   ncltot =,  i5  )
  go to 9600
14305 n8 = n8 + 1
  read (unit = abuff(1), fmt = 13303) gpar(ncltot), reacl(ncltot), bus1, bus2, bus6, jclout(ncltot), hist(ncltot)
13303 format (2e14.5, 3a6, i1, e14.5)
  if ( noutpr  .eq.  0 ) write (kunit6, 14306) numum, n8, gpar(ncltot), reacl(ncltot)
14306 format(5h+um -, i3, 12h   coil card, i3, 1h.,2e11.2)
  n11 = 0
  n12 = 0
  do n10 = 1,ntot
     if (n11 .ne. 0) go to 13304
     if (bus1 .ne. bus(n10)) go to 13304
     nodvo1(ncltot) = n10
     n11 = 1
     if (n12 .ne. 0) go to 13305
13304 if (bus2 .ne. bus(n10)) go to 13305
     nodvo2(ncltot) = n10
     n12 = 1
13305 n13 = n11 * n12
     if (n13 .ne. 0) go to 13307
13306 end do
  if ( n11  .eq.  0 ) write (lunit6, 13206)  bus1
13206 format (/, ' error stop.  Just-read coil card bears illegal node name:', a6)
  if ( n12  .eq.  0 ) write (lunit6, 13206)  bus2
  go to 9600
13307 jcltac(ncltot) = 0
  if (bus6 .eq. blank) go to 13313
  do j = 1, ktab
     ndx1 = ilntab( klntab + j )
     if (bus6 .eq. texvec(ndx1)) go to 13311
13309 end do
  write (lunit6, 13207)  bus6
  go to 9600
13311 ndx1 = kxtcs + j
  jcltac(ncltot) = ndx1
13313 if (iprsup .ge. 4) write(lunit6, 13314) ncltot,gpar(ncltot),reacl(ncltot), bus1, bus2, bus6, jclout(ncltot),hist(ncltot)
13314 format(23h ******** coil card nr.,i3,2h :,2e14.5,3a6,i1,e14.5)
  go to 13302
  !.  at this point reading of all um data input is finished
  !         ******************************************************
  !. calculation of real gpar :
500 do n1 = 1, ncltot
     if (gpar(n1) .ne. 0.0) gpar(n1) = 1.0/gpar(n1)
502 end do
  if (istep .ne. -4567) go to 80510
  if (initum .eq. 1) go to 80510
  write(lunit6,80502)
80502 format(/,41h error stop. you have requested the um to, 44h be initialized through a load-flow process., &
       41h you have however failed to set up the um,/,  38h data input for automatic steady-state, &
       43h initialization. consult the emtp rule book, 32h regarding this usage of the um.)
  call stoptp
80510 if (nsmach .eq. 0) go to 510
  !  warning if overflow of etac,ismtac and bustac
  n1 = ntotac - ntacb
  if (n1 .le. 10) go to 510
  write (lunit6,504) n1
504 format(/, 40h warning: in using sm type-50 to 59 data,36h input,you have requested a total of, i4)
  write (lunit6,505)
505 format(/, 34h to be passed to tacs. the default,37h storage allocation is just for 10 of)
  write (lunit6, 506)
506 format(/, 38h such variables because in deck synmac,36h the arrays ismtac, etac, bustac are)
  write (lunit6, 507)
507 format(/, 41h dimensioned to 10. did you increase this,39h dimension to be at least equal to this)
  write (lunit6, 508)
508 format(/, 43h total nr of variables you want to transfer,27h to tacs?  you should have.    )
510 kcoil(1) = 1
  numout = 0
  if (loopss(8) .ne. 1) go to 530
  n1 = 6 * numum
  if (n1 .le. nclfix) go to 530
  write (lunit6,520)
520 format( /, 28h overflow of um coil table, ,32h increase nclfix on um dimension,32h card to be greater than 6 times, &
       35h the total nr. of used um machines.)
  go to 9600
  !. machine looping for special processing of some parameters:
530 do n1 = 1, numum
     numoui = numout
     rppair = nppair(n1)
     if (jtype(n1) .eq. 13) jtmtac(n1) = 0
     if (initum .eq. 1) omegm(n1) = rotmom(n1)/rppair
     omold(n1) = omegm(n1)
     flxd(n1) = 0.0
     flxq(n1) = 0.0
     !. omold initialization, torque angle for type 1 and 1,
     !. and coil adjustment for three-phase exc. coils :
     if (jtype(n1) .gt. 2) go to 921
     thetam(n1) = (thetam(n1) + pi/2.0)/rppair
     !. set up of array kcoil(1:numum) :
921  if (n1 .eq. numum) go to 950
     n2 = n1 + 1
     n3 = 3 + ncld(n1) + nclq(n1)
     if (jtype(n1) .ne. 4) go to 940
     n3 = n3 + 1
940  kcoil(n2) = kcoil(n1) + n3
950  kcl = kcoil(n1)
     !  making sure that power coils are not tacs controlled :
     do n2 = 1, 3
        n3 = kcl - 1 + n2
        if (jcltac(n3) .eq. 0) go to 958
        write(lunit6,952) n1
952     format(/,22h error stop. um number, i4, 12h is provided,42h with tacs controlled sources on the power, &
             44h side. this is only allowed for the coils on,/, 43h the excitation side. tacs control of power, &
             45h side coils is to be done through the network, 35h which is connected to these coils.)
        call stoptp
958  end do
     if (initum .eq. 0) go to 990
     do n2 = 1, 3
        n3 = kcl - 1 + n2
        !  preparing shift of leakage inductances of power coils *******
        !  note : the emtp address it of these shifted inductances
        !         are stored in fpar(kcl).
961     if (nodvo1(n3) .eq. nodvo2(n3)) go to 980
        !  create new node behind leakage inductance. this node
        !    becomes the new um power node.
        ntot = ntot + 1
        kode(ntot) = 0
        bus(ntot) = trash
        if (n1 .gt. 5) go to 962
        n4 = 75 + (n1-1)*3 + n2
        bus(ntot) = texta(n4)
        !  creation of the leakage inductance branche
962     call ibrinc
        it = it + 1
        length(ibr) = 1
        nr(ibr) = - it
        tr(it) = 0.0
        c(it) = 0.0
        d1 = reacl(kcl+2)
        if (d1 .gt. reacl(kcl+1)) d1 = reacl(kcl+1)
        if (jtype(n1) .lt. 3) go to 966
        if (jtype(n1) .gt. 5) go to 966
        if (n2 .ne. 3) go to 966
        reacl(kcl+1) = reacl(kcl+1) - d1
        reacl(kcl+2) = reacl(kcl+2) - d1
966     if (xopt .eq. 0.0) tx(it) = d1 * 1.0d+3
        if (xopt .ne. 0.0) tx(it) = d1 * twopi*xopt
        if (n3 .eq. kcl) fpar(kcl) = ibr
        n4 = 3 * (n1 - 1)
        if (nodvo1(n3) .eq. 1) go to 972
        umcurp(n4+n2) = nodvo1(n3)
        kbus(ibr) = nodvo1(n3)
        mbus(ibr) = ntot
        nodvo1(n3) = ntot
        go to 974
972     umcurp(n4+n2) = nodvo2(n3)
        kbus(ibr) = nodvo2(n3)
        mbus(ibr) = ntot
        nodvo2(n3) = ntot
974     if (n2 .ne. 1) go to 976
        if (iprsup .ge. 1) write(lunit6,975) n1
975     format( /, 36h additional branches created to move, 38h leakage induct to emtp network for um, &
             7h number,i3,1h.,/,21x,12hnode to node,5x, 3hibr,6x,2hit,8x,6htx(it))
976     if (iprsup .ge. 1) write (lunit6,977) mbus(ibr),kbus(ibr),ibr,it,tx(it)
977     format( 21x,i4,4x,i4,4x,i4,4x,i4,e14.5)
980  end do
     !  creation of speed capacitors for steady-state initialization:
     ntot = ntot + 1
     if (jtmtac(n1) .le. 0) jtmtac(n1) = - ntot
     bus(ntot) = trash
     if (n1 .lt. 4) bus(ntot) = texta(98+n1)
     call ibrinc
     it = it + 1
     fpar(kcl+2) =  it
     length(ibr) = 1
     nr(ibr) = - it
     kbus(ibr) = nodom(n1)
     mbus(ibr) = ntot
     tr(it) = epsiln
     c(it) = 0.0
     tx(it) = 0.0
     if (iprsup .ge. 1) write (lunit6,80980) mbus(ibr),kbus(ibr),ibr,it,tr(it),c(it)
80980 format(10h *********,29h potential speed cap : mbus =,i6, 7h kbus =,i6,6h ibr =,i6, 5h it =,i6, 9h tr(it) =,e14.5, &
          8h c(it) =,e14.5)
     !  creation of sources to represent im excitation coils for
     !    steady-state initialization (accommodates kpsour use):
     if (jtype(n1) .lt. 3) go to 990
     if (jtype(n1) .gt. 7) go to 990
     n5 = kcl + 3
     n6 = kcl + 2 + ncld(n1) + nclq(n1)
     if (jtype(n1) .eq. 4) n6 = n6 + 1
     n11 = kconst + 1
     do j = n5, n6
        if (nodvo1(j) .eq. nodvo2(j)) go to 988
        do n10 = 1, 2
           if (n10 .eq. 2) go to 982
           if (nodvo1(j) .eq. 1) go to 987
           kconst = kconst + 1
           n20 = nodvo1(j)
           node(kconst) = - n20
           kode(n20) = 0
           go to 984
982        if (nodvo2(j) .eq. 1) go to 987
           kconst = kconst + 1
           n20 = nodvo2(j)
           node(kconst) = - n20
           kode(n20) = 0
984        if (kconst .eq. n11) fpar(kcl+1) = kconst
           iform(kconst) = 14
           crest(kconst) = 0.0
           time1(kconst) = 0.0
           tstart(kconst) = - 1.0
           tstop(kconst) = 0.0
           sfreq(kconst) = 0.0001
           if (iprsup .ge. 1 .and. j .eq. n5) write(lunit6,985)
985        format(10h *********,47h steady-state curr sources for im excit coils :, &
                44h    node  kconst         sfreq         crest, 14h         tstop)
           if (iprsup .ge. 1) write (lunit6,986) node(kconst),kconst,sfreq(kconst),crest(kconst),tstop(kconst)
986        format(10h *********,47x,2i8,3e14.5)
987     end do
988  end do
990  n2 = kcoil(n1)
     n3 = n2 + ncld(n1) + nclq(n1) + 2
     if (jtype(n1) .eq. 4) n3 = n3 + 1
     if (iprsup .ge. 1) write(lunit6,992) n1
992  format(/, 35h electric terminal nodes for um nr.,i3,2h: )
     if (iprsup .ge. 1) write (lunit6,993) (nodvo1(n4),n4=n2,n3)
     if (iprsup .ge. 1) write (lunit6,994) (nodvo2(n4),n4=n2,n3)
993  format( 9h nodvo1 :,3x,15i4)
994  format( 9h nodvo2 :,3x,15i4)
     if (iprsup .ge. 1) write (lunit6,995) nodom(n1),nodmum(n1),nodfum(n1)
995  format(/, 31h um mass node nr. = nodom(jm) =,i4,/, 14h slack buses :,3x,12hnodmum(jm) =,i4,10x,12hnodfum(jm) =,i4)
     !. determination of pointers for output vector :
     if (jtqout(n1) .eq. 0) go to 1000
     numout = numout + 1
     kumout(numout) = n1
     jumout(numout) = 1 + numfix
     if (jtqout(n1) .lt. 2) go to 1000
     numout = numout + 1
     kumout(numout) = n1
     jumout(numout) = 4 + numfix
     if (jtqout(n1) .ne. 3) go to 1000
     numout = numout + 1
     kumout(numout) = n1
     jumout(numout) = 5 + numfix
1000 if (jomout(n1) .eq. 0) go to 1010
     numout = numout + 1
     kumout(numout) = n1
     jumout(numout) = 2 + numfix
     if (jomout(n1) .lt. 2) go to 1010
     numout = numout + 1
     kumout(numout) = n1
     jumout(numout) = 6 + numfix
     if (jomout(n1) .ne. 3) go to 1010
     numout = numout + 1
     kumout(numout) = n1
     jumout(numout) = 7 + numfix
1010 if (jthout(n1) .eq. 0) go to 1020
     numout = numout + 1
     kumout(numout) = n1
     !  if output request for thetam (torque angle or rotor position)
     if (jthout(n1) .eq. 1) jumout(numout) = 3 + numfix
     !  if output request for tqexc (for sm type-59 data input)
     if (jthout(n1) .eq. 3) jumout(numout) = 11 + numfix
1020 n2 = 3 + ncld(n1) + nclq(n1)
     if (jtype(n1) .ne. 4) go to 1030
     n2 = n2 + 1
1030 do n3 = 1, n2
        n4 = kcoil(n1) - 1 + n3
        if (reacl(n4) .ge. 0.0) go to 1038
        write (lunit6, 1032) n1
1032    format( /, 42h error stop. a negative leakage inductance, 39h inductance is used to simulate machine, &
             7h number, i4,1h.)
        go to 9600
1038    if (jclout(n4) .eq. 0) go to 1040
        numout = numout + 1
        kumout(numout) = n1
        jumout(numout) = 11 + numfix + n3
        if (jclout(n4) .ne. 2) go to 1040
        if (n3 .eq. 1) jumout(numout) = numfix + 8
        if (n3 .eq. 2) jumout(numout) = numfix + 9
        if (n3 .eq. 3) jumout(numout) = numfix + 10
1040 end do
     if (numout .eq. numoui) go to 1050
     if (n1 .eq. 1) numbus = jumout(numout)
     n11 = jumout(numout)
     if (n11 .gt. numbus) numbus = n11
1050 end do
  if (numbus .le. ibsfix) go to 1060
  write (lunit6,1055)
1055 format( /, 35h overflow of um output name table, , 38h increase ibsfix on um dimension card.)
  go to 9600
  !  shifting entries of umoutp vector because of sm type-59
  !    request for transfer of variables to tacs :
1060 if (nsmach .eq. 0) go to 18010
  if (nsmtac .eq. 0) go to 18010
  do n5 = 1, nsmtac
     n6 = nsmtac + 1 - n5
     n7 = numout + 3 + n6
     umoutp(n7) = umoutp(n6)
18000 end do
  do n5 = 1,numout
18002 umoutp(n5) = 0.0
  end do
  n5 = numout + nsmtac + 3
  umoutp(numout+3) = n5
  !  umoutp(numout+3) is last entry of umoutp that is used
  umoutp(numout+2) = nangre
  !  umoutp(numout+2) .ne. 0.0 indicates request for angle transf.
  umoutp(numout+1) = - 9999.0
  !  umoutp(numout+1) .eq. -9999. is a flag for request of tacs
  !  transfer of um variables.
  if (iprsup .ge. 1) write (lunit6,18006) (umoutp(n1),n1=1,n5)
18006 format(/, 45h the um output table umoutp(numout+nsmtac+3):,/, 6(6x,e14.5)/(6(6x,e14.5)) )
18010 istart = 0
  if (numout .le. iotfix) go to 18020
  write (lunit6, 17960)
17960 format( /, 38h overflow of um output table, increase,35h in card for absolute um dimensions,/, 17h value of iotfix.,/, &
       37h remark : if sm type-59 data input is,30h included, then iotfix is also, /, 36h related to outputs of mech. Systems, &
       33h in all sm type-59 data machines.)
  go to 9600
18020 loopss(1) = 0
  loopss(2) = 0
  loopss(4) = 0
  loopss(10) = 0
  if (iprsup .ge. 1) write (lunit6,18040) numum,numout,nsmtac
18040 format(/, 40h exit  "umdata" :  numum  numout  nsmtac,/,17x,3i8)
  return
9600 call stoptp
  return
end subroutine umdata
!
!     subroutine umdatb.
!
subroutine umdatb (reacl, gpar, fpar, nodvo1, nodvo2, jcltac, jtype, nodom, ncld, jtmtac, reamdu, reamds, flxds, flxdr, &
     reamqu, flxqs, flxqr, jcdsat, jcqsat, nppair, rotmom, nclq, jtqout, jthout, reamqs, voltum, anglum, nodfum, nodmum, &
     kumout, jumout, jclout, dcoef, jomout, umoutp)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension nodvo2(1), jtmtac(1), flxqs(1), rotmom(1)
  dimension reacl(1),gpar(1),fpar(1),nodvo1(1),nodfum(1)
  dimension jcltac(1), jclout(1), jtype(1), nodom(1)
  dimension reamds(1), flxds(1), flxdr(1), reamqu(1)
  dimension flxqr(1), jcdsat(1), jcqsat(1), nppair(1)
  dimension ncld(1), nclq(1), jtqout(1),jomout(1),jthout(1)
  dimension reamqs(1),dcoef(1),voltum(1),anglum(1)
  dimension nodmum(1), kumout(1), jumout(1), umoutp(1)
  dimension reamdu(1)
  common /umlocal/ texta, d1,d2,d3,d17, stat59, fmum, rmvaum, rkvum, s1um, s2um, zlsbum, s1qum, s2qum, aglqum, raum, xdum, &
       squm, xdpum, xqpum, xdppum, xqppum, tdpum, tdppum, x0um, rnum, xnum, xfum, xdfum, xdkdum, xkdum, xkqum, xqkqum, &
       xgkqum, xqum, xqgum, xgum, distrf, hjum, dsynum, dmutum, spring, dabsum, tqppum, agldum, xlum, n1, n2, n3, n4, n5, n6, &
       n7, n8, n9, n10, n11, n12, n14, n15, n16, n17, n18, n19, n20, jr, jf, nexc, kconex, ibrexc, nstan, numasu, nmgen, nmexc, &
       ntypsm, netrun, netrum, nsmtpr, nsmtac, nrsyn, ntorq, mlum, nparum, ngroup, nall, nangre, nexcsw, limasu, lopss2, lopss1, &
       lopss8, lopss9, lopss10, lopss4, nshare
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'umdeck.ftn'
  include 'tacsar.ftn'
  include 'syncom.ftn'
  character*8 texta(101), text3,text4,text5
  character*8 textm, tesm1, tesm2, tesm3, tesm4, tesm5
  character*8 tesm6, tesm7, tesm8, tesm9, text6, text7
  data textm / 6hmore  /
  data tesm1, tesm2, tesm3, tesm4, tesm5, tesm6, tesm7, tesm8, tesm9 / 'smdata', 'tolera', 'parame', 'all   ', 'none  ' , 'extend', &
       'finish', ' part ', 'share ' /
  !     start reading sm type -50 to 59 data input format **********
17040 initum = 1
  nsmach = numum
  n1 = numum
  jtype(n1) = 13
  ncld(n1) = 2
  nclq(n1) = 2
  jtmtac(n1) = 0
  nmexc = 0
  n5 = ncltot + 1
  n6 = ncltot + 7
  do n10 = n5,n6
     fpar(n10) = 0.0
  end do
  !  *************************** read class 1 sm type-59 data
  do n5 = 1, 3
     if (n5 .ne. 1) call cimage
     n6 = ncltot + n5
     read (unit = abuff(1), fmt = 17050) n19, bus1, d1, d2, d3
17050 format(i2, a6, 2x, 3e10.6)
     if (noutpr .eq. 0) write (kunit6, 17052)  numum, n5
17052 format(5h+um -, i3, 23h   sm-59 class 1, card ,i1)
     if (n5 .ne. 1) go to 17056
     ntypsm = n19
     if (n19 .eq. 59) go to 17054
     if (n19 .eq. 50) go to 17054
     if (n19 .eq. 52) go to 17054
     write (lunit6,17053)
17053 format(/, 29h error stop. the um code does, 26h not accept dual machines.)
     go to 9600
17054 voltum(n1) = d1
     anglum(n1) = d3
     if (d2 .eq. 0.0) d2 = statfr
     rotmom(n1) = twopi * d2
     stat59 = d2
     !  creation of 2 excitation nodes :
     ntot = ntot + 1
     kode(ntot) = 0
     nodvo1(ncltot+4) = ntot
     nodvo2(ncltot+4) = 1
     bus(ntot) = trash
     ntot = ntot + 1
     kode(ntot) = 0
     nexc = ntot
     bus(nexc) = trash
     if (numum .gt. 5) go to 17056
     n3 = 41 + (numum - 1)*4
     bus(ntot-1) = texta(n3+1)
     bus(nexc) = texta(n3+2)
17056 do n10 = 1, ntot
        if (bus1 .ne. bus(n10)) go to 17060
        nodvo1(n6) = n10
        go to 17070
17060 end do
     write(lunit6, 17062) bus1
17062 format(/ , 40h error stop.   just-read sm class 1 card, 25h bears illegal node name:, a6)
     go to 9600
17070 end do
  if (iprsup .ge. 1) write(lunit6,17072) n1
17072 format(/, 10h *********,39h elements internally created for um nr.,i3,2h: ,12hnode to node,5x,3hibr,6x,2hit,2x, &
       6hkswtch,2x,6hlswtch,2x,6hkconst,9x,5hsfreq,/)
  !  set voltage source for excitation
  kconst = kconst + 1
  kconex = kconst
  nodfum(n1) = kconst
  iform(kconst) = 14
  node(kconst) = nexc
  kode(nexc) = - nexc
  sfreq(kconst) = epsiln * 1.0d+3
  tstart(kconst) = - 1.0
  tstop(kconst) = fltinf
  if (iprsup .ge. 1) write(lunit6,17076) node(kconst),kconst,sfreq(kconst)
17076 format(10h *********,21h field voltage source,23x,i4,44x,i4,e14.5)
  !  set series and parallel resistance for excitation
  call ibrinc
  ibrexc = ibr
  it = it + 1
  kbus(ibr) = nodvo1(ncltot+4)
  mbus(ibr) = nexc
  length(ibr) = 1
  nr(ibr) = - it
  tr(it) = epsiln
  itexc = it
  !  note : tr(it=itexc) will be changed later to 0.5 * rf
  tx(it) = 0.0
  c(it) = 0.0
  if (iprsup .ge. 1) write(lunit6,17080) mbus(ibr),kbus(ibr),ibr,it
17080 format(10h *********,21h series field resist.,23x,i4,4x,i4,4x,i4,4x,i4)
  call ibrinc
  it = it + 1
  kbus(ibr) = nexc
  mbus(ibr) = 1
  length(ibr) = 1
  nr(ibr) = - it
  tr(it) = epsiln
  tx(it) = 0.0
  c(it) = 0.0
  if (iprsup .ge. 1) write(lunit6,17082) mbus(ibr),kbus(ibr),ibr,it
17082 format(10h *********,21h parall field resist.,23x,i4,4x,i4,4x,i4,4x,i4)
  !  shorting damper and eddy current coils
  do n5 = 5, 7
     nodvo1(ncltot+n5) = 1
     nodvo2(ncltot+n5) = 1
17090 end do
  !  *************************** read class 2 sm type-59 data
  !  nstan = 1 is a flag for standard manufacturer data
17100 nstan = 0
  call cimage
  read (unit = abuff(1), fmt = 17102) text3
17102 format(a6)
  if (text3 .ne. tesm2) go to 17106
  if (noutpr .eq. 0) write (kunit6, 17104)  numum
17104 format(5h+um -, i3, 32h   sm-59 class 2, tolerance card)
  call cimage
17106 n19 = 1
  read (unit = abuff(1), fmt = 17107) text3, fmum
17107 format(a6, 18x, e8.0)
  if (text3 .ne. tesm3) go to 17200
  if (noutpr .eq. 0) write (kunit6, 17108)  numum
17108 format(5h+um -, i3, 33h   sm-59 class 2, par fit request)
  nstan = 1
  call cimage
  !  *************************** read class 3 sm type-59 data
17200 n2 = 0
  read (unit = abuff(1), fmt = 17210) numasu, nmgen, nmexc, n20, d17, rmvaum, rkvum, agldum, s1um, s2um
17210 format(3i2, i4, f6.0, 14x, 5e10.6)
  if (numasu .eq. 0) numasu = d17
  if (noutpr .eq. 0) write (kunit6, 17212)  numum
17212 format (5h+um -, i3, 17h   sm-59 class 3,, 19h general parameters)
  !   zlsbum is the stator base inductance, if multiplied with
  !      rotmom(n1), it becomes the stator base impedance.
  rmvaum = rmvaum * 1.0d+6
  rkvum = rkvum * 1.0d+3
  zlsbum = rkvum * rkvum
  zlsbum = zlsbum/(rotmom(n1) * rmvaum)
  jcdsat(n1) = 0
  if (agldum .lt. 0.0) jcdsat(n1) = 1
  if (agldum .lt. 0.0) agldum = - agldum
17214 nppair(n1) = n20/2
  n5 = 2
  n6 = nppair(n1) * n5
  if (n6 .eq. n20) go to 17218
  write (lunit6,17216) n1
17216 format( /, 41h error stop. erroneous data input because,35h an uneven number of poles has been, &
       26h specified for mach number, i4, 1h.)
  go to 9600
17218 call cimage
  read (unit = abuff(1), fmt = 17222) text3, text4, text5, aglqum, s1qum, s2qum
17222 format(3a6, 32x, 3e10.6)
  if (text3 .ne. blank) go to 17224
  if (text4 .ne. blank) go to 17224
  if (text5 .ne. blank) go to 17224
  go to 17226
17224 aglqum = 0.0
  s1qum = 0.0
  s2qum = 0.0
  go to 17230
17226 if (noutpr .eq. 0) write (kunit6, 17228)  numum
17228 format(5h+um -,i3, 17h   sm-59 class 3,, 23h q-axis saturation card)
  call cimage
17230 jcqsat(n1) = 0
  d1 = s1qum + s2qum
  if (d1 .ne. 0.0) jcqsat(n1) = 1
  if (aglqum .ge. 0.0) go to 17240
  jcqsat(n1) = 1
  aglqum = 1.6 * agldum
  s1qum = 1.5 * s1um
  s2qum = 1.5 * s2um
  !  reading standard manufacturer data :
17240 if (ntypsm .eq. 52) go to 17400
  if (ntypsm .eq. 53) go to 17400
  if (ntypsm .eq. 50) go to 17241
  if (ntypsm .eq. 51) go to 17241
  if (nstan .ne. 1) go to 17400
17241 n2 = 0
  read (unit = abuff(1), fmt = 17242) raum, xlum, xdum, xqum, xdpum, xqpum, xdppum, xqppum
17242 format(8e10.6)
  n2 = n2 + 1
  if (noutpr .eq. 0) write (kunit6, 17244)  numum, n2
17244 format(5h+um -,i3,17h   sm-59 class 3,,18h manufacture card ,i1)
  call cimage
  read (unit = abuff(1), fmt = 17246) tdpum, tqpum, tdppum, tqppum, x0um, rnum, xnum, netrum
17246 format(7e10.6, i10)
  n2 = n2 + 1
  if (noutpr .eq. 0) write (kunit6, 17244)  numum, n2
  !  conversion of pu system to stator refered si system :
  raum = raum * zlsbum * rotmom(n1)
  xlum = xlum * zlsbum
  xdum = xdum * zlsbum
  xqum = xqum * zlsbum
  xdpum = xdpum * zlsbum
  xqpum = xqpum * zlsbum
  xdppum = xdppum * zlsbum
  xqppum = xqppum * zlsbum
  x0um = x0um * zlsbum
  rnum = rnum * zlsbum * rotmom(n1)
  xnum = xnum * zlsbum
  !   establishing stator and mach table variables
  if (raum .eq. 0.0) raum = epsiln
  do n5 = 1,3
     gpar(ncltot+n5) = raum
     if (n5 .eq. 1) go to 17260
     reacl(ncltot+n5) = xlum
17260 end do
  reacl(ncltot+1) = x0um
  reamdu(n1) = xdum - xlum
  reamqu(n1) = xqum - xlum
  if (reamqu(n1) .gt. 0.0) go to 17262
  write(lunit6,6198) n1
6198 format( /, 41h error stop. incorrect um data input. the, 42h result is that the q-axis main inductance, /, 7x, &
       39h is either zero or negative for machine, 7h number, i4, 1h.)
  go to 9600
17262 if (xqum .ne. xqpum) go to 17290
  if (xqum .ne. xqppum) go to 17280
  gpar(ncltot+6) = 0.0
  gpar(ncltot+7) = 0.0
  reacl(ncltot+6) = 1.0
  reacl(ncltot+7) = 1.0
  go to 17290
17280 if (fmum .ne. 1.0) go to 17285
  gpar(ncltot+6) = 0.0
  reacl(ncltot+6) = 1.0
17285 if (fmum .eq. 0.0) fmum = 0.95
  if (fmum .lt. 1.0) xqpum = fmum * xqum
17290 if (xdum .ne. xdpum) go to 17310
  if (xdum .ne. xdppum) go to 17300
  gpar(ncltot+4) = 0.0
  gpar(ncltot+5) = 0.0
  reacl(ncltot+4) = 1.0
  reacl(ncltot+5) = 1.0
  go to 17310
17300 if (fmum .ne. 1.0) go to 17305
  gpar(ncltot+4) = 0.0
  reacl(ncltot+4) = 1.0
17305 if (fmum .eq. 0.0) fmum = 0.3
  if (fmum .lt. 1.0) xdpum = fmum * xdum
  !  establishing rotor variables :
17310 d1 = reamdu(n1) * reamdu(n1)
  if (reacl(ncltot+4) .ne. 1.0) go to 17320
  if (reacl(ncltot+5) .eq. 1.0) go to 17330
  d10 = d1/(xdum - xdppum)
  reacl(ncltot+5) = d10 - reamdu(n1)
  d10 = reamdu(n1) + reacl(ncltot+5)
  gpar(ncltot+5) = d10/tdppum
  go to 17330
17320 d10 = d1/(xdum - xdpum)
  reacl(ncltot+4) = d10 - reamdu(n1)
  d10 = reamdu(n1) + reacl(ncltot+4)
  gpar(ncltot+4) = d10/tdpum
  if (reacl(ncltot+5) .eq. 1.0) go to 17330
  d11 = xdum - xdppum + d10 - 2.0 * reamdu(n1)
  d11 = d11 * d1
  d12 = (xdum - xdppum) * d10 - d1
  reacl(ncltot+5) = d11/d12 - reamdu(n1)
  d11 = reamdu(n1) + reacl(ncltot+5)
  d12 = d1/d10
  gpar(ncltot+5) = (d11 - d12)/tdppum
17330 d1 = reamqu(n1) * reamqu(n1)
  if (reacl(ncltot+6) .ne. 1.0) go to 17340
  if (reacl(ncltot+7) .eq. 1.0) go to 17500
  d10 = d1/(xqum - xqppum)
  reacl(ncltot+7) = d10 - reamqu(n1)
  d10 = reamqu(n1) + reacl(ncltot+7)
  gpar(ncltot+7) = d10/tqppum
  go to 17500
17340 d10 = d1/(xqum - xqpum)
  reacl(ncltot+6) = d10 - reamqu(n1)
  d10 = reamqu(n1) + reacl(ncltot+6)
  gpar(ncltot+6) = d10/tqpum
  if (reacl(ncltot+7) .eq. 1.0) go to 17500
  d11 = xqum - xqppum + d10 - 2.0 * reamqu(n1)
  d11 = d11 * d1
  d12 = (xqum - xqppum) * d10 - d1
  reacl(ncltot+7) = d11/d12 - reamqu(n1)
  d11 = reamqu(n1) + reacl(ncltot+7)
  d12 = d1/d10
  gpar(ncltot+7) = (d11 - d12)/tqppum
  go to 17500
  !  processing standard manufacturer data completed.
  !  reading of inductance/resistance data input :
17400 n2 = 0
  read (unit = abuff(1), fmt = 17410) xfum, xdfum, xfkdum, xdum, xdkdum, xkdum
17410 format(6e10.6)
  n2 = n2 + 1
  if (noutpr .eq. 0) write (kunit6, 17412)  numum, n2
17412 format(5h+um -,i3,17h   sm-59 class 3,, 20h r and l param card ,i1)
  call cimage
  read (unit = abuff(1), fmt = 17420) xkqum, xqkqum, xgkqum, xqum, xqgum, xgum, netrum
17420 format(6e10.6, 10x, i10)
  n2 = n2 + 1
  if (noutpr .eq. 0) write (kunit6, 17412)  numum, n2
  call cimage
  read (unit = abuff(1), fmt = 17430) reacl(ncltot + 1), gpar(ncltot + 1), gpar(ncltot + 4), gpar(ncltot + 5), &
       gpar(ncltot + 6), gpar(ncltot + 7), rnum, xnum
17430 format(8e10.6)
  n2 = n2 + 1
  if (noutpr .eq. 0) write (kunit6, 17412)  numum, n2
  !  conversion pu system into stator refered si system and um
  !    parameter input
  rnum = rnum * zlsbum * rotmom(n1)
  xnum = xnum * zlsbum
  if (gpar(ncltot+1) .eq. 0.0) gpar(ncltot+1) = epsiln
  gpar(ncltot+2) = gpar(ncltot+1)
  gpar(ncltot+3) = gpar(ncltot+1)
  d1 = zlsbum * rotmom(n1)
  do n5 = 1, 7
17432 gpar(ncltot+n5) = gpar(ncltot+n5) * d1
  end do
  reamdu(n1) = xdfum
  reamqu(n1) = xqkqum
  reacl(ncltot+1) = reacl(ncltot+1) * zlsbum
  reacl(ncltot+2) = (xdum - reamdu(n1)) * zlsbum
  reacl(ncltot+3) = (xqum - reamqu(n1)) * zlsbum
  reacl(ncltot+4) = (xfum - reamdu(n1)) * zlsbum
  reacl(ncltot+5) = (xkdum - reamdu(n1)) * zlsbum
  reacl(ncltot+6) = (xkqum - reamqu(n1)) * zlsbum
  reacl(ncltot+7) = (xgum - reamqu(n1)) * zlsbum
  reamdu(n1) = reamdu(n1) * zlsbum
  reamqu(n1) = reamqu(n1) * zlsbum
  !  reduction factor 1/kf = ifb/isb , stored in dcoef(n1) :
17500 dcoef(n1) = reamdu(n1)*agldum * rotmom(n1)/rkvum
  !  start processing saturation :
  flxds(n1) = 0.0
  flxqs(n1) = 0.0
  reamds(n1) = 0.0
  reamqs(n1) = 0.0
  flxdr(n1) = 0.0
  flxqr(n1) = 0.0
  if (jcdsat(n1) .eq. 0) jcqsat(n1) = 0
  if (jcdsat(n1) .eq. 0) go to 17510
  reamds(n1) = 0.2*reamdu(n1)*agldum/(s2um-s1um)
  d1 = reamds(n1) - epsiln
  if (d1 .le. reamdu(n1)) go to 17504
  write(lunit6,17502)
17502 format(/,  38h error stop. you have chosen incorrect, 42h d - axis saturation parameters s1 and s2.)
  write (lunit6,17503) n1
17503 format(/,45h the result is a saturated inductance greater, 38h than the unsaturated one. the machine, &
       24h concerned is um number , i4, 1h.,/, 41h please take realistically either a lower, &
       39h value for s1 or a higher value for s2.)
  go to 9600
17504 flxds(n1) = (s2um - 1.2*s1um) * rkvum/rotmom(n1)
  d2 = s2um - s1um - 0.2*agldum
  if (d2 .ne. 0.0) go to 17505
  jcdsat(n1) = 0.0
  jcqsat(n1) = 0.0
  go to 17510
17505 flxds(n1) = flxds(n1) /d2
  d10 = 1.0 - reamds(n1)/reamdu(n1)
  d10 = d10 * flxds(n1)
  if (d10 .ge. 0.0) go to 97510
  write (lunit6,97506)
97506 format(/,39h error stop.  you have chosen incorrect, 40 h d-axis saturation parameters s1 and s2.)
  write (lunit6,97507) n1
97507 format(40h the result is an unrealistic saturation,47h characteristic with the saturated line-segment, &
       46h not intersecting the unsaturated line-segment, /, 45h in the first quadrant. the machine concerned, &
       13h is um number,i4,1h.)
  go to 9600
97510 if (jcqsat(n1) .eq. 0) go to 17510
  reamqs(n1) = 0.2*reamqu(n1)*aglqum/(s2qum-s1qum)
  d1 = reamqs(n1) - epsiln
  if (d1 .le. reamqu(n1)) go to 17508
  write(lunit6,17506)
17506 format(/,  38h error stop. you have chosen incorrect, 42h q - axis saturation parameters s1 and s2.)
  write(lunit6,17503) n1
  go to 9600
17508 flxqs(n1) = (s2qum-1.2*s1qum) * rkvum/rotmom(n1)
  d2 = s2qum - s1qum - 0.2*aglqum
  if (d2 .ne. 0.0) go to 17509
  jcqsat(n1) = 0.0
  go to 17510
17509 flxqs(n1) = flxqs(n1)/d2
  d10 = 1.0 - reamqs(n1)/reamqu(n1)
  d10 = d10 * flxqs(n1)
  if (d10 .ge. 0.0) go to 17510
  write(lunit6,97520)
97520 format(/,39h error stop.  you have chosen incorrect, 40h q-axis saturation paramaters s1 and s2.)
  write(lunit6,97507) n1
  go to 9600
  !  start processing neutral element values :
17510 d2 = 1.0d+2
  if (netrum .eq. 1) rnum = d2
  d1 = rnum + xnum * 1.0d+3
  if (d1 .gt. d2) rnum = d2
  if (d1 .gt. d2) xnum = 0.0
  n7 = 1
  !  creation of neutral node
  do n5 = 1,3
     n6 = ncltot + n5
     if (d1 .eq. 0.0) go to 17520
     if (n5 .gt. 1) go to 17520
     ntot = ntot + 1
     kode(ntot) = 0
     n7 = ntot
     bus(ntot) = trash
     if (numum .gt. 5) go to 17520
     bus(ntot) = texta(numum+60)
17520 nodvo2(n6) = n7
17530 end do
  !  create high resistances in parallel over coils if
  if (loopss(8) .ne. 0) go to 17536
  do n5 = 1, 3
     call ibrinc
     it = it + 1
     kbus(ibr) = nodvo1(ncltot+n5)
     mbus(ibr) = nodvo2(ncltot+n5)
     length(ibr) = 1
     nr(ibr) = - it
     tr(it) = 1.0d+8
     tx(it) = 0.0
     c(it) = 0.0
     if (iprsup .ge. 1) write (lunit6,17531) mbus(ibr),kbus(ibr),ibr,it
17531 format(10h *********,19h high power resist.,25x,i4,4x,i4,4x,i4,4x,i4)
17532 end do
  !  insertion of neutral impedance
17536 if (d1 .eq. 0.0) go to 17540
  call ibrinc
  it = it + 1
  kbus(ibr) = nodvo2(ncltot+1)
  mbus(ibr) = 1
  length(ibr) = 1
  nr(ibr) = - it
  tr(it) = rnum
  if (xopt .eq. 0.0) tx(it) = xnum * 1.0d+3
  if (xopt .ne. 0.0) tx(it) = xnum * twopi * xopt
  c(it) = 0.0
  if (iprsup .ge. 1)write(lunit6,17539) mbus(ibr),kbus(ibr),ibr,it
17539 format(10h *********,18h neutral impedance,26x,i4,4x,i4,4x,i4,4x,i4)
  !  set 0.5*rf for external exciter resistance:
  !    (internal field resistance adjusted at 17958)
17540 tr(itexc) = 0.5*gpar(ncltot+4)/(dcoef(n1)*dcoef(n1))
  !  *************************** read class 4 sm type-59 data
  d10 = 1.0d+6
  nsmtpr = nsmtac
  nrsyn = 0
  ntorq = 0
  !   ntotst + k = emtp node nr of mass nr. k
  !   nmgen now becomes emtp node nr of generator mass
  !   nmexc now becomes emtp node nr of exciter mass
  jtmtac(n1) = 0
  call cimage
  read (unit = abuff(1), fmt = 17550) text3
17550 format(a6)
  if (text3 .ne. tesm9) go to 17570
  read (unit = abuff(1), fmt = 17552) n3, n4
17552 format(6x,2i6)
  if (noutpr .eq. 0) write(kunit6,17553) numum
17553 format(5h+um -,i3,33h   sm-59 class 4, share mech netw)
  if (n1 .ne. n3 .and. n1 .ne. n4) go to 17560
  write (lunit6,15827) n1
15827 format(/,21h error stop. this um-,i3,15h is supposed to,45h share its mech network with other um's. what, &
       41h should be specified on this card are the, /, 36h numbers of these other um's without, &
       40h including the number of the um which is,27 h being processed right now.)
  go to 9600
17560 n6 = n1
  if (n3 .ne. 0 .and. n3 .lt. n6) n6 = n3
  if (n4 .ne. 0 .and. n4 .lt. n6) n6 = n4
  !  only the lowest numbered um of the machines sharing
  !   a common mech network are specified with the mech network
  !   structure (through the mass cards of class 4). the flag
  !   jtmtac(n1)=-999999 indicates that um number n1 is not the
  !   this lowest numbered um.
  if (n1 .eq. n6) jtmtac(n1) = - ntot
  if (n1 .ne. n6) jtmtac(n1) = - 999999
  ntotst = - jtmtac(n6)
  go to 17571
17570 ntotst = ntot
17571 nmgen = ntotst + nmgen
  if (nmexc .eq. 0) go to 17575
  nmexc = ntotst + nmexc
  !. storing in fpar and create current source for "tqexc" :
  kconst = kconst + 1
  iform(kconst) = 14
  node(kconst) = - nmexc
  kode(nmexc) = 0
  crest(kconst) = 0.0
  tstart(kconst) = - 1.0
  tstop(kconst) = fltinf
  sfreq(kconst) = 0.00001
  if (iprsup .ge. 1) write(lunit6,17572) node(kconst),kconst,sfreq(kconst)
17572 format(10h *********, 22h exciter torque source,22x,i4,44x,i4,e14.5)
  fpar(ncltot+4) = - kconst
  fpar(ncltot+5) = nmexc
  fpar(ncltot+6) = nexc
17575 nodom(n1) = nmgen
  !  set all mechanical nodes
  if (jtmtac(n1) .eq. -999999) go to 17700
  ntot = ntot + 2 * numasu
  n6 = ntotst + 1
  do n5 = n6, ntot
17577 kode(n5) = 0
  end do
  !  nsmtpr is to store the value of nsmtac of the previous mach.
  !  nsmtac is the total nr of variables to be passed to tacs.
  !  ntorq is the number of masses with distrf .ne. 0.0 , thus
  !    for the masses with an externally applied torque.
  !  Storage approach in umoutp for each sm :
  !    the first numasu cells behind nsmtpr is for distrf.
  !    the second numasu cells for l12 (and of course l23,etc).
  !    the third numasu cells for l12/r12 .
  !    then the rest for quantities reflecting tacs transfer.
  !    after completion of reading all sm data, the 3*numasu
  !      cells behind nsmtpr are destroyed and the quantities
  !      for tacs transfer are shifted to behind nsmtpr.
  !  Storage approach for each variable to be transfered :
  !   * for voltages (speeds) :
  !          first cell = code nr provided with neg sign
  !          second cell = node nr
  !          third cell = tacs table nr (ntotac)
  !   * for currents (torques) :
  !        same as voltages, except third cell = switch table nr.
  !   * for mass angles :
  !          first, second and third cell same as voltages
  !          fourth cell = angle history, angle part
  !          fifth cell = angle history, omega*deltat/2 part
17580 do n5 = 1, numasu
     if (n5 .eq. 1 .and. jtmtac(n1) .eq. 0) go to 17582
     call cimage
17582 n17 = 0
     read (unit = abuff(1), fmt = 17600) mlum, n17, distrf, hjum, dsynum, dmutum, spring, dabsum, bus1
17600 format(i2,i6,2x,6e10.6,a6)
     if (mlum .eq. 0) mlum = n17
     if (noutpr .eq. 0) write (kunit6, 17602)  numum, mlum
17602 format(5h+um -,i3, 26h   sm-59 class 4, mass nr.,i6)
     if (mlum .le. numasu) go to 17604
     write(lunit6,17603) mlum,numasu
17603 format(/,45h error stop. the last card indicates the data, 15h of mass number,i6,24h, and yet in class-3 of, &
          /, 40h of sm data cards you have specified the, 43h total number of masses "numas" to be equal, &
          3h to,i6,1h.,/, 29h please remove this conflict.)
     go to 9600
     !  output names of mechanical nodes :
17604 bus(ntotst+mlum) = bus1
     !  creating mass element
     call ibrinc
     it = it + 1
     kbus(ibr) = ntotst + mlum
     mbus(ibr) = 1
     length(ibr) = 1
     nr(ibr) = - it
     tr(it) = 0.0
     tx(it) = 0.0
     !  the following factor of d10=1.0d+6 is due to micro f or mho.
     c(it) = d10*d10*hjum/23.73
     if (copt .ne. 0.0) c(it) = c(it)*copt*twopi
     if (iprsup .ge. 1) write (lunit6,17605) mlum,mbus(ibr),kbus(ibr),ibr,it
17605 format(10h *********,16h mass branch nr.,i3,25x,i4,4x,i4,4x,i4,4x,i4)
     !  creating spring element :
     n6 = nsmtpr + numasu + mlum + 1
     if (mlum .eq. numasu) go to 17608
     if (spring .eq. 0.0) go to 17608
     call ibrinc
     it = it + 1
     kbus(ibr) = ntotst + numasu + mlum
     mbus(ibr) = ntotst + mlum + 1
     length(ibr) = 1
     nr(ibr) = - it
     tr(it) = 0.0
     !  the following factor of 1.0d+3 is because of milli henry
     d1 = d10 * spring/0.73756
     if (xopt .eq. 0.0) tx(it) = 1.0d+3/d1
     if (xopt .ne. 0.0) tx(it) = twopi*xopt/d1
     c(it) = 0.0
     if (iprsup .ge. 1) write(lunit6,17606) mbus(ibr),kbus(ibr),ibr,it
17606 format(10h *********,14h spring branch,30x,i4,4x,i4,4x,i4,4x,i4)
     umoutp(n6) = 1.0/d1
     go to 17610
17608 umoutp(n6) = 0.0
     !  creating synchronous damping and synchronous speed source :
17610 if (dsynum .eq. 0.0) go to 17620
     ntot = ntot + 1
     nrsyn = nrsyn + 1
     kode(ntot) = 0
     bus(ntot) = trash
     if (numum .gt. 5) go to 17614
     if (n5 .ne. 1) go to 17612
     bus(ntot) = texta(numum+65)
17612 if (n5 .ne. 2) go to 17614
     bus(ntot) = texta(numum+70)
17614 kconst = kconst + 1
     iform(kconst) = 14
     node(kconst) = ntot
     kode(ntot) = - ntot
     sfreq(kconst) = 0.00001
     tstart(kconst) = - 1.0
     tstop(kconst) = fltinf
     crest(kconst) = rotmom(n1)/nppair(n1)
     if (iprsup .ge. 1) write(lunit6,17615) node(kconst),kconst,sfreq(kconst)
17615 format(10h *********, 30h synchr damping voltage source,14x,i4,44x,i4,e14.5)
     call ibrinc
     it = it + 1
     kbus(ibr) = ntotst + mlum
     mbus(ibr) = ntot
     length(ibr) = 1
     nr(ibr) = - it
     tr(it) = 0.73756/dsynum
     tx(it) = 0.0
     c(it) = 0.0
     if (iprsup .ge. 1) write(lunit6,17616) mlum,mbus(ibr),kbus(ibr),ibr,it
17616 format(10h *********, 36h synchr. damping branch for mass nr.,i3,5x,i4,4x,i4,4x,i4,4x,i4)
     !  creating mutual damping :
17620 n6 = nsmtpr + 2*numasu + mlum + 1
     if (mlum .eq. numasu) go to 17628
     if (dmutum .eq. 0.0) go to 17628
     call ibrinc
     it = it + 1
     kbus(ibr) = ntotst + numasu + mlum
     mbus(ibr) = ntotst + mlum + 1
     length(ibr) = 1
     nr(ibr) = - it
     tr(it) = 0.73756/dmutum
     tx(it) = 0.0
     c(it) = 0.0
     if (iprsup .ge. 1) write (lunit6,17626) mbus(ibr),kbus(ibr),ibr,it
17626 format(10h *********, 22h mutual damping branch,22x,i4,4x,i4,4x,i4,4x,i4)
     if (spring .eq. 0.0) go to 17628
     d1 = 0.73756/(d10*spring)
     umoutp(n6) = d1/tr(it)
     go to 17630
17628 umoutp(n6) = 0.0
     !  creating absolute damping :
17630 if (dabsum .eq. 0.0) go to 17640
     call ibrinc
     it = it + 1
     kbus(ibr) = ntotst + mlum
     mbus(ibr) = 1
     length(ibr) = 1
     nr(ibr) = - it
     tr(it) = 0.73756/dabsum
     tx(it) = 0.0
     c(it) = 0.0
     if (iprsup .ge. 1) write(lunit6,17638) mlum,mbus(ibr),kbus(ibr),ibr,it
17638 format(10h *********,34h abslt damping branch for mass nr.,i3,7x,i4,4x,i4,4x,i4,4x,i4)
     !  storing applied torque distribution factors distrf in
     !   umoutp and the corresponding mass node nrs in kumout
17640 if (distrf .eq. 0.0) go to 17660
     ntorq = ntorq + 1
     kumout(ntorq) = ntotst + mlum
     if (ntorq .ne. 1) go to 17650
     distr1 = distrf
     !  distr1 is the first mass encountered with distrf .ne. 0.0
     umoutp(nsmtpr+1) = 1.0d+3
     go to 17660
17650 umoutp(nsmtpr+ntorq) = 1.0d+3 * distrf/distr1
17660 end do
  !  output names of nodes next to measurement switches :
  n6 = numasu - 1
  if (numasu .eq. 1) go to 17672
  do n5 = 1, n6
     n7 = ntotst + numasu + n5
     n8 = ntotst + n5 + 1
     bus(n7) = bus(n8)
17670 end do
17672 n7 = ntotst + 2*numasu
  bus(n7) = trash
  if (numum .gt. 5) go to 17700
  n3 = 41 + (numum - 1)*4
  bus(n7) = texta(n3)
  !  *************************** read class 5 sm type-59 data
17700 call cimage
  read (unit = abuff(1), fmt = 17701) texxt3
17701 format(a6)
  if (text3 .eq. blank) go to 17770
  if (text3 .eq. tesm4) go to 17750
  if (text3 .eq. tesm5) go to 17750
  if (text3 .eq. tesm6) go to 17750
  n15 = 68
  if (numasu .le. 68) n15 = numasu
  read (unit = abuff(1), fmt = 17702) nparum, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, (jumout(i), i = 1, n15)
17702 format(80i1)
  if (jtmtac(n1) .ne. -999999) go to 17703
  do i = 1, n15
     jumout(i) = 0
  end do
17703 if (noutpr .eq. 0) write (kunit6, 17704)  numum
17704 format(5h+um -,i3, 29h   sm-59 class 5, output card)
  n16 = 0
  n17 = 1
17706 if (numasu .le. n15) go to 17710
17708 call cimage
  n16 = n16 + 1
  n14 = 68*n17 + (n16 - 1)*80 + 1
  n15 = 68*n17 + n16*80
  if (numasu .le. n15) n15 = numasu
  read (unit = abuff(1), fmt = 17702) (jumout(i), i = n14, n15)
  if (jtmtac(n1) .ne. -999999) go to 17709
  do i = n14, n15
     jumout(i) = 0
  end do
17709 if (noutpr .eq. 0) write (kunit6, 17704)  numum
  go to 17706
17710 if (n3 .eq. 0) go to 17716
  do n15 = 1,3
     jclout(ncltot+n15) = 2
  end do
17716 if (n2 .ne. 0) kssout = 1
  if (n4 .ne. 0) jclout(ncltot+4) = 1
  if (n5 .ne. 0) jclout(ncltot+5) = 1
  if (n6 .ne. 0) jclout(ncltot+6) = 1
  if (n7 .ne. 0) jclout(ncltot+7) = 1
  if (n9 .ne. 0) jtqout(n1) = 1
  if (n10 .eq. 1) jthout(n1) = 3
  if (nmexc .eq. 0) jthout(n1) = 0
  if (n10 .eq. 3) jthout(n1) = 1
  if (n3 .ne. 0) go to 17726
  if (n11 .eq. 0) go to 17726
  do n15 = 1, 3
     jclout(ncltot+n15) = 1
  end do
17726 if (n12 .eq. 0) go to 17728
  if (n12 .eq. 2) go to 17727
  jtqout(n1) = 2
  jomout(n1) = 2
  go to 17728
17727 jtqout(n1) = 3
  jomout(n1) = 3
  !  interfacing if mass speed output is requested :
17728 do n15 = 1,numasu
     if (jumout(n15) .eq. 0) go to 17734
     if (jumout(n15) .eq. 2) go to 17734
     nv = nv + 1
     if (nv .le. lsiz12) go to 17732
     write (lunit6, 17730) numum
17730 format(/, 36h error stop.   output nv .gt. lsiz12, 19h for speeds of um -, i3)
     go to 9600
17732 ibrnch(nv) = ntotst + n15
     jbrnch(nv) = 1
17734 end do
  !  set measurement switches and shaft torque outputting :
  !   (kswsta+1) is first switch to be set from mass 1 to mass 2
  if (jtmtac(n1) .eq. -999999) go to 17800
  kswsta = kswtch
  n16 = numasu - 1
  if (numasu .eq. 1) go to 17742
  do n15 = 1, n16
     kswtch = kswtch + 1
     ndx1 = kswtch + lswtch
     kmswit(kswtch) = ntotst + n15 + numasu
     kmswit(ndx1)  =  ntotst + n15
     kswtyp(kswtch) = 0
     tclose(kswtch) = - 1.0
     topen(kswtch) = fltinf
     kpos(kswtch) = 11
     if (jumout(n15) .eq. 2) kpos(kswtch) = - 11
     if (jumout(n15) .eq. 3) kpos(kswtch) = - 11
     if (iprsup .ge. 1) write (lunit6,17738) kmswit(kswtch),kmswit(ndx1),kswtch,lswtch
17738 format(10h *********,21h torque sensor switch,23x,i4,4x,i4,20x,i4,4x,i4)
17740 end do
  !  create small series resistance for applied torque to um mass
17742 call ibrinc
  it = it + 1
  kbus(ibr) = nmgen
  mbus(ibr) = ntotst + 2*numasu
  length(ibr) = 1
  nr(ibr) = - it
  tr(it) = 1.0
  tx(it) = 0.0
  c(it) = 0.0
  if (iprsup .ge. 1) write(lunit6,17748) mbus(ibr),kbus(ibr),ibr,it
17748 format(10h *********,40h small series resist for apll gen torque, 4x,i4,4x,i4,4x,i4,4x,i4)
  go to 17800
17750 n15 = 1
  read (unit = abuff(1), fmt = 17752) nparum, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12
17752 format(10x, 12i5)
  if (noutpr .eq. 0) write (kunit6, 17704)  numum
  if (text3 .ne. tesm4) go to 17756
  if (jtmtac(n1) .eq. -999999) go to 17710
  do n15 = 1,numasu
     jumout(n15) = 3
  end do
  go to 17710
17756 if (text3 .ne. tesm5) go to 17760
  do n15 = 1,numasu
     jumout(n15) = 0
  end do
  go to 17710
17760 n15 = 0
  n16 = 0
  n17 = 0
  go to 17708
  !  in case of new sm-59 output request usage :
17770 if (noutpr .eq. 0) write(kunit6,17772) numum
17772 format(5h+um -,i3,34h   sm-59 class 4, termination card)
17774 call cimage
  read (unit = abuff(1), fmt = 17701) text3
  if (text3 .eq. blank) go to 17799
  read (unit = abuff(1), fmt = 17776) ngroup, nall,(ndum(i), i = 1, 12)
17776 format(2x,2i1,4x,12i6)
  if (noutpr .eq. 0) write(kunit6,17778) numum,ngroup
17778 format(5h+um -,i3,30h   sm-59 class 5, output group,i2)
  if (ngroup .gt. 0 .and. ngroup .le. 4) go to 97778
  write (lunit6,97777) ngroup
97777 format(/,43h error stop. the last-read card is an sm-59,37h output request card for output group,i2,1h., &
       39h this is an illegal output group as you, /, 43h could find out by consulting the EMTP rule, &
       47h book regarding the sm-59 output request rules.)
  go to 9600
97778 go to (17782,17779,17793,17798), ngroup
  !  treatment of group 2 (angles) output request : ignored
17779 if (noutpr .eq. 0) write(lunit6,17780)
17780 format(/,39h warning : this output request for mass, 34h angles is ignored by the um. they, &
       40h are to be obtained from the tacs output, /, 44h after request of transfer into tacs through, &
       44h the class-6 sm-59 data input cards (consult, 16h EMTP rule book),/)
  go to 17774
  !   treatment of group 1 (electrical variables) output request :
17782 if (nall .eq. 0) go to 17784
  kssout = 1
  if (nmexc .ne. 0) jthout(n1) = 3
  if (nmexc .eq. 0) jthout(n1) = 1
  jtqout(n1) = 3
  jomout(n1) = 3
  do i = 1,7
     jclout(ncltot+i) = 1
  end do
  go to 17774
17784 do i = 1, 12
     if (ndum(i) .eq. 0) go to 17786
     do j = 4, 10
        if (ndum(i) .ne. j) go to 17785
        n2 = j
        if (j .gt. 7) n2 = j - 7
        jclout(ncltot+n2) = 1
        go to 17786
17785 end do
     if (ndum(i) .eq. 15) jthout(n1) = 3
     if (ndum(i) .ne. 14) go to 17786
     jtqout(n1) = 1
     jomout(n1) = 1
     if (jthout(n1) .eq. 0) jthout(n1) = 1
17786 end do
  n3 = 0
  do i = 1, 12
     if (ndum(i) .eq. 0) go to 17792
     do j = 1, 3
        if (ndum(i) .ne. j) go to 17791
        if (jclout(ncltot+j) .eq. 0) go to 17790
        n3 = 1
17788   format(/,39h warning : simultaneous request of park, 41h domain currents and real currents of the, &
             30h power windings of this um nr.,i3, 7h is not, /, 42h honored. the output will only display the, &
             15h real currents.,/)
        go to 17792
17790   jclout(ncltot+j) = 2
        go to 17792
17791 end do
     if (ndum(i) .lt. 12) go to 17792
     if (ndum(i) .gt. 13) go to 17792
     jtqout(n1) = 3
     jomout(n1) = 3
17792 end do
  if (noutpr .eq. 0 .and. n3 .ne. 0) write(lunit6,17788) num
  go to 17774
  !   treatment of group 3 (mass speeds) output request :
17793 n2 = 2
  n3 = 1
17794 if (jtmtac(n1) .ne. -999999) go to 17796
  do i = 1, numasu
     jumout(i) = 0
  end do
  go to 17774
17796 if (nall .eq. 0) go to 97797
  do i = 1, numasu
     if (jumout(i) .eq. n2) jumout(i) = 3
     if (jumout(i) .eq. 0) jumout(i) = n3
17797 end do
  go to 17774
97797 do i = 1,12
     n15 = ndum(i)
     if (n15 .eq. 0) go to 97798
     if (jumout(n15) .eq. n2) jumout(n15) = 3
     if (jumout(n15) .eq. 0) jumout(n15) = n3
97798 end do
  go to 17774
  !   treatment of group 4 (torques) output request :
17798 n2 = 1
  n3 = 2
  go to 17794
  !   blank card ending sm-59 output request :
17799 if (noutpr .eq. 0) write(kunit6,97799) numum
97799 format(5h+um -,i3,34h   sm-59 class 5, termination card)
  go to 17728
  !  *************************** read class 6 sm type-59 data
17800 nangre = 0
  nexcsw = 0
  ntacb = ntotac
  !  nangre is first entry of umoutp that indicates angle transfer
  n19 = 0
  !  n19 = 1 is flag for tacs control of total applied torque.
  niunrs = iuty(kiuty+1)
17810 call cimage
  n17 = 0
  !  n17 is in reading class 6 reserved as flag for modified
  !    format of code numbers if n20 .eq. 0
  read (unit = abuff(1), fmt = 17812) n20, bus1, n2
17812 format(i2,a6,i6)
  if (n20 .eq. 0) n17 = 3 * limasu
  if (n20 .eq. 0) n20 = n2
  if (n20 .eq. 0) go to 17816
  if (noutpr .eq. 0) write (kunit6, 17814)  numum
17814 format(5h+um -,i3,30h   sm-59 class 6, tacs request)
17816 if (bus1 .ne. tesm7) go to 17820
  read (unit = abuff(1), fmt = 17817) bus1, bus2
17817 format(2x,2a6)
  if (bus2 .ne. tesm8) go to 17818
  write (lunit6,17053)
  go to 9600
17818 if (noutpr .eq. 0) write (kunit6, 17819)  numum
17819 format(5h+um -, i3, 20h   reading completed)
  if (n19 .eq. 0) go to 17890
  go to 17900
  !  request for tacs control of field voltage :
17820 n4 = 71 + n17
  if (n20 .ne. n4) go to 17830
  do n5 = 1, ktab
     ndx1 = ilntab( klntab + n5 )
     if (bus1 .eq. texvec(ndx1)) go to 17827
17825 end do
  write (lunit6,17826) bus1
17826 format(/, 40h error stop.   just-read card requesting, 36h control by tacs, bears unrecognized,/, 12h tacs name :, a6)
  go to 9600
17827 tstop(kconex) = 0.0
  !  create type-60 source :
  kconst = kconst + 1
  iform(kconst) = 60
  node (kconst) = nexc
  kode(nexc) = - nexc
  bus(nexc) = bus1
  sfreq(kconst) = n5
  tstart(kconst) = 0.0
  tstop(kconst) = fltinf
  if (iprsup .ge. 1) write (lunit6,17828) node(kconst),kconst,sfreq(kconst)
17828 format(10h *********,42h tacs originated field volt type-60 source, 2x,i4,44x,i4,e14.5)
  write (lunit6,17829)
17829 format(/, 41h note: the emtp connectivity listing will, 44h show one of the nodes bearing the tacs name, &
           41h as used in the last read card. no reason, /, 42h to get alarmed, since this node is one of, &
           42h the auxiliary nodes created internally by, 15h the u.m. code.,/)
  go to 17810
  ! request for transfer to tacs of exciter voltage :
17830 n4 = 73 + n17
  if( n20 .ne. n4  .and.  n20 .ne. (n4+1)  .and. n20 .gt. ( 3*limasu ) )   go  to  17870
  !     load address of bus1 into tacs array 'ud1' ***************
  ndy5 = kud1
  do ijk = niunrs, niu
     ndx1 = ilntab( kaliu+ijk )
     n1iu = iuty( kiuty+ijk )
     if( n1iu .ne. 92 )   go  to  15481
     if( bus1 .ne. texvec( ndx1 ) )  go to 15481
     go to 15482
15481 ndy5 = ndy5 + 5
  end do
  kill = 108
  lstat( 19 ) = 15481
  lstat( 15 ) = lbstac
  lstat( 16 ) = ntotac
  lstat( 14 ) = n20
  lstat( 17 ) = -1
  lstat( 13 ) = 0
  bus6 = bus1
  jr = 1
  return
15482 ntotac = ntotac + 1
  ud1( ndy5+2 ) = ntotac
  if( ntotac .le. lbstac )  go  to 17834
  write (lunit6,17831)
17831 format(/, 37h error stop.   overflow lbstac during,34h execution of request in umdata to)
  write (lunit6,17832) numum
17832 format( /,39h pass um variables to tacs. the machine,38h in question, um -,i3, 12h,is provided)
  write (lunit6,17833)
17833 format(/, 28h with sm type-59 data input.)
  go to 9600
17834 ismtac(ntotac) = n20
  if (n20 .ne. n4) go to 17840
  n3 = 3*numasu + nsmtac
  umoutp(n3+1) = - 5.0
  umoutp(n3+2) = nexc
  umoutp(n3+3) = ntotac
  nsmtac = nsmtac + 3
  go to 17810
  !  request for transfer to tacs of exciter current :
17840 n4 = 74 + n17
  if (n20 .ne. n4) go to 17850
  !    create additional exciter node to insert switch
  ntot = ntot + 1
  kode(ntot) = 0
  nexcsw = ntot
  bus(nexcsw) = trash
  if (numum .gt. 5) go to 17842
  n3 = 41 + (numum - 1)*4
  bus(nexcsw) = texta(n3+3)
  !    create switch in exciter circuit
17842 kswtch = kswtch + 1
  kswexc = kswtch
  ndx1 = kswtch + lswtch
  kmswit(kswtch) = nexcsw
  kmswit(ndx1) = nexc
  kswtyp(kswtch) = 0
  tclose(kswtch) = - 1.0
  topen(kswtch) = fltinf
  kpos(kswtch) = 11
  if (iprsup .ge. 1) write(lunit6,17843) kmswit(kswtch),kmswit(ndx1),kswtch,lswtch
17843 format(10h *********,28h field current sensor switch,16x,i4,4x,i4,20x,i4,4x,i4)
  !    connect exciter series resistance to new switch node
  mbus(ibrexc) = nexcsw
  n3 = 3*numasu + nsmtac
  umoutp(n3+1) = - 4.0
  umoutp(n3+2) = kswexc
  umoutp(n3+3) = ntotac
  nsmtac = nsmtac + 3
  go to 17810
  !  transfer to solvum for angle history calculations if angle
  !    transfer to tacs is requested :
17850 if (jtmtac(n1) .eq. -999999) go to 17810
  if (n20 .gt. limasu) go to 17858
  !  note : in case that just one mass angle is requested to be
  !         passed to tacs, then the angle history of all masses
  !         needs to be transfered to solvum.
  if (nangre .ne. 0) go to 17854
  nangre = nsmtac + 1
  do n5 = 1, numasu
     n3 = 3*numasu + nsmtac
     n4 = nsmtpr + n5
     umoutp(n3+1) = - 299
     umoutp(n3+2) = ntotst + n5
     umoutp(n3+3) = 0.0
     umoutp(n3+4) = umoutp(n4)
     umoutp(n3+5) = umoutp(numasu+n4)
     nsmtac = nsmtac + 5
     if (n5 .ne. 1) go to 17852
     umoutp(n3+1) = - 300.0 - kswsta
     umoutp(n3+4) = 5.0 * numasu
     umoutp(n3+5) = numasu
17852 end do
  !  request for transfer to tacs of mass angles
17854 continue
  n3 = 3*numasu + nangre
  if (n20 .eq. 1) go to 17857
  n6 = n3 + (n20-1)*5
  umoutp(n6) = - 3.0
17857 n6 = n3 + 2 + (n20-1)*5
  umoutp(n6) = ntotac
  go to 17810
  !  request for transfer to tacs of mass speeds :
17858 if (jtmtac(n1) .eq. -999999) go to 17810
  n10 = 2*limasu
  if (n20 .gt. n10) go to 17860
  n3 = 3*numasu + nsmtac
  umoutp(n3+1) = - 2.0
  umoutp(n3+2) = ntotst + n20 - limasu
  umoutp(n3+3) = ntotac
  nsmtac = nsmtac + 3
  go to 17810
  !  request for transfer to tacs of shaft torques :
17860 if (jtmtac(n1) .eq. -999999) go to 17810
  n10 = 3 * limasu
  if (n20 .gt. n10) go to 17870
  n3 = 3*limasu + nsmtac
  umoutp(n3+1) = - 1.0
  umoutp(n3+2) = kswsta + n20 - 2*limasu
  umoutp(n3+3) = ntotac
  nsmtac = nsmtac + 3
  go to 17810
  !  set type-14 and 18 for electromech. exciter torque from tacs:
17870 n4 = 80 + n17
  if (n20 .ne. n4) go to 17880
  write (lunit6,17872) numum
17872 format(/,45h note : this tacs interface request regarding,33h the exciter torque for um number,i4, &
           43h,  is no longer in effect. the influence of, /, 41h the exciter torque will be automatically, &
           41h included in exactly the same way as with, 21h the sm type-59 code.,/)
  go to 17810
  ! request for tacs control of total applied torque :
17880 if (jtmtac(n1) .eq. -999999) go to 17810
  n4 = 72 + n17
  if (n20 .ne. n4) go to 17810
  n19 = 1
  do n10 = 1, ktab
     ndx1 = ilntab( klntab + n10 )
     if (bus1 .eq. texvec(ndx1)) go to 17890
17882 end do
  write (lunit6,17826) bus1
  go to 9600
  !  set type-14 sources for applied torques :
17890 if (jtmtac(n1) .eq. -999999) go to 17899
  do n5 = 1, ntorq
     if (n19 .ne. 1) go to 17894
     kconst = kconst + 1
     sfreq(kconst) = n10
     iform(kconst) = 17
     node(kconst) = - 1
     tstart(kconst) = - 1.0
     tstop(kconst) = fltinf
     if (iprsup .ge. 1) write(lunit6,17892) node(kconst),kconst,sfreq(kconst)
17892 format(10h *********,37h type-17 source for next tacs control,7x,i4,44x,i4,e14.5)
17894 kconst = kconst + 1
     if (n5 .eq. 1) nodmum(n1) = kconst
     iform(kconst) = 14
     n6 = kumout(n5)
     if (n6 .eq. nmgen) n6 = ntotst + 2*numasu
     node(kconst) = - n6
     kode(n6) = 0
     n4 = nsmtpr + n5
     crest(kconst) = umoutp(n4)
     umoutp(n4) = 0.0
     tstart(kconst) = - 7777.0
     tstop(kconst) = fltinf
     sfreq(kconst) = 0.00001
     if (iprsup .ge. 1) write(lunit6,17896) node(kconst),kconst,sfreq(kconst)
17896 format(10h *********,22h applied torque source,22x,i4,44x,i4,e14.5)
17898 end do
17899 if (n19 .eq. 0) go to 17900
  go to 17810
  ! ********************* finish statements of sm type-59 data
17900 ncltot = ncltot + 7
  if (ncltot .le. nclfix) go to 17905
  write(lunit6,17904) ncltot
17904 format( /,  29h overflow of u.m. coil table,,10h ncltot = , i5, 9h increase,29h nclfix on um dimension card.)
  go to 9600
17905 do n5 = 1, numasu
     kumout(n5) = 0
     jumout(n5) = 0
  end do
  mjm = ncltot - 6
  do nk = mjm, ncltot
     jcltac(nk) = 0
  end do
  if (iprsup .ge. 1) write(lunit6,17930) ntotst,numasu,nrsyn,nmgen,nmexc
17930 format(/,10h *********,38h created nodes for mechanical system :, 41h first node nr = ntotst+1, last node nr =, &
       28h ntotst + 2*numasu + nrsyn .,/, 30x,6hntotst,2x,6hnumasu,2x,6h nrsyn,2x,6h nmgen,2x, 6h nmexc, &
       /,32x,i4,4x,i4,4x,i4,4x,i4,4x,i4)
  if (iprsup .ge. 1) write(lunit6,17932) nexc,nexcsw
17932 format(/,10h *********,34h created nodes for field circuit :,8h nexc = ,i4,10x, 9hnexcsw = ,i4)
  !  shifting umoutp entries with 3*numasu to behind nsmtpr :
  n6 = nsmtac - nsmtpr
  n7 = nsmtpr + n6
  do n5 = 1, n6
     n3 = nsmtpr + n5
     n4 = n3 + 3*numasu
     umoutp(n3) = umoutp(n4)
     if (n4 .gt. n7) umoutp(n4) = 0.0
17950 end do
  if (nparum .ne. 0) go to 17951
  if (iprsup .lt. 3) go to 17958
17951 write (lunit6,17952) n1,reamdu(n1),reamds(n1),reamqu(n1),reamqs(n1),flxds(n1),flxqs(n1)
17952 format(/, 21h parameters of um nr.,i4,1h:,10x,4hlmud,x,4hlmsd,10x,4hlmuq,10x,4hlmsq,9x,5hflxsd,9x,5hflxsq,/, 26x,6e14.5)
  write (lunit6,17953)
17953 format(/, 42h coil     resistance    leakage inductance)
  n6 = ncltot - 6
  do n5 = n6, ncltot
     if (gpar(n5) .eq. 0.0) write (lunit6,17954) n5
17954 format(1x,i4,10x, 11h dummy coil)
     if (gpar(n5) .eq. 0.0) go to 17957
     write (lunit6,17955) n5,gpar(n5),reacl(n5)
17955 format(1x,i4,2x,2e14.5)
     if (n5 .eq. ncltot) write(lunit6,17956)
17956 format(/, 1h )
17957 end do
  !  set 0.5*rf for internal field resistance :
17958 gpar(ncltot-3) = 0.5 * gpar(ncltot-3)
  n5 = nsmtac + 3*numasu
  if (n5 .le. iotfix) go to 17970
  write (lunit6, 17960)
17960 format( /, 38h overflow of um output table, increase,35h in card for absolute um dimensions,/, 17h value of iotfix.,/, &
       37h remark : if sm type-59 data input is, 30h included, then iotfix is also,/, 36h related to outputs of mech. systems, &
       33h in all sm type-59 data machines.)
  go to 9600
17970 jf = 1
  return
  !  end reading sm type-59 data input $$$$$$$$$$$$$$$$$$$$$$$$$$
9600 call stoptp
  return
end subroutine umdatb
!
!     smdat.
!
subroutine smdat (mtype)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     this module is used only by type-59 brandwajn  s.m.  model
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'tacsar.ftn'
  include 'syncom.ftn'
  include 'synmac.ftn'
  !     the following array is sized 3 (nn4-1) * n50 ( n50 = 50  at pre-
  !     sent ) + nn10 ( no. of class 1 requests <=15 at present> ) ******
  dimension  npbuf( 165 )
  equivalence (ismdat(22),ipout), (ismdat(23),n56), ( ismdat(24), ismold), (ismdat(25),nn10), (ismdat(26),nn4), (ismdat(27),nn14)
  dimension massex(1)
  equivalence (histq(1),massex(1))
  character*8 text1, text2, text3
  character*8 text7, text8,        text10, text11, text12
  character*8 text16, text17
  character*8 text18, text19, text20, text21
  data  text1   / 6hfinish  /
  data  text2  / 6htolera /
  data  text3  / 6hnces   /
  data  text7  / 6hparame /
  data  text8  / 6hter    /
  data  text10  / 6hdelta  /
  data  text11  / 6hconnec /
  data  text12  / 6htion   /
  data  text17  / 6hfittin /
  data  text18  / 6hg      /
  data  text19  / 6ht      /
  data  text20  / 6hpf     /
  data  text21  / 6hd!     /
  data  text16  / 6h part  /
  if ( iprsup  .ge.  1 ) write ( lunit6, 4567 )
4567 format ( 23h  "begin module smdat." )
  !     define no. of outputs in class 1 (nn10) and no. of classes (nn4)**
  nn10 = 15
  nn4 = 4
  nn14 = 14
  n50 = 50
  nright = 0
  nfrfld = 1
  d10  =  1.0  / fltinf
  if ( numsm .gt. 1 ) go to 123
  call rinfin
  n19 = locf( flstat(1) ) - locf(  voltbc(1) )
  if(  n19  .ge. 41 )     go  to  8258
  kill = 180
  lstat( 18 ) = nchain
  lstat( 19 ) = 8258
  lstat( 17 ) = 41
  lstat( 16 ) = n19
  go to 9999
8258 d8  =  3.0 / 2.0
  thtw  =  sqrtz( d8 )
  athtw = 1.0 / thtw
  d8 = 3.0
  sqrt3 = sqrtz( d8 )
  asqrt3 = 1.0 / sqrt3
  sqrt32 = sqrt3 * onehaf
  omdt = omega * deltat
  radeg = 360.0 / twopi
  bdam = 1.356306d0 * tenm6
  bin = .0421409745d0
  nst = 0
  itold = it
  ibrold = ibr
  ismold = 0
  ipout = 0
  n56 = ( nn4-1 ) * n50 + nn10
  mfirst = 6 * limass
  d8 = nbyte(3)
  d8 = d8 / nbyte(4)
  om2 = d8
  d8 = mfirst * d8 - 1.0
  mfirst = d8
123 jk = numsm
  ism = 0
  fm = -2.0
  k = nst + 1
  if( k .le. lsyn )  go  to  8818
  kill = 1
  lstat( 19 ) = 123
  lstat( 16 ) = 17
  go to 9999
8818 j30 = 30 * jk - 29
  ismdat( j30 ) = 0
  statfr = sfreq( kconst-1 )
  idelta = 0
  if ( iprsup  .ge.  1 ) write (lunit6, 3604)  k,  nst, mtype, kconst, numsm
3604 format(/, 26h at start of  'smdat'  ..., 40h       k     nst   mtype  kconst   numsm  ,/,  24x, 10i8 )
  !     read input card using cimage
1 call cimage
  if ( kill  .gt.  0 )   go to 9999
  nright = -2
  n11 = kolbeg
  kolbeg = 1
  call freone ( d1 )
  nright = 0
  !     check for key word   'tolerances'  (or  't' ).
  if ( texta6(1)  .eq.  text19 )   go to 7103
  if ( texta6(1)  .ne.  text2 )   go to  7106
  if ( texta6(2)  .ne.  text3 )   go to  7106
7103 if ( n11  .gt.  0 )   go to 7104
  read (unit = abuff(1), fmt = 3609) d1, d2, d3, n9
3609 format ( 10x, 3e10.0, 10x, i10 )
  go to 7105
7104 nfrfld = 1
  call freone ( d1 )
  call freone ( d2 )
  call freone ( d3 )
  call frefld ( voltbc(1) )
  n9 = voltbc(1)
7105 if ( d1  .gt.  0.0 )   epsuba = d1
  if ( d2  .gt.  0.0 )   epomeg = d2
  if ( d3  .gt.  0.0 )   epdgel = d3
  if ( n9  .gt.  0 )   iprsov(37) = n9
  if ( noutpr  .eq.  0 ) write (kunit6, 3610)  epsuba, epomeg, epdgel, iprsov(37)
3610 format ( 11h+  epsilon.,  3e11.2,  i5  )
  go to 1
  !     check for key word   'parameter fitting'  (or  'pf' ).
7106 if ( texta6(1)  .eq.  text20 )   go to 2115
  if ( texta6(1)  .ne.  text7  )   go to 8641
  if ( texta6(2)  .ne.  text8  )   go to 8641
  if ( texta6(3)  .ne.  text17 )   go to 8641
  if ( texta6(4)  .ne.  text18 )   go to 8641
2115 if ( n11  .gt.  0 )   go to 7109
  read (unit = abuff(1), fmt = 6011) fm
6011 format ( 24x, e8.0 )
  go to 7110
7109 nfrfld = 1
  call freone ( fm )
7110 if( fm  .gt.  1.0  )  go  to  152
  ism  =  1
  go  to  153
152 ism  =  0
  fm  =  1.0
153 if ( noutpr  .eq.  0 ) write( kunit6, 3617 )  fm
3617 format ("+  optimize Park's data.", 3x, e12.3)
  go to 1
  !     check for key word   'delta connection'  (or  'dc' ).
8641 if ( texta6(1)  .eq.  text21 )   go to 2125
  if ( texta6(1)  .ne.  text10 )   go to 8659
  if ( texta6(2)  .ne.  text11 )   go to 8659
  if ( texta6(3)  .ne.  text12 )   go to 8659
2125 if ( noutpr  .eq.  0 ) write (kunit6, 8646)
8646 format (  44h+  notification of delta-connected armature.      )
  idelta = 1
  go to 1
8659 kolbeg = n11
  ismdat( j30 ) = ismdat( j30 ) + 1
  nst = nst + 1
  if( nst .le. lsyn )  go  to  8819
  kill = 1
  lstat( 19 ) = 8659
  lstat( 16 ) = 17
  go to 9999
8819 k = nst
  i101 = 101 * k - 100
  i26  = i101
  i30 = 30 * k - 29
  ismdat( i30+1 ) = idelta
  if ( kolbeg  .gt.  0 )   go to 7113
  read (unit = abuff(1), fmt = 5) ismdat(i30 + 11 ), ismdat(i30 + 12), ismdat(i30 + 13), np, sm3, sm4, rmva, rkv, aglin, dab, dac
5 format( 3i2, i4, 7e10.6 )
  go to 7114
7113 nfrfld = 4
  call frefld ( voltbc(1) )
  ismdat( i30+11 ) = voltbc(1)
  ismdat( i30+12 ) = voltbc(2)
  ismdat( i30+13 ) = voltbc(3)
  np = voltbc( 4 )
  nfrfld = 1
  call freone ( sm3 )
  call freone ( sm4 )
  call freone ( rmva )
  call freone ( rkv )
  call freone ( aglin )
  call freone( dab )
  call freone( dac )
7114 if ( noutpr  .eq.  0 ) write (kunit6,3619) ismdat( i30+11 ), ismdat( i30+12 ), ismdat( i30+13 ), np, sm3, sm4
3619 format ( 17h+  4th s.m. card.,  4i4,  2f7.3  )
  elp( i26+25 ) = np / 2
  if( sm3  .eq.  0.0  )    sm3 = d10
  if( sm4  .eq.  0.0  )    sm4 = d10
  elp( i26+28 ) = sm3
  elp( i26+29 ) = sm4
  numask = ismdat( i30+11 )
300 if( numask .lt. 0  )  go  to  306
  iln = ismold * 12
  ismold = ismold + numask
  if( ismold .le. limass )  go to 8
  kill = 1
  lstat( 19 ) = 300
  lstat( 16 ) = 16
  go to 9999
306 kill = 105
  lstat( 19 ) = 306
  lstat( 14 ) = numask
  lstat( 18 ) = nchain
  go to 9999
8 ismdat( i30+8 ) = 0
  if( aglin .gt. 0.0 )   go  to  150
  ismdat( i30+8 ) = 2
150 aglin = absz( aglin )
  !     read  input  cards  using  cimage
  call  cimage
  read (unit = abuff(1), fmt = 163) ad1, ad2, aq1, aq2, qaa, qab, qac
163 format( 10x, 7e10.6 )
  if( noutpr  .eq.   0 ) write( kunit6, 154 )  aq1, qaa, qab, qac
154 format( 9h+  q-axis,  f7.3, 3f10.4  )
  !     read input card using cimage.
  call cimage
5564 zb = ( rkv**2 ) / rmva
  !     if( idelta  .eq.  1  )  zb = 3.d0 * zb
  emf = rkv / ( tenm3*aglin )
  if( fm.le.-1.0) go to 100
  if ( kolbeg  .gt.  0 )   go to 7124
  read (unit = abuff(1), fmt = 6) ra, xl, xd, xq, xdp, xqp, xdpp, xqpp
6 format(8e10.6)
  go to 7125
7124 call freone ( ra )
  call freone ( xl )
  call freone ( xd )
  call freone ( xq )
  call freone ( xdp )
  call freone ( xqp )
  call freone ( xdpp )
  call freone ( xqpp )
7125 if ( noutpr  .eq.  0 ) write (kunit6, 3628)  ra, xl, xd
3628 format ( 17h+  5th s.m. card.,  3f10.4  )
  !     check for an error in reactance values ***************************
  if ( ra  .lt.  0.0 )   go to 19
  if ( xl  .lt.  0.0 )   go to 19
  if ( xd  .lt.  0.0 )   go to 19
  if ( xq  .lt.  0.0 )   go to 19
  if ( xdp  .lt.  0.0 )   go to 19
  if ( xqp  .lt.  0.0 )   go to 19
  if ( xdpp  .lt.  0.0 )   go to 19
  if ( xqpp  .lt.  0.0 )   go to 19
  if((xd-xdp).le.flzero.and.fm.eq.0.0) go to 19
  if((xdp-xdpp).le.flzero) go to 19
  if((xdpp-xl).le.flzero)  go to 19
  ndwqa = 0
  if( xq .eq. xqp .and.  xqp .eq. xqpp ) ndwqa = -2
  if( ndwqa .lt. 0 )   go to 25
  if((xq-xqp).le.flzero.and.fm.eq.0.0) go to 19
  if((xqp-xqpp).le.flzero) go to 19
  if((xqpp-xl).le.flzero)  go to 19
  go to 25
19 kill = 176
  go to 9999
25 continue
  !     convert machine parameters to physical values ,this will  result
  !     in all data being referred to the rotor side ********************
  xq = xq * zb
  xd = xd * zb
  xl = xl * zb
  ra = ra * zb
  if ( iprsup  .gt.  0  ) write (kunit6, 3628)  ra, xl, xd
  !     read input card using cimage
  call cimage
  if ( kolbeg  .gt.  0 )   go to 7136
  read (unit = abuff(1), fmt = 30) tdop, tqop, tdopp, tqopp, el2, r1, el1
30 format (7e10.6,9x,i1)
  go to 7137
7136 call freone ( tdop )
  call freone ( tqop )
  call freone ( tdopp )
  call freone ( tqopp )
  call freone ( el2 )
  call freone ( r1 )
  call freone ( el1 )
7137 if ( noutpr  .eq.  0 ) write (kunit6, 3637)  tdop, tqop, tdopp
3637 format ( 17h+  6th s.m. card.,  3f10.4  )
  !     process manufacturer's data ,start with the d-axis **************
  x1(1) = xd
  x1(2) = xdp * zb
  x1(3) = xdpp * zb
  x1(4) = tdopp * omega
  x1(5) = tdop * omega
  x1(6) = xl
  call smpfit( x1(1), ism, fm, lunit6, noutpr )
  !     calculate transformer ratio for scaling of rotor circuits ********
  a = x1(2) / thtw
  rat = emf / a
  rat2 = 2.0 * rat**2 / 3.0
  a=emf*rat/thtw
  !     load processed data into permanent storage **********************
  ib2 = 20
  call mover0( elp( i26 ), ib2 )
  elp( i26 ) = xd
  elp( i26+1 ) = emf
  if( x1(5) .eq. 0 ) go to 6014
  elp( i26+2 ) = x1(3) * rat2
  elp( i26+3 ) = emf
  elp( i26+4 ) = a
  elp( i26+5 ) = x1(4) * rat2
  elp( i26+6 ) = x1(5) * rat2
  elp( i26+7 ) = x1(6) * rat2
  go to 6015
6014 elp( i26+2 ) = x1(4) * rat2
  elp( i26+5 ) = rat2
  elp( i26+6 ) = x1(6) * rat2
6015 rat1 = emf / x1( 2 )
  !     quadrature axis parameters ***************************************
  elp( i26+8 ) = xq
  if( ndwqa .lt. 0 )  go to 6013
  x1(1) = xq
  x1(2) = xqp * zb
  x1(3) = xqpp * zb
  x1(4) = tqopp * omega
  x1(5) = tqop * omega
  x1(6) = xl
  call smpfit( x1(1), ism, fm, lunit6, noutpr )
  a = x1(2) * rat / thtw
  elp( i26+9 ) = a
  if( x1(5) .eq. 0. )  go to 71
  elp( i26+10 ) = x1(3) * rat2
  elp( i26+11 ) = a
  elp( i26+12 ) = x1(2) * rat2
  elp( i26+13 ) = x1(4) * rat2
  elp( i26+14 ) = x1(5) * rat2
  elp( i26+15 ) = x1(6) * rat2
  go to 70
71 elp( i26+10 ) = x1(4) * rat2
  elp( i26+13 ) = rat2
  elp( i26+14 ) = x1(6) * rat2
  go to 70
6013 elp( i26+10 ) = rat2
  elp( i26+13 ) = rat2
  !     store remaining variables ***************************************
70 elp( i26+16 ) = el2 * zb  + 3.0 * el1
  elp( i26+17 ) = ra  +  3.0 * r1
  elp( i26+18 ) = xl
  elp( i26+19 ) = ra
  elp( i26+20 ) = rat1
  !     calculate and store saturation constants ************************
  if( ismdat( i30+8 ) .eq. 0 )   go  to 158
  !     start   with   the  d-axis   characteristic***********************
  !     check for inconsistent saturation data ***************************
  if( dab .ge. aglin .and. dac .gt. dab )  go  to  167
  kill = 211
  lstat( 14 ) = k
  lstat( 19 ) = 167
  flstat( 13 ) = aglin
  flstat( 14 ) = dab
  flstat( 15 ) = dac
  go to 9999
167 if( ad1 .le. 0.0 )    ad1 = 1.0
  if( ad2 .le. 0.0 )    ad2 = 1.2
  sf5 = dab / ( aglin * ad1 )
  sf2 = dac / ( aglin * ad2 )
  sf3 = ( sf2 - sf5 ) / ( dac - dab )
  sf6 = dac + ( 1.0 - sf2 ) / sf3
  if( sf6 .gt. (aglin * onehaf ) )  go  to  437
  !     reset saturation parameters *************************************
  dsm = rkv / aglin
  hsp = rkv * ( ad2 - ad1 ) / ( dac - dab )
  dsd = rkv * ad1 - hsp * dab
  dsr = dsd / ( dsm - hsp )
  ilv = 167
  write( lunit6, 6110 )  ilv, sf6, dsr
6110 format( 32h  ****** warning, warning ******,/, 43h  subroutine smdat, nearby statement number, i8, &
          /, 32h  saturation treshold reset from, 2x, e15.8, 3x, 2hto, 2x, e15.8 )
  sf6 = dsr
  sf3 = ( 1.0 - sf2 ) / ( sf6 - dac )
437 elp( i26+21 ) = sf6
  elp( i26+22 ) = sf3
  if( iprsup .ge. 1 ) write(lunit6,155) k,sf3,sf6,sf5,sf2
155 format(1x,22h saturation constants.,7x,1hk,11x,3hsf3,11x,3hsf6,11x,3hsf5,11x,3hsf2,/,22x,i8,4e14.5,/,1x)
  !     calculate  parameters  for  the  q-axis **************************
  if( qaa .gt.  0.0  )   go  to 157
  !     unknown   parameters-calculate an  approximate characteristic ****
  b6 = ( elp( i26+8 ) - xl )  / ( elp( i26 ) - xl )
  sf6 = sf6 * b6
  elp( i26+23 ) = sf6
  elp( i26+24 ) = sf3
  if( iprsup  .ge.  1 ) write (lunit6, 156 )  k, sf3, sf6
156 format(10x,54happroximate characteristic for the q-axis, machine no.,i5,/,14x,6hqsat12,14x,6hqsat10,/,2(6x,e14.5) )
  go  to 158
  !     known  parameters , calculate constants **************************
  !     check for missing  saturation data for the q-axis ****************
157 if( qab .ge. qaa  .and.  qac .gt. qab )  go  to  166
  kill = 211
  lstat( 19 ) = 157
  lstat( 14 ) = k
  flstat( 13 ) = qaa
  flstat( 14 ) = qab
  flstat( 15 ) = qac
  go to 9999
166 if( aq1 .le. 0.0 )   aq1 = 1.0
  if( aq2 .le. 0.0 )   aq2 = 1.2
  sf5 = qab / ( qaa * aq1 )
  sf2 = qac / ( qaa * aq2 )
  sf3 = ( sf2 - sf5 ) / ( qac - qab )
  sf6 = qac + ( 1.0 - sf2 ) / sf3
  if( sf6 .gt. ( qaa * onehaf ) )  go  to  439
  dsm = rkv / qaa
  hsp = rkv * ( aq2 - aq1 ) / ( qac - qab )
  dsd = rkv * aq1 - hsp * qab
  dsr = dsd / ( dsm - hsp )
  ilv = 166
  write( lunit6, 6110 )   ilv, sf6, dsr
  sf6 = dsr
  sf3 = ( 1.0 - sf2 ) / ( sf6 - dac )
439 elp( i26+23 ) = sf6
  elp( i26+24 ) = sf3
  if( iprsup .ge. 1 ) write(lunit6,156) k,sf3,sf6,sf5,sf2
  !     process mechanical data of the generator  ***********************
  !     scale the mechanical data to mks units   *************************
  !     this scaling of data corresponds to torque given in mva  *********
158 smext = 0.
  num2 = numask + numask
  num3 = num2 + numask
  num4 = num2 + num2
  num5 = num3 + num2
  ib2 = iln + num2
  do i = 1, numask
     !     read input card using cimage
     call cimage
     if ( kolbeg  .gt.  0 )   go to 7144
     read (unit = abuff(1), fmt = 36) mloc, extrs, hico, dsr, dsm, hsp, dsd
36   format( i2, 8x, 6e10.6 )
     go to 7145
7144 call frefld ( voltbc(1) )
     if ( kill  .gt.  0 )   go to 9999
     mloc = voltbc(1)
     nfrfld = 1
     call freone( extrs )
     call freone( hico )
     call freone( dsr )
     call freone( dsm )
     call freone( hsp )
     call freone( dsd )
7145 if( mloc .gt. 0   .and.   mloc .le. numask )  go  to  7146
     n167 = 7145
7147 kill = 181
     lstat( 19 ) = n167
     lstat( 18 ) = nchain
     lstat( 15 ) = mloc
     lstat( 16 ) = numask
     go  to  9999
7146 if ( noutpr  .eq.  0 ) write( kunit6, 3653 ) i, mloc, extrs, hico, dsd
3653 format( 12h+  mass card,   i2,   i2,   3e11.3)
     il1 = ib2 + mloc
     shp(  il1  ) = extrs
     shp( il1+numask ) = hico * bin
     shp( il1+num2 ) = dsm * bdam
     shp( il1+num3 ) = hsp * bdam / tenm6
     shp( il1+num4 ) = dsd * bdam
     shp( il1+num5 ) = dsr * bdam
     smext = smext + extrs
  end do
  if(  numask  .eq.  1  )    smext = 1.0
  if( absz(smext) .gt. flzero )   go to 410
420 kill = 107
  lstat( 19 ) = 420
  go to 9999
410 il2 = ib2 + numask
  il1 = ib2 + 1
  do il = il1, il2
     shp( il ) = shp( il ) / smext
  end do
  !     check for blank card terminating mass cards  *********************
  !     read input card using cimage   ***********************************
  call cimage
  read (unit = abuff(1), fmt = 1122) (voltbc(i), i = 1, 41)
1122 format ( 41f1.0 )
  n167 = 3654
  if( kill .gt. 0 )  go  to  9999
  !     set-up dummy branches to reserve space for future use  ***********
  if ( it+6  .le.  ldata )   go to 8917
  kill = 1
  lstat(19) = 8917
  lstat(16) = 3
  go to 9999
8917 if ( ibr+3  .le.  lbrnch )   go to 8921
  kill = 1
  lstat(19) = 8921
  lstat(16) = 2
  go to 9999
8921 do il=1, 3
     it = it + 1
     ibr = ibr + 1
     length( ibr ) = 1
     kbus( ibr ) = 1
     mbus( ibr ) = 1
     nr( ibr ) = -it
     tr( it ) = 1.0
     tx( it ) = 0.0
     c( it ) = 0.0
  end do
  it = it + 3
  !     read output specification card. ismout is array of output flags.
  !     zero-out output request arrays. preset flags for parameter output
  !     requests     *****************************************************
  iexc = 0
  if( ismdat( i30+13 ) .eq. 0 )    iexc = 1
  n14 = i30 + 20
  n8 = n14 - 3
  do il = n8, n14
     ismdat( il ) = 0
  end do
  elp( i26+26 ) = 0.0
  elp( i26+27 ) = 0.0
  ism = 0
  n8 = n8 - 1
  do ij = 1, 41
     if ( voltbc( ij )  .ne.  0.0 )  go to 1983
1500 end do
  if ( noutpr .eq. 0 )  write (kunit6, 1700)
1700 format( 36h+ blank card terminating mass cards.)
  !     read input card using cimage
7219 call cimage
  read (unit = abuff(1), fmt = 3654) n7
3654 format ( i8 )
  if( n7 .ne. 0 )   go  to  8235
  if( noutpr .eq. 0 ) write (kunit6, 3656)
3656 format( 41h+ blank card terminating output requests.)
  go to 7251
  !     check for overflow of npbuf() ***********************************
8235 n55 = ( nn4-1 ) * numask + nn10
  if( n55 .lt. n56 )  go to  7220
  kill = 111
  lstat( 13 ) = n56
  lstat( 15 ) = k
  lstat( 16 ) = n55
  lstat( 17 ) = 0
  lstat( 19 ) = 8235
  go to 9999
7220 continue
  read (unit = abuff(1), fmt = 3657) (voltbc(i), i = 1, nn14)
3657 format(2x, 2f1.0, 4x, 12f6.0)
  ioutr = voltbc( 1 )
  iall = voltbc( 2 )
  if( ioutr .gt. 0  .and.  ioutr .le. (nn4+1) )  go  to  7221
  write(lunit6,3658) ioutr
3658 format( 1x, 10(1h*),  11h  warning  , 10( 1h* ), 3x,38h request for nonexistent output class ,i4, 12h  neglected. )
  go to 7219
7221 if( noutpr .eq. 0 ) write (kunit6, 3659)  ioutr
3659 format( 1h+, 2x, 30h output request card for class , i4 )
7222 go to ( 7230, 7240, 7240, 7250, 7210 ), ioutr
  !     process flags for outputs after the s.s. solution  ***************
7210 n16 = 2
  ivar = 0
  do ipl = 3, nn14
     n6 = voltbc( ipl )
     if( iall .eq. 0 )  go to 7213
     n6 = ipl - 2
     go to 7212
7213 if( n6 .gt. -1  .and.  n6 .le. n16 )  go  to  7212
     write(lunit6,3662) n6, ioutr, k, n16
     go to 7211
7212 if( n6 .eq. 0 )   go   to  7211
     ivar = ivar + 1
     if( ivar .gt. n16 )  go  to  7219
     kflag = i26 + n6
     elp( kflag+25 ) = n6
7211 end do
  go  to   7219
7230 n16 = nn10 - iexc
  n21 = 0
7231 n17 = n8 + ioutr
  n21 = ismdat( n17 ) + n21
  if(  iall .gt. 0 )  go  to  7238
  do il = 3, nn14
     n6 = voltbc( il )
     if( n6 .le. n16  .and.  n6 .gt. -1 )  go to 7233
     write(lunit6,3662) n6,ioutr,k,n16
3662 format(1x, 10(1h*),11h  warning  , 10(1h*), 3x,33h request for nonexistent variable, i4, 9h in class, i3, &
          13h of s.m. no. , i3, 12h discarded. , /, 14h in this class,51h the above s.m. can have numbers between 1(one) and, i4 )
     go to 7232
7233 if( n6 .eq. 0 )  go to 7232
     ism = ism + 1
     n21 = n21 + 1
     ismdat( n17 ) = ismdat( n17 ) + 1
     npbuf( n21 ) = n6
     !     remove duplicate requsts. put all request in an ascending order
     ipl = ismdat( n17 )  -  1
     if( ipl .lt. 1 )  go to 7232
     ijn = n21
     do ijk = 1, ipl
        ijl = ijn - 1
        if( npbuf(ijl) .lt. npbuf(ijn) ) go to 7232
        if( npbuf(ijl) .eq. npbuf(ijn) ) go to 7245
        n30 = npbuf( ijl )
        npbuf( ijl ) = npbuf( ijn )
        npbuf( ijn ) = n30
7242    ijn = ijn - 1
     end do
     go to 7232
     !     discard duplicate request ****************************************
7245 write(lunit6, 7223 ) npbuf(ijn), ioutr
7223 format( 1x,10(1h*), 11h  warning  , 10(1h*), 3x,18h duplicate request, i5, 10h  in class, i5, 12h  discarded.)
     ism = ism - 1
     ismdat( n17 ) = ismdat( n17 ) - 1
     do ijk = ijn, ipl
7246    npbuf( ijk ) = npbuf( ijk+1 )
     end do
7232 end do
7234 if( (ipout+ism) .le.  lsmout ) go to 7219
  kill = 111
  lstat( 19 ) = 7234
  lstat( 13 ) =  lsmout
  lstat( 15 ) =  k
  lstat( 16 ) =  ipout + ism
  lstat( 17 ) =  0
  go to 9999
7238 if( n16 .gt. 0 )  go  to  7239
  write(lunit6,3662) n16,ioutr,k,n16
  go to 7234
7239 n21 = n21 - ismdat( n17 )
  ismdat( n17 ) = 0
  do il = 1, n16
     ismdat( n17 ) = ismdat( n17 ) + 1
     ism = ism + 1
     n21 = n21 + 1
     npbuf( n21 ) = il
7237 end do
  go to 7234
7240 n16 = numask
  n21 = nn10 + (ioutr-2) * numask
  go to 7231
7250 n16 = numask - 1
  n21 = nn10 + (ioutr-2) * numask
  go to 7231
  !     processing old output cards
1983 n21 = 0
  n17 = n8 + 1
  jj3 = 31 + numask
  jkm = -1
  ij = 1
1125 if (ij .eq. 13 .or. ij .eq. 23 .or. ij .eq. 33) go to 1999
1130 n6 = voltbc(ij)
  if ( n6 .gt. -1  .and.  n6 .le. 2 )  go to 1155
  write ( lunit6, 1133 )  ij, n6
1133 format ( 1x, 10(1h*), 11h  warning  , 10(1h*)  ,/,30h  the output request in column, i4,  11h  discarded, &
          29h  because of the wrong number, i4  ,/, 40h  the correct number should be 1 or 2 .  )
  go to 1899
1155 if ( n6   .eq.  0 )  go to 1899
  if ( ij   .ge.  3 )  go to 1255
  kflag = i26 + ij
  elp( kflag+25 ) = ij
  go to 1950
1255 if ( ij .ne. 3  .and.  ij .ne. 11 ) go to 1355
  jkn = 0
  if ( ij  .eq.  11 )  jkn = 7
  do jk = 1, 3
     n21 = n21 + 1
     ismdat( n17 ) = ismdat( n17 ) + 1
     npbuf(n21) = jk + jkn
1277 end do
  if ( ij  .eq.  3 )   go to 1950
1299 ij = ij - 3
  go to 1130
1355 n21 = n21 + 1
  ismdat( n17 ) = ismdat( n17 ) + 1
  if ( ij  .ge. 13 )  go to 1990
  npbuf(n21) = ij
  if ( ij  .eq.  8 )  npbuf(n21) = npbuf(n21) + 3
  if (ij .eq. 9  .or.  ij .eq. 10) npbuf(n21) = npbuf(n21) + 5
1899 if ( ij .eq. 11 .or.  ij .eq. 12)  go to 1299
     if ( ij  .eq.  10 )  ij = ij + 2
     if ( ij .eq. 7  .or.  ij .eq. 8 )  ij = ij + 3
1950 ij = ij + 1
     if ( ij  .gt.  jj3 )  go to 7251
     go to 1125
1990 npbuf(n21) = ij - 12 - jkm * 10
     go to 1950
1999 n17 = n17 + 1
     jkm = jkm + 1
     n21 = nn10 + jkm * numask
     go to 1130
     !     load output requests into permanent storage **********************
7251 ids = ipout + 1
     n21 = 0
     ip1 = 3 * ipout - 2
     do il = 1, 4
        n17 = n8 + il
        n6 = ismdat( n17 )
        if( n6 .eq. 0 )  go to 7252
        do i = 1, n6
           ipout = ipout + 1
           n21 = n21  + 1
           ip1 = ip1 + 3
7253       ismout( ip1 ) = npbuf( n21 )
        end do
7252    n21 = (il-1) * numask + nn10
     end do
     if( ids .gt. ipout ) ismdat( n8+1 ) = -10
     n2 = ntotac
     i72 = 0
     niunrs = iuty( kiuty+1 )
     n18 = 3 * numask - 1
     n19 = 17
     do i = 1, 9999
        !     read auxiliary input to tacs card
        !     read input card using cimage
        call cimage
        if ( kolbeg  .gt.  0 )   go to 2183
        read (unit = abuff(1), fmt = 3669) n3, bus6, bus5, iv
3669    format ( i2,  2a6, i3  )
        go to 2185
2183    nfrfld = 1
        if ( kill  .gt.  0 )   go to 9999
        call freone ( d11 )
        n3 = d11
        nfrfld = -2
        call freone ( d1 )
        bus6 = texta6(1)
        bus5 = texta6(2)
        nfrfld = 1
        call freone( d11 )
        iv = d11
        nfrfld = 0
2185    if ( bus6  .ne.  text1 )   go to 3674
        n2 = ntotac - n2
        if ( noutpr  .ne.  0 )   go to 7841
        if ( bus3  .ne.  text16 ) write (kunit6, 3671)  n2
3671    format ( 9h+  end of,  i3, 27h  tacs interface variables.   )
        if ( bus5  .eq.  text16 ) write (kunit6, 6743)  n2
6743    format ( 9h+  end of,  i3, 37h  tacs interface variables.  parallel  )
7841    ismdat( i30+14 ) = n2
        go to 205
3674    if ( n3  .ne.  71 )   go to 5431
        if ( noutpr  .eq.  0 ) write (kunit6, 3670)  bus6
3670    format (  33h+  tacs controlled excitation.  ',  a6,  3h' .  )
        do m = 1, ktab
           ndx1 = ilntab( klntab+m )
           if ( bus6  .eq.  texvec(ndx1) )   go to 3672
3676    end do
        n3671 = 3676
        go to 5448
3672    ismdat( i30+15 ) = m
        go to 204
5431    if ( n3  .ne.  72 )   go to 5460
        if ( noutpr  .eq.  0 ) write (kunit6, 5436)   bus6
5436    format (39h+  tacs controlled mechanical torque. ', a6, 3h' .)
        n3671 = 5434
        if( i72 .gt. 0 )   go to  5434
        i72 = 1
        ismdat( i30+16 ) = mfirst
        !     check for possible overflow *************************************
        nfirst = mfirst - numask
        nsmout = 5 * ismold
        d8 = nsmout * om2 + 1.0
        nsmout = d8
        if( nfirst .ge. nsmout )    go to 5432
        kill = 1
        lstat( 19 ) = 5432
        lstat( 16 ) = 16
        go to 9999
5432    do m = 1, numask
           m1 = mfirst - m
5433       massex( m1 ) = 0
        end do
        m1 = mfirst
        mfirst = nfirst
        nfirst = m1
5434    if( iv .le. 0 .or. iv .gt. numask )  go to 5448
        m1 = nfirst - iv
        do m = 1, ktab
           ndx1 = ilntab( klntab+m )
           if ( bus6  .eq.  texvec(ndx1) )   go to 5455
5442    end do
        n3671 = 5442
5448    kill = 190
        lstat( 19 ) = n3671
        lstat( 14 ) = n3
        lstat( 15 ) = iv
        go to 9999
5455    massex( m1 ) = m
        go to 204
5460    if ( noutpr  .eq.  0 ) write (kunit6, 3675)  n3, bus6, iv
3675    format ( 32h+  auxiliary s.m. input to tacs.,  i3,2h ',  a6,  1h', i3 )
        lstat( 17 ) = 1
        lstat( 19 ) = 5461
        if( iv .gt. 0  )    go  to  5461
        go  to  202
5461    if( n3 .ne. 74  .and.  n3 .ne. 73 )  go to  202
        !     load address of bus6 into tacs array 'ud1' **********************
        ndy5 = kud1
        do ip = niunrs, niu
           ndx1 = ilntab( kaliu+ip )
           m1 = iuty( kiuty+ip )
           if( m1 .ne. 92 )   go to 5481
           if( bus6 .ne. texvec( ndx1 ) )  go to 5481
           go  to  5482
5481       ndy5 = ndy5 + 5
        end do
        lstat( 17 ) = -1
        lstat( 19 ) = 5481
        go to 202
5482    ud1( ndy5+2 ) = ntotac + 1
5462    if( n3 .eq. 74 )    go  to  5463
        lstat( 19 ) = 5462
        if( iv .gt. n19 )   go  to  202
        if( ismdat( i30+13 ) .ne. 0  )   go  to  5463
        if( iv .eq. 15 )    go  to  202
        iv = -iv
        go  to  5464
5463    lstat( 19 ) = 5463
        if( iv .gt. n18 )   go  to  202
5464    ntotac = ntotac + 1
        lstat( 19 ) = 5464
        if( ntotac  .gt.  lbstac )  go  to  202
        ismtac( ntotac ) = iv
        if ( iprsup  .ge.  2 ) write( lunit6, 3678 )    ntotac, ( ismtac( n6 ), n6 = 1, ntotac )
3678    format ( /,   9h ntotac =,  i4,49h .   (ismtac(j), j = 1, ntotac) follows.              ,/, (  12( 1x, i5 ) ) )
        go to 204
202     kill = 108
        lstat( 13 ) = iabs( iv )
        lstat( 14 ) = n3
        lstat( 15 ) = lbstac
        lstat( 16 ) = ntotac
        go to 9999
204  end do
205  if ( bus5  .ne.  text16 )   go to 99
     ism=0
     fm=-2.0
     go to 1
99   if ( iprsup  .le.  0 )   go to 4793
     write (lunit6, 4703)  numsm, kconst, rmva, rkv, aglin
4703 format ( //,  36h dump of tables at end of  'smdat' .,16h   numsm  kconst,  11x,  4hrmva,  12x,  3hrkv, &
          10x,  5haglin,  /,   36x,  2i8,  3e15.7  )
     do k = 1, nst
        k1 = 30 * k
        k2 = 101 * k
        write( lunit6, 4707 )    k, ismdat( k1-29 ), ismdat( k1-28 ), ismdat( k1-18 ), ismdat( k1-17 ), ismdat( k1-16 ), &
             elp( k2-75 ), elp( k2-100 ), elp( k2-92 ), elp( k2-98 ), elp( k2-96 ), elp( k2-97 )
4707    format ( /,  4h row,48h  imdual  iconfg   numas    kmac    kexc     cnp,12x,  3held,  12x,  3helq,  12x,  3helf, &
             10x,  5helfkd,10x, 5helakd, /, ( 1x, i3, 5i8, f8.0, 5e15.6) )
        write( lunit6, 4712 )   k, elp( k2-90 ), elp( k2-88 ), elp( k2-84 ), elp( k2-91 ), elp( k2-89 ), elp( k2-87 ), &
             elp( k2-95 ), elp( k2-99 )
4712    format ( /,  4h row,  12x,  3helg,  10x,  5helgkq,12x,  3hel0,  11x,  4helag,  10x,  5helakq,  11x,  4helkq, &
             11x,  4helkd,  11x,  4helaf ,/,  ( 1x,  i3,  8e15.6 ) )
        write(lunit6, 4717)  k, elp( k2-81 ), elp( k2-83 ), elp( k2-94 ), elp( k2-93 ), elp( k2-86 ), elp( k2-85 ), &
             elp( k2-72 ), elp( k2-71 )
4717    format ( /,  4h row, 13x, 2hra, 13x,  2hr0,  13x,  2hrf,  12x,3hrkd, 13x,  2hrg,  12x,  3hrkq,  9x,  6hsmoutp,  9x, &
             6hsmoutq  ,/,  ( 1x,  i3,  8e15.6 )  )
        n4 = k2 - 76
        write( lunit6,4738 )  k, ismdat( k1-21 ), elp(n4-3), elp(n4-1), elp(n4-2), elp(n4), elp(n4-4), elp(n4-6)
4738    format(/,4h row,11x,4hisat,10x,5hsat10,9x,6hqsat10,10x,5hsat12,9x,6hqsat12,11x,4hrat1,9x,6hagline,/,(1x,i3,10x,i2,3x, &
             6e15.6))
8259 end do
     n2 = 0
     do k = 1, nst
        num2 = ismdat( 30*k-18 ) * 2
        n3 = n2 + num2 * 4
        n2 = n2 + num2 + 1
        write( lunit6, 4723 )   ( shp( k2 ),  k2 = n2, n3 )
4723    format( /, 43h 'shfdat' storage cells follow ............, /,( 1x,  8e16.7 )  )
8260    n2 = n3 + num2 * 2
     end do
4793 return
100  continue
     !     read per unit machine data  as normally defined in transient
     !     stability programs,i.e., obtained with an assymmetric matrix
     if ( kolbeg  .gt.  0 )   go to 7313
     read (unit = abuff(1), fmt = 105) elf, elaf, elfkd, xd, elakd, elkd
     go to 7314
7313 call freone ( elf )
     call freone ( elaf )
     call freone ( elfkd )
     call freone ( xd )
     call freone ( elakd )
     call freone ( elkd )
7314 if ( noutpr  .eq.  0 ) write (kunit6, 3628)  elf, elaf, elfkd
     !     read input card using cimage.
     call cimage
     if ( kolbeg  .gt.  0 )   go to 7327
     read (unit = abuff(1), fmt = 105) elg, elag, elgkq, xq, elakq, elkq
105  format(6e10.6,19x,i1)
     go to 7328
7327 call freone ( elg )
     call freone ( elag )
     call freone ( elgkq )
     call freone ( xq )
     call freone ( elakq )
     call freone ( elkq )
7328 if ( noutpr  .eq.  0 ) write (kunit6, 3637)  elg, elag, elgkq
     !     read input card using cimage.
     call cimage
     if ( kolbeg  .gt.  0 )   go to 7235
     read (unit = abuff(1), fmt = 110) el2, ra, rf, rkd, rg, rkq, r1, el1
110  format(8e10.6)
     go to 7236
7235 call freone ( el2 )
     call freone ( ra )
     call freone ( rf )
     call freone ( rkd )
     call freone ( rg )
     call freone ( rkq )
     call freone ( r1 )
     call freone ( el1 )
7236 if ( noutpr  .eq.  0 ) write (kunit6, 3687)  el2, ra, rf
3687 format ( 17h+  7th s.m. card.,  3f10.4  )
     elp( i26 ) = xd * zb
     elp( i26+8 ) = xq * zb
     elaf = elaf * zb
     xl = ( xd - elaf ) * zb
     ra = ra * zb
     zb3 = zb
     !     calculate the transformer ratio  *********************************
     a = elaf / thtw
     rat = emf / a
     rat2 = 2.0 * rat**2 / 3.0
     rat1 = emf / elaf
     elp( i26+1 ) = emf
     zb1 = zb * rat2
     elp( i26+2 ) = elf * zb1
     elp( i26+4 ) = elfkd * zb1
     elp( i26+5 ) = elkd * zb1
     elp( i26+6 ) = rf * zb1
     elp( i26+7 ) = rkd * zb1
     elp( i26+10 ) = elg * zb1
     elp( i26+12 ) = elgkq * zb1
     elp( i26+13 ) = elkq * zb1
     elp( i26+14 ) = rg * zb1
     elp( i26+15 ) = rkq * zb1
     zb = zb * rat / thtw
     elp( i26+3 ) = elakd * zb
     elp( i26+9 ) = elag * zb
     elp( i26+11 ) = elakq * zb
     zb = zb3
     go to 70
9999 if ( iprsup  .ge.  1 )  write ( lunit6, 4568 )
4568 format ( 23h  "exit  module smdat." )
     return
   end subroutine smdat
   !
   ! subroutine smpfit.
   !
   subroutine smpfit(x, ism, fm, lunit6, noutpr )
     implicit real*8 (a-h, o-z), integer*4 (i-n)
     !     this module applies only to  s.m.  modeling (both 50, 59).
     !     this module applies only to s.m.  modelling ( both 50 and 59 )
     dimension x(6)
     !     unimproved parameters ( resistances )    *   *   *   *   *   *   *
     d1 = x(1)
     h3 = x(6)
     h2 = x(2)
     if(d1 .eq. h2) h2 = h2 * fm
     a = x( 3 ) - h3
     c = d1 - h3
     u2 = c * c
     if( d1 .eq. h2 )   go  to  5
     b = u2 / ( d1 - h2 )  - c
     f1 = b + c
     f3 = f1 / x( 5 )
     d = b * c
     a = -a / ( ( a * f1 ) / d - 1.0 )
     f2 = a + c
     f4 = ( a + d / f1 ) / x( 4 )
     if( ism  .eq.  0  )     go   to   13
     !     improve the rotor resistances    *   *   *   *   *   *   *   *   *
     u = ( x( 4 ) + x( 5 ) ) * .5d0
     d = u * u - ( x( 4 ) * x( 5 ) ) / ( 1.0 - u2 / ( f1 * f2 ) )
     if( d .lt. 0. )   go   to    12
     d = u - sqrtz( d )
     f4 = f2 / d
     f3 = f1 / ( 2.0 * u - d )
     go  to  13
12   if( noutpr .ne. 0 )   go   to   13
     write (lunit6, 26 )
26   format ( 5x, 36h note  ----  the last-read data card, 33h belongs to a dynamic synchronous, &
          34h machine, for which the parameters    ,/, 18x, 33hare to be mathematically improved, &
          30h within the module "smpfit" of, 29h overlay number 5.   the user         ,/, &
          18x, 33hrequested this procedure by means, 28h of the  "parameter fitting", &
          28h  card which accompanied the       )
     write (lunit6, 4267)
4267 format (  18x,  28hdata cards for this machine.,30h   the attempted iteration has,20h failed to converge.              )
     write (lunit6, 27)
27   format ( 18x, 31hthe emtp logic will now recover,30h and simply ignore the request,31h to improve machine parameters.      , &
          /, 18x, 31hinput data will be used without, 17h any alterations.                     )
     go  to  13
     !     parameters of the reduced order model    *   *   *   *   *   *   *
5    f1 = 1.0
     f2 = u2 / ( d1 - x( 3 ) )
     f3 = 0.
     f4 = f2 / x( 4 )
13   x( 2 ) = c
     x( 3 ) = f1
     x( 4 ) = f2
     x( 5 ) = f3
     x( 6 ) = f4
     return
   end subroutine smpfit
   !
   !     end of file: over5.for
   !
