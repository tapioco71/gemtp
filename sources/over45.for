!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over45.for
!
!
!     subroutine subr45.
!
subroutine subr45
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Code connected to  'semlyen setup'  special request card.
  !)  This overlay is used to calculate
  !)     1)  propagation step response as approximated by an analytic
  !)         (two exponebtials and a constant) function.
  !)     2)  admittance step response as approximated by an analytic
  !)         (two exponentials and a constant) function.
  !)     3)  steady state modal distributed impedance and admittance of
  !)         the line.
  !)     4)  phase to mode transformation matrices.
  !)  this routine punches the branch cards to be read in as data when
  !)  Using the 'semlyen recursive convolution' frequency dependent line.
  include 'blkcom.ftn'
  include 'deck45.ftn'
  include 'labl45.ftn'
  dimension stg(11)
  dimension lltemp(20)
  double precision zift(11)
  equivalence (zift(1), karray(1))
  equivalence (stg(1), karray(1))
  equivalence (indtv(1),npoint)
  if (lastov .eq. 1) go to 20
  nph = ktab
  ntri = nph*(nph + 1)/2
  nphsq = nph * nph
  nph2 = nph + nph
  nphpi2 = nph2 + 2
  n22 = nphsq + nphsq
  d13 = voltbc(2) / voltbc(1)
  nfr = alogz(d13) / alogz(voltbc(3)) + 1.5
  nfr1 = nfr + 1
  n8 = nchain
  if ( kburro .eq. 1)  n8 =29
  call dimens (lltemp(1),n8,trash,trash)
  do i=1, 9999, 2
     if ( lltemp(i)  .ne.  0 )   go to 5654
     lsem = lltemp(i+1) * nbyte(4)/nbyte(3)
     go to 5655
5654 end do
  call stoptp
5655 lx = nph * ( 22*nph + 3*nfr1 + 30 ) + 4*nfr1 + 3*npoint + 3
  if (nph .le. 50 .and. lx .lt. lsem) go to 10
  kill = 198
  lstat(15) = lsem
  lstat(16) = lx
  lstat(19) = 10
  lstat(18) = 45
  lastov = nchain
  nchain = 51
  go to 9999
10 lpbuf = 1
  lcq = lpbuf + 14*nph
  lz = lcq + n22
  ly = lz + n22
  lzy = ly + n22
  lzya = lzy + n22
  lzyb = lzya + n22
  lzyc = lzyb + n22
  lzyd = lzyc + n22
  lcqt = lzyd + n22
  lq = lcqt + n22
  lqi = lq + nphsq
  lg = lqi + nphsq
  lg60 = lg + nph2
  lyo = lg60 + nph2
  lxr = lyo + nph2
  lxl = lxr + ntri
  lxg = lxl + ntri
  lxc = lxg + ntri
  lum = lxc + ntri
  lsi = lum + 3*nph2
  lvresp = lsi + nfr1 + 1
  lzift = lvresp / 2 + 1
  if ( locint(stg(11)) - locint(stg(1)) .eq. locint(zift(11)) - locint(zift(1)) ) lzift = lvresp
  lym = lvresp + ( 3*nph + 3 ) * nfr1 + 2
  lfv = lym + nph2
  lhhm = lfv + npoint
  lhhn = lhhm + npoint
20 call guts45 ( stg(lpbuf), stg(lcq), stg(lz), stg(ly), stg(lzy), stg(lzya), stg(lzyb), stg(lzyc), stg(lzyd), stg(lcqt), &
       stg(lq), stg(lqi), stg(lg), stg(lg60), stg(lyo), stg(lxr), stg(lxl), stg(lxg), stg(lxc), stg(lum), stg(lsi), &
       stg(lvresp),stg(lym),stg(lfv),stg(lhhm),stg(lhhn),zift(lzift))
  if ( kill .gt. 0 )     go to 9999
  if ( ialter .ne. 3 )   go to 9999
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunit3
  go to 5655
9999 return
end subroutine subr45
!
!     subroutine guts45.
!
subroutine guts45(pbuf, cq, z, y, zy, zya, zyb, zyc, zyd, cqt, q, qi, g, g60, yo, xr, xl, xg, xc, um, si, vresp, ymin, fv, hhm, hhn, zcos)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labl45.ftn'
  include 'volt45.ftn'
  dimension pbuf(1), cq(1), z(1), y(1), zy(1), zya(1), zyb(1)
  dimension zyc(1), zyd(1), cqt(1), q(1), qi(1), g(1), g60(1)
  dimension yo(1), xr(1), xl(1), xg(1), xc(1), um(1), si(1)
  dimension vresp(1), ymin(1), fv(1), hhm(1), hhn(1)
  dimension ygr(6), ww(3), mfrecv(10), b(4), e(4), itemp(12)
  dimension russ(5)
  dimension amarti(5)
  equivalence (vim(1),eps),(vim(2),eps1),(vim(3),fit2z)
  equivalence (vim(4),pivthr),(vim(5),epsrse)
  equivalence (vim(7),ft2emx),(vim(8),epseig),(vim(9),epspv2)
  equivalence (vim(10),fmed),(vim(11),epsyc),(vim(12),epsn)
  equivalence (vim(13),ffinp)
  equivalence (ipntv(1),nfit),(ipntv(2),niter1),(ipntv(3),niter)
  equivalence (ipntv(4),nitery),(ipntv(5),nieig)
  equivalence (ipntv(10),noo),(indtv(1),npoint),(indtv(2),nss)
  equivalence (indtv(3),kfit),(indtv(4),kps),(indtv(5),kyc)
  equivalence (indtv(6),idoc),(indtv(7),iotx),(indtv(8),ioss)
  equivalence (indtv(9),iofl),(indtv(10),npan)
  character*8 text1, text2, text3, text5, text6
  character*8 text10, text11, text12
  character*8 text13, text14, text15, text16, text17
  double precision den1,den2,hssr,hssi,yssr,yssi,hamp,hang,alnh,w2
  double precision dd11, zcos(1)
  data text1 / 6hbranch /
  data text2   /  6hnew rh  /
  data text3   /  6ho       /
  data text5   /  6hold da  /
  data text6   /  6hta      /
  data text10 / 6htolera /
  data text11 / 6hnces   /
  data text12 / 6hline c /
  data text13 / 6honstan /
  data text14 / 6hts     /
  data text15 / 6hcable  /
  data text16 / 6hconsta /
  data text17 / 6hnts    /
  ll6 = 6
  if ( lastov  .eq.  1 )   go to 3107
  if ( iprsup  .ge.  1 ) write (lunit6, 3106)  lunit5, ialter, lastov, nss, icheck, iotx, ioss, iofl, ktab, ci1, ck1, voltbc(1), voltk(1), volti(1)
3106 format (/,  " At beginning of 'over45' .  lunit5  ialter  lastov     nss  icheck    iotx    ioss    iofl    ktab", /, &
       27x,  9i8, /, 13x, 3hci1, 12x, 3hck1, 6x, 9hvoltbc(1), 7x, 8hvoltk(1), 7x, 8hvolti(1),    /, 1x, 5e15.6     )
  if ( ialter  .eq.  3 )   go to 10140
  go to 10180
  !     default definition of  'tolerances'  parameters.
3107 eps = 0.5d-4
  eps1 = .5d-2
  fit2z = 1.0d-1
  pivthr = 1.0d-5
  epsrse = 1.0d-2
  ft2emx = 1.0d-4
  fmed = 5000.
  epsyc = 1.0d-3
  epsn = 1.0d-3
  epspv2 = 1.0d-16
  ffinp = 1.0
  epseig = 1.0d-7
  nieig = 30
  nfit = 10
  niter = 10
  niter1 = 10
  nitery = 10
  m = 1
  do  i=1, 24
10014 vstacs(i) = blank
  end do
  !     read input card using cimage
2946 call cimage
10015 ialter = 1
  read (unit = abuff, fmt = 10020) bus1
10020 format(13a6,a2)
  if (bus1 .ne. text1) go to 10060
  !     optional  'branch'  card, which specifies  a6  branch names.
  n1 = m + 11
  read (unit = abuff, fmt = 10040) (vstacs(i), i = m, n1)
10040 format(8x,12a6)
  m = m + 12
  write (kunit6, 10050)
10050 format (26h+bus names for each phase.)
  go to 2946
10060 continue
  read (unit = abuff(1), fmt = 10020) bus1, bus2
  if (bus1 .ne. text10) go to 10090
  if (bus2 .ne. text11) go to 10090
  !     optional  'tolerances'  card, to redefine overlay tolerances.
  read (unit = abuff, fmt = 10065) (mfrecv(i), russ(i), i = 1, 5)
10065 format(12x,5(i2,e11.0))
  write (kunit6, 10070)  ( mfrecv(i), i=1, 5 )
10070 format ( 24h+new semlyen tolerances.,  5i4  )
  do i=1, 5
     n1 = mfrecv(i)
     if ( n1  .le.  0 )   go to 10080
     if ( n1  .gt.  18 )   go to 10080
     if (n1 .gt. 13) go to 10075
     vim(n1) = russ(i)
     go to 10080
10075 ipntv(n1-13) = russ(i)
10080 end do
  go to 2946
  !     read  'semlyen setup'  miscellaneous data parameters.
10090 continue
  read (unit = abuff, fmt = 10100) bus1, bus2, noo, npoint, nss, kfit, kps, kyc, ipun, idoc, iotx, ioss, iofl, npan
10100 format(a6,a2,2i5,6i2,3i6,24x,i3)
  write (kunit6, 10095)  noo, npoint, nss, kfit, kps, kyc, ipun
10095 format ( 12h+misc. data.,  7i5  )
  if ( iprsup  .ge.  1 ) write (lunit6, 10097)  noo, npoint, nss, kfit, kps, kyc, ipun, idoc
10097 format ( /,  20h semlyen misc. data., '     noo  npoint     nss    kfit     kps     kyc    ipun    idoc   ', /,  20x,  8i8  )
  if ( bus1  .ne.  text5 )   go to 10120
  if ( bus2  .ne.  text6 )   go to 10120
  ialter = 3
  if ( nss + icheck .eq. 2 ) go to 10132
  kill = 182
  lstat(14) = icheck
  lstat(15) = nss
  lstat(19) = 10132
  go to 9200
10132 continue
  read (unit = abuff(1), fmt = 10130) russ(1), ck1
10130 format( 48x,2e12.5 )
  if ( ck1 .eq. 0. ) ck1 = sll
  if ( russ(1) .eq. ci1 .or. russ(1) .eq. 0. ) go to 10131
  ialter = 2
  ci1 = russ(1)
  go to 10140
10131 rewind lunit3
  go to 9999
10120 if ( bus1  .ne.  text2 )   go to 10136
  if ( bus2  .ne.  text3 )   go to 10136
  ialter = 2
  if ( nss + icheck  .eq.  2 )   go to 10133
  kill = 182
  lstat(14) = icheck
  lstat(15) = nss
  lstat(19) = 10133
  go to 9200
10133 continue
  read (unit = abuff(1), fmt = 10130) ci1, ck1
  if ( ck1 .eq. 0. ) ck1 = sll
  go to 10140
10136 ci1 = -fltinf
  ck1 = -fltinf
10140 if ( iotx  .ne.  -1 )   go to 1774
  do i=1, 3
     n7 = 0
     n10 = 0
     !     read input card using cimage
     call cimage
     read (unit = abuff(1), fmt = 1704) (itemp(n8), n8 = 1, 12)
1704 format ( 20i4 )
     do j=1, 4
        n8 = 4
        n9 = 0
        do l=1, 3
           n7 = n7 + 1
           if ( itemp(n7)  .eq.  0 )   go to 1729
           n9 = n9 + n8
1729       n8 = n8 / 2
        end do
1734    n10 = n10 * 10  +  n9
     end do
     if ( i  .gt.  1 )   go to 1743
     iotx = n10
     write (kunit6, 1739)  iotx
1739 format ( 38h+  'iotx'  components.   octal value =,  i5   )
     go to 1765
1743 if ( i  .gt.  2 )   go to 1753
     ioss = n10
     write (kunit6, 1748)  ioss
1748 format ( 38h+  'ioss'  components.   octal value =,  i5   )
     go to 1765
1753 iofl = n10
     write (kunit6, 1757)  iofl
1757 format ( 38h+  'iofl'  components.   octal value =,  i5   )
1765 end do
1774 do i=7, 9
     n1 = iabs(indtv(i))
     n2 = 0
     n3 = 1
10150 n4 = n1/10
     n1 = n1 - 10*n4
     if (n1 .lt. 8) go to 10160
     kill = 154
     lstat(13) = i-6
     lstat(14) = indtv(i)
     lstat(19) = 10160
     go to 9200
10160 n2 = n2 + n3*n1
     n3 = 8*n3
     n1 = n4
     if (n1 .gt. 0) go to 10150
     indtv(i) = n2
10170 end do
  if (ialter .gt. 2) go to 10310
  if (ialter .eq. 2)  go to 10172
  if ( lastov  .eq.  1 )   go to 3121
  if (lastov .eq. 44)  go to 3121
  if (lastov .eq. 47)  go to 3121
10172 n1 = nchain
  nchain = lastov
  lastov = n1
  go to 9900
  !     read input card using cimage.
3121 call cimage
  read (unit = abuff(1), fmt = 10020) bus3, bus4, bus5
  if ( bus3  .ne.  text12 )   go to 3128
  if ( bus4  .ne.  text13 )   go to 3128
  if ( bus5  .ne.  text14 )   go to 3128
  if ( iprsup .ge. 1 ) write (lunit6, 3123)
3123 format ( 32h transfer to  'line constants' .     )
  kreqab = 0
  lastov = nchain
  nchain = 44
  go to 9900
3128 if ( bus3  .ne.  text15 )   go to 3136
  if ( bus4  .ne.  text16 )   go to 3136
  if ( bus5  .ne.  text17 )   go to 3136
  write (kunit6, 3132)
3132 format (  33h+transfer to  'cable constants' .   )
  kreqab = 1
  lastov = nchain
  nchain = 47
  go to 9900
3136 kill = 187
  lstat(19) = 3136
  go to 9200
10180 if (nss+icheck .eq. 2) go to 10190
  kill = 182
  lstat(14) = icheck
  lstat(15) = nss
  lstat(19) = 10190
  go to 9200
10190 do i=1, icheck
     if ( volti(i)  .eq.  ci1 )   go to 10200
     kill = 183
     lstat(13) = i
     lstat(19) = 10200
     go to 9200
10200 end do
  lcin = 0
  n1 = 1
  rewind lunit2
  rewind lunit3
  n1 = 0
  do n12 = 1, 9999
     read (lunit2,10290) (pl(i),i=1,14)
10290 format(13a6,a2)
     lcin = lcin + 1
     if ( iprsup  .ge.  1 ) write (lunit6,3148) lcin, (pl(i),i=1,14)
3148 format ( /, 39h 80-col. card image from 'lunit2' , no.,  i3, 1h.,  13a6,  a2 )
     if (pl(1) .eq. blank  .and. pl(2) .eq. blank ) n1 = n1 + 1
     if ( n1 .ge. 1 .and. kreqab .eq. 1) go to 10310
     if ( n1 .ge. 2 .and. kreqab .eq. 0 ) go to 10310
10210 end do
10310 sll = ck1
  if (npan .gt. 0) xpan = twopi / npan
  si(1) = 0.
  iss = 0
  cold = 0.
  if (kreqab .eq. 1)   go to 10320
  spdlt = 186283.5807d0
  go to 10330
10320 spdlt = speedl
10330 d1 = 10.
  conv5 = 20. / alogz(d1)
  nfrph = nfr1 * nph
  ratio = voltbc(3)
  dplu = ratio + 1.0
  dmin = ratio - 1.0
  lcosi = 3 * nfrph + 1
  kprec = 1
  if( locint( pbuf(11)) - locint(pbuf(1)) .eq. locint(zcos(11)) - locint(zcos(1)) )
1 kprec = 2
  lcosd = lcosi / 2 + 1
  if( kprec .eq. 2 ) lcosd = lcosi
  lift = lcosi + nfr1 * 2 + 1
  if( kprec .eq. 2 ) lift = lcosi + nfr1
  ipunm = ipun / 10
  ipun = ipun - 10 * ipunm
  if (iotx .gt. 0) write (lunit6,10490) nss
10490 format ('1', 5x, 'Calculation of transformation matrices.  Continuous transposition flag = ', i2)
  !              ***   at the transformation frequency   ***
  if (nss .eq. 1) go to 10520
  if (iotx .lt. 128) go to 10500
  kill = 185
  lstat(13) = iotx
  lstat(14) = 127
  lstat(19) = 10500
  go to 9200
10500 iwork = 2
  f = voltk(1)
  w = twopi*f
  call frqdom ( iotx, pbuf, z, y, zy, zya, zyb, zyc, zyd, cq, cqt, q, qi, g, g60, yo, xr, xl, xg, xc)
  if ( kill  .gt.  0 )   go to 9200
  j = 1
  do i=1, n22, 2
     q(j) = cq(i)
     qi(j) = zya(i)
10510 j= j + 1
  end do
  if( ipunm .lt. 1 ) go to 10605
  zero = 0.
  do i = 1, nph
     write(lunit7,6002) ( qi(j), j = i, nphsq, nph )
6001 write(lunit7,6002) ( zero, j=1, nph )
  end do
6002 format(6f12.9)
  go to 10605
10520 k=1
  do j=1, nph
     do i=1, nph
        q(k) = 1.
        if (i .eq. j .and. i .ne. 1) q(k) =       1 - i
        if (i .gt. j .and. j .ne. 1) q(k) = 0.
10530   k = k + 1
     end do
10540 end do
  do i=1, nphsq, nph
     k = i + nph - 1
     temp = 0.
     do j=i, k
10550   temp = temp + q(j)**2
     end do
     temp = sqrtz(temp)
     do j=i, k
        q(j) = q(j)/temp
10560   qi(j) = q(j)
     end do
10565 end do
  iotx = iotx/2
  n1 = iotx/2
  if (n1+n1 .eq. iotx) go to 10605
  if (nph .le. 6) go to 5025
  write (lunit6,5020)
5020 format(1h0,41x,21hmode to phase voltage   ,/, 42x, 21htransformation matrix       )
  do i=1, nph
     write (lunit6,5021) (q(j), j=i, nphsq, nph)
5021 format(/ (1x,12(2x,f8.6)))
5022 end do
  write (lunit6,5023)
5023 format(1h0,41x,21hmode to phase current   ,/, 42x, 21htransformation matrix   )
  do i=1, nph
5024 write (lunit6,5021) (qi(j), j=i, nphsq, nph)
  end do
  go to 10605
5025 write (lunit6,10570)
10570 format('0', 21x, 'Mode to phase voltage ', 44x, 'mode to phase current ', /, 22x, 'transformation matrix ', 45x, 'transformation matrix', //)
  do i=1, nph
     write (lunit6,10590) (q(j),j=i,nphsq,nph)
10590 format(65x,1h*/1x,6(2x,f8.6))
10580 write (kunit6, 10600) (qi(j),j=i,nphsq,nph)
  end do
10600 format(1h+,64x,1h*,1x,6(2x,f8.6))
  !              ***   at the steady-state frequency   ***
10605 f = voltk(icheck)
  w = twopi*f
  iadm = 1
  iss = 1
  ww(iadm) = w
  if ( ioss  .gt.  0 ) write (lunit6, 10610 )  f
10610 format (//, ' Calculation of steady-state parameters for frequency = ',  e15.3,  '   Hz.'    )
  iwork = 896
  call frqdom ( ioss, pbuf, z, y, zy, zya, zyb, zyc, zyd, cq, cqt, q, qi, g, g60, yo, xr, xl, xg, xc)
  if ( kill  .gt.  0 )   go to 9200
  iss = 0
  do i=1, nph2
10615 um(i) = yo(i)
  end do
  j = 1
  do i = 1, nph2, 2
     jp1 = j + 1
     ymin(i) = sqrtz ( y( jp1) / z ( jp1 ) )
     ymin(i+1) = sll * g(i+1) / w
10616 j = j + nphpi2
  end do
10620 wo = twopi*voltbc(1)
  wm = wo*ratio**(nfr-1)
  si(nfr1 + 1) = wm * ratio
  do i=1, 8
10630 mfrecv(i) = i*nfr/10
  end do
  mfrecv(9) = nfr - 1
  mfrecv(10) = nfr
  write(lunit6,10640) nfr, ( voltbc(i), i=1,3 )
10640 format('1', 2x,'no.freq = ', i3, 5x, 'first frequency = ', f10.5, 5x, 'last frequency = ', f10.1, 5x, 'geo. ratio = ', f5.3 )
  do i=1, nph
     n1 = i*nfr1 - nfr
10650 vresp(n1) = ffinp
  end do
  kmin = 640
  if (kfit .le. 9 .and. kyc .le. 9) kmin = 128
  kypr = kyc / 10
  kyc = kyc - 10 * kypr
  jxq = 0
  if (kyc .ne. 3)  go to 3178
  d13 = 2.0 * voltbc(1) * tt
  jxq  =   - alogz(d13) /  alogz(ratio)
  jxq = jxq + 1
  !     this loop calculates the line step response for both the
  !     propagation function(nfr points) and for the admittance function
  !     (3 points) in the frequency domain.
3178 iadm = 2
  il = 1
  w = wo
  constm = 0.62137
  if( kreqab .eq. 1 ) constm = 1000.
  do jm=1, nfr
     si(jm+1) = w
     f = w/twopi
     !     factor for surpression of the gibb's phenomenon.
     !     note.  1./w factor converts impulse to step response.
     sif = w*twopi/wm/2.
     sif = sinz(sif)/sif/w
     if (jm .ne. jxq) go to 10653
     n1 = iofl
     iwork = 640
     d1 = 1.0/(2.*tt)
     if (n1 .ne. 0) write (lunit6,10651) d1
10651 format('1', 10x, 'This frequency is closest(from below) to the natural frequency, f = (speed of light)/(2*length) = ', f10.2, /, 11x, &
          'it will be used to calculate the constant characteristic admittance for use in the time-step loop convolution.'   )
     ww(1) = w
     do i=1, nph2
10652   um(i) = yo(i)
     end do
     call frqdom ( n1, pbuf, z, y, zy, zya, zyb, zyc, zyd, cq, cqt, q, qi, g, g60, yo, xr, xl, xg, xc)
     go to 10656
10653 iwork = kmin
     n1 =  0
     if (jm .ne. mfrecv(il)) go to 10655
     n1 = iofl
     iwork = 640
     if (n1 .ne. 0 .and. jm .ne. 1) write (lunit6,10654)
10654 format( //, 1x )
10655 call frqdom ( n1, pbuf, z, y, zy, zya, zyb, zyc, zyd, cq, cqt, q, qi, g, g60, yo, xr, xl, xg, xc )
     if ( kill  .gt.  0 )   go to 9200
     if( ipunm .lt. 1 ) go to 10656
     amarti(1) = w
     j = 1
     do i = 1, nph
        amarti(2) = y(j) * constm
        amarti(3) = y(j+1) * constm
        amarti(4) = z(j) * constm
        amarti(5) = z(j+1) * constm
        write(lunit7,6005) amarti
6004    j = j + nphpi2
     end do
6005 format( 5e16.9 )
10656 ix = jm + 1
     j = 1
     do i = 1, nph2, 2
        jp1 = j + 1
        ttchk = sqrtz( y(jp1) / z(jp1) )
        if( ttchk .lt. ymin(i) ) ymin(i) = ttchk
10657   j = j + nphpi2
     end do
10658 do i = 1, nph2, 2
        d1 = sif * expz(-g(i) * sll)
        d2 = g(i + 1) * sll
        ttchk = d2 / w
        if( ttchk .gt. ymin(i+1) ) ymin(i+1) = ttchk
        vresp(ix) = d1 * cosz( d2 )
        n1 = ix + nfrph
        vresp(n1) = d1 * sinz( d2 )
        n1 = n1 + nfrph
        vresp(n1) = yo(i)
10660   ix = ix + nfr1
     end do
     if (jm .ne. mfrecv(il))go to 10690
     il = il + 1
     cst = w/twopi
     if (cst .lt. fmed) go to 10670
     iadm = 3
10670 ww(iadm) = w
     n1 = nph2 * (iadm - 1)
     do i=1, nph2
        n2 = n1 + i
10680   um(n2) = yo(i)
     end do
10690 w = wo * ratio**jm
  end do
  w = twopi * voltk(icheck)
  if (iprsup .lt. 3) go to 70140
  write (lunit6,70550) (i,i=1, nph)
70550 format ( 1h0, 5x, 'Propagation step response in the frequency domain.' &
       ,/,  '0',  5x,  5homega,  3( 21x, 5hmode , i2, 6x) &
       ,/, (11x,  3(21x, 5hmode , i2, 6x)  )  )
  jmax = nfrph
  do i=1, nfr1
70130 write(lunit6,70510) si(i), ( vresp(n2), vresp(n2+nfrph), n2 = i, jmax, nfr1 )
  end do
  if ( kfit .le. 9 .and. kypr .le. 0 ) go to 70140
  write( lunit6,70131) ( i, i=1,nph )
70131 format(1h1, 5x, 49hadmittance step response in the frequency domain. ,/, 1h0, 5x, 5homega, 3( 21x, 5hmode ,i2, 6x ) ,/, &
       ( 11x, 3( 21x, 5hmode ,i2, 6x ) ) )
  jmax = 3 * nfrph
  do i = 1, nfr1
     n1 = 2 * nfrph + i
70551 write(lunit6,70511) si(i), ( vresp(n2), n2 = n1, jmax, nfr1)
  end do
70511 format( 4x, e12.5, 3( 10x, e13.5, 9x ) ,/, ( 16x, 3( 10x, e13.5, 9x ) ) )
70140 continue
  pi2 = 4./twopi
  do i=1, 91
70150 pl(i) = blank
  end do
70510 format ( 4x, e12.5,  6( 2x, e13.5, 2x )   ,/, ( 16x,  6( 2x, e13.5, 2x )  )  )
  kfys = kfit / 10
  kfps = kfit - 10 * kfys
  if (kfys .eq. 0 .and. kypr .eq. 0) go to 230
  n1 = nfrph + nfrph + 1
  do i=1, nph2, 2
     vresp(n1) = yo(i) - vresp(n1+1)
200  n1 = n1 + nfr1
  end do
  !     calculate frequency domain sequence used to produce the char-
  !     acteristic admittance step reponse in the time domain
  do jm=2, nfr1
     w = si(jm)
     sif = onehaf * twopi * w / wm
     sif = sinz(sif) / sif / w
     n1 = nfrph + nfrph + jm
     do i=1, nph2, 2
        vresp(n1) = sif * (yo(i) - vresp(n1))
210     n1 = n1 + nfr1
     end do
220 end do
230 d1 = fltinf
  n1 = 1
  do i=1, nph
     if (d1 .lt. um(n1)) go to 344
     d1 = um(n1)
     kmin = i
344  n1 = n1 + 2
  end do
  if (kyc .eq. 1 .or. kyc .eq. 3) kmin = nph + nph
  iy = 2
  iy3 = 3*iy
240 if (iprsup .le. 5) go to 640
  write (lunit6,610)
610 format ( 1h0, 5x, 73hdata for fitting of characteristic admittance exponential approximation.  ,/,  10x,  9hfrequency,  17x, &
       31h... cmplx. char. admittance ...   )
  do i=1, nph
     n1 = (i - 1) * nph2 + 1
     n2 = n1 - 1 + nph2
     d1 = ww(i)/twopi
620  write (lunit6,630) d1, (um(j),j=n1,n2)
  end do
630 format( /,  1x,  7(5x, e12.5)  ,/,  (18x,  6(5x, e12.5) )  )
640 write (lunit6,10654)
  if( ipunm .eq. 2 ) go to 437
  !     loop over all modes to calculate(via inverse fourier
  !     transformation)   and fit(via least squares) the propagation step
  !     response in the time domain.
  w = twopi * voltk(icheck)
  do i1=1, nph
     lstat(14) = i1
     write (lunit6,40060) i1
40060 format(1h1,10x,50hfitting of the propagation step response for mode  ,  i2,  ///  1x)
     ix = (i1 - 1)*nfr1 + 1
     i2 = i1 + i1
     i3 = 14 * i1
     i4 = nfrph + nfrph + ix
     tretrd = sll * g(i2) / wm
     tt = ymin(i2) - tretrd
     ffin = ffinp
     tstrt = tretrd * 0.98
     tstep = tt / noo
     flstat(11) = tstep
     vresp(lift) = vresp(ix)
     do j=1, nfr
        n1 = ix + j
        n2 = n1 + nfrph
        n3 = lift + j
        sif = tstrt * si(j+1)
        vresp(n3) = vresp(n1) * cosz(sif) + vresp(n2) * sinz(sif)
        if ( iprsup  .ge. 6 ) write (lunit6, 40002)  j, sif, vresp(n1), vresp(n2), vresp(n3)
40002   format ( 5x, i10,  4( 5x,  e15.7 )  )
40020 end do
     ictrl = kps
     if (kfps .eq. 0) go to 40040
     i5 = lift
     d1 = 0.02 * tretrd
     d2 = d1
     d3 = 1. / 3.
     d4 = 2. / 3.
     call rise( d2, d4, tstep, vresp(i5), si, zcos( lcosd ) )
     if( kill .le. 0 ) go to 40026
     lstat(10) = npoint
     lstat(13) = kfps
     lstat(15) = kfit
     lstat(19) = 40026
     flstat(16) = d4
     go to 9200
40026 d1 = d2
     tstep =  4. * d2 / npoint
     if ( kfps .eq. 1 ) go to 40040
     tstep = d2 / 10.
     flstat(11) = tstep
     call rise( d1, d3, tstep, vresp(i5), si, zcos( lcosd ) )
     if( kill .le. 0 ) go to 40027
     lstat(10) = npoint
     lstat(13) = kfps
     lstat(14) = i1
     lstat(19) = 40027
     flstat(16) = d3
     go to 9200
40027 tstep = 20. * ( d2 - d1 ) / npoint
40040 i5 = lift
     shifti = expz( - g60(i2 - 1) * sll)
     d1 = g60(i2) * sll
     shiftr = shifti * cosz( d1 )
     shifti = shifti * sinz( d1 )
     call tdfit( vresp(i5), si, fv, hhm, hhn, zcos( lcosd ) )
     if (kill .gt. 0) go to 9200
     pbuf(i3 - 4) = d
     pbuf(i3 - 3) = x(1)
     pbuf(i3 - 0) = x(3)
     pbuf(i3 - 2) = ffin - d
     pbuf(i3 - 1) = x(2)
     !     time domain fitting of the characteristic admittance step
     !     response function
     tstrt = 0.0
     ffin = yo( i2 - 1 ) - ymin( i2 - 1 )
     if (kfys .eq. 1) go to 40070
     !     fitting of characteristic admittance step in the frequency
     !     domain.   two points are used to fit the transform of
     !      yc(i) = y0 + e(1)*exp(-t/b(1))  +  e(2)*exp(-t/b(2))
     !     iteratively.
     if ( kyc .eq. 0 ) go to 401
     if (kmin .eq. i1) go to 401
     pbuf(i3-9) = 0.0
     pbuf(i3-8) = 1.0
     pbuf(i3-7) = 0.0
     pbuf(i3-6) = 1.0
     pbuf(i3-5) = um(i2 - 1)
     write (lunit6, 400)  i1, pbuf(i3 - 5)
400  format ( /, 5x, 5hmode ,  i2, 37hconstant characteristic admittance = , e12.5  )
     go to 40090
401  n1 = i1 + i1
     n2 = n1 + nph2 + nph2
     ygr(1) = um(n2 - 1) - um(n1 - 1)
     ygr(2) = um(n2    ) - um(n1    )
     ygr(3) = ww(1)
     n1 = n1 + nph2
     ygr(4) = um(n2 - 1) - um(n1 - 1)
     ygr(5) = um(n2    ) - um(n1    )
     ygr(6) = ww(2)
     ysurg = ymin ( i2-1 )
403  call setplt
     write( lunit6, 404 ) i1
404  format ( //// 5x, 46hfitting for characteristic admittance mode no   ,  i2  )
     if (iprsup .gt. 0) write (lunit6,99887) kmin, n1, n2, kyc, (ygr(j), j=1, 6)
99887 format(/,5x,51hkmin, n1, n2, kyc, ygr(j), j=1, 6) follow ..... ,/,  4i10, 6e15.8 )
     do j=1, iy
        e(j) = 0.0
        b(j) = 0.0
        e(j+2) = 1.0
405     b(j+2) = 1.0
     end do
     iter = 0
406  iter = iter + 1
     n1 = 1
     do j=1, iy3, 3
        zr = ygr(j)
        zi = ygr(j+1)
        n2 = 1
        do k=1, iy3, 3
           if ( j  .eq.  k )   go to 407
           d1 = ygr(j+2) * b(n2)
           d2 = 1.0  +  d1 * d1
           zr = zr + e(n2) / d2
           zi = zi - e(n2) * d1 / d2
407        n2 = n2 + 1
        end do
        b(n1) = - zi / ( ygr(j+2) * zr )
        e(n1) = -(zr * zr  +  zi * zi) / zr
408     n1 = n1 + 1
     end do
     do j=1, iy
        if ( absz( (e(j) - e(j+2) ) / e(j+2) )     .gt.   epsyc ) go to 412
        if ( absz( (b(j) - b(j+2) ) / b(j+2) )     .gt.   epsyc ) go to 412
409  end do
     do j=1, iy3, 3
        y(j) = ygr(j)
        y(j+1) = ygr(j+1)
        do k=1, iy
           d1 = ygr(j+2) * b(k)
           d2 = 1.0 + d1*d1
           y(j) = y(j) + e(k) / d2
410        y(j+1) = y(j+1) - e(k) * d1 / d2
        end do
        zr = y(j) * y(j) + y(j+1) * y(j+1)
        if ( sqrtz(zr)  .gt.  epsn )   go to 412
411  end do
     go to 415
412  do j=1, iy
        b(j+2) = b(j)
413     e(j+2) = e(j)
     end do
     if ( iprsup  .gt.  5 ) write (lunit6, 414)  iter,  ( e(j), b(j), j=1, iy )
414  format ( /,  20h admittance fitting.,  5x,  i5, 6(5x, e12.5)  )
     if ( iter  .lt.  nitery )   go to 406
     kill = 155
     lstat(13) = nitery
     flstat(13) = epsyc
     flstat(14) = epsn
     lstat(19) = 415
     go to 9200
415  write (lunit6, 416)  iter
416  format ( /, 5x,  20hno. of iterations = ,  i3,  //  )
     do j=1, iy3, 3
        d1 = ygr(j+2) / twopi
        if ( ysurg .eq. 0. ) d1  =  -1.0 / ( twopi * ygr(j+2) )
417     write(lunit6, 418) d1, ygr(j), ygr(j+1), y(j), y(j+1)
     end do
418  format ( 2x,  12hfrequency = ,  e12.5,  5x, 10himpulse = ,  e12.5,  2x,  e12.5,  5x,  8herror =  , e12.5,  2x,  e12.5  )
     n1 = i3 - 9
     pbuf(i3 - 5) = ysurg
     do j=1, iy
        if ( ysurg .ne. 0. ) go to 4183
        b(j) = 1.0 / b(j)
        !     go to 4187
4183    e(j) = - e(j)
        pbuf(i3 - 5) = pbuf(i3 - 5) + e(j)
4187    pbuf(n1) = e(j)
        pbuf(n1 + 1) = b(j)
        if (b(j) .le. 0.0) pbuf(n1) = 0.0
419     n1 = n1 + 2
     end do
     write (lunit6, 420)  ( e(j), j=1, iy )
420  format ( 5x,  13hamplitudes = ,   3(e12.5, 5x)   )
     write (lunit6, 421)  ( b(j), j=1, iy )
421  format ( 5x, 13htime const = ,   3(e12.5, 5x)   )
     write (lunit6, 422)  pbuf(i3-5)
422  format ( 5x,  45hinitial value of characteristic admittance = , e12.5  )
40090 if ( kypr .eq. 0   .and.   kfys .eq. 0 )   go to 40200
     if( kypr .eq. 1 .and. kfys .eq. 0 ) ffin = pbuf(i3-9) + pbuf(i3-7)
     d = pbuf(i3 - 9)
     x(1) = pbuf(i3 - 8)
     x(2) = pbuf(i3 - 6)
     x(3) = 0.0
     ictrl = -10
     tstep = pbuf(i3 - 8)
     tt = tstep
     if (tstep .lt. pbuf(i3 - 6)) tstep = pbuf(i3 - 6)
     if ( tt .gt. pbuf(i3-6) ) tt = pbuf(i3-6)
     tt = tt * 2.
     tstep = tstep / 10.
     if ( tt .gt. tstep ) tt = tstep
     tstep = tt / npoint
     if ( kfys .le. 0 )  go to 40030
40070 write (lunit6,40050) i1
40050 format(1h1, 10x, 72htime domain fitting of characteristic admittance step response for node , i2 )
     ictrl = -kyc - 1
     pbuf(i3 - 5) = yo(i2 - 1)
     shiftr = yo(i2 - 1) - um(i2 - 1)
     shifti = yo(i2    ) - um(i2    )
     if (kfys .gt. 1) go to 40030
     tt = .005
     tstep = tt / 200.
40080 d1 = tstep
     targ = 0.5
     flstat(11) = tstep
     if( iprsup .gt. 3 ) write( lunit6,40082 ) d1, targ, tstep, ffin, tt, yo(i2-1)
40082 format( 5x, 'At entry to rise, initial time, target, timestep, final value, tt, and initial value are ', /, 25x, 6e12.5 )
     call rise( d1, targ, tstep, vresp(i4), si, zcos( lcosd ) )
     if( kill .le. 0 ) go to 40081
     lstat(10) = npoint
     lstat(13) = kfys
     lstat(15) = kfit
     lstat(19) = 40080
     flstat(13) = tt * 2
     flstat(16) = targ
     go to 9200
40081 tstep = d1 / npoint * 2.
     tt = npoint * tstep
     write( lunit6, 422 ) pbuf(i3-5)
40030 call tdfit( vresp(i4), si, fv, hhm, hhn, zcos( lcosd ) )
     if (kill .gt. 0) go to 9200
     tt = sll / spdlt
     pbuf(i3 - 9) = d
     pbuf(i3 - 8) = x(1)
     pbuf(i3 - 7) = ffin - d
     pbuf(i3 - 6) = x(2)
40200 end do
  w = twopi * voltk(icheck)
  w2 = w * w
  write( lunit6,706 ) w
706 format(//, 15x, f5.1, 'Hz steady state modal parameters in ohms and micromhos  ', /,  &
       15x,  'as calculated from fitted exponentials via Fourier transform ', //, 25x, &
       'mode     r           x     g           b ')
  do i1 = 1, nph
     i3 = 14 * i1
     i2 = i3-13
     if( iprsup .ge. 3 ) write( lunit6,702 ) i1, i2, i3, ( pbuf(i), i=i2,i3 )
702  format ( 14h pbuf for mode, i3, 5h  i2=, i3, 5h  i3=, i3 ,/, (1x,  8e16.8 ) )
     den1 = 1.0 / pbuf(i3-3)
     den1 = den1 * den1 + w2
     den2 = 1.0 / pbuf(i3-1)
     den2 = den2 * den2 + w2
     hssr = 1.0 - pbuf(i3-4) * w2 / den1 - pbuf(i3-2) * w2 / den2
     hssi = - pbuf(i3-4) / pbuf(i3-3) * w / den1 - pbuf(i3-2) / pbuf(i3-1) * w / den2
     den1 = 1.0 / pbuf(i3-8)
     den1 = den1 * den1 + w2
     den2 = 1.0 / pbuf(i3-6)
     den2 = den2 * den2 + w2
     yssr = pbuf(i3-5) -(pbuf(i3-9) + pbuf(i3-7)  ) + pbuf(i3-9) * w2 / den1 + pbuf(i3-7) * w2 / den2
     yssi = pbuf(i3-9) / pbuf(i3-8) * w / den1 + pbuf(i3-7) / pbuf(i3-6) * w / den2
     hamp = ( hssr * hssr + hssi * hssi )**.5
     call datn2z(hssi,hssr,dd11)
     hang = w * pbuf(i3) - dd11
     call dlogz(hamp, dd11)
     alnh = -dd11
     pbuf(i3-11) = alnh * yssr - hang * yssi
     pbuf(i3-10) = alnh * yssi + hang * yssr
     den1 = yssr * yssr + yssi * yssi
     pbuf(i3-13) = ( alnh * yssr + hang * yssi ) / den1
     pbuf(i3-12) = ( hang * yssr - alnh * yssi ) / den1
     pbuf(i3-10) = pbuf(i3-10) / tenm6
     pbuf(i3-11) = pbuf(i3-11) / tenm6
     i4 = i3-10
     if( iprsup .ge. 3 ) write(lunit6,703) i1, i2, i4, hssr, hssi, yssr, yssi, hamp, hang, (pbuf(i),i=i2,i4)
703  format(55h during calculation of steady state parameters for mode, i3,48h  hssr, hssi, yssr, yssi, hamp, hang, pbuf(i),i=, i3, 1h,, &
          i3, /,1x, 10e13.4 )
     write( lunit6,705 ) i1, ( pbuf(i), i=i2, i4 )
705  format( 26x, i2, 4e12.5 )
701 end do
  !     documentation of output cards to be punched.
  if (ipun .ne. 1) write (lunit6,425)
425 format(//,27x, 34hsequential list of punched output.     ,/, 27x,34(1h-),//,4x, &
       '0         1         2         3         4        5         6         7         8  ', /, &
       9(4x,1h0,5x), /, 4x, 81('-' ), /, 1x)
  nss = iabs(nss)
  kfit = iabs(kfit)
  kps = iabs(kps)
  kyc = iabs(kyc) + iabs(kypr) * 10
  f = voltk(icheck)
  if( kreqab .eq. 1 ) go to 704
  if (ipun .ne. 1) write (lunit6,449) sll,ci1,f,nss,kfit,kps,kyc
449 format(5x,4hc l=,f8.1,11hmiles, rho=,f8.1,10h, ss freq=,f8.2, 6h, nss=,i1,7h, kfit=,i2,6h, kps=,i1,6h, kyc=,i2)
  if (ipun .ne. 2) write (lunit7,448) sll,ci1,f,nss,kfit,kps,kyc
448 format (  4hc l=,f8.1,11hmiles, rho=,f8.1,10h, ss freq=,f8.2, 6h, nss=,i1,7h, kfit=,i2,6h, kps=,i1,6h, kyc=,i2)
  go to 452
704 if( ipun .ne. 1 ) write( lunit6,453 ) sll, ci1, f, nss, kfit, kps, kyc
453 format(5x,4hc l=, f8.0, 12hmeters, rho=, f8.1, 10h, ss freq=,f8.2, 6h, nss= , i1, 7h, kfit= , i2, 6h, kps= , i1, 6h, kyc= , i2 )
  if( ipun .ne. 2 ) write(lunit7,451) sll, ci1, f, nss, kfit, kps, kyc
451 format(   4hc l=, f8.0, 12hmeters, rho=, f8.1, 10h, ss freq=,f8.2, 6h, nss= , i1, 7h, kfit= , i2, 6h, kps= , i1, 6h, kyc= , i2 )
452 if ( idoc .eq. 0 ) go to 428
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunit2
  do j=1, lcin
     read (lunit2,447) (pl(i),i=1,14)
     if (ipun .ne. 1) write (lunit6,446) (pl(i),i=1,14)
     if (ipun .ne. 2) write (lunit7,445) (pl(i),i=1,13)
429 end do
441 format(5x,2h-1,2a6,12x,2e12.5,12x,2i3,8h  2  2  ,i2/ 7x,5e15.8,/, (7x, 6e12.5))
440 format(2h-1,2a6,12x,2e12.5,12x,2i3,8h  2  2  ,i2/ 2x,5e15.8,/, (2x,6e12.5))
443 format(7x,6e12.5)
444 format(2x,6e12.5)
445 format(2hc , 13a6)
446 format(5x, 2hc , 14a6)
447 format(13a6, a2)
428 d1 = 0.0
  !     punch modal data
  do i=1, nph
     kmax1 = 14*i
     kmin1 = kmax1 - 4
     kmax2 = kmax1 - 6
     kmin2 = kmax1 - 9
     kmin = kmax1 - 13
     kmax = kmax1 - 10
     pbuf(kmin + 2) = pbuf(kmin + 2) * tenm6
     pbuf(kmin + 3) = pbuf(kmin + 3) * tenm6
     pbuf(kmin1 + 1) = 1.0 / pbuf(kmin1 + 1)
     pbuf(kmin1 + 3) = 1.0 / pbuf(kmin1 + 3)
     pbuf(kmin2 + 0) = - pbuf(kmin2 + 0)
     pbuf(kmin2 + 1) = 1.0 / pbuf(kmin2 + 1)
     pbuf(kmin2 + 2) = - pbuf(kmin2 + 2)
     pbuf(kmin2 + 3) = 1.0 / pbuf(kmin2 + 3)
     if (ipun .ne. 1) write (lunit6, 441) vstacs(2*i-1), vstacs(2*i), pbuf(kmax2+1), pbuf(kmin1+4), i, i, nph, (pbuf(k), k=kmin, kmax), voltk(icheck), &
          d1, pbuf(kmin1+1), pbuf(kmin1+0), d1, pbuf(kmin1+3), pbuf(kmin1+2), d1, pbuf(kmin2+1), pbuf(kmin2+0), d1, pbuf(kmin2+3), pbuf(kmin2+2)
     if (ipun .ne. 2) write (lunit7, 440) vstacs(2*i-1), vstacs(2*i), pbuf(kmax2+1), pbuf(kmin1+4), i, i, nph, (pbuf(k), k=kmin, kmax), voltk(icheck), &
          d1, pbuf(kmin1+1), pbuf(kmin1+0), d1, pbuf(kmin1+3), pbuf(kmin1+2), d1, pbuf(kmin2+1), pbuf(kmin2+0), d1, pbuf(kmin2+3), pbuf(kmin2+2)
430 end do
  !     punch mode to phase voltage & current transformation matrices.
  do i=1, nph
     if (ipun .ne. 1) write (lunit6, 443) (q(k), d1, k=i, nphsq, nph)
     if (ipun .ne. 2) write (lunit7, 444) (q(k), d1, k=i, nphsq, nph)
433 end do
  do i=1, nph
     if (ipun .ne. 1) write (lunit6, 443) (qi(k), d1, k=i, nphsq, nph)
     if (ipun .ne. 2) write (lunit7, 444) (qi(k), d1, k=i, nphsq, nph)
436 end do
437 write( lunit6,10654 )
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 10290) (vstacs(i), i = 1, 14)
  do i=1, 14
     if (vstacs(i) .ne. blank)  go to 516
450 end do
  write (kunit6, 470)
470 format ( 43h+blank card terminating   'semlyen setup' .   )
  call interp
  nchain = 51
  ialter = 0
  go to 9900
516 do i=1, 14
523  vstacs(i) = blank
  end do
  m = 1
  go to 10015
9200 nchain = 51
  lstat(18) = 45
  if (iprsup .ge. 1) write (lunit6,480) kill,lstat(19),lstat(10)
480 format(31h0 over45 exit with kill code = ,i5,5x,2i10)
9900 lastov = 45
9999 return
end subroutine guts45
!
! subroutine cxc.
!
subroutine cxc(a,b,c,kode)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !)  this subroutine performs some of the matrix manipulation procedures
  !)  used by over45 on linearized complex matrices stored in column
  !)  order.  these processes include multiplication by complex mtx, by
  !)  real part of complex mtx, by transpose of real part oc complex mtx
  !)  (in either order for last two) and inversion of complex mtx.
  !)  these operations each have a separate entry, where the arguments are
  !)  first, followed by the result of the procedure.
  include 'blkcom.ftn'
  include 'labl45.ftn'
  include 'volt45.ftn'
  dimension a(1), b(1), c(1), ij(50)
  equivalence (vim(9),epspv2)
  go to (1000, 2000, 3000, 4000, 5000, 6000), kode
1000 n3 = 1
  ir = 1
  n4 = 0
  do j=1, nph
     n4 = n4 + nph2
     do i=1, nph2, 2
        n1 = i
        im = ir + 1
        c(ir) = 0.
        c(im) = 0.
        do n2=n3, n4, 2
           c(ir) = c(ir) + a(n1)*b(n2) - a(n1+1)*b(n2+1)
           c(im) = c(im) + a(n1)*b(n2+1) + a(n1+1)*b(n2)
10         n1 = n1 + nph2
        end do
20      ir = ir + 2
     end do
30   n3 = n3 + nph2
  end do
  return
2000 irow = 2
  icol = nph2
40 ir = 1
  n3 = 1
  do j=1, nph
     do i=1, nph2, 2
        im = ir + 1
        n2 = n3
        c(ir) = 0.
        c(im) = 0.
        do n1=i, n22, nph2
           c(ir) = c(ir) + a(n1)*b(n2)
           c(im) = c(im) + a(n1+1)*b(n2)
50         n2 = n2 + irow
        end do
60      ir = ir + 2
     end do
70   n3 = n3 + icol
  end do
  return
3000 irow = nph2
  icol = 2
  go to 40
4000 irow = 2
  icol = nph2
75 n3 = 1
  do j=1, nph2, 2
     ir = j
     do i=1, n22, nph2
        n1 = n3
        n4 = i + nph2 - 2
        im = ir + 1
        c(ir) = 0.
        c(im) = 0.
        do n2=i, n4, 2
           c(ir) = c(ir) + a(n1)*b(n2)
           c(im) = c(im) + a(n1)*b(n2+1)
80         n1 = n1 + icol
        end do
90      ir = ir + nph2
     end do
100  n3 = n3 + irow
  end do
  return
5000 irow = nph2
  icol = 2
  go to 75
6000 do i=1, nph
110  ij(i) = 0
  end do
  do j=1, n22
120  b(j) = a(j)
  end do
  l = 0
130 l = l + 1
  if (l .gt. nph) return
  t = 0.
  n1 = 1
  do j=1, nph
     if (ij(j) .gt. 0) go to 140
     u = b(n1)*b(n1) + b(n1+1)*b(n1+1)
     if (u .le. t) go to 140
     t = u
     n2 = j
     k = n1
140  n1 = n1 + nphpi2
  end do
  ij(n2) = 1
  if (t .gt. epspv2) go to  145
  kill = 199
  flstat(15) = f
  flstat(13) = t
  flstat(14) = epspv2
  lstat(13) = l
  lstat(14) = nph
  lstat(19) = 145
  return
145 u = b(k)/t
  t = -b(k+1)/t
  b(k) = u
  b(k+1) = t
  n3 = (n2 - 1)*nph2 + 1
  n2 = 2*n2 - 1
  do i=1, nph2, 2
     if (i .eq. n2) go to 160
     n1 = i + (n2 - 1)*nph
     v = u*b(n1) - t*b(n1+1)
     d9 = u * b(n1+1)  +  t * b(n1)
     do j=1, n22, nph2
        if (j .eq. n3) go to 150
        k = i + j
        n4 = j + n2
        b(k-1) = b(k-1)  -  b(n4-1) * v  +  b(n4) * d9
        b(k) = b(k)  -  b(n4-1) * d9  -  b(n4) * v
150  end do
160 end do
  k = n2
  do i=1, nph2, 2
     if (i .eq. n2) go to 170
     n4 = n3 + i
     v = b(n4-1)*u - b(n4)*t
     b(n4) = b(n4-1)*t + b(n4)*u
     b(n4-1) = v
     v = b(k+1)*t - b(k)*u
     b(k+1) = -b(k)*t - b(k+1)*u
     b(k) = v
170  k = k + nph2
  end do
  go to 130
end subroutine cxc
!
! subroutine frqdom.
!
subroutine frqdom(ioutp, pbuf, z, y, zy, zya, zyb, zyc, zyd, cq,cqt, q, qi, g, g60, yo, xr, xl, xg, xc)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !)  this subroutine is used to process line/cable constants data at a
  !)  specific frequency.  the r, l, c matrices (low. tri, col order) are
  !)  read from tape3.  each matrix is a separate record.  the routine can
  !)  selectively calculate and/or output the following quantities.
  !)
  !)     1.  modal transformation marrices(exact only).
  !)     2.  alpha, beta, attenuation % velocity for each mode.
  !)     3.  modal distributed impedance and shunt admittance.
  !)     4.  modal characteristic impedance.
  !)     5.  modal equivalent pi impedance and admitance.
  !)     6.  phase equivalent pi matrices.
  !)
  !)  these values may be calculated on the basis of exact diagonal-
  !)  ization of zy matrix or by use of pre-calculated transformation
  !)  matrices.
  include 'blkcom.ftn'
  include 'labl45.ftn'
  include 'volt45.ftn'
  equivalence (vim(8),epseig),(ipntv(5),nieig),(indtv(2),nss)
  dimension pbuf(1), z(1), y(1), zy(1), zya(1), zyb(1), zyc(1)
  dimension zyd(1), cq(1), cqt(1), q(1), qi(1), g(1), g60(1)
  dimension yo(1), xr(1), xl(1), xg(1), xc(1)
  ll1 = 1
  ll2 = 2
  ll3 = 3
  ll4 = 4
  ll5 = 5
  ll6 = 6
  i = ioutp
  j = iwork
  k = 1
61 n1 = i/2
  n2 = j/2
  if (i .ne. n1 + n1 .and. j .eq. n2 + n2) iwork = iwork + k
  k = k + k
  i = n1
  j = n2
  if (i .ne. 0) go to 61
  !     read matrices  (g), (b), (r), and (x)  from unit 3.
  if ( iprsup  .ge.  1 ) write (lunit6, 3614)  ioutp, iwork, nph, ntri, nph2, nss, cold, f, w
3614 format ( /,  20h at begin 'frqdom' ., 48h   ioutp   iwork     nph    ntri    nph2     nss    , &
       11x,  4hcold,  14x,  1hf,  14x,  1hw   ,/, 20x, 6i8, 3e15.5  )
  if ( kreqab  .eq.  0 )   go to 3603
  n1 = 2 * nph * nph
  read (lunit3)  ( y(i), i=1, n1 )
  read (lunit3)  ( z(i), i=1, n1 )
  m=1
  do i = 1, nph
     kcol = 2 * nph * (i-1) + 1
     ktri = kcol + 2 * ( i-1 )
     do j= kcol, ktri, 2
        xg(m) = y(j)
        xr(m) = z(j)
        xc(m) = y(j+1)
        xl(m) = z(j+1)
888     m = m + 1
     end do
3615 end do
  d1 = y(2) / f
  go to 3622
3603 read (lunit3) (xg(i),i=1,ntri)
  read (lunit3) (xc(i),i=1,ntri)
  read (lunit3) (xr(i),i=1,ntri)
  read (lunit3) (xl(i),i=1,ntri)
  if ( iprsup  .ge.  4 ) write (lunit6, 3617)  xg(1), xc(1), xr(1), xl(1)
3617 format ( /,  19h after unit 3 read.  ,  10x,  5hxg(1), 10x, 5hxc(1), 10x, 5hxr(1), 10x, 5hxl(1)  ,/, 19x,  4e15.5  )
  d1 = xc(1) / f
3622 if ( cold  .eq.  0.0 )   go to 1
  d2 = ( d1 - cold ) / d1
  d3 = epsiln * 100.
  if ( absz(d2)  .le.  d3 )   go to 1
  write (lunit6, 3629)  f, cold, d1, d2
3629 format ( /, 9h at 3629     ,  4e20.10  )
  kill = 188
  flstat(13) = d1
  flstat(14) = f
  flstat(15) = cold
  flstat(19) = 1
  return
1 cold = d1
  if (ioutp .ne. 0) write (lunit6,136) f
136 format( ///, 54x, 12hfrequency = , e13.4  ,/,  54x,     25(1h*) ,/, 1x )
  if ( kreqab  .gt.  0 )   go to 3637
  l = 1
  do i=1, nph
     j = (i - 1)*nph2 + 1
     k = i + i - 1
     do n1=1, i
        z(j) = xr(l)
        z(k) = xr(l)
        y(j) = xg(l)
        y(k) = xg(l)
        z(j+1) = xl(l)
        z(k+1) = xl(l)
        y(j+1) = xc(l)
        y(k+1) = xc(l)
        j = j + 2
        k = k + nph2
2       l = l + 1
     end do
3 end do
  if (nss .eq. 0)   go to 3637
  if (nph .eq. 1)   go to 3637
  zoff = 0.0
  zoff1 = 0.0
  zdia = 0.0
  zdia1 = 0.0
  yoff = 0.0
  yoff1 = 0.0
  ydia = 0.0
  ydia1 = 0.0
  do i=1, nph2, 2
     k = ( i + 1) / 2
     l = (k-1) * nphpi2
     n = l + 1
     j = 1
     if (j .eq. k)   go to 75
     do m=i, l, nph2
        m1 = m + 1
        zoff = zoff + z(m)
        yoff = yoff + y(m)
        zoff1 = zoff1 + z(m1)
        yoff1 = yoff1 + y(m1)
70   end do
     go to 80
75   n1 = n + 1
     zdia = zdia + z(n)
     ydia = ydia + y(n)
     zdia1 = zdia1 + z(n1)
     ydia1 = ydia1 + y(n1)
80   if (i .eq. 1)   go to 90
     j = j + k - 1
     if (j .le. k)   go to 75
90 end do
  noff = nph * (nph - 1) / 2
  zoff = zoff/noff
  yoff = yoff/noff
  zoff1 = zoff1/noff
  yoff1 = yoff1/noff
  zdia = zdia/nph
  ydia = ydia/nph
  zdia1 = zdia1/nph
  ydia1 = ydia1/nph
  do i=1, nph
     j = (i-1) * nphpi2 + 1
     l = (i - 1) * nph2+ 1
     l1 = i *nph2
     do k = l, l1, 2
        k1 = k + 1
        if (k .eq. j)   go to 93
        z(k) = zoff
        y(k) = yoff
        z(k1) = zoff1
        y(k1) = yoff1
        go to 94
93      z(k) = zdia
        y(k) = ydia
        z(k1) = zdia1
        y(k1) = ydia1
94   end do
95 end do
3637 iwork = -iwork
  iout1 = ioutp
  ioutp = ioutp/2
  if (ioutp + ioutp .eq. iout1) go to 13
  if( kreqab .eq. 1 ) go to 34
  write (lunit6,131)
131 format ( /, 10x, 16himpedance matrix, 25x,  17hadmittance matrix ,/, 13x,  10h(ohm/mile),  31x,  10h(mho/mile)   )
  go to 27
34 write( lunit6,134 )
134 format( /,  10x, 16himpedance matrix, 25x,  17hadmittance matrix ,/, 14x,   8h(ohm/m ) ,  33x,   8h(mho/m )  )
  go to 27
30 iwork = iwork/2
  n1 = iwork/64
  n2 = ioutp/64
  if (64*n1 .eq. iwork .or. nss .eq. 1) go to 35
  if (64*n2 .ne. ioutp) write (lunit6,139)
139 format ( /, 10x,  82hthe following values are computed on the basis of exact diagonalization of zy mtx.    )
  dx = -(spdlt/w)**2
  j = 1
  do i=1, n22
     zya(i) = dx*zy(i)
     if (i .ne. j) go to 14
     zya(i) = zya(i) - unity
     j = j + nphpi2
14 end do
  !     calculation of eigenvalues and eigenvectors by the power method.
  do i=1, n22
41   zyb(i) = zya(i)
  end do
  l = 0
42 l = l + 1
  iq  = 0
  if (l .ge. nph) go to 414
43 iq = iq + 1
  if (iq .le. nieig) go to 44
  lstat(19) = 44
  kill = 165
  lstat(19) = 44
  return
44 do i=1, n22
45   zyc(i) = zyb(i)
  end do
  call cxc(zyc,zyc,zyb,ll1)
  diagx = 0.
  j = 1
  do i=1, nph2, 2
     dx = zyb(j)*zyb(j) + zyb(j+1)*zyb(j+1)
     if (dx .le. diagx) go to 46
     diagx = dx
     n1 = j
     n2 = i
46   j = j + nphpi2
  end do
  zyr = zyb(n1)/diagx
  zyi = - zyb(n1+1)/diagx
  do i=1, n22, 2
     dx = zyr*zyb(i) - zyi*zyb(i+1)
     zyb(i+1) = zyr*zyb(i+1) + zyi*zyb(i)
47   zyb(i) = dx
  end do
  diag = 0.
  j = (n2 - 1)*nph + 1
  k = j + nph2 - 2
  do i=j, k, 2
     if (i .eq. n1) go to 48
     dx = zyb(i)*zyb(i) + zyb(i+1)*zyb(i+1)
     if (dx .le. diag) go to 48
     diag = dx
     n3 = i
48 end do
  if (diag .gt. 0.) go to 410
  do i=n2, n22, nph2
     if (i .eq. n1) go to 49
     dx = zyb(i) * zyb(i) + zyb(i+1) * zyb(i+1)
     if (dx .le. diag) go to 49
     diag = dx
     n3 = i
49 end do
410 dx = (zyc(n1)*zyc(n1) + zyc(n1+1)*zyc(n1+1))/(zyc(n3)*zyc(n3) + zyc(n3+1)*zyc(n3+1))
  if (iprsup .gt. 6) write (lunit6,10101) n1, n2, n3, j, iq, dx, diag, diagx, epseig, (zyb(i), i=j, k), (zyb(i), i=n2, n22, nph2)
10101 format(/, 53h in eigen power loop.  n1, n2, n3,  j, iq, dx, diag,,61hdiagx, epsieg, (zyb(i), i=j, k), (zyb(i), i=n2, n22, nph2) = , &
       /, 1x, i9, 4i10, 4e20.11  ,/,  ( 1x, 6e20.11 )  )
  if (absz(sqrtz(dx*diag) - 1.0) .gt. epseig) go to 43
  l2 = l*nph2 + 1
  n3 = l2 - nph2
  k = (n2 - 1)*nph + 1
  zyr = 0.
  zr = 0.
  zyi = 0.
  zi = 0.
  do i=n2, n22, nph2
     zyr = zyr + zya(i)*zyb(k) - zya(i+1)*zyb(k+1)
     zyi = zyi + zya(i)*zyb(k+1) + zya(i+1)*zyb(k)
     zr = zr + zyb(i)*zyb(k) - zyb(i+1)*zyb(k+1)
     zi = zi + zyb(i)*zyb(k+1) + zyb(i+1)*zyb(k)
     cq(n3) = zyb(k)
     cq(n3+1) = zyb(k+1)
     cq(l2) = zyb(i)
     cq(l2+1) = zyb(i+1)
     k = k + 2
     n3 = n3 + 2
411  l2 = l2 + 2
  end do
  l2 = l + l
  g(l2-1) = zyr
  g(l2) = zyi
  dx = zr*zr + zi*zi
  zyr = (zr*g(l2-1) + zi*g(l2))/dx
  zyi = (zr*g(l2) - zi*g(l2-1))/dx
  k = 1
  n2 = l*nph2 + 1
  n1 = n2 - nph2
  n3 = n2 - 2
  n4 = n3 + nph2
  do i=n2, n4, 2
     zr = cq(i)*zyr - cq(i+1)*zyi
     zi = cq(i)*zyi + cq(i+1)*zyr
     do j=n1, n3, 2
        zyb(k) = zya(k) - cq(j)*zr + cq(j+1)*zi
        zyb(k+1) = zya(k+1) - cq(j)*zi - cq(j+1)*zr
        zya(k) = zyb(k)
        zya(k+1) = zyb(k+1)
412     k = k + 2
     end do
413 end do
  go to 42
414 diagx = 0.
  j = 1
  do i=1, nph2, 2
     dx = zya(j)*zya(j) + zya(j+1)*zya(j+1)
     if (dx .le. diagx) go to 415
     diagx = dx
     n1 = j
     n2 = i
415  j= j + nphpi2
  end do
  n3 = (l-1)*nph2 + 1
  k = (n2 - 1)*nph + 1
  zyr = 0.
  zyi = 0.
  zr = zya(n1)/diagx
  zi = -zya(n1+1)/diagx
  do i=n2, n22, nph2
     zyr = zyr + zya(i)*zya(k) - zya(i+1)*zya(k+1)
     zyi = zyi + zya(i)*zya(k+1) + zya(i+1)*zya(k)
     cq(n3) = zr*zya(k) - zya(k+1)*zi
     cq(n3+1) = zi*zya(k) + zya(k+1)*zr
     n3 = n3 + 2
416  k = k + 2
  end do
  l2 = l + l
  g(l2-1) = zr*zyr - zi*zyi
  g(l2) = zr*zyi + zi*zyr
  if (iprsup .lt. 5) go to 418
  write (lunit6,135) (g(i),i=1,nph2)
135 format ( /, 5x,  31hraw eigenvalues % vector output  ,/, ( /,  10e12.5 ) )
  do i=1, n22, nph2
     j = i + nph2 - 1
417  write (lunit6,107) (cq(k),k=i,j)
  end do
107 format ( 1h0, 5x,  6(5x, e12.5) ,/, 6x,  6(5x, e12.5 ) )
418 continue
  dx = w/spdlt
  do i=1, nph2, 2
     g(i) = g(i) + 1.0
     zyr = sqrtz(g(i)*g(i) + g(i+1)*g(i+1))
     if (g(i) .lt. 0.0)   go to 200
     zr = sqrtz((g(i) + zyr)/2.)
     zi = g(i+1) / zr / 2.
     go to 201
200  zi = sqrtz((-g(i) +zyr) / 2.)
     if (g(i+1) .lt. 0.0) zi = -zi
     zr = g(i+1) / zi /2.
201  g(i) = - dx * zi
     g(i+1) = dx*zr
16 end do
  !     calculation of current transformation matrix
  call cxc(cq,zyb,zya,ll6)
  if ( kill  .gt.  0 )   go to 9200
  n1 = 1
  do i=1, nph2, 2
     k = n1
     do j=i, n22, nph2
        zya(k) = zyb(j)
        zya(k+1) = zyb(j+1)
55      k = k + 2
     end do
56   n1 = n1 + nph2
  end do
  iwork = iwork / 2
  iout1 = ioutp
  ioutp = ioutp / 2
  if ( ioutp + ioutp   .eq.   iout1 )   go to 18
  !     output the transformation matrices.
  if (nph .le. 6) go to 127
  write (lunit6, 120)
120 format(//,42x,21hmode to phase voltage   ,/, 42x, 21htransformation matrix ,/, 1x    )
  n1 = 5 * nph2
  do i=1, nph2, 2
     n2 = i
     write (lunit6, 121)
121  format (1x)
122  n3 = n2 + n1
     if (n3 .gt. n22) n3 = n22
     n4 = n2 + 1
     n5 = n3 + 1
     write (lunit6, 121)
     write (lunit6, 123) (cq(j), j=n2, n3, nph2)
     write (lunit6, 123) (cq(j), j=n4, n5, nph2)
     n2 = n3 + nph2
     if (n3 .lt. n22) go to 122
123  format (1x, 12(2x,f8.2))
124 end do
  write( lunit6,119 )
119 format(//, 42x,  21hmode to phase current  ,/, 42x,  21htransformation matrix  ,/,  1x  )
  do i=1, nph2, 2
     n2 = i
     write (lunit6,121)
125  n3 = n2 + n1
     if (n3 .gt. n22) n3 = n22
     n4 = n2 + 1
     n5 = n3 + 1
     write (lunit6, 121)
     write (lunit6, 123) (zya(j), j=n2, n3, nph2)
     write (lunit6, 123) (zya(j), j=n4, n5, nph2)
     n2 = n3 + nph2
     if (n3 .lt. n22) go to 125
126 end do
127 write (lunit6, 141 )
141 format ( /,  22x,  21hmode to phase voltage,  44x, 21hmode to phase current,  /,  22x,  21htransformation matrix, &
       44x,  21htransformation matrix,  /,  1x  )
  do i=1, nph2, 2
     write (lunit6, 116)
116  format ( 65x, 1h*  )
     write (lunit6, 109)  ( cq(j), j=i, n22, nph2 )
109  format ( 1x,  6(2x, f8.5)  )
     write (lunit6, 110)  ( zya(j), j=i, n22, nph2 )
110  format ( 1h+, 64x, 1h*, 1x, 6(2x, f8.5)  )
     k = i + 1
     write (lunit6, 109)  ( cq(j), j=k, n22, nph2 )
17   write (lunit6, 110)  ( zya(j), j=k, n22, nph2 )
  end do
  !     output eigenvectors, attenuation, and velocity
18 iout1 = ioutp
  ioutp = ioutp / 2
  if ( ioutp + ioutp   .eq.   iout1 )   go to 21
  if (kreqab .eq. 1)   go to 39138
  write (lunit6, 138)
138 format ( /, 20x,  5halpha,  13x,  4hbeta,  9x, 11hattenuation,  8x,  8hvelocity,  /,  9x,  4hmode,  5x, &
       10h(neper/mi),  6x,  12h(radians/mi),  7x,  7h(db/mi), 9x,  10h(mile/sec)    ,/,  1x  )
  go to 39168
39138 write (lunit6, 39148)
39148 format ( /, 20x,  5halpha,  13x,  4hbeta,  9x, 11hattenuation,  8x,  8hvelocity,  /,  9x,  4hmode,  5x, &
       10h(neper/m ),  6x,  12h(radians/m ),  7x,  7h(db/m ), 9x, 10h  (m/sec)     ,/, 1x)
39168 do i = 1, nph
     j = i + i
     zyr = conv5 * g(j-1)
     zyi = w / g(j)
20   write (lunit6, 115)  i, g(j-1), g(j), zyr, zyi
  end do
115 format ( 10x, i2,  4(5x, e12.5)  )
21 iwork = iwork / 2
  n2 = ioutp / 16
  n1 = iwork / 16
  if ( 16*n1   .eq.   iwork )   go to 35
  n = 1
  do j = 1, n22, nph2
     k = j + nph2 - 1
     l = n
     do i = j, k, 2
        cqt(l) = cq(i)
        cqt(l+1) = cq(i+1)
        l = l + nph2
2056 end do
     n = n + 2
2156 end do
  call cxc(z,zya,zyd,ll1)
  call cxc(y,cq,zyc,ll1)
  call cxc(zyb,zyd,z,ll1)
  call cxc(cqt,zyc,y,ll1)
  j = 1
  do i = 1, nph
     y(j) = 0.0
19121 j = j + nphpi2
  end do
  if ( iss  .eq.  0 )   go to 23
  do i=1, nph2
1976 g60(i) = g(i)
  end do
  j = 1
  do i=1, n22, nphpi2
     pbuf(j) = sll*z(i)
     pbuf(j+1) = sll*z(i+1)
     pbuf(j+2) = sll*y(i)
     pbuf(j+3) = sll * y(i+1)/tenm6
22   j= j + 14
  end do
23 i = iwork / 2
  n1 = i - 8 * (i/8)
  i = i / 2
  n2 = i - 4 * ( i / 4 )
  i = 2 * ( ioutp / 2 )
  n3 = ioutp - i
  j = 4 * ( i / 4 )
  n4 = i - j
  n5 = j - 8 * ( j / 8 )
  i1 = n3 + n4 + n5
  if (i1 .eq. 0)   go to 49500
  if (kreqab .eq. 1)   go to 49410
  write (lunit6, 137)
137 format (/, 9x,  18hdistributed series,  7x,  17hdistributed shunt ,10x,  14hcharacteristic,  8x,  20hequivalent pi series, &
       5x,  19hhalf equiv pi shunt,  /,  9x, 19himpedance(ohm/mile),  5x,  20hadmittance(mho/mile), 8x,  15himpedance(ohms),  &
       9x,  15himpedance(ohms), 10x,  16hadmittance(mhos),  /,  5h mode, 5(5x, 4hreal, 8x, 4himag, 4x)    ,/,  1x  )
  go to 49500
49410 write (lunit6, 49420)
49420 format (/, 9x,  18hdistributed series,  7x,  17hdistributed shunt ,10x,  14hcharacteristic,  8x,  20hequivalent pi series, &
       5x,  19hhalf equiv pi shunt,  /,  9x, 19h impedance(ohm/m)  , 5x, 20h admittance(mho/m), 8x,  15himpedance(ohms),  9x,  &
       15himpedance(ohms), 10x,  16hadmittance(mhos),  /,  5h mode, 5(5x, 4hreal, 8x, 4himag, 4x)    ,/,  1x  )
49500 j = 1
  call mover0 ( zyb, n22 )
  call mover0 ( zyc, n22 )
  do i=1, nph
     if (i1 .ne. 0) write (lunit6,111) i
111  format ( 3x, i1 )
     if ( n3  .ne.  0 ) write (lunit6, 112 )  z(j), z(j+1), y(j), y(j+1)
112  format ( 1h+, 5x, 2e12.5, 1x, 2e12.5 )
     if ( n1  .eq.  0 )   go to 24
     l = 2 * i - 1
     zr = g(l+1)/y(j+1)
     zi = - g(l)/y(j+1)
205  k = i + i
     dx  =  zr * zr  +  zi * zi
     yo(k-1) = zr/dx
     yo(k) = -zi/dx
     if ( n4  .ne.  0 ) write (lunit6, 113 )  zr, zi
113  format ( 1h+, 55x, 2e12.5 )
     if ( n2  .eq.  0 )   go to 24
     d1 = z(j)*y(j)  -  z(j+1)*y(j+1)
     d2 = z(j)*y(j+1)  +  z(j+1)*y(j)
     dx = sqrtz ( d1*d1  +  d2*d2 )
     if (d1 .lt. 0.0)   go to 202
     d3 = sqrtz((d1 + dx) / 2.)
     d4 = d2 / d3 / 2.
     go to 203
202  d4 = sqrtz((-d1 +dx) / 2.)
     if ( d2  .lt.  0.0 )     d4 = -d4
     d3 = d2 / d4 /2.
203  d3 = sll * d3
     d4 = sll * d4
     dx = expz ( d3 )
     d3 = dx * cosz(d4 )
     d4 = dx * sinz(d4 )
     dx = d3*d3 + d4*d4
     d1 = ( d3 - d3/dx ) / 2.0
     d2 = ( d4 + d4/dx ) / 2.0
     zyb(j) = d1*zr - d2*zi
     zyb(j+1) = d1*zi + zr*d2
     diag = d3 - 1.0
     d1 = diag*diag - d4*d4
     d2 = 2.0 * diag * d4
     dx = 2.0 * dx
     zyr  =  ( d1*d3 + d2*d4 ) / dx
     zyi = ( d2*d3 - d1*d4 ) / dx
     dx  =  zyb(j) * zyb(j)  +  zyb(j+1) * zyb(j+1)
     zyc(j)  =  ( zyr*zyb(j)  +  zyi * zyb(j+1) ) / dx
     zyc(j+1)  =  ( zyi * zyb(j)  -  zyr * zyb(j+1) ) / dx
     if ( n5  .ne.  0 ) write (lunit6, 114)  zyb(j), zyb(j+1), zyc(j), zyc(j+1)
114  format ( 1h+, 80x, 2e12.5, 1x, 2e12.5 )
24   j = j + nphpi2
  end do
  iwork = iwork / 8
  n2 = ioutp / 16
  n1 = iwork / 2
  if ( n1+n1  .eq.  iwork )   go to 35
  call cxc(cq,zyb,y,ll4)
  call cxc(y,cq,z,ll3)
  call cxc(zya,zyc,zyb,ll4)
  call cxc(zyb,zya,y,ll3)
  ioutp = ioutp / 8
  iout1 = ioutp
  ioutp = ioutp / 2
  if ( ioutp + ioutp   .eq.   iout1 )   go to 13
  write (lunit6, 132 )
132 format ( /, 9x,  20hpi equivalent series, 23x, 19hhalf pi equiv shunt   ,/,  8x,  22himpedance matrix(ohms),  20x, 23hadmittance matrix(mhos)   )
27 if (nph .le. 6) go to 157
  nphpi = nph + 1
  n1 = 5 * nph2
  do i=1, nph2, 2
     n2 = i
     n3 = (i - 1) * nphpi + 1
     write (lunit6, 121)
151  n4 = n2 + n1
     if (n4 .lt. n3) n4 = n3
     write (lunit6,152) (z(j), j=n2, n4, nph2)
     n5 = n2 + 1
     n6 = n4 + 1
     write (lunit6, 153) (z(j), j=n5, n6, nph2)
     n2 = n4 + nph2
     if ( n4 .lt. n3) go to 151
152  format(/8(3x,e12.5))
153  format( 8(3x,e12.5))
154 end do
  write (lunit6, 121)
  do i=1, nph2, 2
     n2 = i
     n3 = (i - 1) * nphpi + 1
     write (lunit6, 121)
155  n4 = n2 + n1
     write (lunit6, 152) (y(j), j=n2, n4, nph2)
     n5 = n2 + 1
     n6 = n4 + 1
     write (lunit6, 153) (y(j), j=n5, n4, nph2)
     n2 = n4 + nph2
     if (n4 .lt. n3) go to 155
156 end do
  go to 13
157 n2 = 1
  n3 = 1
  do n1=1, nph
     write (lunit6, 108 )
108  format ( 1x )
4    go to (5, 6, 7, 8, 9, 10 ), n1
5    write (lunit6, 101)   ( z(i), i=n2, n3, nph2 ), ( y(i), i=n3, n22, nph2 )
101  format ( 1(3x, e12.5), 25x, 6(3x, e12.5)  )
     go to 11
6    write (lunit6, 102)   ( z(i), i=n2, n3, nph2 ), ( y(i), i=n3, n22, nph2 )
102  format ( 2(3x, e12.5), 25x, 5(3x, e12.5)  )
     go to 11
7    write (lunit6, 103)   ( z(i), i=n2, n3, nph2 ), ( y(i), i=n3, n22, nph2 )
103  format ( 3(3x, e12.5), 25x, 4(3x, e12.5)  )
     go to 11
8    write (lunit6, 104)   ( z(i), i=n2, n3, nph2 ), ( y(i), i=n3, n22, nph2 )
104  format ( 4(3x, e12.5), 25x, 3(3x, e12.5)  )
     go to 11
9    write (lunit6, 105)   ( z(i), i=n2, n3, nph2 ), ( y(i), i=n3, n22, nph2 )
105  format ( 5(3x, e12.5), 25x, 2(3x, e12.5)  )
     go to 11
10   write (lunit6, 106)   ( z(i), i=n2, n3, nph2 ), ( y(i), i=n3, n22, nph2 )
106  format ( 6(3x, e12.5), 25x, 1(3x, e12.5)  )
11   n2 = n2 + 1
     n3 = n3 + 1
     if ( n2  .le.  n1+n1 )   go to 4
12   n3 = n3 + nph2
  end do
13 if ( iwork  .eq.  0 )   return
  if ( iwork  .gt.  0 )   go to 31
  iwork = -iwork
  call cxc(z,y,zy,ll1)
  if ( iprsup  .lt.  5 )   go to 29
  write (lunit6, 133 )
133 format ( /, 5x,   22htranspose of zy matrix    ,/,  1x  )
  do i=1, n22, nph2
     j = i + nph2 - 1
28   write (lunit6, 107)  ( zy(k),k=i, j)
  end do
29 go to 30
31 call mover0 ( cq(1), n22 )
  call mover0 ( zya(1), n22 )
  i = 1
  do j=1, n22, 2
     cq(j) = q(i)
     zya(j) = qi(i)
32   i = i + 1
  end do
  call cxc(zya,zy,zyb,ll5)
  call cxc(zyb,cq,zyc,ll2)
  i = 1
  do j=1, nph2, 2
     dx = sqrtz( zyc(i) * zyc(i) + zyc(i+1) * zyc(i+1) )
     if (zyc(i) .lt. 0.0)   go to 206
     g(j) = sqrtz ( ( zyc(i) + dx ) / 2. )
     g(j+1) = zyc(i+1)/g(j)/2.
     go to 33
206  g(j+1) = sqrtz((-zyc(i) + dx) /2.)
     if ( zyc(i+1) .lt. 0. ) g(j+1) = - g(j+1)
     g(j) = zyc(i+1)/g(j+1)/2.
33   i = i + nphpi2
  end do
  if (ioutp .ne. 0) write (lunit6, 140 )
140 format ( /,  10x,   77hthe following values are computed by use of constant transformation matrices.   )
  call cxc(cq,zyb,zya,ll6)
  if (nss .eq. 1)   go to 19763
  l = 1
  do i=1, nph
     j = (i-1) * nph2 + 1
     k = 2 * i - 1
     do n1=1, i
        z(j) = xr(l)
        z(k) = xr(l)
        y(j) = xg(l)
        y(k) = xg(l)
        z(j+1) = xl(l)
        z(k+1) = z(j+1)
        y(j+1) = xc(l)
        y(k+1) = y(j+1)
        j = j + 2
        k = k + nph2
62      l = l + 1
     end do
63 end do
19763 if (iss .eq. 0)   go to 18
  do i=1, nph2
57   g60(i) = g(i)
  end do
  go to 18
35 iwork = n1
  ioutp = n2
  go to 13
9200 return
end subroutine frqdom
!
! subroutine xift.
!
function xift( tsp, omegas, funw, cosi )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl45.ftn'
  include 'blkcom.ftn'
  !  this routine calculates the inverse fourier transform of a geometric
  !     series of frequencies in omegas  with the points in the frequency
  !     domain in funw(nfr1).  linear interpolation between the frequency
  !     sequence is used in order to perform the numerical integration.
  !     note.  the first point in 'funw' corresponds to zero prequency.
  !     this value, of course, is not multiplied by (1/(j*w)) to convert
  !     from impulse to step response.  the limit of sin(w*t)/w for w = 0.
  !     being equal to t is used.
  dimension omegas(1), funw(1)
  double precision xtmp, ttft, time, dtemp, cosi(1)
  equivalence (indtv(10), npan)
  data n1 / 3 /
  time = tsp
  if (npan .gt. 0) go to 40004
  ttft = time * time
  do j = 1, nfr
     xtmp = time * omegas( j+1 )
     nw =  xtmp / twopi
     xtmp =  xtmp - nw * twopi
40002 call dsinz( xtmp, cosi(j) )
  end do
  xtmp = omegas(2) * time
  call dcosz( xtmp, dtemp )
  xtmp = xtmp * onehaf * ( funw(1) * ttft + funw(2) * time * ( 2. * dtemp / xtmp + cosi(1) ) ) - ratio / dmin * funw(2) * ( cosi(2) - cosi(1) ) / omegas(3)
  do j = 3, nfr
     xtmp = xtmp + funw(j) * ( dplu * cosi(j-1) - ratio * cosi(j-2) - cosi(j) ) / ( dmin * omegas(j) )
40003 end do
  xift = pi2 * xtmp / ttft
  return
40004 if (iprsup .eq. 0) n1 = 0
  xift = 0.0
  w  = 0.0
  d4 = funw(1)
  d3 = omegas(1)
  d2 = funw(2)
  d1 = omegas(2)
  d7 = d2 * d1
  d5 = (d7 - d4) / (d1 - d3)
  d6 = d4 - d5 * d3
  d7 = onehaf * time * d4
  deltaw = (omegas(2) - omegas(1)) / npan
  do i=1, npan
     xift = xift + d7
     w = w + deltaw
     xtmp = w * time
     nw = xtmp / twopi
     d7 = xtmp - nw * twopi
5    d7 = ( d5 * w + d6 ) * sinz( d7 ) / w
  end do
  d7 = onehaf * d7
  xift = deltaw * (xift + d7)
  d4 = (omegas(3) - omegas(2)) / npan
  deltaw = xpan / absz(tsp)
  i = 1
  if (n1 .gt. 0) write (lunit6,100) npan, xpan, pi2, deltaw, time, xift, d1, d2, d3, d4, d5, d6, d7, (omegas(i), funw(i), i=1,5)
100 format(/,51h before integration loop.  npan, xpan, pi2, deltaw,  ,60htime, xift, d1, d2, d3, d4, d5, d6, d7, (omegas(i), funw(i), &
       11h i=1, 5) =   ,/,i10,6(1x,e20.10)/(6(1x,e20.10)))
  if (deltaw .le. d4) go to 40
  do i=2, nfr
     d3 = 0.
     d5 = (funw(i+1) - d2) / (omegas(i+1) - d1)
     d6 = d2 - d5*d1
     d1 = omegas(i+1)
     d2 = funw(i+1)
     do k=1, npan
        d3 = d3 + d7
        w = w + d4
        xtmp = w * time
        nw = xtmp / twopi
        d7 = xtmp - nw * twopi
10      d7 = ( d5 * w + d6 ) * sinz( d7 )
     end do
     d7 = onehaf * d7
     xift = xift + d4 * (d3 + d7)
     if (n1 .gt. 0) write (lunit6, 200) i, w, deltaw, omegas(i), funw(i), xift, d1, d2, d3, d4, d5, d6, d7
200  format(/,52h inside ift loop.  i, w, deltaw, omegas(i), funw(i), , 36hxift, d1, d2, d3, d4, d5, d6, d7 =    ,/, &
          i10,6(1x,e20.10)/(6(1x,e20.10)))
     d4 = (omegas(i+2) - d1) / npan
     if (d4 .gt. deltaw) go to 40
30 end do
  go to 70
40 i = i + 1
  d3 = d7
  do j=i, nfr
     d5 = (funw(j+1) - d2) / (omegas(j+1) - d1)
     d6 = d2 - d5 * d1
     d1 = omegas(j+1)
     d2 = funw(j+1)
     if (n1 .gt. 0) write (lunit6, 200) i, w, deltaw, omegas(i), funw(i), xift, d1, d2, d3, d4, d5, d6, d7
50   if (w .gt. d1) go to 60
     d3 = d3 + d7
     w = w + deltaw
     xtmp = w * time
     nw = xtmp / twopi
     d7 = xtmp - nw * twopi
     d7 = ( d5 * w + d6 ) * sinz( d7 )
     go to 50
60 end do
  xift = xift + deltaw * (d3 - onehaf * d7)
70 xift = pi2 * xift
  if (n1 .gt. 0) write (lunit6,300) time, xift
300 format (/,24h  ift of funw(w) at t = ,e20.10,4h is ,e20.10)
  if (n1 .gt. 0) n1 = n1 - 1
  return
end function xift
!
! subroutine rise.
!
subroutine rise(time, thr, t2, vresp, si, cosi)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     this routine is used to estimate the time at which the step
  !     response in the time domain is at value 'thr'(0.0 to 1.0).
  !     the initial guess is in 'time' when the  'rise' function is called
  !     and the final approximation is returned thru the same argument.
  !     the principle of operation is linear interpolation/extrapolation
  !     to find a new time closer to 'thr'.
  !
  include 'blkcom.ftn'
  include 'labl45.ftn'
  include 'volt45.ftn'
  dimension si(1), vresp(1)
  double precision cosi(1)
  equivalence (ipntv(1), nfit),  (vim(5), epsrse)
  n1 = nfit
  temp = time
  mitt = 0
7403 x1 = xift ( time, si, vresp, cosi )
  target = ffin * absz(thr)
  close = ffin * epsrse
15 t1 = time + t2
  if( x1 .gt. target  ) t1 = time - t2
  if ( t1 .le. 0.)  t1 = time / 10.
  x2 = xift(t1, si, vresp, cosi)
  if ( iprsup .ge. 4) write (lunit6, 1)
1 format ( /, 10x,2hx2,13x,2ht1,13x,2hx1,12x,4htime)
  if ( iprsup .ge. 4) write (lunit6, 2) x2, t1, x1, time
2 format ( 4e15.5)
  if ( (x2-x1)/(t1-time)  .lt.  0. )   go to 30
  if( x1 .ge. target .and. x2 .le. target ) go to 5
  if( x1 .le. target .and. x2 .ge. target ) go to 5
30 time = t1
  x1 = x2
  if( time .lt. 0. .or. time .gt. 2. * tt ) go to 25
  go to 15
25 if ( mitt .gt. 3 .or. ictrl .ge. 0 ) go to 26
  thr = thr * onehaf
  time = temp
  mitt = mitt + 1
  go to 7403
26 kill = 156
  lstat(11) = nfit
  lstat(12) = 1
  return
5 t2 = ( time + t1 ) / 2.
  x3 = xift( t2, si, vresp, cosi )
  if ( iprsup .gt. 3 ) write (lunit6,10) n1, x1, x2, x3, time, t1, t2
10 format ( /, 41h in rise.  n1, x1, x2, x3, time, t1, t2 = ,/,  i10,  6e20.5 )
  if ( absz(x3 - target)  .gt.  close ) go to 40
  time = t2
  return
40 if ( x2 .gt. target  .and.  x3 .lt. target )   go to 41
  if ( x2 .lt. target  .and.  x3 .gt. target )   go to 41
  t1 = t2
  x2 = x3
  n1 = n1 - 1
  go to 42
41 time = t2
  x1 = x3
  n1 = n1 - 1
42 if ( n1 .ge. nfit / 2 )   go to 5
43 t2 = time + ( t1 - time ) * ( target - x1 ) / ( x2 - x1 )
  x3 = xift( t2, si, vresp, cosi )
  if ( iprsup .gt. 3 ) write (lunit6, 10)  n1, x1, x2, x3, time, t1, t2
  if ( x2 .gt. target .and. x3 .lt. target )   go to 20
  if ( x2 .lt. target .and. x3 .gt. target )   go to 20
  t1 = time
  x2 = x1
20 time = t2
  x1 = x3
  n1 = n1 - 1
  if (absz(x1 - target) .lt. close) return
  if ( n1 .gt. 0 )   go to 43
  kill = 156
  lstat(11) =  nfit
  lstat(12) = nfit - n1 + 1
  return
end subroutine rise
!
!     subroutine tdfit.
!
subroutine tdfit(vresp, si, fv, hhm, hhn, cosi)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     This routine calculates the time domain sequence of points
  !     representing the step response in 'fv' by inverse fourier trans-
  !     formation of 'vresp'.  This sequence is then fitted by a least-
  !     squares newton-raphson algorithm to the function
  !
  !     z(t) = d*(1.0 - exp(-(t - x(3))/x(1))) + (ffin - d) *
  !                          (1.0 -exp(-(t-x(3))/x(2)))
  !
  include 'blkcom.ftn'
  include 'labl45.ftn'
  include 'volt45.ftn'
  equivalence (vim(1),eps),(vim(2),eps1),(vim(3),fit2z)
  equivalence (vim(4),pivthr),(vim(7),ft2emx),(vim(9),epspv2)
  equivalence (ipntv(2),niter1),(ipntv(3),niter),(indtv(1),npoint)
  dimension vresp(1), si(1), fv(1), hhm(1), hhn(1)
  double precision cosi(1)
  dimension b(4), db(3,3), hac(3,3), e(4), eold(3), xold(3)
  character*8 text7, text8, text9
  data text7  / 6h.      /
  data text8  / 6h0      /
  data text9  / 6h*      /
  ktdiv = 0
  ktzer = 0
  kcrit = npoint * 0.8
40066 no = tt / tstep + onehaf
  kmax = 0
  tstep = tt / no
  kzero = 0
  !     calculate (by ift) step response in time domain
  if (iprsup .gt. 3) write (lunit6, 70560)
70560 format(/,5x,13houtput of ift,/,9x,1ht,11x,5hfv(k),/,1x)
  t = tstep
  if( ictrl .lt. 0 ) t = 5. * tstep
  fvmx = - fltinf
  fvmin = ffin/1000.
  kmp = 1
40063 do k = kmp, npoint
     fv(k) = xift(t, si, vresp, cosi)
     if ( fv(k) .le. fvmx ) go to 40065
     fvmx = fv(k)
     kmax = k
40065 if ( fv(k) .ge. fvmin )  go to 40064
     kzero = k
     fvmx = -fltinf
40064 if ( iprsup .gt. 3 )   write (lunit6, 70510)  t, fv(k)
     t = t + tstep
40050 end do
  if( ictrl .lt. 0 ) go to 40061
  if ( kzero  .le.  0 )   go to 40061
  ktzer = ktzer + 1
  if ( ktzer .le. 5 ) go to 40069
  write ( lunit6, 40070 )
40070 format (' Trouble in tdfit - - portions of ift results have been discarded more than five times ', /, &
       ' in search for positive data. ', /, ' Temporary error stop in "tdfit".')
  call stoptp
40069 km = npoint - kzero
  kmax = kmax - kzero
  do k = 1, km
     kp = k + kzero
40062 fv(k) = fv(kp)
  end do
  kmp = km + 1
  tstrt = tstrt  +  kzero * tstep
  kzero = 0
  go to 40063
40061 if ( ictrl .ge. 0 .or. kmax .ge. kcrit ) go to 40067
  tt = kmax * tstep / 2.
  tstep = tt / no
  ktdiv = ktdiv + 1
  if ( ktdiv .le. 5 ) go to 40066
  write ( lunit6, 40068 )
40068 format (' Trouble in tdfit - - time span has been reduced more than five times  ', /, &
       ' in search of legitimate ift results. ', /,  '   Temporary error stop in "tdfit"'.    )
  call stoptp
40067 continue
  if (ictrl .eq. -10) go to 40500
  !     this routine fits two exponentials with an artificial time delay
  !     to the line prop or adm step response using a least square error
  !     minimizing technique solved by a newton-raphson algorithm.
  !     list of variables.
  !        fv -    vector containing numerical values of the curve
  !               to be fitted
  !        nvar - number of variables to be determined by newton-raphson
  !               algorithm. the subroutine fixes by itself the value
  !               of "nvar" to 3 for propagation and 2 for admittance.
  !        x - vector containing parameters of the exponential fitting 'z'
  !            in the form'
  !
  !     z(t) = d * (1.0 - exp(-(t - x(3)) / x(1))) + (ffin - d) *
  !           (1.0-exp(-(t-x(3))/x(2)))
  !
  !             where'
  !        ffin - final value for the curve 'y'
  !        x(3) - artifical time delay  (equal to zero for admittance)
  !        e - right hand side of system of equations to be solved
  !            for increment 'dx' by a newton raphson algorithm.
  !            after solving the solution 'dx' will be stored in e.
  !        jac - jacobian matrix.
  !        b,db - auxiliary matrices for calculating jacobian 'jac'.
  !        eps - the least square error desired to be attained
  !              in the fitting rocess.
  !        eps1 - the fitting is stopped if the relative increment 'dx/x'
  !               is less than eps1
  !        npoint - number of known points for the step response
  !                 to be used for fitting.
  !        niter - maximum number of iterations allowed.
  jgrad=0
  iter=0
  nvar=3
  error = 100.
  db(1,2)=0.
  !     calculate initial guess for 'x'.
  temp = ffin * fit2z
  if ( ictrl .lt. 0 ) temp = temp * 0.1
  do i=1, npoint
     if (fv(i) .lt. temp) go to 60050
     d1 = i - 1
     x(3)=  d1 / no
     go to 60051
60050 end do
  kill = 157
  lstat(14) = i1
  lstat(15) = npoint
  flstat(14) = fv(npoint)
  flstat(15) = temp
4521 lstat(19) = 4521
  go to 9200
60051 do k=i, npoint
     if (k .eq. 1) go to 60053
     y1 = fv(k+1) - fv(k)
     y2 = fv(k) - fv(k-1)
     if (y1 .gt. y2) go to 60052
     ncount=k
     go to 60054
60053 if (fv(k+1) - fv(k) .gt. fv(k)) go to 60052
     ncount=k
     go to 60054
60052 end do
  kill = 157
  lstat(14) = 0
  lstat(15) = npoint
  lstat(16) = i
  lstat(17) = k
  flstat(14) = fv(k-1)
  flstat(15) = fv(k)
  flstat(16) = fv(k+1)
4533 lstat(19) = 4533
  go to 9200
60054 ntrd = npoint / 3
  dy = ( fv(npoint) - fv(npoint - ntrd) ) * no / ntrd
  if (dy .le. 0.) dy = pivthr
  x(2) = dy/(ffin - fv(npoint))
  x(2)=absz(x(2))
  if ( ncount .eq. 1 ) ncount = 2
  if ( ncount .eq. 1 )   ncount = 2
  dy = (fv(ncount) - fv(ncount-1))*no
  x(1) = dy / fv(ncount) / 2.
  d1 = ncount - onehaf
  x(3) =  d1 / no   -  onehaf *(fv(ncount)+fv(ncount-1))/dy
  x1=x(1)
  if (ictrl .lt. 0) go to 60055
  write(lunit6,60200)
60200 format ( /,  2x,  8hno.iter.,  8x,  5hampl.,  13x,  4hx(1), 13x,  4hx(2),  13x,  4hx(3),   13x,  5herror,  /,  1x  )
  go to 60012
60055 x(1) = no * fv(2) * onehaf / fvmx
  nvar=2
  x1 = x(2) / 1.2
  x(3)=0.
  write (lunit6,60400)
60400 format ( /,  2x,  8hno.iter.,  8x,  5hampl.,  13x,  4hx(1), 13x,  4hx(2),  13x,  5herror  ,/,  1x )
  !     newton-raphson iterative solution process begins here.
60012 iter = iter + 1
  iter2 = 0
  cnt = 1.0
  error1=error
60014 jgrad = jgrad + 1
  if ( jgrad .gt. 20 ) go to 62500
  if ( ictrl .ge. 0 ) x(1) = x1 * ( 1. + fit2z * ( jgrad - 1 ) )
  if ( ictrl .lt. 0 ) x(2) = x1 * ( 1. + fit2z * ( jgrad - 1 ) )
62500 error = 0.
  if ( jgrad .ne. 21 ) go to 62501
  if ( ictrl .ge. 0 ) x(1) = x11
  if ( ictrl .lt. 0 ) x(2) = x11
62501 if ( x(3) .lt. 0. ) x(3) = 0.
  if (x(2) .lt. 0.) go to 60019
  if (x(1) .lt. 0.) go to 60019
  dd=0.
  if (iprsup .ge. 4) write (lunit6,80002) iter,x(1),x(2),x(3)
80002 format (8h  calc d,5x,i5,3(5x,e20.10 ))
  d=0.
  do j=1, nvar
     e(j)=0.
     do k=1, nvar
60003   hac(j,k) = 0.
     end do
  end do
  !     calculation of the first exponential amplitude 'd'.
  nn2 = x(3)*no + 1
  do i=nn2, npoint
     ppi = i
     pno = no
     t2 = ppi / pno
     ddt = t2 -x(3)
     hhm(i)=x(1)*ddt
     if(hhm(i).gt.80.)hhm(i)=80.
     hhm(i)=expz(-hhm(i))
     hhn(i)=x(2)*ddt
     if(hhn(i).gt.80.)hhn(i)=80.
     hhn(i)=expz(-hhn(i))
     d = d + (hhm(i) - hhn(i))*(ffin*(1.-hhn(i))-fv(i))
     dd=dd+(hhm(i)-hhn(i))**2
60001 end do
  d=d/dd
  !     calculation of the fitted curve 'z' and square error 'error'.
  dd = ffin - d
  do i=nn2, npoint
     zp = ffin - d*hhm(i) - dd*hhn(i)
     a = zp - fv(i)
     error=error+a*a
     if ( jgrad .le. 20 ) go to 60016
     d1 = i
     t2 =  d1 / no
     ddt = t2 - x(3)
     b(1)=ddt*hhm(i)*d
     b(2) = ddt*hhn(i)*dd
     b(3) = -hhm(i)*d*x(1) - hhn(i)*dd*x(2)
     db(1,1)=-ddt*ddt*hhm(i)*d
     db(1,3)=-hhm(i)*(1.-x(1)*ddt)*d
     db(2,2) = -ddt*ddt*hhn(i)*dd
     db(2,3) = -hhn(i)*(1.0 - x(2)*ddt)*dd
     db(3,3) = -x(1)*x(1)*d*hhm(i) - x(2)*x(2)*dd*hhn(i)
     do j=1, nvar
        do k=1, j
60002      db(j,k) = db(k,j)
        end do
     end do
     !     form the jacobian matrix in 'hac'
     do j=1, nvar
        e(j)=e(j)-a*b(j)
        do k=1, nvar
           hac(j,k)=hac(j,k)+b(j)*b(k)+a*db(j,k)
60017   end do
     end do
60016 end do
  error = error / (npoint * ffin * ffin)
  write(lunit6,60300) iter,d,(x(j),j=1,nvar),error
60300 format(4x,i2,4x,5(5x ,e12.5))
  if ( jgrad .gt. 20 .and. error .gt. error1 ) go to 60019
  if ( error .ge. error1 ) go to 60306
  error1 = error
  x11 = x(1)
  if ( ictrl .lt. 0 ) x11 = x(2)
60306 if ( jgrad .le. 20 ) go to 60014
  if ( iter .lt. niter ) go to 60302
  write ( lunit6,60301 )niter
60301 format( 26h reached max itterations = , i3 )
  go to 60010
60302 if ( error .ge. eps ) go to 60304
  write ( lunit6,60303 )
60303 format ( 23h successful convergence )
  go to 60010
  !     solve the system of equations for incrementing variables 'x'.
60304 if ( iprsup .ge. 4 )
1 write (lunit6,80003) ((hac(i,j),j=i,3),e(i),i=1,3)
80003 format (9(2x,e12.5))
  do i=1, nvar
     ipi = i + 1
     if (absz(hac(i,i)).ge. epspv2)   go to 50004
     if (ipi .gt. nvar) go to 50003
     do  j=ipi, nvar
        if (absz(hac(j,i)).lt. epspv2)   go to 50002
        do k=i, nvar
           temp = hac(j,k)
           hac(j,k) = hac(i,k)
50001      hac(i,k) = temp
        end do
        go to 50004
50002 end do
     !     kill the case if jacobian matrix is singular.
50003 kill = 156
4545 lstat(19) = 4545
     go to 9200
50004 temp = hac(i,i)
     e(i) = e(i)/temp
     if (ipi .gt. nvar) go to 50006
     do k=ipi, nvar
50005   hac(i,k) = hac(i,k)/temp
     end do
50006 do j=1, nvar
        if (j .eq. i) go to 50008
        temp = hac(j,i)
        e(j) = e(j) - temp*e(i)
        if (ipi .gt. nvar) go to 50008
        do k=ipi, nvar
50007      hac(j,k) = hac(j,k) - temp*hac(i,k)
        end do
50008 end do
50009 end do
  go to 60015
  !                           type 3 iteration
  !
  !     the sum of the square error has increased over the last iteration.
  !     recover the original increment to the x vector and use a scaled
  !     multiple of this increment to form the new x vector.
60019 cnt = cnt / 10.
  iter2 = iter2 + 1
  if ( jgrad .le. 20 ) go to 4557
  do j=1, nvar
60020 x(j) = xold(j) + cnt * xold(j) * eold(j) / absz(eold(j))
  end do
  if ( iter2 .le. niter ) go to 62500
  !     program stuck in 'type 3' loop.   kill the run.
4557 kill = 156
  lstat(19) = 4557
  go to 9200
  !     update variables 'x'
60015 do j=1, nvar
     xold(j) = x(j)
     eold(j) = e(j)
60005 x(j) = x(j) + e(j)
  end do
                          do 60013 j=1, nvar
                             if (absz(e(j)/x(j)) .gt. eps1) go to 60012
60013                        continue
                             write ( lunit6,60305 )
60305                        format ( 74h termination of itteration because relative correction
1                            s are less than eps1 )
60010                        x(3) = x(3) * tt + tstrt
                             x(1)=tt/x(1)
                             if ( ictrl .lt. 0 ) ictrl = - ictrl - 1
                             d1 = w * x(3)
                             d2 = sinz(d1)
                             d1 = cosz(d1)
                             d3 = shiftr * d1 + shifti * d2
                             shifti = shiftr * d2 - shifti * d1
                             shiftr = d3
                             write (lunit6, 60406)  shiftr, shifti
60406                        format(/, 5x, 72hsteady state frequency impulse response, rotated
1                            through delay time x(3)  ,/, 5x, 72hfor use in adjusting by hand f
2                            or a precise fit at steady state frequency  ,/, 5x, 2e14.5)
                             if (ictrl .eq. 0) go to 60205
                             !     adjust fitting to agree exactly at steady state
70510                        format(8(2x,e12.5))
                             write(lunit6,60401)
60401                        format (/, 2x, 54hiterations to adjust fitting at steady state fre
1                            quency  ,/,
1 18                         x,  6hampl 1,  11x,  6hampl 2,  9x,  10htime const,  8x,
2 13                         herror at 60hz  ,/,  1x )
                             error = tt / x(2)
                             if ( error / x(1)  .lt.  10. )  go to 60205
                             x1=d
                             a = ffin - d
                             iiter=0
                             ddt = w * x(1)
                             zp = unity + ddt * ddt
                             ddt = ddt / zp
                             temp = ft2emx * sqrtz(shiftr * shiftr + shifti * shifti)
                             !     this loop adjusts d and x(2) to make the analytically computed
                             !     fourier transform of the analytical approximation agree exactly
                             !     with the data at steady state frequency.
                             c
                             !     fouriertx((d/dt)z(t)) = cexp(cmplx(0,-x(3) * w)) *
                             !          (d/cmplx(1,w / x(1)) + (ffin - d)/cmplx(1,w / x(2)))
                             if ( iprsup .ge. 3)
1                            write (lunit6, 40001) shiftr, shifti, ddt, tt, w, x(1),
2                            x(2), zp, x1, a, error
40001                        format(//,105h prior to steady state iteration in tdfit, shiftr,
1                            shifti, ddt, tt, w, x(1), x(2), zp, x1, a, error are ,/,
2                            (2x, 6e12.5) )
61002                        zr = shiftr - x1 / zp
                             zi = shifti + x1 * ddt
                             d1 = w * error
                             d2 = 1.0  +  d1 * d1
                             d3 = zr - a / d2
                             d4 = zi + a * d1/d2
                             error1 = sqrtz ( d3*d3 + d4*d4 )
                             if (error1 .lt. ft2emx)  go to 61000
                             iiter=iiter+1
                             if (iiter .lt. niter1) go to 61001
                             if ( ictrl .eq. 2 )  go to 60205
                             write(lunit6, 61006)
61006                        format( 5x, 96hwarning---iteration to adjust time domain step resp
1                            onse to fit exactly at steady state frequency  ,/, 5x, 59hhas fail
2                            ed to converge,  last steady state fitting assumed.  )
                             go to 61004
61001                        error = -zi / zr / w
                             if ( error  .le.  0.0 )   go to 60205
                             a = ( zr*zr + zi*zi ) / zr
                             x1 = ffin - a
                             if ( iprsup .ge. 3)
1                            write (lunit6, 40002) iiter, zr, zi, d1, d2, d3, d4, error1,
2                            error, a, x1
40002                        format(//,58h in the steady state iteration loop in tdfit, at iter
1                            ation,i3,49h zr, zi, d1, d2, d3, d4, error1, error, a, x1 are,
2                            /, 2x, 10e12.5)
                             write (lunit6,60300) iiter,x1,a,error,error1
                             go to 61002
61000                        write (lunit6,61003) iiter,error1
61003                        format( /, 5x, 43h steady state frequency fitting iterations=,
1                            i3, 20x,
1 21                         h60 hz fitting error =, e14.5, /, 1x )
61004                        x(2) = error
                             d=x1
                             go to 60206
60205                        a = ffin - d
                             x(2)=tt/x(2)
                             if (ictrl .eq. 0) go to 60206
                             write (lunit6,60207)
60207                        format( 5x,   100hwarning---program can not adjust time domain ste
1                            p response to fit exactly at steady state frequency.  ,/,
2 15                         x,  24hinitial fitting assumed.    )
60206                        call setplt
                             write (lunit6, 60203)
60203                        format ( ///// )
                             write( lunit6, 60201 ) d, x(1), a, x(2), x(3)
60201                        format ( //,  30x,  23hfirst amplitude      = ,  e15.6  ,/,
1 30                         x,  23hfirst time constant  = ,  e15.6  ,/,
2 30                         x,  23hsecond amplitude     = ,  e15.6  ,/,
3 30                         x,  23hsecond time constant = ,  e15.6  ,/,
4 30                         x,  23hdelay                = ,  e15.6,  /,  1x  )
                             !     output printer plot of step function and its analytical approx-
                             !     imation and calcualte the final error.
40500                        x3 = x(3) - tstrt
                             a = ffin - d
                             error1=0.
                             if (fvmx .lt. ffin) fvmx = ffin
                             write (lunit6,60405) i1,tstep,fvmx
60405                        format(1h0,5x,5hmode ,i1,5x,12htime step = ,e12.5,57h sec.(. = exp
1                            . approx., 0 = ift output, * = intersection)  ,/,
2 8                          x,  5herror,  11x,  4htime,  5x,  3h0.0,
3 80                         x,  e12.5  ,/,  33x,  2h.1,  10(9h........1)  )
                             fvmx = 90./fvmx
                             do 60500 i=1, npoint
                                t2 = i*tstep - x3
                                if (t2 .lt. 0.0) go to 60403
                                hhm(i) = t2/x(1)
                                if (hhm(i) .gt. 30.) hhm(i) = 30.
                                hhm(i) = expz(-hhm(i))
                                hhn(i) = t2/x(2)
                                if (hhn(i) .gt. 30.)   hhn(i) = 30.
                                hhn(i) = expz(-hhn(i))
                                zp = d*(1.0 - hhm(i)) + a*(1.0 - hhn(i))
                                error = zp - fv(i)
                                go to 60402
60403                           zp = 0.0
                                error = 0.0
60402                           error1 = error1 + error * error
                                j = fvmx*zp + 1.5
                                k = fvmx*fv(i) + 1.5
                                if (j .ge. 1 .and. j .le. 91) pl(j) = text7
                                if (k .ge. 1 .and. k .le. 91) pl(k) = text8
                                if (j .eq. k .and. j .ge. 1 .and. k .le. 91) pl(j) = text9
                                t2 = t2 + x(3)
                                write (lunit6,60600) error,t2,(pl(l),l=1,91)
60600                           format(5x,e12.5,3x,e12.5,2h .,91a1)
                                if (k .ge. 1 .and. k .le. 91) pl(k) = blank
                                if (j .ge. 1 .and. j .le. 91) pl(j) = blank
60500                           continue
                                error1 = error1 / (npoint * ffin * ffin)
                                write (lunit6,60204) iter, error1
60204                           format ( 2x,  13hiterations = ,  i3,  60x,
1     36                        hnormalized square error per point = ,e12.5,/,1x)
                                call setstd
9200                            return
                             end do
                             c
                             !     end of file: over45.for
                             c
