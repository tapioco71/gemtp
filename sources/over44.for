!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over44.for
!
!
!     subroutine subr44.
!
subroutine subr44
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'deck44.ftn'
  include 'labl44.ftn'
  dimension lltemp(20)
  dimension stg(1)
  equivalence (stg(1), karray(1))
  !     list-zero "karray" is always 1st, and maybe "over29":
  if ( iprsup  .ge.  1 ) write ( lunit6, 4567 )
4567 format ( 24h  "begin module subr44." )
  n8 = nchain
  if ( kburro  .eq.  1 )   n8 = 29
  call dimens ( lltemp(1), n8, trash, trash )
  do j=1, 9999, 2
     if ( lltemp(j)  .eq.  0 )   go to 5636
1003 end do
  write (lunit6, 5632)  lltemp(1), kburro, nchain
5632 format (  29h temp error stop in "subr44".,  3i8 )
  call stoptp
5636 n7 = lltemp(j+1) * nbyte(4) / nbyte(3)
  call dimens ( lltemp(1), nchain, trash, trash )
  n3 = 0
  do 5654  i=1, 9999, 2
     if ( n3  .ge.  2 )   go to 5655
     if ( lltemp(i)  .ne.  71 )   go to 5641
     lphase = lltemp(i+1)
     n3 = n3 + 1
5641 if ( lltemp(i)  .ne.  74 )   go to 5654
     lgdbd = lltemp(i+1)
     n3 = n3 + 1
5654 end do
  call stoptp
5655 lphpl1 = lphase + 1
  lphd2 = lphase / 2
  write (kunit6, 2456)  lphase
2456 format (  44h+request for line-constants supporting prog., i6   )
  ndim = lphase
  ntri = ndim * (ndim + 1) / 2
  nsqr = ndim * ndim
  nsqr2 = 2 * nsqr
  iofarr = 1
  iofxwc = iofarr + ntri
  iofxwy = iofxwc + ntri
  iofyzr = iofxwy + ntri
  iofyzi = iofyzr + nsqr
  ioftii = iofyzi + nsqr
  ioftir = ioftii + nsqr
  ioftvi = ioftir + nsqr
  ioftvr = ioftvi + nsqr
  iofer  = ioftvr + nsqr
  iofei  = iofer  + ndim
  iofthe = iofei  + ndim
  iofxtr = iofthe + ndim
  iofxti = iofxtr + ndim
  iofzsu = iofxti + ndim
  iofdum = iofzsu + ndim
  iofdur = iofdum + ndim
  ioftix = iofdur + ndim
  iofwor = ioftix + nsqr2
  n5 = iofwor + nsqr2
  if ( n5 .lt. n7 )  go to 10
  kill = 82
  lstat(19) = 10
  lstat(15) = lphase
  lstat(18) = nchain
  lastov = nchain
  nchain = 51
  go to 99999
10 call guts44( stg(iofarr), stg(iofxwc), stg(iofxwy), stg(iofyzr), stg(iofyzi), stg(ioftii), stg(ioftir),stg(ioftvi), &
        stg(ioftvr), stg(iofer), stg(iofei), stg(iofthe), stg(iofxtr), stg(iofxti), stg(iofzsu), stg(iofdum), stg(iofdur),stg(ioftix), &
        stg(iofwor), ndim, ntri, nsqr2 )
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
4568 format ( 24h  "exit  module subr44." )
99999 return
end subroutine subr44
!
!     subroutine guts44.
!
subroutine guts44(array, xwc, xwy, yzr, yzi, tii, tir, tvi, tvr, er, ei, theta2, xtir, xtii, zsurge, dummi, dummr, tixf, &
     work1, ndim, ntri,nsqr2)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'deck44.ftn'
  include 'labl44.ftn'
  include 'volt45.ftn'
  common /linemodel/ kexact, nsolve, fminsv, numrun, nphlmt
  common /linemodel/ char80, chlmfs(18)
  character*6 chlmfs        ! 9-phase as limit for lmfs test
  character*80 char80
  character*8 text1, text2, fmetrc, englis, bufsem
  character*8 text3, text4, text5, text6, text7
  character*8 text8, text9
  dimension bufsem(14), jprmat(16)
  dimension array(1)
  dimension xwc(ntri), xwy(ntri), yzi(ndim,ndim), yzr(ndim,ndim)
  dimension tii(ndim, ndim), tir(ndim, ndim)
  dimension er(ndim), ei(ndim), theta2(ndim)
  dimension xtir(ndim), xtii(ndim), zsurge(ndim), dummi(ndim)
  dimension dummr(ndim), tixf(nsqr2), work1(nsqr2)
  equivalence ( jprmat(1), j1 ),     ( jprmat(2), j2 )
  equivalence ( jprmat(3), j3 ),     ( jprmat(4), j4 )
  equivalence ( jprmat(5), j5 ),     ( jprmat(6), j6 )
  equivalence ( jprmat(7), j7 ),     ( jprmat(8), j8 )
  equivalence ( jprmat(9), j9 ),     ( jprmat(10), j10 )
  equivalence ( jprmat(11), j11 ),    ( jprmat(12), j12 )
  equivalence ( jprmat(13), j13 ),    ( jprmat(14), j14)
  equivalence ( jprmat(15), j15 ),    ( jprmat(16), j16)
  data text1  / 6hchange /
  data text2  / 6hbranch /
  data text3  / 6hfreque /
  data text4  / 6hspecia /
  data text5  / 6hl doub /
  data text6  / 6huntran /
  data text7  / 6hsposed /
  data text8  / 6htransp /
  data text9  / 6hosed   /
  data fmetrc  / 6hmetric /
  data englis   /  6henglis  /
  data nrp / 0 /
  data mrr / 0 /
  mfrqpr = 0
  if ( kexact .ne. 88333 )  go to 423
  nfqpl1 = 0
  close (unit=lunit2, status='delete' )
  open (unit=lunit2, status='scratch',form='formatted')
423 rewind  lunit9
  rewind lunit2
  rewind lunit3
  rewind lunt13
  if ( ialter  .ne.  2 )   go to 7407
  l5save = lunit5
  lunit5 = lunit2
 7407 ldisfr  =  locf( flstat(1) )     -     locf( voltbc(1) )
  metrik=0
  finpcm = unity / 2.5400d0
  ftpm = 100. * finpcm / 12.
  fmipkm = ftpm * 1000. / 5280.
  d8 = 0.3048d0
  fspac = .024d0 * twopi * alogz(d8)
  !     fspac needed for conversion of reactance at 1 meter
  !     spacing to 1 foot spacing.
  !              start of constants definition.
  valu1    =  5584596.2d0
  valu2    =  .00085646541d0
  valu3    =  .00064373888d0
  valu4    =  .61593152d0
  valu5    =  8.68588964d0
  valu6    =  .0005000000d0
  valu7 = 3.0
  valu7    =  onehaf * sqrtz( valu7 )
  valu8    =  .0040447306d0
  valu9    =  .21139217d0
  valu10   =  .5772156649015328606d0
  valu11   =  .3926991d0
  valu12   =  .398942280401433d0
  valu13   =  1.2533141373155d0
  valu14 = 2.302585093000d0
  aaa1    = .785398163397448d0
  aaa2    = .318309886183791d0
  sqrt2 = 1.4142135623730950488d0
  ccars(2)=1.3659315156584124488d0
  ll0   =   0
  ll1   =   1
  ll2   =   2
  ll3   =   3
  ll5   =   5
  ll6   =   6
  ll7   =   7
  ll8   =   8
  ll9   =   9
  ll10  =   10
  liu = 0
  nfreq = 0
  nbundl = 0
  pi = twopi * onehaf
  picon = 360. / twopi
  corchk = unity - tenm6/10.
  !              begin calculate constants for carson % bessel.
  bcars(1)=sqrt2/6.
  bcars(2)=unity/16.
  dcars(2)=bcars(2)*pi/4.
  do i=3,30
     isn=(-1) ** ((i-1)/2)
     bcars(i) =-bcars(i-2)/(i * i + 2. * i)*isn
6603 dcars(i)=bcars(i)*pi/4.
  end do
  ccars(1)=unity/sqrt2
  ccars(3)=ccars(1)
  ccars(5)=3.*sqrt2/2.
  ccars(7)=-45.*onehaf*sqrt2
  do i=1,29,2
     if ( i .gt. 8 ) ccars(i) = 0.0
64   dcars(i) = ccars(i)
  end do
  dcars(3) = -dcars(3)
  dcars(7) = -dcars(7)
  do i=4,30,2
46   ccars(i)=ccars(i-2)+unity/i + unity/(i+2.)
  end do
  fbe(1)=16.
  fbed(1)=-4.
  d1 = unity - valu10
  fke(1)=fbe(1)*d1
  do i=2, 14
     isn= (-1)**i
     fbe(i)=fbe(i-1)*(16./(i*i))*(-isn)
     fbed(i)=fbe(i)/(2.*i+2.)*isn
     d1=d1+unity/i
     fke(i)=fbe(i)*d1
811  fked(i-1) = fke(i) * i / 32.
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
  if(iprsup.ge.2)write(lunit6,333)(fbe(i),i=1,20),(fbed(i),i=1,20), (fke(i),i=1,20),(fked(i),i=1,20),(bcars(i),i=1,30), &
       (dcars(i),i=1,30),(ccars(i),i=1,30)
333 format(1x,10hdata fbe /  ,/,4(1x,5e25.15,/),/,12h data fbed / ,/ 4(1x,5e25.15,/),/,11h data fke / ,/,4(1x,5e25.15,/),/, &
         12h data fked /  ,/,4(1x,5e25.15,/),/ ,13h data bcars / ,/,6(1x,5e25.15,/),/,13h data dcars / ,/,6(1x, 5e25.15,/), &
         /,13h data ccars / ,/,6(1x,5e25.15,/),/,1x)
  if ( iprsup  .ge.  1 ) write (lunit6, 3866)  pi, picon, sqrt2
3866 format ( /,  9h at 3866  , 3e25.15  )
2 m = 1
  do i = 1, 40
13866 brname(i) = blank
  end do
  mspedb = 0
  muntrn = 0
  mtrnsp = 0
  !     segmented, 1, vax e/t can skip translation of rewind:
  if (lastov .eq. 1)  rewind lunit2
  rewind lunt13
  !     read input card using cimage
7403 call cimage
7413 continue
  read (unit = abuff, fmt = 4230) bus1
  if ( bus1 .ne. text2)  go to 37403
  !     optional "branch" card, which species  a6  branch names
  n1 = m + 11
  read (unit = abuff, fmt = 17403) (brname(i), i = m, n1)
17403 format (8x, 12a6)
  m = m + 12
  write (kunit6, 27403)
27403 format (26h+bus names for each phase.)
  go to 7403
37403 continue
  read (unit = abuff(1), fmt = 4230) bufsem
4230 format ( 13a6, a2 )
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
  bus1 = bufsem(1)
  if ( bus1 .ne. fmetrc ) goto 4258
  metrik = 1
  write (kunit6, 4252)
4252 format ( 44h+request card for metric  units on all data.    )
  go to 7403
4258 if ( bus1  .ne.  englis )   go to 24258
  metrik = 0
  write (kunit6, 14258 )
14258 format (  44h+request card for english units on all data.   )
  go to 7403
24258 if ( bus1  .ne.  text3 )   go to 34258
  write (kunit6, 44258)
44258 format ( 37h+request for frequency-loop printout.  )
  mfrqpr = 1
  go to 7403
34258 if ( bufsem(1) .ne. text4 )  go to 54258
  if ( bufsem(2) .ne. text5 )  go to 54258
  mspedb = 1
  write (kunit6, 5010)
5010 format (47h+request for special double circuit transposed.  )
  go to 7403
54258 if ( bufsem(1) .ne. text6 )  go to 5020
  if ( bufsem(2) .ne. text7 )  go to 5020
  muntrn = 1
  write (kunit6, 5015)
5015 format (40h+request for untransposed line modeling.  )
  go to 7403
5020 if ( bufsem(1) .ne. text8 )  go to 5029
  if ( bufsem(2) .ne. text9 )  go to 5029
  mtrnsp = 1
  write (kunit6,5027)
5027 format (38h+request for transposed line modeling.  )
  go to 7403
5029 do j=1, 14
     if ( bufsem(j)  .ne.  blank )   go to 4251
7436 end do
  write (kunit6, 4244 )
4244 format (  45h+blank card terminating line-constants cases.  )
  call interp
  if ( lastov  .eq.  1 )   go to 7439
  n1 = lastov
  lastov = nchain
  nchain = n1
  if ( ialter  .eq.  2 ) lunit5 = l5save
  if ( ipunch  .eq.  0 )   go to 7496
  d1 = 0.0
  write (lunit9)  d1, d1, d1
  if ( iprsup .ge. 1   .or.   lastov .eq. 39 ) write (lunit6, 14244)  d1, d1, d1, ipunch
14244 format (21h last record on unit9, 3e15.6,/, 1x, 8hipunch =,i10)
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunit9
7496 go to 9900
7439 lastov = nchain
  nchain = 51
  go to 9900
  !     segmented, 1, vax e/t can skip translation of rewind:
4251 rewind lunt13
  ik = 0
  if ( bus1  .ne.  text1 )   go to 4260
  write (kunit6, 4257)
4257 format (  37h+request card for change-case option.   )
  go to 500
4260 i = 1
  assign 4236 to moon
  go to 4164
5 i = i + 1
  !     read input card using cimage
  call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
4164 if ( kolbeg  .gt.  0 )   go to 8712
  read (unit = abuff(1), fmt = 11) itbic(i), tbtb2(i), tbr(i),itbtb3(i),tbg(i),tbd(i), tbx(i), h1, h2, d8, d9, tbtext(i), i3
11 format ( i3, f5.4, f8.5, i2, 2f8.5, 3f8.3, f8.5, f6.2, a6, i2 )
  go to 8715
8712 nfrfld = 1
  call freone ( d11 )
  itbic(i) = d11
  call frefld ( tbtb2(i) )
  call frefld ( tbr(i) )
  call freone ( d11 )
  itbtb3(i) = d11
  call frefld ( tbg(i) )
  call frefld ( tbd(i) )
  call frefld ( tbx(i) )
  call freone ( h1 )
  call freone ( h2 )
  call freone ( d8 )
  call freone ( d9 )
  nright = -1
  call freone ( d1 )
  nright = 0
  tbtext(i) = texta6(1)
  call freone ( d11 )
  i3 = d11
  if ( kill  .gt.  0 )   go to 9200
  go to 4235
8715 if ( i  .eq.  1 )   go to 4320
4280 if( itbic(i)  .ne.  0 )  go to 4285
  read (unit = abuff, fmt = 4281) bus1
4281 format ( a3 )
  if ( bus1  .ne.  blank )   go to 4285
  itbic(i) = itbic(i-1)
4285 if ( tbtb2(i)  .ne.  0.0 )  go to 4291
  read (unit = abuff, fmt = 4286) bus1
4286 format ( 3x, a5 )
  if( bus1  .ne.  blank )  go to 4291
  tbtb2(i) = tbtb2(i-1)
4291 if( tbr(i)  .ne.  0.0 )  go to 4296
  read (unit = abuff, fmt = 4292) bus1, bus2
4292 format ( 8x, 2a4 )
  if( bus1 .ne. blank )  go to 4296
  if( bus2 .ne. blank )  go to 4296
  tbr(i) = tbr(i-1)
4296 if ( itbtb3(i)  .ne.  0 )  go to 4305
  read (unit = abuff, fmt = 4297) bus1
4297 format ( 16x, a2 )
  if ( bus1  .ne.  blank )  go to 4305
  itbtb3(i) = itbtb3(i-1)
4305 if( tbg(i)  .ne.  0.0 )  go to 4312
  read (unit = abuff, fmt = 4306) bus1, bus2
4306 format ( 18x, 2a4 )
  if( bus1  .ne.  blank )  go to 4312
  if( bus2  .ne.  blank )  go to 4312
  tbg(i) = tbg(i-1)
4312 if ( tbd(i)  .ne.  0.0 )  go to 4320
  read (unit = abuff, fmt = 4313) bus1, bus2
4313 format ( 26x, 2a4 )
  if ( bus1  .ne.  blank )  go to 4320
  if ( bus2  .ne.  blank )  go to 4320
  tbd(i) = tbd(i-1)
4320 if ( h1  .ne.  0.0 )  go to 4227
  read (unit = abuff, fmt = 4321) bus1, bus2
4321 format ( 42x, 2a4 )
  if( bus1  .ne.  blank )  go to 4227
  if( bus2  .ne.  blank )  go to 4227
  h1 = h2
4227 if ( h2  .ne.  0.0 )  go to 4235
  read (unit = abuff, fmt = 4228) bus1, bus2
4228 format ( 50x, 2a4 )
  if ( bus1  .ne.  blank )  go to 4235
  if ( bus2  .ne.  blank )  go to 4235
  h2 = h1
4235 tby(i) = (h1 +h2 +h2) / 3.0
  go to moon, (4236, 711)
4236 if( tbx(i)  .ne.  0.0 )  go to 4239
  if( tby(i)  .ne.  0.0 )  go to 4239
  write (kunit6, 4268)
4268 format (  40h+blank card terminating conductor cards.   )
  go to 6
4239 write (kunit6, 4240)   tbtb2(i), tbr(i), itbtb3(i)
4240 format ( 21h+line-conductor card.,  2e11.3, i6  )
  if (i3 .le. 1)   go to 706
  nbundl = 1
  i4 = 1
  xx = tbx(i)
  yy = tby(i)
  angl = (pi - twopi  / i3) / 2.0
  dangl = twopi  / i3
  radius = d8 / (24.0 * cosz(angl))
  if( metrik .eq. 1 ) radius = radius * .12d0
  d9r = d9 / picon
703 tbx(i) = xx +radius * cosz(d9r -dangl * i4)
  tby(i) = yy +radius * sinz(d9r -dangl * i4)
  if (i4 .eq. i3)   go to 706
  i4 = i4 +1
  i = i +1
  itbic(i) = itbic(i-1)
  tbtb2(i) = tbtb2(i-1)
  tbr(i) = tbr(i-1)
  itbtb3(i) = itbtb3(i-1)
  tbg(i) = tbg(i-1)
  tbd(i) = tbd(i-1)
  tbtext(i) = tbtext(i-1)
  go to 703
706 if (i .le. lphase )   go to 5
7 kill = 82
  lstat(15) = lphase
  lstat(19) = 7
  go to 9200
  !                                         sorting of conductor data
6 tbd(i) = 0.0
  if( iprsup .ge. 1 ) write (lunit6, 3625) (itbic(k), itbtb3(k), tbtb2(k), tbr(k), tbg(k), tbd(k), tbx(k), tby(k), tbtext(k), k=1, i )
3625 format ( //,  25h unsorted conductor table   ,/, ( 1x, 2i6, 6e15.5, 5x, a6 ) )
  !              '      '      '    convert from si units for calculation.
  if (metrik .ne. 1 ) goto 981
  do k = 1 , i
     tbd(k) = tbd(k) * finpcm
     tbx(k) = tbx (k) * ftpm
     tby(k) = tby(k) * ftpm
     tbr(k) = tbr(k) / fmipkm
     jb = itbtb3(k)
     if ( jb  .lt.  2 )   tbg(k) = (tbg(k) + fspac) / fmipkm
     if ( jb .eq. 2 ) tbg(k) = tbg(k) * finpcm
982 end do
981 call move0 ( ic(1), lphase )
  ktot=0
  kcirct=0
  k=0
10 k=k+1
  if(tbd(k).eq.0.) go to 15
  i1=itbic(k)
  if(i1.eq.0) go to 12
  if(i1.lt.0) go to 10
  if(i1.gt.kcirct) kcirct=i1
  i = lphpl1 - i1
  if(ic(i).eq.0) go to 13
12 ktot=ktot+1
  i=ktot
  if(i1.eq.0) go to 13
  ip=ktot
18 j=i-1
  if(j.eq.0) go to 13
  if(ic(j).gt.0) go to 13
  i=j
  ic(ip)=ic(i)
  tb2(ip)=tb2(i)
  itb3(ip)=itb3(i)
  r(ip)=r(i)
  d(ip)=d(i)
  gmd(ip)=gmd(i)
  x(ip)=x(i)
  y(ip)=y(i)
  text(ip)=text(i)
  ip=ip-1
  go to 18
13 kfull=ktot+kcirct
  if(kfull.gt.lphase ) go to 7
  ic(i)=i1
  tb2(i)=tbtb2(k)
  itb3(i)=itbtb3(k)
  r(i)=tbr(k)
  d(i)=tbd(k)
  gmd(i)=tbg(k)
  x(i)=tbx(k)
  y(i)=tby(k)
  text(i)=tbtext(k)
  go to 10
15 iprint=0
  ntol = 0
16 if ( ik  .gt.  0 )   go to 3006
  !     read input card using cimage
33316 call cimage
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunt13
  read (unit = abuff(1), fmt = 4230) bufsem
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
  read (unit = abuff, fmt = 17) rearth, freq, corr, j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, &
       j11, j12, iw, dist, j13, j14, j15, j16, isegm, mutual, ik, ips, j17, imodal, itrnsf, conduc
17 format(f8.2,f10.2,f10.6,i2,5i1,i2,5i1,i2,f8.3,i2,5i1,3i3, 2i2, e8.2)
  if (metrik .eq. 1)  conduc = conduc /fmipkm
  if ( conduc .eq. 0 )  conduc = 3.22d-9
  if ( muntrn .eq. 1 )  imodal = 1
  if ( mtrnsp .eq. 1  .or.  mspedb .eq. 1 )  imodal = 0
  if ( kexact .ne. 88333 )  go to 3168
  freq = fminfs
  ik = alog1z ( fmaxfs / fminfs ) + epsiln
  ips = valu14 / alogz(-delffs)  + epsiln
  distsv = dist
  go to 217
3168 if ( lastov .eq. 1  .or. imodal .eq. 1 )  go to 217
  if ( lastov .ne. 39 )  go to 217
  gmode = conduc
  if ( metrik .eq. 1 )  gmode = gmode * fmipkm
217 jspecl = j1 + j2 + j3 + j4 + j5 + j6 + j7 + j8 + j9 +j10 + j11 + j12
  nfreq = nfreq + 1
  if ( lastov .ne. 1 .and. imodal .eq. 0 .and. nfreq .eq. 2 .and. ik .eq. 0 .and. ips .eq. 0 .and. lastov .ne. 45) go to 33316
  j5out = j5
  j8out = j8
  j2out = j2
  j4out = j4
  j6out = j6
  j9out = j9
  if ( imodal .ne. 0 ) go to 117
  j5 = 1
  j9 = 1
117 if (j17 .ne. 44)  go to 28725
  j5 = 1
  j8 = 1
  iw = 1
28725 if (imodal .le. 0 ) go to 18725
  j2 = 1
  j8 = 1
18725 if ( ik  .le.  0 )   go to 8726
  j6 = 1
  j9 = 1
8726 if ( ck1  .ne.  -fltinf )   go to 7446
  if ( dist  .le.  0.0 )   go to 7446
  ck1 = dist
  if( metrik .eq. 1 ) ck1 = ck1 * fmipkm
7446 if ( ci1  .ne.  -fltinf )   go to 7449
  if ( rearth  .le.  0.0 )   go to 7449
  ci1 = rearth
7449 if ( mutual  .gt.  0 )   j8 = 1
  if ( kolbeg  .gt.  0 )   go to 4264
  if ( corr  .ne.  0.0 )  go to 4264
  read (unit = abuff, fmt = 4256) bus1, bus2
4256 format ( 18x, a6, a4 )
  if( bus1 .ne. blank )  go to 4264
  if( bus2 .ne. blank )  go to 4264
  corr = tenm6
4264 iprint = iprint + 1
  if ( rearth  .ne.  0.0 )   go to 4267
  write (kunit6, 4266)
4266 format (  40h+blank card terminating frequency cards.   )
  if ( kexact .ne. 88333)    go to 2
  lastov = 44
  !      nchain = 8
  nchain = 2
  go to 9900
4267 if ( ialter .eq. 2)  rearth = ci1
  write (kunit6, 3618)   rearth, freq, dist
3618 format ( 16h+frequency card.,  3e11.3  )
  call interp
  if( metrik .eq. 1 ) dist = dist * fmipkm
  ipunch = j17
  jpralt = 1
  if ( ialter  .eq.  0 )   go to 3729
  do jj=1, 16
     if (jprmat (jj) .ne. 0)   go to 13721
3721 end do
  jpralt = 0
  if (ialter .eq. 3)   go to 13723
3723 jprmat(5) = 1
  jprmat(8) = 1
  iw = 0
  go to 3729
13721 if (ialter .ne. 3)   go to 3723
13723 jprmat (6) = 1
  jprmat(9) = 1
3729 kkk = 0
  if ( iprsup  .ge.  1 ) write (lunit6, 7454)  iprint, ldisfr, ialter
7454 format ( /,  44h new frequency card.  iprint  ldisfr  ialter, /,20 x, 3i8  )
  do jj = 1, 16
     if (jprmat (jj) .ne. 0)  go to 3733
3731 end do
  go to 33316
3733 if (freq .eq. 0.0)  freq = statfr
  if ( ik  .gt.  0 )   go to 7456
  if ( ialter  .eq.  3 )   go to 7453
  if ( lastov  .eq.  1 )   go to 7453
  volti(iprint) = rearth
  if ( ialter .eq. 2 )  go to 7453
  voltk(iprint) = freq
  if ( iprint  .le.  ldisfr )   go to 7453
  kill = 170
  lstat(14) = ldisfr
  lstat(19) = 7456
  go to 9200
7453 go to 3007
7456 if ( ips  .eq.  0 )   ips = 1
  if (ips*ik .le. 399)   go to 7458
  !     this check really belongs in "semlyen setup" (overlay 45):
  if ( lastov  .ne.  45 )   go to 7458
  kill = 160
  lstat(14) = 399
  lstat(19) = 7458
  go to 9200
7458 continue
  factor = ips
  factor = valu14 / factor
  fdecad=freq
  if ( lastov .eq. 1  .and. imodal .ne. 0)  go to 7459
  dist=0.
7459 iii=0
  voltbc(1) = freq
  voltbc(2) = freq * 10.**ik
  voltbc(3) = expz(factor)
  d13 = voltbc(2) / voltbc(1)
  voltbc(4) = alogz(d13) / alogz(voltbc(3)) + 1.5
  voltbc(5) = ik
  voltbc(6) = ips
  icheck = iprint - 1
  iprint = icheck
  ktab = kcirct
  nmode = 2
  if ( kcirct .lt. 2 )  nmode = 1
  if ( iprsup  .ge.  1 ) write (lunit6, 7457) icheck, ktab, (voltbc(jj), jj=1, 3)
7457 format ( /, 39h logarithmic frequency.  icheck    ktab, 8 x,  7hvolt(1),  8x,  7hvolt(2),  8x,  7hvolt(3),  /, &
          23x,  2i8,  3e15.6  )
  if ( mfrqpr  .gt.  0 ) write (lunit6, 3004)
  go to 3006
3005 iii=iii+1
!!!!      if ( iii  .ge.  ik )   go to 33316
  if (iii .ge. ik) go to 8801
  go to 8802
8801 if ( kexact .ne. 88333 )  go to 8701
  if ( nfqpl1 .eq. 1 )  go to 8701
  nfqpl1 = 1
  go to 8802
8701 if (nfreq .ne. 3) go to 8800
  !!    ** identical eigenvalue ? **
  if (nrp.gt.0 .or. itrnsf .eq. 0) go to 8800
  write (*,*) '$$  mrr,ntol,nfreq are  : ', mrr,ntol,nfreq
  rtio=(mrr*1.0)/ntol
  if (rtio.lt.0.75) go to 8800
  nrp=1
  freq=fdecad
  do i=1,ntol*ktab
     backspace lunit9
8804 end do
  ntol=0
  go to 16
8800 go to 33316
8802 fdecad=fdecad*10.0
  kkk = 1
3006 pkkk = kkk
  freq = fdecad * expz( pkkk * factor )
  iprint=iprint+1
  if ( iprsup  .ge.  1 ) write (lunit6, 23006)  iprint, kkk, iii, freq, ci1, ck1
23006 format ( /, 63h next logarithmically-spaced frequency.  iprint     kkk     iii, 11x, 4hfreq, 12x, 3hci1, 12x, 3hck1,  /, &
           39x, 3i8, 3e15.6  )
  if(kkk.gt.ips) go to 3005
  if ( freq+epsiln .lt. fmaxfs  .or. kexact .ne. 88333)  go to 8806
  if ( nfqpl1 .eq. 1 )  kkk = ips
8806 kkk = kkk + 1
3007 omega=twopi    *freq
  if ( ik  .gt.  0 )   go to 3008
  write (lunit6, 4)
4 format (//, ' Line-conductor table after sorting and initial processing.    ',/, 1x, &
       5htable, 3x, 5hphase, 3x, 11hskin effect, 4x, 10hresistance, 3x, 28hreactance-data specification, 5x, &
       8hdiameter, 3x, 10hhorizontal, 3x, 10havg height, 3x,  9hconductor  )
8 format( 3x, 3hrow, 2x, 6hnumber, 8x, 6hr-type, 4x, 10hr (ohm/mi), 3x, 6hx-type, 6x, 16hx(ohm/mi) or gmr, 5x, 8h(inches), 5x, 8hx (feet), &
       5x, 8hy (feet), 8x, 4hname   )
  if ( metrik .eq. 0 ) write(lunit6,8)
  if ( metrik .eq. 1 ) write(lunit6,9)
9 format(3x, 3hrow, 2x, 6hnumber, 8x, 6hr-type, 4x, 10hr (ohm/km), 3x, 6hx-type, 6x, 16hx(ohm/km) or gmr, 5x, 8h(  cm  ), 5x, 8hx (mtrs), &
       5x, 8hy (mtrs), 8x, 4hname   )
3008 idist = 1
  if ( dist  .eq.  0.0  .and. ipunch  .ne. 44 )  go to 4271
  if ( kcirct  .eq.  0 )  go to 4271
  if ( kcirct  .lt.  lphd2 )  go to 4275
4271 idist = 0
4275 j56 = j5+j6 + idist
  !
  !                                                  formation of p-matrix
  ip=0
  k=0
  f1=unity/omega
  if(iw.gt.0) f1=unity
  f1 = f1 * valu1
  f2=f1*2.0
20 k=k+1
  if(k.gt.kfull) go to 30
  if(k.le.kcirct) go to 28
  j=k-kcirct
21 x1=x(j)
  y1=y(j)
  d1=d(j)
  if(iprint.ne.1) go to 222
  if ( metrik .eq. 0 ) goto 221
  rj = r(j) * fmipkm
  yj = y(j) / ftpm
  xj = x(j) / ftpm
  dj = d(j) / finpcm
  gmdj = gmd(j)
  jb = itb3(j)
  if ( jb  .lt.  2 )   gmdj = gmdj * fmipkm - fspac
  if ( jb  .eq.  2 )   gmdj = gmdj / finpcm
  write(lunit6,22) k,ic(j),tb2(j),rj,jb, gmdj, dj,xj,yj,text(j)
  goto 225
221 write(lunit6,22) k,ic(j),tb2(j),r(j),itb3(j),gmd(j),d(j),x(j),y(j), text(j)
22 format ( 1x, i5, i8, 2f14.5, i9, f22.6, f13.5, 2f13.3, 6x, a6 )
225 if(itb3(j).ne.4) go to 222
  if(tb2(j).gt.0.) go to 224
  kill = 83
  lstat(15) = j
  lstat(19) = 224
  go to 9200
224 gmd(j)=d(j)
222 i=0
23 i=i+1
  ip=ip+1
  if(i.eq.k) go to 24
  j=i-kcirct
  if ( i .le. kcirct )  j = lphpl1 - i
  dx=(x(j)-x1)**2
  h1=y(j)-y1
  h2=y(j)+y1
  if(dx.eq.0.0 .and. h1.eq.0.0) go to 26
  r1=alogz((dx+h2*h2)/(dx+h1*h1))
  p(ip)=r1*f1
  z(ip)=r1
  go to 23
24 p(ip)=alogz(48.*y1/d1)*f2
  go to 20
26 kill = 84
  lstat(15) = i
  lstat(19) = 28
  go to 9200
28 j = lphpl1 - k
  if(ic(j).ne.0) go to 21
  kill = 85
  lstat(19) = 30
  lstat(15) = k
  go to 9200
30 if (kkk .gt. 0 .and. lastov .ne. 1) go to 3020
  if ( jspecl .eq. 0 )  go to 7025
  write(lunit6,25)rearth,freq,corr
25 format(//, 46h0following matrices are for earth resistivity=,f8.2,21h ohm-m and frequency=,f13.2,23h hz. correction factor=, f10.6)
7025 if ( kkk .gt. 1 )  go to 3020
  if(isegm.gt.0) write(lunit6,12345)
12345 format(1h ,30x,53h************earth wires will be segmented************)
  if(ik.gt.0) write(lunit6,3004)
3004 format(1h0,6x,47h-----------------zero sequence-----------------,13x,47h---------------positive sequence---------------,/,4x, &
          25halpha,8x,4hbeta,8x,1hr,8x,8hl(milli-,4x,8hc(micro-,6x,5halpha,8x,4hbeta,8x,1hr,8x,8hl(milli-,4x,8hc(micro-,4x,9hfrequency,/, &
          59h neper/mile  radian/mile  ohm/mile  henry/mile) farad/mile),61h  neper/mile  radian/mile  ohm/mile   henry/mile) farad/mile) 5x,2hhz )
3020 write (lunt13)  (z(i),i=1,ip)
  if ( iprsup  .ge.  2 ) write (lunit6, 4427)  ( z(i), i=1, ip )
4427 format ( /, 28h at 4427.  (z(i), i=1, ip) .   ,/, ( 1x, 8e16.6 ) )
  if ( iprsup  .ge.  1 ) write (lunit6, 4428)  j1, j4, kcirct, j2, j3, j56, kfull
4428 format ( /, 9h at 4428.  , 7i12 )
!
  !                                   reductions and inversions p-matrix
  switch=-unity
  if(j1.gt.0) go to 36
31 if(j4.gt.0) go to 37
32 if(kcirct.eq.0) go to 80
  if(j2+j3+j56  .eq.0) go to 80
  !                          begin elimination of earth wires and bundling
49 k=kcirct
50 k=k+1
  if(k.gt.kfull) go to 56
  i=k-kcirct
  i=ic(i)
  if(i.eq.0) go to 50
  i2=i*(i-1)/2
  k2=k*(k-1)/2
  kk=k2+i
  h1=p(kk)
  h2=z(kk)
  kk=k2+k
  l=0
51 if(l.lt.i) go to 54
  i2=i2+l
52 if(l.lt.k) go to 55
  k2=k2+l
53 l=l+1
  p(k2)=p(k2)-p(i2)
  if(switch.gt.0.)z(k2)=z(k2)-z(i2)
  if(l.ne.kfull) go to 51
  p(kk)=p(kk)-h1
  if(switch.gt.0.) z(kk)=z(kk)-h2
  go to 50
54 i2=i2+1
  go to 52
55 k2=k2+1
  go to 53
56 kp=kcirct*(kcirct+1)/2
  if(switch.gt.0.) go to 202
  call redu44 ( p(1), workr1(1), kfull, kcirct )
!!!!      write (*,*) ' after redu44 on c.  p(1:6) =',
!!!!     1                                ( p(i), i=1, 6 )
  if( kill .gt. 0 )  go to 9200
  !                            end elimination of earth wires and bundling
!!!!      write (*,*) ' branch to 38?  j2, j3 =',  j2, j3
  if(j2+j3.gt.0) go to 38
33 if(j56.gt.0) go to 39
  !
  !                                         formation of z-matrix
80 switch=+unity
  j56=j8+j9+j11+j12+idist
  if(j56+j7+j10.eq.0) go to 600
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunt13
  k=0
  if(rearth.eq.0.) go to 100
  f1 = valu2 * sqrtz( freq/rearth )
100 f2 = omega * valu3
  imax=30
  if(corr.eq.0. .or.rearth.eq.0.) imax=-1
  if ( corr   .gt.   corchk )   imax = corr - onehaf
  if(imax.gt.31) imax=30
  read (lunt13)  (p(i),i=1,ip)
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunt13
  ip=0
120 k=k+1
  if(k.gt.kfull) go to 200
  if(k.le.kcirct) go to 129
  j=k-kcirct
  go to 121
129 j = lphpl1 - k
121 x1=x(j)
  y1=y(j)
  r1=r(j)
  h1=tb2(j)
  h2=unity-2.0*h1
  if(h1.gt.0.) call skin(h2,r1,freq,r1,h1)
  g1=gmd(j)
  ix=itb3(j)
  izero=1
  xm=0.
  if(ic(j).eq.0 .and. isegm.gt.0) izero=0
  if(ix.lt.2) go to 150
  if(ix.eq.3) g1=g1*d(j)/2.0
  xs=alogz(24.0*y1/g1)*onehaf
122 if(ix.ne.4) go to 125
  xs = xs + h1/f2 - onehaf*alogz( d(j)*onehaf/g1 )
125 i=0
123 i=i+1
  ip=ip+1
  if(i.eq.k) go to 124
  j=i-kcirct
  if ( i  .le.  kcirct ) j = lphpl1- i
  rm=0.
  if(izero.eq.0) go to 126
  xm=p(ip)/4.0
  if(imax.lt.0) go to 126
  dx=absz(x(j)-x1)
  h2=y(j)+y1
  s=sqrtz(h2*h2+dx*dx)
  z1=s*f1
  if(z1.gt.5.0) go to 151
  !                    begin carson series for mutual impedance if z.le.5.
  rm = twopi / 16.
  zl=alogz(z1)
  xm = xm  +  ( valu4 - zl ) * onehaf
  if(imax.eq.0)go to 126
  s1=dx/s
  c1=h2/s
  cs=c1*z1
  sn=s1*z1
  if (imax .gt. 1)   phi = asinz(s1)
  m5=0
  error=0.
1110 k5=0
  i5=m5*4
1111 k5=k5+1
  i5=i5+1
  if(k5.eq.1.or.k5.eq.3) go to 1113
  deltap=((ccars(i5)-zl)*cs+phi*sn)*bcars(i5)
  h1=-dcars(i5)*cs
  if(k5.eq.4) go to 1114
  deltaq=h1
1004 rm=rm+deltap
  xm=xm+deltaq
  if(i5.eq.imax) go to 126
  if ( corr   .gt.   corchk )   go to 1005
  if(absz(deltap).lt.corr .and. absz(deltaq).lt.corr) go to 1115
  error = 0.
1005 h1=sn*c1+cs*s1
  cs=(cs*c1-sn*s1)*z1
  sn=h1*z1
  if(k5.lt.4) go to 1111
  m5=m5+1
  go to 1110
1113 deltaq=bcars(i5)*cs
  deltap=deltaq
  if(k5.eq.1) deltap=-deltap
  go to 1004
1114 deltaq=-deltap
  deltap=h1
  go to 1004
1115 if ( error  .gt.  onehaf )   go to 126
  error = unity
  go to 1005
!                      end carson series for mutual impedance if z.le.5.
!
  !                    begin carson series for mutual impedance if z.gt.5.
151 s=s*z1
  sn=dx/s
  cs=h2/s
  s2=sn*cs*2.0
  c2=cs*cs-sn*sn
  rm=-c2
  do i5=1,7,2
     rm=rm+ccars(i5)*cs
     xm=xm+dcars(i5)*cs
     h1=cs*s2+sn*c2
     cs=cs*c2-sn*s2
1006 sn=h1
  end do
  !                      end carson series for mutual impedance if z.gt.5.
126 p(ip)=rm*f2
  z(ip)=xm*f2
  if ( iprsup  .ge.  4 ) write (lunit6, 4439)  i, k, kfull, kcirct, i5, ip, p(ip), z(ip)
4439 format ( 9h at 4439   , 6i10, 2e16.6  )
  go to 123
124 if(imax.lt.0) go to 128
  r1=r1/f2
  z1=y1*f1*2.0
  if (z1.gt.5.0) go to 152
  !                    begin carson series for self impedance if z.le.5.0
  r1 = r1 + twopi/16.
  zl=alogz(z1)
  xs = xs  +  ( valu4 - zl ) * onehaf
  if(imax.eq.0) go to 127
  m5=0
  cs=z1
  error=0.
2110 k5=0
  i5=m5*4
2111 k5=k5+1
  i5=i5+1
  if(k5.eq.1 .or. k5.eq.3) go to 2113
  deltap=(ccars(i5)-zl)*bcars(i5)*cs
  h1=-dcars(i5)*cs
  if(k5.eq.4) go to 2114
  deltaq=h1
2004 r1=r1+deltap
  xs=xs+deltaq
  if(i5.eq.imax) go to 127
  if ( corr   .gt.   corchk )   go to 2005
  if(absz(deltap).lt.corr .and. absz(deltaq).lt.corr) go to 2115
  error = 0.
2005 cs=cs*z1
  if(k5.lt.4) go to 2111
  m5=m5+1
  go to 2110
2113 deltaq=bcars(i5)*cs
  deltap=deltaq
  if(k5.eq.1) deltap=-deltap
  go to 2004
2114 deltaq=-deltap
  deltap=h1
  go to 2004
2115 if ( error  .gt.  onehaf )   go to 127
  error = unity
  go to 2005
  !                      end carson series for self impedance if z.le.5.0
  !
  !                    begin carson series for self impedance if z.gt.5.0
152 cs=unity/z1
  c2=cs*cs
  r1=r1-c2
  do i5=1,7,2
     r1=r1+ccars(i5)*cs
     xs=xs+dcars(i5)*cs
2006 cs=cs*c2
  end do
  !                      end carson series for self impedance if z.gt.5.0
127 r1=r1*f2
128 z(ip)=xs*f2
  p(ip)=r1
  if ( iprsup  .ge.  4 ) write (lunit6, 4440)  i, k, kfull, kcirct, i5, ip, p(ip), z(ip)
4440 format ( 9h at 4440   , 6i10, 2e16.6  )
  go to 120
150 xs=g1/f2
  if(ix.eq.1) xs=xs*freq/60.0
  xs=xs+alogz(y1*2.0)* onehaf
  g1=24.0*y1/expz(2.0*xs)
  go to 122
!
!                                  routines for z-printing and inversion
!     impedance matrix for physical conductors is in p(1),...p(ip) (real
!     part) and z(1)....z(ip) (imaginary part).
200 if ( j7  .gt.  0 ) call output ( metrik, p(1), z(1), unity, kfull, ll6, ll1 )
  if ( iprsup  .ge.  1 ) write (lunit6, 4434)  j7, j10, j56, kcirct, j8, mutual, j11, j12, idist, j9, kp, j2, j3, j5, j6
4434 format ( /,  27h at 4234.   misc. integers.   ,/, ( 1x, 10i12 ) )
  if(j10.gt.0) go to 250
  if(j56.eq.0) go to 600
201 if(kcirct.eq.0) go to 600
  go to 49
202 call cxred2( p(1), z(1), workr1(1), workr2(1), kfull, kcirct )
  !     impedance matrix for equivalent phase conductors is in p(1),...
  !     p(kp) (real part) and z(1),...z(kp) (imaginary part).
  if ( j8out  .gt.  0 ) call output ( metrik, p(1), z(1), unity, kcirct, ll6, ll2 )
  if ( ipunch .ne. 44 )  go to 8202
  call punpie ( kcirct )
8202 if ( iprsup  .ge.  1 ) write (lunit6, 3202) imodal, dist
3202 format (  15h imodal, dist =,  i5, e17.5  )
  if ( lastov .ne. 39 )   go to 3203
  if ( nfreq   .ne.  1  )   go to 3203
  !     "marti setup" assumes distance in kilometers is on 9:
  distkm = dist / fmipkm
  write (lunit9) imodal, metrik, distkm, mspedb, itrnsf
  if (iprsup .gt. 1) write (lunit6, *) ' mspedb written on lunit9 =', mspedb
3203 if ( imodal .le. 0 )  go to 2202
  if( lastov .eq. 1  .and.  itrnsf .ne. -1  .and. itrnsf .ne. -9 )  itrnsf = 0
  n8 = kfull
  if ( nbundl .eq. 1 )  n8 = kcirct
  if ( kcirct .le. ndim)  go to 1202
  kill = 221
  lstat(19) = 1202
  lstat(14) = kcirct
  go to 9200
1202 if ( nfreq .eq. 3 )  ntol = ntol + 1
  if ( kexact .eq. 88333) dist = distsv
  call modal(array, xwc, xwy, yzr, yzi, tii, tir, tvi, tvr, er, ei, theta2, xtir, xtii, zsurge, dummi, dummr, tixf, work1, freq, &
       kcirct, iw, dist, metrik, fmipkm, ndim, ntri, nsqr2, itrnsf,  kfull, mrr, nrp, ntol, conduc)
  if( kill .ne. 0 ) go to 9900
2202 if (lastov.eq.39 .or. lastov.eq.45 .or. imodal .ne.0) go to 8777
  if ( kexact .eq. 88333 )  go to 45454
  kount = 0
  rewind lunit1
  if ( ipunch  .eq.  0 ) write (lunit7, 8769)  tclock, date1
8769 format ('c   Punched card output of transposed line which began at ',  2x,  2a4,  2x,  2a4  )
  if ( ipunch .ne. 0 )  go to 8770
  if ( imodal .eq. 0 .and. mspedb .eq. 0) write (lunit7, 8780) freq
8780 format (56hc   ***** transposed k.c. lee line segment calculated at, 2x, e10.3, 10h hz. *****   )
  if ( imodal .eq. 0 .and. mspedb .eq. 1) write (lunit7, 8785) freq
8785 format (59hc **** special double circuit transposed line calculated at, 2x, e10.3, 9h hz. ****         )
8770 rewind lunit2
  n5 = 0
  !     if ( jdatcs .gt. 0 )  go to 8777
  do n12=1, 9999
     read (lunit2, 8771)  ( texta6(i), i=1, 14 )
8771 format ( 13a6, a2 )
     if ( ipunch  .eq.  0 ) write (lunit7, 8772)  ( texta6(i), i=1, 13 )
8772 format ( 2hc ,  13a6 )
     if (idebug.eq.0) go to 8100
     write (lunit1, 8773)  ( texta6(i), i=1, 14 )
8773 format ( 3h c ,  13a6,  a2 )
     kount = kount+1
8100 if ( texta6(1) .eq. blank     .and. texta6(2) .eq. blank )   n5 = n5 + 1
     if ( n5  .ge.  1 )   go to 8777
8774 end do
8777 if ( ialter  .le.  0 )   go to 3734
  if ( lastov  .eq.  39 )  go to 3734
  write (lunit3)  ( p(jj), jj=1, kp )
  write (lunit3)  ( z(jj), jj=1, kp )
  if ( iprsup  .eq.  0 )   go to 3734
  write (lunit6, 7423)  kp,  ( p(jj), jj=1, kp ),  ( z(jj), jj=1, kp )
7423 format ( /,  39h output of  (r)  and  (l)  for semlyen,    , i6,   13h  cells each.,  /,  ( 1x,  8e16.4 ) )
3734 if ( mutual  .gt.  0 ) call outspc(p(1), z(1),kcirct,metrik,fmipkm)
  j56 = j11+j12+idist
  if ( lastov .eq. 39 )  go to 253
  if(j9.gt.0 .and. kcirct.ge.2) go to 253
203 if(j56.eq.0) go to 600
  do i=1,kp
     p(i)=-p(i)
204  z(i) = -z(i)
  end do
  call cxred2 ( p(1), z(1), workr1(1), workr2(1), kcirct, ll0 )
  !     inverted impedance matrix for equivalent phase conductors is in
  !     p(1),...p(kp) (real part) and z(1),...z(kp) (imaginary part).
  if ( j11  .gt.  0 ) call output ( metrik, p(1), z(1), unity, kcirct, ll5, ll2 )
  if(idist.eq.0)  go to 207
  do i=1,kp
     gd(i)=p(i)
206  bd(i)=z(i)
  end do
207 if(j12.eq.0 .or. kcirct.lt.2) go to 600
  call symm ( p(1), z(1), unity, kcirct, kk )
  !     inverted impedance matrix for symmetrical components of equivalent
  !     phase conductors is in p(1),...p(kk*(kk+1)/2) (real part) and z(1)
  !     ,...z(kk*(kk+1)/2) (imaginary part). definition of kk same as for
  !     inverse susceptance matrix
  call output ( metrik, p(1), z(1), unity, kk, ll5, ll3 )
  go to 600
250 if(j56.eq.0) go to 251
  write (lunt13)  (p(i),z(i),i=1,ip)
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunt13
251 do i=1,ip
     p(i) = -p(i)
252  z(i) = -z(i)
  end do
  call cxred2 ( p(1), z(1), workr1(1), workr2(1), kfull, ll0 )
  !     inverted impedance matrix for physical conductors is in p(1),...
  !     p(ip) (real part) and z(1),...z(ip) (imaginary part).
  call output ( metrik, p(1), z(1), unity, kfull, ll5, ll1 )
  if(j56.eq.0) go to 600
  read (lunt13)  (p(i),z(i),i=1,ip)
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunt13
  go to 201
253 if(j56.eq.0) go to 254
  write (lunt13)  (p(i),z(i),i=1,kp)
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunt13
254 go to 45454
  !  254 if ( lastov .eq. 39  .or.
  !     1          kcirct .gt. 3 ) go to 45454
256 call symm ( p(1), z(1), unity, kcirct, kk )
  !     impedance matrix for symmetrical components of equivalent phase
  !     conductors is in p(1),...p(kk*(kk+1)/2) (real part) and z(1),...
  !     z(kk*(kk+1)/2) (imaginary part). definition of kk same as for
  !     inverse susceptance matrix.
  if ( j9out  .gt.  0 ) call output ( metrik, p(1), z(1), unity, kk, ll6, ll3 )
  if(j6.eq.0) go to 9998
  rzero=p(1)
  xzero=z(1)
  rpos = p(ll5)
  xpos = z(ll5)
  zo=sqrtz(rzero**2+xzero**2)
  z1=sqrtz(rpos**2+xpos**2)
  zso=sqrtz(zo/yzero)
  zs1=sqrtz(z1/ypos)
  zoang=atan2z(xzero,rzero)* onehaf
  z1ang=atan2z(xpos,rpos)* onehaf
  propo=sqrtz(zo*yzero)
  prop1=sqrtz(z1*ypos)
  zsodrg=zoang*picon - 45.
  zs1drg=z1ang*picon - 45.
  d2 = twopi / 8.0
  alphao=propo*cosz(zoang+ d2)
  alpha1=prop1*cosz(z1ang+ d2)
  beto  =propo*sinz(zoang+ d2 )
  bet1=  prop1*sinz(z1ang+ d2 )
  go to 2254
45454 d13 = twopi * freq
!!!!      write (*,*) ' ready for z avg.  p(1:6) =',
!!!!     1                              ( p(i), i=1, 6 )
!!!!      write (*,*) ' ready for z avg.  z(1:6) =',
!!!!     1                              ( z(i), i=1, 6 )
!!!!      write (*,*) ' imodal, nfreq =',  imodal, nfreq
  !      if (imodal .ne. 0)    go to 2254
  if ( lastov .eq. 39 )  go to 6256
  if (imodal .ne. 0)    go to 256
  go to 6276
6256 if ( nfreq .eq. 1 )  go to 9998
  if ( imodal .eq. 0 )  go to 6276
  if ( nfreq .lt. 3 )  go to 9998
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
  if ( kcirct .lt. 2 )  go to 5059
  if ( mspedb .ne. 1 )  go to 5090
  rcoup = 0.
  xcoup = 0.
  do 5030  i = 1, 6
     rdiag = rdiag + p(n55)
     xdiag = xdiag + z(n55)
     if ( n55 .eq. 10 )  go to 5028
     jm = n55 - i + 1
     if ( n55 .ge. 15 )  jm = jm + 3
     do jp= jm, n55-1
        roff = roff + p(jp)
5025    xoff = xoff + z(jp)
     end do
5028 n55 = n55 + i + 1
5030 end do
  rdiag = rdiag / 6
  xdiag = xdiag / 6
  roff = roff / 6
  xoff = xoff / 6
  n55 = 6
  do jp = 4, 6
     j = n55 + 1
     do i = j, j+2
        rcoup = rcoup + p(i)
5040    xcoup = xcoup + z(i)
     end do
     n55 = n55 + jp
5050 end do
  rcoup = rcoup / 9
  xcoup = xcoup / 9
  xtir(1) = rdiag + 2*roff + 3*rcoup
  xtii(1) = xdiag + 2*xoff + 3*xcoup
  xtir(2) = xtir(1) - 6*rcoup
  xtii(2) = xtii(1) - 6*xcoup
  xtir(3) = rdiag - roff
  xtii(3) = xdiag - xoff
  if ( lastov .eq. 1 )  go to 5080
  if ( lastov .ne. 39 .and. kexact .ne. 88333)  go to 9998
  do 5053  i = 1, 3
     zmmag = sqrtz ( xtir(i) ** 2 + xtii(i) ** 2 )
     zmang = atan2z ( xtii(i), xtir(i) )
     ymmag = sqrtz ( gmode ** 2  + dummi(i) ** 2 )
     ymang = atan2z ( dummi(i), gmode  )
     zsurge(i) = sqrtz ( ymmag / zmmag )
     theta2(i) = ( ymang - zmang ) / 2.
     d1 = sqrtz ( zmmag * ymmag )
     d2 = ( zmang + ymang ) / 2.
     er(i) = d1 * cosz(d2) * fmipkm
     ei(i) = d1 * sinz(d2) * fmipkm
     if ( kexact .ne. 88333 )  go to 5053
     er(i) = d1 * cosz(d2) * distsv
     ei(i) = d1 * sinz(d2) * distsv
5053 end do
  do i = 1, kcirct
     if ( i .gt. 2 )  go to 5058
     write (lunit9) d13,zsurge(i),theta2(i),er(i),ei(i)
     go to 5060
5058 write (lunit9) d13,zsurge(3),theta2(3),er(3),ei(3)
5060 end do
  if (iprsup .ge. 1 ) write (lunit6, 5065) freq, kcirct, (zsurge(i),theta2(i), er(i), ei(i), i = 1,3 )
5065 format ('  at 5065, freq,kcirct,zsurge,theta2,er,ei are: ',/, e10.3,i3,12e9.2 )
  go to 9998
5080 write (lunit7,228)
  do i = 1, 3
     zsurge(i) = sqrtz( xtii(i) / dummi(i) )
     er(i) = d13 / sqrtz( xtii(i) * dummi(i) )
5081 end do
  if (iprsup .ge. 1 ) write (lunit6,*) ' zsurge, er = ',(zsurge(i),er(i),i=1,3)
  do i = 1, kcirct
     if ( i .gt. 1 )  go to 5082
     write (lunit7,140) i, brname(2*i-1), brname(2*i), xtir(i), zsurge(i), er(i), dist
     go to 5088
5082 if ( i .gt. 2 )  go to 5084
     write (lunit7,140) i, brname(2*i-1), brname(2*i), xtir(3), zsurge(3), er(3), dist
     go to 5088
5084 if ( i .gt. 3 )  go to 5086
     write (lunit7,140) i, brname(2*i-1), brname(2*i), xtir(2), zsurge(2), er(2), dist
     go to 5088
5086 if ( i .le. 9 ) write (lunit7,1475) i, brname(2*i-1), brname(2*i)
     if ( i .gt. 9 ) write (lunit7,9475) i, brname(2*i-1), brname(2*i)
9475 format ( i2, 2a6 )
5088 end do
  write (lunit7, 29)
  go to 9998
5059 xtir(1) = p(1)
  xtii(1) = z(1)
  go to 480
!!!!      write (*,*) ' enter do 478 loop.   kcirct =',  kcirct
5090 do i=1, kcirct
     rdiag = rdiag + p(n55)
     xdiag = xdiag + z(n55)
     do jp=n55-i+1, n55-1
        roff = roff + p(jp)
479     xoff = xoff + z(jp)
     end do
478  n55 = n55 + i + 1
  end do
  rdiag = rdiag / kcirct
  xdiag = xdiag / kcirct
  roff = roff / ( kcirct * ( kcirct - 1 ) / 2 )
  xoff = xoff / ( kcirct * ( kcirct - 1 ) / 2 )
!!!!      write (*,*) ' average of diagonals,  rdiag, xdiag =',  rdiag,
!!!!      write (*,*) ' average of off-diag,  roff, xoff =',  roff, xoff
  xtir(1) = rdiag + ( kcirct - 1 ) * roff
  xtii(1) = xdiag + ( kcirct - 1 ) * xoff
  xtir(2) = rdiag -  roff
  xtii(2) = xdiag -  xoff
  if ( lastov .eq. 39 .or. lastov .eq. 45  .or. kexact .eq. 88333) go to 480
  if ( iprsup .lt. 1  )  go to 1228
  write (lunit6,*) ' gmode,xtir, xtii, , dummi =', gmode, (xtir(i),xtii(i),dummi(i), i = 1, 2)
1228 write (lunit7,228)
228 format (11h$vintage, 1)
  do i = 1, 2
     zsurge(i) = sqrtz( xtii(i) / dummi(i) )
     er(i) = d13 / sqrtz( xtii(i) * dummi(i) )
5092 end do
  do i = 1, kcirct
     if ( i .gt. 2 )  go to 1473
     write (lunit7,140) i, brname(2*i-1), brname(2*i),xtir(i), zsurge(i), er(i), dist
140  format (1h-,i1,2a6,12x,4e12.5,2h 1,2x)
     go to 1480
1473 if ( i .le. 9 ) write (lunit7,1475) i, brname(2*i-1), brname(2*i)
     if ( i .gt. 9 ) write (lunit7,9475) i, brname(2*i-1), brname(2*i)
1475 format (1h-,i1,2a6)
1480 end do
  write (lunit7, 29)
29 format (11h$vintage, 0)
  go to 256
480 do i = 1, nmode
     zmmag = sqrtz ( xtir(i) ** 2 + xtii(i) ** 2 )
     zmang = atan2z ( xtii(i), xtir(i) )
     ymmag = sqrtz ( gmode ** 2  + dummi(i) ** 2 )
     ymang = atan2z ( dummi(i), gmode  )
     zsurge(i) = sqrtz ( ymmag / zmmag )
     theta2(i) = ( ymang - zmang ) / 2.
     d1 = sqrtz ( zmmag * ymmag )
     d2 = ( zmang + ymang ) / 2.
     er(i) = d1 * cosz(d2) * fmipkm
     ei(i) = d1 * sinz(d2) * fmipkm
     if ( kexact .ne. 88333 )  go to 5093
     er(i) = d1 * cosz(d2) * distsv
     ei(i) = d1 * sinz(d2) * distsv
5093 end do
  do i = 1, kcirct
     if ( i .gt. 1 )  go to 5098
     write (lunit9) d13,zsurge(i),theta2(i),er(i),ei(i)
     go to 5094
5098 write (lunit9) d13,zsurge(2),theta2(2),er(2),ei(2)
5094 end do
  if (iprsup .ge. 1 ) write (lunit6, 5095) freq, kcirct, (zsurge(i),theta2(i), er(i), ei(i), i = 1,2 )
5095 format ('  at 5095, freq,kcirct,zsurge,theta2,er,ei are: ',/, e10.3,i3,8e12.3 )
  go to 9998
2254 rzero1 = rzero
  rpos1 = rpos
  xzero1 = xzero
  xpos1 = xpos
  yzero1 = yzero
  ypos1 = ypos
  zo=1000./omega
  z1=1000000./omega
  xzero=xzero*zo
  xpos=xpos*zo
  yzero=yzero*z1
  ypos=ypos*z1
  if (iprsup .ge. 1) write (lunit6, 13012)  ipunch
13012 format (19h at 13012, ipunch =, i10)
  if ( ipunch  .eq.  0  .or.  ipunch .eq. 44)  go to 13039
  if ( ipunch  .ne.  1 )   go to 13011
  write (lunit7, 3011)  alphao, beto, alpha1, bet1, freq
3011 format(5e15.5)
  go to 13039
13011 if ( mfrqpr  .gt.  0 ) write (lunit6, 3012)  alphao, beto, rzero, xzero, yzero, alpha1, bet1, rpos, xpos, ypos, freq
3012 format ( 1x, e11.4, 9e12.4, e12.5 )
  if ( ipunch  .ne. 2 )   go to 13019
  write (lunit7, 13015)  rzero, xzero, freq
13015 format ( 3e16.8 )
  go to 13039
13019 if ( ipunch  .ne.  3 )   go to 13024
  write (lunit7, 13015)  rpos, xpos, freq
  go to 13039
13024 if ( ipunch  .ne.  88 )   go to 13031
  if (metrik .eq. 0 ) go to 13026
  rzero = rzero * fmipkm
  xzero = xzero * fmipkm
13026 write (lunit9)  rzero, xzero, freq
  if (iprsup .ge. 1) write (lunit6, 23024)  rzero, xzero, freq
23024 format (31h rzero, xzero and freq on unit9, 3e15.6)
  go to 13039
13031 if ( ipunch  .eq.  89 )   go to 13035
  kill = 162
  lstat(19) = 13031
  go to 9200
13035 if (metrik .eq. 0) go to 13037
  rpos = rpos * fmipkm
  xpos = xpos * fmipkm
13037 write (lunit9)  rpos, xpos, freq
13039 if (ik .gt. 0)   go to 3006
3010 vol=omega/beto
  alphao=alphao*valu5
  alpha1=alpha1*valu5
  v1l=omega/bet1
  waveo= twopi /beto
  wave1= twopi /bet1
  if( metrik .eq. 1 ) go to 9992
  write(lunit6,9991)zso,zsodrg,alphao,vol,waveo,rzero,xzero1,yzero1, zs1,zs1drg,alpha1,v1l,wave1,rpos,xpos1,ypos1
9991 format('0sequence', 6x, 'surge impedance', 7x, 'attenuation   velocity    wavelength   resistance    reactance   susceptance', /, 9x, &
          'magnitude(Ohm) angle(degr.)   dB/mile      miles/s       miles Ohm/mile     Ohm/mile     mho/mile', /, '   zero  ', 8e13.5/ &
          ' positive', 8e13.5)
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
  write (lunit6, 9993) zso,zsodrg,alphao,vol,waveo,rzero1,xzero1, yzero1,zs1,zs1drg,alpha1,v1l,wave1,rpos1,xpos1,ypos1
9993 format('0sequence', 6x, 'surge impedance', 7x, 'attenuation   velocity    wavelength   resistance    reactance   susceptance', /, 9x, &
          'magnitude(Ohm) angle(degr.)    dB/km         km/s          km Ohm/km       Ohm/km       mho/km ', /, '   zero  ', 8e13.5, / &
          ' positive', 8e13.5)
9998 if ( kexact .ne. 88333 )  go to 9996
  n1 = 1
  n9sq = kcirct ** 2
  if ( mspedb .eq. 1 )  go to 9540
  k = 1
  do j=1, kcirct
     do i=1, kcirct
        dummr(k) = 1.
        if ( i .eq. j .and. i .ne. 1 )   dummr(k) = 1 - i
        if ( i .gt. j .and. j .ne. 1 )   dummr(k) = 0.
5955    k = k + 1
     end do
5960 end do
  do i =1,n9sq,kcirct
     k = i + kcirct - 1
     temp = 0.
     do j=i, k
5965    temp = temp + dummr(j) ** 2
     end do
     temp = sqrtz(temp)
     do  j=i, k
        tixf(n1) = dummr(j)/temp
        work1(n1) = 0.0
5968    n1 = n1 + 1
     end do
5970 end do
  go to 9994
9540 d1 = 2.0
  d2 = 6.0
  d3 = 1.0/sqrtz(d1)
  d4 = 1.0/sqrtz(d2)
  do j = 1, 36
     if ( j .gt. 9 )  go to 15975
     tixf(n1) = d4
     go to 25960
15975 if ( j .gt. 12 )  go to 15980
     tixf(n1) = -d4
     go to 25960
15980 if ( j .gt. 13 )  go to 15985
     tixf(n1) = d3
     go to 25960
15985 if ( j .gt. 14 )  go to 15990
     tixf(n1) = -d3
     go to 25960
15990 if ( j .gt. 18 )  go to 15991
     tixf(n1) = 0.0
     go to 25960
15991 if ( j .gt. 20 )  go to 15992
     tixf(n1) = d4
     go to 25960
15992 if ( j .gt. 21)  go to 15993
     tixf(n1) = - d1 * d4
     go to 25960
15993 if ( j .gt. 27)  go to 15994
     tixf(n1) = 0.0
     go to 25960
15994 if ( j .gt. 28 )  go to 15995
     tixf(n1) = d3
     go to 25960
15995 if ( j .gt. 29 )  go to 15996
     tixf(n1) = -d3
     go to 25960
15996 if ( j .gt. 33 )  go to 15997
     tixf(n1) = 0.0
     go to 25960
15997 if ( j .gt. 35 )  go to 15999
     tixf(n1) = d4
     go to 25960
15999 tixf(n1) = - d1 * d4
25960 work1(n1) = 0.0
     n1 = n1 + 1
25965 end do
9994 write(lunit9) (tixf(i),i=1,n9sq), (work1(i),i=1,n9sq)
9996 if(j56.eq.0) go to 600
  read (lunt13)  (p(i),z(i),i=1,kp)
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunt13
  go to 203
  !                          end of routines for z-printing and inversions
  !
  !                                routines for p-printing and inversions
36 i2=1
  !     inverse susceptance or capacitance matrix for physical conductors
  !     is in p(1),...p(ip).
  if (iw.eq.0) i2 = i2+1
  call output ( metrik, p(1), z(1), switch, kfull, i2, ll1 )
  go to 31
37 i2=3
  if ( iprsup  .ge.  1 ) write (lunit6, 4444)  iw, ip, ll0, ll1
4444 format ( /,  28h at 4444.  iw, ip, ll0, ll1=, 4i10 )
  if (iw.eq.0) i2 = i2+1
  do i=1,ip
371  z(i)=-p(i)
  end do
  call redu44 ( z(1), workr1(1), kfull, ll0 )
  if( kill .gt. 0 )  go to 9200
  !     susceptance or capacitance matrix for physical conductors is in
  !     z(1),...z(ip).
  call output ( metrik, z(1), p(1), switch, kfull, i2, ll1 )
  go to 32
38 i2=1
  !     inverse susceptance or capacitance matrix for equivalent phase
  !     conductors is in p(1),...p(kp)
  !     to store wc(inv) or c(inv) for modal analysis
  if ( iprsup .ge. 1 ) write (*,*) ' at s.n. 38,  p(1) =',  p(1)
  if ( imodal .le. 0 )  go to 403
  do i = 1, kp
404  xwc(i) = p(i)
  end do
403 if (iw.eq.0) i2=i2+1
  if ( j2out  .gt.  0 ) call output ( metrik, p(1), z(1), switch, kcirct, i2, ll2 )
  if ( iprsup  .ge.  4 ) write (lunit6, 2654)  j3, kcirct, j56, kp, iw, i2, j2, ( p(i), i=1, kp )
2654 format ( /,  36h before 'c'-matrix call to  'symm' ., 32h      j3  kcirct     j56      kp, &
          24h      iw      i2      j2  ,/,  36x,  7i8  ,/, 29h (p(i), i=1, kp)  follow ....  ,/,  ( 1x,  8e16.7 )  )
  if(j3.eq.0 .or. kcirct.lt.2) go to 33
  if (j56.eq.0) go to 40
  write (lunt13)   ( p(i), i=1, kp )
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind lunt13
40 call symm ( p(1), z(1), switch, kcirct, kk )
  i2=1
  if (iw.eq.0) i2=i2+1
  !     inverse susceptance or capacitance matrix for symmetrical compo-
  !     nents of equivalent phase conductors (grouped as 3-phase circuits
  !     in order 1-2-3, 4-5-6 etc.) is in p(1),...p(kk*(kk+1)/2) for real
  !     part and z(1),...z(kk*(kk+1)/2) for imaginary part with kk=highest
  !     number of equivalent conductor being a multiple of 3.
  if(ik.le.0) call output ( metrik, p(1), z(1), unity, kk, i2, ll3 )
  if (j56.eq.0) go to 33
  read (lunt13)  d1
  read (lunt13)  (p(i),i=1,kp)
  go to 33
39 i2=3
  if(iw.eq.0) i2=i2+1
  do i=1,kp
391  p(i) = -p(i)
  end do
  call redu44 ( p(1), workr1(1), kcirct, ll0 )
  if( kill .gt. 0 )  go to 9200
  !     susceptance or capacitance matrix for equivalent phase conductors
  !     is in p(1),...p(kp).
  if ( imodal .eq. 1 )  go to 1378
  if ( iprsup .lt. 1 )  go to 5124
  write (*,*) ' ready for [c] avg.  p(1:6) =', ( p(i), i=1, 6 )
  write (lunit6,*) ' begin averaging of capacitance.'
  write (lunit6,*) ' 1st diagonal,  p(1) =',  p(1)
  write (lunit6,*) ' 1st off-diagonal,  p(2) =',  p(2)
  write (lunit6,*) ' =======  iw, kcirct, freq =', iw, kcirct, freq
5124 cdiag = 0
  coff = 0
  n55 = 1
  if (mspedb .ne. 1 )  go to 5190
  ccoup = 0.
  do i = 1, 6
     cdiag = cdiag + p(n55)
     if ( n55 .eq. 10 )  go to 5128
     jm = n55 - i + 1
     if ( n55 .ge. 15 )  jm = jm + 3
     do jp= jm, n55-1
        coff = coff + p(jp)
5125 end do
5128 n55 = n55 + i + 1
5130 end do
  cdiag = cdiag / 6
  coff = coff / 6
  if ( iprsup .ge. 1 ) write (lunit6,*) ' cdiag, coff are ', cdiag, coff
  n55 = 6
  do jp = 4, 6
     j = n55 + 1
     do i = j, j+2
        ccoup = ccoup + p(i)
5140 end do
     n55 = n55 + jp
5150 end do
  ccoup = ccoup / 9
  dummi(1) = cdiag + 2*coff + 3*ccoup
  dummi(2) = dummi(1) - 6*ccoup
  dummi(3) = cdiag - coff
  if ( iprsup .ge. 1) write (lunit6,*) ' at 5150, dummi =', (dummi(i),i= 1,3)
  if ( iw .eq. 0 )  go to 1378
  d13 = twopi * freq
  do i = 1,3
     dummi(i) = dummi(i) * d13
1260 end do
  go to 1378
5190 if ( kcirct .lt. 2 )  go to 1379
  do i=1, kcirct
     cdiag = cdiag + p(n55)
     do jp=n55-i+1, n55-1
379     coff = coff + p(jp)
     end do
378  n55 = n55 + i + 1
  end do
  cdiag = cdiag / kcirct
  coff = coff / ( kcirct * ( kcirct - 1 ) / 2 )
  !     write (*,*) ' average of diagonals,  cdiag =',  cdiag
  !     write (*,*) ' average of off-diag,   coff =',   coff
  dummi(1) = cdiag + ( kcirct - 1 ) * coff
  dummi(2) = cdiag -  coff
  go to 2379
1379 dummi(1) = p(1)
  dummi(2) = 0.
2379 if ( iw .eq. 0 )  go to 1378
  d13 = twopi * freq
  dummi(1) = dummi(1) * d13
  dummi(2) = dummi(2) * d13
1378 if ( j5out  .gt.  0 ) call output ( metrik, p(1), z(1), switch, kcirct, i2, ll2 )
  zero = 0.0
  if ( ialter  .le.  0 )   go to 3741
  write (lunit3)  ( zero, jj=1, kp )
  write (lunit3)  ( p(jj), jj=1, kp )
  if ( iprsup  .le.  0 )   go to 3741
  write (lunit6, 7428)  ( p(jj),  jj=1, kp )
7428 format ( /, 26h output  (c)  for semlyen.  ,/, ( 1x, 8e16.4 ) )
3741 if ( idist  .eq.  0 ) go to 393
  do i=1,kp
392  yd(i)=p(i)
  end do
393 if(j6.eq.0 .or. kcirct.lt.2) go to 80
  call symm ( p(1), z(1), switch, kcirct, kk )
  i2=3
  if(iw.eq.0) i2=i2+1
  !     susceptance or capacitance matrix for symmetrical components of
  !     equivalent phase conductors is in p(1),...p(kk*(kk+1)/2) for real
  !     part and z(1),...z(kk*(kk+1)/2) for imaginary part. definition of
  !     kk same as for inverse susceptance matrix
  if ( j6out  .gt.  0 ) call output ( metrik, p(1), z(1), unity, kk, i2, ll3 )
  if (ipunch .lt.  88 )   go to 399
  if (ipunch .gt.  89 )   go to 399
  if (ik .gt. 0)   go to 399
  if (liu .ne. 0)   go to 399
  czero = p(1)/ tenm6
  cpos = p(ll5) / tenm6
  if (iw .ne. 0)  go to 394
  czero = czero/omega
  cpos = cpos / omega
394 if (ipunch .ne. 88)  go to 395
  if ( metrik .eq. 1 )  czero = czero * fmipkm
  write (lunit9)   czero
  if (iprsup .ge. 1) write (lunit6, 98765)  czero
98765 format (15h czero on unit9, e15.6)
  go to 396
395 if (metrik .eq. 1 )  cpos = cpos * fmipkm
  write(lunit9)  cpos
  if (iprsup .ge. 1) write (lunit6, 397)  cpos
397 format ( 14h cpos on unit9, e15.6)
396 liu = 1
399 yzero = p(1)
  ypos = p(ll5)
  if(iw.eq.0) go to 80
  yzero=yzero*omega
  ypos=ypos*omega
  go to 80
  !                          end of routines for p-printing and inversions
  !
  !                                        changes in conductor data
500 icount=0
  k=0
  if(kcirct.eq.0) go to 503
  i2 = lphpl1 - kcirct
  i = lphase
  kk=-1
501 k=k+1
  itbic(k)=ic(i)
  tbtb2(k)=tb2(i)
  itbtb3(k)=itb3(i)
  tbr(k)=r(i)
  tbd(k)=d(i)
  tbg(k)=gmd(i)
  tbx(k)=x(i)
  tby(k)=y(i)
  tbtext(k)=text(i)
  if(i.eq.i2) go to 502
  i=i+kk
  go to 501
502 if(icount.gt.0) go to 504
503 icount=1
  i=1
  i2=ktot
  kk=1
  if(ktot.gt.0) go to 501
  !     read input card using cimage
504 call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
  read (unit = abuff, fmt = 11) i1, y1, r1, i2, g1, d1, x1, h1, h2, d8, d9, bus1, i3
  if( i1 .ne. 0 )  go to 4423
  if( y1 .ne. 0 )  go to 4423
  if( r1 .ne. 0 )  go to 4423
  if( i2 .ne. 0 )  go to 4423
  if( g1 .ne. 0.0 )  go to 4423
  if( d1 .ne. 0.0 )  go to 4423
  if( x1 .ne. 0.0 )  go to 4423
  if( h1 .ne. 0.0 )  go to 4423
  if( h2 .ne. 0.0 )  go to 4423
  if( d8 .ne. 0.0 )  go to 4423
  if( d9 .ne. 0.0 )  go to 4423
  if( bus1 .eq. blank )  go to 507
4423 isw = 1
  i = 0
1505 i = i +1
  if (i .gt. kfull)   go to 505
  if ( tbtext(i)  .eq.  bus1 )  go to 506
  go to 1505
505 if (isw .eq. 2)   go to 504
  kfull=kfull+1
  if( kfull .gt. lphase )  go to 7
  i=kfull
  itbic(i) = i1
  tbtb2(i) = y1
  tbr(i) = r1
  itbtb3(i) = i2
  tbg(i) = g1
  tbd(i) = d1
  tbx(i) = x1
  tbtext(i) = bus1
  assign 711 to moon
  go to 4280
506 continue
  read (unit = abuff(1), fmt = 4281) bus1
  if( bus1  .ne.  blank )  itbic(i) = i1
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
600 if(dist.eq.0.) go to 16
  j1516=j15+j16
  if(j13+j14+j1516.eq.0) go to 16
  if( metrik .eq. 1 ) go to 610
  write(lunit6,601) dist
601 format (/, 27h matrices for line length =, e13.6, 7h miles.)
  go to 612
610 distm = dist / fmipkm
  write( lunit6,611 ) distm
611 format( /, 27h matrices for line length =, f8.3,   4h km. )
612 continue
  if(idist.eq.1) go to 603
  write(lunit6,602) kcirct
602 format(1h+,41x,58hcannot be calculated with number of equivalent conductors=,i3)
  go to 16
603 ip=0
  d1 = fltinf
  k=0
604 k=k+1
  if(k.gt.kcirct) go to 605
  ip=ip+k
  h1= valu6*bd(ip)/yd(ip)
  if(absz(h1).lt.d1) d1=absz(h1)
  go to 604
605 if(iw.gt.0) d1=d1/omega
  d1=sqrtz(d1*2.0)
  i1=dist/d1+ onehaf
  i2=1
  do isec=1,34
     if(i1.le.i2) go to 608
606  i2=i2*2
  end do
  write(lunit6,607)
607 format(1h+,41x,60hcannot be calculated with number of necessary sections=2**33)
  go to 16
608 x1=i2
  i1=isec-1
  deltad=dist/x1
  if( metrik .eq. 1 ) go to 613
  write(lunit6,609) i1,deltad
609 format (38h computed by connecting in cascade 2**,i2,18h equal sections of, e13.6, 12h miles each.)
  go to 615
613 deltam = deltad / fmipkm
  write( lunit6,614 ) i1, deltam
614 format(1h+,41x,36hcomputed by connecting in series 2**,i2,18h equal sections of,f9.4,9h km each. )
615 continue
  f1=unity/deltad
  f2=deltad* onehaf
  if(iw.gt.0) f2=omega*f2
  kp=kcirct*(kcirct+1)/2
  kcir2=kcirct+kcirct
  if (i1 .eq. 0)  go to 670
  do i=1,kp
     r1=f1*gd(i)
     x1=f1*bd(i)
     g1=f2*yd(i)
     d1=x1+g1
     p(i)=r1
     z(i)=d1
     gd(i)=r1+r1
621  bd(i)=x1+d1
  end do
  !                                  begin of loop for connecting sections
  icount=0
622 icount=icount+1
  if(icount.eq.isec) go to 650
  !                                       expanding matrix
  ip=0
  i3=kp
  k=0
623 k=k+1
  if(k.gt.kcirct) go to 626
  i2=i3+kcirct
  l4=kp+k
  i=0
624 i=i+1
  l1=ip+i
  l2=i2+i
  l3=i3+i
  r1=p(l1)
  x1=z(l1)
  h1=r1-gd(l1)
  h2=x1-bd(l1)
  p(l2)=r1*2.0
  z(l2)=x1*2.0
  p(l3)=h1
  z(l3)=h2
  gd(l1)=r1
  bd(l1)=x1
  if(i.eq.k) go to 625
  p(l4)=h1
  z(l4)=h2
  l4=l4+i+kcirct
  go to 624
  625 ip=ip+k
  i3=i2+k
  go to 623
626 call cxred2( p(1), z(1), workr1(1), workr2(1), kcir2, kcirct )
  go to 622
  !                                       output of matrices for length
650 do i=1,kp
     gd(i)=gd(i)-p(i)
     bd(i)=bd(i)-z(i)
     p(i) = (p(i) - gd(i)) * 2.
651  z(i) = (z(i) - bd(i)) * 2.
  end do
673 if (j1516 .gt. 0)  go to 660
652 if(j13.eq.0) go to 653
  !     transfer admittance matrix for equivalent cond. is in gd+j*bd.
  call output ( ll0, gd(1), bd(1), unity, kcirct, ll7, ll2 )
  !     shunt admittance matrix for equivalent cond. is in p+j*z.
  call output ( ll0,  p(1),  z(1), unity, kcirct, ll8, ll2 )
653 if(j14.eq.0) go to 654
  call symm(gd(1), bd(1), unity, kcirct, kk)
  !     transfer admittance matrix for symmetrical comp. is in gd+j*bd.
  call output ( ll0, gd(1), bd(1), unity, kk, ll7, ll3 )
  call symm ( p(1), z(1), unity, kcirct, kk )
  !     shunt admittance matrix for symmetrical comp. is in p+j*z.
  call output ( ll0,  p(1),  z(1), unity, kk, ll8 , ll3 )
654 if(j1516.eq.0) go to 16
  do i=1,kp
     n1 = 2 * lgdbd + 1 + i
     n2 = i + lgdbd
     gd(i) = -p(n2)
     bd(i) = -z(n2)
     p(i) = -p(n1)
655  z(i) = -z(n1)
  end do
  call cxred2 ( gd(1), bd(1), workr1(1), workr2(1), kcirct, ll0 )
  call cxred2 ( p(1), z(1), workr1(1), workr2(1), kcirct, ll0 )
  if(j15.eq.0) go to 656
  !     transfer impedance matrix for equivalent conductors is in gd+j*bd.
  call output ( ll0, gd(1), bd(1), unity, kcirct, ll9, ll2 )
  !     shunt impedance matrix for equivalent conductors is in p+j*z.
  call output ( ll0,  p(1),  z(1), unity, kcirct, ll10, ll2 )
656 if(j16.eq.0) go to 16
  call symm ( gd(1), bd(1), unity, kcirct, kk )
  !     transfer impedance matrix for symmetrical comp. is in gd+j*bd.
  call output ( ll0, gd(1), bd(1), unity, kk, ll9, ll3 )
  call symm ( p(1), z(1), unity, kcirct, kk )
  !     shunt impedance matrix for symmetrical comp. is in p+j*z.
  call output ( ll0,  p(1),  z(1), unity, kk, ll10, ll3 )
  go to 16
660 i = lgdbd + 1
  call mover ( gd(1), p(i), kp )
  call mover ( bd(1), z(i), kp )
  i = 2 * i
  call mover ( p(1), p(i), kp )
  call mover ( z(1), z(i), kp )
  go to 652
670 f2 = f2 * 2.
  do i=1, kp
     p(i) = 0.
     z(i) = f2 * yd(i)
     gd(i) = f1 * gd(i)
671  bd(i) = f1 * bd(i)
  end do
  go to 673
9200 lstat(18) = nchain
  if ( ialter  .eq.  2 ) lunit5 = l5save
  lastov = nchain
  nchain = 51
9900 return
end subroutine guts44
!
! subroutine punpie.
!
subroutine punpie ( kcirct )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labl44.ftn'
  include 'deck44.ftn'
  if ( iprsup .ge. 1 )  write (*,*) ' top "punpie".'
  write (lunit7, 1201)
1201 format ( 11h$vintage, 1)
  j = 1
  do i = 1, kcirct
     k = i + j - 1
     do kk = j, k
        yd(kk) = yd(kk) / tenm6
        if ( kk .eq. j ) write (lunit7, 5201) i, brname(2*i-1), brname(2*i), p(kk), z(kk), yd(kk)
5201    format ( i2, 2a6, 12x, 3e16.5)
        if ( kk .ne. j) write (lunit7, 6201) p(kk), z(kk), yd(kk)
6201    format ( 26x, 3e16.5)
7201 end do
     j = k + 1
8201 end do
  write (lunit7, 2201)
2201 format ( 11h$vintage, 0)
  if ( iprsup .ge. 1 )  write (*,*) ' exit "punpie".'
  return
end subroutine punpie
!
!     subroutine modal.
!
subroutine modal(array, xwc, xwy, yzr, yzi, tii, tir, tvi, tvr, er, ei, theta2, xtir, xtii, zsurge, dummi, dummr, tixf, &
     work1, freq, m, iw, dist, metrik, fmipkm, ndim, ntri, nsqr2, itrnsf, kfull, mrr, nrp, ntol, conduc)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labl44.ftn'
  include 'deck44.ftn'
  common /linemodel/ kexact, nsolve, fminsv, numrun, nphlmt
  common /linemodel/ char80, chlmfs(18)

!  common /linemodel/ kexact, nsolve, fminsv, kbrnum, numrun
  character*8 text1, text2, text3, text4, text5, text6
  character*8 text7, text8, text9, text10
  dimension array(1)
  dimension xwc(ntri),xwy(ntri),yzi(ndim,ndim),yzr(ndim,ndim)
  dimension tii(ndim,ndim),tir(ndim,ndim),tvi(ndim,ndim)
  dimension er(ndim),ei(ndim),theta2(ndim),tvr(ndim,ndim)
  dimension xtir(ndim),xtii(ndim),zsurge(ndim),dummi(ndim)
  dimension dummr(ndim),tixf(nsqr2),work1(nsqr2)
  dimension ping(200),iseq(15),kmax(15)
  dimension pp1(20,20), pp2(20,20), ee1(15), ee2(15)
  dimension tempr(20,20), tempi(20,20)
  data text2  /  1ha  /
  data text3  /  1hb  /
  data text4  /  1hc  /
  data text5  /  1hd  /
  data text6  /  1he  /
  data text7  /  1hf  /
  data text8  /  1hg  /
  data text9  /  1hh  /
  data text10 /  1hi  /
  data iseq  /  15*0  /
  if ( kexact .eq. 88333 )  go to 100
  if (itrnsf .eq. 1  .and. nfreq .eq. 1) return
  if (itrnsf .eq. 1  .and. nfreq .eq. 2) return
  if (lastov .eq. 39 .and. nfreq .eq. 2) return
100 if ( iprsup .ge. 1 ) write (lunit6,*) ' top of modal.   ndim =',  ndim
  ll0 = 0
  mm=m*(m+1)/2
  c=unity
  if (iprsup .ge. 1 ) write (lunit6, 300) m, mm, (p(i),z(i), i=1, mm)
300 format ( 34h at the top of modal, m and mm are, 2i10, /,  28h (p(i),  z (i), i=1, mm) are,/, (1x, 8e15.6) )
  d13 = twopi * freq
  if ( iw .eq. 1 ) c = c / d13
  do i=1,mm
     xwc(i) = xwc(i) * c
27   xwy(i)=-xwc(i)
  end do
  call redu44(xwy(1),workr1(1) ,m,ll0)
  !    const. [t], marti setup will bypass the exact diagonalization
  !     at each looping frequency
  if (nfreq .ne. 1  .and.  lastov .eq. 39 .and. itrnsf .ne. 1) go to 2520
  if ( lastov .eq. 39 .and. nfreq .eq. 3 )  go to 7030
  if ( kexact .eq. 88333 )  go to 7030
  write(lunit6,45)freq
  write(lunit6,44)
  if(metrik.eq.1) go to 7029
  write(lunit6,65)
  go to 7030
7029 continue
  write(lunit6,7031)
7030 continue
45 format(//,28h modal parameters at freq = ,e13.5,3h hz)
  ! ***** create y * z matrix from array from lcp main
  k=0
3 k=k+1
  do j=1,m
     !     n=max0z(j,k)
     n = j
     if ( n .lt. k )  n = k
     n=n*(n-3)/2+j+k
     er(j)=p(n)
50   ei(j)=z(n)
  end do
  ! **** the k-th column of r+xwl is stored
  i=0
2 i=i+1
  cr=0.0
  cl=0.0
  do j=1,m
     !     n=max0z(j,i)
     n = j
     if ( n .lt. i )  n = i
     n=n*(n-3)/2+j+i
     cc=xwy(n)
     if ( lastov .eq. 1 )  conduc = 0.0
     cr=cr+cc*er(j)+conduc*ei(j)
1    cl=cl-cc*ei(j)+conduc*er(j)
  end do
  ! **** the element of row i and column k of yz is obtained
  yzr(i,k)=cl
  yzi(i,k)=cr
  if (iprsup .ge. 1 ) write (lunit6, 350) i,k,yzr(i,k),yzi(i,k)
350 format ( 46h       i       k       yzr(i,k)       yzi(i,k) ,/,  2i8,  2e15.6 )
  if(i.lt.m)go to 2
  ! **** yz is obtained in order of (1,1),(2,1),...,(1,2),(2,2),columnwise
  if(k.lt.m)go to 3
  ! *** find eigenvalues + eigenvectors ***
  if ( kexact .eq. 88333 )  go to 6999
  if ( itrnsf .eq. -1 )  go to 459
  if ( lastov .eq. 1 .or. itrnsf .ne. 1)  go to 7011
  ! $$$$  change to zy-matrix i.e. transformation matrix is tv   $$$$
6999 do i=1,m
     do j=1,m
        tempr(i,j)=yzr(j,i)
        tempi(i,j)=yzi(j,i)
7001 end do
  end do
  do i=1,m
     do j=1,m
        yzr(i,j) = tempr(i,j)
        yzi(i,j) = tempi(i,j)
7008 end do
  end do
  call dceign(yzr,yzi,tvi,tvr,er,ei,m,m,ierror,ll1,ll0,lunit6, iprsup,ndim)
  !      write(*,*)(er(i), i=1,m)
  !      write(*,*)(ei(i), i=1,m)
  go to 7012
7011 call dceign(yzr,yzi,tii,tir,er,ei,m,m,ierror,ll1,ll0,lunit6, iprsup,ndim)
  !      write(*,*)(er(i), i=1,m)
  !      write(*,*)(ei(i), i=1,m)
  go to 7072
7012 kthl=1
  ping(kthl)=alog1z(freq)
  d55 = 1.0 / freq
  d56 = 1.0 / sqrtz( freq )
  do i=1,m
     em=sqrtz( er(i)*er(i)+ei(i)*ei(i) )
     ea=atan2z(ei(i),er(i))
     p(i) = sqrtz(em)
     z(i) = ea /2
     if ( kexact .eq. 88333 )  go to 5600
     cc=sqrtz(em)*cosz(ea/2.)
     d14=sqrtz(em)*sinz(ea/2.)
     vmode=d13/d14
     ! $$$$  put data in ping vector  $$$$
     ping(kthl+1) = em*d55
     ping(kthl+2) = ea/(twopi/360.)
     ping(kthl+3) = vmode
     ping(kthl+4) = cc*d56
     kthl = kthl + 4
5600 end do
  if ( iprsup .lt. 1 )  go to 5602
  write(lunit6,*) ' tv before unwind. '
  do i = 1, m
     write (lunit6,59) ( tvr(i,j), j=1,m )
     write( lunit6,59) ( tvi(i,j), j=1,m )
2411 end do
5602 if ( kexact .eq. 88333 )  go to 5620
  call unwind( ping,kthl,mrr,nrp,ntol,iseq )
  if ( iprsup .lt. 1 )  go to 5605
  write(lunit6, *) ' iseq after unwind are, ', (iseq(i),i=1,m)
  write(lunit6, *) ' ping after unwind are, ', (ping(i),i=1,kthl)
5605 continue
  ! $$$$  begin the process of eigenvectors  $$$$
  do i=1,m
     ee1(i)=p(i)
     ee2(i)=z(i)
     do j=1,m
        ! $$$$  use tv  $$$$
        pp1(i,j)=tvr(i,j)
        pp2(i,j)=tvi(i,j)
5613 end do
  end do
  do i=1,m
     p(i)=ee1(iseq(i))
     z(i)=ee2(iseq(i))
     do j=1,m
        ! $$$$ use tv  $$$$
        tvr(i,j)=pp1(i,iseq(j))
        tvi(i,j)=pp2(i,iseq(j))
5612 end do
  end do
  ! $$$$ new code to get ti from tv-t $$$$
5620 ll=1
  do i=1,m
     do j=1,m
        tixf(ll)=tvr(i,j)
        tixf(ll+1)=tvi(i,j)
        ll=ll+2
7003 end do
7002 end do
  call cominv(tixf(1),work1(1),m,freq)
  ll=1
7766 if (iprsup .ge. 3) write(lunit6,*) ' before do 7004.  m, ndim, freq =', m, ndim, freq
  do j=1,m
     do i=1,m
        tir(i,j)=work1(ll)
        tii(i,j)=work1(ll+1)
        ll=ll+2
7005 end do
7004 end do
  if ( kexact .eq. 88333 )  go to 7072
  do i=1,m
     do j=1,m
        pp1(i,j)=sqrtz( (tir(i,j))**2+(tii(i,j))**2 )
        pp2(i,j)=atan2z( tii(i,j) , tir(i,j) )
5621 end do
5611 end do
  !
  if ( ntol.ne.1 ) go to 5812
  do j=1,m
     kmax(j)=1
     do i=1,m
        if ( pp1(i,j) .le. pp1(kmax(j),j) ) go to 5802
        kmax(j)=i
5802 end do
5801 end do
  write (lunit9) ( kmax(j), j = 1,m )
5812 do j=1,m
     if ( pp1(kmax(j),j) .eq. 0.0 ) go to 5614
     dv=1.0/pp1(kmax(j),j)
     da=pp2(kmax(j),j)
     if (iprsup .ge. 3) write(*,*) 'in do loop 5614, j=, kmax(j)=,dv=,da=', j, kmax(j), dv, da
     do i=1,m
        if (iprsup .ge. 3) write(*,*) 'in do loop 5624, i, j, pp1(i,j), pp2(i,j)', i, j, pp1(i,j), pp2(i,j)
        pp1(i,j)=pp1(i,j)*dv
        pp2(i,j)=pp2(i,j)-da
        !     *** to keep angles within principal value region
        if ( pp2(i,j) .gt. twopi/2. )  pp2(i,j)=pp2(i,j) - twopi
        if ( pp2(i,j) .lt.-twopi/2. )  pp2(i,j)=pp2(i,j) + twopi
        if (iprsup .ge. 3) write(*,*) 'just before 5624, i, j, pp1(i,j), pp2(i,j)', i, j, pp1(i,j), pp2(i,j)
5624 end do
5614 end do
!!    ** fix the error of using  ti as tv  **
  do i=1,m
     do j=1,m
        tir(i,j)=(pp1(i,j))*cosz(pp2(i,j))
        tii(i,j)=(pp1(i,j))*sinz(pp2(i,j))
6001 end do
  end do
7120 if ( iprsup .ge. 1 ) write (lunit6, 400) freq, ((tir(i,j),tii(i,j),j=1,m),i=1,m)
400 format (' at ', e12.5, 'Hz, eigenvectors tir and tii after scaling are ' ,/,   (1x, 8e15.6) )
7072 if(ierror.eq.0)go to 463
  kill = 221
  lstat(19) = 72
  lstat(14) = ierror
  go to 9200
459 do j = 1, m
     do i = 1, m, 3
        ij = i + 2
        call cimage
        read (unit = abuff(1), fmt = 469) (tir(j, ibk), tii(j, ibk), ibk = i, ij)
469     format( 6e13.0 )
462  end do
  end do
  !     !   $$$$ skip the rotation and normalization  for freq-dep [t] ??
463 if( lastov.eq.39 .and. itrnsf.eq. 1  .or. kexact .eq. 88333) go to 2500
  if ( itrnsf .eq. -9  .or.  itrnsf .eq. -1 )  go to 464
  if ( iprsup .lt. 4 ) go to 88
  write (lunit6, 1462)
1462 format( 65h current transformation matrix before zeroing the imaginary part: )
  write (lunit6, 39)
  do  k = 1, m
     write(lunit6,9) (tir(k,j),j=1,m)
1472 end do
  write (lunit6, 60)
  do k = 1, m
     write (lunit6,9) (tii(k,j),j=1,m)
1482 end do
88 continue
  do j = 1, m
     cr = 0.
     fnorm = 0.
     do  i = 1, m
        fnorm = fnorm +(tir(i,j)**2 - tii(i,j)**2)
        cr = cr + tir(i,j) * tii(i,j)
466  end do
     cr = -2.0 * cr
     theta = atan2z( cr, fnorm) / 2.
     cr = cosz( theta )
     cl = sinz( theta )
     do i = 1, m
        tir(i,j) = tir(i,j) * cr - tii(i,j) * cl
467     tii(i,j) = 0.
     end do
465 end do
464 do i = 1, m
     fnorm=0.0
     do j=1,m
26      fnorm=fnorm+tir(j,i)**2+tii(j,i)**2
     end do
     fnorm=1./sqrtz(fnorm)
     do j=1,m
        tii(j,i)=tii(j,i)*fnorm
        tir(j,i)=tir(j,i)*fnorm
5    end do
  end do
  ! $$$$  calculation of tv from ti  $$$$
2500 ll=1
  do i=1,m
     do j=1,m
        tixf(ll)=tir(i,j)
        tixf(ll+1)=tii(i,j)
        ll=ll+2
6003 end do
6002 end do
  call cominv(tixf(1),work1(1),m,freq)
  ll=1
  do j=1,m
     do i=1,m
        tvr(i,j)=work1(ll)
        tvi(i,j)=work1(ll+1)
        ll=ll+2
6005 end do
6004 end do
  !!    ** now, we have both ti and tv **
  if ( iprsup .ge. 1 ) write (lunit6, 403)  ((tvr(i,j),tvi(i,j),j=1,m),i=1,m)
403 format (' eigenvectors tvr and tvi are ',/,(1x, 8e15.6))
  if (lastov .eq. 39 .and. itrnsf .ne. 1 )  go to 166
  if ( lastov  .ne.  1 )   go to 2520
  write (lunit7, 8769)  tclock, date1
8769 format (    "!    Punched output of K. C. Lee's line which began at ",  2x,  2a4,  2x,  2a4  )
  write (lunit7, 8770)  freq
8770 format ( "c  ***** Untransposed K. C. Lee line segment calculated at ", 2x, e10.3,  ' Hz. *****')
  rewind lunit2
  n5 = kfull+nfreq+1
  do n12=1, n5
     read (lunit2, 8771, end=8775)  ( texta6(i), i=1, 14 )
8771 format ( 13a6, a2 )
     write (lunit7, 8772)  ( texta6(i), i=1, 13 )
8772 format ( 2hc ,  13a6 )
8774 end do
 8775 write (lunit7, 28)
28 format (11h$vintage, 1)
  ! ****** to get modal y *********
2520 do i=1,m
     do j=1,m
        !     $$$ now, using tv to get ymode $$$
        xtir(j)=tvr(j,i)
        xtii(j)=tvi(j,i)
41   end do
     call mult(xwy,xtir,dummi,m,ll0)
     b = 0.0
     call mult(xwy,xtii,dummr,m,ll0)
     ! *** xwy  is j(wc) ******
     do j = 1, m
        dummr(j) = - dummr(j)
3141 end do
     do j = 1, m
        dummi(j) = dummi(j) + conduc * xtii(j)
        dummr(j) = dummr(j) + conduc * xtir(j)
3151 end do
     g = 0.0
     do ig=1,m
        c= dummr(ig)
        d1= dummi(ig)
        xa=xtir(ig)
        xb=xtii(ig)
        b=b+xa*d1+xb*c
43      g=g+xa*c-xb*d1
     end do
     if ( iprsup .ge. 1 ) write (lunit6, *) ' modal admittance ym for mode ', i, g, b
44   format(//, ' mode   resistance  reactance  susceptance surge impedance(Ohm)          velocity  attenuation')
65   format('          Ohm/mile    Ohm/mile   S/mile        real     imag       lossless     mile/sec  neper/mile')
7031 format('          Ohm/km      Ohm/km     S/km         real     imag      lossless     km/sec     neper/km')
33   format(i5,3x,9e12.5)
     ! to get rotation for  y=0+xwc
     ya = atan2z(b,g)
     theta=pi*onehaf- ya
     theta2(i)=-theta*onehaf
     dumm1=sqrtz(g*g+b*b)
     continue
     ! **** to get zmodal, zsurge, vmode from ymodal & eigenvalues ****
     if ( lastov .eq. 1 )  go to 2900
     !!  ** new coding **
     if ( itrnsf .ne. 1 .and. kexact .ne. 88333 )  go to 7041
     gamma = p(i)
     ea = z(i)
1232 ycharm = dumm1 / gamma
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
7033 alpha = alpha * fmipkm
     beta = beta * fmipkm
     go to 155
7041 zr = 0.0
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
2600       end do
           dummr(l) = cr
           dummi(l) = ci
2700    end do
        zr = zr + tixf(kk)*dummr(jj) - tixf(kk+1)*dummi(jj)
        zi = zi + tixf(kk)*dummi(jj) + tixf(kk+1)*dummr(jj)
2800 end do
2899 dr = zr * g - zi * b
     di = zr * b + zi * g
     em = sqrtz( dr*dr + di*di )
     gamma = sqrtz( em )
     ea = atan2z( di, dr ) / 2.
     ym=sqrtz( g**2 + b**2 )
     ya=atan2z( b, g )
     go to 1232
2900 g=ei(i)/dumm1
     b=-er(i)/dumm1
58   format(2x,10e11.4)
2950 zsurge(i)=sqrtz(b/dumm1)
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
137  write(lunit6,33)i,g,b,dumm1,zcharr,zchari,zsurge(i),vmode,cc
     distm = -dist
     if ( m  .lt.  10     .and.     lastov  .eq.  1 ) write (lunit7,140) i, brname(2*i-1), brname(2*i), g,zsurge(i),vmode,distm,m
140  format (1h-,i1,2a6,12x,4e12.5,2h 1,2x,i1)
     if ( m .ge. 10  .and.  lastov .eq. 1)  go to 141
     go to 40
141  if ( m .eq. 10 )  text1 = text2
     if ( m .eq. 11 )  text1 = text3
     if ( m .eq. 12 )  text1 = text4
     if ( m .eq. 13 )  text1 = text5
     if ( m .eq. 14 )  text1 = text6
     if ( m .eq. 15 )  text1 = text7
     if ( m .eq. 16 )  text1 = text8
     if ( m .eq. 17 )  text1 = text9
     if ( m .eq. 18 )  text1 = text10
     if ( i .le. 9 ) write (lunit7,143) i, brname(2*i-1), brname(2*i), g,zsurge(i),vmode,distm,text1
143  format (1h-,i1,2a6,12x,4e12.5,2h 1,2x,a1)
     if ( i .gt. 9 ) write (lunit7,145) i, brname(2*i-1), brname(2*i), g,zsurge(i),vmode,distm,text1
145  format (i2,2a6,12x,4e12.5,2h 1,2x,a1)
     go to 40
155  write(lunit9) d13,ycharm,ychara,alpha,beta
     if ( iprsup .ge. 1 ) write (lunit6, 158) d13, i, ycharm,ychara,alpha,beta
158  format (' at ', e12.5,' hz,ycharm,ychara,alpha,beta for mode',  i5, ' are', 5x, 4e12.5)
     if ( kexact .eq. 88333 )  go to 40
     if ( itrnsf .ne. 1 )  go to 40
     write(lunit9) (pp1(j,i),pp2(j,i), j=1,m )
40 end do
  if ( kexact .ne. 88333 )  go to 119
  ll = 1
  do j = 1, m
     do ij = 1, m
        tixf(ll) = tir(ij,j)
        work1(ll) = tii(ij,j)
        ll = ll + 1
115  end do
  end do
  n9sq = m * m
  write(lunit9) ( tixf(k),k=1,n9sq), (work1(k),k=1,n9sq)
  go to 9900
119 if ( lastov .eq. 1 )  write (lunit7,240)
240 format (11h$vintage, 0)
  if ( nfreq .ne. 1  .and.  lastov .eq. 39 )  go to 9900
  do i=1,m
     c=cosz(theta2(i))
     d1=sinz(theta2(i))
     do j=1,m
        cc=tir(j,i)
        dd=tii(j,i)
        tir(j,i)=cc*c-dd*d1
        tii(j,i)=cc*d1+dd*c
37   end do
  end do
  ! ***** end of rotation of ti matrix for k.c.lee's const.param.*******
39 format(//,33h      real components, row by row)
166 write(lunit6,66)
66 format(////, ' Eigenvector matrix ti for current transformation i (phase)=ti*i(mode)')
  write(lunit6,39)
9 format(2x,6e12.5)
  do k=1,m
     write(lunit6,9)(tir(k,j),j=1,m)
59   format (6f12.8)
57 end do
  write(lunit6,60)
60 format(//,37h     imaginary components, row by row)
  do k=1,m
     write(lunit6,9)(tii(k,j),j=1,m)
  end do
  do i =1, m
     if ( lastov .eq. 39 )  go to 161
     write (lunit7,59) ( tir(i,j), j=1,m )
     write (lunit7,59) ( tii(i,j), j=1,m )
     go to 1204
161  write (lunit9)   (tir(i,j), tii(i,j), j=1, m )
1204 end do
  if ( lastov .eq. 39 )  go to 9900
  ! to get zsurge matrix in phase domain
75 do i=1,m
     c = 1./zsurge(i)
     do j=1,m
80      yzr(i,j)=tir(j,i) * c
     end do
  end do
  n=0
  do i=1,m
     do k=1,i
        c = 0.0
        do j=1,m
81         c=c-tir(i,j)*yzr(j,k)
        end do
        n=n+1
82      xwc(n)=c
     end do
  end do
  call redu44(xwc(1),workr1(1),m,ll0)
  if( kill .ne. 0 ) go to 9200
  write(lunit6,83)
83 format(/,' phase domain zsurge (real; the imag. of ti ignored)')
  n=1
  do i=1,m
     k=n+i-1
     write(lunit6,9)(xwc(j),j=n,k)
84   n=n+i
  end do
  go to 9900
9200 lstat(18) = nchain
  lastov = nchain
  nchain = 51
9900 return
end subroutine modal
!
! subroutine cominv.
!
subroutine cominv(a,b,m,freq)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !   this subroutine performs inversion of a complex matrix
  include 'blkcom.ftn'
  include 'labl44.ftn'
  dimension a(1), b(1), ij(50)
  epspv2 = 1.0d-16
  do i=1, m
110  ij(i) = 0
  end do
  nph2 = m + m
  n22 = 2 * m * m
  nphpi2 = nph2 + 2
  do j=1, n22
120  b(j) = a(j)
  end do
  l = 0
130 l = l + 1
  if (l .gt. m) return
  t = 0.
  n1 = 1
  do j=1, m
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
  flstat(15) = freq
  flstat(13) = t
  flstat(14) = epspv2
  lstat(13) = l
  lstat(14) = m
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
     n1 = i + (n2 - 1)*m
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
end subroutine cominv
!
! subroutine dceign.
!
subroutine dceign(ar,ai,vi,vr,er,ei,n,nm,ierr,nv,nb,lunit6,iprsup,ndim)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension ar(ndim,ndim),ai(ndim,ndim),er(ndim),ei(ndim),vi(ndim,ndim),vr(ndim,ndim)
  dimension scale(20),int(20),iord(20)
  if ( iprsup .ge. 1 ) write (*,*) ' top of dceign.  input matrix ar ....'
  do i=1,n
     if ( iprsup .ge. 1 ) write(*,*) (ar(i,j), j=1,n)
2222 end do
  if ( iprsup .ge. 1 ) write (*,*) '                 input matrix ai ....'
  do i=1,n
     if ( iprsup .ge. 1 ) write(*,*) (ai(i,j), j=1,n)
2223 end do
  if ( n .gt. nm ) go to 90
  low=1
  nupp=n
  if(nb.eq.0) go to 1
  !  do balancing
  call cbal(nm,n,ar,ai,low,nupp,scale,ndim)
  !  do transformation to upper hessenberg form
1 continue
  if (iprsup .ge. 3 ) write (lunit6, 300) nm,n,low,nupp,nb,nv,((ar(i,j),ai(i,j),j=1, nm),i=1,nm)
300 format ( 49h before call comhes, nm, n, low, nupp, nb, nv are, 6i5,/, 38h ((ar(i,j),ai(i,j),j=1,nm),i=1,nm) are,/, (1x,8e15.6))
  call comhes(nm,n,low,nupp,ar,ai,int,lunit6,iprsup,ndim,iord)
  if(nv.eq.0) go to 12
  !  calculate values and vectors
  if ( iprsup .ge. 3 ) write (lunit6, 350) ((ar(i,j),ai(i,j),j=1,nm),i=1,nm)
350 format ( 43h after call comhes, ar(i,j) and ai(i,j) are,/, (1x, 8e15.6) )
  call comlr2(nm,n,low,nupp,int,ar,ai,vi,vr,er,ei,ierr,ndim,iord)
  if (iprsup .lt. 3 ) go to 410
  write (lunit6,400)  (er(i),ei(i),i=1,nm)
400 format ( 49h after call comlr2 in dceign, the eigenvalues are,/, ( 1x, 8e15.6), / )
  write (lunit6, 405)  ((vr(i,j),vi(i,j),j=1,nm),i=1,nm)
405 format ( 21h and eigenvectors are,/ (1x,8e15.6) )
410 if(ierr.ne.0) go to 5
  !  transform vectors to vectors of original matrix
  if(nb.eq.0) return
  call cbabk2(nm,n,low,nupp,scale,n,vi,vr,ndim)
  return
  !  calculate values only
12 continue
  call comlr(nm,n,low,nupp,ar,ai,er,ei,ierr,ndim)
  if(ierr.ne.0) go to 5
  return
  !  output error messages
5 write(lunit6,6) ierr
6 format ( 31h ***warning-dceign:  eigenvalue,  i4, 18h did not converge.)
  return
90 ierr=-1
  write(lunit6,91) n,nm
91 format(52h **error-dceign:  order of matrix greater than first, 20h dimension of matrix,/,18x,6horder=,i5, 17h first dimension=, i5)
  write (*,*) 'output matrix ar '
  do i=1,n
     write(*,*) (ar(i,j), j=1,n)
2224 end do
  write (*,*) 'output matrix ai '
  do i=1,n
     write(*,*) (ai(i,j), j=1,n)
2225 end do
  return
end subroutine dceign
!
! subroutine dceign.
!
subroutine cbal(nm,n,ar,ai,low,igh,scale,ndim)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension ar(ndim,ndim),ai(ndim,ndim),scale(20)
  !     this subroutine is a translation of the algol procedure
  !     cbalance, which is a complex version of balance,
  !     num. math. 13, 293-304(1969) by parlett and reinsch.
  !     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
  !
  !     this subroutine balances a complex matrix and isolates
  !     eigenvalues whenever possible.
  !
  !     on input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        ar and ai contain the real and imaginary parts,
  !          respectively, of the complex matrix to be balanced.
  !
  !     on output:
  !
  !        ar and ai contain the real and imaginary parts,
  !          respectively, of the balanced matrix;
  !
  !        low and igh are two integers such that ar(i,j) and ai(i,j)
  !          are equal to zero if
  !           (1) i is greater than j and
  !           (2) j=1,...,low-1 or i=igh+1,...,n;
  !
  !        scale contains information determining the
  !           permutations and scaling factors used.
  !
  !     suppose that the principal submatrix in rows low through igh
  !     has been balanced, that p(j) denotes the index interchanged
  !     with j during the permutation step, and that the elements
  !     of the diagonal matrix used are denoted by d(i,j).  then
  !        scale(j) = p(j),    for j = 1,...,low-1
  !                 = d(j,j)       j = low,...,igh
  !                 = p(j)         j = igh+1,...,n.
  !     the order in which the interchanges are made is n to igh+1,
  !     then 1 to low-1.
  !
  !     note that 1 is returned for igh if igh is zero formally.
  !
  !     the algol procedure exc contained in cbalance appears in
  !     cbal  in line.  (note that the algol roles of identifiers
  !     k,l have been reversed.)
  !
  !     arithmetic is real throughout.
  !
  !     questions and comments should be directed to b. s. garbow,
  !     applied mathematics division, argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
  !     :::::::::: radix is a machine dependent parameter specifying
  !                the base of the machine floating point representation.
  !                radix = 16.0d0 for long form arithmetic
  !                on s360 ::::::::::
  !     data radix/z4210000000000000/
  !
  !     &&&&&&&&&&&&&&&   temporary patch for f4v vax compiler:
  radix = 16.0d0
  !     data statement just above is disabled (error for f4v). &&&
  !     &&&&&&&&&&&&&&&&&&&&&&&&&&&&  end temporary patch.  23 jan
  b2 = radix * radix
  k = 1
  l = n
  go to 100
  !     :::::::::: in-line procedure for row and
  !                column exchange ::::::::::
20 scale(m) = j
  if (j .eq. m) go to 50
  do i = 1, l
     f = ar(i,j)
     ar(i,j) = ar(i,m)
     ar(i,m) = f
     f = ai(i,j)
     ai(i,j) = ai(i,m)
     ai(i,m) = f
30 end do
  do i = k, n
     f = ar(j,i)
     ar(j,i) = ar(m,i)
     ar(m,i) = f
     f = ai(j,i)
     ai(j,i) = ai(m,i)
     ai(m,i) = f
40 end do
50 go to (80,130), iexc
  !     :::::::::: search for rows isolating an eigenvalue
  !                and push them down ::::::::::
80 if (l .eq. 1) go to 280
  l = l - 1
  !     :::::::::: for j=l step -1 until 1 do -- ::::::::::
100 do jj = 1, l
     j = l + 1 - jj
     do  i = 1, l
        if (i .eq. j) go to 110
        if (ar(j,i) .ne. 0.0 .or. ai(j,i) .ne. 0.0) go to 120
110  end do
     m = l
     iexc = 1
     go to 20
120 end do
  go to 140
  !     :::::::::: search for columns isolating an eigenvalue
  !                and push them left ::::::::::
130 k = k + 1
140 do j = k, l
     do i = k, l
        if (i .eq. j) go to 150
        if (ar(i,j) .ne. 0.0 .or. ai(i,j) .ne. 0.0) go to 170
150  end do
     m = k
     iexc = 2
     go to 20
170 end do
  !     :::::::::: now balance the submatrix in rows k to l ::::::::::
  do i = k, l
180  scale(i) = 1.0
  end do
  !     :::::::::: iterative loop for norm reduction ::::::::::
190 noconv = 0
  do i = k, l
     c = 0.0
     r = 0.0
     do j = k, l
        if (j .eq. i) go to 200
        c = c + absz(ar(j,i)) + absz(ai(j,i))
        r = r + absz(ar(i,j)) + absz(ai(i,j))
200  end do
     !     :::::::::: guard against zero c or r due to underflow ::::::::::
     if (c .eq. 0.0 .or. r .eq. 0.0) go to 270
     g = r / radix
     f = 1.0
     s = c + r
210  if (c .ge. g) go to 220
     f = f * radix
     c = c * b2
     go to 210
220  g = r * radix
230  if (c .lt. g) go to 240
     f = f / radix
     c = c / b2
     go to 230
     !     :::::::::: now balance ::::::::::
240  c1 = 0.95
     if ((c + r) / f .ge.  c1 * s) go to 270
     g = 1.0 / f
     scale(i) = scale(i) * f
     noconv = 1
     do j = k, n
        ar(i,j) = ar(i,j) * g
        ai(i,j) = ai(i,j) * g
250  end do
     do j = 1, l
        ar(j,i) = ar(j,i) * f
        ai(j,i) = ai(j,i) * f
260  end do
270 end do
  if (noconv .eq. 1) go to 190
280 low = k
  igh = l
  return
end subroutine cbal
!
! subroutine cbabk2.
!
subroutine cbabk2(nm,n,low,igh,scale,m,zr,zi,ndim)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension scale(20),zr(ndim,ndim),zi(ndim,ndim)
  !     this subroutine is a translation of the algol procedure
  !     cbabk2, which is a complex version of balbak,
  !     num. math. 13, 293-304(1969) by parlett and reinsch.
  !     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
  !
  !     this subroutine forms the eigenvectors of a complex general
  !     matrix by back transforming those of the corresponding
  !     balanced matrix determined by  cbal.
  !
  !     on input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        low and igh are integers determined by  cbal;
  !
  !        scale contains information determining the permutations
  !          and scaling factors used by  cbal;
  !
  !        m is the number of eigenvectors to be back transformed;
  !
  !        zr and zi contain the real and imaginary parts,
  !          respectively, of the eigenvectors to be
  !          back transformed in their first m columns.
  !
  !     on output:
  !
  !        zr and zi contain the real and imaginary parts,
  !          respectively, of the transformed eigenvectors
  !          in their first m columns.
  !
  !     questions and comments should be directed to b. s. garbow,
  !     applied mathematics division, argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
  if (m .eq. 0) go to 200
  if (igh .eq. low) go to 120
  do i = low, igh
     s = scale(i)
     !     :::::::::: left hand eigenvectors are back transformed
     !                if the foregoing statement is replaced by
     !                s=1.0/scale(i). ::::::::::
     do j = 1, m
        zr(i,j) = zr(i,j) * s
        zi(i,j) = zi(i,j) * s
100  end do
110 end do
  !     :::::::::: for i=low-1 step -1 until 1,
  !                igh+1 step 1 until n do -- ::::::::::
120 do ii = 1, n
     i = ii
     if (i .ge. low .and. i .le. igh) go to 140
     if (i .lt. low) i = low - ii
     k = scale(i)
     if (k .eq. i) go to 140
     do j = 1, m
        s = zr(i,j)
        zr(i,j) = zr(k,j)
        zr(k,j) = s
        s = zi(i,j)
        zi(i,j) = zi(k,j)
        zi(k,j) = s
130  end do
140 end do
200 return
end subroutine cbabk2
!
! subroutine cmhes.
!
subroutine comhes(nm,n,low,igh,ar,ai,int,lunit6,iprsup,ndim,iord)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension ar(ndim,ndim), ai(ndim,ndim), int(20), iord(20)
  !     this subroutine is a translation of the algol procedure comhes,
  !     num. math. 12, 349-368(1968) by martin and wilkinson.
  !     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
  !
  !     given a complex general matrix, this subroutine
  !     reduces a submatrix situated in rows and columns
  !     low through igh to upper hessenberg form by
  !     stabilized elementary similarity transformations.
  !
  !     on input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        low and igh are integers determined by the balancing
  !          subroutine  cbal.  if  cbal  has not been used,
  !          set low=1, igh=n;
  !
  !        ar and ai contain the real and imaginary parts,
  !          respectively, of the complex input matrix.
  !
  !     on output:
  !
  !        ar and ai contain the real and imaginary parts,
  !          respectively, of the hessenberg matrix.  the
  !          multipliers which were used in the reduction
  !          are stored in the remaining triangles under the
  !          hessenberg matrix;
  !
  !        int contains information on the rows and columns
  !          interchanged in the reduction.
  !          only elements low through igh are used.
  !
  !     arithmetic is real except for the replacement of the algol
  !     procedure cdiv by complex division using subroutine cmplxz.
  !
  !     questions and comments should be directed to b. s. garbow,
  !     applied mathematics division, argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
!!!!      flzero = 1.d-12       !replaced by epscmh }
  epscmh = 1.e-15
  do i=1, 20
     iord(i) = i
121 end do
  la = igh - 1
  kp1 = low + 1
  if (iprsup .ge. 3 ) write (lunit6, 20 )  igh, low, la, kp1
20 format ( 47h at the top of comhes, igh, low, la and kp1 are, 4i10)
  d13=epscmh
  if (la .lt. kp1) go to 200
  do m = kp1, la
     mm1 = m - 1
     xr = 0.0
     xi = 0.0
     i = m
     do j = m, igh
!!!!            if (absz(ar(j,mm1)) + absz(ai(j,mm1))
!!!!     1         .le. absz(xr) + absz(xi)) go to 100
        if ((ar(j,mm1)**2) + (ai(j,mm1)**2) .le. (xr*xr) + (xi*xi)) go to 100
        xr = ar(j,mm1)
        xi = ai(j,mm1)
        i = j
100  end do
     int(m) = i
     if (i .eq. m) go to 130
     !     :::::::::: interchange rows and columns of ar and ai ::::::::::
     it=iord(i)
     iord(i) = iord(m)
     iord(m) = it
     do j = mm1, n
        yr = ar(i,j)
        ar(i,j) = ar(m,j)
        ar(m,j) = yr
        yi = ai(i,j)
        ai(i,j) = ai(m,j)
        ai(m,j) = yi
110  end do
     do j = 1, igh
        yr = ar(j,i)
        ar(j,i) = ar(j,m)
        ar(j,m) = yr
        yi = ai(j,i)
        ai(j,i) = ai(j,m)
        ai(j,m) = yi
120  end do
     !     :::::::::: end interchange ::::::::::
     !  130    if (xr .lt. d13 .and. xi .lt. d13)  go to 180
     !!  130    if (absz(xr) .lt. d13 .and. absz(xi) .lt. d13)  go to 180
130  if (xr .eq. 0.0 .and. xi .eq. 0.0)  go to 180
     mp1 = m + 1
     do i = mp1, igh
        yr = ar(i,mm1)
        yi = ai(i,mm1)
        !      if (yr .lt. d13 .and. yi .lt. d13) go to 160
        !!    if (absz(yr) .lt. d13 .and. absz(yi) .lt. d13) go to 160
        if (yr .eq. 0.0 .and. yi .eq. 0.0) go to 160
        d1 = xr * xr + xi * xi
        d2 = (xr*yr + yi*xi) / d1
        d3 = (xr*yi - xi*yr) / d1
        yr = d2
        yi = d3
        ar(i,mm1) = yr
        ai(i,mm1) = yi
        do j = m, n
           ar(i,j) = ar(i,j) - yr * ar(m,j) + yi * ai(m,j)
           ai(i,j) = ai(i,j) - yr * ai(m,j) - yi * ar(m,j)
140     end do
        do j = 1, igh
           ar(j,m) = ar(j,m) + yr * ar(j,i) - yi * ai(j,i)
           ai(j,m) = ai(j,m) + yr * ai(j,i) + yi * ar(j,i)
150     end do
160  end do
     if ( iprsup .ge. 3 ) write (lunit6, 170) m, igh, int(m),(ar(j,m),ai(j,m),j=1,igh)
170  format ( 29h m, igh and int(m) at 170 are, 3i10,/, 31h (ar(j,m),ai(j,m),j=1,igh), are,/, (1x,8e15.6) )
180 end do
200 return
end subroutine comhes
!
! subroutine comlr.
!
subroutine comlr(nm,n,low,igh,hr,hi,wr,wi,ierr,ndim)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  dimension hr(ndim,ndim),hi(ndim,ndim),wr(ndim),wi(ndim)
  !     this subroutine is a translation of the algol procedure comlr,
  !     num. math. 12, 369-376(1968) by martin and wilkinson.
  !     handbook for auto. comp., vol.ii-linear algebra, 396-403(1971).
  !
  !     this subroutine finds the eigenvalues of a complex
  !     upper hessenberg matrix by the modified lr method.
  !
  !     on input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        low and igh are integers determined by the balancing
  !          subroutine  cbal.  if  cbal  has not been used,
  !          set low=1, igh=n;
  !
  !        hr and hi contain the real and imaginary parts,
  !          respectively, of the complex upper hessenberg matrix.
  !          their lower triangles below the subdiagonal contain the
  !          multipliers which were used in the reduction by  comhes,
  !          if performed.
  !
  !     on output:
  !
  !        the upper hessenberg portions of hr and hi have been
  !          destroyed.  therefore, they must be saved before
  !          calling  comlr  if subsequent calculation of
  !          eigenvectors is to be performed;
  !
  !        wr and wi contain the real and imaginary parts,
  !          respectively, of the eigenvalues.  if an error
  !          exit is made, the eigenvalues should be correct
  !          for indices ierr+1,...,n;
  !
  !        ierr is set to
  !          zero       for normal return,
  !          j          if the j-th eigenvalue has not been
  !                     determined after 30 iterations.
  !
  !     questions and comments should be directed to b. s. garbow,
  !     applied mathematics division, argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
  !     :::::::::: epmach is a machine dependent parameter specifying
  !                the relative precision of floating point arithmetic.
  !                epmach = 16.0d0**(-13) for long form arithmetic
  !                on s360 ::::::::::
  !     data epmach/z3410000000000000/
  !     epmach will be set to equal to the system dependent floating
  !     point zero variable  "flzero"
  epmach = flzero
  ierr = 0
  !     :::::::::: store roots isolated by cbal ::::::::::
  do i = 1, n
     if (i .ge. low .and. i .le. igh) go to 200
     wr(i) = hr(i,i)
     wi(i) = hi(i,i)
200 end do
  ien = igh
  tr = 0.0
  ti = 0.0
  !     :::::::::: search for next eigenvalue ::::::::::
220 if (ien .lt. low) go to 1001
  its = 0
  ienm1 = ien - 1
  !     :::::::::: look for single small sub-diagonal element
  !                for l=ien step -1 until low d0 -- ::::::::::
240 do ll = low, ien
     l = ien + low - ll
     if (l .eq. low) go to 300
     if (absz(hr(l,l-1)) + absz(hi(l,l-1)) .le. epmach * (absz(hr(l-1,l-1)) + absz(hi(l-1,l-1)) + absz(hr(l,l)) + absz(hi(l,l)))) go to 300
260 end do
  !     :::::::::: form shift ::::::::::
300 if (l .eq. ien) go to 660
  if (its .eq. 30) go to 1000
  if (its .eq. 10 .or. its .eq. 20) go to 320
  sr = hr(ien,ien)
  si = hi(ien,ien)
  xr = hr(ienm1,ien) * hr(ien,ienm1) - hi(ienm1,ien) * hi(ien,ienm1)
  xi = hr(ienm1,ien) * hi(ien,ienm1) + hi(ienm1,ien) * hr(ien,ienm1)
  if (xr .eq. 0.0 .and. xi .eq. 0.0) go to 340
  yr = (hr(ienm1,ienm1) - sr) * onehaf
  yi = (hi(ienm1,ienm1) - si) * onehaf
  d1 = yr**2 - yi**2 + xr
  d2 = 2.0 * yr * yi + xi
  d3 = sqrtz( d1 ** 2 + d2 ** 2 )
  d3 = sqrtz( d3 )
  d4 = onehaf * atan2z(d2,d1)
  zzr = d3 * cosz (d4)
  zzi = d3 * sinz (d4)
  if (yr * zzr + yi * zzi .ge. 0.0) go to 310
  zzr = -zzr
  zzi = -zzi
310 d1 = (yr+zzr) **2 + (yi+zzi) **2
  d2 = ( xr*(yr+zzr) + xi*(yi+zzi) ) / d1
  d3 = ( xi*(yr+zzr) - xr*(yi+zzi) ) / d1
  sr = sr - d2
  si = si - d3
  go to 340
  !     :::::::::: form exceptional shift ::::::::::
320 sr = absz(hr(ien,ienm1)) + absz(hr(ienm1,ien-2))
  si = absz(hi(ien,ienm1)) + absz(hi(ienm1,ien-2))
340 do i = low, ien
     hr(i,i) = hr(i,i) - sr
     hi(i,i) = hi(i,i) - si
360 end do
  tr = tr + sr
  ti = ti + si
  its = its + 1
  !     :::::::::: look for two consecutive small
  !                sub-diagonal elements ::::::::::
  xr = absz(hr(ienm1,ienm1)) + absz(hi(ienm1,ienm1))
  yr = absz(hr(ien,ienm1)) + absz(hi(ien,ienm1))
  zzr = absz(hr(ien,ien)) + absz(hi(ien,ien))
  !     :::::::::: for m=ien-1 step -1 until l do -- ::::::::::
  do mm = l, ienm1
     m = ienm1 + l - mm
     if (m .eq. l) go to 420
     yi = yr
     yr = absz(hr(m,m-1)) + absz(hi(m,m-1))
     xi = zzr
     zzr = xr
     xr = absz(hr(m-1,m-1)) + absz(hi(m-1,m-1))
     if (yr .le. epmach * zzr / yi * (zzr + xr + xi)) go to 420
380 end do
  !     :::::::::: triangular decomposition h=l*r ::::::::::
420 mp1 = m + 1
  do i = mp1, ien
     im1 = i - 1
     xr = hr(im1,im1)
     xi = hi(im1,im1)
     yr = hr(i,im1)
     yi = hi(i,im1)
     if (absz(xr) + absz(xi) .ge. absz(yr) + absz(yi)) go to 460
     !     :::::::::: interchange rows of hr and hi ::::::::::
     do j = im1, ien
        zzr = hr(im1,j)
        hr(im1,j) = hr(i,j)
        hr(i,j) = zzr
        zzi = hi(im1,j)
        hi(im1,j) = hi(i,j)
        hi(i,j) = zzi
440  end do
     d1 = yr ** 2 + yi ** 2
     d2 = ( xr*yr + xi*yi ) / d1
     d3 = ( yr*xi - xr*yi ) / d1
     wr(i) = 1.0
     go to 480
460  d1 = xr ** 2 + xi ** 2
     d2 = ( yr*xr + yi*xi ) / d1
     d3 = ( xr*yi - yr*xi ) / d1
     wr(i) = -1.0
480  zzr = d2
     zzi = d3
     hr(i,im1) = zzr
     hi(i,im1) = zzi
     do j = i, ien
        hr(i,j) = hr(i,j) - zzr * hr(im1,j) + zzi * hi(im1,j)
        hi(i,j) = hi(i,j) - zzr * hi(im1,j) - zzi * hr(im1,j)
500  end do
520 end do
  !     :::::::::: composition r*l=h ::::::::::
  do j = mp1, ien
     xr = hr(j,j-1)
     xi = hi(j,j-1)
     hr(j,j-1) = 0.0
     hi(j,j-1) = 0.0
     !     :::::::::: interchange columns of hr and hi,
     !                if necessary ::::::::::
     if (wr(j) .le. 0.0) go to 580
     do i = l, j
        zzr = hr(i,j-1)
        hr(i,j-1) = hr(i,j)
        hr(i,j) = zzr
        zzi = hi(i,j-1)
        hi(i,j-1) = hi(i,j)
        hi(i,j) = zzi
540  end do
580  do i = l, j
        hr(i,j-1) = hr(i,j-1) + xr * hr(i,j) - xi * hi(i,j)
        hi(i,j-1) = hi(i,j-1) + xr * hi(i,j) + xi * hr(i,j)
600  end do
640 end do
  go to 240
  !     :::::::::: a root found ::::::::::
660 wr(ien) = hr(ien,ien) + tr
  wi(ien) = hi(ien,ien) + ti
  ien = ienm1
  go to 220
  !     :::::::::: set error -- no convergence to an
  !                eigenvalue after 30 iterations ::::::::::
1000 ierr = ien
1001 return
  !     :::::::::: last card of comlr ::::::::::
end subroutine comlr
!
! subroutine comlr2.
!
subroutine comlr2(nm,n,low,igh,int,hr,hi,zi,zr,wr,wi,ierr,ndim,iord)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  dimension hr(ndim,ndim),hi(ndim,ndim),wr(ndim),wi(ndim)
  dimension zr(ndim,ndim),zi(ndim,ndim)
  dimension int(20),iord(20),lseq(20)
  dimension umr(20), umi(20)
  dimension eim(20),vtr(20),vti(20),ppr(20,20),ppi(20,20)
  !     this subroutine is a translation of the algol procedure comlr2,
  !     num. math. 16, 181-204(1970) by peters and wilkinson.
  !     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
  !
  !     this subroutine finds the eigenvalues and eigenvectors
  !     of a complex upper hessenberg matrix by the modified lr
  !     method.  the eigenvectors of a complex general matrix
  !     can also be found if  comhes  has been used to reduce
  !     this general matrix to hessenberg form.
  !
  !     on input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        low and igh are integers determined by the balancing
  !          subroutine  cbal.  if  cbal  has not been used,
  !          set low=1, igh=n;
  !
  !        int contains information on the rows and columns interchanged
  !          in the reduction by  comhes, if performed.  only elements
  !          low through igh are used.  if the eigenvectors of the hessen-
  !          berg matrix are desired, set int(j)=j for these elements;
  !
  !        hr and hi contain the real and imaginary parts,
  !          respectively, of the complex upper hessenberg matrix.
  !          their lower triangles below the subdiagonal contain the
  !          multipliers which were used in the reduction by  comhes,
  !          if performed.  if the eigenvectors of the hessenberg
  !          matrix are desired, these elements must be set to zero.
  !
  !     on output:
  !
  !        the upper hessenberg portions of hr and hi have been
  !          destroyed, but the location hr(1,1) contains the fnorm
  !          of the triangularized matrix;
  !
  !        wr and wi contain the real and imaginary parts,
  !          respectively, of the eigenvalues.  if an error
  !          exit is made, the eigenvalues should be correct
  !          for indices ierr+1,...,n;
  !
  !        zr and zi contain the real and imaginary parts,
  !          respectively, of the eigenvectors.  the eigenvectors
  !          are unnormalized.  if an error exit is made, none of
  !          the eigenvectors has been found;
  !
  !        ierr is set to
  !          zero       for normal return,
  !          j          if the j-th eigenvalue has not been
  !                     determined after 30 iterations.
  !
  !     questions and comments should be directed to b. s. garbow,
  !     applied mathematics division, argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
  !     :::::::::: epmach is a machine dependent parameter specifying
  !                the relative precision of floating point arithmetic.
  !                epmach = 16.0d0**(-13) for long form arithmetic
  !                on s360 ::::::::::
  !     data epmach/z3410000000000000/
  !     like in subroutine comlr, epmach is set to be flzero
!!!!      epmach = 1.0e-50      ! now in choice.dat }
  epmach = 1.e-32
  itsmax = 300
  ierr = 0
  !     :::::::::: initialize eigenvector matrix ::::::::::
  do i = 1, n
     do j = 1, n
        zr(i,j) = 0.0
        zi(i,j) = 0.0
        if (i .eq. j) zr(i,j) = 1.0
100  end do
  end do
  !     :::::::::: form the matrix of accumulated transformations
  !                from the information left by comhes ::::::::::
  iend = igh - low - 1
  if (iend .le. 0) go to 180
  !     :::::::::: for i=igh-1 step -1 until low+1 do -- ::::::::::
  do ii = 1, iend
     i = igh - ii
     ip1 = i + 1
     do k = ip1, igh
        zr(k,i) = hr(k,i-1)
        zi(k,i) = hi(k,i-1)
120  end do
     j = int(i)
     if (i .eq. j) go to 160
     do k = i, igh
        zr(i,k) = zr(j,k)
        zi(i,k) = zi(j,k)
        zr(j,k) = 0.0
        zi(j,k) = 0.0
140  end do
     zr(j,i) = 1.0
160 end do
  if ( iprsup .ge. 3 ) write (lunit6, 170) ((zr(i,j),zi(i,j),j=1,igh),i=1,igh)
170 format ( 69h after do 160 loop in comlr2, ((zr(i,j),zi(i,j),j=1,igh),i=1,igh) are,/, (1x,8e15.6) )
  !     :::::::::: store roots isolated by cbal ::::::::::
180 do i = 1, n
     if (i .ge. low .and. i .le. igh) go to 200
     wr(i) = hr(i,i)
     wi(i) = hi(i,i)
200 end do
  ien = igh
  tr = 0.0
  ti = 0.0
  !     :::::::::: search for next eigenvalue ::::::::::
220 if (ien .lt. low) go to 680
  its = 0
  ienm1 = ien - 1
  !     :::::::::: look for single small sub-diagonal element
  !                for l=ien step -1 until low do -- ::::::::::
240 do ll = low, ien
     l = ien + low - ll
     if (l .eq. low) go to 300
     if (absz(hr(l,l-1)) + absz(hi(l,l-1)) .le. epmach * (absz(hr(l-1,l-1)) + absz(hi(l-1,l-1)) + absz(hr(l,l)) + absz(hi(l,l)))) go to 300
260 end do
  !     :::::::::: form shift ::::::::::
300 if (l .eq. ien) go to 660
  if (its .eq. itsmax ) go to 1000
!!!!      if ((its/10)*10 .eq. its ) go to 320
!!!!!     if (its .eq. 10 .or. its .eq. 20) go to 320
  sr = hr(ien,ien)
  si = hi(ien,ien)
  xr = hr(ienm1,ien) * hr(ien,ienm1) - hi(ienm1,ien) * hi(ien,ienm1)
  xi = hr(ienm1,ien) * hi(ien,ienm1) + hi(ienm1,ien) * hr(ien,ienm1)
  if ( iprsup .ge. 3) write (lunit6, 305) sr, si, xr, xi
305 format ( 29h sr, si, xr and xi at 305 are, 4e15.6 )
  if (xr .eq. 0.0 .and. xi .eq. 0.0) go to 340
  yr = (hr(ienm1,ienm1) - sr) * onehaf
  yi = (hi(ienm1,ienm1) - si) * onehaf
  d1 = yr**2 - yi**2 + xr
  d2 = 2.0 * yr * yi + xi
  d3 = sqrtz( d1 ** 2 + d2 ** 2 )
  d3 = sqrtz( d3 )
  d4 = onehaf * atan2z(d2,d1)
  zzr = d3 * cosz (d4)
  zzi = d3 * sinz (d4)
  if (yr * zzr + yi * zzi .ge. 0.0) go to 310
  zzr = -zzr
  zzi = -zzi
310 d1 = (yr+zzr) **2 + (yi+zzi) **2
  d2 = ( xr*(yr+zzr) + xi*(yi+zzi) ) / d1
  d3 = ( xi*(yr+zzr) - xr*(yi+zzi) ) / d1
  if ( iprsup .ge. 3 ) write (lunit6, 315) yr, yi, zzr, zzi, d2,d3
315 format ( 38h yr, yi, zzr, zzi and d2,d3 at 315 are, 6e15.6)
  sr = sr - d2
  si = si - d3
  go to 340
  !     :::::::::: form exceptional shift ::::::::::
!!!!  320 sr = absz(hr(ien,ienm1)) + absz(hr(ienm1,ien-2))
!!!!      si = absz(hi(ien,ienm1)) + absz(hi(ienm1,ien-2))
340 do i = low, ien
     hr(i,i) = hr(i,i) - sr
     hi(i,i) = hi(i,i) - si
     if ( iprsup .ge. 3 ) write (lunit6, 350) i, sr, si, hr(i,i),hi(i,i)
  350 format ( 43h i, sr, si, hr(i,i), and hi(i,i) at 350 are,/,10 x, i8, 4e15.6)
360 end do
  tr = tr + sr
  ti = ti + si
  its = its + 1
  !     :::::::::: look for two consecutive small
  !                sub-diagonal elements ::::::::::
  xr = absz(hr(ienm1,ienm1)) + absz(hi(ienm1,ienm1))
  yr = absz(hr(ien,ienm1)) + absz(hi(ien,ienm1))
  zzr = absz(hr(ien,ien)) + absz(hi(ien,ien))
  if ( iprsup .ge. 3 ) write (lunit6, 370) xr, yr, zzr
370 format ( 27h xr, yr, and zzr at 370 are, 3e15.6)
  !     :::::::::: for m=ien-1 step -1 until l do -- ::::::::::
  do mm = l, ienm1
     m = ienm1 + l - mm
     if (m .eq. l) go to 420
     yi = yr
     yr = absz(hr(m,m-1)) + absz(hi(m,m-1))
     xi = zzr
     zzr = xr
     xr = absz(hr(m-1,m-1)) + absz(hi(m-1,m-1))
     if ( iprsup .ge. 3 ) write (lunit6, 375) mm, yr, zzr, yi, xr, xi, epmach
375  format ( 47h at 375, mm, yr, zzr, yi, xr, xi and epmach are,/, 10x, i8, 6e15.6)
     if (yr .le. epmach * zzr / yi * (zzr + xr + xi)) go to 420
380 end do
  !     :::::::::: triangular decomposition h=l*r ::::::::::
420 mp1 = m + 1
  do i = mp1, ien
     im1 = i - 1
     xr = hr(im1,im1)
     xi = hi(im1,im1)
     yr = hr(i,im1)
     yi = hi(i,im1)
     if (absz(xr) + absz(xi) .ge. absz(yr) + absz(yi)) go to 460
     !     :::::::::: interchange rows of hr and hi ::::::::::
     do j = im1, n
        zzr = hr(im1,j)
        hr(im1,j) = hr(i,j)
        hr(i,j) = zzr
        zzi = hi(im1,j)
        hi(im1,j) = hi(i,j)
        hi(i,j) = zzi
440  end do
     !     it=iord(i)
     ittemp = iord(i)
     iord(i) = iord(im1)
     !     iord(im1) = it
     iord(im1) = ittemp
441  continue
     d1 = yr ** 2 + yi ** 2
     d2 = ( xr*yr + xi*yi ) / d1
     d3 = ( yr*xi - xr*yi ) / d1
     wr(i) = 1.0
     go to 480
460  d1 = xr ** 2 + xi ** 2
     d2 = ( yr*xr + yi*xi ) / d1
     d3 = ( xr*yi - yr*xi ) / d1
     wr(i) = -1.0
480  zzr = d2
     zzi = d3
     hr(i,im1) = zzr
     hi(i,im1) = zzi
     do j = i, n
        hr(i,j) = hr(i,j) - zzr * hr(im1,j) + zzi * hi(im1,j)
        hi(i,j) = hi(i,j) - zzr * hi(im1,j) - zzi * hr(im1,j)
500  end do
     if ( iprsup .ge. 3 ) write (lunit6, 510) i,n, (hr(i,j), hi(i,j), j=1,n)
510  format ( 42h at 510, (hr(i,j), hi(i,j), j=1,n) for i =, i8, 10h, and n = , i8,  2x, 3hare,/, (1x, 8e15.6) )
520 end do
  !     :::::::::: composition r*l=h ::::::::::
  do j = mp1, ien
     xr = hr(j,j-1)
     xi = hi(j,j-1)
     hr(j,j-1) = 0.0
     hi(j,j-1) = 0.0
     !     :::::::::: interchange columns of hr, hi, zr, and zi,
     !                if necessary ::::::::::
     if (wr(j) .le. 0.0) go to 580
     do i = 1, j
        zzr = hr(i,j-1)
        hr(i,j-1) = hr(i,j)
        hr(i,j) = zzr
        zzi = hi(i,j-1)
        hi(i,j-1) = hi(i,j)
        hi(i,j) = zzi
540  end do
     do i = low, igh
        zzr = zr(i,j-1)
        zr(i,j-1) = zr(i,j)
        zr(i,j) = zzr
        zzi = zi(i,j-1)
        zi(i,j-1) = zi(i,j)
        zi(i,j) = zzi
560  end do
580  do i = 1, j
        hr(i,j-1) = hr(i,j-1) + xr * hr(i,j) - xi * hi(i,j)
        hi(i,j-1) = hi(i,j-1) + xr * hi(i,j) + xi * hr(i,j)
600  end do
     !     :::::::::: accumulate transformations ::::::::::
     do i = low, igh
        zr(i,j-1) = zr(i,j-1) + xr * zr(i,j) - xi * zi(i,j)
        zi(i,j-1) = zi(i,j-1) + xr * zi(i,j) + xi * zr(i,j)
620  end do
     if ( iprsup .ge. 3 ) write ( lunit6, 630) j, low, igh, (hr(i,j),hi(i,j),i=1,j), (zr(i,j),zi(i,j),i=low,igh)
630  format ( 26h j, low and igh at 630 are, 3i8, 60h (hr(i,j),hi(i,j),i=1,j) and (zr(i,j),zi(i,j),i=low,igh) are, /, (1x,8e15.6) )
640 end do
  go to 240
  !     :::::::::: a root found ::::::::::
660 hr(ien,ien) = hr(ien,ien) + tr
  wr(ien) = hr(ien,ien)
  hi(ien,ien) = hi(ien,ien) + ti
  wi(ien) = hi(ien,ien)
  ien = ienm1
  if ( iprsup .ge. 3 ) write (lunit6, 670) ien,hr(ien,ien),hi(ien,ien),wr(ien),wi(ien)
670 format ( 67h     ien    hr(ien,ien)    hi(ien,ien)       wr(ien)      wi(ien),/, i8, 4e15.6)
  go to 220
680 continue
  do id=low,igh
     j = iord(id)
     umr(j) = wr(id)
     umi(j) = wi(id)
671 end do
  !     :::::::::: all roots found.  backsubstitute to find
  !                vectors of upper triangular form ::::::::::
  fnorm = 0.0
  do i = 1, n
     do j = i, n
        fnorm = fnorm + absz(hr(i,j)) + absz(hi(i,j))
720  end do
  end do
  hr(1,1) = fnorm
  if (n .eq. 1 .or. fnorm .eq. 0.0) go to 1001
  !     :::::::::: for ien=n step -1 until 2 do -- ::::::::::
  do nn = 2, n
     ien = n + 2 - nn
     xr = wr(ien)
     xi = wi(ien)
     ienm1 = ien - 1
     !     :::::::::: for i=ien-1 step -1 until 1 do -- ::::::::::
     do ii = 1, ienm1
        i = ien - ii
        zzr = hr(i,ien)
        zzi = hi(i,ien)
        if (i .eq. ienm1) go to 760
        ip1 = i + 1
        do j = ip1, ienm1
           zzr = zzr + hr(i,j) * hr(j,ien) - hi(i,j) * hi(j,ien)
           zzi = zzi + hr(i,j) * hi(j,ien) + hi(i,j) * hr(j,ien)
740     end do
760     yr = xr - wr(i)
        yi = xi - wi(i)
        if (yr .eq. 0.0 .and. yi .eq. 0.0) yr = epmach * fnorm
        d1 = yr ** 2 + yi ** 2
        d2 = ( zzr*yr + zzi*yi ) / d1
        d3 = ( yr*zzi - yi*zzr ) / d1
        hr(i,ien) = d2
        hi(i,ien) = d3
        if ( iprsup .ge. 3 ) write (lunit6, 770)  nn,n,ien,ii,i,ienm1,zzr,zzi,yr,yi,d2,d3, hr(i,ien),hi(i,ien)
770     format ( 87h nn, n, ien, ii, i, ienm1, zzr, zzi, yr, yi, d2, d3, hr(i,ien) and hi(i,ien) at 770 are,/, 1x, 6i5, 8e12.5)
780  end do
800 end do
  !     :::::::::: end backsubstitution ::::::::::
  ienm1 = n - 1
  !     :::::::::: vectors of isolated roots ::::::::::
  do i = 1, ienm1
     if (i .ge. low .and. i .le. igh) go to 840
     ip1 = i + 1
     do j = ip1, n
        zr(i,j) = hr(i,j)
        zi(i,j) = hi(i,j)
820  end do
840 end do
  !     :::::::::: multiply by transformation matrix to give
  !                vectors of original full matrix.
  !                for j=n step -1 until low+1 do -- ::::::::::
  do jj = low, ienm1
     j = n + low - jj
     !        m = min0z(j-1,igh)
     m = j - 1
     if ( m .gt. igh )  m = igh
     do i = low, igh
        zzr = zr(i,j)
        zzi = zi(i,j)
        do k = low, m
           zzr = zzr + zr(i,k) * hr(k,j) - zi(i,k) * hi(k,j)
           zzi = zzi + zr(i,k) * hi(k,j) + zi(i,k) * hr(k,j)
860     end do
        zr(i,j) = zzr
        zi(i,j) = zzi
        if (iprsup .ge. 3 ) write (lunit6, 870) jj, j, m, zr(i,j),zi(i,j)
870     format ( 62h at the end of do 880 loop, jj, j,  m, zr(i,j) and zi(i,j) are,5x,3i8,2e15.6)
880  end do
  end do
  go to 1001
  !     :::::::::: set error -- no convergence to an
  !                eigenvalue after 30 iterations ::::::::::
1000 ierr = ien
1001 continue
!!!!      do 1002 i=low, igh
!!!!      wr(i) = umr(i)
!!!!      wi(i) = umi(i)
1002 continue
  !!     ** sorting the eigenvalues with magnitude **
  !!        also reordering the eigenvector
  do i=1,20
     lseq(i)=i
1003 end do
  do i=1,n
     vtr(i)=wr(i)
     vti(i)=wi(i)
     do j=1,n
        ppr(i,j)=zr(i,j)
        ppi(i,j)=zi(i,j)
1004 end do
  end do
  do i=1,n
     eim(i) = wr(i)*wr(i) + wi(i)*wi(i)
1005 end do
  do j=1, n
     l=n-j
     do i=1, l
        if ( eim(i) .ge. eim(i+1) ) go to 1007
        t=eim(i)
        eim(i)=eim(i+1)
        eim(i+1)=t
        t=lseq(i)
        lseq(i)=lseq(i+1)
        lseq(i+1)=t
1007 end do
1006 end do
  do i=1,n
     wr(i)=vtr(lseq(i))
     wi(i)=vti(lseq(i))
     do j=1,n
        zr(i,j)=ppr(i,lseq(j))
        zi(i,j)=ppi(i,lseq(j))
1009 end do
1008 end do
1100 return
  !     :::::::::: last card of comlr2 ::::::::::
end subroutine comlr2
!
! subroutine cxred2.
!
subroutine cxred2( a, c, b, d, n, m )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !)    elimination of variables m+1,...n in symmetric complex matrix with
  !)    a=real part, c=imaginary part. a and c are
  !)    stored as triangle (1 element for 1.column,2 for 2.column etc.).
  !)    result is reduced matrix in columns 1,...m in case of reduction
  !)    (m unequal 0) or negative inverse matrix in columns 1,...n in case
  !)    of inversion (m=0).
  dimension a(1), c(1), b(1), d(1)
  j = n + 1
  w=1.0
  if(m.gt.0) w=-w
  ij=n*j/2
3 j=j-1
  if(j.eq.m) return
  h1=a(ij)
  g1=c(ij)
  x=1.0/(h1*h1+g1*g1)
  h1=-h1*x
  g1=g1*x
  b(j)=h1
  d(j)=g1
  ij=ij-j
  k=0
  ik=0
  !                                   begin k-loop
4 ik=ik+k
  i1=ik+1
  k=k+1
  if(k.gt.n) go to 3
  if(k.lt.j) go to 9
  if(w.lt.0.) go to 3
  if(k.eq.j) go to 7
  i=ik+j
5 h2=a(i)
  g2=c(i)
  b(k) = h2*h1-g2*g1
  d(k)=h2*g1+g2*h1
  !                                   begin i-loop
  i2=ik+k
  l=0
  do i=i1,i2
     l=l+1
     x=b(l)
     y=d(l)
     a(i)=a(i)+x*h2-y*g2
6    c(i)=c(i)+x*g2+y*h2
  end do
  if(k.lt.j) go to 4
  i=ik+j
  a(i)=b(k)
  c(i)=d(k)
  go to 4
  !                                   end i-loop
7 i=ij
  do l=1,j
     i=i+1
     c(i)=d(l)
8    a(i)=b(l)
  end do
  go to 4
  !                                   end k-loop
9 i=ij+k
  go to 5
end subroutine cxred2
!
! subroutine symn.
!
subroutine symm(p,z,switch,kcirct,kk)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension p(9), z(9)
  dimension ar(3,3),ai(3,3),fr(3),fi(3)
  include 'blkcom.ftn'
  include 'labl44.ftn'
  if(kcirct.eq.2) go to 100
  fr(1) = unity
  fi(1) = 0.
  fr(2) = - onehaf
  fi(2) = valu7
  fr(3) = - onehaf
  fi(3) = - fi(2)
  kk=0
63 ki=kk*(kk+1)/2
  j=ki+kk
  kold=kk
  kk=kk+3
  if(kk.gt.kcirct) go to 79
75 l=ki+1
  k=0
64 k=k+1
  if(ki.eq.j) go to 76
   65 l3 = l+2
   66 l2 = l+1
67 do i=1,3
     f1 = p(l) + fr(i)*(p(l2)+p(l3))
     f2 = fi(i)*(p(l2) - p(l3))
     if(switch.lt.0.0) go to 68
     f1 = f1 + fi(i)*(z(l3)-z(l2))
     f2 = f2+z(l)+fr(i)*(z(l2)+z(l3))
68   ar(i,k) = f1
681  ai(i,k) = f2
  end do
  if (k.eq.3) go to 69
  l = l+kold+k
  go to 64
69 l=ki
  k=0
70 k=k+1
  do i=1,3
     m=l+i
     if(ki.lt.j) go to 71
     if(i.gt.k) go to 73
71   p(m) =(ar(i,1)+fr(k)*(ar(i,2)+ar(i,3))+fi(k)*(ai(i,3)-ai(i,2)) ) / 3.0
72   z(m) =(ai(i,1)+fr(k)*(ai(i,2)+ai(i,3))+fi(k)*(ar(i,2)-ar(i,3)) ) / 3.0
  end do
73 if (k.eq.3) go to 74
  l = l+kold+k
  go to 70
74 ki = ki+3
  if (ki.gt.j) go to 63
  go to 75
76 if(k.eq.3) go to 65
  l2 = j+kk-1
  l3 = l2+kk
  if (k.eq.2) go to 66
  l3 = l3-1
  go to 67
79 kk=kk-3
  !                                   end of symmetrical components matrix
  return
100 f1=(p(1)+p(3))* onehaf
  f2=p(2)
  p(2)=(p(1)-p(3))* onehaf
  p(1)=f1+f2
  p(3)=f1-f2
  p(5)=p(3)
  kk=2
  if(switch.lt. 0.) return
  f1=(z(1)+z(3))* onehaf
  f2=z(2)
  z(2)=(z(1)-z(3))* onehaf
  z(1)=f1+f2
  z(3)=f1-f2
  z(5)=z(3)
  return
end subroutine symm
!
! subroutine skin.
!
subroutine skin (s,r,freq,rf,xf)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labl44.ftn'
  double precision  vsmall
  vsmall = 1.e-37
  call undrfl(jjj)
  s2 = s*s
  s3=(unity-s2)*r
  r2 = freq * valu8 / s3
  rf=r
  xf=0.
  if(r2.eq.0.) go to 9900
  qremb = 0.0
  if ( s  .lt.  tenm6 )   go to 5
  q2 = r2*s2
  if (s2 .lt. 0.8)  go to 11
  if (q2 .le. 64.0 .and. r2 .gt. 64.0)  write(lunit6,10) q2, r2
10 format (' Results from subroutine skin unreliable with mq**2= ', f9.4, ' and mr**2= ', f9.4)
11 if (q2 .gt. 64.0)  qremb=sqrtz(q2) * sqrt2
  x = sqrtz(q2)
  x2=x*x/64.0
  iback = 2
  if (x2.le.unity) go to 100
  go to 200
4 a = - berd
  b = - beid
  aremb = gerd
  bremb = geid
  xremb = x
5 x = sqrtz(r2)
  x2=x*x/64.0
  iback = 1
  if (x2.le.unity) go to 100
  go to 200
6 g = ber
  h = bei
  e = berd
  f = beid
  if ( s  .lt.  tenm6 )   go to 7
  g = a*ger - b*gei + aremb*ber - bremb *bei
  h = a*gei + b*ger + aremb*bei + bremb*ber
  e = a*gerd - b*geid + aremb*berd - bremb*beid
  f = a*geid + b*gerd + aremb*beid + bremb*berd
7 e2f2 = e**2 + f**2
  s2 =  x * s3 * onehaf / e2f2
  if (iprsup .ge. 1) write (lunit6, 320)  e, f, e2f2, s2
320 format (27h e, f,  e2f2, and s2 at 320, 4e16.6)
  rf = (-h*e+g*f)*s2
  xf = (g*e+h*f)*s2
  go to 9900
  !      calculation of kelvin-functions for argument.le.8.
100 z = x2
  ber = unity
  bei = 0.0
  berd = 0.0
  beid = onehaf
  gerd = 0.0
  geid =  valu9
  ger = - valu10
  gei = 0.0
  ialt = 1
  do k=1,14
     if(ialt.eq.1) go to 101
     ber = ber+fbe(k)*z
     beid = beid+fbed(k)*z
     if ( s  .lt.  tenm6 )   go to 102
     geid = geid+fked(k)*z
     if(iback.eq.2) go to 102
     ger = ger+fke(k)*z
     go to 102
101  bei = bei+fbe(k)*z
     berd = berd+fbed(k)*z
     if ( s  .lt.  tenm6 )   go to 102
     gerd = gerd+fked(k)*z
     if(iback.eq.2) go to 102
     gei = gei+fke(k)*z
102  z = z*x2
103  ialt = -ialt
  end do
  beid = beid*x
  berd = berd*x
  if ( s  .lt.  tenm6 )   go to 104
  xl = alogz(x*onehaf )
  gerd = -xl*berd-ber/x+beid* aaa1            +x*gerd
  geid = -xl*beid-bei/x-berd* aaa1            +x*geid
  if (iback.eq.2) go to 104
  ger = -xl*ber+bei* aaa1            +ger
  gei = -xl*bei-ber* aaa1            +gei
104 go to (6 , 4 ),iback
  !      calculations of kelvin-functions for argument.gt.8.
200 x2 = 8.0  /x
  z = x2
  ber = 0.0
  bei = - valu11
  berd = ber
  beid = bei
  ger = unity / sqrt2
  gei = ger
  gerd = ger
  geid = gei
  ialt = 1
  do k=1, 6
     thetar = fbe(k+14) * z
     thetai = fbed(k+14)*z
     phir = fke(k+14)*z
     phii = fked(k+14)*z
     ber = ber+thetar
     bei = bei+thetai
     ger = ger+phir
     gei = gei+phii
     if (ialt.eq.1) go to 201
     berd = berd+thetar
     beid = beid+thetai
     gerd = gerd+phir
     geid = geid+phii
     go to 202
201  berd = berd-thetar
     beid = beid-thetai
     gerd = gerd-phir
     geid = geid-phii
202  ialt = -ialt
203  z = z*x2
  end do
  xl=x* sqrt2
  if (qremb .lt. 1.0)  go to 204
  xl = xl - qremb
204 thetar = - xl + berd
  thetai = -xl+beid
  z = sqrtz(x)
  x2 =  valu12 / z
  z = valu13 / z* expz(thetar)
  fr = z* cosz(thetai)
  fi = z* sinz(thetai)
  x2=x2* expz(ber)
  thetar=x2* cosz(bei)
  thetai=x2* sinz(bei)
  z = -fr*gerd+fi*geid
  geid = -fr*geid-fi*gerd
  gerd = z
  z = aaa2 * expz(-qremb)
  gr = z*sinz(qremb)
  gi = z*cosz(qremb)
  berd = thetar*ger - thetai*gei + gerd*gr - geid*gi
  beid = thetar*gei + thetai*ger + gerd*gi + geid*gr
  ger = fr
  gei = fi
  ber = thetar + ger*gr - gei*gi
  bei = thetai + ger*gi + gei*gr
  go to ( 6 , 4 ),iback
9900 continue
  if( jjj .eq. 3 ) z = vsmall
  return
end subroutine skin
!
! subroutine undrfl.
!
subroutine undrfl ( n1 )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     dummy imitation of ontario hydro univac module.
  n1 = -7654
  return
end subroutine undrfl
!
! subroutine wrte.
!
subroutine wrte ( p, i2, i3, iflag, l, unit, lunit6 )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension p(1),r(10)
  j=0
  do i=i2,i3
     j=j+1
30   r(j)=p(i)*unit
  end do
  goto (1,2,3),iflag
1 write(lunit6,11)(r(i), i=1,j)
  return
2 write(lunit6,22)(r(i),i=1,j)
  return
3 write(lunit6,33) l, (r(i),i=1,j)
  return
11 format(1h0,3x,9e14.5)
22 format(4x,9e14.5)
33 format(1h0,i3,9e14.5)
end subroutine wrte
!
!     subroutine redu44.
!
subroutine redu44 ( a, b, n, m )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !)    Elimination of variables m+1,...n in symmetric matrix a. A is
  !)    stored as triangle (1 element for 1.column,2 for 2.column etc.).
  !)    Result is reduced matrix in columns 1,...m in case of reduction
  !)    (m unequal 0) or negative inverse matrix in columns 1,...n in case
  !)    of inversion (m=0).
  dimension a(1), b(1)
  include 'blkcom.ftn'
  include 'labl44.ftn'
  j = n + 1
  w=unity
  if(m.gt.0) w=-w
  ij=n*j/2
  if ( iprsup  .ge.  4 ) write (lunit6, 2692)  n, m, ij,  ( a(k), k=1, ij )
2692 format ( /,  24h at start of  'redu44' ., 24h       n       m      ij  ,/, &
          24x,  3i8  ,/,  29h (a(k), k=1, ij)  follow ....  ,/, ( 1x,  8e16.7 )  )
3 j=j-1
  if(j.eq.m) return
  h1=a(ij)
  if ( absz(h1)  .gt.  epsiln )  go to 6421
  kill = 86
  lstat(19) = 6421
  lstat(18) = 51
  lstat(13) = n
  lstat(14) = m
  lstat(15) = j
  flstat(15) = epsiln
  flstat(16) = h1
  return
6421 h1=-unity/h1
  b(j)=h1
  ij=ij-j
  k=0
  ik=0
  !                                   begin k-loop
4 ik=ik+k
  i1=ik+1
  k=k+1
  if(k.gt.n) go to 3
  if(k.lt.j) go to 9
  if(w.lt.0.) go to 3
  if(k.eq.j) go to 7
  i=ik+j
5 h2=a(i)
  b(k)=h2*h1
  !                                   begin i-loop
  i2=ik+k
  l=0
  do i=i1,i2
     l=l+1
6    a(i)=a(i)+b(l)*h2
  end do
  if(k.lt.j) go to 4
  i=ik+j
  a(i)=b(k)
  go to 4
  !                                   end i-loop
7 i=ij
  do l=1,j
     i=i+1
8    a(i)=b(l)
  end do
  go to 4
  !                                   end k-loop
9 i=ij+k
  go to 5
end subroutine redu44
!
!     subroutine output.
!
subroutine output(metrik, p, z, switch, kmax, is, k2)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labl44.ftn'
  dimension p(1), z(1)
  character*8 txtmi, txtkm, txtunt
  data txtmi / 6hmiles  /
  data txtkm / 6h  km    /
  if ( jpralt  .eq.  0 )   return
  ll1 = 1
  ll2 = 2
  ll3 = 3
  txtunt = txtmi
  unit = 1.0
  if ( metrik .eq. 1 ) txtunt = txtkm
  if ( metrik .eq. 1 ) unit = 5280. * 12. *2.54d0/ 100000.d0
  check=switch
  write(lunit6,66)
66 format(1h0)
  go to (305,306,307,308,332,333,336,337,340,341), is
305 write(lunit6,301) txtunt
301 format(1h0,12x,35hinverted capacitance matrix (daraf- ,a4, 1h) )
  go to 5
306 write(lunit6,302) txtunt
302 format(1h0,14x,33hinverted susceptance matrix (ohm- ,a4, 1h) )
  go to 5
307 write(lunit6,303) txtunt
303 format(1h0,21x,26hcapacitance matrix (farad/ ,a4, 1h) )
  unit = 1.0 / unit
  go to 5
308 write(lunit6,304) txtunt
304 format(1h0,23x,24hsusceptance matrix (mho/ ,a4, 1h) )
  unit = 1.0 / unit
  go to 5
332 write(lunit6,330) txtunt
330 format(1h0,16x,31hinverted impedance matrix (mho- ,a4, 1h) )
  go to 5
333 write(lunit6,331) txtunt
331 format(1h0,25x,22himpedance matrix (ohm/ ,a4, 1h) )
  unit = 1.0 / unit
  go to 5
336 write(lunit6,334)
334 format(1h0,19x,33htransfer admittance matrix (mhos))
  go to 5
337 write(lunit6,335)
335 format(1h0, 16x, 36htwice shunt admittance matrix (mhos))
  go to 5
340 write(lunit6,338)
338 format(1h0,20x,32htransfer impedance matrix (ohms))
  go to 5
341 write(lunit6,339)
339 format(1h0, 11x, 41hone half of shunt impedance matrix (ohms))
5 go to (320,321,322),k2
320 write(lunit6,309)
309 format(1h+,53x,37hfor the system of physical conductors)
  go to 6
321 write(lunit6,310)
310 format(1h+,53x,45hfor the system of equivalent phase conductors)
  go to 6
322 write(lunit6,311)
311 format(1h+,53x,65hfor the symmetrical components of the equivalent phase conductors)
6 if (k2.eq.3  ) go to 3
  write (lunit6,312)
312 format(1h ,30x,54hrows and columns proceed in same order as sorted input)
4 k=0
  if (is .eq. 8)  write(lunit6, 350)
  if (is .eq.10)  write(lunit6, 350)
350 format(' Sum of two equal shunt admittances at both terminals or its inverse, printed to conform to transients program input format')
  kk=0
  ki = 0
1 k = k+1
  if(k.gt.kmax) return
  icount=0
  i2 = ki + 1
  i3=i2+8
  ki = ki+k
7 if(i3.gt.ki) i3=ki
  if(icount.eq.0) go to 8
  call wrte ( p(1), i2, i3, ll1, l, unit, lunit6 )
10 if ( check  .gt.  0.0 ) call wrte ( z(1), i2, i3, ll2, l, unit, lunit6 )
  if(i3.eq.ki) go to 1
  i2=i3+1
  i3=i2+8
  go to 7
8 icount=1
  if(k2.eq.3) go to 20
  l=k
21 call wrte ( p(1), i2, i3, ll3, l, unit, lunit6 )
  go to 10
3 if(kmax.eq.2) go to 400
  write(lunit6,313)
313 format(' ', 25x, 'Rows proceed in sequence 0,1,2, 0,1,2 etc. and columns proceed in sequence 0,2,1, 0,2,1 etc.')
  go to 4
400 write(lunit6,401)
401 format(' ', 38x, 'This is a two-phase line. Rows and columns proceed in sequence 0,1')
  if(is.le.4) check=-1.0
  go to 4
20 l=kk
  kk=kk+1
  if(kk.eq.3) kk=0
  go to 21
end subroutine output
!
! subroutine outspc.
!
subroutine outspc(p, z, kmax, metrik, fmipkm)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension p(9), z(9)
  include 'blkcom.ftn'
  include 'labl44.ftn'
  if(kmax.eq.4) go to 999
  write(lunit6,222)
222 format (/, ' Special output for mutuals not applicable to this case')
  return
999 aa= valu7 *(z(8)-z(9))
  bb= -valu7 *(p(8)-p(9))
  a2=p(7)-onehaf*(p(8)+p(9))
  b2=z(7)-onehaf*(z(8)+z(9))
  a1=a2+aa
  b1=b2+bb
  a2=a2-aa
  b2=b2-bb
  a0=p(7)+p(8)+p(9)
  b0=z(7)+z(8)+z(9)
  c0=sqrtz(a0**2+b0**2)
  c1=sqrtz(a1**2+b1**2)
  c2=sqrtz(a2**2+b2**2)
  if(metrik .eq. 1)go to 1120
  write(lunit6,1111) c1,c2,c0
1111 format(' Mutual impedance  positive= ', f8.5, ' Ohm/mile  negative= ', f8.5, ' Ohm/mile  zero= ', f8.4, ' Ohm/mile')
  return
1120 c1 = c1 * fmipkm
  c2 = c2 * fmipkm
  c0 = c0 * fmipkm
  write(lunit6, 1121) c1, c2, c0
1121 format(' Mutual impedance  positive= ', f8.5, ' Ohm/km    negative= ', f8.5, ' Ohm/km    zero= ', f8.4, ' Ohm/km')
  return
end subroutine outspc
!
!     end of file: over44.for
!
