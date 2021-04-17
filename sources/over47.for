!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over47.for
!
!
!     subroutine subr47.
!
subroutine subr47
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'deck47.ftn'
  include 'labl47.ftn'
  dimension lltemp(20)
  dimension rtg(1), itg(1), ctg(1)
  complex*16 ctg
  equivalence (karray(1), itg(1), rtg(1), ctg(1))
  if (iprsup .ge. 1) write (lunit6, 4567)
4567 format (24h  begin module "subr47".)
  n8 = nchain
  if ( kburro .eq. 1)  n8 =29
1000 call  dimens ( lltemp(1), n8, trash, trash )
  n7 = lltemp(2) * nbyte(4) / nbyte(3)
  cc = 2 * nbyte(2)
  ppa = nbyte(3)
  cc = cc / ppa
  dd = 2 + ktrlsw(3)
  ppa = 12. + 12. * dd * dd * ( 1. + 3. * cc * cc )
  ppb = 36. + dd * ( 5. + cc )
  cc = n7 - 50
  cc = 2. * ppa * cc + ppb * ppb
  cc = ( sqrtz(cc) - ppb ) / ppa
  ldm = cc
  ldn = cc * dd
  if ( iprsup  .gt.  0 )  write ( lunit6, 2000 )  ldm, ldn
2000 format ( 33h  the cable number is limited to ,   i4, /,    37h  the conductor number is limited to ,  i4  )
  ldn2 = ldn + ldn
  lmq = ldm * ldm
  lnq = ldn * ldn
  lnq2 = lnq + lnq
  loq = 3 * ldm
  iof01 = 1
  iof02 = iof01 + ldn
  iof03 = ( iof02 + ldm ) * nbyte(4) / nbyte(3) + 2
  iof04 = iof03 + ldm
  iof05 = iof04 + ldm
  iof06 = iof05 + ldm
  iof07 = iof06 + ldm
  iof08 = iof07 + ldm
  iof09 = iof08 + ldm
  iof10 = iof09 + ldm
  iof11 = iof10 + ldm
  iof12 = iof11 + ldm
  iof13 = iof12 + ldm
  iof14 = iof13 + ldm
  iof15 = iof14 + ldm
  iof16 = iof15 + ldn
  iof17 = iof16 + ldn
  iof18 = iof17 + ldn
  iof19 = iof18 + ldn
  iof20 = iof19 + lnq
  iof21 = iof20 + lnq
  iof22 = iof21 + lnq
  iof23 = iof22 + lnq
  iof24 = iof23 + lmq
  iof25 = iof24 + lmq
  iof26 = iof25 + lmq
  iof27 = iof26 + lmq
  iof28 = iof27 + lmq
  iof29 = iof28 + lmq
  iof30 = iof29 + loq
  iof31 = iof30 + loq
  iof32 = iof31 + loq
  iof33 = iof32 + loq
  iof34 = iof33 + 3 * ldn
  iof35 = iof34 + 7 * ldm
  iof36 = (iof35 + lnq2) * nbyte(3) / (nbyte(2)*2) + 2
  iof37 = iof36 + ldn
  iof38 = iof37 + lnq
  iof39 = iof38 + lnq
  iof40 = iof39 + lnq
  iof41 = iof40 + lnq
  iof42 = iof41 + lnq
  iof43 = iof42 + lnq
  iof44 = iof43 + lnq
  iof45 = iof44 + lnq
  iof46 = iof45 + lnq
  iof47 = iof46 + lnq
  iof48 = iof47 + lnq
  iof49 = iof48 + lnq
  iof50 = iof49 + lnq
  iof51 = iof50 + lnq
  iof52 = iof51 + lnq
  iof53 = iof52 + lnq
  c     step over last vector to find total memory requirements:
  n13 = ( iof53 + lnq2 ) * 2 * nbyte(2) / nbyte(3)
  if ( n13  .le.  n7 )  go to 7835
  lstat(19) = 7835
  kill = 225
  go to 7842
7835 call guts47 (itg(iof01), itg(iof02), rtg(iof03), rtg(iof04), rtg(iof05), rtg(iof06), rtg(iof07), rtg(iof08), rtg(iof09), &
       rtg(iof10), rtg(iof11), rtg(iof12), rtg(iof13), rtg(iof14), rtg(iof15), rtg(iof16), rtg(iof17), rtg(iof18), rtg(iof19), &
       rtg(iof20), rtg(iof21), rtg(iof22), rtg(iof23), rtg(iof24), rtg(iof25), rtg(iof26), rtg(iof27), rtg(iof28), rtg(iof29), &
       rtg(iof30), rtg(iof31), rtg(iof32), rtg(iof33), rtg(iof34), rtg(iof35), ctg(iof36), ctg(iof37), ctg(iof38), ctg(iof39), &
       ctg(iof40), ctg(iof41), ctg(iof42), ctg(iof43), ctg(iof44), ctg(iof45), ctg(iof46), ctg(iof47), ctg(iof48), ctg(iof49), &
       ctg(iof50), ctg(iof51), ctg(iof52), ctg(iof53), ldm, ldn, ldn2, lnq2  )
7842 if ( kill .gt. 0 ) lstat(18) = nchain
  lastov = 47
  return
end subroutine subr47
!
!     subroutine guts47.
!
subroutine guts47(ngg, ncpp, al1i, al2i, al3i, dci, thc, bio, bi1, bi2, bi3, bi4, bi5, hi,  di, gn, rad, wy, zy, yz, sc, qc, &
     dr0, th0, al0, dij, dir, ang, roi, esi, usi, usr, gi, radi, yzn, qn, yo, ys, yc, zs, zc, ze, zp, zpc, a, ai, b, bi, ca, &
     cb, cc, cd, f, ldm, ldn, ldn2, lnq2)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labl47.ftn'
  include 'volt45.ftn'
  complex*16 ys(ldn, ldn), yc(ldn, ldn), zs(ldn, ldn), zc(ldn, ldn)
  complex*16 ze(ldn, ldn), ai(ldn, ldn), bi(ldn, ldn), zp(ldn, ldn)
  complex*16 zpc(ldn, ldn), a(ldn, ldn),  b(ldn, ldn), yo(ldn, ldn)
  complex*16 qn( ldn ), cmplxz, cj , f(ldn, ldn2)
  complex*16 ca(ldn, ldn), cb(ldn, ldn), cc(ldn, ldn), cd(ldn, ldn)
  character*8 bufsem, cname, text1, text2, text3, text4
  dimension bufsem(14)
  dimension ngg(ldn), ncpp(ldm), rad(ldn), yzn(lnq2), wy(ldn)
  dimension sc(ldn,ldn), qc(ldn,ldn), gi(ldn,3)
  dimension dr0(ldm, ldm), th0(ldm, ldm), al0(ldm, ldm)
  dimension dij(ldm, ldm), dir(ldm, ldm), ang(ldm, ldm)
  dimension radi(ldm, 7),   zy(ldn, ldn),  yz(ldn, ldn)
  dimension roi(ldm, 3), esi(ldm, 3), usi(ldm, 3), usr(ldm, 3)
  dimension bio(ldm), bi1(ldm), bi2(ldm), bi3(ldm), bi4(ldm)
  dimension bi5(ldm), dci(ldm), thc(ldm),  hi(ldm),  di(ldn)
  dimension al1i(ldm),   al2i(ldm),   al3i(ldm),   gn(ldn)
  data text1   /  6hpunch    /
  data text2   /  6h line   /
  data text3   /  6hcable   /
  nrp =  0
  mrr =  0
  junit4=77
  mispun = 0
  lnq1 = ldn * ldn + 1
  if (iprsup .ge. 1) write (lunit6, 2314)  lastov, ialter, lunit2, lunit5, speedl, epsiln, twopi
2314 format ( /,  18h enter  'guts47' ., 32h  lastov  ialter  lunit2  lunit5,  14x,  6hspeedl, &
       14x,  6hepsiln,  15x,  5htwopi  ,/,  18x,  4i8,  3e20.11  )
  rewind lunit2
  rewind lunit3
  rewind lunit4
  if ( iprsup .ge. 1) write (lunit6,*)  ' %%--  ready to rewind lunit9.'
  rewind lunit9
  numaki = 0
  ncmod = 0
  fzero = 0.0
  cimag1 = cmplxz(fzero, unity)
  creal1 = cmplxz(unity,fzero)
  czero = cmplxz(fzero, fzero)
  if (lastov .eq. 45) numaki = 9
  if (lastov .eq. 39) numaki = 9
  if (lastov .eq. 43) numaki = 9
  if (ialter .ne. 2) go to 7407
  l5save = lunit5
  lunit5 = lunit2
7407 ldisfr  =  locf( voltk(1) )     -     locf( volti(1) )
  spdlgt = speedl
  logsix = lunit6
  iprs47 = iprsup
  ten = 10.
  value2 = epsiln
  value1 = .5 * value2/1000.
  value3 = 20. * 1000. / alogz(ten)
  value4 = 1.7810 7241 7990 d0
  value5 = tenm3
  valu14 = 2.3025 8509 3000 d0
  liu = 0
  pai = twopi/2.
  u0 = 2 * twopi * tenm6 / ten
  spl2 = speedl ** 2
  e0 = 1./u0/spl2
  u2p=u0/twopi
  e2p=e0*twopi
  npais = 0
  ncros = 0
  irsep = 0
  xmajor = 1.
  rsg = 1.e10
5 ngrnd = 0
  !     read input card using cimage.
  call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
4230 format ( 13a6, a2 )
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
  if ( bufsem(1)  .ne.  text1 )   go to 8214
  mispun = 1
  go to 5
8214 continue
  read (unit = abuff, fmt = 900) n1, n2, n3, n4, n5, n6, n7, n8, n9
900 format(20i5)
  nenerg = 0
  if (n1 .ne. 0)   go to 6
  if (n2 .ne. 0)   go to 4501
  if (n3 .ne. 0)   go to  4501
  write (kunit6, 1200)
1200 format(50h+blank card terminating  'cable constants'  cases.)
  call interp
  if ( lastov  .eq.  1 )   go to 7439
  n1 = lastov
  lastov = nchain
  nchain = n1
  if ( ialter  .eq.  2 ) lunit5 = l5save
  if (ipunch .eq. 0)   go to 7496
  d1 = 0.0
  write (lunit9)  d1, d1, d1
  rewind lunit9
  if (iprs47 .ge. 1) write (lunit6, 11200)  d1, d1, d1, ipunch
11200 format (22h last record on unit9., 3e15.6, /, 9h ipunch =, i10)
7496 go to 9900
7439 lastov = nchain
  nchain = 51
  go to 9900
4501 kill = 172
  lstat(14) = n1
  lstat(19) = 4501
  go to 9200
6 itypec = n1
  isyst = n2
  iearth = n4
  kmode = n5
  izflag = n6
  iyflag = n7
  !     initialize optional "punch" card variables (wsm, nov, 81):
  npais = 0
  ncros = 0
  irsep = 0
  xmajor = 0.0
  rsg = 0.0
  cname = blank
  if ( mispun  .ne.  1 )   go to 8234
  !     read input card using cimage.
  call cimage
  read (unit = abuff(1), fmt = 903) npais, ncros, irsep, xmajor, rsg, cname
903 format( 3i5, 2e10.1, a1 )
8234 mispun = 0
  go to (10, 20, 20) , itypec
  !     overhead line data input
10 ncct = n3
  if (n9 .ne. 0)  nenerg = n9
  write (kunit6, 1212)  itypec,isyst,ncct,iearth,kmode,izflag, iyflag,nenerg
1212 format(21h+misc. data for lines, 1x, 8i3)
  if(npais.ne.0) write(lunit6,1213) npais,xmajor,rsg,cname
1213 format(21h misc. data for lines, i3, 2e10.3, a1 )
  do i=1, ncct
     im = 4*(i-1) + 1
     in = im + 3
     !     read input card using cimage.
     call cimage
     read (unit = abuff(1), fmt = 4230) bufsem
     if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
     read (unit = abuff(1), fmt = 900) (ncpp(j), j = im, in)
     write (kunit6, 1220)
1220 format (42h+additional data for phase & ground wires.)
     !     read input card using cimage.
     call cimage
     read (unit = abuff(1), fmt = 4230) bufsem
     if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
     read (unit = abuff(1), fmt = 901) (radi(i, j), j = 1, 4), (dr0(i, j), j = 1, 2)
901  format(8e10.1)
     write (kunit6, 1230)
1230 format (40h+geometrical data of bundled conductors.)
     !     read input card using cimage.
     call cimage
     read (unit = abuff(1), fmt = 4230) bufsem
     if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
     read (unit = abuff(1), fmt = 901) (roi(i, j), usr(i, j), j = 1, 2)
     write (kunit6, 1240)
1240 format (49h+resistivity & permeability of phase & gd. wires.)
13 end do
  ncc= 0
  npc = 0
  do i=1, ncct
     im = 4*(i-1) + 1
     in = im + 1
     ncc= ncc+ ncpp(im)
     npc=npc + ncpp(im) + ncpp(in)
15 end do
  np2 = npc
  lines = ( npc + 1) / 2
  im = -1
  do j=1, lines
     im = im + 2
     in = im + 1
     !     read input card using cimage.
     call cimage
     read (unit = abuff(1), fmt = 4230) bufsem
     if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
     read (unit = abuff(1), fmt = 901) (thc(i), dci(i), di(i), i = im, in)
     if (j .gt. 1)   go to 19
     write (kunit6, 16)
16   format (46h+heights and horizontal distance of each line.)
19 end do
  go to 85
  !     cable data input
20 npc = n3
  npp = n8
  ngrnd=n9
  ncc = 0
  npc2 = 0
  if (itypec .eq. 3)   go to 3010
  write (kunit6, 3210) itypec, isyst,npc,iearth,kmode,izflag, iyflag, ngrnd
3210 format(22h+misc. data for cables, 8i3)
  if(npais.ne.0) write(lunit6,3211) npais,ncros,irsep,xmajor,rsg, cname
3211 format(22h misc. data for cables, 3i3 , 2e8.1, a1 )
  if (isyst .ne. -1)   go to 21
  if (iearth .ne. 99)   go to 21
  kill = 173
  lstat(19) = 3210
  go to 9200
21 np2 = npc * 3
  go to 3050
3010 write (kunit6, 3030)  itypec, isyst, npc, iearth, kmode, izflag, iyflag, npp, ngrnd
3030 format (26h+misc. data for pipe cable, 9i3)
  if(npais.ne.0) write(lunit6,3031) npais,ncros,xmajor,rsg,cname
3031 format(26h misc. data for pipe cable, 2i3, 2e8.1, a1 )
  if(npp.ne.1) npp=1
  !     read input card using cimage
  call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if (ialter .ne. 2) write (lunit2, 4230)  bufsem
  read (unit = abuff(1), fmt = 901) (radp(i), i = 1, 3), rop, usp, es1, es2
  write (kunit6, 3035)
3035 format (21h+pipe characteristic.)
  if (radp(3) .ne. 0.0) go to 3036
  radp(3) = radp(2) + 100. * epsiln
  es2 = 1.0
  !     read input card using cimage
3036 call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if (ialter .ne. 2) write (lunit2, 4230)  bufsem
  read (unit = abuff(1), fmt = 901) (dci(i), thc(i), i = 1, npc)
  write (kunit6, 3040)
3040 format (34h+relation between cables and pipe.)
  np2 = npc * 3  +  npp
  !     read input card using cimage.
3050 call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
  read (unit = abuff(1), fmt = 900) (ncpp(i), i = 1, npc)
  write (kunit6, 3220)
3220 format (36h+number of conductors in each cable.)
  if ( itypec  .eq.  2 )   go to 22
  i = npc
3223 thc(i) = thc(i) - thc(1)
  i = i - 1
  if ( i  .gt.  0 )   go to 3223
22 do i=1, npc
     !     read input card using cimage.
     call cimage
     read (unit = abuff(1), fmt = 4230) bufsem
     if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
     read (unit = abuff(1), fmt = 901) (radi(i, j), j = 1, 7)
     write (kunit6, 3230) (radi(i,j), j=1, 7)
3230 format(7h+radii., 1x, 7f6.2 )
     !     read input card using cimage.
     call cimage
     read (unit = abuff(1), fmt = 4230) bufsem
     if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
     read (unit = abuff(1), fmt = 901) (roi(i, j), usr(i, j), usi(i, j), esi(i, j), j= 1, 2)
     write (kunit6, 3240)
3240 format(50h+physical constants for conductors and insulators.)
     if (ncpp(i) .le. 2)  go to 3245
     !     read input card using cimage.
     call cimage
     read (unit = abuff(1), fmt = 4230) bufsem
     if (ialter .ne. 2) write(lunit2, 4230)  bufsem
     read (unit = abuff(1), fmt = 901) roi(i, 3), usr(i, 3), usi(i, 3), esi(i, 3)
     write(kunit6, 3240)
     if ( radi(i,7) .ne. 0. ) go to 3239
     radi(i,7)=radi(i,6) + 100.*epsiln
     usi(i,3) = 1.0
     esi(i,3) = 1.0
     go to 3239
3245 usi(i,3) = 1.0
     esi(i,3) = 1.0
     if (ncpp(i) .eq. 2)  go to 3235
     if (radi(i,3) .ne. 0. ) go to 3231
     radi(i,3)=radi(i,2) + 100.*epsiln
     usi(i,1) = 1.0
     esi(i,1) = 1.0
3231 radi(i,4) = radi(i,3)
     radi(i,5) = radi(i,4)
     radi(i,6) = radi(i,5)
     radi(i,7) = radi(i,6)
     usi(i,2) = 1.0
     esi(i,2) = 1.0
     usr(i,2) = 1.0
     usr(i,3) = 1.0
     roi(i,2) = roi(i,1)
     roi(i,3) = roi(i,2)
     go to 3239
3235 if (radi(i,5) .ne. 0.) go to 3237
     radi(i,5)=radi(i,4) + 100.*epsiln
23235 usi(i,2) = 1.0
     esi(i,2) = 1.0
3237 radi(i,6) = radi(i,5)
     radi(i,7) = radi(i,6)
     usr(i,3) = 1.0
     roi(i,3) = roi(i,2)
3239 if(ncpp(i) .lt. 2)  go to 23
     npc2 = npc2 + 1
     ncmod = ncmod + 1 - ngrnd
     if ( ncpp(i)  .ge.  3 )   ncmod = ncmod + 1
23   ncc = ncc + ncpp(i)
  end do
  npc2=npc+npc2
  if (itypec .eq. 2)   go to 24
  ncc = ncc + npp
  if (npp .eq. 0)   go to 85
  !     read input card using cimage
  call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if (ialter .ne. 2) write (lunit2, 4230)  bufsem
  read (unit = abuff(1), fmt = 901) (hi(i), di(i), i = 1, npp)
  write (kunit6, 3055)
3055 format (45h+height and horizontal distance of each pipe.)
  go to 85
24 lines = (npc + 3) / 4
  do j = 1, lines
     im = 4 * (j-1) + 1
     in = im + 3
     !     read input card using cimage.
     call cimage
     read (unit = abuff(1), fmt = 4230) bufsem
     if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
     read (unit = abuff(1), fmt = 901) (hi(i), di(i), i = im, in)
     if (j .gt. 1)   go to 93
     write (kunit6, 3250)
3250 format(46h+height and horizontal distance of each cable.)
93 end do
85 if(npc .le. ldm  .and.  ncc .le. ldn) go to 1945
  kill = 225
  lstat(14) = ldn
  lstat(15) = ldm
  lstat(19) = 85
  go to 9200
1945 do i = 1, ncc
1960 ngg(i) = 0
  end do
  if ( ngrnd  .le.  3 )  go to 3983
  call cimage
  read (unit = abuff(1), fmt = 1999) (ngg(i), i = 1, npc ), npipe
1999 format ( 2x, 78i1 )
  do i = 1, npc
     if ( ngg(i)  .le.  1 ) go to 1919
     jdx1 = npc + i
     jdx2 = npc2 + i
     kgc = ngg(i)
     if ( kgc  .lt.  5 )  ngg(i) = 0
     if (kgc .ne. 3  .and.  kgc .ne. 6) ngg(jdx1) = 1
     if (kgc .ne. 2  .and.  kgc .ne. 5) ngg(jdx2) = 1
1919 end do
  if ( itypec  .eq.  2 ) go to 3983
  ngg(ncc) = npipe
3983 if (numaki .eq. 0)   go to 87
  write (lunit4)  itypec, isyst, npc, ncc
  if (iprs47 .ge. 1) write (lunit6, 86)  itypec, isyst, npc, ncc
86 format(16h at 86 of subr47, /, 4i5)
  !     read input card using cimage
87 call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
  read (unit = abuff, fmt = 902) d9, freq, ik, ips, dist, j13, j14
902 format (2e15.6, 2i5, f8.3, i10, i2)
  ipunch = j13
  roe = d9
  itrnsf = j14
  if (freq .eq. 0.)   freq = statfr
  if (ips .eq. 0)   ips = 1
  write (kunit6, 3256)  roe, freq, ik, ips, dist, ipunch, itrnsf
3256 format (11h+freq. card, 2e10.3, 2i3, e8.3, 2i2)
  call interp
  if (iearth .ne. 99)   go to 95
  !     read input card using cimage.
  call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
  read (unit = abuff, fmt = 901) dep1, dep2, roe3, roe4
  write (kunit6, 3280)
3280 format (49h+depths & resistivities of 2nd & 3rd layer earth.)
  !     read input card using cimage.
  call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
  read (unit = abuff, fmt = 901) htoj2, htoj3, htoj4, hyud2, hyud3, hyud4
  write (kunit6, 3290)
3290 format (49h+permeability & permittivity of the three layers.)
  alf1 = roe / roe3
  alf2 = roe / roe4
95 continue
  anpais=iabs (npais)
  xtotal=xmajor*anpais
  if(npais.eq.0) xtotal=xmajor
  if(npais.ge.0) call nyan(itypec,npc,ncc,ncpp,ngrnd,ncros,npais,ldm)
96 if (itypec .ne. 1)   go to 200
  !     overhead line data outputs & precal.
  jnc = 1
  do i=1, ncct
     im = 4*(i-1) + 1
     do  j=1, 2
        j1 = im + j + 1
        in = ncpp(j1)
        if ( in  .eq.  0 )   go to 120
        j2 = 2 * j - 1
        i1 = i + ncct
        if (in .eq. 1)   go to 111
        d0 = dr0(i,j)/sinz(pai/in)
        usi(i,j) = radi(i,j2)
        in1 = in - 1
        do k=1, in1
110        usi(i,j)=usi(i,j)*d0*sinz(pai/in*k)
        end do
        usi(i1,j)=usi(i,j)**(1./in)
        go to 120
111     usi (i1, j) = radi(i,j2)
120  end do
     if (iprsup .ge. 1) write (lunit6, 4001)  i, (usi(i1, j), j=1, 2)
4001 format (1x,7hcircuit,i3,3h0  ,32hequivalent radius of phase wire , e10.4,/, 34x, 11hground wire, e10.4)
     do j=1, 2
        j1 = im + j - 1
        if ( ncpp(j1)  .ne.  0 )   go to 123
        usi(i,j) = 0.0
        esi(i,j) = 0.0
        gi(i,j) = 0.0
        go to 125
123     j1 = 2 * j
        j2 = j1 - 1
        usi(i,j)  =  radi(i,j1) * sqrtz( u0*usr(i,j) / roi(i,j) )
        esi(i,j)  =  radi(i,j2) * sqrtz( u0*usr(i,j) / roi(i,j) )
        if ( radi(i,j1)  .lt.  radi(i,j2) )   go to 124
        kill = 174
        lstat(14) = i
        flstat(15) = radi(i,j2)
        flstat(16) = radi(i,j1)
        lstat(19) = 123
        go to 9200
124     gi(i,j) = roi(i,j)/pai/(radi(i,j2)**2 - radi(i,j1)**2)
        !     1          /ncpp(im+j+1)
125  end do
130 end do
  do j=1,2
     do i=1,ncct
        i1=i+ncct
        im=4*(i-1)+1
        j1=im+j-1
        j1 = ncpp(j1)
        if(j1.eq.0) go to 135
        j2 = j1 + jnc - 1
        do k=jnc,j2
133        rad(k)=usi(i1,j)
        end do
        jnc = j2 + 1
135  end do
136 end do
  write (lunit6, 170)
170 format(//, 132h------------------------------------------------------------------------------------------------------------------------------------,//,1x)
  if ( isyst  .ne.  0 )   go to 140
  write (lunit6, 180)
180 format (47h table of overhead untransposed line parameters,/,1x)
  go to 142
140 write (lunit6, 182)
182 format (45h table of overhead transposed line parameters,/,1x)
142 j1 = npc - ncc
  write (lunit6, 171)  ncc, j1
171 format(   10x,27htotal number of phase wires,i3,3x,12hground wires ,i3, /, 1x)
  do i=1, ncct
     i1 = i + ncct
     im = 4*(i-1) + 1
     j1 = im + 1
     j2 = im + 2
     j3 = im + 3
     write (lunit6, 184)  i, ( radi(i,j), j= 1,  2), ncpp(j2), usi(i1,1)
184  format ( 10x, 7hcircuit, i2, /, 10x, 27hradius | phase wire  (outer,  e11.4,  9h  , inner, e11.4,  3h )*,  i2,  8h bundles, /, 20x, 13hequiv. radius, e11.4  )
     j1=ncpp(j1)
     if(j1.eq.0) go to 148
     write (lunit6, 185)  ( radi(i,j), j= 3,  4), ncpp(j3), usi(i1,2)
185  format (19x,  18hground wire (outer,  e11.4,  9h  , inner, e11.4,  3h )*,  i2,  8h bundles, /,  20x, 13hequiv. radius, e11.4  )
148  write(lunit6,186) (roi(i,j),j=1,2),(usr(i,j),j=1,2),(gi(i,j),j=1,2)
186  format ( 10x,  31hresistivity(ohm-m) | phase wire, e11.4,  3x,  11hground wire,  e11.4,  /,  10x,23hrelative permeability |, 8x,  f11.4,  14x,  f11.4,  /, &
          10x,  22hdc resistance(ohm/m) |,  9x,  e11.4,14x,  e11.4  )
150 end do
  do i=1, npc
160  hi(i) = (2. * dci(i) + thc(i) ) / 3.
  end do
  write (lunit6, 165)  (i, hi(i), i=1, npc)
165 format(10x, 29heffective height of the line , 5(i5, f12.5))
  write(lunit6,915) (di(i),i=1,npc)
915 format ( 10x,  29hdistance between phases     |,  5f17.5  )
  write (lunit6, 917)  freq, ik, ips
917 format (10x, 20hbeginning frequency ,5x,e15.6,5x,18hnumber of decades , i5, 5x, 32hnumber of points in each decade , i5)
  write(lunit6,913) roe
913 format (10x, 19hearth | resistivity, f12.5)
  if (iearth .ne. 99) go to 167
  write(lunit6,916) dep1,dep2,roe,roe3,roe4,htoj2,htoj3,htoj4,hyud2,hyud3,hyud4
916 format(10x,13h3-layer earth ,/,10x,30hdepth from surface , 1st layer, f7.2,3x,9h2nd layer,f8.2,3x,21h3rd layer to infinite,/, &
       10x, 19hearth resistivity 0, e18.2, 6x, e14.2, 6x, e18.2,/, 10x,23hrelative permeability 0, e14.2, 6x,e14.2,6x,e18.2,/, &
       10x,23hrelative permittivity 0, e14.2,6x,e14.2,6x,e18.2)
167 nw = npc
  nz = npc
  ngrnd=ncc
  go to 700
  !     cable data outputs and precalculation
200 do i=1,npc
     rad(i) = radi(i,7)
     do j=1,3
        gi(i,j) = speedl/sqrtz(esi(i,j))
210  end do
  end do
  write (lunit6, 170)
  if (isyst .ge. 0)   go to 211
  write (lunit6, 280)
280 format (39h table of under-ground cable parameters,/,1x)
  go to 214
211 if (isyst .gt. 0)   go to 213
  write (lunit6, 281)
281 format(58h table of cable parameters for cables on the earth surface)
  go to 214
213 write(lunit6,282)
282 format (35h table of overhead cable parameters,/,1x)
214 if ( ngrnd  .gt.  3 ) go to 1980
  if ( ngrnd  .ne.  0 )  go to 325
  ngrnd = ncc
  go to 215
325 if ( ngrnd  .gt.  1 )  go to 330
  ngrnd = ncc
  if ( itypec  .eq.  2 ) go to 215
  ngrnd = ncc - 1
  go to 215
330 if( ngrnd .gt. 2 ) go to 3333
  ngrnd = npc2
  go to 215
3333 ngrnd = npc
  go to 215
1980 do i = 1, ncc
     kpd = ncc + 1 - i
     if ( ngg(kpd)  .gt.  0 ) go to 1982
     ngrnd = kpd
     go to 215
1982 end do
  write ( lunit6, 1777 )
1777 format( 43h  no ungrounded conductor needs calculation )
  call stoptp
215 if (itypec .eq. 2)   go to 1215
  write (lunit6, 370)
370 format (17h pipe type cables,/,1x)
  if (npp .ne. 0)   go to 216
  write (lunit6, 371)
371 format (31h earth return path not included)
216 write (lunit6, 380)
380 format (10x, 23h pipe and its insulator)
  if (npp .ne. 0)  go to 1216
  radp(2) = fltinf
  radp(3) = fltinf
1216 write (lunit6, 381)  (radp(i), i=1,3), rop, usp
381 format (10x, 'pipe | inner radius', e13.5, 2x, 'outer', e13.5, 2x, 'houter radius of pipe insulator', e13.5,/, 18x, &
       'resistivity(Ohm-m)', e16.5, 2x, 'relative permeability', f8.1)
  write (lunit6, 382)  es1, es2
382 format (10x, 40hinsulator | relative permittivity  inner, f8.1, 2x, 5houter, f8.1)
  if (es2 .eq. 0.0)   es2 = 1.0
  if (npp .ne. 0) write (lunit6, 383)  (i, hi(i), di(i), i=1, npp)
383 format (10x, 4hpipe, i2, 27h | dist. from earth surface, f9.4, 1hm, 3x, 17hdist. from pipe 1, f9.4, 1hm)
  write (lunit6, 390)
390 format (10x, 28hinner conductors (sc cables))
  write (lunit6, 391)  (i,dci(i), i=1, npc)
391 format (10x, 24hdist. from pipe center |, 6(i5, f9.4))
  write (lunit6, 392)  (thc(i), i=1, npc)
392 format (10x, 24hangle to first cable   |, 6(f14.4))
1215 do i=1, npc
     jn=ncpp(i)
     write (lunit6, 284) i, (j, radi(i,j), j=1, 7)
284  format(10x,5hphase,i2,1x,17hboundary radii  | ,/10x,7(i3,e13.5))
217  write (lunit6, 285)  (roi(i, j), j =1,3), (usr(i, j), j=1,3),(j,usi(i,j),esi(i,j),gi(i,j),j=1,jn)
  end do
285 format (18x, 'resistivity(Ohm-m)| core', e12.5, 3x, 'sheath', e12.5, 3x, 'armor', e12.5,, /, 18x, 'relative permeability |  ', &
       f12.5, 9x, f12.5, 8x, f12.5, /, 3(18x, 'insulator', i2, '| (relative) permeability', f6.2,3x, &
       'permittivity', f6.2, 3x, 'velocity(m/s)', e12.5, /, 1x))
  if(itypec . ne . 3 )  go to 220
  three = 3.0
  rlimit = ( 1.0 + 2.0/sqrtz(three)) * radi(1,7)
  if(npc.eq.2) rlimit=2.0*radi(1,7)
  if(npc.eq.1) rlimit=radi(1,7)
  if(radp(1).gt.rlimit) go to 221
  write(lunit6,393)
393 format(1h0,10x,59hphysically inner conductors cannot be exsisted in the pipe.    /11x,56hplease check the radii of the pipe and inner conductors.   //,1x)
  call stoptp
220 write(lunit6,914) (i, hi(i), i= 1,npc )
914 format (10x, 29hdistance from earth surface |, 5(i5, f12.5))
221 do i=1,npc
     if ( i .eq. 1 ) go to 223
     i1=i-1
     if(ncpp(i1).ge.ncpp(i)) go to 223
     write(lunit6,289)
     call stoptp
289  format('0', 10x, "Invalid data of 'ncpp(i)', no further calculation.  It should be 'ncpp(i-1).ge.ncpp(i)'.", /, 11x,, 'Please check your data.'  )
223  continue
     bio(i) = radi(i,1) * sqrtz(u0/roi(i,1) *usr(i,1))
     bi1(i) = radi(i,2) * sqrtz(u0/roi(i,1) *usr(i,1))
     al1i(i) = alogz(radi(i,3) / radi(i,2) )
     bi2(i) = radi(i,3) * sqrtz(u0/roi(i,2) * usr(i,2))
     bi3(i) = radi(i,4) * sqrtz(u0/roi(i,2) * usr(i,2))
     al2i(i) = alogz(radi(i,5) / radi(i,4) )
     bi4(i)=radi(i,5)*sqrtz(u0/roi(i,3)*usr(i,3))
     bi5(i)=radi(i,6)*sqrtz(u0/roi(i,3)*usr(i,3))
     al3i(i) = alogz(radi(i,7) / radi(i,6) )
230 end do
  if (iprs47 .gt. 1) write (lunit6, 2437)  npc, ncc, npp, ncct, numaki, lastov, ipunch, ialter, iearth, itypec, isyst, kmode, izflag, iyflag
2437 format ( /,  18h various integers., 40h     npc     ncc     npp    ncct  numaki, 40h  lastov  ipunch  ialter  iearth  itypec, &
       32h   isyst   kmode  izflag  iyflag  ,/,  18x,  14i8  )
  if ( iprsup  .ge.  2 ) write (lunit6, 2441)  ( i, bi1(i), bi2(i), bi3(i), al1i(i), al2i(i), i=1, npc )
2441 format ( /, 75h derived vectors of length  'npc'  in  'subr47' , as matrix grunt begins.  ,/,  1x,  5x,  3hrow,  17x,  3hbi1, &
       17x,  3hbi2,  17x,  3hbi3,  16x,  4hal1i,  16x,  4hal2i  ,/, ( 1x,  i8,  5e20.11 )  )
  if (itypec .eq. 3) call ptzy1(radi,dci,thc,dr0,th0,al0,ldm)
  if (kill .ge. 1)   go to 9200
  nw = npc
  nz = ncc
  if (itypec .ne. 3)   go to 1710
  nw = npp
  if (npp) 1750, 1750, 1711
1710 write (lunit6, 915)  (di(i), i=1, npc)
1711 write (lunit6, 917) freq, ik, ips
  write (lunit6, 913)  roe
  if (iearth .ne. 99)   go to 700
  write (lunit6,916) dep1,dep2,roe,roe3,roe4,htoj2,htoj3,htoj4, hyud2, hyud3, hyud4
700 if ( iprsup  .ge.  3 )  write (lunit6, 1714) (i, rad(i), radp(i), di(i), hi(i), i=1, nw)
1714 format ( /,   25h before call to  'simp' .,  7x,  1hi, 12x,  3hrad,  11x,  4hradp,  13x,  2hdd,  14x,  1hh  ,/, ( 25x,  i8,  4e15.6 )  )
  if(npais.eq.0) go to 610
  write(lunit6,919) cname
  go to 615
610 if(ncros.eq.0) go to 714
615 if(itypec.ne.1) go to 620
  write(lunit6,920) xtotal,xmajor,npais
  go to 714
620 if(npais.gt.0 .and. ncros.eq.0) go to 625
  write(lunit6,921) xtotal,xmajor,npais,rsg
  go to 630
625 write(lunit6,922) xtotal,xmajor,npais
630 if(ncros.ne.0) write(lunit6,923)
  if(npais.lt.0) write(lunit6,924)
  if(npais.gt.0) write(lunit6,925)
919 format('0', 10x, 'Data cards by pi-circuit modeling are punched out for the following specifications ; node name ', a1, "'" )
920 format(10x, 'Total length of overhead line =', e12.5, /, 10x, 'length of one pi-section = ', e12.5, 3x, 'number of pi sections = ', i3)
921 format(10x, 'Total length of cable = ', e12.5, /, 10x, 'length of one major section = ', e12.5, 3x, 'number of major sections = ', i3, /, 10x, &
       'sheath grounding resistance at major section ; ', e12.5)
922 format(10x, 'Total lrngth of cable = ', e12.5, /, 10x, 'length of one major section =  ', e12.5, 3x, 'number of major sections = ', i3   )
923 format(10x, 'The cable is crossbonded.   ')
924 format(10x, 'Discrete pi-circuit modeling  ')
925 format(10x, 'Homogeneous pi-circuit modeling   ')
714 call simp (nw, hi, di, rad, zy, dir, dij, ang, ldm, ldn )
  if (kill .ge. 1)   go to 9200
  if ( iprsup  .lt.  3 )   go to 1750
  write (lunit6, 1719)  nw, nz, isyst, itypec, iearth, npc, ncc
1719 format ( /, 24h after call to  'simp' .,56h      nw      nz   isyst  itypec  iearth     npc     ncc  ,/,24x,  7i8  )
  write (lunit6, 1721)  (  ( zy(i,j), j=1, nw),  i=1, nw )
1721 format ( /, 30h ((zy(i,j), j=1, nw), i=1, nw)  ,/, ( 1x, 8e15.8) )
1750 call ymatrx(isyst, lunit6, ncpp, zy, yz, esi, al0, al1i, al2i, al3i, yzn(1), yzn(lnq1), ldm, ldn)
  if (kill .ge. 1) go to 9200
  if (iprsup .ge. 3) write (lunit6, 1723)  ((yz(i, j), j = 1, nz), i = 1, nz)
1723 format ( /,  58h ((yz(i,j), j=1, nz), i=1, nz)   after call to  'ymatrx' .  ,/,  ( 1x,  8e15.8 )   )
  do i=1, nz
     do j=1, nz
        if ( j  .lt.  i )   go to 702
        ys(i,j) = cmplxz(yz(i,j), fzero)
        ys(j,i) = ys(i,j)
702  end do
  end do
  call minv ( ys, nz, f, ldn, ldn2 )
  if (kill .ge. 1)   go to 9200
  if (itypec .ne. 1)   go to 703
  if (isyst .eq. 2) call transp(ys, ncpp, yzn(1), yzn(ldn2), ca, ldm, ldn)
  if (kill .ge. 1)   go to 9200
703 do i=1, nz
     do j=1, nz
        if ( j  .lt.  i )   go to 705
        yz(i,j)  =  realz ( ys(i,j) )
        yz(j,i) = yz(i,j)
705  end do
  end do
  nx=ngrnd
  if(npais.ge.0 .and. ncros.ne.0) nx=4
  if(npais.ne.0) call gomen(itypec,npc,nx,npais,ncros,irsep,ncpp,ldm)
  if ( iprsup  .ge.  3 ) write (lunit6, 2457)  ( ( yz(i,j), j=1, nz), i=1, nz )
2457 format (/, " in  'subr47' ,   capacitance.   ( (yz(i,j), j=1, nz), i=1, nz )    ", /, (1x, 6e20.11))
  iprint = 0
9003 continue
706 if ( ck1  .ne.  -fltinf )   go to 7446
  if ( dist  .le.  0.0 )   go to 7446
  ck1 = dist
7446 if ( ci1  .ne.  -fltinf )   go to 7449
  if ( roe     .le.  0.0 )   go to 7449
  ci1 = roe
7449 if ( ialter  .eq.  2 ) roe    = ci1
  iii = 0
  kkk = 1
  if ( iprsup  .ge.  1 ) write (lunit6, 2469)  nz, ik, ialter, iprsup, roe, ci1, ck1, dist, yz(1,1), yz(1,2)
2469 format ( /, 40h in  'subr47' ,   done with capacitance., 32h      nz      ik  ialter  iprsup  ,/, 40x,  4i8  ,/,  1x,  17x,  3hroe,  17x,  3hci1, &
       17x,  3hck1,  16x,  4hdist,  13x,  7hyz(1,1),  13x,  7hyz(1,2) ,/,  1x,  6e20.11  )
  if ( ik  .gt.  0 )   go to 7456
  iprint = iprint + 1
  if ( lastov .ne. 39 )  go to 7451
  if ( iprint .gt. 1 )  go to 7451
  kmode = 1
  l1 = 1
  l0 = 0
  d9 = dist * tenm3
!!!   write (*,*)  ' %%-- guts47 write on  lunit9.',
!!!   1             '    dist, d9, =',  dist, d9,
  write (lunit9)  l1, l1, d9, l0, itrnsf
7451 if ( ialter  .ne.  1 )   go to 3007
  volti(iprint) = roe
  voltk(iprint) = freq
  if ( iprint  .le.  ldisfr )   go to 3007
  kill = 170
  lstat(14) = ldisfr
  lstat(19) = 7456
  go to 9200
7456 factor = ips
  factor = valu14 / factor
  fdecad = freq
  iprint = iprint + 1
  voltbc(1) = freq
  voltbc(2) = freq * 10. ** ik
  voltbc(3) = expz(factor)
  d13 = voltbc(2) / voltbc(1)
  voltbc(4) = alogz(d13) / alogz(voltbc(3)) + 1.5
  voltbc(5) = ik
  voltbc(6) = ips
  write (*,*) ' guts47.   below s.n. 7456, voltbc(6) = ',  voltbc(6)
  dist = 0.0
  icheck = iprint - 1
  go to 3007
3005 iii = iii + 1
  fdecad = fdecad * 10.0
  freq = fdecad
  iprint = iprint + 1
  kkk = 1
  go to 3007
3006 pkkk = kkk
  freq = fdecad * expz(pkkk * factor)
  kkk = kkk + 1
  if (kkk .gt. ips)   go to 3005
  iprint = iprint + 1
  if (iprs47 .ge. 1) write (lunit6, 23006)    iprint, kkk, iii, freq, ci1, ck1
23006 format(/, 63h next logarithmacally spaced frequency.  iprint     kkk     iii, 11x, 4hfreq,12x, 3hci1,12x,3hck1,/,39x,3i8,3e16.6)
3007 if (numaki .eq. 9)  go to 730
  text4 = text2
  if ( itypec .ne. 1 )  text4 = text3
  write (lunit6, 941)  text4, freq
941 format ( //, 20h *******************  ,  a6, 35h constants matrices for frequency =, e15.6,    40h hz   ********************************** )
730 w = freq * twopi
  if (numaki .eq. 1)   freqs = freq
  call zymx( w,nz,isyst,ngrnd, ngg,ncpp, radi,zy,yz,dir,dij, ang,usi,usr,esi,dr0,th0,al0,hi,di,bio,bi1,bi2,bi3,bi4,bi5, &
       al1i,al2i,al3i,dci,  nx, yzn,    ys,yc,zp,zpc,zs,ze,zc,ca,cb,cc,cd,f,  ldm, ldn, ldn2, lnq2 )
800 if(npais.eq.0) go to 810
  write(lunit6,951)
  npk = lnq2 / 3
  nki = npk + 1
  nkj = nki + npk
  call datout(w, zc, yc, rsg, xmajor, nx, npais, ncros, irsep, cname, ldn, yzn(1), yzn(nki), yzn(nkj), npk)
951 format(//1h0,10x,41h*****lists of punched out data cards***** )
810 continue
  if (kill .ge. 1)   go to 9200
  if (kmode .eq. 0)   go to 750
  ktab = nx
  call prcon( w, nx, zc,zs,ys,yc,yo,qn,gn,ze,a,ai,b,bi, yzn, ca, cb, cc, f, ldn, ldn2, lnq2, mrr, nrp )
  if (kill .ge. 1)   go to 9200
  if (lastov .ne. 43)   go to 750
  if (ipunch .eq. 0)   go to 750
  if (ik .gt. 0)   go to 2745
  if (liu .ne. 0)   go to 2745
  cczero = aimagz(ys(1,1))/w/tenm6
  write(lunit9)   cczero
  liu = 1
  if (iprs47 .ge. 1) write(lunit6, 1745)  freq, cczero
1745 format (25h capacitance on unit9 at , e15.6, 3hhz., 5x, e16.8)
2745 rzero = realz(zs(1,1))
  xzero = aimagz( zs(1,1) ) / w * 1000.
  write (lunit9)  rzero, xzero, freq
  if (iprs47 .ge. 1) write (lunit6, 3745)  rzero, xzero, freq
3745 format (21h r, l and f on unit9., 5x, 3e16.8)
750 if (iii .eq. ik)   go to 9001
  go to 3006
  !!                                            ** identical eigenvalue ?
9001 if (iprint.eq.1) go to 9002
  if (nrp.gt.0) go to 9002
  rtio=(mrr*1.0)/(iprint-1.0)
  if (rtio.lt.0.75) go to 9002
  nrp=1
  freq=freqsv
  do i=1,(iprint-1)*ktab
     backspace lunit9
     write (*,*) ' guts47.  backspace lunit 9.  iprint, i = ', iprint
9004 end do
  do 9005 i=1,(iprint-1)
     backspace junit4
9005 end do
  write (*,*) ' backspace junit4.  junit4, iprint =', junit4, iprint
  iprint=1
  go to 9003
9002 continue
  !     read input card using cimage
2899 call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if ( ialter  .ne.  2 ) write (lunit2, 4230)  bufsem
  read (unit = abuff, fmt = 902) d9, freq, ik, ips, dist, j13, j14
  freqsv=freq
  if (d9 .ne. 0.)   go to 3899
  if (freq .eq. 0.)  go to 929
3899 roe = d9
  ipunch = j13
  itrnsf = j14
  if (freq .eq. 0.)   freq = statfr
  if (ips .eq. 0)   ips = 1
  write (kunit6, 3256)  roe, freq, ik, ips, dist, ipunch, itrnsf
  if (iearth .ne. 99)   go to 706
  !     read input card using cimage.
  call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if (ialter .ne. 2) write (lunit2, 4230)   bufsem
  read (unit = abuff, fmt = 901) dep1, dep2, roe3, roe4
  write (kunit6, 3280)
  !     read input card using cimage.
  call cimage
  read (unit = abuff(1), fmt = 4230) bufsem
  if (ialter .ne. 2) write (lunit2, 4230)   bufsem
  read (unit = abuff, fmt = 901) htoj2, htoj3, htoj4, hyud2, hyud3, hyud4
  write (kunit6, 3290)
  alf1 = roe/roe3
  alf2 = roe/roe4
  go to 706
929 write (kunit6, 930)
930 format ( 40h+blank card terminating frequency cards.  )
  call interp
  write (lunit6, 4239)
4239 format ( ///,  1x )
  go to 5
9200 lstat(18) = nchain
  if (ialter .eq. 2) lunit5 = l5save
  lastov=nchain
  nchain=51
9900 if ( iprsup  .ge.  1 ) write (lunit6, 2592)  nchain, lunit5, numaki, ialter
2592 format ( /,  17h exit  'subr47' ., 32h  nchain  lunit5  numaki  ialter  ,/,  17x,  4i8  )
  return
end subroutine guts47
!
! subroutine crosa4.
!
subroutine crosa4(czy,icont,ldn,ca,cb,cc,cd,ce,cf,cg,f,ldn2)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  complex*16  czy(ldn,ldn)
  complex*16  ca(ldn,ldn), cb(ldn,ldn), cc(ldn,ldn), cd(ldn,ldn)
  complex*16  ce(ldn,ldn), cf(ldn,ldn), cg(ldn,ldn), f(ldn, ldn2)
  complex*16  cwork1
  write ( 6, 1001 )
1001 format ( 33h  begin module "crosa4".  ******  )
  cwork1=0.
  do i=4,6
     cwork1 = cwork1 + czy(i,i)
10 end do
  cwork1 = cwork1/3.
  do i=4,6
     czy(i,i) = cwork1
20 end do
  cwork1 = czy(4,5) + czy(4,6) + czy(5,6)
  cwork1 = cwork1/3.
  czy(4,5) = cwork1
  czy(4,6) = cwork1
  czy(5,6) = cwork1
  do i = 1,3
     cwork1 = 0.
     do j = 4,6
        cwork1 = cwork1 + czy(i,j)
40   end do
     cwork1 = cwork1/3.
     do j = 4,6
        czy(i,j) = cwork1
50   end do
30 end do
  do i = 1,6
     do j = 1,6
        if(i.le.3.and.j.le.3) go to 60
        if(i.ge.j) go to 60
        czy(j,i) = czy(i,j)
60   end do
  end do
  if(icont.eq.0) call minvn( czy,6,3,0,ldn,ca,cb,cc,cd,ce,cf,cg,f,ldn2 )
  do j = 1,3
     czy(4,j) = czy(4,j) + czy(5,j) + czy(6,j)
     czy(j,4) = czy(j,4) + czy(j,5) + czy(j,6)
70 end do
  czy(4,4) = czy(4,4) + czy(5,4) + czy(6,4) + czy(4,5) + czy(5,5) + czy(6,5) + czy(4,6) + czy(5,6) + czy(6,6)
  if(icont.eq.0) call minvn( czy,4,3,0,ldn, ca,cb,cc,cd,ce,cf,cg,f,ldn2 )
  return
end subroutine crosa4
!
! subroutine minvn.
!
subroutine minvn( cinout, n, l, ix, ldn, ca, cb, cc, cd, cwork1, cwork2, cwork3, f, ldn2 )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  complex*16  ca(ldn,ldn),cb(ldn,ldn),cc(ldn,ldn),cd(ldn,ldn)
  complex*16  cwork1(ldn,ldn),cwork2(ldn,ldn),cwork3(ldn,ldn)
  complex*16  cinout(ldn,ldn), f(ldn, ldn2)
  nml=n-l
  call cutmat(cinout,ca,cb,cc,cd,n,l,ldn)
  do i=1,nml
     do j=1,nml
        cwork1(i,j)=cd(i,j)
10   end do
  end do
  call minv(cwork1,nml,f,ldn,ldn2)
  call mxmnm(cwork1,cc,cwork2,nml,nml,l,ldn)
  call mxmnm(cb,cwork2,cwork1,l,nml,l,ldn)
  do i=1,l
     do j=1,l
        cwork1(i,j)=ca(i,j)-cwork1(i,j)
        cinout(i,j)=cwork1(i,j)
20   end do
  end do
  if(ix.eq.1) return
  call minv(cwork1,l,f,ldn,ldn2)
  do i=1,l
     do j=1,l
        cinout(i,j)=cwork1(i,j)
30   end do
  end do
  call mxmnm(cwork2,cwork1,cwork3,nml,l,l,ldn)
  do i=1,nml
     do j=1,l
        ipl=i+l
        cinout(ipl,j)=-cwork3(i,j)
40   end do
  end do
  do i=1,l
     do j=1,l
        cwork1(i,j)=ca(i,j)
50   end do
  end do
  call minv(cwork1,l,f,ldn,ldn2)
  call mxmnm(cwork1,cb,cwork2,l,l,nml,ldn)
  call mxmnm(cc,cwork2,cwork1,nml,l,nml,ldn)
  do i=1,nml
     do j=1,nml
        cwork1(i,j)=cd(i,j)-cwork1(i,j)
60   end do
  end do
  call minv(cwork1,nml,f,ldn,ldn2)
  do i=1,nml
     do j=1,nml
        ipl=i+l
        jpl=j+l
        cinout(ipl,jpl)=cwork1(i,j)
70   end do
  end do
  call mxmnm(cwork2,cwork1,cwork3,l,nml,nml,ldn)
  do i=1,l
     do j=1,nml
        jpl=j+l
        cinout(i,jpl)=-cwork3(i,j)
80   end do
  end do
  return
end subroutine minvn
!
! subroutine cutmat.
!
subroutine cutmat(cinput,ca,cb,cc,cd,n,l,ldn)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  complex*16  cinput( ldn, ldn )
  complex*16  ca(ldn,ldn),cb(ldn,ldn),cc(ldn,ldn),cd(ldn,ldn)
  do i=1,l
     do j=1,l
        ca(i,j)=cinput(i,j)
10   end do
  end do
  nml=n-l
  do i=1,l
     do j=1,nml
        jpl=j+l
        cb(i,j)=cinput(i,jpl)
20   end do
  end do
  do i=1,nml
     do j=1,l
        ipl=i+l
        cc(i,j)=cinput(ipl,j)
30   end do
  end do
  do i=1,nml
     do j=1,nml
        ipl=i+l
        jpl=j+l
        cd(i,j)=cinput(ipl,jpl)
40   end do
  end do
  return
end subroutine cutmat
!
!     subroutine mxmnm.
!
subroutine mxmnm(ca, cb, cc, l, m, n, ldn)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  complex*16 ca(ldn, ldn), cb(ldn, ldn), cc(ldn, ldn)
  do i = 1, l
     do j = 1, n
        cc(i, j) = 0.
        do k = 1, m
           cc(i,j ) = cc(i, j) + ca(i, k) * cb(k, j)
30      end do
20   end do
10 end do
  return
end subroutine mxmnm
!
!     subroutine datout.
!
subroutine datout(w, zc, yc, rs, xmajor, nub6, npais, nncros, irsep, cha, ldn, r, al, c, npk)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     8.19 data cards punch out subroutine
  character cha
  dimension r(npk), al(npk), c(npk)
  complex*16 zc(ldn, ldn), yc(ldn, ldn)
  lunit6 = 6
  lunit7 = 7
  !     ltype=0 ; output rs
  !     ltype=1 ; not output rs
  !     ntype=0 ; crossbonded cable
  !     ntype=1 ; non-crossbonded cable
  !     mtype=0 ; all sheathes are conected
  !     mtype=1 ; all sheathes are not conected
  ncros=iabs(npais)
  ltype=0
  if(npais.gt.0.and.nncros.eq.0) ltype=1
  ntype=1
  if(npais.lt.0.and.nncros.ne.0) ntype=0
  mtype=irsep
  if(npais.gt.0.and.nncros.eq.0) mtype=1
  nub3=3
  if(ntype.eq.1) nub3=1
  l=0
  xleng=xmajor
  if(ntype.eq.0) xleng=xleng/3.
  do i=1,nub6
     do j=1,nub6
        if(j.gt.i) go to 400
        l=l+1
        r(l)=realz(zc(i,j))*xleng
        al(l)=aimagz(zc(i,j))/w*xleng*1.e3
        c(l)=aimagz(yc(i,j))/w*xleng*1.e6
400  end do
  end do
  write(lunit6,1400)
  write(lunit7,1401)
1400 format(1h ,11h$vintage, 1)
1401 format(11h$vintage, 1)
  l=0
  if(ltype.eq.1) go to 610
  kstep=1
  kk=nub6/2+1
  if(nub6.eq.4) kk=4
  kstop=(nub6+1)/2+1
  if(mtype.ne.0) kstop=nub6
  if(nub6.eq.4) kstop=4
  if(nub6.eq.7.and.mtype.eq.0) kstop=4
  do k=kk,kstop,kstep
     write(lunit6,1300) cha,k,rs
     write(lunit7,1301) cha,k,rs
900 end do
1300 format(1h ,2x,a1,3hin ,i2,18x,e16.5)
1301 format(2x,a1,3hin ,i2,18x,e16.5)
610 continue
  !     i ; number of major section
  !     j ; number of minor section
  !     k ; number of phase
  do i=1,ncros
     do j=1,nub3
        do k=1,nub6
           !     following two statements are for the third minor section
           k2=k
           if(mtype.eq.1) go to 910
           if(j.eq.3.or.ntype.eq.1) call cha444(k,k2)
910        continue
           !     following two statements are for crossbonding
           k1=k
           if(j.ne.1) call cha645(k,k1)
           !     following one statement is for the first minor section
           if(j.eq.1.and.mtype.ne.1) call cha444(k,k1)
           !     following two statements are for seriese conection of
           !     major section
           j1=j
           if(ntype.ne.1) call cha312(j,j1)
           !     following two statements are for sending end
           i1=i
           if(j.eq.1) i1=i-1
           ipri=1
           if(i1.eq.0) ipri=2
           if(i.ne.ncros) go to 700
           if(ntype.eq.1.or.j.eq.3) ipri=3
           !     ipri=1 ;
           !     ipri=2 ; sending end
           !     ipri=3 ; recieving end
700        continue
           if(ncros .eq. 1 .and. ntype .eq. 1) ipri = 4
           call pri(i, j, k, i1, j1, k1, k2, l, ipri, cha, r, al, c,npk)
300     end do
200  end do
     if(ltype.eq.1) go to 100
     j=3
     if(ntype.eq.1) j=1
     if(i.eq.ncros) go to 2010
     do k=kk,kstop,kstep
        write(lunit6,2000) cha,i,j,k,cha,k
        write(lunit7,2001) cha,i,j,k,cha,k
920  end do
2000 format(1h ,2x,a1,i2,i1,i2,6x,a1,3hin ,i2)
2001 format(2x,a1,i2,i1,i2,6x,a1,3hin ,i2)
     go to 100
2010 continue
     do 930 k=kk,kstop,kstep
        write(lunit6,2100) cha,k,cha,k
        write(lunit7,2101) cha,k,cha,k
930  end do
2100 format(1h ,2x,a1,3hout,i2,6x,a1,3hin ,i2)
2101 format(2x,a1,3hout,i2,6x,a1,3hin ,i2)
100 end do
  write(lunit6,1410)
  write(lunit7,1411)
1410 format(1h ,11h$vintage, 0)
1411 format(11h$vintage, 0)
  return
end subroutine datout
!
!     subroutine cha645.
!
subroutine cha645(k,k1)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  if(k.le.3.or.k.ge.7) return
  kk=k-3
  go to (10,20,30),kk
10 k1=6
  return
20 k1=4
  return
30 k1=5
  return
end subroutine cha645
!
! subroutine cha312.
!
subroutine cha312(j,j1)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  go to (10,20,30),j
10 j1=3
  return
20 j1=1
  return
30 j1=2
  return
end subroutine cha312
!
! subroutine cha444.
!
subroutine cha444(k,kk)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  if(k.le.3.or.k.gt.7) return
  kk=4
  return
end subroutine cha444
!
!     subroutine pri.
!!
subroutine pri(i, j, k, i1, j1, k1, k2, l, ipri, cha, r, al, c, npk)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  character cha
  dimension r(npk),al(npk),c(npk)
  lunit6 = 6
  lunit7 = 7
  go to (10, 20, 30, 40),ipri
10 continue
  if (k .ne. 1) go to 15
  write(lunit6,1000) k,cha,i1,j1,k1,cha,i,j,k2,cha,cha
  write(lunit7,1001) k,cha,i1,j1,k1,cha,i,j,k2,cha,cha
1000 format(1h ,i2,a1,i2,i1,i2,a1,i2,i1,i2,a1,5hin  1,a1,5h 11 1)
1001 format(i2,a1,i2,i1,i2,a1,i2,i1,i2,a1,5hin  1,a1,5h 11 1)
  return
15 continue
  write(lunit6,1010) k,cha,i1,j1,k1,cha,i,j,k2
  write(lunit7,1011) k,cha,i1,j1,k1,cha,i,j,k2
1010 format(1h ,i2,a1,i2,i1,i2,a1,i2,i1,i2)
1011 format(i2,a1,i2,i1,i2,a1,i2,i1,i2)
  return
20 continue
  l=l+1
  write(lunit6,1100) k,cha,k1,cha,i,j,k2,r(l),al(l),c(l)
  write(lunit7,1101) k,cha,k1,cha,i,j,k2,r(l),al(l),c(l)
1100 format(1h ,i2,a1,3hin ,i2,a1,i2,i1,i2,12x,3e16.5)
1101 format(i2,a1,3hin ,i2,a1,i2,i1,i2,12x,3e16.5)
2000 continue
  if(k.eq.1) return
  do ll=2,k
     l=l+1
     write(lunit6,1110) r(l),al(l),c(l)
     write(lunit7,1111) r(l),al(l),c(l)
1110 format(1h ,26x,3e16.5)
1111 format(26x,3e16.5)
100 end do
  return
30 continue
  if(k.ne.1) go to 35
  write(lunit6,1200) k,cha,i1,j1,k1,cha,k2,cha,cha
  write(lunit7,1201) k,cha,i1,j1,k1,cha,k2,cha,cha
1200 format(1h ,i2,a1,i2,i1,i2,a1,3hout,i2,a1,5hin  1,a1,5h 11 1)
1201 format(i2,a1,i2,i1,i2,a1,3hout,i2,a1,5hin  1,a1,5h 11 1)
  return
35 continue
  write(lunit6,1210) k,cha,i1,j1,k1,cha,k2
  write(lunit7,1211) k,cha,i1,j1,k1,cha,k2
1210 format(1h ,i2,a1,i2,i1,i2,a1,3hout,i2)
1211 format(i2,a1,i2,i1,i2,a1,3hout,i2)
  return
40 continue
  l = l + 1
  write(lunit6,1300) k,cha,k1,cha,k2,r(l),al(l),c(l)
  write(lunit7,1301) k,cha,k1,cha,k2,r(l),al(l),c(l)
1300 format(1h ,i2,a1,3hin ,i2,a1,3hout,i2,12x,3e16.5)
1301 format(i2,a1,3hin ,i2,a1,3hout,i2,12x,3e16.5)
  go to 2000
end subroutine pri
!
!     subroutine nyan.
!
subroutine nyan(itype,npc,nc,ncpp,ngrnd,ncros,npais,ldm)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension ncpp(ldm)
  lunit6=6
  if(itype.eq.1) return
  if(ncros.eq.0) return
  if(npc.ne.3) go to 9000
  do i=1,3
     if(ncpp(i).eq.1.or.ncpp(i).gt.3) go to 9100
100 end do
  nwork=6+ngrnd
  if(nwork.ne.nc) go to 9200
  if(ncpp(1).le.ncpp(2).and.ncpp(2).le.ncpp(3)) return
  go to 9300
9000 continue
  write(lunit6,7000)
  write(lunit6,8000)
8000 format(1h0,10x,31hnumber of phases should be '3'.)
  call stoptp
9100 continue
  write(lunit6,7000)
  write(lunit6,8100)
8100 format('0', 10x, 'number of conductors in one phase should', "be '2' (core and sheath) with 'ngrnd'='0", &
       "' for an sc cable and 'ngrnd'='1' for a /", ' ', 10x, "pt cable, of '3' (core, sheath and armor", &
       ") with 'ngrnd'='3, considering the fact ", 'that all the 3-phase cables have the    /' &
       ' ', 10x, 'same configuration.                     ')
  call stoptp
9200 continue
  write(lunit6,7000)
  write(lunit6,8200)
8200 format('0', 10x, "In the case of 'npais.ge.0 .and. ncros.ne.0', the final number of conductors  ",/,11x, &
       "considering grounded conductors reduction should be '6'.", /, 11x, &
       "'ngrnd' should be '3' for an sc cable with armor and '1' for a pt cable.  If a 3-phase sc cable with armor is enclosed  ",/, &
       ' ', 10x, 'within a pipe (i.e., pt cable), please,  neglect the pipe, i.e., regard as an sc ', &
       "cable with 'ngrand'='3', considering the", /, ' ', 10x, 'fact that all the 3-phase cables have   the same configuration.                 ')
  call stoptp
9300 continue
  write(lunit6,7000)
  write(lunit6,8300)
8300 format('0',10x, 'Eeach cable of a 3-phase cable system has the same configuration in general. If not, please arrange the data cards as    ', /, &
       ' ', 10x, 'follows : first comes a cable of which the number of conductors is smallest, second comes a cable of the second smallest', /, &
       ' ', 10x, 'number of conductors,.... Please check your data.')
  call stoptp
  return
7000 format('0', 10x, "Errors for a crossbonded cable (ncros.ne.0) when 'npais'='0'.")
end subroutine nyan
!
! subroutine gomen.
!
subroutine gomen(itype,npc,nx,npais,ncros,irsep,ncpp,ldm)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  dimension ncpp(ldm)
  lunit6=6
  if(npais.lt.0) go to 1000
  if(ncros.ne.0) go to 1200
  if(irsep.eq.0) return
  go to 9120
1000 if(ncros.eq.0) go to 1100
  if(nx.lt.6.or.nx.gt.7) go to 9010
1200 if(npc.ne.3) go to 9020
  do i=1,3
     if(ncpp(i).eq.1.or.ncpp(i).gt.3) go to 9030
100 end do
  if(ncpp(1).gt.ncpp(2).or.ncpp(2).gt.ncpp(3)) go to 9040
  if(nx.eq.6) return
  if(npais.le.0 .or. ncros.eq.0) go to 200
  if(nx.eq.4) return
200 if(ncpp(2).eq.2) return
  go to 9110
1100 if(nx.gt.7) go to 9050
  if(nx.ge.6) go to 1200
  if(nx.eq.2) go to 1300
  if(nx.eq.3) go to 1400
  go to 9060
1400 if(irsep.eq.0) go to 9070
  if(npc.ne.1) go to 9080
  return
1300 if(npc.ne.1) go to 9090
  return
9010 continue
  write(lunit6,8010)
8010 format('0', 10x, "In the case of 'npais.lt.0.and.ncros.ne.0' the final number of conductors including grounded conductors reduction should", / &
       ' ', 10x, "be 6 or 7, i.e., 'total number of conductors of the cable system'-'ngrnd'='6' or, '7', but 'ngrnd' should be '1' for a pt", / &
       ' ' ,10x, 'cable.                                  ')
  call stoptp
9020 continue
  write(lunit6,8020)
8020 format(1h0,10x,40hnumber of phases should be '3'.         )
  call stoptp
9030 continue
  write(lunit6,8030)
8030 format(1h0,10x,40hnumber of conductors in one phase should, 40 h be '2' (core and sheath) with 'ngrnd'=', 40 h0' for an sc cable and 'ngrnd'='1' for a/ &
       1 h ,10x,40hpt cable, or '3' (core, sheath and armor, 40 h) with 'ngrnd'='3' for an sc cable.     )
  call stoptp
9040 continue
  write(lunit6,8040)
8040 format('0', 10x, 'Eeach cable of a 3-phase cable system has the same configuration in general. If not, please arrange the data cards as    ', / &
       ' ', 10x, 'follows : first comes a cable of which the number of conductors is smallest, second comes a cable of the second smallest ', / &
       ' ', 10x, 'number of conductors,.... Please check your data                                ')
  call stoptp
9050 continue
  write(lunit6,8050)
8050 format('0', 10x, "In the case of 'npais.lt.0', the final number of conductors including grounded conductors reduction should not be       ", / &
       ' ', 10x, "greater than '7'. Please check your data.")
  call stoptp
9060 continue
  write(lunit6,8060)
8060 format('0', 10x, "In the case of 'npais.lt.0', the final number of conductors including grounded conductors reduction should not be '1' or", / &
       ' ', 10x, "'4' or '5'. Please check your data.            ")
  call stoptp
9070 continue
  write(lunit6,8070)
8070 format('0', 10x, "In this case, it should not be 'irsep=0', please change 'irsep=1'                 ")
  call stoptp
9080 continue
  write(lunit6,8080)
8080 format('0', 10x, "In this case, number of phases (npc) shohuld only be '1'. Please check your data.")
  call stoptp
9090 continue
  write(lunit6,8090)
8090 format('0', 10x, "Sorry, in the case of 'npais.lt.0', a two phase cable (npc=2) consisting only of core or consisting of core and sheath  ", / &
       ' ', 10x, 'can not be dealt with. Please check your data.                                   ')
  call stoptp
9110 continue
  write(lunit6,8110)
8110 format(' ', 10x, "In this case, ncpp(2) should be '2'. Please check your data.                       ")
  call stoptp
9120 continue
  write(lunit6,8120)
8120 format('0', 10x, "In the case of 'npais.gt.0.and.ncro,34 hs.eq.0', it should not be 'irsep=1,', please change 'irsep=0'.        ")
  call stoptp
  return
end subroutine gomen
!
! subroutine prcon.
!
subroutine  prcon(w,nconpw, zc,zs,ys,yc,yo,qn,gn,ze,a,ai,b,bi,an,  ca, zo, cc, f, ldn, ldn2, lnq2, mrr, nrp )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labl47.ftn'
  dimension  gn(ldn), an(lnq2)
  dimension  tir(20,20), tii(20,20)
  dimension  pp1(20,20), pp2(20,20)
  dimension  zz(30), ps(30), ping(200), iseq(15),kmax(15)
  complex*16  pp6(20,20),ee6(15)
  complex*16  anglec, cjw
  complex*16  csqrtz
  complex*16  cmplxz, d1, d2, d3, d4
  complex*16  ca(ldn, ldn), zo(ldn, ldn), cc(ldn, ldn)
  complex*16  zc(ldn, ldn), zs(ldn, ldn), ys(ldn, ldn), yc(ldn, ldn)
  complex*16  a(ldn, ldn), ai(ldn, ldn),  b(ldn, ldn), bi(ldn, ldn)
  complex*16  yo(ldn, ldn), ze(ldn, ldn), qn(ldn), f(ldn, ldn2)
  data  iseq  /  15*0  /
  ntol=iprint-1
  cjw = cmplxz ( fzero, w )
  if ( iprs47  .ge.  1 ) write (logsix, 2624)  nconpw, numaki, value1, value3, pai, cjw
2624 format ( /,  17h enter  'prcon' .,  16h  nconpw  numaki, 13x,  6hvalue1,  13x,  6hvalue3,  16x,  3hpai, 11x,  8hreal-cjw,  11x,  8himag-cjw  ,/,  17x,  2i8,  5e19.10 )
  ll1 = 1
  ll2 = 2
  call mxm(zc,yc,zs,nconpw,ldn)
  call eigen( cjw,zs,nconpw,  a,ai,qn,ca,zo,cc,ldn  )
!!!!  *****
  if (kill .gt. 1)   go to 9200
  if (itypec .ne. 2)  go to 75
  n1 = npc * 2
  if (nconpw .ne. n1 ) go to 75
  icorr = 0
  npc1 = npc + 1
  do i = npc1, npc2
     if (realz(qn(i)) .gt. 0.)  go to 65
     i1 = i + 1
     if (i1 .gt. npc2)  i1 = i - 1
     if (i1 .le. npc)  go to 65
     if (realz(qn(i1)) .lt. 0.)  go to 63
61   qn(i) = qn(i1)
     icorr = 1
     write(lunit6, 901)  i
901  format (1h0, 42hmodifications of modal quantities for mode, i2, 10h are made. /)
     go to 65
63   i1 = i + 2
     if (i1 .gt. npc2)  i1 = i - 2
     if (realz(qn(i1)) .gt. 0.)  go to 61
65 end do
  if (icorr .eq. 0)  go to 75
  do i=npc1, npc2
     i1 = i - npc
     do j=1, npc2
        ai(i,j) = czero
        if (i1.eq.j)  ai(i,j) = creal1
        if(i .eq. j)  ai(i,j) = -creal1
700  end do
  end do
  do i=1,nconpw
     do j=1, nconpw
71      a(i,j) = ai(i,j)
     end do
  end do
  write(*,*) ' after #71 '
  call minv( a, nconpw, f, ldn, ldn2 )
75 continue
  do iz=1,30
     ps(iz)=0.
     zz(iz)=0.
388 end do
  do i=1,nconpw
     an(i) = value3 * realz(qn(i))
     gn(i) = w / aimagz( qn(i) )
     if (an(i) .lt. 0.)  go to 50
     a1 = 1.05
     spdtol = spdlgt * a1
     if (gn(i) .gt. spdtol)  go to 50
     if (gn(i) .lt. spdlgt)  go to 55
     gn(i) = spdlgt
     write(lunit6, 961)
961  format (1h0, 10x, 33hmodification for imag(q) is made.  //, 1x)
     go to 55
50   write(lunit6, 962)
962  format ('0', 10x, 'Conductor impedance may include numerical errors.  ', /, &
          11x, 'The correct solutions for modes showing errors can be given by the very last mode.  ', //, 1x)
55   continue
     !       if (lastov .ne. 39 )  go to 600
     !       if (iprint .gt. 1 )  go to 600
     do j= 1, nconpw
        bi(i,j)=a(j,i)
        b(i,j)=ai(j,i)
        if ( lastov .ne. 39 )  go to 60
        if ( iprint .gt. 1 )  go to 60
        tir(i,j) = realz(b(i,j))
        tii(i,j) = aimagz(b(i,j))
60   end do
600  continue
1960 end do
  if (ntol.eq.0 .or. lastov .ne. 39) go to 5602
  kthl=1
  ping(kthl)=alog1z(w/twopi)
  d55 = 1.0 /(w/twopi)
  d56 = 1.0 / sqrtz( w/twopi )
  do i=1,nconpw
     !     em=(realz(qn(i)))**2 + (aimagz(qn(i)))**2
     !     ea=2.0*atan2z( aimag(qn(i)),real(qn(i)) )
     em=sqrtz((realz(qn(i)**2))**2 + (aimagz(qn(i)**2))**2)
     ea=atan2z( aimagz(qn(i)**2),realz(qn(i)**2) )
     db=value3*realz(qn(i))
     vmode=w/aimagz(qn(i))
5601 continue
     ping(kthl+1) = em*d55
     ping(kthl+2) = ea/(twopi/360.)
     ping(kthl+3) = vmode
     ping(kthl+4) = db*d56
     kthl = kthl + 4
5603 continue
5600 end do
  call unwind( ping,kthl,mrr,nrp,ntol,iseq )
  do i=1,nconpw
     ee6(i)=qn(i)**2
     do j=1,nconpw
        pp6(i,j)=a(i,j)
5613 end do
  end do
  do i=1,nconpw
     qn(i) = csqrtz( ee6(iseq(i)) )
     do j=1,nconpw
        a(i,j)=pp6(i,iseq(j))
5612 end do
  end do
  do i=1,nconpw
     do j=1,nconpw
        ai(i,j)=a(i,j)
5701 end do
  end do
  if ( iprsup .ge. 1 ) write(*,*) ' prcon after s.n. 5701.   [a]  follows ....'
  do i=1,nconpw
     if ( iprsup .ge. 1 ) write(*,*) (a(i,j), j=1,nconpw)
7801 end do
  call minv( ai, nconpw, f, ldn, ldn2 )
  if ( iprsup .ge. 1 ) write(*,*) ' after "minv".   [ai]  follows ..... '
  do i=1,nconpw
     if ( iprsup .ge. 1 ) write(*,*) (ai(i,j), j=1,nconpw)
7802 end do
  do i=1,nconpw
     do j=1,nconpw
        pp1(j,i)=sqrtz( (realz(ai(i,j)))**2+(aimagz(ai(i,j)))**2 )
        pp2(j,i)=atan2z( aimagz(ai(i,j)) , realz(ai(i,j)) )
5621 end do
5611 end do
  if ( ntol.eq.1 ) go to 5800
  go to 5810
5800 continue
  do j=1,nconpw
     kmax(j)=1
     do i=1,nconpw
        if ( pp1(i,j) .le. pp1(kmax(j),j) ) go to 5802
        kmax(j)=i
5802 end do
5801 end do
  write (lunit9)  (kmax(j),j=1,nconpw)
5810 continue
  if ( iprsup .ge. 1 ) write (*, *) ' prcon.  use normalization cells kmax(1:nconpw) =', ( kmax(i), i=1,nconpw )
  do j=1,nconpw
     if ( pp1(kmax(j),j) .eq. 0.0 ) go to 5614
     dv=1.0/pp1(kmax(j),j)
     da=pp2(kmax(j),j)
     do i=1,nconpw
        pp1(i,j)=pp1(i,j)*dv
        pp2(i,j)=pp2(i,j)-da
        !     *** to keep angles within principal value region
        if ( pp2(i,j) .gt. twopi/2. )  pp2(i,j)=pp2(i,j) - twopi
        if ( pp2(i,j) .lt.-twopi/2. )  pp2(i,j)=pp2(i,j) + twopi
        if (iprsup .ge. 3) write(*,*) 'just before 5624, i, j, pp1(i,j), pp2(i,j)', i, j, pp1(i,j), pp2(i,j)
5624 end do
5614 end do
  fout = w / twopi
  !   ******************** temporary diagnostic ************
  if ( iprsup .ge. 0 ) write (*, *) ' prcon.  freq =',  fout, '     pp1 follow : '
  do i=1,nconpw
     if ( iprsup .ge. 0 ) write (*, *) ( pp1(i,j), j=1, nconpw )
7701 end do
  if ( iprsup .ge. 0 ) write (*, *) ' pp2  follow ... '
  do 7702 i=1,nconpw
     if ( iprsup .ge. 0 ) write (*, *) ( pp2(i,j), j=1,nconpw )
7702 end do
  do i=1,nconpw
     do j=1,nconpw
        u1= (pp1(i,j))*cosz(pp2(i,j))
        u2= (pp1(i,j))*sinz(pp2(i,j))
        ai(j,i)=cmplxz(u1,u2)
6004 end do
  end do
  do j=1,nconpw
     do i=1,nconpw
        a(i,j)=ai(i,j)
6002 end do
  end do
  call minv ( a, nconpw, f, ldn, ldn2 )
  do i=1, nconpw
     do j=1, nconpw
        bi(i,j)=a(j,i)
        b(i,j)=ai(j,i)
6003 end do
  end do
5602 continue
  do i=1, nconpw
     do j=1, nconpw
        aii = aimagz ( ai(i,j) )
        air = realz  ( ai(i,j) )
        d18 = cabsz ( ai(i,j) )
        deg = atan2z ( aii, air ) * 180.0 / pai
        ze(i,j) = cmplxz ( d18, deg )
        acomi = aimagz ( a(i,j) )
        acomr = realz ( a(i,j) )
        d19 = cabsz ( a(i,j) )
        deg = atan2z ( acomi, acomr ) * 180.0 / pai
        ys(i,j) = cmplxz ( d19, deg )
1470 end do
  end do
  if ( numaki .gt. 3 )  go to 5673
  write (lunit6, 2953)
2953 format (  /, 10x, 'voltage transformation matrices in polar', ' coordinates (with angles in degrees).',/, 15x, ' inverse of [tv] --- from phase to mode domain:'  )
  call print ( ze, nconpw, ll1, ldn )
  write (lunit6, 4954)
4954 format ( 15x,  ' [tv] --- from mode to phase domain:' )
  call print ( ys, nconpw, ll1, ldn )
5673 call mxm ( bi, yc, ca, nconpw, ldn )
  call mxm ( ca, a, ys, nconpw, ldn )
  do i=1,nconpw
     do j=1,nconpw
        if(i.ne.j) go to 79
        aa=aimagz(ys(i,i))
        bb=realz(ys(i,i))
        if ( iprsup .ge. 1 ) write(*,*) ' i, ys(i,i) =',  i, ys(i,i)
        !!  $$$$ protection ?? $$$$
        !     if((absz(bb).le.1.0e-18).or.(absz(aa).le.1.0e-18))
        if((absz(bb).le.1.0e-18) .and. (absz(aa).le.1.0e-18)) go to 9801
        go to 9802
9801    continue
        ys(i,j)=ys(i,j)*1.0e10
        zs(i,i) = qn(i)**2 / ys(i,i) * 1.e-10
        !     zo(i,i) = (qn(i)/ys(i,i))*1.0e10
        write (*,*) 'qn, ys  for i=',qn(i),ys(i,j),i
        ys(i,i) = ys(i,i)*1.0e-10
        go to 9803
9802    continue
        !     zo(i,i) = qn(i)/ys(i,i)
        zs(i,i) = qn(i)**2 / ys(i,i)
9803    continue
        cc  $$$$ end of protection  $$$$
        !     yo(i,i) = 1./zo(i,i)
        !     zs(i,i) = qn(i) * zo(i,i)
        yo(i,i) = csqrtz( ys(i,i)/zs(i,i) )
        zo(i,i) = 1. / yo(i,i)
        go to 80
79      zo(i,j)=czero
        yo(i,j) = czero
        zs(i,j) = cmplxz(fzero, fzero)
80   end do
     if (lastov .ne. 39)  go to 1980
     if ( iprint .lt. 2 ) go to 1978
     !     ysre = realz(ys(i,i))
     !     ysim = aimagz(ys(i,i))
     !     zsre = realz(zs(i,i))
     !     zsim = aimagz(zs(i,i))
     !!      write (lunit9)  w, ysre,ysim,zsre,zsim
     w1=w
     !   ** the unit here is /km now! **
     !     zz(i)=zsre *1000.
     !     zz(i+nconpw)=zsim*1000.
     !     ps(i)=ysre*1000.
     !     ps(i+nconpw)=ysim*1000.
     ycharm = cabsz(yo(i,i))
     ychara = atan2z( aimagz(yo(i,i)), realz(yo(i,i)) )
     alpha = realz(qn(i)) * 1000.
     beta = aimagz(qn(i)) *1000.
     if (lastov .ne. 39)  go to 1980
     if ( iprint .lt. 2 ) go to 1978
     write(lunit9) w1,ycharm, ychara, alpha, beta
     do 78 k = 1, nconpw
        pp1(k,i) = cabsz(b(k,i))
        pp2(k,i) = atan2z( aimagz(b(k,i)), realz(b(k,i)) )
78   end do
     write(lunit9) ( pp1(k,i),pp2(k,i), k = 1, nconpw )
     !    **********  thlthl
1978 if ( iprsup .ge. 0 )
     !    **********  thlthl
1    write (lunit6, 1979)  i, w, ycharm, ychara, alpha, beta
1979 format ( ' ychar and eigenvalue for mode', i3, ' at frequency =' , e12.5,  2x, 5h are , 4e12.5)
1980 end do
  if (numaki .gt. 3)   go to 9900
  write (lunit6, 956)
956 format ( /, 10x, 'characteristic impedances in the phase domain' )
  call mxm(a,zo,ca,nconpw,ldn)
  call mxm(ca,bi,ze,nconpw,ldn)
  call print(ze,nconpw ,ll1,ldn)
85 write (lunit6, 305)
305 format(//, 53x, 27h table of modal quantities.)
  write (lunit6, 315)
315 format(//, 11x, 'modal', 4x, 'propagation', 7x, 'modal impedance', 12x, 'modal', 6x, 'charac. imp., natural mode', 4x, &
       'charac. adm., natural mode', /, ' mode', 3x, 'attenuation   velocity', 7x, 'real', 7x, 'imaginary', 4x, 'susceptance', &
       7x, 'real', 8x, 'imaginary', 9x, 'real', 8x, 'imaginary', /, 10x, '(db/km)', 6x, '(m/s)', 12x, 'z(Ohm/m)', 14x, 'imy(mho/m)', &
       9x, 'sqrt(z/y) (Ohm)  ', 13x, 'sqrt(y/z) (mho)  ')
  do i=1, nconpw
     zsr    = realz(zs(i,i))
     zsi   = aimagz(zs(i,i))
     ysi    = aimagz(ys(i,i))
     zor    = realz(zo(i,i))
     zoi    = aimagz(zo(i,i))
     yor     = realz(yo(i,i))
     yoi     = aimagz(yo(i,i))
     write (lunit6, 325)  i, an(i), gn(i), zsr, zsi, ysi, zor, zoi, yor, yoi
325  format(/, 1x, i3, 3x, 2e12.5, 1x, 3e14.6, 2(1x, 2e14.6) )
205 end do
  write (lunit6, 335)
335 format ( ///,  1x )
  go to 9900
9200 continue
9900 if ( iprs47  .ge.  1 ) write (logsix, 2689)
2689 format ( /,  16h exit  'prcon' .  )
  return
end subroutine prcon
!
! subroutine unwind.
!
subroutine unwind (ping,kthl,mrr,nrp,ntol,iseq)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
!!!!! %include  '//a/tsu/tpplotkom.ins.ftn'
  dimension ping(200),tping(200)
  dimension ps(30),zz(30)
  dimension bom(180),vfreq(3), kuid(15), kuse(15)
  dimension iseq(15), iseqa(15), iseqt(15), iold(15)
  dimension sb(60), tt(4), tzz(30), tps(30)
  data  bom  /  180*0.0d0  /
  data  kunf  / 0 /
  !      write(*,*) ' change order ? 1 (for yes) or 2 (for no)'
  !      read(*,*) ny
  if ( iprsup .ge. 1 ) write(*,*) ' beginning of unwind.   kthl, mrr, nrp, ntol =', kthl, mrr, nrp, ntol
  ny=2
  !      if(ny .eq. 2) go to 3800
  !      do 3802 i=1,15
  !      iseqa(i)=i
  !      iseqt(i)=i
  ! 3802 continue
  !      write(*,*) ' freq ?'
  !      read(*,*) chf
  !      write(*,*) ' new order ?'
  !      read(*,3801) (iseqa(k), k=1,15)
  ! 3801 format( 15i3 )
  ! 3800 continue
  if ((ntol.gt.1).or.(nrp.eq.0)) go to 1050
  do i=1,15
     iseq(i)=i
     iold(i)=i
1051 end do
  do i=1,180
     bom(i)=0.0
1052 end do
1050 continue
  if (iseq(1).ne.0) go to 10
  do i=1,15
     iseq(i)=i
1001 end do
  !!      write(*,*)
  !!     1 ' identical eigenvalues ? 1 (for  yes) or 2 (for no)'
  !!      read(*,*) lwh
  !!      if (lwh .eq. 2) go to 10
  !!      kunf=1
  !!      write(*,*) ' mode ?'
  !!      read(*,4801) (kuid(k), k=1,15)
  !! 4801 format( 15i3 )
10 continue
  do i=1,3
     vfreq(i)=0.
1002 end do
  !!    ****  read data from 'ping' vector  ****
  kpt=kthl-1
  li=kpt/4
  vfreq(3)=vfreq(2)
  vfreq(2)=vfreq(1)
  vfreq(1)=ping(1)
  do i=1,kpt+1
     tping(i)=ping(i)
1006 end do
  do l=1,li
     do im=1,4
        ping(4*l-im+2)=tping(4*iseq(l)-im+2)
1008 end do
     iold(l)=iseq(l)
1007 end do
  do i=1,kpt
     bom(i+2*kpt)=bom(i+kpt)
     bom(i+kpt)=bom(i)
     bom(i)=ping(i+1)
1003 end do
  if (ntol .lt. 2)  return
  !!     **** begin to smooth ****
  !!  ** initialization **
!!!!!      do 88 l=1, 15
!!!!!      iseq(l) = l
!!!!!   88 continue
  do 601 i=1,kpt
     cccc!      sb(i)=bom(i)
     sb(i)=tping(i+1)
601 end do
  ic=0
  iq=0
  if ( iprsup .ge. 1 )  write (*, *) '  *****  li, kpt =',  li, kpt
99 if (ic .gt. li*(li-1) ) go to 12
  if ( iq .eq. 1 ) go to 12
  iq=1
  do l=1,li
     do im=1, 4
        bom(4*l-im+1) = sb(4*iseq(l)-im+1)
702  end do
701 end do
  if ( iprsup .ge. 1 ) write (*,*) ' beginning  bom(1:4*li) =',  ( bom(ip), ip=1, 4*li )
  !   * process the data between freq, freq-1 & freq-2 *
  do j=2, kpt-2, 4
     do i=j+4, kpt-2, 4
        !   * test the crossing of ang *
        d1=bom(j+kpt) - bom(i+kpt)
        d2=bom(j) - bom(i)
        if ( d1*d2 .le. 0. ) go to 801
        d1=bom(j-1+kpt) - bom(i-1+kpt)
        d2=bom(j-1) - bom(i-1)
        if ( d1*d2 .le. 0. ) go to 801
        go to 102
        !   * test the slope change of ang , if switching *
801     continue
        aj=0.
        ai=0.
        if((bom(j+kpt) - bom(j+2*kpt)) .eq. 0. ) aj=1.0e-12
        if((bom(i+kpt) - bom(i+2*kpt)) .eq. 0. ) ai=1.0e-12
        smj1=(bom(j) - bom(j+kpt))/((bom(j+kpt) - bom(j+2*kpt))+aj)
        smi1=(bom(i) - bom(i+kpt))/((bom(i+kpt) - bom(i+2*kpt))+ai)
        smj2=(bom(i) - bom(j+kpt))/((bom(j+kpt) - bom(j+2*kpt))+aj)
        smi2=(bom(j) - bom(i+kpt))/((bom(i+kpt) - bom(i+2*kpt))+ai)
        if ( iprsup .ge. 1 ) write (*,*) ' smj1, smj2, smi1, smi2 =', smj1, smj2, smi1, smi2
        if ((abs( smj1 -1 ) .lt. abs( smj2-1 )) .and. (abs( smi1 -1 ) .lt. abs( smi2 -1 ))) go to 102
        !   * test the slope change of mag , if switching *
        !     789012345678901234567890123456789012345678901234567890123456789012
        aj=0.
        ai=0.
        if((bom(j-1+kpt) - bom(j-1+2*kpt)) .eq. 0. ) aj=1.0e-12
        if((bom(i-1+kpt) - bom(i-1+2*kpt)) .eq. 0. ) ai=1.0e-12
        smj1=
1       (bom(j-1)-bom(j-1+kpt))/((bom(j-1+kpt)-bom(j-1+2*kpt))+aj)
        smi1=
1       (bom(i-1)-bom(i-1+kpt))/((bom(i-1+kpt)-bom(i-1+2*kpt))+ai)
        smj2=
1       (bom(i-1)-bom(j-1+kpt))/((bom(j-1+kpt)-bom(j-1+2*kpt))+aj)
        smi2=
1       (bom(j-1)-bom(i-1+kpt))/((bom(i-1+kpt)-bom(i-1+2*kpt))+ai)
        if ((abs( smj1 -1 ) .lt. abs( smj2-1 )) .and.
1       (abs( smi1 -1 ) .lt. abs( smi2 -1 ))) go to 102
        c   * switching *
        iq=0
        t=iseq( (j+2)/4 )
        iseq( (j+2)/4 ) = iseq( (i+2)/4 )
        iseq( (i+2)/4 ) = t
        do im=1,4
           tt(im) = bom(j+im-2)
           bom(j+im-2) = bom(i+im-2)
           bom(i+im-2) = tt(im)
401     end do
102  end do
101 end do
  ic=ic + 1
  go to 99
12 continue
  !!  **  begin the second loop for vel and db  **
  !   * test the slope change of vel *
  ic=0
  iq=0
899 if (ic .gt. li*(li-1) ) go to 812
  if (iq .eq. 1 ) go to 812
  iq=1
  do l=1,li
     do im=1, 4
        bom(4*l-im+1) = sb(4*iseq(l)-im+1)
9702 end do
9701 end do
  !   * test the slope change of vel *
  do jb=1,li
     jbeg=jb*4-1
     aj=0.
     if((bom(jbeg+kpt)-bom(jbeg+2*kpt)) .eq. 0.) aj=1.0e-12
     saj=(bom(jbeg)-bom(jbeg+kpt))/((bom(jbeg+kpt)-bom(jbeg+2*kpt))+aj)
     if ( abs( saj - 1 ) .gt. 0.5 ) go to 8001
8901 end do
  go to 812
8001 do j=jbeg, kpt-1, 4
     do i=j+4, kpt-1, 4
        !   * test the crossing of vel or db *
        jb=(j+1)/4
        ib=(i+1)/4
        d1=bom(j+kpt) - bom(i+kpt)
        d2=bom(j) - bom(i)
        if ( d1*d2 .le. 0. ) go to 8801
        d1=bom(j+1+kpt) - bom(i+1+kpt)
        d2=bom(j+1) - bom(i+1)
        if ( d1*d2 .le. 0. ) go to 8801
        go to 8102
        !      go to 8122
8801    continue
        aj=0.
        ai=0.
        if((bom(j+kpt) - bom(j+2*kpt)) .eq. 0. )  aj=1.0e-12
        if((bom(i+kpt) - bom(i+2*kpt)) .eq. 0. )  ai=1.0e-12
        smj1=(bom(j)-bom(j+kpt))/((bom(j+kpt)-bom(j+2*kpt))+aj)
        smi1=(bom(i)-bom(i+kpt))/((bom(i+kpt)-bom(i+2*kpt))+ai)
        smj2=(bom(i)-bom(j+kpt))/((bom(j+kpt)-bom(j+2*kpt))+aj)
        smi2=(bom(j)-bom(i+kpt))/((bom(i+kpt)-bom(i+2*kpt))+ai)
        if ((abs( smj1 -1 ) .lt. abs( smj2-1 )) .and.(abs( smi1 -1 ) .lt. abs( smi2 -1 ))) go to 8102
        !     1 (abs( smi1 -1 ) .lt. abs( smi2 -1 ))) go to 8132
        !   * test the slope change of db, if switch *
        aj=0.
        ai=0.
        if((bom(j+1+kpt)-bom(j+1+2*kpt)) .eq. 0. ) aj=1.0e-12
        if((bom(i+1+kpt)-bom(i+1+2*kpt)) .eq. 0. ) ai=1.0e-12
        saj= (bom(i+1)-bom(j+1+kpt))/((bom(j+1+kpt)-bom(j+1+2*kpt))+aj)
        sai= (bom(j+1)-bom(i+1+kpt))/((bom(i+1+kpt)-bom(i+1+2*kpt))+ai)
        if ( ( abs( saj-1 ) .gt. 1.0 ) .and. ( abs( sai-1 ) .gt. 1.0 )) go to 8102
        !     1 go to 8142
        !     789012345678901234567890123456789012345678901234567890123456789012
        !   * switching *
        iq=0
        t=iseq(jb)
        iseq(jb) = iseq(ib)
        iseq(ib) = t
        do im=1,4
           tt(im) = bom(j+im-3)
           bom(j+im-3) = bom(i+im-3)
           bom(i+im-3) = tt(im)
8401    end do
        ! 8112 write(*,*) vfreq(1), i,j, 'stop 1'
        !      go to 8102
        ! 8122 write(*,*) vfreq(1), i,j, 'stop 2'
        !      go to 8102
        ! 8132 write(*,*) vfreq(1), i,j, 'stop 3'
        !      go to 8102
        ! 8142 write(*,*) vfreq(1), i,j, 'stop 4'
        !      go to 8102
8102 end do
8101 end do
  ic=ic+1
  go to 899
812 continue
  !   * check the equality between two eigenvalues *
  if(nrp.eq.1) go to 2030
  ku=0
  kunf=0
  do j=1, kpt-3, 4
     mu=0
     do i=j+4, kpt-3, 4
        if((abs(bom(j)-bom(i)).gt.abs(bom(j)*1e-6)).or.(abs(bom(j+1)-bom(i+1)).gt.abs(bom(j+1)*1e-6)))go to 1005
        if((abs(bom(j+kpt)-bom(i+kpt)).gt.abs(bom(j+kpt)*1e-6)).or. (abs(bom(j+1+kpt)-bom(i+1+kpt)).gt.abs(bom(j+1+kpt)*1e-6)))
2       go to 1005
!!!!      if((bom(j).ne.bom(i)).or.(bom(j+1).ne.bom(i+1))) go to 1005
!!!!      if((bom(j+kpt).ne.bom(i+kpt)).or.
!!!!     1 (bom(j+1+kpt).ne.bom(i+1+kpt))) go to 1005
        kunf=1
        mu=1
        kuid(ku+1)=(i+3)/4
        ku=ku+1
1005 end do
     if(mu.eq.0) go to 1004
     kuid(ku+1)=(j+3)/4
     ku=ku+1
     if ( iprsup .ge. 1 ) write (*,*) ' identical eigenvalues.   j, ku, kuid(1:ku) =', j, ku, ( kuid(ip), ip=1, ku )
1004 end do
  if (kunf.eq.0) go to 2000
  mrr=mrr+1
  go to 2000
2030 continue
  do i=1,ku
     kk=ku-i
     do j=1,kk
        if ( kuid(j).lt.kuid(j+1) ) go to 2008
        t=kuid(j)
        kuid(j)=kuid(j+1)
        kuid(j+1)=t
2008 end do
     kuse(i)=i
2007 end do
  write(*,*) ' kuid after sorting : ',(kuid(i),i=1,ku)
  do i=1,ku
     kk=ku-i
     do j=1,kk
        if ( iold(kuid(j)) .lt. iold(kuid(j+1)) ) go to 2003
        t=iold(kuid(j))
        iold(kuid(j))=iold(kuid(j+1))
        iold(kuid(j+1))=t
        t=kuse(j)
        kuse(j)=kuse(j+1)
        kuse(j+1)=t
2003 end do
2002 end do
  do i=1,ku
     kk=ku-i
     do j=1,kk
        if ( iseq(kuid(j)) .lt. iseq(kuid(j+1)) ) go to 2012
        t=iseq(kuid(j))
        iseq(kuid(j))=iseq(kuid(j+1))
        iseq(kuid(j+1))=t
2012 end do
2011 end do
  do l=1,li
     iseqt(l)=iseq(l)
2010 end do
  do i=1,ku
     iseq(kuid(kuse(i)))=iseqt(kuid(i))
2020 end do
15 continue
  !   * re-ordering, if necessary *
2000 if ( ny .eq. 2 ) go to 2004
  if ( vfreq(1) .ne. chf ) go to 2004
  do l=1,li
     iseqt(l)=iseq(l)
500 end do
  do l=1,li
     iseq(l) = iseqt(iseqa(l))
501 end do
2004 continue
  do l=1,li
     do im=1, 4
        bom(4*l-im+1) = sb(4*iseq(l)-im+1)
2006 end do
2005 end do
  if ( iprsup .ge. 1 ) write(*,*) ' end of unwind.  vfreq(1) =',  vfreq(1)
  if ( iprsup .ge. 1 ) write(*,*) ' end of unwind.   iseq(1:li) =', (iseq(l),l=1,li)
  if ( iprsup .ge. 1 ) write(*,*) ' end of unwind.   kthl, mrr, nrp, ntol =', kthl, mrr, nrp, ntol
11 continue
  do i=1,kpt
     ping(i+1)=bom(i)
2001 end do
  !!    **  reorder the modal quantities  **
!!!!      do 1009 i=1,2*li
!!!!      tzz(i)=zz(i)
!!!!      tps(i)=ps(i)
!!!! 1009 continue
!!!!      do 1010 i=1,li
!!!!      zz(i)=tzz(iseq(i))
!!!!      zz(i+li)=tzz(iseq(i)+li)
!!!!      ps(i)=tps(iseq(i))
!!!!      ps(i+li)=tps(iseq(i)+li)
!!!c 1010 continue
!!!      write(*,*) 'done with unwind '
  return
end subroutine unwind
!
! subroutine zymx.
!
subroutine zymx( w,nw,isyst,ngrnd, ngg,ncpp, radi,zy,yz,dir,dij,ang,usi,usr,esi,dr0,th0,al0,hi,di,bio,bi1,bi2,bi3,bi4, &
     bi5,al1i,al2i,al3i,dci, nx, yzn,   ys,yc,zp,zpc,zs,ze,zc,ca,cb,cc,cd,f, ldm, ldn, ldn2, lnq2 )
  implicit real*8 (a-h, o-z),integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labl47.ftn'
  complex*16  zpc(ldn, ldn), cmplxz, cjw
  complex*16  ys(ldn, ldn), yc(ldn, ldn), zp(ldn, ldn)
  complex*16  zs(ldn, ldn), ze(ldn, ldn), zc(ldn, ldn)
  complex*16  ca(ldn, ldn), cb(ldn, ldn), cc(ldn, ldn)
  complex*16  cd(ldn, ldn), f(ldn, ldn2)
  dimension  ngg(ldn), ncpp(ldm), yzn(lnq2)
  dimension  radi(ldm, 7),  zy(ldn, ldn),  yz(ldn, ldn)
  dimension  dij(ldm, ldm), ang(ldm, ldm), usi(ldm, 3), usr(ldm, 3)
  dimension  dir(ldm, ldm), esi(ldm, 3),   dci(ldm),    hi(ldm)
  dimension  dr0(ldm, ldm), th0(ldm, ldm), al0(ldm, ldm), di(ldn)
  dimension  bio(ldm), bi1(ldm),bi2(ldm), bi3(ldm), bi4(ldm)
  dimension  bi5(ldm), al1i(ldm), al2i(ldm), al3i(ldm)
  ll0=0
  ll1=1
  nx=ngrnd
  do i=1, ldn
     do j=1, ldn
        ys(i,j) = czero
        yc(i,j) = czero
        zp(i,j) = czero
        zpc(i,j) = czero
        zs(i,j) = czero
        zc(i,j) = czero
5       ze(i,j) = czero
     end do
  end do
  cjw = cmplxz ( fzero, w )
  ll2 = 2
  if ( iprs47  .ge.  1 ) write (logsix, 2706)  nw, isyst, np2, itypec, numaki, npc, ncc, izflag, iyflag, w
2706 format ( /,  16h begin  'zymx' .,  24h      nw   isyst     np2, 48h  itypec  numaki     npc     ncc  izflag  iyflag, 19x, 1hw, /, 16x, 9i8, e20.11 )
  go to  (710, 720, 30), itypec
710 call  olzy ( w,ncpp,zy,dij,ang,usi,usr,esi,hi,di,zs,ze,zc,ldm, ldn )
  if (kill .ge. 1)   go to 9200
  go to 800
720 call  sczy1 ( w, isyst, zy, dir, dij, ang, hi, di, zs, ze,ldm, ldn )
  if (kill .ge. 1)   go to 9200
  call  sczy2 ( cjw, ncpp, radi, usi, usr, bio, bi1, bi2, bi3, bi4, bi5, al1i, al2i, al3i, zc, ldm, ldn )
  if (kill .ge. 1)   go to 9200
  go to 800
30 if ( npp  .ne.  0 ) call  sczy1 ( w, isyst, zy, dir, dij, ang, hi, di, zs, ze, ldm, ldn )
  if (kill .ge. 1)   go to 9200
  call  sczy2 ( cjw, ncpp, radi, usi, usr, bio, bi1, bi2, bi3, bi4, bi5, al1i, al2i, al3i, zc, ldm, ldn )
  if (kill .ge. 1)   go to 9200
  call ptzy2(cjw,ncpp,dci,dr0,th0,al0,zp,zpc,ldm,ldn)
  if (kill .ge. 1)   go to 9200
800 if (numaki .gt. 3)   go to 840
  if (iprs47 .lt. 1)   go to 840
943 format(  /,10x,18hearth impedance   )
  write(lunit6,944)
944 format (   / ,  10x,  31hconductor internal impedance      )
  call print(zc,nw,ll2,ldn)
  if ( itypec  .ne.  3 )   go to 105
  write(lunit6,961)
961 format( /, 10x,14hpipe impedance   )
  call print ( zp, nw, ll2, ldn )
  if ( npp  .eq.  0 )   go to 840
  write(lunit6,962)
962 format( /, 10x,20hconnection impedance   )
  call print (zpc, nw, ll2, ldn)
105 write (lunit6, 943)
  call print(ze,nw,ll2,ldn)
  if ( isyst  .eq.  -1 )   go to 840
  write(lunit6, 946)
946 format (  /, 10x, 18hspace impedance   )
  call print(zs,nw,ll2,ldn)
840 do i=1, nw
     do j=1, nw
        c1 = e2p * yz(i,j)
        yc(i,j) = cjw * cmplxz(c1, fzero)
250     zc(i,j)=zc(i,j)+ze(i,j)+zs(i,j)+zp(i,j)+zpc(i,j)
     end do
  end do
  if(itypec .eq. 1 .and. nenerg .ne. 0)  go to 335
  i = nx
1945 i = i - 1
  if ( ngg(i)  .lt.  1 )  go to 1966
  mdx = i + 1
  do j = 1, nw
1931 yzn(j) = realz(zc(i, j))
  end do
  do j = mdx, nx
     mdy = j - 1
     do k = 1, nw
        yc(mdy, k) = yc(j, k)
1933    zc(mdy, k) = zc(j, k)
     end do
  end do
  do j = 1, nw
1937 zc(nx, j) = cmplxz(yzn(j),fzero)
  end do
  do j = 1, nw
1939 yzn(j) = realz(zc(j, i))
  end do
  do j = mdx, nx
     mdy = j - 1
     do k = 1, nw
        yc(k, mdy) = yc(k, j)
1941    zc(k, mdy) = zc(k, j)
     end do
  end do
  do j = 1, nw
1943 zc(j, nx) = cmplxz(yzn(j),fzero)
  end do
  nx = nx - 1
1966 if ( i  .gt.  1 )  go to 1945
  if (itypec .ne. 1)   go to 300
  if (nw-nx) 890, 340, 310
300 if(nx .eq. nw)  go to 350
310 call minvn(zc,nw,nx,ll1,ldn,ca,cb,cc,cd,ze,zp,zpc,f,ldn2)
  if(itypec .ne. 1) go to 350
  if (kill .ge. 1)   go to 9200
  go to 340
335 nx = nw
340 if (isyst .eq. 2) call transp(zc, ncpp, yzn(1), yzn(ldn2), ca, ldm, ldn)
  if (kill .ge. 1)   go to 9200
  if (ialter .le. 0)  go to 90
  if (numaki .lt. 9 )  go to 90
350 if(ncros.eq.0 .or. npais.lt.0) go to 890
  nx=4
  call crosa4( zc,ll0,ldn,ca,cb,cc,cd,ze,zp,zpc,f,ldn2 )
  call crosa4( yc,ll1,ldn,ca,cb,cc,cd,ze,zp,zpc,f,ldn2 )
890 n1 = nx
  n2 = 2 * n1 * n1
  k = 1
  do j = 1, n1
     do i = 1, n1
        yzn(k) = realz(yc(i,j))
        l = k + 1
        yzn(l) = aimagz(yc(i,j))
        k = l + 1
894  end do
898 end do
  write (lunit3) (yzn(i), i=1, n2)
  if (iprsup .lt. 1) go to 1983
  freq = w / twopi
  write (lunit6, 1900) freq, (yzn(i),i=1,n2)
1900 format ( 30h  y written on lunit3 at freq , e16.7,2x, 3hare,/, (1x,8e15.6) )
1983 k = 1
  do j = 1, n1
     do i = 1, n1
        yzn(k) = realz(zc(i,j))
        l = k + 1
        yzn(l) = aimagz(zc(i,j))
        k = l + 1
1919 end do
1918 end do
  write (lunit3) (yzn(i), i=1, n2)
  if (iprsup .lt. 1) go to 90
  freq = w / twopi
  write (lunit6, 2010) freq, (yzn(i),i=1,n2)
2010 format ( 30h  z written on lunit3 at freq , e16.7,2x,3hare,/, (1x,8e15.6) )
90 if (numaki .gt. 3)   go to 900
  if (izflag .eq. 0)   go to 100
  write (lunit6, 94)
94 format(/, 10x, 16himpedance matrix)
  call print(zc, nx  ,ll2, ldn )
  if (izflag .eq. 1)   go to 150
100 continue
  write (lunit6, 125)
125 format(/, 10x, 25hresistance and inductance)
  !         begin code associated with [z] dump onto unit-34 plot file
!!!!      k = 1
!!!!      sing(k) = alog1z ( w / twopi )
  do i=1, nx
     do j=1, nx
        d1 = realz(zc(i,j) )
        d2 = aimagz( zc(i,j) ) / w
!!!      sing(k+1) = d1            ! store resistance in real*4 vector
!!!!      sing(k+2) = d2            ! store inductance in real*4 vector
!!!!      k = k + 2          !  advance index to last-stored number
140     zc(i,j) = cmplxz(d1, d2)
     end do
  end do
!!!!      write (junit4) ( sing(j), j=1, k ) ! output vector written to
!!!!      write (*, 4488)  sing(1), sing(2), sing(3), sing(k-1), sing(k)
!!!! 4488 format ( ' sing(1), sing(2), sing(3), sing(k-1), sing(k) =',
!!!!     1         f10.4, 4e13.4 )
  call print(zc, nx   , ll2, ldn)
  !     now restore  'zc'  to impedance in  do 4908  loop below.
  do i=1, nx
     do j=1, nx
        d1 = realz(zc(i,j))
        d2 = aimagz( zc(i,j) )  *  w
4908    zc(i,j) = cmplxz(d1, d2)
     end do
  end do
150 if (iyflag .eq. 0)   go to 160
  write (lunit6, 155)
155 format(/, 10x, 17hadmittance matrix)
  call print (yc, nx, ll2, ldn)
  if (iyflag .eq. 1)   go to 900
160 write (lunit6, 165)
165 format(/, 10x, 27hconductance and capacitance)
  do i=1, nx
     do j=1, nx
        d3 = realz(yc(i,j))
        d4 = aimagz( yc(i,j) ) / w
180     yc(i,j) = cmplxz(d3,d4)
     end do
  end do
  call print (yc, nx, ll2, ldn)
  !     now restore  'yc'  to admittance in  do 4928  loop below.
  do i=1, nx
     do j=1, nx
        d1 = realz(yc(i,j))
        d2 = aimagz( yc(i,j) ) * w
4928    yc(i,j) = cmplxz(d1, d2)
     end do
  end do
9200 continue
900 if ( iprs47  .ge.  1 ) write (logsix, 2794)
2794 format ( /,  15h exit  'zymx' .  )
  return
end subroutine zymx
!
! subroutine ymatr.
!
subroutine ymatrx (isyst, lunit6, ncpp, zy, yz, esi, al0, al1i, al2i, al3i, a1, a2, ldm, ldn )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  dimension  a1(ldn,ldn), a2(ldn,ldn), ncpp(ldm)
  dimension  zy(ldn, ldn), yz(ldn, ldn), al0(ldm, ldm)
  dimension  esi(ldm, 3), al1i(ldm), al2i(ldm), al3i(ldm)
  do i=1,ldn
     do j=1,ldn
        a1(i,j) = 0.0
        a2(i,j) = 0.0
        yz(i,j) = 0.0
10   end do
  end do
  if ( iprs47  .ge.  1 ) write (lunit6, 1719)  isyst, np2, itypec, npc, npp
1719 format ( /,  28h at beginning of  'ymatrx' .,40h   isyst     np2  itypec     npc     npp    ,/,28x,  5i8  )
  if ( iprs47  .ge.  2 ) write (lunit6, 1724)  ( i, ncpp(i), al1i(i), al2i(i), al3i(i), esi(i,1), esi(i,2), esi(i,3), i=1, 10)
1724 format ( /,  5x,  3hrow,  4x,  4hncpp,  11x,  4hal1i, 11x,4hal2i,11x,  4hal3i,  7x,  8hesi(i,1),  7x,  8hesi(i,2),7x,8hesi(i,3),/, ( 1x, i7, i8, 6e15.6 )   )
  if(itypec . eq . 3 )  go to 25
  if(isyst.eq.-1) go to 25
  do i=1,npc
     do j=1,npc
        if(j.lt.i) go to 15
        yz(i,j)=zy(i,j)
        yz(j,i)=zy(i,j)
15   end do
  end do
  if( itypec . eq . 1 ) go to 200
  do i=1,ncc
     i1=i
     if(i.gt.npc) i1=i-npc
     if(i.gt.npc2) i1=i-npc2
     do j=1,ncc
        if(j.lt.i) go to 20
        j1=j
        if(j.gt.npc) j1=j-npc
        if(j.gt.npc2) j1=j-npc2
        yz(i,j)=zy(i1,j1)
        yz(j,i)=yz(i,j)
20   end do
  end do
25 do i=1,npc
     i1=i+npc
     i2=i+npc2
     if( i2 . gt . ncc )  go to 35
     a1(i2,i2)=al3i(i)/esi(i,3)
     a1(i2,i)=a1(i2,i2)
     a1(i,i2)=a1(i2,i2)
     a1(i1,i2)=a1(i2,i2)
     a1(i2,i1)=a1(i2,i2)
35   if(ncpp(i).eq.1) i1=i
     if(i1.gt.npc2) go to 39
     a1(i1,i1)=al2i(i)/esi(i,2)+a1(i2,i2)
     a1(i1,i)=a1(i1,i1)
     a1(i,i1)=a1(i1,i1)
39   a1(i,i)=a1(i1,i1)+al1i(i)/esi(i,1)
40 end do
  if( itypec . eq . 3 )  go to 60
  do i=1,ncc
     do j=1,ncc
        if(j.lt.i) go to 50
        yz(i,j)=yz(i,j)+a1(i,j)
        yz(j,i)=yz(i,j)
50   end do
  end do
  go to 200
60 ypo1 = 0.0
  if(npp.ne.0) ypo1 = alpi / es2
  if(npp.eq.0) zy(1,1)=0.
  if(isyst.ne.-1) ypo1 = ypo1 + zy(1,1)
  do i=1,npc
     i1=i+npc
     i2=i+npc2
     do j=1,npc
        j1=j+npc
        j2=j+npc2
        a2(i,j)=al0(i,j)/es1
        if(ncpp(i).eq.1) go to 65
        a2(i1,j)=a2(i,j)
        a2(i1,j1)=a2(i,j)
65      if(ncpp(j).eq.1) go to 70
        a2(i,j1)=a2(i,j)
        if(ncpp(i).eq.2) go to 67
        a2(i2,j)=a2(i,j)
        a2(i2,j1)=a2(i,j)
        a2(i2,j2)=a2(i,j)
67      if(ncpp(j).eq.2) go to 70
        a2(i,j2)=a2(i,j)
        a2(i1,j2)=a2(i,j)
70   end do
80 end do
  do i=1,ncc
     if(npp.eq.0) go to 85
     a1(i,ncc) = 0.
     a1(ncc,i) = 0.
     a2(i,ncc) = 0.
     a2(ncc,i) = 0.
85   do j=1,ncc
        if(j.lt.i) go to 90
        yz(i,j)=a1(i,j)+a2(i,j)+ypo1
        yz(j,i)=yz(i,j)
90   end do
  end do
200 if ( iprs47  .ge.  1 ) write (logsix, 2876)  ncc, ypo1, es1, alpi, yz(1,1), yz(1,2)
2876 format ( /,  17h exit  'ymatrx' .,  8h     ncc,  16x,  4hypo1, 17x,  3hes1,  16x,  4halpi,  13x,  7hyz(1,1),13x,  7hyz(1,2)  ,/,  17x,  i8,  5e20.11  )
  return
end subroutine ymatrx
!
! subroutine simp.
!
subroutine  simp(nw,h,dd,rad,zy,dir,dij,ang,ldm,ldn)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  dimension  rad(ldn), h(ldm), dd(ldn),  zy(ldn, ldn)
  dimension  dir(ldm, ldm), dij(ldm, ldm), ang(ldm, ldm)
  if ( iprs47  .ge.  1 ) write (logsix, 2924)  nw, itypec, h(1), dd(1), dd(2), rad(1), radp(3)
2924 format ( /,  16h enter  'simp' .,  16h      nw  itypec, 16x,  4hh(1),  15x,  5hdd(1),  15x,  5hdd(2),  14x,  6hrad(1), 13x,  7hradp(3)  ,/,  16x,  2i8,  5e20.11 )
  do i=1, nw
     do j=1, nw
        dij(i,j) = 0.0
        dir(i,j) = 0.0
        ang(i,j) = 0.0
5       zy(i,j) = 0.0
     end do
  end do
  do i=1,nw
     r = rad(i)
     if ( itypec  .eq.  3 )   r = radp(3)
     do j=1,nw
        if (i .ge. j)   go to 15
        v = dd(i) - dd(j)
        r1 = h(i) - h(j)
        r2 = absz ( h(i) + h(j) )
        ang(i,j) = atanz(absz(v)/r2)
        ang(j,i)=ang(i,j)
        v = v*v
        r1 = r1 * r1
        r2 = r2 * r2
        dij(i,j) = sqrtz(v + r2)
        dij(j,i)=dij(i,j)
        dir(i,j) = sqrtz(v + r1)
        dir(j,i)=dir(i,j)
        go to 20
15      if (i .gt. j)   go to 30
        ang(i,j) = 0.
        dij(i,j) = 2. * absz ( h(i) )
        dir(i,j) = r
20      zy(i,j) = alogz( dij(i,j)/dir(i,j) )
        zy(j,i)=zy(i,j)
30   end do
  end do
  if ( iprs47  .ge.  1 ) write (logsix, 2936)  dir(1,2), dij(1,2), v, zy(1,1), zy(1,2)
2936 format ( /,  15h exit  'simp' .,  12x,  8hdir(1,2), 12x,  8hdij(1,2),  19x,  1hv,  13x,  7hzy(1,1), 13x,  7hzy(1,2)  ,/,  15x,  5e20.11 )
  if ( iprs47  .ge.  5 ) write (logsix, 2948)  ( ( zy(i,j), j=1, nw ),  i=1, nw )
2948 format ( /,  47h exit  'simp' .   ((zy(i,j), j=1, nw), i=1, nw),/,  ( 1x,  6e20.11 ) )
  return
end subroutine simp
!
! subroutine sczy1.
!
subroutine  sczy1 ( w,isyst,zy,dir,dij,ang,hi,di,zs,ze,ldm,ldn )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  dimension  zy(ldn, ldn), dir(ldm, ldm), dij(ldm, ldm)
  dimension  ang(ldm, ldm), hi(ldm), di(ldn)
  complex*16  zs(ldn, ldn),  ze(ldn, ldn)
  complex*16  cjw, xe
  complex*16  cmplxz
  cjw = cmplxz ( fzero, w )
  if ( iprs47  .ge.  1 ) write (logsix, 3027)  isyst, npc, iearth, itypec, roe, u0, w
3027 format ( /,  17h enter  'sczy1' ., 32h   isyst     npc  iearth  itypec,  17x,  3hroe,  18x,  2hu0, 19x, 1hw, /, 17x, 4i8, 3e20.11 )
  c2 = u2p * zy(1,1)
  if(itypec .eq. 3) go to 50
  if(isyst.eq.-1) go to 35
  do i = 1, npc
     do j=1,npc
        c1 = u2p * zy(i,j)
33      zs(i,j) = cjw * cmplxz ( c1, fzero )
     end do
  end do
35 do i=1,npc
     do j=1,npc
        if(j.lt.i) go to 40
        if ( iearth  .eq.  99 )   go to 37
        be1=dir(i,j)*sqrtz(u0/roe)
        if(isyst.ne.-1) be1=0.
        be2=dij(i,j)*sqrtz(u0/roe)
        th=ang(i,j)
        call zegen(be1,be2,th,w,xe,isyst)
        go to 38
37      d12 = absz( di(j) - di(i) )
        call zest ( hi(i), hi(j), d12, roe, w, xe )
38      ze(i,j) = xe
        ze(j,i)=xe
40   end do
  end do
  if ( iprs47  .ge.  2 ) write (logsix, 3039)
3039 format ( /,  44h diagnostic within  'sczy1' ,   ze(i,j)  for, 18h(i,j)=1, ... npc .    )
  ll0 = 0
  if ( iprs47  .ge.  4 ) call print ( ze(1,1), npc, ll0, ldn )
  if( npc . eq . ncc ) go to 90
  if (isyst .eq. -1)  go to  45
  do i=1, ncc
     do j=1, ncc
        if (j .lt. i)  go to 43
        i1 = i
        j1 = j
        if (i1 .gt. npc2)  i1=i-npc2
        if (i1 .gt. npc)  i1=i-npc
        if(j1 .gt. npc2)  j1=j-npc2
        if (j1 .gt. npc)  j1=j-npc
        zs(i,j) = zs(i1,j1)
        zs(j,i) = zs(i,j)
43   end do
  end do
45 do i=1,ncc
     do j=1,ncc
        if (j .lt. i)  go to 47
        i1 = i
        j1 = j
        if (i1 .gt. npc2)  i1=i-npc2
        if (i1 .gt. npc)  i1=i-npc
        if(j1 .gt. npc2)  j1=j-npc2
        if (j1 .gt. npc)  j1=j-npc
        ze(i,j) = ze(i1, j1)
        ze(j,i) = ze(i,j)
47   end do
  end do
  go to 90
50 if(isyst.eq.-1) go to 60
  if(npp.eq.0) go to 90
  do i=1,ncc
     do j=1,ncc
        if(j.lt.i) go to 55
        zs(i,j) = cjw * cmplxz(c2, fzero)
        zs(j,i)=zs(i,j)
55   end do
  end do
60 be1=dir(1,1)*sqrtz(u0/roe)
  if(isyst.ne.-1) be1=0.
  be2=dij(1,1)*sqrtz(u0/roe)
  th=ang(1,1)
  call zegen(be1,be2,th,w,xe,isyst)
  do i=1,ncc
     do j=1,ncc
70      ze(i,j)=xe
     end do
  end do
90 if ( iprs47  .ge.  2 ) write (logsix, 3056)  xe
3056 format ( /,  45h diagnostic at exit  'sczy1' .   zs(i,j)  for, 33h  (i,j)=1, ... npc .    real-xe =,  e20.11, 4x,  9himag-xe =,  e20.11 )
  if ( iprs47  .ge.  4 ) call print ( zs(1,1), npc, ll0, ldn )
  return
end subroutine sczy1
!
! subroutine sczy2.
!
subroutine  sczy2 ( s, ncpp, radi, usi, usr, bio, bi1,bi2, bi3, bi4, bi5, al1i, al2i, al3i, zc, ldm, ldn )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  common  / komthl /  pekexp
  complex*16  s, ss, s1, s2, s3, s4, s5, s6, s8, su0
  complex*16  z11, z12, z2i, z2m, z2o, z23
  complex*16  cexpz, cmplxz, csqrtz
  complex*16  s0, s7, z3i, z3m, z3o, z34
  complex*16  c1, c2,  c3
  complex*16  zc(ldn, ldn)
  dimension  ncpp(ldm), radi(ldm, 7), usi(ldm, 3), usr(ldm, 3)
  dimension  bio(ldm), bi1(ldm), bi2(ldm), bi3(ldm), bi4(ldm)
  dimension  bi5(ldm), al1i(ldm), al2i(ldm), al3i(ldm)
  ll1 = 1
  ll2 = 2
  if ( iprs47  .ge.  1 ) write (logsix, 3107)  npc, s
3107 format ( /,  17h enter  'sczy2' .,  8h     npc,  14x,  6hreal-s,14x,  6himag-s  ,/,  17x,  i8,  2e20.11 )
1001 inm=0
  do i=1,npc
     b0=bio(i)
     b1=bi1(i)
     b2=bi2(i)
     b3=bi3(i)
     b4=bi4(i)
     b5=bi5(i)
     al1=al1i(i)
     al2=al2i(i)
     al3=al3i(i)
     ss=csqrtz(s)
     c1 = cmplxz(b0, fzero)
     s0=c1*ss
     c1 = cmplxz(b1, fzero)
     s1 = c1 * ss
     c1 = cmplxz(b2, fzero)
     s2=c1*ss
     c1 = cmplxz(b3, fzero)
     s3=c1*ss
     c1 = cmplxz(u2p, fzero)
     su0=s*c1
     if ( iprs47  .ge.  2 ) write (logsix, 3124)  i, b0, su0, s0
3124 format ( /,  20h re-loop over  'i' .,  8h       i,  18x,  2hb0,12x,  8hreal-su0,  12x,  8himag-su0,  13x,  7hreal-s0,13x,  7himag-s0  ,/,  19x,  i8,  5e20.11 )
     c1 = cmplxz (usr(i,1), fzero)
     ixa=0
     xa=cabsz(s0)
     if(radi(i,1).le.0.) xa=cabsz(s1)
     if(xa.gt.10.) ixa=1
     call bsikm(s1,ll2,bin,bkn,ll1,ixa)
     if(radi(i,1).gt.0. ) go to 15
     z11=su0/s1*bin(1)/bin(2)*c1
     go to 25
15   s4=bin(1)
     s5=bin(2)
     s6=bkn(1)
     z12=bkn(2)
     call bsikm(s0,ll2,bin,bkn,ll1,ixa)
     if (ixa .gt. 0)  go to 20
     ss = s5 * bkn(2) - z12 * bin(2)
     z11=su0/s1*(bkn(2)*s4+s6*bin(2))/ss*c1
     go to 25
20   ss=s1-s0
     if(cabsz(ss) .gt. pekexp ) go to 23
     ss=cexpz(ss)
     s7=s5*bkn(2)*ss-z12*bin(2)/ss
     z11=su0/s1*(bkn(2)*s4*ss+s6*bin(2)/ss)/s7*c1
     go to 25
23   z11=su0/s1*s4/s5*c1
25   d1 = usi(i,1)*al1
     c1 = cmplxz(d1, fzero)
     z12=su0*c1
     z2i = czero
     z2m = czero
     z2o = czero
     d1 = usi(i,2) * al2
     c1 = cmplxz(d1, fzero)
     z23 = su0 * c1
     if ( iprs47  .ge.  3 ) write (logsix, 3136)  ixa, ncpp(i), radi(i,1), z11, z12
3136 format ( /,  1x,  16h     ixa ncpp(i),  11x,  9hradi(i,1), 12x,  8hreal-z11,  12x,  8himag-z11, 12x,  8hreal-z12,  12x,  8himag-z12  ,/,  1x,  2i8,  5e20.11 )
     z3i = czero
     z3m = czero
     z3o = czero
     d1 = usi(i,3) * al3
     c1 = cmplxz(d1, fzero)
     z34 = su0 * c1
     if(ncpp(i).eq.1) go to 90
     ido=2
28   ixa=0
     xa=cabsz(s2)
     if(xa.gt.10) ixa=1
     call bsikm(s2,ll2,bin,bkn,ll1,ixa)
     s4=bin(1)
     s5=bin(2)
     s1=bkn(1)
     s0=bkn(2)
     call bsikm(s3,ll2,bin,bkn,ll1,ixa)
     c1 = cmplxz(usr(i,ido), fzero)
     if (ixa .gt. 0)  go to 35
     ss = bin(2) * s0 - s5 * bkn(2)
     s7=su0/s2*(s4*bkn(2)+s1*bin(2))/ss*c1
     s8=su0/s3*(bin(1)*s0+bkn(1)*s5)/ss*c1
     go to 40
35   s6=s3-s2
     if(cabsz(s6) .gt. pekexp ) go to 42
     s6=cexpz(s6)
     ss=bin(2)*s0*s6-s5*bkn(2)/s6
     s7=su0/s2*(s4*bkn(2)/s6+s1*bin(2)*s6)/ss*c1
     s8=su0/s3*(bin(1)*s0*s6+bkn(1)*s5/s6)/ss*c1
40   if (ido .ne. 2 ) go to 41
     d1 = u2p*radi(i,3)/radi(i,4)/b2/b2*usr(i,2)
     c2 = cmplxz(d1, fzero)
     z2m = c2 / ss
     go to 44
41   d1 = u2p*radi(i,5)/radi(i,6)/b4/b4*usr(i,3)
     c2 = cmplxz(d1, fzero)
     if (ido .eq. 3)  z3m = c2/ss
     go to 44
42   s7=su0/s2*s1/s0*c1
     s8=su0/s3*bin(1)/bin(2)*c1
     z2m = czero
     z3m = czero
44   if ( iprs47  .ge.  3 ) write (logsix, 3148)  z2i, z2o, z2m, z23
3148 format ( /,  1x, 8x,  8hreal-z2i,  8x,  8himag-z2i, 8x,  8hreal-z2o,  8x,  8himag-z2o, 8x,  8hreal-z2m,  8x, &
          8himag-z2m, 8x,  8hreal-z23,  8x,  8himag-z23  ,/,  1x,  8e16.7  )
     if ( ido  .eq.  3 )   go to 60
     z2i=s7
     z2o=s8
     z3i = czero
     z3m = czero
     z3o = czero
     z34 = czero
     if(ncpp(i).eq.2) go to 90
     ss=csqrtz(s)
     c1 = cmplxz(b4, fzero)
     s2=c1*ss
     c1 = cmplxz(b5, fzero)
     s3 = c1*ss
     ido=3
     go to 28
60   z3i=s7
     z3o=s8
90   s1=z11+z12+z2i
     s2=z2o+z23+z3i
     s3=z3o+z34
     s4=s3-2.*z3m
     s5=s3-z3m
     s6=s2+s4
     s7=s6-z2m
     s8 = cimag1
     d1 = aimagz(s5)
     d2 = aimagz(s6)
     d3 = aimagz(s7)
     c1 = cmplxz(d1, fzero)
     c2 = cmplxz(d2, fzero)
     c3 = cmplxz(d3, fzero)
     if(realz(s5) .lt. 0.) s5=s8*c1
     if(realz(s6) .lt. 0.) s6=s8*c2
     if(realz(s7) .lt. 0.) s7=s8*c3
     zc(i,i)=s1+s2-2.*z2m+s4
     if(ncpp(i).eq.1) go to 100
     i1=i+npc
     if(i1.gt.npc2) go to 100
     zc(i1,i1)=s6
     zc(i,i1)=s7
     zc(i1,i)=s7
     if(ncpp(i).eq.2) go to 100
     i1=i+npc2
     zc(i1,i1)=s3
     zc(i1,i)=s5
     zc(i,i1)=s5
     i2=i+npc
     zc(i1,i2)=s5
     zc(i2,i1)=s5
     if ( iprs47  .ge.  3 ) write (logsix, 3159)  s1, s2, zc(i,i), zc(i,i1)
3159 format (/,  " Store  'zc'  values.", 13x, 'real-s1', 13x, 'imag-s1', 13x, 'real-s2', 13x, 'imag-s2', /, 21x, 4e20.11  ,/, &
          8x, 'real-zc(i,i)', 8x, 'imag-zc(i,i)', 7x, 'real-zc(i,i1)', 7x, 'imag-zc(i,i1)  ',/, &
          1x,  4e20.11 )
100 end do
  if ( iprs47  .ge.  1 ) write (logsix, 3174)
3174 format ( /,  16h exit  'sczy2' .   )
  return
end subroutine sczy2
!
! subroutine ptzy1.
!
subroutine  ptzy1 ( radi, dci, thc, dr0, th0, al0, ldm )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  dimension  radi(ldm, 7), dci(ldm), thc(ldm)
  dimension  dr0(ldm, ldm), th0(ldm, ldm), al0(ldm, ldm)
  p2=2.*pai
  bp1=radp(1)*sqrtz(u0/rop*usp)
  bp2=radp(2)*sqrtz(u0/rop*usp)
  if ( iprs47  .ge.  1 ) write (logsix, 3238)  npp, npc, bp1, bp2, radp(3)
3238 format ( /,  17h begin  'ptzy1' .,  16h     npp     npc, 17x,  3hbp1,  17x,  3hbp2,  13x,  7hradp(3)  ,/, &
       17x,  2i8,  3e20.11 )
  if (npp .eq. 0)   go to 5
  alpi=alogz(radp(3)/radp(2))
  go to 8
5 alpi = 0.0
8 do i=1, npc
     do j=1,npc
        dr0(i,j)=dci(i)*dci(j)/radp(1)**2
        if(i-j) 10,20,50
10      th0(i,j)=(thc(j)-thc(i))*pai/180.
        if(th0(i,j).gt.p2) th0(i,j)=th0(i,j)-p2
        dkl = sqrtz(dci(i)**2+dci(j)**2-2.*dci(i)*dci(j)*cosz(th0(i,j)))
        al0(i,j) = alogz(radp(1)/dkl)
        th0(j,i)=th0(i,j)
        al0(j,i)=al0(i,j)
        go to 50
20      th0(i,j)=0.
        dkl = radi(i,7)
        al0(i,j) = alogz(radp(1)/dkl*(1.-dr0(i,j)))
50   end do
  end do
  n = 19
  do i=1, npc
     do j=1, npc
        cn = 0.0
        if (i .eq. j)   go to 70
        if (j .lt. i)   go to 70
        if (dci(i) * dci(j) .lt. 1.0e-6)   go to 65
        do k=1, n
           ak = k
           cn = dr0(i,j) **k * cosz(ak * th0(i,j)) / ak + cn
60      end do
65      al0(i,j) = al0(i,j) - cn
        al0(j,i) = al0(i,j)
70   end do
  end do
  if ( iprs47  .ge.  1 ) write (logsix, 3256)  dkl, cn, alpi, al0(1,1), al0(1,2)
3256 format ( /,  16h exit  'ptzy1' .,  17x,  3hdkl,  18x,  2hcn, 16x,  4halpi,  12x,  8hal0(1,1),  12x,  8hal0(1,2)  ,/, &
       16x,  5e20.11 )
  if ( iprs47  .ge.  4 ) write (logsix, 3263)  ( (al0(i,j), j=1, npc), i=1, npc )
3263 format ( /,  63h diagnostic output matrix.    ( (al0(i,j), j=1, npc), i=1, npc)     ,/,  ( 1x,  6e20.11 ) )
  return
end subroutine ptzy1
!
! subroutine ptzy2.
!
subroutine ptzy2(s,ncpp,dci,dr0,th0,al0,zp,zpc,ldm,ldn)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include  'labl47.ftn'
  common  / komthl /  pekexp
  complex*16  s, ss, s1, s2, s3, s4, s5, s6, su0
  complex*16  cexpz, cmplxz, csqrtz
  complex*16  se0, zm, zi, zzo, zzi
  complex*16  c1, c2, c3, c4, c5, c6
  complex*16  zp(ldn, ldn), zpc(ldn, ldn)
  dimension  ncpp(ldm),  dci(ldm)
  dimension  dr0(ldm, ldm), th0(ldm, ldm), al0(ldm, ldm)
  unity = 1.0
  contwo = 2.0
  c3 = cmplxz(usp, fzero)
  c4 = cmplxz(contwo, fzero)
  c8 = rop/2./pai/radp(1)/radp(2)
  c6 = cmplxz(c8, fzero)
  ll1 = 1
  ll3 = 3
  nbess=19
  kn=nbess+1
  ss=csqrtz(s)
  su0 = s * cmplxz(u2p, fzero)
  se0 = s * cmplxz(e2p, fzero)
  s1 = ss * cmplxz(bp1, fzero)
  s2 = ss * cmplxz(bp2, fzero)
  zm = czero
  zzo = su0 * cmplxz(alpi, fzero)
  ixa=0
  xa=cabsz(s1)
  if(xa.gt.10.) ixa=1
  if ( iprs47  .ge.  1 ) write (logsix, 3316)  npc, npp, xa, alpi, s
3316 format ( /,  17h begin  'ptzy2' .,  16h     npc     npp, 18x,  2hxa,  16x,  4halpi,  14x,  6hreal-s,  14x,  6himag-s,/,  17x,  2i8,  4e20.11 )
  if(npp.eq.0) go to 40
  call bsikm(s2,kn,bin,bkn,ll1,ixa)
  se0=bin(2)
  s3=bkn(2)
  s4=bin(1)
  s5=bkn(1)
40 call bsikm(s1,kn,bin,bkn,ll3,ixa)
  if(npp.ne.0) go to 42
  zzi=su0*c3*bkn(1)/bkn(2)/s1
  go to 55
42 if (ixa .eq. 1) go to 45
  ss = se0*bkn(2)-bin(2)*s3
  zm = c6/ss
  zzo =su0*c3/s2*(s4*bkn(2)+s5*bin(2))/ss+zzo
  zzi = su0*c3*(bin(1)*s3+bkn(1)*se0)/ss/s1
  go to 55
45 s6 = s2 - s1
  if (cabsz(s6) .gt. pekexp ) go to 50
  s6 = cexpz(s6)
  ss = se0*bkn(2)*s6-bin(2)*s3/s6
  zm = c6/ss
  zzo=su0*c3/s2*(s4*bkn(2)*s6+s5*bin(2)/s6)/ss+zzo
  zzi=su0*c3*(bin(1)*s3/s6+bkn(1)*se0*s6)/ss/s1
  go to 55
50 zzo=su0*c3/s2*s4/se0+zzo
  zzi=su0*c3*bkn(1)/bkn(2)/s1
55 continue
  do i=1,npc
     do j=1,npc
        zi = czero
        if(j.lt.i) go to 25
        if(dci(i)*dci(j).lt.1.e-6) go to 21
        do k=1,nbess
           k1=k+1
           ak=k
           ck=dr0(i,j)**k*cosz(ak*th0(i,j))
           c1 = cmplxz(ck, fzero)
           c9 = ak * (usp + 1.)
           c2 = cmplxz(c9, fzero)
           zi = zi + c1/ (c2 + s1 * bkn(k)/bkn(k1))
20      end do
21      c5 = cmplxz(al0(i,j), fzero)
        zp(i,j)=su0*(c3*c4*zi + c5)
        zp(j,i)=zp(i,j)
25      i1 = i + npc
        j1=j+npc
        if (ncpp(i) .eq. 1)  go to 26
        zp(i1,j)=zp(i,j)
        zp(i1, j1) = zp(i,j)
26      if (ncpp(j) .eq. 1)  go to 30
        zp(i,j1)=zp(i,j)
        i2 = i + npc2
        j2 = j + npc2
        if (ncpp(i) .eq.2)  go to 27
        zp(i2,j) = zp(i,j)
        zp(i2,j2) = zp(i,j)
        zp(i2,j1) = zp(i,j)
27      if (ncpp(j) .eq. 2)  go to 30
        zp(i,j2) = zp(i,j)
        zp(i1,j2) = zp(i,j)
30   end do
  end do
  if ( iprs47  .ge.  2 ) write (logsix, 3327)  ( zp(1,j), j=1, 3 )
3327 format ( /,  8h middle.,8x,  12hreal-zp(1,1),  8x,  12himag-zp(1,1),8x,  12hreal-zp(1,2),  8x,  12himag-zp(1,2), &
       8x,  12hreal-zp(1,3),  8x,  12himag-zp(1,3)  ,/, 8x,  6e20.11  ,/,46h diagnostic   zp(i,j)  for  (i,j)=1, ... npc .   )
  ll0 = 0
  if ( iprs47  .ge.  5 ) call print ( zp(1,1), npc, ll0, ldn )
  if ( npp  .eq.  0 )   go to 90
  s1=zzi+zzo-2.*zm
  s2=zzo-zm
  s3 = cimag1
  d1 = aimagz(s1)
  d2 = aimagz(s2)
  c1 = cmplxz(d1, fzero)
  c2 = cmplxz(d2, fzero)
  if(realz(s1) .lt. 0.) s1=s3*c1
  if(realz(s2) .lt. 0.) s2=s3*c2
  if ( iprs47  .ge.  3 ) write (logsix, 3345)  ss, zm, zzo
3345 format ( /,  6h pipe.,13x,  7hreal-ss,  13x,  7himag-ss,13x,  7hreal-zm,  13x,  7himag-zm, &
       12x,  8hreal-zzo,  12x,  8himag-zzo  ,/,  6x,  6e20.11 )
  nc1=ncc-npp
  if (iprs47 .gt. 1) write (logsix, 3354)  ncc, nc1, ixa, s1, s2
3354 format ( /,  11h more pipe.,  24h     ncc     nc1     ixa, 13x,  7hreal-s1,  13x,  7himag-s1,13x,  7hreal-s2,  13x,  7himag-s2  ,/,  11x,  3i8,  4e20.11 )
  do i=1,nc1
     zp(i,ncc) = czero
     zp(ncc,i) = czero
     zpc(i,ncc)=s2
     zpc(ncc,i)=s2
     do j=1,nc1
        if(j.lt.i) go to 65
        zpc(i,j)=s1
        zpc(j,i)=s1
65   end do
  end do
  zp(ncc,ncc) = czero
  zpc(ncc,ncc)=zzo
90 if ( iprs47  .ge.  1 ) write (logsix, 3368)  usp, pai, u2p, e2p, su0
3368 format ( /,  16h exit  'ptzy2' .,  16x,  3husp,  16x,  3hpai, 16x,  3hu2p,  16x,  3he2p,  11x,  8hreal-su0, 11x,  8himag-su0  ,/,  16x,  6e19.10 )
  return
end subroutine ptzy2
!
! subroutine bsikm.
!
subroutine bsikm (x, kn, bbin, bbkn, ikm, ixa)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  complex*16  bk0, bk1, bi0, bj1, x
  complex*16  y, y0, y1, y2, y3, y4, y5, y6, y7, y8
  complex*16  cexpz, cmplxz, clogz, csqrtz
  complex*16  bbin(kn), bbkn(kn)
  xa=cabsz(x)
  c1 = 3.75
  c2 =  2.
  y = x/cmplxz(c1, fzero)
  if ( iprs47  .ge.  5 ) write (logsix, 3426)  kn, ikm, ixa, x, y, xa
3426 format ( /,  17h begin  'bsikm' .,  24h      kn     ikm     ixa, 9x,  6hreal-x,  9x,  6himag-x,  9x,  6hreal-y,  9x,  6himag-y, &
       13x,  2hxa  ,/,  17x,  3i8,  5e15.6  )
  if (xa .gt. c1)  go to 25
  y1 = y * y
  y2=y1*y1
  y3=y2*y1
  y4=y3*y1
  y5=y4*y1
  y6=y5*y1
  bi0=1.+3.5156229*y1+3.0899424*y2+1.2067492*y3+0.2659732*y4+0.0360768 *y5+0.0045813*y6
  bj1=x*(0.5+0.87890594*y1+0.51498869*y2+0.15084934*y3+0.02658733*y4+0.00301532*y5+0.00032411*y6)
  go to 29
25 y0=csqrtz(x)
  if(ixa.ne.1) y0=y0*cexpz(-x)
  y1=1./y
  y2=y1/y
  y3=y2/y
  y4=y3/y
  y5=y4/y
  y6=y5/y
  y7=y6/y
  y8=y7/y
  bi0=(0.39894228+0.01328592*y1+0.00225319*y2-0.00157565*y3+0.00916281*y4-0.02057706*y5+0.02635537*y6-0.01647633*y7+0.00392377*y8)/y0
  bj1=(0.39894228-0.03988024*y1-0.00362018*y2+0.00163801*y3-0.01031555*y4+0.02282967*y5-0.02895312*y6+0.01787654*y7-0.00420059*y8)/y0
29 if (xa .gt. c2)  go to 35
  y = x/cmplxz(c2, fzero)
  y0=clogz(y)
  y1=y*y
  y2=y1*y1
  y3=y2*y1
  y4=y3*y1
  y5=y4*y1
  y6=y5*y1
  bk0=-y0*bi0-0.57721566+0.42278420*y1+0.23069756*y2+0.03488590*y3+0.00262698*y4+0.00010750*y5+0.00000740*y6
  bk1=y0*bj1+(1.+0.15443144*y1-0.67278579*y2-0.18156897*y3-0.01919402*y4-0.00110404*y5-0.00004686*y6)/x
  go to 40
35 y = cmplxz(c2, fzero) / x
  y0=csqrtz(x)
  if(ixa.ne.1) y0=y0*cexpz(x)
  y1=y*y
  y2=y1*y
  y3=y2*y
  y4=y3*y
  y5=y4*y
  bk0=(1.25331414-0.07832358*y+0.02189568*y1-0.01062446*y2+0.00587872*y3-0.00251540*y4+0.00053208*y5)/y0
  bk1=(1.25331414+0.23498619*y-0.03655620*y1+0.01504268*y2-0.00780353*y3+0.00325614*y4-0.00068245*y5)/y0
40 bbin(1) = bi0
  bbin(2)=bj1
  bbkn(1)=bk0
  bbkn(2)=bk1
  if ( iprs47  .ge.  5 ) write (logsix, 3452)  bi0, bj1, bk0, bk1
3452 format ( /,  9h scalars.,7x,  8hreal-bi0,  7x,  8himag-bi0,7x,  8hreal-bj1,  7x,  8himag-bj1, &
       7x,  8hreal-bk0,  7x,  8himag-bk0,7x,  8hreal-bk1,  7x,  8himag-bk1  ,/,  9x,  8e15.6  )
  if(ikm.eq.1) go to 70
  do ikn=3,kn
     bbin(ikn) = creal1
45   bbkn(ikn) = creal1
  end do
  if(ikm.eq.3) go to 60
  do ikn = 3, kn
     ik1=ikn-1
     ik2=ikn-2
55   bbin(ikn) = bbin(ik2) - 2. *ik2 / x * bbin(ik1)
  end do
  if(ikm.eq.2) go to 70
60 do ikn=3,kn
     ik1=ikn-1
     ik2=ikn-2
65   bbkn(ikn) = bbkn(ik2) + 2. * ik2 / x * bbkn(ik1)
  end do
70 if ( iprs47  .ge.  6 ) write (logsix, 3476)  ( l, bbin(l), bbkn(l), l=1, kn )
3476 format ( /,  18h exit    'bsikm' .,  5x,  3hrow, 11x,  9hreal-bbin,  11x,  9himag-bbin, 11x,  9hreal-bbkn,  11x,  9himag-bbkn,/,  (  18x,  i8,  4e20.11 )  )
  return
end subroutine bsikm
!
! subroutine olzy.
!
subroutine  olzy( w,ncpp,zy,dij,ang,usi,usr,esi,hi,di,zs,ze,zc,ldm,ldn )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  dimension  ncpp(ldm)
  dimension  zy(ldn, ldn), dij(ldm, ldm), ang(ldm,ldm), di(ldn)
  dimension  usi(ldm, 3),  usr(ldm, 3),   esi(ldm, 3),  hi(ldm)
  complex*16  zs(ldn, ldn), ze(ldn, ldn),  zc(ldn, ldn)
  complex*16  cwu, s, ss, xc, xe
  complex*16  cmplxz, csqrtz
  s = cmplxz(fzero, w)
  cwu = s * cmplxz(u2p, fzero)
  ss = csqrtz ( s )
  if ( iprs47  .ge.  1 ) write (logsix, 3508)  iearth, ncct, u2p, w, ss
3508 format ( /,  16h begin  'olzy' .,  16h  iearth    ncct, 17x, 3hu2p, 19x, 1hw, 13x,  7hreal-ss,  13x,  7himag-ss  ,/,  16x,  2i8,  4e20.11 )
  ll1 = 1
  do i=1, npc
     do j=1, npc
        if ( j  .lt.  i )   go to 20
        zs(i,j) = cwu * cmplxz(zy(i,j), fzero)
        zs(j, i) = zs(i,j)
        if ( iearth  .eq.  99 )   go to 15
        b1 = dij(i,j) * sqrtz ( u0 / roe )
        b2 = ang(i,j)
        zero = 0.0
        call zegen ( zero, b1, b2, w, xe, ll1)
        go to 18
15      d12 = absz( di(j) - di(i) )
        call zest (hi(i), hi(j), d12, roe, w, xe )
18      ze(i,j) = xe
        ze(j,i) = xe
20   end do
  end do
  if ( iprs47  .ge.  2 ) write (logsix, 3523)  ncct, npc, u0, roe
3523 format (63h diagnostic within  'olzy' .   ze(i,j)  for  (i,j)=1, ... npc.,  5x,  16h    ncct     npc,  13x,  2hu0,  12x,  3hroe  ,/,68x,  2i8,  2e15.6  )
  ll0 = 0
  if ( iprs47  .ge.  4 ) call print ( ze(1,1), npc, ll0, ldn )
  jnc = 1
  do j = 1, 2
     do i=1, ncct
        im = 4*(i-1) + 1
        j1 = im + j + 1
        j1 = ncpp(j1)
        if ( j1  .eq.  0 )   go to 40
        b1 = usi(i,j)
        b2 = esi(i,j)
        ur=usr(i,j)
        call skin47 (b1,b2,ur,s,ss,xc)
        xc = xc / j1
        j1 = im + j - 1
        j1 = ncpp(j1)
        j2 = j1 + jnc - 1
        do 35  k=jnc, j2
35         zc(k,k) = xc
        end do
        if ( iprs47  .ge.  6 ) write (logsix, 3541)  j, i, jnc, j2, j1, xc
3541    format ( /, 33h inside  (j,i)  loop of  'olzy' ., 40h       j       i     jnc      j2      j1,  13x,  7hreal-xc, 13x,  7himag-xc  ,/,  33x,  5i8,  2e20.11  )
        jnc = j2 + 1
40   end do
  end do
  if ( iprs47  .ge.  1 ) write (logsix, 3554)
3554 format ( /,  15h exit  'olzy' .  )
  return
end subroutine olzy
!
!     subroutine transp.
!
subroutine transp(yyc, ncpp, ann, jnn, znn, ldm, ldn)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  real*8 jnn
  complex*16 cmplxz
  complex*16 yyc(ldn,ldn), zss, zmm, znn(ldn)
  dimension ncpp(ldm), ann(ldn), jnn(ldn)
  if (iprs47 .ge. 1) write (logsix, 3611)  ncct, yyc(1,1), yyc(1,2)
3611 format ( /,  " enter  'transp' .,  8h    ncct", 7x,  'real-yyc(1,1)', 7x, 'imag-yyc(1,1)', 7x,  'real-yyc(1,2)', 7x, 'imag-yyc(1,2)', /,  &
       18x, i8, 4e20.11, /, ' diagnostic input matrix.   yyc(i,j)  for  (i,j)=1, ... i2 .')
  i2 = 6
  ll0 = 0
  if ( iprs47  .ge.  4 ) call print ( yyc(1,1), i2, ll0, ldn )
  i2 = 0
  do 100 k=1, ncct
     k1 = (k-1)*4 + 1
     j1 = i2 + 1
     i2 = ncpp(k1) + i2
     jnn(k) = j1
     zss = czero
     ass = 0.0
     zmm = czero
     amm = 0.0
     do i=1, ncct
        znn(i) = czero
        ann(i) = 0.0
5    end do
     do 45  i=1, i2
        do 40  j=j1, i2
           if ( i  .lt.  j1 )   go to 20
           if ( j - i )   40,  10,  15
10         zss = zss + yyc(i,j)
           ass = ass + 1.0
           go to 40
15         zmm = zmm + yyc(i,j)
           amm = amm + 1.0
           go to 40
20         if ( k  .eq.  1 )   go to 40
           dnn =  ( j - jnn(k) ) / ( jnn(k) - 1 )
           if ( dnn  .lt.  1.0 )   dnn = 0.0
           l = dnn + 1.0
           znn(l) = znn(l) + yyc(i,j)
           ann(l) = ann(l) + 1.0
40      end do
45   end do
     zss = zss/cmplxz(ass, fzero)
     zmm = zmm/ cmplxz(amm, fzero)
     k1 = k - 1
     if ( k1  .le.  0 )   go to 55
     do 50  i=1, k1
        znn(i) = znn(i) / cmplxz(ann(i), fzero)
50   end do
55   do i=1, i2
        do j=j1, i2
           if ( j  .lt.  i )   go to 90
           if ( i  .lt.  j1 )   go to 70
           if ( j - i )   90,  60,  65
60         yyc(i,j) = zss
           go to 90
65         yyc(i,j) = zmm
           go to 85
70         if ( k  .eq.  1 )   go to 85
           dnn = ( j - jnn(k) ) / ( jnn(k) - 1 )
           if ( dnn  .lt.  1.0 )   dnn = 0.0
           l = dnn + 1.0
           yyc(i,j) = znn(l)
85         yyc(j,i) = yyc(i,j)
90      end do
95   end do
100 end do
  if ( iprs47  .ge.  1 ) write (logsix, 3623)  yyc(1,1), yyc(1,2)
3623 format ( /,  " exit  'transp' .", 7x,  'real-yyc(1,1)', 7x, 'imag-yyc(1,1)', 7x,  'real-yyc(1,2)', 7x, 'imag-yyc(1,2)', &
       /, 17x, 4e20.11 , /,' diagnostic output matrix.  yyc(i,j)  for  (i,j)=1, ... i2 .')
  if (iprs47 .ge. 4) call print(yyc(1, 1), i2, ll0, ldn)
  return
end subroutine transp
!
!     subroutine skin47.
!
subroutine skin47(b1,b2,ur,cjw,sjw,zcc)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  common  / komthl /  pekexp
  complex*16  cjw, sjw, x1, x2, x3, x4, zcc
  complex*16  cexpz, cmplxz
  complex*16  c1, c2
  c1 = cmplxz(u2p, fzero)
  c2 = cmplxz(ur, fzero)
  x1 = cmplxz(b1, fzero) * sjw
  x2 = cmplxz(b2, fzero) * sjw
  ixa = 0
  xa = cabsz(x2)
  ll1 = 1
  ll2 = 2
  d15 = value1 * value5 / 100.
  if ( iprs47  .ge.  2 ) write (logsix, 3672)  b1, b2, d15, xa, xb
3672 format ( /,  18h begin  'skin47' .,  18x,  2hb1,  18x,  2hb2, 17x,  3hd15,  18x,  2hxa,  18x,  2hxb  ,/,  18x,  5e20.11  )
  if (b1 .lt. d15)   go to 30
  xb = cabsz(x1)
  ten = 10.
  if (xa .lt. ten)  go to 10
  d1 = 15./4.
  if (xb .gt. d1)  ixa = 1
10 call bsikm ( x1, ll2, bin, bkn, ll1, ixa )
  x3 = bin(2)
  x4 = bkn(2)
  call bsikm ( x2, ll2, bin, bkn, ll1, ixa )
  if (ixa .eq. 1) go to 20
  zcc = cjw * c1/x2 * (bin(1) * x4 + bkn(1) * x3) * c2
  x3 = bin(2) * x4  -  bkn(2) * x3
  go to 25
20 x1=x2-x1
!!!!  d2 = 50.
  if (cabsz(x1) .gt. pekexp )  go to 33
  x1=cexpz(x1)
  zcc= cjw * c1  / x2  *  ( bin(1)*x4*x1 + bkn(1)*x3/x1 ) * c2
  x3 = bin(2) * x4 * x1  -  bkn(2) * x3 / x1
25 zcc = zcc/x3
  go to 40
30 call bsikm ( x2, ll2, bin, bkn, ll1, ll1 )
33 zcc= cjw * c1  / x2  *  bin(1) / bin(2)  *  c2
40 if ( iprs47  .ge.  2 ) write (logsix, 3688)  ur, cjw, zcc
3688 format ( /,  17h exit  'skin47' .,  18x,  2hur, 12x,  8hreal-cjw,  12x,  8himag-cjw, 12x,  8hreal-zcc,  12x,  8himag-zcc  ,/,  17x,  5e20.11  )
  return
end subroutine skin47
!
! subroutine zegen.
!
subroutine zegen(be1,be2,th,w,xe,isyst)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  common  / komthl /  pekexp
  complex*16  cj, cjw, x1, x2, xe
  complex*16  cexpz, cmplxz, csqrtz
  complex*16  c1
  unity = 1.0
  c1 = cmplxz(u2p, fzero)
  cj = cimag1
  euc = 2./value4
  d1 = 2.0
  sq2 = sqrtz(d1)
  ll1 = 1
  ll2 = 2
  if (isyst .ge. 0)   go to 140
  cjw = cj * cmplxz(w, fzero)
  xe=csqrtz(cjw)
  x1 = cmplxz(be1, fzero) * xe
  x2 = cmplxz(be2, fzero) * xe
  ixa=0
  xa=cabsz(x2)
  if(xa.gt.10.) ixa=1
  call bsikm(x1,ll2,bin,bkn,ll1,ixa)
  xe=bkn(1)
  if(xa.gt.100.) go to 130
  call bsikm(x2,ll2,bin,bkn,ll1,ixa)
  if (ixa .gt. 0)   go to 120
110 xe = cjw * c1 * (xe - bkn(1))
  go to 5
120 xe=cjw*c1 *(xe-bkn(1) /cexpz(x2-x1))/cexpz(x1)
  go to 5
130 xa1=cabsz(x1)
  if(xa1 .gt. pekexp ) go to 140
  xe = cjw * c1 * xe/cexpz(x1)
  go to 5
140 xe = czero
5 e = be2 * sqrtz ( w )
  if ( iprs47  .ge.  2 )
1 write (logsix, 3741)  isyst, ixa, be1, be2, xa, th, xa1, xe, w,e
3741 format ( /,  18h within  'zegen' .,  16h   isyst     ixa, 16x,  3hbe1,  16x,  3hbe2,  17x,  2hxa,  17x,  2hth, &
       16x,  3hxa1  ,/,  18x,  2i8,  5e19.10  ,/,  1x, 13x,  7hreal-xe,  13x,  7himag-xe, &
       19x, 1hw, 19x, 1he,/, 1x, 4e20.11 )
  if ( e  .gt.  5.)   go to 60
  r2 = e ** 4
  r1=r2/16.
  sn = e**2/8.
  bn=r1/12.
  cn = e/3.
  dn=5./4.
  en = e**3/45.
  fn=5./3.
  iter=11
  if(e.ge.1. ) iter=21
  iter = 21
15 do i=1,iter
     t=i-1
     t1=t*2.
     t2=t*4.
     cs1=cosz((t2+2.)*th)
     ss1=sinz((t2+2.)*th)
     cs2=cosz((t2+4.)*th)
     ss2=sinz((t2+4.)*th)
     cs3=cosz((t2+1.)*th)
     cs4=cosz((t2+3.)*th)
     if (i.gt. 1)   go to 30
     a1 = sn * cs1
     a2=sn*ss1
     a3=bn*cs2
     a4=bn*ss2
     b1=cn*cs3
     b2=dn*a1
     b3=en*cs4
     b4=fn*a3
     evennn = a1 + a2 + a3 + a4 + b1 + b2 + b3 + b4
     go to 50
30   t3=-t1*(t1+1.)**2*(t1+2.)
     t4=-(t1+1.)*(t1+2.)**2*(t1+3.)
     t5=-(t2-1.)*(t2+1.)**2*(t2+3.)
     t6=1./t2+1./(t1+1.)+1./(t1+2.)-1./(t2+4.)
     t7=-(t2+1.)*(t2+3.)**2*(t2+5.)
     t8=1./(t2+2.)+1./(t1+2.)+1./(t1+3.)-1./(t2+6.)
     sn=sn*r1/t3
     bn=bn*r1/t4
     cn=cn*r2/t5
     dn=dn+t6
     en=en*r2/t7
     fn=fn+t8
     a1=a1+sn*cs1
     a2=a2+sn*ss1
     a3=a3+bn*cs2
     a4=a4+bn*ss2
     b1=b1+cn*cs3
     b2=b2+dn*sn*cs1
     b3=b3+en*cs4
     b4=b4+fn*bn*cs2
     verbin = evennn
     evennn = a1 + a2 + a3 + a4 + b1 + b2 + b3 + b4
     if ((1.-verbin/evennn)*(1.-verbin/evennn) .lt. 1.e-12) go to 888
50 end do
888 continue
  p1=pai*(1.-a3)/4.+a1*alogz(euc/e) + th*a2 + b2 + sq2*(b3-b1)
  q1=0.5+(1.-a3)*alogz(euc/e)-th*a4-pai*a1/4.-b4+sq2*(b1+b3)
  go to 70
60 cs1=sq2*cosz(th)
  cs2=cosz(2.*th)*2.
  cs3=sq2*cosz(3.*th)
  cs4=3.*sq2*cosz(5.*th)
  p1=(cs1+(cs4/e**3+cs3/e-cs2)/e)/e
  q1=(cs1+(cs4/e**2-cs3)/e**2)/e
70 xe = cmplxz(w,fzero) * c1 * (cmplxz(p1,fzero) + cj * cmplxz(q1, fzero)) + xe
  if ( iprs47  .ge.  2 ) write (logsix, 3782)  p1, q1, xe
3782 format ( /,  16h exit  'zegen' ., 17x, 2hp1, 17x, 2hq1, 12x,  7hreal-xe,  12x,  7himag-xe  ,/,  16x,  4e19.10  )
  return
end subroutine zegen
subroutine eigen ( cjw, p, n, a, ai, qn, q, xx, yy, ldn )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include  'labl47.ftn'
  complex*16  ad, cjw, qi, s, sa
  complex*16  cmplxz, csqrtz
  complex*16  q(ldn, ldn), xx(ldn, ldn), yy(ldn, ldn)
  complex*16  p(ldn, ldn), a(ldn, ldn), ai(ldn, ldn), qn(ldn)
  complex*16  c1, c2
  !     eigenvalue calculation subroutine.   'kvalue'  =  iteration limit.
  kvalue = 20
  c1 = cjw/cmplxz(spdlgt, fzero)
  c2 = cmplxz(unity, fzero)
  qi = c2 / c1
  qi = qi * qi
  if ( iprs47  .ge.  1 ) write (logsix, 3806)  n, cjw, qi
3806 format ( /,  17h enter  'eigen' .,  8h       n,12x, 8hreal-cjw,  12x,  8himag-cjw,13x, 7hreal-qi,   13x,  7himag-qi  ,/, &
       17x, i8,  4e20.11  ,/,  1x  )
  do i=1,n
     do j=1,n
4       p(i,j)=p(i,j)*qi
     end do
5    p(i, i) = p(i, i) - c2
  end do
  do i=1,n
     do j=1,n
10      q(i,j)=p(i,j)
     end do
  end do
  l=0
15 l=l+1
  iq=0
  if(l-n) 20,90,90
20 iq=iq+1
  if(iq.le. kvalue ) go to 25
  iq=iq-1
  write(lunit6,902) iq
902 format(5x, "Warning ; a higher accuracy can't be, achieved by this computer.", /, &
       5x, 'eigen values   1 vectors at itteration iq= ', i3, ' is adopted.    ')
  go to 63
25 do i=1,n
     do j=1,n
        xx(i,j) = q(i,j)
30      yy(i,j) = q(i,j)
     end do
  end do
  do i=1,n
     do j=1,n
        q(i,j)=czero
        do k=1,n
35         q(i,j) = q(i,j) + xx(i,k) * yy(k,j)
        end do
     end do
  end do
  dm=0.
  do i=1,n
     dx=cabsz(q(i,i))
     if(dx.le.dm) go to 40
     dm=dx
     im=i
40 end do
  qi=1./q(im,im)
  do i=1,n
     do j=1,n
45      q(i,j)=q(i,j)*qi
     end do
  end do
  dm=0.
  do i=1,n
     if(i.eq.im) go to 50
     dx=cabsz(q(i,im))
     if(dx.le.dm) go to 50
     dm=dx
     i1=i
     i2=im
50 end do
  if (dm .ne. 0.)   go to 60
  do i=1, n
     if(i.eq.im) go to 59
     dx=cabsz(q(im,i))
     if(dx.le.dm) go to 59
     dm=dx
     i1=im
     i2=i
59 end do
60 sa = q(im, im) / q(i1, i2)
  s = xx(im, im) / xx(i1, i2)
  r = cabsz(s / sa)
  d1 = 50.* epsiln
  !     non-64-bit complex math redefines tolerance in "sysdep":
  if ( znvref .ne. 0.0 )  d1 = 50. * znvref
  d2 = r - unity
  if ( iprs47  .ge.  31 ) write (logsix, 3827)  l, iq, kvalue, d2, d1
3827 format (  42h done another iteration.   l, iq, kvalue =, 3i8,  10x,  8hd2, d1 =,  2e15.6  )
  if (absz(d2) .gt. d1)   go to 20
63 do i=1,n
     a(i,l)=q(i,im)
65   ai(l,i)=q(im,i)
  end do
  ad=czero
  do 70 i=1,n
70   ad=ad+p(im,i)*q(i,im)
  end do
  qn(l)=ad/q(im,im)
  ad=czero
  do i=1,n
75   ad=ad+ai(l,i)*a(i,l)
  end do
  ad=1./ad
  do i=1,n
80   ai(l,i)=ad*ai(l,i)
  end do
  do i=1,n
     do j=1,n
        q(i,j)=p(i,j)-a(i,l)*qn(l)*ai(l,j)
85      p(i,j)=q(i,j)
     end do
  end do
  if ( iprs47  .ge.  7 ) write (logsix, 3842)  l, qn(l), ad
3842 format (  21h eigenvalue finished.,  8h       l, 10x,  10hreal-qn(l),  10x,  10himag-qn(l), 13x,  7hreal-ad,  13x,  7himag-ad  ,/, &
       21x,  i8,  4e20.11  ,/,  1x  )
  go to 15
90 dm=0.
  do i=1,n
     dx=cabsz(p(i,i))
     if(dx.le.dm) go to 95
     dm=dx
     im=i
95 end do
  ad=czero
  do i=1,n
     ad=ad+p(im,i)*p(i,im)
98   a(i,l)=p(i,im)/p(im,im)
  end do
  qn(l)=ad/p(im,im)
  do i=1,n
100  ai(l,i)=p(im,i)/qn(l)
  end do
  do i=1,n
110  qn(i) = c1 * csqrtz(c2 + qn(i))
  end do
9200 if ( iprs47  .ge.  3 ) write (logsix, 3854)  dm, spdlgt, p(im,im), ( qn(i), i=1, n )
3854 format ( /, 35h done all eigenvalues in  'eigen' ., 18x,  2hdm,  14x,  6hspdlgt,  7x,  13hreal-p(im,im), &
       7x,  13himag-p(im,im)  ,/,  35x,  4e20.11,   /, 49h complex eigenvalues  (qn(i), i=1, n)  follow ...  ,/, &
       ( 1x,  6e20.11 )  )
  if ( iprs47  .ge.  1 ) write (logsix, 3867)
3867 format ( /,  88h diagnostic upon exit  'eigen' .   matrix of eigenvectors  a(i,l)  for  (i,l)=1, ... n .    )
  ll0 = 0
  if ( iprs47  .ge.  6 ) call print ( a(1,1), n, ll0, ldn )
  return
end subroutine eigen
!
! subroutine zest.
!
subroutine  zest ( h1, h2, e, res, omg, s )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  complex*16  qq, bbb, rom, s, sa, s1, s2
  complex*16  s3, s5, s6, s8, sp12, sp23, sm12
  complex*16  sm23, sq1, sq3, u, u1, u2, z
  complex*16  cexpz, cmplxz, csqrtz
  complex*16  c1, c2, c3, c4, c5, c6, c7, c8
  toj = u0
  if ( iprs47  .ge.  2 ) write (logsix, 3917)  h1, h2, e, res, omg
3917 format ( /,  16h enter  'zest' .,  18x,  2hh1,  18x,  2hh2, 19 x,  1he,  17x,  3hres,  17x,  3homg  ,/,  16x,  5e20.11  )
  hkr = spdlgt**2
  yud=1./toj/hkr
  h1ph2=h1+h2
  ab = 10. * e/h1ph2
  omg2=omg*omg
  d1 = toj * omg/res
  rom = cmplxz(fzero, d1)
  bp=pai/2.
  rmax=0.
  s = czero
  l=2
  ram=0.
700 ram2=ram*ram
  d2 = ram2 + omg2 * (1. - htoj2 * hyud2)/hkr
  c1 = cmplxz(d2, fzero)
  c2 = rom * cmplxz(htoj2, fzero)
  s1 = csqrtz(c1 + c2)
  d3 = ram2 + omg2 * (1. - htoj3 * hyud3)/hkr
  c3 = cmplxz(d3, fzero)
  c4 = cmplxz(alf1, fzero) * rom * cmplxz(htoj3, fzero)
  s2 = csqrtz(c3 + c4)
  d4 = ram2 + omg2 * (1. - htoj4 * hyud4)/hkr
  c5 = cmplxz(d4, fzero)
  c6 = cmplxz(alf2, fzero) * rom * cmplxz(htoj4, fzero)
  s3 = csqrtz(c5 + c6)
  s6=s2
  s5=s1
  d5 = 1./htoj2/toj
  s1 = s1 * cmplxz(d5, fzero)
  d6 = 1./htoj3/toj
  s2 = s2 * cmplxz(d6, fzero)
  d7 = 1./htoj4/toj
  s3 = s3 * cmplxz(d7, fzero)
  s8 = cmplxz(toj, fzero) * s1
  sp12=s1+s2
  sp23=s2+s3
  sm12=s1-s2
  d8 = 2.*( dep1 - dep2)
  c7 = cmplxz(d8,fzero)
  sm23 = (s2 - s3) * cexpz(s6 * c7)
  d9 = -2. * dep1
  c8 = cmplxz(d9, fzero)
  bbb = cexpz(s5 * c8)
  d10 = expz(-h1ph2 * ram) * cosz(e * ram)
  contwo = 2.0
  z=cmplxz(fzero,contwo )*(sp12*sp23+sm12*sm23+bbb*(sm12*sp23+sp12 * sm23)) * cmplxz(d10, fzero)/((cmplxz(ram, fzero) +s8) * &
       (sp12*sp23+sm12* sm23) + (cmplxz(ram,fzero) - s8) * bbb*(sm12*sp23+sp12*sm23))
  if ( iprs47  .ge.  5 ) write (logsix, 3928)  l, bp, ab, ram, rmax, z
3928 format ( /,  1x,  8h       l,  18x,  2hbp,  18x,  2hab, 17x,  3hram,  16x,  4hrmax,  14x,  6hreal-z,  14x,  6himag-z,/,  1x,  i8,  6e20.11  )
  go to (702,701),l
701 u1=z
  jd=50
  go to 650
600 u1 = czero
650 if (bp .lt. ab)   go to 300
  jd = 100
  rmin=rmax
  rmax=10./h1ph2
  l=1
  ram=rmax
  go to 700
702 u2=z
  go to 210
300 rmin=rmax
  rmax=bp/e
  bp=bp+pai
  u2 = czero
210 jjj=1
  qq=u1+u2
  n=123
  dx=rmax-rmin
  d11 = dx/2.
  sq1 = qq * cmplxz(d11, fzero)
  if ( iprs47  .ge.  6 ) write (logsix, 3943)  dx, htoj2, rmin, omg2, toj, value5, qq
3943 format ( /,  1x,  14x,  2hdx,  11x,  5hhtoj2,  12x,  4hrmin, 12x,  4homg2,  13x,  3htoj,  10x,  6hvalue5,  9x,  7hreal-qq, 9x,  7himag-qq  ,/,  1x,  8e16.7  )
75 dx2=dx
  dx=dx/2.0
  ram=rmin+dx
  n=2*n
  if(jjj.eq.1) n=1
  jjj=100
  do kn=1,n
     ram2=ram*ram
     d2 = ram2 + omg2 * (1. - htoj2 * hyud2)/hkr
     c1 = cmplxz(d2, fzero)
     c2 = rom * cmplxz(htoj2, fzero)
     s1 = csqrtz(c1 + c2)
     d3 = ram2 + omg2 * (1. - htoj3 * hyud3)/hkr
     c3 = cmplxz(d3, fzero)
     c4 = cmplxz(alf1, fzero) * rom * cmplxz(htoj3, fzero)
     s2 = csqrtz(c3 + c4)
     d4 = ram2 + omg2 * (1. - htoj4 * hyud4)/hkr
     c5 = cmplxz(d4, fzero)
     c6 = cmplxz(alf2, fzero) * rom * cmplxz(htoj4, fzero)
     s3 = csqrtz(c5 + c6)
     s6=s2
     s5=s1
     d5 = 1./htoj2/toj
     s1 = s1 * cmplxz(d5, fzero)
     d6 = 1./htoj3/toj
     s2 = s2 * cmplxz(d6, fzero)
     d7 = 1./htoj4/toj
     s3 = s3 * cmplxz(d7, fzero)
     s8 = cmplxz(toj, fzero) * s1
     sp12=s1+s2
     sp23=s2+s3
     sm12=s1-s2
     d8 = 2.*( dep1 - dep2)
     c7 = cmplxz(d8,fzero)
     sm23 = (s2 - s3) * cexpz(s6 * c7)
     d9 = -2. * dep1
     c8 = cmplxz(d9, fzero)
     bbb = cexpz(s5 * c8)
     d10 = expz(-h1ph2 * ram) * cosz(e * ram)
     z=cmplxz(fzero,contwo)*(sp12*sp23+sm12*sm23+bbb*(sm12*sp23+sp12*sm23))*cmplxz(d10, fzero)/((cmplxz(ram, fzero) +s8)*&
          (sp12*sp23+sm12*sm23)+(cmplxz(ram,fzero) - s8) * bbb*(sm12*sp23+sp12*sm23))
     ram=ram+dx2
     u=2.*z
77   qq=qq+u
  end do
  d12 = dx/2.
  sq3 = qq * cmplxz(d12, fzero)
  sa = sq1 - sq3
  r = cabsz(sa)/cabsz(sq3)
  v5 = value5 * .5
  if ( iprs47  .ge.  24 ) write (logsix, 3956)  n, r, dx, ram, qq
3956 format ( /,  13h bottom loop.,  8h       n,  19x,  1hr, 18x,  2hdx,  17x,  3hram,  13x,  7hreal-qq,  13x,  7himag-qq,/,  13x,  i8,  5e20.11  )
  if ( r .le. v5)   go to 50
  sq1 = sq3
  go to 75
50 s = s + sq3
  if(jd.eq.100) go to 55
  go to 600
55 c9 = .5 * u0/ pai
  s = s * cmplxz(omg, fzero) * cmplxz(c9, fzero)
  if ( iprs47  .ge.  2 ) write (logsix, 3968)  omg, value2, s
3968 format ( /,  15h exit  'zest' .,  17x,  3homg,  14x,  6hvalue2, 14x,  6hreal-s,  14x,  6himag-s  ,/,  15x,  4e20.11  )
  return
end subroutine zest
!
! subroutine minv.
!
subroutine minv ( tcmpx, m, f, ldn, ldn2 )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  complex*16  ad, cc, d
  include  'labl47.ftn'
  complex*16  tcmpx(ldn,ldn), f(ldn, ldn2),  d2,  cmplxz, fnew
  complex*16  fident
  dimension  fr(10,20), fi(10,20), fnew(10,20), fident(10,20)
  d1 = 0.0
  do i=1,m
     do j=1,m
        d2 = tcmpx(i,j)
        if ( cabsz ( d2 ) .gt. d1)   d1 = cabsz(d2)
        fr(i,j) = realz ( d2 )
4       fi(i,j) = aimagz ( d2 )
     end do
  end do
  !!!!    4 f(i,j) = d2
  d1 = d1 * value2
  if ( iprs47  .ge.  1 ) write(logsix, 4005) m, value2, d1, f(1, 1)
4005 format ( /,  16h start  'minv' .,  8h       m,  14x,  6hvalue6, 18x,  2hd1,  9x,  11hreal-f(1,1),  9x,  11himag-f(1,1)  ,/, &
          16x,  i8,  4e20.11  ,/, 47h diagnostic   tcmpx(i,j)  for  (i,j)=1, ... m .    )
  ll0 = 0
  if ( iprs47  .ge.  7 ) call print ( tcmpx(1,1), m, ll0, ldn )
  do i=1, m
     j1=m+1
     m1=m*2
     do j = j1, m1
        if ((j-m) .eq. i)   go to 20
        ccc!     f(i,j) = czero
        fr(i,j) = 0.0
        fi(i,j) = 0.0
        go to 25
!!!!  20 f(i,j) = creal1
20      fr(i,j) = 1.0
        fi(i,j) = 0.0
25   end do
30 end do
  do 115  k1 =1, m
     d9 = 0.0
     n=k1
     do i2 = k1, m
        !!!!     bx=cabsz(f(i2,k1))
        !!!!     if (bx .le. d9)   go to 50
        d18 = absz ( fr(i2,k1) )
        d19 = absz ( fi(i2,k1) )
        if ( d18 .lt. d19 ) d18 = d19
        if ( d18 .le. d9 )  go to 50
        d9 = d18
        n=i2
50   end do
     !      write (*,*) ' next variable.  k1, n, d9, d1 =',
     !     1                              k1, n, d9, d1
     if ( d9 .gt. d1 ) go to 60
     write (lunit6, 5706)  m, k1, d9
5706 format ( /, 106h stop. ---- matrix inversion within subroutine  'minv'  has been suspended, due to failure to find a large     ,/, &
          12x,  58henough pivot element.   the matrix in question is of order,  i5,  36h ,    with breakdown having occurred     ,/, &
          12x,  51hwhile working on the elimination of variable number,  i5,   '.   in this columns, the largest real or'       ,/, &
          12x,  ' imaginary part had absolute value', e14.3,  45h ,   while the applicable near-zero tolerance     )
     write(lunit6, 5707) d1, value2
5707 format (12x,  11his equal to,   e14.3,   36h .    this latter number is equal to,   e14.3,   20h   times the largest           ,/, &
          12x,  67helement of the original input matrix (considering absolute values).     )
     stop
60   if (n .eq. k1)   go to 75
     do j = k1, m1
        !!!!     cc=f(k1,j)
        ccr = fr(k1,j)
        cci = fi(k1,j)
        !!!!     f(k1,j)=f(n,j)
        fr(k1,j) = fr(n,j)
        fi(k1,j) = fi(n,j)
!!!!   70 f(n,j)=cc
        fr(n,j) = ccr
70      fi(n,j) = cci
     end do
75   do i=1,m
        if (i.eq. k1)   go to 100
!!!!     d = f(i,k1)
        dr = fr(i,k1)
        di = fi(i,k1)
        do j=k1, m1
!!!!  f(i,j) = f(i,j) - d/f(k1,k1) * f(k1,j)
           d14 = fr(k1,k1)**2 + fi(k1,k1)**2
           d22 = dr * fr(k1,k1) + di * fi(k1,k1)
           d23 = di * fr(k1,k1) - dr * fi(k1,k1)
           d5 = d22 * fr(k1,j) - d23 * fi(k1,j)
           d6 = d22 * fi(k1,j) + d23 * fr(k1,j)
           fr(i,j) = fr(i,j) - d5 / d14
           fi(i,j) = fi(i,j) - d6 / d14
           !      write (*,*) ' revise f(i,j).  i, j, k1, f(i,j) =',
           !     1                              i, j, k1, fr(i,j), fi(i,j)
95      end do
100  end do
!!!!     ad = f(k1, k1)
     adr = fr(k1,k1)
     adi = fi(k1,k1)
     d14 = adr**2 + adi**2
     adr = adr / d14
     adi = -adi / d14
     do 110  j=k1, m1
        cccc 110 f(k1,j) = f(k1,j)/ad
        d16  =     fr(k1,j) * adr - fi(k1,j) * adi
        fi(k1,j) = fr(k1,j) * adi + fi(k1,j) * adr
110     fr(k1,j) = d16
     end do
     !  110 write (*,*) ' new  f(k1,j).   k1, j, fr(k1,j), fi(k1,j) =',
     !     1                              k1, j, fr(k1,j), fi(k1,j)
115 end do
  do i = 1, m
     do j=1,m
        j1=j+m
325     fnew(i,j) = cmplxz ( fr(i,j1), fi(i,j1) )
     end do
  end do
  do i = 1, m
     do j=1, m
        fident(i,j) = czero
        do k=1, m
288        fident(i,j) = fident(i,j) + tcmpx(i,k) * fnew(k,j)
        end do
625  end do
  end do
  !      write (*,*) ' minv, [a]*[a]-1  follows.   m =',  m
  !      do 725  i = 1, m
  !  725 write (*,*) ' row', i, ( fident(i,j), j=1, m )
  do i = 1, m
     do j=1,m
        j1=j+m
!!!! 125 tcmpx(i,j) = f(i,j1)
125     tcmpx(i,j) = cmplxz ( fr(i,j1), fi(i,j1) )
     end do
  end do
  !  125 write (*,*) ' transfer.',
  !     1  ' i, j, j1, fr(i,j1), fi(i,j1), tcmpx(i,j) =',
  !     2    i, j, j1, fr(i,j1), fi(i,j1), tcmpx(i,j)
  if ( iprs47  .ge.  1 ) write (logsix, 4027)  tcmpx(1,1), tcmpx(1,2)
4027 format ( /,  24h exit  'minv'  normally., 5x,15hreal-tcmpx(1,1),  5x,  15himag-tcmpx(1,1),5x,,15hreal-tcmpx(1,2),  5x,  15himag-tcmpx(1,2)  ,/, &
          24x,  4e20.11  ,/,55h diagnostic inverse.   tcmpx(i,j)  for  (i,j)=1, ... m.  )
  if ( iprs47  .ge.  7 ) call print ( tcmpx(1,1), m, ll0, ldn )
150 return
end subroutine minv
!
! subroutine mxm.
!
subroutine mxm(xm,yym,c,n,ldn)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'labl47.ftn'
  complex*16  xm(ldn,ldn), yym(ldn,ldn), c(ldn,ldn)
  if ( iprs47  .ge.  1 ) write (logsix, 4062)  n, xm(1,1), yym(1,1)
4062 format ( /,  15h enter  'mxm' .,  8h       n, 7x,  13hreal- xm(1,1),  7x,  13himag- xm(1,1), 7x,  13hreal-yym(1,1),  7x,  13himag-yym(1,1)  ,/, &
          15x,  i8,  4e20.11  ,/, 57h diagnostic left factor.   xm(i,j)  for  (i,j)=1, ... n .  )
  if ( iprs47  .lt.  8 )   go to 4079
  ll0 = 0
  call print ( xm(1,1), n, ll0, ldn )
  write (logsix, 4073)
4073 format ( /, 58h diagnostic right factor.  yym(i,j)  for  (i,j)=1, ... n . )
  call print ( yym(1,1), n, ll0, ldn )
4079 do i=1, n
     do j=1,n
        c(i,j)=czero
        do k=1,n
10         c(i,j) = c(i,j) + xm(i,k) * yym(k,j)
        end do
     end do
  end do
  if ( iprs47  .ge.  1 ) write (logsix, 4091)  c(1,1), c(1,2)
4091 format ( /,  14h exit  'mxm' ., 9x,  11hreal-c(1,1),  9x,  11himag-c(1,1), 9x,  11hreal-c(1,2),  9x,  11himag-c(1,2)  ,/, &
          14x,  4e20.11  ,/, 52h diagnostic product.   c(i,j)  for  (i,j)=1, ... n .   )
  if ( iprs47  .ge.  8 ) call print ( c(1,1), n, ll0, ldn )
  return
end subroutine mxm
!
! subroutine print.
!
subroutine print(c,n,iform,ldn)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labl47.ftn'
  complex*16  c(ldn,ldn)
  dimension  workr(8), worki(8)
  real*8          text1, text2, text3
  data  text1   /  3hrow  /
  data  text2   /  3h     /
  nline = ( n + 7 ) / 8
  do i=1, n
     text3 = text2
     im = -7
     do k=1, nline
        im = im + 8
        in = im + 7
        if ( in  .gt.  n )   in = n
        l = 0
        do j=im, in
           l = l + 1
           workr(l) = realz( c(i,j) )
4617       worki(l) = aimagz( c(i,j) )
        end do
        if ( k  .eq.  nline ) text3 = text1
        if ( iform  .eq.  1 )   go to 4658
        write (lunit6, 4649)  text3,  ( workr(m), m=1, l )
4649    format ( 1x, a3, 8e16.7  )
        write (lunit6, 4651)  ( worki(m), m=1, l )
4651    format ( 4x, 8e16.7 )
        go to 4681
4658    write (lunit6, 4659)  text3,  ( workr(m), m=1, l )
4659    format ( 1x, a3, 8f16.7 )
        write (lunit6, 4661)  ( worki(m), m=1, l )
4661    format ( 4x, 8f16.7  )
4681 end do
4695 write (lunit6, 4632)  i
  end do
4632 format ( 1h+, i2, /, 1x )
  return
end subroutine print
!
!     end of file: over47.for
!
