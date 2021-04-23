!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over10.for
!
!
!     subroutine over10.
!
subroutine over10
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'space2.ftn'
  dimension itemp(1)
  equivalence (itemp(1), voltk(1))
  character*8 fxtem1
  locatn(i, j) = (j * j - j) / 2 + i
  if (iprsup .ge. 1) write (lunit6, 2941)  ntot, ioffd, loopss(2)
2941 format ( 44h top of "over10".   ntot, ioffd, loopss(2) =, 2i8 )
  n14 = ntot + loopss(2)
  call mover0 ( solr(1), n14 )
  ! 3456789012345678901234567890123456789012345678901234567890123456789012
  call mover0 ( soli(1), n14 )
  do i = 1, ntot
     ia = index(i)
     go to 3003
3002 ia = ia +1
3003 if (ia .eq. index(i+1)) go to 3009
     ib = iloc(ia)
     ic = ia
3005 ic = ic +1
     if (ic .eq. index(i+1)) go to 3002
     if (ib .le. iloc(ic)) go to 3005
     iloc(ia) = iloc(ic)
     iloc(ic) = ib
     ib = iloc(ia)
     go to 3005
3009 end do
  n1 = 1 + iofgnd
  n2 = 1 + iofbnd
  call mover0 ( gnd(n1), ioffd)
  call mover0 ( bnd(n2),ioffd)
  call mover0 ( diag(1), ntot )
  call mover0 ( diab(1), ntot )
  ia = ntot
  if ( ia  .lt.  ibr )   ia = ibr
  if ( ia  .lt.  it )   ia = it
  if ( iprsup .ge. 3 ) write (lunit6,1139) ( i,norder(i), index(i), iloc(i),kbus(i), kode(i),  mbus(i), nr(i), kodebr(i), length(i), &
       tr(i), tx(i), emtpc(i), i=1, ia )
1139 format ( //,  33h arrays at beginning of  over10 .   ,/, 5x, 3hrow, 2x, 6hnorder, 3x, 5hindex, 4x, 4hiloc, &
          4x, 4hkbus, 4x, 4hkode, 4x, 4hmbus, 6x, 2hnr, 2x, 6hkodebr, 2x, 6hlength, 13x, 2htr, 13x, 2htx, 14x, 1hc, &
          /, ( 10i8, 3e15.4  ) )
  !     initialize counters for the -666 branches  * * * * * * * * * * * *
  ikf = 0
  isfd = 0
  i = 1
  !
  !                        beginning of bulding [y] from very first branch
  !
3080 ic = iabs ( kbus(i) )
  xopt = xoptbr(i)
  copt = coptbr(i)
  d3 = tenm3
  d4 = unity / 1000000.
  if ( xopt .gt. 0.0 )  d3 = 1.0 / ( twopi * xopt )
  if ( copt .gt. 0.0 )  d4 = d4 / ( twopi * copt )
  omltem =  d3 * twopi
  omctem =  d4 * twopi
  if ( ic  .eq.  1 )  ic = iabs( mbus(i) )
  n16 = iabs ( kssfrq(ic) )
  omegal = omltem * sfreq(n16)
  omegac = omctem * sfreq(n16)
  omega = twopi * sfreq(n16)
  if ( iprsup  .ge.  3 ) write (lunit6, 3087)  i, ic, n16, omega, omegal, omegac
3087 format ( 42h next branch.   i, ic, n16, omega, omegal,, 9h omegac =,  3i6,  3e15.6  )
  if ( iprsup .ge. 1 ) write (lunit6,*) ' bnd(1) =', bnd(1)
  if ( kodsem(i) .eq. 0 .and. imodel(i) .eq. 0 ) go to 5698
  if ( imodel(i) .eq. -4 )  omegac =1.0
  if (kodebr(i) .eq. -2 .and. kodsem(i) .eq. 0) go to 43400
  if (kodebr(i) .eq. -2 .and. imodel(i) .eq. -2) go to 43400
5698 if (iabs(length(i)).gt.1) go to 3300
  ic = iabs(kbus(i))
  ic = norder(ic)
  ig = iabs(mbus(i))
  ig = norder(ig)
  j = iabs(nr(i))
  rr = tr(j)
  if ( ic .gt. ig )  go to 3130
  if ( ic .lt. ig )  go to 3120
  if ( kbus(i)  .eq.  1 ) go to 3283
  lstat(19) = 3112
  lstat(15) = i
  n1 = iabs( kbus(i) )
  bus1 = bus(n1)
3112 kill = 17
  go to 9999
3120 in = ig
  im = ic
  go to 3140
3130 in = ic
  im = ig
3140 ia = index(im)
  if (nr(i) .ge. 0) go to 3220
  if (emtpc(j) .eq. 0.0)   go to 3164
  xx = tx(j)*omegal -1.0/(emtpc(j)*omegac)
  go to 3168
3164 xx = tx(j)*omegal
3168 dd = rr*rr +xx*xx
  gg = rr/dd
  bb = -xx/dd
  go to 3180
3170 ia = ia + 1
3180 if (ia .lt. index(im + 1)) go to 3200
  lstat(19) = 3180
  go to 3112
3200 if (iloc(ia) .ne. in) go to 3170
  isubs1 =  iofgnd+ia
  gnd(isubs1)    = gnd(isubs1)    - gg
  isubs1 =  iofbnd+ia
  bnd(isubs1)    = bnd(isubs1)    - bb
  go to 3280
3220 xx = tx(j)*omegal
  gg = rr
  bb = xx
  if ( imodel(i) .eq. -4 ) go to 3240
  dd = rr*rr + xx*xx
  gg = rr/dd
  bb = -xx/dd
  go to 3240
3230 ia = ia + 1
3240 if (ia .lt. index(im + 1)) go to 3260
  lstat(19) = 3240
  go to 3112
3260 if (iloc(ia).ne.in) go to 3230
  isubs1 =  iofgnd+ia
  gnd(isubs1)    = gnd(isubs1)    - gg
  isubs1 =  iofbnd+ia
  bnd(isubs1)    = bnd(isubs1)    - bb
  bb = bb + emtpc(j)*omegac
  if (kbus(i) .lt. 0) gg = gg +r(j)
3280 diag(im) = diag(im) + gg
  diab(im) = diab(im) + bb
  diag(in) = diag(in) + gg
  diab(in) = diab(in) + bb
3283 i = i + 1
  go to 3490
3300 is = iabs(nr(i))
  nt = iabs(length(i))
  if (length(i) .lt. 0) go to 3320
  jt = nt*(nt + 1)/2
  if (kodebr(i) .le. 0 ) go to 3515
  if ( kodsem(i) .ne. 0  .and. imodel(i) .ne. -2) go to 3515
  if ( jt .le. lbus )  go to 3504
  kill = 51
  k = iabs( kbus(i) )
  m = iabs( mbus(i) )
  lstat(19) = 3504
  bus1 = bus(k)
  bus2 = bus(m)
  lstat(14) = nt
  go to 9999
  !        for case of [a], [b], 1st form unsymmetric  [q] = -[b] + jw[u]:
3504 call mover( tr(is), emtpf(1), jt )
  n = is + jt - 1
  do j = is, n
     tr(j) = -tx(j)
  end do
  call mover0 ( tx(is), jt )
  call addmxd( tx(is), omega, tx(is), nt )
  if ( iprsup .ge. 2 ) write (lunit6, 3505)  (  tr(j),  tx(j), j=is, n )
3505 format( /,  39h at 3505.   cmr +j cmi before inversion  ,/,( 1x, 8e16.6 ) )
3515 l = is
  m = 1
  n = 1
  if (imodel(i) .eq. -3  .or. imodel(i) .eq. -4       ) go to 3320
  !     check for the presence of the  -666 branches *   *   *   *   *   *
  if ( length(i+1) .ne. -666 ) go to 10
  call sseqiv( ikf, isfd, omegal, omegac )
  !     convert modal data to phase coordinates  *   *   *   *   *   *   *
  cz = nt
  azr = ( volti(1) - volti(2) ) / cz
  azi = ( voltk(1) - voltk(2) ) / cz
  lk = l
  lb = l
  kb = 1
  !     load eqivalent impedance into the tr and tx tables   *   *   *   *
  do  ka = 1, jt
     tr(lk) = azr
     tx(lk) = azi
     if ( lb .ne. lk ) go to 2
     tr(lb) = tr(lb) + volti(2)
     tx(lb) = tx(lb) + voltk(2)
     kb = kb + 1
     lb = lb + kb
2    lk = lk + 1
  end do
10 do j = 1, jt
     if (kodebr(i) .le. 0 ) tx(l) = tx(l) *omegal
     if (kodsem(i) .ne. 0  .and. imodel(i) .ne. -2) tx(l) = tx(l) * omegal
     if ( iprsup .ge. 3 ) write (lunit6, 3524)  i, kbus(i), mbus(i), j, l, tr(l), tx(l)
3524 format( 9h at 3524    , 5i10, 2e20.8 )
     if ( j .ne. n )  go to 3530
     emtpe(m) = tr(l)**2  +  tx(l)**2
     m = m + 1
     n = n + m
3530 l = l + 1
  end do
  xa = tr(is)*tr(is) + tx(is)*tx(is)
  if ( xa .gt. tolmat*emtpe(1) )  go to 3570
  n = 1
3560 kill = 18
  lstat(19) = 3560
  n1 = iabs( kbus(i) )
  n2 = iabs( mbus(i) )
  lstat(14) = nt
  bus1 = bus(n1)
  bus2 = bus(n2)
  flstat(13) = tolmat
  flstat(12) = xa
  flstat(11) = emtpe(n)
  go to 9999
3570 tr(is) = tr(is)/xa
  tx(is) = -tx(is)/xa
  if (nt .le. 1) go to 3320
  js = is
  ne = is
  n1 = is + jt - 1
  do n = 2, nt
     do m = 1, n
        js = js +1
        solr(m) = tr(js)
3630    soli(m) = tx(js)
     end do
     m = n - 1
     j = 1
     ja = is -1
3650 k = ja
     xr = 0.0
     xi = 0.0
     la = 0
     do l = 1, j
        k = k + 1
        la = la + 1
        xr = xr + solr(la)*tr(k) - soli(la)*tx(k)
3680    xi = xi + solr(la)*tx(k) + soli(la)*tr(k)
     end do
     k = k + j
     if ( m .eq. j )  go to 3760
     ja = ja + j
     j = j + 1
     do ll = j, m
        la = la + 1
        xr = xr + solr(la)*tr(k) - soli(la)*tx(k)
        xi = xi + soli(la)*tr(k) + solr(la)*tx(k)
3740    k = k + ll
     end do
     tr(k) = xr
     tx(k) = xi
     go to 3650
3760 tr(k) = xr
     tx(k) = xi
     vr = 0.0
     vi = 0.0
     ke = ne
     do lb = 1, m
        ke = ke + 1
        vr = vr + solr(lb)*tr(ke) - soli(lb)*tx(ke)
3790    vi = vi + solr(lb)*tx(ke) + soli(lb)*tr(ke)
     end do
     xr = solr(m+1) - vr
     xi = soli(m+1) - vi
     xa = xr*xr + xi*xi
     if ( xa .lt. tolmat*emtpe(n) )  go to 3560
     xtr = xr/xa
     xti = -xi/xa
     je = ne
     ne = ne + n
     ky = ne
     tr(ne) = xtr
     tx(ne) = xti
     do jc = 1, m
        lc = n - jc
        ky = ky - 1
        kkk = ky
        vvr = xtr*tr(ky) - xti*tx(ky)
        vvi = xtr*tx(ky) + xti*tr(ky)
        do ic = 1, lc
           tr(je) = tr(je) + tr(kkk)*vvr - tx(kkk)*vvi
           tx(je) = tx(je) + tr(kkk)*vvi + tx(kkk)*vvr
           je = je - 1
3870       kkk = kkk - 1
        end do
        tr(ky) = -vvr
3890    tx(ky) = -vvi
     end do
     if ( iprsup .ge. 3 ) write (lunit6, 3897)  is,  ( tr(jc), tx(jc), jc=is, n1 )
3897 format( /, 9h at 3897    , i10, /, ( 1x, 8e15.6 ) )
3900 end do
  if (kodebr(i) .le. 0 ) go to 3320
  if (kodsem(i) .ne. 0  .and. imodel(i) .ne. -2) go to 3320
  ll = is + jt - 1
  if ( iprsup .ge. 2 ) write (lunit6, 3506)  (  tr(j),  tx(j), j=is, ll )
3506 format( /,  39h at 3506.   cmr +j cmi after  inversion  ,/,( 1x, 8e16.6 ) )
  call multmx( tr(is), emtpf(1), emtpe(1), solr(1), nt )
  call mover( emtpe(1), tr(is), jt )
  call multmx( tx(is), emtpf(1), emtpe(1), solr(1), nt )
  call mover( emtpe(1), tx(is), jt )
  l = is + jt - 1
  if ( iprsup .ge. 2 ) write (lunit6, 3511)  ( j, tr(j), tx(j), j=is, l )
3511 format( /,  36h at 3511.   final y-branch in tr, tx  ,/,9x, 1hj, 15x, 5htr(j), 15x, 5htx(j), /, ( 1x, i9, 2e20.8 ) )
  !
  !                    beginning of multi-phase branches
  !
3320 k = is -1
  ii = i
  iendd = i + nt
3330 na1 = iabs(kbus(i))
  na1 = norder(na1)
  na2 = iabs(mbus(i))
  na2 = norder(na2)
  j = ii
  go to 3440
3430 j = j +1
3440 k = k +1
  if (j .eq. i)   go to 3340
  nb1 = iabs(kbus(j))
  nb1 = norder(nb1)
  nb2 = iabs(mbus(j))
  nb2 = norder(nb2)
  n1 = na1 - nb1
  if ( n1 .gt. 0 ) go to 4030
  if ( n1 .eq. 0 )  go to 4020
  mna1b1 = na1
  mxa1b1 = nb1
  go to 4040
4020 diag(na1) = diag(na1) + tr(k)*2.0
  if (kbus(i) .lt. 0)   diag(na1) = diag(na1) +r(k)*2.0
  diab(na1) = diab(na1) +tx(k)*2.0 +emtpc(k)*2.0*omegac
  go to 4110
4030 mna1b1 = nb1
  mxa1b1 = na1
4040 l = index(mna1b1)
  go to 4060
4050 l = l + 1
4060 if (l .lt. index(mna1b1+1)) go to 4080
  kill = 19
  lstat(19) = 4080
  go to 9999
4080 m = iloc(l)
  if (m .ne. mxa1b1) go to 4050
  isubs1 =  iofgnd+l
  gnd(isubs1)   = gnd(isubs1)   + tr(k)
  isubs1 =  iofbnd+l
  bnd(isubs1)   = bnd(isubs1)   +tx(k) +emtpc(k)*omegac
  isubs1 =  iofgnd+l
  if (kbus(i) .lt. 0) gnd(isubs1)=gnd(isubs1)+r(k)
  if ( iprsup .ge. 1 ) write (lunit6,*) ' below 4080. l, bnd(isubs1) =', l, bnd(isubs1)
4110 n1 = na1 - nb2
  if ( n1 .gt. 0 ) go to 4140
  if ( n1 .eq. 0 )  go to 4130
  mna1b2 = na1
  mxa1b2 = nb2
  go to 4150
4130 diag(na1) = diag(na1) - tr(k)*2.0
  diab(na1) = diab(na1) - tx(k)*2.0
  go to 4220
4140 mna1b2 = nb2
  mxa1b2 = na1
4150 l = index(mna1b2)
  go to 4170
4160 l = l + 1
4170 if (l .lt. index(mna1b2 + 1)) go to 4190
  lstat(19) = 4170
  go to 3112
4190 m = iloc(l)
  if (m .ne. mxa1b2) go to 4160
  isubs1 =  iofgnd+l
  gnd(isubs1)   = gnd(isubs1)   - tr(k)
  isubs1 =  iofbnd+l
  bnd(isubs1)   = bnd(isubs1)   - tx(k)
  if ( iprsup .ge. 1 ) write (lunit6,*) ' below 4190.   l, bnd(isubs1) =', l, bnd(isubs1)
4220 n1 = na2 - nb1
  if ( n1 .gt. 0 )  go to 4250
  if ( n1 .eq. 0 )  go to 4240
  mna2b1 = na2
  mxa2b1 = nb1
  go to 4260
4240 diag(na2) = diag(na2) - tr(k)*2.0
  diab(na2) = diab(na2) -tx(k) * 2.0
  go to 4330
4250 mna2b1 = nb1
  mxa2b1 = na2
4260 l = index(mna2b1)
  go to 4280
4270 l = l + 1
4280 if (l .lt. index(mna2b1+1)) go to 4300
  lstat(19) = 4280
  go to 3112
4300 m = iloc(l)
  if (m .ne. mxa2b1) go to 4270
  isubs1 =  iofgnd+l
  gnd(isubs1)   = gnd(isubs1)   - tr(k)
  isubs1 =  iofbnd+l
  bnd(isubs1)   = bnd(isubs1)   - tx(k)
  if ( iprsup .ge. 1 ) write (lunit6,*) ' below 4300.   l, bnd(isubs1) =', l, bnd(isubs1)
4330 n1 = na2 - nb2
  if ( n1 .gt. 0 )  go to 4360
  if ( n1 .eq. 0 )  go to 4350
  mna2b2 = na2
  mxa2b2 = nb2
  go to 4370
4350 diag(na2) = diag(na2) + tr(k)*2.0
  if (kbus(i) .lt. 0)   diag(na2) = diag(na2) +r(k)*2.0
  diab(na2) = diab(na2) +tx(k)*2.0 +emtpc(k)*2.0*omegac
  go to 3430
4360 mna2b2 = nb2
  mxa2b2 = na2
4370 l = index(mna2b2)
  go to 4390
4380 l = l + 1
4390 if (l .lt. index(mna2b2+1)) go to 4410
  kill = 20
  lstat(19) = 4410
  go to 9999
4410 m = iloc(l)
  if (m .ne. mxa2b2)   go to 4380
  isubs1 =  iofgnd+l
  gnd(isubs1)   = gnd(isubs1)   + tr(k)
  isubs1 =  iofbnd+l
  bnd(isubs1)   = bnd(isubs1)   +tx(k) +emtpc(k)*omegac
  isubs1 =  iofgnd+l
  if (kbus(i) .lt. 0)   gnd(isubs1)   = gnd(isubs1)   +r(k)
  if ( iprsup .ge. 1 ) write (lunit6,*) ' below 4410.   l, bnd(isubs1) =', l, bnd(isubs1)
  if ( iprsup .ge. 1 .and. l .eq. 1 ) write (lunit6,*) ' k, tx(k), emtpc(k), omegac =', k, tx(k), emtpc(k), omegac
  go to 3430
  !                                                      end of [y] buding
3340 n1 = na2 - na1
  if ( n1 .gt. 0 )  go to 3360
  if ( n1 .lt. 0 )  go to 3370
  lstat(19) =3360
  go to 3112
3360 in = na2
  im = na1
  go to 3380
3370 in = na1
  im = na2
3380 ia = index(im)
  gg = tr(k)
  bb = tx(k)
  go to 3400
3390 ia = ia + 1
3400 if (ia .lt. index(im + 1)) go to 3410
  lstat(19) = 3400
  go to 3112
3410 if (iloc(ia) .ne. in) go to 3390
  isubs1 =  iofgnd+ia
  gnd(isubs1)    = gnd(isubs1)    - gg
  isubs1 =  iofbnd+ia
  bnd(isubs1)    = bnd(isubs1)    - bb
  bb = bb + emtpc(k) * omegac
  if (kbus(i) .lt. 0)   gg = gg +r(k)
  diag(im) = diag(im) + gg
  diab(im) = diab(im) + bb
  diag(in) = diag(in) + gg
  diab(in) = diab(in) + bb
  i = i +1
  if ( i.lt. iendd)   go to 3330
  go to 3490
  !
  !
43400 nphcas=iabs(length(i))
  locy11=nr(i)-1
  do ix = 1, nphcas
     n1 = ix + i - 1
     nrow1 = kbus(n1)
     nrow1=norder(nrow1)
     nnpos=locatn(ix,ix)+locy11
     diag(nrow1)=diag(nrow1)+tr(nnpos)
     diab(nrow1)=diab(nrow1)+tx(nnpos)
     ix2=ix+1
     if (ix2.gt.nphcas) go to 43430
     do ixx = ix2, nphcas
        n2 = ixx + i - 1
        nrow2 = kbus(n2)
        nrow2=norder(nrow2)
        if (nrow1.gt.nrow2) goto43420
        istart=index(nrow1)
        istop=index(nrow1+1)-1
        do iy = istart, istop
           if (nrow2.eq.iloc(iy))goto43404
43403   end do
43420   istart=index(nrow2)
        istop=index(nrow2+1)-1
        do 43410 iy=istart,istop
           if (nrow1.eq.iloc(iy))goto43404
43410   end do
43404   nnpos=locatn(ix,ixx)+locy11
        isubs1 =  iofgnd+iy
        gnd(isubs1)   =gnd(isubs1)   +tr(nnpos)
        isubs1 =  iofbnd+iy
        bnd(isubs1)   =bnd(isubs1)   +tx(nnpos)
43402 end do
43430 do ixx = 1,nphcas
        n3 = ixx + i - 1
        nrow2 = mbus(n3)
        nrow2=norder(nrow2)
        if (nrow1.gt.nrow2)goto43412
        istart=index(nrow1)
        istop=index(nrow1+1)-1
        do iy = istart, istop
           if (nrow2.eq.iloc(iy))goto43406
43405   end do
43412   istart=index(nrow2)
        istop=index(nrow2+1)-1
        do iy = istart,istop
           if (nrow1.eq.iloc(iy))goto43406
43414   end do
43406   nnpos=locatn(ix,ixx+nphcas)+locy11
        isubs1 =  iofgnd+iy
        gnd(isubs1)   =gnd(isubs1)   +tr(nnpos)
        isubs1 =  iofbnd+iy
        bnd(isubs1)   =bnd(isubs1)   +tx(nnpos)
43401 end do
  end do
  do ix = 1, nphcas
     n1 = ix + i - 1
        nrow1 = mbus(n1)
        nrow1=norder(nrow1)
        nnpos=locatn(ix+nphcas,ix+nphcas)+locy11
        diag(nrow1)=diag(nrow1)+tr(nnpos)
        diab(nrow1)=diab(nrow1)+tx(nnpos)
        ix2=ix+1
        if (ix2.gt.nphcas)goto43407
        do ixx = ix2, nphcas
           n2 = ixx + i - 1
           nrow2 = mbus(n2)
           nrow2=norder(nrow2)
           if (nrow1.gt.nrow2)goto43415
           istart=index(nrow1)
           istop=index(nrow1+1)-1
           do iy = istart, istop
              if (nrow2.eq.iloc(iy))goto43409
43408      end do
43415      istart=index(nrow2)
           istop=index(nrow2+1) -1
           do 43416 iy=istart,istop
              if (nrow1.eq.iloc(iy))goto43409
43416      end do
43409      nnpos=locatn(ix+nphcas,ixx+nphcas)+locy11
           isubs1 =  iofgnd+iy
           gnd(isubs1)   =gnd(isubs1)   +tr(nnpos)
           isubs1 =  iofbnd+iy
           bnd(isubs1)   =bnd(isubs1)   +tx(nnpos)
43431   end do
43407 end do
  i=i+nphcas
3490 if ( i .le. ibr )  go to 3080
  !
  !                                                    end of [y] building
  !
  do i = 1, ntot
     emtpe(i) = diag(i)**2 + diab(i)**2
     if (kode(i) .gt. 0)   kode(i) = -kode(i)
4502 end do
  n3 = 1
  call vecrsv( tclose(1), kswtch, n3 )
  call vecrsv( diag(1), ntot, n3 )
  call vecrsv( diab(1), ntot, n3 )
  call vecrsv( gnd(iofgnd+1), ioffd, n3 )
  call vecrsv( bnd(iofbnd+1), ioffd, n3 )
  n12 = -4
  n15 = 0
  call vecrsv( volt(1), n12, n15 )
  if ( iprsup .lt. 3 )  go to 4517
  write (lunit6, 4513)  ntot, ioffd
4513 format( /,  77h y-matrix of s.s. phasor solution, b4 starting elimination.    ntot, ioffd =  , 2i8, /, 9x, 1hi, 6x, 4hiloc, &
          5x, 5hindex, 17x, 3hgnd, 17x, 3hbnd, 21x, 4hdiag, 16x, 4hdiab  )
  n3 = ntot + 1
  if ( ioffd .gt. n3 )  n3 = ioffd
  n1 = iofgnd + 1
  n2 = iofbnd + 1
  do i = 1, n3
     write (lunit6, 4514)  i, iloc(i), index(i), gnd(n1), bnd(n2),diag(i), diab(i)
4514 format( 3i10, 2e20.8, 5x, 2e20.8 )
     n1 = n1 + 1
     n2 = n2 + 1
  end do
4517 do i = 1, ntot
     if (kode(i) .gt. 0) go to 4570
     j = i
     im = -kode(j)
4530 if (im .gt. 0 ) in = norder(im)
     il = norder(j)
     if (im .eq. 0) in = il
     im = -kode(il)
     kode(il) = in
     j = il
     if (j .ne. i) go to 4530
4570 end do
  do i = 1, ntot
     j = kode(i)
     if (j .ge. i) go to 4820
4610 j = kode(j)
     n1 = j - i
     if ( n1 .lt. 0 )  go to 4610
     if ( n1 .eq. 0 )  go to 4820
     j = i
     k = 0
4640 k = k + 1
     if ( k  .gt.  lsiz26 )   go to 4670
     itemp(k) = j
     j = kode(j)
     if (j .ne. i) go to 4640
     if ( k .gt. 1 )  go to 4680
     lstat(19) = 4670
     go to 3112
4670 kill = 21
     lstat(19) = 4670
     go to 9999
4680 mk = k - 1
     do m = 1, mk
        kmm = k - m
        do n = 1, kmm
           n1 = itemp(n) - itemp(n+1)
           if ( n1 .gt. 0 )  go to 4740
           if ( n1 .lt. 0 )  go to 4750
           lstat(19) = 4740
           go to 3112
4740       itp = itemp(n)
           itemp(n) = itemp(n+1)
           itemp(n+1) = itp
4750       end do
4760    end do
     j = itemp(1)
     do m = 2, k
        l = itemp(m)
        kode(j) = l
4800    j = l
     end do
     kode(j) = itemp(1)
4820 end do
  call mover0 ( solr(1), n14 )
  call mover0 ( soli(1), n14 )
  if ( numsub  .le.  0 ) go to 4905
  !     assign unit current sources to begin thevenin calculation:
  if ( iprsup  .ge.  1 ) write (lunit6, 4823)  numsub, isubeg(numsub+1)
4823 format (  35h begin phasor thevenin calculation., 29h   numsub, isubeg(numsub+1) =,  2i6  )
  if ( iprsup  .le.  1 )   go to 4837
  write (lunit6, 4827)  ( isubeg(i),  i=1, numsub )
4827 format (    8h isubeg:,  20i6  )
  n1 = isubeg(numsub+1) - 1
  write (lunit6, 4831)  ( ksub(i),  i=1, n1 )
4831 format (    6h ksub:,  20i6  )
  write (lunit6, 4832)  ( msub(i),  i=1, n1 )
4832 format (    6h msub:,  20i6  )
4837 l = 1
  do n2 = 1, numsub
     n1 = ntot
     n7 = isubeg(n2+1)
4863 d8 = -1.0
     j = ksub(l)
     do n10=1, 2
        if ( j  .eq.  1 )  go to 4871
        n12 = norder(j)
        n5 = n12 + n1
        solr(n5) = d8
        if ( iprsup  .ge.  3 ) write (lunit6, 4869)  n5, j, n12, n2, l, d8
4869    format ( 42h thevenin source.  n5, j, n12, n2, l, d8 =, 5i8,  e14.3  )
4871    d8 = 1.0
4878    j = msub(l)
     end do
     l = l + 1
     if ( l  .ge.  n7 ) go to 4892
     n1 = n1 + ntot
     go to 4863
4892 end do
4905 n1 = 2 * lbus
  if ( kconst  .eq.  0 )   go to 4977
  do i = 1, kconst
     j = node(i)
     if ( iabs( iform(i) ) .ne.14 ) go to 4976
     if ( tstart(i)  .eq.  5432. ) go to 4975
     if (  tstart(i).ge. 0.0) go to 4976
     if ( j .ge. -n1 )  go to 4975
     node(i) = node(i) + n1
     go to 4976
4975 if ( iform(i+1)  .eq.  18 ) j = time1(i+1)
     j = iabs(j)
     j = norder(j)
     solr(j) = solr(j) + crest(i) * cosz(time1(i) )
     soli(j) = soli(j) + crest(i) * sinz(time1(i) )
     if ( iprsup .ge. 5 ) write (lunit6, 1999) i,j,node(i),kode(j),crest(i),time1(i)
1999 format( /,39h i,j,node(i),kode(j),crest(i),time1(i), 4i4,2e18.6)
     if ( iprsup .ge. 5 ) write (lunit6,1497) isubs1,isubs2,isubs3,isubs4,n1,n2,n3,ia,ja,ii,jj,kkk,iy,ib,ix
1497 format(15i8)
     if (j .le. ncurr)   go to 4976
     k = kode(j)
4979 if (k .eq. j)  go to 4976
     solr(k) = solr(j)
     soli(k) = soli(j)
     k = kode(k)
     go to 4979
4976 end do
4977 nl = 0
4972 n12 = nl + 1
  n13 = nl + ntot
  if ( iprsup  .ge.  2 ) write (lunit6, 5004)  n12, n13,( solr(i), soli(i), i=n12, n13 )
5004 format(   6h cells,  i5,  10h   through,  i5, 43h   of right hand side vector (complex) ....,/,  ( 1x,  8e16.7 )  )
  nl = nl + ntot
  if ( nl  .le.  loopss(2) ) go to 4972
  if ( iprsup .ge. 2 ) write (lunit6, 5006)  ( kode(i), i=1, ntot )
5006 format ( /, 102h (kode(i), i=1, ntot)   in  'over10'  just before beginning network elimination on steady-state  (y) .   ,/, &
          ( 1x, 20i6 ) )
  i = 1
  go to 5020
5010 nl = 0
5013 n12 = nl + i
  so = solr(n12)*diag(i) - soli(n12)*diab(i)
  soli(n12) = solr(n12)*diab(i) + soli(n12)*diag(i)
  solr(n12) = so
  nl = nl + ntot
  if ( nl  .le.  loopss(2) ) go to 5013
  i = i + 1
5020 if (i .gt. ncurr) go to 5500
  if (kode(i) .gt. i) go to 5170
  yy = diag(i)*diag(i) + diab(i)*diab(i)
  if ( iprsup .ge. 6 ) write (lunit6, 5023)  i, ncurr, index(i), index(i+1),diag(i), diab(i)
5023 format ( 41h new row  i  of steady-state elimination.  ,7x, 1hi, 3x, 5hncurr, 4x, 8hindex(i), 2x, 10hindex(i+1), &
          8x, 7hdiag(i), 8x, 7hdiab(i), /,  41x, 2i8, 2i12, 2e20.10 )
  if ( yy .gt. tolmat*emtpe(i) )  go to 5045
  do n23 = 1, ntot
     if ( norder(n23) .eq. i )  go to 5043
5042 end do
  kill = 22
  lstat(19) = 5042
  lstat(16) = i
  go to 9999
5043 write (lunit6, 5041)  bus(n23), emtpe(i), yy, tolmat
5041 format( /, 107h caution. ---- during y-matrix elimination for steady-state solution voltages, a near-zero diagonal element  ,/, &
          15x, 10hfor node ', a6,  92h' exists just before reciprocation. Using magnitudes squared for all 3 quantities, we have  ,/, &
          15x, 25horiginal diagonal value =, e12.3, 24h,   questionable value =, e12.3, 21h,   tolerance ratio =, &
          e12.3, 1h. ,/, 15x, 107hthe node in question may be connected to other nodes, forming a subnetwork.   but the subnetwork has no (or     )
  write (lunit6, 5044)
5044 format( 15x, 102hvery weak) path to ground or other known-voltage node in the steady-state.   solution voltages of this  ,/, &
          15x, 35hsubnetwork will all be set to zero.   )
  if ( solr(i) .ne. 0.0 )  go to 5046
  if ( soli(i) .eq. 0.0 )  go to 5048
5046 kill = 23
  lstat(19) = 5046
  bus1 = bus(n23)
  go to 9999
5048 diag(i) = 0.0
  diab(i) = 0.0
  go to 15045
5045 diag(i) = diag(i) / yy
  diab(i) = -diab(i)/yy
15045 ii = index(i)
  go to 5060
5050 isubs1 =  iofgnd+ii
  gnd(isubs1)    = gj
  isubs1 =  iofbnd+ii
  bnd(isubs1)    = bj
  ii = ii + 1
5060 if (ii .eq. index(i+1))go to 5010
  j = iloc(ii)
  isubs1 =  iofgnd+ii
  isubs2 =  iofbnd+ii
  gj = gnd(isubs1)   *diag(i) - bnd(isubs2)   *diab(i)
  isubs1 =  iofgnd+ii
  isubs2 =  iofbnd+ii
  bj = gnd(isubs1)   *diab(i) + bnd(isubs2)   *diag(i)
  isubs1 =  iofgnd+ii
  isubs2 =  iofbnd+ii
  diag(j) = diag(j) - gj*gnd(isubs1)    + bj*bnd(isubs2)
  isubs1 =  iofgnd+ii
  isubs2 =  iofbnd+ii
  diab(j) = diab(j) - gj*bnd(isubs2)    - bj*gnd(isubs1)
  isubs1 =  iofgnd+ii
  isubs2 =  iofbnd+ii
  if ( iprsup .ge. 20 ) write (lunit6, 5063)  i, ii, j, gj, bj, diag(i), diab(i),diag(j), diab(j), gnd(isubs1)   , bnd(isubs2)
5063 format( /,  78h y(i,j)/y(i,i)   and   y(j,j) - y(j,i)*(y(i,j)/y(i,i))   at 5063.   i, ii, j =  , 3i10, /, 1x, 8e16.6  )
  if (j .gt. ncurr)   go to 5072
  nl = 0
5068 n12 = nl + j
     n13 = nl + i
     solr(n12) = solr(n12) - gj*solr(n13) + bj*soli(n13)
     soli(n12) = soli(n12) - gj*soli(n13) - bj*solr(n13)
     nl = nl + ntot
     if ( nl  .le.  loopss(2) )   go to 5068
5072 ja = index(j)
     ia = ii +1
5080 if (ia .eq. index(i+1)) go to 5050
     ik = iloc(ia)
     go to 5110
5100 ja = ja + 1
5110 if (ja .lt. index(j+1)) go to 5130
     lstat(19) = 5110
     go to 3112
5130 jk = iloc(ja)
     if (ik .ne. jk)   go to 5100
     isubs1 =  iofgnd+ja
     isubs2 =  iofgnd+ia
     isubs3 =  iofbnd+ia
     gnd(isubs1)    = gnd(isubs1)    - gj*gnd(isubs2) + bj*bnd(isubs3)
     isubs1 =  iofbnd+ja
     isubs2 =  iofbnd+ia
     isubs3 =  iofgnd+ia
     bnd(isubs1)    = bnd(isubs1)    - gj*bnd(isubs2)-bj * gnd(isubs3)
     isubs1 =  iofgnd+ja
     isubs2 =  iofbnd+ja
     isubs3 =  iofgnd+ia
     isubs4 =  iofbnd+ia
     if ( iprsup .ge. 20 ) write (lunit6, 5134)  ja, jk, ia, gnd(isubs1)   , bnd(isubs2)   , gnd(isubs3)   , bnd(isubs4)
5134 format( /,  24h at 5134.   ja, jk, ia = , 3i10, /, 1x, 4e20.6 )
     ia = ia + 1
     ja = ja + 1
     go to 5080
5170 ii = index(i)
     j = kode(i)
     jj = index(j)
     go to 5190
5180 ii = ii + 1
5190 if (ii .ge. index(i+1)) go to 5360
     ia = iloc(ii)
     n1 = ia - j
     if ( n1 .gt. 0 )  go to 5230
     if ( n1 .lt. 0 )  go to 5300
     isubs1 =  iofgnd+ii
     diag(j) = diag(j) + diag(i) + 2.*gnd(isubs1)
     isubs1 =  iofbnd+ii
     diab(j) = diab(j) + diab(i) + 2.*bnd(isubs1)
     go to 5180
5230 if (jj .lt. index(j+1)) go to 5250
     if ( iprsup .ge. 1 ) write (lunit6, 5231) (index(i), iloc(i), i = 1, 40)
5231 format( /, 9h at 5231   ,/, ( 1x, 10i12 ) )
     lstat(19) = 5231
     go to 3112
5250 ja = iloc(jj)
     if (ia .eq. ja)   go to 5290
     jj = jj + 1
     go to 5230
5290 isubs1 =  iofgnd+jj
     isubs2 =  iofgnd+ii
     gnd(isubs1)    = gnd(isubs1)    + gnd(isubs2)
     isubs1 =  iofbnd+jj
     isubs2 =  iofbnd+ii
     bnd(isubs1)    = bnd(isubs1)    + bnd(isubs2)
     jj = jj + 1
     go to 5180
5300 kkk = index(ia)
     go to 5320
5310 kkk = kkk + 1
     if (kkk .lt. index(ia+1))   go to 5320
     kill = 24
     lstat(19) = 5320
     lstat(16) = kkk
     lstat(15) = ia
     lstat(14) = index(ia+1)
     go to 9999
5320 ka = iloc(kkk)
     if (ka .ne. j)   go to 5310
     isubs1 =  iofgnd+kkk
     isubs2 =  iofgnd+ii
     gnd(isubs1)     = gnd(isubs1)     + gnd(isubs2)
     isubs1 =  iofbnd+kkk
     isubs2 =  iofbnd+ii
     bnd(isubs1)     = bnd(isubs1)     + bnd(isubs2)
     go to 5180
5360 nl = 0
5366 n12 = nl + j
     n13 = nl + i
     solr(n12) = solr(n12) + solr(n13)
     soli(n12) = soli(n12) + soli(n13)
     nl = nl + ntot
     if ( nl  .le.  loopss(2) )   go to 5366
     i = i + 1
     go to 5020
5500 if ( istep  .ne.  -4567 )  go to 5757
     istep = 0
     call fxsour
     if ( kill  .gt.  0 )  go to 9999
     go to 6000
5757 i = ncurr
5510 if (i .eq. 0) go to 6000
     if (kode(i) .gt. i) go to 5560
     ii = index(i)
5540 if (ii .eq. index(i+1)) go to 5570
     j = iloc(ii)
     isubs1 =  iofgnd+ii
     isubs2 =  iofbnd+ii
     nl = 0
5547 n12 = nl + i
     n13 = nl + j
     solr(n12) = solr(n12)  -  gnd(isubs1) * solr(n13)   + bnd(isubs2) * soli(n13)
     soli(n12) = soli(n12)  -  gnd(isubs1) * soli(n13)   - bnd(isubs2) * solr(n13)
     if ( iprsup .ge. 20 ) write (lunit6, 5191)  nl, i, ii, j, solr(n12), soli(n12), solr(n13), soli(n13), gnd(isubs1), bnd(isubs2)
5191 format( /,  39h backsubstitution step.  nl, i, ii, j =,4i9   ,/,   1x, 6e20.6  )
     nl = nl + ntot
     if ( nl  .le.  loopss(2) )   go to 5547
     ii = ii + 1
     go to 5540
5560 j = kode(i)
     nl = 0
5563 n12 = nl + i
     n13 = nl + j
     solr(n12) = solr(n13)
     soli(n12) = soli(n13)
     nl = nl + ntot
     if ( nl  .le.  loopss(2) )   go to 5563
5570 i = i - 1
     go to 5510
6000 lastov = nchain
     nchain = 11
     nl = 0
6007 n12 = nl + 1
     n13 = nl + ntot
     if ( iprsup .ge. 2 ) write (lunit6, 6013)  nl, (solr(i), soli(i), i=n12, n13)
6013 format( /, 43h s.s. solution at exit from "over10".  nl =,i5  ,/, ( 1x, 10e13.4 ) )
     nl = nl + ntot
     if ( nl  .le.  loopss(2) )   go to 6007
     if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
4568 format ( 24h  "exit  module over10." )
     go to 99999
9999 lastov = nchain
     nchain = 51
     lstat(18) = 10
     if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
99999 return
   end subroutine over10
!
!     subroutine fxsour.
!
   subroutine fxsour
     implicit real*8 (a-h, o-z), integer*4 (i-n)
     !     This module of overlay 10 is called only once, if and only
     !     if the data case involves phasor power (p+jq, etc.)
     !     constraints.  This is EMTP load flow of "fix source" request.
      include 'blkcom.ftn'
      include 'labcom.ftn'
      include 'space2.ftn'
      include 'deck10.ftn'
      character*8 fixbu3, fxtem1
      dimension vdiff(20)
      if (iprsup .ge. 2) write (lunit6,10)
10    format(/,33h top of fxsour = load flow module)
      llm1 = -1
      ll0 = 0
      ll1 = 1
      ll2 = 2
      nekstp = 0
      if ( iprsup .le. 0 )  go to 2000
      call runtym ( cc1, cc2 )
      write ( lunit6, 8484 )  cc1, cc2
8484  format ( 20h  @@@@@@@@  time =  , 2e20.5 )
2000  continue
      read (unit = abuff, fmt = 2250) nnnout, nitera, nflout,nprint, ralchk, cfitev, cfitea
2250  format ( 8x, 4i8, 3e8.0 )
      if ( nitera  .eq.  0 )  nitera = 500
      !     next only allow nflout = 1, 2, 4, 5, 10, or 20 values
      if (nflout .le. 0  .or.  nflout .gt. 20)  nflout = 20
 2256 if ( 20/nflout*nflout .eq. 20 )  go to 2259
      nflout = nflout + 1
      go to 2256
 4567 write ( lunit6, 5678 )
 5678 format( 26h ***** no fix source ***** )
      go to 9000
 2259 if (ralchk .eq. 0.0) ralchk = 0.01
      if (cfitev .eq. 0.0) cfitev = 0.2
      if (cfitea .eq. 0.0) cfitea = 2.5
      if (noutpr .eq. 0) write (kunit6, 3636) nitera, ralchk
 3636 format ( 23h+ last fix source card. , i6, f14.8 )
      nkr = lstat(69)
      if ( nkr  .eq.  0 )  go to 4567
      eknom1 = 0.0
      do i = 1, nkr
         pmax = absz( fixbu5(i) )
         qmax = absz( fixbu8(i) )
         if ( eknom1 .lt. pmax )  eknom1 = pmax
         if ( eknom1 .lt. qmax )  eknom1 = qmax
      end do
      nekite = 0
      picon = 360. / twopi
      n14 = ntot + loopss(2)
      np = ncurr + 1
      do i = 1, ntot
         jj = norder(i)
         jndex(jj) = i
      end do
      mm = 0
      do jj = np, ntot
         if (jj .eq. norder(1)) go to 1030
         if (kode(jj) .gt. jj) go to 1030
         i = jndex(jj)
         mm = mm + 1
         fxtem1(mm) = bus(i)
         kndex(mm) = i
         do ixx = 1, kconst
            if (iabs(node(ixx)) .ne. i) go to 1039
            if (crest(ixx) .ne. 0) fxtem6(mm) = 1.0/crest(ixx)
1039     end do
1030  end do
      call mover(solr(1), solrsv(1), n14)
      call mover(soli(1), solisv(1), n14)
      go to 3466
      !     begin calculation of node currents
1000  n3 = 2
      call vecrsv( diagg(1), ntot, n3 )
      call vecrsv( diabb(1), ntot, n3 )
      if ( kburro .eq. 1 ) go to 4372
      if ( ioffd .le. lstat(14) )  go to 4372
      write ( lunit6, 4848 ) ioffd, lstat(14)
4848  format ( 41h no enough space for load flow .  ioffd = ,i8, 33h but the limit space is lsiz23 =  , i8 )
      stop
 4372 call vecrsv( gndd(1), ioffd, n3 )
      call vecrsv( bndd(1), ioffd, n3 )
      n3 = -4
      n15 = 0
      call vecrsv( volt(1), n3, n15 )
      nekcc = 0
      vdiff(npp) = 0.0
      if ( ncurr  .le.  0 )  ncurr = 1
      do   ip=1, ntot
         emtpe(ip) = 0.0
         emtpf(ip) = 0.0
      end do
      i = 1
7350  if ( i .gt. ntot )  go to 7390
      vr = solr(i)
      vi = soli(i)
      jj = index(i)
      kkk = index(i+1)
      emtpe(i) = emtpe(i)  +  diagg(i)*vr - diabb(i)*vi
      emtpf(i) = emtpf(i)  +  diagg(i)*vi + diabb(i)*vr
      go to 7370
7360  jj = jj + 1
7370  if ( jj .ge. kkk )  go to 7380
      j = iloc(jj)
      der = gndd(jj)   *vr - bndd(jj)   *vi
      dei = gndd(jj)   *vi + bndd(jj)   *vr
      emtpe(j) = emtpe(j) + der
      emtpf(j) = emtpf(j) + dei
      emtpe(i) = emtpe(i)  +  gndd(jj)   *solr(j) - bndd(jj)   *soli(j)
      emtpf(i) = emtpf(i)  +  gndd(jj)   *soli(j) + bndd(jj)   *solr(j)
      go to 7360
 7380 i = i + 1
      go to 7350
 7390 i = ncurr + 1
 7405 if (i .gt. ntot) go to 8888
      if (i .eq. norder(1))  go to 7440
      j = kode(i)
      if (j .le. i) go to 7410
      emtpe(j) = emtpe(i) + emtpe(j)
      emtpf(j) = emtpf(i) + emtpf(j)
      go to 7440
      !     begin calculation of  p,q,v for voltage sources
 7410 nekcc = nekcc + 1
      fxtem5(nekcc) = sqrtz( solr(i)**2 + soli(i)**2 )
      fxtem4(nekcc) = 0.0
      if ( fxtem5(nekcc)  .gt.  0.0 ) fxtem4(nekcc) = picon * atan2z( soli(i), solr(i) )
      fxtem2(nekcc) = ( solr(i) * emtpe(i) + soli(i) * emtpf(i) ) * onehaf
      fxtem3(nekcc) = ( soli(i) * emtpe(i) - solr(i) * emtpf(i) ) * onehaf
      if (iprsup  .gt.  8)  write (lunit6, 1050)  fxtem1(nekcc),fxtem2(nekcc),fxtem3(nekcc),fxtem4(nekcc),fxtem5(nekcc)
1050  format ( 22h bus,p,q,angle,v(peak), a6, 2x, 4e13.4 )
7440  i = i + 1
      go to 7405
      !     begin to check p,q,v, angle, correction of them
8888  nekstp = 1
      ekitev = ( nitera - nekite ) * ( nitera - nekite )
      ekfact = nitera * nitera
      ekitev = ekitev / ekfact
      ekfact = ekitev * cfitea
      ekitev = ekitev * cfitev
      do i = 1, nkr
         psum = 0.0
         qsum = 0.0
         nekn1 = 0
         do k = 1, nekcc
            if (fxtem1(k) .eq. fixbu1(i)) go to 7171
            if (fxtem1(k) .eq. fixbu2(i)) go to 7171
            if (fxtem1(k) .ne. fixbu3(i)) go to 7030
7171        psum = psum + fxtem2(k)
            qsum = qsum + fxtem3(k)
            nekn1 = nekn1 + 1
            if (nekn1 .eq. 3)  go to 7272
7030     end do
         if (nekn1 .ne. 0)  go to 7272
         write ( lunit6, 7032 )  i
7032     format( 45h error in table, node name on fix source card, 43h does not fit any node name on source cards , &
              /, 18h fix source number, i4, 16h will be ignored )
         go to 7010
7272     if ( iprsup  .ge.  2 ) write ( lunit6, 7035 ) i, psum, qsum
7035     format ( 25h fix source, psum, qsum : , i6, 2x, 2e13.4  )
         fixb10(i) = psum
         fixb11(i) = qsum
         !     denominator is calculated
         !     eknom1 = onehaf * ( absz (psum) + absz( fixbu5(i) ) )
         !     eknom2 = onehaf * ( absz (qsum) + absz( fixbu8(i) ) )
         ekdif1 = 0.0
         ekdif2 = 0.0
         if (nekfix(i) .eq. 2)  go to 8110
         if (eknom1 .lt. tolmat)  eknom1 = tolmat
         ekdif1 = (psum - fixbu5(i)) / eknom1
         if (nekfix(i) .eq. 1)  go to 8112
8110     ekdif2 = (qsum - fixbu8(i)) / eknom1
         !     the relative difference check for p and q
8112     if (absz(ekdif1) .le. ralchk .and. absz(ekdif2) .le. ralchk) go to 3434
         !     the absolute difference check for p and q
         !     if ( absz( psum - fixbu5(i) )  .lt.  abschk   .and.
         !    1  absz(qsum - fixbu8(i)) .lt. abschk )  go to 3434
         nekstp = 0
         !     change peak voltage and angle
3434     ekdif1 = ekdif1 * ekfact
         if ( ekdif1  .gt.  1.0 )  ekdif1 =   1.0
         if ( ekdif1  .lt. -1.0 )  ekdif1 = - 1.0
         ekdif2 = ekdif2 * ekitev
         if ( ekdif2  .gt.  0.01 ) ekdif2 =   0.01
         if ( ekdif2  .lt. -0.01 ) ekdif2 = - 0.01
         if ( iprsup .gt. 1 )  write ( lunit6, 3536 ) ekdif1, ekdif2
3536     format ( 29h  voltage and angle change = , 2e20.6  )
         do np = 1, 3
            go to  ( 4141, 4242, 4343 ),  np
4141        do mm = 1, nekcc
               if ( fxtem1(mm)  .eq.  fixbu1(i) )   go to 8181
            end do
            go to 7010
4242        if (fixbu2(i) .eq. blank) go to 7010
            do mm = 1, nekcc
               if ( fxtem1(mm)  .eq.  fixbu2(i) )   go to 8282
            end do
            go to 7010
4343        if ( fixbu3(i)  .eq.  blank )  go to 7010
            do mm = 1, nekcc
               if ( fxtem1(mm)  .eq.  fixbu3(i) )   go to 8383
            end do
            go to 7010
8181        ekn1 = 0.0
6769        angel = fxtem4(mm) - ekdif1
            if (angel .le. fixbu6(i) .and. angel .ge. fixbu4(i)) go to 6363
            ekdif1 = 0.1 * ekdif1
            !     from ma ren-ming.   installed march, 1987.
            if ( absz ( ekdif1 ) .gt. epsiln ) go to 6769
            ekdif1 = 0.0
            go to 6363
8282        ekn1 = -120.0
            go to 6363
8383        ekn1 = 120.0
6363        if ( nekfix(i) .eq. 2 )  fxtem4(mm) = fixbu5(i) + ekn1
            if (nekfix(i) .eq. 2) go to 8400
            !     correction of angles
            fxtem4(mm) = fxtem4(mm) - ekdif1
            !     voltage
            if ( nekfix(i)  .eq.  1 )  fxtem5(mm) = fixbu8(i)
            if (nekfix(i) .eq. 1) go to 8510
            !     correction of voltages
8400        fxtem5(mm) = fxtem5(mm)*( 1.0 - ekdif2 )
            !     check for min/max voltage
            if (fxtem5(mm)  .gt.  fixbu9(i))  fxtem5(mm) = fixbu9(i)
            if (fxtem5(mm)  .lt.  fixbu7(i))  fxtem5(mm) = fixbu7(i)
            !     new voltage is calculated
8510        ekdgr = fxtem4(mm) / picon
            nekn4 = kndex(mm)
            nekn4 = norder(nekn4)
            solrsv(nekn4) = fxtem5(mm) * cosz (ekdgr)
            solisv(nekn4) = fxtem5(mm) * sinz (ekdgr)
            nkode = kode(nekn4)
8585        if (nkode .eq. nekn4) go to 8787
            solrsv(nkode) = solrsv(nekn4)
            solisv(nkode) = solisv(nekn4)
            nkode = kode(nkode)
            go to 8585
            !     calculation of voltage change
8787        cchanr = solrsv(nekn4) - solr(nekn4)
            cchani = solisv(nekn4) - soli(nekn4)
            if ( absz(cchani) .gt. absz(cchanr) ) cchanr = cchani
            cchanr = cchanr * fxtem6(mm)
            if ( absz(cchanr) .le. absz(vdiff(npp)) )  go to 7777
            vdiff(npp) = cchanr
            jndex(npp) = i
7777        if (iprsup .ge. 4)  write ( lunit6, 8800 ) fxtem1(mm), solr(nekn4), soli(nekn4), solrsv(nekn4), solisv(nekn4)
8800        format ( 32h busname,solr(nekn4),soli(nekn4),a6,2e16.6,20h   the new value is ,  2e16.6 )
8050     end do
7010  end do
      if ( nekstp .eq. 1 )  go to 2121
      if ( nflknt .gt. 0 )   go to 2222
2121  write (lunit6, 3333) (vdiff(k), k = 1, npp)
3333  format ( 12h+vchang(k)= , 20f6.3 )
      if ( npp .lt. 20   .and. nekstp .ne. 1 )  go to 3476
      if ( nnnout  .eq.  1 ) write (lunit6, 4444) (jndex(k), k = 1, npp)
4444  format ( 12h fix source , 20i6 )
3466  write (lunit6, 3471)
3471  format ( 1x )
      npp = 0
3476  nflknt = nflout
2222  if (nekstp .eq. 1) go to 6789
      if (nekite .ge. nitera) go to 8900
      nflknt = nflknt - 1
      do ip = 1, n14
         solr(ip) = solrsv(ip)
2517     soli(ip) = solisv(ip)
      end do
      nekite = nekite + 1
      npp = npp + 1
      i = ncurr
5510  if (i .eq. 0) go to 1000
      if (kode(i) .gt. i) go to 5560
      ii = index(i)
5540  if (ii .eq. index(i+1)) go to 5570
      j = iloc(ii)
      isubs1 =  iofgnd+ii
      isubs2 =  iofbnd+ii
      solr(i) = solr(i)  -  gnd(isubs1) * solr(j)   + bnd(isubs2) * soli(j)
      soli(i) = soli(i)  -  gnd(isubs1) * soli(j)   - bnd(isubs2) * solr(j)
      ii = ii + 1
      go to 5540
5560  j = kode(i)
      solr(i) = solr(j)
      soli(i) = soli(j)
5570  i = i - 1
      go to 5510
8900  write ( lunit6, 8920 )  nitera
8920  format(/, 45h warning : the number of load-flow iterations,8h exceeds,i8,36h, but the solution is not converging, &
           23h to the desired result. ,/,46h the iteration process will be stopped at this, &
           51h point to continue with the initialization process.)
      !     done with all iterations, redefine vector crest and time1
6789  write (lunit6, 6788) nekite
6788  format (/, 38h number of load flow iterations used =, i5, 38h. status of the sources after the last, &
           12h iteration :)
      if (iprsup .ge. 1 .or. nprint .eq. 1) write (lunit6,3952)
3952  format ( 18h  row  node   name,  10x, 7hvoltage, 4x, 7hdegrees,  8x, 10hreal power  3x, 14hreactive power )
      do m = 1, nkr
         bus1 = fixbu1(m)
         n = 1
1199     if ( bus1  .eq.  blank ) go to 3300
         do j = 1, nekcc
            if ( fxtem1(j) .ne. bus1 ) go to 3000
            i = kndex(j)
            do nfix = 1, kconst
               if (iabs(node(nfix)) .eq. i) go to 3939
3737        end do
            go to 3000
3939        crest(nfix) = fxtem5(j)
            time1(nfix) = fxtem4(j) / picon
            if ( iprsup .lt. 9  .and.  nprint .ne. 1 ) go to 3000
            if ( n  .gt.  1 ) go to 3739
            write (lunit6, 3500) nfix, i, bus(i), crest(nfix), fxtem4(j), fixb10(m), fixb11(m)
3500        format ( 1x, i5, i6, 1x, a6, e17.6, f11.4, e18.6, e17.6 )
            go to 3000
3739        write (lunit6, 3500) nfix, i, bus(i), crest(nfix), fxtem4(j)
3000     end do
         go to ( 2345, 3456, 3300 ), n
2345     bus1 = fixbu2(m)
         n = 2
         go to 1199
3456     bus1 = fixbu3(m)
         n = 3
         go to 1199
3300  end do
      if ( iprsup .le. 0 ) go to 9000
      call runtym ( cc1, cc2 )
      write ( lunit6, 8484 )  cc1, cc2
      if ( iprsup .le. 6 )  go to 9000
      do i = 1, kconst
         write (lunit6,5755) i, node(i),crest(i),time1(i)
5755     format( /, 29h i,node(i),crest(i),time1(i) ,2i4,2e18.6)
5656  end do
9000  if (iprsup .ge. 2) write (lunit6,9010)
9010  format(31h exit fxsour = load flow module,/)
      return
    end subroutine fxsour
!
!     subroutine sseqiv.
!
    subroutine sseqiv(ikf, isfd, omegal, omegac)
      implicit real*8 (a-h, o-z), integer*4 (i-n)
      include 'blkcom.ftn'
      include 'labcom.ftn'
      dimension ur(2), ui(2)
      equivalence (volti(1), ur(1)), (voltk(1), ui(1))
      !     this routine produces a s.s. equivalent of the branches.this
      !     equivalent is inserted into the tr and tx tables *   *   *   *   *
      idk = 2 * ikf
      ikf = ikf + 1
      if ( iprsup .gt. 0 ) write ( lunit6, 1 ) ikf, isfd, imfd(idk+1), imfd(idk+2)
1     format(41h integer counters at start of sseqiv.....,10x,7x,3hikf,6x,4hisfd,6x,4hizfd,6x,4hipfd,/,51x,4i10)
      do  kb = 1, 2
         ui(kb) = 0.
         ur(kb) = 0.
         kc = idk + kb
         isf = imfd(kc)
         ist = isfd + 1
         isu = isfd + isf * 5
         do ka = ist, isu, 5
            !     calculate branch impedance   *   *   *   *   *   *   *   *   *   *
            ar1 = rmfd(ka)
            al1 = rmfd(ka+1) * omegal
            ac1 = rmfd(ka+2) * omegac
            arl = rmfd(ka+3)
            azr = 0.
            azi = al1
            if ( ac1 .ne. 0.0 ) ac1 = 1.0 / ac1
            !     process the parallel r and l connection, first   *   *   *   *   *
            if ( arl .eq. 0.0 ) go to 2
            den = 1.0 / ( arl * arl + al1 * al1 )
            azr = arl * ( al1 * al1 ) * den
            azi = al1 * ( arl * arl ) * den
2           azr = azr + ar1
            azi = azi - ac1
            !     invert impedance to sum admittances  *   *   *   *   *   *   *
            den = 1.0 / ( azr * azr + azi * azi )
            ur(kb) = ur(kb) + azr * den
3           ui(kb) = ui(kb) - azi * den
         end do
         if ( iprsup .gt. 0 ) write ( lunit6, 4 ) kb, ur(kb), ui(kb)
4        format(41h equivalent modal admittance for mode no.,i6,10x, 2e22.10)
         isfd = isu
         !     convert modal admittance to impedance*   *   *   *   *   *   *   *
         den = 1.0 / ( ur(kb) * ur(kb) + ui(kb) * ui(kb) )
         ur(kb) = ur(kb) * den
5        ui(kb) = -ui(kb) * den / omegal
      end do
      return
    end subroutine sseqiv
    !
    !     end of file: over10.for
    !
