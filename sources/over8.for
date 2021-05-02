!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over8.for
!
!
!     subroutine over8.
!
subroutine over8
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'synmac.ftn'
  include 'umdeck.ftn'
  include 'space2.ftn'
  !     %include  '//c/tsu/cables.ftn'
  real(8) l
  common /linemodel/ kexact, nsolve, fminsv, numrun, nphlmt
  common /linemodel/ char80, chlmfs(18)
  character(6) chlmfs        ! 9-phase as limit for lmfs test
  character(80) char80
  dimension infdli(1)
  equivalence  ( namebr(1),  infdli(1) )
  dimension wk1(1)
  equivalence ( semaux(1), wk1(1) )
  dimension vim(1)
  equivalence ( volt(1), vim(1) )
  dimension ispum(1)
  equivalence ( spum(1), ispum(1) )
  dimension integx(1)
  equivalence ( x(1), integx(1) ),   ( moncar(1), knt )
  character(6) tempbus
  !     1001 if ( iprsup .ge. 1 )
  if (iprsup .ge. 1) write (lunit6, 101) loopss(1), iv, it, tmax
101 format (' Top of "over8".   loopss(1) iv, it, tmax =', 3i8, e15.5)
  isecti = 400
  n7 = 1
  if (tmax .le. 0.0 .and. nchain .gt. lastov) go to 40014
  n7 = 2
  rewind lunit2
  call tapsav ( integx(1), lunit2, iv, n7 )
  n12 = 0
  call vecrsv ( volt(1), n12, n12 )
  n12 = ktrlsw(7)
  call vecrsv (  emtpc(1), n12, n7 )
  call vecrsv ( tr(1), n12, n7 )
  call vecrsv ( tx(1), n12, n7 )
  call vecrsv (  r(1), n12, n7 )
  n12 = ktrlsw(8)
  call vecisv (     nr(1), n12, n7 )
  call vecisv ( length(1), n12, n7 )
  if ( numum .eq. 0 .or. lastov .lt. nchain ) go to 6123
  if ( loopss(1)  .eq.  7766 )  go to 6121
  call vecisv( kssfrq(1), ntot, n7 )
  call vecisv( kpsour(1), ntot, n7 )
  go to 6123
6121 m7 = 0
  m8 = 8
  call vecrsv( volt(1), m8, m7 )
6123 if ( ktrlsw(5) .eq. 1  .and.  lastov .gt. nchain ) call vecrsv (cnvhst(1), lhist, n7)
  if ( loopss(1) .ne. 7766 )  go to 3489
  !     back from "over11" after final phasor solution, so exit:
  lastov = nchain
  nchain = 12
  lstat(22) = ibr
  go to 9600
  !     2nd or later pass skips renumbering of 3rd partition labcom:
3489 if ( lastov  .gt.  nchain )  go to 40014
  if (inonl.eq.0) go to 4141
  do i = 1, inonl
     k = nonlk(i)
     m = nonlm(i)
     l = iabs(k)
     nonlk(i) = norder(l)
     if( k .lt. 0 )  nonlk(i) = - nonlk(i)
     l = iabs(m)
     nonlm(i) = norder(l)
     if( m .lt. 0 )  nonlm(i) = - nonlm(i)
40104 end do
4141 do i = 1, ibr
     k = kbus(i)
     m = mbus(i)
     l = iabs(k)
     kbus(i) = norder(l)
     if( k .lt. 0 )  kbus(i) = - kbus(i)
     l = iabs(m)
     mbus(i) = norder(l)
     if( m .lt. 0 )  mbus(i) = - mbus(i)
40007 end do
  do i = 2, ntot
     ich2(i) = i
  end do
  do i = 2, ntot
3442 l = ich2(i)
     j = norder(l)
     if( j .eq. i )  go to 3456
     bus1 = bus(j)
     nsave = ktrans(j)
     n3 = ich2(j)
     bus(j) = bus(i)
     ktrans(j) = ktrans(i)
     ich2(j) = ich2(i)
     bus(i) = bus1
     ktrans(i) = nsave
     ich2(i) = n3
     go to 3442
3456 end do
  if( iprsup .gt. 1 ) write(lunit6, 73825)  kpartb, ntot
73825 format(//, ' Final renumbering maps.  kpartb= ', i5, 5x, 'ntot=', i3, /, 9x, 'i', 4x, 'bus(i)', 3x, 'norder(i)', 5x, &
           'ich1(i)', 5x, 'kode(i)')
  if( iprsup .gt. 1 ) write(lunit6, 73826)  ( i, bus(i), norder(i),   ich1(i), kode(i), i=1, ntot )
73826 format (i10, 4x, a6, 3i12)
  if (kswtch.lt.1) go to 40017
  do i = 1, kswtch
     ndx1 = lswtch + i
     k = kmswit(i)
     m = kmswit(ndx1)
     kmswit(i) = norder(k)
     kmswit(ndx1) = norder(m)
  end do
40017 if (kconst.lt.1) go to 40011
  do i = 1, kconst
     k = node(i)
     l = iabs(k)
     node(i) = norder(l)
     if( k .lt. 0 )  node(i) = - node(i)
     if ( iform(i) .ne. 18 )   go to 40009
     n6 = int(time1(i))
     time1(i) = norder(n6)
40009 end do
40011 if(nv.eq.0) go to 40013
  do i = 1, nv
     k = ibrnch(i)
     ibrnch(i) = norder(k)
     m = jbrnch(i)
40012 jbrnch(i) = norder(m)
  end do
40013 if (numsm .eq. 0 ) go to 40014
  !  transient renumbering of synchronous machine nodes
  j30 = 2
  do i = 1, numsm
     do iji = 1, 3
        j31 = j30 + iji
        ksma = ismdat( j31 )
99      ismdat( j31 ) = norder( ksma )
     end do
100  j30 = j30 + 30
  end do
40014 if (numum .eq. 0 .or. nchain .lt. lastov) go to 6666
  call frqchk
  n20 = 0
  if (istart .eq. 0 .and. loopss(10) .eq. 0) n20 = 1
  if (n20 .eq. 1 .and. initum .eq. 0) n20 = 2
  if (numum .gt. 0 .and. n20 .eq. 1) kill = 0
  if ( kill  .gt.  0 )   go to 9200
  m7 = 1
  call vecisv ( kssfrq(1), ntot, m7 )
  call vecisv ( kpsour(1), ntot, m7 )
6666 if ( nchain  .gt.  lastov )   go to 2236
  call vecrsv ( tclose(1), kswtch, n7 )
  n12 = -1
  if ( ktrlsw(5) .eq. 1 ) n12 = -2
  n15 = 0
  call vecrsv( volt(1), n12, n15 )
  !     preceding call to "frqchk" shows user-defined sources
  !     identified by subnetwork for use in following "umrenu",
  !     for cases having one or more u.m. (numum .gt. 0).
2236 if ( numum  .gt.  0  ) call umrenu(spum(iureac), spum(iugpar), spum(iufpar), spum(iuhist), spum(iuumrp), ispum(iunod1), &
       ispum(iunod2), ispum(iujclt), ispum(iujclo), ispum(iujtyp), ispum(iunodo), ispum(iujtmt), spum(iuhism), spum(iuomgm), &
       spum(iuomld), spum(iutham), spum(iuredu), spum(iureds), spum(iuflds), spum(iufldr), spum(iurequ), spum(iuflqs), &
       spum(iuflqr), ispum(iujcds), ispum(iujcqs), spum(iuflxd), spum(iuflxq), ispum(iunppa), spum(iurotm), ispum(iuncld), &
       ispum(iunclq), ispum(iujtqo), ispum(iujomo), ispum(iujtho), spum(iureqs), spum(iuepso), spum(iudcoe), ispum(iukcoi), &
       spum(iuvolt), spum(iuangl), ispum(iunodf), ispum(iunodm), ispum(iukumo), ispum(iujumo), spum(iuumou))
  if ( kill .gt. 0 )  go to 9200
  if ( istead  .eq.  0 )   go to 9500
  if ( fmaxfs  .eq.  0.0 )   go to 1413
  if ( knt  .eq.  1 )   go to 3224
  ibr = it1
  if ( fminfs  .le.  fmaxfs )   go to 3234
  if ( kexact .ne. 88333 )  go to 2240
  nsolve = nsolve + 1
  if ( nsolve .gt. 1  ) go to 2238
  fminfs = fminsv
  kodsem(1) = 0     ! kbrnum is replaced by '1', 2/21/90, thl.
  imodel(1) = 0
  kbus(1) = -2
  kodebr(1) = 0
  length(1) = nphlmt
  knt = 1
  lastov = 8
  nchain = 44
  go to 9600
2238 if ( numrun .eq. 3  )  go to 2240
  if ( numrun .eq. 2  .and. length(1) .le. 3 )  go to 2240
  nsolve = 0
  llbuff = -3333
  rewind  5
  call datain    !  go to datain to regenerate data input for lmfs t
  lastov = nchain
  nchain = 1    ! program generated 2nd & 3rd lmfs data
  go to 9600
2240 begmax(1) = 0.0
  lastov = nchain
  nchain = 20
  call runtym(d1,d2)
  flstat(3) = flstat(3) + d1
  flstat(4) = flstat(4) + d2
  flstat(7) = flstat(7) - d1
  flstat(8) = flstat(8) - d2
  tmax = fminfs
  go to 9600
3234 if ( delffs  .lt.  0.0 )   go to 3218
  fminfs = fminfs  +  delffs
  go to 3224
3218 fminfs = fminfs * (-delffs)
3224 do j = 1, kconst
     if ( iform(j)  .eq.  18 )   go to 3225
     sfreq(j) = fminfs
3225 end do
  if ( iprsup  .ge.  2 ) write (lunit6, 3227)  knt, ltlabl, fminfs, fmaxfs
3227 format (/, ' Begin next freq.     knt  ltlabl', 9x, 'fminfs', 9x, 'fmaxfs', /, 17x, 2i8, 2e15.6)
1413 if ( iprsup  .ge.  1 ) write (lunit6, 1417)  kconst, ibr, inonl, kswtch, istead, xopt, copt, twopi
1417 format (1x, '  kconst     ibr   inonl  kswtch  istead', 11x, 'xopt',  11x, 'copt', 10x, 'twopi', /, 1x, 5i8, 3e15.6)
  if ( iprsup .ge. 1 ) write (lunit6, 1419)  ( xoptbr(j), coptbr(j), j=1, ibr )
1419 format (' x/coptbr =', 10f12.2)
  !     before starting steady-state solution for initial conditions, we
  !     must find pi-equivalents for distributed branches.
  !     1425 itadd = it + 1
  itadd = it + 1
  !     next make sure of frequency separation within subnetworks
  !     for phasor solution by call to "frqchk" :
  call frqchk
  if ( kill  .gt.  0 )   go to 9200
  iprint=3
  !     400 k=1
  k = 1
  !     enter top of loop over branches (index "k") for pi-equiv:
401 n1=length(k)
  it2=iabs(n1)
  n15 = iabs ( kbus(k) )
  if ( n15  .eq.  1 )   n15 = iabs ( mbus(k) )
  xopt = xoptbr(k)
  copt = coptbr(k)
  omltem = 1000. / twopi
  omctem = 1000. * omltem
  if( xopt .gt. 0.0 ) omltem = xopt * tenm3 * twopi *omltem
  if( copt .gt. 0.0 ) omctem = copt * twopi * omctem
  n16 = iabs ( kssfrq(n15) )
  d2 = omltem / sfreq(n16)
  h3 = omctem / sfreq(n16)
  omega = twopi * sfreq(n16)
  if(kbus(k).gt.0) go to 411
  im1=imodel(k)
  if ( nsolve .lt. 1 )  go to 40002
  if ( k .ne. 1 )  go to 40002
  if ( kexact .eq. 88333 .and. fminfs .eq. fminsv .and.  nsolve .eq. 1 )  rewind lunit9
  do i = 1, it2
     read (lunit9) dum, ycharm, ychara, alpha, beta
     if ( iprsup .ge. 1 ) write(lunit6,*) 'f,ycharm,ychara,alpha,beta=', dum,ycharm,ychara,alpha,beta
     ypos = expz(alpha)
     yneg = 1.0 / ypos
     cosha = 0.5 * ( ypos + yneg)
     sinha = cosha - yneg
     sinhgr = sinha * cosz(beta)
     sinhgi = cosha * sinz(beta)
     d9 = 1.0 / ycharm
     volti(i) = d9 * ( cosz(ychara)*sinhgr + sinz(ychara)*sinhgi )
     voltk(i) = d9 * ( -sinz(ychara)*sinhgr + cosz(ychara)*sinhgi )
     cc1 = cosha * cosz(beta) - 1.
     cc2 = sinha * sinz(beta)
     d7 = volti(i) ** 2 + voltk(i) ** 2
     volt(i) = ( volti(i)*cc1 + voltk(i)*cc2 ) / d7
     vim(i+lsiz26) =  ( volti(i)*cc2 - voltk(i)*cc1 ) / d7
     if (iprsup .ge. 1 ) write(lunit6,*) ' for mode ', i,' pi-equiv. zseres, 1/2 yshunt=', volti(i), voltk(i), volt(i), &
          vim(i + lsiz26)
1234 end do
  n9sq = it2 * it2
  read (lunit9) (qfd(i),i=1,n9sq), (sfd(i),i=1,n9sq)
  if ( iprsup .ge. 1 ) write(lunit6,*) ' qfd and sfd read from lunit9.', (qfd(i),i=1,n9sq), (sfd(i),i=1,n9sq)
  n2 = 1
  go to 460
40002 length(k)=it2
  if ( iprsup .ge. 1 ) write (lunit6,*) ' over8.  k, imodel(k), it2, omega, h3, d2 =', k, imodel(k), it2, omega, h3, d2
  !      if ( kexact .eq. 88333  .and. nsolve .eq. 0)  kbrnum = k
  if ( kodsem(k) .eq. 0  .or. im1 .eq. -2 )  go to 5136
  if ( im1 .eq. -4 )  go to 1536
  it2 = iabs(kodebr(k))
  !  Formation of phase symmetric pi matrices for components reperesented
  !  by recursive convolution.  for lumped elements the steady-state modal
  !  values of the symmetric pi are stored in sconst(j+0 ... j+3).  for
  !  transmiddion lines, sconst(j+0 ... j+3) holds the modal series z and
  !  shunt y.  (not per unit length)
  !
  !     (zpi) = (s)*(diag(...sqrt(z/y)*sinh(sqrt(z*y))...))*(q)**(-1)
  !
  !     (ypi) = (q)*(diag(...sqrt(y/z)*tanh(sqrt(z*y)/2)...))*(s)**(-1)
  !
  !  s(q) is the mode to phase voltage(current) transformation matrix.
  !  note.  transpose(s) = q**(-1)
  if (iprsup .lt. 6) go to 8009
  n1 = int(absz(cik(k)) - 1.0)
  n5 = it2 * it2
  n3 = n1 + n5
  n4 = n3 + n5
  write (lunit6, 8001) cik(k)
8001 format (//, 5x, 'sfd(', f4.0, ') ...', /)
  do i = 1, it2
     n2 = i + n1
     write (lunit6, 8002) i, (sfd(j), j=n2, n3, it2)
8002 format (/, 2x, i5, 8(1x, e15.7), /, (7x, 8(1x, e15.7)))
     n2 = i + n3
     write (lunit6, 8003) (sfd(j), j=n2, n4, it2)
8003 format (7x, 8(1x, e15.7))
8004 end do
  write (lunit6, 8005) cik(k)
8005 format (///, 5x, 'qfd(', f4.0, ') ...', /)
  do i = 1, it2
     n2 = i + n1
     write (lunit6, 8002) i, (qfd(j), j=n2, n3, it2)
     n2 = i + n3
     write (lunit6, 8003) (qfd(j), j=n2, n4, it2)
8006 end do
8009 do i = 1, it2
     ii = i + k - 1
     length(ii) = it2
     n1 = indhst(ii)
     if (ci(ii) .gt. 0.0) n1 = n1 + 1
     if (cnvhst(n1 + 4) .gt. 0.0) go to 8700
     n2 = iabs(kodsem(ii)) + 1
     n3 = int(ci(ii))
     d1 = 0.0
     d2 = 0.0
     if (n3) 8300, 8400, 8200
8200 if (iprsup .ge. 9) write(lunit6, 8201) n1, n2, n3, sconst(n2), sconst(n2+1), sconst(n2+2), d1, d2
8201 format (' n1, n2, n3, sconst(n2 ... n2+2), d1, d2 = ', 3i10, /, 5(1x, e15.8))
     if (n3 .le. 0) go to 8250
     if (sconst(n2)) 8210, 8220, 8230
8210 d5 = sconst(n2 - 3)
     d6 = - sconst(n2 + 1)
     d7 = sconst(n2 - 2)
     d8 = - sconst(n2 + 2)
     go to 8240
8220 d5 = sconst(n2 + 1)
     d6 = 0.0
     d7 = sconst(n2 + 2)
     d8 = 0.0
     go to 8240
8230 d5 = sconst(n2 + 1)
     d6 = sconst(n2 - 3)
     d7 = sconst(n2 + 2)
     d8 = sconst(n2 - 2)
8240 n3 = n3 - 1
     n2 = n2 + 5
     d9 = d5 * d7 - d6 * d8
     d8 = d5 * d8 + d6 * d7
     d7 = d9
     d6 = d6 + omega
     d9 = expz( - d5 * cnvhst(n1 - 1))
     d10 = - d9 * sinz(d6 * cnvhst(n1 - 1))
     d9 = d9 * cosz(d6 * cnvhst(n1 - 1))
     d13 = d5 * d5 + d6 * d6
     d11 = (d9 * d5 + d10 * d6) / d13
     d12 = (- d9 * d6 + d10 * d5) / d13
     d1 = d1 + d7 * d11 - d8 * d12
     d2 = d2 + d7 * d12 + d8 * d11
     go to 8200
8250 d4 = d1 * d1 + d2 * d2
     d3 = d1 / d4
     d4 = - d2 / d4
     go to 8400
8300 d5 = 0.0
8310 if (iprsup .ge. 9) write (lunit6, 8311) n2, n3, sconst(n2), sconst(n2+1), d5, d6, d1, d2
8311 format ('  n2, n3, sconst(n2 ... n2+1), d5, d6, d1, d2 = ', 2i10, /, 6(1x, e15.8))
     if (n3 .ge. 0) go to 8320
     n3 = n3 + 1
     d6 = - omega * sconst(n2)
     d5 = sconst(n2 + 1) - d5
     d1 = d1 + d5 * cosz(d6)
     d2 = d2 + d5 * sinz(d6)
     d5 = sconst(n2 + 1)
     n2 = n2 + 2
     go to 8310
8320 d6 = - omega * tmax
     d1 = (d1 - d5 * cosz(d6)) / omega
     d2 = - (d2 - d5 * sinz(d6)) / omega
     d4 = d1 * d1 + d2 * d2
     d3 = d2 / d4
     d4 = - d1 / d4
8400 n3 = int(ck(ii))
     d1 = 0.0
     d2 = 0.0
     if (n3) 8500, 8700, 8420
8420 if (iprsup .ge. 9) write (lunit6, 8201) n1, n2, n3, sconst(n2), sconst(n2+1), sconst(n2+2), d1, d2
     if (n3 .le. 0) go to 8470
     if (sconst(n2)) 8430, 8440, 8450
8430 d5 = sconst(n2 - 2)
     d6 = - sconst(n2 + 1)
     d7 = sconst(n2 - 1)
     d8 = - sconst(n2 + 2)
     go to 8460
8440 d5 = sconst(n2 + 1)
     d6 = 0.0
     d7 = sconst(n2 + 2)
     d8 = 0.0
     go to 8460
8450 d5 = sconst(n2 + 1)
     d6 = sconst(n2 + 5)
     d7 = sconst(n2 + 2)
     d8 = sconst(n2 + 6)
8460 n3 = n3 - 1
     n2 = n2 + 4
     d11 = d5 * d7 - d6 * d8
     d12 = d5 * d8 + d6 * d7
     d6 = d6 + omega
     d9 = d5 * d5 + d6 * d6
     d1 = d1 + (d11 * d5 + d12 * d6) / d9
     d2 = d2 + (- d11 * d6 + d12 * d5) / d9
     go to 8420
8470 n3 = iabs(kodsem(ii))
     d1 = d1 + sconst(n3)
     d10 = d1 * d1 + d2 * d2
     d9 = d1 / d10
     d10 = - d2 / d10
     go to 8600
8500 d5 = 0.0
     if (iprsup .ge. 9) write (lunit6, 8311) n2, n3, sconst(n2), sconst(n2+1), d5, d6, d1, d2
     if (n3 .ge. 0) go to 8320
     n3 = n3 + 1
     d6 = - omega * sconst(n2)
     d5 = sconst(n2 + 1) - d5
     d1 = d1 + d5 * cosz(d6)
     d2 = d2 + d5 * sinz(d6)
     d5 = sconst(n2 + 1)
     n2 = n2 + 2
     go to 8520
8520 d5 = -omega * tmax
     n3 = iabs(kodsem(ii))
     d1 = (d1 - d5 * cosz(d6)) / omega
     d2 = sconst(n3) - (d2 - d5 * sinz(d6)) / omega
     d10 = d1 * d1 + d2 * d2
     d9 = d2 / d10
     d10 = - d1 / d10
8600 cnvhst(n1 + 0) = d9 * d9 - d10 * d10
     cnvhst(n1 + 1) = 2.0 * d9 * d10
     cnvhst(n1 + 2) = unity
     cnvhst(n1 + 3) = 0.0
     cnvhst(n1 + 4) = omega
     go to 8070
8700 continue
     if (absz(cnvhst(n1 + 4) - omega) .lt. 0.0001) go to 8020
     n2 = -kbus(ii)
     n3 = iabs(mbus(ii))
     write (lunit6, 8010) bus(n2), bus(n3), i, cnvhst(n1 + 4), omega
8010 format(//, ' Warning...  Steady state modal parameters for recursive-convolution component connecting nodes ', "'", a6, "'", ' and ', a6, "'", /, &
          13x, 'for mode ', i2, ' are determined at angular frequency of', e12.5, ' radians/sec.  The steady-state solution frequency', /, &
          13x, 'being used is ', e12.5, ' radians/sec.')
8020 if (kodsem(k) .gt. 0) go to 8030
     volt(i) = cnvhst(n1 + 0)
     volti(i) = cnvhst(n1 + 1) * d2
     voltk(i) = cnvhst(n1 + 2)
     ndx1 = lsiz26 + i
     vim(ndx1) = cnvhst(n1 + 3) * h3
     go to 8080
8030 d3 = cnvhst(n1+0) * cnvhst(n1+2) - cnvhst(n1+1) * cnvhst(n1+3)
     d4 = cnvhst(n1+0) * cnvhst(n1+3) + cnvhst(n1+1) * cnvhst(n1+2)
     d5 = sqrtz( d3 * d3  +  d4 * d4 )
     if (d3 .lt. 0.0) go to 8040
     d5 = sqrtz((d3 + d5) * onehaf)
     d6 = onehaf * d4 / d5
     go to 8050
8040 d6 = sqrtz(( -d3 + d5) * onehaf)
     if ( d4 .ge. 0.0) go to 18050
     dtemp = -d6
     d5 = onehaf * d4 / dtemp
     go to 8050
18050 d5 = onehaf * d4 / d6
8050 d4 = expz( d5 )
     d3 = d4 * cosz ( d6 )
     d4 = d4 * sinz ( d6 )
     d8 = cnvhst(n1+2) ** 2  +  cnvhst(n1+3) ** 2
     d7 = (cnvhst(n1+0)*cnvhst(n1+2) + cnvhst(n1+1)*cnvhst(n1+3)) / d8
     d8 = (-cnvhst(n1+0)*cnvhst(n1+3) + cnvhst(n1+1)*cnvhst(n1+2)) / d8
     d9 = sqrtz(d7 * d7  +  d8 * d8)
     if (d7 .lt. 0.0) go to 8060
     d9 = sqrtz((d7 + d9) * onehaf)
     d10 = onehaf * d8 / d9
     go to 8070
8060 d10 = sqrtz((-d7 + d9) * onehaf)
     if (d8 .lt. 0.0) d10 = - d10
     d9 = onehaf * d8 / d10
8070 d11 = d3 * d3  +  d4 * d4
     d5 = onehaf * (d3 - d3 / d11)
     d6 = onehaf * (d4 + d4 / d11)
     volt(i) = d9 * d5  - d10 * d6
     volti(i) = d9 * d6 + d10 * d5
     d3 = onehaf * (d3 + d3 / d11) - unity
     d4 = onehaf * (d4 - d4 / d11)
     d11 = volt(i) ** 2  + volti(i) ** 2
     voltk(i) = (volt(i) * d3  +  volti(i) * d4) / d11
     ndx1 = lsiz26 + i
     vim(ndx1) = (volt(i) * d4  -  volti(i) * d3) / d11
8080 if (iprsup .le. 6) go to 8090
     n2 = - kbus(ii)
     n3 = iabs(mbus(ii))
     write (lunit6, 8085) bus(n2), bus(n3), ii, n1, cnvhst(n1 + 0), cnvhst(n1 + 1), cnvhst(n1 + 2), cnvhst(n1 + 3), &
          cnvhst(n1 + 4), d3, d4, d5, d6, d7, d8, d9, d10, volt(i), volti(i), voltk(i), vim(ndx1)
8085 format(/, 5x, a6, ' to ', a6, 5x, 'br. index = ', i3, 5x, 'n1 = ', i3, /, 1x, 'cnvhst(n1) ... cnvhst(n1+4) =  ', 5(1x, e15.7), /, &
          1x, 'd3, d4, d5, d6, d7, d8 =  ', 6(1x, e15.7), /, 1x, 'd9, d10, volt, volti, voltk, vim =   ', 6(1x, e15.7))
8090 end do
  n1 = int(absz(cik(k)) - 1.0)
  n2 = n1 + it2 * it2
  n6 = 4 * it2
  do i = 1, it2
     n3 = n1 + i
     n4 = n2 + i
     n5 = 1
     ndx1 = lsiz26
     do j = 1, it2
        semaux(n5 + 0) = volt(j) * sfd(n3) - volti(j) * sfd(n4)
        semaux(n5 + 1) = volt(j) * sfd(n4) + volti(j) * sfd(n3)
        ndx1 = ndx1 + 1
        semaux(n5 + 2) = voltk(j) * qfd(n3) - vim(ndx1) * qfd(n4)
        semaux(n5 + 3) = voltk(j) * qfd(n4) + vim(ndx1) * qfd(n3)
        n3 = n3 + it2
        n4 = n4 + it2
        n5 = n5 + 4
8100 end do
     ii = i + k - 1
     nr(ii) = itadd
     do j = 1, i
        d3 = 0.0
        d4 = 0.0
        d5 = 0.0
        d6 = 0.0
        n3 = n1 + j
        n4 = n2 + j
        do l = 1, n6, 4
           d3 = d3 + semaux(l + 0) * sfd(n3) - semaux(l + 1) * sfd(n4)
           d4 = d4 + semaux(l + 0) * sfd(n4) + semaux(l + 1) * sfd(n3)
           d5 = d5 + semaux(l + 2) * qfd(n3) - semaux(l + 3) * qfd(n4)
           d6 = d6 + semaux(l + 2) * qfd(n4) + semaux(l + 3) * qfd(n3)
           n3 = n3 + it2
           n4 = n4 + it2
8110    end do
        if (itadd .gt. ldata) go to 9000
        tr(itadd) = d3
        tx(itadd) = d4 * d2
        r(itadd) = d5
        emtpc(itadd) = d6 * h3
        itadd = itadd + 1
8120 end do
     if (iprsup .lt. 6) go to 8130
     n3 = itadd - 1
     n4 = nr(ii)
     write (lunit6, 8125) n4, nr(ii), ii, (tr(j), tx(j), r(j), emtpc(j), j = n4, n3)
8125 format (//, 5x, 'n4 = ', i4, 5x, 'nr(ii) = ', i4, 5x, 'ii = ', i4, 5x, '(tr(j), tx(j), r(j), emtpc(j), j=n3, n4)  ....', /, (8(1x, e15.8)))
8130 end do
  it2 = 1
8140 if (cki(k) .lt. 0.0) go to 407
  k = k + 1
  go to 8140
1536 continue
  n1 = it2
  n2 = litype(k)
  !      if ( kexact .eq. 88333  .and. nsolve .eq. 0)  kbrnum = k
  if (im1 .ne. -4 )  go to 5137
  !      ktrlsw(5) = 1
  if ( iprsup  .ge.  1 ) write (lunit6, 50002)  omega
50002 format (' The following lines are frequency dependent parameters at ', f10.3, ' radians/sec.', /, &
           ' bus1', 7x, 'bus2', 14x, 'surge impedance', 20x, ' propagation function', 12x, 'travel time')
  do i = 1, it2
     j = i + k -1
     npz = int(cki(j))
     npa = int(ckkjm(j))
     n3 = kodsem(j)
     n8 = indhst(j)
     nhst = n8 + 15
     !      if ( cnvhst(nhst) .gt. 0.) go to 2015
     cnvhst(n8+1) = sconst(n3)
     cnvhst(n8+2) = 0.0
     gjl = 56789.0
     jgl = 0
     if ( iprsup .ge. 1) write(*,1990)
1990 format ('npz   ii   n4    sconst(n4)    sconst(n5)        cnvhst(n8+1)  cnvhst(n8+2)')
     do ii = 1, npz
        n4 = n3 + ii
        if( imodel(j) .eq. -4 ) go to 8015
        if ( absz(sconst(n4)) .lt. 0.1e17 ) go to 8015
        jgl = jgl + 1
        if (jgl .lt. 2 ) gjl = 56789.0
        if (jgl .ge. 2 ) gjl = 98765.0
        if (jgl .eq. 2 ) jgl = 0
        if ( absz(sconst(n4)) .ge. 0.1e15 .and. gjl .eq. 98765.0 ) go to 30001
        d1 = sconst(n4)
        dd1 = sconst(n4+1)
        n5 = npz + n4
        dd2 = sconst(n5)
        dd3 = sconst(n5+1)
        dd3 = dd3 + omega
        d3 = dd2 ** 2 + dd3 ** 2
        d1 = d1 * dd2 + dd1 * dd3
        cnvhst(n8+1)=cnvhst(n8+1)+d1*2/d3
        go to 30001
8015    d1 = sconst(n4)
        n5 = npz + n4
        dd2 = sconst(n5)
        d3 = dd2 ** 2 + omega ** 2
        cnvhst(n8+1)=cnvhst(n8+1)+d1*dd2/d3
        cnvhst(n8+2) = cnvhst(n8+2)-omega*d1/d3
        if ( iprsup .ge. 1) write(*,1991) npz,  ii,  n4, sconst(n4), sconst(n5), cnvhst(n8+1), cnvhst(n8+2)
1991    format (1x, i2, 2x, i3, 2x, i6, 2x, 4e26.9)
30001 end do
     cnvhst(n8+3) = 0.0
     cnvhst(n8+4) = 0.0
     jkl = 0
     if ( iprsup .ge. 1) write(*,1988)
1988 format(41hnpa   ii   n6    sconst(n6)    sconst(n7)      28h  cnvhst(n8+3)  cnvhst(n8+4))
     do ii = 1, npa
        n6 = n5 + ii
        n7 = n6 + npa
        if ( absz( sconst(n6) ) .ge. 1.e+13 ) go to 1989
        d1 = sconst(n6)
        dd2 = sconst(n7)
        d3 = dd2 ** 2 + omega ** 2
        cnvhst(n8+3) = cnvhst(n8+3) + d1*dd2/d3
        cnvhst(n8+4) = cnvhst(n8+4)-omega*d1/d3
        go to 1993
1989    jkl = jkl + 1
        if ( jkl .eq. 2 ) go to 5486
        akr = sconst(n6) / 1.e+15
        aki = sconst(n6+1) / 1.e+15
        apr = sconst(n7)   / 1.e+15
        api = sconst(n7+1) / 1.e+15
        dkr = apr * akr - api * aki
        dki = apr * aki + api * akr
        akr = dkr
        aki = dki
        dqq3 = apr ** 2 + ( omega + api ) ** 2
        ddd3 = ( akr * apr + aki  * (omega + api ) ) / dqq3
        ddd4 = ( aki * apr - akr * ( omega + api ) ) / dqq3
        dqq4 = apr ** 2 + ( omega - api ) ** 2
        ccc3 = ( akr * apr - aki * ( omega - api ) ) / dqq4
        ccc4 = ( akr * ( omega - api ) + aki * apr ) / dqq4
        cnvhst(n8+3) = cnvhst(n8+3) + ddd3 + ccc3
        cnvhst(n8+4) = cnvhst(n8+4) + ddd4 - ccc4
        !      cnvhst(n8+3) = cnvhst(n8+3) + ddd3
        !      cnvhst(n8+4) = cnvhst(n8+4) + ddd4
        if ( jkl .eq. 1 ) go to 1993
5486    jkl = 0
1993    if ( iprsup .ge. 1) write(*,1992) npa,  ii,  n6, sconst(n6), sconst(n6+1), sconst(n7),sconst(n7+1),cnvhst(n8+3), cnvhst(n8+4)
1992    format(1x,i2,2x,i3,2x,i6,2x, 6e19.9)
40001 end do
     go to 2022
     !     2015  nhst = indhst(k)
     nhst = indhst(k)
     n1 = nhst + 15
     d3 = cnvhst(n1+0) * cnvhst(n1+2) - cnvhst(n1+1) * cnvhst(n1+3)
     d4 = cnvhst(n1+0) * cnvhst(n1+3) + cnvhst(n1+1) * cnvhst(n1+2)
     d5 = sqrtz( d3 * d3  +  d4 * d4 )
     if (d3 .lt. 0.0) go to 2017
     d5 = sqrtz((d3 + d5) * onehaf)
     d6 = onehaf * d4 / d5
     go to 2018
2017 d6 = sqrtz(( -d3 + d5) * onehaf)
     if ( d4 .ge. 0.0) go to 2019
     dtemp = -d6
     d5 = onehaf * d4 / dtemp
     go to 8050
2019 d5 = onehaf * d4 / d6
2018 d4 = expz( d5 )
     d5 = onehaf * d4 / d6
     d4 = expz( d5 )
     d3 = d4 * cosz ( d6 )
     d4 = d4 * sinz ( d6 )
     d8 = cnvhst(n1+2) ** 2  +  cnvhst(n1+3) ** 2
     d7 = (cnvhst(n1+0)*cnvhst(n1+2) + cnvhst(n1+1)*cnvhst(n1+3)) / d8
     d8 = (-cnvhst(n1+0)*cnvhst(n1+3) + cnvhst(n1+1)*cnvhst(n1+2)) / d8
     d9 = sqrtz(d7 * d7  +  d8 * d8)
     if (d7 .lt. 0.0) go to 2020
     d9 = sqrtz((d7 + d9) * onehaf)
     d10 = onehaf * d8 / d9
     go to 2021
2020 d10 = sqrtz((-d7 + d9) * onehaf)
     if (d8 .lt. 0.0) d10 = - d10
     d9 = onehaf * d8 / d10
2021 d11 = d3 * d3  +  d4 * d4
     d5 = onehaf * (d3 - d3 / d11)
     d6 = onehaf * (d4 + d4 / d11)
     volt(i) = d9 * d5  - d10 * d6
     volti(i) = d9 * d6 + d10 * d5
     d3 = onehaf * (d3 + d3 / d11) - unity
     d4 = onehaf * (d4 - d4 / d11)
     d11 = volt(i) ** 2  + volti(i) ** 2
     voltk(i) = (volt(i) * d3  +  volti(i) * d4) / d11
     ndx1 = lsiz26 + i
     vim(ndx1) = (volt(i) * d4  -  volti(i) * d3) / d11
     if (iprsup .le. 6) go to 2022
     n2 = - kbus(ii)
     n3 = iabs(mbus(ii))
     write (lunit6, 2023) bus(n2), bus(n3), ii, n1, cnvhst(n1 + 0), cnvhst(n1 + 1), cnvhst(n1 + 2), cnvhst(n1 + 3), &
          cnvhst(n1 + 4), d3, d4, d5, d6, d7, d8, d9, d10, volt(i), volti(i), voltk(i), vim(ndx1)
2023 format(/,5x,a6,4h to ,a6,5x,12hbr. index = ,i3,5x,5hn1 = ,i3,/,1x,29hcnvhst(n1) ... cnvhst(n1+4) =  ,5(1x,e15.7),/, &
          1x,25hd3, d4, d5, d6, d7, d8 =  ,6(1x,e15.7),/, 1x,34hd9, d10, volt, volti, voltk, vim =   ,6(1x,e15.7)  )
2022 d4 = cnvhst(n8)
     d9 = omega * d4
     d1 = dcos(d9)
     dd2 =-dsin(d9)
     d3 = cnvhst(n8+3) * d1 - cnvhst(n8+4) * dd2
     d5 = cnvhst(n8+4) * d1 + cnvhst(n8+3) * dd2
     cnvhst(n8+3) = d3
     cnvhst(n8+4) = d5
     !
     xr1=1.0-d3
     xi1=-d5
     xr2=1.0+d3
     xi2=d5
     d=xr2**2+xi2**2
     br=(xr1*xr2+xi1*xi2)/d
     bi=(-xr1*xi2+xi1*xr2)/d
     !
     d=br**2+bi**2
     xr1=.5d0*(br/d-br)
     xi1=.5d0*(-bi/d-bi)
     n11 = iabs(kbus(j))
     n12 = iabs(mbus(j))
     j1 = n8 + 1
     j2 = n8 + 4
     if ( iprsup  .ge.  2 ) write (lunit6, 70001)  bus(n11), bus(n12), (cnvhst(l), l=j1, j2),  d4
70001 format(1h ,a6,5x,a6,5x,2f15.10,5x,2f15.10,5x,f15.10)
     koff1 =900
     koff2 = koff1 + isecti
     koff3  = koff2  + isecti
     koff4  = koff3  + isecti
     koff5  = koff4  + isecti
     koff6  = koff5  + isecti
     koff7  = koff6  + isecti
     koff8  = koff7  + isecti
     koff9  = koff8  + isecti
     koff10 = koff9  + isecti
     !
     !           (1/b-b)*yc/2
     !
     wk1(koff7+i)=xr1*cnvhst(n8+1)-xi1*cnvhst(n8+2)
     wk1(koff9+i)=xr1*cnvhst(n8+2)+xi1*cnvhst(n8+1)
     !
     !           b*yc
     !
     wk1(koff8+i)=br*cnvhst(n8+1)-bi*cnvhst(n8+2)
     ndx1 = lsiz26 + i
     wk1(koff10+i)=br*cnvhst(n8+2)+bi*cnvhst(n8+1)
     if ( iprsup .ge. 1) write (lunit6, 90001) i, wk1(koff7+i),wk1(koff9+i),wk1(koff8+i),wk1(koff10+i)
90001 format (" Marti's pi-equiv(r,x,g,b) printout in over8: mode",i10, 3x, 4e15.6)
11100 end do
  !  do 952 i=1, it2
  !  imodel(i) = -4
  ! 952   continue
  go to 640
  !   5136 if ( kexact .eq. 88333  .and. nsolve .eq. 0 )  kbrnum = k
5136 n1 = it2
  n2 = litype(k)
  if (im1 .ne. -2 )  go to 5137
  ktrlsw(5) = 1
  if ( iprsup  .ge.  1 ) write (lunit6, 20005)  omega
20005 format(44h the following lines are frequency dependent, 15h parameters at ,  f10.3,  13h radians/sec.  ,/, &
       5h bus1, 7x, 4hbus2, 14x, 15hsurge impedance, 20x, 21h propagation function, 12x, 11htravel time)
  do i = 1, it2
     j = i + k -1
     npz = int(cki(j))
     npa = ckkjm(j)
     n3 = kodsem(j)
     n8 = indhst(j)
     cnvhst(n8+1) = sconst(n3)
     cnvhst(n8+2) = 0.0
     do ii = 1, npz
        n4 = n3 + ii
        d1 = sconst(n4)
        n5 = npz + n4
        dd2 = sconst(n5)
        d3 = dd2 ** 2 + omega ** 2
        cnvhst(n8+1)=cnvhst(n8+1) + d1*dd2/d3
        cnvhst(n8+2) = cnvhst(n8+2) -omega * d1/d3
10003 end do
     cnvhst(n8+3) = 0.0
     cnvhst(n8+4) = 0.0
     do ii = 1, npa
        n6 = n5 + ii
        d1 = sconst(n6)
        n7 = n6 + npa
        dd2 = sconst(n7)
        d3 = dd2 ** 2 + omega ** 2
        cnvhst(n8+3) = cnvhst(n8+3) + d1*dd2/d3
        cnvhst(n8+4) = cnvhst(n8+4)-omega*d1/d3
10004 end do
     d4 = cnvhst(n8)
     d9 = - omega * d4
     d1 = cosz(d9)
     dd2 = sinz(d9)
     d3 = cnvhst(n8+3) * d1 - cnvhst(n8+4) * dd2
     d5 = cnvhst(n8+3) * dd2 + cnvhst(n8+4) * d1
     cnvhst(n8+3) = d3
     cnvhst(n8+4) = d5
     n11 = iabs(kbus(j))
     n12 = iabs(mbus(j))
     j1 = n8 + 1
     j2 = n8 + 4
     if ( iprsup  .ge.  2 ) write (lunit6, 10007)  bus(n11), bus(n12), (cnvhst(l), l=j1, j2),  d4
10007 format(1h ,a6,5x,a6,5x,2f15.10,5x,2f15.10,5x,f15.10)
     d9 = cnvhst(n8+3) ** 2 + cnvhst(n8+4) ** 2
     csihre = cnvhst(n8+3) * (1/d9 -1.) * onehaf
     csihim = - cnvhst(n8+4) * (1/d9 +1.) * onehaf
     d3 = 1 + d9 + 2. * cnvhst(n8+3)
     ctahre = ( 1. - d9 ) / d3
     ctahim = - 2. * cnvhst(n8+4) / d3
     volti(i) = cnvhst(n8+1) * csihre - cnvhst(n8+2) * csihim
     voltk(i) = cnvhst(n8+1) * csihim + cnvhst(n8+2) * csihre
     d8 = cnvhst(n8+1) ** 2 + cnvhst(n8+2) ** 2
     volt(i) = (ctahre * cnvhst(n8+1) + ctahim * cnvhst(n8+2))/d8
     ndx1 = lsiz26 + i
     vim(ndx1) = (-ctahre*cnvhst(n8+2) + ctahim*cnvhst(n8+1))/d8
     if ( iprsup .ge. 1) write (lunit6, 10009) i, volti(i),voltk(i),volt(i),vim(ndx1)
10009 format (" Marti's pi-equiv(r,x,g,b) printout in over8: mode",i10, 3x, 4e15.6)
10110 end do
  go to 460
5137 call equiv ( volti(1), voltk(1), volt(1), vim(lsiz26+1),ci(k), ck(k), cik(k), omega, n1 )
460 n3=itadd
  if ( iprsup .ge. 1 ) write (lunit6, 1460) it2, (volti(i), voltk(i), volt(i),vim(i+lsiz26), i = 1, it2)
1460 format ( 49h b4 do 464, volti,voltk, volt, and vim for i=1 to, i3,3hare, /, (1x, 8e15.6) )
  do i = 1, it2
     gus1=volti(i)
     gus2=voltk(i)
     gus3=gus1*gus1+gus2*gus2
     volti(i)=gus1/gus3
464  voltk(i)=-gus2/gus3
  end do
  ! *** z inverted and stored in volti & voltk
  if ( iprsup .ge. 1 ) write (lunit6, 1465) k,litype(k),it2,itadd,n3
1465 format (44h at 1465, k, litype(k), it2, itadd, n3, are , 10x, 5i10)
  n3tst = n3 + it2 * ( it2 + 1 ) / 2 -1
  if (n3tst .gt. ldata)   go to 9000
  do i = 1, it2
     do j = 1, i
        gus1=0.
        gus2=0.
        gus3=0.
        gus4=0.
        do ip = 1, it2
           nn3=(ip-1)*it2+n2-1
           jip=nn3+j
           iip=nn3+i
           h1=qfd(iip)
           h2=qfd(jip)
           hh3 = sfd(iip)
           h4 = sfd(jip)
           d1 = volti(ip) * h2 - voltk(ip) * h4
           dd2 = volti(ip) * h4 + voltk(ip) * h2
           !  evaluates the matrix product y = q * d * qt
           d8 = gus1 + h1*d1 - hh3*dd2
           d9 = gus2 + h1*dd2 + hh3*d1
           gus1 = d8
           gus2 = d9
           ndx1 = lsiz26 + ip
           d3 = volt(ip) * h2 - vim(ndx1) * h4
           d4 = volt(ip) * h4 + vim(ndx1) * h2
           d8 = gus3 + h1*d3 - hh3*d4
           d9 = gus4 + h1*d4 + hh3*d3
           gus3 = d8
           gus4 = d9
463     end do
        ! *** calculation of pi circuit parameters (only upper part of symmetric
        !     matrix is calculated)
        r(itadd)=gus3
        emtpc(itadd)=gus4
        tr(itadd)=gus1
        tx(itadd)=gus2
        if ( iprsup .ge. 1 ) write (lunit6, 1462) itadd,  gus3, gus4, gus1, gus2
1462    format ( 46h gus3, gus4, gus1 and gus2 at 1462 for itadd =, i3,3hare, 4e15.6 )
462     itadd=itadd+1
     end do
     nr(k)=itadd-i
461  k=k+1
  end do
  n4=itadd-1
  ll0 = 0
  if ( iprsup .ge. 1 ) write (lunit6, 3465) n3, ( tr(i), tx(i), i=1, n3)
3465 format ( 38h b4 call cxred8, tr and tx for i =1 to, i4, 4h are,/, (1x, 8e15.6) )
  call cxred8(tr(n3),tx(n3),it2,ll0)
  if ( iprsup .ge. 1 ) write ( lunit6, 2465 ) n3, (tr(i), tx(i), i=1,n3)
2465 format ( 41h after call cxred8, tr and tx for i =1 to, i4, 4h are,/, (1x, 8e15.6) )
  ! *** convert x & c to conform to xopt & copt options
  do i = n3, n4
     tr(i)=-tr(i)
     tx(i)=-tx(i)*d2
465  emtpc(i)=emtpc(i)*h3
  end do
  it2=0
  go to 407
  !
640 inoff2 = 2 * lbrnch
  inoff1 = lbrnch
  nq1=infdli(inoff2+k)
  koff13 = koff10 + isecti
  koff14 = koff13 + isecti
  koff15 = koff14 + isecti
  koff16 = koff15 + isecti
  koff17 = koff16 + isecti
  koff18 = koff17 + isecti
  koff19 = koff18 + isecti
  koff20 = koff19
  kqvv = infdli(inoff1+k)
  nphs2=it2*it2
  do kqv = 1, nphs2
     nteq = wk1(koff20+kqvv)
     xk(kqv)=sconst(nq1)
     xm(kqv)=0.d0
     if ( iprsup .ge. 1) write(*,5692)
5692 format(43hkqv nteq    nq1    nq2    nq3   sconst(nq1),42h   sconst(nq2)   sconst(nq3)       qr(kqv))
     do lq = 1, nteq
        nq2 = nq1 + lq
        nq3 = nq2 + nteq
        if ( absz( sconst(nq2) ) .ge. 1.e+13 ) go to 3000
        d = omega ** 2 + sconst(nq3) ** 2
        xk(kqv) =  xk(kqv) + sconst(nq2) * sconst(nq3) / d
        xm(kqv) =  xm(kqv) - sconst(nq2) * omega / d
        go to 3001
3000    jkl = jkl + 1
        if ( jkl .eq. 2 ) go to 3002
        akr = sconst(nq2) / 1.e+15
        aki = sconst(nq2+1) / 1.e+15
        apr = sconst(nq3)   / 1.e+15
        api = sconst(nq3+1) / 1.e+15
        dqq3 = apr ** 2 + ( omega + api ) ** 2
        ddd3 = ( akr * apr + aki  * (omega + api ) ) / dqq3
        ddd4 = ( aki * apr - akr * ( omega + api ) ) / dqq3
        xk(kqv) = xk(kqv) + 2*ddd3
        if ( jkl .eq. 1 ) go to 1993
3002    jkl = 0
3001    if ( iprsup .ge. 1) write(*,1333) kqv,nteq,nq1,nq2,nq3,sconst(nq1),sconst(nq2),sconst(nq3), xk(kqv)
1333    format ( 1x,i2,3x,i2,2x,i5,2x,i5,2x,i5, 4e14.5)
        nq2 = nq2 + 1
        nq3 = nq3 + 1
801  end do
     nq1 = nq1 + 2 * nteq + 1
     kqvv = kqvv + 1
609 end do
  if ( iprsup .ge. 1) write (lunit6,*) 'following are qr(k) by rows____________'
  if ( iprsup .ge. 1) write (lunit6,4515)   (  xk(j), j=1, nphs2 )
  if ( iprsup .ge. 1) write (lunit6,*) 'following are qi(k) by rows____________'
  if ( iprsup .ge. 1) write (lunit6,4516)   (  xm(j), j=1, nphs2 )
4515 format ( 7h+ qr-i:, 3e14.5 )
4516 format ( 7h+ qi-i:, 3e14.5 )
  !
  do ik = 1, nphs2
     if ( iprsup  .ge.  3 ) write (lunit6, 4160) nphs2, xk(ik), xm(ik)
4160 format ( 24hnphs2 & qr(ik) & qi(ik)=,i3, 2x, 2e15.6 )
933 end do
  !
  !        evaluate     ypi = q * b*yc * qt
  !                 zpi**-1 = q * (1/b-b)*yc/2 * qt
  !
  !     111  nn1 = it2 * ( it2 + 1 ) / 2
  nn1 = it2 * (it2 + 1) / 2
  nn2 = itadd - 1
  do iqy=1, nn1
     r(nn2+iqy) = 0.d0
     emtpc(nn2+iqy) = 0.d0
1008 end do
  do 1018 kqy=1,it2
     km1 = ( kqy - 1 ) * it2
     dyrk = wk1(koff8+kqy)
     dyik = wk1(koff10+kqy)
     ij=0
     do 1027 jqy =1, it2
        jk = km1 + jqy
        qrjk =  xk(jk)
        qijk =  xm(jk)
        do 1037 iqy = 1, jqy
           ij = ij + 1
           ikq = km1 + iqy
           qrik =  xk(ikq)
           qiik =  xm(ikq)
           br = qrik * qrjk - qiik * qijk
           bi = qrik * qijk + qiik * qrjk
           r(nn2+ij) = r(nn2+ij) + dyrk * br - dyik * bi
           emtpc(nn2+ij) = emtpc(nn2+ij) + dyrk * bi + dyik * br
1037    end do
1027 end do
1018 end do
  do iqy = 1, nn1
     tr(nn2+iqy) = 0.d0
     tx(nn2+iqy) = 0.d0
1007 end do
  do kqy = 1, it2
     km1 = ( kqy - 1 ) * it2
     dyrk = wk1(koff7+kqy)
     dyik = wk1(koff9+kqy)
     ij=0
     do jqy =1, it2
        jk = km1 + jqy
        qrjk =  xk(jk)
        qijk =  xm(jk)
        do iqy = 1, jqy
           ij = ij + 1
           ikq = km1 + iqy
           qrik =  xk(ikq)
           qiik =  xm(ikq)
           br = qrik * qrjk - qiik * qijk
           bi = qrik * qijk + qiik * qrjk
           tr(nn2+ij) = tr(nn2+ij) + dyrk * br - dyik * bi
           tx(nn2+ij) = tx(nn2+ij) + dyrk * bi + dyik * br
103     end do
102  end do
1017 end do
  it0 = it2 * ( it2 + 1 ) / 2
  if ( iprsup  .ge.  3 ) write(lunit6,5284)(i,tr(i),tx(i),r(i),emtpc(i), i = itadd,itadd+it0)
5284 format ( 7x,  3hrow,  13x,  2htr,  13x,  2htx,  14x, 1hr,  14x,  1hc  ,/,  ( i10,  4e15.5  ) )
  do ik = 1, it2
     nr(k) = itadd + ( ik - 1 ) * ik / 2
     k = k + 1
932 end do
  itadd = itadd + it0
  !
  go to 393
411 if(n1.lt.0) go to 407
  i=nr(k)
  if(i.lt.0) go to 407
  n2=i+it2*(it2+1)/2-1
  do l = i, n2
     emtpc(l) = emtpc(l) * onehaf
  end do
407 k=k+it2
393 if ( k .le. ibr ) go to 401
  n13 = 1
  if (ktrlsw(5) .eq. 1) call vecrsv (cnvhst(1), lhist, n13)
  i = it + 1
  j0 = itadd - 1
  if ( lastov .eq. nchain - 1  .or.  iprsup .ge. 1 ) write (lunit6, 47884)  i, j0
47884 format (   68h pi-equiv branches of distrib lines in tr, tx, etc. between limits    , 2i6 )
  if ( iprsup  .ge.  3 ) write (lunit6, 2584)  ( i, tr(i), tx(i), r(i), emtpc(i), i=1, j0 )
2584 format ( 7x,  3hrow,  13x,  2htr,  13x,  2htx,  14x, 1hr,  14x,  1hc  ,/,  ( i10,  4e15.5  ) )
  if(inonl.eq.0) go to 414
  if ( noutpr  .eq.  0 ) write(lunit6,406)
406 format(121h nonlinear and time-varying resistances ignored in steady state solution. nonlinear inductances included with linear part )
  d2=1000.0
  if( xopt .gt. 0.0 )  d2 = xopt * twopi
  if ( iprsup  .le.  1 )   go to 4265
  write (lunit6, 4261)  ( i, namenl(i), nltype(i), nonlk(i), nonlm(i), nonlad(i), nonle(i), ilast(i), vnonl(i),curr(i), anonl(i), &
       vzero(i), i=1, inonl )
4261 format ( /,  25h nonlinear element table.  ,/,36h row  namenl  nltype   nonlk   nonlm,24h  nonlad   nonle   ilast,10x, &
       5hvnonl, 11x, 4hcurr, 10x, 5hanonl, 10x, 5hvzero, /,( 1x, i3,  7i8, 4e15.6 ) )
  write (lunit6, 4262)  ( i, cchar(i), vchar(i), gslope(i),i=1, ichar )
4262 format ( /,  43h nonlinear element characteristics.     row, 10x, 5hcchar, 10x, 5hvchar, 9x, 6hgslope, /,( 35x, i8, 3e15.6 ) )
4265 iprint = 2
  do i = 1, inonl
     if( nltype(i) .gt. 0 )  go to 4269
     if ( nltype(i) .eq. -98   .or. nltype(i) .eq. -96 )  go to 4269
     if( anonl(i) .ge. 0.0 )  go to 413
     go to 4272
4269 j0 = nonle(i)
     if(j0.gt.0) go to 413
     if(vzero(i).eq.0.) go to 413
     if( anonl(i) .gt. 0.0 )  go to 4272
     kill = 14
     lstat(19) = 406
     flstat(16) = anonl(i)
     n1 = nonlk(i)
     n2 = iabs( nonlm(i) )
     bus1 = bus(n1)
     bus2 = bus(n2)
     go to 9200
4272 call ibrinc
     if( iprsup .ge. 3 ) write(lunit6, 65457)  ( k, kbus(k), mbus(k), nr(k), length(k), kodebr(k), k=1, ibr )
65457 format(  32h branch table integers at 65457.   ,/, ( 1x, 6i10 ) )
     lstat(19)= 4272
     if(ibr  .gt.lbrnch) go to 9000
     nr(ibr)  =-itadd
     length(ibr)  =1
     kbus(ibr)  =nonlk(i)
     mbus(ibr)  =iabs(nonlm(i))
     if ( nltype(i)  .ne.  -97     .and. nltype(i)  .ne.  -99 )   go to 65464
     l = nonlad(i)
     tr(itadd) = 1.0 / gslope(l)
     tx(itadd) = 0.0
     go to 65468
65464 tr(itadd) = 0.0
     tx(itadd)=anonl(i)/vzero(i)*d2
65468 emtpc(itadd) = 0.0
     itadd=itadd+1
413 end do
414 lastov = nchain
  nchain = nchain + 1
  lstat(23) = itadd - 1
  lstat(22) = ibr
  if ( fmaxfs .gt. 0.0  .and.  knt .gt. 1 )  go to 9600
  go to 8900
9500 if ( kconst  .lt.  lexct )   go to 1517
  !     temporary error stop --- unable to add another source.
  write (lunit6, 1514)
1514 format (   39h too many sources.  s.n. 1514 "frqchk". )
  call stoptp
1517 n13 = kconst + 1
  sfreq(n13) = 1.0
  call copyi ( n13, kssfrq(1), ntot )
  if( tmax .gt. -delta2 )  go to 1534
  kill = 74
  lstat(19) = 1517
  flstat(15) = tmax + delta2
  lstat(16) = kconst
  go to 9200
1534 lastov = nchain
  nchain = 12
8900 call runtym ( d1, d2 )
  flstat(1) = flstat(1) + d1
  flstat(2) = flstat(2) + d2
  flstat(3) = flstat(3) - d1
  flstat(4) = flstat(4) - d2
  go to 9600
9000 lstat(16) = iprint
  kill = 1
9200 lastov = nchain
  nchain = 51
  lstat(18) = 8
9600 if ( iprsup  .ge.  1 ) write (lunit6, 1786) nchain, kconst, kill, itadd, fmaxfs
1786 format ( 16h exit "over8".   , 38h nchain, kconst, kill, itadd, fmaxfs =, 4i8, e14.4 )
  !99999return
  return
end subroutine over8
!
!     function indblk.
!
function  indblk(tempbus)
  character*6  tempbus
  indblk = index ( tempbus, ' ' )
  return
end function indblk
!
!     subroutine frqchk.
!
subroutine frqchk
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     module called in two places by "over8" to determine which
  !     nodes are excited by which sources (subnetwork bounding),
  !     with error stop if there is frequency mixing.
  include 'blkcom.ftn'
  include 'labcom.ftn'
  if ( iprsup  .ge.  1 ) write (lunit6, 1428)    ntot, kconst, ibr, kswtch, it
1428 format (    17h top of "frqchk"., 40h    ntot  kconst     ibr  kswtch      it   ,/,  17x,  8i8  )
  !     first determine the phasor frequency of each network node:
  call move0 ( kssfrq(1), ntot )
  n13 = 0
  n11 = 0
  n14 = 0
  d12 = 0.0
  n16 = 0
  do i = 1, kconst
     n4 = iabs ( node(i) )
     kpsour(i) = i
     if ( kssfrq(n4)  .gt.  0 )        go to 1438
     if ( iabs(iform(i))  .ne.  14 )   go to 1438
     if ( tstart(i)  .eq.  5432. )   go to 1434
     if ( tstart(i)  .ge.  0.0 )       go to 1438
1434 kssfrq(n4) = i
     if ( iform(i+1)  .ne.  18 )   go to 1436
     n4 = node(i+1)
     kssfrq(n4) = i
1436 if ( n13  .eq.  0 )   n13 = i
     if ( d12  .eq.  0.0 )   d12 = sfreq(i)
     if ( sfreq(i)  .eq.  d12 )   go to 1438
     n16 = 1
1438 end do
  if ( n16  .eq.  0 )   go to 1480
1441 k = 1
  n8 = 0
1446 n6 = iabs( length(k) )
  n7 = k
  do j = 1, n6
     n1 = iabs( kbus(n7) )
     n2 = iabs( mbus(n7) )
     if ( kssfrq(n1)  .eq.  0 )   go to 1453
     if ( n1  .eq.  1 )           go to 1453
     n12 = kssfrq(n1)
     go to 1458
1453 if ( kssfrq(n2)  .eq.  0 )   go to 1456
     if ( n2  .eq.  1 )           go to 1456
     n12 = kssfrq(n2)
     go to 1458
1456 n7 = n7 + 1
  end do
  k = k + n6
  go to 1469
1458 d12 = sfreq(n12)
  do j = 1, n6
     n2 = 0
     n1 = iabs( kbus(k) )
1461 n16 = kssfrq(n1)
     if ( n1  .le.  1 )   go to 2465
     if ( n16  .le.  0 )   go to 1464
     if ( sfreq(n16)  .eq.  d12 )   go to 1462
     kill = 222
     n11 = n11 + 1
     if (n11 .gt. 1) go to 1462
     n5 = k
     n10 = n12
     n15 = iabs ( kbus(k) )
     n20 = iabs ( mbus(k) )
     bus1 = bus(n1)
     bus2 = bus(n2)
1462 if ( kpsour(n12)  .lt.  kpsour(n16) )   go to 1463
     kpsour(n12) = kpsour(n16)
     go to 2465
1463 kpsour(n16) = kpsour(n12)
     go to 2465
1464 kssfrq(n1) = n12
     n8 = n8 + 1
2465 if ( n2  .gt.  0 )   go to 1467
     n2 = 1
     n1 = iabs( mbus(k) )
     go to 1461
1467 k = k + 1
  end do
  if ( iprsup  .ge.  6 ) write (lunit6, 1468)  k, length(k), n6, n8, n12, d12
1468 format (  33h k, length(k), n6, n8, n12, d12 =, 5i6,  f10.2  )
1469 if ( k  .le.  ibr )   go to 1446
  if ( kswtch  .le.  0 )   go to 1475
  do j = 1, kswtch
     if ( adelay(j)  .eq.  -44444. )   go to 1470
     if ( tclose(j)  .ge.  0.0 )     go to 1474
     if ( adelay(j)  .eq.  44444. )  go to 1474
1470 n1 = iabs( kmswit(j) )
     ndx1 = lswtch + j
     n2 = iabs( kmswit(ndx1) )
     if ( n1  .eq.  1 )   go to 1474
     if ( n2  .eq.  1 )   go to 1474
     n16 = kssfrq(n1)
     n17 = kssfrq(n2)
     if ( n16+n17  .eq.  0 )   go to 1474
     if ( n16*n17  .eq.  0 )   go to 1473
     if ( sfreq(n16)  .eq.  sfreq(n17) )   go to 1471
     kill = 223
     n14 = n14 + 1
     if (n14 .gt. 1) go to 1471
     n9 = j
     bus3 = bus(n1)
     bus4 = bus(n2)
     n18 = kssfrq(n1)
     n19 = kssfrq(n2)
1471 if ( kpsour(n16)  .lt.  kpsour(n17) ) kpsour(n17) = kpsour(n16)
     if ( kpsour(n16)  .gt.  kpsour(n17) ) kpsour(n16) = kpsour(n17)
     go to 1474
1473 n8 = n8 + 1
     if ( kssfrq(n1)  .eq.  0 )   kssfrq(n1) = kssfrq(n2)
     if ( kssfrq(n2)  .eq.  0 )   kssfrq(n2) = kssfrq(n1)
1474 end do
1475 if ( iprsup  .ge.  5 ) write (lunit6, 1476)  n8,  ( kssfrq(j), j=1, ntot )
1476 format ( 39h additional frequency spreading.   n8 =,  i5, 42h      (kssfrq(j), j=1, ntot)  follows ....    ,/, ( 1x, 20i4 ) )
  if ( iprsup  .ge.  5 ) write (lunit6, 1478)  ( kpsour(j), j=1, kconst )
1478 format (  8h kpsour:, 20i5  )
  if ( n8  .gt.  0 )   go to 1441
1480 do i = 1, ntot
     if ( kssfrq(i)  .eq.  0 )   kssfrq(i) = -n13
1481 end do
  if ( iprsup  .ge.  2 ) write (lunit6, 1482)  ( kpsour(j), j=1, kconst )
1482 format (  8h kpsour:,  20i5  )
  if ( iprsup  .ge.  2 ) write (lunit6, 1484)  ( kssfrq(j), j=1, ntot )
1484 format ( 43h final  (kssfrq(j), j=1, ntot)  follows ... ,/,  ( 1x,  20i4 ) )
  do k=1, kconst
     if ( kpsour(k)  .lt.  0 )   go to 1497
     n16 = k
     n2 = kpsour(k)
     if ( k  .eq.  kconst )   go to 1494
     n7 = k + 1
     do j = n7, kconst
        if ( kpsour(j)  .ne.  n2 )   go to 1492
        kpsour(n16) = -j
        n16 = j
1492 end do
1494 kpsour(n16) = -k
1497 end do
  do k=1, kconst
     kpsour(k) = -kpsour(k)
  end do
  if ( iprsup  .ge.  2 ) write (lunit6, 1482)  ( kpsour(j), j=1, kconst )
  !     9200 if (kill .ne. 222) go to 9202
  if (kill .ne. 222) go to 9202
  lstat(19) = 1462
  lstat(14) = n5
  lstat(15) = n10
  n1 = n15
  n2 = n20
  go to 9204
9202 if (kill .ne. 223) go to 9204
  lstat(19) = 1471
  lstat(14) = n9
  bus1 = bus3
  bus2 = bus4
  lstat(15) = n18
  lstat(16) = n19
9204 if ( iprsup  .ge.  1 ) write (lunit6, 9207)
9207 format (  15h exit "frqchk".  )
  return
end subroutine frqchk
!
! subroutine cxred8.
!
subroutine cxred8(a,c,n,m)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     elimination of variables m+1,...n in symmetric complex matrix with
  !     a=real part, c=imaginary part. a and c are
  !     stored as triangle (1 element for 1.column,2 for 2.column etc.).
  !     result is reduced matrix in columns 1,...m in case of reduction
  !     (m unequal 0) or negative inverse matrix in columns 1,...n in case
  !     of inversion (m=0).
  dimension a(1),emtpc(1),b(30),d(30)
  j = n + 1
  w=1.0
  if(m.gt.0) w=-w
  ij=n*j/2
3 j=j-1
  if(j.eq.m) return
  h1=a(ij)
  g1=emtpc(ij)
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
  g2=emtpc(i)
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
6    emtpc(i)=emtpc(i)+x*g2+y*h2
  end do
  if(k.lt.j) go to 4
  i=ik+j
  a(i)=b(k)
  emtpc(i)=d(k)
  go to 4
  !                                   end i-loop
7 i=ij
  do l = 1, j
     i=i+1
     emtpc(i)=d(l)
8    a(i)=b(l)
  end do
  go to 4
  !                                   end k-loop
9 i=ij+k
  go to 5
end subroutine cxred8
!
! subroutine umrenu.
!
subroutine umrenu(reacl, gpar, fpar, hist,umcurp, nodvo1, nodvo2, jcltac, jclout,jtype,nodom, jtmtac, histom, omegm, omold, &
     thetam, reamdu, reamds, flxds, flxdr, reamqu, flxqs, flxqr, jcdsat, jcqsat, flxd, flxq, nppair, rotmom, ncld, nclq, &
     jtqout, jomout, jthout, reamqs, epsom, dcoef, kcoil, voltum, anglum, nodfum, nodmum, kumout, jumout, umoutp )
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  dimension  reacl(1), gpar(1), fpar(1), hist(1), umcurp(1)
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
  dimension  zths3(3,3),zths3i(3,3),fjm(4,4),fjv(4),gmat(6,7)
  dimension  dumma1(3,3), dumma2(3,3)
  common /umlocl/ n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n17, n18, n19, n20, d1, d2, d3, d4, d5, d6, d7, d8
  common /umlocl/ d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, lfim3, lfim4i, ncomcl, ncomum, kcld1, kclq1, kclf, nminum
  common /umlocl/ lopss1, lopss2, lopss4, lopss8, lopss9, lopss10, slip
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'space2.ftn'
  include 'umdeck.ftn'
  !     kconst is the entry of the source type 14 vector
  !     numsub is the entry of the subnetwork vector
  !     isubeg(numsub) is the first node of subnetwork numsub
  !     n15 is the entry of the compensation vector
  !     kbus(n15) and mbus(n15) are the pair of nodes of
  !                            compensation vector entry n15.
  !     ksubum stores the first subnetwork connected to um.
  !     kssfrq(n) is the entry kconst of the type 14 source table
  !       which is in the same subnetwork as that of node n.
  !     kpsour(kconst) is the next type 14 source (if any) which
  !       is in the same subnetwork as type 14 source kconst.
  !  **************** umrenu coding strategy *********************
  !   "first part" of umrenu code is entered twice if load-flow
  !       is requested (loopss(10) = 1 or 2). this is needed to
  !       let the load-flow do its work to adjust the sources.
  !       however, if um type-3 is present, the first part will
  !       be entered one additional pass prior to the two
  !       mentioned passes in order to find the thev parameters of
  !       the um type-3 excitation circuits (loopss(10) = 3).
  !   "second part" of umrenu code is entered twice if one of the
  !       um is an induction machine and prediction option is
  !       requested (loopss(9)=2) since norton equivalent of ind.
  !       mach. would be needed, otherwise it is only passed once.
  !   "slack bus" calculations (2 passes) are conducted to adjust
  !       the external excitation and mechanical sources so as to
  !       accommodate the um currents found in previous passes.
  !   emtp restore feature : before each reentrance of umrenu as
  !       well as after permanent exit of umrenu, the emtp
  !       r,l,c branche elements created in umdata are restored
  !       to the values as initialized in umdata. however, the
  !       branches created in umrenu are not restored, they will
  !       maintain  values as initialized at any desired pass
  !       through umrenu.
  !       the vectors kpsour and kssfrq which are used to scan
  !       the connectivity of the subnetworks, are also restored
  !       before each reentrance of umrenu. thus only the
  !       network configuration as determined in umdata will be
  !       observed by these two vectors.
  !   over11 is entered permanently after slack bus calculations.
  !   loopss(1) .gt. 0 = request to go back from over11 to over8
  !                      for multiple ss calculations.
  !           (on permanent exit of umrenu, set loopss(1) = -1,
  !            and after completion of this last ss calculation
  !            in over11, then there loopss(1) is set to 7766).
  !   loopss(2) = 3*ntot = request to solve thev variables, else 0
  !   loopss(3) = used to store first type 14 source set by um
  !   loopss(4) .gt. 0 = request to set slack buses and um
  !                      steady-state eqns are totally skipped.
  !             .eq. 3 = umrenu is never to be called again
  !   note :  all branches created in umrenu will be wiped
  !           out after total completion of ss-calculation.
  !           however, if option for non-compensated power
  !           coils is used, created branches are permanent.
  !   loopss(5) is to store the last branch which is created
  !     created permanently by umrenu. temporary branches are not
  !     included to loopss(5)
  !   loopss(6) is used to store kconst before additional sources
  !      are created during the slack bus calculations
  !   loopss(7) = used to store misc. data parameter "kssout"
  !   loopss(8) was set to 1 in umdata if user requested for
  !     the option of using the um prediction code (power coils)
  !   loopss(9) is set to 1 in first part of umrenu if loopss(8)
  !              was set to 1 and if one of um is an induct. mach.
  !       then in "second part" if loopss(9)=1, loopss(9) is set
  !          to 2 to request is for norton par calculations of
  !          ind. mach and dummy run for sm if sm also present.
  !          however : loopss(9) is set immediately to 3 if load-
  !          is requested, thus bypassing norton calculations.
  !       then at next pass if loopss(9)=2, loopss(9) is set to 3
  !         indicating final pass thru "second part" of umrenu,
  !         and in this pass the um power resistances are placed
  !         on the stator if um prediction is requested.
  !   loopss(10) is set to 1 if istep = -4567 , indicating request
  !         for load flow interfacing.
  !         loopss(10)=1 , pass through "first part" for preparing
  !                        um load-flow equivalent.
  !         loopss(10)=2 , pass through "first and second part"
  !                        for processing load-flow output and
  !                        preparing ss-calculations.
  !                        on completion of this pass, loopss(10)
  !                        is reset to 0 on exit of "first pass".
  !         loopss(10)=3 , if ld-flow with um type-3 is requested.
  !                        this results in an additional pass thru
  !                        umrenu prior to the passes mentioned as
  !                        passes with loopss(10) = 1 and 2.
  !                        after completion of this pass with
  !                        loopss(10)=3, we reset loopss(10) to 1
  !   loopss(11) is used for passing "next" in over 5,6,7,8.
  !   loopss(12) is used for temporary storage of loopss(5) if
  !      "first part" needs to be entered twice
  !   istart .eq. 0 = run has not gone through second part code
  !      istart is used to count the number of umrenu passes.
  !   initum .eq. 0 = no steady state initialization
  !               1 = user's request for ss initialization
  !   hist(kcl) is used temporarily in first and second part of
  !        umrenu to store address ibr of power resist. branche.
  !   hist(kcl+1) is borrowed to store numsub as needed for ld-
  !        flow processing with um type-3. it is released after
  !        umrenu is reentered when the ld-flow is completed.
  !   hist(kcl+2) is borrowed to store kconst of power coil source
  !        if the load-flow involves a um type-3 .
  !   fpar(kcl) is used to store address ibr of leakage
  !        inductances (initialized in umdata).
  !   fpar(kcl+1) is used to store first source kconst which
  !      represents an im excitation coil (initilized in umdata).
  !   fpar(kcl+2) is used to store it address of the speed
  !      capacitor (initialized in umdata)
  !   fpar(kcl+3) was initialized in umdata and is - kconst of
  !      the exciter torque source.
  !   fpar(kcl+4) and fpar(kcl+5) are both initialized in umdata
  !      and used to store resp the exciter mass and field nodes.
  !   jcltac(kcl and kcl+1) will be borrowed to store
  !      respectively 2 potential um's connected to the same
  !      mechanical network as the um with kcoil(jm)=kcl
  !   jtmtac(jm) was initialized in umdata to the negative value
  !      of the node nr of the speed capacitor (thus if initum=1).
  !      it will be reset to zero here after its use.
  !.  entrance code of umrenu ************************************
  !     300 if (initum .eq. 0) go to 305
  if (initum .eq. 0) go to 305
  if (istep .eq. -4567) loopss(10) = 1
  loopss(1) = 1
305 lfim3 = 0
  if ( iprsup  .ge. 1 ) write (lunit6,307) numum,ncltot,numout,istart,kconst,ibr, loopss(1),loopss(2),loopss(4),loopss(8), &
       loopss(9), loopss(10)
307 format ( /,   18h begin  "umrenu" ., 48h   numum  ncltot  numout  istart  kconst     ibr, &
         49h  lopss1  lopss2  lopss4  lopss8  lopss9  lopss10                               ,/, 18x,11i8,1x,i8)
  if (loopss(4) .gt. 0) go to 13100
  !.  ....... code for um steady-state initialization ............
  call umrnu2(  reacl,gpar,fpar, hist,umcurp,nodvo1,jtype,nodom, jtmtac,thetam, imach,reamdu, reamqu,nodvo2, &
       nppair,rotmom, ncld,jcltac, kcl,nclq, epsom,dcoef,kcoil,voltum, anglum,nodfum, nodmum, umoutp )
  if (istart .gt. 0) goto 7000
  if (initum .eq. 0) goto 14000
  if (lfim3 .eq. 1) goto 7000
  !  now go out of module to calculate currents for um power coils
  !     2400 go to 13500
  go to 13500
  !.  second part of code : umsseq = steady state equations ******
  !   this part of code is entered only if istart .ge. 1
7000 nclout = loopss(3) + 1
  !  storing loopss(9) in case of um type-3 presence :
  if (lfim3 .ne. 1) go to 7060
  lpss9 = loopss(9)
  loopss(9) = 2
  go to 7090
  !  normal control of loopss(9) :
7060 if (loopss(9) .eq. 2) loopss(9) = 3
  if (loopss(9) .eq. 1) loopss(9) = 2
  !.  start machine do-loop **************************************
7090 do jm = 1,numum
     kcl = kcoil(jm)
     numibr = int(hist(kcl))
     lfim3i = 0
     if (lfim3 .ne. 1) go to 7098
     if (jtype(jm) .eq. 3) lfim3i = 1
     if (lfim3i .eq. 0) go to 13050
     if (iprsup .ge. 1) write(lunit6,7096) jm
7096 format(/,38h *************************************,42h norton equivalent of um type-3 excitation, &
          36h circuits for ld-flow with um number,i5,1h:)
     if (iprsup .ge. 1) write (lunit6,7097)
7097 format(20h *******************,5x,3hibr,8x,6htr(it),8x,6htx(it),2x,6hkconst,9x,5hsfreq,9x,5hcrest, &
          9x,5htime1,9x,5htstop)
     go to 8002
7098 if (iprsup .ge. 1) write (lunit6,7100) jm
7100 format(/,38h *************************************,24h changes to created emtp, &
          42h elements for steady-state calculations of,10h um number,i4,2h :)
     if (iprsup .ge. 1) write (lunit6,7110)
7110 format(20h ********** changed:,5x,3hibr,8x,6htr(it),8x,6htx(it),2x,6hkconst,9x,5hsfreq,9x,5hcrest, &
          9x,5htime1,9x,5htstop)
     !  in case of sm type-59 data input
     ntyp59 = 0
     if (jtype(jm) .ne. 13) go to 8002
     ntyp59 = 1
     jtype(jm) = 1
     inpust = inpu
     inpu = 0
8002 imach = 1
     if (jtype(jm) .lt. 3) imach = 0
     if (jtype(jm) .gt. 7) imach = 0
     if (imach .eq. 1) slip = voltum(jm)/100.0
     zthrr = 0.0
     zthri = 0.0
     do n1 = 1, 40
        vinp(n1) = 0.0
        zthevs(n1) = 0.0
8003    umcur(n1) = 0.0
     end do
     do n1 = 1,3
        do n2 = 1,3
           zths3(n1,n2) = 0.0
8004       zths3i(n1,n2) = 0.0
        end do
     end do
     !  no source initialization for sm if norton calculations are
     !    requested for im (this is of course for multi-mach case):
     if (loopss(9) .ne. 2) go to 8009
     if (imach .ne. 0) go to 8009
     !     8005 do 8008 n1 = 1,3
     do 8008 n1 = 1,3
        n2 = n1 + kcoil(jm) - 1
        if (nodvo1(n2) .eq. nodvo2(n2)) go to 8008
        if (nodvo1(n2) .ne. 1) nclout = nclout + 1
        if (nodvo2(n2) .ne. 1) nclout = nclout + 1
8008 end do
8009 do n1 = 1,10
8010    con(n1) = 0.0
     end do
     n1 = jtype (jm)
     if (n1 .gt. 12) go to 14000
     go to (8011,8012,8011,8011,8012,8013,8013,8014,8014,8014,8014,8014), n1
8011 con(3) = 1.0
     go to 8020
8012 con(2) = 1.0
     go to 8020
8013 con(1) = 1.0
     go to 8020
8014 con(8) = 1.0
8020 seltat = rotmom(jm)
     if (inpu .eq. 1) seltat = 1.0
     if (jtype(jm) .le. 7) go to 8100
     if (jtype(jm) .gt. 12) go to 8100
     seltat = omegm(jm) * nppair(jm)
     !.  coil markings
8100 kcld1 = kcl + 3
     kclde = kcld1 + ncld(jm) - 1
     kclq1 = kclde + 1
     kclqe = kclq1 + nclq(jm) - 1
     kcle = kcl + 2 + ncld(jm) + nclq(jm)
     ncl = 3 + ncld(jm) + nclq(jm)
     if (jtype(jm) .eq. 4) kcle = kcle + 1
     if (jtype(jm) .eq. 4) ncl = ncl + 1
     !.  sm and dm equations code ***********************************
     !.  input interface elect netw with sm and dm power side *******
     if (imach .eq. 1) go to 10000
     !  make leakage inductance zero for this next round of ss
     !   calculation if loopss(9)=2 since otherwise it would be
     !   restored to values of umdata (see comments near fortran
     !   stmt 2070)
     if (loopss(9) .ne. 2) go to 8204
     d1 = epsiln * 1.0d+3
     if (xopt .ne. 0.0) d1 = epsiln * twopi * xopt
     do n10 = 1,3
        n1 = kcl - 1 + n10
        if (nodvo1(n1) .eq. nodvo2(n1)) go to 8203
        n11 = int(fpar(kcl) + n10 - 1)
        n2 = - nr(n11)
        if (n2 .lt. 0) n2 = - n2
        tx(n2) = d1
        if (iprsup .ge. 1) write(lunit6,8860) n11,tr(n2),tx(n2)
8203 end do
     go to 9045
8204 d1 = anglum(jm) * twopi/360.0
     dumvec(1) = d1 * con(3)
     dumvec(2) = (dumvec(1) - twopi/3.0) * con(3)
     dumvec(2) = dumvec(2) + d1 * con(2)
     dumvec(3) = (dumvec(1) + twopi/3.0) * con(3)
     dumvec(3) = dumvec(3) + (dumvec(2) + twopi/4.0)*con(2)
     d2 = epsiln
     n1 = nodvo1(kcl+2)
     n2 = nodvo2(kcl+2)
     if (n1 .ne. 1 .and. n2 .ne. 1) d2 = epsiln * 1.0d+3
     do n10 = 1,3
        d1 = dumvec(n10)
        n11 = n10 + kcl - 1
        n1 = nodvo1(n11)
        n1 = norder(n1)
        n2 = nodvo2(n11)
        n2 = norder(n2)
        vinp(n10) = 0.0
        vinp(n10 + 20) = 0.0
        umcur(n10) = 0.0
        umcur(n10+20) = 0.0
        if (n1 .eq. n2) go to 8210
        vinp(n10) = voltum(jm) * cosz(d1)
        vinp(n10+20) = voltum(jm) * sinz(d1)
        umcur(n10) = vinp(n10) - solr(n1) + solr(n2)
        umcur(n10) = umcur(n10)/d2
        umcur(n10+20) = vinp(n10+20) - soli(n1) + soli(n2)
        umcur(n10+20) = umcur(n10+20)/d2
8210 end do
     !     8300 if (con(1) .eq. 1) go to 8800
     if (con(1) .eq. 1) go to 8800
     if (con(8) .eq. 1) go to 8800
     d1 = vinp(1) + vinp(2) + vinp(3)
     d2 = vinp(21) + vinp(22) + vinp(23)
     d1 = d1 * d1
     d1 = d1 + d2 * d2
     d1 = sqrtz(d1)
     if (d1 .lt. epsiln) go to 8800
     write (lunit6,8310) jm
8310 format( /, 36h warning : umbalanced electric power, 28h network elements or sources, 18h connected to um -, i3, 16h.   steady-state, &
          28h solution will not be exact.,/)
     !.  calculation of power side pos sequence input volt and curr:
8800 d10 = con(1) + con(4)
     vinp(1) = con(3)*vinp(1)+con(2)*vinp(2)+d10*vinp(3)
     vinp(21) = con(3)*vinp(21)+con(2)*vinp(22)+d10*vinp(23)
     umcur(1) = con(3)*umcur(1)+con(2)*umcur(2)+d10*umcur(3)
     umcur(21) = con(3)*umcur(21)+con(2)*umcur(22)
     umcur(21) = umcur(21) + d10*umcur(23)
     !.  including mach resistances and restore leakage inductances
     !       to power network; in case of compensated power coils
     !       the resistances are kept in the um modules.
     d19 = vinp(1)
     d20 = vinp(21)
     d17 = reacl(kcl+2)
     if (d17 .gt. reacl(kcl+1)) d17 = reacl(kcl+1)
     if (jtype(jm) .gt. 7) go to 8840
     d18 = d17 * seltat
     d19 = d19 - d18 * umcur(21)
     d20 = d20 + d18 * umcur(1)
8840 n11 = - nr(numibr)
     if (n11 .lt. 0) n11 = - n11
     d1 = 1.0/tr(n11)
     if (loopss(8) .eq. 1) d1 = gpar(kcl+2)
     d11 = d19*d1 + umcur(1)
     d12 = d20*d1 + umcur(21)
     d10 = d11*d11 + d12*d12
     d10 = sqrtz(d10)
     d1 = d12/d11
     d1 = atanz(d1)
     !     8850 if (d11 .lt. 0.0) d1 = d1 + twopi/2.0
     if (d11 .lt. 0.0) d1 = d1 + twopi / 2.0
     d2 = con(2)*d1 + con(3)*(d1 - twopi/3.0)
     d3 = con(2)*(d1 + twopi/4.0) + con(3)*(d1 + twopi/3.0)
     d3 = d3 + con(1)*d1
     do n1 = 1, 3
        n2 = n1 + kcl - 1
        n11 = 3 * (jm - 1)
        n13 = 3 * (numum + jm - 1)
        if (nodvo1(n2) .eq. nodvo2(n2)) go to 8510
        if (loopss(8) .ne. 1) go to 8870
        n12 = numibr - 3 + n1
        n10 = - nr(n12)
        if (n10 .lt. 0) n10 = - n10
        tr(n10) = 1.0/gpar(kcl+2)
        if (iprsup .ge. 1) write(lunit6,8860) n12,tr(n10),tx(n10)
8860    format(20h ********** changed:,2x,i6,2e14.5)
8870    d15 = d1
        if (n1 .eq. 2) d15 = d2
        if (n1 .eq. 3) d15 = d3
        if (nodvo1(n2) .eq. 1) go to  8500
        crest(nclout) = d10
        time1(nclout) = d15
        if (iprsup .ge. 1) write(lunit6,8862) nclout,sfreq(nclout),crest(nclout),time1(nclout),tstop(nclout)
8862    format(20h ********** changed:,38x,i6,4e14.5)
        nclout = nclout + 1
8500    if (nodvo2(n2) .eq. 1) go to 8505
        crest(nclout) = - d10
        time1(nclout) = d15
        if (iprsup .ge. 1) write(lunit6,8862) nclout,sfreq(nclout),crest(nclout), time1(nclout),tstop(nclout)
        nclout = nclout + 1
8505    n3 = int(fpar(kcl) + n1 - 1)
        n4 = - nr(n3)
        if (n4 .lt. 0) n4 = - n4
        tx(n4) = d17 * 1.0d+3
        if (xopt .ne. 0.0) tx(n4) = d17 * twopi * xopt
        if (iprsup .ge. 1) write(lunit6,8860) n3,tr(n4),tx(n4)
8510 end do
     !   um domain pos sequence volt and current :
     !     9000 d10 = con(3)*sroot3 + con(2)*sroot2 + con(1) + con(8)
     d10 = con(3)*sroot3 + con(2)*sroot2 + con(1) + con(8)
     vinp(1) = d10 * vinp(1)
     vinp(21) = d10 * vinp(21)
     umcur(1) = d10 * umcur(1)
     umcur(21) = d10 * umcur(21)
     if (jcdsat(jm) .ne. 5) go to 9002
     reamqu(jm) = reamdu(jm)
     reamqs(jm) = reamds(jm)
     flxqr(jm) = flxdr(jm)
     flxqs(jm) = flxds(jm)
9002 cdsat = 0.0
     cqsat = 0.0
     flxda = 0.0
     flxqa = 0.0
     flxdrr = 0.0
     flxqrr = 0.0
     sflxd = 1.0
     sflxq = 1.0
     n2 = 0
     !.  dm excitation code *****************************************
     d2 = 1.0/gpar(kcl+2)
     thetam(jm) = twopi/(4.0*nppair(jm))
     vinp(1) = vinp(1) + d2*umcur(1)
     !   storing umcur and vinp from emtp network :
9004 d11 = umcur(1)
     d12 = umcur(21)
     d13 = vinp(1)
     d14 = vinp(21)
     umcur(2) = 0.0
     umcur(3) = d11
     if (jtype(jm) .gt. 7) go to 9006
     !.  sm excitation code *****************************************
     d1 = (1.0 - cqsat)*reamqu(jm) + cqsat*reamqs(jm)
     d1 = (d1 + reacl(kcl+2)) * seltat
     d13 = d13 - d1*d12
     d14 = d14 + d2*d12 + d1*d11
     d3 = d14/d13
     d3 = atanz(d3)
     if (d13 .lt. 0.0) d3 = d3 + twopi/2.0
     d8 = (d13*d13 + d14*d14)/2.0
     d8 = sqrtz(d8)
     d7 = cqsat * sflxq * flxqa * seltat
     d7 = d7 + (1.0 - cqsat) * sflxq * flxqrr * seltat
     if (d7 .eq. 0.0) go to 9005
     d6 = d8*d8 - d7*d7
     d6 = sqrtz(d6)
     d6 = - d7/d6
     d6 = atanz(d6)
     d3 = d3 - d6
     d8 = d8 * cosz(d6)
9005 d4 = cosz(d3)
     d5 = sinz(d3)
     thetam(jm) = (d3 + twopi/4.0)/nppair(jm)
     umcur(3) = (d4*d11 + d5*d12)/sroot2
     umcur(2) = (d4*d12 - d5*d11)/sroot2
9006 d1 = (1.0 - cdsat)*reamdu(jm) - (1.0 - cqsat)*reamqu(jm)
     d1 = d1 + cdsat*reamds(jm) - cqsat*reamqs(jm)
     d1 = d1 * umcur(2)
     d3 = (reacl(kcl+1) - reacl(kcl+2)) * umcur(2)
     d1 = d1 + d3
     umcur(4) = d8/seltat - cdsat*sflxd*flxda - d1
     umcur(4) = umcur(4) - (1.0 - cdsat)*sflxd*flxqrr
     d3 = (1.0 - cdsat)*reamdu(jm) + cdsat*reamds(jm)
     umcur(4) = umcur(4)/d3
     vinp(4) = umcur(4)/gpar(kcld1)
     if (n2 .gt. 0) go to 9015
     !   saturation check :
     n1 = jcdsat(jm) + jcqsat(jm)
     if (n1 .eq. 0) go to 9015
     if (inpu .eq. 2) go to 9009
     flxdr(jm) = 0.0
     flxqr(jm) = 0.0
     go to 9010
     !  if remanent flux option is requested (inpu = 2) :
9009 if (flxdr(jm) .lt. 0.0) flxdr(jm) = - flxdr(jm)
     if (flxqr(jm) .lt. 0.0) flxqr(jm) = - flxqr(jm)
9010 if (jcdsat(jm) .ne. 5) go to 9011
     flxdrr = flxdr(jm)
     flxqrr = flxdrr
     go to 9015
9011 flxdrr = flxdr(jm)
     flxqrr = flxqr(jm)
9015 d10 = umcur(2) + umcur(4)
     d15 = umcur(3)
     if (n1 .eq. 0) go to 89028
     if (jcdsat(jm) .eq. 5) go to 9016
     if (n2 .eq. 0) go to 9020
     go to 89028
     !  code for the case of total saturation (jcdsat = 5) :
9016 curmt = d10*d10 + d15*d15
     if (curmt .eq. 0.0) go to 89028
     curmt = sqrtz(curmt)
     d19 = d10 * flxds(jm)/curmt
     d20 = d15 * flxqs(jm)/curmt
     if (d19 .lt. 0.0) d19 = - d19
     if (d20 .lt. 0.0) d20 = - d20
     if (reamdu(jm) .eq. 0.0) go to 9018
     d18 = reamds(jm)/reamdu(jm)
     flxda = d19 + d18*(flxdrr - d19)
9018 flxqa = d20 + d18*(flxqrr - d20)
     if (n2 .gt. 0) go to 89028
     go to 9027
     !  code for the case of non-total saturation :
9020 if (jcdsat(jm) .eq. 0) go to 9024
     if (reamdu(jm) .eq. 0.0) go to 9024
     d11 = flxds(jm)/reamdu(jm)
     d12 = - d11
     if (d10 .gt. d11) cdsat = 1.0
     if (d10 .lt. d12) cdsat = 1.0
     flxda = (reamds(jm)/reamdu(jm)) * (flxdrr - flxds(jm))
     flxda = flxda + flxds(jm)
9024 if (jcqsat(jm) .eq. 0) go to 9027
     d16 = flxqs(jm)/reamqu(jm)
     d17 = - d16
     if (d15 .gt. d16) cqsat = 1.0
     if (d15 .lt. d17) cqsat = 1.0
     flxqa = (reamqs(jm)/reamqu(jm)) * (flxqrr - flxqs(jm))
     flxqa = flxqa + flxqs(jm)
9027 n2 = 1
     if (d10 .lt. 0.0) sflxd = - 1.0
     if (d15 .lt. 0.0) sflxq = - 1.0
     d1 = cdsat + cqsat
     if (d1 .ne. 0.0) go to 9004
     !   calculations of main fluxes :
89028 if (cdsat .eq. 1.0) go to 89030
     flxd(jm) = reamdu(jm) * d10 + sflxd * flxdrr
     go to 89032
89030 flxd(jm) = reamds(jm) * d10 + sflxd * flxda
89032 if (cqsat .eq. 1.0) go to 89034
     flxq(jm) = reamqu(jm) * d15 + sflxq * flxqrr
     go to 9028
89034 flxq(jm) = reamqs(jm) * d15 + sflxq * flxqa
     !.  output for um = sm and dm :
9028 umcur(1) = 0.0
     if (jtype(jm) .gt. 7) go to 9029
     omegm(jm) = seltat/nppair(jm)
9029 omold(jm) = omegm(jm)
     do n1 = 1, 3
        n2 = kcl - 1 + n1
9030    hist(n2) = umcur (n1)
     end do
     hist(kcld1) = - umcur(4)
     !   zeroing of internal um leakage since leakage inductances
     !        are moved to the emtp network.
     d17 = reacl(kcl+2)
     if (reacl(kcl) .gt. d17) reacl(kcl) = reacl(kcl) - d17
     reacl(kcl+1) = reacl(kcl+1) - d17
     reacl(kcl+2) = 0.0
     n1 = kcld1 + 1
     do n3 = n1, kcle
9040    hist(n3) = 0.0
     end do
     !.  output interface of sm and dm excitation with elect. netw.
     voltum(jm) = vinp(4)
     if (ntyp59 .eq. 1) voltum(jm) = vinp(4)/dcoef(jm)
9045 if (nodvo1(kcl+3) .eq. 1) go to 9050
     if (loopss(9) .eq. 2) go to 9046
     crest(nclout) = - umcur(4)
     if (ntyp59 .eq. 1) crest(nclout) = - umcur(4)*dcoef(jm)
     if (iprsup .ge. 1) write(lunit6,8862) nclout,sfreq(nclout),crest(nclout),time1(nclout),tstop(nclout)
9046 nclout = nclout + 1
9050 if (nodvo2(kcl+3) .eq. 1) go to 9060
     if (loopss(9) .eq. 2) go to 9052
     crest(nclout) = + umcur(4)
     if (ntyp59 .eq. 1) crest(nclout) = + umcur(4)*dcoef(jm)
     if (iprsup .ge. 1) write(lunit6,8862) nclout,sfreq(nclout),crest(nclout),time1(nclout),tstop(nclout)
9052 nclout = nclout + 1
9060 if (loopss(9) .eq. 2) go to 12100
     !  in case of sm type-59 and exciter mass is present :
     if (ntyp59 .ne. 1) go to 12000
     if (fpar(kcl+3) .ge. 0.0) go to 12000
     n3 = int(-fpar(kcl + 3))
     d1 = umcur(4) * umcur(4)
     crest(n3) = - 2.0 * d1/(gpar(kcld1) * omegm(jm))
     go to 12000
     !.  im equations code *****************************************
     !.  input interface elect netw with im power side **************
     !     norton=0 means no norton calculations
     !     norton=1 means calculation of i - norton
     !     norton=2 means calculation of y - norton
     !  note : norton parameter calculations are only performed
     !         in the pass with loopss(9) = 2
10000 norton = 0
     if (loopss(9) .eq. 2) norton = 1
     !     10019 if (norton .eq. 1) go to 10080
     if (norton .eq. 1) go to 10080
     do n10 = 1, 3
        n11 = n10 + kcl - 1
        if (nodvo1(n11) .eq. nodvo2(n11)) go to 10020
        n1 = nodvo1(n11)
        n1 = norder(n1)
        n2 = nodvo2(n11)
        n2 = norder(n2)
        dumvec(n10) = solr(n1) - solr(n2)
        dumvec(n10+3) = soli(n1) - soli(n2)
10020 end do
     if (con(1) .eq. 1) go to 10024
     if (con(8) .eq. 1) go to 10024
     if (loopss(9) .eq. 2) go to 10024
     !     10022 d1 = dumvec(1) + dumvec(2) + dumvec(3)
     d1 = dumvec(1) + dumvec(2) + dumvec(3)
     d2 = dumvec(4) + dumvec(5) + dumvec(6)
     d1 = d1 * d1
     d1 = d1 + d2 * d2
     d1 = sqrtz(d1)
     if (d1 .lt. epsiln) go to 10024
     write (lunit6,8310) jm
     !   pos sequence real and imaginary power thev voltages :
10024 vinp(1) = sroot3*con(3)*dumvec(1)+sroot2*con(2)*dumvec(2)
     vinp(1) = vinp(1) + con(1)*dumvec(3)
     vinp(21) = sroot3*con(3)*dumvec(4)+sroot2*con(2)*dumvec(5)
     vinp(21) = vinp(21) + con(1)*dumvec(6)
     !     10028 if (loopss(8) .eq. 1) go to 10080
     if (loopss(8) .eq. 1) go to 10080
     n10 = 0
     n20 = 2
     if (nodvo1(kcl) .eq. nodvo2(kcl)) n20 = 1
10030 n1 = nodvo1(kcl+1)
     n1 = norder(n1) + n20*ntot
     n2 = nodvo2(kcl+1)
     n2 = norder(n2) + n20*ntot
     !   pos sequence real and imaginary power thev impedances :
     zthrr = - zthrr + solr(n1) - solr(n2)
     zthri = - zthri + soli(n1) - soli(n2)
     if (n10 .eq. 1) go to 10040
     n20 = n20 + 1
     n10 = 1
     go to 10030
10040 if (con(1) .ne. 1.0) go to 10080
     n1 = nodvo1(kcl+2)
     n1 = norder(n1)
     n2 = nodvo2(kcl+2)
     n2 = norder(n2)
     zthrr = - solr(n1+ntot) + solr(n2+ntot)
     zthri = - soli(n1+ntot) + soli(n2+ntot)
     !.  formation of constant switch matrix ptheta :
10080 do n1 = 1, 3
        ptheta(3,n1) = 1.0/sroot3
     end do
     ptheta(1,1) = - 1.0/(sroot2*sroot3)
     ptheta(1,3) = sroot2/sroot3
     ptheta(1,2) = ptheta(1,1)
     ptheta(2,1) = - 1.0/sroot2
     ptheta(2,2) = - ptheta(2,1)
     ptheta(2,3) = 0.0
     !.  input interface im excitation and elect.netw. (thev variables):
     !     10100 n17 = kcle - kcl - 2
     n17 = kcle - kcl - 2
     if (ncl .gt. 6) n17 = 3
     do n1 = 1,9
10110   ndum(n1) = 0
     end do
     do n1 = 1, n17
        n2 = kcl + 2 + n1
        n3 = nodvo1(n2)
        ndum(n1) = norder(n3)
        n3 = nodvo2(n2)
        ndum(n1+3) = norder(n3)
        ndum(n1+6) = nodvo1(n2) - nodvo2(n2)
10120 end do
     do n1 = 1, n17
        n2 = ndum(n1)
        n3 = ndum(n1+3)
        n4 = n1 + 6
        if (ndum(n4) .eq. 0) go to 10130
        vinp(n1+3) = - solr(n2) + solr(n3)
        vinp(n1+23) = - soli(n2) + soli(n3)
        if (slip .lt. 0.0) vinp(n1+23) = - vinp(n1+23)
10130 end do
     n20 = 1
     do n1 = 1,n17
        n2 = ndum(n1) + n20 * ntot
        n3 = ndum(n1+3) + n20 * ntot
        n5 = ndum(n1) + ntot
        n6 = ndum(n1+3) + ntot
        n7 = n1 + 6
        n8 = ndum(1) + n20 * ntot
        n9 = ndum(4) + n20 * ntot
        if (ndum(n7) .eq. 0) go to 10140
        n20 = n20 + 1
        zths3(n1,n1) = - solr(n2) + solr(n3)
        zths3i(n1,n1) = - soli(n2) + soli(n3)
        if (n17 .eq. 1) go to 10200
        if (n1 .eq. 1) go to 10140
        if (ndum(7) .eq. 0) go to 10140
        zths3(1,n1) = - solr(n8) + solr(n9)
        zths3i(1,n1) = - soli(n8) + soli(n9)
        zths3(n1,1) = - solr(n5) + solr(n6)
        zths3i(n1,1) = - soli(n5) + soli(n6)
10140 end do
     if (slip .gt. 0.0) go to 10146
     do n1 = 1,n17
        do n2 = 1, n17
           zths3i(n1,n2) = - zths3i(n1,n2)
        end do
10144 end do
10146 if (n17 .lt. 3) go to 10200
     n19 = 0
     n20 = 1
     n1 = ndum(8) * ndum(9)
     if (n1 .eq. 0) go to 10170
     if (ndum(7) .ne. 0) n20 = 2
     n1 = 3
10150 n2 = ndum(n1) + n20 * ntot
     n3 = ndum(n1+3) + n20 * ntot
     if (n19 .eq. 1) go to 10160
     zths3(3,2) = - solr(n2) + solr(n3)
     zths3i(3,2) = - soli(n2) + soli(n3)
     n20 = n20 + 1
     n19 = 1
     n1 = 2
     go to 10150
10160 zths3(2,3) = - solr(n2) + solr(n3)
     zths3i(2,3) = - soli(n2) + soli(n3)
10170 if (ncl .le. 6) go to 10200
     n17 = kcld1 + 3
     do n1 = n17,kcle
        n2 = nodvo1(n1)
        n2 = norder(n2)
        n4 = n2 + ntot
        n3 = nodvo2(n1)
        n3 = norder(n3)
        n5 = n3 + ntot
        n10 = n1 - kcl + 1
        if (nodvo1(n1) .eq. nodvo2(n1)) go to 10180
        zthevs(n10) = - solr(n4) + solr(n5)
        zthevs(n10+20) = - soli(n4) + soli(n5)
        vinp(n10) = - solr(n2) + solr(n3)
        vinp(n10+20) = - soli(n2) + soli(n3)
10180 end do
     !.  transformation of thev variables if um type 4 :
10200 if (jtype(jm) .ne. 4) go to 10580
     do n1 = kcld1, kcle
        if (nodvo1(n1) .ne. nodvo2(n1)) go to 10504
10502 end do
     go to 10580
10504 n3 = 0
10505 do n1 = 1,3
        n2 = n1 + 3
        if (n3 .eq. 1) n2 = n1 + 23
10510   dumvec(n1) = vinp(n2)
     end do
     call matvec(ptheta,dumvec)
     do n1 = 1,3
        n2 = n1 + 3
        if (n3 .eq. 1) n2 = n1 + 23
10520   vinp(n2) = dumvec(n1)
     end do
     if (n3 .eq. 1) go to 10525
     n3 = 1
     go to 10505
10525 do n1 = 1, 3
        do n2 = 1, 3
10530      dummat(n1,n2) = ptheta(n2,n1)
        end do
10535 end do
     call matmul(zths3,dummat)
     call matmul(zths3i,dummat)
     n3 = 0
10540 do n1 = 1,3
        do n2 = 1, 3
           dummat(n1,n2) = ptheta(n1,n2)
        end do
10550 end do
     if (n3 .eq. 1) go to 10565
     call matmul(dummat,zths3)
     do n1 = 1, 3
        do n2 = 1, 3
           zths3(n1, n2) = dummat(n1, n2)
        end do
10560 end do
     n3 = 1
     go to 10540
10565 call matmul(dummat,zths3i)
     do n1 = 1, 3
        do n2 = 1, 3
           zths3i(n1,n2) = dummat(n1,n2)
10575   end do
     end do
     !.  start calculations of y - norton :
10580 if (norton .ne. 2) go to 10588
     vinp(1) = con(3)*sroot3 + con(2)*sroot2 + con(1)
     vinp(21) = 0.0
     do n1 = kcld1,kcle
        n10 = n1 - kcl + 1
        vinp(n10) = 0.0
        vinp(n10+20) = 0.0
     end do
     !.  calculation of im main fluxes :
     !.       real and imaginary flux : flxd, stored, flxq, storeq
10588 jacob = 1
     flxd(jm) = 1.0
     stored = 0.0
     flxq(jm) = 0.0
     storeq = 0.0
     go to 11000
10600 do n1 = 1,4
        fjm(n1,1) = fjv(n1)
     end do
     jacob = 2
     flxd(jm) = 0.0
     stored = 1.0
     go to 11000
10610 do n1 = 1, 4
        fjm(n1,2) = fjv(n1)
     end do
     jacob = 3
     stored = 0.0
     flxq(jm) = 1.0
     go to 11000
10620 do n1 = 1,4
        fjm(n1,3) = fjv(n1)
     end do
     jacob = 4
     flxq(jm) = 0.0
     storeq = 1.0
     go to 11000
10630 do n1 = 1,4
        fjm(n1,4) = fjv(n1)
     end do
     jacob = 5
     storeq = 0.0
     go to 11000
10640 jacob = 6
     go to 10700
10650 fjv(1) = umcur(2)
     fjv(2) = umcur(22)
     fjv(3) = umcur(3)
     fjv(4) = umcur(23)
     if (kcld1 .gt. kclde) go to 10670
     do n1 = kcld1,kclde
        n2 = n1 - kcl + 1
        n3 = n2 + 20
        fjv(1) = fjv(1) + umcur(n2)
        fjv(2) = fjv(2) + umcur(n3)
     end do
10670 if (kclq1 .gt. kclqe) go to 10690
     do n1 = kclq1,kclqe
        n2 = n1 - kcl + 1
        n3 = n2 + 20
        fjv(3) = fjv(3) + umcur(n2)
        fjv(4) = fjv(4) + umcur(n3)
     end do
10690 go to (10600,10610,10620,10630,10640), jacob
     !.  now calculate real and imaginary fluxes by gaussian
     !    elimination at stmt. nr. 11625 . point of return = 10750
10700 do n1 = 1,4
        do n2 = 1,4
           fjm(n1,n2) = fjm(n1,n2) - fjv(n1)
        end do
10709 end do
     do n1 = 1, 2
        n2 = n1 + 2
        fjm(n1,n1) = fjm(n1,n1) - 1.0/reamdu(jm)
        fjm(n2,n2) = fjm(n2,n2) - 1.0/reamqu(jm)
     end do
     do n1 = 1,4
        do n2 = 1, 4
           gmat(n1,n2) = - fjm(n1,n2)
        end do
10730 end do
     do n1 = 1, 4
10740   gmat(n1,5) = fjv(n1)
     end do
     jgauss = 1
     n5 = 4
     go to 11625
10750 jgauss = 0
     flxd(jm) = dumvec(1)
     stored = dumvec(2)
     flxq(jm) = dumvec(3)
     storeq = dumvec(4)
     !.   calculation of umcur for im *******************************
     !.   umcur power :
11000 d10 = reacl(kcl+1) * seltat
     d11 = reacl(kcl+2) * seltat
     d11 = d11 + zthri + (d10-d11)/2.0
     d12 = zthrr + 1.0/gpar(kcl+2)
     d13 = 2.0 * (d11*d11 + d12*d12)
     d17 = (flxd(jm) + storeq) * seltat
     d18 = (stored - flxq(jm)) * seltat
     d1 = - sroot2*vinp(1)
     d1 = d1 + d17
     d2 = - sroot2*vinp(21)
     d2 = d2 + d18
     umcur(1) = 0.0
     umcur(21) = 0.0
     umcur(2) = (d2*d12 - d1*d11)/d13
     umcur(3) = (d1*d12 + d2*d11)/d13
     umcur(22) = - umcur(3)
     umcur(23) = + umcur(2)
     !.  umcur excitation because of input voltage
     if (jacob .lt. 6) go to 11100
     if (loopss(9) .ne. 2) go to 11100
     if (slip .ne. 0.0) go to 11100
     go to 11720
11100 do n10 = kcld1, kcle
        d1 = slip * seltat * reacl(n10) * gpar(n10)
        d2 = 1.0 + d1*d1
        fpar(n10) = 1.0/d2
        n5 = n10 - kcl + 1
        n6 = n5 + 20
        d3 = fpar(n10) * gpar(n10)
        umcur(n5) = - d3 * (vinp(n5) + d1*vinp(n6))
        umcur(n6) = - d3 * (vinp(n6) - d1*vinp(n5))
11500 end do
     !.  umcur excitation because of fluxes
     !     11515 if (ncld(jm) .eq. 0) go to 11530
     if (ncld(jm) .eq. 0) go to 11530
     do n10 = kcld1,kclde
        n5 = n10 - kcl + 1
        n6 = n5 + 20
        d1 = slip * seltat * gpar(n10) * reacl(n10)
        d2 = slip * seltat * gpar(n10) * fpar(n10)
        umcur(n5) = umcur(n5) + d2*(stored - d1*flxd(jm))
        umcur(n6) = umcur(n6) - d2*(d1*stored + flxd(jm))
11520 end do
11530 if (nclq(jm) .eq. 0) go to 11550
     do n10 = kclq1,kclqe
        n5 = n10 - kcl + 1
        n6 = n5 + 20
        d1 = slip * seltat * gpar(n10) * reacl(n10)
        d2 = slip * seltat * gpar(n10) * fpar(n10)
        umcur(n5) = umcur(n5) + d2*(storeq - d1*flxq(jm))
        umcur(n6) = umcur(n6) - d2*(d1*storeq + flxq(jm))
11540 end do
     !.  umcur excitation because of thev impedances :
11550 do n1 = kcld1, kcle
        if (nodvo1(n1) .ne. nodvo2(n1)) go to 11554
11552 end do
     go to 11720
11554 n15 = kcle
     if (ncl .gt. 6) n15 = kcl + 5
     do n10 = 1,3
        do n11 = 1, 3
           zthevr(n10,n11) = 0.0
           dummat(n10,n11) = 0.0
        end do
     end do
     n7 = kcl + 3
     do n10 = n7, n15
        n6 = n10 - kcl - 2
        zthevr(n6,n6) = gpar(n10) * fpar(n10)
        dummat(n6,n6) = zthevr(n6,n6)
     end do
     call matmul(dummat,zths3)
     call matmul(zthevr,zths3i)
     do n10 = 1, 3
        do n11 = 1, 3
           dumma1(n10,n11) = dummat(n10,n11)
           dumma2(n10,n11) = zthevr(n10,n11)
        end do
     end do
     do n10 = 1, 3
        n12 = n10 + kcl + 2
        d1 = slip * seltat * reacl(n12) * gpar(n12)
        do n11 = 1, 3
           zthevr(n10,n11) = d1 * zthevr(n10,n11)
           dummat(n10,n11) = d1 * dummat(n10,n11)
        end do
11580 end do
     do n10 = 1, 3
        do n11 = 1, 3
           dumma1(n10,n11) = dumma1(n10,n11) + zthevr(n10,n11)
           dumma2(n10,n11) = dumma2(n10,n11) - dummat(n10,n11)
        end do
     end do
     do n10 = 1, 3
        n12 = n10 + 3
        do  n11 = 1, 3
           n13 = n11 + 3
           gmat(n10,n11) = + dumma1(n10,n11)
           gmat(n12,n13) = + dumma1(n10,n11)
           gmat(n10,n13) = - dumma2(n10,n11)
           gmat(n12,n11) = + dumma2(n10,n11)
        end do
11605 end do
     do n10 = 1, 6
11610   gmat(n10,n10) = gmat(n10,n10) + 1.0
     end do
     do n10 = 1,3
        n11 = n10 + 3
        n12 = n11 + 20
        gmat(n10,7) = umcur(n11)
11620   gmat(n11,7) = umcur(n12)
     end do
     jgauss = 0
     n5 = 6
     !.  gaussian elimination for input : n5 and gmat(n5,n5+1) *****
     !             output is stored in  : dumvec(n5)
11625 n6 = n5 + 1
     do n1 = 1, n5
        n2 = n1 + 1
        if (n2 .gt. n5) go to 11650
        do n10 = n2, n5
           n8 = n6 - n1
           do n9 = 1, n8
              n11 = n6 + 1 - n9
              d1 = gmat(n10,n1) * gmat(n1,n11)/gmat(n1,n1)
11630         gmat(n10,n11) = gmat(n10,n11) - d1
           end do
           n11 = n6 - n8
           gmat(n10,n11) = 0.0
        end do
     end do
11650 do n10 = 1, n5
        n7 = n5 + 1 - n10
        dumvec(n7) = gmat(n7,n6)/gmat(n7,n7)
        do n12 = 1, n5
           n13 = n7 + n12
           if (n13 .gt. n5) go to 11670
           dumvec(n7)=dumvec(n7)-gmat(n7,n13)*dumvec(n13)/gmat(n7,n7)
        end do
11670 end do
     !.  end of gaussian elimination ********************************
     if (jgauss .eq. 1) go to 10750
     do n10 = 1, 3
        n11 = n10 + 3
        n12 = n11 + 20
        umcur(n11) = dumvec(n10)
11680   umcur(n12) = dumvec(n11)
     end do
     !     11700 if (ncl .le. 6) go to 11720
     if (ncl .le. 6) go to 11720
     n15 = kcl + 6
     do n10 = n15, kcle
        n11 = n10 - kcl + 1
        n12 = n11 + 20
        zthevs(n11) = gpar(n10) * fpar(n10) * zthevs(n11)
        zthevs(n12) = gpar(n10) * fpar(n10) * zthevs(n12)
        d1 = slip * seltat * gpar(n10) * reacl(n10)
        d2 = zthevs(n11)
        zthevs(n11) = 1.0 + zthevs(n11) + d1*zthevs(n12)
        zthevs(n12) = + zthevs(n12) - d1*d2
        d3 = zthevs(n11) * zthevs(n11)
        d3 = d3 + zthevs(n12)*zthevs(n12)
        d3 = 1.0/d3
        d1 = umcur(n11)
        umcur(n11) = zthevs(n11)*umcur(n11)+zthevs(n12)*umcur(n12)
        umcur(n11) = d3 * umcur(n11)
        umcur(n12) = - zthevs(n12)*d1 + zthevs(n11)*umcur(n12)
11710   umcur(n12) = d3 * umcur(n12)
     end do
11720 if (jacob .lt. 6) go to 10650
     !.  output for um = im
     if (loopss(9) .eq. 2) go to 11800
     !     11725 omegm(jm) = (1.0 - slip)*seltat/nppair(jm)
     omegm(jm) = (1.0 - slip) * seltat / nppair(jm)
     omold(jm) = omegm(jm)
     do n10 = 1, 3
        n11 = kcl - 1 + n10
        hist(n11) = umcur(n10)
     end do
     do n10 = kcld1,kcle
        n11 = n10 - kcl + 1
        hist(n10) = - umcur(n11)
     end do
     !.  back transformation for um type 4 excitation currents
     !     11750 if (jtype(jm) .ne. 4) go to 11800
     if (jtype(jm) .ne. 4) go to 11800
     n13 = 0
11760 do n10 = 1,3
        n12 = n10 + 3 + n13*20
        dumvec(n10) = umcur(n12)
        do n11 = 1,3
           dummat(n10,n11) = ptheta(n11,n10)
        end do
     end do
     call matvec(dummat,dumvec)
     do n10 = 1,3
        n11 = n10 + 3 + n13*20
11790   umcur(n11) = dumvec(n10)
     end do
     if(n13 .eq. 1) go to 11800
     n13 = 1
     go to 11760
     !.  output interface of im and elect. network :
11800 d14 = con(3)/(sroot2*sroot3) + con(2)/2.0 + con(1)
     d15 = con(3)/sroot3 + con(2)/sroot2 + con(1)
     d11 = (- umcur(22) + umcur(3)) * d14
     d12 = (+ umcur(2) + umcur(23)) * d14
     !  adjusting current output to include power resistance to emtp
     !   for the case of prediction request :
     if (loopss(9) .ne. 3) go to 11806
     d11 = d11 + vinp(1)*gpar(kcl+2)*d15
     d12 = d12 + vinp(21)*gpar(kcl+2)*d15
11806 d10 = d11*d11 + d12*d12
     if (d10 .ne. 0.0) go to 11810
     d11 = 0.0
     d2 = 0.0
     d3 = 0.0
     go to 11815
11810 if (norton .eq. 2) d5 = - d11/d10
     if (norton .eq. 2) d6 = + d12/d10
     d10 = sqrtz(d10)
     d1 = d12/d11
     d1 = atanz(d1)
     if (d11 .lt. 0.0) d1 = d1 + twopi/2.0
     d2 = con(2)*d1 + con(3)*(d1 - twopi/3.0)
     d3 = con(2)*(d1 + twopi/4.0) + con(3)*(d1 + twopi/3.0)
     d3 = d3 + con(1)*d1
11815 do n1 = 1, 3
        n2 = n1 + kcl - 1
        n11 = 3 * (jm-1)
        if (nodvo1(n2) .eq. nodvo2(n2)) go to 11824
        !      including mach resistances to power network at
        !      last pass through this part of code for non -
        !      compensated power coils.
        if (lfim3i .ne. 0) go to 11818
        if (loopss(8) .ne. 1) go to 11821
11818   n11 = numibr - 3 + n1
        n12 = - nr(n11)
        if (n12 .lt. 0) n12 = - n12
        tr(n12) = 1.0/gpar(kcl+2)
        tx(n12) = 0.0
        if (norton .ne. 2) go to 11821
        tr(n12) = d5
        tx(n12) = d6 * 1.0d+3/seltat
        if (xopt .ne. 0.0) tx(n12) = d6 * xopt * twopi/seltat
        go to 11823
11821   d11 = d1
        if (n1 .eq. 2) d11 = d2
        if (n1 .eq. 3) d11 = d3
        if (n1 .eq. 1 .and. lfim3i .ne. 0) nclout = int(hist(kcl + 2))
        if (nodvo1(n2) .eq. 1) go to 11822
        crest(nclout) = d10
        time1(nclout) = d11
        if (iprsup .ge. 1) write(lunit6,8862) nclout,sfreq(nclout),crest(nclout),time1(nclout),tstop(nclout)
        nclout = nclout + 1
11822   if (nodvo2(n2) .eq. 1) go to 11823
        crest(nclout) = - d10
        time1(nclout) = d11
        if (iprsup .ge. 1) write(lunit6,8862) nclout,sfreq(nclout),crest(nclout),time1(nclout),tstop(nclout)
        nclout = nclout + 1
11823   if (loopss(8) .eq. 0 .and. lfim3i .eq. 0) go to 11824
        if (iprsup .ge. 1 .and. norton .ne. 1) write(lunit6,8860) n11,tr(n12),tx(n12)
11824 end do
     if (loopss(9) .ne. 2) go to 11825
     if (norton .eq. 2) go to 11825
     norton = 2
     go to 10580
11825 if (lfim3i .ne. 0) go to 13050
     nclind = int(fpar(kcl + 1))
     do n10 = 4, ncl
        n11 = n10 + 20
        n12 = n10 + kcl - 1
        if (slip .lt. 0.0) umcur(n11) = - umcur(n11)
        if (nodvo1(n12) .eq. nodvo2(n12)) go to 11840
        if (loopss(9) .eq. 2) go to 11828
        d2 = umcur(n10)*umcur(n10) + umcur(n11)*umcur(n11)
        d2 = sqrtz(d2)
        d3 = umcur(n11)/umcur(n10)
        d3 = atanz(d3)
        if (umcur(n10) .lt. 0.0) d3 = d3 + twopi/2.0
11828   if (nodvo1(n12) .eq. 1) go to 11830
        if (loopss(9) .eq. 2) go to 11829
        crest(nclind) = - d2
        time1(nclind) = d3
        if (iprsup .ge. 1) write(lunit6,8862) nclind,sfreq(nclind),crest(nclind),time1(nclind),tstop(nclind)
11829   nclind = nclind + 1
11830   if (nodvo2(n12) .eq. 1) go to 11840
        if (loopss(9) .eq. 2) go to 11832
        crest(nclind) = + d2
        time1(nclind) = d3
        if (iprsup .ge. 1) write(lunit6,8862) nclind,sfreq(nclind),crest(nclind),time1(nclind),tstop(nclind)
11832   nclind = nclind + 1
11840 end do
     !.  calculation of mech. variables *****************************
     !   note : this second part of umrenu is entered only at the
     !          last pass through second part of code. here we
     !          will use anglum(jm) to store tqgen for the slack-bus
     !          calculation.
     !     11900 if (loopss(9) .eq. 2) go to 12100
     if (loopss(9) .eq. 2) go to 12100
12000 d1 = 3.0*con(3) + 2.0*con(2) + con(8) + con(1)
     d2 = (reacl(kcl+1) - reacl(kcl+2))*umcur(2)*umcur(3)
     d3 = umcur(2) * flxq(jm)
     d4 = umcur(3) * flxd(jm)
     tqgen = (d2 - d3 + d4) * nppair(jm)
     if (inpu .eq. 1) tqgen = tqgen/d1
     !.  output interface of um with mechanical network :
     anglum(jm) = + tqgen
     crest(nclout) = omegm(jm)
     if (loopss(10) .ne. 3) jtmtac(jm) = 0
     if (iprsup .ge. 1) write(lunit6,8862) nclout,sfreq(nclout),crest(nclout),time1(nclout),tstop(nclout)
     if (iprsup .ge. 1) write(lunit6,12010) (hist(n1),n1 = 1,ncl)
12010 format(/,14h hist(1:ncl) =, (1x,6e15.6))
     if (iprsup .ge. 1) write(lunit6,12020) tqgen
12020 format(/, 8h tqgen =, 1x,e15.6)
12100 nclout = nclout + 1
     !  final statements of machine do-loop
     !     13000 if (ntyp59 .eq. 1) jtype(jm) = 13
     if (ntyp59 .eq. 1) jtype(jm) = 13
     if (ntyp59 .eq. 1) inpu = inpust
     if (loopss(9) .eq. 2) hist(kcl) = numibr
13050 end do
  if (lfim3 .eq. 1) loopss(9) = lpss9
  if (lfim3 .eq. 1) go to 13500
  if (loopss(9) .eq. 2) go to 13500
  !     13060 loopss(2) = 0
  loopss(2) = 0
  loopss(6) = kconst
  !.  calculation of slack buses :
13100 kconst = loopss(6)
  do jm = 1, numum
     kcl = kcoil(jm)
     n1 = jcltac(kcl)
     n2 = jcltac(kcl+1)
     nshare = n1 + n2
     if (nshare .ne. 0) nshare = 1
     !  note : if nshare=0 then no multi um sharing mech netw
     if (n1 .ne. 0 .and. jm .gt. n1) nshare = 10
     if (n2 .ne. 0 .and. jm .gt. n2) nshare = 10
     !  note: if nshare=10, no mech slack bus calc but speed capacitor
     !         and torque source of um nr. n1 and um nr. n2 to be set
     if (iprsup .ge. 1) write (lunit6,13110) jm
13110 format(/,26h *************************,39h start adjustment of slack buses in the, &
           37h EMTP networks connected to um number,i4,2h :)
     !  checking speeds for multi-mach sharing common network :
     if (loopss(4) .gt. 0) go to 13118
     if (nshare .eq. 0) go to 13118
     if (nshare .eq. 10) go to 13118
     if (omegm(jm) .ne. omegm(n1)) go to 13112
     if (n2 .eq. 0) go to 13118
     if (omegm(jm) .ne. omegm(n2)) go to 13112
     go to 13118
13112 if (n2 .eq. 0) go to 13115
     write(lunit6,13113) jm,n1,n2
13113 format(/,45h ********************************************,45h ********************************************,&
           20h warning : um number,i4,2h, ,i4,5h, and,i4)
     write (lunit6,13114)
13114 format(  42h share a common mechanical network, but do, 45h not have equal mechanical machine-speeds. is,&
           43h this intentional or have you made an error    ,/,   45h in designing your mechanical network ? these, &
           45h speeds (rad/sec) are equal to (1-slip) times,         44 h angular freq of electric power grid devided)
     write(lunit6,93116)
93116 format(  39h by the nr of pole-pairs. the slip here, 42h is in abs. values and in steady-state can, &
           40h obviously only be nonzero for induction, 10h machines.,/)
13115 write(lunit6,13116) jm,n1
13116 format(/,45h ********************************************, 45h ********************************************, &
           20h warning : um number,i4,5h, and,i4)
     write(lunit6,13114)
     write(lunit6,93116)
13118 if (iprsup .ge. 1) write(lunit6,13120)
13120 format(30h *********** slack bus source:,2x,6hkconst,4x,4hnode,9x,5hsfreq,9x,5hcrest,9x,5htime1,8x,6htstart,9x,5htstop)
     imach = 1
     if (jtype(jm) .lt. 3) imach = 0
     if (jtype(jm) .gt. 7) imach = 0
     if (imach .ne. 1) n10 = nodfum(jm)
     if (loopss(4) .gt. 0) go to 13130
     if (imach .ne. 1) crest(n10) = voltum(jm)
     if (imach .ne. 1 .and. iprsup .ge. 1) write(lunit6,13122) n10,node(n10),sfreq(n10),crest(n10),time1(n10),tstart(n10),tstop(n10)
13122 format(30h ****** exct slack bus source:,2x,i6,2x,i6,5e14.5)
13123 format(30h ****** mech slack bus source:,2x,i6,2x,i6,5e14.5)
     if (nshare .eq. 10) go to 13170
     go to 13370
     !  coil terminal voltage and mechanical speed for given crest:
     !   temporary storage of voltage in flxd(jm) and voltum(jm)
     !   temporary storage for torque in flxq(jm) and anglum(jm)
     !  note: solr is here node voltage since no compensation on
     !        the um terminals which are considered here.
     !  code for loopss(4) = 1 and 2 ********************************
13130 n11 = nodmum(jm)
     if (imach .eq. 1) go to 13140
     n5 = kcl + 3
     n1 = nodvo1(n5)
     n1 = norder(n1)
     n2 = nodvo2(n5)
     n2 = norder(n2)
     d1 = solr(n1) - solr(n2)
13140 n12 = nodom(jm)
     n3 = norder(n12)
     d3 = -emtpe(n3)
     !  code for loopss(4) = 1  *************************************
     if (loopss(4) .gt. 1) go to 13150
     if (imach .ne. 1) flxd(jm) = d1
     if (imach .ne. 1) crest(n10) = 0.0
     if (imach .ne. 1 .and. iprsup .ge. 1) write(lunit6,13122) n10,node(n10),sfreq(n10),crest(n10),time1(n10),tstart(n10),tstop(n10)
     if (nshare .eq. 10) go to 13170
     !     13141 flxq(jm) = d3
     flxq(jm) = d3
     tstop(n11) = crest(n11)
     crest(n11) = 0.0
     if (iprsup .ge. 1) write(lunit6,13123) n11,node(n11),sfreq(n11),crest(n11),time1(n11),tstart(n11),tstop(n11)
     n13 = kssfrq(n12)
     if (n13 .lt. 0) go to 13370
     n14 = n13
13142 if (n13 .eq. n11) go to 13144
     if (tstart(n13) .ne. -7777.) go to 13144
     tstop(n13) = crest(n13)
     crest(n13) = 0.0
     if (iprsup .ge. 1) write(lunit6,13123) n13,node(n13),sfreq(n13),crest(n13),time1(n13),tstart(n13),tstop(n13)
13144 n13 = kpsour(n13)
     if (n14 .ne. n13) go to 13142
     !     13148 go to 13370
     go to 13370
     !     code for loopss(4) = 2 (final umrenu pass) ******************
13150 if (imach .eq. 1) go to 13160
     d5 = voltum(jm) - d1
     d5 = d5/(flxd(jm) - d1)
     crest(n10) = d5 * voltum(jm)
     if (iprsup .ge. 1) write(lunit6,13122) n10,node(n10),sfreq(n10),crest(n10),time1(n10),tstart(n10),tstop(n10)
13160 if (nshare .eq. 10) go to 13170
     !     13161 d5 = anglum(jm) - d3
     d5 = anglum(jm) - d3
     d5 = d5/(flxq(jm) - d3)
     crest(n11) = d5 * tstop(n11)
     tstop(n11) = 9999.
     if (iprsup .ge. 1) write(lunit6,13123) n11,node(n11),sfreq(n11),crest(n11),time1(n11),tstart(n11),tstop(n11)
     n13 = kssfrq(n12)
     if (n13 .lt. 0) go to 13370
     n14 = n13
13162 if (n13 .eq. n11) go to 13164
     if (tstart(n13) .ne. -7777.) go to 13164
     crest(n13) = d5 * tstop(n13)
     tstop(n13) = 9999.
     if (iprsup .ge. 1) write(lunit6,13123) n13,node(n13),sfreq(n13),crest(n13),time1(n13),tstart(n13),tstop(n13)
13164 n13 = kpsour(n13)
     if (n14 .ne. n13) go to 13162
     !     set speed capacitors and sources for tqgen (for um's with
     !     no slack bus calculations, i.e. jm gt than jcltac(kcl) and
     !     for jm gt jcltac(kcl+1) .
13170 if (nshare .ne. 10) go to 13370
     n2 = int(fpar(kcl + 2))
     emtpc(n2) = 1.0d+8
     if (copt .ne. 0.0) emtpc(itcap) = emtpc(itcap)*copt*twopi
     tr(n2) = 0.0
     kconst = kconst + 1
     if ( kconst .le. lexct )  go to 6784
     write (lunit6, 6783)  lexct
6783 format ( 38h overflow list 4 in "umrenu".  lexct =,i5,    18h     stop locally.     )
     call stoptp
6784 node(kconst) = - nodom(jm)
     iform(kconst) = 14
     crest(kconst) = - anglum(jm)
     time1(kconst) = 0.0
     tstart(kconst) = - 1.0
     tstop(kconst) = 0.0
     n5 = nodmum(jm)
     sfreq(kconst) = sfreq(n5)
     if (iprsup .ge. 1) write(lunit6,13172) kconst,node(kconst),sfreq(kconst),crest(kconst),time1(kconst),tstart(kconst),tstop(kconst)
13172 format(31h ****** temporary tqgen source:,1x,i6,2x,i6,5e14.5)
13370 end do
  loopss(4) = loopss(4) + 1
  if (loopss(4) .lt. 3) go to 13500
  !.  permanent exit of umrenu if loopss(4) .ge. 3
  loopss(1) = - 1
  kssout = loopss(7)
  istart = 0
  if (iprsup .ge. 1) write (lunit6,13400)
13400 format( /, 40h permanent exit of umrenu **************)
  go to 14000
13500 istart = istart + 1
  if (iprsup .lt. 4) go to 14000
  write (lunit6, 13616)  (i, kbus(i), mbus(i), nr(i), length(i), kodebr(i), kodsem(i), i = 1, ibr)
13616 format (/, 21h branch table vectors,/,  70h       row      kbus    mbus        nr    length    kodebr    kodsem,/, (1x, 7i10) )
  write (lunit6, 13716)  (i, iform(i), node(i), crest(i), time1(i), tstart(i), sfreq(i), i = 1, kconst)
13716 format (/, 21h source table vectors,/, 30h       row     iform    node,15x, 5hcrest, 15x, 5htime1, 14x, 6htstart,15x,5hsfreq,/, &
           (3i10, 4e20.10)   )
14000 if (iprsup .ge. 1) write(lunit6, 14100)  istart,istep,loopss(1),loopss(2), loopss(4),loopss(8),loopss(9),loopss(10),kconst,ibr, &
           inonl,kswtch,istead,omegrf
14100 format (/,26h *************************,30h setting at this umrenu exit :    ,/, 48h  istart   istep  lopss1  lopss2  lopss4  lopss8, &
           48h  lopss9 lopss10  kconst     ibr   inonl  kswtch, 8h  istead,8x,6homegrf,/,13i8,e14.5, /)
  return
end subroutine umrenu
!
!     subroutine umrnu2.
!
subroutine umrnu2(reacl,gpar,fpar, hist,umcurp,nodvo1,jtype,nodom, jtmtac,thetam, imach,reamdu, reamqu,nodvo2, &
     nppair,rotmom, ncld,jcltac, kcl,nclq, epsom,dcoef,kcoil,voltum, anglum,nodfum, nodmum, umoutp)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  real(8) n6, n8
  dimension  reacl(1), gpar(1), fpar(1), hist(1), umcurp(1)
  dimension  nodvo1(1), nodvo2(1), jcltac(1)
  dimension  jtype(1), nodom(1), jtmtac(1)
  dimension  thetam(1), reamdu(1), reamqu(1)
  dimension  nppair(1), rotmom(1), ncld(1), nclq(1)
  dimension  epsom(1), dcoef(1), kcoil(1), umoutp(1)
  dimension  voltum(1), anglum(1), nodfum(1), nodmum(1)
  dimension  zths3(3,3),zths3i(3,3),fjm(4,4),fjv(4)
  dimension  dumma1(3,3), dumma2(3,3), gmat(6,7)
  common /umlocl/ n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n17, n18, n19, n20, d1, d2, d3, d4, d5, d6, d7, d8
  common /umlocl/ d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, lfim3,lfim4i, ncomcl, ncomum, kcld1, kclq1, kclf, nminum
  common /umlocl/ lopss1, lopss2, lopss4, lopss8, lopss9, lopss10, slip
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'space2.ftn'
  include 'umdeck.ftn'
  !     "first part" of umrenu code is entered twice if load-flow
  !     is requested (loopss(10) = 1 or 2). This is needed to
  !     let the load-flow do its work to adjust the sources.
  !     However, if um type-3 is present, the first part will
  !     be entered one additional pass prior to the two
  !     mentioned passes in order to find the thev parameters of
  !     the um type-3 excitation circuits (loopss(10) = 3).
  !     .  ....... code for um steady-state initialization ............
  !     . "first part" of code : set up of vectors ********************
  !     note: this part till stmt 1400 is only entered if istart=0
  if (loopss(10) .gt. 1) go to 1400
  if (istart .gt. 0) return
  loopss(9) = 0
  imach = 0
  !   renumbering mechanical nodes :
  do i = 1,numum
     n9 = int(nodom(i))
     nodom(i) = norder(n9)
1230 end do
  do i = 1, numum
     kcl = kcoil(i)
     if (jtmtac(i) .gt. 0) go to 1250
     !. find the um's connected to same mechanical network :
     !    (initialize jcltac(kcl and kcl+1) and identifying nodmum)
     !     1245 n10 = nodom(i)
     n10 = nodom(i)
     n10 = kssfrq(n10)
     if (n10 .lt. 0) go to 1250
     n11 = n10
     n1 = 0
1246 n1 = n1 + 1
     ndum(n1) = n10
     n10 = kpsour(n10)
     if (n10 .ne. n11) go to 1246
     if (n1 .eq. 1) go to 1250
     if (n1 .le. 40) go to 81246
     write (lunit6,81245) n1,i
81245 format(/,23h error stop. there are ,i2, 8h type-14, 41h sources present in mechanical network of, &
           10h um number, i4, 28h.  this number overflows the,/, 42h limit of 40. increasing this limit can be, &
           42h done by increasing the dimension of array, 41h "ndum" which is dimensioned in "umdeck".)
     call stoptp
81246 nummec = 0
     do j = 1, numum
        if (j .eq. i) go to 1248
        kclj = kcoil(j)
        n4 = nodom(j)
        n4 = kssfrq(n4)
        if (n4 .lt. 0) go to 1248
        !     81247 do 1247 n2 = 1,n1
        do n2 = 1, n1
           if (ndum(n2) .ne. n4) go to 1247
           nummec = nummec + 1
           if (nummec .eq. 1) jcltac(kcl) = j
           if (nummec .eq. 2) jcltac(kcl+1) = j
           if (j .gt. i) nodmum(j) = nodmum(i)
           go to 1248
1247    end do
1248 end do
     if (nummec .le. 2) go to 1250
     n2 = nummec - 1
     write (lunit6,1249) i,n2
1249 format(/,22h error stop. um number, i4,13h is connected, 17h mechanically to ,i2,17h other um's. this, &
          44h can not be honored because at the most only,/,   45h 3 um's can be connected to a same mechanical, &
          41h network. you may resolve this problem by,42h inserting stub lines in order to seperate)
     write (lunit6,81249)
81249 format(" the mechanical network from the remaining um's.")
     call stoptp
     !. default value for epsom(i) :
1250 if (epsom(i) .ne. 0.0) go to 1260
     epsom(i) = 0.001
     if (jtype(i) .eq. 13) go to 1255
     if (inpu .eq. 1) go to 1260
1255 epsom(i) = 0.001 * rotmom(i)/nppair(i)
1260 if (jtype(i) .gt. 2 .and. jtype(i) .lt. 8) imach=1
     if (jtype(i) .eq. 3 .and. loopss(10) .eq. 1) loopss(10)=3
     !   renumbering electrical nodes :
     if (jtmtac(i) .ge. 0) go to 1262
     n9 = - jtmtac(i)
     jtmtac(i) = - norder(n9)
1262 if (jtype(i) .ne. 13) go to 1270
     if (fpar(kcl+3) .ge. 0.0) go to 1270
     n5 = int(fpar(kcl + 4))
     fpar(kcl+4) = norder(n5)
     n5 = int(fpar(kcl + 5))
     fpar(kcl+5) = norder(n5)
1270 n2 = 3 * (i - 1)
     do n1 = 1,3
        n3 = int(umcurp(n1 + n2))
        n3 = int(norder(n3))
        umcurp(n1+n2) = n3
1280 end do
1290 end do
  if (imach .eq. 1 .and. loopss(8) .eq. 1) loopss(9)=1
  do i = 1, ncltot
     n8 = nodvo1(i)
     nodvo1(i) = norder(n8)
     n9 = nodvo2(i)
     nodvo2(i) = norder(n9)
  end do
  !  renumbering in case of tacs transfer with sm type-59 data :
  if (nsmach .eq. 0) go to 1340
  n5 = int(umoutp(numout + 1))
  if (n5 .ne. -9999.0) go to 1340
  n5 = numout + 4
  n6 = umoutp(numout+3) - 2
1330 if (n5 .gt. n6) go to 1338
  n8 = umoutp(n5)
  if (n8 .eq. -2) go to 1332
  if (n8 .eq. -3) go to 1332
  if (n8 .eq. -5) go to 1332
  if (n8 .gt. -298) go to 1334
1332 n9 = int(umoutp(n5+1))
  umoutp(n5+1) = norder(n9)
  n5 = n5 + 3
  if (n8 .le. -299) n5 = n5 + 5
  go to 1330
1334 n5 = n5 + 1
  go to 1330
1338 n5 = int(umoutp(numout+3))
  if (iprsup .ge. 1) write (lunit6,1339) (umoutp(i),i=1,n5)
1339 format(/,40h um output table umoutp(numout+nsmtac+3),40h with some entries renumbered because of,&
          38h tacs interface for sm-59 data input :                     ,/,6(6x,e14.5)/(6(6x,e14.5)) )
1340 if (iprsup .lt. 1) go to 1350
  write (lunit6, 1341)
1341 format (/,29h um nodes after renumbering :)
  write (lunit6, 1345)  ( nodvo1(i), i=1, ncltot )
1345 format (19h nodvo1(1:ncltot) :,  20i5  )
  write (lunit6, 1346)  ( nodvo2(i), i=1, ncltot )
1346 format (19h nodvo2(1:ncltot) :,  20i5  )
  n5 = 3 * numum
  write (lunit6, 1349)  ( umcurp(i), i=1, n5)
1349 format (19h umcurp(1:3numum) :,  20f5.1)
  write(lunit6,1347) (kcoil(i),i=1,numum)
1347 format (19h kcoil(1:numum)   :,20i5)
  write(lunit6,1348) (nodom(i),i=1,numum)
1348 format (19h nodom(1:numum)   :,20i5)
  !.   if user does not request for automatic initialization :
1350 if (initum .eq. 0) return
  !.   setting of all flags (if not included, then they are 0):
  loopss(3) = kconst
  loopss(5) = ibr
  loopss(12) = loopss(5)
  loopss(7) = kssout
  hist(kcl+1) = numsub
  kssout = 0
  !  ******* point of entrance at second pass thru "first part".
1400 if (loopss(10) .ne. 3) go to 1480
  if (istep .eq. -4567) go to 1480
  loopss(10) = 1
  istep = - 4567
  lfim3 = 1
1480 if (loopss(10) .ne. 2) go to 1500
  kconst = loopss(3)
  ibr = loopss(12)
  it = - nr(ibr)
  numsub = int(hist(kcl + 1))
1500 n15 = 0
  ncomcl = 0
  ncomum = 0
  !   ************** start machine do-loop ***************
  do k = 1, numum
     lfim4i = 0
     kcl = kcoil(k)
     ncl = 3 + ncld(k) + nclq(k)
     kcld1 = kcl + 3
     kclq1 = kcld1 + ncld(k)
     if (jtype(k) .eq. 4) ncl = ncl + 1
     kcle = kcl - 1 + ncl
     if (k .eq. 1) ksubum = numsub + 1
     imach = 1
     if (jtype(k) .lt. 3) imach = 0
     if (jtype(k) .gt. 7) imach = 0
     if (imach .eq. 1) slip = voltum(k)/100.0
     if (loopss(10) .ne. 3) go to 1900
     if (iprsup .ge. 1) write(lunit6,1850)
1850 format(/,40h ***************************************,45h preparing thevenin calculation for um type-3,&
          44h excitation circuits due to ld-flow request:)
     go to 2008
1900 if (iprsup .ge. 1) write (lunit6,1902)
1902 format(/,40h ***************************************,38h momentary creation of additional EMTP, &
          38h elements (permanent branches if power, /,   40h ***************************************, &
          38h coils are non-comp.) for steady-state, 16h calculations of)
     if (iprsup .ge. 1) write (lunit6,1904) k
1904 format(10h um number,i4,2h :,28x,18hnode to node   ibr,4x,2hit,8x,6htr(it),8x,6htx(it),2x,6hkconst, &
          9x,5hsfreq,9x,5hcrest)
     if (jtype(k) .eq. 4 .and. loopss(10) .eq. 1) lfim4i = 1
     if (jtype(k) .eq. 4 .and. loopss(10) .eq. 2) lfim4i = 2
     !  bring the im type-4 excitation  sources back to slip-freq at
     !       pass for which loopss(10) = 2 and um = im type-4 :
     !  note : this is not necessary for the rotor resistances
     !         because of the "restore" feature for r,l,c branches.
     if (lfim4i .ne. 2) go to 2000
     if (loopss(10) .ne. 2) go to 2000
     d3 = slip
     if (d3 .lt. 0.0) d3 = - d3
     n19 = int(fpar(kcl + 1))
     n18 = ncl - 3
     n20 = n19 - 1 + n18
     do n13 = 1,n18
        n14 = kcl + 2 + n13
        n2 = nodvo1(n14)
        if (n2 .eq. 1) n2 = nodvo2(n14)
        if (n2 .eq. 1) go to 1970
        n3 = kssfrq(n2)
        n4 = n3
        if (n3 .ge. n19 .and. n3 .le. n20) then
           sfreq(n3) = d3 * sfreq(n3)
        end if
1950    if (n3 .ge. n19 .and. n3 .le. n20) go to 1960
        if (node(n3) .lt. 0) go to 1960
        crest(n3) = - slip * crest(n3)
        sfreq(n3) = d3 * sfreq(n3)
        if (slip .lt. 0.0) time1(n3) = - time1(n3)
        d4 = time1(n3) * 360.0/twopi
        n6 = node(n3)
        if (n13 .eq. 1) write(lunit6,1952) k
        if (n13 .eq. 1) write(lunit6,1954)
        write(lunit6,1956) bus(n6),crest(n3),sfreq(n3),d4
        if (n13 .eq. n18) write(lunit6,1958)
1952    format(/,45h on completion of the load-flow calculations,,45h the arguments of the type-14 sources used to, &
             42h represent the external voltage sources in,/,    36h the excitation network of um number,i4, &
             35h,  are adjusted by the program as :)
1954    format(9h bus name,2x,14h  amplitude(v),2x, 14h frequency(hz),2x,14h   angle(degr))
1956    format(3x,a6,3e16.5)
1958    format(/)
1960    n3 = kpsour(n3)
        if (n3 .ne. n4) go to 1950
1970 end do
     !  error stop if load flow of im which is not of type 4 and not
     !    symmetrical round rotor :
2000 if (loopss(10) .eq. 0) go to 2008
     if (lfim4i .eq. 0) go to 2008
     if (jtype(k) .ne. 4) go to 2002
     if (reamdu(k) .eq. reamqu(k)) go to 2008
2002 write(lunit6,2004)
2004 format(/,44h error stop . you have requested a load-flow, 45h of a network containing an induction machine, &
          42h which is either not of type 4 or which is, /,43h of type 4 but is not a symmetrical machine, &
          45h with a round rotor. the current emtp version, 42h is not capable of honoring your load flow,/, 9h request.)
     call stoptp
     !  compensation code ************************************* :
2008 if (loopss(10) .eq. 1) go to 2048
     if (imach .ne. 1) go to 2048
     if (iprsup .ge. 1) write (lunit6,2010) k,numsub
2010 format(44h * * * * * * * * * * * * * * * * * * * * * *, 37h begin compensation for ind.machines:, 10h um number,i4,3x, 9h numsub =, i4)
     !     2017 n1 = 0
     n1 = 0
     n18 = 0
     n2 = ncomcl
     if (loopss(8) .eq. 1) go to 2021
     n5 = kcoil(k)
     n9 = n5 + 2
2018 numsub = numsub + 1
     isubeg(numsub) = n15 + 1
     do j = n5,n9
        if (nodvo1(j) .eq. nodvo2(j)) go to 2020
        n15 = n15 + 1
        ncomcl = ncomcl + 1
        n18 = n18 + 1
        ksub(n15) = nodvo1(j)
        msub(n15) = nodvo2(j)
2020 end do
     if (n1 .eq. 1) go to 2025
2021 n7 = kcle - kcoil(k) + 1
     n9 = kcle
     if (n7 .gt. 6) n9 = kcoil(k) + 5
     n5 = kcoil(k) + 3
     n1 = 1
     if (loopss(8) .eq. 1) go to 2022
     if (ncomcl .ne. n2) go to 2022
     isubeg(numsub) = isubeg(numsub) - n15 - 1
     numsub = numsub -1
2022 n18 = 0
     go to 2018
2025 if (n18 .ne. 0) go to 2030
     isubeg(numsub) = isubeg(numsub) - n15 - 1
     numsub = numsub - 1
2030 if (n7 .le. 6) go to 2045
     n5 = kcoil(k) + 6
     do j = n5, kcle
        if (nodvo1(j) .eq. nodvo2(j)) go to 2040
        ncomcl = ncomcl + 1
        numsub = numsub + 1
        isubeg(numsub) = n15 + 1
        n15 = n15 + 1
        ksub(n15) = nodvo1(j)
        msub(n15) = nodvo2(j)
2040 end do
2045 if (iprsup .ge. 1) write (lunit6,2047) numsub
2047 format(44h * * * * * * * * * * * * * * * * * * * * * *,30h * * * * * * * * * * * * * * *, &
          42h * end compensation for ind.mach. numsub =, i4)
2048 ncomum = ncomcl
     if (loopss(10) .eq. 3) go to 2087
     !.  set type 14 current sources and parallel r for power coils:
     !     note : if loopss(8)=0, momentary r for sm and dm only till
     !              completion of all steady-state umrenu passes.
     !            if loopss(8)=1, permanent branches for all um types
     if (imach .eq. 1) go to 2050
     if (loopss(10) .eq. 1) d10 = voltum(k)
     if (loopss(10) .eq. 2) voltum(k) = crest(kconst+1)
     if (loopss(10) .eq. 2) anglum(k) = time1(kconst+1)*360.0/t
2050 do n1 = 1, 3
        n2 = n1 + kcoil(k) - 1
        n4 = 3 * (k - 1)
        if (nodvo1(n2) .eq. nodvo2(n2)) go to 2080
        if (loopss(8) .eq. 0 .and. imach .eq. 1) go to 2055
        call ibrinc
        it = it + 1
        kbus(ibr) = nodvo1(n2)
        mbus(ibr) = nodvo2(n2)
        length(ibr) = 1
        nr(ibr) = - it
        tr(it) = epsiln
        tx(it) = 0.0
        emtpc(it) = 0.0
        if (n1 .eq. 3) hist(kcl) = ibr
        !  load-flow preparation of main inductance of im type-4 :
        !  note : after completion of processing the load-flow, these
        !         branches serve as the power resistance branche across
        !         the terminals of the machine. the fully compensated im
        !         will be provided with these branches only during the
        !         load-flow process and are created further down. they
        !         will be wiped out automatically after completion of
        !         the ld-flow since in reentering the "first part" of
        !         umrenu ibr will be restored to its value as determined
        !         in umdata.
        if (lfim4i .ne. 1) go to 2053
        tr(it) = 0.0
        tx(it) = reamqu(k) * 1.0d+3
        if (xopt .ne. 0.0) tx(it) = reamqu(k) * twopi * xopt
        if (iprsup .ge. 1) then
           write(lunit6,2052) kbus(ibr),mbus(ibr),ibr,it,tr(it),tx(i)
        end if
2052    format(10h *********,32h im main ind branche for ld-flow,2x,i4,4x,i4,2i6,2e14.5)
        go to 2055
        !   set parallel resistances for sm and dm and predicted im :
2053    if (kbus(ibr) .ne. 1 .and. mbus(ibr) .ne. 1) tr(it)=epsiln
        if (loopss(10) .eq. 1) tr(it) = 1.0d+6
        if (imach .eq. 1) tr(it) = 1.0
        if (iprsup .ge. 1) write (lunit6,2054) kbus(ibr),mbus(ibr),ibr,it,tr(it),tx(it)
2054    format(10h *********,31h parall resist. for power coils, 3x,i4,4x,i4,2i6,2e14.5)
        if (loopss(10) .eq. 1) go to 2055
        d10 = voltum(k)/tr(it)
        !   set sources representing the um power coils :
2055    if (imach .eq. 1) d10 = 0.0
        !     2056 do 2070 n3 = 1,2
        do n3 = 1, 2
           if (n3 .eq. 2) go to 2060
           if(nodvo1(n2) .eq. 1) go to 2070
           kconst = kconst + 1
           if ( kconst .le. lexct )  go to 6785
           write (lunit6, 6783)  lexct
6783       format ( 38h overflow list 4 in "umrenu".  lexct =, i5,    18h     stop locally.     )
           call stoptp
6785       n10 = nodvo1(n2)
           node(kconst) = - nodvo1(n2)
           if (loopss(10) .ne. 1) go to 2058
           if (imach .eq. 1) go to 2058
           node(kconst) = int(umcurp(n4 + n1))
2058       crest(kconst) = d10
           go to 2062
2060       if (nodvo2(n2) .eq. 1) go to 2070
           kconst = kconst + 1
           if ( kconst .le. lexct )  go to 6786
           write (lunit6, 6783)  lexct
           call stoptp
6786       n10 = nodvo2(n2)
           node(kconst) = - nodvo2(n2)
           if (loopss(10) .ne. 1) go to 2061
           if (imach .eq. 1) go to 2061
           node(kconst) = int(umcurp(n4 + n1))
2061       crest(kconst) = - d10
2062       iform(kconst) = 14
           if (lfim3 .eq. 1 .and. n1 .eq. 1) hist(kcl+2) = kconst
           d1 = anglum(k) * twopi/360.0
           if (n1 .ne. 2) go to 2064
           d2 = d1 - twopi/3.0
           if (jtype(k) .eq. 2) d2 = d1
           d1 = d2
           go to 2065
2064       if (n1 .ne. 3) go to 2065
           d2 = d1 + twopi/3.0
           if (jtype(k) .eq. 2) d2 = d1 + twopi/4.0
           if (jtype(k) .eq. 5) d2 = d1 + twopi/4.0
           d1 = d2
2065       if (imach .eq. 1) d1 = 0.0
           if (loopss(10) .eq. 2) go to 2066
           time1(kconst) = d1
           tstart(kconst) = - 1.0
           tstop(kconst) = 0.0
           n10 = kssfrq(n10)
           if (n10 .gt. 0) go to 92066
           sfreq(kconst) = rotmom(k)/twopi
           go to 2066
92066      sfreq(kconst) = sfreq(n10)
           !     for load-flow the sm always provide a voltage source to
           !     represent machine, whereas im provides a dummy current
           !     source which will be initialized at a later pass after
           !     destruction of the im load-flow network configuration.
2066       if (loopss(10) .eq. 0) go to 2068
           if (imach .eq. 1) go to 2068
           if (iprsup .ge. 1) write (lunit6,2067) node(kconst),kconst,sfreq(kconst),crest(kconst)
2067       format(10h *********, 30h voltage source for power coil, 4x,i4,50x,i6,e14.5,1x,e13.5)
           go to 2070
2068       if (iprsup .ge. 1) write (lunit6,2069) node(kconst),kconst,sfreq(kconst),crest(kconst)
2069       format(10h *********,30h current source for power coil, 4x,i4,50x,i6,e14.5,1x,e13.5)
2070    end do
        !     for sm and dm : make leakage inductance zero in emtp
        !     network for this next round of ss calculation because
        !     user supplied terminal voltage specification needs to
        !     be matched. on the next round they will be restored.
        !     note: if load-flow is requested, this zeroing will be done
        !     on the second pass through the first part of the
        !     umrenu code.
        !     if ind mach is present and no load-flow requested , t
        !     then this procedure is to be done if loopss(9) = 2,
        !     i.e. in "second part" of the umrenu code.
        if (loopss(10) .eq. 1) go to 2076
        if (imach .eq. 1) go to 2076
        if (loopss(9) .ne. 0) go to 2076
        n10 = int(fpar(kcl) + n1 - 1)
        n5 = - nr(n10)
        if (n5 .lt. 0) n5 = - n5
        tx(n5) = epsiln * 1.0d+3
        if (xopt .ne. 0.0) tx(n5) = epsiln * twopi * xopt
        if (iprsup .ge. 1) write (lunit6,2074) kbus(n10),mbus(n10),n10,n5,tr(n5),tx(n5)
2074    format (32h *********  leakage ind. change:,12x,i4,4x,i4,2i6,2e14.5)
        !  load-flow preparation of im type-4 power series branche :
2076    if (lfim4i .ne. 1) go to 2080
        n10 = int(fpar(kcl) + n1 - 1)
        n5 = - nr(n10)
        if (n5 .lt. 0) n5 = - n5
        tr(n5) = 1.0/gpar(kcl+2)
        if (iprsup .ge. 1) write (lunit6,2078) kbus(n10),mbus(n10),n10,n5,tr(n5),tx(n5)
2078    format (39h ********* im power branche for ld-flow,5x,i4,4x,i4,2i6,2e14.5)
2080 end do
     !  storing permanently created branches (if full compensation is
     !   requested, then all created branches in umrenu are just
     !   temporarily)
     if (loopss(8) .eq. 0) go to 2081
     if (loopss(10) .ne. 1) loopss(5) = ibr
     !   initialization if induction machines
     !     note : for im we choose the torque angle = 0.0
2081 if (loopss(10) .eq. 2) go to 2082
     if (imach .eq. 1) thetam(k) = twopi/(4.0*nppair(k))
     !.  set type 14 sources for um = sm & dm excitation coils
     !    (im excitation coil sources were created in over5)
2082 n5 = kcoil(k) + 3
     if (imach .eq. 1) go to 2087
     if (nodvo1(n5) .eq. nodvo2(n5)) go to 2100
     do n1 = 1, 2
        if (n1 .eq. 2) go to 2083
        if (nodvo1(n5) .eq. 1) go to 2086
        kconst = kconst + 1
        if ( kconst .le. lexct )  go to 6787
6788    write (lunit6, 6783)  lexct
        call stoptp
6787    node(kconst) = - nodvo1(n5)
        go to 2084
2083    if (nodvo2(n5) .eq. 1) go to 2086
        kconst = kconst + 1
        if ( kconst .gt. lexct )  go to 6788
        node(kconst) = - nodvo2(n5)
2084    iform(kconst) = 14
        crest(kconst) = 0.0
        time1(kconst) = 0.0
        tstart(kconst) = - 1.0
        tstop(kconst) = 0.0
        n10 = nodfum(k)
        sfreq(kconst) = sfreq(n10)
        if (iprsup .ge. 1) write(lunit6,2085) node(kconst),kconst,sfreq(kconst),crest(kconst)
2085    format(10h *********,31h current source for excit coils,3x,i4,50x,i6,e14.5,1x,e13.5)
2086 end do
     go to 2100
     !  treatment of  excitation sources of im :
2087 if (loopss(10) .eq. 2) go to 2100
     d3 = slip
     if (d3 .lt. 0.0) d3 = - d3
     d1 = d3 * rotmom(k)/twopi
     if (lfim4i .ne. 0) d1 = rotmom(k)/twopi
     n19 = int(fpar(kcl+1))
     n20 = n19 - 1 + ncld(k) + nclq(k)
     if (jtype(k) .eq. 4) n20 = n19 + 2
     nminum = 10000
     n18 = 0
     do j = kcld1, kcle
        n2 = nodvo2(j)
        if (n2 .eq. 1) n2 = nodvo1(j)
        n3 = kssfrq(n2)
        if (n3 .lt. 0) go to 2094
        n4 = n3
        sfreq(n3) = d1
        if (n3 .ge. n19 .and. n3 .le. n20) go to 2090
        n18 = n18 + 1
        if (j .eq. kcld1) d10 = time1(n3)
        if (j .eq. kclq1) d11 = time1(n3)
        !  reverse excitation voltage polarity due to um sign convention
        if (node(n3) .lt. 0) go to 2090
        if (lfim4i .eq. 0) go to 2090
        crest(n3) = - crest(n3)/slip
        !  negative sequence conversion to positive if slip .lt. 0.0 :
        if (slip .lt. 0.0) time1(n3) = - time1(n3)
2090    if (iprsup .ge. 1) write (lunit6,2088) node(n3),n3,sfreq(n3),crest(n3)
2088    format(10h *********,31h adjusted im excitation sources,3x,i4,50x,i6,e14.5,1x,e13.5)
2089    n3 = kpsour(n3)
        if (n3 .eq. n4) go to 2094
        sfreq(n3) = d1
        if (n3 .ge. n19 .and. n3 .le. n20) go to 2093
        n18 = n18 + 1
        if (j .eq. kcld1) d10 = time1(n3)
        if (j .eq. kclq1) d11 = time1(n3)
        if (node(n3) .lt. 0) go to 2089
        if (lfim4i .eq. 0) go to 2089
        crest(n3) = - crest(n3)/slip
        if (slip .lt. 0.0) time1(n3) = - time1(n3)
        if (nminum .gt. n3) nminum = n3
2093    go to 2090
2094 end do
     !  check for correct phase sequence of external exc sources :
     if (imach .ne. 1) go to 2097
     if (n18 .eq. 0) go to 2097
     d12 = d10 - d11
     d13 = twopi/36.0
     d14 = twopi/3.0
     if (jtype(k) .eq. 3) d14 = - twopi/4.0
     d15 = 2.0 * d14
     if (jtype(k) .eq. 3) d15 = - d14
     if (slip .gt. 0.0) go to 2096
     d16 = - d14 - d13
     d17 = - d14 + d13
     if (d12 .gt. d16 .and. d12 .lt. d17) go to 2097
     d16 = d15 - d13
     d17 = d15 + d13
     if (d12 .gt. d16 .and. d12 .lt. d17) go to 2097
     write (lunit6,2095) k
2095 format(/,22h error stop. um number,i4,16h is an ind. mach,42h and is requested to be initialized with a, &
          43h negative slip value. the phase sequence of,/,    45h the specified external excitation sources is, &
          42h incorrect. it should be negative sequence,31h rather than positive sequence.)
     call stoptp
2096 d16 = d14 - d13
     d17 = d14 + d13
     if (d12 .gt. d16 .and. d12 .lt. d17) go to 2097
     d16 = - d15 - d13
     d17 = - d15 + d13
     if (d12 .gt. d16 .and. d12 .lt. d17) go to 2097
     write (lunit6,82096) k
82096 format(/,22h error stop. um number,i4,16h is an ind. mach,42h and is requested to be initialized with a, &
           43h positive slip value. the phase sequence of,/,    45h the specified external excitation sources is, &
           42h incorrect. it should be positive sequence,31h rather than negative sequence.)
     call stoptp
2097 if (nminum .ge. 10000) go to 2100
     if (node(nminum) .gt. 0) go to 2100
     if (lfim4i .eq. 0) go to 2100
     write(lunit6,2098)
2098 format(/,43h warning :   you have requested a load-flow,38h for a network containing an induction, &
          42h machine. you chose the exciter sources as, /,     42h current sources and will therefore not be, &
          34h adjusted by the load-flow module.,/)
2100 if (jtmtac(k) .le. 0) go to 2150
     write (lunit6,2110)
2110 format(24h0program stop because of, 26h conflicting request, i.e.)
     write (lunit6,2120)
2120 format(35h no initialization if no mechanical,15h network option)
     call stoptp
     !   creating temporary speed-source for each um mass :
2150 if (loopss(10) .eq. 3) go to 2200
     kconst = kconst + 1
     if ( kconst .ge. lexct )  go to 6788
     iform(kconst) = 14
     node(kconst) = nodom(k)
     n1 = jcltac(kcl)
     n2 = jcltac(kcl+1)
     if (n1 .eq. 0 .and. n2 .eq. 0) go to 2154
     n4 = k
     if (n1 .ne. 0 .and. k .gt. n1) n4 = n1
     if (n2 .ne. 0 .and. n4 .gt. n2) n4 = n2
     if (k .eq. n4) go to 2154
     node(kconst) = - jtmtac(k)
2154 tstart(kconst) = -1.0
     tstop(kconst) = 0.0
     n10 = nodmum(k)
     sfreq(kconst) = sfreq(n10)
     crest(kconst) = 0.0
2160 format(10h *********,21h um mass speed-source,13x,i4,50x,i6,e14.5,1x,e13.5)
     if (iprsup .ge. 1) write(lunit6,2160) node(kconst),kconst,sfreq(kconst),crest(kconst)
2200 end do
  !  load-flow preparation of im type-4 temporary branches, which
  !   are to be wiped out in the second part of the umrenu code :
  if (loopss(10) .ne. 1) go to 2250
  do 2248 k = 1,numum
     if (jtype(k) .lt. 3) go to 2248
     if (jtype(k) .gt. 4) go to 2248
     slip = voltum(k)/100.0
     if (iprsup .ge. 1) write(lunit6,2201)
2201 format(/,40h ***************************************,43h momentary changes or creation of momentary, &
          44h elements for load-flow involving ind. mach.)
     if (iprsup .ge. 1) write(lunit6,1904) k
     kcl = kcoil(k)
     ncl = ncld(k) + nclq(k)
     if (jtype(k) .eq. 3) go to 2226
     if (jtype(k) .eq. 4) ncl = ncl + 1
     !  set temporary im rotor branches for ld-flow :
     ndum(31) = nodvo1(kcl+5)
     ndum(32) = nodvo1(kcl+3)
     ndum(33) = nodvo1(kcl+4)
     ndum(34) = nodvo2(kcl+5)
     ndum(35) = nodvo2(kcl+3)
     ndum(36) = nodvo2(kcl+4)
     do n1 = 31, 33
        n3 = ndum(n1)
        n4 = ndum(n1+3)
        !     2202 call ibrinc
        call ibrinc
        it = it + 1
        n5 = kcl + n1 - 31
        kbus(ibr) = nodvo1(n5)
        if (nodvo1(n5) .eq. 1) kbus(ibr) = nodvo2(n5)
        mbus(ibr) = n3
        if (n4 .ne. 1) mbus(ibr) = n4
        length(ibr) = 1
        nr(ibr) = - it
        tr(it) = 1.0/(gpar(kcl+4) * slip)
        emtpc(it) = 0.0
        tx(it) = reacl(kcl+4) * 1.0d+3
        if (xopt .ne. 0.0) tx(it) = reacl(kcl+4) * twopi * xopt
        if (iprsup .ge. 1) write(lunit6,2208) kbus(ibr),mbus(ibr),ibr,it,tr(it),tx(it)
2208    format(10h *********,34h im rotor branche just for ld-flow,i4,4x,i4,2i6,2e14.5)
2220 end do
     if (ndum(33) .ne. 1 .and. ndum(36) .ne. 1) go to 2222
     if (nodvo1(kcl) .eq. 1 .or. nodvo2(kcl) .eq. 1) go to 2226
2222 write(lunit6,2224)
2224 format(/,43h error stop. you have requested a load-flow,45h of a network containing an induction machine, &
          35h with ungrounded power coils and/or, /,    38h excitation coils. this request is not, &
          43h honored in the current emtp version. these, 38h coils are both to be y-connected with, &
          19h grounded neutrals.)
     call stoptp
     !  set temporary branches for im main inductances if full comp :
2226 if (loopss(8) .ne. 0) go to 2230
     do n1 = 1, 3
        n2 = n1 + kcl - 1
        call ibrinc
        it = it + 1
        if (n1 .eq. 3) hist(kcl) = ibr
        kbus(ibr) = nodvo1(n2)
        mbus(ibr) = nodvo2(n2)
        length(ibr) = 1
        nr(ibr) = - it
        tr(it) = 0.0
        emtpc(it) = 0.0
        if (jtype(k) .ne. 3) go to 2227
        tr(it) = epsiln
        tx(it) = 0.0
        if (iprsup .ge. 1) write(lunit6,2054) kbus(ibr),mbus(ibr),ibr,it,tr(it),tx(i)
        go to 2228
2227    tx(it) = reamqu(k) * 1.0d+3
        if (xopt .ne. 0.0) tx(it) = reamqu(k) * twopi * xopt
        if (iprsup .ge. 1) write(lunit6,2052) kbus(ibr),mbus(ibr),ibr,it,tr(it),tx(i)
2228 end do
     if (jtype(k) .eq. 3) go to 2250
     !  adjusting external excitation network resistances of im into
     !    stator frequency domain in case of load-flow request :
2230 do n1 = 1,40
        ndum(n1) = 0
     end do
     n1 = 0
     do n13 = 1, ncl
        n14 = kcl + 2 + n13
        n2 = nodvo1(n14)
        if (n2 .eq. 1) n2 = nodvo2(n14)
        if (n2 .eq. 1) go to 2248
        n3 = kssfrq(n2)
        n4 = n3
2240    n1 = n1 + 1
        ndum(n1) = n3
        n3 = kpsour(n3)
        if (n3 .ne. n4) go to 2240
2242 end do
     n13 = loopss(5)
     vinp(1) = 0.0
     n18 = 1
     if (n1 .lt. 40) go to 82242
     write(lunit6,82245) k
     call stoptp
82242 do n10 = 1,n13
        n11 = kbus(n10)
        if (n11 .lt. 0) n11 = - n11
        n12 = mbus(n10)
        if (n12 .lt. 0) n12 = - n12
        n3 = kssfrq(n11)
        !   now determine branches in exc subnetwork with sources ndum
        do n8 = 1, n1
           if (n3 .ne. ndum(n8)) go to 2244
           n9 = nr(n10)
           if (n9 .lt. 0) n9 = - n9
           !   avoid reinitialization of referenced excit branches :
           n17 = 0
           do n19 = 1,n18
              n20 = int(vinp(n19))
              if (n9 .eq. n20) n17 = 1
82243      end do
           if (n17 .eq. 1) go to 82244
           n18 = n18 + 1
           vinp(n18) = n9
           tr(n9) = tr(n9)/slip
82244      if (n18 .lt. 40) go to 82246
           write(lunit6,82245) k
82245      format(/,22h error stop. um number,i4,14h has more than, 44h 40 excitation branches or sources. overflow, &
                42h is avoided by increasing the dimension of ,/,   45h the arrays "vinp" and "ndum" in umdeck to be, &
                37h one higher than these used elements.)
           call stoptp
82246      if (iprsup .ge. 1) write(lunit6,2243) n11,n12,n10,n9,tr(n9),tx(n9)
2243       format(10h *********,34h change ext exc r just for ld-flow,i4,4x,i4,2i6,2e14.5)
           go to 2245
2244    end do
2245 end do
2248 end do
  !  ld-flow conditions if ld-flow not yet conducted ******** :
  !  (a) condition of first umrenu pass, unless um type-3 is prese
  !      which would make it the second pass.
2250 if (loopss(10) .ne. 1) go to 2260
  loopss(10) = 2
  if (lfim3 .eq. 1) return
  go to 2400
  !  (b) condition of first pass through "first part" if um type-3
  !      is present.
2260 if (loopss(10) .ne. 3) go to 2280
  istep = 0
2280 if (ncomum .eq. 0) ksubum = ksubum - numsub - 1
  if (ncomum .eq. 0) go to 2300
  loopss(2) = 3 * ntot
  isubeg(numsub+1) = n15 + 1
2300 if (loopss(10) .eq. 3) go to 2400
  if (loopss(10) .eq. 2) loopss(10) = 0
  !  detecting presence of zero component current :
  if (loopss(8) .eq. 0) go to 2400
  do n5 = 1, numum
     n1 = - 9999
     n3 = kcoil(n5)
     if (nodvo2(n3+2) .eq. 1) n1 = 0
     if (nodvo1(n3+2) .eq. 1) n1 = 0
     if (n1 .eq. 0) go to 2350
     do n2 = 1, ibr
        if (kbus(n2) .ne. nodvo2(n3+2)) go to 2310
        if (mbus(n2) .eq. nodvo1(n3+2)) go to 2310
        n1 = mbus(n2)
2310 end do
     if (n1 .eq. -9999) gpar(n3) = 0.0
     if (n1 .eq. -9999) reacl(n3) = 0.0
2350 end do
2400 return
end subroutine umrnu2
!
! subroutine equiv.
!
subroutine equiv(req,xeq,geq,beq,z,r,tau,w,n)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  dimension req(1), xeq(1), geq(1), beq(1), z(1), r(1), tau(1)
  equivalence (anumr, rzero), (anumx, xzero), (ypos, sinb), (yneg, cosb), (epos, sinhgr), (eneg, sinhgi), (c, cosha), (d, sinha)
  do l=1, n
     j=l
     a = w * tau(j)
     zs=z(j)
     rs=r(j)
     if ( iprsup  .ge.  3 ) write (lunit6, 1786)  l, n, w, tau(j), zs, rs
1786 format ( /,  28h within l-loop of  'equiv' ., 16h       l       n,  15x,  1hw,  10x,  6htau(j), 14x,  2hzs,  14x,  2hrs  ,/,  28x,  2i8,  4e16.7  )
     if( rs .ge. 0.0 )  go to 1
     rs = -rs
     denr = a * zs
     denx = a / zs
     piov2 = twopi * 0.25
     d1 = sqrtz ( rs**2  +  denr**2 )
     d2 = atan2z ( denr, rs )
     d3 = 0.5 * ( d2 - piov2 )
     d4 = sqrtz ( d1 / denx )
     rzero = d4 * cosz(d3 )
     xzero = d4 * sinz( d3 )
     d3 = d3 + piov2
     d4 = sqrtz ( d1 * denx )
     e = d4 * cosz(d3)
     a = d4 * sinz(d3)
     ypos = expz(e)
     yneg = 1.0 / ypos
     cosha = ( ypos + yneg ) / 2.0
     sinha = cosha - yneg
     sinb = sinz(a)
     cosb = cosz(a)
     sinhgr = sinha * cosb
     sinhgi = cosha * sinb
     denr = rzero * sinhgr  -  xzero * sinhgi
     denx = rzero * sinhgi  +  xzero * sinhgr
     anumr = cosha * cosb  -  1.0
     anumx = sinha * sinb
     go to 2
1    e = rs / zs
     if(zs.lt.0.) go to 3
     a=a / 2.0
     c=sinz(a)
     d=cosz(a)
     e2=e**2
     c2=c**2
     denr  =  ( 48. + e2 ) / 32.
     denr  =  ( 1.0  - c2 * denr ) * rs
     e2div8 = e2 / 8.0
     denx=( 3.0*e2div8 +2.0)*c*d*zs
     anumr=(-2.0- e2div8 )*c2
     anumx=e*c*d
2    e = denr**2  +  denx**2
     req(l)=denr
     xeq(l)=denx
4    geq(l)=(anumr*denr+anumx*denx)/e
     beq(l)=(anumx*denr-anumr*denx)/e
     go to 5
3    e=e / 2.0
     c=sinz(a)
     d=cosz(a)
     ypos=expz(-e)
     yneg=1.0/ypos
     epos=ypos+yneg
     eneg=ypos-yneg
     anumr=d*epos-2.
     anumx=c*eneg
     denr=d*eneg
     denx=c*epos
     tden=(denr*denr+denx*denx)
     e = - zs / 2.0
     req(l) = e * denr
     xeq(l) = e * denx
     e=-zs*tden
     go to 4
5 end do
  return
end subroutine equiv
!
!     end of file: over8.for
!
