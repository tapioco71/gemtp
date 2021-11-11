!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over39.f90
!

!
! subroutine subr39.
!

subroutine subr39
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labl39.ftn'
  include 'deck39.ftn'
  dimension lltemp(20)
  equivalence (kdeflt, indtv(1))
  character(8) :: text1, text2, text3, text4, text5, text6
  character(8) :: text7, text8, text9, text10, text11, text12
  character(8) :: texta, textp, text13, text14, text15
  dimension texta(14), textp(14)
  dimension alintp(4100)
  dimension minust(11)
  dimension akfrs1(100), akfrs2(100), alphs1(100), alphs2(100)
  data  text1   /  'line  ' /
  data  text2   /  'consta' /
  data  text3   /  'nts   ' /
  data  text4   /  'cable ' /
  data  text5   /  'lc    ' /
  data  text6   /  'cc    ' /
  data  text7   /  'old   ' /
  data  text8   /  'data  ' /
  data  text9   /  'new   ' /
  data  text10  /  'rho   ' /
  data  text11  /  'branch' /
  data  text12  /  'common' /
  data  text13  /  'cards ' /
  data  text14  /  'printe' /
  data  text15  /  'r plot' /
  if ( iprsup  .ge.  1 ) write (lunit6, 1546)  lastov
1546 format (  ' Top of "subr39".  lastov =',  i6 )
  if (lastov .eq. 1 )  go to 5657
  onehav = onehaf
  oneqtr = 0.25d0
  ifqdpt = 1
  pai = twopi/2.
  tendeg = pai / 18.
  ! ==== change these limits if dimensions in labl39 are changed:
  !   number of modes:
  mmodes = 18
  !   curve zones = peaks+valleys+1 :
  !   xknee(mxknee),noprao(mxknee)
  !     thl + edwin + wsm, 1 aug 1986.  change "labl39" arrays 20 to 100:
  mxknee = 100
  !   order of the approximation = no. of poles :
  !   xauxd(2*mpoles),fcz(mpoles),fcp(mpoles),
  !   fczr(mpoles),fcpr(mpoles),indxv(mpoles),
  !   akfrac(mpoles),alphaf(mpoles)
  mpoles = 100
  !   dimension of xchkra = 2*sum(2**i), i=0,n ; n such that 2**n.ge.mpole
  !   xchkra(mxchkr)
  mxchkr = 255
  !   dimension of zoprau, zoprao, azepo = 4*mpoles  :
  !   zoprau(mzopra),zoprao(mzopra),azepo(mzopra)
  mzopra = 400
  ! ====  end of dimension-limit assignments ("labl39" storage)
  lunt35 = 35
  llm1 = -1
  ll4 = 4
  jdatcs = 0
  call dimens ( lltemp(1), nchain, trash, trash )
  if ( iprsup .ge. 1) write (6, 7001)   lltemp(1), lltemp(2)
7001 format ( /, 1x, 'lltemp(1:2) =',  2i8  )
  n3 = 0
  do i=1, 9999, 2
     if ( n3  .ge.  1 )   go to 1550
     if ( lltemp(i)  .ne.  71 )   go to 5654
     mdapts = lltemp(i+1)
     n3 = n3 + 1
5654 end do
  call stoptp
1550 n13 = 0
  kdeflt = 0
  if(lastov.ne.1) go to 4040
5657 ialdum = 0
  ialter = 1
  do  i=1, 24
7004 vstacs(i) = blank
  end do
  sglfir = 0.2d0
  m = 1
  !     read input card using cimage
4040 call cimage
  nright = -2
  n4 = kolbeg
  kolbeg = 1
  !  call freone (d1)
  call free (d1)
  if (kill .gt. 0) go to 9200
  nright = 0
  if ( texta6(1)  .ne.  text11 )   go to 7618
  !     optional "branch" card giving pairs of node names
  n1 = m + 11
  read (unit = abuff, fmt = 7609) (vstacs(i), i = m, n1)
7609 format (8x,12a6)
  if (iprsup .ge. 6 ) write (lunit6, 7613) m, (vstacs(i), i = m, n1)
7613 format ( ' vstacs names:',  12a6  )
  m = m + 12
  write (kunit6, 7616)
7616 format ('+bus names for each phase.')
  go to 4040
7618 if(texta6(1).ne.text7 ) go to 4042
  if(texta6(2).ne.text8 ) go to 4042
  ialdum = 3
  ialter = 3
  if ( kolbeg  .gt.  0 )   go to 7623
  read (unit = abuff, fmt = 7622) dist
7622 format ( 24x,  e8.0 )
  go to 7624
7623 nfrfld = 1
  !  call freone (dist)
  call free (dist)
7624 if (metrik .eq. 0 .and. n13 .ne. 8765) dist = dist * 1.60935d0
  write (unit = kunit6, fmt = 7625) dist
7625 format ('+Same line, but with new length =', f9.2,   '  km.'   )
  go to 4040
4042 if (to_lower (texta6(1)) .ne. text9) go to 4043
  if(texta6(2).ne.text10) go to 4043
  !     temporary new rho area - to be completed later, someday
4043 if ( texta6(1) .ne. text14 )  go to 8819
  if ( texta6(2) .ne. text15 )  go to 8819
  read (unit = abuff, fmt = 7622) sglfir
  write (kunit6, 8814) sglfir
8814 format ( '+new log-f/line of printer plot.  sglfir =', f8.4 )
8819 continue
  read (unit = abuff, fmt = 1019) (texta(k), k = 1, 14)
1019 format ( 13a6, a2 )
  do k=1, 14
     if ( texta(k)  .ne.  blank )  go to 4044
1023 end do
  write (kunit6, 1719)
1719 format ('+Blank card ending Marti setup' )
  call interp
  go to 9200
  !     check for key word  'line constants'  (or  'lc' ).
4044 if ( texta6(1)  .eq.  text5 )   go to 1706
  if ( texta6(1)  .ne.  text1 )   go to 4049
  if ( texta6(2)  .ne.  text2 )   go to 4049
  if ( texta6(3)  .ne.  text3 )   go to 4049
1706 if ( iprsup .ge. 1 ) write (lunit6, 4045)
4045 format (" Transfer to  'line constants' .")
  lastov = nchain
  nchain = 44
  go to 9999
  !     check for key word  'cable constants'  (or  'cc' ).
4049 if ( texta6(1)  .eq.  text6 )   go to 1716
  if ( texta6(1)  .ne.  text4 )   go to 4055
  if ( texta6(2)  .ne.  text2 )   go to 4055
  if ( texta6(3)  .ne.  text3 )   go to 4055
1716 write (kunit6, 4053)
4053 format ("+transfer to  'cable constants' .")
  lastov = nchain
  nchain = 47
  go to 9999
4055 if( texta6(1) .ne. text8 )   go   to  4057
  if( texta6(2) .ne. text13 )   go   to  4057
  !     read data card using cimage
  call cimage
  read (unit = abuff, fmt = 4027) ktab, imodal, metrik, d9
4027 format (3i8,e8.0)
  ck1 = d9
  if( metrik .eq. 0 )  ck1 = ck1 * .62135896d0
!!!      write (*,*) ' subr39.  ready to to rewind lunit9 =',
!!!     1                                          lunit9
  rewind lunit9
  !     write five dummy zeros
  d11 = 0.
  write (lunit9) d11, d11, d11, d11,d11
  rewind lunit9
  write (lunit9) imodal, metrik, d9, mspedb, itrnsf
  !     read data card using cimage
  call cimage
  read (unit = abuff, fmt = 4029) (voltbc(i), i = 1, 3)
4029 format ( 5e16.0)
  !     read in the current transformation matrix   *  *  *  *  *  *  *
  if( imodal .eq. 0 ) go to 4033
  do j = 1, ktab
     do i = 1, ktab, 6
        ij = i + 5
        if( ij .gt. ktab ) ij = ktab
        !     read data card using cimage   *   *   *   *   *   *   *   *   *
        call cimage
        read (unit = abuff, fmt = 4023) (tir(j, kp), kp = i, ij)
4023    format ( 6e12.0 )
4021 end do
     do i = 1, ktab, 6
        ij = i + 5
        if( ij .gt. ktab ) ij = ktab
        !     read data card using cimage   *   *   *   *   *   *   *   *   *
        call cimage
        read (unit = abuff, fmt = 4023) (tii(j, kp), kp = i, ij)
4022 end do
!!!      write (*,*) ' subr39.  tir, tii on  lunit9.'
     write (lunit9) (tir(j, kp), tii(j, kp), kp = 1, ktab)
4026 end do
  !     read modal g, b, r, x    *   *   *   *   *   *   *   *   *   *   *
  !     read data card using cimage
4033 call cimage
  read (unit = abuff, fmt = 4029) (alinvc(i), i = 1, 5)
  if( alinvc(ll4) .le. 0. )   go  to  4034
!!!      write (*,*) ' subr39.  alinvc on lunit9.'
  write (lunit9) (alinvc(i), i = 1, 5)
  go to   4033
4034 rewind lunit9
  !     advance the card reader to conform to misc39's requirements
  call cimage
  jdatcs = 2
  !     end key-word section, prepare for misc. data card:
4057 do i=1, 2
     do j=1, 9
4058    modskp(i,j) = 0
     end do
  end do
  modesk = 0
  rewind lunit9
!!!      write (*,*) ' subr39.  ready to read imodal, etc. from 9.'
  read (lunit9)  imodal, metrik, d9, mspedb, itrnsf
  modify = 0
  !   ==  read miscellaneous data card by call to  "misc39" :    ==
  call misc39
!!!      write (*,*) ' subr39.  imodal, modesk, ialdum, ialter, ktab =',
!!!     1                       imodal, modesk, ialdum, ialter, ktab
  if ( modesk  .eq.  1 )   go to 4065
  do i=1, 2
     do j=1, 9
4061    modskp(i,j) = 1
     end do
  end do
4065 if ( ialdum  .ne.  3 )  dist = d9
  !     again, the usage of 'ialter' is restored here
  if (ialter .ne. 3 )  dist = d9
  if ( imodal  .eq.  0  .or. itrnsf .eq. 1)   go to 8215
  do ip = 1, ktab
     read (lunit9) ( tir(ip,jp), tii(ip,jp), jp=1, ktab )
     if( iprsup .lt. 9 ) go to 8203
     write (lunit6, 9436) ip, (tir(ip, jp), jp = 1, ktab)
9436 format (  ' row',  i3,  '.',  9e13.5  )
     write (lunit6, 9436) ip, (tii(ip, jp), jp = 1, ktab)
8203 end do
  !
  !   === miscellaneous initializations ===
8215 lout = lunit6
  nmode = ktab
  nfitmx = 0
  if (nmode.gt.mmodes) go to 2170
  convun = 1.d0
  if (metrik.eq.0) convun = 1.60935d0
  modify = 1
  nexmis = 1
  aptdec = voltbc(6)
  npoint = voltbc(4)
  if (npoint.gt.mdapts) go to 2100
  !   == fixed parameters ==
  !    separation between consecutive poles or zeroes (.1%)
  d13 = 1.0 + tenm3
  deminp = alog1z ( d13 )
  !    initial separation from horizontal level (1.%)
  clear = tenm3 * 10.
  !   == prepare images of punch file ==
  kount = 0
  rewind lunit1
  if ( ipunch  .eq.  0 ) write (lunit7, 8769) tclock, date1
8769 format ('c    punched card output of "jmarti setup" which began at',  2x,  2a4,  2x,  2a4  )
  if ( ipunch .ne. 0 )  go to 8789
  if ( imodal .eq. 1 ) write (lunit7, 8770)
8770 format ('c   ***** untransposed jmarti line segment ******'    )
  if ( imodal .eq. 0 .and. mspedb .eq. 0) write (lunit7, 8780)
8780 format ('c   *****  transposed jmarti line segment  ******'    )
  if ( imodal .eq. 0 .and. mspedb .eq. 1) write (lunit7, 8785)
8785 format ('c   ***** special double circuit transposed jmarti line segment ******'           )
8789 rewind lunit2
  n5 = 0
  if ( jdatcs .gt. 0 )  go to 8777
  do n12=1, 9999
     read (lunit2, 8771)  ( texta6(i), i=1, 14 )
8771 format ( 13a6, a2 )
     if ( ipunch  .eq.  0 ) write (lunit7, 8772) (texta6(i), i = 1, 13)
8772 format ( 'c ',  13a6 )
     if (idebug.eq.0) go to 8100
     write (lunit1, 8773)  (texta6(i), i = 1, 14)
8773 format (' c ',  13a6,  a2 )
     kount = kount+1
8100 if ( texta6(1) .eq. blank     .and. texta6(2) .eq. blank )   n5 = n5 + 1
     if ( n5  .ge.  2 )   go to 8777
8774 end do
  !
  !             modes fitting loop
  !
8777 d13 = 1.005d0
  d1lim = alog1z ( d13 )
  d13 = 1.0025d0
  d2lim = alog1z ( d13 )
  do imode = 1,nmode
     rewind lunit9
     !  ??? dummy read to skip header ???
     read(lunit9) id1,id1,d1, id1, id1
     if ( imodal .eq. 0  .or. itrnsf .eq. 1)  go to 3205
     do ip = 1, ktab
3230    read(lunit9) (tdum(ip,jp),tdum(ip,jp),jp=1,ktab)
     end do
3205 if (itrnsf .eq. 1) read(lunit9) (lltemp(i), i =1,ktab)
     !  ??? end of dummy read ???
     do ipoint = 1, npoint
        do i = 1, nmode
           read (lunit9) twopif, ycharm, ychara, alpha,beta
           if ( itrnsf .ne. 1 )  go to 3213
           read (lunit9) (tir(j,i),tii(j,i), j=1,nmode)
3213       if ( imode .ne. i )  go to 3215
           if ( nfitmx .eq. 1 )  go to 8210
           index = (ipoint-1) * 5
           alintp(index+1) = twopif
           alintp(index+2) = ycharm
           alintp(index+3) = ychara
           alintp(index+4) = alpha
           alintp(index+5) = beta
           if (iprsup .le. 1 )   go to 3215
           write (lunit6, *) ' %% in subr39, omega, ychar(mag, ang), alpha, beta', alintp(index + 1), alintp(index + 2), alintp(index + 3), &
                alintp(index + 4), alintp(index + 5)
           go to 3215
8210       index = (2*nmode+1) * ( ipoint-1 )
           alintp(index+1) = twopif
           do j = 1, nmode
              l = 2 * (j-1) + 1
              alintp(index+l+1) = tir(j,i)
              alintp(index+l+2) = tii(j,i)
3214       end do
           if( iprsup .ge. 1) write (lunit6, *) ' ti for column ', imode, 'are ', (tir(j, i), tii(j, i), j = 1, nmode)
3215    end do
3220 end do
     !             yc & a1 & ti functions
     ncurve = 2
     if ( nfitmx .eq. 1 )  ncurve = nmode
     do i = 1, ncurve
123     minust(i) = 0
     end do
     do icurve = 1, ncurve
        call time44 ( tclock(1) )
        write (lunit6, 6301) tclock, imode, ncurve
6301    format ( ' begin modal fit at ', 2a4, '.     imode, ncurve =', 2i5)
        numone = 0
        numzro = 0
        iftype = icurve
        if ( nfitmx .eq. 1 )  iftype = 1
        !
        !             read parameters
        !
        if (icurve.eq.2 .and. nfitmx .eq. 0) go to 3150
        if (imode.ne.nexmis) go to 3152
        call misc39
        epstzc = epstol
        normzc = normax
        iecozc = iecode
        ifwtzc = ifwta
        ifplzc = ifplot
        ifdazc = ifdat
        inelzc = inelim
3152    if (modskp(1,imode).eq.0) go to 3200
        if ( nfitmx .eq. 0 )  go to 3156
        write (lunit6, 3155) icurve, imode
3155    format (//,' @@@ begin ti fitting for element:', 2i2, 3x, '@@@'  )
        if ( lltemp(imode) .ne. icurve )  go to 3160
        write (lunit6, *) ' this tij = (1., 0.) '
        go to 8792
3156    write (lunit6, 7007) imode
7007    format (//, 1x, '@@@ begin  yc  fitting for mode',i3,3x, '@@@', 1x)
        go to 3160
3150    if (imode.ne.1) go to 3170
        if (nexmis.eq.-1) go to 3170
        write (lunit6, 7009) nexmis
7009    format (//, 1x, '*** error stop. User has not requested the reading of miscellaneous data for mode 1 of a1 fitting. nexmis =', i5)
        call stoptp
3170    if (imode.ne.-nexmis) go to 3180
        !     this mode of a1 has new miscellaneous data:
        modify = 2
        call misc39
        epsta1 = epstol
        norma1 = normax
        iecoa1 = iecode
        ifwta1 = ifwta
        ifpla1 = ifplot
        ifdaa1 = ifdat
        inela1 = inelim
3180    if (modskp(2,imode).eq.0) go to 3200
        write (lunit6, 7012) imode
7012    format (//, 1x, '@@@ begin  a1  fitting for mode', i3, 3x, '@@@', 1x)
3160    if (icurve.eq.2 .and. nfitmx .eq. 0) go to 3162
        epstol = epstzc
        normax = normzc
        iecode = iecozc
        ifwta = ifwtzc
        ifplot = ifplzc
        ifdat = ifdazc
        inelim = inelzc
        go to 3164
3162    epstol = epsta1
        normax = norma1
        iecode = iecoa1
        ifwta = ifwta1
        ifplot = ifpla1
        ifdat = ifdaa1
        inelim = inela1
3164    if (normax.le.mpoles) go to 3165
        write (lunit6, 7015) normax, mpoles
7015    format (//, 1x, '%%% specified maximum order (', i3, ') exceeds dimensions limit (', i3, ').', /, 1x, &
             'maximum order will be set equal to this limit %%%', 1x)
        normax = mpoles
        !
        !             read data points
        !
3165    if (ifdat.eq.0) go to 3210
        write (lunit6, 7017)
7017    format (//, 1x, 30x, '###  d a t a  f u n c t i o n  ###')
        if (icurve .ge. 2 .or. nfitmx .eq. 1) go to 3167
        write (lunit6, 7019)
7019    format (//, x, ' units: freq in hz; yc in mhos, phyc in degrees' )
        write (lunit6, 7021)
7021    format (/, 1x, 4x, 'freq', 8x, 'yc', 8x, 'phyc', 7x, 'freq')
        go to 3210
3166    write (lunit6, 7024)
7024    format (//, 1x, 'units: freq in hz; ti unitless, phti in degrees' )
        write (lunit6, 7027)
7027    format (/, 1x, 4x, 'freq', 8x, 'ti', 8x, 'phti', 7x, 'freq')
        go to 3210
3167    if ( nfitmx .eq. 1 )  go to 3166
        write (lunit6, 7029)
7029    format (//, 1x, ' units: freq in hz; velocity in km/sec, traveling time in millisec, a1 in per unit, pha1 in degrees' )
        write (lunit6, 7031)
7031    format (/, 5x, 'freq', 4x, 'velocity', 3x, 'trav.time', 5x, 'a1', 8x, 'pha1', 7x, 'freq')
3210    do ipoint = 1,npoint
           if (ipoint.eq.1)     f0=alintp(1)
           index = (ipoint-1) *  5
           if ( nfitmx .eq. 1 )  index =( 2*nmode + 1 )* (ipoint-1)
           fdat = alintp(index+1)
           if (icurve .ge. 2 .or. nfitmx .eq. 1) go to 3110
           adat = alintp(index+2)
           !  === value of g directly in the modal domain ===
           !     if (gdat.eq.0.) gdat = gmode/convun
           phdat = alintp(index+3)
           !  === evaluate line functions ===
           if (icurve .ge. 2 .or. nfitmx .eq. 1) go to 3110
3102       if (ifdat.eq.0) go to 3120
           d3 = phdat * 360. / twopi
           write (lunit6, 7034) fdat, adat, d3, fdat
7034       format (1x, 4d11.4)
           go to 3120
3104       l = index + 2 * (icurve-1) + 2
           adat = alintp( l )
           if ( ipoint .eq. 1 )  asave = adat * tenm3
           if ( adat .lt. asave ) go to 3130
           phdat = alintp(l + 1 )
           if (ifdat.eq.0) go to 3103
           d3 = phdat * 360. / twopi
           write (lunit6, 7034) fdat, adat, d3, fdat
3103       if ( absz(adat - 1.) .lt. epsiln )  numone = numone + 1
           !     if ( (180. - absz( phdat*360./twopi )) .gt. 10. )  go to 3120
           if ( minust(icurve) .eq. 1 ) go to 3105
           if ( (pai - absz(phdat)) .gt. tendeg )  go to 3106
           if ( ipoint .gt. 1 )  go to 3106
           minust(icurve) = 1
3105       phdat = phdat + pai
3106       if ( adat .lt. tenm3)  numzro = numzro + 1
           go to 3120
           !  ### evaluation of the propagation function a1 ###
3110       if (nfitmx .eq. 1 )  go to 3104
           alpha = alintp(index+4)
           beta = alintp(index+5)
           if ( iprsup .ge. 1 ) write (lunit6, *) ' alpha, beta = ', alpha, beta
           adat = expz(-alpha*dist)
           if (adat.gt.1.5d0) go to 3132
           if (adat.lt.amina1) go to 3130
           phdat = -beta*dist
           if (ifdat.eq.0) go to 3120
           d3 = fdat/beta
           d4 = (dist/d3)*1.d3
           d5 = phdat*360.d0/twopi
           write (lunit6, 7039) fdat, d3, d4, adat, d5, fdat
7039       format (1x, 6d11.4)
3120       d11 = fdat/twopi
           xdat(ipoint) = alog1z(d11)
           ydat(ipoint) = alog1z(adat)
           aphdat(ipoint) = phdat
3100    end do
        n9 = .75 * npoint
        if ( numone .lt.  n9 )  go to 3128
        write (lunit6, *) ' This is a near-one tij element. '
        go to 8792
3128    if ( numzro .ge.  n9 )  go to 8793
        !
        ndata = npoint
        go to 3140
3130    ndata = ipoint-1
        go to 3140
3132    write (lunit6, 7037) imode
7037    format (//, 1x, 25x, '??????????  a t t e n t i o n  ??????????', /, &
             1x, 'The magnitude of the line propagation function (a1) for mode', &
             i3, 3x, 'attains values larger than 1.', /, 1x, &
             'This condition is probably caused by the limitations of using a constant complex transformation matrix, ', /, &
             1x, 'to diagonalize the line matrices at all frequencies.', &
             'Nevertheless, the function will be fit up to a', /, 1x, 'magnitude value of 1.5.')
        write (lunit6, 7041)
7041    format (1x, 'However, more realistic results could probably be obtained by using only the real part of the transformation', /, &
             1x, 'matrix (option -2 in frequency loop card in line constants data).')
        ndata = ipoint-1
        !   == travelling time of highest freq. point ==
3140    if (icurve .eq. 2 .and. nfitmx .eq. 0) travhf = dist*beta/fdat
        !
        !             define curve zones
        !
        iknee = 1
        xknee(1) = xdat(1)
        ylevl0 = ydat(1)
        i = 1
120     i = i + 1
        if (i .gt. ndata) go to 230
        if (ydat(i).lt.(ylevl0-d1lim)) go to 130
        if (ydat(i).gt.(ylevl0+d1lim)) go to 180
        go to 120
        !  look for valley
130     ylevl0 = ydat(i)
        x1levl = xdat(i)
140     i = i + 1
        if (i.gt.ndata) go to 230
        if (ydat(i).lt.(ylevl0-d1lim)) go to 130
        if (ydat(i).gt.(ylevl0+d1lim)) go to 150
        go to 140
150     iscape = i
160     i = i - 1
        if (ydat(i).le.(ylevl0+d2lim)) go to 170
        go to 160
170     x2levl = xdat(i)
        iknee = iknee+1
        if (iknee.gt.mxknee) go to 2110
        xlevl = (x1levl+x2levl) * onehaf
        xknee(iknee) = xlevl
        !  look for peak
        i = iscape
180     ylevl0 = ydat(i)
        x1levl = xdat(i)
190     i = i + 1
        if (i.gt.ndata) go to 230
        if (ydat(i).gt.(ylevl0+d1lim)) go to 180
        if (ydat(i).lt.(ylevl0-d1lim)) go to 200
        go to 190
200     iscape = i
210     i = i - 1
        if (ydat(i).ge.(ylevl0-d2lim)) go to 220
        go to 210
220     x2levl = xdat(i)
        iknee = iknee+1
        if (iknee.gt.mxknee) go to 2110
        xlevl = (x1levl+x2levl) * onehaf
        xknee(iknee) = xlevl
        i = iscape
        go to 130
230     iknee = iknee+1
        if (iknee.gt.mxknee) go to 2110
        xknee(iknee) = xdat(ndata)
        if (idebug.eq.0) go to 250
        write (lunit6, 7044)
7044    format (/,1x,13x,'--- curve zones ---',1x)
        do 240 i=1,iknee
           write (lunit6, 7047) i, xknee(i)
7047       format (/,10x,i10,f10.3)
240     end do
250     nzone = iknee-1
        !   == parameters for least squares error checking ==
        dxcomp = .05d0
        nechk1 = ndata/(aptdec*dxcomp)+onehav
        xbegr = xdat(1)
        xendr = xknee(2)
        d1 = ydat(1)
        refb = yfun39(xendr)
        d3 = .001d0
        d4 = refb - d1
        d5 = 10.d0 ** d4
        ycut = d1 + alog1z( unity+d3*(d5-unity) )
        ycutpu = (ycut-d1)/(refb-d1)
        refa = yfun39(xbegr)
        call split(xbegr,xendr,ycutpu,xbegtl)
        xbegr = xdat(ndata)
        xendr = xknee(iknee-1)
        d1 = ydat(ndata)
        refb = yfun39(xendr)
        if (icurve.eq.2 .and. nfitmx .eq. 0) d3 = .25d0
        d4 = refb - d1
        d5 = 10.d0 ** d4
        ycut = d1 + alog1z( unity+d3*(d5-unity) )
        ycutpu = (ycut-d1)/(refb-d1)
        refa = yfun39(xbegr)
        call split(xbegr,xendr,ycutpu,xendtl)
        i1 = aptdec*(xbegtl-xdat(1))+1.d0
        i2 = aptdec*(xendtl-xdat(1))+2.d0
        nechk2 = (i2-i1+1)/(aptdec*dxcomp)+onehav
        !
        !             main loop for all-segments allocation
        !
        iterlp = 0
        incrtl = 0
        erropt = 1.d12
        ichkp = 0
        npole = 0
        if (normax.gt.10) go to 252
        tolfac = 3.0d0
        tolmin = .70d0
        go to 260
252     tolfac = 2.0d0
        tolmin = .20d0
260     iterlp = iterlp+1
        if (idebug .ge. 1) write (lunit6, 7049) iterlp
7049    format (//,2x,'###  a l l o c a t i o n  l o o p  no.',i3,4x, '###',1x)
265     ntotra = 0
        do izone=1,nzone
           xbegz = xknee(izone)
           xendz = xknee(izone+1)
           if (yfun39(xbegz).gt.yfun39(xendz)) go to 270
           !   positive slope zone
           signz = 1.
           go to 280
           !   negative slope zone
270        signz = -1.
           !   allocate slopes in zone
280        ioprau = 0
           ichkra = 1
           nchkra = 1
           xchkra(1) = xbegz
           xchkra(2) = xendz
           !
290        id = 2*ichkra
           xbegr = xchkra(id-1)
           xendr = xchkra(id)
           refa = yfun39(xbegr)
           refb = yfun39(xendr)
           xcla = 0.
           xclb = 0.
           xlim1 = xbegr
           xlim2 = xendr
           if (xbegr.eq.xbegz) call split (xbegr,xendr,clear,xcla)
           if (xendr.ne.xendz) go to 310
           refa = yfun39(xendr)
           refb = yfun39(xbegr)
           call split (xendr,xbegr,clear,xclb)
           d1 = refa
           refa = refb
           refb = d1
310        if (xcla.ne.0.) xlim1 = xcla
           if (xclb.ne.0.) xlim2 = xclb
           if (idebug.ge.3) write (lunit6, 7051) xbegr, xendr
7051       format (//,1x, 'region:',3x, 'xbeg=',e11.4,3x, 'xend=',e11.4,/)
!!!      write (*,*) ' after #130 xlim2, xlim1  ', xlim2 , xlim1
           abeta = ( refb - refa ) / (xlim2-xlim1)
           nalph1 = abeta
           alpha = nalph1+signz
           call locsl (xbegr,xendr,alpha,xmidr,xcorna,xcornb,erymax)
!!!      write (*,*) 'izone, ichkra, xmidr, xendr after call locsl  ',
!!!     1 izone, ichkra, xmidr, xendr
           if ( absz(xmidr-xendr) .gt. 0.001d0 )  go to 313
           if ( xendr .eq. xendz ) go to 440
           ichkra = ichkra + 1
           go to 290
313        tolera = 14.5938964d0+3.26036143d0*alogz(absz(abeta))
           if (tolera.lt.tolmin) tolera = tolmin
           tolera = tolfac*tolera
           d13 = 1.0 + tolera / 100.
           tolera = alog1z ( d13 )
           if (idebug.lt.3) go to 315
           d1 = 100.*(10.**erymax-1.)
           d2 = 100.*(10.**tolera-1.)
           write (lunit6, 7054) xmidr, xcorna, xcornb, abeta, alpha, d1, d2
7054       format (/,1x, 'xmidr=',e11.4,3x, 'xcorna=',e11.4,3x, 'xcornb=',e11.4,/,1x, &
                'abeta =',f7.2,4x, 'alpha =',f7.2,/, 1x, 'erymax=',f7.2, '%',3x, 'tolera=',f7.2, '%',1x)
315        if (erymax.lt.tolera) go to 330
           !   divide region into two subregions
           id = 2*(nchkra+1)
           nchkra = nchkra+2
           if (2*nchkra.le.mxchkr) go to 320
           if (iterlp.eq.1) go to 2120
           go to 790
320        xchkra(id-1) = xbegr
           xchkra(id)   = xmidr
           xchkra(id+1) = xmidr
           xchkra(id+2) = xendr
           ichkra = ichkra+1
           go to 290
           !   store parameters, do not subdivide
330        ioprau = ioprau+1
           if (4*ioprau.le.mzopra) go to 340
           if (iterlp.eq.1) go to 2130
           go to 790
340        id = 4*ioprau
           zoprau(id-3) = xbegr
           zoprau(id-2) = xcorna
           zoprau(id-1) = xcornb
           zoprau(id)   = alpha
           if(ichkra.eq.nchkra) go to 350
           ichkra = ichkra+1
           go to 290
           !
350        if (idebug.lt.3) go to 360
           write (lunit6, 7057)
7057       format (//,16x,'--- ranges checked ---',/)
           do i = 1,nchkra
              id = 2*i
              write (lunit6, 7059) xchkra(id - 1), xchkra(id)
7059          format (15x, 2e11.4)
353        end do
           write (lunit6, 7061)
7061       format (/,5x,'----- ranges in zone vector. unordered -----')
           write (lunit6, 7064)
7064       format (/,10x, 'xbegr',6x, 'xcorna',6x, 'xcornb',4x, 'alpha')
           do i = 1,ioprau
              id = 4*i
              write (lunit6, 7067) zoprau(id - 3), zoprau(id - 2), zoprau(id - 1), zoprau(id)
7067          format (5x, 3e12.4, f6.0)
356        end do
           !             sort ranges of identified regions
360        noprao(izone) = ioprau
           if (ioprau.eq.1) go to 420
           !   fill in auxiliary buffer
           do 370 i=1,ioprau
              id1 = 2*i-1
              id2 = 4*i-3
              xauxd(id1) = zoprau(id2)
              xauxd(id1+1) = i
370        end do
           !   sort auxiliary buffer
           k=ioprau
           iflag = k
380        if (iflag.le.0) go to 400
           k = iflag-1
           iflag = 0
           do j=1,k
              id = 2*j-1
              if (xauxd(id).le.xauxd(id+2)) go to 390
              xd1 = xauxd(id)
              xd2 = xauxd(id+1)
              xauxd(id) = xauxd(id+2)
              xauxd(id+1) = xauxd(id+3)
              xauxd(id+2) = xd1
              xauxd(id+3) = xd2
              iflag = j
390        end do
           go to 380
           !             fill-in ordered vector with all-ranges information
400        n1 = ntotra
           ntotra = ntotra+ioprau
           if (4*ntotra.le.mzopra) go to 405
           if (iterlp.eq.1) go to 2140
           go to 790
405        do i=1,ioprau
              id1 = 4*(n1+i)-3
              id = xauxd(2*i)+onehav
              id2 = 4*id-3
              zoprao(id1) = zoprau(id2)
              zoprao(id1+1) = zoprau(id2+1)
              zoprao(id1+2) = zoprau(id2+2)
              zoprao(id1+3) = zoprau(id2+3)
410        end do
           go to 440
420        n1 = ntotra
           ntotra = ntotra+ioprau
           if (4*ntotra.le.mzopra) go to 425
           if (iterlp.eq.1) go to 2140
           go to 790
425        id1 = 4*(n1+1)-3
           zoprao(id1)   = zoprau(1)
           zoprao(id1+1) = zoprau(2)
           zoprao(id1+2) = zoprau(3)
           zoprao(id1+3) = zoprau(4)
440     end do
        !
        if (idebug.lt.2) go to 450
        write (lunit6, 7069)
7069    format (/,6x,'-----  all-ranges vector. ordered  -----')
        write (lunit6, 7071)
7071    format (/, 10x, 'xbegr', 6x, 'xcorna', 6x, 'xcornb', 4x, 'alpha')
        do i = 1,ntotra
           id = 4*i
           write (lunit6, 7074) zoprao(id - 3), zoprao(id - 2), zoprao(id - 1), zoprao(id)
7074       format (1x, 4x, 3e12.4, f6.0)
445     end do
        !  check number of poles
450     npoles = 0
        do irange = 1,ntotra
           alpha = zoprao(4*irange)
           jn = absz(alpha)+onehav
           npoles = npoles+jn
460     end do
        !   == order increment or decrement check ==
        if (npoles.le.mpoles) go to 465
        if (iterlp.gt.1) go to 790
        go to 475
465     if (npoles.le.normax) go to 470
        if (iterlp.gt.1) go to 795
        go to 475
470     d1 = npole
        d2 = npoles
        if (npole.eq.0) go to 485
        if (d2/d1.lt.1.3d0) go to 480
        if (ichkp.eq.1) go to 485
        tolfac = 1.1d0*tolfac
        ichkp = 1
        go to 265
475     tolfac = 2.5d0*tolfac
        incrtl = incrtl+1
        if (incrtl.gt.10) go to 2160
        go to 265
480     if (npoles.gt.npole) go to 485
        if (ichkp.eq.1) go to 482
        tolfac = .8d0*tolfac
        go to 265
482     tolfac = tolfac/1.1d0
        go to 265
485     ichkp = 0
        !
        lrange = 0
        do irange = 1,ntotra
           alpha = zoprao(4*irange)
           if (alpha.gt.0.) go to 490
           jn = -alpha+onehav
           if (jn.eq.1) go to 540
           go to 500
490        jn = alpha+onehav
           if (jn.eq.1) go to 540
           !   assign zeroes for +alpha or poles for -alpha
500        plocpn = 0.
           j2 = jn-1
           do j = 1,j2
              plocpn = plocpn+j
510        end do
           alocpn = zoprao(4*irange-2)+deminp*plocpn/jn
           lrange = lrange+(2*jn+2)
           azepo(lrange-2*jn-1) = zoprao(4*irange-3)
           azepo(lrange-2*jn) = alpha
           azepo(lrange-jn) = alocpn
           do j = 1,j2
              azepo(lrange-jn-j) = alocpn-j*deminp
520        end do
           !   assign poles for +alpha or zeroes for -alpha
           alocpn = zoprao(4*irange-1)-deminp*plocpn/jn
           azepo(lrange-jn+1) = alocpn
           do j = 1,j2
              azepo(lrange-jn+1+j) = alocpn+j*deminp
530        end do
           go to 550
540        lrange = lrange+4
           azepo(lrange-3) = zoprao(4*irange-3)
           azepo(lrange-2) = alpha
           azepo(lrange-1) = zoprao(4*irange-2)
           azepo(lrange)   = zoprao(4*irange-1)
550     end do
        if (idebug.lt.2) go to 570
        !   output azepo vector
        write (lunit6, 7077)
7077    format (//, 1x, 4x, 'xbegr', 3x, 'alpha', 10x, 'poles and zeroes before adjustment (log f)', /)
        index = 0
        do irange = 1,ntotra
           alpha = azepo(index+2)
           jn = absz(alpha)+onehav
           i1 = index+1
           i2 = i1+1+2*jn
           write (lunit6, 7079) (azepo(i), i = i1, i2)
7079       format (1x,e12.4,f4.0,6e11.4,/,17x,6e11.4,/,17x,6e11.4)
           index = i2
560     end do
        !   store indexes in vector
570     indxv(1) = 0
        do i=2,ntotra
           index = indxv(i-1)
           alpha = azepo(index+2)
           jn = absz(alpha)+onehav
           indxv(i) = index+2+2*jn
580     end do
        !  == check for type of curve 2 ==
        id = indxv(ntotra)
        alpha = azepo(id+2)
        if (icurve.eq.2.and.alpha.gt.0. .and. nfitmx .eq. 0) iftype = 1
        !  convert corners to frequency values
        do irange=1,ntotra
           index = indxv(irange)
           id = index+2
           alpha = azepo(id)
           jn = absz(alpha)+onehav
           j2n = 2*jn
           d13 = 10.
           do j=1,j2n
590           azepo(id+j) = d13**azepo(id+j)
           end do
           if ( idebug .lt. 4 )  go to 600
           write (lunit6, *) ' poles and zeroes in Hz b4 do 610 loop.'
           write (lunit6,7078) (azepo(i), i = id + 1, id + j2n)   ! thl
7078       format ((1x, 10e11.4))
600     end do
        !
        !             corners adjustments
        !
        do j=1,3
           if (idebug .ge. 3) write (lunit6, 7081) j
7081       format (//, 10x, '*** corners adjustment loop no.', i2, 3x, '***')
           call adjpk
           call adjcr
           if ( idebug .lt. 4 )  go to 610   !thl
           write (lunit6, *) ' after call adjcr, j=,', j  ! thl
           write (lunit6, 7083)
           index = 0
           do irange=1,ntotra
              alpha = azepo(index+2)
              jn = absz(alpha)+onehav
              i1 = index+1
              i2 = i1+1+2*jn
              write (lunit6, 7079) (azepo(i), i = i1, i2)
              index = i2
375        end do                !  thl
610     end do
        if (idebug.lt.3) go to 630
        !             output poles & zeroes after adjustments
        write (lunit6, 7083)
7083    format (//, 1x, 4x, 'xbegr', 3x, 'alpha', 10x, 'poles and zeroes after adjustment (Hz)', /)
        index = 0
        do irange=1,ntotra
           alpha = azepo(index+2)
           jn = absz(alpha)+onehav
           i1 = index+1
           i2 = i1+1+2*jn
           write (lunit6, 7079) (azepo(i), i = i1, i2)
           index = i2
620     end do
        !  ==  store zeroes and poles on separate vectors ==
630     ncorn = 0
        do irange=1,ntotra
           id = indxv(irange)+2
           alpha = azepo(id)
           jn = absz(alpha)+onehav
           if (alpha.gt.0.) go to 660
           do j=1,jn
640           fcp(ncorn+j) = azepo(id+j)
           end do
           do j=1,jn
650           fcz(ncorn+j) = azepo(id+jn+j)
           end do
           go to 690
660        do j=1,jn
670           fcz(ncorn+j) = azepo(id+j)
           end do
           do j=1,jn
680           fcp(ncorn+j) = azepo(id+jn+j)
           end do
690        ncorn = ncorn+jn
700     end do
        npole = ncorn
        nzero = ncorn
        if (iftype.eq.2) nzero = nzero-jn
        !
        !              check least-squares deviation
        !
        error1 = 0.
        error2 = 0.
        x = xdat(1)-dxcomp
710     x = x+dxcomp
        if (x.gt.xdat(ndata)) go to 750
        freq = 10.d0**x
        freq2 = freq**2
        amagp = 1.d0
        if (nzero.eq.0) go to 725
        do j=1,nzero
720        amagp = amagp*(fcz(j)**2+freq2)/(fcp(j)**2+freq2)
        end do
725     amaglg = alog1z(amagp)
        if (iftype.eq.1) go to 740
        j1 = nzero+1
        do j=j1,npole
730        amaglg = amaglg-alog1z(fcp(j)**2+freq2)
        end do
740     amaglg = hreflg+amaglg/2.d0
        d1 = amaglg-yfun39(x)
        if (x.gt.1.60d0.and.x.lt.1.956d0) d1 = 3.d0*d1
        error1 = error1+d1**2
        if (x.lt.xbegtl.or.x.gt.xendtl) go to 710
        error2 = error2+d1**2
        go to 710
750     epser1 = error1/nechk1
        epser2 = error2/nechk2
        epserr = epser2
        if (idebug.eq.0) go to 756
        d1 = 100.d0*(10.d0**sqrtz(epser1)-1.d0)
        d2 = 100.d0*(10.d0**sqrtz(epser2)-1.d0)
        write (lunit6, 7085) d1, d2, npole, nzero
7085    format (//, 1x, '*** lsq ave error =', e12.4, 1x, '%', 7x, 'lsq check error =', e12.4, 1x, '%', 2x, '***', /, 1x, &
             '***', 6x, 'no. of poles =', i3, 20x, 'no. of zeroes =', i3, 9x, '***', 1x)
        if (iecode.eq.1) go to 754
        d1 = 100.d0*(10.d0**sqrtz(epstol)-1.d0)
        write (lunit6, 7087) d1
7087    format (/, 1x, '--- Error criterion: lsq check error less than', f5.2, 1x, '%  ---', 1x)
        go to 756
754     write (lunit6, 7089) normax
7089    format (/, 1x, '--- Error criterion: minimum lsq check error within given maximum order (', i3, 2x, 'poles ) ---')
756     if (epserr.ge.erropt) go to 780
        !  store optimum values
        do j=1,npole
760        fcpr(j) = fcp(j)
        end do
        do j=1,nzero
770        fczr(j) = fcz(j)
        end do
        hrflgr = hreflg
        npoler = npole
        nzeror = nzero
        erropt = epserr
780     if (epserr.lt.epstol.and.iecode.eq.0) go to 800
        if (epserr.ge.5.d0*erropt.and.inelim.eq.0) go to 797
        tolfac = .8d0*tolfac
        go to 260
790     if (idebug.ge.1) write (lunit6, 7091)
7091    format (//, 1x, '--- Further order increase was not possible because, of vectors dimensions ---', 1x)
        go to 800
795     if (idebug.ge.1) write (lunit6, 7093) npoles, normax
7093    format (//, 1x, '--- No. of poles in this loop (', i3, ') is larger than specified limit (', i3, ') ---', 1x)
        go to 800
797     if (idebug.ge.1) write (lunit6, 7095)
7095    format (//, 1x, '--- No further order increase was allowed because error', /, 1x, &
             'in this loop was 5 times larger than previous minimum.', /, 1x, &
             'If further order increase is desired make "inelim = 1" ', /, 1x, &
             'in parameters list ---', 1x)
800     if (erropt.gt.epstol.and.idebug.ge.1) write (lunit6, 7097)
7097    format (//, 1x, '--- Maximum tolerance criterion could not be met ---')
        !
        !             time delay for propagation function
        !
        if (icurve .eq. 1 .or. nfitmx .eq. 1)  go to 805
        dx = .1d0
        x = xdat(1)-dx
900     x = x+dx
        if (x.gt.xdat(ndata)) go to 910
        if (aph(x).lt.-.08727d0) go to 920
        go to 900
920     x1 = x
        x2 = xdat(ndata)
        sumtau = 0.
        n = 0
        x = x1-dx
930     x = x+dx
        if (x.gt.x2) go to 940
        n = n+1
        freq = 10.d0**x
        php = 0.
        do j=1,nzeror
932        php = php+atan2z(freq,fczr(j))-atan2z(freq,fcpr(j))
        end do
        if (iftype.eq.1) go to 935
        j1 = nzeror+1
        do j=j1,npoler
934        php = php-atan2z(freq,fcpr(j))
        end do
935     tau = (php-aph(x))/freq
        sumtau = sumtau+tau
        go to 930
940     taur = sumtau/(n*twopi)
        go to 950
910     write (lunit6, 7099)
7099    format (//, 1x, '*** Specified frequency range does not contain enough curve ', &
             /, ' dynamics for an accurate phase displacement evaluation. ', &
             /, ' tau will be taken as the travelling time at the highest ', &
             /, ' given frequency ***')
        taur = travhf
950     if (idebug.eq.0) go to 807
        d1 = 1.d3*taur
        d2 = 10.d0**xdat(ndata)
        d3 = 1.d3*travhf
        write (lunit6, 8001) d1, d2, d3
8001    format (//, 1x, 'Phase displacement tau = ', e12.4, 3x, 'msec', /, 1x, 'travelling time at', e12.4, 2x, 'Hz', 3x, 'is', &
             e12.4, 3x, 'msec')
        !   == add extra pole to a1 function with type 1 fit ==
807     if (iftype.eq.2) go to 805
        npoler = npoler+1
        if (npoler.gt.mpoles) go to 2150
        d1 = xdat(ndata)+5.d0
        fcpr(npoler) = 10.d0**d1
805     if (ifwta.eq.0) go to 850
        !
        !             comparison table
        !
        d1 = 10.d0**hrflgr
        write (lunit6, 8003) iftype, d1
8003    format (/,1x,'Curve type =',i2,4x,'reference level =',e11.4)
        write (lunit6, 8005) npoler, nzeror
8005    format (/,1x,'No. poles =',i3,4x,'no. zeroes =',i3)
        if (icurve .eq. 2  .or. nfitmx .eq. 1) go to 812
        write (lunit6, 8007) imode
8007    format (//,' Table of yc vs. yceq for mode',i3,/,' units: freq. in Hz, magnitude in Ohms, phase in Deg., delmag in %, delph in Deg.', &
             //,5x,'freq',6x,'ycmag',5x,'yceqmag',5x,'delmag',6x,'ycph', 6x,'yceqph',5x,'delph',7x,'freq')
        go to 814
812     if ( nfitmx .eq. 1 )  go to 813
        write (lunit6, 8009) imode
8009    format (/,1x,'Table of  "a1"  vs.  "a1eq"  for mode',i3, /,1x,'units: frequency in Hz,  magnitude in per unit,  phase in Deg.', &
             'delmag in %,  delph in Deg.', / , 1x, 'p1ph = a1eqph+omega*tau; tau in msec; deltau in % .', &
             //,5x,'freq',6x,'a1mag',5x,'a1eqmag',5x,'delmag',6x,'a1ph', 6x,'a1eqph',5x,'delph',7x,'p1ph',7x,'tau',7x,'deltau', 6x,'freq')
        go to 814
813     write (lunit6, 1813) icurve, imode
1813    format (//,' table of ti vs. tieq for element ', 2i2, /, ' units: freq. in hz, magnitude unitless, phase in deg., delmag in %, delph in deg.', &
             //,5x,'freq',6x,'timag',5x,'tieqmag',5x, 'delmag',6x,'tiph', 6x,'tieqph',5x,'delph',7x,'freq')
814     dxcomp = .1d0
        x = xdat(1)-dxcomp
810     x = x+dxcomp
        if (x.gt.xdat(ndata)) go to 850
        freq = 10.d0**x
        freq2 = freq**2
        amagp = 1.d0
        php = 0.
        if (nzeror.eq.0) go to 825
        do  j=1,nzeror
           amagp = amagp*(fczr(j)**2+freq2)/(fcpr(j)**2+freq2)
           php = php+atan2z(freq,fczr(j))-atan2z(freq,fcpr(j))
820     end do
825     amaglg = alog1z(amagp)
        if (iftype.eq.1) go to 840
        j1 = nzeror+1
        do j=j1,npoler
           amaglg = amaglg-alog1z(fcpr(j)**2+freq2)
           php = php-atan2z(freq,fcpr(j))
830     end do
840     amaglg = hrflgr+amaglg/2.d0
        amagp = 10.d0**amaglg
        ax = 10.d0**yfun39(x)
        d1 = (amagp/ax-1.d0)*100.d0
        d2 = aph(x)
        d3 = d2*360.d0/twopi
        d4 = php*360.d0/twopi
        d5 = absz(d4-d3)
        if (icurve.eq.2 .and. nfitmx .eq. 0) go to 842
        write (lunit6, 8011) freq, ax, amagp, d1, d3, d4, d5, freq
8011    format (1x, 8e11.4)
        go to 810
842     tau = (php-d2)/(twopi*freq)
        d8 = tau*1.d3
        d9 = (tau/taur-1.d0)*100.d0
        d6 = d4-360.d0*freq*taur
        d7 = absz(d6-d3)
        write (lunit6, 8013) freq, ax, amagp, d1, d3, d6, d7, d4, d8, d9, freq
8013    format (1x, 11e11.4)
        go to 810
        !
850     if (ifplot.eq.1)  call ftplot (icurve, imode, nfitmx)
        !
        !             partial fraction expansion
        !
        do ifrac=1,npoler
           s = -fcpr(ifrac)
           resilg = hrflgr
           sign = 1.d0
           if (nzeror.eq.0) go to 875
           do j=1,nzeror
              if (j.eq.ifrac) go to 865
              d1 = s+fczr(j)
              d2 = s+fcpr(j)
              d3 = absz(d1)
              d4 = absz(d2)
              sign = sign*(d1/d3)*(d2/d4)
              resilg = resilg+alog1z(d3)-alog1z(d4)
              go to 860
865           d1 = s+fczr(j)
              d3 = absz(d1)
              sign = sign*(d1/d3)
              resilg = resilg+alog1z(d3)
860        end do
           if (iftype.eq.1) go to 870
875        j1 = nzeror+1
           do j=j1,npoler
              if (j.eq.ifrac) go to 880
              d2 = s+fcpr(j)
              d4 = absz(d2)
              sign = sign*(d2/d4)
              resilg = resilg-alog1z(d4)
880        end do
870        resid = (10.d0**resilg)*sign
890        akfrac(ifrac) = twopi*resid
           if ( minust(icurve) .eq. 1 ) akfrac(ifrac) = - akfrac(ifrac)
           alphaf(ifrac) = twopi*fcpr(ifrac)
855     end do
        !
        !     if (icurve.eq.1) hrefr = 10.d0**hrflgr
        if (icurve .eq. 1 .or. nfitmx .eq. 1) hrefr = 10.d0**hrflgr
        !   == punch parameters ==
        n23 = npoler
        if ( iofgnd .eq. 1 ) n23 = ( npoler + 2 ) / 3 * 3
        if (icurve .eq. 2 .and. nfitmx .eq. 0) go to 8779
        if (nfitmx .eq. 1) go to 8794
        d6 = koutpr
        n8 = 2 * imode
        if ( imodal  .gt.  0 .or. mspedb .eq. 1 )   go to  8787
        if ( ipunch  .eq.  0 ) write (lunit7, 8781) imode, vstacs(n8 - 1), vstacs(n8), d6
8781    format ( '-',  i1,  2a6,  12x,  f6.0,4x, '1.',14x, '-4', 3x, '1' )
        if (idebug.eq.0) go to 8794
        write (lunit1, 8782) imode, vstacs(n8 - 1), vstacs(n8), d6
8782    format ( 1x,  '-',  i1,  2a6,  12x,  f6.0, 4x, '1.',14x, '-4',3x,'1','                         ')
        kount = kount+1
        go to 8794
8787    if ( itrnsf .eq. 1 )  ifqdpt = 0
        if ( ipunch  .eq.  0 ) write (lunit7, 8790)  imode, vstacs(n8 - 1), vstacs(n8), d6, nmode, ifqdpt
8790    format ( '-', i1, 2a6, 12x, f6.0,4x, '1.',14x, '-4', 2i2)
        if (idebug.eq.0) go to 8794
        write (lunit1, 8791)  imode, vstacs(n8 - 1), vstacs(n8), d6, nmode, ifqdpt
8791    format (1x,'-',i1,2a6, 12x, f6.0,4x,'1.',14x, '-4',2i2, '                        ')
        kount = kount+1
        go to 8794
8793    write (lunit6, *) ' This is a near-zero tij element.'
        npoler = 1
        hrefr  = 0.
        akfrac(1) = 0.0
        alphaf(1) = 1.0
        n23 = 1
        if ( iofgnd .eq. 1 )  n23 = 3
        go to 8794
8792    npoler = 1
        hrefr = 1.
        akfrac(1) = 0.0
        alphaf(1) = 1.0
        numone = 0
        n23 = 1
        if ( iofgnd .eq. 1 )  n23 = 3
8794    if ( minust(icurve) .eq. 1 )  hrefr = - hrefr
        if ( ipunch  .eq.  0 ) write (lunit7, 8798) npoler, hrefr
8798    format ( i8, e32.20 )
        if (idebug.eq.0) go to 8788
        write (lunit1, 8799) npoler, hrefr
8799    format ( 1x,  i8,  e32.20, '                                              ')
        kount = kount+1
        go to 8788
8779    if ( ipunch  .eq.  0 ) write (lunit7, 8798) npoler, taur
        if (idebug.eq.0) go to 8788
        write (lunit1, 8799) npoler, taur
        kount = kount+1
8788    if ( ipunch  .eq.  0 ) write (lunit7, 8803) (akfrac(kp), kp = 1, npoler)
8803    format ( 3e26.18 )
        if (idebug.eq.0) go to 8806
        write (lunit1, 8804)  (akfrac(kp), kp = 1, n23)
8804    format ( 1x,  3e26.18,  '    ')
8806    if ( ipunch  .eq.  0 ) write (lunit7, 8803)  (alphaf(kp), kp = 1, npoler)
        if (idebug.eq.0) go to 8810
        write (lunit1, 8804)  (alphaf(kp), kp = 1, n23)
        d1 = npoler/3.d0
        n2 = npoler/3
        kount = kount+2*n2
        if (d1.gt.n2) kount = kount+2
8810    if ( imode  .lt. 2 )  go to 3200
        if ( iprsup .ge. 1) write (lunit6, *) ' imode, imodal and mspedb at 8810 = ', imode, imodal, mspedb
        if ( imodal .gt. 0 )  go to 3200
        if ( mspedb .eq. 1  .and.  imode .eq. 2 )  go to 3200
        if ( iprsup .ge. 1 ) write (lunit6, *) ' icurve =', icurve
        if ( icurve .eq. 2 .and. nfitmx .eq. 0 )  go to 8815
        npols1 = npoler
        if ( iprsup .ge. 1 ) write (lunit6, *) ' Storing  zc info.'
        do kp = 1, npoler
           akfrs1(kp) = akfrac(kp)
           alphs1(kp) = alphaf(kp)
8812    end do
        go to 3200
8815    npols2 = npoler
        if ( iprsup .ge. 1 ) write (lunit6, *) ' Storing a1 info.'
        do kp = 1, npoler
           akfrs2(kp) = akfrac(kp)
           alphs2(kp) = alphaf(kp)
8816    end do
        go to 6000
        !
3200 end do
     !
3300 end do
  if ( itrnsf .ne. 1 )  go to 8820
  if ( nfitmx .ne. 0 )  go to 8841
  nfitmx = 1
  go to 8777
6000 do imode = 3, nmode
     do icurve = 1, 2
        if ( iprsup .ge. 1) write (lunit6, *) ' Restoring the stored info at 6300.'
        if ( mspedb .eq. 1  .and.  imode .eq. 3 )  go to 6300
        if ( icurve .eq. 1 )  npoler = npols1
        if ( icurve .eq. 2 )  npoler = npols2
        n23 = npoler
        if ( iofgnd .eq. 1 ) n23 = ( npoler + 2 ) / 3 * 3
        if (icurve.eq.2) go to 6779
        d6 = koutpr
        n8 = 2 * imode
        if ( mspedb  .gt.  0 )   go to  6787
        if ( ipunch  .eq.  0 ) write (lunit7, 8781) imode, vstacs(n8 - 1), vstacs(n8), d6
        if (idebug.eq.0) go to 6794
        write (lunit1, 8782) imode, vstacs(n8 - 1), vstacs(n8), d6
        kount = kount+1
        go to 6794
6787    if ( ipunch  .eq.  0 ) write (lunit7, 8790)  imode, vstacs(n8 - 1), vstacs(n8), d6, nmode, ifqdpt
        if (idebug.eq.0) go to 6794
        write (lunit1, 8791)  imode, vstacs(n8 - 1), vstacs(n8), d6, nmode, ifqdpt
        kount = kount+1
6794    if ( ipunch  .eq.  0 ) write (lunit7, 8798) npoler, hrefr
        if (idebug.eq.0) go to 6788
        write (lunit1, 8799) npoler, hrefr
        kount = kount+1
        go to 6788
6779    if ( ipunch  .eq.  0 ) write (lunit7, 8798) npoler, taur
        if (idebug.eq.0) go to 6788
        write (lunit1, 8799) npoler, taur
        kount = kount+1
6788    if ( icurve .eq. 2 )  go to 6800
        if ( ipunch  .eq.  0 ) write (lunit7, 8803)  (akfrs1(kp), kp = 1, npoler)
        if (idebug.eq.0) go to 6806
        write (lunit1, 8804)  (akfrs1(kp), kp = 1, n23)
6806    if ( ipunch  .eq.  0 ) write (lunit7, 8803)  (alphs1(kp), kp = 1, npoler)
        if (idebug.eq.0) go to 6280
        write (lunit1, 8804)  (alphs1(kp), kp = 1, n23)
        go to 6810
6800    if ( ipunch  .eq.  0 ) write (lunit7, 8803)  (akfrs2(kp), kp = 1, npoler)
        if (idebug.eq.0) go to 6807
        write (lunit1, 8804)  (akfrs2(kp), kp = 1, n23)
6807    if ( ipunch  .eq.  0 ) write (lunit7, 8803)  (alphs2(kp), kp = 1, npoler)
        if (idebug.eq.0) go to 6280
        write (lunit1, 8804) (alphs2(kp), kp = 1, n23)
6810    d1 = npoler/3.d0
        n2 = npoler/3
        kount = kount+2*n2
        if (d1.gt.n2) kount = kount+2
6280 end do
6300 end do
  !
  !   == punch transformation matrix ==
8820 if ( imodal  .eq.  0   .and.  mspedb .eq. 0 )   go to 8841
  if ( imodal .gt. 0 )  go to 8822
8015 d1 = 2.0
  d2 = 6.0
  d3 = 1.0/sqrtz(d1)
  d4 = 1.0/sqrtz(d2)
  n1 = 1
  do i = 1, 6
     do j = 1, 6
        if ( i .gt. 1 )  go to 8021
        tir(j,i) = d4
        go to 8023
8021    if ( i .gt. 2 )  go to 8025
        if ( j .gt. 3 )  go to 8027
        tir(j,i) =  d4
        go to 8023
8027    tir(j,i) = -d4
        go to 8023
8025    if ( i .gt. 3 )  go to 8029
        if ( j .gt. 2 )  go to 8031
        if ( j .eq. 1 )  tir(j,i) =  d3
        if ( j .eq. 2 )  tir(j,i) = -d3
        go to 8023
8031    tir(j,i) = 0.
        go to 8023
8029    if ( i .gt. 4 )  go to 8033
        if ( j .gt. 3 )  go to 8035
        if ( j .gt. 2 )  go to 8037
        tir(j,i) = d4
        go to 8023
8037    tir(j,i) = - d1*d4
        go to 8023
8035    tir(j,i) = 0.
        go to 8023
8033    if ( i .gt. 5 )  go to 8039
        if ( j .ge. 4 .and. j .le. 5) go to 8041
        go to 8031
8041    if ( j .eq. 4 )  tir(j,i) =   d3
        if ( j .eq. 5 )  tir(j,i) = - d3
        go to 8023
8039    if ( j .gt. 3)  go to 8043
        go to 8031
8043    if ( j .gt. 5 )  go to 8045
        tir(j,i) = d4
        go to 8023
8045    tir(j,i) = - d1 * d4
8023    tii(j,i) = 0.0
8019 end do
8017 end do
8047 if (iprsup .ge. 1 ) write (lunit6, 8049) ((tir(j, i), i = 1, 6), j = 1, 6), ((tii(j, i), i = 1, 6), j = 1, 6)
8049 format (' tir and tii for this special transposed double circuit are', /, (1x, 8e15.7))
8822 npoler = 1
  akfrac(1) = 0.0
  alphaf(1) = 1.0
  n23 = 1
  if ( iofgnd .eq. 1 ) n23 = ( npoler + 2 ) / 3 * 3
  do imode=1, nmode
     do j = 1, nmode
        hrefr = tir (j, imode )
        if ( ipunch  .eq.  0 ) write (lunit7, 8798)  npoler,hrefr
        if (idebug.eq.0) go to 8836
        write (lunit1, 8799)  npoler,hrefr
        kount = kount+1
8836    if ( ipunch  .eq.  0 ) write (lunit7, 8803)   akfrac(1)
        if (idebug.eq.0) go to 8838
        write (lunit1, 8804)   akfrac(1)
8838    if ( ipunch  .eq.  0 ) write (lunit7, 8803)   alphaf(1)
        if (idebug.eq.0) go to 8834
        write (lunit1, 8804)   alphaf(1)
        kount = kount+2
8834 end do
8832 end do
  !
8841 if ( idebug  .ge.  1 ) write (lunit6, 8768)
8768 format ( /, ' Punched output (on lunit7) begins with comment cards documenting the', &
          ' transmission circuit geometry.    ',/, 'The following is a record of the', &
          ' punching,  including the final two columns which do not fit on card.  ',/, &
          1x,  82( '=' )  ,/, ' 001234567890123456789012345678901234567890', &
          '1234567890123456789012345678901234567890', /, 1x,  82( '=' )    )
  !   == recover information from lunit1, print on lunit6 ==
  rewind lunit1
  if ( kount .eq. 0 )  go to 1550
  do j=1,kount
     read(lunit1,1542) (textp(i), i=1,14)
1542 format (13a6,a2)
     write (lunit6,1542) (textp(i), i=1,14)
8835 end do
  go to 1550
9200 lstat(18)=nchain
  lastov=nchain
  nchain=51
9999 if ( iprsup  .ge.  6 ) write (lunit6, 9998)  nchain, kill
9998 format (  ' Exit "subr39".   nchain, kill =',  2i6  )
  return
  !
  !             terminating conditions
  !
2170 write (lunit6,8050) nmode,mmodes
8050 format (//, 1x, '%%% Number of modes (', i2, ') exceeds limit (', i2, '). Execution terminated %%%')
  call stoptp
2100 write (lunit6,8051) mdapts
8051 format (//, 1x, '%%% Dimension of data vectors (', i3, ') exceeded. Execution terminated %%%')
  call stoptp
2110 write (lunit6,8053) mxknee
8053 format (//, 1x, '%%% Dimension of xknee (', i2, ') exceeded. Execution terminated %%%')
  call stoptp
2120 write (lunit6,8055) mxchkr
8055 format (//, 1x, '%%% Dimension of xchkra (', i3, ') exceeded. Execution terminated %%%')
  call stoptp
2130 write (lunit6,8057) mzopra
8057 format (//, 1x, '%%% Dimension of zoprau (', i3, ') exceeded. Execution terminated %%%')
  call stoptp
2140 write (lunit6,8059) mzopra
8059 format (//, 1x, '%%% Dimension of zoprao (', i3, ') exceeded. Execution terminated %%%')
  call stoptp
2160 write (lunit6,8061) normax
8061 format (//, 1x, '%%% The algorithm cannot fit this curve, within the maximum order specified (', i3, ').', &
          /, 1x, '=== increase this limit ===.', /, 1x, 'No approximation was generated %%%')
  call stoptp
2150 write (lunit6,8063) mpoles
8063 format (//, 1x, '%%% Dimension of corner vectors (', i2, ') exceeded. Execution terminated %%%')
  call stoptp
  return
end subroutine subr39

!
! subroutine locsl.
!

subroutine locsl (xbeg, xend, alpha, xmid, xcorna, xcornb, erymax)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'labl39.ftn'
  include 'deck39.ftn'
  d78 = xend - xbeg
  if ( absz(d78) .ge.  0.01 )  go to 50
  if ( d78 .gt. 0.0 )  xmid = xbeg + d78/2  ! really small range
  if ( d78 .lt. 0.0 )  xmid = xend - d78/2  ! consideration,  thl
  go to 80                                     !  9/14/89
50 call split (xbeg,xend, onehav, xmid)
80 d1 = absz( xend - xmid)
  if ( d1 .le. 0.001d0)  return
  if ( idebug .lt. 4 )  go to 2100
  refd2 = absz ( (refa-refb) * onehav )
  refdpc = 100.d0*(10.d0**refd2-1.d0)
  write (lout,8065) refa,refb,refdpc
8065 format (1x, 'refa=',e11.4,3x, 'refb=',e11.4,3x, 'refdpc=', e11.4, '%')
  !   place segment initially at mid point
2100 d2 = (refb-refa)/2./alpha
  xcorna = xmid - d2
  xcornb = xmid + d2
  jn = absz(alpha)
  d1 = xend-xbeg
  xdelta = d1/(20*jn)
  dxedma = d1/(50*jn)
  !
  do iter = 1, 2
     xcorar = xcorna
     xcorbr = xcornb
     xcorna = xcorna-xdelta
     xcornb = xcornb-xdelta
     erropt = 1.d12
     idefac = -1
     lsign = 1
     !    in 248 loop, n=1 is for r-shift, n=2 is for l-shift
     do n = 1, 2
        if ( n .eq. 1 )  go to 98
        idefac = 0
        lsign = -1
98      do j = 1, 5
           idefac = idefac+lsign
           xcorna = xcorna+xdelta*lsign
           xcornb = xcornb+xdelta*lsign
           !   check allocation error
           error = 0.
           x = xbeg-xdelta
100        x = x+xdelta
           if (x.gt.xend) go to 140
           if (x.le.xcorna) go to 110
           if (x.gt.xcornb) go to 120
           yapprx = refa+(x-xcorna)*alpha
           go to 130
110        yapprx = refa
           go to 130
120        yapprx = refb
130        error = error+(yapprx-yfun39(x))**2
           go to 100
140        if (idebug.lt.4)  go to 8069
           if ( n .eq. 1 ) write (lout,8067) idefac,xcorna,xcornb,error,erropt
8067       format (1x, 'shift r',i5,4e12.4)
           if ( n .eq. 2 ) write (lout,8068) idefac, xcorna, xcornb, error, erropt
8068       format (1x, 'shift l', i5, 4e12.4)
8069       if(error.ge.erropt) go to 150    ! avoid infinite loop, change
           erropt = error                   ! .gt. to .ge.,  thl, 9/12/89
           idefop = idefac
150        if (error.ge.2./iter*erropt) go to 170   !  thl
160     end do
170     if ( n .eq. 2 )  go to 250
        !   restore original position before shifting to the left
        xcorna = xcorar
        xcornb = xcorbr
248  end do
     !   shift according to optimum value
250  xcorna = xcorar+idefop*xdelta
     xcornb = xcorbr+idefop*xdelta
     xdelta = xdelta/4.
  end do
260 continue
  !  obtain maximum deviation
  erymax = 0.
  xl1 = xknee(izone)
  xl2 = xknee(izone+1)
  x = xcorna-dxedma
270 x = x+dxedma
  if (x.gt.xcornb) go to 310
  x1 = x-dxedma
  x2 = x+dxedma
  if (x1.lt.xl1) x1 = x
  if (x2.gt.xl2) x2 = x
  if (x1.ne.x2) go to 280
  agamma = 0.
  go to 290
280 agamma = (yfun39(x2)-yfun39(x1))/(x2-x1)
290 yapprx = refa+(x-xcorna)*alpha
  d13 = 1.0 + agamma**2
  errory = absz ( (yapprx-yfun39(x)) / sqrtz ( d13 )  )
  if (errory.lt.erymax) go to 300
  erymax = errory
300 go to 270
310 return
end subroutine locsl

!
! subroutine adjpk.
!

subroutine adjpk
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  dimension fpz1(50),fpz2(50)
  include 'labl39.ftn'
  include 'deck39.ftn'
  !  change this limit if dimension is changed
  !   order of slope in segment (consecutive poles or zeroes)
  mdimpz = 50
  if (ntotra.eq.1) go to 340
  ntotr1 = ntotra-1
  do irange = 1,ntotr1
     index1 = indxv(irange)
     index2 = indxv(irange+1)
     alph1 = azepo(index1+2)
     alph2 = azepo(index2+2)
     if (alph1*alph2.gt.0.) go to 330
     xl1 = azepo(index1+1)
     if (irange.lt.ntotr1) go to 100
     xr1 = xdat(ndata)
     go to 110
100  id = indxv(irange+2)
     xr1 = azepo(id+1)
110  xpeak = azepo(index2+1)
     refa = yfun39(xpeak)
     refb = yfun39(xl1)
     call split (xpeak,xl1, oneqtr, xbeg)
     refb = yfun39(xr1)
     call split (xpeak,xr1, oneqtr, xend)
     jn1 = absz(alph1)+onehav
     jn2 = absz(alph2)+onehav
     if (jn1.gt.mdimpz.or.jn2.gt.mdimpz) go to 2100
     id1 = index1+2+jn1
     id2 = index2+2
     flim1 = azepo(id1)
     flim2 = azepo(id2+jn2+1)
     if (idebug.ge.3) write (lout,8071) xpeak
8071 format (/,1x,10x,'adjustment of peak:',e12.4)
     djn1 = 1.d0 / jn1
     djn2 = 1.d0 / jn2
     erropt = 1.e12   !  thl
     do isub = 1,3
        dsub2 = 1.d0 / ( isub**2 )
        dxepk = .3d0 * dsub2
        tolerr = 1.01d0**dsub2
        dfx = 10.d0**( .02d0*dsub2/isub )
        dfpk1 = dfx**djn1
        dfpk2 = dfx**djn2
        dfpk11 = 1.d0 / dfpk1
        dfpk22 = 1.d0 / dfpk2
        !   store original values in temporary vectors
        do j = 1,jn1
           fpz1(j) = azepo(id1+j)
           azepo(id1+j) = azepo(id1+j) * dfpk11
120     end do
        do j = 1,jn2
           fpz2(j) = azepo(id2+j)
           azepo(id2+j) = azepo(id2+j)*dfpk2
130     end do
        !   shift inwards
        !      erropt = 1.e12    !  thl
        idefac = -1
        do 190 jsh = 1,50
           idefac = idefac+1
           fcorn1 = azepo(id1+jn1)*dfpk1
           fcorn2 = azepo(id2+1) * dfpk22
           if (fcorn1.gt.fcorn2) go to 200
           do j = 1,jn1
140           azepo(id1+j) = azepo(id1+j)*dfpk1
           end do
           do j = 1,jn2
150           azepo(id2+j) = azepo(id2+j) * dfpk22
           end do
           call refh
           !   check error
           error = 0.
           x = xbeg-dxepk
160        x = x+dxepk
           if (x.gt.xend) go to 170
           f = 10.d0**x
           call ratp (f,amaglg)
           yapprx = amaglg
           error = error+(yapprx-yfun39(x))**2
           go to 160
170        if (idebug.ge.4) write (lout,8073) idefac,fcorn1,fcorn2,error,erropt
8073       format (1x, 'shift in ',i5,4e12.4)
           if (error.ge.erropt) go to 180    ! thl
           erropt = error
           idefop = idefac
180        if (error.ge.tolerr*erropt) go to 200    ! thl
190     end do
        !   restore original values before shifting in the opposite direction
200     do j = 1,jn1
210        azepo(id1+j) = fpz1(j)
        end do
        do j = 1,jn2
220        azepo(id2+j) = fpz2(j)
        end do
        !   shift outwards
        idefac = 0
        do jsh = 1,50
           idefac = idefac-1
           fcorn1 = azepo(id1+1) * dfpk11
           fcorn2 = azepo(id2+jn2)*dfpk2
           if (fcorn1.lt.flim1) go to 290
           if (fcorn2.gt.flim2) go to 290
           do j = 1,jn1
230           azepo(id1+j) = azepo(id1+j) * dfpk11
           end do
           do j = 1,jn2
240           azepo(id2+j) = azepo(id2+j)*dfpk2
           end do
           call refh
           !   check error
           error = 0.
           x = xbeg-dxepk
250        x = x+dxepk
           if (x.gt.xend) go to 260
           f = 10.d0**x
           call ratp (f,amaglg)
           yapprx = amaglg
           error = error+(yapprx-yfun39(x))**2
           go to 250
260        if (idebug.ge.4) write (lout,8075) idefac,fcorn1,fcorn2,error,erropt
8075       format (1x, 'shift out',i5,4e12.4)
           if (error.ge.erropt) go to 270   ! thl
           erropt = error
           idefop = idefac
270        if (error.ge.tolerr*erropt) go to 290     ! thl
280     end do
        !   shift according to optimum value
290     df11 = dfpk1**idefop
        do j = 1,jn1
300        azepo(id1+j) = fpz1(j) * df11
        end do
        df22 = dfpk22**idefop
        do j = 1,jn2
310        azepo(id2+j) = fpz2(j) * df22
        end do
320  end do
330 end do
340 return
  !               terminating conditions
2100 write (lout,8077) mdimpz
8077 format (//,1x,'%%% dimension of fpz1 or fpz2 (',i2, ') exceeded. Execution terminated %%%'   )
  call stoptp
  return
end subroutine adjpk

!
! subroutine adjcr.
!

subroutine adjcr
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'labl39.ftn'
  include 'deck39.ftn'
  call refh
  xlast = xdat(ndata)
  xfirst = xdat(1)
  nzone1 = nzone-1
  do iter = 1,3
     if (idebug.ge.3) write (lout,8079) iter
8079 format (//,5x,26h*** shifting iteration no., i2,3x, 3h***)
     irangt = 0
     !             shifting of zones
     do izone = 1,nzone
        nrange = noprao(izone)
        nrang1 = nrange-1
        if (idebug.ge.3) write (lout,8081) izone,nrange
8081    format (/,1x,30h*** corners adjustment of zone, i3, 1h;, 2x, 9hthere are, i3,2x,11h ranges ***)
        do irange = 1,nrange
           irangt = irangt+1
           call inran (irangt,indx0,indxr1,indxr2,indxr3,indxl1,indxl2)
           if ( nzone .eq. 1 )  go to 180
           if (nrange.eq.1) go to 280
           if (nrange.eq.2) go to 290
           if (nrange.eq.3) go to 300
           if (irange.eq.1) go to 240
           if (irange.eq.2) go to 250
           if (irange.eq.nrang1) go to 260
           if (irange.eq.nrange) go to 270
           !   intermediate ranges
100        xbeg = azepo(indxl2+1)
           xbegf = azepo(indxl1+1)
           fbeg = 10.**xbegf
           xend = azepo(indxr3+1)
           xendf = azepo(indxr2+1)
           fend = 10.**xendf
           go to 320
           !             special case: one zone only
180        if (nrange.le.2) go to 190
           if (nrange.eq.3) go to 200
           if (irange.eq.1 ) go to 240
           if ( irange .eq. 2 )  go to 250
           if (irange.ge.nrang1) go to 210
           go to 100
190        xbeg = xfirst
           fbeg = 10.**(xbeg-1.)
           xend = xlast
           fend = 10.**(xend+1.)
           go to 320
200        if (irange.eq.1) go to 240
           if (irange.eq.2) go to 190
210        xbeg = azepo(indxl2+1)
           xbegf = azepo(indxl1+1)
           fbeg = 10.**xbegf
           xend = xlast
           fend = 10.**(xend+1.)
           go to 320
           !   first range
240        xbeg = azepo(indxl1+1)
           xbegf = azepo(indx0+1)
           fbeg = 10.**xbegf
           if ( izone .eq. 1 )  fbeg = fbeg / 10.
           if ( izone .eq. nzone  .and. nrange .eq. 2 )  go to 245
           xend = azepo(indxr3+1)
           fend = azepo(indxr1+3)
           if ( izone .ne. 1 )  go to 320
           xendf = azepo(indxr2+1)
           fend = 10. ** xendf
           go to 320
245        xend = xlast
           fend = azepo(indxr1+3)
           go to 320
           !   second range
250        xbeg = azepo(indxl2+1)
           fbeg = azepo(indx0)
           if ( izone .eq. 1 )  fbeg = fbeg /10.
           xend = azepo(indxr3+1)
           xendf = azepo(indxr2+1)
           fend = 10.**xendf
           go to 320
           !   one-to-last range
260        xbeg = azepo(indxl2+1)
           xbegf = azepo(indxl1+1)
           fbeg = 10.**xbegf
           if ( izone .eq. nzone )  go to 285
           xend = azepo(indxr3+1)
           fend = azepo(indxr1+3)
           go to 320
285        xend = xlast
           fend = 10. ** (xend+1.)
           go to 320
           !   last range
270        xbeg = azepo(indxl2+1)
           fbeg = azepo(indx0)
           if ( izone .eq. nzone )  go to 285
           xend = azepo(indxr2+1)
           xendf = azepo(indxr1+1)
           fend = 10.**xendf
           go to 320
           !   special cases
           !   one-range zone
280        xbeg = azepo(indxl1+1)
           xbegf = azepo(indx0+1)
           fbeg = 10.**xbegf
           if ( izone .eq. 1 )  fbeg = fbeg / 10.
           if ( izone .eq. nzone )  go to 285
           xend = azepo(indxr2+1)
           xendf = azepo(indxr1+1)
           fend = 10.**xendf
           go to 320
           !   two-range zone
290        if (irange.eq.1) go to 240
           if (irange.eq.2) go to 270
           !   three-range zone
300        if (irange.eq.1) go to 240
           if (irange.eq.2) go to 310
           if (irange.eq.3) go to 270
           !   second range (3-r case)
310        xbeg = azepo(indxl2+1)
           fbeg = azepo(indx0)
           if ( izone .eq. 1 )  fbeg = fbeg / 10.
           xend = azepo(indxr3+1)
           fend = azepo(indxr1+3)
320        call shira (xbeg,xend,fbeg,fend,iter,indx0,irange)
330     end do
340  end do
     if ( idebug .lt. 4 )  go to 460
     write (lunit6,7083)              !  thl
7083 format (//, ' after do 340 loop in adjcr', /, 1x,4x, 5hxbegr,3x,5halpha,10x, &
          38hpoles and zeroes after adjustment (hz),/)
     index = 0
     do irange=1,ntotra
        alpha = azepo(index+2)
        jn = absz(alpha)+onehav
        i1 = index+1
        i2 = i1+1+2*jn
        write (lunit6,7079) (azepo(i),i=i1,i2)
7079    format (1x,e12.4,f4.0,6e11.4,/,17x,6e11.4,/,17x,6e11.4)
        index = i2
620  end do
460 end do
  return
end subroutine adjcr

!
! subroutine shira.
!

subroutine shira (xbeg, xend, fbeg, fend, iter, index, irange)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  dimension fpz(50)
  include 'labl39.ftn'
  include 'deck39.ftn'
  !  change this limit if dimension is changed
  !   order of slope in segment * 2
  mdifpz = 100
  iflgrh = 0
  if (izone.eq.nzone.and.irange.eq.nrange.and.iftype.eq.2) iflgrh = 1
  alpha = azepo(index+2)
  jn = absz(alpha)+onehav
  j2n = 2*jn
  if (j2n.gt.mdifpz) go to 2100
  id = index+2
  if (idebug.ge.3) write (lout,8083) irange,xbeg,xend
8083 format (/,1x,5x,27hcorners adjustment of range,i3, 2h :, 2x, 3hx1=,e10.4,3x, 3hx2=,e10.4)
  d13 = 1.d0 / jn
  d13 = sqrtz( d13 )
  erropt = 1.d12    !  thl
  do isub = 1,3
     dxecr = .5d0/isub
     tolerr = 1.01d0**(1./isub**2)
     dfx = 10.d0**( dxecr*.1d0 )
     fdelta = dfx**d13
     fdelt1 = 1.d0 / fdelta
     !   preserve original positions and prepare for shifting
     do j = 1,j2n
        fpz(j) = azepo(id+j)
        azepo(id+j) = azepo(id+j) * fdelt1
100  end do
     !   shift to the right
     !      erropt = 1.d12       ! i think this should be moved outside loop
     idefac = -1
     idefop = 0
     do jsh = 1,50
        fcorn2 = azepo(id+j2n)*fdelta
        if (fcorn2.gt.fend) go to 160
        idefac = idefac+1
        do j = 1,j2n
110        azepo(id+j) = azepo(id+j)*fdelta
        end do
        !   check error
        if (iflgrh.eq.1) call refh
        error = 0.
        x = xbeg-dxecr
120     x = x+dxecr
        if (x.gt.xend) go to 130
        f = 10.d0**x
        call ratp (f,amaglg)
        yapprx = amaglg
        error = error+(yapprx-yfun39(x))**2
        go to 120
130     if (idebug.ge.4) write (lout,8085) idefac,fcorn2,fend,error,erropt
8085    format (1x, 7hshift r,i5,4e12.4)
        if (error.ge.erropt) go to 140     ! thl
        erropt = error
        idefop = idefac
140     if (error.ge.tolerr*erropt) go to 160 ! thl
150  end do
     !   restore original values before shifting to the left
160  do j = 1,j2n
170     azepo(id+j) = fpz(j)
     end do
     !   shift to the left
     idefac = 0
     do jsh = 1,50
        fcorn1 = azepo(id+1) * fdelt1
        if (fcorn1.lt.fbeg) go to 230
        idefac = idefac-1
        do j = 1,j2n
180        azepo(id+j) = azepo(id+j) * fdelt1
        end do
        !   check error
        if (iflgrh.eq.1) call refh
        error = 0.
        x = xbeg-dxecr
190     x = x+dxecr
        if (x.gt.xend) go to 200
        f = 10.d0**x
        call ratp (f,amaglg)
        yapprx = amaglg
        error = error+(yapprx-yfun39(x))**2
        go to 190
200     if (idebug.ge.4) write (lout,8087) idefac,fcorn1,fbeg,error,erropt
8087    format (1x, 7hshift l,i5,4e12.4)
        if (error.ge.erropt) go to 210   !  thl
        erropt = error
        idefop = idefac
210     if (error.ge.tolerr*erropt) go to 230  !  thl
220  end do
     !   shift according to optimum value
230  fdelt2 = fdelta**idefop
     do j = 1,j2n
240     azepo(id+j) = fpz(j) * fdelt2
     end do
250 end do
  if (iflgrh.eq.1) call refh
  return
  !             terminating conditions
2100 write (lout,8089) mdifpz
8089 format (//,1x,22h%%% dimension of fpz (,i2, 36h) exceeded. execution terminated %%%   )
  call stoptp
  return
end subroutine shira

!
! subroutine inran.
!

subroutine inran (irange, indx0, indxr1, indxr2, indxr3, indxl1, indxl2)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'labl39.ftn'
  include 'deck39.ftn'
  id = irange
  indx0 = indxv(id)
  id = id+1
  if (id.gt.ntotra) id = ntotra
  indxr1 = indxv(id)
  id = id+1
  if (id.gt.ntotra) id = ntotra
  indxr2 = indxv(id)
  id = id+1
  if (id.gt.ntotra) id = ntotra
  indxr3 = indxv(id)
  id = irange-1
  if (id.lt.1) id = 1
  indxl1 = indxv(id)
  id = id-1
  if (id.lt.1) id = 1
  indxl2 = indxv(id)
  return
end subroutine inran

!
! subroutine refh.
!

subroutine refh
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'labl39.ftn'
  include 'deck39.ftn'
  xref = xdat(1)
  i2 = ntotra
  if (iftype.eq.2) i2 = ntotra-1
  href = 1.d0
  if (i2.eq.0) go to 180
  do irange = 1,i2
     id = indxv(irange)
     alpha = azepo(id+2)
     if (alpha.gt.0.) go to 110
     jn = -alpha+onehav
     indexp = id+2
     indexz = indexp+jn
     go to 120
110  jn = alpha+onehav
     indexz = id+2
     indexp = indexz+jn
120  do j=1,jn
        fpole = azepo(indexp+j)
        fzero = azepo(indexz+j)
        href = href*fpole/fzero
130  end do
100 end do
180 hreflg = alog1z(href)
  if (iftype.eq.1) go to 170
  id = indxv(ntotra)
  alpha = azepo(id+2)
  jn = -alpha+onehav
  indexp = id+2
  do j=1,jn
     fpole = azepo(indexp+j)
     hreflg = hreflg+alog1z(fpole)
140 end do
170 hreflg = hreflg+yfun39(xref)
  return
end subroutine refh

!
! subroutine ratp.
!

subroutine ratp (freq,amaglg)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'labl39.ftn'
  include 'deck39.ftn'
  i2 = ntotra
  if (iftype.eq.2) i2 = ntotra-1
  freq2 = freq**2
  amagp = 1.d0
  if (i2.eq.0) go to 180
  do irange = 1,i2
     id = indxv(irange) + 2
     alpha = azepo( id )
     if (alpha.gt.0.) go to 110
     jn = -alpha+onehav
     indexp = id
     indexz = indexp+jn
     go to 120
110  jn = alpha+onehav
     indexz = id
     indexp = indexz+jn
120  do j = 1, jn
        fpole = azepo(indexp+j)
        fzero = azepo(indexz+j)
        amagp = amagp*(fzero**2+freq2)/(fpole**2+freq2)
     end do
130  continue
  end do
100 continue
180 amaglg = alog1z(amagp)
  if (iftype.eq.1) go to 170
  id = indxv(ntotra) + 2
  alpha = azepo( id )
  jn = -alpha+onehav
  do j=1,jn
     fpole = azepo( id+j )
     amaglg = amaglg-alog1z(fpole**2+freq2)
140 end do
170 amaglg = hreflg+amaglg/2.d0
  return
end subroutine ratp

!
! subroutine split.
!

subroutine split (xbeg, xend, ycutpu, xcut)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'labl39.ftn'
  include 'deck39.ftn'
  dxspli = .1d0
  xfirst = xdat(1)
  xlast = xdat(ndata)
  ycut = refa + ycutpu * ( refb - refa )
  if ( absz(xbeg-xend) .lt. 0.1 )  dxspli = .01d0  ! avoid overshot
  if (xbeg.gt.xend) go to 130
  if( refa .lt. refb )   go to 110
90 x = xbeg-dxspli
100 x1 = x
  x = x+dxspli
  if (x.gt.xlast) x = xlast
  if (yfun39(x).le.ycut) go to 170
  if( x .eq. xlast )  go to  110
  go to 100
110 x = xbeg-dxspli
120 x1 = x
  x = x+dxspli
  if (x.gt.xlast) x = xlast
  if (yfun39(x).ge.ycut) go to 170
  if( x .eq. xlast )  go to  90
  go to 120
130 if( refa .gt. refb )  go to 150
135 x = xbeg+dxspli
140 x1 = x
  x = x-dxspli
  if (x.lt.xfirst) x = xfirst
  if (yfun39(x).ge.ycut) go to 170
  if( x .eq. xfirst )  go to 150
  go to 140
150 x = xbeg+dxspli
160 x1 = x
  x = x-dxspli
  if (x.lt.xfirst) x = xfirst
  if (yfun39(x).le.ycut) go to 170
  if( x .eq. xfirst )  go to 135
  go to 160
170 if (x1.lt.xfirst) x1 = xfirst
  d1 = yfun39(x1)
  xcut = x1+(x-x1)*(ycut-d1)/(yfun39(x)-d1)
  return
end subroutine split

!
! function yfun39.
!

function yfun39 (x)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'labl39.ftn'
  include 'deck39.ftn'
  if (x.le.xdat(1)) go to 120
  if (x.ge.xdat(ndata)) go to 100
  ai = aptdec*(x-xdat(1))+1.d0
  i1 = ai
  y = (x-xdat(i1))*(ydat(i1+1)-ydat(i1))/(xdat(i1+1)-xdat(i1))
  yfun39 = y + ydat(i1)
  go to 110
120 yfun39 = ydat(1)
  go to 110
100 yfun39 = ydat(ndata)
110 return
end function yfun39

!
! function aph.
!

function aph (x)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'labl39.ftn'
  include 'deck39.ftn'
  if (x.le.xdat(1)) go to 120
  if (x.ge.xdat(ndata)) go to 100
  ai = aptdec*(x-xdat(1))+1.d0
  i1 = ai
  aph = ( x-xdat(i1) ) * ( aphdat(i1+1)-aphdat(i1) ) / ( xdat(i1+1)-xdat(i1) ) + aphdat( i1 )
  go to 110
120 aph = aphdat(1)
  go to 110
100 aph = aphdat(ndata)
110 return
end function aph

!
! subroutine ftplot.
!

subroutine ftplot (icurve, imode, nfitmx)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  real(8)       text1,text2,text3,blank,pl
  dimension pl(92)
  include 'labl39.ftn'
  include 'deck39.ftn'
  data text1 / 1h0 /, text2 / 1hi /, text3 /1h* /, blank / 1h  /
  do j=1,92
200  pl(j) = blank
  end do
  if ( icurve .eq. 2 .and. nfitmx .eq. 0 )  go to 120
  ymin =  1.d20
  ymax =  -1.d20
  do i=1,ndata
     d1 = ydat(i)
     if (d1.gt.ymin) go to 110
     ymin = d1
110  if (d1.lt.ymax) go to 100
     ymax = d1
100 end do
  amin = .90d0*10.d0**ymin
  amax = 1.1d0*10.d0**ymax
  dfamp = amax-amin
  if ( nfitmx .gt. 0 )  go to 112   !  ti fitting
  write (lout,8091) imode
8091 format (//, ' Printer plot of  yc  fitting for mode', i3, /, ' plot of ycmag vs. yceqmag in mhos', /, ' plot symbols:   0 = ycmag     i = yceqmag     * = intersection point.')
  write (unit = lout, fmt = 8093) amin, amax
8093 format (/, 3x, 'ycmag', 6x, 'yceqmag', 15x, e10.4, 74x, e10.4, /, 28x, 'freq', /, 35x, '.1', 10('........1'))
  go to 130
112 write (unit = lout, fmt = 114) icurve, imode
114 format (//, ' Printer plot of  ti  fitting for element', 2i2, /, ' plot of timag vs. tieqmag', /, ' plot symbols:   0 = timag     i = tieqmag     * = intersection point.')
  write (unit = lout, fmt = 116) amin, amax
116 format (/, 3x, 'timag', 6x, 'tieqmag', 15x, e10.4, 74x, e10.4, /, 28x, 'freq', /, 35x, '.1', 10('........1'))
  go to 130
120 if (iftype .eq. 1) go to 122
  amin = 0.
  amax = 1.d0
  dfamp = 1.d0
  go to 124
122 amin = 0.
  amax = 1.5d0
  dfamp = 1.5d0
124 write (lout,8095) imode
8095 format (//, ' Printer plot of  a1  fitting for mode', i3, /, ' plot of a1mag vs. a1eqmag in p.u.', /, ' plot symbols:   0 = a1mag     i = a1eqmag     * = intersection point.')
  write (unit = lout, fmt = 8097) amin, amax
8097 format (/, 3x, 'a1mag', 6x, 'a1eqmag', 15x, e10.4, 74x, e10.4, /, 28x, 'freq', /, 35x, '.1', 10('........1'))
130 x = xdat(1)-.2d0
810 x = x+.2d0
  if (x .gt. xdat(ndata)) go to 850
  freq = 10.d0**x
  freq2 = freq**2
  amagp = 1.d0
  if (nzeror.eq.0) go to 825
  do j = 1, nzeror
     amagp = amagp*(fczr(j)**2+freq2)/(fcpr(j)**2+freq2)
  end do
820 continue
825 amaglg = alog1z(amagp)
  if (iftype.eq.1) go to 840
  j1 = nzeror+1
  do j = j1, npoler
     amaglg = amaglg-alog1z(fcpr(j)**2+freq2)
  end do
830 continue
840 amaglg = hrflgr+amaglg/2.d0
  ax = 10.d0**yfun39(x)
  amagp = 10.d0**amaglg
  intd1 = (ax-amin)*90.d0/dfamp+1.5d0
  intd2 = (amagp-amin)*90.d0/dfamp+1.5d0
  if (intd1.lt.1.or.intd1.gt.91) intd1 = 92
  if (intd2.lt.1.or.intd2.gt.91) intd2 = 92
  pl(intd1) = text1
  pl(intd2) = text2
  if (intd1.eq.intd2) pl(intd1) = text3
  write (lout,8099) ax,amagp,freq,( pl(i), i=1,91 )
8099 format (1x,e10.4,2x,e10.4,2x,e10.4,1x,91a1)
  pl(intd1) = blank
  pl(intd2) = blank
  go to 810
850 return
end subroutine ftplot

!
!     subroutine misc39.
!

subroutine misc39
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labl39.ftn'
  !     This module is called by "subr39"  of
  !     overlay 39 when new miscellaneous data cards are required
  !     (possibly once for each mode,  in the most extreme case).
  character(8) :: text1, text2, text3, text4
  dimension itemp(9)
  equivalence (kdeflt, indtv(1))
  data text1 / 'data  ' /
  data text2 / 'select' /
  data text3 / 'defaul' /
  data text4 / 't     ' /
  if (iprsup .ge.  6) write (lunit6, 2243)  modify
2243 format (' Top of "misc39".   modify =', i4)
  if ( modify  .eq.  0 )   go to 2268
  !     read input card using cimage
2245 if ( kdeflt  .ne.  1 ) call cimage
  read (unit = abuff, fmt = 2247) bus1, bus3
2247 format (  2a6  )
  if (bus1 .ne. text1) go to 4063
2268 continue
  read (unit = abuff, fmt = 2247) bus1, bus3
  if ( bus1  .ne.  text3 )   go to 2274
  if ( bus3  .ne.  text4 )   go to 2274
  write (kunit6, 2270)
2270 format ('+Request for default fitting (= 3 blank cards).')
  call interp
  kdeflt = 1
  !     erase "default" from "abuff" (installation-dependent):
  call defblk ( abuff )
2274 if ( bus1  .ne.  text2 )   go to 2286
  modesk = 1
  read (unit = abuff, fmt = 2281) itemp
2281 format ( 8x,  9i8 )
  do j=1, 9
     if ( itemp(j)  .eq.  0 )   go to 2283
     k = 1
     if ( itemp(j)  .lt.  0 )   k = 2
     n3 = iabsz ( itemp(j) )
     modskp(k, n3) = 1
2283 end do
  !     read input card using cimage
  call cimage
  go to 2268
2286 continue
  read (unit = abuff, fmt = 2289) bus2
2289 format ( 66x,  a6 )
  !     begin code to read overall program miscellaneous param.:
  read (unit = abuff, fmt = 4057) idebug, ipunch, koutpr, gmode
4057 format ( 8x,3i8,e8.0)
  if ( gmode.le.0.0 .and. metrik.eq.0 ) gmode=.48d-7
  if ( gmode.le.0.0 .and. metrik.eq.1 ) gmode=.30d-7
  if ( koutpr  .eq.  0 )   koutpr = 2
  if ( koutpr  .lt.  0 )   koutpr = 0
  if ( kdeflt  .eq.  0 ) write (kunit6, 4059) gmode,ipunch,idebug,koutpr
4059 format (  8h+param. ,e8.1,3i8)
  if ( modify  .ne.  0 )   go to 2245
  go to 9900
4063 if ( modify  .eq.  2 )   go to 4387
  !     begin code for miscellaneous param. of  zc  fitting
  read (unit = abuff, fmt = 4065) nexmis, epstol, normax, iecode, ifwta, ifplot, ifdat, inelim
4065 format (i8,e8.0,6i8)
  if ( nexmis  .eq.  0 )   nexmis = -1
  if (epstol.eq.0) epstol = .3d0
  d1 = epstol
  d13 = 1.0 + epstol / 100.
  epstol = alog1z ( d13 )**2
  if ( normax  .le.  0 )   normax  = 30
  if ( kdeflt  .eq.  0 ) write (kunit6, 4082)  nexmis,d1,normax,iecode, ifwta,ifplot,ifdat,inelim
4082 format ('+Yc fit.', i3, e8.1, 6i3)
  call interp
  go to 9900
  !     begin code for miscellaneous param. of a1 fitting
4387 continue
  read (unit = abuff, fmt = 4391) nexmis, epstol, normax, iecode, ifwta, ifplot, ifdat, inelim, amina1
4391 format (i8,e8.0,6i8,e8.0)
  if (epstol.eq.0) epstol = .3d0
  d1 = epstol
  d13 = 1.0 + epstol / 100.
  epstol = alog1z ( d13 )**2
  if ( normax .le. 0 )   normax  = 30
  if (amina1.eq.0.) amina1 = .05d0
  if ( kdeflt  .eq.  0 ) write (kunit6, 4396)  nexmis,d1,normax,iecode, ifwta,ifplot,ifdat,inelim,amina1
4396 format ( 8h+a1 fit.,i3,e8.1,6i3,e8.1)
  call interp
9900 if ( iprsup  .ge.  6 ) write (lunit6, 9903)
9903 format ( 15h exit "misc39".  )
  return
end subroutine misc39

!
!     subroutine defblk.
!

subroutine defblk (abuff)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  character(*), intent(out) :: abuff
  !     Almost-universal module for blanking out "abuff" card
  !     image of "default" data card of "marti setup".   The one
  !     and only call is by module "misc39" of overlay 39.   It
  !     is not universal only for those computers (e.g., apollo)
  !     which have  "character abuff*80"  declaration.
  abuff(1 : 8) = abuff(40 : 48)
  abuff(16 : 24) = abuff(40 : 48)
  return
end subroutine defblk

!
! end of file over39.f90
!
