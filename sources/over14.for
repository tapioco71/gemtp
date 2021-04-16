!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over14.for
!
!
!     subroutine over14.
!
subroutine over14
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  equivalence   ( moncar(1), knt )
  if ( iprsup  .ge. 1 ) write ( lunit6, 4567 )
4567 format ( 24h  "begin module over14." )
  !     define "kentnb" and "nbhdsw" vectors for module "switch"
  !     of overlay 16 (called by "subts1") via  "do 2472":
1310 n6 = 0
  if ( kswtch  .le.  0 )   go to 2483
  mswtch = lswtch * 3
  do j=1, kswtch
     kentnb(j) = n6 + 1
     k = kmswit(j)
     ndx1 = lswtch + j
     m = kmswit(ndx1)
     do i=1, kswtch
        if ( i  .eq.  j )   go to 2461
        ndx2 = lswtch + i
        if (k .ne. kmswit(i) .and. k .ne. kmswit(ndx2) .and. m .ne. kmswit(i) .and. m .ne. kmswit(ndx2)) go to 2461
        n6 = n6 + 1
        if ( n6  .gt.  mswtch )  go to  1983
        nbhdsw(n6) = i
2461 end do
2472 end do
  kentnb(kswtch+1) = n6 + 1
2483 continue
  !     initialize counters for the -666 branches    *   *   *   *   *   *
  isfd = 0
  ibf = 0
  ikf = 0
  call mover0 ( finit(1), ntot )
  if ( iprsup  .gt.  0 ) write (lunit6, 4286)  inonl, nv, ibr, it, ntot, nenerg, knt, fltinf, flzero, omega, delta2, deltat
4286 format ( /,  44h various scalars at beginning of  'last14' .   ,/, 1x, 56h   inonl      nv     ibr      it    ntot  nenerg     knt, &
          9x, 6hfltinf, 9x, 6hflzero, 10x, 5homega, 9x, 6hdelta2, 9x, 6hdeltat, /,  1x, 7i8, 5e15.6  )
  i = inonl
  go to 73577
73571 if( nltype(i) .gt. 0 )  go to 73574
  if( nltype(i) .ne. -97 )  go to 73572
  curr(i) = 0.0
  if( anonl(i) .ge. 0.0 )  go to 73572
  curr(i) = 1.0
  anonl(i) = -1.0 / fltinf
73572 k = nonlk(i)
  n1 = nonlm(i)
  m = iabs(n1)
  n15 = k
  if ( n15  .eq.  1 )   n15 = m
  n16 = iabs ( kssfrq(n15) )
  omega = twopi * sfreq(n16)
  d1 = ( f(k) - f(m) ) / omega
  d2 = e(k) - e(m)
  if ( nltype(i)  .ne.  -96 )  go to 73584
  n7 = nonlad(i)
  curr(i) = cchar(n7+3)
  anonl(i) = gslope(n7)
  if ( iprsup  .ge.  2 ) write (lunit6, 2620) i, n7, curr(i), vnonl(i), anonl(i)
2620 format ( /,  21h type-96 in "over14"., 16h       i      n7,  21x, 4hcurr,  20x, 5hvnonl, 20x, 5hanonl  ,/,  21x,  2i8,  3e25.15  )
  go to 73585
73584 if ( nltype(i)  .ne.  -98 )  go to 73574
  nonle(i) = -nonle(i)
  j0 = nonlad(i)
  anonl(i) = gslope(j0) * ( d1/delta2 - d2 )
73585 k = nonlk(i)
  m = iabs( nonlm(i) )
  finit(m) = finit(m) + anonl(i)
  finit(k) = finit(k) - anonl(i)
  if( iprsup .gt. 0 ) write(lunit6, 594)  i, k, m, j0,  ( finit(j), j=1, ntot )
594 format( /,  38h finit(j), j=1, ntot at 594 of main14.   , 4i10, /, ( 1x, 5e25.15 ) )
73574 i = i - 1
73577 if( i .gt. 0 )  go to 73571
  if( inonl .gt. 0   .and.   iprsup .gt. 0 ) write (lunit6, 73578)  ( i, nonlk(i), nonlm(i), nltype(i), nonlad(i), nonle(i), ilast(i), vnonl(i), curr(i), &
       anonl(i), vzero(i), i=1, inonl )
73578 format ( /,  59h nonlinear-element table before  (y)  formation, in over14.  ,/, 7x, 3hrow, 5x, 5hnonlk, 5x, 5hnonlm, 4x, 6hnltype, &
           4x, 6hnonlad, 5x, 5hnonle, 5x, 5hilast, 10x, 5hvnonl, 11x, 4hcurr, 10x, 5hanonl, 10x, 5hvzero, /, &
           ( 7i10, 4e15.5 ) )
  call last14
  !     following 2 cards extracted from "last14" (no ov16 use):
  if ( numsm  .gt.  0 ) call past
  lastov = nchain
  if ( kill  .gt.  0 )   go to 9600
  nchain = nchain + 1
  !     scan branch tables for the -666 branches and initialize branch
  if( ibr .lt. 2 ) go to 99999
  !     variables    *   *   *   *   *   *   *   *   *   *   *   *   *   *
  do ka = 2, ibr
     if( length(ka) .ne. -666 ) go to 401
     kb = ka - 1
     it2 = length(kb)
     it2 = iabsz( it2 )
     it2d = ka + it2 - 1
     kc = 0
     do kd = kb, it2d
        kc = kc + 1
        lfd = iabsz( kbus(kd) )
        mfd = iabsz( mbus(kd) )
400     volt(kc) = e(mfd) - e(lfd)
     end do
     call breqiv( ikf, isfd, ibf )
401 end do
  go to 9800
1983 kill = 1
  write ( lunit6, 1984 )
1984 format ( 47h ------ switch vector nbhdsw overflow at over14 , 50h to solve the problem, just enlarge list 6 -----  )
  lstat(16) = 6
9600 lstat(18) = 14
  nchain = 51
9800 if ( iprsup  .ge.  1 ) write (lunit6, 9817)
9817 format ( 16h exit  "over14".   )
99999 return
end subroutine over14
!
! subroutine last14.
!
subroutine last14
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  dimension cblhst(1)
  equivalence  ( cnvhst(1),  cblhst(1) )
  dimension infdli(1)
  equivalence  ( namebr(1),  infdli(1) )
  dimension  wk1(1)
  equivalence ( semaux(1), wk1(1) )
  common /fdqlcl/ koff1,koff2,koff3,koff4,koff5,koff6,koff7,koff8,koff9,koff10,koff13,koff14,koff15,koff16,koff17,koff18, &
       koff19,koff20,koff21,koff22,koff23,koff24,koff25,inoff1,inoff2,inoff3,inoff4,inoff5,nqtt, lcbl, lmode, nqtw
  !     beginning of formation of the real admittance matrix (y)
  icucpl = 0
  jglnn = 0
  isecti = 400
  if ( iprsup .ge. 1 ) write (lunit6, 2213) (kodsem(i), i=1, ibr)
2213 format ( 45h top of "last14".  kodsem(1:ibr) follow  ....   ,/,  ( 1x,  20i6 ) )
  if ( iprsup  .ge.  5 ) write (lunit6, 2217)  ( indhst(i), i=1, ibr )
2217 format ( 45h                   indhst(1:ibr) follow  ....   ,/,  ( 1x,  20i6 ) )
  kss=lymat+1
  ialter = 1
  l = 1
  !                                       establishing l-th y-row from bra
600 l=l+1
  call mover0 ( f(1), ntot )
  k=1
620 n1=kbus(k)
  it2=length(k)
  if ( iprsup  .ge.  6 ) write (lunit6, 607)  l, ntot, k, n1, it1, it2, ibr, kss
607 format ( /,   8h at 607.,   64h       l    ntot       k      n1  it1     it2     ibr     kss      ,/,  8x,  8i8  )
  if(n1.lt.0) go to 690
  it2=iabs(it2)
  if(it2.eq.1) go to 650
  it1=k
  do i=1,it2
     if(kbus(it1).eq.l) go to 700
     if(mbus(it1).eq.l) go to 710
     go to 630
700  icheck=0
     go to 720
710  icheck=1
720  i1 = i -1
     n1=nr(it1)
     !                                          !>column of branch it1 being
     do j0=1, it2
        j = j0 -1
        n2=k+j
        if(icheck.ne.0) go to 760
        n3=kbus(n2)
        n4=mbus(n2)
730     if(j.le.i1) go to 770
        n2=nr(n2)+i1
740     yx=tx(n2)
        if( kodebr(k) .gt. 0 )   yx = x(n2)
        f(n3)=f(n3)+yx+c(n2)/2.0
        f(n4)=f(n4)-yx
        if ( iprsup  .ge.  9 ) write (lunit6, 746)  n3, n4, n2, kodebr(k), yx, c(n2), f(n3), f(n4)
746     format (  39h coupled elem adds to (y).  n3, n4, n2,,37 h kodebr(k), yx, c(n2), f(n3), f(n4) =,4i10  ,/,  1x,  4e25.15  )
        go to 750
760     n3=mbus(n2)
        n4=kbus(n2)
        go to 730
770     n2=n1+j
        go to 740
750  end do
630  it1=it1+1
  end do
640 k=k+it2
  if ( iprsup  .ge.  6 ) write (lunit6, 643)  k, it2, ibr, kodsem(k)
643 format ( /,  35h at 643.   k, it2, ibr, kodsem(k) =,  4i8  )
  if(k.le.ibr) go to 620
  go to 780
650 mabs=iabs(mbus(k))
  if(n1.eq.l) go to 660
  if(mabs.eq.l) go to 670
  go to 640
660 n4=mabs
  go to 680
670 n4=n1
680 n3=nr(k)
  n2=iabs(n3)
  if ( iprsup .ge. 3 ) write (*,*) ' series r-l-c. n3, n2, x(n2), f(l), f(n4) =', n3, n2, x(n2), f(l), f(n4)
  yx=x(n2)
  if(n3.gt.0) yx=1.0/yx
  f(l)=f(l)+yx
  f(n4)=f(n4)-yx
  if(n3.ge.0) f(l)=f(l)+c(n2)/2.0
  if ( iprsup .ge. 3 ) write (*,*) ' after r-l-c.   f(l), f(n4) =',  f(l), f(n4)
  go to 640
690 if ( imodel(k) .ne. -3 ) go to 3681
  !                                            for testing new logic which
3681 if ( imodel(k) .ne. -4 ) go to 3688
  ioff1 = 0
  ioff2 = ioff1 + isecti
  ioff3 = ioff2 + isecti
  ioff4 = ioff3 + isecti
  ioff5 = ioff4 + isecti
  ioff6 = ioff5 + isecti
  ioff7 = ioff6 + isecti
  ioff8 = ioff7 + isecti
  if ( ioff8 .le. lymat ) go to 5337
  write(lunit6, 5338) lymat, ioff8
5338 format( ' In over14, new Marti line solution logic overflowed sto', 'rage list no. 5: lymat =', i5, ' needed space here ioff8=',i5, &
          '.',/, ' execution is aborted, and redimension with larger value of list no. 5 is required.'  )
  stop
5337 lcbl = 0
  lmode= 0
8893 koff21 = koff20 + nqtt
  koff22 = koff21 + lbrnch
  koff23 = koff22 + nqtw
  koff24 = koff23 + nqtw
  koff25 = koff24 + 288
  koff11 = koff25 + 288
  if ( koff11 .lt. lhist) go to 5339
  write (lunit6,5340)  lhist, koff11
5340 format( ' In over14, new Marti line solution logic overflowed sto', 'rage list no. 22: lhist =', i5,' needed space here koff11=',i5, &
          '.',/, ' execution is aborted, and redimension with larger value of list no. 22 is required.'  )
5339 kq = k
  if ( l .gt. 2 ) go to 9743
  !
  !        evaluate integration constants of q
  !
7896 kq = k
  nphs = abs(length(kq))
  nphs2 = nphs*nphs
  icb=infdli(inoff1+kq)
  nq1=infdli(inoff2+kq)
  nq0=infdli(inoff1+kq)
  nnq1=infdli(inoff3+kq)
  do iq=1,nphs2
     nteq = wk1(koff20+icb)
     sume = 0.d0
     sumd = sconst(nq1)
     if ( iprsup .gt. 0 ) write(*,4839)
4839 format(45h  i  nteq  nnq1  nnq2  nnq3   nq4   nq5   nq6,42h        qk0(i)    cj(ntermq)    dj(ntermq),42h    ej(ntermq)      sumdj(i)      sumej(i))
     jkl = 0
     icbscn = lhist + icb
     nticpl = cblhst( icbscn )
     icucpl = icucpl + nticpl
     icucpl  = icucpl * 3
     nk1 = lhist + ifd + icucpl - nticpl * 3
     do i=1, nteq
        nq2 = nq1 + i
        nq3 = nq2 + nteq
        nnq2 = nnq1 + i
        nnq3 = nnq2 + nteq
        nq4 = nnq3 + nteq
        nq5 = nnq1 + 3 * nteq + 1
        nq6 = nq5 + 1
        if ( sconst(nq2) .ge. 1.e+13 ) go to 1001
        apidt = sconst(nq3) * deltat
        cj = dexp(-apidt)
        hj = ( 1.d0 - cj ) / apidt
        apidt = sconst(nq2) / sconst(nq3)
        dj = apidt * ( 1.d0 - hj )
        sconst(nq4) = - apidt * ( cj - hj )
        sconst(nnq2) = cj
        sconst(nnq3) = dj
        sume        = sume + sconst(nq4)
        sconst(nq6) = sume
        sumd        = sumd + dj
        sconst(nq5) = sumd
        go to 1002
1001    jkl = jkl + 1
        if (jkl .eq. 2 ) go to 1004
        fac1 = sconst(nq3)*deltat/1.e15
        sconst(n5) = expz(-fac1)
        dblpr2 =  sconst(n5)
        fac3 = sconst(nq3+1) * deltat / 1.e15
        fac4 = sconst(nq3+1) / 1.e15
        fac5 = sconst(nq3) / 1.e15
        call dcosz(fac3, dblpr3)
        call dsinz(fac3, dblpr4)
        dblpr1 = dblpr2 * dblpr3
        dblpr2 = dblpr2 * dblpr4
        d18 = fac4*fac4 + fac5*fac5
        nn10 = nk1  + 1
        nn11 = nn10 + 1
        nn12 = nn11 + 1
        nk1 = nk1 + 3
        if ( nk1 .ge. ihist+lfd/2 ) write( lunit6 , 1005) nk1, ihist, lfd
1005    format(28h second part of cnvhst nk1= ,i4,19h larger than ihist ,i4,9h + lfd/2 , i4 )
        if ( nk1 .ge. ihist+lfd/2 ) stop
        sk1r = sconst(nq2)/1.e+15
        sk1i = sconst(nq2+1)/1.e+15
        rll = (  (unity - dblpr1) * fac5 + dblpr2 * fac4 )
        sll = (  dblpr2 * fac5 - (unity - dblpr1) * fac4 )
        yll = sk1r * rll - sk1i * sll
        xll = sk1r * dblpr1 + sk1i * dblpr2
        sconst(nnq2) = 2*dblpr1
        sconst(nnq3) = 2*sk1r - yll/(deltat*d18)
        sconst(nq4) = -2*( xll - yll/(deltat*d18) )
        sumd        = sumd + sconst(nnq3)
        sconst(nq5) = sumd
        sume        = sume + sconst(nq4)
        sconst(nq6) = sume
1002    if ( iprsup .gt. 0 ) write(*,145) i,nteq,nnq1,nnq2,nnq3,nq4,nq5,nq6,sconst(nq1),sconst(nnq2),sconst(nnq3), &
             sconst(nq4),sconst(nq5),sconst(nq6)
145     format(1x,i2,3x,i3,2x,i4,2x,i4,2x,i4,2x,i4,2x,i4,2x,i4,6e14.5)
        if ( jkl .eq. 1 ) go to 100
1004    jkl = 0
100  end do
     nnq1 = nq6
     nq1 = nq3 + 1
     icb=icb+1
     !     q0(nq0) = sconst(nq5)                                            !
     wk1(koff22+nq0) = sconst(nq5)
     nq0 = nq0 + 1
103 end do
  !
  !        evaluate equivalent admittance
  !
102 icb = infdli(inoff1+k)
  icu = infdli(inoff4+kq)
  !      call qyqtr( nphs, wk1(koff21+kq),
  !     1            wk1(koff22+icb), wk1(koff23+icu) )
  !        this subroutine evaluates the matrix product y = q * d * qt
  !        where d is a diagonal matrix and y will be symmetrical
  !        all matrices are real.
  nnn1=nphs*(nphs+1)/2
  do i=1,nnn1
     !      y(i)=0.d0
     wk1(koff23+icu-1+i) = 0.0
1009 end do
  if ( iprsup .gt. 0 ) write(*,4849)
4849 format('  nnk   km1    j   i   ij   ik', '       dyk-y05           qjk           qik', '             b   yeq(koff23)')
  do nnk=1,nphs
     km1 = (nnk - 1 ) * nphs
     !      dyk = dy(nnk)
     dyk = wk1(koff21+kq-1+nnk)
     ij = 0
     do j = 1, nphs
        jk = km1 + j
        !      qjk = q(jk)
        qjk = wk1(koff22+icb-1+jk)
        do i = 1 , j
           ij = ij + 1
           ik = km1 + i
           !      qik = q(ik)
           qik = wk1(koff22+icb-1+ik)
           b = qik * qjk
           !      y(ij) = y(ij) + dyk * b
           wk1(koff23+icu-1+ij) = wk1(koff23+icu-1+ij) + dyk * b
           if ( iprsup .gt. 0 ) write(*,445)  nnk,km1,j ,i,ij ,ik, dyk,qjk,qik,b, wk1(koff23+icu-1+ij)
445        format(1x,i4,2x,i4,2x,i4,2x,i4,1x,i4,2x,i4,5e14.5)
1039    end do
1029 end do
101 end do
9743 if ( iprsup .gt. 0 ) write (*, *) ' k, it2, l, n1, mbus(k), ibr =', k, it2, l, n1, mbus(k), ibr
  if ( it2 .lt. 0 ) go to 682
  it2 = 1
  if ( -n1 .ne. l .and. iabs ( mbus(k) ) .ne. l ) go to 640
  f(l) = f(l) + wk1(koff21+icb)
  go to 640
682 it2 = -it2
  it1 = k - 1
  j0 = k
  iadrs = infdli(inoff4+kq) - 1
  do ibi = 1,it2
     it1 = it1 + 1
     if ( -kbus(it1) .eq. l ) go to 684
     if ( iabs ( mbus(it1) ) .eq. l ) go to 685
     go to 683
684  j0 = 0
685  ibrj = k
     do ibj = 1,it2
        if ( j0 .eq. 0 ) n1 = -kbus(ibrj)
        if ( j0 .ne. 0 ) n1 = iabs(mbus(ibrj) )
        if ( ibi .lt. ibj ) iyeq = iadrs + ibi + ( ibj - 1 ) * ibj/2
        if ( ibi .ge. ibj ) iyeq = iadrs + ibj + ( ibi - 1 ) * ibi/2
        f(n1) = f(n1) + wk1(koff23+iyeq)
        ibrj = ibrj + 1
686  end do
683 end do
  it1 = it1 + 1
  go to 640
!
3688 yx = absz ( ci(k) )
  !      if (kodsem(k) .ne. 0  .and. imodel(k) .ne. -2) go to 6920
  if (kodsem(k) .ne. 0  .and. imodel(k) .ge. 0) go to 6920
  it2 = -it2
  if (it2 .lt. 0 ) it2 = 1
  n2 = k + it2 - 1
  do i = k, n2
     if ( ck(i) .ge. 0.0 )  go to 21690
     j = cik(i)
     ci(i) = ci(i) / eta(j)
21690 end do
  it1=k
  j0=k
  do i=1,it2
     if((-kbus(it1)).eq.l) go to 694
     if(mbus(it1).eq.l) go to 695
     go to 693
694  j0=0
695  continue
     n2=k
     do n4=1,it2
        if(j0.eq.0) go to 696
        n1=mbus(n2)
        go to 697
696     n1=-kbus(n2)
697     yx = 0.0
        kj=k
        ij=litype(k)-1
        n4j=ij+n4
        ij=ij+i
        do j=1,it2
           yx=yx+qfd(ij)*absz(ci(kj))*qfd(n4j)
           if ( iprsup .ge. 3 ) write (lunit6, 2696) j, it2, kj,ci(kj), n4j, qfd(n4j),ij, qfd(ij),yx
2696       format (10h at  2696 , 10x,101h       j     it2      kj        ci(kj)       n4j       qfd(n4j)      ij        qfd(ij)             yx, &
                /, 20x, 3i8,e15.6,i8,e15.6,i8,2e15.6)
           ij=ij+it2
           n4j=n4j+it2
1696       kj=kj+1
        end do
        pit2 = it2
        yx = yx * pit2
        f(n1)=f(n1)+yx
        if ( iprsup .ge. 3 ) write (lunit6, 1697) n1, n2, n3, n4, yx, f(n1)
1697    format (64h      n1      n2      n3      n4             yx          f(n1),/, 4i8, e15.6,2x,e15.6)
698     n2=n2+1
     end do
     if ( iprsup .ge. 3 ) write (lunit6, 2697) ci1, l, f(l)
2697 format ( 29h ci1, l, and f(l) at 2697 are, 5x, e15.6,i8,e15.6)
693  it1=it1+1
  end do
  !     to recover the array ci
  n2 = k + it2 - 1
  do i= k, n2
     if ( ck(i) .ge. 0.0)  go to 31690
     j = cik(i)
     ci(i) = ci(i) * eta(j)
31690 end do
  go to 640
6920 it2 = iabs(kodebr(k))
  n1 = cik(k)
  do i=1, it2
     if (n1 .gt. 0) call mover0(volt(1), it2)
     n9 = k + i - 1
     if (-kbus(n9) .ne. l) go to 13010
     n3 = 1
     d1 = 1.0
     go to 13020
13010 if (iabs(mbus(n9)) .ne. l) go to 13200
     n3 = - 1
     d1 = 1.0
     if (kodsem(k) .lt. 0) d1 = -1.0
13020 kbr = k
13030 n4 = absz(cki(kbr)) - 1.0
     n5 = n4 / it2 + 1
     n4 = n4 - (n5 - 1) * it2 + 1
     if (n1 .lt. 0 .and. n4 .ne. i) go to 13140
     !      if (kodsem(kbr) .gt. 0 .and. imodel(kbr) .ne. -2)
     if (kodsem(k) .ne. 0  .and. imodel(k) .ge. 0) go to 13040
     n6 = - kodsem(kbr) + 2
     d2 = sconst(n6 - 2) + sconst(n6 - 1)
     go to 13050
13040 n6 = kodsem(kbr)
     d2 = sconst(n6)
     n7 = ci(kbr)
     n6 = n6 - n7 - n7 + 1
     if (n7 .gt. 0) n6 = n6 + 7 * n7
13050 n7 = ck(kbr)
     if (n7) 13060, 13110, 13070
13060 d2 = d2 + sconst(n6 + 1)
     go to 13110
13070 n7 = n6 + 4 * (n7 - 1)
     do j=n6, n7, 4
        if (sconst(j)) 13100, 13090, 13080
13080   d2 = d2 + sconst(j + 2)
13090   d2 = d2 + sconst(j + 2)
13100 end do
13110 if (iprsup .ge. 6) write (lunit6, 13115) k, l, i, n9, n3, n4, n5, n6, n7, d2
13115 format(100h at 13110.      k       l       i      n9      n3       n4      n5      n6      n7                d2   /,10x,9i8,5x,e15.7)
     if (n1 .lt. 0) go to 13120
     !  volt(.) is the i th row of (qfd) * (y-modal)
     n6 = (n4 - 1) * it2 + i + n1 - 1
     volt(n5) = volt(n5) + qfd(n6) * d1 * d2
     go to 13140
13120 n6 = k + n5 - 1
     !  add(subtract) i th row of y-phase to(from) the l-th row of (y-total)
     if (n3 .gt. 0) go to 13130
     n6 = iabs(mbus(n6))
     go to 13140
13130 n6 = - kbus(n6)
     e(n6) = e(n6) + d1 * d2
13140 kbr = kbr + 1
     if (cki(kbr - 1) .gt. 0.0) go to 13030
     if (n1 .lt. 0) go to 13190
     n4 = k + it2 - 1
     do kbr=k, n4
        if (n3 .gt. 0) go to 13150
        n5 = iabs(mbus(kbr))
        go to 13160
13150   n5 = - kbus(kbr)
13160   n6 = n1 + kbr - k
        d1 = 0.0
        do j=1, it2
           d1 = d1 + volt(j) * qfd(n6)
           n6 = n6 + it2
13170   end do
        if (iprsup .gt. 5) write (lunit6, 13175) n3, n5, kbr, f(n5), d1
13175   format(17h at 13170.  n3 = ,i5,5x,5hn5 = ,i5,5x,6hkbr = ,i5,5x,8hf(n5) = ,e15.7,5x,5hd1 = ,e15.7   )
        f(n5) = f(n5) + d1
13180 end do
13190 if (n3 .gt. 0) go to 13010
13200 end do
  it2 = 1
13210 if (cki(k) .lt. 0.0) go to 640
  k = k + 1
  go to 13210
  780 if( num99 .le. 0 )  go to 2801
  do i=1, inonl
     if ( nltype(i) .gt. 0 )  go to 2793
     if ( nltype(i)  .eq.  -98   .or. nltype(i)  .eq.  -96 )   go to 2781
     if( anonl(i) .ge. 0.0 )  go to 2793
2781 k = nonlk(i)
     m = iabs( nonlm(i) )
     if( k .ne. l )  go to 2784
     n4 = m
     go to 2786
2784 if( m .ne. l )  go to 2793
     n4 = k
2786 if ( nltype(i)  .ne.  -96 )  go to 2787
     n3 = nonlad(i) + 1
     yx = gslope(n3)
     f(l) = f(l) + yx
     f(n4) = f(n4) - yx
     if ( iprsup  .ge.  2) write ( lunit6, 2790) i, k, m, l, n4, yx, f(l), f(n4)
2790 format ( /,  31h type-96 g added to row of y.  , 5i10, /, 3e25.15)
     go to 2793
2787 n3 = nonlad(i)
     yx = gslope(n3)
     f(l) = f(l) + yx
     f(n4) = f(n4) - yx
     if( iprsup .ge. 2 ) write(lunit6, 2789)  i, k, m, l, n4, yx, f(l), f(n4)
2789 format( /, 31h type-98 g added to row of (y).  , 5i10, /, 3e25.15 )
2793 end do
2801 kks(l) = kss
  if ( f(l) .ne. 0.0 )  go to 4308
  if ( kconst  .le. 1 )   go to 2813
  do i=2, kconst
     if ( iform(i)  .ne.  18 )   go to 2808
     n6 = time1(i)
     if ( n6  .eq.  l )   go to 4308
2808 end do
  !     connect high resistance to ground for nonzero diagonal:
2813 f(l) = epsiln
  write (lunit6, 4108)  bus(l), epsiln
4108 format (   8h node  ",  a6,  19h"  has no connected,39h linear branches.   add (to ground) g =,e13.4  ,/, 1x  )
4308 if( num99 .le. 0 )  go to 73620
  do i=1, inonl
     if( nltype(i) .gt. 0 )  go to 73610
     k = nonlk(i)
     m = iabs( nonlm(i) )
     if( k .ne. l )  go to 73600
     if( f(m) .eq. 0.0 )  f(m) = 1.0 / fltinf
     go to 73610
73600 if( m .ne. l )  go to 73610
     if( f(k) .eq. 0.0 )  f(k) = 1.0 / fltinf
73610 end do
73620 if ( iprsup  .ge.  4 ) write (lunit6, 783)  bus(l),  ( f(i), i=1, ntot )
783 format (   8h node  ",  a6,  18h"  row of (y) ....,/,  ( 1x,  5e25.15 ) )
  do i=2, ntot
     if ( f(i)  .eq.  0.0 ) go to 885
     kss = kss - 1
     km(kss) = i
     ykm(kss) = f(i)
885 end do
  km(kss) = -km(kss)
  if( iprsup .gt. 0 ) write(lunit6, 73647)  l, kks(l), kss,     inonl, num99
73647 format( 40h end of storage of row of (y), at 73647.   , 5i10 )
  if ( kss  .gt.  ntot )   go to 894
  write (lunit6, 892)  l
892 format ( 35h before any factoring,  (y) storage, 38h has almost overflowed list 5 storage., 35h   only ntot cells left at row  l =,  i4  )
  kill = 1
  lstat(16) = 5
  lstat(14) = l
  lstat(13) = 1
  lstat(19) = 894
  go to 9900
894 if ( l  .lt.  kpartb )   go to 600
  kks(1) = kss
  call move0 ( kpsour(1), ntot )
  l = 1
  mp = 1000
202 if ( mp .gt. ntot )  go to 1000
  mp = mp + 1000
  go to 202
1000 l = l + 1
  m = l + mp
  j = kks(l)
2000 j = j - 1
  i = iabs( km(j) )
  k = kpsour(i)
  if ( k .ne. 0  .and.  k .lt. m )  m = k
  if ( km(j)  .gt.  0 ) go to 2000
  j = kks(l)
3000 j = j - 1
  i = iabs ( km(j) )
  if ( i .gt. kpartb )  go to 5000
  if ( kpsour(i) .eq. 0 )  kpsour(i) = m
  if ( kpsour(i) .eq. m )  go to 5000
  np = kpsour(i)
  do mm = 2, kpartb
4000 if ( kpsour(mm) .eq. np ) kpsour(mm) = m
  end do
5000 if ( km(j) .gt. 0 )  go to 3000
  if ( l .lt. kpartb ) go to 1000
  do np = 1, inonl
     if ( nltype(np) .lt. 0 ) go to 6868
     mark = kpsour( nonlk(np) )
     if ( mark .eq. 0 ) go to 6868
     marm = kpsour( iabs(nonlm(np)) )
     if ( marm .eq. 0  .or.  marm .eq. mark ) go to 6868
     do nq = 2, ntot
6768    if ( kpsour(nq) .eq. marm ) kpsour(nq) = mark
     end do
6868 end do
  do mm = 1, kswtch
     knode = kpsour( kmswit(mm) )
     mnode = kpsour( kmswit(mm + lswtch) )
     if ( knode .eq. mnode ) go to 5800
     if ( knode .eq. 0  .or.  mnode .eq. 0 )  go to 5800
     do nn = 2, ntot
5700    if ( kpsour(nn) .eq. mnode ) kpsour(nn) = knode
     end do
5800 end do
  if( iprsup .ge. 1 ) write(lunit6, 903) kpartb,ntot,(kpsour(i),i=1,ntot)
903 format( /,  15h kpartb, ntot =, 2i10 ,/, 28h kpsour cells folow........ , /,  ( 1x, 20i6 )  )
  lstat(27) = kss
  if( iprsup .le. 2 )  go to 9900
  write (lunit6, 3744)  ( kks(i), i=1, kpartb )
3744 format ( /,  36h ( kks(i), i=1, kpartb ) follow .... ,/,  ( 1x,  20i6 )  )
  write (lunit6, 8193)  (i, km(i), ykm(i), i = kss, lymat)
8193 format ( /, 2( 5x, 1hi, 7x, 5hkm(i), 19x, 6hykm(i), 6x ),/,   ( i6, i12, e25.16,   2i12, e25.16 )  )
9900 if ( iprsup  .ge. 1 ) write (lunit6, 9906)
9906 format ( 16h exit  "last14".  )
  return
end subroutine last14
!
! subroutine qyqtr.
!
subroutine qyqtr(nphs,dy,q,y)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !      insert deck  labcom
  dimension dy(1)   ,q(1)    ,y(1)
  !
  n1=nphs*(nphs+1)/2
  do i=1,n1
     y(i)=0.d0
100 end do
  !
  do k=1,nphs
     km1=(k-1)*nphs
     dyk=dy(k)
     ij=0
     do j=1,nphs
        jk=km1+j
        qjk=q(jk)
        do i=1,j
           ij=ij+1
           ik=km1+i
           qik=q(ik)
           b=qik*qjk
           y(ij)=y(ij)+dyk*b
103     end do
102  end do
101 end do
  return
end subroutine qyqtr
!      subroutine qyqtr(nphs,dy,q,y)                                    2
!      implicit real*8 (a-h, o-z) ,
!     1      integer*4 (i-n)
!c      insert deck  labcom                                             3
!      dimension dy(1)   ,q(1)    ,y(1)                                 4
!c                                                                      5
!      n1=nphs*(nphs+1)/2                                               6
!      do 100 i=1,n1                                                    7
!      y(i)=0.d0                                                        8
!100   continue                                                         9
!c                                                                      0
!      do 101 k=1,nphs                                                  1
!      km1=(k-1)*nphs                                                   2
!      dyk=dy(k)                                                        3
!      ij=0                                                             4
!      do 102 j=1,nphs                                                  5
!      jk=km1+j                                                         6
!      qjk=q(jk)                                                        7
!      do 103 i=1,j                                                     8
!      ij=ij+1                                                          9
!      ik=km1+i                                                         0
!      qik=q(ik)                                                        1
!      b=qik*qjk                                                        2
!      y(ij)=y(ij)+dyk*b                                                3
!103   continue                                                         4
!102   continue                                                         5
!101   continue                                                         6
!      return                                                           7
!      end
!
! subroutine breqiv.
!
subroutine breqiv( ikf, isfd, ibf )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  dimension  ur(40)
  !     this routine produces companion model for each branch. it also
  !     initializes the current injections for the branches  *   *   *   *
  idk = ikf * 2
  ikf = ikf + 1
  isc = ibf + 1
  isf = isfd + 1
  if( iprsup .gt. 0 ) write( lunit6, 1 ) ikf, isfd, ibf, imfd(idk+1), imfd(idk+2)
1 format(41h integer counters at start of breqiv.....,7x,3hikf,6x,4hisfd,7x,3hibf,6x,4hizfd,6x,4hipfd,/,41x,5i10)
  !     calculate modal voltages from phase values   *   *   *   *   *   *
  cz =  it2
  cz = 1.0 / cz
  ur(1) = volt(1)
  do ka = 2, it2
     ur(1) = ur(1) + volt(ka)
2    ur(ka) = ( volt(1) - volt(ka) ) * cz
  end do
  ar = ur(1) * cz
  if( iprsup .gt. 0 ) write( lunit6, 3 ) ar, ( ur(ka), ka = 2, it2 )
3 format(25h modal voltages in breqiv,/,(2x,6e21.12))
  !     process 'zero' sequence information first    *   *   *   *   *   *
  ist = isfd + 1
  isk = imfd(idk+1)
  isfd = isfd + isk * 5
  isv = ibf - 2
  do ka = ist, isfd, 5
     isv = isv + 3
     al1 = rmfd(ka+1) / delta2
     ar1 = rmfd(ka)
     ac1 = rmfd(ka+2)
     arl = rmfd(ka+3)
     if( ac1 .gt. 0 ) ac1 = delta2 / ac1
     a2 = 0.
     a1 = al1
     if( al1 .eq. 0. .or. arl .eq. 0. ) go to 4
     a1 = ( al1 * arl ) / ( arl + al1 )
     a2 = 2. * a1 / arl
4    azr = 1. / ( ar1 + a1 + ac1 )
     azi = ( ar1 - a1 + ac1 ) * azr
     cikfd(isv) = -cikfd(isv+1) + azr * ar
     !     store constants for later use in the time-step loop  *   *   *   *
     rmfd(ka+1) = azr
     rmfd(ka+2) = ac1
     rmfd(ka+3) = azi
     rmfd(ka+4) = a2
5 end do
  !     process the remaining modes  *   *   *   *   *   *   *   *   *   *
  ibf = isv + 2
  ist = isfd + 1
  isk = imfd(idk+2)
  isu = isfd + isk * 5
  isk = isk * 3
  !     loop across all 'positive' sequence branches *   *   *   *   *   *
  do ka = ist, isu, 5
     al1 = rmfd(ka+1) / delta2
     ar1 = rmfd(ka)
     arl = rmfd(ka+3)
     ac1 = rmfd(ka+2)
     if( ac1 .gt. 0. ) ac1 = delta2 / ac1
     a2 = 0.
     a1 = al1
     if( arl .eq. 0. .or. al1 .eq. 0. ) go to 6
     a1 = ( al1 * arl ) / ( al1 + arl )
     a2 = 2. * a1 / arl
6    azr = 1. / ( ar1 + a1 + ac1 )
     azi = ( ar1 - a1 + ac1 ) * azr
     !     internal loop across the remaining ( it2 - 1 ) modes *   *   *   *
     isv = isv + 3
     isw = isv - isk
     do kb = 2, it2
        isw = isw + isk
7       cikfd(isw) = -cikfd(isw+1) + azr * ur(kb)
     end do
     !     store constants for later use in the time-step loop  *   *   *   *
     rmfd(ka+1) = azr
     rmfd(ka+3) = azi
     rmfd(ka+2) = ac1
     rmfd(ka+4) = a2
8 end do
  ibf = ibf + ( it2 - 1 ) * isk
  isfd = isu
  if( iprsup .lt. 1 ) go to 15
  write( lunit6, 9 ) ikf
9 format(26h arrays for branch set no.,i6,18h  at end of breqiv)
  write( lunit6, 10 ) isf, isfd, ( rmfd(ka), ka = isf, isfd )
10 format(16h array rmfd from,i6,4h  to,i6,/,(2x,6e21.11))
  write( lunit6, 11 ) isc, ibf, ( cikfd(ka), ka = isc, ibf )
11 format(17h array cikfd from,i6,4h  to,i6,/,(2x,6e21.11))
15 return
end subroutine breqiv
!
! subroutine past.
!
subroutine past
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     this module is used by brandwajn (type-59) s.m. model
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'tacsar.ftn'
  include 'synmac.ftn'
  !     initialization after phasor solution for type-59 s.m.
  !     this routine moves the initial conditions to time '-deltat'
  ibr = ibrold
  ilk = 0
  in = 1
  nwd = 24
  iht = 0
  ivk = 0
  i26 = 0
  j30 = 1
  i30 = 1
  j75 = 27
  ako = 1.0 + damrat
  delta6 = 6.0 * deltat
  do k = 1, numsm
     n1 = ismdat( j30+2 )
     n2 = ismdat( j30+5 )
     v1 = e(n1) - e(n2)
     n1 = ismdat( j30+3 )
     n2 = ismdat( j30+6 )
     v2 = e(n1) - e(n2)
     n1 = ismdat( j30+4 )
     n2 = ismdat( j30+7 )
     v3 = e(n1) - e(n2)
     cv0 = ( v1 + v2 + v3 ) * asqrt3
     im = ismdat( j30 )
     !     retrieve and store the elements of matrix /ykm/ pertaining to *
     !     the synchronous machines connected to nodes nodsma(k),nodsmb(k),**
     !     and nodsmc(k) ****************************************************
     idk = j75 + 54
     jd30 = j30 + 20
     je30 = j30 + 1
     do jdk = 1, 3
        k31 = je30 + jdk
        n1 = ismdat( k31 )
        n3 = kks( n1 )
        kbk = jdk + 6
        k31 = je30
        do kp = jdk, kbk, 3
           k31 = k31 + 1
           n1 = ismdat( k31 )
           n4 = n3 - 1
12         if( iabs ( km(n4) )  .ne.  n1 )   go to 14
           elp( idk+kp ) = ykm( n4 )
           n5 = kp + jd30
           ismdat( n5 ) = n4
           go  to  15
14         n4 = n4 - 1
           go  to  12
15      end do
20   end do
     ap1 = 0.0
     ap2 = 0.0
     idelta = ismdat( j30+1 )
800  ilk = ilk + 1
     i75 = i26 + 27
     c1 = cu( in+11 )
     c2 = cu( in+12 )
     c3 = cu( in+13 )
     a1 = elp( i75 )
     a2 = elp(i75+1)
     !     calculate branch history terms cik ******************************
     cik( ibr+1 ) = -c1 - a1*v1 - a2*(v2+v3)
     cik( ibr+2 ) = -c2 - a1*v2 - a2*(v1+v3)
     cik( ibr+3 ) = -c3 - a1*v3 - a2*(v1+v2)
     ibr = ibr + 3
     if( iprsup  .lt.  2  )    go  to  100
     izu = ibr - 2
     write( lunit6, 6600 )   ( cik(izn), izn = izu, ibr )
6600 format( 1x, 18hcurrent injection , 3e21.12 )
     !     calculate history terms for rotor circuits * * * * * * * * * * * *
     !     these calculations use the currents from time t = 0.0 ***********
100  a1 = cu(  in  )
     a2 = cu( in+1 )
     a3 = cu( in+3 )
     a4 = cu( in+4 )
     a5 = cu( in+5 )
     a6 = cu( in+6 )
     !     history terms for rotor circuits*********************************
     ih = i75 + 17
     cu( in+3 ) = elp(ih+6) * a1 + (elp(ih+7) + ako * elp(i26+7)) * a3  + elp(ih+8) * a4
     cu( in+4 ) = elp(ih+9) * a1 + elp(ih+10) * a3 + ( elp(ih+11) +  ako * elp(i26+8) ) * a4
     cu( in+11 ) = 1.0
     cu( in+5 ) = elp(ih+12) * a2 + ( elp(ih+13) + ako * elp(i26+15) ) * a5 + elp(ih+14) * a6
     cu( in+6 ) = elp(ih+15) * a2 + elp(ih+16) * a5 + ( elp(ih+17) + ako * elp(i26+16) ) * a6
     cu( in+12 ) = cu( in+7 )
     cu( in+13 ) = cu( in+8 )
     !     calculate history terms for stator circuits**********************
     cu( in+2 ) = cv0 + cu( in+2 ) / elp( i26+17 )
     afd = cu( in+14 ) * omega
     afq = cu( in+16 ) * omega
     !     not finished*****************************************************
     cu(  in  ) = cu( in+7 ) + a1 / elp(i75+39) + afq
     cu( in+1 ) = cu( in+8 ) + a2 * elp(i75+41) - afd
     if( iprsup .lt. 1 )  go  to  150
     ikm = in + nwd - 1
     write( lunit6, 6601 )  ilk, (cu(ij), ij = in, ikm )
6601 format ( /, 40h current history terms for generator no., i5 , /, ( 1x,  7e17.8 )  )
150  a5 = elp( i75 )
     a6 = elp(i75+1)
     if( idelta .eq. 0 )  go  to  160
     a11 = ( a5 - a6 ) / 3.0
     a5 = a11 + a11
     a6 = -a11
160  ap1 = ap1 + a5
     ap2 = ap2 + a6
     !     back-off the mechanical equations to t = -deltat   * * * * * * * *
     !     angle = angle - ( deltat * speed ) / 2.0    speed = torque - y *
     !     speed    * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     cz = elp( i26+26 )
     numask = ismdat( i30+11 )
     num2 = numask + numask
     num4 = num2 + num2
     nlocg = ismdat( i30+12 )
     nloce = ismdat( i30+13 )
     n1 = iht + numask
     n2 = n1 + num2
     n22 = ivk + num2
     m22 = n22
     m26 = iht + num4
     do ka = 1, numask
        kb = n1 + ka
        kd = n2 + ka
        ke = m26 + ka
        kc = ke + numask
        m22 = m22 + 1
        shp( m22 ) =  histq( kb )
        histq( ke ) = - shp( m22 )
21      histq( kd ) = histq( kc ) / histq( kb )
     end do
     kb = n2 + nlocg
     cd = ( ( afd * a2  -  afq * a1 ) * tenm6 ) / omega
     histq( kb ) = histq( kb ) - cd * cz
     if( nloce .eq. 0 )  go  to  22
     kb = n2 + nloce
     nloce1 = n22 + nloce
     cexc = - cu( in+10 ) * a3 * tenm6
     histq( kb ) = histq( kb ) - cexc / shp( nloce1 )
22   do ka = 1, numask
        kd = iht + ka
        kb = kd + num2
        kc = m26 + ka
23      histq( kb ) = histq( kd ) + delta2 * histq( kc )
     end do
     kv = ivk + 1
     kw = n2 + 1
     ku = m26 + 1
     call banmul( shp(kv), histq(ku), histq(kw), numask )
     kl = ivk + num2
     if( iprsup .gt. 1  ) write( lunit6, 6602 )   kv, kl, ( shp(ii), ii = kv, kl )
6602 format( 1x,  13hmatrix y from,  i6,  2x,  2hto,  i6, /,  ( 2x,  6e20.11 ) )
     !     triangularize an appropriate part of the matrix  'y' ************
     call bandel( shp( kv ), numask )
     ivk = kl + 5 * num2
     ka = iht + 1
     iht = iht + 3 * num2
     if( iprsup .lt. 1 )   go  to  200
     write( lunit6, 6603 )  ka, kb, ( histq( kc ), kc = ka, iht )
6603 format( 18h  array histq from, i5, 4h  to, i5, /, (2x,6e20.11) )
     write( lunit6, 6602 )  kv, kl, ( shp(ii), ii = kv, kl )
     kl = kl + 3 * num2
     kv = kl + 1
     kl = kl + 2 * num2
     write( lunit6, 6602 )   kv, kl, ( shp(ii), ii = kv, kl )
200  im = im - 1
     in = in + nwd
     i26 = i26 + 101
     i30 = i30 + 30
     if( im .gt. 0 ) go to 800
     !     calculate and store the constant off-set terms for updating of the
     !     matrix (ykm). these terms will be used only for saturable . ******
     !     synchronous machines *********************************************
     elp( idk+1 ) = elp( idk+1 ) - ap1
     elp( idk+5 ) = elp( idk+5 ) - ap1
     elp( idk+9 ) = elp( idk+9 ) - ap1
     elp( idk+2 ) = elp( idk+2 ) - ap2
     elp( idk+3 ) = elp( idk+3 ) - ap2
     elp( idk+4 ) = elp( idk+4 ) - ap2
     elp( idk+6 ) = elp( idk+6 ) - ap2
     elp( idk+7 ) = elp( idk+7 ) - ap2
     elp( idk+8 ) = elp( idk+8 ) - ap2
     if( iprsup .le. 0 )    go  to   899
     idv = idk + 9
     jdk = idk + 1
     write( lunit6, 6604 )    ( elp( iu ),  iu = jdk,idv )
6604 format( 8h at 900., ( /, 6e20.12 ) )
     idv = jd30 + 9
     jdk = jd30 + 1
     write( lunit6, 6605 )  ( ismdat(iu), iu = jdk ,idv )
6605 format( 8h at 900., /, 9i12, / )
899  j30 = j30 + 30
     j75 = j75 + 101
900 end do
  if( iprsup .lt. 2 )   go  to  2500
  ibk = 0
  do i = 1, ilk
     ibl = ibk + 1
     ibk = ibk + 101
     write( lunit6, 6606 )  ( elp( ik ), ik = ibl, ibk )
6606 format( /, ( 1x, 6e20.12 ) )
2400 end do
2500 return
end subroutine past
!
!     subroutine banmul.
!
subroutine banmul(ab, x, y, n)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     This module performs postmultiplication of a vector by a tri-
  !     diagonal matrix 'ab'. the results are added to the vector 'y'.
  dimension ab(1), x(2), y(1)
  !     initialization **************************************************
  s1 = 0.0
  i2 = 0
  d1 = x( 1 )
  if( n1 .eq. 1 )   go to 20
  n1 = n - 1
  !     start loop ******************************************************
  do i = 1, n1
     d3 = x(i + 1)
     i2 = i2 + 2
     d2 = ab( i2 )
     y(i) = y(i)  +  ab( i2-1 ) * d1  +  d2 * d3  +  s1
     s1 = d2 * d1
     d1 = d3
10 end do
  !     finish last row operations **************************************
20 y( n ) = y( n ) + ab( i2+1 ) * d1 + s1
  return
end subroutine banmul
!
! subroutine bandel.
!
subroutine bandel( ab, n )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Triangularization of symmetric tridiagonal matrix 'ab' stored as
  !     a one vector containing diagonals and lower (upper) off-diagonals
  !     each diagonal is followed by an apropriate off-diagonal . There
  !     are altogether  2 * n  entries in the vector 'ab'.
  dimension  ab( 1 )
  !     initialization *************************************************
  n2 = n + n
  i2 = 2
  d = 1.0 / ab( 1 )
  !     start loop over remaining entries ******************************
10 ab( i2-1 )  =  d
  e = ab( i2 )
  d = d * e
  ab( i2 )  =  d
  i2 = i2 + 2
  if( i2  .gt.  n2 )   go to  20
  d = 1.0 / ( ab( i2-1 ) -  d * e )
  go to 10
20 return
end subroutine bandel
!
!     end of file: over14.for
!
