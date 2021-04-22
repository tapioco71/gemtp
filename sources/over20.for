!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over20.for
!
!
!     subroutine over20.
!
subroutine over20
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  equivalence (moncar(1), knt), (moncar(2), kbase)
  equivalence (moncar(3), ltdelt), (moncar(4), isw)
  equivalence (moncar(10), mtape)
  if (iprsup .ge. 1) write ( lunit6, 4567 )
4567 format ( 23h begin module "over20".  )
  zero = 0.0
  ll1 = 1
  ll9  =  9
  ll10 = 10
  call runtym(d1,d2)
  n10 = 1
  if (iprsup .ge. 1) write (lunit6, 1324)    iplot, nenerg, kbase, m4plot,  mtape, icat, lstat(32), t, aincr
1324 format ( /,  1x,   23h   iplot  nenerg  kbase, 32h  m4plot   mtape    icat lstat32, 14x, 1ht, 10x, 5haincr  ,/,  1x, 7i8, 2e15.5  )
  if ( max99m .lt. 0 ) write (lunit6, 9345)  max99m
9345 format (  ' ++++  number of suppressed type-99 flashover', ' or clearing messages is negative of',  i7  )
  if ( peaknd(1)  .eq.  0.0 )   go to 5019
  n6 = peaknd(3)
  write (lunit6, 5011)  peaknd(1), peaknd(2), bus(n6)
5011 format ( 8x,  38hoverall simulation peak node voltage =, e15.6,      17h .   time (sec) =, e14.5,      12h .   bus = ',   a6,   3h' .    )
5019 k = lstat(32) + 1
  if( iplot .lt. 0 )  go to 8005
  volti(1) = -9999.
  if ( m4plot  .eq.  0 )   go to 5022
  call pltfil ( k )
  go to 8005
5022 write (lunit4)  ( volti(i), i=1, k )
8005 if ( icat  .gt.  0     .or.      memsav  .gt.  0 ) call katalg
  close (unit=79)
  k = lstat(32)
  !      both max and min will be printed for each energization when
  !      'aincr' is set to be greater than or equal to '55.0'.
  if ( kbase .eq. 2  .and. aincr .lt. 55.0) go to 3614
  if ( begmax(1)  .le.  0.0 )   go to 3614
  write (lunit6, 8002)   ( xmax(l), l=1, k )
8002 format (/, 106h maxima and minima which occurred during the simulation follow.   the order and column positioning are the     ,/, &
       49h same as for the regular printed output vs. time.      ,/,18h variable maxima :   ,/,  ( 15x, 9e13.6 )  )
  !     extrema vector  "xmax"  actually has four partitions,
  !     each the size of list 12:  (xmax, tmax, xmin, tmin) .
  ndx1 = lsiz12 + 1
  ndx2 = lsiz12 + k
  write (lunit6, 8003)  ( xmax(l), l=ndx1, ndx2 )
8003 format ( 18h times of maxima :  ,/,  ( 15x, 9e13.6 )  )
  ndx1 = ndx1 + lsiz12
  ndx2 = ndx2 + lsiz12
  write (lunit6, 8004)   ( xmax(l), l=ndx1, ndx2 )
8004 format ( 18h variable minima :   ,/,  ( 15x, 9e13.6 )  )
  ndx1 = ndx1 + lsiz12
  ndx2 = ndx2 + lsiz12
  write (lunit6, 18005)  ( xmax(l), l=ndx1, ndx2 )
18005 format ( 18h times of minima :  ,/,  ( 15x, 9e13.6 )  )
3614 if (nenerg .ne. 0)   go to 605
  flstat(9) = d1
  flstat(10) = d2
  if ( begmax(1)  .ne.  0.0   .and. begmax(2)  .ne.  0.0 ) write (lunit6, 8006)  begmax(2)
8006 format (  56x,  32hsearch for extrema began at time, e15.6,   7h   sec.    )
  write (lunit6, 8007)
8007 format ( 1x )
  if ( ipunch  .le.  0 )   go to 9800
5759 kcount = 2
  iold=3
  if ( n10  .ne.  0 ) write (lunit6, 7009)  t
7009 format ( /,  74h printout of the saving of terminal conditions for all components, at time,    e15.7,     11h   seconds.       ,/, &
       30h begin with all node voltages.                          ,/, 3( 21x,  4hnode )    ,/,  19x,  6hnumber,  21x,  4hname, &
       18x,  7hvoltage      )
  do k = 2, ntot
     if (n10 .ne. 0) write(lunit6,7012) bus(k), e(k), zero, k
7012 format( 1h , a6, 2e13.5, 41x, i6 )
7010 write(lunit7, 7011) kcount, bus(k), e(k), zero, zero, k
  end do
7011 format( i2, a6, 3e15.8, 21x, i6 )
  if (n10 .ne. 0) write (lunit6, 7013)
7013 format ( /,  " linear branch table state variables ( 'currents' ) follow.",/, &
       7x,  3hrow,  9x,  11h'from' node,  11x,  9h'to' node,  9x, &
       'linear branch parameters, identified by the fortran vector storage' ,/,  9x,  1hi,  8x,  12hbus(kbus(i)),  8x, &
       12hbus(mbus(i)),  19x,  6hcik(i),  20x,  5hci(i), 20x,  5hck(i)    )
  k=1
7014 it2=1
  if(kbus(k).lt.0) go to 7024
  if(nr(k).ge.0) go to 7022
  ci1=ci(k)
  ck1=ck(k)
  if(ci1.eq.0. .and. ck1.eq.0.) go to 7025
  l=kbus(k)
  m=iabs(mbus(k))
  write(lunit7,7020) iold, bus(l), bus(m), ci1, ck1, zero, zero, k
7020 format( i2, 2a6, 4e15.8, i6)
  if( n10 .ne. 0 ) write(lunit6,7021) bus(l), bus(m), ci1, ck1, zero, zero, k
7021 format( 1h , a6, 1x, a6, 4e13.5, i6 )
  go to 7025
7022 it2=iabs(length(k))
  it1=k+it2-1
  do i=k,it1
     n1=kbus(i)
     n2=iabs(mbus(i))
     if ( n10  .ne.  0 ) write(lunit6,7021) bus(n1),bus(n2), cik(i), ci(i), ck(i), zero,i
7023 write(lunit7,7020) iold, bus(n1), bus(n2), cik(i), ci(i), ck(i), zero, i
  end do
  go to 7025
7024 n1 = iabsz( kbus(k) )
  n2 = iabsz( mbus(k) )
  write( lunit6, 70241 ) bus(n1), bus(n2)
70241 format ( 42h  ****  warning.   ****   no currents will, 36h be punched for the distributed line, &
       20h connecting nodes  ",  a6,  9h"  and  ", a6,  3h" .   )
  it2 = length(k)
  if( it2 .lt. 0 ) it2 = -it2
  if( length(k) .gt. 0 ) it2 = 1
  if( kodsem( k ) .ne. 0  .and. imodel( k ) .ne. 2 ) it2 = iabsz( kodebr(k) )
7025 k=k+it2
  if(k.le.ibr) go to 7014
  if (inonl .eq. 0) go to 9207
  if ( n10  .ne.  0 ) write (lunit6, 4688)
4688 format ( /, 48h nonlinear element table state variables follow. )
  iold=4
  do k=1,inonl
     if(nonle(k).ge.0) go to 7035
     n1=nonlk(k)
     n2=iabs(nonlm(k))
     if ( nltype(k)  .ne.  -96 )  go to 7031
     n5 = nonlad(k)
     n6 = n5 + 1
     n7 = n5 + 2
     n8 = n5 + 3
     write(lunit7, 7020) iold, bus(n1), bus(n2), vchar(n7), cchar(n8)
     n9 = cchar(n6)
     n11 = cchar(n7)
     write (lunit7, 7018)  n9, n11,  ( vchar(ipp), ipp=n5, n8 )
7018 format ( 2i10, 4e15.8)
     n12 = n5 + 4
     n13 = n5 + 5
     n14 = cchar(n12)
     write(lunit7,7026) n14, vchar(n12), vchar(n13), gslope(n12), gslope(n13)
7026 format ( i10 , 4e15.8 )
     if ( n10  .eq.  0)  go to 7035
     write ( lunit6, 7021) k, bus(n1), bus(n2), vchar(n7), cchar(n8)
     write ( lunit6, 7019)  n9, n11,  ( vchar(ipp), ipp=n5, n8 )
7019 format ( 1h ,i10, 2x, i10, 4( 3x, e15.8) )
     write(lunit6,7027)  n14, vchar(n13), vchar(n14), gslope(n12), gslope(n13)
7027 format ( 1h , i10 , 4(3x,e15.8) )
     go to 7035
7031 a = curr(k)
     if(a.eq.0.) go to 7035
     write(lunit7,7020) iold,bus(n1),bus(n2),a
     if ( n10  .ne.  0 ) write (lunit6, 7021)  bus(n1), bus(n2), a
7035 end do
9207 if (kswtch .eq. 0) go to 4020
  if ( n10  .ne.  0 ) write (lunit6, 7348)
7348 format ( /,  38h status variables for switches follow.       ,/, &
       1x,  6hbus(l),  1x,  6hbus(m),  3x,  7hkpos(k),  2x, &
       8h kode(l),  2x,  8h kode(m),  14x,  6htclose,  14x, &
       6hadelay,  14x,  6henergy,  16x,  4hcrit   )
  do k=1, kswtch
     l = iabs(kmswit(k))
     kn1 = lswtch + k
     m = iabs(kmswit(kn1))
     npl = nextsw(k)
     if ( npl  .ne.  0 )  npl = 87
     write (lunit7,4003) bus(l), bus(m), kpos(k), kode(l), kode(m), tclose(k), adelay(k), energy(k), crit(k), npl
4003 format(2h 5,2a6,3i4,4e13.6 ,/, 14x, i4)
     if ( n10  .eq.  0 )   go to 4010
     write (lunit6,4004) bus(l), bus(m), kpos(k), kode(l), kode(m), tclose(k), adelay(k), energy(k), crit(k), nextsw(k)
4004 format(1x,a6,1x,a6,3i10,4e20.10 ,/, 14x, i10)
4010 end do
4020 go to 9800
  !     code below is just for  'statistics'  or  'systematic'  runs. ----
605 nstat = k
  do 8010  l=1, nstat
     ndx1 = 2 * lsiz12  +  l
     if ( -xmax(ndx1)  .le.  xmax(l) )   go to 8010
     xmax(l) = xmax(ndx1)
     ndx3 = lsiz12 + l
     ndx2 = ndx1 + lsiz12
     xmax(ndx3) = xmax(ndx2)
8010 end do
  if (iprsup .gt. 0) write (lunit6,8903) isw, nstat, knt, nenerg, (xmax(i), i=1, nstat)
8903 format(27h isw, nstat, knt, nenerg = ,4i10,/, 25h (xmax(i), i=1, nstat) =   /  6(5x,e15.8))
  knt = knt + 1
  write (lunit9)  ( xmax(l), l=1, nstat )
  if ( begmax(1)  .eq.  0.0 )   go to 627
  if(nstat.le.9) go to 609
  if (nenerg .lt. 0)   go to 612
  if ( nswtpe .le. 1 )  go to 1610
  write (lunit6, 1510) angle, angtpe, ( xmax(l),l=1,ll9)
1510 format ( 1x, f6.2, 1x, f6.2, 1x, 9e13.6 )
  go to 611
1610 write (lunit6, 617)  angle,  ( xmax(l), l=1, ll9 )
611 write (lunit6, 618)  ( xmax(l), l=ll10, nstat )
  go to 624
612 write (lunit6, 618)  ( xmax(l), l=1, ll9 )
  go to 611
609 if (nenerg .lt. 0)   go to 614
  if ( nswtpe .le. 1 )  go to 1620
  write (lunit6, 1510) angle, angtpe, (xmax(l), l = 1, nstat)
  go to 624
1620 write (lunit6, 617)  angle, (xmax(l), l = 1, nstat)
  go to 624
614 write (lunit6, 618)   (xmax(l), l = 1, nstat)
617 format ( 1x, f9.4, 5x, 9e13.6 )
618 format ( 15x, 9e13.6 )
624 ndx1 = lsiz12 + 1
  ndx2 = lsiz12 + nstat
  if ( begmax(1)  .gt.  0.0 ) write (lunit6, 8003)  ( xmax(l), l=ndx1, ndx2 )
627 if ( knt  .gt.  iabs(nenerg) )   go to 610
  lastov = nchain
  nchain = 12
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
4568 format ( 15h exit "over20".   )
  go to 99999
610 write (lunit6, 654 )
654 format ( //,1x)
  lunit5 = mtape
  !     reset numdcd counter if it is not a omit base case
  if ( lstat(15) .ne. intinf )    numdcd = numdcd - ipntv(11)
  write (lunit6, 659 )
659 format ( 132h ----------------------------------------------------&
       ------------------------------------------------------------------&
       -------------   )
  write (lunit6, 616)  nenerg
616 format (  30h Simulation of all  'nenerg' =,  i4, 75h  energizations is now complete, and the emtp is ready to begin statistical ,/, &
       108h processing of the voltage solutions.   but before doing so, two cautions about discretization of continuous    ,/, &
       61h variables are probably appropriate to mention at this point.,/, 5x,  &
       109h1.  The switch closing times which are printed along with peak voltages of each energization are only desired   ,/, &
       9x, 113htimes, just as with all such times which are punched on data cards which define switches upon data input.   since   )
  write (lunit6, 644)
644 format(9x,112htime is discretized in multiples of time-step-size 'deltat' ,   actual closure will occur at the first discrete   ,/, &
       9x,  47hstep which does not precede the requested time.    ,/, 5x, &
       112h2.  In the tabulation of voltage distributions which follow, an arbitrary decision had to be made concerning the   ,/, &
       9x, 108hcolumn which is labeled  'frequency' .    The continuous variable voltage has been discretized, divided into   ,/, &
       9x, 114hcompartments of size  'aincr'  (read from columns 25-32 of the special statistics misc. data card).   for an entry   )
  write (lunit6, 648)
648 format ( 9x, 105hwhich is printed in a row marked as being for voltage  'vrow' ,   the associated compartment contains all  ,/, &
       9x,  73hvoltages  'v'  which satisfy       vrow  .le.  v  .lt.  (vrow + aincr)  .    ,/, &
       5x, 44h3.  floating-point counting (t = t + deltat), 45h is used to increment time.   Switching times, &
       28h which are an exact multiple    ,/,  9x, 47hof the time-step thus are ambiguous;  different, &
       43h computers may switch one step later, then.   )
  if ( ltdelt  .le.  0 )   go to 7314
  write (lunit6, 7305)
7305 format ( //,1x)
  write (lunit6, 7304)
7304 format ( 60h ***********************************************************  , 12h look, look , &
       60h************************************************************ )
  write (lunit6, 7309)  ltdelt
7309 format ( /,  ' During this run, a total of', i4,  '  random switch closings less than time zero were generated by the random  ',/, &
       ' number generator.   But the EMTP has no way of handling such closures.   All such illegal closing times were converted  ',/, &
       ' to time zero (they should show up in the printout that way) for simulation purposes.   The implications of this   ',/, &
       ' modification should be understood by the user.   If in any doubt, the user is strongly advised to seek experienced   ',/, &
       ' counsel on this subject. ', /, 1x)
  write (lunit6, 7304)
  write (lunit6, 7305)
7314 write (lunit6, 659 )
  flstat(9) = d1
  flstat(10) = d2
  lastov = nchain
  nchain = 29
  flstat(7) = flstat(7) + d1
  flstat(8) = flstat(8) + d2
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
  go to 99999
9800 if ( m4plot  .ne.  1 )   go to 9810
  if ( kbase .eq. 1 )  go to 9810
  !     vax simulator return to "over16" after table-saving:
  lastov = nchain
  nchain = 16
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
  go to 99999
9810 if ( lastov  .gt.  nchain )   go to 9850
  flstat(7) = flstat(7) + d1
  flstat(8) = flstat(8) + d2
9850 lastov = nchain
  nchain = 31
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
99999 return
end subroutine over20
!
!     subroutine katalg.
!
subroutine katalg
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     VAX-11/780  installation-dependent module which is used
  !     to honor the miscellaneous data card request for emtp table
  !     saving, for later  "start again"  usage.   A call to the
  !     universal module  "tables"  actually dumps memory onto disk.
  !     Logic associated with other than  memsav=0  or  memsav=1
  !     can generally be ignored by other systems;  it applies
  !     only to rtm (real time monitor) use of BPA VAX.
  include 'blkcom.ftn'
  include 'dekspy.ftn'
  character*132 ansi132
  common /comlock/ locker(2)   ! share with "over1" only
  if ( iprsup  .ge.  1 ) write (lunit6, 2467)  icat, memsav, lunit2, ltlabl
2467 format ( /,  18h enter  "katalg" ., 32h    icat  memsav  lunit2  ltlabl        ,/,  18x,  10i8  )
  if ( memsav .eq. 0 )  go to 9800  ! no table moving at all
  ! if interactive emtp control, then
  if ( m4plot .eq. 1 )  tmax = fltinf      ! make end-time of simulation infinite
  if ( memsav  .eq.  1016 )   go to 2634   ! "restore"   use
  write (lunit6, 2472)
2472 format ( /,  20x,  '----- "memsav = 1  represents', ' request for table dumping on disk.' )
  close (unit = lunit2)
  open (unit = lunit2, status = 'new', form = 'unformatted', file = 'tptables.bin' )
  call tables
  write (lunit2)  locker
  rewind 79
  do j=1, 9999
     read (79, 2474, end=2479)  ansi132
2474 format ( a132 )
2476 write (lunit2) ansi132
  end do
2479 close (unit = lunit2, status = 'keep' )
  karray(1) = indbuf
  karray(2) = mflush
  karray(3) = newvec
  write (lunit6, 2483)  ltlabl
2483 format (  26x,  'successful saving of emtp', ' tables as file  "tptables.bin" .', '    ltlabl  =',  i8  )
  if ( memsav .eq. 1 )  go to 9800  ! exit module (no spy)
  go to 9700     ! exit module after "emtspy" linkage set
  !     following code services  "restore"  of  "emtspy".  memory
  !     is restored from dumping of previous "sleep" or "save":
2634 write (lunit6, 2637)  ltlabl
2637 format (  ' begin emtp table restoration.  ltlabl =', i7,  ' .  wait for completion.'  )
  close (unit = lunit2)
  open (unit = lunit2, status = 'old', form = 'unformatted', file = 'tptables.bin')
  call tables
  close (unit = lunit2, status = 'keep')
  !     next, for rtm use, restore key "ovdrivkom.for" variables:
  indbuf = karray(1)
  mflush = karray(2)
  newvec = karray(3)
  write (lunit6, 2642)
2642 format (  ' restoration complete, user can now', ' begin  depositing  emtp  variables via  spy .' )
  memsav = 0     !  ?????????????  table changes done.
9700 kbreak = 1   ! lock flag for "emtspy" dialogue in "over16"
9800 return
end subroutine katalg
!
!     subroutine emtspy.
!
subroutine emtspy
  implicit real*8 (a-h, o-z),  integer*4 (i-n)
  !     Module of interactive EMTP only, which services "emtspy".
  !     If no interactive use, convert to dummy module ("return").
  include 'blkcom.ftn'
  include 'dekspy.ftn'
  include 'synmac.ftn'
  save
  data n18    / 0 /
  n18 = n18 + 1
  if ( n18 .lt. maxflg ) go to 9008
  n18 = 0
  if ( iprspy .lt. 9 )  go to 5611
  write (munit6, 5608)  kbreak, kwtspy, nbreak, lockbr, nchain, jjroll, t, tbreak
5608 format ( ' top "emtspy".  kbreak, kwtspy, nbreak, lockbr', ' jjroll, nchain =',  6i4,  '    t, tbreak =',  2e14.5  )
  call window
5611 if ( jjroll .gt. 0 )  go to 5632
  if ( kbreak .eq. 1 ) go to 5613
  if ( lockbr .eq. 1 ) go to 5623
  if ( t .lt. tbreak ) go to 5623
  if ( nchain .ne. nbreak ) go to 5623
  !     ok, now we service "break" of spy (time and place to do it):
  tbreak = 8877.e33
5613 lockbr = 1
  write (munit6, 5615)  nchain, t
5615 format ( '   // start "break" service in "emtspy".','   nchain =', i3,  '   t =', e14.5  )
  call window
  if ( kfile5 .eq. 1 ) go to 6258
5617 write (prom80, 5618)
5618 format ( ' spy:' )
  call prompt
5623 if ( ksmspy(3) .eq. 1 ) go to 5632
  call flager
  if ( kwtspy .eq. 1 )  go to 5632
  if ( kfile5 .eq. 1 )  go to 6258
  go to 9000
5632 kwtspy = 0
  call spying
  if ( jjroll .gt. 0 ) go to 9000
  if ( lockbr .eq. 1 )  go to 5623
  go to 9000
  !     entry point for pre-defined spy commands ($spy or "@"):
6258 if ( komadd .eq. 0 )  go to 6260
  komadd = komadd + 1
  buff77 = file6(komadd)
  if (buff77(1:3) .eq. 'eof') go to 6278
  go to 6264
6260 read (munit5, 6261, end=6274)  buff77
6261 format ( a80 )
6264 if ( kfile5 .eq. 1 ) call percnt ( buff77, 80 )
  if ( kilper  .ne.  0 )  go to 6274
6266 call spying
  if ( lockbr .eq. 1 )  go to 6258
  go to 9000
  !     end-of-file during disk read, so switch to keyboard input:
6274 close ( unit=munit5 )
6278 munit5 = muntsv(1)
  kfile5 = 0
  kilper = 0
  if ( muntsv(2) .ne. 2288 ) go to 5617
  muntsv(2) = 0
9000 if ( iprspy .lt. 1 )  go to 9008
  write (munit6, 9007)  kbreak, nchain, lastov, m4plot
9007 format (   ' exit "emtspy".  kbreak, nchain, lastov,', ' m4plot =',  4i6  )
  call window
9008 return
end subroutine emtspy
!
!     subroutine spying.
!
subroutine spying
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive EMTP only, which services "emtspy".
  !     This is the principle module, called by "emtspy".
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'synmac.ftn'
  include 'tacsar.ftn'
  include 'dekspy.ftn'
  character*8 chard7
  character*8 spytim(2), spdate(2)
  save
  if (iprspy .lt. 1) go to 31006
  write (munit6, 1006)  nchain, jjroll, kbreak, lockbr, nexmod, buff77(1:20)
1006 format ( ' enter "spying".  nchain, jjroll, kbreak,', ' lockbr, nexmod =',  5i5,  '    buff77(1:20) =', a20 )
  call window
  write (*,*)  ' top spying, ksmspy(1:3) =',  ksmspy
31006 if ( buff77(1:4) .ne. 'spy ' ) go to 51006
  nexmod = 0
  ksmspy(1) = 2
  ksmspy(3) = 0
51006 if ( nexmod .ne. 2 ) go to 1007
  nexmod = 0
  go to nextsn
1007 if ( nexmod .eq. 3 ) go to 8500
  if ( nexmod .eq. 1 ) go to 3208
  if ( nexmod .eq. 7 ) go to 1320
  if ( nexmod .ge. 4 ) go to 1319
  if ( jjroll .gt. 0 ) go to 1520
  if ( kbrser .ne. 2 )  go to 1009
  jword = 52
  go to 8500
1009 memkar = locint ( kar1(1) )
  if ( kfile5 .ne. 2 )  go to 1240
  kfile5 = 0
  go to 31269
1240 assign 31269 to nextsn
  write (prom80, 1260)
1260 format (  ' spy:'  )
51269 go to 9800
31269 answ80 = buff77
  if ( iprspy .le. 9 ) go to 39843
  write (munit6, 29843)  answ80
29843 format ( '  --- just read answ80:',  a80 )
  call window
39843 if ( answ80(1:4)  .eq.  'type'  )   go to 2506
  if (answ80(1:2) .ne. 'c ') go to 1275
  if ( kfile5  .eq.  1    .and.  kverfy  .eq.  0 ) go to 51269
  if ( icomm .ne. 0 )  go to 51269
  write (munit6, 1273)  answ80
1273 format ( ' comment:',  a80  )
  call window
  go to 51269
1275 if (answ80(1:1) .ne. '@')  go to 1289
  !     $$$$$$$  key word no. 19:  "@?"       $$$$  $$$$  $$$$  $$$$  $$$$
  !     now process user request for disk-file connection to keyboard (5):
  if ( kfile5 .ne. 1 ) go to 1278
  write (munit6, 1277)
1277 format (  '  === reject  "@"  usage;  one such', ' file is presently connected.  try again.'  )
  call window
  go to 1240
1278 if ( answ80(2:2) .ne. '/' ) go to 51278
  answ80(1:2) = '  '
  call frein1 ( answ80, komadd )
  komadd = komadd - 1
  n13 = 6
  go to 1276
51278 ansi32 = 'inclspy .dat                    '
  komadd = 0
  n26 = 2
  go to 2511
  !     ok, so file is legal;  next check for argument list;  extract any:m35.1747
1276 n6 = n13 + 1
  maxarg = 0
  do j=n6, 80
     if (answ80(j:j) .ne. ' ')  go to 4535
4531 end do
  go to 1271
4535 maxarg = 0
  do idmxxx=1, 10
     m = 0
     ansi8(1:8) = blan80(1:8)
     do l=1, 80
        if ( n6  .gt.  80 )   go to 4542
        if ( iprspy .lt. 1 )  go to 8900
        write (munit6, 8899)  l, m, n6, answ80(n6:n6)
8899    format ( ' next character. l, m, n6, answ80(n6) =', 3i5, 1x, a1 )
        call window
8900    if ( m  .eq.  0     .and.  answ80(n6:n6)  .eq.  '(' ) answ80(n6:n6) = ' '
        if ( answ80(n6:n6)  .eq.  ')' )   go to 4544
        if ( answ80(n6:n6)  .eq.  ' ' )   go to 4541
        if ( answ80(n6:n6)  .eq.  ',' )   go to 4544
        m = m + 1
        if ( m  .le.  8 )  go to 4540
        write (munit6, 4539)  idmxxx
4539    format (  '  ??? argument number',  i3, '  of  "@?"  has over 8 digits.  try again.'   )
        call window
        go to 1240
4540    ansi8(m:m) = answ80(n6:n6)
        if ( ansi8(m:m)  .eq.  '#' ) ansi8(m:m) = ' '
4541    n6 = n6 + 1
     end do
     !     no ")" on "@?"  argument list means we ran through column 80:
4542 if ( m  .eq.  0 )   go to 1271
     !     one or more non-blank characters since last "," is final argument:
     maxarg = maxarg + 1
     texpar(maxarg) = ansi8
     if ( iprspy .lt. 1 )  go to 1271
     write (munit6, 4543)  maxarg
4543 format ( ' maxarg =',  i8 )
     call window
     go to 1271
4544 maxarg = maxarg + 1
     read  (ansi8, 1360)  texpar(maxarg)
1360 format ( 15a1 )
     if ( answ80(n6:n6)  .eq.  ')' )   go to 1271
4549 n6 = n6 + 1
  end do
  write (munit6, 4551)
4551 format (  '   ? ? ?  illegal  "@?"  use.', '   over 10 arguments.   try again.'  )
  call window
  go to 1240
  !     any arguments have been successfully found;  now use file
1271 kfile5 = 1
  itexp = 0
  go to 51269
  !       former eof s.n. 1286 with file close and other resetting
1289 if ( answ80(1:8)  .ne.  blan80(1:8) )   go to 1293
  !     blank (just <cr>) represents request for repeat "examine"
  go to 1520
  !     check for  "%%%%%%%%"  field of  "@?"  arguments;  replace any:
  !     next check card image for one of the "numkey" key-words:
1293 n13 = 0
  if ( answ80(1:5) .ne. 'wake4' )  go to 1296
  n13 = 1
  answ80(5:5) = ' '
1296 do jword=1, numkey
     if ( answ80(1:8)  .eq.  spykwd(jword) )   go to 1307
1302 end do
  !     we reach here if illegal response to "spy:" prompt has been read:
1303 write (munit6, 1304)  answ80(1:8)
1304 format ( '   ? ? ?  illegal spy command  "',  a, '" .   try again  .....'   )
  call window
  go to 1240
1307 if ( iprspy .lt. 1 ) go to 1309
  write (munit6, 1308)  jword
1308 format ( '   keyword found is  jword =',  i8 )
  call window
1309 jwdsav = jword
  if ( jword .gt. 63 )  go to 71309
  go to ( &
                                !       heading  stop    plot    help  examine  deposit  switch  (1-7)
       1500,   9000,   1319,   1311,   1337,   1460,   8500, &
                                !       append   save   restore   go     echo    find    list    (8-14)
       1320,   2206,   2317,   2402,   3673,   2040,   2092, &
                                !         spy    break   when  comment    @?     roll   type?   (15-21)
       1240,   2124,   2430,   2486,   1303,   1424,   1303, &
                                !       verify   files   sleep  source   edit    wake  language (22-28)
       2494,   2563,   2591,   8500,   8500,   2604,   8500, &
                                !      catalog  begin    step    debug   data    ramp    time   (29-35)
       2907,   2926,   2937,   2964,   2974,   3095,   3153, &
                                !         tek   branch   yform    noy   factor    nof    rlc    (36-42)
       3174,   3256,   3357,   3381,   3394,   3406,   8500, &
                                !        width    bus    size   limit    iout    node   nonlin  (43-49)
       8500,   8500,   8500,   8500,   8500,   8500,   8500, &
                                !        space  lunit4  series   lock     [y]     [f]   noroll  (50-56)
       8500,   8500,   8500,   8500,   8500,   8500,   8500, &
                                !        open    close    sm     honk   choice   tacs    wait   (57-63)
       3568,   3584,   3606,   3623,   8500,   8500,   3644), jword
  call stoptp
71309 n14 = jword - 63
  go to ( &
                                !         v-i                                                   (64-64)
       8500 ), n14
  call stoptp
  !     $$$$$$$  key word no.  4:  "help"     $$$$  $$$$  $$$$  $$$$  $$$$
1311 write (munit6, 11312 )
11312 format (  '    available key-word responses to the  "spy:"', '  prompt are as follows:'  )
  call window
  do j=1, numkey, 8
     n15 = j + 7
     if ( n15 .gt. numkey )  n15 = numkey
     write (munit6, 1313)  (spykwd(k), k=j, n15)
1313 format ( 5x,  8a9 )
     call window
21313 end do
  jword = jwdsav
21314 write (prom80, 1314)
1314 format (  '       key-word of interest (all, <cr>,', ' top, bot, back):'  )
  assign 1315 to nextsn
  go to 9800
1315 spycd2 = buff77(1:35)
  if ( spycd2(1:4) .eq. '    ')   go to 11315
  if ( spycd2(1:4) .eq. 'end ' )  go to 1240
  if ( spycd2(1:4) .ne. 'bot ')  go to 51315
  jword = numkey
  go to 1317
51315 if ( spycd2(1:4) .ne. 'back' )  go to 61315
  jword = jword - 1
  if ( jword .gt. 0 )  go to 1317
  jword = numkey
  write (munit6, 31315)
31315 format ( '      //// wrap around, beginning to end ////' )
  call window
  go to 1317
61315 if ( spycd2(1:4) .eq. 'top ' )   go to 41315
  if ( spycd2(1:4) .ne. 'next' )   go to 21315
11315 jword = jword + 1
  if ( jword  .le.  numkey )  go to 1317
  if ( spycd2(1:4) .eq. 'all ' )  go to 21314
  write (munit6, 81315)
81315 format ( '      //// wrap around, end to beginning ////' )
  call window
41315 jword = 1
  go to 1317
21315 jword = 1
  if ( spycd2(1:4) .eq. 'all ' )     go to 1317
  do jword=1, numkey
     if ( spycd2(1:8)  .eq.  spykwd(jword) )    go to 1317
1316 end do
  write (munit6, 1760)  spycd2
  call window
  go to 1318
1317 n17 = kbegtx(jword)
  n18 = kbegtx(jword+1) - 1
  do j=n17, n18
     munit6(1:81) = ' '//texspy(j)
     call window
91315 end do
  if ( jword .ne. 4 )  go to 61317
  write (munit6, 11312)
  call window
  do j=1, numkey, 8
     write (munit6, 1313) ( spykwd(k), k=j, j+7)
     call window
51317 end do
61317 jwdsav = jword
  if ( spycd2(1:4) .ne. 'all ' )  go to 1318
  call quiter
  if ( kwtspy .eq. 0 ) go to 11315
  kwtspy = 0
1318 go to 21314
  !     $$$$$$$  key word no.  3:  "plot"     $$$$  $$$$  $$$$  $$$$  $$$$
1319 call rtmplt
  if ( nexmod .ge. 4 ) go to 9803
  go to 1240
  !     $$$$$$$  key word no.  8:  "append"   $$$$  $$$$  $$$$  $$$$  $$$$
1320 call append
  if ( nexmod .eq. 7 ) go to 9803
  go to 1240
  !     $$$$$$$  key word no.  5:  "examine"  $$$$  $$$$  $$$$  $$$$  $$$$
1337 numex = 0
  khead = 2
  heding = ' '
  go to 1460
  !     encode heading buffer with variable request now known:
1340 numex = numex + 1
  intout(numex) = intype
  if ( nchd2  .lt.  6 ) spycd2(nchd2+1:6) = blan80(nchd2+1:6)
  n14 = n1
1354 if ( n14  .gt.  n2 )   go to 1460
  heding(khead:khead+14) = blan80(khead:khead+14)
  !     reals or vectors occupy 15 cols, so can be shifted:
  if ( intype .eq. 0  .or.  ivec(ind) .eq. 1 .or. n2     .ne. 1 )   khead = khead + 2
  !     real scalars can be shifted 3 more columns:
  if ( intype .eq. 0  .and.  ivec(ind) .eq. 0   .and.  n2     .eq. 1 )   khead = khead + 3
  !     next see if symbol can be profitably centered in 6-column slot:
  n15 = 6
  if ( nchd2  .le.  4 )   go to 1373
  if ( nchd2  .eq.  6 )   go to 1377
  if ( heding(khead-1:khead-1)  .eq.  ' ' )   go to 1377
  n17 = 1
  go to 1374
1373 n17 = ( 6 - nchd2 ) / 2
1374 khead = khead + n17
  n15 = n15 - n17
1377 heding(khead:khead+5) = spycd2(1:6)
  khead = khead + n15
  !     real scalars end up with four blanks on the right:
  if ( intype .eq. 0  .and.  ivec(ind) .eq. 0   .and.  n2     .eq. 1 )   khead = khead + 4
  locout(numex) = ind
  imin(numex) = n1
  imax(numex) = n2
  !     if ( ivec(ind)  .eq.  0 )   go to 1460    ! original record.
  !     next we want to execute s.n. 1394 if subscripting is involved:
  if ( ivec(ind)  .eq.  1 )   go to 1394
  if ( n1 .eq. 1   .and.   n2 .eq. 1 )   go to 1460
1394 write (ansi8, 1400)  n14
1400 format ( 1h(,  i4,  2h)    )
  heding(khead:khead+6) = ansi8(1:7)
  khead = khead + 7
  n14 = n14 + 1
  go to 1354
  !     $$$$$$$  key word no. 20:  "roll"     $$$$  $$$$  $$$$  $$$$  $$$$
1424 jjroll = 1
  go to 1496
  !     $$$$$$$  key word no.  6:  "deposit"  $$$$  $$$$  $$$$  $$$$  $$$$
1460 write (prom80, 1480)
1480 format ( '  send emtp variable:' )
  assign 1485 to nextsn
  go to 9800
1485 spycd2 = buff77(1:35)
  if ( spycd2(1:1) .ne. 'c'   .or.  spycd2(2:2) .ne. ' ' )  go to 1491
  if ( icomm .ne. 0 )  go to 1460
  write (munit6, 1487)  spycd2
1487 format ( ' comment:',  a35 )
  call window
  go to 1460
1491 if ( spycd2(1:4)  .ne.  'end ' )   go to 1720
  !     if processing "deposit" = spykwd(6), then back to "spy:":
1496 if ( answ80(1:8)  .eq.  spykwd(6) )    go to 1240
  !     if internal "deposit" issued from "ramp"= spykwd(34), return:
  if ( answ80(1:8)  .eq.  spykwd(34) )  go to 3138
  !     if internal "deposit", issued from "restore", then back to "spy:":
  if ( answ80(1:8)  .eq.  spykwd(10) )   go to 1240
  !     ok, this is "examine";  first, blank unused right portion:
  if ( khead .lt. 132 ) heding(khead+1:132) = ' '
  !     if disk file is connected to unit 5, we want return to  "spy:" :
  if ( kfile5  .eq.  1 )   go to 1240
  !     $$$$$$$  key word no.  1:  "heading"  $$$$  $$$$  $$$$  $$$$  $$$$
1500 munit6 = heding
  call window
  !     following code outputs table dumping in accord with earlier
  !     setup using  "examine"  specification.
1520 call examin
  if ( jjroll  .eq.  0 )  go to 1682
  jjroll = jjroll + 1
  if ( jjroll .le. 2 )  go to 1669
  call quiter
  if ( kwtspy .eq. 0 ) go to 1523
  jjroll = 0
  go to 1240
1523 if ( outsav .eq. outlin ) go to 1684
1669 outsav = outlin
  !     output line of "examine" is all built;  display it:
1682 if ( kfile5 .ne. 1  .and.  jjroll .eq. 0  .and. kolout .le. 81 ) go to 1685
  munit6 = outlin
  call window
  prom80(1:8) = blan80(1:8)
1684 if ( jjroll .gt. 0 )  go to 9833
  go to 1714
1685 prom80 = outlin(1:kolout-1)
1714 assign 31269 to nextsn
  go to 9800
  !     process emtp symbol name (spycd2) which has just been read;
1720 nchd2 = 0
  n13 = 0
  do j=1, 8
     if ( spycd2(j:j)  .eq.  '('  )   go to 1726
     if ( spycd2(j:j)  .eq.  ','  )   go to 1726
     if ( spycd2(j:j)  .ne.  ' ' )   go to 1721
     if ( nchd2  .eq.  0 )   nchd2 = j - 1
     go to 1723
1721 if ( spycd2(j:j)  .ne.  '='  )   go to 1723
     n13 = j
     if ( nchd2  .eq.  0 )   nchd2 = j - 1
1723 end do
  n1 = 1
  n2 = 1
  j = n13
  if ( n13  .eq.  0 )   j = 7
  go to 1735
1726 if ( nchd2  .eq.  0 )   nchd2 = j - 1
  n9 = 0
  do k=j, 20
     if ( spycd2(k:k)  .ne.  '-' )   go to 1727
     n12 = -1
     go to 1733
1727 if ( spycd2(k:k)  .eq.  ' ' )   go to 1733
     if ( spycd2(k:k)  .ne.  '='  )   go to 21727
     n13 = k
     go to 1734
21727 do i=1, 10
        if ( spycd2(k:k)  .eq.  digit(i) )   go to 1730
1728 end do
     if ( n9  .gt.  0 )   limarr(n9) = limarr(n9) * n12
     !     non-digit means we initialize for next number to be built:
     n9 = n9 + 1
     n12 = 1
     limarr(n9) = 0
     go to 1733
1730 if ( i  .eq.  10 )   i = 0
     limarr(n9) = 10 * limarr(n9)  +  i
1733 end do
1734 n1 = limarr(1)
  n2 = limarr(2)
  if ( n9  .le.  2 )   n2 = n1
1735 spycd2(j:8) = blan80(j:8)
  !     next identify the emtp name involved in this request:
1738 do ind=1, numsym
     if ( spycd2(1:8)  .eq.  symb(ind) )   go to 1780
1740 end do
  write (munit6, 1760)  spycd2
1760 format ( '   ? ?  sorry, no symbol match for  "', a6, '" .')
  call window
  go to 1460
1780 intype = 0
  if ( spycd2(1:1)  .eq.  'i' )   intype = 1
  if ( spycd2(1:1)  .eq.  'j' )   intype = 1
  if ( spycd2(1:1)  .eq.  'k' )   intype = 1
  if ( spycd2(1:1)  .eq.  'l' )   intype = 1
  if ( spycd2(1:1)  .eq.  'm' )   intype = 1
  if ( spycd2(1:1)  .eq.  'n' )   intype = 1
  if ( ivec(ind) .eq. 1   .or.   n2 .le. n1 )  go to 31807
  write (munit6, 1807)
1807 format ( '    note :  this is a vector dump using a scalar', ' emtp variable.'  )
  call window
31807 if ( answ80(1:8)  .eq.  spykwd(5) )   go to 1340
  if ( answ80(1:8)  .eq.  spykwd(34) )   go to 3134
  !     for the case of "deposit", the user-desired value must be read:
  if ( n13  .eq.  0 )   go to 1837
  if ( iascii(ind)  .eq.  1 )   go to 1819
  n13 = n13 + 1
  n22 = 0
  do j=n13, 35
     if ( spycd2(j:j)  .eq.  ' ' )   go to 1813
     n22 = n22 + 1
     if ( n22  .le.  20 )   go to 1808
     write (munit6, 51807)
51807 format ( ' sorry, spy buffer overflow.  try again ....' )
     call window
     go to 1837
1808 bytbuf(n22:n22) = spycd2(j:j)
1813 end do
  if ( n22 .lt. 20 ) bytbuf(n22+1:20) = blan80(n22+1:20)
  !     following code is for alphanumeric emtp symbol after "=" :
1819 ansi8(1:6) = spycd2(n13+1:n13+6)
  if ( iprspy .lt. 1 ) go to 1824
  write (munit6, 1820)  ansi8
1820 format ( ' ansi8 after alphanumeric transfer =',  a )
  call window
1824 go to 1846
  !     no equal sign means that we prompt for separate number:
1837 write (prom80, 1840)
1840 format ( 4x,  'new value:' )
  assign 1842 to nextsn
  go to 9800
1842 bytbuf = buff77(1:20)
  if ( iascii(ind)  .eq.  0 )   go to 1844
  ansi8(1:6) = bytbuf(1:6)
  go to 1846
  !     following code first checks 20-column "bytbuf" working vector
  !     for a legal fortran number;  then free-format write/read gets it:
1844 call numchk ( bytbuf, 20, n33 )
  if ( n33  .eq.  1 )   go to 1837
  call frefp1(bytbuf, d4)
1846 call deposi ( ind, intype, n1, n2, d4 )
  go to 1460
  !     $$$$$$$  key word no. 13:  "find"     $$$$  $$$$  $$$$  $$$$  $$$$
2040 write (munit6, 2042)
2042 format ( 1x,  40h    symbol      word   address      next   )
  call window
2043 write (prom80, 2044)
2044 format ( '      index   address    change      symbol :' )
2050 assign 2048 to nextsn
  go to 9800
2048 spycd2 = buff77(1:35)
  if ( spycd2(1:8)  .eq.  'end     '  )   go to 1240
  if ( spycd2(1:8)  .eq.  'stop    '   )   go to 1240
  if ( spycd2(1:8)  .ne.  blan80(1:8)   )   go to 2052
  write (prom80, 2051)
2051 format ( '   ? ? ?  blank emtp symbol is clearly wrong.', '  try again :'  )
  go to 2050
  !     first we must see if  "spycd2"  has any wild-card characters:
2052 do i=1, 8
     if ( spycd2(i:i)  .eq.  '*'  )   go to 2069
2053 end do
  !     simple case with no wild-card characters:
  do ind=1, numsym
     if ( spycd2(1:8)  .eq.  symb(ind) )   go to 2063
2055 end do
  write (munit6, 2088)
  call window
  go to 2050
2063 n4 = locate(ind) - n5
  n5 = locate(ind)
  write (prom80, 2066)  ind, n5, n4
2066 format ( 1x,  3i10,  4x, 1h: )
  call prompt
  go to 2050
  !     begin search for case where "d2" has one or more wild cards ("*"):
2069 n4 = 0
  do ind=1, numsym
     chard7 = symb(ind)
     n7 = 1
     n2 = 1
     !     enter loop which compares  chard2(n2)  against  chard7(n7):
2071 if ( spycd2(n2:n2) .ne. '*' )  go to 2079
     !     enter code where "*" implies skipping indeterminate number of
     !     characters in  "d7" :
2072 n2 = n2 + 1
     if ( n2  .ge.  9 )  go to 2081
     if ( spycd2(n2:n2)  .eq.  '*' )   go to 2072
     if ( spycd2(n2:n2)  .eq.  ' ' )   go to 2081
     !     nonblank  spycd2(n2:n2) means we must search "d7" for it:
     do i=n7, 6
        if ( chard7(i:i) .eq. spycd2(n2:n2) )  go to 2077
2074 end do
     go to 2086
2077 n2 = n2 + 1
     n7 = i + 1
     if ( n2  .ge.  9 )  go to 2081
     go to 2071
     !     now we are ready to check characters of two words for match:
2079 if ( spycd2(n2:n2) .ne. chard7(n7:n7) )  go to 2086
     n2 = n2 + 1
     n7 = n7 + 1
     if ( n2  .ge.  9 )   go to 2081
     if ( spycd2(n2:n2) .ne. ' ' )  go to 2071
     !     blank in "d2" means success;  we have found a match.  now output:
2081 n4 = locate(ind) - n5
     n6 = locate(ind)
     write (munit6, 2083)  ind, n6, n4, chard7
2083 format ( 1x,  3i10,  6x,  a6,  ' = name  '  )
     call window
2086 call quiter
     if ( kwtspy .eq. 0 ) go to 2087
     kwtspy = 0
     go to 2040
2087 end do
  if ( n4 .ne. 0 )  go to 2043
  write (prom80, 2088)
2088 format ( '    sorry, no matches.   try again :' )
  go to 2050
  !     $$$$$$$  key word no. 14:  "list"     $$$$  $$$$  $$$$  $$$$  $$$$
2092 write (munit6, 2093)  numsym
2093 format ('    row    symbol    word    vector?    ascii?   next', 5x,  i4,  ' symbols'   )
  call window
  write (prom80, 2095)
2095 format ('  number    name   address   (yes=1)   (yes=1)   :'  )
  n33 = 0
2099 assign 2101 to nextsn
  go to 9800
2101 bytbuf = buff77(1:20)
  if ( bytbuf(1:4)  .eq.  'end ' )   go to 1240
  if ( bytbuf(1:4)  .eq.  'stop' )   go to 1240
  if ( bytbuf(1:4)  .ne.  '    '   .and. bytbuf(1:4)  .ne.  'next' )  go to 2102
  n1 = n2 + 1
  n2 = n1
  go to 2108
2102 if ( bytbuf(1:4) .ne. 'top ' )  go to 2103
  n1 = 1
  n2 = 1
  go to 2109
2103 if ( bytbuf(1:4) .ne. 'bot ' )  go to 2105
  n1 = numsym
  n2 = numsym
  go to 2109
2105 if ( bytbuf(1:4) .ne. 'all ' )  go to 2107
  n1 = 1
  n2 = numsym
  go to 2109
2107 call frein2(bytbuf, n1, n2)
2108 if ( n1 .le. 0 )  n1 = 1
  if ( n1 .gt. numsym )  n1 = numsym
  if ( n2 .le. n1 )  n2 = n1
  if ( n2 .gt. numsym )  n2 = numsym
2109 do i=n1, n2-1
     write (munit6, 2113)  i, symb(i), locate(i), ivec(i), iascii(i)
2113 format ( i8, 4x, a6, i9, i8, i10 )
     call window
     call quiter
     if ( kwtspy .eq. 0 ) go to 2116
     kwtspy = 0
     go to 2092
2116 end do
  write (prom80, 3217)  n2, symb(n2), locate(n2), ivec(n2), iascii(n2)
3217 format ( i8, 4x, a6, i9, i8, i10, 4x,  1h:  )
  go to 2099
  !     $$$$$$$  key word no. 16:  "break"    $$$$  $$$$  $$$$  $$$$  $$$$
2124 write (prom80, 2125)
2125 format (  '   send  t-break  in', ' sec. ("-" means step #):'  )
  assign 2126 to nextsn
  go to 9800
2126 call frefp1 ( buff77, tbreak )
  nbreak = 16
  if (tbreak .gt. 0.0)  go to 2142
  if (tbreak .eq. 0.0)  go to 2131
  d13 = -tbreak * deltat
  write (munit6, 2128)  deltat, d13
2128 format ( '    ok, using  deltat =',  e12.3, '  this gives  t-break =',  e13.5  )
  call window
  tbreak = d13
  go to 2142
2131 write (prom80, 2136)
2136 format (  '    send utpf overlay number nchain for', ' the break :'   )
  assign 2139 to nextsn
  go to 9800
2139 call frein1 ( buff77, nbreak )
  if ( nbreak .lt. -1   .or. nbreak .gt. 20 )  go to 2131
  if ( nbreak .ne. 16 ) tbreak = -9876.e33
2142 go to 1240
  !     $$$$$$$  key word no.  9:  "save"     $$$$  $$$$  $$$$  $$$$  $$$$
2206 tmax = t - epsiln
  if ( twhen  .gt.  flzero )   tmax = twhen
  memsav = 16
  go to 1240
  !     $$$$$$$  key word no. 10:  "restore"  $$$$  $$$$  $$$$  $$$$  $$$$
2317 tmax = t - epsiln
  if ( twhen  .gt.  flzero )   tmax = twhen
  memsav = 1016
  go to 1240
  !     $$$$$$$  key word no. 11:  "go"       $$$$  $$$$  $$$$  $$$$  $$$$
2402 lockbr = 0
  if ( iprspy .lt. 1 )  go to 2409
  write (munit6, 2407)
2407 format ( ' "go" acknowledged, set lockbr = 0.'  )
  call window
2409 go to 1240
  !     $$$$$$$  key word no. 17:  "when"     $$$$  $$$$  $$$$  $$$$  $$$$
2430 write (prom80, 2433)  twhen
2433 format (  '  supply tmax for "save" & "restore"  [',  f8.4, ' ] :'   )
  assign 2436 to nextsn
  go to 9800
2436 bytbuf = buff77(1:20)
  if ( bytbuf(1:4)  .eq.  '    ' )   go to 2439
  call frefp1 ( bytbuf, d34 )
  if ( d34  .gt.  0.0 )   twhen = d34
2439 go to 1240
  !     $$$$$$$  key word no. 18:  "comment"  $$$$  $$$$  $$$$  $$$$  $$$$
2486 if ( icomm  .eq.  0 )   go to 2491
  icomm = 0
  write (munit6, 2488)
2488 format ('   begin displaying comment cards of  "@"  usage.' )
  call window
  go to 1240
2491 icomm = 1
  write (munit6, 2492)
2492 format ('   stop showing comment cards of  "@"  usage.' )
  call window
  go to 1240
  !     $$$$$$$  key word no. 22:  "verify"   $$$$  $$$$  $$$$  $$$$  $$$$
2494 if ( kverfy  .eq.  0 )   go to 2497
  kverfy = 0
  write (munit6, 2495)
2495 format ( '   begin echoing commands of  "@"  file usage.' )
  call window
  go to 1240
2497 kverfy = 1
  write (munit6, 2499)
2499 format ( '   stop echoing commands of  "@"  file usage.' )
  call window
  go to 1240
  !     $$$$$$$  key word no. 21:  "type?"    $$$$  $$$$  $$$$  $$$$  $$$$
2506 n16 = 0
  n26 = 5
  ansi32 = 'inclspy .dat                    '
  if ( iprspy .lt. 1 )  go to 2509
  write (munit6, 2508)   answ80(1:20)
2508 format ( '   start "type" service.  answ80(1:20) =', a20 )
  call window
2509 if ( answ80(5:7) .eq. '?  ' ) n16 = 1
  if ( answ80(5:7) .eq. ' ? ' ) n16 = 1
  if ( answ80(5:7) .eq. '  ?' ) n16 = 1
  if ( answ80(1:8) .eq. 'type    '  )   n16 = 1
  if ( n16 .eq. 0 ) go to 2511
  n18 = 0
  go to 2548
2511 n12 = 0
  n13 = 0
  do j=n26, 32
     if ( n12 .gt. 0 )  go to 2514
     if ( answ80(j:j) .eq. ' ' )  go to 2517
     n12 = j
2514 if ( answ80(j:j) .eq. ' ' ) go to 2519
     n13 = j
2517 end do
2519 if ( iprspy .lt. 1 )  go to 32521
  write (munit6, 2521)  n12, n13
2521 format ( ' file name bounded.  n12, n13 =',  2i5 )
  call window
32521 if ( n13 .ne. n12 )  go to 2524
  ansi32(8:8) = answ80(n12:n12)
  do i=1, 9
     if ( answ80(n12:n12) .eq. digit(i)  .and. filext(i) .eq. 'x' ) go to 2536
2522 end do
  write (munit6, 2526) answ80(n12:n12)
  call window
  go to 1240
2524 inquire (file=answ80(n12:n13), exist=logvar)
  if ( logvar ) go to 2529
  write (munit6, 2526) answ80(n12:n13)
2526 format ( '   ?? ??  sorry, no such file named : ', a32 )
  call window
  go to 1240
2529 ansi32 = blan80(1:32)
  ansi32(1:n13-n12+1) = answ80(n12:n13)
2532 if ( answ80(1:1) .eq. '@' )  go to 2536
  write (munit6, 2534)  ansi32
2534 format ( 20x,  ' ******  listing of file : ',  a32 )
  call window
2536 muntsv(1) = munit5
  munit5 = muntsv(2)
2537 open (unit=munit5, status='old', file=ansi32)
  if ( answ80(1:1) .eq. '@' ) go to 1276
  do i=1, 9999
     call quiter
     if ( kwtspy .eq. 0 )  go to 2538
     kwtspy = 0
     go to 2545
2538 read (munit5, 1269, end=2545)  buff77
1269 format ( a80 )
     write (munit6, 2539)  i, buff77
2539 format ( ' line', i4,  ': ',  a80 )
     call window
2542 end do
2545 close (unit=munit5, status='keep')
  if ( n16 .eq. 0 ) go to 2551
2548 n18 = n18 + 1
  if ( n18 .eq. 10 )  go to 2551
  if ( filext(n18) .ne. 'x' )  go to 2548
  ansi32(8:8) = digit(n18)
  go to 2537
2551 munit5 = muntsv(1)
  go to 1240
  !     $$$$$$$  key word no. 23:  "files"    $$$$  $$$$  $$$$  $$$$  $$$$
  !     "files"  results in a display of the status of "@?" files, as
  !     previously determined by  "initsp"  as simulation began.
2563 write (munit6, 2572)  (j, j=1, 9)
2572 format (  '   file number :',  9i5  )
  call window
  write (munit6, 2576)  ( filext(j), j=1, 9 )
2576 format (  '   inclspy?.dat:',  9(4x, a1) )
  call window
  go to 1240
  !     $$$$$$$  key word no. 24:  "sleep"    $$$$  $$$$  $$$$  $$$$  $$$$
  !     "sleep"  provides for exit to overlay 20, followed by the
  !     saving of emtp tables on disk.   the subsequent use of
  !     "wake" can revive the suspended simulation.
2591 tmax = -9999.
  memsav = 2016
  go to 1240
  !     $$$$$$$  key word no. 25:  "wake"     $$$$  $$$$  $$$$  $$$$  $$$$
2604 write (munit6, 32604)
32604 format ( '     begin  "wake"  processing.'  )
  call window
  if ( n13 .ne. 1 )  go to 2611
  !     regenerate header of plot file attached to unit lunit4:
  write (munit6, 2605)
2605 format ( '    start regenerating lunit4 header.'  )
  call window
  rewind lunit4
  read (lunit4)  date1, tclock, numnam, numnvo, numbco, numbrn
  if ( lbus  .ge.  numnam    .and. lsiz12  .ge.  numnvo    .and. lbrnch .ge. numbrn )   go to 2608
  write (munit6, 2606)
2606 format (  '   $$$$  overflow.  emtp list sizes', ' are too small to regenerate the header', &
       ' information of the plot file.'  )
  call window
  write (munit6, 2607)  numnam, numnvo, numbrn
2607 format (  '       abort this command.  sorry.', '   numnam, numnvo, numbrn =', 3i5 )
  call window
  go to 1240
2608 rewind lunit4
  read (lunit4)  date1, tclock, numnam, numnvo, numbco, numbrn, ( bus(j), j=1, numnam )
  if ( numnvo  .gt.  0 ) read (lunit4)  ( ibsout(j), j=1, numnvo )
  if ( numbrn  .gt.  0 ) read (lunit4)  ( kbus(j), mbus(j), j=1, numbrn )
  rewind lunit4
  write (lunit4)  date1, tclock, numnam, numnvo, numbco, numbrn, ( bus(j), j=1, numnam )
  if ( numnvo  .gt.  0 ) write (lunit4)  ( ibsout(j), j=1, numnvo )
  if ( numbrn  .gt.  0 ) write (lunit4)  ( kbus(j), mbus(j), j=1, numbrn )
2611 call runtym ( d1, d2 )
  call pfatch
  call tables
  flstat(1) = -d1
  flstat(2) = -d2
  if ( lstat(16) .eq. ltlabl )  go to 2614
  write (munit6, 2612)
2612 format ( '   ????  sorry, present emtp dimensions', ' do not agree with those of the disk file' )
  call window
  write (munit6, 32612)
32612 format ( 9x, 'just attached.  sleeping simulation can not', ' be awakened.  abort the command.  sorry.'  )
  call window
  go to 1240
2614 kill = 7733
  lstat(15) = 16
  go to 2402
  !     $$$$$$$  key word no. 29:  "catalog"  $$$$  $$$$  $$$$  $$$$  $$$$
2907 write (prom80, 2910)
2910 format ( ' send complete, legal disk-file name:'  )
  assign 2913 to nextsn
  go to 9800
2913 ansi32 = buff77(1:32)
  if ( ansi32 .eq. blan80(1:32) ) ansi32 = 'spydata                         '
  close (unit=luntsp)
  open (unit=luntsp, status='new', file=ansi32)
  n1 = 1
  n2 = 80
  do j=1, numcrd
2914 write (luntsp, 1269)  file6(j)
  end do
  write (munit6, 2917)  luntsp, numcrd
2917 format ( '    ---- write to disk via unit',  i4, '   is now complete.  number of cards =',  i5  )
  call window
  close (unit=luntsp, status='keep')
  go to 1240
  !     $$$$$$$  key word no. 30:  "begin"    $$$$  $$$$  $$$$  $$$$  $$$$
2926 kill = 7733
  llbuff = -3333
  lstat(15) = 1
  indbuf = 0
  mflush = 0
  go to 2402
  !     $$$$$$$  key word no. 31:  "step"     $$$$  $$$$  $$$$  $$$$  $$$$
2937 if ( kbreak .eq. 0 )  go to 2944
  write (munit6, 2941)
2941 format ( '   ---  toggle to "off"  (no more stepping)'  )
  call window
  go to 2946
2944 write (munit6, 2945)
2945 format ( '   ---  toggle to "on"  (begin stepping)'  )
  call window
2946 kbreak = kbreak + 1
  if ( kbreak .ge. 2 )  kbreak = 0
  if ( kbreak .eq. 1 ) lockbr = 1
  go to 1240
  !     $$$$$$$  key word no. 32:  "debug"    $$$$  $$$$  $$$$  $$$$  $$$$
2964 write (prom80, 2967)   iprspy
2967 format ( '   send new "iprspy" value  [',  i3,  '  ] :'  )
  assign 2969 to nextsn
  go to 9800
2969 call frein1 ( buff77, iprspy )
  go to 1240
  !     $$$$$$$  key word no. 33:  "data"     $$$$  $$$$  $$$$  $$$$  $$$$
2974 n12 = 1
  n13 = 9999
  n14 = 1
  n15 = 0
2975 write (prom80, 2976)
2976 format ( '   send emtp data file name (control) :' )
  assign 2979 to nextsn
  go to 9800
2979 ansi32 = buff77(1:32)
  if ( ansi32(1:8) .ne. 'control ' ) go to 2984
  write (prom80, 2981)
2981 format ( ' send disk card numbers n-beg and n-end :'  )
  assign 32981 to nextsn
  go to 9800
32981 call frein2(buff77, n12, n13)
  write (prom80, 2982)
2982 format (  ' send n-beg in emtp, lunit5 offset :'  )
  assign 2983 to nextsn
  go to 9800
2983 call frein2(buff77, n14, n15)
  go to 2975
2984 if ( iprspy .lt. 1 ) go to 32985
  write (munit6, 2985)  n12, n13, n14, n15
2985 format ( ' prepare to open.  n12, n13, n14, n15 =', 4i8 )
  call window
32985 close (lunt14)
  open (unit=lunt14, status='unknown', file=ansi32)
  if ( iprspy .lt. 1 ) go to 2987
  write (munit6, 2986)
2986 format ( ' after file open (it worked).'  )
  call window
2987 do l=1, n13
     if ( l .ge. n12 )  go to 2988
     read (lunt14, 2991)  ansi8
     go to 2994
2988 read (lunt14, 2991, end= 2998)  file6(n14)
2991 format ( a80 )
     if ( file6(n14)(1:4) .eq. 'eof ' )  go to 2998
     n14 = n14 + 1
     if ( n14 .le. limcrd )  go to 2994
     write (munit6, 2191)
2191 format ( '   ****  warning.   card image buffer has', ' filled.  disk read is truncated.'      )
     call window
     write (munit6, 2192)  limcrd
2192 format ( '                    storage capacity in', ' cards = limcrd =',  i5  )
     call window
     go to 2998
2994 end do
  write (munit6, 2996)  n13
2996 format ( '    note :  no  end-of-file  encountered.', '   stop reading after card number',  i5  )
  call window
2998 if ( numcrd .lt. n14 ) numcrd = n14 - 1
  write (munit6, 2999)  numcrd
2999 format (  i9,  '  =  numcrd  (upper bound on card-image', ' storage in file6 cache).'  )
  call window
  if ( l .gt. 1 )  go to 3006
  write (munit6, 3004)
3004 format ( '  ? ? ? ? ?   warning.  data file is empty.', '   did user misspell the file name?'  )
  call window
3006 close ( unit=lunt14 )
  numdcd = n15
  go to 1240
  !     $$$$$$$  key word no. 34:  "ramp"     $$$$  $$$$  $$$$  $$$$  $$$$
3095 if ( numrmp .le. 0 ) go to 3099
  !     beginning of loop over ramps having same begin and ending times:
3096 write (prom80, 3098)
3098 format ( ' send "t-begin", "t-end" (end, show, rewind) :' )
  assign 33098 to nextsn
  go to 9800
33098 bytbuf = buff77(1:20)
  if ( bytbuf(1:4)  .eq.  'end ' )  go to 1240
  if ( bytbuf(1:4) .eq. 'stop' ) call stoptp
  if ( bytbuf(1:6) .ne. 'rewind' )  go to 3100
3099 numrmp = 0
  kanal = 0
  tmaxrp = -1.e20
  tminrp =  1.e20
  go to 3096
3100 if ( bytbuf(1:4)  .ne.  'show' )   go to 3116
  !     following code provides confirmation of the ramped deposits:
  write (munit6, 3102)
3102 format (  ' row  name   index   num    t-begin        t-end', '        f-begin        f-end'     )
  call window
  do j=1, numrmp
     write (munit6, 3103) j, symbrp(j), indxrp(j), looprp(j),  tbegrp(j), tendrp(j), fbegrp(j), fendrp(j)
3103 format ( 1x, i2,  2x, a6, 2i6,  4e14.5  )
     call window
3104 end do
  write (munit6, 3105)  tminrp, tmaxrp
3105 format ( '         no ramping before',  e13.4,  '  or after',  e13.4,  '  sec.'  )
  call window
  go to 3096
3116 call frefp2 ( bytbuf, tim1rp, tim2rp )
  if ( tim1rp  .eq.  0.0 ) tim1rp = t
  if ( tim2rp  .lt.  0.0 ) tim2rp = tim1rp + abs ( tim2rp )
  !     beginning of loop over function values (for given times):
3122 write (prom80, 3125)
3125 format ( ' send "f-begin", "f-end" (end) : '  )
  assign 3128 to nextsn
  go to 9800
3128 bytbuf = buff77(1:20)
  if ( bytbuf(1:4)  .eq.  'end ' )   go to 3096
  if ( bytbuf(1:4) .eq. 'stop' ) call stoptp
  call frefp2 ( bytbuf, val1rp, val2rp )
  !     beginning of loop over variables (for given f vs. t ramps):
3131 go to 1460
  !     we return to 3134 after "deposit" logic has found variable info.
  !     actual transfer back here is from just below 1807.
3134 n8 = locate(ind) + 2*(n1-1)
  n9 = ( n8 - memkar ) / 2
  n10 = memkar + 2*n9 - n8
  if ( n10  .ne.  0     .and.  n9 .lt. 0 )  n9 = n9 -1
  numrmp = numrmp + 1
  kanal = 1
  if ( numrmp  .le.  20 )   go to 3135
  write (munit6, 23134)
23134 format (   '  ** **  table overflow.   erase all', '  "ramp"  entries, and start again ....' )
  call window
  go to 3095
3135 memrmp(numrmp) = n9 + 1
  n10rmp(numrmp) = n10
  looprp(numrmp) = n2 - n1 + 1
  tbegrp(numrmp) = tim1rp
  if ( tim1rp .lt. tminrp )  tminrp = tim1rp
  tendrp(numrmp) = tim2rp
  if ( tim2rp .gt. tmaxrp )  tmaxrp = tim2rp
  fbegrp(numrmp) = val1rp
  fendrp(numrmp) = val2rp
  symbrp(numrmp) = symb(ind)
  indxrp(numrmp) = n1
  rampsl(numrmp) = (val2rp - val1rp) / (tim2rp - tim1rp)
  rampcn(numrmp)  =   val2rp  -  rampsl(numrmp) * tim2rp
  write (prom80, 3136)
3136 format (  '     [y] change? (0=no, 1=yes) :'  )
  assign 43137 to nextsn
  go to 9800
43137 call frein1 ( buff77, n8 )
  kyramp(numrmp) = n8
  if ( n8 .eq. 0 )  go to 3131
  tendrp(numrmp) = tim2rp + deltat
  if ( tmaxrp .lt. tendrp(numrmp) )  tmaxrp = tendrp(numrmp)
  if ( n1 .gt. ibr )  go to 3131
  write (munit6, 3137)  n1, ibr
3137 format ( '  %%  %%  cancel last "ramp" request.', '  row number',  i4,  '  does not exceed ibr =', i4 )
  call window
  numrmp = numrmp - 1
  go to 3131
  !     we return to 3138 from below 1496, if "end" read during "deposit"
3138 go to 3122
  !     $$$$$$$  key word no. 35:  "time"     $$$$  $$$$  $$$$  $$$$  $$$$
3153 call time44 ( spytim(1) )
  call date44 ( spdate(1) )
  write (munit6, 3158)  t, tmax, deltat, spytim, spdate
3158 format ( 3x,  3ht =,  e13.6,  3x,  6htmax =,  e11.4,  3x, 8hdeltat =,  e11.4,  3x,  2a4,  2x,  2a4 )
  call window
  go to 1240
  !     $$$$$$$  key word no. 36:  "tek"      $$$$  $$$$  $$$$  $$$$  $$$$
3174 write (munit6, 3177)
3177 format ( '  >< to tamper with the rolling vector plot,  send', ' choice.'    )
  call window
3178 write (prom80, 3179)
3179 format ( '  >< option (mark, delay, inner, overlap,', ' end, help) : ' )
  assign 3180 to nextsn
  go to 9800
3180 ansi8 = buff77(1:8)
  if ( ansi8 .ne. 'help    ' ) go to 3186
  write (munit6, 3181)
3181 format (  '     mark  ---- for instantaneous marking of curves', ' on tek screen;'                   )
  call window
  write (munit6, 33181)
33181 format (  '     delay  --- to control how simultaneous the', ' rolling is to be;'                )
  call window
  write (munit6, 43181)
43181 format (  '     inner  ---- to  call timval  (the "inner:"', ' level of ploting);'           )
  call window
  write (munit6, 53181)
53181 format (  '     overlap  -- to modify the percent overlap', ' for new-page plot;'           )
  call window
  write (munit6, 3182)
3182 format (  '     end   ---- for return to  "spy:"  prompt.'   )
  call window
  go to 3178
3186 if ( ansi8 .ne. 'mark    ' )   go to 3193
  write (prom80, 3189)  ksymbl
3189 format (  '    send repeat frequency after', ' immediate 1st one [',  i6,  ' ] :'  )
  assign 3191 to nextsn
  go to 9800
3191 call frein1 ( buff77, ksymbl )
  go to 3178
3193 if ( ansi8 .ne. 'delay   ' ) go to 3207
  write (prom80, 3202)  kslowr
3202 format  ( '  send new rolling frequency [', i5, ' ] :' )
  assign 3204 to nextsn
  go to 9800
3204 call frein1 ( buff77, kslowr )
  go to 3178
3207 if ( ansi8 .ne. 'inner   ' )  go to 3209
3208 monitr = 2345
  call timval
  monitr = 1
  if ( nexmod .eq. 0 ) go to 3178
  nexmod = 1
  go to 9803
3209 if ( ansi8 .ne. 'overlap ' )  go to 3214
  write (prom80, 3211)  inchlp
3211 format ( '    send time-overlap percentage [',  i3,  ' ] :'  )
  assign 3204 to nextsn
  go to 9800
3212 call frein1 ( buff77, inchlp )
  go to 3178
3214 if ( ansi8 .eq. 'end     ' )  go to 1240
  write (munit6, 3227)
3227 format ( '   sorry,  spy  does not understand.   come again ...' )
  call window
  go to 3178
  !     $$$$$$$  key word no. 37:  "branch"   $$$$  $$$$  $$$$  $$$$  $$$$
3256 bytbuf(1:4) = answ80(1:4)
  n33 = 0
  n24 = 0
  go to 3308
3306 assign 3307 to nextsn
  go to 9800
3307 bytbuf = buff77(1:20)
3308 if ( bytbuf(1:4)  .eq.  'end ' )   go to 1240
  if ( bytbuf(1:4)  .eq.  'stop' )   go to 1240
  if ( bytbuf(1:6) .ne. 'series' )  go to 3309
  n24 = 1
  go to 3306
3309 if ( bytbuf(1:4) .ne. 'extr' ) go to 3317
  n33 = n33 + 1
  if ( n33 .ge. 2 )  n33 = 0
3310 if ( n33 .eq. 0 )  go to 3314
  write (prom80, 3311)
3311 format ( 4h row, 13x, 2hci,  13x, 2hck,  12x, 3hcik, 12x, 3hcik,  10x, 5hckkjm,  1h:  )
  go to 3306
3314 write (prom80, 3315)
3315 format ( ' row  name  name-k name-m kbus mbus lgth  nr', ' kodebr kodsem litype imodel indhst:'  )
  go to 3306
3317 if ( bytbuf(1:4) .eq. spykwd(37)(1:4)  .or. bytbuf(1:4) .eq. spykwd(1)(1:4) ) go to 3310
  !     Since not key-word response, we extract (n1,n2) integer
  !     pair (free-format); "ibr" is maximum; "n17" is error flag:
3322 call intpar ( ibr, n1, n2, n17 )
  if (n17 .gt. 0 )  go to 3306
  n27 = 1
  i = n1
3323 call quiter
  if ( kwtspy .eq. 0 ) go to 3325
  kwtspy = 0
  go to 3306
3325 n5 = iabs ( kbus(i) )
  n6 = iabs ( mbus(i)  )
  bus1 = bus(n5)
  if ( n5 .eq. 1 )  bus1 = terra
  bus2 = bus(n6)
  if ( n6 .eq. 1 )  bus2 = terra
  if ( n24 .eq. 0 )  go to 3327
  n27 = length(i)
  if ( n5 .lt. 0 )  go to 3334
  if ( kodebr(i) .gt. 0 )  go to 3334
  if ( n27 .ne. 1 )  go to 3334
  if ( nr(i) .ge. 0 )  go to 3334
3327 if ( n33 .ne. 0 )  go to 3329
  n7 = namebr(i)
  write (munit6, 3328) i, texvec(n7), bus1, bus2, kbus(i), mbus(i), length(i), nr(i), kodebr(i), kodsem(i), &
       litype(i), imodel(i), indhst(i)
3328 format ( 1x, i3, 1x, a6, 1x, a6, 1x, a6,  3i5,  i4,  5i7 )
  call window
3329 if ( n33 .ne. 1 )  go to 3334
  write (munit6, 3331) i, ci(i), ck(i), cik(i), cki(i), ckkjm(i)
3331 format ( 1x, i3,  5e15.6  )
  call window
3334 i = i + n27
  if ( i .le. n2 )  go to 3323
  prom80 = blan80
  go to 3306
  !     $$$$$$$  key word no. 38:  "yform"    $$$$  $$$$  $$$$  $$$$  $$$$
3357 kansav = kanal
  kanal = 2
  go to 3394
  !     $$$$$$$  key word no. 39:  "noy"      $$$$  $$$$  $$$$  $$$$  $$$$
3381 kanal = kansav
  go to 3406
  !     $$$$$$$  key word no. 40:  "factor"   $$$$  $$$$  $$$$  $$$$  $$$$
3394 ialter = 2
  go to 1240
  !     $$$$$$$  key word no. 41:  "nof"      $$$$  $$$$  $$$$  $$$$  $$$$
3406 if ( ialter .eq. 2 ) ialter = 1
  go to 1240
  !     $$$$$$$  service 7:switch, 25:source, 26:edit, 28:language,
  !              42:rlc, 43:width, 44:bus, 45:size, 46:limit,
  !              47:iout, 48:node, 49:nonlin, 50:space, 51:lunit4,
  !              52:series, 53:lock, 54:[y], 55:[f], 56:noroll
8500 call spyink
  if ( nexmod .ne. 0 ) go to 9803
  go to 1240
  !     $$$$$$$  key word no. 57:  "open"     $$$$  $$$$  $$$$  $$$$  $$$$
3568 write (prom80, 3571)
3571 format ( '    next unit, name (i2, a32 ) :'  )
  assign 3573 to nextsn
  go to 9800
3573 read (buff77, 3576)  n7, ansi32
3576 format ( i2, a32 )
  if ( n7 .le. 0 )  go to 1240
  open (unit=n7, status='unknown', file=ansi32)
  go to 3568
  !     $$$$$$$  key word no. 58:  "close"    $$$$  $$$$  $$$$  $$$$  $$$$
3584 write (prom80, 3587)
3587 format ( '    next unit, status (i2, a32) :' )
  assign 3591 to nextsn
  go to 9800
3591 read (buff77, 3576)  n7, ansi32
  if ( n7 .le. 0 )  go to 1240
  if ( ansi32(1:6) .eq. 'keep  ' ) go to 3596
  if ( ansi32(1:6) .eq. 'delete' ) go to 3596
  close ( unit=n7 )
  go to 3584
3596 close ( unit=n7, status=ansi32 )
  go to 3584
  !     $$$$$$$  key word no. 59:  "sm"       $$$$  $$$$  $$$$  $$$$  $$$$
3606 if ( nchain .ge. 16   .and. nst .gt. 0 ) go to 43606
  write (munit6, 3607)
3607 format ( '   ?? ??  no s.m. data present or not yet', ' in time-step loop.'  )
  call window
  go to 1240
43606 ksmspy(3) = 1
  lockbr = 0
3608 ksmspy(1) = -1
  ksmspy(2) = 0
  maxflg = 1
  assign 3609 to nextsn
  prom80(1:8) = '        '
  go to 9800
3609 if ( iprspy .lt. 1 ) go to 83609
  write (munit6, 23609)  ksmspy(2)
23609 format ( ' at s.n. 3609 of "spying".  ksmspy(2) =', i4 )
  call window
83609 if ( ksmspy(2) .eq. 0 ) go to 3608
  ksmspy(3) = 0
  write (prom80, 43609)  ksmspy(2)
43609 format ( '  s.m.', i3,  '  wanted (y or n) ? :'  )
  ksmspy(1) = +1
  assign 3610 to nextsn
  go to 9800
3610 if ( buff77(1:1) .eq. 'n' )  go to 43606
  ksmspy(1) = +1
3611 write (prom80, 3612)
3612 format ( '     send s.m. class (elec, mech, elva, meva,', ' all, end) :'  )
  assign 3613 to nextsn
  go to 9800
3613 n14 = ksmspy(2)
  if ( buff77(1:4) .eq. 'end' )  go to 43606
  if ( buff77(1:4) .ne. 'elec'   .and. buff77(1:4) .ne. 'all ' )  go to 43615
  k2 = ( n14-1 ) * 101
  write (munit6, 33614)
33614 format ( '             ld            laf             lf', '           lakd           lfkd' )
  call window
  xl = elp( k2+20 )
  d3 = elp( k2+1 ) + xl
  d4 = elp( k2+2 )
  d5 = ( elp( k2+3 ) + elp( k2+62 ) ) / factom
  d6 = elp( k2+4 )
  d8 = elp( k2+5 )
  write (munit6, 53614)   d3, d4, d5, d6, d8
53614 format( 5e15.7 )
  call window
  write ( munit6, 63614 )
63614 format ( '            lkd             rf            rkd', '             lq            lag' )
  call window
  d3 = ( elp( k2+6 ) + elp( k2+63 ) ) / factom
  d4 = elp( k2+7 )
  d5 = elp( k2+8 )
  d6 = elp( k2+9 ) + xl
  d8 = elp( k2+10 )
  write (munit6, 53614)  d3, d4, d5, d6, d8
  call window
  write (munit6, 73614)
73614 format ( '             lg           lakq           lgkq', '            lkq             rg' )
  call window
  d3 = ( elp( k2+11 ) + elp( k2+64 ) ) / factom
  d4 = elp( k2+12 )
  d5 = elp( k2+13 )
  d6 = ( elp( k2+14 ) + elp( k2+65 ) ) / factom
  d8 = elp( k2+15 )
  write (munit6, 53614)  d3, d4, d5, d6, d8
  call window
  write( munit6, 83614 )
83614 format ( '            rkq             l0             r0', '         agline             ra' )
  call window
  d3 = elp( k2+16 )
  d4 = 1.0 / elp( k2+17 )
  d5 = elp( k2+18 )
  d5 = ( d4 -d5 ) / ( 1.0 + damrat )
  d4 = ( d4 - d5 ) / factom
  d8 = elp( k2+20 )
  write (munit6, 53614)  d3, d4, d5, xl, d8
  call window
  write (munit6, 23615)
23615 format ( '           rat1         dsat10         dsat12', '         qsat10         qsat12' )
  call window
  k3 = k2 + 21
  k2 = k2 + 25
  write (munit6, 53614)  ( elp( k1 ), k1 = k3, k2 )
  call window
43615 if ( buff77(1:4) .ne. 'mech'   .and. buff77(1:4) .ne. 'all ' )  go to 13617
  k2 = ( n14-1 ) * 101 + 26
  k3 = ( n14-1 ) * 30  + 12
  numask = ismdat( k3 )
  write ( munit6, 53615 )
53615 format ( '  numask   nlocg   nloce     cnp' )
  call window
  write (munit6, 63615)  numask, ismdat(k3+1), ismdat(k3+2), elp(k2)
63615 format ( 3i8, f8.1 )
  call window
  write ( munit6, 83615 )
83615 format ( '           hico            dsm            hsp', '            dsr' )
  call window
  k2 = 0
  kp = 0
  do k1 = 1, n14
     k2 = k2 + ismdat( kp+12 )
3616 kp = kp + 30
  end do
  num2 = numask + numask
  num3 = numask + num2
  num5 = num3 + num2
  k2 = ( k2 - numask ) * 12
  k2 = k2 + num2
  do k1 = 1, numask
     k2 = k2 + 1
     write (munit6, 53614)  shp( k2+numask ), shp( k2+num2 ), shp( k2+num3 ), shp( k2+num5 )
     call window
43616 end do
13617 if ( buff77(1:4) .ne. 'meva'   .and. buff77(1:4) .ne. 'all ' )  go to 13618
  write( munit6, 23617 )
23617 format( ' mechanical angles in units of radians ' )
  call window
  k3 = ( n14-1 ) * 30 + 12
  numask = ismdat( k3 )
  k2 = 0
  kp = 0
  do k1 = 1, n14
     k2 = k2 + ismdat( kp+12 )
3617 kp = kp + 30
  end do
  k2 = ( k2 - numask ) * 6
  k3 = k2 + 1
  k2 = k2 + numask
  write (munit6, 53614)  ( histq( k1 ), k1 = k3, k2 )
  call window
  k3 = k2 + 1
  k2 = k2 + numask
  write( munit6, 33617 )
33617 format( ' mechanical speeds in units of  radians/sec ' )
  call window
  write (munit6, 53614)  ( histq( k1 ), k1 = k3, k2 )
  call window
13618 if ( buff77(1:4) .ne. 'elva'  .and. buff77(1:4) .ne. 'all ' )  go to 3611
  write( munit6, 23618 )
23618 format( '             id             iq             i0', '             if            ikd' )
  call window
  k2 = ( n14-1 ) * 24
  k3 = k2 + 1
  k2 = k2 + 5
  write (munit6, 53614)  ( cu( k1 ), k1 = k3, k2 )
  call window
  write( munit6, 33618 )
33618 format( '             ig            ikq             ia', '             ib             ic' )
  call window
  k3 = k2 + 1
  k2 = k2 + 2
  write (munit6, 53614)  cu( k3 ), cu( k2 ), ( smoutv( k1 ), k1 = 1, 3 )
  call window
  write( munit6, 43618 )
43618 format( '             vd             vq             v0', '             vf             va' )
  call window
  write (munit6, 53614)  ( smoutv( k1 ), k1 = 4, 8 )
  call window
  write( munit6, 53618 )
53618 format( '             vb             vc            teq', '           texc' )
  call window
  write (munit6, 53614)  ( smoutv( k1 ), k1 = 9, 12 )
  call window
3619 continue
  go to 3611
  !     $$$$$$$  key word no. 60:  "honk"     $$$$  $$$$  $$$$  $$$$  $$$$
3623 write (prom80, 3626)
3626 format ( '   send severity level of alert (1 to 10) :' )
  assign 3629 to nextsn
  go to 9800
3629 call frein1 ( buff77, n24 )
  call honker ( n24 )
  go to 1240
  !     $$$$$$$  key word no. 63:  "wait"     $$$$  $$$$  $$$$  $$$$  $$$$
3644 write (prom80, 3647)
3647 format ( '     send desired hibernation time in seconds :' )
  assign 3649 to nextsn
  go to 9800
3649 call frefp1 ( buff77, d13 )
  call tdelay ( d13 )
  go to 1240
  !     $$$$$$$  key word no. 12:  "echo"     $$$$  $$$$  $$$$  $$$$  $$$$
3673 write (prom80, 3676)
3676 format ( '    send desired operation (begin, file, show) :' )
  assign 3681 to nextsn
  go to 9800
3681 if ( buff77(1:6) .ne. 'begin ' )  go to 3686
  kspsav = limcrd + 1
  n23 = limcrd - numcrd
  write (munit6, 3683)  n23
3683 format ( '         ok, up to a maximum of',  i7, '   commands can be accumulated, if no more data.' )
  call window
  go to 1240
3686 if ( buff77(1:5) .ne. 'file ' )  go to 3702
  write (*, *)  ' to be completed later.  ?????????'
  go to 1240
3702 if ( buff77(1:5) .ne. 'show ' )  go to 1303
  write (*, *)  ' to be completed later.  ??????'
  go to 3673
  !     $$$$$$$  key word no.  2:  "stop"     $$$$  $$$$  $$$$  $$$$  $$$$
9000 call time44 ( spytim(1) )
  call date44 ( spdate(1) )
  write (munit6, 3158)  t, tmax, deltat, spytim, spdate
  call window
  call stoptp
9800 nexmod = 2
9803 if ( iprspy .lt. 1 ) go to 9811
  write (munit6, 9807) jjroll, kbreak, lockbr, nchain, prom80(1:20)
9807 format (  ' exit "spying".  jjroll, kbreak, lockbr,', ' nchain =',  4i5,  '   prom80(1:20) =',  a20  )
  call window
9811 if ( prom80(1:8) .eq. '        ' )  go to 9833
  if ( kfile5 .eq. 1 ) go to 9833
  call prompt
9833 return
end subroutine spying
!
! subroutine spyink.
!
subroutine spyink
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive emtp only, which services "emtspy".
  !     this is the 2nd half of principle module "spying".
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'tacsar.ftn'
  include 'dekspy.ftn'
  character d13
  dimension mmhold(20)
  character*8 text1, text2, d12
  save
  data text1 /  6htamper  /
  if ( iprspy .lt. 1 ) go to 1003
  write (munit6, 1002) nchain, jjroll, kbreak, lockbr, nexmod
1002 format ( ' Enter "spying".  nchain  jjroll  kbreak', '  lockbr, nexmod =',  5i5 )
  call window
1003 if ( nexmod .ne. 3 ) go to 1004
  nexmod = 0
  go to nextsn
1004 if ( jword .gt. 63 )     go to 1007
  go to ( &
                                !       heading  stop    plot    help  examine  deposit  switch  (1-7)
       9999 ,   9999,   9999,   9999,   9999,   9999,   2804, &
                                !        rest    save   restore   go    blank    find    list    (8-14)
       9999 ,   9999,   9999,   9999,   9999,   9999,   9999, &
                                !         spy    break   when  comment    @?     roll   type?   (15-21)
       9999 ,   9999,   9999,   9999,   9999,   9999,   9999, &
                                !       verify   files   sleep  source   edit    wake  language (22-28)
       9999 ,   9999,   9999,   2856,   2617,   9999,   2782, &
                                !      catalog  begin    step    debug   data    ramp    time   (29-35)
       9999 ,   9999,   9999,   9999,   9999,   9999,   9999, &
                                !         tek   branch   yform    noy   factor    nof    rlc    (36-42)
       9999 ,   9999,   9999,   9999,   9999,   9999,   3456, &
                                !        width    bus    size   limit    iout    node   nonlin  (43-49)
       3526 ,   3541,   3635,   3647,   3662,   3684,   3752, &
                                !        space  lunit4  series   lock     [y]     [f]   noroll  (50-56)
       3842 ,   4002,   4163,   4436,   4471,   4512,   4563, &
                                !        open    close  unlock   honk   choice   tacs    wait   (57-63)
       9999 ,   9999,   9999,   9999,   4592,   4716,   9999), jword
  call stoptp
1007 n14 = jword - 63
  go to ( &
                                !         v-i                                                   (64-64)
       4823 ),  n14
  call stoptp
  !     $$$$$$$  key word no. 26:  "edit"     $$$$  $$$$  $$$$  $$$$  $$$$
  !     "edit"  command allows the user to look at the emtp data case
  !     which is stored in the rtm data base; display commands are
  !     similar to  vax/vms  editor  "sos" :
2617 linnow = 0
2618 write (prom80, 2621)
2621 format (' *')
  assign 2620 to nextsn
  go to 9800
2620 read (buff77, 2623)  char1, bytbuf
2623 format ( a1, a20 )
  if ( iprspy .lt. 1 )  go to 2626
  write (munit6, 2622)  char1,  bytbuf(ip:ip+9)
2622 format ( ' char1, bytbuf(1:10) after read:', a2, 1x, a10 )
  call window
2626 if ( char1  .eq.  'f' )   go to 2669
  if ( char1  .eq.  's'  )   go to 2669
  if ( char1  .eq.  'e' )    go to 1240
  if ( char1  .eq.  '8' )   go to 2734
  if ( char1  .eq.  'c' .and. bytbuf(1:1) .eq. 'o' .and. bytbuf(2:2)  .eq.  'l' )   go to 2756
  if ( char1  .eq.  'p' )   go to 2629
  if ( char1  .eq.  'd' )   go to 2629
  if ( char1  .eq.  'i' )   go to 2629
  if ( char1  .eq.  'r' )   go to 2629
  if ( char1  .eq.  ' '  )   go to 2629
  write (munit6, 42622)
42622 format (  '     sorry, 1st character is meaningless.', '   try again .... '     )
  call window
  go to 2618
2629 call sosrng ( n17 )
  if ( iprspy .lt. 2 )  go to 2633
  write (munit6, 2632)  n17, lidnt1, lidnt2
2632 format ( ' n17, lidnt1, lidnt2 =',  3i8 )
  call window
2633 if ( n17  .gt.  0 )   go to 2618
  if ( char1  .eq.  'i' )  go to 2695
  if ( char1  .eq.  'd' )   go to 2719
  if ( char1  .eq.  'r' )   go to 2719
  !     we reach here ready to print lines  (lidnt1, lidnt2) :  *p  case
2634 do j=lidnt1, lidnt2
     call quiter
     if ( kwtspy .eq. 0 ) go to 2639
     kwtspy = 0
     linnow = j - 1
     lidnt1 = j - 1
     lidnt2 = j - 1
     go to 2618
2639 write (munit6, 2667)  j, file6(j)
2667 format ( 1x, i4,  1x,  a80 )
     call window
2665 end do
  linspn = lidnt2 - lidnt1
  linnow = lidnt2
  go to 2618
  !     following code is for  "*f[string]@"  locate request (like sos):
2669 n4 = 0
  n5 = 0
  do i=2, 20
     if ( bytbuf(i:i) .eq.  '@' )   n4 = 1
     if ( bytbuf(i:i) .ne.  ' ' )   n5 = 1
2670 end do
  if ( n5  .eq.  0 )   go to 2671
  if ( n4  .eq.  1 )   go to 2673
  write (munit6, 32670)
32670 format ('  illegal "*f" --- string not terminated by "@".' )
  call window
  go to 2618
2671 if ( mstrng  .eq.  1 ) go to 2672
  write (munit6, 22671)
22671 format (  '  illegal "*f" --- blank string, but no', ' earlier usage for reference.'  )
  call window
  go to 2618
  !2672 call movers ( bytfnd(1), bytbuf(1), 20 )    ! restore old string
2672 bytbuf = bytfnd
  linnow = linnow + 1
2673 n6 = linnow
  do j=n6, numcrd
     l = 1
     do k=1, 80
        if ( file6(j)(k:k) .eq. bytbuf(l:l) )   go to 2674
        l = 1
        go to 2677
2674    if ( bytbuf(l+1:l+1) .ne. '@' )  go to 2676
        linnow = j
        lidnt2 = j
        if ( char1 .eq. 's' )  go to 2763
        go to 2691
2676    l = l + 1
2677 end do
2683 end do
  l = 0
  write (munit6, 2686)
2686 format (  ' string not found, search failed'  )
  call window
  linnow = linnow - 1
2691 if ( l .le. 0 )  go to 2693
  write (munit6, 2692)  j,  file6(j)
2692 format ( i5, 1x,  a80 )
  call window
2693 bytfnd = bytbuf
  mstrng = 1
  go to 2618
  !     following code is to process  "*i"  command:
  !     following code is for  "*i"  request :
2695 n1 = 0
  linnow = lidnt1
2701 if ( n1  .lt. 20 )   go to 2703
  write (munit6, 52701)
52701 format ( '    filled working storage of 20 lines.', '   truncated insertion now in progress.'  )
  call window
  go to 2704
2703 linnow = linnow + 1
  write (prom80, 2702)  linnow
2702 format ( 1x, i4,  1h   )
  assign 52703 to nextsn
  go to 9800
52703 if ( buff77(1:1) .ne. '@'  )  go to 2711
2704 j = numcrd
2706 file6(j+n1) = file6(j)
  j = j - 1
  if ( j .gt. lidnt1 )  go to 2706
  do j=1, n1
2708 file6(linnow+j) = file6b(j)
  end do
  linnow = linnow - 1
  lidnt1 = linnow
  lidnt2 = linnow
  numcrd = numcrd + n1/10
  go to 2618
2711 n1 = n1 + 1
  file6b(n1) = buff77
  go to 2701
  !     following code is for  "*d"  or  "*r"  request:
2719 n2 = lidnt1
  j  = lidnt2 + 1
2723 file6(n2) = file6(j)
  n2 = n2 + 1
  j = j + 1
  if ( j .le. numcrd )  go to 2723
  numcrd = numcrd - ( lidnt2 - lidnt1 + 1 )
  linnow = lidnt1 - 1
  lidnt1 = linnow
  lidnt2 = linnow
  if ( char1 .ne. 'r' )  go to 2618
  go to 2695
  !     following code is for  "*8"  request :
2734 if ( bytbuf(1:4)  .ne.  '    ' )  go to 2743
  j = linnow
2736 if ( file6(j)(80:80) .eq.  ' ' )  go to 2738
  write (munit6, 2692)  j,  file6(j)
  call window
  read (munit5, 2623)  bytbuf
  if ( bytbuf(1:1) .eq. 'e' )  go to 2618
  if ( bytbuf(1:1) .eq. ' ' )   go to 2738
  bytbuf(2:2) = file6(j)(80:80)
  if ( bytbuf(1:1) .eq. '0' ) file6b(j)(80:80) = ' '
  if ( bytbuf(1:1) .eq.  '1'   .or.  bytbuf(1:1) .eq.  '2'   .or.  bytbuf(1:1) .eq.  '3'   .or. bytbuf(1:1) .eq.  '4') file6b(j)(80:80) = bytbuf(1:1)
  if ( bytbuf(2:2) .ne. file6(j)(80:80)) go to 2738
  write (munit6, 2737)  bytbuf(1:1)
2737 format (  '   illegal col. 80 value  "',  a1, '" .   cancel substitution.'   )
  call window
2738 j = j + 1
  if ( j  .le.  numcrd )   go to 2736
  linnow = numcrd
  go to 2618
  !     following code is for  "*8,?"  case, to deposit in column 80:
2743 if ( bytbuf(1:1) .eq. ',' )  go to 2749
2744 write (munit6, 2746)
2746 format (   ' sorry,  "*8"  must be followed by a', ' comma and digit 1-4, or blanks.'     )
  call window
  go to 2618
2749 file6b(linnow)(80:80) = bytbuf(2:2)
  lidnt1 = linnow
  lidnt2 = linnow
  go to 2634
  !     following code is for  "*rule"  request for column marking:
2756 if ( bytbuf(3:3) .eq.  ' ' )  go to 2758
  write (munit6, 2759)
2759 format ( 1x,  '1234567890123456789012345678901234567890', '1234567890123456789012345678901234567890'  )
  call window
  go to 2761
2758 write (munit6, 2760)
2760 format ( 5x,  '1234567890123456789012345678901234567890', '1234567890123456789012345678901234567890'  )
  call window
2761 go to 2618
  !     the following is for  "*s"  processing, after branch from the
  !     middle of  "*f"  (where string was found):
2763 k = k + 1
  if ( iprspy .lt. 1 ) go to 32764
  write (munit6, 2764)  k, l
2764 format (   ' *s exited to 2763.  k, l =',  2i8  )
  call window
32764 n7 = k - l
  n12 = 81 - k
  buff77(1:n12) = file6(linnow)(k:80)
  buff77(n12+1:n12+20) = blan80(1:20)
  l = l + 2
  go to 2767
2765 file6(linnow)(n7:n7) = bytbuf(l:l)
  if ( n7 .eq. 80 )  go to 2781
  n7 = n7 + 1
  l = l + 1
  if ( l .gt. 20 )  go to 2769
2767 if ( bytbuf(l:l) .ne. '@' )  go to 2765
  n14 = 81 - n7
  file6(linnow)(n7:80) = buff77(1:n14)
  if ( iprspy .lt. 1 )  go to 2781
  write (munit6, 2768)
2768 format (   ' done with string sub.  exit to 2772.'  )
  call window
  go to 2781
2769 write (munit6, 2772)
2772 format (   '   illegal  "*s" ---- 2nd  "@"  missing.'  )
  call window
  go to 2618
2781 j = linnow
  if ( iprspy .lt. 1 )  go to 2691
  write (munit6, 62781)  j, n3, n4
62781 format (  ' at 2781.  j, n3, n4 =',  3i8  )
  call window
  go to 2691
  !     $$$$$$$  key word no. 28:  "language" $$$$  $$$$  $$$$  $$$$  $$$$
2782 write (prom80, 2785)
2785 format ( '   next command language operation (single,', ' entire, show, spy :'  )
  assign 2786 to nextsn
  go to 9800
2786 ansi8 = buff77(1:8)
  if ( ansi8 .ne. 'entire  ' ) go to 32788
  n13 = 1
  n14 = 10
32786 assign 2787 to nextsn
  go to 9800
2787 read (buff77, 2788) ( spykwd(j), j=n13, n14 )
2788 format ( 10a8 )
  if ( n14 .eq. numkey )  go to 2782
  n13 = n13 + 10
  n14 = n14 + 10
  if ( n14 .gt. numkey )  n14 = numkey
  go to 32786
32788 if ( ansi8 .ne. 'show    ' )   go to 2790
  do j=1, numkey, 10
     write (munit6, 2789) ( spykwd(ip), ip=j, j+9 )
2789 format ( 1x, 10a8 )
     call window
32789 end do
  go to 2782
2790 if ( ansi8 .ne. 'single  ' )  go to 2782
2791 write (prom80, 2792)
2792 format (   '      send next old and new symbols', ' as 2a8 (end) : '  )
  call prompt
  assign 2793 to nextsn
  go to 9800
2793 ansi8 = buff77(1:8)
  buff77(1:8) = buff77(9:16)
  if ( ansi8 .eq. 'end     ' )  go to 2782
  do j=1, numkey
     if ( ansi8 .eq. spykwd(j) )  go to 2798
2794 end do
  write (munit6, 2796)  ansi8
2796 format ( '    ? ? ?   sorry,  no such symbol found.', '   try again ...'  )
  call window
  go to 2791
2798 spykwd(j) = buff77(1:8)
  go to 2791
  !     $$$$$$$  key word no.  7:  "switch"   $$$$  $$$$  $$$$  $$$$  $$$$
2804 bytbuf(1:4) = answ80(1:4)
  n33 = 0
  if ( kswtch .gt. 0 ) go to 2808
  write (munit6, 2805)
2805 format ( '   ---  sorry, no such elements available in', ' this data case.  try another key word ...'  )
  call window
  go to 1240
2806 assign 2807 to nextsn
  go to 9800
2807 bytbuf = buff77(1:20)
2808 if ( bytbuf(1:4)  .eq.  'end ' )   go to 1240
  if ( bytbuf(1:4)  .eq.  'stop' )   go to 1240
  if ( bytbuf(1:4)  .ne.  'extr' )   go to 2814
2810 write (prom80, 2811)
2811 format ( 7h  name , 10x, 4hakey, 10x, 4hcrit, 8x, 6henergy, '  kswtyp  modswt  nextsw',  2h :   )
  n33 = 1
  go to 2806
2814 if ( bytbuf(1:4) .ne. spykwd(7)(1:4)  .and. bytbuf(1:4) .ne. spykwd(1)(1:4)  ) go to 2822
  write (prom80, 2815)
2815 format ( ' row name-k name-m k(kmswit)m kpos kentnb', 3x, 6htclose,  7x,  6hadelay,  7x, 6htopen:  )
  call prompt
  if ( n33  .gt.  0 )   go to 2810
  go to 2806
  !     we get to 2822 if no known key word;  extract integer pair.   this
  !     is dec free-format; "kswtch" is maximum;  "n17" is error flag:
2822 call intpar ( kswtch, n1, n2, n17 )
  if ( n17  .gt.  0 )   go to 2806
  do i=n1, n2
     call quiter
     if ( kwtspy .eq. 0 ) go to 2825
     kwtspy = 0
     go to 2806
2825 n5 = iabs ( kmswit(i) )
     n6 = iabs ( kmswit(lswtch+i)  )
     d12 = bus(n5)
     if ( n5 .eq. 1 ) d12 = terra
     d13 = bus(n6)
     if (n6 .eq. 1) d13 = terra
     write (munit6, 2828)  i, d12, d13, kmswit(i), kmswit(lswtch + i), kpos(i), kentnb(i), tclose(i), adelay(i), topen(i)
2828 format (1x, i3, 1x, a6, 1x, a6, i6, i5, i5, i7, 3e13.4)
     call window
     if (n33 .ne. 1) go to 2834
     n7 = namesw(i)
     write (munit6, 2831) texvec(n7), adelay(i), crit(i), energy(i), kswtyp(i), modswt(i), nextsw(i)
2831 format (1x, a6, 3e14.5, 3i8)
     call window
2834 end do
  go to 2806
  !     $$$$$$$  key word no. 25:  "source"   $$$$  $$$$  $$$$  $$$$  $$$$
2856 n33 = 0
2859 prom80 = '        '
  assign 42859 to nextsn
  go to 9800
42859 bytbuf = buff77(1:20)
  if ( bytbuf(1:4)  .eq.  'end ' )   go to 1240
  if ( bytbuf(1:4)  .eq.  'stop' )   go to 1240
  if ( bytbuf(1:4)  .ne.  'tacs'  )   go to 2862
  n33 = 1
  n14 = kxtcs + nuk
  write (munit6, 2860)
2860 format (  ' row  a6 tacs  source value', 3x, 12hdata field-a,  4x, 12hdata field-b, &
       4x, 12hdata field-!      )
  call window
  write (prom80, 2861)  n14, kud1, kud3, kud2
2861 format ( ' no.   name', 4x,  6hoffset, i6,  3x, 6hoffset, i6, 4x,  6hoffset, i6,  4x, 6hoffset, i6,  2h :  )
  go to 2859
2862 if ( bytbuf(1:4)  .ne.  'elec'  )   go to 2868
  !     "elec" is the command for a new heading for electric-network
  !     source table:
  n33 = 0
2863 write (prom80, 2865)
2865 format (  22h row  name  node iform,  6x, 5hcrest, 9x, 5hsfreq,  10x, 5htime1,  9x, 8htstart :  )
  go to 2859
2868 if ( n33  .eq.  1 )   go to 2884
  !     following is display code for electric-network source table:
  call intpar ( kconst, n1, n2, n17 )
  if ( n17 .gt. 0 )   go to 2859
  do i=n1, n2
     call quiter
     if ( kwtspy .eq. 0 ) go to 2873
     kwtspy = 0
     go to 2859
2873 n5 = iabs ( node(i) )
     write (munit6, 2878)  i, bus(n5), node(i), iform(i), crest(i), sfreq(i), time1(i), tstart(i)
2878 format ( 1x, i3,  1x, a6,  i5,  i6,  2e14.5,  2e15.6  )
     call window
2876 end do
  prom80(1:8) = blan80(1:8)
  go to 2859
  !     we reach 2884 with request to output another set of tacs sources:
2884 call intpar ( niu,  n1,  n2,  n17 )
  if ( n17  .gt.  0 )   go to 2859
  do i=n1, n2
     call quiter
     if ( kwtspy .eq. 0 ) go to 2887
     kwtspy = 0
     go to 2859
2887 ansi8 = blan80(1:8)
     if ( iuty(kiuty+i) .lt. 90 ) write (ansi8, 3691) sptacs(kaliu+i)
     write (munit6, 2891)  i,  ansi8,   sptacs(kxtcs+nuk+i), sptacs(kud1+i),  sptacs(kud3+i),  sptacs(kud2+i)
2891 format ( 1x,  i3,  1x, a6,  4e16.6  )
     call window
2888 end do
  prom80(1:8) = blan80(1:8)
  go to 2859
  !     $$$$$$$  key word no. 42:  "rlc"      $$$$  $$$$  $$$$  $$$$  $$$$
3456 bytbuf(1:4) = answ80(1:4)
  n33 = 0
  go to 3467
3462 assign 3464 to nextsn
  go to 9800
3464 bytbuf = buff77(1:20)
3467 if ( bytbuf(1:4)  .eq.  'end ' )   go to 1240
  if ( bytbuf(1:4)  .eq.  'stop' )   go to 1240
  if ( bytbuf(1:4)  .ne.  'extr' ) go to 3490
  n33 = n33 + 1
  if ( n33 .ge. 2 )  n33 = 0
3471 if ( n33 .eq. 1 ) write (prom80, 3478)
3478 format ( ' no extension yet in use.  but structure allows it.' )
  if ( n33 .eq. 0 )  write (prom80, 3488)
3488 format (  4h row,  9x, 5htr(i),  9x, 5htx(i),  10x, 4hr(i), 10x, 4hc(i),  10x, 5hx(i):  )
  go to 3462
3490 if ( bytbuf(1:4) .eq. spykwd(42)(1:4)  .or. bytbuf(1:4) .eq. spykwd(1)(1:4) ) go to 3471
  !     we get here if no known key word;  extract integer pair.   this
  !     is dec free-format; "it" is maximum;  "n17" is error flag:
  call intpar ( it, n1, n2, n17 )
  if ( n17  .gt.  0 )   go to 3462
  do i=n1, n2
     call quiter
     if ( kwtspy .eq. 0 ) go to 3497
     kwtspy = 0
     go to 3462
3497 write (munit6, 3503)  i, tr(i), tx(i), r(i), c(i), x(i)
3503 format ( 1x, i3,  5e14.5  )
     call window
3513 end do
  prom80(1:8) = blan80(1:8)
  go to 3462
  !     $$$$$$$  key word no. 43:  "width"    $$$$  $$$$  $$$$  $$$$  $$$$
3526 n13 = kol132
  if ( n13 .ne. 80 )  go to 3529
  write (munit6, 3527)
3527 format ( '   ---  switch from 80- to 132-column', ' lunit6 emtp output.'  )
  call window
  go to 3532
3529 write (munit6, 3528)
3528 format ( '   ---  switch from 132- to 80-column', ' lunit6 emtp output.'   )
  call window
3532 kol132 = 132
  if ( n13 .eq. 132 )  kol132 = 80
  go to 1240
  !     $$$$$$$  key word no. 44:  "bus"      $$$$  $$$$  $$$$  $$$$  $$$$
3541 bytbuf(1:4) = answ80(1:4)
  n33 = 0
  go to 3567
3562 assign 3564 to nextsn
  go to 9800
3564 bytbuf = buff77(1:20)
3567 if ( bytbuf(1:4)  .eq.  'end ' )   go to 1240
  if ( bytbuf(1:4)  .eq.  'stop' )   go to 1240
  if ( bytbuf(1:4)  .ne.  'extr' ) go to 3590
  n33 = n33 + 1
  if ( n33 .ge. 2 )  n33 = 0
3572 if ( n33 .eq. 1 ) write (prom80, 3578)
3578 format ( 5h  row,  8x,  5hfinit,   6x, 14hznonl-a kknonl, 6x, 14hznonl-b kknonl,  6x, 14hznonl-c kknonl ,  2h :  )
  if ( n33 .eq. 0 ) write (prom80, 3588)
3588 format (  ' node  name  kode  kssfrq    kk     kks', 12x, 4he(i),  12x, 6hf(i) :  )
  go to 3562
3590 if ( bytbuf(1:4)  .eq.  spykwd(44)(1:4)  .or. bytbuf(1:4)  .eq.  spykwd(1)(1:4) ) go to 3572
  !     we get here if no known key word;  extract integer pair.   this
  !     is dec free-format; "it" is maximum;  "n17" is error flag:
  call intpar ( ntot, n1, n2, n17 )
  if ( n17  .gt.  0 )   go to 3562
  do i=n1, n2
     call quiter
     if ( kwtspy .eq. 0 ) go to 3597
     kwtspy = 0
     go to 3562
3597 if ( n33 .eq. 0 ) write (munit6, 3603)  i, bus(i), kode(i), kssfrq(i), kk(i), kks(i), e(i), f(i)
3603 format ( 1x, i4,  2x, a6,  i4, i8, i6, i8,  2e16.6  )
     n22 = ntot + i
     n23 = ntot + n22
     if ( n33 .eq. 1 ) write (munit6, 3609)  i,  finit(i),  znonl(i), kknonl(i), znonl(n22), kknonl(n22),  znonl(n23), kknonl(n23)
3609 format ( 1x, i4, f13.4,  3( f14.5, i6)  )
     call window
3613 end do
  prom80(1:8) = blan80(1:8)
  go to 3562
  !     $$$$$$$  key word no. 45:  "size"     $$$$  $$$$  $$$$  $$$$  $$$$
3635 write (munit6, 3638)  ( lstat(i+20),  i=1, 10 )
3638 format (   14h   size  1-10:,  10i6  )
  call window
  write (munit6, 3639)  ( lstat(i+20), i=11, 20 )
3639 format (   14h   size 11-20:,  10i6  )
  call window
  write (munit6, 3640)  ( lstat(i+20), i=21, 27 )
3640 format (   14h   size 21-on:,  10i6   )
  call window
  go to 1240
  !     $$$$$$$  key word no. 46:  "limit"    $$$$  $$$$  $$$$  $$$$  $$$$
3647 write (munit6, 3649)  lbus, lbrnch, ldata, lexct, lymat, lswtch, lsize7, lpast, lnonl, lchar
3649 format (  15h   limit  1-10:,  10i6 )
  call window
  write (munit6, 3650)  lsmout, lsiz12, lfdep, lwt, ltails, limass, lsyn, maxpe, ltacst, lfsem
3650 format (  15h   limit 11-20:,  10i6 )
  call window
  write (munit6, 3651)  lfd, lhist, lsiz23, lcomp, lspcum, lsiz26, lsiz27
3651 format (  15h   limit 21-on:,  10i6 )
  call window
  go to 1240
  !     $$$$$$$  key word no. 47:  "iout"     $$$$  $$$$  $$$$  $$$$  $$$$
3662 write (prom80, 3664)  iout
3664 format (  '    send new printout frequency [', i5,  ' ] : '  )
  assign 3665 to nextsn
  go to 9800
3665 call frein1 ( buff77, n6 )
  limstp = 999999
  if ( n6 .le. 0 )  n6 = 1
  call copyi ( n6, multpr(1), 5 )
  n7 = istep - 1
  do j=1, 9999
     n7 = n7 + 1
     n8 = n7 / n6
     if ( n6*n8 .ne. n7 )  go to 3668
     if ( iprsup .lt. 1 )  go to 3667
     write (munit6, 3666)  n7
3666 format ( ' new round stop.  n7 =',  i8  )
     call window
3667 if ( n6 .lt. 20 )  n7 = n7 + n6
     if ( n7 .lt. istep+20 ) n7 = n7 + n6
     kprchg(2) = n7
     kprchg(5) = 999999
     kprchg(4) = 999999
     kprchg(3) = 999999
     go to 3675
3668 end do
3675 iout = n6
  if ( isprin .gt. iout ) isprin = iout
  limstp = n7
  indstp = 2
  if ( iprspy .lt. 1 ) go to 3679
  write (munit6, 3678) limstp, indstp, istep, iout, isprin
3678 format ( ' limstp, indstp, istep, iout, isprin =', 5i8 )
  call window
3679 go to 1240
  !     $$$$$$$  key word no. 48:  "node"     $$$$  $$$$  $$$$  $$$$  $$$$
3684 write (prom80, 3687)
3687 format (  '       send  a6  bus name (connect) :'  )
3688 assign 3690 to nextsn
  go to 9800
3690 spycd2 = buff77(1:35)
  if ( spycd2(1:8) .eq. 'stop    '   .or. spycd2(1:8) .eq. 'end     '   )  go to 1240
  if ( spycd2(1:8) .eq. 'connect '   )  go to 3701
  do j=1, ntot
     write (brobus, 3691)  bus(j)
3691 format ( a6, 2x )
     if ( brobus .ne. spycd2(1:8) )  go to 3693
     write (prom80, 3692)  j
3692 format (  '       >>> node =',  i4,  15x,  1h:  )
     go to 3688
3693 end do
  write (munit6, 3695)
3695 format ('        ????  sorry, no such bus.  try again ...'  )
  call window
  go to 3684
3701 mmfind = j
  n6 = 0
  do k=1, ibr
     if ( mmfind  .ne.  iabs(kbus(k))   .and. mmfind  .ne.  iabs(mbus(k)) )   go to 3705
     if ( n6 .lt. 20 ) go to 3703
     write (munit6, 3708)  ( mmhold(ip), ip=1, n6 )
     call window
     n6 = 0
3703 n6 = n6 + 1
     mmhold(n6) = k
3705 end do
  if ( n6 .le. 0 )  go to 3709
  write (munit6, 3708)  ( mmhold(k), k=1, n6 )
3708 format (  '    %% linear branches :',  20i5  )
  call window
3709 n6 = 0
  do k=1, kswtch
     if ( mmfind .ne. iabs(kmswit(k)) .and. mmfind .ne. iabs(kmswit(lswtch+k) ) )   go to 3719
     n6 = n6 + 1
     mmhold(n6) = k
3719 end do
  if ( n6 .le. 0 )  go to 3722
  write (munit6, 3721)  ( mmhold(k), k=1, n6 )
3721 format (  '    %% emtp  switches  :',  20i5  )
  call window
3722 n6 = 0
  do k=1, inonl
     if ( mmfind .ne. iabs(nonlk(k)) .and. mmfind .ne. iabs(nonlm(k)) )  go to 3726
     n6 = n6 + 1
     mmhold(n6) = k
3726 end do
  if ( n6 .le. 0 )  go to 3731
  write (munit6, 3730)  ( mmhold(k), k=1, n6 )
3730 format (  '    %% nonlinear elem. :',  20i5  )
  call window
3731 n6 = 0
  do k=1, kconst
     if ( mmfind .ne. iabs(node(k)) )  go to 3734
     n6 = n6 + 1
     mmhold(n6) = k
3734 end do
  if ( n6 .le. 0 )  go to 3739
  write (munit6, 3738)  ( mmhold(k), k=1, n6 )
3738 format (  '    %% emtp  sources  : ',  20i5  )
  call window
3739 mmfind = 0
  go to 3684
  !     $$$$$$$  key word no. 49:  "nonlin"   $$$$  $$$$  $$$$  $$$$  $$$$
3752 bytbuf(1:4) = answ80(1:4)
  n33 = 0
  if ( inonl .gt. 0 ) go to 3762
  write (munit6, 2805)
  call window
  go to 1240
3758 assign 3760 to nextsn
  go to 9800
3760 bytbuf = buff77(1:20)
3762 if ( bytbuf(1:4)  .eq.  'end ' )   go to 1240
  if ( bytbuf(1:4)  .eq.  'stop' )   go to 1240
  if ( bytbuf(1:4)  .ne.  'extr' )   go to 3774
3766 write (prom80, 3769)
3769 format (  9x, 5hanonl,  8x, 5hvzero,  8x, 5hvnonl, 9x, 4hcurr,  7x, 6hvecnl1,  7x,  7hvecnl1:   )
  n33 = 1
  go to 3758
3774 if ( bytbuf(1:4) .ne. spykwd(49)(1:4)  ) go to 3781
  write (prom80, 3777)
3777 format ( ' row  name  name-k name-m  nonlk  nonlm nltype', ' nonlad nonle ilast kupl nlsub :'  )
  call prompt
  if ( n33  .gt.  0 )   go to 3766
  prom80(1:8) = blan80(1:8)
  go to 3758
  !     we get to 3781 if no known key word;  extract integer pair.   this
  !     is dec free-format; "inonl" is maximum;  "n17" is error flag:
3781 call intpar ( inonl, n1, n2, n17 )
  if ( n17  .gt.  0 )   go to 3758
  do 3796  i=n1, n2
     call quiter
     if ( kwtspy .eq. 0 ) go to 3785
     kwtspy = 0
     go to 3758
3785 n5 = iabs ( nonlk(i) )
     d12 = bus(n5)
     if ( n5 .eq. 1 )  d12 = terra
     n6 = iabs ( nonlm(i) )
     d13 = bus(n6)
     if (n6 .eq. 1) d13 = terra
     n7 = namenl(i)
     write (munit6, 3786) i, texvec(n7), d12, d13, nonlk(i), nonlm(i), nltype(i), nonlad(i), nonle(i), ilast(i), kupl(i),  nlsub(i)
3786 format ( 1x, i3,  3( 1x, a6),  4i7, 2i6, i5, i6  )
     call window
     if ( n33 .ne. 1 ) go to 3796
     write (munit6, 3792)  anonl(i), vzero(i), vnonl(i), curr(i), vecnl1(i), vecnl2(i)
3792 format ( 1x,  6e13.4 )
     call window
3796 end do
  prom80(1:8) = blan80(1:8)
  go to 3758
  !     $$$$$$$  key word no. 50:  "space"    $$$$  $$$$  $$$$  $$$$  $$$$
3842 d14 = pltbuf(indbeg+1)
  kptplt = lstat(32)
  n77 = 0
  n22 = kptplt + 1
  write (munit6, 3843)
3843 format ( 2x, 40h current values.  indbuf  indbeg  limbuf, 32h  mflush  numdcd  numcrd  limcrd   )
  call window
  write (munit6, 3844)  indbuf, indbeg, limbuf, mflush, numdcd, numcrd, limcrd
3844 format ( 18x, 7i8 )
  call window
  write (munit6, 3845)  d14, t
3845 format ( 3x, 'plot timespan now in memory (in sec) =',  2e15.6 )
  call window
  if ( mflush .lt. 1000 )  go to 3848
3846 buff77(1:6) = 'flush '
  d17 = -1
  d18 = fltinf
  n4 = 1
  go to 3876
3848 if ( lastov .ne. 9911 ) go to 3851
  lastov = nchain - 1
  go to 3857
3851 write (prom80, 3854)
3854 format ( '    choose class (cards, plot) :'  )
  assign 3855 to nextsn
  go to 9800
3855 if ( buff77(1:4) .ne. 'plot' ) go to 3934
  !     begin logic to free some storage space for emtp plot data:
3857 write (prom80, 3858)
3858 format ( '   operation (write, thin, flush,', ' delete, auto, read, out, help) :'  )
  assign 43858 to nextsn
  go to 9800
43858 if ( buff77(1:6) .eq. 'delete' )  n77 = 1
  if ( buff77(1:6) .eq. 'read  ' )  n77 = 2
  if ( buff77(1:6) .eq. 'thin  ' )  n77 = 3
  if ( buff77(1:6) .eq. 'write ' )  n77 = 4
  if ( buff77(1:6) .eq. 'flush ' )  n77 = 5
  if ( buff77(1:4) .ne. 'help' )  go to 3861
  write (munit6, 3859)
3859 format ( '        to free memory for more simulation', ' plot points,  use one of these:'      )
  call window
  write (munit6, 33859)
33859 format ( '        either: 1) begin by saving', ' points on disk, prior to loss ("write");'  )
  call window
  write (munit6, 43859)
43859 format ( '                2) thin out present', ' storage by regular omission ("thin");'  )
  call window
  write (munit6, 53859)
53859 format ( '                3) delete older', ' points, and shift the remainder ("delete") :' )
  call window
  write (munit6, 3860)
3860 format ('                4) combined "write" and', ' "thin", with common iplot ("flush").'    )
  call window
  write (munit6, 23860)
23860 format ( '        send  "read"  to load memory', ' with plot data now on disk (lunit4).'  )
  call window
  write (munit6, 63860)
63860 format (  '        send  "out"  to switch from plot', ' file to cards,  or  "spy"  to abort.'  )
  call window
  write (munit6, 73860)
73860 format ( '        finally, send  "auto"  for automatic', ' full flush to disk from now on.'   )
  call window
  go to 3857
3861 if ( buff77(1:4) .ne. 'auto' )  go to 3862
  if ( mflush .lt. 1000 ) mflush = mflush + 1000
  go to 3846
3862 if ( buff77(1:4) .eq. 'out ') go to 3851
  write (prom80, 3863)
3863 format ( '     send [tmin, tmax]', ' (all, half) :'  )
  assign 3865 to nextsn
  go to 9800
3865 answ80 = buff77
  if ( answ80(1:4) .ne. 'all ' )  go to 3867
  d17 = -1.
  d18 = fltinf
  go to 3874
3867 if ( answ80(1:4) .ne. 'half' )  go to 3871
  n1 =  ( indbuf - indbeg ) / 2
  n2 = ( n1 / n22 ) * n22
  n3 = indbeg + n2 + 1
  d17 = -1.
  d18 = pltbuf(n3)
  if ( iprspy .ge. 1 ) write (*, 3869)  n22, n3, d18
3869 format ( ' "half" computed.  n22, n3, d18 =',  2i8, e14.5 )
  go to 3874
3871 call frefp2 ( answ80, d17, d18 )
3874 n4 = intinf
  if ( n77 .eq. 1 ) go to 3876
  write (prom80, 3875)
3875 format ( '      process every  n-th  step.  send n:'  )
  assign 43875 to nextsn
  go to 9800
43875 prom80 = buff77
  call frein1 ( prom80, n4 )
  if ( n4 .eq. 0 ) n4 = 1
  if ( n4 .lt. 0 )  go to 3857
  if ( n77 .eq. 2 )  go to 3915
3876 n13 = indbeg + 1
3877 if ( pltbuf(n13) .ge. d17 )  go to 3886
  n13 = n13 + n22
  if ( n13 .lt. indbuf )  go to 3877
  write (munit6, 3881)
3881 format ( '       ?? ??  sorry, no plot points in the', ' requested time span.  abort.'  )
  call window
  go to 1240
3886 if ( n77 .ne. 4   .and. n77 .ne. 5 )  go to 3904
  mflush = mflush + 1
  write (lunit6, 3891)  mflush, n6, n4, d17, d18
3891 format ( '   ++++  begin plot-data copy from', ' memory to disk.   mflush, n6,', &
       ' n4, tbeg, tend =',  i3, 2i8, 2e14.5  )
  go to 3906
3904 write (lunit6, 3905)  n4, d17, d18
3905 format ( '   ****  begin thinning of plot data',' points, to reduce memory burden.', &
       '   n4, tbeg, tend =',  i10, 2e14.5  )
3906 ip = 0
  n8 = n13 + kptplt
  n6 = n13
  do l=1, 999999
     if ( pltbuf(n13) .gt. d18 ) go to 3911
     ip = ip + 1
     if ( ip .lt. n4 ) go to 3908
     ip = 0
     if ( n77 .ne. 4   .and. n77 .ne. 5 )  go to 3907
     write (lunit4)  ( pltbuf(m), m=n13, n8 )
     if ( n77 .eq. 4 )  go to 3908
3907 call movesp(pltbuf(n13), pltbuf(n6), n22)
     n6 = n6 + n22
3908 n13 = n13 + n22
     if ( n13 .ge. indbuf )  go to 3911
3910 n8 = n8 + n22
  end do
3911 if ( n77 .eq. 4 )  go to 3914
  n14 = indbuf - n13 + 1
  if ( n14 .gt. kptplt ) call movesp(pltbuf(n13), pltbuf(n6), n14)
  indbuf = n6 + n14 - 1
  iascii(1000) = n13 - n6
  if ( n77 .ne. 5 ) go to 3914
  n5 = limbuf - indbuf
  write (lunit6, 3913)  n5
3913 format ( 9x,  'completed "flush", leaving', i8,    '  free cells for new plot points.'  )
  if ( mflush .ge. 1000 ) go to 1240
3914 go to 3857
  !     begin "read" servicing (plot data goes disk to memory):
3915 write (prom80, 3916)
3916 format ( '      destination (0--start; 1--as now) :'  )
  assign 3917 to nextsn
  go to 9800
3917 prom80 = buff77
  call frein1 ( prom80, n3 )
  n13 = indbuf
  if ( n3 .eq. 0 )  n13 = indbeg
  ip = 0
  n8 = n13 + kptplt
  n11 = 0
  do j=1, 999999
     ip = ip + 1
     if ( ip .ge. n4 )  go to 3919
     read (lunit4, end=3926) d15
     if ( d15 .gt. d18 )  go to 3928
     go to 3921
3919 read (lunit4, end=3926) ( pltbuf(l), l=n13, n8 )
     if ( pltbuf(n13) .gt. d18 )  go to 3934
     n11 = n11 + 1
3921 n8 = n8 + n22
     if ( n8 .le. limbuf )  go to 3925
     write (munit6, 3923)  pltbuf(n13)
3923 format ( '   ###  squish.  memory space for plot data', ' has run out after storing t=',  e14.5  )
     call window
     go to 3928
3925 n13 = n13 + n22
  end do
  j = 999999
  go to 3928
3926 write (munit6, 3927)  d18
3927 format ( '     eof during read (lunit4).  quit search', ' for tend =',  e13.4  )
  call window
3928 indbuf = n13
  n6 = limbuf - indbuf
  write (munit6, 3931)  j, n11, n6
3931 format ( '     steps read and retained =',  i7, i6, '   remaining free words =',  i8  )
  call window
  go to 3857
  !     begin "cards" servicing (file6 character storage):
3934 write (prom80, 3937)
3937 format ( '   choose operation (move, blank,', ' out, space) :'  )
  assign 3938 to nextsn
  go to 9800
3938 if ( buff77(1:4) .eq. 'out '   )  go to 3851
  if ( buff77(1:6) .eq. 'space ' )  go to 3851
  n66 = 0
  if ( buff77(1:6) .eq. 'blank ' )  n66 = 1
  write (prom80, 3941)
3941 format ( '      number of cards to be handled :'  )
  assign 3942 to nextsn
  go to 9800
3942 call frein1 ( buff77, n11 )
  if ( n11 .le. 0 )  go to 3851
  if ( n66 .eq. 1 )  go to 3955
  write (prom80, 3944)
3944 format ( '      card addresses n-from and n-to :'  )
  assign 3947 to nextsn
  go to 9800
3947 call frein1 ( buff77, n11 )
  if ( n17 .le. 0   )  go to 3934
  if ( n18 .lt. n17 )  go to 3934
  do j=1, n11
     file6(n18) = file6(n17)
     n17 = n17 + 1
3952 n18 = n18 + 1
  end do
  go to 3934
3955 write (prom80, 3956)
3956 format ( '      card address :'  )
  assign 3959 to nextsn
  go to 9800
3959 call frein1 ( buff77, n17 )
  if ( n17 .le. 0 )  go to 3851
  do j=1, n11
     file6(n17) = blan80
3962 n17 = n17 + 1
  end do
  go to 3934
  !     $$$$$$$  key word no. 51:  "lunit4"   $$$$  $$$$  $$$$  $$$$  $$$$
4002 forbyt(1) = -fltinf
  kptplt = lstat(32)
  d24 = fltinf
4008 write (prom80, 4010)
4010 format ( '   operation (open, close,', ' top, bot, next, back, time) :'  )
  assign 4011 to nextsn
  go to 9800
4011 if ( buff77(1:5) .ne. 'close' )  go to 4022
  write (prom80, 4013)
4013 format ( '     save permanently? (y or n) :'  )
  assign 4016 to nextsn
  go to 9800
4016 ansi8 = 'keep    '
  if ( buff77(1:1) .eq. 'n' )  ansi8 = 'delete  '
  close (unit=lunit4, status=ansi8)
  go to 4008
4022 if ( buff77(1:5) .ne. 'open ' )  go to 4036
  write (prom80, 4025)
4025 format ( '     desired disk file name :'  )
  assign 4026 to nextsn
  go to 9800
4026 ansi32 = buff77(1:32)
  write (prom80, 4028)
4028 format ( '     desired status (new, old) :'  )
  assign 4029 to nextsn
  go to 9800
4029 open (unit = lunit4, status = buff77, file = ansi32, form = 'unformatted' )
  go to 4008
4036 if ( buff77(1:4) .ne. 'top ' )  go to 4051
4037 rewind lunit4
  read (lunit4)  date1, tclock, n1, n2, n3, n4
  if ( nchain .le. 16  .or. nchain .gt. 20 ) go to 4044
  if ( n2 .eq. numnvo  .and. n3 .eq. nc-nv   .and. n4 .eq. nc )  go to 4044
  write (munit6, 4038)
4038 format ( '   ++++  error.  inconsistent lunit4 plot', ' file.  automatic close.  try again.'   )
  call window
  write (munit6, 4039)  date1, tclock, n1, n2, n3, n4
4039 format (  '         date1, tclock =', 2( 1x, 2a4), 5x,  'n1:n4 =',  4i8  )
  call window
  close (lunit4)
  go to 4008
4044 if ( n2 .gt. 0 )  read (lunit4)  n5
  if ( n4 .gt. 0 )  read (lunit4)  n6
  kptplt = numnvo + nc
  if ( buff77(1:5) .eq. 'time ' ) go to 24085
  go to 4008
4051 if ( buff77(1:4) .ne. 'bot ' )  go to 4068
  n15 = 0
  do k=1, 100
     do j=1, 1000
4053    read (lunit4, end=4062)  forbyt(1)
     end do
     n15 = n15 + 1000
     write (munit6, 4056)  n15
4056 format ( '+      still reading.',  i8  )
     call window
4059 end do
  go to 4008
4062 n15 = n15 + j
  write (munit6, 4063)  forbyt(1), n15
4063 format ( '       ok, at end lunit4.  t-end =', e15.6,  '   steps taken =',  i8  )
  call window
  go to 4008
4068 if ( buff77(1:4) .ne. 'next' )  go to 4079
4069 read (lunit4, end=4075)  ( forbyt(j), j=1, kptplt+1 )
  write (munit6, 4071)     ( forbyt(j), j=1, kptplt+1 )
4071 format ( 1x, 10e13.4 )
  call window
  if ( forbyt(1) .lt. d24 )  d24 = forbyt(1)
  go to 4008
4075 write (munit6, 4076)
4076 format ( '     ===   sorry,  no more  lunit4  data.', '   eof hit.'  )
  call window
  go to 4008
4079 if ( buff77(1:4) .ne. 'back' )  go to 4083
  if ( noback .ne. 1 )  go to 4082
  write (munit6, 4080)
4080 format ( '    ok,  but wait for  "top"  and  "time"', '  using  t = t - 2*deltat'  )
  call window
  buff77(1:5) = 'time '
  d8 = forbyt(1) - 2 * deltat
  go to 4037
4082 backspace lunit4
  read (lunit4, end=4075)   ( forbyt(j), j=1, kptplt+1 )
  write (munit6, 4071)      ( forbyt(j), j=1, kptplt+1 )
  call window
  if ( forbyt(1) .gt. d24 ) backspace lunit4
  go to 4008
4083 if ( buff77(1:4) .ne. 'time' )  go to 4098
  write (prom80, 4084)  forbyt(1)
4084 format ( '       send desired time [',  e15.6,  ' ] :'  )
  assign 4085 to nextsn
  go to 9800
4085 call frefp1 ( buff77, d8 )
24085 n15 = 0
  do k=1, 100
     do j=1, 1000
        read (lunit4, end=4090) forbyt(1)
        if ( forbyt(1) .ge. d8 ) go to 4094
4086 end do
     n15 = n15 + 1000
     write (munit6, 4056)  n15
     call window
4089 end do
  go to 4008
4090 n15 = n15 + j
  write (munit6, 4091)   n15
4091 format (  '      ? ? ?   search fails with eof after', i8,  '   steps.'  )
  call window
  go to 4008
4094 n15 = n15 + j
  write (munit6, 4095)  forbyt(1), n15
4095 format ( '      ok, record of  t =',  e15.6, '  just read on try number',  i7  )
  call window
  go to 4008
  !     put any later operation choices here and below:
4098 write (munit6, 2796)
  call window
  go to 4008
  !     $$$$$$$  key word no. 52:  "series"   $$$$  $$$$  $$$$  $$$$  $$$$
4163 if ( nchain .le. 11 ) kbrser = 1
  n42 = lstat(22)
  n43 = lstat(23)
  if ( kbrser .ne. 2 )  go to 4165
  write (munit6, 4164)
4164 format ( '   @ @ @ @ @ @   ok, guy, this is the', ' "over12" break for "series" usage.'   )
  call window
  kbrser = 0
4165 write (prom80, 4166)
4166 format ( '  operation (show, extra, change,', ' step, rewind) :'  )
  assign 4169 to nextsn
  go to 9800
4169 ansi32 = buff77(1:32)
  if ( ansi32(1:6) .ne. 'rewind' )  go to 4172
  kserlc = 0
  lserlc = 0
  go to 4165
4172 if ( ansi32(1:5) .ne. 'show ' )  go to 4184
  write (munit6, 4174)
4174 format (  '  memory:  list-2 ---',  2i5, '     list-3 ---',  2i5,  '    lserlc =',  i4  )
  call window
  write (munit6, 4175)  n42, lbrnch, n43, ldata, lserlc
4175 format ( ' num row use  bus-k  bus-m   name ????', 5x, 9hpresent-r,  5x, 9hpresent-l,  5x, 9hpresent-c  )
  call window
  do j=1, lserlc
     ndx1 = n42 + j
     ndx2 = n43 + j
     k = kbus(ndx1)
     if ( k .eq. 0 )  go to 4180
     m = mbus(ndx1)
     ndx3 = namebr(ndx1)
     write (munit6, 4179)  j, litype(ndx1), nr(ndx1), bus(k), bus(m), namebr(ndx3),  ci(ndx1), ck(ndx1), cik(ndx1)
4179 format ( 1x, i3, 2i4, 3( 1x, a6 ), 5x, 3e13.4 )
     call window
4180 end do
  go to 4165
4184 if ( ansi32(1:5) .ne. 'extra' )  go to 4197
  write (munit6, 4187)
4187 format ( 4x, 10hstarting-r, 3x,  10hstarting-l, 3x, 10hstarting-c, 7x, 6hnext-r,  7x, 6hnext-l,  7x, 6hnext-c  )
  call window
  do j=1, lserlc
     ndx1 = n42 + j
     if ( kbus(ndx1) .eq. 0 )  go to 4192
     ndx2 = n43 + j
     write (munit6, 4191)  cki(ndx1), ckkjm(ndx1), r(ndx2), tr(ndx2), tx(ndx2), c(ndx2)
4191 format ( 1x, 6e13.4 )
     call window
4192 end do
  go to 4165
4197 if ( ansi32(1:5) .ne. 'step ' )  go to 4211
  kserlc = 1
  go to 4165
4211 if ( ansi32(1:6) .ne. 'change' )  go to 4165
4214 write (prom80, 4215)
4215 format ( '    change type (data, move, blank, use, value,', ' end) :'  )
  assign 34215 to nextsn
  go to 9800
34215 ansi32 = buff77(1:32)
  if ( ansi32(1:4) .eq. 'end ' ) go to 4165
  if ( ansi32(1:6) .eq. 'series')go to 4165
  if ( ansi32(1:5) .ne. 'move ' )  go to 4223
4216 write (prom80, 4217)
4217 format ( '    send  n-from, n-to (1, 2, ...) :'  )
  assign 4219 to nextsn
  go to 9800
4219 bytbuf = buff77(1:20)
  if ( bytbuf(1:4) .eq. 'end ' ) go to 4214
  call frein2(bytbuf, n13, n14)
  if ( n14 .gt. lserlc )  lserlc = n14
  n13 = n13 + n42
  n14 = n14 + n42
  kbus(n14) =  kbus(n13)
  mbus(n14) =  mbus(n13)
  n24 = 0
  call namea6 ( text1, n24 )
  namebr(n14) = n24
  litype(n14) = litype(n13)
  imodel(n14) = imodel(n13)
  nr(n14) = nr(n13)
  ci(n14)  = ci(n13)
  ck(n14)  = ck(n13)
  cik(n14) = cik(n13)
  cki(n14) = cki(n13)
  ckkjm(n14) = ckkjm(n13)
  n13 = n13 - n42 + n43
  n14 = n14 - n42 + n43
  tr(n14) = tr(n13)
  tx(n14) = tx(n13)
  r(n14)  = r(n13)
  c(n14)  = c(n13)
  go to 4216
4223 if ( ansi32(1:5) .ne. 'data ' )  go to 4232
4225 write (prom80, 4217)
  assign 4227 to nextsn
  go to 9800
4227 bytbuf = buff77(1:20)
  if ( bytbuf(1:4) .eq. 'end ' ) go to 4214
  call frein2(bytbuf, n13, n14)
  if ( n14 .gt. lserlc )  lserlc = n14
  n14 = n14 + n42
  kbus(n14) = iabs ( kbus(n13) )
  mbus(n14) = iabs ( mbus(n13) )
  nr(n14) = 0
  n24 = 0
  call namea6 ( text1, n24 )
  namebr(n14) = n24
  litype(n14) = n13
  n13 = iabs ( nr(n13) )
  cki(n14)   = tr(n13)
  ckkjm(n14) = tx(n13)
  ci(n14)    = tr(n13)
  ck(n14)    = tx(n13)
  cik(n14)   = c(n13)
  n14 = n14 - n42 + n43
  tr(n14)    = tr(n13)
  tx(n14)    = tx(n13)
  c(n14)     = c(n13)
  r(n14)     = c(n13)
  go to 4225
4232 if ( ansi32(1:5) .ne. 'blank' )  go to 4245
4235 write (prom80, 4236)
4236 format ( '    send n-beg and n-end line numbers :'   )
  assign 4237 to nextsn
  go to 9800
4237 bytbuf = buff77(1:20)
  if ( bytbuf(1:4) .eq. 'end ' ) go to 4214
  call frein2(bytbuf, n13, n14)
  if ( n14 .lt. lserlc )  go to 4239
  lserlc = n13 - 1
  go to 4235
4239 do j=n13, n14
4241 kbus(j+n42) = 0
  end do
  go to 4235
4245 if ( ansi32(1:4) .ne. 'use ' )  go to 4256
  write (munit6, 4248)  ( nr(j+n42), j=1, lserlc )
4248 format ( '    nr:',  10i7  )
  call window
4251 write (prom80, 4253)
4253 format ( '    send number to toggle (0=end) :'  )
  assign 4254 to nextsn
  go to 9800
4254 if ( buff77(1:4) .eq. 'end ' ) go to 4214
  call frein1 ( buff77, n6 )
  if ( n6 .le. 0 )  go to 4214
  n7 = n6 + n42
  nr(n7) = nr(n7) + 1
  if ( nr(n7) .ge. 2 )  nr(n7) = 0
  go to 4251
  !     place any additional subcommands here:
4256 go to 4214
  !     $$$$$$$  key word no. 53:  "lock"     $$$$  $$$$  $$$$  $$$$  $$$$
4436 lockbr = 1
  go to 1240
  !     $$$$$$$  key word no. 54:  "[y]"      $$$$  $$$$  $$$$  $$$$  $$$$
4471 j = 1
4473 write (prom80, 4474)
4474 format ( '   send row name or number (end) :'  )
  assign 54474 to nextsn
  go to 9800
54474 prom80 = buff77
  if ( prom80(1:4) .eq. 'end ' ) go to 1240
  if ( prom80(1:6) .ne. '      '  .and. prom80(1:6) .ne. 'next  ' )  go to 4475
  j = j + 1
  go to 4494
4475 if ( prom80(1:4) .ne. 'top ' ) go to 4476
  j = 2
  go to 4494
4476 if ( prom80(1:4) .ne. 'bot ' ) go to  4477
  j = -9999
  go to 4494
4477 do j=1, 10
     if ( prom80(1:1) .eq. digit(j) )  go to 4491
4478 end do
  read (prom80, 1380)  text2
1380 format ( a6 )
  do j=1, kpartb
     if (text2 .eq. bus(j)) go to 4494
4483 end do
4486 write (munit6, 4487)
4487 format ( '  %% %% == ++  sorry, no such row.  try again.' )
  call window
  go to 4473
4491 call frein1 ( prom80, j )
4494 if ( answ80(1:3) .eq. '[f]' ) go to 4515
  if ( j .le. 1 )  j = kpartb
  if ( j .gt. kpartb )  j = 2
  n17 = kks(j) - 1
  n16 = kks(j+1)
  if ( j .eq. kpartb )  n16 = kks(1)
  write (munit6, 4496)  j, bus(j), n16, n17
4496 format ( ' row',  i4,  '   name = ',  a6,  '   limits =', 2i5,  '    nonzero  (m, ykm)  follow ....'  )
  call window
  do l=n16, n17, 4
     n23 = l + 3
     if ( n23 .gt. n17 )  n23 = n17
     write (munit6, 4497)   ( km(m), ykm(m), m=l, n23 )
4497 format ( 1x,  4( i5, e15.6 )  )
     call window
4502 end do
  go to 4473
  !     $$$$$$$  key word no. 55:  "[f]"      $$$$  $$$$  $$$$  $$$$  $$$$
4512 go to 4473
4515 if ( j .le. 1 )  j = kpartb
  if ( j .gt. kpartb )  j = 2
  n17 = kk(j-1) + 1
  write (munit6, 4521)  j, bus(j), n17, kk(j)
4521 format ( ' row',  i4,  '   name = ',  a6,  '   limits =', 2i5,  '    nonzero  (m, fkm)  follow ....'  )
  call window
  do l=n17, kk(j), 4
     n23 = l + 3
     if ( n23 .gt. kk(j) )  n23 = kk(j)
     write (munit6, 4497)   ( km(m), ykm(m), m=l, n23 )
     call window
4524 end do
  go to 4512
  !     $$$$$$$  key word no. 56:  "noroll"   $$$$  $$$$  $$$$  $$$$  $$$$
4563 if ( monits .eq. 0 ) go to 4571
  monits = 0
  write (munit6, 4566)
4566 format ( 6x, '===  cancel previous rolling printer plot.' )
  call window
4571 if ( monitr .eq. 0 ) go to 1240
  monitr = 0
  write (munit6, 4573)
4573 format ( 6x, '===  cancel previous rolling vector plot.' )
  call window
  go to 1240
  !     $$$$$$$  key word no. 61:  "choice"   $$$$  $$$$  $$$$  $$$$  $$$$
4592 n17 = nc - nv
  n24 = numnvo + nc + nsmout + ioutcs + numout
  write (munit6, 4595)  n24
4595 format ( '     total number of emtp outputs (sum of 5', ' classes) =',  i5   )
  call window
  write (munit6, 4596)
4596 format ( '     ordered class limits :   numnvo      nv', '   nc-nv  nsmout  ioutcs   numout'  )
  call window
  write (munit6, 4597) numnvo, nv, n17, nsmout, ioutcs, numout
4597 format ( 28x, 6i8 )
  call window
  write (munit6, 4608) numnvo
4608 format ( '     selective node voltage outputs (cells  1', '  through',  i3,  ' ) :'   )
  call window
  do j=1, numnvo, 7
     n17 = j + 6
     if ( n17 .gt. numnv0 )  n17 = numnvo
     write (munit6, 4609)  ( bus(ibsout(m)), m=j, n17 )
4609 format ( 7x, 10a7 )
     call window
4611 end do
  n7 = numnvo + 1
  n8 = numnvo + nv
  if ( n8 .lt. n7 )  go to 4631
  write (munit6, 4615)  n7, n8
4615 format ( '     branch voltage node-name pairs (cells', i3,   '  through',  i3,  ' ) :' )
  call window
  n9 = 7878
  call spyout ( n9, n9 )
  do j=1, nv
4623 call spyout ( ibrnch(j), jbrnch(j) )
  end do
  n9 = -7878
  call spyout ( n9, n9 )
4631 n7 = numnvo + nv + 1
  n8 = numnvo + nc
  write (munit6, 4636)  n7, n8
4636 format ( '     element current node-name pairs (cells', i3,  '  through',  i3,  ' ) :'  )
  call window
  if ( n17 .le. 0 ) go to 4658
  n9 = 7878
  call spyout ( n9, n9 )
  do j=1, kswtch
     if ( kmswit(lswtch+j) .gt. 0 )  go to 4639
     call spyout ( kmswit(j), kmswit(lswtch+j) )
4639 end do
  do j=1, inonl
     if ( nonlm(j) .gt. 0 )  go to 4644
     call spyout ( nonlk(j), nonlm(j) )
4644 end do
  do j=1, ibr
     if ( mbus(j) .gt. 0 )  go to 4649
     call spyout ( kbus(j), mbus(j) )
4649 end do
  n9 = -7878
  call spyout ( n9, n9 )
4658 go to 1240
  !     $$$$$$$  key word no. 62:  "tacs"     $$$$  $$$$  $$$$  $$$$  $$$$
4716 write (prom80, 4717)
4717 format ( ' send control (rewind, source,', ' supplemental, patch, show, end) :'  )
  assign 4719 to nextsn
  go to 9800
4719 prom80 = buff77
  if ( prom80(1:4) .eq. 'end ' ) go to 1240
  if ( prom80(1:6) .ne. 'rewind' )  go to 4724
  niu = konsce
  nsup = konsup
  ktab = kofsce
  koncur = 0
  go to 4716
4724 if ( prom80(1:6) .ne. 'source' )  go to 4726
  koncur = 1
  go to 4734
4726 if ( prom80(1:6) .ne. 'supple' )  go to 4752
  koncur = 2
4734 n23 = lunit5
  lunit5 = munit5
  !     read input data card using cimage.
4738 call cimage
  read (unit = abuff, fmt = 2788) ansi8
  if ( ansi8(1:4) .ne. 'end ' ) go to 4741
  lunit5 = n23
  go to 4716
4741 if ( ansi8(1:4) .ne. 'spy ' ) go to 4745
  lunit5 = n23
  go to 1240
4745 if ( koncur .eq. 1 )  call tacs1
  if ( koncur .eq. 2 )  call tacs1a
  go to 4738
4752 if ( prom80(1:6) .ne. 'patch ' )  go to 4792
  n11 = locint ( kar1(1) )
  n12 = locint ( volti(1) )
  n13 = ( n12 - n11 ) / 2
  if ( iprspy .le. 1 )  go to 34753
  write (munit6, 4753)  n11, n12, n13
4753 format ( ' volti memory indices.  n11, n12, n13 =', 3i8,  '    fkar1(j), j=n13+1, n13+9) ....'  )
  call window
  write (munit6, 24753)   (fkar1(j), j=n13+1, n13+9)
24753 format ( 9e14.4 )
  call window
34753 n14 = 0
4754 write (prom80, 4757)
4757 format ( ' send source name, address (a6, i6), or "end":' )
  assign 4759 to nextsn
  go to 9800
4759 prom80 = buff77
  if ( prom80(1:4) .eq. 'end ' ) go to 4776
  read (prom80, 4763)  bus1, n7
4763 format ( a6, i6, e8.0 )
  do j=1, niu
     ndx3 = ilntab( kaliu + j )
     bus2 = texvec(ndx3)
     if ( bus1 .ne. bus2 ) go to 4769
     n14 = n14 + 1
     ndx4 = j + kxtcs + nuk
     kontac(n14) = ndx4
     epskon(n14) = 0.0
     konadd(n14) = n7
     go to 4754
4769 end do
  write (munit6, 4771)
4771 format ( '  ? ? ?   sorry, no such tacs source named  "', a6,  '".  try again ....'  )
  call window
  go to 4754
4776 write (prom80, 4777)
4777 format ( ' send usage name, address, tolerance', ' (a6, i6, e8.0), or "end":'  )
  assign 4778 to nextsn
  go to 9800
4778 prom80 = buff77
  if ( prom80(1:4) .ne. 'end ' ) go to 4780
  kontot = n14
  go to 4716
4780 read (prom80, 4763)  bus1, n7, d13
  do j=1, ktab
     ndx1 = kxtcs + j
     ndx2 = ilntab(klntab+j)
     if ( bus1 .ne. texvec(ndx2) ) go to 4785
     if ( koncur .eq. 0 )  koncur = n14
     n14 = n14 + 1
     kontac(n14) = ndx1
     konadd(n14) = n7
     epskon(n14) = ichar(d13)
     go to 4776
4785 end do
  write (munit6, 4788)
4788 format ( '  ? ? ?   sorry, no such tacs output named  "', a6,  '".  try again ....'  )
  call window
  go to 4776
4792 if ( prom80(1:5) .ne. 'show ' )  go to 4807
  write (munit6, 4794) koncur, kontot
4794 format ( ' concurrent sequential processing (csp)', ' controls.  koncur =',  i4,  5x,  'kontot =', i4 )
  call window
  write (munit6, 4795)
4795 format ( '     row    name   index  memory    threshold'  )
  call window
  do i=1, kontot
     n13 = ilntab( kontac(i) + klntab - kxtcs )
     write (munit6, 4798)  i, texvec(n13), kontac(i), konadd(i), epskon(i)
4798 format ( i8, 2x, a6, 2i8, e13.4 )
     call window
4801 end do
  !     any additional responses to tacs prompt go here:
4807 go to 4716
  !     $$$$$$$  key word no. 64:  "v-i"      $$$$  $$$$  $$$$  $$$$  $$$$
4823 n17 = 0
  n8 = 0
  write (munit6, 4825)  inonl, i_char
4825 format ( ' lists 9, 10 =',  2i4, '      (next, <cr>, last, all, mode).' )
  call window
4824 write (prom80, 4827)
4827 format ( 1x, ' row        vchar        cchar       gslope', ' row nltype nonlad nonle  class :'  )
  assign 4828 to nextsn
  go to 9800
4828 bytbuf = buff77(1:20)
  if ( bytbuf(1:4)  .eq.  'end ' )   go to 1240
  if ( bytbuf(1:4)  .eq.  'stop' )   go to 1240
  if ( bytbuf(1:4)  .ne.  'next'  .and. bytbuf(1:4)  .ne.  '    ' )  go to 4829
  n17 = n17 + 1
  if ( n17 .gt. inonl )  n17 = 1
  go to 4844
4829 if ( bytbuf(1:4) .ne. 'last' )  go to 4831
  n17 = n17 - 1
  if ( n17 .le. 0 )  n17 = inonl
  go to 4844
4831 if ( bytbuf(1:4) .ne. 'all ' )  go to 4832
  n1 = 1
  n2 = i_char
  go to 4854
4832 if ( bytbuf(1:4) .ne. 'mode' )  go to 4834
  n8 = n8 + 1
  if ( n8 .ge. 2 )  n8 = 0
  write (munit6, 4833)  n8
4833 format ( '    ----  new mode flag n8 =',  i2 )
  call window
  go to 4824
4834 call frein2(bytbuf, n1, n2)
  if ( n8 .eq. 1 )  go to 4854
  do n17=1, inonl
     if ( iabsz(nonlad(n17)) .lt. n1 )  go to 4837
     if ( iabsz(nonlad(n17)) .le. n2 )  go to 4844
4837 end do
  go to 4824
4844 n1 = iabsz ( nonlad(n17) )
  n2 = iabsz ( nonle(n17) )
  ansi8(1:6) = 'pseudo'
  if ( nltype(n17) .gt. 0 )  ansi8(1:6) = ' true '
  write (munit6, 4851) n1, vchar(n1), cchar(n1), gslope(n1), n17, nltype(n17), nonlad(n17), nonle(n17), ansi8(1:6)
4851 format ( 1x, i4, 3e13.4, i4, 2i7, i6, 1x, a6 )
  call window
  n1 = n1 + 1
4854 do j=n1, n2
     write (munit6, 4851)  j, vchar(j), cchar(j), gslope(j)
     call window
     call quiter
     if ( kwtspy .eq. 0 ) go to 4858
     kwtspy = 0
     go to 4824
4858 end do
  go to 4824
1240 nexmod = 0
  go to 9804
9800 nexmod = 3
9804 if ( iprspy .lt. 1 ) go to 9999
  write (munit6, 9807) jjroll, kbreak, lockbr, nchain, prom80(1:20)
9807 format (  ' exit "spyink".  jjroll, kbreak, lockbr,', ' nchain =',  4i5,  '   prom80(1:20) =',  a20  )
  call window
9999 return
  entry spytac
  !     called only by "subts3", once, for tacs csp application.
  do j=1, koncur
     n7 = kontac(j)
     n13 = konadd(j)
3472 xtcs(n7) = fkar1(n13)
  end do
  call tacs3
  do j=koncur+1, kontot
     n7 = kontac(j)
     n13 = konadd(j)
     d8 = fkar1(n13) - xtcs(n7)
     if (absz(d8) .le. epskon(j)) go to 3486
     fkar1(n13) = xtcs(n7)
     ialter = 1
3486 end do
  if ( iprspy .lt. 1 ) go to 3496
  write (munit6, 3493)  koncur, kontot, ialter
3493 format ( ' exit "spytac" after csp.  koncur,', ' kontot, ialter =',  3i5  )
  call window
3496 return
end subroutine spyink
!
!     subroutine initsp.
!
subroutine initsp
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive EMTP only, which services "emtspy".
  !     If no interactive emtp use, this module can be deleted.
  !     Universal initialization module for "spying".   Constants
  !     must be set only once, at the beginning of execution only.
  include 'blkcom.ftn'
  include 'dekspy.ftn'
  character*8 textay(75)
  !     next come all possible key-word responses to "spy:" prompt:
  data  textay(1)   /  'heading '  /
  data  textay(2)   /  'stop    '  /
  data  textay(3)   /  'plot    '  /
  data  textay(4)   /  'help    '  /
  data  textay(5)   /  'examine '  /
  data  textay(6)   /  'deposit '  /
  data  textay(7)   /  'switch  '  /
  data  textay(8)   /  'append  '  /
  data  textay(9)   /  'save    '  /
  data  textay(10)  /  'restore '  /
  data  textay(11)  /  'go      '  /
  data  textay(12)  /  'echo    '  /
  data  textay(13)  /  'find    '  /
  data  textay(14)  /  'list    '  /
  data  textay(15)  /  'spy     '  /
  data  textay(16)  /  'break   '  /
  data  textay(17)  /  'when    '  /
  data  textay(18)  /  'comment '  /
  data  textay(19)  /  '@?      '  /
  data  textay(20)  /  'roll    '  /
  data  textay(21)  /  'type?   '  /
  data  textay(22)  /  'verify  '  /
  data  textay(23)  /  'files   '  /
  data  textay(24)  /  'sleep   '  /
  data  textay(25)  /  'source  '  /
  data  textay(26)  /  'edit    '  /
  data  textay(27)  /  'wake    '  /
  data  textay(28)  /  'language'  /
  data  textay(29)  /  'catalog '  /
  data  textay(30)  /  'begin   '  /
  data  textay(31)  /  'step    '  /
  data  textay(32)  /  'debug   '  /
  data  textay(33)  /  'data    '  /
  data  textay(34)  /  'ramp    '  /
  data  textay(35)  /  'time    '  /
  data  textay(36)  /  'tek     '  /
  data  textay(37)  /  'branch  '  /
  data  textay(38)  /  'yform   '  /
  data  textay(39)  /  'noy     '  /
  data  textay(40)  /  'factor  '  /
  data  textay(41)  /  'nof     '  /
  data  textay(42)  /  'rlc     '  /
  data  textay(43)  /  'width   '  /
  data  textay(44)  /  'bus     '  /
  data  textay(45)  /  'size    '  /
  data  textay(46)  /  'limit   '  /
  data  textay(47)  /  'iout    '  /
  data  textay(48)  /  'node    '  /
  data  textay(49)  /  'nonlin  '  /
  data  textay(50)  /  'space   '  /
  data  textay(51)  /  'lunit4  '  /
  data  textay(52)  /  'series  '  /
  data  textay(53)  /  'lock    '  /
  data  textay(54)  /  '[y]     '  /
  data  textay(55)  /  '[f]     '  /
  data  textay(56)  /  'noroll  '  /
  data  textay(57)  /  'open    '  /
  data  textay(58)  /  'close   '  /
  data  textay(59)  /  'sm      '  /
  data  textay(60)  /  'honk    '  /
  data  textay(61)  /  'choice  '  /
  data  textay(62)  /  'tacs    '  /
  data  textay(63)  /  'wait    '  /
  data  textay(64)  /  'v-i     '  /
  data  textay(65)  /  '        '  /
  n3 = 29
  call dimens ( memrmp(1), n3, bus1, bus1 )
  limbuf = memrmp(2)
  if ( iprspy .lt. 1 )  go to 1144
  write (munit6, 1143)  limbuf
1143 format ( ' near top of "initsp".  limbuf =',  i8 )
  call window
1144 lockbr = 1
  kbreak = 0
  indbuf = 0
  noback = 0
  monitr = 0
  monits = 0
  inchlp = 0
  kslowr = 5
  maxflg = 1
  tbreak = 8877.e33
  kspsav = 0
  numrmp = 0
  kbrser = 0
  kserlc = 0
  lserlc = 0
  komadd = 0
  blan80 = ' '
  do j=1, 9999
     if ( textay(j) .eq. '        ' )  go to 1152
1148 spykwd(j) = textay(j)
  end do
1152 numkey = j - 1
  nexmod = 0
  junker = 'junk    '
  digit(1) = '1'
  digit(2) = '2'
  digit(3) = '3'
  digit(4) = '4'
  digit(5) = '5'
  digit(6) = '6'
  digit(7) = '7'
  digit(8) = '8'
  digit(9) = '9'
  digit(10) = '0'
  call locatn
  do j=1, 9
     filext(j) =  ' '
     write (ansi32, 1283)          j
1283 format ( 7hinclspy,  i1 , 4h.dat,  13x )
     inquire (file=ansi32, exist=logvar)
     if ( logvar )  filext(j) = 'x'
     if ( iprspy .lt. 1 )  go to 2795
     write (munit6, 2789)  j, ansi32
2789 format ( ' next use of "inquire".  j, ansi32 =', i5, 1x, a32 )
     call window
2795 end do
  kwtspy = 0
  jjroll = 0
  tdroll = 1.0
  kfile5 = 0
  lidnt1 = 1
  lidnt2 = 1
  linspn = 1
  return
end subroutine initsp
!
! subroutine flager.
!
subroutine flager
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     module of interactive emtp only, which services "emtspy".
  !     if no interactive use, convert to dummy module ("return").
  !       vax-11 installation-dependent emtp module which serves
  !       to read spy command from munit5 if: 1) ctrl-c interrupt
  !       has occurred, or 2) if  lockbr = 1  upon entry.
  include 'blkcom.ftn'
  include 'dekspy.ftn'
  common /comkwt/  kwtvax           ! magic block for ctrl-c trapping
  dimension  idum(3)                !  dummy vector for ctrl-c handling
  external kwiter                   ! needed for ctrl-c initialization
  if ( iprspy .lt. 10 ) go to 3456  ! jump around diagnostic
  write (munit6, 3409)  istep, kwtspy, itype, lastov
3409 format (' Top flager.  istep, kwtspy, itype, lastov =', 4i6)
  call window                       ! output of character variable munit6
3456 if (lastov .ne. 9911) go to 3614 ! not "pltfil" overflow
  buff77 = 'space'                  ! next spy command we want to execute
  kwtspy = 1                        ! set flag for global emtp use (remember)
  kfile5 = 2                        ! signal to "spying" that buff77 now read
  go to 3651                        ! exit module after possible diagnostic
3614 if ( kwtvax .eq. 0 ) go to 3642 ! no user-keyed interrupt
  write (prom80, 3462)              ! prompt for spy keyboard input
3462 format (' spy: ' )
  call prompt                       ! write prom80 with cursor control (no lf)
  kwtvax = 0                        ! reset flag of ctrl-c interception for next
  !     call enable_ctrl_c ( kwiter, idum(1) )  ! re-enable it
  if ( iprspy .lt. 1 )  go to 3491  ! jump around diagnostic
  write (munit6, 3487)
3487 format (' Enable VAX ctrl-c interception in "flager" .')
  call window         ! output of character variable munit6
3491 go to 3643     ! jump to read from unit munit5 (keyboard)
3642 if ( lockbr .ne. 1 )  go to 3651        ! no forced input
  if ( kfile5 .eq. 1 )  go to 3651  ! "@" read out in emtspy
3643 read (munit5, 3647) buff77         ! read next spy input
3647 format (a80)
  kwtspy = 1    ! set flag for global emtp use (remember)
  kfile5 = 2     ! signal to "spying" that buff77 now read
  if ( kspsav .eq. 0 )  go to 3650  ! no echoing of spy command
  if ( buff77(1:7) .ne. 'cancel ' )  go to 3648
  kspsav = kspsav + 1   ! erase previous, erroneous command
  go to 3643      ! loop back for another, a real, command
3648 kspsav = kspsav - 1  ! next file6 cell to save buff77 in
  if ( kspsav .gt. numcrd )  go to 3649   ! no overlap yet
  write (*, *)  ' Error stop; overflow in "flager".'
  call stoptp     ! installation-dependent fortran stop
3649 file6(kspsav) = buff77   ! accumulate user-keyed spy input
3650 go to 3651    ! exit module with 80-col. buff77 card now read
3651 if ( iprspy .lt. 9 )  go to 9000
  write (munit6, 9004)  kwtspy, buff77
9004 format (' Exit "flager".  kwtspy =',  i4, '   buff77 = ',  a80  )
  call window         ! output of character variable munit6
9000 return
end subroutine flager
!
!     subroutine quiter.
!
subroutine quiter
  implicit real*8 (a-h, o-z),  integer*4 (i-n)
  !     module of interactive emtp only, which services "emtspy".
  !     if no interactive use, convert to dummy module ("return").
  !     this module provides a special-purpose connection to the
  !     more general "flager".  here, we only want to sense a
  !     user-keyed interrupt (no spy input is to be read).
  include 'dekspy.ftn'
  n24 = lockbr   ! save current time-slicing flag value
  n25 = kfile5   ! save current status of input connection
  lockbr = 0         ! temporarily turn time-sharing on
  call flager    ! check for user-keyed interrupt, and return
  lockbr = n24   ! restore original value of b4 flager use
  kfile5 = n25   ! restore original value of b4 flager use
  return
end subroutine quiter
!
!     subroutine honker.
!
subroutine honker (klevel)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive EMTP only, which services "emtspy".
  !     If no interactive use, this module can be deleted.
  !     VAX-11 installation-dependent emtp module which issues
  !     an audible alarm (via a terminal speaker) of intensity
  !     controlled by argument klevel.  This is on a scale of
  !     one to ten, with zero meaning no noise at all, and ten
  !     corresponding to a disaster alert (e.g., air raid sirens).
  !     This VAX-11 module is designed for DEC VT100 terminal.  The
  !     idea is by Albert H. Schmidt, BPA route EOGA.  At the keyboard,
  !     ctrl-g will ring the bell, and the ascii character 7 is
  !     equivalent within a program.  Shortage of 1 bell is due to
  !     carriage control, maybe (see n8 = j + 1, rather than j, below).
  include 'blkcom.ftn'
  include 'dekspy.ftn'
  real*8 d13
  character*8 spytim(2), spdate(2)
  d13 = 2.0                 ! initialize time delay at two seconds
  call time44 ( spytim(1) )    ! emtp wall-clock time
  call date44 ( spdate(1) )    ! emtp date utility
  buff77(1:1) = char(7)  ! ascii "7" = ctrl-g of keyboard = 1 bell
  buff77(2:2) = buff77(1:1)     ! define 2nd bell, if needed
  buff77(3:3) = buff77(1:1)     ! define 3rd bell, if needed
  buff77(4:6) = buff77(1:3)     ! define bells number 4 to 6
  buff77(7:10) = buff77(1:4)      ! define bells number 7-10
3608 write (munit6, 3627) spytim, spdate, klevel, buff77(1:klevel)
3627 format ( ' audible alarm began at  ',  2a4,  2x,  2a4, 5x,  i3,  ' bells.',  a )
  call window         ! output of character variable munit6
  if ( klevel .lt. 10 ) go to 9000  ! exit module (no loop)
  call tdelay ( d13 )           ! now stall for d13 seconds
  if ( d13 .eq. -7654 ) go to 9000   ! interrupt in "tdelay"
  call flager               ! check for user-keyed interrupt signal
  if ( kwtspy .eq. 0 ) go to 3643   ! no user abort of alarm
  kwtspy = 0        ! reset interrupt indicator as we begin service
  go to 9000                     ! jump out of loop (abort honking)
3643 d13 = 1.5 * d13   ! lengthen delay after next ten bells
  go to 3608       ! loop back to repeat string of 10 bells
9000 return
end subroutine honker
!
!     subroutine tdelay.
!
subroutine tdelay ( d8 )
  implicit real*8 (a-h, o-z),  integer*4 (i-n)
  !     Module of interactive emtp only, which services "emtspy".
  !     If no interactive use, this module can be deleted.
  !     VAX-11   module designed to stall for  d8  seconds.
  !     Present use of disk writes is temporary only, and should
  !     later be replaced by a less-wasteful, true hibernation.
  include 'blkcom.ftn'
  n23 = d8                  ! integer number of seconds for hibernation
  do j=1, n23         ! loop once for each second of delay
     do k=1, 6        ! one second = about 6 writes to disk
        rewind 36           ! rewind unused, dummy i/o channel 36
3624    write (36, 3629)   k ! dummy write to disk
     end do
3629 format ( i2 )
     call quiter               ! check for user-keyed interrupt signal
     if ( kwtspy .eq. 0 ) go to 3641 ! no user abort of alarm
     kwtspy = 0                ! reset interrupt indicator as we begin service
     d8 = -7654                ! argument flag remembering abort (for honker)
     go to 9000                ! jump out of time-delay loop, to return
3641 end do
9000 return
end subroutine tdelay
!
!     subroutine kwiter.
!
subroutine kwiter ( idum )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     VAX-11  installation-dependent emtp module which serves
  !     control interactive usage.  if none, destroy the module.
  !     Purpose is to sense user-keyed interrupt, and set flag.
  !     Name "comkwt" is reserved (connected to "controlc.obj")
  include 'dekspy.ftn'
  common /comkwt/  kwtvax
  dimension  idum(3)
  kwtvax = 1
  return
end subroutine kwiter
!
!     subroutine percnt.
!
subroutine percnt ( vbyte, n7 )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive emtp usage only, which services "emtspy".
  !     Utility which serves to replace  "%%%%%%%%"  strings of disk
  !     files by parameters of  "@?"  call.   columns 1, ... n7  are
  !     searched, of character vector  "vbyte" .
  !     For non-interactive emtp, this module can be destroyed.
  include 'dekspy.ftn'
  character*1 vbyte(1)
  do k=1, n7
     if ( vbyte(k)  .ne.  '%' )   go to 1297
     do j=1, 7
        if ( vbyte(k+j) .ne.  '%' )   go to 1297
1253 end do
     !     we exit  do 1253  with string of 8 "%" beginning in column k
     itexp = itexp + 1
     if ( itexp  .le.  maxarg )   go to 1284
     write (munit6, 1274)  maxarg
1274 format ( ' ????  trouble.  "@?"  usage only defined', i4,  '   arguments, while the disk'     )
     call window
     write (munit6, 1275)
1275 format (  '                 file has more  %-strings.', '   trouble detected in following:'   )
     call window
     write (munit6, 1277)  ( vbyte(i), i=1, n7 )
1277 format ( '              >>>',  80a1  )
     call window
     kilper = 1
     go to 1313
1284 ansi8 = texpar(itexp)
     read (ansi8, 1296)  ( vbyte(k+j-1), j=1, 8 )
1296 format ( 80a1 )
1297 end do
  if ( kverfy .ne. 0 )  go to 1313
  write (munit6, 1306)  ( vbyte(j), j=1, n7 )
1306 format ( ' @>>>',  80a1 )
  call window
1313 return
end subroutine percnt
!
!     subroutine numchk.
!
subroutine numchk ( vbyte, nchar, kill )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive emtp usage only, which services "emtspy".
  !     This utility serves to scrutinize the input character string
  !     (vbyte(j), j=1, nchar)  to see if it is a legal floating-point
  !     number.   If so,  "kill"  is to be set to zero;  if number is
  !     structurally deficient,  "kill"  is to be set positive.
  !     For non-interactive emtp, this module can be destroyed.
  include 'dekspy.ftn'
  character*1 vbyte(1)
  kill = 0
  koldig = 0
  nper = 0
  nsign = 0
  nume = 0
  kk = 0
  do i=1, nchar
     if ( vbyte(i)  .eq.  ' '  )  go to 3481
     kk = kk + 1
     if ( vbyte(i)  .ne.  '+'     .and. vbyte(i)  .ne.  '-'  )   go to 3412
     !     following code considers ramifications of just-found "+" or "-" :
     nsign = nsign + 1
     if ( nsign  .gt.  2 )   kill = 1
     if ( kk  .eq.  1 )   go to 3481
     if ( kolexp  .eq.  i-1 )   go to 3481
     kill = 1
     go to 3481
3412 if ( vbyte(i)  .ne.  'e'     .and. vbyte(i)  .ne.  'd'  )   go to 3425
     !     following code considers ramifications of just-found "d" or "e":
     nume = nume + 1
     kolexp = i
     if ( nume .gt. 1 )   kill = 1
     go to 3481
3425 if ( vbyte(i)  .ne.  '.' )   go to 3428
     !     following code considers ramifications of just-found decimal point
     nper = nper + 1
     if ( nper .gt. 1 )  kill = 1
     kolper = i
     go to 3481
3428 do j=1, 10
        if ( vbyte(i)  .eq.  digit(j) )   go to 3438
3431 end do
     kill = 1
     go to 3481
     !     following code considers ramifications of just-found digit:
3438 koldig = i
3481 end do
  if ( nume  .eq.  1     .and. kolexp  .gt.  koldig ) kill = 1
  if ( nume  .eq.  1     .and. kolper  .gt.  kolexp ) kill = 1
  if ( kill  .eq.  0 )   return
  write (munit6, 3492)  ( vbyte(j), j=1, nchar )
3492 format ( ' ??? sorry, illegal numeric just read:',  50a1 )
  write (munit6, 3493)
3493 format ( '            make a second try, please ....'  )
  return
end subroutine numchk
!
!     subroutine getnum.
!
subroutine getnum ( num )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive emtp usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  include 'dekspy.ftn'
  character*1   c4
  if ( iprspy .lt. 1 )  go to 4204
  write (munit6, 4203)  ibegcl, bytbuf(ibegcl:ibegcl)
4203 format ( ' begin  "getnum".   ibegcl =', i5,   '      bytbuf(ibegcl) =',  a1 )
  call window
4204 n1 = 1
  num = 0
  do i=ibegcl, 20
     c4 = bytbuf(i:i)
     if ( c4  .eq.  ' ' )   go to 4286
     if ( c4  .eq.  ':'  )   go to 4286
     if ( c4  .eq.  '#' )   go to 4286
     if ( c4  .eq.  ','  )   go to 4286
     if ( c4  .eq.  '+'  )   go to 4265
     if ( c4  .ne.  '-'  )   go to 4218
     n1 = -1
     go to 4265
4218 do j=1, 10
        if ( c4  .eq.  digit(j) )   go to 4256
4234 end do
     write (munit6, 4239)  i, c4
4239 format (  '    -- illegal byte in "number".', 1x,  i4,  3x,  a1,  3x,  'try again ...'  )
     call window
     num = -87654
     go to 4294
4256 if ( j  .eq.  10 )   j = 0
     num = 10 * num  +  j
     if ( iprspy .lt. 2 )  go to 4265
     write (munit6, 4259)  j, num
4259 format ( ' next digit.  j, num =',  2i8 )
     call window
4265 end do
  write (munit6, 4271)  num
4271 format ( ' stop at 4271 of "getnum".  num =',  i8 )
  call window
  call stoptp
4286 if ( n1  .lt.  0 )   num = -num
  iendcl = i - 1
  if ( iprspy .lt. 1 )  go to 4294
  write (munit6, 4293)  iendcl, num
4293 format (  ' exit "number".   iendcl, num =',  2i8  )
  call window
4294 return
end subroutine getnum
!
!     subroutine movers.
!
subroutine movers ( from, to, num )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive emtp usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  character*1   from(1), to(1)
  do j=1, num
1763 to(j) = from(j)
  end do
  return
end subroutine movers
!
!     subroutine moverl.
!
subroutine moverl ( from, to, num )
  implicit real*8 (a-h, o-z),  integer*4 (i-n)
  !     Module of interactive emtp usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  real*8   from(1), to(1)
  do j=1, num
1763 to(j) = from(j)
  end do
  return
end subroutine moverl
!
!     subroutine window.
!
subroutine window
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive emtp usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  !     For character*132 spy display channel munit6, this serves
  !     to actually output the line to the spy display device.
  !     VAX-11  installation-dependent emtp module.
  include 'blkcom.ftn'
  include 'dekspy.ftn'
  !     temporarily, until we learn how to write to a 2nd crt for
  !     vax/vms, we will just write to lunit6:
  if ( iabs(kverfy) .eq. 34543 ) go to 9000 ! no spy windows
  do j=1, 132         ! search line for right most non-blank
     k = 133 - j            ! reverse index (step from right to left)
     if ( munit6(k:k) .ne. ' ' )  go to 5621 ! end of line
5614 end do                   ! end  do 5614  loop to find right edge of line
5621 write (lunit6, 5624)  munit6(1:k) ! output nonblank part
5624 format ( a )
9000 return
end subroutine window
!
!     subroutine spylin.
!
subroutine spylin
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive emtp usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  !     One blank line is written on spy screen by this module.
  include 'blkcom.ftn'
  include 'dekspy.ftn'
  !     temporarily, until we learn how to write to a 2nd crt for
  !     VAX/VMS, we will just write to lunit6 in universal form:
5621 write (lunit6, 5624)
5624 format ( 1x )
end subroutine spylin
!
!     subroutine spyout.
!
subroutine spyout(n1, n2)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive EMTP only, which services "emtspy".
  !     If no interactive use, this module can be deleted.
  !     Arguments n1 and n2 are node nos., with possible "-" signs.
  !     Purpose is to load and print corresponding names for "output"
  include 'labcom.ftn'
  include 'dekspy.ftn'
  save
  character*8 text10(10), terra
  data terra / 6hterra  /
  n11 = n1
  n12 = n2
  !     first check for special initialization or flushing calls:
  if (n11 .ne. 7878) go to 4618
  k = 0
  go to 9000
4618 if (n11 .ne. -7878) go to 4626
4621 if (k .le. 0) go to 9000
  write (munit6, 4629) (text10(j), j = 1, k)
4629 format(7x, 10a7)
  call window
  k = 0
  go to 9000
  !     insert first node name into buffer:
4626 if (n11 .lt. 0) n11 = -n11
  k = k + 1
  text10(k) = bus(n11)
  if ( n11 .eq. 1 ) text10(k) = terra
  if ( k .eq. 10 )  go to 4621
  if ( n12 .eq. 0 ) go to 9000
  if ( n12 .lt. 0 ) n12 = -n12
  k = k + 1
  text10(k) = bus(n12)
  if ( n12 .eq. 1 ) text10(k) = terra
  if ( k .eq. 10 )  go to 4621
9000 if ( iprspy .lt. 3 )  go to 9007
  write (munit6, 9004)  n1, n2, k, n11, n12
9004 format ( ' exit "spyout".  n1, n2, k, n11, n12 =', 5i6 )
  call window
9007 return
end subroutine spyout
!
!     subroutine examin.
!
subroutine examin
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive EMTP usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  !     This near-universal module serves to build the character*132
  !     output vector outlin of the "examine" command of spy.
  !     Computers with index problems (e.g., prime) need replacement
  include 'dekspy.ftn'
  if ( iprspy .lt. 1 ) go to 1718
  write (munit6, 1707)  numex, imin(1), locout(1)
1707 format (' top of "examin".  numex, imin(1), locout(1) =', 3i6 )
  call window
1718 jj = 0
  outlin(1:1) = ' '
  kolout = 2
  if ( numex .le. 0 )  go to 9000
1540 jj = jj + 1
  n5 = imin(jj)
  n3 = locout(jj)
  n8 = locate(n3) + n5-1
  if ( intout(jj)  .eq.  0 )   n8 = n8 + n5 - 1
  if ( iprspy .lt. 3 )  go to 1560
  write (munit6, 1544)  jj, n5, n3, n8, intout(jj)
1544 format ( ' next  examine.  jj, n5, n3, n8, intout(jj)',  5i8 )
  call window
1560 if ( intout(jj)  .eq.  0 )   go to 1600
  n9 =  n8 - memkar
  n8 = n8 + 1
  if ( ivec(n3)  .eq.  1 )   go to 1577
  if ( n5 .eq. 1  .and.  imax(jj) .eq. 1 )  go to 1640
1577 write (ansi16, 1580)  kar1(1+n9)
1580 format ( i10 )
  outlin(kolout:kolout+9) = ansi16(1:10)
  outlin(kolout+10:kolout+14) = blan80(1:5)
  kolout = kolout + 15
  go to 1664
1600 n9 = ( n8 - memkar ) / 2
  n10 = memkar + 2 * n9  -  n8
  n8 = n8 + 2
  if ( n10  .ne.  0 )   go to 1628
  !     following real*8 extractions line up (no need for 4-byte shift):
  if ( iascii(n3) .eq. 0 ) write (ansi16, 1620)  fkar1(1+n9)
1620 format ( e15.6 )
  if ( iascii(n3) .eq. 1 ) write (ansi16, 1624)  fkar1(1+n9)
1624 format ( 4x,  1h",  a6,  1h",  3x )
  go to 1633
  !     Following real*8 extractions require a 4-byte shift (fkar2 use).
  !     fkar2 has higher memory address than fkar1, so it is correct with
  !     n9 offset if n9 is positive.   But if n9 is negative, the half
  !     word shift really should be toward lower addresses, which we
  !     compensate for by the continued use of fkar2 but with one smaller
  !     n9 value:
1628 if ( n9 .lt. 0 )  n9 = n9 - 1
  if ( iascii(n3) .eq. 0 ) write (ansi16, 1620)  fkar2(1+n9)
  if ( iascii(n3) .eq. 1 ) write (ansi16, 1624)  fkar2(1+n9)
1633 outlin(kolout:kolout+14) = ansi16(1:15)
  kolout = kolout + 15
  go to 1664
1640 write (ansi8, 1660)  kar1(1+n9)
1660 format ( i6 )
  outlin(kolout:kolout+5) = ansi8(1:6)
  if ( iprspy .lt. 3 ) go to 1662
  write (munit6, 1661)  n9,  kar1(1+n9)
1661 format (' i6 integer encoded for examine.  n9,kar1(1+n9) =', 2i8 )
  call window
1662 kolout = kolout + 6
1664 n5 = n5 + 1
  if ( n5  .le.  imax(jj) )   go to 1560
  if ( jj .lt. numex ) go to 1540
  if ( kolout .lt. 132 ) outlin(kolout:132) = ' '
  if ( iprspy .lt. 1 ) go to 9000
  n17 = kolout
  if ( n17 .gt. 80 )  n17 = 80
  write (munit6, 8872)  kolout, outlin(1:n17)
8872 format (' exit "examin".   kolout =',  i5, '    outlin(1:80) =',  a  )
  call window
9000 return
end subroutine examin
!
! subroutine deposi.
!
subroutine deposi ( ind, intype, n1, n2, d4 )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module of interactive emtp usage only, which services "emtspy".
  !     for non-interactive EMTP, this module can be destroyed.
  !     this near-universal module services "deposit", to actually
  !     perform the tampering.   Symbol is in row ind.  if it is
  !     of type alphanumeric, ansi8(1:6) in "dekspy" carries the
  !     new contents.  Otherwise, d4 is the numeric value to be
  !     deposited, and intype gives the mode (1=integer, 0=real).
  !     Bounding subscripts on the deposit are n1 and n2, respectively.
  !     Computers with index problems (e.g., prime) need replacement
  include 'dekspy.ftn'
  if ( iprspy .lt. 1 ) go to 1846
  write (munit6, 1707)  ind, intype, iascii(ind), d4, ansi8
1707 format (' top of "deposi".   ind, intype, iascii(ind),', ' d4, ansi8 =',  3i8, e15.4, 3x, a8 )
  call window
1846 n8 = locate(ind)
  !     following use of real*8 deposit logic to also handle
  !     alphanumeric only works for computers with this equality:
  if ( iascii(ind) .eq. 1 ) read (ansi8, 1849)  d4
1849 format ( a6 )
  if ( intype .eq. 0 ) go to 1880
  n9 = n8 - memkar + n1
  !     enter loop of integer deposits, subscripts n1 through n2:
1854 kar1(n9) = d4
  n1 = n1 + 1
  n9 = n9 + 1
  if ( n1 .le. n2 ) go to 1854
  go to 8000
  !     our vector is  real*8, so requires the following special deposit:
1880 n9 = (n8 - memkar)/2 + n1
  n10 = memkar + 2*(n9-n1) - n8
  !     enter loop of real deposits, subscripts n1 through n2:
1904 if ( n10 .gt. 0 ) fkar2(n9) = d4
  if ( n10 .eq. 0 ) fkar1(n9) = d4
  n1 = n1 + 1
  n9 = n9 + 1
  if ( n1  .le.  n2 )   go to 1904
8000 if ( iprspy .lt. 1 ) go to 9000
  write (munit6, 8872)
8872 format (' exit "deposi".' )
  call window
9000 return
end subroutine deposi
!
! subroutine append.
!
subroutine append
  implicit real*8 (a-h, o-z),  integer*4 (i-n)
  !     module connected to key word "append" of spy.  others
  !     can perfect and use their own installation-dependent
  !     (and perhaps proprietary) extensions via this module.
  return
end subroutine append
!
! subroutine intpar.
!
subroutine intpar ( max, n1, n2, kill )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     module of interactive emtp usage only, which services "emtspy".
  !     this module is designed to extract two free-format row numbers
  !     from  bytbuf(20)  input buffer of common.   these numbers must
  !     be positive, ordered (1st less than or equal to 2nd), and not
  !     in excess of the last table row "max".   the to row numbers are
  !     passed back as  "n1"  and  "n2"  arguments.   if this extraction
  !     was successful,  "kill"  is set to zero;  if it failed,  kill = 1.
  !     for non-interactive emtp, this module can be destroyed.
  include 'dekspy.ftn'
  kill = 0
  !     check for english request "top" (for the top of the file):
  if ( bytbuf(1:1)   .ne.  't' )  go to 2071
  n1 = 1
  n2 = 1
  go to 2110
  !     check for english request "bot" (for the bottom of the file):
2071 if ( bytbuf(1:1)  .ne. 'b' )   go to 2075
  n1 = max
  n2 = max
  go to 2110
  !     check for english request "all" (for the entire file):
2075 if ( bytbuf(1:1)  .ne.  'a' )   go to 2088
  n1 = 1
  n2 = max
  go to 2110
2088 do i=1, 20
     n13 = 21 - i
     if ( bytbuf(n13:n13) .eq. ' ' )  go to 2046
     if ( bytbuf(n13:n13) .ne. ',' )   go to 2054
     n6 = 20 - i
     do j=1, n6
        n13 = 21 - i + j
2037    bytbuf(n13:n13) = bytbuf(j:j)
     end do
     go to 2054
2046 end do
2054 n22 = 0
  n13 = 0
  do i=1, 20
     n17 = i - 1
     if ( bytbuf(i:i)  .ne.  ' ' )   go to 2091
     !     blank is ignored if before 1st digit, of if not 1st of string:
     if ( n22 .eq. 0 )  go to 2104
     if ( bytbuf(n17:n17) .eq. ' ' )  go to 2104
     if ( bytbuf(n17:n17) .eq. ',' )  go to 2104
     n13 = n13 + 1
     go to 2104
     !     begin processing of non-blank character  bytbuf(i) :
2091 n22 = n22 + 1
     if ( bytbuf(i:i)  .ne.  ',' )  go to 2093
     if ( bytbuf(n17:n17) .eq. ' ' )  go to 2104
     n13 = n13 + 1
     go to 2104
2093 do 2097  j=1, 10
        if ( bytbuf(i:i)  .eq.  digit(j) )   go to 2104
2097 end do
2104 end do
  if ( n22  .gt.  0 )   go to 2109
  !     blank response is interpreted as a request for more of same:
  n1 = n2 + 1
  if ( n1  .le.  max )  go to 2107
  write (munit6, 2105)
2105 format ( '    ---- wrap around, end to beginning ----' )
  call window
  n1 = 1
2107 n2 = n1 + n33
  go to 2110
2109 if ( n13  .eq. 2 )   go to 2132
2113 write (munit6, 2115)
2115 format (  ' ????  illegal data (not blank or two dec', ' free-format integers.   try again.'   )
  call window
  write (munit6, 2118)
2118 format (  '       is it possible user"s data has no', ' such table (other cause of message)?'  )
  call window
  kill = 1
  go to 9000
2132 call frein2(bytbuf, n1, n2)
  n33 = n2 - n1
2110 if ( n1  .le.  0  )       n1 = 1
  if ( n2  .gt.  max )   n2 = max
9000 if ( iprspy .lt. 1 ) go to 9006
  write (munit6, 9002)  max, n1, n2, kill
9002 format (  ' return from "intpar".  max, n1, n2, kill =', 4i6 )
  call window
9006 return
end subroutine intpar
!
! subroutine sosrng.
!
subroutine sosrng ( kill )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     module of interactive emtp usage only, which services "emtspy".
  !     this module serves to extract a beginning and ending line number
  !     for sos-like editing operations of  "edit"  command.   these two
  !     integer outputs are  (lidnt1, lidnt2)  of common.   the only
  !     argument is  "kill",  which tells whether the operation was a
  !     success:  "0" means success,  "1" means error.
  !     for non-interactive emtp, this module can be destroyed.
  include 'dekspy.ftn'
  if ( iprspy .lt. 1 )  go to 2615
  write (munit6, 2613)   bytbuf
2613 format ( ' top "sosrange".  bytbuf(a20) =',   a20  )
  call window
  write (munit6, 2614)  numsym, lidnt1, linnow, char1
2614 format ( ' numsym, lidnt1, linnow, char1 =',  3i5, 2x, a1 )
  call window
2615 kill = 0
  if ( char1  .ne.  ' ' )   go to 2629
  !     blank means we just continue displaying as many lines as last time
  n12 = linspn
  lidnt1 = lidnt2 + 1
  lidnt2 = lidnt1 + n12
  go to 2662
2629 if ( bytbuf(1:1)  .ne.  '#' )   go to 2632
  !     following code processes a command like  "*p22" :
  ibegcl = 2
  call getnum ( n1 )
  if ( n1  .ne.  -87654 )  go to 2630
2621 kill = 1
  go to 9000
2630 lidnt1 = linnow + 1
  lidnt2 = linnow + n1
  go to 2662
2632 if ( bytbuf(1:1)  .ne.  ' ' )   go to 2635
  !     following code process simple "*p" ---- next 16 lines, like sos:
  if ( linnow  .lt.  numcrd )   go to 2634
  print *,   ' no such lines exist'
  go to 2621
2634 n1 = 16
  go to 2630
2635 ibegcl = 0
  n13 = 0
  lidnt1 = 0
  lidnt2 = 0
2636 ibegcl = ibegcl + 1
  n24 = 0
  if ( iprspy  .lt.  2 )  go to 2644
  write (munit6, 2640)
2640 format (  ' begin next lidnt.', '    ibeg     n13  lidnt1  jpoint      n1     n24'  )
  call window
  write (munit6, 2641) ibegcl, n13, lidnt1, linnow, n1, n24
2641 format (  '                  ',   6i8  )
  call window
2644 if ( bytbuf(ibegcl:ibegcl)  .ne.  '.' )   go to 2645
  n24 = linnow
  go to 2654
2645 if ( bytbuf(ibegcl:ibegcl)  .ne.  '^' )   go to 2648
  n24 = 1
  go to 2654
2648 if ( bytbuf(ibegcl:ibegcl)  .ne.  '*' )   go to 2657
  n24 = numcrd
2654 ibegcl = ibegcl + 1
2657 call getnum ( n1 )
  if ( n1  .eq.  -87654 )   go to 2621
  n13 = n13 + 1
  if ( n13  .eq.  1 )   lidnt1 = n24 + n1
  if ( n13  .eq.  2 )   lidnt2 = n24 + n1
  if ( n13  .eq.  2 )   go to 2662
  ibegcl = iendcl + 1
  if ( bytbuf(ibegcl:ibegcl)  .eq.  ':' )   go to 2636
  if ( bytbuf(ibegcl:ibegcl)  .ne.  '#' )   go to 2659
  !     we get here with a request like  "*p200!5",  after "200" is known
  ibegcl = ibegcl + 1
  call getnum ( n1 )
  if ( n1  .eq.  -87654 )   go to 2621
  lidnt2 = lidnt1 + n1 - 1
  go to 2662
  !     we reach 2659 if nothing follows first number (e.g., "*p200"):
2659 lidnt2 = lidnt1
  !     now we display lines  lidnt1, ...., lidnt2,  after limit check:
2662 if ( lidnt1  .ge.  1 )   go to 2663
  print *,   ' illegal syntax of command'
  go to 2621
2663 if ( lidnt1  .le.  numcrd )   go to 2664
  print *,   ' range given does not contain any lines'
  go to 2621
2664 if ( lidnt2 .gt. numcrd )  lidnt2 = numcrd
9000 if ( iprspy .lt. 1 )  go to 9006
  write (munit6, 9003)  lidnt1, lidnt2,  kill, char2
9003 format ( ' exit "sosrng".  lidnt1, lidnt2, kill, char2 =', 3i6,  2x,  a1  )
  call window
9006 return
end subroutine sosrng
!
!     subroutine movesp.
!
subroutine movesp(from, to, n15)
  !     Module of interactive EMTP usage only, which services "emtspy".
  !     For non-interactive EMTP, this module can be destroyed.
  !     This routine transfers single-precision from(1:n15) to
  !     to(1:n15).  except for missing implicit, it equals "mover".
  real*8 from, to
  dimension from(1), to(1)
  do j=1, n15
4829 to(j) = from(j)
  end do
  return
end subroutine movesp
!
!     subroutine prompt.
!
subroutine prompt
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     VAX-11  installation-dependent EMTP module used only
  !     for interactive EMTP ("emtspy").  Input is program
  !     prompt in character*80 variable prom80 of deck "dekspy".
  !     The prompt must end with colon (":").  then line feed
  !     will be suppressed, so subsequent read is to right of ":".
  !     For non-interactive EMTP, this module can be destroyed.
  include 'blkcom.ftn'
  include 'dekspy.ftn'
  n2 = 80
  do j=1, 80
     if ( prom80(n2:n2) .ne. ' ' )  go to 1426
1394 n2 = n2 - 1
  end do
  return
  !     following lunit6 should really go to 2nd screen ("window")
1426 write (lunit6, 1475)  prom80(1:n2)
1475 format ( a, $ )           ! ",$" is dec magic to hold cursor
  return
end subroutine prompt
!
!     subroutine frefp1.
!
subroutine frefp1(ansi, d12)
  implicit real*8 (a-h, o-z),  integer*4 (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode a
  !     single floating point number.  The input is character*80
  !     variable  ansi,  and the output is double precision d12.
  !     For non-interactive emtp, this module can be destroyed.
  character*80  ansi
  n8 = 1
  call frefix ( ansi, n8 )
  read (ansi, 3892)  d12
3892 format ( e20.0 )
  return
end subroutine frefp1
!
!     subroutine fresp1.
!
subroutine fresp1(ansi, d12)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode one
  !     floating point numbers d12 from character*80 ansi.   This
  !     is identical to "frefp1", except for single precision.
  character*80  ansi
  real*8 d12
  n8 = 1
  call frefix(ansi, n8)
  read (ansi, 3892) d12
3892 format (e20.0)
  return
end subroutine fresp1
!
!     subroutine frefp2.
!
subroutine frefp2(ansi, d12, d13)
  implicit real*8 (a-h, o-z),  integer*4 (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode two
  !     floating point numbers d12 and d13 from character*80 ansi.
  !     For non-interactive emtp, this module can be destroyed.
  character*80  ansi
  real*8 d12, d13
  n8 = 2
  call frefix ( ansi, n8 )
  read (ansi, 3892)  d12, d13
3892 format ( 2e20.0 )
  return
end subroutine frefp2
!
! subroutine fresp2.
!
subroutine fresp2(ansi, d12, d13)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  it is called to decode two
  !     floating point numbers d12 and d13 from character*80 ansi.
  !     This is identical to "frefp2", except for single precision.
  character*80 ansi
  real*8 d12, d13
  n8 = 2
  call frefix ( ansi, n8 )
  read (ansi, 3892)  d12, d13
3892 format ( 2e20.0 )
  return
end subroutine fresp2
!
!     subroutine frefp3.
!
subroutine frefp3(ansi, d12, d13, d14)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode three
  !     floating point numbers d12,d13,d14 from character*80 ansi.
  !     For non-interactive emtp, this module can be destroyed.
  character*80  ansi
  real*8 d12, d13, d14
  n8 = 3
  call frefix ( ansi, n8 )
  read (ansi, 3892)  d12, d13, d14
3892 format ( 3e20.0 )
  return
end subroutine frefp3
!
!     subroutine fresp3.
!
subroutine fresp3(ansi, d12, d13, d14)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode three
  !     floating point numbers d12, d13, d13 from character*80 ansi.
  !     This is identical to "frefp3", except for single precision.
  character*80 ansi
  real*8 d12, d13, d14
  n8 = 3
  call frefix ( ansi, n8 )
  read (ansi, 3892)  d12, d13, d14
3892 format ( 3e20.0 )
  return
end subroutine fresp3
!
!     subroutine frein1.
!
subroutine frein1(ansi, n12)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  it is called to decode a
  !     single integer number n12 from character*80 input ansi.
  !     For non-interactive EMTP, this module can be destroyed.
  character*80  ansi
  integer*4 n12
  n8 = 1
  call frefix ( ansi, n8 )
  read (ansi, 3892)  d12
3892 format ( 3e20.0 )
  n12 = d12
  return
end subroutine frein1
!
!     subroutine frein2.
!
subroutine frein2(ansi, n12, n13)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode two
  !     integer numbers n12 and n13 from character*80 input ansi.
  !     For non-interactive emtp, this module can be destroyed.
  character*80 ansi
  n8 = 2
  call frefix(ansi, n8)
  read (ansi, 3892)  d12, d13
3892 format ( 3e20.0 )
  n12 = d12
  n13 = d13
  return
end subroutine frein2
!
!     subroutine frefix.
!
subroutine frefix(ansi, n8)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Universal module used only by interactive emtp ("emtspy").
  !     for non-interactive use only, it can be destroyed.
  !     This module is called by "frefp1", "frein1", etc. --- all
  !     of the free-field input modules of spy usage.  It checks
  !     for legal numerical data, and will re-prompt for more if
  !     the user has made a mistake, thereby avoiding possible
  !     death of interactive emtp execution due to typing error.
  !     Ansi is the input being processed, for n8 numbers.  Upon
  !     exit, ansi will be converted to 4e20.0 fixed-format data.
  include 'dekspy.ftn'
  character*80 ansi, hold
  if ( iprspy .lt. 3 )  go to 3582
  write (munit6, 3579)  n8, ansi(1:40)
3579 format ( ' top of "frefix".  n8 =',  i5, '   ansi(1:40) = ',  a40  )
  call window
3582 hold(1:80) = ' '
  kk = 0
  i = 0
  go to 3648
3612 if (ansi(i:i) .eq. ' ' .or. ansi(i:i) .eq. ','  .or. i .ge. 80) go to 3615
  i = i + 1
  go to 3612
3615 n5 = i - 1
  kk = kk + 1
  n13 = 0
  n14 = 0
  do j=n3, n5
     if (ansi(j:j) .ne. '+' .and. ansi(j:j) .ne. '-' ) go to 3617
     if (j .eq. n3) go to 3642
     n16 = j - 1
     if (ansi(n16:n16) .eq. 'e' .or. ansi(n16:n16) .eq. 'd') go to 3642
     go to 3758
3617 if (ansi(j:j) .ne. '.') go to 3626
     n14 = n14 + 1
     if (n14 .le. 1) go to 3642
     go to 3758
3626 if (ansi(j:j) .ne. 'd' .and. ansi(j:j) .ne. 'e') go to 3634
     n13 = n13 + 1
     if (n13 .le. 1) go to 3642
     go to 3758
3634 do k = 1, 10
        if (ansi(j:j) .eq. digit(k)) go to 3642
3638 end do
     go to 3758
3642 end do
  n2 = 20 * kk
  n1 = n2 - ( n5 - n3 )
  hold(n1:n2) = ansi(n3:n5)
  if (kk .ge. n8) go to 4100
3648 i = i + 1
  if (i .gt. 80) go to 3758
  if (ansi(i:i) .eq. ' ') go to 3648
  n3 = i
  go to 3612
3758 write (prom80, 3761)  j, ansi(j-2:j+2)
3761 format ( '   @@@  illegal data in column', i3, ' [',  a5,  '].  resend line or end :'  )
  call prompt
  write (*,*) ' ok, "frefix" call to "flager" begins.'
  lockbr = 1
  call flager
  write (*,*) ' ok, back in "frefix" with buff77 again.'
  ansi = buff77
  if (ansi(1:4) .ne. 'end ') go to 3582
  ansi(1:80) = ' '
  go to 4103
4100 ansi(1:n2) = hold(1:n2)
4103 if ( iprspy .lt. 2 )  go to 4109
  write (munit6, 4108)  ansi(1:n2)
4108 format ( ' exit "frefix".  ansi(1:n2) =',  a80 )
  call window
4109 return
end subroutine frefix
!
!     subroutine locatn.
!
subroutine locatn
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'synmac.ftn'
  include 'tacsar.ftn'
  include 'umdeck.ftn'
  include 'dekspy.ftn'
  include 'fixcom.ftn'
  locate(   1)  =  locint ( bus1   )
  locate(   2)  =  locint ( bus2   )
  locate(   3)  =  locint ( bus3   )
  locate(   4)  =  locint ( bus4   )
  locate(   5)  =  locint ( bus5   )
  locate(   6)  =  locint ( bus6   )
  locate(   7)  =  locint ( trash  )
  locate(   8)  =  locint ( blank  )
  locate(   9)  =  locint ( terra  )
  locate(  10)  =  locint ( userid )
  locate(  11)  =  locint ( branch )
  locate(  12)  =  locint ( copy   )
  locate(  13)  =  locint ( csepar )
  locate(  14)  =  locint ( chcont )
  locate(  15)  =  locint ( texcol )
  locate(  16)  =  locint ( texta6 )
  locate(  17)  =  locint ( date1  )
  locate(  18)  =  locint ( tclock )
  locate(  19)  =  locint ( vstacs )
  locate(  20)  =  locint ( abuff  )
  locate(  21)  =  locint ( ci1    )
  locate(  22)  =  locint ( ck1    )
  locate(  23)  =  locint ( deltat )
  locate(  24)  =  locint ( delta2 )
  locate(  25)  =  locint ( freqcs )
  locate(  26)  =  locint ( epsiln )
  locate(  27)  =  locint ( xunits )
  locate(  28)  =  locint ( aincr  )
  locate(  29)  =  locint ( xmaxmx )
  locate(  30)  =  locint ( znvref )
  locate(  31)  =  locint ( epszno )
  locate(  32)  =  locint ( epwarn )
  locate(  33)  =  locint ( epstop )
  locate(  34)  =  locint ( t      )
  locate(  35)  =  locint ( hertz  )
  locate(  36)  =  locint ( tolmat )
  locate(  37)  =  locint ( twopi  )
  locate(  38)  =  locint ( tmax   )
  locate(  39)  =  locint ( omega  )
  locate(  40)  =  locint ( copt   )
  locate(  41)  =  locint ( xopt   )
  locate(  42)  =  locint ( szplt  )
  locate(  43)  =  locint ( szbed  )
  locate(  44)  =  locint ( sglfir )
  locate(  45)  =  locint ( sigmax )
  locate(  46)  =  locint ( epsuba )
  locate(  47)  =  locint ( epdgel )
  locate(  48)  =  locint ( epomeg )
  locate(  49)  =  locint ( fminfs )
  locate(  50)  =  locint ( delffs )
  locate(  51)  =  locint ( fmaxfs )
  locate(  52)  =  locint ( tenerg )
  locate(  53)  =  locint ( begmax )
  locate(  54)  =  locint ( tenm3  )
  locate(  55)  =  locint ( tenm6  )
  locate(  56)  =  locint ( unity  )
  locate(  57)  =  locint ( onehaf )
  locate(  58)  =  locint ( peaknd )
  locate(  59)  =  locint ( fltinf )
  locate(  60)  =  locint ( flzero )
  locate(  61)  =  locint ( degmin )
  locate(  62)  =  locint ( degmax )
  locate(  63)  =  locint ( statfr )
  locate(  64)  =  locint ( voltbc )
  locate(  65)  =  locint ( flstat )
  locate(  66)  =  locint ( dtnext )
  locate(  67)  =  locint ( angle  )
  locate(  68)  =  locint ( pu     )
  locate(  69)  =  locint ( seedr  )
  locate(  70)  =  locint ( speedl )
  locate(  71)  =  locint ( kstart )
  locate(  72)  =  locint ( knt    )
  locate(  73)  =  locint ( kbase  )
  locate(  74)  =  locint ( ltdelt )
  locate(  75)  =  locint ( unused )
  locate(  76)  =  locint ( mtape  )
  locate(  77)  =  locint ( lunit1 )
  locate(  78)  =  locint ( lunit2 )
  locate(  79)  =  locint ( lunit3 )
  locate(  80)  =  locint ( lunit4 )
  locate(  81)  =  locint ( lunit5 )
  locate(  82)  =  locint ( lunit6 )
  locate(  83)  =  locint ( lunit7 )
  locate(  84)  =  locint ( lunit8 )
  locate(  85)  =  locint ( lunit9 )
  locate(  86)  =  locint ( lunt10 )
  locate(  87)  =  locint ( lunt11 )
  locate(  88)  =  locint ( lunt12 )
  locate(  89)  =  locint ( lunt13 )
  locate(  90)  =  locint ( lunt14 )
  locate(  91)  =  locint ( lunt15 )
  locate(  92)  =  locint ( nexout )
  locate(  93)  =  locint ( nright )
  locate(  94)  =  locint ( nfrfld )
  locate(  95)  =  locint ( kolbeg )
  locate(  96)  =  locint ( kprchg )
  locate(  97)  =  locint ( multpr )
  locate(  98)  =  locint ( ipntv  )
  locate(  99)  =  locint ( indtv  )
  locate( 100)  =  locint ( lstat  )
  locate( 101)  =  locint ( nbyte  )
  locate( 102)  =  locint ( lunsav )
  locate( 103)  =  locint ( iprsov )
  locate( 104)  =  locint ( icheck )
  locate( 105)  =  locint ( unused )
  locate( 106)  =  locint ( iend   )
  locate( 107)  =  locint ( iline  )
  locate( 108)  =  locint ( inonl  )
  locate( 109)  =  locint ( iold   )
  locate( 110)  =  locint ( iout   )
  locate( 111)  =  locint ( iprint )
  locate( 112)  =  locint ( ipunch )
  locate( 113)  =  locint ( iread  )
  locate( 114)  =  locint ( kol132 )
  locate( 115)  =  locint ( istep  )
  locate( 116)  =  locint ( unused )
  locate( 117)  =  locint ( itype  )
  locate( 118)  =  locint ( it1    )
  locate( 119)  =  locint ( it2    )
  locate( 120)  =  locint ( iupper )
  locate( 121)  =  locint ( izero  )
  locate( 122)  =  locint ( kcount )
  locate( 123)  =  locint ( istead )
  locate( 124)  =  locint ( unused )
  locate( 125)  =  locint ( ldata  )
  locate( 126)  =  locint ( lbrnch )
  locate( 127)  =  locint ( limtxf )
  locate( 128)  =  locint ( mdebug )
  locate( 129)  =  locint ( lexct  )
  locate( 130)  =  locint ( lbus   )
  locate( 131)  =  locint ( lymat  )
  locate( 132)  =  locint ( lswtch )
  locate( 133)  =  locint ( lnonl  )
  locate( 134)  =  locint ( lchar  )
  locate( 135)  =  locint ( m4plot )
  locate( 136)  =  locint ( lpast  )
  locate( 137)  =  locint ( lsmat  )
  locate( 138)  =  locint ( iplot  )
  locate( 139)  =  locint ( ncomp  )
  locate( 140)  =  locint ( nv     )
  locate( 141)  =  locint ( lcomp  )
  locate( 142)  =  locint ( numsm  )
  locate( 143)  =  locint ( ifdep  )
  locate( 144)  =  locint ( ltails )
  locate( 145)  =  locint ( lfdep  )
  locate( 146)  =  locint ( lwt    )
  locate( 147)  =  locint ( last   )
  locate( 148)  =  locint ( npower )
  locate( 149)  =  locint ( maxpe  )
  locate( 150)  =  locint ( lpeak  )
  locate( 151)  =  locint ( nout   )
  locate( 152)  =  locint ( iv     )
  locate( 153)  =  locint ( ineof  )
  locate( 154)  =  locint ( ktrlsw )
  locate( 155)  =  locint ( num99  )
  locate( 156)  =  locint ( kpartb )
  locate( 157)  =  locint ( llbuff )
  locate( 158)  =  locint ( kanal  )
  locate( 159)  =  locint ( nsmth  )
  locate( 160)  =  locint ( ntcsex )
  locate( 161)  =  locint ( nstacs )
  locate( 162)  =  locint ( kloaep )
  locate( 163)  =  locint ( lastov )
  locate( 164)  =  locint ( ltacst )
  locate( 165)  =  locint ( lhist  )
  locate( 166)  =  locint ( ifx    )
  locate( 167)  =  locint ( ndelta )
  locate( 168)  =  locint ( idelta )
  locate( 169)  =  locint ( inecho )
  locate( 170)  =  locint ( noutpr )
  locate( 171)  =  locint ( ktab   )
  locate( 172)  =  locint ( jflsos )
  locate( 173)  =  locint ( numdcd )
  locate( 174)  =  locint ( numum  )
  locate( 175)  =  locint ( lspcum )
  locate( 176)  =  locint ( nphcas )
  locate( 177)  =  locint ( locz11 )
  locate( 178)  =  locint ( locbr1 )
  locate( 179)  =  locint ( ialter )
  locate( 180)  =  locint ( i_char )
  locate( 181)  =  locint ( ktref  )
  locate( 182)  =  locint ( kph    )
  locate( 183)  =  locint ( kreqab )
  locate( 184)  =  locint ( ksat   )
  locate( 185)  =  locint ( memsav )
  locate( 186)  =  locint ( lisoff )
  locate( 187)  =  locint ( lspov4 )
  locate( 188)  =  locint ( kburro )
  locate( 189)  =  locint ( iaverg )
  locate( 190)  =  locint ( lsiz23 )
  locate( 191)  =  locint ( lsiz26 )
  locate( 192)  =  locint ( numout )
  locate( 193)  =  locint ( moldat )
  locate( 194)  =  locint ( lsiz27 )
  locate( 195)  =  locint ( ltlabl )
  locate( 196)  =  locint ( iwt    )
  locate( 197)  =  locint ( ifdep2 )
  locate( 198)  =  locint ( idoubl )
  locate( 199)  =  locint ( ioutin )
  locate( 200)  =  locint ( ipun   )
  locate( 201)  =  locint ( jst    )
  locate( 202)  =  locint ( jst1   )
  locate( 203)  =  locint ( unused )
  locate( 204)  =  locint ( numsub )
  locate( 205)  =  locint ( maxzno )
  locate( 206)  =  locint ( kalplt )
  locate( 207)  =  locint ( niomax )
  locate( 208)  =  locint ( niamax )
  locate( 209)  =  locint ( ibr1   )
  locate( 210)  =  locint ( ifsem  )
  locate( 211)  =  locint ( lfsem  )
  locate( 212)  =  locint ( iadd   )
  locate( 213)  =  locint ( lfd    )
  locate( 214)  =  locint ( laux   )
  locate( 215)  =  locint ( iofgnd )
  locate( 216)  =  locint ( iofbnd )
  locate( 217)  =  locint ( unused )
  locate( 218)  =  locint ( jseedr )
  locate( 219)  =  locint ( modout )
  locate( 220)  =  locint ( iftail )
  locate( 221)  =  locint ( ipoint )
  locate( 222)  =  locint ( lpast2 )
  locate( 223)  =  locint ( ncurr  )
  locate( 224)  =  locint ( ioffd  )
  locate( 225)  =  locint ( isplot )
  locate( 226)  =  locint ( isprin )
  locate( 227)  =  locint ( maxout )
  locate( 228)  =  locint ( ipos   )
  locate( 229)  =  locint ( unused )
  locate( 230)  =  locint ( unused )
  locate( 231)  =  locint ( kill   )
  locate( 232)  =  locint ( ivolt  )
  locate( 233)  =  locint ( nchain )
  locate( 234)  =  locint ( iprsup )
  locate( 235)  =  locint ( unused )
  locate( 236)  =  locint ( intinf )
  locate( 237)  =  locint ( kconst )
  locate( 238)  =  locint ( kswtch )
  locate( 239)  =  locint ( it     )
  locate( 240)  =  locint ( ntot   )
  locate( 241)  =  locint ( ibr    )
  locate( 242)  =  locint ( lcom10 )
  locate( 243)  =  locint ( ltrnst )
  locate( 244)  =  locint ( lsyn   )
  locate( 245)  =  locint ( kssout )
  locate( 246)  =  locint ( loopss )
  locate( 247)  =  locint ( infexp )
  locate( 248)  =  locint ( numref )
  locate( 249)  =  locint ( nword1 )
  locate( 250)  =  locint ( nword2 )
  locate( 251)  =  locint ( iloaep )
  locate( 252)  =  locint ( lnpin  )
  locate( 253)  =  locint ( ntot1  )
  locate( 254)  =  locint ( limstp )
  locate( 255)  =  locint ( indstp )
  locate( 256)  =  locint ( nc     )
  locate( 257)  =  locint ( unused )
  locate( 258)  =  locint ( unused )
  locate( 259)  =  locint ( icat   )
  locate( 260)  =  locint ( numnvo )
  locate( 261)  =  locint ( unused )
  locate( 262)  =  locint ( nenerg )
  locate( 263)  =  locint ( isw    )
  locate( 264)  =  locint ( itest  )
  locate( 265)  =  locint ( idist  )
  locate( 266)  =  locint ( x      )
  locate( 267)  =  locint ( ykm    )
  locate( 268)  =  locint ( km     )
  locate( 269)  =  locint ( xk     )
  locate( 270)  =  locint ( xm     )
  locate( 271)  =  locint ( weight )
  locate( 272)  =  locint ( iwtent )
  locate( 273)  =  locint ( con1   )
  locate( 274)  =  locint ( iskip  )
  locate( 275)  =  locint ( zinf   )
  locate( 276)  =  locint ( eta    )
  locate( 277)  =  locint ( nhist  )
  locate( 278)  =  locint ( stailm )
  locate( 279)  =  locint ( stailk )
  locate( 280)  =  locint ( xmax   )
  locate( 281)  =  locint ( koutvp )
  locate( 282)  =  locint ( bnrg   )
  locate( 283)  =  locint ( sconst )
  locate( 284)  =  locint ( cnvhst )
  locate( 285)  =  locint ( sfd    )
  locate( 286)  =  locint ( qfd    )
  locate( 287)  =  locint ( semaux )
  locate( 288)  =  locint ( ibsout )
  locate( 289)  =  locint ( bvalue )
  locate( 290)  =  locint ( sptacs )
  locate( 291)  =  locint ( kswtyp )
  locate( 292)  =  locint ( modswt )
  locate( 293)  =  locint ( kbegsw )
  locate( 294)  =  locint ( lastsw )
  locate( 295)  =  locint ( kentnb )
  locate( 296)  =  locint ( nbhdsw )
  locate( 297)  =  locint ( topen  )
  locate( 298)  =  locint ( crit   )
  locate( 299)  =  locint ( kdepsw )
  locate( 300)  =  locint ( tdns   )
  locate( 301)  =  locint ( isourc )
  locate( 302)  =  locint ( energy )
  locate( 303)  =  locint ( iardub )
  locate( 304)  =  locint ( ardube )
  locate( 305)  =  locint ( nonlad )
  locate( 306)  =  locint ( nonle  )
  locate( 307)  =  locint ( vnonl  )
  locate( 308)  =  locint ( curr   )
  locate( 309)  =  locint ( anonl  )
  locate( 310)  =  locint ( vecnl1 )
  locate( 311)  =  locint ( vecnl2 )
  locate( 312)  =  locint ( brnonl )
  locate( 313)  =  locint ( vzero  )
  locate( 314)  =  locint ( ilast  )
  locate( 315)  =  locint ( nltype )
  locate( 316)  =  locint ( kupl   )
  locate( 317)  =  locint ( nlsub  )
  locate( 318)  =  locint ( cursub )
  locate( 319)  =  locint ( cchar  )
  locate( 320)  =  locint ( vchar  )
  locate( 321)  =  locint ( gslope )
  locate( 322)  =  locint ( kk     )
  locate( 323)  =  locint ( c      )
  locate( 324)  =  locint ( tr     )
  locate( 325)  =  locint ( tx     )
  locate( 326)  =  locint ( r      )
  locate( 327)  =  locint ( nr     )
  locate( 328)  =  locint ( length )
  locate( 329)  =  locint ( cik    )
  locate( 330)  =  locint ( ci     )
  locate( 331)  =  locint ( ck     )
  locate( 332)  =  locint ( swname )
  locate( 333)  =  locint ( ibrnch )
  locate( 334)  =  locint ( jbrnch )
  locate( 335)  =  locint ( tstop  )
  locate( 336)  =  locint ( nonlk  )
  locate( 337)  =  locint ( nonlm  )
  locate( 338)  =  locint ( spum   )
  locate( 339)  =  locint ( kks    )
  locate( 340)  =  locint ( kknonl )
  locate( 341)  =  locint ( znonl  )
  locate( 342)  =  locint ( znonlb )
  locate( 343)  =  locint ( znonlc )
  locate( 344)  =  locint ( finit  )
  locate( 345)  =  locint ( ksub   )
  locate( 346)  =  locint ( msub   )
  locate( 347)  =  locint ( isubeg )
  locate( 348)  =  locint ( litype )
  locate( 349)  =  locint ( imodel )
  locate( 350)  =  locint ( kbus   )
  locate( 351)  =  locint ( mbus   )
  locate( 352)  =  locint ( kodebr )
  locate( 353)  =  locint ( cki    )
  locate( 354)  =  locint ( ckkjm  )
  locate( 355)  =  locint ( indhst )
  locate( 356)  =  locint ( kodsem )
  locate( 357)  =  locint ( brname )
  locate( 358)  =  locint ( iform  )
  locate( 359)  =  locint ( node   )
  locate( 360)  =  locint ( crest  )
  locate( 361)  =  locint ( time1  )
  locate( 362)  =  locint ( time2  )
  locate( 363)  =  locint ( tstart )
  locate( 364)  =  locint ( sfreq  )
  locate( 365)  =  locint ( kmswit )
  locate( 366)  =  locint ( nextsw )
  locate( 367)  =  locint ( rmfd   )
  locate( 368)  =  locint ( cikfd  )
  locate( 369)  =  locint ( imfd   )
  locate( 370)  =  locint ( tclose )
  locate( 371)  =  locint ( adelay )
  locate( 372)  =  locint ( kpos   )
  locate( 373)  =  locint ( e      )
  locate( 374)  =  locint ( f      )
  locate( 375)  =  locint ( kssfrq )
  locate( 376)  =  locint ( kode   )
  locate( 377)  =  locint ( kpsour )
  locate( 378)  =  locint ( volti  )
  locate( 379)  =  locint ( voltk  )
  locate( 380)  =  locint ( volt   )
  locate( 381)  =  locint ( bus    )
  locate( 382)  =  locint ( eld    )
  locate( 383)  =  locint ( elaf   )
  locate( 384)  =  locint ( elf    )
  locate( 385)  =  locint ( elakd  )
  locate( 386)  =  locint ( elfkd  )
  locate( 387)  =  locint ( elkd   )
  locate( 388)  =  locint ( elq    )
  locate( 389)  =  locint ( elag   )
  locate( 390)  =  locint ( elg    )
  locate( 391)  =  locint ( elakq  )
  locate( 392)  =  locint ( elgkq  )
  locate( 393)  =  locint ( elkq   )
  locate( 394)  =  locint ( el0    )
  locate( 395)  =  locint ( ra     )
  locate( 396)  =  locint ( rf     )
  locate( 397)  =  locint ( rkd    )
  locate( 398)  =  locint ( rg     )
  locate( 399)  =  locint ( rkq    )
  locate( 400)  =  locint ( r0     )
  locate( 401)  =  locint ( agline )
  locate( 402)  =  locint ( rat1   )
  locate( 403)  =  locint ( smoutp )
  locate( 404)  =  locint ( smoutq )
  locate( 405)  =  locint ( teg    )
  locate( 406)  =  locint ( texc   )
  locate( 407)  =  locint ( cnp    )
  locate( 408)  =  locint ( a22    )
  locate( 409)  =  locint ( a12    )
  locate( 410)  =  locint ( a21    )
  locate( 411)  =  locint ( ac     )
  locate( 412)  =  locint ( ai     )
  locate( 413)  =  locint ( at     )
  locate( 414)  =  locint ( ah     )
  locate( 415)  =  locint ( xay    )
  locate( 416)  =  locint ( cu     )
  locate( 417)  =  locint ( cv     )
  locate( 418)  =  locint ( dsat   )
  locate( 419)  =  locint ( qsat   )
  locate( 420)  =  locint ( acr    )
  locate( 421)  =  locint ( ce     )
  locate( 422)  =  locint ( dsr    )
  locate( 423)  =  locint ( dsd    )
  locate( 424)  =  locint ( hico   )
  locate( 425)  =  locint ( dsm    )
  locate( 426)  =  locint ( hsp    )
  locate( 427)  =  locint ( power  )
  locate( 428)  =  locint ( extrs  )
  locate( 429)  =  locint ( histq  )
  locate( 430)  =  locint ( histr  )
  locate( 431)  =  locint ( yfor   )
  locate( 432)  =  locint ( zsk    )
  locate( 433)  =  locint ( y      )
  locate( 434)  =  locint ( tork   )
  locate( 435)  =  locint ( temp   )
  locate( 436)  =  locint ( z      )
  locate( 437)  =  locint ( x1     )
  locate( 438)  =  locint ( sqrt3  )
  locate( 439)  =  locint ( asqrt3 )
  locate( 440)  =  locint ( sqrt32 )
  locate( 441)  =  locint ( thtw   )
  locate( 442)  =  locint ( athtw  )
  locate( 443)  =  locint ( radeg  )
  locate( 444)  =  locint ( omdt   )
  locate( 445)  =  locint ( factom )
  locate( 446)  =  locint ( damrat )
  locate( 447)  =  locint ( isat   )
  locate( 448)  =  locint ( ised   )
  locate( 449)  =  locint ( iseq   )
  locate( 450)  =  locint ( imdual )
  locate( 451)  =  locint ( iconfg )
  locate( 452)  =  locint ( kmac   )
  locate( 453)  =  locint ( kexc   )
  locate( 454)  =  locint ( numas  )
  locate( 455)  =  locint ( nodma  )
  locate( 456)  =  locint ( nodmb  )
  locate( 457)  =  locint ( nodmc  )
  locate( 458)  =  locint ( jasmit )
  locate( 459)  =  locint ( jsmtor )
  locate( 460)  =  locint ( jexcit )
  locate( 461)  =  locint ( isloc  )
  locate( 462)  =  locint ( noutsm )
  locate( 463)  =  locint ( ismout )
  locate( 464)  =  locint ( mfirst )
  locate( 465)  =  locint ( limass )
  locate( 466)  =  locint ( nst    )
  locate( 467)  =  locint ( itold  )
  locate( 468)  =  locint ( ibrold )
  locate( 469)  =  locint ( busum  )
  locate( 470)  =  locint ( ptheta )
  locate( 471)  =  locint ( zthevr )
  locate( 472)  =  locint ( vinp   )
  locate( 473)  =  locint ( zthevs )
  locate( 474)  =  locint ( umcur  )
  locate( 475)  =  locint ( con    )
  locate( 476)  =  locint ( dumvec )
  locate( 477)  =  locint ( dummat )
  locate( 478)  =  locint ( date   )
  locate( 479)  =  locint ( clock  )
  locate( 480)  =  locint ( pi     )
  locate( 481)  =  locint ( sroot2 )
  locate( 482)  =  locint ( sroot3 )
  locate( 483)  =  locint ( omegrf )
  locate( 484)  =  locint ( inpu   )
  locate( 485)  =  locint ( numbus )
  locate( 486)  =  locint ( ncltot )
  locate( 487)  =  locint ( ndum   )
  locate( 488)  =  locint ( initum )
  locate( 489)  =  locint ( iureac )
  locate( 490)  =  locint ( iugpar )
  locate( 491)  =  locint ( iufpar )
  locate( 492)  =  locint ( iuhist )
  locate( 493)  =  locint ( iuumrp )
  locate( 494)  =  locint ( iunod1 )
  locate( 495)  =  locint ( iunod2 )
  locate( 496)  =  locint ( iujclt )
  locate( 497)  =  locint ( iujclo )
  locate( 498)  =  locint ( iujtyp )
  locate( 499)  =  locint ( iunodo )
  locate( 500)  =  locint ( iujtmt )
  locate( 501)  =  locint ( iuhism )
  locate( 502)  =  locint ( iuomgm )
  locate( 503)  =  locint ( iuomld )
  locate( 504)  =  locint ( iutham )
  locate( 505)  =  locint ( iuredu )
  locate( 506)  =  locint ( iureds )
  locate( 507)  =  locint ( iuflds )
  locate( 508)  =  locint ( iufldr )
  locate( 509)  =  locint ( iurequ )
  locate( 510)  =  locint ( iuflqs )
  locate( 511)  =  locint ( iuflqr )
  locate( 512)  =  locint ( iujcds )
  locate( 513)  =  locint ( iujcqs )
  locate( 514)  =  locint ( iuflxd )
  locate( 515)  =  locint ( iuflxq )
  locate( 516)  =  locint ( iunppa )
  locate( 517)  =  locint ( iurotm )
  locate( 518)  =  locint ( iuncld )
  locate( 519)  =  locint ( iunclq )
  locate( 520)  =  locint ( iujtqo )
  locate( 521)  =  locint ( iujomo )
  locate( 522)  =  locint ( iujtho )
  locate( 523)  =  locint ( iureqs )
  locate( 524)  =  locint ( iuepso )
  locate( 525)  =  locint ( iudcoe )
  locate( 526)  =  locint ( iukcoi )
  locate( 527)  =  locint ( iuvolt )
  locate( 528)  =  locint ( iuangl )
  locate( 529)  =  locint ( iunodf )
  locate( 530)  =  locint ( iunodm )
  locate( 531)  =  locint ( iukumo )
  locate( 532)  =  locint ( iujumo )
  locate( 533)  =  locint ( iuumou )
  locate( 534)  =  locint ( nclfix )
  locate( 535)  =  locint ( numfix )
  locate( 536)  =  locint ( iotfix )
  locate( 537)  =  locint ( ibsfix )
  locate( 538)  =  locint ( ksubum )
  locate( 539)  =  locint ( nsmach )
  locate( 540)  =  locint ( istart )
  locate( 541)  =  locint ( karray )
  locate( 542)  =  locint ( rampcn )
  locate( 543)  =  locint ( rampsl )
  locate( 544)  =  locint ( kyramp )
  locate( 545)  =  locint ( texpar )
  locate( 546)  =  locint ( fendrp )
  locate( 547)  =  locint ( tminrp )
  locate( 548)  =  locint ( tmaxrp )
  locate( 549)  =  locint ( tbegrp )
  locate( 550)  =  locint ( tendrp )
  locate( 551)  =  locint ( fbegrp )
  locate( 552)  =  locint ( tbreak )
  locate( 553)  =  locint ( indxrp )
  locate( 554)  =  locint ( ivec   )
  locate( 555)  =  locint ( iascii )
  locate( 556)  =  locint ( numsym )
  locate( 557)  =  locint ( jjroll )
  locate( 558)  =  locint ( itexp  )
  locate( 559)  =  locint ( labels )
  locate( 560)  =  locint ( maxarg )
  locate( 561)  =  locint ( kilper )
  locate( 562)  =  locint ( kfile5 )
  locate( 563)  =  locint ( kverfy )
  locate( 564)  =  locint ( ibegcl )
  locate( 565)  =  locint ( iendcl )
  locate( 566)  =  locint ( lidnt1 )
  locate( 567)  =  locint ( lidnt2 )
  locate( 568)  =  locint ( linnow )
  locate( 569)  =  locint ( linspn )
  locate( 570)  =  locint ( numcrd )
  locate( 571)  =  locint ( munit5 )
  locate( 572)  =  locint ( indbuf )
  locate( 573)  =  locint ( indbeg )
  locate( 574)  =  locint ( mflush )
  locate( 575)  =  locint ( newvec )
  locate( 576)  =  locint ( munit6 )
  locate( 577)  =  locint ( lserlc )
  locate( 578)  =  locint ( kserlc )
  locate( 579)  =  locint ( kbrser )
  locate( 580)  =  locint ( lockbr )
  locate( 581)  =  locint ( iprspy )
  locate( 582)  =  locint ( monitr )
  locate( 583)  =  locint ( monits )
  locate( 584)  =  locint ( locate )
  locate( 585)  =  locint ( nline  )
  locate( 586)  =  locint ( kwtspy )
  locate( 587)  =  locint ( kbreak )
  locate( 588)  =  locint ( limbuf )
  locate( 589)  =  locint ( inchlp )
  locate( 590)  =  locint ( ksymbl )
  locate( 591)  =  locint ( kopyit )
  locate( 592)  =  locint ( kslowr )
  locate( 593)  =  locint ( limcrd )
  locate( 594)  =  locint ( looprp )
  locate( 595)  =  locint ( n10rmp )
  locate( 596)  =  locint ( memrmp )
  locate( 597)  =  locint ( kar1   )
  locate( 598)  =  locint ( kar2   )
  locate( 599)  =  locint ( numrmp )
  locate( 600)  =  locint ( luntsp )
  locate( 601)  =  locint ( logvar )
  locate( 602)  =  locint ( filext )
  locate( 603)  =  locint ( symb   )
  locate( 604)  =  locint ( col    )
  locate( 605)  =  locint ( bytfnd )
  locate( 606)  =  locint ( char1  )
  locate( 607)  =  locint ( symbrp )
  locate( 608)  =  locint ( chard4 )
  locate( 609)  =  locint ( bytbuf )
  locate( 610)  =  locint ( buff77 )
  locate( 611)  =  locint ( file6b )
  locate( 612)  =  locint ( file6  )
  locate( 613)  =  locint ( blan80 )
  locate( 614)  =  locint ( prom80 )
  locate( 615)  =  locint ( digit  )
  locate( 616)  =  locint ( iac    )
  locate( 617)  =  locint ( idctcs )
  locate( 618)  =  locint ( ipl    )
  locate( 619)  =  locint ( ipr    )
  locate( 620)  =  locint ( ixr    )
  locate( 621)  =  locint ( jpl    )
  locate( 622)  =  locint ( jpr    )
  locate( 623)  =  locint ( kint   )
  locate( 624)  =  locint ( kout   )
  locate( 625)  =  locint ( nds    )
  locate( 626)  =  locint ( nkn    )
  locate( 627)  =  locint ( nmax   )
  locate( 628)  =  locint ( nuk    )
  locate( 629)  =  locint ( kwrite )
  locate( 630)  =  locint ( kpr    )
  locate( 631)  =  locint ( kpl    )
  locate( 632)  =  locint ( mxtacw )
  locate( 633)  =  locint ( iptacw )
  locate( 634)  =  locint ( nhst   )
  locate( 635)  =  locint ( kvin   )
  locate( 636)  =  locint ( kvou   )
  locate( 637)  =  locint ( kvxx   )
  locate( 638)  =  locint ( icsup  )
  locate( 639)  =  locint ( nxic   )
  locate( 640)  =  locint ( kksj   )
  locate( 641)  =  locint ( kksk   )
  locate( 642)  =  locint ( kkfst  )
  locate( 643)  =  locint ( kkni   )
  locate( 644)  =  locint ( kkhst  )
  locate( 645)  =  locint ( kifls  )
  locate( 646)  =  locint ( kidum  )
  locate( 647)  =  locint ( kslim1 )
  locate( 648)  =  locint ( kslim2 )
  locate( 649)  =  locint ( kslim3 )
  locate( 650)  =  locint ( kpac1r )
  locate( 651)  =  locint ( kpac1i )
  locate( 652)  =  locint ( kpac2r )
  locate( 653)  =  locint ( kpac2i )
  locate( 654)  =  locint ( kalksx )
  locate( 655)  =  locint ( kilms1 )
  locate( 656)  =  locint ( kilms2 )
  locate( 657)  =  locint ( kdumj  )
  locate( 658)  =  locint ( kdumk  )
  locate( 659)  =  locint ( kkzj   )
  locate( 660)  =  locint ( kkzk   )
  locate( 661)  =  locint ( kiflz  )
  locate( 662)  =  locint ( kgnz   )
  locate( 663)  =  locint ( kzlim1 )
  locate( 664)  =  locint ( kzlim2 )
  locate( 665)  =  locint ( kalkzx )
  locate( 666)  =  locint ( kilmz1 )
  locate( 667)  =  locint ( kilmz2 )
  locate( 668)  =  locint ( kksus  )
  locate( 669)  =  locint ( kalksu )
  locate( 670)  =  locint ( kiuty  )
  locate( 671)  =  locint ( kud1   )
  locate( 672)  =  locint ( kud2   )
  locate( 673)  =  locint ( kud3   )
  locate( 674)  =  locint ( kud4   )
  locate( 675)  =  locint ( kud5   )
  locate( 676)  =  locint ( kaliu  )
  locate( 677)  =  locint ( ktysup )
  locate( 678)  =  locint ( kjsup  )
  locate( 679)  =  locint ( kksup  )
  locate( 680)  =  locint ( kspvar )
  locate( 681)  =  locint ( kopsup )
  locate( 682)  =  locint ( kfnsup )
  locate( 683)  =  locint ( krgsup )
  locate( 684)  =  locint ( kprsup )
  locate( 685)  =  locint ( ktypdv )
  locate( 686)  =  locint ( kkdj   )
  locate( 687)  =  locint ( kkdk   )
  locate( 688)  =  locint ( kgndev )
  locate( 689)  =  locint ( kdev1  )
  locate( 690)  =  locint ( kdev2  )
  locate( 691)  =  locint ( kldev1 )
  locate( 692)  =  locint ( kldev2 )
  locate( 693)  =  locint ( kkdus  )
  locate( 694)  =  locint ( kalkdu )
  locate( 695)  =  locint ( ktbdev )
  locate( 696)  =  locint ( kpn    )
  locate( 697)  =  locint ( kpd    )
  locate( 698)  =  locint ( kxhst  )
  locate( 699)  =  locint ( khscr  )
  locate( 700)  =  locint ( khsci  )
  locate( 701)  =  locint ( kilim1 )
  locate( 702)  =  locint ( kilim2 )
  locate( 703)  =  locint ( krowcs )
  locate( 704)  =  locint ( krhsde )
  locate( 705)  =  locint ( kvlim1 )
  locate( 706)  =  locint ( kvlim2 )
  locate( 707)  =  locint ( kkxic  )
  locate( 708)  =  locint ( kawkcs )
  locate( 709)  =  locint ( kxar   )
  locate( 710)  =  locint ( kxai   )
  locate( 711)  =  locint ( kbwkcs )
  locate( 712)  =  locint ( kxtcs  )
  locate( 713)  =  locint ( klntab )
  locate( 714)  =  locint ( klmxic )
  locate( 715)  =  locint ( kcolcs )
  locate( 716)  =  locint ( katcs  )
  locate( 717)  =  locint ( kbtcs  )
  locate( 718)  =  locint ( kjout  )
  locate( 719)  =  locint ( kkout  )
  locate( 720)  =  locint ( kxmncs )
  locate( 721)  =  locint ( ktxmn  )
  locate( 722)  =  locint ( kxmxcs )
  locate( 723)  =  locint ( ktxmx  )
  locate( 724)  =  locint ( klnout )
  locate( 725)  =  locint ( ekbuf  )
  locate( 726)  =  locint ( ektemp )
  locate( 727)  =  locint ( errchk )
  locate( 728)  =  locint ( solrsv )
  locate( 729)  =  locint ( solisv )
  locate( 730)  =  locint ( nitera )
  locate( 731)  =  locint ( nekreq )
  locate( 732)  =  locint ( nekcod )
  return
end subroutine locatn
!
!     data block.
!
block data
   include 'dekspy.ftn'
   data symb(  1) / 'bus1  '/,  ivec(  1) /  0/,  iascii(  1) /1/
   data symb(  2) / 'bus2  '/,  ivec(  2) /  0/,  iascii(  2) /1/
   data symb(  3) / 'bus3  '/,  ivec(  3) /  0/,  iascii(  3) /1/
   data symb(  4) / 'bus4  '/,  ivec(  4) /  0/,  iascii(  4) /1/
   data symb(  5) / 'bus5  '/,  ivec(  5) /  0/,  iascii(  5) /1/
   data symb(  6) / 'bus6  '/,  ivec(  6) /  0/,  iascii(  6) /1/
   data symb(  7) / 'trash '/,  ivec(  7) /  0/,  iascii(  7) /1/
   data symb(  8) / 'blank '/,  ivec(  8) /  0/,  iascii(  8) /1/
   data symb(  9) / 'terra '/,  ivec(  9) /  0/,  iascii(  9) /1/
   data symb( 10) / 'userid'/,  ivec( 10) /  0/,  iascii( 10) /1/
   data symb( 11) / 'branch'/,  ivec( 11) /  0/,  iascii( 11) /1/
   data symb( 12) / 'copy  '/,  ivec( 12) /  0/,  iascii( 12) /1/
   data symb( 13) / 'csepar'/,  ivec( 13) /  0/,  iascii( 13) /1/
   data symb( 14) / 'chcont'/,  ivec( 14) /  0/,  iascii( 14) /1/
   data symb( 15) / 'texcol'/,  ivec( 15) /  1/,  iascii( 15) /1/
   data symb( 16) / 'texta6'/,  ivec( 16) /  1/,  iascii( 16) /1/
   data symb( 17) / 'date1 '/,  ivec( 17) /  1/,  iascii( 17) /1/
   data symb( 18) / 'tclock'/,  ivec( 18) /  1/,  iascii( 18) /1/
   data symb( 19) / 'vstacs'/,  ivec( 19) /  1/,  iascii( 19) /1/
   data symb( 20) / 'abuff '/,  ivec( 20) /  1/,  iascii( 20) /1/
   data symb( 21) / 'ci1   '/,  ivec( 21) /  0/,  iascii( 21) /0/
   data symb( 22) / 'ck1   '/,  ivec( 22) /  0/,  iascii( 22) /0/
   data symb( 23) / 'deltat'/,  ivec( 23) /  0/,  iascii( 23) /0/
   data symb( 24) / 'delta2'/,  ivec( 24) /  0/,  iascii( 24) /0/
   data symb( 25) / 'freqcs'/,  ivec( 25) /  0/,  iascii( 25) /0/
   data symb( 26) / 'epsiln'/,  ivec( 26) /  0/,  iascii( 26) /0/
   data symb( 27) / 'xunits'/,  ivec( 27) /  0/,  iascii( 27) /0/
   data symb( 28) / 'aincr '/,  ivec( 28) /  0/,  iascii( 28) /0/
   data symb( 29) / 'xmaxmx'/,  ivec( 29) /  0/,  iascii( 29) /0/
   data symb( 30) / 'znvref'/,  ivec( 30) /  0/,  iascii( 30) /0/
   data symb( 31) / 'epszno'/,  ivec( 31) /  0/,  iascii( 31) /0/
   data symb( 32) / 'epwarn'/,  ivec( 32) /  0/,  iascii( 32) /0/
   data symb( 33) / 'epstop'/,  ivec( 33) /  0/,  iascii( 33) /0/
   data symb( 34) / 't     '/,  ivec( 34) /  0/,  iascii( 34) /0/
   data symb( 35) / 'hertz '/,  ivec( 35) /  0/,  iascii( 35) /0/
   data symb( 36) / 'tolmat'/,  ivec( 36) /  0/,  iascii( 36) /0/
   data symb( 37) / 'twopi '/,  ivec( 37) /  0/,  iascii( 37) /0/
   data symb( 38) / 'tmax  '/,  ivec( 38) /  0/,  iascii( 38) /0/
   data symb( 39) / 'omega '/,  ivec( 39) /  0/,  iascii( 39) /0/
   data symb( 40) / 'copt  '/,  ivec( 40) /  0/,  iascii( 40) /0/
   data symb( 41) / 'xopt  '/,  ivec( 41) /  0/,  iascii( 41) /0/
   data symb( 42) / 'szplt '/,  ivec( 42) /  0/,  iascii( 42) /0/
   data symb( 43) / 'szbed '/,  ivec( 43) /  0/,  iascii( 43) /0/
   data symb( 44) / 'sglfir'/,  ivec( 44) /  0/,  iascii( 44) /0/
   data symb( 45) / 'sigmax'/,  ivec( 45) /  0/,  iascii( 45) /0/
   data symb( 46) / 'epsuba'/,  ivec( 46) /  0/,  iascii( 46) /0/
   data symb( 47) / 'epdgel'/,  ivec( 47) /  0/,  iascii( 47) /0/
   data symb( 48) / 'epomeg'/,  ivec( 48) /  0/,  iascii( 48) /0/
   data symb( 49) / 'fminfs'/,  ivec( 49) /  0/,  iascii( 49) /0/
   data symb( 50) / 'delffs'/,  ivec( 50) /  0/,  iascii( 50) /0/
   data symb( 51) / 'fmaxfs'/,  ivec( 51) /  0/,  iascii( 51) /0/
   data symb( 52) / 'tenerg'/,  ivec( 52) /  0/,  iascii( 52) /0/
   data symb( 53) / 'begmax'/,  ivec( 53) /  1/,  iascii( 53) /0/
   data symb( 54) / 'tenm3 '/,  ivec( 54) /  0/,  iascii( 54) /0/
   data symb( 55) / 'tenm6 '/,  ivec( 55) /  0/,  iascii( 55) /0/
   data symb( 56) / 'unity '/,  ivec( 56) /  0/,  iascii( 56) /0/
   data symb( 57) / 'onehaf'/,  ivec( 57) /  0/,  iascii( 57) /0/
   data symb( 58) / 'peaknd'/,  ivec( 58) /  1/,  iascii( 58) /0/
   data symb( 59) / 'fltinf'/,  ivec( 59) /  0/,  iascii( 59) /0/
   data symb( 60) / 'flzero'/,  ivec( 60) /  0/,  iascii( 60) /0/
   data symb( 61) / 'degmin'/,  ivec( 61) /  0/,  iascii( 61) /0/
   data symb( 62) / 'degmax'/,  ivec( 62) /  0/,  iascii( 62) /0/
   data symb( 63) / 'statfr'/,  ivec( 63) /  0/,  iascii( 63) /0/
   data symb( 64) / 'voltbc'/,  ivec( 64) /  1/,  iascii( 64) /0/
   data symb( 65) / 'flstat'/,  ivec( 65) /  1/,  iascii( 65) /0/
   data symb( 66) / 'dtnext'/,  ivec( 66) /  1/,  iascii( 66) /0/
   data symb( 67) / 'angle '/,  ivec( 67) /  0/,  iascii( 67) /0/
   data symb( 68) / 'pu    '/,  ivec( 68) /  0/,  iascii( 68) /0/
   data symb( 69) / 'seedr '/,  ivec( 69) /  0/,  iascii( 69) /0/
   data symb( 70) / 'speedl'/,  ivec( 70) /  0/,  iascii( 70) /0/
   data symb( 71) / 'kstart'/,  ivec( 71) /  0/,  iascii( 71) /0/
   data symb( 72) / 'knt   '/,  ivec( 72) /  0/,  iascii( 72) /0/
   data symb( 73) / 'kbase '/,  ivec( 73) /  0/,  iascii( 73) /0/
   data symb( 74) / 'ltdelt'/,  ivec( 74) /  0/,  iascii( 74) /0/
   data symb( 75) / 'unused'/,  ivec( 75) /  0/,  iascii( 75) /0/
   data symb( 76) / 'mtape '/,  ivec( 76) /  0/,  iascii( 76) /0/
   data symb( 77) / 'lunit1'/,  ivec( 77) /  0/,  iascii( 77) /0/
   data symb( 78) / 'lunit2'/,  ivec( 78) /  0/,  iascii( 78) /0/
   data symb( 79) / 'lunit3'/,  ivec( 79) /  0/,  iascii( 79) /0/
   data symb( 80) / 'lunit4'/,  ivec( 80) /  0/,  iascii( 80) /0/
   data symb( 81) / 'lunit5'/,  ivec( 81) /  0/,  iascii( 81) /0/
   data symb( 82) / 'lunit6'/,  ivec( 82) /  0/,  iascii( 82) /0/
   data symb( 83) / 'lunit7'/,  ivec( 83) /  0/,  iascii( 83) /0/
   data symb( 84) / 'lunit8'/,  ivec( 84) /  0/,  iascii( 84) /0/
   data symb( 85) / 'lunit9'/,  ivec( 85) /  0/,  iascii( 85) /0/
   data symb( 86) / 'lunt10'/,  ivec( 86) /  0/,  iascii( 86) /0/
   data symb( 87) / 'lunt11'/,  ivec( 87) /  0/,  iascii( 87) /0/
   data symb( 88) / 'lunt12'/,  ivec( 88) /  0/,  iascii( 88) /0/
   data symb( 89) / 'lunt13'/,  ivec( 89) /  0/,  iascii( 89) /0/
   data symb( 90) / 'lunt14'/,  ivec( 90) /  0/,  iascii( 90) /0/
   data symb( 91) / 'lunt15'/,  ivec( 91) /  0/,  iascii( 91) /0/
   data symb( 92) / 'nexout'/,  ivec( 92) /  1/,  iascii( 92) /0/
   data symb( 93) / 'nright'/,  ivec( 93) /  0/,  iascii( 93) /0/
   data symb( 94) / 'nfrfld'/,  ivec( 94) /  0/,  iascii( 94) /0/
   data symb( 95) / 'kolbeg'/,  ivec( 95) /  0/,  iascii( 95) /0/
   data symb( 96) / 'kprchg'/,  ivec( 96) /  1/,  iascii( 96) /0/
   data symb( 97) / 'multpr'/,  ivec( 97) /  1/,  iascii( 97) /0/
   data symb( 98) / 'ipntv '/,  ivec( 98) /  1/,  iascii( 98) /0/
   data symb( 99) / 'indtv '/,  ivec( 99) /  1/,  iascii( 99) /0/
   data symb(100) / 'lstat '/,  ivec(100) /  1/,  iascii(100) /0/
   data symb(101) / 'nbyte '/,  ivec(101) /  1/,  iascii(101) /0/
   data symb(102) / 'lunsav'/,  ivec(102) /  1/,  iascii(102) /0/
   data symb(103) / 'iprsov'/,  ivec(103) /  1/,  iascii(103) /0/
   data symb(104) / 'icheck'/,  ivec(104) /  0/,  iascii(104) /0/
   data symb(105) / 'unused'/,  ivec(105) /  0/,  iascii(105) /0/
   data symb(106) / 'iend  '/,  ivec(106) /  0/,  iascii(106) /0/
   data symb(107) / 'iline '/,  ivec(107) /  0/,  iascii(107) /0/
   data symb(108) / 'inonl '/,  ivec(108) /  0/,  iascii(108) /0/
   data symb(109) / 'iold  '/,  ivec(109) /  0/,  iascii(109) /0/
   data symb(110) / 'iout  '/,  ivec(110) /  0/,  iascii(110) /0/
   data symb(111) / 'iprint'/,  ivec(111) /  0/,  iascii(111) /0/
   data symb(112) / 'ipunch'/,  ivec(112) /  0/,  iascii(112) /0/
   data symb(113) / 'iread '/,  ivec(113) /  0/,  iascii(113) /0/
   data symb(114) / 'kol132'/,  ivec(114) /  0/,  iascii(114) /0/
   data symb(115) / 'istep '/,  ivec(115) /  0/,  iascii(115) /0/
   data symb(116) / 'unused'/,  ivec(116) /  0/,  iascii(116) /0/
   data symb(117) / 'itype '/,  ivec(117) /  0/,  iascii(117) /0/
   data symb(118) / 'it1   '/,  ivec(118) /  0/,  iascii(118) /0/
   data symb(119) / 'it2   '/,  ivec(119) /  0/,  iascii(119) /0/
   data symb(120) / 'iupper'/,  ivec(120) /  0/,  iascii(120) /0/
   data symb(121) / 'izero '/,  ivec(121) /  0/,  iascii(121) /0/
   data symb(122) / 'kcount'/,  ivec(122) /  0/,  iascii(122) /0/
   data symb(123) / 'istead'/,  ivec(123) /  0/,  iascii(123) /0/
   data symb(124) / 'unused'/,  ivec(124) /  0/,  iascii(124) /0/
   data symb(125) / 'ldata '/,  ivec(125) /  0/,  iascii(125) /0/
   data symb(126) / 'lbrnch'/,  ivec(126) /  0/,  iascii(126) /0/
   data symb(127) / 'limtxf'/,  ivec(127) /  0/,  iascii(127) /0/
   data symb(128) / 'mdebug'/,  ivec(128) /  0/,  iascii(128) /0/
   data symb(129) / 'lexct '/,  ivec(129) /  0/,  iascii(129) /0/
   data symb(130) / 'lbus  '/,  ivec(130) /  0/,  iascii(130) /0/
   data symb(131) / 'lymat '/,  ivec(131) /  0/,  iascii(131) /0/
   data symb(132) / 'lswtch'/,  ivec(132) /  0/,  iascii(132) /0/
   data symb(133) / 'lnonl '/,  ivec(133) /  0/,  iascii(133) /0/
   data symb(134) / 'lchar '/,  ivec(134) /  0/,  iascii(134) /0/
   data symb(135) / 'm4plot'/,  ivec(135) /  0/,  iascii(135) /0/
   data symb(136) / 'lpast '/,  ivec(136) /  0/,  iascii(136) /0/
   data symb(137) / 'lsmat '/,  ivec(137) /  0/,  iascii(137) /0/
   data symb(138) / 'iplot '/,  ivec(138) /  0/,  iascii(138) /0/
   data symb(139) / 'ncomp '/,  ivec(139) /  0/,  iascii(139) /0/
   data symb(140) / 'nv    '/,  ivec(140) /  0/,  iascii(140) /0/
   data symb(141) / 'lcomp '/,  ivec(141) /  0/,  iascii(141) /0/
   data symb(142) / 'numsm '/,  ivec(142) /  0/,  iascii(142) /0/
   data symb(143) / 'ifdep '/,  ivec(143) /  0/,  iascii(143) /0/
   data symb(144) / 'ltails'/,  ivec(144) /  0/,  iascii(144) /0/
   data symb(145) / 'lfdep '/,  ivec(145) /  0/,  iascii(145) /0/
   data symb(146) / 'lwt   '/,  ivec(146) /  0/,  iascii(146) /0/
   data symb(147) / 'last  '/,  ivec(147) /  0/,  iascii(147) /0/
   data symb(148) / 'npower'/,  ivec(148) /  0/,  iascii(148) /0/
   data symb(149) / 'maxpe '/,  ivec(149) /  0/,  iascii(149) /0/
   data symb(150) / 'lpeak '/,  ivec(150) /  0/,  iascii(150) /0/
   data symb(151) / 'nout  '/,  ivec(151) /  0/,  iascii(151) /0/
   data symb(152) / 'iv    '/,  ivec(152) /  0/,  iascii(152) /0/
   data symb(153) / 'ineof '/,  ivec(153) /  0/,  iascii(153) /0/
   data symb(154) / 'ktrlsw'/,  ivec(154) /  1/,  iascii(154) /0/
   data symb(155) / 'num99 '/,  ivec(155) /  0/,  iascii(155) /0/
   data symb(156) / 'kpartb'/,  ivec(156) /  0/,  iascii(156) /0/
   data symb(157) / 'llbuff'/,  ivec(157) /  0/,  iascii(157) /0/
   data symb(158) / 'kanal '/,  ivec(158) /  0/,  iascii(158) /0/
   data symb(159) / 'nsmth '/,  ivec(159) /  0/,  iascii(159) /0/
   data symb(160) / 'ntcsex'/,  ivec(160) /  0/,  iascii(160) /0/
   data symb(161) / 'nstacs'/,  ivec(161) /  0/,  iascii(161) /0/
   data symb(162) / 'kloaep'/,  ivec(162) /  0/,  iascii(162) /0/
   data symb(163) / 'lastov'/,  ivec(163) /  0/,  iascii(163) /0/
   data symb(164) / 'ltacst'/,  ivec(164) /  0/,  iascii(164) /0/
   data symb(165) / 'lhist '/,  ivec(165) /  0/,  iascii(165) /0/
   data symb(166) / 'ifx   '/,  ivec(166) /  0/,  iascii(166) /0/
   data symb(167) / 'ndelta'/,  ivec(167) /  0/,  iascii(167) /0/
   data symb(168) / 'idelta'/,  ivec(168) /  0/,  iascii(168) /0/
   data symb(169) / 'inecho'/,  ivec(169) /  0/,  iascii(169) /0/
   data symb(170) / 'noutpr'/,  ivec(170) /  0/,  iascii(170) /0/
   data symb(171) / 'ktab  '/,  ivec(171) /  0/,  iascii(171) /0/
   data symb(172) / 'jflsos'/,  ivec(172) /  0/,  iascii(172) /0/
   data symb(173) / 'numdcd'/,  ivec(173) /  0/,  iascii(173) /0/
   data symb(174) / 'numum '/,  ivec(174) /  0/,  iascii(174) /0/
   data symb(175) / 'lspcum'/,  ivec(175) /  0/,  iascii(175) /0/
   data symb(176) / 'nphcas'/,  ivec(176) /  0/,  iascii(176) /0/
   data symb(177) / 'locz11'/,  ivec(177) /  0/,  iascii(177) /0/
   data symb(178) / 'locbr1'/,  ivec(178) /  0/,  iascii(178) /0/
   data symb(179) / 'ialter'/,  ivec(179) /  0/,  iascii(179) /0/
   data symb(180) / 'i_char'/,  ivec(180) /  0/,  iascii(180) /0/
   data symb(181) / 'ktref '/,  ivec(181) /  0/,  iascii(181) /0/
   data symb(182) / 'kph   '/,  ivec(182) /  0/,  iascii(182) /0/
   data symb(183) / 'kreqab'/,  ivec(183) /  0/,  iascii(183) /0/
   data symb(184) / 'ksat  '/,  ivec(184) /  0/,  iascii(184) /0/
   data symb(185) / 'memsav'/,  ivec(185) /  0/,  iascii(185) /0/
   data symb(186) / 'lisoff'/,  ivec(186) /  0/,  iascii(186) /0/
   data symb(187) / 'lspov4'/,  ivec(187) /  0/,  iascii(187) /0/
   data symb(188) / 'kburro'/,  ivec(188) /  0/,  iascii(188) /0/
   data symb(189) / 'iaverg'/,  ivec(189) /  0/,  iascii(189) /0/
   data symb(190) / 'lsiz23'/,  ivec(190) /  0/,  iascii(190) /0/
   data symb(191) / 'lsiz26'/,  ivec(191) /  0/,  iascii(191) /0/
   data symb(192) / 'numout'/,  ivec(192) /  0/,  iascii(192) /0/
   data symb(193) / 'moldat'/,  ivec(193) /  0/,  iascii(193) /0/
   data symb(194) / 'lsiz27'/,  ivec(194) /  0/,  iascii(194) /0/
   data symb(195) / 'ltlabl'/,  ivec(195) /  0/,  iascii(195) /0/
   data symb(196) / 'iwt   '/,  ivec(196) /  0/,  iascii(196) /0/
   data symb(197) / 'ifdep2'/,  ivec(197) /  0/,  iascii(197) /0/
   data symb(198) / 'idoubl'/,  ivec(198) /  0/,  iascii(198) /0/
   data symb(199) / 'ioutin'/,  ivec(199) /  0/,  iascii(199) /0/
   data symb(200) / 'ipun  '/,  ivec(200) /  0/,  iascii(200) /0/
   data symb(201) / 'jst   '/,  ivec(201) /  0/,  iascii(201) /0/
   data symb(202) / 'jst1  '/,  ivec(202) /  0/,  iascii(202) /0/
   data symb(203) / 'unused'/,  ivec(203) /  0/,  iascii(203) /0/
   data symb(204) / 'numsub'/,  ivec(204) /  0/,  iascii(204) /0/
   data symb(205) / 'maxzno'/,  ivec(205) /  0/,  iascii(205) /0/
   data symb(206) / 'kalplt'/,  ivec(206) /  0/,  iascii(206) /0/
   data symb(207) / 'niomax'/,  ivec(207) /  0/,  iascii(207) /0/
   data symb(208) / 'niamax'/,  ivec(208) /  0/,  iascii(208) /0/
   data symb(209) / 'ibr1  '/,  ivec(209) /  0/,  iascii(209) /0/
   data symb(210) / 'ifsem '/,  ivec(210) /  0/,  iascii(210) /0/
   data symb(211) / 'lfsem '/,  ivec(211) /  0/,  iascii(211) /0/
   data symb(212) / 'iadd  '/,  ivec(212) /  0/,  iascii(212) /0/
   data symb(213) / 'lfd   '/,  ivec(213) /  0/,  iascii(213) /0/
   data symb(214) / 'laux  '/,  ivec(214) /  0/,  iascii(214) /0/
   data symb(215) / 'iofgnd'/,  ivec(215) /  0/,  iascii(215) /0/
   data symb(216) / 'iofbnd'/,  ivec(216) /  0/,  iascii(216) /0/
   data symb(217) / 'unused'/,  ivec(217) /  0/,  iascii(217) /0/
   data symb(218) / 'jseedr'/,  ivec(218) /  0/,  iascii(218) /0/
   data symb(219) / 'modout'/,  ivec(219) /  0/,  iascii(219) /0/
   data symb(220) / 'iftail'/,  ivec(220) /  0/,  iascii(220) /0/
   data symb(221) / 'ipoint'/,  ivec(221) /  0/,  iascii(221) /0/
   data symb(222) / 'lpast2'/,  ivec(222) /  0/,  iascii(222) /0/
   data symb(223) / 'ncurr '/,  ivec(223) /  0/,  iascii(223) /0/
   data symb(224) / 'ioffd '/,  ivec(224) /  0/,  iascii(224) /0/
   data symb(225) / 'isplot'/,  ivec(225) /  0/,  iascii(225) /0/
   data symb(226) / 'isprin'/,  ivec(226) /  0/,  iascii(226) /0/
   data symb(227) / 'maxout'/,  ivec(227) /  0/,  iascii(227) /0/
   data symb(228) / 'ipos  '/,  ivec(228) /  0/,  iascii(228) /0/
   data symb(229) / 'unused'/,  ivec(229) /  0/,  iascii(229) /0/
   data symb(230) / 'unused'/,  ivec(230) /  0/,  iascii(230) /0/
   data symb(231) / 'kill  '/,  ivec(231) /  0/,  iascii(231) /0/
   data symb(232) / 'ivolt '/,  ivec(232) /  0/,  iascii(232) /0/
   data symb(233) / 'nchain'/,  ivec(233) /  0/,  iascii(233) /0/
   data symb(234) / 'iprsup'/,  ivec(234) /  0/,  iascii(234) /0/
   data symb(235) / 'unused'/,  ivec(235) /  0/,  iascii(235) /0/
   data symb(236) / 'intinf'/,  ivec(236) /  0/,  iascii(236) /0/
   data symb(237) / 'kconst'/,  ivec(237) /  0/,  iascii(237) /0/
   data symb(238) / 'kswtch'/,  ivec(238) /  0/,  iascii(238) /0/
   data symb(239) / 'it    '/,  ivec(239) /  0/,  iascii(239) /0/
   data symb(240) / 'ntot  '/,  ivec(240) /  0/,  iascii(240) /0/
   data symb(241) / 'ibr   '/,  ivec(241) /  0/,  iascii(241) /0/
   data symb(242) / 'lcom10'/,  ivec(242) /  0/,  iascii(242) /0/
   data symb(243) / 'ltrnst'/,  ivec(243) /  0/,  iascii(243) /0/
   data symb(244) / 'lsyn  '/,  ivec(244) /  0/,  iascii(244) /0/
   data symb(245) / 'kssout'/,  ivec(245) /  0/,  iascii(245) /0/
   data symb(246) / 'loopss'/,  ivec(246) /  1/,  iascii(246) /0/
   data symb(247) / 'infexp'/,  ivec(247) /  0/,  iascii(247) /0/
   data symb(248) / 'numref'/,  ivec(248) /  0/,  iascii(248) /0/
   data symb(249) / 'nword1'/,  ivec(249) /  0/,  iascii(249) /0/
   data symb(250) / 'nword2'/,  ivec(250) /  0/,  iascii(250) /0/
   data symb(251) / 'iloaep'/,  ivec(251) /  0/,  iascii(251) /0/
   data symb(252) / 'lnpin '/,  ivec(252) /  0/,  iascii(252) /0/
   data symb(253) / 'ntot1 '/,  ivec(253) /  0/,  iascii(253) /0/
   data symb(254) / 'limstp'/,  ivec(254) /  0/,  iascii(254) /0/
   data symb(255) / 'indstp'/,  ivec(255) /  0/,  iascii(255) /0/
   data symb(256) / 'nc    '/,  ivec(256) /  0/,  iascii(256) /0/
   data symb(257) / 'unused'/,  ivec(257) /  0/,  iascii(257) /0/
   data symb(258) / 'unused'/,  ivec(258) /  0/,  iascii(258) /0/
   data symb(259) / 'icat  '/,  ivec(259) /  0/,  iascii(259) /0/
   data symb(260) / 'numnvo'/,  ivec(260) /  0/,  iascii(260) /0/
   data symb(261) / 'unused'/,  ivec(261) /  0/,  iascii(261) /0/
   data symb(262) / 'nenerg'/,  ivec(262) /  0/,  iascii(262) /0/
   data symb(263) / 'isw   '/,  ivec(263) /  0/,  iascii(263) /0/
   data symb(264) / 'itest '/,  ivec(264) /  0/,  iascii(264) /0/
   data symb(265) / 'idist '/,  ivec(265) /  0/,  iascii(265) /0/
   data symb(266) / 'x     '/,  ivec(266) /  1/,  iascii(266) /0/
   data symb(267) / 'ykm   '/,  ivec(267) /  1/,  iascii(267) /0/
   data symb(268) / 'km    '/,  ivec(268) /  1/,  iascii(268) /0/
   data symb(269) / 'xk    '/,  ivec(269) /  1/,  iascii(269) /0/
   data symb(270) / 'xm    '/,  ivec(270) /  1/,  iascii(270) /0/
   data symb(271) / 'weight'/,  ivec(271) /  1/,  iascii(271) /0/
   data symb(272) / 'iwtent'/,  ivec(272) /  1/,  iascii(272) /0/
   data symb(273) / 'con1  '/,  ivec(273) /  1/,  iascii(273) /0/
   data symb(274) / 'iskip '/,  ivec(274) /  1/,  iascii(274) /0/
   data symb(275) / 'zinf  '/,  ivec(275) /  1/,  iascii(275) /0/
   data symb(276) / 'eta   '/,  ivec(276) /  1/,  iascii(276) /0/
   data symb(277) / 'nhist '/,  ivec(277) /  1/,  iascii(277) /0/
   data symb(278) / 'stailm'/,  ivec(278) /  1/,  iascii(278) /0/
   data symb(279) / 'stailk'/,  ivec(279) /  1/,  iascii(279) /0/
   data symb(280) / 'xmax  '/,  ivec(280) /  1/,  iascii(280) /0/
   data symb(281) / 'koutvp'/,  ivec(281) /  1/,  iascii(281) /0/
   data symb(282) / 'bnrg  '/,  ivec(282) /  1/,  iascii(282) /0/
   data symb(283) / 'sconst'/,  ivec(283) /  1/,  iascii(283) /0/
   data symb(284) / 'cnvhst'/,  ivec(284) /  1/,  iascii(284) /0/
   data symb(285) / 'sfd   '/,  ivec(285) /  1/,  iascii(285) /0/
   data symb(286) / 'qfd   '/,  ivec(286) /  1/,  iascii(286) /0/
   data symb(287) / 'semaux'/,  ivec(287) /  1/,  iascii(287) /0/
   data symb(288) / 'ibsout'/,  ivec(288) /  1/,  iascii(288) /0/
   data symb(289) / 'bvalue'/,  ivec(289) /  1/,  iascii(289) /0/
   data symb(290) / 'sptacs'/,  ivec(290) /  1/,  iascii(290) /0/
   data symb(291) / 'kswtyp'/,  ivec(291) /  1/,  iascii(291) /0/
   data symb(292) / 'modswt'/,  ivec(292) /  1/,  iascii(292) /0/
   data symb(293) / 'kbegsw'/,  ivec(293) /  1/,  iascii(293) /0/
   data symb(294) / 'lastsw'/,  ivec(294) /  1/,  iascii(294) /0/
   data symb(295) / 'kentnb'/,  ivec(295) /  1/,  iascii(295) /0/
   data symb(296) / 'nbhdsw'/,  ivec(296) /  1/,  iascii(296) /0/
   data symb(297) / 'topen '/,  ivec(297) /  1/,  iascii(297) /0/
   data symb(298) / 'crit  '/,  ivec(298) /  1/,  iascii(298) /0/
   data symb(299) / 'kdepsw'/,  ivec(299) /  1/,  iascii(299) /0/
   data symb(300) / 'tdns  '/,  ivec(300) /  1/,  iascii(300) /0/
   data symb(301) / 'isourc'/,  ivec(301) /  1/,  iascii(301) /0/
   data symb(302) / 'energy'/,  ivec(302) /  1/,  iascii(302) /0/
   data symb(303) / 'iardub'/,  ivec(303) /  1/,  iascii(303) /0/
   data symb(304) / 'ardube'/,  ivec(304) /  1/,  iascii(304) /0/
   data symb(305) / 'nonlad'/,  ivec(305) /  1/,  iascii(305) /0/
   data symb(306) / 'nonle '/,  ivec(306) /  1/,  iascii(306) /0/
   data symb(307) / 'vnonl '/,  ivec(307) /  1/,  iascii(307) /0/
   data symb(308) / 'curr  '/,  ivec(308) /  1/,  iascii(308) /0/
   data symb(309) / 'anonl '/,  ivec(309) /  1/,  iascii(309) /0/
   data symb(310) / 'vecnl1'/,  ivec(310) /  1/,  iascii(310) /0/
   data symb(311) / 'vecnl2'/,  ivec(311) /  1/,  iascii(311) /0/
   data symb(312) / 'brnonl'/,  ivec(312) /  1/,  iascii(312) /1/
   data symb(313) / 'vzero '/,  ivec(313) /  1/,  iascii(313) /0/
   data symb(314) / 'ilast '/,  ivec(314) /  1/,  iascii(314) /0/
   data symb(315) / 'nltype'/,  ivec(315) /  1/,  iascii(315) /0/
   data symb(316) / 'kupl  '/,  ivec(316) /  1/,  iascii(316) /0/
   data symb(317) / 'nlsub '/,  ivec(317) /  1/,  iascii(317) /0/
   data symb(318) / 'cursub'/,  ivec(318) /  1/,  iascii(318) /0/
   data symb(319) / 'cchar '/,  ivec(319) /  1/,  iascii(319) /0/
   data symb(320) / 'vchar '/,  ivec(320) /  1/,  iascii(320) /0/
   data symb(321) / 'gslope'/,  ivec(321) /  1/,  iascii(321) /0/
   data symb(322) / 'kk    '/,  ivec(322) /  1/,  iascii(322) /0/
   data symb(323) / 'c     '/,  ivec(323) /  1/,  iascii(323) /0/
   data symb(324) / 'tr    '/,  ivec(324) /  1/,  iascii(324) /0/
   data symb(325) / 'tx    '/,  ivec(325) /  1/,  iascii(325) /0/
   data symb(326) / 'r     '/,  ivec(326) /  1/,  iascii(326) /0/
   data symb(327) / 'nr    '/,  ivec(327) /  1/,  iascii(327) /0/
   data symb(328) / 'length'/,  ivec(328) /  1/,  iascii(328) /0/
   data symb(329) / 'cik   '/,  ivec(329) /  1/,  iascii(329) /0/
   data symb(330) / 'ci    '/,  ivec(330) /  1/,  iascii(330) /0/
   data symb(331) / 'ck    '/,  ivec(331) /  1/,  iascii(331) /0/
   data symb(332) / 'swname'/,  ivec(332) /  1/,  iascii(332) /1/
   data symb(333) / 'ibrnch'/,  ivec(333) /  1/,  iascii(333) /0/
   data symb(334) / 'jbrnch'/,  ivec(334) /  1/,  iascii(334) /0/
   data symb(335) / 'tstop '/,  ivec(335) /  1/,  iascii(335) /0/
   data symb(336) / 'nonlk '/,  ivec(336) /  1/,  iascii(336) /0/
   data symb(337) / 'nonlm '/,  ivec(337) /  1/,  iascii(337) /0/
   data symb(338) / 'spum  '/,  ivec(338) /  1/,  iascii(338) /0/
   data symb(339) / 'kks   '/,  ivec(339) /  1/,  iascii(339) /0/
   data symb(340) / 'kknonl'/,  ivec(340) /  1/,  iascii(340) /0/
   data symb(341) / 'znonl '/,  ivec(341) /  1/,  iascii(341) /0/
   data symb(342) / 'znonlb'/,  ivec(342) /  1/,  iascii(342) /0/
   data symb(343) / 'znonlc'/,  ivec(343) /  1/,  iascii(343) /0/
   data symb(344) / 'finit '/,  ivec(344) /  1/,  iascii(344) /0/
   data symb(345) / 'ksub  '/,  ivec(345) /  1/,  iascii(345) /0/
   data symb(346) / 'msub  '/,  ivec(346) /  1/,  iascii(346) /0/
   data symb(347) / 'isubeg'/,  ivec(347) /  1/,  iascii(347) /0/
   data symb(348) / 'litype'/,  ivec(348) /  1/,  iascii(348) /0/
   data symb(349) / 'imodel'/,  ivec(349) /  1/,  iascii(349) /0/
   data symb(350) / 'kbus  '/,  ivec(350) /  1/,  iascii(350) /0/
   data symb(351) / 'mbus  '/,  ivec(351) /  1/,  iascii(351) /0/
   data symb(352) / 'kodebr'/,  ivec(352) /  1/,  iascii(352) /0/
   data symb(353) / 'cki   '/,  ivec(353) /  1/,  iascii(353) /0/
   data symb(354) / 'ckkjm '/,  ivec(354) /  1/,  iascii(354) /0/
   data symb(355) / 'indhst'/,  ivec(355) /  1/,  iascii(355) /0/
   data symb(356) / 'kodsem'/,  ivec(356) /  1/,  iascii(356) /0/
   data symb(357) / 'brname'/,  ivec(357) /  1/,  iascii(357) /1/
   data symb(358) / 'iform '/,  ivec(358) /  1/,  iascii(358) /0/
   data symb(359) / 'node  '/,  ivec(359) /  1/,  iascii(359) /0/
   data symb(360) / 'crest '/,  ivec(360) /  1/,  iascii(360) /0/
   data symb(361) / 'time1 '/,  ivec(361) /  1/,  iascii(361) /0/
   data symb(362) / 'time2 '/,  ivec(362) /  1/,  iascii(362) /0/
   data symb(363) / 'tstart'/,  ivec(363) /  1/,  iascii(363) /0/
   data symb(364) / 'sfreq '/,  ivec(364) /  1/,  iascii(364) /0/
   data symb(365) / 'kmswit'/,  ivec(365) /  1/,  iascii(365) /0/
   data symb(366) / 'nextsw'/,  ivec(366) /  1/,  iascii(366) /0/
   data symb(367) / 'rmfd  '/,  ivec(367) /  1/,  iascii(367) /0/
   data symb(368) / 'cikfd '/,  ivec(368) /  1/,  iascii(368) /0/
   data symb(369) / 'imfd  '/,  ivec(369) /  1/,  iascii(369) /0/
   data symb(370) / 'tclose'/,  ivec(370) /  1/,  iascii(370) /0/
   data symb(371) / 'adelay'/,  ivec(371) /  1/,  iascii(371) /0/
   data symb(372) / 'kpos  '/,  ivec(372) /  1/,  iascii(372) /0/
   data symb(373) / 'e     '/,  ivec(373) /  1/,  iascii(373) /0/
   data symb(374) / 'f     '/,  ivec(374) /  1/,  iascii(374) /0/
   data symb(375) / 'kssfrq'/,  ivec(375) /  1/,  iascii(375) /0/
   data symb(376) / 'kode  '/,  ivec(376) /  1/,  iascii(376) /0/
   data symb(377) / 'kpsour'/,  ivec(377) /  1/,  iascii(377) /0/
   data symb(378) / 'volti '/,  ivec(378) /  1/,  iascii(378) /0/
   data symb(379) / 'voltk '/,  ivec(379) /  1/,  iascii(379) /0/
   data symb(380) / 'volt  '/,  ivec(380) /  1/,  iascii(380) /0/
   data symb(381) / 'bus   '/,  ivec(381) /  1/,  iascii(381) /1/
   data symb(382) / 'eld   '/,  ivec(382) /  1/,  iascii(382) /0/
   data symb(383) / 'elaf  '/,  ivec(383) /  1/,  iascii(383) /0/
   data symb(384) / 'elf   '/,  ivec(384) /  1/,  iascii(384) /0/
   data symb(385) / 'elakd '/,  ivec(385) /  1/,  iascii(385) /0/
   data symb(386) / 'elfkd '/,  ivec(386) /  1/,  iascii(386) /0/
   data symb(387) / 'elkd  '/,  ivec(387) /  1/,  iascii(387) /0/
   data symb(388) / 'elq   '/,  ivec(388) /  1/,  iascii(388) /0/
   data symb(389) / 'elag  '/,  ivec(389) /  1/,  iascii(389) /0/
   data symb(390) / 'elg   '/,  ivec(390) /  1/,  iascii(390) /0/
   data symb(391) / 'elakq '/,  ivec(391) /  1/,  iascii(391) /0/
   data symb(392) / 'elgkq '/,  ivec(392) /  1/,  iascii(392) /0/
   data symb(393) / 'elkq  '/,  ivec(393) /  1/,  iascii(393) /0/
   data symb(394) / 'el0   '/,  ivec(394) /  1/,  iascii(394) /0/
   data symb(395) / 'ra    '/,  ivec(395) /  1/,  iascii(395) /0/
   data symb(396) / 'rf    '/,  ivec(396) /  1/,  iascii(396) /0/
   data symb(397) / 'rkd   '/,  ivec(397) /  1/,  iascii(397) /0/
   data symb(398) / 'rg    '/,  ivec(398) /  1/,  iascii(398) /0/
   data symb(399) / 'rkq   '/,  ivec(399) /  1/,  iascii(399) /0/
   data symb(400) / 'r0    '/,  ivec(400) /  1/,  iascii(400) /0/
   data symb(401) / 'agline'/,  ivec(401) /  1/,  iascii(401) /0/
   data symb(402) / 'rat1  '/,  ivec(402) /  1/,  iascii(402) /0/
   data symb(403) / 'smoutp'/,  ivec(403) /  1/,  iascii(403) /0/
   data symb(404) / 'smoutq'/,  ivec(404) /  1/,  iascii(404) /0/
   data symb(405) / 'teg   '/,  ivec(405) /  1/,  iascii(405) /0/
   data symb(406) / 'texc  '/,  ivec(406) /  1/,  iascii(406) /0/
   data symb(407) / 'cnp   '/,  ivec(407) /  1/,  iascii(407) /0/
   data symb(408) / 'a22   '/,  ivec(408) /  1/,  iascii(408) /0/
   data symb(409) / 'a12   '/,  ivec(409) /  1/,  iascii(409) /0/
   data symb(410) / 'a21   '/,  ivec(410) /  1/,  iascii(410) /0/
   data symb(411) / 'ac    '/,  ivec(411) /  1/,  iascii(411) /0/
   data symb(412) / 'ai    '/,  ivec(412) /  1/,  iascii(412) /0/
   data symb(413) / 'at    '/,  ivec(413) /  1/,  iascii(413) /0/
   data symb(414) / 'ah    '/,  ivec(414) /  1/,  iascii(414) /0/
   data symb(415) / 'xay   '/,  ivec(415) /  1/,  iascii(415) /0/
   data symb(416) / 'cu    '/,  ivec(416) /  1/,  iascii(416) /0/
   data symb(417) / 'cv    '/,  ivec(417) /  1/,  iascii(417) /0/
   data symb(418) / 'dsat  '/,  ivec(418) /  1/,  iascii(418) /0/
   data symb(419) / 'qsat  '/,  ivec(419) /  1/,  iascii(419) /0/
   data symb(420) / 'acr   '/,  ivec(420) /  1/,  iascii(420) /0/
   data symb(421) / 'ce    '/,  ivec(421) /  1/,  iascii(421) /0/
   data symb(422) / 'dsr   '/,  ivec(422) /  1/,  iascii(422) /0/
   data symb(423) / 'dsd   '/,  ivec(423) /  1/,  iascii(423) /0/
   data symb(424) / 'hico  '/,  ivec(424) /  1/,  iascii(424) /0/
   data symb(425) / 'dsm   '/,  ivec(425) /  1/,  iascii(425) /0/
   data symb(426) / 'hsp   '/,  ivec(426) /  1/,  iascii(426) /0/
   data symb(427) / 'power '/,  ivec(427) /  1/,  iascii(427) /0/
   data symb(428) / 'extrs '/,  ivec(428) /  1/,  iascii(428) /0/
   data symb(429) / 'histq '/,  ivec(429) /  1/,  iascii(429) /0/
   data symb(430) / 'histr '/,  ivec(430) /  1/,  iascii(430) /0/
   data symb(431) / 'yfor  '/,  ivec(431) /  1/,  iascii(431) /0/
   data symb(432) / 'zsk   '/,  ivec(432) /  1/,  iascii(432) /0/
   data symb(433) / 'y     '/,  ivec(433) /  1/,  iascii(433) /0/
   data symb(434) / 'tork  '/,  ivec(434) /  1/,  iascii(434) /0/
   data symb(435) / 'temp  '/,  ivec(435) /  1/,  iascii(435) /0/
   data symb(436) / 'z     '/,  ivec(436) /  1/,  iascii(436) /0/
   data symb(437) / 'x1    '/,  ivec(437) /  1/,  iascii(437) /0/
   data symb(438) / 'sqrt3 '/,  ivec(438) /  0/,  iascii(438) /0/
   data symb(439) / 'asqrt3'/,  ivec(439) /  0/,  iascii(439) /0/
   data symb(440) / 'sqrt32'/,  ivec(440) /  0/,  iascii(440) /0/
   data symb(441) / 'thtw  '/,  ivec(441) /  0/,  iascii(441) /0/
   data symb(442) / 'athtw '/,  ivec(442) /  0/,  iascii(442) /0/
   data symb(443) / 'radeg '/,  ivec(443) /  0/,  iascii(443) /0/
   data symb(444) / 'omdt  '/,  ivec(444) /  0/,  iascii(444) /0/
   data symb(445) / 'factom'/,  ivec(445) /  0/,  iascii(445) /0/
   data symb(446) / 'damrat'/,  ivec(446) /  0/,  iascii(446) /0/
   data symb(447) / 'isat  '/,  ivec(447) /  1/,  iascii(447) /0/
   data symb(448) / 'ised  '/,  ivec(448) /  1/,  iascii(448) /0/
   data symb(449) / 'iseq  '/,  ivec(449) /  1/,  iascii(449) /0/
   data symb(450) / 'imdual'/,  ivec(450) /  1/,  iascii(450) /0/
   data symb(451) / 'iconfg'/,  ivec(451) /  1/,  iascii(451) /0/
   data symb(452) / 'kmac  '/,  ivec(452) /  1/,  iascii(452) /0/
   data symb(453) / 'kexc  '/,  ivec(453) /  1/,  iascii(453) /0/
   data symb(454) / 'numas '/,  ivec(454) /  1/,  iascii(454) /0/
   data symb(455) / 'nodma '/,  ivec(455) /  1/,  iascii(455) /0/
   data symb(456) / 'nodmb '/,  ivec(456) /  1/,  iascii(456) /0/
   data symb(457) / 'nodmc '/,  ivec(457) /  1/,  iascii(457) /0/
   data symb(458) / 'jasmit'/,  ivec(458) /  1/,  iascii(458) /0/
   data symb(459) / 'jsmtor'/,  ivec(459) /  1/,  iascii(459) /0/
   data symb(460) / 'jexcit'/,  ivec(460) /  1/,  iascii(460) /0/
   data symb(461) / 'isloc '/,  ivec(461) /  1/,  iascii(461) /0/
   data symb(462) / 'noutsm'/,  ivec(462) /  1/,  iascii(462) /0/
   data symb(463) / 'ismout'/,  ivec(463) /  1/,  iascii(463) /0/
   data symb(464) / 'mfirst'/,  ivec(464) /  0/,  iascii(464) /0/
   data symb(465) / 'limass'/,  ivec(465) /  0/,  iascii(465) /0/
   data symb(466) / 'nst   '/,  ivec(466) /  0/,  iascii(466) /0/
   data symb(467) / 'itold '/,  ivec(467) /  0/,  iascii(467) /0/
   data symb(468) / 'ibrold'/,  ivec(468) /  0/,  iascii(468) /0/
   data symb(469) / 'busum '/,  ivec(469) /  1/,  iascii(469) /1/
   data symb(470) / 'ptheta'/,  ivec(470) /  1/,  iascii(470) /0/
   data symb(471) / 'zthevr'/,  ivec(471) /  1/,  iascii(471) /0/
   data symb(472) / 'vinp  '/,  ivec(472) /  1/,  iascii(472) /0/
   data symb(473) / 'zthevs'/,  ivec(473) /  1/,  iascii(473) /0/
   data symb(474) / 'umcur '/,  ivec(474) /  1/,  iascii(474) /0/
   data symb(475) / 'con   '/,  ivec(475) /  1/,  iascii(475) /0/
   data symb(476) / 'dumvec'/,  ivec(476) /  1/,  iascii(476) /0/
   data symb(477) / 'dummat'/,  ivec(477) /  1/,  iascii(477) /0/
   data symb(478) / 'date  '/,  ivec(478) /  1/,  iascii(478) /0/
   data symb(479) / 'clock '/,  ivec(479) /  1/,  iascii(479) /0/
   data symb(480) / 'pi    '/,  ivec(480) /  0/,  iascii(480) /0/
   data symb(481) / 'sroot2'/,  ivec(481) /  0/,  iascii(481) /0/
   data symb(482) / 'sroot3'/,  ivec(482) /  0/,  iascii(482) /0/
   data symb(483) / 'omegrf'/,  ivec(483) /  0/,  iascii(483) /0/
   data symb(484) / 'inpu  '/,  ivec(484) /  0/,  iascii(484) /0/
   data symb(485) / 'numbus'/,  ivec(485) /  0/,  iascii(485) /0/
   data symb(486) / 'ncltot'/,  ivec(486) /  0/,  iascii(486) /0/
   data symb(487) / 'ndum  '/,  ivec(487) /  1/,  iascii(487) /0/
   data symb(488) / 'initum'/,  ivec(488) /  0/,  iascii(488) /0/
   data symb(489) / 'iureac'/,  ivec(489) /  0/,  iascii(489) /0/
   data symb(490) / 'iugpar'/,  ivec(490) /  0/,  iascii(490) /0/
   data symb(491) / 'iufpar'/,  ivec(491) /  0/,  iascii(491) /0/
   data symb(492) / 'iuhist'/,  ivec(492) /  0/,  iascii(492) /0/
   data symb(493) / 'iuumrp'/,  ivec(493) /  0/,  iascii(493) /0/
   data symb(494) / 'iunod1'/,  ivec(494) /  0/,  iascii(494) /0/
   data symb(495) / 'iunod2'/,  ivec(495) /  0/,  iascii(495) /0/
   data symb(496) / 'iujclt'/,  ivec(496) /  0/,  iascii(496) /0/
   data symb(497) / 'iujclo'/,  ivec(497) /  0/,  iascii(497) /0/
   data symb(498) / 'iujtyp'/,  ivec(498) /  0/,  iascii(498) /0/
   data symb(499) / 'iunodo'/,  ivec(499) /  0/,  iascii(499) /0/
   data symb(500) / 'iujtmt'/,  ivec(500) /  0/,  iascii(500) /0/
   data symb(501) / 'iuhism'/,  ivec(501) /  0/,  iascii(501) /0/
   data symb(502) / 'iuomgm'/,  ivec(502) /  0/,  iascii(502) /0/
   data symb(503) / 'iuomld'/,  ivec(503) /  0/,  iascii(503) /0/
   data symb(504) / 'iutham'/,  ivec(504) /  0/,  iascii(504) /0/
   data symb(505) / 'iuredu'/,  ivec(505) /  0/,  iascii(505) /0/
   data symb(506) / 'iureds'/,  ivec(506) /  0/,  iascii(506) /0/
   data symb(507) / 'iuflds'/,  ivec(507) /  0/,  iascii(507) /0/
   data symb(508) / 'iufldr'/,  ivec(508) /  0/,  iascii(508) /0/
   data symb(509) / 'iurequ'/,  ivec(509) /  0/,  iascii(509) /0/
   data symb(510) / 'iuflqs'/,  ivec(510) /  0/,  iascii(510) /0/
   data symb(511) / 'iuflqr'/,  ivec(511) /  0/,  iascii(511) /0/
   data symb(512) / 'iujcds'/,  ivec(512) /  0/,  iascii(512) /0/
   data symb(513) / 'iujcqs'/,  ivec(513) /  0/,  iascii(513) /0/
   data symb(514) / 'iuflxd'/,  ivec(514) /  0/,  iascii(514) /0/
   data symb(515) / 'iuflxq'/,  ivec(515) /  0/,  iascii(515) /0/
   data symb(516) / 'iunppa'/,  ivec(516) /  0/,  iascii(516) /0/
   data symb(517) / 'iurotm'/,  ivec(517) /  0/,  iascii(517) /0/
   data symb(518) / 'iuncld'/,  ivec(518) /  0/,  iascii(518) /0/
   data symb(519) / 'iunclq'/,  ivec(519) /  0/,  iascii(519) /0/
   data symb(520) / 'iujtqo'/,  ivec(520) /  0/,  iascii(520) /0/
   data symb(521) / 'iujomo'/,  ivec(521) /  0/,  iascii(521) /0/
   data symb(522) / 'iujtho'/,  ivec(522) /  0/,  iascii(522) /0/
   data symb(523) / 'iureqs'/,  ivec(523) /  0/,  iascii(523) /0/
   data symb(524) / 'iuepso'/,  ivec(524) /  0/,  iascii(524) /0/
   data symb(525) / 'iudcoe'/,  ivec(525) /  0/,  iascii(525) /0/
   data symb(526) / 'iukcoi'/,  ivec(526) /  0/,  iascii(526) /0/
   data symb(527) / 'iuvolt'/,  ivec(527) /  0/,  iascii(527) /0/
   data symb(528) / 'iuangl'/,  ivec(528) /  0/,  iascii(528) /0/
   data symb(529) / 'iunodf'/,  ivec(529) /  0/,  iascii(529) /0/
   data symb(530) / 'iunodm'/,  ivec(530) /  0/,  iascii(530) /0/
   data symb(531) / 'iukumo'/,  ivec(531) /  0/,  iascii(531) /0/
   data symb(532) / 'iujumo'/,  ivec(532) /  0/,  iascii(532) /0/
   data symb(533) / 'iuumou'/,  ivec(533) /  0/,  iascii(533) /0/
   data symb(534) / 'nclfix'/,  ivec(534) /  0/,  iascii(534) /0/
   data symb(535) / 'numfix'/,  ivec(535) /  0/,  iascii(535) /0/
   data symb(536) / 'iotfix'/,  ivec(536) /  0/,  iascii(536) /0/
   data symb(537) / 'ibsfix'/,  ivec(537) /  0/,  iascii(537) /0/
   data symb(538) / 'ksubum'/,  ivec(538) /  0/,  iascii(538) /0/
   data symb(539) / 'nsmach'/,  ivec(539) /  0/,  iascii(539) /0/
   data symb(540) / 'istart'/,  ivec(540) /  0/,  iascii(540) /0/
   data symb(541) / 'karray'/,  ivec(541) /  1/,  iascii(541) /0/
   data symb(542) / 'rampcn'/,  ivec(542) /  1/,  iascii(542) /0/
   data symb(543) / 'rampsl'/,  ivec(543) /  1/,  iascii(543) /0/
   data symb(544) / 'kyramp'/,  ivec(544) /  1/,  iascii(544) /0/
   data symb(545) / 'texpar'/,  ivec(545) /  1/,  iascii(545) /0/
   data symb(546) / 'fendrp'/,  ivec(546) /  1/,  iascii(546) /0/
   data symb(547) / 'tminrp'/,  ivec(547) /  0/,  iascii(547) /0/
   data symb(548) / 'tmaxrp'/,  ivec(548) /  0/,  iascii(548) /0/
   data symb(549) / 'tbegrp'/,  ivec(549) /  1/,  iascii(549) /0/
   data symb(550) / 'tendrp'/,  ivec(550) /  1/,  iascii(550) /0/
   data symb(551) / 'fbegrp'/,  ivec(551) /  1/,  iascii(551) /0/
   data symb(552) / 'tbreak'/,  ivec(552) /  0/,  iascii(552) /0/
   data symb(553) / 'indxrp'/,  ivec(553) /  1/,  iascii(553) /0/
   data symb(554) / 'ivec  '/,  ivec(554) /  1/,  iascii(554) /0/
   data symb(555) / 'iascii'/,  ivec(555) /  1/,  iascii(555) /0/
   data symb(556) / 'numsym'/,  ivec(556) /  0/,  iascii(556) /0/
   data symb(557) / 'jjroll'/,  ivec(557) /  0/,  iascii(557) /0/
   data symb(558) / 'itexp '/,  ivec(558) /  0/,  iascii(558) /0/
   data symb(559) / 'labels'/,  ivec(559) /  1/,  iascii(559) /0/
   data symb(560) / 'maxarg'/,  ivec(560) /  0/,  iascii(560) /0/
   data symb(561) / 'kilper'/,  ivec(561) /  0/,  iascii(561) /0/
   data symb(562) / 'kfile5'/,  ivec(562) /  0/,  iascii(562) /0/
   data symb(563) / 'kverfy'/,  ivec(563) /  0/,  iascii(563) /0/
   data symb(564) / 'ibegcl'/,  ivec(564) /  0/,  iascii(564) /0/
   data symb(565) / 'iendcl'/,  ivec(565) /  0/,  iascii(565) /0/
   data symb(566) / 'lidnt1'/,  ivec(566) /  0/,  iascii(566) /0/
   data symb(567) / 'lidnt2'/,  ivec(567) /  0/,  iascii(567) /0/
   data symb(568) / 'linnow'/,  ivec(568) /  0/,  iascii(568) /0/
   data symb(569) / 'linspn'/,  ivec(569) /  0/,  iascii(569) /0/
   data symb(570) / 'numcrd'/,  ivec(570) /  0/,  iascii(570) /0/
   data symb(571) / 'munit5'/,  ivec(571) /  0/,  iascii(571) /0/
   data symb(572) / 'indbuf'/,  ivec(572) /  0/,  iascii(572) /0/
   data symb(573) / 'indbeg'/,  ivec(573) /  0/,  iascii(573) /0/
   data symb(574) / 'mflush'/,  ivec(574) /  0/,  iascii(574) /0/
   data symb(575) / 'newvec'/,  ivec(575) /  0/,  iascii(575) /0/
   data symb(576) / 'munit6'/,  ivec(576) /  0/,  iascii(576) /0/
   data symb(577) / 'lserlc'/,  ivec(577) /  0/,  iascii(577) /0/
   data symb(578) / 'kserlc'/,  ivec(578) /  0/,  iascii(578) /0/
   data symb(579) / 'kbrser'/,  ivec(579) /  0/,  iascii(579) /0/
   data symb(580) / 'lockbr'/,  ivec(580) /  0/,  iascii(580) /0/
   data symb(581) / 'iprspy'/,  ivec(581) /  0/,  iascii(581) /0/
   data symb(582) / 'monitr'/,  ivec(582) /  0/,  iascii(582) /0/
   data symb(583) / 'monits'/,  ivec(583) /  0/,  iascii(583) /0/
   data symb(584) / 'locate'/,  ivec(584) /  1/,  iascii(584) /0/
   data symb(585) / 'nline '/,  ivec(585) /  1/,  iascii(585) /0/
   data symb(586) / 'kwtspy'/,  ivec(586) /  0/,  iascii(586) /0/
   data symb(587) / 'kbreak'/,  ivec(587) /  0/,  iascii(587) /0/
   data symb(588) / 'limbuf'/,  ivec(588) /  0/,  iascii(588) /0/
   data symb(589) / 'inchlp'/,  ivec(589) /  0/,  iascii(589) /0/
   data symb(590) / 'ksymbl'/,  ivec(590) /  0/,  iascii(590) /0/
   data symb(591) / 'kopyit'/,  ivec(591) /  0/,  iascii(591) /0/
   data symb(592) / 'kslowr'/,  ivec(592) /  0/,  iascii(592) /0/
   data symb(593) / 'limcrd'/,  ivec(593) /  0/,  iascii(593) /0/
   data symb(594) / 'looprp'/,  ivec(594) /  1/,  iascii(594) /0/
   data symb(595) / 'n10rmp'/,  ivec(595) /  1/,  iascii(595) /0/
   data symb(596) / 'memrmp'/,  ivec(596) /  1/,  iascii(596) /0/
   data symb(597) / 'kar1  '/,  ivec(597) /  1/,  iascii(597) /0/
   data symb(598) / 'kar2  '/,  ivec(598) /  1/,  iascii(598) /0/
   data symb(599) / 'numrmp'/,  ivec(599) /  0/,  iascii(599) /0/
   data symb(600) / 'luntsp'/,  ivec(600) /  0/,  iascii(600) /0/
   data symb(601) / 'logvar'/,  ivec(601) /  0/,  iascii(601) /0/
   data symb(602) / 'filext'/,  ivec(602) /  1/,  iascii(602) /0/
   data symb(603) / 'symb  '/,  ivec(603) /  1/,  iascii(603) /0/
   data symb(604) / 'col   '/,  ivec(604) /  1/,  iascii(604) /0/
   data symb(605) / 'bytfnd'/,  ivec(605) /  0/,  iascii(605) /0/
   data symb(606) / 'char1 '/,  ivec(606) /  0/,  iascii(606) /0/
   data symb(607) / 'symbrp'/,  ivec(607) /  1/,  iascii(607) /0/
   data symb(608) / 'chard4'/,  ivec(608) /  0/,  iascii(608) /0/
   data symb(609) / 'bytbuf'/,  ivec(609) /  0/,  iascii(609) /0/
   data symb(610) / 'buff77'/,  ivec(610) /  0/,  iascii(610) /0/
   data symb(611) / 'file6b'/,  ivec(611) /  1/,  iascii(611) /0/
   data symb(612) / 'file6 '/,  ivec(612) /  1/,  iascii(612) /0/
   data symb(613) / 'blan80'/,  ivec(613) /  0/,  iascii(613) /0/
   data symb(614) / 'prom80'/,  ivec(614) /  0/,  iascii(614) /0/
   data symb(615) / 'digit '/,  ivec(615) /  1/,  iascii(615) /0/
   data symb(616) / 'iac   '/,  ivec(616) /  0/,  iascii(616) /0/
   data symb(617) / 'idctcs'/,  ivec(617) /  0/,  iascii(617) /0/
   data symb(618) / 'ipl   '/,  ivec(618) /  0/,  iascii(618) /0/
   data symb(619) / 'ipr   '/,  ivec(619) /  0/,  iascii(619) /0/
   data symb(620) / 'ixr   '/,  ivec(620) /  0/,  iascii(620) /0/
   data symb(621) / 'jpl   '/,  ivec(621) /  0/,  iascii(621) /0/
   data symb(622) / 'jpr   '/,  ivec(622) /  0/,  iascii(622) /0/
   data symb(623) / 'kint  '/,  ivec(623) /  0/,  iascii(623) /0/
   data symb(624) / 'kout  '/,  ivec(624) /  0/,  iascii(624) /0/
   data symb(625) / 'nds   '/,  ivec(625) /  0/,  iascii(625) /0/
   data symb(626) / 'nkn   '/,  ivec(626) /  0/,  iascii(626) /0/
   data symb(627) / 'nmax  '/,  ivec(627) /  0/,  iascii(627) /0/
   data symb(628) / 'nuk   '/,  ivec(628) /  0/,  iascii(628) /0/
   data symb(629) / 'kwrite'/,  ivec(629) /  0/,  iascii(629) /0/
   data symb(630) / 'kpr   '/,  ivec(630) /  0/,  iascii(630) /0/
   data symb(631) / 'kpl   '/,  ivec(631) /  0/,  iascii(631) /0/
   data symb(632) / 'mxtacw'/,  ivec(632) /  0/,  iascii(632) /0/
   data symb(633) / 'iptacw'/,  ivec(633) /  0/,  iascii(633) /0/
   data symb(634) / 'nhst  '/,  ivec(634) /  0/,  iascii(634) /0/
   data symb(635) / 'kvin  '/,  ivec(635) /  0/,  iascii(635) /0/
   data symb(636) / 'kvou  '/,  ivec(636) /  0/,  iascii(636) /0/
   data symb(637) / 'kvxx  '/,  ivec(637) /  0/,  iascii(637) /0/
   data symb(638) / 'icsup '/,  ivec(638) /  0/,  iascii(638) /0/
   data symb(639) / 'nxic  '/,  ivec(639) /  0/,  iascii(639) /0/
   data symb(640) / 'kksj  '/,  ivec(640) /  0/,  iascii(640) /0/
   data symb(641) / 'kksk  '/,  ivec(641) /  0/,  iascii(641) /0/
   data symb(642) / 'kkfst '/,  ivec(642) /  0/,  iascii(642) /0/
   data symb(643) / 'kkni  '/,  ivec(643) /  0/,  iascii(643) /0/
   data symb(644) / 'kkhst '/,  ivec(644) /  0/,  iascii(644) /0/
   data symb(645) / 'kifls '/,  ivec(645) /  0/,  iascii(645) /0/
   data symb(646) / 'kidum '/,  ivec(646) /  0/,  iascii(646) /0/
   data symb(647) / 'kslim1'/,  ivec(647) /  0/,  iascii(647) /0/
   data symb(648) / 'kslim2'/,  ivec(648) /  0/,  iascii(648) /0/
   data symb(649) / 'kslim3'/,  ivec(649) /  0/,  iascii(649) /0/
   data symb(650) / 'kpac1r'/,  ivec(650) /  0/,  iascii(650) /0/
   data symb(651) / 'kpac1i'/,  ivec(651) /  0/,  iascii(651) /0/
   data symb(652) / 'kpac2r'/,  ivec(652) /  0/,  iascii(652) /0/
   data symb(653) / 'kpac2i'/,  ivec(653) /  0/,  iascii(653) /0/
   data symb(654) / 'kalksx'/,  ivec(654) /  0/,  iascii(654) /0/
   data symb(655) / 'kilms1'/,  ivec(655) /  0/,  iascii(655) /0/
   data symb(656) / 'kilms2'/,  ivec(656) /  0/,  iascii(656) /0/
   data symb(657) / 'kdumj '/,  ivec(657) /  0/,  iascii(657) /0/
   data symb(658) / 'kdumk '/,  ivec(658) /  0/,  iascii(658) /0/
   data symb(659) / 'kkzj  '/,  ivec(659) /  0/,  iascii(659) /0/
   data symb(660) / 'kkzk  '/,  ivec(660) /  0/,  iascii(660) /0/
   data symb(661) / 'kiflz '/,  ivec(661) /  0/,  iascii(661) /0/
   data symb(662) / 'kgnz  '/,  ivec(662) /  0/,  iascii(662) /0/
   data symb(663) / 'kzlim1'/,  ivec(663) /  0/,  iascii(663) /0/
   data symb(664) / 'kzlim2'/,  ivec(664) /  0/,  iascii(664) /0/
   data symb(665) / 'kalkzx'/,  ivec(665) /  0/,  iascii(665) /0/
   data symb(666) / 'kilmz1'/,  ivec(666) /  0/,  iascii(666) /0/
   data symb(667) / 'kilmz2'/,  ivec(667) /  0/,  iascii(667) /0/
   data symb(668) / 'kksus '/,  ivec(668) /  0/,  iascii(668) /0/
   data symb(669) / 'kalksu'/,  ivec(669) /  0/,  iascii(669) /0/
   data symb(670) / 'kiuty '/,  ivec(670) /  0/,  iascii(670) /0/
   data symb(671) / 'kud1  '/,  ivec(671) /  0/,  iascii(671) /0/
   data symb(672) / 'kud2  '/,  ivec(672) /  0/,  iascii(672) /0/
   data symb(673) / 'kud3  '/,  ivec(673) /  0/,  iascii(673) /0/
   data symb(674) / 'kud4  '/,  ivec(674) /  0/,  iascii(674) /0/
   data symb(675) / 'kud5  '/,  ivec(675) /  0/,  iascii(675) /0/
   data symb(676) / 'kaliu '/,  ivec(676) /  0/,  iascii(676) /0/
   data symb(677) / 'ktysup'/,  ivec(677) /  0/,  iascii(677) /0/
   data symb(678) / 'kjsup '/,  ivec(678) /  0/,  iascii(678) /0/
   data symb(679) / 'kksup '/,  ivec(679) /  0/,  iascii(679) /0/
   data symb(680) / 'kspvar'/,  ivec(680) /  0/,  iascii(680) /0/
   data symb(681) / 'kopsup'/,  ivec(681) /  0/,  iascii(681) /0/
   data symb(682) / 'kfnsup'/,  ivec(682) /  0/,  iascii(682) /0/
   data symb(683) / 'krgsup'/,  ivec(683) /  0/,  iascii(683) /0/
   data symb(684) / 'kprsup'/,  ivec(684) /  0/,  iascii(684) /0/
   data symb(685) / 'ktypdv'/,  ivec(685) /  0/,  iascii(685) /0/
   data symb(686) / 'kkdj  '/,  ivec(686) /  0/,  iascii(686) /0/
   data symb(687) / 'kkdk  '/,  ivec(687) /  0/,  iascii(687) /0/
   data symb(688) / 'kgndev'/,  ivec(688) /  0/,  iascii(688) /0/
   data symb(689) / 'kdev1 '/,  ivec(689) /  0/,  iascii(689) /0/
   data symb(690) / 'kdev2 '/,  ivec(690) /  0/,  iascii(690) /0/
   data symb(691) / 'kldev1'/,  ivec(691) /  0/,  iascii(691) /0/
   data symb(692) / 'kldev2'/,  ivec(692) /  0/,  iascii(692) /0/
   data symb(693) / 'kkdus '/,  ivec(693) /  0/,  iascii(693) /0/
   data symb(694) / 'kalkdu'/,  ivec(694) /  0/,  iascii(694) /0/
   data symb(695) / 'ktbdev'/,  ivec(695) /  0/,  iascii(695) /0/
   data symb(696) / 'kpn   '/,  ivec(696) /  0/,  iascii(696) /0/
   data symb(697) / 'kpd   '/,  ivec(697) /  0/,  iascii(697) /0/
   data symb(698) / 'kxhst '/,  ivec(698) /  0/,  iascii(698) /0/
   data symb(699) / 'khscr '/,  ivec(699) /  0/,  iascii(699) /0/
   data symb(700) / 'khsci '/,  ivec(700) /  0/,  iascii(700) /0/
   data symb(701) / 'kilim1'/,  ivec(701) /  0/,  iascii(701) /0/
   data symb(702) / 'kilim2'/,  ivec(702) /  0/,  iascii(702) /0/
   data symb(703) / 'krowcs'/,  ivec(703) /  0/,  iascii(703) /0/
   data symb(704) / 'krhsde'/,  ivec(704) /  0/,  iascii(704) /0/
   data symb(705) / 'kvlim1'/,  ivec(705) /  0/,  iascii(705) /0/
   data symb(706) / 'kvlim2'/,  ivec(706) /  0/,  iascii(706) /0/
   data symb(707) / 'kkxic '/,  ivec(707) /  0/,  iascii(707) /0/
   data symb(708) / 'kawkcs'/,  ivec(708) /  0/,  iascii(708) /0/
   data symb(709) / 'kxar  '/,  ivec(709) /  0/,  iascii(709) /0/
   data symb(710) / 'kxai  '/,  ivec(710) /  0/,  iascii(710) /0/
   data symb(711) / 'kbwkcs'/,  ivec(711) /  0/,  iascii(711) /0/
   data symb(712) / 'kxtcs '/,  ivec(712) /  0/,  iascii(712) /0/
   data symb(713) / 'klntab'/,  ivec(713) /  0/,  iascii(713) /0/
   data symb(714) / 'klmxic'/,  ivec(714) /  0/,  iascii(714) /0/
   data symb(715) / 'kcolcs'/,  ivec(715) /  0/,  iascii(715) /0/
   data symb(716) / 'katcs '/,  ivec(716) /  0/,  iascii(716) /0/
   data symb(717) / 'kbtcs '/,  ivec(717) /  0/,  iascii(717) /0/
   data symb(718) / 'kjout '/,  ivec(718) /  0/,  iascii(718) /0/
   data symb(719) / 'kkout '/,  ivec(719) /  0/,  iascii(719) /0/
   data symb(720) / 'kxmncs'/,  ivec(720) /  0/,  iascii(720) /0/
   data symb(721) / 'ktxmn '/,  ivec(721) /  0/,  iascii(721) /0/
   data symb(722) / 'kxmxcs'/,  ivec(722) /  0/,  iascii(722) /0/
   data symb(723) / 'ktxmx '/,  ivec(723) /  0/,  iascii(723) /0/
   data symb(724) / 'klnout'/,  ivec(724) /  0/,  iascii(724) /0/
   data symb(725) / 'ekbuf '/,  ivec(725) /  1/,  iascii(725) /0/
   data symb(726) / 'ektemp'/,  ivec(726) /  1/,  iascii(726) /0/
   data symb(727) / 'errchk'/,  ivec(727) /  0/,  iascii(727) /0/
   data symb(728) / 'solrsv'/,  ivec(728) /  1/,  iascii(728) /0/
   data symb(729) / 'solisv'/,  ivec(729) /  1/,  iascii(729) /0/
   data symb(730) / 'nitera'/,  ivec(730) /  0/,  iascii(730) /0/
   data symb(731) / 'nekreq'/,  ivec(731) /  0/,  iascii(731) /0/
   data symb(732) / 'nekcod'/,  ivec(732) /  1/,  iascii(732) /0/
   data  numsym   / 732 /
end block data

block data blkhlp
   include 'dekspy.ftn'
   data texspy (   1 ) / 'key word no.  1:  "heading"     ----  ----  ----                                '  /
   data texspy (   2 ) / '  response will be the printing of the previously-defined heading of  "examine",'  /
   data texspy (   3 ) / '  followed by current values of all variables (as for the 1st  "examine"  use). '  /
   data texspy (   4 ) / 'key word no.  2:  "stop"        ----  ----  ----                                '  /
   data texspy (   5 ) / '  this command will terminate interactive EMP execution immediately,  by  means '  /
   data texspy (   6 ) / '  of a fortran "stop" statement.  There will be no automatic saving  of  tables,'  /
   data texspy (   7 )   /  '  or  of plot data points before such a termination  (use prior  "sleep"  and/or'  /
   data texspy (   8 )   /  '  "lunit4"  commands, if such preservation is desired).                         '  /
   data texspy (   9 )   /  'key word no.  3:  "plot"        ----  ----  ----                                '  /
   data texspy (  10 )   /  '  issue this command to transfer control to the  "outer:"  prompt of interactive'  /
   data texspy (  11 )   /  '  emtp plotting (the former separate EMTP crt plotting program  "tpplot").   due'  /
   data texspy (  12 )   /  '  to the absorbtion into spy,  several changes have been made.  first,  no plot-'  /
   data texspy (  13 )   /  '  file specification is required of the user  (he should subsequently send  "go"'  /
   data texspy (  14 )   /  '  if no other outer-level response is desired).   at the  "middle:"  level,  the'  /
   data texspy (  15 )   /  '  "timespan"  computation is now automatically performed,  internally.   at  any'  /
   data texspy (  16 )   /  '  level,  "stop"  no longer terminates program execution, but instead it returns'  /
   data texspy (  17 )   /  '  control to the  "spy:"  prompt.   continuous,  automatic plotting  of  the on-'  /
   data texspy (  18 )   /  '  going solution  (like a strip-chart) is  based  on the use of either  "rollc"'  /
   data texspy (  19 )   /  '  (for character plotting)  or  "rollv" (for vector plotting)  as  commands  at'  /
   data texspy (  20 )   /  '  the  "inner:"  level.   for  a  detailed explanation of all  "plot"  commands,'  /
   data texspy (  21 )   /  '  send  "help"  at any of the three levels within  "plot".                      '  /
   data texspy (  22 )   /  'key word no.  4:  "help"        ----  ----  ----                                '  /
   data texspy (  23 )   /  '  education for the ignorant, such as the user is now being subjected to (joke).'  /
   data texspy (  24 )   /  '  a  carriage return  <cr>  will  produce text for the next key word  in  order,'  /
   data texspy (  25 )   /  '  while  a  return  to the  "spy:"  prompt is accomplished by  "spy", or  "end".'  /
   data texspy (  26 )   /  '  "all"  will loop over  all  explanations  (subject  to  user-keyed interrupt).'  /
   data texspy (  27 )   /  '  sending  "top"  will rewind to the first message, while  "bot"  will give the '  /
   data texspy (  28 )   /  '  last.   use  "back"  to back up one message.                                  '  /
   data texspy (  29 )   /  'key word no.  5:  "examine"     ----  ----  ----                                '  /
   data texspy (  30 )   /  '  issue this command  to  examine the contents of any EMTP common variables  (of'  /
   data texspy (  31 )   /  '  solution overlays).   integer  scalars require 6 columns;  all other variables'  /
   data texspy (  32 )   /  '  require 15.   subsequent prompts willallow  the  user  to specify scalars and'  /
   data texspy (  33 )   /  '  vector ranges (e.g.,  "kbus(3:8)"  for  cells  3  through  8 of kbus).   "end"'  /
   data texspy (  34 )   /  '  terminates the list,  resulting  in adisplay of heading and numerical values.'  /
   data texspy (  35 )   /  '  any later striking of the  "return" key  will  then display current numerical'  /
   data texspy (  36 )   /  '  values only.   send  "heading"  for a  refresh of the variable names.  for  a'  /
   data texspy (  37 )   /  '  rolling display,  send  "roll"  (also,  see  separate  instructions  for  this'  /
   data texspy (  38 )   /  '  command).  for  rolling, the  output vector  is  re-formed at each spy chance,'  /
   data texspy (  39 )   /  '  but is only output when one or more variables has changed.  to  terminate  the'  /
   data texspy (  40 )   /  '  roll-loop,  use the regular user-keyed interrupt.                             '  /
   data texspy (  41 )   /  'key word no.  6:  "deposit"     ----  ----  ----                                '  /
   data texspy (  42 )   /  '  issue this command to modify the contents of any  EMTP  common  variables  (of'  /
   data texspy (  43 )   /  '  solution overlays).   subsequent prompts  will  permit  the  user  to  specify'  /
   data texspy (  44 )   /  '  scalars  and vector ranges  (e.g.,  "kbus(3:8)"  for  cells  3  through  8  of'  /
   data texspy (  45 )   /  '  kbus).   "end"  terminates the list, returning to the  "spy:"  prompt.  after'  /
   data texspy (  46 )   /  '  each  variable,  there will be a prompt for the desired new value (free-format'  /
   data texspy (  47 )   /  '  number),  if no  "="  is used.   the separately-prompted input  is  rigorously'  /
   data texspy (  48 )   /  '  free-format  (so  that any  i, f, or e-field number is permissible).  but  for'  /
   data texspy (  49 )   /  '  simple numeric values which  can  be read  using  f15.0,  or  for  text (a6),'  /
   data texspy (  50 )   /  '  follow the scalar or vector by an equal sign and then the number.  the case of'  /
   data texspy (  51 )   /  '  alphanumeric  must  not  have imbedded blanks after the  "=",  but numbers can'  /
   data texspy (  52 )   /  '  (within the span of 15 columns).'  /
   data texspy (  53 )   /  'key word no.  7:  "switch"      ----  ----  ----                                '  /
   data texspy (  54 )   /  '  this response to the  "spy:"  prompt is  issued  for  a  display  of the EMTP'  /
   data texspy (  55 )   /  '  switch  table.   subsequently  send an  additional  "extra"  to change to the'  /
   data texspy (  56 )   /  '  next sub-table (as of february 1984, there are two),  if different columns are'  /
   data texspy (  57 )   /  '  desired.  within any one choice as to sub-table,  there is a loop in which the'  /
   data texspy (  58 )   /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy (  59 )   /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy (  60 )   /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage'  /
   data texspy (  61 )   /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy (  62 )   /  '  and return to the  "spy:"  prompt.  to refresh the heading, use either  "swit"'  /
   data texspy (  63 )   /  '  or  "head"  (only 4 characters of "switch" or "heading" are checked).         '  /
   data texspy (  64 )   /  'key word no.  8:  "append"      ----  ----  ----                                '  /
   data texspy (  65 )   /  '  this command is the gateway to installation-dependent commands which are non- '  /
   data texspy (  66 )   /  '  standard,  and which have been installed locally.   the  utpf carries a dummy '  /
   data texspy (  67 )   /  '  subroutine append,  by  definition. one clean  way  of adding a functional '  /
   data texspy (  68 )   /  '  module is at translation time,  using a submod  request.                     '  /
   data texspy (  69 )   /  'key word no.  9:  "save"        ----  ----  ----                                '  /
   data texspy (  70 )   /  '  when the EMTP is in the time-step loop, this command will force an exit to    '  /
   data texspy (  71 )   /  '  overlay 20 upon completion of present time-step.   there,  "katalg"  saves    '  /
   data texspy (  72 )   /  '  EMTP tables as rapidly as possible (may not be permanent).   the simulation   '  /
   data texspy (  73 )   /  '  then recommences by an automatic transfer back to the beginning of the        '  /
   data texspy (  74 )   /  '  time-step loop ("over16").   since plot data points are not part of the       '  /
   data texspy (  75 )   /  '  saving, if the user wants these to be preserved, too, he must be careful.     '  /
   data texspy (  76 )   /  '  provided EMTP storage for plot data points does not fill up before the        '  /
   data texspy (  77 )   /  '  subsequent  restore  operation (to back up time to the  save  point), there   '  /
   data texspy (  78 )   /  '  is no problem.  otherwise, consider use of  "space"  and  "lunit4"  commands. '  /
   data texspy (  79 )   /  'key word no. 10:  "restore"     ----  ----  ----                                '  /
   data texspy (  80 )   /  '  the  "restore"  command is the reverse of  "save".  when within the deltat    '  /
   data texspy (  81 )   /  '  loop, use of  "restore"  will rapidly transfer to overlay 20.  there, module  '  /
   data texspy (  82 )   /  '  "katalg" restores former tables, after which control is transferred to spy.   '  /
   data texspy (  83 )   /  '  possibly the  "space"  and  "lunit4" would also be appropriate, to restore   '  /
   data texspy (  84 )   /  '  any separately plot files.   also after tables have been restored, the user   '  /
   data texspy (  85 )   /  '  can modify any EMTP variables  desired,  and exit  "deposit"  with  "end";    '  /
   data texspy (  86 )   /  '  finally, send  "go"  in response to the  "spy:"  prompt, to transfer back to  '  /
   data texspy (  87 )   /  '  the top of the time-step loop ("over16").                                     '  /
   data texspy (  88 )   /  'key word no. 11:  "go"          ----  ----  ----                                '  /
   data texspy (  89 )   /  '  this command is issued in response to the  "spy:"  prompt to terminate        '  /
   data texspy (  90 )   /  '  several sequences, such as one which might have begun with  "restore".  in    '  /
   data texspy (  91 )   /  '  this case, there would be the preceding transfers of control from overlay 16  '  /
   data texspy (  92 )   /  '  to overlay 20 ("katalg"), and then back to overlay 16 again.  as a second     '  /
   data texspy (  93 )   /  '  example,  "go"  cancels the  "rest" command.  third, it can be used to       '  /
   data texspy (  94 )   /  '  begin the EMTP solution following the  "data"  command (for data card input   '  /
   data texspy (  95 )   /  '  at the beginning of program execution).   finally,  "go"  cancels the         '  /
   data texspy (  96 )   /  '  suspension   of execution which accompanies the  "sleep"  command.            '  /
   data texspy (  97 )   /  'key word no. 12:  "echo"        ----  ----  ----                                '  /
   data texspy (  98 )   /  '  users who are interested in keeping a history of spy commands which are to    '  /
   data texspy (  99 )   /  '  be issued can use the  "echo"  command.   a subsequent prompt then allows     '  /
   data texspy ( 100 )   /  '  several choices.  to begin such accumulation, send  "begin";  to end it,      '  /
   data texspy ( 101 )   /  '  send  "file",  which will result in the dumping of all accumulation onto      '  /
   data texspy ( 102 )   /  '  disk as a permanent file of the users choice (a subsequent prompt will        '  /
   data texspy ( 103 )   /  '  ask for the desired file name).   the history consists of 80-column card      '  /
   data texspy ( 104 )   /  '  images,  stored from the bottom of  file6  upward  (with cell  kspsav         '  /
   data texspy ( 105 )   /  '  storing the last, and  limcrd  storing the first).  to view the accumulation  '  /
   data texspy ( 106 )   /  '  to date, use  "show".   when  "file" is used,  not only is a copy sent to    '  /
   data texspy ( 107 )   /  '  disk, but those in memory are erased. hence, if  echoing  is to continue,   '  /
   data texspy ( 108 )   /  '  the user must send  "begin"  again, immediately after  "file"  is complete.  '  /
   data texspy ( 109 )   /  '  in order to erase an erroneous, immediately-preceding command, use  "cancel"  '  /
   data texspy ( 110 )   /  '  at any point.  this is intercepted by the input routine  "flager",  so is     '  /
   data texspy ( 111 )   /  '  not a spy command per se.   there will be confirmation of the erasure.        '  /
   data texspy ( 112 )   /  'key word no. 13:  "find"        ----  ----  ----                                '  /
   data texspy ( 113 )   /  '  the sending of  "find"  will result in the printing of a heading, followed    '  /
   data texspy ( 114 )   /  '  by a pause, as  spy  waits for the user to supply a 6-character EMTP symbol   '  /
   data texspy ( 115 )   /  '  name.   this is a closed loop: after receiving a name,  spy  will display     '  /
   data texspy ( 116 )   /  '  the memory address, and then wait foranother such symbol.   exit by  "end".  '  /
   data texspy ( 117 )   /  '  wild cards ("*") of  vax/vms  are honored here, although the candidate        '  /
   data texspy ( 118 )   /  '  string is limited to eight characters maximum (only the first 8 are read).    '  /
   data texspy ( 119 )   /  'key word no. 14:  "list"        ----  ----  ----                                '  /
   data texspy ( 120 )   /  '  this command will result in the printing of a heading, followed by a pause.   '  /
   data texspy ( 121 )   /  '  at this point the user is in a loop, supplying row numbers (beginning and     '  /
   data texspy ( 122 )   /  '  ending rows as a pair of free-format integers).   spy responds to each with   '  /
   data texspy ( 123 )   /  '  a display of those rows of the spy symbol table, and then waits for the next  '  /
   data texspy ( 124 )   /  '  request.   the sending of  "0,0"  will break out, returning to the  "spy:"    '  /
   data texspy ( 125 )   /  '  prompt.   the user can interrupt any excessively long display with his keyed  '  /
   data texspy ( 126 )   /  '  interrupt (details depend upon computer).  instead of  "0,0"  to break out,   '  /
   data texspy ( 127 )   /  '  "end"  or  "spy:"  can alternatively be used.  sending nothing (just a        '  /
   data texspy ( 128 )   /  '  carriage return <cr>) is interpreted by spy as a request for "more of the     '  /
   data texspy ( 129 )   /  '  same" ---- the same number of rows as just displayed, beginning where the     '  /
   data texspy ( 130 )   /  '  last display left off.   special trickery is required if an argument of  "@"  '  /
   data texspy ( 131 )   /  '  usage is to respond to the  "list"  prompt,  since a comma must not be used   '  /
   data texspy ( 132 )   /  '  as the free-format separator (due to confusion with use of the same symbol    '  /
   data texspy ( 133 )   /  '  for argument separation by "@").  instead, a pounds sign  "#"  must be used   '  /
   data texspy ( 134 )   /  '  rather than a blank (due to extraction of blanks by  "@"  logic).  finally,   '  /
   data texspy ( 135 )   /  '  sending  "all"  instead of two row numbers displays the whole table.          '  /
   data texspy ( 136 )   /  'key word no. 15:  "spy"         ----  ----  ----                                '  /
   data texspy ( 137 )   /  '  this text, supplied almost anywhere that the program is looking for text,     '  /
   data texspy ( 138 )   /  '  will cause an internal interruption of whatever was happening, and a return   '  /
   data texspy ( 139 )   /  '  to the  "spy:"  prompt.   one exception is the  "help"  prompt which is now   '  /
   data texspy ( 140 )   /  '  being serviced.    '  /
   data texspy ( 141 )   /  'key word no. 16:  "break"       ----  ----  ----                                '  /
   data texspy ( 142 )   /  '  this response to the  "spy:"  prompt is appropriate if the user wants the     '  /
   data texspy ( 143 )   /  '  simulation to continue uninterrupted until some pre-specified time, when a    '  /
   data texspy ( 144 )   /  '  clean break at the start of "subts1" will be made.  a subsequent prompt will  '  /
   data texspy ( 145 )   /  '  allow the user to specify the future break time  t-break  in seconds.  if a   '  /
   data texspy ( 146 )   /  '  minus sign is appended, then the input is taken to be a step number of the    '  /
   data texspy ( 147 )   /  '  time-step loop, and the program will calculate  t-break   by multiplying by   '  /
   data texspy ( 148 )   /  '  deltat.  oh, in case "subts1" means nothing to the user, this is the first of '  /
   data texspy ( 149 )   /  '  four pieces of overlay 16 (the time-step loop).  in case the user sends zero  '  /
   data texspy ( 150 )   /  '  for  t-break, an added prompt will seek clarification as to which utpf overlay'  /
   data texspy ( 151 )   /  '  is the desired stopping point (immediately prior to entry).  common usage     '  /
   data texspy ( 152 )   /  '  involves overlays numbered 6  (after all sources have been read),  12  (when  '  /
   data texspy ( 153 )   /  '  the   phasor solution is complete), or  16  (the time-step loop).             '  /
   data texspy ( 154 )   /  'key word no. 17:  "when"        ----  ----  ----                                '  /
   data texspy ( 155 )   /  '  this response to the  "spy:"  prompt will redefine the EMTP table-handling    '  /
   data texspy ( 156 )   /  '  time of  "save"  and  "restore" .   that is,  if so defined,  this overrides  '  /
   data texspy ( 157 )   /  '  the otherwise immediate exit of the time-step loop for table-handling.        '  /
   data texspy ( 158 )   /  'key word no. 18:  "comment"     ----  ----  ----                                '  /
   data texspy ( 159 )   /  '  this response to the  "spy:"  prompt will toggle the switch which controls the'  /
   data texspy ( 160 )   /  '  printing of comment cards ("c ") which may be contained within command files. '  /
   data texspy ( 161 )   /  '  the default (starting value) is to show comment cards during execution ("@"). '  /
   data texspy ( 162 )   /  'key word no. 19:  "@?"          ----  ----  ----                                '  /
   data texspy ( 163 )   /  '  this response to the  "spy:"  prompt will result in the internal  opening  of '  /
   data texspy ( 164 )   /  '  an arbitary disk file, and the connection of this file as replacement for     '  /
   data texspy ( 165 )   /  '  keyboard input to spy.  if the file name consists of just a single digit      '  /
   data texspy ( 166 )   /  '  (?=1-9),  inclspy?.dat  is the disk file name to be used.  reading from the   '  /
   data texspy ( 167 )   /  '  disk file continues until an end-of-file is hit, at which point the spy input '  /
   data texspy ( 168 )   /  '  channel is connected once again to the keyboard.  such usage can not be nested'  /
   data texspy ( 169 )   /  '  (i.e., no such disk file can itself contain an  "@"  statement).   EMTP       '  /
   data texspy ( 170 )   /  '  comment cards ("c ") are permitted within such disk files, however,  with the '  /
   data texspy ( 171 )   /  '  "comment"  switch controlling whether or not they are written to the screen   '  /
   data texspy ( 172 )   /  '  (the default is for such writing).other lines of an executed disk file are '  /
   data texspy ( 173 )   /  '  generally not seen by the user as they are read during execution, and neither '  /
   data texspy ( 174 )   /  '  are the repetitive  "spy:"  prompts. parameters are possible, to substitute  '  /
   data texspy ( 175 )   /  '  for 8-column   "%%%%%%%%"   fields of the disk file.   blanks are ignored,    '  /
   data texspy ( 176 )   /  '  and arguments are to be separated by commas.  an opening parenthesis  "("  and'  /
   data texspy ( 177 )   /  '  a closing parenthesis  ")"  are optional delimiters.   each command line is   '  /
   data texspy ( 178 )   /  '  limited to 80 columns (no continuation), and a maximum of 10 arguments.   the '  /
   data texspy ( 179 )   /  '  left-to-right list is applied to the file %-fields from top to bottom, in     '  /
   data texspy ( 180 )   /  '  order.  the %-fields can be built into any line of the disk file which        '  /
   data texspy ( 181 )   /  '  is read by  spy  proper (not plotting).   finally, there is the use of a pound'  /
   data texspy ( 182 )   /  '  sign "#" for reserved blanks, which are otherwise ignored.   the classic case '  /
   data texspy ( 183 )   /  '  where it is needed is for a response to  "list"  (e.g.,  "2#4");   this is    '  /
   data texspy ( 184 )   /  '  free-format information, with a comma impossible due to the conflict with "@".'  /
   data texspy ( 185 )   /  '  although not a part of the interactive  spy  code per se, it should also be   '  /
   data texspy ( 186 )   /  '  remembered that commands which could be built into  "@"  files can also be    '  /
   data texspy ( 187 )   /  '  executed as part of the regular EMTP data.  the key to such usage is  "$spy", '  /
   data texspy ( 188 )   /  '  which is processed by  "cimage"  and/or  "erexit".   if all spy commands are  '  /
   data texspy ( 189 )   /  '  to be built in-line as part of the lunit5 EMTP input data, then precede such  '  /
   data texspy ( 190 )   /  '  data by a card reading  "$spy",  with columns 5 onward blank.  in this        '  /
   data texspy ( 191 )   /  '  case,  "erexit"  reads and removes such data cards, creating special reserved '  /
   data texspy ( 192 )   /  '  disk files named  spyfile?.dat,  where  "?"  is a single digit between one    '  /
   data texspy ( 193 )   /  '  and nine (allowing a maximum of nine such distinct groups of spy commands).   '  /
   data texspy ( 194 )   /  '  the last spy command of each such group is to be followed by  "$endspy"  as a '  /
   data texspy ( 195 )   /  '  special marker record.  on the other hand, if the user does not care about    '  /
   data texspy ( 196 )   /  '  unifying all such EMTP data in a single disk file, then a single line of EMTP '  /
   data texspy ( 197 )   /  '  data,  "$spy, filename",  is all that is required to provide the connection to'  /
   data texspy ( 198 )   /  '  spy.  in this second card,  filename can be any legal, full file name of the '  /
   data texspy ( 199 )   /  '  computer system being considered (if the user supplies the file name, and     '  /
   data texspy ( 200 )   /  '  puts commands in the disk file, then there is no limit on the number of       '  /
   data texspy ( 201 )   /  '  such usages.   the role of  "cimage" is to treat  "$spy"  like   "$include".  '  /
   data texspy ( 202 )   /  'key word no. 20:  "roll"        ----  ----  ----                                '  /
   data texspy ( 203 )   /  '  this response to the  "spy:"  prompt will result in a "rolling" of previously-'  /
   data texspy ( 204 )   /  '  defined  "examine"  request.   while this happens, no other spy activity is   '  /
   data texspy ( 205 )   /  '  permitted (any <cr> will abort the loop, and return to the  "spy:"  prompt).  '  /
   data texspy ( 206 )   /  '  following  "roll",  the simulation will recommence, with spy called at each   '  /
   data texspy ( 207 )   /  '  opportunity for a spy break (within frequency  maxflg  of  "dekspy",  which   '  /
   data texspy ( 208 )   /  '  nominally has the value one, implying four checks per time step).   the very  '  /
   data texspy ( 209 )   /  '  first check, the heading and current value of the  "examine"  vector will be  '  /
   data texspy ( 210 )   /  '  displayed;  thereafter, output will be produced if and only if a change in    '  /
   data texspy ( 211 )   /  '  the output (compared with the previous evaluation) has occurred.              '  /
   data texspy ( 212 )   /  '  a final thought is about other commands which function much like the  "roll"  '  /
   data texspy ( 213 )   /  '  commands, although in fact these are not issued at the  "spy:" level.  for    '  /
   data texspy ( 214 )   /  '  "plot"  use,  at the  "inner:"  level,  "rollc"  will produce continuous      '  /
   data texspy ( 215 )   /  '  character plotting, while  "rollv"  does the same for vector plotting.   it   '  /
   data texspy ( 216 )   /  '  is the  "noroll"  command of spy which will cancel either or both of these.   '  /
   data texspy ( 217 )   /  'key word no. 21:  "type?"       ----  ----  ----                                '  /
   data texspy ( 218 )   /  '  this response to the  "spy:"  prompt will result in the listing of command    '  /
   data texspy ( 219 )   /  '  file  inclspy?.dat  of  "@?"  usage. for any specific numerical digit       '  /
   data texspy ( 220 )   /  '  "?",  just that one file will be listed.   but if symbolic "?" is retained, or'  /
   data texspy ( 221 )   /  '  if column 5 onward is blank, then all possible  "@?"  files will be listed    '  /
   data texspy ( 222 )   /  '  in natural order, preceded by an identifying heading.  also, for arbitrary    '  /
   data texspy ( 223 )   /  '  disk files of card images with names of 2 or more characters, this command    '  /
   data texspy ( 224 )   /  '  will display them.  "type filename" is the form of this more general         '  /
   data texspy ( 225 )   /  '  command, where  filename  is any legal file name of 32 or fewer characters.   '  /
   data texspy ( 226 )   /  '  if no such file exists, there will be a warning message, so this command      '  /
   data texspy ( 227 )   /  '  can be used to check on the existence of arbitary disk files of card images.  '  /
   data texspy ( 228 )   /  'key word no. 22:  "verify"      ----  ----  ----                                '  /
   data texspy ( 229 )   /  '  this response to the  "spy:"  prompt will toggle the switch that controls the '  /
   data texspy ( 230 )   /  '  echoing of data cards within a disk file which is read via  "@"  usage.  the  '  /
   data texspy ( 231 )   /  '  default (beginning) setting is to have such echoing.   a related command is   '  /
   data texspy ( 232 )   /  '  "comment",  which can separately control the display of comment ("c ") cards  '  /
   data texspy ( 233 )   /  '  as they are encountered during processing of the  "@"  file.  if there is no  '  /
   data texspy ( 234 )   /  '  echoing of spy data, then comment cards might likewise not be displayed;  or, '  /
   data texspy ( 235 )   /  '  they can be used as an absolute reference (if they are displayed), to mark the'  /
   data texspy ( 236 )   /  '  beginning or ending of invisible operations due to no-echoing of "verify".    '  /
   data texspy ( 237 )   /  'key word no. 23:  "files"       ----  ----  ----                                '  /
   data texspy ( 238 )   /  '  this  response  to  the  "spy:"  prompt will result in a display of all of the'  /
   data texspy ( 239 )   /  '  inclspy?.dat  files which exist,  based on the fortran "inquire" operation  at'  /
   data texspy ( 240 )   /  '  the time program execution began.  an "x"  means that the file exists (in the'  /
   data texspy ( 241 )   /  '  display),  whereas a blank means that it does not.  there are 9 columns.      '  /
   data texspy ( 242 )   /  'key word no. 24:  "sleep"       ----  ----  ----                                '  /
   data texspy ( 243 )   /  '  this response to the  "spy:"  prompt will put the EMTP to sleep in such a way '  /
   data texspy ( 244 )   /  '  that the simulation can be continued at any later time.  the interactive      '  /
   data texspy ( 245 )   /  '  "sleep"  command is comparable to the batch-mode use of miscellaneous data    '  /
   data texspy ( 246 )   /  '  parameter   memsav = 1   for the saving of EMTP tables on disk.  to service a '  /
   data texspy ( 247 )   /  '  "sleep"  command, spy exits the time-step loop and jumps to  "over20"  for    '  /
   data texspy ( 248 )   /  '  table dumping to disk.  subsequent awakening is via  "wake",  which reads     '  /
   data texspy ( 249 )   /  '  EMTP tables back from disk into EMTP memory.  any plot data must be separately'  /
   data texspy ( 250 )   /  '  and manually provided for by the user (using  "space", "lunit4"),  if it, too,'  /
   data texspy ( 251 )   /  '  is to be permanently saved.      '  /
   data texspy ( 252 )   /  'key word no. 25:  "source"      ----  ----  ----                                '  /
   data texspy ( 253 )   /  '  this response to the  "spy:"  prompt will allow the user to look at either    '  /
   data texspy ( 254 )   /  '  the electric network or the tacs source table.   there will be a pause after  '  /
   data texspy ( 255 )   /  '  spy receives  "source",  as it waits to receive either  "tacs"  or  "elec"  as'  /
   data texspy ( 256 )   /  '  an indication of source-table choice. then  spy  waits for a pair of free-  '  /
   data texspy ( 257 )   /  '  format integer row numbers, to define the limits of the table display.   a few'  /
   data texspy ( 258 )   /  '  key words are also accepted here:  "all"  to display entire table,  "end"  or '  /
   data texspy ( 259 )   /  '  "stop"  or  "spy"  to return to  "spy:"  prompt,  and  "tacs"  or  "elec"  to '  /
   data texspy ( 260 )   /  '  produce a new table heading (or switch between the two tables).   the tacs    '  /
   data texspy ( 261 )   /  '  table displays offsets of  "sptacs" which are used with  "deposit/examine",  '  /
   data texspy ( 262 )   /  '  in case the user wants to redefine such quantities.                           '  /
   data texspy ( 263 )   /  'key word no. 26:  "edit"        ----  ----  ----                                '  /
   data texspy ( 264 )   /  '  this response to the  "spy:"  prompt will allow the user to examine and modify'  /
   data texspy ( 265 )   /  '  card images which are currently stored in memory (see the  "data"  command).  '  /
   data texspy ( 266 )   /  '  a  "*"  prompt will next appear, at which point vax sos-like editing commands '  /
   data texspy ( 267 )   /  '  can be issued.  the only bothersome changes of notation are the use of  "#"   '  /
   data texspy ( 268 )   /  '  in place of vax"s  "!"  (because of in-line comment problems with e/ts),  and '  /
   data texspy ( 269 )   /  '  use of  "@"  in place of vax"s  "esc" key as a character string delimiter.   '  /
   data texspy ( 270 )   /  '  for an explanation of sos editing rules, see dec vax-11 user documentation.   '  /
   data texspy ( 271 )   /  '  examples of printing include:  "*p^:*",  "*p5",  "*p^:18",  "*p5#10",  "*p",  '  /
   data texspy ( 272 )   /  '  and  "p.-20:."   there also is  "*f" usage,  only with  "@"  replacing  <esc> '  /
   data texspy ( 273 )   /  '  of  vax/vms  sos.  to exit the  "edit"  command and return to the  "spy:"     '  /
   data texspy ( 274 )   /  '  prompt, use  "*e"  (analogous to the sos exit).  additional user commands     '  /
   data texspy ( 275 )   /  '  include  "*d"  (for deletion of lines),  "*r"  (for replacement of lines),    '  /
   data texspy ( 276 )   /  '  "*i"  (for insertion of new lines), and  "*s"  (for the substitution of one  '  /
   data texspy ( 277 )   /  '  character string by another).  concerning  "*s",  however, once again  "@"    '  /
   data texspy ( 278 )   /  '  is used as a delimiter rather than  <esc>,  and no qualifiers (e.g., ",d" for '  /
   data texspy ( 279 )   /  '  "decide mode") are allowed.   finally, there are special EMTP-designed        '  /
   data texspy ( 280 )   /  '  commands.   the first of these is  "*8",  which initiates a search for the    '  /
   data texspy ( 281 )   /  '  next card which has a non-blank column 80.   after display of this record,    '  /
   data texspy ( 282 )   /  '  spy awaits a user decision regarding disposition:  <cr>  will leave the card  '  /
   data texspy ( 283 )   /  '  unchanged and initiate a search for the following one;  digits 0, 1, 2, 3, 4  '  /
   data texspy ( 284 )   /  '  will result in the punching of this value into column 80 before searching for '  /
   data texspy ( 285 )   /  '  the next such record (with "0" internally changed to a blank before punching).'  /
   data texspy ( 286 )   /  '  the  "*"  prompt will reappear automatically when the search hits the bottom  '  /
   data texspy ( 287 )   /  '  of the file.  or,  "*"  can be reached at any point of the search-display loop'  /
   data texspy ( 288 )   /  '  by sending  "e" .   even if not in the  "*8"  loop, column-80 deposits are    '  /
   data texspy ( 289 )   /  '  possible by use of  "*8,?"  where  "?"  is the desired col.-80 content of the '  /
   data texspy ( 290 )   /  '  current line.   finally,  the command "*col"  will produce a heading of      '  /
   data texspy ( 291 )   /  '  column numbers, which is useful when inserting new data records using  "*i" . '  /
   data texspy ( 292 )   /  '  this ruler heading will be shifted to line up with  "*p"  displays.   if an   '  /
   data texspy ( 293 )   /  '  unshifted display is desired,  use  "*col8" .                                 '  /
   data texspy ( 294 )   /  'key word no. 27:  "wake"        ----  ----  ----                                '  /
   data texspy ( 295 )   /  '  this response to the  "spy:"  prompt will awaken a hibernating solution (one  '  /
   data texspy ( 296 )   /  '  which was put to bed with an earlier "sleep"  command).   altered program    '  /
   data texspy ( 297 )   /  '  dimensions are not allowed  (both programs must be dimensioned identically),  '  /
   data texspy ( 298 )   /  '  just as with the batch-mode  "start again".  the  "wake"  command is the exact'  /
   data texspy ( 299 )   /  '  interactive equivalent of the batch-mode request  "start again".  there is a  '  /
   data texspy ( 300 )   /  '  related  "wake4"  command for regeneration of the lunit4 plot-file header     '  /
   data texspy ( 301 )   /  '  information, in case this is wanted by the user (it is in fact needed, if the '  /
   data texspy ( 302 )   /  '  "plot"  command is to be used, as of 26 feb 1984).                            '  /
   data texspy ( 303 )   /  'key word no. 28:  "language"    ----  ----  ----                                '  /
   data texspy ( 304 )   /  '  this response to the  "spy:"  prompt allows the user to either examine or     '  /
   data texspy ( 305 )   /  '  modify "spy:"-level command words (e.g., "plot", "data", etc.).  a loop will  '  /
   data texspy ( 306 )   /  '  be entered, in which there are only 4 legal responses  ("single",  "entire",  '  /
   data texspy ( 307 )   /  '  "show",  and  "spy")  to the prompt. sending  "single"  will lead to an      '  /
   data texspy ( 308 )   /  '  inner input loop in which old and new symbol pairs are to be redefined one at '  /
   data texspy ( 309 )   /  '  a time, terminated by  "end"  (to return to outer  "language"  loop) or  "spy"'  /
   data texspy ( 310 )   /  '  (to return to the  "spy:"  prompt).  the second outer response,  "entire",   '  /
   data texspy ( 311 )   /  '  must be followed by an unabridged dictionary of symbols using 10a8 format.    '  /
   data texspy ( 312 )   /  '  sending the third response  "show"  will result in an unabridged display of   '  /
   data texspy ( 313 )   /  '  all such current commands.  finally, sending  "spy"  will exit the outer loop,'  /
   data texspy ( 314 )   /  '  and return to the  "spy:"  prompt.  the language of  "plot"  usage does not   '  /
   data texspy ( 315 )   /  '  occur at the command level with prompt  "spy:",  so it can only be redefined  '  /
   data texspy ( 316 )   /  '  within that utility (by means of the "set data"  command).                   '  /
   data texspy ( 317 )   /  'key word no. 29:  "catalog"     ----  ----  ----                                '  /
   data texspy ( 318 )   /  '  this response to the  "spy:"  prompt will create a new permanent disk file, it'  /
   data texspy ( 319 )   /  '  will dump a copy of the data case which is presently contained within the     '  /
   data texspy ( 320 )   /  '  EMTP into that disk file.   the name of this new disk file is user-supplied   '  /
   data texspy ( 321 )   /  '  (in response to a subsequent prompt for such a name, which must, naturally, be'  /
   data texspy ( 322 )   /  '  legal for the computer system being used).   the EMTP data case in question   '  /
   data texspy ( 323 )   /  '  generally will differ from that originally read in using the  "data"  command,'  /
   data texspy ( 324 )   /  '  of course  (assuming  "edit"  operations have produced alterations).          '  /
   data texspy ( 325 )   /  'key word no. 30:  "begin"       ----  ----  ----                                '  /
   data texspy ( 326 )   /  '  this response to the  "spy:"  prompt will abort the current EMTP solution,    '  /
   data texspy ( 327 )   /  '  and initiate a complete new solution. it is the simulator-stored EMTP data  '  /
   data texspy ( 328 )   /  '  cards which are used as input for the new solution, and these will usually    '  /
   data texspy ( 329 )   /  '  have just been modified using  "edit" operations.  the transition is         '  /
   data texspy ( 330 )   /  '  "instantaneous" for cases which have been put to bed via  "rest"  or  "sleep",'  /
   data texspy ( 331 )   /  '  or which are cycling the time-step loop.   for execution in earlier overlays, '  /
   data texspy ( 332 )   /  '  there may be a delay until the present overlay is exited.                     '  /
   data texspy ( 333 )   /  'key word no. 31:  "step"        ----  ----  ----                                '  /
   data texspy ( 334 )   /  '  this response to the  "spy:"  prompt represents a request to toggle the binary'  /
   data texspy ( 335 )   /  '  switch that  forces  a  spy  interrupt at each and every possible opportunity.'  /
   data texspy ( 336 )   /  '  either  there  are  such  ever-present forced interrupts, or  there  are none.'  /
   data texspy ( 337 )   /  '  interrupt opportunities exist at the start of each overlay,  at  the start  of'  /
   data texspy ( 338 )   /  '  each of the four pieces of overlay 16 (the time-step loop),  and  finally, as'  /
   data texspy ( 339 )   /  '  each new data card is read by input module  "cimage".                         '  /
   data texspy ( 340 )   /  'key word no. 32:  "debug"       ----  ----  ----                                '  /
   data texspy ( 341 )   /  '  this response to the  "spy:"  prompt is used only for debugging of interactive'  /
   data texspy ( 342 )   /  '  EMTP execution itself, in case of faulty operation.   it is not used for cases'  /
   data texspy ( 343 )   /  '  of suspected EMTP error (do not confuse with  "iprsup"  of the EMTP).  anyway,'  /
   data texspy ( 344 )   /  '  there will be a subsequent prompt for diagnostic control variable  iprspy.    '  /
   data texspy ( 345 )   /  'key word no. 33:  "data"       ----  ----  ----                                 '  /
   data texspy ( 346 )   /  '  this response to the  "spy:"  prompt is appropriate when the user wants to    '  /
   data texspy ( 347 )   /  '  specify a disk file of EMTP data which is to be solved next.  the program     '  /
   data texspy ( 348 )   /  '  will then prompt for the desired file name, which will generally be computer- '  /
   data texspy ( 349 )   /  '  dependent.  a full, legal file name for the computer being used must then be  '  /
   data texspy ( 350 )   /  '  provided.  before giving the real file name, however, the user could send     '  /
   data texspy ( 351 )   /  '  "control",  should he want to input only a portion of the disk file, or should'  /
   data texspy ( 352 )   /  '  it be desired to have the data placed in the EMTP cache with an offset, or    '  /
   data texspy ( 353 )   /  '  should it be desired to have the  lunit5  usage pointer set to other than the '  /
   data texspy ( 354 )   /  '  default value of unity (offset zero). a second non-file response is  "spy",  '  /
   data texspy ( 355 )   /  '  to abort the file-name prompt after the  "control"  definitions (in case the  '  /
   data texspy ( 356 )   /  '  user only wants to redefine the  lunit5  usage pointer, for example).   a     '  /
   data texspy ( 357 )   /  '  final point concerns the avoidance of non-existent files (which would result  '  /
   data texspy ( 358 )   /  '  in an error stop of the EMTP by the operating system).  if in doubt about a   '  /
   data texspy ( 359 )   /  '  file"s existence, try to  type  it first (the  "type"  command warns of non-  '  /
   data texspy ( 360 )   /  '  existent files, without any termination of execution).                        '  /
   data texspy ( 361 )   /  'key word no. 34:  "ramp"        ----  ----  ----                                '  /
   data texspy ( 362 )   /  '  this response to the  "spy:"  prompt will result in prompts which allow the   '  /
   data texspy ( 363 )   /  '  user to modify any variables in EMTP common blocks as linear functions of     '  /
   data texspy ( 364 )   /  '  time.   that is, the the user can ramp the values between beginning and ending'  /
   data texspy ( 365 )   /  '  limits,  as EMTP simulation time moves between beginning and ending times --- '  /
   data texspy ( 366 )   /  '  with all control parameters being user-defined.  any variable which can be    '  /
   data texspy ( 367 )   /  '  seen via the  "examine"  command can also be  ramped.  in specifying parameter'  /
   data texspy ( 368 )   /  '  values, there are three nested loops. the outer loop is for time, the middle'  /
   data texspy ( 369 )   /  '  one is for variable values, and the inner one is for variable names.   the    '  /
   data texspy ( 370 )   /  '  user can stay inside (middle or inner loops) as long as the outer quantity is '  /
   data texspy ( 371 )   /  '  not altered.   "end" will exit any one level,  and head outward to the next   '  /
   data texspy ( 372 )   /  '  one.   at the outer-most level, the optional  "show"  command will display all'  /
   data texspy ( 373 )   /  '  ramps which the user has defined thus far,  while  "end"  or  "spy"  will     '  /
   data texspy ( 374 )   /  '  return to the  "spy:"  prompt.  actual variable redefinition is handled within'  /
   data texspy ( 375 )   /  '  module  "analyt"  of overlay 20 of the EMTP, where ramping logic has been     '  /
   data texspy ( 376 )   /  '  built (this is automatic, as the simulation progresses).                      '  /
   data texspy ( 377 )   /  'key word no. 35:  "time"        ----  ----  ----                                '  /
   data texspy ( 378 )   /  '  this response to the  "spy:"  prompt will result in the display of  both  the'  /
   data texspy ( 379 )   /  '  wall-clock time  (the date, too)  and also  the  current EMTP simulation time'  /
   data texspy ( 380 )   /  '  parameters (t, tmax, deltat).      '  /
   data texspy ( 381 )   /  'key word no. 36:  "tek"         ----  ----  ----                                '  /
   data texspy ( 382 )   /  '  this response to the  "spy:"  prompt allows modification to an ongoing vector-'  /
   data texspy ( 383 )   /  '  graphic display which is currently  rolling.  yes, the user could simply      '  /
   data texspy ( 384 )   /  '  regenerate the plot from the beginning (by use of a  "plot"  command following'  /
   data texspy ( 385 )   /  '  a user-keyed interrupt), but minor changes can often be made more easily, and '  /
   data texspy ( 386 )   /  '  with less disruption, using  "tek". a subsequent prompt will then list a    '  /
   data texspy ( 387 )   /  '  menu of alternatives which are available for such "on-the-fly"  rolling       '  /
   data texspy ( 388 )   /  '  "plot"  tampering.  for completeness, that menu is repeated here:             '  /
   data texspy ( 389 )   /  '       spy:tek      '  /
   data texspy ( 390 )   /  '        >< to tamper with the rolling vector plot,  send choice.                '  /
   data texspy ( 391 )   /  '        >< option (mark, delay, inner, overlap, end, help) :                    '  /
   data texspy ( 392 )   /  '           mark  ---- for instantaneous marking of curves on tek screen;        '  /
   data texspy ( 393 )   /  '           delay  --- to control how simultaneous the rolling is to be;         '  /
   data texspy ( 394 )   /  '           inner  ---- to  call timval (the "inner:" level of ploting);        '  /
   data texspy ( 395 )   /  '           overlap  -- to modify the percent overlap for new-page plot;         '  /
   data texspy ( 396 )   /  '           end   ---- for return to  "spy:"  prompt.                            '  /
   data texspy ( 397 )   /  '  unless the user requests curve identification via  "mark",  the  rolling plot '  /
   data texspy ( 398 )   /  '  will only have it for the regenerated portion (the overlap from the previous  '  /
   data texspy ( 399 )   /  '  page, as controlled by  "overlap"). the  "delay"  command allows the user   '  /
   data texspy ( 400 )   /  '  to control the time-step multiplicity between plotting sessions.  there is a  '  /
   data texspy ( 401 )   /  '  trade off between solution efficiency (maximized if plotting is infrequent)   '  /
   data texspy ( 402 )   /  '  and simultaneous observation (maximized if plotting occurs on each new time   '  /
   data texspy ( 403 )   /  '  step).   so much for the concept.  but the details of counting are in fact a  '  /
   data texspy ( 404 )   /  '  little different, since it is not time steps, but rather plot points, which   '  /
   data texspy ( 405 )   /  '  are counted.   only if every solution point becomes a plot point (only if the '  /
   data texspy ( 406 )   /  '  miscellaneous data parameter  iplot is equal to unity) are these two equal,  '  /
   data texspy ( 407 )   /  '  note.  so, if  iout = 3  and the user wants to see plotting progress about    '  /
   data texspy ( 408 )   /  '  every twentieth time-step, he would send "7" in response to the prompt for a'  /
   data texspy ( 409 )   /  '  multiplicity after his  "delay"  was accepted.   the result would then be     '  /
   data texspy ( 410 )   /  '  incremental plotting every 21st step (21 = 3*7).   concerning  "overlap",  it '  /
   data texspy ( 411 )   /  '  is generally recommended that this only be used for terminals without memory, '  /
   data texspy ( 412 )   /  '  since any percentage greater than zero does increase the plotting burden.   in'  /
   data texspy ( 413 )   /  '  the case of displays with near-infinite storage (e.g., apollo windows),  it is'  /
   data texspy ( 414 )   /  '  better to leave the overlap at the default value of zero.   use of  "inner"   '  /
   data texspy ( 415 )   /  '  both very powerful and potentially very tricky, since it results in a  call to'  /
   data texspy ( 416 )   /  '  subroutine timval,  which is responsible for the  "inner:"  level of  "plot"  '  /
   data texspy ( 417 )   /  '  dialogue.   all  "inner:"  level definitions are therefore available to the   '  /
   data texspy ( 418 )   /  '  user, although effects may or may not be as expected (either trial and error  '  /
   data texspy ( 419 )   /  '  experience, or understanding of "tpplot" fortran, are required in order to    '  /
   data texspy ( 420 )   /  '  predict the results of any given operation).   the one big difference is that '  /
   data texspy ( 421 )   /  '  the use of  "time"  or just  <cr>  do not result in the production of a new   '  /
   data texspy ( 422 )   /  '  plot.  to return back to the  "tek" prompt from the  "inner:"  prompt,       '  /
   data texspy ( 423 )   /  '  send  "spy".   as for return to the "spy"  prompt,  either the listed  "end" '  /
   data texspy ( 424 )   /  '  or  "spy"  will work.    '  /
   data texspy ( 425 )   /  'key word no. 37:  "branch"      ----  ----  ----                                '  /
   data texspy ( 426 )   /  '  this response to the  "spy:"  prompt is  issued  for  a  display  of the EMTP'  /
   data texspy ( 427 )   /  '  branch table.  subsequently send an additional  "extra"  to change to the next'  /
   data texspy ( 428 )   /  '  sub-table  (as of february 1984, there  are  two),  if  different columns  are'  /
   data texspy ( 429 )   /  '  desired.  within any one choice as to sub-table,  there is a loop in which the'  /
   data texspy ( 430 )   /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy ( 431 )   /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy ( 432 )   /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage'  /
   data texspy ( 433 )   /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy ( 434 )   /  '  and return to the  "spy:"  prompt.  to refresh the heading, use either  "bran"'  /
   data texspy ( 435 )   /  '  or  "head"  (only 4 characters of  "branch"  or  "heading"  are checked).     '  /
   data texspy ( 436 )   /  'key word no. 38:  "yform"       ----  ----  ----                                '  /
   data texspy ( 437 )   /  '  this  response  to  the  "spy:"  prompt  will result in continuous (every time'  /
   data texspy ( 438 )   /  '  step)  re-formation of  [y],  followed by re-triangularization.   it is turned'  /
   data texspy ( 439 )   /  '  off (no more [y]-forming)  by sending "noy"  after  "spy:"  prompt.  ???  ???'  /
   data texspy ( 440 )   /  '  warning:  like  the earlier batch-mode  "modify deltat",  this looks better in'  /
   data texspy ( 441 )   /  '  theory than  it  does in practice.  for  important elements such as frequency-'  /
   data texspy ( 442 )   /  '  dependent transmission lines, the original data which is needed to reform  [y]'  /
   data texspy ( 443 )   /  '  has been destroyed by the solution, so the result  will  be erroneous.  unless'  /
   data texspy ( 444 )   /  '  the user has a particularly degenerate problem,  and  he is sure that he knows'  /
   data texspy ( 445 )   /  '  what he is doing, the general recommendation is to avoid all such use.  ??? ??'  /
   data texspy ( 446 )   /  'key word no. 39:  "noy"         ----  ----  ----                                '  /
   data texspy ( 447 )   /  '  this response to the  "spy:"  prompt will cancel a preceding  "yform"  request'  /
   data texspy ( 448 )   /  '  for continuous [y]-formation.    '  /
   data texspy ( 449 )   /  'key word no. 40:  "factor"      ----  ----  ----                                '  /
   data texspy ( 450 )   /  '  this  response  to  the  "spy:"  prompt  will result in continuous (every time'  /
   data texspy ( 451 )   /  '  step) triangularization.   it is turned off  (no more automatic factoring)  by'  /
   data texspy ( 452 )   /  '  sending  "nof"  in response to the  "spy:"  prompt.                           '  /
   data texspy ( 453 )   /  'key word no. 41:  "nof"         ----  ----  ----                                '  /
   data texspy ( 454 )   /  '  this  response  to  the  "spy:"  prompt  will  cancel  a  preceding   "factor"'  /
   data texspy ( 455 )   /  '  request for the continuous re-triangularization of [y].                       '  /
   data texspy ( 456 )   /  'key word no. 42:  "rlc"         ----  ----  ----                                '  /
   data texspy ( 457 )   /  '  this response to the  "spy:"  prompt will produce a display of the EMTP  r-l-c'  /
   data texspy ( 458 )   /  '  tables.  subsequently  send  an  additional  "extra"  to  change to  the  next'  /
   data texspy ( 459 )   /  '  sub-table  (as of february 1984, there  are  two),  if  different columns  are'  /
   data texspy ( 460 )   /  '  desired.  within any one choice as to sub-table,  there is a loop in which the'  /
   data texspy ( 461 )   /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy ( 462 )   /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy ( 463 )   /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage'  /
   data texspy ( 464 )   /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy ( 465 )   /  '  and return to the  "spy:"  prompt.  to refresh the heading,  use either  "rlc"'  /
   data texspy ( 466 )   /  '  or  "head"  (only 4 characters of  "heading"  are checked).                   '  /
   data texspy ( 467 )   /  'key word no. 43:  "width"       ----  ----  ----                                '  /
   data texspy ( 468 )   /  '  this response to the  "spy:"  prompt will toggle the output line length  (if  '  /
   data texspy ( 469 )   /  '  equal to 132 at the time the command is issued, it will be changed to 80,  and'  /
   data texspy ( 470 )   /  '  vice-versa).  this is for EMTP line printer output (channel lunit6) only.     '  /
   data texspy ( 471 )   /  'key word no. 44:  "bus"         ----  ----  ----                                '  /
   data texspy ( 472 )   /  '  this response to the  "spy:"  prompt will produce a display  of  the  EMTP bus'  /
   data texspy ( 473 )   /  '  vectors.   subsequently  send  an  additional  "extra"  to  change to the next'  /
   data texspy ( 474 )   /  '  next sub-table (as of february 1984, there are two),  if different columns are'  /
   data texspy ( 475 )   /  '  desired.  within any one choice as to sub-table,  there is a loop in which the'  /
   data texspy ( 476 )   /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy ( 477 )   /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy ( 478 )   /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage'  /
   data texspy ( 479 )   /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy ( 480 )   /  '  and return to the  "spy:"  prompt.  to refresh the heading,  use either  "bus"'  /
   data texspy ( 481 )   /  '  or  "head"  (only 4 characters of "heading" are checked).                     '  /
   data texspy ( 482 )   /  'key word no. 45:  "size"        ----  ----  ----                                '  /
   data texspy ( 483 )   /  '  this response  to  the  "spy:"  prompt  will  produce a display of actual EMTP'  /
   data texspy ( 484 )   /  '  data requirements  ----  the  "present figure"  list sizes which are  seen  at'  /
   data texspy ( 485 )   /  '  the end of batch-mode EMTP printout, in the case-summary statistics.         '  /
   data texspy ( 486 )   /  'key word no. 46:  "limit"       ----  ----  ----                                '  /
   data texspy ( 487 )   /  '  this response  to  the  "spy:"  prompt  will produce a display of the limiting'  /
   data texspy ( 488 )   /  '  EMTP table sizes  ----  the  "program limit"  list sizes which are seen at the'  /
   data texspy ( 489 )   /  '  end of batch-mode EMTP printout,  in the case-summary statistics.             '  /
   data texspy ( 490 )   /  'key word no. 47:  "iout"        ----  ----  ----                                '  /
   data texspy ( 491 )   /  '  this response  to  the  "spy:"  prompt  will alter the EMTP printout frequency'  /
   data texspy ( 492 )   /  '  according  to  the user"s latest desire.   the response will be instantaneous,'  /
   data texspy ( 493 )   /  '  with  a  later  minor adjustment at the first round step number  (the next one'  /
   data texspy ( 494 )   /  '  which  is  divisible  by  iout  with zero  remainder).   this command cancels'  /
   data texspy ( 495 )   /  '  any  previous  batch-mode requests  for  modification  of the output frequency'  /
   data texspy ( 496 )   /  '  (e.g., the special-request word  "change printout frequency").                '  /
   data texspy ( 497 )   /  'key word no. 48:  "node"        ----  ----  ----                                '  /
   data texspy ( 498 )   /  '  this response  to  the  "spy:"  prompt  will yield a question/answer loop with'  /
   data texspy ( 499 )   /  '  input being a user-supplied 6-character bus name, and output (the spy display)'  /
   data texspy ( 500 )   /  '  being the corresponding EMTP node number.   after any one node number has been'  /
   data texspy ( 501 )   /  '  displayed,  the  user can send  "connect"  to obtain a list of all row numbers'  /
   data texspy ( 502 )   /  '  of all connected branches/switches/nonlinear elements.                        '  /
   data texspy ( 503 )   /  'key word no. 49:  "nonlin"      ----  ----  ----                                '  /
   data texspy ( 504 )   /  '  this  response  to  the  "spy:"  prompt  is  issued  for a display of the EMTP'  /
   data texspy ( 505 )   /  '  nonlinear element table.  subsequently  send an additional  "extra"  to change'  /
   data texspy ( 506 )   /  '  to the next sub-table (as of february 1984, there are just two),  if different'  /
   data texspy ( 507 )   /  '  columns are desired.  within any one sub-table,  there is a loop in which  the'  /
   data texspy ( 508 )   /  '  user sends a pair of free-format beginning and ending row numbers for display,'  /
   data texspy ( 509 )   /  '  or  a  key word.  recognized key words here  are  "all"  for the entire table,'  /
   data texspy ( 510 )   /  '  "top"  for row one,  "bot"  for the last  row,  "next"  or  just  a  carriage'  /
   data texspy ( 511 )   /  '  return for the following row, and of course  "spy"  to  abort the display loop'  /
   data texspy ( 512 )   /  '  and return to the  "spy:"  prompt.  to refresh the heading, use either  "nonl"'  /
   data texspy ( 513 )   /  '  or  "head"  (only 4 characters of "nonlin" or "heading" are checked).         '  /
   data texspy ( 514 )   /  'key word no. 50:  "space"       ----  ----  ----                                '  /
   data texspy ( 515 )   /  '  this response to the  "spy:"  prompt begins dialogue which can rearrange      '  /
   data texspy ( 516 )   /  '  storage of either data cards or plot data points.  such use is mandatory if   '  /
   data texspy ( 517 )   /  '  no additional room for plot data remains (logic of "pltfil" will deny passage '  /
   data texspy ( 518 )   /  '  until more storage has so been freed, in such a case).  the management of card'  /
   data texspy ( 519 )   /  '  images will generally be performed only in conjunction with  "edit"  use, to  '  /
   data texspy ( 520 )   /  '  modify EMTP data.  parameters related to these two storages will be displayed:'  /
   data texspy ( 521 )   /  '     indbeg --- beginning location (index to /c29b01/) of plot data points;     '  /
   data texspy ( 522 )   /  '     indbuf --- next free cell (index to /c29b01/) for storage of plot data;    '  /
   data texspy ( 523 )   /  '     limbuf --- limit on indbuf (end of /c29b01/);                              '  /
   data texspy ( 524 )   /  '     numdcd --- last card read by EMTP (index to character*80 file6 storage)    '  /
   data texspy ( 525 )   /  '     numcrd --- largest index for card storage in file6 (as read by data);      '  /
   data texspy ( 526 )   /  '     limcrd --- limiting index for file6 card storage (dimensioned limit).      '  /
   data texspy ( 527 )   /  '  immediately after this display there will be a choice among the possible      '  /
   data texspy ( 528 )   /  '  responses  "cards",  "plot",  and  "spy".  the third of these will abort      '  /
   data texspy ( 529 )   /  '  the command, while the first two select between the two major classes of      '  /
   data texspy ( 530 )   /  '  space management.    '  /
   data texspy ( 531 )   /  '       (1) for the  "plot"  case,  the user must next choose among  "write",    '  /
   data texspy ( 532 )   /  '  "thin",  "delete",  and  "read",  basically.   the first of these writes plot '  /
   data texspy ( 533 )   /  '  points to disk just as a non-interactive program would, using i/o channel     '  /
   data texspy ( 534 )   /  '  number lunit4.   the only change is that the time span  [tbeg, tend]  to be   '  /
   data texspy ( 535 )   /  '  dumped,  as well as the frequency of the output (the effective iplot), are    '  /
   data texspy ( 536 )   /  '  under interactive control (chosen by responses to subsequent prompts).  the   '  /
   data texspy ( 537 )   /  '  "thin"  command discards points within the  [tbeg, tend]  window according to '  /
   data texspy ( 538 )   /  '  a user-specified frequency.  the  "delete"  option destroys all points        '  /
   data texspy ( 539 )   /  '  (the effective iplot is infinite) within the user-specified time range        '  /
   data texspy ( 540 )   /  '  [tbeg,  tend].   the  "read"  option is the reverse of  "write",  allowing the'  /
   data texspy ( 541 )   /  '  restoration of lunit4 disk-stored points to the working interactive memory.   '  /
   data texspy ( 542 )   /  '  this is done with user-specified frequency within the time range [tbeg, tend].'  /
   data texspy ( 543 )   /  '  finally, there is a hybrid command  "flush"  which combines  "write"  and     '  /
   data texspy ( 544 )   /  '  "thin"  (assuming that the user wants to use a common multiplicity for both   '  /
   data texspy ( 545 )   /  '  operations).  after compacting operations, there will be printout of available'  /
   data texspy ( 546 )   /  '  free space.  the normal EMTP line printer output (i/o channel lunit6) will    '  /
   data texspy ( 547 )   /  '  contain a record of the plot-file manipulations, to remind the user of what   '  /
   data texspy ( 548 )   /  '  he has done.  an extra command is  "auto",  which gives an automatic, full    '  /
   data texspy ( 549 )   /  '  "flush"  from that point onward.    '  /
   data texspy ( 550 )   /  '       (2) for the  cards  branch, the user is prompted to choose among  "move",'  /
   data texspy ( 551 )   /  '  "copy",  and  "blank".   the first of these is for block transfers (leaving   '  /
   data texspy ( 552 )   /  '  blanks in vacated locations), while the second is for block reproductions     '  /
   data texspy ( 553 )   /  '  (leaving the original locations unaltered).  the  "blank"  command is to      '  /
   data texspy ( 554 )   /  '  erase a block of cards.  in all three cases, beginning locations and total    '  /
   data texspy ( 555 )   /  '  number of cards processed are used to define the blocks involved.             '  /
   data texspy ( 556 )   /  '       many sub-commands of the large "space"  command allow the abort option  '  /
   data texspy ( 557 )   /  '  "spy" (to return to the  "spy:"  prompt), or the option of backing up one     '  /
   data texspy ( 558 )   /  '  level (via  "out").    '  /
   data texspy ( 559 )   /  'key word no. 51:  "lunit4"      ----  ----  ----                                '  /
   data texspy ( 560 )   /  '  issue  this  command  to  connect  (fortran open), disconnect (fortran close),'  /
   data texspy ( 561 )   /  '  position,  or  inspect  the  contents of  the disk file of plot data which is'  /
   data texspy ( 562 )   /  '  connected  to  i/o unit number  lunit4.   recall that a disk file can be given'  /
   data texspy ( 563 )   /  '  solution  points  via  the  "disk"  or  "flush"  subcommands  of  the  "space"'  /
   data texspy ( 564 )   /  '  command.  subcommand choices are as follows: "open",  "close",  "top",  "bot",'  /
   data texspy ( 565 )   /  '  "next",  "back",  and  "time".   in order,  these shall now                   '  /
   data texspy ( 566 )   /  '  be summarized:    '  /
   data texspy ( 567 )   /  '       open -- subsequent prompts will allow for a user-supplied file name      '  /
   data texspy ( 568 )   /  '               (not to exceed 32 characters), and the  "status"  choice         '  /
   data texspy ( 569 )   /  '               between  "new"  and  "old"  (vacuous or pre-defined files).      '  /
   data texspy ( 570 )   /  '       close -- there are no parameters; the distinction between  "keep"        '  /
   data texspy ( 571 )   /  '                and  "delete"  is implicit (all opened files should be saved).  '  /
   data texspy ( 572 )   /  '       top  --- to position the lunit4 disk file ready to read the beginning    '  /
   data texspy ( 573 )   /  '                plot data (for smallest time);                                  '  /
   data texspy ( 574 )   /  '       bot  --- to position the lunit4 disk file after reading the final plot   '  /
   data texspy ( 575 )   /  '                data (there is a read until an end-of-file).  certain computers '  /
   data texspy ( 576 )   /  '                (e.g., vax) allow writeing at this point, so old plot files can '  /
   data texspy ( 577 )   /  '                be directly added to after such an initial positioning.         '  /
   data texspy ( 578 )   /  '       next --- to read and display the plot points of the next time instant.   '  /
   data texspy ( 579 )   /  '                this forward read is fast for all known computers.              '  /
   data texspy ( 580 )   /  '       back --- to read and display plot points of the preceding time step.     '  /
   data texspy ( 581 )   /  '                since this uses backspace, be aware that for some computers     '  /
   data texspy ( 582 )   /  '                (prime, apollo, etc.), this command is internally converted to  '  /
   data texspy ( 583 )   /  '                a  "top"  and  "time" command which backs up two steps.        '  /
   data texspy ( 584 )   /  '       time --- to position the lunit4 plot file immediately after having read  '  /
   data texspy ( 585 )   /  '                points for the first step at or beyond the user-specified time; '  /
   data texspy ( 586 )   /  'key word no. 52:  "series"      ----  ----  ----                                '  /
   data texspy ( 587 )   /  '  issue this command in order to modify the values of series r-l-c branches     '  /
   data texspy ( 588 )   /  '  within the time-step loop.  such usage requires one unused row of both the    '  /
   data texspy ( 589 )   /  '  branch table (list 2) and the branch-parameter table (list 7) for each series '  /
   data texspy ( 590 )   /  '  r-l-c branch of interest.  the  "series"  command is coupled to the  "ramp"   '  /
   data texspy ( 591 )   /  '  command in that  "series"  must be used to select the branches of interest    '  /
   data texspy ( 592 )   /  '  or potential interest ahead of time, while it is  "ramp"  that actually varies'  /
   data texspy ( 593 )   /  '  them when the time comes.  this would be for continuous variation (ramping),  '  /
   data texspy ( 594 )   /  '  which is the most common case of interest.  for step changes (single-time     '  /
   data texspy ( 595 )   /  '  occurances),  "ramp"  is not used.  in either case,  actual changes to [y]    '  /
   data texspy ( 596 )   /  '  occur just prior to factoring of [y] in  "subts1"  of overlay 16.  previously,'  /
   data texspy ( 597 )   /  '  the user must issue a spy call for  "series"  ----   at any point prior to    '  /
   data texspy ( 598 )   /  '  overlay 12.  this is remembered, so that the EMTP will later automatically    '  /
   data texspy ( 599 )   /  '  break in the middle of  "over12"  with a message that this is the time to     '  /
   data texspy ( 600 )   /  '  define the table of series r-l-c branches which might later be tampered with. '  /
   data texspy ( 601 )   /  '  at the automatic break in  "over12", the user chooses among the commands     '  /
   data texspy ( 602 )   /  '  "show",  "extra",  "change",  "step", "rewind",  and  "spy".   "show"  will  '  /
   data texspy ( 603 )   /  '  display the table of series r-l-c branches which have thus far been considered'  /
   data texspy ( 604 )   /  '  for possible later change,  with  "extra"  yielding an extension to the       '  /
   data texspy ( 605 )   /  '  display (a second table,  giving starting and next values).   "change"  is the'  /
   data texspy ( 606 )   /  '  gateway to various further choices  ("data",  "move",  "blank",  "use",       '  /
   data texspy ( 607 )   /  '  "value",  "end",  and  "spy")  for the definition and manipulation of the     '  /
   data texspy ( 608 )   /  '  table of  "show".   quickly summarizing these,  "data"  is used to copy series'  /
   data texspy ( 609 )   /  '  r-l-c branches into the tamper table, while  "move"  will copy from one row of'  /
   data texspy ( 610 )   /  '  the tamper table to another, and  "blank"  will erase any such row.   "use"   '  /
   data texspy ( 611 )   /  '  allows the user to toggle the activity status of any entry in the tamper      '  /
   data texspy ( 612 )   /  '  table,  while  "value"  allows modification of r-l-c parameters in case this  '  /
   data texspy ( 613 )   /  '  is to be done discontinuously.   "end"  moves outward, back to the preceding  '  /
   data texspy ( 614 )   /  '  prompt,  while  "spy"  aborts  "show" and returns to the  "spy:"  prompt.    '  /
   data texspy ( 615 )   /  '  this completes  "change"  usage.   next is  "step",  which is the command for '  /
   data texspy ( 616 )   /  '  a manual, discontinuous change at the next available opportunity (within one  '  /
   data texspy ( 617 )   /  '  time step).   "rewind"  will erase the tamper table completely, so it is only '  /
   data texspy ( 618 )   /  '  recommended if the existing table is unsalvageable.   for details of the      '  /
   data texspy ( 619 )   /  '  behind the ramping of series r-l-c elements, see vladimir"s article in the    '  /
   data texspy ( 620 )   /  '  EMTP newsletter, vol. 4, no. 2, november, 1983.                               '  /
   data texspy ( 621 )   /  'key word no. 53:  "lock"        ----  ----  ----                                '  /
   data texspy ( 622 )   /  '  issue this command to disable time-sharing between  spy  and the ongoing EMTP '  /
   data texspy ( 623 )   /  '  simulation.  subsequent cancellation of the command is by  "go".   since this '  /
   data texspy ( 624 )   /  '  command is absolute, it can be repeated any number of times.   besides this   '  /
   data texspy ( 625 )   /  '  user-defined  "lock"  command,  there are hidden, internal ones, such as the  '  /
   data texspy ( 626 )   /  '  one by module  "pltfil"  when plot data space has been exhausted.  but a word '  /
   data texspy ( 627 )   /  '  of caution:  all of this talk about time-sharing has meaning only for those   '  /
   data texspy ( 628 )   /  '  computers which allow time-sharing (with type-ahead that is not erased by the '  /
   data texspy ( 629 )   /  '  user-keyed interrupt).    '  /
   data texspy ( 630 )   /  'key word no. 54:  "[y]"         ----  ----  ----                                '  /
   data texspy ( 631 )   /  '  issue this command to display one or more rows of the nodal admittance matrix '  /
   data texspy ( 632 )   /  '  [y] of the time-step loop.   a subsequent prompt will request the desired row,'  /
   data texspy ( 633 )   /  '  which may either be specified by bus name (assumed to begin with a letter), or'  /
   data texspy ( 634 )   /  '  by a node number.  alternate responses are  "spy"  or  "end"  (to return to   '  /
   data texspy ( 635 )   /  '  the  "spy:"  prompt),  "top"  (to display the row for node 2),  "bot"  (to    '  /
   data texspy ( 636 )   /  '  display the row for node  kpartb),  and  "next"  or  nothing  (to display the '  /
   data texspy ( 637 )   /  '  following row).   each nonzero term of the row will be displayed,  next to    '  /
   data texspy ( 638 )   /  '  the associated column number.   a minus sign applied to the column number is  '  /
   data texspy ( 639 )   /  '  used to mark the largest column of the row.   in case separate  "examine"     '  /
   data texspy ( 640 )   /  '  usage might be contemplated,  kks  points to the  (km, ykm)  pairs.           '  /
   data texspy ( 641 )   /  'key word no. 55:  "[f]"         ----  ----  ----                                '  /
   data texspy ( 642 )   /  '  issue this command to display one or more rows of the table of factors (the   '  /
   data texspy ( 643 )   /  '  triangularized [y] of the time-step loop).   display options are generally    '  /
   data texspy ( 644 )   /  '  identical to the immediately-preceding  "[y]"  command,  except that here     '  /
   data texspy ( 645 )   /  '  only the upper triangle exists,  a minus sign marks the diagonal entry,  and  '  /
   data texspy ( 646 )   /  '  kk  is the pointer (which can be observed  via the  "bus"  command).          '  /
   data texspy ( 647 )   /  'key word no. 55:  "noroll"      ----  ----  ----                                '  /
   data texspy ( 648 )   /  '  use this command to cancel  rolling character and/or vector plots.   these   '  /
   data texspy ( 649 )   /  '  began,  it should be recalled,  via "rollc"  and/or  "rollv"  commands       '  /
   data texspy ( 650 )   /  '  (independent of each other, and also independent of the "tek" setting) at the '  /
   data texspy ( 651 )   /  '  "inner:"  level of plotting.   the user-keyed interrupt will by itself stop   '  /
   data texspy ( 652 )   /  '  the rolling only if control is within "chrplt"  or  "tekplt"  plotting       '  /
   data texspy ( 653 )   /  '  modules when it is spotted.  otherwise, this added manual request is needed.  '  /
   data texspy ( 654 )   /  '  the only alternative would be to go back into the  "inner:"  level of         '  /
   data texspy ( 655 )   /  '  plotting to toggle the appropriate switch (via  "rollc"  and/or  "rollv"      '  /
   data texspy ( 656 )   /  '  requests).    '  /
   data texspy ( 657 )   /  'key word no. 57:  "open"        ----  ----  ----                                '  /
   data texspy ( 658 )   /  '  issue this command to  open  a formatted disk file.  a loop will be entered in'  /
   data texspy ( 659 )   /  '  which there will be a prompt for the next i/o unit number, and the disk       '  /
   data texspy ( 660 )   /  '  file which is to be connected thereto.  to exit the loop, send a blank line.  '  /
   data texspy ( 661 )   /  'key word no. 58:  "close"       ----  ----  ----                                '  /
   data texspy ( 662 )   /  '  issue this command to  close  a disk file.  a loop will be entered in         '  /
   data texspy ( 663 )   /  '  which there will be a prompt for the next i/o unit number, and the  status    '  /
   data texspy ( 664 )   /  '  (either  keep  or  delete  are ok). to exit the loop, send a blank line.     '  /
   data texspy ( 665 )   /  'key word no. 59:  "sm"          ----  ----  ----                                '  /
   data texspy ( 666 )   /  '  issue this command to display the various electrical, mechanical, and solution'  /
   data texspy ( 667 )   /  '  parameters of any type-59 synchronous machine (s.m.).  time-sharing by spy    '  /
   data texspy ( 668 )   /  '  is automatically suspended during this display, due to algorithmic limitations'  /
   data texspy ( 669 )   /  '  of the type-59 model (examination is only possible as that machine is being   '  /
   data texspy ( 670 )   /  '  processed within subroutine update, which is called by "subts1" (the first    '  /
   data texspy ( 671 )   /  '  piece of the time-step loop.  the user can issue the  "sm"  request at any    '  /
   data texspy ( 672 )   /  '  time, but in fact the EMTP simulation will stop within the processing loop of '  /
   data texspy ( 673 )   /  '  "update", and allow the user to accept or reject (y or n responses) each      '  /
   data texspy ( 674 )   /  '  machine in turn.  actually, only  "n" vetos the opportunity (a simple <cr>   '  /
   data texspy ( 675 )   /  '  will accept the next machine).  once considering any one type-59 s.m., there  '  /
   data texspy ( 676 )   /  '  are subsequent choices as to which parameters the user wants to see:  "elec"  '  /
   data texspy ( 677 )   /  '  for electrical parameters,  "mech"  for mechanical parameters, etc.           '  /
   data texspy ( 678 )   /  '  ?????????   as of 26 feb 84, command is not complete   ?????????????????????  '  /
   data texspy ( 679 )   /  'key word no. 60:  "honk"        ----  ----  ----                                '  /
   data texspy ( 680 )   /  '  issue this command to ring the terminal bell (i.e., honk its horn).  there    '  /
   data texspy ( 681 )   /  '  will then be a prompt for an integer severity level, which should be between  '  /
   data texspy ( 682 )   /  '  one and ten (zero produces no response, while 10 indicates a disaster).       '  /
   data texspy ( 683 )   /  'key word no. 61:  "choice"      ----  ----  ----                                '  /
   data texspy ( 684 )   /  '  issue this command to see output variable names of any of the five classes    '  /
   data texspy ( 685 )   /  '  making up the EMTP output vector (1=node voltages,  2=branch voltages,  etc.).'  /
   data texspy ( 686 )   /  '  order of the five classes corresponds to the order of concatenation in the    '  /
   data texspy ( 687 )   /  '  EMTP output vector;  and within each class, the order corresponds, too.  the  '  /
   data texspy ( 688 )   /  '  display is dynamic,  so it always reflects the current output variables       '  /
   data texspy ( 689 )   /  '  (unlike  "choice"  at the  "middle:" level of plotting,  which only displays '  /
   data texspy ( 690 )   /  '  the original status of overlay 15). '  /
   data texspy ( 691 )   /  'key word no. 62:  "tacs"        ----  ----  ----                                '  /
   data texspy ( 692 )   /  '  issue this command as the gateway to concurrent sequential processing (csp),  '  /
   data texspy ( 693 )   /  '  which is based on special, spy-defined tacs supplemental variable usage.  then'  /
   data texspy ( 694 )   /  '  one enters a loop over csp control commands.   if  "rewind"  is requested,    '  /
   data texspy ( 695 )   /  '  all previous definitions are erased, so definitions begin from level zero.  if'  /
   data texspy ( 696 )   /  '  "source"  is sent,  all following input is assumed to be tacs source cards,   '  /
   data texspy ( 697 )   /  '  until terminated by an  "end"  card. if  "supplemental"  is sent,  then all '  /
   data texspy ( 698 )   /  '  following data cards (spy input) is assumed to be tacs supplemental variable/ '  /
   data texspy ( 699 )   /  '  device data cards,  also until an  "end"  card is encountered.  the subcommand'  /
   data texspy ( 700 )   /  '  "patch"  allows the user to connect any variables to his tacs sources  (to    '  /
   data texspy ( 701 )   /  '  define the inputs),  and apply any supplemental variable results anywhere.    '  /
   data texspy ( 702 )   /  '  the "any" comes from the use of a memory address of the spy commands  "list"  '  /
   data texspy ( 703 )   /  '  or  "find",  just as  "examine"  or "deposit"  are quite unrestricted.  there'  /
   data texspy ( 704 )   /  '  will be separate prompts for all such tacs input and output connections.  when'  /
   data texspy ( 705 )   /  '  all such connections are complete,  "show"  can be used to display a summary  '  /
   data texspy ( 706 )   /  '  table of all tacs csp input/output connections.   working room for such usage '  /
   data texspy ( 707 )   /  '  can be controlled by the user in several ways, if there are shortages.  first,'  /
   data texspy ( 708 )   /  '  if a user wants dummy output channels for printing/plotting, reserve dummy    '  /
   data texspy ( 709 )   /  '  node voltage outputs with  "chan01" punched in cols. 3-8, and the desired    '  /
   data texspy ( 710 )   /  '  number punched in columns 9-16 with i8format.   resultant names will be      '  /
   data texspy ( 711 )   /  '  serialized from the aforementioned root, and added to the output vector at the'  /
   data texspy ( 712 )   /  '  point of definition (if this card is first, all such dummy names will come    '  /
   data texspy ( 713 )   /  '  first, before other node voltages). second, there is array dimensioning for  '  /
   data texspy ( 714 )   /  '  all vectors of the  "choice"  table ---  presently fixed in deck "dekspy", at'  /
   data texspy ( 715 )   /  '  the moment.  finally, in addition to the familiar tacs table limits, there is '  /
   data texspy ( 716 )   /  '  an automatic reservation of space for all user-defined tacs csp sources.  this'  /
   data texspy ( 717 )   /  '  is maximized automatically, within existing tacs dimensions (as assigned by   '  /
   data texspy ( 718 )   /  '  absolute tacs dimensions, for example).  as usual,  "spy"  will break out of  '  /
   data texspy ( 719 )   /  '  any loop, in case an abortion is desired.                                     '  /
   data texspy ( 720 )   /  'key word no. 63:  "wait"        ----  ----  ----                                '  /
   data texspy ( 721 )   /  '  the response to this spy command will be a prompt for the desired delay       '  /
   data texspy ( 722 )   /  '  (hibernation) time in seconds.  once this is accepted, the installation-      '  /
   data texspy ( 723 )   /  '  dependent subroutine tdelay  is called, to return control only after the      '  /
   data texspy ( 724 )   /  '  requested nap.    '  /
   data texspy ( 725 )   /  'key word no. 64:  "v-i""        ----  ----  ----                                '  /
   data texspy ( 726 )   /  '  this spy command is used to examine the list-10 characteristics of all        '  /
   data texspy ( 727 )   /  '  nonlinear and pseudo-nonlinear elements.  the name  "v-i"  is symbolic        '  /
   data texspy ( 728 )   /  '  only, since flux-current or resistance-time characteristics are also          '  /
   data texspy ( 729 )   /  '  included.  immediately upon entry, there will be a display of the number      '  /
   data texspy ( 730 )   /  '  of nonlinear elements (list 9) and data points of the characteristics         '  /
   data texspy ( 731 )   /  '  (list 10).  also shown will be a listof options to a pair of free-format     '  /
   data texspy ( 732 )   /  '  row numbers:    '  /
   data texspy ( 733 )   /  '     next or <cr>  ---- to display the characteristic of the next element       '  /
   data texspy ( 734 )   /  '                        of the nonlinear element table.  there is wrap around   '  /
   data texspy ( 735 )   /  '                        from beginning to end.  no initialization required.     '  /
   data texspy ( 736 )   /  '     last  ---- to display the characteristic of the preceding element of       '  /
   data texspy ( 737 )   /  '                the nonlinear element table.  there is wrap around from the     '  /
   data texspy ( 738 )   /  '                beginning to the end. no initialization is required.           '  /
   data texspy ( 739 )   /  '     all  --- to display all rows of the list 10 characteristics.               '  /
   data texspy ( 740 )   /  '     spy, end, or stop ---- to abort display loop, and return to  "spy:"        '  /
   data texspy ( 741 )   /  '     mode  ---- to toggle the binary switch which selects between a display     '  /
   data texspy ( 742 )   /  '                by elements (value n8=0), and a display by rows of list 10.     '  /
   data texspy ( 743 )   /  '                initially, the display by elements, only one at a time, is      '  /
   data texspy ( 744 )   /  '                assumed.    '  /
   data texspy ( 745 )   /  '  in case the use inputs a pair of numbers instead of one of these key          '  /
   data texspy ( 746 )   /  '  words, then the response depends on mode.   for the display by elements,     '  /
   data texspy ( 747 )   /  '  the first list-9 entry with a characteristic contained within the range       '  /
   data texspy ( 748 )   /  '  [n1, n2]  is identified, and displayed (where n1 and n2 are the ordered       '  /
   data texspy ( 749 )   /  '  pair of free-format numbers which the user has inputted.  on the other hand,  '  /
   data texspy ( 750 )   /  '  with  mode  toggled for the absolute, unlimited display of list 10 rows,      '  /
   data texspy ( 751 )   /  '  then these rows (within limits of availability) are displayed.                '  /
   data texspy ( 752 )   /  '  the following plot commands are honored at all three levels (either  "outer:",'  /
   data texspy ( 753 )   /  '  "middle",  or  "inner:")  of plot dialogue:                                   '  /
   data texspy ( 754 )   /  '  stop  ---  to abort  "plot"  activity,  and return to the  "spy:"  prompt.    '  /
   data texspy ( 755 )   /  '  spy   ---  to abort  "plot"  activity,  and return to the  "spy:"  prompt.    '  /
   data texspy ( 756 )   /  '  in   ----  to transfer one level inward  (either from  "outer"  to  "middle:" '  /
   data texspy ( 757 )   /  '             or from  "middle"  to  "inner")  in the plot dialogue.             '  /
   data texspy ( 758 )   /  '  help  ---  to produce the display now being viewed.  if unqualified, messages '  /
   data texspy ( 759 )   /  '             will be restricted to the level of current operation.  but one     '  /
   data texspy ( 760 )   /  '             blank and then a qualifier ("all", "outer", "middle", or "inner")  '  /
   data texspy ( 761 )   /  '             provide the opportunity to see messages of other levels, too.      '  /
   data texspy ( 762 )   /  '  debug  --  to alter the level of diagnostic printout.  if none is now being   '  /
   data texspy ( 763 )   /  '             used, this command will turn it on.  in fact, the control is       '  /
   data texspy ( 764 )   /  '             identical to that used in response to the  "spy:"  prompt (iprspy).'  /
   data texspy ( 765 )   /  '               < < <  =======   end of any-level  commands   =======  > > >     '  /
   data texspy ( 766 )   /  '  set data -- to over-ride default data values with user-established choices.   '  /
   data texspy ( 767 )   /  '              the associated user-defined data file must be pre-stored on disk, '  /
   data texspy ( 768 )   /  '              and must have the file name  "tpparam.dat".                       '  /
   data texspy ( 769 )   /  '  tek  ---- to toggle the switch which chooses between character plotting and   '  /
   data texspy ( 770 )   /  '            vector-graphic (e.g., tektronix) plotting.  the default may be      '  /
   data texspy ( 771 )   /  '            computer dependent, or come from  "set data"  usage.                '  /
   data texspy ( 772 )   /  '  column -- to toggle the switch that chooses between 80 and 132-column widths  '  /
   data texspy ( 773 )   /  '            for character plotting.    '  /
   data texspy ( 774 )   /  '  set column -- to set character plot widths to any value.  but if either 80 or '  /
   data texspy ( 775 )   /  '                132 is desired (common choices), use the simpler  "column".     '  /
   data texspy ( 776 )   /  '               < < <  =======   end of outer-level  commands   =======  > > >   '  /
   data texspy ( 777 )   /  '  timespan -- to be shown the time range  [t-min, t-max]  of the plot data.     '  /
   data texspy ( 778 )   /  '  choice -- to produce a tabulation of plotable EMTP variables.   but be very   '  /
   data texspy ( 779 )   /  '            careful, since the resultant tabulation corresponds to the status   '  /
   data texspy ( 780 )   /  '            before the time-step loop was entered.  for a dynamic tabulation,   '  /
   data texspy ( 781 )   /  '            use  "choice"  in response to the  "spy:"  prompt.                  '  /
   data texspy ( 782 )   /  '  time units -- to specify the integer which characterizes the time units used  '  /
   data texspy ( 783 )   /  '                in the plotting.  this is just as with batch-mode EMTP plotting,'  /
   data texspy ( 784 )   /  '                where:  "1"  -- for degrees based on the power frequency;       '  /
   data texspy ( 785 )   /  '                        "2"  -- for cycles  based on the power frequency;       '  /
   data texspy ( 786 )   /  '                        "3,4,5"  -- for seconds, milliseconds, and microseconds '  /
   data texspy ( 787 )   /  '                        "6,7" -- for  "frequency scan"  data cases, using either'  /
   data texspy ( 788 )   /  '                                 hz or the log to base 10 of hz,  rather than t.'  /
   data texspy ( 789 )   /  '  x   ----- any garbage character (including blank) represents a request for the'  /
   data texspy ( 790 )   /  '            program to begin the interrogation for plot variables and labeling. '  /
   data texspy ( 791 )   /  '            hereafter, in response to the different prompts, several special    '  /
   data texspy ( 792 )   /  '            key words are applicable (immediately below).  upon the completion  '  /
   data texspy ( 793 )   /  '            of all such information, there is an automatic transfer to the      '  /
   data texspy ( 794 )   /  '            "inner:"  level of program dialogue.  but first, options are:       '  /
   data texspy ( 795 )   /  '    repeat >> to reuse all former plot labels, send  "repeat"  when first asked '  /
   data texspy ( 796 )   /  '              for label information (which begins with the super title).  for   '  /
   data texspy ( 797 )   /  '              the first plot, "former labels" are all blank (initialization).   '  /
   data texspy ( 798 )   /  '    back >>>> to abort input of plot variables, and return to "middle:" prompt. '  /
   data texspy ( 799 )   /  '    end  >>>> to terminate all indeterminate inputs.  included are the three    '  /
   data texspy ( 800 )   /  '              classes of plot variables, and lines of case-title text.          '  /
   data texspy ( 801 )   /  '    last >>>> to terminate all plot-variable input, and begin specifying labels.'  /
   data texspy ( 802 )   /  '    blank >>> sending nothing (just a carriage return) will usually reuse the   '  /
   data texspy ( 803 )   /  '              old datum (e.g., node name, or line of case-title text).  old data'  /
   data texspy ( 804 )   /  '              are usually displayed within parentheses for this reason.         '  /
   data texspy ( 805 )   /  '    flush >>> to rewind the pointer which counts the lines of case-title text.  '  /
   data texspy ( 806 )   /  '              the text then remains, to be accepted or overwritten line by line.'  /
   data texspy ( 807 )   /  '    playback> to display the entire present case title.  this command is legal  '  /
   data texspy ( 808 )   /  '              at any point before the "end"  which freezes the case title.     '  /
   data texspy ( 809 )   /  '  label  -- to skip around the plot-variable specification (see "x" above), and '  /
   data texspy ( 810 )   /  '            begin the input of plot labels.  plot variables remain unchanged.   '  /
   data texspy ( 811 )   /  '               < < <  =======   end of middle-level  commands   =======  > > >  '  /
   data texspy ( 812 )   /  '  extrema -- to toggle the switch that decides whether or not variable extrema  '  /
   data texspy ( 813 )   /  '             of subsequent plots are to be displayed.  such output precedes the '  /
   data texspy ( 814 )   /  '             associated plot (as does that of  "level"  below).  the program    '  /
   data texspy ( 815 )   /  '             then pauses before drawing the graph, waiting for the user to send '  /
   data texspy ( 816 )   /  '             a blank.  if the user wants to skip the plot and return to the     '  /
   data texspy ( 817 )   /  '             "middle:"  level of dialogue, send  "no plot".                     '  /
   data texspy ( 818 )   /  '  level -- to toggle the switch that decides whether or not level-triggers for  '  /
   data texspy ( 819 )   /  '           variables are to be activated.  if such triggers are being turned on,'  /
   data texspy ( 820 )   /  '           the program will next request a level vector.  the response is using '  /
   data texspy ( 821 )   /  '           free-format.  the same pause exists as with  "extrema".              '  /
   data texspy ( 822 )   /  '  smooth -- to change the tolerance which is used to discard plot points.       '  /
   data texspy ( 823 )   /  '  size   -- to change the length of the time axis of character plots.           '  /
   data texspy ( 824 )   /  '  show   -- to display the current values of many important plot parameters.    '  /
   data texspy ( 825 )   /  '  factor -- to specify a new vector of multiplicative scaling factors for plot  '  /
   data texspy ( 826 )   /  '            variables (the "a" of  z = a*y + b ).   zero is taken to mean unity,'  /
   data texspy ( 827 )   /  '            and free-format is used. '  /
   data texspy ( 828 )   /  '  offset -- like  "factor",  only to specify the vector of constants  "b".      '  /
   data texspy ( 829 )   /  '  rescale-- to return to natural scaling (i.e.,  a=1.0 ,  b=0.0 ) for all plot  '  /
   data texspy ( 830 )   /  '            variables.  this is used to erase previous  "factor"  or  "offset". '  /
   data texspy ( 831 )   /  '  limits -- to specify new minimum and maximum values for the vertical axis of  '  /
   data texspy ( 832 )   /  '            plots.  send  "0,0"  to cancel previous manually-specified limits,  '  /
   data texspy ( 833 )   /  '            and return to the automatic scaling of the computer.                '  /
   data texspy ( 834 )   /  '  average-- to change the limit on the number of consecutive oscillations after '  /
   data texspy ( 835 )   /  '            which the averaging of successive ordinates is to be instituted.    '  /
   data texspy ( 836 )   /  '  time   -- to specify time-axis limits t-min  and  t-max  of the plot.  this  '  /
   data texspy ( 837 )   /  '            must be done at least once, for the first plot, unless  "timespan"  '  /
   data texspy ( 838 )   /  '            was issued at the  "middle:"  level (resulting in full time range). '  /
   data texspy ( 839 )   /  '  all time--this is a request for a plot over the full time range of the data.  '  /
   data texspy ( 840 )   /  '            it functions correctly only if  "timespan"  was previously used at  '  /
   data texspy ( 841 )   /  '            the  "middle:"  level. '  /
   data texspy ( 842 )   /  '  blank  -- a blank (just a <cr>) is interpreted as a request for a plot using  '  /
   data texspy ( 843 )   /  '            time-axis scaling as for the preceding plot (or the full range, if  '  /
   data texspy ( 844 )   /  '            no previous plots existed, but  "timespan"  was ordered).           '  /
   data texspy ( 845 )   /  '  cursor -- to toggle the switch that indicates whether or not cursor input is  '  /
   data texspy ( 846 )   /  '            expected after the next vector plot.  when the cursor is switched to'  /
   data texspy ( 847 )   /  '            "on",  following the plot there will be keyboard input following the'  /
   data texspy ( 848 )   /  '            positioning of the cursor: '  /
   data texspy ( 849 )   /  '     p  >>>>> to mark another cursor point (wherever the cursor is sitting)     '  /
   data texspy ( 850 )   /  '     e  >>>>> to terminate such marking input (end of cursor use, actually).    '  /
   data texspy ( 851 )   /  '     show >>> to produce a tabulation of all cursor points (previous "p" usage).'  /
   data texspy ( 852 )   /  '     slope >> to produce  dx,  dy,  f, and  dy/dx  of any pair of points that  '  /
   data texspy ( 853 )   /  '              the user is interested in.  after the last such  (m,k)  pair of   '  /
   data texspy ( 854 )   /  '              point numbers which are of interest,  send  "0,0"  to terminate.  '  /
   data texspy ( 855 )   /  '     end  >>> to terminate all cursor displays, and return to  "inner:"  prompt.'  /
   data texspy ( 856 )   /  '  x-y plot--to toggle the switch which chooses between regular plotting as a    '  /
   data texspy ( 857 )   /  '            function of time (the default), and x-y plotting.                   '  /
   data texspy ( 858 )   /  '  rollv  -- to toggle the switch which produces a  rolling  vector plot.   it is'  /
   data texspy ( 859 )   /  '            normal to turn on such rolling after the basic plot is in place, and'  /
   data texspy ( 860 )   /  '            the axis scaling is judged to be appropriate.  then, after  "rollv",'  /
   data texspy ( 861 )   /  '            the user will normally send "spy"  to return to the  "spy:"  prompt'  /
   data texspy ( 862 )   /  '            (from which  "go"  will be required to actually make the plot  roll,'  /
   data texspy ( 863 )   /  '            if  "break"  or  "lock"  were in effect at that point).   should the'  /
   data texspy ( 864 )   /  '            user wish to cancel a rolling vector plot from spy, send  "noroll"  '  /
   data texspy ( 865 )   /  '            (which will stop both  rolling  vector and character plots).        '  /
   data texspy ( 866 )   /  '  rollc  -- to toggle the switch which produces a  rolling  character plot.  see'  /
   data texspy ( 867 )   /  '            preceding  "rollv"  for details (functioning is comparable).        '  /
   data texspy ( 868 )   /  '               < < <  =======   end of inner-level  commands   =======  > > >   '  /
   data kbegtx (  1 )   /     1   /
   data kbegtx (  2 )   /     4   /
   data kbegtx (  3 )   /     9   /
   data kbegtx (  4 )   /    22   /
   data kbegtx (  5 )   /    29   /
   data kbegtx (  6 )   /    41   /
   data kbegtx (  7 )   /    53   /
   data kbegtx (  8 )   /    64   /
   data kbegtx (  9 )   /    69   /
   data kbegtx ( 10 )   /    79   /
   data kbegtx ( 11 )   /    88   /
   data kbegtx ( 12 )   /    97   /
   data kbegtx ( 13 )   /   112   /
   data kbegtx ( 14 )   /   119   /
   data kbegtx ( 15 )   /   136   /
   data kbegtx ( 16 )   /   141   /
   data kbegtx ( 17 )   /   154   /
   data kbegtx ( 18 )   /   158   /
   data kbegtx ( 19 )   /   162   /
   data kbegtx ( 20 )   /   202   /
   data kbegtx ( 21 )   /   217   /
   data kbegtx ( 22 )   /   228   /
   data kbegtx ( 23 )   /   237   /
   data kbegtx ( 24 )   /   242   /
   data kbegtx ( 25 )   /   252   /
   data kbegtx ( 26 )   /   263   /
   data kbegtx ( 27 )   /   294   /
   data kbegtx ( 28 )   /   303   /
   data kbegtx ( 29 )   /   317   /
   data kbegtx ( 30 )   /   325   /
   data kbegtx ( 31 )   /   333   /
   data kbegtx ( 32 )   /   340   /
   data kbegtx ( 33 )   /   345   /
   data kbegtx ( 34 )   /   361   /
   data kbegtx ( 35 )   /   377   /
   data kbegtx ( 36 )   /   381   /
   data kbegtx ( 37 )   /   425   /
   data kbegtx ( 38 )   /   436   /
   data kbegtx ( 39 )   /   446   /
   data kbegtx ( 40 )   /   449   /
   data kbegtx ( 41 )   /   453   /
   data kbegtx ( 42 )   /   456   /
   data kbegtx ( 43 )   /   467   /
   data kbegtx ( 44 )   /   471   /
   data kbegtx ( 45 )   /   482   /
   data kbegtx ( 46 )   /   486   /
   data kbegtx ( 47 )   /   490   /
   data kbegtx ( 48 )   /   497   /
   data kbegtx ( 49 )   /   503   /
   data kbegtx ( 50 )   /   514   /
   data kbegtx ( 51 )   /   559   /
   data kbegtx ( 52 )   /   586   /
   data kbegtx ( 53 )   /   621   /
   data kbegtx ( 54 )   /   630   /
   data kbegtx ( 55 )   /   641   /
   data kbegtx ( 56 )   /   647   /
   data kbegtx ( 57 )   /   657   /
   data kbegtx ( 58 )   /   661   /
   data kbegtx ( 59 )   /   665   /
   data kbegtx ( 60 )   /   679   /
   data kbegtx ( 61 )   /   683   /
   data kbegtx ( 62 )   /   691   /
   data kbegtx ( 63 )   /   720   /
   data kbegtx ( 64 )   /   725   /
   data kbegtx ( 65 )   /   752   /
   data kbegtx ( 66 )   /   766   /
   data kbegtx ( 67 )   /   777   /
   data kbegtx ( 68 )   /   812   /
   data kbegtx ( 69 )   /   869   /
end block data

!
! subroutine sysplt.
!
subroutine sysplt ( lunit4 )
  !     module of interactive emtp only, which services "emtspy".
  !     if no interactive emtp use, this module can be deleted.
  !     it is called only by "sysdep", for storage in "dekplt".
  !     separate module needed because "dekplt" has no implicit.
  include 'dekplt.ftn'
  l4plot = lunit4
  return
end subroutine sysplt
!
!     subroutine stopin.
!
subroutine stopin
  implicit real*8 (a-h, o-z),  integer*4 (i-n)
  !     universal module of interactive emtp (spy of "emtspy").
  !     if non-interactive version, module can be destroyed.  this
  !     module is called only to display erroneous file6(istep), &
  !     prompt user to send a corrected copy.  called by "datain".
  include 'blkcom.ftn'
  include 'dekspy.ftn'
  write (munit6, 1218) istep
1218 format ('   ? ? ?   trouble with input data.  last card', ' number',  i5,  '   is in error.'   )
  call window
  write (munit6, 1219)
1219 format ( '     1234567890123456789012345678901234567890', '1234567890123456789012345678901234567890' )
  call window
  write (munit6, 1221)  file6(istep)
1221 format ( a80 )
  call window
  if ( m4plot .eq. 1 )  go to 1227
  kill = 79
  lstat(19) = 1218
  go to 9000
1227 write (prom80, 1232)
1232 format ( ' send corrected card (spy, stop) :' )
  call prompt
  read (munit5, 1236)  buff77
1236 format ( a80 )
  if ( buff77(1:4) .eq. 'stop' ) call stoptp
  if ( buff77(1:4) .ne. 'spy' )  go to 1244
  call spying
  go to 9000
1244 file6(istep) = buff77
9000 return
end subroutine stopin
!
! subroutine rtmplt.
!
subroutine  rtmplt
  !     module used only for interactive emtp (service to "emtspy").
  !     for non-interactive emtp, this module can be destroyed.
  call tpplot
end subroutine rtmplt
!
!     data block blkplt.
!
block data blkplt
   !     module used only for interactive emtp (service to "emtspy").
   !     for non-interactive emtp, this module can be destroyed.
   include 'dekspy.ftn'
   include 'dekplt.ftn'
   data texfnt  /  'f7x13.b '  /
   data  xytitl(1:24)  /  '                        '  /
   data  headl(1:16)   /  '                '  /
   data  vertl(1:16)   /  '                '  /
   data  horzl(1)    /  'degrees based on 60 hz  '  /
   data  horzl(2)    /  'cycles based on 60 hz   '  /
   data  horzl(3)    /  'seconds                 '  /
   data  horzl(4)    /  'milliseconds            '  /
   data  horzl(5)    /  'microseconds            '  /
   data  horzl(6)    /  'frequency in hertz      '  /
   data  horzl(7)    /  'log10 frequency in hertz'  /
   data  horzl(8)    /  '1st variable of x-y pair'  /
   data  curren     /  'current '  /
   data  voltag     /  'voltage '  /
   !     begin command-word definitions.   ^^^^^^  ^^^^^^   ^^^^^^   ^^^^^
   data  choice     /  'choice  '  /
   data  stop       /  'stop    '  /
   data  purge      /  'purge   '  /
   data  help       /  'help    '  /
   data  smooth     /  'smooth  '  /
   data  size       /  'size    '  /
   data  show       /  'show    '  /
   data  linezz     /  'line    '  /
   data  photo      /  'copy    '  /
   data  end        /  'end     '  /
   data  repeat     /  'repeat  '  /
   data  flush      /  'flush   '  /
   data  playba     /  'playback'  /
   data  pen        /  'pen     '  /
   data  multip     /  'factor  '  /
   data  offset     /  'offset  '  /
   data  limits     /  'limits  '  /
   data  time       /  'time    '  /
   data  timesp     /  'timespan'  /
   data  debug      /  'debug   '  /
   data  tek        /  'tek     '  /
   data  stack      /  'stack   '  /
   data  printe     /  'printer '  /
   data  metric     /  'metric  '  /
   data  alltim     /  'all time'  /
   data  column     /  'column  '  /
   data  setcol     /  'set colu'  /
   data  out        /  'out     '  /
   data  longer     /  'longer  '  /
   data  averag     /  'average '  /
   data  inner      /  'in      '  /
   data  rescal     /  'rescale '  /
   data  lastpl     /  'last    '  /
   data  batch      /  'batch   '  /
   data  punch      /  'punch   '  /
   data  extrem     /  'extrema '  /
   data  level      /  'level   '  /
   data  noplot     /  'no plot '  /
   data  messag     /  'message '  /
   data  timeun     /  'time uni'  /
   data  label      /  'label   '  /
   data  cursor     /  'cursor  '  /
   data  xyplot     /  'x-y plot'  /
   data  slope      /  'slope   '  /
   data  back       /  'back    '  /
   data  refile     /  'refile  '  /
   data  texblk     /  'blank   '  /
   data  setdat     /  'set data'  /
   !     end of command definitions  ^^^^^^   ^^^^^^   ^^^^^^   ^^^^^^
   data  tolrce     /  8.e-5  /
   data  finfin     /  1.e12  /
   data  timbeg     /   0.0   /
   data  timend     /  1.e20  /
   data  paplim     /  36.    /
   data  vs      /  1.0  /
   data  vl      /  5.0  /
   data  vh      /  6.0  /
   data  nsmplt  /  50   /
   data  kslowr  /   3   /
   data  limfix  /   0   /
   data  klevl   /   0   /
   data  kextr   /   0   /
   data  jhmsp   /   0   /
   data  taxisl  /  5.0  /
   data  mu6std  /  6  /
   data  htax    /  4.0  /
   data  limcol  /  79   /
   data  ltek    /  1   /
   data  numtek  /   0   /
   data  inwait  /   1   /
   !     begin parameters of tektronix screen
   data  nxinch   /    74   /
   data  nyinch   /    68   /
   data  nxoff    /    100  /
   data  nyoff    /    40   /
   data  nxvern   /    30   /
   data  inchpx   /    2    /
   data  inchpy   /    2    /
   data  look     /    6    /
   data  nymax    /   800   /
   data  nxmax    /   800   /
   data  lchid    /    2    /
   data  lchsup   /    1    /
   data  lchtit   /    2    /
   data  lchxax   /    0    /
   data  lchyax   /    0    /
   data  izgr1    /    0    /
   data  izgr2    /    0    /
   data  ldshg1   /    1    /
   data  ldshg2   /    1    /
   data  izxax    /    0    /
   data  izyax    /    0    /
   data  izid     /    0    /
   data  iterm    /    2    /
   data  ltic     /    7    /
   data  iztit    /    0    /
   data  nxid6    /   10   /
   data  nyid6    /   330  /
   data  nxend    /   512   /
   data  nyend    /    50   /
   data  icurse   /    0    /
   data  ichref   /   1hp   /
   data  ichend   /   1he   /
   data  vaxisl   /   4.0   /
   data  fxnumv   /   1.5   /
   data  fxnumh   /   5.0   /
   data  fxvert   /   0.0   /
   data  fsymb    /   .83   /
   data  lsymb    /    1    /
   data  lchfil   /    0    /
   data  lchlim   /    0    /
   data  ibaud    /   960   /
   !     end parameters of tektronix screen
   data  lu7plt   /  7  /
   data  linepr   /  9  /
   data  mxypl    /  0  /
   data  numflt   /  0  /
   data  numtit   /  0  /
   data  xtit     /  0.5  /
   data  ytit     /  8.5  /
   data  siztit   /  .12  /
   data  xsuper   /  1.0  /
   data  ysuper   /  9.0  /
   data  sizsup   /  0.3  /
   data  nchsup   /   0   /
   data  nchver   /   0   /
   data  dxgrd1   /  1.0  /
   data  dygrd1   /  1.0  /
   data  dxgrd2   /  0.2  /
   data  dygrd2   /  0.2  /
   data  fill1    /  1.0  /
   data  fill2    /  1.0  /
   data  ncut1    /   1   /
   data  ncut2    /   1   /
   data  maxsym   /   3   /
   data  fline    /  1.7  /
   data  sizid    /  .12  /
   data  xid      /  0.5  /
   data  yid      /  .75  /
   data  fact     /  1.0  /
   data  fhtax    /  0.5  /
   data  fxsup    /  0.3  /
   data  fysup    /  -.03 /
   data  fxtit    /  .10  /
   data  fytit    /  .15  /
   data  fxid     /  .05  /
   data  fyid     /  .10  /
   data  ftcarr   /  1.0  /
   data  fvaxtt   /  -7.5  /
   data  mtit     /  3  /
   data  maxisx   /  3  /
   data  maxisy   /  3  /
   data  mgrid1   /  2  /
   data  mgrid2   /  1  /
   data  msuper   /  5  /
   data  mid      /  3  /
   data  mline    /  3  /
   data  killpl   /  0  /
end block data
!
!     subroutine tpplot.
!
subroutine tpplot
  !     module used only for interactive emtp (service to "emtspy").
  !     for non-interactive emtp, this module can be destroyed.
  include 'dekspy.ftn'
  include 'dekplt.ftn'
  save
  if ( nexmod .gt. 4 ) go to 3769
  if ( nexmod .ne. 4 ) go to 1742
  nexmod = 0
  go to nextsn
1742 hmax = 0.0
  hmin = 0.0
  ihs = 3
  tmult = 1.0
  maxev = locint ( bx(1) )     -     locint ( ev(1) )
  maxew = locint ( finfin )    -     locint ( ew(1) )
  maxew = maxew - 50
  do j=1, 6
1751 sext(j) = blan80
  end do
  do i=1, 20
     aaa(i) = 1.0
     bbb(i) = 0.0
     mmm(i) = 0
     ylevel(i) = 0.0
1765 slot1(i) = '        '
  end do
563 do j=1, 20
564  mcurve(j) = mline
  end do
1000 write (prom80, 5000)
5000 format (  ' --- outer :'  )
  assign 5003 to nextsn
  go to 9800
5003 buffin = buff77
  kill = 0
  if ( buffin(1:8)  .ne.  purge )   go to 2716
  close ( unit=14,  status='delete' )
  go to 1000
2716 if ( buffin(1:8)  .ne.  setdat  )   go to 2757
  call setrtm
  go to 563
2757 if ( buffin(1:8)  .ne.  debug )   go to 7368
  write (prom80, 7358)  iprspy
7358 format (  '  supply level-number  iprspy  (', i3,  ' ) :'  )
  assign 7361 to nextsn
  go to 9800
7361 call frein1 ( buff77, iprspy )
  go to 1000
7368 if ( iprspy .lt. 1 )  go to 3877
  write (munit6, 3876)  buffin
3876 format ( '  buffin vector =',  a80  )
  call window
3877 if( buffin(1:8) .eq.  stop ) go to 3818
  if ( buffin(1:8)  .ne.  tek )   go to 5136
  if ( ltek .eq. 1 )  go to 7387
  write (munit6, 7385)
7385 format ( '  ---- switch from character to vector-', 'graphic plotting.'  )
  call window
  go to 7388
7387 write (munit6, 7386)
7386 format ( '  ---- switch from vector-graphic to', ' character plotting'  )
  call window
7388 ltek = ltek + 1
  if ( ltek  .eq.  2 )   ltek = 0
  go to 1000
5136 if ( buffin(1:8)  .ne.  column )   go to 5143
  n1 = limcol
  if ( n1  .ne.  131 )   limcol = 131
  if ( n1  .ne.  79)   limcol =  79
  write (munit6, 5139)  n1
5139 format (  '   character-plot column width was',  i5, '  columns.'   )
  call window
  write (munit6, 5140)  limcol
5140 format (  '   it is now being toggled to',  i5,  ' .'  )
  call window
  go to 1000
5143 if ( buffin(1:8)  .ne.  setcol )   go to 5154
  write (prom80, 5146)  limcol
5146 format (  '  supply printer-plot column width (',  i4, ' ) :'  )
  assign 5147 to nextsn
  go to 9800
5147 call frein1 ( buff77, limcol )
  write (munit6, 5148)  limcol
5148 format (  '   confirmation.   new value =',  i4,  ' .'  )
  call window
  go to 1000
5154 if ( buffin(1:8)  .eq.  inner )   go to 3769
  if ( buffin(1:4) .eq. 'stop' )  return
  if ( buffin(1:5)  .ne.  help(1:5) )   go to 5200
  call helper ( 1 )
  go to 1000
5200 newfil = 0
  tstep = -1.0
  rewind l4plot
  read (l4plot)  datepl, tclopl, numnam, numnvo, numbco, numbrn,  ( buslst(i), i=1, numnam )
  if ( iprspy .lt. 2 )  go to 5739
  write (munit6, 1001)  datepl, tclopl, numnam, numnvo, numbco, numbrn
1001 format ( 23h plot-file header info.,  2(1x, 2a4), 4i6 )
  call window
  do j=1, numnam, 10
     write (munit6, 5733)  ( buslst(k), k=j, j+9 )
5733 format ( 1x, 10a7 )
     call window
5736 end do
5739 kptplt = numnvo + numbrn
  if ( numnvo  .gt.  0 ) read (l4plot)  ( ibsout(j), j=1, numnvo )
  if ( numbrn  .gt.  0 )   read (l4plot) (ibrnch(j), j=1, numbrn), (jbrnch(j), j=1, numbrn)
  if ( iprspy .lt. 2 ) go to 5754
  do j=1, numnvo, 10
     write (munit6, 1002)  ( ibsout(k), k=j, j+9 )
1002 format (  ' node numbers ibsout:',  10i5  )
     call window
5742 end do
  do j=1, numbrn, 5
     write (munit6, 5744)  (ibrnch(k), jbrnch(k), k=j, j+4)
5744 format ( ' branch node pairs:',  5( 2x, 2i4)  )
     call window
5747 end do
5754 numout = numnvo + 2 * numbrn
  nv = numbrn - numbco
  jbegbv = numnvo + 1
  jbegbc = jbegbv + 2 * nv
  nt2 = numbrn * 2
  i = 1
1008 if ( i  .gt.  numnvo )   go to 1009
  j = ibsout(i)
  bbus(i) = buslst(j)
  i = i + 1
  go to 1008
1009 j = 1
1012 if ( j  .gt.  numbrn )   go to 1013
  k = ibrnch(j)
  l = jbrnch(j)
  bbus(i) = buslst(k)
  i = i + 1
  bbus(i) = buslst(l)
  i = i + 1
  j = j + 1
  go to 1012
1013 if ( iprspy .lt. 2 ) go to 3769
  write (munit6, 1014)  numout, numnvo, nv, nt2, i
1014 format ( ' numout, numnvo, nv, nt2, i =',  5i6 )
  call window
  do j=1, numout, 10
     write (munit6, 5763) ( bbus(m), m=j, j+9 )
5763 format ( 1x, 10a6 )
     call window
5766 end do
3769 call pltvar
  if ( nexmod .gt. 0 ) go to 9835
  if ( buffin(1:4) .eq. 'stop' )  return
  !     if ( buffin(1:4) .eq. 'spy ' )  return  ! abort "plot" use
  if ( kill  .eq.  0 )   go to 1000
3818 close ( unit=14,  status='keep' )
  !     now stop.   if there were tektronix crt plots, call for
  !     buffer flushing in  #tekplt# .
  if ( numtek  .gt.  0 )   call tekplt
9800 if ( iprspy .lt. 1 ) go to 9817
  write (munit6, 9807)   prom80(1:40)
9807 format (  ' exit "tpplot".   prom80(1:40) =',  a40  )
  call window
9817 nexmod = 4
9835 return
end subroutine tpplot
!
!     subroutine helper.
!
subroutine helper ( n1 )
  !     module used only for interactive emtp (service to "emtspy").
  !     for non-interactive emtp, this module can be destroyed.
  !     this module does nothing other than service the "help"
  !     subcommand of the "plot" command of spy.
  include 'dekspy.ftn'
  include 'dekplt.ftn'
  n8 = n1
  if ( buffin(5:10) .ne. '      ' )  go to 3618
3613 k = numkey + n8 + 1
  n23 = kbegtx(k)
  n24 = kbegtx(k+1) - 1
  go to 3673
3618 if ( buffin(6:10) .ne. 'outer' ) go to 3622
  n8 = 1
  go to 3613
3622 if ( buffin(6:11) .ne. 'middle' )  go to 3627
  n8 = 2
  go to 3613
3627 if ( buffin(6:10) .ne. 'inner' )  go to 3634
  n8 = 3
  go to 3613
3634 n23 = kbegtx(numkey+1)
  n24 = kbegtx(numkey+5) - 1
  if ( buffin(6:8) .eq. 'all' )  go to 3673
  do j=n23, n24
     if ( texspy(j)(1:3) .eq. '   ' )  go to 3642
     if ( texspy(j)(3:10) .eq. buffin(6:13) )  go to 3649
3642 end do
  write (munit6, 3645)
3645 format ( '    ? ? ?   sorry, no such plot command.', '   try again ... '  )
  call window
  go to 9000
3649 n23 = j
  do j=n23+1, n24
     if ( texspy(j)(1:3) .ne. '   ' )  go to 3658
3653 end do
3658 n24 = j - 1
3673 do j=n23, n24
     write (munit6, 3677)  texspy(j)
3677 format ( a80 )
     call window
3681 end do
9000 if ( iprspy .lt. 1 ) go to 9011
  write (munit6, 9004)  n1, n8, n23, n24
9004 format ( ' exit  "helper".  n1, n8, n23, n24 =',  4i6 )
  call window
9011 return
end subroutine helper
!
!     subroutine pltvar.
!
subroutine pltvar
  !     module used only for interactive emtp (service to "emtspy").
  !     for non-interactive emtp, this module can be destroyed.
  include 'dekspy.ftn'
  include 'dekplt.ftn'
  if ( iprspy .lt. 1 ) go to 1003
  write (munit6, 1002)  nexmod
1002 format ( ' enter "pltvar".  nexmod =',  i4 )
  call window
1003 if ( nexmod .eq. 6 ) go to 1550
  if ( nexmod .ne. 5 )  go to 1005
  nexmod = 0
  go to nextsn
1005 tolrce = 0.0
  go to 1008
1053 write(prom80,1045)
1045 format (  ' --- middle :'  )
  assign 1048 to nextsn
  go to 9800
1048 buffin = buff77
  if ( buffin(1:4) .eq. 'stop' )  return
  !     if ( buffin(1:4) .eq. 'spy ' )  return  ! abort "plot" use
  if ( buffin(1:8)  .ne.  refile )   go to 1007
  write (prom80, 1004)
1004 format (  ' file :' )
  assign 1052 to nextsn
  go to 9800
1052 buffin = buff77
  newfil = 1
  go to 1053
1007 if ( buffin(1:8)  .eq.  inner )   go to 1550
  if ( buffin(1:8)  .ne.  timesp )   go to 5681
1008 timbeg = pltbuf(newvec)
  n4 = (indbuf - newvec ) / (kptplt + 1 )
  if ( iprspy .lt. 1 ) go to 5673
  write (munit6, 5672)  indbuf, newvec, kptplt, n4
5672 format (' timespan calc.  indbuf, newvec, kptplt, n4 =', 4i8)
  call window
5673 n4 = newvec + n4 * (kptplt + 1)
  timend = pltbuf(n4)
  if ( iprspy .lt. 1 ) go to 35675
  write (munit6, 5675)  newvec, n4, timbeg, timend
5675 format ( ' pltbuf cells', 2i8,    '     tmin, tmax', ' (sec) =',  2e14.5  )
  call window
35675 if ( iprspy .lt. 5 )  go to 3218
  do m=newvec, n4, 8
     write (munit6, 5676) j, (pltbuf(ip), ip=j, j+7)
5676 format ( ' pltbuf(',  i6,  2h:),  8e14.5  )
     call window
3214 end do
3218 write (munit6, 5677)  timbeg, timend
5677 format ( '   time limits are :',  2e14.6  )
  call window
  if ( ihs  .eq.  0 )   ihs = 3
  go to 7531
5681 if (  buffin(1:8)  .ne.  choice )   go to 1957
  call spylin
  i = 1
  nc = 2 * nv
  write (munit6, 1041)
1041 format ( 32h type-1 entries (node voltages).   )
  call window
  do j=1, numnvo, 10
     n13 = j+9
     if ( n13 .gt. numnvo )  n13 = numnvo
     write (munit6, 3223) ( bbus(ip), ip=j, n13 )
3223 format ( 1x, 10a7 )
     call window
3227 end do
  i = i + numnvo
  jj = i + nc - 1
  write (munit6, 1047)
1047 format ( 44h type-8 entries (branch voltages or powers).  )
  call window
  do j=i, jj, 8
     n13 = j+7
     if ( n13 .gt. jj )  n13 = jj
     write (munit6, 3232) ( bbus(ip), ip=j, n13 )
3232 format ( 1x,  4(2a7, 3x)  )
     call window
3236 end do
  i = i + nc
  write (munit6, 1051)
1051 format ( 46h type-9 entries (branch currents or energies).  )
  call window
  do j=i, numout, 8
     n13 = j+7
     if ( n13 .gt. numout )  n13 = numout
     write (munit6, 3232) ( bbus(ip), ip=j, n13 )
     call window
3247 end do
  go to 1053
1957 if ( buffin(1:8)  .ne.  stop )   go to 4423
  kill = 99
  return
4423 if ( buffin(1:8) .ne. purge ) go to 1958
  close ( unit=14,  status='delete' )
  return
1958 if ( buffin(1:8) .ne. out )go to 1931
  return
1931 if ( buffin(1:5) .ne. help(1:5) ) go to 1059
  call helper ( 2 )
  go to 1053
1059 if ( buffin(1:8)  .eq.  label    )   go to 1210
  if ( buffin(1:8)  .ne.  timeun )   go to 1167
1155 write (prom80, 1164)  ihs
1164 format (  '   send time-units code (',  i2,  ' ) :'  )
  assign 1165 to nextsn
  go to 9800
1165 call frein1 ( buff77, n4 )
  if ( n4  .gt.  0 )   ihs = n4
  if ( ihs  .gt.  0     .and.     ihs  .lt.  8 ) go to 7531
  write (munit6, 4163)
4163 format (  '  ? ? ?  illegal value.  try again ...' )
  call window
  go to 1155
7531 if ( ihs  .eq.  1 )   tmult = 21600.
  if ( ihs  .eq.  2 )   tmult = 60.
  if ( ihs  .eq.  3 )   tmult = 1.0
  if ( ihs  .eq.  4 )   tmult = 1000.
  if ( ihs  .eq.  5 )   tmult = 1.e6
  if ( ihs  .eq.  6 )   tmult = 1.0
  if ( ihs  .eq.  7 )   tmult = 1.0
  go to 1053
1167 l = 1
  icp = 4
  jplt = 0
7338 write (prom80, 7341)  slot1(l)
7341 format (  '   send node name or end (',  a6, ') :'   )
  assign 7342 to nextsn
  go to 9800
7342 textd1 = buff77(1:8)
  if ( textd1  .eq.  lastpl )   go to 1175
7344 if ( textd1  .ne.  end  )   go to 7348
  icp = 8
  brclas = voltag
  go to 1168
7348 if ( textd1  .eq.  '        ' )   textd1 = slot1(l)
  if ( textd1  .eq.  back  )   go to 1053
  if ( l  .eq.  1     .and.     jplt  .gt.  0 .and.   textd1  .eq.  repeat  )   go to 1210
  k = 0
1600 k=k+1
  if ( k  .le.  numnvo )   go to 1640
  write (munit6,1620) textd1
1620 format ( ' ??? list of node voltages does not include  "', a6,  '" .   try again ....'  )
  call window
  go to 7338
1640 write (ansi, 1641)  bbus(k)
1641 format ( a6, 2x )
  if ( textd1 .ne. ansi )  go to 1600
  jplt = l
  l = l + 1
  mplot(jplt)=k
  slot1(jplt) = textd1
  go to 7338
1168 write (prom80, 1169)  brclas,  slot1(l), slot1(l+1)
1169 format (  '   send branch ',  a7,  ' names or end (', a6,  1h,,  a6,  ' ) :'   )
  assign 31169 to nextsn
  go to 9800
31169 textd1 = buff77(1:6)
  textd2 = buff77(7:12)
  if ( textd1  .eq.  back  )   go to 1053
  if ( textd1  .eq.  end  )   go to 1175
  if ( textd1  .eq.  lastpl )   go to 1175
  if ( textd1  .eq.  '        '     .and. textd2  .eq.  '        '  )   go to 1173
  slot1(l) = textd1
  slot1(l+1) = textd2
1173 if ( slot1(l)  .eq.  '        '     .and. slot1(l+1)  .eq.  '        '  )   go to 1168
  if ( icp  .eq.  9 )   go to 1720
  ib = jbegbv
  il = jbegbc
  n1 = ib
  go to 1740
1720 ib=jbegbc
  il=numout
  n1 = jbegbv + nv
1740 if (ib.lt.il) go to 1780
  write (munit6, 1760)  brclas, slot1(l), slot1(l+1)
1760 format (  ' ??? branch ',  a7,  ' list does not include', ' an entry from  "',  a6,  '"  to  "',  a6, &
       ' " .   try again ....'  )
  call window
  go to 1168
1780 write (ansi, 1641)  bbus(ib)
  if ( slot1(l) .ne. ansi )   go to 1800
  write (ansi, 1641)  bbus(ib+1)
  if ( slot1(l+1) .ne. ansi )   go to 1820
  !               node pair found - sign correct
  mplot(jplt+1)= n1
  go to 1840
1800 write (ansi, 1641)  bbus(ib)
  if ( slot1(l+1) .ne. ansi )   go to 1820
  write (ansi, 1641)  bbus(ib+1)
  if ( slot1(l) .ne. ansi )   go to 1820
  !               node pair found - sign negative
  mplot(jplt+1)=-n1
  go to 1840
1820 ib=ib+2
  n1 = n1 + 1
  go to 1740
1840 jplt=jplt+1
  l = l + 2
  go to 1168
1175 namvar = l - 1
  if ( iprspy .lt. 1 ) go to 1218
  write (munit6, 1217)  jplt, icp, namvar
1217 format ( ' done class.    jplt, icp, namvar =',  3i8 )
  call window
1218 if ( iprspy .lt. 1 )  go to 3252
  n14 = jplt
  if ( n14 .gt. 20 )  n14 = 20
  write (munit6, 3251) ( mplot(i), i=1, n14 )
3251 format ( ' mplot:', 20i6 )
  call window
3252 if ( textd1  .ne.  lastpl     .and. icp     .lt.   9   )   go to 1204
  if ( jplt  .gt.  0 )   go to 1210
  write (munit6, 1202)
1202 format (  '  ???  error.   no valid plot variables of', ' any type were specified.   try again ....' )
  call window
  go to 1167
1204 icp = 9
  brclas = curren
  go to 1168
1210 nc = namvar + 1
  do i=nc, 20
1215 slot1(i) = '        '
  end do
  ievsw=0
  nc = nt2 / 2  -  nv
  if ( iprspy .lt. 1 ) go to 1880
  write (munit6, 4217)  kptplt, numnvo, numbrn
4217 format ( ' assign kptplt.  kptplt, numnvo, numbrn =', 3i10 )
  call window
1880 jplt1=jplt+1
  kplt=0
  write (prom80, 1882)  headl(1:16)
1882 format (  '   send super-title (',  a16,  '...) :'   )
  assign 31882 to nextsn
  go to 9800
31882 alpha = buff77
  if ( alpha(1:8)  .eq.  repeat  )   go to 1550
  if ( alpha(1:8)  .eq.  texblk )   go to 1536
  do j=1, 80
     n3 = 81 - j
     if ( alpha(n3:n3)  .eq.  ' ' )   go to 1535
     headl = alpha
     nchsup = n3
     go to 1885
1535 end do
1536 nchsup = 0
1885 write (prom80, 1886)  vertl(1:16)
1886 format (  '   send vertical axis label (', a16,  '...) :'    )
  assign 1887 to nextsn
  go to 9800
1887 vertl = buff77
  if ( vertl(1:8)  .eq.  repeat )   go to 1550
  if ( vertl(1:8)  .eq.  texblk )  go to 1545
  do j=1, 78
     n3 = 81 - j
     if ( vertl(n3:n3) .eq. ' ' )  go to 1544
     nchver = n3
     go to 1888
1544 end do
1545 nchver = 0
1888 n3 = numtit  +  1
  write (prom80, 1891)  n3,  sext(n3)(1:16)
1891 format (  '   send case-title line',  i2,  ' (',  a16, '...) :'   )
  assign 1892 to nextsn
  go to 9800
1892 alpha = buff77
  if ( alpha(1:8)  .ne.  flush  )   go to 1894
  numtit = 0
  go to 1888
1894 if ( alpha(1:8)  .ne.  playba  )   go to 1896
  write (munit6, 1895)
1895 format ( 6x, 'playback of total title (80a1 format) ...' )
  call window
  do ip=1, numtit
     write (munit6, 3264)  sext(ip)
3264 format ( 1x, a80 )
     call window
3267 end do
  go to 1888
1896 if ( alpha(1:8)  .eq.  '        ' )   go to 1899
  if ( alpha(1:8)  .eq.  end  )   go to 1550
  if ( alpha(1:8)  .ne.  texblk )   go to 1897
  alpha(1:8) = '        '
1897 sext(n3) = alpha
1899 numtit = n3
  if ( numtit .lt. 5 )  go to 1901
  write (munit6, 1547)
1547 format (  ' ** warning.  title storage is now full.', '   no additional lines can be accepted.'  )
  call window
1901 go to 1888
1550 call timval
  if ( nexmod .gt. 0 ) go to 9835
  if ( kill  .eq.  99 )  go to 9835
  if ( buffin(1:4) .eq. 'stop' ) go to 9835
  !     if ( buffin(1:4) .eq. 'spy ' ) go to 9835 ! abort "plot" use
  go to 1053
9800 if ( iprspy .lt. 1 ) go to 9817
  write (munit6, 9807)   prom80(1:40)
9807 format (  ' exit "pltvar".   prom80(1:40) =',  a40  )
  call window
9817 nexmod = 5
9835 return
end subroutine pltvar
!
!     subroutine timval.
!
subroutine timval
  !     Module used only for interactive EMTP (service to "emtspy").
  !     for non-interactive EMTP, this module can be destroyed.
  include 'dekspy.ftn'
  include 'dekplt.ftn'
  real*8 d1, d3, tolrce, vmin, vmax, din1, din2
  maxevk = maxev - 30
  if ( iprspy .lt. 1 ) go to 1549
  write (munit6, 1386)  monitr, limfix, vmin, vmax
1386 format (  ' begin  "timval".  monitr, limfix =',  2i6, '    vmin, vmax =',  2e13.4  )
  call window
  n16 = jplt
  if ( n16 .gt. 20 )  n16 = 20
  write (munit6, 3274) ( mplot(i), i=1, n16 )
3274 format ( ' mplot:',  20i6 )
  call window
1549 if ( nexmod .ne. 6 ) go to 31549
  nexmod = 0
  go to nextsn
31549 if ( monitr .eq. 8765 ) go to 1926
1550 write(prom80,1930)
1930 format (  ' --- inner :'  )
  assign 1556 to nextsn
  go to 9800
1556 buffin = buff77
  !     if ( buffin(1:4) .eq. 'spy ' ) go to 9835   ! exit module
  if ( buffin(1:8)  .ne.  setdat )   go to 2713
  call setrtm
  go to 1550
2713 if ( buffin(1:8)  .ne.  'rollv   '     )   go to 2715
  monitr = monitr + 1
  if ( monitr  .ge.  2 )   monitr = 0
  if ( monitr .ne. 0 )  write (munit6, 42713)
42713 format (  '            ===  begin  rolling  of', ' vector-graphic  plot.'  )
  if ( monitr .eq. 0 )  write (munit6, 2714)
2714 format (  '            ===  previous  rolling  of', ' vector plot is hereby cancelled.  ==='  )
  call window
  go to 1550
2715 if ( buffin(1:8) .ne. 'rollc   ' )  go to 42715
  monits = monits + 1
  if ( monits .ge. 2 )  monits = 0
  if ( monits .ne. 0 )  go to 1550
  write (munit6, 22714)
22714 format (  '            ===  previous  rolling  of', ' character plot is hereby cancelled.  ==='  )
  call window
  go to 1550
42715 if ( buffin(1:8)  .ne.  'span    '     )   go to 2718
  write (prom80, 2716)
2716 format ( ' send desired tmax-tmin:'  )
  assign 52716 to nextsn
  go to 9800
52716 call fresp1(buff77, d3)
2717 n4 = (indbuf - newvec ) / (kptplt + 1 )
  if ( iprspy .lt. 1 ) go to 82717
  write (munit6, 42717)  indbuf, newvec, kptplt, n4
42717 format (   ' timespan calculation.', '  indbuf, newvec, kptplt, n4=',   4i8  )
  call window
82717 n4 = newvec + n4 * (kptplt + 1)
  din2 = pltbuf(n4)
  din1 = din2 - d3
  go to 1927
2718 if ( buffin(1:8)  .ne.  'front   '    )   go to 2720
  d3 = spsave
  go to 2717
2720 if ( buffin(1:8)  .ne.  xyplot )   go to 2728
  mxypl = mxypl + 1
  if ( mxypl  .ge.  2 )   mxypl = 0
  if ( mxypl  .eq.  0 )   go to 2724
  write (prom80, 2721)  xytitl(1:16)
2721 format (  ' send x-axis label (',  a16,  '...) :'  )
  assign 42721 to nextsn
  go to 9800
42721 buffin = buff77
  if ( buffin(1:8)  .eq.  '        ' )   go to 2724
  if ( buffin(1:8)  .eq.  texblk )   go to 2722
  xytitl(1:24) = buffin(1:24)
  go to 2724
2722 xytitl = '                        '
2724 go to 1550
2728 if ( buffin(1:8)  .ne.  cursor )   go to 2733
  icurse = icurse + 1
  if ( icurse  .eq.  2 )   icurse = 0
  go to 1550
2733 if ( buffin(1:8)  .ne.  debug )   go to 2742
  write (prom80, 2738)  iprspy
2738 format (  '  supply level-number  iprspy  (', i3,  ' ) :'  )
  assign 2740 to nextsn
  go to 9800
2740 call frein1 ( buff77, iprspy )
  go to 1550
2742 if ( buffin(1:8)  .ne.  extrem )   go to 4774
  kextr = kextr + 1
  if ( kextr  .gt.  1 )   kextr = 0
  go to 1550
4774 if ( buffin(1:8)  .ne.  level )   go to 4781
  klevl = klevl + 1
  if ( klevl  .gt.  1 )   klevl = 0
  if ( klevl  .eq.  0 )   go to 4777
  write (munit6, 4776)  ( ylevel(j), j=1, jplt )
4776 format (  '   input vector of levels :'  )
  call window
  write (munit6, 3284)  ( ylevel(j), j=1, jplt )
3284 format ( 1x, 7e10.2 )
  call window
  if ( ltek .le. 0 )  go to 34776
  write (munit6, 3303)
  call window
34776 read (munit5, *)  ( ylevel(j), j=1, jplt )
4777 go to 1550
4781 if ( buffin(1:8)  .ne.  stop )   go to 4784
  kill = 99
  go to 9835
4784 if ( buffin(1:8)  .ne.  smooth )   go to 4805
  write (prom80, 4791)  tolrce
4791 format (  ' supply new tolerance in', ' inches**2 (',  e9.2,  ' ) :'  )
  assign 4794 to nextsn
  go to 9800
4794 call fresp1(buff77, tolrce)
  go to 1550
4805 if ( buffin(1:8)  .ne.  size )   go to 4823
  write (prom80, 4808)  taxisl
4808 format (  ' supply new time-axis length', ' in inches (',  f5.1,  ' ) :'  )
  assign 4811 to nextsn
  go to 9800
4811 call fresp1 (buff77, d1)
  if ( d1  .gt.  2.0 )   taxisl = d1
  go to 1550
4823 if ( buffin(1:8)  .ne.  show )   go to 4841
  n1 = limcol + 1
  write (munit6, 4831)
4831 format (  ' parameter settings follow ....'  )
  call window
  write (munit6, 2291)  n1
2291 format (  i15,  ' = number of columns in character plot' )
  call window
  write (munit6, 2293)  iprspy
2293 format (  i15,  ' = diagnostic (debug) printout level' )
  call window
  write (munit6, 2295)  taxisl
2295 format (  e15.3,  ' = length of time axis in inches' )
  call window
  write (munit6, 2297)  tolrce
2297 format (  e15.3,  ' = smoothing tolerance in inches' )
  call window
  write (munit6, 2299)  linepr
2299 format ( i15, ' = logical unit number of line printer.' )
  call window
  write (munit6, 2303)  lu7plt
2303 format ( i15,  ' = logical unit number of current output' )
  call window
  write (munit6, 2305)  ltek
2305 format ( i15,  ' = ltek (1 if vector plot, 0 otherwise)' )
  call window
  write (munit6, 2307)  ihs
2307 format ( i15,  ' = time units code ihs'   )
  call window
  write (munit6, 2309)  ibaud
2309 format ( i15,  ' = ibaud (tektronix plot10 characters/sec)' )
  call window
  write (munit6, 2311)  nxmax
2311 format ( i15,  ' = nxmax (vector points horizontally)' )
  call window
  go to 1550
4841 if ( buffin(1:5) .ne. help(1:5) ) go to 1990
  call helper ( 3 )
  go to 1550
1990 if ( iprspy .lt. 1 )  go to 8992
  write (munit6, 8991)  kptplt, numnvo, numbrn
8991 format (  ' kptplt, numnvo, numbrn below 1990 =', 3i8 )
  call window
8992 if ( buffin(1:8)  .ne.  out  )go to 1921
  go to 9835
1921 if ( buffin(1:8)  .ne.  multip )   go to 1960
  write (munit6, 1954)
1954 format (  '   input vector of multipliers :'  )
  call window
  write (munit6, 3317)  ( aaa(j), j=1, jplt )
3317 format ( 1x,  7e10.2 )
  call window
  if ( ltek .le. 0 )  go to 1957
  write (munit6, 1956)
1956 format ( 1x )
  call window
1957 assign 1958 to nextsn
  go to 9800
1958 read (buff77, 1959)  ( aaa(j), j=1, jplt )
1959 format ( 10e8.0 )
  go to 1550
1960 if ( buffin(1:8)  .ne.  offset )   go to 1970
  write (munit6, 1964)
1964 format (  '   input vector of offsets :'  )
  call window
  write (munit6, 3317)  ( bbb(j), j=1, jplt )
  call window
  if ( ltek .le. 0 )  go to 1969
  write (munit6, 1956)
  call window
1969 assign 31969 to nextsn
  go to 9800
31969 read (buff77, 1959)  ( bbb(j), j=1, jplt )
  go to 1550
1970 if ( buffin(1:8)  .ne.  limits )   go to 1980
  write (prom80, 1973)  vmin, vmax
1973 format (  '   vertical min & max (', 2e12.3,  ' ) :'   )
  assign 1975 to nextsn
  go to 9800
1975 call fresp2(buff77, vmin, vmax)
  limfix = 1
  if ( vmin  .eq.  0.0     .and. vmax  .eq.  0.0 )   limfix = 0
  go to 1550
1980 if ( buffin(1:8)  .ne.  averag )   go to 1989
  write (prom80, 1982)  nsmplt
1982 format (  '   supply consecutive-oscillation limit (', i6,    ' ) :'   )
  assign 1985 to nextsn
  go to 9800
1985 call frein1 ( buff77, n1 )
  if ( n1  .gt.  0 )   nsmplt = n1
  go to 1550
1989 if ( buffin(1:8)  .ne.  rescal )   go to 1992
  limfix = 0
  do j=1, 20
     aaa(j) = 1.0
1991 bbb(j) = 0.0
  end do
  go to 1550
1992 if ( buffin(1:8)  .ne.  time )   go to 5618
  din1 = hmin * tmult
  din2 = hmax * tmult
  write (prom80, 1925)  din1, din2
1925 format (  '   supply tmin & tmax (', 2e12.3,  ' ) :'   )
  assign 31925 to nextsn
  go to 9800
31925 call fresp2(buff77, din1, din2)
  if ( din1  .ne.  0.0 )   go to 1927
  if ( din2  .ne.  0.0 )   go to 1927
  go to 1928
  !     auto internal "time", called from "pltfil" if rolling plot needs
  !     a new page:
1926 d6 = din2 - din1
  d7 = inchlp
  din1 = din2 - (d7/100.) * d6
  din2 = din1 + d6
  if ( iprspy .le. 0 )  go to 1927
  write (munit6, 41927)  limfix, inchlp, vmin, vmax, din1, din2
41927 format ( ' "timval", s.n. 1926.  limfix, inchlp, vmin,', ' vmax, din1, din2 =', 2i8,  4e13.4  )
  call window
1927 hmin = din1
  hmax = din2
  spsave = din2 - din1
  hmin=hmin/tmult
  hmax=hmax/tmult
  hpi = (hmax - hmin ) / taxisl
1928 if ( iprspy  .lt.  1 )  go to 1942
  write (munit6, 1934)  kplt, jplt
1934 format (  ' b4 read points.  kplt, jplt =',  2i8 )
  call window
  write (munit6, 3324)  hmin, hmax, hpi, taxisl, tmult
3324 format (  '         hmin, hmax, hpi, taxisl, tmult =', 5e15.6  )
  call window
1942 if ( monitr .ne. 2345 )  go to 5648
  go to 1550
5618 if ( buffin(1:8)  .ne.  '        '     .and. buffin(1:8)  .ne.  inner )   go to 4431
  if ( hmax  .gt.  hmin )   go to 5648
  go to 4437
4431 if ( buffin(1:8)  .ne.  alltim )   go to 5623
4437 hmin = timbeg
  hmax = timend
  hpi = ( hmax - hmin ) / taxisl
  go to 5648
5623 write (munit6, 5624)
5624 format (  ' ??? unrecognizable data.  try again ....'  )
  call window
  go to 1550
5648 kplt = 0
  if ( iprspy .lt. 1 ) go to 75648
  write (munit6, 35648)   mplot
35648 format ( ' begin points extraction.  mplot =', 20i4 )
  call window
75648 if ( limfix  .eq.  1 )   go to 5647
  vmin=-finfin
  vmax=finfin
5647 vmaxr=vmin+(vmax-vmin)*(vh-vs)/(vl-vs)
  vminr=vmin-(vmax-vmin)*vs/(vl-vs)
  mfake = 0
  do j=1, jplt
     mmm(j) = 1
     if ( aaa(j)  .eq.  0.0 )   go to 5646
     if ( aaa(j)  .eq.  1.0 )   go to 5646
     go to 5645
5646 if ( bbb(j)  .ne.  0.0 )   go to 5645
  end do
  mmm(j) = 0
  mfake = mfake + 1
5645 continue
5649 indexp = newvec - (kptplt + 1)
  if ( iprspy .lt. 1 ) go to 1940
  write (munit6, 45649)   newvec, kptplt
45649 format ( ' reinitialize pointer:  newvec, kptplt =', 2i9 )
  call window
1940 indexp = indexp + kptplt + 1
  if ( iprspy .lt. 3 ) go to 81940
  write (munit6, 61940)   indexp
61940 format ( ' indexp at the time test =',  i9 )
  call window
81940 if ( indexp  .ge.  indbuf )   go to 2200
  tstep = pltbuf(indexp)
  if (tstep.ge.hmin) go to 2060
  go to 1940
2060 if (tstep.gt.hmax) go to 2205
  if ( indexp  .gt.  indbuf )   go to 2200
  if (kplt.le.maxevk) go to 2100
  write (munit6, 2068)  tstep
2068 format ( 2x,  17hplot truncated to,  e11.3, 32h seconds due to filled storage.  )
  call window
  go to 2205
2100 kplt=kplt+1
  ev(kplt)=tstep
  do j=1,jplt
     j1=mplot(j)
     if (j1.ge.0) go to 2120
     j1=-j1
     bx(j1)=-bx(j1)
2120 if ( mxypl  .eq.  0 )   go to 2126
     if ( j  .eq.  1 )   go to 2127
2126 kplt=kplt+1
2127 ev(kplt) = pltbuf(indexp+j1)
     if ( mplot(j)  .lt.  0 )   ev(kplt) = -ev(kplt)
     if ( mmm(j)  .eq.  0 )   go to 2140
     if ( aaa(j)  .ne.  0.0 ) ev(kplt) = ev(kplt) * aaa(j)
     ev(kplt) = ev(kplt) + bbb(j)
2140 end do
  go to 1940
2200 tstep = 1.e+30
2205 if ( iprspy .lt. 4 ) go to 3342
  do j=1, kplt, 8
     write (munit6, 3336) j, ( ev(ip), ip=j, j+7 )
3336 format ( ' ev(', i5,  3h:)=,  8e15.6  )
     call window
3338 end do
3342 ind1 = indexp - ( kptplt + 1 )
  if ( iprspy .lt. 1 )  go to 3343
  write (munit6, 82205)  ind1
82205 format ( ' another ind1 change.  ind1 =',  i8  )
  call window
3343 if ( mxypl  .eq.  0 )   go to 2218
  k = 0
  n7 = 0
  n8 = 2 * kplt / jplt  +  4
  gxmin = finfin
  gymin = finfin
  gxmax = -gxmin
  gymax = -gymin
8303 if ( k  .ge.  kplt )   go to 8317
  kk = n7
  do j=1, jplt, 2
     if ( ev(k+1)  .lt.  gxmin )   gxmin = ev(k+1)
     if ( ev(k+1)  .gt.  gxmax )   gxmax = ev(k+1)
     if ( ev(k+2)  .lt.  gymin )   gymin = ev(k+2)
     if ( ev(k+2)  .gt.  gymax )   gymax = ev(k+2)
     ew(kk+1) = ev(k+1)
     ew(kk+2) = ev(k+2)
     k = k + 2
8310 kk = kk + n8
  end do
  n7 = n7 + 2
  go to 8303
8317 write (munit6, 8322)  gxmin, gxmax, gymin, gymax
8322 format (  '  x-min, y-min =',  2e12.4, '  y-min, y-max =', 2e12.4  )
  call window
  write (prom80, 3356)
3356 format (  ' send axis limits :' )
  assign 43356 to nextsn
  go to 9800
43356 read (buff77, 1959)  d1, d2, d3, d4
  if ( d1  .ne.  0.0 )   gxmin = d1
  if ( d2  .ne.  0.0 )   gxmax = d2
  if ( d3  .ne.  0.0 )   gymin = d3
  if ( d4  .ne.  0.0 )   gymax = d4
  vmin = gymin
  vmax = gymax
  mfake = mfake / 2
  l = 0
  kk = n8
8371 do j=1, jplt, 2
     l = l + 1
     mstart(l) = kk - 4
     ew(kk-3) = gxmin
     ew(kk-2) = gymin
     ew(kk-1) = ( gxmax - gxmin ) / taxisl
     ew(kk)   = ( gymax - gymin ) / 8.0
8376 kk = kk + n8
  end do
  kk = kk - n8
  jplt = l
  if ( iprspy .lt. 1 )  go to 8382
  write (munit6, 8381)  jplt,  ( mstart(j), j=1, jplt )
8381 format (  ' done with x-y pack.   jplt =',  i3, 5x,  'mstart(j) :',  5i5  )
  call window
8382 if ( iprspy .lt. 9 ) go to 7309
  do j=1, kk, 8
     write (munit6, 3361) j, ( ew(ip), ip=j, j+7 )
3361 format ( ' ew(', i5,  3h:)=,  8e15.6  )
     call window
3363 end do
  go to 7309
2218 dx = hpi
  evmx=0.0
  k9=kplt+jplt1+jplt1
  do j=1,jplt
     evh=0.0
     evdh=0.0
     isw=0
     itimes = 0
     do i=1,kplt,jplt1
        ipj=i+j
        evp=ev(ipj)
        if (isw.gt.0) go to 2240
        evdp=evp-evh
        if (evdp*evdh.ge.0.0) go to 2250
        if (isww.eq.1) go to 2255
        itimes=itimes+1
        if ( itimes .le. nsmplt )   go to 2260
        isw=1
2240    ev(ipj)=(evh+evp)/2.0
        go to 2260
2250    isww=1
        go to 2260
2255    itimes=1
        isww=0
2260    evdh=evdp
        evh=evp
        evmx=amax1(evmx,abs(ev(ipj)))
        if (ev(ipj).lt.vminr) go to 2280
        if (ev(ipj).gt.vmaxr) go to 2300
        go to 2320
2280    ev(ipj)=vminr
        go to 2320
2300    ev(ipj)=vmaxr
2320 end do
2340 end do
  if ( iprspy .lt. 4 ) go to 3369
  write (munit6, 2344)
2344 format (  ' after points discard.', 11x,  4hevmx,  11x,  4hvmax,  11x,  4hvmin, 9x,  6hfinfin,  13x,  2hvl,   13x,  2hvs   )
  call window
  write (munit6, 3366)  evmx, vmax, vmin, finfin, vl, vs
3366 format ( 22x,  6e15.5  )
  call window
3369 if ( limfix  .eq.  1 )   go to 2540
  if (evmx.eq.0.0) evmx=1.0
2357 a=8.0
  evmxf=evmx*0.8
2360 d1=evmxf-a
  if (d1.eq.0.0) go to 2400
  if (d1.gt.0.0) go to 2380
  a=a/10.
  go to 2360
2380 a=a*10.
  d1=evmxf-a
  if (d1.eq.0.0) go to 2400
  if (d1.gt.0.0) go to 2380
  d1=evmxf-0.5*a
  if (d1.eq.0.0) go to 2440
  if (d1.lt.0.0) go to 2420
2400 vmax=a
  go to 2520
2420 d1=evmxf-.25*a
  if (d1.lt.0.0) go to 2460
  if (d1.eq.0.0) go to 2480
2440 vmax=a/2.0
  go to 2520
2460 if (evmxf-a/8.0.le.0.0) go to 2500
2480 vmax=a/4.0
  go to 2520
2500 vmax=a/8.0
2520 vmin=-vmax
  if ( vmax  .ge.  0.99*evmx )   go to 2540
  evmx = evmx * 1.1
  go to 2357
2540 dy=(vmax-vmin)/(vl-vs)
  if ( kplt  .gt.  0 )   go to 2543
  write (munit6, 2542)  hmin, hmax
2542 format (  '   ** sorry.  no data available between times', 2e13.4,  'sec.'  )
  call window
  go to 1550
2543 ipl=0
  kpltq=kplt/jplt1
  istore = 0
  if ( iprspy  .lt. 4 )  go to 3382
  write (munit6, 2548)
2548 format ( ' dump cells 1-20 of ev at s.n. 2548 . . . .' )
  call window
  write (munit6, 3374)  ( ev(i), i=1,  10 )
  call window
  write (munit6, 3374)  ( ev(i), i=11, 20 )
3374 format ( 1x, 10e13.4 )
  call window
3382 do i=1, jplt
     yymin(i) = 1.e30
     yymax(i) = -yymin(i)
     mlevel(i) = 0
     if ( klevl  .eq.  0 )   go to 2567
     mlevel(i) = -1
     ttlev(i) = -9999.
2567 end do
  do i=1,jplt
     istold = istore
     ibase=1
     num=0
     n13 = 1+jplt1+i
     vold = ev(n13)
     istore = istore + 1
3000 evbasx=ev(ibase)
     evbasy=ev(ibase+i)
     ew(istore)=evbasx
     ew(istore+1)=evbasy
     istore=istore+2
     if ( istore  .le.  maxew )   go to 3004
     write (munit6, 3001)
3001 format ( ' sorry, tables have overflowed during smoothing.' )
     call window
     write (munit6, 3392)
3392 format ( ' either try a larger tolerance (to discard more' )
     call window
     write (munit6, 3394)
3394 format ( ' points), or decrease the time span, or expand' )
     call window
     write (munit6, 3396)  maxew
3396 format ( ' array  ew(',  i6,  ')  in deck "dekspy" .'  )
     call window
     go to 1550
3004 if ( kextr  .eq.  0 )   go to 2949
     if ( evbasy  .le.  yymax(i) )   go to 2941
     yymax(i) = evbasy
     ttmax(i) = evbasx
2941 if ( evbasy  .ge.  yymin(i) )   go to 2949
     yymin(i) = evbasy
     ttmin(i) = evbasx
2949 if ( mlevel(i)  .eq.  0 )   go to 3010
     d1 = evbasy - ylevel(i)
     if ( d1*dyold(i)  .gt.  0.0 )   go to 2956
     if ( mlevel(i)  .eq.  -1 )   go to 2954
     d3 = d1 * ( evbasx - ew(istore-4) )
     ttlev(i) = evbasx  -  d3 / ( d1 - dyold(i) )
     mlevel(i) = 0
     go to 3010
2954 mlevel(i) = 1
2956 dyold(i) = d1
3010 hvec=(ev(ibase+jplt1)-evbasx)/dx
     n13 = ibase+jplt1+i
     vvec = ( ev(n13) - evbasy ) / dy
     denom=hvec*hvec+vvec*vvec
     ipontr=ibase+jplt1
3020 ipontr=ipontr+jplt1
     if (ipontr.gt.kplt) go to 3140
     hdif=(ev(ipontr)-evbasx)/dx
     vnew=ev(ipontr+i)
     vdif=(vnew-evbasy)/dy
     disqr=hdif*hdif+vdif*vdif-(hvec*hdif+vvec*vdif)**2/denom
     if (disqr.gt.tolrce) go to 3040
     vchnge=vnew-vold
     vold=vnew
     if(vvec.eq.0.0.and.vchnge.eq.0.0)go to 3020
     if (vvec*vchnge.gt.0.0) go to 3020
3040 vold=vnew
     ibase=ipontr-jplt1
     go to 3000
3140 ipontr=ipontr-jplt1
     ew(istore)=ev(ipontr)
     ew(istore+1)=ev(ipontr+i)
     mstart(i) = istore + 1
     istore = istore + 1
     ew(istore+1) = hmin
     ew(istore+3) = hpi
     ew(istore+2) = vmin
     ew(istore+4) = (vmax - vmin ) / 8.0
     n5 = istore + 4
     if ( iprspy .lt. 2 )  go to 3406
     write (munit6, 3176)
3176 format (  ' store calcomp scaling.', 16h       i  istore,  10x,  '(ew(j), j=istore, n5)'  )
     call window
     write (munit6, 3402)  i, istore, ( ew(kl), kl=istore, n5 )
3402 format (  23x,  2i8,  5e15.6  )
     call window
3406 istore = n5
     if ( iprspy .lt. 1 )  go to 3235
     write (munit6,3234)  i, istore
3234 format (  ' done smoothing next curve.  i, istore =', 2i8 )
     call window
3235 numpts(i) = ( istore - istold ) / 2
3240 end do
3263 if ( kextr  .eq.  1 )   go to 3270
  if ( klevl  .eq.  1 )   go to 3288
  go to 7309
3270 write (munit6, 3271)
3271 format (  ' mplot  name-1  name-2',  7x, 7hminimum,  7x,  7hmaximum,  6x,  8ht of min, 6x,  8ht of max  )
  call window
  l = 1
  do j=1, jplt
     textd1 = slot1(l)
     textd2 = '        '
     l = l + 1
     if ( iabs( mplot(j) )  .le.  numnvo )   go to 3279
     textd2 = slot1(l)
     l = l + 1
     write (munit6, 3282)  mplot(j), textd1, textd2, yymin(j), yymax(j), ttmin(j), ttmax(j)
3282 format ( 1x,  i5,  2( 2x, a6 ),  4e14.5  )
     call window
3279 end do
  if ( klevl  .eq.  0 )   go to 3296
3288 write (munit6, 3294)  ( ylevel(j), j=1, jplt )
3294 format (  ' levels sought :',  4e15.4  )
  call window
  write (munit6, 3295)  ( ttlev(j), j=1, jplt )
3295 format (  ' 1st hit time :',  4e16.6  )
  call window
3296 if ( ltek .le. 0 ) go to 43305
  write (munit6, 3303)
3303 format ( 1h    )
  call window
43305 read (munit5, 3305)  textd1
3305 format ( a8 )
  if ( textd1  .eq.  '        ' )   go to 7309
  if ( textd1  .ne.  linezz )   go to 3306
  mu6std = linepr
  go to 3263
3306 if ( textd1  .eq.  noplot )   go to 1550
7309 if ( ltek  .ne.  0 )   call tekplt
  if ( ltek  .eq.  0 )   call chrplt
  if ( monitr .ne. 8765 )  go to 1550
  !     check if special "pltvar" call for rolling plot paging:
  if ( iprsup .le. 0 )  go to 7324
  write (munit6, 7318)
7318 format ( ' done with "tekplt", monitr = 8765 so', ' exit "timval".'  )
  call window
7324 go to 9835
9800 if ( iprspy .lt. 1 ) go to 9817
  write (munit6, 9807)   prom80(1:40)
9807 format (  ' exit "timval".   prom80(1:40) =',  a40  )
  call window
9817 nexmod = 6
9835 return
end subroutine timval
!
! subroutine back14.
!
subroutine back14
  !     Module used only for interactive EMTP (service to "emtspy").
  !     For non-interactive emtp, this module can be destroyed.
  include 'dekspy.ftn'
  include 'dekplt.ftn'
  if ( iprspy .lt. 1 ) go to 1483
  write (munit6, 1478)  tstep
1478 format ( ' top of  "back14" .   tstep =', e15.5 )
  call window
1483 tstep = -1.0
  return
end subroutine back14
!
!     subroutine setrtm.
!
subroutine setrtm
  !     Module used only for interactive EMTP (service to "emtspy").
  !     For non-interactive EMTP, this module can be destroyed.
  include 'dekspy.ftn'
  include 'dekplt.ftn'
  real*8 d1, d6
  save
  lunt15 = 15
  close ( unit=lunt15 )
  open (unit = lunt15, status = 'old', file = 'tpparam')
  if ( iprspy .lt. 1 ) go to 2719
  write (munit6, 2718)
2718 format ( ' after unit lunt15 connection tpparam.' )
  call window
2719 n22 = 0
  n23 = 0
  rewind lunt15
  read (lunt15, 5100, end=2729)  buffin
5100 format ( a80 )
  rewind lunt15
  if ( buffin(1:8)  .ne.  setdat )   go to 2732
  write (prom80, 2724)
2724 format (  '  send subset number :'  )
  call prompt
  read (munit5, 5100)  prom80
  call frein1 ( prom80, n1 )
  if ( n1  .eq.  0 )   n1 = 1
  do 2728  j=1, 9999
     read (lunt15, 5151, end=2729)  buffin
5151 format ( a16 )
     if ( buffin(1:8)  .ne.  setdat )   go to 2728
     write (ansi, 5100)  buffin(9:16)
     read (ansi, 2726)  n2
2726 format ( i2 )
     if ( n2  .eq.  n1 )   go to 2732
2728 end do
2729 write (munit6, 2730)  n1
2730 format (  ' ** error.   subset number',  i4, '  does not exist.   try again.'   )
  call window
  go to 2751
2732 read (lunt15, 5100)  prom80
  call fresp2(prom80, d6, d1)
  jj = d6
  if ( jj  .eq.  0 )   go to 2741
  if ( jj .gt. n23 )  n23 = jj
  if ( iprspy .lt. 5 ) go to 2736
  write (munit6, 2735)  jj, fvcom(jj), d1
2735 format (  ' next real datum.  jj, fvcom, d1 =', i6,  2e14.5  )
  call window
2736 fvcom(jj) = d1
  go to 2732
2741 read (lunt15, 5100)  prom80
  call frein2(prom80, jj, n1)
  if ( jj  .eq.  0 )   go to 2745
  if ( jj .gt. n22 )  n22 = jj
  if ( iprspy .lt. 5 ) go to 2744
  write (munit6, 2743)  jj, ivcom(jj), n1
2743 format ( ' next integer datum.  jj, ivcom, n1 =',  3i8  )
  call window
2744 ivcom(jj) = n1
  go to 2741
2745 read (lunt15, 2746)  jj, textd1
2746 format ( i2, a8 )
  if ( jj  .le.  0 )   go to 2751
  if ( iprspy .lt. 5 ) go to 2748
  write (munit6, 2747)  jj, anplt(jj), textd1
2747 format (  ' next key word.  j =',  i3,  1x, a8,  1x, a8  )
  call window
2748 anplt(jj) = textd1
  go to 2745
2751 if ( iprspy .lt. 3 ) go to 2763
  write (munit6, 2753)
2753 format (  ' final real data fvcom through the last', ' modified cell follows ....'  )
  call window
  do j=1, n23, 6
     write (munit6, 3413) ( fvcom(ip), ip=j, j+5 )
3413 format ( 1x, 6e13.4 )
     call window
3417 end do
  write (munit6, 2754)
2754 format ( ' final integer data ivcom through the last', ' modified cell follows ....' )
  call window
  do j=1, n22, 10
     write (munit6, 3424) ( ivcom(ip), ip=j, j+9 )
3424 format ( 1x, 10i7 )
     call window
3427 end do
2763 close ( unit=lunt15, status='keep' )
  return
end subroutine setrtm
!
!     subroutine chrplt.
!
subroutine chrplt
  !     module used only for interactive emtp (service to "emtspy").
  !     for non-interactive emtp, this module can be destroyed.
  include 'dekspy.ftn'
  include 'dekplt.ftn'
  dimension temp(6)
  character*9 kunit6
  character*1 dol(131), letter(20)
  save
  data  letter(1)   /  'a'  /
  data  letter(2)   /  'b'  /
  data  letter(3)   /  'c'  /
  data  letter(4)   /  'd'  /
  data  letter(5)   /  'e'  /
  data  letter(6)   /  'f'  /
  data  letter(7)   /  'g'  /
  data  letter(8)   /  'h'  /
  data  letter(9)   /  'i'  /
  data  letter(10)  /  'j'  /
  data  letter(11)  /  'k'  /
  data  letter(12)  /  'l'  /
  data  letter(13)  /  'm'  /
  data  letter(14)  /  'n'  /
  data  letter(15)  /  'o'  /
  data  letter(16)  /  'p'  /
  data  letter(17)  /  'q'  /
  data  letter(18)  /  'r'  /
  data  letter(19)  /  's'  /
  data  letter(20)  /  't'  /
  if ( monits .eq. 1 )  go to 2513
  vspan = vmax - vmin
  d3 = limcol / vspan
  kzero = -vmin * d3
  if ( kzero  .eq.  0 )   kzero = 1
  if ( kzero  .lt.  0 )   kzero = 0
  kp(1) = 1
  dy = vspan / (0.1*limcol)
  if ( mfake  .eq.  jplt )   go to 6413
  write (munit6, 6403)
6403 format (  ' hidden variable scalings follow :'  )
  call window
  write (munit6, 6411)  ( aaa(j), j=1, jplt )
6411 format (  '  factor :',  10e11.3  )
  call window
  write (munit6, 6412)  ( bbb(j), j=1, jplt )
6412 format (  '  offset :',  10e11.3  )
  call window
6413 din1 = hmin * tmult
  din2 = hmax * tmult
  do ip=1, numtit
     write (munit6, 6423)   sext(ip)
6423 format (  ' graph title : ',  a80  )
     call window
6427 end do
  if ( mxypl  .gt.  0 )   go to 6582
  if ( jplt  .le.  1 )   go to 6447
  do k=2, jplt
6431 kp(k) = mstart(k-1) + 5
  end do
6447 dt = hpi / 6.0
  temp(1) = vmin + dy
  d2 = 2.0 * dy
  n4 = 6
  if ( limcol  .le.  79 )   n4 = 4
  do k=2, n4
6449 temp(k) = temp(k-1) + d2
  end do
  write (munit6, 6450)  ( temp(k), k=1, n4 )
6450 format ( 3x,  6( e14.5, 6x )   )
  call window
  if ( limcol  .le. 79 )  go to 6453
  write (munit6, 6451)
6451 format (  1x, '1---------1---------1---------1', &
       '---------1---------1---------1---------1', &
       '---------1---------1---------1---------1', &
       '---------1---------1'  )
  call window
  go to 6456
6453 write (munit6, 6454)
6454 format (  1x, '1---------1---------1---------1---------1', &
       '---------1---------1---------1---------1'  )
  call window
6456 if ( iprspy  .lt. 1 )  go to 6460
  write (munit6, 6458)
6458 format (  ' begin character plot.',  11x, 4hhmin, 11x, 4hhmax,  13x, 2hdt,  11x, 4hvmin,  11x, 4hvmax )
  call window
  write (munit6, 6459)  hmin, hmax, dt, vmin, vmax
6459 format ( 22x, 5e15.6 )
  call window
6460 inch = 0
  idat = jplt
  t = hmin
6461 t = t + dt
  inch = inch + 1
  call quiter
  if ( kwtspy .eq. 1 )  go to 9000
  if ( t  .gt.  hmax )   go to 6582
  do k=1, limcol
6478 dol(k) = ' '
  end do
  do k=1, jplt
     l = kp(k)
     if ( iprspy  .ne.  34 )  go to 6483
     write (munit6, 6481)  k, l, mstart(k), ew(l), t
6481 format ( 1x,  24h       k       l  mstart,  10x, 5hew(l),  14x,  1ht  )
     call window
     write (munit6, 6482)  k, l, mstart(k), ew(l), t
6482 format ( 1x, 3i8, 2e15.6 )
     call window
6483 if ( l  .eq.  0 )   go to 6531
6485 if ( ew(l)  .le.  t )   go to 6493
     kp(k) = l
     go to 6531
6493 d1 = ew(l+1)
     if ( d1  .gt.  vmin )   go to 6503
     dol(1) = letter(k)
     go to 6518
6503 if ( d1  .lt.  vmax )   go to 6508
     dol(limcol) = letter(k)
     go to 6518
6508 m = ( d1 - vmin ) * d3
     dol(m) = letter(k)
     if ( iprspy .ne. 34 ) go to 6518
     write (munit6, 6511)  m, dol(m)
6511 format (  ' in-bounds insert..  m, dol(m) =',  i4, 1x, a1 )
     call window
6518 l = l + 2
     if ( l  .lt.  mstart(k) )   go to 6485
     kp(k) = 0
     idat = idat - 1
6531 end do
  if ( inch  .ge.  6 )   go to 6548
  if ( kzero  .le.  9 )   go to 6542
  if ( dol(kzero)  .eq.  ' '  )  dol(kzero) = '1'
6542 write (munit6, 6543)  ( dol(k), k=1, limcol )
6543 format ( 1x, 131a1 )
  call window
  go to 6559
6548 if ( t  .le.  9.999999 ) write (kunit6, 6553)  t
6553 format ( 1h , f8.6 )
  if ( t  .ge.  10.0 ) write (kunit6, 6554)  t
6554 format ( 1h , f8.5 )
  if ( kzero  .le.  9 )  go to 6555
  if ( dol(kzero)  .eq.  ' '  )  dol(kzero) = '1'
6555 write (munit6, 6556) kunit6, ( dol(k), k=10, limcol )
6556 format ( 1x, a9, 123a1 )
  call window
  inch = 0
6559 if ( idat  .gt.  0 ) go to 6461
6582 if ( mu6std .eq. 6 ) go to 2694
  mu6std = 6
  write (munit6, 2687)  linepr
2687 format (  '   ***  line printer copy completed on', ' unit',  i3,  ' .  ***'   )
  call window
2694 if ( monits  .eq.  0 )   go to 9000
  if ( iprspy .lt. 1 )  go to 2501
  write (munit6, 7832)  ind1
7832 format (  ' begin rolling.  ind1 =',  i8 )
  call window
2501 t = t + dt
  call quiter
  if ( kwtspy .eq. 1 )  go to 9000
  inch = inch + 1
  do k=1, limcol
2504 dol(k) = ' '
  end do
2507 if ( ind1 .le. indbuf-kptplt )  go to 2513
  go to 9000
2513 if ( iprspy .lt. 3 ) go to 2517
  write (munit6, 2516)  ind1, jplt, pltbuf(ind1), t
2516 format (  ' past 1/6 inch? ind1, jplt, pltbuf(ind1), t =', 2i7, 2e15.5  )
  call window
2517 if ( pltbuf(ind1)  .gt.  t )   go to 2564
  do k=1, jplt
     j1 = iabs( mplot(k) )
     d1 = pltbuf(ind1+j1)
     if ( mplot(k)  .lt.  0 )   d1 = -d1
     if ( d1  .gt.  vmin )   go to 2532
     dol(1) = letter(k)
     go to 2542
2532 if ( d1  .lt.  vmax )   go to 2537
     dol(limcol) = letter(k)
     go to 2542
2537 m = (d1 - vmin ) * d3
     dol(m) = letter(k)
2542 end do
  ind1 = ind1 + kptplt + 1
  go to 2507
2564 if ( inch  .lt.  6 )   go to 2571
  write (ansi8, 2569)  t
2569 format ( 1x, f7.4 )
  read (ansi8, 2566)  ( dol(ip), ip=1, 8 )
2566 format ( 8a1 )
  inch = 0
  go to 2574
2571 if ( kzero  .eq.  0 )   go to 2574
  if ( dol(kzero)  .eq.  ' ' )   dol(kzero) = '1'
2574 write (munit6, 6543)  ( dol(k), k=1, limcol )
  call window
  go to 2501
9000 if ( iprspy .lt. 1 )  go to 9008
  write (munit6, 9003)
9003 format (  ' exit "chrplt".'  )
  call window
9008 return
end subroutine chrplt
!
!     subroutine tekplt.
!
subroutine tekplt
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  return
end subroutine tekplt
!
!     subroutine flatbd.
!
subroutine flatbd         ! no implicit
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module used only for interactive EMTP (service to "emtspy").
  !     For non-interactive emtp, this module can be destroyed.
  !     The one and only use is to provide calcomp copies of what
  !     is on the screen (for "plot" command of "emtspy"), with
  !     BPA pseudo-calcomp versatek calls actually used.
  include 'dekspy.ftn'
  include 'dekplt.ftn'
  real*8 d1, d2
  if ( killpl  .eq.  0 )   go to 2005
  call plot ( 0.0, 0.0, 999 )
  return
2005 d1 = hpi * tmult
  write (prom80, 2010)  d1
2010 format (  ' send t-axis units/inch (',  e12.3, ' ) :'   )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  buff77   ! read a80 character input
  if ( buff77(1:8)  .ne.  metric )   go to 7236
  fact = .7874
  dxgrd1 = 0.5
  dygrd1 = dxgrd1
  dxgrd2 = 0.1
  dygrd2 = dxgrd2
  go to 2005
7236 if ( buff77(1:8)  .ne.  pen )   go to 7261
  write (munit6, 7238)
7238 format ( ' respond to each pen-width request with integer'  ,/, &
       ' information.  blank or zero implies that there'  ,/, &
       ' will be no change from the present value, while' ,/, &
       ' a value of  -1  suppresses the printout, and'    ,/, &
       ' values of  1  through  5  are versatek dot'      ,/, &
       ' widths (vax "calcomp" at bpa is versatek).'      )
  write (prom80, 7240)  mtit
7240 format ( 3x,  '80-col. case-title (',  i2,  ' ) :'   )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
7241 format ( a80 )
  call frein1 ( prom80, n1 )   ! decode free-field n1
  if ( n1  .ne.  0 )   mtit = n1
  write (prom80, 7242)  maxisx
7242 format ( 3x,  'x-axis structure (',  i2,  ' ) :'    )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call frein1 ( prom80, n1 )   ! decode free-field n1
  if ( n1  .ne.  0 )   maxisx = n1
  write (prom80, 7244)  maxisy
7244 format ( 3x,  'y-axis structure (',  i2,  ' ) :'    )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call frein1 ( prom80, n1 )   ! decode free-field n1
  if ( n1  .ne.  0 )   maxisy = n1
  write (prom80, 7246)  mgrid1
7246 format ( 3x,  'big background grid (',  i2,  ' ) :'    )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call frein1 ( prom80, n1 )   ! decode free-field n1
  if ( n1  .ne.  0 )   mgrid1 = n1
  write (prom80, 7247)  mgrid2
7247 format ( 3x,  'fine inner grid (',  i2,  ' ) :'    )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call frein1 ( prom80, n1 )   ! decode free-field n1
  if ( n1  .ne.  0 )   mgrid2 = n1
  write (prom80, 7250)  msuper
7250 format ( 3x,  'super-title (',  i2,  ' ) :'    )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call frein1 ( prom80, n1 )   ! decode free-field n1
  if ( n1  .ne.  0 )   msuper = n1
  write (prom80, 7251)  mid
7251 format ( 3x,  'date, time, etc. (',  i2,  ' ) :'    )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call frein1 ( prom80, n1 )   ! decode free-field n1
  if ( n1  .ne.  0 )   mid = n1
  write (munit6, 7254)  ( mcurve(j), j=1, jplt )
7254 format ( 3x,  'vector of curve weights (',  20i3,  ' ) :'  )
  read (munit5, *)  ( kp(j), j=1, jplt )
  do j=1, jplt
     if ( kp(j)  .ne.  0 )   mcurve(j) = kp(j)
7255 end do
  go to 2005
7261 if ( buff77(1:8)  .ne.  show  )   go to 7287
  write (munit6, 3421)  mtit, maxisx, maxisy, mgrid1, mgrid2, msuper, mid
3421 format ( /,  ' begin with  #pen#  parameters :'  ,/, &
       i5,  ' = pen for 80-column case title lines;'  ,/, &
       i5,  ' = pen for x-axis structure;'            ,/, &
       i5,  ' = pen for y-axis structure;'            ,/, &
       i5,  ' = pen for big background grid;'         ,/, &
       i5,  ' = pen for fine inner grid;'             ,/, &
       i5,  ' = pen for 1-line super-title;'          ,/, &
       i5,  ' = pen for date, time, etc.'    )
  write (munit6, 3428)  ( mcurve(j), j=1, jplt )
3428 format ( 8x,  'pens for drawing individual curves follow ...' ,/,  ( 8x,  10i5 )  )
  write (munit6, 3453)  xtit, ytit, siztit, xid, yid, sizid, xsuper, ysuper, sizsup
3453 format ( /,  ' next come the  #size#  parameters :'  ,/, &
       4x,  7hx-begin,  3x,  7hy-begin,  3x,  6hheight    ,/, &
       1x,  3f10.2,  '  --- 80-col. case title (1st line)'    ,/, &
       1x,  3f10.2,  '  --- date, time, etc. (top line)'      ,/, &
       1x,  3f10.2,  '  --- one line super-title'    )
  write (munit6, 3459)  htax, fact, numsym
3459 format ( /, 1x,  f14.3,  '  --- height of time axis;'               ,/, &
       1x,  f14.6,  '  --- graph magnification factor', &
       ' (.7874 for metric);'              ,/, &
       1x,  i14,    '  --- number of symbols marking each curve.'  )
  write (munit6, 3464)  fill1, fill2, ncut1, ncut2
3464 format ( /, 11h major grid,  4x,  10hminor grid  ,/, &
       1x,  f10.3,  f14.3,  '  --- fill-in fractions (1.0 for', &
       ' solid grid);'                 ,/, &
       1x,    i10,    i14,  '  --- number of dashes per grid line;'  )
  write (munit6, 3472)  dxgrd1, dxgrd2, dygrd1, dygrd2
3472 format ( 1x,  f10.3,  f14.3,  '  --- spacing between vertical', &
       ' grid lines;'                  ,/, &
       1x,  f10.3,  f14.3,  '  --- spacing between horizontal', &
       ' grid lines;'   ,/, 1x  )
  go to 2005
7287 if ( buff77(1:8)  .ne.  size )   go to 7328
  write (munit6, 7288)
7288 format (  ' respond to each request with revised', &
       ' values.   a blank or zero means no'   ,/, &
       ' change from the value shown within', &
       ' square brackets.'                       )
  write (prom80, 7291)  xtit, ytit, siztit
7291 format ( '   x,y coordinates and size of 79-char. title  (', &
       3 f6.2,  ' ) :'  )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call fresp3(prom80, d1, d2, d3)     ! decode free-field
  if ( d1  .ne.  0.0 )   xtit = d1
  if ( d2  .ne.  0.0 )   ytit = d2
  if ( d3  .ne.  0.0 )   siztit = d3
  write (prom80, 7304)  xid, yid, sizid
7304 format ( '   likewise for date, time, etc.  (', 3f6.2,  ' ) :'  )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call fresp3 ( prom80, d1, d2, d3 )     ! decode free-field
  if ( d1  .ne.  0.0 )   xid = d1
  if ( d2  .ne.  0.0 )   yid = d2
  if ( d3  .ne.  0.0 )   sizid = d3
  write (prom80, 7309)  xsuper, ysuper, sizsup
7309 format ( '   likewise for 16-char. super-title  (', 3f6.2,  ' ) :'  )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call fresp3 ( prom80, d1, d2, d3 )     ! decode free-field
  if ( d1  .ne.  0.0 )   xsuper = d1
  if ( d2  .ne.  0.0 )   ysuper = d2
  if ( d3  .ne.  0.0 )   sizsup = d3
  write (prom80, 7315)  htax
7315 format (  '   height of time-axis  (',  f6.2,  ' ) :'   )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call fresp1(prom80, d1)   ! decode free-field d1
  if ( d1  .ne.  0.0 )   htax = d1
  write (prom80, 7319)  fact
7319 format (  '   blowup factor.  metric=.7874  (', f6.4,  ' ) :'    )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call fresp1 ( prom80, d1 )   ! decode free-field d1
  if ( d1  .ne.  0.0 )   fact = d1
  write (prom80, 7320)  fill1, fill2
7320 format (  '   fill-in fractions for major and minor grids  (', 2f6.3,  ' ) :'  )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call fresp2(prom80, d1, d2)  ! decode free-field d1, d2
  if ( d1  .ne.  0.0 )   fill1 = d1
  if ( d2  .ne.  0.0 )   fill2 = d2
  write (prom80, 7321)  ncut1, ncut2
7321 format (  '   number of breaks for major and minor grids  (', i4,  '.0 ',  i4,  '.0 ) :'   )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call fresp2 ( prom80, d1, d2 )  ! decode free-field d1, d2
  if ( d1  .ne.  0.0 )   ncut1 = d1
  if ( d2  .ne.  0.0 )   ncut2 = d2
  write (prom80, 7322)  dxgrd1, dygrd1
7322 format (  '   x,y spacing between major grid lines  (', 2f6.3,  ' ) :'  )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call fresp2 ( prom80, d1, d2 )  ! decode free-field d1, d2
  if ( d1  .gt.  0.0 )   dxgrd1 = d1
  if ( d2  .gt.  0.0 )   dygrd1 = d2
  write (prom80, 7323)  dxgrd2, dygrd2
7323 format (  '   x,y spacing between minor grid lines  (', 2f6.3,  ' ) :'  )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call fresp2 ( prom80, d1, d2 )  ! decode free-field d1, d2
  if ( d1  .gt.  0.0 )   dxgrd2 = d1
  if ( d2  .gt.  0.0 )   dygrd2 = d2
  write (prom80, 7325)  numsym
7325 format (  '   number of marking symbols per curve  (', i3,  ' ) :'   )
  call prompt     ! write prom80 with cursor control (no lf)
  read (munit5, 7241)  prom80
  call frein1 ( prom80, n2 )   ! decode free-field n2
  if ( n2  .ne.  0 )   numsym = n2
  go to 2005
7328 read (buff77, 2030)  d3
2030 format ( 10e6.0 )
  if ( d3  .eq.  0.0 )   go to 2060
  hpi = d3 / tmult
2060 d1 = ( hmax - hmin ) / hpi
  if(d1 .le. 72.0) go to 2090
  write (munit6, 2070)
2070 format ( 64h error. ----- requested flatbed plot would exceed 72 inch limit.   ,/,  32h send another time-axis scaling.   )
  go to 2005
2090 if ( iprsrt  .ge.  1 ) write (munit6, 2103)  jplt, ihs, numflt, vmin, vmax, hmin, hmax, hpi
2103 format ( /,  ' begin flatbed.', 24h    jplt     ihs  numflt  ,/,  15x,  3i8  ,/, &
       1x,  11x,  4hvmin,  11x,  4hvmax,  11x,  4hhmin, 11x,  4hhmax,  12x,  3hhpi  ,/,  1x,  5e15.6  )
  if ( numflt  .gt.  0 )   go to 2108
  call plots ( 0, 0, 0 )
  call plot ( 1.0, 1.0, -3 )
  call factor ( fact )
2108 numflt = numflt + 1
  write (munit6, 2109)
2109 format ( '    ready to draw calcomp titles.'  )
  n3 = numtit / 10
  if ( n3  .le.  0 )   go to 2114
  if ( mtit  .eq.  -1 )   go to 2114
  d2 = ytit
  n4 = 1
  if ( ytit  .gt.  10 )   go to 2114
  call newpen ( mtit )
  do j=1, n3
     call symbol ( xtit, d2, siztit, sext(n4), 0.0, 80 )
     d2 = d2 - fline * siztit
2113 n4 = n4 + 10
  end do
2114 n1 = 3 * ( ihs - 1 )  +  1
  !     only units of  #seconds#  can be presently honored.
  n1 = 7
  if ( mxypl  .eq.  1 )   n1 = 22
  d2 = (vmax - vmin ) / 8.0
  call newpen ( maxisx )
  write (munit6, 2115)
2115 format ( '   ready to draw calcomp axes.'  )
  if ( maxisx  .gt.  0 ) call axis ( 0.0,  htax, horzl(n1),  -24,  d1,  0.0, hmin,  hpi )
  call newpen ( maxisy )
  if ( maxisy  .gt.  0 ) call axis ( 0.0,  0.0,  vertl,   nchver,  8.,  90., vmin,  d2 )
  n8 = d1 / dxgrd1  +  0.5
  n9 = 8.0 / dygrd1  +  0.5
  fill = fill1
  ncut = ncut1
  call newpen ( mgrid1 )
  write (munit6, 2116 )
2116 format (  '   ready to call calcomp grid number 1.'  )
  if ( mgrid1  .gt.  0 ) call grid ( 0.0, 0.0, n8, dxgrd1, n9, dygrd1, lmask1 )
  write (munit6, 2117 )
2117 format ( '   done with calcomp grid number 1.'  )
  call newpen ( mgrid2 )
  fill = fill2
  ncut = ncut2
  n8  =  d1 / dxgrd2  +  0.5
  n9  =  8.0/ dygrd2  +  0.5
  if ( mgrid2  .gt.  0 ) call grid ( 0.0, 0.0, n8, dxgrd2, n9, dygrd2, lmask2 )
  call newpen ( msuper )
  write (munit6, 2118)
2118 format ( '   done with calcomp grid number 2.'  )
  if ( msuper  .gt.  0 ) call symbol ( xsuper, ysuper, sizsup, headl, 0.0, nchsup )
  d6 = yid
  !     encode (18, 2119, alpha(1) )   date1, tclock
  write (ansi32, 2119)  datepl, tclopl
2119 format ( 2a4, 2x, 2a4 )
  call newpen ( mid )
  if ( mid  .eq.  -1 )   go to 2127
  call symbol ( xid, d6, sizid, ansi32, 0.0, 18 )
  d6 = d6 - fline * sizid
  !     encode (14, 2122, alpha(1) )   icp
  write (ansi16, 2122)  icp
2122 format ( 9hplot type, i5 )
  call symbol ( xid, d6, sizid, ansi16, 0.0, 14 )
  d6 = d6 - fline * sizid
  if ( mfake  .eq.  jplt )   go to 3149
  n5 = jplt
  if ( jplt  .gt.  6 )   n5 = 6
  !     encode (80, 3146, alpha(1) )  ( aaa(j), j=1, n5 )
  write (buff77, 3146)  ( aaa(j), j=1, n5 )
3146 format (  9h factor :,  6e11.3  )
  call symbol ( xid, d6, sizid, buff77, 0.0, 80 )
  d6 = d6 - fline * sizid
  !     encode (80, 3147, alpha(1) )  ( bbb(j), j=1, n5 )
  write (buff77, 3147)  ( bbb(j), j=1, n5 )
3147 format (  9h offset :,  6e11.3  )
  call symbol ( xid, d6, sizid, buff77, 0.0, 80 )
  d6 = d6 - fline * sizid
3149 write (munit6, 3150)
3150 format ( '   ready for slot1 encode.'  )
  n5 = 4
  if ( namvar  .lt.  4 )   n5 = namvar
  !     encode (36, 2125, alpha(1) )   ( slot1(j), j=1, n5 )
  write (buff77, 2125)  ( slot1(j), j=1, n5 )
2125 format ( 8hnames : ,  4a7  )
  call symbol ( xid, d6, sizid, buff77, 0.0, 36 )
  if ( namvar  .le.  4 )   go to 2127
  do j=5, namvar, 4
     n5 = j + 3
     !     encode (36, 2137, alpha(1) )
     write (buff77, 2137)
2137 format ( 36x )
     !     encode (36, 2138, alpha(1) )   ( slot1(i), i=j, n5 )
     write (buff77, 2137)
2138 format ( 8x,  4a7 )
     d6 = d6 - fline * sizid
2140 call symbol ( xid, d6, sizid, buff77, 0.0, 36 )
  end do
2127 n4 = 0
  do j=1, jplt
     n5 = kstart(j)
     n6 = ( n5 - n4 ) / 2
     n7 = n6 / numsym
     if ( numsym  .lt.  0 )   n7 = 99999
     if ( numsym  .gt.  n6 )  n7 = 1
     ew(n5+3) = hpi
     n8 = n4 + 2*n6 + 8
     if ( iprsrt  .ge.  5 ) write (munit6, 3117)  j, n4, n5, n6, n7, (ew(mm), mm=n4, n8)
3117 format ( /,  ' ready to call line.', 40h       j      n4      n5      n6      n7  ,/, &
          20x,  5i8  ,/,  ( 1x,  8e15.6  )  )
     call newpen ( mcurve(j) )
     sx = 1.0 / ew(n5+3)
     sy = 1.0 / ew(n5+4)
     xmin = ew(n5+1)
     ymin = ew(n5+2)
     d1 = ( ew(n4+1) - xmin ) * sx
     d2 = ( ew(n4+2) - ymin ) * sy
     call plot ( d1, d2, 3 )
     n6 = n7
     if ( iprsrt  .ge.  2 ) write (munit6, 4122)  xmin, ymin, sx, sy, d1, d2
4122 format ( /, ' enter curve-loop.  xmin, ymin, sx, sy, d1, d2 =', /, 1x, 6e13.4 )
2121 n4 = n4 + 2
     if ( n4  .ge.  n5 )   go to 2132
     n6 = n6 - 1
     if ( n6  .gt.  0 )   go to 8243
     call symbol ( d1, d2, sizid, j, 0.0, -1 )
     n6 = n7
8243 d1 = (ew(n4+1) - xmin ) * sx
     d2 = (ew(n4+2) - ymin ) * sy
     if ( iprsrt  .ge.  9 ) write (munit6, 2123)  n4, n6, d1, d2
2123 format ( ' next point drawn.  n4, n6, d1, d2 =',  2i5, 2e13.4 )
     call plot ( d1, d2, 2 )
     go to 2121
2132 n4 = n5 + 4
  end do
  d1 = d1 + 1.0
  call plot ( d1, 0.0, -3 )
  if ( iprspy  .ge.  1 ) write (munit6, 9006)
9006 format ( '   done with calcomp copy of screen.'  )
  return
end subroutine flatbd
!
!     subroutine tgrid.
!
subroutine tgrid(ix,iy,nx,idelx,ny,idely,ldash) ! no implicit
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Module used only for interactive EMTP (service to "emtspy").
  !     for non-interactive EMTP, this module can be destroyed.
  !     Reliance upon Tektronix plot10 makes this installation-
  !     dependent (this is VAX-11 module, actually).
  if ( nx  .le.  0 )   return
  if ( ny  .le.  0 )   return
  call movabs ( ix, iy )
  n8 = ix  +  ( nx - 1 ) * idelx
  n9 = iy  +  ( ny - 1 ) * idely
  n3 = n9
  n2 = ix
  do j=1, nx
     call dshabs ( n2, n3, ldash )
     n2 = n2 + idelx
     if ( j  .eq.  nx )   go to 1356
     call movabs ( n2, n3 )
     n4 = n3
     n3 = n9
     if ( n4  .eq.  n9 )   n3 = iy
1341 end do
1356 call movabs ( n8, n9 )
  n2 = ix
  n3 = n9
  do 1374  j=1, ny
     call dshabs ( n2, n3, ldash )
     if ( j  .eq.  ny )   go to 1382
     n3 = n3 - idely
     call movabs ( n2, n3 )
     n4 = n2
     n2 = n8
     if ( n4  .eq.  n8 )   n2 = ix
1374 end do
1382 return
end subroutine tgrid
!
!     end of file: over20.for
!
