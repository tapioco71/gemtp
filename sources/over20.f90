!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over20.f90
!

!
! subroutine over20.
!

subroutine over20
  use blkcom
  use labcom
  use tracom
  use comthl
  implicit none
  integer(4) :: i, iold, ipp
  integer(4) :: k, kn1
  integer(4) :: l, ll1, ll9, ll10
  integer(4) :: m
  integer(4) :: n1, n2, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, ndx1, ndx2
  integer(4) :: ndx3, npl, nstat
  real(8) :: a, d1, d2, zero
  !
  !  equivalence (moncar(1), knt), (moncar(2), kbase)
  !  equivalence (moncar(3), ltdelt), (moncar(4), isw)
  !  equivalence (moncar(10), mtape)
  !
  integer(4), pointer :: isw => moncar(4)
  integer(4), pointer :: kbase => moncar(2)
  integer(4), pointer :: knt => moncar(1)
  integer(4), pointer :: ltdelt => moncar(3)
  integer(4), pointer :: mtape => moncar(10)
  !
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  Begin module "over20".')
  zero = 0.0
  ll1 = 1
  ll9  =  9
  ll10 = 10
  call runtym (d1, d2)
  n10 = 1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1324) iplot, nenerg, kbase, m4plot,  mtape, icat, lstat(32), t, aincr
1324 format (/, 1x, '   iplot  nenerg  kbase  m4plot   mtape    icat lstat32', 14x, 't', 10x, 'aincr', /, 1x, 7i8, 2e15.5)
  if (max99m .lt. 0) write (unit = lunit(6), fmt = 9345) max99m
9345 format (' ++++  Number of suppressed type-99 flashover or clearing messages is negative of', i7)
  if (peaknd(1) .eq. 0.0) go to 5019
  n6 = int (peaknd(3), kind (n6))
  write (unit = lunit(6), fmt = 5011) peaknd(1), peaknd(2), bus(n6)
5011 format (8x, 'Overall simulation peak node voltage =', e15.6, ' .   Time (sec) =', e14.5, ' .   bus = ', "'", a6, "'", ' .')
5019 k = lstat(32) + 1
  if (iplot .lt. 0) go to 8005
  volti(1) = -9999.
  if (m4plot .eq. 0) go to 5022
  call pltfil (k)
  go to 8005
5022 write (lunit(4)) (volti(i), i = 1, k)
8005 if (icat .gt. 0 .or. memsav .gt. 0) call katalg
  close (unit = 79)
  k = lstat(32)
  !      both max and min will be printed for each energization when
  !      'aincr' is set to be greater than or equal to '55.0'.
  if (kbase .eq. 2 .and. aincr .lt. 55.0) go to 3614
  if (begmax(1) .le. 0.0) go to 3614
  write (unit = lunit(6), fmt = 8002) (xmax(l), l = 1, k)
8002 format (/, ' Maxima and minima which occurred during the simulation follow.   The order and column positioning are the', /, ' same as for the regular printed output vs. time.', /, ' Variable maxima :', /, (15x, 9e13.6))
  !     Extrema vector  "xmax"  actually has four partitions,
  !     each the size of list 12:  (xmax, tmax, xmin, tmin) .
  ndx1 = lsiz12 + 1
  ndx2 = lsiz12 + k
  write (unit = lunit(6), fmt = 8003) (xmax(l), l = ndx1, ndx2)
8003 format (' Times of maxima :', /, (15x, 9e13.6))
  ndx1 = ndx1 + lsiz12
  ndx2 = ndx2 + lsiz12
  write (unit = lunit(6), fmt = 8004) (xmax(l), l = ndx1, ndx2)
8004 format (' Variable minima :', /, (15x, 9e13.6))
  ndx1 = ndx1 + lsiz12
  ndx2 = ndx2 + lsiz12
  write (unit = lunit(6), fmt = 18005)  (xmax(l), l = ndx1, ndx2)
18005 format (' Times of minima :', /, (15x, 9e13.6))
3614 if (nenerg .ne. 0) go to 605
  flstat(9) = d1
  flstat(10) = d2
  if (begmax(1) .ne. 0.0 .and. begmax(2) .ne. 0.0) write (unit = lunit(6), fmt = 8006) begmax(2)
8006 format (56x, 'Search for extrema began at time', e15.6, '   sec.')
  write (unit = lunit(6), fmt = 8007)
8007 format (1x)
  if (ipunch .le. 0) go to 9800
!5759 kcount = 2
  kcount = 2
  iold = 3
  if (n10 .ne. 0) write (unit = lunit(6), fmt = 7009) t
7009 format (/, ' Printout of the saving of terminal conditions for all components, at time', e15.7, '   seconds.', /, ' Begin with all node voltages.', /, 3(21x, 'node'), /, 19x, 'number', 21x, 'name', 18x, 'voltage')
  do k = 2, ntot
     if (n10 .ne. 0) write (unit = lunit(6), fmt = 7012) bus(k), e(k), zero, k
7012 format(' ', a6, 2e13.5, 41x, i6)
!7010 write (unit = lunit7, fmt = 7011) kcount, bus(k), e(k), zero, zero, k
     write (unit = lunit(7), fmt = 7011) kcount, bus(k), e(k), zero, zero, k
  end do
7011 format (i2, a6, 3e15.8, 21x, i6)
  if (n10 .ne. 0) write (unit = lunit(6), fmt = 7013)
7013 format (/, " Linear branch table state variables ( 'currents' ) follow.", /, 7x,  'row', 9x, "'from' node", 11x, "'to' node", 9x, 'linear branch parameters, identified by the Fortran vector storage', /,  9x, 'i', 8x, 'bus(kbus(i))', 8x, 'bus(mbus(i))', 19x, 'cik(i)', 20x, 'ci(i)', 20x, 'ck(i)')
  k = 1
7014 it2 = 1
  if (kbus(k) .lt. 0) go to 7024
  if (nr(k) .ge. 0) go to 7022
  ci1 = ci(k)
  ck1 = ck(k)
  if (ci1 .eq. 0. .and. ck1 .eq. 0.) go to 7025
  l = kbus(k)
  m = iabs(mbus(k))
  write (unit = lunit(7), fmt = 7020) iold, bus(l), bus(m), ci1, ck1, zero, zero, k
7020 format (i2, 2a6, 4e15.8, i6)
  if (n10 .ne. 0) write (unit = lunit(6), fmt = 7021) bus(l), bus(m), ci1, ck1, zero, zero, k
7021 format (1x, a6, 1x, a6, 4e13.5, i6)
  go to 7025
7022 it2 = iabs(length(k))
  it1 = k + it2 - 1
  do i = k, it1
     n1 = kbus(i)
     n2 = iabs(mbus(i))
     if (n10 .ne. 0) write (unit = lunit(6), fmt = 7021) bus(n1), bus(n2), cik(i), ci(i), ck(i), zero, i
!7023 write (unit = lunit(7), fmt = 7020) iold, bus(n1), bus(n2), cik(i), ci(i), ck(i), zero, i
     write (unit = lunit(7), fmt = 7020) iold, bus(n1), bus(n2), cik(i), ci(i), ck(i), zero, i
  end do
  go to 7025
7024 n1 = iabsz (kbus(k))
  n2 = iabsz (mbus(k))
  write (unit = lunit(6), fmt = 70241) bus(n1), bus(n2)
70241 format ('  ****  Warning.   ****   No currents will be punched for the distributed line connecting nodes  ', '"', a6,  '"', '  and  ', '"', a6, '"', ' . ')
  it2 = length(k)
  if (it2 .lt. 0) it2 = -it2
  if (length(k) .gt. 0) it2 = 1
  if (kodsem(k) .ne. 0 .and. imodel(k) .ne. 2) it2 = iabsz (kodebr(k))
7025 k = k + it2
  if (k .le. ibr) go to 7014
  if (inonl .eq. 0) go to 9207
  if (n10 .ne. 0) write (unit = lunit(6), fmt = 4688)
4688 format (/, ' Nonlinear element table state variables follow.')
  iold = 4
  do k = 1, inonl
     if (nonle(k) .ge. 0) cycle
     n1 = nonlk(k)
     n2 = iabs(nonlm(k))
     if (nltype(k) .ne. -96) go to 7031
     n5 = nonlad(k)
     n6 = n5 + 1
     n7 = n5 + 2
     n8 = n5 + 3
     write (unit = lunit(7), fmt = 7020) iold, bus(n1), bus(n2), vchar(n7), cchar(n8)
     n9 = int (cchar(n6))
     n11 = int (cchar(n7))
     write (unit = lunit(7), fmt = 7018) n9, n11, (vchar(ipp), ipp = n5, n8)
7018 format (2i10, 4e15.8)
     n12 = n5 + 4
     n13 = n5 + 5
     n14 = int (cchar(n12))
     write (unit = lunit(7), fmt = 7026) n14, vchar(n12), vchar(n13), gslope(n12), gslope(n13)
7026 format (i10 , 4e15.8)
     if (n10 .eq. 0) cycle
     write (unit = lunit(6), fmt = 7021) k, bus(n1), bus(n2), vchar(n7), cchar(n8)
     write (unit = lunit(6), fmt = 7019) n9, n11, (vchar(ipp), ipp = n5, n8)
7019 format (1x, i10, 2x, i10, 4(3x, e15.8))
     write (unit = lunit(6), fmt = 7027)  n14, vchar(n13), vchar(n14), gslope(n12), gslope(n13)
7027 format (1x, i10 , 4(3x, e15.8))
     cycle
7031 a = curr(k)
     if (a .eq. 0.) cycle
     write (unit = lunit(7), fmt = 7020) iold, bus(n1), bus(n2), a
     if (n10 .ne. 0) write (unit = lunit(6), fmt = 7021) bus(n1), bus(n2), a
  end do
9207 if (kswtch .eq. 0) go to 4020
  if (n10 .ne. 0) write (unit = lunit(6), fmt = 7348)
7348 format (/, ' Status variables for switches follow.', /, 1x,  'bus(l)', 1x, 'bus(m)', 3x, 'kpos(k)',  2x, ' kode(l)', 2x, ' kode(m)', 14x, 'tclose', 14x, 'adelay', 14x, 'energy', 16x, 'crit')
  do k = 1, kswtch
     l = iabs (kmswit(k))
     kn1 = lswtch + k
     m = iabs (kmswit(kn1))
     npl = nextsw(k)
     if (npl .ne. 0) npl = 87
     write (unit = lunit(7), fmt = 4003) bus(l), bus(m), kpos(k), kode(l), kode(m), tclose(k), adelay(k), energy(k), crit(k), npl
4003 format (' 5', 2a6, 3i4, 4e13.6, /, 14x, i4)
     if (n10 .eq. 0) cycle
     write (unit = lunit(6), fmt = 4004) bus(l), bus(m), kpos(k), kode(l), kode(m), tclose(k), adelay(k), energy(k), crit(k), nextsw(k)
4004 format (1x, a6, 1x, a6, 3i10, 4e20.10, /, 14x, i10)
  end do
4020 go to 9800
  !     code below is just for  'statistics'  or  'systematic'  runs. ----
605 nstat = k
  do l = 1, nstat
     ndx1 = 2 * lsiz12  +  l
     if (-xmax(ndx1) .le. xmax(l)) cycle
     xmax(l) = xmax(ndx1)
     ndx3 = lsiz12 + l
     ndx2 = ndx1 + lsiz12
     xmax(ndx3) = xmax(ndx2)
  end do
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 8903) isw, nstat, knt, nenerg, (xmax(i), i = 1, nstat)
8903 format (' isw, nstat, knt, nenerg = ', 4i10, /, ' (xmax(i), i=1, nstat) =', /, 6(5x,e15.8))
  knt = knt + 1
  write (unit = lunit(9)) (xmax(l), l = 1, nstat)
  if (begmax(1) .eq. 0.0) go to 627
  if (nstat .le. 9) go to 609
  if (nenerg .lt. 0) go to 612
  if (nswtpe .le. 1) go to 1610
  write (unit = lunit(6), fmt = 1510) angle, angtpe, (xmax(l), l = 1, ll9)
1510 format (1x, f6.2, 1x, f6.2, 1x, 9e13.6)
  go to 611
1610 write (unit = lunit(6), fmt = 617) angle, (xmax(l), l = 1, ll9)
611 write (unit = lunit(6), fmt = 618) (xmax(l), l = ll10, nstat)
  go to 624
612 write (unit = lunit(6), fmt = 618) (xmax(l), l = 1, ll9)
  go to 611
609 if (nenerg .lt. 0) go to 614
  if (nswtpe .le. 1) go to 1620
  write (unit = lunit(6), fmt = 1510) angle, angtpe, (xmax(l), l = 1, nstat)
  go to 624
1620 write (unit = lunit(6), fmt = 617) angle, (xmax(l), l = 1, nstat)
  go to 624
614 write (unit = lunit(6), fmt = 618) (xmax(l), l = 1, nstat)
617 format (1x, f9.4, 5x, 9e13.6)
618 format (15x, 9e13.6)
624 ndx1 = lsiz12 + 1
  ndx2 = lsiz12 + nstat
  if (begmax(1) .gt. 0.0) write (unit = lunit(6), fmt = 8003) (xmax(l), l = ndx1, ndx2)
627 if (knt .gt. iabs (nenerg)) go to 610
  lastov = nchain
  nchain = 12
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  Exit "over20".')
  go to 99999
610 write (unit = lunit(6), fmt = 654)
654 format (//, 1x)
  lunit(5) = mtape
  !     reset numdcd counter if it is not a omit base case
  if (lstat(15) .ne. intinf) numdcd = numdcd - ipntv(11)
  write (unit = lunit(6), fmt = 659)
659 format (1x, 131('-'))
  write (unit = lunit(6), fmt = 616) nenerg
616 format (" Simulation of all  'nenerg' =", i4, '  energizations is now complete, and the EMTP is ready to begin statistical', /, ' processing of the voltage solutions.   But before doing so, two cautions about discretization of continuous', /, ' variables are probably appropriate to mention at this point.', /, 5x, '1.  The switch closing times which are printed along with peak voltages of each energization are only desired', /, 9x, 'times, just as with all such times which are punched on data cards which define switches upon data input.   Since')
  write (unit = lunit(6), fmt = 644)
644 format (9x, "time is discretized in multiples of time-step-size 'deltat' ,   actual closure will occur at the first discrete", /, 9x,  'step which does not precede the requested time.', /, 5x, '2.  In the tabulation of voltage distributions which follow, an arbitrary decision had to be made concerning the', /, 9x, "column which is labeled  'frequency' .    The continuous variable voltage has been discretized, divided into", /, 9x, "compartments of size  'aincr'  (read from columns 25-32 of the special statistics misc. data card).   For an entry")
  write (unit = lunit(6), fmt = 648)
648 format (9x, "Which is printed in a row marked as being for voltage  'vrow' ,   the associated compartment contains all", /, 9x, "voltages  'v'  which satisfy       vrow  .le.  v  .lt.  (vrow + aincr)  .", /, 5x, '3.  floating-point counting (t = t + deltat) is used to increment time.   Switching times which are an exact multiple', /, 9x, 'of the time-step thus are ambiguous;  different computers may switch one step later, then.')
  if (ltdelt .le. 0) go to 7314
  write (unit = lunit(6), fmt = 7305)
7305 format (//, 1x)
  write (unit = lunit(6), fmt = 7304)
7304 format (' ***********************************************************   Look, look   ************************************************************')
  write (unit = lunit(6), fmt = 7309) ltdelt
7309 format (/, ' During this run, a total of', i4,  '  random switch closings less than time zero were generated by the random  ',/, ' number generator.   But the EMTP has no way of handling such closures.   All such illegal closing times were converted  ', /, ' to time zero (they should show up in the printout that way) for simulation purposes.   The implications of this   ', /, ' modification should be understood by the user.   If in any doubt, the user is strongly advised to seek experienced   ',/, ' counsel on this subject. ', /, 1x)
  write (unit = lunit(6), fmt = 7304)
  write (unit = lunit(6), fmt = 7305)
7314 write (unit = lunit(6), fmt = 659)
  flstat(9) = d1
  flstat(10) = d2
  lastov = nchain
  nchain = 29
  flstat(7) = flstat(7) + d1
  flstat(8) = flstat(8) + d2
  if (iprsup .ge. 1) write (lunit(6), 4568)
  go to 99999
9800 if (m4plot .ne. 1) go to 9810
  if (kbase .eq. 1) go to 9810
  !     VAX simulator return to "over16" after table-saving:
  lastov = nchain
  nchain = 16
  if (iprsup .ge. 1) write (lunit(6), 4568)
  go to 99999
9810 if (lastov .gt. nchain) go to 9850
  flstat(7) = flstat(7) + d1
  flstat(8) = flstat(8) + d2
9850 lastov = nchain
  nchain = 31
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
99999 return
end subroutine over20

!
! subroutine katalg.
!

subroutine katalg
  use comlock
  use blkcom
  use labcom, only : karray
  use dekspy
  implicit none
  !     VAX-11/780  installation-dependent module which is used
  !     to honor the miscellaneous data card request for EMTP table
  !     saving, for later  "start again"  usage.   A call to the
  !     universal module  "tables"  actually dumps memory onto disk.
  !     Logic associated with other than  memsav=0  or  memsav=1
  !     can generally be ignored by other systems;  it applies
  !     only to rtm (real time monitor) use of BPA VAX.
  character(132) :: ansi132
  integer(4) :: j
  !
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2467) icat, memsav, lunit(2), ltlabl
2467 format (/, ' Enter  "katalg" .    icat  memsav  lunit2  ltlabl', /, 18x, 10i8)
  if (memsav .eq. 0) go to 9800                             ! no table moving at all
  ! if interactive emtp control, then
  if (m4plot .eq. 1) tmax = fltinf                          ! make end-time of simulation infinite
  if (memsav .eq. 1016)   go to 2634                        ! "restore"   use
  write (unit = lunit(6), fmt = 2472)
2472 format (/, 20x, '----- "memsav = 1  represents', ' request for table dumping on disk.')
  close (unit = lunit(2))
  open (unit = lunit(2), status = 'new', form = 'unformatted', file = 'tptables.bin')
  call tables
  write (unit = lunit(2)) locker
  rewind 79
  do j = 1, 9999
     read (unit = 79, fmt = 2474, end = 2479) ansi132
2474 format (a132)
!2476 write (unit = lunit(2)) ansi132
     write (unit = lunit(2)) ansi132
  end do
2479 close (unit = lunit(2), status = 'keep')
  karray(1) = indbuf
  karray(2) = mflush
  karray(3) = newvec
  write (unit = lunit(6), fmt = 2483) ltlabl
2483 format (26x, 'Successful saving of EMTP tables as file  "tptables.bin" .    ltlabl  =', i8)
  if (memsav .eq. 1) go to 9800                             ! exit module (no spy)
  go to 9700                                                ! exit module after "emtspy" linkage set
  !     following code services  "restore"  of  "emtspy".  memory
  !     is restored from dumping of previous "sleep" or "save":
2634 write (unit = lunit(6), fmt = 2637) ltlabl
2637 format (' Begin EMTP table restoration.  ltlabl =', i7,  ' .  Wait for completion.')
  close (unit = lunit(2))
  open (unit = lunit(2), status = 'old', form = 'unformatted', file = 'tptables.bin')
  call tables
  close (unit = lunit(2), status = 'keep')
  !     next, for rtm use, restore key "ovdrivkom.for" variables:
  indbuf = karray(1)
  mflush = karray(2)
  newvec = karray(3)
  write (unit = lunit(6), fmt = 2642)
2642 format (' Restoration complete, user can now begin  depositing  EMTP  variables via  spy .')
  memsav = 0        !  ?????????????  table changes done.
9700 kbreak = 1     ! lock flag for "emtspy" dialogue in "over16"
9800 return
end subroutine katalg

!
! subroutine emtspy.
!

subroutine emtspy
  use blkcom
  use dekspy
  use smach
  implicit none
  !     Module of interactive EMTP only, which services "emtspy".
  !     If no interactive use, convert to dummy module ("return").
  integer(4) :: n18
  !
  data n18 / 0 /
  n18 = n18 + 1
  if (n18 .lt. maxflg) go to 9008
  n18 = 0
  if (iprspy .lt. 9) go to 5611
  write (unit = munit6, fmt = 5608) kbreak, kwtspy, nbreak, lockbr, nchain, jjroll, t, tbreak
5608 format (' Top "emtspy".  kbreak, kwtspy, nbreak, lockbr, jjroll, nchain =', 6i4, '    t, tbreak =', 2e14.5)
  call window
5611 if (jjroll .gt. 0) go to 5632
  if (kbreak .eq. 1) go to 5613
  if (lockbr .eq. 1) go to 5623
  if (t .lt. tbreak) go to 5623
  if (nchain .ne. nbreak) go to 5623
  !     ok, now we service "break" of spy (time and place to do it):
  tbreak = 8877.e33
5613 lockbr = 1
  write (unit = munit6, fmt = 5615) nchain, t
5615 format (//, '    Start "break" service in "emtspy".   nchain =', i3, '   t =', e14.5)
  call window
  if (kfile5 .eq. 1) go to 6258
5617 write (unit = prom80, fmt = 5618)
5618 format (' spy:')
  call prompt
5623 if (ksmspy(3) .eq. 1) go to 5632
  call flager
  if (kwtspy .eq. 1) go to 5632
  if (kfile5 .eq. 1) go to 6258
  go to 9000
5632 kwtspy = 0
  call spying
  if (jjroll .gt. 0) go to 9000
  if (lockbr .eq. 1) go to 5623
  go to 9000
  !     entry point for pre-defined spy commands ($spy or "@"):
6258 if (komadd .eq. 0) go to 6260
  komadd = komadd + 1
  buff77 = file6(komadd)
  if (buff77(1 : 3) .eq. 'eof') go to 6278
  go to 6264
6260 read (unit = munit5, fmt = 6261, end = 6274) buff77
6261 format (a80)
6264 if (kfile5 .eq. 1) call percnt (buff77, 80)
  if (kilper .ne. 0) go to 6274
!6266 call spying
  call spying
  if (lockbr .eq. 1) go to 6258
  go to 9000
  !     end-of-file during disk read, so switch to keyboard input:
6274 close (unit = munit5)
6278 munit5 = muntsv(1)
  kfile5 = 0
  kilper = 0
  if (muntsv(2) .ne. 2288) go to 5617
  muntsv(2) = 0
9000 if (iprspy .lt. 1) go to 9008
  write (unit = munit6, fmt = 9007) kbreak, nchain, lastov, m4plot
9007 format (' Exit "emtspy".  kbreak, nchain, lastov, m4plot =', 4i6)
  call window
9008 return
end subroutine emtspy

!
!     subroutine spying.
!

subroutine spying
  use blkcom
  use labcom
  use smach
  use tacsar
  use dekspy
  use indcom
  use bcdtim
  use bcddat
  implicit none
  !     Module of interactive EMTP only, which services "emtspy".
  !     This is the principle module, called by "emtspy".
  character(4) :: spytim(2), spdate(2)
  character(8) :: chard7
  integer(4) :: i, icomm, idmxxx, ind, intype, ios
  integer(4) :: j, jwdsav
  integer(4) :: k, k1, k2, k3, kansav, khead, kp
  integer(4) :: l
  integer(4) :: m
  integer(4) :: n1, n2, n4, n5, n6, n7, n8, n9, n10, n12, n13, n14, n15, n16, n17
  integer(4) :: n18, n22, n23, n24, n26, n27, n33, nchd2, nd13, num2
  integer(4) :: num3, num5, numask, numbco, numbrn
  integer(4) :: numnam
  real(8) :: d1, d2, d3, d4, d5, d6, d8, d13, d34
  real(8) :: tim1rp, tim2rp, twhen
  real(8) :: val1rp, val2rp
  real(8) :: xl
  !
  if (iprspy .lt. 1) go to 31006
  write (unit = munit6, fmt = 1006) nchain, jjroll, kbreak, lockbr, nexmod, buff77(1 : 20)
1006 format ('  Enter "spying".  nchain, jjroll, kbreak, lockbr, nexmod =', 5i5, '    buff77(1 : 20) =', a20)
  call window
  write (unit = *, fmt = *) ' Top spying, ksmspy(1 : 3) =', ksmspy
31006 if (buff77(1 : 4) .ne. 'spy ') go to 51006
  nexmod = 0
  ksmspy(1) = 2
  ksmspy(3) = 0
51006 if (nexmod .ne. 2) go to 1007
  nexmod = 0
  !go to nextsn
  select case (nextsn)
  case (1315)
     go to 1315

  case (1485)
     go to 1485

  case (1842)
     go to 1842

  case (2048)
     go to 2048

  case (2101)
     go to 2101

  case (2126)
     go to 2126

  case (2139)
     go to 2139

  case (2436)
     go to 2436

  case (2913)
     go to 2913

  case (2969)
     go to 2969

  case (2979)
     go to 2979

  case (2983)
     go to 2983

  case (3128)
     go to 3128

  case (3180)
     go to 3180

  case (3191)
     go to 3191

  case (3204)
     go to 3204

  case (3307)
     go to 3307

  case (3573)
     go to 3573

  case (3591)
     go to 3591

  case (3609)
     go to 3609

  case (3610)
     go to 3610

  case (3613)
     go to 3613

  case (3629)
     go to 3629

  case (3649)
     go to 3649

  case (3681)
     go to 3681

  case (31269)
     go to 31269

  case (32981)
     go to 32981

  case (33098)
     go to 33098

  case (43137)
     go to 43137

  end select
1007 if (nexmod .eq. 3) go to 8500
  if (nexmod .eq. 1) go to 3208
  if (nexmod .eq. 7) go to 1320
  if (nexmod .ge. 4) go to 1319
  if (jjroll .gt. 0) go to 1520
  if (kbrser .ne. 2) go to 1009
  jword = 52
  go to 8500
1009 memkar = location (kar1)
  if (kfile5 .ne. 2) go to 1240
  kfile5 = 0
  go to 31269
  !1240 assign 31269 to nextsn
1240 nextsn = 31269
  write (unit = prom80, fmt = 1260)
1260 format (' spy:')
51269 go to 9800
31269 answ80 = buff77
  if (iprspy .le. 9) go to 39843
  write (unit = munit6, fmt = 29843) answ80
29843 format ('  --- Just read answ80:', a80)
  call window
39843 if (answ80(1 : 4) .eq. 'type') go to 2506
  if (answ80(1 : 2) .ne. 'c ') go to 1275
  if (kfile5 .eq. 1 .and. kverfy .eq. 0) go to 51269
  if (icomm .ne. 0) go to 51269
  write (unit = munit6, fmt = 1273) answ80
1273 format (' Comment:', a80)
  call window
  go to 51269
1275 if (answ80(1 : 1) .ne. '@') go to 1289
  !     $$$$$$$  key word no. 19:  "@?"       $$$$  $$$$  $$$$  $$$$  $$$$
  !     now process user request for disk-file connection to keyboard (5):
  if (kfile5 .ne. 1) go to 1278
  write (unit = munit6, fmt = 1277)
1277 format ('  === Reject  "@"  usage;  one such file is presently connected.  Try again.')
  call window
  go to 1240
1278 if (answ80(2 : 2) .ne. '/') go to 51278
  answ80(1 : 2) = '  '
  call frein1 (answ80, komadd)
  komadd = komadd - 1
  n13 = 6
  go to 1276
51278 ansi32 = 'inclspy .dat                    '
  komadd = 0
  n26 = 2
  go to 2511
  !     ok, so file is legal;  next check for argument list;  extract any:
1276 n6 = n13 + 1
  maxarg = 0
  do j = n6, 80
     if (answ80(j : j) .ne. ' ') go to 4535
  end do
  go to 1271
4535 maxarg = 0
  do idmxxx = 1, 10
     m = 0
     ansi8(1 : 8) = blan80(1 : 8)
     do l = 1, 80
        if (n6 .gt. 80) go to 4542
        if (iprspy .lt. 1) go to 8900
        write (unit = munit6, fmt = 8899) l, m, n6, answ80(n6 : n6)
8899    format (' Next character. l, m, n6, answ80(n6) =', 3i5, 1x, a1)
        call window
8900    if (m .eq. 0 .and. answ80(n6 : n6) .eq. '(') answ80(n6 : n6) = ' '
        if (answ80(n6 : n6) .eq. ')') go to 4544
        if (answ80(n6 : n6) .eq. ' ') go to 4541
        if (answ80(n6 : n6) .eq. ',') go to 4544
        m = m + 1
        if (m .le. 8) go to 4540
        write (unit = munit6, fmt = 4539) idmxxx
4539    format ('  ??? Argument number',  i3, '  of  "@?"  has over 8 digits.  Try again.')
        call window
        go to 1240
4540    ansi8(m : m) = answ80(n6 : n6)
        if (ansi8(m : m) .eq. '#') ansi8(m : m) = ' '
4541    n6 = n6 + 1
     end do
     !     no ")" on "@?"  argument list means we ran through column 80:
4542 if (m .eq. 0) go to 1271
     !     one or more non-blank characters since last "," is final argument:
     maxarg = maxarg + 1
     texpar(maxarg) = ansi8
     if (iprspy .lt. 1) go to 1271
     write (unit  = munit6, fmt = 4543) maxarg
4543 format (' maxarg =', i8)
     call window
     go to 1271
4544 maxarg = maxarg + 1
     read (unit = ansi8, fmt = 1360) texpar(maxarg)
1360 format (15a1)
     if (answ80(n6 : n6) .eq. ')') go to 1271
!4549 n6 = n6 + 1
     n6 = n6 + 1
  end do
  write (unit = munit6, fmt = 4551)
4551 format ('   ? ? ?  Illegal  "@?"  use.   Over 10 arguments.   Try again.')
  call window
  go to 1240
  !     any arguments have been successfully found;  now use file
1271 kfile5 = 1
  itexp = 0
  go to 51269
  !       former eof s.n. 1286 with file close and other resetting
1289 if (answ80(1 : 8) .ne. blan80(1 : 8)) go to 1293
  !     blank (just <cr>) represents request for repeat "examine"
  go to 1520
  !     check for  "%%%%%%%%"  field of  "@?"  arguments;  replace any:
  !     next check card image for one of the "numkey" key-words:
1293 n13 = 0
  if (answ80(1 : 5) .ne. 'wake4') go to 1296
  n13 = 1
  answ80(5 : 5) = ' '
1296 do jword = 1, numkey
     if (answ80(1 : 8) .eq. spykwd(jword)) go to 1307
  end do
!1302 continue
  !     we reach here if illegal response to "spy:" prompt has been read:
1303 write (unit = munit6, fmt = 1304) answ80(1 : 8)
1304 format ('   ? ? ?  Illegal spy command  "',  a, '" .   Try again  .....')
  call window
  go to 1240
1307 if (iprspy .lt. 1) go to 1309
  write (unit = munit6, fmt = 1308) jword
1308 format ('   Keyword found is  jword =', i8)
  call window
1309 jwdsav = jword
  if (jword .gt. 63) go to 71309
  select case (jword)
  case (1)     ! heading
     go to 1500

  case (2)     ! stop
     go to 9000

  case (3)     ! plot
     go to 1319

  case (4)     ! help
     go to 1311

  case (5)     ! examine
     go to 1337

  case (6)     ! deposit
     go to 1460

  case (7)     ! switch
     go to 8500

  case (8)     ! append
     go to 1320

  case (9)     ! save
     go to 2206

  case (10)    ! restore
     go to 2317

  case (11)    ! go
     go to 2402

  case (12)    ! echo
     go to 3673

  case (13)    ! find
     go to 2040

  case (14)    ! list
     go to 2092

  case (15)    ! spy
     go to 1240

  case (16)    ! break
     go to 2124

  case (17)    ! when
     go to 2430

  case (18)    ! comment
     go to 2486

  case (19)    ! @?
     go to 1303

  case (20)    ! roll
     go to 1424

  case (21)    ! type?
     go to 1303

  case (22)    ! verify
     go to 2494

  case (23)    ! files
     go to 2563

  case (24)    ! sleep
     go to 2591

  case (25 : 26, 42 : 56, 61 : 62)    ! source, edit, rlc, width, bus, size, limit, iout, node, nonlin, space, lunit(4), series, lock, [y], [f], noroll, choice, tacs
     go to 8500

  case (27)    ! wake
     go to 2604

  case (28)    ! language
     go to 8500

  case (29)    ! catalog
     go to 2907

  case (30)    ! begin
     go to 2926

  case (31)    ! step
     go to 2937

  case (32)    ! debug
     go to 2964

  case (33)    ! data
     go to 2974

  case (34)    ! ramp
     go to 3095

  case (35)    ! time
     go to 3153

  case (36)    ! tek
     go to 3174

  case (37)    ! branch
     go to 3256

  case (38)    ! yform
     go to 3357

  case (39)    ! noy
     go to 3381

  case (40)    ! factor
     go to 3394

  case (41)    ! nof
     go to 3406


     go to 8500

  case (57)    ! open
     go to 3568

  case (58)    ! close
     go to 3584

  case (59)    ! sm
     go to 3606

  case (60)    ! honk
     go to 3623

  case (63)    ! wait
     go to 3644
  end select

!  go to ( &
!      heading  stop   plot    help    examine deposit switch         (1-7)
!       1500,   9000,   1319,   1311,   1337,   1460,   8500, &
!      append   save   restore go    echo   find   list               (8-14)
!       1320,   2206,   2317,   2402,   3673,   2040,   2092, &
!      spy     break   when    comment @?      roll    type?          (15-21)
!       1240,   2124,   2430,   2486,   1303,   1424,   1303, &
!      verify  files   sleep   source  edit    wake    language       (22-28)
!       2494,   2563,   2591,   8500,   8500,   2604,   8500, &
!      catalog begin   step    debug   data    ramp    time           (29-35)
!       2907,   2926,   2937,   2964,   2974,   3095,   3153, &
!      tek     branch  yform   noy     factor  nof     rlc            (36-42)
!       3174,   3256,   3357,   3381,   3394,   3406,   8500, &
!      width   bus     size    limit   iout    node    nonlin         (43-49)
!       8500,   8500,   8500,   8500,   8500,   8500,   8500, &
!      space   lunit(4)  series  lock    [y]     [f]     noroll         (50-56)
!       8500,   8500,   8500,   8500,   8500,   8500,   8500, &
!      open    close   sm      honk    choice  tacs    wait           (57-63)
!       3568,   3584,   3606,   3623,   8500,   8500,   3644), jword
  call stoptp
71309 n14 = jword - 63
!  go to ( &
!      v-i                                                   (64-64)
!       8500 ), n14
  select case (n14)
  case (1)
     go to 8500
  end select
  call stoptp
  !     $$$$$$$  key word no.  4:  "help"     $$$$  $$$$  $$$$  $$$$  $$$$
1311 write (unit = munit6, fmt = 11312)
11312 format ('    Available key-word responses to the  "spy:"  prompt are as follows:')
  call window
  do j = 1, numkey, 8
     n15 = j + 7
     if (n15 .gt. numkey) n15 = numkey
     write (unit = munit6, fmt = 1313) (spykwd(k), k = j, n15)
1313 format (5x, 8a9)
     call window
  end do
!21313 continue
  jword = jwdsav
21314 write (unit = prom80, fmt = 1314)
1314 format ('       key-word of interest (all, <cr>,', ' top, bot, back):')
!  assign 1315 to nextsn
  nextsn = 1315
  go to 9800
1315 spycd2 = buff77(1 : 35)
  if (spycd2(1 : 4) .eq. '    ') go to 11315
  if (spycd2(1 : 4) .eq. 'end ') go to 1240
  if (spycd2(1 : 4) .ne. 'bot ') go to 51315
  jword = numkey
  go to 1317
51315 if (spycd2(1 : 4) .ne. 'back') go to 61315
  jword = jword - 1
  if (jword .gt. 0) go to 1317
  jword = numkey
  write (unit = munit6, fmt = 31315)
31315 format ('      //// Wrap around, beginning to end ////' )
  call window
  go to 1317
61315 if (spycd2(1 : 4) .eq. 'top ') go to 41315
  if (spycd2(1 : 4) .ne. 'next') go to 21315
11315 jword = jword + 1
  if (jword .le. numkey) go to 1317
  if (spycd2(1 : 4) .eq. 'all ') go to 21314
  write (unit = munit6, fmt = 81315)
81315 format ('      //// Wrap around, end to beginning ////' )
  call window
41315 jword = 1
  go to 1317
21315 jword = 1
  if (spycd2(1 : 4) .eq. 'all ') go to 1317
  do jword = 1, numkey
     if (spycd2(1 : 8) .eq. spykwd(jword)) go to 1317
  end do
  write (unit = munit6, fmt = 1760) spycd2
  call window
  go to 1318
1317 n17 = kbegtx(jword)
  n18 = kbegtx(jword + 1) - 1
  do j = n17, n18
     munit6(1 : 81) = ' ' // texspy(j)
     call window
  end do
!91315 continue
  if (jword .ne. 4) go to 61317
  write (unit = munit6, fmt = 11312)
  call window
  do j = 1, numkey, 8
     write (unit = munit6, fmt = 1313) (spykwd(k), k = j, j + 7)
     call window
  end do
61317 jwdsav = jword
  if (spycd2(1 : 4) .ne. 'all ') go to 1318
  call quiter
  if (kwtspy .eq. 0) go to 11315
  kwtspy = 0
1318 go to 21314
  !     $$$$$$$  key word no.  3:  "plot"     $$$$  $$$$  $$$$  $$$$  $$$$
1319 call rtmplt
  if (nexmod .ge. 4) go to 9803
  go to 1240
  !     $$$$$$$  key word no.  8:  "append"   $$$$  $$$$  $$$$  $$$$  $$$$
1320 call append
  if (nexmod .eq. 7) go to 9803
  go to 1240
  !     $$$$$$$  key word no.  5:  "examine"  $$$$  $$$$  $$$$  $$$$  $$$$
1337 numex = 0
  khead = 2
  heding = ' '
  go to 1460
  !     encode heading buffer with variable request now known:
1340 numex = numex + 1
  intout(numex) = intype
  if (nchd2 .lt. 6) spycd2(nchd2 + 1 : 6) = blan80(nchd2 + 1 : 6)
  n14 = n1
1354 if (n14 .gt. n2) go to 1460
  heding(khead : khead + 14) = blan80(khead : khead + 14)
  !     reals or vectors occupy 15 cols, so can be shifted:
  if (intype .eq. 0 .or. ivec(ind) .eq. 1 .or. n2 .ne. 1) khead = khead + 2
  !     real scalars can be shifted 3 more columns:
  if (intype .eq. 0 .and. ivec(ind) .eq. 0 .and. n2 .eq. 1) khead = khead + 3
  !     next see if symbol can be profitably centered in 6-column slot:
  n15 = 6
  if (nchd2 .le. 4) go to 1373
  if (nchd2 .eq. 6) go to 1377
  if (heding(khead - 1 : khead - 1) .eq. ' ') go to 1377
  n17 = 1
  go to 1374
1373 n17 = (6 - nchd2) / 2
1374 khead = khead + n17
  n15 = n15 - n17
1377 heding(khead : khead + 5) = spycd2(1 : 6)
  khead = khead + n15
  !     real scalars end up with four blanks on the right:
  if (intype .eq. 0 .and. ivec(ind) .eq. 0 .and. n2 .eq. 1) khead = khead + 4
  locout(numex) = ind
  imin(numex) = n1
  imax(numex) = n2
  !     if ( ivec(ind)  .eq.  0 )   go to 1460    ! original record.
  !     next we want to execute s.n. 1394 if subscripting is involved:
  if (ivec(ind) .eq. 1) go to 1394
  if (n1 .eq. 1 .and. n2 .eq. 1) go to 1460
1394 write (ansi8, 1400) n14
1400 format ('(', i4, ') ')
  heding(khead : khead + 6) = ansi8(1 : 7)
  khead = khead + 7
  n14 = n14 + 1
  go to 1354
  !     $$$$$$$  key word no. 20:  "roll"     $$$$  $$$$  $$$$  $$$$  $$$$
1424 jjroll = 1
  go to 1496
  !     $$$$$$$  key word no.  6:  "deposit"  $$$$  $$$$  $$$$  $$$$  $$$$
1460 write (unit = prom80, fmt = 1480)
1480 format ('  Send EMTP variable:')
  !  assign 1485 to nextsn
  nextsn = 1485
  go to 9800
1485 spycd2 = buff77(1 : 35)
  if (spycd2(1 : 1) .ne. 'c' .or. spycd2(2 : 2) .ne. ' ') go to 1491
  if (icomm .ne. 0) go to 1460
  write (unit = munit6, fmt = 1487) spycd2
1487 format (' Comment:', a35)
  call window
  go to 1460
1491 if (spycd2(1 : 4) .ne. 'end ') go to 1720
  !     if processing "deposit" = spykwd(6), then back to "spy:":
1496 if (answ80(1 : 8) .eq. spykwd(6)) go to 1240
  !     if internal "deposit" issued from "ramp"= spykwd(34), return:
  if (answ80(1 : 8) .eq. spykwd(34)) go to 3138
  !     if internal "deposit", issued from "restore", then back to "spy:":
  if (answ80(1 : 8) .eq. spykwd(10)) go to 1240
  !     ok, this is "examine";  first, blank unused right portion:
  if (khead .lt. 132) heding(khead + 1 : 132) = ' '
  !     if disk file is connected to unit 5, we want return to  "spy:" :
  if (kfile5 .eq. 1) go to 1240
  !     $$$$$$$  key word no.  1:  "heading"  $$$$  $$$$  $$$$  $$$$  $$$$
1500 munit6 = heding
  call window
  !     following code outputs table dumping in accord with earlier
  !     setup using  "examine"  specification.
1520 call examin
  if (jjroll .eq. 0) go to 1682
  jjroll = jjroll + 1
  if (jjroll .le. 2) go to 1669
  call quiter
  if (kwtspy .eq. 0) go to 1523
  jjroll = 0
  go to 1240
1523 if (outsav .eq. outlin) go to 1684
1669 outsav = outlin
  !     output line of "examine" is all built;  display it:
1682 if (kfile5 .ne. 1 .and. jjroll .eq. 0 .and. kolout .le. 81) go to 1685
  munit6 = outlin
  call window
  prom80(1 : 8) = blan80(1 : 8)
1684 if (jjroll .gt. 0) go to 9833
  go to 1714
1685 prom80 = outlin(1 : kolout - 1)
  !1714 assign 31269 to nextsn
1714 nextsn = 31269
  go to 9800
  !     process EMTP symbol name (spycd2) which has just been read;
1720 nchd2 = 0
  n13 = 0
  do j = 1, 8
     if (spycd2(j : j) .eq. '(') go to 1726
     if (spycd2(j : j) .eq. ',') go to 1726
     if (spycd2(j : j) .ne. ' ') go to 1721
     if (nchd2 .eq. 0) nchd2 = j - 1
     cycle
1721 if (spycd2(j : j) .ne. '=') cycle
     n13 = j
     if (nchd2 .eq. 0) nchd2 = j - 1
  end do
  n1 = 1
  n2 = 1
  j = n13
  if (n13 .eq. 0) j = 7
  go to 1735
1726 if (nchd2 .eq. 0) nchd2 = j - 1
  n9 = 0
  do k = j, 20
     if (spycd2(k : k) .ne. '-') go to 1727
     n12 = -1
     cycle
1727 if (spycd2(k : k) .eq. ' ') cycle
     if (spycd2(k : k) .ne. '=') go to 21727
     n13 = k
     go to 1734
21727 do i = 1, 10
        if (spycd2(k : k) .eq. digit(i)) go to 1730
     end do
     if (n9 .gt. 0) limarr(n9) = limarr(n9) * n12
     !     non-digit means we initialize for next number to be built:
     n9 = n9 + 1
     n12 = 1
     limarr(n9) = 0
     cycle
1730 if (i .eq. 10) i = 0
     limarr(n9) = 10 * limarr(n9) + i
  end do
1734 n1 = limarr(1)
  n2 = limarr(2)
  if (n9 .le. 2) n2 = n1
1735 spycd2(j : 8) = blan80(j : 8)
  !     next identify the emtp name involved in this request:
!1738 do ind = 1, numsym
  do ind = 1, numsym
     if (spycd2(1 : 8) .eq. symb(ind)) go to 1780
  end do
!1740 continue
  write (unit = munit6, fmt = 1760) spycd2
1760 format ('   ? ?  Sorry, no symbol match for  "', a6, '" .')
  call window
  go to 1460
1780 intype = 0
  if (spycd2(1 : 1) .eq. 'i') intype = 1
  if (spycd2(1 : 1) .eq. 'j') intype = 1
  if (spycd2(1 : 1) .eq. 'k') intype = 1
  if (spycd2(1 : 1) .eq. 'l') intype = 1
  if (spycd2(1 : 1) .eq. 'm') intype = 1
  if (spycd2(1 : 1) .eq. 'n') intype = 1
  if (ivec(ind) .eq. 1 .or. n2 .le. n1) go to 31807
  write (unit = munit6, fmt = 1807)
1807 format ('    Note :  this is a vector dump using a scalar EMTP variable.')
  call window
31807 if (answ80(1 : 8) .eq. spykwd(5)) go to 1340
  if (answ80(1 : 8) .eq. spykwd(34)) go to 3134
  !     for the case of "deposit", the user-desired value must be read:
  if (n13 .eq. 0) go to 1837
  if (iascii(ind) .eq. 1) go to 1819
  n13 = n13 + 1
  n22 = 0
  do j = n13, 35
     if (spycd2(j : j) .eq. ' ') cycle
     n22 = n22 + 1
     if (n22 .le. 20) go to 1808
     write (unit = munit6, fmt = 51807)
51807 format (' Sorry, spy buffer overflow.  Try again ....')
     call window
     go to 1837
1808 bytbuf(n22 : n22) = spycd2(j : j)
  end do
  if (n22 .lt. 20) bytbuf(n22 + 1 : 20) = blan80(n22 + 1 : 20)
  !     following code is for alphanumeric emtp symbol after "=" :
1819 ansi8(1 : 6) = spycd2(n13 + 1 : n13 + 6)
  if (iprspy .lt. 1) go to 1824
  write (unit = munit6, fmt = 1820) ansi8
1820 format (' ansi8 after alphanumeric transfer =', a)
  call window
1824 go to 1846
  !     no equal sign means that we prompt for separate number:
1837 write (unit = prom80, fmt = 1840)
1840 format (4x, 'New value:')
  !  assign 1842 to nextsn
  nextsn = 1842
  go to 9800
1842 bytbuf = buff77(1 : 20)
  if (iascii(ind) .eq. 0) go to 1844
  ansi8(1 : 6) = bytbuf(1 : 6)
  go to 1846
  !     following code first checks 20-column "bytbuf" working vector
  !     for a legal fortran number;  then free-format write/read gets it:
1844 call numchk (bytbuf, 20, n33)
  if (n33 .eq. 1) go to 1837
  call frefp1 (bytbuf, d4)
1846 call deposi (ind, intype, n1, n2, d4)
  go to 1460
  !     $$$$$$$  key word no. 13:  "find"     $$$$  $$$$  $$$$  $$$$  $$$$
2040 write (unit = munit6, fmt = 2042)
2042 format (1x, '    symbol      word   address      next')
  call window
2043 write (unit = prom80, fmt = 2044)
2044 format ('      index   address    change      symbol :')
  !2050 assign 2048 to nextsn
2050 nextsn = 2048
  go to 9800
2048 spycd2 = buff77(1 : 35)
  if (spycd2(1 : 8) .eq. 'end     ') go to 1240
  if (spycd2(1 : 8) .eq. 'stop    ') go to 1240
  if (spycd2(1 : 8) .ne. blan80(1 : 8)) go to 2052
  write (prom80, 2051)
2051 format ('   ? ? ?  Blank EMTP symbol is clearly wrong.  Try again :')
  go to 2050
  !     first we must see if  "spycd2"  has any wild-card characters:
2052 do i = 1, 8
     if (spycd2(i : i) .eq. '*') go to 2069
  end do
!2053 continue
  !     simple case with no wild-card characters:
  do ind = 1, numsym
     if (spycd2(1 : 8) .eq. symb(ind)) go to 2063
  end do
  write (unit = munit6, fmt = 2088)
  call window
  go to 2050
2063 n4 = locate(ind) - n5
  n5 = locate(ind)
  write (unit = prom80, fmt = 2066) ind, n5, n4
2066 format (1x, 3i10, 4x, ':')
  call prompt
  go to 2050
  !     begin search for case where "d2" has one or more wild cards ("*"):
2069 n4 = 0
  do ind = 1, numsym
     chard7 = symb(ind)
     n7 = 1
     n2 = 1
     !     enter loop which compares  chard2(n2)  against  chard7(n7):
2071 if (spycd2(n2 : n2) .ne. '*') go to 2079
     !     enter code where "*" implies skipping indeterminate number of
     !     characters in  "d7" :
2072 n2 = n2 + 1
     if (n2 .ge. 9) go to 2081
     if (spycd2(n2 : n2) .eq. '*') go to 2072
     if (spycd2(n2 : n2) .eq. ' ') go to 2081
     !     nonblank  spycd2(n2:n2) means we must search "d7" for it:
     do i = n7, 6
        if (chard7(i : i) .eq. spycd2(n2 : n2)) go to 2077
     end do
     go to 2086
2077 n2 = n2 + 1
     n7 = i + 1
     if (n2 .ge. 9) go to 2081
     go to 2071
     !     now we are ready to check characters of two words for match:
2079 if (spycd2(n2 : n2) .ne. chard7(n7 : n7)) go to 2086
     n2 = n2 + 1
     n7 = n7 + 1
     if (n2 .ge. 9) go to 2081
     if (spycd2(n2 : n2) .ne. ' ') go to 2071
     !     blank in "d2" means success;  we have found a match.  now output:
2081 n4 = locate(ind) - n5
     n6 = locate(ind)
     write (unit = munit6, fmt = 2083) ind, n6, n4, chard7
2083 format (1x, 3i10, 6x, a6, ' = name  ')
     call window
2086 call quiter
     if (kwtspy .eq. 0) cycle
     kwtspy = 0
     go to 2040
  end do
  if (n4 .ne. 0) go to 2043
  write (unit = prom80, fmt = 2088)
2088 format ('    Sorry, no matches.   Try again :')
  go to 2050
  !     $$$$$$$  key word no. 14:  "list"     $$$$  $$$$  $$$$  $$$$  $$$$
2092 write (unit = munit6, fmt = 2093) numsym
2093 format ('    row    symbol    word    vector?    ascii?   next', 5x, i4, ' symbols')
  call window
  write (unit = prom80, fmt = 2095)
2095 format ('  number    name   address   (yes=1)   (yes=1)   :')
  n33 = 0
  !2099 assign 2101 to nextsn
2099 nextsn = 2101
  go to 9800
2101 bytbuf = buff77(1 : 20)
  if (bytbuf(1 : 4) .eq. 'end ') go to 1240
  if (bytbuf(1 : 4) .eq. 'stop') go to 1240
  if (bytbuf(1 : 4) .ne. '    ' .and. bytbuf(1 : 4) .ne. 'next') go to 2102
  n1 = n2 + 1
  n2 = n1
  go to 2108
2102 if (bytbuf(1 : 4) .ne. 'top ') go to 2103
  n1 = 1
  n2 = 1
  go to 2109
2103 if (bytbuf(1 : 4) .ne. 'bot ') go to 2105
  n1 = numsym
  n2 = numsym
  go to 2109
2105 if (bytbuf(1 : 4) .ne. 'all ') go to 2107
  n1 = 1
  n2 = numsym
  go to 2109
2107 call frein2 (bytbuf, n1, n2)
2108 if (n1 .le. 0) n1 = 1
  if (n1 .gt. numsym) n1 = numsym
  if (n2 .le. n1) n2 = n1
  if (n2 .gt. numsym) n2 = numsym
2109 do i = n1, n2 - 1
     write (unit = munit6, fmt = 2113) i, symb(i), locate(i), ivec(i), iascii(i)
2113 format (i8, 4x, a6, i9, i8, i10)
     call window
     call quiter
     if (kwtspy .eq. 0) cycle
     kwtspy = 0
     go to 2092
  end do
  write (unit = prom80, fmt = 3217) n2, symb(n2), locate(n2), ivec(n2), iascii(n2)
3217 format (i8, 4x, a6, i9, i8, i10, 4x, ':')
  go to 2099
  !     $$$$$$$  key word no. 16:  "break"    $$$$  $$$$  $$$$  $$$$  $$$$
2124 write (unit = prom80, fmt = 2125)
2125 format ('   Send  t-break  in', ' sec. ("-" means step #):')
  !  assign 2126 to nextsn
  nextsn = 2126
  go to 9800
2126 call frefp1 (buff77, tbreak)
  nbreak = 16
  if (tbreak .gt. 0.0) go to 2142
  if (tbreak .eq. 0.0) go to 2131
  d13 = -tbreak * deltat
  write (unit = munit6, fmt = 2128) deltat, d13
2128 format ('    Ok, using  deltat =', e12.3, '  this gives  t-break =', e13.5)
  call window
  tbreak = d13
  go to 2142
2131 write (unit = prom80, fmt = 2136)
2136 format ('    Send utpf overlay number nchain for the break :')
  !  assign 2139 to nextsn
  nextsn = 2139
  go to 9800
2139 call frein1 (buff77, nbreak)
  if (nbreak .lt. -1 .or. nbreak .gt. 20) go to 2131
  if (nbreak .ne. 16) tbreak = -9876.e33
2142 go to 1240
  !     $$$$$$$  key word no.  9:  "save"     $$$$  $$$$  $$$$  $$$$  $$$$
2206 tmax = t - epsiln
  if (twhen .gt. flzero) tmax = twhen
  memsav = 16
  go to 1240
  !     $$$$$$$  key word no. 10:  "restore"  $$$$  $$$$  $$$$  $$$$  $$$$
2317 tmax = t - epsiln
  if (twhen .gt. flzero) tmax = twhen
  memsav = 1016
  go to 1240
  !     $$$$$$$  key word no. 11:  "go"       $$$$  $$$$  $$$$  $$$$  $$$$
2402 lockbr = 0
  if (iprspy .lt. 1) go to 2409
  write (unit = munit6, fmt = 2407)
2407 format (' "go" acknowledged, set lockbr = 0.')
  call window
2409 go to 1240
  !     $$$$$$$  key word no. 17:  "when"     $$$$  $$$$  $$$$  $$$$  $$$$
2430 write (unit = prom80, fmt = 2433)  twhen
2433 format ('  Supply tmax for "save" & "restore"  [',  f8.4, ' ] :')
  !  assign 2436 to nextsn
  nextsn = 2436
  go to 9800
2436 bytbuf = buff77(1 : 20)
  if (bytbuf(1 : 4) .eq. '    ') go to 2439
  call frefp1 (bytbuf, d34)
  if (d34 .gt. 0.0) twhen = d34
2439 go to 1240
  !     $$$$$$$  key word no. 18:  "comment"  $$$$  $$$$  $$$$  $$$$  $$$$
2486 if (icomm .eq. 0) go to 2491
  icomm = 0
  write (unit = munit6, fmt = 2488)
2488 format ('   Begin displaying comment cards of  "@"  usage.')
  call window
  go to 1240
2491 icomm = 1
  write (unit = munit6, fmt = 2492)
2492 format ('   Stop showing comment cards of  "@"  usage.')
  call window
  go to 1240
  !     $$$$$$$  key word no. 22:  "verify"   $$$$  $$$$  $$$$  $$$$  $$$$
2494 if (kverfy .eq. 0) go to 2497
  kverfy = 0
  write (unit = munit6, fmt = 2495)
2495 format ('   Begin echoing commands of  "@"  file usage.')
  call window
  go to 1240
2497 kverfy = 1
  write (unit = munit6, fmt = 2499)
2499 format ('   Stop echoing commands of  "@"  file usage.')
  call window
  go to 1240
  !     $$$$$$$  key word no. 21:  "type?"    $$$$  $$$$  $$$$  $$$$  $$$$
2506 n16 = 0
  n26 = 5
  ansi32 = 'inclspy .dat                    '
  if (iprspy .lt. 1) go to 2509
  write (unit = munit6, fmt = 2508) answ80(1 : 20)
2508 format ('   Start "type" service.  answ80(1:20) =', a20)
  call window
2509 if (answ80(5 : 7) .eq. '?  ') n16 = 1
  if (answ80(5 : 7) .eq. ' ? ') n16 = 1
  if (answ80(5 : 7) .eq. '  ?') n16 = 1
  if (answ80(1 : 8) .eq. 'type    ') n16 = 1
  if (n16 .eq. 0) go to 2511
  n18 = 0
  go to 2548
2511 n12 = 0
  n13 = 0
  do j = n26, 32
     if (n12 .gt. 0)  go to 2514
     if (answ80(j : j) .eq. ' ') cycle
     n12 = j
2514 if (answ80(j : j) .eq. ' ') go to 2519
     n13 = j
  end do
2519 if (iprspy .lt. 1) go to 32521
  write (unit = munit6, fmt = 2521) n12, n13
2521 format (' File name bounded.  n12, n13 =', 2i5)
  call window
32521 if (n13 .ne. n12) go to 2524
  ansi32(8 : 8) = answ80(n12 : n12)
  do i = 1, 9
     if (answ80(n12 : n12) .eq. digit(i) .and. filext(i) .eq. 'x') go to 2536
  end do
  write (unit = munit6, fmt = 2526) answ80(n12 : n12)
  call window
  go to 1240
2524 inquire (file = answ80(n12 : n13), exist = logvar)
  if (logvar) go to 2529
  write (unit = munit6, fmt = 2526) answ80(n12 : n13)
2526 format ('   ?? ??  Sorry, no such file named : ', a32)
  call window
  go to 1240
2529 ansi32 = blan80(1 : 32)
  ansi32(1 : n13 - n12 + 1) = answ80(n12 : n13)
!2532 if (answ80(1 : 1) .eq. '@') go to 2536
  if (answ80(1 : 1) .eq. '@') go to 2536
  write (unit = munit6, fmt = 2534) ansi32
2534 format (20x, ' ******  Listing of file : ', a32)
  call window
2536 muntsv(1) = munit5
  munit5 = muntsv(2)
2537 open (unit = munit5, status = 'old', file = ansi32)
  if (answ80(1 : 1) .eq. '@') go to 1276
  do i = 1, 9999
     call quiter
     if (kwtspy .eq. 0) go to 2538
     kwtspy = 0
     go to 2545
2538 read (unit = munit5, fmt = 1269, end = 2545) buff77
1269 format (a80)
     write (unit = munit6, fmt = 2539) i, buff77
2539 format (' Line', i4,  ': ', a80)
     call window
  end do
!2542 continue
2545 close (unit = munit5, status = 'keep')
  if (n16 .eq. 0) go to 2551
2548 n18 = n18 + 1
  if (n18 .eq. 10) go to 2551
  if (filext(n18) .ne. 'x') go to 2548
  ansi32(8 : 8) = digit(n18)
  go to 2537
2551 munit5 = muntsv(1)
  go to 1240
  !     $$$$$$$  key word no. 23:  "files"    $$$$  $$$$  $$$$  $$$$  $$$$
  !     "files"  results in a display of the status of "@?" files, as
  !     previously determined by  "initsp"  as simulation began.
2563 write (unit = munit6, fmt = 2572)  (j, j = 1, 9)
2572 format ('   File number :', 9i5)
  call window
  write (unit = munit6, fmt = 2576) (filext(j), j = 1, 9)
2576 format ('   inclspy?.dat:', 9(4x, a1))
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
2604 write (unit = munit6, fmt = 32604)
32604 format ('     Begin  "wake"  processing.')
  call window
  if (n13 .ne. 1) go to 2611
  !     regenerate header of plot file attached to unit lunit(4):
  write (unit = munit6, fmt = 2605)
2605 format ('    Start regenerating lunit4 header.')
  call window
  rewind lunit(4)
  read (unit = lunit(4)) date1, tclock, numnam, numnvo, numbco, numbrn
  if (lbus .ge. numnam .and. lsiz12 .ge. numnvo .and. lbrnch .ge. numbrn) go to 2608
  write (unit = munit6, fmt = 2606)
2606 format ('   $$$$  Overflow.  EMTP list sizes are too small to regenerate the header information of the plot file.')
  call window
  write (unit = munit6, fmt = 2607) numnam, numnvo, numbrn
2607 format ('       Abort this command.  sorry.   numnam, numnvo, numbrn =', 3i5)
  call window
  go to 1240
2608 rewind lunit(4)
  read (unit = lunit(4)) date1, tclock, numnam, numnvo, numbco, numbrn, (bus(j), j = 1, numnam)
  if (numnvo .gt. 0) read (lunit(4)) (ibsout(j), j = 1, numnvo)
  if (numbrn .gt. 0) read (lunit(4)) (kbus(j), mbus(j), j = 1, numbrn)
  rewind lunit(4)
  write (unit = lunit(4)) date1, tclock, numnam, numnvo, numbco, numbrn, (bus(j), j = 1, numnam)
  if (numnvo .gt. 0) write (lunit(4)) (ibsout(j), j = 1, numnvo)
  if (numbrn .gt. 0) write (lunit(4)) (kbus(j), mbus(j), j = 1, numbrn)
2611 call runtym (d1, d2)
  call pfatch
  call tables
  flstat(1) = -d1
  flstat(2) = -d2
  if (lstat(16) .eq. ltlabl) go to 2614
  write (unit = munit6, fmt = 2612)
2612 format ('   ????  Sorry, present EMTP dimensions do not agree with those of the disk file')
  call window
  write (unit = munit6, fmt = 32612)
32612 format (9x, 'Just attached.  Sleeping simulation can not be awakened.  Abort the command.  Sorry.')
  call window
  go to 1240
2614 kill = 7733
  lstat(15) = 16
  go to 2402
  !     $$$$$$$  key word no. 29:  "catalog"  $$$$  $$$$  $$$$  $$$$  $$$$
2907 write (unit = prom80, fmt = 2910)
2910 format (' Send complete, legal disk-file name:')
  !  assign 2913 to nextsn
  nextsn = 2913
  go to 9800
2913 ansi32 = buff77(1 : 32)
  if (ansi32 .eq. blan80(1 : 32)) ansi32 = 'spydata                         '
  close (unit = luntsp)
  open (unit = luntsp, status = 'new', file = ansi32)
  n1 = 1
  n2 = 80
  do j = 1, numcrd
!2914 write (unit = luntsp, fmt = 1269) file6(j)
     write (unit = luntsp, fmt = 1269) file6(j)
  end do
  write (unit = munit6, fmt = 2917) luntsp, numcrd
2917 format ('    ---- Write to disk via unit', i4, '   is now complete.  Number of cards =', i5)
  call window
  close (unit = luntsp, status = 'keep')
  go to 1240
  !     $$$$$$$  key word no. 30:  "begin"    $$$$  $$$$  $$$$  $$$$  $$$$
2926 kill = 7733
  llbuff = -3333
  lstat(15) = 1
  indbuf = 0
  mflush = 0
  go to 2402
  !     $$$$$$$  key word no. 31:  "step"     $$$$  $$$$  $$$$  $$$$  $$$$
2937 if (kbreak .eq. 0) go to 2944
  write (unit = munit6, fmt = 2941)
2941 format ('   ---  Toggle to "off"  (no more stepping)')
  call window
  go to 2946
2944 write (unit = munit6, fmt = 2945)
2945 format ('   ---  Toggle to "on"  (begin stepping)')
  call window
2946 kbreak = kbreak + 1
  if (kbreak .ge. 2) kbreak = 0
  if (kbreak .eq. 1) lockbr = 1
  go to 1240
  !     $$$$$$$  key word no. 32:  "debug"    $$$$  $$$$  $$$$  $$$$  $$$$
2964 write (unit = prom80, fmt = 2967) iprspy
2967 format ('   Send new "iprspy" value  [', i3, '  ] :')
  !  assign 2969 to nextsn
  nextsn = 2969
  go to 9800
2969 call frein1 (buff77, iprspy)
  go to 1240
  !     $$$$$$$  key word no. 33:  "data"     $$$$  $$$$  $$$$  $$$$  $$$$
2974 n12 = 1
  n13 = 9999
  n14 = 1
  n15 = 0
2975 write (unit = prom80, fmt = 2976)
2976 format ('   Send EMTP data file name (control) :')
  !  assign 2979 to nextsn
  next = 2979
  go to 9800
2979 ansi32 = buff77(1 : 32)
  if (ansi32(1 : 8) .ne. 'control ') go to 2984
  write (unit = prom80, fmt = 2981)
2981 format (' Send disk card numbers n-beg and n-end :')
  !  assign 32981 to nextsn
  nextsn = 32981
  go to 9800
32981 call frein2 (buff77, n12, n13)
  write (unit = prom80, fmt = 2982)
2982 format (' Send n-beg in EMTP, lunit5 offset :')
  !  assign 2983 to nextsn
  nextsn = 2983
  go to 9800
2983 call frein2 (buff77, n14, n15)
  go to 2975
2984 if (iprspy .lt. 1) go to 32985
  write (unit = munit6, fmt = 2985) n12, n13, n14, n15
2985 format (' Prepare to open.  n12, n13, n14, n15 =', 4i8)
  call window
32985 close (unit = lunit(14))
  open (unit = lunit(14), status = 'unknown', file = ansi32, iostat = ios)
  if (ios .ne. 0) then
     write (unit = *, fmt = "('Could not open file ', a32)") ansi32
     call stoptp
  end if
  if (iprspy .lt. 1) go to 2987
  write (unit = munit6, fmt = 2986)
2986 format (' After file open (it worked).')
  call window
2987 do l = 1, n13
     if (l .ge. n12) go to 2988
     read (unit = lunit(14), fmt = 2991) ansi8
     cycle
2988 read (unit = lunit(14), fmt = 2991, end = 2998) file6(n14)
2991 format (a80)
     if (file6(n14)(1 : 4) .eq. 'eof ') go to 2998
     n14 = n14 + 1
     if (n14 .le. limcrd) cycle
     write (unit = munit6, fmt = "('   ****  Warning.   Card image buffer has filled.  Disk read is truncated.')")
     call window
     write (unit = munit6, fmt = 2192) limcrd
2192 format ('                    Storage capacity in cards = limcrd =', i5)
     call window
     go to 2998
  end do
  write (unit = munit6, fmt = 2996) n13
2996 format ('    Note :  no  end-of-file  encountered.   Stop reading after card number', i5)
  call window
2998 if (numcrd .lt. n14) numcrd = n14 - 1
  write (unit = munit6, fmt = 2999) numcrd
2999 format (i9, '  =  numcrd  (upper bound on card-image storage in file6 cache).')
  call window
  if (l .gt. 1) go to 3006
  write (unit = munit6, fmt = 3004)
3004 format ('  ? ? ? ? ?   Warning.  Data file is empty.   Did user misspell the file name?')
  call window
3006 close (unit = lunit(14))
  numdcd = n15
  go to 1240
  !     $$$$$$$  key word no. 34:  "ramp"     $$$$  $$$$  $$$$  $$$$  $$$$
3095 if (numrmp .le. 0) go to 3099
  !     beginning of loop over ramps having same begin and ending times:
3096 write (unit = prom80, fmt = 3098)
3098 format (' Send "t-begin", "t-end" (end, show, rewind) :')
  !  assign 33098 to nextsn
  nextsn = 33098
  go to 9800
33098 bytbuf = buff77(1:20)
  if (bytbuf(1 : 4) .eq. 'end ') go to 1240
  if (bytbuf(1 : 4) .eq. 'stop') call stoptp
  if (bytbuf(1 : 6) .ne. 'rewind') go to 3100
3099 numrmp = 0
  kanal = 0
  tmaxrp = -1.e20
  tminrp =  1.e20
  go to 3096
3100 if (bytbuf(1 : 4) .ne. 'show') go to 3116
  !     following code provides confirmation of the ramped deposits:
  write (unit = munit6, fmt = 3102)
3102 format (' row  name   index   num    t-begin        t-end        f-begin        f-end')
  call window
  do j = 1, numrmp
     write (unit = munit6, fmt = 3103) j, symbrp(j), indxrp(j), looprp(j),  tbegrp(j), tendrp(j), fbegrp(j), fendrp(j)
3103 format (1x, i2,  2x, a6, 2i6, 4e14.5)
     call window
  end do
  write (unit = munit6, fmt = 3105) tminrp, tmaxrp
3105 format ('         No ramping before', e13.4, '  or after', e13.4, '  sec.')
  call window
  go to 3096
3116 call frefp2 (bytbuf, tim1rp, tim2rp)
  if (tim1rp .eq. 0.0) tim1rp = t
  if (tim2rp .lt. 0.0) tim2rp = tim1rp + abs (tim2rp)
  !     beginning of loop over function values (for given times):
3122 write (unit = prom80, fmt = 3125)
3125 format (' Send "f-begin", "f-end" (end) : ')
  !  assign 3128 to nextsn
  nextsn = 3128
  go to 9800
3128 bytbuf = buff77(1 : 20)
  if (bytbuf(1 : 4) .eq. 'end ') go to 3096
  if (bytbuf(1 : 4) .eq. 'stop') call stoptp
  call frefp2 (bytbuf, val1rp, val2rp)
  !     beginning of loop over variables (for given f vs. t ramps):
3131 go to 1460
  !     we return to 3134 after "deposit" logic has found variable info.
  !     actual transfer back here is from just below 1807.
3134 n8 = locate(ind) + 2 * (n1 - 1)
  n9 = (n8 - memkar) / 2
  n10 = memkar + 2 * n9 - n8
  if (n10 .ne. 0 .and. n9 .lt. 0) n9 = n9 - 1
  numrmp = numrmp + 1
  kanal = 1
  if (numrmp .le. 20) go to 3135
  write (unit = munit6, fmt = 23134)
23134 format ('  ** **  Table overflow.   Erase all  "ramp"  entries, and start again ....')
  call window
  go to 3095
3135 memrmp(numrmp) = n9 + 1
  n10rmp(numrmp) = n10
  looprp(numrmp) = n2 - n1 + 1
  tbegrp(numrmp) = tim1rp
  if (tim1rp .lt. tminrp) tminrp = tim1rp
  tendrp(numrmp) = tim2rp
  if (tim2rp .gt. tmaxrp) tmaxrp = tim2rp
  fbegrp(numrmp) = val1rp
  fendrp(numrmp) = val2rp
  symbrp(numrmp) = symb(ind)
  indxrp(numrmp) = n1
  rampsl(numrmp) = (val2rp - val1rp) / (tim2rp - tim1rp)
  rampcn(numrmp) = val2rp - rampsl(numrmp) * tim2rp
  write (unit = prom80, fmt = 3136)
3136 format ('     [y] change? (0=no, 1=yes) :')
  !  assign 43137 to nextsn
  nextsn = 43137
  go to 9800
43137 call frein1 (buff77, n8)
  kyramp(numrmp) = n8
  if (n8 .eq. 0) go to 3131
  tendrp(numrmp) = tim2rp + deltat
  if (tmaxrp .lt. tendrp(numrmp)) tmaxrp = tendrp(numrmp)
  if (n1 .gt. ibr)  go to 3131
  write (unit = munit6, fmt = 3137) n1, ibr
3137 format ('  %%  %%  Cancel last "ramp" request.  row number', i4, '  does not exceed ibr =', i4)
  call window
  numrmp = numrmp - 1
  go to 3131
  !     we return to 3138 from below 1496, if "end" read during "deposit"
3138 go to 3122
  !     $$$$$$$  key word no. 35:  "time"     $$$$  $$$$  $$$$  $$$$  $$$$
3153 call time44 (spytim)
  call date44 (spdate)
  write (unit = munit6, fmt = 3158) t, tmax, deltat, spytim, spdate
3158 format (3x, 't =', e13.6, 3x, 'tmax =', e11.4, 3x, 'deltat =', e11.4, 3x, 2a4, 2x, 2a4)
  call window
  go to 1240
  !     $$$$$$$  key word no. 36:  "tek"      $$$$  $$$$  $$$$  $$$$  $$$$
3174 write (unit = munit6, fmt = 3177)
3177 format ('  >< To tamper with the rolling vector plot,  send choice.')
  call window
3178 write (unit = prom80, fmt = 3179)
3179 format ('  >< Option (mark, delay, inner, overlap, end, help) : ' )
  !  assign 3180 to nextsn
  nextsn = 3180
  go to 9800
3180 ansi8 = buff77(1 : 8)
  if (ansi8 .ne. 'help    ') go to 3186
  write (unit = munit6, fmt = 3181)
3181 format ('     Mark  ---- for instantaneous marking of curves on tek screen;')
  call window
  write (unit = munit6, fmt = 33181)
33181 format ('     Delay  --- to control how simultaneous the rolling is to be;')
  call window
  write (unit = munit6, fmt = 43181)
43181 format ('     Inner  ---- to  call timval  (the "inner:" level of ploting);')
  call window
  write (unit = munit6, fmt = 53181)
53181 format ('     Overlap  -- to modify the percent overlap for new-page plot;')
  call window
  write (unit = munit6, fmt = 3182)
3182 format ('     End   ---- for return to  "spy:"  prompt.')
  call window
  go to 3178
3186 if (ansi8 .ne. 'mark    ') go to 3193
  write (unit = prom80, fmt = 3189) ksymbl
3189 format ('    Send repeat frequency after immediate 1st one [',  i6,  ' ] :')
  !  assign 3191 to nextsn
  nextsn = 3191
  go to 9800
3191 call frein1 (buff77, ksymbl)
  go to 3178
3193 if (ansi8 .ne. 'delay   ') go to 3207
  write (unit = prom80, fmt = 3202) kslowr
3202 format ('  Send new rolling frequency [', i5, ' ] :')
  !  assign 3204 to nextsn
  nextsn = 3204
  go to 9800
3204 call frein1 (buff77, kslowr)
  go to 3178
3207 if (ansi8 .ne. 'inner   ') go to 3209
3208 monitr = 2345
  call timval
  monitr = 1
  if (nexmod .eq. 0) go to 3178
  nexmod = 1
  go to 9803
3209 if (ansi8 .ne. 'overlap ') go to 3214
  write (unit = prom80, fmt = 3211) inchlp
3211 format ('    Send time-overlap percentage [',  i3,  ' ] :')
  !  assign 3204 to nextsn
  nextsn = 3204
  go to 9800
!3212 call frein1 (buff77, inchlp)
  call frein1 (buff77, inchlp)
  go to 3178
3214 if (ansi8 .eq. 'end     ') go to 1240
  write (unit = munit6, fmt = 3227)
3227 format ('   Sorry,  spy  does not understand.   Come again ...')
  call window
  go to 3178
  !     $$$$$$$  key word no. 37:  "branch"   $$$$  $$$$  $$$$  $$$$  $$$$
3256 bytbuf(1 : 4) = answ80(1 : 4)
  n33 = 0
  n24 = 0
  go to 3308
  !3306 assign 3307 to nextsn
3306 nextsn = 3307
  go to 9800
3307 bytbuf = buff77(1 : 20)
3308 if (bytbuf(1 : 4) .eq. 'end ') go to 1240
  if (bytbuf(1 : 4) .eq. 'stop') go to 1240
  if (bytbuf(1 : 6) .ne. 'series') go to 3309
  n24 = 1
  go to 3306
3309 if (bytbuf(1 : 4) .ne. 'extr') go to 3317
  n33 = n33 + 1
  if (n33 .ge. 2) n33 = 0
3310 if (n33 .eq. 0) go to 3314
  write (unit = prom80, fmt = 3311)
3311 format (' row', 13x, 'ci', 13x, 'ck', 12x, 'cik', 12x, 'cik', 10x, 'ckkjm', ':')
  go to 3306
3314 write (unit = prom80, fmt = 3315)
3315 format (' row  name  name-k name-m kbus mbus lgth  nr kodebr kodsem litype imodel indhst:')
  go to 3306
3317 if (bytbuf(1 : 4) .eq. spykwd(37)(1 : 4) .or. bytbuf(1 : 4) .eq. spykwd(1)(1 : 4)) go to 3310
  !     Since not key-word response, we extract (n1,n2) integer
  !     pair (free-format); "ibr" is maximum; "n17" is error flag:
!3322 call intpar (ibr, n1, n2, n17)
  call intpar (ibr, n1, n2, n17)
  if (n17 .gt. 0) go to 3306
  n27 = 1
  i = n1
3323 call quiter
  if (kwtspy .eq. 0) go to 3325
  kwtspy = 0
  go to 3306
3325 n5 = iabs (kbus(i))
  n6 = iabs (mbus(i))
  bus1 = bus(n5)
  if (n5 .eq. 1) bus1 = terra
  bus2 = bus(n6)
  if (n6 .eq. 1) bus2 = terra
  if (n24 .eq. 0) go to 3327
  n27 = length(i)
  if (n5 .lt. 0) go to 3334
  if (kodebr(i) .gt. 0) go to 3334
  if (n27 .ne. 1) go to 3334
  if (nr(i) .ge. 0) go to 3334
3327 if ( n33 .ne. 0) go to 3329
  n7 = namebr(i)
  write (unit = munit6, fmt = 3328) i, texvec(n7), bus1, bus2, kbus(i), mbus(i), length(i), nr(i), kodebr(i), kodsem(i), &
       litype(i), imodel(i), indhst(i)
3328 format (1x, i3, 1x, a6, 1x, a6, 1x, a6, 3i5, i4, 5i7)
  call window
3329 if (n33 .ne. 1) go to 3334
  write (unit = munit6, fmt = 3331) i, ci(i), ck(i), cik(i), cki(i), ckkjm(i)
3331 format (1x, i3, 5e15.6)
  call window
3334 i = i + n27
  if (i .le. n2) go to 3323
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
3406 if (ialter .eq. 2) ialter = 1
  go to 1240
  !     $$$$$$$  service 7:switch, 25:source, 26:edit, 28:language,
  !              42:rlc, 43:width, 44:bus, 45:size, 46:limit,
  !              47:iout, 48:node, 49:nonlin, 50:space, 51:lunit(4),
  !              52:series, 53:lock, 54:[y], 55:[f], 56:noroll
8500 call spyink
  if (nexmod .ne. 0) go to 9803
  go to 1240
  !     $$$$$$$  key word no. 57:  "open"     $$$$  $$$$  $$$$  $$$$  $$$$
3568 write (unit = prom80, fmt = 3571)
3571 format ('    Next unit, name (i2, a32 ) :')
  !  assign 3573 to nextsn
  nextsn = 3573
  go to 9800
3573 read (unit = buff77, fmt = 3576) n7, ansi32
3576 format (i2, a32)
  if (n7 .le. 0) go to 1240
  open (unit = n7, status = 'unknown', file = ansi32)
  go to 3568
  !     $$$$$$$  key word no. 58:  "close"    $$$$  $$$$  $$$$  $$$$  $$$$
3584 write (unit = prom80, fmt = 3587)
3587 format ('    Next unit, status (i2, a32) :')
  !  assign 3591 to nextsn
  nextsn = 3591
  go to 9800
3591 read (unit = buff77, fmt = 3576) n7, ansi32
  if (n7 .le. 0) go to 1240
  if (ansi32(1 : 6) .eq. 'keep  ') go to 3596
  if (ansi32(1 : 6) .eq. 'delete') go to 3596
  close (unit = n7)
  go to 3584
3596 close (unit = n7, status = ansi32)
  go to 3584
  !     $$$$$$$  key word no. 59:  "sm"       $$$$  $$$$  $$$$  $$$$  $$$$
3606 if (nchain .ge. 16 .and. nst .gt. 0) go to 43606
  write (unit = munit6, fmt = 3607)
3607 format ('   ?? ??  no s.m. data present or not yet in time-step loop.')
  call window
  go to 1240
43606 ksmspy(3) = 1
  lockbr = 0
3608 ksmspy(1) = -1
  ksmspy(2) = 0
  maxflg = 1
  !  assign 3609 to nextsn
  nextsn = 3609
  prom80(1 : 8) = '        '
  go to 9800
3609 if (iprspy .lt. 1) go to 83609
  write (unit = munit6, fmt = 23609) ksmspy(2)
23609 format (' At s.n. 3609 of "spying".  ksmspy(2) =', i4)
  call window
83609 if (ksmspy(2) .eq. 0) go to 3608
  ksmspy(3) = 0
  write (unit = prom80, fmt = 43609) ksmspy(2)
43609 format ('  s.m.', i3, '  wanted (y or n) ? :')
  ksmspy(1) = +1
  !  assign 3610 to nextsn
  nextsn = 3610
  go to 9800
3610 if (buff77(1 : 1) .eq. 'n') go to 43606
  ksmspy(1) = +1
3611 write (unit = prom80, fmt = 3612)
3612 format ('     Send s.m. class (elec, mech, elva, meva, all, end) :')
  !  assign 3613 to nextsn
  nextsn = 3613
  go to 9800
3613 n14 = ksmspy(2)
  if (buff77(1 : 4) .eq. 'end') go to 43606
  if (buff77(1 : 4) .ne. 'elec' .and. buff77(1 : 4) .ne. 'all ') go to 43615
  k2 = (n14 - 1) * 101
  write (unit = munit6, fmt = 33614)
33614 format ('             ld            laf             lf           lakd           lfkd')
  call window
  xl = elp(k2 + 20)
  d3 = elp(k2 + 1) + xl
  d4 = elp(k2 + 2)
  d5 = (elp(k2 + 3) + elp(k2 + 62)) / factom
  d6 = elp(k2 + 4)
  d8 = elp(k2 + 5)
  write (unit = munit6, fmt = 53614) d3, d4, d5, d6, d8
53614 format (5e15.7)
  call window
  write (unit = munit6, fmt = 63614)
63614 format ('            lkd             rf            rkd             lq            lag')
  call window
  d3 = (elp(k2 + 6) + elp(k2 + 63)) / factom
  d4 = elp(k2 + 7)
  d5 = elp(k2 + 8)
  d6 = elp(k2 + 9) + xl
  d8 = elp(k2 + 10)
  write (unit = munit6, fmt = 53614) d3, d4, d5, d6, d8
  call window
  write (unit = munit6, fmt = 73614)
73614 format ('             lg           lakq           lgkq            lkq             rg')
  call window
  d3 = (elp(k2 + 11 ) + elp(k2 + 64)) / factom
  d4 = elp(k2 + 12)
  d5 = elp(k2 + 13)
  d6 = (elp(k2 + 14) + elp(k2 + 65)) / factom
  d8 = elp(k2 + 15)
  write (unit = munit6, fmt = 53614) d3, d4, d5, d6, d8
  call window
  write (unit = munit6, fmt = 83614)
83614 format ('            rkq             l0             r0         agline             ra')
  call window
  d3 = elp(k2 + 16)
  d4 = 1.0 / elp(k2 + 17)
  d5 = elp(k2 + 18)
  d5 = (d4 - d5) / (1.0 + damrat)
  d4 = (d4 - d5) / factom
  d8 = elp(k2 + 20)
  write (unit = munit6, fmt = 53614) d3, d4, d5, xl, d8
  call window
  write (unit = munit6, fmt = 23615)
23615 format ('           rat1         dsat10         dsat12         qsat10         qsat12')
  call window
  k3 = k2 + 21
  k2 = k2 + 25
  write (unit = munit6, fmt = 53614) (elp(k1), k1 = k3, k2)
  call window
43615 if (buff77(1 : 4) .ne. 'mech' .and. buff77(1 : 4) .ne. 'all ') go to 13617
  k2 = (n14 - 1) * 101 + 26
  k3 = (n14 - 1) * 30 + 12
  numask = ismdat(k3)
  write (unit = munit6, fmt = 53615)
53615 format ('  numask   nlocg   nloce     cnp')
  call window
  write (unit = munit6, fmt = 63615) numask, ismdat(k3 + 1), ismdat(k3 + 2), elp(k2)
63615 format (3i8, f8.1)
  call window
  write (unit = munit6, fmt = 83615)
83615 format ('           hico            dsm            hsp            dsr')
  call window
  k2 = 0
  kp = 0
  do k1 = 1, n14
     k2 = k2 + ismdat(kp + 12)
!3616 kp = kp + 30
     kp = kp + 30
  end do
  num2 = numask + numask
  num3 = numask + num2
  num5 = num3 + num2
  k2 = (k2 - numask) * 12
  k2 = k2 + num2
  do k1 = 1, numask
     k2 = k2 + 1
     write (unit = munit6, fmt = 53614) shp(k2 + numask), shp(k2 + num2), shp(k2 + num3), shp(k2 + num5)
     call window
  end do
13617 if (buff77(1 : 4) .ne. 'meva' .and. buff77(1 : 4) .ne. 'all ') go to 13618
  write (unit = munit6, fmt = 23617)
23617 format(' Mechanical angles in units of radians ')
  call window
  k3 = (n14 - 1) * 30 + 12
  numask = ismdat(k3)
  k2 = 0
  kp = 0
  do k1 = 1, n14
     k2 = k2 + ismdat(kp + 12)
!3617 kp = kp + 30
     kp = kp + 30
  end do
  k2 = (k2 - numask) * 6
  k3 = k2 + 1
  k2 = k2 + numask
  write (unit = munit6, fmt = 53614) (histq(k1), k1 = k3, k2)
  call window
  k3 = k2 + 1
  k2 = k2 + numask
  write (unit = munit6, fmt = 33617)
33617 format (' Mechanical speeds in units of  radians/sec ')
  call window
  write (unit = munit6, fmt = 53614) (histq(k1), k1 = k3, k2)
  call window
13618 if (buff77(1 : 4) .ne. 'elva' .and. buff77(1 : 4) .ne. 'all ') go to 3611
  write (unit = munit6, fmt = 23618)
23618 format ('             id             iq             i0             if            ikd')
  call window
  k2 = (n14 - 1) * 24
  k3 = k2 + 1
  k2 = k2 + 5
  write (unit = munit6, fmt = 53614) (cu(k1), k1 = k3, k2)
  call window
  write (unit = munit6, fmt = 33618)
33618 format ('             ig            ikq             ia             ib             ic')
  call window
  k3 = k2 + 1
  k2 = k2 + 2
  write (unit = munit6, fmt = 53614) cu(k3), cu(k2), (smoutv(k1), k1 = 1, 3)
  call window
  write (unit = munit6, fmt = 43618)
43618 format ('             vd             vq             v0             vf             va')
  call window
  write (unit = munit6, fmt = 53614) (smoutv(k1), k1 = 4, 8)
  call window
  write (unit = munit6, fmt = 53618)
53618 format('             vb             vc            teq           texc')
  call window
  write (unit = munit6, fmt = 53614) (smoutv(k1), k1 = 9, 12)
  call window
!3619 continue
  go to 3611
  !     $$$$$$$  key word no. 60:  "honk"     $$$$  $$$$  $$$$  $$$$  $$$$
3623 write (unit = prom80, fmt = 3626)
3626 format ('   Send severity level of alert (1 to 10) :')
  !  assign 3629 to nextsn
  nextsn = 3629
  go to 9800
3629 call frein1 (buff77, n24)
  call honker (n24)
  go to 1240
  !     $$$$$$$  key word no. 63:  "wait"     $$$$  $$$$  $$$$  $$$$  $$$$
3644 write (unit = prom80, fmt = 3647)
3647 format ('     Send desired hibernation time in seconds :')
  !  assign 3649 to nextsn
  nextsn = 3649
  go to 9800
3649 call frefp1 (buff77, d13)
  nd13 = int (d13, kind (nd13))
  call tdelay (nd13)
  go to 1240
  !     $$$$$$$  key word no. 12:  "echo"     $$$$  $$$$  $$$$  $$$$  $$$$
3673 write (unit = prom80, fmt = 3676)
3676 format ('    Send desired operation (begin, file, show) :')
  !  assign 3681 to nextsn
  nextsn = 3681
  go to 9800
3681 if (buff77(1 : 6) .ne. 'begin ') go to 3686
  kspsav = limcrd + 1
  n23 = limcrd - numcrd
  write (unit = munit6, fmt = 3683) n23
3683 format ('         Ok, up to a maximum of', i7, '   commands can be accumulated, if no more data.')
  call window
  go to 1240
3686 if (buff77(1 : 5) .ne. 'file ') go to 3702
  write (unit = *, fmt = *) ' to be completed later.  ?????????'
  go to 1240
3702 if (buff77(1 : 5) .ne. 'show ') go to 1303
  write (unit = *, fmt = *) ' to be completed later.  ??????'
  go to 3673
  !     $$$$$$$  key word no.  2:  "stop"     $$$$  $$$$  $$$$  $$$$  $$$$
9000 call time44 (spytim)
  call date44 (spdate)
  write (unit = munit6, fmt = 3158) t, tmax, deltat, spytim, spdate
  call window
  call stoptp
9800 nexmod = 2
9803 if (iprspy .lt. 1) go to 9811
  write (unit = munit6, fmt = 9807) jjroll, kbreak, lockbr, nchain, prom80(1 : 20)
9807 format (' Exit "spying".  jjroll, kbreak, lockbr, nchain =', 4i5, '   prom80(1:20) =', a20)
  call window
9811 if (prom80(1 : 8) .eq. '        ') go to 9833
  if (kfile5 .eq. 1) go to 9833
  call prompt
9833 return
end subroutine spying

!
! subroutine spyink.
!

subroutine spyink
  use blkcom
  use labcom
  use tacsar
  use dekspy
  use indcom
  use tracom
  use bcddat
  use bcdtim
  use movcop
  use strcom
  implicit none
  !     Module of interactive emtp only, which services "emtspy".
  !     this is the 2nd half of principle module "spying".
  integer(4) :: i, ioutcs, ip
  integer(4) :: j, k, kptplt, kud2, kud3
  integer(4) :: l, ltacst
  integer(4) :: m, mmfind, mmhold, mstrng
  integer(4) :: n1, n2, n3, n4, n5, n6, n7, n8, n9, n11, n12, n13, n14, n15, n16
  integer(4) :: n17, n18, n22, n23, n24
  integer(4) :: n33, n42, n43, n66
  integer(4) :: n77, ndx1, ndx2, ndx3, ndx4, nsmout, numnv0
  real(8) :: d8, d14, d15, d17, d18, d24
  character(8) :: d13, text1, text2, d12
  dimension mmhold(20)
  !
  save
  data text1 / 'tamper' /
  if (iprspy .lt. 1) go to 1003
  write (unit = munit6, fmt = 1002) nchain, jjroll, kbreak, lockbr, nexmod
1002 format (' Enter "spying".  nchain  jjroll  kbreak  lockbr, nexmod =', 5i5)
  call window
1003 if (nexmod .ne. 3) go to 1004
  nexmod = 0
!  go to nextsn
  select case (nextsn)
  case (2620)
     go to 2620

  case (2786)
     go to 2786

  case (2787)
     go to 2787

  case (2793)
     go to 2793

  case (2807)
     go to 2807

  case (3464)
     go to 3464

  case (3564)
     go to 3564

  case (3665)
     go to 3665

  case (3690)
     go to 3690

  case (3760)
     go to 3760

  case (3855)
     go to 3855

  case (3865)
     go to 3865

  case (3917)
     go to 3917

  case (3938)
     go to 3938

  case (3942)
     go to 3942

  case (3947)
     go to 3947

  case (3959)
     go to 3959

  case (4011)
     go to 4011

  case (4016)
     go to 4016

  case (4026)
     go to 4026

  case (4029)
     go to 4029

  case (4085)
     go to 4085

  case (4169)
     go to 4169

  case (4219)
     go to 4219

  case (4227)
     go to 4227

  case (4254)
     go to 4254

  case (4719)
     go to 4719

  case (4759)
     go to 4759

  case (4778)
     go to 4778

  case (34215)
     go to 34215

  case (42859)
     go to 42859

  case (43858)
     go to 43858

  case (43875)
     go to 43875

  case (52703)
     go to 52703

  case (54474)
     go to 54474

  end select
1004 if ( jword .gt. 63 )     go to 1007
  select case (jword)
  case (1 : 6, 8 : 24, 27, 29 : 41, 57 : 60, 63)
     go to 9999

  case (25)
     go to 2856

  case (26)
     go to 2617

  case (28)
     go to 2782

  case (42)
     go to 3456

  case (43)
     go to 3526

  case (44)
     go to 3541

  case (45)
     go to 3635

  case (46)
     go to 3647

  case (47)
     go to 3662

  case (48)
     go to 3684

  case (49)
     go to 3752

  case (50)
     go to 3842

  case (51)
     go to 4002

  case (52)
     go to 4163

  case (53)
     go to 4436

  case (54)
     go to 4471

  case (55)
     go to 4512

  case (56)
     go to 4563

  case (61)
     go to 4592

  case (62)
     go to 4716
  end select
!  go to ( &
!      heading  stop    plot    help  examine  deposit  switch  (1-7)
!       9999 ,   9999,   9999,   9999,   9999,   9999,   2804, &
!      rest    save   restore   go    blank    find    list    (8-14)
!       9999 ,   9999,   9999,   9999,   9999,   9999,   9999, &
!      spy    break   when  comment    @?     roll   type?   (15-21)
!       9999 ,   9999,   9999,   9999,   9999,   9999,   9999, &
!      verify   files   sleep  source   edit    wake  language (22-28)
!       9999 ,   9999,   9999,   2856,   2617,   9999,   2782, &
!      catalog  begin    step    debug   data    ramp    time   (29-35)
!       9999 ,   9999,   9999,   9999,   9999,   9999,   9999, &
!      tek   branch   yform    noy   factor    nof    rlc    (36-42)
!       9999 ,   9999,   9999,   9999,   9999,   9999,   3456, &
!      width    bus    size   limit    iout    node   nonlin  (43-49)
!       3526 ,   3541,   3635,   3647,   3662,   3684,   3752, &
!      space  lunit(4)  series   lock     [y]     [f]   noroll  (50-56)
!       3842 ,   4002,   4163,   4436,   4471,   4512,   4563, &
!      open    close  unlock   honk   choice   tacs    wait   (57-63)
!       9999 ,   9999,   9999,   9999,   4592,   4716,   9999), jword
  call stoptp
1007 n14 = jword - 63
  select case (n14)
  case (1)
     go to 4823
  end select
!  go to ( &
!      v-i                                                   (64-64)
!       4823 ),  n14
  call stoptp
  !     $$$$$$$  key word no. 26:  "edit"     $$$$  $$$$  $$$$  $$$$  $$$$
  !     "edit"  command allows the user to look at the emtp data case
  !     which is stored in the rtm data base; display commands are
  !     similar to  vax/vms  editor  "sos" :
2617 linnow = 0
2618 write (unit = prom80, fmt = 2621)
2621 format (' *')
  !  assign 2620 to nextsn
  nextsn = 2620
  go to 9800
2620 read (buff77, 2623) char1, bytbuf
2623 format (a1, a20)
  if (iprspy .lt. 1) go to 2626
  write (munit6, 2622) char1, bytbuf(ip : ip + 9)
2622 format (' char1, bytbuf(1 : 10) after read:', a2, 1x, a10)
  call window
2626 if (char1 .eq. 'f') go to 2669
  if (char1 .eq. 's') go to 2669
  if (char1 .eq. 'e') go to 1240
  if (char1 .eq. '8') go to 2734
  if (char1 .eq. 'c' .and. bytbuf(1 : 1) .eq. 'o' .and. bytbuf(2 : 2) .eq. 'l') go to 2756
  if (char1 .eq. 'p') go to 2629
  if (char1 .eq. 'd') go to 2629
  if (char1 .eq. 'i') go to 2629
  if (char1 .eq. 'r') go to 2629
  if (char1 .eq. ' ') go to 2629
  write (munit6, 42622)
42622 format ('     Sorry, 1st character is meaningless.   Try again .... ')
  call window
  go to 2618
2629 call sosrng ( n17 )
  if ( iprspy .lt. 2 )  go to 2633
  write (munit6, 2632)  n17, lidnt1, lidnt2
2632 format (' n17, lidnt1, lidnt2 =', 3i8)
  call window
2633 if (n17 .gt. 0) go to 2618
  if (char1 .eq. 'i') go to 2695
  if (char1 .eq. 'd') go to 2719
  if (char1 .eq. 'r') go to 2719
  !     we reach here ready to print lines  (lidnt1, lidnt2) :  *p  case
2634 do j = lidnt1, lidnt2
     call quiter
     if (kwtspy .eq. 0) go to 2639
     kwtspy = 0
     linnow = j - 1
     lidnt1 = j - 1
     lidnt2 = j - 1
     go to 2618
2639 write (munit6, 2667) j, file6(j)
2667 format (1x, i4, 1x, a80)
     call window
  end do
  linspn = lidnt2 - lidnt1
  linnow = lidnt2
  go to 2618
  !     following code is for  "*f[string]@"  locate request (like sos):
2669 n4 = 0
  n5 = 0
  do i = 2, 20
     if (bytbuf(i:i) .eq. '@') n4 = 1
     if (bytbuf(i:i) .ne. ' ') n5 = 1
  end do
!2670 continue
  if (n5 .eq. 0) go to 2671
  if (n4 .eq. 1) go to 2673
  write (munit6, 32670)
32670 format ('  Illegal "*f" --- string not terminated by "@".')
  call window
  go to 2618
2671 if (mstrng .eq. 1) go to 2672
  write (munit6, 22671)
22671 format ('  Illegal "*f" --- blank string, but no earlier usage for reference.')
  call window
  go to 2618
  !2672 call movers ( bytfnd(1), bytbuf(1), 20 )    ! restore old string
2672 bytbuf = bytfnd
  linnow = linnow + 1
2673 n6 = linnow
  do j = n6, numcrd
     l = 1
     do k = 1, 80
        if (file6(j)(k : k) .eq. bytbuf(l : l)) go to 2674
        l = 1
        cycle
2674    if (bytbuf(l + 1 : l + 1) .ne. '@') go to 2676
        linnow = j
        lidnt2 = j
        if (char1 .eq. 's') go to 2763
        go to 2691
2676    l = l + 1
     end do
  end do
  l = 0
  write (unit = munit6, fmt = 2686)
2686 format (' String not found, search failed')
  call window
  linnow = linnow - 1
2691 if (l .le. 0) go to 2693
  write (munit6, 2692) j, file6(j)
2692 format (i5, 1x, a80)
  call window
!2693 bytfnd = bytbuf
2693 bytfnd(1:20) = bytbuf(1:20)
  mstrng = 1
  go to 2618
  !     following code is to process  "*i"  command:
  !     following code is for  "*i"  request :
2695 n1 = 0
  linnow = lidnt1
2701 if (n1 .lt. 20) go to 2703
  write (munit6, 52701)
52701 format ('    Filled working storage of 20 lines.   Truncated insertion now in progress.')
  call window
  go to 2704
2703 linnow = linnow + 1
  write (prom80, 2702) linnow
2702 format (1x, i4, ' ')
  !  assign 52703 to nextsn
  nextsn = 52703
  go to 9800
52703 if (buff77(1 : 1) .ne. '@') go to 2711
2704 j = numcrd
2706 file6(j + n1) = file6(j)
  j = j - 1
  if (j .gt. lidnt1) go to 2706
  do j = 1, n1
!2708 file6(linnow + j) = file6b(j)
     file6(linnow + j) = file6b(j)
  end do
  linnow = linnow - 1
  lidnt1 = linnow
  lidnt2 = linnow
  numcrd = numcrd + n1 / 10
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
  if (j .le. numcrd) go to 2723
  numcrd = numcrd - (lidnt2 - lidnt1 + 1)
  linnow = lidnt1 - 1
  lidnt1 = linnow
  lidnt2 = linnow
  if (char1 .ne. 'r') go to 2618
  go to 2695
  !     following code is for  "(8)"  request :
2734 if (bytbuf(1 : 4) .ne. '    ') go to 2743
  j = linnow
2736 if (file6(j)(80 : 80) .eq. ' ') go to 2738
  write (munit6, 2692) j, file6(j)
  call window
  read (munit5, 2623) bytbuf
  if (bytbuf(1 : 1) .eq. 'e') go to 2618
  if (bytbuf(1 : 1) .eq. ' ') go to 2738
  bytbuf(2 : 2) = file6(j)(80 : 80)
  if (bytbuf(1 : 1) .eq. '0') file6b(j)(80 : 80) = ' '
  if (bytbuf(1 : 1) .eq. '1' .or. bytbuf(1:1) .eq. '2' .or. bytbuf(1 : 1) .eq. '3' .or. bytbuf(1 : 1) .eq. '4') file6b(j)(80 : 80) = bytbuf(1 : 1)
  if (bytbuf(2 : 2) .ne. file6(j)(80 : 80)) go to 2738
  write (munit6, 2737) bytbuf(1 : 1)
2737 format ('   Illegal col. 80 value  "', a1, '" .   Cancel substitution.')
  call window
2738 j = j + 1
  if (j .le. numcrd) go to 2736
  linnow = numcrd
  go to 2618
  !     following code is for  "(8),?"  case, to deposit in column 80:
2743 if (bytbuf(1 : 1) .eq. ',') go to 2749
!2744 write (munit6, 2746)
  write (unit = munit6, fmt = 2746)
2746 format (' Sorry,  "(8)"  must be followed by a comma and digit 1-4, or blanks.')
  call window
  go to 2618
2749 file6b(linnow)(80 : 80) = bytbuf(2 : 2)
  lidnt1 = linnow
  lidnt2 = linnow
  go to 2634
  !     following code is for  "*rule"  request for column marking:
2756 if (bytbuf(3 : 3) .eq. ' ' ) go to 2758
  write (munit6, 2759)
2759 format (1x, '12345678901234567890123456789012345678901234567890123456789012345678901234567890')
  call window
  go to 2761
2758 write (munit6, 2760)
2760 format (5x, '12345678901234567890123456789012345678901234567890123456789012345678901234567890')
  call window
2761 go to 2618
  !     the following is for  "*s"  processing, after branch from the
  !     middle of  "*f"  (where string was found):
2763 k = k + 1
  if (iprspy .lt. 1) go to 32764
  write (munit6, 2764) k, l
2764 format (' *s exited to 2763.  k, l =', 2i8)
  call window
32764 n7 = k - l
  n12 = 81 - k
  buff77(1 : n12) = file6(linnow)(k : 80)
  buff77(n12 + 1 : n12 + 20) = blan80(1 : 20)
  l = l + 2
  go to 2767
2765 file6(linnow)(n7 : n7) = bytbuf(l : l)
  if (n7 .eq. 80) go to 2781
  n7 = n7 + 1
  l = l + 1
  if (l .gt. 20) go to 2769
2767 if (bytbuf(l : l) .ne. '@') go to 2765
  n14 = 81 - n7
  file6(linnow)(n7 : 80) = buff77(1 : n14)
  if (iprspy .lt. 1) go to 2781
  write (munit6, 2768)
2768 format (' Done with string sub.  Exit to 2772.')
  call window
  go to 2781
2769 write (munit6, 2772)
2772 format ('   Illegal  "*s" ---- 2nd  "@"  missing.')
  call window
  go to 2618
2781 j = linnow
  if (iprspy .lt. 1) go to 2691
  write (munit6, 62781) j, n3, n4
62781 format (' at 2781.  j, n3, n4 =', 3i8)
  call window
  go to 2691
  !     $$$$$$$  key word no. 28:  "language" $$$$  $$$$  $$$$  $$$$  $$$$
2782 write (prom80, 2785)
2785 format ('   Next command language operation (single, entire, show, spy :')
  !  assign 2786 to nextsn
  nextsn = 2786
  go to 9800
2786 ansi8 = buff77(1 : 8)
  if (ansi8 .ne. 'entire  ') go to 32788
  n13 = 1
  n14 = 10
  !32786 assign 2787 to nextsn
32786 nextsn = 2787
  go to 9800
2787 read (buff77, 2788) (spykwd(j), j = n13, n14)
2788 format (10a8)
  if (n14 .eq. numkey) go to 2782
  n13 = n13 + 10
  n14 = n14 + 10
  if (n14 .gt. numkey) n14 = numkey
  go to 32786
32788 if (ansi8 .ne. 'show    ') go to 2790
  do j = 1, numkey, 10
     write (unit = munit6, fmt = 2789) (spykwd(ip), ip = j, j + 9)
2789 format (1x, 10a8)
     call window
  end do
  go to 2782
2790 if (ansi8 .ne. 'single  ') go to 2782
2791 write (prom80, 2792)
2792 format ('      Send next old and new symbols as 2a8 (end) : ')
  call prompt
  !  assign 2793 to nextsn
  nextsn = 2793
  go to 9800
2793 ansi8 = buff77(1 : 8)
  buff77(1 : 8) = buff77(9:16)
  if (ansi8 .eq. 'end     ') go to 2782
  do j = 1, numkey
     if (ansi8 .eq. spykwd(j)) go to 2798
  end do
  write (munit6, 2796) ansi8
2796 format ('    ? ? ?   Sorry,  no such symbol found.   Try again ...')
  call window
  go to 2791
2798 spykwd(j) = buff77(1 : 8)
  go to 2791
  !     $$$$$$$  key word no.  7:  "switch"   $$$$  $$$$  $$$$  $$$$  $$$$
!2804 bytbuf(1 : 4) = answ80(1 : 4)
  bytbuf(1 : 4) = answ80(1 : 4)
  n33 = 0
  if (kswtch .gt. 0) go to 2808
  write (munit6, 2805)
2805 format ('   ---  Sorry, no such elements available in this data case.  Try another key word ...')
  call window
  go to 1240
  !2806 assign 2807 to nextsn
2806 nextsn = 2807
  go to 9800
2807 bytbuf = buff77(1 : 20)
2808 if (bytbuf(1 : 4)  .eq.  'end ') go to 1240
  if (bytbuf(1 : 4) .eq. 'stop') go to 1240
  if (bytbuf(1 : 4) .ne. 'extr') go to 2814
2810 write (prom80, 2811)
2811 format ('  name ', 10x, 'akey', 10x, 'crit', 8x, 'energy  kswtyp  modswt  nextsw :')
  n33 = 1
  go to 2806
2814 if (bytbuf(1 : 4) .ne. spykwd(7)(1 : 4) .and. bytbuf(1 : 4) .ne. spykwd(1)(1 : 4)) go to 2822
  write (prom80, 2815)
2815 format (' row name-k name-m k(kmswit)m kpos kentnb', 3x, 'tclose', 7x, 'adelay',  7x, 'topen:')
  call prompt
  if (n33 .gt. 0) go to 2810
  go to 2806
  !     we get to 2822 if no known key word;  extract integer pair.   this
  !     is dec free-format; "kswtch" is maximum;  "n17" is error flag:
2822 call intpar (kswtch, n1, n2, n17)
  if (n17  .gt.  0) go to 2806
  do i = n1, n2
     call quiter
     if (kwtspy .eq. 0) go to 2825
     kwtspy = 0
     go to 2806
2825 n5 = iabs (kmswit(i))
     n6 = iabs (kmswit(lswtch + i))
     d12 = bus(n5)
     if (n5 .eq. 1) d12 = terra
     d13 = bus(n6)
     if (n6 .eq. 1) d13 = terra
     write (munit6, 2828) i, d12, d13, kmswit(i), kmswit(lswtch + i), kpos(i), kentnb(i), tclose(i), adelay(i), topen(i)
2828 format (1x, i3, 1x, a6, 1x, a6, i6, i5, i5, i7, 3e13.4)
     call window
     if (n33 .ne. 1) cycle
     n7 = namesw(i)
     write (munit6, 2831) texvec(n7), adelay(i), crit(i), energy(i), kswtyp(i), modswt(i), nextsw(i)
2831 format (1x, a6, 3e14.5, 3i8)
     call window
  end do
  go to 2806
  !     $$$$$$$  key word no. 25:  "source"   $$$$  $$$$  $$$$  $$$$  $$$$
2856 n33 = 0
2859 prom80 = '        '
  !  assign 42859 to nextsn
  nextsn = 42859
  go to 9800
42859 bytbuf = buff77(1:20)
  if (bytbuf(1:4) .eq. 'end ') go to 1240
  if (bytbuf(1:4) .eq. 'stop') go to 1240
  if (bytbuf(1:4) .ne. 'tacs') go to 2862
  n33 = 1
  n14 = kxtcs + nuk
  write (munit6, 2860)
2860 format (' row  a6 tacs  source value', 3x, 'data field-A', 4x, 'data field-B', 4x, 'data field-C')
  call window
  write (prom80, 2861) n14, kud1, kud3, kud2
2861 format ( ' No.   name', 4x, 'offset', i6,  3x, 'offset', i6, 4x, 'offset', i6, 4x, 'offset', i6, ' :')
  go to 2859
2862 if (bytbuf(1 : 4) .ne. 'elec') go to 2868
  !     "elec" is the command for a new heading for electric-network
  !     source table:
  n33 = 0
!2863 write (prom80, 2865)
  write (unit = prom80, fmt = 2865)
2865 format (' row  name  node iform', 6x, 'crest', 9x, 'sfreq', 10x, 'time1', 9x, 'tstart :')
  go to 2859
2868 if (n33 .eq. 1) go to 2884
  !     following is display code for electric-network source table:
  call intpar (kconst, n1, n2, n17)
  if (n17 .gt. 0) go to 2859
  do i = n1, n2
     call quiter
     if (kwtspy .eq. 0) go to 2873
     kwtspy = 0
     go to 2859
2873 n5 = iabs (node(i))
     write (munit6, 2878) i, bus(n5), node(i), iform(i), crest(i), sfreq(i), time1(i), tstart(i)
2878 format (1x, i3, 1x, a6, i5, i6, 2e14.5, 2e15.6)
     call window
  end do
  prom80(1 : 8) = blan80(1 : 8)
  go to 2859
  !     we reach 2884 with request to output another set of tacs sources:
2884 call intpar (niu, n1, n2, n17)
  if (n17 .gt. 0) go to 2859
  do i = n1, n2
     call quiter
     if (kwtspy .eq. 0) go to 2887
     kwtspy = 0
     go to 2859
2887 ansi8 = blan80(1 : 8)
     if (iuty(kiuty + i) .lt. 90) write (ansi8, 3691) sptacs(kaliu + i)
     write (munit6, 2891) i, ansi8, sptacs(kxtcs + nuk + i), sptacs(kud1 + i), sptacs(kud3 + i), sptacs(kud2 + i)
2891 format (1x, i3, 1x, a6, 4e16.6)
     call window
  end do
  prom80(1 : 8) = blan80(1 : 8)
  go to 2859
  !     $$$$$$$  key word no. 42:  "rlc"      $$$$  $$$$  $$$$  $$$$  $$$$
3456 bytbuf(1 : 4) = answ80(1 : 4)
  n33 = 0
  go to 3467
!3462 assign 3464 to nextsn
3462 nextsn = 3464
  go to 9800
3464 bytbuf = buff77(1 : 20)
3467 if (bytbuf(1 : 4) .eq. 'end ') go to 1240
  if (bytbuf(1 : 4) .eq. 'stop') go to 1240
  if (bytbuf(1 : 4) .ne. 'extr') go to 3490
  n33 = n33 + 1
  if (n33 .ge. 2) n33 = 0
3471 if (n33 .eq. 1) write (prom80, 3478)
3478 format (' No extension yet in use.  But structure allows it.')
  if (n33 .eq. 0) write (prom80, 3488)
3488 format (' row', 9x, 'tr(i)', 9x, 'tx(i)', 10x, 'hr(i)', 10x, 'c(i)', 10x, 'x(i):')
  go to 3462
3490 if (bytbuf(1 : 4) .eq. spykwd(42)(1 : 4) .or. bytbuf(1 : 4) .eq. spykwd(1)(1 : 4)) go to 3471
  !     we get here if no known key word;  extract integer pair.   this
  !     is dec free-format; "it" is maximum;  "n17" is error flag:
  call intpar (it, n1, n2, n17)
  if (n17 .gt. 0) go to 3462
  do i = n1, n2
     call quiter
     if (kwtspy .eq. 0) go to 3497
     kwtspy = 0
     go to 3462
3497 write (munit6, 3503) i, tr(i), tx(i), r(i), c(i), x(i)
3503 format (1x, i3, 5e14.5)
     call window
  end do
  prom80(1 : 8) = blan80(1 : 8)
  go to 3462
  !     $$$$$$$  key word no. 43:  "width"    $$$$  $$$$  $$$$  $$$$  $$$$
3526 n13 = kol132
  if (n13 .ne. 80) go to 3529
  write (munit6, 3527)
3527 format ('   ---  Switch from 80- to 132-column lunit6 EMTP output.')
  call window
  go to 3532
3529 write (munit6, 3528)
3528 format ('   ---  Switch from 132- to 80-column lunit6 emtp output.')
  call window
3532 kol132 = 132
  if (n13 .eq. 132) kol132 = 80
  go to 1240
  !     $$$$$$$  key word no. 44:  "bus"      $$$$  $$$$  $$$$  $$$$  $$$$
3541 bytbuf(1 : 4) = answ80(1 : 4)
  n33 = 0
  go to 3567
  !3562 assign 3564 to nextsn
3562 nextsn = 3564
  go to 9800
3564 bytbuf = buff77(1 : 20)
3567 if (bytbuf(1 : 4) .eq. 'end ') go to 1240
  if (bytbuf(1 : 4) .eq. 'stop') go to 1240
  if (bytbuf(1 : 4) .ne. 'extr') go to 3590
  n33 = n33 + 1
  if (n33 .ge. 2) n33 = 0
3572 if (n33 .eq. 1) write (prom80, 3578)
3578 format ('  row', 8x, 'finit', 6x, 'znonl-a kknonl', 6x, 'znonl-b kknonl', 6x, 'znonl-c kknonl', ' :')
  if (n33 .eq. 0) write (prom80, 3588)
3588 format (' node  name  kode  kssfrq    kk     kks', 12x, 'e(i)', 12x, 'f(i) :')
  go to 3562
3590 if (bytbuf(1 : 4) .eq. spykwd(44)(1 : 4) .or. bytbuf(1 : 4) .eq. spykwd(1)(1 : 4)) go to 3572
  !     we get here if no known key word;  extract integer pair.   this
  !     is dec free-format; "it" is maximum;  "n17" is error flag:
  call intpar (ntot, n1, n2, n17)
  if (n17 .gt. 0) go to 3562
  do i = n1, n2
     call quiter
     if (kwtspy .eq. 0) go to 3597
     kwtspy = 0
     go to 3562
3597 if (n33 .eq. 0) write (munit6, 3603) i, bus(i), kode(i), kssfrq(i), kk(i), kks(i), e(i), f(i)
3603 format (1x, i4,  2x, a6,  i4, i8, i6, i8, 2e16.6)
     n22 = ntot + i
     n23 = ntot + n22
     if (n33 .eq. 1) write (munit6, 3609) i, finit(i), znonl(i), kknonl(i), znonl(n22), kknonl(n22),  znonl(n23), kknonl(n23)
3609 format (1x, i4, f13.4, 3( f14.5, i6))
     call window
  end do
!3613 continue
  prom80(1 : 8) = blan80(1 : 8)
  go to 3562
  !     $$$$$$$  key word no. 45:  "size"     $$$$  $$$$  $$$$  $$$$  $$$$
3635 write (munit6, 3638) (lstat(i + 20), i = 1, 10)
3638 format ('   size  1-10:', 10i6)
  call window
  write (munit6, 3639) (lstat(i + 20), i = 11, 20)
3639 format ('   size 11-20:', 10i6)
  call window
  write (munit6, 3640) (lstat(i + 20), i = 21, 27)
3640 format ('   size 21-on:', 10i6)
  call window
  go to 1240
  !     $$$$$$$  key word no. 46:  "limit"    $$$$  $$$$  $$$$  $$$$  $$$$
3647 write (munit6, 3649) lbus, lbrnch, ldata, lexct, lymat, lswtch, lsize7, lpast, lnonl, lchar
3649 format ('   limit  1-10:', 10i6)
  call window
  write (munit6, 3650) lsmout, lsiz12, lfdep, lwt, ltails, limass, lsyn, maxpe, ltacst, lfsem
3650 format ('   limit 11-20:', 10i6)
  call window
  write (munit6, 3651) lfd, lhist, lsiz23, lcomp, lspcum, lsiz26, lsiz27
3651 format ('   limit 21-on:', 10i6)
  call window
  go to 1240
  !     $$$$$$$  key word no. 47:  "iout"     $$$$  $$$$  $$$$  $$$$  $$$$
3662 write (prom80, 3664) iout
3664 format ('    Send new printout frequency [', i5,  ' ] : ')
  !  assign 3665 to nextsn
  nextsn = 3665
  go to 9800
3665 call frein1 (buff77, n6)
  limstp = 999999
  if (n6 .le. 0) n6 = 1
  !  call copyi (n6, multpr, 5)
  call copy (n6, multpr, 5)
  n7 = istep - 1
  do j = 1, 9999
     n7 = n7 + 1
     n8 = n7 / n6
     if (n6 * n8 .ne. n7) cycle
     if (iprsup .lt. 1) go to 3667
     write (munit6, 3666)  n7
3666 format (' New round stop.  n7 =', i8)
     call window
3667 if (n6 .lt. 20) n7 = n7 + n6
     if (n7 .lt. istep + 20) n7 = n7 + n6
     kprchg(2) = n7
     kprchg(5) = 999999
     kprchg(4) = 999999
     kprchg(3) = 999999
     go to 3675
  end do
3675 iout = n6
  if (isprin .gt. iout) isprin = iout
  limstp = n7
  indstp = 2
  if (iprspy .lt. 1) go to 3679
  write (munit6, 3678) limstp, indstp, istep, iout, isprin
3678 format (' limstp, indstp, istep, iout, isprin =', 5i8)
  call window
3679 go to 1240
  !     $$$$$$$  key word no. 48:  "node"     $$$$  $$$$  $$$$  $$$$  $$$$
3684 write (prom80, 3687)
3687 format ('       Send  a6  bus name (connect) :')
  !3688 assign 3690 to nextsn
3688 nextsn = 3690
  go to 9800
3690 spycd2 = buff77(1 : 35)
  if (spycd2(1 : 8) .eq. 'stop    ' .or. spycd2(1 : 8) .eq. 'end     ') go to 1240
  if (spycd2(1 : 8) .eq. 'connect ') go to 3701
  do j = 1, ntot
     write (brobus, 3691) bus(j)
3691 format (a6, 2x)
     if (brobus .ne. spycd2(1 : 8)) cycle
     write (prom80, 3692) j
3692 format ('       >>> node =', i4, 15x, ':')
     go to 3688
  end do
  write (munit6, 3695)
3695 format ('        ????  Sorry, no such bus.  Try again ...')
  call window
  go to 3684
3701 mmfind = j
  n6 = 0
  do k=1, ibr
     if (mmfind .ne. iabs(kbus(k)) .and. mmfind .ne. iabs(mbus(k))) cycle
     if (n6 .lt. 20) go to 3703
     write (munit6, 3708) (mmhold(ip), ip = 1, n6)
     call window
     n6 = 0
3703 n6 = n6 + 1
     mmhold(n6) = k
  end do
  if (n6 .le. 0) go to 3709
  write (munit6, 3708) (mmhold(k), k = 1, n6)
3708 format ('    %% linear branches :', 20i5)
  call window
3709 n6 = 0
  do k = 1, kswtch
     if (mmfind .ne. iabs(kmswit(k)) .and. mmfind .ne. iabs(kmswit(lswtch + k))) cycle
     n6 = n6 + 1
     mmhold(n6) = k
  end do
  if (n6 .le. 0) go to 3722
  write (munit6, 3721) (mmhold(k), k = 1, n6)
3721 format ('    %% EMTP  switches  :', 20i5)
  call window
3722 n6 = 0
  do k = 1, inonl
     if (mmfind .ne. iabs(nonlk(k)) .and. mmfind .ne. iabs(nonlm(k))) cycle
     n6 = n6 + 1
     mmhold(n6) = k
  end do
  if (n6 .le. 0) go to 3731
  write (munit6, 3730) (mmhold(k), k = 1, n6)
3730 format ('    %% nonlinear elem. :', 20i5)
  call window
3731 n6 = 0
  do k = 1, kconst
     if (mmfind .ne. iabs(node(k))) cycle
     n6 = n6 + 1
     mmhold(n6) = k
  end do
  if (n6 .le. 0) go to 3739
  write (munit6, 3738) (mmhold(k), k = 1, n6)
3738 format ('    %% EMTP  sources  : ', 20i5)
  call window
3739 mmfind = 0
  go to 3684
  !     $$$$$$$  key word no. 49:  "nonlin"   $$$$  $$$$  $$$$  $$$$  $$$$
3752 bytbuf(1 : 4) = answ80(1 : 4)
  n33 = 0
  if (inonl .gt. 0) go to 3762
  write (munit6, 2805)
  call window
  go to 1240
  !3758 assign 3760 to nextsn
3758 nextsn = 3760
  go to 9800
3760 bytbuf = buff77(1 : 20)
3762 if (bytbuf(1 : 4) .eq. 'end ') go to 1240
  if (bytbuf(1 : 4) .eq. 'stop') go to 1240
  if (bytbuf(1 : 4) .ne. 'extr') go to 3774
3766 write (prom80, 3769)
3769 format (9x, 'anonl', 8x, 'vzero', 8x, 'vnonl', 9x, 'curr', 7x, 'vecnl1', 7x, 'vecnl1:')
  n33 = 1
  go to 3758
3774 if (bytbuf(1 : 4) .ne. spykwd(49)(1 : 4)) go to 3781
  write (prom80, 3777)
3777 format ( ' row  name  name-k name-m  nonlk  nonlm nltype nonlad nonle ilast kupl nlsub :')
  call prompt
  if (n33 .gt. 0) go to 3766
  prom80(1 : 8) = blan80(1 : 8)
  go to 3758
  !     we get to 3781 if no known key word;  extract integer pair.   this
  !     is dec free-format; "inonl" is maximum;  "n17" is error flag:
3781 call intpar (inonl, n1, n2, n17)
  if (n17 .gt. 0) go to 3758
  do i = n1, n2
     call quiter
     if (kwtspy .eq. 0) go to 3785
     kwtspy = 0
     go to 3758
3785 n5 = iabs (nonlk(i))
     d12 = bus(n5)
     if (n5 .eq. 1) d12 = terra
     n6 = iabs (nonlm(i))
     d13 = bus(n6)
     if (n6 .eq. 1) d13 = terra
     n7 = namenl(i)
     write (munit6, 3786) i, texvec(n7), d12, d13, nonlk(i), nonlm(i), nltype(i), nonlad(i), nonle(i), ilast(i), kupl(i),  nlsub(i)
3786 format (1x, i3, 3(1x, a6), 4i7, 2i6, i5, i6)
     call window
     if (n33 .ne. 1) cycle
     write (unit = munit6, fmt = 3792) anonl(i), vzero(i), vnonl(i), curr(i), vecnl1(i), vecnl2(i)
3792 format (1x, 6e13.4)
     call window
  end do
  prom80(1 : 8) = blan80(1 : 8)
  go to 3758
  !     $$$$$$$  key word no. 50:  "space"    $$$$  $$$$  $$$$  $$$$  $$$$
3842 d14 = pltbuf(indbeg + 1)
  kptplt = lstat(32)
  n77 = 0
  n22 = kptplt + 1
  write (munit6, 3843)
3843 format (2x, ' Current values.  indbuf  indbeg  limbuf  mflush  numdcd  numcrd  limcrd')
  call window
  write (munit6, 3844)  indbuf, indbeg, limbuf, mflush, numdcd, numcrd, limcrd
3844 format (18x, 7i8)
  call window
  write (munit6, 3845) d14, t
3845 format (3x, 'Plot timespan now in memory (in sec) =', 2e15.6)
  call window
  if (mflush .lt. 1000) go to 3848
3846 buff77(1 : 6) = 'flush '
  d17 = -1
  d18 = fltinf
  n4 = 1
  go to 3876
3848 if (lastov .ne. 9911) go to 3851
  lastov = nchain - 1
  go to 3857
3851 write (prom80, 3854)
3854 format ('    Choose class (cards, plot) :')
  !  assign 3855 to nextsn
  nextsn = 3855
  go to 9800
3855 if (buff77(1 : 4) .ne. 'plot') go to 3934
  !     begin logic to free some storage space for emtp plot data:
3857 write (prom80, 3858)
3858 format ('   Operation (write, thin, flush, delete, auto, read, out, help) :')
  !  assign 43858 to nextsn
  nextsn = 43858
  go to 9800
43858 if (buff77(1 : 6) .eq. 'delete') n77 = 1
  if (buff77(1 : 6) .eq. 'read  ') n77 = 2
  if (buff77(1 : 6) .eq. 'thin  ') n77 = 3
  if (buff77(1 : 6) .eq. 'write ') n77 = 4
  if (buff77(1 : 6) .eq. 'flush ') n77 = 5
  if (buff77(1 : 4) .ne. 'help') go to 3861
  write (munit6, 3859)
3859 format ('        To free memory for more simulation plot points,  use one of these:')
  call window
  write (munit6, 33859)
33859 format ('        Either: 1) begin by saving points on disk, prior to loss ("write");')
  call window
  write (munit6, 43859)
43859 format ('                2) thin out present storage by regular omission ("thin");')
  call window
  write (munit6, 53859)
53859 format ('                3) delete older points, and shift the remainder ("delete") :')
  call window
  write (munit6, 3860)
3860 format ('                4) combined "write" and "thin", with common iplot ("flush").')
  call window
  write (munit6, 23860)
23860 format ('        Send  "read"  to load memory with plot data now on disk (lunit4).')
  call window
  write (munit6, 63860)
63860 format ('        Send  "out"  to switch from plot file to cards,  or  "spy"  to abort.')
  call window
  write (munit6, 73860)
73860 format ('        Finally, send  "auto"  for automatic full flush to disk from now on.')
  call window
  go to 3857
3861 if (buff77(1 : 4) .ne. 'auto') go to 3862
  if (mflush .lt. 1000) mflush = mflush + 1000
  go to 3846
3862 if (buff77(1 : 4) .eq. 'out ') go to 3851
  write (prom80, 3863)
3863 format ('     Send [tmin, tmax] (all, half) :')
  !  assign 3865 to nextsn
  nextsn = 3865
  go to 9800
3865 answ80 = buff77
  if (answ80(1 : 4) .ne. 'all ') go to 3867
  d17 = -1.
  d18 = fltinf
  go to 3874
3867 if (answ80(1 : 4) .ne. 'half') go to 3871
  n1 = (indbuf - indbeg) / 2
  n2 = (n1 / n22) * n22
  n3 = indbeg + n2 + 1
  d17 = -1.
  d18 = pltbuf(n3)
  if (iprspy .ge. 1) write (*, 3869) n22, n3, d18
3869 format (' "half" computed.  n22, n3, d18 =', 2i8, e14.5)
  go to 3874
3871 call frefp2 (answ80, d17, d18)
3874 n4 = intinf
  if (n77 .eq. 1) go to 3876
  write (prom80, 3875)
3875 format ('      Process every  n-th  step.  Send n:')
  !  assign 43875 to nextsn
  nextsn = 43875
  go to 9800
43875 prom80 = buff77
  call frein1 (prom80, n4)
  if (n4 .eq. 0) n4 = 1
  if (n4 .lt. 0) go to 3857
  if (n77 .eq. 2) go to 3915
3876 n13 = indbeg + 1
3877 if (pltbuf(n13) .ge. d17) go to 3886
  n13 = n13 + n22
  if (n13 .lt. indbuf) go to 3877
  write (munit6, 3881)
3881 format ('       ?? ??  Sorry, no plot points in the requested time span.  Abort.')
  call window
  go to 1240
3886 if (n77 .ne. 4 .and. n77 .ne. 5) go to 3904
  mflush = mflush + 1
  write (unit = lunit(6), fmt = 3891) mflush, n6, n4, d17, d18
3891 format ('   ++++  Begin plot-data copy from memory to disk.   mflush, n6, n4, tbeg, tend =', i3, 2i8, 2e14.5)
  go to 3906
3904 write (unit = lunit(6), fmt = 3905) n4, d17, d18
3905 format ('   ****  Begin thinning of plot data points, to reduce memory burden.   n4, tbeg, tend =', i10, 2e14.5)
3906 ip = 0
  n8 = n13 + kptplt
  n6 = n13
  do l = 1, 999999
     if (pltbuf(n13) .gt. d18) go to 3911
     ip = ip + 1
     if (ip .lt. n4) go to 3908
     ip = 0
     if (n77 .ne. 4 .and. n77 .ne. 5) go to 3907
     write (lunit(4)) (pltbuf(m), m = n13, n8)
     if (n77 .eq. 4 ) go to 3908
3907 call movesp (pltbuf(n13), pltbuf(n6), n22)
     n6 = n6 + n22
3908 n13 = n13 + n22
     if (n13 .ge. indbuf) go to 3911
!3910 n8 = n8 + n22
     n8 = n8 + n22
  end do
3911 if (n77 .eq. 4) go to 3914
  n14 = indbuf - n13 + 1
  if (n14 .gt. kptplt) call movesp (pltbuf(n13), pltbuf(n6), n14)
  indbuf = n6 + n14 - 1
  iascii(1000) = n13 - n6
  if (n77 .ne. 5) go to 3914
  n5 = limbuf - indbuf
  write (unit = lunit(6), fmt = 3913) n5
3913 format (9x, 'Completed "flush", leaving', i8, '  free cells for new plot points.')
  if (mflush .ge. 1000) go to 1240
3914 go to 3857
  !     begin "read" servicing (plot data goes disk to memory):
3915 write (unit = prom80, fmt = 3916)
3916 format ('      Destination (0--start; 1--as now) :')
  !  assign 3917 to nextsn
  nextsn = 3917
  go to 9800
3917 prom80 = buff77
  call frein1 (prom80, n3)
  n13 = indbuf
  if (n3 .eq. 0) n13 = indbeg
  ip = 0
  n8 = n13 + kptplt
  n11 = 0
  do j = 1, 999999
     ip = ip + 1
     if (ip .ge. n4) go to 3919
     read (unit = lunit(4), end = 3926) d15
     if (d15 .gt. d18) go to 3928
     go to 3921
3919 read (unit = lunit(4), end = 3926) (pltbuf(l), l = n13, n8)
     if (pltbuf(n13) .gt. d18) go to 3934
     n11 = n11 + 1
3921 n8 = n8 + n22
     if (n8 .le. limbuf) go to 3925
     write (unit = munit6, fmt = 3923) pltbuf(n13)
3923 format ('   ###  Squish.  Memory space for plot data has run out after storing t=', e14.5)
     call window
     go to 3928
3925 n13 = n13 + n22
  end do
  j = 999999
  go to 3928
3926 write (munit6, 3927)  d18
3927 format ('     EOF during read (lunit(4)).  Quit search for tend =', e13.4)
  call window
3928 indbuf = n13
  n6 = limbuf - indbuf
  write (munit6, 3931) j, n11, n6
3931 format ('     Steps read and retained =',  i7, i6, '   remaining free words =', i8)
  call window
  go to 3857
  !     begin "cards" servicing (file6 character storage):
3934 write (unit = prom80, fmt = 3937)
3937 format ('   Choose operation (move, blank, out, space) :')
  !  assign 3938 to nextsn
  nextsn = 3938
  go to 9800
3938 if (buff77(1 : 4) .eq. 'out ') go to 3851
  if (buff77(1 : 6) .eq. 'space ') go to 3851
  n66 = 0
  if (buff77(1 : 6) .eq. 'blank ') n66 = 1
  write (unit = prom80, fmt = 3941)
3941 format ('      Number of cards to be handled :')
  !  assign 3942 to nextsn
  nextsn = 3942
  go to 9800
3942 call frein1 (buff77, n11)
  if (n11 .le. 0) go to 3851
  if (n66 .eq. 1) go to 3955
  write (unit = prom80, fmt = 3944)
3944 format ('      Card addresses n-from and n-to :')
  !  assign 3947 to nextsn
  nextsn = 3947
  go to 9800
3947 call frein1 (buff77, n11)
  if (n17 .le. 0) go to 3934
  if (n18 .lt. n17) go to 3934
  do j = 1, n11
     file6(n18) = file6(n17)
     n17 = n17 + 1
     n18 = n18 + 1
  end do
  go to 3934
3955 write (unit = prom80, fmt = 3956)
3956 format ('      Card address :')
  !  assign 3959 to nextsn
  nextsn = 3959
  go to 9800
3959 call frein1 (buff77, n17)
  if (n17 .le. 0) go to 3851
  do j = 1, n11
     file6(n17) = blan80
!3962 n17 = n17 + 1
     n17 = n17 + 1
  end do
  go to 3934
  !     $$$$$$$  key word no. 51:  "lunit4"   $$$$  $$$$  $$$$  $$$$  $$$$
4002 forbyt(1) = -fltinf
  kptplt = lstat(32)
  d24 = fltinf
4008 write (unit = prom80, fmt = 4010)
4010 format ('   Operation (open, close, top, bot, next, back, time) :')
  !  assign 4011 to nextsn
  nextsn = 4011
  go to 9800
4011 if (buff77(1 : 5) .ne. 'close') go to 4022
  write (unit = prom80, fmt = 4013)
4013 format ('     Save permanently? (y or n) :')
  !  assign 4016 to nextsn
  nextsn = 4016
  go to 9800
4016 ansi8 = 'keep'
  if (buff77(1 : 1) .eq. 'n') ansi8 = 'delete'
  close (unit = lunit(4), status = ansi8)
  go to 4008
4022 if (buff77(1 : 5) .ne. 'open ' )  go to 4036
  write (unit = prom80, fmt = 4025)
4025 format ('     Desired disk file name :')
  !  assign 4026 to nextsn
  nextsn = 4026
  go to 9800
4026 ansi32 = buff77(1 : 32)
  write (unit = prom80, fmt = 4028)
4028 format ('     Desired status (new, old) :')
  !  assign 4029 to nextsn
  nextsn = 4029
  go to 9800
4029 open (unit = lunit(4), status = buff77, file = ansi32, form = 'unformatted' )
  go to 4008
4036 if (buff77(1 : 4) .ne. 'top ') go to 4051
4037 rewind lunit(4)
  read (unit = lunit(4)) date1, tclock, n1, n2, n3, n4
  if (nchain .le. 16 .or. nchain .gt. 20) go to 4044
  if (n2 .eq. numnvo .and. n3 .eq. nc-nv .and. n4 .eq. nc) go to 4044
  write (unit = munit6, fmt = 4038)
4038 format ('   ++++  Error.  Inconsistent lunit4 plot file.  Automatic close.  Try again.')
  call window
  write (unit = munit6, fmt = 4039) date1, tclock, n1, n2, n3, n4
4039 format ('         date1, tclock =', 2(1x, 2a4), 5x, 'n1:n4 =', 4i8)
  call window
  close (unit = lunit(4))
  go to 4008
4044 if (n2 .gt. 0) read (unit = lunit(4)) n5
  if (n4 .gt. 0) read (unit = lunit(4)) n6
  kptplt = numnvo + nc
  if (to_lower (buff77(1 : 5)) .eq. 'time ') go to 24085
  go to 4008
4051 if (to_lower (buff77(1 : 4)) .ne. 'bot ') go to 4068
  n15 = 0
  do k = 1, 100
     do j = 1, 1000
!4053    read (lunit(4), end=4062)  forbyt(1)
        read (unit = lunit(4), end = 4062) forbyt(1)
     end do
     n15 = n15 + 1000
     write (unit = munit6, fmt = 4056) n15
4056 format ('+      Still reading.', i8)
     call window
  end do
!4059 continue
  go to 4008
4062 n15 = n15 + j
  write (unit = munit6, fmt = 4063) forbyt(1), n15
4063 format ('       Ok, at end lunit4.  t-end =', e15.6, '   steps taken =', i8)
  call window
  go to 4008
4068 if (to_lower (buff77(1 : 4)) .ne. 'next') go to 4079
  !4069 read (lunit(4), end=4075)  ( forbyt(j), j=1, kptplt+1 )
  read (unit = lunit(4), end = 4075) (forbyt(j), j = 1, kptplt + 1)
  write (unit = munit6, fmt = 4071) (forbyt(j), j = 1, kptplt + 1)
4071 format (1x, 10e13.4)
  call window
  if (forbyt(1) .lt. d24) d24 = forbyt(1)
  go to 4008
4075 write (unit = munit6, fmt = 4076)
4076 format ('     ===   Sorry,  no more  lunit4  data.   eof hit.')
  call window
  go to 4008
4079 if (to_lower (buff77(1 : 4)) .ne. 'back') go to 4083
  if (noback .ne. 1)  go to 4082
  write (unit = munit6, fmt = 4080)
4080 format ('    Ok,  but wait for  "top"  and  "time"  using  t = t - 2*deltat')
  call window
  buff77(1 : 5) = 'time '
  d8 = forbyt(1) - 2 * deltat
  go to 4037
4082 backspace lunit(4)
  read (unit = lunit(4), end = 4075) (forbyt(j), j = 1, kptplt + 1)
  write (unit = munit6, fmt = 4071) (forbyt(j), j = 1, kptplt + 1)
  call window
  if (forbyt(1) .gt. d24) backspace lunit(4)
  go to 4008
4083 if (to_lower (buff77(1 : 4)) .ne. 'time') go to 4098
  write (unit = prom80, fmt = 4084) forbyt(1)
4084 format ('       Send desired time [',  e15.6,  ' ] :')
  !  assign 4085 to nextsn
  nextsn = 4085
  go to 9800
4085 call frefp1 (buff77, d8)
24085 n15 = 0
  do k = 1, 100
     do j = 1, 1000
        read (unit = lunit(4), end = 4090) forbyt(1)
        if (forbyt(1) .ge. d8) go to 4094
     end do
!4086 continue
     n15 = n15 + 1000
     write (unit = munit6, fmt = 4056) n15
     call window
  end do
!4089 continue
  go to 4008
4090 n15 = n15 + j
  write (unit = munit6, fmt = 4091) n15
4091 format ('      ? ? ?   Search fails with eof after', i8, '   steps.')
  call window
  go to 4008
4094 n15 = n15 + j
  write (unit = munit6, fmt = 4095) forbyt(1), n15
4095 format ('      Ok, record of  t =',  e15.6, '  just read on try number', i7)
  call window
  go to 4008
  !     put any later operation choices here and below:
4098 write (unit = munit6, fmt = 2796)
  call window
  go to 4008
  !     $$$$$$$  key word no. 52:  "series"   $$$$  $$$$  $$$$  $$$$  $$$$
4163 if (nchain .le. 11) kbrser = 1
  n42 = lstat(22)
  n43 = lstat(23)
  if (kbrser .ne. 2) go to 4165
  write (unit = munit6, fmt = 4164)
4164 format ('   @ @ @ @ @ @   Ok, guy, this is the "over12" break for "series" usage.')
  call window
  kbrser = 0
4165 write (unit = prom80, fmt = 4166)
4166 format ('  Operation (show, extra, change, step, rewind) :')
  !  assign 4169 to nextsn
  nextsn = 4169
  go to 9800
4169 ansi32 = buff77(1 : 32)
  if (to_lower (ansi32(1 : 6)) .ne. 'rewind') go to 4172
  kserlc = 0
  lserlc = 0
  go to 4165
4172 if (to_lower (ansi32(1 : 5)) .ne. 'show ')  go to 4184
  write (unit = munit6, fmt = 4174)
4174 format ('  Memory:  list-2 ---', 2i5, '     list-3 ---', 2i5, '    lserlc =', i4)
  call window
  write (unit = munit6, fmt = 4175) n42, lbrnch, n43, ldata, lserlc
4175 format (' num row use  bus-k  bus-m   name ????', 5x, 'present-R', 5x, 'present-L',  5x, 'present-C')
  call window
  do j = 1, lserlc
     ndx1 = n42 + j
     ndx2 = n43 + j
     k = kbus(ndx1)
     if (k .eq. 0) cycle
     m = mbus(ndx1)
     ndx3 = namebr(ndx1)
     write (unit = munit6, fmt = 4179) j, litype(ndx1), nr(ndx1), bus(k), bus(m), namebr(ndx3),  ci(ndx1), ck(ndx1), cik(ndx1)
4179 format (1x, i3, 2i4, 3( 1x, a6 ), 5x, 3e13.4)
     call window
  end do
  go to 4165
4184 if (to_lower (ansi32(1 : 5)) .ne. 'extra') go to 4197
  write (unit = munit6, fmt = 4187)
4187 format (4x, 'starting-R', 3x, 'starting-L', 3x, 'starting-C', 7x, 'next-R', 7x, 'next-L', 7x, 'next-C')
  call window
  do j = 1, lserlc
     ndx1 = n42 + j
     if (kbus(ndx1) .eq. 0) cycle
     ndx2 = n43 + j
     write (unit = munit6, fmt = 4191) cki(ndx1), ckkjm(ndx1), r(ndx2), tr(ndx2), tx(ndx2), c(ndx2)
4191 format (1x, 6e13.4)
     call window
  end do
  go to 4165
4197 if (to_lower (ansi32(1 : 5)) .ne. 'step ') go to 4211
  kserlc = 1
  go to 4165
4211 if (to_lower (ansi32(1 : 6)) .ne. 'change') go to 4165
4214 write (unit = prom80, fmt = 4215)
4215 format ('    Change type (data, move, blank, use, value,', ' end) :')
  !  assign 34215 to nextsn
  nextsn = 34215
  go to 9800
34215 ansi32 = buff77(1:32)
  if (to_lower (ansi32(1 : 4)) .eq. 'end ') go to 4165
  if (to_lower (ansi32(1 : 6)) .eq. 'series') go to 4165
  if (to_lower (ansi32(1 : 5)) .ne. 'move ')  go to 4223
4216 write (unit = prom80, fmt = 4217)
4217 format ('    Send  n-from, n-to (1, 2, ...) :')
  !  assign 4219 to nextsn
  nextsn = 4219
  go to 9800
4219 bytbuf = buff77(1 : 20)
  if (bytbuf(1 : 4) .eq. 'end ') go to 4214
  call frein2 (bytbuf, n13, n14)
  if (n14 .gt. lserlc) lserlc = n14
  n13 = n13 + n42
  n14 = n14 + n42
  kbus(n14) = kbus(n13)
  mbus(n14) = mbus(n13)
  n24 = 0
  call namea6 (text1, n24)
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
4223 if (to_lower (ansi32(1 : 5)) .ne. 'data ') go to 4232
4225 write (unit = prom80, fmt = 4217)
  !  assign 4227 to nextsn
  nextsn = 4227
  go to 9800
4227 bytbuf = buff77(1 : 20)
  if (to_lower (bytbuf(1 : 4)) .eq. 'end ') go to 4214
  call frein2 (bytbuf, n13, n14)
  if (n14 .gt. lserlc) lserlc = n14
  n14 = n14 + n42
  kbus(n14) = iabs (kbus(n13))
  mbus(n14) = iabs (mbus(n13))
  nr(n14) = 0
  n24 = 0
  call namea6 (text1, n24)
  namebr(n14) = n24
  litype(n14) = n13
  n13 = iabs (nr(n13))
  cki(n14) = tr(n13)
  ckkjm(n14) = tx(n13)
  ci(n14) = tr(n13)
  ck(n14) = tx(n13)
  cik(n14) = c(n13)
  n14 = n14 - n42 + n43
  tr(n14) = tr(n13)
  tx(n14) = tx(n13)
  c(n14) = c(n13)
  r(n14)= c(n13)
  go to 4225
4232 if (to_lower (ansi32(1 : 5)) .ne. 'blank') go to 4245
4235 write (unit = prom80, fmt = 4236)
4236 format ('    Send n-beg and n-end line numbers :')
  !  assign 4237 to nextsn
  nextsn = 4237
  go to 9800
!4237 bytbuf = buff77(1 : 20)
  bytbuf = buff77(1 : 20)
  if (to_lower (bytbuf(1 : 4)) .eq. 'end ') go to 4214
  call frein2 (bytbuf, n13, n14)
  if (n14 .lt. lserlc) go to 4239
  lserlc = n13 - 1
  go to 4235
4239 do j = n13, n14
!4241 kbus(j+n42) = 0
     kbus(j + n42) = 0
  end do
  go to 4235
4245 if (to_lower (ansi32(1 : 4)) .ne. 'use ') go to 4256
  write (unit = munit6, fmt = 4248) (nr(j + n42), j = 1, lserlc)
4248 format ('    nr:', 10i7)
  call window
4251 write (unit = prom80, fmt = 4253)
4253 format ('    Send number to toggle (0=end) :')
  !  assign 4254 to nextsn
  nextsn = 4254
  go to 9800
4254 if (to_lower (buff77(1 : 4)) .eq. 'end ') go to 4214
  call frein1 (buff77, n6)
  if (n6 .le. 0) go to 4214
  n7 = n6 + n42
  nr(n7) = nr(n7) + 1
  if (nr(n7) .ge. 2) nr(n7) = 0
  go to 4251
  !     place any additional subcommands here:
4256 go to 4214
  !     $$$$$$$  key word no. 53:  "lock"     $$$$  $$$$  $$$$  $$$$  $$$$
4436 lockbr = 1
  go to 1240
  !     $$$$$$$  key word no. 54:  "[y]"      $$$$  $$$$  $$$$  $$$$  $$$$
4471 j = 1
4473 write (unit = prom80, fmt = 4474)
4474 format ('   Send row name or number (end) :')
  !  assign 54474 to nextsn
  nextsn = 54474
  go to 9800
54474 prom80 = buff77
  if (to_lower (prom80(1 : 4)) .eq. 'end ') go to 1240
  if (prom80(1 : 6) .ne. '      '  .and. to_lower (prom80(1 : 6)) .ne. 'next  ') go to 4475
  j = j + 1
  go to 4494
4475 if (to_lower (prom80(1 : 4)) .ne. 'top ') go to 4476
  j = 2
  go to 4494
4476 if (to_lower (prom80(1 : 4)) .ne. 'bot ') go to 4477
  j = -9999
  go to 4494
4477 do j = 1, 10
     if (prom80(1 : 1) .eq. digit(j)) go to 4491
  end do
!4478 continue
  read (unit = prom80, fmt = 1380) text2
1380 format (a6)
  do j = 1, kpartb
     if (text2 .eq. bus(j)) go to 4494
  end do
!4483 continue
!4486 write (munit6, 4487)
  write (unit = munit6, fmt = 4487)
4487 format ('  %% %% == ++  Sorry, no such row.  Try again.')
  call window
  go to 4473
4491 call frein1 (prom80, j)
4494 if (answ80(1 : 3) .eq. '[F]') go to 4515
  if (j .le. 1) j = kpartb
  if (j .gt. kpartb) j = 2
  n17 = kks(j) - 1
  n16 = kks(j + 1)
  if (j .eq. kpartb) n16 = kks(1)
  write (unit = munit6, fmt = 4496) j, bus(j), n16, n17
4496 format (' row',  i4, '   name = ', a6, '   limits =', 2i5, '    nonzero  (m, ykm)  follow ....')
  call window
  do l = n16, n17, 4
     n23 = l + 3
     if (n23 .gt. n17) n23 = n17
     write (munit6, 4497) (km(m), ykm(m), m = l, n23)
4497 format (1x, 4(i5, e15.6))
     call window
  end do
!4502 continue
  go to 4473
  !     $$$$$$$  key word no. 55:  "[f]"      $$$$  $$$$  $$$$  $$$$  $$$$
4512 go to 4473
4515 if (j .le. 1) j = kpartb
  if (j .gt. kpartb) j = 2
  n17 = kk(j - 1) + 1
  write (unit = munit6, fmt = 4521) j, bus(j), n17, kk(j)
4521 format (' row', i4, '   name = ', a6, '   limits =', 2i5, '    nonzero  (m, fkm)  follow ....')
  call window
  do l = n17, kk(j), 4
     n23 = l + 3
     if (n23 .gt. kk(j)) n23 = kk(j)
     write (munit6, 4497) (km(m), ykm(m), m = l, n23)
     call window
  end do
!4524 continue
  go to 4512
  !     $$$$$$$  key word no. 56:  "noroll"   $$$$  $$$$  $$$$  $$$$  $$$$
4563 if ( monits .eq. 0 ) go to 4571
  monits = 0
  write (munit6, 4566)
4566 format (6x, '===  Cancel previous rolling printer plot.')
  call window
4571 if (monitr .eq. 0) go to 1240
  monitr = 0
  write (unit = munit6, fmt = 4573)
4573 format (6x, '===  Cancel previous rolling vector plot.')
  call window
  go to 1240
  !     $$$$$$$  key word no. 61:  "choice"   $$$$  $$$$  $$$$  $$$$  $$$$
4592 n17 = nc - nv
  n24 = numnvo + nc + nsmout + ioutcs + numout
  write (unit = munit6, fmt = 4595) n24
4595 format ('     Total number of EMTP outputs (sum of 5 classes) =', i5)
  call window
  write (unit = munit6, fmt = 4596)
4596 format ( '     Ordered class limits :   numnvo      nv   nc-nv  nsmout  ioutcs   numout')
  call window
  write (unit = munit6, fmt = 4597) numnvo, nv, n17, nsmout, ioutcs, numout
4597 format (28x, 6i8)
  call window
  write (unit = munit6, fmt = 4608) numnvo
4608 format ('     Selective node voltage outputs (cells  1  through', i3, ' ) :')
  call window
  do j = 1, numnvo, 7
     n17 = j + 6
     if (n17 .gt. numnv0) n17 = numnvo
     write (unit = munit6, fmt = 4609) (bus(ibsout(m)), m = j, n17)
4609 format (7x, 10a7)
     call window
  end do
!4611 continue
  n7 = numnvo + 1
  n8 = numnvo + nv
  if (n8 .lt. n7) go to 4631
  write (unit = munit6, fmt = 4615) n7, n8
4615 format ('     Branch voltage node-name pairs (cells', i3, '  through', i3, ' ) :')
  call window
  n9 = 7878
  call spyout ( n9, n9 )
  do j=1, nv
!4623 call spyout ( ibrnch(j), jbrnch(j) )
     call spyout (ibrnch(j), jbrnch(j))
  end do
  n9 = -7878
  call spyout ( n9, n9 )
4631 n7 = numnvo + nv + 1
  n8 = numnvo + nc
  write (munit6, 4636)  n7, n8
4636 format ('     Element current node-name pairs (cells', i3, '  through', i3, ' ) :')
  call window
  if ( n17 .le. 0 ) go to 4658
  n9 = 7878
  call spyout ( n9, n9 )
  do j = 1, kswtch
     if (kmswit(lswtch + j) .gt. 0) cycle
     call spyout (kmswit(j), kmswit(lswtch + j))
  end do
  do j = 1, inonl
     if (nonlm(j) .gt. 0) cycle
     call spyout (nonlk(j), nonlm(j))
  end do
  do j = 1, ibr
     if (mbus(j) .gt. 0) cycle
     call spyout (kbus(j), mbus(j))
  end do
  n9 = -7878
  call spyout (n9, n9)
4658 go to 1240
  !     $$$$$$$  key word no. 62:  "tacs"     $$$$  $$$$  $$$$  $$$$  $$$$
4716 write (prom80, 4717)
4717 format (' Send control (rewind, source, supplemental, patch, show, end) :')
  !  assign 4719 to nextsn
  nextsn = 4719
  go to 9800
4719 prom80 = buff77
  if (prom80(1 : 4) .eq. 'end ') go to 1240
  if (prom80(1 : 6) .ne. 'rewind') go to 4724
  niu = konsce
  nsup = konsup
  ktab = kofsce
  koncur = 0
  go to 4716
4724 if (prom80(1 : 6) .ne. 'source') go to 4726
  koncur = 1
  go to 4734
4726 if (prom80(1 : 6) .ne. 'supple') go to 4752
  koncur = 2
4734 n23 = lunit(5)
  lunit(5) = munit5
  !     read input data card using cimage.
4738 call cimage
  read (unit = abuff, fmt = 2788) ansi8
  if (ansi8(1 : 4) .ne. 'end ') go to 4741
  lunit(5) = int (n23, kind (lunit(5)))
  go to 4716
4741 if (ansi8(1 : 4) .ne. 'spy ') go to 4745
  lunit(5) = int (n23, kind (lunit(5)))
  go to 1240
4745 if (koncur .eq. 1) call tacs1
  if (koncur .eq. 2) call tacs1a
  go to 4738
4752 if (prom80(1 : 6) .ne. 'patch ') go to 4792
  n11 = location (kar1)
  n12 = location (volti)
  n13 = (n12 - n11) / 2
  if (iprspy .le. 1) go to 34753
  write (munit6, 4753) n11, n12, n13
4753 format (' volti memory indices.  n11, n12, n13 =', 3i8, '    fkar1(j), j=n13+1, n13+9) ....')
  call window
  write (munit6, 24753) (fkar1(j), j = n13 + 1, n13 + 9)
24753 format (9e14.4)
  call window
34753 n14 = 0
4754 write (prom80, 4757)
4757 format (' Send source name, address (a6, i6), or "end":')
  !  assign 4759 to nextsn
  nextsn = 4759
  go to 9800
4759 prom80 = buff77
  if (prom80(1 : 4) .eq. 'end ') go to 4776
  read (prom80, 4763) bus1, n7
4763 format (a6, i6, e8.0)
  do j = 1, niu
     ndx3 = ilntab(kaliu + j)
     bus2 = texvec(ndx3)
     if (bus1 .ne. bus2) cycle
     n14 = n14 + 1
     ndx4 = j + kxtcs + nuk
     kontac(n14) = ndx4
     epskon(n14) = 0.0
     konadd(n14) = n7
     go to 4754
  end do
  write (unit = munit6, fmt = 4771)
4771 format ('  ? ? ?   Sorry, no such TACS source named  "', a6,  '".   Try again ....')
  call window
  go to 4754
4776 write (prom80, 4777)
4777 format (' Send usage name, address, tolerance (a6, i6, e8.0), or "end":')
  !  assign 4778 to nextsn
  nextsn = 4778
  go to 9800
4778 prom80 = buff77
  if (prom80(1 : 4) .ne. 'end ') go to 4780
  kontot = n14
  go to 4716
4780 read (prom80, 4763) bus1, n7, d13
  do j = 1, ktab
     ndx1 = kxtcs + j
     ndx2 = ilntab(klntab + j)
     if (bus1 .ne. texvec(ndx2)) cycle
     if (koncur .eq. 0) koncur = n14
     n14 = n14 + 1
     kontac(n14) = ndx1
     konadd(n14) = n7
!     epskon(n14) = ichar(d13)
     epskon(n14) = ichar (d13(1 : 1))
     go to 4776
  end do
  write (unit = munit6, fmt = 4788)
4788 format ('  ? ? ?   Sorry, no such TACS output named  "', a6,  '".   Try again ....')
  call window
  go to 4776
4792 if (prom80(1 : 5) .ne. 'show ') go to 4807
  write (munit6, 4794) koncur, kontot
4794 format (' Concurrent sequential processing (csp) controls.  koncur =',  i4,  5x, 'kontot =', i4)
  call window
  write (munit6, 4795)
4795 format ('     row    name   index  memory    threshold')
  call window
  do i = 1, kontot
     n13 = ilntab(kontac(i) + klntab - kxtcs)
     write (munit6, 4798) i, texvec(n13), kontac(i), konadd(i), epskon(i)
4798 format (i8, 2x, a6, 2i8, e13.4)
     call window
  end do
!4801 continue
  !     any additional responses to tacs prompt go here:
4807 go to 4716
  !     $$$$$$$  key word no. 64:  "v-i"      $$$$  $$$$  $$$$  $$$$  $$$$
4823 n17 = 0
  n8 = 0
  write (unit = munit6, fmt = 4825) inonl, i_char
4825 format (' lists 9, 10 =', 2i4, '      (next, <cr>, last, all, mode).')
  call window
4824 write (prom80, 4827)
4827 format (1x, ' row        vchar        cchar       gslope row nltype nonlad nonle  class :')
  !  assign 4828 to nextsn
  nextsn = 4828
  go to 9800
!4828 bytbuf = buff77(1 : 20)
  bytbuf = buff77(1 : 20)
  if (bytbuf(1 : 4) .eq. 'end ') go to 1240
  if (bytbuf(1 : 4) .eq. 'stop') go to 1240
  if (bytbuf(1 : 4) .ne. 'next' .and. bytbuf(1 : 4) .ne. '    ') go to 4829
  n17 = n17 + 1
  if (n17 .gt. inonl) n17 = 1
  go to 4844
4829 if (bytbuf(1 : 4) .ne. 'last') go to 4831
  n17 = n17 - 1
  if (n17 .le. 0) n17 = inonl
  go to 4844
4831 if (bytbuf(1 : 4) .ne. 'all ') go to 4832
  n1 = 1
  n2 = i_char
  go to 4854
4832 if (bytbuf(1 : 4) .ne. 'mode') go to 4834
  n8 = n8 + 1
  if (n8 .ge. 2) n8 = 0
  write (munit6, 4833) n8
4833 format ('    ----  New mode flag n8 =', i2)
  call window
  go to 4824
4834 call frein2(bytbuf, n1, n2)
  if (n8 .eq. 1) go to 4854
  do n17 = 1, inonl
     if (iabsz(nonlad(n17)) .lt. n1) cycle
     if (iabsz(nonlad(n17)) .le. n2) go to 4844
  end do
  go to 4824
4844 n1 = iabsz (nonlad(n17))
  n2 = iabsz (nonle(n17))
  ansi8(1 : 6) = 'pseudo'
  if (nltype(n17) .gt. 0) ansi8(1 : 6) = ' true '
  write (munit6, 4851) n1, vchar(n1), cchar(n1), gslope(n1), n17, nltype(n17), nonlad(n17), nonle(n17), ansi8(1 : 6)
4851 format (1x, i4, 3e13.4, i4, 2i7, i6, 1x, a6)
  call window
  n1 = n1 + 1
4854 do j = n1, n2
     write (munit6, 4851) j, vchar(j), cchar(j), gslope(j)
     call window
     call quiter
     if (kwtspy .eq. 0) cycle
     kwtspy = 0
     go to 4824
  end do
  go to 4824
1240 nexmod = 0
  go to 9804
9800 nexmod = 3
9804 if (iprspy .lt. 1) go to 9999
  write (unit = munit6, fmt = 9807) jjroll, kbreak, lockbr, nchain, prom80(1 : 20)
9807 format (' Exit "spyink".  jjroll, kbreak, lockbr, nchain =', 4i5,  '   prom80(1:20) =', a20)
  call window
9999 return
  entry spytac
  !     called only by "subts3", once, for tacs csp application.
  do j = 1, koncur
     n7 = kontac(j)
     n13 = konadd(j)
!3472 xtcs(n7) = fkar1(n13)
     xtcs(n7) = fkar1(n13)
  end do
  call tacs3
  do j = koncur + 1, kontot
     n7 = kontac(j)
     n13 = konadd(j)
     d8 = fkar1(n13) - xtcs(n7)
     if (absz(d8) .le. epskon(j)) cycle
     fkar1(n13) = xtcs(n7)
     ialter = 1
  end do
  if (iprspy .lt. 1) go to 3496
  write (munit6, 3493) koncur, kontot, ialter
3493 format (' Exit "spytac" after csp.  koncur, kontot, ialter =',  3i5)
  call window
3496 return
end subroutine spyink

!
! subroutine initsp.
!

subroutine initsp
  use blkcom
  use dekspy
  implicit none
  !     Module of interactive EMTP only, which services "emtspy".
  !     If no interactive emtp use, this module can be deleted.
  !     Universal initialization module for "spying".   Constants
  !     must be set only once, at the beginning of execution only.
  character(8) :: textay(75)
  integer(4) :: j, n3
  real(8) :: tdroll
  !
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
  call dimens (memrmp(1), n3, bus1, bus1)
  limbuf = memrmp(2)
  if (iprspy .lt. 1) go to 1144
  write (unit = munit6, fmt = 1143) limbuf
1143 format (' Near top of "initsp".  limbuf =', i8)
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
  do j = 1, 9999
     if (textay(j) .eq. '        ') go to 1152
!1148 spykwd(j) = textay(j)
     spykwd(j) = textay(j)
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
  call locating
  do j = 1, 9
     filext(j) = ' '
     write (ansi32, 1283)          j
1283 format ('inclspy', i1, '.dat', 13x)
     inquire (file = ansi32, exist = logvar)
     if (logvar) filext(j) = 'x'
     if (iprspy .lt. 1) cycle
     write (unit = munit6, fmt = 2789) j, ansi32
2789 format (' Next use of "inquire".  j, ansi32 =', i5, 1x, a32)
     call window
  end do
  kwtspy = 0
  jjroll = 0
  tdroll = 1.0d0
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
  use comkwt
  use blkcom
  use dekspy
  implicit none
  !     Module of interactive EMTP only, which services "emtspy".
  !     if no interactive use, convert to dummy module ("return").
  !     VAX-11 installation-dependent EMTP module which serves
  !     to read spy command from munit5 if: 1) ctrl-c interrupt
  !     has occurred, or 2) if  lockbr = 1  upon entry.
  integer(4) :: ios
  !  dimension idum(3)                                      !  dummy vector for ctrl-c handling
  !
  external kwiter                                           ! needed for ctrl-c initialization
  if (iprspy .lt. 10) go to 3456                            ! jump around diagnostic
  write (unit = munit6, fmt = 3409) istep, kwtspy, itype, lastov
3409 format (' Top flager.  istep, kwtspy, itype, lastov =', 4i6)
  call window                                               ! output of character variable munit6
3456 if (lastov .ne. 9911) go to 3614                       ! not "pltfil" overflow
  buff77 = 'space'                                          ! next spy command we want to execute
  kwtspy = 1                                                ! set flag for global emtp use (remember)
  kfile5 = 2                                                ! signal to "spying" that buff77 now read
  go to 3651                                                ! exit module after possible diagnostic
3614 if (kwtvax .eq. 0) go to 3642                          ! no user-keyed interrupt
  write (prom80, 3462)                                      ! prompt for spy keyboard input
3462 format (' spy:')
  call prompt                                               ! write prom80 with cursor control (no lf)
  kwtvax = 0                                                ! reset flag of ctrl-c interception for next
                                                            !     call enable_ctrl_c ( kwiter, idum(1) )  ! re-enable it
  if (iprspy .lt. 1) go to 3491                             ! jump around diagnostic
  write (munit6, 3487)
3487 format (' Enable VAX ctrl-c interception in "flager" .')
  call window                                               ! output of character variable munit6
3491 go to 3643                                             ! jump to read from unit munit5 (keyboard)
3642 if (lockbr .ne. 1) go to 3651                          ! no forced input
  if (kfile5 .eq. 1) go to 3651                             ! "@" read out in emtspy
3643 read (unit = munit5, fmt = 3647, iostat = ios) buff77  ! read next spy input
3647 format (a80)
  kwtspy = 1                                                ! set flag for global emtp use (remember)
  kfile5 = 2                                                ! signal to "spying" that buff77 now read
  if (kspsav .eq. 0) go to 3650                             ! no echoing of spy command
  if (buff77(1:7) .ne. 'cancel ') go to 3648
  kspsav = kspsav + 1                                       ! erase previous, erroneous command
  go to 3643                                                ! loop back for another, a real, command
3648 kspsav = kspsav - 1                                    ! next file6 cell to save buff77 in
  if (kspsav .gt. numcrd) go to 3649                        ! no overlap yet
  write (*, *) ' Error stop; overflow in "flager".'
  call stoptp                                               ! installation-dependent fortran stop
3649 file6(kspsav) = buff77                                 ! accumulate user-keyed spy input
3650 go to 3651                                             ! exit module with 80-col. buff77 card now read
3651 if (iprspy .lt. 9) go to 9000
  write (unit = munit6, fmt = 9004) kwtspy, buff77
9004 format (' Exit "flager".  kwtspy =', i4, '   buff77 = ', a80)
  call window                                               ! output of character variable munit6
9000 return
end subroutine flager

!
! subroutine quiter.
!

subroutine quiter
  use dekspy
  implicit none
  !     Module of interactive EMTP only, which services "emtspy".
  !     if no interactive use, convert to dummy module ("return").
  !     this module provides a special-purpose connection to the
  !     more general "flager".  Here, we only want to sense a
  !     user-keyed interrupt (no spy input is to be read).
  integer(4) :: n24, n25
  !
  n24 = lockbr   ! save current time-slicing flag value
  n25 = kfile5   ! save current status of input connection
  lockbr = 0     ! temporarily turn time-sharing on
  call flager    ! check for user-keyed interrupt, and return
  lockbr = n24   ! restore original value of b4 flager use
  kfile5 = n25   ! restore original value of b4 flager use
  return
end subroutine quiter

!
! subroutine honker.
!

subroutine honker (klevel)
  use blkcom
  use dekspy
  use bcdtim
  use bcddat
  implicit none
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
  integer(4), intent(in) :: klevel
  integer(4) :: nd13
  real(8) :: d13
  character(4) :: spytim(2), spdate(2)
  !
  d13 = 2.0d0                                               ! initialize time delay at two seconds
  call time44 (spytim)                                      ! emtp wall-clock time
  call date44 (spdate)                                      ! emtp date utility
  buff77(1 : 1) = char(7)                                   ! ascii "7" = ctrl-g of keyboard = 1 bell
  buff77(2 : 2) = buff77(1 : 1)                             ! define 2nd bell, if needed
  buff77(3 : 3) = buff77(1 : 1)                             ! define 3rd bell, if needed
  buff77(4 : 6) = buff77(1 : 3)                             ! define bells number 4 to 6
  buff77(7 : 10) = buff77(1 : 4)                            ! define bells number 7-10
3608 write (munit6, 3627) spytim, spdate, klevel, buff77(1 : klevel)
3627 format ( ' Audible alarm began at  ', 2a4, 2x, 2a4, 5x, i3, ' bells.', a)
  call window                                               ! output of character variable munit6
  if (klevel .lt. 10) go to 9000                            ! exit module (no loop)
  nd13 = int (d13, kind (nd13))
  call tdelay (nd13)                                        ! now stall for d13 seconds
  if (d13 .eq. -7654) go to 9000                            ! interrupt in "tdelay"
  call flager                                               ! check for user-keyed interrupt signal
  if (kwtspy .eq. 0) go to 3643                             ! no user abort of alarm
  kwtspy = 0                                                ! reset interrupt indicator as we begin service
  go to 9000                                                ! jump out of loop (abort honking)
3643 d13 = 1.5 * d13                                        ! lengthen delay after next ten bells
  go to 3608                                                ! loop back to repeat string of 10 bells
9000 return
end subroutine honker

!
! subroutine tdelay.
!

subroutine tdelay (d8)
  use blkcom
  implicit none
  !     Module of interactive EMTP only, which services "emtspy".
  !     If no interactive use, this module can be deleted.
  !     VAX-11   module designed to stall for  d8  seconds.
  !     Present use of disk writes is temporary only, and should
  !     later be replaced by a less-wasteful, true hibernation.
  !  include 'blkcom.ftn'
  integer(4), intent(out) :: d8
  integer(4) :: j, k, n23
  n23 = d8                                                  ! integer number of seconds for hibernation
  do j = 1, n23                                             ! loop once for each second of delay
     do k = 1, 6                                            ! one second = about 6 writes to disk
        rewind 36                                           ! rewind unused, dummy i/o channel 36
        write (unit = 36, fmt = 3629) k                     ! dummy write to disk
     end do
3629 format (i2)
     call quiter                                            ! check for user-keyed interrupt signal
     if (kwtspy .eq. 0) cycle                               ! no user abort of alarm
     kwtspy = 0                                             ! reset interrupt indicator as we begin service
     d8 = -7654                                             ! argument flag remembering abort (for honker)
     go to 9000                                             ! jump out of time-delay loop, to return
  end do
9000 return
end subroutine tdelay

!
! subroutine kwiter.
!

subroutine kwiter (idum)
  use comkwt
  use dekspy
  implicit none
  !     VAX-11  installation-dependent EMTP module which serves
  !     control interactive usage.  if none, destroy the module.
  !     Purpose is to sense user-keyed interrupt, and set flag.
  !     Name "comkwt" is reserved (connected to "controlc.obj")
  integer(4), intent(in) :: idum(3)
  !
  kwtvax = 1
  write (unit = *, fmt = *) idum(1 : 3)
  return
end subroutine kwiter

!
! subroutine percnt.
!

subroutine percnt (vbyte, n7)
  use dekspy
  implicit none
  !     Module of interactive emtp usage only, which services "emtspy".
  !     Utility which serves to replace  "%%%%%%%%"  strings of disk
  !     files by parameters of  "@?"  call.   columns 1, ... n7  are
  !     searched, of character vector  "vbyte" .
  !     For non-interactive emtp, this module can be destroyed.
  character(*), intent(out) :: vbyte
  integer(4), intent(in) :: n7
  integer(4) :: i, j, k
  !
  do k = 1, n7
     if (vbyte(k : k) .ne. '%') go to 1297
     do j = 1, 7
        if (vbyte(k + j : k + j) .ne. '%') go to 1297
     end do
     !1253 continue
     !     we exit  do 1253  with string of 8 "%" beginning in column k
     itexp = itexp + 1
     if (itexp .le. maxarg) go to 1284
     write (unit = munit6, fmt = 1274) maxarg
1274 format (' ????  Trouble.  "@?"  Usage only defined', i4, '   arguments, while the disk')
     call window
     write (unit = munit6, fmt = 1275)
1275 format ('                 File has more  %-strings.   Trouble detected in following:')
     call window
     write (unit = munit6, fmt = 1277) (vbyte(i : i), i = 1, n7)
1277 format ('              >>>', 80a1)
     call window
     kilper = 1
     go to 1313
1284 ansi8 = texpar(itexp)
     read (unit = ansi8, fmt = 1296) (vbyte(k + j - 1 : k + j - 1), j = 1, 8)
1296 format (80a1)
1297 continue
  end do
  if (kverfy .ne. 0) go to 1313
  write (unit = munit6, fmt = 1306) (vbyte(j : j), j = 1, n7)
1306 format (' @>>>', 80a1)
  call window
1313 return
end subroutine percnt

!
! subroutine numchk.
!

subroutine numchk (vbyte, nchar, kill)
  use dekspy
  implicit none
  !     Module of interactive EMTP usage only, which services "emtspy".
  !     This utility serves to scrutinize the input character string
  !     (vbyte(j), j=1, nchar)  to see if it is a legal floating-point
  !     number.   If so,  "kill"  is to be set to zero;  if number is
  !     structurally deficient,  "kill"  is to be set positive.
  !     For non-interactive emtp, this module can be destroyed.
  integer(4), intent(in) :: nchar
  integer(4), intent(out) :: kill
  character(1), intent(in) :: vbyte(*)
  integer(4) :: i, j, kk, koldig, kolexp, kolper, nper, nsign, nume
  !
  kill = 0
  koldig = 0
  nper = 0
  nsign = 0
  nume = 0
  kk = 0
  do i = 1, nchar
     if (vbyte(i) .eq. ' ') cycle
     kk = kk + 1
     if (vbyte(i) .ne. '+' .and. vbyte(i) .ne. '-') go to 3412
     !     following code considers ramifications of just-found "+" or "-" :
     nsign = nsign + 1
     if (nsign .gt. 2) kill = 1
     if (kk .eq. 1) cycle
     if (kolexp .eq. i - 1) cycle
     kill = 1
     cycle
3412 if (vbyte(i) .ne. 'e' .and. vbyte(i) .ne. 'd') go to 3425
     !     following code considers ramifications of just-found "d" or "e":
     nume = nume + 1
     kolexp = i
     if (nume .gt. 1) kill = 1
     cycle
3425 if (vbyte(i) .ne. '.') go to 3428
     !     following code considers ramifications of just-found decimal point
     nper = nper + 1
     if (nper .gt. 1) kill = 1
     kolper = i
     cycle
3428 do j = 1, 10
        if (vbyte(i) .eq. digit(j)) go to 3438
     end do
!3431 continue
     kill = 1
     cycle
     !     following code considers ramifications of just-found digit:
3438 koldig = i
  end do
  if (nume .eq. 1 .and. kolexp .gt. koldig) kill = 1
  if (nume .eq. 1 .and. kolper .gt. kolexp) kill = 1
  if (kill .eq. 0) return
  write (unit = munit6, fmt = 3492) (vbyte(j), j = 1, nchar)
3492 format (' ??? Sorry, illegal numeric just read:', 50a1)
  write (unit = munit6, fmt = 3493)
3493 format ('            Make a second try, please ....')
  return
end subroutine numchk

!
! subroutine getnum.
!

subroutine getnum (num)
  use dekspy
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     Module of interactive emtp usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  integer(4), intent(out) :: num
  integer(4) :: i
  integer(4) :: j
  integer(4) :: n1
  character(1) :: c4
  !
  if (iprspy .lt. 1) go to 4204
  write (unit = munit6, fmt = 4203) ibegcl, bytbuf(ibegcl:ibegcl)
4203 format (' Begin  "getnum".   ibegcl =', i5, '      bytbuf(ibegcl) =', a1)
  call window
4204 n1 = 1
  num = 0
  do i = ibegcl, 20
     c4 = bytbuf(i:i)
     if (c4 .eq. ' ') go to 4286
     if (c4 .eq. ':') go to 4286
     if (c4 .eq. '#') go to 4286
     if (c4 .eq. ',') go to 4286
     if (c4 .eq. '+') cycle
     if (c4 .ne. '-') go to 4218
     n1 = -1
     cycle
4218 do j = 1, 10
        if (c4 .eq. digit(j)) go to 4256
     end do
!4234 continue
     write (unit = munit6, fmt = 4239) i, c4
4239 format ('    -- Illegal byte in "number".', 1x,  i4,  3x,  a1,  3x,  'Try again ...')
     call window
     num = -87654
     go to 4294
4256 if (j .eq. 10) j = 0
     num = 10 * num  +  j
     if (iprspy .lt. 2) cycle
     write (unit = munit6, fmt = 4259) j, num
4259 format (' Next digit.  j, num =', 2i8)
     call window
  end do
  write (unit = munit6, fmt = 4271) num
4271 format (' Stop at 4271 of "getnum".  num =', i8)
  call window
  call stoptp
4286 if (n1 .lt. 0) num = -num
  iendcl = i - 1
  if (iprspy .lt. 1) go to 4294
  write (unit = munit6, fmt = 4293) iendcl, num
4293 format (' Exit "number".   iendcl, num =', 2i8)
  call window
4294 return
end subroutine getnum

!
!     subroutine window.
!

subroutine window
  use blkcom
  use dekspy
  implicit none
  !     Module of interactive emtp usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  !     For character*132 spy display channel munit6, this serves
  !     to actually output the line to the spy display device.
  !     VAX-11  installation-dependent emtp module.
  integer(4) :: j, k
  !
  !     temporarily, until we learn how to write to a 2nd crt for
  !     vax/vms, we will just write to lunit6:
  if (iabs (kverfy) .eq. 34543) go to 9000        ! no spy windows
  do j = 1, 132                                   ! search line for right most non-blank
     k = 133 - j                                  ! reverse index (step from right to left)
     if (munit6(k : k) .ne. ' ') go to 5621       ! end of line
  end do
5621 write (unit = lunit(6), fmt = *) munit6(1 : k) ! output nonblank part
9000 return
end subroutine window

!
! subroutine spylin.
!

subroutine spylin
  use blkcom
  use dekspy
  implicit none
  !     Module of interactive emtp usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  !     One blank line is written on spy screen by this module.
  !     Temporarily, until we learn how to write to a 2nd crt for
  !     VAX/VMS, we will just write to lunit6 in universal form:
  !5621 write (unit = lunit6, fmt = 5624)
  write (unit = lunit(6), fmt = 5624)
5624 format (1x)
end subroutine spylin

!
! subroutine spyout.
!

subroutine spyout(n1, n2)
  use labcom
  use dekspy
  implicit none
  !     Module of interactive EMTP only, which services "emtspy".
  !     If no interactive use, this module can be deleted.
  !     Arguments n1 and n2 are node nos., with possible "-" signs.
  !     Purpose is to load and print corresponding names for "output"
  integer(4), intent(in) :: n1, n2
  integer(4) :: k, n11, n12
  save
  character(8) :: text10(10)
  integer(4) :: j
  !
  !  data terra / 'terra ' /
  n11 = n1
  n12 = n2
  !     first check for special initialization or flushing calls:
  if (n11 .ne. 7878) go to 4618
  k = 0
  go to 9000
4618 if (n11 .ne. -7878) go to 4626
4621 if (k .le. 0) go to 9000
  write (unit = munit6, fmt = 4629) (text10(j), j = 1, k)
4629 format (7x, 10a7)
  call window
  k = 0
  go to 9000
  !     insert first node name into buffer:
4626 if (n11 .lt. 0) n11 = -n11
  k = k + 1
  text10(k) = bus(n11)
  if (n11 .eq. 1) text10(k) = terra
  if (k .eq. 10) go to 4621
  if (n12 .eq. 0) go to 9000
  if (n12 .lt. 0) n12 = -n12
  k = k + 1
  text10(k) = bus(n12)
  if (n12 .eq. 1) text10(k) = terra
  if (k .eq. 10) go to 4621
9000 if (iprspy .lt. 3)  go to 9007
  write (unit = munit6, fmt = 9004) n1, n2, k, n11, n12
9004 format (' Exit "spyout".  n1, n2, k, n11, n12 =', 5i6)
  call window
9007 return
end subroutine spyout

!
! subroutine examin.
!

subroutine examin
  use dekspy
  implicit none
  !     Module of interactive EMTP usage only, which services "emtspy".
  !     For non-interactive emtp, this module can be destroyed.
  !     This near-universal module serves to build the character*132
  !     output vector outlin of the "examine" command of spy.
  !     Computers with index problems (e.g., prime) need replacement
  integer(4) :: jj, n3, n5, n8, n9, n10, n17
  !
  if (iprspy .lt. 1) go to 1718
  write (unit = munit6, fmt = 1707) numex, imin(1), locout(1)
1707 format (' Top of "examin".  numex, imin(1), locout(1) =', 3i6)
  call window
1718 jj = 0
  outlin(1 : 1) = ' '
  kolout = 2
  if (numex .le. 0) go to 9000
1540 jj = jj + 1
  n5 = imin(jj)
  n3 = locout(jj)
  n8 = locate(n3) + n5 - 1
  if (intout(jj) .eq. 0) n8 = n8 + n5 - 1
  if (iprspy .lt. 3) go to 1560
  write (unit = munit6, fmt = 1544) jj, n5, n3, n8, intout(jj)
1544 format (' Next  examine.  jj, n5, n3, n8, intout(jj)', 5i8)
  call window
1560 if (intout(jj) .eq. 0) go to 1600
  n9 =  n8 - memkar
  n8 = n8 + 1
  if (ivec(n3) .eq. 1) go to 1577
  if (n5 .eq. 1 .and. imax(jj) .eq. 1) go to 1640
1577 write (unit = ansi16, fmt = 1580) kar1(1 + n9)
1580 format (i10)
  outlin(kolout : kolout + 9) = ansi16(1 : 10)
  outlin(kolout + 10 : kolout + 14) = blan80(1 : 5)
  kolout = kolout + 15
  go to 1664
1600 n9 = (n8 - memkar) / 2
  n10 = memkar + 2 * n9 - n8
  n8 = n8 + 2
  if (n10 .ne. 0) go to 1628
  !     following real(8) extractions line up (no need for 4-byte shift):
  if (iascii(n3) .eq. 0) write (unit = ansi16, fmt = 1620) fkar1(1 + n9)
1620 format (e15.6)
  if (iascii(n3) .eq. 1) write (unit = ansi16, fmt = 1624) fkar1(1 + n9)
1624 format (4x, '"', a6, '"', 3x)
  go to 1633
  !     Following real(8) extractions require a 4-byte shift (fkar2 use).
  !     fkar2 has higher memory address than fkar1, so it is correct with
  !     n9 offset if n9 is positive.   But if n9 is negative, the half
  !     word shift really should be toward lower addresses, which we
  !     compensate for by the continued use of fkar2 but with one smaller
  !     n9 value:
1628 if (n9 .lt. 0) n9 = n9 - 1
  if (iascii(n3) .eq. 0) write (unit = ansi16, fmt = 1620) fkar2(1 + n9)
  if (iascii(n3) .eq. 1) write (unit = ansi16, fmt = 1624) fkar2(1 + n9)
1633 outlin(kolout : kolout + 14) = ansi16(1 : 15)
  kolout = kolout + 15
  go to 1664
1640 write (unit = ansi8, fmt = 1660) kar1(1 + n9)
1660 format (i6)
  outlin(kolout : kolout + 5) = ansi8(1 : 6)
  if (iprspy .lt. 3) go to 1662
  write (unit = munit6, fmt = 1661) n9, kar1(1 + n9)
1661 format (' i6 integer encoded for examine.  n9,kar1(1 + n9) =', 2i8 )
  call window
1662 kolout = kolout + 6
1664 n5 = n5 + 1
  if (n5 .le. imax(jj)) go to 1560
  if (jj .lt. numex) go to 1540
  if (kolout .lt. 132) outlin(kolout : 132) = ' '
  if (iprspy .lt. 1) go to 9000
  n17 = kolout
  if (n17 .gt. 80) n17 = 80
  write (unit = munit6, fmt = 8872) kolout, outlin(1 : n17)
8872 format (' Exit "examin".   kolout =', i5, '    outlin(1:80) =', a)
  call window
9000 return
end subroutine examin

!
! subroutine deposi.
!

subroutine deposi (ind, intype, n1, n2, d4)
  use dekspy
  implicit none
  !     Module of interactive EMTP usage only, which services "emtspy".
  !     For non-interactive EMTP, this module can be destroyed.
  !     This near-universal module services "deposit", to actually
  !     perform the tampering.   Symbol is in row ind.  if it is
  !     of type alphanumeric, ansi8(1 : 6) in "dekspy" carries the
  !     new contents.  Otherwise, d4 is the numeric value to be
  !     deposited, and intype gives the mode (1=integer, 0=real).
  !     Bounding subscripts on the deposit are n1 and n2, respectively.
  !     Computers with index problems (e.g., prime) need replacement
  integer(4), intent(in) :: ind, intype
  integer(4), intent(out) :: n1, n2
  real(8), intent(out) :: d4
  integer(4) :: n8, n9, n10
  !
  if (iprspy .lt. 1) go to 1846
  write (unit = munit6, fmt = 1707) ind, intype, iascii(ind), d4, ansi8
1707 format (' Top of "deposi".   ind, intype, iascii(ind), d4, ansi8 =', 3i8, e15.4, 3x, a8)
  call window
1846 n8 = locate(ind)
  !     following use of real(8) deposit logic to also handle
  !     alphanumeric only works for computers with this equality:
  if (iascii(ind) .eq. 1) read (unit = ansi8, fmt = 1849) d4
1849 format (a6)
  if (intype .eq. 0) go to 1880
  n9 = n8 - memkar + n1
  !     enter loop of integer deposits, subscripts n1 through n2:
1854 kar1(n9) = int (d4, kind (kar1))
  n1 = n1 + 1
  n9 = n9 + 1
  if (n1 .le. n2) go to 1854
  go to 8000
  !     our vector is  real(8), so requires the following special deposit:
1880 n9 = (n8 - memkar) / 2 + n1
  n10 = memkar + 2 * (n9 - n1) - n8
  !     enter loop of real deposits, subscripts n1 through n2:
1904 if (n10 .gt. 0) fkar2(n9) = d4
  if (n10 .eq. 0) fkar1(n9) = d4
  n1 = n1 + 1
  n9 = n9 + 1
  if (n1 .le. n2) go to 1904
8000 if (iprspy .lt. 1) go to 9000
  write (unit = munit6, fmt = 8872)
8872 format (' Exit "deposi".')
  call window
9000 return
end subroutine deposi

!
! subroutine append.
!

subroutine append
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     module connected to key word "append" of spy.  others
  !     can perfect and use their own installation-dependent
  !     (and perhaps proprietary) extensions via this module.
  return
end subroutine append

!
! subroutine intpar.
!

subroutine intpar (max, n1, n2, kill)
  use dekspy
  implicit none
  !     Module of interactive EMTP usage only, which services "emtspy".
  !     this module is designed to extract two free-format row numbers
  !     from  bytbuf(20)  input buffer of common.   These numbers must
  !     be positive, ordered (1st less than or equal to 2nd), and not
  !     in excess of the last table row "max".   The to row numbers are
  !     passed back as  "n1"  and  "n2"  arguments.   if this extraction
  !     was successful,  "kill"  is set to zero;  if it failed,  kill = 1.
  !     for non-interactive emtp, this module can be destroyed.
  integer(4), intent(in) :: max
  integer(4), intent(out) :: n1, n2, kill
  integer(4) :: i, j, n6, n13, n17, n22, n33
  !
  kill = 0
  !     check for english request "top" (for the top of the file):
  if (bytbuf(1 : 1) .ne. 't') go to 2071
  n1 = 1
  n2 = 1
  go to 2110
  !     check for english request "bot" (for the bottom of the file):
2071 if (bytbuf(1 : 1) .ne. 'b') go to 2075
  n1 = max
  n2 = max
  go to 2110
  !     check for english request "all" (for the entire file):
2075 if (bytbuf(1 : 1) .ne. 'a') go to 2088
  n1 = 1
  n2 = max
  go to 2110
2088 do i = 1, 20
     n13 = 21 - i
     if (bytbuf(n13 : n13) .eq. ' ') cycle
     if (bytbuf(n13 : n13) .ne. ',') go to 2054
     n6 = 20 - i
     do j = 1, n6
        n13 = 21 - i + j
        bytbuf(n13 : n13) = bytbuf(j : j)
     end do
     go to 2054
  end do
2054 n22 = 0
  n13 = 0
  do i = 1, 20
     n17 = i - 1
     if (bytbuf(i : i) .ne. ' ') go to 2091
     !     blank is ignored if before 1st digit, of if not 1st of string:
     if (n22 .eq. 0) go to 2104
     if (bytbuf(n17 : n17) .eq. ' ') go to 2104
     if (bytbuf(n17 : n17) .eq. ',') go to 2104
     n13 = n13 + 1
     go to 2104
     !     begin processing of non-blank character  bytbuf(i) :
2091 n22 = n22 + 1
     if (bytbuf(i : i) .ne.  ',') go to 2093
     if (bytbuf(n17 : n17) .eq. ' ') go to 2104
     n13 = n13 + 1
     go to 2104
2093 do j = 1, 10
        if (bytbuf(i : i) .eq. digit(j)) go to 2104
     end do
2104 continue
  end do
  if (n22 .gt. 0) go to 2109
  !     blank response is interpreted as a request for more of same:
  n1 = n2 + 1
  if (n1 .le. max) go to 2107
  write (unit = munit6, fmt = 2105)
2105 format ('    ---- Wrap around, end to beginning ----')
  call window
  n1 = 1
2107 n2 = n1 + n33
  go to 2110
2109 if (n13 .eq. 2) go to 2132
  write (unit = munit6, fmt = 2115)
2115 format (' ????  Illegal data (not blank or two dec free-format integers.   Try again.')
  call window
  write (munit6, 2118)
2118 format ("       Is it possible user's data has no such table (other cause of message)?")
  call window
  kill = 1
  go to 9000
2132 call frein2 (bytbuf, n1, n2)
  n33 = n2 - n1
2110 if (n1 .le. 0) n1 = 1
  if (n2 .gt. max) n2 = max
9000 if (iprspy .lt. 1) go to 9006
  write (unit = munit6, fmt = 9002)  max, n1, n2, kill
9002 format (' Return from "intpar".  max, n1, n2, kill =', 4i6)
  call window
9006 return
end subroutine intpar

!
! subroutine sosrng.
!

subroutine sosrng (kill)
  use dekspy
  implicit none
  !     Module of interactive EMTP usage only, which services "emtspy".
  !     This module serves to extract a beginning and ending line number
  !     for sos-like editing operations of  "edit"  command.   These two
  !     integer outputs are  (lidnt1, lidnt2)  of common.   The only
  !     argument is  "kill",  which tells whether the operation was a
  !     success:  "0" means success,  "1" means error.
  !     For non-interactive emtp, this module can be destroyed.
  integer(4), intent(out) :: kill
  character(1) :: char2
  integer(4) :: n1, n12, n13, n24
  !
  if (iprspy .lt. 1) go to 2615
  write (unit = munit6, fmt = 2613) bytbuf
2613 format (' Top "sosrange".  bytbuf(a20) =', a20)
  call window
  write (unit = munit6, fmt = 2614) numsym, lidnt1, linnow, char1
2614 format (' numsym, lidnt1, linnow, char1 =', 3i5, 2x, a1)
  call window
2615 kill = 0
  if (char1 .ne. ' ') go to 2629
  !     blank means we just continue displaying as many lines as last time
  n12 = linspn
  lidnt1 = lidnt2 + 1
  lidnt2 = lidnt1 + n12
  go to 2662
2629 if (bytbuf(1 : 1) .ne. '#') go to 2632
  !     following code processes a command like  "*p22" :
  ibegcl = 2
  call getnum (n1)
  if (n1 .ne. -87654) go to 2630
2621 kill = 1
  go to 9000
2630 lidnt1 = linnow + 1
  lidnt2 = linnow + n1
  go to 2662
2632 if (bytbuf(1 : 1) .ne. ' ') go to 2635
  !     following code process simple "*p" ---- next 16 lines, like sos:
  if (linnow .lt. numcrd) go to 2634
  print *, ' No such lines exist'
  go to 2621
2634 n1 = 16
  go to 2630
2635 ibegcl = 0
  n13 = 0
  lidnt1 = 0
  lidnt2 = 0
2636 ibegcl = ibegcl + 1
  n24 = 0
  if (iprspy .lt. 2) go to 2644
  write (unit = munit6, fmt = 2640)
2640 format (' Begin next lidnt.    ibeg     n13  lidnt1  jpoint      n1     n24')
  call window
  write (unit = munit6, fmt = 2641) ibegcl, n13, lidnt1, linnow, n1, n24
2641 format ('                  ', 6i8)
  call window
2644 if (bytbuf(ibegcl : ibegcl) .ne. '.') go to 2645
  n24 = linnow
  go to 2654
2645 if (bytbuf(ibegcl : ibegcl) .ne. '^') go to 2648
  n24 = 1
  go to 2654
2648 if (bytbuf(ibegcl : ibegcl) .ne. '*') go to 2657
  n24 = numcrd
2654 ibegcl = ibegcl + 1
2657 call getnum (n1)
  if (n1 .eq. -87654) go to 2621
  n13 = n13 + 1
  if (n13 .eq. 1) lidnt1 = n24 + n1
  if (n13 .eq. 2) lidnt2 = n24 + n1
  if (n13 .eq. 2) go to 2662
  ibegcl = iendcl + 1
  if (bytbuf(ibegcl : ibegcl) .eq. ':') go to 2636
  if (bytbuf(ibegcl : ibegcl) .ne. '#') go to 2659
  !     we get here with a request like  "*p200!5",  after "200" is known
  ibegcl = ibegcl + 1
  call getnum (n1)
  if (n1 .eq. -87654) go to 2621
  lidnt2 = lidnt1 + n1 - 1
  go to 2662
  !     we reach 2659 if nothing follows first number (e.g., "*p200"):
2659 lidnt2 = lidnt1
  !     now we display lines  lidnt1, ...., lidnt2,  after limit check:
2662 if (lidnt1 .ge. 1) go to 2663
  print *, ' Illegal syntax of command'
  go to 2621
2663 if (lidnt1 .le. numcrd) go to 2664
  print *, ' Range given does not contain any lines'
  go to 2621
2664 if (lidnt2 .gt. numcrd) lidnt2 = numcrd
9000 if (iprspy .lt. 1) go to 9006
  write (unit = munit6, fmt = 9003) lidnt1, lidnt2,  kill, char2
9003 format (' Exit "sosrng".  lidnt1, lidnt2, kill, char2 =', 3i6, 2x, a1)
  call window
9006 return
end subroutine sosrng

!
! subroutine movesp.
!

subroutine movesp (from, to, n15)
  implicit none
  !     Module of interactive EMTP usage only, which services "emtspy".
  !     For non-interactive EMTP, this module can be destroyed.
  !     This routine transfers single-precision from(1 : n15) to
  !     to(1 : n15).  Except for missing implicit, it equals "mover".
  real(8), intent(in) :: from(*)
  real(8), intent(out) :: to(*)
  integer(4), intent(in) :: n15
  to(1 : n15) = from(1 : n15)
  return
end subroutine movesp

!
! subroutine prompt.
!

subroutine prompt
  use blkcom
  use dekspy
  implicit none
  !     VAX-11  installation-dependent EMTP module used only
  !     for interactive EMTP ("emtspy").  Input is program
  !     prompt in character(80) variable prom80 of deck "dekspy".
  !     The prompt must end with colon (":").  Then line feed
  !     will be suppressed, so subsequent read is to right of ":".
  !     For non-interactive EMTP, this module can be destroyed.
  integer(4) :: j, n2
  !
  n2 = 80
  do j = 1, 80
     if (prom80(n2 : n2) .ne. ' ') go to 1426
     n2 = n2 - 1
  end do
  return
  !     following lunit6 should really go to 2nd screen ("window")
  !1426 write (lunit6, '(a, $)') prom80(1 : n2)                ! ",$" is DEC magic to hold cursor
1426 write (unit = lunit(6), fmt = '(a)', advance = 'no') prom80(1 : n2)
  return
end subroutine prompt

!
! subroutine frefp1.
!

subroutine frefp1 (ansi, d12)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode a
  !     single floating point number.  The input is character(80)
  !     variable  ansi,  and the output is double precision d12.
  !     For non-interactive EMTP, this module can be destroyed.
  character(80), intent(in) :: ansi
  real(8), intent(out) :: d12
  integer(4) :: n8
  n8 = 1
  call frefix (ansi, n8)
  read (unit = ansi, fmt = '(e20.0)') d12
  return
end subroutine frefp1

!
! subroutine fresp1.
!

subroutine fresp1 (ansi, d12)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode one
  !     floating point numbers d12 from character(80) ansi.   This
  !     is identical to "frefp1", except for single precision.
  character(80), intent(in) :: ansi
  real(8), intent(out) :: d12
  integer(4) :: n8
  n8 = 1
  call frefix (ansi, n8)
  read (unit = ansi, fmt = '(e20.0)') d12
  return
end subroutine fresp1

!
! subroutine frefp2.
!

subroutine frefp2 (ansi, d12, d13)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode two
  !     floating point numbers d12 and d13 from character(80) ansi.
  !     For non-interactive emtp, this module can be destroyed.
  character(80), intent(in) :: ansi
  real(8), intent(out) :: d12, d13
  integer(4) :: n8
  n8 = 2
  call frefix (ansi, n8)
  read (unit = ansi, fmt = '(2e20.0)') d12, d13
  return
end subroutine frefp2

!
! subroutine fresp2.
!

subroutine fresp2 (ansi, d12, d13)
  implicit none
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode two
  !     floating point numbers d12 and d13 from character(80) ansi.
  !     This is identical to "frefp2", except for single precision.
  character(80), intent(in) :: ansi
  real(8), intent(out) :: d12, d13
  integer(4) :: n8
  n8 = 2
  call frefix (ansi, n8)
  read (unit = ansi, fmt = '(2e20.0)') d12, d13
  return
end subroutine fresp2

!
! subroutine frefp3.
!

subroutine frefp3 (ansi, d12, d13, d14)
  implicit none
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode three
  !     floating point numbers d12, d13, d14 from character(80) ansi.
  !     For non-interactive EMTP, this module can be destroyed.
  character(80), intent(in) :: ansi
  real(8), intent(out) :: d12, d13, d14
  integer(4) :: n8
  n8 = 3
  call frefix (ansi, n8)
  read (unit = ansi, fmt = '(3e20.0)') d12, d13, d14
  return
end subroutine frefp3

!
! subroutine fresp3.
!

subroutine fresp3 (ansi, d12, d13, d14)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode three
  !     floating point numbers d12, d13, d13 from character(8)0 ansi.
  !     This is identical to "frefp3", except for single precision.
  character(80), intent(in) :: ansi
  real(8), intent(out) :: d12, d13, d14
  integer(4) :: n8
  n8 = 3
  call frefix (ansi, n8)
  read (unit = ansi, fmt = '(3e20.0)') d12, d13, d14
  return
end subroutine fresp3

!
! subroutine frein1.
!

subroutine frein1 (ansi, n12)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode a
  !     single integer number n12 from character(80) input ansi.
  !     For non-interactive EMTP, this module can be destroyed.
  character(80), intent(in) :: ansi
  integer(4), intent(out) :: n12
  integer(4) :: d12, n8
  n8 = 1
  call frefix (ansi, n8)
  read (unit = ansi, fmt = '(3e20.0)') d12
  n12 = d12
  return
end subroutine frein1

!
! subroutine frein2.
!

subroutine frein2 (ansi, n12, n13)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     Universal module (works for any computer) used only for the
  !     interactive EMTP ("emtspy").  It is called to decode two
  !     integer numbers n12 and n13 from character(80) input ansi.
  !     For non-interactive emtp, this module can be destroyed.
  character(80), intent(in) :: ansi
  integer(4), intent(out) :: n12, n13
  integer(4) :: d12, d13, n8
  n8 = 2
  call frefix (ansi, n8)
  read (unit = ansi, fmt = '(3e20.0)') d12, d13
  n12 = d12
  n13 = d13
  return
end subroutine frein2

!
! subroutine frefix.
!

subroutine frefix (ansi, n8)
  use dekspy
  implicit none
  !     Universal module used only by interactive EMTP ("emtspy").
  !     For non-interactive use only, it can be destroyed.
  !     This module is called by "frefp1", "frein1", etc. --- all
  !     of the free-field input modules of spy usage.  It checks
  !     for legal numerical data, and will re-prompt for more if
  !     the user has made a mistake, thereby avoiding possible
  !     death of interactive EMTP execution due to typing error.
  !     Ansi is the input being processed, for n8 numbers.  Upon
  !     exit, ansi will be converted to 4e20.0 fixed-format data.
  character(80) :: ansi, hold
  integer(4), intent(in) :: n8
  integer(4) :: i, j, k, kk, n1, n2, n3, n5, n13, n14, n16
  !
  if (iprspy .lt. 3) go to 3582
  write (munit6, 3579) n8, ansi(1 : 40)
3579 format (' Top of "frefix".  n8 =', i5, '   ansi(1:40) = ', a40)
  call window
3582 hold(1 : 80) = ' '
  kk = 0
  i = 0
  go to 3648
3612 if (ansi(i : i) .eq. ' ' .or. ansi(i : i) .eq. ',' .or. i .ge. 80) go to 3615
  i = i + 1
  go to 3612
3615 n5 = i - 1
  kk = kk + 1
  n13 = 0
  n14 = 0
  do j = n3, n5
     if (ansi(j : j) .ne. '+' .and. ansi(j : j) .ne. '-' ) go to 3617
     if (j .eq. n3) go to 3642
     n16 = j - 1
     if (ansi(n16 : n16) .eq. 'e' .or. ansi(n16 : n16) .eq. 'd') go to 3642
     go to 3758
3617 if (ansi(j : j) .ne. '.') go to 3626
     n14 = n14 + 1
     if (n14 .le. 1) go to 3642
     go to 3758
3626 if (ansi(j : j) .ne. 'd' .and. ansi(j : j) .ne. 'e') go to 3634
     n13 = n13 + 1
     if (n13 .le. 1) go to 3642
     go to 3758
3634 do k = 1, 10
        if (ansi(j : j) .eq. digit(k)) go to 3642
     end do
     go to 3758
3642 continue
  end do
  n2 = 20 * kk
  n1 = n2 - (n5 - n3)
  hold(n1 : n2) = ansi(n3 : n5)
  if (kk .ge. n8) go to 4100
3648 i = i + 1
  if (i .gt. 80) go to 3758
  if (ansi(i : i) .eq. ' ') go to 3648
  n3 = i
  go to 3612
3758 write (unit = prom80, fmt = 3761) j, ansi(j - 2 : j + 2)
3761 format ('   @@@  Illegal data in column', i3, ' [', a5, '].  Resend line or end :')
  call prompt
  write (unit = *, fmt = *) ' Ok, "frefix" call to "flager" begins.'
  lockbr = 1
  call flager
  write (unit = *, fmt = *) ' Ok, back in "frefix" with buff77 again.'
  ansi = buff77
  if (ansi(1 : 4) .ne. 'end ') go to 3582
  ansi(1 : 80) = ' '
  go to 4103
4100 ansi(1 : n2) = hold(1 : n2)
4103 if (iprspy .lt. 2) go to 4109
  write (unit = munit6, fmt = 4108) ansi(1 : n2)
4108 format (' Exit "frefix".  ansi(1 : n2) =', a80)
  call window
4109 return
end subroutine frefix

!
! subroutine locating.
!

subroutine locating
  use blkcom
  use labcom
  use smach
  use tacsar
  use umcom
  use dekspy
  use fixcom
  use indcom
  use bcddat
  use bcdtim
  implicit none
  integer(4) :: iac, ibr1, icsup, iconfg, idctcs, idelta, idist, iend, imdual
  integer(4) :: ineof, infexp, iold, ipl, ipoint, ipos, ipr, iprint, iptacw, isat, ised
  integer(4) :: iseq, isloc, isw, itest, iupper, ixr
  integer(4) :: jasmit, jexcit, jpl, jpr, jseedr, jsmtor
  integer(4) :: kalkdu, kalksx, kalkzx, kalplt, kbase, kbtcs, kbwkcs, kdev1, kdev2
  integer(4) :: kdumj, kdumk, kexc, kfnsup, kgndev, kgnz, khsci, khscr, kidum
  integer(4) :: kifls, kiflz, kilim1, kilim2, kilms1, kilms2, kilmz1, kilmz2
  integer(4) :: kint, kjsup, kkdj, kkdk, kkdus, kkfst, kkhst, kkni, kkout
  integer(4) :: kksj, kksk, kksup, kkxic, kkzj, kkzk, kldev1, kldev2, klmxic
  integer(4) :: klnout, kloaep, kmac, knt, kopsup, kout, kpac1i, kpac1r, kpac2i, kpac2r
  integer(4) :: kpd, kph, kpl, kpn, kpr, kreqab, krgsup, krhsde, krowcs, ksat
  integer(4) :: kslim1, kslim2, kslim3, kstart, ktbdev, ktxmn, ktxmx, ktypdv
  integer(4) :: ktysup, kud2, kud3, kud4, kud5, kvin, kvlim1, kvlim2, kvou
  integer(4) :: kvxx, kwrite, kxai, kxhst, kxmncs, kxmxcs, kzlim1, kzlim2
  integer(4) :: laux, lcom10, limtxf, locbr1, locz11, lpast2, lpeak, lsmat
  integer(4) :: lspov4, ltacst, ltdelt, ltrnst
  integer(4) :: mdebug, mtape, mxtacw
  integer(4) :: ndelta, nds, nhst, niamax, niomax, nkn
  integer(4) :: nmax, nodma, nodmb, nodmc, nout, noutsm, ntcsex, numas, nxic
  real(8) :: power
  real(8) :: qsat
  real(8) :: r0, ra, rat1, rf, rg, rkd, rkq
  real(8) :: seedr, smoutp, smoutq, swname
  real(8) :: teg, temp, texc, tork
  real(8) :: unused
  real(8) :: xay
  real(8) :: y, yfor
  real(8) :: znvref, zsk
  real(8) :: a12, a21, a22, ac, acr, agline, ah, ai, at
  real(8) :: brname, brnonl
  real(8) :: ce, chard4, cnp, col, cv
  real(8) :: dsat, dsd, dsm, dsr, dtnext
  real(8) :: el0, elaf, elag, elakd, elakq, eld, elf, elfkd, elg, elgkq, elkd
  real(8) :: elkq, elq, extrs
  real(8) :: hertz, hico, histr, hsp
  !
  locate(   1)  =  location ( bus1   )
  locate(   2)  =  location ( bus2   )
  locate(   3)  =  location ( bus3   )
  locate(   4)  =  location ( bus4   )
  locate(   5)  =  location ( bus5   )
  locate(   6)  =  location ( bus6   )
  locate(   7)  =  location ( trash  )
  locate(   8)  =  location ( blank  )
  locate(   9)  =  location ( terra  )
  locate(  10)  =  location ( userid )
  locate(  11)  =  location ( branch )
  locate(  12)  =  location ( chcopy   )
  locate(  13)  =  location ( csepar )
  locate(  14)  =  location ( chcont )
  locate(  15)  =  location ( texcol(1) )
  locate(  16)  =  location ( texta6(1) )
  locate(  17)  =  location ( date1(1)  )
  locate(  18)  =  location ( tclock(1) )
  locate(  19)  =  location ( vstacs(1) )
  locate(  20)  =  location ( abuff  )
  locate(  21)  =  location ( ci1    )
  locate(  22)  =  location ( ck1    )
  locate(  23)  =  location ( deltat )
  locate(  24)  =  location ( delta2 )
  locate(  25)  =  location ( freqcs )
  locate(  26)  =  location ( epsiln )
  locate(  27)  =  location ( xunits )
  locate(  28)  =  location ( aincr  )
  locate(  29)  =  location ( xmaxmx )
  locate(  30)  =  location ( znvref )
  locate(  31)  =  location ( epszno )
  locate(  32)  =  location ( epwarn )
  locate(  33)  =  location ( epstop )
  locate(  34)  =  location ( t      )
  locate(  35)  =  location ( hertz  )
  locate(  36)  =  location ( tolmat )
  locate(  37)  =  location ( twopi  )
  locate(  38)  =  location ( tmax   )
  locate(  39)  =  location ( omega  )
  locate(  40)  =  location ( copt   )
  locate(  41)  =  location ( xopt   )
  locate(  42)  =  location ( szplt  )
  locate(  43)  =  location ( szbed  )
  locate(  44)  =  location ( sglfir )
  locate(  45)  =  location ( sigmax )
  locate(  46)  =  location ( epsuba )
  locate(  47)  =  location ( epdgel )
  locate(  48)  =  location ( epomeg )
  locate(  49)  =  location ( fminfs )
  locate(  50)  =  location ( delffs )
  locate(  51)  =  location ( fmaxfs )
  locate(  52)  =  location ( tenerg )
  locate(  53)  =  location ( begmax )
  locate(  54)  =  location ( tenm3  )
  locate(  55)  =  location ( tenm6  )
  locate(  56)  =  location ( unity  )
  locate(  57)  =  location ( onehaf )
  locate(  58)  =  location ( peaknd(1) )
  locate(  59)  =  location ( fltinf )
  locate(  60)  =  location ( flzero )
  locate(  61)  =  location ( degmin )
  locate(  62)  =  location ( degmax )
  locate(  63)  =  location ( statfr )
  locate(  64)  =  location ( voltbc )
  locate(  65)  =  location ( flstat )
  locate(  66)  =  location ( dtnext )
  locate(  67)  =  location ( angle  )
  locate(  68)  =  location ( pu     )
  locate(  69)  =  location ( seedr  )
  locate(  70)  =  location ( speedl )
  locate(  71)  =  location ( kstart )
  locate(  72)  =  location ( knt    )
  locate(  73)  =  location ( kbase  )
  locate(  74)  =  location ( ltdelt )
  locate(  75)  =  location ( unused )
  locate(  76)  =  location ( mtape  )
  locate(  77)  =  location ( lunit(1) )
  locate(  78)  =  location ( lunit(2) )
  locate(  79)  =  location ( lunit(3) )
  locate(  80)  =  location ( lunit(4) )
  locate(  81)  =  location ( lunit(5) )
  locate(  82)  =  location ( lunit(6) )
  locate(  83)  =  location ( lunit(7) )
  locate(  84)  =  location ( lunit(8) )
  locate(  85)  =  location ( lunit(9) )
  locate(  86)  =  location ( lunit(10) )
  locate(  87)  =  location ( lunit(11) )
  locate(  88)  =  location ( lunit(12) )
  locate(  89)  =  location ( lunit(13) )
  locate(  90)  =  location ( lunit(14) )
  locate(  91)  =  location ( lunit(15) )
  locate(  92)  =  location ( nexout )
  locate(  93)  =  location ( nright )
  locate(  94)  =  location ( nfrfld )
  locate(  95)  =  location ( kolbeg )
  locate(  96)  =  location ( kprchg )
  locate(  97)  =  location ( multpr )
  locate(  98)  =  location ( ipntv  )
  locate(  99)  =  location ( indtv  )
  locate( 100)  =  location ( lstat  )
  locate( 101)  =  location ( nbyte  )
  locate( 102)  =  location ( lunsav )
  locate( 103)  =  location ( iprsov )
  locate( 104)  =  location ( icheck )
  locate( 105)  =  location ( unused )
  locate( 106)  =  location ( iend   )
  locate( 107)  =  location ( iline  )
  locate( 108)  =  location ( inonl  )
  locate( 109)  =  location ( iold   )
  locate( 110)  =  location ( iout   )
  locate( 111)  =  location ( iprint )
  locate( 112)  =  location ( ipunch )
  locate( 113)  =  location ( iread  )
  locate( 114)  =  location ( kol132 )
  locate( 115)  =  location ( istep  )
  locate( 116)  =  location ( unused )
  locate( 117)  =  location ( itype  )
  locate( 118)  =  location ( it1    )
  locate( 119)  =  location ( it2    )
  locate( 120)  =  location ( iupper )
  locate( 121)  =  location ( izero  )
  locate( 122)  =  location ( kcount )
  locate( 123)  =  location ( istead )
  locate( 124)  =  location ( unused )
  locate( 125)  =  location ( ldata  )
  locate( 126)  =  location ( lbrnch )
  locate( 127)  =  location ( limtxf )
  locate( 128)  =  location ( mdebug )
  locate( 129)  =  location ( lexct  )
  locate( 130)  =  location ( lbus   )
  locate( 131)  =  location ( lymat  )
  locate( 132)  =  location ( lswtch )
  locate( 133)  =  location ( lnonl  )
  locate( 134)  =  location ( lchar  )
  locate( 135)  =  location ( m4plot )
  locate( 136)  =  location ( lpast  )
  locate( 137)  =  location ( lsmat  )
  locate( 138)  =  location ( iplot  )
  locate( 139)  =  location ( ncomp  )
  locate( 140)  =  location ( nv     )
  locate( 141)  =  location ( lcomp  )
  locate( 142)  =  location ( numsm  )
  locate( 143)  =  location ( ifdep  )
  locate( 144)  =  location ( ltails )
  locate( 145)  =  location ( lfdep  )
  locate( 146)  =  location ( lwt    )
  locate( 147)  =  location ( last   )
  locate( 148)  =  location ( npower )
  locate( 149)  =  location ( maxpe  )
  locate( 150)  =  location ( lpeak  )
  locate( 151)  =  location ( nout   )
  locate( 152)  =  location ( iv     )
  locate( 153)  =  location ( ineof  )
  locate( 154)  =  location ( ktrlsw )
  locate( 155)  =  location ( num99  )
  locate( 156)  =  location ( kpartb )
  locate( 157)  =  location ( llbuff )
  locate( 158)  =  location ( kanal  )
  locate( 159)  =  location ( nsmth  )
  locate( 160)  =  location ( ntcsex )
  locate( 161)  =  location ( nstacs )
  locate( 162)  =  location ( kloaep )
  locate( 163)  =  location ( lastov )
  locate( 164)  =  location ( ltacst )
  locate( 165)  =  location ( lhist  )
  locate( 166)  =  location ( ifx    )
  locate( 167)  =  location ( ndelta )
  locate( 168)  =  location ( idelta )
  locate( 169)  =  location ( inecho )
  locate( 170)  =  location ( noutpr )
  locate( 171)  =  location ( ktab   )
  locate( 172)  =  location ( jflsos )
  locate( 173)  =  location ( numdcd )
  locate( 174)  =  location ( numum  )
  locate( 175)  =  location ( lspcum )
  locate( 176)  =  location ( nphcas )
  locate( 177)  =  location ( locz11 )
  locate( 178)  =  location ( locbr1 )
  locate( 179)  =  location ( ialter )
  locate( 180)  =  location ( i_char )
  locate( 181)  =  location ( ktref  )
  locate( 182)  =  location ( kph    )
  locate( 183)  =  location ( kreqab )
  locate( 184)  =  location ( ksat   )
  locate( 185)  =  location ( memsav )
  locate( 186)  =  location ( lisoff )
  locate( 187)  =  location ( lspov4 )
  locate( 188)  =  location ( kburro )
  locate( 189)  =  location ( iaverg )
  locate( 190)  =  location ( lsiz23 )
  locate( 191)  =  location ( lsiz26 )
  locate( 192)  =  location ( numout )
  locate( 193)  =  location ( moldat )
  locate( 194)  =  location ( lsiz27 )
  locate( 195)  =  location ( ltlabl )
  locate( 196)  =  location ( iwt    )
  locate( 197)  =  location ( ifdep2 )
  locate( 198)  =  location ( idoubl )
  locate( 199)  =  location ( ioutin )
  locate( 200)  =  location ( ipun   )
  locate( 201)  =  location ( jst    )
  locate( 202)  =  location ( jst1   )
  locate( 203)  =  location ( unused )
  locate( 204)  =  location ( numsub )
  locate( 205)  =  location ( maxzno )
  locate( 206)  =  location ( kalplt )
  locate( 207)  =  location ( niomax )
  locate( 208)  =  location ( niamax )
  locate( 209)  =  location ( ibr1   )
  locate( 210)  =  location ( ifsem  )
  locate( 211)  =  location ( lfsem  )
  locate( 212)  =  location ( iadd   )
  locate( 213)  =  location ( lfd    )
  locate( 214)  =  location ( laux   )
  locate( 215)  =  location ( iofgnd )
  locate( 216)  =  location ( iofbnd )
  locate( 217)  =  location ( unused )
  locate( 218)  =  location ( jseedr )
  locate( 219)  =  location ( modout )
  locate( 220)  =  location ( iftail )
  locate( 221)  =  location ( ipoint )
  locate( 222)  =  location ( lpast2 )
  locate( 223)  =  location ( ncurr  )
  locate( 224)  =  location ( ioffd  )
  locate( 225)  =  location ( isplot )
  locate( 226)  =  location ( isprin )
  locate( 227)  =  location ( maxout )
  locate( 228)  =  location ( ipos   )
  locate( 229)  =  location ( unused )
  locate( 230)  =  location ( unused )
  locate( 231)  =  location ( kill   )
  locate( 232)  =  location ( ivolt  )
  locate( 233)  =  location ( nchain )
  locate( 234)  =  location ( iprsup )
  locate( 235)  =  location ( unused )
  locate( 236)  =  location ( intinf )
  locate( 237)  =  location ( kconst )
  locate( 238)  =  location ( kswtch )
  locate( 239)  =  location ( it     )
  locate( 240)  =  location ( ntot   )
  locate( 241)  =  location ( ibr    )
  locate( 242)  =  location ( lcom10 )
  locate( 243)  =  location ( ltrnst )
  locate( 244)  =  location ( lsyn   )
  locate( 245)  =  location ( kssout )
  locate( 246)  =  location ( loopss )
  locate( 247)  =  location ( infexp )
  locate( 248)  =  location ( numref )
  locate( 249)  =  location ( nword1 )
  locate( 250)  =  location ( nword2 )
  locate( 251)  =  location ( iloaep )
  locate( 252)  =  location ( lnpin  )
  locate( 253)  =  location ( ntot1  )
  locate( 254)  =  location ( limstp )
  locate( 255)  =  location ( indstp )
  locate( 256)  =  location ( nc     )
  locate( 257)  =  location ( unused )
  locate( 258)  =  location ( unused )
  locate( 259)  =  location ( icat   )
  locate( 260)  =  location ( numnvo )
  locate( 261)  =  location ( unused )
  locate( 262)  =  location ( nenerg )
  locate( 263)  =  location ( isw    )
  locate( 264)  =  location ( itest  )
  locate( 265)  =  location ( idist  )
  locate( 266)  =  location ( x      )
  locate( 267)  =  location ( ykm    )
  locate( 268)  =  location ( km     )
  locate( 269)  =  location ( xk     )
  locate( 270)  =  location ( xm     )
  locate( 271)  =  location ( weight )
  locate( 272)  =  location ( iwtent )
  locate( 273)  =  location ( con1   )
  locate( 274)  =  location ( iskip  )
  locate( 275)  =  location ( zinf   )
  locate( 276)  =  location ( eta    )
  locate( 277)  =  location ( nhist  )
  locate( 278)  =  location ( stailm )
  locate( 279)  =  location ( stailk )
  locate( 280)  =  location ( xmax   )
  locate( 281)  =  location ( koutvp )
  locate( 282)  =  location ( bnrg   )
  locate( 283)  =  location ( sconst )
  locate( 284)  =  location ( cnvhst )
  locate( 285)  =  location ( sfd    )
  locate( 286)  =  location ( qfd    )
  locate( 287)  =  location ( semaux )
  locate( 288)  =  location ( ibsout )
  locate( 289)  =  location ( bvalue )
  locate( 290)  =  location ( sptacs )
  locate( 291)  =  location ( kswtyp )
  locate( 292)  =  location ( modswt )
  locate( 293)  =  location ( kbegsw )
  locate( 294)  =  location ( lastsw )
  locate( 295)  =  location ( kentnb )
  locate( 296)  =  location ( nbhdsw )
  locate( 297)  =  location ( topen  )
  locate( 298)  =  location ( crit   )
  locate( 299)  =  location ( kdepsw )
  locate( 300)  =  location ( tdns   )
  locate( 301)  =  location ( isourc )
  locate( 302)  =  location ( energy )
  locate( 303)  =  location ( iardub )
  locate( 304)  =  location ( ardube )
  locate( 305)  =  location ( nonlad )
  locate( 306)  =  location ( nonle  )
  locate( 307)  =  location ( vnonl  )
  locate( 308)  =  location ( curr   )
  locate( 309)  =  location ( anonl  )
  locate( 310)  =  location ( vecnl1 )
  locate( 311)  =  location ( vecnl2 )
  locate( 312)  =  location ( brnonl )
  locate( 313)  =  location ( vzero  )
  locate( 314)  =  location ( ilast  )
  locate( 315)  =  location ( nltype )
  locate( 316)  =  location ( kupl   )
  locate( 317)  =  location ( nlsub  )
  locate( 318)  =  location ( cursub )
  locate( 319)  =  location ( cchar  )
  locate( 320)  =  location ( vchar  )
  locate( 321)  =  location ( gslope )
  locate( 322)  =  location ( kk     )
  locate( 323)  =  location ( c      )
  locate( 324)  =  location ( tr     )
  locate( 325)  =  location ( tx     )
  locate( 326)  =  location ( r      )
  locate( 327)  =  location ( nr     )
  locate( 328)  =  location ( length )
  locate( 329)  =  location ( cik    )
  locate( 330)  =  location ( ci     )
  locate( 331)  =  location ( ck     )
  locate( 332)  =  location ( swname )
  locate( 333)  =  location ( ibrnch )
  locate( 334)  =  location ( jbrnch )
  locate( 335)  =  location ( tstop  )
  locate( 336)  =  location ( nonlk  )
  locate( 337)  =  location ( nonlm  )
  locate( 338)  =  location ( spum   )
  locate( 339)  =  location ( kks    )
  locate( 340)  =  location ( kknonl )
  locate( 341)  =  location ( znonl  )
  locate( 342)  =  location ( znonlb )
  locate( 343)  =  location ( znonlc )
  locate( 344)  =  location ( finit  )
  locate( 345)  =  location ( ksub   )
  locate( 346)  =  location ( msub   )
  locate( 347)  =  location ( isubeg )
  locate( 348)  =  location ( litype )
  locate( 349)  =  location ( imodel )
  locate( 350)  =  location ( kbus   )
  locate( 351)  =  location ( mbus   )
  locate( 352)  =  location ( kodebr )
  locate( 353)  =  location ( cki    )
  locate( 354)  =  location ( ckkjm  )
  locate( 355)  =  location ( indhst )
  locate( 356)  =  location ( kodsem )
  locate( 357)  =  location ( brname )
  locate( 358)  =  location ( iform  )
  locate( 359)  =  location ( node   )
  locate( 360)  =  location ( crest  )
  locate( 361)  =  location ( time1  )
  locate( 362)  =  location ( time2  )
  locate( 363)  =  location ( tstart )
  locate( 364)  =  location ( sfreq  )
  locate( 365)  =  location ( kmswit )
  locate( 366)  =  location ( nextsw )
  locate( 367)  =  location ( rmfd   )
  locate( 368)  =  location ( cikfd  )
  locate( 369)  =  location ( imfd   )
  locate( 370)  =  location ( tclose )
  locate( 371)  =  location ( adelay )
  locate( 372)  =  location ( kpos   )
  locate( 373)  =  location ( e      )
  locate( 374)  =  location ( f      )
  locate( 375)  =  location ( kssfrq )
  locate( 376)  =  location ( kode   )
  locate( 377)  =  location ( kpsour )
  locate( 378)  =  location ( volti  )
  locate( 379)  =  location ( voltk  )
  locate( 380)  =  location ( volt   )
  locate( 381)  =  location ( bus(1)    )
  locate( 382)  =  location ( eld    )
  locate( 383)  =  location ( elaf   )
  locate( 384)  =  location ( elf    )
  locate( 385)  =  location ( elakd  )
  locate( 386)  =  location ( elfkd  )
  locate( 387)  =  location ( elkd   )
  locate( 388)  =  location ( elq    )
  locate( 389)  =  location ( elag   )
  locate( 390)  =  location ( elg    )
  locate( 391)  =  location ( elakq  )
  locate( 392)  =  location ( elgkq  )
  locate( 393)  =  location ( elkq   )
  locate( 394)  =  location ( el0    )
  locate( 395)  =  location ( ra     )
  locate( 396)  =  location ( rf     )
  locate( 397)  =  location ( rkd    )
  locate( 398)  =  location ( rg     )
  locate( 399)  =  location ( rkq    )
  locate( 400)  =  location ( r0     )
  locate( 401)  =  location ( agline )
  locate( 402)  =  location ( rat1   )
  locate( 403)  =  location ( smoutp )
  locate( 404)  =  location ( smoutq )
  locate( 405)  =  location ( teg    )
  locate( 406)  =  location ( texc   )
  locate( 407)  =  location ( cnp    )
  locate( 408)  =  location ( a22    )
  locate( 409)  =  location ( a12    )
  locate( 410)  =  location ( a21    )
  locate( 411)  =  location ( ac     )
  locate( 412)  =  location ( ai     )
  locate( 413)  =  location ( at     )
  locate( 414)  =  location ( ah     )
  locate( 415)  =  location ( xay    )
  locate( 416)  =  location ( cu     )
  locate( 417)  =  location ( cv     )
  locate( 418)  =  location ( dsat   )
  locate( 419)  =  location ( qsat   )
  locate( 420)  =  location ( acr    )
  locate( 421)  =  location ( ce     )
  locate( 422)  =  location ( dsr    )
  locate( 423)  =  location ( dsd    )
  locate( 424)  =  location ( hico   )
  locate( 425)  =  location ( dsm    )
  locate( 426)  =  location ( hsp    )
  locate( 427)  =  location ( power  )
  locate( 428)  =  location ( extrs  )
  locate( 429)  =  location ( histq  )
  locate( 430)  =  location ( histr  )
  locate( 431)  =  location ( yfor   )
  locate( 432)  =  location ( zsk    )
  locate( 433)  =  location ( y      )
  locate( 434)  =  location ( tork   )
  locate( 435)  =  location ( temp   )
  locate( 436)  =  location ( z      )
  locate( 437)  =  location ( x1     )
  locate( 438)  =  location ( sqrt3  )
  locate( 439)  =  location ( asqrt3 )
  locate( 440)  =  location ( sqrt32 )
  locate( 441)  =  location ( thtw   )
  locate( 442)  =  location ( athtw  )
  locate( 443)  =  location ( radeg  )
  locate( 444)  =  location ( omdt   )
  locate( 445)  =  location ( factom )
  locate( 446)  =  location ( damrat )
  locate( 447)  =  location ( isat   )
  locate( 448)  =  location ( ised   )
  locate( 449)  =  location ( iseq   )
  locate( 450)  =  location ( imdual )
  locate( 451)  =  location ( iconfg )
  locate( 452)  =  location ( kmac   )
  locate( 453)  =  location ( kexc   )
  locate( 454)  =  location ( numas  )
  locate( 455)  =  location ( nodma  )
  locate( 456)  =  location ( nodmb  )
  locate( 457)  =  location ( nodmc  )
  locate( 458)  =  location ( jasmit )
  locate( 459)  =  location ( jsmtor )
  locate( 460)  =  location ( jexcit )
  locate( 461)  =  location ( isloc  )
  locate( 462)  =  location ( noutsm )
  locate( 463)  =  location ( ismout )
  locate( 464)  =  location ( mfirst )
  locate( 465)  =  location ( limass )
  locate( 466)  =  location ( nst    )
  locate( 467)  =  location ( itold  )
  locate( 468)  =  location ( ibrold )
  locate( 469)  =  location ( busum(1)  )
  locate( 470)  =  location ( ptheta(1, 1) )
  locate( 471)  =  location ( zthevr(1, 1) )
  locate( 472)  =  location ( vinp   )
  locate( 473)  =  location ( zthevs )
  locate( 474)  =  location ( umcur  )
  locate( 475)  =  location ( con    )
  locate( 476)  =  location ( dumvec )
  locate( 477)  =  location ( dummat(1, 1) )
  locate( 478)  =  location ( date(1)   )
  locate( 479)  =  location ( clock  )
  locate( 480)  =  location ( pi     )
  locate( 481)  =  location ( sroot2 )
  locate( 482)  =  location ( sroot3 )
  locate( 483)  =  location ( omegrf )
  locate( 484)  =  location ( inpu   )
  locate( 485)  =  location ( numbus )
  locate( 486)  =  location ( ncltot )
  locate( 487)  =  location ( ndum   )
  locate( 488)  =  location ( initum )
  locate( 489)  =  location ( iureac )
  locate( 490)  =  location ( iugpar )
  locate( 491)  =  location ( iufpar )
  locate( 492)  =  location ( iuhist )
  locate( 493)  =  location ( iuumrp )
  locate( 494)  =  location ( iunod1 )
  locate( 495)  =  location ( iunod2 )
  locate( 496)  =  location ( iujclt )
  locate( 497)  =  location ( iujclo )
  locate( 498)  =  location ( iujtyp )
  locate( 499)  =  location ( iunodo )
  locate( 500)  =  location ( iujtmt )
  locate( 501)  =  location ( iuhism )
  locate( 502)  =  location ( iuomgm )
  locate( 503)  =  location ( iuomld )
  locate( 504)  =  location ( iutham )
  locate( 505)  =  location ( iuredu )
  locate( 506)  =  location ( iureds )
  locate( 507)  =  location ( iuflds )
  locate( 508)  =  location ( iufldr )
  locate( 509)  =  location ( iurequ )
  locate( 510)  =  location ( iuflqs )
  locate( 511)  =  location ( iuflqr )
  locate( 512)  =  location ( iujcds )
  locate( 513)  =  location ( iujcqs )
  locate( 514)  =  location ( iuflxd )
  locate( 515)  =  location ( iuflxq )
  locate( 516)  =  location ( iunppa )
  locate( 517)  =  location ( iurotm )
  locate( 518)  =  location ( iuncld )
  locate( 519)  =  location ( iunclq )
  locate( 520)  =  location ( iujtqo )
  locate( 521)  =  location ( iujomo )
  locate( 522)  =  location ( iujtho )
  locate( 523)  =  location ( iureqs )
  locate( 524)  =  location ( iuepso )
  locate( 525)  =  location ( iudcoe )
  locate( 526)  =  location ( iukcoi )
  locate( 527)  =  location ( iuvolt )
  locate( 528)  =  location ( iuangl )
  locate( 529)  =  location ( iunodf )
  locate( 530)  =  location ( iunodm )
  locate( 531)  =  location ( iukumo )
  locate( 532)  =  location ( iujumo )
  locate( 533)  =  location ( iuumou )
  locate( 534)  =  location ( nclfix )
  locate( 535)  =  location ( numfix )
  locate( 536)  =  location ( iotfix )
  locate( 537)  =  location ( ibsfix )
  locate( 538)  =  location ( ksubum )
  locate( 539)  =  location ( nsmach )
  locate( 540)  =  location ( istart )
  locate( 541)  =  location ( karray(1) )
  locate( 542)  =  location ( rampcn )
  locate( 543)  =  location ( rampsl )
  locate( 544)  =  location ( kyramp )
  locate( 545)  =  location ( texpar(1) )
  locate( 546)  =  location ( fendrp )
  locate( 547)  =  location ( tminrp )
  locate( 548)  =  location ( tmaxrp )
  locate( 549)  =  location ( tbegrp )
  locate( 550)  =  location ( tendrp )
  locate( 551)  =  location ( fbegrp )
  locate( 552)  =  location ( tbreak )
  locate( 553)  =  location ( indxrp )
  locate( 554)  =  location ( ivec   )
  locate( 555)  =  location ( iascii )
  locate( 556)  =  location ( numsym )
  locate( 557)  =  location ( jjroll )
  locate( 558)  =  location ( itexp  )
  locate( 559)  =  location ( labels )
  locate( 560)  =  location ( maxarg )
  locate( 561)  =  location ( kilper )
  locate( 562)  =  location ( kfile5 )
  locate( 563)  =  location ( kverfy )
  locate( 564)  =  location ( ibegcl )
  locate( 565)  =  location ( iendcl )
  locate( 566)  =  location ( lidnt1 )
  locate( 567)  =  location ( lidnt2 )
  locate( 568)  =  location ( linnow )
  locate( 569)  =  location ( linspn )
  locate( 570)  =  location ( numcrd )
  locate( 571)  =  location ( munit5 )
  locate( 572)  =  location ( indbuf )
  locate( 573)  =  location ( indbeg )
  locate( 574)  =  location ( mflush )
  locate( 575)  =  location ( newvec )
  locate( 576)  =  location ( munit6 )
  locate( 577)  =  location ( lserlc )
  locate( 578)  =  location ( kserlc )
  locate( 579)  =  location ( kbrser )
  locate( 580)  =  location ( lockbr )
  locate( 581)  =  location ( iprspy )
  locate( 582)  =  location ( monitr )
  locate( 583)  =  location ( monits )
  locate( 584)  =  location ( locate )
  locate( 585)  =  location ( nline  )
  locate( 586)  =  location ( kwtspy )
  locate( 587)  =  location ( kbreak )
  locate( 588)  =  location ( limbuf )
  locate( 589)  =  location ( inchlp )
  locate( 590)  =  location ( ksymbl )
  locate( 591)  =  location ( kopyit )
  locate( 592)  =  location ( kslowr )
  locate( 593)  =  location ( limcrd )
  locate( 594)  =  location ( looprp )
  locate( 595)  =  location ( n10rmp )
  locate( 596)  =  location ( memrmp )
  locate( 597)  =  location ( kar1   )
  locate( 598)  =  location ( kar2   )
  locate( 599)  =  location ( numrmp )
  locate( 600)  =  location ( luntsp )
  locate( 601)  =  location ( logvar )
  locate( 602)  =  location ( filext(1) )
  locate( 603)  =  location ( symb(1)   )
  locate( 604)  =  location ( col    )
  locate( 605)  =  location ( bytfnd )
  locate( 606)  =  location ( char1  )
  locate( 607)  =  location ( symbrp(1) )
  locate( 608)  =  location ( chard4 )
  locate( 609)  =  location ( bytbuf )
  locate( 610)  =  location ( buff77 )
  locate( 611)  =  location ( file6b(1) )
  locate( 612)  =  location ( file6(1)  )
  locate( 613)  =  location ( blan80 )
  locate( 614)  =  location ( prom80 )
  locate( 615)  =  location ( digit(1)  )
  locate( 616)  =  location ( iac    )
  locate( 617)  =  location ( idctcs )
  locate( 618)  =  location ( ipl    )
  locate( 619)  =  location ( ipr    )
  locate( 620)  =  location ( ixr    )
  locate( 621)  =  location ( jpl    )
  locate( 622)  =  location ( jpr    )
  locate( 623)  =  location ( kint   )
  locate( 624)  =  location ( kout   )
  locate( 625)  =  location ( nds    )
  locate( 626)  =  location ( nkn    )
  locate( 627)  =  location ( nmax   )
  locate( 628)  =  location ( nuk    )
  locate( 629)  =  location ( kwrite )
  locate( 630)  =  location ( kpr    )
  locate( 631)  =  location ( kpl    )
  locate( 632)  =  location ( mxtacw )
  locate( 633)  =  location ( iptacw )
  locate( 634)  =  location ( nhst   )
  locate( 635)  =  location ( kvin   )
  locate( 636)  =  location ( kvou   )
  locate( 637)  =  location ( kvxx   )
  locate( 638)  =  location ( icsup  )
  locate( 639)  =  location ( nxic   )
  locate( 640)  =  location ( kksj   )
  locate( 641)  =  location ( kksk   )
  locate( 642)  =  location ( kkfst  )
  locate( 643)  =  location ( kkni   )
  locate( 644)  =  location ( kkhst  )
  locate( 645)  =  location ( kifls  )
  locate( 646)  =  location ( kidum  )
  locate( 647)  =  location ( kslim1 )
  locate( 648)  =  location ( kslim2 )
  locate( 649)  =  location ( kslim3 )
  locate( 650)  =  location ( kpac1r )
  locate( 651)  =  location ( kpac1i )
  locate( 652)  =  location ( kpac2r )
  locate( 653)  =  location ( kpac2i )
  locate( 654)  =  location ( kalksx )
  locate( 655)  =  location ( kilms1 )
  locate( 656)  =  location ( kilms2 )
  locate( 657)  =  location ( kdumj  )
  locate( 658)  =  location ( kdumk  )
  locate( 659)  =  location ( kkzj   )
  locate( 660)  =  location ( kkzk   )
  locate( 661)  =  location ( kiflz  )
  locate( 662)  =  location ( kgnz   )
  locate( 663)  =  location ( kzlim1 )
  locate( 664)  =  location ( kzlim2 )
  locate( 665)  =  location ( kalkzx )
  locate( 666)  =  location ( kilmz1 )
  locate( 667)  =  location ( kilmz2 )
  locate( 668)  =  location ( kksus  )
  locate( 669)  =  location ( kalksu )
  locate( 670)  =  location ( kiuty  )
  locate( 671)  =  location ( kud1   )
  locate( 672)  =  location ( kud2   )
  locate( 673)  =  location ( kud3   )
  locate( 674)  =  location ( kud4   )
  locate( 675)  =  location ( kud5   )
  locate( 676)  =  location ( kaliu  )
  locate( 677)  =  location ( ktysup )
  locate( 678)  =  location ( kjsup  )
  locate( 679)  =  location ( kksup  )
  locate( 680)  =  location ( kspvar )
  locate( 681)  =  location ( kopsup )
  locate( 682)  =  location ( kfnsup )
  locate( 683)  =  location ( krgsup )
  locate( 684)  =  location ( kprsup )
  locate( 685)  =  location ( ktypdv )
  locate( 686)  =  location ( kkdj   )
  locate( 687)  =  location ( kkdk   )
  locate( 688)  =  location ( kgndev )
  locate( 689)  =  location ( kdev1  )
  locate( 690)  =  location ( kdev2  )
  locate( 691)  =  location ( kldev1 )
  locate( 692)  =  location ( kldev2 )
  locate( 693)  =  location ( kkdus  )
  locate( 694)  =  location ( kalkdu )
  locate( 695)  =  location ( ktbdev )
  locate( 696)  =  location ( kpn    )
  locate( 697)  =  location ( kpd    )
  locate( 698)  =  location ( kxhst  )
  locate( 699)  =  location ( khscr  )
  locate( 700)  =  location ( khsci  )
  locate( 701)  =  location ( kilim1 )
  locate( 702)  =  location ( kilim2 )
  locate( 703)  =  location ( krowcs )
  locate( 704)  =  location ( krhsde )
  locate( 705)  =  location ( kvlim1 )
  locate( 706)  =  location ( kvlim2 )
  locate( 707)  =  location ( kkxic  )
  locate( 708)  =  location ( kawkcs )
  locate( 709)  =  location ( kxar   )
  locate( 710)  =  location ( kxai   )
  locate( 711)  =  location ( kbwkcs )
  locate( 712)  =  location ( kxtcs  )
  locate( 713)  =  location ( klntab )
  locate( 714)  =  location ( klmxic )
  locate( 715)  =  location ( kcolcs )
  locate( 716)  =  location ( katcs  )
  locate( 717)  =  location ( kbtcs  )
  locate( 718)  =  location ( kjout  )
  locate( 719)  =  location ( kkout  )
  locate( 720)  =  location ( kxmncs )
  locate( 721)  =  location ( ktxmn  )
  locate( 722)  =  location ( kxmxcs )
  locate( 723)  =  location ( ktxmx  )
  locate( 724)  =  location ( klnout )
  locate( 725)  =  location ( ekbuf  )
  locate( 726)  =  location ( ektemp )
  locate( 727)  =  location ( errchk )
  locate( 728)  =  location ( solrsv )
  locate( 729)  =  location ( solisv )
  locate( 730)  =  location ( nitera )
  locate( 731)  =  location ( nekreq )
  locate( 732)  =  location ( nekcod )
  return
end subroutine locating

!
! subroutine sysplt.
!

subroutine sysplt (lunit4)
  use dekplt
  implicit none
  !     Module of interactive emtp only, which services "emtspy".
  !     If no interactive emtp use, this module can be deleted.
  !     It is called only by "sysdep", for storage in "dekplt".
  !     Separate module needed because "dekplt" has no implicit.
  integer(4), intent(in) :: lunit4
  !
  l4plot = lunit4
  return
end subroutine sysplt

!
!     subroutine stopin.
!

subroutine stopin
  use blkcom
  use dekspy
  implicit none
  !     Universal module of interactive emtp (spy of "emtspy").
  !     If non-interactive version, module can be destroyed.  This
  !     module is called only to display erroneous file6(istep), &
  !     prompt user to send a corrected copy.  Called by "datain".
  write (unit = munit6, fmt = 1218) istep
1218 format ('   ? ? ?   Trouble with input data.  Last card', ' number',  i5,  '   is in error.')
  call window
  write (unit = munit6, fmt = 1219)
1219 format ('     12345678901234567890123456789012345678901234567890123456789012345678901234567890')
  call window
  write (unit = munit6, fmt = 1221) file6(istep)
1221 format (a80)
  call window
  if (m4plot .eq. 1) go to 1227
  kill = 79
  lstat(19) = 1218
  go to 9000
1227 write (unit = prom80, fmt = 1232)
1232 format (' Send corrected card (spy, stop) :')
  call prompt
  read (unit = munit5, fmt = 1236) buff77
1236 format (a80)
  if (buff77(1 : 4) .eq. 'stop') call stoptp
  if (buff77(1 : 4) .ne. 'spy')  go to 1244
  call spying
  go to 9000
1244 file6(istep) = buff77
9000 return
end subroutine stopin

!
! subroutine rtmplt.
!

subroutine  rtmplt
  implicit none
  !     module used only for interactive emtp (service to "emtspy").
  !     for non-interactive emtp, this module can be destroyed.
  call tpplot
end subroutine rtmplt

!
! data block blkplt.
!

! block data blkplt
!    use dekspy
!    use dekplt
!    implicit none
!    !     module used only for interactive emtp (service to "emtspy").
!    !     for non-interactive emtp, this module can be destroyed.
!    !   include 'dekspy.ftn'
!    !   include 'dekplt.ftn'
!    data texfnt  /  'f7x13.b '  /
!    data  xytitl(1 : 24)  /  '                        '  /
!    data  headl(1 : 16)   /  '                '  /
!    data  vertl(1 : 16)   /  '                '  /
!    data  horzl(1)    /  'degrees based on 60 hz  '  /
!    data  horzl(2)    /  'cycles based on 60 hz   '  /
!    data  horzl(3)    /  'seconds                 '  /
!    data  horzl(4)    /  'milliseconds            '  /
!    data  horzl(5)    /  'microseconds            '  /
!    data  horzl(6)    /  'frequency in hertz      '  /
!    data  horzl(7)    /  'log10 frequency in hertz'  /
!    data  horzl(8)    /  '1st variable of x-y pair'  /
!    data  curren     /  'current '  /
!    data  voltag     /  'voltage '  /
!    !     begin command-word definitions.   ^^^^^^  ^^^^^^   ^^^^^^   ^^^^^
!    data  choice     /  'choice  '  /
!    data  stop       /  'stop    '  /
!    data  purge      /  'purge   '  /
!    data  help       /  'help    '  /
!    data  smooth     /  'smooth  '  /
!    data  size       /  'size    '  /
!    data  show       /  'show    '  /
!    data  linezz     /  'line    '  /
!    data  photo      /  'chcopy  '  /
!    data  end        /  'end     '  /
!    data  repeat     /  'repeat  '  /
!    data  flush      /  'flush   '  /
!    data  playba     /  'playback'  /
!    data  pen        /  'pen     '  /
!    data  multip     /  'factor  '  /
!    data  offset     /  'offset  '  /
!    data  limits     /  'limits  '  /
!    data  time       /  'time    '  /
!    data  timesp     /  'timespan'  /
!    data  debug      /  'debug   '  /
!    data  tek        /  'tek     '  /
!    data  stack      /  'stack   '  /
!    data  printe     /  'printer '  /
!    data  metric     /  'metric  '  /
!    data  alltim     /  'all time'  /
!    data  column     /  'column  '  /
!    data  setcol     /  'set colu'  /
!    data  out        /  'out     '  /
!    data  longer     /  'longer  '  /
!    data  averag     /  'average '  /
!    data  inner      /  'in      '  /
!    data  rescal     /  'rescale '  /
!    data  lastpl     /  'last    '  /
!    data  batch      /  'batch   '  /
!    data  punch      /  'punch   '  /
!    data  extrem     /  'extrema '  /
!    data  level      /  'level   '  /
!    data  noplot     /  'no plot '  /
!    data  messag     /  'message '  /
!    data  timeun     /  'time uni'  /
!    data  label      /  'label   '  /
!    data  cursor     /  'cursor  '  /
!    data  xyplot     /  'x-y plot'  /
!    data  slope      /  'slope   '  /
!    data  back       /  'back    '  /
!    data  refile     /  'refile  '  /
!    data  texblk     /  'blank   '  /
!    data  setdat     /  'set data'  /
!    !     end of command definitions  ^^^^^^   ^^^^^^   ^^^^^^   ^^^^^^
!    data  tolrce     /  8.e-5  /
!    data  finfin     /  1.e12  /
!    data  timbeg     /   0.0   /
!    data  timend     /  1.e20  /
!    data  paplim     /  36.    /
!    data  vs      /  1.0  /
!    data  vl      /  5.0  /
!    data  vh      /  6.0  /
!    data  nsmplt  /  50   /
!    data  kslowr  /   3   /
!    data  limfix  /   0   /
!    data  klevl   /   0   /
!    data  kextr   /   0   /
!    data  jhmsp   /   0   /
!    data  taxisl  /  5.0  /
!    data  mu6std  /  6  /
!    data  htax    /  4.0  /
!    data  limcol  /  79   /
!    data  ltek    /  1   /
!    data  numtek  /   0   /
!    data  inwait  /   1   /
!    !     begin parameters of tektronix screen
!    data  nxinch   /    74   /
!    data  nyinch   /    68   /
!    data  nxoff    /    100  /
!    data  nyoff    /    40   /
!    data  nxvern   /    30   /
!    data  inchpx   /    2    /
!    data  inchpy   /    2    /
!    data  look     /    6    /
!    data  nymax    /   800   /
!    data  nxmax    /   800   /
!    data  lchid    /    2    /
!    data  lchsup   /    1    /
!    data  lchtit   /    2    /
!    data  lchxax   /    0    /
!    data  lchyax   /    0    /
!    data  izgr1    /    0    /
!    data  izgr2    /    0    /
!    data  ldshg1   /    1    /
!    data  ldshg2   /    1    /
!    data  izxax    /    0    /
!    data  izyax    /    0    /
!    data  izid     /    0    /
!    data  iterm    /    2    /
!    data  ltic     /    7    /
!    data  iztit    /    0    /
!    data  nxid6    /   10   /
!    data  nyid6    /   330  /
!    data  nxend    /   512   /
!    data  nyend    /    50   /
!    data  icurse   /    0    /
!    !   data  ichref   /   'p'   /
!    data ichref    /   112   /
!    !   data  ichend   /   'e'   /
!    data ichend    /   101   /
!    data  vaxisl   /   4.0   /
!    data  fxnumv   /   1.5   /
!    data  fxnumh   /   5.0   /
!    data  fxvert   /   0.0   /
!    data  fsymb    /   .83   /
!    data  lsymb    /    1    /
!    data  lchfil   /    0    /
!    data  lchlim   /    0    /
!    data  ibaud    /   960   /
!    !     end parameters of tektronix screen
!    data  lu7plt   /  7  /
!    data  linepr   /  9  /
!    data  mxypl    /  0  /
!    data  numflt   /  0  /
!    data  numtit   /  0  /
!    data  xtit     /  0.5  /
!    data  ytit     /  8.5  /
!    data  siztit   /  .12  /
!    data  xsuper   /  1.0  /
!    data  ysuper   /  9.0  /
!    data  sizsup   /  0.3  /
!    data  nchsup   /   0   /
!    data  nchver   /   0   /
!    data  dxgrd1   /  1.0  /
!    data  dygrd1   /  1.0  /
!    data  dxgrd2   /  0.2  /
!    data  dygrd2   /  0.2  /
!    data  fill1    /  1.0  /
!    data  fill2    /  1.0  /
!    data  ncut1    /   1   /
!    data  ncut2    /   1   /
!    data  maxsym   /   3   /
!    data  fline    /  1.7  /
!    data  sizid    /  .12  /
!    data  xid      /  0.5  /
!    data  yid      /  .75  /
!    data  fact     /  1.0  /
!    data  fhtax    /  0.5  /
!    data  fxsup    /  0.3  /
!    data  fysup    /  -.03 /
!    data  fxtit    /  .10  /
!    data  fytit    /  .15  /
!    data  fxid     /  .05  /
!    data  fyid     /  .10  /
!    data  ftcarr   /  1.0  /
!    data  fvaxtt   /  -7.5  /
!    data  mtit     /  3  /
!    data  maxisx   /  3  /
!    data  maxisy   /  3  /
!    data  mgrid1   /  2  /
!    data  mgrid2   /  1  /
!    data  msuper   /  5  /
!    data  mid      /  3  /
!    data  mline    /  3  /
!    data  killpl   /  0  /
! end block data

!
! subroutine tpplot.
!

subroutine tpplot
  use dekspy
  use dekplt
  use indcom
  implicit none
  !     Module used only for interactive emtp (service to "emtspy").
  !     For non-interactive EMTP, this module can be destroyed.
  integer(4) :: i, j, k, l, m, numbco, numnam
  integer(4) :: n1
  !
  save
  if (nexmod .gt. 4) go to 3769
  if (nexmod .ne. 4) go to 1742
  nexmod = 0
  !go to nextsn
  select case (nextsn)
  case (5003)
     go to 5003

  case (7361)
     go to 7361

  case (5147)
     go to 5147
  end select
1742 hmax = 0.0
  hmin = 0.0
  ihs = 3
  tmult = 1.0
  maxev = location (bx) - location (ev)
  maxew = location (finfin) - location (ew)
  maxew = maxew - 50
  do j = 1, 6
     sext(j) = blan80
  end do
  do i = 1, 20
     aaa(i) = 1.0
     bbb(i) = 0.0
     mmm(i) = 0
     ylevel(i) = 0.0
     slot1(i) = '        '
  end do
563 do j = 1, 20
     mcurve(j) = mline
  end do
1000 write (unit = prom80, fmt = 5000)
5000 format (' --- Outer :')
  nextsn = 5003
  go to 9800
5003 buffin = buff77
  kill = 0
  if (buffin(1 : 8) .ne. purge) go to 2716
  close (unit = 14, status = 'delete')
  go to 1000
2716 if (buffin(1 : 8) .ne. setdat) go to 2757
  call setrtm
  go to 563
2757 if (buffin(1 : 8) .ne. debug) go to 7368
  write (unit = prom80, fmt = 7358) iprspy
7358 format ('  Supply level-number  iprspy  (', i3,  ' ) :')
  nextsn = 7361
  go to 9800
7361 call frein1 (buff77, iprspy)
  go to 1000
7368 if (iprspy .lt. 1) go to 3877
  write (unit = munit6, fmt = 3876) buffin
3876 format ('  Buffin vector =', a80)
  call window
3877 if (buffin(1 : 8) .eq. stop) go to 3818
  if (buffin(1 : 8) .ne. tek) go to 5136
  if (ltek .eq. 1) go to 7387
  write (munit6, 7385)
7385 format ('  ---- Switch from character to vector-graphic plotting.')
  call window
  go to 7388
7387 write (unit = munit6, fmt = 7386)
7386 format ('  ---- Switch from vector-graphic to character plotting')
  call window
7388 ltek = ltek + 1
  if (ltek .eq. 2) ltek = 0
  go to 1000
5136 if (buffin(1 : 8) .ne. column) go to 5143
  n1 = limcol
  if (n1 .ne. 131) limcol = 131
  if (n1 .ne. 79) limcol =  79
  write (unit = munit6, fmt = 5139)  n1
5139 format ('   Character-plot column width was',  i5, '  columns.')
  call window
  write (unit = munit6, fmt = 5140) limcol
5140 format ('   It is now being toggled to', i5, ' .')
  call window
  go to 1000
5143 if (buffin(1 : 8) .ne. setcol) go to 5154
  write (unit = prom80, fmt = 5146) limcol
5146 format ('  Supply printer-plot column width (', i4, ' ) :')
  nextsn = 5147
  go to 9800
5147 call frein1 (buff77, limcol)
  write (unit = munit6, fmt = 5148) limcol
5148 format ('   Confirmation.   New value =', i4, ' .')
  call window
  go to 1000
5154 if (buffin(1 : 8) .eq. inner) go to 3769
  if (buffin(1 : 4) .eq. 'stop') return
  if (buffin(1 : 5) .ne. help(1 : 5)) go to 5200
  call helper (1)
  go to 1000
5200 newfil = 0
  tstep = -1.0
  rewind l4plot
  read (unit = l4plot) datepl, tclopl, numnam, numnvo, numbco, numbrn, (buslst(i), i = 1, numnam)
  if (iprspy .lt. 2) go to 5739
  write (unit = munit6, fmt = 1001) datepl, tclopl, numnam, numnvo, numbco, numbrn
1001 format (' plot-file header info.', 2(1x, 2a4), 4i6)
  call window
  do j = 1, numnam, 10
     write (unit = munit6, fmt = 5733) (buslst(k), k = j, j + 9)
5733 format (1x, 10a7)
     call window
  end do
!5736 continue
5739 kptplt = numnvo + numbrn
  if (numnvo .gt. 0) read (l4plot) (ibsout(j), j = 1, numnvo)
  if (numbrn .gt. 0) read (l4plot) (ibrnch(j), j = 1, numbrn), (jbrnch(j), j = 1, numbrn)
  if (iprspy .lt. 2) go to 5754
  do j = 1, numnvo, 10
     write (unit = munit6, fmt = 1002) (ibsout(k), k = j, j + 9)
1002 format (' Node numbers ibsout:', 10i5)
     call window
  end do
!5742 continue
  do j = 1, numbrn, 5
     write (unit = munit6, fmt = 5744) (ibrnch(k), jbrnch(k), k = j, j + 4)
5744 format (' Branch node pairs:', 5(2x, 2i4))
     call window
  end do
!5747 continue
5754 numout = numnvo + 2 * numbrn
  nv = numbrn - numbco
  jbegbv = numnvo + 1
  jbegbc = jbegbv + 2 * nv
  nt2 = numbrn * 2
  i = 1
1008 if (i .gt. numnvo) go to 1009
  j = ibsout(i)
  bbus(i) = buslst(j)
  i = i + 1
  go to 1008
1009 j = 1
1012 if (j .gt. numbrn) go to 1013
  k = ibrnch(j)
  l = jbrnch(j)
  bbus(i) = buslst(k)
  i = i + 1
  bbus(i) = buslst(l)
  i = i + 1
  j = j + 1
  go to 1012
1013 if (iprspy .lt. 2) go to 3769
  write (unit = munit6, fmt = 1014) numout, numnvo, nv, nt2, i
1014 format (' numout, numnvo, nv, nt2, i =', 5i6)
  call window
  do j = 1, numout, 10
     write (unit = munit6, fmt = 5763) (bbus(m), m = j, j + 9)
5763 format (1x, 10a6)
     call window
  end do
!5766 continue
3769 call pltvar
  if (nexmod .gt. 0 ) go to 9835
  if (buffin(1 : 4) .eq. 'stop') return
  !     if ( buffin(1:4) .eq. 'spy ' )  return  ! abort "plot" use
  if (kill .eq. 0) go to 1000
3818 close (unit = 14,  status = 'keep')
  !     now stop.   if there were tektronix crt plots, call for
  !     buffer flushing in  #tekplt# .
  if (numtek .gt. 0) call tekplt
9800 if (iprspy .lt. 1) go to 9817
  write (unit = munit6, fmt = 9807) prom80(1 : 40)
9807 format (' Exit "tpplot".   prom80(1 : 40) =', a40)
  call window
9817 nexmod = 4
9835 return
end subroutine tpplot

!
! subroutine helper.
!

subroutine helper (n1)
  use dekspy
  use dekplt
  implicit none
  !     Module used only for interactive EMTP (service to "emtspy").
  !     For non-interactive EMTP, this module can be destroyed.
  !     This module does nothing other than service the "help"
  !     subcommand of the "plot" command of spy.
  integer(4), intent(in) :: n1
  integer(4) :: j, k, n8, n23, n24
  !
  n8 = n1
  if (buffin(5 : 10) .ne. '      ') go to 3618
3613 k = numkey + n8 + 1
  n23 = kbegtx(k)
  n24 = kbegtx(k + 1) - 1
  go to 3673
3618 if (buffin(6 : 10) .ne. 'outer') go to 3622
  n8 = 1
  go to 3613
3622 if (buffin(6 : 11) .ne. 'middle') go to 3627
  n8 = 2
  go to 3613
3627 if (buffin(6 : 10) .ne. 'inner') go to 3634
  n8 = 3
  go to 3613
3634 n23 = kbegtx(numkey + 1)
  n24 = kbegtx(numkey + 5) - 1
  if (buffin(6 : 8) .eq. 'all') go to 3673
  do j = n23, n24
     if (texspy(j)(1 : 3) .eq. '   ') cycle
     if (texspy(j)(3 : 10) .eq. buffin(6 : 13)) go to 3649
  end do
  write (unit = munit6, fmt = 3645)
3645 format ('    ? ? ?   Sorry, no such plot command.   Try again ... ')
  call window
  go to 9000
3649 n23 = j
  do j = n23 + 1, n24
     if (texspy(j)(1 : 3) .ne. '   ') go to 3658
  end do
3658 n24 = j - 1
3673 do j = n23, n24
     write (unit = munit6, fmt = 3677) texspy(j)
3677 format (a80)
     call window
  end do
9000 if (iprspy .lt. 1) go to 9011
  write (unit = munit6, fmt = 9004) n1, n8, n23, n24
9004 format (' Exit  "helper".  n1, n8, n23, n24 =', 4i6)
  call window
9011 return
end subroutine helper

!
! subroutine pltvar.
!

subroutine pltvar
  use dekspy
  use dekplt
  implicit none
  !     Module used only for interactive emtp (service to "emtspy").
  !     for non-interactive emtp, this module can be destroyed.
  integer(4) :: i, ib, ievsw, il, ip, j, jj, k, kplt, l, m
  integer(4) :: n1, n3, n4, n13, n14
  !
  if (iprspy .lt. 1) go to 1003
  write (unit = munit6, fmt = 1002) nexmod
1002 format (' Enter "pltvar".  nexmod =', i4)
  call window
1003 if (nexmod .eq. 6) go to 1550
  if (nexmod .ne. 5) go to 1005
  nexmod = 0
  !  go to nextsn
  select case (nextsn)
  case (1048)
     go to 1048

  case (1052)
     go to 1052

  case (1165)
     go to 1165

  case (1887)
     go to 1887

  case (1892)
     go to 1892

  case (7342)
     go to 7342

  case (31169)
     go to 31169

  case (31882)
     go to 31882
  end select
1005 tolrce = 0.0
  go to 1008
1053 write (unit = prom80, fmt = 1045)
1045 format (' --- Middle :')
  !  assign 1048 to nextsn
  nextsn = 1048
  go to 9800
1048 buffin = buff77
  if (buffin(1 : 4) .eq. 'stop') return
  !     if ( buffin(1:4) .eq. 'spy ' )  return  ! abort "plot" use
  if (buffin(1 : 8) .ne. refile) go to 1007
  write (unit = prom80, fmt = 1004)
1004 format (' File :')
  !  assign 1052 to nextsn
  nextsn = 1052
  go to 9800
1052 buffin = buff77
  newfil = 1
  go to 1053
1007 if (buffin(1 : 8) .eq. inner) go to 1550
  if (buffin(1 : 8) .ne. timesp) go to 5681
1008 timbeg = pltbuf(newvec)
  n4 = (indbuf - newvec ) / (kptplt + 1 )
  if (iprspy .lt. 1) go to 5673
  write (unit = munit6, fmt = 5672) indbuf, newvec, kptplt, n4
5672 format (' Timespan calc.  indbuf, newvec, kptplt, n4 =', 4i8)
  call window
5673 n4 = newvec + n4 * (kptplt + 1)
  timend = pltbuf(n4)
  if (iprspy .lt. 1) go to 35675
  write (unit = munit6, fmt = 5675) newvec, n4, timbeg, timend
5675 format (' pltbuf cells', 2i8, '     tmin, tmax (sec) =', 2e14.5)
  call window
35675 if (iprspy .lt. 5) go to 3218
  do m = newvec, n4, 8
     write (unit = munit6, fmt = 5676) j, (pltbuf(ip), ip = j, j + 7)
5676 format (' pltbuf(', i6, ':)', 8e14.5)
     call window
  end do
!3214 continue
3218 write (unit = munit6, fmt = 5677) timbeg, timend
5677 format ('   Time limits are :', 2e14.6)
  call window
  if (ihs .eq. 0) ihs = 3
  go to 7531
5681 if (buffin(1 : 8) .ne. choice) go to 1957
  call spylin
  i = 1
  nc = 2 * nv
  write (unit = munit6, fmt = 1041)
1041 format (' Type-1 entries (node voltages).')
  call window
  do j = 1, numnvo, 10
     n13 = j + 9
     if (n13 .gt. numnvo) n13 = numnvo
     write (unit = munit6, fmt = 3223) (bbus(ip), ip = j, n13)
3223 format (1x, 10a7)
     call window
  end do
!3227 continue
  i = i + numnvo
  jj = i + nc - 1
  write (unit = munit6, fmt = 1047)
1047 format (' Type-8 entries (branch voltages or powers).')
  call window
  do j = i, jj, 8
     n13 = j + 7
     if (n13 .gt. jj) n13 = jj
     write (unit = munit6, fmt = 3232) (bbus(ip), ip = j, n13)
3232 format (1x, 4(2a7, 3x))
     call window
  end do
!3236 continue
  i = i + nc
  write (unit = munit6, fmt = 1051)
1051 format (' Type-9 entries (branch currents or energies).')
  call window
  do j = i, numout, 8
     n13 = j + 7
     if (n13 .gt. numout) n13 = numout
     write (unit = munit6, fmt = 3232) (bbus(ip), ip = j, n13)
     call window
  end do
!3247 continue
  go to 1053
1957 if (buffin(1 : 8) .ne. stop) go to 4423
  kill = 99
  return
4423 if (buffin(1 : 8) .ne. purge) go to 1958
  close (unit = 14, status = 'delete')
  return
1958 if (buffin(1 : 8) .ne. out) go to 1931
  return
1931 if (buffin(1 : 5) .ne. help(1 : 5)) go to 1059
  call helper (2)
  go to 1053
1059 if (buffin(1 : 8) .eq. label) go to 1210
  if (buffin(1 : 8) .ne. timeun) go to 1167
1155 write (unit = prom80, fmt = 1164) ihs
1164 format ('   Send time-units code (', i2, ' ) :')
  !  assign 1165 to nextsn
  nextsn = 1165
  go to 9800
1165 call frein1 (buff77, n4)
  if (n4 .gt. 0) ihs = n4
  if (ihs .gt. 0 .and. ihs .lt. 8) go to 7531
  write (unit = munit6, fmt = 4163)
4163 format ('  ? ? ?  Illegal value.   Try again ...')
  call window
  go to 1155
7531 if (ihs .eq. 1) tmult = 21600.
  if (ihs .eq. 2) tmult = 60.
  if (ihs .eq. 3) tmult = 1.0
  if (ihs .eq. 4) tmult = 1000.
  if (ihs .eq. 5) tmult = 1.e6
  if (ihs .eq. 6) tmult = 1.0
  if (ihs .eq. 7) tmult = 1.0
  go to 1053
1167 l = 1
  icp = 4
  jplt = 0
7338 write (unit = prom80, fmt = 7341) slot1(l)
7341 format ('   Send node name or end (', a6, ') :')
  !  assign 7342 to nextsn
  nextsn = 7342
  go to 9800
7342 textd1 = buff77(1 : 8)
  if (textd1 .eq. lastpl) go to 1175
!7344 if (textd1 .ne. end) go to 7348
  if (textd1 .ne. end) go to 7348
  icp = 8
  brclas = voltag
  go to 1168
7348 if (textd1 .eq. '        ') textd1 = slot1(l)
  if (textd1 .eq. back) go to 1053
  if (l .eq. 1 .and. jplt .gt. 0 .and. textd1 .eq. repeat) go to 1210
  k = 0
1600 k = k + 1
  if (k .le. numnvo) go to 1640
  write (unit = munit6, fmt = 1620) textd1
1620 format (' ??? List of node voltages does not include  "', a6,  '" .   Try again ....')
  call window
  go to 7338
1640 write (unit = ansi, fmt = 1641) bbus(k)
1641 format (a6, 2x)
  if (textd1 .ne. ansi) go to 1600
  jplt = l
  l = l + 1
  mplot(jplt) = k
  slot1(jplt) = textd1
  go to 7338
1168 write (unit = prom80, fmt = 1169) brclas, slot1(l), slot1(l + 1)
1169 format ('   Send branch ', a7, ' names or end (', a6, ',', a6, ' ) :')
  !  assign 31169 to nextsn
  nextsn = 31169
  go to 9800
31169 textd1 = buff77(1 : 6)
  textd2 = buff77(7 : 12)
  if (textd1 .eq. back) go to 1053
  if (textd1 .eq. end) go to 1175
  if (textd1 .eq. lastpl) go to 1175
  if (textd1 .eq. '        ' .and. textd2 .eq. '        ') go to 1173
  slot1(l) = textd1
  slot1(l + 1) = textd2
1173 if (slot1(l) .eq. '        ' .and. slot1(l + 1) .eq. '        ') go to 1168
  if (icp .eq. 9) go to 1720
  ib = jbegbv
  il = jbegbc
  n1 = ib
  go to 1740
1720 ib = jbegbc
  il = numout
  n1 = jbegbv + nv
1740 if (ib .lt. il) go to 1780
  write (unit = munit6, fmt = 1760) brclas, slot1(l), slot1(l + 1)
1760 format (' ??? Branch ', a7, ' list does not include an entry from  "', a6, '"  to  "', a6, '" .   Try again ....')
  call window
  go to 1168
1780 write (unit = ansi, fmt = 1641) bbus(ib)
  if (slot1(l) .ne. ansi) go to 1800
  write (unit = ansi, fmt = 1641) bbus(ib + 1)
  if (slot1(l + 1) .ne. ansi) go to 1820
  !               node pair found - sign correct
  mplot(jplt + 1) = n1
  go to 1840
1800 write (unit = ansi, fmt = 1641) bbus(ib)
  if (slot1(l + 1) .ne. ansi) go to 1820
  write (unit = ansi, fmt = 1641) bbus(ib + 1)
  if (slot1(l) .ne. ansi) go to 1820
  !               node pair found - sign negative
  mplot(jplt + 1) = -n1
  go to 1840
1820 ib = ib + 2
  n1 = n1 + 1
  go to 1740
1840 jplt = jplt + 1
  l = l + 2
  go to 1168
1175 namvar = l - 1
  if (iprspy .lt. 1) go to 1218
  write (unit = munit6, fmt = 1217) jplt, icp, namvar
1217 format (' Done class.    jplt, icp, namvar =', 3i8)
  call window
1218 if (iprspy .lt. 1) go to 3252
  n14 = jplt
  if (n14 .gt. 20) n14 = 20
  write (unit = munit6, fmt = 3251) (mplot(i), i = 1, n14)
3251 format (' mplot:', 20i6)
  call window
3252 if (textd1 .ne. lastpl .and. icp .lt. 9) go to 1204
  if (jplt .gt. 0) go to 1210
  write (unit = munit6, fmt = 1202)
1202 format ('  ???  Error.   No valid plot variables of any type were specified.   Try again ....')
  call window
  go to 1167
1204 icp = 9
  brclas = curren
  go to 1168
1210 nc = namvar + 1
  do i = nc, 20
!1215 slot1(i) = '        '
     slot1(i) = '        '
  end do
  ievsw = 0
  nc = nt2 / 2  -  nv
  if (iprspy .lt. 1) go to 1880
  write (unit = munit6, fmt = 4217) kptplt, numnvo, numbrn
4217 format (' Assign kptplt.  kptplt, numnvo, numbrn =', 3i10)
  call window
1880 jplt1 = jplt + 1
  kplt = 0
  write (unit = prom80, fmt = 1882) headl(1 : 16)
1882 format ('   Send super-title (', a16, '...) :')
  !  assign 31882 to nextsn
  nextsn = 31882
  go to 9800
31882 alpha = buff77
  if (alpha(1 : 8) .eq. repeat) go to 1550
  if (alpha(1 : 8) .eq. texblk) go to 1536
  do j = 1, 80
     n3 = 81 - j
     if (alpha(n3 : n3) .eq. ' ') cycle
     headl = alpha
     nchsup = n3
     go to 1885
  end do
1536 nchsup = 0
1885 write (unit = prom80, fmt = 1886) vertl(1 : 16)
1886 format ('   Send vertical axis label (', a16, '...) :')
  !  assign 1887 to nextsn
  nextsn = 1887
  go to 9800
1887 vertl = buff77
  if (vertl(1 : 8) .eq. repeat) go to 1550
  if (vertl(1 : 8) .eq. texblk) go to 1545
  do j = 1, 78
     n3 = 81 - j
     if (vertl(n3 : n3) .eq. ' ') cycle
     nchver = n3
     go to 1888
  end do
1545 nchver = 0
1888 n3 = numtit + 1
  write (unit = prom80, fmt = 1891) n3, sext(n3)(1 : 16)
1891 format ('   Send case-title line', i2, ' (', a16, '...) :')
  !  assign 1892 to nextsn
  nextsn = 1892
  go to 9800
1892 alpha = buff77
  if (alpha(1 : 8) .ne. flush) go to 1894
  numtit = 0
  go to 1888
1894 if (alpha(1 : 8) .ne. playba) go to 1896
  write (unit = munit6, fmt = 1895)
1895 format (6x, 'Playback of total title (80a1 format) ...')
  call window
  do ip = 1, numtit
     write (unit = munit6, fmt = 3264) sext(ip)
3264 format (1x, a80)
     call window
  end do
!3267 continue
  go to 1888
1896 if (alpha(1 : 8) .eq. '        ') go to 1899
  if (alpha(1 : 8) .eq. end) go to 1550
  if (alpha(1 : 8) .ne. texblk) go to 1897
  alpha(1 : 8) = '        '
1897 sext(n3) = alpha
1899 numtit = n3
  if (numtit .lt. 5) go to 1901
  write (unit = munit6, fmt = 1547)
1547 format (' ** Warning.  Title storage is now full.   No additional lines can be accepted.')
  call window
1901 go to 1888
1550 call timval
  if (nexmod .gt. 0) go to 9835
  if (kill .eq. 99) go to 9835
  if (buffin(1 : 4) .eq. 'stop') go to 9835
  !     if ( buffin(1:4) .eq. 'spy ' ) go to 9835 ! abort "plot" use
  go to 1053
9800 if (iprspy .lt. 1) go to 9817
  write (unit = munit6, fmt = 9807) prom80(1 : 40)
9807 format (' Exit "pltvar".   prom80(1 : 40) =', a40)
  call window
9817 nexmod = 5
9835 return
end subroutine pltvar

!
! subroutine timval.
!

subroutine timval
  use dekspy
  use dekplt
  implicit none
  !     Module used only for interactive EMTP (service to "emtspy").
  !     for non-interactive EMTP, this module can be destroyed.
  integer(4) :: i, ibase, ip, ipj, ipl, ipontr, iprsup, istold, istore, isw, isww
  integer(4) :: itimes
  integer(4) :: j, j1, k, k9, kk, kl, kplt, kpltq, l, maxevk
  integer(4) :: n1, n4, n5, n7, n8, n13, n16, num
  real(8) :: a
  real(8) :: d1, d2, d3, d4, d6, d7, denom, din1, din2, disqr
  real(8) :: evbasx, evbasy, evdh, evdp, evh, evmx, evmxf, evp
  real(8) :: gymax, gymin, hdif, hvec
  real(8) :: spsave
  real(8) :: vchnge, vdif, vold, vnew, vvec
  !
  maxevk = maxev - 30
  if (iprspy .lt. 1) go to 1549
  write (unit = munit6, fmt = 1386) monitr, limfix, vmin, vmax
1386 format ('  Begin  "timval".  monitr, limfix =', 2i6, '    vmin, vmax =', 2e13.4)
  call window
  n16 = jplt
  if (n16 .gt. 20) n16 = 20
  write (unit = munit6, fmt = 3274) (mplot(i), i = 1, n16)
3274 format (' mplot:', 20i6)
  call window
1549 if (nexmod .ne. 6) go to 31549
  nexmod = 0
  !  go to nextsn
  select case (nextsn)
  case (1556)
     go to 1556

  case (2740)
     go to 2740

  case (4794)
     go to 4794

  case (4811)
     go to 4811

  case (31925)
     go to 31925

  case (31969)
     goto 31969

  case (42721)
     go to 42721

  case (43356)
     go to 43356

  case (52716)
     go to 52716
  end select
31549 if (monitr .eq. 8765) go to 1926
1550 write (unit = prom80, fmt = 1930)
1930 format (' --- Inner :')
  !  assign 1556 to nextsn
  nextsn = 1556
  go to 9800
1556 buffin = buff77
  !     if ( buffin(1:4) .eq. 'spy ' ) go to 9835   ! exit module
  if (buffin(1 : 8) .ne. setdat) go to 2713
  call setrtm
  go to 1550
2713 if (buffin(1 : 8) .ne. 'rollv   ') go to 2715
  monitr = monitr + 1
  if (monitr .ge. 2) monitr = 0
  if (monitr .ne. 0) write (unit = munit6, fmt = 42713)
42713 format ('            ===  Begin  rolling  of vector-graphic  plot.')
  if (monitr .eq. 0) write (unit = munit6, fmt = 2714)
2714 format ('            ===  Previous  rolling  of vector plot is hereby cancelled.  ===')
  call window
  go to 1550
2715 if (buffin(1 : 8) .ne. 'rollc   ') go to 42715
  monits = monits + 1
  if (monits .ge. 2) monits = 0
  if (monits .ne. 0) go to 1550
  write (unit = munit6, fmt = 22714)
22714 format ('            ===  Previous  rolling  of character plot is hereby cancelled.  ===')
  call window
  go to 1550
42715 if (buffin(1 : 8) .ne. 'span    ') go to 2718
  write (unit = prom80, fmt = 2716)
2716 format (' Send desired tmax-tmin:')
  !  assign 52716 to nextsn
  nextsn = 52716
  go to 9800
52716 call fresp1 (buff77, d3)
2717 n4 = (indbuf - newvec) / (kptplt + 1)
  if (iprspy .lt. 1) go to 82717
  write (unit = munit6, fmt = 42717) indbuf, newvec, kptplt, n4
42717 format (' Timespan calculation.  indbuf, newvec, kptplt, n4=', 4i8)
  call window
82717 n4 = newvec + n4 * (kptplt + 1)
  din2 = pltbuf(n4)
  din1 = din2 - d3
  go to 1927
2718 if (buffin(1 : 8) .ne. 'front   ') go to 2720
  d3 = spsave
  go to 2717
2720 if (buffin(1 : 8) .ne. xyplot) go to 2728
  mxypl = mxypl + 1
  if (mxypl .ge. 2) mxypl = 0
  if (mxypl .eq. 0) go to 2724
  write (unit = prom80, fmt = 2721) xytitl(1 : 16)
2721 format (' Send x-axis label (', a16, '...) :')
  !  assign 42721 to nextsn
  nextsn = 42721
  go to 9800
42721 buffin = buff77
  if (buffin(1 : 8) .eq. '        ') go to 2724
  if (buffin(1 : 8) .eq. texblk) go to 2722
  xytitl(1 : 24) = buffin(1 : 24)
  go to 2724
2722 xytitl = '                        '
2724 go to 1550
2728 if (buffin(1 : 8) .ne. cursor) go to 2733
  icurse = icurse + 1
  if (icurse .eq. 2) icurse = 0
  go to 1550
2733 if (buffin(1 : 8) .ne. debug) go to 2742
  write (unit = prom80, fmt = 2738) iprspy
2738 format ('  Supply level-number  iprspy  (', i3, ' ) :')
!  assign 2740 to nextsn
  nextsn = 2740
  go to 9800
2740 call frein1 (buff77, iprspy)
  go to 1550
2742 if (buffin(1 : 8) .ne. extrem) go to 4774
  kextr = kextr + 1
  if (kextr .gt. 1) kextr = 0
  go to 1550
4774 if (buffin(1 : 8) .ne. level) go to 4781
  klevl = klevl + 1
  if (klevl .gt. 1) klevl = 0
  if (klevl .eq. 0) go to 4777
  write (unit = munit6, fmt = 4776) (ylevel(j), j = 1, jplt)
4776 format ('   Input vector of levels :')
  call window
  write (unit = munit6, fmt = 3284) (ylevel(j), j = 1, jplt)
3284 format (1x, 7e10.2)
  call window
  if (ltek .le. 0) go to 34776
  write (unit = munit6, fmt = 3303)
  call window
34776 read (unit = munit5, fmt = *) (ylevel(j), j = 1, jplt)
4777 go to 1550
4781 if (buffin(1 : 8) .ne. stop) go to 4784
  kill = 99
  go to 9835
4784 if (buffin(1 : 8) .ne. smooth) go to 4805
  write (unit = prom80, fmt = 4791) tolrce
4791 format (' Supply new tolerance in inches**2 (', e9.2, ' ) :')
!  assign 4794 to nextsn
  nextsn = 4794
  go to 9800
4794 call fresp1 (buff77, tolrce)
  go to 1550
4805 if (buffin(1 : 8) .ne. size) go to 4823
  write (unit = prom80, fmt = 4808) taxisl
4808 format (' Supply new time-axis length in inches (', f5.1, ' ) :')
!  assign 4811 to nextsn
  nextsn = 4811
  go to 9800
4811 call fresp1 (buff77, d1)
  if (d1 .gt. 2.0) taxisl = d1
  go to 1550
4823 if (buffin(1 : 8) .ne. show) go to 4841
  n1 = limcol + 1
  write (unit = munit6, fmt = 4831)
4831 format (' Parameter settings follow ....')
  call window
  write (unit = munit6, fmt = 2291) n1
2291 format (i15, ' = number of columns in character plot')
  call window
  write (unit = munit6, fmt = 2293) iprspy
2293 format (i15, ' = diagnostic (debug) printout level')
  call window
  write (unit = munit6, fmt = 2295) taxisl
2295 format (e15.3, ' = length of time axis in inches')
  call window
  write (unit = munit6, fmt = 2297) tolrce
2297 format (e15.3, ' = smoothing tolerance in inches')
  call window
  write (unit = munit6, fmt = 2299) linepr
2299 format ( i15, ' = logical unit number of line printer.')
  call window
  write (unit = munit6, fmt = 2303) lu7plt
2303 format (i15, ' = logical unit number of current output')
  call window
  write (unit = munit6, fmt = 2305) ltek
2305 format (i15, ' = ltek (1 if vector plot, 0 otherwise)')
  call window
  write (unit = munit6, fmt = 2307) ihs
2307 format (i15, ' = time units code ihs')
  call window
  write (unit = munit6, fmt = 2309) ibaud
2309 format (i15, ' = ibaud (tektronix plot10 characters/sec)')
  call window
  write (unit = munit6, fmt = 2311) nxmax
2311 format (i15, ' = nxmax (vector points horizontally)')
  call window
  go to 1550
4841 if (buffin(1 : 5) .ne. help(1 : 5)) go to 1990
  call helper (3)
  go to 1550
1990 if (iprspy .lt. 1) go to 8992
  write (unit = munit6, fmt = 8991) kptplt, numnvo, numbrn
8991 format (' kptplt, numnvo, numbrn below 1990 =', 3i8)
  call window
8992 if (buffin(1 : 8) .ne. out) go to 1921
  go to 9835
1921 if (buffin(1 : 8) .ne. multip) go to 1960
  write (unit = munit6, fmt = 1954)
1954 format ('   Input vector of multipliers :')
  call window
  write (unit = munit6, fmt = 3317) (aaa(j), j = 1, jplt)
3317 format (1x, 7e10.2)
  call window
  if (ltek .le. 0) go to 1957
  write (unit = munit6, fmt = 1956)
1956 format (1x)
  call window
!1957 assign 1958 to nextsn
1957 nextsn = 1958
  go to 9800
!1958 read (buff77, 1959) (aaa(j), j = 1, jplt)
  read (buff77, 1959) (aaa(j), j = 1, jplt)
1959 format (10e8.0)
  go to 1550
1960 if (buffin(1 : 8) .ne. offset) go to 1970
  write (unit = munit6, fmt = 1964)
1964 format ('   Input vector of offsets :')
  call window
  write (unit = munit6, fmt = 3317) (bbb(j), j = 1, jplt)
  call window
  if (ltek .le. 0) go to 1969
  write (unit = munit6, fmt = 1956)
  call window
!1969 assign 31969 to nextsn
1969 nextsn = 31969
  go to 9800
31969 read (unit = buff77, fmt = 1959) (bbb(j), j = 1, jplt)
  go to 1550
1970 if (buffin(1 : 8) .ne. limits) go to 1980
  write (unit = prom80, fmt = 1973) vmin, vmax
1973 format ('   Vertical min & max (', 2e12.3, ' ) :')
!  assign 1975 to nextsn
  nextsn = 1975
  go to 9800
!1975 call fresp2 (buff77, vmin, vmax)
  call fresp2 (buff77, vmin, vmax)
  limfix = 1
  if (vmin .eq. 0.0 .and. vmax .eq. 0.0) limfix = 0
  go to 1550
1980 if (buffin(1 : 8) .ne. averag) go to 1989
  write (unit = prom80, fmt = 1982) nsmplt
1982 format ('   Supply consecutive-oscillation limit (', i6, ' ) :')
!  assign 1985 to nextsn
  nextsn = 1985
  go to 9800
!1985 call frein1 (buff77, n1)
  call frein1 (buff77, n1)
  if (n1 .gt. 0) nsmplt = n1
  go to 1550
1989 if (buffin(1 : 8) .ne. rescal) go to 1992
  limfix = 0
  do j = 1, 20
     aaa(j) = 1.0
!1991 bbb(j) = 0.0
     bbb(j) = 0.0
  end do
  go to 1550
1992 if (buffin(1 : 8) .ne. time) go to 5618
  din1 = hmin * tmult
  din2 = hmax * tmult
  write (unit = prom80, fmt = 1925) din1, din2
1925 format ('   Supply tmin & tmax (', 2e12.3, ' ) :')
!  assign 31925 to nextsn
  nextsn = 31925
  go to 9800
31925 call fresp2 (buff77, din1, din2)
  if (din1 .ne. 0.0) go to 1927
  if (din2 .ne. 0.0) go to 1927
  go to 1928
  !     auto internal "time", called from "pltfil" if rolling plot needs
  !     a new page:
1926 d6 = din2 - din1
  d7 = inchlp
  din1 = din2 - (d7 / 100.) * d6
  din2 = din1 + d6
  if (iprspy .le. 0) go to 1927
  write (unit = munit6, fmt = 41927) limfix, inchlp, vmin, vmax, din1, din2
41927 format (' "timval", s.n. 1926.  limfix, inchlp, vmin, vmax, din1, din2 =', 2i8, 4e13.4)
  call window
1927 hmin = din1
  hmax = din2
  spsave = din2 - din1
  hmin=hmin / tmult
  hmax=hmax / tmult
  hpi = (hmax - hmin ) / taxisl
1928 if (iprspy .lt. 1) go to 1942
  write (unit = munit6, fmt = 1934) kplt, jplt
1934 format (' b4 read points.  kplt, jplt =', 2i8)
  call window
  write (unit = munit6, fmt = 3324) hmin, hmax, hpi, taxisl, tmult
3324 format ('         hmin, hmax, hpi, taxisl, tmult =', 5e15.6)
  call window
1942 if (monitr .ne. 2345) go to 5648
  go to 1550
5618 if (buffin(1 : 8) .ne. '        ' .and. buffin(1 : 8) .ne. inner) go to 4431
  if (hmax .gt. hmin) go to 5648
  go to 4437
4431 if (buffin(1 : 8) .ne. alltim) go to 5623
4437 hmin = timbeg
  hmax = timend
  hpi = (hmax - hmin) / taxisl
  go to 5648
5623 write (unit = munit6, fmt = 5624)
5624 format (' ??? Unrecognizable data.  Try again ....')
  call window
  go to 1550
5648 kplt = 0
  if (iprspy .lt. 1) go to 75648
  write (unit = munit6, fmt = 35648) mplot
35648 format (' Begin points extraction.  mplot =', 20i4)
  call window
75648 if (limfix .eq. 1) go to 5647
  vmin = -finfin
  vmax = finfin
5647 vmaxr = vmin + (vmax - vmin) * (vh - vs) / (vl - vs)
  vminr = vmin - (vmax - vmin) * vs / (vl - vs)
  mfake = 0
  do j = 1, jplt
     mmm(j) = 1
     if (aaa(j) .eq. 0.0) go to 5646
     if (aaa(j) .eq. 1.0) go to 5646
     go to 5645
5646 if (bbb(j) .ne. 0.0) go to 5645
  end do
  mmm(j) = 0
  mfake = mfake + 1
5645 continue
!5649 indexp = newvec - (kptplt + 1)
  indexp = newvec - (kptplt + 1)
  if (iprspy .lt. 1) go to 1940
  write (unit = munit6, fmt = 45649) newvec, kptplt
45649 format (' Reinitialize pointer:  newvec, kptplt =', 2i9)
  call window
1940 indexp = indexp + kptplt + 1
  if (iprspy .lt. 3) go to 81940
  write (unit = munit6, fmt = 61940) indexp
61940 format (' indexp at the time test =', i9)
  call window
81940 if (indexp .ge. indbuf) go to 2200
  tstep = pltbuf(indexp)
  if (tstep .ge. hmin) go to 2060
  go to 1940
2060 if (tstep .gt. hmax) go to 2205
  if (indexp .gt. indbuf) go to 2200
  if (kplt .le. maxevk) go to 2100
  write (unit = munit6, fmt = 2068) tstep
2068 format (2x, 'Plot truncated to', e11.3, ' seconds due to filled storage.')
  call window
  go to 2205
2100 kplt = kplt + 1
  ev(kplt) = tstep
  do j = 1, jplt
     j1 = mplot(j)
     if (j1 .ge. 0) go to 2120
     j1 = -j1
     bx(j1) = -bx(j1)
2120 if (mxypl .eq. 0) go to 2126
     if (j .eq. 1) go to 2127
2126 kplt = kplt + 1
2127 ev(kplt) = pltbuf(indexp + j1)
     if (mplot(j) .lt. 0) ev(kplt) = -ev(kplt)
     if (mmm(j) .eq. 0) cycle
     if (aaa(j) .ne. 0.0d0) ev(kplt) = ev(kplt) * aaa(j)
     ev(kplt) = ev(kplt) + bbb(j)
  end do
  go to 1940
2200 tstep = 1.e+30
2205 if (iprspy .lt. 4) go to 3342
  do j = 1, kplt, 8
     write (unit = munit6, fmt = 3336) j, (ev(ip), ip = j, j + 7)
3336 format (' ev(', i5, ':)=', 8e15.6)
     call window
  end do
!3338 continue
3342 ind1 = indexp - (kptplt + 1)
  if (iprspy .lt. 1)  go to 3343
  write (unit = munit6, fmt = 82205) ind1
82205 format (' Another ind1 change.  ind1 =', i8)
  call window
3343 if (mxypl .eq. 0) go to 2218
  k = 0
  n7 = 0
  n8 = 2 * kplt / jplt  +  4
  gxmin = finfin
  gymin = finfin
  gxmax = -gxmin
  gymax = -gymin
8303 if (k .ge. kplt) go to 8317
  kk = n7
  do j=1, jplt, 2
     if (ev(k + 1) .lt. gxmin) gxmin = ev(k + 1)
     if (ev(k + 1) .gt. gxmax) gxmax = ev(k + 1)
     if (ev(k + 2) .lt. gymin) gymin = ev(k + 2)
     if (ev(k + 2) .gt. gymax) gymax = ev(k + 2)
     ew(kk + 1) = ev(k + 1)
     ew(kk + 2) = ev(k + 2)
     k = k + 2
!8310 kk = kk + n8
     kk = kk + n8
  end do
  n7 = n7 + 2
  go to 8303
8317 write (unit = munit6, fmt = 8322) gxmin, gxmax, gymin, gymax
8322 format ('  x-min, y-min =', 2e12.4, '  y-min, y-max =', 2e12.4)
  call window
  write (prom80, 3356)
3356 format (' Send axis limits :')
!  assign 43356 to nextsn
  nextsn = 43356
  go to 9800
43356 read (buff77, 1959)  d1, d2, d3, d4
  if (d1 .ne. 0.0) gxmin = d1
  if (d2 .ne. 0.0) gxmax = d2
  if (d3 .ne. 0.0) gymin = d3
  if (d4 .ne. 0.0) gymax = d4
  vmin = gymin
  vmax = gymax
  mfake = mfake / 2
  l = 0
  kk = n8
!8371 do j = 1, jplt, 2
  do j = 1, jplt, 2
     l = l + 1
     mstart(l) = kk - 4
     ew(kk-3) = gxmin
     ew(kk-2) = gymin
     ew(kk-1) = (gxmax - gxmin) / taxisl
     ew(kk)   = (gymax - gymin) / 8.0
!8376 kk = kk + n8
     kk = kk + n8
  end do
  kk = kk - n8
  jplt = l
  if (iprspy .lt. 1) go to 8382
  write (unit = munit6, fmt = 8381) jplt, (mstart(j), j = 1, jplt)
8381 format (' Done with x-y pack.   jplt =',  i3, 5x,  'mstart(j) :', 5i5)
  call window
8382 if (iprspy .lt. 9) go to 7309
  do j = 1, kk, 8
     write (unit = munit6, fmt = 3361) j, (ew(ip), ip = j, j + 7)
3361 format (' ew(', i5, ':)=', 8e15.6)
     call window
  end do
!3363 continue
  go to 7309
2218 dx = hpi
  evmx = 0.0
  k9 = kplt + jplt1 + jplt1
  do j = 1, jplt
     evh = 0.0
     evdh = 0.0
     isw = 0
     itimes = 0
     do i = 1, kplt, jplt1
        ipj = i + j
        evp = ev(ipj)
        if (isw .gt. 0) go to 2240
        evdp = evp - evh
        if (evdp * evdh .ge. 0.0) go to 2250
        if (isww .eq. 1) go to 2255
        itimes = itimes + 1
        if (itimes .le. nsmplt) go to 2260
        isw = 1
2240    ev(ipj) = (evh + evp) / 2.0
        go to 2260
2250    isww = 1
        go to 2260
2255    itimes = 1
        isww = 0
2260    evdh = evdp
        evh = evp
        evmx = amax1(evmx, abs(ev(ipj)))
        if (ev(ipj) .lt. vminr) go to 2280
        if (ev(ipj) .gt. vmaxr) go to 2300
        cycle
2280    ev(ipj) = vminr
        cycle
2300    ev(ipj) = vmaxr
     end do
  end do
!2340 continue
  if (iprspy .lt. 4) go to 3369
  write (unit = munit6, fmt = 2344)
2344 format (' After points discard.', 11x, 'evmx', 11x, 'vmax', 11x, 'vmin', 9x, 'finfin', 13x, 'vl', 13x, 'vs')
  call window
  write (unit = munit6, fmt = 3366)  evmx, vmax, vmin, finfin, vl, vs
3366 format ( 22x,  6e15.5  )
  call window
3369 if (limfix .eq. 1) go to 2540
  if (evmx .eq. 0.0) evmx = 1.0
2357 a = 8.0
  evmxf = evmx * 0.8
2360 d1 = evmxf - a
  if (d1 .eq. 0.0) go to 2400
  if (d1 .gt. 0.0) go to 2380
  a = a / 10.
  go to 2360
2380 a = a * 10.
  d1 = evmxf - a
  if (d1 .eq. 0.0) go to 2400
  if (d1 .gt. 0.0) go to 2380
  d1 = evmxf - 0.5 * a
  if (d1 .eq. 0.0) go to 2440
  if (d1 .lt. 0.0) go to 2420
2400 vmax = a
  go to 2520
2420 d1 = evmxf - .25 * a
  if (d1 .lt. 0.0) go to 2460
  if (d1 .eq. 0.0) go to 2480
2440 vmax = a / 2.0
  go to 2520
2460 if (evmxf - a / 8.0 .le. 0.0) go to 2500
2480 vmax = a / 4.0
  go to 2520
2500 vmax = a / 8.0
2520 vmin = -vmax
  if (vmax .ge. 0.99 * evmx) go to 2540
  evmx = evmx * 1.1
  go to 2357
2540 dy = (vmax - vmin) / (vl - vs)
  if (kplt .gt. 0) go to 2543
  write (unit = munit6, fmt = 2542) hmin, hmax
2542 format ('   ** Sorry.  No data available between times', 2e13.4, 'sec.')
  call window
  go to 1550
2543 ipl = 0
  kpltq = kplt / jplt1
  istore = 0
  if (iprspy .lt. 4) go to 3382
  write (unit = munit6, fmt = 2548)
2548 format (' Dump cells 1-20 of ev at s.n. 2548 . . . .')
  call window
  write (unit = munit6, fmt = 3374) (ev(i), i = 1, 10)
  call window
  write (unit = munit6, fmt = 3374) (ev(i), i = 11, 20)
3374 format (1x, 10e13.4)
  call window
3382 do i = 1, jplt
     yymin(i) = 1.e30
     yymax(i) = -yymin(i)
     mlevel(i) = 0
     if (klevl .eq. 0) cycle
     mlevel(i) = -1
     ttlev(i) = -9999.0d0
  end do
  do i = 1, jplt
     istold = istore
     ibase = 1
     num = 0
     n13 = 1 + jplt1 + i
     vold = ev(n13)
     istore = istore + 1
3000 evbasx = ev(ibase)
     evbasy = ev(ibase + i)
     ew(istore) = evbasx
     ew(istore + 1) = evbasy
     istore = istore + 2
     if (istore .le. maxew) go to 3004
     write (unit = munit6, fmt = 3001)
3001 format (' Sorry, tables have overflowed during smoothing.')
     call window
     write (unit = munit6, fmt = 3392)
3392 format (' Either try a larger tolerance (to discard more')
     call window
     write (unit = munit6, fmt = 3394)
3394 format (' points), or decrease the time span, or expand')
     call window
     write (unit = munit6, fmt = 3396)  maxew
3396 format (' array  ew(',  i6,  ')  in deck "dekspy" .')
     call window
     go to 1550
3004 if (kextr .eq. 0) go to 2949
     if (evbasy .le. yymax(i)) go to 2941
     yymax(i) = evbasy
     ttmax(i) = evbasx
2941 if (evbasy .ge. yymin(i)) go to 2949
     yymin(i) = evbasy
     ttmin(i) = evbasx
2949 if (mlevel(i) .eq. 0) go to 3010
     d1 = evbasy - ylevel(i)
     if (d1 * dyold(i) .gt. 0.0) go to 2956
     if (mlevel(i) .eq. -1) go to 2954
     d3 = d1 * (evbasx - ew(istore - 4))
     ttlev(i) = evbasx - d3 / (d1 - dyold(i))
     mlevel(i) = 0
     go to 3010
2954 mlevel(i) = 1
2956 dyold(i) = d1
3010 hvec = (ev(ibase + jplt1) - evbasx) / dx
     n13 = ibase + jplt1 + i
     vvec = (ev(n13) - evbasy) / dy
     denom = hvec * hvec + vvec * vvec
     ipontr = ibase + jplt1
3020 ipontr = ipontr + jplt1
     if (ipontr .gt. kplt) go to 3140
     hdif = (ev(ipontr) - evbasx) / dx
     vnew = ev(ipontr + i)
     vdif = (vnew - evbasy) / dy
     disqr = hdif * hdif + vdif * vdif - (hvec * hdif + vvec * vdif) ** 2 / denom
     if (disqr .gt. tolrce) go to 3040
     vchnge = vnew - vold
     vold = vnew
     if (vvec .eq. 0.0 .and. vchnge .eq. 0.0) go to 3020
     if (vvec * vchnge .gt. 0.0) go to 3020
3040 vold = vnew
     ibase = ipontr - jplt1
     go to 3000
3140 ipontr = ipontr - jplt1
     ew(istore) = ev(ipontr)
     ew(istore + 1) = ev(ipontr + i)
     mstart(i) = istore + 1
     istore = istore + 1
     ew(istore + 1) = hmin
     ew(istore + 3) = hpi
     ew(istore + 2) = vmin
     ew(istore + 4) = (vmax - vmin ) / 8.0
     n5 = istore + 4
     if (iprspy .lt. 2) go to 3406
     write (unit = munit6, fmt = 3176)
3176 format (' Store calcomp scaling.       i  istore', 10x, '(ew(j), j = istore, n5)')
     call window
     write (unit = munit6, fmt = 3402) i, istore, (ew(kl), kl = istore, n5)
3402 format (23x, 2i8, 5e15.6)
     call window
3406 istore = n5
     if (iprspy .lt. 1) go to 3235
     write (munit6,3234) i, istore
3234 format (' Done smoothing next curve.  i, istore =', 2i8)
     call window
3235 numpts(i) = (istore - istold) / 2
  end do
!3240 continue
3263 if (kextr .eq. 1) go to 3270
  if (klevl .eq. 1) go to 3288
  go to 7309
3270 write (unit = munit6, fmt = 3271)
3271 format (' mplot  name-1  name-2', 7x, 'minimum', 7x, 'maximum',  6x, 't of min', 6x, 't of max')
  call window
  l = 1
  do j = 1, jplt
     textd1 = slot1(l)
     textd2 = '        '
     l = l + 1
     if (iabs(mplot(j)) .le. numnvo) cycle
     textd2 = slot1(l)
     l = l + 1
     write (unit = munit6, fmt = 3282) mplot(j), textd1, textd2, yymin(j), yymax(j), ttmin(j), ttmax(j)
3282 format (1x, i5, 2(2x, a6), 4e14.5)
     call window
  end do
  if (klevl .eq. 0) go to 3296
3288 write (unit = munit6, fmt = 3294) (ylevel(j), j = 1, jplt)
3294 format (' Levels sought :', 4e15.4)
  call window
  write (unit = munit6, fmt = 3295) (ttlev(j), j = 1, jplt)
3295 format (' 1st hit time :', 4e16.6)
  call window
3296 if (ltek .le. 0) go to 43305
  write (unit = munit6, fmt = 3303)
3303 format (' ')
  call window
43305 read (munit5, 3305) textd1
3305 format (a8)
  if (textd1 .eq. '        ') go to 7309
  if (textd1 .ne. linezz) go to 3306
  mu6std = linepr
  go to 3263
3306 if (textd1 .eq. noplot) go to 1550
7309 if (ltek .ne. 0) call tekplt
  if (ltek .eq. 0) call chrplt
  if (monitr .ne. 8765) go to 1550
  !     check if special "pltvar" call for rolling plot paging:
  if (iprsup .le. 0) go to 7324
  write (unit = munit6, fmt = 7318)
7318 format (' Done with "tekplt", monitr = 8765 so exit "timval".')
  call window
7324 go to 9835
9800 if (iprspy .lt. 1) go to 9817
  write (unit = munit6, fmt = 9807) prom80(1 : 40)
9807 format (' Exit "timval".   prom80(1 : 40) =', a40)
  call window
9817 nexmod = 6
9835 return
end subroutine timval

!
! subroutine back14.
!

subroutine back14
  use dekspy
  use dekplt
  implicit none
  !     Module used only for interactive EMTP (service to "emtspy").
  !     For non-interactive emtp, this module can be destroyed.
  if (iprspy .lt. 1) go to 1483
  write (unit = munit6, fmt = 1478) tstep
1478 format (' Top of  "back14" .   tstep =', e15.5)
  call window
1483 tstep = -1.0
  return
end subroutine back14

!
! subroutine setrtm.
!

subroutine setrtm
  use dekspy
  use dekplt
  implicit none
  !     Module used only for interactive EMTP (service to "emtspy").
  !     For non-interactive EMTP, this module can be destroyed.
  integer(4) :: ip, j, jj, lunt15, n1, n2, n22, n23
  real(8) :: d1, d6
  !
  save
  lunt15 = 15
  close (unit = lunt15)
  open (unit = lunt15, status = 'old', file = 'tpparam')
  if (iprspy .lt. 1) go to 2719
  write (unit = munit6, fmt = 2718)
2718 format (' After unit lunt15 connection tpparam.')
  call window
2719 n22 = 0
  n23 = 0
  rewind lunt15
  read (lunt15, 5100, end = 2729) buffin
5100 format (a80)
  rewind lunt15
  if (buffin(1 : 8) .ne. setdat) go to 2732
  write (prom80, 2724)
2724 format ('  Send subset number :')
  call prompt
  read (munit5, 5100) prom80
  call frein1 (prom80, n1)
  if (n1 .eq. 0) n1 = 1
  do j = 1, 9999
     read (lunt15, 5151, end = 2729) buffin
5151 format (a16)
     if (buffin(1 : 8) .ne. setdat) cycle
     write (unit = ansi, fmt = 5100) buffin(9 : 16)
     read (unit = ansi, fmt = 2726) n2
2726 format (i2)
     if (n2 .eq. n1) go to 2732
  end do
2729 write (unit = munit6, fmt = 2730) n1
2730 format (' ** Error.   Subset number', i4, '  does not exist.   Try again.')
  call window
  go to 2751
2732 read (unit = lunt15, fmt = 5100) prom80
  call fresp2 (prom80, d6, d1)
!  jj = d6
  jj = int (d6, kind (jj))
  if (jj .eq. 0) go to 2741
  if (jj .gt. n23) n23 = jj
  if (iprspy .lt. 5) go to 2736
  write (unit = munit6, fmt = 2735) jj, fvcom(jj), d1
2735 format (' Next real datum.  jj, fvcom, d1 =', i6, 2e14.5)
  call window
2736 fvcom(jj) = d1
  go to 2732
2741 read (lunt15, 5100) prom80
  call frein2 (prom80, jj, n1)
  if (jj .eq. 0) go to 2745
  if (jj .gt. n22) n22 = jj
  if (iprspy .lt. 5) go to 2744
  write (unit = munit6, fmt = 2743) jj, ivcom(jj), n1
2743 format (' Next integer datum.  jj, ivcom, n1 =', 3i8)
  call window
2744 ivcom(jj) = n1
  go to 2741
2745 read (lunt15, 2746) jj, textd1
2746 format (i2, a8)
  if (jj .le. 0) go to 2751
  if (iprspy .lt. 5) go to 2748
  write (unit = munit6, fmt = 2747) jj, anplt(jj), textd1
2747 format (' Next key word.  j =', i3, 1x, a8, 1x, a8)
  call window
2748 anplt(jj) = textd1
  go to 2745
2751 if (iprspy .lt. 3) go to 2763
  write (unit = munit6, fmt = 2753)
2753 format (' Final real data fvcom through the last modified cell follows ....')
  call window
  do j = 1, n23, 6
     write (unit = munit6, fmt = 3413) (fvcom(ip), ip = j, j + 5)
3413 format (1x, 6e13.4)
     call window
  end do
!3417 continue
  write (unit = munit6, fmt = 2754)
2754 format (' Final integer data ivcom through the last modified cell follows ....')
  call window
  do j = 1, n22, 10
     write (unit = munit6, fmt = 3424) (ivcom(ip), ip = j, j + 9)
3424 format (1x, 10i7)
     call window
  end do
!3427 continue
2763 close (unit = lunt15, status = 'keep')
  return
end subroutine setrtm

!
! subroutine chrplt.
!

subroutine chrplt
  use dekspy
  use dekplt
  implicit none
  !     Module used only for interactive EMTP (service to "emtspy").
  !     For non-interactive emtp, this module can be destroyed.
  character(9) :: kunit6
  character(1) :: dol(131), letter(20)
  integer(4) :: inch, ip
  integer(4) :: j, j1
  integer(4) :: k, kwtspy, kzero
  integer(4) :: l
  integer(4) :: m
  integer(4) :: n4
  real(8) :: d1, d2, d3, din1, din2, dt, idat, t, temp(6), vspan
  !  dimension temp(6)
  !
  save
  data letter(1)  /  'a'  /
  data letter(2)  /  'b'  /
  data letter(3)  /  'c'  /
  data letter(4)  /  'd'  /
  data letter(5)  /  'e'  /
  data letter(6)  /  'f'  /
  data letter(7)  /  'g'  /
  data letter(8)  /  'h'  /
  data letter(9)  /  'i'  /
  data letter(10) /  'j'  /
  data letter(11) /  'k'  /
  data letter(12) /  'l'  /
  data letter(13) /  'm'  /
  data letter(14) /  'n'  /
  data letter(15) /  'o'  /
  data letter(16) /  'p'  /
  data letter(17) /  'q'  /
  data letter(18) /  'r'  /
  data letter(19) /  's'  /
  data letter(20) /  't'  /
  if (monits .eq. 1) go to 2513
  vspan = vmax - vmin
  d3 = limcol / vspan
!  kzero = -vmin * d3
  kzero = int (-vmin * d3)
  if (kzero .eq. 0) kzero = 1
  if (kzero .lt. 0) kzero = 0
  kp(1) = 1
  dy = vspan / (0.1 * limcol)
  if (mfake .eq. jplt) go to 6413
  write (unit = munit6, fmt = 6403)
6403 format (' Hidden variable scalings follow :')
  call window
  write (unit = munit6, fmt = 6411) ( aaa(j), j = 1, jplt)
6411 format ('  Factor :', 10e11.3)
  call window
  write (unit = munit6, fmt = 6412) (bbb(j), j = 1, jplt)
6412 format ('  Offset :', 10e11.3)
  call window
6413 din1 = hmin * tmult
  din2 = hmax * tmult
  do ip = 1, numtit
     write (unit = munit6, fmt = 6423) sext(ip)
6423 format (' Graph title : ', a80)
     call window
  end do
!6427 continue
  if (mxypl .gt. 0) go to 6582
  if (jplt .le. 1) go to 6447
  do k = 2, jplt
!6431 kp(k) = mstart(k - 1) + 5
     kp(k) = mstart(k - 1) + 5
  end do
6447 dt = hpi / 6.0
  temp(1) = vmin + dy
  d2 = 2.0 * dy
  n4 = 6
  if (limcol .le. 79) n4 = 4
  do k = 2, n4
!6449 temp(k) = temp(k - 1) + d2
     temp(k) = temp(k - 1) + d2
  end do
  write (unit = munit6, fmt = 6450) (temp(k), k = 1, n4)
6450 format (3x, 6(e14.5, 6x))
  call window
  if (limcol .le. 79) go to 6453
  write (unit = munit6, fmt = 6451)
6451 format (1x, 13('1', 9('-')))
  call window
  go to 6456
6453 write (unit = munit6, fmt = 6454)
6454 format (1x, 8('1', 9('-')))
  call window
6456 if (iprspy .lt. 1) go to 6460
  write (unit = munit6, fmt = 6458)
6458 format (' Begin character plot.', 11x, 'hmin', 11x, 'hmax',  13x, 'dt',  11x, 'vmin',  11x, 'vmax')
  call window
  write (unit = munit6, fmt = 6459) hmin, hmax, dt, vmin, vmax
6459 format (22x, 5e15.6)
  call window
6460 inch = 0
  idat = jplt
  t = hmin
6461 t = t + dt
  inch = inch + 1
  call quiter
  if (kwtspy .eq. 1) go to 9000
  if (t .gt. hmax) go to 6582
  do k = 1, limcol
!6478 dol(k) = ' '
     dol(k) = ' '
  end do
  do k = 1, jplt
     l = kp(k)
     if (iprspy .ne. 34) go to 6483
     write (unit = munit6, fmt = 6481) k, l, mstart(k), ew(l), t
6481 format (1x, '       k       l  mstart', 10x, 'ew(l)', 14x, 't')
     call window
     write (unit = munit6, fmt = 6482) k, l, mstart(k), ew(l), t
6482 format (1x, 3i8, 2e15.6)
     call window
6483 if (l .eq. 0) cycle
6485 if (ew(l) .le. t) go to 6493
     kp(k) = l
     cycle
6493 d1 = ew(l + 1)
     if (d1 .gt. vmin) go to 6503
     dol(1) = letter(k)
     go to 6518
6503 if (d1 .lt. vmax) go to 6508
     dol(limcol) = letter(k)
     go to 6518
!6508 m = (d1 - vmin) * d3
6508 m = int ((d1 - vmin) * d3)
     dol(m) = letter(k)
     if (iprspy .ne. 34) go to 6518
     write (unit = munit6, fmt = 6511) m, dol(m)
6511 format (' In-bounds insert..  m, dol(m) =', i4, 1x, a1)
     call window
6518 l = l + 2
     if (l .lt. mstart(k)) go to 6485
     kp(k) = 0
     idat = idat - 1
  end do
  if (inch .ge. 6) go to 6548
  if (kzero .le. 9) go to 6542
  if (dol(kzero) .eq. ' ') dol(kzero) = '1'
6542 write (unit = munit6, fmt = 6543) (dol(k), k = 1, limcol)
6543 format (1x, 131a1)
  call window
  go to 6559
6548 if (t .le. 9.999999) write (unit = kunit6, fmt = 6553) t
6553 format (' ', f8.6)
  if (t .ge. 10.0) write (kunit6, 6554) t
6554 format (' ', f8.5)
  if (kzero .le. 9) go to 6555
  if (dol(kzero) .eq. ' ') dol(kzero) = '1'
6555 write (unit = munit6, fmt = 6556) kunit6, (dol(k), k = 10, limcol)
6556 format (1x, a9, 123a1)
  call window
  inch = 0
6559 if (idat .gt. 0) go to 6461
6582 if (mu6std .eq. 6) go to 2694
  mu6std = 6
  write (unit = munit6, fmt = 2687)  linepr
2687 format ('   ***  Line printer copy completed on unit', i3, ' .  ***')
  call window
2694 if (monits .eq. 0) go to 9000
  if (iprspy .lt. 1) go to 2501
  write (unit = munit6, fmt = 7832) ind1
7832 format (' Begin rolling.  ind1 =', i8)
  call window
2501 t = t + dt
  call quiter
  if (kwtspy .eq. 1) go to 9000
  inch = inch + 1
  do k = 1, limcol
!2504 dol(k) = ' '
     dol(k) = ' '
  end do
2507 if (ind1 .le. indbuf - kptplt) go to 2513
  go to 9000
2513 if (iprspy .lt. 3) go to 2517
  write (unit = munit6, fmt = 2516) ind1, jplt, pltbuf(ind1), t
2516 format (' Past 1/6 inch? ind1, jplt, pltbuf(ind1), t =', 2i7, 2e15.5)
  call window
2517 if (pltbuf(ind1) .gt. t) go to 2564
  do k = 1, jplt
     j1 = iabs(mplot(k))
     d1 = pltbuf(ind1 + j1)
     if (mplot(k) .lt. 0) d1 = -d1
     if (d1 .gt. vmin) go to 2532
     dol(1) = letter(k)
     cycle
2532 if (d1 .lt. vmax) go to 2537
     dol(limcol) = letter(k)
     cycle
!2537 m = (d1 - vmin ) * d3
2537 m = int ((d1 - vmin ) * d3)
     dol(m) = letter(k)
  end do
  ind1 = ind1 + kptplt + 1
  go to 2507
2564 if (inch .lt. 6) go to 2571
  write (unit = ansi8, fmt = 2569) t
2569 format (1x, f7.4)
  read (unit = ansi8, fmt = 2566)  (dol(ip), ip = 1, 8)
2566 format ( 8a1 )
  inch = 0
  go to 2574
2571 if ( kzero  .eq.  0 )   go to 2574
  if ( dol(kzero)  .eq.  ' ' )   dol(kzero) = '1'
2574 write (unit = munit6, fmt = 6543)  ( dol(k), k=1, limcol )
  call window
  go to 2501
9000 if ( iprspy .lt. 1 )  go to 9008
  write (unit = munit6, fmt = 9003)
9003 format (' Exit "chrplt".')
  call window
9008 return
end subroutine chrplt

!
! subroutine tekplt.
!

subroutine tekplt
  implicit none
  return
end subroutine tekplt

!
!     subroutine flatbd.
!

subroutine flatbd
  use dekspy
  use dekplt
  implicit none
  !     Module used only for interactive EMTP (service to "emtspy").
  !     For non-interactive emtp, this module can be destroyed.
  !     The one and only use is to provide calcomp copies of what
  !     is on the screen (for "plot" command of "emtspy"), with
  !     BPA pseudo-calcomp versatek calls actually used.
  integer(4) :: j
  integer(4) :: lmask1, lmask2
  integer(4) :: mm
  integer(4) :: n1, n2, n3, n4, n5, n6, n7, n8
  real(8) :: d1, d2, d3, d6
  real(8) :: n9, sx, sy, xmin, ymin
  !
  if (killpl .eq. 0) go to 2005
  call plot (0.d0, 0.d0, 999)
  return
2005 d1 = hpi * tmult
  write (unit = prom80, fmt = 2010) d1
2010 format (' Send t-axis units/inch (', e12.3, ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) buff77                   ! read a80 character input
  if (buff77(1 : 8) .ne. metric) go to 7236
  fact = .7874
  dxgrd1 = 0.5
  dygrd1 = dxgrd1
  dxgrd2 = 0.1
  dygrd2 = dxgrd2
  go to 2005
7236 if (buff77(1 : 8) .ne. pen) go to 7261
  write (unit = munit6, fmt = 7238)
7238 format (' Respond to each pen-width request with integer', /, ' information.  Blank or zero implies that there', /, ' will be no change from the present value, while', /, ' a value of  -1  suppresses the printout, and', /, ' values of  1  through  5  are versatek dot', /, ' widths (VAX "calcomp" at BPA is Versatek).')
  write (unit = prom80, fmt = 7240) mtit
7240 format (3x, '80-col. case-title (', i2, ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
7241 format (a80)
  call frein1 (prom80, n1)                                  ! decode free-field n1
  if (n1 .ne. 0) mtit = n1
  write (unit = prom80, fmt = 7242) maxisx
7242 format (3x, 'x-axis structure (', i2, ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call frein1 (prom80, n1)                                  ! decode free-field n1
  if (n1 .ne. 0) maxisx = n1
  write (unit = prom80, fmt = 7244) maxisy
7244 format (3x, 'y-axis structure (', i2, ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call frein1 (prom80, n1)                                  ! decode free-field n1
  if (n1 .ne. 0) maxisy = n1
  write (unit = prom80, fmt = 7246) mgrid1
7246 format (3x, 'Big background grid (',  i2,  ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call frein1 (prom80, n1)                                  ! decode free-field n1
  if (n1 .ne. 0) mgrid1 = n1
  write (unit = prom80, fmt = 7247) mgrid2
7247 format (3x, 'Fine inner grid (', i2, ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call frein1 (prom80, n1)                                  ! decode free-field n1
  if (n1 .ne. 0) mgrid2 = n1
  write (unit = prom80, fmt = 7250) msuper
7250 format (3x, 'Super-title (', i2, ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call frein1 (prom80, n1)                                  ! decode free-field n1
  if (n1 .ne. 0) msuper = n1
  write (unit = prom80, fmt = 7251) mid
7251 format (3x, 'Date, time, etc. (', i2, ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call frein1 (prom80, n1)                                  ! decode free-field n1
  if (n1 .ne. 0) mid = n1
  write (unit = munit6, fmt = 7254) (mcurve(j), j = 1, jplt)
7254 format (3x, 'Vector of curve weights (', 20i3, ' ) :')
  read (munit5, *) (kp(j), j = 1, jplt)
  do j = 1, jplt
     if (kp(j) .ne. 0) mcurve(j) = kp(j)
  end do
!7255 continue
  go to 2005
7261 if (buff77(1 : 8) .ne. show) go to 7287
  write (unit = munit6, fmt = 3421) mtit, maxisx, maxisy, mgrid1, mgrid2, msuper, mid
3421 format (/, ' Begin with  #pen#  parameters :', /, i5, ' = pen for 80-column case title lines;', /, i5, ' = pen for x-axis structure;', /, i5, ' = pen for y-axis structure;', /, i5, ' = pen for big background grid;', /, i5, ' = pen for fine inner grid;', /, i5, ' = pen for 1-line super-title;', /, i5, ' = pen for date, time, etc.')
  write (unit = munit6, fmt = 3428) (mcurve(j), j = 1, jplt)
3428 format (8x, 'Pens for drawing individual curves follow ...', /, ( 8x, 10i5))
  write (unit = munit6, fmt = 3453) xtit, ytit, siztit, xid, yid, sizid, xsuper, ysuper, sizsup
3453 format (/, ' Next come the  #size#  parameters :', /, 4x, 'x-begin',  3x,  'y-begin',  3x,  'height', /, 1x, 3f10.2, '  --- 80-col. case title (1st line)', /, 1x, 3f10.2, '  --- date, time, etc. (top line)', /, 1x, 3f10.2, '  --- one line super-title')
  write (unit = munit6, fmt = 3459) htax, fact, numsym
3459 format (/, 1x, f14.3, '  --- height of time axis;', /, 1x, f14.6, '  --- Graph magnification factor (.7874 for metric);', /, 1x, i14, '  --- Number of symbols marking each curve.')
  write (unit = munit6, fmt = 3464) fill1, fill2, ncut1, ncut2
3464 format (/, ' Major grid', 4x, 'minor grid', /, 1x, f10.3, f14.3, '  --- fill-in fractions (1.0 for solid grid);', /, 1x, i10, i14, '  --- number of dashes per grid line;')
  write (unit = munit6, fmt = 3472) dxgrd1, dxgrd2, dygrd1, dygrd2
3472 format (1x, f10.3, f14.3, '  --- spacing between vertical grid lines;', /, 1x,  f10.3,  f14.3,  '  --- spacing between horizontal grid lines;', /, 1x)
  go to 2005
7287 if (buff77(1 : 8) .ne. size) go to 7328
  write (unit = munit6, fmt = 7288)
7288 format (' Respond to each request with revised values.   A blank or zero means no', /, ' change from the value shown within square brackets.')
  write (unit = prom80, fmt = 7291) xtit, ytit, siztit
7291 format ('   x, y coordinates and size of 79-char. Title  (', 3f6.2, ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call fresp3 (prom80, d1, d2, d3)                          ! decode free-field
  if (d1 .ne. 0.0d0) xtit = d1
  if (d2 .ne. 0.0d0) ytit = d2
  if (d3 .ne. 0.0d0) siztit = d3
  write (unit = prom80, fmt = 7304) xid, yid, sizid
7304 format ('   Likewise for date, time, etc.  (', 3f6.2,  ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call fresp3 (prom80, d1, d2, d3)                          ! decode free-field
  if (d1 .ne. 0.0d0) xid = d1
  if (d2 .ne. 0.0d0) yid = d2
  if (d3 .ne. 0.0d0) sizid = d3
  write (unit = prom80, fmt = 7309) xsuper, ysuper, sizsup
7309 format ('   Likewise for 16-char. super-title  (', 3f6.2,  ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call fresp3 (prom80, d1, d2, d3)                          ! decode free-field
  if (d1 .ne. 0.0d0) xsuper = d1
  if (d2 .ne. 0.0d0) ysuper = d2
  if (d3 .ne. 0.0d0) sizsup = d3
  write (unit = prom80, fmt = 7315) htax
7315 format ('   Height of time-axis  (', f6.2, ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call fresp1 (prom80, d1)                                  ! decode free-field d1
  if (d1 .ne. 0.0d0) htax = d1
  write (unit = prom80, fmt = 7319) fact
7319 format ('   Blowup factor.  metric=.7874  (', f6.4,  ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call fresp1 (prom80, d1)                                  ! decode free-field d1
  if (d1 .ne. 0.0d0) fact = d1
  write (unit = prom80, fmt = 7320) fill1, fill2
7320 format ('   Fill-in fractions for major and minor grids  (', 2f6.3,  ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call fresp2 (prom80, d1, d2)                              ! decode free-field d1, d2
  if (d1 .ne. 0.0d0) fill1 = d1
  if (d2 .ne. 0.0d0) fill2 = d2
  write (unit = prom80, fmt = 7321) ncut1, ncut2
7321 format ('   Number of breaks for major and minor grids  (', i4,  '.0 ',  i4,  '.0 ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call fresp2 (prom80, d1, d2)                              ! decode free-field d1, d2
  if (d1 .ne. 0.0d0) ncut1 = int (d1, kind (ncut1))
  if (d2 .ne. 0.0d0) ncut2 = int (d2, kind (ncut2))
  write (unit = prom80, fmt = 7322) dxgrd1, dygrd1
7322 format ('   x, y spacing between major grid lines  (', 2f6.3,  ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call fresp2 (prom80, d1, d2)                              ! decode free-field d1, d2
  if (d1 .gt. 0.0d0) dxgrd1 = d1
  if (d2 .gt. 0.0d0) dygrd1 = d2
  write (unit = prom80, fmt = 7323) dxgrd2, dygrd2
7323 format ('   x, y spacing between minor grid lines  (', 2f6.3,  ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call fresp2 (prom80, d1, d2)                              ! decode free-field d1, d2
  if (d1 .gt. 0.0d0) dxgrd2 = d1
  if (d2 .gt. 0.0d0) dygrd2 = d2
  write (unit = prom80, fmt = 7325) numsym
7325 format ('   Number of marking symbols per curve  (', i3,  ' ) :')
  call prompt                                               ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 7241) prom80
  call frein1 (prom80, n2)                                  ! decode free-field n2
  if (n2 .ne. 0) numsym = n2
  go to 2005
7328 read (unit = buff77, fmt = 2030) d3
2030 format (10e6.0)
  if (d3 .eq. 0.0d0) go to 2060
  hpi = d3 / tmult
2060 d1 = (hmax - hmin) / hpi
  if (d1 .le. 72.0d0) go to 2090
  write (unit = munit6, fmt = 2070)
2070 format (' Error. ----- requested flatbed plot would exceed 72 inch limit.', /, ' Send another time-axis scaling.')
  go to 2005
2090 if (iprsrt .ge. 1) write (unit = munit6, fmt = 2103) jplt, ihs, numflt, vmin, vmax, hmin, hmax, hpi
2103 format (/, ' Begin flatbed.    jplt     ihs  numflt', /, 15x, 3i8, /, 1x, 11x, 'vmin', 11x, 'vmax', 11x, 'hmin', 11x, 'hmax', 12x, 'hpi', /, 1x, 5e15.6)
  if (numflt .gt. 0) go to 2108
  call plots (0, 0, 0)
  call plot (1.0d0, 1.0d0, -3)
  call factor (fact)
2108 numflt = numflt + 1
  write (unit = munit6, fmt = 2109)
2109 format ('    Ready to draw calcomp titles.')
  n3 = numtit / 10
  if (n3 .le. 0) go to 2114
  if (mtit .eq. -1) go to 2114
  d2 = ytit
  n4 = 1
  if (ytit .gt. 10) go to 2114
  call newpen (mtit)
  do j = 1, n3
     call symbol (xtit, d2, siztit, sext(n4), 0.0d0, 80)
     d2 = d2 - fline * siztit
!2113 n4 = n4 + 10
     n4 = n4 + 10
  end do
2114 n1 = 3 * (ihs - 1) + 1
  !     only units of  #seconds#  can be presently honored.
  n1 = 7
  if (mxypl .eq. 1) n1 = 22
  d2 = (vmax - vmin) / 8.0
  call newpen (maxisx)
  write (unit = munit6, fmt = 2115)
2115 format ('   Ready to draw calcomp axes.')
  if (maxisx .gt. 0) call axis (0.0d0, htax, horzl(n1), -24, d1, 0.0d0, hmin, hpi)
  call newpen (maxisy)
  if (maxisy .gt. 0) call axis (0.0d0, 0.0d0, vertl, nchver, 8.0d0, 90.0d0, vmin, d2)
!  n8 = d1 / dxgrd1 + 0.5
  n8 = int (d1 / dxgrd1 + 0.5d0)
  n9 = 8.0d0 / dygrd1 + 0.5d0
  fill = fill1
  ncut = ncut1
  call newpen (mgrid1)
  write (unit = munit6, fmt = 2116)
2116 format ('   Ready to call calcomp grid number 1.')
  if (mgrid1 .gt. 0) call grid (0.0d0, 0.0d0, n8, dxgrd1, n9, dygrd1, lmask1)
  write (unit = munit6, fmt = 2117)
2117 format ('   Done with calcomp grid number 1.')
  call newpen (mgrid2)
  fill = fill2
  ncut = ncut2
!  n8  =  d1 / dxgrd2 + 0.5
  n8 = int (d1 / dxgrd2 + 0.5)
  n9  =  8.0d0 / dygrd2 + 0.5
  if (mgrid2 .gt. 0) call grid (0.0d0, 0.0d0, n8, dxgrd2, n9, dygrd2, lmask2)
  call newpen (msuper)
  write (unit = munit6, fmt = 2118)
2118 format ('   Done with calcomp grid number 2.')
  if (msuper .gt. 0) call symbol (xsuper, ysuper, sizsup, headl, 0.0d0, nchsup)
  d6 = yid
  !     encode (18, 2119, alpha(1) )   date1, tclock
  write (unit = ansi32, fmt = 2119) datepl, tclopl
2119 format (2a4, 2x, 2a4)
  call newpen (mid)
  if (mid .eq. -1) go to 2127
  call symbol (xid, d6, sizid, ansi32, 0.0d0, 18)
  d6 = d6 - fline * sizid
  !     encode (14, 2122, alpha(1) )   icp
  write (unit = ansi16, fmt = 2122) icp
2122 format ('Plot type', i5)
  call symbol (xid, d6, sizid, ansi16, 0.0d0, 14)
  d6 = d6 - fline * sizid
  if (mfake .eq. jplt) go to 3149
  n5 = jplt
  if (jplt .gt. 6) n5 = 6
  !     encode (80, 3146, alpha(1) )  ( aaa(j), j=1, n5 )
  write (unit = buff77, fmt = 3146) (aaa(j), j = 1, n5)
3146 format (' factor :', 6e11.3)
  call symbol (xid, d6, sizid, buff77, 0.0d0, 80)
  d6 = d6 - fline * sizid
  !     encode (80, 3147, alpha(1) )  ( bbb(j), j=1, n5 )
  write (unit = buff77, fmt = 3147) (bbb(j), j = 1, n5)
3147 format (' offset :',  6e11.3)
  call symbol (xid, d6, sizid, buff77, 0.0d0, 80)
  d6 = d6 - fline * sizid
3149 write (unit = munit6, fmt = 3150)
3150 format ('   Ready for slot1 encode.')
  n5 = 4
  if (namvar .lt. 4) n5 = namvar
  !     encode (36, 2125, alpha(1) )   ( slot1(j), j=1, n5 )
  write (unit = buff77, fmt = 2125) (slot1(j), j = 1, n5)
2125 format ('names : ',  4a7)
  call symbol (xid, d6, sizid, buff77, 0.0d0, 36)
  if (namvar .le. 4) go to 2127
  do j = 5, namvar, 4
     n5 = j + 3
     !     encode (36, 2137, alpha(1) )
     write (unit = buff77, fmt = 2137)
2137 format (36x)
     !     encode (36, 2138, alpha(1) )   ( slot1(i), i=j, n5 )
     write (unit = buff77, fmt = 2137)
!2138 format (8x,  4a7)
     d6 = d6 - fline * sizid
!2140 call symbol (xid, d6, sizid, buff77, 0.0, 36)
     call symbol (xid, d6, sizid, buff77, 0.0d0, 36)
  end do
2127 n4 = 0
  do j = 1, jplt
     n5 = kstart(j)
     n6 = (n5 - n4) / 2
     n7 = n6 / numsym
     if (numsym .lt. 0) n7 = 99999
     if (numsym .gt. n6) n7 = 1
     ew(n5 + 3) = hpi
     n8 = n4 + 2 * n6 + 8
     if (iprsrt .ge. 5) write (unit = munit6, fmt = 3117) j, n4, n5, n6, n7, (ew(mm), mm = n4, n8)
3117 format (/, ' Ready to call line.       j      n4      n5      n6      n7', /, 20x, 5i8, /, (1x, 8e15.6))
     call newpen (mcurve(j))
     sx = 1.0d0 / ew(n5 + 3)
     sy = 1.0d0 / ew(n5 + 4)
     xmin = ew(n5 + 1)
     ymin = ew(n5 + 2)
     d1 = (ew(n4 + 1) - xmin) * sx
     d2 = (ew(n4 + 2) - ymin) * sy
     call plot (d1, d2, 3)
     n6 = n7
     if (iprsrt .ge. 2) write (unit = munit6, fmt = 4122) xmin, ymin, sx, sy, d1, d2
4122 format (/, ' Enter curve-loop.  xmin, ymin, sx, sy, d1, d2 =', /, 1x, 6e13.4)
2121 n4 = n4 + 2
     if (n4 .ge. n5) go to 2132
     n6 = n6 - 1
     if (n6 .gt. 0) go to 8243
     call symbol (d1, d2, sizid, char (j), 0.0d0, -1)
     n6 = n7
8243 d1 = (ew(n4 + 1) - xmin) * sx
     d2 = (ew(n4 + 2) - ymin) * sy
     if (iprsrt .ge. 9) write (unit = munit6, fmt = 2123) n4, n6, d1, d2
2123 format (' Next point drawn.  n4, n6, d1, d2 =',  2i5, 2e13.4)
     call plot (d1, d2, 2)
     go to 2121
2132 n4 = n5 + 4
  end do
  d1 = d1 + 1.0
  call plot (d1, 0.d0, -3)
  if (iprspy .ge. 1) write (unit = munit6, fmt = 9006)
9006 format ('   Done with calcomp copy of screen.')
  return
end subroutine flatbd

!
! subroutine tgrid.
!

subroutine tgrid (ix, iy, nx, idelx, ny, idely, ldash)
  implicit none
  !     Module used only for interactive EMTP (service to "emtspy").
  !     for non-interactive EMTP, this module can be destroyed.
  !     Reliance upon Tektronix plot10 makes this installation-
  !     dependent (this is VAX-11 module, actually).
  integer(4), intent(in) :: idelx, idely, ix, iy, ldash, nx, ny
  integer(4) :: j, n2, n3, n4, n8, n9
  !
  if (nx .le. 0) return
  if (ny .le. 0) return
  call movabs (ix, iy)
  n8 = int (ix + (nx - 1) * idelx, kind (n8))
  n9 = int (iy + (ny - 1) * idely, kind (n9))
  n3 = n9
  n2 = int (ix, kind (n2))
  do j = 1, nx
     call dshabs (n2, n3, ldash)
     n2 = n2 + idelx
     if (j .eq. nx) go to 1356
     call movabs (n2, n3)
     n4 = n3
     n3 = n9
     if (n4 .eq. n9) n3 = iy
  end do
1356 call movabs (n8, n9)
  n2 = ix
  n3 = n9
  do j = 1, ny
     call dshabs (n2, n3, ldash)
     if (j .eq. ny) go to 1382
     n3 = n3 - idely
     call movabs (n2, n3)
     n4 = n2
     n2 = n8
     if (n4 .eq. n8) n2 = ix
  end do
1382 return
end subroutine tgrid

!
! end of file over20.f90
!
