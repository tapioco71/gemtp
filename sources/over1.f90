!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over1.f90
!

module komthl
  implicit none
  real(8) :: pekexp
end module komthl
!
! subroutine over1.
!

subroutine over1
  use blkcom
  use labcom
  use umcom
  use com2
  use dekspy
  use indcom
  use iocons
  use bcdtim
  use bcddat
  use movcop
  use strcom
  use freedom
  use comlock
  implicit none
  !     %include  '//c/tsu/cables.ins.ftn'
  !     To avoid "insert deck tacsar" here, use small part of it:
  character(8) :: text1, text2, text6, datexx(2)
  character(8) :: text3, text4, text5, tcloxx(2)
  character(80) :: disk_file
  character(132) :: ansi132
  integer(4) :: i, iadqq, ijk, ios, ip, iswent, iy, j
  integer(4) :: k, kswpe4
  integer(4) :: ll1, ll6, ll8, ll11, ll20, ll24, ll25, ll30, ll40, ll60, ll64, ll80
  integer(4) :: lstacs(8), lu2, lu6, lunt77
  integer(4) :: n5, n6, n7, n8, n9, n12, n14, n15, n18, n19, n23, nfdbr, nfdhst, nfdph
  integer(4) :: nfdpol, ngroup, niunrs, nk, ntlin, nturn, num888, numnam
  integer(4) :: numbco, numbrn
  real(8) :: d1, d2, d3, d4, d7, d8, d12, d13, ddd, znvref
  !  dimension kpen(1), ibusum(1)
  !  dimension aupper(14)
  !  dimension lstacs(8)
  !
  !  equivalence (volti(1), r4(1))
  !  equivalence (bus1, kpen(1))
  !  equivalence (busum(1), ibusum(1))
  !  equivalence (moncar(1), knt)
  !  equivalence (moncar(2), kbase)
  !  equivalence (moncar(4), isw)
  !  equivalence (moncar(5), idist)
  !  equivalence (moncar(6), itest)
  !  equivalence (moncar(8), jseedr)
  !  equivalence (moncar(9), kloaep)
  !  equivalence (moncar(10), mtape)
  !  equivalence (iprsov(39), nmauto)
  !
  !     default list sizes for tacs proportioning of emtp list 19.
  !
  data text2 / 'name  ' /
  data text6 / 'copy  ' /
  data text1 / 'tacs o' /
  data text3 / 'tacs h' /
  data text4 / 'tacs s' /
  data text5 / '4     ' /
  data text6 / 'tacs  ' /
  data ll1   /  1 /
  data ll6   /  6 /
  data ll8   /  8 /
  data ll11  / 11 /
  data ll20  / 20 /
  data ll24  / 24 /
  data ll25  / 25 /
  data ll30  / 30 /
  data ll40  / 40 /
  data ll60  / 60 /
  data ll64  / 64 /
  data ll80  / 80 /
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module over1."')
  lstacs(1) = 20
  lstacs(2) = 90
  lstacs(3) = 100
  lstacs(4) = 20
  lstacs(5) = 30
  lstacs(6) = 250
  lstacs(7) = 350
  lstacs(8) = 60
  pu = fltinf
  !  call move0 (ktrlsw(1), ll6)
  call move0 (ktrlsw(1 :), ll6)
  !  call move0 (ipntv(1), ll11)
  call move0 (ipntv(1 :), ll11)
  !  call move0 (loopss(1), ll11)
  call move0 (loopss(1 :), ll11)
  moncar(7) = 0
  nmauto = 0
  busum(1) = text2
  nright = 0
  iofgnd = 0
  moldat = 0
  istep = 0
  kloaep = 0
  tenerg = 1.0e+20
  branch = text2
  chcopy = text6
  ialter = 0
  isprin = 0
  isplot = 0
  indstp = 1
  noutpr = 0
  do i = 0, 15
     lunit(i) = i
  end do
  speedl = 2.997925d8
  peaknd(1) = 0.0
  kburro = 0
  ! assign default relative precision for 6 EMTP variable types.
  ! 1 --- alphanumeric              2 --- complex
  ! 3 --- floating-point numeric    4 --- integer numeric
  ! 5 --- 3 of 'subr31'             6 --- 4 of 'subr31'
  ! setting all equal to unity means that all have equal length.
  !  call copyi (1, nbyte(1), 6)
  call copy (1, nbyte(1 :), 6)
  call dimens (lstat(1), nchain, bus1, bus2)
  lbus   = lstat(1)
  lbrnch = lstat(2)
  ldata  = lstat(3)
  lexct  = lstat(4)
  lymat  = lstat(5)
  lswtch = lstat(6)
  lsize7 = lstat(7)
  lpast  = lstat(8)
  lnonl  = lstat(9)
  lchar  = lstat(10)
  lsmout = lstat(11)
  lsiz12 = lstat(12)
  lfdep  = lstat(13)
  lwt    = lstat(14)
  ltails = lstat(15)
  limass = lstat(16)
  lsyn   = lstat(17)
  maxpe  = lstat(18)
  ltacst = lstat(19)
  lfsem  = lstat(20)
  lfd    = lstat(21)
  lhist  = lstat(22)
  lsiz23 = lstat(23)
  lcomp  = lstat(24)
  lspcum = lstat(25)
  lsiz26 = lstat(26)
  lsiz27 = lstat(27)
  lsiz28 = lstat(28)
  ! assign  'n1'  equal to the number of emtp lists of variable
  ! dimensioning (of the solution overlays).
  n1 = 28
  ltlabl = lstat(n1 + 1)
  ! locker(1) = bus1
  ! read (unit = bus1, fmt = '(i4)') locker(1)
  call str2int (bus1, locker(1), ios)
  if (.not. (ios .eq. 0)) then
     write (unit = lunit(6), fmt = "('Error converting bus1 string.  Stop.')")
     stop
  end if
  ! locker(2) = bus2
  ! read (unit = bus2, fmt = '(i4)') locker(2)
  call str2int (bus2, locker(2), ios)
  if (.not. (ios .eq. 0)) then
     write (unit = lunit(6), fmt = "('Error converting bus2 string.  .Stop.')")
     stop
  end if
  ! write (*,*) ' save  locker =',  locker
  d13 = ltacst
  d13 = d13 / 1600.0
  do j = 1, 8
     lstacs(j) = int (lstacs(j) * d13, kind (lstacs(j)))
  end do
  n1 = -9999
  !  call copyi (n1, lstat(1), ll60)
  call copy (n1, lstat(1 :), ll60)
  call sysdep
  call move0 (flstat, ll20)
  call runtym (d1, d2)
  flstat(1) = flstat(1) - d1
  flstat(2) = flstat(2) - d2
  tenm6 = tenm3 ** 2
  maxzno = 50
  epszno = epsiln
  epwarn = tenm3
  epstop = 0.1d0
  znolim(1) = 1.0d0
  znolim(2) = 1.5d0
  kbase = 0
  isw = 0
  kprchg(1) = -7777
  jflsos = 0
  rewind lunit(1)
  rewind lunit(3)
  rewind lunit(4)
  knt = 1
  mtape = lunit(5)
  ifdep = 0
  ntcsex = 0
  nstacs = 0
  kanal = 0
  ! initialize "kpartb" with nonzero value so later minus
  ! sign can be applied as flag of "renumber bypass" usage:
  kpartb = 1000
  ! "sysdep" will redefine if low-prec. complex ("cable constants"):
  znvref = 0.0d0
  fmaxfs = 0.0d0
  begmax(1) = 0.0d0
  begmax(2) = 0.0d0
  begmax(3) = fltinf
  ktref = 0
  ncomp = 0
  numsm = 0
  numum = 0
  numout = 0
  kill = 0
  num99 = 0
  modout = 0
  inecho = 0
  bus(1) = blank
  iaverg = 0
  !  call move0 (isourc(1), lswtch)
  call move0 (isourc(1 :), lswtch)
  !  call move0 (kodebr(1), lbrnch)
  call move0 (kodebr(1 :), lbrnch)
  !  call move0 (kodsem(1), lbrnch)
  call move0 (kodsem(1 :), lbrnch)
  !  call move0 (length(1), lbrnch)
  call move0 (length(1 : ), lbrnch)
  !  call move0 (indhst(1), lbrnch)
  call move0 (indhst(1 :), lbrnch)
  !  call mover0 (bvalue(1), lsiz12)
  call move0 (bvalue(1 :), lsiz12)
  iswent = 1
  omega = 0.0
  degmin = 0.0
  degmax = 0.0
  sglfir = twopi
  jst = 0
  jst1 = 1
  ifdep2 = 1
  iadd = 0
  ifsem = 0
  idm = 1
  idq = 1
  idu = 1
  idt = 1
  iadqq = 1
  iq = 0
  ida = 0
  idy = 0
  ifkc = 0
  nk = 0
  ngroup = 0
  nturn = 0
  nfdbr = 27
  nfdph = 9
  nfdhst = 5400
  ! nfdpol = 25      ! assign maximum order of rational approximation
  nfdpol = 30
  ntlin = 0
  lu2 = 69
  lu6 = lunit(6)
  ! if ( abuff(1:5) .ne. 'file:' )  go to 5223
  ! write (*,*) ' ready to open  abuff(6:40) =',  abuff(6:40)
  ! open ( unit=lu2,  status='old',  file=abuff(6:40) )
  ! write (*,*) ' successful open abuff(6:40) =', abuff(6:40)
  ! call cimage  ! erase spcl request of cables by rding 1st real brnch
  ifx = 0
  ip = 2
  iy = 2
  icat = 0
  if (noutpr .ne. 0) go to 15
  if (kol132 .eq. 132) go to 6452
  write (unit = lunit(6), fmt = 6438) ltlabl, lbus, lbrnch, ldata, lexct, lymat, lswtch, lsize7, lpast, lnonl, lchar, lsmout, lsiz12, lfdep, lwt, ltails, limass, lsyn, maxpe, ltacst, lfsem, lfd, lhist, lsiz23, lcomp, lspcum, lsiz26, lsiz27, lsiz28
6438 format (' *********  Begin "M40." EMTP solution.   Size /label/ =', i7, '  integer words.', /, ' list limits  1-10 :', 10i6, /, ' list limits 11-20 :', 10i6, /, ' list limits 21-end:', 10i6)
  go to 15
6452 write (unit = lunit(6), fmt = 83044) locker
83044 format (' Associated user documentation is the 864-page EMTP rule book dated  June, 1984.   Version M43.   Vardim time/date =', 2i7)
  write (unit = lunit(6), fmt = 5241) ltlabl, lbus, lbrnch, ldata, lexct, lymat, lswtch, lsize7, lpast, lnonl, lchar, lsmout, lsiz12, lfdep, lwt, ltails, limass, lsyn, maxpe, ltacst, lfsem, lfd, lhist, lsiz23, lcomp, lspcum, lsiz26, lsiz27, lsiz28
5241 format (' Independent list limits follow. Total length of /label/  equals ', i8, '  integer words.', 3x, 6i6, /, (1x, 21i6, i5))
  write (unit = lunit(6), fmt = 83049)
  write (unit = lunit(6), fmt = 83047) (i, i = 1, 8)
83047 format (' Descriptive interpretation of new-case input data 1 input data card images printed below, all 80 columns, character by character.', /, 51x, '0', 8(9x, i1))
  j = 0
  write (unit = lunit(6), fmt = 83048) (j, i = 1, 8)
83048 format (51x, '0', 8(9x, i1))
  write (unit = lunit(6), fmt = 83049)
83049 format (1x, 50('-'), '+', 80('-'))
  ! begin loop over all input data cards read by "over1" :
  ! read input card using cimage.
15 do
     call cimage
     if (kill .gt. 0) go to 9200
     nright = -2
     n9 = kolbeg
     kolbeg = 1
     !     read (unit = abuff(1), fmt = 3246, iostat = ios) texcol
     !3246 format (80a1)
     read (unit = abuff, fmt = 3246, iostat = ios) texcol
3246 format (80a1)
     if (ios .ne. 0) then
        write (unit = lunit(6), fmt = "('Could not read from abuff. over1 libe 3246.  Stop.')")
        call stoptp
     end if
     !     call freone (d1)
     call free (d1)
3247 nright = 0
     if (n9 .eq. -intinf) kolbeg = n9
     if (nfrfld .gt. 0) go to 3280
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 3270)
3270 format ('+Blank termination-of-run card.')
     call interp
     ivolt = 7777
3273 kill = 9999
     nchain = 31
     go to 9800
3280 call reques
     i = lstat(18)
     select case (i)
     case (0)       ! request word number  0  implies miscellaneous data cards: 0
        go to 2843

     case (15)      ! next come exceptions handled outside subroutine: 15, 32, 33
        go to 8015

     case (32)
        go to 8032

     case (33)
        go to 8033

     case (11)      ! next come exceptional terminations (special jumps): 11, 28, 34, 38, 40
        go to 3273

     case (28)
        go to 3247

     case (34)
        go to 6523

     case (38)
        go to 15

     case (40)
        go to 4308
     end select
     ! next come exits to other overlays:
     if (kill .gt. 0) go to 9200
     if (nchain .ne. 1) go to 9800
     ! continue in loop of request words, if none of above.
  end do
  ! $$$$$    special-request word no. 15.   'start again'       $$$$$
8015 ialter = lunit(2)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 2857)
2857 format ('+Continue partially-completed data case.')
  if (texcol(13) .ne. text5) go to 2861
  read (unit = abuff, fmt = 2852) disk_file
2852 format (a80)
  do j = 1, 2
     n7 = index (disk_file, ',')
     disk_file(1 : n7) = ' '
  end do
  do j = n7, 80
     if (disk_file(j : j) .ne. ' ') go to 2849
  end do
2849 write (unit = *, fmt = *) ' name of old pl4 file  disk_file(j : 80) =', disk_file(j : 80)
  lunt77 = 77
  open (unit = lunt77, status = 'old', file = disk_file(j : 80), form = 'unformatted' )
  texcol(13) = blank
  rewind lunt77
  read (unit = lunt77) datexx, tcloxx, numnam, numnvo, numbco, numbrn
  write (unit = *, fmt = *) ' 1st record.  numnam, numnvo, numbco, numbrn =', numnam, numnvo, numbco, numbrn
  if(lbus + lsize7 .ge. numnam .and. lsiz12 .ge. numnvo .and. lbrnch .ge. numbrn) go to 2859
  write (unit = lunit(6), fmt = 2858) numnam, numnvo, numbrn
2858 format (' Temporary error stop in "over1".   Program dimensioning is inadequate.   numnam, numnvo, numbrn =', 3i8)
  call stoptp
2859 rewind lunt77
  n8 = lbus
  if(lbus .ge. numnam) n8 = numnam - 1
  write (unit = *, fmt = *) ' New name logic. n8, numnam, lbus =', n8, numnam, lbus
  read (unit = lunt77) datexx, tcloxx, numnam, numnvo, numbco, numbrn, (bus(j), j = 1, n8)
  write (unit = *, fmt = 9442) (bus(j), j = 1, n8)
9442 format (' bus(1 : n8) =', 10a7)
  rewind lunt77
  num888 = numnam - n8
  write (unit = *, fmt = *) ' Ready for final, full read with  num888 =', num888
  read (unit = lunt77) datexx, tcloxx, numnam, numnvo, numbco, numbrn, (bus(j), j = 1, n8), (texvec(j), j = 1, num888)
  write (unit = *, fmt = *) ' After all names are read.  next, ibsout.'
  if (numnvo .gt. 0) read (unit = lunt77) (ibsout(j), j = 1, numnvo)
  write (unit = *, fmt = *) ' After ibsout.  next, ....'
  if (numbrn .gt. 0) read (unit = lunt77) (kbus(j), mbus(j), j = 1, numbrn)
  write (unit = *, fmt = *) ' After kbus, mbus.'
  write (unit = lunit(4)) date1, tclock, numnam, numnvo, numbco, numbrn, (bus(j), j = 1, n8), (texvec(j), j = 1, num888)
  write (unit = *, fmt = 3899) date1, tclock
3899 format (' over1, lunit(4) date and time =', 2a4, 2x, 2a4)
  if (numnvo .gt. 0) write (unit = lunit(4)) (ibsout(j), j = 1, numnvo)
  if (numbrn .gt. 0) write (unit = lunit(4)) (kbus(j), mbus(j), j = 1, numbrn)
  n18 = numnvo + numbrn + 1
  write (unit = *, fmt = *) ' Enter loop over numbers.  n18 =', n18
  do j = 1, 99999
     read (unit = lunt77, end = 6539) (r4(k), k = 1, n18)
     if (iprsup .eq. 7 .or. iprsup .gt. 9) write (unit = *, fmt = *) ' j, r4(1) =', j, r4(1)
     if (r4(1) .eq. -9999.) go to 6539
     write (unit = lunit(4)) (r4(k), k = 1, n18)
  end do
6539 write (unit = *, fmt = *) ' Done transferring lunt77 to lunit(4).  j =', j
2861 call runtym (d1, d2)
  n18 = locker(1)
  n19 = locker(2)
  call pfatch
  call tables
  write (unit = *, fmt = *) ' n18, n19, locker(1), locker(2) =', n18, n19, locker(1), locker(2)
  flstat(1) = -d1
  flstat(2) = -d2
  if (n18 .eq. locker(1) .and. n19 .eq. locker(2)) go to 2863
  kill = 201
  lstat(19) = 2861
  go to 9200
2863 continue
  ! read input card using cimage.
2868 call cimage
  read (unit = abuff, fmt = 1212) ijk
1212 format (54x, i6)
  if (ijk .ne. 0) go to 6161
  if (kolbeg .gt. 0) go to 2872
  read (unit = abuff, fmt = 2870) n3, d7, d8
2870 format (i8, 2e16.0)
  go to 2875
2872 nfrfld = 1
  call freone (d3)
  n3 = int (d3)
  !  call freone (d7)
  call free (d7)
  !  call freone (d8)
  call free (d8)
  !  call freone (ddd)
  call free (ddd)
2875 if (n3 .eq. 9999) go to 2879
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 2876)  n3, d7, d8
2876 format ('+Altered switch.', i4, 2e13.4)
  tclose(n3) = d7
  if (d8 .gt. 0.0) topen(n3) = d8
  go to 2868
6161 if (iabs (ijk) .eq. 1111) go to 6363
  call swmodf
  go to 2868
6363 call tacs1c
  go to 2868
2879 if (noutpr .eq. 0) write (unit = kunit6, fmt = 2882)
2882 format ('+Terminator for switch closing times.')
  read (unit = lunit(2)) locker
  do j = 1, 9999
     read (unit = lunit(2), end = 2479) ansi132
2474 format (a132)
     write (unit = lunit(6), fmt = 2474) ansi132
  end do
2479 limstp = 0
  indstp = 1
  isplot = 0
  flstat(14) = t
  nchain = 1
  lastov = 0
  go to 15
  !     $$$$$  special request-word no. 32.  'absolute tacs dimensions'  $
8032 if (noutpr .eq. 0) write (unit = kunit6, fmt = 7020)
7020 format ('+set absolute TACS list size limits.')
  !     read input card using cimage
  call cimage
  if (kolbeg .gt. 0) go to 7030
  call intchk (ll1, ll80, ll8)
  if (kill .gt. 0) go to 9200
  read (unit = abuff, fmt = 5) (lstacs(i), i = 1, 8)
  go to 7050
7030 nfrfld = 10
  !  call frefld (voltbc)
  call free (voltbc)
  do i = 1, 8
     lstacs(i) = int (voltbc(i))
  end do
7050 if (lstacs(1) .lt. 5) lstacs(1) = 5
  if (noutpr .eq. 0 ) write (unit = kunit6, fmt = 7060) lstacs(1), lstacs(2), lstacs(3)
7060 format ('+1st TACS dimensions card.', 3i6)
  go to 15
  ! $$$$$  special request-word no. 33.  'relative tacs dimensions'  $
8033 if (noutpr .eq. 0) write (unit = kunit6, fmt = 7110)
7110 format ('+Proportional allocation of total TACS storage.')
  ! read input card using cimage
  call cimage
  if (kolbeg .gt. 0) go to 7120
  call expchk (ll1, ll80, ll8)
  if (kill .gt. 0) go to 9200
  read (unit = abuff, fmt = 3415) (voltbc(i), i = 1, 10)
  go to 7130
7120 nfrfld = 10
  !  call frefld (voltbc)
  call free (voltbc)
7130 if (noutpr .eq. 0) write (unit = kunit6, fmt = 7140) voltbc(1), voltbc(2), voltbc(3)
7140 format ('+Relative list sizes.', 3e9.2)
  d1 = 0.0
  do i = 1, 8
     d1 = d1 + voltbc(i)
  end do
  d1 = ltacst * nbyte(3) / d1
  lstacs(1) = int (voltbc(1) * d1 / (4 * nbyte(3) + 8 * nbyte(4)))
  lstacs(2) = int (voltbc(2) * d1 / (2 * nbyte(3) + nbyte(4)))
  lstacs(3) = int (voltbc(3) * d1 / (2 * nbyte(4)))
  lstacs(4) = int (voltbc(4) * d1 / (5 * nbyte(3) + nbyte(4)))
  lstacs(5) = int (voltbc(5) * d1 / (3 * nbyte(4)))
  lstacs(6) = int (voltbc(6) * d1 / nbyte(4))
  lstacs(7) = int (voltbc(7) * d1 / nbyte(3))
  lstacs(8) = int (voltbc(8) * d1 / (6 * nbyte(3) + 2 * nbyte(4)))
  go to 15
  !     begin processing floating point misc. data card ....
2843 if (noutpr .ne. 0 .and. iprsup .gt. 0) noutpr = 0
  xopt = statfr
  copt = statfr
  kolbeg = n9
  if (kolbeg .gt. 0) go to 4201
  call expchk (ll1, ll80, ll8)
  if (kill .gt. 0) go to 9200
  !  read (unit = abuff, fmt = 3415, iostat = ios) deltat, tmax, d1, d2, d3, tolmat, t
  !3415 format (10e8.0)
  read (unit = abuff, fmt = 3415, iostat = ios) deltat, tmax, d1, d2, d3, tolmat, t
3415 format (10e8.0)
!  if (ios .ne. 0) then
!     write (unit = lunit(6), fmt = "(' Could not read from abuff.  over1 line 3415.  Stop.')")
!     call stoptp
!  end if
  if (t .eq. 0.0) t = 0.0d0
  go to 4202
4201 nfrfld = 1
  nright = 0
  !  call freone (deltat)
  call free (deltat)
  !  call freone (tmax)
  call free (tmax)
  !  call freone (d1)
  call free (d1)
  !  call freone (d2)
  call free (d2)
  !  call freone (d3)
  call free (d3)
  !  call freone (tolmat)
  call free (tolmat)
  !  call freone (t)
  call free (t)
4202 if (noutpr .eq. 0) write (unit = kunit6, fmt = 4205) deltat, tmax, d1
4205 format ('+Misc. data.', 3e12.3)
  if (iofbnd .ne. 33666) go to 4206
  nchain = 41
  xopt = d1
  go to 9800
  !     read input card using cimage.
4206 call cimage
  if (kolbeg .gt. 0) go to 4207
  call intchk (ll1, ll80, ll8)
  if (kill .gt. 0) go to 9200
  read (unit = abuff, fmt = 5, iostat = ios) iout, iplot, idoubl, kssout, maxout, ipun, memsav, icat, n1, n2
5 format (10i8)
!  if (ios .ne. 0) then
!     write (unit = lunit(6), fmt = 42061)
!42061 format (' Could not read from abuff.')
!     call stoptp
!  end if
  go to 4208
4207 nfrfld = 10
  !  call frefld (voltbc)
  call free (voltbc)
  iout = int (voltbc(1))
  iplot = int (voltbc(2))
  idoubl = int (voltbc(3))
  kssout = int (voltbc(4))
  maxout = int (voltbc(5))
  ipun = int (voltbc(6))
  memsav = int (voltbc(7))
  icat = int (voltbc(8))
  n1 = int (voltbc(9))
  n2 = int (voltbc(10))
4208 nenerg = n1
  if (iplot .eq. 0) iplot = 1
  if (m4plot .eq. 1 .and. iplot .eq. -1) iplot = 1
  if (iplot .eq. -1) isplot = intinf
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 4210) iout, iplot, idoubl, kssout, maxout, ipun, memsav, icat, n1, n2
4210 format ('+Misc. data.', 2i5, 8i3)
  begmax(1) = maxout
  maxout = 2
  if (n2 .eq. 0) go to 6519
  !  call copyi (n2, iprsov(1), ll30)
  call copy (n2, iprsov(1 :), ll30)
6519 iprsup = iprsov(1)
  if (icat .gt. 2) icat = 0
  if (n1 .eq. 0) go to 600
  !     read input card using cimage
6523 call cimage
  if (kolbeg .gt. 0) go to 623
  call intchk (ll1, ll24, ll8)
  call expchk (ll25, ll64, ll8)
  if (kill .gt. 0) go to 9200
  read (unit = abuff, fmt = 620, iostat = ios) isw, itest, idist, aincr, xmaxmx, degmin, degmax, d4, sigmax, jseedr
620 format (3i8, 6f8.0, i8)
  go to 624
623 nfrfld = 3
  !  call frefld (voltbc)
  call free (voltbc)
  isw = int (voltbc(1))
  itest = int (voltbc(2))
  idist = int (voltbc(3))
  nfrfld = 1
  !  call freone (aincr)
  call free (aincr)
  !  call freone (xmaxmx)
  call free (xmaxmx)
  !  call freone (degmin)
  call free (degmin)
  !  call freone (degmax)
  call free (degmax)
  !  call freone (d4)
  call free (d4)
  !  call freone (sigmax)
  call free (sigmax)
  !  call frefld (voltbc)
  call free (voltbc)
  jseedr = int (voltbc(1))
624 if (noutpr .eq.  0) write (unit = kunit6, fmt = 630, advance = 'no') isw, itest, idist, aincr
  !630 format ('+Statistics data.', 3i8, f9.4, $)
630 format ('+Statistics data.', 3i8, f9.4)
  if (xmaxmx .eq. 0.0d0) xmaxmx = 2.0d0
  if (aincr .eq. 0.0d0) aincr = unity / 20.0d0
  if (d4 .gt. 0.0d0) statfr = d4
  if (degmax .eq. 0.0d0) degmax = 360.0d0
  if (sigmax .eq. 0.0d0) sigmax = 4.0d0
  if (jseedr .gt. 0) jseedr = intinf
  if (kbase .ne. intinf) kbase = 1
  begmax(1) = 1.0d0
  if (nenerg .ne. intinf) go to 600
  nchain = 29
  go to 9800
600 if (d1 .eq. 0.0d0) go to 6260
  if (d1 .eq. xopt) go to 6260
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 6255) xopt, d1
6255 format (' ----- Warning. Nonzero misc. data  parameter "xopt" differs from the  power frequency of ', f8.2, ' . This is unusual.', /, 7x, 'A value of ', e13.4, ' was read from columns 17-24 of the data card just read. Execution will continue using', /, 7x, 'this value, as suspicious as it seems to the EMTP.')
6260 xopt = d1
  if (d2 .eq. 0.0d0) go to 6265
  if (d2 .eq. copt) go to 6265
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 6256) copt, d2
6256 format (' ----- Warning. Nonzero misc. data parameter "copt" differs from the power frequency of', f8.2, ' .   This is unusual.', /, 7x,  'A value of', e13.4, ' was read from columns 25-32 of the data card just read.   Execution will continue using', /, 7x, 'this value, as suspicious as it seems to the EMTP.')
6265 copt = d2
  if (d3 .gt. 0.0d0) epsiln = d3
  if (tolmat .le. 0.0d0) tolmat = epsiln
  tolmat = tolmat ** 2
  if (iplot .eq. 0) iplot = 1
  if (iplot .lt. 0) go to 6279
  if ((iplot / 2) * 2 .eq. iplot) write (unit = lunit(6), fmt = *) '    -----  Warning!  even  iplot =', iplot
6279 if (iout .eq. 0) iout = 1
  if (tmax .le. 0.0d0 .and. deltat .le. 0.0d0) deltat = 1.0d0
  if (deltat .gt. 0.0d0 .and. t .ge. 0.0d0) go to 4215
  kill = 2
  flstat(16) = deltat
  lstat(19) = 4215
  go to 9200
4215 d12 = tmax / deltat + 1.0d0
  d13 = intinf
  if (d12 .lt. d13) go to 4223
  kill = 81
  lstat(19) = 4215
  flstat(15) = d12
  flstat(16) = d13
  go to 9200
4223 ioutin = iout
  delta2 = deltat / 2.0d0
  dltinv = 1.0d0 / deltat
  tmax = tmax - delta2
  if (memsav .eq. 1) tmax = tmax + deltat
  i = 1
  if (ipun .lt. 0) go to 4303
  if (kprchg(1) .eq. -7777) go to 4213
  go to 4312
4303 ipun = 0
  ! read input card using cimage.
4308 call cimage
  if (kolbeg .gt. 0) go to 4217
  call intchk (ll1, ll80, ll8)
  if (kill .gt. 0) go to 9200
  read (unit = abuff, fmt = 4211) (kprchg(i), multpr(i), i = 1, 5)
4211 format (10i8)
  go to 4219
4217 nfrfld = 10
  !  call frefld (voltbc)
  call free (voltbc)
  j = 1
  do i = 1, 5
     kprchg(i) = int (voltbc(j))
     multpr(i) = int (voltbc(j + 1))
     j = j + 2
  end do
4219 if (noutpr .eq. 0) write (unit = kunit6, fmt = 14211) (kprchg(i), multpr(i), i = 1, 3)
14211 format ('+Printout :', 6i6)
  do i = 1, 5
     if(kprchg(i) .eq. 0) go to 4213
  end do
  i = 6
4213 kprchg(i) = intinf
  if (ktref .ne. -7777) go to 4312
  ktref = 0
  go to 15
4312 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4258) deltat, tmax, xopt, copt, epsiln, tolmat
4258 format (10x, 'deltat', 11x, 'tmax', 11x, 'xopt', 11x, 'copt', 9x, 'epsiln', 9x, 'tolmat', /, 1x, 6e15.5, /, 1x)
  if (iprsup .le. 0) go to 4266
  if (nenerg .eq. 0) go to 4266
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 4264)  isw, itest, idist, aincr, xmaxmx, degmin, degmax, statfr
4264 format (/, ' Statistics parameters.     iswitest   idist ', 10x, 'aincr', 9x, 'xmaxmx', 9x, 'degmin', 9x, 'degmax', 9x, 'statfr', /, 23x, 3i8, 5e15.5)
4266 if (ifdep .ne. -5555) go to 4269
  ifdep = 0
  go to 15
4269 xunits = 1000.
  if (xopt .gt. 0.0) xunits = twopi * xopt
  ntot=1
  maxbus = 0
  n23 = 0
  call namea6 (blank, n23)
  icheck = 1
  ibr = 0
  i_char = 0
  inonl = 0
  kswtch = 0
  numsub = 0
  npower = 0
  kswpe4 = 0
  nv=0
  numnvo = 0
  it = 1
  n1 = 0
  n12 = jflsos / 100
  n15 = jflsos - 100 * n12
  n13 = n15 / 10
  n14 = n15 - 10*n13
  lstat(14) = n12
  lstat(15) = n13
  lstat(16) = n14
  call midov1
  lstat(39) = 137
  !     read input card using cimage
2691 call cimage
  read (unit = abuff, fmt = 3245) (aupper(i), i = 1, 14)
3245 format (13a6, a2)
  if (to_lower (aupper(1)) .eq. text1) go to 2697
  if (to_lower (aupper(1)) .eq. text3) go to 2697
  if (to_lower (aupper(1)) .eq. text4) go to 2699
  if (to_lower (aupper(1)) .ne. text6) go to 7722
  write (unit = kunit6, fmt = 5389)
5389 format (' Begin tacs. ==========================')
  newtac = 1
  ntcsex = 1
  niunrs = 1
  write (unit = *,  fmt = *) ' Prepare to call  ntacs1  from over1.'
  call ntacs1
  write (unit = *, fmt = *) ' Back from ntacs1, back in over1.'
  go to 4284
7722 go to 4281
2697 ntcsex = 1
2699 n1 = 1
  if (noutpr .ne. 0) go to 2691
  if (ntcsex .eq. 0) go to 22699
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 32699)
32699 format ('+TACS hybrid setup.  TACS data cards follow.')
  read (unit = abuff, fmt = 1984) lstat(52)
1984 format (18x, i2)
  go to 2691
22699 if (noutpr .eq. 0) write (unit = kunit6, fmt = 42699)
42699 format ('+TACS stand-alone setup.  Data cards follow.')
  read (unit = abuff, fmt = 1984) lstat(52)
  go to 2691
4281 if (n1 .eq. 0) go to 4284
  !  call move (lstacs(1), lstat(61), ll8)
  call move (lstacs(1 :), lstat(61 :), ll8)
  ktab = 1
  call tacs1
  if (kill .gt. 0) go to 9200
  if (ntcsex + nstacs .gt. 0) go to 4276
  indstp = 1
  limstp = kprchg(1)
  call runtym (d1, d2)
  flstat(1) = flstat(1) + d1
  flstat(2) = flstat(2) + d2
  flstat(7) = flstat(7) - d1
  flstat(8) = flstat(8) - d2
  nchain = 12
  go to 9800
  !     read input card using cimage
4276 call cimage
4284 nchain = 2
  go to 9800
9200 nchain = 51
  lstat(18) = 1
9800 lastov = 1
  n5 = location (ida)
  n6 = location (ifkc)
  !     write (*,*) ' end over1.  n5, n6, ida, ifkc =',
  !     1                          n5, n6, ida, ifkc
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format (' "Exit  module over1."')
  return
end subroutine over1

!
! subroutine tacs1c.
!

subroutine tacs1c
  use blkcom
  use labcom
  use tacsar
  implicit none
  !     called only by over1 for start again usage
  !  include 'blkcom.ftn'
  !  include 'labcom.ftn'
  !  include 'tacsar.ftn'
  integer(4) :: i, ijk, ndy5, n, ndx1
  real(8) ::  dum1, dum2, dum3, pru, prx
  character(8) :: alnode
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module tacs1c."')
  read (unit = abuff, fmt = 187) n, alnode, dum1, dum3, dum2, ijk, prx, pru
187 format (i2, a6, 2x, 3e10.0, 14x, i6, 2e10.0)
  if (niu .lt. 12) go to 2868
  ndy5 = kud1 - 5
  do i = 12, niu
     ndy5 = ndy5 + 5
     ndx1 = ilntab(kaliu + i)
     if (texvec(ndx1) .eq. alnode) go to 6767
  end do
  write (unit = lunit(6), fmt = 1313)
1313 format (' $$$ No such sources, the card will be ignored $$$')
  go to 2868
6767 if (n .eq. 0) go to 6811
  iuty(kiuty + i) = n
6811 if (dum1 .ne. 0. .or. ijk .lt. 0) ud1(ndy5 + 1) = dum1
  if (dum2 .eq. 0. .and. ijk .gt. 0) go to 6822
  ud1(ndy5 + 2) = dum2
  if (iuty(kiuty + i) .eq. 14) ud1(ndy5 + 2) = dum2 * twopi / 360.
  if (iuty(kiuty + i) .ne. 23) go to 6822
  if (ud1(ndy5 + 2) .lt. deltat) ud1(ndy5 + 2) = deltat
6822 if (dum3 .ne. 0. .or. ijk .lt. 0) ud1(ndy5 + 3) = dum3
  if (prx .ne. 0. .or. ijk .lt. 0) ud1(ndy5 + 4) = prx
  if (pru .ne. 0.) ud1(ndy5 + 5) = pru
  if (noutpr .gt. 0) write (unit = lunit(6), fmt = 1515)
1515 format (' Another TACS source changing card')
2868 return
end subroutine tacs1c

!
! subroutine swmodf.
!

subroutine swmodf
  use blkcom
  use labcom
  use tracom
  use strcom
  implicit none
  !     called only by over1 for start again usage
  !  include 'blkcom.ftn'
  !  include 'labcom.ftn'
  integer(4) :: ijk, j, jdu, jk, k, m, msw
  real(8) :: a, gus3, gus4
  character(8) :: text14, text15
  !
  data text14 / 'ing   ' /
  data text15 / 'closed' /
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format (' Begin module "swmodf".')
  read (unit = abuff, fmt = 35) it2, bus1, bus2, gus3, gus4, ck1, a, jk, bus4, bus5, bus6, jdu, j
35 format (i2, 2a6, 4e10.0, i6, a4, 2a6, 2x, 2i1)
  do msw = 1, kswtch
     k = kmswit(msw)
     if (bus(k) .ne. bus1) go to 3535
     m = kmswit(msw + lswtch)
     if (bus(m) .eq. bus2) go to 3510
  end do
3535 continue
  write (unit = lunit(6), fmt = 3131)
3131 format (' No such switch, the card will be discarded')
  go to 209
3510 if (it2 .ne. 0 .or. kswtyp(msw) .ne. 0) go to 209
  if (to_lower (bus4) .ne. text14) go to 7218
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 1218)
1218 format ('+Permanently-closed switch used for metering.')
  gus3 = -1.0
  gus4 = fltinf
  go to 216
7218 if (noutpr .eq. 0) write (unit = kunit6, fmt = 36) gus3, gus4, ck1, a
36 format ('+Switch.', 2x, 4e10.2)
  if (a .eq. 0.0) go to 216
  if (gus4 .ne. 0.0 .or. ijk .lt. 0) adelay(msw) = absz(gus4)
  gus4 = absz (a)
  if (gus3 .ge. 0.) go to 216
  gus3 = 0.
  if(iprsup .ge. 1) write (unit = lunit(6), fmt = 217)
217 format (16x, 'tclose changed to zero')
216 if (gus3 .ne. 0.0 .or. ijk .lt. 0) tclose(msw) = gus3
  if (gus4 .ne. 0.0 .or. ijk .lt. 0) topen(msw) = gus4
  if (ck1 .ne. 0.0 .or. ijk .lt. 0) crit(msw) = ck1
209 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  Exit  module "swmodf."')
  return
end subroutine swmodf

module requests
  use blkcom
  use labcom
  use dekspy
  use umcom
  use movcop
  use freedom
  use strcom
  use linemodel
  implicit none

contains

  !
  ! subroutine xformer.
  !

  subroutine xformer
    ! $$$$$       special-request word no. 1.   'xformer'                         $$$$$
    implicit none
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 83056)
83056 format ('+Request for transformer impedance-matrix routine.')
    nchain = 41
    return
  end subroutine xformer

  !
  ! subroutine saturation.
  !

  subroutine saturation
    ! $$$$$       special-request word no. 2.   'saturation'                      $$$$$
    implicit none
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 83057)
83057 format ('+Request for magnetic-saturation routine.')
    nchain = 42
    return
  end subroutine saturation

  !
  ! subroutine type99.
  !

  subroutine type99
    ! $$$$$       special-request word no. 3.   'type99 limit'                    $$$$$
    implicit none
    real(8) :: d1
    !
    if (kolbeg .gt. 0) go to 3352
    read (unit = abuff, fmt = 2642) max99m
2642 format (32x, 6i8)
    go to 3354
3352 nfrfld = 1
    !        call freone (d1)
    call free (d1)
    max99m = int (d1, kind (max99m))
3354 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3355) max99m
3355 format ('+Redefine type-99 message limit to', i6)
    return
  end subroutine type99

  !
  ! subroutine replot.
  !

  subroutine replot
    ! $$$$$       special-request word no. 4.   'replot'                          $$$$$
    implicit none
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 3364)
3364 format ('+Request to re-plot old plot data.')
    degmax = 0.0
    ialter = lunit(4)
    call midov1
    call pfatch
    nchain = 31
    return
  end subroutine replot

  !
  ! subroutine bndc.
  !

  subroutine bndc
    ! $$$$$       special-request word no. 5.   'begin new data case'             $$$$$
    implicit none
    if (noutpr .eq. 0) then
       write (unit = kunit6, fmt = 4154)
4154   format ('+Marker card preceding new data case.')
    end if
    return
  end subroutine bndc

  !
  ! subroutine line.
  !

  subroutine line
    ! $$$$$       special-request word no. 6.   'line constants'                  $$$$$
    implicit none
    nchain = 44
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3411)
3411 format (' Set nchain = 44 for "line constants".')
    return
  end subroutine line

  !
  ! subroutine ppf.
  !

  subroutine ppf (farray)
    ! $$$$$       special-request word no. 7.   'postprocess plot file'           $$$$$
    implicit none
    real(8), intent (out) :: farray(:)
    integer(4) :: k, n1, n2, n3, n4, n5, n13
    real(8) :: d1
    !
    if (kolbeg .gt. 0) go to 3375
    read (unit = abuff, fmt = 2642) iofbnd
2642 format (32x, 6i8)
    go to 3379
3375 nfrfld = 1
    !        call freone (d1)
    call free (d1)
    iofbnd = int (d1)
3379 write (unit = kunit6, fmt = 3382) iofbnd
3382 format ('+Postprocess.  iplot =', i6)
    read (unit = lunit(2)) bus1, bus2, bus3, bus4, n1, n2, n3, n4, (bus5, k = 1, n1)
    n13 = n2 + n4
    if (iprsup .gt. 0) write (unit = lunit(6), fmt = 3383) bus1, bus2, n1, n2, n3, n4
3383 format (/, ' After 1st record.  date=', 2a4, 4i5)
    if (n2 .gt. 0) read (unit = lunit(2)) (n5, k = 1, n2)
    if (n4 .gt. 0) read (unit = lunit(2)) (n5, k = 1, n4)
    if (m4plot .eq. 0) go to 3385
    iofgnd = 1
    call pltlu2 (epsuba, farray(1))
    go to 3386
3385 read (unit = lunit(2)) epsuba, farray(1)
3386 ltdelt = -6789
    iofgnd = n13
    return
  end subroutine ppf

  !
  ! subroutine pph.
  !

  subroutine pph (ll25, ll80, ll8)
    ! $$$$$       special-request word no. 8.   'plotter paper height'            $$$$$
    implicit none
    integer(4), intent (out) :: ll25, ll80, ll8
    !
    if (kolbeg .gt. 0) go to 2608
    call expchk (ll25, ll80, ll8)
    if (kill .gt. 0) go to 9200
    read (unit = abuff, fmt = 2605) szplt
2605 format (32x, e8.0)
    go to 2612
2608 nfrfld = 1
    !        call freone (szplt)
    call free (szplt)
2612 if (noutpr .eq. 0) write (unit = kunit6, fmt = 2614) szplt
2614 format ('+New plotter paper-height limit.', 2x, e13.3)
9200 return
  end subroutine pph

  !
  ! subroutine pli.
  !

  subroutine pli (ll25, ll80, ll8)
    ! $$$$$    special-request word no. 9.   'printer lines per inch'             $$$$$
    implicit none
    integer(4), intent (out) :: ll25, ll80, ll8
    !
    if (kolbeg .gt. 0) go to 2628
    call intchk (ll25, ll80, ll8)
    if (kill .gt. 0) go to 9200
    read (unit = abuff, fmt = 2642) lnpin
2642 format (32x, 6i8)
    go to 2631
2628 nfrfld = 1
    !        call frefld (voltbc)
    call free (voltbc)
    lnpin = voltbc(1)
2631 if (noutpr .eq. 0 ) write (unit = kunit6, fmt = 2634) lnpin
2634 format ('+New printer spacing, lines/distance =', 2x, i8)
9200 return
  end subroutine pli

  !
  ! subroutine mvout.
  !

  subroutine mvout (ll25, ll80, ll8)
    ! $$$$$    special-request word no. 10.   'mode voltage output'               $$$$$
    implicit none
    integer(4), intent(out) :: ll25, ll80, ll8
    !
    if (kolbeg .gt. 0) go to 2644
    call intchk (ll25, ll80, ll8)
    if (kill .gt. 0) go to 9200
    read (unit = abuff, fmt = 2642) modout
2642 format (32x, 6i8)
    go to 2646
2644 nfrfld = 1
    !        call frefld (voltbc)
    call free (voltbc)
    modout = int (voltbc(1), kind (modout))
2646 if (modout .le. 0) modout = 3
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 2648) modout
2648 format ('+Request for tricky Karrenbauer output,', i3, '  modes.')
9200 return
  end subroutine mvout

  !
  ! subroutine lpo.
  !

  subroutine lpo
    !     $$$$$ special-request word no. 13.   'limit on plot oscillations'       $$$$$
    implicit none
    if (kolbeg .gt. 0) go to 2671
    read (unit = abuff, fmt = 2642) nsmth
2642 format (32x, 6i8)
    go to 2673
2671 nfrfld = 1
    !        call frefld (voltbc)
    call free (voltbc)
    nsmth = int (voltbc(1), kind (nsmth))
2673 if (noutpr .eq. 0) write (unit = kunit6, fmt = 2675) nsmth
2675 format ('+Change successive oscillation limit.', 2x, i8)
    return
  end subroutine lpo

  !
  ! subroutine diagnostic.
  !

  subroutine diagnostic
    ! $$$$$    special-request word no. 23.   'diagnostic'                        $$$$$
    implicit none
    integer(4) :: i, k
    !
    if (kolbeg .gt. 0) go to 2814
    read (unit = abuff, fmt = 2811) (iprsov(i), i = 1, 30)
2811 format (20x, 30i2)
    go to 2816
2814 nfrfld = 30
    !        call frefld (voltbc)
    call free (voltbc)
    if (kill .gt. 0) go to 9200
    do  i = 1, 30
       iprsov(i) = int (voltbc(i))
    end do
2816 iprsup = iprsov(1)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 2813) (iprsov(k), k = 1, 5)
2813 format ('+Diagnostic printout codes.', 5i4)
9200 return
  end subroutine diagnostic

  !
  ! subroutine adc.
  !

  subroutine adc (jpntr, textax)
    ! $$$$$    special-request word no. 28.   'abort data case'                   $$$$$
    implicit none
    character(8), intent(in) :: textax(:)
    integer(4), intent(in) :: jpntr(:)
    integer(4) :: i, k, l, n1, n2, n9
    real(8) :: d1
    !
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 4695)
4695 format ('+Request to abort this data case.')
    l = 0
    !     read input card using cimage
    do
       call cimage
       n9 = kolbeg
       kolbeg = 1
       nright = -2
       !        call freone (d1)
       call free (d1)
       n1 = jpntr(5)
       n2 = jpntr(6) - 1
       k = 0
       do i = n1, n2
          k = k + 1
          if (to_lower (texta6(k)) .ne. textax(i)) go to 4711
       end do
       exit
4711   l = l + 1
       if (noutpr .eq. 0) write (unit = kunit6, fmt = 4713) l
4713   format ('+Discarded card number', i5, '   of skipped case.')
    end do
  end subroutine adc

  !
  ! subroutine redefeps.
  !

  subroutine redefeps
    ! $$$$$    special request-word no. 39.   'redefine tolerance epsiln'         $$$$$
    implicit none
    if (kolbeg .gt. 0) go to 7217
    read (unit = abuff, fmt = 2605) epsiln
2605 format (32x, e8.0)
    go to 7223
7217 nfrfld = 1
    !        call freone (epsiln)
    call free (epsiln)
7223 write (unit = kunit6, fmt = 7226) epsiln
7226 format ("+Misc. data constant  'epsiln' .", e12.3)
    return
  end subroutine redefeps

  !
  ! subroutine begpeak.
  !

  subroutine begpeak
    ! $$$$$    special request-word no. 41.   'begin peak value search'           $$$$$
    implicit none
    integer(4) :: ip
    !
    if (kolbeg .gt. 0) go to 7247
    read (unit = abuff, fmt = 2605) begmax(2)
2605 format (32x, e8.0)
    go to 7249
7247 nfrfld = 1
    !        call frefld (begmax(2 :))
    call free (begmax(2 :))
7249 if (noutpr .eq. 0) write (unit = kunit6, fmt = 7252) begmax(2)
7252 format ('+Extrema calc. begins at', e13.4, '  seconds.')
    if (begmax(2) .ne. -1.) go to 9200
    !     read input data card using cimage
    call cimage
    read (unit = abuff, fmt = 7253) (begmax(ip), ip = 2, 6)
7253 format (10e8.0)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 7254) (begmax(ip), ip = 2, 5)
7254 format ('+(t1, t2):', 4e10.2)
9200 return
  end subroutine begpeak

  !
  ! subroutine relumdim
  !

  subroutine relumdim
    ! $$$$$  special request-word no. 46.   'relative u.m. dimensions'            $$$$$
    implicit none
    integer(4) :: j, k
    real(8) :: d1
    !
    if (kolbeg .gt. 0) go to 7298
    read (unit = abuff, fmt = 7276) (voltbc(k), k = 1, 4)
7276 format (32x, 6i8)
    go to 7303
7298 nfrfld = 4
    !        call frefld (voltbc)
    call free (voltbc)
7303 d1 = 0.0
    do j = 1, 4
       d1 = d1 + voltbc(j)
    end do
    if (d1 .gt. 0.0) go to 7310
    voltbc(1) = 42
    voltbc(2) = 23
    voltbc(3) = 10
    voltbc(4) = 10
    go to 7303
7310 d1 = lspcum * nbyte(3) / d1
    nclfix = int (voltbc(1) * d1 / (4 * nbyte(3) +  4 * nbyte(4)))
    numfix = int (voltbc(2) * d1 / (17 * nbyte(3) + 12 * nbyte(4)))
    iotfix = int (voltbc(3) * d1 / (2 * nbyte(4)))
    ibsfix = int (voltbc(4) * d1 / (1 * nbyte(1)))
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 7309) nclfix, numfix, iotfix, ibsfix
7309 format ('+Derived U.M. sizes.', 4i6)
    return
  end subroutine relumdim

  !
  ! subroutine timsl.
  !

  subroutine timsl
    ! $$$$$  special request-word no. 47.   'time step loop'                      $$$$$
    real(8) :: d1, d2, t
    !
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 7314)
7314 format ('+Transfer control to time-step loop.')
    nchain = 16
    t = flstat(14)
    !        call mover0 (flstat(3), 4)
    call move0 (flstat(3 :), 4)
    call runtym (d1, d2)
    flstat(1) = d1 + flstat(1)
    flstat(2) = d2 + flstat(2)
    flstat(7) = -d1
    flstat(8) = -d2
    return
  end subroutine timsl


  !
  ! subroutine altdp.
  !

  subroutine altdp
    ! $$$ special request-word no. 48.   'alternate diagnostic printout'          $$$$$
    implicit none
    integer(4) :: j
    !
    if (kolbeg .gt. 0) go to 7322
    read (unit = abuff, fmt = 2642) (iprsov(j + 30), j = 1, 4)
2642 format (32x, 6i8)
    go to 7329
7322 nfrfld = 4
    !        call frefld (voltbc)
    call free (voltbc)
    do j = 1, 4
       iprsov(j + 30) = int (voltbc(j))
    end do
7329 if (noutpr .eq. 0) write (unit = kunit6, fmt = 7331) (iprsov(j + 30), j = 1, 4)
7331 format ('+Deltat-loop printout.', 4i6)
    return
  end subroutine altdp

  !
  ! subroutine tacswl.
  !

  subroutine tacswl
    ! $$$$$  special request-word no. 49.   'tacs warn limit'                     $$$$$
    implicit none
    if (kolbeg .gt. 0) go to 7334
    read (unit = abuff, fmt = 2582) lstat(51), pu
2582 format (16x, i8, e8.0)
    go to 7335
7334 nfrfld = 2
    !        call frefld (voltbc)
    call free (voltbc)
    lstat(51) = int (voltbc(1))
    pu = voltbc(2)
7335 if (noutpr .eq. 0) write (unit = kunit6, fmt = 7336) lstat(51), pu
7336 format ('+Warning controls.  lim, t-beg =', i6, e10.2)
    return
  end subroutine tacswl

  !
  ! subroutine marti.
  !

  subroutine marti
    ! $$$$$    special-request word no. 50.   'marti setup'                       $$$$$
    ! $$$$$    special-request word no. 51.   'jmarti setup'                      $$$$$
    implicit none
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 7352)
7352 format ('+Request for new, fortified Marti setup.')
    nchain = 39
    return
  end subroutine marti

  !
  ! subroutine cuspf.
  !

  subroutine cuspf
    ! $$$$$  special request-word no. 52.   'custom plot file'                    $$$$$
    implicit none
    integer(4) :: n4
    !
    n4 = m4plot
    if (n4 .eq. 0) m4plot = 2
    if (n4 .eq. 2) m4plot = 0
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 7359) m4plot
7359 format ('+Non-std. choice of disk plot file.   m4plot =',  i2)
    return
  end subroutine cuspf

  !
  ! subroutine outwdth.
  !

  subroutine outwdth (nw)
    ! $$$$$  special request-word no. 53.   'output width 132'                    $$$$$
    ! $$$$$  special request-word no. 54.   'output width 80'                     $$$$$
    implicit none
    integer(4), intent (in) :: nw
    !
    select case (nw)
    case (80)
       if (noutpr .eq. 0) write (unit = kunit6, fmt = 7364)
7364   format ('+Use narrow (80-col.) printout.')
       kol132 = 80

    case (132)
       if (noutpr .eq. 0) write (unit = kunit6, fmt = 7368)
7368   format ('+Use full-width (132-col.) printout.')
       kol132 = 132

    case default
       write (unit = lunit(6), fmt = 7372) nw
7372   format ('+Invalid printout width ', i4, '.  Reverting to 80-col..')
       kol132 = 80
    end select
    return
  end subroutine outwdth

  !
  ! subroutine faultdu.
  !

  subroutine faultdu
    ! $$$$$  special request-word no. 56.   'fault data usage'                    $$$$$
    implicit none
    if (kolbeg .gt. 0) go to 7379
    read (unit = abuff, fmt = 2642) iofbnd
2642 format (32x, 6i8)
    go to 7382
7379 nfrfld = 1
    !        call frefld (voltbc)
    call free (voltbc)
    iofbnd = int (voltbc(1))
7382 if (noutpr .eq. 0) write (unit = kunit6, fmt = 7385)
7385 format ('+Request for generator equivalents.')
    istep = -6633
    nchain = 29
    return
  end subroutine faultdu

  !
  ! subroutine usersst.
  !

  subroutine usersst
    ! $$$$$  special request-word no. 58.   'user supplied switch times'          $$$$$
    implicit none
    integer(4) :: n14
    !
    if (kolbeg .gt. 0) go to 7396
    read (unit = abuff, fmt = 2642) n14
2642 format (32x, 6i8)
    go to 7402
7396 nfrfld = 1
    !        call frefld (voltbc)
    call free (voltbc)
    n14 = int (voltbc(1))
7402 moncar(7) = n14
    if (n14 .eq. 0) moncar(7) = 24
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 7406) moncar(7)
7406 format ('+File of random switching times.  unit =', i6)
    return
  end subroutine usersst

  !
  ! subroutine ametani.
  !

  subroutine ametani
    ! $$$$$  special request-word no. 59.   'ametani setup'                       $$$$$
    implicit none
    if (noutpr .eq. 0) write (unit = lunit(6), fmt = 7411)
7411 format ('+Request for Ametani step-response routine.')
    nchain = 46
    return
  end subroutine ametani

  !
  ! subroutine hauer.
  !

  subroutine hauer
    ! $$$$$  special request-word no. 60.   'hauer setup'                         $$$$$
    implicit none
    if (noutpr .eq. 0) write (unit = lunit(6), fmt = 7418)
7418 format ('+Request for Hauer impulse-response routine.')
    nchain = 48
    nenerg = 49
    return
  end subroutine hauer

  !
  ! subroutine linefs.
  !

  subroutine linefs (kbrnum)
    ! $$$$$  special request-word no. 61.   'line model freq. scan                $$$$$
    implicit none
    integer(4), intent (out) :: kbrnum
    !
    kexact = 88333
    nsolve = 0
    kbrnum = 0
    return
  end subroutine linefs

end module requests

!
! subroutine reques.
!

subroutine reques
  use systematic
  use linemodel
  use blkcom
  use umcom
  use dekspy
  use tracom
  use bcdtim
  use movcop
  use strcom
  use random
  use freedom
  use requests
  implicit none
  character(8) :: textax(300), textay(100)
  integer(4) :: i, ip, j, jpntr(100), k, kbrnum
  integer(4) :: l, ll1, ll8, ll16, ll25, ll32, ll33, ll40, ll48, ll49, ll56
  integer(4) :: ll80
  integer(4) :: m
  integer(4) :: n1, n2, n3, n4, n5, n7, n8, n9, n13, n14
  real(8) :: d1, d2, d7, d8, d13, deltfs, farray(1), fltin
  real(8) :: seed, statr
  real(8) :: znvref
  !
  !  dimension textax(300), jpntr(100), textay(100)
  !  dimension anglex(1), farray(1)
  !
  !  equivalence (anglex(1), angle)
  !  equivalence (moncar(2), kbase)
  !  equivalence (moncar(3), ltdelt)
  !  equivalence (iprsov(39), nmauto)
  !
  ! $$$$$       special-request word no. 1.   'xformer'                         $$$$$
  data textay(1)   / 'x     ' /
  data jpntr(1)    / 1 /
  data textax(1)   / 'xforme' /
  data textax(2)   / 'r     ' /
  ! $$$$$       special-request word no. 2.   'saturation'                      $$$$$
  data textay(2)   / 's     ' /
  data jpntr(2)    / 3 /
  data textax(3)   / 'satura' /
  data textax(4)   / 'tion  ' /
  ! $$$$$       special-request word no. 3.   'type99 limit'                    $$$$$
  data textay(3)   / 'tl    ' /
  data jpntr(3)    / 5 /
  data textax(5)   / 'type99' /
  data textax(6)   / 'limit ' /
  ! $$$$$       special-request word no. 4.   'replot'                          $$$$$
  data textay(4)   / 'r     ' /
  data jpntr(4)    / 7 /
  data textax(7)   / 'replot' /
  ! $$$$$       special-request word no. 5.   'begin new data case'             $$$$$
  data textay(5)   / 'bndc  ' /
  data jpntr(5)    / 8 /
  data textax(8)   / 'begin ' /
  data textax(9)   / 'new   ' /
  data textax(10)  / 'data  ' /
  data textax(11)  / 'case  ' /
  ! $$$$$       special-request word no. 6.   'line constants'                  $$$$$
  data textay(6)   / 'lc    ' /
  data jpntr(6)    / 12 /
  data textax(12)  / 'line  ' /
  data textax(13)  / 'consta' /
  data textax(14)  / 'nts   ' /
  ! $$$$$       special-request word no. 7.   'postprocess plot file'           $$$$$
  data textay(7)   / 'ppf   ' /
  data jpntr(7)    / 15 /
  data textax(15)  / 'postpr' /
  data textax(16)  / 'ocess ' /
  data textax(17)  / 'plot  ' /
  data textax(18)  / 'file  ' /
  ! $$$$$       special-request word no. 8.   'plotter paper height'            $$$$$
  data textay(8)   / 'pph   ' /
  data jpntr(8)    / 19 /
  data textax(19)  / 'plotte' /
  data textax(20)  / 'r     ' /
  data textax(21)  / 'paper ' /
  data textax(22)  / 'height' /
  ! $$$$$       special-request word no. 9.   'printer lines per inch'          $$$$$
  data textay(9)   / 'plpi  ' /
  data jpntr(9)    / 23 /
  data textax(23)  / 'printe' /
  data textax(24)  / 'r     ' /
  data textax(25)  / 'lines ' /
  data textax(26)  / 'per   ' /
  data textax(27)  / 'inch  ' /
  ! $$$$$    special-request word no. 10.   'mode voltage output'               $$$$$
  data textay(10)  / 'mvo   ' /
  data jpntr(10)   / 28 /
  data textax(28)  / 'mode  ' /
  data textax(29)  / 'voltag' /
  data textax(30)  / 'e     ' /
  data textax(31)  / 'output' /
  ! $$$$$    special-request word no. 11.   'end last data case'                $$$$$
  data textay(11)  / 'eldc  ' /
  data jpntr(11)   / 32 /
  data textax(32)  / 'end   ' /
  data textax(33)  / 'last  ' /
  data textax(34)  / 'data  ' /
  data textax(35)  / 'case  ' /
  ! $$$$$    special-request word no. 12.   'analytic sources usage'            $$$$$
  data textay(12)  / 'asu   ' /
  data jpntr(12)   / 36 /
  data textax(36)  / 'analyt' /
  data textax(37)  / 'ic    ' /
  data textax(38)  / 'source' /
  data textax(39)  / 's     ' /
  data textax(40)  / 'usage ' /
  ! $$$$$ special-request word no. 13.   'limit on plot oscillations'           $$$$$
  data textay(13)  / 'lopo  ' /
  data jpntr(13)   / 41 /
  data textax(41)  / 'limit ' /
  data textax(42)  / 'on    ' /
  data textax(43)  / 'plot  ' /
  data textax(44)  / 'oscill' /
  data textax(45)  / 'ations'  /
  ! $$$$$    special-request word no. 14.   'tacs emtp sources'                 $$$$$
  data textay(14)  / 'tes   ' /
  data jpntr(14)   / 46 /
  data textax(46)  / 'tacs  ' /
  data textax(47)  / 'emtp  ' /
  data textax(48)  / 'source' /
  data textax(49)  / 's     ' /
  ! $$$$$    special-request word no. 15.   'start again'                       $$$$$
  data textay(15)  / 'sa    ' /
  data jpntr(15)   / 50 /
  data textax(50)  / 'start ' /
  data textax(51)  / 'again ' /
  ! $$$$$    special-request word no. 16.   'semlyen setup'                     $$$$$
  data textay(16)  / 'ss    ' /
  data jpntr(16)   / 52 /
  data textax(52)  / 'semlye' /
  data textax(53)  / 'n     ' /
  data textax(54)  / 'setup ' /
  ! $$$$$     special request-word no. 17.   'linear bias usage'                $$$$$
  data textay(17)  / 'lbu   ' /
  data jpntr(17)   / 55 /
  data textax(55)  / 'linear' /
  data textax(56)  / 'bias  ' /
  data textax(57)  / 'usage ' /
  ! $$$$$     special-request word no. 18.   'cable constants'                  $$$$$
  data textay(18)  / 'cc    ' /
  data jpntr(18)   / 58 /
  data textax(58)  / 'cable ' /
  data textax(59)  / 'consta' /
  data textax(60)  / 'nts   ' /
  ! $$$$$    special-request word no. 19.   'auto name'                         $$$$$
  data textay(19)  / 'an    ' /
  data jpntr(19)   / 61 /
  data textax(61)  / 'auto  ' /
  data textax(62)  / 'name  ' /
  ! $$$$$    special-request word no. 20.   'renumber bypass'                   $$$$$
  data textay(20)  / 'rb    ' /
  data jpntr(20)   / 63 /
  data textax(63)  / 'renumb' /
  data textax(64)  / 'er    ' /
  data textax(65)  / 'bypass' /
  ! $$$$$    special-request word no. 21.   'frequency scan'                    $$$$$
  data textay(21)  / 'fs    ' /
  data jpntr(21)   / 66 /
  data textax(66)  / 'freque' /
  data textax(67)  / 'ncy   ' /
  data textax(68)  / 'scan  ' /
  ! $$$$$    special-request word no. 22.   'free format'                       $$$$$
  data textay(22)  / 'ff    ' /
  data jpntr(22)   / 69 /
  data textax(69)  / 'free  ' /
  data textax(70)  / 'format' /
  ! $$$$$    special-request word no. 23.   'diagnostic'                        $$$$$
  data textay(23)  / 'd     ' /
  data jpntr(23)   / 71 /
  data textax(71)  / 'diagno' /
  data textax(72)  / 'stic  ' /
  ! $$$$$    special-request word no. 24.   'power frequency'                   $$$$$
  data textay(24)  / 'pf    ' /
  data jpntr(24)   / 73 /
  data textax(73)  / 'power ' /
  data textax(74)  / 'freque' /
  data textax(75)  / 'ncy   ' /
  ! $$$$$    special-request word no. 25.   'file request'                      $$$$$
  data textay(25)  / 'fr    ' /
  data jpntr(25)   / 76 /
  data textax(76)  / 'file  ' /
  data textax(77)  / 'reques' /
  data textax(78)  / 't     ' /
  ! $$$$$    special-request word no. 26.   'user identification'               $$$$$
  data textay(26)  / 'ui    ' /
  data jpntr(26)   / 79 /
  data textax(79)  / 'user  ' /
  data textax(80)  / 'identi' /
  data textax(81)  / 'ficati' /
  data textax(82)  / 'on    ' /
  ! $$$$$    special-request word no. 27.   'convert zno'                       $$$$$
  data textay(27)  / 'cz    ' /
  data jpntr(27)   / 83 /
  data textax(83)  / 'conver' /
  data textax(84)  / 't     ' /
  data textax(85)  / 'zno   ' /
  ! $$$$$    special-request word no. 28.   'abort data case'                   $$$$$
  data textay(28)  / 'adc   ' /
  data jpntr(28)   / 86 /
  data textax(86)  / 'abort ' /
  data textax(87)  / 'data  ' /
  data textax(88)  / 'case  ' /
  ! $$$$$    special-request word no. 29.   'kill codes'                        $$$$$
  data textay(29)  / 'kc    ' /
  data jpntr(29)   / 89 /
  data textax(89)  / 'kill  ' /
  data textax(90)  / 'codes ' /
  ! $$$$$    special-request word no. 30.   'high resistance'                   $$$$$
  data textay(30)  / 'hr    ' /
  data jpntr(30)   / 91 /
  data textax(91)  / 'high  ' /
  data textax(92)  / 'resist' /
  data textax(93)  / 'ance  ' /
  ! $$$$$    special-request word no. 31.   'average output'                    $$$$$
  data textay(31)  / 'ao    ' /
  data jpntr(31)   / 94 /
  data textax(94)  / 'averag' /
  data textax(95)  / 'e     ' /
  data textax(96)  / 'output' /
  ! $$$$$    special request-word no. 32.  'absolute tacs dimensions'           $$$$$
  data textay(32)  / 'atd   ' /
  data jpntr(32)   / 97 /
  data textax(97)  / 'absolu' /
  data textax(98)  / 'te    ' /
  data textax(99)  / 'tacs  ' /
  data textax(100) / 'dimens' /
  data textax(101) / 'ions  ' /
  ! $$$$$    special request-word no. 33.  'relative tacs dimensions'           $$$$$
  data textay(33)  / 'rtd   ' /
  data jpntr(33)   / 102 /
  data textax(102) / 'relati' /
  data textax(103) / 've    ' /
  data textax(104) / 'tacs  ' /
  data textax(105) / 'dimens' /
  data textax(106) / 'ions  ' /
  ! $$$$$    special request-word no. 34.  'tabulate energization results'      $$$$$
  data textay(34)  / 'ter   ' /
  data jpntr(34)   / 107 /
  data textax(107) / 'tabula' /
  data textax(108) / 'te    ' /
  data textax(109) / 'energi' /
  data textax(110) / 'zation' /
  data textax(111) / 'result' /
  data textax(112) / 's     ' /
  ! $$$$$    special request-word no. 35. 'statistics output salvage'           $$$$$
  data textay(35)  / 'sos   ' /
  data jpntr(35)   / 113 /
  data textax(113) / 'statis' /
  data textax(114) / 'tics  ' /
  data textax(115) / 'output' /
  data textax(116) / 'salvag' /
  data textax(117) / 'e     ' /
  ! $$$$$    special request-word no. 36.   'omit base case'                    $$$$$
  data textay(36)  / 'obc   ' /
  data jpntr(36)   / 118 /
  data textax(118) / 'omit  ' /
  data textax(119) / 'base  ' /
  data textax(120) / 'case  ' /
  ! $$$$$    special request-word no. 37.   'change switch'                     $$$$$
  data textay(37)  / 'cs    ' /
  data jpntr(37)   / 121 /
  data textax(121) / 'change' /
  data textax(122) / 'switch' /
  ! $$$$$    special request-word no. 38.   'miscellaneous data cards'          $$$$$
  data textay(38)  / 'mdc   ' /
  data jpntr(38)   / 123 /
  data textax(123) / 'miscel' /
  data textax(124) / 'laneou' /
  data textax(125) / 's     ' /
  data textax(126) / 'data  ' /
  data textax(127) / 'cards ' /
  ! $$$$$    special request-word no. 39.   'redefine tolerance epsiln'         $$$$$
  data textay(39)  / 'rte   ' /
  data jpntr(39)   / 128 /
  data textax(128) / 'redefi' /
  data textax(129) / 'ne    ' /
  data textax(130) / 'tolera' /
  data textax(131) / 'nce   ' /
  data textax(132) / 'epsiln' /
  ! $$$$$    special request-word no. 40.   'change printout frequency          $$$$$
  data textay(40)  / 'cpf   ' /
  data jpntr(40)   / 133 /
  data textax(133) / 'change' /
  data textax(134) / 'printo' /
  data textax(135) / 'ut    ' /
  data textax(136) / 'freque' /
  data textax(137) / 'ncy   ' /
  ! $$$$$    special request-word no. 41.   'begin peak value search'           $$$$$
  data textay(41)  / 'bpvs  ' /
  data jpntr(41)   / 138 /
  data textax(138) / 'begin ' /
  data textax(139) / 'peak  ' /
  data textax(140) / 'value ' /
  data textax(141) / 'search' /
  ! $$$$$    special request-word no. 42.   'time of dice roll'                 $$$$$
  data textay(42)  / 'todr  ' /
  data jpntr(42)   / 142 /
  data textax(142) / 'time  ' /
  data textax(143) / 'of    ' /
  data textax(144) / 'dice  ' /
  data textax(145) / 'roll  ' /
  ! $$$$$    special request-word no. 43.   'zinc oxide'                        $$$$$
  data textay(43)  / 'zo    ' /
  data jpntr(43)   / 146 /
  data textax(146) / 'zinc  ' /
  data textax(147) / 'oxide ' /
  ! $$$$$    special request-word no. 44.   'peak voltage monitor'              $$$$$
  data textay(44)  / 'pvm   ' /
  data jpntr(44)   / 148 /
  data textax(148) / 'peak  ' /
  data textax(149) / 'voltag' /
  data textax(150) / 'e     ' /
  data textax(151) / 'monito' /
  data textax(152) / 'r     ' /
  ! $$$$$    special request-word no. 45.   'absolute u.m. dimensions'          $$$$$
  data textay(45)  / 'aumd  ' /
  data jpntr(45)   / 153 /
  data textax(153) / 'absolu' /
  data textax(154) / 'te    ' /
  data textax(155) / 'u.m.  ' /
  data textax(156) / 'dimens' /
  data textax(157) / 'ions  ' /
  ! $$$$$    special request-word no. 46.   'relative u.m. dimensions'          $$$$$
  data textay(46)  / 'rumd  ' /
  data jpntr(46)   / 158 /
  data textax(158) / 'relati' /
  data textax(159) / 've    ' /
  data textax(160) / 'u.m.  ' /
  data textax(161) / 'dimens' /
  data textax(162) / 'ions  ' /
  ! $$$$$    special request-word no. 47.   'time step loop'                    $$$$$
  data textay(47)  / 'tsl   ' /
  data jpntr(47)   / 163 /
  data textax(163) / 'time  ' /
  data textax(164) / 'step  ' /
  data textax(165) / 'loop  ' /
  ! $$$$$    special request-word no. 48.   'alternate diagnostic printout'     $$$$$
  data textay(48)  / 'adp   ' /
  data jpntr(48)   / 166 /
  data textax(166) / 'altern' /
  data textax(167) / 'ate   ' /
  data textax(168) / 'diagno' /
  data textax(169) / 'stic  ' /
  data textax(170) / 'printo' /
  data textax(171) / 'ut    ' /
  ! $$$$$    special request-word no. 49.   'tacs warn limit'                   $$$$$
  data textay(49)  / 'twl   ' /
  data jpntr(49)   / 172 /
  data textax(172) / 'tacs  ' /
  data textax(173) / 'warn  ' /
  data textax(174) / 'limit ' /
  ! $$$$$    special-request word no. 50.   'marti setup'                       $$$$$
  data textay(50)  / 'ms    ' /
  data jpntr(50)   / 175 /
  data textax(175) / 'marti ' /
  data textax(176) / 'setup ' /
  ! $$$$$    special-request word no. 51.   'jmarti setup'                      $$$$$
  data textay(51)  / 'jms   ' /
  data jpntr(51)   / 177 /
  data textax(177) / 'jmarti' /
  data textax(178) / 'setup ' /
  ! $$$$$    special request-word no. 52.   'custom plot file'                  $$$$$
  data textay(52)  / 'cpf   ' /
  data jpntr(52)   / 179 /
  data textax(179) / 'custom' /
  data textax(180) / 'plot  ' /
  data textax(181) / 'file  ' /
  ! $$$$$    special request-word no. 53.   'output width 132'                  $$$$$
  data textay(53)  / 'ow1   ' /
  data jpntr(53)   / 182 /
  data textax(182) / 'output' /
  data textax(183) / 'width ' /
  data textax(184) / '132   ' /
  ! $$$$$    special request-word no. 54.   'output width 80'                   $$$$$
  data textay(54)  / 'ow8   ' /
  data jpntr(54)   / 185 /
  data textax(185) / 'output' /
  data textax(186) / 'width ' /
  data textax(187) / '80    ' /
  ! $$$$$    special request-word no. 55.   'modify switch logic                $$$$$
  data textay(55)  / 'msl   ' /
  data jpntr(55)   / 188 /
  data textax(188) / 'modify' /
  data textax(189) / 'switch' /
  data textax(190) / 'logic ' /
  ! $$$$$    special request-word no. 56.   'fault data usage'                  $$$$$
  data textay(56)  / 'fdu   ' /
  data jpntr(56)   / 191 /
  data textax(191) / 'fault ' /
  data textax(192) / 'data  ' /
  data textax(193) / 'usage ' /
  ! $$$$$    special request-word no. 57.   'fix source'                        $$$$$
  data textay(57)  / 'fxs   ' /
  data jpntr(57)   / 194 /
  data textax(194) / 'fix   ' /
  data textax(195) / 'source' /
  ! $$$$$    special request-word no. 58.   'user supplied switch times'        $$$$$
  data textay(58)  / 'usst  ' /
  data jpntr(58)   / 196 /
  data textax(196) / 'user  ' /
  data textax(197) / 'suppli' /
  data textax(198) / 'ed    ' /
  data textax(199) / 'switch' /
  data textax(200) / 'times ' /
  ! $$$$$    special request-word no. 59.   'ametani setup'                     $$$$$
  data textay(59)  / 'as    ' /
  data jpntr(59)   / 201 /
  data textax(201) / 'ametan' /
  data textax(202) / 'i     ' /
  data textax(203) / 'setup ' /
  ! $$$$$    special request-word no. 60.   'hauer setup'                       $$$$$
  data textay(60)  / 'hs    ' /
  data jpntr(60)   / 204 /
  data textax(204) / 'hauer ' /
  data textax(205) / 'setup ' /
  ! $$$$$    special request-word no. 61.   'line model freq. scan              $$$$$
  data textay(61)  / 'lmfs  ' /
  data jpntr(61)   / 206 /
  data textax(206) / 'line  ' /
  data textax(207) / 'model ' /
  data textax(208) / 'freq  ' /
  data textax(209) / 'scan  ' /
  data jpntr(62) / 210 /
  data jpntr(63) / 0 /
  data ll1  / 1 /
  data ll8  / 8 /
  data ll16 / 16 /
  data ll25 / 25 /
  data ll32 / 32 /
  data ll33 / 33 /
  data ll40 / 40 /
  data ll48 / 48 /
  data ll49 / 49 /
  data ll56 / 56 /
  data ll80 / 80 /
  lstat(18) = 0
  if(iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module reques."')
  do i = 1, size(jpntr) - 1                       ! was 1 to 9999
     n1 = jpntr(i)
     n2 = jpntr(i + 1) - 1
     !     Next check if last request word has been exhaused.  If so,
     !     exit with  lstat(18)  equal to zero.
     if (n2 .lt. 0) go to 9200
     if (iprsup .ge. 35) write (unit = lunit(6), fmt = 3285) i, (textax(j), j = n1, n2)
3285 format (/, ' Special-request word', i4, ' .', 10a6)
     if (textax(n1) .eq. blank) go to 3306
     l = 0
     n3 = n2 - n1 + 1
     if (n3 .ne. nfrfld) go to 3306
     do j = n1, n2
        l = l + 1
        if (to_lower (texta6(l)) .ne. textax(j)) go to 3306
     end do
     !3294 lstat(18) = i
3294 lstat(18) = i
    select case (i)
     case (1)
        ! $$$$$    special-request word no. 1.   'xformer'                 $$$$$
        call xformer
        go to 5617

     case (2)
        ! $$$$$    special-request word no. 2.   'saturation'              $$$$$
        call saturation
        go to 5617

     case (3)
        ! $$$$$    special-request word no. 3.   'type99 limit'            $$$$$
        call type99
        go to 15

     case (4)
        ! $$$$$    special-request word no. 4.   'replot'                  $$$$$
        call replot
        go to 5617

     case (5)
        ! $$$$$    special-request word no. 5.   'begin new data case'     $$$$$
        call bndc
        go to 15

     case (6)
        ! $$$$$    special-request word no. 6.   'line constants'          $$$$$
        call line
        go to 5617

     case (7)
        ! $$$$$    special-request word no. 7.   'postprocess plot file'   $$$$$
        call ppf (farray)
        go to 15

     case (8)
        ! $$$$$    special-request word no. 8.   'plotter paper height'    $$$$$
        call pph (ll25, ll80, ll8)
        if (kill .gt. 0) go to 9200
        go to 15

     case (9)
        ! $$$$$    special-request word no. 9.   'printer lines per inch'  $$$$$
        call pli (ll25, ll80, ll8)
        if (kill .gt. 0) go to 9200
        go to 15

     case (10)
        ! $$$$$    special-request word no. 10.   'mode voltage output'    $$$$$
        call mvout (ll25, ll80, ll8)
        if (kill .gt. 0) go to 9200
        go to 15

     case (11)
        ! $$$$$    special-request word no. 11.   'end last data case'     $$$$$
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 2654)
2654    format ('+Special terminator record for all data.')
        go to 15

     case (12)
        ! $$$$$    special-request word no. 12.   'analytic sources usage' $$$$$
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 2667)
2667    format ('+Request for use of analytic type 1-10 sources.')
        kanal = 1
        go to 15

     case (13)
        !     $$$$$ special-request word no. 13.   'limit on plot oscillations'
        call lpo
        go to 15

     case (14)
        ! $$$$$    special-request word no. 14.   'tacs emtp sources'      $$$$$
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 2682)
2682    format ('+TACS names controlling type 1-10 EMTP sources.')
        if (kolbeg .gt. 0) go to 2683
        read (unit = abuff, fmt = 2685) (vstacs(j), j = 1, 10)
2685    format (20x, 10a6)
        go to 2686
2683    nright = -2
        !        call freone (d1)
        call free (d1)
        do j = 1, 10
           vstacs(j) = texta6(j)
        end do
2686    nstacs = 10
2687    if (vstacs(nstacs) .ne. blank) go to 2690
        nstacs = nstacs - 1
        if (nstacs .gt. 0) go to 2687
2690    go to 15

     case (15)
        ! $$$$$    special-request word no. 15.   'start again'            $$$$$
        !          this request is handled outside of subroutine.
        go to 9200

     case (16)
        ! $$$$$    special-request word no. 16.   'semlyen setup'          $$$$$
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 2705)
2705    format ('+Request for Semlyen step-response routine.')
        nchain = 45
        go to 5617

     case (17)
        ! $$$$$  special request-word no. 17.   'linear bias usage'        $$$$$
        if (noutpr .eq. 0) write (unit = lunit(6), fmt = 8719)
8719    format ('+Ramped linear variation of random bias.')
        linsys = 1
        go to 15

     case (18)
        ! $$$$$    special-request word no. 18.   'cable constants'        $$$$$
        read (unit = abuff, fmt = 2732) d13, ktrlsw(3)
2732    format (48x, e8.0, i8)
        if (d13 .gt. 0.0) znvref = d13
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 2726) ktrlsw(3)
2726    format ('+Transfer to "cable constants".  type =', i6)
        nchain = 47
        go to 5617

     case (19)
        ! $$$$$    special-request word no. 19.   'auto name'              $$$$$
        nmauto = nmauto + 1
        if (nmauto .ge. 2) nmauto = 0
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 2743) nmauto
2743    format ('+Toggle branch naming option.   nmauto =', i8)
        go to 15

     case (20)
        ! $$$$$    special-request word no. 20.   'renumber bypass'        $$$$$
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 2758)
2758    format ('+Bypass of transient network renumbering.')
     ! negative "kpartb" (1000 or exponent of "high resistance")
     ! is flag carried into "over6" to bypass overlay 7:
        kpartb = -iabs (kpartb)
        go to 15

     case (21)
        ! $$$$$    special-request word no. 21.   'frequency scan'         $$$$$
        go to 8021

     case (22)
        ! $$$$$    special-request word no. 22.   'free format'            $$$$$
        read (unit = abuff, fmt = 2796) bus4, bus5
2796    format (16x, a1, 7x, a1)
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 2801) bus4, bus5
2801    format ('+Free-field characters.   ', a1, '   and   ', a1, ' .')
        if (bus4 .ne. blank) csepar = bus4
        if (bus5 .ne. blank) chcont = bus5
        go to 15

     case (23)
        ! $$$$$    special-request word no. 23.   'diagnostic'             $$$$$
        call diagnostic
        if (kill .gt. 0) go to 9200
        go to 15

     case (24)
        ! $$$$$    special-request word no. 24.   'power frequency'        $$$$$
        if (kolbeg .gt. 0) go to 2820
        read (unit = abuff, fmt = 2605) statfr
2605    format (32x, e8.0)
        go to 2822
2820    nfrfld = 1
        !        call freone (statfr)
        call free (statfr)
2822    if (noutpr .eq. 0) write (unit = kunit6, fmt = 2824) statfr
2824    format ('+Redefined power frequency =', e12.3, '  Hz.')
        go to 15

     case (25)
        ! $$$$$    special-request word no. 25.   'file request'           $$$$$
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 4654)
4654    format ('+Call subroutine "midov1" .')
        call midov1
        go to 15

     case (26)
          ! $$$$$    special-request word no. 26.   'user identification'  $$$$$
        if (kolbeg .gt. 0) go to 4661
        read (unit = abuff, fmt = 4658) userid
4658    format (24x, 8a6)
        go to 4664
4661    nright = -2
        !        call freone (d1)
        call free (d1)
        nright = 0
        userid = texta6(1)
4664    if (noutpr .eq. 0) write (unit = kunit6, fmt = 4667) userid
4667    format ('+User identification.', 1x, 4a6)
        go to 15

     case (27)
        ! $$$$$    special-request word no. 27.   'convert zno'            $$$$$
        iofbnd = 99876
        nchain = 42
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 4685)
4685    format ('+Request to convert old zno data to new formats.')
        go to 5617

     case (28)
        ! $$$$$    special-request word no. 28.   'abort data case'        $$$$$
        call adc (jpntr, textax)
        go to 15

     case (29)
        ! $$$$$    special-request word no. 29.   'kill codes'             $$$$$
        ipntv(1) = -8888
        if (kolbeg .gt. 0) go to 4724
        read (unit = abuff, fmt = 4721) kill, ipntv(2)
4721    format (32x, 2i8)
        go to 4726
4724    nfrfld = 2
        !        call frefld (voltbc)
        call free (voltbc)
        kill = int (voltbc(1))
        ipntv(2) = int (voltbc(2))
4726    write (unit = kunit6, fmt = 4728) kill, ipntv(2)
4728    format ('+Listing of error messages.', 2i8)
        ipntv(3) = kill
        write (unit = lunit(6), fmt = 4733) kill
4733    format (/, ' Message of kill-code number', i4, '.')
        bus1 = trash
        bus2 = trash
        bus3 = trash
        bus4 = trash
        bus5 = trash
        bus6 = trash
        do j = 11, 17
           lstat(j) = 0
           flstat(j) = 0.0
        end do
        nchain = 51
        go to 9200

     case (30)
        ! $$$$$    special-request word no. 30.   'high resistance'        $$$$$
        n7 = kpartb
        if (kolbeg .gt. 0) go to 4735
        read (unit = abuff, fmt = 4721) kpartb
        call intchk (ll33, ll40, ll8)
        if (kill .gt. 0) go to 9200
        go to 4737
4735    nfrfld = 1
        !        call frefld (voltbc)
        call free (voltbc)
        kpartb = int (voltbc(1))
4737    d1 = 10.0 ** kpartb
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 4739) d1
4739    format ('+Exponent of high resistance.   R =', e12.2)
        !     possible negative sign on previous "kpartb" must be
        !     retained as flag for "renumber bypass" usage:
        if (n7 .lt. 0) kpartb = -kpartb
        go to 15

     case (31)
        ! $$$$$    special-request word no. 31.   'average output'         $$$$$
        iaverg = 1
        nsmth = intinf
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 7010)
7010    format ('+Request to average (smooth) output.')
        go to 15

     case (32)
        ! $$$$$  special request-word no. 32.  'absolute tacs dimensions'  $$$$$
        !        this request is handled outside of subroutine.
        go to 9200

     case (33)
        ! $$$$$  special request-word no. 33.  'relative tacs dimensions'  $$$$$
        !        this request is handled outside of subroutine.
        go to 9200

     case (34)
        ! $$$ special request-word no. 34.  'tabulate energization results' $$$$
        write (unit = kunit6, fmt = 7183)
7183    format ("+Request for  'statistics'  termination.")
        nenerg = intinf
        go to 15

     case (35)
        ! $$$$$ special request-word no. 35.  'statistics output salvage'  $$$$$
        if (kolbeg .gt. 0) go to 7193
        read (unit = abuff, fmt = 7191) jflsos
7191    format (29x, i3)
        go to 7195
7193    nfrfld = 1
        !        call freone (d1)
        call free (d1)
        jflsos = int (d1)
7195    if (jflsos .gt. 0) go to 7199
        !     find random integer  'jflsos'  between zero and 999.
        call runtym (d1, d2)
        seed = seedy (tclock(1)) + 1000.0 * (d1 + d2)
        n13 = int (alog1z (seed) + epsiln)
        n13 = n13 - 2
        seed = seed / 10.0 ** n13
        jflsos = int (seed)
7199    if (noutpr .eq. 0) write (unit = kunit6, fmt = 7200) jflsos
7200    format ('+Disk storage of energization results.', i8)
        go to 15

     case (36)
        ! $$$$$  special request-word no. 36.   'omit base case'           $$$$$
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 7203)
7203    format ('+Omit base case if statistics/systematic case.')
        kbase = intinf
        go to 15

     case (37)
        ! $$$$$  special request-word no. 37.   'change switch'            $$$$$
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 7206)
7206    format ('+Convert switched-L,R to pseudo-nonlinear.')
        iofbnd = 33666
        go to 15

     case (38)
        ! $$$$$  special request-word no. 38.   'miscellaneous data cards' $$$$$
        ifdep = -5555
        write (unit = kunit6, fmt = 7212)
7212    format ('+Request record before misc. data cards.')
        go to 15

     case (39)
        ! $$$$$ special request-word no. 39.   'redefine tolerance epsiln' $$$$$
        call redefeps
        go to 15

     case (40)
        ! $$$$$ special request-word no. 40.   'change printout frequency' $$$$$
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 7234)
7234    format ('+Request record before printout frequencies.')
        ktref = -7777
        go to 15

     case (41)
        ! $$$$$  special request-word no. 41.   'begin peak value search'  $$$$$
        call begpeak
        go to 15

     case (42)
        ! $$$$$  special request-word no. 42.   'time of dice roll'        $$$$$
        if (kolbeg .gt. 0) go to 7255
        read (unit = abuff, fmt = 2605) tenerg
        go to 7261
7255    nfrfld = 1
        !        call freone (tenerg)
        call free (tenerg)
7261    if (noutpr .eq. 0) write (unit = kunit6, fmt = 7262) tenerg
7262    format ('+Statistics table-saving time =', e12.3, '  sec. ')
        go to 15

     case (43)
          ! $$$$$  special request-word no. 43.   'zinc oxide'             $$$$$
        if (kolbeg .gt. 0) go to 7266
        read (unit = abuff, fmt = 7264) n13, (flstat(m), m = 15, 19)
7264    format (16x, i8, 5e8.0)
        go to 7268
7266    nfrfld = 6
        !        call frefld (flstat(14 :))
        call free (flstat(14 :))
        n13 = int (flstat(14))
7268    if (n13 .gt. 0)  maxzno = n13
        if (flstat(15) .gt. 0.0) epszno = flstat(15)
        if (flstat(16) .gt. 0.0) epwarn = flstat(16)
        if (flstat(17) .gt. 0.0) epstop = flstat(17)
        if (flstat(18) .gt. 0.0) znolim(1) = flstat(18)
        if (flstat(19) .gt. 0.0) znolim(2) = flstat(19)
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 7270) maxzno, epszno, epwarn, epstop
7270    format ('+ZnO const.', i4, 3e11.3)
        go to 15

     case (44)
        ! $$$$$  special request-word no. 44.   'peak voltage' monitor'    $$$$$
        peaknd(1) = flzero
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 7273)
7273    format ('+Overall problem peak node voltage.')
        go to 15

     case (45)
        ! $$$$$  special request-word no. 45.   'absolute u.m. dimensions' $$$$$
        if (kolbeg .gt. 0) go to 7279
        read (unit = abuff, fmt = 7276) nclfix, numfix, iotfix, ibsfix
7276    format (32x, 6i8)
        go to 7282
7279    nfrfld = 4
        !        call frefld (voltbc)
        call free (voltbc)
        nclfix = int (voltbc(1))
        numfix = int (voltbc(2))
        iotfix = int (voltbc(3))
        ibsfix = int (voltbc(4))
7282    if (nclfix .gt. 0) go to 7284
        nclfix = 20
        numfix = 3
        iotfix = 50
        ibsfix = 60
7284    if (noutpr .eq. 0) write (unit = kunit6, fmt = 7287) nclfix, numfix, iotfix, ibsfix
7287    format ('+U.M. table sizes.', 4i6)
        go to 15

     case (46)
        ! $$$$$  special request-word no. 46.   'relative u.m. dimensions' $$$$$
        call relumdim
        go to 15

     case (47)
        ! $$$$$  special request-word no. 47.   'time step loop'           $$$$$
        call timsl
        go to 15

     case (48)
        ! $$$ special request-word no. 48.   'alternate diagnostic printout' $$$
        call altdp
        go to 15

     case (49)
        ! $$$$$  special request-word no. 49.   'tacs warn limit'          $$$$$
        call tacswl
        go to 15

     case (50, 51)
        ! $$$$$    special-request word no. 50.   'marti setup'            $$$$$
        ! $$$$$    special-request word no. 51.   'jmarti setup'           $$$$$
        call marti
        go to 5617

     case (52)
        ! $$$$$  special request-word no. 52.   'custom plot file'         $$$$$
        call cuspf
        go to 15

     case (53)
        ! $$$$$  special request-word no. 53.   'output width 132'         $$$$$
        call outwdth (132)
        go to 15

     case (54)
        ! $$$$$  special request-word no. 54.   'output width 80'          $$$$$
        call outwdth (80)
        go to 15

     case (55)
        ! $$$$$  special request-word no. 55.   'modify switch logic       $$$$$
        n7 = ktrlsw(6) + 1
        if (n7 .ge. 2) n7 = 0
        ktrlsw(6) = n7
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 7374) n7
7374    format ('+Request for altered logic.  ktrlsw(6) =', i2)
        go to 15

     case (56)
        ! $$$$$  special request-word no. 56.   'fault data usage'         $$$$$
        call faultdu
        go to 5617

     case (57)
        ! $$$$$  special request-word no. 57.   'fix source'               $$$$$
        istep = -4567
        if (noutpr .eq. 0) write (unit = kunit6, fmt = 7388)
7388    format ('+Declaration of desired load flow use.')
        go to 15

     case (58)
        ! $$$$$  special request-word no. 58.   'user supplied             $$$$$
        ! $$$$$  switch times'                                             $$$$$
        call usersst
        go to 15

     case (59)
        ! $$$$$  special request-word no. 59.   'ametani setup'            $$$$$
        call ametani
        go to 5617

     case (60)
        ! $$$$$  special request-word no. 60.   'hauer setup'              $$$$$
        call hauer
        go to 5617

     case (61)
        ! $$$$$  special request-word no. 61.   'line model freq. scan     $$$$$
        call linefs (kbrnum)
        go to 8021

     case (62)
        goto 5617

     end select

3306 if (to_lower (texta6(1)) .eq. textay(i)) go to 3294
  end do
  !     control will never reach  "stop"  which follows.
  call stoptp
  !     these two requests handled outside of subroutine.
  !     $$$$$$$  additional key words go in slots below:
8021 if (kolbeg .gt. 0) go to 2773
  call expchk (ll25, ll48, ll8)
  call intchk (ll49, ll56, ll8)
  if (kill .gt. 0) go to 9200
  read (unit = abuff, fmt = 2779) fminfs, deltfs, fmaxfs, n8
2779 format (24x, 3e8.0, i8)
  go to 2776
2773 nfrfld = 4
  !  call frefld (voltbc)
  call free (voltbc)
  fminfs = voltbc(1)
  delffs = voltbc(2)
  fmaxfs = voltbc(3)
  n8 = int (voltbc(4))
2776 if (noutpr .ne. 0) go to 2781
  if (kexact .eq. 88333) go to 2778
  write (unit = kunit6, fmt = 2780) fminfs, delffs, fmaxfs, n8
2780 format ('+F-scan.', 3e12.3, i5)
  go to 2781
2778 write (unit = kunit6, fmt = 3779) fminfs, fmaxfs, n8
3779 format ('Line model freq scan.', 2e12.3, i3)
  fminsv = fminfs
2781 if (fminfs .le. 0.0) go to 2785
  if (fmaxfs .le. fminfs) go to 2785
  if (n8 .ne. 0) go to 2782
  if (delffs .le. 0.0) go to 2785
  go to 15
2782 if (n8 .lt. 0) go to 2785
  d7 = 10.
  d8 = alogz (d7) / n8
  delffs = -expz (d8)
  go to 15
2785 kill = 193
  !  lstat(14) = n8
  lstat(14) = n8
  !  lstat(19) = 2785
  lstat(19) = 2785
  go to 9200
5617 flstat(7) = -9999.0
  ck1 = -fltinf
  ci1 = -fltin
15 continue
9200 if (iprsup .ge. 5) write (unit = lunit(6), fmt = 9201) nchain, lunit(6), lstat(18), kill
9201 format (/, ' Exit  "reques" .  nchain  lunit(6) lstat(18)   kill ', /, 17x, 4i8)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module reques."')
  return
end subroutine reques

!
! subroutine strip.
!

subroutine strip (s, c)
  implicit none
  character(*), intent(out) :: s
  character, intent(in) :: c
  integer(4) :: i, j
  character, pointer :: temp(:)
  !
  allocate (temp(len (s)))
  if (associated (temp)) then
     j = 1
     do i = 1, len (s)
        if (s(i : i) .ne. c) then
           temp(j : j) = s(i : i)
           j = j + 1
        end if
     end do
     do i = 1, j - 1
        s(i : i) = temp(i)
     end do
     deallocate (temp)
     nullify (temp)
  end if
end subroutine strip

!
! subroutine sysdep.
!

subroutine sysdep
  use komthl
  use blkcom
  use dekspy
  use bcdtim
  use bcddat
  implicit none
  !  dimension intbus(1)
  !  equivalence (intbus(1), bus1)
  logical(1) :: od
  character(25) :: col
  character(18) :: colxxx
  character(8) :: busnm1, busnm2, busnm3, text1, text2, temp
  character :: lettra, lettrb, lettrc !, colxxx(18)
  integer(4) :: n7
  !
  !     first 5 characters of file name "col" are reserved
  !     for explicit directory (e.g., "[plt]" ), if desired.
  data col    / '' /
  data colxxx / '_plt_000000000.pl4' /
  data lettra / 'a' /
  data lettrb / 'b' /
  data lettrc / 'c' /
  data busnm1 / '      ' /
  data busnm2 / '........' /
  data busnm3 / 'terra ' /
  data text1  / ',' /
  data text2  / '$' /
  col(1 : 18) = colxxx(1 : 18)
  lunit(2) = 19
  lunit(4) = 20
  call sysplt (lunit(4))                            ! define l4plot=lunit(4) for "dekplt"
  luntsp = 14                                     ! the catalog command of "spying" uses this
  mflush = 0
  if (m4plot .eq. 1) go to 1355
  if (llbuff .eq. -3333) go to 1342
  if (nenerg .eq. 49) close (unit = lunit(1), status = 'keep')
  if (nenerg .ne. 49) close (unit = lunit(1), status = 'delete')
  close (unit = lunit(2))
  close (unit = lunit(3))
  if (icat .eq. 0) go to 120
  if (icat .gt. 2) go to 120
  close (unit = lunit(4), status = 'keep')
  go to 140
120 close (unit = lunit(4), status = 'delete')
140 continue
  close (unit = lunit(9))
  close (unit = lunit(10))
  close (unit = lunit(11))
  close (unit = lunit(12))
  close (unit = lunit(13))
  close (unit = lunit(14))
  close (unit = lunit(15))
  !     1342 open (unit=lunit(1), type='new', form='formatted')
  !     open (unit=lunit(2), type='scratch', form='formatted', status='delete' )
  !     open ( unit=lunit(3), type='scratch', form='unformatted', status='delete' )
  !     unit 8 is calcomp plot file (unused with our vax-versate connection )
1342 open (unit = lunit(1), form = 'formatted')
  open (unit = lunit(2), status = 'scratch', form = 'formatted')
  open (unit = lunit(3), status = 'scratch', form = 'unformatted')
  open (unit = lunit(10), status = 'scratch', form = 'unformatted')
  open (unit = lunit(11), status = 'scratch', form = 'unformatted')
  open (unit = lunit(12), status = 'scratch', form = 'unformatted')
  open (unit = lunit(13), status = 'scratch', form = 'unformatted')
  open (unit = lunit(14), status = 'scratch', form = 'unformatted')
  open (unit = lunit(15), status = 'scratch', form = 'unformatted')
1355 if (llbuff .ne. -3333)  go to 1359
  call settym
  iprsov(38) = 0
  llbuff = 1025
  if (ltacst .ge. 3000) go to 1359
  !     VAX link-edits according to longest common block, not
  !     according to 1st appearance (size of vardim output):
  ltlabl = ltlabl + 2 * (3000 - ltacst)
  ltacst = 3000
1359 statfr = 60.
  kburro = 1
  pekexp = 43
  szplt = 10.
  szbed = 72.
  epsuba = 1.d-16
  epomeg = 1.d-15
  epdgel = 1.d-16
  iprsov(37) = 10
  lnpin = 6
  nsmth = 50
  call date44 (date1)
  call time44 (tclock)
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 6301)
6301 format (' Electromagnetic Transients Program (EMTP) Digital (DEC) VAX-11/780 translation as used by BPA in Portland, Oregon  97208;  USA.')
  read (unit = date1(1), fmt = 3641) col(25 : 25), col(6 : 6), col(7 : 7)
3641 format (2a1, 1x, a1)
  if (col(25 : 25) .eq. '0') go to 3642
  if (col(6 : 6) .eq. '0') col(6 : 6) = lettra
  if (col(6 : 6) .eq. '1') col(6 : 6) = lettrb
  if (col(6 : 6) .eq. '2') col(6 : 6) = lettrc
3642 continue
  read (unit = date1(2), fmt = 3641) col(8 : 8)
  !  read (unit = tclock, fmt = 3652) col(9 : 14)
  !3652 format (2a1, 1x, a1, 4x, a1, 1x, 2a1)
  !call strip(col, ' ')
  temp = tclock(1)(1 : 4) // tclock(2)(1 : 4)
  call strip (temp, '.')
  col(9 : 14) = temp
  write (unit = ansi32, fmt = 3668) col(1 : 18)
3668 format (a18)
  call strip (ansi32, ' ')
  if (iprsup .ge. 1 ) write (unit = lunit(6), fmt = 3672) ansi32
3672 format (/,  ' in  #sysdep# ,   ansi32 =',  a32)
  open (unit = lunit(4), action = 'readwrite', status = 'replace', file = ansi32, form = 'unformatted')
  inquire (unit = lunit(4), opened = od)
  if (od .neqv. .true.) then
     write (unit = lunit(6), fmt = "('Could not open ', a25, ' .  Exiting.')") ansi32
     call stoptp
  end if
  blank = busnm1
  trash = busnm2
  terra = busnm3
  csepar = text1
  chcont = text2
  tenm3 = 1.d-3
  unity = 1.d0
  onehaf = 0.5d0
  intinf = 10000000
  nbyte(1) = 2
  nbyte(2) = 2
  !     znvref = 1.e-6  ! 32-bit "complex" in "cable constants"
  !     following zero definition is redundant.
  ktrlsw(6) = 0                                             ! use "= 1" if simple logic is desired
  nbyte(3) = 2
  fltinf = 1.0d+20
  flzero = 1.0d-12
  epsiln = 1.0d-8
  twopi = 6.28318530717958647692d+00
  userid = blank
  if (noutpr .eq. 0) then
     write (unit = lunit(6), fmt = 6305) date1, tclock, col(6 : 18)
6305 format (' Date (mm/dd/yy) and time of day (hh.mm.ss.) =', 1x, 2a4, 2x, 2a4, 11x, 'name of VAX/VMS plot data file (if any) = ', a13)
  end if
  ! if not interactive emtp usage,
  if (m4plot .ne. 1) m4plot = 2                   ! use "pltfil" for real*4 plot file on d
  lunit(5) = -5                                   ! interactive or not uses "nextcard", no
  return

  entry nextcard
  !     This entry is used only for interactive EMTP.  it gets
  !     next card image from memory rather than unit 5.
  n7 = numdcd + 1                                 ! next data card is right after last
1472 if (iprspy .lt. 5) go to 1486                ! jump around diagnostic
  write (unit = munit6, fmt = 1477) n7, file6(n7)
1477 format (' in "nextcard":', i5, 1x, a80)
  call window                                     ! output of character variable munit6
1486 if (n7 .le. numcrd) go to 1488               ! at least 1 card remains
  write (unit = lunit(6), fmt = 1483) numcrd
1483 format ('   ****  ****   Data crisis.   Last card has been read.   numcrd =', i6 ,/, 'Use "data" command of spy to read in next block of data.')
  if (m4plot .ne. 1)  go to 9000                  ! set kill, then exit
  call emtspy                                     ! allow user to change data card storage
  go to 1472                                      ! loop back for another try at reading
  !1488 read (unit = file6(n7), fmt = 1489) (abuff(j), j = 1, 10)
  !1489 format (10a8)
1488 read (unit = file6(n7), fmt = 1489) abuff
1489 format (a80)
  go to 9200                                      ! exit module with new card image in abuff
9000 kill = 7654                                  ! positive kill is eof flag in "cimage"
9200 return
end subroutine sysdep

!
! subroutine midov1.
!

subroutine midov1
  use blkcom
  use dekspy
  implicit none
  !     module called only from one location in "over1" of overlay
  !     one.  it should be acceptable to all fortran 77 compilers
  integer(4) :: j, n4
  !  equivalence (moncar(3), ltdelt)
  !
  ! if interactive execution (spy),
  ! and if not Monte Carlo study,
  if (m4plot .eq. 1 .and. nenerg .eq. 0) tmax = fltinf      ! set end-time of study to infinity
  !     flag for postprocessing (ltdelt=-6789) skips the
  !     tampering with old plot file now connected to  lunit(2):
  if (ltdelt .eq. -6789) go to 1815
  close (unit = lunit(2), status = 'delete')
  !open (unit = lunit(2), status = 'scratch', form = 'unformatted')
  open (unit = lunit(2), status = 'scratch', form = 'unformatted')
1815 if (jflsos .eq. 0) go to 4271
  if (lastov .eq. 20) go to 4271
  close (unit = lunit(3))
  close (unit = lunit(9))
  n4 = lunit(3)                                   ! 1st of two units to be opened as scratch
5910 write (unit = ansi16, fmt = 5914) n4, (lstat(j), j = 14, 16)
5914 format ('st', i1, 'log', 3i1, '.dat', 3x)
  open (unit = n4, status = 'new', file = ansi16, form = 'unformatted')
  if (n4 .eq. lunit(9)) go to 4271                  ! both opened, so exit
  n4 = lunit(9)                                     ! switch to second of two i/o channels
  go to 5910
4271 if (nenerg .eq. 0) go to 5933
  !     "statisitics"  requires formatted writes to unit 12, which
  !     conflicts with usual usage.
  close (unit = lunit(12))
  !     open ( unit=lunit(12), type='scratch' )
  open (unit = lunit(12), status = 'scratch')
5933 if (m4plot .ne. 1) go to 5947                ! not interactive emtp
  if (iplot .ge. 0) go to 5947                    ! yes, plotting requested
  iplot = intinf                                  ! write little to plot file, not nothing
5947 return
end subroutine midov1

!
! subroutine nmincr.
!

subroutine nmincr (texta, n12)
  implicit none
  !     Module designed to serialize input root name  texta  with decimal
  !     component number  n12,  encoding only required digits.   It is
  !     assumed that a fortran 77 compiler is being used, and that
  !     "alphanumeric" is translated to something other than  character*6
  integer(4), intent(in) :: n12
  character(8), intent(out) :: texta              ! input argument carries a6 root name
  character(6) :: text1, text2                    ! local character-handling variables
  integer(4) :: j
  !
  write (unit = text1, fmt = 4523) n12            ! encode component number
4523 format (i6)                                  ! 6-digit format allows number through 999999
  do j = 1, 6                                     ! search for first non-blank in encoded number
     if (text1(j : j) .ne. ' ') exit              !  if nonblank, exit
  end do                                          ! end  do 4538  loop to find non-blank left edge
  write (unit = text2, fmt = 4556) texta          ! transfer input alphanumeric to char*6
4556 format (a6)                                  ! alphanumeric variables are 6 characters wide
  text2(j : 6) = text1(j : 6)                     ! add component number onto input name
  read (unit = text2, fmt = 4556) texta           ! convert back from char*6 to alphanum.
  !     write (*,*)  ' exit "nmincr".   n12 =',  n12,  '   texta =', texta
  return
end subroutine nmincr

!
! subroutine tacs1.
!

subroutine tacs1
  use blkcom
  use labcom
  use tacsar
  use smtacs
  use tracom
  use indcom
  use freedom
  implicit none
  character(8) :: stacs(11), alph(5), alnode, alnm1, alnm2
  character(8) :: dumj(33), sminus, splus, sbn(2)
  integer(4) :: i, ij, ijk, is, isour
  integer(4) :: j, j1, j2, jr
  integer(4) :: k, karg, kbtcs, kbwkcs, kcc, kjsup, kksup, kxai
  integer(4) :: ll0, ll1, ll2, ll3
  integer(4) :: m, mm
  integer(4) :: n, n1, n2, n3, n22, n23, ndx1, ndx2, ndx3, ndx5, ndy5, niunrs, nuki
  integer(4) :: nukr
  real(8) :: d1, dum(3)
  real(8) :: pru, prx
  !  dimension stacs(11), alph(5), dum(3), dumj(13)
  !
  data stacs(1)  / 'timex ' /
  data stacs(2)  / 'istep ' /
  data stacs(3)  / 'deltat' /
  data stacs(4)  / 'freqhz' /
  data stacs(5)  / 'omegar' /
  data stacs(6)  / 'zero  ' /
  data stacs(7)  / 'plus1 ' /
  data stacs(8)  / 'minus1' /
  data stacs(9)  / 'unity ' /
  data stacs(10) / 'infnty' /
  data stacs(11) / 'pi    ' /
  data splus     / '+' /
  data sminus    / '-' /
  data sbn(1)    / 'num.  ' /
  data sbn(2)    / 'den.  ' /
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module tacs1."')
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 701) ltacst, (lstat(i), i = 61, 68)
701 format (6x, 'ltacst, (lstacs(i), i = 1, 8) ... .', /, 9i10, /)
  if (nchain .eq. 1) go to 1001
  ndy5 = kud1 + (niu - 12) * 5
  go to 2210
1001 krsblk =  30
  kud1   =  krsblk + lstat(61) * 4
  kprsup =  kud1   + lstat(64) * 5
  kxar   =  kprsup + lstat(67)
  kxai   =  kxar   + lstat(68)
  kxtcs  =  kxai   + lstat(68)
  kawkcs =  kxtcs  + lstat(68) * 2
  kbwkcs =  kawkcs + lstat(68)
  katcs  =  kbwkcs + lstat(68)
  kbtcs  =  katcs  + lstat(62)
  kisblk = (kbtcs  + lstat(62)) * nbyte(3) / nbyte(4) + 1
  kksus  =  kisblk + lstat(61) * 8
  kalksu =  kksus  + lstat(63)
  kiuty  =  kalksu + lstat(63)
  kinsup =  kiuty  + lstat(64)
  kivarb =  kinsup + lstat(65) * 3
  klntab =  kivarb + lstat(66)
  kjout  =  klntab + lstat(68)
  kcolcs =  kjout  + lstat(68)
  n1 = (kcolcs + lstat(62)) * nbyte(4) / nbyte(3) + 1
  kaliu = klntab
  kspvar = kaliu + lstat(64)
  if (iprsup .lt. 2) go to 7811
  write (unit = lunit(6), fmt = 703) n1, kisblk, krsblk, kksus , kalksu, kiuty , kud1  , kinsup, kivarb, kprsup, kawkcs, kxar, kxtcs, &
       klntab, katcs , kcolcs, kjout
703 format ('  Use cells  n1 = ',  i8 ,/, '  kisblk = ', i5, '  krsblk = ', i5, '  kksus  = ',i5, &
       '  kalksu = ', i5, '  kiuty  = ',i5, '  kud1   = ', i5, '  kinsup = ', i5, '  kivarb = ', i5 ,/, &
       '  kprsup = ',i5, '  kawkcs = ', i5, '  kxar   = ', i5, '  kxtcs  = ', i5, '  klntab = ', i5, '  katcs  = ', i5, &
       '  kcolcs = ', i5, '  kjout  = ', i5, /)
7811 lstat(39) = n1
  koncur = 0
  if (n1 .lt. ltacst) go to 781
  kill = 1
  lstat(19) = 781
  lstat(16) = 19
  go to 9000
781 do i = 1, 8
     lstat( i) = lstat(i + 60)
  end do
  do i = 30, ltacst
     parsup(i) = 0.0
  end do
  isour = lstat(4)
  do i = 1, isour
     n23 = 0
     if (i .gt. 11) go to 3388
     alnm1 = stacs(i)
     call namea6 (alnm1, n23)
3388 ilntab(kaliu + i) = n23
     iuty(kiuty + i) = 0
  end do
  iuty(kiuty + 3) = 20
  sptacs(29) = fltinf
  if (pu .eq. fltinf) go to 4499
  sptacs(29) = pu
  iuty(kiuty + 2) = nuk
  iuty(kiuty + 3) = 0
  if (pu .le. 0.) iuty(kiuty + 3) = nuk
4499 d1 = alog1z (fltinf)
  iuty(kiuty + 11) = int (d1 + 0.5)
  !     $$$  input  $$$m13. 364
  lbstac = location (ismtac(1)) - location (etac(1))
  ntotac = 0
  nsu = 0
  nsudv = lstat(63)
  nuk = 0
  nuki = kisblk - 8
  nukr = krsblk - 4
  niu = 11
  ndy5 = kud1 - 5
  niunrs = 12
  iuty(kiuty + 1) = niunrs
  nsup = 0
  karg = kivarb - 3
  kpar = 0
  ioutcs = 0
  kxic = 0
  ll0 = 0
  ll1 = 1
  ll2 = 2
  ll3 = 3
  go to 2210
100 call cimage
2210 if (kill .gt. 0) go to 9000
  if (kolbeg .gt. 0) go to 6574
  read (unit = abuff, fmt = 176) n
176 format (i2)
1234 if (n .eq. 88 .or. n .eq. 98 .or. n .eq. 99) go to 113
  if (n .eq. 33) go to 2222
  if (n .eq. 77) go to 3333
  if (n .gt. 10 .and. n .lt. 25) go to 1066
  if (n .gt. 89 .and. n .lt. 94) go to 1066
  if (kolbeg .gt. 0 ) go to 1111
  read (unit = abuff, fmt = 177) (dumj(i), i = 1, 5)
  read (unit = abuff, fmt = 178) n, alnode, (alph(i), i = 1, 5), (dum(i), i = 1, 3), alnm1, alnm2
177 format (10x, 5(1x, a6, 1x))
178 format (i2, a6, 2x, 5(a1, 7x), 3e6.0, 2a6)
  go to 6590
6574 nfrfld = 1
  !  call frefld (voltbc)
  call free (voltbc)
  n = int (voltbc(1))
  go to 1234
1111 nright = -1
  !  call freone (d1)
  call free (d1)
  alnode = texta6(1)
  do i = 1, 5
6576 bus1 = texcol(kolbeg)
     if (bus1 .ne. blank) go to 6577
     kolbeg = kolbeg + 1
     go to 6576
6577 if (bus1 .eq. sminus) go to 6578
     if (bus1 .eq. splus) go to 6578
     if (bus1 .eq. csepar) go to 6584
     alph(i) = splus
     go to 6581
6578 alph(i) = bus1
     kolbeg = kolbeg + 1
     !6581 call freone (d1)
6581 call free (d1)
     dumj(i) = texta6(1)
     go to 6587
6584 alph(i) = blank
     dumj(i) = blank
     kolbeg = kolbeg + 1
  end do
6587 continue
  nright = 0
  nfrfld = 3
  !  call frefld (dum)
  call free (dum)
  nright = -1
  nfrfld = 2
  !  call freone (d1)
  call free (d1)
  alnm1 = texta6(1)
  alnm2 = texta6(2)
  nright = 0
6590 n1 = 0
  do i = 1, 5
     if (dumj(i) .eq. blank) go to 136
     if (alph(i) .eq. sminus) go to 135
     if (alph(i) .eq. splus) go to 135
     kill = 137
     lstat(14) = i
     bus1 = alnode
     bus2 = dumj(i)
     bus3 = alph(i)
     lstat(19) = 133
     go to 9000
135  n1 = 1
  end do
136 continue
  if (n1 .gt. 0) go to 139
  if (alnode .ne. blank) go to 139
  if (n .ne. 1) go to 1399
  iuty(kiuty + 6) = 9999
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 152)
152 format ('+Request for output of all TACS variables.')
  go to 100
1399 if (n .ne. 0) go to 139
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 137)
137 format ('+Blank card terminating all TACS data cards .')
  go to 101
139 if (dum(1) .eq. 0.0) dum(1) = 1.0
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 179) alnode, n, dum(1)
179 format ('+TACS function ', "'", a6, "'", ', order', i2, '.', e14.4)
  if (n .lt. 8 .and. n .ge. 0) go to 118
  kill = 115
  lstat(19) = 118
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 163)
163 format ('0limit.  Dimensions in TACS have not been set to accept an order greater than  7.                                   ', /, &
       'It is suggested to break this monstruous block into a set of smaller ones in cascade.')
7433 bus1 = alnode
  lstat(14) = n
  go to 9000
118 nuk = nuk + 1
  nuki = nuki + 8
  nukr = nukr + 4
  if (nuk .le. lstat(61)) go to 8101
  kill = 122
  lstat(16) = lstat(61)
  lstat(17) = 1
  lstat(19) = 8101
  go to 9000
8101 ndx1 = katcs + nuk
  n23 = 0
  call namea6 (alnode, n23)
  atcs(ndx1) = n23
  isblk(nuki+1) = nsu + 1
  isblk(nuki+4) = 0
  n1 = 0
  do i = 1, 5
     if (dumj(i) .eq. blank) go to 7413
     n1 = n1 + 1
     nsu = nsu + 1
     if (nsu .le. nsudv) go to 8102
     kill = 122
     lstat(16) = lstat(63)
     lstat(17) = 3
     lstat(19) = 8102
     go to 9000
8102 ndx1 = kalksu + nsu
     n23 = 0
     alnode = dumj(i)
     call namea6 (alnode, n23)
     ksus(ndx1) = n23
     ndx1 = kksus  + nsu
     ksus(ndx1) = 1
     if (alph(i) .eq. sminus) ksus(ndx1) = -1
     if (i .eq. 5) go to 7413
     mm = i + 1
     do ij = mm, 5
        if (dumj(i) .ne. dumj(ij)) go to 7418
        dumj(ij) = blank
        ksus(ndx1) = ksus(ndx1) + 1
        if (alph(ij) .eq. sminus) ksus(ndx1) = ksus(ndx1) - 2
     end do
7418 continue
     if (ksus(ndx1) .ne. 0) go to 7413
     nsu = nsu - 1
     n1  =  n1 - 1
  end do
7413 continue
  if (n1 .gt. 0) go to 7420
  kill = 136
  ndx1 = int (atcs(katcs + nuk))
  bus1 = texvec(ndx1)
  lstat(14) = n
  lstat(19) = 7420
  write (unit = lunit(6), fmt = 3)
3 format (/, ' Error.  No input node specified.')
  go to 9000
7420 if (n .eq. 0) go to 103
  rsblk(nukr + 1) = 999999.99
  j = kpar + 1
  isblk(nuki + 3) = j
  kcc = n * 6
  kpar = j + kcc + 3
  if (kpar .le. lstat(67)) go to 8103
  kill = 122
  lstat(16) = lstat(67)
  lstat(17) = 7
  lstat(19) = 8103
  go to 9000
103 rsblk(nukr + 1) = dum(1)
  isblk(nuki + 3) = 0
8103 isblk(nuki + 2) = n + 1
  ndx1 = kxar + nuk
  xar(ndx1)  = dum(2)
  ndx1 = kxai + nuk
  xar(ndx1) = dum(3)
  ndx1 = kawkcs + nuk
  n23 = 0
  call namea6 (alnm1, n23)
  awkcs(ndx1)  = n23
  ndx1 = kbwkcs + nuk
  n23 = 0
  call namea6 (alnm2, n23)
  awkcs(ndx1)  = n23
  if (alnm1 .ne. blank .or. alnm2 .ne. blank .or. dum(2) .ne. 0.0 .or. dum(3) .ne. 0.0) isblk(nuki + 4) = -1
  if (n .eq. 0) go to 100
  n3 = kcc
  if (kcc .gt. 18) n3 = 18
  j1 = j + 1
  ndx3 = kprsup + j1 + n3
  ijk = 0
  do j2 = j, j1
     call cimage
     ijk = ijk + 1
     ndx1 = kprsup + j2
     if (kolbeg .gt. 0) go to 6514
     ndx2 = kprsup + kpar
     read (unit = abuff, fmt = 181) (parsup(i), i = ndx1, ndx2, 6)
181  format (8e10.0)
     go to 6517
6514 nfrfld = 1
     n22 = n + 1
     do is = 1, n22
        !        call frefld (parsup(ndx1 :))
        call free (parsup(ndx1 :))
        ndx1 = ndx1 + 6
     end do
     ndx1 = kprsup + j2
6517 if (noutpr .eq. 0) write (unit = kunit6, fmt = 182) sbn(ijk), (parsup(i), i = ndx1, ndx3, 6)
  end do
182 format ('+  ', a6, 4e10.2)
  do i = j, kpar, 6
     ndx1 = kprsup  + i
     parsup(ndx1 + 3) = parsup(ndx1 + 1)
     if (parsup(ndx1) .eq. 0.0) go to 119
     parsup(ndx1) = parsup(ndx1) * dum(1)
     parsup(ndx1 + 2) = parsup(ndx1)
     go to 102
119  if (i .ne. kpar - 3 .and. i .ne. j) go to 102
     if (parsup(ndx1 + 1) .ne. 0.0) go to 102
     kill = 116
     lstat(19) = 119
     lstat(17) = 0
     if (i .eq. j .and. i .ne. kpar - 3) lstat(17) = 1
     go to 7433
  end do
102 continue
  go to 100
101 konsce = niu
  konsup = nsup
  if (nuk .gt. 0) go to 231
  nuk = 1
  nsu = 1
  atcs(katcs + 1) = 1
  isblk(kisblk + 4) = 0
  isblk(kisblk + 1) = 1
  ksus(kalksu + 1) = 1
  ksus(kksus + 1) = 1
  rsblk(krsblk + 1) = 1.0
  isblk(kisblk + 3) = 0
  isblk(kisblk + 2) = 1
  awkcs(kawkcs + 1) = 1
  awkcs(kbwkcs + 1) = 1
231 if (iprsup .lt. 2) go to 4444
  if (nuk .eq. 0) go to 2315
  write (unit = lunit(6), fmt = 1998) nuk
1998 format (' nuk = ', i4, /, 10x, ' alksx    ksj   kfst    kni')
  nuki = kisblk - 8
  do n = 1, nuk
     nuki = nuki + 8
     write (unit = lunit(6), fmt = 1999) n, atcs(katcs + n), isblk(nuki + 1), isblk(nuki + 2), isblk(nuki + 3)
  end do
1999 format (2x, i4, 2x, f8.0, 3i7)
  write (unit = lunit(6), fmt = 2005) nsu, (n, ksus(kalksu + n), ksus(kksus + n), n = 1, nsu)
2005 format ('0 nsu = ', i3, /, 8x, 'ksu    ksus  ', /, (1x, i3, 1x, 2i6))
2315 write (unit = lunit(6), fmt = 2006) niu
2006 format ('0 niu = ', i3, /, 8x, 'aliu   iuty', 7x, 'ud1', 12x, 'ud2', 12x, 'ud3', 12x, 'ud4', 12x, 'ud5')
  ndx5 = kud1 - 5
  do n = 1, niu
     n1 = ilntab(kaliu + n)
     if (n .gt. 11) go to 2121
     write (unit = lunit(6), fmt = 2306) n, texvec(n1)
     go to 2323
2121 ndx5 = ndx5 + 5
     write (unit = lunit(6), fmt = 2306) n, texvec(n1), iuty(kiuty + n), ud1(ndx5 + 1), ud1(ndx5 + 2), ud1(ndx5 + 3),  ud1(ndx5 + 4), ud1(ndx5 + 5)
2306 format (1x, i3, 1x, a8, i6, 5e15.6)
  end do
2323 continue
  if (nsup .eq. 0) go to 4444
  write (unit = lunit(6), fmt = 2007) nsup, (n, ilntab(kspvar + n), insup(kjsup + n), insup(kksup + n), n = 1, nsup)
2007 format ('0 nsup=', i6, /, 8x, '  supvar    jsup    ksup', /, (4i8))
  write (unit = lunit(6), fmt = 1033) karg
1033 format ('  karg = ', i8, /, '       n  iopsup  ifnsup  irgsup    idev     kdj     kdk  ildev1  ildev2')
  do i = 1, nsup
     n1 = insup(kjsup + i)
     if (n1 .lt. 0) go to 2014
     n2 = insup(kksup + i)
     write (unit = lunit(6), fmt = 2008) (n, ivarb(n + 1), ivarb(n + 2), ivarb(n + 2), n = n1, n2, 3)
2008 format (4i8)
     go to 2034
2014 n1 = -n1
     write (unit = lunit(6), fmt = 2022) n1, ivarb(n1), ivarb(n1 + 1), ivarb(n1 + 2), ivarb(n1 + 3), ivarb(n1 + 4)
2022 format (i8, 24x, 5i8)
  end do
2034 continue
  if ( kpar .ne. 0 ) write (unit = lunit(6), fmt = 2009) kpar, (parsup(kprsup + n), n = 1, kpar)
2009 format ('  kpar=', i8, 6x, 'parsup as follows:', /, (8e16.6))
  n22 = nsudv + 1
  if ( nsudv .lt. lstat(63) ) write (unit = lunit(6), fmt = 10031) nsudv, (n, ksus(kalksu + n), ksus(kksus + n), n = n22, lstat(63))
10031 format ('0 nsudv=', i4, /, 12x, 'alksu    ksus', /, (1x, 3i8))
  go to  4444
1066 if (kill .gt. 0) go to 9000
  if (kolbeg .gt. 0) go to 6534
  read (unit = abuff, fmt = 187) n, alnode, dum(1), dum(3), dum(2), prx, pru
187 format (i2, a6, 2x, 3e10.0, 20x, 2e10.0)
  go to 6537
6534 nright = -1
  !  call freone (d1)
  call free (d1)
  alnode = texta6(1)
  nright = 0
  nfrfld = 5
  !  call frefld (voltbc)
  call free (voltbc)
  dum(1) = voltbc(1)
  dum(3) = voltbc(2)
  dum(2) = voltbc(3)
  prx = voltbc(4)
  pru = voltbc(5)
6537 niu = niu + 1
  ndy5 = ndy5 + 5
  if (niu .gt. lstat(64)) go to 8107
  ndx1 = kiuty + niu
  iuty(ndx1) = n
  ndx1 = kaliu  + niu
  n23 = 0
  call namea6 (alnode, n23)
  ilntab(ndx1) = n23
  ud1(ndy5 + 1) = dum(1)
  ud1(ndy5 + 2) = dum(2)
  if (n .ne. 14) go to 8108
  ud1(ndy5 + 2) = dum(2) * twopi / 360.
8108 if (n .ne. 23) go to 8109
  if (ud1(ndy5 + 2) .lt. deltat) ud1(ndy5 + 2) = deltat
8109 continue
  ud1(ndy5 + 3) = dum(3)
  ud1(ndy5 + 4) = prx
  ud1(ndy5 + 5) = fltinf
  if (pru .ne. 0.0) ud1(ndy5 + 5) = pru
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 170) (dum(i), i = 1, 3)
170 format ('+TACS source.', 3e12.3)
  if (nchain .eq. 1) go to 100
  if (n .ne. 90) go to 1091
  do j = 2, ntot
     if (alnode .eq. bus(j)) go to 479
  end do
  write (unit = lunit(6), fmt = 168) alnode
168 format ('  Type 90 source ', '"', a6, '"', ' is not a recognizable node name in EMTP. The card will be discard')
  go to 8579
1091 if (n1 .ne. 91 .and. n1 .ne. 93) go to 1092
  do j = 1, kswtch
     k = iabs (kmswit(j))
     ndx2 = lswtch + j
     m = iabs (kmswit(ndx2))
     if (alnode .eq. bus(k)) go to 479
     if (alnode .eq. bus(m)) go to 479
  end do
  write (unit = lunit(6), fmt = 55168) alnode
55168 format ('  Source 90 or 91 ', '"', a6, '"', ' is not a recognizable switch node name in EMTP. The card will be discard')
8579 niu  = niu  - 1
  ndy5 = ndy5 - 5
  go to 9000
1092 if (n1 .ne. 92) go to 8579
  write (unit = lunit(6), fmt = 5566)
5566 format ('  ----- This card will be ignored, because #92 source cannot be changed   ----')
  go to  8579
479 ud1(ndy5 + 2) = j
  go to 9000
8107 kill = 122
  lstat(16) = lstat(64)
  lstat(17) = 4
  lstat(19) = 8107
  go to 9000
2222 if (kolbeg .gt. 0) go to 6554
  read (unit = abuff, fmt = 190) (dumj(i), i = 1, 13)
190 format (2x, 13a6)
  go to 6557
6554 nfrfld = 13
  nright = -1
  !  call freone (d1)
  call free (d1)
  nright = 0
  do i = 1, 13
     dumj(i) = texta6(i)
  end do
6557 jr = 0
  do i = 1, 13
     if (dumj(i) .eq. blank) go to 1199
     ioutcs = ioutcs + 1
     if (ioutcs .gt. lstat(68)) go to 4466
     jr = 1
     n23 = 0
     alnode = dumj(i)
     call namea6 (alnode, n23)
     ndx1 = kjout + ioutcs
     jout(ndx1) = n23
  end do
1199 continue
  if (jr .eq. 1) go to 3535
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 4455)
3535 if (noutpr .eq. 0) write (unit = kunit6, fmt = 154)
154 format ('+TACS variables for EMTP output vector.')
  go to 100
4466 lstat(19) = 1199
9300 kill = 122
  lstat(16) = lstat(68)
  lstat(17) = 8
  go to 9000
3333 if (kolbeg .gt. 0) go to 6564
  read (unit = abuff, fmt = 195) alnode, prx
195 format (2x, a6, 2x, e10.0)
  go to 6567
6564 nfrfld = 1
  nright = -1
  !  call freone (d1)
  call free (d1)
  alnode = texta6(1)
  nright = 0
  !  call freone (prx)
  call free (prx)
6567 if (alnode .ne. blank) go to 3838
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 4455)
4455 format ('+Ignore the illegal card.')
  go to 100
3838 kxic = kxic + 1
  if (kxic .lt. lstat(68)) go to 3377
  lstat(19) = 3377
  go to 9300
3377 ndx1 = kxtcs + lstat(68) + kxic
  xtcs(ndx1) = prx
  ndx2 = kxtcs + kxic
  n23 = 0
  call namea6 (alnode, n23)
  xtcs(ndx2) = n23
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 4195) alnode, prx
4195 format ('+Init. cond. ', "'", a6, "'", e16.6)
  go to 100
113 call tacs1a
  if (kill .gt. 0) go to 9000
  if (nchain .eq. 1) go to 100
  ktab = ktab + 1
  if (ktab .le. lstat(68)) go to 4444
  lstat(19) = 113
  go to 9300
4444 call tacs1b
9000 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module tacs1."')
  return
end subroutine tacs1

!
! subroutine tacs1a.
!

subroutine tacs1a
  use blkcom
  use labcom
  use tacsar
  use movcop
  use freedom
  implicit none
  integer(4) :: i, i1, i2, i3, i4, iargel, icurch, iel, iflpnt, ifree, ifst, ifstch
  integer(4) :: ikill1, ikill2, ilgcl, ilglph, ilgnum, ilst, ilst1, isrchl, itmpbf
  integer(4) :: j, j1, j2
  integer(4) :: k, k1, k2, k3, k4, karg, kjsup, kksup, m, moon, mpar
  integer(4) :: n, n1, n6, n23, ndx1, ndx6
  real(8) :: argel, d1, d9, d10, dum, pru, prx
  character(8) :: alph, dumj
  dimension alph(5), dum(3), dumj(13)
  character(8) :: text1, text2, text4, sminus, splus, smultp
  character(8) :: supfn, supop, text5, text6, text7
  character(8) :: alnode, alnm1, alnm2, atmpbf
  dimension supfn( 35), supop( 6)
  character(8) :: el, alnrcl, sepch, opname, btmpbf
  character(8) :: csprch, curch, curch1, contch, chdum1, chdum2
  character(8) :: eqlsgn, ch9, chdolr, comma, che, chd
  character(8) :: cha, chn, cho, cht, chq, chl, chg, chr
  dimension el(100), alnrcl(10), sepch(8), opname(18)
  dimension atmpbf(20), btmpbf(80)
  dimension argel(100)
  dimension iel(100)
  !
  data alnrcl(1)  / '1     ' /
  data alnrcl(2)  / '2     ' /
  data alnrcl(3)  / '3     ' /
  data alnrcl(4)  / '4     ' /
  data alnrcl(5)  / '5     ' /
  data alnrcl(6)  / '6     ' /
  data alnrcl(7)  / '7     ' /
  data alnrcl(8)  / '8     ' /
  data alnrcl(9)  / '9     ' /
  data alnrcl(10) / '0     ' /
  data sepch(1)   / '(     ' /
  data sepch(2)   / ')     ' /
  data sepch(3)   / '+     ' /
  data sepch(4)   / '-     ' /
  data sepch(5)   / '/     ' /
  data sepch(6)   / '*     ' /
  data sepch(7)   / '.     ' /
  data sepch(8)   / '**    ' /
  data opname(1)  / '(     ' /
  data opname(2)  / '.not. ' /
  data opname(3)  / '.or.  ' /
  data opname(4)  / '.ornot' /
  data opname(5)  / '.and. ' /
  data opname(6)  / '.andnt' /
  data opname(7)  / ')     ' /
  data opname(8)  / '.ne.  ' /
  data opname(9)  / '.eq.  ' /
  data opname(10) / '.lt.  ' /
  data opname(11) / '.le.  ' /
  data opname(12) / '.ge.  ' /
  data opname(13) / '.gt.  ' /
  data opname(14) / '*     ' /
  data opname(15) / '/     ' /
  data opname(16) / '**    ' /
  data opname(17) / '+     ' /
  data opname(18) / '-     ' /
  data eqlsgn / '=     ' /
  data chdolr / '$     ' /
  data ch9    / '9     ' /
  data  che   / 'e' /
  data  chd   / 'd' /
  data  comma / ',' /
  data  cha   / 'a' /
  data  chn   / 'n' /
  data  cho   / 'o' /
  data  cht   / 't' /
  data  chq   / 'q' /
  data  chl   / 'l' /
  data  chg   / 'g' /
  data  chr   / 'r' /
  data supfn( 1) / '     ' /
  data supfn( 2) / 'and. ' /
  data supfn( 3) / 'or.  ' /
  data supfn( 4) / 'not. ' /
  data supfn( 5) / 'nand.' /
  data supfn( 6) / 'nor. ' /
  data supfn( 7) / '     ' /
  data supfn( 8) / '     ' /
  data supfn( 9) / '     ' /
  data supfn(10) / '     ' /
  data supfn(11) / '     ' /
  data supfn(12) / 'sin  ' /
  data supfn(13) / 'cos  ' /
  data supfn(14) / 'tan  ' /
  data supfn(15) / 'cotan' /
  data supfn(16) / 'sinh ' /
  data supfn(17) / 'cosh ' /
  data supfn(18) / 'tanh ' /
  data supfn(19) / 'asin ' /
  data supfn(20) / 'acos ' /
  data supfn(21) / 'atan ' /
  data supfn(22) / 'exp  ' /
  data supfn(23) / 'log  ' /
  data supfn(24) / 'log10' /
  data supfn(25) / 'sqrt ' /
  data supfn(26) / 'abs  ' /
  data supfn(27) / 'trunc' /
  data supfn(28) / 'minus' /
  data supfn(29) / 'invrs' /
  data supfn(30) / 'rad  ' /
  data supfn(31) / 'deg  ' /
  data supfn(32) / 'seq6 ' /
  data supfn(33) / 'sign ' /
  data supfn(34) / 'not  ' /
  data supfn(35) / 'ran  ' /
  data supop( 1) / '+    ' /
  data supop( 2) / '-    ' /
  data supop( 3) / '*    ' /
  data supop( 4) / '/    ' /
  data supop( 5) / '0    ' /
  data supop( 6) / '.    ' /
  data  text4    / 'countr' /
  data  text5    / 'input ' /
  data  text6    / 'output' /
  data  text7    / 'inside' /
  data  splus    / '+' /
  data  sminus   / '-' /
  data  smultp   / '*' /
  data  text1    / 'value ' /
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module tacs1a."')
  kjsup = kinsup + lstat(65)
  kksup = kjsup  + lstat(65)
  read (unit = abuff, fmt = 180) n, alnode, m
180 format (i2, a6, i2)
  nsup = nsup + 1
  if (nsup .le. lstat(65)) go to 2515
  kill = 122
  lstat(19) = 2515
  lstat(16) = lstat(65)
  lstat(17) = 5
  go to 9000
2515 ndx1 = kspvar + nsup
  n23 = 0
  call namea6 (alnode, n23)
  ilntab(ndx1) = n23
  insup(kinsup + nsup) = 0
  if (m .ne. 0) go to 10700
  read (unit = abuff, fmt = 12516) alnm1
12516 format (10x, a1)
  if (alnm1 .ne. eqlsgn) go to 12517
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 1251)
1251 format ('+TACS supplemental Fortran expression')
  !     ***  read  this  arbitrary  logical/algebraic
  !     ***   free-format  fortran  expression
  !     ***  * * * * * * * * * * * * * * * * *   ***
  iargel = 0
  ilst = 0
  icurch = 11
  isrchl = 1
  iflpnt = 0
  ilglph = 0
  ilgnum = 0
  ilgcl = 0
  itmpbf = 0
  ifstch = 12
  curch = chdolr
  csprch = csepar
  csepar = comma
  contch = chcont
  if (contch .eq. ch9) contch = chdolr
  ifree = 100
  !  call move0 (iel(1), ifree)
  call move0 (iel(1 :), ifree)
  !     :::  interpret next character   :::
50010 icurch = icurch + 1
  if (icurch .gt. 80) go to 50030
  curch1 = curch
  curch = texcol(icurch)
  if (curch .eq. blank) go to 50035
  if (curch .eq. contch) go to 50040
  k1 =  0
  do j = 1, 7
     if (curch .eq. sepch(j)) k1 = j
  end do
  if (isrchl .eq. 1) go to 50065
  if (ilgcl .eq. 1 .and. iflpnt .eq. 2) go to 50120
  if (k1 .eq. 0) go to 50020
  if (ilgnum .eq. 0) go to 50016
  if (k1 .ne. 3 .and. k1 .ne. 4) go to 50016
  if (curch1 .eq. che .or. curch1 .eq. chd) go to 50020
50016 if (k1 .ne. 6 .or. curch1 .ne. sepch(6)) go to 50017
  k1 = 8
  go to 50165
50017 if (k1 .ge. 1 .and. k1 .le. 6) go to 50120
  ! m28.2042
  ! :::  this char is '.' not first  :::
  iflpnt = iflpnt + 1
  if (ilgcl .eq. 1) go to 50020
  ! m28.2046
  ! :::  this  '.'  follows alph or num  :::
  go to 50021
50019 if (ilgcl .eq. 1) go to 50120
  ! m28.2050
  ! :::  this char not first not separ  :::
50020 if (ilglph .gt. 0) ilglph = ilglph + 1
  if (ilgnum .gt. 0) ilgnum = ilgnum + 1
  go to 50010
  ! m28.2055
  ! :::  preread  4  char  :::
50021 if (icurch .gt. 77) go to 50024
  curch1 = texcol(icurch + 1)
  chdum1 = texcol(icurch + 2)
  chdum2 = texcol(icurch + 3)
  if (chdum2 .eq. sepch(7)) go to 50022
  if (icurch .gt. 76) go to 50024
  if (texcol(icurch + 4) .ne. sepch(7)) go to 50024
  if (curch1 .eq. chn .and. chdum1 .eq. cho .and. chdum2 .eq. cht) go to 50023
  if (curch1 .eq. cha .and. chdum1 .eq. chn .and. chdum2 .eq. chd) go to 50023
  go to 50024
50022 if (curch1 .eq. cho .and. chdum1 .eq. chr) go to 50023
  if (curch1 .eq. chn .and. chdum1 .eq. che) go to 50023
  if (curch1 .eq. che .and. chdum1 .eq. chq) go to 50023
  if (chdum1 .ne. cht .and. chdum1 .ne. che) go to 50024
  if (curch1 .ne. chl .and. curch1 .ne. chg) go to 50024
50023 ilgcl = 1
50024 if (ilglph + ilgnum .gt. 0) go to 50019
  if (ilgcl .eq. 0) go to 50025
  ilglph = 1
  go to 50010
50025 do j = 1, 10
     if (curch1 .eq. alnrcl(j)) go to 50028
  end do
50027 ikill1 = 12
  ikill2 = 50026
  lstat(16) = icurch
  go to 50900
50028 ilgnum = 1
  go to 50010
  ! m28.2088
  ! :::  hit col. 81  :::
50030 if (isrchl .eq. 1) go to 50185
  isrchl = -1
  go to 50120
  ! m28.2093
  ! :::  hit a blank  :::
50035 if (isrchl .eq. 1) go to 50010
  if (iflpnt .eq. 1 .and. curch1 .eq. sepch(7) .and. ilgnum .eq. 0) go to 50027
  isrchl = 1
  go to 50120
  ! m28.2100
  ! :::  hit continuation-character  :::
50040 if (isrchl .eq. 1) go to 50050
  icurch = icurch - 1
  if (icurch .lt. ifstch) go to 50050
  do j = ifstch, icurch
     itmpbf = itmpbf + 1
     atmpbf(itmpbf) = texcol(j)
  end do
50050 icurch = 0
  ifstch = 1
  call cimage
  go to 50010
  ! :::  first character of a new word  :::
50065 isrchl = 0
  ifstch = icurch
  if (k1 .ne. 6) go to 50070
  if (icurch .eq. 80) go to 50070
  if (texcol(icurch + 1) .eq. sepch(6)) go to 50010
50070 if (k1 .ne. 7) go to 50075
  iflpnt = 1
  go to 50021
50075 if (k1 .ge. 1 .and. k1 .le. 6) go to 50165
  ! :::  first char. of numer.  or  alphanum.  :::
  do j = 1, 10
     if (curch .eq. alnrcl(j)) go to 50090
  end do
  ilglph = 1
  go to 50010
50090 ilgnum = 1
  go to 50010
  ! :::  current word is complete  :::
50120 curch1 = texcol(icurch)
  texcol(icurch) = comma
  if (ilgnum .le. 20) go to 50125
  ikill1 = 10
  ikill2 = 50120
  lstat( 16) = icurch - 1
  go to 50900
50125 if (ilglph .le. 6) go to 50130
  ikill1 = 11
  ikill2 = 50125
  lstat( 16) = icurch - 1
  go to 50900
50130 if (itmpbf .eq. 0) go to 50150
  ! : word is spread over more than one card
  do j = 1, 80
     btmpbf(j) = texcol(j)
  end do
  do j = 1, itmpbf
     texcol(j) = atmpbf(j)
  end do
  i1 = ilgnum + ilglph + 1
  itmpbf = itmpbf + 1
  do j = itmpbf, i1
     k = j - itmpbf + 1
     texcol(j) = btmpbf(k)
  end do
50150 if (ilgnum .gt. 0) go to 50155
  if (ilglph .gt. 0) go to 50160
  ! :::  storing numerical word  :::
50155 if (iflpnt .le. 1) go to 50156
  ikill1 = 13
  ikill2 = 50155
  lstat(16) = icurch - 1
50156 nfrfld = 1
  nright = 0
  kolbeg = ifstch
  iargel = iargel + 1
  !  call frefld (argel(iargel :))
  call free (argel(iargel :))
  ilst = ilst + 1
  iel(ilst) = -ilst - 1
  el(ilst) = char (iargel)
  ilgnum = 0
  iflpnt = 0
  go to 50170
  ! :::  storing alphanumeric word  :::
50160 nfrfld = 1
  nright = -1
  kolbeg = ifstch
  !  call freone (d1)
  call free (d1)
  ilst = ilst + 1
  iel(ilst) = ilst + 1
  el(ilst) = texta6(1)
  ilglph = 0
  ilgcl = 0
  iflpnt = 0
  go to 50170
  ! :::  storing  operator  :::
50165 isrchl = 1
  ilst = ilst + 1
  iel(ilst) = ilst + 1
  el(ilst) = sepch(k1)
  go to 50010
  ! :::  restore current line  :::
50170 if (itmpbf .eq. 0) go to 50180
  itmpbf = 0
  do j = 1, 80
     texcol(j) = btmpbf(j)
  end do
50180 texcol(icurch) = curch1
  !  if (isrchl) 50185, 50065, 50010
  if (isrchl .lt. 0) then
     go to 50185
  else if (isrchl .eq. 0) then
     go to 50065
  else
     go to 50010
  end if
  ! :::  out : whole expression has been interpreted  :::
50185 if (ilst .ne. 0) go to 50190
  ikill1 = 2
  ikill2 = 50185
  go to 50900
50190 if (ilst .le. 100) go to 50195
  ikill1 = 8
  ikill2 = 50190
  go to 50900
50195 j = iel(ilst)
  iel(ilst) = 999
  if (j .lt. 0) iel(ilst) = -999
  csepar = csprch
  ! ***  check  validity  and  pre-process
  ! ***   this  fortran  expression
  ! ***   * * * * * * * * * * * * * * * * *
  ifst = 1
  ilst1 = ilst
  ! :::  check brackets  :::
50200 i1 = 0
  k1 = 0
  i2 = 0
  i = ifst
50201 alnm1 = el(i)
  if (alnm1 .eq. opname(1)) go to 50215
  if (alnm1 .ne. opname(7)) go to 50220
  i1 = i1 - 1
  !  if (i1) 50205, 50210, 50220
  if (i1 .lt. 0) then
     go to 50205
  else if (i1 .eq. 0) then
     go to 50210
  else
     go to 50220
  end if
50205 ikill1 = 3
  ikill2 = 50205
  go to 50900
50210 if (k1 .eq. 1) go to 50220
  k1 = 1
  if (i2 .eq. 0 .or. i .lt. ilst) go to 50220
  el(ifst) = blank
  el(ilst) = blank
  iel(ifst) = 0
  iel(ilst) = 0
  ifst = ifst + 1
  ilst = ilst - 1
  ilst1 = ilst
  j = iel(ilst)
  iel(ilst) = 999
  if (j .lt. 0) iel(ilst) = -999
  if (ifst .le. ilst) go to 50200
  ikill1 = 2
  ikill2 = 50210
  go to 50900
50215 if (i .eq. ifst) i2 = 1
  i1 = i1 + 1
50220 i = i + 1
  if (i .le. ilst) go to 50201
  if (i1 .le. 0) go to 50225
  ikill1 = 1
  ikill2 = 50220
  go to 50900
  ! m28.2264
  ! :::  check all pairs of adjacent elements  :::
50225 i = ifst - 1
50230 i = i + 1
  if (i .gt. ilst) go to 50485
50235 k1 = 0
  alnm1 = el(i)
  if (alnm1 .eq. blank) go to 50230
  do j = 1, 18
     if (alnm1 .eq. opname(j)) k1 = j
  end do
  k2 = 0
  k = i
50245 k = k + 1
  if (k .gt. ilst) go to 50255
  alnm2 = el(k)
  if (alnm2 .eq. blank) go to 50245
  do j = 1, 18
     if (alnm2 .eq. opname(j)) k2 = j
  end do
50255 if (k1 .ne. 0) go to 50265
  k3 = 0
  do j = 12, 35
     if (alnm1 .eq. supfn(j)) k3 = j
  end do
50265 if (i .eq. ilst) go to 50470
  if (k1 .eq. 0) go to 50380
  if (k2 .lt. 2) go to 50270
  if (k1 .ge. 14) go to 50300
50270 if (k2 .lt. 2 .or. k2 .gt. 16) go to 50275
  if (k1 .eq. 3 .or. k1 .eq. 5 .or. k1 .eq. 7) go to 50275
  if (k1 .ge. 2 .and. k1 .le. 13) go to 50300
50275 if (k2 .lt. 3 .or. k2 .gt. 16) go to 50280
  if (k1 .eq. 3 .or. k1 .eq. 5) go to 50300
50280 if (k2 .lt. 5 .or. k2 .gt. 16) go to 50285
  if (k2 .eq. 7) go to 50285
  if (k1 .eq. 1) go to 50300
50285 if (k2 .gt. 2) go to 50290
  if (k1 .eq. 7) go to 50300
50290 if (i .ne. ifst) go to 50295
  if (k1 .ge. 5 .and. k1 .le. 16) go to 50315
50295 go to 50320
50300 ikill1 = 5
  ikill2 = 50300
  bus2 = alnm1
  bus3 = alnm2
  go to 50900
50315 ikill1 = 6
  ikill2 = 50315
  bus2 = alnm1
  go to 50900
50320 if (k1 .ne. 1 .or. k2 .ne. 7) go to 50355
  el(i) = blank
  el(k) = blank
  if (i .eq. ifst) go to 50330
50325 i = i - 1
  if (el(i) .eq. blank) go to 50325
50330 if (k .eq. ilst) go to 50340
50335 k = k + 1
  if (el(k) .eq. blank) go to 50335
50340 if (i .ne. ifst) go to 50345
  ifst = k
  go to 50200
50345 if (k .ne. ilst) go to 50350
  ilst = i
  ilst1 = ilst
  j = iel(ilst)
  iel(ilst) = 999
  if (j .lt. 0) iel(ilst) = -999
  if (ifst .le. ilst) go to 50200
  ikill1 = 2
  ikill2 = 50345
  go to 50900
50350 j = iel(i)
  iel(i) = k
  if (j .lt. 0) iel(i) = -k
  go to 50235
50355 if (k2 .ne. 2) go to 50380
  if (k1 .ne. 3) go to 50360
  el(k) = opname(4)
  go to 50365
50360 if (k1 .ne. 5) go to 50380
  el(k) = opname(6)
50365 el(i) = blank
  if (i .ne. ifst) go to 50370
  ifst = k
  go to 50375
50370 i = i - 1
  if (el(i) .eq. blank) go to 50370
  i1 = iel(i)
  iel(i) = k
  if (i1 .lt. 0) iel(i) = -k
50375 i = k
  go to 50235
50380 if (k1 .eq. 2) el(i) = opname(4)
  if (k1 .ne. 0) go to 50400
  if (k2 .ne. 0 .and. k2 .ne. 2) go to 50385
  ikill1 = 5
  ikill2 = 50380
  bus2 = alnm1
  bus3 = alnm2
  go to 50900
50385 if (k3 .ne. 0 .or. k2 .ne. 1) go to 50390
  ikill1 = 5
  ikill2 = 50385
  bus2 = alnm1
  bus3 = alnm2
  go to 50900
50390 if (k3 .eq. 0 .or. k2 .eq. 1) go to 50400
  ikill1 = 7
  ikill2 = 50390
  bus2 = alnm1
  go to 50900
  ! :::  insert  '+'  or  '.or.'  where needed  :::
50400 i1 = 0
  i2 = 0
  i3 = k
  i4 = 0
  if (k1 .ne. 0 .or. i .ne. ifst) go to 50405
  if (k3 .eq. 0) go to 50445
  i1 = 1
  go to 50435
50405 if (k1 .ne. 1) go to 50230
  if (i .ne. ifst) go to 50410
  i1 = 1
  i4 = 1
  k = i
  go to 50435
50410 if (k2 .ge. 17) go to 50230
  if (k2 .eq. 2 .or. k2 .eq. 3) go to 50230
  if (k2 .eq. 0) go to 50420
  i1 = 1
  go to 50435
50420 k4 = 0
  do j = 12, 35
     if (alnm2 .eq. supfn(j)) k4 = j
  end do
  if (k4 .ne. 0) go to 50435
50430 k = k + 1
  if (el(k) .eq. blank) go to 50430
  go to 50445
50435 k = k + 1
  if (k .le. ilst) go to 50440
  if (k1 .eq. 0 .and. k3 .ne. 0) go to 50455
  ikill1 = 1
  ikill2 = 50435
  go to 50900
50440 if (el(k) .eq. opname(1)) i1 = i1 + 1
  if (i1 .eq. 0) go to 50445
  if (el(k) .eq. opname(7)) i1 = i1 - 1
  go to 50435
50445 if (el(k) .ne. opname(3) .and. el(k) .ne. opname(4) .and. el(k) .ne. opname(5) .and. el(k) .ne. opname(6)) go to 50450
  i2 = 1
50450 if (i .eq. ifst) go to 50455
  j = iel(i)
  iel(i) = ifree
  if (j .lt. 0) iel(i) = -ifree
  iel(ifree) = i3
  go to 50460
50455 ifst = ifree
  iel(ifree) = i
50460 if (ifree .gt. ilst1) go to 50465
  ikill1 = 8
  ikill2 = 50460
  go to 50900
50465 el(ifree) = opname(17)
  if (i2 .eq. 1) el(ifree) = opname(3)
  ifree = ifree - 1
  if (i4 .eq. 1) go to 50235
  go to 50230
50470 if (k1 .eq. 0 .or. k1 .eq. 7) go to 50475
  ikill1 = 4
  ikill2 = 50470
  bus2 = alnm1
  go to 50900
50475 if (k1 .ne. 0) go to 50485
  if (k3 .eq. 0) go to 50480
  ikill1 = 7
  ikill2 = 50475
  bus2 = alnm1
  go to 50900
50480 if (i .ne. ifst) go to 50485
  i2 = 0
  go to 50455
  ! m28.2453
  ! :::  change  ".nn.expression"  into  ".nn.+expression)"
50485 i = ifst
50490 i = iel(i)
  if (i .lt. 0) i = -i
  if (i .eq. 999) go to 50535
  k1 = 0
  alnm1 = el(i)
  do j = 8, 13
     if (alnm1 .eq. opname(j)) k1 = j
  end do
  if (k1 .eq. 0) go to 50490
  k = i
  i1 = 0
  i3 = 0
50500 i2 = k
  k = iel(k)
  if (k .lt. 0) k = -k
  if (k .eq. 999) go to 50515
  k2 = 0
  alnm2 = el(k)
  do j = 1, 7
     if (alnm2 .eq. opname(j)) k2 = j
  end do
  if (i3 .ne. 0) go to 50510
  i3 = 1
  if (k2 .gt. 2) go to 50510
  el(ifree) = opname( 17)
  go to 50525
50510 if (k2 .eq. 0) go to 50500
  if (k2 .eq. 1) i1 = i1 + 1
  if (i1 .eq. 0) go to 50520
  if (k2 .eq. 7) i1 = i1 - 1
  go to 50500
50515 ilst = ifree
50520 el(ifree) = opname(7)
50525 j = iel(i2)
  iel(i2) = ifree
  if (j .lt. 0) iel(i2) = -ifree
  iel(ifree) = k
  if (ifree .gt. ilst1) go to 50530
  ikill1 = 8
  ikill2 = 50525
  go to 50900
50530 ifree = ifree - 1
  if (i2 .eq. i) go to 50500
  go to 50490
  ! m28.2500
  ! :::  change  "expression.nn."  into  "(expression.nn."
50535 j = ilst
  go to 50550
50540 do j = 1, 100
     if (iel(j) .eq. i) go to 50550
     if (iel(j) .eq. -i) go to 50550
  end do
50550 if (j .eq. ifst) go to 50600
  i = j
  k1 = 0
  alnm1 = el(i)
  do j = 8, 13
     if (alnm1 .eq. opname(j)) k1 = j
  end do
  if (k1 .eq. 0) go to 50540
  i2 = i
  i1 = 0
50560 do k = 1, 100
     if (iel(k) .eq. i2) go to 50570
     if (iel(k) .eq. -i2) go to 50570
  end do
50570 k2 = 0
  alnm2 = el(k)
  i2 = k
  do j = 1, 7
     if (alnm2 .eq. opname(j)) k2 = j
  end do
  if (k2 .ne. 0) go to 50580
  if (k .ne. ifst) go to 50560
  ifst = ifree - 2
  go to 50590
50580 if (k2 .eq. 7) i1 = i1 + 1
  if (i1 .eq. 0) go to 50585
  if (k2 .eq. 1) i1 = i1 - 1
  go to 50560
50585 i2 = iel(k)
  iel(k) = ifree - 2
  if (k2 .eq. 1) go to 50590
  iel (k) = ifree - 1
  go to 50592
50590 iel(ifree - 2) = ifree - 1
  el(ifree - 2) = opname(3)
50592 iel(ifree - 1) = ifree
  iel(ifree) = i2
  el(ifree - 1) = opname(1)
  el(ifree) = opname(17)
  ifree = ifree - 3
  if (k2 .gt. 1) ifree = ifree + 1
  if (ifree .ge. ilst1) go to 50540
  ikill1 = 8
  ikill2 = 50590
  go to 50900
  ! m28.2550
  ! :::  check homology of successive operators  :::
50600 i = ifst
  go to 50610
50605 i = iel( i)
  if (i .lt. 0)  i = -i
  if (i .eq. 999) go to 50645
50610 k1 = 0
  alnm1 = el(i)
  do j = 3, 6
     if (alnm1 .eq. opname(j)) k1 = 2
  end do
  do j = 14, 18
     if (alnm1 .eq. opname(j)) k1 = 1
  end do
  if (k1 .eq. 0) go to 50605
  k = i
  i1 = 0
50625 k = iel(k)
  if (k .lt. 0) k = -k
  if (k .eq. 999) go to 50605
  k2 = 0
  alnm2 = el(k)
  do j = 1, 18
     if (alnm2 .eq. opname(j)) k2 = j
  end do
  if (k2 .eq. 0) go to 50625
  if (k2 .eq. 1) i1 = i1 + 1
  if (i1 .eq. 0) go to 50635
  if (k2 .eq. 7) i1 = i1 - 1
  go to 50625
50635 if (k1 .ne. 1) go to 50640
  if (k2 .lt. 3 .or. k2 .gt. 6) go to 50605
  ikill1 = 9
  ikill2 = 50635
  bus2 = alnm1
  bus3 = alnm2
  go to 50900
50640 if (k2 .lt. 14) go to 50605
  ikill1 = 9
  ikill2 = 50640
  bus2 = alnm1
  bus3 = alnm2
  go to 50900
  ! m28.2594
  ! :::  store 'el' into ivarb, parsup  :::
50645 ndx1 = kjsup + nsup
  insup(ndx1) = karg + 3
  i = ifst
  go to 50655
50650 if (k .eq. 999) go to 50685
  i = k
50655 k = iel( i)
  if (k .gt. 0) go to 50660
  ! : numerical argument
  k = -k
  ivarb(karg + 1) = -1
  kpar = kpar + 1
  ivarb(karg + 3) = kpar
  j = ichar (el(i)(1 : 1))
  ndx1 = kprsup + kpar
  parsup( ndx1) = argel(j)
  go to 50650
50660 alnm1 = el(i)
  do j = 1, 18
     if (alnm1 .eq. opname(j)) go to 50675
  end do
  do j = 12, 35
     if (alnm1 .eq. supfn(j)) go to 50680
  end do
  ! : tacs variable
  ivarb(karg + 1) = 1
  n23 = 0
  call namea6 (alnm1, n23)
  ivarb(karg + 3) = n23
  go to 50650
  !     : operator
50675 karg = karg + 3
  ivarb(karg + 2) = j
  ivarb(karg + 1) = 0
  ivarb(karg + 3) = 0
  go to 50650
  ! : function
50680 ivarb(karg + 1) = 2
  ivarb(karg + 3) = j - 1
  go to 50650
  ! : exit
50685 ndx1 = kksup + nsup
  insup(ndx1) = -karg
  ndx1 = kjsup + nsup
  if (karg - insup( ndx1) .lt. 150) go to 12599
  ikill1 = 8
  ikill2 = 50685
  ! m28.2651
  ! ***  kill  codes  for  fortran  expression  ***
  ! ***  * * * * * * * * * * * * * * * * * * *  ***
50900 kill = 218
  lstat( 19) = ikill2
  lstat( 17) = ikill1
  bus1 = alnode
  go to 9000
  ! m28.2659
  ! ***  old  fixed-format  pseudo-fortran  expression  ***
  ! ***  * * * * * * * * * * * * * * * * * * * * * * *  ***
12517 if (noutpr .eq. 0) write (unit = kunit6, fmt = 9304)
9304 format ('+TACS supplemental variable')
  read (unit = abuff, fmt = 18001) prx, alph(1), dumj(1), dumj(6), alph(2), dumj(2), dumj(7), alph(3), dumj(3), pru, alph(4), dumj(4), dumj(9), alph(5), dumj(5), dumj(10)
18001 format (10x, e10.0, 2(a1, a5, a6), a1, a5, e6.0, 2(a1, a5, a6))
  ndx1 = kjsup + nsup
  insup(ndx1) = karg + 3
  if (prx .eq. 0.0) go to 126
  karg = karg + 3
  ivarb(karg + 1) = -1
  ivarb(karg + 2) = 0
  kpar = kpar + 1
  ivarb(karg + 3) = kpar
  ndx1 = kprsup + kpar
  parsup(ndx1) = prx
126 do i = 1, 5
     if (alph(i) .ne. blank) go to 12601
     ! Previous record fails occasionally for Burroughs EMTP
     ! (see vol. xi  emtp memo,  page dttm-4,  24 dec 1981).
     ! Yet following burroughs patch produces fatal VAX error:
     ! if ( .not. (alph(i) .is. blank) )   go to 12601
     if (i .eq. 3) go to 12602
     if (dumj(i + 5) .eq. blank) go to 125
     if (i .ne. 1) go to 12603
     alph(1) = splus
     go to 12700
12603 lstat(14) = 1
     bus1 = dumj(i + 5)
     go to 12610
12602 if (pru .eq. 0.0) go to 125
     lstat(14) = 2
     flstat(16) = pru
     go to 12610
12601 if (i .ne. 3 .and. dumj(i + 5) .ne. blank) go to 12700
     if (i .eq. 3 .and. pru .ne. 0.0) go to 12700
     lstat(14) = 3
     bus2 = alph(i)
12610 kill = 139
     lstat(19) = 12610
     bus3 = alnode
     go to 9000
12700 karg = karg + 3
     do j = 1, 6
        if (alph(i) .eq. supop(j)) go to 12702
     end do
     kill = 130
     lstat(19) = 12702
     bus1 = alph(i)
     bus2 = alnode
     go to 9000
12702 ivarb(karg + 1) = j
     do j = 1, 35
        if (dumj(i) .eq. supfn(j)) go to 12704
     end do
     kill = 132
     lstat(19) = 12704
     bus1 = dumj(i)
     bus2 = alnode
     go to 9000
12704 ivarb(karg + 2) = j - 1
     if (i .eq. 3) go to 12710
     n23 = 0
     text2 = dumj(i + 5)
     call namea6 (text2, n23)
     ivarb(karg + 3) = n23
     go to 125
12710 ivarb(karg + 1) = - ivarb(karg + 1)
     kpar = kpar + 1
     ivarb(karg + 3) = kpar
     ndx1 = kprsup + kpar
     parsup(ndx1) = pru
  end do
125 continue
  ndx1 = kksup + nsup
  insup(ndx1) = karg
12599 continue
  if (karg - kivarb .lt. lstat(66)) go to 12721
  kill = 122
  lstat(19) = 12721
  lstat(16) = lstat(66)
  lstat(17) = 6
  go to 9000
12721 if (kpar .le. lstat(67)) go to 12722
  kill = 122
  lstat(19) = 12722
  lstat(16) = lstat(67)
  lstat(17) = 7
  go to 9000
12722 ndx1 = kjsup + nsup
  if (karg .ge. insup(ndx1)) go to 9000
  kill = 133
  bus1 = alnode
  lstat(19) = 104
  go to 9000
10700 if (noutpr .eq. 0) write (unit = kunit6, fmt = 19305) m
19305 format ('+TACS supplemental device type ', i2)
  read (unit = abuff, fmt = 18002) alph(1), dumj(1), alph(2), dumj(2), alph(3), dumj(3), alph(4), dumj(4), alph(5), dumj(5), (dum(i), i = 1, 3), alnm1, alnm2
18002 format (10x, 5(a1, a6, 1x), 3e6.0, 2a6)
  if (m .ge. 50) go to 10701
  kill = 134
  lstat(19) = 10701
  bus1 = alnode
  lstat(14) = m
  go to 9000
10701 karg = karg + 4
  if ((karg - kivarb + 4) .le. lstat(66)) go to 10703
  kill = 122
  lstat(16) = lstat(66)
  lstat(17) = 6
  lstat(19) = 10703
  go to 9000
10703 ndx1 = kjsup + nsup
  insup(ndx1) = -karg
  ndx1 = kksup + nsup
  insup(ndx1) = m
  ivarb(karg) = kprsup + kpar + 1
  ivarb(karg + 2) = nsudv
  n1 = 0
  do i = 1, 5
     if (m .eq. 61 .or. m .eq. 63) go to 1107
     if (dumj(i) .eq. blank) go to 1071
     n1 = n1 + 1
1107 if (nsudv .gt. nsu) go to 1072
     kill = 122
     lstat(16) = lstat(63)
     lstat(17) = 3
     lstat(19) = 1072
     go to 9000
1072 ndx1 = kalksu + nsudv
     n23 = 0
     text2 = dumj(i)
     call namea6 (text2, n23)
     ksus(ndx1) = n23
     ndx1 = kksus + nsudv
     ksus(ndx1) = 1
     if (alph(i) .eq. sminus) ksus(ndx1) = -1
     if (alph(i) .eq. smultp) ksus(ndx1) = 9
     nsudv = nsudv - 1
  end do
1071 continue
  if (n1 .gt. 0) go to 1073
  if (m .eq. 61 .or. m .eq. 63) go to 1073
  kill = 135
  bus1 = alnode
  lstat(19) = 1073
  go to 9000
1073 ivarb(karg + 1) = nsudv + 1
  mpar = kprsup + kpar
  parsup(mpar + 1) = dum(1)
  parsup(mpar + 2) = dum(2)
  parsup(mpar + 3) = dum(3)
  n23 = -9999
  if (alnm1 .eq. text4) go to 1177
  n23 = 0
  call namea6 (alnm1, n23)
1177 ivarb(karg + 3) = n23
  n23 = 0
  call namea6 (alnm2, n23)
  ivarb(karg + 4) = n23
  if (m .gt. 67) go to 6799
  moon = m - 49
  select case (moon)
  case (1)
     go to 650

  case (2 : 3)
     go to 651

  case (4)
     go to 653

  case (5)
     go to 654

  case (6 : 8)
     go to 655

  case (9)
     go to 658

  case (10)
     go to 659

  case (11)
     go to 660

  case (12)
     go to 661

  case (13)
     go to 662

  case (14 : 15)
     go to 663

  case (16)
     go to 6511

  case (17)
     go to 666

  case (18)
     go to 667
  end select
650 ivarb(karg + 3) = 0
  kpar = kpar + 4
  parsup(mpar + 4) = dum(2)
  if (dum(2) .eq. 0.0) parsup(mpar + 4) = 0.5
  go to 3636
651 if (dum(3) .eq. 0.0) parsup(mpar + 3) = 2.0
6511 kpar = kpar + 3
3636 karg = karg + 1
  if (kpar .le. lstat(67)) go to 9000
  kill = 122
  lstat(16) = lstat(67)
  lstat(17) = 7
  lstat(19) = 3636
  go to 9000
653 if (alnm1 .eq. blank) parsup(mpar + 3) = parsup(mpar + 2)
  d9 = parsup(mpar + 3) / deltat - flzero * 10.
  i = 0
10706 i = i + 1
  d10 = i
  if (d9 .gt. d10) go to 10706
  parsup(mpar + 3) = d10
  kpar = kpar + 5
  parsup(mpar + 1) = kpar
  parsup(mpar + 4) = 0.0
  ivarb(karg + 4) = - kpar
  n6 = kpar + i - 1
  !     wsm patch for dan goldsworth type-53 history.  27 mar 83
  if (alnm2 .eq. text1) go to 1707
  parsup(mpar + 4) = dum(1)
  dum(1) = 0.0
1707 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 8841) alnm2
8841 format (' Test delay line i.c., value =', a6)
  do i1 = kpar, n6
     ndx6 = kprsup + i1
     parsup(ndx6) = dum(1)
  end do
  kpar = n6
  go to 3636
654 if (parsup(mpar + 1) .eq. 0.0) parsup(mpar + 1) = -9999.
  if (parsup(mpar + 3) .eq. 0.0) parsup(mpar + 3) = -9999.
  go to 6511
655 ivarb(karg + 3) = -kpar - 4
  kpar = kpar + 3
  ! read input card using cimage
10711 call cimage
  if (kolbeg .gt. 0) go to 6544
  read (unit = abuff, fmt = 10712) prx, pru
10712 format (2e16.0)
  go to 6547
6544 nfrfld = 1
  !  call frefld (voltbc)
  call free (voltbc)
  prx = voltbc(1)
  if (prx .eq. 9999.) go to 10713
  !  call frefld (voltbc)
  call free (voltbc)
  pru = voltbc(1)
6547 if (prx .eq. 9999.) go to 10713
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 10715) prx, pru
10715 format ('+Data values.             ', 2e13.4)
  kpar = kpar + 1
  ndx1 = kprsup + kpar
  parsup(ndx1) = prx
  if (m .eq. 55 .or. m .eq. 57) go to 10711
  kpar = kpar + 1
  ndx1 = kprsup +  kpar
  parsup(ndx1) = pru
  go to 10711
10713 ivarb(karg + 4) = -kpar
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 10716)
10716 format ('+End of data values for last-read device.')
  if (m .eq. 57) parsup(mpar + 2) = 0.
  if (m .eq. 57) parsup(mpar + 3) = 0.
  go to  3636
658 if (parsup(mpar + 1) .eq. 0.0) parsup(mpar + 1) = 1.0
  parsup(mpar + 3) = 2.0 / deltat * parsup(mpar + 3) / parsup(mpar + 1)
  parsup(mpar + 1) = parsup(mpar + 2) / parsup(mpar + 1)
  parsup(mpar + 2) = parsup(mpar + 1) + parsup(mpar + 3)
  parsup(mpar + 3) = parsup(mpar + 1) - parsup(mpar + 3)
  if (parsup(mpar + 2) .ne. 0.0 .or. alnm1 .eq. text4) go to 6511
  kill = 214
  bus1 = alnode
  lstat(19) = 10714
  go to 9000
659 if (parsup(mpar + 1) .eq. 0.0) parsup(mpar + 1) = 1.0
  parsup(mpar + 1) = parsup(mpar + 1) / deltat
  go to 6511
660 if (alnm1 .eq. blank) go to 10720
  if (dumj(1) .eq. blank) go to 10720
  if (dumj(2) .eq. blank) go to 10720
  if (dumj(3) .eq. blank) go to 10720
  if (dumj(4) .ne. blank) go to 10720
  if (dumj(5) .ne. blank) go to 10720
  kpar = kpar + 2
  go to 3636
10720 kill = 215
  bus1 = alnode
  lstat(19) = 10720
  go to 9000
661 if (alnm2 .ne. blank) go to 6511
  kill = 216
  bus1 = alnode
  lstat( 19) = 10721
  go to 9000
662 parsup(mpar + 1) = 0.0
  go to 6511
663 if (dum(2) .eq. -1.0 .or. dum(2) .eq. 1.0) go to 6511
  kill = 217
  bus1 = alnode
  lstat(19) = 10723
  lstat(17) = m
  flstat(14) = dum( 2)
  go to 9000
666 parsup(mpar + 1) = dum(1) * deltat
  ivarb(karg + 3) = int (-1.0 / parsup(mpar + 1) - 0.5)
  ivarb(karg + 4) = -1
  kpar = kpar - ivarb(karg + 3) + 1
  if (alnm2 .ne. text1) dum(2) = 0.0
  j1 = mpar + 2
  j2 = mpar + n + 1
  do j = j1, j2
     parsup(j) = dum(2)
  end do
  go to 3636
667 parsup(mpar + 2) = parsup(mpar + 2) * dum(1)
  parsup(mpar + 3) = parsup(mpar + 3) * dum(1)
  kpar = kpar + 4
  go to 3636
6799 if (noutpr .eq. 0) write (unit = kunit6, fmt = 1104)
1104 format ('+No such sup. device.')
9000 if (iprsup .ge.  1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module tacs1a."')
  return
end subroutine tacs1a

!
! subroutine tacs1b.
!

subroutine tacs1b
  use blkcom
  use labcom
  use tacsar
  use bcddat
  use bcdtim
  use movcop
  implicit none
  integer(4) :: i, ij, ij9, ijk, infir, inlst, irr, is, isour, iuser, izs
  integer(4) :: j, j1, j2, j8, j11, j12, j13, jc, jj, jjj, jk, jk1, jk2, jl, jm, jn
  integer(4) :: jni, jr
  integer(4) :: k, k1, k2, karg, kargsa, kbtcs, kbwkcs, kint, kjsup, kksup, kxai
  integer(4) :: kxtcs2
  integer(4) :: l, lc, ll0, ll1, ll2, ll3, llm1
  integer(4) :: m, mc, min, mins, mjump, mkk, mm, mmm, mnp, mpk, mpp, mpq
  integer(4) :: n, n1, n2, n3, n4, n6, n11, n12, n23, namexm, namin, namout, namsbk
  integer(4) :: namsup, ndx1, ndx2, ndx3, ndxb, ndy, nexd, nfun, ngp, nik, nivarb
  integer(4) :: nj, nkn, nn, nnn, nom, np, np1, nq, nstep, nuki, nukj, nukjl, nukk
  integer(4) :: nukl, nukm, nukn, nukq, nukr
  real(8) :: dn
  !dimension dumj(13)
  !character(8) dumj, delay
  character(8) :: delay
  !
  data  delay / 'delay' /
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module tacs1b."')
  ll0 = 0
  ll1 = 1
  ll2 = 2
  ll3 = 3
  llm1 = -1
  isour = lstat(64)
  kjsup = kinsup + lstat(65)
  kksup = kjsup + lstat(65)
  kxai  = kxar + lstat(68)
  kbtcs = katcs + lstat(62)
  kbwkcs = kawkcs + lstat(68)
  if (nchain .eq. 1) go to 5566
  i = nsup
  go to 5577
5566 nuki = kisblk - 8
  nexd = 1
  kargsa = karg + 3
  iuty(kiuty + 7) = karg + 4
  iprsov(35) = -8888
  ndxb = nuk + 1
  mc = 1
  mins = 0
  iuser = 0
  ! $$$  ordering  $$$
  kint = -1000
  go to 1555
4747 if (iabs (ia) .ne. 1) go to 1984
  i = 1
  iuser = 9999
  if (ia .le. 0) nuki = nuki + nuk * 8 + 8
555 nuki = nuki + 8
  if (ia .lt. 0) nuki = nuki - 16
  go to 6677
2555 i = i + 1
  if (i .le. nuk) go to 555
  go to 2254
1984 mpq = 0
  mpp = 0
  mkk = 0
  ngp = 0
  kint = 1000
1900 mpk = 0
  i = 1
  nuki = kisblk - 8
1910 nuki = nuki + 8
  if (i .gt. nuk) go to 8103
  if (isblk(nuki + 4) .gt. 0) go to 1950
  infir = iabs (isblk(nuki + 1))
  inlst = iabs (isblk(nuki + 9)) - 1
  if (i .eq. nuk) inlst = nsu
  namout = int (atcs(katcs + i))
  izs = isblk(nuki + 2)
  if (mpq .eq. 0 .or. izs .eq. 1) go to 1911
  j = isblk(nuki + 3)
  ndx1 = kprsup + j
  ndx2 = ndx1 + 1
  if (parsup(ndx2) .eq. 0) go to 1911
  m = izs * 6 + j - 4
  dn = -parsup(ndx1) / parsup(ndx2)
  do l = j, m, 6
     dn = -dn
     ndx1 = kprsup + l
     if (parsup(ndx1) .ne. parsup(ndx1 + 1) * dn) go to 1911
     if (parsup(ndx2) * parsup(ndx1 + 1) .lt. 0) go to 1911
  end do
  go to 1983
1911 do j = infir, inlst
     namin = ksus(kalksu + j)
     do k = 1, niu
        if (namin .eq. ilntab(kaliu + k)) go to 1940
     end do
     do k = 1, nsup
        ndx2 = kspvar + k
        if (namin .ne. ilntab(ndx2)) go to 1920
        if (insup(kinsup + k) .ne. 0) go to 1940
        go to 1950
     end do
1920 continue
     do k = 1, nuk
        namsbk = int (atcs(katcs + k))
        if (namin .ne. namsbk) go to 1919
        ndx1 = kisblk + k * 8 - 4
        if (isblk(ndx1) .gt. 0) go to 1940
        go to 1950
     end do
1919 continue
  end do
1940 continue
1983 mpk = 1
6677 ndxb = ndxb - 1
  isblk(nuki + 4) = ndxb
  ndy = kisblk + ndxb * 8 - 1
  isblk(ndy) = 0
1555 if (mins .eq. nsup) go to 1950
  nnn = 0
4646 jjj = 0
  mmm = 0
  mm = mc
  do is = mm, nsup
     if (insup(kinsup + is) .ne. 0) go to 1980
     n1 = insup(kjsup + is)
     n3 = insup(kksup + is)
     n2 = iabs(n3)
     nstep = 1
     if (n1 .lt. 0) go to 1970
     j  = n1
     k2 = n2
     nstep = 3
1973 if (n1 .lt. 0) go to 2973
     n4 = ivarb(j + 1)
     if (n4 .lt. 0) go to 1959
     if (n3 .le. 0 .and. n4 .ne. 1) go to 1959
     namsup = ivarb(j + 3)
     go to 1853
2973 namsup = ksus(kalksu + j)
1853 do k = 1, nuk
        namsbk = int (atcs(katcs + k))
        if (namsbk .ne. namsup) go to 1951
        ndx1 = kisblk + k * 8 - 4
        if (isblk(ndx1) .gt. 0) go to 1959
        go to 1990
     end do
1951 continue
     do k = 1, nsup
        if (ilntab(kspvar + k) .ne. namsup) go to 1953
        if (insup(kinsup + k) .ne. 0) go to 1959
        go to 1990
     end do
1953 continue
1959 j = j + nstep
     if (j .le. k2) go to 1973
     if (n1 .gt. 0) go to 1978
     if (j .gt. k2 + 1) go to 1851
     namsup = ivarb(nn + 3)
     if (namsup .gt. 1) go to 1853
     go to 1959
1851 if (j .gt. k2 + 2) go to 1978
     namsup = ivarb(nn + 4)
     if (namsup .gt. 1) go to 1853
1978 mmm = 1
     if (ndxb .le. nuk .or. kint .ne. -1000) go to 4848
     iuty(kiuty + 4) = is
     kint = -100
     go to 4949
4848 if (nnn .eq. 0) isblk(ndy) = is
     if (nnn .ne. 0) insup(kinsup+nnn) = is
4949 nnn = is
     insup(kinsup + is) = -1
     mins = mins + 1
     go to 1980
1970 if (n2 .eq. 53 .and. ivarb(nn + 3) .eq. 1) go to 1978
     nn = - n1
     k2 = ivarb(nn + 2)
     j = ivarb(nn + 1)
     go to 1973
1990 if (jjj .eq. 1) go to 1980
     jjj = 1
     mc = is
  end do
1980 continue
  if (mmm .eq. 1 .and. mins .lt. nsup) go to 4646
1950 if (iuser .eq. 9999) go to 2555
  if (ndxb .eq. 1) go to 2254
  if (kint .lt. 0) go to 4747
  if (i .eq. 0) go to 1900
  i = i + 1
  go to 1910
7104 kill = 122
  lstat(16) = lstat(68)
  lstat(17) = 8
  lstat(19) = 7104
  go to 9000
777 nukr = krsblk + nuk * 4
  write (unit = lunit(6), fmt = 7171)
7171 format (' --------- More than one limiter in the Zs loop insert a delay s-block to decouple the loop')
  nuki = kisblk - 4
  do lc = 1, nuk
     nuki = nuki + 8
     if (isblk(nuki) .gt. 0) isblk(nuki) = isblk(nuki) + 1
  end do
  nuk = nuk + 1
  if (nuk .le. lstat(61)) go to 1347
  kill = 122
  lstat(16) = lstat(61)
  lstat(17) = 1
  lstat(19) = 1347
  go to 9000
1347 nsu = nsu + 1
  if (nsu .le. nsudv) go to 1357
  kill = 122
  lstat(16) = lstat(63)
  lstat(17) = 3
  lstat(19) = 1357
  go to 9000
1357 j = kpar + 1
  ndx1 = kprsup + kpar
  kpar = kpar + 10
  if (kpar .le. lstat(67)) go to 1367
  kill = 122
  lstat(16) = lstat(67)
  lstat(17) = 7
  lstat(19) = 1367
  go to 9000
1367 nuki = nuki + 4
  isblk(nuki + 1) = nsu
  isblk(nuki + 2) = 2
  isblk(nuki + 3) =  j
  isblk(nuki + 4) = ndxb
  call nmincr (delay, nexd)
  nexd = nexd + 1
  n23 = 0
  call namea6 (delay, n23)
  atcs(katcs + nuk) = n23
  ksus(kksus + nsu) = 1
  ksus(kalksu + nsu) = ksus(kalksu + k)
  ksus(kalksu + k) = n23
  rsblk(nukr + 4) = 0.0
  xar(kxar + nuk) = 0.0
  xar(kxai + nuk) = 0.0
  awkcs(kawkcs + nuk) = 1.0
  awkcs(kbwkcs + nuk) = 1.0
  parsup(ndx1 + 1) = 1.0
  parsup(ndx1 + 2) = 1.0
  parsup(ndx1 + 3) = 1.0
  parsup(ndx1 + 4) = 1.0
  parsup(ndx1 + 5) = 0.0
  parsup(ndx1 + 6) = 0.0
  parsup(ndx1 + 7) = -deltat / 2.0
  parsup(ndx1 + 8) = -parsup(ndx1 + 5)
  parsup(ndx1 + 9) = parsup(ndx1 + 7)
  parsup(ndx1 + 10) = parsup(ndx1 + 8)
  do jc = 2, nuk
     isblk(nuki + 7) = isblk(nuki - 1)
     nuki = nuki - 8
  end do
  mkk = 0
  ndy = kisblk + ndxb * 8 - 1
  isblk(ndy) = 0
  go to 1900
8103 mpq = 0
  if (mpk .eq. 1) go to 1900
  if (ngp .gt. 0) go to 8811
  if (mkk .gt. 0) go to 1997
  if (mpp .eq. 1) go to 1996
  mpq = 1
  mpp = 1
  go to 1900
1996 nuki = kisblk - 8
  do i = 1, nuk
     nuki = nuki + 8
     if (isblk(nuki + 4) .ge. 0) go to 1414
     namout = int (atcs(katcs + i))
     np =  1
     atcs(kbtcs + np) = namout
     isblk(kisblk + 8) = 0
     j11 = kisblk + 5
     j12 = j11 + nuk * 8 - 2
     do j13 = j11, j12, 8
        isblk(j13) = 0
        isblk(j13 + 1) = 0
     end do
     isblk(kisblk + 5) = i
     mnp = 1
     nukm = kisblk
1444 nq = np
     nukq = kisblk + nq * 8 - 8
     j = 1
     nukj = kisblk - 8
1427 nukj = nukj + 8
     if ( isblk(nukj + 4) .gt. 0) go to 1429
     n11 = iabs (isblk(nukj + 1))
     n12 = iabs (isblk(nukj + 9)) - 1
     if (j .eq. nuk) n12 = nsu
     namsbk = int (atcs(katcs + j))
     nik = 0
     do k = n11, n12
        namin = ksus(kalksu + k)
        l = 1
        nukl = kisblk - 8
1423    nukl = nukl + 8
        namexm = int (atcs(kbtcs + l))
        if ( namin .ne. namexm) go to 1425
        if (isblk(nukj + 6) .gt. 0) go to 1429
        irr = isblk(nukl + 8)
        if (irr .eq. 1 .and. namsbk .eq. namout) go to 777
        if (namsbk .eq. namout) go to 1421
        if (isblk(nukj + 4) .lt. 0 .or. irr .eq. 1) isblk(nukj + 6) = 1
        if (isblk(nukj + 6) .lt. 0) go to 1421
        if (nik .eq. 1)  go to 1474
        nik = 1
        nq = nq + 1
        nukq = nukq + 8
        atcs(kbtcs + nq) = namsbk
        if (isblk(nukj + 6) .eq. 0) isblk(nukj + 6) = -1
1474    isblk(nukq + 8) = isblk(nukj + 6)
        if (isblk(nukj + 6) .lt. 0) go to 1421
        if (isblk(nukj + 4) .lt. 0) go to 1616
        go to 1429
1616    mnp = mnp + 1
        nukm = nukm + 8
        isblk(nukm + 5) = j
        go to 1429
1425    l = l + 1
        if (l .le. np) go to 1423
     end do
1421 continue
1429 j = j + 1
     if (j .le. nuk) go to 1427
     if (nq .eq. np) go to 1714
     nj = nq - np
     np1 = np * 8
     nukj = kisblk - 8
     do j = 1, nj
        nukj = nukj + 8
        atcs(kbtcs + j) = atcs(kbtcs + j + np)
        isblk(nukj + 8) = isblk(nukj + 8 + np1)
     end do
     np = nj
     go to 1444
1714 if (mkk .ne. 0) go to 1814
     jk2 = kcolcs + lstat(62)
     mkk = jk2 - mnp
     nukm = kisblk - 8
     do jj = 1, mnp
        nukm = nukm + 8
        icolcs(mkk + jj) = isblk(nukm + 5)
     end do
     go to 1414
1814 jk1 = mkk + 1
     ijk = 0
     nukm = kisblk - 8
     do jj = 1, mnp
        nukm = nukm + 8
        do jk = jk1, jk2
           if (icolcs(jk) .ne. isblk(nukm + 5)) go to 1861
           if (jj .eq. 1) go to 1878
           isblk(nukm + 5) = 0
           ijk = ijk + 1
           go to 1840
        end do
1861    continue
     end do
1840 continue
     if (ijk .ne. 0) go to 1888
     mkk = mkk - mnp
     nukk = kisblk - 3
     do jl = 1, mnp
        nukk = nukk + 8
        icolcs(mkk + jl) = isblk(nukk)
     end do
     go to 1414
1878 if (mnp .eq. 1) go to 1414
     jjj = jk
     nukjl = kisblk - 8
     do jl = 2, mnp
        nukjl = nukjl + 8
        do jm = jk1, jk2
           if (icolcs(jm) .ne. isblk(nukjl+5)) go to 1897
           if (jm .gt. jjj) go to 1898
           jjj = jm
           go to 1898
        end do
1897    continue
        call stoptp
     end do
1898 continue
     if (jjj .eq. jk) go to 1414
     mnp = icolcs(jk)
     nnn = jk - jjj
     do jn = 1, nnn
        jni = jk - jn + 1
        icolcs(jni) = icolcs(jni - 1)
     end do
     icolcs(jjj) = mnp
     go to 1414
1888 mkk = mkk - mnp + ijk
     mjump = mkk
     nukm = kisblk - 8
     do jl = 1,mnp
        nukm = nukm + 8
        if (isblk(nukm + 5) .eq. 0) go to 1893
        mjump = mjump + 1
        icolcs(mjump) = isblk(nukm + 5)
     end do
1893 continue
  end do
1414 continue
  if (mkk .eq. 0) go to 8811
1997 mkk = mkk + 1
  if (mkk .gt. jk2) go to 8811
  nfun = icolcs(mkk)
  nuki = kisblk + nfun * 8 - 8
  if (isblk(nuki + 4) .gt. 0) go to 1997
4141 infir = iabs(isblk(nuki + 1))
  inlst = iabs(isblk(nuki + 9)) - 1
  if (nfun .eq. nuk) inlst = nsu
  nom = 0
  do j1 = infir, inlst
     namin = ksus(kalksu + j1)
     do j2 = 1, nsup
        ndx2 = kspvar + j2
        if (namin .ne. ilntab(ndx2)) go to 4667
        if (insup(kinsup + j2) .ne. 0) go to 4556
        if (nom .gt. 0) go to 4343
        nom = 1
        kargsa = kargsa + 1
        ivarb(kargsa) = -nfun
        isblk(nuki + 1) = -isblk(nuki + 1)
4343    kargsa = kargsa + 1
        ivarb(kargsa) = j2
        iuty(kiuty+8) = kargsa
        if (kargsa - kivarb .lt. lstat(66)) go to 4556
        kill = 122
        lstat(19) = 4343
        lstat(16) = lstat(66)
        lstat(17) = 6
        go to 9000
     end do
4667 continue
  end do
4556 continue
  i = 0
  go to 6677
8811 ngp = 0
  i = 0
  nukj = kisblk - 8
  do jj = 1, nuk
     nukj = nukj + 8
     if (isblk(nukj + 4) .gt. 0) go to 8822
     ij9 = iabs(isblk(nukj + 9))
     if (jj .eq. nuk) ij9 = nsu + 1
     ij = ij9 - iabs (isblk(nukj + 1))
     if (ij .le. ngp) go to 8822
     ngp = ij
     nfun = jj
  end do
8822 continue
  nuki = kisblk + nfun * 8 - 8
  go to 4141
2254 if (mins .lt. nsup) go to 9200
  nj = lstat(64) + nsup
  ktab = nuk + nj
  if (ktab .gt. lstat(68)) go to 7104
  kofsce = ktab
  kaliu = klntab + nuk
  kspvar = kaliu + lstat(64)
  ndx1 = klntab + ktab + 1
  ndx2 = klntab + nj + 1
  do i = 1, nj
     ilntab(ndx1 - i) = ilntab(ndx2 - i)
  end do
  nuki = kisblk - 8
  do i = 1, nuk
     nuki = nuki + 8
     k = klntab + isblk(nuki + 4)
     ilntab(k) = int (atcs(katcs + i))
  end do
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 8844) (ilntab(klntab + i), i = 1, nuk)
8844 format ('  $$$ ilntab $$$ =', /, (5x, 15i8))
  ! $$$  form tables  $$$ m37.1855
  if (nsup .eq. 0) go to 21100
  i = 1
5577 j = insup(kjsup + i)
  k = insup(kksup + i)
  if (j .lt. 0) go to 2116
  k1 = k
  if (k .lt. 0) k = -k
  do n = j, k, 3
     if (ivarb(n + 1) .lt. 0) go to 2113
     if (k1 .lt. 0 .and. ivarb(n + 1) .ne. 1) go to 2113
     nivarb = ivarb(n + 3)
     do m = 1, ktab
        ndx2 = klntab + m
        if (nivarb .eq. ilntab(ndx2)) go to 2175
     end do
     kill = 143
     bus1 = texvec(nivarb)
     bus2 = texvec(ilntab(kspvar + i))
     lstat(19) = 2115
     go to 9000
2175 ivarb(n + 3) = m
  end do
2113 continue
  go to 2111
2116 j = - j
  if (ivarb(j + 4) .gt. 1) go to 7162
  if (ivarb(j + 4) .eq. 1) ivarb(j + 4) = 0
  ivarb(j + 4) = -ivarb(j + 4)
  go to 7165
7162 do m = 1, ktab
     ndx2 = klntab + m
     if (ivarb(j + 4) .eq. ilntab(ndx2)) go to 2719
  end do
  kill = 143
  bus1 = texvec(ivarb(j + 4))
  bus2 = texvec(ilntab(kspvar + i))
  lstat(19) = 2119
  go to 9000
2719 ivarb(j + 4) = m
7165 if (ivarb(j + 3) .gt. 1 .or. ivarb(j + 3) .eq. -9999) go to 7163
  if (ivarb(j + 3) .eq. 1) ivarb(j + 3) = 0
  ivarb(j + 3) = -ivarb(j + 3)
  go to 7161
7163 do m = 1, ktab
     ndx2 = klntab + m
     if (ivarb(j + 3) .eq. ilntab(ndx2)) go to 2199
  end do
  if (k .eq. 58 .and. ivarb(j + 3) .eq. -9999) go to 7161
  kill = 143
  bus1 = texvec(ivarb(j + 3))
  bus2 = texvec(ilntab(kspvar + i))
  lstat(19) = 2192
  go to 9000
2199 ivarb(j + 3) = m
7161 jk = ivarb(j + 2)
  jm = ivarb(j + 1)
  do n = jm, jk
     ndx1 = kalksu + n
     if (ksus(ndx1) .ne. 1) go to 1171
     ksus(ndx1) = 0
     go to 1170
1171 do m = 1, ktab
        ndx2 = klntab + m
        if (ksus(ndx1) .eq. ilntab(ndx2)) go to 1199
     end do
     kill = 143
     bus1 = texvec(ksus(ndx1))
     bus2 = texvec(ilntab(kspvar + i))
     lstat( 19) = 1190
     go to 9000
1199 ksus(ndx1) = m
  end do
1170 continue
  if (k .ne. 50) go to 2111
  nn = ivarb(j)
  mm = kxtcs + nuk + i + lstat(64)
  xtcs(mm) = parsup(nn)
2111 i = i + 1
  if (i .le. nsup) go to 5577
  if (nchain .ne. 18) go to  21100
  ndx1 = kinsup + nsup
  insup(ndx1) = - 1
  if (nsup .eq. konsup + 1) go to 9000
  insup(ndx1 - 1) = nsup
  go to 9000
9200 kill = 500
  write (unit = lunit(6), fmt = 9202)
9202 format ('  The case will be killed because there are some loops in supplemental variables or devices.', /, &
          '  This is not allowed.    The looped supplemental blocks are as follows :')
  j = 0
  do i = 1, nsup
     if (insup(kinsup + i) .ne. 0) go to 9208
     j = j + 1
     ndx1 = ilntab(kspvar + i)
     write (unit = lunit(6), fmt = 9204) j, texvec(ndx1)
9204 format ('  ********', 2x, i5, 4x, a6, '  ********')
  end do
9208 continue
  go to 9000
21100 do i = 1, nsu
     ndx3 = kalksu + i
     do j = 1, ktab
        ndx2 = klntab + j
        if (ksus(ndx3) .eq. ilntab(ndx2)) go to 213
     end do
     nukk = kisblk - 8
     do k = 1, nuk
        nukk = nukk + 8
        ndx1 = iabs (isblk(nukk + 1))
        ndx2 = iabs (isblk(nukk + 9)) - 1
        if (k .eq. nuk) ndx2 = nsu
        if (i .ge. ndx1 .and. i .le. ndx2) go to 469
     end do
     call stoptp
469  k = isblk(nukk+4)
     kill = 121
     lstat(19) = 472
     bus1 = texvec(ksus(ndx3))
     ndx1 = klntab + k
     bus2 = texvec(ilntab(klntab + k))
     write (unit = lunit(6), fmt = 497) bus1, bus2
497  format ('0 Error. Entry =', "'", a6, "'", '= referenced in function =', "'", a6, "'", '= is undefined.')
     go to 9000
213  ksus(ndx3) = j
  end do
  nuki = kisblk - 8
  do i = 1, nuk
     nuki = nuki + 8
     m = isblk(nuki + 4)
     nukm = kisblk + m * 8 - 8
     nukr = krsblk + m * 4 - 4
     isblk(nukm + 5) = 0
     isblk(nukm + 6) = 0
     rsblk(nukr + 2) = -fltinf
     rsblk(nukr + 3) = fltinf
     do l = 1, 2
        if (l .eq. 2) go to 50
        n6 = int (awkcs(kawkcs + i))
        go to 51
50      n6 = int (awkcs(kbwkcs + i))
51      if (n6 .eq. 1) go to 20
        do j = 1, ktab
           if (n6 .eq. ilntab(klntab + j)) go to 40
        end do
        kill = 131
        bus1 = texvec(ilntab( klntab + m))
        bus6 = texvec(n6)
        lstat(14) = l
        lstat(19) = 30
        write (unit = lunit(6), fmt = 99) bus6, bus1
99      format (' Error.  Name of limit  ', a6, '  at block = ', a6, '  is unidentifiable.')
        go to 9000
40      if (l .eq. 2) go to 41
        isblk(nukm + 5) = j
        go to 20
41      isblk(nukm + 6) = j
     end do
20   continue
     if (xar(kxar + i) .eq. 0.0 .and. xar(kxai + i) .eq. 0.0) go to 10
     rsblk(nukr + 2) = xar(kxar + i)
     rsblk(nukr + 3) = xar(kxai + i)
  end do
10 continue
  if (iprsup .lt. 2) go to 1006
  write (unit = lunit(6), fmt = 2007)
2007 format ('         n    ifls   ilim1   ilim2   nuksp')
  nukn = kisblk - 8
  do  n = 1, nuk
     nukn = nukn + 8
     write (unit = lunit(6), fmt = 2010) n, isblk(nukn + 4), isblk(nukn + 5), isblk(nukn + 6), isblk(nukn + 7)
  end do
2010 format (2x, 5i8)
  write (unit = lunit(6), fmt = 2013) (n, ksus(kalksu + n), n = 1, nsu)
2013 format ('0', 17x, 'ksu', /, (2x, 2i8))
  if (nsup .eq. 0) go to 1012
  write (unit = lunit(6), fmt = 1033) karg
1033 format ('  karg = ', i8, /, '       n  iopsup  ifnsup  irgsup    idev     kdj     kdk  ildev1  ildev2')
  do i = 1, nsup
     n1 = insup(kjsup + i)
     if (n1 .lt. 0) go to 2014
     n2 = insup(kksup + i)
     write (unit = lunit(6), fmt = 2008) (n, ivarb(n + 1), ivarb(n + 2), ivarb(n + 2), n = n1, n2, 3)
2008 format (4i8)
     go to 2034
2014 n1 = -n1
     write (unit = lunit(6), fmt = 2022) n1, ivarb(n1), ivarb(n1 + 1), ivarb(n1 + 2), ivarb(n1 + 3), ivarb(n1 + 4)
2022 format (i8, 24x, 5i8)
  end do
2034 continue
  if (nsudv .lt. lstat(63)) write (unit = lunit(6), fmt = 2015) (n, ksus(kalksu + n), ksus(kksus + n), n = nsudv + 1, lstat(63))
2015 format ('0', 11x, 'alksu    ksus', /, (1x, 3i8))
1012 write (unit = lunit(6), fmt = 2017) ktab, (n, ilntab(klntab + n), n = 1, ktab)
2017 format ('0 ktab = ',i5, /, 9x, ' ilntab ', /, (1x, 2i8))
  write (unit = lunit(6), fmt = 2018) nuk
2018 format ('   nuk  = ',i6)
1006 n2 = 0
  min = nuk + niu
  nkn = nuk + lstat(64)
  do k = 1, ktab
     if (k .gt. min .and. k .le. nkn) go to 2046
     ndx1 = klntab + k
     n6 = ilntab(klntab + k)
     n1 = k + 1
     do j = n1, ktab
        ndx1 = klntab + j
        if (ilntab(ndx1) .ne. n6) go to 2045
        n2 = n2 + 1
        bus6 = texvec(n6)
        write (unit = lunit(6), fmt = 2041) k, j, bus6
2041    format (/, ' Entries number', i5, '   and',  i5, '   of the tacs name-vector both have contents  ', "'", a6,  "'", ' .')
     end do
2045 continue
  end do
2046 continue
  if (n2 .eq. 0) go to 2072
  kill = 189
  lstat(15) = n2
  lstat(19) = 2059
  go to 9000
  ! *****  output request  *****               m38.1007
2072 if (iuty(kiuty + 6) .ne. 9999) go to 153
  ioutcs = 0
  n1 = nuk + niu
  n2 = nuk + lstat(64)
  do i = 1, ktab
     if (i .gt. n1 .and. i .le. n2) go to 157
     ioutcs = ioutcs + 1
     ndx1 = kjout  + ioutcs
     jout(ndx1) = i
  end do
157 continue
  go to 3072
153 jr = 0
  if (ioutcs .le. 0) go to 3712
  do i = 1, ioutcs
     j8 = jout(kjout + i)
     do j = 1, ktab
        if (j8 .eq. ilntab(klntab + j)) go to 229
     end do
     if (noutpr .eq. 0) write (unit = lunit(6), fmt = 232) texvec(j8)
232  format (/, '   The user has requested a tacs variable named "', a6, '" for output purposes. But this is a non-existant tacs', /, &
          ' variable, so the request will be disregarded.')
     go to 109
229  jr = jr + 1
     ndx1 = kjout + jr
     jout(ndx1) = j
  end do
109 continue
3712 ioutcs = jr
  if (ioutcs .gt. 0) go to 111
  if (tmax .le. 0.0) go to 111
  if (ntcsex + nstacs .gt. 0) go to 111
  kill = 140
  lstat(19) = 23153
  go to 9000
111 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 192) (jout(kjout + i), i = 1, ioutcs)
192 format ('0program output will be prepared for nodes 0', /, (1x, 16i8))
  ! ******   initial condition   **********
3072 if (kxic .eq. 0) go to 231
  kxtcs2 = kxtcs + lstat(68)
  do i = 1, kxic
     awkcs(kawkcs + i) = xtcs(kxtcs + i)
     xtcs(kxtcs + i) = 0.0
  end do
  jr = 0
  do i = 1, kxic
     j8 = int (awkcs(kawkcs + i))
     do k = 1, ktab
        if (j8 .eq. ilntab(klntab + k)) go to 412
     end do
     write (unit = lunit(6), fmt = 7686) texvec(j8)
7686 format (' $$$$$ Ignore the initial condition card since no such name ( ', "'", a6, "'", ' ) in the TACS table $$$$$')
     go to 4111
412  jr = jr + 1
     xtcs(kxtcs + k) = xtcs(kxtcs2 + i)
  end do
4111 continue
  !  call mover0 (xtcs(kxtcs2 + 1), kxic)
  call move0 (xtcs(kxtcs2 + 1 :), kxic)
  kxic = jr
  if (kxic .eq. 0 .or. iprsup .lt. 2) go to 231
  do i = 1, ktab
     ndx1 = ilntab(klntab + i)
     if (xtcs(kxtcs + i) .ne. 0.0) write (unit = lunit(6), fmt = 196) texvec(ndx1), xtcs(kxtcs + i)
  end do
196 format ('  User-defined initial condition', a6, e20.8)
231 if (iplot .lt. 0) go to 9000
  n1 = 0
  write (unit = lunit(4)) date1, tclock, maxbus, ioutcs, n1, n1, (texvec(i), i = 1, maxbus)
  n2 = kcolcs + 1
  n3 = kcolcs + ioutcs
  do i = n2, n3
     n1 = n1 + 1
     j = jout(kjout + n1)
     icolcs(i) = ilntab(klntab + j)
  end do
  write (unit = lunit(4)) (icolcs(i), i = n2, n3)
9000 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module tacs1b."')
  return
end subroutine tacs1b

!
! subroutine expchk.
!

subroutine expchk (n1, n2, n5)
  use blkcom
  implicit none
  integer(4), intent(in) :: n1, n2, n5
  integer(4) :: i, j, k, key, l, n3, n4
  character(8) :: texnum, text1, x, textp, textn
  dimension x(80), texnum(11)
  !
  data text1 / 'e' /
  data textp / '+' /
  data textn / '-' /
  data texnum(1)  / '1' /
  data texnum(2)  / '2' /
  data texnum(3)  / '3' /
  data texnum(4)  / '4' /
  data texnum(5)  / '5' /
  data texnum(6)  / '6' /
  data texnum(7)  / '7' /
  data texnum(8)  / '8' /
  data texnum(9)  / '9' /
  data texnum(10) / '0' /
  data texnum(11) / '.' /
  read (unit = abuff, fmt = 2618) (x(i), i = 1, 80)
2618 format (80a1)
  do i = n1, n2, n5
     key = 0
     do j = 1, n5
        n3 = i + j -1
        if (x(n3) .eq. text1) go to 2622
        if (key .gt. 0) go to 2620
        do k = 1, 11
           if (x(n3) .eq. texnum(k)) key = k
        end do
        go to 2621
2620    if (x(n3) .eq. textp .or. x(n3) .eq. textn) go to 2622
     end do
2621 continue
     go to 2625
2622 n4 = i + n5 - 1
     do l = 1, 10
        if (x(n4) .eq. texnum(l)) go to 2625
     end do
     kill = 97
     lstat(14) = i
     lstat(15) = n4
     lstat(19) = 2624
     return
  end do
2625 continue
  return
end subroutine expchk

!
! subroutine intchk.
!

subroutine intchk (n1, n2, n5)
  use blkcom
  implicit none
  integer(4), intent(in) :: n1, n2, n5
  integer(4) :: i, n3, n4, n6, n7, n8
  character(8) :: x
  dimension x(80)
  !
  read (unit = abuff, fmt = 2642) (x(i), i = 1, 80)
2642 format (80a1)
  n6 = n1 - 1
  do i = n1, n2
     if (x(i) .eq. blank) go to 2648
     n7 = i - n6
     n8 = n7 / n5
     n3 = n8 * n5
     if (n3 .eq. n7) go to 2648
     n4 = n6 + (n8 + 1) * n5
     if (n4 .gt. n2) go to 2645
     if (x(n4) .ne. blank) go to 2648
2645 kill = 98
     lstat(14) = i
     lstat(15) = n4
     bus1 = x(i)
     lstat(19) = 2642
     return
  end do
2648 continue
  return
end subroutine intchk

!
! subroutine pfatch.
!

subroutine pfatch
  use blkcom
  use dekspy
  implicit none
  ! This installation-dependent module serves to connect a
  ! file to  i/o  channel  ialter  (/blank/ variable),
  ! based on the file specification contained on last-
  ! read data card.   Different usages include:
  ! replot, file specification
  ! start again, file specification
  ! free-format is here required, temporarily.
  ! Module written specially for  DEC  VAX-11/780 .
  !  include 'blkcom.ftn'
  !  include 'dekspy.ftn'
  integer(4) :: j, k, n4, n7
  character(25) :: filen
  !
  if (m4plot .ne. 1) go to 4519                   ! not interactive emtp
  write (unit = prom80, fmt = 4504)
4504 format ('    Send VAX disk file name:')
  call prompt                                     ! write prom80 with cursor control (no lf)
  read (unit = munit5, fmt = 4507) (texcol(j), j = 1, 30)
4507 format (30a1)
  texcol(31) = csepar                             ! put "," terminator after name
  ialter = lunit(2)                               ! connect emtp tables file to this unit
4519 n4 = 0
  write (unit = filen(1 : 25), fmt = 4523)
4523 format (25x)
  do k = kolbeg, 80
     if(texcol(k) .eq. blank) go to 4532
     if(texcol(k) .eq. csepar) go to 4536
     n4 = n4 + 1
     write (unit = filen(n4 : 25), fmt = 3041) texcol(k)
3041 format (80a1)
  end do
4532 continue
4536 nfrfld = 1
  kolbeg = k + 1
  n7 = ialter
  if(iprsup .ge. 1) write (unit = lunit(6), fmt = 4548) ialter, filen
4548 format (/, ' Ready to connect file to unit  "ialter" =', i3, ' .   "filen" =', a25)
  close (unit = n7)
  open(unit = n7, status = 'old', form = 'unformatted', file = filen)
  if(iprsup .ge. 1) write (unit = lunit(6), fmt = 4561)
4561 format (/, ' Successful file opening in  "pfatch" .')
  icat = 2
  return
end subroutine pfatch

!
! end of file over1.f90
!
