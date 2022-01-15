! -*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file datain.f90
!

!
! subroutine datain.
!

subroutine datain
  use blkcom
  use dekspy
  use labcom
  use iocons
  use bcdtim
  use bcddat
  use movcop
  use strcom
  use linemodel
  implicit none
  !     Universal module of interactive EMTP (spy of "emtspy").
  !     If non-interactive version, module can be destroyed.
  !     First EMTP data input, and "spy" choice, are made here.
  !     Module is called only by installation-dependent "erexit".
  integer(4) :: ios, ip, j
  integer(4) :: k, kard(200), karg(200), kbeg(200), kcut, kend(200)
  integer(4) :: kkkdum(35), kntdum, kntmax, kntold, kolinc(35), komlev, krdcom
  integer(4) :: krdoff, ktex(200)
  integer(4) :: l, lentyp(36), limarg, ll
  integer(4) :: m, modarg(35), munit4
  integer(4) :: n, n1, n2, n3, n4, n5, n6, n7, n8, n10, n11, n12, n13, n14, n15
  integer(4) :: n16, n17, n18, n19, n20, n22, n24, n26, nchpre, nchsuf, nn1, nn2
  integer(4) :: ntacs, numarg, numhld, numtyp
  character(6) :: dumnam
  character(12) :: typdat(36)
  character(20) :: arginc(35)
  character(32) :: filsav
  character(40) :: prefix, suffix
  character(80), allocatable :: tank(:)
  !  dimension kard(200), karg(200), kbeg(200), kend(200)
  !  dimension ktex(200), lentyp(36)
  !  dimension kolinc(35), modarg(35), kkkdum(35)
  !
  data kcut       / 0 /
  data komlev     / -1 /                                   ! default comment level (none)
  data nchpre     / 0 /                                    ! begin with no file name prefix
  data nchsuf     / 0 /                                    ! begin with no file name suffix
  data dumnam     / 'dum   ' /                             ! default dummy root name
  data typdat(1)  / 'request     ' /, lentyp(1)  /  7  /
  data typdat(2)  / 'function    ' /, lentyp(2)  /  8  /
  data typdat(3)  / 'tacs source ' /, lentyp(3)  / 11  /
  data typdat(4)  / 'supplemental' /, lentyp(4)  / 12  /
  data typdat(5)  / 'tacs output ' /, lentyp(5)  / 11  /
  data typdat(6)  / 'tacs initial' /, lentyp(6)  / 12  /
  data typdat(7)  / 'branch      ' /, lentyp(7)  /  6  /
  data typdat(8)  / 'switch      ' /, lentyp(8)  /  6  /
  data typdat(9)  / 'source      ' /, lentyp(9)  /  6  /
  data typdat(10) / 'load flow   ' /, lentyp(10) /  9  /
  data typdat(11) / 'initial     ' /, lentyp(11) /  7  /
  data typdat(12) / 'output      ' /, lentyp(12) /  6  /
  data typdat(13) / 'plot        ' /, lentyp(13) /  4  /
  data typdat(14) / 'statistics  ' /, lentyp(14) / 10  /
  data numtyp     / 14 /                                    ! total number of data type names
  data filsav     / '                                '  /   ! for lmfs
  allocate(tank(1 : 1000))
  if (.not. allocated(tank)) then
     write (unit = *, fmt = 10) numhld
10   format ('Could not allocate tank array for ', i4, ' dimension.  Exiting.')
     stop
  end if
  if (kexact .ne. 88333) numrun = 0                         ! init. numrun for lmfs runs
  if (kexact .eq. 88333 .and. numrun .gt. 0) go to 5266
  if (numhld .eq. -8899) stop
  if (numhld .eq. 0) go to 5244
  do j = 1, numhld
     if ((toLower (tank(j)(1 : 19)) .eq. 'begin new data case') .and. j .gt. 3) go to 3389
     file6(j) = tank(j)
  end do
3389 numcrd = j - 1
  do k = j, numhld
     tank(k - numcrd) = tank(k)
  end do
  numhld = numhld - numcrd
  if (numhld .le. 0) numhld = -8899
  numdcd = 0
  go to 1774
5266 kcut = 0
  if (filsav .eq. '                                ') go to 1712 ! data input assigned from outside
  open (unit = munit5, status = 'old', file = filsav)       ! reuse input for
1712 if (llbuff .eq. -3333) rewind munit5                   ! generating 2nd & 3rd lmf
                                                            ! 2nd or later pass, skip
5244 if (llbuff .ne. -3333 .and. toLower (file6(numcrd + 1)(1 : 4)) .ne. 'eof ') go to 1708 ! keyboard
  kverfy = -4545                                  ! local flag (no windows yet opened)
  limarg = 35                                     ! dimensioned limit on arguments of "module"
  numdcd = 0                                      ! set pointer at zero (no "cimage" calls yet)
  munit5 = 5                                      ! i/o channel for "emtspy" input (keyboard)
  limcrd = 30000                                  ! present fixed limit on file6 of "dekspy"
  n13 = 0                                                   ! initially assume no debug printout
  ntacs = 0                                                 ! old tacs data format
  call date44 (date1)                                       ! find calendar date and the
  call time44 (tclock)                                      ! time of day for documentation
  call initsp                                               ! initialize spy common (digit needed to sort)
1311 write (unit = lunit(6), fmt = 1324)                      ! prompt user at "emtspy" keyboard
1324 format (' EMTP begins.  Send (spy, $attach, debug, help, module, junk, stop): ')
  read (unit = munit5, fmt = 1329, iostat = ios) buff77     ! read first card of EMTP data
1329 format (a80)
  if (ios .ne. 0) then
     write (unit = lunit(6), fmt = "(' Could not read from stdin.')")
     stop
  end if
  if (toLower (buff77(1 : 5)) .eq. 'stop ') call stoptp
  if (toLower (buff77(1 : 5)) .ne. 'disk ') go to 51329
  maxzno = 4545                                             ! signal to apollo "sysdep" for disk lunit(6)
  go to 1311
51329 if (toLower (buff77(1 : 7)) .eq. '$attach') go to 1347         ! batch mode
  if (toLower (buff77(1 : 5)) .ne. 'junk ') go to 1332
  write (unit = lunit(6), fmt = 1330)
1330 format ('   Send root word to over-ride "junk" for spy and plot windows :')
  read (unit = munit5, fmt = 1331) junker                   ! read new window pad name
  buff77(1 : 8) = 'spy     '                                ! implied command next serviced
1331 format (a8)
1332 if (kverfy .eq. 0) go to 41332                         ! windows already open
  kverfy = -34543                                           ! flag indicating desire to open windows
  call window                                               ! open 2 extra windows for spy and plot
  kverfy = 0                                                ! erase flag, now that both windows are open
41332 if (toLower (buff77(1 : 6)) .eq. 'module') go to 2613
  if (toLower (buff77(1 : 5)) .ne. 'help ') go to 1342
  write (unit = munit6, fmt = 1333)
1333 format ('    Greetings, greetings.  Welcome to the wonderful new world of interactive')
  call window                                               ! output of character variable munit6
  write (munit6, 1334)
1334 format ('    EMTP execution, observation, and control.  After sending  "spy",  send')
  call window                                               ! output of character variable munit6
  write (munit6, 1335)
1335 format ('    "help",  and then  "all"  to receive some 500 lines of instruction.  Also,')
  call window                                               ! output of character variable munit6
  write (munit6, 1336)
1336 format ('    see section 9 of the rule book dated June, 1984.  Also see  "Apollo".')
  call window                                               ! output of character variable munit6
  go to 1311
1342 if (toLower (buff77(1 : 6)) .ne. 'debug ') go to 1347
  n13 = 99                                                  ! change diagnostic printout control to "on"
  go to 1311                                                ! back to prompt for interactive choice again
1347 iprspy = n13                                           ! diagnostic spy printout (0 or 99)
  iprsup = n13                                              ! diagnostic EMTP printout (0 or 99)
  iprsov(1) = n13                                           ! EMTP diagnostic only thru 1st overlay
  if (toLower (buff77(1 : 3)) .ne. 'spy') go to 1724       ! not interactive
  ! begin interactive control sequence, leading to "emtspy":
  m4plot = 1                                                ! set flag remembering use of spy
  write (unit = prom80, fmt = 1357)                         ! build very 1st spy prompt
1357 format (' spy:')
  call prompt                                               ! write prom80 with cursor control (no lf)
1708 if (m4plot .ne. 1) go to 2320                          ! non-interactive case
  lockbr = 1                                                ! forced spy read within "flager"
  call emtspy                                               ! b4 any EMTP computation, start spy dialogue
  if (lockbr .eq. 1) go to 1708                             ! spy loop until "go"
  go to 1774                                                ! jump to $include removal, then exit module
  ! begin non-interactive variable initialization:
1724 m4plot = 2                                             ! not interactive, and use real*4 lunit4 plots
  lunit(13) = 5                                                ! initially assume externally-connected data
  numcrd = 1                                                ! so far, we have read one input data card
  file6(1) = buff77                                         ! 1st card image permanently stored
  if (toLower (buff77(1 : 7)) .ne. '$attach') go to 1753
  ! "$attach,filename,5" usage requires extraction of name:
  lunit(13) = 13                                               ! we will internally connect data to unit 13
  n16 = 0                                                   ! column which begins file name is not yet known
  do j = 9, 40                                              ! search for 2nd comma in these columns
     ! if nonblank column,
     if (buff77(j : j) .ne. ' ' .and. n16 .eq. 0) n16 = j   ! and 1st, save it in n16
     if (buff77(j : j) .eq. ',') go to 1746                 ! 2nd comma found
  end do
1736 write (unit = munit6, fmt = 1737)
1737 format ('  ----  Illegal file name.  Try again ....' )
  call window                                               ! output of character variable munit6
  go to 1311                                                ! loop back to opening prompt (repeat it)
1746 n14 = j - n16                                          ! number of characters in file name
  ansi32(1 : n14) = buff77(n16 : j - 1)                     ! transfer disk file name
  ansi32(n14 + 1 : 32) = blan80(n14 + 1 : 32)               ! blank out remainder
  filsav = ansi32(1 : 32)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1752) ansi32
1752 format (' Extracted file name ansi32(1 : 32) =', a32)
  inquire (file = ansi32, exist = logvar)                   ! ask if file exists
  if (.not. logvar) go to 1736                              ! illegal file; reprompt
  spycd2(1 : 32) = ansi32                                   ! save file name for prime "erexit"
  open (unit = lunit(13), status = 'old', file = ansi32)
  file6(1) = 'c ' // buff77(1 : 78)                         ! make $attach into comment
  kcut = 0
1753 continue
  krdoff = numcrd
  krdcom = 0
  do j = 1, limcrd                                          ! read until an end-of-file detected
     read (unit = lunit(13), fmt = 1329, end = 1766) file6(krdoff + j)
     if (kcut .eq. 1) go to 5486
     if (toLower (file6(krdoff + j)(1 : 2)) .eq. 'c ') krdcom = krdcom + 1
     if ((toLower (file6(krdoff + j)(1 : 19)) .ne. 'begin new data case') .or. j - krdcom .le. 3) go to 1756
     kcut = 1
5486 numhld = numhld + 1
     if (numhld .gt. 1000) then
        write (unit = munit6, fmt = *) ' Input data cards overflow tank(1000).   Halt.'
        call stoptp
     end if
     tank(numhld) = file6(krdoff + j)
     ! if all EMTP data (e.g., "kill codes" use) comes via key
     ! board, it is ended with "eof"; when solved, more keyboard.
1756 if (toLower (file6(krdoff + j)(1 : 4)) .eq. 'eof ') go to 1766
     if (kcut .eq. 0) numcrd = numcrd + 1                   ! another input data card now read
  end do
1760 write (unit = munit6, fmt = 1761) limcrd
1761 format ('  & & & & &   Input buffer overflow.  Limit =', i6, '.   Reject this data, and reprompt ....')
  call window                                               ! output of character variable munit6
  ! if this was $attach usage, then
  if (lunit(13) .eq. 13) go to 1311                            ! loop back to start anew the data input
  call stoptp                                               ! installation-dependent program stop card
1766 numcrd = numcrd + 1
  write (unit = munit6, fmt = 1767) numcrd
1767 format (' Done reading disk file into EMTP cache.   numcrd =', i5, '  cards.')
  call window                                               ! output of character variable munit6
  ! if internally-connected file, then
  if(lunit(13) .ne. 5) close (unit = lunit(13), status = 'keep')  ! disconnect it
1774 n22 = 1                                                ! initialize pass number of $include processing
  n13 = 1                                                   ! 1st $include might be 1st data card (do 1786)
  ! begin loop to replace next presently-visible $include :
1776 n17 = 0                                                ! switch set so do 1816 loop executed 1st time
  do j = n13, numcrd                                        ! search suspect data for $include
1777 if (file6(j)(1 : 1) .ne. '$') go to 1786               ! skip all non-$
     buff77 = file6(j)                                      ! transfer to scalar working storage
     if (iprspy .lt. 3) go to 41777                         ! jump around diagnostic
     write (unit = munit6, fmt = 31777) j, buff77
31777 format (' j =', i4, '   next $-card = ', a80)
     call window                                            ! output of character variable munit6
41777 if (toLower (buff77(1 : 8)) .eq. '$include') go to 1787
     ! only processes ! these limited ! dollar cards
     if (toLower (buff77(1 : 7)) .ne. '$prefix' .and. toLower (buff77(1 : 7)) .ne. '$suffix' .and. toLower (buff77(1 : 6)) .ne. '$level' .and. toLower (buff77(1 : 6)) .ne. '$dummy') go to 1786
     k = 8                                                  ! begin searching for file pre/suffix in col. 8
     ! if not blank and not
1778 if (buff77(k : k) .ne. ' ' .and. buff77(k : k) .ne. ',') go to 1779 ! comma, exit
     k = k + 1                                              ! next column to right in search for file start
     if (k .lt. 80) go to 1778                              !  back to check new column k
     k = 10                                                 ! pretend pre/suffix name begins in column 10
     l = 4                                                  ! pretend pre/suffix name is 5 characters long
     if (toLower (buff77(5 : 7)) .eq. 'fix') go to 31779   ! $pre/suffix
     istep = j                                              ! blank common communication of card number
     call stopin                                            ! allow user to correct erroneous card j
     if (kill .gt. 0) go to 9200                            ! on our way to "over51"
     go to 1777                                             !  return to re-process corrected card j
1779 if (toLower (buff77(1 : 7)) .ne. '$prefix' .and. toLower (buff77(1 : 7)) .ne. '$suffix') go to 1783
     l = index (buff77(k :), ' ')                           ! one col. past end of name
     m = index (buff77(k :), ',')                           ! col. of comma, if any
     ! if there is a trailing comma, and
     ! if this precedes the blank, then
     if (m .ne. 0 .and. m .lt. l) l = m                     ! it bounds user-supplied name;  bound is "l"
     l = l - 1                                              ! index for final character of prefix/suffix
31779 n5 = k - 1 + l                                        ! end of pre/suffix relative to col. 1
     if (toLower (buff77(1 : 7)) .ne. '$prefix') go to 1782
     prefix = buff77(k : n5)                                ! permanent storage of name prefix
     nchpre = l                                             ! length of file name prefix now stored
     go to 1785                                             ! done processing $prefix card; make comment
1782 suffix = buff77(k : n5)                                ! permanent storage of name suffix
     nchsuf = l                                             ! length of file name suffix now stored
     go to 1785                                             ! done processing $suffix card; make comment
1783 if (toLower (buff77(1 : 6)) .ne. '$level') go to 1784
     prom80 = buff77(k : 80)                                ! transfer numerical part of card
     call frein1 (prom80, komlev)                           ! decode komlev from prom80
     go to 1785                                             ! done processing $level card;  make comment
1784 if (toLower (buff77(1 : 6)) .ne. '$dummy') go to 1786
     dumnam(1 : 3) = buff77(k : k + 2)                      ! store new 3-char root name
     ansi8(1 : 3) = buff77(k + 3 : k + 5)                   ! transfer digits to cell 1
     read (unit = ansi8, fmt = 31784) kntdum                ! re-initialize serialization
31784 format (3i1)                                          ! 3-digit decimal serialization for dummy
1785 file6(j) = 'c ' // buff77(1 : 78)                      ! convert to comment card
  end do
1786 continue                                               ! end  do 1786  check of data card j for $incl.
  go to 2320                                                ! done with input data; no $include remain
1787 n13 = j                                                ! remember index of 1st $include, for next search
1788 n26 = 9                                                ! begin looking for separator in column 11
1789 if (buff77(n26 : n26) .ne. ' ' .and. buff77(n26 : n26) .ne. ',') go to 1797
  n26 = n26 + 1                                             ! move one column to right in search
  if (n26 .lt. 40) go to 1789
1794 istep = j                                              ! blank common communication of card number
  call stopin                                               ! allow user to correct erroneous card j
  if (kill .gt. 0) go to 9200                               ! on our way to "over51"
  go to 1776                                                !  return to re-process corrected card j
1797 k = n26 + 1                                            ! starting candidate for last col. of name
  ! if comma or blank, exit
  do
     if (buff77(k : k) .eq. ',' .or. buff77(k : k) .eq. ' ') go to 1804   ! with file name
     k = k + 1                                              ! next col. right in search for end of name
     if (k .gt. 60) go to 1794                              ! jump to error correction
  end do                                                    ! loop back to continue search for end of name
  ! if non-blank prefix exists, then
1804 if (nchpre .gt. 0) answ80(1 : nchpre) = prefix(1 : nchpre) ! prefix begins name
  n7 = nchpre + k - n26                                     ! length of prefix + center name
  answ80(nchpre + 1 : n7) = buff77(n26 : k - 1)             ! add center name
  n8 = n7 + nchsuf                                          ! length of entire, complete file name
  ! if non-blank suffix exists, then
  if (nchsuf .gt. 0) answ80(n7 + 1 : n8) = suffix(1 : nchsuf)     ! suffix finishes name
  answ80(n8 + 1 : 80) = blan80(n8 + 1 : 80)                 ! blank out remainder
  if (n17 .eq. 1) go to 1819                                ! 2nd or later pass, so skip
  l = numcrd                                                ! 1st card copied is bottom of data case
  n19 = limcrd                                              ! destination of this first card copied
  do m = j + 1, numcrd                                      ! loop over all data below $incl.
     file6(n19) = file6(l)                                  ! move card down as far as possible
     l = l - 1                                              ! preceding data card is next to be copied
     n19 = n19 - 1                                          ! corresponding card destination address
  end do
  n19 = n19 + 1                                             ! remember 1st card of copy stored below
1819 write (unit = lunit(6), fmt = 1820) n22, j, answ80(1 : n8)
1820 format ('   --- Pass', i3,  ',  card =', i4,'.   Ready to open $include =', a)
  inquire (file = answ80(1 : n8), exist = logvar)           ! file exists?
  if (.not. logvar) go to 1794                              ! illegal name correction
  prom80 = file6(j)                                         ! temp storage b4 2-byte shift
  file6(j) = 'c ' // prom80(1 : 78)                         ! make into a comment card
  open (unit = lunit(13), status = 'old', file = answ80(1 : n8))
  n16 = 0                                                   ! so far, no arguments of $include are known
  n26 = k + 1                                               ! point to "," or " " ending file name
4203 do l = n26, 80                                         ! search cols. n26-80 for nonblank
     ! if not "," or blank,
     if (buff77(l : l) .ne. ',' .and. buff77(l : l) .ne. ' ') go to 4208   ! argument starts
  end do                                                    ! end  do 4205  loop;  col. "l" not argument
  go to 4226                                                ! all arguments found; now use them
4208 n16 = n16 + 1                                          ! another (the n16-th) argument begins
  ! if number of arguments is too large,
  if (n16 .gt. 10) call stoptp                              ! installation-dependent program stop card
  n12 = index (buff77(l :), ',')                            ! locate bounding comma
  n13 = index (buff77(l :), ' ')                            ! locate bounding blank
  n14 = n12                                                 ! assume there's a comma, in this col.
  if (n12 .gt. 0) go to 4214                                ! trailing comma is present
  n14 = n13                                                 ! assume there's a blank, in this col.
  if (n13 .gt. 0) go to 4220                                ! no ",", but there is " "
  write (unit = munit6, fmt = 4211)
4211 format (' No bounding symbol.  Stop after display.' )
  call window                                               ! output of character variable munit6
  write (unit = munit6, fmt = 4223) l, n12, n13, n14, n26
  call window                                               ! output of character variable munit6
  call stoptp                                               ! installation-dependent program stop card
  ! if trailing blank, and if blank
4214 if (n13 .gt. 0 .and. n13 .lt. n14) n14 = n13           ! precedes ",", blank bounds
4220 n15 = n14 - 1                                          ! number of columns making up the argument
  kolinc(n16) = n15                                         ! remember the width of argument n16
  n18 = l - 1 + n15                                         ! col. ending argument, based on col. 1
  arginc(n16) = ' '                                         ! blank out storage b4 adding string
  arginc(n16)(1 : n15) = buff77(l : n18)                    ! remember argument
  n26 = n18 + 1                                             ! column to begin search for next argument
  if (iprspy .lt. 5) go to 4224                             ! jump around diagnostic
  write (unit = munit6, fmt = 4223) l, n12, n13, n14, n26
4223 format (' Done with argument.  l, n12, n13, n14, n26 =', 8i6)
  call window                                               ! output of character variable munit6
4224 go to 4203                                             ! loop back to process next argument if any
4226 kard(1) = 999999                                       ! assume no arguments (this is bound)
  n1 = 0                                                    ! initialize offset for pointer vector reads below
  if (n16 .eq. 0) go to 4239                                ! skip argument pointers
4228 read (unit = lunit(13), fmt = 4232) (kbeg(l), l = 1, 25)  ! read next card
  do l = 1, 25                                              ! search i3 replacement fields for blank
     if (kbeg(l) .eq. 0) go to 4230                         ! yes, bound is found
  end do                                                    ! end  do 4229  loop to bound replacements
  n1 = n1 + 25                                              ! 25 more parameter usages found
  if (n1 .le. 175) go to 4228                               ! still room for 25 more
  ! overflow.  199 is max number of replacements, temporarily
  call stoptp                                               ! installation-dependent program stop card
4230 rewind lunit(13)                                          ! rewind $include file, to start again
  n6 = n1 + l - 1                                           ! number of effective argument usages
  read (unit = lunit(13), fmt = 4232) (kard(k), k = 1, n6)     ! card nos. used
  read (unit = lunit(13), fmt = 4232) (karg(k), k = 1, n6)     ! arguments used
  read (unit = lunit(13), fmt = 4232) (kbeg(k), k = 1, n6)     ! col. no. start
  read (unit = lunit(13), fmt = 4232) (kend(k), k = 1, n6)     ! col. no. ending
  read (unit = lunit(13), fmt = 4232) (ktex(k), k = 1, n6)     ! alphanum. flag
4232 format (4x, 25i3)
  if (iprspy .lt. 1) go to 4235                             ! jump around diagnostic
  write (unit = munit6, fmt = 4233) n6
4233 format (' Done reading argument usage vectors.  n4 =', i5)
  call window                                               ! output of character variable munit6
4235 if (iprspy .lt. 5) go to 4238                          ! jump around diagnostic
  write (unit = munit6, fmt = 4236)
4236 format (' Vectors kard, karg, kbeg, kend, ktext(1 : 25) ...')
  call window                                               ! output of character variable munit6
  write (unit = munit6, fmt = 4237) (kard(k), k = 1, n5)
  call window                                               ! output of character variable munit6
  write (unit = munit6, fmt = 4237) (karg(k), k = 1, n5)
  call window                                               ! output of character variable munit6
  write (unit = munit6, fmt = 4237) (kbeg(k), k = 1, n5)
  call window                                               ! output of character variable munit6
  write (unit = munit6, fmt = 4237) (kend(k), k = 1, n5)
  call window                                               ! output of character variable munit6
  write (unit = munit6, fmt = 4237) (ktex(k), k = 1, n5)
4237 format (4x, 25i3)
  call window                                               ! output of character variable munit6
4238 kard(n6 + 1) = 999999                                  ! bound (no more argument usage)
4239 n20 = 0                                                ! no records read from $include file so far
  n24 = 1                                                   ! start by considering 1st argument first
  n5 = 0                                                    ! so far, no "/" cards encountered in include file
  n18 = j + 1                                               ! do-loop limit (needed, for j changes inside)
  kntmax = 0                                                ! initialize maximum counter for dummy names
  kntold = kntdum                                           ! save serialize index at start of file
  do k = n18, limcrd                                        ! read $include records until eof
     read (unit = lunit(13), fmt = 1329, end = 1828) buff77    ! next data card
     if (toLower (buff77(1 : 4)) .eq. '$eof') go to 1828   ! effective eof
     if (buff77(1 : 1) .eq. '/') n5 = 1                     ! yes, 1 or more "/"
     if (toLower (buff77(1 : 1)) .ne. 'c') go to 4247      ! accept non-com.
     do  l = 1, 10                                          ! see if col. 2 is one of 10 digits
        if (buff77(2 : 2) .eq. digit(l)) go to 4244         ! yes, digit
     end do                                                 ! end  do 4241  loop to check if col. 2 is digit
     go to 4247                                             ! non-digited comment card is accepted
4244 if (l .eq. 10) l = 0                                   ! digit(10) = 0   is exception
     if (l .gt. komlev) go to 4273                          ! ignore comment card
     buff77(2 : 2) = ' '                                    ! blank out level-digit in col. 2
4247 j = j + 1                                              ! accept this data card; do not discard it
     file6(j) = buff77                                      ! transfer buffer to regular storage
     ! comment cards can have no
     if (toLower (buff77(1 : 1)) .eq. 'c') go to 4273      ! arguments, so bypass the check for usage
     n20 = n20 + 1                                          ! no. of countable $include card just read
     if (iprspy .lt. 3) go to 4249                          ! jump around diagnostic
     write (unit = munit6, fmt = 4248) n20, n24, kard(n24)
4248 format (' Ready with next card.  n20, n24, kard(n24) =', 3i8)
     call window                                            ! output of character variable munit6
4249 if (n20 .lt. kard(n24)) go to 4273                     ! no argument usage
     n1 = kbeg(n24)                                         ! beginning column number of replacement
     n2 = kend(n24)                                         ! ending    column number of replacement
     n4 = karg(n24)                                         ! index number of argument being used
     n3 = kolinc(n4)                                        ! length of argument being substituted
     if (n4 .le. n16) go to 34250                           ! legal argument request
     write (unit = lunit(6), fmt = 4250) n24, n4, n16
4250 format ('   ? ? ? ?   Error stop at s.n. 4250 of "datain".   Insufficient number of $include arguments.', /, '             n24, n4, n16 =', 3i8)
     call stoptp                                            ! installation-dependent program stop card
34250 if (n4 .gt. 0) go to 4252                             ! use argument of $include
     kntdum = kntold - n4                                   ! serialization for dummy name usage
     if (-n4 .gt. kntmax) kntmax = -n4                      ! new larger dummy
     write (unit = ansi8, fmt = 4251) kntdum                ! convert integer to character
4251 format (i3)
     if (kntdum .lt. 100) ansi8(1 : 1) = '0'                ! 1st of 3 digits
     if (kntdum .lt. 10) ansi8(2 : 2) = '0'                 ! 2nd of 3 digits
     ! if name is not 6 characters,
     if (n2 - n1 .ne. 5) call stoptp                        ! installation-dependent program stop card
     dumnam(4 : 6) = ansi8(1 : 3)                           ! add serialization to root name
     file6(j)(n1 : n2) = dumnam                             ! transfer dummy name to card
     go to 4249                                             ! loop back to consider possible next argument
     ! if arg. length mismatch, ! & if this is text, then
4252 if (n2 - n1 .ne. n3 - 1 .and. ktex(n24) .eq. 1) go to 4253 ! jump to correction chance
     if (n2 - n1 .ge. n3 - 1 ) go to 4261                   ! arg. is short enough
4253 write (unit = munit6, fmt = 4254) n4, n20
4254 format ('   +++ Argument', i4, '   length-mismatch error.   Used on card', i4, ' .')
     call window                                            ! output of character variable munit6
     write (unit = munit6, fmt = 4255) n24, n1, n2, n3
4255 format ('       n24 =', i4, '    kbeg, kend =', 2i4, '    length from $include =', i4, ' .')
     call window                                            ! output of character variable munit6
     write (unit = prom80, fmt = 4256)
4256 format (' Send corrected argument (stop) :')
     call prompt                                            ! write prom80 with cursor control (no lf)
     if (m4plot .eq. 1) go to 4259                          ! spy usage allows recovery
     kill = 79                                              ! kill code indicating interactive hopelessness
     lstat(19) = 4259
     go to 9200                                             ! assign lstat(18), then nchain=51; then exit
4259 read (unit = munit5, fmt = 4260) arginc(n4)            ! read revised argument
4260 format (a20)
     ! if user surrenders,
     if (toLower (arginc(n4)(1 : 5)) .eq. 'stop ') call stoptp       ! installation-dependent program stop card
4261 n = n2                                                 ! initialize destination address at right edge
     if (iprspy .lt. 4) go to 34261                         ! jump around diagnostic
     write (unit = munit6, fmt = 24261) arginc(n4)
24261 format (' Argument now processed, arginc(n4) =', a20)
     call window                                            ! output of character variable munit6
34261 do ip = 1, 20                                         ! process all 20 characters of argument
        m = 21 - ip                                         ! process from right to left (right-adjust)
        char1 = arginc(n4)(m : m)                           ! transfer byte to temp scalar
        if (char1 .eq. ' ') go to 4263                      ! skip over blank
        if (iprspy .lt. 8) go to 54262                      ! jump around diagnostic
        write (unit = munit6, fmt = 4262) ip, m, char1
4262    format ('  Next non-blank digit.  ip, n, digit =', 2i6, 3x, '"', a1, '"')
        call window                                         ! output of character variable munit6
        ! pounds reserves blank space, so
54262   if (char1 .eq. '#') char1 = ' '                     ! it is now blanked as it is used
        if (n .lt. n1) go to 4253                           ! space error; allow correction
        file6(j)(n : n) = char1                             ! substitute this argument byte
        n = n - 1                                           ! back up destination address for next byte
     end do
4263 continue                                               ! end  do 4263  loop over arginc(20:1)
     ! if blank space remains on left, ! & if this is text, then
     if (n + 1 .gt. n1 .and. ktex(n24) .eq. 1) go to 4253   ! allow user correction of erroneous string
     ! if data field still has space on left,
     if (n .ge. n1) file6(j)(n1 : n) = ' '                  ! then blank this remainder out
     n24 = n24 + 1                                          ! done with present replacement; on to next
     go to 4249                                             ! loop back to consider this next argument
  end do
4273 continue                                               ! end  do 4273  loop reading all file cards
  go to 1760                                                ! stop after message about buffer overflow
1828 close (unit = lunit(13), status = 'keep')
  if (iprspy .lt. 1) go to 1832                             ! jump around diagnostic
  write (unit = munit6, fmt = 1831) j, n19
1831 format (' Done with disk file (close).  j, n19 =', 2i8)
  call window                                               ! output of character variable munit6
1832 kntold = kntold + kntmax                               ! update dummy serializ. reference
  if (n5 .eq. 1) go to 1833                                 ! "/" usage, so skip c-end
  buff77(1 : 32) = 'c end of $include.  file name = '
  buff77(33 : 80) = answ80(1 : 48)                          ! transfer file name of open
  j = j + 1                                                 ! comment card just built needs storage index
  file6(j) = buff77                                         ! transfer to permanent storage
1833 if (j .gt. n19) go to 1760                             ! overflow error stop
  do m = n19, limcrd                                        ! copy lower cards back as k+1,..
     j = j + 1                                              ! destination index for next data card copied
     file6(j) = file6(m)
     if (toLower (file6(j)(1 : 8)) .eq. '$include') go to 1841
  end do
  numcrd = j                                                ! new number of cards making up data case
  n22 = n22 + 1                                             ! increment pass number of $include removal
  go to 1776                                                ! loop back to see if any more $include exist
1841 n19 = m + 1                                            ! lower index 1 beyond $include considered
  if (iprspy .lt. 1) go to 1847                             ! jump around diagnostic
  write (unit = munit6, fmt = 1846) m
1846 format (' Next $include recognized in do 1835.  m =', i6)
  call window                                               ! output of character variable munit6
1847 buff77 = file6(j)                                      ! load working storage used at s.n.1788
  n17 = 1                                                   ! signal to bypass do 1816 cards transfer
  go to 1788                                                ! loop back to process file6(j) as $include
  ! following code processes any $spy usage:
2320 j = 1
  n7 = 0
  n17 = 0                                                   ! so far, no data-sorting requests ("/") found
  do
     if (j .gt. numcrd) go to 2415                          ! done with $spy cards
     if (toLower (file6(j)(1 : 8)) .ne. '$spy    ') go to 2378
     n7 = n7 + 1
     ! if over 9 such $spy usages, then
     if (n7 .gt. 9) call stoptp                             ! installation-dependent program stop card
     file6(j) = '$spy, spyfile .dat, 0,'
     file6(j)(14 : 14) = digit(n7)
     ansi32 = 'spyfile .dat'
     ansi32(8 : 8) = digit(n7)
     if (iprsup .lt. 1) go to 2353                          ! jump around diagnostic
     write (unit = munit6, fmt = 2354) ansi32
2354 format (' Prepare to open for $spy.  ansi32 =', a32)
     call window                                            ! output of character variable munit6
2353 open (unit = lunit(13), status = 'new', file = ansi32)
     do k = j + 1, numcrd
        if (toLower (file6(k)(1 : 7)) .ne. '$spyend') go to 2361
        close (unit = lunit(13))
        n24 = k - j
        do l = k + 1, numcrd
           file6(l - n24) = file6(l)
        end do
        numcrd = numcrd - n24
        write (unit = munit6, fmt = 2358) n24, j, numcrd
2358    format (' Done with upward shift.  n24, j, numcrd =', 3i8 )
        call window                                         ! output of character variable munit6
        go to 2378
2361    write (unit = lunit(13), fmt = 1329) file6(k)
     end do
2378 if (file6(j)(1 : 1) .eq. '/') n17 = 1                  ! sorting needed
     if (toLower (file6(j)(1 : 8)) .ne. 'tacs old') go to 2410
     ntacs = 2                                              ! flag for old tacs data w/o '/' cards
     file6(j)(1 : 2) = 'c '
2410 j = j + 1
  end do                                                    ! label 2347
2415 if (n17 .eq. 0) go to 9800                             ! no data sorting ("/")
  ! begin code to sort EMTP data according to class :
  n1 = 0                                                    ! no class-1  ("request")   usage found so far
  n10 = 0                                                   ! no class-10 ("load flow") usage found so far
  n11 = 0                                                   ! no class-11 ("initial")   usage found so far
  n12 = 0                                                   ! initialize number of data class ("/") cards found
  if (iprsup .ge. 9) write (unit = lunit(6), fmt = 2416) (j, file6(j), j = 1, numcrd)
2416 format (' Entire input file as we start sorting ...', /, (i5, a80))
  do j = 1, numcrd                                          ! search each data card for "/" usage
2418 if (file6(j)(1 : 1) .ne. '/') go to 2431               ! skip non-"/" card
     n12 = n12 + 1                                          ! use next row in table storing "/" pointers
     kssfrq(n12) = j                                        ! remember card number at start of data class
     kpsour(n12) = 0                                        ! so far, no ending card number of "/" use
     if (n12 .eq. 1) go to 2420                             ! no previous "/" to bound
     ! if previous "/" usage is
     if (kpsour(n12 - 1) .eq. 0) kpsour(n12 - 1) = j - 1    ! unbounded, then remember last card
2420 do k = 1, numtyp                                       ! check for each possible data class name
        if (toLower (file6(j)(2 : 13)) .eq. typdat(k)) go to 2428     ! yes, found it
     end do                                                 ! end  do 2421  loop to identify name after "/"
     if (toLower (file6(j)(2 : 10)) .ne. 'tacs data') go to 2424
     k = 6                                                  ! set 'tacs data' to 'tacs initial' if
     ntacs = 1                                              ! new tacs data format used.
     go to 2428                                             !  ntacs set to 1 if tacs with new format
     ! if execution is not interactive,
2424 if (munit4 .ne. 1) call stoptp                         ! installation-dependent program stop card
     write (unit = munit6, fmt = 1737)                      ! tell user to send corrected name
     call window                                            ! output of character variable munit6
     read (unit = munit5, fmt = 1329) file6(j)              ! read corrected "/" card
     go to 2418                                             ! look back to recognize just-read "/" card
2428 kode(n12) = k                                          ! remember data class number in summary table
     if (k .eq. 1) n1 = 1                                   ! remember usage of "/request"
     if (k .eq. 10) n10 = 1                                 ! remember usage of "/load flow"
     if (k .eq. 11) n11 = 1                                 ! remember usage of "/initial"
     ! if 1 or more "/" found so far,
     ! & last "/" not yet ended,
     ! and card is blank,
2431 if (n12 .ge. 1) then
        if (kpsour(n12) .eq. 0 .and. toLower (file6(j)(1 : 6)) .eq. 'blank ') then
           kpsour(n12) = j - 1                              ! then bound last "/" usage
        end if
     end if
  end do                                                    ! end  do 2436  loop which sets up summary "/" table
  if (kpsour(n12) .eq. 0) kpsour(n12) = numcrd              ! final bound
  l = 0
  if (iprspy .lt. 2) go to 12438                            ! jump around diagnostic
  write (unit = munit6, fmt = 2438)
2438 format ('     row  kssfrq  kpsour    kode')
  call window                                               ! output of character variable munit6
  do j = 1, n12
     write (unit = munit6, fmt = 2442) j, kssfrq(j), kpsour(j), kode(j)
2442 format (4i8)
     call window                                            ! output of character variable munit6
  end do                                                    ! end  do 2443  loop over
12438 do                                                    ! beginning conversion of old tacs data format
     l = l + 1
     if (l .gt. n12) go to 2444
     if (kode(l) .le. 1 .or. kode(l) .gt. 6) go to 12438
     if (ntacs .eq. 1) go to 12438
     nn1 = kssfrq(l) + 1                                    !  1st card of class l
     nn2 = kpsour(l)                                        !  last card of class l
     if (kode(l) .gt. 2) go to 12448
     do ll = nn1, nn2
        if (file6(ll)(1 : 2) .ne. '99') go to 12440         ! type '99'
        file6(ll)(1 : 2) = '  '                             ! function cards become '  '
     end do
12440 continue
  end do                                                    ! label 12438
12448 if (kode(l) .gt. 3) go to 12458
  do ll = nn1, nn2                                          ! for tacs sources, convert
     if (file6(ll)(1 : 1) .eq. '$' .or. file6(ll)(1 : 1) .eq. '9' .or. toLower (file6(ll)(1 : 1)) .eq. 'c') go to 12456
     if (file6(ll)(2 : 2) .ne. '1')  go to 12450            ! type '01'
     file6(ll)(1 : 1) = '1'                                 ! to '11',  '02' to '14', '03' to
     go to 12456                                            ! '23' and '04' to '24'
12450 if (file6(ll)(2 : 2) .ne. '2') go to 12452
     file6(ll)(1 : 2) = '14'
     go to 12456
12452 file6(ll)(1 : 1) = '2'
  end do
12456 continue
  go to 12438
12458 if (kode(l) .gt. 4) go to 12468
  go to 12438                                               ! no change on supplemental variable data
12468 if (kode(l) .gt. 5) go to 12478
  do ll = nn1, nn2
     if (file6(ll)(1 : 1) .ne. ' ') go to 12470
     file6(ll)(1 : 2) = '33'                                ! set type '  ' to '33' for tacs
  end do
12470 continue                                              ! output cards
  go to 12438
12478 do ll = nn1, nn2
     if (file6(ll)(1 : 1) .ne. ' ') go to 12480
     file6(ll)(1 : 2) = '77'                                ! set type '  ' to '77' for tacs
  end do
12480 continue                                              ! initial condition cards
  go to 12438
2444 n17 = 1                                                ! initialize entry of "/" summary table we'll hit next
  n18 = kssfrq(n17)                                         ! initialize card number where these begin
  n8 = 0                                                    ! initialize "/" index to remove possible garbage
  n24 = numcrd                                              ! initialize destination address (1 b4 1st)
  j = 0                                                     ! initialize card number of original data done
  if (toLower (file6(1)(1 : 9)) .ne. 'c $attach') go to 2445
  n24 = n24 + 1                                             ! increment destination address past this
  file6(n24) = file6(1)                                     ! transfer case-marker card below
  j = 1                                                     ! update number of original data cards now done
2445 if (toLower (file6(j + 1)(1 : 16)) .ne. 'begin new data c') go to 2446
  n24 = n24 + 1                                             ! increment destination address past this
  file6(n24) = file6(j + 1)                                 ! transfer case-marker card below
  j = j + 1                                                 ! update number of original data cards now done
2446 if (n1 .eq. 0) go to 2453                              ! no "/request" cards used
  n8 = 1                                                    !  index for "request", data which we now add
  go to 2472                                                ! jump to the insertion of this class-1 data
2453 j = j + 1                                              ! next card number of original data considered
  if (j .gt. numcrd) go to 2497                             ! done with all data
  if (j .ne. n18) go to 2456                                ! not a "/" card, so bypass such
  j = kpsour(n17)                                           ! advance card pointer to last card in "/" class
  n17 = n17 + 1                                             ! following "/" usage is in this next row of table
  n18 = kssfrq(n17)                                         ! card number where next "/" cards will appear
  go to 2453                                                ! loop back to continue search for "blank" card
2456 if (n8 .ne. 9) go to 2458                              ! not at end of source data
  if (n10 .eq. 0) go to 2460                                ! no "/load flow" requests
  n8 = n10                                                  ! index for "load flow", data which we now add
  go to 2472                                                ! jump to the insertion of this class-10 data
2458 if (n8 .ne. 10) go to 2462                             ! "/initial" not just added
2460 if (n11 .eq. 0) go to 2462                             ! no "/initial" to be added
  n8 = 11                                                   ! index for "initial", data which we now add
  go to 2472                                                ! jump to the insertion of this class-11 data
2462 if (toLower (file6(j)(1 : 6)) .ne. 'blank ') go to 2493         ! not end of class
  if (iprspy .lt. 2) go to 2465                             ! jump around diagnostic
  write (unit = munit6, fmt = 2464) j
2464 format (' Blank card recognized.  j =', i6)
  call window                                               ! output of character variable munit6
2465 do k = 1, numtyp                                       ! search class table to identify data type
     n8 = lentyp(k)                                         ! number of characters in k-th key word
     n14 = index (toLower (file6(j)), typdat(k)(1 : n8))              ! search for keyword k
     if (n14 .gt. 0) go to 2469                             ! yes, one of our data classes foun
  end do                                                    ! end  do 2464  loop seeking to identify blank card
  n14 = index (file6(j), 'tacs data')
  if (n14 .eq. 0) go to 2493                                ! only one class if new tacs
  k = 6                                                     ! data format, so set class no. to 6 (last one)
2469 n8 = k                                                 ! data class which this blank card terminates
  ! convert end of class blank card in old tacs data to comment card
  if (k .eq. 2 .or. k .eq. 3 .or. k .eq. 4 .or. k .eq. 5) file6(j)(1 : 2) = 'c '
2472 if (iprspy .lt. 3) go to 2475                          ! jump around diagnostic
  write (unit = munit6, fmt = 2474) n8
2474 format (' Data class needs consideration.  n8 =', i6)
  call window                                               ! output of character variable munit6
2475 do k = 1, n12                                          ! search rows of "/" table for class number n8
     if (kode(k) .ne. n8) go to 2484                        ! not correct class for blank
     n5 = kssfrq(k) + 1                                     ! beginning active data card of k-th "/" usage
     n6 = kpsour(k)                                         ! ending data card of k-th "/" usage
     if (n24 + n6 - n5 .ge. limcrd) go to 1760              ! overflow message
     do l = n5, n6                                          ! loop over each active card of k-th "/" usage
        n24 = n24 + 1                                       ! next destination card address we copy into
        file6(n24) = file6(l)                               ! insert "/" card in lower working area
     end do
     if (iprspy .lt. 3) go to 2484                          ! jump around diagnostic
     write (unit = munit6, fmt = 2479) n5, n6, n24
2479 format (' Done with "/" copy below.  n5, n6, n24 =', 3i6)
     call window                                            ! output of character variable munit6
  end do
2484 continue                                               ! end  do 2484  loop over all entries "k" in "/" table
  ! if just done with "/load flow" or
  if (n8 .eq. 10 .or. n8 .eq. 11) go to 2456                ! "/initial", special return
  if (j .le. 2) go to 2453                                  ! "/request" preceded nothing
2493 n24 = n24 + 1                                          ! next destination card address we copy into
  file6(n24) = file6(j)                                     ! copy original card j into low assembly
  if (n24 .ge. limcrd) go to 1760                           ! overflow error message
  if (j .lt. numcrd) go to 2453                             ! not finished; back for next j
2497 if (iprspy .lt. 2) go to 2500                          ! jump around diagnostic
  write (unit = munit6, fmt = 2499) n24
2499 format (' Done with lower assembly.  n24 =', i8)
  call window                                               ! output of character variable munit6
2500 j = 0                                                  ! initialize card destination index for upward shift
  do k = numcrd + 1, n24                                    ! consider each lower-assembled card k
     j = j + 1                                              ! final upper index, where card in lower row k goes
     file6(j) = file6(k)                                    ! shift card image upward to final position
     if (iprspy .lt. 9) go to 2505                          ! jump around diagnostic
     write (unit = munit6, fmt = 2501) j, file6(j)
2501 format (' Card', i3, '.', a80)
     call window                                            ! output of character variable munit6
  end do
2505 continue
  numcrd = j                                                ! final length of EMTP input data, after "/" removal
  if (iprspy .lt. 1) go to 2509                             ! jump around diagnostic
  write (unit = munit6, fmt = 2508) numcrd
2508 format (' Done with all "/" processing.  numcrd =', i8)
  call window                                               ! output of character variable munit6
  ! ?????  if interactive use,
2509 if (m4plot .eq. 1) call spying                         ! ????????   temporary diagnostic  ??????
  go to 9800                                                ! exit module after possible diagnostic
  ! Begin code to process "module" of user.  Convert user data
  ! from  "arg", "dum", and "num" character declarations to
  ! the vectors kard/karg/kbeg/kend/ktex as needed by $include
2613 iprspy = n13                                           ! diagnostic spy printout (0 or 99)
  numcrd = 0                                                ! erase memory of any previous data storage
  call spying                                               ! allow "data" usage to read input file
  n8 = 0                                                    ! initialize the number of declaration cards recognized
  numarg = 0                                                ! initialize the number of non-dummy arguments
  n11 = limcrd + 1                                          ! initialize destination address 1 past bottom
  n13 = 0                                                   ! initialize count of card being considered next
  n4 = 1                                                    ! temporary integer variable storage for unity
  !  call copyi (n4, modarg(1), limarg)                        ! initialize mode
  call copy (n4, modarg(1 :), limarg)                         ! initialize mode
  go to 2628                                                ! jump into loop over all input cards n13
2621 n11 = n11 - 1                                          ! next destination address (store from bottom up)
  file6(n11) = buff77                                       ! copy non-declaration card down below
2628 n13 = n13 + 1                                          ! next input card (natural order) considered
  if (n13 .gt. numcrd) go to 2703                           ! done processing all cards
  buff77 = file6(n13)                                       ! copy input card into working storage
  ! skip over any card ! not "arg" or "dum"
  if (toLower (buff77(1 : 3)) .ne. 'arg' .and. toLower (buff77(1 : 3)) .ne. 'dum' .and. toLower (buff77(1 : 3)) .ne. 'num') go to 2621 ! or "num"
  k = 4
  n8 = n8 + 1                                               ! one more declaration pushed to top of vector
  file6(n8) = buff77                                        ! store declaration after last such card
  ! start of next argument is
2637 if (buff77(k : k) .ne. ' ' .and. buff77(k : k) .ne. ',') go to 2648 ! non-"," & non-" "
  k = k + 1                                                 ! move one byte to right in search for next argument
  if (k .le. 80) go to 2637                                 ! loop back to try this new column
  go to 2628                                                ! done with this card, so loop back to read another
2648 l = k + 1                                              ! initialize search for right edge of argument
  ! blank or comma bounds
2656 if (buff77(l : l) .eq. ' ' .or. buff77(l : l) .eq. ',') go to 2664 ! right edge of argument
  l = l + 1                                                 ! move one byte to right in search for right edge
  if (l .le. 80) go to 2656                                 ! loop back to try this new column
2664 l = l - 1                                              ! right edge of argument is one byte to left of bound
  if (toLower (buff77(1 : 3)) .ne. 'num') go to 2687
  do m = 1, numarg                                          ! search existing argument list for this one
     n6 = l - k + 1                                         ! number of characters in argument just identified
     if (kolinc(m) .ne. n6) go to 2672                      ! wrong length; skip it
     if (arginc(m)(1 : n6) .ne. buff77(k : l)) go to 2672   ! no match
     modarg(m) = 0                                          ! indicate numeric storage (right adjust)
     go to 2695                                             ! exit loop; on to next argument on "num" card
  end do
2672 continue                                               ! end  do 2672  card searching for argument
  write (unit = munit6, fmt = 2679) buff77(k : l)
2679 format ('   ???  Illegal "num" declaration.  Unrecognized name = ', a)
  call window                                               ! output of character variable munit6
  call stoptp                                               ! installation-dependent program stop card
2687 numarg = numarg + 1                                    ! index for this input argument storage
  if (numarg .le. limarg) go to 2692                        ! no overflow yet
  write (unit = munit6, fmt = 2689) limarg
2689 format (' Overflow error stop.  Argument usage is limited in number to limarg =', i5)
  call window                                               ! output of character variable munit6
  call stoptp                                               ! installation-dependent program stop card
2692 arginc(numarg) = buff77(k : l)                         ! store newly-identified argument
  kolinc(numarg) = l - k + 1                                ! byte length of argument
  if (toLower (buff77(1 : 3)) .eq. 'dum') kkkdum(numarg) = 1
2695 k = l + 1                                              ! next left edge could be 1 beyond present right edge
  go to 2637                                                ! loop back to identify next argument of buff77
2703 n11 = n11 - 1                                          ! next destination address for $eof we create
  write (unit = ansi32, fmt = 2705) date1, tclock
2705 format (2a4, 2x, 2a4)
  file6(n11) = '$eof   user-supplied header cards follow.  '
  file6(n11)(51 : 68) = ansi32(1 : 18)                      ! add on date and time
  do j = 1, n8                                              ! loop over all declaration, now stored at top
     n11 = n11 - 1                                          ! next destination address for declaration card
     file6(n11) = file6(j)                                  ! transfer declaration from top to bottom
  end do
  if (iprspy .lt. 1) go to 2721                             ! jump around diagnostic
  write (unit = munit6, fmt = 2716) n8, n11, limcrd
2716 format (' Done processing declarations.  n8, n11, limcrd =', 3i5)
  call window                                               ! output of character variable munit6
  write (unit = munit6, fmt = 2717)
2717 format ('     row  kolinc  kkkdum  modarg  arginc ....')
  call window                                               ! output of character variable munit6
  do j = 1, numarg                                          ! print each row j of argument table
     write (unit = munit6, fmt = 2718) j, kolinc(j), kkkdum(j), modarg(j), arginc(j)
2718 format (4i8, a20)
     call window                                            ! output of character variable munit6
  end do                                                    ! end  do 2719  loop over all rows "j" of table
2721 n20 = 0                                                ! initialize number of argument usages found so far
  n16 = limcrd + 1                                          ! initialize location of next card considered
  n13 = 0                                                   ! so far, no active (non-comment) cards read
  do n17 = n11, limcrd                                      ! process all non-declation cards
     n16 = n16 - 1                                          ! reverse-indexed location of next non-decl. card
     buff77 = file6(n16)                                    ! transfer card to scalar working storage
     if (toLower (buff77(1 : 1)) .eq. 'c') go to 2766                 ! skip comment cards
     n13 = n13 + 1                                          ! present card has this active card number in file
     if (buff77(1 : 1) .eq. '/') go to 2766                 ! skip sorting commands
     if (toLower (buff77(1 : 4)) .eq. '$eof') go to 2772   ! effective file end
     do j = 1, numarg                                       ! check present card for each argument
        l = 1                                               ! begin search for string in column 1 of buff77
        n15 = kolinc(j)                                     ! number of characters in the j-th argument
2724    k = index (buff77(l :), arginc(j)(1 : n15))
        if (k .eq. 0) go to 2754                            ! no more j-th string usage
        n20 = n20 + 1                                       ! another argument usage found; advance storage
        if (n20 .le. 200) go to 2737                        ! not overflow working vectors
        write (unit = munit6, fmt = 2731) n16
2731    format ('  ====   Overflow error stop at card number', i6, '    over 200 arguments.')
        call window                                         ! output of character variable munit6
        call stoptp                                         ! installation-dependent program stop card
2737    karg(n20) = j                                       ! it is the j-th string which is used here
        ! if this is a dummy rather than real one,
        if (j .gt. numarg) karg(n20) = -(j - numarg)        ! store dummy arg number with "-" flag
        kard(n20) = n13                                     ! active card count of this record
        kbeg(n20) = l - 1 + k                               ! beginning card column number for string
        kend(n20) = kbeg(n20) + n15 - 1                     ! ending column number
        ! if this is a non-dummy argument, then
        if (j .le. numarg) ktex(n20) = modarg(j)            ! store right adjust flag (0=y, 1=n)
        l = kend(n20) + 1                                   ! search continues one byte beyond string end
        if (iprspy .lt. 6) go to 2749                       ! jump around diagnostic
        write (unit = munit6, fmt = 2748) n16, j, l, k, n20
2748    format ('    Another string found.  n16, j, l, k, n20 =', 5i6)
        call window                                         ! output of character variable munit6
2749    go to 2724                                          ! loop back to check for another appearance to right
     end do
2754 continue                                               ! end  do 2754  loop over each candidate string j
     if (iprspy .lt. 2) go to 2766                          ! jump around diagnostic
     write (unit = munit6, fmt = 2759) n16, n20, buff77
2759 format (' Done with this card.  n16, n20 =', 2i5, '   buff77=', a80)
     call window                                            ! output of character variable munit6
  end do
2766 continue                                               ! end  do 2766  loop over card number n16
2772 if (iprspy .lt. 1) go to 2778                          ! jump around diagnostic
  write (unit = munit6, fmt = 2777) n20
2777 format (' Done with identifying all arguments of all cards.   n20 =', i4)
  call window                                               ! output of character variable munit6
2778 if ((n20 / 25) * 25 .ne. n20) go to 2783               ! 25i3 cards not filled
  ! extra zero entry must be added, since otherwise card is full:
  n20 = n20 + 1                                             ! add zero entry, which goes on blank extra card
  kard(n20) = 0                                             ! only value actually used, of quintuplet
  karg(n20) = 0                                             ! remove possible garbage, to avoid output mess
  kbeg(n20) = 0                                             ! remove possible garbage, to avoid output mess
  kend(n20) = 0                                             ! remove possible garbage, to avoid output mess
  ktex(n20) = 0                                             ! remove possible garbage, to avoid output mess
2783 write (unit = prom80, fmt = 2787)
2787 format (' Send output file name for final $include file :')
  call prompt
  read (unit = munit5, fmt = 1329) buff77
  open (unit = lunit(13), status = 'new', file = buff77)
  rewind lunit(13)
  ansi8(1 : 4) = 'kard'
  write (unit = lunit(13), fmt = 2791) ansi8(1 : 4), (kard(j), j = 1, n20)
  ansi8(1 : 4) = 'karg'
  write (unit = lunit(13), fmt = 2791) ansi8(1 : 4), (karg(j), j = 1, n20)
  ansi8(1 : 4) = 'kbeg'
  write (unit = lunit(13), fmt = 2791) ansi8(1 : 4), (kbeg(j), j = 1, n20)
  ansi8(1 : 4) = 'kend'
  write (unit = lunit(13), fmt = 2791) ansi8(1 : 4), (kend(j), j = 1, n20)
  ansi8(1 : 4) = 'ktex'
  write (unit = lunit(13), fmt = 2791) ansi8(1 : 4), (ktex(j), j = 1, n20)
2791 format (a4, 25i3, /, (4x, 25i3))
  do j = n11, limcrd                                        ! next, dump all input data cards
     write (unit = lunit(13), fmt = 2802) file6(limcrd + n11 - j)
  end do
2802 format (a80)
  close (unit = lunit(13))
  go to 1311                                                ! back to original prompt at start of emtp execution
9200 nchain = 51                                            ! head for error overlays, for "kill" message
  lstat(18) = -1                                            ! overlay number presently being executed
9800 if (ntacs .ne. 2) go to 9002
  k = 1                                                     ! begin to convert tacs data without '/' cards
  do j = 1, numcrd
     if (toLower (file6(j)(1 : 5)) .ne. 'blank' .and. file6(j)(1 : 80) .ne. blan80(1 : 80)) go to 3005
     k = k + 1                                              ! counter for tacs data types
     if (k .ge. 6) go to 9002                               ! done with all 5 data types
     file6(j)(1 : 2) = 'c '                                 ! convert blank card to comment card
     go to 3500                                             ! except the last one to end tacs data
3005 select case (k)
     case (1)
        !convert type code '99' to
        if (file6(j)(1 : 2) .eq. '99') file6(j)(1 : 2) = '  '  !'  ' for functions
        go to 3500

     case (2)
        if (toLower (file6(j)(1 : 1)) .eq. 'c' .or. file6(j)(1 : 1) .eq. '9' .or. file6(j)(1 : 1) .eq. '$') go to 3500
        if (file6(j)(2 : 2) .ne. '1') go to 3022
        file6(j)(1 : 1) = '1'                               ! to convert tacs source cards:
        go to 3500                                          ! type ' 1' to '11', ' 2' to '14'

     case (3)
        go to 3500

     case (4)
        if (file6(j)(1 : 1) .ne. ' ') go to 3500
        file6(j)(1 : 2) = '33'                              !convert type '  ' to '33'
        go to 3500                                          !for tacs output variables

     case (5)
        if (file6(j)(1 : 1) .ne. ' ') go to 3500
        file6(j)(1 : 2) = '77'                              !convert type '  ' to '77'
     end select
3022 if (file6(j)(2 : 2) .ne. '2') go to 3024
     file6(j)(1 : 2) = '14'                                 ! ' 3' to '23', and ' 4' to '24'
     go to 3500
3024 file6(j)(1 : 1) = '2'
     go to 3500
  end do
3500 continue                                               !for tacs initial condition cards
9002 if (iprsup .lt. 1) go to 9007                          ! jump around diagnostic
  write (unit = munit6, fmt = 9004) numcrd, limcrd, kill
9004 format (' Exit "datain".   numcrd, limcrd, kill =', 3i8)
  call window                                               ! output of character variable munit6
9007 if (allocated (tank)) deallocate (tank)
  return
end subroutine datain

!
! end of file datain.f90
!
