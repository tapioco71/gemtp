!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file inlmfs.f90
!

!
! subroutine inlmfs.
!

subroutine inlmfs
  use blkcom
  use dekspy
  use strcom
  implicit none
  character(20) :: arginc(35)
  !  dimension kard(200), karg(200), kbeg(200), kend(200)
  !  dimension ktex(200)
  !  dimension kolinc(35)
  !     character*80  answ80,  file6(500)
  !     character*1  char1, digit(10)
  !     logical  logvar
  integer(4) :: incdat, ip
  integer(4) :: j
  integer(4) :: k, kard(200), karg(200), kbeg(200), keybrd, kend(200), kolinc(35)
  integer(4) :: komlev, ktex(200)
  integer(4) :: l
  integer(4) :: m
  integer(4) :: n, n1, n2, n3, n4, n5, n6, n7, n8, n12, n13, n14, n15, n16, n18
  integer(4) :: n19, n20, n24, n26
  !
  data komlev / -1 /
  if (buff77(1 : 1) .eq. '$') go to 1788                    !1st time in for $include
  n16 = kard(200)
  n26 = 2
  !     write(*,*) ' Encoding "c " card, n16=', n16
  if (buff77(1 : 2) .eq. 'c ') go to 4203                   !2nd cont. card
  write(unit = *, fmt = *) ' Invalid cont. card for $include argument list.'
  stop
1788 n26 = 9
1789 if (buff77(n26 : n26) .ne. ' ' .and. buff77(n26 : n26) .ne. ',') go to 1797
  n26 = n26 + 1
  if (n26 .lt. 40) go to 1789
1794 istep = j
  write (unit = *, fmt = *) ' Halt with bad $include in inlmfs.'
  stop
1797 k = n26 + 1
1801 if (buff77(k : k) .eq. ','  .or. buff77(k : k) .eq. ' ') go to 1804
  k = k + 1
  if (k .gt. 60) go to 1794
  go to 1801
1804 n7 = k - n26
  answ80(1 : n7) = buff77(n26 : k - 1)
  n8 = n7
  answ80(n8 + 1 : 80) = blan80(n8 + 1 : 80)
  l = numcrd
  n19 = limcrd
  j = numdcd
  !       write (*,*) ' move line con..  j, numcrd =',  j, numcrd
  do m = j, numcrd
     !      write (*,*) ' next line constants card.   m, file6(m) =',
     !    1                                           m, file6(m)
     file6(n19) = file6(m)
     n19 = n19 - 1
  end do
  !     n19 = n19 + 1
  write (unit = lunit(6), fmt = 1820) j, answ80(1 : n8)
1820 format ('   --- thl pass.  card =', i4, '.   Ready to open $include =', a)
  inquire (file = answ80(1 : n8), exist = logvar)
  if (logvar) go to 2294
  write (unit = lunit(6), fmt = 5823) answ80(1 : n8)
5823 format (/, ' Trouble with special  $include that is to service line model frequency scan.', /, &
          ' Halt in  inlmfs  called by  over2.  Bye, bye!' )
  stop
2294 prom80 = file6(j)
  file6(j) = 'c '//prom80(1:78)
  open (unit = lunit(13), status = 'old', file = answ80(1 : n8))
  n16 = 0
  n26 = k + 1
4203 do l = n26, 80                                         ! search cols. n26-80 for nonblank
     if (buff77(l : l + 1) .eq. '$$') go to 2006            ! if continue card, exit
     ! if not "," or blank,
     if (buff77(l : l) .ne. ',' .and. buff77(l : l) .ne. ' ') go to 4208 ! argument starts
  end do
  go to 4226                                                ! all arguments found; now use them
2006 write (unit = *, fmt = *) ' continuation next.   n13, n16, keybrd, incdat, l =', n13, n16, keybrd, incdat, l
  kard(200) = n16                                           ! save n16 when '$$' card is read
  return                                                    !  go back  to over2 for 2nd 'arg' card for lmfs
  !      n13 = n13 + 1 ! next lunt10 index below last $include: for continu
!!!!  call read10 ( n13, abuff ) ! load abuff with lunt10 card numbr n13
  !      buff77 = file6(n13)     ! transfer to scalar working storage
  !      write (*,*) ' continue is card  n13, buff77 =',
  !     1                                n13, buff77
  !      n26 = 2   ! reset column number to begin search for next abuff arg
  !      write (*,*) ' begin arg continuation  buff77 =',  buff77
  !      go to 4203   ! loop back to begin search for args on this continue
4208 n16 = n16 + 1                                          ! another (the n16-th) argument begins
  !      if ( n16 .gt. 10 )  ! if number of arguments is too large,
  ! current limit of 18 nodes (9-phase)
  if (n16 .gt. 18) call stoptp
  n12 = index (buff77(l :), ',')
  n13 = index (buff77(l :), ' ')
  n14 = n12
  if (n12 .gt. 0) go to 4214
  n14 = n13
  if (n13 .gt. 0) go to 4220
  write (unit = munit6, fmt = 4211)
4211 format (' No bounding symbol.  Stop after display.')
  call window
  write (unit = munit6, fmt = 4223) l, n12, n13, n14, n26
  call window
  call stoptp
4214 if (n13 .gt. 0 .and. n13 .lt. n14) n14 = n13
4220 n15 = n14 - 1
  kolinc(n16) = n15
  n18 = l - 1 + n15
  arginc(n16) = ' '
  arginc(n16)(1 : n15) = buff77(l : n18)
  n26 = n18 + 1
  if (iprspy .lt. 5) go to 4224
  write (unit = munit6, fmt = 4223) l, n12, n13, n14, n26
4223 format (' Done with argument.  l, n12, n13, n14, n26 =', 8i6)
  call window
4224 go to 4203
4226 kard(1) = 999999
  n1 = 0
  if (n16 .eq. 0) go to 4239
4228 read (unit = lunit(13), fmt = 4232) (kbeg(l), l = 1, 25)
  do l = 1, 25
     if (kbeg(l) .eq. 0) go to 4230
  end do
  n1 = n1 + 25
  if (n1 .le. 175) go to 4228
  call stoptp
4230 rewind lunit(13)
  n6 = n1 + l - 1
  read (unit = lunit(13), fmt = 4232) (kard(k), k = 1, n6)
  read (unit = lunit(13), fmt = 4232) (karg(k), k = 1, n6)
  read (unit = lunit(13), fmt = 4232) (kbeg(k), k = 1, n6)
  read (unit = lunit(13), fmt = 4232) (kend(k), k = 1, n6)
  read (unit = lunit(13), fmt = 4232) (ktex(k), k = 1, n6)
4232 format (4x, 25i3)
  if (iprspy .lt. 1) go to 4235
  write (unit = munit6, fmt = 4233) n6
4233 format (' Done reading argument usage vectors.  n4 =', i5)
  call window
4235 if (iprspy .lt. 5) go to 4238
  write (unit = munit6, fmt = 4236)
4236 format (' Vectors kard, karg, kbeg, kend, ktext(1:25) ...')
  call window
  write (unit = munit6, fmt = 4237) (kard(k), k = 1, n5)
  call window
  write (unit = munit6, fmt = 4237) (karg(k), k = 1, n5)
  call window
  write (unit = munit6, fmt = 4237) (kbeg(k), k = 1, n5)
  call window
  write (unit = munit6, fmt = 4237) (kend(k), k = 1, n5)
  call window
  write (unit = munit6, fmt = 4237) (ktex(k), k = 1, n5)
4237 format (4x, 25i3)
  call window
4238 kard(n6 + 1) = 999999
4239 n20 = 0
  n24 = 1
  n18 = j + 1
  do k = n18, limcrd
     read (unit = lunit(13), fmt = 1329, end = 1828) buff77
1329 format (a80)
     if (to_lower (buff77(1 : 1)) .ne. 'c') go to 4247
     do l = 1, 10
        if (buff77(2 : 2) .eq. digit(l)) go to 4244
     end do
     go to 4247
4244 if (l .eq. 10) l = 0
     if (l .gt. komlev) go to 4273
     buff77(2 : 2) = ' '
4247 j = j + 1
     file6(j) = buff77
     if (to_lower (buff77(1 : 1)) .eq. 'c') go to 4273
     n20 = n20 + 1
     if (iprspy .lt. 3) go to 4249
     write (unit = munit6, fmt = 4248) n20, n24, kard(n24)
4248 format (' Ready with next card.  n20, n24, kard(n24) =', 3i8)
     call window
4249 if (n20 .lt. kard(n24)) go to 4273
     n1 = kbeg(n24)
     n2 = kend(n24)
     n4 = karg(n24)
     n3 = kolinc(n4)
     if (n4 .le. n16) go to 34250
     write (unit = lunit(6), fmt = 4250) n24, n4, n16
4250 format ('   ? ? ? ?   Error stop at s.n. 4250 of "datain".   Insufficient number of $include arguments.', /, '             n24, n4, n16 =', 3i8)
     call stoptp
34250 if (n2 - n1 .ne. n3 - 1 .and. ktex(n24) .eq. 1) go to 4253
     if (n2 - n1 .ge. n3 - 1) go to 4261
4253 write (unit = munit6, fmt = 4254) n4, n20
4254 format ('   +++ Argument',  i4,  '   length-mismatch error.   used on card', i4, ' .')
     call window
     write (unit = munit6, fmt = 4255) n24, n1, n2, n3
4255 format ('       n24 =', i4, '    kbeg, kend =', 2i4, '    length from $include =', i4, ' .')
     call window
     stop
4261 n = n2
     if (iprspy .lt. 4) go to 34261
     write (unit = munit6, fmt = 24261) arginc(n4)
24261 format (' Argument now processed, arginc(n4) =', a20)
     call window
34261 do ip = 1, 20
        m = 21 - ip
        char1 = arginc(n4)(m : m)
        if (char1 .eq. ' ') go to 4263
        if (iprspy .lt. 8) go to 54262
        write (unit = munit6, fmt = 4262) ip, m, char1
4262    format ('  Next non-blank digit.  ip, n, digit =', 2i6, '   "', a1, '"')
        call window
54262   if (char1 .eq. '#') char1 = ' '
        if (n .lt. n1) go to 4253
        file6(j)(n : n) = char1
        n = n - 1
     end do
4263 continue
     if (n + 1 .gt. n1 .and. ktex(n24) .eq. 1) go to 4253
     if (n .ge. n1) file6(j)(n1 : n) = ' '
     n24 = n24 + 1
     go to 4249
  end do
4273 continue
  stop
1828 close (unit = lunit(13), status = 'keep')
  if (iprspy .lt. 1) go to 1832
  write (unit = munit6, fmt = 1831) j, n19
1831 format (' Done with disk file (close).  j, n19 =', 2i8)
  call window
1832 buff77(1 : 32) = 'c end of $include.  File name = '
  buff77(33 : 80) = answ80(1 : 48)
  j = j + 1
  file6(j) = buff77
  n13 = limcrd + 1
  do m = n19, limcrd
     j = j + 1
     n13 = n13 - 1
     file6(j) = file6(n13)
  end do
  numcrd = j
  !     write (*,*) ' exit inlmfs.   numdcd, numcrd =',
  !    1                             numdcd, numcrd
  return
end subroutine inlmfs

!
! end of file inlmfs.f90
!
