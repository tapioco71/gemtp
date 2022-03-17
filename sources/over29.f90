!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over29.f90
!

! Copyright 1977-2021 Bonneville Power Administration
! Copyright 2019-2021 Angelo Rossi <angelo.rossi.homelab@gmail.com>
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice,
!    this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! 3. Neither the name of the copyright holder nor the names of its contributors
!    may be used to endorse or promote products derived from this software
!    without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.

!
! subroutine subr29.
!

subroutine subr29
  use blkcom
  use deck29
  use com29
  implicit none
  real(8), allocatable, target :: array(:)
  integer(4) :: iofkms, iofkde, iofibr, iofjbr, iofake, iofbus, ioftcl, ioftop, ioftst
  integer(4) :: j
  integer(4) :: ll0
  integer(4) :: n9
  !  dimension text(1), array(1)
  !  equivalence (karray(1), text(1), array(1))
  !
  character(8), pointer :: text(:)
  !
  ll0 = size (transfer (karray, array))
  allocate (array(ll0))
  array = transfer (karray, array)
  text = transfer (array, text)
  !
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2371) lswtch, lsiz12, lbus
2371 format (' At top of "subr29".  lswtch, lsiz12, lbus =', 3i8)
  nvar = lswtch
  if (lsiz12 .gt. lswtch) nvar = lsiz12
  call dimens (karray(1), nchain, trash, trash)
  do j = 1, 9999, 2
     if (karray(j) .ne. 0) go to 2364
     maxo29 = karray(j + 1)
     go to 2368
2364 end do
  call stoptp
2368 iofbus = 1
  iofkms = (iofbus + 2 * lbus) * nbyte(1) / nbyte(4)
  iofkde = iofkms + 2 * lswtch
  iofibr = iofkde + lswtch
  iofjbr = iofibr + lsiz12
  iofake = (iofjbr + lsiz12) * nbyte(4) / nbyte(3) + 1
  ioftst = iofake + lswtch
  ioftcl = ioftst + lswtch
  ioftop = ioftcl + lswtch
  iofarr = ioftop + 2 * lswtch
  n9 = iofarr * nbyte(3) / nbyte(4)
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 2388) nvar, (nbyte(j), j = 1, 4), maxo29, iofbus, iofkms, iofkde, iofibr, iofjbr, iofake, ioftst, ioftcl, ioftop, iofarr, n9
2388 format (' After offset computation.  nvar, nbyte(1:4) =', 5i5, 5x, 'maxo29 =', i8, /, '   iofbus  iofkms  iofkde  iofibr  iofjbr  iofake  ioftst  ioftcl  ioftop  iofarr', /, 1x, 10i8)
  if (n9 .lt. maxo29) go to 3462
  write (unit = lunit(6), fmt = 3457) n9, maxo29
3457 format (/, ' Temporary error stop in "subr29" of overlay 29.   Working storage of /c29b01/ ("vardim" output) must equal at least', i7, '   words', /, ' to solve the problem, although user-dimensioning only provided for', i7, '   words (both integer figures).   Redimension the EMTP.')
  call stoptp
  !3462 call guts29 (text(iofbus), karray(iofkms), karray(iofkde), karray(iofibr), karray(iofjbr), array(iofake), array(ioftst),  array(ioftcl), array(ioftop))
3462 call guts29 (text(iofbus :), karray(iofkms :), karray(iofkde :), karray(iofibr :), karray(iofjbr :), array(iofake :), array(ioftst :), array(ioftcl :), array(ioftop :))
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3489)  iofarr, kill
3489 format (' At exit of "subr29".  iofarr, kill =', 2i8)
  karray = transfer(array, karray)
  if (allocated(array)) deallocate(array)
  return
end subroutine subr29

!
! subroutine plotng.
!

subroutine plotng (kkrv, kklm)
  use blkcom
  implicit none
  integer(4), intent(in) :: kkrv, kklm
  !
  character(1) :: klank, kom, kut(131), ktsin(4), kdig(10)
  integer(4) :: j
  integer(4) :: klm, krv
  integer(4) :: n1, n4, n5, n6, n7, n8, n14
  !     universal module called only by "innr29" of overlay 29.
  !     some logic is similar to "linplt" of overlay 31.
  !  include 'blkcom.ftn'
  !  dimension  kut(131), ktsin(4), kdig(10)
  !     Burroughs: preserve local variable between module calls:
  !
  data kut(1)  / ' ' /
  data ktsin(1) / 'a' /
  data ktsin(2) / 'b' /
  data ktsin(3) / 'c' /
  data ktsin(4) / 'd' /
  data kom / '*' /
  data klank / ' ' /
  data kdig(1) / '1' /
  data kdig(2) / '2' /
  data kdig(3) / '3' /
  data kdig(4) / '4' /
  data kdig(5) / '5' /
  data kdig(6) / '6' /
  data kdig(7) / '7' /
  data kdig(8) / '8' /
  data kdig(9) / '9' /
  data kdig(10) / '0' /
  !
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1349) kkrv, kklm
1349 format (' Top of "plotng".  kkrv, kklm =', 2i8)
  krv = kkrv
  klm = kklm
  if (klm .eq. -5678) go to 350
  if (klm .gt. 131) go to 5831
  if (klm .eq. -4321) go to 4832
  if (klm .ge. 0) go to 3652
  write (unit = lunit(6), fmt = 230) kut
230 format (1x, 131a1)
350 do j = 1, 131
     kut(j) = klank
  end do
  kut(5) = kdig(1)
  go to 9000
  !     begin code to add curve "krv" to column "klm" of kut(131)
3652 if (krv .ne. 2) go to 3664
  if (kut(47) .eq. klank) kut(47) = kdig(1)
3664 if (krv .ne. 3) go to 3671
  if (kut(89) .eq. klank) kut(89) = kdig(1)
3671 if (kut(klm) .eq. klank) go to 3688
  if (kut(klm) .eq. kdig(1)) go to 3688
  kut(klm) = kom
  go to 9000
3688 kut(klm) = ktsin(krv)
  go to 9000
  !     begin code to encode mean closing time stored in tolmat:
4832 n4 = 6
  if (krv .eq. 2) n4 = 48
  if (krv .eq. 3) n4 = 90
  n5 = n4 + 11
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4834) n4, tolmat
4834 format (' lunt12 write.  n4, tolmat =', i8, e15.3)
  rewind lunit(12)
  write (unit = lunit(12), fmt = 4837) tolmat
4837 format (e12.3)
  rewind lunit(12)
  read  (unit = lunit(12), fmt = 4844) (kut(j), j = n4, n5)
4844 format (12a1)
  go to 9000
  !     begin code to encode 3-digit klm on right edge of kut:
5831 if (krv .eq. 1) klm = klm - 5
  if (krv .eq. 2) klm = klm - 47
  if (krv .eq. 3) klm = klm - 89
  if (klm .lt. 1000) go to 5856
  write (unit = lunit(6), fmt = 5845) krv, klm
5845 format (' Too large klm in "plotng".  Stop.   krv, klm =', 2i8, 8x, 'kut(1:131) ...', /,  1x, 131a1)
5856 n6 = klm / 100
  n1 = klm - 100 * n6
  n7 = n1 / 10
  n8 = n1 - 10 * n7
  if (n7 .eq. 0) n7 = 10
  if (n8 .eq. 0) n8 = 10
  n14 = 120
  if (krv .eq. 2) n14 = 124
  if (krv .eq. 3) n14 = 128
  kut(n14) = kdig(n6)
  kut(n14 + 1) = kdig(n7)
  kut(n14 + 2) = kdig(n8)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 9045) krv, klm, n14, n6, n7, n8, kut
9045 format (' Encode klm.   krv, klm, n14, n6, n7, n8 =', 6i6, 8x, 'kut(1:131) ....', /, 1x, 131a1)
9000 if (iprsup .ge. 3) write (unit = lunit(6), fmt = 9004) krv, klm, kut
9004 format (' Exit "plotng",  krv, klm =', 2i6, 8x, 'kut(1:131) follows .....', /, 1x, 131a1)
  return
end subroutine plotng

!
! subroutine fltdat.
!

subroutine fltdat (array, arrsav, soln, rhs, ymat, kspars, maxsq, nmax2, nmax, lspars)
  use blkcom
  use tracom
  use freedom
  use movcop
  use pckcom
  implicit none
  integer(4), intent(inout) :: kspars(lspars, lspars)
  integer(4), intent(in) :: lspars
  integer(4), intent(in) :: maxsq
  integer(4), intent(in) :: nmax, nmax2
  real(8), intent(inout) :: array(maxsq, maxsq), arrsav(maxsq, maxsq)
  real(8), intent(inout) :: rhs(maxsq)
  real(8), intent(inout) :: soln(maxsq)
  real(8), intent(inout) :: ymat(nmax, nmax2)
  !
  !  dimension array(maxsq, maxsq), arrsav(maxsq, maxsq)
  !  dimension soln(maxsq), rhs(maxsq), ymat(nmax, nmax2)
  !  dimension kspars(lspars,lspars)
  character(1) :: kblank, kfill, korig
  character(8) :: chara, charb, charc, charq
  character(8) :: texta(35), textb(35), text1(6), text2(6), text9(1), text80(14)
  !     Following dimensioned vector with "35" really should be
  !     variably-dimensioned to "iofbnd" user-requested size.  But
  !     it is easier to leave them fixed, bigger than ever needed:
  integer(4) :: i, ichara(35), icharb(35), ipncom
  integer(4) :: j
  integer(4) :: k, kk
  integer(4) :: l, limit, ll35, loop
  integer(4) :: m, maxcol
  integer(4) :: n2, n4, n5, n6, n7, n8, n12, n13, n18, negate(35), niter, nitmax
  integer(4) :: ntot2, ntotm1, ntotp1, ntotsq
  real(8) :: admit(35)
  real(8) :: d2, d3, d4, d6, d7, d13, drive
  real(8) :: from(maxsq * maxsq), from2(nmax * nmax2)
  real(8) :: output(35)
  real(8) :: percen
  real(8) :: rthev(35), rzero(35)
  real(8) :: scale
  real(8) :: to(maxsq * maxsq)
  real(8) :: xthev(35), xzero(35)
  !  dimension  texta(35), textb(35), rzero(35), xzero(35)
  !  dimension  ichara(35), icharb(35), admit(35)
  !  dimension  text80(14), text1(6), text2(6), text9(1)
  !
  data korig / 'x' /
  data kfill / 'f' /
  data kblank / ' ' /
  data chara / 'a     ' /
  data charb / 'b     ' /

  data charc / 'c     ' /
  data charq / '?     ' /
  !
  ll35 = 35
  drive = 1.0d0
  limit = maxsq ** 2
5270 loop = 1
  call move0 (negate(1 :), ll35)
  !     enter loop over sequence networks (zero first, then positive):
5286 n13 = 2 * maxsq
  from2 = reshape (ymat, (/ nmax*nmax2 /))
  !  call mover0 (ymat(1, 1), n13)
  call move0 (from2, n13)
  ymat = reshape (from2, (/ nmax, nmax2 /))
  iprsup = iprsov(nchain)
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 5306) nitmax, n13, epsiln, percen, ipunch, ipncom, bus1, bus2, bus3, bus4
5306 format ( 2i8, 2e8.0, 2i8, 4x, 4a1 )
  if (nitmax .gt. 0 .or. epsiln .gt. 0.0d0) go to 5311
  write (unit = lunit(6), fmt = 5308)
5308 format ('+Blank card terminates "fdu" subcases.')
  go to 9900
5311 write (unit = lunit(6), fmt = 5314)  nitmax, n13, epsiln, percen, ipncom, bus1, bus2, bus3, bus4
5314 format ('+misc.', 2i5, 2e12.3, i4, 1x, 4a1)
  if (iprsup .le. 0) iprsup = n13
  if (iprsup .le. 0) iprsup = 1
  if (bus1 .ne. blank) chara = bus1
  if (bus2 .ne. blank) charb = bus2
  if (bus3 .ne. blank) charc = bus3
  if (bus4 .ne. blank) charq = bus4
  scale = percen
  ntot = 0
  !     enter loop with top at 5321 where another branch card is read:
  !     read input card using cimage.
5321 call cimage
  read (unit = abuff, fmt = 5326) k, m, d6
5326 format (2i8, e8.0)
  if (k .le. 0 .or. m .le. 0 .or. d6 .le. 0) go to 5347
  if (k .gt. ntot) ntot = k
  if (m .gt. ntot) ntot = m
  if (ntot .le. nmax) go to 5335
  write (unit = lunit(6), fmt = 5331) ntot
5331 format (/, '  @@@@@@@  overflow error stop.  Generator or node number is too large.   ntot =', i5)
  kill = 1
  lstat(19) = 5331
  lstat(16) = 23
  go to 9900
5335 write (unit = lunit(6), fmt = 5336) k, m, d6
5336 format ('+branch card.  nodes =', 2i4, '   x =', e13.4)
  d6 = 1.0d0 / d6
  ymat(k, m) = -d6
  ymat(m, k) = -d6
  ymat(k, k) = ymat(k, k) + d6
  ymat(m, m) = ymat(m, m) + d6
  go to 5321
5347 write (unit = lunit(6), fmt = 5348)
5348 format ('+Blank card terminating line cards.')
  ntot2 = 2 * ntot
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5354) ntot
5354 format (' admittance matrix follows.   ntot =', i3)
  ntotsq = ntot * ntot
  ntotp1 = ntot + 1
  ntotm1 = ntot - 1
  maxcol = 2 * ntot - 1
  do j = 1, ntot
     n12 = ntot + j
     ymat(j, n12) = 1.0d0
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 5357) l, texta(l), textb(l), rthev(l), xthev(l), n13
5357 format (i2, 2a6, 2e10.0, i6)
     read (unit = abuff, fmt = 5358) text1, text2
5358 format (2x, 12a1)
     write (unit = lunit(6), fmt = 5360) l, rthev(l), xthev(l), n13
5360 format ('+Thevenin.', i6, 2e13.4, i6)
     if (negate(l) .eq. 0) negate(l) = n13
     n4 = 0
     do m = 1, 6
        if (text1(m) .ne. charq) go to 5361
        ichara(l) = m
        n4 = n4 + 1
5361    if (text2(m) .ne. charq) go to 5363
        icharb(l) = m
        n4 = n4 + 1
5363 end do
     if (n4 .ge. 2) go to 5368
     ichara(l) = 6
     icharb(l) = 6
     do  k = 2, 6
        if (text1(k) .eq. blank .and. text1(k - 1) .eq. chara) ichara(l) = k - 1
        if (text2(k) .eq. blank .and. text2(k - 1) .eq. chara) icharb(l) = k - 1
     end do
5368 admit(l) = 1.0d0 / xthev(l)
     if (iprsup .ge. 2)  write (unit = lunit(6), fmt = 5371) l, rthev(l), xthev(l), l, (ymat(l, l), l = 1, ntot)
5371 format (' row', i2, '.   X/R =', f11.3, '   Xthev =', f11.3, '   y(', i2, ',1:ntot) =', 3e13.5, /, 1x, 10e13.5)
  end do
  !     Next assemble constant part of Jacobian in arrsav array.  This
  !     is top of loop over admittance level (for sequential adjustment
  !     of [y] to minimize shock to newton solution):
5379 n7 = 0
  !  call mover0 (arrsav(1, 1), limit)
  from = reshape (arrsav, (/ maxsq*maxsq /))
  call move0 (from, limit)
  arrsav = reshape (from, (/ maxsq, maxsq /))
  do kk = 1, ntot
     n8 = n7
     do i = 1, ntot
        n7 = n7 + 1
        if (scale .gt. percen) go to 5385
        soln(n7) = 0.0d0
        if (i .eq. kk) soln(n7) = admit(kk)
5385    n4 = (i - 1) * ntot + i
        !     arrsav(n7,n4) = -gen(i)          ! df(i)/dys(i) = -vs(i)
        if (lspars .eq. 1) go to 5395
        do l = 1, ntotsq
           kspars(n7, l) = ichar (kblank)
        end do
        kspars(n7, n4) = ichar (korig)
5395    if (i .eq. kk) arrsav(n7, n7) = arrsav(n7, n7) + drive
        do j = 1, ntot
           if (j .eq. kk) go to 5402
           arrsav(n7, n8 + j) = scale * ymat(i, j)
           if (lspars .ne. 1) kspars(n7, n8 + j) = ichar (korig)
5402    end do
     end do
  end do
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5425) (soln(j), j = 1, ntotsq)
5425 format (' Initial guess soln(1:ntotsq) =', 6e15.6, /, 1x,  8e15.6)
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5437)
5437 format (/, ' Partial constant Jacobian follows.')
  do i = 1, ntotsq
     if (iprsup .ge. 3) write (unit = lunit(6), fmt = 5443) (arrsav(i, j), j = 1, ntotsq)
5443 format (1x, 10e13.5)
  end do
  if (iprsup .le. 1) go to 5458
  if (lspars .eq. 1) go to 5458
  write (unit = lunit(6), fmt = 5753)
  do k = 1, ntotsq
     write (unit = lunit(6), fmt = 5755) k, (kspars(k, l), l = 1, ntotsq)
  end do
5458 niter = 0
  write (unit = lunit(6), fmt = 5443)
  go to 5723
  !     top of loop for next Newton iteration:
  !5481 call mover (arrsav(1, 1), array(1, 1), limit)
  from = reshape (arrsav, (/ maxsq*maxsq /))
  to = reshape (array, (/ maxsq*maxsq /))
5481 call mover (from, to, limit)
  arrsav = reshape (from, (/ maxsq, maxsq /))
  array = reshape (to, (/ maxsq, maxsq /))
  n7 = 0
  !     inside do 5506, rhs(i) is function f(i);  newton "-" added at 5529
  do kk = 1, ntot
     n8 = n7
     do i = 1, ntot
        n7 = n7 + 1
        n4 = (i - 1) * ntot + i
        if (i .eq. kk) rhs(n7) = (soln(n7) - admit(i)) * drive
        if (i .ne. kk) rhs(n7) = soln(n4) * soln(n7)
        !     enter loop to evaluate sum over j of y(i,j)*v(j) for j .ne. i:
        d13 = ymat(i, kk) * drive
        n2 = n8
        do j = 1, ntot
           n2 = n2 + 1
           if (j .eq. kk) go to 5492
           d13 = d13 + ymat(i, j) * soln(n2)
5492    end do
        if (iprsup .ge. 6) write (unit = lunit(6), fmt = 5496) kk, i, n4, n7, n2,  scale, rhs(n7), d13
5496    format(' Below 5492.  kk, i, n4, n7, n2 =', 5i5, /, 'scale, rhs(n7), d13 =', 3e15.5)
        rhs(n7) = rhs(n7) + scale * d13
        if ( i .eq. kk )  go to 5502
        array(n7, n7) = array(n7, n7) + soln(n4)
        array(n7, n4) = array(n7, n4) + soln(n7)
        if (iprsup .le. 1) go to 5502
        if (lspars .eq. 1) go to 5502
        kspars(n7, n7) = ichar (korig)
        kspars(n7, n4) = ichar (korig)
5502 end do
  end do
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5517)
5517 format (/, ' Final Jacobian after variable part has been added.  rhs = function evaluation (also done).')
  do i = 1, ntotsq
     if (iprsup .ge. 4) write (unit = lunit(6), fmt = 5527) i, rhs(i), (array(i, j), j = 1, ntotsq)
5527 format (i3, '. rhs =', e15.6, '   y(i,1:ntotsq) =', 6e15.6, /, 1x, 8e15.6)
     rhs(i) = -rhs(i)
  end do
  if (iprsup .le. 1) go to 5538
  if (lspars .eq. 1) go to 5538
  write (unit = lunit(6), fmt = 5753)
  do k = 1, ntotsq
     write (unit = lunit(6), fmt = 5755) k, (kspars(k, l), l = 1, ntotsq)
  end do
5538 do i = 1, ntotsq
     if (i .eq. 1) go to 5563
     n6 = i - 1
     do j = 1, n6
        if (array(i, j) .eq. 0.0d0)  go to 5561
        if (lspars .eq. 1) go to 5541
        if (kspars(i, j) .eq. ichar (kblank)) kspars(i, j) = ichar (kfill)
5541    d3 = array(i, j)
        array(i, j) = 0.0d0
        n8 = j + 1
        do l = n8, ntotsq
           array(i, l) = array(i, l) - d3 * array(j, l)
        end do
        rhs(i) = rhs(i) - d3 * rhs(j)
        if (iprsup .ge. 5) write (unit = lunit(6), fmt = 5549) i, j, rhs(i), (array(i, l), l = 1, ntotsq)
5549    format(' zero (', i2, ',', i2, '). rhs(i) =', e15.6, '   a(i,1:n9) =', 5e15.6, /, 1x, 8e15.6)
5561 end do
5563 if (iprsup .ge. 4) write (unit = lunit(6), fmt = 5564) i, i, array(i, i)
5564 format (' Diagonal ready to be reciprocated,  a(', i2, ',',  i2, ') =', e15.6  )
     d4 = 1.0d0 / array(i, i)
     array(i, i) = 1.0d0
     if (i .eq. ntotsq) go to 5574
     n8 = i + 1
     do l = n8, ntotsq
        if (array(i, l) .eq. 0.0d0) go to 5572
        array(i, l) = d4 * array(i, l)
        if (lspars .eq. 1) go to 5572
        if (kspars(i, l) .eq. ichar (kblank)) kspars(i, l) = ichar (kfill)
5572 end do
5574 rhs(i) = d4 * rhs(i)
     if (iprsup .ge. 3) write (unit = lunit(6), fmt = 5576) i, rhs(i), (array(i, j), j = 1, ntotsq)
5576 format (' Final row', i3, '.  rhs =', e15.6, '   a(i,1:ntotsq) =', 5e15.6, /, 1x, 8e15.6)
  end do
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5594)
5594 format (' Done with downward, begin backsubstitution.')
  i = ntotsq
5614 i = i - 1
  n8 = i + 1
  do j = n8, ntotsq
     if (array(i, j) .eq. 0.0d0) go to 5627
     rhs(i) = rhs(i) - array(i, j) * rhs(j)
5627 end do
  if (i .gt. 1) go to 5614
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5697) ( rhs(j), j = 1, ntotsq)
5697 format (' Complete correction vector:', 6e15.6, /, 1x, 8e15.6)
  d4 = 0.0d0
  do j = 1, ntotsq
     if (absz (rhs(j)) .gt. d4) d4 = absz (rhs(j))
     soln(j) = soln(j) + rhs(j)
  end do
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 5714) niter, d4
5714 format (' Largest correction at Newton iteration  niter =', i3, '  is  d4 =', e15.6)
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 5716) (soln(i), i = 1, ntotsq)
5716 format (' Corrected solution estimate:', 6e15.6, /, 1x, 8e15.6)
  if (d4 .lt. epsiln) go to 5729
5723 niter = niter + 1
  if (niter .le. nitmax) go to 5481
  write (unit = lunit(6), fmt = 5727) nitmax, d4, scale
5727 format (/, ' ***********  Nonconvergence error.  Iteration limit has been reached.   nitmax =', i3, /,  '   peak correction (abs) =', e15.6, '   for admittance level "scale" =', f6.3)
  go to 5778
5729 if (absz (scale - 1.0d0) .gt. epsiln) go to 5782
  !     problem is solved;  only conversion from "y" to "x" remains:
  write (unit = lunit(6), fmt = 5737)
5737 format (/, ' Final generator equivalent impedances, plus confirmation of original generator parameters.', /, ' Resistance is ignored during the internal computation, and added as fixed % only at end.', /, ' X-fault is short circuit reactance computed after removal of user-flagged generators.', /,  3x, 'gen',  10x, 'X-new', 10x, 'R-new', 8x, 'X-fault', 9x, 'X-thev', 7x, 'X/R-thev')
  n18 = 0
  rewind lunit(1)
  !     next we add y-gen to [y], to calculate short circuit current:
  do j = 1, ntot
     if (negate(j) .ne. 0) go to 5734
     n8 = (j - 1) * ntot + j
     ymat(j, j) = ymat(j, j) + soln(n8)
5734 end do
  !     next we invert total [y], to give s.s. currents as diagonals:
  do k = 1, ntot
     d3 = 1.0d0 / ymat(k, k)
     n6 = k + 1
     do l = n6, ntot2
        ymat(k, l) = ymat(k, l) * d3
     end do
     do m = 1, ntot
        if (m .eq. k) go to 6748
        d2 = ymat(m, k)
        do l = n6, ntot2
           ymat(m, l) = ymat(m, l) - d2 * ymat(k, l)
        end do
6748 end do
  end do
  !     done with ymat inversion;  output generator goodies:
  do j = 1, ntot
     n8 = (j - 1) * ntot + j
     output(j) = fltinf
     if (soln(n8) .ne. 0.0d0) output(j) = 1.0d0 / soln(n8)
     d7 = output(j) / rthev(j)
     n2 = ntot + j
     write (unit = lunit(6), fmt = 5739) j, output(j), d7, ymat(j,n2), xthev(j), rthev(j)
5739 format (1x, i5, 4e15.6, f15.3)
     if (negate(j) .eq. 1) output(j) = fltinf
     d7 = output(j) / rthev(j)
     if (loop .eq. 2) go to 5740
     rzero(j) = d7
     xzero(j) = output(j)
     go to 5746
     !     with both z-ground and z-sky modes now known, punch branch cards:
5740 if (negate(j) .ne. 0) go to 5746
     n6 = 51
     n4 = ichara(j)
     n5 = icharb(j)
     text9(1) = chara
     !     call packa1 ( text9(1), texta(j), n4 )
     call pack (text9(1), texta(j), n4)
     !     call packa1 ( text9(1), textb(j), n5 )
     call pack (text9(1), textb(j), n5)
     if (ipncom .eq. 0) go to 5743
     write (unit = lunit(7), fmt = 5741)
5741 format ('c')
     write (unit = lunit(1), fmt = 5742)
5742 format (' c')
     n18 = n18 + 1
5743 if (ipunch .eq. 0) write (unit = lunit(7), fmt = 5744) n6, texta(j), textb(j), rzero(j), xzero(j)
5744 format (i2, ',',  2(a6, ','), 6x, ',,,', e23.14, ',', e23.14, ',,,,,,')
     write (unit = lunit(1), fmt = 5745) n6, texta(j), textb(j), rzero(j), xzero(j)
5745 format (i3, ',', 2(a6, ','), 6x, ',,,', e23.14, ',', e23.14, ',,,,,,')
     n18 = n18 + 1
     n4 = ichara(j)
     n5 = icharb(j)
     text9(1) = charb
     !     call packa1 ( text9(1), texta(j), n4 )
     call pack (text9(1), texta(j), n4)
     !     call packa1 ( text9(1), textb(j), n5 )
     call pack (text9(1), textb(j), n5)
     n6 = 52
     if (ipunch .eq. 0) write (unit = lunit(7), fmt = 5744) n6, texta(j), textb(j), d7, output(j)
     write (unit = lunit(1), fmt = 5745) n6, texta(j), textb(j), d7, output(j)
     n18 = n18 + 1
     text9(1) = charc
     !     call packa1 ( text9(1), texta(j), n4 )
     call pack (text9(1), texta(j), n4)
     !     call packa1 ( text9(1), textb(j), n5 )
     call pack (text9(1), textb(j), n5)
     n6 = 53
     if (ipunch .eq. 0) write (unit = lunit(7), fmt = 2744) n6, texta(j), textb(j)
2744 format (i2, ',',  2(a6, ','), 6x, ',,,,,,,,,,')
     write (unit = lunit(1), fmt = 2745)  n6, texta(j), textb(j)
2745 format (i3, ',', 2(a6, ','), 6x, ',,,,,,,,,,')
     n18 = n18 + 1
5746 end do
  if (iprsup .le. 1) go to 5764
  if (lspars .eq. 1) go to 5764
  write (unit = lunit(6), fmt = 5753)
5753 format (/, ' Sparsity pattern of triangularized Jacobian follows  .....', /, 6x, '1234567890123456789012345678901234567890123456789012345678901234567890')
  do k = 1, ntotsq
     write (unit = lunit(6), fmt = 5755) k, (kspars(k, l), l = 1, ntotsq)
5755 format (i3, ' : ', 120a1)
  end do
5764 loop = loop + 1
  write (unit = lunit(6), fmt = 5443)
  if (loop .le. 2) go to 5286
  if (ipunch .ne. 0) go to 5778
  write (unit = lunit(6), fmt = 5767)
5767 format (' 80-column card images of coupled R-L branches that may be punched on lunit7 (if ipunch = 0).', /, ' 12345678901234567890123456789012345678901234567890123456789012345678901234567890', /, ' --------------------------------------------------------------------------------')
  rewind lunit(1)
  do j = 1, n18
     read (unit = lunit(1), fmt = 5770) text80
5770 format (13a6, a3)
     write (unit = lunit(6), fmt = 5770) text80
  end do
5778 go to 5270
5782 scale = scale + percen
  if (scale .gt. 1.0d0) scale = 1.0d0
  go to 5379
9900 if (iprsov(nchain) .ge. 1) write (unit = lunit(6), fmt = 9904) kill
9904 format (' Exit "fltdat".  kill =', i5)
  return
end subroutine fltdat

!
! subroutine statrs.
!

subroutine statrs
  use blkcom
  implicit none
  !     installation-dependent module of overlay 29,  called only
  !     for  "tabulate energization results"  usage.
  character :: c1
  character(20) :: filn20
  character :: filnam(20)
  !  dimension filnam(20)
  integer(4) :: i
  integer(4) :: j
  equivalence (filn20, filnam(1))
  !
  data c1 / ' ' /
  !
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1793) (lstat(i), i = 13, 16)
1793 format (' Top of "statrs" .   lstat(13:16) =',  4i5)
  write (unit = filnam(1), fmt = 1804) (lstat(i), i = 13, 16)
1804 format ('st', i1, 'log', 3i1, '.dat ')
  do j = 15, 20
     filnam(j) = c1
  end do
  close (unit = lunit(1))
  if (lstat(13) .eq. 0) go to 9000
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1808) filnam
1808 format (' in "statrs", b4 open of unit lunit1 .', 20a1)
  open (unit = lunit(1), status = 'old', file = filn20, form = 'unformatted')
  write (unit = lunit(6), fmt = 1822) lstat(13), filnam
1822 format (20x, '----  Successful  open   of  lunit',  i1,  '  data', ' on disk.   Permanent file name = ', 20a1, ' .')
9000 return
end subroutine statrs

!
! end of file over29.f90
!
