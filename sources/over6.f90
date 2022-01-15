!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over6.f90
!

!
! subroutine over6.
!

subroutine over6
  use blkcom
  use labcom
  use smtacs
  use space2
  use indcom
  use movcop
  use veccom
  use savcom
  implicit none
  character(8) :: buff(20)
  integer(4) :: i, icas, il, ipass, ir, isubs1
  integer(4) :: j, j1, j2, jleft, jtest
  integer(4) :: k
  integer(4) :: l, l1, l2, l3, ll0, lleft, ltest
  integer(4) :: m, moon
  integer(4) :: n1, n2, n3, n7, n9, n11, n17, n22, ndx1, ndx2, nz
  real(8) :: d1
  !  dimension buff(20)
  !  dimension integx(1)
  !
  !  equivalence (iofkol, iofgnd)
  !  equivalence (iofkor, iofbnd)
  !  equivalence (x(1), integx(1))
  !  equivalence (loopss(11), next)
  !
  !  Following carries "next" among over6, insert, over7, & over9:
  integer(4), allocatable :: integx(:)
  integer(4), pointer :: iofkol => iofgnd
  integer(4), pointer :: iofkor => iofbnd
  integer(4), pointer :: next => loopss(11)
  !
  ll0 = size (transfer (x, integx))
  allocate (integx(ll0))
  integx = transfer (x, integx)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module over6."')
  ntot1 = ntot - 1
  n3 = 0
  d1 = 1.0d0 / (100.0d0 * flzero)
  ! 1st remove minus sign of "kpartb" if present as flag
  ! indicating "renumber bypass".   Then redefine value of
  ! this lofty resistance if user had a "high resistance"
  ! request (known by n7 .ne. default value of 1000 in over1):
  n7 = iabs (kpartb)
  if (n7 .lt. 1000) d1 = 10.0d0 ** n7
  i = inonl
54147 if (i .eq. 0) go to 54180
  if (nltype(i) .gt. 0) go to 54174
  k = nonlk(i)
  m = iabs (nonlm(i))
  do j = 1, ibr
     n1 = iabs (kbus(j))
     n2 = iabs (mbus(j))
     if (n1 .ne. k) go to 54154
     if (n2 .eq. m) go to 54174
     cycle
54154 if (n2 .ne. k) cycle
     if (n1 .eq. m) go to 54174
  end do
  ibr = ibr + 1
  if (ibr .le. lbrnch) go to 4716
  lstat(19) = 4716
  kill = 1
  lstat(16) = 2
  go to 9200
4716 kbus(ibr) = k
  mbus(ibr) = m
  if (n3 .gt. 0) go to 54162
  j1 = k
  j2 = m
  it = it + 1
  tr(it) = d1
  tx(it) = 0.0d0
  c(it) = 0.0d0
54162 nr(ibr) = -it
  length(ibr) = 1
  if (n3 .gt. 0) length(ibr) = -1
  n7 = -nltype(i)
  if (n7 .ne. 98) n7 = 99
  if ((noutpr .eq. 0) .and. (n3 .gt. 0)) write (unit = lunit(6), fmt = 54164) n7, i, bus(k), bus(m), bus(j1), bus(j2)
54164 format (' High res. added by EMTP across type-', i2, ' elem.', i3,  1x,  2x, '1 0', 4a6)
  if (n3 .eq. 0 .and. noutpr .eq. 0) write (unit = lunit(6), fmt = 54165) n7, i, bus(k), bus(m), tr(it)
54165 format (' High res. added by EMTP across type-', i2, ' elem.', i3, 1x, 2x, '1 0', 2a6, 12x, e6.1)
  n3 = 1
54174 i = i - 1
  if (i .gt. 0) go to 54147
54180 lstat(24) = kconst
  lstat(23) = it
  it1 = ibr
  iv  = location (c(1)) - location (x(1))
  if (kburro .eq.  1) go to 4802
  last = (iv - 1) / (3 * nbyte(3) / nbyte(4))
  lstat(14) = lsiz23
  lsiz23 = last
  iofgnd = last
  iofbnd = 2 * last
  go to 4803
4802 last = lsiz23
  iofgnd = 0
  iofbnd = 0
4803 n11 = 1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 55414) kburro, iv, last
55414 format (/, ' In "over6", kburro =', i3, ' .   Compute iv, last =', 2i8)
  rewind lunit(2)
  call tapsav (integx, lunit(2), iv, n11)
  n17 = 0
  call vecsav (volt, n17, n17)
  call vecsav (c, it, n11)
  call vecsav (tr, it, n11)
  call vecsav (tx, it, n11)
  call vecsav (r, it, n11)
  call vecsav (nr, ibr, n11)
  call vecsav (length, ibr, n11)
  ktrlsw(7) = it
  ktrlsw(8) = ibr
  call move0 (kolum(1 :), ibr)
  !     output network-connectivity table if so requested (idoubl=1).
  if (idoubl .le. 0) go to 5324
  call move0 (loca(1 :), ntot)
  next = 1
  i = 1
5010 if (i .gt. ibr) go to 5040
  k = iabs (kbus(i))
  m = iabs (mbus(i))
  if (bus(k) .eq. trash) go to 5020
  if (bus(m) .eq. trash) go to 5020
  !     assign 5020 to moon
  moon = 5020
  go to 5200
5020 i = i + 1
  go to 5010
5040 i = 1
5045 if (i .gt. inonl) go to 5060
  k = iabs (nonlk(i))
  m = iabs (nonlm(i))
  !  assign 5050 to moon
  moon = 5050
  go to 5200
5050 i = i + 1
  go to 5045
5060 i = 1
5065 if (i .gt. kswtch) go to 5250
  ndx1 = lswtch + i
  k = kmswit(i)
  m = kmswit(ndx1)
  ltest = isourc(i)
  if (ltest .le. 0) go to 5080
  k = iabs (node(ltest + 1))
5080 if (bus(k) .eq. trash) go to 5085
  if (bus(m) .eq. trash) go to 5085
  !  assign 5085 to moon
  moon = 5085
  go to 5200
5085 i = i + 1
  go to 5065
5200 if (k .gt. ntot .or. m .gt. ntot) go to 5240
  ipass = 0
5204 lleft = loca(k)
  if (lleft .gt. 0) go to 5214
  ltest = 0
5208 loca(k) = next
  go to 5235
5214 isubs1 = iofkol + lleft
  jleft = kolum(isubs1)
  if (m .gt. jleft) go to 5222
  ltest = lleft
  go to 5208
5222 isubs1 = iofkor + lleft
  ltest = korder(isubs1)
  if (ltest .eq. 0) go to 5228
  isubs1 = iofkol + ltest
  jtest = kolum(isubs1)
  if(jtest .ge. m) go to 5228
  lleft = ltest
  go to 5222
5228 isubs1 = iofkor + lleft
  korder(isubs1) = next
5235 isubs1 = iofkol+next
  kolum(isubs1) = m
  isubs1 = iofkor + next
  korder(isubs1) = ltest
  if (next .lt. last) go to 5237
  kill = 192
  lstat(19) = 5237
  go to 9200
5237 next = next + 1
  if (ipass .gt. 0) go to 5240
  ipass = k
  k = m
  m = ipass
  go to 5204
  !5240 go to moon, (5020, 5050, 5085)
5240 select case (moon)
  case (5020)
     go to 5020

  case (5050)
     go to 5050

  case (5085)
     go to 5085
  end select
5250 write (unit = lunit(6), fmt = 5254)
5254 format (//, ' List of input elements connected to each bus.', /, 10x, '1) Only the physical connections of multiphase lines are shown (capacitive and inductive coupling ignored)', /, 10x, '2) Repeated entries imply parallel connections', /, 10x,  '3) Sources are omitted, although switches  are included;', /, 10x, '4) u.m. usage produces extra, internally-defined nodes "um????" (1st 2 letters "um").')
  write (unit = lunit(6), fmt = 5261)
5261 format(' From bus name 1 names of all adjacent busses')
  write (unit = lunit(6), fmt = 5266)
5266 format(' --------------+----------------------------------------------------------------------------------------------------------')
  n22 = kwtspy
  ipass = 0
  k = 2
5268 if (loca(k) .eq. 0) go to 5294
  bus5 = bus(k)
5270 ltest = loca(k)
  if (ltest .eq. 0) go to 5294
5272 isubs1 = iofkol + ltest
  m = kolum(isubs1)
  ipass = ipass + 1
  buff(ipass) = bus(m)
  if (m .eq. 1) buff(ipass) = terra
  if (ipass .lt. 15) go to 5285
5277 write (unit = lunit(6), fmt = 5278) bus5, (buff(m), m = 1, ipass)
5278 format (9x, a6, '|', 15(a6, '*'))
  bus5 = blank
  ipass = 0
  if (ltest .eq. 0) go to 5294
5285 isubs1 =  iofkor + ltest
  ltest = korder(isubs1)
  if (ltest .gt. 0) go to 5272
  go to 5277
5294 k = k + 1
  if (idoubl .eq. 0) go to 5324
  if (m4plot .eq. 1) call emtspy
  if (k .eq. 2) go to 5320
  if (k .le. ntot) go to 5268
  bus5 = terra
  k = 1
  go to 5270
5320 write (unit = lunit(6), fmt = 5266)
  write (unit = lunit(6), fmt = 1337)
  if (n22 .ge. 2) kwtspy = 1
5324 if (tmax .gt. 0.0) go to 5344
  do i = 1, ntot
     ich1(i) = i
     norder(i) = i
  end do
  go to 40013
  ! renumber nodes based on sparsity of coeff matrix of transient
  ! network solution.
5344 if (iprsup .gt. 0) write (unit = lunit(6), fmt = 73524) (bus(i), i = 1, ntot)
73524 format (/, ' Bus names', /, (1x, 10a12))
  if (kpartb .gt. 0) go to 5351
  ! negative sign on "kpartb" is for "renumber bypass":
  norder(1) = 1
  n9 = 1
  do i = 2, ntot
     if (kode(i) .lt. 0) cycle
     n9 = n9 + 1
     norder(i) = n9
     ich1(n9) = i
  end do
  do i = 2, ntot
     if (kode(i) .eq. 0) cycle
     if (tstart(i) .ne. -9988.0d0) cycle
     n9 = n9 + 1
     norder(i) = n9
     ich1(n9) = i
  end do
  kpartb = n9
  do i = 2, ntot
     if (kode(i) .eq. 0) cycle
     if (tstart(i) .eq. -9988.0d0) cycle
     n9 = n9 + 1
     norder(i) = n9
     ich1(n9) = i
  end do
  go to 40013
5351 next = 1
  nz = 0
  call move0 (loca(1 :), ntot)
  call move0 (kownt(1 :), ntot)
  i = 1
41001 l = iabs (length(i))
  ! if (kodsem(i) .ne. 0 .and. imodel(i) .ne. -2
  !1                     .and. imodel(i) .ne. -4) l = iabs(kodebr(i))
  if (kodsem(i) .ne. 0 .and. imodel(i) .ge. 0) l = iabs (kodebr(i))
  if (l .gt. 1) go to 41002
  call insert (kbus(i), iabs (mbus(i)))
  go to 40004
41002 l1 = i + l - 2
  l3 = l1 + 1
  do j1 = i, l1
     k = iabs (kbus(j1))
     m = iabs (mbus(j1))
     l2 = j1 + 1
     do j2 = l2, l3
        call insert (k, iabs (kbus(j2)))
        call insert (m, iabs (mbus(j2)))
     end do
  end do
  if (kbus(i) .lt. 0) go to 40004
  do j1 = i, l3
     k = iabs (kbus(j1))
     do j2 = i, l3
        call insert (k, iabs (mbus(j2)))
     end do
  end do
40004 if (kill .eq. 0) go to 5372
  lstat(19) = 5372
  lstat(14) = i
  go to 9200
5372 i = i + l
  if (i .le. ibr) go to 41001
  if (num99 .le. 0) go to 73535
  do i = 1, inonl
     if (nltype(i) .gt. 0) cycle
     k = nonlk(i)
     m = iabs (nonlm(i))
     call insert (k, m)
  end do
73535 continue
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1335) nz, iofkol, iofkor
1335 format (/, " In  'over6' ,   nz, iofkol, iofkor =", 3i10, /, 1x, '   row            kode           kolum           koder           kownt')
  do i = 1, 10
     il = i + iofkol
     ir = i + iofkor
     if (iprsup .ge. 3) write (unit = lunit(6), fmt = 1337) i, kode(i), kolum(il), korder(ir), kownt(i)
1337 format (i6, 4i16)
  end do
  if (kswtch .lt. 1) go to 40015
  do i = 1, kswtch
     ndx2 = lswtch + i
     k = kmswit(i)
     m = kmswit(ndx2)
     call insert (k, m)
  end do
40015 do i = 2, ntot
     if (kode(i) .eq. 0) cycle
     if (kownt(i) .eq. (-1)) cycle
     nz = nz + 1
     kownt(i) = -1
  end do
  kownt(1) = 0
  if (kill .eq. 0) go to 5379
  lstat(19) = 5379
  lstat(14) = 0
  go to 9200
5379 kpartb = ntot - nz
  do i = next, last
     isubs1 =  iofkol + i
     kolum(isubs1) = 0
     isubs1 = iofkor + i
     korder(isubs1) = i + 1
  end do
  isubs1 = iofkor + last
  korder(isubs1) = 0
  icas = 0
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 54230) next, last, ntot, kswtch, kconst, nv, ibr, inonl,  npower, it, istead, num99,  nz, iv
54230 format (/, " Scalars at end of  'over6' .", /, '    Next    last    ntot  kswtch  kconst      nv     ibr   inonl  npower      it  istead   num99      nz      iv', /, 1x, 14i8)
  ncurr = ntot - nz
  lastov = nchain
  nchain = 7
  go to 9988
40013 lastov = nchain
  nchain = nchain + 2
  if (iprsup  .ge.  1) write (unit = lunit(6), fmt = 4568)
  go to 99999
9200 lastov = nchain
  lstat(18) = 6
  nchain = 51
9988 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module over6."')
99999 if (allocated (integx)) then
     x = transfer (integx, x)
     deallocate (integx)
  end if
  return
end subroutine over6

!
! subroutine rinfin.
!

subroutine rinfin
  use blkcom
  use labcom
  implicit none
  integer(4) :: i
  integer(4) :: j, j1, j2
  integer(4) :: k
  integer(4) :: m
  integer(4) :: n1, n2, n3, n7
  real(8) :: d1
  !
  n3 = 0
  d1 = 1.0d0 / (100.0d0 * flzero)
  ! 1st remove minus sign of "kpartb" if present as flag
  ! indicating "renumber bypass".   then redefine value of
  ! this lofty resistance if user had a "high resistance"
  ! request (known by n7 .ne. default value of 1000 in over1):
  n7 = iabs (kpartb)
  if (n7 .lt. 1000) d1 = 10.0d0 ** n7
  i = inonl
54147 if (i .eq. 0) go to 54180
  if (nltype(i) .gt. 0) go to 54174
  k = nonlk(i)
  m = iabs (nonlm(i))
  do j = 1, ibr
     n1 = iabs (kbus(j))
     n2 = iabs (mbus(j))
     if (n1 .ne. k) go to 54154
     if (n2 .eq. m) go to 54174
     cycle
54154 if (n2 .ne. k) cycle
     if (n1 .eq. m) go to 54174
  end do
  ibr = ibr + 1
  if (ibr .le. lbrnch) go to 4716
  lstat(19) = 4716
  kill = 1
  lstat(16) = 2
  !      go to 9200
4716 kbus(ibr) = k
  mbus(ibr) = m
  if (n3 .gt. 0) go to 54162
  j1 = k
  j2 = m
  it = it + 1
  tr(it) = d1
  tx(it) = 0.0
  c(it) = 0.0
54162 nr(ibr) = - it
  length(ibr) = 1
  if (n3 .gt. 0) length(ibr) = -1
  n7 = -nltype(i)
  if (n7 .ne. 98) n7 = 99
  if (noutpr .eq. 0 .and. n3 .gt. 0) write (unit = lunit(6), fmt = 54164) n7, i, bus(k), bus(m), bus(j1), bus(j2)
54164 format (' High res. added by EMTP across type-', i2, ' elem.', i3, 1x, 2x, '1 0', 4a6)
  if (n3 .eq. 0 .and. noutpr .eq. 0) write (unit = lunit(6), fmt = 54165) n7, i, bus(k), bus(m), tr(it)
54165 format (' High res. added by EMTP across type-', i2, ' elem.', i3, 1x, 2x, '1 0', 2a6, 12x, e6.1)
  n3 = 1
54174 i = i - 1
  if (i .gt. 0) go to 54147
54180 return
end subroutine rinfin

!
! subroutine insert.
!

subroutine insert (irrr, icc)
  use blkcom
  use labcom
  use space2
  implicit none
  integer(4), intent(in) :: irrr, icc
  integer(4) :: i, ic, ir, irn, irr, isubs1
  integer(4) :: n1, n2
  ! Following carries "next" among over6, insert, over7, & over9:
  !
  !  equivalence (loopss(11), next)
  !
  integer(4), pointer :: next => loopss(11)
  !
  if (irrr .le. 1) return
  if (icc .le. 1) return
  if (irrr .eq. icc) return
  if (kill .gt. 0) return
  i = 0
  ir = irrr
  irr = irrr
  ic = icc
99 irn = loca(ir)
  if (irn .eq. 0) go to 1
  n2 = irn + iofgnd
  n1 = ic - kolum(n2)
  if (n1 .gt. 0) go to 2
  if (n1 .eq. 0) go to 5
1 loca(ir) = next
  go to 4
2 ir = irn
  isubs1 = iofbnd + ir
  irn = korder(isubs1)
  if (irn .eq. 0) go to 3
  n2 = irn + iofgnd
  n1 = ic - kolum(n2)
  if (n1 .gt. 0) go to 2
  if (n1 .eq. 0) go to 5
3 isubs1 = iofbnd + ir
  korder(isubs1) = next
4 isubs1 = iofbnd + next
  korder(isubs1) = irn
  isubs1 = iofgnd + next
  kolum(isubs1) = ic
  if (next .lt. last) go to 2486
  kill = 200
  return
2486 next = next + 1
  if (kownt(irr) .lt. 0) go to 5
  kownt(irr) = kownt(irr) + 1
5 if (i .eq. 1) return
  i = 1
  ir = icc
  irr = icc
  ic = irrr
  go to 99
end subroutine insert

!
! end of file over6.f90
!
