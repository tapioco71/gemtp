!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over47.f90
!

!
! subroutine subr47.
!

subroutine subr47
  use blkcom
  use c29b01
  use com47
  use tracom
  implicit none
  integer(4) :: lltemp(20)

  integer(4) :: iof01, iof02, iof03, iof04, iof05, iof06, iof07, iof08, iof09
  integer(4) :: iof10, iof11, iof12, iof13, iof14, iof15, iof16, iof17, iof18
  integer(4) :: iof19, iof20, iof21, iof22, iof23, iof24, iof25, iof26, iof27
  integer(4) :: iof28, iof29, iof30, iof31, iof32, iof33, iof34, iof35, iof36
  integer(4) :: iof37, iof38, iof39, iof40, iof41, iof42, iof43, iof44, iof45
  integer(4) :: iof46, iof47, iof48, iof49, iof50, iof51, iof52, iof53
  integer(4) :: ldm, ldn, ldn2, ll0, lmq, lnq, lnq2, loq
  integer(4) :: n7, n8, n13
  real(8) :: cc
  real(8) :: dd
  real(8) :: ppa, ppb
  !
  !  dimension lltemp(20)
  !  dimension rtg(1), itg(1), ctg(1)
  !
  !  equivalence (karray(1), itg(1), rtg(1), ctg(1))
  !
  integer(4), pointer :: itg(:)
  real(8), allocatable :: rtg(:)
  complex(16), allocatable :: ctg(:)
  !
  itg => karray
  ll0 = size (transfer (karray, ctg))
  allocate (ctg(ll0))
  ctg = transfer (karray, ctg)
  ll0 = size (transfer (karray, rtg))
  allocate (rtg(ll0))
  rtg = transfer (karray, rtg)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  Begin module "subr47".')
  n8 = nchain
  if ( kburro .eq. 1) n8 = 29
  call dimens (lltemp(1), n8, trash, trash)
  n7 = lltemp(2) * nbyte(4) / nbyte(3)
  cc = 2 * nbyte(2)
  ppa = nbyte(3)
  cc = cc / ppa
  dd = 2 + ktrlsw(3)
  ppa = 12.0d0 + 12.0d0 * dd * dd * (1.0d0 + 3.0d0 * cc * cc)
  ppb = 36.0d0 + dd * (5.0d0 + cc)
  cc = n7 - 50
  cc = 2.0d0 * ppa * cc + ppb * ppb
  cc = (sqrtz (cc) - ppb) / ppa
  ldm = int (cc)
  ldn = int (cc * dd)
  if (iprsup .gt. 0) write (unit = lunit(6), fmt = 2000) ldm, ldn
2000 format ('  The cable number is limited to ', i4, /, '  the conductor number is limited to ',  i4)
  ldn2 = ldn + ldn
  lmq = ldm * ldm
  lnq = ldn * ldn
  lnq2 = lnq + lnq
  loq = 3 * ldm
  iof01 = 1
  iof02 = iof01 + ldn
  iof03 = (iof02 + ldm) * nbyte(4) / nbyte(3) + 2
  iof04 = iof03 + ldm
  iof05 = iof04 + ldm
  iof06 = iof05 + ldm
  iof07 = iof06 + ldm
  iof08 = iof07 + ldm
  iof09 = iof08 + ldm
  iof10 = iof09 + ldm
  iof11 = iof10 + ldm
  iof12 = iof11 + ldm
  iof13 = iof12 + ldm
  iof14 = iof13 + ldm
  iof15 = iof14 + ldm
  iof16 = iof15 + ldn
  iof17 = iof16 + ldn
  iof18 = iof17 + ldn
  iof19 = iof18 + ldn
  iof20 = iof19 + lnq
  iof21 = iof20 + lnq
  iof22 = iof21 + lnq
  iof23 = iof22 + lnq
  iof24 = iof23 + lmq
  iof25 = iof24 + lmq
  iof26 = iof25 + lmq
  iof27 = iof26 + lmq
  iof28 = iof27 + lmq
  iof29 = iof28 + lmq
  iof30 = iof29 + loq
  iof31 = iof30 + loq
  iof32 = iof31 + loq
  iof33 = iof32 + loq
  iof34 = iof33 + 3 * ldn
  iof35 = iof34 + 7 * ldm
  iof36 = (iof35 + lnq2) * nbyte(3) / (nbyte(2)*2) + 2
  iof37 = iof36 + ldn
  iof38 = iof37 + lnq
  iof39 = iof38 + lnq
  iof40 = iof39 + lnq
  iof41 = iof40 + lnq
  iof42 = iof41 + lnq
  iof43 = iof42 + lnq
  iof44 = iof43 + lnq
  iof45 = iof44 + lnq
  iof46 = iof45 + lnq
  iof47 = iof46 + lnq
  iof48 = iof47 + lnq
  iof49 = iof48 + lnq
  iof50 = iof49 + lnq
  iof51 = iof50 + lnq
  iof52 = iof51 + lnq
  iof53 = iof52 + lnq
  !     step over last vector to find total memory requirements:
  n13 = (iof53 + lnq2) * 2 * nbyte(2) / nbyte(3)
  if (n13 .le. n7) go to 7835
  lstat(19) = 7835
  kill = 225
  go to 7842
7835 call guts47 (itg(iof01 :), itg(iof02 :), rtg(iof03 :), rtg(iof04 :), rtg(iof05 :), rtg(iof06 :), rtg(iof07:), rtg(iof08 :), rtg(iof09 :), rtg(iof10 :), rtg(iof11 :), rtg(iof12 :), rtg(iof13 :), rtg(iof14 :), rtg(iof15 :), rtg(iof16 :), rtg(iof17 :), rtg(iof18 :), rtg(iof19 :), rtg(iof20 :), rtg(iof21 :), rtg(iof22 :), rtg(iof23 :), rtg(iof24 :), rtg(iof25 :), rtg(iof26 :), rtg(iof27 :), rtg(iof28 :), rtg(iof29 :), rtg(iof30 :), rtg(iof31 :), rtg(iof32 :), rtg(iof33 :), rtg(iof34 :), rtg(iof35 :), ctg(iof36 :), ctg(iof37 :), ctg(iof38 :), ctg(iof39 :), ctg(iof40 :), ctg(iof41 :), ctg(iof42 :), ctg(iof43 :), ctg(iof44 :), ctg(iof45 :), ctg(iof46 :), ctg(iof47 :), ctg(iof48 :), ctg(iof49 :), ctg(iof50 :), ctg(iof51 :), ctg(iof52 :), ctg(iof53 :), ldm, ldn, ldn2, lnq2)
7842 if (kill .gt. 0) lstat(18) = nchain
  lastov = 47
  if (allocated (rtg)) then
     karray = transfer (rtg, karray)
     deallocate (rtg)
  end if
  if (allocated (ctg)) then
     karray = transfer (ctg, karray)
     deallocate (ctg)
  end if
  return
end subroutine subr47

!
! subroutine guts47.
!

subroutine guts47 (ngg, ncpp, al1i, al2i, al3i, dci, thc, bio, bi1, bi2, bi3, bi4, bi5, hi,  di, gn, rad, wy, zy, yz, sc, qc, dr0, th0, al0, dij, dir, ang, roi, esi, usi, usr, gi, radi, yzn, qn, yo, ys, yc, zs, zc, ze, zp, zpc, a, ai, b, bi, ca, cb, cc, cd, f, ldm, ldn, ldn2, lnq2)
  use blkcom
  use com47
  use volpri
  use indcom
  use tracom
  implicit none
  !
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ldn2
  integer(4), intent(in) :: lnq2
  integer(4), intent(out) :: ncpp
  integer(4), intent(out) :: ngg
  real(8), intent(out) :: al0
  real(8), intent(out) :: al1i
  real(8), intent(out) :: al2i
  real(8), intent(out) :: al3i
  real(8), intent(out) :: ang
  real(8), intent(out) :: bi1
  real(8), intent(out) :: bi2
  real(8), intent(out) :: bi3
  real(8), intent(out) :: bi4
  real(8), intent(out) :: bi5
  real(8), intent(out) :: bio
  real(8), intent(out) :: dci
  real(8), intent(out) :: di
  real(8), intent(out) :: dij
  real(8), intent(out) :: dir
  real(8), intent(out) :: dr0
  real(8), intent(out) :: esi
  real(8), intent(out) :: gi
  real(8), intent(out) :: gn
  real(8), intent(out) :: hi
  real(8), intent(in) :: qc
  real(8), intent(out) :: rad
  real(8), intent(out) :: radi
  real(8), intent(out) :: roi
  real(8), intent(in) :: sc
  real(8), intent(out) :: th0
  real(8), intent(out) :: thc
  real(8), intent(out) :: usi
  real(8), intent(out) :: usr
  real(8), intent(in) :: wy
  real(8), intent(out) :: yz
  real(8), intent(out) :: yzn
  real(8), intent(out) :: zy
  integer(4) :: i, i1, iii, ik, im, in, in1, ips, irsep, isyst, itrnsf
  integer(4) :: j, j1, j2, j3, j13, j14, jdx1, jdx2, jn, jnc, junit4
  integer(4) :: k, kgc, kkk, kpd
  integer(4) :: l0, l1, l5save, ldisfr, lines, liu, lnq1
  integer(4) :: mispun, mrr
  integer(4) :: n1, n2, n3, n4, n5, n6, n7, n8, n9, ncmod, ngrnd, nki, nkj
  integer(4) :: npipe, npk, nrp, nw, nx, nz
  real(8) :: anpais
  real(8) :: cczero
  real(8) :: d0, d1, d9, d13, dist
  real(8) :: factor, fdecad, freq, freqs, freqsv
  real(8) :: pkkk
  real(8) :: rlimit, roe3, roe4, rsg, rtio, rzero
  real(8) :: spl2
  real(8) :: ten, three
  real(8) :: w
  real(8) :: xmajor, xtotal, xzero
  complex(16) :: ys(ldn, ldn), yc(ldn, ldn), zs(ldn, ldn), zc(ldn, ldn)
  complex(16) :: ze(ldn, ldn), ai(ldn, ldn), bi(ldn, ldn), zp(ldn, ldn)
  complex(16) :: zpc(ldn, ldn), a(ldn, ldn),  b(ldn, ldn), yo(ldn, ldn)
  complex(16) :: qn(ldn), f(ldn, ldn2)
  complex(16) :: ca(ldn, ldn), cb(ldn, ldn), cc(ldn, ldn), cd(ldn, ldn)
  character(8) :: bufsem, cname, text1, text2, text3, text4
  !
  dimension bufsem(14)
  dimension ngg(ldn), ncpp(ldm), rad(ldn), yzn(lnq2), wy(ldn)
  dimension sc(ldn,ldn), qc(ldn,ldn), gi(ldn,3)
  dimension dr0(ldm, ldm), th0(ldm, ldm), al0(ldm, ldm)
  dimension dij(ldm, ldm), dir(ldm, ldm), ang(ldm, ldm)
  dimension radi(ldm, 7),   zy(ldn, ldn),  yz(ldn, ldn)
  dimension roi(ldm, 3), esi(ldm, 3), usi(ldm, 3), usr(ldm, 3)
  dimension bio(ldm), bi1(ldm), bi2(ldm), bi3(ldm), bi4(ldm)
  dimension bi5(ldm), dci(ldm), thc(ldm),  hi(ldm),  di(ldn)
  dimension al1i(ldm),   al2i(ldm),   al3i(ldm),   gn(ldn)
  !
  data text1 / 'punch ' /
  data text2 / ' line ' /
  data text3 / 'cable ' /
  !
  nrp =  0
  mrr =  0
  junit4=77
  mispun = 0
  lnq1 = ldn * ldn + 1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2314) lastov, ialter, lunit(2), lunit(5), speedl, epsiln, twopi
2314 format (/, " Enter  'guts47' .  lastov  ialter  lunit2  lunit5", 14x, 'speedl', 14x, 'epsiln', 15x, 'twopi', /, 18x, 4i8, 3e20.11)
  rewind lunit(2)
  rewind lunit(3)
  rewind lunit(4)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = *)  ' %%--  ready to rewind lunit9.'
  rewind lunit(9)
  numaki = 0
  ncmod = 0
  fzero = 0.0
  cimag1 = cmplxz (fzero, unity)
  creal1 = cmplxz (unity, fzero)
  czero = cmplxz (fzero, fzero)
  if (lastov .eq. 45) numaki = 9
  if (lastov .eq. 39) numaki = 9
  if (lastov .eq. 43) numaki = 9
  if (ialter .ne. 2) go to 7407
  l5save = lunit(5)
  lunit(5) = lunit(2)
7407 ldisfr = location (voltk) - location (volti)
  spdlgt = speedl
  logsix = lunit(6)
  iprs47 = iprsup
  ten = 10.0d0
  value2 = epsiln
  value1 = 0.5d0 * value2 / 1000.0d0
  value3 = 20.0d0 * 1000.0d0 / alogz (ten)
  value4 = 1.781072417990d0
  value5 = tenm3
  valu14 = 2.302585093000d0
  liu = 0
  pai = twopi / 2.0d0
  u0 = 2 * twopi * tenm6 / ten
  spl2 = speedl ** 2
  e0 = 1.0d0 /u0 / spl2
  u2p = u0 / twopi
  e2p = e0 * twopi
  npais = 0
  ncros = 0
  irsep = 0
  xmajor = 1.0d0
  rsg = 1.e10
5 ngrnd = 0
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 4230) bufsem
4230 format (13a6, a2)
  if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
  if (bufsem(1) .ne. text1) go to 8214
  mispun = 1
  go to 5
8214 continue
  read (unit = abuff, fmt = 900) n1, n2, n3, n4, n5, n6, n7, n8, n9
900 format(20i5)
  nenerg = 0
  if (n1 .ne. 0)   go to 6
  if (n2 .ne. 0)   go to 4501
  if (n3 .ne. 0)   go to  4501
  write (unit = kunit6, fmt = 1200)
1200 format ("+Blank card terminating  'cable constants'  cases.")
  call interp
  if (lastov .eq. 1) go to 7439
  n1 = lastov
  lastov = nchain
  nchain = n1
  if (ialter .eq. 2) lunit(5) = l5save
  if (ipunch .eq. 0) go to 7496
  d1 = 0.0d0
  write (unit = lunit(9)) d1, d1, d1
  rewind lunit(9)
  if (iprs47 .ge. 1) write (unit = lunit(6), fmt = 11200)  d1, d1, d1, ipunch
11200 format (' Last record on unit9.', 3e15.6, /, ' ipunch =', i10)
7496 go to 9900
7439 lastov = nchain
  nchain = 51
  go to 9900
4501 kill = 172
  lstat(14) = n1
  lstat(19) = 4501
  go to 9200
6 itypec = n1
  isyst = n2
  iearth = n4
  kmode = n5
  izflag = n6
  iyflag = n7
  !     initialize optional "punch" card variables (wsm, nov, 81):
  npais = 0
  ncros = 0
  irsep = 0
  xmajor = 0.0d0
  rsg = 0.0d0
  cname = blank
  if (mispun .ne. 1) go to 8234
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 903) npais, ncros, irsep, xmajor, rsg, cname
903 format (3i5, 2e10.1, a1)
8234 mispun = 0
  go to (10, 20, 20) , itypec
  !     overhead line data input
10 ncct = n3
  if (n9 .ne. 0)  nenerg = n9
  write (unit = kunit6, fmt = 1212) itypec, isyst, ncct, iearth, kmode, izflag, iyflag, nenerg
1212 format ('+Misc. data for lines', 1x, 8i3)
  if (npais .ne. 0) write (unit = lunit(6), fmt = 1213) npais, xmajor, rsg, cname
1213 format (' Misc. data for lines', i3, 2e10.3, a1)
  do i = 1, ncct
     im = 4 * (i - 1) + 1
     in = im + 3
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 4230) bufsem
     if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
     read (unit = abuff, fmt = 900) (ncpp(j), j = im, in)
     write (unit = kunit6, fmt = 1220)
1220 format ('+Additional data for phase & ground wires.')
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 4230) bufsem
     if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
     read (unit = abuff, fmt = 901) (radi(i, j), j = 1, 4), (dr0(i, j), j = 1, 2)
901  format (8e10.1)
     write (unit = kunit6, fmt = 1230)
1230 format ('+Geometrical data of bundled conductors.')
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 4230) bufsem
     if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
     read (unit = abuff, fmt = 901) (roi(i, j), usr(i, j), j = 1, 2)
     write (unit = kunit6, fmt = 1240)
1240 format ('+Resistivity & permeability of phase & gd. wires.')
  end do
  ncc = 0
  npc = 0
  do i = 1, ncct
     im = 4 * (i - 1) + 1
     in = im + 1
     ncc = ncc + ncpp(im)
     npc = npc + ncpp(im) + ncpp(in)
  end do
  np2 = npc
  lines = ( npc + 1) / 2
  im = -1
  do j=1, lines
     im = im + 2
     in = im + 1
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 4230) bufsem
     if ( ialter  .ne.  2 ) write (lunit(2), 4230)  bufsem
     read (unit = abuff, fmt = 901) (thc(i), dci(i), di(i), i = im, in)
     if (j .gt. 1)   go to 19
     write (kunit6, 16)
16   format (46h+heights and horizontal distance of each line.)
19 end do
  go to 85
  !     cable data input
20 npc = n3
  npp = n8
  ngrnd = n9
  ncc = 0
  npc2 = 0
  if (itypec .eq. 3) go to 3010
  write (unit = kunit6, fmt = 3210) itypec, isyst, npc, iearth, kmode, izflag, iyflag, ngrnd
3210 format ('+Misc. data for cables', 8i3)
  if (npais .ne. 0) write (unit = lunit(6), fmt = 3211) npais, ncros, irsep, xmajor, rsg, cname
3211 format (' Misc. data for cables', 3i3, 2e8.1, a1)
  if (isyst .ne. -1) go to 21
  if (iearth .ne. 99) go to 21
  kill = 173
  lstat(19) = 3210
  go to 9200
21 np2 = npc * 3
  go to 3050
3010 write (unit = kunit6, fmt = 3030) itypec, isyst, npc, iearth, kmode, izflag, iyflag, npp, ngrnd
3030 format ('+Misc. data for pipe cable', 9i3)
  if (npais .ne. 0) write (unit = lunit(6), fmt = 3031) npais, ncros, xmajor, rsg, cname
3031 format (' Misc. data for pipe cable', 2i3, 2e8.1, a1)
  if (npp .ne. 1) npp = 1
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
  read (unit = abuff, fmt = 901) (radp(i), i = 1, 3), rop, usp, es1, es2
  write (unit = kunit6, fmt = 3035)
3035 format ('+Pipe characteristic.')
  if (radp(3) .ne. 0.0d0) go to 3036
  radp(3) = radp(2) + 100.0d0 * epsiln
  es2 = 1.0
  !     read input card using cimage
3036 call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
  read (unit = abuff, fmt = 901) (dci(i), thc(i), i = 1, npc)
  write (unit = kunit6, fmt = 3040)
3040 format ('+Relation between cables and pipe.')
  np2 = npc * 3 + npp
  !     read input card using cimage.
3050 call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
  read (unit = abuff, fmt = 900) (ncpp(i), i = 1, npc)
  write (unit = kunit6, fmt = 3220)
3220 format ('+Number of conductors in each cable.')
  if (itypec .eq. 2) go to 22
  i = npc
3223 thc(i) = thc(i) - thc(1)
  i = i - 1
  if (i .gt. 0) go to 3223
22 do i = 1, npc
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 4230) bufsem
     if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
     read (unit = abuff, fmt = 901) (radi(i, j), j = 1, 7)
     write (unit = kunit6, fmt = 3230) (radi(i, j), j = 1, 7)
3230 format ('+radii.', 1x, 7f6.2)
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 4230) bufsem
     if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
     read (unit = abuff, fmt = 901) (roi(i, j), usr(i, j), usi(i, j), esi(i, j), j= 1, 2)
     write (unit = kunit6, fmt = 3240)
3240 format ('+Physical constants for conductors and insulators.')
     if (ncpp(i) .le. 2)  go to 3245
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 4230) bufsem
     if (ialter .ne. 2) write(lunit(2), 4230)  bufsem
     read (unit = abuff, fmt = 901) roi(i, 3), usr(i, 3), usi(i, 3), esi(i, 3)
     write (unit = kunit6, fmt = 3240)
     if (radi(i, 7) .ne. 0.0d0) go to 3239
     radi(i, 7) = radi(i, 6) + 100.0d0 * epsiln
     usi(i, 3) = 1.0d0
     esi(i, 3) = 1.0d0
     go to 3239
3245 usi(i, 3) = 1.0d0
     esi(i, 3) = 1.0d0
     if (ncpp(i) .eq. 2) go to 3235
     if (radi(i, 3) .ne. 0.0d0) go to 3231
     radi(i, 3) = radi(i, 2) + 100.0d0 * epsiln
     usi(i, 1) = 1.0d0
     esi(i, 1) = 1.0d0
3231 radi(i, 4) = radi(i, 3)
     radi(i, 5) = radi(i, 4)
     radi(i, 6) = radi(i, 5)
     radi(i, 7) = radi(i, 6)
     usi(i, 2) = 1.0d0
     esi(i, 2) = 1.0d0
     usr(i, 2) = 1.0d0
     usr(i, 3) = 1.0d0
     roi(i, 2) = roi(i, 1)
     roi(i, 3) = roi(i, 2)
     go to 3239
3235 if (radi(i, 5) .ne. 0.0d0) go to 3237
     radi(i, 5) = radi(i, 4) + 100.0d0 * epsiln
     usi(i, 2) = 1.0d0
     esi(i, 2) = 1.0d0
3237 radi(i, 6) = radi(i, 5)
     radi(i, 7) = radi(i, 6)
     usr(i, 3) = 1.0d0
     roi(i, 3) = roi(i, 2)
3239 if(ncpp(i) .lt. 2)  go to 23
     npc2 = npc2 + 1
     ncmod = ncmod + 1 - ngrnd
     if ( ncpp(i)  .ge.  3 )   ncmod = ncmod + 1
23   ncc = ncc + ncpp(i)
  end do
  npc2 = npc + npc2
  if (itypec .eq. 2)   go to 24
  ncc = ncc + npp
  if (npp .eq. 0)   go to 85
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if (ialter .ne. 2) write (lunit(2), 4230)  bufsem
  read (unit = abuff, fmt = 901) (hi(i), di(i), i = 1, npp)
  write (unit = kunit6, fmt = 3055)
3055 format ('+Height and horizontal distance of each pipe.')
  go to 85
24 lines = (npc + 3) / 4
  do j = 1, lines
     im = 4 * (j-1) + 1
     in = im + 3
     !     read input card using cimage.
     call cimage
     read (unit = abuff, fmt = 4230) bufsem
     if ( ialter  .ne.  2 ) write (lunit(2), 4230)  bufsem
     read (unit = abuff, fmt = 901) (hi(i), di(i), i = im, in)
     if (j .gt. 1)   go to 93
     write (kunit6, 3250)
3250 format ('+Height and horizontal distance of each cable.')
93 end do
85 if((npc .le. ldm) .and. (ncc .le. ldn)) go to 1945
  kill = 225
  lstat(14) = ldn
  lstat(15) = ldm
  lstat(19) = 85
  go to 9200
1945 do i = 1, ncc
     ngg(i) = 0
  end do
  if (ngrnd .le. 3) go to 3983
  call cimage
  read (unit = abuff, fmt = 1999) (ngg(i), i = 1, npc ), npipe
1999 format (2x, 78i1)
  do i = 1, npc
     if (ngg(i) .le. 1) go to 1919
     jdx1 = npc + i
     jdx2 = npc2 + i
     kgc = ngg(i)
     if (kgc .lt. 5) ngg(i) = 0
     if (kgc .ne. 3 .and. kgc .ne. 6) ngg(jdx1) = 1
     if (kgc .ne. 2 .and. kgc .ne. 5) ngg(jdx2) = 1
1919 end do
  if (itypec .eq. 2) go to 3983
  ngg(ncc) = npipe
3983 if (numaki .eq. 0)   go to 87
  write (unit = lunit(4)) itypec, isyst, npc, ncc
  if (iprs47 .ge. 1) write (unit = lunit(6), fmt = 86) itypec, isyst, npc, ncc
86 format (' At 86 of subr47', /, 4i5)
  !     read input card using cimage
87 call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
  read (unit = abuff, fmt = 902) d9, freq, ik, ips, dist, j13, j14
902 format (2e15.6, 2i5, f8.3, i10, i2)
  ipunch = j13
  roe = d9
  itrnsf = j14
  if (freq .eq. 0.0d0) freq = statfr
  if (ips .eq. 0) ips = 1
  write (unit = kunit6, fmt = 3256) roe, freq, ik, ips, dist, ipunch, itrnsf
3256 format ('+Freq. card', 2e10.3, 2i3, e8.3, 2i2)
  call interp
  if (iearth .ne. 99)   go to 95
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if ( ialter  .ne.  2 ) write (lunit(2), 4230)  bufsem
  read (unit = abuff, fmt = 901) dep1, dep2, roe3, roe4
  write (unit = kunit6, fmt = 3280)
3280 format ('+Depths & resistivities of 2nd & 3rd layer earth.')
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
  read (unit = abuff, fmt = 901) htoj2, htoj3, htoj4, hyud2, hyud3, hyud4
  write (unit = kunit6, fmt = 3290)
3290 format ('+Permeability & permittivity of the three layers.')
  alf1 = roe / roe3
  alf2 = roe / roe4
95 continue
  anpais = iabs (npais)
  xtotal = xmajor * anpais
  if(npais .eq. 0) xtotal = xmajor
  if(npais .ge. 0) call nyan (itypec, npc, ncc, ncpp, ngrnd, ncros, npais, ldm)
  if (itypec .ne. 1) go to 200
  !     overhead line data outputs & precal.
  jnc = 1
  do i = 1, ncct
     im = 4 * (i - 1) + 1
     do  j = 1, 2
        j1 = im + j + 1
        in = ncpp(j1)
        if (in .eq. 0) go to 120
        j2 = 2 * j - 1
        i1 = i + ncct
        if (in .eq. 1) go to 111
        d0 = dr0(i, j) / sinz (pai / in)
        usi(i, j) = radi(i, j2)
        in1 = in - 1
        do k = 1, in1
           usi(i, j) = usi(i, j) * d0 * sinz (pai / in * k)
        end do
        usi(i1, j) = usi(i, j) ** (1.0d0 / in)
        go to 120
111     usi (i1, j) = radi(i,j2)
120  end do
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4001) i, (usi(i1, j), j = 1, 2)
4001 format (1x, 'circuit', i3, '0  equivalent radius of phase wire ', e10.4, /, 34x, 'ground wire', e10.4)
     do j = 1, 2
        j1 = im + j - 1
        if (ncpp(j1) .ne. 0) go to 123
        usi(i, j) = 0.0d0
        esi(i, j) = 0.0d0
        gi(i, j) = 0.0d0
        go to 125
123     j1 = 2 * j
        j2 = j1 - 1
        usi(i,j) = radi(i, j1) * sqrtz (u0 * usr(i, j) / roi(i, j))
        esi(i,j) = radi(i, j2) * sqrtz (u0 * usr(i, j) / roi(i, j))
        if (radi(i, j1) .lt. radi(i, j2)) go to 124
        kill = 174
        lstat(14) = i
        flstat(15) = radi(i, j2)
        flstat(16) = radi(i, j1)
        lstat(19) = 123
        go to 9200
124     gi(i, j) = roi(i, j) / pai / (radi(i, j2) ** 2 - radi(i, j1) ** 2)
        !     1          /ncpp(im+j+1)
125  end do
  end do
  do j = 1, 2
     do i = 1, ncct
        i1 = i + ncct
        im = 4 * (i - 1) + 1
        j1 = im + j - 1
        j1 = ncpp(j1)
        if (j1 .eq. 0) go to 135
        j2 = j1 + jnc - 1
        do k = jnc, j2
           rad(k) = usi(i1, j)
        end do
        jnc = j2 + 1
135  end do
  end do
  write (unit = lunit(6), fmt = 170)
170 format (//, 132('-'), //, 1x)
  if (isyst .ne. 0) go to 140
  write (unit = lunit(6), fmt = 180)
180 format (' Table of overhead untransposed line parameters', /, 1x)
  go to 142
140 write (unit = lunit(6), fmt = 182)
182 format (' Table of overhead transposed line parameters', /, 1x)
142 j1 = npc - ncc
  write (unit = lunit(6), fmt = 171) ncc, j1
171 format (10x, 'Total number of phase wires', i3, 3x, 'ground wires ', i3, /, 1x)
  do i = 1, ncct
     i1 = i + ncct
     im = 4 * (i - 1) + 1
     j1 = im + 1
     j2 = im + 2
     j3 = im + 3
     write (unit = lunit(6), fmt = 184) i, (radi(i, j), j = 1, 2), ncpp(j2), usi(i1, 1)
184  format (10x, 'circuit', i2, /, 10x, 'radius | phase wire  (outer',  e11.4, '  , inner', e11.4, ' )*', i2, ' bundles', /, 20x, 'equiv. radius', e11.4)
     j1 = ncpp(j1)
     if(j1 .eq. 0) go to 148
     write (unit = lunit(6), fmt = 185) (radi(i, j), j = 3, 4), ncpp(j3), usi(i1, 2)
185  format (19x, 'Ground wire (outer', e11.4, '  , inner', e11.4, ' )*', i2, ' bundles', /, 20x, 'equiv. radius', e11.4)
148  write (unit = lunit(6), fmt = 186) (roi(i, j), j = 1, 2), (usr(i, j), j = 1, 2), (gi(i, j), j = 1, 2)
186  format (10x, 'Resistivity(Ohm-m) | phase wire', e11.4, 3x, 'ground wire', e11.4, /, 10x, 'relative permeability |', 8x, f11.4, 14x, f11.4, /, 10x, 'DC resistance(Ohm/m) |', 9x, e11.4, 14x,  e11.4)
  end do
  do i = 1, npc
     hi(i) = (2.0d0 * dci(i) + thc(i) ) / 3.0d0
  end do
  write (unit = lunit(6), fmt = 165) (i, hi(i), i = 1, npc)
165 format (10x, 'Effective height of the line ', 5(i5, f12.5))
  write (unit = lunit(6), fmt = 915) (di(i), i = 1, npc)
915 format (10x, 'Distance between phases     |', 5f17.5)
  write (unit = lunit(6), fmt = 917) freq, ik, ips
917 format (10x, 'Beginning frequency ', 5x, e15.6, 5x, 'number of decades ', i5, 5x, 'number of points in each decade ', i5)
  write (unit = lunit(6), fmt = 913) roe
913 format (10x, 'earth | resistivity', f12.5)
  if (iearth .ne. 99) go to 167
  write (unit = lunit(6), fmt = 916) dep1, dep2, roe, roe3, roe4, htoj2, htoj3, htoj4, hyud2, hyud3, hyud4
916 format (10x, '3-layer earth ', /, 10x, 'depth from surface , 1st layer', f7.2, 3x, '2nd layer', f8.2, 3x, '3rd layer to infinite', /, 10x, 'earth resistivity 0', e18.2, 6x, e14.2, 6x, e18.2,/, 10x, 'relative permeability 0', e14.2, 6x, e14.2, 6x, e18.2, /, 10x, 'relative permittivity 0', e14.2, 6x, e14.2, 6x, e18.2)
167 nw = npc
  nz = npc
  ngrnd=ncc
  go to 700
  !     cable data outputs and precalculation
200 do i = 1, npc
     rad(i) = radi(i, 7)
     do j = 1, 3
        gi(i, j) = speedl / sqrtz (esi(i, j))
     end do
  end do
  write (unit = lunit(6), fmt = 170)
  if (isyst .ge. 0)   go to 211
  write (unit = lunit(6), fmt = 280)
280 format (' table of under-ground cable parameters', /, 1x)
  go to 214
211 if (isyst .gt. 0)   go to 213
  write (unit = lunit(6), fmt = 281)
281 format (' table of cable parameters for cables on the earth surface')
  go to 214
213 write (unit = lunit(6), fmt = 282)
282 format (' table of overhead cable parameters', /, 1x)
214 if (ngrnd .gt. 3) go to 1980
  if (ngrnd .ne. 0) go to 325
  ngrnd = ncc
  go to 215
325 if (ngrnd .gt. 1) go to 330
  ngrnd = ncc
  if (itypec .eq. 2) go to 215
  ngrnd = ncc - 1
  go to 215
330 if (ngrnd .gt. 2) go to 3333
  ngrnd = npc2
  go to 215
3333 ngrnd = npc
  go to 215
1980 do i = 1, ncc
     kpd = ncc + 1 - i
     if (ngg(kpd) .gt. 0) go to 1982
     ngrnd = kpd
     go to 215
1982 end do
  write (unit = lunit(6), fmt = 1777)
1777 format ('  no ungrounded conductor needs calculation')
  call stoptp
215 if (itypec .eq. 2)   go to 1215
  write (unit = lunit(6), fmt = 370)
370 format (' pipe type cables', /, 1x)
  if (npp .ne. 0)   go to 216
  write (unit = lunit(6), fmt = 371)
371 format (' earth return path not included')
216 write (unit = lunit(6), fmt = 380)
380 format (10x, ' pipe and its insulator')
  if (npp .ne. 0)  go to 1216
  radp(2) = fltinf
  radp(3) = fltinf
1216 write (unit = lunit(6), fmt = 381) (radp(i), i = 1, 3), rop, usp
381 format (10x, 'pipe | inner radius', e13.5, 2x, 'outer', e13.5, 2x, 'houter radius of pipe insulator', e13.5,/, 18x, 'resistivity(Ohm-m)', e16.5, 2x, 'relative permeability', f8.1)
  write (unit = lunit(6), fmt = 382) es1, es2
382 format (10x, 'insulator | relative permittivity  inner', f8.1, 2x, 'outer', f8.1)
  if (es2 .eq. 0.0d0) es2 = 1.0d0
  if (npp .ne. 0) write (unit = lunit(6), fmt = 383) (i, hi(i), di(i), i = 1, npp)
383 format (10x, 'pipe', i2, ' | dist. from earth surface', f9.4, 'm', 3x, 'dist. from pipe 1', f9.4, 'm')
  write (unit = lunit(6), fmt = 390)
390 format (10x, 'inner conductors (sc cables)')
  write (unit = lunit(6), fmt = 391) (i, dci(i), i = 1, npc)
391 format (10x, 'dist. from pipe center |', 6(i5, f9.4))
  write (unit = lunit(6), fmt = 392) (thc(i), i = 1, npc)
392 format (10x, 'angle to first cable   |', 6(f14.4))
1215 do i = 1, npc
     jn = ncpp(i)
     write (unit = lunit(6), fmt = 284) i, (j, radi(i, j), j = 1, 7)
284  format (10x, 'phase', i2, 1x, 'boundary radii  | ', /, 10x, 7(i3, e13.5))
     write (unit = lunit(6), fmt = 285) (roi(i, j), j = 1, 3), (usr(i, j), j = 1, 3), (j, usi(i, j), esi(i, j), gi(i, j), j = 1, jn)
  end do
285 format (18x, 'resistivity(Ohm-m)| core', e12.5, 3x, 'sheath', e12.5, 3x, 'armor', e12.5, /, 18x, 'relative permeability |  ', f12.5, 9x, f12.5, 8x, f12.5, /, 3(18x, 'insulator', i2, '| (relative) permeability', f6.2,3x, 'permittivity', f6.2, 3x, 'velocity(m/s)', e12.5, /, 1x))
  if (itypec .ne. 3) go to 220
  three = 3.0d0
  rlimit = (1.0d0 + 2.0d0 / sqrtz(three)) * radi(1, 7)
  if(npc .eq. 2) rlimit = 2.0d0 * radi(1, 7)
  if(npc .eq. 1) rlimit = radi(1, 7)
  if(radp(1) .gt. rlimit) go to 221
  write (unit = lunit(6), fmt = 393)
393 format ('0', 10x, 'physically inner conductors cannot be exsisted in the pipe.', /, 11x, 'please check the radii of the pipe and inner conductors.', //, 1x)
  call stoptp
220 write (unit = lunit(6), fmt = 914) (i, hi(i), i = 1, npc)
914 format (10x, 'distance from earth surface |', 5(i5, f12.5))
221 do i = 1, npc
     if (i .eq. 1) go to 223
     i1 = i - 1
     if (ncpp(i1) .ge. ncpp(i)) go to 223
     write (unit = lunit(6),fmt = 289)
     call stoptp
289  format ('0', 10x, "Invalid data of 'ncpp(i)', no further calculation.  It should be 'ncpp(i-1).ge.ncpp(i)'.", /, 11x, 'Please check your data.'  )
223  continue
     bio(i) = radi(i,1) * sqrtz(u0/roi(i,1) *usr(i,1))
     bi1(i) = radi(i,2) * sqrtz(u0/roi(i,1) *usr(i,1))
     al1i(i) = alogz(radi(i,3) / radi(i,2) )
     bi2(i) = radi(i,3) * sqrtz(u0/roi(i,2) * usr(i,2))
     bi3(i) = radi(i,4) * sqrtz(u0/roi(i,2) * usr(i,2))
     al2i(i) = alogz(radi(i,5) / radi(i,4) )
     bi4(i)=radi(i,5)*sqrtz(u0/roi(i,3)*usr(i,3))
     bi5(i)=radi(i,6)*sqrtz(u0/roi(i,3)*usr(i,3))
     al3i(i) = alogz(radi(i,7) / radi(i,6) )
  end do
  if (iprs47 .gt. 1) write (lunit(6), 2437)  npc, ncc, npp, ncct, numaki, lastov, ipunch, ialter, iearth, itypec, isyst, kmode, izflag, iyflag
2437 format (/, ' various integers.     npc     ncc     npp    ncct  numaki  lastov  ipunch  ialter  iearth  itypec   isyst   kmode  izflag  iyflag', /, 18x, 14i8)
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 2441) (i, bi1(i), bi2(i), bi3(i), al1i(i), al2i(i), i = 1, npc)
2441 format (/, " derived vectors of length  'npc'  in  'subr47' , as matrix grunt begins.", /, 1x, 5x, 'row', 17x, 'bi1', 17x, 'bi2', 17x, 'bi3', 16x, 'al1i', 16x, 'al2i', /, (1x, i8, 5e20.11))
  if (itypec .eq. 3) call ptzy1 (radi, dci, thc, dr0, th0, al0, ldm)
  if (kill .ge. 1)   go to 9200
  nw = npc
  nz = ncc
  if (itypec .ne. 3)   go to 1710
  nw = npp
  !  if (npp) 1750, 1750, 1711
  if (npp .le. 0) then
     go to 1750
  else
     go to 1711
  end if
1710 write (unit = lunit(6), fmt = 915) (di(i), i = 1, npc)
1711 write (unit = lunit(6), fmt = 917) freq, ik, ips
  write (unit = lunit(6), fmt = 913) roe
  if (iearth .ne. 99)   go to 700
  write (unit = lunit(6), fmt = 916) dep1, dep2, roe, roe3, roe4, htoj2, htoj3, htoj4, hyud2, hyud3, hyud4
700 if (iprsup .ge. 3)  write (unit = lunit(6), fmt = 1714) (i, rad(i), radp(i), di(i), hi(i), i = 1, nw)
1714 format (/, " Before call to  'simp' .", 7x, 'i', 12x, 'rad', 11x, 'radp', 13x, 'dd', 14x, 'h', /, (25x, i8, 4e15.6))
  if (npais .eq. 0) go to 610
  write (unit = lunit(6), fmt = 919) cname
  go to 615
610 if (ncros .eq. 0) go to 714
615 if (itypec .ne. 1) go to 620
  write (unit = lunit(6), fmt = 920) xtotal, xmajor, npais
  go to 714
620 if ((npais .gt. 0) .and. (ncros .eq. 0)) go to 625
  write (unit = lunit(6), fmt = 921) xtotal, xmajor, npais, rsg
  go to 630
625 write (unit = lunit(6), fmt = 922) xtotal, xmajor, npais
630 if (ncros .ne. 0) write (unit = lunit(6), fmt = 923)
  if (npais .lt. 0) write (unit = lunit(6), fmt = 924)
  if (npais .gt. 0) write (unit = lunit(6), fmt = 925)
919 format ('0', 10x, 'Data cards by pi-circuit modeling are punched out for the following specifications ; node name ', a1, "'" )
920 format (10x, 'Total length of overhead line =', e12.5, /, 10x, 'length of one pi-section = ', e12.5, 3x, 'number of pi sections = ', i3)
921 format (10x, 'Total length of cable = ', e12.5, /, 10x, 'length of one major section = ', e12.5, 3x, 'number of major sections = ', i3, /, 10x, 'sheath grounding resistance at major section ; ', e12.5)
922 format (10x, 'Total lrngth of cable = ', e12.5, /, 10x, 'length of one major section =  ', e12.5, 3x, 'number of major sections = ', i3)
923 format (10x, 'The cable is crossbonded.   ')
924 format (10x, 'Discrete pi-circuit modeling  ')
925 format (10x, 'Homogeneous pi-circuit modeling   ')
714 call simp (nw, hi, di, rad, zy, dir, dij, ang, ldm, ldn )
  if (kill .ge. 1) go to 9200
  if (iprsup .lt. 3) go to 1750
  write (unit = lunit(6), fmt = 1719) nw, nz, isyst, itypec, iearth, npc, ncc
1719 format (/, " After call to  'simp' .      nw      nz   isyst  itypec  iearth     npc     ncc", /, 24x, 7i8)
  write (unit = lunit(6), fmt = 1721) ((zy(i, j), j = 1, nw), i = 1, nw)
1721 format (/, ' ((zy(i,j), j=1, nw), i=1, nw)', /, (1x, 8e15.8))
1750 call ymatrx (isyst, lunit(6), ncpp, zy, yz, esi, al0, al1i, al2i, al3i, yzn(1), yzn(lnq1), ldm, ldn)
  if (kill .ge. 1) go to 9200
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 1723) ((yz(i, j), j = 1, nz), i = 1, nz)
1723 format (/, " ((yz(i,j), j=1, nz), i=1, nz)   after call to  'ymatrx' .", /, (1x, 8e15.8))
  do i = 1, nz
     do j = 1, nz
        if (j .lt. i) go to 702
        ys(i, j) = cmplxz (yz(i, j), fzero)
        ys(j, i) = ys(i, j)
702  end do
  end do
  call minv (ys, nz, f, ldn, ldn2)
  if (kill .ge. 1) go to 9200
  if (itypec .ne. 1) go to 703
  if (isyst .eq. 2) call transp (ys, ncpp, yzn(1 :), yzn(ldn2 :), ca, ldm, ldn)
  if (kill .ge. 1) go to 9200
703 do i = 1, nz
     do j = 1, nz
        if (j .lt. i) go to 705
        yz(i, j) = realz (ys(i, j))
        yz(j, i) = yz(i, j)
705  end do
  end do
  nx = ngrnd
  if (npais .ge. 0 .and. ncros .ne. 0) nx = 4
  if (npais .ne. 0) call gomen (itypec, npc, nx, npais, ncros, irsep, ncpp, ldm)
  if (iprsup .ge. 3) write (unit = lunit(6), fmt = 2457) ((yz(i, j), j = 1, nz), i = 1, nz)
2457 format (/, " in  'subr47' ,   capacitance.   ( (yz(i,j), j=1, nz), i=1, nz )", /, (1x, 6e20.11))
  iprint = 0
9003 continue
706 if (ck1 .ne. -fltinf) go to 7446
  if (dist .le. 0.0d0) go to 7446
  ck1 = dist
7446 if (ci1 .ne. -fltinf) go to 7449
  if (roe .le. 0.0d0) go to 7449
  ci1 = roe
7449 if (ialter .eq. 2) roe = ci1
  iii = 0
  kkk = 1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2469) nz, ik, ialter, iprsup, roe, ci1, ck1, dist, yz(1, 1), yz(1, 2)
2469 format (/, " In  'subr47' ,   done with capacitance.      nz      ik  ialter  iprsup", /, 40x, 4i8, /, 1x, 17x, 'roe', 17x, 'ci1', 17x, 'ck1', 16x, 'dist', 13x, 'yz(1,1)', 13x, 'yz(1,2)', /, 1x, 6e20.11)
  if (ik .gt. 0) go to 7456
  iprint = iprint + 1
  if (lastov .ne. 39) go to 7451
  if (iprint .gt. 1) go to 7451
  kmode = 1
  l1 = 1
  l0 = 0
  d9 = dist * tenm3
!!!   write (*,*)  ' %%-- guts47 write on  lunit9.',
!!!   1             '    dist, d9, =',  dist, d9,
  write (unit = lunit(9)) l1, l1, d9, l0, itrnsf
7451 if (ialter .ne. 1) go to 3007
  volti(iprint) = roe
  voltk(iprint) = freq
  if (iprint .le. ldisfr) go to 3007
  kill = 170
  lstat(14) = ldisfr
  lstat(19) = 7456
  go to 9200
7456 factor = ips
  factor = valu14 / factor
  fdecad = freq
  iprint = iprint + 1
  voltbc(1) = freq
  voltbc(2) = freq * 10. ** ik
  voltbc(3) = expz(factor)
  d13 = voltbc(2) / voltbc(1)
  voltbc(4) = alogz(d13) / alogz(voltbc(3)) + 1.5
  voltbc(5) = ik
  voltbc(6) = ips
  write (*,*) ' guts47.   below s.n. 7456, voltbc(6) = ',  voltbc(6)
  dist = 0.0
  icheck = iprint - 1
  go to 3007
3005 iii = iii + 1
  fdecad = fdecad * 10.0
  freq = fdecad
  iprint = iprint + 1
  kkk = 1
  go to 3007
3006 pkkk = kkk
  freq = fdecad * expz(pkkk * factor)
  kkk = kkk + 1
  if (kkk .gt. ips)   go to 3005
  iprint = iprint + 1
  if (iprs47 .ge. 1) write (unit = lunit(6), fmt = 23006) iprint, kkk, iii, freq, ci1, ck1
23006 format (/, ' next logarithmacally spaced frequency.  iprint     kkk     iii', 11x, 'freq', 12x, 'ci1', 12x, 'ck1', /, 39x, 3i8, 3e16.6)
3007 if (numaki .eq. 9)  go to 730
  text4 = text2
  if (itypec .ne. 1) text4 = text3
  write (unit = lunit(6), fmt = 941) text4, freq
941 format (//, ' *******************  ',  a6, ' constants matrices for frequency =', e15.6, ' Hz   **********************************')
730 w = freq * twopi
  if (numaki .eq. 1) freqs = freq
  call zymx (w, nz, isyst, ngrnd, ngg, ncpp, radi, zy, yz, dir, dij, ang, usi, usr, esi, dr0, th0, al0, hi, di, bio, bi1, bi2, bi3, bi4, bi5, al1i, al2i, al3i, dci, nx, yzn, ys, yc, zp, zpc, zs, ze, zc, ca, cb, cc, cd, f, ldm, ldn, ldn2, lnq2)
  if (npais .eq. 0) go to 810
  write (unit = lunit(6), fmt = 951)
  npk = lnq2 / 3
  nki = npk + 1
  nkj = nki + npk
  call datout (w, zc, yc, rsg, xmajor, nx, npais, ncros, irsep, cname, ldn, yzn(1), yzn(nki), yzn(nkj), npk)
951 format (//, '0', 10x, '*****lists of punched out data cards*****')
810 continue
  if (kill .ge. 1) go to 9200
  if (kmode .eq. 0) go to 750
  ktab = nx
  call prcon (w, nx, zc, zs, ys, yc, yo, qn, gn, ze, a, ai, b, bi, yzn, ca, cb, cc, f, ldn, ldn2, lnq2, mrr, nrp)
  if (kill .ge. 1)   go to 9200
  if (lastov .ne. 43)   go to 750
  if (ipunch .eq. 0)   go to 750
  if (ik .gt. 0)   go to 2745
  if (liu .ne. 0)   go to 2745
  cczero = aimagz (ys(1, 1)) / w / tenm6
  write (unit = lunit(9)) cczero
  liu = 1
  if (iprs47 .ge. 1) write (unit = lunit(6), fmt = 1745) freq, cczero
1745 format (' capacitance on unit9 at ', e15.6, 'Hz.', 5x, e16.8)
2745 rzero = realz(zs(1, 1))
  xzero = aimagz (zs(1, 1)) / w * 1000.0d0
  write (unit = lunit(9)) rzero, xzero, freq
  if (iprs47 .ge. 1) write (unit = lunit(6), fmt = 3745) rzero, xzero, freq
3745 format (' R, L and F on unit9.', 5x, 3e16.8)
750 if (iii .eq. ik)   go to 9001
  go to 3006
  !!                                            ** identical eigenvalue ?
9001 if (iprint .eq. 1) go to 9002
  if (nrp .gt. 0) go to 9002
  rtio = (mrr * 1.0d0) / (iprint - 1.0d0)
  if (rtio .lt. 0.75d0) go to 9002
  nrp = 1
  freq = freqsv
  do i = 1, (iprint - 1) * ktab
     backspace lunit(9)
     write (unit = *, fmt = *) ' guts47.  backspace lunit 9.  iprint, i = ', iprint
  end do
  do i = 1, (iprint - 1)
     backspace junit4
  end do
  write (unit = *, fmt = *) ' backspace junit4.  junit4, iprint =', junit4, iprint
  iprint = 1
  go to 9003
9002 continue
  !     read input card using cimage
  call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
  read (unit = abuff, fmt = 902) d9, freq, ik, ips, dist, j13, j14
  freqsv = freq
  if (d9 .ne. 0.0d0) go to 3899
  if (freq .eq. 0.0d0) go to 929
3899 roe = d9
  ipunch = j13
  itrnsf = j14
  if (freq .eq. 0.0d0) freq = statfr
  if (ips .eq. 0) ips = 1
  write (unit = kunit6, fmt = 3256) roe, freq, ik, ips, dist, ipunch, itrnsf
  if (iearth .ne. 99)   go to 706
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
  read (unit = abuff, fmt = 901) dep1, dep2, roe3, roe4
  write (unit = kunit6, fmt = 3280)
  !     read input card using cimage.
  call cimage
  read (unit = abuff, fmt = 4230) bufsem
  if (ialter .ne. 2) write (unit = lunit(2), fmt = 4230) bufsem
  read (unit = abuff, fmt = 901) htoj2, htoj3, htoj4, hyud2, hyud3, hyud4
  write (unit = kunit6, fmt = 3290)
  alf1 = roe / roe3
  alf2 = roe / roe4
  go to 706
929 write (unit = kunit6, fmt = 930)
930 format ('+Blank card terminating frequency cards.')
  call interp
  write (unit = lunit(6), fmt = 4239)
4239 format (///, 1x)
  go to 5
9200 lstat(18) = nchain
  if (ialter .eq. 2) lunit(5) = l5save
  lastov = nchain
  nchain = 51
9900 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 2592) nchain, lunit(5), numaki, ialter
2592 format (/, " Exit  'subr47' .  nchain  lunit5  numaki  ialter", /, 17x, 4i8)
  return
end subroutine guts47

!
! subroutine crosa4.
!

subroutine crosa4 (czy, icont, ldn, ca, cb, cc, cd, ce, cf, cg, f, ldn2)
  implicit none
  integer(4), intent(in) :: icont
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ldn2
  complex(16), intent(out) :: czy(ldn, ldn)
  complex(16), intent(out) :: ca(ldn, ldn)
  complex(16), intent(out) :: cb(ldn, ldn)
  complex(16), intent(out) :: cc(ldn, ldn)
  complex(16), intent(out) :: cd(ldn, ldn)
  complex(16), intent(out) :: ce(ldn, ldn)
  complex(16), intent(out) :: cf(ldn, ldn)
  complex(16), intent(out) :: cg(ldn, ldn)
  complex(16), intent(in) :: f(ldn, ldn2)
  integer(4) :: i
  integer(4) :: j
  complex(16) :: cwork1
  !
  write (unit = 6, fmt = 1001)
1001 format ('  Begin module "crosa4".  ******')
  cwork1=0.
  do i=4,6
     cwork1 = cwork1 + czy(i,i)
  end do
  cwork1 = cwork1/3.
  do i = 4, 6
     czy(i, i) = cwork1
  end do
  cwork1 = czy(4, 5) + czy(4, 6) + czy(5, 6)
  cwork1 = cwork1 / 3.0d0
  czy(4, 5) = cwork1
  czy(4, 6) = cwork1
  czy(5, 6) = cwork1
  do i = 1, 3
     cwork1 = 0.0d0
     do j = 4, 6
        cwork1 = cwork1 + czy(i, j)
     end do
     cwork1 = cwork1 / 3.0d0
     do j = 4, 6
        czy(i, j) = cwork1
     end do
  end do
  do i = 1, 6
     do j = 1, 6
        if(i .le. 3 .and. j .le. 3) cycle
        if(i .ge. j) cycle
        czy(j, i) = czy(i, j)
     end do
  end do
  if (icont .eq. 0) call minvn (czy, 6, 3, 0, ldn, ca, cb, cc, cd, ce, cf, cg, f, ldn2)
  do j = 1,3
     czy(4,j) = czy(4,j) + czy(5,j) + czy(6,j)
     czy(j,4) = czy(j,4) + czy(j,5) + czy(j,6)
  end do
  czy(4,4) = czy(4,4) + czy(5,4) + czy(6,4) + czy(4,5) + czy(5,5) + czy(6,5) + czy(4,6) + czy(5,6) + czy(6,6)
  if(icont.eq.0) call minvn (czy, 4, 3, 0, ldn, ca, cb, cc, cd, ce, cf, cg, f, ldn2)
  return
end subroutine crosa4

!
! subroutine minvn.
!

subroutine minvn (cinout, n, l, ix, ldn, ca, cb, cc, cd, cwork1, cwork2, cwork3, f, ldn2)
  implicit none
  integer(4), intent(in) :: ix
  integer(4), intent(in) :: l
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ldn2
  integer(4), intent(in) :: n
  complex(16), intent(out) :: ca(ldn, ldn)
  complex(16), intent(out) :: cb(ldn, ldn)
  complex(16), intent(out) :: cc(ldn, ldn)
  complex(16), intent(out) :: cd(ldn, ldn)
  complex(16), intent(out) :: cinout(ldn, ldn)
  complex(16), intent(out) :: cwork1(ldn, ldn)
  complex(16), intent(out) :: cwork2(ldn, ldn)
  complex(16), intent(out) :: cwork3(ldn, ldn)
  complex(16), intent(in) :: f(ldn, ldn2)
  integer(4) :: i, ipl
  integer(4) :: j, jpl
  integer(4) :: nml
  !
  nml = n - l
  call cutmat (cinout, ca, cb, cc, cd, n, l, ldn)
  do i = 1, nml
     do j = 1, nml
        cwork1(i, j) = cd(i, j)
     end do
  end do
  call minv (cwork1, nml, f, ldn, ldn2)
  call mxmnm (cwork1, cc, cwork2, nml, nml, l, ldn)
  call mxmnm (cb, cwork2, cwork1, l, nml, l, ldn)
  do i = 1, l
     do j =1, l
        cwork1(i, j) = ca(i, j) - cwork1(i, j)
        cinout(i, j) = cwork1(i, j)
     end do
  end do
  if(ix.eq.1) return
  call minv(cwork1,l,f,ldn,ldn2)
  do i=1,l
     do j=1,l
        cinout(i,j)=cwork1(i,j)
     end do
  end do
  call mxmnm (cwork2, cwork1, cwork3, nml, l, l, ldn)
  do i=1,nml
     do j=1,l
        ipl=i+l
        cinout(ipl,j)=-cwork3(i,j)
     end do
  end do
  do i=1,l
     do j=1,l
        cwork1(i,j)=ca(i,j)
     end do
  end do
  call minv(cwork1,l,f,ldn,ldn2)
  call mxmnm(cwork1,cb,cwork2,l,l,nml,ldn)
  call mxmnm(cc,cwork2,cwork1,nml,l,nml,ldn)
  do i=1,nml
     do j=1,nml
        cwork1(i,j)=cd(i,j)-cwork1(i,j)
     end do
  end do
  call minv(cwork1,nml,f,ldn,ldn2)
  do i=1,nml
     do j=1,nml
        ipl=i+l
        jpl=j+l
        cinout(ipl,jpl)=cwork1(i,j)
     end do
  end do
  call mxmnm(cwork2,cwork1,cwork3,l,nml,nml,ldn)
  do i=1,l
     do j=1,nml
        jpl=j+l
        cinout(i,jpl)=-cwork3(i,j)
     end do
  end do
  return
end subroutine minvn

!
! subroutine cutmat.
!

subroutine cutmat (cinput, ca, cb, cc, cd, n, l, ldn)
  implicit none
  integer(4), intent(in) :: l
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: n
  complex(16), intent(out) :: ca(ldn, ldn)
  complex(16), intent(out) :: cb(ldn, ldn)
  complex(16), intent(out) :: cc(ldn, ldn)
  complex(16), intent(out) :: cd(ldn, ldn)
  complex(16), intent(in) :: cinput(ldn, ldn)
  integer(4) :: i, ipl
  integer(4) :: j, jpl
  integer(4) :: nml
  !
  do i = 1, l
     do j = 1, l
        ca(i, j) = cinput(i, j)
     end do
  end do
  nml = n - l
  do i = 1, l
     do j = 1, nml
        jpl = j + l
        cb(i, j) = cinput(i, jpl)
     end do
  end do
  do i = 1, nml
     do j = 1 ,l
        ipl = i + l
        cc(i, j) = cinput(ipl, j)
     end do
  end do
  do i = 1, nml
     do j = 1, nml
        ipl = i + l
        jpl = j + l
        cd(i, j) = cinput(ipl, jpl)
     end do
  end do
  return
end subroutine cutmat

!
! subroutine mxmnm.
!

subroutine mxmnm (ca, cb, cc, l, m, n, ldn)
  implicit none
  integer(4), intent(in) :: l
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: m
  integer(4), intent(in) :: n
  complex(16), intent(in) :: ca(ldn, ldn)
  complex(16), intent(in) :: cb(ldn, ldn)
  complex(16), intent(out) :: cc(ldn, ldn)
  integer(4) :: i
  integer(4) :: j
  integer(4) :: k
  !
  do i = 1, l
     do j = 1, n
        cc(i, j) = 0.0d0
        do k = 1, m
           cc(i,j ) = cc(i, j) + ca(i, k) * cb(k, j)
        end do
     end do
  end do
  return
end subroutine mxmnm

!
! subroutine datout.
!

subroutine datout (w, zc, yc, rs, xmajor, nub6, npais, nncros, irsep, cha, ldn, r, al, c, npk)
  use iocons
  use tracom
  implicit none
  !     8.19 data cards punch out subroutine
  character(1), intent(in) :: cha
  integer(4), intent(in) :: irsep
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: nncros
  integer(4), intent(in) :: npais
  integer(4), intent(in) :: npk
  integer(4), intent(in) :: nub6
  real(8), intent(out) :: al(npk)
  real(8), intent(out) :: c(npk)
  real(8), intent(out) :: r(npk)
  real(8), intent(in) :: rs
  real(8), intent(in) :: xmajor
  real(8), intent(in) :: w
  complex(16), intent(in) :: yc(ldn, ldn)
  complex(16), intent(in) :: zc(ldn, ldn)
  integer(4) :: i, i1, ipri
  integer(4) :: j, j1
  integer(4) :: k, k1, k2, kk, kstep, kstop
  integer(4) :: l, ltype, lunit(6), lunit7
  integer(4) :: mtype
  integer(4) :: ncros, ntype, nub3
  real(8) :: xleng
  !
  lunit(6) = 6
  lunit7 = 7
  !     ltype=0 ; output rs
  !     ltype=1 ; not output rs
  !     ntype=0 ; crossbonded cable
  !     ntype=1 ; non-crossbonded cable
  !     mtype=0 ; all sheathes are conected
  !     mtype=1 ; all sheathes are not conected
  ncros = iabs (npais)
  ltype = 0
  if (npais .gt. 0 .and. nncros .eq. 0) ltype = 1
  ntype = 1
  if (npais .lt. 0 .and. nncros .ne. 0) ntype = 0
  mtype = irsep
  if (npais .gt. 0 .and. nncros .eq. 0) mtype = 1
  nub3 = 3
  if (ntype .eq. 1) nub3 = 1
  l = 0
  xleng = xmajor
  if (ntype .eq. 0) xleng = xleng / 3.0d0
  do i = 1, nub6
     do j = 1, nub6
        if (j .gt. i) cycle
        l = l + 1
        r(l) = realz (zc(i, j)) * xleng
        al(l) = aimagz (zc(i, j)) / w * xleng * 1.0e3
        c(l) = aimagz (yc(i, j)) / w * xleng * 1.0e6
     end do
  end do
  write (unit = lunit(6), fmt = 1400)
  write (unit = lunit7, fmt = 1401)
1400 format (1x, '$vintage, 1')
1401 format ('$vintage, 1')
  l = 0
  if (ltype .eq. 1) go to 610
  kstep = 1
  kk = nub6 / 2 + 1
  if (nub6 .eq. 4) kk = 4
  kstop = (nub6 + 1) / 2 + 1
  if (mtype .ne. 0) kstop = nub6
  if (nub6 .eq. 4) kstop = 4
  if (nub6 .eq. 7 .and. mtype .eq. 0) kstop = 4
  do k = kk, kstop, kstep
     write (unit = lunit(6), fmt = 1300) cha, k, rs
     write (unit = lunit7, fmt = 1301) cha, k, rs
  end do
1300 format (' ', 2x, a1, 'in ', i2, 18x, e16.5)
1301 format (2x, a1, 'in ', i2, 18x, e16.5)
610 continue
  !     i ; number of major section
  !     j ; number of minor section
  !     k ; number of phase
  do i = 1, ncros
     do j = 1, nub3
        do k = 1, nub6
           !     following two statements are for the third minor section
           k2 = k
           if (mtype .eq. 1) go to 910
           if (j .eq. 3 .or. ntype .eq. 1) call cha444(k, k2)
910        continue
           !     following two statements are for crossbonding
           k1 = k
           if (j .ne. 1) call cha645(k, k1)
           !     following one statement is for the first minor section
           if (j .eq. 1 .and. mtype .ne. 1) call cha444(k, k1)
           !     following two statements are for seriese conection of
           !     major section
           j1 = j
           if (ntype .ne. 1) call cha312(j, j1)
           !     following two statements are for sending end
           i1 = i
           if (j .eq. 1) i1 = i - 1
           ipri = 1
           if (i1 .eq. 0) ipri = 2
           if (i .ne. ncros) go to 700
           if (ntype .eq. 1 .or. j .eq. 3) ipri = 3
           !     ipri=1 ;
           !     ipri=2 ; sending end
           !     ipri=3 ; recieving end
700        continue
           if (ncros .eq. 1 .and. ntype .eq. 1) ipri = 4
           call pri (i, j, k, i1, j1, k1, k2, l, ipri, cha, r, al, c,npk)
        end do
     end do
     if (ltype .eq. 1) cycle
     j = 3
     if (ntype .eq. 1) j = 1
     if (i .eq. ncros) go to 2010
     do k = kk, kstop, kstep
        write (unit = lunit(6), fmt = 2000) cha, i, j, k, cha, k
        write (unit = lunit7, fmt = 2001) cha, i, j, k, cha, k
     end do
2000 format (' ', 2x, a1, i2, i1, i2, 6x, a1, 'in ', i2)
2001 format (2x, a1, i2, i1, i2, 6x, a1, 'in ', i2)
     cycle
2010 continue
     do k = kk, kstop, kstep
        write (unit = lunit(6), fmt = 2100) cha, k, cha, k
        write (unit = lunit7, fmt = 2101) cha, k, cha, k
     end do
2100 format (' ', 2x, a1, 'out', i2, 6x, a1, 'in ', i2)
2101 format (2x, a1, 'out', i2, 6x, a1, 'in ', i2)
  end do
  write (unit = lunit(6), fmt = 1410)
  write (unit = lunit7, fmt = 1411)
1410 format (' ', '$vintage, 0')
1411 format ('$vintage, 0')
  return
end subroutine datout

!
! subroutine cha645.
!

subroutine cha645 (k, k1)
  implicit none
  integer(4), intent(in) :: k
  integer(4), intent(out) :: k1
  integer(4) :: kk
  !
  if (k .le. 3 .or. k .ge. 7) return
  kk = k - 3
  !  go to (10,20,30),kk
  select case (kk)
  case (1)
     k1 = 6

  case (2)
     k1 = 4

  case (3)
     k1 = 5
  end select
  return
end subroutine cha645

!
! subroutine cha312.
!

subroutine cha312 (j, j1)
  implicit none
  integer(4), intent(out) :: j1
  integer(4), intent(in) :: j
  !
  !   go to (10,20,30),j
  select case (j)
  case (1)
     j1 = 3

  case (2)
     j1 = 1

  case (3)
     j1 = 2
  end select
  return
end subroutine cha312

!
! subroutine cha444.
!

subroutine cha444 (k, kk)
  implicit none
  integer(4), intent(in) :: k
  integer(4), intent(out) :: kk
  !
  if (k .le. 3 .or. k .gt. 7) return
  kk = 4
  return
end subroutine cha444

!
! subroutine pri.
!

subroutine pri (i, j, k, i1, j1, k1, k2, l, ipri, cha, r, al, c, npk)
  use iocons
  implicit none
  character(1), intent(in) :: cha
  integer(4), intent(in) :: i
  integer(4), intent(in) :: i1
  integer(4), intent(in) :: ipri
  integer(4), intent(in) :: j
  integer(4), intent(in) :: j1
  integer(4), intent(in) :: k
  integer(4), intent(in) :: k1
  integer(4), intent(in) :: k2
  integer(4), intent(out) :: l
  integer(4), intent(in) :: npk
  real(8), intent(in) :: al(npk)
  real(8), intent(in) :: c(npk)
  real(8), intent(in) :: r(npk)
  integer(4) :: ll, lunit(6), lunit7
  !
  lunit(6) = 6
  lunit7 = 7
  continue
  if (k .ne. 1) go to 15
  write (unit = lunit(6), fmt = 1000) k, cha, i1, j1, k1, cha, i, j, k2, cha, cha
  write (unit = lunit7, fmt = 1001) k, cha, i1, j1, k1, cha, i, j, k2, cha, cha
1000 format (' ', i2, a1, i2, i1, i2, a1, i2, i1, i2, a1, 'in  1', a1, ' 11 1')
1001 format (i2, a1, i2, i1, i2, a1, i2, i1, i2, a1, 'in  1', a1, ' 11 1')
  return
15 continue
  write (unit = lunit(6), fmt = 1010) k, cha, i1, j1, k1, cha, i, j, k2
  write (unit = lunit7, fmt = 1011) k, cha, i1, j1, k1, cha, i, j, k2
1010 format (' ', i2, a1, i2, i1, i2, a1, i2, i1, i2)
1011 format (i2, a1, i2, i1, i2, a1, i2, i1, i2)
  return
  continue
  l = l + 1
  write (unit = lunit(6), fmt = 1100) k, cha, k1, cha, i, j, k2, r(l), al(l), c(l)
  write (unit = lunit7, fmt = 1101) k, cha, k1, cha, i, j, k2, r(l), al(l), c(l)
1100 format (' ', i2, a1, 'in ', i2, a1, i2, i1, i2, 12x, 3e16.5)
1101 format (i2, a1, 'in ', i2, a1, i2, i1, i2, 12x, 3e16.5)
2000 continue
  if (k .eq. 1) return
  do ll = 2, k
     l=l+1
     write(lunit(6),1110) r(l),al(l),c(l)
     write(lunit7,1111) r(l),al(l),c(l)
1110 format(1h ,26x,3e16.5)
1111 format(26x,3e16.5)
  end do
  return
  if (k .ne. 1) go to 35
  write (unit = lunit(6), fmt = 1200) k, cha, i1, j1, k1, cha, k2, cha, cha
  write (unit = lunit7, fmt = 1201) k, cha, i1, j1, k1, cha, k2, cha, cha
1200 format (' ',i2, a1, i2, i1, i2, a1, 'out', i2, a1, 'in  1', a1, ' 11 1')
1201 format (i2, a1, i2, i1, i2, a1, 'out', i2, a1, 'in  1', a1, ' 11 1')
  return
35 continue
  write(lunit(6),1210) k,cha,i1,j1,k1,cha,k2
  write(lunit7,1211) k,cha,i1,j1,k1,cha,k2
1210 format(1h ,i2,a1,i2,i1,i2,a1,3hout,i2)
1211 format(i2,a1,i2,i1,i2,a1,3hout,i2)
  return
  l = l + 1
  write(lunit(6),1300) k,cha,k1,cha,k2,r(l),al(l),c(l)
  write(lunit7,1301) k,cha,k1,cha,k2,r(l),al(l),c(l)
1300 format(1h ,i2,a1,3hin ,i2,a1,3hout,i2,12x,3e16.5)
1301 format(i2,a1,3hin ,i2,a1,3hout,i2,12x,3e16.5)
  go to 2000
end subroutine pri

!
! subroutine nyan.
!

subroutine nyan (itype, npc, nc, ncpp, ngrnd, ncros, npais, ldm)
  use iocons
  implicit none
  integer(4), intent(in) :: itype
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: nc
  integer(4), intent(in) :: ncpp(ldm)
  integer(4), intent(in) :: ncros
  integer(4), intent(in) :: ngrnd
  integer(4), intent(in) :: npais
  integer(4), intent(in) :: npc
  integer(4) :: i
  integer(4) :: lunit(6)
  integer(4) :: nwork
  !
  !  dimension ncpp(ldm)
  !
  lunit(6) = 6
  if (itype .eq. 1) return
  if (ncros .eq. 0) return
  if (npc .ne. 3) go to 9000
  do i = 1, 3
     if (ncpp(i) .eq. 1 .or. ncpp(i) .gt. 3) go to 9100
  end do
  nwork = 6 + ngrnd
  if (nwork .ne. nc) go to 9200
  if (ncpp(1) .le. ncpp(2) .and. ncpp(2) .le. ncpp(3)) return
  go to 9300
9000 continue
  write (unit = lunit(6), fmt = 7000)
  write (unit = lunit(6), fmt = 8000)
8000 format ('0', 10x, "Number of phases should be '3'.")
  call stoptp
9100 continue
  write (unit = lunit(6), fmt = 7000)
  write (unit = lunit(6), fmt = 8100)
8100 format('0', 10x, 'number of conductors in one phase should', "be '2' (core and sheath) with 'ngrnd'='0", "' for an sc cable and 'ngrnd'='1' for a /", ' ', 10x, "pt cable, of '3' (core, sheath and armor", ") with 'ngrnd'='3, considering the fact ", 'that all the 3-phase cables have the    /' ' ', 10x, 'same configuration.')
  call stoptp
9200 continue
  write (unit = lunit(6), fmt = 7000)
  write (unit = lunit(6), fmt = 8200)
8200 format('0', 10x, "In the case of 'npais.ge.0 .and. ncros.ne.0', the final number of conductors  ",/,11x, "considering grounded conductors reduction should be '6'.", /, 11x, "'ngrnd' should be '3' for an sc cable with armor and '1' for a pt cable.  If a 3-phase sc cable with armor is enclosed  ",/, ' ', 10x, 'within a pipe (i.e., pt cable), please,  neglect the pipe, i.e., regard as an sc ', "cable with 'ngrand'='3', considering the", /, ' ', 10x, 'fact that all the 3-phase cables have   the same configuration.')
  call stoptp
9300 continue
  write (unit = lunit(6), fmt = 7000)
  write (unit = lunit(6), fmt = 8300)
8300 format('0',10x, 'Eeach cable of a 3-phase cable system has the same configuration in general. If not, please arrange the data cards as', /, ' ', 10x, 'follows : first comes a cable of which the number of conductors is smallest, second comes a cable of the second smallest', /, ' ', 10x, 'number of conductors,.... Please check your data.')
  call stoptp
  return
7000 format('0', 10x, "Errors for a crossbonded cable (ncros.ne.0) when 'npais'='0'.")
end subroutine nyan

!
! subroutine gomen.
!

subroutine gomen (itype, npc, nx, npais, ncros, irsep, ncpp, ldm)
  use iocons
  implicit none
  integer(4), intent(in) :: irsep
  integer(4), intent(in) :: itype
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: ncpp(ldm)
  integer(4), intent(in) :: ncros
  integer(4), intent(in) :: npais
  integer(4), intent(in) :: npc
  integer(4), intent(in) :: nx
  integer(4) :: i
  integer(4) :: lunit(6)
  !
  !  dimension ncpp(ldm)
  !
  lunit(6) = 6
  if (npais .lt. 0) go to 1000
  if (ncros .ne. 0) go to 1200
  if (irsep .eq. 0) return
  go to 9120
1000 if (ncros .eq. 0) go to 1100
  if (nx .lt. 6 .or. nx .gt. 7) go to 9010
1200 if (npc .ne. 3) go to 9020
  do i = 1, 3
     if (ncpp(i) .eq. 1 .or. ncpp(i) .gt. 3) go to 9030
  end do
  if (ncpp(1) .gt. ncpp(2) .or. ncpp(2) .gt. ncpp(3)) go to 9040
  if (nx .eq. 6) return
  if (npais .le. 0 .or. ncros .eq. 0) go to 200
  if (nx .eq. 4) return
200 if (ncpp(2) .eq. 2) return
  go to 9110
1100 if (nx .gt. 7) go to 9050
  if (nx .ge. 6) go to 1200
  if (nx .eq. 2) go to 1300
  if (nx .eq. 3) go to 1400
  go to 9060
1400 if (irsep .eq. 0) go to 9070
  if (npc .ne. 1) go to 9080
  return
1300 if (npc .ne. 1) go to 9090
  return
9010 continue
  write (unit = lunit(6), fmt = 8010)
8010 format ('0', 10x, "In the case of 'npais.lt.0.and.ncros.ne.0' the final number of conductors including grounded conductors reduction should", / ' ', 10x, "be 6 or 7, i.e., 'total number of conductors of the cable system'-'ngrnd'='6' or, '7', but 'ngrnd' should be '1' for a pt", / ' ' ,10x, 'cable.')
  call stoptp
9020 continue
  write (unit = lunit(6), fmt = 8020)
8020 format ('0', 10x, "Number of phases should be '3'.")
  call stoptp
9030 continue
  write (unit = lunit(6), fmt = 8030)
8030 format ('0', 10x, "Number of conductors in one phase should be '2' (core and sheath) with 'ngrnd'='0' for an sc cable and 'ngrnd'='1' for a", /, 1x, 10x, "pt cable, or '3' (core, sheath and armor) with 'ngrnd'='3' for an sc cable.")
  call stoptp
9040 continue
  write (unit = lunit(6), fmt = 8040)
8040 format ('0', 10x, 'Eeach cable of a 3-phase cable system has the same configuration in general. If not, please arrange the data cards as', / 1x, 10x, 'follows : first comes a cable of which the number of conductors is smallest, second comes a cable of the second smallest ', /, 1x, 10x, 'number of conductors,.... Please check your data')
  call stoptp
9050 continue
  write (unit = lunit(6), fmt = 8050)
8050 format ('0', 10x, "In the case of 'npais.lt.0', the final number of conductors including grounded conductors reduction should not be", /, 1x, 10x, "greater than '7'. Please check your data.")
  call stoptp
9060 continue
  write (unit = lunit(6), fmt = 8060)
8060 format('0', 10x, "In the case of 'npais.lt.0', the final number of conductors including grounded conductors reduction should not be '1' or", /, 1x, 10x, "'4' or '5'. Please check your data.")
  call stoptp
9070 continue
  write (unit = lunit(6), fmt = 8070)
8070 format('0', 10x, "In this case, it should not be 'irsep=0', please change 'irsep=1'")
  call stoptp
9080 continue
  write (unit = lunit(6), fmt = 8080)
8080 format('0', 10x, "In this case, number of phases (npc) shohuld only be '1'. Please check your data.")
  call stoptp
9090 continue
  write (unit = lunit(6), fmt = 8090)
8090 format ('0', 10x, "Sorry, in the case of 'npais.lt.0', a two phase cable (npc=2) consisting only of core or consisting of core and sheath  ", /, 1x, 10x, 'can not be dealt with. Please check your data.')
  call stoptp
9110 continue
  write (unit = lunit(6), fmt = 8110)
8110 format (1x, 10x, "In this case, ncpp(2) should be '2'. Please check your data.")
  call stoptp
9120 continue
  write (unit = lunit(6), fmt = 8120)
8120 format ('0', 10x, "In the case of 'npais.gt.0.and.ncro,34 hs.eq.0', it should not be 'irsep=1,', please change 'irsep=0'.")
  call stoptp
  return
end subroutine gomen

!
! subroutine prcon.
!

subroutine  prcon (w, nconpw, zc, zs, ys, yc, yo, qn, gn, ze, a, ai, b, bi, an, ca, zo, cc, f, ldn, ldn2, lnq2, mrr, nrp)
  use blkcom
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ldn2
  integer(4), intent(in) :: lnq2
  integer(4), intent(out) :: mrr
  integer(4), intent(in) :: nconpw
  integer(4), intent(in) :: nrp
  real(8), intent(out) :: an(lnq2)
  real(8), intent(out) :: gn(ldn)
  real(8), intent(in) :: w
  integer(4) :: i, i1, icorr, iseq(15), iz
  integer(4) :: j
  integer(4) :: k, kmax(15), kthl
  integer(4) :: ll1, ll2
  integer(4) :: n1, npc1, ntol
  real(8) :: a1, aa, acomi, acomr, aii, air, alpha
  real(8) :: bb, beta
  real(8) :: d18, d19, d55, d56, da, db, deg, dv
  real(8) :: ea, em
  real(8) :: fout
  real(8) :: pp1(20, 20), pp2(20, 20), ping(200), ps(30)
  real(8) :: spdtol
  real(8) :: tir(20, 20), tii(20, 20)
  real(8) :: u1, u2
  real(8) :: vmode
  real(8) :: w1
  real(8) :: ychara, ycharm, yoi, yor, ysi
  real(8) :: zoi, zor, zsi, zsr, zz(30)
  complex(16) :: pp6(20,20),ee6(15)
  complex(16) :: cjw
  complex(16) :: ca(ldn, ldn), zo(ldn, ldn), cc(ldn, ldn)
  complex(16) :: zc(ldn, ldn), zs(ldn, ldn), ys(ldn, ldn), yc(ldn, ldn)
  complex(16) :: a(ldn, ldn), ai(ldn, ldn),  b(ldn, ldn), bi(ldn, ldn)
  complex(16) :: yo(ldn, ldn), ze(ldn, ldn), qn(ldn), f(ldn, ldn2)
  !
  !  dimension gn(ldn), an(lnq2)
  !  dimension tir(20,20), tii(20,20)
  !  dimension pp1(20,20), pp2(20,20)
  !  dimension zz(30), ps(30), ping(200), iseq(15),kmax(15)
  !
  data  iseq  /  15*0  /
  !
  ntol = iprint - 1
  cjw = cmplxz (fzero, w)
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 2624) nconpw, numaki, value1, value3, pai, cjw
2624 format (/, " Enter  'prcon' .  nconpw  numaki", 13x, 'value1', 13x, 'value3', 16x, 'pai', 11x, 'real-cjw',  11x, 'imag-cjw', /, 17x, 2i8, 5e19.10)
  ll1 = 1
  ll2 = 2
  call mxm (zc, yc, zs, nconpw, ldn)
  call eigen (cjw, zs, nconpw, a, ai, qn, ca, zo, cc, ldn)
!!!!  *****
  if (kill .gt. 1)   go to 9200
  if (itypec .ne. 2)  go to 75
  n1 = npc * 2
  if (nconpw .ne. n1 ) go to 75
  icorr = 0
  npc1 = npc + 1
  do i = npc1, npc2
     if (realz (qn(i)) .gt. 0.0d0) cycle
     i1 = i + 1
     if (i1 .gt. npc2)  i1 = i - 1
     if (i1 .le. npc) cycle
     if (realz(qn(i1)) .lt. 0.)  go to 63
61   qn(i) = qn(i1)
     icorr = 1
     write (unit = lunit(6), fmt = 901) i
901  format ('0modifications of modal quantities for mode', i2, ' are made.', /)
     cycle
63   i1 = i + 2
     if (i1 .gt. npc2)  i1 = i - 2
     if (realz(qn(i1)) .gt. 0.)  go to 61
  end do
  if (icorr .eq. 0) go to 75
  do i = npc1, npc2
     i1 = i - npc
     do j = 1, npc2
        ai(i,j) = czero
        if (i1.eq.j)  ai(i,j) = creal1
        if(i .eq. j)  ai(i,j) = -creal1
     end do
  end do
  do i = 1, nconpw
     do j = 1, nconpw
        a(i, j) = ai(i, j)
     end do
  end do
  write (unit = *, fmt = *) ' after #71 '
  call minv (a, nconpw, f, ldn, ldn2)
75 continue
  do iz = 1, 30
     ps(iz) = 0.0d0
     zz(iz) = 0.0d0
  end do
  do i = 1, nconpw
     an(i) = value3 * realz(qn(i))
     gn(i) = w / aimagz( qn(i) )
     if (an(i) .lt. 0.0d0)  go to 50
     a1 = 1.05d0
     spdtol = spdlgt * a1
     if (gn(i) .gt. spdtol) go to 50
     if (gn(i) .lt. spdlgt) go to 55
     gn(i) = spdlgt
     write (unit = lunit(6), fmt = 961)
961  format (1h0, 10x, 33hmodification for imag(q) is made.  //, 1x)
     go to 55
50   write (unit = lunit(6), fmt = 962)
962  format ('0', 10x, 'Conductor impedance may include numerical errors.  ', /, &
          11x, 'The correct solutions for modes showing errors can be given by the very last mode.  ', //, 1x)
55   continue
     !       if (lastov .ne. 39 )  go to 600
     !       if (iprint .gt. 1 )  go to 600
     do j = 1, nconpw
        bi(i, j) = a(j, i)
        b(i, j) = ai(j, i)
        if (lastov .ne. 39) cycle
        if (iprint .gt. 1) cycle
        tir(i, j) = realz (b(i, j))
        tii(i, j) = aimagz (b(i, j))
     end do
  end do
  if (ntol .eq. 0 .or. lastov .ne. 39) go to 5602
  kthl = 1
  ping(kthl) = alog1z (w / twopi)
  d55 = 1.0d0 / (w / twopi)
  d56 = 1.0d0 / sqrtz (w / twopi)
  do i = 1, nconpw
     !     em=(realz(qn(i)))**2 + (aimagz(qn(i)))**2
     !     ea=2.0*atan2z( aimag(qn(i)),real(qn(i)) )
     em = sqrtz ((realz (qn(i) ** 2)) ** 2 + (aimagz (qn(i) ** 2)) ** 2)
     ea = atan2z (aimagz (qn(i) ** 2), realz(qn(i) ** 2))
     db = value3 * realz (qn(i))
     vmode = w / aimagz (qn(i))
     ping(kthl+1) = em*d55
     ping(kthl+2) = ea/(twopi/360.)
     ping(kthl+3) = vmode
     ping(kthl+4) = db*d56
     kthl = kthl + 4
  end do
  call unwind (ping, kthl, mrr, nrp, ntol, iseq)
  do i = 1, nconpw
     ee6(i) = qn(i) ** 2
     do j = 1, nconpw
        pp6(i, j) = a(i, j)
     end do
  end do
  do i = 1,nconpw
     qn(i) = csqrtz (ee6(iseq(i)))
     do j = 1, nconpw
        a(i, j) = pp6(i, iseq(j))
     end do
  end do
  do i = 1, nconpw
     do j = 1, nconpw
        ai(i, j) = a(i, j)
     end do
  end do
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' prcon after s.n. 5701.   [a]  follows ....'
  do i = 1, nconpw
     if (iprsup .ge. 1) write (unit = *, fmt = *) (a(i,j), j = 1, nconpw)
  end do
  call minv (ai, nconpw, f, ldn, ldn2)
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' after "minv".   [ai]  follows ..... '
  do i = 1, nconpw
     if (iprsup .ge. 1) write (unit = *, fmt = *) (ai(i, j), j = 1, nconpw)
  end do
  do i = 1, nconpw
     do j = 1, nconpw
        pp1(j, i) = sqrtz ((realz (ai(i, j))) ** 2 + (aimagz (ai(i, j))) ** 2)
        pp2(j, i) = atan2z (aimagz (ai(i, j)), realz (ai(i, j)))
     end do
  end do
  if (ntol .eq. 1) go to 5800
  go to 5810
5800 continue
  do j = 1, nconpw
     kmax(j) = 1
     do i = 1, nconpw
        if (pp1(i, j) .le. pp1(kmax(j), j)) cycle
        kmax(j) = i
     end do
  end do
  write (unit = lunit(9))  (kmax(j), j = 1, nconpw)
5810 continue
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' prcon.  use normalization cells kmax(1:nconpw) =', (kmax(i), i = 1, nconpw)
  do j = 1, nconpw
     if (pp1(kmax(j), j) .eq. 0.0d0) cycle
     dv = 1.0d0 / pp1(kmax(j), j)
     da = pp2(kmax(j), j)
     do i = 1, nconpw
        pp1(i, j) = pp1(i, j) * dv
        pp2(i, j) = pp2(i, j) - da
        !     *** to keep angles within principal value region
        if (pp2(i, j) .gt. twopi / 2.0d0) pp2(i, j) = pp2(i, j) - twopi
        if (pp2(i, j) .lt. -twopi / 2.0d0) pp2(i, j) = pp2(i, j) + twopi
        if (iprsup .ge. 3) write (unit = *, fmt = *) 'just before 5624, i, j, pp1(i,j), pp2(i,j)', i, j, pp1(i,j), pp2(i,j)
     end do
  end do
  fout = w / twopi
  !   ******************** temporary diagnostic ************
  if (iprsup .ge. 0) write (unit = *, fmt = *) ' prcon.  freq =',  fout, '     pp1 follow : '
  do i = 1, nconpw
     if (iprsup .ge. 0) write (unit = *, fmt = *) (pp1(i, j), j = 1, nconpw)
  end do
  if (iprsup .ge. 0) write (unit = *, fmt = *) ' pp2  follow ... '
  do i = 1, nconpw
     if (iprsup .ge. 0) write (unit = *, fmt = *) (pp2(i, j), j = 1, nconpw)
  end do
  do i = 1, nconpw
     do j = 1, nconpw
        u1 = (pp1(i, j)) * cosz (pp2(i, j))
        u2 = (pp1(i, j)) * sinz (pp2(i, j))
        ai(j, i) = cmplxz(u1, u2)
     end do
  end do
  do j = 1, nconpw
     do i = 1, nconpw
        a(i, j) = ai(i, j)
     end do
  end do
  call minv (a, nconpw, f, ldn, ldn2)
  do i = 1, nconpw
     do j = 1, nconpw
        bi(i, j) = a(j, i)
        b(i, j) = ai(j, i)
     end do
  end do
5602 continue
  do i = 1, nconpw
     do j = 1, nconpw
        aii = aimagz (ai(i, j))
        air = realz (ai(i, j))
        d18 = cabsz (ai(i, j))
        deg = atan2z (aii, air) * 180.0d0 / pai
        ze(i, j) = cmplxz (d18, deg)
        acomi = aimagz (a(i, j))
        acomr = realz (a(i, j))
        d19 = cabsz (a(i, j))
        deg = atan2z (acomi, acomr) * 180.0d0 / pai
        ys(i, j) = cmplxz (d19, deg)
     end do
  end do
  if (numaki .gt. 3) go to 5673
  write (unit = lunit(6), fmt = 2953)
2953 format (/, 10x, 'voltage transformation matrices in polar coordinates (with angles in degrees).', /, 15x, ' inverse of [tv] --- from phase to mode domain:'  )
  call print (ze, nconpw, ll1, ldn)
  write (unit = lunit(6), fmt = 4954)
4954 format (15x, ' [tv] --- from mode to phase domain:')
  call print (ys, nconpw, ll1, ldn)
5673 call mxm (bi, yc, ca, nconpw, ldn)
  call mxm (ca, a, ys, nconpw, ldn)
  do i = 1, nconpw
     do j = 1, nconpw
        if(i .ne. j) go to 79
        aa = aimagz (ys(i, i))
        bb = realz (ys(i, i))
        if (iprsup .ge. 1) write (unit = *, fmt = *) ' i, ys(i,i) =', i, ys(i, i)
        !!  $$$$ protection ?? $$$$
        !     if((absz(bb).le.1.0e-18).or.(absz(aa).le.1.0e-18))
        if ((absz (bb) .le. 1.0e-18) .and. (absz (aa) .le. 1.0e-18)) go to 9801
        go to 9802
9801    continue
        ys(i, j) = ys(i, j) * 1.0e10
        zs(i, i) = qn(i) ** 2 / ys(i, i) * 1.e-10
        !     zo(i,i) = (qn(i)/ys(i,i))*1.0e10
        write (unit = *, fmt = *) 'qn, ys  for i=', qn(i), ys(i, j), i
        ys(i, i) = ys(i, i) * 1.0e-10
        go to 9803
9802    continue
        !     zo(i,i) = qn(i)/ys(i,i)
        zs(i, i) = qn(i) ** 2 / ys(i, i)
9803    continue
        !!  $$$$ end of protection  $$$$
        !     yo(i,i) = 1./zo(i,i)
        !     zs(i,i) = qn(i) * zo(i,i)
        yo(i, i) = csqrtz (ys(i, i) / zs(i, i))
        zo(i, i) = 1.0d0 / yo(i, i)
        go to 80
79      zo(i, j) = czero
        yo(i, j) = czero
        zs(i, j) = cmplxz (fzero, fzero)
80   end do
     if (lastov .ne. 39) go to 1980
     if (iprint .lt. 2) go to 1978
     !     ysre = realz(ys(i,i))
     !     ysim = aimagz(ys(i,i))
     !     zsre = realz(zs(i,i))
     !     zsim = aimagz(zs(i,i))
     !!      write (lunit(9))  w, ysre,ysim,zsre,zsim
     w1 = w
     !   ** the unit here is /km now! **
     !     zz(i)=zsre *1000.
     !     zz(i+nconpw)=zsim*1000.
     !     ps(i)=ysre*1000.
     !     ps(i+nconpw)=ysim*1000.
     ycharm = cabsz (yo(i, i))
     ychara = atan2z (aimagz (yo(i, i)), realz (yo(i, i)))
     alpha = realz (qn(i)) * 1000.0d0
     beta = aimagz (qn(i)) * 1000.0d0
     if (lastov .ne. 39) go to 1980
     if (iprint .lt. 2) go to 1978
     write (unit = lunit(9)) w1, ycharm, ychara, alpha, beta
     do 78 k = 1, nconpw
        pp1(k, i) = cabsz (b(k, i))
        pp2(k, i) = atan2z (aimagz (b(k, i)), realz (b(k, i)))
78   end do
     write (unit = lunit(9)) (pp1(k, i), pp2(k, i), k = 1, nconpw)
     !    **********  thlthl
     !    **********  thlthl
1978 if (iprsup .ge. 0) write (unit = lunit(6), fmt = 1979) i, w, ycharm, ychara, alpha, beta
1979 format (' ychar and eigenvalue for mode', i3, ' at frequency =', e12.5, 2x, ' are ', 4e12.5)
1980 end do
  if (numaki .gt. 3) go to 9900
  write (unit = lunit(6), fmt = 956)
956 format (/, 10x, 'characteristic impedances in the phase domain')
  call mxm (a, zo, ca, nconpw, ldn)
  call mxm (ca, bi, ze, nconpw, ldn)
  call print (ze, nconpw, ll1, ldn)
  write (unit = lunit(6), fmt = 305)
305 format (//, 53x, ' table of modal quantities.')
  write (unit = lunit(6), fmt = 315)
315 format (//, 11x, 'modal', 4x, 'propagation', 7x, 'modal impedance', 12x, 'modal', 6x, 'charac. imp., natural mode', 4x, 'charac. adm., natural mode', /, ' mode', 3x, 'attenuation   velocity', 7x, 'real', 7x, 'imaginary', 4x, 'susceptance', 7x, 'real', 8x, 'imaginary', 9x, 'real', 8x, 'imaginary', /, 10x, '(dB/km)', 6x, '(m/s)', 12x, 'z(Ohm/m)', 14x, 'imy(mho/m)', 9x, 'sqrt(z/y) (Ohm)  ', 13x, 'sqrt(y/z) (mho)  ')
  do i=1, nconpw
     zsr = realz (zs(i, i))
     zsi = aimagz (zs(i, i))
     ysi = aimagz (ys(i, i))
     zor = realz (zo(i, i))
     zoi = aimagz (zo(i, i))
     yor = realz (yo(i, i))
     yoi = aimagz (yo(i, i))
     write (unit = lunit(6), fmt = 325) i, an(i), gn(i), zsr, zsi, ysi, zor, zoi, yor, yoi
325  format(/, 1x, i3, 3x, 2e12.5, 1x, 3e14.6, 2(1x, 2e14.6))
  end do
  write (unit = lunit(6), fmt = 335)
335 format (///, 1x)
  go to 9900
9200 continue
9900 if (iprs47 .ge. 1) write (unit = logsix, fmt = 2689)
2689 format (/, " Exit  'prcon' .")
  return
end subroutine prcon

!
! subroutine unwind.
!

subroutine unwind (ping, kthl, mrr, nrp, ntol, iseq)
  use blkcom
  use movcop
  implicit none
!!!!! %include  '//a/tsu/tpplotkom.ins.ftn'
  !
  integer(4), intent(out) :: iseq(15)
  integer(4), intent(in) :: kthl
  integer(4), intent(out) :: mrr
  integer(4), intent(in) :: nrp
  integer(4), intent(in) :: ntol
  real(8), intent(out) :: ping(200)
  integer(4) :: i, ib, ic, im, ip, iq, iseqa(15), iseqt(15), iold(15)
  integer(4) :: j, jb, jbeg
  integer(4) :: kk, kpt, ku, kuid(15), kunf, kuse(15)
  integer(4) :: l, li
  integer(4) :: mu
  integer(4) :: ny
  real(8) :: ai, aj
  real(8) :: bom(180)
  real(8) :: chf
  real(8) :: d1, d2
  real(8) :: sai, saj, sb(60), smi1, smi2, smj1, smj2
  real(8) :: tping(200), tt(4)
  real(8) :: vfreq(3)
  !
  !  dimension ping(200),tping(200)
  !  dimension ps(30),zz(30)
  !  dimension bom(180),vfreq(3), kuid(15), kuse(15)
  !  dimension iseq(15), iseqa(15), iseqt(15), iold(15)
  !  dimension sb(60), tt(4), tzz(30), tps(30)
  !
  data bom  / 180 * 0.0d0 /
  data kunf / 0 /
  !
  !      write(*,*) ' change order ? 1 (for yes) or 2 (for no)'
  !      read(*,*) ny
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' Beginning of unwind.   kthl, mrr, nrp, ntol =', kthl, mrr, nrp, ntol
  ny = 2
  !      if(ny .eq. 2) go to 3800
  !      do 3802 i=1,15
  !      iseqa(i)=i
  !      iseqt(i)=i
  ! 3802 continue
  !      write(*,*) ' freq ?'
  !      read(*,*) chf
  !      write(*,*) ' new order ?'
  !      read(*,3801) (iseqa(k), k=1,15)
  ! 3801 format( 15i3 )
  ! 3800 continue
  if ((ntol .gt. 1) .or. (nrp .eq. 0)) go to 1050
  do i = 1, 15
     iseq(i) = i
     iold(i) = i
  end do
  !  do i = 1, 180
  !     bom(i) = 0.0d0
  !  end do
  call move0 (bom, 180)
1050 continue
  if (iseq(1) .ne. 0) go to 10
  do i = 1, 15
     iseq(i) = i
  end do
  !!      write(*,*)
  !!     1 ' identical eigenvalues ? 1 (for  yes) or 2 (for no)'
  !!      read(*,*) lwh
  !!      if (lwh .eq. 2) go to 10
  !!      kunf=1
  !!      write(*,*) ' mode ?'
  !!      read(*,4801) (kuid(k), k=1,15)
  !! 4801 format( 15i3 )
10 continue
  do i = 1, 3
     vfreq(i) = 0.0d0
  end do
  !!    ****  read data from 'ping' vector  ****
  kpt = kthl - 1
  li = kpt / 4
  vfreq(3) = vfreq(2)
  vfreq(2) = vfreq(1)
  vfreq(1) = ping(1)
  do i = 1, kpt + 1
     tping(i) = ping(i)
  end do
  do l = 1, li
     do im = 1, 4
        ping(4 * l - im + 2) = tping(4 * iseq(l) - im + 2)
     end do
     iold(l) = iseq(l)
  end do
  do i = 1, kpt
     bom(i + 2 * kpt) = bom(i + kpt)
     bom(i + kpt) = bom(i)
     bom(i) = ping(i + 1)
  end do
  if (ntol .lt. 2) return
  !!     **** begin to smooth ****
  !!  ** initialization **
!!!!!      do 88 l=1, 15
!!!!!      iseq(l) = l
!!!!!   88 continue
  do i = 1, kpt
!!!!!      sb(i)=bom(i)
     sb(i) = tping(i + 1)
  end do
  ic = 0
  iq = 0
  if (iprsup .ge. 1)  write (unit = *, fmt = *) '  *****  li, kpt =',  li, kpt
99 if (ic .gt. li * (li - 1)) go to 12
  if (iq .eq. 1) go to 12
  iq = 1
  do l = 1, li
     do im = 1, 4
        bom(4 * l - im + 1) = sb(4 * iseq(l) - im + 1)
     end do
  end do
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' Beginning  bom(1:4*li) =', (bom(ip), ip = 1, 4 * li)
  !   * process the data between freq, freq-1 & freq-2 *
  do j = 2, kpt - 2, 4
     do i = j + 4, kpt - 2, 4
        !   * test the crossing of ang *
        d1 = bom(j + kpt) - bom(i + kpt)
        d2 = bom(j) - bom(i)
        if (d1 * d2 .le. 0.0d0) go to 801
        d1 = bom(j - 1 + kpt) - bom(i - 1 + kpt)
        d2 = bom(j - 1) - bom(i - 1)
        if ((d1 * d2) .le. 0.0d0) go to 801
        cycle
        !   * test the slope change of ang , if switching *
801     continue
        aj = 0.0d0
        ai = 0.0d0
        if ((bom(j + kpt) - bom(j + 2 * kpt)) .eq. 0.0d0) aj = 1.0e-12
        if ((bom(i + kpt) - bom(i + 2 * kpt)) .eq. 0.0d0) ai = 1.0e-12
        smj1 = (bom(j) - bom(j + kpt)) / ((bom(j + kpt) - bom(j + 2 * kpt)) + aj)
        smi1 = (bom(i) - bom(i + kpt)) / ((bom(i + kpt) - bom(i + 2 * kpt)) + ai)
        smj2 = (bom(i) - bom(j + kpt)) / ((bom(j + kpt) - bom(j + 2 * kpt)) + aj)
        smi2 = (bom(j) - bom(i + kpt)) / ((bom(i + kpt) - bom(i + 2 * kpt)) + ai)
        if (iprsup .ge. 1) write (unit = *, fmt = *) ' smj1, smj2, smi1, smi2 =', smj1, smj2, smi1, smi2
        if ((abs (smj1 - 1) .lt. abs (smj2 - 1)) .and. (abs (smi1 - 1) .lt. abs (smi2 - 1))) cycle
        !   * test the slope change of mag , if switching *
        !     789012345678901234567890123456789012345678901234567890123456789012
        aj = 0.0d0
        ai = 0.0d0
        if ((bom(j - 1 + kpt) - bom(j - 1 + 2 * kpt)) .eq. 0.0d0) aj = 1.0e-12
        if ((bom(i - 1 + kpt) - bom(i - 1 + 2 * kpt)) .eq. 0.0d0) ai = 1.0e-12
        smj1 = (bom(j - 1) - bom(j - 1 + kpt)) / ((bom(j - 1 + kpt) - bom(j - 1 + 2 * kpt)) + aj)
        smi1 = (bom(i - 1) - bom(i - 1 + kpt)) / ((bom(i - 1 + kpt) - bom(i - 1 + 2 * kpt)) + ai)
        smj2 = (bom(i - 1) - bom(j - 1 + kpt)) / ((bom(j - 1 + kpt) - bom(j - 1 + 2 * kpt)) + aj)
        smi2 = (bom(j - 1) - bom(i - 1 + kpt)) / ((bom(i - 1 + kpt) - bom(i - 1 + 2 * kpt)) + ai)
        if ((abs (smj1 - 1) .lt. abs (smj2 - 1)) .and. (abs (smi1 - 1) .lt. abs (smi2 - 1))) cycle
        !   * switching *
        iq = 0
        t = iseq((j + 2) / 4)
        iseq((j + 2) / 4) = iseq((i + 2) / 4)
        iseq((i + 2) / 4) = int (t, kind (iseq(1)))
        do im = 1, 4
           tt(im) = bom(j+im-2)
           bom(j + im - 2) = bom(i + im - 2)
           bom(i + im - 2) = tt(im)
        end do
     end do
  end do
  ic = ic + 1
  go to 99
12 continue
  !!  **  begin the second loop for vel and db  **
  !   * test the slope change of vel *
  ic = 0
  iq = 0
899 if (ic .gt. li * (li - 1)) go to 812
  if (iq .eq. 1 ) go to 812
  iq = 1
  do l = 1, li
     do im = 1, 4
        bom(4 * l - im + 1) = sb(4 * iseq(l) - im + 1)
     end do
  end do
  !   * test the slope change of vel *
  do jb = 1, li
     jbeg = jb * 4 - 1
     aj = 0.0d0
     if ((bom(jbeg + kpt) - bom(jbeg + 2 * kpt)) .eq. 0.0d0) aj = 1.0e-12
     saj = (bom(jbeg) - bom(jbeg + kpt)) / ((bom(jbeg + kpt) - bom(jbeg + 2 * kpt)) + aj)
     if (abs (saj - 1) .gt. 0.5d0) go to 8001
  end do
  go to 812
8001 do j = jbeg, kpt - 1, 4
     do i = j + 4, kpt - 1, 4
        !   * test the crossing of vel or db *
        jb = (j + 1) / 4
        ib = (i + 1) / 4
        d1 = bom(j + kpt) - bom(i + kpt)
        d2 = bom(j) - bom(i)
        if ((d1 * d2) .le. 0.0d0) go to 8801
        d1 = bom(j + 1 + kpt) - bom(i + 1 + kpt)
        d2 = bom(j + 1) - bom(i + 1)
        if ((d1 * d2) .le. 0.0d0) go to 8801
        cycle
        !      go to 8122
8801    continue
        aj = 0.0d0
        ai = 0.0d0
        if ((bom(j + kpt) - bom(j + 2 * kpt)) .eq. 0.0d0) aj = 1.0e-12
        if ((bom(i + kpt) - bom(i + 2 * kpt)) .eq. 0.0d0) ai = 1.0e-12
        smj1 = (bom(j) - bom(j + kpt)) / ((bom(j + kpt) - bom(j + 2 * kpt)) + aj)
        smi1 = (bom(i) - bom(i + kpt)) / ((bom(i + kpt) - bom(i + 2 * kpt)) + ai)
        smj2 = (bom(i) - bom(j + kpt)) / ((bom(j + kpt) - bom(j + 2 * kpt)) + aj)
        smi2 = (bom(j) - bom(i + kpt)) / ((bom(i + kpt) - bom(i + 2 * kpt)) + ai)
        if ((abs (smj1 - 1) .lt. abs (smj2 - 1)) .and. (abs (smi1 - 1) .lt. abs (smi2 - 1))) cycle
        !     1 (abs( smi1 -1 ) .lt. abs( smi2 -1 ))) go to 8132
        !   * test the slope change of db, if switch *
        aj=0.
        ai=0.
        if((bom(j+1+kpt)-bom(j+1+2*kpt)) .eq. 0. ) aj=1.0e-12
        if((bom(i+1+kpt)-bom(i+1+2*kpt)) .eq. 0. ) ai=1.0e-12
        saj= (bom(i+1)-bom(j+1+kpt))/((bom(j+1+kpt)-bom(j+1+2*kpt))+aj)
        sai= (bom(j+1)-bom(i+1+kpt))/((bom(i+1+kpt)-bom(i+1+2*kpt))+ai)
        if ( ( abs( saj-1 ) .gt. 1.0 ) .and. ( abs( sai-1 ) .gt. 1.0 )) cycle
        !     1 go to 8142
        !     789012345678901234567890123456789012345678901234567890123456789012
        !   * switching *
        iq = 0
        t = iseq(jb)
        iseq(jb) = iseq(ib)
        iseq(ib) = int (t)
        do im = 1, 4
           tt(im) = bom(j + im - 3)
           bom(j + im - 3) = bom(i + im - 3)
           bom(i + im - 3) = tt(im)
        end do
        ! 8112 write(*,*) vfreq(1), i,j, 'stop 1'
        !      go to 8102
        ! 8122 write(*,*) vfreq(1), i,j, 'stop 2'
        !      go to 8102
        ! 8132 write(*,*) vfreq(1), i,j, 'stop 3'
        !      go to 8102
        ! 8142 write(*,*) vfreq(1), i,j, 'stop 4'
        !      go to 8102
     end do
  end do
  ic = ic + 1
  go to 899
812 continue
  !   * check the equality between two eigenvalues *
  if (nrp .eq. 1) go to 2030
  ku = 0
  kunf = 0
  do j = 1, kpt - 3, 4
     mu = 0
     do i = j + 4, kpt - 3, 4
        if ((abs (bom(j) - bom(i)) .gt. abs (bom(j) * 1e-6)) .or. (abs (bom(j + 1) - bom(i + 1)) .gt. abs (bom(j + 1) * 1e-6))) cycle
        if ((abs (bom(j + kpt) - bom(i + kpt)) .gt. abs (bom(j + kpt) * 1e-6)) .or. (abs (bom(j + 1 + kpt) - bom(i + 1 + kpt)) .gt. abs (bom(j + 1 + kpt) * 1e-6))) cycle
!!!!      if((bom(j).ne.bom(i)).or.(bom(j+1).ne.bom(i+1))) go to 1005
!!!!      if((bom(j+kpt).ne.bom(i+kpt)).or.
!!!!     1 (bom(j+1+kpt).ne.bom(i+1+kpt))) go to 1005
        kunf = 1
        mu = 1
        kuid(ku + 1) = (i + 3) / 4
        ku = ku + 1
     end do
     if (mu .eq. 0) cycle
     kuid(ku + 1) = (j + 3) / 4
     ku = ku + 1
     if (iprsup .ge. 1) write (unit = *, fmt = *) ' identical eigenvalues.   j, ku, kuid(1:ku) =', j, ku, (kuid(ip), ip = 1, ku)
  end do
  if (kunf .eq. 0) go to 2000
  mrr = mrr + 1
  go to 2000
2030 continue
  do i = 1, ku
     kk = ku - i
     do j = 1, kk
        if (kuid(j) .lt. kuid(j + 1)) cycle
        t = kuid(j)
        kuid(j) = kuid(j + 1)
        kuid(j + 1) = int (t, kind (kuid(1)))
     end do
     kuse(i) = i
  end do
  write (unit = *, fmt = *) ' kuid after sorting : ',(kuid(i), i = 1, ku)
  do i = 1, ku
     kk = ku - i
     do j = 1, kk
        if (iold(kuid(j)) .lt. iold(kuid(j + 1))) cycle
        t = iold(kuid(j))
        iold(kuid(j)) = iold(kuid(j + 1))
        iold(kuid(j + 1)) = int (t, kind (iold(1)))
        t = kuse(j)
        kuse(j) = kuse(j + 1)
        kuse(j + 1) = int (t, kind (kuse(1)))
     end do
  end do
  do i = 1, ku
     kk = ku - i
     do j = 1, kk
        if (iseq(kuid(j)) .lt. iseq(kuid(j + 1))) cycle
        t = iseq(kuid(j))
        iseq(kuid(j)) = iseq(kuid(j + 1))
        iseq(kuid(j + 1)) = int (t)
     end do
  end do
  do l = 1, li
     iseqt(l) = iseq(l)
  end do
  do i = 1, ku
     iseq(kuid(kuse(i))) = iseqt(kuid(i))
  end do
  !   * re-ordering, if necessary *
2000 if (ny .eq. 2) go to 2004
  if (vfreq(1) .ne. chf) go to 2004
  do l = 1, li
     iseqt(l) = iseq(l)
  end do
  do l = 1, li
     iseq(l) = iseqt(iseqa(l))
  end do
  continue
2004 continue
  do l = 1, li
     do im = 1, 4
        bom(4 * l - im + 1) = sb(4 * iseq(l) - im + 1)
     end do
  end do
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' end of unwind.  vfreq(1) =',  vfreq(1)
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' end of unwind.   iseq(1:li) =', (iseq(l),l=1,li)
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' end of unwind.   kthl, mrr, nrp, ntol =', kthl, mrr, nrp, ntol
  do i = 1, kpt
     ping(i + 1) = bom(i)
  end do
  !!    **  reorder the modal quantities  **
!!!!      do 1009 i=1,2*li
!!!!      tzz(i)=zz(i)
!!!!      tps(i)=ps(i)
!!!! 1009 continue
!!!!      do 1010 i=1,li
!!!!      zz(i)=tzz(iseq(i))
!!!!      zz(i+li)=tzz(iseq(i)+li)
!!!!      ps(i)=tps(iseq(i))
!!!!      ps(i+li)=tps(iseq(i)+li)
!!!c 1010 continue
!!!      write(*,*) 'done with unwind '
  return
end subroutine unwind

!
! subroutine zymx.
!

subroutine zymx (w, nw, isyst, ngrnd, ngg, ncpp, radi, zy, yz, dir, dij, ang, usi, usr, esi, dr0, th0, al0, hi, di, bio, bi1, bi2, bi3, bi4, bi5, al1i, al2i, al3i, dci, nx, yzn, ys, yc, zp, zpc, zs, ze, zc, ca, cb, cc, cd, f, ldm, ldn, ldn2, lnq2)
  use blkcom
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: isyst
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ldn2
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: lnq2
  integer(4), intent(in) :: ngg(ldn)
  integer(4), intent(in) :: ncpp(ldm)
  integer(4), intent(in) :: ngrnd
  integer(4), intent(out) :: nx
  integer(4), intent(in) :: nw
  real(8), intent(in) :: al0(ldm, ldm)
  real(8), intent(in) :: al1i(ldm)
  real(8), intent(in) :: al2i(ldm)
  real(8), intent(in) :: al3i(ldm)
  real(8), intent(in) :: ang(ldm, ldm)
  real(8), intent(in) :: bio(ldm)
  real(8), intent(in) :: bi1(ldm)
  real(8), intent(in) :: bi2(ldm)
  real(8), intent(in) :: bi3(ldm)
  real(8), intent(in) :: bi4(ldm)
  real(8), intent(in) :: bi5(ldm)
  real(8), intent(in) :: dci(ldm)
  real(8), intent(in) :: di(ldn)
  real(8), intent(in) :: dij(ldm, ldm)
  real(8), intent(in) :: dir(ldm, ldm)
  real(8), intent(in) :: dr0(ldm, ldm)
  real(8), intent(in) :: esi(ldm, 3)
  real(8), intent(in) :: hi(ldm)
  real(8), intent(in) :: radi(ldm, 7)
  real(8), intent(in) :: th0(ldm, ldm)
  real(8), intent(in) :: usi(ldm, 3)
  real(8), intent(in) :: usr(ldm, 3)
  real(8), intent(in) :: w
  real(8), intent(in) :: yz(ldn, ldn)
  real(8), intent(out) :: yzn(lnq2)
  real(8), intent(in) :: zy(ldn, ldn)
  integer(4) :: i
  integer(4) :: j
  integer(4) :: k
  integer(4) :: l, ll0, ll1, ll2
  integer(4) :: mdx, mdy
  integer(4) :: n1, n2
  real(8) :: c1
  real(8) :: d1, d2, d3, d4
  real(8) :: freq
  complex(16) :: zpc(ldn, ldn), cjw
  complex(16) :: ys(ldn, ldn), yc(ldn, ldn), zp(ldn, ldn)
  complex(16) :: zs(ldn, ldn), ze(ldn, ldn), zc(ldn, ldn)
  complex(16) :: ca(ldn, ldn), cb(ldn, ldn), cc(ldn, ldn)
  complex(16) :: cd(ldn, ldn), f(ldn, ldn2)
  !
  ll0=0
  ll1=1
  nx=ngrnd
  do i = 1, ldn
     do j = 1, ldn
        ys(i, j) = czero
        yc(i, j) = czero
        zp(i, j) = czero
        zpc(i, j) = czero
        zs(i, j) = czero
        zc(i, j) = czero
        ze(i, j) = czero
     end do
  end do
  cjw = cmplxz (fzero, w)
  ll2 = 2
  if ( iprs47  .ge.  1 ) write (logsix, 2706)  nw, isyst, np2, itypec, numaki, npc, ncc, izflag, iyflag, w
2706 format ( /,  16h begin  'zymx' .,  24h      nw   isyst     np2, 48h  itypec  numaki     npc     ncc  izflag  iyflag, 19x, 1hw, /, 16x, 9i8, e20.11 )
  go to  (710, 720, 30), itypec
710 call  olzy ( w,ncpp,zy,dij,ang,usi,usr,esi,hi,di,zs,ze,zc,ldm, ldn )
  if (kill .ge. 1)   go to 9200
  go to 800
720 call  sczy1 ( w, isyst, zy, dir, dij, ang, hi, di, zs, ze,ldm, ldn )
  if (kill .ge. 1)   go to 9200
  call  sczy2 ( cjw, ncpp, radi, usi, usr, bio, bi1, bi2, bi3, bi4, bi5, al1i, al2i, al3i, zc, ldm, ldn )
  if (kill .ge. 1)   go to 9200
  go to 800
30 if ( npp  .ne.  0 ) call  sczy1 ( w, isyst, zy, dir, dij, ang, hi, di, zs, ze, ldm, ldn )
  if (kill .ge. 1)   go to 9200
  call  sczy2 ( cjw, ncpp, radi, usi, usr, bio, bi1, bi2, bi3, bi4, bi5, al1i, al2i, al3i, zc, ldm, ldn )
  if (kill .ge. 1)   go to 9200
  call ptzy2(cjw,ncpp,dci,dr0,th0,al0,zp,zpc,ldm,ldn)
  if (kill .ge. 1)   go to 9200
800 if (numaki .gt. 3)   go to 840
  if (iprs47 .lt. 1)   go to 840
943 format(  /,10x,18hearth impedance   )
  write(lunit(6),944)
944 format (   / ,  10x,  31hconductor internal impedance      )
  call print(zc,nw,ll2,ldn)
  if ( itypec  .ne.  3 )   go to 105
  write(lunit(6),961)
961 format( /, 10x,14hpipe impedance   )
  call print ( zp, nw, ll2, ldn )
  if ( npp  .eq.  0 )   go to 840
  write(lunit(6),962)
962 format( /, 10x,20hconnection impedance   )
  call print (zpc, nw, ll2, ldn)
105 write (lunit(6), 943)
  call print(ze,nw,ll2,ldn)
  if ( isyst  .eq.  -1 )   go to 840
  write(lunit(6), 946)
946 format (  /, 10x, 18hspace impedance   )
  call print(zs,nw,ll2,ldn)
840 do i=1, nw
     do j=1, nw
        c1 = e2p * yz(i,j)
        yc(i,j) = cjw * cmplxz(c1, fzero)
        zc(i, j) = zc(i, j) + ze(i, j) + zs(i, j) + zp(i, j) + zpc(i, j)
     end do
  end do
  if(itypec .eq. 1 .and. nenerg .ne. 0)  go to 335
  i = nx
1945 i = i - 1
  if ( ngg(i)  .lt.  1 )  go to 1966
  mdx = i + 1
  do j = 1, nw
     yzn(j) = realz (zc(i, j))
  end do
  do j = mdx, nx
     mdy = j - 1
     do k = 1, nw
        yc(mdy, k) = yc(j, k)
        zc(mdy, k) = zc(j, k)
     end do
  end do
  do j = 1, nw
     zc(nx, j) = cmplxz(yzn(j),fzero)
  end do
  do j = 1, nw
     yzn(j) = realz(zc(j, i))
  end do
  do j = mdx, nx
     mdy = j - 1
     do k = 1, nw
        yc(k, mdy) = yc(k, j)
        zc(k, mdy) = zc(k, j)
     end do
  end do
  do j = 1, nw
     zc(j, nx) = cmplxz (yzn(j), fzero)
  end do
  nx = nx - 1
1966 if ( i  .gt.  1 )  go to 1945
  if (itypec .ne. 1)   go to 300
  !  if (nw-nx) 890, 340, 310
  if (nw - nx .lt. 0) then
     go to 890
  else if (nw - nx .eq. 0) then
     go to 340
  else
     go to 310
  end if
300 if(nx .eq. nw)  go to 350
310 call minvn (zc, nw, nx, ll1, ldn, ca, cb, cc, cd, ze, zp, zpc, f, ldn2)
  if(itypec .ne. 1) go to 350
  if (kill .ge. 1)   go to 9200
  go to 340
335 nx = nw
340 if (isyst .eq. 2) call transp(zc, ncpp, yzn(1), yzn(ldn2), ca, ldm, ldn)
  if (kill .ge. 1)   go to 9200
  if (ialter .le. 0)  go to 90
  if (numaki .lt. 9 )  go to 90
350 if(ncros.eq.0 .or. npais.lt.0) go to 890
  nx=4
  call crosa4 (zc, ll0, ldn, ca, cb, cc, cd, ze, zp, zpc, f, ldn2)
  call crosa4 (yc, ll1, ldn, ca, cb, cc, cd, ze, zp, zpc, f, ldn2)
890 n1 = nx
  n2 = 2 * n1 * n1
  k = 1
  do j = 1, n1
     do i = 1, n1
        yzn(k) = realz(yc(i,j))
        l = k + 1
        yzn(l) = aimagz(yc(i,j))
        k = l + 1
     end do
  end do
  write (lunit(3)) (yzn(i), i=1, n2)
  if (iprsup .lt. 1) go to 1983
  freq = w / twopi
  write (lunit(6), 1900) freq, (yzn(i),i=1,n2)
1900 format ('  Y written on lunit3 at freq ', e16.7,2x, 'are', /, (1x, 8e15.6))
1983 k = 1
  do j = 1, n1
     do i = 1, n1
        yzn(k) = realz(zc(i,j))
        l = k + 1
        yzn(l) = aimagz(zc(i,j))
        k = l + 1
     end do
  end do
  write (unit = lunit(3)) (yzn(i), i=1, n2)
  if (iprsup .lt. 1) go to 90
  freq = w / twopi
  write (unit = lunit(6), fmt = 2010) freq, (yzn(i),i=1,n2)
2010 format ('  Z written on lunit3 at freq ', e16.7, 2x, 'are', /, (1x, 8e15.6))
90 if (numaki .gt. 3)   go to 900
  if (izflag .eq. 0)   go to 100
  write (unit = lunit(6), fmt = 94)
94 format (/, 10x, 'impedance matrix')
  call print (zc, nx, ll2, ldn)
  if (izflag .eq. 1) go to 150
100 continue
  write (unit = lunit(6), fmt = 125)
125 format (/, 10x, 'resistance and inductance')
  !         begin code associated with [z] dump onto unit-34 plot file
!!!!      k = 1
!!!!      sing(k) = alog1z ( w / twopi )
  do i = 1, nx
     do j = 1, nx
        d1 = realz (zc(i, j))
        d2 = aimagz (zc(i, j)) / w
!!!      sing(k+1) = d1            ! store resistance in real*4 vector
!!!!      sing(k+2) = d2            ! store inductance in real*4 vector
!!!!      k = k + 2          !  advance index to last-stored number
        zc(i, j) = cmplxz (d1, d2)
     end do
  end do
!!!!      write (junit4) ( sing(j), j=1, k ) ! output vector written to
!!!!      write (*, 4488)  sing(1), sing(2), sing(3), sing(k-1), sing(k)
!!!! 4488 format ( ' sing(1), sing(2), sing(3), sing(k-1), sing(k) =',
!!!!     1         f10.4, 4e13.4 )
  call print(zc, nx, ll2, ldn)
  !     now restore  'zc'  to impedance in  do 4908  loop below.
  do i = 1, nx
     do j = 1, nx
        d1 = realz (zc(i, j))
        d2 = aimagz (zc(i, j)) * w
        zc(i, j) = cmplxz (d1, d2)
     end do
  end do
150 if (iyflag .eq. 0) go to 160
  write (unit = lunit(6), fmt = 155)
155 format (/, 10x, 'admittance matrix')
  call print (yc, nx, ll2, ldn)
  if (iyflag .eq. 1) go to 900
160 write (unit = lunit(6), fmt = 165)
165 format (/, 10x, 'conductance and capacitance')
  do i = 1, nx
     do j = 1, nx
        d3 = realz (yc(i, j))
        d4 = aimagz (yc(i, j)) / w
        yc(i, j) = cmplxz (d3, d4)
     end do
  end do
  call print (yc, nx, ll2, ldn)
  !     now restore  'yc'  to admittance in  do 4928  loop below.
  do i = 1, nx
     do j = 1, nx
        d1 = realz(yc(i,j))
        d2 = aimagz (yc(i, j)) * w
        yc(i, j) = cmplxz (d1, d2)
     end do
  end do
9200 continue
900 if (iprs47 .ge. 1) write (unit = logsix, fmt = 2794)
2794 format (/, " Exit  'zymx' .")
  return
end subroutine zymx

!
! subroutine ymatr.
!

subroutine ymatrx (isyst, lunit6, ncpp, zy, yz, esi, al0, al1i, al2i, al3i, a1, a2, ldm, ldn)
  use com47
  implicit none
  integer(4), intent(in) :: isyst
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: lunit6
  integer(4), intent(in) :: ncpp(ldm)
  real(8), intent(out) :: a1(ldn, ldn)
  real(8), intent(out) :: a2(ldn, ldn)
  real(8), intent(in) :: al0(ldm, ldm)
  real(8), intent(in) :: al1i(ldm)
  real(8), intent(in) :: al2i(ldm)
  real(8), intent(in) :: al3i(ldm)
  real(8), intent(in) :: esi(ldm, 3)
  real(8), intent(out) :: yz(ldn, ldn)
  real(8), intent(out) :: zy(ldn, ldn)
  integer(4) :: i, i1, i2
  integer(4) :: j, j1, j2
  real(8) :: ypo1
  !
  !  dimension  a1(ldn,ldn), a2(ldn,ldn), ncpp(ldm)
  !  dimension  zy(ldn, ldn), yz(ldn, ldn), al0(ldm, ldm)
  !  dimension  esi(ldm, 3), al1i(ldm), al2i(ldm), al3i(ldm)
  !
  do i = 1, ldn
     do j = 1, ldn
        a1(i, j) = 0.0d0
        a2(i, j) = 0.0d0
        yz(i, j) = 0.0d0
     end do
  end do
  if (iprs47 .ge. 1) write (unit = lunit6, fmt = 1719) isyst, np2, itypec, npc, npp
1719 format (/, " At beginning of  'ymatrx' .   isyst     np2  itypec     npc     npp", /, 28x,  5i8)
  if (iprs47 .ge. 2) write (unit = lunit6, fmt = 1724) (i, ncpp(i), al1i(i), al2i(i), al3i(i), esi(i,1), esi(i,2), esi(i,3), i = 1, 10)
1724 format (/, 5x, 'row', 4x, 'ncpp', 11x, 'al1i', 11x, 'al2i', 11x, 'al3i', 7x, 'esi(i,1)', 7x,  'esi(i,2)', 7x, 'esi(i,3)', /, (1x, i7, i8, 6e15.6))
  if (itypec .eq. 3) go to 25
  if (isyst .eq. -1) go to 25
  do i = 1, npc
     do j = 1, npc
        if(j .lt. i) cycle
        yz(i, j) = zy(i, j)
        yz(j, i) = zy(i, j)
     end do
  end do
  if (itypec .eq. 1) go to 200
  do i = 1, ncc
     i1 = i
     if (i .gt. npc) i1 = i - npc
     if (i .gt. npc2) i1 = i - npc2
     do j = 1, ncc
        if (j .lt. i) cycle
        j1 = j
        if (j .gt. npc) j1 = j - npc
        if(j .gt. npc2) j1 = j - npc2
        yz(i, j) = zy(i1, j1)
        yz(j, i) = yz(i, j)
     end do
  end do
25 do i = 1, npc
     i1 = i + npc
     i2 = i + npc2
     if (i2 .gt. ncc) go to 35
     a1(i2, i2) = al3i(i) / esi(i, 3)
     a1(i2, i) = a1(i2, i2)
     a1(i, i2) = a1(i2, i2)
     a1(i1, i2) = a1(i2, i2)
     a1(i2, i1) = a1(i2, i2)
35   if (ncpp(i) .eq. 1) i1 = i
     if (i1 .gt. npc2) go to 39
     a1(i1, i1) = al2i(i) / esi(i, 2) + a1(i2, i2)
     a1(i1, i) = a1(i1, i1)
     a1(i, i1) = a1(i1, i1)
39   a1(i, i) = a1(i1, i1) + al1i(i) / esi(i, 1)
  end do
  if (itypec .eq. 3) go to 60
  do i = 1, ncc
     do j = 1, ncc
        if (j .lt. i) cycle
        yz(i, j) = yz(i, j) + a1(i, j)
        yz(j, i) = yz(i, j)
     end do
  end do
  go to 200
60 ypo1 = 0.0d0
  if (npp .ne. 0) ypo1 = alpi / es2
  if (npp .eq. 0) zy(1, 1) = 0.0d0
  if (isyst .ne. -1) ypo1 = ypo1 + zy(1,1)
  do i = 1, npc
     i1 = i + npc
     i2 = i + npc2
     do j = 1, npc
        j1 = j + npc
        j2 = j + npc2
        a2(i, j) = al0(i, j) / es1
        if (ncpp(i) .eq. 1) go to 65
        a2(i1, j) = a2(i, j)
        a2(i1, j1) = a2(i, j)
65      if (ncpp(j) .eq. 1) cycle
        a2(i, j1) = a2(i, j)
        if (ncpp(i) .eq. 2) go to 67
        a2(i2, j) = a2(i, j)
        a2(i2, j1) = a2(i, j)
        a2(i2, j2) = a2(i, j)
67      if (ncpp(j) .eq. 2) cycle
        a2(i, j2) = a2(i, j)
        a2(i1, j2) = a2(i, j)
     end do
  end do
  do i = 1, ncc
     if(npp .eq. 0) go to 85
     a1(i, ncc) = 0.0d0
     a1(ncc, i) = 0.0d0
     a2(i, ncc) = 0.0d0
     a2(ncc, i) = 0.0d0
85   do j = 1, ncc
        if (j .lt. i) cycle
        yz(i, j) = a1(i, j) + a2(i, j) + ypo1
        yz(j, i) = yz(i, j)
     end do
  end do
200 if (iprs47 .ge. 1) write (unit = logsix, fmt = 2876) ncc, ypo1, es1, alpi, yz(1, 1), yz(1, 2)
2876 format (/, " Exit  'ymatrx' .     ncc", 16x, 'ypo1', 17x, 'es1', 16x, 'alpi', 13x, 'yz(1,1)', 13x, 'yz(1,2)', /, 17x, i8, 5e20.11)
  return
end subroutine ymatrx

!
! subroutine simp.
!

subroutine simp (nw, h, dd, rad, zy, dir, dij, ang, ldm, ldn)
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: nw
  real(8), intent(out) :: ang(ldm, ldm)
  real(8), intent(in) :: dd(ldn)
  real(8), intent(out) :: dij(ldm, ldm)
  real(8), intent(out) :: dir(ldm, ldm)
  real(8), intent(in) :: h(ldm)
  real(8), intent(in) :: rad(ldn)
  real(8), intent(out) :: zy(ldn, ldn)
  !  dimension  rad(ldn), h(ldm), dd(ldn),  zy(ldn, ldn)
  !  dimension  dir(ldm, ldm), dij(ldm, ldm), ang(ldm, ldm)
  integer(4) :: i
  integer(4) :: j
  real(8) :: r, r1, r2
  real(8) :: v
  !
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 2924) nw, itypec, h(1), dd(1), dd(2), rad(1), radp(3)
2924 format (/, " Enter  'simp' .      nw  itypec", 16x, 'h(1)', 15x, 'dd(1)', 15x, 'dd(2)', 14x, 'rad(1)', 13x, 'radp(3)', /, 16x, 2i8, 5e20.11)
  do i = 1, nw
     do j = 1, nw
        dij(i, j) = 0.0d0
        dir(i, j) = 0.0d0
        ang(i, j) = 0.0d0
        zy(i, j) = 0.0d0
     end do
  end do
  do i = 1, nw
     r = rad(i)
     if (itypec .eq. 3) r = radp(3)
     do j = 1, nw
        if (i .ge. j) go to 15
        v = dd(i) - dd(j)
        r1 = h(i) - h(j)
        r2 = absz (h(i) + h(j))
        ang(i, j) = atanz (absz (v) / r2)
        ang(j, i) = ang(i, j)
        v = v * v
        r1 = r1 * r1
        r2 = r2 * r2
        dij(i, j) = sqrtz (v + r2)
        dij(j, i) = dij(i, j)
        dir(i, j) = sqrtz (v + r1)
        dir(j, i) = dir(i, j)
        go to 20
15      if (i .gt. j) cycle
        ang(i, j) = 0.0d0
        dij(i, j) = 2.0d0 * absz (h(i))
        dir(i, j) = r
20      zy(i, j) = alogz (dij(i, j) / dir(i, j))
        zy(j, i) = zy(i, j)
     end do
  end do
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 2936) dir(1, 2), dij(1, 2), v, zy(1, 1), zy(1, 2)
2936 format (/, " Exit  'simp' .", 12x, 'dir(1,2)', 12x, 'dij(1,2)', 19x, 'v', 13x, 'zy(1,1)', 13x, 'zy(1,2)', /, 15x, 5e20.11)
  if (iprs47 .ge. 5) write (unit = logsix, fmt = 2948) ((zy(i, j), j = 1, nw), i = 1, nw)
2948 format (/, " Exit  'simp' .   ((zy(i,j), j=1, nw), i=1, nw)", /, (1x, 6e20.11))
  return
end subroutine simp

!
! subroutine sczy1.
!

subroutine  sczy1 (w, isyst, zy, dir, dij, ang, hi, di, zs, ze, ldm, ldn)
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: isyst
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: ldn
  real(8), intent(in) :: ang(ldm, ldm)
  real(8), intent(in) :: di(ldm)
  real(8), intent(in) :: dij(ldm, ldm)
  real(8), intent(in) :: dir(ldm, ldm)
  real(8), intent(in) :: hi(ldm)
  real(8), intent(in) :: w
  real(8), intent(in) :: zy(ldn, ldn)
  complex(16), intent(out) :: ze(ldn, ldn)
  complex(16), intent(out) :: zs(ldn, ldn)
  !  dimension  zy(ldn, ldn), dir(ldm, ldm), dij(ldm, ldm)
  !  dimension  ang(ldm, ldm), hi(ldm), di(ldn)
  integer(4) :: i, i1
  integer(4) :: j, j1
  integer(4) :: ll0
  real(8) :: be1, be2
  real(8) :: c1, c2
  real(8) :: d12
  real(8) :: th
  complex(16) :: cjw, xe
  !
  cjw = cmplxz (fzero, w)
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3027) isyst, npc, iearth, itypec, roe, u0, w
3027 format (/, " Enter  'sczy1' .   isyst     npc  iearth  itypec", 17x, 'roe', 18x, 'u0', 19x, 'w', /, 17x, 4i8, 3e20.11)
  c2 = u2p * zy(1, 1)
  if (itypec .eq. 3) go to 50
  if (isyst.eq.-1) go to 35
  do i = 1, npc
     do j = 1,npc
        c1 = u2p * zy(i, j)
        zs(i, j) = cjw * cmplxz (c1, fzero)
     end do
  end do
35 do i = 1, npc
     do j = 1, npc
        if (j .lt. i) cycle
        if (iearth .eq. 99) go to 37
        be1 = dir(i, j) * sqrtz (u0 / roe)
        if (isyst .ne. -1) be1 = 0.0d0
        be2 = dij(i, j) * sqrtz (u0 / roe)
        th = ang(i, j)
        call zegen (be1, be2, th, w, xe, isyst)
        go to 38
37      d12 = absz (di(j) - di(i))
        call zest (hi(i), hi(j), d12, roe, w, xe)
38      ze(i, j) = xe
        ze(j, i) = xe
     end do
  end do
  if (iprs47 .ge. 2) write (unit = logsix, fmt = 3039)
3039 format (/, " Diagnostic within  'sczy1' ,   ze(i,j)  for(i,j)=1, ... npc .")
  ll0 = 0
  if (iprs47 .ge. 4) call print (ze(1, 1), npc, ll0, ldn)
  if (npc .eq. ncc) go to 90
  if (isyst .eq. -1) go to  45
  do i = 1, ncc
     do j = 1, ncc
        if (j .lt. i) cycle
        i1 = i
        j1 = j
        if (i1 .gt. npc2) i1 = i - npc2
        if (i1 .gt. npc) i1 = i - npc
        if(j1 .gt. npc2) j1 = j - npc2
        if (j1 .gt. npc) j1 = j - npc
        zs(i, j) = zs(i1, j1)
        zs(j, i) = zs(i, j)
     end do
  end do
45 do i = 1, ncc
     do j = 1, ncc
        if (j .lt. i) cycle
        i1 = i
        j1 = j
        if (i1 .gt. npc2) i1 = i - npc2
        if (i1 .gt. npc) i1 = i - npc
        if (j1 .gt. npc2) j1 = j - npc2
        if (j1 .gt. npc) j1 = j - npc
        ze(i, j) = ze(i1, j1)
        ze(j, i) = ze(i, j)
     end do
  end do
  go to 90
50 if (isyst .eq. -1) go to 60
  if (npp .eq. 0) go to 90
  do i = 1, ncc
     do j = 1, ncc
        if (j .lt. i) cycle
        zs(i, j) = cjw * cmplxz (c2, fzero)
        zs(j, i) = zs(i, j)
     end do
  end do
60 be1 = dir(1, 1) * sqrtz (u0 / roe)
  if (isyst .ne. -1) be1 = 0.0d0
  be2 = dij(1, 1) * sqrtz (u0 / roe)
  th = ang(1, 1)
  call zegen (be1, be2, th, w, xe, isyst)
  do i = 1, ncc
     do j = 1, ncc
        ze(i, j) = xe
     end do
  end do
90 if (iprs47 .ge. 2) write (unit = logsix, fmt = 3056) xe
3056 format (/, " Diagnostic at exit  'sczy1' .   zs(i,j)  for  (i,j)=1, ... npc .    real-xe =", e20.11, 4x, 'imag-xe =', e20.11)
  if (iprs47 .ge. 4) call print (zs(1, 1), npc, ll0, ldn)
  return
end subroutine sczy1

!
! subroutine sczy2.
!

subroutine sczy2 (s, ncpp, radi, usi, usr, bio, bi1,bi2, bi3, bi4, bi5, al1i, al2i, al3i, zc, ldm, ldn)
  use komthl
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ncpp(ldm)
  complex(16), intent(in) :: s
  complex(16), intent(out) :: zc(ldn, ldn)
  real(8), intent(in) :: al1i(ldm)
  real(8), intent(in) :: al2i(ldm)
  real(8), intent(in) :: al3i(ldm)
  real(8), intent(in) :: bio(ldm)
  real(8), intent(in) :: bi1(ldm)
  real(8), intent(in) :: bi2(ldm)
  real(8), intent(in) :: bi3(ldm)
  real(8), intent(in) :: bi4(ldm)
  real(8), intent(in) :: bi5(ldm)
  real(8), intent(in) :: radi(ldm, 7)
  real(8), intent(in) :: usi(ldm, 3)
  real(8), intent(in) :: usr(ldm, 3)
  integer(4) :: i, i1, i2, ido, inm, ixa
  integer(4) :: ll1, ll2
  real(8) :: al1, al2, al3
  real(8) :: b0, b1, b2, b3, b4, b5
  real(8) :: d1, d2, d3
  real(8) :: xa
  complex(16) :: ss, s1, s2, s3, s4, s5, s6, s8, su0
  complex(16) :: z11, z12, z2i, z2m, z2o, z23
  complex(16) :: s0, s7, z3i, z3m, z3o, z34
  complex(16) :: c1, c2,  c3
  !  dimension ncpp(ldm), radi(ldm, 7), usi(ldm, 3), usr(ldm, 3)
  !  dimension bio(ldm), bi1(ldm), bi2(ldm), bi3(ldm), bi4(ldm)
  !  dimension bi5(ldm), al1i(ldm), al2i(ldm), al3i(ldm)
  !
  ll1 = 1
  ll2 = 2
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3107) npc, s
3107 format (/, " Enter  'sczy2' .     npc", 14x, 'real-s', 14x, 'imag-s', /, 17x, i8, 2e20.11)
  inm = 0
  do i = 1, npc
     b0 = bio(i)
     b1 = bi1(i)
     b2 = bi2(i)
     b3 = bi3(i)
     b4 = bi4(i)
     b5 = bi5(i)
     al1 = al1i(i)
     al2 = al2i(i)
     al3 = al3i(i)
     ss = csqrtz (s)
     c1 = cmplxz (b0, fzero)
     s0 = c1 * ss
     c1 = cmplxz (b1, fzero)
     s1 = c1 * ss
     c1 = cmplxz (b2, fzero)
     s2 = c1 * ss
     c1 = cmplxz (b3, fzero)
     s3 = c1 * ss
     c1 = cmplxz (u2p, fzero)
     su0 = s * c1
     if (iprs47 .ge. 2) write (unit = logsix, fmt = 3124) i, b0, su0, s0
3124 format (/, " Re-loop over  'i' .       i", 18x, 'b0', 12x, 'real-su0', 12x, 'imag-su0', 13x, 'real-s0', 13x, 'imag-s0', /, 19x, i8, 5e20.11)
     c1 = cmplxz (usr(i, 1), fzero)
     ixa = 0
     xa = cabsz (s0)
     if (radi(i, 1) .le. 0.0d0) xa =cabsz (s1)
     if (xa .gt. 10.0d0) ixa = 1
     call bsikm (s1, ll2, bin, bkn, ll1, ixa)
     if (radi(i, 1) .gt. 0.0d0) go to 15
     z11 = su0 / s1 * bin(1) / bin(2) * c1
     go to 25
15   s4 = bin(1)
     s5 = bin(2)
     s6 = bkn(1)
     z12 = bkn(2)
     call bsikm (s0, ll2, bin, bkn, ll1, ixa)
     if (ixa .gt. 0) go to 20
     ss = s5 * bkn(2) - z12 * bin(2)
     z11 = su0 / s1 * (bkn(2) * s4 + s6 * bin(2)) / ss * c1
     go to 25
20   ss = s1 - s0
     if (cabsz (ss) .gt. pekexp) go to 23
     ss = cexpz (ss)
     s7 = s5 * bkn(2) * ss - z12 * bin(2) / ss
     z11 = su0 / s1 * (bkn(2) * s4 * ss + s6 * bin(2) / ss) / s7 * c1
     go to 25
23   z11 = su0 / s1 * s4 / s5 * c1
25   d1 = usi(i, 1) * al1
     c1 = cmplxz (d1, fzero)
     z12 = su0 * c1
     z2i = czero
     z2m = czero
     z2o = czero
     d1 = usi(i, 2) * al2
     c1 = cmplxz (d1, fzero)
     z23 = su0 * c1
     if (iprs47 .ge. 3) write (unit = logsix, fmt = 3136) ixa, ncpp(i), radi(i,1), z11, z12
3136 format (/, 1x, '     ixa ncpp(i)', 11x, 'radi(i,1)', 12x, 'real-z11', 12x, 'imag-z11', 12x,  'real-z12', 12x, 'imag-z12', /, 1x, 2i8, 5e20.11)
     z3i = czero
     z3m = czero
     z3o = czero
     d1 = usi(i, 3) * al3
     c1 = cmplxz (d1, fzero)
     z34 = su0 * c1
     if (ncpp(i) .eq. 1) go to 90
     ido = 2
28   ixa = 0
     xa = cabsz (s2)
     if (xa .gt. 10) ixa = 1
     call bsikm (s2, ll2, bin, bkn, ll1, ixa)
     s4 = bin(1)
     s5 = bin(2)
     s1 = bkn(1)
     s0 = bkn(2)
     call bsikm (s3, ll2, bin, bkn, ll1, ixa)
     c1 = cmplxz (usr(i, ido), fzero)
     if (ixa .gt. 0)  go to 35
     ss = bin(2) * s0 - s5 * bkn(2)
     s7 = su0 / s2 * (s4 * bkn(2) + s1 * bin(2)) / ss * c1
     s8 = su0 / s3 * (bin(1) * s0 + bkn(1) * s5) / ss * c1
     go to 40
35   s6 = s3 - s2
     if (cabsz (s6) .gt. pekexp) go to 42
     s6 = cexpz (s6)
     ss = bin(2) * s0 * s6 - s5 * bkn(2) / s6
     s7 = su0 / s2 * (s4 * bkn(2) / s6 + s1 * bin(2) * s6) / ss * c1
     s8 = su0 / s3 * (bin(1) * s0 * s6 + bkn(1) * s5 / s6) / ss * c1
40   if (ido .ne. 2 ) go to 41
     d1 = u2p * radi(i, 3) / radi(i, 4) / b2 / b2 * usr(i, 2)
     c2 = cmplxz (d1, fzero)
     z2m = c2 / ss
     go to 44
41   d1 = u2p * radi(i, 5) / radi(i, 6) / b4 / b4 * usr(i, 3)
     c2 = cmplxz (d1, fzero)
     if (ido .eq. 3) z3m = c2 / ss
     go to 44
42   s7 = su0 / s2 * s1 / s0 * c1
     s8 = su0 / s3 * bin(1) / bin(2) * c1
     z2m = czero
     z3m = czero
44   if (iprs47 .ge. 3) write (unit = logsix, fmt = 3148) z2i, z2o, z2m, z23
3148 format (/, 1x, 8x, 'real-z2i', 8x, 'imag-z2i', 8x, 'real-z2o', 8x, 'imag-z2o', 8x, 'real-z2m', 8x, 'imag-z2m', 8x, 'real-z23', 8x, 'imag-z23', /, 1x, 8e16.7)
     if (ido .eq. 3) go to 60
     z2i = s7
     z2o = s8
     z3i = czero
     z3m = czero
     z3o = czero
     z34 = czero
     if (ncpp(i) .eq. 2) go to 90
     ss = csqrtz (s)
     c1 = cmplxz (b4, fzero)
     s2 = c1 * ss
     c1 = cmplxz (b5, fzero)
     s3 = c1 * ss
     ido = 3
     go to 28
60   z3i = s7
     z3o = s8
90   s1 = z11 + z12 + z2i
     s2 = z2o + z23 + z3i
     s3 = z3o + z34
     s4 = s3 - 2.0d0 * z3m
     s5 = s3 - z3m
     s6 = s2 + s4
     s7 = s6 - z2m
     s8 = cimag1
     d1 = aimagz (s5)
     d2 = aimagz (s6)
     d3 = aimagz (s7)
     c1 = cmplxz (d1, fzero)
     c2 = cmplxz (d2, fzero)
     c3 = cmplxz (d3, fzero)
     if (realz (s5) .lt. 0.0d0) s5 = s8 * c1
     if (realz (s6) .lt. 0.0d0) s6 = s8 * c2
     if (realz (s7) .lt. 0.0d0) s7 = s8 * c3
     zc(i, i) = s1 + s2 - 2.0d0 * z2m + s4
     if (ncpp(i) .eq. 1) cycle
     i1 = i + npc
     if (i1 .gt. npc2) cycle
     zc(i1, i1) = s6
     zc(i, i1) = s7
     zc(i1, i) = s7
     if (ncpp(i) .eq. 2) cycle
     i1 = i + npc2
     zc(i1, i1) = s3
     zc(i1, i) = s5
     zc(i, i1) = s5
     i2 = i + npc
     zc(i1, i2) = s5
     zc(i2, i1) = s5
     if (iprs47 .ge. 3) write (unit = logsix, fmt = 3159) s1, s2, zc(i, i), zc(i, i1)
3159 format (/,  " Store  'zc'  values.", 13x, 'real-s1', 13x, 'imag-s1', 13x, 'real-s2', 13x, 'imag-s2', /, 21x, 4e20.11, /, 8x, 'real-zc(i,i)', 8x, 'imag-zc(i,i)', 7x, 'real-zc(i,i1)', 7x, 'imag-zc(i,i1)', /, 1x, 4e20.11)
  end do
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3174)
3174 format (/, " Exit  'sczy2' .")
  return
end subroutine sczy2

!
! subroutine ptzy1.
!

subroutine  ptzy1 (radi, dci, thc, dr0, th0, al0, ldm)
  use com47
  use tracom
  implicit none
  integer(4) :: ldm
  real(8), intent(out) :: al0(ldm, ldm)
  real(8), intent(in) :: dci(ldm)
  real(8), intent(out) :: dr0(ldm, ldm)
  real(8), intent(in) :: radi(ldm, 7)
  real(8), intent(out) :: th0(ldm, ldm)
  real(8), intent(in) :: thc(ldm)
  !  dimension radi(ldm, 7), dci(ldm), thc(ldm)
  !  dimension dr0(ldm, ldm), th0(ldm, ldm), al0(ldm, ldm)
  integer(4) :: i
  integer(4) :: j
  integer(4) :: k
  integer(4) :: n
  real(8) :: ak
  real(8) :: cn
  real(8) :: dkl
  real(8) :: p2
  !
  p2 = 2.0d0 * pai
  bp1 = radp(1) * sqrtz (u0 / rop * usp)
  bp2 = radp(2) * sqrtz (u0 / rop * usp)
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3238) npp, npc, bp1, bp2, radp(3)
3238 format (/, " Begin  'ptzy1' .     npp     npc", 17x, 'bp1', 17x, 'bp2', 13x, 'radp(3)', /, 17x, 2i8, 3e20.11)
  if (npp .eq. 0) go to 5
  alpi = alogz (radp(3) / radp(2))
  go to 8
5 alpi = 0.0d0
8 do i = 1, npc
     do j = 1, npc
        dr0(i, j) = dci(i) * dci(j) / radp(1) ** 2
        !        if(i-j) 10,20,50
        if (i - j .lt. 0) then
           go to 10
        else if (i - j .eq. 0) then
           go to 20
        else
           cycle
        end if
10      th0(i, j) = (thc(j) - thc(i)) * pai / 180.0d0
        if (th0(i, j) .gt. p2) th0(i, j) = th0(i, j) - p2
        dkl = sqrtz (dci(i) ** 2 + dci(j) ** 2 - 2.0d0 * dci(i) * dci(j) * cosz (th0(i, j)))
        al0(i, j) = alogz (radp(1) / dkl)
        th0(j, i) = th0(i, j)
        al0(j, i) = al0(i, j)
        cycle
20      th0(i, j) = 0.0d0
        dkl = radi(i, 7)
        al0(i, j) = alogz (radp(1) / dkl * (1.0d0 - dr0(i, j)))
     end do
  end do
  n = 19
  do i = 1, npc
     do j = 1, npc
        cn = 0.0d0
        if (i .eq. j) cycle
        if (j .lt. i) cycle
        if (dci(i) * dci(j) .lt. 1.0e-6) go to 65
        do k = 1, n
           ak = k
           cn = dr0(i, j) ** k * cosz (ak * th0(i, j)) / ak + cn
        end do
65      al0(i, j) = al0(i, j) - cn
        al0(j, i) = al0(i, j)
     end do
  end do
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3256) dkl, cn, alpi, al0(1, 1), al0(1, 2)
3256 format (/, " Exit  'ptzy1' .", 17x, 'dkl', 18x, 'cn', 16x, 'alpi', 12x, 'al0(1,1)', 12x, 'al0(1,2)', /, 16x, 5e20.11)
  if (iprs47 .ge. 4) write (unit = logsix, fmt = 3263) ((al0(i, j), j = 1, npc), i = 1, npc)
3263 format (/, ' Diagnostic output matrix.    ( (al0(i,j), j=1, npc), i=1, npc)', /, (1x, 6e20.11))
  return
end subroutine ptzy1

!
! subroutine ptzy2.
!

subroutine ptzy2 (s, ncpp, dci, dr0, th0, al0, zp, zpc, ldm, ldn)
  use komthl
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ncpp(ldm)
  real(8), intent(in) :: al0(ldm, ldm)
  real(8), intent(in) :: dci(ldm)
  real(8), intent(in) :: dr0(ldm, ldm)
  real(8), intent(in) :: th0(ldm, ldm)
  complex(16), intent(in) :: s
  integer(4) :: i, i1, i2, ixa
  integer(4) :: j, j1, j2
  integer(4) :: k, k1, kn
  integer(4) :: ll0, ll1, ll3
  integer(4) :: nbess, nc1
  real(8) :: ak
  real(8) :: c8, c9, ck
  real(8) :: contwo
  real(8) :: d1, d2
  real(8) :: unity
  real(8) :: xa
  complex(16) :: ss, s1, s2, s3, s4, s5, s6, su0
  complex(16) :: se0, zm, zi, zzo, zzi
  complex(16) :: c1, c2, c3, c4, c5, c6
  complex(16) :: zp(ldn, ldn), zpc(ldn, ldn)
  !
  !  dimension ncpp(ldm), dci(ldm)
  !  dimension dr0(ldm, ldm), th0(ldm, ldm), al0(ldm, ldm)
  !
  unity = 1.0d0
  contwo = 2.0d0
  c3 = cmplxz (usp, fzero)
  c4 = cmplxz (contwo, fzero)
  c8 = rop / 2.0d0 / pai / radp(1) / radp(2)
  c6 = cmplxz (c8, fzero)
  ll1 = 1
  ll3 = 3
  nbess = 19
  kn = nbess + 1
  ss = csqrtz (s)
  su0 = s * cmplxz (u2p, fzero)
  se0 = s * cmplxz (e2p, fzero)
  s1 = ss * cmplxz (bp1, fzero)
  s2 = ss * cmplxz (bp2, fzero)
  zm = czero
  zzo = su0 * cmplxz (alpi, fzero)
  ixa = 0
  xa = cabsz (s1)
  if (xa .gt. 10.0d0) ixa = 1
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3316) npc, npp, xa, alpi, s
3316 format (/, " begin  'ptzy2' .",  '     npc     npp', 18x, 'xa', 16x, 'alpi', 14x, 'real-s',  14x, 'imag-s', /, 17x, 2i8, 4e20.11)
  if (npp .eq. 0) go to 40
  call bsikm (s2, kn, bin, bkn, ll1, ixa)
  se0 = bin(2)
  s3 = bkn(2)
  s4 = bin(1)
  s5 = bkn(1)
40 call bsikm (s1, kn, bin, bkn, ll3, ixa)
  if (npp .ne. 0) go to 42
  zzi = su0 * c3 * bkn(1) / bkn(2) / s1
  go to 55
42 if (ixa .eq. 1) go to 45
  ss = se0 * bkn(2) - bin(2) * s3
  zm = c6 / ss
  zzo = su0 * c3 / s2 * (s4 * bkn(2) + s5 * bin(2)) / ss + zzo
  zzi = su0 * c3 * (bin(1) * s3 + bkn(1) * se0) / ss / s1
  go to 55
45 s6 = s2 - s1
  if (cabsz (s6) .gt. pekexp) go to 50
  s6 = cexpz (s6)
  ss = se0 * bkn(2) * s6 - bin(2) * s3 / s6
  zm = c6 / ss
  zzo = su0 * c3 / s2 * (s4 * bkn(2) * s6 + s5 * bin(2) /s6) / ss + zzo
  zzi = su0 * c3 * (bin(1) * s3 / s6 + bkn(1) * se0 * s6) / ss / s1
  go to 55
50 zzo = su0 * c3 / s2 * s4 / se0 + zzo
  zzi = su0 * c3 * bkn(1) / bkn(2) / s1
55 continue
  do i = 1, npc
     do j = 1, npc
        zi = czero
        if (j .lt. i) go to 25
        if ((dci(i) * dci(j)) .lt. 1.e-6) go to 21
        do k = 1, nbess
           k1 = k + 1
           ak = k
           ck = dr0(i, j) ** k * cosz (ak * th0(i, j))
           c1 = cmplxz(ck, fzero)
           c9 = ak * (usp + 1.)
           c2 = cmplxz(c9, fzero)
           zi = zi + c1/ (c2 + s1 * bkn(k)/bkn(k1))
        end do
21      c5 = cmplxz (al0(i, j), fzero)
        zp(i, j) = su0 * (c3 * c4 * zi + c5)
        zp(j, i) = zp(i, j)
25      i1 = i + npc
        j1 = j + npc
        if (ncpp(i) .eq. 1) go to 26
        zp(i1, j) = zp(i, j)
        zp(i1, j1) = zp(i, j)
26      if (ncpp(j) .eq. 1) cycle
        zp(i, j1) = zp(i, j)
        i2 = i + npc2
        j2 = j + npc2
        if (ncpp(i) .eq.2) go to 27
        zp(i2, j) = zp(i, j)
        zp(i2, j2) = zp(i, j)
        zp(i2, j1) = zp(i, j)
27      if (ncpp(j) .eq. 2) cycle
        zp(i, j2) = zp(i, j)
        zp(i1, j2) = zp(i, j)
     end do
  end do
  if (iprs47 .ge. 2) write (unit = logsix, fmt = 3327) (zp(1, j), j = 1, 3)
3327 format (/, ' middle.', 8x, 'real-zp(1,1)', 8x, 'imag-zp(1,1)', 8x, 'real-zp(1,2)', 8x, 'imag-zp(1,2)', 8x, 'real-zp(1,3)', 8x, 'imag-zp(1,3)', /, 8x, 6e20.11, /, ' diagnostic   zp(i,j)  for  (i,j)=1, ... npc .')
  ll0 = 0
  if (iprs47 .ge. 5) call print (zp(1, 1), npc, ll0, ldn)
  if (npp .eq. 0) go to 90
  s1 = zzi + zzo - 2.0d0 * zm
  s2 = zzo - zm
  s3 = cimag1
  d1 = aimagz (s1)
  d2 = aimagz (s2)
  c1 = cmplxz (d1, fzero)
  c2 = cmplxz (d2, fzero)
  if (realz(s1) .lt. 0.) s1=s3*c1
  if (realz(s2) .lt. 0.) s2=s3*c2
  if (iprs47 .ge. 3) write (unit = logsix, fmt = 3345) ss, zm, zzo
3345 format (/, ' pipe.', 13x, 'real-ss', 13x, 'imag-ss', 13x, 'real-zm', 13x, 'imag-zm', 12x, 'real-zzo', 12x, 'imag-zzo', /, 6x, 6e20.11)
  nc1 = ncc - npp
  if (iprs47 .gt. 1) write (unit = logsix, fmt = 3354) ncc, nc1, ixa, s1, s2
3354 format (/, ' more pipe.     ncc     nc1     ixa', 13x, 'real-s1', 13x, 'imag-s1', 13x, 'real-s2', 13x, 'imag-s2', /, 11x, 3i8, 4e20.11)
  do i = 1, nc1
     zp(i, ncc) = czero
     zp(ncc, i) = czero
     zpc(i, ncc) = s2
     zpc(ncc, i) = s2
     do j = 1, nc1
        if (j .lt. i) cycle
        zpc(i, j) = s1
        zpc(j, i) = s1
     end do
  end do
  zp(ncc, ncc) = czero
  zpc(ncc, ncc) = zzo
90 if (iprs47 .ge. 1) write (unit = logsix, fmt = 3368) usp, pai, u2p, e2p, su0
3368 format (/, " Exit  'ptzy2' .", 16x, 'usp', 16x, 'pai', 16x, 'u2p', 16x, 'e2p', 11x, 'real-su0', 11x, 'imag-su0', /, 16x, 6e19.10)
  return
end subroutine ptzy2

!
! subroutine bsikm.
!

subroutine bsikm (x, kn, bbin, bbkn, ikm, ixa)
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: ikm
  integer(4), intent(in) :: ixa
  integer(4), intent(in) :: kn
  complex(16), intent(out) :: bbin(kn)
  complex(16), intent(out) :: bbkn(kn)
  integer(4) :: ik1, ik2, ikn
  integer(4) :: l
  real(8) :: c1, c2
  real(8) :: xa
  complex(16) :: bk0, bk1, bi0, bj1, x
  complex(16) :: y, y0, y1, y2, y3, y4, y5, y6, y7, y8
  !
  xa = cabsz (x)
  c1 = 3.75d0
  c2 =  2.0d0
  y = x / cmplxz (c1, fzero)
  if (iprs47 .ge. 5) write (unit = logsix, fmt = 3426) kn, ikm, ixa, x, y, xa
3426 format (/, " Begin  'bsikm' .      kn     ikm     ixa", 9x, 'real-x', 9x, 'imag-x',  9x, 'real-y', 9x, 'imag-y', 13x, 'xa', /, 17x, 3i8, 5e15.6)
  if (xa .gt. c1)  go to 25
  y1 = y * y
  y2 = y1 * y1
  y3 = y2 * y1
  y4 = y3 * y1
  y5 = y4 * y1
  y6 = y5 * y1
  bi0 = 1.0d0 + 3.5156229d0 * y1 + 3.0899424d0 * y2 + 1.2067492d0 * y3 + 0.2659732d0 * y4 + 0.0360768d0 * y5 + 0.0045813d0 * y6
  bj1 = x * (0.5d0 + 0.87890594d0 * y1 + 0.51498869d0 * y2 + 0.15084934d0 * y3 + 0.02658733d0 * y4 + 0.00301532d0 * y5 + 0.00032411d0 * y6)
  go to 29
25 y0 = csqrtz (x)
  if (ixa .ne. 1) y0 = y0 * cexpz (-x)
  y1 = 1.0d0 / y
  y2 = y1 / y
  y3 = y2 / y
  y4 = y3 / y
  y5 = y4 / y
  y6 = y5 / y
  y7 = y6 / y
  y8 = y7 / y
  bi0 = (0.39894228d0 + 0.01328592d0 * y1 + 0.00225319d0 * y2 - 0.00157565d0 * y3 + 0.00916281d0 * y4 - 0.02057706d0 * y5 + 0.02635537d0 * y6 - 0.01647633d0 * y7 + 0.00392377d0 * y8) / y0
  bj1 = (0.39894228d0 - 0.03988024d0 * y1 - 0.00362018d0 * y2 + 0.00163801d0 * y3 - 0.01031555d0 * y4 + 0.02282967d9 * y5 - 0.02895312d0 * y6 + 0.01787654d0 * y7 - 0.00420059d0 * y8) / y0
29 if (xa .gt. c2) go to 35
  y = x / cmplxz (c2, fzero)
  y0 = clogz (y)
  y1 = y * y
  y2 = y1 * y1
  y3 = y2 * y1
  y4 = y3 * y1
  y5 = y4 * y1
  y6 = y5 * y1
  bk0 = -y0 * bi0 - 0.57721566d0 + 0.42278420d0 * y1 + 0.23069756d0 * y2 + 0.03488590d0 * y3 + 0.00262698d0 * y4 + 0.00010750d0 * y5 + 0.00000740d0 * y6
  bk1 = y0 * bj1 + (1.0d0 + 0.15443144d0 * y1 - 0.67278579d0 * y2 - 0.18156897d0 * y3 - 0.01919402d0 * y4 - 0.00110404d0 * y5 - 0.00004686d0 * y6) / x
  go to 40
35 y = cmplxz (c2, fzero) / x
  y0 = csqrtz (x)
  if (ixa .ne. 1) y0 = y0 * cexpz (x)
  y1 = y * y
  y2 = y1 * y
  y3 = y2 * y
  y4 = y3 * y
  y5 = y4 * y
  bk0 = (1.25331414d0 - 0.07832358d0 * y + 0.02189568d0 * y1 - 0.01062446d0 * y2 + 0.00587872d0 * y3 - 0.00251540d0 * y4 + 0.00053208d0 * y5) / y0
  bk1 = (1.25331414d0 + 0.23498619d0 * y - 0.03655620d0 * y1 + 0.01504268d0 * y2 - 0.00780353d0 * y3 + 0.00325614d0 * y4 - 0.00068245d0 * y5) / y0
40 bbin(1) = bi0
  bbin(2) = bj1
  bbkn(1) = bk0
  bbkn(2) = bk1
  if (iprs47 .ge. 5) write (unit = logsix, fmt = 3452) bi0, bj1, bk0, bk1
3452 format (/, ' scalars.', 7x, 'real-bi0', 7x, 'imag-bi0', 7x, 'real-bj1', 7x, 'imag-bj1', 7x,  'real-bk0', 7x, 'imag-bk0', 7x, 'real-bk1', 7x, 'imag-bk1', /, 9x, 8e15.6)
  if(ikm.eq.1) go to 70
  do ikn=3,kn
     bbin(ikn) = creal1
     bbkn(ikn) = creal1
  end do
  if(ikm.eq.3) go to 60
  do ikn = 3, kn
     ik1=ikn-1
     ik2=ikn-2
     bbin(ikn) = bbin(ik2) - 2.0d0 * ik2 / x * bbin(ik1)
  end do
  if(ikm.eq.2) go to 70
60 do ikn=3,kn
     ik1=ikn-1
     ik2=ikn-2
     bbkn(ikn) = bbkn(ik2) + 2.0d0 * ik2 / x * bbkn(ik1)
  end do
70 if (iprs47 .ge. 6) write (unit = logsix, fmt = 3476) (l, bbin(l), bbkn(l), l = 1, kn)
3476 format (/, " Exit    'bsikm' .",  5x, 'row', 11x, 'real-bbin', 11x, 'imag-bbin', 11x, 'real-bbkn', 11x, 'imag-bbkn', /, (18x, i8, 4e20.11))
  return
end subroutine bsikm

!
! subroutine olzy.
!

subroutine  olzy (w, ncpp, zy, dij, ang, usi, usr, esi, hi, di, zs, ze, zc, ldm, ldn)
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ncpp(ldm)
  real(8), intent(in) :: ang(ldm, ldm)
  real(8), intent(in) :: di(ldn)
  real(8), intent(in) :: dij(ldm, ldm)
  real(8), intent(in) :: esi(ldm, 3)
  real(8), intent(in) :: hi(ldm)
  real(8), intent(in) :: usi(ldm, 3)
  real(8), intent(in) :: usr(ldm, 3)
  real(8), intent(in) :: w
  real(8), intent(in) :: zy(ldn, ldn)
  integer(4) :: i, im
  integer(4) :: j, j1, j2, jnc
  integer(4) :: k
  integer(4) :: ll0, ll1
  real(8) :: b1, b2
  real(8) :: d12
  real(8) :: ur
  real(8) :: zero
  complex(16) :: zs(ldn, ldn), ze(ldn, ldn), zc(ldn, ldn)
  complex(16) :: cwu, s, ss, xc, xe
  !
  !  dimension ncpp(ldm)
  !
  s = cmplxz (fzero, w)
  cwu = s * cmplxz(u2p, fzero)
  ss = csqrtz (s)
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3508) iearth, ncct, u2p, w, ss
3508 format (/, " Begin  'olzy' .  iearth    ncct", 17x, 'u2p', 19x, 'w', 13x, 'real-ss', 13x, 'imag-ss', /, 16x, 2i8, 4e20.11)
  ll1 = 1
  do i = 1, npc
     do j = 1, npc
        if (j .lt. i) cycle
        zs(i, j) = cwu * cmplxz (zy(i, j), fzero)
        zs(j, i) = zs(i, j)
        if (iearth .eq. 99) go to 15
        b1 = dij(i, j) * sqrtz (u0 / roe)
        b2 = ang(i, j)
        zero = 0.0d0
        call zegen (zero, b1, b2, w, xe, ll1)
        go to 18
15      d12 = absz (di(j) - di(i))
        call zest (hi(i), hi(j), d12, roe, w, xe)
18      ze(i, j) = xe
        ze(j, i) = xe
     end do
  end do
  if (iprs47 .ge. 2) write (unit = logsix, fmt = 3523) ncct, npc, u0, roe
3523 format (" Diagnostic within  'olzy' .   ze(i,j)  for  (i,j)=1, ... npc.", 5x, '    ncct     npc', 13x, 'u0', 12x, 'roe', /, 68x, 2i8, 2e15.6)
  ll0 = 0
  if (iprs47 .ge. 4) call print (ze(1, 1), npc, ll0, ldn)
  jnc = 1
  do j = 1, 2
     do i = 1, ncct
        im = 4 * (i - 1) + 1
        j1 = im + j + 1
        j1 = ncpp(j1)
        if (j1 .eq. 0) cycle
        b1 = usi(i, j)
        b2 = esi(i, j)
        ur = usr(i, j)
        call skin47 (b1, b2, ur, s, ss, xc)
        xc = xc / j1
        j1 = im + j - 1
        j1 = ncpp(j1)
        j2 = j1 + jnc - 1
        do k = jnc, j2
           zc(k, k) = xc
        end do
        if (iprs47 .ge. 6) write (unit = logsix, fmt = 3541) j, i, jnc, j2, j1, xc
3541    format (/, " Inside  (j,i)  loop of  'olzy' .       j       i     jnc      j2      j1", 13x, 'real-xc', 13x, 'imag-xc', /, 33x, 5i8, 2e20.11)
        jnc = j2 + 1
     end do
  end do
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3554)
3554 format (/, " Exit  'olzy' .")
  return
end subroutine olzy

!
! subroutine transp.
!

subroutine transp (yyc, ncpp, ann, jnn, znn, ldm, ldn)
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: ldm
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ncpp(ldm)
  real(8), intent(out) :: ann(ldn)
  real(8), intent(out) :: jnn(ldn)
  complex(16), intent(out) :: yyc(ldn, ldn)
  complex(16) :: zss, zmm, znn(ldn)
  integer(4) :: i, i2
  integer(4) :: j, j1
  integer(4) :: k, k1
  integer(4) :: l, ll0
  real(8) :: amm, ass
  real(8) :: dnn
  !
  !  dimension ncpp(ldm), ann(ldn), jnn(ldn)
  !
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3611) ncct, yyc(1, 1), yyc(1, 2)
3611 format (/,  " Enter  'transp' .    ncct", 7x,  'real-yyc(1,1)', 7x, 'imag-yyc(1,1)', 7x,  'real-yyc(1,2)', 7x, 'imag-yyc(1,2)', /, 18x, i8, 4e20.11, /, ' diagnostic input matrix.   yyc(i,j)  for  (i,j)=1, ... i2 .')
  i2 = 6
  ll0 = 0
  if (iprs47 .ge. 4) call print (yyc(1, 1), i2, ll0, ldn)
  i2 = 0
  do k = 1, ncct
     k1 = (k - 1) * 4 + 1
     j1 = i2 + 1
     i2 = ncpp(k1) + i2
     jnn(k) = j1
     zss = czero
     ass = 0.0d0
     zmm = czero
     amm = 0.0d0
     do i = 1, ncct
        znn(i) = czero
        ann(i) = 0.0d0
     end do
     do i = 1, i2
        do j = j1, i2
           if (i .lt. j1) go to 20
           if (j - i .lt. 0) then
              cycle
           else if (j - i .eq. 0) then
              go to 10
           else
              go to 15
           end if
10         zss = zss + yyc(i, j)
           ass = ass + 1.0d0
           cycle
15         zmm = zmm + yyc(i, j)
           amm = amm + 1.0d0
           cycle
20         if (k .eq. 1) cycle
           dnn = (j - jnn(k)) / (jnn(k) - 1)
           if (dnn .lt. 1.0d0) dnn = 0.0d0
           l = int (dnn + 1.0d0)
           znn(l) = znn(l) + yyc(i, j)
           ann(l) = ann(l) + 1.0d0
        end do
     end do
     zss = zss / cmplxz (ass, fzero)
     zmm = zmm / cmplxz (amm, fzero)
     k1 = k - 1
     if (k1 .le. 0) go to 55
     do i = 1, k1
        znn(i) = znn(i) / cmplxz (ann(i), fzero)
     end do
55   do i = 1, i2
        do j = j1, i2
           if (j .lt. i) cycle
           if (i .lt. j1) go to 70
           !           if ( j - i )   90,  60,  65
           if (j - i .lt. 0) then
              cycle
           else if (j - i .eq. 0) then
              go to 60
           else
              go to 65
           end if
60         yyc(i, j) = zss
           cycle
65         yyc(i, j) = zmm
           go to 85
70         if (k .eq. 1) go to 85
           dnn = (j - jnn(k)) / (jnn(k) - 1)
           if (dnn .lt. 1.0d0) dnn = 0.0d0
           l = int (dnn + 1.0d0)
           yyc(i, j) = znn(l)
85         yyc(j, i) = yyc(i, j)
        end do
     end do
  end do
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3623) yyc(1, 1), yyc(1, 2)
3623 format (/, " Exit  'transp' .", 7x, 'real-yyc(1,1)', 7x, 'imag-yyc(1,1)', 7x,  'real-yyc(1,2)', 7x, 'imag-yyc(1,2)', /, 17x, 4e20.11 , /,' diagnostic output matrix.  yyc(i,j)  for  (i,j)=1, ... i2 .')
  if (iprs47 .ge. 4) call print (yyc(1, 1), i2, ll0, ldn)
  return
end subroutine transp

!
! subroutine skin47.
!

subroutine skin47 (b1, b2, ur, cjw, sjw, zcc)
  use komthl
  use com47
  use tracom
  implicit none
  real(8), intent(in) :: b1
  real(8), intent(in) :: b2
  real(8), intent(in) :: ur
  complex(16), intent(in) :: cjw
  complex(16), intent(in) :: sjw
  complex(16), intent(out) :: zcc
  integer(4) :: ixa
  integer(4) :: ll1, ll2
  real(8) :: d1, d15
  real(8) :: ten
  real(8) :: xa, xb
  complex(16) :: x1, x2, x3, x4
  complex(16) :: c1, c2
  !
  c1 = cmplxz(u2p, fzero)
  c2 = cmplxz(ur, fzero)
  x1 = cmplxz(b1, fzero) * sjw
  x2 = cmplxz(b2, fzero) * sjw
  ixa = 0
  xa = cabsz (x2)
  ll1 = 1
  ll2 = 2
  d15 = value1 * value5 / 100.
  if ( iprs47  .ge.  2 ) write (logsix, 3672)  b1, b2, d15, xa, xb
3672 format (/, " Begin  'skin47' .", 18x, 'b1', 18x, 'b2', 17x, 'd15', 18x, 'xa', 18x, 'xb', /, 18x, 5e20.11)
  if (b1 .lt. d15)   go to 30
  xb = cabsz(x1)
  ten = 10.
  if (xa .lt. ten)  go to 10
  d1 = 15./4.
  if (xb .gt. d1)  ixa = 1
10 call bsikm ( x1, ll2, bin, bkn, ll1, ixa )
  x3 = bin(2)
  x4 = bkn(2)
  call bsikm ( x2, ll2, bin, bkn, ll1, ixa )
  if (ixa .eq. 1) go to 20
  zcc = cjw * c1/x2 * (bin(1) * x4 + bkn(1) * x3) * c2
  x3 = bin(2) * x4  -  bkn(2) * x3
  go to 25
20 x1=x2-x1
!!!!  d2 = 50.
  if (cabsz(x1) .gt. pekexp) go to 33
  x1=cexpz(x1)
  zcc= cjw * c1  / x2  *  ( bin(1)*x4*x1 + bkn(1)*x3/x1 ) * c2
  x3 = bin(2) * x4 * x1  -  bkn(2) * x3 / x1
25 zcc = zcc/x3
  go to 40
30 call bsikm (x2, ll2, bin, bkn, ll1, ll1)
33 zcc = cjw * c1  / x2  *  bin(1) / bin(2)  *  c2
40 if (iprs47 .ge. 2) write (unit = logsix, fmt = 3688) ur, cjw, zcc
3688 format (/, " Exit  'skin47' .", 18x, 'ur', 12x, 'real-cjw', 12x, 'imag-cjw', 12x, 'real-zcc', 12x, 'imag-zcc', /, 17x, 5e20.11)
  return
end subroutine skin47

!
! subroutine zegen.
!

subroutine zegen (be1, be2, th, w, xe, isyst)
  use komthl
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: isyst
  real(8), intent(in) :: be1
  real(8), intent(in) :: be2
  real(8), intent(in) :: th
  real(8), intent(in) :: w
  integer(4) :: i, iter, ixa
  integer(4) :: ll1, ll2
  real(8) :: a1, a2, a3, a4
  real(8) :: b1, b2, b3, b4, bn
  real(8) :: cn, cs1, cs2, cs3, cs4
  real(8) :: d1, dn
  real(8) :: e, en, euc, evennn
  real(8) :: fn
  real(8) :: p1
  real(8) :: q1
  real(8) :: r1, r2
  real(8) :: sn, sq2, ss1, ss2
  real(8) :: t, t1, t2, t3, t4, t5, t6, t7, t8
  real(8) :: unity
  real(8) :: verbin
  real(8) :: xa, xa1
  complex(16) :: cj, cjw, x1, x2, xe
  complex(16) :: c1
  !
  unity = 1.0d0
  c1 = cmplxz (u2p, fzero)
  cj = cimag1
  euc = 2.0d0 / value4
  d1 = 2.0d0
  sq2 = sqrtz (d1)
  ll1 = 1
  ll2 = 2
  if (isyst .ge. 0) go to 140
  cjw = cj * cmplxz (w, fzero)
  xe = csqrtz (cjw)
  x1 = cmplxz (be1, fzero) * xe
  x2 = cmplxz (be2, fzero) * xe
  ixa = 0
  xa = cabsz (x2)
  if (xa .gt. 10.0d0) ixa = 1
  call bsikm (x1, ll2, bin, bkn, ll1, ixa)
  xe = bkn(1)
  if (xa .gt. 100.0d0) go to 130
  call bsikm (x2, ll2, bin, bkn, ll1, ixa)
  if (ixa .gt. 0) go to 120
  xe = cjw * c1 * (xe - bkn(1))
  go to 5
120 xe = cjw * c1 * (xe - bkn(1) / cexpz (x2 - x1)) / cexpz(x1)
  go to 5
130 xa1 = cabsz (x1)
  if (xa1 .gt. pekexp) go to 140
  xe = cjw * c1 * xe / cexpz (x1)
  go to 5
140 xe = czero
5 e = be2 * sqrtz (w)
  if (iprs47 .ge. 2) write (unit = logsix, fmt = 3741) isyst, ixa, be1, be2, xa, th, xa1, xe, w,e
3741 format (/, " Within  'zegen' .   isyst     ixa", 16x, 'be1', 16x, 'be2', 17x, 'xa', 17x, 'th', 16x, 'xa1', /, 18x, 2i8, 5e19.10, /, 1x, 13x, 'real-xe', 13x, 'imag-xe', 19x, 'w', 19x, 'e', /, 1x, 4e20.11)
  if (e .gt. 5.0d0) go to 60
  r2 = e ** 4
  r1 = r2 / 16.0d0
  sn = e ** 2 / 8.0d0
  bn = r1 / 12.0d0
  cn = e / 3.0d0
  dn = 5.0d0 / 4.0d0
  en = e ** 3 / 45.0d0
  fn = 5.0d0 / 3.0d0
  iter = 11
  if (e .ge. 1.0d0) iter = 21
  iter = 21
  do i = 1, iter
     t = i - 1
     t1 = t * 2.0d0
     t2 = t * 4.0d0
     cs1 = cosz ((t2 + 2.0d0) * th)
     ss1 = sinz ((t2 + 2.0d0) * th)
     cs2 = cosz ((t2 + 4.0d0) * th)
     ss2 = sinz ((t2 + 4.0d0) * th)
     cs3 = cosz ((t2 + 1.0d0) * th)
     cs4 = cosz ((t2 + 3.0d0) * th)
     if (i .gt. 1) go to 30
     a1 = sn * cs1
     a2 = sn * ss1
     a3 = bn * cs2
     a4 = bn * ss2
     b1 = cn * cs3
     b2 = dn * a1
     b3 = en * cs4
     b4 = fn * a3
     evennn = a1 + a2 + a3 + a4 + b1 + b2 + b3 + b4
     cycle
30   t3 = -t1 * (t1 + 1.0d0) ** 2 * (t1 + 2.0d0)
     t4 = -(t1 + 1.0d0) * (t1 + 2.0d0) ** 2 * (t1 + 3.0d0)
     t5 = -(t2 - 1.0d0) * (t2 + 1.0d0) ** 2 * (t2 + 3.0d0)
     t6 = 1.0d0 / t2 + 1.0d0 / (t1 + 1.0d0) + 1.0d0 / (t1 + 2.0d0) - 1.0d0 / (t2 + 4.0d0)
     t7 = -(t2 + 1.0d0) * (t2 + 3.0d0) ** 2 * (t2 + 5.0d0)
     t8 = 1.0d0 / (t2 + 2.0d0) + 1.0d0 / (t1 + 2.0d0) + 1.0d0 / (t1 + 3.0d0) - 1.0d0 / (t2 + 6.0d0)
     sn = sn * r1 / t3
     bn = bn * r1 / t4
     cn = cn * r2 / t5
     dn = dn + t6
     en = en * r2 / t7
     fn = fn + t8
     a1 = a1 + sn * cs1
     a2 = a2 + sn * ss1
     a3 = a3 + bn * cs2
     a4 = a4 + bn * ss2
     b1 = b1 + cn * cs3
     b2 = b2 + dn * sn * cs1
     b3 = b3 + en * cs4
     b4 = b4 + fn * bn * cs2
     verbin = evennn
     evennn = a1 + a2 + a3 + a4 + b1 + b2 + b3 + b4
     if ((1.0d0 - verbin / evennn) * (1.0d0 - verbin / evennn) .lt. 1.0e-12) go to 888
  end do
888 continue
  p1 = pai * (1.0d0 - a3) / 4.0d0 + a1 * alogz (euc / e) + th * a2 + b2 + sq2 * (b3 - b1)
  q1 = 0.5d0 + (1.0d0 - a3) * alogz (euc / e) - th * a4 - pai * a1 / 4.0d0 - b4 + sq2 * (b1 + b3)
  go to 70
60 cs1 = sq2 * cosz (th)
  cs2 = cosz (2.0d0 * th) * 2.0d0
  cs3 = sq2 * cosz (3.0d0 * th)
  cs4 = 3.0d0 * sq2 * cosz (5.0d0 * th)
  p1 = (cs1 + (cs4 / e ** 3 + cs3 / e - cs2) / e) / e
  q1 = (cs1 + (cs4 / e ** 2 - cs3) / e ** 2) / e
70 xe = cmplxz (w, fzero) * c1 * (cmplxz (p1, fzero) + cj * cmplxz (q1, fzero)) + xe
  if (iprs47 .ge. 2) write (unit = logsix, fmt = 3782) p1, q1, xe
3782 format (/, " Exit  'zegen' .", 17x, 'p1', 17x, 'q1', 12x, 'real-xe', 12x, 'imag-xe', /, 16x, 4e19.10)
  return
end subroutine zegen

!
! subroutine eigen.
!

subroutine eigen (cjw, p, n, a, ai, qn, q, xx, yy, ldn)
  use blkcom
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: n
  complex(16), intent(out) :: a(ldn, ldn)
  complex(16), intent(out) :: ai(ldn, ldn)
  complex(16), intent(in) :: cjw
  complex(16), intent(out) :: q(ldn, ldn)
  complex(16), intent(out) :: qn(ldn)
  complex(16), intent(out) :: p(ldn, ldn)
  complex(16), intent(out) :: xx(ldn, ldn)
  complex(16), intent(out) :: yy(ldn, ldn)
  integer(4) :: i, i1, i2, im, iq
  integer(4) :: j
  integer(4) :: k, kvalue
  integer(4) :: l, ll0
  real(8) :: d1, d2, dm, dx
  real(8) :: r
  real(8) :: znvref
  complex(16) :: ad, qi, s, sa
  complex(16) :: c1, c2
  !     Eigenvalue calculation subroutine.   'kvalue'  =  iteration limit.
  kvalue = 20
  c1 = cjw / cmplxz (spdlgt, fzero)
  c2 = cmplxz (unity, fzero)
  qi = c2 / c1
  qi = qi * qi
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3806) n, cjw, qi
3806 format (/, "Enter  'eigen' .       n", 12x, 'real-cjw', 12x, 'imag-cjw', 13x, 'real-qi', 13x, 'imag-qi', /, 17x, i8, 4e20.11, /, 1x)
  do i = 1, n
     do j = 1, n
        p(i, j) = p(i, j) * qi
     end do
     p(i, i) = p(i, i) - c2
  end do
  do i = 1, n
     do j = 1, n
        q(i, j) = p(i, j)
     end do
  end do
  l = 0
15 l = l + 1
  iq = 0
  !  if(l-n) 20,90,90
  if (l - n .lt. 0) then
     go to 20
  else if (l - n .ge. 0) then
     go to 90
  end if
20 iq = iq + 1
  if (iq .le. kvalue) go to 25
  iq = iq - 1
  write (unit = lunit(6), fmt = 902) iq
902 format (5x, "Warning ; a higher accuracy can't be, achieved by this computer.", /, 5x, 'eigen values   1 vectors at itteration iq= ', i3, ' is adopted.')
  go to 63
25 do i = 1, n
     do j = 1, n
        xx(i, j) = q(i, j)
        yy(i, j) = q(i, j)
     end do
  end do
  do i = 1, n
     do j = 1, n
        q(i, j) = czero
        do k = 1, n
           q(i, j) = q(i, j) + xx(i, k) * yy(k, j)
        end do
     end do
  end do
  dm = 0.0d0
  do i = 1, n
     dx = cabsz (q(i, i))
     if (dx .le. dm) cycle
     dm = dx
     im = i
  end do
  qi = 1.0d0 / q(im, im)
  do i = 1, n
     do j = 1, n
        q(i, j) = q(i, j) * qi
     end do
  end do
  dm = 0.0d0
  do i = 1, n
     if (i .eq. im) cycle
     dx = cabsz (q(i, im))
     if (dx .le. dm) cycle
     dm = dx
     i1 = i
     i2 = im
  end do
  if (dm .ne. 0.0d0) go to 60
  do i = 1, n
     if (i .eq. im) cycle
     dx = cabsz (q(im, i))
     if (dx .le. dm) cycle
     dm = dx
     i1 = im
     i2 = i
  end do
60 sa = q(im, im) / q(i1, i2)
  s = xx(im, im) / xx(i1, i2)
  r = cabsz (s / sa)
  d1 = 50.0d0 * epsiln
  !     non-64-bit complex math redefines tolerance in "sysdep":
  if (znvref .ne. 0.0d0) d1 = 50.0d0 * znvref
  d2 = r - unity
  if (iprs47 .ge. 31) write (unit = logsix, fmt = 3827) l, iq, kvalue, d2, d1
3827 format (' Done another iteration.   l, iq, kvalue =', 3i8, 10x,  'd2, d1 =', 2e15.6)
  if (absz (d2) .gt. d1) go to 20
63 do i = 1, n
     a(i, l) = q(i, im)
     ai(l, i) = q(im, i)
  end do
  ad = czero
  do i = 1, n
     ad = ad + p(im, i) * q(i, im)
  end do
  qn(l) = ad / q(im, im)
  ad = czero
  do i = 1, n
     ad = ad + ai(l, i) * a(i, l)
  end do
  ad = 1.0d0 / ad
  do i = 1, n
     ai(l, i) = ad * ai(l, i)
  end do
  do i = 1, n
     do j = 1, n
        q(i, j) = p(i, j) - a(i, l) * qn(l) * ai(l, j)
        p(i, j) = q(i, j)
     end do
  end do
  if (iprs47 .ge. 7) write (unit = logsix, fmt = 3842) l, qn(l), ad
3842 format (' Eigenvalue finished.       l', 10x, 'real-qn(l)', 10x, 'imag-qn(l)', 13x,  'real-ad',  13x, 'imag-ad', /, 21x, i8, 4e20.11, /, 1x)
  go to 15
90 dm = 0.0d0
  do i = 1, n
     dx = cabsz (p(i, i))
     if (dx .le. dm) cycle
     dm = dx
     im = i
  end do
  ad = czero
  do i = 1, n
     ad = ad + p(im, i) * p(i, im)
     a(i, l) = p(i, im) / p(im, im)
  end do
  qn(l) = ad / p(im, im)
  do i = 1, n
     ai(l, i) = p(im, i) / qn(l)
  end do
  do i = 1, n
     qn(i) = c1 * csqrtz (c2 + qn(i))
  end do
  if (iprs47 .ge. 3) write (unit = logsix, fmt = 3854) dm, spdlgt, p(im, im), (qn(i), i = 1, n)
3854 format (/, " Done all eigenvalues in  'eigen' .", 18x,  'dm', 14x, 'spdlgt', 7x, 'real-p(im,im)', 7x, 'imag-p(im,im)', /, 35x, 4e20.11, /, ' complex eigenvalues  (qn(i), i=1, n)  follow ...', /, (1x, 6e20.11))
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 3867)
3867 format (/, " Diagnostic upon exit  'eigen' .   Matrix of eigenvectors  a(i,l)  for  (i,l)=1, ... n .")
  ll0 = 0
  if (iprs47 .ge. 6) call print (a(1, 1), n, ll0, ldn)
  return
end subroutine eigen

!
! subroutine zest.
!

subroutine  zest (h1, h2, e, res, omg, s)
  use com47
  use tracom
  implicit none
  real(8), intent(in) :: e
  real(8), intent(in) :: h1
  real(8), intent(in) :: h2
  real(8), intent(in) :: omg
  real(8), intent(in) :: res
  complex(16), intent(out) :: s
  integer(4) :: jd, jjj
  integer(4) :: kn
  integer(4) :: l
  integer(4) :: n
  real(8) :: ab
  real(8) :: bp
  real(8) :: c9, contwo
  real(8) :: d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, dx, dx2
  real(8) :: h1ph2, hkr
  real(8) :: omg2
  real(8) :: r, ram, ram2, rmax, rmin
  real(8) :: toj
  real(8) :: v5
  real(8) :: yud
  complex(16) :: qq, bbb, rom, sa, s1, s2
  complex(16) :: s3, s5, s6, s8, sp12, sp23, sm12
  complex(16) :: sm23, sq1, sq3, u, u1, u2, z
  complex(16) :: c1, c2, c3, c4, c5, c6, c7, c8
  !
  toj = u0
  if (iprs47 .ge. 2) write (unit = logsix, fmt = 3917) h1, h2, e, res, omg
3917 format (/, " Enter  'zest' .", 18x, 'h1', 18x, 'h2', 19 x, 'e', 17x, 'res', 17x, 'omg', /,  16x, 5e20.11)
  hkr = spdlgt ** 2
  yud =1.0d0 / toj / hkr
  h1ph2 = h1 + h2
  ab = 10.0d0 * e / h1ph2
  omg2 = omg * omg
  d1 = toj * omg / res
  rom = cmplxz (fzero, d1)
  bp = pai / 2.0d0
  rmax = 0.0d0
  s = czero
  l = 2
  ram = 0.0d0
700 ram2 = ram * ram
  d2 = ram2 + omg2 * (1.0d0 - htoj2 * hyud2) / hkr
  c1 = cmplxz(d2, fzero)
  c2 = rom * cmplxz (htoj2, fzero)
  s1 = csqrtz (c1 + c2)
  d3 = ram2 + omg2 * (1.0d0 - htoj3 * hyud3) / hkr
  c3 = cmplxz (d3, fzero)
  c4 = cmplxz (alf1, fzero) * rom * cmplxz (htoj3, fzero)
  s2 = csqrtz (c3 + c4)
  d4 = ram2 + omg2 * (1.0d0 - htoj4 * hyud4) / hkr
  c5 = cmplxz (d4, fzero)
  c6 = cmplxz (alf2, fzero) * rom * cmplxz (htoj4, fzero)
  s3 = csqrtz (c5 + c6)
  s6 = s2
  s5 = s1
  d5 = 1.0d0 / htoj2 / toj
  s1 = s1 * cmplxz(d5, fzero)
  d6 = 1.0d0 / htoj3 / toj
  s2 = s2 * cmplxz (d6, fzero)
  d7 = 1.0d0 / htoj4 / toj
  s3 = s3 * cmplxz(d7, fzero)
  s8 = cmplxz (toj, fzero) * s1
  sp12 = s1 + s2
  sp23 = s2 + s3
  sm12 = s1 - s2
  d8 = 2.0d0 * (dep1 - dep2)
  c7 = cmplxz (d8, fzero)
  sm23 = (s2 - s3) * cexpz (s6 * c7)
  d9 = -2.0d0 * dep1
  c8 = cmplxz (d9, fzero)
  bbb = cexpz (s5 * c8)
  d10 = expz (-h1ph2 * ram) * cosz (e * ram)
  contwo = 2.0d0
  z = cmplxz (fzero, contwo) * (sp12 * sp23 + sm12 * sm23 + bbb * (sm12 * sp23 + sp12 * sm23)) * cmplxz (d10, fzero) / ((cmplxz (ram, fzero) + s8) * (sp12 * sp23 + sm12 * sm23) + (cmplxz (ram,fzero) - s8) * bbb * (sm12 * sp23 + sp12 * sm23))
  if (iprs47 .ge. 5) write (unit = logsix, fmt = 3928) l, bp, ab, ram, rmax, z
3928 format (/, 1x, '       l', 18x, 'hbp', 18x, 'ab', 17x, 'ram', 16x, 'rmax', 14x, 'real-z', 14x, 'imag-z', /, 1x, i8, 6e20.11)
  !  go to (702,701),l
  select case (l)
  case (1)
     go to 702

  case (2)
     go to 701
  end select
701 u1 = z
  jd = 50
  go to 650
600 u1 = czero
650 if (bp .lt. ab) go to 300
  jd = 100
  rmin = rmax
  rmax = 10.0d0 / h1ph2
  l = 1
  ram = rmax
  go to 700
702 u2 = z
  go to 210
300 rmin = rmax
  rmax = bp / e
  bp = bp + pai
  u2 = czero
210 jjj = 1
  qq = u1 + u2
  n = 123
  dx = rmax - rmin
  d11 = dx / 2.0d0
  sq1 = qq * cmplxz (d11, fzero)
  if (iprs47 .ge. 6) write (unit = logsix, fmt = 3943) dx, htoj2, rmin, omg2, toj, value5, qq
3943 format (/, 1x, 14x, 'dx', 11x, 'htoj2', 12x, 'rmin', 12x, 'omg2', 13x, 'toj', 10x, 'value5', 9x, 'real-qq', 9x, 'imag-qq', /, 1x, 8e16.7)
75 dx2 = dx
  dx = dx / 2.0d0
  ram = rmin + dx
  n = 2 * n
  if (jjj .eq. 1) n = 1
  jjj = 100
  do kn = 1, n
     ram2 = ram * ram
     d2 = ram2 + omg2 * (1.0d0 - htoj2 * hyud2) / hkr
     c1 = cmplxz (d2, fzero)
     c2 = rom * cmplxz(htoj2, fzero)
     s1 = csqrtz (c1 + c2)
     d3 = ram2 + omg2 * (1.0d0 - htoj3 * hyud3) / hkr
     c3 = cmplxz (d3, fzero)
     c4 = cmplxz (alf1, fzero) * rom * cmplxz (htoj3, fzero)
     s2 = csqrtz (c3 + c4)
     d4 = ram2 + omg2 * (1.0d0 - htoj4 * hyud4) / hkr
     c5 = cmplxz (d4, fzero)
     c6 = cmplxz (alf2, fzero) * rom * cmplxz (htoj4, fzero)
     s3 = csqrtz (c5 + c6)
     s6 = s2
     s5 = s1
     d5 = 1.0d0 / htoj2 / toj
     s1 = s1 * cmplxz (d5, fzero)
     d6 = 1.0d0 / htoj3 / toj
     s2 = s2 * cmplxz (d6, fzero)
     d7 = 1.0d0 / htoj4 / toj
     s3 = s3 * cmplxz (d7, fzero)
     s8 = cmplxz (toj, fzero) * s1
     sp12 = s1 + s2
     sp23 = s2 + s3
     sm12 = s1 - s2
     d8 = 2.0d0 * (dep1 - dep2)
     c7 = cmplxz (d8, fzero)
     sm23 = (s2 - s3) * cexpz (s6 * c7)
     d9 = -2.0d0 * dep1
     c8 = cmplxz (d9, fzero)
     bbb = cexpz (s5 * c8)
     d10 = expz (-h1ph2 * ram) * cosz (e * ram)
     z = cmplxz (fzero, contwo) * (sp12 * sp23 + sm12 * sm23 + bbb * (sm12 * sp23 + sp12 * sm23)) * cmplxz (d10, fzero) / ((cmplxz (ram, fzero) + s8) * (sp12 * sp23 + sm12 * sm23) + (cmplxz (ram, fzero) - s8) * bbb * (sm12 * sp23 + sp12 * sm23))
     ram = ram + dx2
     u = 2.0d0 * z
     qq = qq + u
  end do
  d12 = dx / 2.0d0
  sq3 = qq * cmplxz (d12, fzero)
  sa = sq1 - sq3
  r = cabsz (sa) / cabsz (sq3)
  v5 = value5 * 0.5d0
  if (iprs47 .ge. 24) write (unit = logsix, fmt = 3956) n, r, dx, ram, qq
3956 format (/, ' Bottom loop.       n', 19x, 'r', 18x, 'dx', 17x, 'ram', 13x, 'real-qq', 13x,  'imag-qq', /, 13x, i8, 5e20.11)
  if (r .le. v5) go to 50
  sq1 = sq3
  go to 75
50 s = s + sq3
  if (jd .eq. 100) go to 55
  go to 600
55 c9 = 0.5d0 * u0 / pai
  s = s * cmplxz (omg, fzero) * cmplxz (c9, fzero)
  if (iprs47 .ge. 2) write (unit = logsix, fmt = 3968) omg, value2, s
3968 format (/, " Exit  'zest' .", 17x, 'omg', 14x, 'value2', 14x, 'real-s', 14x, 'imag-s', /, 15x, 4e20.11)
  return
end subroutine zest

!
! subroutine minv.
!

subroutine minv (tcmpx, m, f, ldn, ldn2)
  use blkcom
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: ldn2
  integer(4), intent(in) :: m
  complex(16), intent(out) :: tcmpx(ldn, ldn)
  complex(16), intent(in) :: f(ldn, ldn2)
  !  dimension fr(10,20), fi(10,20), fnew(10,20), fident(10,20)
  integer(4) :: i, i2
  integer(4) :: j, j1
  integer(4) :: k, k1
  integer(4) :: ll0
  integer(4) :: m1
  integer(4) :: n
  real(8) :: adi, adr
  real(8) :: cci, ccr
  real(8) :: d1, d5, d6, d9, d14, d16, d18, d19, d22, d23, di, dr
  real(8) :: fr(10, 20), fi(10, 20)
  complex(16) :: d2
  complex(16) :: fident(10, 20), fnew(10, 20)
  !
  d1 = 0.0d0
  do i = 1, m
     do j = 1, m
        d2 = tcmpx(i, j)
        if (cabsz (d2) .gt. d1) d1 = cabsz (d2)
        fr(i, j) = realz (d2)
        fi(i, j) = aimagz (d2)
     end do
  end do
  !!!!    4 f(i,j) = d2
  d1 = d1 * value2
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 4005) m, value2, d1, f(1, 1)
4005 format (/, " Start  'minv' .       m", 14x, 'value6', 18x, 'd1', 9x, 'real-f(1,1)', 9x, 'imag-f(1,1)', /, 16x, i8, 4e20.11, /, ' diagnostic   tcmpx(i,j)  for  (i,j)=1, ... m .')
  ll0 = 0
  if (iprs47 .ge. 7) call print (tcmpx(1, 1), m, ll0, ldn)
  do i = 1, m
     j1 = m + 1
     m1 = m * 2
     do j = j1, m1
        if ((j - m) .eq. i) go to 20
!!!!     f(i,j) = czero
        fr(i, j) = 0.0d0
        fi(i, j) = 0.0d0
        cycle
!!!!  20 f(i,j) = creal1
20      fr(i, j) = 1.0d0
        fi(i, j) = 0.0d0
     end do
  end do
  do k1 = 1, m
     d9 = 0.0d0
     n = k1
     do i2 = k1, m
        !!!!     bx=cabsz(f(i2,k1))
        !!!!     if (bx .le. d9)   go to 50
        d18 = absz (fr(i2, k1))
        d19 = absz (fi(i2, k1))
        if (d18 .lt. d19) d18 = d19
        if (d18 .le. d9) cycle
        d9 = d18
        n = i2
     end do
     !      write (*,*) ' next variable.  k1, n, d9, d1 =',
     !     1                              k1, n, d9, d1
     if (d9 .gt. d1) go to 60
     write (unit = lunit(6), fmt = 5706) m, k1, d9
5706 format (/, " Stop. ---- Matrix inversion within subroutine  'minv'  has been suspended, due to failure to find a large", /, 12x, ' enough pivot element.   The matrix in question is of order', i5, ' ,    with breakdown having occurred', /, 12x, ' while working on the elimination of variable number', i5, '.   In this columns, the largest real or', /, 12x, ' imaginary part had absolute value', e14.3, ' ,   while the applicable near-zero tolerance')
     write (unit = lunit(6), fmt = 5707) d1, value2
5707 format (12x, 'is equal to', e14.3, ' .    This latter number is equal to', e14.3, '   times the largest', /, 12x, 'element of the original input matrix (considering absolute values).')
     stop
60   if (n .eq. k1) go to 75
     do j = k1, m1
        !!!!     cc=f(k1,j)
        ccr = fr(k1, j)
        cci = fi(k1, j)
!!!!     f(k1,j)=f(n,j)
        fr(k1, j) = fr(n, j)
        fi(k1, j) = fi(n, j)
!!!!   70 f(n,j)=cc
        fr(n, j) = ccr
        fi(n, j) = cci
     end do
75   do i = 1, m
        if (i .eq. k1) cycle
!!!!     d = f(i,k1)
        dr = fr(i, k1)
        di = fi(i, k1)
        do j = k1, m1
!!!!  f(i,j) = f(i,j) - d/f(k1,k1) * f(k1,j)
           d14 = fr(k1, k1) ** 2 + fi(k1, k1) ** 2
           d22 = dr * fr(k1, k1) + di * fi(k1, k1)
           d23 = di * fr(k1, k1) - dr * fi(k1, k1)
           d5 = d22 * fr(k1, j) - d23 * fi(k1, j)
           d6 = d22 * fi(k1, j) + d23 * fr(k1, j)
           fr(i, j) = fr(i, j) - d5 / d14
           fi(i, j) = fi(i, j) - d6 / d14
           !      write (*,*) ' revise f(i,j).  i, j, k1, f(i,j) =',
           !     1                              i, j, k1, fr(i,j), fi(i,j)
        end do
     end do
!!!!     ad = f(k1, k1)
     adr = fr(k1, k1)
     adi = fi(k1, k1)
     d14 = adr ** 2 + adi ** 2
     adr = adr / d14
     adi = -adi / d14
     do j = k1, m1
        !!!! 110 f(k1,j) = f(k1,j)/ad
        d16 = fr(k1, j) * adr - fi(k1, j) * adi
        fi(k1, j) = fr(k1, j) * adi + fi(k1, j) * adr
        fr(k1, j) = d16
     end do
     !  110 write (*,*) ' new  f(k1,j).   k1, j, fr(k1,j), fi(k1,j) =',
     !     1                              k1, j, fr(k1,j), fi(k1,j)
  end do
  do i = 1, m
     do j = 1, m
        j1 = j + m
        fnew(i, j) = cmplxz (fr(i, j1), fi(i, j1))
     end do
  end do
  do i = 1, m
     do j = 1, m
        fident(i, j) = czero
        do k = 1, m
           fident(i, j) = fident(i, j) + tcmpx(i, k) * fnew(k, j)
        end do
     end do
  end do
  !      write (*,*) ' minv, [a]*[a]-1  follows.   m =',  m
  !      do 725  i = 1, m
  !  725 write (*,*) ' row', i, ( fident(i,j), j=1, m )
  do i = 1, m
     do j = 1, m
        j1 = j + m
!!!! 125 tcmpx(i,j) = f(i,j1)
        tcmpx(i, j) = cmplxz (fr(i, j1), fi(i, j1))
     end do
  end do
  !  125 write (*,*) ' transfer.',
  !     1  ' i, j, j1, fr(i,j1), fi(i,j1), tcmpx(i,j) =',
  !     2    i, j, j1, fr(i,j1), fi(i,j1), tcmpx(i,j)
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 4027) tcmpx(1,1), tcmpx(1, 2)
4027 format (/, " Exit  'minv'  normally.", 5x, 'real-tcmpx(1,1)', 5x, 'imag-tcmpx(1,1)', 5x, 'real-tcmpx(1,2)', 5x, 'imag-tcmpx(1,2)', /, 24x, 4e20.11, /, ' diagnostic inverse.   tcmpx(i,j)  for  (i,j)=1, ... m.')
  if (iprs47 .ge. 7) call print (tcmpx(1, 1), m, ll0, ldn)
  return
end subroutine minv

!
! subroutine mxm.
!

subroutine mxm (xm, yym, c, n, ldn)
  use com47
  implicit none
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: n
  complex(16), intent(in) :: xm(ldn, ldn)
  complex(16), intent(in) :: yym(ldn, ldn)
  complex(16), intent(out) :: c(ldn, ldn)
  integer(4) :: i
  integer(4) :: j
  integer(4) :: k
  integer(4) :: ll0
  !
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 4062) n, xm(1, 1), yym(1, 1)
4062 format (/, " Enter  'mxm' .       n", 7x,  'real- xm(1,1)',  7x, 'imag- xm(1,1)', 7x, 'real-yym(1,1)', 7x, 'imag-yym(1,1)', /, 15x, i8, 4e20.11, /, ' diagnostic left factor.   xm(i,j)  for  (i,j)=1, ... n .')
  if (iprs47 .lt. 8) go to 4079
  ll0 = 0
  call print (xm(1, 1), n, ll0, ldn)
  write (unit = logsix, fmt = 4073)
4073 format (/, ' Diagnostic right factor.  yym(i,j)  for  (i,j)=1, ... n .')
  call print (yym(1, 1), n, ll0, ldn)
4079 do i = 1, n
     do j = 1, n
        c(i, j) = czero
        do k = 1, n
           c(i, j) = c(i, j) + xm(i, k) * yym(k, j)
        end do
     end do
  end do
  if (iprs47 .ge. 1) write (unit = logsix, fmt = 4091) c(1, 1), c(1, 2)
4091 format (/, " Exit  'mxm' .", 9x, 'real-c(1,1)', 9x, 'imag-c(1,1)', 9x, 'real-c(1,2)', 9x, 'imag-c(1,2)', /, 14x, 4e20.11, /, ' diagnostic product.   c(i,j)  for  (i,j)=1, ... n .')
  if (iprs47 .ge. 8) call print (c(1, 1), n, ll0, ldn)
  return
end subroutine mxm

!
! subroutine print.
!

subroutine print (c, n, iform, ldn)
  use blkcom
  use com47
  use tracom
  implicit none
  integer(4), intent(in) :: iform
  integer(4), intent(in) :: ldn
  integer(4), intent(in) :: n
  complex(16), intent(in) :: c(ldn,ldn)
  !  dimension  workr(8), worki(8)
  character(8) :: text1, text2, text3
  integer(4) :: i, im, in
  integer(4) :: j
  integer(4) :: k
  integer(4) :: l
  integer(4) :: m
  integer(4) :: nline
  real(8) :: worki(8), workr(8)
  !
  data text1 / 'row' /
  data text2 / '   ' /
  nline = (n + 7) / 8
  do i = 1, n
     text3 = text2
     im = -7
     do k = 1, nline
        im = im + 8
        in = im + 7
        if (in .gt. n) in = n
        l = 0
        do j = im, in
           l = l + 1
           workr(l) = realz (c(i, j))
           worki(l) = aimagz (c(i, j))
        end do
        if (k .eq. nline) text3 = text1
        if (iform .eq. 1) go to 4658
        write (unit = lunit(6), fmt = 4649) text3, (workr(m), m = 1, l)
4649    format (1x, a3, 8e16.7)
        write (unit = lunit(6), fmt = 4651) (worki(m), m = 1, l)
4651    format (4x, 8e16.7)
        cycle
4658    write (unit = lunit(6), fmt = 4659) text3, (workr(m), m = 1, l)
4659    format (1x, a3, 8f16.7)
        write (unit = lunit(6), fmt = 4661) (worki(m), m = 1, l)
4661    format (4x, 8f16.7)
     end do
     write (unit = lunit(6), fmt = 4632) i
  end do
4632 format ('+', i2, /, 1x )
  return
end subroutine print

!
! end of file over47.f90
!
