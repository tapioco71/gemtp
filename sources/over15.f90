!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over15.f90
!

module ovr15c
  use blkcom
  use labcom
  use umcom
  implicit none
  save

contains

  !
  ! subroutine uminit.
  !

  subroutine uminit (n15, reacl, gpar, fpar, hist,umcurp, nodvo1, nodvo2, jcltac, jclout, jtype, nodom, jtmtac, histom, omegm, omold, thetam, reamdu, reamds, flxds, flxdr, reamqu,flxqs, flxqr, jcdsat, jcqsat, flxd, flxq, nppair, rotmom, ncld, nclq, jtqout, jomout, jthout, reamqs, epsom, dcoef,kcoil, voltum, anglum, nodfum, nodmum, kumout, jumout, umoutp)
    implicit none
    integer(4), intent(in) :: jcdsat(:)
    integer(4), intent(in) :: jcqsat(:)
    integer(4), intent(in) :: jclout(:)
    integer(4), intent(out) :: jcltac(:)
    integer(4), intent(in) :: jomout(:)
    integer(4), intent(in) :: jthout(:)
    integer(4), intent(in) :: jtmtac(:)
    integer(4), intent(in) :: jtqout(:)
    integer(4), intent(in) :: jtype(:)
    integer(4), intent(in) :: kcoil(:)
    integer(4), intent(in) :: jumout(:)
    integer(4), intent(in) :: kumout(:)
    integer(4), intent(out) :: n15
    integer(4), intent(in) :: ncld(:)
    integer(4), intent(in) :: nclq(:)
    integer(4), intent(in) :: nodfum(:)
    integer(4), intent(in) :: nodmum(:)
    integer(4), intent(in) :: nodom(:)
    integer(4), intent(in) :: nodvo1(:)
    integer(4), intent(in) :: nodvo2(:)
    integer(4), intent(in) :: nppair(:)
    real(8), intent(in) :: anglum(:)
    real(8), intent(in) :: dcoef(:)
    real(8), intent(in) :: epsom(:)
    real(8), intent(in) :: flxd(:)
    real(8), intent(in) :: flxq(:)
    real(8), intent(in) :: flxdr(:)
    real(8), intent(in) :: flxqr(:)
    real(8), intent(in) :: flxds(:)
    real(8), intent(in) :: flxqs(:)
    real(8), intent(in) :: fpar(:)
    real(8), intent(in) :: gpar(:)
    real(8), intent(in) :: hist(:)
    real(8), intent(in) :: histom(:)
    real(8), intent(in) :: omegm(:)
    real(8), intent(in) :: omold(:)
    real(8), intent(in) :: reacl(:)
    real(8), intent(in) :: reamds(:)
    real(8), intent(in) :: reamqs(:)
    real(8), intent(in) :: reamdu(:)
    real(8), intent(in) :: reamqu(:)
    real(8), intent(in) :: rotmom(:)
    real(8), intent(in) :: thetam(:)
    real(8), intent(in) :: umcurp(:)
    real(8), intent(in) :: umoutp(:)
    real(8), intent(in) :: voltum(:)
    !
    !  dimension reacl(1), gpar(1), fpar(1), hist(1), umcurp(1)
    !  dimension nodvo1(1), nodvo2(1), jcltac(1), jclout(1)
    !  dimension jtype(1), nodom(1), jtmtac(1), histom(1)
    !  dimension omegm(1), omold(1), thetam(1)
    !  dimension reamdu(1), reamds(1), flxds(1), flxdr(1)
    !  dimension reamqu(1), flxqs(1), flxqr(1)
    !  dimension jcdsat(1), jcqsat(1), flxd(1), flxq(1)
    !  dimension nppair(1), rotmom(1), ncld(1)
    !  dimension nclq(1), jtqout(1), jomout(1), jthout(1)
    !  dimension reamqs(1), epsom(1), dcoef(1), kcoil(1)
    !  dimension voltum(1), anglum(1), nodfum(1), nodmum(1)
    !  dimension kumout(1), jumout(1), umoutp(1)
    !  dimension nsubkm(1)
    !  equivalence (kknonl(1), nsubkm(1))
    !
    integer(4) :: j
    integer(4) :: k, kcl
    integer(4) :: mark, mars, mm
    integer(4) :: n1, n2, n3, n4, n5, n6, n7, n8, n9, n16, nclcom, nshare, num, numcom
    !
    !  jcltac(kcl and kcl+1) are defined and initialized in umrenu
    !  jcltac(kcl+2) is defined here. it is zero unless a set of
    !     of max 3 um's sharing a common mech netw is dealt with.
    !   * it is set to the um nr if the um with this kcl is the
    !     lowest numbered um in this set. this um takes care of
    !     the multi- phase compensation of the common mech netw.
    !   * it is set to the negative value of the "cursub" entry if
    !     it is not the lowest numbered um in this set.
    !  nshare is a flag to decide how mech networks of each um are
    !     to be compensated.
    !     nshare = 1  : no comp of mech network in case that it
    !                 has been done earlier by a mechanically
    !                 connected lower-numbered um
    !     nshare = 0 : um not mechanically connected to other um's
    !                 and hence single-phase comp as usual.
    !     nshare = 10 : compensation of common mech netwrk to be
    !                   conducted with the lowest numbered um.
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 4567)
4567 format ('  "Begin module uminit."')
    istart = 0
    ksubum = numsub + 1
    nclcom = n15 + 5
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 7302) ksubum
7302 format (/, ' Begin  "uminit" . ksubum =', i4)
    !  ncomp is the max number of compensated phases per subnetwork.
    !  n8 is used to determine the value of this ncomp.
    do k = 1, numum
       kcl = kcoil(k)
       jcltac(kcl + 2) = 0
       n2 = jcltac(kcl)
       n3 = jcltac(kcl + 1)
       n4 = k
       if (n2 .eq. 0 .and. n3 .eq. 0) go to 7010
       if (n2 .ne. 0 .and. n2 .lt. n4) n4 = n2
       if (n3 .ne. 0 .and. n3 .lt. n4) n4 = n3
       if (n4 .eq. k) jcltac(kcl+2) = k
    end do
7010 continue
    do k = 1, numum
       kcl = kcoil(k)
       nshare = jcltac(kcl) + jcltac(kcl + 1)
       if (nshare .ne. 0) nshare = 1
       if (jcltac(kcl + 2) .gt. 0) nshare = 10
       !  compensation of power network *******************************
       n8 = 0
       numsub = numsub + 1
       isubeg(numsub) = n15 + 5
       n5 = kcl
       n6 = n5 + 2
       n16 = 0
       numcom = k
       if (loopss(8) .eq. 1) go to 7310
       j = n5
       mars = 1
1111   if (nodvo1(j) .eq. nodvo2(j)) go to 7305
6666   n8 = n8 + 1
       n15 = n15 + 5
       nsubkm(n15) = n15 + 5
6767   nsubkm(n15+1) = nodvo1(j)
       nsubkm(n15+2) = nodvo2(j)
       ! code section repeatedly called for each compensated node-pair:
6868   nsubkm(n15 + 3) = -k
       nsubkm(n15 + 4) = mars
       n16 = 1
       nclcom = nclcom + 1
       num = 0
       mark = kpsour(nsubkm(n15 + 1))
1010   if (mark .eq. 0 .or. mark .eq. numsub) go to 1515
       if (mark .lt. ntot) go to 7777
       do mm = 2, kpartb
          if (kpsour(mm) .eq. mark) kpsour(mm) = numsub
       end do
1515   if (num .gt. 0) go to 1616
       num = 1
       mark = kpsour(nsubkm(n15 + 2))
       go to 1010
7777   write (unit = lunit6, fmt = 8888) k
8888   format (/, ' Error stop. Something has gone wrong with setting up the compensation tables in module "uminit" for um -', i4, '. Consult EMTP', /, ' management after making sure that you did not violate the um rule regarding the three-phase restriction.')
       kill = 229
       n15 = isubeg(mark)
       lstat(15) = k
       lstat(16) = nsubkm(n15 + 3)
       go to 9800
       !1616 go to (7305, 7312, 7318, 7322), mars
1616   select case (mars)
       case (1)
          go to 7305

       case (2)
          go to 7312

       case (3)
          go to 7318

       case (4)
          go to 7322
       end select
       !  end code section ............................................
7305   j = j + 1
       if (j .le. n6) go to 1111
       !  The following condition is needed for the last node-pair
       !    to be compensated for this subnetwork. this condition is
       !    to be inserted for all the other subnetworks.
       if (n16 .eq. 1) nsubkm(n15) = isubeg(numsub)
       if (n8 .gt. ncomp) ncomp = n8
       !  compensation of excit network for 3 or less coils ***********
       n8 = 0
       if (n16 .eq. 0) go to 7310
       numsub = numsub + 1
       isubeg(numsub) = n15 + 5
7310   n16 = 0
       n6 = n5 + 2 + ncld(k) + nclq(k)
       if (jtype(k) .eq. 4) n6 = n6 + 1
       n7 = n6 - n5 + 1
       n9 = n6
       if (n7 .gt. 6) n9 = n5 + 5
       n5 = n5 + 3
       j = n5
       mars = 2
2222   if (j .gt. n9) go to 2323
       if (nodvo1(j) .ne. nodvo2(j)) go to 6666
7312   j = j + 1
       go to 2222
2323   if (n16 .eq. 1) nsubkm(n15) = isubeg(numsub)
       if (n8 .gt. ncomp) ncomp = n8
       !  Compensation of excit network for 4 or more coils ***********
       !    note : in this case they will all be single-phase comp
       n8 = 0
       if (n7 .le. 6) go to 7319
       n5 = n5 + 3
       j = n5
       mars = 3
3333   if (j .gt. n6) go to 3434
       if (nodvo1(j) .eq. nodvo2(j)) go to 7318
       if (n16 .eq. 0) go to 7316
       numsub = numsub + 1
       isubeg(numsub) = n15 + 5
7316   n15 = n15 + 5
       n8 = 1
       nsubkm(n15) = n15
       go to 6767
7318   j = j + 1
       go to 3333
3434   if (n8 .gt. ncomp) ncomp = n8
       !  Compensation of mech network ********************************
7319   n8 = 0
       if (jtmtac(k) .ne. 0) go to 7328
       if (nshare .eq. 1) go to 7328
       mars = 4
       if (n16 .eq. 0) go to 7320
       numsub = numsub + 1
       isubeg(numsub) = n15 + 5
7320   n15 = n15 + 5
       n8 = n8 + 1
       nsubkm(n15) = n15 + 5
       nsubkm(n15+1) = nodom(numcom)
       nsubkm(n15+2) = 1
       go to 6868
       !  Compensation of higher numbered um if mechanically connected
7322   if (nshare .eq. 0) go to 7325
       if (nshare .eq. -1) go to 7324
       nshare = 0
       if (jcltac(kcl + 1) .ne. 0) nshare = - 1
       numcom = jcltac(kcl)
       if (numcom .eq. 0) go to 7324
       n1 = kcoil(numcom)
       jcltac(n1 + 2) = -nclcom
       go to 7320
7324   numcom = jcltac(kcl + 1)
       nshare = 0
       n1 = kcoil(numcom)
       jcltac(n1 + 2) = -nclcom
       go to 7320
7325   if (n16 .eq. 1) nsubkm(n15) = isubeg(numsub)
       if (n8 .gt. ncomp) ncomp = n8
7328   if (n16 .eq. 0)  numsub = numsub - 1
    end do
9800 if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
4568 format ('  "Exit  module uminit."')
    return
  end subroutine uminit

end module ovr15c

!
! subroutine over15.
!

subroutine over15
  use blkcom
  use labcom
  use smtacs
  use smach
  use umcom
  use space2
  use tacsar
  use tracom
  use bcdtim
  use movcop
  use ovr15c
  implicit none
  character(8) :: text1, text2, text3
  character(8) :: text4, text5, text6, text7
  character(8) :: text8, text9, text10, text11, text12
  character(132) :: outlin
  integer(4) :: i, i3, ichar, ijk, ik, ip, iprint
  integer(4) :: j, jk
  integer(4) :: k, k1, kprsta, kswpe4
  integer(4) :: l, lunit6save
  integer(4) :: m, moon, mpower, mpr
  integer(4) :: n1, n2, n3, n4, n5, n6, n8, n9, n11, n12, n13, n14, n15, n16
  integer(4) :: n17, n18, n19, n23, n44, ndx1, ndx2, ndx4, nk1, nk2, ntacs
  integer(4) :: ncsave, numbco
  real(8) :: d1, d2, d6, d11
  real(8) :: sm
  equivalence (d2, sm)
  !  dimension ispum(1)
  !  dimension aupper(13), alower(13)
  !  dimension nsubkm(1)
  !
  !  equivalence (spum(1), ispum(1))
  !  equivalence (moncar(1), knt), (moncar(2), kbase)
  !  equivalence (moncar(3), ltdelt), (moncar(4), isw)
  !  equivalence (moncar(5), idist), (moncar(6), itest)
  !  equivalence (kknonl(1), nsubkm(1))
  !
  data  text1   / 'tacs  ' /
  data  text4   / 'normal' /
  data  text5   / '      ' /
  data  text6   / 'unifor' /
  data  text7   / 'm     ' /
  data  text8   / ' middl' /
  data  text9   / 'e     ' /
  data  text10  / 'minimu' /
  data  text11  / 'm     ' /
  data  text12  / 'chan01' /
  !     transfer to  "top15"  for front end of overlay 15.
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4567)
4567 format ('  "Begin module over15."')
  call move0 (kssfrq, ntot)
  call top15
  if (kill .gt. 0) go to 9200
  go to 3038
  !     read user-supplied names for emtp node-voltage output.  ----------
  !     read input card using cimage.
1030 call cimage
3038 continue
  read (unit = abuff, fmt = 3043) ijk, (aupper(i), i = 1, 13)
3043 format (i2, 13a6)
  if (ijk .ne.  1) go to 8211
  numnvo = ntot1
  ivolt = ijk
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 3045)
3045 format ('+Request for output of all node voltages.')
  call interp
  if (aupper(1) .eq. blank) go to 3094
  go to 1030
8211 l = 0
  if (aupper(1) .ne. text12) go to 4693
  !     honor "chan01" in cols. 3-8 by building dummy node voltage
  !     channels with names "chan01", "chan02", etc.:
  read (unit = abuff, fmt = 4671) n13
4671 format (8x, i8)
  if (n13 + ntot .le. lbus .and. n13 .gt. 0) go to 4682
4675 write (unit = lunit6, fmt = 4676) n13, ntot, lbus
4676 format ('   = = =  No, too many dummy node voltage output channels for emtp dimensions.', /, 'n13 + ntot .gt. lbus,   where   n13, ntot, lbus =', 3i8, '     If execution is', /, '          interactive,  deposit  revised  n13  value in  istep  at next spy break.')
  call spying
  n13 = istep
  istep = 0
4682 if (noutpr .eq. 0) write (unit = kunit6, fmt = 4683) n13
4683 format ('+Dummy node voltage output number', i8)
  do j = 1, n13
     call nmincr (text12, j)
     bus(ntot + j) = text12
     numnvo = numnvo + 1
     if (numnvo .gt. lsiz12) go to 4675
     e(ntot + j) = 0.0d0
     ibsout(numnvo) = ntot + j
  end do
  go to 1030
!!!!  4693 do 8288 i = 1, 13
4693 if (ijk .ne. -5) go to 1830
!!!!  write (*,*) ' begin search for branch voltages.   nv =', nv
  do i = 1, 11, 2
     if (aupper(i) .eq. blank .and. aupper(i + 1) .eq. blank) go to 4962
     do jk = 1, 2
        bus1 = aupper(i + jk - 1)
!!!!  write (*,*) ' ready to check next name.  jk, bus1 =',  jk, bus1
        do ip = 1, ntot
           if (bus1 .ne. bus(ip)) go to 2801
!!!!  write (*,*) ' match found.   jk, ip =',  jk, ip
           if (jk .eq. 1) ibrnch(nv + 1) = ip
           if (jk .eq. 2) jbrnch(nv + 1) = ip
           if (jk .eq. 1) go to 5183
           nv = nv + 1
!!!!  write (*,*) ' done assigning v-branch out.   nv, ibrnch(nv),',
!!!! 1            ' jbrnch(nv) =',   nv, ibrnch(nv), jbrnch(nv)
           go to 4962
        end do
2801    continue
        if (noutpr .eq. 0) write (unit = lunit6, fmt = 6925) bus1
6925    format (5x, 'ignore v-branch request involving nonexistent node  "', a6, '"')
        go to 4962
     end do
5183 continue
  end do
4962 continue
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 3872)
3872 format ('+card of name pairs for branch voltages.')
  go to 1030
1830 do i = 1, 13
     if (ijk .lt. 0) go to 8215
     bus1 = aupper(i)
     if (bus1 .eq. blank) go to 8288
     l = 1
     do ik = 2, ntot
        if( bus1 .eq. bus(ik) )  go to 3056
     end do
     if (noutpr .eq. 0) write (unit = lunit6, fmt = 3053) bus1
3053 format (5x, 'Request for voltage output of nonexistent node ', "'", a6, "'", ' will be ignored.')
     go to 8288
3056 if (ivolt .eq. 1) go to 8888
     numnvo = numnvo + 1
     ibsout(numnvo) = ik
     if (numnvo .le. lsiz12) go to 8288
     iprint = 11
     lstat(19) = 3056
     go to 9000
8888 write (unit = lunit6, fmt = 8899)
8899 format (' There has been the request for all node voltage output, so this request will be ignored.')
     go to 1030
8215 l = 1
     if (aupper(i) .eq. blank) go to 8288
     do j = 1, ibr
        ip = namebr(j)
        if (texvec(ip) .eq. aupper(i)) go to 8231
     end do
     do j = 1, inonl
        ip = namenl(j)
        if (texvec(ip) .eq. aupper(i)) go to 8233
     end do
     do j = 1, kswtch
        ip = namesw(j)
        if (texvec(ip) .eq. aupper(i)) go to 8235
     end do
     write (unit = lunit6, fmt = 8229 )
8229 format ('     Request for branch output of nonexistent branch will be ignored')
     go to 8288
8311 iprint = 11
     lstat(19) = 8237
     go to 9000
8333 iprint = 18
     lstat(19) = 8237
     go to 9000
8231 jk = 1111
     nk1 = iabs (kbus(j))
     nk2 = iabs (mbus(j))
     if (ijk .ne. -2) mbus(j) = -nk2
     go to 8237
8235 jk = 3333
     nk1 = kmswit(j)
     nk2 = kmswit(j + lswtch)
     if (ijk .ne. -2) kpos(j) = -iabs (kpos(j))
     go to 8237
8233 jk = 2222
     nk1 = nonlk(j)
     nk2 = iabs (nonlm(j))
     if (ijk .ne. -2) nonlm(j) = -nk2
8237 if (ijk .gt. -2) go to 8288
     nv = nv + 1
     if (nv .gt. lsiz12) go to 8311
     if (ijk .ge. -3) go to 8260
     npower = npower + 1
     if (npower .gt. maxpe) go to 8333
     mpower = npower + maxpe
     koutvp(npower) = nv
     if (jk .eq. 3333) koutvp(npower) = -nv
     koutvp(mpower) = j
     if (jk .eq. 2222) koutvp(mpower) = -j
     if (jk .eq. 3333 .and. kswpe4 .eq. 0) kswpe4 = npower
     if (iprsup .gt. 2) write (unit = lunit6, fmt = 8244) npower, maxpe, koutvp(npower), koutvp(mpower)
8244 format (/, ' Power output request', 4i10)
8260 if (jk .ne. 2222 .or. nk1 .le. nk2) go to 8262
     if (iabs (nltype(j)) .ne. 99) go to 8262
     ibrnch(nv) = nk2
     jbrnch(nv) = nk1
     go to 8288
8262 ibrnch(nv) = nk1
     jbrnch(nv) = nk2
  end do
8288 continue
  if (l .eq. 0) go to 3081
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 3072)
3072 format ('+Card of bus names for node-voltage output.')
  go to 1030
3081 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54167)
54167 format ('+Blank card ending node names for voltage output.')
  call interp
  if (nenerg .eq. 0) go to 3094
  write (unit = lunit6, fmt = 637) nenerg
637 format (//, ' The data case now ready to be solved is a statistical overvoltage study which involves ', i10, '   energizations')
  if (nenerg .lt. 0) go to 1329
  text2 = text4
  text3 = text5
  if (idist .eq. 0) go to 1310
  text2 = text6
  text3 = text7
1310 write (unit = lunit6, fmt = 1311) text2, text3
1311 format (' (parameter  "nenerg"  of columns 65-72, of the 2nd misc. data card).   Switch closing or opening times for the ', /, ' specially-flagged switches (with "statistics" punched in columns 55-64) are varied randomly, according to a ',  a6, a1, ' distribution.')
  write (unit = lunit6, fmt = 1324)
1324 format (" The user can select either uniform or normal distributions, based on the value of parameter  'idist'  of columns", /, ' 17-24 of the special statistics misc. data card.   The following is a listing of switches whose closing times are', /, ' to be statistically varied, along with the associated mean and standard deviation for the distribution, as requested by the user.')
  if (xmaxmx .lt. 0.0) write (unit = lunit6, fmt = 1325) xmaxmx
1325 format (' The user punched statistics miscellaneous data parameter  xmaxmx =',  e12.3, &
       ' ,   with the negative value representing   ', /, ' a request for use of the 100 built-in random, numbers.   As such, the answer is really deterministic, then (a second solution', /, ' on any computer should give the same answer).')
  go to 1336
1329 text2 = text8
  text3 = text9
  if (itest .gt. 0) go to 8329
  text2 = text10
  text3 = text11
8329 write (unit = lunit6, fmt = 1330) text2, text3
1330 format (' (parameter  "nenerg"  of columns 65-72 of the 2nd misc. data card).', /, " closing times for the specially-flagged switches (with  'systematic'  punched in columns 55-64) are varied", /, 1x, 'regularly between minimum and maximum times, with a uniform increment in between for each switch.   The following', /, 1x, 'is a listing of the switches whose closing times are to be systematically varied for this data case.   Also tabulated   ', /, 1x, 'are the associated ', a6, a1, '  closing times and step-increment sizes for the, switches, exactly as was read from the switch cards.')
1336 write (unit = lunit6, fmt = 1338)
1338 format (/, 10x, 'entry', 4x, 'switch', 6x, 'from', 8x, 'to', 9x, 'columns 15-24', 8x, 'columns 25-34', 7x, 'reference switch no.', /, 5x,  2(4x, 'number'),  2(7x, 'bus'), 10x, '(in seconds)', 9x, '(in seconds)', 5x, '(0 implies independence)')
  n1 = 0
  do i = 1, kswtch
     if (absz (adelay(i)) .ne. 44444.) go to 1352
     l = iabs (kmswit(i))
     ndx1 = lswtch + i
     m = iabs (kmswit(ndx1))
     n1 = n1 + 1
     d1 = absz (topen(ndx1))
     write (unit = lunit6, fmt = 1346) n1, i, bus(l), bus(m), crit(i), d1, kdepsw(i)
1346 format (5x, 2i10, 2(4x, a6), 2f21.6, 6x, i10)
  end do
1352 continue
  if (nenerg .lt. 0) go to 1373
  write (unit = lunit6, fmt = 1371) degmin, degmax, statfr
1371 format (" Now in addition to switch-time variation caused by each switch's own distribution, there is the added random", /, " delay which is the same for all switches, refered to by the term 'reference angle'.   Distribution for this", /, ' angle is uniform over the time interval from', e14.5, ' to', e14.5, ' degrees based on', e13.3, ' Hz', /, " frequency.   This was all specified by the user using fields  'degmin' ,  'degmax' ,  and  'statfr'  (cols. 41-64)", /, ' of the special statistics misc. data card.')
  write (unit = lunit6, fmt = 1372)
1372 format (" However, if the parameter 'itest' (found in col. 9-16 of the statistics misc. data card) is greater than zero", /, ' the reference angle is set to zero and, consequently, there is no added random delay.', /, 1x)
1373 write (unit = lunit6, fmt = 1374)
1374 format (' In the following printout, switch times which are used for each energization are tabulated, followed by peak', /, ' output-variable values which occur for the associated simulation.   Format for peak values is identical to the', /, ' printout for maximum variable values of a, conventional study')
  if (nenerg .gt. 0) write (unit = lunit6, fmt = 1362)
1362 format (' Except that printout of the, reference angle in degrees, has been added at the extreme left.')
3094 nc = nv
  if (iprsup .lt. 3) go to 7436
  write (unit = lunit6, fmt = 7427)
7427 format (/, ' Mixture of integer arrays in over15.   Only the longest column(s) will not have garbage at the ends.', /, 12x, 'row', 5x, 'bus1 ', 6x, 'bus2  ', 4x, 'kswtyp', 6x, 'kpos', 6x, 'node', 4x, 'isourc', 6x, 'kbus', 6x, 'mbus', 5x, 'nonlk', 5x, 'nonlm')
  n2 = kswtch
  if (kconst .gt. n2) n2 = kconst
  if (ibr .gt. n2) n2 = ibr
  if (inonl .gt. n2) n2 = inonl
  do k = 1, n2
     ndx1 = lswtch + k
     write (unit = lunit6, fmt = 7432) k, kmswit(k), kmswit(ndx1), kswtyp(k), kpos(k), node(k), isourc(k), kbus(k), mbus(k), nonlk(k), nonlm(k)
  end do
7432 format (5x, 11i10)
7436 iprint = 11
  if (iprsup .ge. 2) write (unit = lunit6, fmt = 8025) ibr, inonl, kswtch, nv, npower
8025 format (/, ' at 8025 ', 5i10)
  if (kswtch.eq.0) go to 912
  if (iprsup .ge. 2) write (unit = lunit6, fmt = 8026) (koutvp(l), l = 1, npower)
  ndx1 = maxpe + 1
  ndx2 = maxpe + npower
  if (iprsup .ge. 2) write (unit = lunit6, fmt = 8026) (koutvp(l), l = ndx1, ndx2)
8026 format (/, ' koutvp or koutie', /, (1x, 10i10))
  do k = 1, kswtch
     n3 = kpos(k)
     if (n3 .ge. 0) go to 907
     nc = nc + 1
     lstat(19) = 907
     if (nc .gt. lsiz12) go to 9000
     n2 = iabs (kmswit(k))
     ibrnch(nc) = n2
     ndx2 = lswtch + k
     jbrnch(nc) = iabs (kmswit(ndx2))
     if (iprsup .ge. 4)  write (unit = lunit6, fmt = 8028) k, nc, n3, n2
8028 format (/, ' at 8028 ', 4i10)
     do i = 1, npower
        mpower = maxpe + i
        if (koutvp(i) .gt. 0) go to 5438
        if (koutvp(mpower) .eq. k) go to 4385
     end do
5438 continue
     go to 907
4385 koutvp(mpower) = - nc
  end do
907 continue
912 if (inonl .eq. 0) go to 8411
  do k = 1, inonl
     icheck = nonlm(k)
     if (iprsup .gt. 3) write (unit = lunit6, fmt = 8029) k, icheck, nc
8029 format (/, ' at 8029 ', 3i10)
     if (icheck .ge. 0) go to 909
     nc = nc + 1
     lstat(19) = 8029
     if (nc .gt. lsiz12) go to 9000
     ibrnch(nc) = nonlk(k)
     jbrnch(nc) = -icheck
     do i = 1, npower
        mpower = maxpe + i
        if (koutvp(i) .lt. 0) go to 5439
        if (koutvp(mpower) .ge. 0) go to 5439
        if (koutvp(mpower) .eq. -k) go to 4398
     end do
5439 continue
     go to 909
4398 koutvp(mpower) = -(1000 + nc)
  end do
909 continue
  !     bring "n15" of "top15" out into "over15" using "nfrfld"
8411 n15 = nfrfld
  if (numum .gt. 0) call uminit (n15, spum(iureac :), spum(iugpar :), spum(iufpar :), spum(iuhist :), spum(iuumrp :), ispum(iunod1 :), ispum(iunod2 :), ispum(iujclt :), ispum(iujclo :), ispum(iujtyp :), ispum(iunodo :), ispum(iujtmt :), spum(iuhism :), spum(iuomgm :), spum(iuomld :), spum(iutham :), spum(iuredu :), spum(iureds :), spum(iuflds :), spum(iufldr :), spum(iurequ :), spum(iuflqs :), spum(iuflqr :), ispum(iujcds :), ispum(iujcqs :), spum(iuflxd :), spum(iuflxq :), ispum(iunppa :), spum(iurotm :), ispum(iuncld :), ispum(iunclq :), ispum(iujtqo :), ispum(iujomo :), ispum(iujtho :), spum(iureqs :), spum(iuepso :), spum(iudcoe :), ispum(iukcoi :), spum(iuvolt :), spum(iuangl :), ispum(iunodf :), ispum(iunodm :), ispum(iukumo :), ispum(iujumo :), spum(iuumou :))
  if (kill .gt. 0) go to 9200
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 7309) numsub, n15, ncomp
7309 format (/, ' Done setting comp. vectors.  numsub     n15   ncomp', /, 28x, 3i8)
  if (iprsup .lt. 2) go to 7321
  n11 = numsub + 1
  write (unit = lunit6, fmt = 7312) (isubeg(k), k = 1, n11)
7312 format (' isubeg :', 10i10)
  n12 = isubeg(numsub) + 4
  write (unit = lunit6, fmt = 7315) (nsubkm(k), k = 1, n12)
7315 format (' nsubkm :', 10i10)
  write (unit = lunit6, fmt = 7319) (kupl(k), k = 1, inonl)
7319 format (' kupl :', 10i10)
  write (unit = lunit6, fmt = 7320) (vecnl2(k), k = 1, inonl)
7320 format (' vecnl2 :', 10e12.4)
7321 n14 = lcomp * lbus
  n13 = ncomp * ntot
  if (n13 .le. n14) go to 7327
  kill = 1
  iprint = 24
  lstat(19) = 7321
  go to 9000
7327 do k = 1, ibr
     icheck = mbus(k)
     if (iprsup .ge. 3) write (unit = lunit6, fmt = 8030) k, icheck, nc
8030 format (' at 8030 of "over15", k, icheck, nc =', 3i6)
     if (icheck .ge. 0) go to 911
     nc = nc + 1
     lstat(19) = 8030
     if (nc .gt. lsiz12) go to 9000
     ibrnch(nc) = iabs (kbus(k))
     jbrnch(nc) = -icheck
     do i = 1, npower
        mpower = maxpe + i
        if (koutvp(i) .le. 0) go to 5540
        if (koutvp(mpower) .eq. k) go to 5415
     end do
5540 continue
     go to 911
5415 koutvp(mpower) = -nc
  end do
911 continue
  do i = 1, npower
     mpr = maxpe + i
     if (koutvp(mpr) .ge. 0) go to 5507
     n44 = iabs (koutvp(mpr))
     if (n44 .gt. 1000) n44 = n44 - 1000
     koutvp(mpr) = n44
  end do
5507 continue
  lstat(31) = nc
  if (ktab .gt. 0) go to 4752
  ioutcs = 0
4752 if (numsm .gt. 0) call smout
  lstat(37) = nst
  lstat(36) = mfirst
  if (kill .gt. 0) go to 9200
  n5 = nc + nsmout + ioutcs + numout
  n16 = 1
  if (numbus .gt. 0) n16 = numbus
  n9 = ioutcs + 1
  n23 = 0
  call namea6 (text1, n23)
  ntacs = ntot + n23
  n8 = ntot + maxbus
  n18 = n8 + n16
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 3207) nc, lsiz12, nsmout, ioutcs, ntot, numnvo, nv, n8, maxbus, n9, n18
3207 format (/, ' Before output-variable heading.      nc  lsiz12  nsmout  ioutcs    ntot  numnvo      nv      n8  maxbus      n9     n18', /, 32x, 12i8)
  if (n5 .le. lsiz12) go to 3204
  kill = 179
  lstat(19) = 3204
  lstat(14) = n5
  lstat(15) = nsmout
  lstat(16) = ioutcs
  lstat(13) = numout
  lstat(12) = nc
  go to 9200
3204 ncsave = nc
  if (nenerg .ne. 0 .and. kbase .ne. intinf) go to 3224
  i = 0
  i3 = 0
3206 i = i + 1
  if (i .gt. nsmout) go to 3209
  nc = nc + 1
  i3 = i3 + 3
  ibrnch(nc) = ismout(i3 - 1) + ntot
  jbrnch(nc) = ismout(i3) + ntot
  go to 3206
3209 i = 0
3211 i = i + 1
  if (i .gt. ioutcs) go to 3218
  nc = nc + 1
  ibrnch(nc) = ntacs
  ik = jout(kjout + i)
  jbrnch(nc) = ntot + ilntab(klntab + ik)
  go to 3211
3218 if (numout .le. 0) go to 3224
  n17 = iukumo - 1
  n19 = iujumo - 1
  do i = 1, numout
     nc = nc + 1
     ndx1 = n17 + i
     ndx2 = n19 + i
     ibrnch(nc) = ispum(ndx1) + n8
     jbrnch(nc) = ispum(ndx2) + n8
  end do
3224 numbco = nc - nv
  lstat(31) = nc
  k = numnvo + nc
  n2 = lsiz26
  if (iaverg .eq. 0) n2 = n2 + n2
  if (k .le. n2) go to 3098
  kill = 1
  lstat(16) = 26
  lstat(19) = 3098
  go to 9200
3098 lstat(32) = k
  if (begmax(1) .le. 0.0) go to 1061
  if (k .le. lsiz12) go to 1063
  iprint = 12
  lstat(19) = 1063
  go to 9000
  !     next initialize extrema vector  "xmax" .   there are 4
  !     partitions of size  list 12:  (xmax tmax xmin tmin) .
1063 do i = 1, k
     xmax(i) = -fltinf
     ndx1 = lsiz12 + i
     xmax(ndx1) = 0.0
     ndx1 = lsiz12 + ndx1
     xmax(ndx1) = fltinf
     ndx1 = lsiz12 + ndx1
     xmax(ndx1) = 0.0d0
  end do
1061 call runtym (d1, d2)
  limstp = kprchg(indstp)
  flstat(7) = flstat(7) - d1
  flstat(8) = flstat(8) - d2
  if (iout .ge. 0) go to 300
  isprin = intinf
  limstp = intinf
300 if (iplot .lt. 0) isplot = intinf
  lunit6save = lunit6
  lunit6 = 79
  open (unit = 79, status = 'scratch')
  if (nenerg .ne. 0 .and. kbase .ne. intinf) go to 54242
  flstat(5) = flstat(5) + d1
  flstat(6) = flstat(6) + d2
  if (iplot .lt. 0 .and. kbase .eq. 0) go to 7802
  if (iplot .lt. 0) go to 3106
  rewind lunit4
  write (unit = lunit4) date1, tclock, n18, numnvo, numbco, nc, (bus(i), i = 1, ntot), (texvec(i), i = 1, maxbus), (busum(i), i = 1, n16)
  if (iprsup .ge. 1) write (unit = *, fmt = 3207) nc, lsiz12, nsmout, ioutcs, ntot, numnvo, nv, n8, maxbus, n9, n18
  if (ivolt .ne. 1) go to 3103
  write (unit = lunit4) (i, i = 2, ntot)
  go to 3104
3103 if (numnvo .gt. 0) write (unit = lunit4) (ibsout(i), i = 1, numnvo)
3104 if (nc .gt. 0) write (unit = lunit4) (ibrnch(i), i = 1, nc), (jbrnch(i), i = 1, nc)
3106 if (kbase .ne. 1 .and. kbase .ne. intinf) go to 7802
  write (unit = lunit9) date1, tclock, ivolt, maxbus, ndx1, ndx2, n16, nsmout, ioutcs, numout, n18, numnvo, numbco, nc, kswtch
  write (unit = lunit9) (bus(i), i = 1, ntot), (texvec(i), i = 1, maxbus), (busum(i), i = 1, n16)
  if (iprsup .ge. 1) write (unit = *, fmt = 3207) nc, lsiz12, nsmout, ioutcs, ntot, numnvo, nv, n8, maxbus, n9, n18
  if (ivolt .ne. 1) go to 4103
  write (unit = lunit9) (i, i = 2, ntot)
  go to 4104
4103 if (numnvo .gt. 0) write (unit = lunit9) (ibsout(i), i = 1, numnvo)
4104 if (nc .gt. 0) write (unit = lunit9) (ibrnch(i), i = 1, nc), (jbrnch(i), i = 1, nc)
7802 if (ivolt .eq. 1) go to 3107
  if (numnvo .gt. nc) lstat(31) = numnvo
3107 if (noutpr .ne. 0) go to 54242
  n4 = ncsave - nv
  if (kol132 .eq. 132) go to 6637
  write (unit = lunit6, fmt = 6632) numnvo, nv, n4, nsmout, ioutcs
6632 format (' Time-step loop begins.  Number of node voltages, branch voltages, currents, tacs', /, &
       ' variables, and s.m. variables are :', 5i6)
  go to 6645
6637 write (unit = lunit6, fmt = 3108) k
3108 format (/, ' Column headings for the', i4, '  EMTP output variables follow.   These are ordered according to the five', /, ' possible EMTP output-variable classes, as follows ....')
  write (unit = lunit6, fmt = 3114) numnvo
3114 format (3x, 'First', i4, '  output variables are electric-network node voltages (with respect to local ground)|')
  write (unit = lunit6, fmt = 3115) nv
3115 format (3x, ' Next', i4, '  output variables are branch voltages (voltage of upper node minus voltage of lower node)|')
  write (unit = lunit6, fmt = 3116) n4
3116 format (3x, ' Next', i4, '  output variables are branch currents (flowing from the upper emtp node to the lower)|')
  write (unit = lunit6, fmt = 3117) nsmout
3117 format (3x, ' Next', i4, '  output variables pertain to dynamic synchronous machines, with names generated internally|')
  write (unit = lunit6, fmt = 3118) ioutcs
3118 format (3x, 'final', i4, "  output variables belong to  'TACS'  (note internally-added upper name of pair).")
  write (unit = lunit6, fmt = 3122)
3122 format (' Branch power  consumption (power  flow, if a switch) is treated like a branch voltage for this grouping|', /, ' branch energy consumption (energy flow, if a switch) is treated like a branch current for this grouping.')
6645 if (iprsup .le. 0) go to 54242
  if (npower .le. 0) go to 54242
  write (unit = lunit6, fmt = 3112) (ibrnch(i), jbrnch(i), i = 1, nc)
3112 format (/, " 'ibrnch'  and  'jbrnch'  pairs of branch-output node numers, in  over15", /, (1x, 20i6))
  write (unit = lunit6, fmt = 3113) (koutvp(i), i = 1, npower)
3113 format (/, " 'koutvp'  and  'koutie'  pairs.", /, (1x, 10i12))
  ndx1 = maxpe + 1
  ndx2 = maxpe + npower
  write (unit = lunit6, fmt = 3113) (koutvp(l), l = ndx1, ndx2)
54242 k = 0
  n1 = 0
  if (nenerg .ne. 0) n1 = 1
  kprsta = 1
  if (nenerg .gt. 0) kprsta = 0
  go to 3152
3119 k = k + 1
  j = k + 1
  if (ivolt .ne. 1) j = ibsout(k)
  m = m + 1
  aupper(m) = bus(j)
  alower(m) = blank
  if (m .lt. 9) go to 3158
  !  assign 3152 to moon
  moon = 3152
3126 if (noutpr .ne. 0) go to 7113
  if (n1 .eq. 1) write (unit = lunit6, fmt = 3131) (aupper(i), i = 1, 9)
3131 format (/, 15x, 9(7x, a6))
  if (n1 .eq. 0) write (unit = lunit6, fmt = 3132) (aupper(i), i = 1, 9)
3132 format (/, '  step     time', 9(7x, a6))
  if (n1 .gt. 0) go to 3144
  n1 = 1
3144 if (kprsta .eq. 1) write (unit = lunit6, fmt = 3145) (alower(i), i = 1, 9)
3145 format (15x, 9(7x, a6))
  if (kprsta .eq. 0) write (unit = lunit6, fmt = 8145) (alower(i), i = 1, 9)
8145 format ('  ref. angle  ', 9(7x, a6))
  kprsta = 1
  !7113 go to (3152, 3166, 3188, 3197, 3177, 8637) moon
7113 select case (moon)
  case (3152)
     go to 3152

  case (3166)
     go to 3166

  case (3188)
     go to 3188

  case (3197)
     go to 3197

  case (3177)
     go to 3177

  case (8637)
     go to 8637
  end select
3152 m = 0
3158 if (k .lt. numnvo) go to 3119
  k = 0
  go to 3169
3164 k = k + 1
  l = ibrnch(k)
  j = jbrnch(k)
  m = m + 1
  aupper(m) = bus(l)
  if (l .eq. 1) aupper(m) = terra
  alower(m) = bus(j)
  if (j .eq. 1) alower(m) = terra
  if (m .lt. 9) go to 3169
  !  assign 3166 to moon
  moon = 3166
  go to 3126
3166 m = 0
3169 if (k .lt. ncsave) go to 3164
  k = 0
  if (nenerg .ne. 0) go to 3198
  go to 3191
3182 k = k + 1
  l = ismout(3 * k - 1)
  j = ismout(3 * k)
  m = m + 1
  aupper(m) = texvec(l)
  alower(m) = texvec(j)
  if (m .lt. 9) go to 3191
  !  assign 3188 to moon
  moon = 3188
  go to 3126
3188 m = 0
3191 if (k .lt. nsmout) go to 3182
  k = 0
  go to 3199
3195 k = k + 1
  m = m + 1
  aupper(m) = text1
  ndx1 = jout(kjout + k)
  ndx1 = ilntab(klntab + ndx1)
  alower(m) = texvec(ndx1)
  if (m .lt. 9) go to 3199
  !  assign 3197 to moon
  moon = 3197
  go to 3126
3197 m = 0
3199 if (k .lt. ioutcs) go to 3195
  k = 0
  go to 8641
8634 k = k + 1
  ndx1 = n17 + k
  ndx2 = n19 + k
  l = ispum(ndx1)
  j = ispum(ndx2)
  m = m + 1
  aupper(m) = busum(l)
  alower(m) = busum(j)
  if (m .lt. 9) go to 8641
  !  assign 8637 to moon
  moon = 8637
  go to 3126
8637 m = 0
8641 if (k .lt. numout) go to 8634
3198 if (m .eq. 0) go to 3177
3171 m = m + 1
  aupper(m) = blank
  alower(m) = blank
  if(m .lt. 9) go to 3171
  !  assign 3177 to moon
  moon = 3177
  go to 3126
3177 kcount = nv
  lunit6 = lunit6save
  rewind 79
  do ip = 1, 99999
     read (unit = 79, fmt = 8927, end = 8644) outlin
8927 format (a132)
     write (unit = lunit6, fmt = 8927) outlin
  end do
8644 if (kswtch .le. 0) go to 8719
  n11 = 0
  do i = 1, kswtch
     ndx4 = lswtch + i
     k = kmswit(i)
     m = kmswit(ndx4)
     d6 = tclose(i)
     if (ktrlsw(1) .eq. 8877) go to  8688
     d6 = 0.0
     if (tclose(i) .gt. 0.0) go to  8710
     if (tclose(i) .eq. 0.0) go to 8695
     tclose(i) = 0.0
     go to 8689
8688 if (nextsw(i) .ne. 87) go to 8695
8689 d11 = 0.0
     if (kbase .eq. 2) go to 8702
     write (unit = lunit6, fmt = 8692) tclose(i), bus(k), bus(m), d11
8692 format (' ***          phasor i(0) =', e15.7, 33x, 'switch ', '"', a6,  '"', ' to ', '"', a6, '"', ' closed after', e12.5, ' sec.')
     go to 8702
8693 d6 = 0.0
     go to 8710
8695 if (tclose(i) .gt. 0.0) go to 8693
     if (iabs (kpos(i)) .ne. 10) go to 8284
     if (absz (e(k) - e(m)) .lt. topen(i)) go to 8710
8284 if (kbase .eq. 2) go to 8702
     write (unit = lunit6, fmt = 8699) bus(k), bus(m), tclose(i)
8699 format (' ***                       ', 15x, 33x, 'switch ', '"', a6, '"', ' to ', '"', a6, '"', ' closed after', e12.5, ' sec.')
8702 n8 = kpos(i)
     kpos(i) = 2
     if (n8 .lt. 0) kpos(i) = -2
     n11 = n11 + 1
     modswt(n11) = i
8710 if (kpos(i) .ge. 0) go to 8713
     kcount = kcount + 1
     bvalue(kcount) = d6
     if (iprsup .ge. 1) write (unit = lunit6, fmt = 8712) kcount, bvalue(kcount)
8712 format (' kcount, bvalue(kcount) =', i8, e15.6)
  end do
8713 continue
  ktrlsw(1) = n11
8719 nc = ncsave
  if (kbase .ne. 2 .and. kbase .ne. intinf) go to 710
  if (isw .eq. 0) go to 710
  n6 = 0
  do k1 = 1, kswtch
     if (iabs (kpos(k1)) .eq. 2) go to 4208
     if (adelay(k1) .ne. 44444.) go to 4216
     volti(n6+1) = tclose(k1)
     go to 4213
4208 if (adelay(k1) .ne. -44444.) go to 4216
     volti(n6 + 1) = topen(k1)
4213 n6 = n6 + 1
     if (n6 .lt. 2 * lsiz26) go to 4215
     lstat(19) = 4213
     iprint = 26
     go to 9200
4215 nextsw(n6) = k1
  end do
4216 continue
  write (unit = lunit6, fmt = 701) knt, (nextsw(k1), volti(k1), k1 = 1, n6 )
701 format (/, 32x, 'Random switching times for energization number', i4, /, (32x, 5(i4, e16.6)))
710 if (iprsup .ge. 1) write (unit = lunit6, fmt = 1077) kprchg, multpr
1077 format (/, ' Begin del-t loop at 1077.  kprchg, multpr=', /, (1x, 12i10))
  call move0 (nextsw(1 :), kswtch)
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 76101) (i, iform(i), node(i), crest(i), time1(i), time2(i), tstart(i), sfreq(i), i = 1, kconst)
76101 format (/, ' Source table.', /, 5x, 'row', 3x, 'iform', 4x, 'node', 10x, 'crest', 10x, 'time1', 10x, 'time2', 9x, 'tstart', 10x, 'sfreq', /, (3i8, 5e15.6))
  if (iprsup .gt. 0) write (unit = lunit6, fmt = 73899)
73899 format (/, ' n.l. elem table upon entering time-step loop', /, 5x, 'row', 2x, 'nltype', 3x, 'nonlk', 3x, 'nonlm', 2x, 'nonlad', 2x, 'nonle', 9x, 'vecnl1', 3x, 'anonl', 4x, 'curr', 8x, 'vnonl')
  i = 1
73900 if (i .gt. inonl) go to 73902
  if (iprsup .gt. 0) write (unit = lunit6, fmt = 73901) i, nltype(i), nonlk(i), nonlm(i), nonlad(i), nonle(i), vecnl1(i), anonl(i), curr(i), vnonl(i)
73901 format (6i8, e15.5, 2f8.0, e12.3)
  i = i + 1
  go to 73900
73902 i = 1
73903 if (i .gt. ichar) go to 1000
  if (iprsup .gt. 1) write (unit = lunit6, fmt = 73904) i, gslope(i), cchar(i), vchar(i)
73904 format (' n.l. char at 73904 of main14.   ', i10, 3e15.5)
  i = i + 1
  go to 73903
1000 if (nenerg .ne. 0) go to 1009
  n1 = 2 * modout
  if (modout .eq. 0) go to 9999
  if (noutpr .eq. 0) write (unit = lunit6, fmt = 1006) n1, modout
1006 format (/, 10x, 'Remember ---- what are labeled as the initial', i3,  '  branch-output currents are in reality', /, &
       24x, 'modal voltages at the two ends of the last distributed-parameter line of the data case being solved.', /, &
       24x, 'The first', i3, "  mode voltages at the  'bus1'  end all come first, followed by all the corresponding", /, &
       24x, "entries for the  'bus2'  end of the line.", /, 1x)
  go to 9999
1009 if (kbase .eq. intinf) kbase = 2
  if (kbase .eq. 2 .and. tenerg .lt. 0.0) call tables
  ltdelt = 0
  go to 9999
9000 lstat(16) = iprint
  kill = 1
9200 lastov = nchain
  nchain = 51
  lstat(18) = 15
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
4568 format ('  "Exit  module over15."')
  go to 99999
9999 lastov = nchain
  nchain = nchain + 1
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
99999 return
end subroutine over15

!
! subroutine top15.
!

subroutine top15
  use blkcom
  use labcom
  use tracom
  use movcop
  implicit none
  !
  !  dimension nsubkm(1)
  !  equivalence (kknonl(1), nsubkm(1))
  !  equivalence (lstat(14), knum)
  !
  integer(4) :: i, il
  integer(4) :: k, kntbr
  integer(4) :: l
  integer(4) :: mark
  integer(4) :: n2, n3, n15, nn1, nn15, nn16, numc
  real(8) :: a
  real(8) :: gus1, gus2, gus3, gus4
  real(8) :: h1, h2
  !
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 752) nenerg, ibr, kswtch, inonl
752 format (/, ' Enter  "top15" .  nenerg     ibr  kswtch   inonl', /, 17x, 5i8)
  knum = 1
  kntbr = 0
  iprsov(35) = 0
  kcount = 2
  k = 0
  i = 1
11905 if (kbus(i) .ge. 0) go to 905
  ci1 = ci(i)
  !      if (kodsem(i) .ne. 0 .and. imodel(i) .ne. -2)  go to 309
  ! 309  if (kodsem(i) .ne. 0 .and. imodel(i) .ne. -4)  go to 903
  if (kodsem(i) .ne. 0 .and. imodel(i) .ge. 0)  go to 903
  ck1 = absz (ck(i))
  a = 1.0d0
  if (ci1.lt.0.) a = ck1
  if (ck(i) .ge. 0.0) go to 25903
  l = int (cik(i))
  a = 1.0 / eta(l)
  if (iprsup .ge. 2) write (unit = lunit6, fmt = 24903) i, l, k, a, ck1, ci(i)
24903 format (/, ' freq dep  2 / m * z  at 24903   ', 3i10, 3e20.6)
25903 ci(i) = ci(i) * (a + ck1)
905 i = i + 1
  go to 904
903 nn1 = iabsz (kodebr(i))
  i = i + nn1
904 if (i .le. ibr) go to 11905
  if (kconst .eq. 0) go to 912
  do k = 1, kconst
     if (iform(k) .eq. 17) crest(k) = crest(k + 1)
     if (iabs (iform(k)) .ne. 14) go to 902
     sfreq(k) = sfreq(k) * twopi
     if (tstart(k) .lt. 0.0) tstart(k) = 0.0d0
902  if (iabs (iform(k)) .ne. 16) go to 906
     ck1 = tstop(k + 1)
     tstop(k + 1) = 0.
     n3 = iabs (node(k))
     n2 = iabs (node(k + 1))
     gus4 = e(n3) - e(n2)
     gus3 = crest(k + 2) - gus4 * tstop(k)
     gus1 = gus3
     if (iform(k + 1) .gt. 1) gus1 = ck1
     gus2 = gus4 * tstop(k) - crest(k)
     ck1 = gus2 / time1(k) + gus1 * sfreq(k) / crest(k + 1)
     h1 = time1(k + 1) * tstop(k) - crest(k)
     h2 = tstart(k + 1) * tstop(k) - crest(k)
     crest(k) = crest(k) + 2.0 * ck1
     sfreq(k) = 1.0 - 2.0 * sfreq(k) / crest(k + 1)
     time1(k) = 1.0 - 2.0 / time1(k)
     crest(k + 1) = 2.0 * ck1
     sfreq(k + 1) = gus2
     time1(k + 1) = h1
     time2(k + 1) = h2
     tstart(k + 1) = sfreq(k) * gus3 + time1(k) * gus2
  end do
906 continue
912 n15 = -4
  numsub = 0
  if (inonl .eq. 0) go to 9200
  call move0 (cursub(1 :), inonl)
  call move (kpsour(1 :), kssfrq(1 :), ntot)
  do k = 1, inonl
     nlsub(k) = 0
     if (nltype(k) .lt. 0) go to 7248
     n15 = n15 + 5
     nsubkm(n15) = n15
     nsubkm(n15 + 1) = nonlk(k)
     nsubkm(n15 + 2) = iabs (nonlm(k))
     nsubkm(n15 + 3) = k
     nsubkm(n15 + 4) = 0
     if (nltype(k) .gt. 920) nsubkm(n15 + 4) = nltype(k) - 920
     mark = kssfrq(nsubkm(n15 + 1))
     if (mark .eq. 0) mark = kssfrq(nsubkm(n15 + 2))
     if (mark .lt. ntot .and. mark .ne. 0) go to 2345
     numsub = numsub + 1
     isubeg(numsub) = n15
     nlsub(k) = numsub
     if (mark .eq. 0) go to 6789
     do il = 2, kpartb
        if (kssfrq(il) .eq. mark) kssfrq(il) = numsub
     end do
     go to 6789
7777 kill = 229
     lstat(15) = k
     lstat(16) = nsubkm(nn15 + 3)
     go to 9200
2345 nn15 = isubeg(mark)
     if (nsubkm(n15 + 4) .eq. 0) go to 7777
     if (nsubkm(nn15 + 4) .eq. 0) go to 7777
     numc = 2
2745 nn16 = nsubkm(nn15)
     if (nn15 .ge. nn16) go to 4567
     nn15 = nn16
     numc = numc + 1
     go to 2745
4567 nsubkm(nn15) = n15
     nsubkm(n15) = nn16
     nlsub(k) = mark
     if (numc .gt. ncomp) ncomp = numc
6789 if (iprsup .ge. 1) write (unit = lunit6, fmt = 7241) k,  numsub, n15, ncomp, isubeg(numsub)
7241 format ('  k, numsub, n15, ncomp, isubeg(numsub) =', 5i8)
  end do
7248 continue
  if (iprsup .ge. 7) write (unit = *, fmt = *) ' top15.  kssfrq(1:ntot) =', (kssfrq(k), k = 1, ntot)
  do k = 2, kpartb
     if (kssfrq(k) .gt. ntot) kssfrq(k) = 0
  end do
  if (iprsup .ge. 7) write (unit = *, fmt = *) ' after tamper..  kssfrq(1 : ntot) =', (kssfrq(k), k = 1, ntot)
  call move (kssfrq(1 :), kpsour(1 :), ntot)
9200 call move0 (kode(1 :), ntot)
  nfrfld = n15
  if (iprsup .ge. 3) write (unit = lunit6, fmt = 9201)
9201 format (' Exit  "top15" .')
  return
end subroutine top15

!
! subroutine smout.
!

subroutine smout
  use blkcom
  use labcom
  use smach
  implicit none
  !     This module is used only by  type 59 s.m.  modeling
  character(8) :: busvec(1)
  character(8) :: digit(10)
  character(8) :: text1, text2, texta(15), textb(3)
  !  dimension texta(15), digit(10), textb(3), busvec(1)
  integer(4) :: i, i5, i30, icnt, ip, ip1
  integer(4) :: jb, jk, jk1
  integer(4) :: l, ll5, ll6
  integer(4) :: m
  integer(4) :: n, n1, n2, n5, n6, n7, n10, n15
  real(8) :: d12
  !
  texta = (/ 'id    ', 'iq    ', 'i0    ', 'if    ', 'ikd   ', 'ig    ', 'ikq   ', 'ia    ', 'ib    ', 'ic    ', 'efd   ', 'mforce', 'mang  ', 'tq gen', 'tq exc' /)
  ! data texta(1)   /6hid    /
  ! data texta(2)   /6hiq    /
  ! data texta(3)   /6hi0    /
  ! data texta(4)   /6hif    /
  ! data texta(5)   /6hikd   /
  ! data texta(6)   /6hig    /
  ! data texta(7)   /6hikq   /
  ! data texta(8)   /6hia    /
  ! data texta(9)   /6hib    /
  ! data texta(10)  /6hic    /
  ! data texta(11)  /6hefd   /
  ! data texta(12)  /6hmforce/
  ! data texta(13)  /6hmang  /
  ! data texta(14)  /6htq gen/
  ! data texta(15)  /6htq exc/
  textb = (/ 'ang   ', 'vel   ', 'tor   ' /)
  ! data  textb(1)   /  6hang     /
  ! data  textb(2)   /  6hvel     /
  ! data  textb(3)   /  6htor     /
  digit = (/ '1     ', '2     ', '3     ', '4     ', '5     ', '6     ', '7     ', '8     ', '9     ', '0     ' /)
  ! data  digit(1)    /  6h1       /
  ! data  digit(2)    /  6h2       /
  ! data  digit(3)    /  6h3       /
  ! data  digit(4)    /  6h4       /
  ! data  digit(5)    /  6h5       /
  ! data  digit(6)    /  6h6       /
  ! data  digit(7)    /  6h7       /
  ! data  digit(8)    /  6h8       /
  ! data  digit(9)    /  6h9       /
  ! data  digit(10)   /  6h0       /
  text1 = 'mach  '
  !data  text1   /  6hmach    /
  ll5 = 5
  ll6 = 6
  nsmout = 0
  jk = 0
  jk1 = 0
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 1328) nst, lsyn
1328 format (/, " Begin  'smout' .     nst    lsyn", /, 17x, 4i8)
  n1 = 0
  n2 = 0
  ip1 = -2
  do ip = 1, nst
     i30 = 30 * ip - 29
     n2 = n2 + 1
     if (n2 .le. 9) go to 4113
     n2 = 0
     n1 = n1 + 1
4113 i = ip
     n = i
     i5 = i30 + 17
     n15 = ismdat( i5 )
     if (n15 .lt. 0) go to  3648
     l = 5
     busvec(1) = text1
     if (n1 .gt. 0) call packa1 (digit(n1), busvec(1), l)
     l = l + 1
     n10 = n2
     if (n10 .eq. 0) n10 = 10
     call packa1 (digit(n10), busvec(1), l)
     text2 = busvec(1)
     icnt = 0
     call namea6 (text2, icnt)
     if (n15 .eq. 0) go to 21
     do l = 1, n15
        ip1 = ip1 + 3
        m = ismout(ip1)
        jk = jk + 1
        jk1 = jk1 + 3
        ismout(jk1 - 1) = icnt
        text2 = texta(m)
        jb = 0
        call namea6 (text2, jb)
        ismout(jk1) = jb
     end do
21   do n7 = 1, 3
        i5 = i5 + 1
        n15 = ismdat(i5)
        if (n15 .eq. 0) go to 4224
        do l = 1, n15
           ip1 = ip1 + 3
           n6 = ismout(ip1)
           n5 = n6 / 10
           n6 = n6 - 10 * n5
           busvec(1) = textb(n7)
           if (n5 .gt. 0) go to 4206
           call packa1 (digit(n6), busvec(1), ll5)
           go to 4215
4206       call packa1 (digit(n5), busvec(1), ll5)
           n10 = n6
           if (n10 .eq. 0) n10 = 10
           call packa1 (digit(n10), busvec(1), ll6)
4215       jk = jk + 1
           jk1 = jk1 + 3
           ismout(jk1 - 1) = icnt
           jb = 0
           text2 = busvec(1)
           call namea6 (text2, jb)
           ismout(jk1) = jb
        end do
     end do
4224 continue
     if (jk .le. lsmout) go to 3648
     kill = 1
     lstat(19) = 4224
     lstat(16) = 11
     return
  end do
3648 continue
  if (iprsup .ge. 3) write (unit = lunit6, fmt = 4240) (i, ismout(3 * i), ismout(3 * i - 1), i = 1, jk)
4240 format (/, ' (i, jsmout(i), ksmout(i), i = 1, jk)', /, (1x, 10(3i4, 1x)))
  nsmout = jk
  d12 = nbyte(4)
  d12 = (3 * nsmout) * d12 / nbyte(3)
  msmout = int (d12 + 1.0d0)
  return
end subroutine smout

!
! end of file over15.f90
!
