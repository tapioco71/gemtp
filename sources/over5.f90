!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over5.f90
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

module umlocal
  use blkcom
  implicit none
  character(8) :: texta(101)
  integer(4) :: ibrexc
  integer(4) :: jf, jr
  integer(4) :: kconex
  integer(4) :: limasu, lopss1, lopss2, lopss4, lopss8, lopss9, lopss10
  integer(4) :: mlum
  integer(4) :: n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n14, n15, n16
  integer(4) :: n17, n18, n19, nall, nangre, netrum, netrun, nexc, nexcsw
  integer(4) :: ngroup, nmexc, nmgen, nparum, nrsyn, nsmtac, nsmtpr, nstan
  integer(4) :: nshare, ntorq, ntypsm, numasu
  real(8) :: agldum, aglqum
  real(8) :: d1, d2, d3, d17, dabsum, distrf, dsynum, dmutum
  real(8) :: fmum
  real(8) :: hjum
  real(8) :: raum, rkvum, rmvaum, rnum
  real(8) :: s1um, s1qum, s2um, s2qum, spring, squm, stat59
  real(8) :: tdppum, tdpum, tqppum
  real(8) :: x0um, xdfum, xgkqum, xkdum, xkqum, xdppum, xdpum, xdum, xfum, xgum
  real(8) :: xlum, xnum, xqgum, xqkqum
  real(8) :: xqppum, xqpum, xqum
  real(8) :: zlsbum
  save
end module umlocal

module umdat
  implicit none

contains

  !
  ! subroutine umdata.
  !

  subroutine umdata (reacl, gpar, fpar, hist,umcurp, nodvo1, nodvo2, jcltac, jclout,jtype,nodom, jtmtac, histom, omegm, omold, thetam, reamdu, reamds, flxds, flxdr, reamqu, flxqs, flxqr, jcdsat, jcqsat, flxd, flxq, nppair, rotmom, ncld, nclq, jtqout, jomout, jthout, reamqs, epsom, dcoef, kcoil, voltum, anglum, nodfum, nodmum, kumout, jumout, umoutp)
    use umlocal
    use blkcom
    use labcom
    use umcom
    use tacsar
    use smtacs
    use tracom
    implicit none
    integer(4), intent(out) :: jcdsat(:)
    integer(4), intent(out) :: jcltac(:)
    integer(4), intent(out) :: jclout(:)
    integer(4), intent(out) :: jcqsat(:)
    integer(4), intent(out) :: jomout(:)
    integer(4), intent(out) :: jthout(:)
    integer(4), intent(out) :: jtmtac(:)
    integer(4), intent(out) :: jtqout(:)
    integer(4), intent(out) :: jtype(:)
    integer(4), intent(out) :: jumout(:)
    integer(4), intent(out) :: kcoil(:)
    integer(4), intent(out) :: kumout(:)
    integer(4), intent(out) :: ncld(:)
    integer(4), intent(out) :: nclq(:)
    integer(4), intent(out) :: nodfum(:)
    integer(4), intent(out) :: nodmum(:)
    integer(4), intent(out) :: nodom(:)
    integer(4), intent(out) :: nodvo1(:)
    integer(4), intent(out) :: nodvo2(:)
    integer(4), intent(out) :: nppair(:)
    real(8), intent(out) :: anglum(:)
    real(8), intent(out) :: dcoef(:)
    real(8), intent(out) :: epsom(:)
    real(8), intent(out) :: fpar(:)
    real(8), intent(out) :: flxd(:)
    real(8), intent(out) :: flxds(:)
    real(8), intent(out) :: flxdr(:)
    real(8), intent(out) :: flxq(:)
    real(8), intent(out) :: flxqs(:)
    real(8), intent(out) :: flxqr(:)
    real(8), intent(out) :: gpar(:)
    real(8), intent(out) :: hist(:)
    real(8), intent(in) :: histom(:)
    real(8), intent(out) :: omegm(:)
    real(8), intent(out) :: omold(:)
    real(8), intent(out) :: reacl(:)
    real(8), intent(out) :: reamdu(:)
    real(8), intent(out) :: reamds(:), reamqs(:), reamqu(:)
    real(8), intent(out) :: rotmom(:)
    real(8), intent(out) :: thetam(:)
    real(8), intent(out) :: umcurp(:)
    real(8), intent(out) :: umoutp(:)
    real(8), intent(out) :: voltum(:)
    !  dimension reacl(1), gpar(1), fpar(1), hist(1),umcurp(1)
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
    character(8) :: text3, text4, text5
    character(8) :: textm, tesm1, tesm2, tesm3, tesm4, tesm5
    character(8) :: tesm6, tesm7, tesm8, tesm9
    integer(4) :: j
    integer(4) :: kcl
    integer(4) :: n13, n20, ncldq, ndx1, ntacb, numoui
    real(8) :: d10
    real(8) :: rppair
    !
    !. data initialization:
    !     be careful with fpar(kcl) and umcurp(kcl). in umdata and
    !      and in umrenu they are used differently than in solvum.
    !     fpar(kcl) in umdata and umrenu : adress ibr of leakage ind
    !     fpar(kcl+1) in umdata and umrenu : adress kconst of excit
    !       sources to represent im excit coils (umdata creation)
    !     umcurp in umdata and umrenu : used to store shift in power
    !      nodes if load-flow is requested. then after load-flow is
    !      is completed, these same cells of umcurp are used as
    !      pointers for the shift of power resistances to network.
    texta = (/ 'um-1  ', 'um-2  ', 'um-3  ', 'um-4  ', 'um-5  ', 'um-6  ', 'um-7  ', 'um-8  ', 'um-9  ', 'um-10 ', 'um-11 ', 'um-12 ', 'um-13 ', 'um-14 ', 'um-15 ', 'um-16 ', 'um-17 ', 'um-18 ', 'um-19 ', 'um-20 ', 'tqgen ', 'omegm ', 'thetam', 'ipa   ', 'ipb   ', 'ipc   ', 'ie1   ', 'ie2   ', 'ie3   ', 'ie4   ', 'ie5   ', 'ie6   ', 'ie7   ', 'ie8   ', 'ie9   ', 'ie10  ', 'ie11  ', 'ie12  ', 'ie13  ', 'ie14  ', 'um1mxx', 'um1e1x', 'um1e2x', 'um1e3x', 'um2mxx', 'um2e1x', 'um2e2x', 'um2e3x', 'um3mxx', 'um3e1x', 'um3e2x', 'um3e3x', 'um4mxx', 'um4e1x', 'um4e2x', 'um4e3x', 'um5mxx', 'um5e1x', 'um5e2x', 'um5e3x', 'um1ntr', 'um2ntr', 'um3ntr', 'um4ntr', 'um5ntr', 'um1ms1', 'um2ms1', 'um3ms1', 'um4ms1', 'um5ms1', 'um1ms2', 'um2ms2', 'um3ms2', 'um4ms2', 'um5ms2', 'um1tla', 'um1tlb', 'um1tlc', 'um2tla', 'um2tlb', 'um2tlc', 'um3tla', 'um3tlb', 'um3tlc', 'um4tla', 'um4tlb', 'um4tlc', 'um5tla', 'um5tlb', 'um5tlc', 'fluxmd', 'imd   ', 'fluxmq', 'imq   ', 'ip0   ', 'ipd   ', 'ipq   ', 'tqexc ', 'um1mcc', 'um2mcc', 'um3mcc' /)
    data textm / 'more  ' /
    data tesm1, tesm2, tesm3, tesm4, tesm5, tesm6, tesm7, tesm8, tesm9 / 'smdata', 'tolera', 'parame', 'all   ', 'none  ', 'extend', 'finish', ' part ', 'share ' /
    ! if mech network option is used, rotmom is used to store
    !    frequency of network connected to power coil. initiali -
    !    zation is in umrenu, except if sm type-59 is used.
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module umdata."')
    if (numum .eq. 0) go to 40
    write (unit = lunit(6), fmt = 30)
30  format (/, ' um error stop. You have requested another um to be included to this data case. The rules regarding the ordering of um')
    write (unit = lunit(6), fmt = 31)
31  format (' data cards have however been violated. The data cards of this um should be placed immediately behind those of the', /, ' previous um in accordance with the following rules :')
    write (unit = lunit(6), fmt = 32)
32  format (/, ' (1). The data cards of the next um have to follow immediately those of the previous um without the insertion of')
    write (unit = lunit(6), fmt = 33)
33  format (' a blank card and without repeated insertion of the class-1 um data cards.  These class-1 cards, consisting of two')
    write (unit = lunit(6), fmt = 34)
34  format (' cards and a blank termination card, contains initial information regarding the reading of all um cards to follow. It is')
    write (unit = lunit(6), fmt = 35)
35  format (' therefore to be placed on top and never to be repeated between the cards of the', " different um's.")
    write (unit = lunit(6), fmt = 36)
36  format (/, ' (2). The data cards of the last um to be included to this data case is to be terminated with a blank card, indicating' , /, ' the ending of all um data cards.')
    go to 9600
    ! . busum adjustment due to variable numfix :
40  do n1 = 1, numfix
       busum(n1) = texta(n1)
    end do
    do n1 = 1, 28
       n2 = n1 + numfix
       n3 = n1 + 12
       if (n1 .gt. 11) go to 50
       n3 = n1 + 87
       if (n1 .gt. 3) go to 50
       n3 = n1 + 20
50     busum(n2) = texta(n3)
    end do
    pi = twopi * onehaf
    d2 = 2.0d0
    d3 = 3.0d0
    sroot2 = sqrtz (d2)
    sroot3 = sqrtz (d3)
    omegrf = twopi * statfr
    ! start reading um data ***************************************
    ! note : the following variable names are reserved for
    ! looping during reading of the data input :
    numum = 0
    ncltot = 0
    nsmtac = 0
    nsmach = 0
    ! nsmtac is the total number of additional entries to
    ! umoutp(numout) because of sm tacs transfer.
    n1 = 0
    ! .user1 : general specifications (class 1 um data cards)
    ! read input card using cimage
    call cimage
    read (unit = abuff, fmt = 60) inpu, initum, bus3, limasu, loopss(8)
60  format (2i1, a6, i6, i1)
    if (bus3 .eq. tesm1) initum = 1
    if (limasu .eq. 0) limasu = 10
    if (loopss(8) .eq. 0) go to 105
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 102)
102 format ('+um power coil interfacing by prediction')
    go to 150
105 if (noutpr .eq. 0) write (unit = kunit6, fmt = 110)
110 format ('+Um fully compensated')
    ! read (lunit5,203) jtype(n1),ncld(n1),nclq(n1),jtqout(n1),
    ! vjomout(n1),jthout(n1),bum3(n1),bum4(n1),nppair(n1),
    ! vrotmom(n1),dcoef(n1),epsom(n1),omegm(n1),reamdu(n1),
    ! vjcdsat(n1),reamds(n1),flxds(n1),flxdr(n1),thetam(n1),
    ! vreamqu(n1),jcqsat(n1),reamqs(n1),flxqs(n1),flxqr(n1),noread
    !203 format (3i2, 3i1, 2a6, i2, 3e14.5, /, 2e14.5, i1, 3e14.5, /, 2e14.5, i1, 3e14.5,i1)
    ! decision on m31 or m32 data input ***************************
    ! with m32 , the input is modular.
150 call cimage
    read (unit = abuff, fmt = 1207) text3, text4, text5
    if (text3 .ne. blank) go to 3202
    if (text4 .ne. blank) go to 3202
    if (text5 .ne. blank) go to 3202
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 1001)
1001 format ('+Blank card ending class-1 um data cards')
    go to 11202
    ! data input for m31 and older versions ***********************
    !. user2 : mach-table input for m31 and older
    ! read input card using cimage
1202 call cimage
1203 n6 = 0
    read (unit = abuff, fmt = 1207) text3, text4, text5
1207 format (13a6, a2)
    if (text3 .ne. blank) go to 3202
    if (text4 .ne. blank) go to 3202
    if (text5 .ne. blank) go to 3202
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 2202)
2202 format ('+Blank card ending machine table.')
    go to 300
3202 numum = numum + 1
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 170) numum
170 format ('0the machine-table input of um number', i3, ':')
    if (numum .le. numfix) go to 5202
    write (unit = lunit(6), fmt = 4202) numum
4202 format (/, ' Overflow u.m. machine table in module "umdata". Current case has', 2x, i4, 2x, 'u.m.,  numfix on the card for absolute um dimensions', /, ' must be set to no less than', 2x, i4, '. ')
    go to 9600
5202 n1 = numum
    read (unit = abuff, fmt = 1204) nclq(n1), jtqout(n1), jomout(n1), jthout(n1), bus3, bus4, nppair(n1), rotmom(n1), dcoef(n1), epsom(n1)
1204 format (3i2, 3i1, 2a6, i2, 3e14.5)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 3203) jtype(n1), ncld(n1), nclq(n1)
3203 format ('+Mach-table card   1.', 3i5)
    do n10 = 1, ntot
       if (bus3 .eq. blank) go to 205
       if (bus3 .ne. bus(n10)) cycle
       nodom(n1) = n10
       rotmom(n1) = twopi * statfr
       go to 205
    end do
    write (unit = lunit(6), fmt = 3204) bus3
3204 format (/, ' Error stop.   The node name  "bus3" on the last-read card is not that', /, ' of any electric network node. bus3=', a6)
    go to 9600
205 jtmtac(n1) = 0
    if (bus3 .ne. blank) go to 213
    do j = 1, ktab
       ndx1 = ilntab(klntab + j)
       if (bus4 .eq. texvec(ndx1)) go to 209
    end do
    if (bus4 .eq. blank) go to 213
    write (unit = lunit(6), fmt = 3207) bus4
3207 format (/, ' Error stop.   The TACS name  "bus4" on the last-read card is unrecognized.   bus4=', a6)
    go to 9600
209 ndx1 = kxtcs + j
    jtmtac(n1) = ndx1
    ! read input card using cimage
213 call cimage
    read (unit = abuff, fmt = 4203) omegm(n1), reamdu(n1), jcdsat(n1), reamds(n1), flxds(n1), flxdr(n1)
4203 format (2e14.5, i1, 3e14.5)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 5203) omegm(n1), reamdu(n1)
5203 format ('+Mach-table card   2.', 2e12.4)
    ! read input card using cimage
    call cimage
    read (unit = abuff, fmt = 4203) thetam(n1), reamqu(n1), jcqsat(n1), reamqs(n1), flxqs(n1), flxqr(n1)
    if (reamqu(n1) .gt. 0.0) go to 6199
    write (unit = lunit(6), fmt = 6198) n1
6198 format (/, ' Error stop. Incorrect um data input. the result is that the q-axis main inductance', /, 7x, ' is either zero or negative for machine number', i4, '.')
    go to 9600
6199 if (noutpr  .eq.  0) write (unit = kunit6, fmt = 6203) thetam(n1), reamqu(n1)
6203 format ('+Mach-table card   3.', 2e12.4)
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 214) jtype(n1), ncld(n1), nclq(n1), jtqout(n1), jomout(n1), jthout(n1), bus3, bus4, nppair(n1), rotmom(n1), dcoef(n1), epsom(n1), omegm(n1), reamdu(n1), jcdsat(n1), reamds(n1), flxds(n1), flxdr(n1), thetam(n1), reamqu(n1), jcqsat(n1), reamqs(n1), flxqs(n1), flxqr(n1)
214 format ('0', 3i2, 3i1, 2a6, i2, 3e14.5, /, ' ', 2e14.5, i1, 3e14.5, ' ', 2e14.5, i1, 3e14.5)
    if (initum .eq. 0) go to 249
    call cimage
    d1 = 1.0e+3
    n5 = 4
    n6 = 0
    read (unit = abuff, fmt = 6204) voltum(n1), anglum(n1), bus5, bus6, distrf
6204 format (2e14.5, 2a6, e14.5)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 6205) voltum(n1), anglum(n1)
6205 format ('+Mach-table card   4.', 2e12.4)
    if (jtype(n1) .eq. 3) go to 230
    if (jtype(n1) .eq. 4) go to 230
    if (jtype(n1) .eq. 5) go to 230
    do n10 = 1,ntot
       if (bus5 .ne. bus(n10)) cycle
       nodfum(n1) = n10
       do n11 = 1, kconst
          n12 = node(n11)
          if (n12 .lt. 0) n12 = - n12
          if (nodfum(n1) .ne. n12) cycle
          nodfum(n1) = n11
          go to 230
       end do
    end do
    write (unit = lunit(6), fmt = 222) bus5
222 format (/, ' Error stop.   The node name  "busf" on the last-read card is not that', /, ' of any electric network source node. busf=', a6)
    go to 9600
230 do n10 = 1,ntot
       if (bus6 .ne. bus(n10)) cycle
       do n11 = 1,kconst
          n12 = node(n11)
          if (n12 .lt. 0) n12 = - n12
          if (n10 .ne. n12) cycle
          crest(n11) = d1 * distrf
          if (n6 .ne. 0) go to 231
          nodmum(n1) = n11
          if (distrf .eq. 0.0) crest(n11) = d1
231       tstart(n11) = -7777.0d0
          go to 240
       end do
    end do
    write (unit = lunit(6), fmt = 234) bus6
234 format (/, ' Error stop.   The node name  "busm" on the last-read card is not that', /, ' of any electric network source node. busm=', a6)
    go to 9600
240 call cimage
    read (unit = abuff, fmt = 6240) text3
6240 format (a6)
    if (text3 .ne. textm) go to 248
    n5 = n5 + 1
    n6 = 1
    read (unit = abuff, fmt = 6241) text3, bus6, distrf
6241 format (a6, 28x, a6, e14.5)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 6242) n5, distrf
6242 format ('+mach-table card', i4, '.', e12.4)
    go to 230
248 go to 1203
249 go to 1202
    ! . if flux output is desired :
    ! . user3 : coil-table input for m31 and older
300 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 301)
301 format ('0the coil-table input:', /, 1x)
    ! . avoid conflict blank termination card with dummy coil
    ! . and adjustment of ncld and nclq for um type 4 :
    n5 = 0
    do n10 = 1, numum
       if (jtype(n10) .ne. 4) go to 302
       n5 = n5 + 1
       ncld(n10) = 1
       nclq(n10) = 1
302    n5 = n5 + ncld(n10) + nclq(n10)
    end do
    n5 = n5 + 3 * numum
    !     read (lunit5,303) gpar(n1),reacl(n1),bum1(n1),bum2(n1),
    !     vbum6(n1),jclout(n1),hist(n1),noread
303 format (2e14.5, 3a6, i1, e14.5)
    !     read input card using cimage
3302 call cimage
    read (unit = abuff, fmt = 1207) text3, text4, text5
    if (text3 .ne. blank) go to 4303
    if (text4 .ne. blank) go to 4303
    if (text5 .ne. blank) go to 4303
    if (ncltot .lt. n5) go to 4303
    if (noutpr .eq. 0) write (kunit6, 4302)
4302 format ('+Blank card ending coil table.')
    go to 500
4303 ncltot = ncltot + 1
    fpar(ncltot) = 0.0d0
    if (ncltot .le. nclfix) go to 6302
    write (unit = lunit(6), fmt = 5302) ncltot
5302 format (/, ' Overflow u.m. coil table.   ncltot =', i5)
    go to 9600
6302 n1 = ncltot
    read (unit = abuff, fmt = 303) gpar(n1), reacl(n1), bus1, bus2, bus6, jclout(n1), hist(n1)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 2203) ncltot, gpar(n1), reacl(n1), jclout(n1)
2203 format ('+Coil', i4, '.', 2e14.5, i5)
    n11 = 0
    n12 = 0
    do n10 = 1, ntot
       if (n11 .ne. 0) go to 304
       if (bus1 .ne. bus(n10)) go to 304
       nodvo1(n1) = n10
       n11 = 1
       if (n12 .ne. 0) go to 305
304    if (bus2 .ne. bus(n10)) go to 305
       nodvo2(n1) = n10
       n12 = 1
305    n13 = n11 * n12
       if (n13 .ne. 0) go to 307
    end do
    if (n11 .eq. 0) write (unit = lunit(6), fmt = 3206) bus1
3206 format (/, ' Error stop.  Just-read coil card bears illegal node name:', a6)
    if (n12 .eq. 0) write (unit = lunit(6), fmt = 3206) bus2
    go to 9600
307 jcltac(n1) = 0
    if (bus6 .eq. blank) go to 313
    do j = 1, ktab
       ndx1 = ilntab(klntab + j)
       if (bus6 .eq. texvec(ndx1)) go to 311
    end do
    write (unit = lunit(6), fmt = 3207) bus6
    go to 9600
311 ndx1 = kxtcs + j
    jcltac(n1) = ndx1
313 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 314) gpar(n1), reacl(n1), bus1, bus2, bus6, jclout(n1), hist(n1)
314 format (1x, 2e14.5, 3a6, i1, e14.5)
    go to 3302
    !  data input for m32 and newer versions ********************
    !. user2 : mach-table input for m32 and newer
    !     class 2 and class 3 um data cards
11202 call cimage
    read (unit = abuff, fmt = 11207) text3, text4, text5
11207 format (13a6, a2)
    if (text3 .ne. blank) go to 13202
    if (text4 .ne. blank) go to 13202
    if (text5 .ne. blank) go to 13202
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 12202)
12202 format ('+Blank card ending um data.')
    go to 500
13202 numum = numum + 1
    if (numum .le. numfix) go to 17000
    write (unit = lunit(6), fmt = 14202) numum
14202 format (/, 'Overflow u.m. machine table.  numum =', i4, 'increase numfix in card for absolute', /, ' um dimensions.')
    go to 9600
    !  checking of data input format ******************************
17000 n10 = 0
    read (unit = abuff, fmt = 17002) n10
17002 format (i2)
    if (n10 .ne. 19) go to 17004
    write (unit = lunit(6), fmt = 30)
    write (unit = lunit(6), fmt = 31)
    write (unit = lunit(6), fmt = 32)
    write (unit = lunit(6), fmt = 33)
    write (unit = lunit(6), fmt = 34)
    write (unit = lunit(6), fmt = 35)
    write (unit = lunit(6), fmt = 36)
    go to 9600
17004 if (n10 .ge. 50 .and. n10 .lt. 60) go to 17040
    if (n10 .lt. 50) go to 15202
    ! start reading sm type -50 to 59 data input format **********
    jf = 0
    jr = 0
17040 call umdatb (reacl, gpar, fpar, nodvo1, nodvo2, jcltac, jtype, nodom, ncld, jtmtac, reamdu, reamds, flxds, flxdr, reamqu, flxqs, flxqr, jcdsat, jcqsat, nppair, rotmom, nclq, jtqout, jthout, reamqs, voltum, anglum, nodfum, nodmum, kumout, jumout, jclout, dcoef, jomout, umoutp)
    if (jf .eq. 1) go to 11202
    if (jr .eq. 1) return
    ! end reading sm type-59 data input $$$$$$$$$$$$$$$$$$$$$$$$$$
    ! start reading pure um data input format ******************
15202 n1 = numum
    read (unit = abuff, fmt = 11203) jtype(n1), ncld(n1), nclq(n1), jtqout(n1), jomout(n1), jthout(n1), bus3, bus4, nppair(n1), rotmom(n1), dcoef(n1), epsom(n1), d10
11203 format (3i2, 3i1, 2a6, i2, 4e14.5)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 13203) numum, jtype(n1)
13203 format ('+um -', i3, '   mach card  1.   type = ', i2)
    do n10 = 1, ntot
       if (bus3 .eq. blank) go to 10205
       if (bus3 .ne. bus(n10)) cycle
       nodom(n1) = n10
       if (d10 .eq. 0.0d0) d10 = statfr
       rotmom(n1) = twopi * d10
       go to 10205
    end do
    write (unit = lunit(6), fmt = 13204) bus3
13204 format (/, ' Error stop.   The node name  "bus3" on the last-read card is not that', /, ' of any electric network node. bus3=', a6)
    go to 9600
10205 jtmtac(n1) = 0
    if (bus3 .ne. blank) go to 10213
    do j = 1, ktab
       ndx1 = ilntab(klntab + j)
       if (bus4 .eq. texvec(ndx1)) go to 10209
    end do
    if (bus4 .eq. blank) go to 10213
    write (unit = lunit(6), fmt = 13207) bus4
13207 format (/, ' Error stop.   The TACS name  "bus4" on the last-read card is', /, ' unrecognized.   bus4=', a6)
    go to 9600
10209 ndx1 = kxtcs + j
    jtmtac(n1) = ndx1
    ! read input card using cimage
10213 call cimage
    read (unit = abuff, fmt = 14203) omegm(n1), reamdu(n1), jcdsat(n1), reamds(n1), flxds(n1), flxdr(n1)
14203 format (2e14.5, i1, 3e14.5)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 15203) numum, omegm(n1), reamdu(n1)
15203 format ('+um -', i3, '   mach card  2.', 2e10.2)
    ! read input card using cimage
    call cimage
    read (unit = abuff, fmt = 14203) thetam(n1), reamqu(n1), jcqsat(n1), reamqs(n1), flxqs(n1), flxqr(n1)
    if (reamqu(n1) .gt. 0.0d0) go to 15910
    write (unit = lunit(6), fmt = 6198) n1
    go to 9600
15910 if (noutpr .eq. 0) write (unit = kunit6, fmt = 15903) numum, thetam(n1), reamqu(n1)
15903 format ('+um -', i3, '   mach card  3.', 2e10.2)
    if (iprsup .ge. 4) write (unit = lunit(6), fmt = 15912) n1
15912 format (/, ' *************************************** The machine-table input of um number', i4, ' :')
    if (iprsup .ge. 4) write (unit = lunit(6), fmt = 10214) jtype(n1), ncld(n1), nclq(n1), jtqout(n1), jomout(n1), jthout(n1), bus3, bus4, nppair(n1), rotmom(n1), dcoef(n1), epsom(n1), omegm(n1), reamdu(n1), jcdsat(n1), reamds(n1), flxds(n1), flxdr(n1), thetam(n1), reamqu(n1), jcqsat(n1), reamqs(n1), flxqs(n1), flxqr(n1)
10214 format (' *********', 3i2, 3i1, 2a6, i2, 3e14.5, ' *********', 2e14.5, i1, 3e14.5, ' *********', 2e14.5, i1, 3e14.5)
    ! read share card(if sharing of mech netw is requested) :
    nshare = 0
    call cimage
    read (unit = abuff, fmt = 15800) text3
15800 format (a6)
    if (text3 .ne. tesm9) go to 15830
    nshare = 1
    read (unit = abuff, fmt = 15810) n5, n6
15810 format (6x, 2i6)
    if (n6 .eq. 0) go to 15822
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 15820) numum,n5,n6
15820 format ('+um -', i3, ' shares mech netw with um-', i3, 'and', i3)
    go to 15826
15822 if (noutpr .eq. 0) write (unit = kunit6, fmt = 15824) numum,n5
15824 format ('+um -', i3, ' shares mech netw with um-', i3)
15826 if (n1 .ne. n5 .and. n1 .ne. n6) go to 15828
    write (unit = lunit(6), fmt = 15827) n1
15827 format (/, ' Error stop. This um-', i3, " is supposed to share its mech network with other um's. What", ' should be specified on this card are the', /, " numbers of these other um's without", ' including the number of the um which is being processed right now.')
    call stoptp
15828 n10 = n1
    if (n10 .gt. n5) n10 = n5
    if ((n6 .ne. 0) .and. (n10 .gt. n6)) n10 = n6
    if (n1 .ne. n10) nshare = 2
15830 if (initum .eq. 0) go to 10300
    if (nshare .eq. 0) go to 15880
    call cimage
15880 d1 = 1.0e+3
    ! make sure that user sets up automatic ss-calc if ldflow :
    if (istep .ne. -4567) go to 15890
    if (nshare .eq. 2) go to 15890
    read (unit = abuff, fmt = 15882) bus6
15882 format (34x, a6)
    if (bus6 .ne. blank) go to 15890
    write (unit = lunit(6), fmt = 80502)
    call stoptp
15890 n5 = 4
    n6 = 0
    read (unit = abuff, fmt = 15904) voltum(n1), anglum(n1), bus5, bus6, distrf
15904 format (2e14.5, 2a6, e14.5)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 15905) numum, voltum(n1), anglum(n1)
15905 format ('+um -', i3, '   mach card  4.', 2e10.2)
    if (jtype(n1) .eq. 3) go to 10230
    if (jtype(n1) .eq. 4) go to 10230
    if (jtype(n1) .eq. 5) go to 10230
    do n10 = 1, ntot
       if (bus5 .ne. bus(n10)) cycle
       nodfum(n1) = n10
       do n11 = 1, kconst
          n12 = node(n11)
          if (n12 .lt. 0) n12 = -n12
          if (nodfum(n1) .ne. n12) cycle
          nodfum(n1) = n11
          go to 10230
       end do
    end do
    write (unit = lunit(6), fmt = 10222) trim (bus5)
10222 format (/, ' Error stop.   The node name  "busf" on the last-read card is not that', /, ' of any electric network source node. busf=', a)
    go to 9600
10230 do n10 = 1, ntot
       if (nshare .eq. 2 .and. bus6 .eq. blank) go to 10250
       if (bus6 .ne. bus(n10)) cycle
       do n11 = 1,kconst
          n12 = node(n11)
          if (n12 .lt. 0) n12 = - n12
          if (n10 .ne. n12) cycle
          crest(n11) = d1 * distrf
          if (n6 .ne. 0) go to 10231
          nodmum(n1) = n11
          if (distrf .eq. 0.0d0) crest(n11) = d1
10231     tstart(n11) = - 7777.
          go to 10250
       end do
    end do
    if (nshare .ne. 2) go to 90233
    write (unit = lunit(6), fmt = 90200) trim (bus6)
90200 format (/, ' Error stop. The node name', '"', a, '"', ' on the last-read card should be left blank.')
    write (unit = lunit(6), fmt = 90202) n1
90202 format (' this um -', i3, " is requested to share its mechanical network with other um's.  The", ' specification of all applied torques in this', /, ' mech network should have been placed with', " the data input for the lowest-numbered um of the set of um's sharing this mech network.")
    call stoptp
90233 write (unit = lunit(6), fmt = 10234) bus6
10234 format (/, ' Error stop.   The node name  "busm" on the last-read card is not that', /, ' of any electric network source node. busm=', a6)
    go to 9600
10250 call cimage
    read (unit = abuff, fmt = 10260) text3
10260 format (a6)
    if (text3 .ne. textm) go to 10300
    if (nshare .ne. 2) go to 90260
    write (unit = lunit(6), fmt = 90250) n1
90250 format (/, ' Error stop. This card 5 of class 2 um data cards of this um -', i3, ' should be removed.')
    write (unit = lunit(6), fmt = 90202) n1
    call stoptp
90260 n5 = n5 + 1
    n6 = 1
    read (unit = abuff, fmt = 10261) text3, bus6, distrf
10261 format (a6, 28x, a6, e14.5)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 10262) numum, n5, distrf
10262 format ('+um -', i3, '   mach card', i3, '.', e14.5)
    go to 10230
    ! . if flux output is desired :
    ! . user3 : coil-table input for m32 and newer
10300 if (iprsup .ge. 4) write (unit = lunit(6), fmt = 10301) numum
10301 format (/, ' *************************************** The coil-table input of um number', i4, ' :')
    n8 = 0
    ncldq = ncld(n1) + nclq(n1) + 3
    ! . adjustment of ncld and nclq for um type 4 :
    if (jtype(n1) .ne. 4) go to 10304
    ncld(n1) = 1
    nclq(n1) = 1
    ncldq = ncld(n1) + nclq(n1) + 4
10304 if (initum .ne. 0) go to 14303
13302 if (n8 .ge. ncldq) go to 11202
    if (n8 .eq. 0 .and. nshare .eq. 0) go to 14303
    call cimage
14303 ncltot = ncltot + 1
    fpar(ncltot) = 0.0
    if (ncltot .le. nclfix) go to 14305
    write (unit = lunit(6), fmt = 14304) ncltot
14304 format (/, ' Overflow u.m. coil table.   ncltot =', i5)
    go to 9600
14305 n8 = n8 + 1
    read (unit = abuff, fmt = 13303) gpar(ncltot), reacl(ncltot), bus1, bus2, bus6, jclout(ncltot), hist(ncltot)
13303 format (2e14.5, 3a6, i1, e14.5)
    if ( noutpr  .eq.  0 ) write (kunit6, 14306) numum, n8, gpar(ncltot), reacl(ncltot)
14306 format ('+um -', i3, '   coil card', i3, '.', 2e11.2)
    n11 = 0
    n12 = 0
    do n10 = 1,ntot
       if (n11 .ne. 0) go to 13304
       if (bus1 .ne. bus(n10)) go to 13304
       nodvo1(ncltot) = n10
       n11 = 1
       if (n12 .ne. 0) go to 13305
13304  if (bus2 .ne. bus(n10)) go to 13305
       nodvo2(ncltot) = n10
       n12 = 1
13305  n13 = n11 * n12
       if (n13 .ne. 0) go to 13307
    end do
    if (n11 .eq. 0) write (unit = lunit(6), fmt = 13206) bus1
13206 format (/, ' Error stop.  Just-read coil card bears illegal node name:', a6)
    if (n12 .eq. 0) write (unit = lunit(6), fmt = 13206) bus2
    go to 9600
13307 jcltac(ncltot) = 0
    if (bus6 .eq. blank) go to 13313
    do j = 1, ktab
       ndx1 = ilntab(klntab + j)
       if (bus6 .eq. texvec(ndx1)) go to 13311
    end do
    write (unit = lunit(6), fmt = 13207)  bus6
    go to 9600
13311 ndx1 = kxtcs + j
    jcltac(ncltot) = ndx1
13313 if (iprsup .ge. 4) write (unit = lunit(6), fmt = 13314) ncltot, gpar(ncltot), reacl(ncltot), bus1, bus2, bus6, jclout(ncltot), hist(ncltot)
13314 format (' ******** Coil card nr.', i3, ' :', 2e14.5, 3a6, i1, e14.5)
    go to 13302
    ! . at this point reading of all um data input is finished
    !   ******************************************************
    ! . calculation of real gpar :
500 do n1 = 1, ncltot
       if (gpar(n1) .ne. 0.0) gpar(n1) = 1.0 / gpar(n1)
    end do
    if (istep .ne. -4567) go to 80510
    if (initum .eq. 1) go to 80510
    write (unit = lunit(6), fmt = 80502)
80502 format (/, ' Error stop. You have requested the um to be initialized through a load-flow process. You have however failed to set up the um', /, ' data input for automatic steady-state initialization. Consult the EMTP rule book regarding this usage of the um.')
    call stoptp
80510 if (nsmach .eq. 0) go to 510
    ! warning if overflow of etac,ismtac and bustac
    n1 = ntotac - ntacb
    if (n1 .le. 10) go to 510
    write (unit = lunit(6), fmt = 504) n1
504 format (/, ' Warning: in using sm type-50 to 59 data input, you have requested a total of', i4)
    write (unit = lunit(6), fmt = 505)
505 format (/, ' to be passed to TACS. The default storage allocation is just for 10 of')
    write (unit = lunit(6), fmt = 506)
506 format (/, ' such variables because in deck synmac the arrays ismtac, etac, bustac are')
    write (unit = lunit(6), fmt = 507)
507 format (/, ' dimensioned to 10. Did you increase this dimension to be at least equal to this')
    write (unit = lunit(6), fmt = 508)
508 format (/, ' total nr of variables you want to transfer to TACS?  You should have.')
510 kcoil(1) = 1
    numout = 0
    if (loopss(8) .ne. 1) go to 530
    n1 = 6 * numum
    if (n1 .le. nclfix) go to 530
    write (unit = lunit(6), fmt = 520)
520 format (/, ' overflow of um coil table,  increase nclfix on um dimension card to be greater than 6 times the total nr. of used um machines.')
    go to 9600
    ! . machine looping for special processing of some parameters:
530 do n1 = 1, numum
       numoui = numout
       rppair = nppair(n1)
       if (jtype(n1) .eq. 13) jtmtac(n1) = 0
       if (initum .eq. 1) omegm(n1) = rotmom(n1) / rppair
       omold(n1) = omegm(n1)
       flxd(n1) = 0.0d0
       flxq(n1) = 0.0d0
       ! . omold initialization, torque angle for type 1 and 1,
       ! . and coil adjustment for three-phase exc. coils :
       if (jtype(n1) .gt. 2) go to 921
       thetam(n1) = (thetam(n1) + pi / 2.0) / rppair
       ! . set up of array kcoil(1:numum) :
921    if (n1 .eq. numum) go to 950
       n2 = n1 + 1
       n3 = 3 + ncld(n1) + nclq(n1)
       if (jtype(n1) .ne. 4) go to 940
       n3 = n3 + 1
940    kcoil(n2) = kcoil(n1) + n3
950    kcl = kcoil(n1)
       ! making sure that power coils are not tacs controlled :
       do n2 = 1, 3
          n3 = kcl - 1 + n2
          if (jcltac(n3) .eq. 0) cycle
          write (unit = lunit(6), fmt = 952) n1
952       format (/, ' Error stop. um number', i4, ' is provided with TACS controlled sources on the power side. This is only allowed for the coils on', /, ' the excitation side. TACS control of power side coils is to be done through the network which is connected to these coils.')
          call stoptp
       end do
       if (initum .eq. 0) go to 990
       do n2 = 1, 3
          n3 = kcl - 1 + n2
          ! preparing shift of leakage inductances of power coils *******
          ! note : the EMTP address it of these shifted inductances
          !        are stored in fpar(kcl).
          if (nodvo1(n3) .eq. nodvo2(n3)) cycle
          ! create new node behind leakage inductance. this node
          ! becomes the new um power node.
          ntot = ntot + 1
          kode(ntot) = 0
          bus(ntot) = trash
          if (n1 .gt. 5) go to 962
          n4 = 75 + (n1 - 1) * 3 + n2
          bus(ntot) = texta(n4)
          ! creation of the leakage inductance branche
962       call ibrinc
          it = it + 1
          length(ibr) = 1
          nr(ibr) = -it
          tr(it) = 0.0d0
          c(it) = 0.0d0
          d1 = reacl(kcl + 2)
          if (d1 .gt. reacl(kcl + 1)) d1 = reacl(kcl + 1)
          if (jtype(n1) .lt. 3) go to 966
          if (jtype(n1) .gt. 5) go to 966
          if (n2 .ne. 3) go to 966
          reacl(kcl + 1) = reacl(kcl + 1) - d1
          reacl(kcl + 2) = reacl(kcl + 2) - d1
966       if (xopt .eq. 0.0d0) tx(it) = d1 * 1.0d+3
          if (xopt .ne. 0.0d0) tx(it) = d1 * twopi*xopt
          if (n3 .eq. kcl) fpar(kcl) = ibr
          n4 = 3 * (n1 - 1)
          if (nodvo1(n3) .eq. 1) go to 972
          umcurp(n4 + n2) = nodvo1(n3)
          kbus(ibr) = nodvo1(n3)
          mbus(ibr) = ntot
          nodvo1(n3) = ntot
          go to 974
972       umcurp(n4 + n2) = nodvo2(n3)
          kbus(ibr) = nodvo2(n3)
          mbus(ibr) = ntot
          nodvo2(n3) = ntot
974       if (n2 .ne. 1) go to 976
          if (iprsup .ge. 1) write (unit = lunit(6), fmt = 975) n1
975       format (/, ' Additional branches created to move leakage induct to EMTP network for um number', i3, '.', /, 21x, 'node to node', 5x, 'ibr', 6x, 'it', 8x, 'tx(it)')
976       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 977) mbus(ibr), kbus(ibr), ibr, it, tx(it)
977       format (21x, i4, 4x, i4, 4x, i4, 4x, i4, e14.5)
       end do
       ! creation of speed capacitors for steady-state initialization:
       ntot = ntot + 1
       if (jtmtac(n1) .le. 0) jtmtac(n1) = -ntot
       bus(ntot) = trash
       if (n1 .lt. 4) bus(ntot) = texta(98 + n1)
       call ibrinc
       it = it + 1
       fpar(kcl+2) =  it
       length(ibr) = 1
       nr(ibr) = - it
       kbus(ibr) = nodom(n1)
       mbus(ibr) = ntot
       tr(it) = epsiln
       c(it) = 0.0d0
       tx(it) = 0.0d0
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 80980) mbus(ibr), kbus(ibr), ibr, it, tr(it), c(it)
80980  format (' ********* Potential speed cap : mbus =', i6, ' kbus =', i6, ' ibr =', i6, ' it =', i6, ' tr(it) =', e14.5, ' c =', e14.5)
       ! creation of sources to represent im excitation coils for
       ! steady-state initialization (accommodates kpsour use):
       if (jtype(n1) .lt. 3) go to 990
       if (jtype(n1) .gt. 7) go to 990
       n5 = kcl + 3
       n6 = kcl + 2 + ncld(n1) + nclq(n1)
       if (jtype(n1) .eq. 4) n6 = n6 + 1
       n11 = kconst + 1
       do j = n5, n6
          if (nodvo1(j) .eq. nodvo2(j)) cycle
          do n10 = 1, 2
             if (n10 .eq. 2) go to 982
             if (nodvo1(j) .eq. 1) cycle
             kconst = kconst + 1
             n20 = nodvo1(j)
             node(kconst) = -n20
             kode(n20) = 0
             go to 984
982          if (nodvo2(j) .eq. 1) cycle
             kconst = kconst + 1
             n20 = nodvo2(j)
             node(kconst) = -n20
             kode(n20) = 0
984          if (kconst .eq. n11) fpar(kcl+1) = kconst
             iform(kconst) = 14
             crest(kconst) = 0.0d0
             time1(kconst) = 0.0d0
             tstart(kconst) = -1.0d0
             tstop(kconst) = 0.0d0
             sfreq(kconst) = 0.0001d0
             if ((iprsup .ge. 1) .and. (j .eq. n5)) write (unit = lunit(6), fmt = 985)
985          format (' ********* Steady-state curr sources for im excit coils :    node  kconst         sfreq         crest         tstop')
             if (iprsup .ge. 1) write (unit = lunit(6), fmt = 986) node(kconst), kconst, sfreq(kconst), crest(kconst), tstop(kconst)
986          format (' *********', 47x, 2i8, 3e14.5)
          end do
       end do
990    n2 = kcoil(n1)
       n3 = n2 + ncld(n1) + nclq(n1) + 2
       if (jtype(n1) .eq. 4) n3 = n3 + 1
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 992) n1
992    format (/, ' Electric terminal nodes for um nr.', i3, ': ')
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 993) (nodvo1(n4), n4 = n2, n3)
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 994) (nodvo2(n4), n4 = n2, n3)
993    format (' nodvo1 :', 3x, 15i4)
994    format (' nodvo2 :', 3x, 15i4)
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 995) nodom(n1), nodmum(n1), nodfum(n1)
995    format (/, ' um mass node nr. = nodom(jm) =', i4, /, ' slack buses :', 3x, 'nodmum(jm) =', i4, 10x, 'nodfum(jm) =', i4)
       ! . determination of pointers for output vector :
       if (jtqout(n1) .eq. 0) go to 1000
       numout = numout + 1
       kumout(numout) = n1
       jumout(numout) = 1 + numfix
       if (jtqout(n1) .lt. 2) go to 1000
       numout = numout + 1
       kumout(numout) = n1
       jumout(numout) = 4 + numfix
       if (jtqout(n1) .ne. 3) go to 1000
       numout = numout + 1
       kumout(numout) = n1
       jumout(numout) = 5 + numfix
1000   if (jomout(n1) .eq. 0) go to 1010
       numout = numout + 1
       kumout(numout) = n1
       jumout(numout) = 2 + numfix
       if (jomout(n1) .lt. 2) go to 1010
       numout = numout + 1
       kumout(numout) = n1
       jumout(numout) = 6 + numfix
       if (jomout(n1) .ne. 3) go to 1010
       numout = numout + 1
       kumout(numout) = n1
       jumout(numout) = 7 + numfix
1010   if (jthout(n1) .eq. 0) go to 1020
       numout = numout + 1
       kumout(numout) = n1
       ! if output request for thetam (torque angle or rotor position)
       if (jthout(n1) .eq. 1) jumout(numout) = 3 + numfix
       ! if output request for tqexc (for sm type-59 data input)
       if (jthout(n1) .eq. 3) jumout(numout) = 11 + numfix
1020   n2 = 3 + ncld(n1) + nclq(n1)
       if (jtype(n1) .ne. 4) go to 1030
       n2 = n2 + 1
1030   do n3 = 1, n2
          n4 = kcoil(n1) - 1 + n3
          if (reacl(n4) .ge. 0.0d0) go to 1038
          write (unit = lunit(6), fmt = 1032) n1
1032      format (/, ' Error stop. A negative leakage inductance inductance is used to simulate machine number', i4, '.')
          go to 9600
1038      if (jclout(n4) .eq. 0) cycle
          numout = numout + 1
          kumout(numout) = n1
          jumout(numout) = 11 + numfix + n3
          if (jclout(n4) .ne. 2) cycle
          if (n3 .eq. 1) jumout(numout) = numfix + 8
          if (n3 .eq. 2) jumout(numout) = numfix + 9
          if (n3 .eq. 3) jumout(numout) = numfix + 10
       end do
       if (numout .eq. numoui) cycle
       if (n1 .eq. 1) numbus = jumout(numout)
       n11 = jumout(numout)
       if (n11 .gt. numbus) numbus = n11
    end do
    if (numbus .le. ibsfix) go to 1060
    write (unit = lunit(6), fmt = 1055)
1055 format (/, ' Overflow of um output name table,  increase ibsfix on um dimension card.')
    go to 9600
    ! shifting entries of umoutp vector because of sm type-59
    ! request for transfer of variables to tacs :
1060 if (nsmach .eq. 0) go to 18010
    if (nsmtac .eq. 0) go to 18010
    do n5 = 1, nsmtac
       n6 = nsmtac + 1 - n5
       n7 = numout + 3 + n6
       umoutp(n7) = umoutp(n6)
    end do
    do n5 = 1,numout
       umoutp(n5) = 0.0d0
    end do
    n5 = numout + nsmtac + 3
    umoutp(numout + 3) = n5
    ! umoutp(numout+3) is last entry of umoutp that is used
    umoutp(numout + 2) = nangre
    ! umoutp(numout+2) .ne. 0.0 indicates request for angle transf.
    umoutp(numout + 1) = -9999.0d0
    ! umoutp(numout+1) .eq. -9999. is a flag for request of tacs
    ! transfer of um variables.
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 18006) (umoutp(n1), n1 = 1, n5)
18006 format (/, ' The um output table umoutp(numout + nsmtac + 3):', /, 6(6x,e14.5), /, (6(6x, e14.5)))
18010 istart = 0
    if (numout .le. iotfix) go to 18020
    write (unit = lunit(6), fmt = 17960)
17960 format (/, ' Overflow of um output table, increase in card for absolute um dimensions', /, ' value of iotfix.', /, ' Remark : if sm type-59 data input is included, then iotfix is also', /, ' related to outputs of mech. Systems in all sm type-59 data machines.')
    go to 9600
18020 loopss(1) = 0
    loopss(2) = 0
    loopss(4) = 0
    loopss(10) = 0
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 18040) numum, numout, nsmtac
18040 format (/, ' Exit  "umdata" :  numum  numout  nsmtac', /, 17x, 3i8)
    return
9600 call stoptp
    return
  end subroutine umdata

  !
  ! subroutine umdatb.
  !

  subroutine umdatb (reacl, gpar, fpar, nodvo1, nodvo2, jcltac, jtype, nodom, ncld, jtmtac, reamdu, reamds, flxds, flxdr, reamqu, flxqs, flxqr, jcdsat, jcqsat, nppair, rotmom, nclq, jtqout, jthout, reamqs, voltum, anglum, nodfum, nodmum, kumout, jumout, jclout, dcoef, jomout, umoutp)
    use umlocal
    use blkcom
    use labcom
    use umcom
    use tacsar
    use smtacs
    implicit none
    character(8) :: text3, text4, text5
    character(8) :: textm, tesm1, tesm2, tesm3, tesm4, tesm5
    character(8) :: tesm6, tesm7, tesm8, tesm9
    integer(4), intent(out) :: jcdsat(:)
    integer(4), intent(out) :: jclout(:)
    integer(4), intent(out) :: jcltac(:)
    integer(4), intent(out) :: jcqsat(:)
    integer(4), intent(out) :: jomout(:)
    integer(4), intent(out) :: jthout(:)
    integer(4), intent(out) :: jtmtac(:)
    integer(4), intent(out) :: jtqout(:)
    integer(4), intent(out) :: jtype(:)
    integer(4), intent(out) :: jumout(:)
    integer(4), intent(out) :: kumout(:)
    integer(4), intent(out) :: ncld(:)
    integer(4), intent(out) :: nclq(:)
    integer(4), intent(out) :: nodfum(:)
    integer(4), intent(out) :: nodmum(:)
    integer(4), intent(out) :: nodom(:)
    integer(4), intent(out) :: nodvo1(:)
    integer(4), intent(out) :: nodvo2(:)
    integer(4), intent(out) :: nppair(:)
    real(8), intent(out) :: anglum(:)
    real(8), intent(out) :: dcoef(:)
    real(8), intent(out) :: flxdr(:)
    real(8), intent(out) :: flxds(:)
    real(8), intent(out) :: flxqr(:)
    real(8), intent(out) :: flxqs(:)
    real(8), intent(out) :: fpar(:)
    real(8), intent(out) :: gpar(:)
    real(8), intent(out) :: reacl(:)
    real(8), intent(out) :: reamds(:)
    real(8), intent(out) :: reamqs(:)
    real(8), intent(out) :: reamdu(:)
    real(8), intent(out) :: reamqu(:)
    real(8), intent(out) :: rotmom(:)
    real(8), intent(out) :: umoutp(:)
    real(8), intent(out) :: voltum(:)
    !  dimension nodvo2(1), jtmtac(1), flxqs(1), rotmom(1)
    !  dimension reacl(1),gpar(1),fpar(1),nodvo1(1),nodfum(1)
    !  dimension jcltac(1), jclout(1), jtype(1), nodom(1)
    !  dimension reamds(1), flxds(1), flxdr(1), reamqu(1)
    !  dimension flxqr(1), jcdsat(1), jcqsat(1), nppair(1)
    !  dimension ncld(1), nclq(1), jtqout(1),jomout(1),jthout(1)
    !  dimension reamqs(1),dcoef(1),voltum(1),anglum(1)
    !  dimension nodmum(1), kumout(1), jumout(1), umoutp(1)
    !  dimension reamdu(1)
    integer(4) :: i, ijk, itexc
    integer(4) :: j
    integer(4) :: kswexc, kswsta
    integer(4) :: mjm
    integer(4) :: n20, n1iu, ndx1, ndy5
    integer(4) :: niunrs, nk
    integer(4) :: ntacb, ntotst, num
    real(8) :: d10, d11, d12, distr1
    real(8) :: texxt3, tqpum
    real(8) :: xdkdum, xfkdum
    !
    data textm / 'more  ' /
    data tesm1, tesm2, tesm3, tesm4, tesm5, tesm6, tesm7, tesm8, tesm9 / 'smdata', 'tolera', 'parame', 'all   ', 'none  ' , 'extend', 'finish', ' part ', 'share ' /
    ! start reading sm type -50 to 59 data input format **********
    initum = 1
    nsmach = numum
    n1 = numum
    jtype(n1) = 13
    ncld(n1) = 2
    nclq(n1) = 2
    jtmtac(n1) = 0
    nmexc = 0
    n5 = ncltot + 1
    n6 = ncltot + 7
    do n10 = n5, n6
       fpar(n10) = 0.0d0
    end do
    ! *************************** read class 1 sm type-59 data
    do n5 = 1, 3
       if (n5 .ne. 1) call cimage
       n6 = ncltot + n5
       read (unit = abuff, fmt = 17050) n19, bus1, d1, d2, d3
17050  format (i2, a6, 2x, 3e10.6)
       if (noutpr .eq. 0) write (unit = kunit6, fmt = 17052) numum, n5
17052  format ('+um -', i3, '   sm-59 class 1, card ', i1)
       if (n5 .ne. 1) go to 17056
       ntypsm = n19
       if (n19 .eq. 59) go to 17054
       if (n19 .eq. 50) go to 17054
       if (n19 .eq. 52) go to 17054
       write (unit = lunit(6), fmt = 17053)
17053  format (/, ' Error stop. The um code does not accept dual machines.')
       go to 9600
17054  voltum(n1) = d1
       anglum(n1) = d3
       if (d2 .eq. 0.0d0) d2 = statfr
       rotmom(n1) = twopi * d2
       stat59 = d2
       ! creation of 2 excitation nodes :
       ntot = ntot + 1
       kode(ntot) = 0
       nodvo1(ncltot + 4) = ntot
       nodvo2(ncltot + 4) = 1
       bus(ntot) = trash
       ntot = ntot + 1
       kode(ntot) = 0
       nexc = ntot
       bus(nexc) = trash
       if (numum .gt. 5) go to 17056
       n3 = 41 + (numum - 1) * 4
       bus(ntot-1) = texta(n3 + 1)
       bus(nexc) = texta(n3 + 2)
17056  do n10 = 1, ntot
          if (bus1 .ne. bus(n10)) cycle
          nodvo1(n6) = n10
          go to 17070
       end do
       write (unit = lunit(6), fmt = 17062) bus1
17062  format (/, ' Error stop.   Just-read sm class 1 card bears illegal node name:', a6)
       go to 9600
17070  continue
    end do
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17072) n1
17072 format (/, ' ********* Elements internally created for um nr.', i3, ': node to node', 5x, 'ibr', 6x, 'it' ,2x, 'kswtch', 2x, 'lswtch', 2x, 'kconst', 9x, 'sfreq', /)
    ! set voltage source for excitation
    kconst = kconst + 1
    kconex = kconst
    nodfum(n1) = kconst
    iform(kconst) = 14
    node(kconst) = nexc
    kode(nexc) = - nexc
    sfreq(kconst) = epsiln * 1.0d+3
    tstart(kconst) = -1.0d0
    tstop(kconst) = fltinf
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17076) node(kconst), kconst, sfreq(kconst)
17076 format (' ********* Field voltage source', 23x, i4, 44x, i4, e14.5)
    ! set series and parallel resistance for excitation
    call ibrinc
    ibrexc = ibr
    it = it + 1
    kbus(ibr) = nodvo1(ncltot + 4)
    mbus(ibr) = nexc
    length(ibr) = 1
    nr(ibr) = - it
    tr(it) = epsiln
    itexc = it
    ! note : tr(it=itexc) will be changed later to 0.5 * rf
    tx(it) = 0.0d0
    c(it) = 0.0d0
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17080) mbus(ibr), kbus(ibr), ibr, it
17080 format (' ********* Series field resist.', 23x, i4, 4x, i4, 4x, i4, 4x, i4)
    call ibrinc
    it = it + 1
    kbus(ibr) = nexc
    mbus(ibr) = 1
    length(ibr) = 1
    nr(ibr) = - it
    tr(it) = epsiln
    tx(it) = 0.0
    c(it) = 0.0
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17082) mbus(ibr),kbus(ibr),ibr,it
17082 format (' ********* Parall field resist.', 23x, i4, 4x, i4, 4x, i4, 4x, i4)
    ! shorting damper and eddy current coils
    do n5 = 5, 7
       nodvo1(ncltot + n5) = 1
       nodvo2(ncltot + n5) = 1
    end do
    ! *************************** read class 2 sm type-59 data
    ! nstan = 1 is a flag for standard manufacturer data
    nstan = 0
    call cimage
    read (unit = abuff, fmt = 17102) text3
17102 format (a6)
    if (text3 .ne. tesm2) go to 17106
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17104)  numum
17104 format ('+um -', i3, '   sm-59 class 2, tolerance card')
    call cimage
17106 n19 = 1
    read (unit = abuff, fmt = 17107) text3, fmum
17107 format (a6, 18x, e8.0)
    if (text3 .ne. tesm3) go to 17200
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17108)  numum
17108 format ('+um -', i3, '   sm-59 class 2, par fit request')
    nstan = 1
    call cimage
    ! *************************** read class 3 sm type-59 data
17200 n2 = 0
    read (unit = abuff, fmt = 17210) numasu, nmgen, nmexc, n20, d17, rmvaum, rkvum, agldum, s1um, s2um
17210 format (3i2, i4, f6.0, 14x, 5e10.6)
    if (numasu .eq. 0) numasu = int (d17)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17212)  numum
17212 format ('+um -', i3, '   sm-59 class 3, general parameters')
    ! zlsbum is the stator base inductance, if multiplied with
    ! rotmom(n1), it becomes the stator base impedance.
    rmvaum = rmvaum * 1.0d+6
    rkvum = rkvum * 1.0d+3
    zlsbum = rkvum * rkvum
    zlsbum = zlsbum/(rotmom(n1) * rmvaum)
    jcdsat(n1) = 0
    if (agldum .lt. 0.0d0) jcdsat(n1) = 1
    if (agldum .lt. 0.0d0) agldum = - agldum
    nppair(n1) = n20 / 2
    n5 = 2
    n6 = nppair(n1) * n5
    if (n6 .eq. n20) go to 17218
    write (unit = lunit(6), fmt = 17216) n1
17216 format (/, ' Error stop. Erroneous data input because an uneven number of poles has been specified for mach number', i4, '.')
    go to 9600
17218 call cimage
    read (unit = abuff, fmt = 17222) text3, text4, text5, aglqum, s1qum, s2qum
17222 format (3a6, 32x, 3e10.6)
    if (text3 .ne. blank) go to 17224
    if (text4 .ne. blank) go to 17224
    if (text5 .ne. blank) go to 17224
    go to 17226
17224 aglqum = 0.0d0
    s1qum = 0.0d0
    s2qum = 0.0d0
    go to 17230
17226 if (noutpr .eq. 0) write (unit = kunit6, fmt = 17228) numum
17228 format ('+um -', i3, '   sm-59 class 3, q-axis saturation card')
    call cimage
17230 jcqsat(n1) = 0
    d1 = s1qum + s2qum
    if (d1 .ne. 0.0d0) jcqsat(n1) = 1
    if (aglqum .ge. 0.0d0) go to 17240
    jcqsat(n1) = 1
    aglqum = 1.6d0 * agldum
    s1qum = 1.5d0 * s1um
    s2qum = 1.5d0 * s2um
    ! reading standard manufacturer data :
17240 if (ntypsm .eq. 52) go to 17400
    if (ntypsm .eq. 53) go to 17400
    if (ntypsm .eq. 50) go to 17241
    if (ntypsm .eq. 51) go to 17241
    if (nstan .ne. 1) go to 17400
17241 n2 = 0
    read (unit = abuff, fmt = 17242) raum, xlum, xdum, xqum, xdpum, xqpum, xdppum, xqppum
17242 format (8e10.6)
    n2 = n2 + 1
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17244) numum, n2
17244 format ('+um -', i3, '   sm-59 class 3, manufacture card ', i1)
    call cimage
    read (unit = abuff, fmt = 17246) tdpum, tqpum, tdppum, tqppum, x0um, rnum, xnum, netrum
17246 format (7e10.6, i10)
    n2 = n2 + 1
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17244) numum, n2
    ! conversion of pu system to stator refered si system :
    raum = raum * zlsbum * rotmom(n1)
    xlum = xlum * zlsbum
    xdum = xdum * zlsbum
    xqum = xqum * zlsbum
    xdpum = xdpum * zlsbum
    xqpum = xqpum * zlsbum
    xdppum = xdppum * zlsbum
    xqppum = xqppum * zlsbum
    x0um = x0um * zlsbum
    rnum = rnum * zlsbum * rotmom(n1)
    xnum = xnum * zlsbum
    ! establishing stator and mach table variables
    if (raum .eq. 0.0d0) raum = epsiln
    do n5 = 1,3
       gpar(ncltot + n5) = raum
       if (n5 .eq. 1) cycle
       reacl(ncltot + n5) = xlum
    end do
    reacl(ncltot + 1) = x0um
    reamdu(n1) = xdum - xlum
    reamqu(n1) = xqum - xlum
    if (reamqu(n1) .gt. 0.0d0) go to 17262
    write (unit = lunit(6), fmt = 6198) n1
6198 format (/, ' Error stop. Incorrect um data input. The result is that the q-axis main inductance', /, 7x, ' is either zero or negative for machine number', i4, '.')
    go to 9600
17262 if (xqum .ne. xqpum) go to 17290
    if (xqum .ne. xqppum) go to 17280
    gpar(ncltot + 6) = 0.0d0
    gpar(ncltot + 7) = 0.0d0
    reacl(ncltot + 6) = 1.0d0
    reacl(ncltot + 7) = 1.0d0
    go to 17290
17280 if (fmum .ne. 1.0d0) go to 17285
    gpar(ncltot + 6) = 0.0d0
    reacl(ncltot + 6) = 1.0d0
17285 if (fmum .eq. 0.0d0) fmum = 0.95d0
    if (fmum .lt. 1.0d0) xqpum = fmum * xqum
17290 if (xdum .ne. xdpum) go to 17310
    if (xdum .ne. xdppum) go to 17300
    gpar(ncltot + 4) = 0.0d0
    gpar(ncltot + 5) = 0.0d0
    reacl(ncltot + 4) = 1.0d0
    reacl(ncltot + 5) = 1.0d0
    go to 17310
17300 if (fmum .ne. 1.0d0) go to 17305
    gpar(ncltot + 4) = 0.0d0
    reacl(ncltot + 4) = 1.0d0
17305 if (fmum .eq. 0.0) fmum = 0.3d0
    if (fmum .lt. 1.0d0) xdpum = fmum * xdum
    ! establishing rotor variables :
17310 d1 = reamdu(n1) * reamdu(n1)
    if (reacl(ncltot + 4) .ne. 1.0d0) go to 17320
    if (reacl(ncltot + 5) .eq. 1.0d0) go to 17330
    d10 = d1 / (xdum - xdppum)
    reacl(ncltot + 5) = d10 - reamdu(n1)
    d10 = reamdu(n1) + reacl(ncltot + 5)
    gpar(ncltot + 5) = d10 / tdppum
    go to 17330
17320 d10 = d1 / (xdum - xdpum)
    reacl(ncltot + 4) = d10 - reamdu(n1)
    d10 = reamdu(n1) + reacl(ncltot + 4)
    gpar(ncltot + 4) = d10 / tdpum
    if (reacl(ncltot + 5) .eq. 1.0) go to 17330
    d11 = xdum - xdppum + d10 - 2.0 * reamdu(n1)
    d11 = d11 * d1
    d12 = (xdum - xdppum) * d10 - d1
    reacl(ncltot + 5) = d11 / d12 - reamdu(n1)
    d11 = reamdu(n1) + reacl(ncltot + 5)
    d12 = d1 / d10
    gpar(ncltot + 5) = (d11 - d12) / tdppum
17330 d1 = reamqu(n1) * reamqu(n1)
    if (reacl(ncltot + 6) .ne. 1.0d0) go to 17340
    if (reacl(ncltot + 7) .eq. 1.0d0) go to 17500
    d10 = d1 / (xqum - xqppum)
    reacl(ncltot + 7) = d10 - reamqu(n1)
    d10 = reamqu(n1) + reacl(ncltot + 7)
    gpar(ncltot + 7) = d10 / tqppum
    go to 17500
17340 d10 = d1 / (xqum - xqpum)
    reacl(ncltot + 6) = d10 - reamqu(n1)
    d10 = reamqu(n1) + reacl(ncltot + 6)
    gpar(ncltot + 6) = d10 / tqpum
    if (reacl(ncltot + 7) .eq. 1.0d0) go to 17500
    d11 = xqum - xqppum + d10 - 2.0d0 * reamqu(n1)
    d11 = d11 * d1
    d12 = (xqum - xqppum) * d10 - d1
    reacl(ncltot + 7) = d11 / d12 - reamqu(n1)
    d11 = reamqu(n1) + reacl(ncltot + 7)
    d12 = d1 / d10
    gpar(ncltot + 7) = (d11 - d12) / tqppum
    go to 17500
    ! processing standard manufacturer data completed.
    ! reading of inductance/resistance data input :
17400 n2 = 0
    read (unit = abuff, fmt = 17410) xfum, xdfum, xfkdum, xdum, xdkdum, xkdum
17410 format (6e10.6)
    n2 = n2 + 1
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17412) numum, n2
17412 format ('+um -', i3, '   sm-59 class 3, R and L param card ', i1)
    call cimage
    read (unit = abuff, fmt = 17420) xkqum, xqkqum, xgkqum, xqum, xqgum, xgum, netrum
17420 format (6e10.6, 10x, i10)
    n2 = n2 + 1
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17412)  numum, n2
    call cimage
    read (unit = abuff, fmt = 17430) reacl(ncltot + 1), gpar(ncltot + 1), gpar(ncltot + 4), gpar(ncltot + 5), gpar(ncltot + 6), gpar(ncltot + 7), rnum, xnum
17430 format (8e10.6)
    n2 = n2 + 1
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17412) numum, n2
    ! conversion pu system into stator refered si system and um
    ! parameter input
    rnum = rnum * zlsbum * rotmom(n1)
    xnum = xnum * zlsbum
    if (gpar(ncltot + 1) .eq. 0.0) gpar(ncltot + 1) = epsiln
    gpar(ncltot + 2) = gpar(ncltot + 1)
    gpar(ncltot + 3) = gpar(ncltot + 1)
    d1 = zlsbum * rotmom(n1)
    do n5 = 1, 7
       gpar(ncltot + n5) = gpar(ncltot + n5) * d1
    end do
    reamdu(n1) = xdfum
    reamqu(n1) = xqkqum
    reacl(ncltot + 1) = reacl(ncltot + 1) * zlsbum
    reacl(ncltot + 2) = (xdum - reamdu(n1)) * zlsbum
    reacl(ncltot + 3) = (xqum - reamqu(n1)) * zlsbum
    reacl(ncltot + 4) = (xfum - reamdu(n1)) * zlsbum
    reacl(ncltot + 5) = (xkdum - reamdu(n1)) * zlsbum
    reacl(ncltot + 6) = (xkqum - reamqu(n1)) * zlsbum
    reacl(ncltot + 7) = (xgum - reamqu(n1)) * zlsbum
    reamdu(n1) = reamdu(n1) * zlsbum
    reamqu(n1) = reamqu(n1) * zlsbum
    ! reduction factor 1 / kf = ifb / isb, stored in dcoef(n1) :
17500 dcoef(n1) = reamdu(n1) * agldum * rotmom(n1) / rkvum
    ! start processing saturation :
    flxds(n1) = 0.0d0
    flxqs(n1) = 0.0d0
    reamds(n1) = 0.0d0
    reamqs(n1) = 0.0d0
    flxdr(n1) = 0.0d0
    flxqr(n1) = 0.0d0
    if (jcdsat(n1) .eq. 0) jcqsat(n1) = 0
    if (jcdsat(n1) .eq. 0) go to 17510
    reamds(n1) = 0.2d0 * reamdu(n1) * agldum / (s2um - s1um)
    d1 = reamds(n1) - epsiln
    if (d1 .le. reamdu(n1)) go to 17504
    write (unit = lunit(6), fmt = 17502)
17502 format (/, ' Error stop. You have chosen incorrect d - axis saturation parameters s1 and s2.')
    write (unit = lunit(6), fmt = 17503) n1
17503 format (/, ' The result is a saturated inductance greater than the unsaturated one. The machine concerned is um number ', i4, '.', /, ' please take realistically either a lower value for s1 or a higher value for s2.')
    go to 9600
17504 flxds(n1) = (s2um - 1.2 * s1um) * rkvum / rotmom(n1)
    d2 = s2um - s1um - 0.2d0 * agldum
    if (d2 .ne. 0.0d0) go to 17505
    jcdsat(n1) = 0.0d0
    jcqsat(n1) = 0.0d0
    go to 17510
17505 flxds(n1) = flxds(n1) / d2
    d10 = 1.0d0 - reamds(n1) / reamdu(n1)
    d10 = d10 * flxds(n1)
    if (d10 .ge. 0.0d0) go to 97510
    write (unit = lunit(6), fmt = 97506)
97506 format (/, ' Error stop.  You have chosen incorrect d-axis saturation parameters s1 and s2.')
    write (unit = lunit(6), fmt = 97507) n1
97507 format (' The result is an unrealistic saturation characteristic with the saturated line-segment not intersecting the unsaturated line-segment', /, ' in the first quadrant. The machine concerned is um number', i4, '.')
    go to 9600
97510 if (jcqsat(n1) .eq. 0) go to 17510
    reamqs(n1) = 0.2*reamqu(n1)*aglqum/(s2qum-s1qum)
    d1 = reamqs(n1) - epsiln
    if (d1 .le. reamqu(n1)) go to 17508
    write (unit = lunit(6), fmt = 17506)
17506 format (/, ' Error stop. You have chosen incorrect q - axis saturation parameters s1 and s2.')
    write (unit = lunit(6), fmt = 17503) n1
    go to 9600
17508 flxqs(n1) = (s2qum-1.2*s1qum) * rkvum/rotmom(n1)
    d2 = s2qum - s1qum - 0.2*aglqum
    if (d2 .ne. 0.0d0) go to 17509
    jcqsat(n1) = 0.0d0
    go to 17510
17509 flxqs(n1) = flxqs(n1)/d2
    d10 = 1.0d0 - reamqs(n1) / reamqu(n1)
    d10 = d10 * flxqs(n1)
    if (d10 .ge. 0.0d0) go to 17510
    write (unit = lunit(6), fmt = 97520)
97520 format (/, ' Error stop.  You have chosen incorrect q-axis saturation paramaters s1 and s2.')
    write (unit = lunit(6), fmt = 97507) n1
    go to 9600
    ! start processing neutral element values :
17510 d2 = 1.0d+2
    if (netrum .eq. 1) rnum = d2
    d1 = rnum + xnum * 1.0d+3
    if (d1 .gt. d2) rnum = d2
    if (d1 .gt. d2) xnum = 0.0d0
    n7 = 1
    !  creation of neutral node
    do n5 = 1,3
       n6 = ncltot + n5
       if (d1 .eq. 0.0d0) go to 17520
       if (n5 .gt. 1) go to 17520
       ntot = ntot + 1
       kode(ntot) = 0
       n7 = ntot
       bus(ntot) = trash
       if (numum .gt. 5) go to 17520
       bus(ntot) = texta(numum + 60)
17520  nodvo2(n6) = n7
    end do
    ! create high resistances in parallel over coils if
    if (loopss(8) .ne. 0) go to 17536
    do n5 = 1, 3
       call ibrinc
       it = it + 1
       kbus(ibr) = nodvo1(ncltot + n5)
       mbus(ibr) = nodvo2(ncltot + n5)
       length(ibr) = 1
       nr(ibr) = - it
       tr(it) = 1.0d+8
       tx(it) = 0.0d0
       c(it) = 0.0d0
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17531) mbus(ibr), kbus(ibr), ibr, it
17531  format (' ********* High power resist.', 25x, i4, 4x, i4, 4x, i4, 4x, i4)
    end do
    ! insertion of neutral impedance
17536 if (d1 .eq. 0.0d0) go to 17540
    call ibrinc
    it = it + 1
    kbus(ibr) = nodvo2(ncltot + 1)
    mbus(ibr) = 1
    length(ibr) = 1
    nr(ibr) = -it
    tr(it) = rnum
    if (xopt .eq. 0.0d0) tx(it) = xnum * 1.0d+3
    if (xopt .ne. 0.0d0) tx(it) = xnum * twopi * xopt
    c(it) = 0.0d0
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17539) mbus(ibr), kbus(ibr), ibr, it
17539 format (' ********* Neutral impedance', 26x, i4, 4x, i4, 4x, i4, 4x, i4)
    !  set 0.5*rf for external exciter resistance:
    !    (internal field resistance adjusted at 17958)
17540 tr(itexc) = 0.5d0 * gpar(ncltot + 4) / (dcoef(n1) * dcoef(n1))
    !  *************************** read class 4 sm type-59 data
    d10 = 1.0d+6
    nsmtpr = nsmtac
    nrsyn = 0
    ntorq = 0
    !   ntotst + k = emtp node nr of mass nr. k
    !   nmgen now becomes emtp node nr of generator mass
    !   nmexc now becomes emtp node nr of exciter mass
    jtmtac(n1) = 0
    call cimage
    read (unit = abuff, fmt = 17550) text3
17550 format (a6)
    if (text3 .ne. tesm9) go to 17570
    read (unit = abuff, fmt = 17552) n3, n4
17552 format (6x,2i6)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17553) numum
17553 format ('+um -', i3, '   sm-59 class 4, share mech netw')
    if ((n1 .ne. n3) .and. (n1 .ne. n4)) go to 17560
    write (unit = lunit(6), fmt = 15827) n1
15827 format (/, " Error stop. This um-', i3, ' is supposed to share its mech network with other um's. What should be specified on this card are the", /, " numbers of these other um's without including the number of the um which is being processed right now.")
    go to 9600
17560 n6 = n1
    if ((n3 .ne. 0) .and. (n3 .lt. n6)) n6 = n3
    if ((n4 .ne. 0) .and. (n4 .lt. n6)) n6 = n4
    ! only the lowest numbered um of the machines sharing
    ! a common mech network are specified with the mech network
    ! structure (through the mass cards of class 4). the flag
    ! jtmtac(n1)=-999999 indicates that um number n1 is not the
    ! this lowest numbered um.
    if (n1 .eq. n6) jtmtac(n1) = -ntot
    if (n1 .ne. n6) jtmtac(n1) = -999999
    ntotst = -jtmtac(n6)
    go to 17571
17570 ntotst = ntot
17571 nmgen = ntotst + nmgen
    if (nmexc .eq. 0) go to 17575
    nmexc = ntotst + nmexc
    !. storing in fpar and create current source for "tqexc" :
    kconst = kconst + 1
    iform(kconst) = 14
    node(kconst) = -nmexc
    kode(nmexc) = 0
    crest(kconst) = 0.0d0
    tstart(kconst) = -1.0d0
    tstop(kconst) = fltinf
    sfreq(kconst) = 0.00001d0
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17572) node(kconst), kconst, sfreq(kconst)
17572 format (' ********* Exciter torque source', 22x, i4, 44x, i4, e14.5)
    fpar(ncltot + 4) = -kconst
    fpar(ncltot + 5) = nmexc
    fpar(ncltot + 6) = nexc
17575 nodom(n1) = nmgen
    ! set all mechanical nodes
    if (jtmtac(n1) .eq. -999999) go to 17700
    ntot = ntot + 2 * numasu
    n6 = ntotst + 1
    do n5 = n6, ntot
       kode(n5) = 0
    end do
    ! nsmtpr is to store the value of nsmtac of the previous mach.
    ! nsmtac is the total nr of variables to be passed to tacs.
    ! ntorq is the number of masses with distrf .ne. 0.0 , thus
    ! for the masses with an externally applied torque.
    ! Storage approach in umoutp for each sm :
    ! the first numasu cells behind nsmtpr is for distrf.
    ! the second numasu cells for l12 (and of course l23,etc).
    ! the third numasu cells for l12/r12 .
    ! then the rest for quantities reflecting tacs transfer.
    ! after completion of reading all sm data, the 3*numasu
    ! cells behind nsmtpr are destroyed and the quantities
    ! for tacs transfer are shifted to behind nsmtpr.
    ! Storage approach for each variable to be transfered :
    ! * for voltages (speeds) :
    ! first cell = code nr provided with neg sign
    ! second cell = node nr
    ! third cell = tacs table nr (ntotac)
    ! * for currents (torques) :
    ! same as voltages, except third cell = switch table nr.
    ! * for mass angles :
    ! first, second and third cell same as voltages
    ! fourth cell = angle history, angle part
    ! fifth cell = angle history, omega*deltat/2 part
    do n5 = 1, numasu
       if (n5 .eq. 1 .and. jtmtac(n1) .eq. 0) go to 17582
       call cimage
17582  n17 = 0
       read (unit = abuff, fmt = 17600) mlum, n17, distrf, hjum, dsynum, dmutum, spring, dabsum, bus1
17600  format (i2, i6, 2x, 6e10.6, a6)
       if (mlum .eq. 0) mlum = n17
       if (noutpr .eq. 0) write (unit = kunit6, fmt = 17602) numum, mlum
17602  format ('+um -', i3, '   sm-59 class 4, mass nr.', i6)
       if (mlum .le. numasu) go to 17604
       write (unit = lunit(6), fmt = 17603) mlum, numasu
17603  format (/, ' Error stop. The last card indicates the data of mass number', i6, ', and yet in class-3 of', /, ' of sm data cards you have specified the total number of masses "numas" to be equal to', i6, '.', /, ' Please remove this conflict.')
       go to 9600
       ! output names of mechanical nodes :
17604  bus(ntotst + mlum) = bus1
       ! creating mass element
       call ibrinc
       it = it + 1
       kbus(ibr) = ntotst + mlum
       mbus(ibr) = 1
       length(ibr) = 1
       nr(ibr) = -it
       tr(it) = 0.0d0
       tx(it) = 0.0d0
       ! the following factor of d10=1.0d+6 is due to micro f or mho.
       c(it) = d10 * d10 * hjum / 23.73
       if (copt .ne. 0.0d0) c(it) = c(it) * copt * twopi
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17605) mlum, mbus(ibr), kbus(ibr), ibr, it
17605  format (' ********* Mass branch nr.', i3, 25x, i4, 4x, i4, 4x, i4, 4x, i4)
       ! creating spring element :
       n6 = nsmtpr + numasu + mlum + 1
       if (mlum .eq. numasu) go to 17608
       if (spring .eq. 0.0d0) go to 17608
       call ibrinc
       it = it + 1
       kbus(ibr) = ntotst + numasu + mlum
       mbus(ibr) = ntotst + mlum + 1
       length(ibr) = 1
       nr(ibr) = - it
       tr(it) = 0.0d0
       !  the following factor of 1.0d+3 is because of milli henry
       d1 = d10 * spring / 0.73756d0
       if (xopt .eq. 0.0d0) tx(it) = 1.0d+3 / d1
       if (xopt .ne. 0.0d0) tx(it) = twopi * xopt / d1
       c(it) = 0.0
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17606) mbus(ibr), kbus(ibr), ibr, it
17606  format (' ********* Spring branch', 30x, i4, 4x, i4, 4x, i4, 4x, i4)
       umoutp(n6) = 1.0d0 / d1
       go to 17610
17608  umoutp(n6) = 0.0d0
       ! creating synchronous damping and synchronous speed source :
17610  if (dsynum .eq. 0.0d0) go to 17620
       ntot = ntot + 1
       nrsyn = nrsyn + 1
       kode(ntot) = 0
       bus(ntot) = trash
       if (numum .gt. 5) go to 17614
       if (n5 .ne. 1) go to 17612
       bus(ntot) = texta(numum + 65)
17612  if (n5 .ne. 2) go to 17614
       bus(ntot) = texta(numum + 70)
17614  kconst = kconst + 1
       iform(kconst) = 14
       node(kconst) = ntot
       kode(ntot) = - ntot
       sfreq(kconst) = 0.00001d0
       tstart(kconst) = -1.0d0
       tstop(kconst) = fltinf
       crest(kconst) = rotmom(n1) / nppair(n1)
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17615) node(kconst), kconst, sfreq(kconst)
17615  format (' ********* Synchr damping voltage source', 14x, i4, 44x, i4, e14.5)
       call ibrinc
       it = it + 1
       kbus(ibr) = ntotst + mlum
       mbus(ibr) = ntot
       length(ibr) = 1
       nr(ibr) = - it
       tr(it) = 0.73756d0 / dsynum
       tx(it) = 0.0d0
       c(it) = 0.0d0
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17616) mlum, mbus(ibr), kbus(ibr), ibr, it
17616  format (' ********* Synchr. damping branch for mass nr.', i3, 5x, i4, 4x, i4, 4x, i4, 4x, i4)
       ! creating mutual damping :
17620  n6 = nsmtpr + 2 * numasu + mlum + 1
       if (mlum .eq. numasu) go to 17628
       if (dmutum .eq. 0.0d0) go to 17628
       call ibrinc
       it = it + 1
       kbus(ibr) = ntotst + numasu + mlum
       mbus(ibr) = ntotst + mlum + 1
       length(ibr) = 1
       nr(ibr) = - it
       tr(it) = 0.73756d0 / dmutum
       tx(it) = 0.0d0
       c(it) = 0.0d0
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17626) mbus(ibr), kbus(ibr), ibr, it
17626  format (' ********* Mutual damping branch', 22x, i4, 4x, i4, 4x, i4, 4x, i4)
       if (spring .eq. 0.0d0) go to 17628
       d1 = 0.73756d0 / (d10 * spring)
       umoutp(n6) = d1 / tr(it)
       go to 17630
17628  umoutp(n6) = 0.0d0
       ! creating absolute damping :
17630  if (dabsum .eq. 0.0d0) go to 17640
       call ibrinc
       it = it + 1
       kbus(ibr) = ntotst + mlum
       mbus(ibr) = 1
       length(ibr) = 1
       nr(ibr) = - it
       tr(it) = 0.73756d0 / dabsum
       tx(it) = 0.0d0
       c(it) = 0.0d0
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17638) mlum, mbus(ibr), kbus(ibr), ibr, it
17638  format (' ********* Abslt damping branch for mass nr.', i3, 7x, i4, 4x, i4, 4x, i4, 4x, i4)
       ! storing applied torque distribution factors distrf in
       ! umoutp and the corresponding mass node nrs in kumout
17640  if (distrf .eq. 0.0d0) cycle
       ntorq = ntorq + 1
       kumout(ntorq) = ntotst + mlum
       if (ntorq .ne. 1) go to 17650
       distr1 = distrf
       ! distr1 is the first mass encountered with distrf .ne. 0.0
       umoutp(nsmtpr + 1) = 1.0d+3
       cycle
17650  umoutp(nsmtpr + ntorq) = 1.0d+3 * distrf / distr1
    end do
    !  output names of nodes next to measurement switches :
    n6 = numasu - 1
    if (numasu .eq. 1) go to 17672
    do n5 = 1, n6
       n7 = ntotst + numasu + n5
       n8 = ntotst + n5 + 1
       bus(n7) = bus(n8)
    end do
17672 n7 = ntotst + 2 * numasu
    bus(n7) = trash
    if (numum .gt. 5) go to 17700
    n3 = 41 + (numum - 1) * 4
    bus(n7) = texta(n3)
    !  *************************** read class 5 sm type-59 data
17700 call cimage
    read (unit = abuff, fmt = 17701) texxt3
17701 format (a6)
    if (text3 .eq. blank) go to 17770
    if (text3 .eq. tesm4) go to 17750
    if (text3 .eq. tesm5) go to 17750
    if (text3 .eq. tesm6) go to 17750
    n15 = 68
    if (numasu .le. 68) n15 = numasu
    read (unit = abuff, fmt = 17702) nparum, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, (jumout(i), i = 1, n15)
17702 format (80i1)
    if (jtmtac(n1) .ne. -999999) go to 17703
    do i = 1, n15
       jumout(i) = 0
    end do
17703 if (noutpr .eq. 0) write (unit = kunit6, fmt = 17704) numum
17704 format ('+um -', i3, '   sm-59 class 5, output card')
    n16 = 0
    n17 = 1
17706 if (numasu .le. n15) go to 17710
17708 call cimage
    n16 = n16 + 1
    n14 = 68 * n17 + (n16 - 1) * 80 + 1
    n15 = 68 * n17 + n16 * 80
    if (numasu .le. n15) n15 = numasu
    read (unit = abuff, fmt = 17702) (jumout(i), i = n14, n15)
    if (jtmtac(n1) .ne. -999999) go to 17709
    do i = n14, n15
       jumout(i) = 0
    end do
17709 if (noutpr .eq. 0) write (unit = kunit6, fmt = 17704) numum
    go to 17706
17710 if (n3 .eq. 0) go to 17716
    do n15 = 1, 3
       jclout(ncltot + n15) = 2
    end do
17716 if (n2 .ne. 0) kssout = 1
    if (n4 .ne. 0) jclout(ncltot + 4) = 1
    if (n5 .ne. 0) jclout(ncltot + 5) = 1
    if (n6 .ne. 0) jclout(ncltot + 6) = 1
    if (n7 .ne. 0) jclout(ncltot + 7) = 1
    if (n9 .ne. 0) jtqout(n1) = 1
    if (n10 .eq. 1) jthout(n1) = 3
    if (nmexc .eq. 0) jthout(n1) = 0
    if (n10 .eq. 3) jthout(n1) = 1
    if (n3 .ne. 0) go to 17726
    if (n11 .eq. 0) go to 17726
    do n15 = 1, 3
       jclout(ncltot + n15) = 1
    end do
17726 if (n12 .eq. 0) go to 17728
    if (n12 .eq. 2) go to 17727
    jtqout(n1) = 2
    jomout(n1) = 2
    go to 17728
17727 jtqout(n1) = 3
    jomout(n1) = 3
    !  interfacing if mass speed output is requested :
17728 do n15 = 1, numasu
       if (jumout(n15) .eq. 0) cycle
       if (jumout(n15) .eq. 2) cycle
       nv = nv + 1
       if (nv .le. lsiz12) go to 17732
       write (unit = lunit(6), fmt = 17730) numum
17730  format (/, ' Error stop.   Output nv .gt. lsiz12 for speeds of um -', i3)
       go to 9600
17732  ibrnch(nv) = ntotst + n15
       jbrnch(nv) = 1
    end do
    ! set measurement switches and shaft torque outputting :
    ! (kswsta+1) is first switch to be set from mass 1 to mass 2
    if (jtmtac(n1) .eq. -999999) go to 17800
    kswsta = kswtch
    n16 = numasu - 1
    if (numasu .eq. 1) go to 17742
    do n15 = 1, n16
       kswtch = kswtch + 1
       ndx1 = kswtch + lswtch
       kmswit(kswtch) = ntotst + n15 + numasu
       kmswit(ndx1) = ntotst + n15
       kswtyp(kswtch) = 0
       tclose(kswtch) = -1.0d0
       topen(kswtch) = fltinf
       kpos(kswtch) = 11
       if (jumout(n15) .eq. 2) kpos(kswtch) = - 11
       if (jumout(n15) .eq. 3) kpos(kswtch) = - 11
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17738) kmswit(kswtch), kmswit(ndx1), kswtch, lswtch
17738  format (' ********* Torque sensor switch', 23x, i4, 4x, i4, 20x, i4, 4x, i4)
    end do
    ! create small series resistance for applied torque to um mass
17742 call ibrinc
    it = it + 1
    kbus(ibr) = nmgen
    mbus(ibr) = ntotst + 2 * numasu
    length(ibr) = 1
    nr(ibr) = -it
    tr(it) = 1.0d0
    tx(it) = 0.0d0
    c(it) = 0.0d0
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17748) mbus(ibr), kbus(ibr), ibr, it
17748 format (' ********* Small series resist for apll gen torque', 4x, i4, 4x, i4, 4x, i4, 4x, i4)
    go to 17800
17750 n15 = 1
    read (unit = abuff, fmt = 17752) nparum, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12
17752 format (10x, 12i5)
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17704) numum
    if (text3 .ne. tesm4) go to 17756
    if (jtmtac(n1) .eq. -999999) go to 17710
    do n15 = 1,numasu
       jumout(n15) = 3
    end do
    go to 17710
17756 if (text3 .ne. tesm5) go to 17760
    do n15 = 1,numasu
       jumout(n15) = 0
    end do
    go to 17710
17760 n15 = 0
    n16 = 0
    n17 = 0
    go to 17708
    ! in case of new sm-59 output request usage :
17770 if (noutpr .eq. 0) write (unit = kunit6, fmt = 17772) numum
17772 format ('+um -', i3, '   sm-59 class 4, termination card')
17774 call cimage
    read (unit = abuff, fmt = 17701) text3
    if (text3 .eq. blank) go to 17799
    read (unit = abuff, fmt = 17776) ngroup, nall,(ndum(i), i = 1, 12)
17776 format (2x, 2i1, 4x, 12i6)
    if (noutpr .eq. 0) write (kunit6,17778) numum,ngroup
17778 format ('+um -', i3, '   sm-59 class 5, output group', i2)
    if (ngroup .gt. 0 .and. ngroup .le. 4) go to 97778
    write (unit = lunit(6), fmt = 97777) ngroup
97777 format (/, ' Error stop. The last-read card is an sm-59 output request card for output group', i2, '. This is an illegal output group as you', /, ' could find out by consulting the EMTP rule book regarding the sm-59 output request rules.')
    go to 9600
    !97778 go to (17782, 17779, 17793, 17798), ngroup
97778 select case (ngroup)
    case (1)
       go to 17782

    case (2)
       go to 17779

    case (3)
       go to 17793

    case (4)
       go to 17798

    end select
    ! treatment of group 2 (angles) output request : ignored
17779 if (noutpr .eq. 0) write (unit = lunit(6), fmt = 17780)
17780 format (/, ' Warning : this output request for mass angles is ignored by the um. They are to be obtained from the TACS output', /, ' after request of transfer into TACS through the class-6 sm-59 data input cards (consult EMTP rule book)', /)
    go to 17774
    ! treatment of group 1 (electrical variables) output request :
17782 if (nall .eq. 0) go to 17784
    kssout = 1
    if (nmexc .ne. 0) jthout(n1) = 3
    if (nmexc .eq. 0) jthout(n1) = 1
    jtqout(n1) = 3
    jomout(n1) = 3
    do i = 1, 7
       jclout(ncltot + i) = 1
    end do
    go to 17774
17784 do i = 1, 12
       if (ndum(i) .eq. 0) go to 17786
       do j = 4, 10
          if (ndum(i) .ne. j) cycle
          n2 = j
          if (j .gt. 7) n2 = j - 7
          jclout(ncltot + n2) = 1
          go to 17786
       end do
       if (ndum(i) .eq. 15) jthout(n1) = 3
       if (ndum(i) .ne. 14) go to 17786
       jtqout(n1) = 1
       jomout(n1) = 1
       if (jthout(n1) .eq. 0) jthout(n1) = 1
17786  continue
    end do
    n3 = 0
    do i = 1, 12
       if (ndum(i) .eq. 0) go to 17792
       do j = 1, 3
          if (ndum(i) .ne. j) cycle
          if (jclout(ncltot + j) .eq. 0) go to 17790
          n3 = 1
17788     format (/, ' Warning : simultaneous request of Park domain currents and real currents of the power windings of this um nr.', i3, ' is not', /, ' honored. The output will only display the real currents.', /)
          go to 17792
17790     jclout(ncltot + j) = 2
          go to 17792
       end do
       if (ndum(i) .lt. 12) go to 17792
       if (ndum(i) .gt. 13) go to 17792
       jtqout(n1) = 3
       jomout(n1) = 3
17792  continue
    end do
    if ((noutpr .eq. 0) .and. (n3 .ne. 0)) write (unit = lunit(6), fmt = 17788) num
    go to 17774
    ! treatment of group 3 (mass speeds) output request :
17793 n2 = 2
    n3 = 1
17794 if (jtmtac(n1) .ne. -999999) go to 17796
    do i = 1, numasu
       jumout(i) = 0
    end do
    go to 17774
17796 if (nall .eq. 0) go to 97797
    do i = 1, numasu
       if (jumout(i) .eq. n2) jumout(i) = 3
       if (jumout(i) .eq. 0) jumout(i) = n3
    end do
    go to 17774
97797 do i = 1, 12
       n15 = ndum(i)
       if (n15 .eq. 0) cycle
       if (jumout(n15) .eq. n2) jumout(n15) = 3
       if (jumout(n15) .eq. 0) jumout(n15) = n3
    end do
    go to 17774
    ! treatment of group 4 (torques) output request :
17798 n2 = 1
    n3 = 2
    go to 17794
    ! blank card ending sm-59 output request :
17799 if (noutpr .eq. 0) write (unit = kunit6, fmt = 97799) numum
97799 format ('+um -', i3, '   sm-59 class 5, termination card')
    go to 17728
    ! *************************** read class 6 sm type-59 data
17800 nangre = 0
    nexcsw = 0
    ntacb = ntotac
    ! nangre is first entry of umoutp that indicates angle transfer
    n19 = 0
    ! n19 = 1 is flag for tacs control of total applied torque.
    niunrs = iuty(kiuty+1)
17810 call cimage
    n17 = 0
    ! n17 is in reading class 6 reserved as flag for modified
    ! format of code numbers if n20 .eq. 0
    read (unit = abuff, fmt = 17812) n20, bus1, n2
17812 format (i2,a6,i6)
    if (n20 .eq. 0) n17 = 3 * limasu
    if (n20 .eq. 0) n20 = n2
    if (n20 .eq. 0) go to 17816
    if (noutpr .eq. 0) write (unit = kunit6, fmt = 17814) numum
17814 format ('+um -', i3, '   sm-59 class 6, TACS request')
17816 if (bus1 .ne. tesm7) go to 17820
    read (unit = abuff, fmt = 17817) bus1, bus2
17817 format (2x,2a6)
    if (bus2 .ne. tesm8) go to 17818
    write (unit = lunit(6), fmt = 17053)
    go to 9600
17818 if (noutpr .eq. 0) write (unit = kunit6, fmt = 17819) numum
17819 format ('+um -', i3, '   reading completed')
    if (n19 .eq. 0) go to 17890
    go to 17900
    !  request for tacs control of field voltage :
17820 n4 = 71 + n17
    if (n20 .ne. n4) go to 17830
    do n5 = 1, ktab
       ndx1 = ilntab( klntab + n5 )
       if (bus1 .eq. texvec(ndx1)) go to 17827
    end do
    write (unit = lunit(6), fmt = 17826) bus1
17826 format (/, ' Error stop.   Just-read card requesting control by TACS, bears unrecognized', /, ' TACS name :', a6)
    go to 9600
17827 tstop(kconex) = 0.0d0
    !  create type-60 source :
    kconst = kconst + 1
    iform(kconst) = 60
    node (kconst) = nexc
    kode(nexc) = -nexc
    bus(nexc) = bus1
    sfreq(kconst) = n5
    tstart(kconst) = 0.0d0
    tstop(kconst) = fltinf
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17828) node(kconst), kconst, sfreq(kconst)
17828 format (' ********* TACS originated field volt type-60 source', 2x, i4, 44x, i4, e14.5)
    write (unit = lunit(6), fmt = 17829)
17829 format (/, ' Note: the EMTP connectivity listing will show one of the nodes bearing the TACS name as used in the last read card. No reason', /, ' to get alarmed, since this node is one of the auxiliary nodes created internally by the u.m. code.', /)
    go to 17810
    ! request for transfer to tacs of exciter voltage :
17830 n4 = 73 + n17
    if ((n20 .ne. n4) .and. (n20 .ne. (n4 + 1)) .and. (n20 .gt. (3 * limasu))) go to 17870
    !     load address of bus1 into tacs array 'ud1' ***************
    ndy5 = kud1
    do ijk = niunrs, niu
       ndx1 = ilntab(kaliu + ijk)
       n1iu = iuty(kiuty + ijk)
       if (n1iu .ne. 92) go to 15481
       if (bus1 .ne. texvec(ndx1)) go to 15481
       go to 15482
15481  ndy5 = ndy5 + 5
    end do
    kill = 108
    lstat(19) = 15481
    lstat(15) = lbstac
    lstat(16) = ntotac
    lstat(14) = n20
    lstat(17) = -1
    lstat(13) = 0
    bus6 = bus1
    jr = 1
    return
15482 ntotac = ntotac + 1
    ud1(ndy5 + 2) = ntotac
    if (ntotac .le. lbstac) go to 17834
    write (unit = lunit(6), fmt = 17831)
17831 format (/, ' Error stop.   Overflow lbstac during execution of request in umdata to')
    write (unit = lunit(6), fmt = 17832) numum
17832 format (/, ' pass um variables to TACS. The machine in question, um -', i3, ',is provided')
    write (unit = lunit(6), fmt = 17833)
17833 format (/, ' with sm type-59 data input.')
    go to 9600
17834 ismtac(ntotac) = n20
    if (n20 .ne. n4) go to 17840
    n3 = 3 * numasu + nsmtac
    umoutp(n3 + 1) = -5.0d0
    umoutp(n3 + 2) = nexc
    umoutp(n3 + 3) = ntotac
    nsmtac = nsmtac + 3
    go to 17810
    ! request for transfer to tacs of exciter current :
17840 n4 = 74 + n17
    if (n20 .ne. n4) go to 17850
    ! create additional exciter node to insert switch
    ntot = ntot + 1
    kode(ntot) = 0
    nexcsw = ntot
    bus(nexcsw) = trash
    if (numum .gt. 5) go to 17842
    n3 = 41 + (numum - 1)*4
    bus(nexcsw) = texta(n3+3)
    ! create switch in exciter circuit
17842 kswtch = kswtch + 1
    kswexc = kswtch
    ndx1 = kswtch + lswtch
    kmswit(kswtch) = nexcsw
    kmswit(ndx1) = nexc
    kswtyp(kswtch) = 0
    tclose(kswtch) = -1.0d0
    topen(kswtch) = fltinf
    kpos(kswtch) = 11
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17843) kmswit(kswtch), kmswit(ndx1), kswtch, lswtch
17843 format (' ********* Field current sensor switch', 16x, i4, 4x, i4, 20x, i4, 4x, i4)
    ! connect exciter series resistance to new switch node
    mbus(ibrexc) = nexcsw
    n3 = 3 * numasu + nsmtac
    umoutp(n3 + 1) = -4.0d0
    umoutp(n3 + 2) = kswexc
    umoutp(n3 + 3) = ntotac
    nsmtac = nsmtac + 3
    go to 17810
    ! transfer to solvum for angle history calculations if angle
    ! transfer to tacs is requested :
17850 if (jtmtac(n1) .eq. -999999) go to 17810
    if (n20 .gt. limasu) go to 17858
    ! note : in case that just one mass angle is requested to be
    ! passed to tacs, then the angle history of all masses
    ! needs to be transfered to solvum.
    if (nangre .ne. 0) go to 17854
    nangre = nsmtac + 1
    do n5 = 1, numasu
       n3 = 3 * numasu + nsmtac
       n4 = nsmtpr + n5
       umoutp(n3 + 1) = -299
       umoutp(n3 + 2) = ntotst + n5
       umoutp(n3 + 3) = 0.0d0
       umoutp(n3 + 4) = umoutp(n4)
       umoutp(n3 + 5) = umoutp(numasu + n4)
       nsmtac = nsmtac + 5
       if (n5 .ne. 1) cycle
       umoutp(n3 + 1) = -300.0d0 - kswsta
       umoutp(n3 + 4) = 5.0d0 * numasu
       umoutp(n3 + 5) = numasu
    end do
    ! request for transfer to tacs of mass angles
17854 continue
    n3 = 3 * numasu + nangre
    if (n20 .eq. 1) go to 17857
    n6 = n3 + (n20 - 1) * 5
    umoutp(n6) = -3.0
17857 n6 = n3 + 2 + (n20 - 1) * 5
    umoutp(n6) = ntotac
    go to 17810
    ! request for transfer to tacs of mass speeds :
17858 if (jtmtac(n1) .eq. -999999) go to 17810
    n10 = 2 * limasu
    if (n20 .gt. n10) go to 17860
    n3 = 3 * numasu + nsmtac
    umoutp(n3 + 1) = -2.0
    umoutp(n3 + 2) = ntotst + n20 - limasu
    umoutp(n3 + 3) = ntotac
    nsmtac = nsmtac + 3
    go to 17810
    ! request for transfer to tacs of shaft torques :
17860 if (jtmtac(n1) .eq. -999999) go to 17810
    n10 = 3 * limasu
    if (n20 .gt. n10) go to 17870
    n3 = 3 * limasu + nsmtac
    umoutp(n3 + 1) = -1.0
    umoutp(n3 + 2) = kswsta + n20 - 2 * limasu
    umoutp(n3 + 3) = ntotac
    nsmtac = nsmtac + 3
    go to 17810
    ! set type-14 and 18 for electromech. exciter torque from tacs:
17870 n4 = 80 + n17
    if (n20 .ne. n4) go to 17880
    write (unit = lunit(6), fmt = 17872) numum
17872 format (/, ' Note : this TACS interface request regarding the exciter torque for um number', i4, ',  is no longer in effect. The influence of', /, ' the exciter torque will be automatically included in exactly the same way as with the sm type-59 code.', /)
    go to 17810
    ! request for tacs control of total applied torque :
17880 if (jtmtac(n1) .eq. -999999) go to 17810
    n4 = 72 + n17
    if (n20 .ne. n4) go to 17810
    n19 = 1
    do n10 = 1, ktab
       ndx1 = ilntab( klntab + n10 )
       if (bus1 .eq. texvec(ndx1)) go to 17890
    end do
    write (unit = lunit(6), fmt = 17826) bus1
    go to 9600
    ! set type-14 sources for applied torques :
17890 if (jtmtac(n1) .eq. -999999) go to 17899
    do n5 = 1, ntorq
       if (n19 .ne. 1) go to 17894
       kconst = kconst + 1
       sfreq(kconst) = n10
       iform(kconst) = 17
       node(kconst) = - 1
       tstart(kconst) = - 1.0
       tstop(kconst) = fltinf
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17892) node(kconst), kconst, sfreq(kconst)
17892  format (' ********* type-17 source for next TACS control', 7x, i4, 44x, i4, e14.5)
17894  kconst = kconst + 1
       if (n5 .eq. 1) nodmum(n1) = kconst
       iform(kconst) = 14
       n6 = kumout(n5)
       if (n6 .eq. nmgen) n6 = ntotst + 2*numasu
       node(kconst) = -n6
       kode(n6) = 0
       n4 = nsmtpr + n5
       crest(kconst) = umoutp(n4)
       umoutp(n4) = 0.0
       tstart(kconst) = -7777.0d0
       tstop(kconst) = fltinf
       sfreq(kconst) = 0.00001d0
       if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17896) node(kconst), kconst, sfreq(kconst)
17896  format (' ********* Applied torque source', 22x, i4, 44x, i4, e14.5)
    end do
17899 if (n19 .eq. 0) go to 17900
    go to 17810
    ! ********************* finish statements of sm type-59 data
17900 ncltot = ncltot + 7
    if (ncltot .le. nclfix) go to 17905
    write (unit = lunit(6), fmt = 17904) ncltot
17904 format (/, ' Overflow of u.m. coil table, ncltot = ', i5, ' increase nclfix on um dimension card.')
    go to 9600
17905 do n5 = 1, numasu
       kumout(n5) = 0
       jumout(n5) = 0
    end do
    mjm = ncltot - 6
    do nk = mjm, ncltot
       jcltac(nk) = 0
    end do
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17930) ntotst, numasu, nrsyn, nmgen, nmexc
17930 format (/, ' ********* Created nodes for mechanical system : first node nr = ntotst + 1, last node nr = ntotst + 2 * numasu + nrsyn .', /, 30x, 'ntotst', 2x, 'numasu', 2x, ' nrsyn', 2x, ' nmgen', 2x, ' nmexc', /, 32x, i4, 4x, i4, 4x, i4, 4x, i4, 4x, i4)
    if (iprsup .ge. 1) write (unit = lunit(6), fmt = 17932) nexc, nexcsw
17932 format (/, ' ********* Created nodes for field circuit : nexc = ', i4, 10x, 'nexcsw = ', i4)
    !  shifting umoutp entries with 3*numasu to behind nsmtpr :
    n6 = nsmtac - nsmtpr
    n7 = nsmtpr + n6
    do n5 = 1, n6
       n3 = nsmtpr + n5
       n4 = n3 + 3*numasu
       umoutp(n3) = umoutp(n4)
       if (n4 .gt. n7) umoutp(n4) = 0.0d0
    end do
    if (nparum .ne. 0) go to 17951
    if (iprsup .lt. 3) go to 17958
17951 write (unit = lunit(6), fmt = 17952) n1, reamdu(n1), reamds(n1), reamqu(n1), reamqs(n1), flxds(n1), flxqs(n1)
17952 format (/, ' Parameters of um nr.', i4, ':', 10x, 'lmud', 1x, 'lmsd', 10x, 'lmuq', 10x, 'lmsq', 9x, 'flxsd', 9x, 'flxsq', /, 26x, 6e14.5)
    write (unit = lunit(6), fmt = 17953)
17953 format (/, ' coil     resistance    leakage inductance')
    n6 = ncltot - 6
    do n5 = n6, ncltot
       if (gpar(n5) .eq. 0.0d0) write (unit = lunit(6), fmt = 17954) n5
17954  format (1x, i4, 10x, ' dummy coil')
       if (gpar(n5) .eq. 0.0d0) cycle
       write (unit = lunit(6), fmt = 17955) n5, gpar(n5), reacl(n5)
17955  format (1x, i4, 2x, 2e14.5)
       if (n5 .eq. ncltot) write (unit = lunit(6), fmt = 17956)
17956  format (/, ' ')
    end do
    ! set 0.5 * rf for internal field resistance :
17958 gpar(ncltot - 3) = 0.5d0 * gpar(ncltot - 3)
    n5 = nsmtac + 3 * numasu
    if (n5 .le. iotfix) go to 17970
    write (unit = lunit(6), fmt = 17960)
17960 format (/, ' Overflow of um output table, increase in card for absolute um dimensions', /, ' value of iotfix.', /, ' Remark : if sm type-59 data input is included, then iotfix is also', /, ' related to outputs of mech. Systems,  in all sm type-59 data machines.')
    go to 9600
17970 jf = 1
    return
    ! end reading sm type-59 data input $$$$$$$$$$$$$$$$$$$$$$$$$$
9600 call stoptp
    return
  end subroutine umdatb

end module umdat

!
! subroutine over5.
!

subroutine over5
  use blkcom
  use labcom
  use tacsar
  use tracom
  use strcom
  use movcop
  use a8swmod
  use freedom
  implicit none
  character(8) :: text1, text2, text5, text6, text7
  character(8) :: text8, text13, text14, text15, text16
  character(8) :: text17
  character(80) :: buffer
  integer(4) :: i, idube1, idube2, idube3, ios, ip, ip2, ip3, iprint
  integer(4) :: j, jdube1, jk
  integer(4) :: kpass, kswt2
  integer(4) :: ll10, lsw2
  integer(4) :: mk, mpower
  integer(4) :: n1, n2, n3, n4, n5, n6, n7, n24, n98, n99, ndx1, ndx2, ndx3
  integer(4) :: ndx4, ndxi, ndxj, ndxk
  real(8) :: a, abc, adube1, adube2, adube3, atemp
  real(8) :: d1, d2
  real(8) :: gus3, gus4
  real(8) :: ssigma, ststat
  real(8) :: targ, timchk
  !
  !  dimension akey(1), tstat(1)
  !
  !  equivalence (akey(1), adelay(1))
  !  equivalence (tstat(1), crit(1))
  !  equivalence (moncar(1), knt)
  !  equivalence (moncar(4), isw)
  !  equivalence (moncar(5), idist)
  !  equivalence (moncar(6), itest)
  !  equivalence (moncar(9), kloaep)
  !
  integer(4), pointer :: idist => moncar(5)
  integer(4), pointer :: isw => moncar(4)
  integer(4), pointer :: itest => moncar(6)
  integer(4), pointer :: kloaep => moncar(9)
  integer(4), pointer :: knt => moncar(1)
  real(8), pointer :: akey(:)
  real(8), pointer :: tstat(:)
  !
  data text1  / 'name  ' /
  data text2  / 'swt001' /
  data text5  / 'statis' /
  data text6  / 'tics  ' /
  data text7  / 'system' /
  data text8  / 'atic'   /
  data text13 / 'measur' /
  data text14 / 'ing   ' /
  data text15 / 'closed' /
  data text16 / 'same  ' /
  data text17 / 'target' /
  !
  akey => adelay
  tstat => crit
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  Begin module "over5".')
  lstat(29) = inonl
  lstat(30) = i_char
  lstat(33) = ifdep
  lstat(34) = jst
  lstat(40) = ifsem
  lstat(41) = iadd
  call move0 (crest(1 :), lexct)
  call move0 (time1(1 :), lexct)
  call move0 (sfreq(1 :), lexct)
  call move0 (xk(1 :), lpast)
  call move0 (xm(1 :), lpast)
  omega = 0.0d0
  iwtent(ifdep2) = jst1
  if (iprsup .le. 0) go to 6693
  if (ifdep .eq. 0) go to 6693
  if (iprsup .ge. 9) write (unit = lunit(6), fmt = 6656) (weight(i), i = 1, jst)
6656 format (' Weight ---- array of weighting function storage', /, (1x, 10e13.4))
  write (unit = lunit(6), fmt = 6648)
6648 format (/, 2x, 'row', 3x, 'iskip', 2x, 'iwtent', 12x, 'eta', 11x, 'zinf', 10x, 'tailp', 10x, 'tailq', 10x, 'tailt', 4x, 'sum weights')
  do i = 1, ifdep
     n1 = 2 * i - 1
     n3 = iwtent(n1)
     n4 = iwtent(n1 + 2) - 1
     d1 = 0.0d0
     ip2 = 2 * lfdep + i
     ip3 = 2 * ip2 - i
     do j = n3, n4
        d1 = d1 + weight(j)
     end do
     write (unit = lunit(6), fmt = 6671) i, iskip(i), iwtent(i), eta(i), zinf(i), con1(i), con1(ip2), con1(ip3), d1
  end do
6671 format (1x, i4, 2i8, 6e15.6)
  do i = 1, ifdep
     n1 = ifdep + i
     n2 = 2 * lfdep + n1
     n3 = 4 * lfdep + n1
     write (unit = lunit(6), fmt = 6678) n1, iwtent(n1), con1(n1), con1(n2), con1(n3)
  end do
6678 format (1x, i4, 8x, i8, 10x, 3e15.6)
6693 continue
  ! reading in switch data
  kswtch = 0
  kconst = 0
  n4 = 0
  kpass = 0
  idube1 = 0
  mk = 0
  ll10 = 10
  lsw2 = lswtch + lswtch
  call move0 (indtv(1 :), ll10)
  call move0 (kdepsw(1 :), lsw2)
  call move0 (adelay(1 :), lsw2)
209 if (kill .gt. 0) go to 9200
  if (iprsup .ge. 5) write (unit = lunit(6), fmt = 76) kswtch, kloaep, (kdepsw(ip), ip = 1, kswtch)
76 format (/, ' Read next switch card.  kswtch  kloaep', /, 23x, 2i8, /, (1x, 20i6))
  ! read input card using cimage.
3483 call cimage
  if (kolbeg .gt. 0) go to 3503
  read (unit = abuff, fmt = 35, iostat = ios) it2, bus1, bus2, gus3, gus4, ck1, a, bus3, bus4, bus5, bus6, jdube1, j
35 format (i2, 2a6, 4e10.0, a6, a4, 2a6, 2x, 2i1)
  go to 3506
3503 nfrfld = 1
  call ffree (voltbc)
  it2 = int (voltbc(1))
  nright = -1
  nfrfld = 2
  call ffree (d1)
  nright = 0
  bus1 = texta6(1)
  bus2 = texta6(2)
  nfrfld = 4
  call ffree (voltbc)
  gus3 = voltbc(1)
  gus4 = voltbc(2)
  ck1 = voltbc(3)
  a = voltbc(4)
  nright = -2
  nfrfld = 3
  call ffree (d1)
  nright = 0
  bus3 = texta6(1)
  bus4 = texta6(2)
  bus5 = texta6(3)
  bus6 = texta6(4)
  nfrfld = 2
  call ffree (voltbc)
  jdube1 = int (voltbc(1))
  j = int (voltbc(2))
3506 if (bus1 .ne. blank) go to 3510
  if (bus2 .eq. blank) go to 213
3510 kswtch = kswtch + 1
  kswt2 = lswtch + kswtch
  if (kswtch .le. lswtch) go to 3517
  lstat(19) = 3510
  iprint = 6
  go to 9000
3517 crit(lswtch + 1 - kswtch) = 0.0
  if (bus1 .ne. text1) go to 8212
  if (noutpr .eq. 0) write (unit = lunit(6), fmt = 8233) bus2, kswtch
8233 format (' moniker', '"', a6, '"', ' is for next switch ', i6, ' .')
  call namea6 (bus2, n24)
  namesw(kswtch) = n24
  go to 3483
8212 if (bus1 .ne. bus2) go to 3519
  kill = 224
  lstat(19) = 3517
  go to 9200
3519 call nmincr (text2, kswtch)
  call namea6 (text2, n24)
  namesw(kswtch) = n24
  adelay(kswtch) = 0.
  atemp = a
  n1 = 0
  n2 = 0
  do i = 1, ntot
     if (bus1 .eq. bus(i)) n1 = i
     if (bus2 .eq. bus(i)) n2 = i
  end do
  iprint = 1
  if (n1 .ne. 0) go to 211
  ntot = ntot + 1
  lstat(19) = 210
  if (ntot .gt. lbus) go to 9000
  n1 = ntot
  bus(n1) = bus1
211 if (n2 .ne. 0) go to 212
  ntot = ntot + 1
  lstat(19) = 211
  if (ntot .gt. lbus) go to 9000
  n2 = ntot
  bus(n2) = bus2
212 isourc(kswtch) = 0
  if (j .le. 3) go to 4228
  j = 3
  if (npower .lt. maxpe) go to 54201
  iprint = 18
  lstat(19) = 54201
  go to 9000
54201 npower = npower + 1
  mpower = npower + maxpe
  koutvp(npower) = -(nv + 1)
  koutvp(mpower) = kswtch
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 4227) npower, maxpe, koutvp(npower), koutvp(mpower)
4227 format (/, ' Power output request', 4i10)
4228 continue
  if (j .lt. 2) go to 224
  nv = nv + 1
  ibrnch(nv) = n1
  jbrnch(nv) = n2
  if (nv .le. lsiz12) go to 224
  iprint = 11
  lstat(19) = 224
  go to 9000
224 if (j .eq. 2) j = 0
  if (it2 .ge. 11 .and. it2 .le. 13) go to 250
  if (it2 .eq. 0) go to 218
  if (it2 .ne. 76) go to 4235
  it2 = 9976
  go to 218
4235 kill = 7
  lstat(19) = 224
  lstat(16) = it2
  go to 9200
  ! *****   interpreting   ******************************************
  ! *****     diode  or  tacs-controlled valve  (type-11)  **********
  ! *****     tacs-controlled spark gap         (type-12)  **********
  ! *****     tacs-controlled ordinary switch   (type-13)  **********
250 if (bus4 .ne. text16) go to 262
  if (it2 .ne. 11) go to 262
  if (idube1 .ne. 0) go to 257
  kill = 146
  lstat(19) = 250
  go to 9200
262 adube1 = gus3
  adube2 = gus4
  adube3 = ck1
  idube1 = 1
257 idube2 = 0
  idube3 = 0
  if (bus5 .eq. blank) go to 264
  if (it2 .eq. 13) go to 264
  if (ktab .eq. 0) go to 259
  do i = 1, ktab
     ndx1 = ilntab(klntab + i)
     if (bus5 .eq. texvec(ndx1)) go to 261
  end do
259 kill = 147
  lstat(19) = 259
  go to 9200
261 idube2 = i
264 if (bus6 .eq. blank) go to 251
  if (ktab .eq. 0) go to 265
  do i = 1, ktab
     ndx1 = ilntab(klntab + i)
     if (bus6 .eq. texvec(ndx1)) go to 267
  end do
265 kill = 119
  lstat(19) = 265
  go to 9200
267 idube3 = i
251 if (noutpr .ne. 0) go to 25199
  if (it2 .ne. 11) go to 25100
  if (idube2 .ne. 0) go to 25110
  write (unit = kunit6, fmt = 252)
252 format ('+Diode,  no TACS grid')
  go to 25150
25110 ndx1 = ilntab(klntab + idube2)
  write (unit = kunit6, fmt = 253) texvec(ndx1)
253 format ('+Valve, TACS grid= ', "'", a6, "'")
  go to 25150
25100 if (it2 .ne. 12) go to 25120
  if (idube2 .ne. 0) go to 25105
  write (unit = kunit6, fmt = 25101)
25101 format ('+Gap,  no TACS spark')
  go to 25150
25105 ndx1 = ilntab(klntab + idube2)
  write (unit = kunit6, fmt = 25106) texvec(ndx1)
25106 format ('+Gap, TACS spark = ', "'", a6, "'")
25150 if (idube3 .eq. 0) go to 25199
  ndx1 = ilntab(klntab + idube3)
  write (unit = kunit6, fmt = 25151) texvec(ndx1)
25151 format ('+', 26x, ', TACS clamp = ', "'", a6, "'")
  go to 25199
25120 if (idube3 .ne. 0) go to 25125
  write (unit = kunit6, fmt = 25121)
25121 format ('+Switch, TACS control signal missing  ????????????')
  go to 25199
25125 ndx1 = ilntab(klntab + idube3)
  write (unit = kunit6, fmt = 25126) texvec(ndx1)
25126 format ('+Switch, TACS control signal = ', "'", a6, "'")
25199 continue
  adelay(kswtch) = a
  energy(kswtch) = 0.0
  kdepsw(kswt2) = 8888
  if (it2 .eq. 12) kdepsw(kswt2) = 8890
  if (it2 .eq. 13) kdepsw(kswt2) = 8891
  topen(kswtch) = fltinf
  tclose(kswtch) = fltinf
  if (bus3 .eq. text15) tclose(kswtch) = -1.0
  kpos(kswtch) = 11
  if (j .gt. 0) kpos(kswtch) = -11
  kswtyp(kswtch) = 0
  ndx1 = lswtch + kswtch
  ndx2 = lswtch + ndx1
  ndx3 = lswtch + ndx2
  ardube(ndx1) = adube1
  ardube(ndx2) = adube2
  ardube(ndx3) = adube3
  ardube(kswtch) = -fltinf
  iardub(kswtch) = idube2
  iardub(ndx1) = idube3
  iardub(ndx2) = jdube1
  if (a .ne. 7777.) go to 220
  call cimage
  read (unit = abuff, fmt = 3355) a8sw(mk + 1), a8sw(mk + 2), a8sw(mk + 3), a8sw(mk + 8)
3355 format (4e16.6)
  a8sw(mk + 7) = 0.0
  mk = mk + 8
  adelay(kswtch) = 7 - mk
  if (mk .le. 400) go to 220
  kill = 1
  write (unit = lunit(6), fmt = 3434)
3434 format (' ***** Overflow the temporary storage 300 cells for vector a6sw near statement 218 at over5 *****')
  go to 9200
218 if (bus3 .ne. text13) go to 70218
  if (bus4 .ne. text14) go to 70218
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 10218)
10218 format ('+Permanently-closed switch used for metering.')
  gus3 = -1.0
  gus4 = fltinf
  go to 220
70218 if (noutpr .eq. 0) write (unit = kunit6, fmt = 36) gus3, gus4, ck1, a
36 format ('+Switch.', 2x, 4e10.2)
  if (a .eq. 0.0) go to 220
  if (atemp .eq. 3333.) go to 220
  adelay(kswtch) = absz (gus4)
220 kmswit(kswtch) = n1
  ndx2 = kswtch + lswtch
  kmswit(ndx2) = n2
  if (it2 .ge. 11 .and. it2 .le. 13) go to 209
  icheck = 11
  if (a .eq. 0.0) go to 216
  if (atemp .eq. 3333.) go to 216
  gus4 = absz (a)
  icheck = 10
  if (gus3 .ge. 0.) go to 216
  gus3 = 0.
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 217)
217 format (16x, 'tclose changed to zero')
216 if (j .gt. 0) icheck = -icheck
  kpos(kswtch) = icheck
  kswtyp(kswtch) = it2
  tclose(kswtch) = gus3
  topen(kswtch) = gus4
  crit(kswtch) = ck1
  read (unit = abuff, fmt = 3898) buffer
3898 format (a80)
  n98 = index (buffer, '   statistics')
  n99 = index (buffer, '   st')
  if (n98 .eq. 52 .or. n99 .eq. 0) go to 3920
  if (n99 .lt. 30 .or. n99 .gt. 60) go to 3920
  write (unit = lunit(6), fmt = 3906) buffer
3906 format (" Temporary error stop in  over5.   The user's data is for a monte carlo simulation,", /, ' but  "statistics"  in columns 55-64 of the last-read data card has been misspelled.', /, ' The defective 80-column card image =', a80)
  stop
3920 if (toLower (bus3) .ne. text5) go to 3260
  if (toLower (bus4) .eq. text6) go to 3360
3260 if (toLower (bus3) .ne. text7) go to 605
  if (toLower (bus4) .ne. text8) go to 605
  sigmax = 0.0d0
  idist = 0
  if (nenerg .lt. 0)  go to 615
  kill = 141
  lstat(19) = 3260
  go to 9200
605 go to 209
3360 if (nenerg .gt. 0) go to 615
  kill = 141
  lstat(19) = 3360
  go to 9200
615 akey(kswtch) = +44444.0d0
  if (a .ne. 3333.) go to 3344
  akey(kswtch) = -44444.0d0
  ndx1 = lswtch + 1 - kswtch
  if (ck1 .le. 0.0) go to 4674
  !     patch to allow current margin with random opening (subts1):
  if (ndx1 .gt. kswtch) go to 4668
  write (unit = lunit(6), fmt = 4661) ndx1, kswtch, lswtch
4661 format (' Overflow error stop, list 6.  random opening with current margin.   ndx1, kswtch, lswtch =', 3i8)
  call stoptp
4668 crit(ndx1) = ck1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4669) i, kswtch, ck1
4669 format (5x, 'i, kswtch, ck1 =', 2i8, e15.5)
4674 continue
  topen(kswtch) = tclose(kswtch)
  tclose(kswtch) = -1.0
3344 kpass = kpass + 1
  if (bus5 .ne. text17) go to 34605
  kloaep = kswtch
  go to 225
34605 n5 = 0
  n6 = 0
  do i = 1, ntot
     if (bus5 .eq. bus(i)) n5 = i
     if (bus6 .eq. bus(i)) n6 = i
  end do
  if (n5 * n6 .eq. 1) go to 225
  n7 = kswtch - 1
  if (n7 .le. 0) go to 7245
  do i = 1, n7
     if (n5 .ne. kmswit(i)) cycle
     ndx3 = lswtch + i
     if (n6 .ne. kmswit(ndx3)) cycle
     kdepsw(kswtch) = i
     go to 225
  end do
7245 kill = 110
  lstat(19) = 245
  lstat(15) = n5
  lstat(16) = n6
  go to 9200
225 tdns(kswtch) = ck1
  tstat(kswtch) = gus3
  if (gus4 .lt. 0.0) go to  620
  ndxi = kswtch + lswtch
  topen(ndxi) = gus4
  if ((it2 .eq. 9976) .or. (idist .eq. 1)) topen(ndxi) = -gus4
  go to 209
617 kill = 94
  flstat(14) = ststat
  flstat(15) = ssigma
  go to 9200
618 kill = 95
  flstat(14) = ststat
  flstat(15) = ssigma
  go to 9200
620 kill = 89
  lstat(19) = 620
  flstat(15) = tstat(kswtch)
  flstat(16) = gus4
  go to 9200
213 if (nenerg .eq. 0) go to 8299
  do i = 1, kswtch
     if (absz (akey(i)) .ne. 44444.0d0) cycle
     j = i
     ststat = 0.0d0
     ssigma = 0.0d0
4632 ndxj = j + lswtch
     targ = tstat(j)
     jk = kdepsw(j)
     if ((kloaep .ne. 0) .and. (jk .eq. 0)) targ = tstat(kloaep)
     ststat = ststat + targ
     ssigma = sqrtz (ssigma * ssigma + topen(ndxj) * topen(ndxj))
     if (jk .eq. 0) go to 4682
     j = jk
     go to 4632
4682 if (ststat .ge. 0.0d0) go to 4688
     lstat(14) = i
     lstat(15) = 4433
     go to 620
4688 if (nenerg .lt. 0) go to 8222
     if ((idist .eq. 1) .or. (kswtyp(i) .eq. 9976)) go to 616
     timchk = ststat - sigmax * ssigma
     if (timchk .lt. 0.0d0) go to 617
     go to 614
8222 abc = 1.0d0 - itest
     if ((kloaep .eq. 0) .or. (kloaep .eq. i)) go to 8303
     ndxk = lswtch + kloaep
     ststat = ststat - 0.5d0 * abc * topen(ndxk) * (tdns(kloaep) - 1.0d0)
     abc = 1.0d0
8303 timchk = ststat - 0.5d0 * abc * topen(ndxj) * (tdns(j) - 1.0d0)
     if (timchk .lt. 0.0d0) go to 618
     go to 614
616  timchk = ststat - 1.732050808d0 * ssigma
     if (timchk .lt. 0.0d0) go to 618
614  if (timchk .lt. tenerg) tenerg = timchk
  end do
8299 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54133)
54133 format ('+Blank card terminating switch cards.')
  call move0 (ipntv(1 :), ll10)
  if (tenerg .ge. 1.e+19) go to 1642
  if (tenerg .lt. tmax) go to 1642
  if (nenerg .eq. 1) go to 1642
  kill = 207
  lstat(19) = 1642
  go to 9200
1642 numref = 0
  if (nenerg .eq. 0) go to 640
  n1 = 1
  do i = 1, kswtch
     if (absz (akey(i)) .ne. 44444.0d0) cycle
     if (kdepsw(i) .ne. 0) cycle
     numref = numref + 1
     if (numref .le. 10) go to 500
     kill = 112
     lstat(14) = i
     lstat(19) = 500
     n1 = kmswit(i)
     ndx4 = lswtch + i
     n2 = kmswit(ndx4)
     bus1 = bus(n1)
     bus2 = bus(n2)
     go to 9200
500  if (nenerg .gt. 0) go to 510
     n1 = int (n1 * tdns(i))
510  ipntv(numref) = i
     if (kloaep .ne. i) cycle
     iloaep = numref
  end do
  if (nenerg .gt. 0) go to 640
  n2 = iabs (nenerg)
  if (n1 .eq. n2) go to 640
  kill = 113
  lstat(19) = 625
  lstat(12) = n1
  j = 0
  do i = 1, kswtch
     if (absz (akey(i)) .ne. 44444.0d0) cycle
     if (kdepsw(i) .ne. 0) cycle
     j = j + 1
     ipntv(j) = int (tdns(i))
  end do
  go to 9200
640 if (nenerg .le. 0) go to 643
  if (isw .ne. 4444) go to 643
  call runtym (d1, d2)
  flstat(1) = flstat(1) + d1
  flstat(2) = flstat(2) + d2
  flstat(5) = flstat(5) - d1
  flstat(6) = flstat(6) - d2
  lastov = nchain
  nchain = 12
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module over5."')
  go to 99999
643 lstat(21) = ntot
  lstat(26) = kswtch
  lstat(38) = npower
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 8025) ibr, inonl, kswtch, nv, npower, ntot, it, ifdep, num99, i_char, knt
8025 format (/, " Misc. scalars, in  'over5'  after completing switch input.", /, 1x, '     ibr   inonl  kswtch      nv  npower ntot      it   ifdep   num99   ichar     knt', /, 1x, 11i8)
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 8026) (koutvp(i), i = 1, npower)
8026 format (/, ' koutvp, koutie', /, (1x, 10i10))
  ndx1 = maxpe + 1
  ndx2 = maxpe + npower
  if (iprsup .ge. 2) write (unit = lunit(6), fmt = 8026) (koutvp(i), i = ndx1, ndx2)
  ipunch = ipun
  iout = ioutin
  ! code for input of EMTP sources is in  "over5a" .
  call over5a
  if (ktab .eq. 0) go to 3636
  ndx1 = kxtcs + nuk
  xtcs(ndx1 + 5) = omega
  xtcs(ndx1 + 4) = omega / twopi
3636 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
  if (kill .eq. 0) go to 99999
  go to 9200
9000 lstat(16) = iprint
  kill = 1
9200 lastov = nchain
  nchain = 51
  lstat(18) = 5
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
99999 return
end subroutine over5

!
! subroutine over5a.
!

subroutine over5a
  use blkcom
  use labcom
  use tacsar
  use smach
  use smtacs
  use umcom
  use dekspy
  use tracom
  use movcop
  use umdat
  use freedom
  implicit none
  character(8) :: text12
  integer(4) :: i, iprint
  integer(4) :: j, j30
  integer(4) :: k, k13, kpen(4), kpu
  integer(4) :: ll0, ll2, ll3, ll4, loutbr
  integer(4) :: m, machfl
  integer(4) :: n1, n2, n3, n14, n2mach, ndx1, ndx2
  real(8) :: a
  real(8) :: d1, d2, dcfreq
  real(8) :: gus2, gus3, gus4
  real(8) :: h1, h2, h3
  real(8) :: reps
  real(8) :: smamp, smang, smangl
  real(8) :: yx
  !
  !  dimension ispum(1), kpen(4)
  !  equivalence (spum(1), ispum(1))
  !
  integer(4), allocatable, target :: ispum(:)
  !
  data j30 / 1 /
  data text12 / 'typ-16' /
  !
  ll0 = size (transfer (spum, ispum))
  allocate (ispum(ll0))
  ispum = transfer (spum, ispum)
  ll2 = 2
  ll3 = 3
  ll4 = 4
  iread = 0
  istead = 0
  iprint = 4
  kpu = 0
  nsmout = 0
  machfl = 0
  numum = 0
  call move0 (ksmspy(1 :), ll3)
  call move0 (kode(1 :), ntot)
  kode(1) = -1
  ! read input card using cimage.
310 call cimage
  if (kill .gt. 0) return
  if (machfl .eq. 1) go to 110
  if (machfl .eq. 2) go to 110
  if (machfl .eq. 3) machfl = 0
  if (kolbeg .gt. 0) go to 313
  read (unit = abuff, fmt = 6304) n2, bus1, n1
6304 format (i2, a6, i2)
  if (n2 .ne. 18) go to 6347
  ! special type-18 input extends preceding source to allow
  ! for ungrounded 2nd terminal and ideal transformer:
  kconst = kconst + 1
  lstat(19) = 6304
  iprint = 4
  if (kconst .gt. lexct) go to 9000
  read (unit = abuff, fmt = 6309) crest(kconst), bus2, bus3, bus4
6309 format (10x, e10.0, 3a6)
  call move0 (kpen(1 :), ll4)
  do j = 1, ntot
     if (bus(j) .ne. bus1) go to 6313
     kpen(1) = j
     node(kconst) = j
     iform(kconst) = 18
6313 if (bus(j) .ne. bus2) go to 6317
     kpen(2) = j
     time2(kconst) = j
6317 if (bus(j) .ne. bus3) cycle
     kpen(3) = j
     sfreq(kconst) = j
  end do
  if (kpen(1) .ne. 0) go to 6325
  bus6 = bus1
  go to 6330
6325 if (kpen(2) .ne. 0) go to 6326
  bus6 = bus2
  go to 6330
6326 if (kpen(3) .ne. 0) go to 6335
  bus6 = bus3
6330 write (unit = lunit(6), fmt = 6331) bus6
6331 format ('  ++++++  Warning.  ++++++  type-18 source has unrecognizable node name  ', '"', a6, '"', ' .   Device ignored.')
  kconst = kconst - 1
  go to 310
6335 if (n1 .ne. -1) go to 6339
  ! ungrounded current source (no transformer allowed):
  iform(kconst) = iform(kconst - 1)
  crest(kconst) = -crest(kconst - 1)
  node(kconst) = kpen(1)
  sfreq(kconst) = sfreq(kconst - 1)
  tstart(kconst) = tstart(kconst - 1)
  tstop(kconst) = tstop(kconst - 1)
  time1(kconst) = time1(kconst - 1)
  go to 310
6339 n14 = iabs (node(kconst - 1))
  kode(n14) = 0
  ntot = ntot + 1
  ktrans(ntot) = -664422
  iprint = 1
  lstat(19) = 6335
  if (ntot .gt. lbus) go to 9000
  bus(ntot) = bus4
  time1(kconst) = ntot
  iprint = 2
  lstat(19) = 6343
  if (ibr + 8 .gt. lbrnch) go to 9000
  iprint = 3
  lstat(19) = 6343
  if (it + 8 .gt. ldata) go to 9000
  kpen(4) = iabs (node(kconst - 1))
  volti(ll4) = -1.0
  volti(1) = +1.0
  volti(ll2) = +1.0 / crest(kconst)
  volti(ll3) = -volti(ll2)
  do j = 1, 4
     if (volti(j) .eq. 0.0d0) cycle
     ibr = ibr + 1
     kbus(ibr) = kpen(j)
     mbus(ibr) = ntot
     length(ibr) = 1
     nr(ibr) = it + 1
     it = it + 1
     tr(it) = volti(j)
     tx(it) = 0.0d0
     c(it) = 0.0d0
     r(it) = 0.0d0
     ibr = ibr + 1
     kbus(ibr) = kpen(j)
     mbus(ibr) = 1
     length(ibr) = 1
     nr(ibr) = it + 1
     it = it + 1
     tr(it) = -volti(j)
     tx(it) = 0.0d0
     c(it) = 0.0d0
     r(it) = 0.0d0
  end do
  go to 310
  ! end of type-18 (ideal transformer) code;  consider non-18:
6347 continue
  read (unit = abuff, fmt = 6358) a, d1, gus2, h1, h2, gus3, gus4
6358 format (10x, 7e10.0)
  if (n2 .ne. 19) go to 314
  ! universal machine of hian lauw --- type-19 source.
  call umoffs
  call umdata (spum(iureac :), spum(iugpar :), spum(iufpar :), spum(iuhist :), spum(iuumrp :), ispum(iunod1 :), ispum(iunod2 :), ispum(iujclt :), ispum(iujclo :), ispum(iujtyp :), ispum(iunodo :), ispum(iujtmt :), spum(iuhism :), spum(iuomgm :), spum(iuomld :), spum(iutham :), spum(iuredu :), spum(iureds :), spum(iuflds :), spum(iufldr :), spum(iurequ :), spum(iuflqs :), spum(iuflqr :), ispum(iujcds :), ispum(iujcqs :), spum(iuflxd :), spum(iuflxq :), ispum(iunppa :), spum(iurotm :), ispum(iuncld :), ispum(iunclq :), ispum(iujtqo :), ispum(iujomo :), ispum(iujtho :), spum(iureqs :), spum(iuepso :), spum(iudcoe :), ispum(iukcoi :), spum(iuvolt :), spum(iuangl :), ispum(iunodf :), ispum(iunodm :), ispum(iukumo :), ispum(iujumo :), spum(iuumou :))
  if (kill .gt. 0) return
  go to 310
313 continue
  read (unit = abuff, fmt = 6304) n2, bus1
  nfrfld = 1
  nright = 6
  call ffree (h2)
  n1 = int (h2)
  call ffree (a)
  call ffree (d1)
  call ffree (gus2)
  call ffree (h1)
  call ffree (h2)
  call ffree (gus3)
  call ffree (gus4)
314 smang = gus2
  if (n2 .gt. 0) go to 4568
  if (bus1 .ne. blank) go to 4562
  if (a .eq. 0.0) go to 400
4562 kill = 10
  lstat(19) = 4562
  lstat(16) = n2
  return
4568 if (n2 .ge. 50 .and. n2 .le. 59) go to 114
  ! check that conventional sources dont follow synch.machines
  if (numsm .eq. 0) go to 114
  kill = 177
  lstat(14) = n2
  lstat(19) = 8
  return
110 if (kolbeg .gt. 0) go to 7642
  read (unit = abuff, fmt = 111) bus1
111 format (2x, a6)
  go to 7646
7642 nfrfld = 1
  nright = -1
  call ffree (d1)
  nright = 0
  bus1 = texta6(1)
7646 smang = smang - 120.
  read (unit = abuff, fmt = 102) smamp, smangl
102 format (10x, e10.6, 10x, e10.6)
  if (smamp .eq. 0. .and. smangl .eq. 0.) go  to  103
  smang = smangl
  a = smamp
103 n2 = 14
  gus2 = smang
  if (machfl .eq. 2) go to 130
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 3128) a, smang
3128 format ('+2nd phase of s.m. ', e12.5, 2x, f8.2)
  go to 113
130 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3129) a, smang
3129 format ('+3rd phase of s.m. ', e12.5, 2x, f8.2)
  !     transfer control to synchronous machine data-input subroutine.
  call smdat (n2mach)
  if (kill .gt. 0) return
113 machfl = machfl + 1
114 kconst = kconst + 1
  kpu = kpu+1
  if (n2 .lt. 50) go to 115
  if (n2 .gt. 59) go to 115
  !     this source is phase a of a synchronous machine
  numsm = numsm + 1
  j30 = 30 * numsm - 28
  if (numsm .le. lsyn) go to 125
  kill = 1
  lstat(16) = 17
  lstat(19) = 125
  return
125 machfl = 1
  n2mach = n2
  n2 = 14
  gus3 = -9988.
115 if (kconst .le. lexct) go to 27
  lstat(19) = 310
  go to 9000
27 if (gus4 .eq. 0.0) gus4 = fltinf
  if (kpu .eq. 1) pu = a
  sfreq(kconst) = d1
  if (machfl .le. 1 .and. noutpr .eq. 0) write (unit = kunit6, fmt = 7) a, sfreq(kconst), gus2, gus3
7 format ('+Source.', 2x, 4e10.2)
  if (n2 .eq. 17) go to 4259
  if (n2 .ne. 14) go to 317
  if (sfreq(kconst) .gt. 0.0) go to 317
  kill = 149
  flstat(14) = sfreq(kconst)
  lstat(19) = 317
  return
317 do i = 1, ntot
     if (bus1 .eq. bus(i)) go to 330
  end do
  lstat(19) = 320
4258 kill = 12
  return
330 if (n2 .le. 16) go to 4250
  lstat(19) = 330
  if (n2 .lt. 60) go to 4249
4259 do m = 1, ktab
     ndx1 = ilntab(klntab + m)
     if (bus1 .eq. texvec(ndx1)) go to 3435
  end do
  lstat(19) = 3429
  go to 4249
3435 sfreq(kconst) = m
  go to 362
4249 kill = 10
  lstat(16) = n2
  return
4250 if (n2 .ne. 16) go to 349
  if (n1 .ge. 1 .and. n1 .le. 3) go to 4254
  kill = 11
  lstat(19) = 4250
  lstat(16) = n1
  return
4254 d2 = gus2 + h2
  h3 = gus2 * h2
  yx = gus3
  gus3 = 1.0 + d2 / delta2 + h3 / delta2 / delta2
  gus4 = (1.0 + h1 / delta2) * a
  ck1 = sfreq(kconst)
  sfreq(kconst) = a
  time1(kconst) = gus3
  tstart(kconst) = 4.0 * h3 / deltat / gus3
  iform(kconst) = 16
  node(kconst) = -i
  kconst = kconst + 1
  lstat(19) = 4254
  if (kconst .gt. lexct) go to 9000
  iform(kconst) = n1
  !     read input card using cimage.
  call cimage
  if (kolbeg .gt. 0) go to 7653
  read (unit = abuff, fmt = 4255) bus1, n3, a, gus2, time1(kconst), tstart(kconst), reps, dcfreq, loutbr
4255 format (2x, a6, i2, 6e10.6, 9x, i1)
  go to 7658
7653 nfrfld = 1
  call ffree (voltbc)
  n3 = int (voltbc(1))
  nright = -1
  call ffree (d1)
  nright = 0
  bus1 = texta6(1)
  call ffree (voltbc)
  n3 = int (voltbc(1))
  call ffree (a)
  call ffree (gus2)
  call ffree (time1(kconst :))
  call ffree (tstart(kconst :))
  call ffree (reps)
  call ffree (dcfreq)
  call ffree (voltbc)
  loutbr = int (voltbc(1))
7658 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54137)
54137 format ('+Second DC simulator card.')
  if (dcfreq .eq. 0.) dcfreq = tenm3
  crest(kconst - 1) = a * gus3 / gus2 / gus4
  gus3 = gus4 * gus2 / gus3
  do k = 1, ntot
     if (bus1 .eq. bus(k)) go to 342
  end do
  lstat(19) = 341
  go to 4258
342 n3 = 4
  if (yx .lt. 0.) yx = 0.
  ntot = ntot + 1
  bus(ntot) = text12
  kode(ntot) = 0
  if (loutbr .lt. 2) go to 4257
  nv = nv + 1
  if (nv .le. lsiz12) go to 4256
  iprint = 11
  lstat(19) = 4256
  go to 9000
4256 ibrnch(nv) = k
  jbrnch(nv) = i
  if (loutbr .eq. 2) loutbr = 0
4257 ntot = ntot + 1
  iprint = 1
  lstat(19) = 342
  if (ntot .gt. lbus) go to 9000
  bus(ntot) = trash
  kode(ntot) = 0
  iprint = 6
  kswtch = kswtch + 1
  if (kswtch .le. lswtch) go to 2342
  lstat(19) = 2342
  go to 9000
2342 kmswit(kswtch) = ntot
  ndx1 = kswtch + lswtch
  kmswit(ndx1) = ntot - 1
  kpos(kswtch) = n3
  kswtyp(kswtch) = 0
  tclose(kswtch) = -yx
  topen(kswtch) = deltat + deltat / 2.0
  adelay(kswtch) = 0.
  crit(kswtch) = 0.
  energy(kswtch) = 0.
  isourc(kswtch) = 0
  ibr = ibr + 1
  kbus(ibr) = k
  mbus(ibr) = ntot - 1
  if (loutbr .gt. 0) mbus(ibr) = -mbus(ibr)
  length(ibr) = 1
  it = it + 1
  if (reps .eq. 0.0) reps = sqrtz (epsiln)
  tr(it) = reps
  tx(it) = 0.0
  c(it) = 0.0
  nr(ibr) = -it
  k = ntot
  node(kconst) = -k
  ibr = ibr + 1
  iprint = 2
  if (ibr .le. lbrnch) go to 2343
  lstat(19) = 2343
  go to 9000
2343 kbus(ibr) = i
  mbus(ibr) = k
  it = it + 1
  iprint = 3
  if (it .le. ldata) go to 2344
  lstat(19) = 2344
  go to 9000
2344 iprint = 4
  nr(ibr) = -it
  length(ibr) = 1
  c(it) = 0.
  tr(it) = gus3
  tx(it) = 0.
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 340) gus3
340 format ('+', 30x, 'equiv R=', f10.6)
  tstop(kconst - 1) = 1.0 / gus3
  crest(kconst) = gus4
  tstop(kconst) = ck1
  if (n1 .le. 0) go to 310
  kconst = kconst + 2
  lstat(19) = 340
  if (kconst .gt. lexct) go to 9000
  if (istead .eq. 0) omega = dcfreq * twopi
  istead = 1
  node(kconst - 1) = -i
  node(kconst) = -k
  iform(kconst - 1) = 14
  iform(kconst) = 14
  if (n1 .ne. 1) go to 343
  if (ck1 .le. tstart(kconst - 2)) go to 4265
4264 kill = 13
  lstat(19) = 4264
  return
4265 if (ck1 .lt. time1(kconst - 2)) go to 4264
343 if (n1 .eq. 2) ck1 = tstart(kconst - 2)
  if (n1 .eq. 3) ck1 = time1(kconst - 2)
  gus2 = ck1 / gus3 + yx
  crest(kconst - 1) = gus2
  crest(kconst) = -gus2
  tstart(kconst - 1) = -1.0d0
  tstart(kconst) = -1.0d0
  tstop(kconst - 1) = delta2
  tstop(kconst) = delta2
  sfreq(kconst) = dcfreq
  sfreq(kconst - 1) = dcfreq
  time1(kconst - 1) = 0.0d0
  time1(kconst) = 0.0d0
  go to 310
349 if (n2 .le. 10) go to 362
  if (a .ne. 0.0d0) go to 362
  kill = 73
  lstat(19) = 349
  lstat(16) = n2
  return
362 if (n1 .ge. 0) go to 336
  i = -i
331 node(kconst) = i
  k13 = j30 + machfl
  ismdat(k13) = i
  if (n2 .gt. 0) go to 4266
  lstat(19) = 331
  go to 4249
4266 tstop(kconst) = gus4
  iform(kconst) = n2
  crest(kconst) = a
  if (n2 .eq. 14) go to 332
  if (gus3 .lt. 0.0d0) gus3 = 0.0d0
  if ((n2 .le. 10) .and. (crest(kconst) .eq. 0.0d0)) iread = 1
  if (n2 .eq. 13) sfreq(kconst) = (h1 - a) / (h2 - gus2)
  go to 333
332 gus2 = gus2 * twopi / 360.0d0
  if (h1 .gt. 0.0d0) gus2 = gus2 * sfreq(kconst) * 360.0d0
  if (gus3 .ge. 0.0d0) go to 333
  if ((omega .eq. 0.0) .and. (istead .eq. 0)) omega = twopi * sfreq(kconst)
  istead = 1
333 time1(kconst) = gus2
  tstart(kconst) = gus3
  if (i .lt. 0) go to 310
  k = 0
334 k = k + 1
  if (k .eq. kconst) go to 310
  if (node(k) .eq. i) go to 335
  go to 334
335 iform(kconst) = -n2
  go to 310
336 kode(i) = -i
  go to 331
400 if (noutpr .eq. 0) write (unit = kunit6, fmt = 54139)
54139 format ('+Blank card terminating source cards.')
  if (numsm .gt. 0) mfirst = ismdat(24)
  call interp
  if (kconst .le. 1) go to 4145
  do j = 1, kconst
     if (iabs (iform(j)) .ne. 17) cycle
     iform(j) = 17
     crest(j) = crest(j + 1)
     write (unit = lunit(6), fmt = 6616) j, crest(j)
6616 format (' Type-17 reassign. j, crest(j) =', i5, e15.4)
  end do
4145 if (t .gt. 0.0d0) go to 5737
  if (istead .ne. 0) go to 5737
  if (inonl .le. 0) go to 5737
  do j = 1, inonl
     if (nltype(j) .ne. -96) cycle
     kill = 203
     lstat(19) = 5732
     return
  end do
5737 if (ncomp .gt. 0) go to 419
  if (inonl .gt. num99) ncomp = 1
419 if (fmaxfs .eq. 0.0d0) go to 447
  tmax = 0.0d0
  n1 = 0
  n2 = 0
  if (kconst .eq. 0) go to 433
  do j = 1, kconst
     if (iform(j) .ne. 14) cycle
     n1 = n1 + 1
     if (tstart(j) .lt. 0.0) go to 424
     n2 = n2 + 1
424  sfreq(j) = fminfs
  end do
  if (n1 .gt. n2) go to 437
433 kill = 194
  lstat(13) = n1
  lstat(14) = n1 - n2
  lstat(19) = 433
  go to 9999
437 if (n2 .eq. 0) go to 447
  write (unit = lunit(6), fmt = 441) kconst, n1, n2
441 format (/, " Notice  ----  this  'frequency scan'  data case contains", i5, '   EMTP sources, of which', i5, '   are', /, 15x, "sinusoidal (type-code 14).   But of these sinusoidal sources, not all have field  'tstart'  of columns", /, 15x, '61-70 punched negative (which would indicate that such a source is present during the steady-state', /, 15x, 'phasor solutions).   There were',  i5, '   exceptions of this sort.   The user is reminded that only')
  write (unit = lunit(6), fmt = 442)
442 format (15x, "sinusoidal sources which have negative  'tstart' will affect the EMTP solution.   By definition of", /, 15x, " 'frequency scan' ,   this data case will involve only steady-state phasor solutions (there will be", /, 15x, 'no transient simulations).')
447 if (ibr .gt. 0) go to 2436
  ! degenerate case without linear branches; add infinite r:
  write (unit = lunit(6), fmt = 2430)
2430 format (/, ' This is a degenerate data case without any linear branches.  Add infinite resistance from node 2 to ground.', /)
  ibr = 1
  kbus(1) = 2
  mbus(1) = 1
  length(1) = 1
  nr(1) = -1
  it = 1
  tr(1) = 1.e10
  tx(1) = 0.0d0
  r(1) = 0.0d0
  c(1) = 0.0d0
2436 if (iprsup .lt. 3) go to 1416
  write (unit = lunit(6), fmt = 12416)
12416 format (/, ' Switch table vectors', /, '     row   bus1     bus2    kpos  kdepsm  isourc  kswtyp', 9x, 'tclose', 9x, 'adelay', 10x, 'topen', 11x, 'crit')
  if (kswtch .le. 0) go to 2428
  do i = 1, kswtch
     ndx2 = lswtch + i
     write (unit = lunit(6), fmt = 2426) i, kmswit(i), kmswit(ndx2), kpos(i), kdepsw(i), isourc(i), kswtyp(i), tclose(i), adelay(i), topen(i), crit(i)
  end do
2426 format (7i8, 4e15.6)
2428 write (unit = lunit(6), fmt = 13416) (i, kbus(i), mbus(i), nr(i), length(i), kodebr(i), kodsem(i), i = 1, ibr)
13416 format (/, ' Branch table vectors', /, '       row      kbus      mbus        nr    length    kodebr    kodsem', /, (7i10))
  write (unit = lunit(6), fmt = 14416) (i, iform(i), node(i), crest(i), time1(i), tstart(i), sfreq(i), i = 1, kconst)
14416 format (/, ' Source table vectors', /, '       row     iform node', 15x, 'crest', 15x, 'time1', 14x, 'tstart', 15x, 'sfreq', /, (3i10, 4e20.10))
  write (unit = lunit(6), fmt = 2584) (i, tr(i), tx(i), r(i), c(i), i = 1, it)
2584 format (/, ' Rows 1 through it of parameters follow: ', /, 7x, 'row', 13x, 'tr', 13x, 'tx', 14x, 'R', 14x, 'C', /, (i10, 4e15.5))
1416 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1417) kconst, ibr, inonl, kswtch, istead, omega, xopt, copt, twopi
1417 format (/, ' Normal exit "over5a".  kconst     ibr   inonl  kswtch  istead', 8x, 'omega', 9x, 'xopt', 9x, 'copt', 8x, 'twopi', /, 22x, 5i8, 4e13.4)
  lastov = nchain
  nchain = nchain + 1
  go to 9999
9000 lstat(16) = iprint
  kill = 1
9999 if (allocated (ispum)) then
     spum = transfer (ispum, spum)
     deallocate (ispum)
  end if
  return
end subroutine over5a

!
! subroutine umoffs.
!

subroutine umoffs
  use blkcom
  use umcom
  implicit none
  ! overlay-5  u.m.  module called by "over5a".
  integer(4) :: i
  integer(4) :: n5
  real(8) :: d5
  !
  if (nclfix .gt. 0) go to 1758
  nclfix = 20
  numfix = 3
  iotfix = 50
  ibsfix = 60
1758 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1853) (nbyte(i), i = 1, 4), nclfix, numfix, iotfix, ibsfix
1853 format (/, ' Begin  "umoffs" .  nbyte1  nbyte2  nbyte3  nbyte4  nclfix  numfix  iotfix  ibsfix', /, 18x, 10i8)
  d5 = nbyte(4)
  d5 = d5 / nbyte(3)
  iureac = 1
  ! step over all coil-table vectors next.
  iugpar = iureac + nclfix
  iufpar = iugpar + nclfix
  iuhist = iufpar + nclfix
  iuumrp = iuhist + nclfix
  iunod1 = int ((iuumrp + nclfix) / d5) + 1
  iunod2 = iunod1 + nclfix
  iujclt = iunod2 + nclfix
  iujclo = iujclt + nclfix
  iujtyp = iujclo + nclfix
  ! step over all machine-table vectors next.
  iunodo = iujtyp + numfix
  iujtmt = iunodo + numfix
  iuhism = int ((iujtmt + numfix) * d5) + 1
  iuomgm = iuhism + numfix
  iuomld = iuomgm + numfix
  iutham = iuomld + numfix
  iuredu = iutham + numfix
  iureds = iuredu + numfix
  iuflds = iureds + numfix
  iufldr = iuflds + numfix
  iurequ = iufldr + numfix
  iuflqs = iurequ + numfix
  iuflqr = iuflqs + numfix
  iujcds = int ((iuflqr + numfix) / d5) + 1
  iujcqs = iujcds + numfix
  iuflxd = int ((iujcqs + numfix) * d5) + 1
  iuflxq = iuflxd + numfix
  iunppa = int ((iuflxq + numfix) / d5) + 1
  iurotm = int ((iunppa + numfix) * d5) + 1
  iuncld = int ((iurotm + numfix) / d5) + 1
  iunclq = iuncld + numfix
  iujtqo = iunclq + numfix
  iujomo = iujtqo + numfix
  iujtho = iujomo + numfix
  iureqs = int ((iujtho + numfix) * d5) + 1
  iuepso = iureqs + numfix
  iudcoe = iuepso + numfix
  iukcoi = int ((iudcoe + numfix) / d5) + 1
  iuvolt = int ((iukcoi + numfix) * d5) + 1
  iuangl = iuvolt + numfix
  iunodf = int ((iuangl + numfix) / d5) + 1
  iunodm = iunodf + numfix
  iukumo = iunodm + numfix
  ! step over all output-table vectors next.
  iujumo = iukumo + iotfix
  iuumou = int ((iujumo + iotfix) * d5) + 1
  ! finally, step over last vector, and check for overflow.
  n5 = iuumou + ibsfix
  lstat(45) = n5
  if (n5 .le. lspcum) go to 3458
  ! following is temporary message, to be moved to error overlays
  ! later, when we have time.
  write (unit = lunit(6), fmt = 3451) n5, lspcum, nclfix, numfix, iotfix, ibsfix
3451 format (/, ' Overflow error stop in  "umoffs" .    Insufficient total working space.', /, 1x, 10i8)
  call stoptp
3458 if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3465) n5, lspcum
3465 format (/, ' Exit  "umoffs" .   n5, lspcum =', 2i6, /)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 3472) n5
3472 format ('+u.m. data begins.  list-25 cells used =', i5)
  return
end subroutine umoffs


!
! subroutine smdat.
!

subroutine smdat (mtype)
  use blkcom
  use labcom
  use tacsar
  use smtacs
  use smach
  use indcom
  use tracom
  use movcop
  use strcom
  use freedom
  implicit none
  ! This module is used only by type-59 Brandwajn  s.m.  model
  ! The following array is sized 3 (nn4-1) * n50 ( n50 = 50  at pre-
  ! sent ) + nn10 ( no. of class 1 requests <=15 at present> ) ******
  integer(4), intent(in) :: mtype
  character(8) :: text1, text2, text3
  character(8) :: text7, text8, text10, text11, text12
  character(8) :: text16, text17
  character(8) :: text18, text19, text20, text21
  integer(4) :: npbuf(165)
  integer(4) :: i, i26, i30, i72, i101, iall, ib2, idelta, ids, iexc, ij, ijl
  integer(4) :: ijk, ijn, il, il1, il2, iln, ilv, ioutr, ip, ip1, ipl, ism, ivar
  integer(4) :: j30, jj3, jk, jkm, jkn
  integer(4) :: k, k1, k2, kflag
  integer(4) :: m, m1, mloc
  integer(4) :: n2, n3, n4, n6, n7, n8, n9, n11, n14, n16, n17, n18, n19, n21
  integer(4) :: n30, n50, n55, n167, n3671, ndwqa, ndx1, ndy5, nfirst, niunrs
  integer(4) :: np, num2, num3, num4, num5, numask
  real(8) :: a, ad1, ad2, aglin, aq1, aq2
  real(8) :: b6
  real(8) :: d1, d2, d3, d8, d10, d11, dab, dac, dsd, dsm, dsr
  real(8) :: el1, el2, elaf, elag, elakd, elakq, elf, elfkd, elg, elgkq, elkd
  real(8) :: elkq, emf, extrs
  real(8) :: fm
  real(8) :: hico, hsp
  real(8) :: qaa, qab, qac
  real(8) :: r1, ra, rat, rat1, rat2, rf, rg, rkd, rkq, rkv, rmva
  real(8) :: sf2, sf3, sf5, sf6, sm3, sm4, smext
  real(8) :: tdop, tdopp, tqop, tqopp
  real(8) :: xd, xq, xdp, xdpp, xl, xqp, xqpp
  real(8) :: zb, zb1, zb3
  !
  !  dimension  npbuf(165)
  !  dimension massex(1)
  !
  !  equivalence (ismdat(22), ipout)
  !  equivalence (ismdat(23), n56)
  !  equivalence (ismdat(24), ismold)
  !  equivalence (ismdat(25), nn10)
  !  equivalence (ismdat(26), nn4)
  !  equivalence (ismdat(27), nn14)
  !  equivalence (histq(1), massex(1))
  !
  integer(4), pointer :: ipout => ismdat(22)
  integer(4), pointer :: ismold => ismdat(24)
  integer(4), pointer :: n56 => ismdat(23)
  integer(4), pointer :: nn10 => ismdat(25)
  integer(4), pointer :: nn14 => ismdat(27)
  integer(4), pointer :: nn4 => ismdat(26)
  real(8), pointer :: massex(:)
  !
  data  text1  / 'finish' /
  data  text2  / 'tolera' /
  data  text3  / 'nces  ' /
  data  text7  / 'parame' /
  data  text8  / 'ter   ' /
  data  text10 / 'delta ' /
  data  text11 / 'connec' /
  data  text12 / 'tion  ' /
  data  text17 / 'fittin' /
  data  text18 / 'g     ' /
  data  text19 / 't     ' /
  data  text20 / 'pf    ' /
  data  text21 / 'dc    ' /
  data  text16 / ' part ' /
  !
  !massex = transfer (histq, massex)
  massex => histq(:)
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module smdat."')
  ! define no. of outputs in class 1 (nn10) and no. of classes (nn4)**
  nn10 = 15
  nn4 = 4
  nn14 = 14
  n50 = 50
  nright = 0
  nfrfld = 1
  d10 = 1.0d0 / fltinf
  if (numsm .gt. 1) go to 123
  call rinfin
  n19 = location (flstat(1)) - location (voltbc(1))
  if (n19 .ge. 41) go to 8258
  kill = 180
  lstat(18) = nchain
  lstat(19) = 8258
  lstat(17) = 41
  lstat(16) = n19
  go to 9999
8258 d8 = 3.0d0 / 2.0d0
  thtw = sqrtz (d8)
  athtw = 1.0d0 / thtw
  d8 = 3.0d0
  sqrt3 = sqrtz (d8)
  asqrt3 = 1.0d0 / sqrt3
  sqrt32 = sqrt3 * onehaf
  omdt = omega * deltat
  radeg = 360.0d0 / twopi
  bdam = 1.356306d0 * tenm6
  bin = .0421409745d0
  nst = 0
  itold = it
  ibrold = ibr
  ismold = 0
  ipout = 0
  n56 = (nn4 - 1) * n50 + nn10
  mfirst = 6 * limass
  d8 = nbyte(3)
  d8 = d8 / nbyte(4)
  om2 = d8
  d8 = mfirst * d8 - 1.0d0
  mfirst = int (d8, kind (mfirst))
123 jk = numsm
  ism = 0
  fm = -2.0d0
  k = nst + 1
  if (k .le. lsyn) go to 8818
  kill = 1
  lstat(19) = 123
  lstat(16) = 17
  go to 9999
8818 j30 = 30 * jk - 29
  ismdat(j30) = 0
  statfr = sfreq(kconst - 1)
  idelta = 0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 3604) k, nst, mtype, kconst, numsm
3604 format (/, " At start of  'smdat'  ...       k     nst   mtype  kconst   numsm  ", /, 24x, 10i8)
  ! read input card using cimage
1 call cimage
  if (kill .gt. 0) go to 9999
  nright = -2
  n11 = kolbeg
  kolbeg = 1
  call ffree (d1)
  nright = 0
  ! check for key word   'tolerances'  (or  't' ).
  if (toLower (texta6(1)) .eq. text19) go to 7103
  if (toLower (texta6(1)) .ne. text2) go to 7106
  if (toLower (texta6(2)) .ne. text3) go to 7106
7103 if (n11 .gt. 0) go to 7104
  read (unit = abuff, fmt = 3609) d1, d2, d3, n9
3609 format (10x, 3e10.0, 10x, i10)
  go to 7105
7104 nfrfld = 1
  call ffree (d1)
  call ffree (d2)
  call ffree (d3)
  call ffree (voltbc)
  n9 = int (voltbc(1))
7105 if (d1 .gt. 0.0d0) epsuba = d1
  if (d2 .gt. 0.0d0) epomeg = d2
  if (d3 .gt. 0.0d0) epdgel = d3
  if (n9 .gt. 0) iprsov(37) = n9
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 3610) epsuba, epomeg, epdgel, iprsov(37)
3610 format ('+  Epsilon.', 3e11.2, i5)
  go to 1
  ! check for key word   'parameter fitting'  (or  'pf' ).
7106 if (texta6(1) .eq. text20) go to 2115
  if (toLower (texta6(1)) .ne. text7) go to 8641
  if (toLower (texta6(2)) .ne. text8) go to 8641
  if (toLower (texta6(3)) .ne. text17) go to 8641
  if (toLower (texta6(4)) .ne. text18) go to 8641
2115 if (n11 .gt. 0) go to 7109
  read (unit = abuff, fmt = 6011) fm
6011 format (24x, e8.0)
  go to 7110
7109 nfrfld = 1
  call ffree (fm)
7110 if (fm .gt. 1.0) go to 152
  ism = 1
  go to 153
152 ism = 0
  fm = 1.0
153 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3617) fm
3617 format ("+  optimize Park's data.", 3x, e12.3)
  go to 1
  ! check for key word   'delta connection'  (or  'dc' ).
8641 if (texta6(1) .eq. text21)   go to 2125
  if (toLower (texta6(1)) .ne. text10) go to 8659
  if (toLower (texta6(2)) .ne. text11) go to 8659
  if (toLower (texta6(3)) .ne. text12) go to 8659
2125 if (noutpr .eq. 0) write (unit = kunit6, fmt = 8646)
8646 format ('+  Notification of delta-connected armature.')
  idelta = 1
  go to 1
8659 kolbeg = n11
  ismdat(j30) = ismdat(j30) + 1
  nst = nst + 1
  if (nst .le. lsyn) go to 8819
  kill = 1
  lstat(19) = 8659
  lstat(16) = 17
  go to 9999
8819 k = nst
  i101 = 101 * k - 100
  i26  = i101
  i30 = 30 * k - 29
  ismdat(i30 + 1) = idelta
  if (kolbeg .gt. 0) go to 7113
  read (unit = abuff, fmt = 5) ismdat(i30 + 11 ), ismdat(i30 + 12), ismdat(i30 + 13), np, sm3, sm4, rmva, rkv, aglin, dab, dac
5 format (3i2, i4, 7e10.6)
  go to 7114
7113 nfrfld = 4
  call ffree (voltbc)
  ismdat(i30 + 11) = int (voltbc(1))
  ismdat(i30 + 12) = int (voltbc(2))
  ismdat(i30 + 13) = int (voltbc(3))
  np = int (voltbc(4))
  nfrfld = 1
  call ffree (sm3)
  call ffree (sm4)
  call ffree (rmva)
  call ffree (rkv)
  call ffree (aglin)
  call ffree (dab)
  call ffree (dac)
7114 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3619) ismdat(i30 + 11), ismdat(i30 + 12), ismdat(i30 + 13), np, sm3, sm4
3619 format ('+  4th s.m. card.', 4i4, 2f7.3)
  elp(i26 + 25) = np / 2
  if (sm3 .eq. 0.0d0) sm3 = d10
  if (sm4 .eq. 0.0d0) sm4 = d10
  elp(i26 + 28) = sm3
  elp(i26 + 29) = sm4
  numask = ismdat(i30 + 11)
  if (numask .lt. 0) go to 306
  iln = ismold * 12
  ismold = ismold + numask
  if (ismold .le. limass) go to 8
  kill = 1
  lstat(19) = 300
  lstat(16) = 16
  go to 9999
306 kill = 105
  lstat(19) = 306
  lstat(14) = numask
  lstat(18) = nchain
  go to 9999
8 ismdat(i30 + 8) = 0
  if (aglin .gt. 0.0d0) go to 150
  ismdat(i30 + 8) = 2
150 aglin = absz (aglin)
  ! read  input  cards  using  cimage
  call  cimage
  read (unit = abuff, fmt = 163) ad1, ad2, aq1, aq2, qaa, qab, qac
163 format (10x, 7e10.6)
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 154) aq1, qaa, qab, qac
154 format ('+  q-axis', f7.3, 3f10.4)
  ! read input card using cimage.
  call cimage
  zb = (rkv ** 2) / rmva
  ! if ( idelta  .eq.  1  )  zb = 3.d0 * zb
  emf = rkv / (tenm3 * aglin)
  if (fm .le. -1.0d0) go to 100
  if (kolbeg .gt. 0) go to 7124
  read (unit = abuff, fmt = 6) ra, xl, xd, xq, xdp, xqp, xdpp, xqpp
6 format (8e10.6)
  go to 7125
7124 call ffree (ra)
  call ffree (xl)
  call ffree (xd)
  call ffree (xq)
  call ffree (xdp)
  call ffree (xqp)
  call ffree (xdpp)
  call ffree (xqpp)
7125 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3628) ra, xl, xd
3628 format ('+  5th s.m. card.', 3f10.4)
  ! check for an error in reactance values ***************************
  if (ra .lt. 0.0d0) go to 19
  if (xl .lt. 0.0d0) go to 19
  if (xd .lt. 0.0d0) go to 19
  if (xq .lt. 0.0d0) go to 19
  if (xdp .lt. 0.0d0) go to 19
  if (xqp .lt. 0.0d0) go to 19
  if (xdpp .lt. 0.0d0) go to 19
  if (xqpp .lt. 0.0d0) go to 19
  if (((xd - xdp) .le. flzero) .and. (fm .eq. 0.0d0)) go to 19
  if ((xdp - xdpp) .le. flzero) go to 19
  if ((xdpp - xl) .le. flzero) go to 19
  ndwqa = 0
  if ((xq .eq. xqp) .and. (xqp .eq. xqpp)) ndwqa = -2
  if (ndwqa .lt. 0) go to 25
  if (((xq - xqp) .le. flzero) .and. (fm .eq. 0.0d0)) go to 19
  if ((xqp - xqpp) .le. flzero) go to 19
  if ((xqpp - xl) .le. flzero) go to 19
  go to 25
19 kill = 176
  go to 9999
25 continue
  ! convert machine parameters to physical values, this will  result
  ! in all data being referred to the rotor side ********************
  xq = xq * zb
  xd = xd * zb
  xl = xl * zb
  ra = ra * zb
  if (iprsup .gt. 0) write (unit = kunit6, fmt = 3628) ra, xl, xd
  ! read input card using cimage
  call cimage
  if (kolbeg .gt. 0) go to 7136
  read (unit = abuff, fmt = 30) tdop, tqop, tdopp, tqopp, el2, r1, el1
30 format (7e10.6, 9x, i1)
  go to 7137
7136 call ffree (tdop)
  call ffree (tqop)
  call ffree (tdopp)
  call ffree (tqopp)
  call ffree (el2)
  call ffree (r1)
  call ffree (el1)
7137 if (noutpr  .eq.  0) write (unit = kunit6, fmt = 3637) tdop, tqop, tdopp
3637 format ('+  6th s.m. card.', 3f10.4)
  ! process manufacturer's data ,start with the d-axis **************
  x1(1) = xd
  x1(2) = xdp * zb
  x1(3) = xdpp * zb
  x1(4) = tdopp * omega
  x1(5) = tdop * omega
  x1(6) = xl
  call smpfit (x1(1), ism, fm, lunit(6), noutpr)
  ! calculate transformer ratio for scaling of rotor circuits ********
  a = x1(2) / thtw
  rat = emf / a
  rat2 = 2.0d0 * rat ** 2 / 3.0d0
  a = emf * rat / thtw
  ! load processed data into permanent storage **********************
  ib2 = 20
  call move0 (elp(i26 :), ib2)
  elp(i26) = xd
  elp(i26 + 1) = emf
  if (x1(5) .eq. 0) go to 6014
  elp(i26 + 2) = x1(3) * rat2
  elp(i26 + 3) = emf
  elp(i26 + 4) = a
  elp(i26 + 5) = x1(4) * rat2
  elp(i26 + 6) = x1(5) * rat2
  elp(i26 + 7) = x1(6) * rat2
  go to 6015
6014 elp(i26 + 2) = x1(4) * rat2
  elp(i26 + 5) = rat2
  elp(i26 + 6) = x1(6) * rat2
6015 rat1 = emf / x1(2)
  ! quadrature axis parameters ***************************************
  elp(i26 + 8) = xq
  if (ndwqa .lt. 0) go to 6013
  x1(1) = xq
  x1(2) = xqp * zb
  x1(3) = xqpp * zb
  x1(4) = tqopp * omega
  x1(5) = tqop * omega
  x1(6) = xl
  call smpfit (x1(1), ism, fm, lunit(6), noutpr)
  a = x1(2) * rat / thtw
  elp(i26 + 9) = a
  if (x1(5) .eq. 0.) go to 71
  elp(i26 + 10) = x1(3) * rat2
  elp(i26 + 11) = a
  elp(i26 + 12) = x1(2) * rat2
  elp(i26 + 13) = x1(4) * rat2
  elp(i26 + 14) = x1(5) * rat2
  elp(i26 + 15) = x1(6) * rat2
  go to 70
71 elp(i26 + 10) = x1(4) * rat2
  elp(i26 + 13) = rat2
  elp(i26 + 14) = x1(6) * rat2
  go to 70
6013 elp(i26 + 10) = rat2
  elp(i26 + 13) = rat2
  ! store remaining variables ***************************************
70 elp(i26 + 16) = el2 * zb  + 3.0 * el1
  elp(i26 + 17) = ra + 3.0 * r1
  elp(i26 + 18) = xl
  elp(i26 + 19) = ra
  elp(i26 + 20) = rat1
  ! calculate and store saturation constants ************************
  if (ismdat(i30 + 8) .eq. 0) go to 158
  ! start   with   the  d-axis   characteristic***********************
  ! check for inconsistent saturation data ***************************
  if (dab .ge. aglin .and. dac .gt. dab) go to 167
  kill = 211
  lstat(14) = k
  lstat(19) = 167
  flstat(13) = aglin
  flstat(14) = dab
  flstat(15) = dac
  go to 9999
167 if (ad1 .le. 0.0d0) ad1 = 1.0d0
  if (ad2 .le. 0.0d0) ad2 = 1.2d0
  sf5 = dab / (aglin * ad1)
  sf2 = dac / (aglin * ad2)
  sf3 = (sf2 - sf5) / (dac - dab)
  sf6 = dac + (1.0d0 - sf2) / sf3
  if (sf6 .gt. (aglin * onehaf)) go to 437
  ! reset saturation parameters *************************************
  dsm = rkv / aglin
  hsp = rkv * (ad2 - ad1) / (dac - dab)
  dsd = rkv * ad1 - hsp * dab
  dsr = dsd / (dsm - hsp)
  ilv = 167
  write (unit = lunit(6), fmt = 6110) ilv, sf6, dsr
6110 format ('  ****** Warning, warning ******', /, '  subroutine smdat, nearby statement number', i8, /, '  saturation treshold reset from', 2x, e15.8, 3x, 'to', 2x, e15.8)
  sf6 = dsr
  sf3 = (1.0d0 - sf2) / (sf6 - dac)
437 elp(i26 + 21) = sf6
  elp(i26 + 22) = sf3
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 155) k, sf3, sf6, sf5, sf2
155 format (1x, ' Saturation constants.', 7x, 'k', 11x, 'sf3', 11x, 'sf6', 11x, 'sf5', 11x, 'sf2', /, 22x, i8, 4e14.5, /, 1x)
  ! calculate  parameters  for  the  q-axis **************************
  if (qaa .gt. 0.0d0) go to 157
  ! unknown   parameters-calculate an  approximate characteristic ****
  b6 = (elp(i26 + 8) - xl) / (elp(i26) - xl)
  sf6 = sf6 * b6
  elp(i26 + 23) = sf6
  elp(i26 + 24) = sf3
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 156) k, sf3, sf6
156 format (10x, 'Approximate characteristic for the q-axis, machine no.', i5, /, 14x, 'qsat12', 14x, 'qsat10', /, 2(6x, e14.5))
  go to 158
  ! known  parameters , calculate constants **************************
  ! check for missing  saturation data for the q-axis ****************
157 if (qab .ge. qaa .and. qac .gt. qab) go to 166
  kill = 211
  lstat(19) = 157
  lstat(14) = k
  flstat(13) = qaa
  flstat(14) = qab
  flstat(15) = qac
  go to 9999
166 if (aq1 .le. 0.0d0) aq1 = 1.0d0
  if (aq2 .le. 0.0d0) aq2 = 1.2d0
  sf5 = qab / (qaa * aq1)
  sf2 = qac / (qaa * aq2)
  sf3 = (sf2 - sf5) / (qac - qab)
  sf6 = qac + (1.0d0 - sf2) / sf3
  if (sf6 .gt. (qaa * onehaf)) go to 439
  dsm = rkv / qaa
  hsp = rkv * (aq2 - aq1) / (qac - qab)
  dsd = rkv * aq1 - hsp * qab
  dsr = dsd / (dsm - hsp)
  ilv = 166
  write (unit = lunit(6), fmt = 6110) ilv, sf6, dsr
  sf6 = dsr
  sf3 = (1.0d0 - sf2) / (sf6 - dac)
439 elp(i26 + 23) = sf6
  elp(i26 + 24) = sf3
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 156) k, sf3, sf6, sf5, sf2
  ! process mechanical data of the generator  ***********************
  ! scale the mechanical data to mks units   *************************
  ! this scaling of data corresponds to torque given in mva  *********
158 smext = 0.
  num2 = numask + numask
  num3 = num2 + numask
  num4 = num2 + num2
  num5 = num3 + num2
  ib2 = iln + num2
  do i = 1, numask
     ! read input card using cimage
     call cimage
     if (kolbeg .gt. 0) go to 7144
     read (unit = abuff, fmt = 36) mloc, extrs, hico, dsr, dsm, hsp, dsd
36   format (i2, 8x, 6e10.6)
     go to 7145
7144 call ffree (voltbc)
     if (kill .gt. 0) go to 9999
     mloc = int (voltbc(1))
     nfrfld = 1
     call ffree (extrs)
     call ffree (hico)
     call ffree (dsr)
     call ffree (dsm)
     call ffree (hsp)
     call ffree (dsd)
7145 if ((mloc .gt. 0) .and. (mloc .le. numask)) go to 7146
     n167 = 7145
     kill = 181
     lstat(19) = n167
     lstat(18) = nchain
     lstat(15) = mloc
     lstat(16) = numask
     go to 9999
7146 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3653) i, mloc, extrs, hico, dsd
3653 format ('+  Mass card', i2, i2, 3e11.3)
     il1 = ib2 + mloc
     shp(il1) = extrs
     shp(il1 + numask) = hico * bin
     shp(il1 + num2) = dsm * bdam
     shp(il1 + num3) = hsp * bdam / tenm6
     shp(il1 + num4) = dsd * bdam
     shp(il1 + num5) = dsr * bdam
     smext = smext + extrs
  end do
  if (numask .eq. 1) smext = 1.0
  if (absz (smext) .gt. flzero) go to 410
  kill = 107
  lstat(19) = 420
  go to 9999
410 il2 = ib2 + numask
  il1 = ib2 + 1
  do il = il1, il2
     shp(il) = shp(il) / smext
  end do
  ! check for blank card terminating mass cards  *********************
  ! read input card using cimage   ***********************************
  call cimage
  read (unit = abuff, fmt = 1122) (voltbc(i), i = 1, 41)
1122 format (41f1.0)
  n167 = 3654
  if (kill .gt. 0) go to 9999
  ! set-up dummy branches to reserve space for future use  ***********
  if (it + 6 .le. ldata) go to 8917
  kill = 1
  lstat(19) = 8917
  lstat(16) = 3
  go to 9999
8917 if ((ibr + 3) .le. lbrnch) go to 8921
  kill = 1
  lstat(19) = 8921
  lstat(16) = 2
  go to 9999
8921 do il = 1, 3
     it = it + 1
     ibr = ibr + 1
     length(ibr) = 1
     kbus(ibr) = 1
     mbus(ibr) = 1
     nr(ibr) = -it
     tr(it) = 1.0d0
     tx(it) = 0.0d0
     c(it) = 0.0d0
  end do
  it = it + 3
  ! read output specification card. ismout is array of output flags.
  ! zero-out output request arrays. preset flags for parameter output
  ! requests     *****************************************************
  iexc = 0
  if (ismdat(i30 + 13) .eq. 0) iexc = 1
  n14 = i30 + 20
  n8 = n14 - 3
  do il = n8, n14
     ismdat(il) = 0
  end do
  elp(i26 + 26) = 0.0d0
  elp(i26 + 27) = 0.0d0
  ism = 0
  n8 = n8 - 1
  do ij = 1, 41
     if (voltbc(ij) .ne. 0.0d0) go to 1983
  end do
  if (noutpr .eq. 0)  write (unit = kunit6, fmt = 1700)
1700 format ('+ blank card terminating mass cards.')
  ! read input card using cimage
7219 call cimage
  read (unit = abuff, fmt = 3654) n7
3654 format (i8)
  if (n7 .ne. 0) go to 8235
  if (noutpr .eq. 0) write (unit = kunit6, fmt = 3656)
3656 format ('+ blank card terminating output requests.')
  go to 7251
  ! check for overflow of npbuf() ***********************************
8235 n55 = (nn4 - 1) * numask + nn10
  if (n55 .lt. n56) go to 7220
  kill = 111
  lstat(13) = n56
  lstat(15) = k
  lstat(16) = n55
  lstat(17) = 0
  lstat(19) = 8235
  go to 9999
7220 continue
  read (unit = abuff, fmt = 3657) (voltbc(i), i = 1, nn14)
3657 format (2x, 2f1.0, 4x, 12f6.0)
  ioutr = int (voltbc(1))
  iall = int (voltbc(2))
  if (ioutr .gt. 0 .and. ioutr .le. (nn4 + 1)) go to 7221
  write (unit = lunit(6), fmt = 3658) ioutr
3658 format (1x, 10('*'),  '  Warning  ', 10('*'), 3x, ' Request for nonexistent output class ',i4, '  neglected.')
  go to 7219
7221 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3659) ioutr
3659 format ('+', 2x, ' Output request card for class ', i4)
  !7222 go to (7230, 7240, 7240, 7250, 7210), ioutr
  select case (ioutr)
  case (1)
     go to 7230

  case (2, 3)
     go to 7240

  case (4)
     go to 7250

  case (5)
     go to 7210
  end select
  !     process flags for outputs after the s.s. solution  ***************
7210 n16 = 2
  ivar = 0
  do ipl = 3, nn14
     n6 = int (voltbc(ipl))
     if (iall .eq. 0) go to 7213
     n6 = ipl - 2
     go to 7212
7213 if (n6 .gt. -1 .and. n6 .le. n16) go to 7212
     write (unit = lunit(6), fmt = 3662) n6, ioutr, k, n16
     cycle
7212 if (n6 .eq. 0) cycle
     ivar = ivar + 1
     if (ivar .gt. n16) go to 7219
     kflag = i26 + n6
     elp(kflag + 25) = n6
  end do
  go to 7219
7230 n16 = nn10 - iexc
  n21 = 0
7231 n17 = n8 + ioutr
  n21 = ismdat(n17) + n21
  if (iall .gt. 0) go to 7238
  do il = 3, nn14
     n6 = int (voltbc(il))
     if ((n6 .le. n16) .and. (n6 .gt. -1)) go to 7233
     write (unit = lunit(6), fmt = 3662) n6, ioutr, k, n16
3662 format (1x, 10('*'), '  Warning  ', 10('*'), 3x, ' Request for nonexistent variable', i4, ' in class', i3, ' of s.m. no. ', i3, ' discarded.', /, ' in this class the above s.m. can have numbers between 1(one) and', i4)
     cycle
7233 if (n6 .eq. 0) go to 7232
     ism = ism + 1
     n21 = n21 + 1
     ismdat(n17) = ismdat(n17) + 1
     npbuf(n21) = n6
     !     remove duplicate requsts. put all request in an ascending order
     ipl = ismdat(n17) - 1
     if (ipl .lt. 1) go to 7232
     ijn = n21
     do ijk = 1, ipl
        ijl = ijn - 1
        if (npbuf(ijl) .lt. npbuf(ijn)) go to 7232
        if (npbuf(ijl) .eq. npbuf(ijn)) go to 7245
        n30 = npbuf(ijl)
        npbuf(ijl) = npbuf(ijn)
        npbuf(ijn) = n30
        ijn = ijn - 1
     end do
     go to 7232
     !     discard duplicate request ****************************************
7245 write (unit = lunit(6), fmt = 7223) npbuf(ijn), ioutr
7223 format (1x, 10('*'), '  Warning  ', 10('*'), 3x, ' duplicate request', i5, '  in class', i5, '  discarded.')
     ism = ism - 1
     ismdat(n17) = ismdat(n17) - 1
     do ijk = ijn, ipl
        npbuf(ijk) = npbuf(ijk + 1)
     end do
7232 continue
  end do
7234 if ((ipout + ism) .le. lsmout) go to 7219
  kill = 111
  lstat(19) = 7234
  lstat(13) = lsmout
  lstat(15) = k
  lstat(16) = ipout + ism
  lstat(17) = 0
  go to 9999
7238 if (n16 .gt. 0) go to 7239
  write (unit = lunit(6), fmt = 3662) n16, ioutr, k, n16
  go to 7234
7239 n21 = n21 - ismdat(n17)
  ismdat(n17) = 0
  do il = 1, n16
     ismdat(n17) = ismdat(n17) + 1
     ism = ism + 1
     n21 = n21 + 1
     npbuf(n21) = il
  end do
  go to 7234
7240 n16 = numask
  n21 = nn10 + (ioutr-2) * numask
  go to 7231
7250 n16 = numask - 1
  n21 = nn10 + (ioutr-2) * numask
  go to 7231
  !     processing old output cards
1983 n21 = 0
  n17 = n8 + 1
  jj3 = 31 + numask
  jkm = -1
  ij = 1
1125 if ((ij .eq. 13) .or. (ij .eq. 23) .or. (ij .eq. 33)) go to 1999
1130 n6 = int (voltbc(ij))
  if ((n6 .gt. -1) .and. (n6 .le. 2)) go to 1155
  write (unit = lunit(6), fmt = 1133) ij, n6
1133 format (1x, 10('*'), '  Warning  ', 10('*'), /, '  The output request in column', i4,  '  discarded  because of the wrong number', i4, /, '  the correct number should be 1 or 2 .')
  go to 1899
1155 if (n6 .eq. 0) go to 1899
  if (ij .ge. 3) go to 1255
  kflag = i26 + ij
  elp(kflag + 25) = ij
  go to 1950
1255 if ((ij .ne. 3) .and. (ij .ne. 11)) go to 1355
  jkn = 0
  if (ij .eq. 11) jkn = 7
  do jk = 1, 3
     n21 = n21 + 1
     ismdat(n17) = ismdat(n17) + 1
     npbuf(n21) = jk + jkn
  end do
  if (ij .eq. 3) go to 1950
1299 ij = ij - 3
  go to 1130
1355 n21 = n21 + 1
  ismdat(n17) = ismdat(n17) + 1
  if (ij .ge. 13) go to 1990
  npbuf(n21) = ij
  if (ij .eq. 8) npbuf(n21) = npbuf(n21) + 3
  if ((ij .eq. 9) .or. (ij .eq. 10)) npbuf(n21) = npbuf(n21) + 5
1899 if ((ij .eq. 11) .or. (ij .eq. 12)) go to 1299
  if (ij .eq. 10) ij = ij + 2
  if (ij .eq. 7 .or. ij .eq. 8) ij = ij + 3
1950 ij = ij + 1
  if (ij .gt. jj3) go to 7251
  go to 1125
1990 npbuf(n21) = ij - 12 - jkm * 10
  go to 1950
1999 n17 = n17 + 1
  jkm = jkm + 1
  n21 = nn10 + jkm * numask
  go to 1130
  !     load output requests into permanent storage **********************
7251 ids = ipout + 1
  n21 = 0
  ip1 = 3 * ipout - 2
  do il = 1, 4
     n17 = n8 + il
     n6 = ismdat( n17 )
     if (n6 .eq. 0) go to 7252
     do i = 1, n6
        ipout = ipout + 1
        n21 = n21  + 1
        ip1 = ip1 + 3
        ismout(ip1) = npbuf(n21)
     end do
7252 n21 = (il - 1) * numask + nn10
  end do
  if (ids .gt. ipout) ismdat(n8 + 1) = -10
  n2 = ntotac
  i72 = 0
  niunrs = iuty(kiuty + 1)
  n18 = 3 * numask - 1
  n19 = 17
  do i = 1, 9999
     !     read auxiliary input to tacs card
     !     read input card using cimage
     call cimage
     if (kolbeg .gt. 0) go to 2183
     read (unit = abuff, fmt = 3669) n3, bus6, bus5, iv
3669 format (i2, 2a6, i3)
     go to 2185
2183 nfrfld = 1
     if ( kill  .gt.  0 )   go to 9999
     call ffree (d11)
     n3 = int (d11)
     nfrfld = -2
     call ffree (d1)
     bus6 = texta6(1)
     bus5 = texta6(2)
     nfrfld = 1
     call ffree (d11)
     iv = int (d11)
     nfrfld = 0
2185 if (toLower (bus6) .ne. text1) go to 3674
     n2 = ntotac - n2
     if (noutpr .ne. 0) go to 7841
     if (toLower (bus3) .ne. text16 ) write (unit = kunit6, fmt = 3671)  n2
3671 format ('  End of', i3, '  TACS interface variables.')
     if (bus5 .eq. text16) write (unit = kunit6, fmt = 6743)  n2
6743 format ('+  end of', i3, '  TACS interface variables.  Parallel')
7841 ismdat( i30+14 ) = n2
     go to 205
3674 if (n3 .ne. 71) go to 5431
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 3670) bus6
3670 format ('+  TACS controlled excitation.  ', '"', a6, "'", ' . ')
     do m = 1, ktab
        ndx1 = ilntab(klntab + m)
        if (bus6 .eq. texvec(ndx1)) go to 3672
     end do
     n3671 = 3676
     go to 5448
3672 ismdat(i30 + 15) = m
     cycle
5431 if (n3 .ne. 72) go to 5460
     if (noutpr .eq. 0) write (unit = kunit6, fmt = 5436) bus6
5436 format ('+  TACS controlled mechanical torque. ', '"', a6, '"', ' . ')
     n3671 = 5434
     if (i72 .gt. 0) go to 5434
     i72 = 1
     ismdat(i30 + 16) = mfirst
     !     check for possible overflow *************************************
     nfirst = mfirst - numask
     nsmout = 5 * ismold
     d8 = nsmout * om2 + 1.0d0
     nsmout = int (d8)
     if (nfirst .ge. nsmout) go to 5432
     kill = 1
     lstat(19) = 5432
     lstat(16) = 16
     go to 9999
5432 do m = 1, numask
        m1 = mfirst - m
        massex(m1) = 0
     end do
     m1 = mfirst
     mfirst = nfirst
     nfirst = m1
5434 if ((iv .le. 0) .or. (iv .gt. numask)) go to 5448
     m1 = nfirst - iv
     do m = 1, ktab
        ndx1 = ilntab(klntab + m)
        if (bus6 .eq. texvec(ndx1)) go to 5455
     end do
     n3671 = 5442
5448 kill = 190
     lstat(19) = n3671
     lstat(14) = n3
     lstat(15) = iv
     go to 9999
5455 massex(m1) = m
     cycle
5460 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3675) n3, bus6, iv
3675 format ('+  Auxiliary s.m. input to TACS.', i3, " '",  a6,  "'", i3)
     lstat(17) = 1
     lstat(19) = 5461
     if (iv .gt. 0) go to 5461
     go  to  202
5461 if (n3 .ne. 74 .and. n3 .ne. 73) go to 202
     !     load address of bus6 into tacs array 'ud1' **********************
     ndy5 = kud1
     do ip = niunrs, niu
        ndx1 = ilntab(kaliu + ip)
        m1 = iuty(kiuty + ip)
        if (m1 .ne. 92) go to 5481
        if (bus6 .ne. texvec(ndx1)) go to 5481
        go to 5482
5481    ndy5 = ndy5 + 5
     end do
     lstat(17) = -1
     lstat(19) = 5481
     go to 202
5482 ud1(ndy5 + 2) = ntotac + 1
     if (n3 .eq. 74) go to 5463
     lstat(19) = 5462
     if (iv .gt. n19) go to 202
     if (ismdat(i30 + 13) .ne. 0) go  to  5463
     if (iv .eq. 15) go to 202
     iv = -iv
     go  to  5464
5463 lstat(19) = 5463
     if (iv .gt. n18) go to 202
5464 ntotac = ntotac + 1
     lstat(19) = 5464
     if (ntotac .gt. lbstac) go to 202
     ismtac(ntotac) = iv
     if (iprsup  .ge.  2 ) write (unit = lunit(6), fmt = 3678)    ntotac, (ismtac(n6), n6 = 1, ntotac)
3678 format (/, ' ntotac =', i4, ' .   (ismtac(j), j = 1, ntotac) follows.', /, (12(1x, i5)))
     cycle
202  kill = 108
     lstat(13) = iabs(iv)
     lstat(14) = n3
     lstat(15) = lbstac
     lstat(16) = ntotac
     go to 9999
  end do
205 if (bus5 .ne. text16) go to 99
  ism = 0
  fm = -2.0d0
  go to 1
99 if (iprsup .le. 0) go to 4793
  write (unit = lunit(6), fmt = 4703) numsm, kconst, rmva, rkv, aglin
4703 format (//, " Dump of tables at end of  'smdat' .   numsm  kconst",  11x,  'rmva', 12x, 'rkv', 10x, 'aglin', /, 36x, 2i8, 3e15.7)
  do k = 1, nst
     k1 = 30 * k
     k2 = 101 * k
     write (unit = lunit(6), fmt = 4707) k, ismdat(k1 - 29), ismdat(k1 - 28), ismdat(k1 - 18), ismdat(k1 - 17), ismdat(k1 - 16), elp(k2 - 75), elp(k2 - 100), elp(k2 - 92), elp(k2 - 98), elp(k2 - 96), elp(k2 - 97)
4707 format (/, ' row  imdual  iconfg   numas    kmac    kexc     cnp', 12x, 'eld', 12x, 'elq', 12x, 'elf', 10x, 'elfkd', 10x, 'elakd', /, (1x, i3, 5i8, f8.0, 5e15.6))
     write (unit = lunit(6), fmt = 4712) k, elp( k2-90 ), elp( k2-88 ), elp( k2-84 ), elp( k2-91 ), elp( k2-89 ), elp( k2-87 ), elp( k2-95 ), elp( k2-99 )
4712 format (/, ' row', 12x, 'elg', 10x, 'elgkq', 12x, 'el0', 11x, 'elag', 10x, 'elakq', 11x, 'elkq', 11x, 'elkd', 11x, 'elaf', /, (1x, i3, 8e15.6))
     write (unit = lunit(6), fmt = 4717)  k, elp( k2-81 ), elp( k2-83 ), elp( k2-94 ), elp( k2-93 ), elp( k2-86 ), elp( k2-85 ), elp( k2-72 ), elp( k2-71 )
4717 format (/, ' row', 13x, 'ra', 13x, 'r0', 13x, 'rf', 12x, 'rkd', 13x, 'rg', 12x, 'rkq', 9x, 'smoutp', 9x, 'smoutq', /, (1x, i3, 8e15.6))
     n4 = k2 - 76
     write (unit = lunit(6), fmt = 4738) k, ismdat(k1 - 21), elp(n4 - 3), elp(n4 - 1), elp(n4 - 2), elp(n4), elp(n4 - 4), elp(n4 - 6)
4738 format (/, ' row', 11x, 'isat', 10x, 'sat10', 9x, 'qsat10', 10x, 'sat12', 9x, 'qsat12', 11x, 'rat1', 9x, 'agline', /, (1x, i3, 10x, i2, 3x, 6e15.6))
  end do
  n2 = 0
  do k = 1, nst
     num2 = ismdat(30 * k - 18) * 2
     n3 = n2 + num2 * 4
     n2 = n2 + num2 + 1
     write (unit = lunit(6), fmt = 4723) (shp(k2), k2 = n2, n3)
4723 format (/, " 'shfdat' storage cells follow ............", /, (1x, 8e16.7))
     n2 = n3 + num2 * 2
  end do
4793 return
100 continue
  ! read per unit machine data  as normally defined in transient
  ! stability programs,i.e., obtained with an assymmetric matrix
  if (kolbeg .gt. 0) go to 7313
  read (unit = abuff, fmt = 105) elf, elaf, elfkd, xd, elakd, elkd
  go to 7314
7313 call ffree (elf)
  call ffree (elaf)
  call ffree (elfkd)
  call ffree (xd)
  call ffree (elakd)
  call ffree (elkd)
7314 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3628) elf, elaf, elfkd
  ! read input card using cimage.
  call cimage
  if (kolbeg .gt. 0) go to 7327
  read (unit = abuff, fmt = 105) elg, elag, elgkq, xq, elakq, elkq
105 format (6e10.6, 19x, i1)
  go to 7328
7327 call ffree (elg)
  call ffree (elag)
  call ffree (elgkq)
  call ffree (xq)
  call ffree (elakq)
  call ffree (elkq)
7328 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3637) elg, elag, elgkq
  ! read input card using cimage.
  call cimage
  if (kolbeg .gt. 0) go to 7235
  read (unit = abuff, fmt = 110) el2, ra, rf, rkd, rg, rkq, r1, el1
110 format (8e10.6)
  go to 7236
7235 call ffree (el2)
  call ffree (ra)
  call ffree (rf)
  call ffree (rkd)
  call ffree (rg)
  call ffree (rkq)
  call ffree (r1)
  call ffree (el1)
7236 if (noutpr .eq. 0) write (unit = kunit6, fmt = 3687)  el2, ra, rf
3687 format ('+  7th s.m. card.', 3f10.4)
  elp(i26) = xd * zb
  elp(i26 + 8) = xq * zb
  elaf = elaf * zb
  xl = (xd - elaf) * zb
  ra = ra * zb
  zb3 = zb
  ! calculate the transformer ratio  *********************************
  a = elaf / thtw
  rat = emf / a
  rat2 = 2.0 * rat ** 2 / 3.0
  rat1 = emf / elaf
  elp(i26 + 1) = emf
  zb1 = zb * rat2
  elp(i26+2) = elf * zb1
  elp(i26+4) = elfkd * zb1
  elp(i26+5) = elkd * zb1
  elp(i26+6) = rf * zb1
  elp(i26+7) = rkd * zb1
  elp(i26+10) = elg * zb1
  elp(i26+12) = elgkq * zb1
  elp(i26+13) = elkq * zb1
  elp(i26+14) = rg * zb1
  elp(i26+15) = rkq * zb1
  zb = zb * rat / thtw
  elp(i26+3) = elakd * zb
  elp(i26+9) = elag * zb
  elp(i26+11) = elakq * zb
  zb = zb3
  go to 70
9999 if (iprsup .ge. 1)  write (unit = lunit(6), fmt = 4568)
4568 format ('  "Exit  module smdat."')
  return
end subroutine smdat

!
! subroutine smpfit.
!

subroutine smpfit (x, ism, fm, lunit6, noutpr)
  use tracom
  implicit none
  !     This module applies only to  s.m.  modeling (both 50, 59).
  !     This module applies only to s.m.  modelling ( both 50 and 59 )
  integer(4), intent(in) :: ism, lunit6, noutpr
  real(8), intent(in) :: fm
  real(8), intent(out) :: x(6)
  real(8) :: a, b, c, d
  real(8) :: d1
  real(8) :: f1, f2, f3, f4
  real(8) :: h2, h3
  real(8) :: u, u2
  !
  !     Unimproved parameters ( resistances )    *   *   *   *   *   *   *
  d1 = x(1)
  h3 = x(6)
  h2 = x(2)
  if (d1 .eq. h2) h2 = h2 * fm
  a = x(3) - h3
  c = d1 - h3
  u2 = c * c
  if (d1 .eq. h2) go to 5
  b = u2 / (d1 - h2) - c
  f1 = b + c
  f3 = f1 / x(5)
  d = b * c
  a = -a / ((a * f1) / d - 1.0d0)
  f2 = a + c
  f4 = (a + d / f1) / x(4)
  if (ism .eq. 0) go to 13
  ! improve the rotor resistances    *   *   *   *   *   *   *   *   *
  u = (x(4) + x(5)) * .5d0
  d = u * u - (x(4) * x(5)) / (1.0d0 - u2 / (f1 * f2))
  if (d .lt. 0.) go to 12
  d = u - sqrtz (d)
  f4 = f2 / d
  f3 = f1 / (2.0d0 * u - d)
  go  to  13
12 if (noutpr .ne. 0) go to 13
  write (unit = lunit6, fmt = 26)
26 format (5x, ' Note  ----  The last-read data card belongs to a dynamic synchronous machine, for which the parameters', /, 18x, 'are to be mathematically improved within the module "smpfit" of overlay number 5.   The user', /, 18x, 'requested this procedure by means of the  "parameter fitting"  card which accompanied the')
  write (unit = lunit6, fmt = 4267)
4267 format (18x, 'data cards for this machine.   The attempted iteration has failed to converge.')
  write (unit = lunit6, fmt = 27)
27 format (18x, 'The EMTP logic will now recover and simply ignore the request to improve machine parameters.', /, 18x, 'Input data will be used without any alterations.')
  go to 13
  !     parameters of the reduced order model    *   *   *   *   *   *   *
5 f1 = 1.0
  f2 = u2 / (d1 - x(3))
  f3 = 0.
  f4 = f2 / x(4)
13 x(2) = c
  x(3) = f1
  x(4) = f2
  x(5) = f3
  x(6) = f4
  return
end subroutine smpfit

!
! end of file over5.f90
!
