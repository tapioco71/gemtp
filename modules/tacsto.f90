! -*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file tacsto.f90
!

module cmrsto
  !  common /cmrsto/ split, ten, zero, one, two, half, p, stpt, stpr1, stpr2, stpr3, a, b, c, d, e, f, g, r0, r1, r2, r3, r4, r5, r6, rinf, rmargn, divzro, pi, rnull, bratio, etime, estep, estop, rspare(6), rsto
  implicit none
  real(8) :: split, ten, zero, one, two, half, p, stpt, stpr1, stpr2, stpr3, a
  real(8) :: b, c, d, e, f, g, r0, r1, r2, r3, r4, r5, r6, rinf, rmargn, divzro
  real(8) :: pi, rnull, bratio, etime, estep, estop, rspare(6), rsto
end module cmrsto

module cmcsto
  !  common /cmcsto/ csto
  implicit none
  real(8) :: csto
end module cmcsto

module tacsto
  implicit none
  integer(4) :: isto, strtbl
  integer(4) :: from, to
  integer(4) :: ilen, rlen, clen, iptr, rptr, cptr, sptr, rsptr, dptr
  integer(4) :: ibkptr, rbkptr, ishenv, env, useenv, datenv
  integer(4) :: xprndx, xprcnt, xprknd, xprsub
  integer(4) :: mflg, mndx
  integer(4) :: stpflg, stpi1, stpi2, stpi3, stpi4, stpi5
  integer(4) :: stpl1, stpc1, stpl2, stpc2
  integer(4) :: ptr0,ptr1,ptr2,ptr3,ptr4,ptr5,ptr6,ptr7,ptr8,ptr9,ptr10
  integer(4) :: ptr11,ptr12,ptr13,ptr14,ptr15,ptr16,ptr17,ptr18,ptr19
  integer(4) :: cnt0,cnt1,cnt2,cnt3,cnt4,cnt5,cnt6,cnt7,cnt8,cnt9,cnt10
  integer(4) :: cnt11,cnt12,cnt13,cnt14,cnt15,cnt16,cnt17,cnt18,cnt19
  integer(4) :: ndx0,ndx1,ndx2,ndx3,ndx4,ndx5,ndx6,ndx7,ndx8,ndx9,ndx10
  integer(4) :: ndx11,ndx12,ndx13,ndx14,ndx15,ndx16,ndx17,ndx18,ndx19
  integer(4) :: cnt, ndx, lpflg, flg, flg1, flg2, ipol, ipn
  integer(4) :: i, j, k, l, m, n
  integer(4) :: i0,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15
  integer(4) :: i16,i17,i18,i19
  integer(4) :: j0,j1,j2,j3,j4,j5,j6,j7,j8,j9
  integer(4) :: k0,k1,k2,k3,k4,k5,k6,k7,k8,k9
  integer(4) :: base1,base2,base3,base4,base5,base6,base7
  integer(4) :: base13,base14,base15,base16
  integer(4) :: iinf, inull
  integer(4) :: strcnt, ishcnt, intcnt, reacnt, iempty, rempty, ctblen
  integer(4) :: unit05, unit06, unit08, unit09, unit10
  integer(4) :: unit11, unit12, unit13, unit14, unit15
  integer(4) :: stoflt, stocha, fltlen, intlen, bkfile
  integer(4) :: pgsize, isize, rsize, csize, rbase
  real(8) :: rspare, rsto
  real(8) :: etime, estep, estop, bratio, split
  real(8) :: ten, zero, one, two, half
  real(8) :: p, stpt, stpr1, stpr2, stpr3
  real(8) :: b, c, d, e, f, g
  real(8) :: r0, r1, r2, r3, r4, r5, r6
  real(8) :: rinf, rmargn, divzro, pi, rnull
  double precision :: a
  character(1) :: csto, char1
  character(1232) :: ctbl
  character(256) :: cbuff
  dimension isto(18000)
  dimension rsto(9000)
  dimension csto(9000)       ! wsm + thl manual modification for bpa emtp
  !     stoflt and stocha are hard-dimensioned in data.ftn
  dimension strtbl( 514 )
  ! wsm + thl manual modification for bpa emtp
  !  common /cmrsto/ split, ten, zero, one, two, half, p, stpt, stpr1, stpr2, stpr3, a, b, c, d, e, f, g, r0, r1, r2, r3, r4, r5, r6, rinf, rmargn, divzro, pi, rnull, bratio, etime, estep, estop, rspare(6), rsto
  ! wsm + thl manual modification for bpa emtp
  !  common /cmcsto/ csto
  ! common isto, strtbl, from, to, ilen, rlen, clen, iptr, rptr, cptr
  ! common sptr, rsptr, dptr, ibkptr, rbkptr, ishenv, env, useenv
  ! common datenv, xprndx, xprcnt, xprknd, xprsub, mflg, mndx, stpflg
  ! common stpi1, stpi2, stpi3, stpi4, stpi5, stpl1, stpc1, stpl2
  ! common stpc2, ptr0, ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7
  ! common ptr8, ptr9, ptr10, ptr11, ptr12, ptr13, ptr14, ptr15
  ! common ptr16, ptr17, ptr18, ptr19, cnt0, cnt1, cnt2, cnt3, cnt4
  ! common cnt5, cnt6, cnt7, cnt8, cnt9, cnt10, cnt11, cnt12, cnt13
  ! common cnt14, cnt15, cnt16, cnt17, cnt18, cnt19, ndx0, ndx1, ndx2
  ! common ndx3, ndx4, ndx5, ndx6, ndx7, ndx8, ndx9, ndx10, ndx11
  ! common ndx12, ndx13, ndx14, ndx15, ndx16, ndx17, ndx18, ndx19
  ! common cnt, ndx, lpflg, flg, flg1, flg2, ipol, ipn
  ! common i, j, k, l, m, n, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9
  ! common i10, i11, i12, i13, i14, i15, i16, i17, i18, i19
  ! common j0, j1, j2, j3, j4, j5, j6, j7, j8, j9
  ! common k0, k1, k2, k3, k4, k5, k6, k7, k8, k9
  ! common base1, base2, base3, base4, base5, base6, base7
  ! common base13, base14, base15, base16
  ! common iinf, inull
  ! common strcnt, ishcnt, intcnt, reacnt, iempty, rempty, ctblen
  ! common unit05, unit06, unit08, unit09, unit10
  ! common unit11, unit12, unit13, unit14, unit15
  ! common stoflt, stocha, fltlen, intlen, bkfile
  ! common pgsize, isize, rsize, csize, rbase
  ! common char1
  ! common ctbl, cbuff
end module tacsto

!
! end of file tacsto.f90
!
