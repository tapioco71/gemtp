!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: ntacs1.for
!

!
!     subroutine ntacs1.
!

subroutine ntacs1
!  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'tacsto.ftn'
  include 'blkcom.ftn'                                      ! wsm + thl manual modification for bpa emtp
  include 'tacsar.ftn'                                      ! wsm + thl manual modification for bpa emtp
  include 'labcom.ftn'                                      ! wsm + thl manual modification for bpa emtp
  !equivalence (moncar(71), ltacst), (moncar(83), ntcsex)    ! wsm + thl manual modification for bpa emtp
  equivalence (lstat (67), maxniu)
  equivalence (lstat (68), maxnuk)
  ntcsex = 1
  do i = 1, 23
     sptacs(i) = 0.0
  end do
!  lstat(48)=0    ! wsm + thl manual modification for bpa emtp
!  moncar(32)=0       ! wsm + thl manual modification for bpa emtp
!  lstat(35)=0        ! wsm + thl manual modification for bpa emtp
  maxnuk = (ltacst - 30) / 11
  maxniu = maxnuk
  write (*, *) ' in ntacs1, use:  ltacst =', ltacst
  do i = 61, 66
     lstat(i) = 0
  end do
  kud1 = 30
  kiuty = kud1 + 5 * maxniu
  kxtcs = kiuty + maxniu
  klntab = kxtcs + maxnuk + maxniu
  kjout = klntab + maxnuk + maxniu
!  stoflt=moncar(27)   ! wsm + thl manual modification for bpa emtp
!  stocha=moncar(29)   ! wsm + thl manual modification for bpa emtp
  call ntacs1a              ! wsm + thl manual modification for bpa emtp
  call ntacs1b              ! wsm + thl manual modification for bpa emtp
  if(ktab .lt. 1) ktab = 1
!  moncar(24)=isize    ! wsm + thl manual modification for bpa emtp
!  moncar(27)=rsize    ! wsm + thl manual modification for bpa emtp
!  moncar(29)=csize    ! wsm + thl manual modification for bpa emtp
!  lstat(35)=cptr      ! wsm + thl manual modification for bpa emtp
!  k=isto(ishenv+7)    ! wsm + thl manual modification for bpa emtp
!  moncar(32)=iptr+isto(k)*isto(k+1)       ! wsm + thl manual modification for bpa emtp
!  k=isto(ishenv+8)                        ! wsm + thl manual modification for bpa emtp
!  lstat(48)=rptr-rbase+isto(k)*isto(k+1)  ! wsm + thl manual modification for bpa emtp
  call cimage
  return
end subroutine ntacs1

!
!     end of file: ntacs1.for
!
