!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file labl44.f90
!

module com44
  implicit none
  ! common /com44/  bcars(30), ccars(30),  dcars(30), fbe(20)
  ! common /com44/  brname(40)
  ! common /com44/  fbed(20), fke(20), fked(20), pi, picon, sqrt2
  ! common /com44/  valu1, valu2, valu3, valu4, valu5, valu6, valu7
  ! common /com44/  valu8, valu9, valu10, valu11, valu12, valu13
  ! common /com44/  corchk, aaa1, aaa2
  ! common /com44/  ll0, ll1, ll2, ll3, ll5, ll6, ll7, ll8, ll9, ll10
  ! common /com44/  lphase, lphpl1, lphd2, lgdbd, jpralt,nfreq
  character(8) :: brname(40)
  integer(4) :: ll0, ll1, ll2, ll3, ll5, ll6, ll7, ll8, ll9, ll10
  integer(4) :: lphase, lphpl1, lphd2, lgdbd, jpralt,nfreq
  real(8) :: bcars(30), ccars(30), dcars(30), fbe(20)
  real(8) :: fbed(20), fke(20), fked(20), pi, picon, sqrt2
  real(8) :: valu1, valu2, valu3, valu4, valu5, valu6, valu7
  real(8) :: valu8, valu9, valu10, valu11, valu12, valu13
  real(8) :: corchk, aaa1, aaa2
end module com44

!
! end of file labl44.f90
!
