!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file labl47.f90
!

module com47
  implicit none
  !  common /com47/  bin(20), bkn(20), cimag1, creal1, czero, ypo
  !     end of complex variables
  !  common /com47/  alf1, alf2, dep1, dep2, e0, e2p, radp(3)
  !  common /com47/  alpi, bp1, bp2, es1, es2, rop, usp
  !  common /com47/  hyud2, hyud3, hyud4, htoj2, htoj3, fzero
  !  common /com47/  htoj4,pai,roe,spdlgt,u0,u2p
  !  common /com47/  value1, value2, value3, value4, value5,valu14
  !     end of real variables
  !  common /com47/  iearth, itypec, ncct, ncc, npc, izflag, iyflag, npc2
  !  common /com47/  np2, logsix, kmode, iprs47, npais, ncros, numaki, npp
  !  common /com47/  iprint
  !     end of integer variables
  integer(4) :: iearth, iprint, iprs47, itypec, iyflag, izflag
  integer(4) :: kmode
  integer(4) :: logsix
  integer(4) :: ncc, ncct, ncros, np2, npais, npc, npc2, npp, numaki
  real(8) :: alf1, alf2, alpi
  real(8) :: bp1, bp2
  real(8) :: dep1, dep2
  real(8) :: e0, e2p, es1, es2
  real(8) :: fzero
  real(8) :: htoj2, htoj3, htoj4, hyud2, hyud3, hyud4
  real(8) :: pai
  real(8) :: radp(3), roe, rop
  real(8) :: spdlgt
  real(8) :: u0, u2p, usp
  real(8) :: value1, value2, value3, value4, value5, valu14
  complex(16) :: bin(20), bkn(20), cimag1, creal1, czero, ypo
end module com47

!
! end of file labl47.f90
!
