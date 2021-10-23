!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file syncom.f90
!

! This deck contains s.m. storage used by tacs modules.

module syncom
  implicit none
  integer(4) :: ismtac, lbstac, ntotac
  real(8), dimension(20) :: etac
  common /smtacs/ etac
  common /smtacs/ ismtac(20), ntotac, lbstac
  ! equivalences friend zone.
  ! from main10
  integer(4) :: jtemp(1)
  equivalence (jtemp(1), etac(1))
end module syncom

!
! end of file syncom.f90
!
