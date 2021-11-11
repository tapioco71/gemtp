!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file syncom.f90
!

! This deck contains s.m. storage used by tacs modules.

module smtacs
  implicit none
  integer(4) :: ismtac(20), lbstac, ntotac, jtemp(1)
  real(8), dimension(20) :: etac
  !  common /smtacs/ etac
  !  common /smtacs/ ismtac(20), ntotac, lbstac

  ! Equivalences.

  equivalence (jtemp(1), etac(1))
end module smtacs

!
! end of file syncom.f90
!
