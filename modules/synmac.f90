!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
!     file: synmac.f90
!

!     real variables precede integer ones          *********************
!     auxilliary arrays ( size based on no. of windings = 7 )  ********

module synmac
  implicit none
  integer(4) :: itold, ibrold, mfirst, msmout, nsmout, nst
  real(8) :: athtw, asqrt3, bdam, bin, damrat, delta6, factom, om2, omdt
  real(8) :: radeg, smoutv, sqrt3, sqrt32, thtw, x1, z
  common /smach/ z(100), x1(36)
  !     spy interface variables *****************************************
  common /smach/ smoutv(15)
  !     computational constants  *****************************************
  common /smach/ sqrt3, asqrt3, sqrt32, thtw, athtw, radeg, omdt
  common /smach/ factom, damrat, delta6, om2, bin, bdam
  !     integer variables      *******************************************
  common /smach/ mfirst, nst, itold, ibrold, nsmout, msmout
  ! equivalences friend zone.
  ! from main10
  integer(4) :: ktemp(1)
  equivalence (ktemp(1), z(1))
end module synmac

!
! end of file synmac.f90
!
