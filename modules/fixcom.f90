!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file fixcom.f90
!

module fixcom
  implicit none
  integer(4) :: nekcod(15), nekreq, nitera
  real(8) :: ekbuf(15, 9), ektemp(45, 5), errchk
  real(8) :: solrsv(2500), solisv(2500)
  common /ekcom1/ ekbuf, ektemp, errchk
  common /ekcom1/ solrsv, solisv, nitera
  common /ekcom1/ nekreq, nekcod
end module fixcom

!
! end of file fixcom.f90
!
