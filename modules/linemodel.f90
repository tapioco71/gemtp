!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file linemodel.f90
!

module linemodel
  character(6) :: chlmfs(18) ! 9-phase as limit for lmfs test
  character(80) :: char80
  integer(4) :: kexact, nsolve, numrun, nphlmt
  real(8) :: fminsv
end module linemodel

!
! end of file linemodel.f90
!
