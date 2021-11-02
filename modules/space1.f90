!-*- mode: f90; syntax: free-format; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file space1.f90
!

module space1
  implicit none
  real(8) :: a1p(1), a2p(3), ap(1)
  real(8) :: tim1(1), tim2(1), tp(1)
  !  common /spac01/   tp(1)
  equivalence (tp(1), tim1(1), tim2(1), ap(1), a1p(1), a2p(1))
end module space1

!
! end of file space1.f90
!
