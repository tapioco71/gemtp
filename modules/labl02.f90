!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file labl02.f90
!

module labl02
  implicit none
  integer ibr1, ida, idm, idq, idt, idu, idy, ifkc, ifq, iq, kgroup
  integer l27dep, lcount
  integer model
  integer n1, n2, n3, n4, nc3, nc4, nc5, nc6, n13, nrecur
  common /com2/ n1, n2, n3, n4, lcount, model, l27dep, ibr1
  common /com2/ nrecur,  kgroup, nc4, nc5, ifq, n13, ida
  common /com2/ ifkc, idy, idm, idq, idu, idt, iq, nc6, nc3
end module labl02

!
! end of file labl02.f90
!
