!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file umdeck.f90
!

module umcom
  implicit none
  character(8) :: busum(50)
  character(8) :: date(2)
  integer(4) :: ibsfix, initum, inpu, iotfix, istart, iuangl, iudcoe, iuepso !, iufldr
  integer(4) :: iuflds, iufldr, iuflqs, iuflxd, iuflxq, iufpar, iugpar
  integer(4) :: iuhism, iuhist, iujcds, iujclo, iujclt, iujcqs, iujomo, iujtho
  integer(4) :: iujtmt, iuflqr, iujtqo, iujtyp, iujumo, iukcoi, iukumo, iuncld
  integer(4) :: iunclq, iunod1, iunod2, iunodf, iunodm, iunodo, iunppa, iuomgm
  integer(4) :: iuomld, iureac, iureds, iuredu, iureqs, iurequ, iurotm, iutham
  integer(4) :: iuumou, iuumrp, iuvolt
  integer(4) :: ksubum
  integer(4) :: nclfix, ncltot, ndum(40), nsmach, numbus, numfix !, niu, nsmachm
  real(8) :: clock(2), con(10)
  real(8) :: dummat(3, 3), dumvec(40)
  real(8) :: omegrf
  real(8) :: pi, ptheta(3, 3)
  real(8) :: sroot2, sroot3
  real(8) :: umcur(40)
  real(8) :: vinp(40)
  real(8) :: zthevr(3, 3), zthevs(40)
  !     done with offsets for subroutine calls.

  ! Equivalences.

  !  integer(4) :: itemp(8 * 50), ibusum(1)
  !  equivalence (itemp(1), busum(1))
  !  equivalence (busum(1), ibusum(1))
end module umcom

!
! end of file umdeck.f90
!
