!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file umdeck.f90
!

module umdeck
  implicit none
  integer(4) :: ibsfix, initum, inpu, iotfix, istart, iuangl, iudcoe, iuepso !, iufldr
  integer(4) :: iuflds, iufldr, iuflqs, iuflxd, iuflxq, iufpar, iugpar
  integer(4) :: iuhism, iuhist, iujcds, iujclo, iujclt, iujcqs, iujomo, iujtho
  integer(4) :: iujtmt, iuflqr, iujtqo, iujtyp, iujumo, iukcoi, iukumo, iuncld
  integer(4) :: iunclq, iunod1, iunod2, iunodf, iunodm, iunodo, iunppa, iuomgm
  integer(4) :: iuomld, iureac, iureds, iuredu, iureqs, iurequ, iurotm, iutham
  integer(4) :: iuumou, iuumrp, iuvolt
  integer(4) :: ksubum
  integer(4) :: nclfix, ncltot, ndum, nsmach, numbus, numfix !, niu, nsmachm
  real(8) :: clock, con
  real(8) :: dummat, dumvec
  real(8) :: omegrf
  real(8) :: pi, ptheta
  real(8) :: sroot2, sroot3
  real(8) :: umcur
  real(8) :: vinp
  real(8) :: zthevr, zthevs
  character(8) :: busum, date
  common /umcom/ busum(50), ptheta(3, 3), zthevr(3, 3)
  common /umcom/ vinp(40), zthevs(40), umcur(40), con(10)
  common /umcom/ dumvec(40), dummat(3, 3), date(2), clock(2)
  common /umcom/ pi, sroot2, sroot3, omegrf
  common /umcom/ inpu, numbus, ncltot, ndum(40), initum
  !     next come offsets for variable-dimensioning, in order.
  common /umcom/ iureac, iugpar, iufpar, iuhist, iuumrp
  common /umcom/ iunod1, iunod2, iujclt, iujclo
  common /umcom/ iujtyp, iunodo, iujtmt, iuhism
  common /umcom/ iuomgm, iuomld, iutham, iuredu
  common /umcom/ iureds, iuflds, iufldr, iurequ
  common /umcom/ iuflqs, iuflqr, iujcds, iujcqs
  common /umcom/ iuflxd, iuflxq, iunppa, iurotm
  common /umcom/ iuncld, iunclq, iujtqo, iujomo
  common /umcom/ iujtho, iureqs, iuepso, iudcoe
  common /umcom/ iukcoi, iuvolt, iuangl, iunodf
  common /umcom/ iunodm, iukumo, iujumo, iuumou
  !     done with offsets for subroutine calls.
  common /umcom/ nclfix, numfix, iotfix, ibsfix, ksubum
  common /umcom/ nsmach, istart
  ! equivalences friend zone
  ! from main00
  integer(4) :: itemp(8 * 50)
  equivalence (itemp, busum)
end module umdeck

!
! end of file umdeck.f90
!
