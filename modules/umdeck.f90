!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file umdeck.f90
!

module umcom
  implicit none
  character(8) :: busum(50), date(2)
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
  !  common /umcom/ busum(50), ptheta(3, 3), zthevr(3, 3)
  !  common /umcom/ vinp(40), zthevs(40), umcur(40), con(10)
  !  common /umcom/ dumvec(40), dummat(3, 3), date(2), clock(2)
  !  common /umcom/ pi, sroot2, sroot3, omegrf
  !  common /umcom/ inpu, numbus, ncltot, ndum(40), initum
  !     next come offsets for variable-dimensioning, in order.
  ! common /umcom/ iureac, iugpar, iufpar, iuhist, iuumrp
  ! common /umcom/ iunod1, iunod2, iujclt, iujclo
  ! common /umcom/ iujtyp, iunodo, iujtmt, iuhism
  ! common /umcom/ iuomgm, iuomld, iutham, iuredu
  ! common /umcom/ iureds, iuflds, iufldr, iurequ
  ! common /umcom/ iuflqs, iuflqr, iujcds, iujcqs
  ! common /umcom/ iuflxd, iuflxq, iunppa, iurotm
  ! common /umcom/ iuncld, iunclq, iujtqo, iujomo
  ! common /umcom/ iujtho, iureqs, iuepso, iudcoe
  ! common /umcom/ iukcoi, iuvolt, iuangl, iunodf
  ! common /umcom/ iunodm, iukumo, iujumo, iuumou
  !     done with offsets for subroutine calls.
  !  common /umcom/ nclfix, numfix, iotfix, ibsfix, ksubum
  !  common /umcom/ nsmach, istart
  ! equivalences friend zone
  ! from main00
  integer(4) :: itemp(8 * 50)
  equivalence (itemp(1), busum(1))
end module umcom

!
! end of file umdeck.f90
!
