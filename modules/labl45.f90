!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file labl45.f90
!

module labl45
  implicit none
  character(8) :: pl
  integer(4) :: ictrl, i1, iss, iwork, ix
  integer(4) :: kreqab
  integer(4) :: n22, nfr, nfr1, nph, nph2, nphpi2, nphsq, ntri
  real(8) :: cold, conv5, d, dplu, dmin, f, ffin, pi2, ratio, shifti, shiftr
  real(8) :: sll, spdlt, tt, tstrt, tretrd, tstep, w, x, xpan
  common /com45/ pl(91)
  common /com45/ f, w, cold, xpan, conv5, ratio, pi2, sll, spdlt
  common /com45/ tt, tstrt, tretrd, tstep, ffin, shiftr, shifti
  common /com45/ d, x(3), dplu, dmin, ictrl, i1
  common /com45/ iwork, nph, nph2, nphpi2, n22, nphsq, ntri
  common /com45/ iss, nfr, nfr1, ix, kreqab
end module labl45

!
! end of file labl45.f90
!
