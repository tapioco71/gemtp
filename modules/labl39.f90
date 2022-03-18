!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file labl39.f90
!

! Copyright 1977-2021 Bonneville Power Administration
! Copyright 2019-2021 Angelo Rossi <angelo.rossi.homelab@gmail.com>
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice,
!    this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! 3. Neither the name of the copyright holder nor the names of its contributors
!    may be used to endorse or promote products derived from this software
!    without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.

module com39
  implicit none
  !
  !     begin with functions of  mmodes=18  limit on modes:
  integer(4), target :: modskp(2, 18)
  real(8), target :: alinvc(90)
  real(8), target :: tir(18, 18), tii(18, 18), tdum(18, 18)
  !     next come functions of  mpoles=100  limit on poles:
  integer(4), target :: indxv(100)
  real(8), target :: akfrac(100), alphaf(100), fczr(100)
  real(8), target :: fcpr(100), fcz(100), fcp(100)
  real(8), target :: xauxd(200)
  real(8), target :: zoprau(400), zoprao(400), azepo(400)
  !     next  mxchkr=2*sum(2**i), i=0,1,..n  with 2**n .ge. mpoles
  real(8), target :: xchkra(255)
  !     next come functions of  mxknee=100  limit on ups and downs:
  integer(4), target :: noprao(100)
  real(8), target :: xknee(100)
  !     next come scalars, with reals before integers (a la ibm):
  integer(4) :: idebug, iftype, lout, ndata, ntotra
  integer(4) :: nzone, izone, nrange, modify, nexmis
  integer(4) :: normax, ifwta, koutpr, inelim, ifplot
  integer(4) :: ifdat, iecode, nzeror, npoler, modesk
  integer(4) :: metrik
  real(8) :: hreflg, aptdec, gmode, amina1, onehav
  real(8) :: oneqtr, hrflgr, epstol, refa, refb

contains

  !
  ! function yfun39.
  !

  function yfun39 (x) result(retVal)
    use deck39
    implicit none
    real(8), intent(in) :: x
    integer(4) :: i1
    real(8) :: ai
    real(8) :: retVal
    real(8) :: y
    !  include 'labl39.ftn'
    !  include 'deck39.ftn'
    !
    if (x .le. xdat(1)) go to 120
    if (x .ge. xdat(ndata)) go to 100
    ai = aptdec * (x - xdat(1)) + 1.0d0
    i1 = int (ai, kind (i1))
    y = (x - xdat(i1)) * (ydat(i1 + 1) - ydat(i1)) / (xdat(i1 + 1) - xdat(i1))
    retVal = y + ydat(i1)
    go to 110
120 retVal = ydat(1)
    go to 110
100 retVal = ydat(ndata)
110 continue
  end function yfun39

  !
  ! function aph.
  !

  function aph (x) result(retVal)
    use deck39
    implicit none
    real(8), intent(in) :: x
    !
    integer(4) :: i1
    real(8) :: retVal
    real(8) :: ai
    !
    if (x .le. xdat(1)) go to 120
    if (x .ge. xdat(ndata)) go to 100
    ai = aptdec * (x - xdat(1)) + 1.0d0
    i1 = int (ai, kind (i1))
    retVal = (x - xdat(i1)) * (aphdat(i1 + 1) - aphdat(i1)) / (xdat(i1 + 1) - xdat(i1)) + aphdat(i1)
    go to 110
120 retVal = aphdat(1)
    go to 110
100 retVal = aphdat(ndata)
110 return
  end function aph

end module com39

!
! end of file labl39.f90
!
