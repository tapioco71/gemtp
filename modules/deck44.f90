!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file deck44.f90
!

module deck44
  use deck29
  implicit none
  ! common /c29b01/   karray(3)
  ! common /c44b02/   p     (9)
  ! common /c44b03/   z     (9)
  ! common /c44b04/   ic    (1)
  ! common /c44b05/   r     (1)
  ! common /c44b06/   d     (1)
  ! common /c44b07/   gmd   (1)
  ! common /c44b08/   x     (1)
  ! common /c44b09/   y     (1)
  ! common /c44b10/   tb2   (1)
  ! common /c44b11/   itb3  (1)
  ! common /c44b12/   workr1(1)
  ! common /c44b13/   workr2(1)
  ! common /c44b14/   text  (1)
  ! character(8) text
  ! common /c44b15/   gd    (9)
  ! common /c44b16/   bd    (9)
  ! common /c44b17/   yd    (1)
  ! common /c44b18/   itbic (1)
  ! common /c44b19/   tbr   (1)
  ! common /c44b20/   tbd   (1)
  ! common /c44b21/   tbg   (1)
  ! common /c44b22/   tbx   (1)
  ! common /c44b23/   tby   (1)
  ! common /c44b24/   tbtb2 (1)
  ! common /c44b25/   itbtb3(1)
  ! common /c44b26/   tbtext(1)
  character(8) tbtext(1)
  !  integer(4), target :: karray(3)
  integer(4) :: ic(1), itb3(1), itbic(1), itbtb3(1)
  real(8) :: p(9)
  real(8) :: z(9)
  real(8) :: r(1)
  real(8) :: d(1)
  real(8) :: gmd(1)
  real(8) :: x(1)
  real(8) :: y(1)
  real(8) :: tb2(1)
  real(8) :: workr1(1)
  real(8) :: workr2(1)
  character(8) text(1)
  real(8) :: gd(9)
  real(8) :: bd(9)
  real(8) :: yd(1)
  real(8) :: tbr(1)
  real(8) :: tbd(1)
  real(8) :: tbg(1)
  real(8) :: tbx(1)
  real(8) :: tby(1)
  real(8) :: tbtb2(1)
end module deck44

!
! end of file deck44.f90
!
