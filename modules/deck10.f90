!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file deck10.f90
!

module deck10
  implicit none

  ! common /c10b01/ jndex (1)
  ! common /c10b02/ diagg (1)
  ! common /c10b03/ diabb (1)
  ! common /c10b04/ solrsv(1)
  ! common /c10b05/ solisv(1)
  ! common /c10b06/ gndd  (1)
  ! common /c10b07/ bndd  (1)
  ! common /c10b08/ nekfix(1)
  ! common /c10b09/ fxtem1(100)
  ! common /c10b10/ fxtem2(100)
  ! common /c10b11/ fxtem3(100)
  ! common /c10b12/ fxtem4(100)
  ! common /c10b13/ fxtem5(100)
  ! common /c10b14/ fxtem6(100)
  ! common /c10b15/ fixbu1(1)
  ! common /c10b16/ fixbu2(1)
  ! common /c10b17/ fixbu3(100)
  ! common /c10b18/ fixbu4(100)
  ! common /c10b19/ fixbu5(100)
  ! common /c10b20/ fixbu6(100)
  ! common /c10b21/ fixbu7(100)
  ! common /c10b22/ fixbu8(100)
  ! common /c10b23/ fixbu9(100)
  ! common /c10b24/ fixb10(100)
  ! common /c10b25/ fixb11(100)
  ! common /c10b26/ kndex (1)

  character(8) :: fixbu1(100), fixbu2(100), fixbu3(100)
  character(10) :: fxtem1(100)
  integer(4) :: jndex(1)
  integer(4) :: kndex (1)
  integer(4) :: nekfix(1)
  real(8) :: diagg(1)
  real(8) :: diabb(1)
  real(8) :: solrsv(1)
  real(8) :: solisv(1)
  real(8) :: gndd(1)
  real(8) :: bndd(1)
  real(8) :: fixbu4(100)
  real(8) :: fixbu5(100)
  real(8) :: fixbu6(100)
  real(8) :: fixbu7(100)
  real(8) :: fixbu8(100)
  real(8) :: fixbu9(100)
  real(8) :: fixb10(100)
  real(8) :: fixb11(100)
  real(8) :: fxtem2(100)
  real(8) :: fxtem3(100)
  real(8) :: fxtem4(100)
  real(8) :: fxtem5(100)
  real(8) :: fxtem6(100)

end module deck10

!
! end of file deck10.f90
!
