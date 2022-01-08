!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file newmods.f90
!

!
! subroutine main10.
!

#ifdef WITH_MAIN10
subroutine main10
  use labcom
  implicit none
  !
  call subr10
  return
end subroutine main10
#endif

!
! subroutine over29.
!

#ifdef WITH_OVER29
subroutine over29
  implicit none
  integer(4) :: karray( 9762865)
  real(8) :: tp(   30000)
  integer(4) :: norder(   93002)
  integer(4) :: index(   93002)
  real(8) :: diag(   93002)
  real(8) :: diab(   93002)
  real(8) :: solr(   93002)
  real(8) :: soli(   93002)
  integer(4) :: ich1(   93002)
  real(8) :: bnd(     300)
  integer(4) :: iloc(   30000)
  real(8) :: gnd(   30000)
  call subr29
  return
end subroutine over29
#endif

!
! subroutine over31.
!

#ifdef WITH_OVER31
subroutine over31
  implicit none
  integer(4) :: karray(     300)
  call subr31
  return
end subroutine over31
#endif

!
! subroutine over39.
!

#ifdef WITH_OVER39
subroutine over39
  implicit none
  real(8) :: xdat(   10000)
  real(8) :: ydat(   10000)
  real(8) :: aphdat(   10000)
  call subr39
  return
end subroutine over39
#endif

!
! subroutine fixs10.
!

#ifdef WITH_FIXS10
subroutine fixs10
  implicit none
  integer(4) :: jndex(   93002)
  real(8) :: diagg(   93002)
  real(8) :: diabb(   93002)
  real(8) :: solrsv(   93002)
  real(8) :: solisv(   93002)
  real(8) :: gndd(   30000)
  real(8) :: bndd(   30000)
  integer(4) :: nekfix(     100)
  real(8) :: fxtem1(     100)
  real(8) :: fxtem2(     100)
  real(8) :: fxtem3(     100)
  real(8) :: fxtem4(     100)
  real(8) :: fxtem5(     100)
  real(8) :: fxtem6(     100)
  real(8) :: fixbu1(     100)
  real(8) :: fixbu2(     100)
  real(8) :: fixbu3(     100)
  real(8) :: fixbu4(     100)
  real(8) :: fixbu5(     100)
  real(8) :: fixbu6(     100)
  real(8) :: fixbu7(     100)
  real(8) :: fixbu8(     100)
  real(8) :: fixbu9(     100)
  real(8) :: fixb10(     100)
  real(8) :: fixb11(     100)
  integer(4) :: kndex(     100)
  call subr10
  return
end subroutine fixs10
#endif

!
! subroutine over44.
!

#ifdef WITH_OVER44
subroutine over44
  implicit none
  integer(4) :: karray(     300)
  real(8) :: p(  109746)
  real(8) :: z(  109746)
  integer(4) :: ic(     468)
  real(8) :: r(     468)
  real(8) :: d(     468)
  real(8) :: gmd(     468)
  real(8) :: x(     468)
  real(8) :: y(     468)
  real(8) :: tb2(     468)
  integer(4) :: itb3(     468)
  real(8) :: workr1(     468)
  real(8) :: workr2(     468)
  character(8) :: text(     936)
  real(8) :: gd(   27508)
  real(8) :: bd(   27508)
  real(8) :: yd(   27508)
  integer(4) :: itbic(     469)
  real(8) :: tbr(     469)
  real(8) :: tbd(     469)
  real(8) :: tbg(     469)
  real(8) :: tbx(     469)
  real(8) :: tby(     469)
  integer(4) :: tbtb2(     469)
  integer(4) :: itbtb3(     469)
  character(8) :: tbtext(     469)
  call subr44
  return
end subroutine over44
#endif

!
! subroutine over45.
!

#ifdef WITH_OVER45
subroutine over45
  implicit none
  integer(4) :: karray(     300)
  call subr45
  return
end subroutine over45
#endif

!
! subroutine over47.
!

#ifdef WITH_OVER47
subroutine over47
  implicit none
  integer(4) :: karray(     300)
  call subr47
  return
end subroutine over47
#endif

!
! subroutine dimens.
!

subroutine dimens (ls, n, b1, b2)
  use indcom
  implicit none
  integer(4), intent(out) :: ls(80)
  integer(4), intent(in) :: n
  integer(4) n7
  character(8), intent(out) :: b1, b2
  if (n .ge. 29) go to 2900
  ls( 1)  =   93002
  ls( 2)  =    3000
  ls( 3)  =   10000
  ls( 4)  =     100
  ls( 5)  =   20000
  ls( 6)  =    1200
  ls( 7)  =    4000
  ls( 8)  =  120000
  ls( 9)  =     300
  ls(10)  =     900
  ls(11)  =     210
  ls(12)  =     900
  ls(13)  =       5
  ls(14)  =     460
  ls(15)  =      90
  ls(16)  =      84
  ls(17)  =       4
  ls(18)  =     254
  ls(19)  =   90000
  ls(20)  =  100000
  ls(21)  =    3000
  ls(22)  =   50000
  ls(23)  =   30000
  ls(24)  =      24
  ls(25)  =   30000
  ls(26)  =    3000
  ls(27)  =     600
  ls(28)  =    1080
  n7 = 28 + 1
  ls(n7) = 9762865
  b1 ='  140834'
  b2 ='   80122'
  return
2900 if (n .gt.  29) go to 3100
  ls( 1)  =       0
  ls( 2) =9762865
  ls( 3)  =      23
  ls( 4) =  30000
  ls( 5)  =       1
  ls( 6) =  93002
  ls( 7)  =       9
  ls( 8) =    300
  return
3100 if (n .gt.  31) go to 3900
  ls( 1)  =       9
  ls( 2) =    300
  return
3900 if (n .gt.  39) go to 1000
  ls( 1)  =      71
  ls( 2) =  10000
  return
1000 if (n .gt.  10) go to 4400
  ls( 1)  =       1
  ls( 2) =  93002
  ls( 3)  =      23
  ls( 4) =  30000
  ls( 5)  =       4
  ls( 6) =    100
  return
4400 if (n .gt.  44) go to 4500
  ls( 1)  =       9
  ls( 2) =    300
  ls( 3)  =      75
  ls( 4) = 109746
  ls( 5)  =      71
  ls( 6) =    468
  ls( 7)  =      76
  ls( 8) =    936
  ls( 9)  =      74
  ls(10) =  27508
  ls(11)  =      73
  ls(12) =    469
  return
4500 if (n .gt.  45) go to 4700
  ls( 1)  =       9
  ls( 2) =    300
  return
4700 if (n .gt.  47) go to 9900
  ls( 1)  =       9
  ls( 2) =    300
  return
 9900 ls(1) = location (b1) - location (b2)
  return
end subroutine dimens

!
! end of file newmods.f90
!
