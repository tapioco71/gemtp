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
  integer(4) :: karray( 9942869)
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
  real(8) :: p(  111628)
  real(8) :: z(  111628)
  integer(4) :: ic(     472)
  real(8) :: r(     472)
  real(8) :: d(     472)
  real(8) :: gmd(     472)
  real(8) :: x(     472)
  real(8) :: y(     472)
  real(8) :: tb2(     472)
  integer(4) :: itb3(     472)
  real(8) :: workr1(     472)
  real(8) :: workr2(     472)
  character(8) :: text(     944)
  real(8) :: gd(   28015)
  real(8) :: bd(   28015)
  real(8) :: yd(   28015)
  integer(4) :: itbic(     473)
  real(8) :: tbr(     473)
  real(8) :: tbd(     473)
  real(8) :: tbg(     473)
  real(8) :: tbx(     473)
  real(8) :: tby(     473)
  integer(4) :: tbtb2(     473)
  integer(4) :: itbtb3(     473)
  character(8) :: tbtext(     473)
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

subroutine dimens (lsize, nchain, bus1, bus2)
  use indcom
  implicit none
  integer(4), intent(out) :: lsize(80)
  integer(4), intent(in) :: nchain
  integer(4) n7
  character(8), intent(out) :: bus1, bus2
  if (nchain .ge. 29) go to 2900
  lsize( 1)  =   93002
  lsize( 2)  =    3000
  lsize( 3)  =   10000
  lsize( 4)  =     100
  lsize( 5)  =   20000
  lsize( 6)  =    1200
  lsize( 7)  =    4000
  lsize( 8)  =  120000
  lsize( 9)  =     300
  lsize(10)  =     900
  lsize(11)  =     210
  lsize(12)  =     900
  lsize(13)  =       5
  lsize(14)  =     460
  lsize(15)  =      90
  lsize(16)  =      84
  lsize(17)  =       4
  lsize(18)  =     254
  lsize(19)  =   90000
  lsize(20)  =  100000
  lsize(21)  =    3000
  lsize(22)  =   50000
  lsize(23)  =   30000
  lsize(24)  =      24
  lsize(25)  =   30000
  lsize(26)  =    3000
  lsize(27)  =     600
  lsize(28)  =    1080
  n7 = 28 + 1
  lsize(n7) = 9942869
  bus1 ='  121230'
  bus2 ='  121121'
  return
2900 if (nchain .gt.  29) go to 3100
  lsize( 1)  =       0
  lsize( 2) =9942869
  lsize( 3)  =      23
  lsize( 4) =  30000
  lsize( 5)  =       1
  lsize( 6) =  93002
  lsize( 7)  =       9
  lsize( 8) =    300
  return
3100 if (nchain .gt.  31) go to 3900
  lsize( 1)  =       9
  lsize( 2) =    300
  return
3900 if (nchain .gt.  39) go to 1000
  lsize( 1)  =      71
  lsize( 2) =  10000
  return
1000 if (nchain .gt.  10) go to 4400
  lsize( 1)  =       1
  lsize( 2) =  93002
  lsize( 3)  =      23
  lsize( 4) =  30000
  lsize( 5)  =       4
  lsize( 6) =    100
  return
4400 if (nchain .gt.  44) go to 4500
  lsize( 1)  =       9
  lsize( 2) =    300
  lsize( 3)  =      75
  lsize( 4) = 111628
  lsize( 5)  =      71
  lsize( 6) =    472
  lsize( 7)  =      76
  lsize( 8) =    944
  lsize( 9)  =      74
  lsize(10) =  28015
  lsize(11)  =      73
  lsize(12) =    473
  return
4500 if (nchain .gt.  45) go to 4700
  lsize( 1)  =       9
  lsize( 2) =    300
  return
4700 if (nchain .gt.  47) go to 9900
  lsize( 1)  =       9
  lsize( 2) =    300
  return
 9900 lsize(1) = location (bus1) - location (bus2)
  return
end subroutine dimens

!
! end of file newmods.f90
!
