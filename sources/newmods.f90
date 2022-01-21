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
  integer(4) :: norder(   43002)
  integer(4) :: index(   43002)
  real(8) :: diag(   43002)
  real(8) :: diab(   43002)
  real(8) :: solr(   43002)
  real(8) :: soli(   43002)
  integer(4) :: ich1(   43002)
  real(8) :: bnd(     300)
  integer(4) :: iloc(   30000)
  real(8) :: gnd(   30000)
  real(8) :: xdat(    6000)
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
  ls( 1)  =   43002
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
  ls(n7) = 5485871
  b1 ='  142141'
  b2 ='  210122'
  return
2900 if (n .gt.  29) go to 3100
  ls( 1)  =       0
  ls( 2) =5485871
  ls( 3)  =       1
  ls( 4) =  43002
  ls( 5)  =       9
  ls( 6) =    300
  ls( 7)  =      23
  ls( 8) =  30000
  ls( 9)  =      71
  ls(10) =   6000
  return
3100 if (n .gt.  31) go to 3900
  ls( 1)  =      71
  ls( 2) =   6000
  return
3900 if (n .gt.  39) go to 1000
  ls( 1)  =       1
  ls( 2) =  43002
  ls( 3)  =      71
  ls( 4) =  10000
  return
1000 if (n .gt.  10) go to 4400
  ls( 1)  =       1
  ls( 2) =  43002
  ls( 3)  =      23
  ls( 4) =  30000
  ls( 5)  =       4
  ls( 6) =    100
  return
4400 if (n .gt.  44) go to 4500
  ls( 1)  =      75
  ls( 2) =  61425
  ls( 3)  =      71
  ls( 4) =    350
  ls( 5)  =      76
  ls( 6) =    700
  ls( 7)  =      74
  ls( 8) =  15465
  ls( 9)  =      73
  ls(10) =    351
  return
4500 if (n .gt.  45) go to 4700
  return
4700 if (n .gt.  47) go to 9900
  return
 9900 ls(1) = location (b1) - location (b2)
  return
end subroutine dimens

!
! end of file newmods.f90
!
