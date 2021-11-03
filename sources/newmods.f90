!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file newmods.f90
!

!
! subroutine main10.
!

#ifdef WITH_MAIN10
subroutine main10
  implicit none
  real(8) :: x(   10000)
  real(8) :: ykm(   20000)
  integer(4) :: km(   20000)
  real(8) :: xk(  121080)
  real(8) :: xm(  121080)
  real(8) :: weight(     460)
  integer(4) :: iwtent(      31)
  real(8) :: con1(      30)
  integer(4) :: iskip(       5)
  real(8) :: zinf(       5)
  real(8) :: eta(       5)
  integer(4) :: nhist(       5)
  real(8) :: stailm(      90)
  real(8) :: stailk(      90)
  real(8) :: xmax(    3600)
  integer(4) :: koutvp(     508)
  real(8) :: bnrg(     254)
  real(8) :: sconst(  100000)
  real(8) :: cnvhst(   53000)
  real(8) :: sfd(    6000)
  real(8) :: qfd(    6000)
  real(8) :: semaux(   50000)
  integer(4) :: ibsout(     900)
  real(8) :: bvalue(     900)
  real(8) :: sptacs(   90000)
  integer(4) :: kswtyp(    1200)
  integer(4) :: modswt(    1200)
  integer(4) :: kbegsw(    1200)
  integer(4) :: lastsw(    1200)
  integer(4) :: kentnb(    1200)
  integer(4) :: nbhdsw(    3600)
  real(8) :: topen(    3600)
  real(8) :: crit(    1200)
  integer(4) :: kdepsw(    3600)
  real(8) :: tdns(    1200)
  integer(4) :: isourc(    1200)
  real(8) :: energy(    1200)
  integer(4) :: iardub(    3600)
  real(8) :: ardube(    4800)
  integer(4) :: nonlad(     300)
  integer(4) :: nonle(     300)
  real(8) :: vnonl(     300)
  real(8) :: curr(     300)
  real(8) :: anonl(     300)
  real(8) :: vecnl1(     300)
  real(8) :: vecnl2(     300)
  integer(4) :: namenl(     300)
  real(8) :: vzero(     300)
  integer(4) :: ilast(     300)
  integer(4) :: nltype(     300)
  integer(4) :: kupl(     300)
  integer(4) :: nlsub(     300)
  real(8) :: xoptbr(    3000)
  real(8) :: coptbr(    3000)
  real(8) :: cursub(     312)
  real(8) :: cchar(     900)
  real(8) :: vchar(     900)
  real(8) :: gslope(     900)
  integer(4) :: ktrans(   93002)
  integer(4) :: kk(   93002)
  real(8) :: emtpc(   10000)
  real(8) :: tr(   20000)
  real(8) :: tx(   20000)
  real(8) :: r(   10000)
  integer(4) :: nr(    3000)
  integer(4) :: length(    3000)
  real(8) :: cik(    3000)
  real(8) :: ci(    3000)
  real(8) :: ck(    3000)
  integer(4) :: ismout(    1052)
  real(8) :: elp(     404)
  real(8) :: cu(      96)
  real(8) :: shp(    1008)
  real(8) :: histq(     504)
  integer(4) :: ismdat(     120)
  character(8) :: texvec(    4000)
  integer(4) :: ibrnch(     900)
  integer(4) :: jbrnch(     900)
  real(8) :: tstop(     100)
  integer(4) :: nonlk(     300)
  integer(4) :: nonlm(     300)
  real(8) :: spum(   30000)
  integer(4) :: kks(   93002)
  integer(4) :: kknonl( 2232048)
  real(8) :: znonl( 2232048)
  real(8) :: znonlb(   93002)
  real(8) :: znonlc(   93002)
  real(8) :: finit(   93002)
  integer(4) :: ksub(     312)
  integer(4) :: msub(     312)
  integer(4) :: isubeg(     304)
  integer(4) :: litype(    3000)
  integer(4) :: imodel(    3000)
  integer(4) :: kbus(    3000)
  integer(4) :: mbus(    3000)
  integer(4) :: kodebr(    3000)
  real(8) :: cki(    3000)
  real(8) :: ckkjm(    3000)
  integer(4) :: indhst(    3000)
  integer(4) :: kodsem(    3000)
  integer(4) :: namebr(   18000)
  integer(4) :: iform(     100)
  integer(4) :: node(     100)
  real(8) :: crest(     100)
  real(8) :: time1(     100)
  real(8) :: time2(     100)
  real(8) :: tstart(     100)
  real(8) :: sfreq(     100)
  integer(4) :: kmswit(    3600)
  integer(4) :: nextsw(    1200)
  real(8) :: rmfd(       1)
  real(8) :: cikfd(       1)
  integer(4) :: imfd(     600)
  real(8) :: tclose(    1200)
  real(8) :: adelay(    3600)
  integer(4) :: kpos(    1200)
  integer(4) :: namesw(    1200)
  real(8), target :: emtpe(   93002)
  real(8) :: emtpf(   93002)
  integer(4) :: kssfrq(   93002)
  integer(4) :: kode(   93002)
  integer(4) :: kpsour(   93002)
  real(8) :: volti(    6000)
  real(8) :: voltk(    3000)
  real(8) :: volt(    6000)
  character(8) :: bus(   93002)
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
  real(8) :: emptd(     472)
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
  bus1 ='  110318'
  bus2 ='   31121'
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
