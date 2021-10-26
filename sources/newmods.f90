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
  common /c0b001/   x     (   10000 )
  real(8) :: x
  common /c0b002/   ykm   (   20000 )
  real(8) :: ykm
  common /c0b003/   km    (   20000 )
  integer(4) :: km
  common /c0b004/   xk    (  121080 )
  real(8) :: xk
  common /c0b005/   xm    (  121080 )
  real(8) :: xm
  common /c0b006/   weight(     460 )
  real(8) :: weight
  common /c0b007/   iwtent(      31 )
  integer(4) :: iwtent
  common /c0b008/   con1  (      30 )
  real(8) :: con1
  common /c0b009/   iskip (       5 )
  integer(4) :: iskip
  common /c0b010/   zinf  (       5 )
  real(8) :: zinf
  common /c0b011/   eta   (       5 )
  real(8) :: eta
  common /c0b012/   nhist (       5 )
  integer(4) :: nhist
  common /c0b013/   stailm(      90 )
  real(8) :: stailm
  common /c0b014/   stailk(      90 )
  real(8) :: stailk
  common /c0b015/   xmax  (    3600 )
  real(8) :: xmax
  common /c0b016/   koutvp(     508 )
  integer(4) :: koutvp
  common /c0b017/   bnrg  (     254 )
  real(8) :: bnrg
  common /c0b018/   sconst(  100000 )
  real(8) :: sconst
  common /c0b019/   cnvhst(   53000 )
  real(8) :: cnvhst
  common /c0b020/   sfd   (    6000 )
  real(8) :: sfd
  common /c0b021/   qfd   (    6000 )
  real(8) :: qfd
  common /c0b022/   semaux(   50000 )
  real(8) :: semaux
  common /c0b023/   ibsout(     900 )
  integer(4) :: ibsout
  common /c0b024/   bvalue(     900 )
  real(8) :: bvalue
  common /c0b025/   sptacs(   90000 )
  real(8) :: sptacs
  common /c0b026/   kswtyp(    1200 )
  integer(4) :: kswtyp
  common /c0b027/   modswt(    1200 )
  integer(4) :: modswt
  common /c0b028/   kbegsw(    1200 )
  integer(4) :: kbegsw
  common /c0b029/   lastsw(    1200 )
  integer(4) :: lastsw
  common /c0b030/   kentnb(    1200 )
  integer(4) :: kentnb
  common /c0b031/   nbhdsw(    3600 )
  integer(4) :: nbhdsw
  common /c0b032/   topen (    3600 )
  real(8) :: topen
  common /c0b033/   crit  (    1200 )
  real(8) :: crit
  common /c0b034/   kdepsw(    3600 )
  integer(4) :: kdepsw
  common /c0b035/   tdns  (    1200 )
  real(8) :: tdns
  common /c0b036/   isourc(    1200 )
  integer(4) :: isourc
  common /c0b037/   energy(    1200 )
  real(8) :: energy
  common /c0b038/   iardub(    3600 )
  integer(4) :: iardub
  common /c0b039/   ardube(    4800 )
  real(8) :: ardube
  common /c0b040/   nonlad(     300 )
  integer(4) :: nonlad
  common /c0b041/   nonle (     300 )
  integer(4) :: nonle
  common /c0b042/   vnonl (     300 )
  real(8) :: vnonl
  common /c0b043/   curr  (     300 )
  real(8) :: curr
  common /c0b044/   anonl (     300 )
  real(8) :: anonl
  common /c0b045/   vecnl1(     300 )
  real(8) :: vecnl1
  common /c0b046/   vecnl2(     300 )
  real(8) :: vecnl2
  common /c0b047/   namenl(     300 )
  integer(4) :: namenl
  common /c0b048/   vzero (     300 )
  real(8) :: vzero
  common /c0b049/   ilast (     300 )
  integer(4) :: ilast
  common /c0b050/   nltype(     300 )
  integer(4) :: nltype
  common /c0b051/   kupl  (     300 )
  integer(4) :: kupl
  common /c0b052/   nlsub (     300 )
  integer(4) :: nlsub
  common /c0b053/   xoptbr(    3000 )
  real(8) :: xoptbr
  common /c0b054/   coptbr(    3000 )
  real(8) :: coptbr
  common /c0b055/   cursub(     312 )
  real(8) :: cursub
  common /c0b056/   cchar (     900 )
  integer(4) :: cchar
  common /c0b057/   vchar (     900 )
  real(8) :: vchar
  common /c0b058/   gslope(     900 )
  real(8) :: gslope
  common /c0b059/   ktrans(   93002 )
  integer(4) :: ktrans
  common /c0b060/   kk    (   93002 )
  integer(4) :: kk
  common /c0b061/   emtpc (   10000 )
  real(8) :: emtpc
  common /c0b062/   tr    (   20000 )
  real(8) :: tr
  common /c0b063/   tx    (   20000 )
  real(8) :: tx
  common /c0b064/   r     (   10000 )
  real(8) :: r
  common /c0b065/   nr    (    3000 )
  integer(4) :: nr
  common /c0b066/   length(    3000 )
  integer(4) :: length
  common /c0b067/   cik   (    3000 )
  real(8) :: cik
  common /c0b068/   ci    (    3000 )
  real(8) :: ci
  common /c0b069/   ck    (    3000 )
  real(8) :: ck
  common /c0b070/   ismout(    1052 )
  integer(4) :: ismout
  common /c0b071/   elp   (     404 )
  real(8) :: elp
  common /c0b072/   cu    (      96 )
  real(8) :: cu
  common /c0b073/   shp   (    1008 )
  real(8) :: shp
  common /c0b074/   histq (     504 )
  real(8) :: histq
  common /c0b075/   ismdat(     120 )
  integer(4) :: ismdat
  common /c0b076/   texvec(    4000 )
  character(8) :: texvec
  common /c0b077/   ibrnch(     900 )
  integer(4) :: ibrnch
  common /c0b078/   jbrnch(     900 )
  integer(4) :: jbrnch
  common /c0b079/   tstop (     100 )
  real(8) :: tstop
  common /c0b080/   nonlk (     300 )
  integer(4) :: nonlk
  common /c0b081/   nonlm (     300 )
  integer(4) :: nonlm
  common /c0b082/   spum  (   30000 )
  real(8) :: spum
  common /c0b083/   kks   (   93002 )
  integer(4) :: kks
  common /c0b084/   kknonl( 2232048 )
  integer(4) :: kknonl
  common /c0b085/   znonl ( 2232048 )
  real(8) :: znonl
  common /c0b086/   znonlb(   93002 )
  real(8) :: znonlb
  common /c0b087/   znonlc(   93002 )
  real(8) :: znonlc
  common /c0b088/   finit (   93002 )
  real(8) :: finit
  common /c0b089/   ksub  (     312 )
  integer(4) :: ksub
  common /c0b090/   msub  (     312 )
  integer(4) :: msub
  common /c0b091/   isubeg(     304 )
  integer(4) :: isubeg
  common /c0b092/   litype(    3000 )
  integer(4) :: litype
  common /c0b093/   imodel(    3000 )
  integer(4) :: imodel
  common /c0b094/   kbus  (    3000 )
  integer(4) :: kbus
  common /c0b095/   mbus  (    3000 )
  integer(4) :: mbus
  common /c0b096/   kodebr(    3000 )
  integer(4) :: kodebr
  common /c0b097/   cki   (    3000 )
  real(8) :: cki
  common /c0b098/   ckkjm (    3000 )
  real(8) :: ckkjm
  common /c0b099/   indhst(    3000 )
  integer(4) :: indhst
  common /c0b100/   kodsem(    3000 )
  integer(4) :: kodsem
  common /c0b101/   namebr(   18000 )
  integer(4) :: namebr
  common /c0b102/   iform (     100 )
  integer(4) :: iform
  common /c0b103/   node  (     100 )
  integer(4) :: node
  common /c0b104/   crest (     100 )
  real(8) :: crest
  common /c0b105/   time1 (     100 )
  real(8) :: time1
  common /c0b106/   time2 (     100 )
  real(8) :: time2
  common /c0b107/   tstart(     100 )
  real(8) :: tstart
  common /c0b108/   sfreq (     100 )
  real(8) :: sfreq
  common /c0b109/   kmswit(    3600 )
  integer(4) :: kmswit
  common /c0b110/   nextsw(    1200 )
  integer(4) :: nextsw
  common /c0b111/   rmfd  (       1 )
  real(8) :: rmfd
  common /c0b112/   cikfd (       1 )
  real(8) :: cikfd
  common /c0b113/   imfd  (     600 )
  integer(4) :: imfd
  common /c0b114/   tclose(    1200 )
  real(8) :: tclose
  common /c0b115/   adelay(    3600 )
  real(8) :: adelay
  common /c0b116/   kpos  (    1200 )
  integer(4) :: kpos
  common /c0b117/   namesw(    1200 )
  integer(4) :: namesw
  common /c0b118/   emtpe (   93002 )
  real(8) :: emtpe
  common /c0b119/   emtpf (   93002 )
  real(8) :: emtpf
  common /c0b120/   kssfrq(   93002 )
  integer(4) :: kssfrq
  common /c0b121/   kode  (   93002 )
  integer(4) :: kode
  common /c0b122/   kpsour(   93002 )
  integer(4) :: kpsour
  common /c0b123/   volti (    6000 )
  real(8) :: volti
  common /c0b124/   voltk (    3000 )
  real(8) :: voltk
  common /c0b125/   volt  (    6000 )
  real(8) :: volt
  common /c0b126/   bus   (   93002 )
  character(8) :: bus
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
  common /c29b01/   karray( 9844967 )
  integer(4) :: karray
  common /spac01/   tp    (   30000 )
  real(8) :: tp
  common /spac02/   norder(   93002 )
  integer(4) :: norder
  common /spac03/   index (   93002 )
  integer(4) :: index
  common /spac04/   diag  (   93002 )
  real(8) :: diag
  common /spac05/   diab  (   93002 )
  real(8) :: diab
  common /spac06/   solr  (   93002 )
  real(8) :: solr
  common /spac07/   soli  (   93002 )
  real(8) :: soli
  common /spac08/   ich1  (   93002 )
  integer(4) :: ich1
  common /spac09/   bnd   (     300 )
  real(8) :: bnd
  common /spac10/   iloc  (   30000 )
  integer(4) :: iloc
  common /spac11/   gnd   (   30000 )
  real(8) :: gnd
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
  common /c31b01/   karray(     300 )
  integer(4) :: karray
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
  common /c39b01/   xdat  (   10000 )
  real(8) :: xdat
  common /c39b02/   ydat  (   10000 )
  real(8) :: ydat
  common /c39b03/   aphdat(   10000 )
  real(8) :: aphdat
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
  common /c10b01/   jndex (   93002 )
  integer(4) :: jndex
  common /c10b02/   diagg (   93002 )
  real(8) :: diagg
  common /c10b03/   diabb (   93002 )
  real(8) :: diabb
  common /c10b04/   solrsv(   93002 )
  real(8) :: solrsv
  common /c10b05/   solisv(   93002 )
  real(8) :: solisv
  common /c10b06/   gndd  (   30000 )
  real(8) :: gndd
  common /c10b07/   bndd  (   30000 )
  real(8) :: bndd
  common /c10b08/   nekfix(     100 )
  integer(4) :: nekfix
  common /c10b09/   fxtem1(     100 )
  real(8) :: fxtem1
  common /c10b10/   fxtem2(     100 )
  real(8) :: fxtem2
  common /c10b11/   fxtem3(     100 )
  real(8) :: fxtem3
  common /c10b12/   fxtem4(     100 )
  real(8) :: fxtem4
  common /c10b13/   fxtem5(     100 )
  real(8) :: fxtem5
  common /c10b14/   fxtem6(     100 )
  real(8) :: fxtem6
  common /c10b15/   fixbu1(     100 )
  real(8) :: fixbu1
  common /c10b16/   fixbu2(     100 )
  real(8) :: fixbu2
  common /c10b17/   fixbu3(     100 )
  real(8) :: fixbu3
  common /c10b18/   fixbu4(     100 )
  real(8) :: fixbu4
  common /c10b19/   fixbu5(     100 )
  real(8) :: fixbu5
  common /c10b20/   fixbu6(     100 )
  real(8) :: fixbu6
  common /c10b21/   fixbu7(     100 )
  real(8) :: fixbu7
  common /c10b22/   fixbu8(     100 )
  real(8) :: fixbu8
  common /c10b23/   fixbu9(     100 )
  real(8) :: fixbu9
  common /c10b24/   fixb10(     100 )
  real(8) :: fixb10
  common /c10b25/   fixb11(     100 )
  real(8) :: fixb11
  common /c10b26/   kndex (     100 )
  integer(4) :: kndex
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
  common /c44b01/   karray(     300 )
  integer(4) :: karray
  common /c44b02/   p     (  110685 )
  real(8) :: p
  common /c44b03/   z     (  110685 )
  real(8) :: z
  common /c44b04/   ic    (     470 )
  integer(4) :: ic
  common /c44b05/   r     (     470 )
  real(8) :: r
  common /c44b06/   emptd (     470 )
  real(8) :: emptd
  common /c44b07/   gmd   (     470 )
  real(8) :: gmd
  common /c44b08/   x     (     470 )
  real(8) :: x
  common /c44b09/   y     (     470 )
  real(8) :: y
  common /c44b10/   tb2   (     470 )
  real(8) :: tb2
  common /c44b11/   itb3  (     470 )
  integer(4) :: itb3
  common /c44b12/   workr1(     470 )
  real(8) :: workr1
  common /c44b13/   workr2(     470 )
  real(8) :: workr2
  common /c44b14/   text  (     940 )
  character(8) :: text
  common /c44b15/   gd    (   27739 )
  real(8) :: gd
  common /c44b16/   bd    (   27739 )
  real(8) :: bd
  common /c44b17/   yd    (   27739 )
  real(8) :: yd
  common /c44b18/   itbic (     471 )
  integer(4) :: itbic
  common /c44b19/   tbr   (     471 )
  real(8) :: tbr
  common /c44b20/   tbd   (     471 )
  real(8) :: tbd
  common /c44b21/   tbg   (     471 )
  real(8) :: tbg
  common /c44b22/   tbx   (     471 )
  real(8) :: tbx
  common /c44b23/   tby   (     471 )
  real(8) :: tby
  common /c44b24/   tbtb2 (     471 )
  real(8) :: tbtb2
  common /c44b25/   itbtb3(     471 )
  integer(4) :: itbtb3
  common /c44b26/   tbtext(     471 )
  character(8) :: tbtext
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
  common /c45b01/   karray(     300 )
  integer(4) :: karray
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
  common /c47b01/   karray(     300 )
  integer(4) :: karray
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
  lsize(n7) = 9844967
  bus1 ='  172612'
  bus2 ='  261021'
  return
2900 if (nchain .gt.  29) go to 3100
  lsize( 1)  =       0
  lsize( 2) =9844967
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
  lsize( 4) = 110685
  lsize( 5)  =      71
  lsize( 6) =    470
  lsize( 7)  =      76
  lsize( 8) =    940
  lsize( 9)  =      74
  lsize(10) =  27739
  lsize(11)  =      73
  lsize(12) =    471
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
