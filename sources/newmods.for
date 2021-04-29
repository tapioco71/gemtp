!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: newmods.for
!
!
!     subroutine main10.
!
!
! subroutine main10.
!
subroutine main10
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  common /c0b001/   x     (   10000 )
  common /c0b002/   ykm   (   20000 )
  common /c0b003/   km    (   20000 )
  common /c0b004/   xk    (  121080 )
  common /c0b005/   xm    (  121080 )
  common /c0b006/   weight(     460 )
  common /c0b007/   iwtent(      31 )
  common /c0b008/   con1  (      30 )
  common /c0b009/   iskip (       5 )
  common /c0b010/   zinf  (       5 )
  common /c0b011/   eta   (       5 )
  common /c0b012/   nhist (       5 )
  common /c0b013/   stailm(      90 )
  common /c0b014/   stailk(      90 )
  common /c0b015/   xmax  (    3600 )
  common /c0b016/   koutvp(     508 )
  common /c0b017/   bnrg  (     254 )
  common /c0b018/   sconst(  100000 )
  common /c0b019/   cnvhst(   53000 )
  common /c0b020/   sfd   (    6000 )
  common /c0b021/   qfd   (    6000 )
  common /c0b022/   semaux(   50000 )
  common /c0b023/   ibsout(     900 )
  common /c0b024/   bvalue(     900 )
  common /c0b025/   sptacs(   90000 )
  common /c0b026/   kswtyp(    1200 )
  common /c0b027/   modswt(    1200 )
  common /c0b028/   kbegsw(    1200 )
  common /c0b029/   lastsw(    1200 )
  common /c0b030/   kentnb(    1200 )
  common /c0b031/   nbhdsw(    3600 )
  common /c0b032/   topen (    3600 )
  common /c0b033/   crit  (    1200 )
  common /c0b034/   kdepsw(    3600 )
  common /c0b035/   tdns  (    1200 )
  common /c0b036/   isourc(    1200 )
  common /c0b037/   energy(    1200 )
  common /c0b038/   iardub(    3600 )
  common /c0b039/   ardube(    4800 )
  common /c0b040/   nonlad(     300 )
  common /c0b041/   nonle (     300 )
  common /c0b042/   vnonl (     300 )
  common /c0b043/   curr  (     300 )
  common /c0b044/   anonl (     300 )
  common /c0b045/   vecnl1(     300 )
  common /c0b046/   vecnl2(     300 )
  common /c0b047/   namenl(     300 )
  common /c0b048/   vzero (     300 )
  common /c0b049/   ilast (     300 )
  common /c0b050/   nltype(     300 )
  common /c0b051/   kupl  (     300 )
  common /c0b052/   nlsub (     300 )
  common /c0b053/   xoptbr(    3000 )
  common /c0b054/   coptbr(    3000 )
  common /c0b055/   cursub(     312 )
  common /c0b056/   cchar (     900 )
  common /c0b057/   vchar (     900 )
  common /c0b058/   gslope(     900 )
  common /c0b059/   ktrans(   93002 )
  common /c0b060/   kk    (   93002 )
  common /c0b061/   emtpc (   10000 )
  common /c0b062/   tr    (   20000 )
  common /c0b063/   tx    (   20000 )
  common /c0b064/   r     (   10000 )
  common /c0b065/   nr    (    3000 )
  common /c0b066/   length(    3000 )
  common /c0b067/   cik   (    3000 )
  common /c0b068/   ci    (    3000 )
  common /c0b069/   ck    (    3000 )
  common /c0b070/   ismout(    1052 )
  common /c0b071/   elp   (     404 )
  common /c0b072/   cu    (      96 )
  common /c0b073/   shp   (    1008 )
  common /c0b074/   histq (     504 )
  common /c0b075/   ismdat(     120 )
  common /c0b076/   texvec(    4000 )
  character(8)      texvec
  common /c0b077/   ibrnch(     900 )
  common /c0b078/   jbrnch(     900 )
  common /c0b079/   tstop (     100 )
  common /c0b080/   nonlk (     300 )
  common /c0b081/   nonlm (     300 )
  common /c0b082/   spum  (   30000 )
  common /c0b083/   kks   (   93002 )
  common /c0b084/   kknonl( 2232048 )
  common /c0b085/   znonl ( 2232048 )
  common /c0b086/   znonlb(   93002 )
  common /c0b087/   znonlc(   93002 )
  common /c0b088/   finit (   93002 )
  common /c0b089/   ksub  (     312 )
  common /c0b090/   msub  (     312 )
  common /c0b091/   isubeg(     304 )
  common /c0b092/   litype(    3000 )
  common /c0b093/   imodel(    3000 )
  common /c0b094/   kbus  (    3000 )
  common /c0b095/   mbus  (    3000 )
  common /c0b096/   kodebr(    3000 )
  common /c0b097/   cki   (    3000 )
  common /c0b098/   ckkjm (    3000 )
  common /c0b099/   indhst(    3000 )
  common /c0b100/   kodsem(    3000 )
  common /c0b101/   namebr(   18000 )
  common /c0b102/   iform (     100 )
  common /c0b103/   node  (     100 )
  common /c0b104/   crest (     100 )
  common /c0b105/   time1 (     100 )
  common /c0b106/   time2 (     100 )
  common /c0b107/   tstart(     100 )
  common /c0b108/   sfreq (     100 )
  common /c0b109/   kmswit(    3600 )
  common /c0b110/   nextsw(    1200 )
  common /c0b111/   rmfd  (       1 )
  common /c0b112/   cikfd (       1 )
  common /c0b113/   imfd  (     600 )
  common /c0b114/   tclose(    1200 )
  common /c0b115/   adelay(    3600 )
  common /c0b116/   kpos  (    1200 )
  common /c0b117/   namesw(    1200 )
  common /c0b118/   emtpe (   93002 )
  common /c0b119/   emtpf (   93002 )
  common /c0b120/   kssfrq(   93002 )
  common /c0b121/   kode  (   93002 )
  common /c0b122/   kpsour(   93002 )
  common /c0b123/   volti (    6000 )
  common /c0b124/   voltk (    3000 )
  common /c0b125/   volt  (    6000 )
  common /c0b126/   bus   (   93002 )
  character(8)         bus
  call subr10
  return
end subroutine main10
!
! subroutine over29.
!
subroutine over29
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  common /c29b01/   karray( 9748865 )
  common /spac01/   tp    (   30000 )
  common /spac02/   norder(   93002 )
  common /spac03/   index (   93002 )
  common /spac04/   diag  (   93002 )
  common /spac05/   diab  (   93002 )
  common /spac06/   solr  (   93002 )
  common /spac07/   soli  (   93002 )
  common /spac08/   ich1  (   93002 )
  common /spac09/   bnd   (     300 )
  common /spac10/   iloc  (   30000 )
  common /spac11/   gnd   (   30000 )
  call subr29
  return
end subroutine over29
!
! subroutine over31.
!
subroutine over31
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  common /c31b01/   karray(     300 )
  call subr31
  return
end subroutine over31
!
! subroutine over39.
!
subroutine over39
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  common /c39b01/   xdat  (   10000 )
  common /c39b02/   ydat  (   10000 )
  common /c39b03/   aphdat(   10000 )
  call subr39
  return
end subroutine over39
!
! subroutine fixs10.
!
subroutine fixs10
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  common /c10b01/   jndex (   93002 )
  common /c10b02/   diagg (   93002 )
  common /c10b03/   diabb (   93002 )
  common /c10b04/   solrsv(   93002 )
  common /c10b05/   solisv(   93002 )
  common /c10b06/   gndd  (   30000 )
  common /c10b07/   bndd  (   30000 )
  common /c10b08/   nekfix(     100 )
  common /c10b09/   fxtem1(     100 )
  common /c10b10/   fxtem2(     100 )
  common /c10b11/   fxtem3(     100 )
  common /c10b12/   fxtem4(     100 )
  common /c10b13/   fxtem5(     100 )
  common /c10b14/   fxtem6(     100 )
  common /c10b15/   fixbu1(     100 )
  common /c10b16/   fixbu2(     100 )
  common /c10b17/   fixbu3(     100 )
  common /c10b18/   fixbu4(     100 )
  common /c10b19/   fixbu5(     100 )
  common /c10b20/   fixbu6(     100 )
  common /c10b21/   fixbu7(     100 )
  common /c10b22/   fixbu8(     100 )
  common /c10b23/   fixbu9(     100 )
  common /c10b24/   fixb10(     100 )
  common /c10b25/   fixb11(     100 )
  common /c10b26/   kndex (     100 )
  call subr10
  return
end subroutine fixs10
!
! subroutine over44.
!
subroutine over44
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  common /c44b01/   karray(     300 )
  common /c44b02/   p     (  108811 )
  common /c44b03/   z     (  108811 )
  common /c44b04/   ic    (     466 )
  common /c44b05/   r     (     466 )
  common /c44b06/   emptd (     466 )
  common /c44b07/   gmd   (     466 )
  common /c44b08/   x     (     466 )
  common /c44b09/   y     (     466 )
  common /c44b10/   tb2   (     466 )
  common /c44b11/   itb3  (     466 )
  common /c44b12/   workr1(     466 )
  common /c44b13/   workr2(     466 )
  common /c44b14/   text  (     932 )
  character(8)        text
  common /c44b15/   gd    (   27261 )
  common /c44b16/   bd    (   27261 )
  common /c44b17/   yd    (   27261 )
  common /c44b18/   itbic (     467 )
  common /c44b19/   tbr   (     467 )
  common /c44b20/   tbd   (     467 )
  common /c44b21/   tbg   (     467 )
  common /c44b22/   tbx   (     467 )
  common /c44b23/   tby   (     467 )
  common /c44b24/   tbtb2 (     467 )
  common /c44b25/   itbtb3(     467 )
  common /c44b26/   tbtext(     467 )
  character(8)      tbtext
  call subr44
  return
end subroutine over44
!
! subroutine over45.
!
subroutine over45
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  common /c45b01/   karray(     300 )
  call subr45
  return
end subroutine over45
!
! subroutine over47.
!
subroutine over47
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  common /c47b01/   karray(     300 )
  call subr47
  return
end subroutine over47
!
! subroutine dimens.
!
subroutine dimens(lsize, nchain, bus1, bus2)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  dimension lsize(62)
  character(8) bus1, bus2
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
  lsize(n7) = 9748865
  bus1 ='  102928'
      bus2 ='  290421'
  return
2900 if (nchain .gt.  29) go to 3100
  lsize( 1)  =       0
  lsize( 2) =9748865
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
  lsize( 4) = 108811
  lsize( 5)  =      71
  lsize( 6) =    466
  lsize( 7)  =      76
  lsize( 8) =    932
  lsize( 9)  =      74
  lsize(10) =  27261
  lsize(11)  =      73
  lsize(12) =    467
  return
4500 if (nchain .gt.  45) go to 4700
  lsize( 1)  =       9
  lsize( 2) =    300
  return
4700 if (nchain .gt.  47) go to 9900
  lsize( 1)  =       9
  lsize( 2) =    300
  return
 9900 lsize(1) = locint(bus1) - locint(bus2)
  return
end subroutine dimens
!
! end of file newmods.for
!
