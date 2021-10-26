!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file labcom.f90
!

module labcom
  use blkcom
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
  integer(4) :: integx( 1)
  integer(4) :: infdli( 1)
  integer(4) :: ispum( 1)
  integer(4) :: kindep( 1)
  integer(4) :: ksing( 1)
  integer(4) :: massex( 1)
  integer(4) :: nsubkm( 1)
  real(8) :: fold( 1)
  real(8) :: volta( 1)
  real(8) :: vsmout( 1)
  real(8) :: wk1( 1)
  real(8) :: xx( 1)
  real(8) :: r4( 1)
  real(8) :: cblhst( 1)
  equivalence (xk( 1), xx( 1))
  equivalence (spum( 1), ispum( 1))
  equivalence (kknonl( 1), nsubkm( 1))
  equivalence (semaux( 1), wk1( 1))
  equivalence (namebr( 1), infdli( 1))
  equivalence (ismout( 1), vsmout( 1))
  equivalence (histq( 1), massex( 1))
  equivalence (volti( 1), volta( 1))
  equivalence (ksing( 1), cchar( 1))
  equivalence (kindep( 1), gslope( 1))
  equivalence (fold( 1), vchar( 1))
  equivalence (r4( 1), volti( 1))
  equivalence (cnvhst( 1), cblhst( 1))
  equivalence (x( 1), integx( 1))
end module labcom

!
! end of file labcom.f90
!

