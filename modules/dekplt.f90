!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file dekplt.f90
!

!     Usage is restricted to "plot" function of "emtspy", due
!     to variable-length conflict (this is single-precision).
!  character(8) datepl, tclopl, buslst(1), bbus

module dekplt
  use dekspy
  implicit none
  integer(4) :: ibaud, ibrnch, ibsout, ichend, ichref, icp, icurse, ihs, inchpx
  integer(4) :: inchpy, ind1, indexp, inwait, iprsrt, iterm, ivcom, izgr1, izgr2, izid
  integer(4) :: iztit, izxax, izyax
  integer(4) :: jbegbc, jbegbv, jbrnch, jchan, jhmsp, jplt, jplt1
  integer(4) :: kextr, kill, killpl, klevl, kp, kptplt, kstart
!  integer(4) :: l, linpr
  integer(4) :: l4plot, labrtm, lchfil, lchid, lchlim, lchsup, lchtit, lchxax
  integer(4) :: lchyax, ldshg1, ldshg2, limcol, limfix, look, lsymb, ltic
  integer(4) :: lu7plt, linepr, ltek
  integer(4) :: maxev, maxew, maxip, maxisx, maxisy, maxsym, mcurve, mfake, mgrid1
  integer(4) :: mgrid2, mid, mlevel, mline, mmm, mplot, mstart, msuper, msymbt, mtit
  integer(4) :: mu6sav, mu6std, mxypl
  integer(4) :: namvar, nc, nchsup, nchver, ncut, ncut1, ncut2, newfil, nolabl, nsmplt
!  integer(4) :: numbco
  integer(4) :: nt2, numbrn, numflt, numnvo, numout, numpts, numraw, numtek
  integer(4) :: numtit, nv, nxend, nxid6, nxinch, nxmax, nxoff, nxvern, nyend, nyid6
  integer(4) :: nyinch, nymax, nyoff
  real(8) :: aaa, bbb, bx, datepl, dx, dxgrd1, dxgrd2, dy, dygrd1, dygrd2, dyold
  real(8) :: ev, evnbyt, ew
  real(8) :: fact, fhtax, fill, fill1, fill2, finfin, fline, fsymb, ftcarr, fvaxtt
  real(8) :: fvcom, fxid, fxnumh, fxnumv, fxref, fxsup, fxtit, fxvert, fyid, fyref
  real(8) :: fysup, fytit
  real(8) :: gxmax, gxmin
  real(8) :: ha, hmax, hmin, hpi, htax
  real(8) :: paplim, papmax
  real(8) :: sizid, sizsup, siztit
  real(8) :: taxisl, tclopl, timbeg, timend, tmult, tolrce, tstep, ttlev, ttmax, ttmin
  real(8) :: vaxisl, vh, vl, vmax, vmaxr, vmin, vminr, vs
  real(8) :: xid, xsuper, xtit
  real(8) :: yid, ylevel, ysuper, ytit, yymax, yymin
  character bbus, buslst(1000)
  common /cblock/ datepl(2), tclopl(2), bbus(300)
  common /cblock/ tmult, dy, dx, hpi, tstep, gxmin, gxmax
  common /cblock/ ew(15000), finfin, fill, fvcom(50)
  common /cblock/ yymin(20), yymax(20), ttmin(20), ttmax(20)
  common /cblock/ ylevel(20), ttlev(20), dyold(20), mlevel(20)
  common /cblock/ aaa(20), bbb(20), kp(20), fxref(25), fyref(25)
  !     evnbyt puts buslst on real*8 byte boundary (ibm level 4 error):
  common /cblock/  evnbyt, ev(15000), bx(150), vminr, vmaxr
  dimension  ibsout(1), ibrnch(1), jbrnch(1)
  equivalence  ( ev(   1), buslst(1) ),  ( ev(1001), ibsout(1) )
  equivalence  ( ev(1301), ibrnch(1) ),  ( ev(1601), jbrnch(1) )
  equivalence  ( fvcom( 1),   vmin ),  ( fvcom( 2),   vmax )
  equivalence  ( fvcom( 3),   hmin ),  ( fvcom( 4),   hmax )
  equivalence  ( fvcom( 5),     ha ),  ( fvcom( 6), taxisl )
  equivalence  ( fvcom( 7), tolrce ),  ( fvcom( 8),   htax )
  equivalence  ( fvcom( 9),   xtit ),  ( fvcom(10),   ytit )
  equivalence  ( fvcom(11), siztit ),  ( fvcom(12), xsuper )
  equivalence  ( fvcom(13), ysuper ),  ( fvcom(14), sizsup )
  equivalence  ( fvcom(15),  fline ),  ( fvcom(16),  sizid )
  equivalence  ( fvcom(17),    xid ),  ( fvcom(18),    yid )
  equivalence  ( fvcom(19),   fact ),  ( fvcom(20), dxgrd1 )
  equivalence  ( fvcom(21), dygrd1 ),  ( fvcom(22), dxgrd2 )
  equivalence  ( fvcom(23), dygrd2 ),  ( fvcom(24),  fill1 )
  equivalence  ( fvcom(25),  fill2 ),  ( fvcom(26),     vs )
  equivalence  ( fvcom(27),     vl ),  ( fvcom(28),     vh )
  equivalence  ( fvcom(29), papmax ),  ( fvcom(30), timbeg )
  equivalence  ( fvcom(31), timend ),  ( fvcom(32), fhtax  )
  equivalence  ( fvcom(33), fxsup  ),  ( fvcom(34), fysup  )
  equivalence  ( fvcom(35), fxtit  ),  ( fvcom(36), fytit  )
  equivalence  ( fvcom(37), fxid   ),  ( fvcom(38), fyid   )
  equivalence  ( fvcom(39), ftcarr ),  ( fvcom(40), vaxisl )
  equivalence  ( fvcom(41), fxnumv ),  ( fvcom(42), fxnumh )
  equivalence  ( fvcom(43), fvaxtt ),  ( fvcom(44), fxvert )
  equivalence  ( fvcom(45), fsymb  ),  ( fvcom(46), paplim )
  common /cblock/ mmm(20), mstart(20), numpts(20), killpl
  common /cblock/ kstart(20)
  common /cblock/ mplot(20), jhmsp, jchan, labrtm(20)
  common /cblock/ jplt, icp, icurse, mxypl, indexp, ind1
  common /cblock/ numflt, ncut, numtek, newfil, mu6sav
  common /cblock/ mcurve(20), namvar, mfake, numraw
  common /cblock/ nchsup, nchver, maxev, kptplt
  common /cblock/ numnvo, nv, nc, numbrn, numout
  common /cblock/ jplt1, jbegbv, jbegbc, limfix, nt2
  common /cblock/ maxew,   maxip, msymbt(20)
  common /cblock/ l4plot, ivcom(60)
  equivalence  ( ivcom( 1),  klevl ),  ( ivcom( 2),  kextr )
  equivalence  ( ivcom( 3),    ihs ),  ( ivcom( 4), lu7plt )
  equivalence  ( ivcom( 5), iprsrt ),  ( ivcom( 6), limcol )
  equivalence  ( ivcom( 7), linepr ),  ( ivcom( 8),  ibaud )
  equivalence  ( ivcom( 9),   ltek ),  ( ivcom(10), numtit )
  equivalence  ( ivcom(11), maxsym ),  ( ivcom(12),   mtit )
  equivalence  ( ivcom(13), maxisx ),  ( ivcom(14), maxisy )
  equivalence  ( ivcom(15), mgrid1 ),  ( ivcom(16), mgrid2 )
  equivalence  ( ivcom(17), msuper ),  ( ivcom(18),    mid )
  equivalence  ( ivcom(19),  mline ),  ( ivcom(20),  ncut1 )
  equivalence  ( ivcom(21),  ncut2 ),  ( ivcom(22), nsmplt )
  equivalence  ( ivcom(23),  lsymb ),  ( ivcom(24),  nxmax )
  equivalence  ( ivcom(25),  nymax ),  ( ivcom(26),  lchid )
  equivalence  ( ivcom(27), nxinch ),  ( ivcom(28), nyinch )
  equivalence  ( ivcom(29),  nxoff ),  ( ivcom(30),  nyoff )
  equivalence  ( ivcom(31),   look ),  ( ivcom(32), lchsup )
  equivalence  ( ivcom(33), lchtit ),  ( ivcom(34), lchxax )
  equivalence  ( ivcom(35), lchyax ),  ( ivcom(36),  iterm )
  equivalence  ( ivcom(37),   ltic ),  ( ivcom(38),  iztit )
  equivalence  ( ivcom(39),  izgr1 ),  ( ivcom(40),  izgr2 )
  equivalence  ( ivcom(41), ldshg1 ),  ( ivcom(42), ldshg2 )
  equivalence  ( ivcom(43),  izxax ),  ( ivcom(44),  izyax )
  equivalence  ( ivcom(45),  nxid6 ),  ( ivcom(46),  nyid6 )
  equivalence  ( ivcom(47),  nxend ),  ( ivcom(48),  nyend )
  equivalence  ( ivcom(49),   izid ),  ( ivcom(50), nxvern )
  equivalence  ( ivcom(51), ichref ),  ( ivcom(52), ichend )
  equivalence  ( ivcom(53), inchpx ),  ( ivcom(54), inchpy )
  equivalence  ( ivcom(55), inwait ),  ( ivcom(56), nolabl )
  equivalence  ( ivcom(57), lchfil ),  ( ivcom(58), lchlim )
  equivalence  ( ivcom(59), mu6std )
  character(80) filnam, alpha, buffin, vertl, headl, sext
!  character(30) file30
  character(24) xytitl, horzl
  character(8) textd1, textd2
  character(8) curren, voltag, brclas !, char20
  character(8) slot1, date, time
  character(8) ibuff,  ansi, abuf77, anplt
!       next type key character key words in common, in order:
  character(8) choice,   stop,  purge,    out,   help
  character(8) smooth,   size,   show, linezz,   photo
  character(8) repeat,  flush, playba,    pen, multip
  character(8) offset, limits,  debug,    tek,  stack
  character(8) printe, metric, alltim, column, setcol
  character(8) longer, averag,  inner, rescal,   lastpl
  character(8) batch,  punch, extrem,  level, noplot
  character(8) messag,    end, timesp,  label, timeun
  character(8) cursor, xyplot,  slope,   back, refile
  character(8) texblk, setdat, texfnt
  common /pltans/ abuf77(10), ansi, ibuff(20), texfnt
  common /pltans/ sext(6), headl, vertl, buffin
  common /pltans/ slot1(20), horzl(8), date, time
  common /pltans/ textd1, textd2, curren, voltag, brclas
  common /pltans/ filnam, alpha, xytitl, anplt(60)
  equivalence ( anplt( 1), choice ),  ( anplt( 2),   stop )
  equivalence ( anplt( 3),  purge ),  ( anplt( 4),    out )
  equivalence ( anplt( 5),   help ),  ( anplt( 6), smooth )
  equivalence ( anplt( 7),   size ),  ( anplt( 8),   show )
  equivalence ( anplt( 9), linezz ),  ( anplt(10),  photo )
  equivalence ( anplt(11), repeat ),  ( anplt(12),  flush )
  equivalence ( anplt(13), playba ),  ( anplt(14),    pen )
  equivalence ( anplt(15), multip ),  ( anplt(16), offset )
  equivalence ( anplt(17), limits ),  ( anplt(18),  debug )
  equivalence ( anplt(19),    tek ),  ( anplt(20),  stack )
  equivalence ( anplt(21), printe ),  ( anplt(22), metric )
  equivalence ( anplt(23), alltim ),  ( anplt(24), column )
  equivalence ( anplt(25), setcol ),  ( anplt(26), longer )
  equivalence ( anplt(27), averag ),  ( anplt(28),  inner )
  equivalence ( anplt(29), rescal ),  ( anplt(30),  lastpl)
  equivalence ( anplt(31),  batch ),  ( anplt(32),  punch )
  equivalence ( anplt(33), extrem ),  ( anplt(34),  level )
  equivalence ( anplt(35), noplot ),  ( anplt(36), messag )
  equivalence ( anplt(37),    end ),  ( anplt(38), timesp )
  equivalence ( anplt(39),  label ),  ( anplt(40), timeun )
  equivalence ( anplt(41), cursor ),  ( anplt(42), xyplot )
  equivalence ( anplt(43),  slope ),  ( anplt(44),   back )
  equivalence ( anplt(45), refile ),  ( anplt(46), texblk )
  equivalence ( anplt(47), setdat )

   !     module used only for interactive emtp (service to "emtspy").
   !     for non-interactive emtp, this module can be destroyed.
   !   include 'dekspy.ftn'
   !   include 'dekplt.ftn'
   data texfnt  /  'f7x13.b '  /
   data  xytitl(1 : 24)  /  '                        '  /
   data  headl(1 : 16)   /  '                '  /
   data  vertl(1 : 16)   /  '                '  /
   data  horzl(1)    /  'degrees based on 60 hz  '  /
   data  horzl(2)    /  'cycles based on 60 hz   '  /
   data  horzl(3)    /  'seconds                 '  /
   data  horzl(4)    /  'milliseconds            '  /
   data  horzl(5)    /  'microseconds            '  /
   data  horzl(6)    /  'frequency in hertz      '  /
   data  horzl(7)    /  'log10 frequency in hertz'  /
   data  horzl(8)    /  '1st variable of x-y pair'  /
   data  curren     /  'current '  /
   data  voltag     /  'voltage '  /
   !     begin command-word definitions.   ^^^^^^  ^^^^^^   ^^^^^^   ^^^^^
   data  choice     /  'choice  '  /
   data  stop       /  'stop    '  /
   data  purge      /  'purge   '  /
   data  help       /  'help    '  /
   data  smooth     /  'smooth  '  /
   data  size       /  'size    '  /
   data  show       /  'show    '  /
   data  linezz     /  'line    '  /
   data  photo      /  'copy    '  /
   data  end        /  'end     '  /
   data  repeat     /  'repeat  '  /
   data  flush      /  'flush   '  /
   data  playba     /  'playback'  /
   data  pen        /  'pen     '  /
   data  multip     /  'factor  '  /
   data  offset     /  'offset  '  /
   data  limits     /  'limits  '  /
   data  time       /  'time    '  /
   data  timesp     /  'timespan'  /
   data  debug      /  'debug   '  /
   data  tek        /  'tek     '  /
   data  stack      /  'stack   '  /
   data  printe     /  'printer '  /
   data  metric     /  'metric  '  /
   data  alltim     /  'all time'  /
   data  column     /  'column  '  /
   data  setcol     /  'set colu'  /
   data  out        /  'out     '  /
   data  longer     /  'longer  '  /
   data  averag     /  'average '  /
   data  inner      /  'in      '  /
   data  rescal     /  'rescale '  /
   data  lastpl     /  'last    '  /
   data  batch      /  'batch   '  /
   data  punch      /  'punch   '  /
   data  extrem     /  'extrema '  /
   data  level      /  'level   '  /
   data  noplot     /  'no plot '  /
   data  messag     /  'message '  /
   data  timeun     /  'time uni'  /
   data  label      /  'label   '  /
   data  cursor     /  'cursor  '  /
   data  xyplot     /  'x-y plot'  /
   data  slope      /  'slope   '  /
   data  back       /  'back    '  /
   data  refile     /  'refile  '  /
   data  texblk     /  'blank   '  /
   data  setdat     /  'set data'  /
   !     end of command definitions  ^^^^^^   ^^^^^^   ^^^^^^   ^^^^^^
   data  tolrce     /  8.e-5  /
   data  finfin     /  1.e12  /
   data  timbeg     /   0.0   /
   data  timend     /  1.e20  /
   data  paplim     /  36.    /
   data  vs      /  1.0  /
   data  vl      /  5.0  /
   data  vh      /  6.0  /
   data  nsmplt  /  50   /
!   data  kslowr  /   3   /
   data  limfix  /   0   /
   data  klevl   /   0   /
   data  kextr   /   0   /
   data  jhmsp   /   0   /
   data  taxisl  /  5.0  /
   data  mu6std  /  6  /
   data  htax    /  4.0  /
   data  limcol  /  79   /
   data  ltek    /  1   /
   data  numtek  /   0   /
   data  inwait  /   1   /
   !     begin parameters of tektronix screen
   data  nxinch   /    74   /
   data  nyinch   /    68   /
   data  nxoff    /    100  /
   data  nyoff    /    40   /
   data  nxvern   /    30   /
   data  inchpx   /    2    /
   data  inchpy   /    2    /
   data  look     /    6    /
   data  nymax    /   800   /
   data  nxmax    /   800   /
   data  lchid    /    2    /
   data  lchsup   /    1    /
   data  lchtit   /    2    /
   data  lchxax   /    0    /
   data  lchyax   /    0    /
   data  izgr1    /    0    /
   data  izgr2    /    0    /
   data  ldshg1   /    1    /
   data  ldshg2   /    1    /
   data  izxax    /    0    /
   data  izyax    /    0    /
   data  izid     /    0    /
   data  iterm    /    2    /
   data  ltic     /    7    /
   data  iztit    /    0    /
   data  nxid6    /   10   /
   data  nyid6    /   330  /
   data  nxend    /   512   /
   data  nyend    /    50   /
   data  icurse   /    0    /
   !   data  ichref   /   'p'   /
   data ichref    /   112   /
   !   data  ichend   /   'e'   /
   data ichend    /   101   /
   data  vaxisl   /   4.0   /
   data  fxnumv   /   1.5   /
   data  fxnumh   /   5.0   /
   data  fxvert   /   0.0   /
   data  fsymb    /   .83   /
   data  lsymb    /    1    /
   data  lchfil   /    0    /
   data  lchlim   /    0    /
   data  ibaud    /   960   /
   !     end parameters of tektronix screen
   data  lu7plt   /  7  /
   data  linepr   /  9  /
   data  mxypl    /  0  /
   data  numflt   /  0  /
   data  numtit   /  0  /
   data  xtit     /  0.5  /
   data  ytit     /  8.5  /
   data  siztit   /  .12  /
   data  xsuper   /  1.0  /
   data  ysuper   /  9.0  /
   data  sizsup   /  0.3  /
   data  nchsup   /   0   /
   data  nchver   /   0   /
   data  dxgrd1   /  1.0  /
   data  dygrd1   /  1.0  /
   data  dxgrd2   /  0.2  /
   data  dygrd2   /  0.2  /
   data  fill1    /  1.0  /
   data  fill2    /  1.0  /
   data  ncut1    /   1   /
   data  ncut2    /   1   /
   data  maxsym   /   3   /
   data  fline    /  1.7  /
   data  sizid    /  .12  /
   data  xid      /  0.5  /
   data  yid      /  .75  /
   data  fact     /  1.0  /
   data  fhtax    /  0.5  /
   data  fxsup    /  0.3  /
   data  fysup    /  -.03 /
   data  fxtit    /  .10  /
   data  fytit    /  .15  /
   data  fxid     /  .05  /
   data  fyid     /  .10  /
   data  ftcarr   /  1.0  /
   data  fvaxtt   /  -7.5  /
   data  mtit     /  3  /
   data  maxisx   /  3  /
   data  maxisy   /  3  /
   data  mgrid1   /  2  /
   data  mgrid2   /  1  /
   data  msuper   /  5  /
   data  mid      /  3  /
   data  mline    /  3  /
   data  killpl   /  0  /

end module dekplt

!
! end of file dekplt.f90
!
