c-*- mode: fortran; syntax: ansi-fortran-77; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
c
c     file: over51.for
c
c
c     subroutine over51.
c
      subroutine over51
      implicit real*8 (a-h, o-z) ,
     1      integer*4 (i-n)
      include  'blkcom.ftn'
      if ( iprsup  .ge.  1 )
     1 write ( lunit6, 4567 )  kill
 4567 format ( 24h begin "over51".  kill =,  i8 )
      call runtym ( vmin, vmax )
      flstat(9) = vmin - flstat(9)
      flstat(10) = vmax - flstat(10)
      if ( kill .eq. 0 )  go to 4521
      call subr51
      go to 4536
 4521 lastov = nchain
      nchain = 55
 4536 if ( iprsup  .ge.  1 )
     1 write ( lunit6, 4568 )
 4568 format ( 22h exit module "over51". )
99999 return
      end
c
c     subroutine subr51.
c
      subroutine subr51
      implicit real*8 (a-h, o-z) ,
     1     integer*4 (i-n)
      include  'blkcom.ftn'
      dimension  kpen(20)
c     note. --- as the structure of the emtp error overlays change,
c               the following assignments may have to be altered ...
c                    nfrfld  ---- total number of error overlays
c                    kpen(j) ---- storage for the highest kill-code
c                                 number handled by error overlay
c                                 number  50+j ,   for all but the
c                                 last error overlay.
      data  kpen(1)   /  50   /
      data  kpen(2)   /  90   /
      data  kpen(3)   /  150  /
      data  kpen(4)   /  200  /
      nfrfld = 5
      if ( iprsup  .ge.  1 )
     1 write ( lunit6, 4567 )  kill
 4567 format ( 24h begin "subr51".  kill =,  i8 )
      if ( kill .eq. 99  .and.  lstat(19) .eq. 11111 )
     1 call stoptp
      kpen(nfrfld) = intinf
      call runtym ( vmin, vmax )
      flstat(9) = vmin - flstat(9)
      if ( ipntv(1)  .ne.  -8888 )   go to 1643
c     following assignments are to define exceptional /blank/
c     variables used in  "kill codes"  usage:
      if ( kill  .eq.  38 )   flstat(12) = 1.0
      if (kill  .eq. 41)   lstat(10) = 1
      if (kill  .eq. 43)  flstat(12) = 1.
      if ( kill - 1  .ne.  ipntv(3) )   go to 1643
      ipntv(3) = kill
      go to 1649
 1643 if ( lastov  .ge.  nchain )   go to 1649
      noutpr = 0
      call interp
      write(lunit6, 5315)
 5315 format( 1h  )
      write(lunit6, 5316)
 5316 format( 132h -----------------------------------------------------
     1------------------------------------------------------------------
     2------------     )
      do 5319  i=1, 2
 5319 write(lunit6, 5320)
 5320 format( 132h error/error/error/error/error/error/error/error/error
     1/error/error/error/error/error/error/error/error/error/error/error
     2/error/error     )
      write(lunit6, 5316)
      if ( m4plot .eq. 1 )  go to 5534
      write(lunit6, 5332)
 5332 format(118h0you lose, fella.   the emtp logic has detected an erro
     1r condition, and is going to terminate your run.  the following  ,
     2 /, 121h printout message summarizes the data difficulty leading t
     3o this program decision.  by studying this message, the problem  ,
     4 /,118h data,  and the rules delineated by the  840-page emtp rule
     5 book,  it is hoped that the user can rectify the problem.     ,
     6 /,  81h if still in doubt after some study, come see program main
     7tenance for assistance.    )
      read (unit = abuff(1), fmt = 1019) (texcol(j), j = 1, 14)
 1019 format ( 13a6, a2 )
      write (lunit6, 5333)  ( texcol(j), j=1, 14 )
 5333 format( 113h where an otherwise-unidentified card is referred to,
     1or is called the 'last' card, this means the most-recently-   ,/,
     2113h read card of the input data deck.   the 80-column card image
     3in question is the last one printed out before this   ,/,
     4 42h termination message.   a copy follows....,  13a6, a2 )
 5534 write (lunit6, 5335)  kill, lstat(18), lstat(19)
 5335 format( 14x, 16hkill code number, 16x, 14hoverlay number,
     1 10x, 20hnearby statement no.  , /, 3i30 )
 1649 do 1658  j=1, nfrfld
      if ( kill  .gt.  kpen(j) )   go to 1658
      if ( j  .eq.  1 )   go to 1684
      lastov = nchain
      nchain = 50 + j
      if ( iprsup  .ge.  1 )
     1 write ( lunit6, 1652 )   kill, j, kpen(j), nchain
 1652 format ( 21h error overlay found.,
     1         29h   kill, j, kpen(j), nchain =,  4i8  )
      go to 9000
 1658 continue
 1684 continue
      go to ( 6001, 6002, 6003, 6004, 6005, 6006, 6007, 6008, 6009,
     1 6010, 6011, 6012, 6013, 6014, 6015, 6016, 6017, 6018, 6019, 6020,
     2 6021, 6022, 6023, 6024, 6025, 6026, 6027, 6028, 6029, 6030,
     3 6031, 6032, 6033, 6034, 6035, 6036, 6037, 6038, 6039, 6040,
     4 6041, 6042, 6043, 6044, 6045, 6046, 6047, 6048, 6049, 6050), kill
 6001 write(lunit6, 7001)  lstat(16)
 7001 format( 5x, 33hstorage exceeded for list number , i2, 46h.   see d
     1imensioned limit in tabulation below.  ,/,
     2 5x, 112hthe problem being inputted is simply too big for the prog
     3ram as currently dimensioned.   since there usually are ,/,
     4 5x, 110hways of circumventing this difficulty, it is suggested th
     5at the user consult his friendly neighborhood program   ,/,
     6 5x,  16hmaintenance man.   )
      go to 6220
 6002 if ( t  .lt.  0.0 )   go to 7102
      write (lunit6, 7002)  flstat(16)
 7002 format( 5x, 108htime-step size 'deltat' as read from columns 1-8 o
     1f the first miscellaneous data card is not positive.   the  ,/,
     2 5x,  23huser punched a value of , e14.4,  75h.   unlike jules ver
     3ne, you are not allowed to stop or decrease time during  ,/,
     4 5x, 111ha simulation, my friend.   don't try riding out of an act
     5ive volcano on a raft floating on molten lava, either,   ,/,
     6 5x,  30halthough that's another story.   )
      go to 6220
 7102 write (lunit6, 7202)  t
 7202 format (5x, 103hthe starting time   't'   as read from the floatin
     1g-point miscellaneous data card is negative, which is        ,/,
     2 5x,  21hillegal.   a value of,    e15.5,     49h   was read from
     3the data field of columns 49-56.      )
      go to 6220
 6003 write(lunit6, 7003)  lstat(16)
 7003 format( 5x,  91hillegal type code read from columns 1-2 of last br
     1anch card.   the user punched a value of , i2, 10h.   either ,/,
     2 5x, 113hthe punched number itself is patently illegal (always an
     3error, under any circumstances), or the card in question  ,/,
     4 5x, 107his out of sequence in relation to the preceding data card
     5 which was inputted.   as an example of the latter  ,/,
     6 5x, 113hcase, consider a '-3' punch, with the preceeding card not
     7 bearing a '-2' punch.   in any case, open up the user's  ,/,
     8 5x,  93hmanual, and reread the rules for the data type that you h
     9ave been trying to input, my friend.    )
      write (lunit6, 7103)
 7103 format (5x, 109hyet, in case this general advice does not seem to
     1apply, consider the possible trouble which can arise from a   ,/,
     2 5x, 113hpreceding faulty use of the reference-branch capability.
     3  this feature has the emtp looking for a certain number   ,/,
     4 5x, 112hand type of branch cards, based on properties of the comp
     5onent to which reference has been made.   if the user's   ,/,
     6 5x, 113hdata cards do not in structure match those of the referen
     7ce component, an error stop of the present type may very    )
      write (lunit6, 7203)
 7203 format (5x, 109hbe expected.   remember that in cases where two or
     1 more branches from reference bus  'bus3'  to reference bus   ,/,
     2 5x, 110h'bus4'  (ordered pair of names, read from columns 15-24 a
     3s  2a6  information) exist, the emtp will always pick    ,/,
     4 5x, 78 hthe first one that it finds (in order of branch input) fo
     5r reference purposes.     )
      go to 6220
 6004 write(lunit6, 7004)
 7004 format( 5x, 108hlast branch was zero impedance (r, l, c fields of
     1columns 27-44 were all zero).   if you really want a short   ,/,
     2   5x, 111hcircuit, you must punch a very small value for  r  or
     3l.   or better yet, why not do away with one of the node   ,/,
     4 5x,  83hnames of this branch, treating both ends as the same bus
     5(a perfect short circuit).   )
      go to 6220
 6005 write(lunit6, 7105)
 7105 format( 5x, 114hthe user has been inputting pairs of points which
     1define a nonlinear (or pseudo-nonlinear) element characteristic. ,
     2 /, 5x, 108hthese must be in order, moving ever to the right and u
     3pward in the x-y plane.   but the user's just-inputted     )
      write(lunit6, 7005)
 7005 format( 5x, 107hcharacteristic is not monotone increasing as it sh
     1ould be.   a decrease in one of the two coordinate points   ,/,
     2 5x,  85hhas been detected on the last data card which was read.
     3 shape up or ship out, jack.   )
      go to 6220
 6006 write(lunit6, 7006)  bus3, bus4
 7006 format( 5x, 114hreference branch names of last data card do not re
     1fer to previously-inputted branch of same type.   the data being ,
     2 /, 5x, 116hreferenced can not be found.   check for spelling erro
     3rs, or position differences of blanks within the field widths. ,/,
     4 5x,  58hdefective names as read off card from columns 15-26 were
     5', a6, 7h' and ', a6, 2h'.   )
      go to 6220
 6007 write(lunit6, 7007)  lstat(16)
 7007 format( 5x, 106han illegal type code has been read from columns 1-
     12 of the last data card, which the emtp believes to be a ,/,
     2 5x,  40hswitch card.   the emtp read a value of , i2,  46h.   but
     3 such a value cannot be accepted, since   ,/,
     4  5x, 43honly type codes  0 (ordinary switch),  92 (,
     5      48hswitched resistance),  93 (switched inductance),,
     6 /5x, 45h  11 (diode and tacs-controlled valve),  12 (,
     7      35htacs-controlled spark gap or triac),
     8 /5x, 46hand  13 (tacs-controlled ordinary switch)  are,
     9      10h allowed.     )
      go to 6220
 6008 write(lunit6, 7008)
 7008 format( 5x, 111han error in the ordering of switch cards has occur
     1ed.   all switched-resistance elements (type-92) must precede ,/,
     2   5x, 104hany switched-inductance (type-93) elements.   the last
     3data card which has been read violates this rule.    ,/,
     4 5x,  27hget with it, guy, shape up.   )
      go to 6220
 6009 write(lunit6, 7109)
 7109 format( 5x,  97hthe last data card which was read has been taken b
     1y the emtp to be a switched-inductance element.    )
      write(lunit6, 7009)  flstat(16)
 7009 format( 5x, 108hbut this component  has saturation (breakpoint) fl
     1ux nonpositive, which is absurd.   from columns 45-54, the   ,/,
     2 5x,  22hprogram read the value , e12.3, 25h from the last data ca
     3rd.  )
      go to 6220
 6010 write(lunit6, 7010)  lstat(16)
 7010 format( 5x, 105hthe last data card which was read has been taken t
     1o be a source card.   but it bears an illegal type code  ,/,
     2 5x,  27hin columns 1-2 (a value of , i2,  79h was read).   open u
     3p the user's manual, and reread the rules for source cards.  )
      go to 6220
 6011 write(lunit6, 7011)  lstat(16)
 7011 format( 5x,  93htype-16 controlled dc voltage source card has ille
     1gal initial condition code in columns 9-10.      ,/,
     2 5x,  19hthe value read was , i2 )
      go to 6220
 6012 write(lunit6, 7012)  bus1
 7012 format( 5x, 104hthe bus name read from columns 3-8 of the last sou
     1rce card is unrecognizable, not having been previously  ,/,
     2 5x,  77hdefined by a branch card or a switch card.   the name rea
     3d from the card is ', a6,  20h'.   the user is not  ,/,
     4 5x, 112hallowed to connect sources to network nodes which are not
     5 a part of the network previously defined by branch and ,/,
     6 5x, 115hswitch cards of this data case.   after all, such nodes w
     7ould be completely disconnected from the network, so could  ,/,
     8 5x, 116hnot affect the network solution.   most probably, one or
     9more spelling errors have been made in punching node names.  )
      write(lunit6, 7131)
      go to 6220
 6013 write(lunit6, 7013)
 7013 format( 5x,  34h simulator voltage outside limits.   )
      go to 6220
 6014 write (lunit6, 7014)  bus1, bus2, flstat(16)
 7014 format( 5x,  35htype-93 nonlinear inductance from ', a6,
     1 6h' to ', a6,  37h' has psi-steady (columns 33-38) zero   ,/,
     2 5x,  71hwhile i-steady (columns 27-32) is nonzero.   value read f
     3or i-steady = , e12.3, 2h . , /,
     4 5x,  47hsuch zero-impedance branches are not permitted.   )
      go to 6220
 6015 write(lunit6, 7015)  lstat(15), lstat(16)
 7015 format( 5x,  86hbreakdown of logic in steady-state setup before ca
     1ll to subroutine number.   next, ii=, 2i10 )
      write(lunit6, 7115)
 7115 format( 5x, 110hthis is probably a program bug, not the fault of t
     1he user.   go seek help from program maintenance about this.   )
      go to 6220
 6016 write(lunit6, 7016)  lstat(16), ntot, lstat(15)
 7016 format( 5x,104harrays 'kolum' and 'korder' of the renumbering calc
     1ulation for the steady-state network have overflowed.   ,/,
     2 5x, 115hthe problem is simply too big or too dense (admittance ma
     3trix has too many nonzero elements) for the existing array  ,/,
     4 5x,   8hsizes of,  i7,  18h   cells.   of the,  i6,  64h   matrix
     5 rows total, overflow has occurred while working on the      ,/,
     6 5x, 29hsimulated elimination for row, i4,  64h.   this overflow t
     7rouble is really trouble with dependent list         )
      write (lunit6, 7116)
 7116 format (5x,  56hnumber 99, about which an explanation will follow
     1below.     ,/,
     2 5x,  87hif the core storage needed to prevent this overflow can n
     3ot be spared, as a last resort     ,/,
     8 5x, 112hthe user might bypass the steady-state calculation comple
     9tely, and simply run in the transients mode for several  )
      write(lunit6, 7216)
 7216 format( 5x, 104hcycles, hoping that by then the solution will have
     1 settled sufficiently into the sinusoidal steady-state  ,/,
     2 5x,  61h(following the energization shock which occurs at time ze
     3ro).    )
      kill = 1
      lstat(16) = 99
      nchain = 51
      if ( iprsup  .ge.  1 )
     1 write ( lunit6, 8943 )  nchain, kill
      go to 9000
 6017 write(lunit6, 7017)
 7017 format( 5x, 111hlogic breakdown in steady-state solution after ren
     1umbering of the s.s. network has been successfully completed.  )
      write(lunit6, 7115)
      write (lunit6, 7117)
 7117 format ( 5x,  31hbut then again, maybe it is not,
     1       38h the fault of the emtp.  several years,
     2       37h of experience with this message have   ,/,
     3         5x,  30hshown that the user's data was,
     4       41h invariably erroneous, though not stopped,
     5       40h at the time the branches were inputted.   )
      write (lunit6, 7217)  bus1, lstat(15)
 7217 format ( 5x,  29hfor example, there might be a,
     1       40h branch both ends of which touch node  ',
     2         a6,  25h' ,   found in row number,   i4,/,2x,
     3       40h   of the branch table (order of input).  )
      go to 6220
 6018 write (lunit6, 7018)  lstat(14), bus1, bus2
 7018 format( 5x, 109hin forming the network admittance matrix for the s
     1teady-state phasor solution for initial conditions, trouble ,/,
     2 5x,  34hhas occurred while processing the , i2,
     3 55h-phase coupled r-l elements the first of which connects, /,5x,
     4  5hbus ', a6, 12h' with bus ', a6,  81h'.   this could either be
     5an actual coupled r-l group, or the series sub-branches ,/,
     6 5x, 115hof a multi-phase pi-circuit.   in either case, the couple
     7d-branch-group impedance matrix    (z) = (r) + jw(l)   has ,/,
     8 5x, 112hbeen found to be near-singular (uninvertible).   in parti
     9cular, a near-zero diagonal element has been found just  )
      write(lunit6, 7118)  flstat(11), flstat(12), flstat(13)
 7118 format( 5x, 112hbefore reciprocation, during the inversion attempt
     1.   using magnitudes squared for all three quantities, we have ,/,
     2 5x, 25horiginal diagonal value =, e12.3,
     3 24h,   questionable value =, e12.3, 21h,   tolerance ratio =,
     4 e12.3, 1h. ,/,  5x,
     5 117hthe user had better check his data values for this coupled gr
     6oup very carefully, to see if something is not abnormal.   )
      write(lunit6, 7218)
 7218 format( 5x, 104hthe program tolerance ratio for near-zero checking
     1 is miscellaneous data parameter 'tolmat'.   perhaps a   ,/,
     2 5x, 109hrereading of the user's manual explanation of this input
     3constant would be in order, should the user still be   ,/,
     4 5x,  40huncertain of the reason for his trouble.   )
      go to 6220
 6019 write(lunit6, 7019)
 7019 format( 5x,  90hsteady-state solution logic trouble.   see program
     1 maintenance, since this is their fault.   )
      go to 6220
 6020 write(lunit6, 7020)
 7020 format( 5x,  74hlogic breakdown in steady-state solution while bui
     1lding admittance matrix.   )
      write(lunit6, 7115)
      go to 6220
 6021 write(lunit6, 7021)
 7021 format( 5x, 105hsteady-state solution breakdown because over 25 no
     1des of network are switched together.   this represents  ,/,
     2 5x, 113hoverflow of array itemp(??).   to solve the problem, simp
     3ly redimension  list 26  , or get rid of that inordinate  , /,
     4 5x, 118hnumber of switches which are all connected together.   th
     5at any legitimate power-system problem should ever reach this  ,/,
     6 5x, 109htermination is highly improbable, and somewhat suspect.
     7 unless the user understands what is wrong, and why,   ,/,
     8 5x, 104hsome consultation with the user's friendly neighborhood p
     9rogram maintenance man is strongly recommended.   )
      go to 6220
 6022 write(lunit6, 7022)  lstat(16)
 7022 format( 5x,  70hbig trouble.   singular nodal admittance matrix in
     1volving node number , i3,
     1                    41h of steady-state solution has been found. ,
     2 /, 5x, 103hbut user bus number for this node could not be found i
     3n renumbering map  norder.   program logic error.    )
      write(lunit6, 7115)
      go to 6220
 6023 write(lunit6, 7023)  bus1
 7023 format( 5x, 105hno.   we can not validly set voltage of disconnect
     1ed subnetwork last identified to zero, since it appears   , /,
     2 5x, 111hthat a current source feeds this subnetwork.   for this c
     3ase, either kirchhoff's current law for the subnetwork  ,/,
     4 5x,    114his violated (current in does not equal current out), o
     5r the solution is indeterminate (subnetwork voltage solution  ,/,
     6 5x, 107his only determined up to an arbitrary constant).   in eit
     7her case, the problem is ill-posed physically, and  ,/,
     8 5x,  84hmust be rejected.   user had better reconsider his networ
     9k in the vicinity of node ', a6,  19h', to either remove    )
      write(lunit6, 7123)
 7123 format( 5x,  51hthe singularity, or the current source(s), or both
     1.  )
      go to 6220
 6024 write(lunit6, 7024)  lstat(16), lstat(15), lstat(14)
 7024 format( 5x, 68hlogic failure near end steady-state solution.   kk,
     1 ia, index(ia+1)=, 3i10 )
      write(lunit6, 7115)
      go to 6220
 6025 write(lunit6, 7025)  ( lstat(i), i=11, 16 )
 7025 format( 5x,  79hlogic trouble within steady-state renumbering.   n
     1ext, mext, ib, i, jbs, nelim=, 6i8 )
      write(lunit6, 7115)
      go to 6220
 6026 write(lunit6, 7026)
 7026 format( 5x,  77hlogic error in steady-state renumbering.   kolum a
     1rray has erroneous numbers.   )
      write(lunit6, 7115)
      go to 6220
 6027 write(lunit6, 7127)  bus1, bus2, bus3
 7127 format ( 5x, 28hsource being read is type-18,
     1             31h ideal-transformer plus source.,
     2             32h   but one or more of the column   ,/,
     3         5x, 34h3-22 name fields are unrecognized.,
     4             17h   names are ....,    3( 2x, 1h", a6,
     5                      1h"),  2h .                  ,/,
     6         5x, 31hunless all three are legal emtp,
     7             30h network node names,  the case,
     8             30h can not be run.   correct it.     )
      go to 6220
 6028 write (lunit6, 7028)
 7028 format ( 5x, 32hthe last-read component has been,
     1             34h identified as an old-format (pre-,
     2             34h"m37.") zinc-oxide surge arrester.  ,/,
     3         5x, 34hsuch old data must be converted to,
     4             35h the newer format.   the supporting,
     5             32h program "zinold" (see rule book    ,/,
     6         5x, 39hindex) will perform all such conversion,
     7             33h for the user automatically, in a,
     8             22h single pass.  try it.               )
      go to 6220
 6029 write (lunit6, 7029)  bus1, bus2, flstat(16)
 7029 format( 5x,  51hdistributed-parameter branch card connecting node
     1', a6, 6h' to ', a6,  29h' represents propagation mode , /,
     2 5x,  24hwith travel time (sec) =, e14.5,  79h.   this is less tha
     3n time-step size  deltat, a case which is illegal.   either , /,
     4 5x,  94hrun with small enough deltat, or replace the distributed
     5modelling by lumped pi-circuit model.   )
      if( flstat(16)  .eq.  0.0 )
     1 write(lunit6, 7129)
 7129 format( 5x, 104hthe just-printed travel time is seen to be zero, t
     1hough, which is generally a default value deserving of    ,/,
     2 5x, 112hfurther comment.   assuming that the user has actually in
     3putted a line of finite length, the travel time must of  ,/,
     4 5x, 110hcourse be positive.   a zero value is used for default in
     5 the case of a frequency-dependent mode, however, for   ,/,
     6 5x, 110hthe case where the infinite-frequency travel time is less
     7 than the time-step-size  'deltat'  that the user has   ,/,
     8 5x, 115hselected.   to find the actual offending travel-time valu
     9e, the user can consult the data-card interpretation which   )
      if( flstat(16) .eq. 0.0 )
     1 write(lunit6, 7229)
 7229 format( 5x, 108hgoes along with the associated branch card as it w
     1as read in.   the emtp does not have the value in question  ,/,
     2 5x, 112himmediately available at this point, and can only tell th
     3e user that he's in trouble (without knowing how much).   )
      go to 6220
 6030 write(lunit6, 7030)  lstat(16)
 7030 format( 5x,  47hfrom columns 1-2 of last data card, a value of ,
     1 i2,  52h was read.   this is illegal.   the card in question , /,
     2 5x, 111hshould be either an initial-condition card, where values
     32, 3, or 4 are legal, or it is an output-specification  , /,
     4 5x,  57hcard, where values 0 or 1 are allowed.   shape up, fella.
     5 )
      go to 6220
 6031 write(lunit6, 7031)  bus1
 7031 format( 5x,  84hlast card gives node-voltage initial condition for
     1 unrecognizable node.   the name ', a6,  11h' which was ,/,
     2 5x,  52hpunched could not be found in the list of bus names.  )
      write(lunit6, 7131)
 7131 format( 5x, 105hcheck name(s) for spelling, as well as the column-
     1positioning of any blanks in the field(s) which is(are)  ,/,
     2 5x,  20hsix characters wide.   )
      go to 6220
 6032 write (lunit6, 7032)  bus1, bus2
 7032 format( 5x,  71hlast card gives initial-conditon currents for line
     1ar branch from node ', a6, 6h' to ', a6, 1h. )
 7232 write(lunit6, 7132)
 7132 format( 5x, 112hbut the branch referred to could not be found in t
     1he appropriate list of branches.   is the orientation correct.  )
      write(lunit6, 7131)
      go to 6220
 6033 write (lunit6, 7033)  bus1, bus2
 7033 format( 5x,  75hlast card gives initial condition currents for non
     1linear branch from node ', a6, 6h' to ', a6, 1h.  )
      go to 7232
 6034 write(lunit6, 7034)  lstat(13), lstat(14), flstat(15), flstat(16)
 7034 format( 5x,  80hlast card gives initial condition which is illegal
     1 for nonlinear element number , i3, 18h.   the last point ,/,
     2 5x,  60hof the nonlinear characteristic is stored in table locati
     3on , i3, 15h and has value , e13.4, 15h.   but initial ,/,
     4 5x,  30hcondition requires a value of , e13.4,  68h, which is off
     5 the end of the characteristic.   user must extend the ,/,
     6 5x,  70hcharacteristic, if this is really the initial condition t
     7hat he wants.   )
      go to 6220
 6035 write(lunit6, 7035)  lstat(16), bus1, bus2,
     1 flstat(11), flstat(12), flstat(13)
 7035 format( 5x,  90hbreakdown of solution when using compensation (sup
     1erposition) on nonlinear element number , i3,  /,
     2 5x,  21hwhich connects node ', a6, 6h' to ', a6, 2h'.  ,/,
     3 5x, 114hthevenin load line does not intersect the nonlinear chara
     4cteristic.   maybe user should extend the characteristic, /,
     5 5x, 112hassuming that the problem is otherwise well-posed.   some
     6 program solution quantities bearing on this breakdown,   ,/,
     7 5x,  91hpotentially useful to your program maintenance man (if no
     8t to the user), are as follows ...   ,/,
     9 5x,  16he(k), e(m), a2 = , 3e13.4  )
      write (lunit6, 6135)
 6135 format (5x, 108hpast experience based on a number of encounters wi
     1th this emtp error stop has shown that most often the user    ,/,
     2 5x, 110hhas under-estimated the peak nonlinear (or time-varying)
     3element ordinate value.   that is, the characteristic        ,/,
     4 5x, 116has inputted was correct, perhaps, but was simply not defi
     5ned for large enough values.   remember that true nonlinear   ,/,
     6 5x, 103hor time-varying elements (with type codes 91, 92, or 93)
     7do not have the final segments on the two ends                )
      write (lunit6, 6235)
 6235 format (5x, 111hextrapolated linearly to infinity (as is the case
     1with pseudo-nonlinear elements of type codes 97, 98, and 99).  ,/,
     2 5x, 105hbefore looking for any more sophisticated trouble, this v
     3ery simple possibility should always be checked.            )
      go to 6220
 6036 write (lunit6, 7036)  lstat(16), bus1, bus2,
     1 flstat(12), flstat(13), flstat(11)
 7036 format( 5x,  63hbreakdown surrounding nonlinear or time-varying el
     1ement number , i3, 24h having terminal nodes ', a6,
     2 7h' and ', a6, 2h'.  ,/,
     3 5x,  99hthis is a time-varying resistance with last time point of
     4 characteristic defined by user being for , e13.4,  /,
     5 5x, 11hsec.   but , e13.4,   89h sec have elapsed since element w
     6as turned on.   user must extend characteristic if study  ,/,
     7 5x,  66his to be run out further than the present simulation time
     8 of   t =, e12.3,  31h sec   at which this run is now  ,/,
     9 5x,  17hbeing terminated.   )
      go to 6220
 6037 write (lunit6, 7037)
 7037 format  (5x, 32hlast data card was decoded using,
     1             41h  4e16.0  and came up with floating point,
     2             35h zeros.   this is not legal for the
     3   ,/,   5x, 34hmodal impedance data of frequency-,
     4             34hdependent generator equivalents as,
     5             29h were being read in "fddata".   )
      go to 6220
 6038 d1 = flstat(13) / flstat(12)
      write(lunit6, 7238)
 7238 format( 5x,  34hmatrix (y) is the nodal admittance,
     1             18h matrix which must  ,/,
     2 5x, 101hbe re-triangularized at every time-step that a switch or
     3pseudo-nonlinear element changes its status.    )
      write (lunit6, 7038)  lstat(16), bus1, flstat(14), d1, flstat(12)
 7038 format( 5x,  41hthere has just been a breakdown factoring,
     1  30h (y), while eliminating on row, i4,
     3  29h  which corresponds to node ", a6, 10h".   after  ,/,
     4 5x,  92helimination to the left of the diagonal in this row, the
     5diagonal element was found to equal, e13.4,   7h, while   ,/,
     6 5x,  59hthe original value before any elimination was performed w
     7as, e13.4,  34h.   the ratio of these two numbers   ,/,
     8 5x,  34hexceeds the near-zero tolerance of, e13.4,  49h, which is
     9 miscellaneous data parameter 'epsiln'.   )
      write(lunit6, 7338)
 7338 format( 5x, 114hthe program thinks that either the node in questio
     1n, or a subnetwork to which it belongs, is disconnected from the )
      write(lunit6, 7138)
 7138 format(  5x,
     1     116hrest of the network.   if actually disconnected, user sho
     2uld add a connecting branch.   if connected by a very-high-  ,/,
     3 5x,  99himpedance branch, either make  epsiln  nearer to zero, or
     4 decrease the impedance of the connection.    ,/,
     5 5x, 109hbut be careful in resorting to a smaller 'epsiln', as num
     6erical solution difficulty will inevitably result as   ,/,
     7 5x, 115h'epsiln' approaches the computer roundoff limit (determin
     8ed by computer word-size).   the user is advised to reread   )
      write(lunit6, 7438)
 7438 format( 5x, 110hthe emtp user's manual explanation of the signific
     1ance of 'epsiln', should he be in doubt.   a call to program ,/,
     2 5x,  49hmaintenance might also be in order, in this case.  )
      write (lunit6, 7538)
 7538 format (/, 5x, 104hit might be desirable to say a few words about
     1how the printed 6-character node name, and the subnetwork     ,/,
     2 5x, 116hin question, are related.   having an isolated subnetwork
     3 means that the system matrix which is being triangularized    ,/,
     4 5x, 110his block diagonal, with the problem subnetwork then assoc
     5iated with a singular (uninvertible) block.   this is       ,/,
     6 5x, 114hdecoupled from the rest of the system, and mathematically
     7 the associated equations have an indeterminate solution.     )
      write (lunit6, 7638)
 7638 format (5x, 106hthis shows up as a zero diagonal element while eli
     1minating on the row for the last node of the subnetwork.      ,/,
     2 5x, 113hwhich node will be the last of the subnetwork is a functi
     3on of internal emtp renumbering, as well as the order of     ,/,
     4 5x, 111hthe network input data.   hence the user should not assum
     5e that there is trouble specifically at the node whose      ,/,
     6 5x, 111hname is printed above.   use of network connectivity prin
     7tout will allow reconstruction of the problem topology     )
      write (lunit6, 7738)
 7738 format (5x, 109hin the vicinity of the printed node.   the user is
     1 advised to go as far away from this node in all directions    ,/,
     2 5x, 115has is necessary, until the limits to the subnetwork in qu
     3estion become clear.   a knowledge of switch status at the     ,/,
     4 5x, 110hinstant of difficulty is of course crucial.   often this
     5error stop will occur just as a switch opens, thereby    ,/,
     6 5x, 111hsegmenting the system into two pieces, one of which has n
     7o paths to ground (which then becomes the disconnected       ,/,
     8 5x,  11hsubsystem).          )
      go to 6220
 6039 write (lunit6, 7039)  lstat(16), bus1, bus2
 7039 format( 5x,  63hsolution breakdown on nonlinear or time-varying el
     1ement number , i3, 18h connecting node ', a6, 13h' with node ',
     2 a6, 2h'.  ,/,
     3 5x, 114hone or more other such elements is actually touching this
     4 one, rather than being separated from it by the required  ,/,
     5 5x,  12htravel time.   )
      write(lunit6, 7139)
 7139 format( 5x, 107hwhile the problem may be well-posed physically, th
     1e present emtp solution assumptions concerning the use of  ,/,
     2 5x, 110hcompensation cannot handle such a case.   to remedy this
     3dilema, the user has two general potential courses of    ,/,
     4 5x,  11haction ....   ,/,
     5 8x, 106h1) eliminate the offending nonlinear or time-varying elem
     6ent(s).   if the offending elements are nonlinear   ,/,
     7 11x, 102hrather than time-varying, use might be made of switched
     8or pseudo-nonlinear elements of the same type.     )
      write(lunit6, 7239)
 7239 format( 11x,  99hwhile this converts true nonlinearities to pseudo
     1-nonlinearities which inherently lag one time step   ,/,
     2 11x, 112hbehind the solution (and thus must be used with caution
     3to ensure that movement along the desired characteristic  ,/,
     4 11x,  77his slow), one does avoid all isolation problems associat
     5ed with compensation.    ,/,
     6 8x, 115h2) introduce sufficient stub lines (small series-inductan
     7ce elements which are represented by lossless distributed-  ,/,
     8 11x, 109hparameter lines having travel-time equal to the time-ste
     9p-size 'deltat') to isolate the interfering elements.   )
      write(lunit6, 7339)
 7339 format( 11x, 104hif 'deltat' is small enough, such added elements
     1may not seriously disturb the problem physically, while  ,/,
     2 11x,  31hproviding the needed isolation.   ,/,
     3 5x, 111has both of these remedies are potentially dangerous if no
     4t used with considerable caution and understanding, it   ,/,
     5 5x, 115his suggested that the user apply some thought to the matt
     6er while studying the user's manual about these questions.  ,/,
     7 5x,  98ha call to program maintenance for some professional advic
     8e might also be appropriate, if in doubt.    )
      go to 6220
 6040 write(lunit6, 7040)  lstat(15), lstat(16), bus5, bus6, bus3, bus4
 7040 format( 5x,  90hsolution breakdown using compensation method on no
     1nlinear or time-varying elements number , i3, 5h and , i3, 1h. ,/,
     2 5x,  25hthe first connects node ', a6, 13h' with node ', a6,
     3  40h', while the second has terminal nodes ', a6, 7h' and ', a6,
     4 2h'.  ,/,
     5 5x,  94hthese two elements are not separated by the required trav
     6el time (distributed-parameter line).    )
      write(lunit6, 7139)
      write(lunit6, 7239)
      write(lunit6, 7339)
      go to 6220
 6041 write(lunit6, 7041)  lstat(10), bus1, bus2
 7041 format( 5x, 14hswitch number , i3, 18h connecting node ', a6,
     1 13h' with node ', a6,  30h' has closed, causing trouble.  )
      go to 6040
 6042 write(lunit6, 7142)
 7142 format( 5x, 103hmatrix  (yaa)  is the first-partition diagonal sub
     1matrix among non-switch (or pseudo-nonlinear-element)   ,/,
     2 5x,  83hnodes which is to be triangularized once and for all outs
     3ide of the time-step loop.    )
      write(lunit6, 7042)  lstat(16), bus1
 7042 format( 5x, 4hrow , i3, 102h in yaa (matrix for nonswitch rows) ha
     1s identically-zero diagonal element before starting elimination. ,
     2 /, 5x,  26hthis corresponds to node ', a6,  79h', where there is
     3either nothing connected, or there is a very strange, perfect  ,/,
     4 5x, 117hresonance.   the user should check all branches connected
     5 to the node in question.   if there are none, put one in or  ,/,
     6 5x,  26hdrop the node in question.   )
      go to 6220
 6043 write(lunit6, 7142)
      d1 = flstat(13) / flstat(12)
      write(lunit6, 7043)  lstat(16), bus1, flstat(14), d1, flstat(12)
 7043 format( 5x,  81hthere has been a breakdown in the triangularizatio
     1n of (yaa) while working on row, i4,  19h, which corresponds  ,/,
     2 5x,   9hto node ', a6, 93h'.   after elimination to the left of t
     3he diagonal in this row, the diagonal element has been  ,/,
     4 5x,  14hfound to equal, e13.4,  62h, while the original matrix va
     5lue (before any elimination) was, e13.4,   7h.   the  ,/,
     6 5x,  61hratio of these two numbers exceeds the near-zero toleranc
     7e of, e13.4,  41h, which is misc. data parameter 'epsiln'.   )
      write(lunit6, 7338)
      write(lunit6, 7138)
      write(lunit6, 7438)
      go to 6220
 6044 write(lunit6, 7044)
 7044 format( 5x, 104hyou dummy, you forgot to request output variables.
     1   if the branch cards have no column-80 branch-output  ,/,
     2 5x, 115hrequests, and no node voltages were requested for output,
     3 there is nothing to look at by way of a problem solution.  ,/,
     4 5x, 108hno variables are to be printed, and none can be plotted.
     5  the emtp will not let you run a problem this way.   ,/,
     6 5x,  88hgo back to the keypunch and add some output, do not pass
     7go, do not collect 200 dollars.    )
      go to 6220
 6045 write (lunit6, 7145)  lstat(17),
     1                      lstat(14), flstat(15), flstat(16)
 7145 format (5x,  20hweighting function a,  i1,
     1             35h(t) just inputted does not have all,
     2             31h which are monotone increasing.       ,/,
     2 5x,  28hin particular, point number , i3,  12h is for time,
     3 e15.6,  39h sec, while the preceding point was for    ,/,
     4 5x,   4htime, e15.6,  91h.   perhaps the card deck has been shuff
     5led, or somehow altered from that as punched by the ,/,
     6 5x,  38hweighting-function supporting program.   )
      go to 6220
 6046 write( lunit6, 7046 ) lstat(15), flstat(15), lstat(14)
 7046 format( 5x,  60hthe weighting function frequency dependent line mo
     1de number ,i2,  11h has failed ,/, 5x,  61hduring an attempt to im
     2prove the low frequency representation ,/, 5x,  64hby adjusting th
     3e exponential time constant of the response tails ,/, 5x,  70hto g
     4ive the theoretically correct areas.  the adjustment was requested
     5 ,/, 5x,  21hby a non-zero value = ,e10.5,  33h for "rtotal" in co
     6lumns 65 to 72 ,/, 5x,  76hof the miscellaneous data card, while a
     7 permissible iteration count of nii = ,i4, /, 5x,  65hwas provided
     8 for the newton-raphson algorithm in columns 73 to 80 )
      go to 6220
 6047 write(lunit6, 7047)  lstat(16)
 7047 format( 5x, 102hswitch-array overflow due to too many saturable tr
     1ansformers.   the switch tables are used for scratch  ,/,
     2 5x, 108hstorage of xformer starting-and-ending data pointers, dur
     3ing branch-data input.   the user has inputted more  ,/,
     4 5x,  65hsaturable xformers than the program limit for switches, w
     5hich is , i3,  41h.   either reduce the former, or increase ,/,
     6 5x,  11hthe latter.   )
      go to 6220
 6048 write(lunit6, 7048 )  bus3
 7048 format( 5x, 104hthe last data card inputted requests a copy of a p
     1receding saturable transformer which purportedly bears ,/,
     2 5x,  31hinternal identifying bus name ', a6,  80h'.   but a searc
     3h of preceding components has failed to locate such a component. ,
     4 /, 5x,  81hmaybe the user's data is out of order, or he has missp
     5elled the name in question.    )
      write(lunit6, 7131)
      go to 6220
 6049 if ( bus1  .ne.  trash )   go to 7249
      write (lunit6, 7349)  ksat, itype
 7349 format ( 5x, 108hthe user is currently inputting a saturable trans
     1former component, which by definition must have two or more  ,/,
     2 5x,  86hwindings.   the last-read data card should have been a wi
     3nding card for winding number,  i4,  20h ,   but this is not  ,/,
     4 5x,  47hwhat was punched in columns  1-2 .   a value of,
     5 i4,  33h   was read from this data field.   )
      go to 6220
 7249 write (lunit6, 7049)  bus1, lstat(16)
 7049 format( 5x, 107hthe user is currently inputting a saturable transf
     1ormer, using the reference branch procedure to get a copy  ,/,
     2 5x,  51hof the unit bearing internal identifying bus name ', a6,
     3  23h'.   but that unit had , i2,  21h windings.   the coil ,/,
     4 5x, 107hcards being read are either out of natural order (check c
     5olumns 1-2 sequence numbers), or they represent an  ,/,
     6 5x, 110hincorrect number of windings.   recall that the winding c
     7ards must be numbered 1, 2, etc. ----- consecutively,    ,/,
     8 5x, 111hwith no missing numbers.   once a break in this natural o
     9rder has been detected for the column 1-2 numbers, the   )
      write(lunit6, 7149)
 7149 format( 5x, 103hemtp logic assumes that data for the transformer i
     1n question has been completed.   this is why shuffled   ,/,
     2 5x,  61hwinding cards produce an 'erroneous-number-of-turns' mess
     3age.   )
      go to 6220
 6050 write(lunit6, 7050 )   bus6, lstat(15)
 7050 format( 5x,  92hdata error has been detected while working on inpu
     1t of 3-phase transformer component bearing   ,/,
     2 5x,  30hinternal bus-name identifier ', a6,  62h'.   the program
     3expects 3 single-phase transformer components   ,/,
     4 5x,  95hto follow the three-phase card, contiguously.   the last-
     5read branch has been found after only  , i3, /,
     6 5x,  24hsingle-phase components.  )
 6220 lastov = nchain
      nchain = nfrfld + 50
      if ( iprsup  .ge.  1 )
     1 write (lunit6, 8943)  nchain, kill
 8943 format ( 31h exit "subr51".  nchain, kill =,  2i6 )
 9000 return
      end
c
c     file: over51.for
c
