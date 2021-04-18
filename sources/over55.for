!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over55.for
!
!
!     subroutine over55.
!
subroutine over55
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  equivalence (moncar(4),  isw)
  if ( iprsup  .ge.  1 ) write ( lunit6, 4567 )  kill
4567 format ( 24h begin "over55".  kill =,  i6  )
  kilsav = kill
  if ( kill  .le.  0 )   go to 6633
6624 call subr55
6633 if ( nchain .eq. 51 )       go to 99999
  if ( nchain .eq. 31 )       go to 99999
  kill = 0
  if ( kol132  .eq.  132 )   go to 6639
  write (lunit6, 6634)  ( lstat(i+20),  i=1, 26 )
6634 format ( /,  42h actual list sizes for preceding solution: ,/,  14h   size  1-10:,  10i6 ,/,  14h   size 11-20:,  10i6 ,/,  14h   size 21-on:,  10i6   )
  go to 6645
6639 write (lunit6, 6554)
6554 format( /, 101h core storage figures for preceding data case now completed.  ---------------------------------------, 2x,7hpresent, 3x, 7hprogram, /, &
          63h a value of  -9999 indicates default, with no figure available.  ,  41x, 6hfigure, 5x, 12hlimit (name)    )
  write(lunit6, 38021)  lstat(21), lbus
38021 format( 5x,  39hsize list 1.   number of network nodes.  , 56x, 2i10, 7h (lbus)   )
  write(lunit6, 38022)  lstat(22), lbrnch
38022 format( 5x, 42hsize list 2.   number of network branches.  , 53x, 2i10, 9h (lbrnch)   )
  write(lunit6, 38023)  lstat(23), ldata
38023 format( 5x, 55hsize list 3.   number of data values in r, l, c tables.  , 40x, 2i10, 8h (ldata)   )
  write(lunit6, 38024)  lstat(24), lexct
38024 format( 5x,  49hsize list 4.   number of entries in source table., 46x, 2i10, 8h (lexct)   )
  write (lunit6, 38025) ktrlsw(3), iprsov(36), lstat(25), lymat
38025 format ( 5x,  34hsize list 5.   storage for (y) and, 20h triangularized (y).,   6x, 11hno. times =,  i5,  3x,   9hfactors =,  i5, 2x,  2i10,  8h (lymat)    )
  write (lunit6, 38026)  ktrlsw(5),  lstat(26), lswtch
38026 format( 5x,  35hsize list 6.   number of entries in, 14h switch table.,  15x,  11hno. flops =, i6,   14x, 2i10, 9h (lswtch)   )
  write (lunit6, 38027)  maxbus, lsize7
38027 format ( 5x, 39hsize list 7.   number of total distinct, 32h alphanumeric (a6) program names, 24x, 2i10,  9h (lsize7)   )
  write(lunit6, 38028)  lstat(28), lpast
38028 format( 5x, 67hsize list 8.   number of past history points for distributed lines.   , 28x, 2i10, 8h (lpast)    )
  write(lunit6, 38029)  lstat(29), lnonl
38029 format( 5x, 44hsize list 9.   number of nonlinear elements.   , 51x, 2i10, 8h (lnonl)   )
  write(lunit6, 38030)  lstat(30), lchar
38030 format( 5x, 67hsize list 10.  number of points defining nonlinear characteristics.   , 28x, 2i10, 8h (lchar)   )
  write (lunit6, 38031)  lstat(31), lsmout
38031 format (5x,  66hsize list 11.  number of branch or selective-node-voltage outputs.   ,  29x,  2i10,  9h (lsmout)    )
  write(lunit6, 38032)  lstat(32), lsiz12
38032 format( 5x, 92hsize list 12.  number of output quantities (limited only when printing max absolute values)., 3x, 2i10, 9h (lsiz12) )
  write(lunit6, 38033)  lstat(33), lfdep
38033 format ( 5x,  68hsize list 13.  number of 'weighting' frequency-dependent line modes.,  27x,  2i10,  8h (lfdep)   )
  write(lunit6, 38034)  lstat(34), lwt
38034 format( 5x, 82hsize list 14.  number of cells used to store freq.-dependence weighting functions.   , 13x, 2i10, 6h (lwt)   )
  write(lunit6, 38035)  lstat(35), ltails
38035 format( 5x, 78hsize list 15.  number of cells used for exponential-tail line-history storage.   , 17x, 2i10, 9h (ltails)   )
  write (lunit6, 38036)  lstat(36), limass
38036 format( 5x, 38hsize list 16.  total number of type-59, 13h s.m. masses.,  44x, 2i10, 9h (limass)   )
  write (lunit6, 38037)  lstat(37), lsyn
38037 format (5x,  54hsize list 17.  number of dynamic synchronous machines.   ,  41x,   2i10,  7h (lsyn)     )
  write (lunit6, 38038)  lstat(38), maxpe
38038 format (5x,  57hsize list 18.  number of branch power-and-energy outputs.     ,   38x,  2i10,  8h (maxpe)    )
  write (lunit6, 38039)  lstat(39), ltacst
38039 format (5x,  64hsize list 19.  floating-point working space for all tacs arrays. ,   31x,  2i10,  9h (ltacst)    )
  !     ktab is in blkcom, so it can not be equivalenced to
  if ( ktab .le. 0 )  go to 7272
  lstat(53) = lstat(63) - lstat(60) + lstat(53)
  lstat(56) = lstat(56) - lstat(58) + 3
  lstat(58) = ktab
  write (lunit6, 3632)  ( k,  k=1, 8 ), ( lstat(k), k=51, 58),   ( lstat(k), k=61, 68 )
3632 format ( 7x,  14htacs table no.,   8i10     ,/, 7x,  14hpresent figure,   8i10     ,/, 7x,  14hprogram limit ,   8i10         )
7272 write (lunit6, 38040)  lstat(40), lfsem
38040 format (5x, 46hsize list 20.  recursive convolution parameter, 42h storage for non-copied branch components., 7x,  2i10,  8h (lfsem)   )
  write (lunit6, 38041)  lstat(41), lfd
38041 format (5x,  75hsize list 21.  total storage cells for modal-phase transformation matrices. ,  20x ,  2i10,  6h (lfd)    )
  write (lunit6, 38042)  lstat(42), lhist
38042 format (5x,  34hsize list 22.  number of cells for, 21h convolution history.,  40x,  2i10,  8h (lhist)    )
  write (lunit6, 38071)  lstat(43), lsiz23
38071 format( 5x,  83hsize list 23.  giant arrays for renumbering and steady-state solution calculations.,  12x,  2i10,  9h (lsiz23)  )
  write (lunit6, 38044)  ncomp, lcomp
38044 format (5x,  72hsize list 24.  number of phases of compensation, based on maximum nodes.,  23x,  2i10,  8h (ncomp)    )
  write (lunit6, 38045)  lstat(45), lspcum
38045 format (5x,  13hsize list 25., 49h  floating-point working space for  u.m.  arrays., 33x,  2i10,  9h (lspcum)     )
  write (lunit6, 38046)  lstat(46), lsiz26
38046 format (5x,  39hsize list 26.  square of maximum number, 19h of coupled phases., 37x, 2i10, 9h (lsiz26)  )
6645 if ( kol132  .eq.  132 ) write (lunit6, 38003)
38003 format( 100h timing figures (decimal) characterizing case solution speed.  -------------------------------------, 4x, 6hcp sec, 3x, 7hi/o sec, 3x, 7hsum sec  )
  if ( flstat(7)  .ne.  -9999. )   go to 2601
  !     special timing code for supporting programs.
  d6 = flstat(9) + flstat(1)
  d7 = flstat(10) + flstat(2)
  d1 = d6 + d7
  if ( kol132  .eq.  132 ) write (lunit6, 38010)   d6, d7, d1
  if ( kol132  .eq.  80 ) write (lunit6, 6651)  d6, d7, d1
6651 format (  39h total case timing (cp, i/o, tot), sec:, 1x,  3f10.3  )
  lastov = nchain
  nchain = 1
  if ( iprsup  .ge.  1 ) write (lunit6, 4568)  kill
4568 format ( 23h exit "over55".  kill =,  i6  )
  go to 99999
2601 d1 = flstat(1) + flstat(2)
  d4 = d1
  if ( kol132  .eq.  132 ) write(lunit6, 38004)  flstat(1), flstat(2), d1
  if ( kol132  .eq.  80 ) write (lunit6, 6652)  flstat(1), flstat(2), d1
6652 format (  29h seconds for overlays  1-6  :, 3f9.3,   22h  --- (cp;  i/o;  tot)    )
38004 format( 5x, 67hdata input, sorting, and renumbering (pre steady state stuff) .....   , 28x, 3f10.3 )
  d1 = flstat(3) + flstat(4)
  d4 = d4 + d1
  if ( kol132  .eq.  132 ) write (lunit6, 38005)  flstat(3), flstat(4), d1
38005 format( 5x, 47hsteady-state (s.s.) solution calculations ..... , 48 x, 3f10.3 )
  if ( kol132  .eq.  80 ) write (lunit6, 6653)  flstat(3), flstat(4), d1
6653 format (  29h seconds for overlays  7-12 :,  3f9.3  )
  d1 = flstat(5) + flstat(6)
  d4 = d4 + d1
  if ( kol132  .eq.  132 ) write(lunit6, 38006) flstat(5), flstat(6), d1
38006 format( 5x, 53hpost-s.s. to pre-integration-setup calculations .....  , 42x, 3f10.3 )
  if ( kol132  .eq.  80 ) write (lunit6, 6654)  flstat(5), flstat(6), d1
6654 format (  29h seconds for overlays 13-15 :,  3f9.3  )
  d1 = flstat(7) + flstat(8)
  d4 = d4 + d1
  if ( kol132  .eq.  132 ) write(lunit6, 38007)  flstat(7), flstat(8), d1
38007 format( 5x, 54hintegration calculation (time in time-step loop) .....  , 41x, 3f10.3   )
  if ( kol132  .eq.  80 ) write (lunit6, 6655)  flstat(7), flstat(8), d1
6655 format (  29h seconds for time-step loop :,  3f9.3  )
  hmin = flstat(9)  +  flstat(10)
  d4 = d4 + hmin
  if ( kol132  .eq.  132 ) write (lunit6, 38008)  flstat(9), flstat(10), hmin
38008 format( 5x,   65hcomputer time in plotting or statistics termination overlay ..... ,30x,3f10.3)
  if ( kol132  .eq.  80 ) write (lunit6, 6656)   flstat(9), flstat(10), hmin
6656 format (  29h seconds after deltat-loop  :,  3f9.3  )
  d1 = flstat(11) + flstat(12)
  d4 = d1 + d4
  if ( kol132  .eq.  132 ) write (lunit6, 38009)  flstat(11), flstat(12), d1
38009 format ( 5x,  37h'deltat'-change restart time  .......,  58x, 3f10.3  ,/,  101x,  29h-----------------------------        )
  d2 = 0.0
  d3 = 0.0
  do i=1, 12, 2
     d2 = d2 + flstat(i)
38011 d3 = d3 + flstat(i+1)
  end do
  if ( kol132  .eq.  132 ) write (lunit6, 38010)  d2, d3, d4
38010 format ( 93x, 7htotals , 3f10.3, //,1x)
  if ( kol132  .eq.  80 ) write (lunit6, 6658)  d2, d3, d4
6658 format (  29x,  27h-------------------------   ,/, 20x,  9htotals  :,  3f9.3  )
  if ( isw  .ne.  4444 )  go to 6673
  isw = -3344
  call subr55
6673 lastov = nchain
  nchain = 1
  kill = 0
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )  kill
99999 return
end subroutine over55
!
!     subroutine subr55.
!
subroutine subr55
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  equivalence  (moncar(1),  knt),   (moncar(4),  isw)
  equivalence                       (moncar(10), mtape)
  character*8         text1, text2, text3, text4, text5
  character*8         text11, text12, text13, text14, text15
  character*8         text16, text17, text18, text19
  data  text1   / 6hbegin   /
  data  text2   / 6hnew     /
  data  text3   / 6hdata    /
  data  text4   / 6hcase    /
  data  text5   / 6hbndc    /
  data  text11  / 6hend     /
  data  text12  / 6hlast    /
  data  text13  / 6heldc    /
  data  text14   /  6hstatis  /
  data  text15   /  6htics    /
  data  text16   /  6houtput  /
  data  text17   /  6hsalvag  /
  data  text18   /  6he       /
  data  text19   /  6hsos     /
  if ( iprsup .ge. 1 ) write (lunit6, 1001)  kill
1001 format (  25h top of "subr55".  kill =,  i6  )
  if ( isw  .eq.  -3344 )  go to 6740
  if ( kill .le. 200 )  go to 4092
  n1 = kill - 200
  go to (6201 , 6202, 6203, 6204, 6205, 6206, 6207, 6208, 6209, 6210, 6211 , 6212, 6213, 6214, 6215, 6216, 6217, 6218, 6219, 6220, &
       6221 , 6222, 6223, 6224, 6225, 6226, 6227, 6228, 6229, 6230, 6231 , 6232, 6233, 6234, 6235, 6236, 6237, 6238, 6239, 6240, &
       6241 ),  n1
  go to 6540
6201 write (lunit6, 7201)  lstat(16)
7201 format (5x, 'The last-read data card is a request for the further (continued) solution of a previously-solved emtp data',/, &
          5x, "case.   The   'restart'   request specifies a permanent file in which is stored   /blank/   and   /label/ .",/, &
          5x, 'but EMTP dimensioning of the present program version is not identical to that for the version which created the',/, &
          5x, 'permanent file.   Specifically, the total length of   /label/   for the file-creating program was',  i8,  '   integer ')
  write (lunit6, 7301)  ltlabl
7301 format (5x,  'words, while the corresponding present figure is', i8,   ' .    Any such discrepancy is illegal.   As a',/, &
          5x, 'general rule, the user is counseled to use the same program version for both operations, thereby guaranteeing success. ')
  go to 6550
6202 continue
  go to 6550
6203 write (lunit6, 7203)
7203 format (5x, 'The data case now being solved involves one or more type-96 hysteretic inductors.   Fine.   However, no',/, &
          5x, 'steady-state phasor solution for initial conditions was requested by the user.   This combination is permitted',/, &
          5x, "only if the simulation begins as the continuation of a previously-halted run (with field  'tstart'  of the",/, &
          5x, 'floating-point miscellaneous data card punched positive).   The data case under consideration does not satisfy these',/, &
          5x, 'restrictions, so solution shall be stopped. ')
  go to 6550
6204 write (lunit6, 7204)  flstat(14), flstat(15)
7204 format (5x, 'The last-inputted EMTP component was a type-96 hysteretic inductor.   Columns  27-32  and  33-38  of the',/, &
          5x, "branch card are to be punched with  'i-steady'  and   'psi-steady' ,   respectively.   But values",  e14.4,  '   and',/, &
          5x,  e14.4, '   were read for these two variables, which represents a point in the current-flux plane that lies outside',/, &
          5x, 'the user-defined major hysteresis loop.   the EMTP does not allow such sloppiness (even though the ratio may be',/, &
          5x,  'correct).   Define a point within the loop, and try again. ')
  go to 6550
6205 write (lunit6, 7205)  flstat(16), flstat(17)
7205 format (5x, 'The last-inputted EMTP component was a type-96 hysteretic inductor.   Columns  39-44  of the branch card are',/, &
          5x, 'to be punched with a residual (remnant) flux value.   But a value of',  e14.4,   '   was read for this, which',/, &
          5x, 'exceeds (in absolute value) the flux of the user-inputted major hysteresis loop at zero current.   This latter flux',/, &
          6 5x,  'value is',  e14.4,   ' .   The result is a current-flux point which lies outside the major hysteresis loop, which',/, &
          5x, 'is impossible.   Punch a legal residual flux value in columns  39-44 ,   and try again. '   )
  go to 6550
6206 write (lunit6, 7206)
7206 format (5x, "The user is trying to combine  'statistics'  or  'systematic'  results using the   'tabulate energization results'",/, &
          5x, 'feature.   But not all of the partial results are compatible.   Files previously attached and read have the',/, &
          5x, 'following characteristic parameters ... ')
  write (lunit6, 7306)  ( lstat(i), i=11, 13), bus2
7306 format (10x,  i5, ' = ntot    (number of electric network nodes)',/, &
          10x, i5, ' = nstat   (number of output variables) ',/, &
          10x, i5, ' = kswtch  (number of switches)',/, &
          9x, a6, ' = bus(ntot)  (name of last network node). ')
  write (lunit6, 7406)  lstat(17)
7406 format (5x, 'On the other hand, the most recently attached file, number',  i5,  '   in order of user specification, has',/, &
          5x, 'the following different characteristics ... ')
  write (lunit6, 7306)  ntot, lstat(14), kswtch, bus1
  write (lunit6, 7506)
7506 format (5x, 'never try to combine results which belong to differently-structured problems, as in this data case. ')
  go to 6550
6207 write (lunit6, 7207)  tmax, tenerg
7207 format (5x, 'This data case has  'statistics'  switches, but it is highly improbable that any would ever close.   The',/, &
          5x, "termination time  'tmax'  of the simulation equals", e14.4, ' ,   while all random switch-closing times',/, &
          5x, 'exceed',  e14.4, '   seconds with  3*sigma  probability.   Such a waste of computer resources will not be tolerated.',/, &
          5x,  "Either increase  'tmax'  beyond this latter figure, or appropriately decrease the closing times. ")
  go to 6550
6208 continue
  go  to  6550
6209 write (lunit6, 7209)  epsiln, lstat(17)
7209 format (5x, 'The jacobian matrix for a Newton solution of zinc-oxide arresters has been found to be singular.',/, &
          5x, "the tolerance  'epsiln'  equals", e12.3,  ' ,   while the iteration count is', i5,   ' . ')
  go to 7412
6210 write(lunit6,7210)  lstat(14)
7210 format (5x, 'The initialization of a saturated synchronous machine has failed to converge. the machine in question',/, &
          5x, 'had the following number', 2x, i6 )
  go to 6550
6211 write(lunit6,7211) lstat(14),flstat(13),flstat(14),flstat(15)
7211 format (5x, 'The program was inputting data for synchronous machine no.', i8, 'a non-positive set of saturation data', /, &
          5x, 'for one of the axis has been detected.   The read in data follow below this line ........',/, &
          10x, 3e20.8, /, 5x, "in a case of an unsaturated s.m. this kill-code is caused by a nonspecified value of parameter 'agline' ".)
  go to 6550
6212 write (lunit6, 7212)  maxzno, epstop, flstat(14)
7212 format (5x,  'a rigorous solution for one or more zinc-oxide arresters has failed.   up to',  i5, "   iterations (variable 'maxzno')",/, &
          5x, 'were allowed to drive the current residuals below',   e12.4, "   amperes (tolerance 'epstop').",/, &
          5x, 'but',   e13.4, '   amperes remain for a problem equation, so the Newton iteration has diverged. '    )
7412 write (lunit6, 7512)  lstat(13), bus1, bus2
7512 format (5x,  'by way of component identification,', &
          ' there are',   i5, '   coupled elements which are being solved simultaneously, with',/, &
          5x,  'the first of these (in order of data input) connecting node  ',  "'", a6, "'", '  to node  ',  "'", a6, "'", ' .   The first element is ')
  write (lunit6, 7612)  lstat(15), lstat(14), lstat(16), t
7612 format (5x,  'located in row',  i5,'   of the nonlinear element table,   while the last is in row number', i5,   ' .',/, &
          5x,   'a rank of',  i5, '   exists for  (zthev) ,   and the simulation time is',  e13.5,  ' sec. ')
  write (lunit6, 7712)
7712 format (5x,  'Possible ameliorative actions include a decrease in time-step size "deltat", or an increase in the iteration',/, &
          5x,  'limit "maxzno", or an increase in the divergence tolerance "epstop" . ')
  go to 6550
6213 write (lunit6, 7213)  lstat(15), lstat(16)
7213 format (5x,  'While reading  zno  arrester data cards, a structural (numbering) defect was found.   This is for',/, &
          5x,  'nonlinear element number',  i5, '   which corresponds to arrester number',  i5, ' . ')
  write (lunit6, 8213)  lstat(17), lstat(16)
8213 format (5x,  ' The read-in identification number', i8,   3x,  'does not agree with the arrester number equal to',  i8,  ' . ' )
  go to 6550
6214 write (lunit6, 7214)  bus1
7214 format (5x,  'The EMTP is in the process of reading the data associated with the  tacs  device',/, &
          5x, ' identified by the 6-character (output) name ', "'", a6, "'", ' . ' )
  write (lunit6, 7314)
7314 format (5x, 'This is a type-58 device defined by the following transfer function:',//, &
          10x, 'gain / ( d0 +  d1 * s  )',//, &
          5x, 'the denominator of this function is presently found to have a value of   0.0  ,  thus creating',/, &
          5x, 'a singularity in the system.   In effect, this denominator is internally transformed ' )
  write (lunit6, 7414)  deltat
7414 format (5x, 'by the trapezoidal rule of implicit integration into the expression:',//, &
          10x, '( d0  +  d1 * 2.0 / deltat  )',//, &
          5x, 'with the value of deltat = ', e14.6 ,/, &
          5x, 'correct this situation by changing either  d0,  d1,  or  deltat . ')
  go to 6550
6215 write (lunit6, 7214) bus1
  write (lunit6, 7215)
7215 format (5x,  'This  type-60  if-device  recognizes  3  and only  3  separate input signals.',//, &
          5x,  'of the  5  fields available for defining the inputs',/, &
          10x, 'each one of the first three must be non-blank  (columns 11 - 33 ),/, &
          10x,  33hand the two remaining fields must be left blank  ( columns 35 - 49 ) ' )
  go to 6550
6216 write (lunit6, 7214) bus1
  write (lunit6, 7216)
7216 format (5x,  'This  type-61  device  selects as output  one of the possibly  8  connected inputs',/, &
          5x, "depending on the value of another tacs variable called 'selector signal'. ")
  write (lunit6, 7316)
7316 format (5x,  'However, the user has neglected to identify the name of the tacs variable that is to serve this purpose.',/, &
          5x, 'The user should specify this selector signal in the  6-character field of columns  75 - 80  . ' )
  go to 6550
6217 write (lunit6, 7214)  bus1
  write (lunit6, 7217)  lstat( 17)
7217 format (5x, 'This  type-',    i2, '  min/max  device  will identify either maxima or minima, depending on',/, &
          5x,'the numerical value read in columns 57 - 62  of the data card. ')
  write (lunit6, 7317)  flstat( 14)
7317 format (5x,  'this value must be typed as either',/, &
          10x, '   +1.0  to indicate that a maximum is to be calculated,',/, &
          10x, 'or -1.0   -     -      -    minimum    -     -     -   .',//, &
          5x, 'The present value was read as ', f13.6 )
  go to 6550
6218 write (lunit6,7218)  bus1
7218 format (5x,  'The program was reading the user-defined free-format fortran expression',/, &
          5x, 'for the tacs variable identified by the  6-character name ', "'", a6, "'", ' ,',/, &
          5x, 'when the following illegal situation was detected: ')
  i1 = lstat( 17)
  if ( i1 .gt. 6 )  go to 62180
  go to ( 62181, 62182, 62183, 62184, 62185, 62186), i1
62180 i1 = i1 - 6
  go to ( 62187, 62188, 62189, 62190, 62191, 62192, 62193), i1
62181 write (lunit6, 72181)
72181 format (10x, 'A parenthesis was opened and never closed. ')
  go to 6550
62182 write (lunit6, 72182)
72182 format (10x, 'This expression contains no argument. ')
  go to 6550
62183 write (lunit6, 72183)
72183 format (10x, 'Attempt to close a parenthesis that had not been opened. ')
  go to 6550
62184 write (lunit6, 72184)  bus2
72184 format (10x, 'The operator ', "'", a6, "'", ' is the last element of this fortran expression.',/, &
           10x, "Isn't there an argument missing ... ")
  go to 6550
62185 write (lunit6, 72185)  bus2, bus3
72185 format (10x, 'The two following arguments cannot be adjacent:',/, &
           10x, "'", a6, "'", '  ', a6, "'" )
  go to 6550
62186 write (lunit6, 72186)  bus2
72186 format (10x, 'The first element of this expression was read as ', "'", a6, "'", ' . ', /, &
           10x, 'Can it really be ... ')
  go to 6550
62187 write (lunit6, 72187)  bus2
72187 format (10x, "Missing  '('  after function ", "'", a6, "' ")
  go to 6550
62188 write (lunit6, 72188)
72188 format (10x, 'Please break this monstruously large expression into smaller sections. ' )
  go to 6550
62189 write (lunit6, 72189)  bus2, bus3
72189 format (10x, 'This expression is not homogeneous.',/, &
           10x, 'The two operators upon which this condition was detected are',/, &
           15x, "'", a6, "'", ' and ', "'", a6, "'". ' . ')
  go to 6550
62190 write (lunit6, 72190) lstat( 16)
72190 format (10x, 'The numerical argument ending in column  ',  i2,/, &
           10x, 'is more than  20  characters long. ')
  go to 6550
62191 write (lunit6, 72191)  lstat( 16)
72191 format (10x, 'The alphanumeric argument ending in column  ', i2, /, &
           10x, 'is more than  6 characters long. ')
  go to 6550
62192 write (lunit6, 72192)  lstat( 16)
72192 format (10x, 'Unrecognizable logical operator near column  ', i2 )
  go to 6550
62193 write( lunit6, 72193)  lstat( 16)
72193 format( 10x, 'The numerical argument ending in column ',  i2,/, &
           10x, 'contains more than one decimal point. ')
  go to 6550
6219  write (lunit6, 7213)  lstat(16), lstat(15)
  write (lunit6, 7219)  flstat(15)
7219 format (5x,  'A non-positive reference voltage equal to',  e13.4, '   was specified by the user. ')
  go to 6550
6220 write (lunit6, 7220) last, lstat(15), ibr
7220 format (5x, ' Overflow of steady-state table space.   List 23 tables are sized at',  i6, '   words,   which is insufficient for',/, &
           5x, 'even just the formation of  (y),  to say nothing of later triangularization.   Overflow has occurred after only',  i5,/, &
           5x, 'branches have been processed, out of a total of',  i5,   ' . '  )
  go to 6550
6221 write (lunit6, 7221)
7221 format (5x, ' The number of phases in this line is larger than the temporary limit of 10 for K. C. Lee modeling. ')
  go to 6550
6222 write (lunit6, 7222)  lstat(14), bus1, bus2, lstat(15)
7222 format (5x, 'The steady-state solution is for two or more frequencies which are not separated.   Trouble was spotted at branch number',  i5,/, &
          5x, 'which connects bus  "',  a6, '"  with bus  "',  a6,  '" .   One source of conflict is in row',  i5 )
  go to 6550
6223 write (lunit6, 7223)  lstat(14), bus1, bus2, lstat(15), lstat(16)
7223 format (5x, 'The steady-state solution is for two or more frequencies which are not separated.   Trouble was spotted at switch number',  i5   ,/, &
          5x,  'which connects bus  "',  a6, '"  with bus  "',  a6,  '" .   The left is excited by source number',  i5,/, &
          5x, 'while the right is excited by source number',  i6,  ' . ' )
  go to 6550
6224 write (lunit6, 7224)
7224 format ( 5x,  'The last-read switch card has both names identical.   The switch is closed on itself, and has no use. ')
  go to 6550
6225 write(lunit6, 7225)  lstat(14), lstat(15)
7225 format( 5x, 'The user has overflowed storage within the cable connstants supporting program.   For the current program',/, &
          5x,  'version, one is limited to cases having not over', i4 , ' conductors or ', i4, ' cables( the storage for a three',/, &
          5x, 'phase transmission line is equal to 5 cables if it has 2 ground wires.  Storage for the arrays in question has',/, &
          5x, 'been optimally (and dynamically) allocated so as to use as much of the EMTP core storage as is available.   It thus',/, &
          5x, 'is not the fault of the cable-constants supporting program itself, but rather of the overall EMTP dimensioning, that',/, &
          5x, 'this data case must be rejected.   Go procure or make another program version which has more total tabular '   )
  write (lunit6, 7182)
7182 format ( 5x, 'storage space, and try the job over again.   Remember, the cable constants calculation requires the storage',/, &
          5x, 'of full matrices, so memory requirements go up approximately as the square of the number of conductors. ')
  go to 6550
6226 continue
6227 continue
6228 continue
6229 continue
6230 continue
6231 continue
6232 continue
6233 continue
6234 continue
6235 continue
6236 continue
6237 continue
6238 continue
6239 continue
6240 continue
6241 continue
6540 write(lunit6,7540) kill, lastov
7540 format(/, 'Invalid  kill  code', i5,  5x, 'lastov =', i4  ,/, 1x  )
6550 if ( ipntv(1)  .ne.  -8888 )   go to 1429
  kill = ipntv(3) + 1
  if ( kill  .le.  ipntv(2) )   go to 1417
  kill = 0
  lastov = nchain
  nchain = 1
  if ( iprsup  .ge.  1 )write ( lunit6, 4568 )   kill, ipntv(2)
4568 format (' Exit module  "subr55".  Kill, ipntv(2) =',  2i6  )
  go to 9000
1417 write (lunit6, 1424)  kill
1424 format ( /,  28h message of kill-code number,  i4,  1h.  )
  lastov = nchain
  nchain = 51
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568)  kill
  go to 9000
1429 if ( jflsos  .eq.  0 )   go to 1430
  if ( nenerg  .eq.  0 )   go to 1430
  !     write bounding records in the case of statistics salvage (sos).
  d7 = -9999.
  n15 = kswtch + 1
  write (lunit3)  ( d7, j=1, n15 )
  n15 = lstat(32)
  write (lunit9)  ( d7, j=1, n15 )
  call statsv
1430 if ( m4plot .ne. 1 )  go to 6645
  call spying
6645 if( kill .gt. 1 )  go to 4092
  write (lunit6, 3391)  lstat
3391 format (/, ' For   kill = 1   error stops, program maintenance may sometimes wish to inspect the contents of error-',/, &
          " interface vectors  'lstat'  and  'flstat' .   These follow ....",//, "vector  'lstat'",/, ( 1x, 10i13 ) )
  write (lunit6, 3392)  flstat
3392 format ( /,   17h vector  'flstat'   ,/, &  ( 1x, 10e13.4 ) )
  n1 = lstat(16)
  write(lunit6, 4000 )  n1
4000 format (/, ' of course maybe the user would like some suggestions as to why the table in question (list number , i2, 1h)', /, &
          ' has overflowed.   If so, read on, good buddy.   the EMTP has a long-established policy of meritorious and laudable',/, &
          ' cooperation in the form of crystal-clear diagnostic messages, such as the following ..... ')
  if ( n1  .eq.  99 )   go to 4499
  go to (4001, 4002, 4003, 4004, 4005, 4006, 4007, 4008, 4009, 4010, 4011, 4012, 4013, 4014, 4015, 4016, 4017, 4018, 4019, &
       4020, 4021, 4022, 4023, 4024, 4025, 4026, 4027, 4028, 4029),  n1
4001 write(lunit6, 4101)
4101 format (5x, "Network nodes are of course defined by the user's branch and switch cards, by the names which identify the ",/, &
          5x, "two ends of the element (fields  'bus1'  and  'bus2'  of the data card, columns 3-14).   In addition, there are  ",/, &
          5x, 'several less-obvious ways in which nodes are added to the bus table ....',/, &
          8x, '1.  Switched-r elements (type-92) and switched-l elements (type 93) each create one internal node for every',/, &
          12x,'such element. ')
      write(lunit6, 4201)
4201  format( 8x,  '2.  Ground (blank node name) always has an entry in the bus list.',/, &
           8x, "3.  Each single-phase saturable transformer component adds one node (node name  'bustop' ,  columns 39-44 of the",/, &
           12x, "card bearing the request word 'transformer ' in columns 53-14).",/, &
           8x, "4.  Each three-phase saturable transformer component adds one node (node name 'bus3ph', read from columns 27-32",/, &
           12x, "of the card bearing the request word 'transformer three phase ' in columns 3-26).   This is a 4-th word, in " )
      write(lunit6, 4301)
4301  format( 12x,  75haddition to the 3 which are added under point 3, for a 3-phase transformer.   )
      go to 4099
4002  write(lunit6, 4102)
4102  format( 5x, 107hnetwork branches are of course defined directly by the user as he inputs branch data.   yet the counting of   ,/, &
           5x, 114hentries in this linear branch table has some subtle points which are worthy of the following detailed comment ....   ,/, &
           8x, 109h1.  true nonlinear elements (type codes 92 or 93) or the continuous time-varying resistance element (type 91)    ,/, &
           12x, 112hnever contribute to the linear branch table.   these elements are pulled outside of the network, and are handled   ,/, &
           12x,  16hby compensation.     )
      write(lunit6, 4202)
4202  format( 8x, 108h2.  switched-resistance elements (type-92) each contribute one entry to the linear branch table.   switched-    ,/, &
           12x,  60hinductance elements (type 93) contribute two entries apiece.    ,/, &
           8x, 109h3.  each type-99 pseudo-nonlinear resistance element contributes an entry, unless it is paralleled by another   ,/, &
           12x, 107hlinear branch.   these added very-high-impedance branches show up with a card image and data interpretation   ,/, &
           12x,  60halmost as though the user had inputted the resistor himself.     )
      write(lunit6, 4302)
4302  format( 8x, 113h4.  each n-winding single-phase saturable transformer component always internally sets up   2(n-1) + 1   branches,/, &
           12x, 110hfor everything but the magnetizing branch.   for the latter, a nonzero magnetizing resistance (field  'rmag' ,     ,/, &
     4 12x, 109hcolumns 45-50 of the transformer request card) will add an entry, as will a saturation characteristic defined   ,/, &
     6 12x, 107hby exactly one point.   a 3-phase saturable-transformer component contributes only in that it consists of 3    ,/, &
     8 12x,  36hsingle-phase units as just detailed.    )
      go to 4099
 4003 write(lunit6, 4103)
 4103 format (5x, 107hthe r, l, c tables store floating-point resistance, inductance, and capacitance parameter values associated   ,/, &
           5x, 113hwith lumped-parameter elements.   although such values are inputted on branch cards (mostly), the user should not   ,/, &
           5x, 109hconfuse the present parameter storage with the branch-table storage of list 2.   contributions to the present        ,/, &
           5x, 113hlist-3 table by different EMTP components are as follows, assuming no usage of the reference-branch or reference-   ,/, &
           5x,  20hcomponent idea .....        )
      write (lunit6, 4203)
4203  format (8x,  61h1.  each uncoupled series r-l-c branch contributes one entry.     ,/, &
           2 8x, 109h2.  each n-phase pi-circuit component, or each n-phase mutually-coupled r-l component, contributes   n(n+1)/2       ,/, &
           4 12x,   8hentries.     ,/, &
           5 8x, 108h3.  each single-phase n-winding saturable-transformer component contributes   3n-2   entries, at least.   if    ,/, &
           7 12x, 113hmagnetizing resistance  'rmag'  is used, add another entry.   if the transformer is actually linear, with finite-   ,/, &
           9 12x,  54hslope magnetization characteristic, add another entry. )
      write (lunit6, 4303)
4303  format (8x, 106h4.  a 3-phase saturable transformer has the aforementioned entries in the table for the three single-phase    ,/, &
           2 12x,  97htransformers which are sub-components of it.   in addition, there are always   3   extra entries.    ,/, &
           4 8x, 113h5.  if a network uses one or more type-99 pseudo-nonlinear resistance elements which is not paralleled by another   ,/, &
     6 12x,  99hlumped-parameter branch, one entry is added to the table (for all such elements, not for each one).     ,/, &
     8 8x,  81h6.  each switched-resistance element (type-92 switch card) contributes one entry.      )
      write (lunit6, 4403)
4403  format (8x,  83h7.  each switched-inductance element (type-93 switch card) contributes two entries.    ,/, &
     2 8x, 104h8.  each  type-16  source element (simplified  ac/dc  converter representation) contributes two entries.            ,/, &
     4 8x, 108h9.  each distributed-parameter transmission circuit contributes     n * (n+1) / 2    entries, where  'n'  is        )
      write (lunit6, 4503)
4503  format (12x, 109hthe number of phases of the line.   in case of such overflow, the  'present figure'   will not include these.  ,/, &
     2 5x, 110hwhere reference-branch or reference-component ideas are used, there generally is no contribution at all to the   ,/, &
     4 5x, 110hr, l, c tables.   in this case, the program simply makes reference to previously-stored (and hence previously-    ,/, &
     6 5x,  21hcounted) data values.     )
      go to 4099
4004  write(lunit6, 4104)
4104  format (5x, 105hcounting the number of entries in the source table (size of list 4) is quite simple, as per the following   ,/, &
           5x, 10hrules ....   ,/, &
           8x, 106h1.  each conventional source component (type code 1 through 14, punched in columns 1-2 of the source card)    ,/, &
           12x,  22hcontributes one entry.    ,/, &
           8x, 106h2.  each type-15 source component (the simplified ac/dc converter model, neglecting ripple on the dc side)    ,/, &
           12x,  24hcontributes two entries.     )
      write (lunit6, 4204)
4204  format (8x, 103h3.  each 3-phase dynamic synchronous-machine component (type codes 21, 22, 23 punched in columns 1-2 of    ,/, &
           12x,  52hconsecutive source cards) contributes three entries. ,/, &
           8x, 116h4.  each switched-resistance element (type code 92 punched in columns 1-2 of the switch card) contributes 2 entries.    )
      go to 4099
4005  write (lunit6, 4105)
4105  format (5x, 107hlist 5 ostensibly gives the size of the table-of-factors storage (l-u decomposition) for the triangularized  ,/, &
           2 5x, 115hreal equivalent nodal admittance matrix  (y)  of the time-step loop.   at each time-step, the real matrix equations   ,/, &
           4 5x, 116h (y)v = i   are solved for real node-voltage vector  v ,  by means of a repeat solution using the table of factors.    ,/, &
           6 5x, 114hbecause  (y)  is symmetric, only the upper-triangular factors (including the diagonal) are stored.   there is only    ,/, &
           8 5x, 107hone integer word and one floating-point word for each factor, it will be noted (see below).   node ordering     )
      if ( lstat(13)  .eq.  1 ) write (lunit6, 4805)  lstat(14), kpartb
4805  format (5x, 39hbeginning with "m32." versions,  list 7 storage of (ybb/ybc) is being destroyed,  and the full (y) is added,/, &
           5x,  39hto the bottom of list 5 (fills from the,39h bottom up).   but space ran out before, 39h the storage of (y) is finished.   only,/, &
           i8,  36h   rows are done,  out of a total of, i4,  42h ,   and factoring has not yet even begun. )
      if ( lstat(13)  .eq.  2 ) write (lunit6, 4905)  lstat(15), kpartb
4905  format (5x, 39hbeginning with "m32." versions,  list 7, 30h storage of (ybb/ybc) is being, 38h destroyed,  and the full (y) is added,/, &
           5x,  39hto the bottom of list 5 (fills from the, 35h bottom up).   the downward-growing, 37h factors spilled over onto (y) at row,/, &
           i8,  40h   of the triangularization,  whereas we, 24h must reach row kpartb =,  i4, 23h   to end successfully.     )
      write (lunit6, 4205)
4205  format (5x, 43hthe order of elimination (node renumbering), 37h is constrained only in that nodes of, 31h known voltage are forced last.   )
      go to 4099
4006  write (lunit6, 4106)
4106  format ( 5x, 108hswitches are completely straightforward, being defined only by switch cards.   one entry in the switch table   ,/, &
           5x, 112his created for every switch card, whether it is for an ordinary switch ( 'itype'  of columns 1-2 equal to zero),     ,/, &
           5x,  96ha switched resistance element ( 'itype' = 92), or a switched-inductance element ( 'itype' = 93).    )
      go to 4099
      !       ???????????   list 7 is presently unused   ????????????????
4007  write (lunit6, 4107)
4107  format (    8h unused    )
      go to 4099
4008  write (lunit6, 4108)
4108  format (5x, 108hpast-history points for distributed-parameter representation of transmission lines are stored in modal form,     ,/, &
           5x, 115halways.   each mode requires storage, where there are as many modes as there are coupled conductors (e.g., a double   ,/, &
           5x, 117hcircuit line has 6 modes.).   a constant-parameter (frequency-independent) mode contributes    tau/deltat    entries,   ,/, &
           5x, 111hwhere  'tau'  is the modal travel-time of the line,  'deltat'  is the time-step size, and the division involves    )
      write (lunit6, 4208)
4208  format (5x, 105hinteger truncation followed by the addition of unity.   for a frequency-dependent mode, more past-history   ,/, &
           5x, 114hthan this is needed, enough to perform the  a2(t)  convolution.   in the preceding formula, take  'tau'  to be the    ,/, &
           5x,  93htime  't2'  at which the exponential tail on  a2(t)  begins (typically 3 travel-times or so).     )
      go to 4099
4009  write (lunit6, 4109)
4109  format ( 5x,  86hentries in the nonlinear-element table are created by the following element types ....    ,/, &
           2 8x,  79h1.  piecewise-linear time-varying resistance elements  R(t) ,   branch-type 91.      ,/, &
           8x,  56h2.  true nonlinear  v-i  characteristic, branch-type 92.,/, &
           8x,  54h3.  true nonlinear inductance element, branch-type 93.,/, &
           8x,  71h4.  staircase time-varying resistance element  r(t) ,   branch-type 97.         )
      write (lunit6, 4209)
4209  format (8x,  56h5.  pseudo-nonlinear inductance element, branch-type 98.     ,/, &
           8x,  58h6.  pseudo-nonlinear  v-i  characteristic, branch-type 99.   ,/, &
           5x,  79hevery element falling into this classification contributes one entry to list 9.     )
      go to 4099
4010  write (lunit6, 4110)
4110  format (5x, 108hthis list-10 storage applies to all characteristics which are defined as pairs of coordinates, terminated by    ,/, &
           5x,    108ha  9999-card.   each pair of coordinates so seen on the input-data listing contributes one entry to list 10.   ,/, &
           5x, 111hbut note carefully the wording of this rule.   it is only the ones which are actually seen visually on the data   ,/, &
           5x, 113hlisting (use of the reference-branch procedure adds nothing to list 10, and will not be seen on the data listing.   )
      write (lunit6, 4210)
4210  format (/, 5x, 105ha second contributor to the list-10 storage requirement is the type-94 nonlinear element component (surge     ,/, &
           5x, 110harrester with current limiting gap).   each such surge arrester which does not use the reference-branch option        ,/, &
           5x, 115hadds  18  entries to the list-10 storage requirement.   for each surge arrester which does use the reference-branch     ,/, &
           5x,  86hprocedure, there is a contribution of  11  entries to the list-10 storage requirement.              )
      write (lunit6, 4310)
4310  format (/, 5x, 118hfinally, if you have zno surge arresters in the case, four addtional cells are required for each of the zno arresters.  )
      go to 4099
4011  write (lunit6, 4111)
4111  format (5x, 109hbranch-output quantities are generated by column-80 punches on branch cards and on switch cards.   each punch    ,/, &
           5x, 110hof  '1'  or  '2'  (branch current or branch voltage) contributes one entry to list 11.   punches of  '3'  (for   ,/, &
           5x, 107hbranch current and voltage) or  '4'  (for branch power and energy) contribute two entries each, to list 11.     ,/, &
           5x, 110hnode-voltage outputs which are specified individually, one at a time, ----- i.e., by punching 6-character node      )
      write (lunit6, 4211)
4211  format (5x, 110hnames in the  13a6  field of the node-voltage output-specification card ----- are likewise limited by list 11.   ,/, &
           5x, 111hif the user has requested the automatic output of every node voltage instead of this selective output (by means      ,/, &
           5x, 109hof a  '1'  punched in column 2 of the aforementioned card), this list-11 limit does not apply to node voltage  ,/, &
           5x,   8houtputs.    )
      go to 4099
4012  write (lunit6, 4112)
4112  format ( 5x,  35hsorry, no special advice available.     )
      go to 4099
4013  write (lunit6, 4113)
4113  format (5x, 110hevery continuously-transposed distributed-parameter transmission-line component (branch type-code  'itype'  of   ,/, &
           5x, 110hcolumns 1-2 equal to  -1,  -2,  etc.) represents a possible contribution to list 13.   each line mode which is   ,/, &
           5x, 113hmodelled as being frequency-dependent (variable  'ipunch'  of columns 53-54 equal to  -1 )  contributes one entry   ,/, &
           5x, 110hto list 13.   generally this will only be for the zero-sequence mode (the first card of the group), if at all.    )
      go to 4099
4014  write (lunit6, 4114)
4114  format (5x, 108hfrequency-dependent representation for a mode of a distributed-parameter transmission line is requested by a   ,/, &
           5x, 108hvalue of  -1  punched in field  'ipunch'  (columns 53-54) of the associated branch card.   assuming that the    ,/, &
           5x, 114hreference-branch procedure is not used, the input of weighting functions  a1(t)  and  a2(t)  follows.   the number    ,/, &
     6 5x, 109hof points on these input cards is irrelevant, and is in no way related to the size of list 14.   instead, the     )
      write (lunit6, 4214)
4214  format (5x, 108hlist-14 storage depends upon both the time-span of the weighting functions, and also upon the time-step size   ,/, &
           5x, 112h'deltat' ,  as follows.   let  't1'  be the time span from the nonzero beginning of  a1(t)  (at about one travel    ,/, &
           5x, 110htime) to where its exponential tail begins (typically about two travel times).   also, define  't2'  to be the    ,/, &
           5x, 109htime at which the exponential tail of  a2(t)  begins (typically about three travel times).   then the storage     )
      write (lunit6, 4314)
4314  format (5x, 102hrequirement in list 14 is given by the relation  np = (t1 + t2) / deltat  .     lines which use the     ,/, &
           5x,  60hreference-branch procedure require no list-14 storage, note.    )
      go to 4099
4015  write (lunit6, 4115)
4115  format (5x, 106hto perform the convolution associated with frequency-dependent modes of distributed-parameter transmission    ,/, &
           5x, 112hlines, modal past-history must be stored for both ends of the line.   whether the reference-branch procedure was   ,/, &
           5x, 109hused or not in no way modifies this requirement.   for every frequency-dependent mode, two cells are taken up    ,/, &
           5x,  11hin list 15.    )
      go to 4099
4016  go to 4012
4017  go to 4012
4018  go to 4012
4019  write (lunit6, 4119)
4119  format (5x, 109hdo not dispair, all is not lost (yet).   what has happened is that list  19  is inadequate for the tacs table  ,/, &
           5x, 112hsizes which were requested.   either the user specified these sizes explicitely himself using an  'absolute tacs     ,/, &
           5x, 115hdimensions'  card, or the EMTP supplied its own default set.   in either case, these absolute tacs table sizes will  ,/, &
           5x, 114hrequire a list-19 size as shown under the  'present figure' column in row  19 .   before simply redimensioning the   )
      write (lunit6, 4219)
4219  format (5x, 110hEMTP to provide such a list-19 figure, however, the user might try to more optimally divide the existing total ,/, &
           5x, 115hamong the different tacs tables, using either an  'absolute tacs dimensions'  card or a  'relative tacs dimensions'  ,/, &
           5x, 111hcard.   finally, because   kill = 122   provides much general information about tacs dimensioning, we shall now      ,/, &
           5x,  20hprint it as well....    )
      kill = 122
      lstat(17) = 0
      lstat(16) = 0
      lastov = nchain
      nchain = 51
      if ( iprsup  .ge.  1 ) write (lunit6, 4568)  kill
      go to 9000
4020  go to 4012
4021  go to 4012
4022  go to 4012
4023  go to 4012
4024  n9 = lcomp * lbus / ntot
      write (lunit6, 4124)  ncomp, n9
4124  format (5x,  30hthe present data case involves,  i4, 36h   phase compensation, which exceeds, 31h the effective program limit of, i4, 2h .   ,/, &
           5x,  34hthis latter figure is the limiting, 31h value of list 24 multiplied by, 30h  lbus/ntot  (the ratio of the     ,/, &
           5x,  30hmaximum number of buses to the, 33h actual number for this problem).   )
      write (lunit6, 4224)
4224  format (5x,  36hnote that the effective limit on the, 33h number of phases of compensation, 35h thus varies inversely with problem  ,/, &
           5x,  38hsize.  cut the size in half, and twice, 38h as many phases are available, without, 38h redimensioning with a larger list 24.  )
      go to 4099
4025  go to 4012
4026  if ( lstat(13)  .eq.  0 )   go to 4226
      write (lunit6, 4126)  lstat(13), lsiz26
4126  format (5x,  "The user's data includes a coupled, 16h branch group of,  i5, 33h   phases.   squaring this number, 20h exceeds  list 26  (,  i5,  3h ). ")
      go to 4099
4226  write (lunit6, 4326)
4326  format (5x,  40hlist 26 working vectors "volt", "volti",, 38h "voltk", "vim", and "volta"  are used, 32h in various ways.   this is one.     ,/, &
           5x,  36hif user does not request 50 or more,, 29h  and if he has trouble,  see, 33h program maintenance for details.    )
      go to 4099
4027  go to 4012
4028  go to 4012
4029  go to 4012
4499  write (lunit6, 4199)
4199  format (5x, 109hboth network node-renumbering (transient and also steady-state) and the steady-state phasor solution make use   ,/, &
           5x, 113hof three very large arrays which overlay most of the labeled-common storage space (the data of which is preserved    ,/, &
           5x, 111hon logical 4 during these calculations).   this is a dynamically-dimensioned table, then, which is sized to use   ,/, &
           5x, 107hall available space (perhaps 2/3 of labeled common).   in particular, this working area includes all of the    )
      write (lunit6, 4299)
4299  format (5x, 113hgenerally-large storage for lists 5 and 8.   increasing the dimensions of either of these two lists will directly,/, &
           5x, 105h(and without any loss) increase the size of list 99.   it might be mentioned that the steady-state phasor    ,/, &
           5x, 107hmanipulations (renumbering, solution) will almost always provide the limiting difficulty.   this is because    ,/, &
           5x, 110hsparsity of the steady-state phasor network is generally worse than for the time-step-loop network, due to the    )
      write (lunit6, 4399)
4399  format (5x, 104hdifference in treatment of distributed-parameter lines.   for steady-state solution, equivalent branches   ,/, &
           5x, "interconnect every terminal node of the line, while the two ends are disconnected by Bergeron's method for the" ,/, &
           5x, 110htime-step-loop network.   double-circuit (6-conductor) lines are particularly nasty in the steady-state, then,    ,/, &
           5x,  77hhaving 12 terminal nodes which are all interconnected by equivalent branches.    )
4099  write (lunit6, 4098)
4098  format ( /, 107h in order to effectively trade memory space among the different tables, one must know how many arrays there   ,/, &
           114h are in each table (effectively).   the following tabulation shows the effective multiplicity associated with each    ,/, &
           113h independent list ----- those lists whose lengths are under user  control by means of EMTP variable dimensioning.   )
      write (lunit6, 4096)
4096  format ( 5x,  30h-------------1----------------, 30h------------------------------, 30h------------------------------, 24h------------------------    )
      write (lunit6, 4095)  ( i, i=1, 25 )
4095  format ( 5x, 14hlist number  1, 25i4 )
      write (lunit6, 4096)
      write (lunit6, 4093)
4093  format ( 5x, 14hfloating pt. 1, 100h   6   5   3   6   1  12   2   2   8   3   1   4   8   1   2   2 0   6   1   1  24   2   1   #   *   1     )
      write (lunit6, 4097)
4097  format(5x, 14hinteger      1, 100h   4   7   0   2   1  10   0   0  11   0   3   0   4   0   0   1  10   2   0   0   0   0   0   0   0       ,/, &
           5x,  14htotal        1, 100h  10  12   3   8   2  22   2   2  19   3   4   4  12   1   2   1  16   3   1  24   2   1   #   *   1       )
      write (lunit6, 4096)
      write (lunit6, 4091)
4091  format ( 3x,  36h# --- used only for virtual machines, 38h (burroughs, prime, vax, apollo, etc.), 31h   others can ignore this list.   ,/, &
           3x,  31h* --- rather than count list 24, 29h itself, add the value to the, 32h floating-point and total counts, 19h for lists 1 and 6.    )
4092  write(lunit6, 5315)
5315  format( 1h  )
      write (lunit6, 5314)
5314  format ( 25x,   97hcaution.   be skeptical of above  'present figure'  entries, due to abnormal termination of case.     )
      write(lunit6, 5316)
5316  format( 132h -----------------------------------------------------------------------------------------------------------------------------------     )
      do  i=1, 2
6512     write(lunit6, 5320)
      end do
5320  format( 132h error/error/error/error/error/error/error/error/error/error/error/error/error/error/error/error/error/error/error/error/error/error     )
      write(lunit6, 5316)
      if ( kilsav  .eq.  92 )    go to 6767
      !     lunit5 = mtape
      write (*,*)  ' commented out  lunit5 = mtape.', '   set kill = 0.'
      kill = 0
      n6 = 0
      !     read input card using cimage
6740  call cimage
      n13 = kolbeg
      nright = -2
      kolbeg = 1
      read (unit = abuff(1), fmt = 6741) texcol
6741  format ( 80a1 )
      call freone ( d1 )
      nright = 0
      if ( nfrfld  .ne.  1 )   go to 6743
      if ( texta6(1)  .eq.  text5 )   go to 6754
      if ( texta6(1)  .eq.  text13 )   go to 6764
      if ( texta6(1)  .eq.  text19 )   go to 6771
6743  if ( nfrfld  .ne.  4 )   go to 6770
      if ( texta6(4)  .ne.  text4 )   go to 6770
      if ( texta6(3)  .ne.  text3 )   go to 6770
      if ( texta6(2)  .ne.  text2 )   go to 6760
      if ( texta6(1)  .ne.  text1 )   go to 6760
6754  write (kunit6, 6755)
6755  format(  37h+marker card preceding new data case.    )
      call interp
      if ( n6  .lt.  5 )   go to 6758
      n6 = n6 - 4
      write (lunit6, 6757)  n6
6757  format ( 51x,  27hend suppression of listing.,  i6, 22h  cards were unlisted.  )
6758  noutpr = 0
      kolbeg = -intinf
      lastov = nchain
      nchain = 1
      if ( iprsup  .ge.  1 ) write ( lunit6, 4568)  4568
      go to 9000
6760  if ( texta6(2)  .ne.  text12 )   go to 6770
      if ( texta6(1)  .ne.  text11 )   go to 6770
6764  write (kunit6, 6765)
6765  format(  38h+marker card following last data case.   )
      call interp
6767  kill = 9999
      lastov = nchain
      nchain = 31
      if ( iprsup  .ge.  1 ) write ( lunit6, 4568)  kill
      go to 9000
6770  if ( nfrfld  .ne.  5 )   go to 6773
      if ( texta6(1)  .ne.  text14 )   go to 6773
      if ( texta6(2)  .ne.  text15 )   go to 6773
      if ( texta6(3)  .ne.  text16 )   go to 6773
      if ( texta6(4)  .ne.  text17 )   go to 6773
      if ( texta6(5)  .ne.  text18 )   go to 6773
6771  if ( nenerg  .eq.  0 )   go to 6773
      if ( knt     .le.  1 )   go to 6773
      write (kunit6, 6772)
6772  format ( 32h+request for statistics salvage.  )
      call interp
      if ( jflsos  .gt.  0 )   go to 6773
      d7 = -9999.
      n15 = kswtch + 1
      write (lunit3)  ( d7, j=1, n15 )
      n15 = lstat(32)
      write (lunit9)  ( d7, j=1, n15 )
      if ( n13  .eq.  -intinf )   kolbeg = n13
      if ( kolbeg  .gt.  0 )   go to 1773
      read (unit = abuff(1), fmt = 1764) n1
1764  format ( 29x, i3 )
      go to 1778
1773  call freone ( d1 )
      n1 = d1
1778  if ( n1  .gt.  0 )   go to 1774
      if ( n1  .lt.  0 )   iprsup = 9
!     find random integer  'n1'  between zero and 999.
      call runtym ( d11, d12 )
      seed = seedy( tclock(1) )  +  1000. * ( d11 + d12 )
      n13 = alog1z(seed)  +  epsiln
      n13 = n13 - 2
      seed = seed / 10.**n13
      n1 = seed
 1774 n2 = n1 / 100
      n5 = n1 - 100 * n2
      n3 = n5 / 10
      n4 = n5 - 10 * n3
      write(lunit6, 1792)  n2, n3, n4
 1792 format ( 1h+,  34x,  1h',  3i1,  1h'  )
      lstat(14) = n2
      lstat(15) = n3
      lstat(16) = n4
      call statsv
      go to 6740
6773  if ( noutpr  .eq.  0 ) write (kunit6, 6775)
6775  format (   47h+card ignored in search for new-case beginning.   )
      n6 = n6 + 1
      if ( n6  .lt.  5 )   go to 6769
      if ( noutpr  .eq.  1 )   go to 6769
      noutpr = 1
      write (lunit6, 6768)
6768  format ( 51x, 45hbegin suppression of skipped-record printout. )
6769  go to 6740
9000  return
    end subroutine subr55
    !
    ! subroutine statsv.
    !
    subroutine statsv
      implicit real*8 (a-h, o-z), integer*4 (i-n)
      return
    end subroutine statsv
!  m99.9999      overlay ( finish, 99999 )
!
!    end of file: over55.for
!
