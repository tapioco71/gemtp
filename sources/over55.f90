!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over55.f90
!

!
! subroutine over55.
!

subroutine over55
  use blkcom
  implicit none
  integer(4) :: i
  integer(4) :: k, kilsav
  integer(4) :: ltacst
  real(8) :: d1, d2, d3, d4, d6, d7
  real(8) :: hmin
  !
  !  equivalence (moncar(4), isw)
  !
  integer(4), pointer :: isw => moncar(4)
  !
  if (iprsup  .ge.  1) write (unit = lunit(6), fmt = 4567) kill
4567 format (' Begin "over55".  kill =', i6)
  kilsav = kill
  if (kill .le. 0) go to 6633
  call subr55
6633 if (nchain .eq. 51) go to 99999
  if (nchain .eq. 31) go to 99999
  kill = 0
  if (kol132 .eq. 132) go to 6639
  write (unit = lunit(6), fmt = 6634) (lstat(i + 20), i = 1, 26)
6634 format (/, ' Actual list sizes for preceding solution:', /, '   size  1-10:', 10i6, /, '   size 11-20:', 10i6, /, '   size 21-on:', 10i6)
  go to 6645
6639 write (unit = lunit(6), fmt = 6554)
6554 format (/, ' Core storage figures for preceding data case now completed.  ---------------------------------------', 2x, 'present', 3x, 'program', /, ' a value of  -9999 indicates default, with no figure available.', 41x, 'figure', 5x, 'limit (name)')
  write (unit = lunit(6), fmt = 38021) lstat(21), lbus
38021 format (5x, 'Size list 1.   Number of network nodes.', 56x, 2i10, ' (lbus)')
  write (unit = lunit(6), fmt = 38022) lstat(22), lbrnch
38022 format (5x, 'Size list 2.   Number of network branches.', 53x, 2i10, ' (lbrnch)')
  write (unit = lunit(6), fmt = 38023) lstat(23), ldata
38023 format (5x, 'Size list 3.   Number of data values in R, L, C tables.', 40x, 2i10, ' (ldata)')
  write (unit = lunit(6), fmt = 38024) lstat(24), lexct
38024 format (5x, 'Size list 4.   Number of entries in source table.', 46x, 2i10, ' (lexct)')
  write (unit = lunit(6), fmt = 38025) ktrlsw(3), iprsov(36), lstat(25), lymat
38025 format (5x, 'Size list 5.   Storage for (Y) and triangularized (Y).', 6x, 'no. times =', i5, 3x, 'factors =', i5, 2x, 2i10, ' (lymat)')
  write (unit = lunit(6), fmt = 38026) ktrlsw(5), lstat(26), lswtch
38026 format (5x, 'Size list 6.   Number of entries in switch table.', 15x, 'no. flops =', i6, 14x, 2i10, ' (lswtch)')
  write (unit = lunit(6), fmt = 38027) maxbus, lsize7
38027 format (5x, 'Size list 7.   Number of total distinct alphanumeric (a6) program names', 24x, 2i10, ' (lsize7)')
  write (unit = lunit(6), fmt = 38028) lstat(28), lpast
38028 format (5x, 'Size list 8.   Number of past history points for distributed lines.', 28x, 2i10, ' (lpast)')
  write (unit = lunit(6), fmt = 38029) lstat(29), lnonl
38029 format (5x, 'Size list 9.   Number of nonlinear elements.', 51x, 2i10, ' (lnonl)')
  write (unit = lunit(6), fmt = 38030) lstat(30), lchar
38030 format (5x, 'Size list 10.  Number of points defining nonlinear characteristics.', 28x, 2i10, ' (lchar)')
  write (unit = lunit(6), fmt = 38031) lstat(31), lsmout
38031 format (5x, 'Size list 11.  Number of branch or selective-node-voltage outputs.', 29x, 2i10, ' (lsmout)')
  write (unit = lunit(6), fmt = 38032) lstat(32), lsiz12
38032 format (5x, 'Size list 12.  Number of output quantities (limited only when printing max absolute values).', 3x, 2i10, ' (lsiz12)')
  write (unit = lunit(6), fmt = 38033) lstat(33), lfdep
38033 format (5x, "Size list 13.  Number of 'weighting' frequency-dependent line modes.", 27x, 2i10, ' (lfdep)')
  write (unit = lunit(6), fmt = 38034) lstat(34), lwt
38034 format (5x, 'Size list 14.  Number of cells used to store freq.-dependence weighting functions.', 13x, 2i10, ' (lwt)')
  write (unit = lunit(6), fmt = 38035) lstat(35), ltails
38035 format (5x, 'Size list 15.  Number of cells used for exponential-tail line-history storage.', 17x, 2i10, ' (ltails)')
  write (unit = lunit(6), fmt = 38036) lstat(36), limass
38036 format (5x, 'Size list 16.  Total number of type-59 s.m. masses.', 44x, 2i10, ' (limass)')
  write (unit = lunit(6), fmt = 38037) lstat(37), lsyn
38037 format (5x, 'Size list 17.  Number of dynamic synchronous machines.', 41x, 2i10, ' (lsyn)')
  write (unit = lunit(6), fmt = 38038) lstat(38), maxpe
38038 format (5x, 'Size list 18.  Number of branch power-and-energy outputs.', 38x, 2i10, ' (maxpe)')
  write (unit = lunit(6), fmt = 38039) lstat(39), ltacst
38039 format (5x, 'Size list 19.  Floating-point working space for all tacs arrays.', 31x, 2i10, ' (ltacst)')
  !     ktab is in blkcom, so it can not be equivalenced to
  if (ktab .le. 0) go to 7272
  lstat(53) = lstat(63) - lstat(60) + lstat(53)
  lstat(56) = lstat(56) - lstat(58) + 3
  lstat(58) = ktab
  write (unit = lunit(6), fmt = 3632) (k, k = 1, 8), (lstat(k), k = 51, 58), (lstat(k), k = 61, 68)
3632 format (7x, 'TACS table no.', 8i10, /, 7x, 'present figure', 8i10, /, 7x, 'program limit', 8i10)
7272 write (unit = lunit(6), fmt = 38040) lstat(40), lfsem
38040 format (5x, 'Size list 20.  Recursive convolution parameter storage for non-copied branch components.', 7x, 2i10, ' (lfsem)')
  write (unit = lunit(6), fmt = 38041) lstat(41), lfd
38041 format (5x, 'Size list 21.  Total storage cells for modal-phase transformation matrices.', 20x, 2i10, ' (lfd)')
  write (unit = lunit(6), fmt = 38042) lstat(42), lhist
38042 format (5x, 'Size list 22.  Number of cells for convolution history.', 40x, 2i10, ' (lhist)')
  write (unit = lunit(6), fmt = 38071) lstat(43), lsiz23
38071 format (5x, 'Size list 23.  Giant arrays for renumbering and steady-state solution calculations.', 12x, 2i10, ' (lsiz23)')
  write (unit = lunit(6), fmt = 38044) ncomp, lcomp
38044 format (5x, 'Size list 24.  Number of phases of compensation, based on maximum nodes.', 23x, 2i10, ' (ncomp)')
  write (unit = lunit(6), fmt = 38045) lstat(45), lspcum
38045 format (5x, 'Size list 25.  Floating-point working space for  u.m.  arrays.', 33x, 2i10, ' (lspcum)')
  write (unit = lunit(6), fmt = 38046) lstat(46), lsiz26
38046 format (5x, 'Size list 26.  Square of maximum number of coupled phases.', 37x, 2i10, ' (lsiz26)')
6645 if (kol132 .eq. 132) write (unit = lunit(6), fmt = 38003)
38003 format (' Timing figures (decimal) characterizing case solution speed.  -------------------------------------', 4x, 'cp sec', 3x, 'i/o sec', 3x, 'sum sec')
  if (flstat(7) .ne. -9999.0d0) go to 2601
  !     special timing code for supporting programs.
  d6 = flstat(9) + flstat(1)
  d7 = flstat(10) + flstat(2)
  d1 = d6 + d7
  if (kol132 .eq. 132) write (lunit(6), 38010) d6, d7, d1
  if (kol132 .eq. 80) write (lunit(6), 6651) d6, d7, d1
6651 format (' Total case timing (cp, i/o, tot), sec:', 1x, 3f10.3)
  lastov = nchain
  nchain = 1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568) kill
4568 format (' Exit "over55".  kill =', i6)
  go to 99999
2601 d1 = flstat(1) + flstat(2)
  d4 = d1
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 38004) flstat(1), flstat(2), d1
  if (kol132 .eq. 80) write (unit = lunit(6), fmt = 6652) flstat(1), flstat(2), d1
6652 format (' Seconds for overlays  1-6  :', 3f9.3, '  --- (cp;  i/o;  tot)')
38004 format (5x, 'Data input, sorting, and renumbering (pre steady state stuff) .....', 28x, 3f10.3)
  d1 = flstat(3) + flstat(4)
  d4 = d4 + d1
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 38005) flstat(3), flstat(4), d1
38005 format (5x, 'Steady-state (s.s.) solution calculations .....', 48x, 3f10.3)
  if (kol132 .eq. 80) write (unit = lunit(6), fmt = 6653) flstat(3), flstat(4), d1
6653 format (' Seconds for overlays  7-12 :', 3f9.3)
  d1 = flstat(5) + flstat(6)
  d4 = d4 + d1
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 38006) flstat(5), flstat(6), d1
38006 format (5x, 'post-s.s. to pre-integration-setup calculations .....', 42x, 3f10.3)
  if (kol132 .eq. 80) write (unit = lunit(6), fmt = 6654) flstat(5), flstat(6), d1
6654 format (' Seconds for overlays 13-15 :', 3f9.3)
  d1 = flstat(7) + flstat(8)
  d4 = d4 + d1
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 38007) flstat(7), flstat(8), d1
38007 format (5x, 'Integration calculation (time in time-step loop) .....', 41x, 3f10.3)
  if (kol132 .eq. 80) write (unit = lunit(6), fmt = 6655) flstat(7), flstat(8), d1
6655 format (' Seconds for time-step loop :', 3f9.3)
  hmin = flstat(9) + flstat(10)
  d4 = d4 + hmin
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 38008) flstat(9), flstat(10), hmin
38008 format (5x, 'Computer time in plotting or statistics termination overlay .....', 30x, 3f10.3)
  if (kol132 .eq. 80) write (unit = lunit(6), fmt = 6656) flstat(9), flstat(10), hmin
6656 format (' Seconds after deltat-loop  :', 3f9.3)
  d1 = flstat(11) + flstat(12)
  d4 = d1 + d4
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 38009) flstat(11), flstat(12), d1
38009 format (5x, "'deltat'-change restart time  .......", 58x, 3f10.3, /, 101x, '-----------------------------')
  d2 = 0.0d0
  d3 = 0.0d0
  do i = 1, 12, 2
     d2 = d2 + flstat(i)
     d3 = d3 + flstat(i + 1)
  end do
  if (kol132 .eq. 132) write (unit = lunit(6), fmt = 38010) d2, d3, d4
38010 format (93x, 'totals ', 3f10.3, //, 1x)
  if (kol132 .eq. 80) write (unit = lunit(6), fmt = 6658) d2, d3, d4
6658 format (29x, '-------------------------', /, 20x, 'totals  :', 3f9.3)
  if (isw .ne. 4444) go to 6673
  isw = -3344
  call subr55
6673 lastov = nchain
  nchain = 1
  kill = 0
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568) kill
99999 return
end subroutine over55

!
! subroutine a4012.
!

subroutine a4012
  use blkcom
  implicit none
  write (unit = lunit(6), fmt = 4112)
4112 format (5x, 'Sorry, no special advice available. ')
end subroutine a4012

!
! subrotuine a7214.
!

subroutine a7412
  use blkcom
  implicit none
  write (unit = lunit(6), fmt = 7512) lstat(13), bus1, bus2
7512 format (5x, 'by way of component identification, there are', i5, '   coupled elements which are being solved simultaneously, with', /, 5x, 'the first of these (in order of data input) connecting node  ',  "'", a6, "'", '  to node  ',  "'", a6, "'", ' .   The first element is ')
  write (unit = lunit(6), fmt = 7612) lstat(15), lstat(14), lstat(16), t
7612 format (5x,  'located in row',  i5,'   of the nonlinear element table,   while the last is in row number', i5,   ' .', /, 5x,   'a rank of',  i5, '   exists for  (zthev) ,   and the simulation time is',  e13.5,  ' sec. ')
  write (unit = lunit(6), fmt = 7712)
7712 format (5x,  'Possible ameliorative actions include a decrease in time-step size "deltat", or an increase in the iteration', /, 5x,  'limit "maxzno", or an increase in the divergence tolerance "epstop" . ')
end subroutine a7412

!
! subroutine subr55.
!

subroutine subr55
  use blkcom
  use tracom
  use bcdtim
  use strcom
  use random
  use freedom
  implicit none
  !  equivalence (moncar(1), knt), (moncar(4), isw)
  !  equivalence (moncar(10), mtape)
  character(8) :: text1, text2, text3, text4, text5
  character(8) :: text11, text12, text13, text14, text15
  character(8) :: text16, text17, text18, text19
  integer(4) :: i, i1, j, kilsav
  integer(4) :: n1, n2, n3, n4, n5, n6, n9, n13, n15
  real(8) :: d1, d7, d11, d12
  real(8) :: seed
  !
  integer(4), pointer :: isw => moncar(4)
  integer(4), pointer :: knt => moncar(1)
  !  integer(4), pointer :: mtape => moncar(10)
  !
  data text1  / 'begin ' /
  data text2  / 'new   ' /
  data text3  / 'data  ' /
  data text4  / 'case  ' /
  data text5  / 'bndc  ' /
  data text11 / 'end   ' /
  data text12 / 'last  ' /
  data text13 / 'eldc  ' /
  data text14 / 'statis' /
  data text15 / 'tics  ' /
  data text16 / 'output' /
  data text17 / 'salvag' /
  data text18 / 'e     ' /
  data text19 / 'sos   ' /
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 1001) kill
1001 format (' Top of "subr55".  kill =', i6)
  if (isw .eq. -3344) go to 6740
  if (kill .le. 200) go to 4092
  n1 = kill - 200
  select case (n1)
  case (1)
     write (unit = lunit(6), fmt = 7201) lstat(16)
7201 format (5x, 'The last-read data card is a request for the further (continued) solution of a previously-solved emtp data',/, 5x, "case.   The   'restart'   request specifies a permanent file in which is stored   /blank/   and   /label/ .",/, 5x, 'but EMTP dimensioning of the present program version is not identical to that for the version which created the',/, 5x, 'permanent file.   Specifically, the total length of   /label/   for the file-creating program was', i8, '   integer ')
     write (unit = lunit(6), fmt = 7301) ltlabl
7301 format (5x,  'words, while the corresponding present figure is', i8,   ' .    Any such discrepancy is illegal.   As a',/, 5x, 'general rule, the user is counseled to use the same program version for both operations, thereby guaranteeing success. ')
     go to 6550

  case (2)
     go to 6550

  case (3)
     write (unit = lunit(6), fmt = 7203)
7203 format (5x, 'The data case now being solved involves one or more type-96 hysteretic inductors.   Fine.   However, no',/, 5x, 'steady-state phasor solution for initial conditions was requested by the user.   This combination is permitted',/, 5x, "only if the simulation begins as the continuation of a previously-halted run (with field  'tstart'  of the", /, 5x, 'floating-point miscellaneous data card punched positive).   The data case under consideration does not satisfy these', /, 5x, 'restrictions, so solution shall be stopped. ')
     go to 6550

  case (4)
     write (unit = lunit(6), fmt = 7204) flstat(14), flstat(15)
7204 format (5x, 'The last-inputted EMTP component was a type-96 hysteretic inductor.   Columns  27-32  and  33-38  of the', /, 5x, "branch card are to be punched with  'i-steady'  and   'psi-steady' ,   respectively.   But values",  e14.4,  '   and', /, 5x,  e14.4, '   were read for these two variables, which represents a point in the current-flux plane that lies outside', /, 5x, 'the user-defined major hysteresis loop.   the EMTP does not allow such sloppiness (even though the ratio may be', /, 5x,  'correct).   Define a point within the loop, and try again. ')
     go to 6550

  case (5)
     write (unit = lunit(6), fmt = 7205) flstat(16), flstat(17)
7205 format (5x, 'The last-inputted EMTP component was a type-96 hysteretic inductor.   Columns  39-44  of the branch card are', /, 5x, 'to be punched with a residual (remnant) flux value.   But a value of',  e14.4,   '   was read for this, which', /, 5x, 'exceeds (in absolute value) the flux of the user-inputted major hysteresis loop at zero current.   This latter flux', /, 6 5x,  'value is',  e14.4,   ' .   The result is a current-flux point which lies outside the major hysteresis loop, which', /, 5x, 'is impossible.   Punch a legal residual flux value in columns  39-44 ,   and try again. ')
     go to 6550

  case (6)
     write (unit = lunit(6), fmt = 7206)
7206 format (5x, "The user is trying to combine  'statistics'  or  'systematic'  results using the   'tabulate energization results'", /, 5x, 'feature.   But not all of the partial results are compatible.   Files previously attached and read have the', /, 5x, 'following characteristic parameters ... ')
     write (unit = lunit(6), fmt = 7306) (lstat(i), i = 11, 13), bus2
7306 format (10x,  i5, ' = ntot    (number of electric network nodes)', /, 10x, i5, ' = nstat   (number of output variables) ', /, 10x, i5, ' = kswtch  (number of switches)', /, 9x, a6, ' = bus(ntot)  (name of last network node). ')
     write (unit = lunit(6), fmt = 7406) lstat(17)
7406 format (5x, 'On the other hand, the most recently attached file, number', i5, '   in order of user specification, has', /, 5x, 'the following different characteristics ... ')
     write (unit = lunit(6), fmt = 7306) ntot, lstat(14), kswtch, bus1
     write (unit = lunit(6), fmt = 7506)
7506 format (5x, 'never try to combine results which belong to differently-structured problems, as in this data case. ')
     go to 6550

  case (7)
     write (unit = lunit(6), fmt = 7207) tmax, tenerg
7207 format (5x, "This data case has  'statistics'  switches, but it is highly improbable that any would ever close.   The", /, 5x, "termination time  'tmax'  of the simulation equals", e14.4, ' ,   while all random switch-closing times', /, 5x, 'exceed',  e14.4, '   seconds with  3*sigma  probability.   Such a waste of computer resources will not be tolerated.', /, 5x,  "Either increase  'tmax'  beyond this latter figure, or appropriately decrease the closing times. ")
     go to 6550

  case (8)
     go to 6550

  case (9)
     write (unit = lunit(6), fmt = 7209) epsiln, lstat(17)
7209 format (5x, 'The jacobian matrix for a Newton solution of zinc-oxide arresters has been found to be singular.', /, 5x, "the tolerance  'epsiln'  equals", e12.3, ' ,   while the iteration count is', i5,   ' . ')
  !     go to 7412
     call a7412

  case (10)
     write (unit = lunit(6), fmt = 7210) lstat(14)
7210 format (5x, 'The initialization of a saturated synchronous machine has failed to converge. the machine in question', /, 5x, 'had the following number', 2x, i6 )
     go to 6550

  case (11)
     write (unit = lunit(6), fmt = 7211) lstat(14), flstat(13), flstat(14), flstat(15)
7211 format (5x, 'The program was inputting data for synchronous machine no.', i8, 'a non-positive set of saturation data', /, 5x, 'for one of the axis has been detected.   The read in data follow below this line ........', /, 10x, 3e20.8, /, 5x, "in a case of an unsaturated s.m. this kill-code is caused by a nonspecified value of parameter 'agline' . ")
     go to 6550

  case (12)
     write (unit = lunit(6), fmt = 7212) maxzno, epstop, flstat(14)
7212 format (5x,  'a rigorous solution for one or more zinc-oxide arresters has failed.   up to',  i5, "   iterations (variable 'maxzno')", /, 5x, 'were allowed to drive the current residuals below',   e12.4, "   amperes (tolerance 'epstop').", /, 5x, 'but',   e13.4, '   amperes remain for a problem equation, so the Newton iteration has diverged. ')
     call a7412
     ! 7412 write (unit = lunit(6), fmt = 7512) lstat(13), bus1, bus2
     ! 7512 format (5x, 'by way of component identification, there are', i5, '   coupled elements which are being solved simultaneously, with', /, 5x, 'the first of these (in order of data input) connecting node  ',  "'", a6, "'", '  to node  ',  "'", a6, "'", ' .   The first element is ')
     !      write (unit = lunit(6), fmt = 7612) lstat(15), lstat(14), lstat(16), t
     ! 7612 format (5x,  'located in row',  i5,'   of the nonlinear element table,   while the last is in row number', i5,   ' .', /, 5x,   'a rank of',  i5, '   exists for  (zthev) ,   and the simulation time is',  e13.5,  ' sec. ')
     !      write (unit = lunit(6), fmt = 7712)
     ! 7712 format (5x,  'Possible ameliorative actions include a decrease in time-step size "deltat", or an increase in the iteration', /, 5x,  'limit "maxzno", or an increase in the divergence tolerance "epstop" . ')
     go to 6550

  case (13)
     write (unit = lunit(6), fmt = 7213) lstat(15), lstat(16)
7213 format (5x,  'While reading  Zno  arrester data cards, a structural (numbering) defect was found.   This is for', /, 5x,  'nonlinear element number', i5, '   which corresponds to arrester number',  i5, ' . ')
     write (unit = lunit(6), fmt = 8213) lstat(17), lstat(16)
8213 format (5x,  ' The read-in identification number', i8,   3x,  'does not agree with the arrester number equal to',  i8,  ' . ' )
     go to 6550

  case (14)
     write (unit = lunit(6), fmt = 7214) bus1
7214 format (5x,  'The EMTP is in the process of reading the data associated with the  tacs  device', /, 5x, ' identified by the 6-character (output) name ', "'", a6, "'", ' . ' )
     write (unit = lunit(6), fmt = 7314)
7314 format (5x, 'This is a type-58 device defined by the following transfer function:', //, 10x, 'gain / ( d0 +  d1 * s  )', //, 5x, 'the denominator of this function is presently found to have a value of   0.0  ,  thus creating', /, 5x, 'a singularity in the system.   In effect, this denominator is internally transformed ')
     write (unit = lunit(6), fmt = 7414) deltat
7414 format (5x, 'by the trapezoidal rule of implicit integration into the expression:', //, 10x, '( d0  +  d1 * 2.0 / deltat  )', //, 5x, 'with the value of deltat = ', e14.6 ,/, 5x, 'correct this situation by changing either  d0,  d1,  or  deltat . ')
     go to 6550

  case (15)
     write (unit = lunit(6), fmt = 7214) bus1
     write (unit = lunit(6), fmt = 7215)
7215 format (5x,  'This  type-60  if-device  recognizes  3  and only  3  separate input signals.', //, 5x, 'of the  5  fields available for defining the inputs', /, 10x, 'each one of the first three must be non-blank  (columns 11 - 33 )', /, 10x, 'and the two remaining fields must be left blank  ( columns 35 - 49 ) ')
     go to 6550

  case (16)
     write (unit = lunit(6), fmt = 7214) bus1
     write (unit = lunit(6), fmt = 7216)
7216 format (5x,  'This  type-61  device  selects as output  one of the possibly  8  connected inputs', /, 5x, "depending on the value of another tacs variable called 'selector signal'. ")
     write (unit = lunit(6), fmt = 7316)
7316 format (5x, 'However, the user has neglected to identify the name of the tacs variable that is to serve this purpose.', /, 5x, 'The user should specify this selector signal in the  6-character field of columns  75 - 80  . ')
     go to 6550

  case (17)
     write (unit = lunit(6), fmt = 7214) bus1
     write (unit = lunit(6), fmt = 7217) lstat(17)
7217 format (5x, 'This  type-', i2, '  min/max  device  will identify either maxima or minima, depending on', /, 5x,'the numerical value read in columns 57 - 62  of the data card. ')
     write (unit = lunit(6), fmt = 7317) flstat(14)
7317 format (5x, 'this value must be typed as either', /, 10x, '   +1.0  to indicate that a maximum is to be calculated,', /, 10x, 'or -1.0   -     -      -    minimum    -     -     -   .', //, 5x, 'The present value was read as ', f13.6)
     go to 6550

  case (18)
     write (unit = lunit(6), fmt = 7218) bus1
7218 format (5x,  'The program was reading the user-defined free-format fortran expression', /, 5x, 'for the tacs variable identified by the  6-character name ', "'", a6, "'", ' ,', /, 5x, 'when the following illegal situation was detected: ')
     i1 = lstat(17)
     !if (i1 .gt. 6) go to 62180
     select case (i1)
     case (1)
        write (unit = lunit(6), fmt = 72181)
72181   format (10x, 'A parenthesis was opened and never closed. ')
        go to 6550

     case (2)
        write (unit = lunit(6), fmt = 72182)
72182   format (10x, 'This expression contains no argument. ')
        go to 6550

     case (3)
        write (unit = lunit(6), fmt = 72183)
72183   format (10x, 'Attempt to close a parenthesis that had not been opened. ')
        go to 6550

     case (4)
        write (unit = lunit(6), fmt = 72184) bus2
72184   format (10x, 'The operator ', "'", a6, "'", ' is the last element of this fortran expression.', /, 10x, "Isn't there an argument missing ... ")
        go to 6550

     case (5)
        write (unit = lunit(6), fmt = 72185) bus2, bus3
72185   format (10x, 'The two following arguments cannot be adjacent:', /, 10x, "'", a6, "'", '  ', a6, "'")
        go to 6550

     case (6)
        write (unit = lunit(6), fmt = 72186) bus2
72186   format (10x, 'The first element of this expression was read as ', "'", a6, "'", ' . ', /, 10x, 'Can it really be ... ')
        go to 6550

     case (7)
        write (unit = lunit(6), fmt = 72187) bus2
72187   format (10x, "Missing  '('  after function ", "'", a6, "' ")
        go to 6550

     case (8)
        write (unit = lunit(6), fmt = 72188)
72188   format (10x, 'Please break this monstruously large expression into smaller sections. ')
        go to 6550

     case (9)
        write (unit = lunit(6), fmt = 72189) bus2, bus3
72189   format (10x, 'This expression is not homogeneous.',/, 10x, 'The two operators upon which this condition was detected are', /, 15x, "'", a6, "'", ' and ', "'", a6, "'", ' . ')
        go to 6550

     case (10)
        write (unit = lunit(6), fmt = 72190) lstat(16)
72190   format (10x, 'The numerical argument ending in column  ',  i2, /, 10x, 'is more than  20  characters long. ')
        go to 6550

     case (11)
        write (unit = lunit(6), fmt = 72191) lstat(16)
72191   format (10x, 'The alphanumeric argument ending in column  ', i2, /, 10x, 'is more than  6 characters long. ')
        go to 6550

     case (12)
        write (unit = lunit(6), fmt = 72192) lstat( 16)
72192   format (10x, 'Unrecognizable logical operator near column  ', i2 )
        go to 6550

     case (13)
        write (unit = lunit(6), fmt = 72193) lstat( 16)
72193   format ( 10x, 'The numerical argument ending in column ', i2, /, 10x, 'contains more than one decimal point. ')
        go to 6550
     end select
     !     go to (62181, 62182, 62183, 62184, 62185, 62186), i1
     !62180 i1 = i1 - 6
     !     go to (62187, 62188, 62189, 62190, 62191, 62192, 62193), i1
  case (19)
     write (unit = lunit(6), fmt = 7213) lstat(16), lstat(15)
     write (unit = lunit(6), fmt = 7219) flstat(15)
7219 format (5x,  'A non-positive reference voltage equal to',  e13.4, '   was specified by the user. ')
     go to 6550

  case (20)
     write (unit = lunit(6), fmt = 7220) last, lstat(15), ibr
7220 format (5x, ' Overflow of steady-state table space.   List 23 tables are sized at',  i6, '   words,   which is insufficient for', /, 5x, 'even just the formation of  (Y),  to say nothing of later triangularization.   Overflow has occurred after only',  i5, /, 5x, 'branches have been processed, out of a total of', i5, ' . ')
     go to 6550

  case (21)
     write (unit = lunit(6), fmt = 7221)
7221 format (5x, ' The number of phases in this line is larger than the temporary limit of 10 for K. C. Lee modeling. ')
     go to 6550

  case (22)
     write (unit = lunit(6), fmt = 7222) lstat(14), bus1, bus2, lstat(15)
7222 format (5x, 'The steady-state solution is for two or more frequencies which are not separated.   Trouble was spotted at branch number',  i5, /, 5x, 'which connects bus  "',  a6, '"  with bus  "',  a6,  '" .   One source of conflict is in row', i5)
     go to 6550

  case (23)
     write (unit = lunit(6), fmt = 7223) lstat(14), bus1, bus2, lstat(15), lstat(16)
7223 format (5x, 'The steady-state solution is for two or more frequencies which are not separated.   Trouble was spotted at switch number', i5, /, 5x,  'which connects bus  "', a6, '"  with bus  "',  a6,  '" .   The left is excited by source number', i5, /, 5x, 'while the right is excited by source number', i6, ' . ')
     go to 6550

  case (24)
     write (unit = lunit(6), fmt = 7224)
7224 format (5x, 'The last-read switch card has both names identical.   The switch is closed on itself, and has no use. ')
     go to 6550

  case (25)
     write (unit = lunit(6), fmt = 7225) lstat(14), lstat(15)
7225 format (5x, 'The user has overflowed storage within the cable connstants supporting program.   For the current program', /, 5x, 'version, one is limited to cases having not over', i4 , ' conductors or ', i4, ' cables( the storage for a three', /, 5x, 'phase transmission line is equal to 5 cables if it has 2 ground wires.  Storage for the arrays in question has', /, 5x, 'been optimally (and dynamically) allocated so as to use as much of the EMTP core storage as is available.   It thus', /, 5x, 'is not the fault of the cable-constants supporting program itself, but rather of the overall EMTP dimensioning, that', /, 5x, 'this data case must be rejected.   Go procure or make another program version which has more total tabular ')
     write (unit = lunit(6), fmt = 7182)
7182 format (5x, 'storage space, and try the job over again.   Remember, the cable constants calculation requires the storage', /, 5x, 'of full matrices, so memory requirements go up approximately as the square of the number of conductors. ')
     go to 6550

  case (26 : 41)
     go to 6540
  end select
  !go to (6201, 6202, 6203, 6204, 6205, 6206, 6207, 6208, 6209, 6210, 6211 , 6212, 6213, 6214, 6215, 6216, 6217, 6218, 6219, 6220, &
  !     6221 , 6222, 6223, 6224, 6225, 6226, 6227, 6228, 6229, 6230, 6231 , 6232, 6233, 6234, 6235, 6236, 6237, 6238, 6239, 6240, &
  !     6241), n1
  go to 6540
6540 write (unit = lunit(6), fmt = 7540) kill, lastov
7540 format (/, 'Invalid  kill  code', i5,  5x, 'lastov =', i4, /, 1x)
6550 if (ipntv(1) .ne. -8888) go to 1429
  kill = ipntv(3) + 1
  if (kill .le. ipntv(2)) go to 1417
  kill = 0
  lastov = nchain
  nchain = 1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568) kill, ipntv(2)
4568 format (' Exit module  "subr55".  Kill, ipntv(2) =', 2i6)
  go to 9000
1417 write (unit = lunit(6), fmt = 1424) kill
1424 format (/, ' Message of kill-code number', i4, '.')
  lastov = nchain
  nchain = 51
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568) kill
  go to 9000
1429 if (jflsos .eq. 0) go to 1430
  if (nenerg .eq. 0) go to 1430
  !     write bounding records in the case of statistics salvage (sos).
  d7 = -9999.
  n15 = kswtch + 1
  write (unit = lunit(3)) (d7, j = 1, n15)
  n15 = lstat(32)
  write (unit = lunit(9)) (d7, j = 1, n15)
  call statsv
1430 if (m4plot .ne. 1) go to 6645
  call spying
6645 if (kill .gt. 1) go to 4092
  write (unit = lunit(6), fmt = 3391) lstat
3391 format (/, ' For   kill = 1   error stops, program maintenance may sometimes wish to inspect the contents of error-', /, " interface vectors  'lstat'  and  'flstat' .   These follow ....", //, "vector  'lstat'", /, (1x, 10i13))
  write (unit = lunit(6), fmt = 3392) flstat
3392 format (/, " Vector  'flstat'", /, (1x, 10e13.4))
  n1 = lstat(16)
  write (unit = lunit(6), fmt = 4000) n1
4000 format (/, ' of course maybe the user would like some suggestions as to why the table in question (list number , i2, 1h)', /, ' has overflowed.   If so, read on, good buddy.   the EMTP has a long-established policy of meritorious and laudable', /, ' cooperation in the form of crystal-clear diagnostic messages, such as the following ..... ')
  if (n1 .eq. 99) go to 4499
  select case (n1)
  case (1)
     write (unit = lunit(6), fmt = 4101)
4101 format (5x, "Network nodes are of course defined by the user's branch and switch cards, by the names which identify the ", /, 5x, "two ends of the element (fields  'bus1'  and  'bus2'  of the data card, columns 3-14).   In addition, there are  ", /, 5x, 'several less-obvious ways in which nodes are added to the bus table ....', /, 8x, '1.  Switched-r elements (type-92) and switched-l elements (type 93) each create one internal node for every', /, 12x,'such element. ')
     write (unit = lunit(6), fmt = 4201)
4201 format (8x, '2.  Ground (blank node name) always has an entry in the bus list.', /, 8x, "3.  Each single-phase saturable transformer component adds one node (node name  'bustop' ,  columns 39-44 of the", /, 12x, "card bearing the request word 'transformer ' in columns 53-14).", /, 8x, "4.  Each three-phase saturable transformer component adds one node (node name 'bus3ph', read from columns 27-32", /, 12x, "of the card bearing the request word 'transformer three phase ' in columns 3-26).   This is a 4-th word, in ")
     write (unit = lunit(6), fmt = 4301)
4301 format (12x, 'Addition to the 3 which are added under point 3, for a 3-phase transformer. ' )
     go to 4099

  case (2)
 write (unit = lunit(6), fmt = 4102)
4102 format (5x, 'Network branches are of course defined directly by the user as he inputs branch data.   Yet the counting of', /, 5x, 'entries in this linear branch table has some subtle points which are worthy of the following detailed comment ....', /, 8x, '1.  True nonlinear elements (type codes 92 or 93) or the continuous time-varying resistance element (type 91)', /, 12x, 'never contribute to the linear branch table.   These elements are pulled outside of the network, and are handled', /, 12x,  'by compensation. ')
 write (unit = lunit(6), fmt = 4202)
4202 format (8x, '2.  Switched-resistance elements (type-92) each contribute one entry to the linear branch table.   Switched-', /, 12x, 'inductance elements (type 93) contribute two entries apiece.', /, 8x, '3.  Each type-99 pseudo-nonlinear resistance element contributes an entry, unless it is paralleled by another', /, 12x, 'linear branch.   These added very-high-impedance branches show up with a card image and data interpretation', /, 12x, 'almost as though the user had inputted the resistor himself. ')
     write (unit = lunit(6), fmt = 4302)
4302 format (8x, '4.  Each n-winding single-phase saturable transformer component always internally sets up   2(n-1) + 1   branches', /, 12x, "for everything but the magnetizing branch.   For the latter, a nonzero magnetizing resistance (field  'rmag' ,", /, 12x, 'columns 45-50 of the transformer request card) will add an entry, as will a saturation characteristic defined', /, 12x, 'by exactly one point.   A 3-phase saturable-transformer component contributes only in that it consists of 3', /, 12x,  'single-phase units as just detailed. ')
     go to 4099

  case (3)
     write (unit = lunit(6), fmt = 4103)
4103 format (5x, 'The R, L, C tables store floating-point resistance, inductance, and capacitance parameter values associated', /, 5x, 'with lumped-parameter elements.   Although such values are inputted on branch cards (mostly), the user should not', /, 5x, 'confuse the present parameter storage with the branch-table storage of list 2.   Contributions to the present', /, 5x, 'list-3 table by different EMTP components are as follows, assuming no usage of the reference-branch or reference-', /, 5x,  'component idea ..... ')
     write (unit = lunit(6), fmt = 4203)
4203 format (8x,  '1.  Each uncoupled series r-l-c branch contributes one entry.', /, 8x, '2.  Each n-phase pi-circuit component, or each n-phase mutually-coupled r-l component, contributes   n(n+1)/2', /, 12x, 'entries.', /, 8x, '3.  Each single-phase n-winding saturable-transformer component contributes   3n-2   entries, at least.   If', /, 12x, "magnetizing resistance  'rmag'  is used, add another entry.   If the transformer is actually linear, with finite-", /, 12x,  'slope magnetization characteristic, add another entry. ')
     write (unit = lunit(6), fmt = 4303)
4303 format (8x, '4.  A 3-phase saturable transformer has the aforementioned entries in the table for the three single-phase', /, 12x,  'transformers which are sub-components of it.   In addition, there are always   3   extra entries.', /, 8x, '5.  If a network uses one or more type-99 pseudo-nonlinear resistance elements which is not paralleled by another', /, 12x, 'lumped-parameter branch, one entry is added to the table (for all such elements, not for each one).', /, 8x, '6.  Each switched-resistance element (type-92 switch card) contributes one entry. ')
     write (unit = lunit(6), fmt = 4403)
4403 format (8x, '7.  Each switched-inductance element (type-93 switch card) contributes two entries.', /, 8x, '8.  Each  type-16  source element (simplified  ac/dc  converter representation) contributes two entries.', /, 8x, "9.  Each distributed-parameter transmission circuit contributes     n * (n + 1) / 2    entries, where  'n'  is ")
     write (unit = lunit(6), fmt = 4503)
4503 format (12x, "the number of phases of the line.   In case of such overflow, the  'present figure'   will not include these.", /, 5x, 'where reference-branch or reference-component ideas are used, there generally is no contribution at all to the', /, 5x, 'R, L, C tables.   In this case, the program simply makes reference to previously-stored (and hence previously-', /, 5x, 'counted) data values. ' )
     go to 4099

  case (4)
     write (unit = lunit(6), fmt = 4104)
4104 format (5x, 'Counting the number of entries in the source table (size of list 4) is quite simple, as per the following', /, 5x, 'rules ....', /, 8x, '1.  Each conventional source component (type code 1 through 14, punched in columns 1-2 of the source card)', /, 12x, 'contributes one entry.', /, 8x, '2.  Each type-15 source component (the simplified ac/dc converter model, neglecting ripple on the dc side)', /, 12x, 'contributes two entries. ')
     write (unit = lunit(6), fmt = 4204)
4204 format (8x, '3.  Each 3-phase dynamic synchronous-machine component (type codes 21, 22, 23 punched in columns 1-2 of', /, 12x, 'consecutive source cards) contributes three entries.', /, 8x, '4.  Each switched-resistance element (type code 92 punched in columns 1-2 of the switch card) contributes 2 entries. ')
     go to 4099

  case (5)
     write (unit = lunit(6), fmt = 4105)
4105 format (5x, 'list 5 ostensibly gives the size of the table-of-factors storage (l-u decomposition) for the triangularized', /, 5x, 'real equivalent nodal admittance matrix  (y)  of the time-step loop.   At each time-step, the real matrix equations', /, 5x, ' (y)v = i   are solved for real node-voltage vector  v ,  by means of a repeat solution using the table of factors.', /, 5x, 'Because  (y)  is symmetric, only the upper-triangular factors (including the diagonal) are stored.   There is only', /, 5x, 'one integer word and one floating-point word for each factor, it will be noted (see below).   Node ordering ')
     if (lstat(13) .eq. 1) write (unit = lunit(6), fmt = 4805) lstat(14), kpartb
4805 format (5x, 'beginning with "m32." versions,  list 7 storage of (Ybb/Ybc) is being destroyed,  and the full (y) is added', /, 5x, 'to the bottom of list 5 (fills from the bottom up).   But space ran out before the storage of (y) is finished.   Only', /, i8, '   rows are done,  out of a total of', i4,  ' ,   and factoring has not yet even begun. ')
     if (lstat(13) .eq. 2) write (unit = lunit(6), fmt = 4905) lstat(15), kpartb
4905 format (5x, 'beginning with "m32." versions,  list 7 storage of (Ybb/Ybc) is being destroyed,  and the full (y) is added', /, 5x, 'to the bottom of list 5 (fills from the bottom up).   The downward-growing factors spilled over onto (y) at row', /, i8, '   of the triangularization,  whereas we must reach row kpartb =',  i4, '   to end successfully. ')
     write (unit = lunit(6), fmt = 4205)
4205 format (5x, 'The order of elimination (node renumbering) is constrained only in that nodes of known voltage are forced last. ')
     go to 4099

  case (6)
     write (unit = lunit(6), fmt = 4106)
4106 format (5x, 'Switches are completely straightforward, being defined only by switch cards.   One entry in the switch table', /, 5x, "is created for every switch card, whether it is for an ordinary switch ('itype'  of columns 1-2 equal to zero),", /, 5x, "a switched resistance element ('itype' = 92), or a switched-inductance element ('itype' = 93). ")
     go to 4099
     ! ???????????   list 7 is presently unused   ????????????????

  case (7)
     write (unit = lunit(6), fmt = 4107)
4107 format (' unused')
     go to 4099

  case (8)
     write (unit = lunit(6), fmt = 4108)
4108 format (5x, 'Past-history points for distributed-parameter representation of transmission lines are stored in modal form,', /, 5x, 'always.   Each mode requires storage, where there are as many modes as there are coupled conductors (e.g., a double', /, 5x, 'circuit line has 6 modes.).   A constant-parameter (frequency-independent) mode contributes    tau/deltat    entries,', /, 5x, "where  'tau'  is the modal travel-time of the line,  'deltat'  is the time-step size, and the division involves " )
     write (unit = lunit(6), fmt = 4208)
4208 format (5x, 'integer truncation followed by the addition of unity.   For a frequency-dependent mode, more past-history', /, 5x, "than this is needed, enough to perform the  a2(t)  convolution.   In the preceding formula, take  'tau'  to be the", /, 5x,  "time  't2'  at which the exponential tail on  a2(t)  begins (typically 3 travel-times or so). ")
     go to 4099

  case (9)
     write (unit = lunit(6), fmt = 4109)
4109 format (5x, 'Entries in the nonlinear-element table are created by the following element types .... ', /, 8x, '1.  Piecewise-linear time-varying resistance elements  R(t) ,   branch-type 91.', /, 8x, '2.  True nonlinear  v-i  characteristic, branch-type 92.', /, 8x, '3.  True nonlinear inductance element, branch-type 93.', /, 8x, '4.  Staircase time-varying resistance element  R(t) ,   branch-type 97. ')
     write (unit = lunit(6), fmt = 4209)
4209 format (8x, '5.  Pseudo-nonlinear inductance element, branch-type 98.', /, 8x, '6.  Pseudo-nonlinear  v-i  characteristic, branch-type 99.', /, 5x, 'Every element falling into this classification contributes one entry to list 9. ')
     go to 4099

  case (10)
     write (unit = lunit(6), fmt = 4110)
4110 format (5x, 'This list-10 storage applies to all characteristics which are defined as pairs of coordinates, terminated by', /, 5x, 'a  9999-card.   Each pair of coordinates so seen on the input-data listing contributes one entry to list 10.', /, 5x, 'but note carefully the wording of this rule.   it is only the ones which are actually seen visually on the data', /, 5x, 'listing (use of the reference-branch procedure adds nothing to list 10, and will not be seen on the data listing. ')
     write (unit = lunit(6), fmt = 4210)
4210 format (/, 5x, 'A second contributor to the list-10 storage requirement is the type-94 nonlinear element component (surge', /, 5x, 'arrester with current limiting gap).   Each such surge arrester which does not use the reference-branch option', /, 5x, 'adds  18  entries to the list-10 storage requirement.   for each surge arrester which does use the reference-branch', /, 5x, 'procedure, there is a contribution of  11  entries to the list-10 storage requirement. ')
     write (lunit(6), 4310)
4310 format (/, 5x, 'Finally, if you have zno surge arresters in the case, four addtional cells are required for each of the zno arresters. ')
     go to 4099

  case (11)
     write (unit = lunit(6), fmt = 4111)
4111 format (5x, 'branch-output quantities are generated by column-80 punches on branch cards and on switch cards.   Each punch', /, 5x, "of  '1'  or  '2'  (branch current or branch voltage) contributes one entry to list 11.   Punches of  '3'  (for", /, 5x, "branch current and voltage) or  '4'  (for branch power and energy) contribute two entries each, to list 11.", /, 5x, 'node-voltage outputs which are specified individually, one at a time, ----- i.e., by punching 6-character node ')
     write (unit = lunit(6), fmt = 4211)
4211 format (5x, 'names in the  13a6  field of the node-voltage output-specification card ----- are likewise limited by list 11.', /, 5x, 'If the user has requested the automatic output of every node voltage instead of this selective output (by means', /, 5x, "of a  '1'  punched in column 2 of the aforementioned card), this list-11 limit does not apply to node voltage ", /, 5x, 'outputs. ')
     go to 4099

  case (12)
     write (unit = lunit(6), fmt = 4112)
4112 format (5x, 'Sorry, no special advice available. ')
     go to 4099

  case (13)
     write (unit = lunit(6), fmt = 4113)
4113 format (5x, "Every continuously-transposed distributed-parameter transmission-line component (branch type-code  'itype'  of ", /, 5x, 'columns 1-2 equal to  -1,  -2,  etc.) represents a possible contribution to list 13.   Each line mode which is', /, 5x, "modelled as being frequency-dependent (variable  'ipunch'  of columns 53-54 equal to  -1 )  contributes one entry ", /, 5x, 'to list 13.   Generally this will only be for the zero-sequence mode (the first card of the group), if at all. ')
     go to 4099

  case (14)
     write (unit = lunit(6), fmt = 4114)
4114 format (5x, 'Frequency-dependent representation for a mode of a distributed-parameter transmission line is requested by a', /, 5x, "value of  -1  punched in field  'ipunch'  (columns 53-54) of the associated branch card.   Assuming that the", /, 5x, 'reference-branch procedure is not used, the input of weighting functions  a1(t)  and  a2(t)  follows.   The number', /, 5x, 'of points on these input cards is irrelevant, and is in no way related to the size of list 14.   Instead, the ')
     write (unit = lunit(6), fmt = 4214)
4214 format (5x, 'list-14 storage depends upon both the time-span of the weighting functions, and also upon the time-step size', /, 5x, "'deltat' ,  as follows.   Let  't1'  be the time span from the nonzero beginning of  a1(t)  (at about one travel", /, 5x, "time) to where its exponential tail begins (typically about two travel times).   also, define  't2'  to be the", /, 5x, 'time at which the exponential tail of  a2(t)  begins (typically about three travel times).   Then the storage')
     write (unit = lunit(6), fmt = 4314)
4314 format (5x, 'requirement in list 14 is given by the relation  np = (t1 + t2) / deltat  .     Lines which use the', /, 5x, 'reference-branch procedure require no list-14 storage, note. ')
     go to 4099

  case (15)
     write (unit = lunit(6), fmt = 4115)
4115 format (5x, 'To perform the convolution associated with frequency-dependent modes of distributed-parameter transmission', /, &
          5x, 'lines, modal past-history must be stored for both ends of the line.   whether the reference-branch procedure was', /, &
          5x, 'used or not in no way modifies this requirement.   For every frequency-dependent mode, two cells are taken up', /, &
          5x,  'in list 15. ')
     go to 4099

  case (16 : 18)
     !4016 go to 4012
     !4017 go to 4012
     !4018 go to 4012
     call a4012

  case (19)
     write (unit = lunit(6), fmt = 4119)
4119 format (5x, 'Do not dispair, all is not lost (yet).   What has happened is that list  19  is inadequate for the tacs table', /, &
          5x, "sizes which were requested.   Either the user specified these sizes explicitely himself using an  'absolute tacs", /, &
          5x, "dimensions'  card, or the EMTP supplied its own default set.   In either case, these absolute tacs table sizes will", /, &
          5x, "require a list-19 size as shown under the  'present figure' column in row  19 .   Before simply redimensioning the ")
     write (unit = lunit(6), fmt = 4219)
4219 format (5x, 'EMTP to provide such a list-19 figure, however, the user might try to more optimally divide the existing total', /, &
          5x, "among the different tacs tables, using either an  'absolute tacs dimensions'  card or a  'relative tacs dimensions'", /, &
          5x, 'card.   Finally, because   kill = 122   provides much general information about tacs dimensioning, we shall now', /, &
          5x, 'print it as well.... ')
     kill = 122
     lstat(17) = 0
     lstat(16) = 0
     lastov = nchain
     nchain = 51
     if (iprsup .ge. 1) write (lunit(6), 4568)  kill
     go to 9000

  case (20 : 23)
     !4020 go to 4012
     !4021 go to 4012
     !4022 go to 4012
     !4023 go to 4012
     call a4012

  case (24)
     n9 = lcomp * lbus / ntot
     write (unit = lunit(6), fmt = 4124) ncomp, n9
4124 format (5x, 'The present data case involves',  i4, '   phase compensation, which exceeds the effective program limit of', i4, ' . ', /, &
          5x, 'This latter figure is the limiting value of list 24 multiplied by  lbus/ntot  (the ratio of the', /, &
          5x, 'maximum number of buses to the, 33h actual number for this problem). ')
     write (unit = lunit(6), fmt = 4224)
4224 format (5x, 'Note that the effective limit on the number of phases of compensation thus varies inversely with problem', /, &
          5x, 'size.  Cut the size in half, and twice as many phases are available, without redimensioning with a larger list 24. ')
     go to 4099

  case (25)
     !4025 go to 4012
     call a4012

  case (26)
     if (lstat(13) .eq. 0) go to 4226
     write (unit = lunit(6), fmt = 4126) lstat(13), lsiz26
4126 format (5x, "The user's data includes a coupled branch group of", i5, '   phases.   Squaring this number exceeds  list 26  (',  i5,  ' ).')
     go to 4099
4226 write (unit = lunit(6), fmt = 4326)
4326 format (5x, 'List 26 working vectors "volt", "volti", "voltk", "vim", and "volta"  are used in various ways.   This is one.',/, &
          5x, 'if user does not request 50 or more,  and if he has trouble,  see program maintenance for details. ')
     go to 4099

  case (27 : 29)
     !4027 go to 4012
     !4028 go to 4012
     !4029 go to 4012
     call a4012
  end select

  !  go to (4001, 4002, 4003, 4004, 4005, 4006, 4007, 4008, 4009, 4010, 4011, 4012, 4013, 4014, 4015, 4016, 4017, 4018, 4019, &
  !       4020, 4021, 4022, 4023, 4024, 4025, 4026, 4027, 4028, 4029), n1

4499 write (unit = lunit(6), fmt = 4199)
4199 format (5x, 'Both network node-renumbering (transient and also steady-state) and the steady-state phasor solution make use', /, 5x, 'of three very large arrays which overlay most of the labeled-common storage space (the data of which is preserved', /, 5x, 'on logical 4 during these calculations).   This is a dynamically-dimensioned table, then, which is sized to use', /, 5x, 'all available space (perhaps 2/3 of labeled common).   In particular, this working area includes all of the ')
  write (unit = lunit(6), fmt = 4299)
4299 format (5x, 'generally-large storage for lists 5 and 8.   Increasing the dimensions of either of these two lists will directly', /, 5x, '(and without any loss) increase the size of list 99.   It might be mentioned that the steady-state phasor', /, 5x, 'manipulations (renumbering, solution) will almost always provide the limiting difficulty.   This is because', /, 5x, 'sparsity of the steady-state phasor network is generally worse than for the time-step-loop network, due to the ')
  write (unit = lunit(6), fmt = 4399)
4399 format (5x, 'difference in treatment of distributed-parameter lines.   For steady-state solution, equivalent branches', /, 5x, "interconnect every terminal node of the line, while the two ends are disconnected by Bergeron's method for the" , /, 5x, 'time-step-loop network.   double-circuit (6-conductor) lines are particularly nasty in the steady-state, then,', /, 5x, 'having 12 terminal nodes which are all interconnected by equivalent branches. ')
4099 write (unit = lunit(6), fmt = 4098)
4098 format (/, ' in order to effectively trade memory space among the different tables, one must know how many arrays there', /, ' are in each table (effectively).   The following tabulation shows the effective multiplicity associated with each', /, ' independent list ----- those lists whose lengths are under user  control by means of EMTP variable dimensioning. ')
  write (unit = lunit(6), fmt = 4096)
4096 format (5x, '-------------1----------------------------------------------------------------------------------------------------')
  write (unit = lunit(6), fmt = 4095) (i, i = 1, 25)
4095 format (5x, 'list number  1', 25i4)
  write (unit = lunit(6), fmt = 4096)
  write (unit = lunit(6), fmt = 4093)
4093 format (5x, 'loating pt. 1   6   5   3   6   1  12   2   2   8   3   1   4   8   1   2   2   0   6   1   1  24   2   1   #   *   1 ')
  write (unit = lunit(6), fmt = 4097)
4097 format (5x, 'Integer      1   4   7   0   2   1  10   0   0  11   0   3   0   4   0   0   1  10   2   0   0   0   0   0   0   0', /, 5x, 'total        1  10  12   3   8   2  22   2   2  19   3   4   4  12   1   2   1  16   3   1  24   2   1   #   *   1 ')
  write (unit = lunit(6), fmt = 4096)
  write (unit = lunit(6), fmt = 4091)
4091 format (3x,  '# --- used only for virtual machines (Burroughs, Prime, VAX, Apollo, etc.)   others can ignore this list.', /, 3x, '* --- rather than count list 24 itself, add the value to the floating-point and total counts for lists 1 and 6. ')
4092 write (unit = lunit(6), fmt = 5315)
5315 format (' ')
  write (unit = lunit(6), fmt = 5314)
5314 format (25x, "Caution.   Be skeptical of above  'present figure'  entries, due to abnormal termination of case. ")
  write (unit = lunit(6), fmt = 5316)
5316 format (1x, 132('-'))
  do  i = 1, 2
     write (unit = lunit(6), fmt = 5320)
  end do
5320 format (1x, 22(' error/'))
  write (unit = lunit(6), fmt = 5316)
  if (kilsav .eq. 92) go to 6767
  !     lunit5 = mtape
  write (unit = *, fmt = *) ' commented out  lunit5 = mtape.   set kill = 0.'
  kill = 0
  n6 = 0
  !     read input card using cimage
6740 call cimage
  n13 = kolbeg
  nright = -2
  kolbeg = 1
  read (unit = abuff, fmt = 6741) (texcol(i), i = 1, 80)
6741 format (80a1)
  !  call freone (d1)
  call ffree (d1)
  nright = 0
  if (nfrfld .ne. 1) go to 6743
  if (toLower (texta6(1)) .eq. text5) go to 6754
  if (toLower (texta6(1)) .eq. text13) go to 6764
  if (texta6(1) .eq. text19) go to 6771
6743 if (nfrfld .ne. 4) go to 6770
  if (toLower (texta6(4)) .ne. text4) go to 6770
  if (toLower (texta6(3)) .ne. text3) go to 6770
  if (toLower (texta6(2)) .ne. text2) go to 6760
  if (toLower (texta6(1)) .ne. text1) go to 6760
6754 write (unit = kunit6, fmt = 6755)
6755 format ('+Marker card preceding new data case.')
  call interp
  if (n6 .lt. 5) go to 6758
  n6 = n6 - 4
  write (unit = lunit(6), fmt = 6757) n6
6757 format (51x, 'End suppression of listing.', i6, '  cards were unlisted.')
6758 noutpr = 0
  kolbeg = -intinf
  lastov = nchain
  nchain = 1
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568) 4568
  go to 9000
6760 if (toLower (texta6(2)) .ne. text12) go to 6770
  if (toLower (texta6(1)) .ne. text11) go to 6770
6764 write (unit = kunit6, fmt = 6765)
6765 format ('+Marker card following last data case.')
  call interp
6767 kill = 9999
  lastov = nchain
  nchain = 31
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568) kill
  go to 9000
6770 if (nfrfld .ne. 5) go to 6773
  if (toLower (texta6(1)) .ne. text14) go to 6773
  if (toLower (texta6(2)) .ne. text15) go to 6773
  if (toLower (texta6(3)) .ne. text16) go to 6773
  if (toLower (texta6(4)) .ne. text17) go to 6773
  if (toLower (texta6(5)) .ne. text18) go to 6773
6771 if (nenerg .eq. 0) go to 6773
  if (knt .le. 1) go to 6773
  write (unit = kunit6, fmt = 6772)
6772 format ('+Request for statistics salvage.')
  call interp
  if (jflsos .gt. 0) go to 6773
  d7 = -9999.
  n15 = kswtch + 1
  write (unit = lunit(3)) (d7, j = 1, n15)
  n15 = lstat(32)
  write (unit = lunit(9)) (d7, j = 1, n15)
  if (n13 .eq. -intinf) kolbeg = n13
  if (kolbeg .gt. 0) go to 1773
  read (unit = abuff, fmt = 1764) n1
1764 format (29x, i3)
  go to 1778
  !1773 call freone (d1)
1773 call ffree (d1)
  n1 = int (d1)
1778 if (n1 .gt. 0) go to 1774
  if (n1 .lt. 0) iprsup = 9
  !     find random integer  'n1'  between zero and 999.
  call runtym (d11, d12)
  seed = seedy (tclock(1)) + 1000. * (d11 + d12)
  n13 = int (alog1z (seed) + epsiln)
  n13 = n13 - 2
  seed = seed / 10. ** n13
  n1 = int (seed)
1774 n2 = n1 / 100
  n5 = n1 - 100 * n2
  n3 = n5 / 10
  n4 = n5 - 10 * n3
  write (unit = lunit(6), fmt = 1792) n2, n3, n4
1792 format ('+', 34x, "'",  3i1, "'")
  lstat(14) = n2
  lstat(15) = n3
  lstat(16) = n4
  call statsv
  go to 6740
6773 if (noutpr .eq. 0) write (unit = kunit6, fmt = 6775)
6775 format ('+Card ignored in search for new-case beginning.')
  n6 = n6 + 1
  if (n6 .lt. 5) go to 6769
  if (noutpr .eq. 1) go to 6769
  noutpr = 1
  write (unit = lunit(6), fmt = 6768)
6768 format (51x, 'Begin suppression of skipped-record printout.')
6769 go to 6740
9000 return
end subroutine subr55

!
! subroutine statsv.
!

subroutine statsv
  implicit none
  return
end subroutine statsv

!  m99.9999      overlay ( finish, 99999 )

!
! end of file over55.f90
!
