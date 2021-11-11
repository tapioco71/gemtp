!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over51.f90
!

!
! subroutine over51.
!

subroutine over51
  use blkcom
  implicit none
  real(8) :: vmin, vmax
  !
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4567) kill
4567 format (' Begin "over51".  kill =', i8)
  call runtym (vmin, vmax)
  flstat(9) = vmin - flstat(9)
  flstat(10) = vmax - flstat(10)
  if (kill .eq. 0) go to 4521
  call subr51
  go to 4536
4521 lastov = nchain
  nchain = 55
4536 if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
4568 format (' Exit module "over51".')
  return
end subroutine over51

!
! subroutine subr51.
!

subroutine subr51
  use blkcom
  implicit none
  integer(4) :: i, j, ksat
  real(8) :: d1, vmin, vmax
  !
  !     Note. --- As the structure of the EMTP error overlays change,
  !               the following assignments may have to be altered ...
  !                    nfrfld  ---- total number of error overlays
  !                    kpen(j) ---- storage for the highest kill-code
  !                                 number handled by error overlay
  !                                 number  50+j ,   for all but the
  !                                 last error overlay.
  kpen(1) = 50
  kpen(2) = 90
  kpen(3) = 150
  kpen(4) = 200
  nfrfld = 5
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4567) kill
4567 format (' Begin "subr51".  kill =', i8)
  if (kill .eq. 99 .and. lstat(19) .eq. 11111) call stoptp
  kpen(nfrfld) = intinf
  call runtym (vmin, vmax)
  flstat(9) = vmin - flstat(9)
  if (ipntv(1) .ne. -8888) go to 1643
  ! following assignments are to define exceptional /blank/
  ! variables used in  "kill codes"  usage:
  if (kill .eq. 38) flstat(12) = 1.0
  if (kill .eq. 41) lstat(10) = 1
  if (kill .eq. 43) flstat(12) = 1.
  if (kill - 1 .ne. ipntv(3)) go to 1643
  ipntv(3) = kill
  go to 1649
1643 if (lastov .ge. nchain) go to 1649
  noutpr = 0
  call interp
  write (unit = lunit6, fmt = 5315)
5315 format (' ')
  write (unit = lunit6, fmt =  5316)
5316 format (1x, 132('-'))
  do i = 1, 2
     write (unit = lunit6, fmt = 5320)
  end do
5320 format (1x, 22('error/'))
  write (unit = lunit6, fmt = 5316)
  if (m4plot .eq. 1) go to 5534
  write (unit = lunit6, fmt = 5332)
5332 format ('0You lose, fella.   The EMTP logic has detected an error condition, and is going to terminate your run.  The following', /, &
          ' printout message summarizes the data difficulty leading to this program decision.  By studying this message, the problem', /, &
       ' data,  and the rules delineated by the  840-page EMTP rule book,  it is hoped that the user can rectify the problem.', /, &
       ' If still in doubt after some study, come see program maintenance for assistance.')
  read (unit = abuff, fmt = 1019) (texcol(j), j = 1, 14)
1019 format (13a6, a2)
  write (unit = lunit6, fmt = 5333) (texcol(j), j = 1, 14)
5333 format (" Where an otherwise-unidentified card is referred to, or is called the 'last' card, this means the most-recently-", /, &
       ' read card of the input data deck.   The 80-column card image in question is the last one printed out before this', /, &
       ' termination message.   A copy follows.... ', 13a6, a2)
5534 write (unit = lunit6, fmt = 5335) kill, lstat(18), lstat(19)
5335 format (14x, 'kill code number', 16x, 'overlay number', 10x, 'nearby statement no.', /, 3i30)
1649 do j = 1, nfrfld
     if (kill .gt. kpen(j)) go to 1658
     if (j .eq. 1) go to 1684
     lastov = nchain
     nchain = 50 + j
     if (iprsup .ge. 1) write (unit = lunit6, fmt = 1652) kill, j, kpen(j), nchain
1652 format (' Error overlay found.   kill, j, kpen(j), nchain =', 4i8)
     go to 9000
  end do
1658 continue
1684 continue
!  go to (6001, 6002, 6003, 6004, 6005, 6006, 6007, 6008, 6009, 6010, 6011, &
!       6012, 6013, 6014, 6015, 6016, 6017, 6018, 6019, 6020, 6021, 6022, &
!       6023, 6024, 6025, 6026, 6027, 6028, 6029, 6030, 6031, 6032, 6033, &
!       6034, 6035, 6036, 6037, 6038, 6039, 6040, 6041, 6042, 6043, 6044, &
!       6045, 6046, 6047, 6048, 6049, 6050), kill
  select case (kill)
  case (1)
     go to 6001

  case (2)
     go to 6002

  case (3)
     go to 6003

  case (4)
     go to 6004

  case (5)
     go to 6005

  case (6)
     go to 6006

  case (7)
     go to 6007

  case (8)
     go to 6008

  case (9)
     go to 6009

  case (10)
     go to 6010

  case (11)
     go to 6011

  case (12)
     go to 6012

  case (13)
     go to 6013

  case (14)
     go to 6014

  case (15)
     go to 6015

  case (16)
     go to 6016

  case (17)
     go to 6017

  case (18)
     go to 6018

  case (19)
     go to 6019

  case (20)
     go to 6020

  case (21)
     go to 6021

  case (22)
     go to 6022

  case (23)
     go to 6023

  case (24)
     go to 6024

  case (25)
     go to 6025

  case (26)
     go to 6026

  case (27)
     go to 6027

  case (28)
     go to 6028

  case (29)
     go to 6029

  case (30)
     go to 6030

  case (31)
     go to 6031

  case (32)
     go to 6032

  case (33)
     go to 6033

  case (34)
     go to 6034

  case (35)
     go to 6035

  case (36)
     go to 6036

  case (37)
     go to 6037

  case (38)
     go to 6038

  case (39)
     go to 6039

  case (40)
     go to 6040

  case (41)
     go to 6041

  case (42)
     go to 6042

  case (43)
     go to 6043

  case (44)
     go to 6044

  case (45)
     go to 6045

  case (46)
     go to 6046

  case (47)
     go to 6047

  case (48)
     go to 6048

  case (49)
     go to 6049

  case (50)
     go to 6050
  end select

6001 write (unit = lunit6, fmt = 7001) lstat(16)
7001 format (5x, 'Storage exceeded for list number ', i2, '.   See dimensioned limit in tabulation below.',/, 5x, 'The problem being inputted is simply too big for the program as currently dimensioned.   Since there usually are ', /, 5x, 'ways of circumventing this difficulty, it is suggested that the user consult his friendly neighborhood program   ', /, 5x, 'maintenance man.')
  go to 6220
6002 if (t .lt. 0.0) go to 7102
  write (unit = lunit6, fmt = 7002) flstat(16)
7002 format (5x, "Time-step size 'deltat' as read from columns 1-8 of the first miscellaneous data card is not positive.   The", /, 5x, 'user punched a value of ', e14.4,  '.   Unlike Jules Verne, you are not allowed to stop or decrease time during', /, 5x, "a simulation, my friend.   Don't try riding out of an active volcano on a raft floating on molten lava, either,", /, 5x, "although that's another story.")
  go to 6220
7102 write (unit = lunit6, fmt = 7202) t
7202 format (5x, "The starting time   't'   as read from the floating-point miscellaneous data card is negative, which is", /, 5x, 'illegal.   A value of', e15.5, '   was read from the data field of columns 49-56.')
  go to 6220
6003 write (unit = lunit6, fmt = 7003) lstat(16)
7003 format (5x, 'Illegal type code read from columns 1-2 of last branch card.   The user punched a value of ', i2, '.   Either', /, 5x, 'the punched number itself is patently illegal (always an error, under any circumstances), or the card in question  ',/, 5x, 'his out of sequence in relation to the preceding data card which was inputted.   As an example of the latter',/, 5x, "case, consider a '-3' punch, with the preceeding card not bearing a '-2' punch.   In any case, open up the user's",/, 5x, 'manual, and reread the rules for the data type that you have been trying to input, my friend.')
  write (unit = lunit6, fmt = 7103)
7103 format (5x, 'Yet, in case this general advice does not seem to apply, consider the possible trouble which can arise from a', /, 5x, 'preceding faulty use of the reference-branch capability. This feature has the emtp looking for a certain number', /, 5x, "and type of branch cards, based on properties of the component to which reference has been made.   If the user's", /, 5x, 'data cards do not in structure match those of the reference component, an error stop of the present type may very')
  write (unit = lunit6, fmt = 7203)
7203 format (5x, "be expected.   Remember that in cases where two or more branches from reference bus  'bus3'  to reference bus", /, 5x, "'bus4'  (ordered pair of names, read from columns 15-24 as  2a6  information) exist, the EMTP will always pick", /, 5x, 'the first one that it finds (in order of branch input) for reference purposes.')
  go to 6220
6004 write (unit = lunit6, fmt = 7004)
7004 format (5x, 'Last branch was zero impedance (R, L, C fields of columns 27-44 were all zero).   If you really want a short', /, 5x, 'circuit, you must punch a very small value for  r  or l.   Or better yet, why not do away with one of the node', /, 5x, 'names of this branch, treating both ends as the same bus (a perfect short circuit).')
  go to 6220
6005 write (unit = lunit6, fmt = 7105)
7105 format (5x, 'The user has been inputting pairs of points which define a nonlinear (or pseudo-nonlinear) element characteristic.', /, 5x, "These must be in order, moving ever to the right and upward in the x-y plane.   But the user's just-inputted")
  write (unit = lunit6, fmt = 7005)
7005 format (5x, 'characteristic is not monotone increasing as it should be.   A decrease in one of the two coordinate points', /, 5x, 'has been detected on the last data card which was read.   Shape up or ship out, Jack.')
  go to 6220
6006 write (unit = lunit6, fmt = 7006) bus3, bus4
7006 format (5x, 'Reference branch names of last data card do not refer to previously-inputted branch of same type.   The data being', /, 5x, 'referenced can not be found.   Check for spelling errors, or position differences of blanks within the field widths. ', /, 5x, 'Defective names as read off card from columns 15-26 were ',  "'", a6, "' and '", a6, "'.")
  go to 6220
6007 write (unit = lunit6, fmt = 7007) lstat(16)
7007 format (5x, 'An illegal type code has been read from columns 1-2 of the last data card, which the EMTP believes to be a ', /, 5x, 'switch card.   The EMTP read a value of ', i2,  '.   But such a value cannot be accepted, since   ', /, 5x, 'only type codes  0 (ordinary switch),  92 (switched resistance),  93 (switched inductance)', /, 5x, ' 11 (diode and tacs-controlled valve),  12 (tacs-controlled spark gap or triac)', /, 5x, 'hand  13 (tacs-controlled ordinary switch)  are allowed.')
  go to 6220
6008 write (unit = lunit6, fmt = 7008)
7008 format (5x, 'An error in the ordering of switch cards has occured.   All switched-resistance elements (type-92) must precede ',/, 5x, 'any switched-inductance (type-93) elements.   The last data card which has been read violates this rule.', /, 5x, 'Get with it, guy, shape up.   ')
  go to 6220
6009 write (unit = lunit6, fmt = 7109)
7109 format (5x,  'The last data card which was read has been taken by the EMTP to be a switched-inductance element.')
  write (unit = lunit6, fmt = 7009) flstat(16)
7009 format (5x, 'But this component  has saturation (breakpoint) flux nonpositive, which is absurd.   From columns 45-54, the', /, 5x, 'program read the value ', e12.3, ' from the last data card.')
  go to 6220
6010 write (unit = lunit6, fmt = 7010) lstat(16)
7010 format (5x, 'The last data card which was read has been taken to be a source card.   But it bears an illegal type code', /, 5x, 'in columns 1-2 (a value of ', i2,  " was read).   Open up the user's manual, and reread the rules for source cards.")
  go to 6220
6011 write (unit = lunit6, fmt = 7011) lstat(16)
7011 format (5x, 'Type-16 controlled dc voltage source card has illegal initial condition code in columns 9-10.', /, 5x, 'The value read was ', i2)
  go to 6220
6012 write (unit = lunit6, fmt = 7012) bus1
7012 format (5x, 'The bus name read from columns 3-8 of the last source card is unrecognizable, not having been previously',/, 5x, 'defined by a branch card or a switch card.   The name read from the card is ', "'", a6,  "'", '.   The user is not', /, 5x, 'allowed to connect sources to network nodes which are not a part of the network previously defined by branch and', /, 5x, 'switch cards of this data case.   After all, such nodes would be completely disconnected from the network, so could', /, 5x, 'not affect the network solution.   Most probably, one or more spelling errors have been made in punching node names.')
  write (unit = lunit6, fmt = 7131)
  go to 6220
6013 write (unit = lunit6, fmt = 7013)
7013 format (5x, ' Simulator voltage outside limits.')
  go to 6220
6014 write (unit = lunit6, fmt = 7014) bus1, bus2, flstat(16)
7014 format (5x, 'Type-93 nonlinear inductance from ', a6, "'", ' to ', "'", a6,  ' has psi-steady (columns 33-38) zero',/, 5x, 'while i-steady (columns 27-32) is nonzero.   Value read for i-steady = ', e12.3, ' .', /, 5x, 'such zero-impedance branches are not permitted.   ')
  go to 6220
6015 write (unit = lunit6, fmt = 7015) lstat(15), lstat(16)
7015 format (5x, 'Breakdown of logic in steady-state setup before call to subroutine number.   next, ii=', 2i10)
  write (unit = lunit6, fmt = 7115)
7115 format (5x, 'This is probably a program bug, not the fault of the user.   Go seek help from program maintenance about this.')
  go to 6220
6016 write (unit = lunit6, fmt = 7016) lstat(16), ntot, lstat(15)
7016 format (5x, "Arrays 'kolum' and 'korder' of the renumbering calculation for the steady-state network have overflowed.", /, 5x, 'the problem is simply too big or too dense (admittance matrix has too many nonzero elements) for the existing array', /, 5x, 'sizes of', i7, '   cells.   Of the', i6, '   matrix rows total, overflow has occurred while working on the', /, 5x, 'simulated elimination for row', i4, '.   This overflow trouble is really trouble with dependent list')
  write (unit = lunit6, fmt = 7116)
7116 format (5x, 'number 99, about which an explanation will follow below.', /, 5x, 'If the core storage needed to prevent this overflow can not be spared, as a last resort', /, 5x, 'the user might bypass the steady-state calculation completely, and simply run in the transients mode for several')
  write (unit = lunit6, fmt = 7216)
7216 format (5x, 'cycles, hoping that by then the solution will have settled sufficiently into the sinusoidal steady-state', /, 5x, '(following the energization shock which occurs at time zero).')
  kill = 1
  lstat(16) = 99
  nchain = 51
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 8943) nchain, kill
  go to 9000
6017 write (unit = lunit6, fmt = 7017)
7017 format (5x, 'Logic breakdown in steady-state solution after renumbering of the s.s. network has been successfully completed.')
  write (unit = lunit6, fmt = 7115)
  write (unit = lunit6, fmt = 7117)
7117 format (5x, 'But then again, maybe it is not the fault of the EMTP.  Several years, of experience with this message have', /, "shown that the user's data was invariably erroneous, though not stopped, at the time the branches were inputted.")
  write (unit = lunit6, fmt = 7217) bus1, lstat(15)
7217 format (5x, 'For example, there might be a branch both ends of which touch node ', "'", a6,  "'", ',   found in row number,   i4', /, 2x, '   of the branch table (order of input).')
  go to 6220
6018 write (unit = lunit6, fmt = 7018) lstat(14), bus1, bus2
7018 format (5x, 'In forming the network admittance matrix for the steady-state phasor solution for initial conditions, trouble ', /, 5x, 'has occurred while processing the ', i2, '-phase coupled r-l elements the first of which connects', /, 5x, 'bus ', "'", a6, "'", ' with bus ', "'", a6, "'", '.   This could either be an actual coupled r-l group, or the series sub-branches ', /, 5x, 'of a multi-phase pi-circuit.   In either case, the coupled-branch-group impedance matrix    (z) = (r) + jw(l)   has ', /, 5x, 'been found to be near-singular (uninvertible).   In particular, a near-zero diagonal element has been found just')
  write (unit = lunit6, fmt = 7118) flstat(11), flstat(12), flstat(13)
7118 format (5x, 'before reciprocation, during the inversion attempt.   Using magnitudes squared for all three quantities, we have', /, 5x, 'original diagonal value =', e12.3, ',   questionable value =', e12.3, ',   tolerance ratio = ', e12.3, '.', /, 5x, 'The user had better check his data values for this coupled group very carefully, to see if something is not abnormal.')
  write (unit = lunit6, fmt = 7218)
7218 format (5x, "The program tolerance ratio for near-zero checking is miscellaneous data parameter 'tolmat'.   Perhaps a", /, 5x, "rereading of the user's manual explanation of this input constant would be in order, should the user still be", /, 5x, 'uncertain of the reason for his trouble.')
  go to 6220
6019 write (unit = lunit6, fmt = 7019)
7019 format (5x, 'Steady-state solution logic trouble.   See program maintenance, since this is their fault.')
  go to 6220
6020 write (unit = lunit6, fmt = 7020)
7020 format (5x, 'Logic breakdown in steady-state solution while building admittance matrix.')
  write (unit = lunit6, fmt = 7115)
  go to 6220
6021 write (unit = lunit6, fmt = 7021)
7021 format (5x, 'Steady-state solution breakdown because over 25 nodes of network are switched together.   This represents', /, 5x, 'overflow of array itemp(??).   To solve the problem, simply redimension  list 26  , or get rid of that inordinate', /, 5x, 'number of switches which are all connected together.   That any legitimate power-system problem should ever reach this', /, 5x, 'termination is highly improbable, and somewhat suspect. Unless the user understands what is wrong, and why,', /, 5x, "some consultation with the user's friendly neighborhood program maintenance man is strongly recommended.")
  go to 6220
6022 write (unit = lunit6, fmt = 7022) lstat(16)
7022 format (5x, 'Big trouble.   Singular nodal admittance matrix involving node number ', i3, ' of steady-state solution has been found.', /, 5x, 'But user bus number for this node could not be found in renumbering map  norder.   Program logic error.')
  write (unit = lunit6, fmt = 7115)
  go to 6220
6023 write (unit = lunit6, fmt = 7023) bus1
7023 format (5x, 'No.   We can not validly set voltage of disconnected subnetwork last identified to zero, since it appears', /, 5x, "that a current source feeds this subnetwork.   For this case, either Kirchhoff's current law for the subnetwork", /, 5x, 'is violated (current in does not equal current out), or the solution is indeterminate (subnetwork voltage solution', /, 5x, 'is only determined up to an arbitrary constant).   In either case, the problem is ill-posed physically, and', /, 5x, 'must be rejected.   User had better reconsider his network in the vicinity of node ', "'", a6, "'", ', to either remove')
  write (unit = lunit6, fmt = 7123)
7123 format (5x, 'the singularity, or the current source(s), or both')
  go to 6220
6024 write (unit = lunit6, fmt = 7024) lstat(16), lstat(15), lstat(14)
7024 format (5x, 'Logic failure near end steady-state solution.   kk, ia, index(ia+1)=', 3i10)
  write (unit = lunit6, fmt = 7115)
  go to 6220
6025 write (unit = lunit6, fmt = 7025) (lstat(i), i = 11, 16)
7025 format (5x, 'Logic trouble within steady-state renumbering.   next, mext, ib, i, jbs, nelim=', 6i8)
  write (unit = lunit6, fmt = 7115)
  go to 6220
6026 write (unit = lunit6, fmt = 7026)
7026 format (5x, 'Logic error in steady-state renumbering.   kolum array has erroneous numbers.')
  write (unit = lunit6, fmt = 7115)
  go to 6220
6027 write (unit = lunit6, fmt = 7127) bus1, bus2, bus3
7127 format (5x, 'Source being read is type-18 ideal-transformer plus source.   But one or more of the column', /, 5x, '3-22 name fields are unrecognized.   Names are ....', 3( 2x, '"', a6, '"'), ' .', /, 5x, 'unless all three are legal EMTP network node names,  the case can not be run.   Correct it.')
  go to 6220
6028 write (unit = lunit6, fmt = 7028)
7028 format (5x, 'The last-read component has been identified as an old-format (pre-', '"m37."', ') zinc-oxide surge arrester.', /, 5x, 'such old data must be converted to the newer format.   The supporting program "zinold" (see rule book', /, 'index) will perform all such conversion for the user automatically in a single pass.  Try it.')
  go to 6220
6029 write (unit = lunit6, fmt = 7029) bus1, bus2, flstat(16)
7029 format (5x, 'Distributed-parameter branch card connecting node.', "'", a6, "'", ' to ', "'", a6, "'",  ' represents propagation mode', /, 5x, 'with travel time (sec) =', e14.5, '.   This is less than time-step size  deltat, a case which is illegal.   Either', /, 5x, 'run with small enough deltat, or replace the distributed modelling by lumped pi-circuit model.')
  if (flstat(16) .eq. 0.0d0) write (unit = lunit6, fmt = 7129)
7129 format (5x, 'The just-printed travel time is seen to be zero, though, which is generally a default value deserving of', /, 5x, 'further comment.   Assuming that the user has actually inputted a line of finite length, the travel time must of', /, 5x, 'course be positive.   A zero value is used for default in the case of a frequency-dependent mode, however, for', /, 5x, "the case where the infinite-frequency travel time is less than the time-step-size  'deltat'  that the user has", /, 5x, 'selected.   To find the actual offending travel-time value, the user can consult the data-card interpretation which')
  if (flstat(16) .eq. 0.0d0) write (unit = lunit6, fmt = 7229)
7229 format (5x, 'goes along with the associated branch card as it was read in.   The EMTP does not have the value in question', /, 5x, "immediately available at this point, and can only tell the user that he's in trouble (without knowing how much).")
  go to 6220
6030 write (unit = lunit6, fmt = 7030) lstat(16)
7030 format (5x, 'From columns 1-2 of last data card, a value of ', i2,  ' was read.   This is illegal.   The card in question', /, 5x, 'should be either an initial-condition card, where values 2, 3, or 4 are legal, or it is an output-specification', /, 5x, 'card, where values 0 or 1 are allowed.   Shape up, fella.')
  go to 6220
6031 write (unit = lunit6, fmt = 7031) bus1
7031 format (5x, 'Last card gives node-voltage initial condition for unrecognizable node.   The name ', "'", a6,  "'", ' which was ', /, 5x, 'punched could not be found in the list of bus names.')
  write (unit = lunit6, fmt = 7131)
7131 format (5x, 'Check name(s) for spelling, as well as the column-positioning of any blanks in the field(s) which is(are)', /, 5x, 'six characters wide.')
  go to 6220
6032 write (unit = lunit6, fmt = 7032) bus1, bus2
7032 format (5x, 'Last card gives initial-conditon currents for linear branch from node ', "'", a6, "'", ' to ', "'", a6, "'.")
7232 write (unit = lunit6, fmt = 7132)
7132 format (5x, 'But the branch referred to could not be found in the appropriate list of branches.   Is the orientation correct.')
  write (unit = lunit6, fmt = 7131)
  go to 6220
6033 write (unit = lunit6, fmt = 7033) bus1, bus2
7033 format (5x, 'Last card gives initial condition currents for non linear branch from node ', "'", a6, "'", ' to ', "'", a6, "'.")
  go to 7232
6034 write (unit = lunit6, fmt = 7034) lstat(13), lstat(14), flstat(15), flstat(16)
7034 format (5x, 'Last card gives initial condition which is illegal for nonlinear element number ', i3, '.   The last point', /, 5x, 'of the nonlinear characteristic is stored in table location ', i3, ' and has value ', e13.4, '.   But initial', /, 5x, 'condition requires a value of ', e13.4,  ', which is off the end of the characteristic.   User must extend the', /, 5x, 'characteristic, if this is really the initial condition that he wants.')
  go to 6220
6035 write (unit = lunit6, fmt = 7035) lstat(16), bus1, bus2, flstat(11), flstat(12), flstat(13)
7035 format (5x, 'Breakdown of solution when using compensation (superposition) on nonlinear element number ', i3, /, 5x, 'which connects node ', "'", a6, "'", ' to ', "'", a6, "'", '. ', /, 5x, 'Thevenin load line does not intersect the nonlinear characteristic.   Maybe user should extend the characteristic', /, 5x, 'assuming that the problem is otherwise well-posed.   Some program solution quantities bearing on this breakdown,', /, 5x, 'potentially useful to your program maintenance man (if not to the user), are as follows ...', /, 5x,  'e(k), e(m), a2 = ', 3e13.4)
  write (unit = lunit6, fmt = 6135)
6135 format (5x, 'Past experience based on a number of encounters with this EMTP error stop has shown that most often the user', /, 5x, 'has under-estimated the peak nonlinear (or time-varying) element ordinate value.   that is, the characteristic', /, 5x, 'has inputted was correct, perhaps, but was simply not defined for large enough values.   Remember that true nonlinear', /, 5x, 'or time-varying elements (with type codes 91, 92, or 93) do not have the final segments on the two ends')
  write (unit = lunit6, fmt = 6235)
6235 format (5x, 'extrapolated linearly to infinity (as is the case with pseudo-nonlinear elements of type codes 97, 98, and 99).', /, 5x, 'Before looking for any more sophisticated trouble, this very simple possibility should always be checked.')
  go to 6220
6036 write (unit = lunit6, fmt = 7036) lstat(16), bus1, bus2, flstat(12), flstat(13), flstat(11)
7036 format (5x, 'Breakdown surrounding nonlinear or time-varying element number ', i3, ' having terminal nodes ', "'", a6, "' and '", a6, "'.", /, 5x, 'This is a time-varying resistance with last time point of characteristic defined by user being for ', e13.4, /, 5x, 'sec.   But ', e13.4, ' sec have elapsed since element was turned on.   User must extend characteristic if study  ', /, 5x, 'his to be run out further than the present simulation time of   t =', e12.3, ' sec   at which this run is now', /, 5x, 'being terminated.')
  go to 6220
6037 write (unit = lunit6, fmt = 7037)
7037 format (5x, 'Last data card was decoded using  4e16.0  and came up with floating point zeros.   This is not legal for the', /, 5x, 'modal impedance data of frequency-dependent generator equivalents as, 29h were being read in "fddata".')
  go to 6220
6038 d1 = flstat(13) / flstat(12)
  write (unit = lunit6, fmt = 7238)
7238 format (5x, 'Matrix (y) is the nodal admittance matrix which must', /, 5x, 'be re-triangularized at every time-step that a switch orpseudo-nonlinear element changes its status.')
  write (unit = lunit6, fmt = 7038) lstat(16), bus1, flstat(14), d1, flstat(12)
7038 format (5x, 'There has just been a breakdown factoring (y), while eliminating on row', i4, '  which corresponds to node ', '"', a6, '".   After  ', /, 5x, 'elimination to the left of the diagonal in this row, the diagonal element was found to equal ', e13.4, ', While', /, 5x, 'The original value before any elimination was performed was', e13.4,  '.   The ratio of these two numbers', /, 5x, 'exceeds the near-zero tolerance of', e13.4,  ", which is miscellaneous data parameter 'epsiln'.")
  write (unit = lunit6, fmt = 7338)
7338 format (5x, 'The program thinks that either the node in question, or a subnetwork to which it belongs, is disconnected from the')
  write (unit = lunit6, fmt = 7138)
7138 format (5x, 'rest of the network.   If actually disconnected, user should add a connecting branch.   if connected by a very-high-', /, 5x,'impedance branch, either make  epsiln  nearer to zero, or decrease the impedance of the connection.', /, 5x, "but be careful in resorting to a smaller 'epsiln', as numerical solution difficulty will inevitably result as", /, 5x, "'epsiln' approaches the computer roundoff limit (determined by computer word-size).   The user is advised to reread")
  write (unit = lunit6, fmt = 7438)
7438 format (5x, "the EMTP user's manual explanation of the significance of 'epsiln', should he be in doubt.   A call to program", /, 5x, 'maintenance might also be in order, in this case.')
  write (unit = lunit6, fmt = 7538)
7538 format (/, 5x, 'Hit might be desirable to say a few words about how the printed 6-character node name, and the subnetwork', /, 5x, 'in question, are related.   Having an isolated subnetwork means that the system matrix which is being triangularized', /, 5x, 'his block diagonal, with the problem subnetwork then associated with a singular (uninvertible) block.   This is', /, 5x, 'decoupled from the rest of the system, and mathematically the associated equations have an indeterminate solution.')
  write (unit = lunit6, fmt = 7638)
7638 format (5x, 'This shows up as a zero diagonal element while eliminating on the row for the last node of the subnetwork.', /, 5x, 'which node will be the last of the subnetwork is a function of internal EMTP renumbering, as well as the order of', /, 5x, 'the network input data.   Hence the user should not assume that there is trouble specifically at the node whose', /, 5x, 'name is printed above.   Use of network connectivity printout will allow reconstruction of the problem topology')
  write (unit = lunit6, fmt = 7738)
7738 format (5x, 'in the vicinity of the printed node.   The user is advised to go as far away from this node in all directions', /, 5x, 'as is necessary, until the limits to the subnetwork in question become clear.   A knowledge of switch status at the', /, 5x, 'instant of difficulty is of course crucial.   Often this error stop will occur just as a switch opens, thereby', /, 5x, 'segmenting the system into two pieces, one of which has no paths to ground (which then becomes the disconnected', /, 5x, 'subsystem).')
  go to 6220
6039 write (unit = lunit6, fmt = 7039) lstat(16), bus1, bus2
7039 format (5x, 'Solution breakdown on nonlinear or time-varying element number ', i3, ' connecting node ', "'", a6, "'", ' with node ', "'", a6, "'.", /, 5x, 'one or more other such elements is actually touching this one, rather than being separated from it by the required', /, 5x, 'travel time.')
  write (unit = lunit6, fmt = 7139)
7139 format (5x, 'While the problem may be well-posed physically, the present EMTP solution assumptions concerning the use of', /, 5x, 'compensation cannot handle such a case.   To remedy this dilema, the user has two general potential courses of', /, 5x, 'action ....   ', /, 8x, '1) eliminate the offending nonlinear or time-varying element(s).   If the offending elements are nonlinear', /, 11x, 'rather than time-varying, use might be made of switched or pseudo-nonlinear elements of the same type.')
  write (unit = lunit6, fmt = 7239)
7239 format (11x, 'While this converts true nonlinearities to pseudo-nonlinearities which inherently lag one time step', /, 11x, 'behind the solution (and thus must be used with caution to ensure that movement along the desired characteristic', /, 11x, 'is slow), one does avoid all isolation problems associated with compensation.', /, 8x, '2) introduce sufficient stub lines (small series-inductance elements which are represented by lossless distributed-', /, 11x, "parameter lines having travel-time equal to the time-step-size 'deltat') to isolate the interfering elements.")
  write (unit = lunit6, fmt = 7339)
7339 format (11x, "If 'deltat' is small enough, such added elements may not seriously disturb the problem physically, while", /, 11x, 'providing the needed isolation.', /, 5x, 'Has both of these remedies are potentially dangerous if not used with considerable caution and understanding, it', /, 5x, "is suggested that the user apply some thought to the matter while studying the user's manual about these questions.", /, 5x, 'a call to program maintenance for some professional advice might also be appropriate, if in doubt.')
  go to 6220
6040 write (unit = lunit6, fmt = 7040) lstat(15), lstat(16), bus5, bus6, bus3, bus4
7040 format (5x, 'Solution breakdown using compensation method on nonlinear or time-varying elements number ', i3, ' and ', i3, '.', /, 5x, 'The first connects node ', "'", a6, "'", ' with node ', "'", a6, "'", ', while the second has terminal nodes ', "'", a6, "'", ' and ', "'", a6, "'.", /, 5x, 'Tthese two elements are not separated by the required travel time (distributed-parameter line).')
  write (unit = lunit6, fmt = 7139)
  write (unit = lunit6, fmt = 7239)
  write (unit = lunit6, fmt = 7339)
  go to 6220
6041 write (unit = lunit6, fmt = 7041) lstat(10), bus1, bus2
7041 format (5x, 'Switch number ', i3, ' connecting node ', "'", a6, "'", ' with node ', "'", a6,  "'", ' has closed, causing trouble.')
  go to 6040
6042 write (unit = lunit6, fmt = 7142)
7142 format (5x, 'Matrix  (Yaa)  is the first-partition diagonal submatrix among non-switch (or pseudo-nonlinear-element)', /, 5x, 'nodes which is to be triangularized once and for all outside of the time-step loop.')
  write (unit = lunit6, fmt = 7042) lstat(16), bus1
7042 format (5x, 'Row ', i3, ' in Yaa (matrix for nonswitch rows) has identically-zero diagonal element before starting elimination.', /, 5x, 'This corresponds to node ', "'", a6, "'", ', where there is either nothing connected, or there is a very strange, perfect', /, 5x, 'resonance.   The user should check all branches connected to the node in question.   If there are none, put one in or', /, 5x, 'drop the node in question.')
  go to 6220
6043 write (unit = lunit6, fmt = 7142)
  d1 = flstat(13) / flstat(12)
  write (unit = lunit6, fmt = 7043) lstat(16), bus1, flstat(14), d1, flstat(12)
7043 format (5x, 'There has been a breakdown in the triangularization of (Yaa) while working on row', i4, ', which corresponds', /, 5x, 'to node ', "'", a6, "'", '.   After elimination to the left of the diagonal in this row, the diagonal element has been', /, 5x, 'found to equal', e13.4, ', while the original matrix value (before any elimination) was', e13.4, '.   The', /, 5x, 'ratio of these two numbers exceeds the near-zero tolerance of', e13.4,  ", which is misc. data parameter 'epsiln'.")
  write (unit = lunit6, fmt = 7338)
  write (unit = lunit6, fmt = 7138)
  write (unit = lunit6, fmt = 7438)
  go to 6220
6044 write (unit = lunit6, fmt = 7044)
7044 format (5x, 'You dummy, you forgot to request output variables. If the branch cards have no column-80 branch-output', /, 5x, 'requests, and no node voltages were requested for output, there is nothing to look at by way of a problem solution.', /, 5x, 'No variables are to be printed, and none can be plotted.  The EEMTP will not let you run a problem this way.', /, 5x, 'Go back to the keypunch and add some output, do not pass go, do not collect 200 dollars.')
  go to 6220
6045 write (unit = lunit6, fmt = 7145) lstat(17), lstat(14), flstat(15), flstat(16)
7145 format (5x, 'Weighting function a', i1, '(t) just inputted does not have all, which are monotone increasing.', /, 5x, 'in particular, point number ', i3, ' is for time', e15.6, ' sec, while the preceding point was for', /, 5x, 'time', e15.6, '.   Perhaps the card deck has been shuffled, or somehow altered from that as punched by the', /, 5x, 'weighting-function supporting program.')
  go to 6220
6046 write (unit = lunit6, fmt = 7046) lstat(15), flstat(15), lstat(14)
7046 format (5x, 'The weighting function frequency dependent line mode number ', i2, ' has failed ', /, 5x, 'during an attempt to improve the low frequency representation ', /, 5x, 'by adjusting the exponential time constant of the response tails ', /, 5x, 'to give the theoretically correct areas.  The adjustment was requested', /, 5x, 'by a non-zero value = ',e10.5,  ' for "rtotal" in columns 65 to 72 ', /, 5x, 'of the miscellaneous data card, while a permissible iteration count of nii = ', i4, /, 5x, 'was provided for the Newton-Raphson algorithm in columns 73 to 80')
  go to 6220
6047 write (unit = lunit6, fmt = 7047) lstat(16)
7047 format (5x, 'Switch-array overflow due to too many saturable transformers.   The switch tables are used for scratch', /, 5x, 'storage of xformer starting-and-ending data pointers, during branch-data input.   The user has inputted more', /, 5x, 'saturable xformers than the program limit for switches, which is ', i3,  '.   Either reduce the former, or increase', /, 5x, 'the latter.')
  go to 6220
6048 write (unit = lunit6, fmt = 7048) bus3
7048 format (5x, 'The last data card inputted requests a copy of a preceding saturable transformer which purportedly bears ', /, 5x, 'internal identifying bus name ', "'", a6,  "'", '.   But a search of preceding components has failed to locate such a component.', /, 5x, "Maybe the user's data is out of order, or he has misspelled the name in question.")
  write (unit = lunit6, fmt = 7131)
  go to 6220
6049 if (bus1 .ne. trash) go to 7249
  write (unit = lunit6, fmt = 7349) ksat, itype
7349 format (5x, 'The user is currently inputting a saturable transformer component, which by definition must have two or more', /, 5x, 'windings.   The last-read data card should have been a winding card for winding number', i4, ' ,   but this is not', /, 5x, 'what was punched in columns  1-2 .   A value of', i4, '   was read from this data field.')
  go to 6220
7249 write (unit = lunit6, fmt = 7049) bus1, lstat(16)
7049 format (5x, 'The user is currently inputting a saturable transformer, using the reference branch procedure to get a copy', /, 5x, 'of the unit bearing internal identifying bus name ', "'", a6, "'", '.   But that unit had ', i2,  ' windings.   The coil', /, 5x, 'cards being read are either out of natural order (check columns 1-2 sequence numbers), or they represent an', /, 5x, 'incorrect number of windings.   Recall that the winding cards must be numbered 1, 2, etc. ----- consecutively,', /, 5x, 'with no missing numbers.   Once a break in this natural order has been detected for the column 1-2 numbers, the')
  write (unit = lunit6, fmt = 7149)
7149 format (5x, 'EMTP logic assumes that data for the transformer in question has been completed.   This is why shuffled', /, 5x, "winding cards produce an 'erroneous-number-of-turns' message.")
  go to 6220
6050 write (unit = lunit6, fmt = 7050) bus6, lstat(15)
7050 format (5x, 'Data error has been detected while working on input of 3-phase transformer component bearing', /, 5x, 'internal bus-name identifier ', "'", a6,  "'", '.   The programexpects 3 single-phase transformer components', /, 5x, 'to follow the three-phase card, contiguously.   The last-read branch has been found after only  ', i3, /, 5x, 'single-phase components.')
6220 lastov = nchain
  nchain = nfrfld + 50
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 8943) nchain, kill
8943 format (' Exit "subr51".  nchain, kill =', 2i6)
9000 return
end subroutine subr51

!
! file over51.f90
!
