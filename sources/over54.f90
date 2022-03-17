!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over54.f90
!

! Copyright 1977-2021 Bonneville Power Administration
! Copyright 2019-2021 Angelo Rossi <angelo.rossi.homelab@gmail.com>
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice,
!    this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! 3. Neither the name of the copyright holder nor the names of its contributors
!    may be used to endorse or promote products derived from this software
!    without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.

!
! subroutine over54.
!

subroutine over54
  use blkcom
  use volpri
  implicit none
  integer(4) :: j
  integer(4) :: n1
  real(8) :: d1, d2
  !
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4567)
4567 format ('  "Begin module over54." ')
  if (nchain .ne. 54) go to 99999
  n1 = kill - 150
  !  go to (6151 , 6152, 6153, 6154, 6155, 6156, 6157, 6158, 6159, 6160, 6161 , 6162, 6163, 6164, 6165, 6166, 6167, 6168, 6169, 6170, 6171 , 6172, 6173, 6174, 6175, 6176, 6177, 6178, 6179, 6180, 6181 , 6182, 6183, 6184, 6185, 6186, 6187, 6188, 6189, 6190, 6191 , 6192, 6193, 6194, 6195, 6196, 6197, 6198, 6199, 6200), n1
  select case (n1)
  case (1)
6151 write (unit = lunit(6), fmt = 7151)
7151 format (5x, "The present data case has a positive parameter  'nenerg'  (read from columns 65-72 of the floating-point",/, &
          5x, "miscellaneous data card), indicating that random switch closing times for  'statistics'  switches are to be",/, &
          5x, "generated.   Since parameter  'isw'  was punched with the key request value  '4444' ,   Only an analysis of the",/, &
          5x, 'switch closing times is desired (no EMTP simulations are to be performed).   but such an analysis is not possible, ')
     write (unit = lunit(6), fmt = 7251)  lstat(14), lstat(15)
7251 format (5x, 'due to a shortage of available working space in memory.   Only', i6,  '  integer cells of   /label/   are',/, &
          5x, 'available, while', i7,  '  such cells are required in order to execute this data case.   A formula for this',/, &
          5x, 'required minimum integer storage space is ....',/, 30x, 'min  =  ( 3 + ns*(ns-1)/2 ) * nc',/, &
          5x, "where  'ns'  is the number of  'statistics'  switches,  and  'nc'  is the number of compartments of the ")
     write (unit = lunit(6), fmt = 7351)  lstat(16), lstat(13)
7351 format (5x, 'cumulative distribution function tabulation.   For this data case,   ns =', i4,  ' ,   while   nc =', i4, ' ,',/, &
          5x, "with the latter figure equal to twice parameter  'sigmax' divided by  'aincr'  (see  'statistics'  miscellaneous",/, &
          5x, "data card parameters).   in order to run this test of the random number generator, either the number of  'statistics'",/, &
          5x, 'switches or tabulation compartments must be appropriately decreased, or the EMTP must be re-dimensioned so as ')
     write (unit = lunit(6), fmt = 7451)
7451 format (5x, 'to provide for more working space in memory.   If the latter course is taken, it is recommended that all of',/, &
          5x, 'increased space be confined to lists number  5,  7,  8,  13 ,  14,  or  15,  in which case there is no waste or ',/, &
          5x, 'loss (the increase is  100 per cent effective). ')
     go to 6220

  case (2)
6152 write (unit = lunit(6), fmt = 7152)  lstat(14)
7152 format (5x, 'execution of the line constants program can not be allowed to continue, due to a shortage of core storage for',/, &
          5x, 'use as tabular working space.   The EMTP is variably-dimensioned, of course, with the present size of   /label/',/, &
          5x, 'simply insufficient to allow for even the solution of a single-conductor problem.   Available dimensioned working',/, &
          5x, "space in subroutine  'vdov44'  is", i5,  '  cells.   Fitting all matrices into this constraint requires that ')
     write (unit = lunit(6), fmt = 7252)  lstat(15)
7252 format (5x,  'a conductor limit of', i5,  '  conductors be imposed.   The user must redimension the EMTP in order to',/, &
          5x, 'provide for more   /label/   storage (with the distribution of this storage among the different lists being',/, &
          5x, 'immaterial), and resubmit the job. ')
     go to 6220

  case (3)
6153 write (unit = lunit(6), fmt = 7153)  bus1
7153 format (5x, 'As part of the tacs data which has now been completely read by the EMTP, the user has employed the same six-',/, &
          5x, 'character alphanumeric name  ', "'", a6,  "'", '  for two different tacs variables.   Unfortunately for the user,',/, &
          5x, 'homographs are to be exiled for life from the EMTP world.   Yet should this succinct characterization be less',/, &
          5x, 'than perfectly clear, suffice it to say that it is a violation of EMTP rules for the user to apply the same  a6 ')
     write (unit = lunit(6), fmt = 7253)
7253 format (5x, 'name to what are supposed to be two distinct tacs variables. ')
     call over54err7421
     go to 6220

  case (4)
6154 write (unit = lunit(6), fmt = 7154)
7154 format (5x, "The EMTP has been inputting data for a  'Semlyen setup'  case, and has discovered an inconsistency or",/, &
          5x, 'illegality in the numerical values which were punched by the user.   Specifically, ')
     write (unit = lunit(6), fmt = 7254)  lstat(13), lstat(14)
7254 format (5x, "data parameters  'iotx' ,  'ioss' ,  and  'iofl' are required to be octal (base-8) numbers.   Digits",/, &
          5x,  " '8'  and  '9'  are thus illegal (nonexistant), and must not be used.   But the field for number",  i4,   '   among',/, &
          5x, 'these was punched with the value',  i6,  ' .   naughty, naughty. ')
     go to 6220

  case (5)
6155 write (unit = lunit(6), fmt = 7155)   lstat(13), lstat(14)
7155 format (5x, "Within the computations for a  'semlyen setup'  data case, the iterative procedure which is used for fitting",/, &
          5x, 'the characteristic-admittance step-response has failed to converge.   This fitting is done by means of an',/, &
          5x, 'algebraic adjustment in the frequency domain, not the more well-known fitting of exponentials in the time domain.',/, &
          5x, 'the iteration limit of',  i5,  "   (parameter  'nitery')  was reached for mode number",  i4,  '   without ')
     write (unit = lunit(6), fmt = 7255)  flstat(13), flstat(14)
7255 format (5x,  "satisfying tolerances  'epsyc'  and  'epsn' ,   which have values",    e15.3,    '   and',    e15.3,   ' ,',/, &
          5x, "respectively.   the user is reminded that these just-mentioned  'Semlyen setup'  parameters are under user",/, &
          5x, "control by means of a special  'tolerances'  request-word card, if this will help any.   Alternatively, did the",/, &
          5x, 'user perhaps already redefined default values by means of such a card, thereby making convergence impossible,',/, &
          5x,  'the EMTP wonders. ')
     go to 6220

  case (6)
6156 write (unit = lunit(6), fmt = 7156) lstat(14)
7156 format (5x, "The present   'Semlyen setup'   data case has proven to be too much for the EMTP logic, for mode number",  i4, ' . ')
     if (lstat(19) .eq. 40080) go to 7556
     if (lstat(19) .eq. 4545 .or. lstat(19) .eq. 4557) go to 7956
     write (unit = lunit(6), fmt = 7256) lstat(15)
7256 format (5x, ' The toe of the step response has been located, based on the speed of the highest frequency specified . ', /, &
          5x, "The time step used in the fitting process is established by a technique determined by parameter 'kfit'", /, &
          5x, 'for which the user has supplied a value of ', i2, ' .' , /, &
          5x, 'if  "0"    the time step is simply toe time / noo  ', /, &
          5x, 'if  "1"    the time step is sized to put 25" of the specified "npoint" points between the toe', /, &
          16x, 'and the time at which the value is 2/3 .', /, &
          5x, 'if  "2"    the time step is sized to put 5" of the specified "npoint" points between the 1/3 and the 2/3  times . ')
     write (unit = lunit(6), fmt = 7356) lstat(11)
7356 format (5x, 'Determining the 1/3 point or the 2/3 point may not be possible because of noise at small times in inverse Fourier routines . ',/, &
          5x, 'This run has suffered from this difficulty', 5x, i4, ' iterations have failed to locate one of these points . ',/, &
          5x, 'The solutions available are "  "a" different value of "kfit", ',/, &
          5x,  '"b" a higher number for "nfit" which controls the permissible no. of iterations,  ',/, &
          5x, '"c" a higher frequency range and/or more frequency points in an attempt to improve the accuracy of the ifr . ')
     write (unit = lunit(6), fmt = 7456) flstat(11), flstat(16), lstat(10)
7456 format (5x, 'other parameters relevent to this non-converged iteration are as follows : ',/, &
          5x, 'the initial size of the time increment was ', e15.5,  ' .  The target value was ', e15.5  ,/, &
          5x,  'the number of points "npoint" as requested by the user =', i4,  ' . ')
     go to 6220
7556 write (unit = lunit(6), fmt = 7656)  lstat(13)
7656 format (5x, "The technique for fitting of the admittance step response has been determined by the first column in parameter 'kfit'",/, &
          5x, 'for which the user has supplied a value of', i2, ' . ')
     write (unit = lunit(6), fmt = 7756)  flstat(16)
7756 format (5x, "if '1', the user has requested a time domain fit, which requires the program to determine the time period to be fitted.",/, &
          5x, 'in this process the program searches a limited time frame for a change in response of ', f4.2,  ' of the initial value .',/, &
          5x,  'Tthe program has been unable to locate this value, either because the period searched was too short,',/, &
          5x,  'or because the ift routine failed to produce it.',/, &
          5x,  'a wider frequency range or more numerous frequency data may improve the ift result.',/, &
          5x,  "a value of '2' will result in a preliminary frequency domain fit to determine the time period. ")
     write (unit = lunit(6), fmt = 7856) flstat(11),flstat(13),lstat(10)
7856 format (5x, 'Other parameters relevant to this search failure are as follows: ',/, &
          5x,  'the initial size of the time increment was ',e15.5,  ' .   The time zone searched was ',e15.5 ,/, &
          5x,  "the number of points 'npoint' as requested by the user = ", i4)
     go to 6220
7956 write (unit = lunit(6), fmt = 8056)
8056 format(5x, 'The Newton-Raphson algorithm, used to fit the line response with exponentials, is in trouble. ',/, &
          5x, 'either the jacobian matrix is singular, or the initial guess made by the program is not close enough.',/, &
          5x,  'after carefully checking data, a consultation with program maintenance is indicated. ')
     go to 6220

  case (7)
6157 write (unit = lunit(6), fmt = 7157)
7157 format (5x, "The 'Semlyen setup'  data case now under consideration requires that modal step responses be computed first,",/, &
          5x, 'by inverse fourier transformation.   This procedure has now been completed. ')
     if (lstat(14) .eq. 0) go to 7457
     write (unit = lunit(6), fmt = 7257)  lstat(14), lstat(15),  flstat(14)
7257 format (5x, 'But the step response for mode number',  i4, '   is physically invalid.   In theory, this should begin   ',/, &
          5x, 'at zero, and should rise monatonically to unity, as time goes to infinity.   But the value for the final',/, &
          5x, 'point number',  i5,  '   is',    e14.3,     ' ,   which is nowhere near unity.   In fact, this final value does ')
     write (unit = lunit(6), fmt = 7357)  flstat(15)
7357 format (5x, "not even reach the threshold  'fit2z' ,   which equals",    e14.3,     ' .    the step response is thus',/, &
          5x, 'invalid, and execution will not be allowed to continue. ')
     go to 6220
7457 write (unit = lunit(6), fmt = 7557)  lstat(16), lstat(15), lstat(17), flstat(14), flstat(15), flstat(16)
7557 format (5x, 'But between points number',  i6,  '   and',  i6, ' ,   the rise to unity is not characterized by',/, &
          5x,  'monotone-decreasing slope (concave downward).   At point number',  i6,  " ,   there is trouble,   let's denote",/, &
          5x,  "this point by  'k' .   Values for points  k-1, k, % k+1 then are  ...",     3e14.5,    '  .')
     go to 6220

  case (8)
6158 write (unit = lunit(6), fmt = 7158)
7158 format (5x, "The data case under study involves the conversion of EMTP data cards for a saturable  'transformer'  to",/, &
          5x, "(r), (l)  format (type-51, 52, ... ).   This is a feature of supporting routine  'xformer' .   But there is",/, &
          5x, "trouble with the user's  'transformer'  data which is now being read.   Specifically, ... ")
     n1 = lstat(14)
     if ( n1  .eq.  1 ) write (unit = lunit(6), fmt = 7258)  bus5
7258 format (8x, 'The EMTP is just beginning a new conversion case, and it expects the last-read data card (since non-blank) to',/, &
          8x, "have key word  'transformer'  punched in columns  3-14 .   But columns  3-8  contain   ",  "'", a6,  "'", ' . ')
     if ( n1  .eq.  2 ) write (unit = lunit(6), fmt = 7358)
7358 format (8x, 'The data card for winding number one can not be located.   Column  2  must be punched with unity, of course,',/, &
          8x, "and column  1  must be either blank or be punched with zero.   Reread the rules governing saturable  'transformer'",/, &
          8x, 'component, and correct the data accordingly. ')
     if ( n1  .eq.  3 ) write (unit = lunit(6), fmt = 7458)
7458 format (8x, "Program working space allows for a maximum of  19 windings,   which has been exceeded by the user's data. ")
     go to 6220

  case (9)
6159 write (unit = lunit(6), fmt = 7159)  kill
7159 format(5x, 'invalid kill code =', i5)
     go  to  6220

  case (10)
6160 write (unit = lunit(6), fmt = 7160)  lstat(14)
7160 format(5x, 'The frequency card containing the number of decades and the number of frequencies per decade',/, &
          5x, 'ik and ips respectively, for the inverse Fourier transform',/, &
          5x, 'has requested a total number of frequencies greater than the available storage of ', i5,/, &
          5x, 'surely with a geometric progression of frequencies, this many steps is not necessary. ')
     go to 6220

  case (11)
6161 write (unit = lunit(6), fmt = 7161)
7161 format (5x, " The last-read data card bears the special request word  'transformer '   in columns  3-14, and is the first",/, &
          5x, 'of several cards used to define a saturable transformer component.   But the user has failed to punch columns',/, &
          5x, "39-44  (field  'bustop' )   with a non-blank 6-character (a6) alphanumeric name.   This is the node name for the",/, &
          6 5x, 'extra, internal node of the saturable transformer component, at the top of the magnetizing branch (see transformer ')
     write (unit = lunit(6), fmt = 7261)
7261 format (5x, "equivalent circuit in the EMTP user's manual).   The data field  'bustop'  for each saturable transformer",/, &
          5x, 'component must be non-blank, and all should be unique --- different from each other, and also different from all',/, &
          5x, 'other EMTP network nodes. ')
     go to 6220

  case (12)
6162 write (unit = lunit(6), fmt = 7162)  ipunch
7162 format (5x, "The user is running a  'line constants'  derivation in which the EMTP automatically loops over the different",/, &
          5x, 'frequencies of interest.   On the frequency card which requested this mode of program execution, the field for',/, &
          5x, "EMTP variable  'ipunch'  (columns  66-68 ,   read using i3  format)  contains an illegal numerical value.",/, &
          5x, 'only values  0,  1,  2,  3,  88,  and  89  are recognized as being legitimate, although a value of  ', "'", i3,  "'", '  was ')
     write (unit = lunit(6), fmt = 7262)
7262 format (5x,  "read from the user's frequency card.   Correct this value, and try again. ")
     go to 6220

  case(13)
6163 write (unit = lunit(6), fmt = 7163)  lstat(14)
7163 format (5x,  'The last-read data card is for a tacs source of type', i5,  ' (type code as read from columns 1-2 of the source',/, &
          5x, "card).   But no valid EMTP electric-network node names were specified by the user on either a preceding   'tacs",/, &
          5x, "EMTP sources'   card,  or on a preceding   'tacs outputs' card.   Hence the EMTP concludes that there is no",/, &
          5x, 'electric network, that the present problem being inputted is a tacs stand-alone case.   But tacs source types ')
     write (unit = lunit(6), fmt = 7263)
7263 format (5x, ' 90,  91,  etc.  all come from the electric-network side of the EMTP, by definition.   In sum, the user has',/, &
          5x, 'requested that a tacs source be determined by a non-existent part of the problem.   This discrepancy must be',/, &
          5x, 'corrected, before execution will be allowed to continue. ')
     go to 6220

  case (14)
6164 write (unit = lunit(6), fmt = 7164)
7164 format (5x, '  Unused.     ')
     go to 6220

  case (15)
6165 write (unit = lunit(6), fmt = 7165)
7165 format(5x, 'During calculation of eigenvalues and eigenvectors by the power method, ',/, &
          5x, 'the iteration count has exceeded "nieig".  This limit and the corresponding tolerance "epseig"',/, &
          5x, 'may be changed by a "tolerances" card. ')
     go to 6220

  case (16)
6166 write (unit = lunit(6), fmt = 7166)  csepar, chcont
7166 format (5x, 'The last-read data card is assumed by the EMTP to have free-format data on it, due to the presence of one or',/, &
          5x, 'more separator characters  ',  "'", a1, "'", '  or  continuation-card characters  ',  "'", a1, "'",  ' .   Well, EMTP control was within module',/, &
          5x, "'frefld'  of utpf overlay  (main00, -1)  when the data ran out.   That is, control was looking for more data items ")
     write (unit = lunit(6), fmt = 7266)
7266 format (5x, 'than have been punched on the card.   The user should first verify that the data card in question was intended',/, &
          5x, 'to be free-field data format.   Then, he should check all data items, counting separator characters (which',/, &
          5x, 'define the number of data items). ')
     go to 6220

  case (17)
6167 n1 = iabs( lstat(14) )
     write (unit = lunit(6), fmt = 7167)  branch, bus4, n1
7167 format (5x,  "The last-read data card has the  'bus3'  field of columns  15-20  punched with   ",  "'", a6,  "'", ' ,   so that the',/, &
          5x, "'bus4'  field of columns  21-26  is to be the name of the branch.   But the 6-character text used for this name,",/, &
          5x, "'",  a6,  "'", ' ,   is not new and unique as required by EMTP law.   90 days in jail or a $100 fine.   Row number',  i6, '   of the ')
     if ( lstat(14)  .gt.  0 ) write (unit = lunit(6), fmt = 7267)
7267 format (5x,  'Linear branch table contains a linear branch which has already been so named. ')
     if ( lstat(14)  .lt.  0 ) write (unit = lunit(6), fmt = 7367)
7367 format (5x, 'Nonlinear branch table contains a nonlinear or pseudo-nonlinear branch which has already been so named. ')
     go to 6220

  case (18)
6168 write (unit = lunit(6), fmt = 7168)
7168 format (' Unused.')

  case (19)
6169 write (unit = lunit(6), fmt = 7169)
7169 format (' Unused.')

  case (20)
6170 write (unit = lunit(6), fmt = 7170)  lstat(14)
7170 format (5x, "The last-read data card is a  'line constants'  frequency card which belongs to the interior data of another",/, &
          5x, "EMTP supporting program (e.g.,  'Semlyen setup' ).   But the EMTP limit on such individually-specified frequencies",/, &
          5x, 'is',  i5,  ' ,   which has been overflowed by the last-read data card.   Dimensioning of the crucial arrays in',/, &
          5x, 'question is not under user control.   A complete EMTP recompilation would be required in order to increase this',/, &
          5x,  'limit, should this prove to be really necessary. ')
     go to 6220

  case (21)
6171 write (unit = lunit(6), fmt = 7171)  xopt, copt
7171 format (5x, 'The branch card now being processed represents a request for semlyen recursive convolution modeling.   But',/, &
          5x, "the user specified floating-point miscellaneous data parameters  'xopt'  and  'copt'  as",    e13.4,  '   and',/, &
          5x,    e13.4,  ' ,   respectively.   This is illegal.   Both parameters must be equal, and also nonzero. ')
     go to 6220

  case (22)
6172 write (unit = lunit(6), fmt = 7172)
7172 format (5x, "The EMTP has been reading data within the  'cable constants'  supporting program, with the last-read data card",/, &
          5x, 'being the miscellaneous data card of a new case.   But the numbers punched hereupon are inconsistent.   Specifically, ')
     write (unit = lunit(6), fmt = 7272)  lstat(14)
7272 format (5x,  'a value of', i5,  "   was read from the field  'itypec'  of columns  1  through  5  of the card, using",/, &
          5x, ' i5  format.   But only the values  1  (for overhead line constants) and  2  (for cable constants) are legal, if',/, &
          5x, "the card in question is nonblank (recall that a blank card marks the end of such cases within  'cable constants' ). ")
     go to 6220

  case (23)
6173 write (unit = lunit(6), fmt = 7172)
     write (unit = lunit(6), fmt = 7173)
7173 format (5x, "field  'isyst'  of columns  6  through  10  has been punched with the integer value  -1 ,   which means that",/, &
          5x, "an underground cable system is to be under consideration.   but field  'iearth'  of columns  16  through  20",/, &
          5x, 'has been punched with the integer value  99 ,   which means that the stratified-earth option is desired.   But',/, &
          5x, 'this combination of features is illegal (the EMTP is incapable of finding the parameters for an underground',/, &
          5x, 'cable when the earth is not homogeneous and uniform). ')
     go to 6220

  case (24)
6174 write (unit = lunit(6), fmt = 7174)
7174 format (5x, "All conductor cards of an overhead-line case within the  'cable constants'  supporting program have now been",/, &
          5x, 'read.   But one or more of these cards has been punched with illegal or inconsistent data.   Specifically, ')
     n1 = 3 * lstat(14) - 1
     write (unit = lunit(6), fmt = 7274)  n1, flstat(15), flstat(16)
7274 format (5x, 'non-comment card number', i4,  '  after the miscellaneous data card bears geometrical conductor data which',/, &
          5x,  'is physically impossible.   The inner radius of the conductor has been read as',    e16.4,        '   meters, while',/, &
          5x,  'the outer radius is',    e16.4,     ' .    This violates the EMTP restriction that the tubular conductor',/, &
          5x,  'thickness must be positive.   Zero or negative thickness is not allowed. ')
     go to 6220

  case (25)
6175 write (unit = lunit(6), fmt = 7175)  lstat(15), flstat(14)
7175 format (5x, "Subroutine  'eigen'  is used to calculate eigenvalues of the   (z)(y)   matrix product, within the  'cable",/, &
          5x,  "constants'  supporting program.   But within the iteration limit of",  i5,   ' ,   the iterative algorithm has',/, &
          5x,  'failed to satisfy the convergence tolerance of', e14.2,  ' .    The resulting eigenvalues must be',/, &
          5x,  'viewed with suspicion (at best), so the solution is being terminated at this point. ')
     go to 6220

  case (26)
6176 write (unit = lunit(6), fmt = 7176)
7176 format(5x, 'Error is in synchronous machine electrical data.',/, &
          5x,  'data has one or more of the following errors' ,/, &
          10x, '1. xl not smallest reactance ',/, 10x, '2. xdpp not smaller than xdp ',/, &
          10x, '3. xdp  not smaller than xd  ',/, 10x, '4. xqpp not smaller than xqp ',/, &
          10x, '5. xqp  not smaller than xq  ',/, 10x, '6. one or more reactances or ra is negative ',/)
     go to 6220

  case (27)
6177 write (unit = lunit(6), fmt = 7177)  numsm
7177 format (5x, 'The EMTP is presently in the process of reading EMTP source cards, with the last-read card producing a',/, &
          5x, 'violation of the ordering restriction which is applicable when dynamic synchronous machine (s.m.) source',/, &
          5x, 'components are present.   Recall that all non-s.m. source cards must precede any s.m. source cards, in order of',/, &
          5x,  'data input.   Thusfar there have been', i4,   '   s.m. source components inputted (with an sce dual machine ')
     write (unit = lunit(6), fmt = 7277)  lstat(14)
7277 format (5x, 'counting as just one source component), before the last-read data card.   But the last data card has the field',/, &
          5x,  "of source type-code  'itype'  (columns 1-2) punched with the integer value", i5,  ' ,   which is not a legal',/, &
          5x, 'characterization for a s.m. source component.   The last-read data card is thus either erroneous by itself, or',/, &
          5x, 'out of order (it must precede the first s.m. source component). ')
     go to 6220

  case (28)
6178 write (unit = lunit(6), fmt = 7178)  lstat(14)
7178 format (5x, 'The EMTP is presently inputting source components, with the last-read data card serving to define the network',/, &
          5x, 'connection for the third and final phase of a 3-phase dynamic synchronous machine (s.m.) component.   But the',/, &
          5x, "source type-code for this component is illegal (unrecognizable).   From the field  'itype'  (columns 1-2) of",/, &
          5x, 'the first data card for this s.m. component --- two non-comment cards before the last-read data card --- was',/, &
          5x, 'read a type code of',  i2,    ' .    But only s.m. model types  51  through  54  and  59  are presently available. ')
     write (unit = lunit(6), fmt = 7278)   numsm
7278 format (5x, 'Further, the user is allowed to use either machine types  51-54 ,   or type  59  ---- but not a mixture of',/, &
          5x, 'the two.   Recall that these two classes of s.m.  are quite different, utilizing different solution methods.',/, &
          5x,  'the just-read dynamic  s.m.  EMTP  source component was number',  i4, '   in order of input. ')
     go to 6220

  case (29)
6179 write (unit = lunit(6), fmt = 7179)
7179 format (5x, 'The data case under consideration has too many output quantities for the present EMTP table sizes.   This is',/, &
          5x, 'really an overflow of EMTP list number  11 ,   though not due to conventional EMTP electric-network branch or',/, &
          5x, 'node quantities.   Rather, it is the added burden of dynamic synchronous machine (s.m.) output variables, plus',/, &
          5x, 'the burden of tacs output variables, which has now led to the present list-11 overflow.   Figures which ')
     write (unit = lunit(6), fmt = 7279) lsiz12, nc, lstat(15), lstat(16), lstat(14)
7279 format (5x, 'characterize this difficulty are as follows ....',/, &
          5x,  i5, ' = present user-dimensioned size of list 11',/, &
          5x,  i5, ' = number of conventional electric-network branch-output quantities',/, &
          5x,  i5, ' = number of s.m. output quantities',/, &
          5x,  i5, ' = number of tacs output quantities',/, &
          5x,  'the last three figures above total',  i6,  ' ,   which is therefore the minimum acceptable size of list number 11. ')
     write (unit = lunit(6), fmt = 7379)
7379 format (5x, 'either increase the size of list 11 to at least this size, or appropriately reduce the number of output',/, &
          5x,  'quantities, before trying once again to solve the data case in question. ')
     kill = 1
     lstat(16) = 11
     nchain = 51
     if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
4568 format (' "exit  module over54."')
     go to 99999

  case (30)
6180 write (unit = lunit(6), fmt = 7180) lstat(16), lstat(17)
7180 format(5x, "The size of array 'voltbc', defined in blkcom to be", i6, ',', 3x, 'is not adequate for the use of that array in the', /, &
          5x, 'processing of output requests for the type 59 s.m.  a minimum of', i5, 3x, 'cells is required. ')
     go  to  6220

  case (31)
6181 write (unit = lunit(6), fmt = 7181) lstat(15), lstat(16), lstat(16)
7181 format(5x, 'inconsistency discoverd on one of the mass cards .   The specified mass no. was', i5, '.', 3x, 'for a s.m. with', i5, '  masses', /, &
          5x, 'allowable range is between     1   and', i5, '  masses. ')
     go  to  6220

  case (32)
6182 write (unit = lunit(6), fmt = 7154)
     write (unit = lunit(6), fmt = 7182)  lstat(14), lstat(15)
7182 format (5x, 'The number of frequency cards which are present in the previously-read line-constants data is incorrect.',/, &
          5x, 'before the one which requests an automatic looping over logarithmically-spaced frequencies, there were', i4, /, &
          5x, 'frequency cards, each of which requests the line-constants calculation at an individual, discrete frequency.',/, &
          5x, "But the transposition flag (miscellaneous data parameter 'nss'  )  had value",  i4,   ' .   these two integers ')
     write (unit = lunit(6), fmt = 7282)
7282 format (5x, 'should add up to two.   But they do not.   Remember, for an untransposed line, there are to be two discrete-',/, &
          5x,  'frequency cards, while a transposed line requires exactly one. ')
     go to 6220

  case (33)
6183 j = lstat(13)
     write (unit = lunit(6), fmt = 7154)
     write (unit = lunit(6), fmt = 7183)  volti(j), j, ci1
7183 format (5x, 'The frequency cards which are present in the previously-read line-constants data are not all consistent.',/, &
          5x, 'The EMTP requires that all such cards apply to the same model of the transmission line.   But it will be noted',/, &
          5x, 'that an earth resistivity of',    e17.5,  '   Ohm-meters was read from frequency card number', i4,   ' ,   while',/, &
          5x,     e17.5,      '   was read from the first such card.   These two resistivities are unequal, which is illegal. ')
     go to 6220

  case (34)
6184 write (unit = lunit(6), fmt = 7154)
     write (unit = lunit(6), fmt = 7184)   lstat(13)
7184 format (5x, "The previously-read line-constants data cards are illegally-structured for usage with  'Semlyen setup' .",/, &
          5x, 'first, there should be the line-conductor cards, terminated by a blank card.   Then come the frequency cards,',/, &
          5x, 'with only the last of these requesting the logarithmic frequency-looping option.   Finally, a blank card',/, &
          5x,  'terminates the frequency cards, and a second blank card finishes the line constants data cards.', 10x, 'n1 =', i2 )
     go to 6220

  case (35)
6185 write (unit = lunit(6), fmt = 7154)
     write (unit = lunit(6), fmt = 7185)   lstat(13), lstat(14)
7185 format (5x, 'The EMTP has previously read a value of',  i6, '   (decimal) for the miscellaneous data parameter',/, &
          5x,  "'iotx'  which controls printout.   But this exceeds the legal meaningful limit of",  i6,  '  (decimal). ')
     go to 6220

  case (36)
6186 write (unit = lunit(6), fmt = 7154)
     write (unit = lunit(6), fmt = 7186)  voltbc(1), voltk(icheck)
7186 format (5x, "for purposes of  'Semlyen setup'  usage, the frequencies for which line constants have just been calculated",/, &
          5x, 'are inappropriate.   Recall that within the line-constants data, the frequency card for logarithmic looping over',/, &
          5x, 'a range of frequencies is to be preceeded by a frequency card for the steady-state frequency.   The semlyen code',/, &
          5x, 'requires that the beginning loop-frequency exceed the steady-state frequency.   But such is not the case for the',/, &
          5x, "user's data (values of",   e16.4,    '   and',    e16.4, '   were punched for these, respectively). ')
     go to 6220

  case (37)
6187 write (unit = lunit(6), fmt = 7187)  bus3, bus4, bus5
7187 format (5x, "The last-read data card belongs to a  'Semlyen setup'  data case, but is not what the EMTP was expecting.",/, &
          5x, 'Following the first semlyen miscellaneous data card, the user must supply a card which bears either the text',/, &
          5x, " 'line constants'   or the text   'cable constants' ,   punched in columns one onward.   not so for the user's ",/, &
          5x, 'card, the column 1-18 contents of which were read as   ', "'", 3a6 , "'", ' .    Correct this card so as to ')
     write (unit = lunit(6), fmt = 7287)
7287 format (5x, 'properly inform the EMTP as to which supporting routine program control is to be transferred to next. ')
     go to 6220

  case (38)
6188 d1 = flstat(13) / twopi
     d2 = flstat(15) / twopi
     write (unit = lunit(6), fmt = 7154)
     write (unit = lunit(6), fmt = 7188)  d1, flstat(14)
7188 format (5x, "After having completed the  'line constants'  or 'cable constants'  calculation, program control is now",/, &
          5x, "back in the  'Semlyen setup'  overlay where matrices  (r),  (x),  (g),  and  (b)  are being read from input/output",/, &
          5x, 'unit number  3 .   But the capacitance matrix is erroneous.   It should be independent of frequency, but is not.',/, &
          5x, 'in particular, the first diagonal entry  c(1,1) =', e20.8 , '   Farad/length for frequency',  e18.6     )
     write (unit = lunit(6), fmt = 7288)  d2, epsiln
7288 format (5x,  'Hz,  while the  c(1,1)  value for the preceding frequency was',     e20.8 ,    ' .   it is required that',/, &
          5x, 'these capacitances agree within a tolerance of',   e14.2, '   (floating-point miscellaneous data parameter',/, &
          5x, " 'epsiln' ).   For the user's data, this check has not been met.   The capacitance matrices are judged to be",/, &
          5x, 'invalid, and the solution is being terminated. ')
     go to 6220

  case (39)
6189 write (unit = lunit(6), fmt = 7189)  lstat(15)
7189 format (5x, 'The EMTP data case now being processed makes use of the tacs modeling capability, all data cards for which',/, &
          5x, 'have now been read.   as the preceding printout shows, the vector of tacs variable names has repeated entries.',/, &
          5x, 'There are',  i4,   '   such conflicts.   The trouble can be traced back to the definition of tacs function blocks,',/, &
          5x, 'tacs summers, and tacs supplemental variables and devices.   All names assigned to such outputs must be distinct. '     )
     write (unit = lunit(6), fmt = 7289)
7289 format (5x, "For example, if a tacs function block is given the name  'kontrl'  (read from columns 3-8 of the data card",/, &
          5x, 'defining the block), then this same 6-character name had better not be used for a tacs supplemental variable',/, &
          5x, '(read from columns 3-8 of the variable-definition card). ' )
     call over54err7421
     go to 6220

  case (40)
6190 write (unit = lunit(6), fmt = 7190)  lstat(14), bus6
7190 format (5x, 'The EMTP has been reading data cards which define a dynamic synchronous machine (s.m.) component.   The last-',/, &
          5x, "read card follows the s.m. output-request card, and precedes the  'finish'  card|   having columns 1-2 punched",/, &
          5x, 'with  ', "'", i2, "'",  ' ,   this card represents a request that the machine be controlled by tacs.    But either the a-6',/, &
          5x, 'name  ', "'", a6,  "'", '  which was read from columns 3-8 is not a legal tacs-variable name for this purpose, since it ')
     write (unit = lunit(6), fmt = 7290)
7290 format (5x, "was not also punched on a   'tacs outputs'   card which began the input of tacs data.   Before tacs variables",/, &
          5x, "can be used within the electric network, they must be declared on a   'tacs outputs'   or a   'tacs EMTP sources'",/, &
          5x,   'card. ')
     write (unit = lunit(6), fmt = 7390) lstat( 15 )
7390 format (5x, 'or... The number punched in columns 15-17 has an incorrect value of,', i5 )
     go to 6220

  case (41)
6191 write (unit = lunit(6), fmt = 7191)
7191 format (5x, "The EMTP finds the user's data case to be obnoxiously degenerate, and refuses to continue with the simulation.",/, &
          5x, 'The electric network has no sources and no dynamic synchronous machines, so only a natural (unforced) solution',/, &
          5x, 'is called for.   But the user has failed to input any non zero initial conditions.   Hence the solution will be',/, &
          5x, 'identically zero for all time.   There is no need to continue with the solution, then. ')
     go to 6220

  case (42)
6192 write (unit = lunit(6), fmt = 7192)
7192 format (5x, 'During the list-building operation which is required for connectivity output, the temporary working vectors',/, &
          5x, 'of list 99 which are used have overflowed.   The proper solution is to redimension the EMTP, though removal of',/, &
          5x, "the '1'-punch in field  'idoubl'  of the integer miscellaneous data card will bypass the present complication.",/, &
          5x, 'but unless the user increases dimensions, he will probably just be stopped shortly hereafter, in renumbering. '        )
     write (unit = lunit(6), fmt = 7292)
7292 format (5x, 'both computations use the same arrays, and have related storage requirements.   Lists number 5 % 8 contribute',/, &
          5x,  'totally to dependent list number 99. ')
     go to 6220

  case (43)
6193 write (unit = lunit(6), fmt = 7193)  fminfs
7193 format (5x, "The last-read data card bears the key word   'frequency scan'   in columns 1-14.   This is a request for the",/, &
          5x, 'automatic looping over steady-state phasor solutions only, as the source frequency is methodically increased.',/, &
          5x, 'a beginning frequency of',    e15.4,      "   was read from the  'fmin'  field of columns 25-32 using  e8.0",/, &
          5x, "format, and must be positive (to be legal data).   The frequency increment  'delf'  of columns 33-40 was read using ")
     write (unit = lunit(6), fmt = 7293)  delffs, fmaxfs, lstat(14)
7293 format (5x,  'e8.0  format as',    e15.4,     " .    This must be positive, unless data field  'log'  of columns 49-56",/, &
          5x, "(see below) is punched positive.   The maximum (or end) frequency  'fmax'  of columns 41-48 was read as",  e15.4,/, &
          5x, " ,   which must exceed  'fmin' .   Finally, integer field 'log'  of columns 49-56 was punched with the value",/, &
          5x, i5, " ,   which must not be negative.   The user's data card is illegal in that it violates one or more of these ",/, &
          5x,  'just-delineated rules. '  )
     go to 6220

  case (44)
6194 write (unit = lunit(6), fmt = 7194)  kconst, lstat(13), lstat(14)
7194 format (5x, "The data case now being read in includes a request for the  'frequency scan'  feature.   but the source",/, &
          5x, 'data is inconsistent with this intended usage.   To be legal, there must be at least one type-14 EMTP source',/, &
          5x, 'component which is present during the steady-state phasor network solutions (as requested by punching data field',/, &
          5x, " 'tstart'  of columns 61-70 negative).   But the user has", i5,  '   EMTP source components, of which',  i5,  '   are',/, &
          5x, 'of type 14, of which (in turn)',  i5, "   have  'tstart'  negative. ")
     go to 6220

  case (45)
6195 write (unit = lunit(6), fmt = 7195)  lstat(14), lstat(15)
7195 format (5x, 'The EMTP is in the process of inputting branch cards for a transmission circuit which is modeled using',/, &
          5x, 'Ametani linear convolution.   The last-read data card contains parameters for mode number',  i5,   ' .    but',/, &
          5x, 'the integer which was read from columns  73-74  using  i2 format is   ',  "'", i2,  "'", ' .    This is the number of',/, &
          5x, 'linear segments which represent the response ---- which is presently constrained to equal five.    Re-run',/, &
          5x,  " 'Ametani setup'   to get a valid new set of branch cards for the circuit, and then try again. ")
     go to 6220

  case (46)
6196 write (unit = lunit(6), fmt = 7196)
7196 format (5x, "EMTP control is now in the   'Ametani setup'   supporting routine, ready to transfer to either   'cable",/, &
          5x, "constants'   or   'line constants' .    But the last-read data card does not bear one of these key words on it,",/, &
          5x, 'beginning in column number  1 . ')
     go to 6220

  case (47)
6197 write (unit = lunit(6), fmt = 7197)   deltat, flstat(14)
7197 format (5x, 'The EMTP is now inputting data for a transmission circuit that is being modeled using',/, &
          5x, "frequency dependent representation.  But for this data, the present time-step size  'deltat'  of", e15.4, ' sec    is',/, &
          5x,  'too large.   The  travel time of the current mode is equal to', e15.4,  " sec ,    which must exceed  'deltat' .   The",/, &
          5x,  'time-step size of the study must be decreased to satisfy this constraint. ')
     go to 6220

  case (48)
6198 write (unit = lunit(6), fmt = 7198)  lstat(16), lstat(15)
7198 format (5x, "The EMTP is in the middle of solving a    'Semlyen setup'    data case, at which point it has been discovered",/, &
          5x, "that insufficient working space exists.    The   'line constants'   or   'cable constants'   calculation has now",/, &
          5x,  'been successfully completed, and a minimum working space of',  i7, '    floating-point cells is now a known',/, &
          5x, "requirement for the completion of   'Semlyen setup'   processing.   But module  'vdov45'  only contains",  i7)
     write (unit = lunit(6), fmt = 7298)
7298 format (5x, 'floating-point cells of such working space.   The EMTP must be re-dimensioned by the user, so as to increase',/, &
          5x, 'the size of   /label/   by the just-indicated shortfall.   Distribution of total storage among the different EMTP',/, &
          5x, 'lists is immaterial in this case (as with the dimensioning of all primary-level non-solution overlays). ')
     go to 6220

  case (49)
6199 write (unit = lunit(6), fmt = 7199)  flstat(15), lstat(14), flstat(13), lstat(13)
7199 format (5x, "the present   'Semlyen setup'   data case has broken down in the middle of a matrix inversion operation for",/, &
          5x, 'frequency',    e14.5,    '    Hertz.   The eigenvector (modal transformation) matrix of order',  i5,   '    has',/, &
          5x, 'the value',     e13.3,    '    for the largest possible pivot element of row number',   i5,   ' .    But this does'    )
     write (unit = lunit(6), fmt = 7299)  flstat(14)
7299 format (5x,  "not exceed the near-zero tolerance  'epspv2' , which has value",    e13.3,     ' .    Execution must',/, &
          5x, "be stopped immediately.   Yet, the user is reminded that he can redefine  'epspv2'  by means of an optional",/, &
          5x,  " 'tolerances'   card (read by   'semlyen setup'  ) . ")
     go to 6220

  case (50)
6200 write (unit = lunit(6), fmt = 7200)
7200 format (5x, 'Memory-overflow problem, before entry into the renumbering overlay (for transient network renumbering).',/, &
          5x, 'recall that space for renumbering comes from a major portion of   /label/ .    Three vectors are used, with size',/, &
          5x, 'given by list number  99  of the case-summary statistics.   This space is insufficient even for the simple storage',/, &
          5x,  'of the connectivity of  (y) --- to say nothing of the simulation of fillin upon triangularization. ')
     if ( lstat(14)  .gt.  0 ) write (unit = lunit(6), fmt = 7300)  ibr, lstat(14)
7300 format (5x,  'of the',  i6, '   entries of the branch table, only',  i6, '   were inserted into the working storage before overflow. ')
     if ( lstat(14)  .eq.  0 ) write (unit = lunit(6), fmt = 7400)
7400 format (5x, 'all branch-table entries were successfully inserted into the working storage, but entries from the nonlinear',/, &
          5x, 'element table and the switch table then produced overflow. ')
     write (unit = lunit(6), fmt = 7500)
7500 format (5x, 'as for redimensioning of the EMTP, lists  5  and contribute  100  per cent to the size of dependent list',/, &
          5x, '99.   Increase one of these list sizes substantially, and try again. ')
  end select
6220 lastov = nchain
  nchain = nfrfld + 50
  if (iprsup .ge. 1) write (unit = lunit(6), fmt = 4568)
99999 return
end subroutine over54

subroutine over54err7421
  use blkcom
  implicit none
  !
7421 write (unit = lunit(6), fmt = 7521)
7521 format (/, 5x, 'Since the user is having trouble with 6-character tacs variable names, it is perhaps worth qualifying the',/, &
       5x, 'preceding error text which complains about an unidentifiable name that is associated with a certain tacs component',/, &
       5x, 'or data class.   All that is really involved here is a spelling comparison with other usages of the same variable',/, &
       5x, "name.   When for some particular tacs component a given name  'name1 '  can not be found in a table where it ")
  write (unit = lunit(6), fmt = 7621)
7621 format (5x, 'belongs, the EMTP says that the former is unrecognizable.   Yet, as used with the component explicitely',/, &
       2 5x, "mentioned in the message,   'name1 '   may in fact be spelled exactly as the user intended.   It may be the table",/, &
       4 5x, 'being searched which is in error, due to faulty spelling of the variable name on some other data card (which',/, &
       5x, 'was the usage that generated the table entry).   Hence the user should look at other data cards for mis-spelling of ')
  write (unit = lunit(6), fmt = 7721)
7721 format (5x, 'the name in question, if the spelling as printed in the above error text is actually as the user wanted it. ')
  write (unit = lunit(6), fmt = 7821)
7821 format (/, 5x, 'then too, while talking about spelling, it might be a good idea to emphasize what is involved.   all six-',/, &
       5x, 'character variable names are  a6  fortran alphanumeric information.   When printed out within error messages, the',/, &
       5x, 'six characters in question are delineated by a leading and a trailing quotation or apostrophe mark.   Position',/, &
       5x, "of imbedded blanks is indeed crucial, then.   For example,   'raver '  and  ' raver'  are completely different, ")
  write (unit = lunit(6), fmt = 7921)
7921 format (5x, 'distinct 6-character names, as far as the EMTP is concerned.   EMTP names (including those of tacs)',/, &
       5x, "consist of an ordered string of 6 characters, with 'blank ' being a character like any other one.   Two variable",/, &
       5x, 'names are equal if and only if both characters of any character position are equal for the two names, for all',/, &
       5x, 'possible character positions  1, 2, .... 6 . ')
  return
end subroutine over54err7421

!
! end of file over54.f90
!
