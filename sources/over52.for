!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over52.for
!
!
!     subroutine over52.
!
subroutine over52
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  if ( iprsup  .ge.  1 ) write ( lunit6, 4567 )
4567 format ('  "begin module over52." ')
  if ( nchain  .ne.  52 )   go to 99999
  n1 = kill - 50
  go to (6051 , 6052, 6053, 6054, 6055, 6056, 6057, 6058, 6059, 6060, 6061 , 6062, 6063, 6064, 6065, 6066, 6067, 6068, 6069, 6070, &
       6071 , 6072, 6073, 6074, 6075, 6076, 6077, 6078, 6079, 6080, 6081 , 6082, 6083, 6084, 6085, 6086, 6087, 6088, 6089, 6090  ), n1
6051 write(lunit6, 7051)  lstat(14), bus1, bus2
7051 format (5x, "The user's network includes a group of mutually-coupled branches of order ", i2, ' which are specified by   ',/, &
          5x, 'matrices (a), (b).   The first branch of this problem group connects bus ', "'", a6, "'", ' with bus ', "'", a6, "'.   When  ",/, &
          5x, 'using the (a), (b) option, the number of upper-triangle matrix elements ( n(n+1)/2 ) must not exceed the bus  ',/, &
          5x, 'dimensioning (list 1) of the program, as it has here.   If the user really wants so many conductors, he should   ',/, &
          5x,  'switch to use of (r), (l) format, if possible.   ')
  go to 6220
6052 write(lunit6, 7052)  lstat(16)
7052 format (5x, 'The user has been inputting a saturable-transformer component, the data for which is not all legal.   Of the  ',/, &
          5x,  'rules to follow, that numbered ', i1,  ' has been violated.   Rules include....   ',/, &
          10x,  '1)  zero-sequence reluctance of air-return path of 3-phase unit must be positive.    ',/, &
          10x, '2)  leakage inductance  l  for all but the first winding must be nonzero.   First winding must have nonzero  ',/, &
          14x, 'leakage impedance.   ')
  write(lunit6, 7152)
7152 format (10x, '3)  rated winding voltages (number of winding turns) must all be positive.      ',/, &
          10x, '4)  the rated winding voltage (number of winding turns) must be the same for corresponding windings of all 3 phases  ',/, &
          14x,  'of a 3-phase transformer.   ')
  goto6220
6053 write(lunit6,7453)
7453 format (5x, 'The user is attempting to use the steady-state cascading of pi-circuits option to solve a very large problem.   ')
  write(lunit6,7053)lstat(12)
7053 format (5x, 'Error in cascaded-pi data.  ',/, &
          5x, "There are too many phases requested for the program to handle.   The number of phases 'nphcas' as read from  ",/, &
          5x, 'columns 27-32 of header card is ', i6,  '.   This is too large for present program dimensions.   The cascading  ',/, &
          5x, 'code has temporary working arrays which are equivalenced to various components of regular (permanent) program  ',/, &
          5x, "tables, and it is one of the list limits for these whichis insufficient for the user's problem.   In particular,   ")
  if( lstat(15) .ne. 1 )  go to 7253
  write(lunit6, 7153)  lstat(16), lpast
7153 format (5x, 'n*(n+1)/2 = ', i5,  "  exceeds 'lpast' of ", i5,  "   , where  n = 3*nphcas ,  and 'lpast' is   ",/, &
          5x,  'the size of list number 8 (see below).   ')
7253 if( lstat(15) .ne. 2 )  go to 6220
  write(lunit6, 7353)  lstat(16), lbus
7353 format (5x,  'nphcas*(nphcas+1)/2 = ', i5,   "  exceeds 'lbus' of", i5,  "where 'lbus' is the size of list number 1  ",/, &
          5x,  '(see below).   ')
  goto6220
6054 write(lunit6, 7154)
7154 format (5x, 'The user has attempted to use the steady-state cascading of pi-circuits option, but has made an input data error.  ')
  write(lunit6,7054)lstat(13),lstat(12)
7054 format (5x, 'The line-position card provides for specification of a transposition by means of a phase-position map   ',/, &
          5x, "beginning in column 25, and using 'i4' format for each of 'nphcas' entries.   These entries must be a   ",/, &
          5x, "permutation of the integers 1, 2, 3, ..., 'nphcas', with 'nphcas' =", i3,  ' for the present line.   But one of  ',/, &
          5x,  'the position-map entries has been read as ', i4, ', which is clearly impossible.   Assuming that the   ')
  write(lunit6, 7254)
7254 format (5x, 'digits which were punched on the card were ok, check for right-justification within the fields of width 4.   ',/, &
          5x, 'Recall that an erroneous shift to the left by one column multiplies the number by 10, while a shift to the right  ',/, &
          5x, 'will spill over into the following field, generally creating a very large integer when the latter field is read.    ')
  goto6220
6055 write(lunit6, 7154)
  write(lunit6,7055)lstat(12)
7055 format (5x, "'multip' is the multiplicity parameter, read from columns 9-12 of the line-position card.   As such, it must  ",/, &
          5x, 'be a non-negative integer, with zero or blank given the default value of unity.   But the EMTP has read a value   ',/, &
          5x, 'of ', i4,  " for 'multip', which has no physical meaning, andmust be rejected.  ")
  goto6220
6056 write(lunit6, 7154)
  write(lunit6,7056) lstat(13), lstat(14), lstat(12)
7056 format(5x,  'The most-recently-inputted group of r-l-c shunt branches were ', i2,  ' in number, with both ends of number  ',/, &
          5x, i2,  ' of this group connected to the same node number ', i2,  '.   Now while such a requested connection    ',/, &
          5x, "presents no problem if taken literally, The EMTP's infallible intuition is inclined to feel that the user has made  ",/, &
          5x, 'a punching error in columns 3-14 of his data card (where the two numbers in question are read in).   A network  ',/, &
          5x,  "solution with such degenerate topology will not be permitted.   Sorry 'bout that.    ")
  goto6220
6057 write(lunit6, 7154)
  write(lunit6,7057)lstat(12),lstat(13)
7057 format (5x, 'Error in cascaded-pi data.  ',/,5x, 'node ', i4, ' short circuited to node ', i4, '.  One of these nodes should be removed from the circuit.  ')
  goto6220
6058 write(lunit6, 7154)
  write(lunit6,7058)
7058 format (5x, "'stop cascade' card missing in referenced cascaded pi section.  This can be corrected by either putting",/, &
          5x, "this card in or removing the 'cascaded pi' card.")
  go to 6220
6059 write (lunit6, 7059)  lstat(16), bus1
7059 format (5x,  '3-phase compensation for synchronous machine number ', i2,  ' breaks down.   Terminal bus ', "'", a6, "' ", /, &
          5x, 'already has another element requiring compensation connected to it.   ')
  go to 6220
6060 write(lunit6, 7154)
  write(lunit6,7060)lstat(12)
7060 format (5x, 'Invalid shunt r-l-c node number  ', i3, 'was used.  This number is out of sequence.  ', /, &
          5x, 'Check connections in this area and renumber nodes.   ')
  go to 6220
6061 write(lunit6, 7061)  flstat(15), flstat(16)
7061 format (5x, 'Switched-inductance element just read has inductance parameters  l1  and  l2  (columns 25-44) punched  ',/, &
          5x, 'as', e14.3, ' and', e14.3,  ', respectively.   One or both of these are non-positive, which is illegal.   If the  ',/, &
          5x,  'user wants a zero value, he must punch a small positive number.   ')
  go to 6220
6062 write(lunit6, 7062)  flstat(14), flstat(15)
7062 format (5x, 'Switched-resistance element just read has resistance  r  (columns 15-24) or breakpoint voltage  ',/, &
          5x, '(columns 35-44) nonpositive, which is illegal.   If the user wants a zero value, he must punch a small positve number.  ',/, &
          5x,  'Numbers read from the two fields in question are', e13.3, ' and ', e13.3,  ', respectively.  ')
  go to 6220
6063 write(lunit6, 7063)
7063 format (5x, "Trouble has been encountered during decoding of user's input data.   Had not this very civilized error  ",/, &
          5x, 'message been specially provided, the computer operating system would have performed the termination in its own ',/, &
          5x, 'cursory, inimitably-inscrutable manner.   Most probably, the last data card read carries some erroneous data,   ',/, &
          5x,  'has per the following suggestions ....   ',/, &
          8x, '1) it is possible that one or more columns carry illegal, completely-uninterpretable punches, in which case the   ')
  write(lunit6, 7163)
7163 format (11x, 'Last 80-column card image printed out above might not be an accurate image of the data card in question.   ',/, &
          11x, 'The user should be aware of this limitation on the fidelity of the printed card images, which can only   ',/, &
          11x, 'be guaranteed if all characters are legally-punched.   Yet this case of illegal punches is rare.   Continue reading.  ',/, &
          8x, '2) more commonly, the user may have punched data a little to the left or to the right of where he had   ',/, &
          11x, 'intended, thereby placing a decimal point in an integer field, or a non-numeric character in a numerical    ')
  write(lunit6, 7263)
7263 format (11x, 'field.  ',/, 8x, '3) one or more random punching errors in a numeric field will generally produce trouble.   Letters or   ',/, &
          11x, 'punctuation are not allowed, of course (with a few very special exceptions, which are highly improbable).    ',/, &
          8x, "4) punching of floating-point numbers with an 'e' (power of 10) can lead to trouble if not right-justified in  ",/, &
          11x, 'the field, since any blank spaces to the right of the last-punched exponent digit are interpreted as zeroes,    ')
  write(lunit6, 7363)
7363 format (11x, 'thereby increasing the exponent by a power of ten.   This may produce a result which is outside the   ',/, &
          11x, 'legal range of floating-point numbers.    ',/, &
          8x, '5) out-of-sequence data is a notorious offender.   For example, if a legitimate branch card is read as a switch  ',/, &
          11x, 'card (maybe it was mixed in with the switch cards by mistake), one could have 2 decimal points punched in a  ',/, &
          11x, "data field which is read under 'e'-format control, which is clearly illegal.   The user should check that   ")
  write(lunit6, 7463)
7463 format (11x, 'blank cards, and any other data-termination or delineation cards, are properly positioned in the data   ',/, &
          11x, 'stream.   Only with such care will the EMTP think that it is reading what the user thinks he is inputting,    ',/, &
          11x, 'at said point in time.    ')
  go to 6220
6064 write (lunit6, 7064)  bus1
7064 format (5x, 'Switch connectivity error.  Subroutine switch has determined that a loop of closed switches is about to be   ',/, &
          5x, 'created involving bus  "',  a6, '".   Such switch loops are illegal.  ')
  go to 6220
6065 write (lunit6, 7065)  lstat(14), flstat(15), flstat(16)
7065 format (5x,  'The last-read data card has been taken by the EMTP to be card number ', i3,  " of the user's frequency-   ",/, &
          5x, "dependent line-constants input.   It is illegal because the frequency, as read from columns 21-30 using 'e' format,   ",/, &
          5x,  'is not greater than that read from the preceding card. The frequency of this card is', e13.4,  ', while that   ',/, &
          5x,  'of the preceding card was', e13.4,  '.   Data cards for line constants must be inputted in order of increasing  ',/, &
          5x,  'frequency ---- strictly monotone-increasing.  ')
  go to 6220
6066 write(lunit6, 7066)
7066 format (5x, "The user is presently trying to input line constants ('r', 'l', and 'f' fields) for too many frequencies.   ")
  write(lunit6, 7166)  lstat(12)
7166 format (5x,  'The current program dimensioned storage for such points is ', i3,  '.   Does the user really need all of   ',/, &
          5x, 'these points that he is using, the EMTP wonders.   If he really does, and can justify his request to program   ',/, &
          5x, 'maintenance, an expansion of these tables which have just overflowed could easily be made.   Otherwise, maybe a   ',/, &
          5x, "re-reading of the user's manual, or a talk with someone familiar with the weighting-function calculation, or   ",/, &
          5x,  'both, might be in order.    ')
  go to 6220
6067 write(lunit6, 7067)  lstat(13)
7067 format (5x, 'To find the near-end weighting function  a2(t) ,     recourse is made to the infinitely-long line.   For time  ',/, &
          5x, "less than the infinite-frequency travel time 'tau', it is the infinite-line response which is actually used.    ",/, &
          5x, "Input parameter  'npinf'  (columns 25-32 of the second misc. data card) specifies the number of points which are   ",/, &
          5x,  'used to define this response, with the user requesting', i4,  '.'   )
  write(lunit6, 7166)  lstat(12)
  go to 6220
6068 write(lunit6, 7068)  flstat(14), lstat(15), lstat(13)
7068 format (5x, 'the number of points which are used to define the weighting functions is controlled (in part) by two misc.   ',/, &
          5x,  "Data parameters.   The first of these is  'rmax'  with value", e12.3,  ", and the second is  'mnum'  with value   ",/, &
          5x, i3, '.   Either these were user-punched numbers, or they are default values built into the EMTP   anyway, these   ',/, &
          5x, 'parameters combine to produce a storage requirement for the finite line given by    (rmax-1)*mnum = ', i4,  ' points.   ')
  write(lunit6,7168) lstat(12)
7168 format (5x, 'The present dimensions of the program permit ', i4, ' points. ')
  if ( flstat(13) .gt. 0. ) write(lunit6,7268)
7268 format (5x, 'Since variable time increment dt was requested by mnum less than zero, the total storage requirement ',/, &
          5x, 'was established by summation of similar expressions with adjusted values of rmax and mnum for each time span. ')
  go to 6220
6069 write(lunit6,7069)
7069 format (5x, 'The user is presently trying to input i-v points which define his magnetic saturation characteristic.    ',/, &
          5x, 'But he is using too many points to define this curve.   ')
  write(lunit6, 7166)  lstat(12)
  go to 6220
6070 write(lunit6,7070)
7070 format (5x, 'This error is most likely due to the computer system library subroutine for the sine function giving a value   ',/, &
          5x, 'greater than unity.   If there were any justice in the world, this trouble would not have arisen.   In any case,   ',/, &
          5x, "the difficulty is probably not the user's fault, so he should go complain to program maintenance about his trouble.    ")
  go to 6220
6071 write(lunit6, 7071)  lstat(12), flstat(13), flstat(14)
7071 format (5x, 'The user is presently trying to input i-v points which define his magnetic saturation characteristic.    ',/, &
          5x, 'But he has violated the rule which requires that both voltage and current be strictly monotone-increasing (for    ',/, &
          5x, 'the order of data-card input).   The last-read card (number ', i3,  ' of the characteristic) violates this   ',/, &
          5x, 'requirement, as shown by the following variable values which were punched by the user .....    ',/, &
          20x, 'Latest current = ', e13.4, 10x, 'preceding current = ', e13.4   )
  write(lunit6, 7171)  flstat(15), flstat(16)
7171 format (20x, 'Latest voltage = ', e13.4, 10x, 'preceding voltage = ', e13.4, '.'  )
  if( lstat(12) .gt. 1 )  go to 6220
  write(lunit6, 7271)
7271 format (5x, "Of course since trouble was spotted upon an examination of the user's very first card of the characteristic,   ",/, &
          5x, "the 'preceding card' refers to the implied point (0, 0) which represents the origin.   In this case, the user's   ",/, &
          5x, 'trouble stems from his failure to punch a positive pair of values (i, v) on his first card of the characteristic.   ')
  go to 6220
6072 write(lunit6, 7072)  flstat(16)
7072 format (5x, 'The user has been inputting a type-97 (staircase) time-varying resistance element, and has violated one of  ',/, &
          5x, 'the data rules pertaining there to.   In particular, the last-read data card has a nonpositive value of  ', e13.4, /, &
          5x, 'punched in columns 9-16.   Such a nonpositive resistance for a portion of the time-vs.-resistance characteristic   ',/, &
          5x,  'is interpreted as a data-punching error by the EMTP   ')
  go to 6220
6073 write(lunit6, 7073)  lstat(16), bus1
7073 format (5x, 'The last-read data card has been taken by the EMTP to be a source card, specifying a generator of type ', i2, /, &
          5x,  'located on bus ', "'", a6, "'.   Now for such a source, columns 11-20 are to be punched with the amplitude (for   ",/, &
          5x, 'a type-14 sinusoidal source, this is the peak value).   But the user has punched a zero, or left the field blank.   ',/, &
          5x, "While such a case could easily be solved, the EMTP's superbly-honed perspicacity is offended by such an idea.   ",/, &
          5x, "After all, if a voltage source is involved, zero amplitude means permanent grounding, in which case  '      '    ")
  write(lunit6, 7173)
7173 format (5x, '(6 blank characters as bus name) or a switch to ground should be used, for clarity.   If a current source   ',/, &
          5x, "is involved, zero amplitude means that the source doesn't exist at all.   Either way, the user should correct this  ", /, &
          5x,  "'aesthetic difficulty', and try again.   Who knows, next time the job may run further.   ")
  go to 6220
6074 write(lunit6, 7074)  flstat(15), lstat(16)
7074 format (5x, "While solution of this problem could continue, the EMTP has become suspicious of a data 'misunderstanding' on   ",/, &
          5x, "the part of the user.   Note that the termination time of the study ( 'tmax' ) is nonpositive, indicating that   ",/, &
          5x, "only a sinusoidal steady-state solution is desired.   The value of  'tmax'  actually read from columns 9-16 of   ",/, &
          5x,  'the first miscellaneous data card is', e13.4, '.   Yet of the ', i3,  ' sources which the user has  ',/, &
          5x, 'inputted, none is a sinusoidal source which is to be present in the steady-state (for time less than zero).     ')
  write(lunit6, 7174)
7174 format (5x, "Recall that  'tstart'  of columns 61-70 of the source card must be punched with a negative number, if the  ",/, &
          5x, 'source is to be present in the steady-state solution.   Thus there are no sources for the phasor solution which  ',/, &
          5x, 'was requested, and the solution must necessarily be identically zero.   Such a degenerate, trivial problem will   ',/, &
          5x,  'not be allowed.   ')
  go to 6220
6075 write(lunit6, 7075)
7075 format (5x,  'The user has been inputting frequency-dependent line constants to the weighting-function supporting    ',/, &
          5x, 'program, in order to calculate  a1(t)  and  a2(t)  for the zero-sequence mode of a distributed-parameter line.   ')
  write(lunit6, 7175)  flstat(15)
7175 format (5x, 'But the last-read data card has gotten him in trouble.   Columns 1-10 (the resistance field, r) are blank   ',/, &
          5x,  'or zero, while columns 11-20 (the inductance field, l) contain the nonzero floating-point number',  e14.4,  '   .'    ,/, &
          5x, 'Now if this card was meant to be the blank card terminating the line-constants input, the user should remove  ',/, &
          5x, 'the punches from columns 11-20, and resubmit the job.   If on the other hand this card was intended to be a   ',/, &
          5x, 'line-constants card, the user had better re-think the matter.   Resistance is the parameter which provides the   ')
  write(lunit6, 7275)
7275 format (5x, 'damping in the zero-sequence network which increases dramatically with frequency, and is a crucial ingredient  ',/, &
          5x, 'to this sophisticated distributed-line representation.         The EMTP will not allow the user to calculate weighting  ',/, &
          5x, 'functions without resistance present.   It is not even clear if the inverse fourier transformation logic is valid  ',/, &
          5x, 'for such a case, since would not the impulse response of such a distortionless line produce an impulse for  a1.    ',/, &
          5x, 'In this case, re-run a line-constants program to calculate legitimate line constants, and then try this job again.  ')
  go to 6220
6076 write(lunit6, 7076)  lstat(16)
7076 format (5x, 'The EMTP logic has interpreted the last-read data card to be part of the specification cards for plotted output. ',/, &
          5x, 'But the type-code read from columns 1-2 cannot be accepted.   A value of , i2, 25h was read, which does not   ',/, &
          5x, 'correspond to any of the legitimate existing codes (0, 1, 2       ).   If the card in question really is not a plot card,    ',/, &
          5x, 'the user must discover how his data does not conform to the assumed input sequencing, and rectify the error.     ',/, &
          5x, 'the correct number of, and placement of, blank delimiter cards is a notorious cause of data-synchronization trouble.   ')
  go to 6220
6077 write(lunit6, 7077)  lstat(14), flstat(15), flstat(16)
7077 format (5x, 'The last-read data card has been taken by the EMTP to be a branch card for conductor number ', i2,  ' of a   ',/, &
          5x, 'distributed-parameter transmission line.   The user is in trouble because of an inconsistent line-length which  ',/, &
          5x, 'he has punched in columns 45-50 of this last-read card.   The first card for this multiphase line carries a length   ',/, &
          5x, 'of ', e13.4,  ', while the last-read card is for length', e13.4,  '.   Get it all together, guy, and correct   ',/, &
          5x, 'this contradiction.   The easiest way is to put the correct length on the first (zero-sequence) card only, and    ')
  write(lunit6, 7177)
7177 format (5x, 'then leave columns 45-50 of the other cards blank.   Such a blank or zero length is interpreted by the emtp as a ',/, &
          5x, 'default value, meaning the same length as on the first card for the distributed line.    ')
  go to 6220
6078 write(lunit6, 7078 )  flstat(16)
7078 format (5x, 'The last-read data card is for the first conductor (or mode) of a distributed-parameter transmission line.   ',/, &
          5x, 'The field of columns 45-50 is supposed to be punched with the line length, with a value of ', e13.4,  ' read   ',/, &
          5x, "by the EMTP from the user's card.   Thou shalt not input transmission lines having nonpositive length, good buddy.   ")
  go to 6220
6079 write(lunit6, 7079)
7079 format (5x,  'Since an interactive EMTP version is not, being used, illegal data card shown, before "error" heading must be corrected         ',/,  &&
          5x, 'after the program stops, and then case, must be re-run.  Too bad (interactive, correction might have been possible).  ')
  call stoptp
6080 write(lunit6, 7080)  lbus, intinf
7080 format (5x, 'Associated with the steady-state phasor solution is a bias applied to node numbers of sources.   The bus tables  ',/, &
          5x,  'are dimensioned at', i6,  ', which is over one third of the largest integer permitted for this program conversion.   ',/, &
          5x,  "The latter figure is stored in variable  'intinf' ,  which for this conversion was given a value of", i6,  '.   The   ',/, &
          5x, 'EMTP wonders how in the hell the user can have so many busses, or such short integers.   Go see program maintenance   ',/, &
          5x,  'about this problem, fella, .... Fast.   ')
  go to 6220
6081 write(lunit6, 7081)  flstat(15), flstat(16), tmax, deltat
7081 format (5x,  "The user's first miscellaneous data card implies a transient simulation of (in e-field notation)", e15.6,   /, &
          5x, "steps.   This exceeds the limit  'intinf'  for integer numbers for this program conversion of (in e-field notation)", /, &
          1x, e15.6,  '.   The case thus can not be run, because   ',/, &
          5x, 'we can not count high enough to count the final time step.   The aforementioned number of solution steps   ',/, &
          5x,  "his calculated by dividing the user's  'tmax'  (columns 9-16 of the first misc. data card) of", e13.4,  ' seconds   ',/, &
          5x,  "by his time-step  'deltat'  (columns 1-8 of the same card) of", e13.4,  '.   if the user does not understand    ')
  write(lunit6, 7181)
7181 format( 5x, 101hthe error of his ways, he had better pay a call to
1         his friendly neighborhood program maintenance man.   )
          go to 6220
6082      write(lunit6, 7082)  lstat(15)
7082      format( 5x, 103hthe user has overflowed storage within the line-co
1         nstants supporting program.   for the current program   ,/,
2 5       x,  48hversion, one is limited to cases having not over, i4,
3  53     h conductors.   storage for the arrays in question has   ,/,
4 5       x, 115hbeen optimally (and dynamically) allocated so as to use a
5         s much of the emtp core storage as is available.   it thus   ,/,
6 5       x, 115his not the fault of the line-constants supporting program
7         itself, but rather of the overall emtp dimensioning, that   ,/,
8 5       x, 106hthis data case must be rejected.   go procure or make ano
9         ther program version which has more total tabular    )
          write (lunit6, 7182)
7182      format ( 5x, 106hstorage space, and try the job over again.   reme
1         mber, the line constants calculation requires the storage   ,/,
2 5       x, 103hof full matrices, so memory requirements go up approximat
3         ely as the square of the number of conductors.   )
          go to 6220
6083      write (lunit6, 7083)  lstat(15)
7083      format (5x, 107hfor inputting conductor data of a line-constants c
1         ase, data-field  'xtype'  of columns 17-18 can be punched   ,/,
2 5       x, 115hwith a  '4' .   if this be the case, the emtp expects a s
3         pecification for the skin-effect calculation to be punched   ,/,
4 5       x, 114hin columns 4-8 (data field  'rtype' ).   but the user has
5         left field  'rtype'  blank (or has punched a zero, which    ,/,
6 5       x,  91his no better), indicating that no skin-effect correction
7         is to be made for conductor number,  i3,  14h of the sorted  ,/,
8 5       x, 113hinput list.   this contradictory situation is not permitt
9         ed.   reconcile the data fields  'xtype'  and  'rtype' ,  )
          write (lunit6, 7183)
7183      format ( 5x,  14hand try again.    )
          go to 6220
6084      write (lunit6, 7084)  lstat(15)
7084      format (5x, 106hthe just-printed conductor has geometrical x-y pos
1         itioning (i.e., horizontal distance and height) which is   ,/,
2 5       x,  39hidentical with that of conductor number, i3,  62h of the
3         sorted input.   thou shalt not place two or more solid    ,/,
4 5       x,  50hobjects in the same space at the same time, fella.   )
          go to 6220
6085      write (lunit6, 7085)  lstat(15)
7085      format (5x,  59hthe user has failed to input a conductor for circu
1         it number,  i3,  39h.   remember, circuit numbers must span   ,/,
2 5       x, 111hthe full range of numbers from unity through the highest
3         conductor number, without any gaps (unused numbers) in   ,/,
4 5       x,  50hbetween.   renumber the conductors, and try again.   )
          go to 6220
6086      write (lunit6, 7086)  lstat(15), flstat(16), lstat(13), lstat(14)
7086      format (5x, 110hgo see program maintenance about this one, fast.
1         the case has stopped inside subroutine "redu44" of the line   ,/,
2 5       x,  51hconstants calculation, with diagonal element of row, i3,
3   9     h equal to, e14.5,  30h, which is less than hermann's   ,/,
4 5       x,  80hbuild-in fixed tolerance of  1.0e-8 .   parameters  'n'
5         and  'm'  are equal to , 2i5,  1h.   )
          go to 6220
6087      write (lunit6, 7087)
7087      format (5x,  38h big trouble.   the computer operating,
1             39 h system has interrupted emtp execution.,
2             39 h   this emtp solution is hereby fatally
3         ,/,  5x,  37h wounded.   if more information about,
4             35 h the interrupt is known, it will be,
5             38 h found immediately preceding the first
6         ,/,  5x,  38h "error  error ...."  line above.   if,
7             35 h this computer system can skip to a,
8             38 h following data case, it will (below).  )
          go to 6220
6088      write (lunit6, 7088)  lstat(32), nenerg
7088      format (5x, 107hthis is a statistical-overvoltage study, in which
1         all solutions are now completed, and the emtp is ready to   ,/,
2 5       x, 112hperform the final statistical processing (calculation of
3         cumulative distribution functions, etc.).   to do this,     ,/,
4 5       x,  20hthe output vectors (,  i3,  43h cells long) for all of th
5         e energizations (,  i3, 34h in number) must be simultaneously  ,/,
6 5       x, 110hstored in core.   this is the dominant demand for memory.
7         there also is the need to store various vectors (a         )
          write (lunit6, 7288)   kswtch,  nenerg
7288      format (5x,  85hlinear overhead).   total memory requirements are
1         given by the following formula ....         ,/,
2 10      x,  70hstorage  =  9*kswtch  +  (nenerg +4)*nvar  +  nc  +  nto
3         t  +  2*liminc            ,/,   5x,   10hwhere  ...   ,/,
4 10      x,   9hkswtch  =,  i4,  45h  =  number of switches in the emtp
5         data case            ,/,
6 10      x,    9hnenerg  =,  i4,  28h  =  number of energizations    )
          write (lunit6, 7388)   lstat(32), nc, ntot, lstat(14)
7388      format ( 10x,   9hnvar    =,  i4,  64h  =  number of output variab
1         les for the energization simulations                         ,/,
2 10      x,   9hnc      =,  i4,  71h  =  number of branch output variabl
3         es for the energization simulations                          ,/,
4 10      x,   9hntot    =,  i4,  53h  =  total number of network nodes (
5         including ground)                                            ,/,
6 10      x,   9hliminc  =,  i4,  71h  =  maximum number of compartments
7         for any one statistical tabulation.                          )
          write (lunit6, 7488)   lstat(15),  lstat(16)
7488      format ( 5x,  27hbut this requirement totals,  i7,
1  37     h   cells, which exceeds the available,  i7   )
          write (lunit6, 7188)
7188      format (5x, 103hcells of working space.   to run this case without
1         increasing the emtp dimensions, the user must either    ,/,
2 5       x, 112hdecrease the number of energizations, or decrease the num
3         ber of output quantities, or both.   alternatively, the    ,/,
4 5       x, 111huser may be willing to increase program dimensions, in wh
5         ich case any of the independent lists can be increased   ,/,
6 5       x,  73h(all are equally good, directly contributing to the stora
7         ge in question).     )
          go to 6220
6089      write (lunit6, 7089)  flstat(15), flstat(16)
7089      format (5x, 106hthe last-read card is a switch-card, bearing the k
1         ey word  'statistics'  in columns 55-64.   the switch is    ,/,
2 5       x, 110hthus a circuit-breaker pole, for which the closing time i
3         s to be a random variable.   but either the specified   ,/,
4 5       x, 113hvariable mean (punched in field  'tclose' ,   columns 15-
524       ) or the standard deviation (field  'topen' ,  columns   ,/,
6 5       x,  84h25-34) has been punched as negative, which is not allowed
7         .   the two values read are, e14.5,    4h and, /,
8 5       x, e14.5,   15h, respectively.    )
          if ( lstat(15) .eq. 4433 )
1         write (lunit6, 7189)  lstat(14)
7189      format ( 5x, 35h====  correction  ====  the problem,
1             36 h switch in question is not the last-,
2             34 hread one, but rather switch number,  i5
3         ,/,   5x, 35h                        in order of,
4             35 h input.  in fact, all switches have,
5             18 h been read by now.         )
          go to 6220
6090      write (lunit6, 7090)  flstat(14), flstat(15)
7090      format (5x, 103hthe emtp has just begun reading data for the conve
1         rsion of an rms current-voltage saturation curve into   ,/,
2 5       x, 112hcurrent-flux form.   but illegal values for either  'vbas
3         e'  or  'pbase'  have been read from the last-read data  ,/,
4 5       x, 108hcard (columns 9-16  and  17-24, respectively).   both of
5         these fields must be punched with positive numbers.   ,/,
6 5       x,  31hbut the emtp has read values of, e14.4,  4h and,
7         e14.4,  35h for these variables, respectively.  )
6220      lastov = nchain
          nchain = nfrfld + 50
          if ( iprsup  .ge.  1 )
1         write ( lunit6, 4568 )
4568      format ( 24h  "exit  module over52." )
99999     return
        end function supporting
        subroutine err87 ( lstat, flstat, lunit6 )
          implicit real*8 (a-h, o-z) ,
1         integer*4 (i-n)
          dimension lstat(1), flstat(1)
          write (lunit6, 7087)
7087      format (5x, 107has part of the interactive crt plotting or  'replo
1         t'  features of the emtp,  plot data points are stored on   ,/,
2 5       x, 111hdisk as a  'permanent file'  (as opposed to temporary, sc
3         ratch storage, which is used for every emtp run).   it   ,/,
4 5       x, 111his the operation of internally cataloging this data file
5         as a permanent file, inside the emtp, which has gotten   ,/,
6 5       x, 114hthe emtp into trouble with the computer operating system
7         (otherwise affectionately known as 'bigger big brother').     )
          return
        end subroutine err87
        c
        c     end of file: over52.for
        c
