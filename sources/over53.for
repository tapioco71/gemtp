!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over53.for
!
!
!     subroutine over53.
!
subroutine over53
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  if ( iprsup  .ge.  1 ) write ( lunit6, 4567 )
4567 format ( ' "begin module over53." ')
  if ( nchain  .ne.  53 )   go to 99999
  n1 = kill - 90
  go to (6091 , 6092, 6093, 6094, 6095, 6096, 6097, 6098, 6099, 6100, 6101, 6102, 6103, 6104, 6105, 6106, 6107, 6108, 6109, 6110, &
       6111, 6112, 6113, 6114, 6115, 6116, 6117, 6118, 6119, 6120, 6121, 6122, 6123, 6124, 6125, 6126, 6127, 6128, 6129, 6130, &
       6131, 6132, 6133, 6134, 6135, 6136, 6137, 6138, 6139, 6140, 6141, 6142, 6143, 6144, 6145, 6146, 6147, 6148, 6149, 6150, &
       6220 ) , n1
6091 write (lunit6, 7091)
7091 format (5x, "The data case now in its final stages of input is for a statistical overvoltage study, since field  'nenerg'",/, &
       5x, 'of columns 65-72 of the 2nd miscellaneous data card was punched with a positive number.   Yet the last-read data',/, &
       5x, "card requests the output of all network node voltages, by means of the  '1'  punched in column 2.   But the EMTP",/, &
       5x, 'does not allow this complete voltage output for statistical overvoltage studies.   If the user really wants to look ')
  write (lunit6, 7191)
7191 format (5x, 'at every node voltage in such a study, he must request each such output individually, using the selective',/, &
       5x,  'node voltage output option. ')
6092 write (lunit6, 7092)
7092 format (5x, "The EMTP logic has just attempted to read another data card by means of a call to subroutine  'cimage' .",/, &
       5x, 'But no more data is to be found on logical unit number 5.   An end-of-file mark has been encountered upon attempting',/, &
       5x, 'to read from this unit.   execution must be stopped, since data which is required for continuation of the study',/, &
       5x,  'simply does not exist. ')
  go to 6220
6093 write (lunit6, 7093)
7093 format (5x, 'The last-read data card is for one phase (mode) of a distributed-parameter transmission line.   But one or more',/, &
       5x, 'of the parameter values read therefrom makes the data illegal.   The four fields of columns 27-50 are not all',/, &
       5x, 'blank, so this mode data is not taken by the EMTP to be a request for a copy of the parameters of the preceding mode.',/, &
       5x, 'Hence columns 27-32 must be punched with mode resistance per unit length, columns 33-44 must be punched for the ')
  write (lunit6, 7193)  flstat(14), flstat(15)
7193 format (5x, 'mode inductance and capacitance (possibly disguized, in terms of surge impedance, propagation speed, or',/, &
       5x, 'travel time), and finally, columns 45-50 must contain the line length.   Now one or both of the two fields which',/, &
       5x, "equally divide columns 33-44 is non-positive, which is illegal.   Values read from the user's card for these",/, &
       5x, 'two fields were',    e14.4 ,   ' and',   e14.4 , ' ,   respectively.   Remember, zero inductance or ')
  write (lunit6, 7293)
7293 format (5x, "capacitance (the case when  'iline'  is zero)  is illegal because it implies infinite velocity of propagation.",/, &
       5x, 'Similar absurdities result for the other two forms of disguizing inductance and capacitance, it can readily be',/, &
       5x,   'seen. ')
  go to 6220
6094 write (lunit6, 7094)  flstat(14), flstat(15)
7094 format (5x, 'The last-read data card has been taken by the t.p. to be a switch card, with closing time to be determined',/, &
       5x, 'randomly as part of a statistical overvoltage study.   But the mean closing time of',  e13.4, '  seconds and the',/, &
       5x,  'standard deviation of',  e13.4, '  seconds are such that switch closing times less than zero would not be totally',/, &
       5x, 'unexpected.   The mean must not be less than  3.0  times the standard deviation, if the data is to be acceptable ')
  write (lunit6, 7194)
7194 format (5x, 'to the EMTP in the case of gaussian (normal) distribution of the switch closing times.   This ensures',/, &
       5x, 'that there is a probability of less than  0.13  percent for any particular switch closing time to be negative',/, &
       5x, "for a normal distribution.   But the user's data has failed this check, for the last-read switch.   Because the",/, &
       5x, "EMTP has no proper way of handling negative switch closing times (they are set to time zero, actually), the user's",/, &
       5x,  'data case must be rejected.',/,1x)
  write (lunit6, 7294)
7294 format (5x, 'If the user really did desire to use the mean and standard deviation which got him into trouble here, he is advised',/, &
       5x, 'of the possibility of delaying closure of all switches by exactly one (or more) fundamental-frequency cycles.',/, &
       5x, 'Provided one is starting from the sinusoidal steady state, the added delay of an integer number of cycles simply',/, &
       5x, 'delays the response accordingly, leaving it otherwise unchanged.   By adding one cycle of fundamental-frequency time ')
  write (lunit6, 7394)
7394 format ( 5x, 'to the mean of the closing time of all switches in the data case, the aforementioned problem of negative closing',/, &
       5x,  'times is usually eliminated, and the case can be run without difficulty. ')
  write (lunit6, 7594)
7594 format (/, 5x, "For an independent switch (fields  'bus5'  and 'bus6'  of columns 65-76 of the switch card being both",/, &
       5x, "blank), the mean closing time  'tbar'  is read from columns  15-24 ,   and the standard deviation  'sigma'  is",/, &
       5x, "read from columns  25-34 .    This is simple.   But should the  'bus5'  and  'bus6'  fields be other than both",/, &
       5x, "blank, then  'tbar'  and  'sigma'  depend not only on the column  15-34  numbers of the switch card for the ")
  write (lunit6, 7694)
7694 format (5x, 'switch in question, but also on all of the other corresponding column  15-34  numbers as the dependency chain',/, &
       5x, "is traced back through previously-inputted switches.   In this case of a dependent switch,   'tbar'  is the sum",/, &
       5x, "of the column  15-24  numbers in the chain, while  'sigma'  is the square root of the sum of the squares of the",/, &
       5x,  'column  25-36  numbers in the chain. ')
  go to 6220
6095 write (lunit6, 7094)  flstat(14), flstat(15)
  write (lunit6, 7095)
7095 format (5x, 'The mean must not be less than the square root of three times the standard deviation.   This condition would',/, &
       5x, 'guarantee that switch closing times would always be positive, for the uniform distribution which the user has',/, &
       5x,  "requested.   But the user's data has failed this check, for the last-read switch.   Because the",/, &
       5x, "EMTP has no proper way of handling negative switch closing times (they are set to time zero, actually), the user's",/, &
       5x,  'data case must be rejected.',/,1x)
  write (lunit6, 7294)
  write (lunit6, 7394)
  go to 6220
6096 write (lunit6, 7096)
7096 format (5x, 'This run is being terminated due to an attempt to plot results.   The EMTP can be variably-dimensioned,',/, &
       5x, 'of course, with the present version simply having no available working space in which even the plot-file header',/, &
       5x, 'information can be stored.   If the user wants to do plotting, he must either do it interactively on the crt, or',/, &
       5x, 'he must work with a program version which has more labeled-common storage. ')
  write (lunit6, 7196)
7196 format (5x, 'More specifically, the size of the working area in this particular non-solution overlay is set by variable-',/, &
       5x, "dimensioning program  'vardim'  according to the relation size = szlab - offset.   In this formula, ....",/, &
       10x,  "'size'  =  the number of floating-point words of working space (insufficient in this case),",/, &
       10x,  "'szlab'  =  the number of floating-point words used for labeled common in the solution overlays,            ")
  write (lunit6, 7296)
7296 format (10x, "'offset'  =  a fixed, non-negative offset which approximately equals the number of floating-point words    ",/, &
       23x,  'by which the compiled instructions of this overlay exceeds the corresponding figure for the longest',/, &
       23x, 'solution overlay (one of the secondary-level modules).',/, &
       5x, "Should  'offset'  exceed  'szlab' ,   then  'size'  is set to unity, so as to avoid compilation errors (fortran",/, &
       5x, "generally requires positive dimensions for all arrays).   Recall that  'szlab'  is printed out for the user every ")
  write (lunit6, 7396)   lstat(14), lstat(15)
7396 format (5x, 'time the EMTP is executed, as part of the header information that precedes the listing of the input data.',/, &
       5x, "For the present case in question, the EMTP finds  'size' equal to",  i6, ' ,   while a minimum of',  i7,  '  words',/, &
       5x, 'are needed in order for execution to continue beyond the point where it is now being stopped.   If it is not clear',/, &
       5x, 'as to what remedial action should be taken, the user is advised to seek experienced counsel about this matter. ')
  write (lunit6, 7496)
7496 format (5x, 'A brief caution about use of a marginal working are a for plotting is perhaps also in order, however.   The',/, &
       5x, 'aforementioned minimum figure should allow for correct plotting, but will generally produce repeated buffer-',/, &
       5x, 'flushing-type spillover onto logical unit number  9  during the plotting of each graph, with its associated',/, &
       5x, "slowdown in execution speed.   In order to avoid all such temporary disk storage,   'size'  can be set no smaller ")
  write (lunit6, 7596)
7596 format (5x, 'than twice the number of plot points of the particular graph multiplied by the number of curves on the graph.',/, &
       5x, "double-precision conversions (ibm  real*8  is the common example) can generally ignore the just-stated  'twice'",/, &
       5x, 'factor, since plotting-point storage for use with software plotting routines like calcomp is almost invariably',/, &
       5x,  'single-precision. ')
  go to 6220
6097 write (lunit6, 7097)  lstat(14), lstat(15)
7097 format (5x,  'The last-read data card has an =e=-field number which is not right-adjusted in its data field.',/, &
       5x,  'In particular, either the letter =e=, or a sign following a numeral was found in the field',/, &
       5x,  'which starts in column number ', i4, ', while column number ', i4,  ', which is the right-most column of the field',/, &
       5x,  'is not punched explicitely with a decimal digit (i.e., 0, 1, .... 9 ).',/, &
       5x, "Now it may well be that such usage is indeed legal fortran data-format input for the user's own particular",/, &
       5x, 'computer system, but it offends the keenly-honed sensibilities of the EMTP (at whose mercy the user now finds ')
  write (lunit6, 7197)
7197 format ( 5x, 'himself, it may be noted).   Too often a user will erroneously wind up with an exponent of ten which is in',/, &
       5x, 'error by a power of ten itself, in such a case.   Hence such data is not allowed by the EMTP   repunch this data',/, &
       5x,  'record, and try again, friend. ')
  write (lunit6, 7297 )
7297 format (5x, 'Alternatively, a card sequence error may have resulted in an attempt to read data with the wrong format.',/, 5x, &
       'Check for missing or out-of-sequence data. ')
  go to 6220
6098 write (lunit6, 7098)  lstat(14), bus1, lstat(15)
7098 format (5x,  'The last-read data card has an =i=-field number which is not right-adjusted in its data field.   In',/, &
       5x,  'particular, column number',  i4, '  contains the character  =',  a1,   '= ,  while column',  i3, ', which is the',/, &
       5x, 'right-most column of the field in question, is blank.   Now maybe such usage is legal fortran input for the user=s',/, &
       5x, 'computer system, but it offends the keenly-honed sensibilities of the EMTP (at whose mercy the user now finds ')
  write (lunit6, 7198)
7198 format (5x, 'himself, it may be noted).   Too often a user will erroneously wind up with a value which is in error by',/, &
       5x, 'a power of ten, in such a case.   Hence such data is simply not allowed by the EMTP   repunch this data',/, &
       5x,  'record, and try again, friend. ')
  go to 6220
6099 write (lunit6, 7099)
7099 format (5x, 'The EMTP has finished the calculation of weighting functions  a1(t)  and  a2(t)  for the user=s frequency-',/, &
       5x, 'dependent line-mode.   Now the program is ready to output these functions --- on punched cards, if the user has',/, &
       5x, 'given variable  =ipunch=  (of columns 33-40 of the second data card) a value of zero.   But this is not possible',/, &
       5x,  'within the  =f=-format fields which have been provided, without overflow occurring. ')
  write (lunit6, 7199)  flstat(15), flstat(16)
7199 format (5x, 'Specifically, the peak weighting function values can not both be punched using  f10.0  format.   The peak',/, &
       5x,  'values in question are, /, 10x, 12hmax(a1(t)) =', e14.5, 10x, 'max(a2(t)) =', e14.5, ' .'   ,/, &
       5x, 'in some way, the user=s data would seem to be highly unusual, and worthy of careful scrutiny.   Two special cases',/, &
       5x, 'which will always lead to numerical trouble are perhaps worthy of mention, in this regard.   First, as the line-')
  write (lunit6, 7299)
7299 format (5x, 'length  =dist=  (columns 1-8 of the first data card) approaches zero,    max(a1(t))    is known to approach',/, &
       5x, 'infinity.   Similar behavior will also result for any fixed-length line, as the impulse attenuation approaches',/, &
       5x, 'zero (e.g., let  r  approach zero and  l  approach a constant value, for all frequencies).   The user is advised',/, &
       5x,  'to seek experienced counsel, if he can not find an obvious massive data error on his own. ')
  go to 6220
6100 write (lunit6, 7099)
  write (lunit6, 7100)  flstat(15)
7100 format (5x, 'Specifically, the weighting functions extend too far out in time, with duration in excess of the  100000',/, &
       5x, 'microseconds which can be punched legally using  f10.4  format.   With light traveling at a rate of  300 km',/, &
       5x, '(186 miles)  per millisecond, this is absurd for a power line, and must be rejected.   Electrically, the user=s',/, &
       5x, 'line is approximately one order of magnitude longer than anything which is typical for american power systems.',/, &
       5x,  "The user's weighting functions have a duration of", e13.4,  '  microseconds. ')
  write (lunit6, 7200)
7200 format (5x, 'The user is reminded that even if he has a very long line of say  1000  miles, he is advised to break it up ',/, &
       5x, 'into maybe five identical segments of  200  miles each.   Then he derives weighting functions for one 200-mile',/, &
       5x, 'segment, and uses the reference-branch capability in setting up the EMTP data case for the network in question,',/, &
       5x, 'thereby saving  4/5  on the otherwise-required weighting-function storage of list number  14 . ')
  go to 6220
6101 write (lunit6, 7101)
7101 format (5x, 'The decision to kill this run has been made within the dummy (i.e., the original, the empty) subroutine',/, &
       5x, " 'analyt'  which is called by subroutine  'subts3'  of overlay 16 (overlay 18 when using ecs overlaying of the",/, &
       5x, 'time-step loop on cdc).   The user declared his intent to use analytically-defined sources at the beginning of',/, &
       5x, "this data case by inputting the special  'analytic sources'  request card.   But he has failed to have program ")
  write (lunit6, 7201)
7201 format (5x, "maintenance replace the original, dummy module  'analyt'  by the one which he should have written especially",/, &
       5x, 'for this data case.   The user needs a module which defines his own special sources --- one or more sources of',/, &
       5x, "type  1  through  10 ,   Defined in fortran (or machine language) within subroutine  'analyt' . ")
  go to 6220
6102 write (lunit6, 7102)  lstat(15), bus1, bus2, flstat(14)
7102 format (5x,    'linear branch number', i4,  '  (in order of data input) connects bus  ', "'", a6, "'", '  with bus  ', "'", a6,  "' .", /, &
       5x, 'data values on this card are for a mode of the distributed-parameter line which is being modeled with resistance',/, &
       5x, "lumped, one half in the middle, and one quarter at each each end (field  'ipunch'  of columns 53-54 equal to -1 ).",/, &
       5x,  'But the total modal line resistance of',    e15.5,   'Ohms is unreasonably large when compared with the surge ')
  write (lunit6, 7202)  flstat(15)
7202 format (5x,  '(characteristic) impedance of',     e15.5,   'Ohms.   Program logic can not handle a resistance which',/, &
       5x, 'exceeds four times the characteristic impedance.   For normal power lines, such values are absurd anyway, so',/, &
       5x,  'the user gets little sympathy on this one. ')
  go to 6220
6103 if(lstat(12).gt.0)write(lunit6,7103)lstat(12)
7103 format (5x, 'Possible loss of significance in elimination step ', 1x, i4, 1x, 'in subroutine dgelg',/)
  go to 6220
6104 write (lunit6, 7104)  iprsov(37), lstat(12)
7104 format (5x, 'the data case now being solved involves one or more dynamic synchronous machine (s.m.) source components.',/, &
       5x, "The associated equations (Park's or Blondel's) are nonlinear, and must be solved by an iterative procedure at",/, &
       5x,  'each time step.   This iteration has failed to converge within the iteration limit of',  i5, ' ,   for dynamic',/, &
       5x,  's.m. number',  i5, "  (in order of input).   The iteration limit is variable  'niomax' ,   which can be ")
  write (lunit6, 7204)  bus1, epomeg, flstat(14)
7204 format (5x, "re-defined during the s.m. data input, using a  'tolerances'  special-request card.   The s.m. in question has",/, &
       5x,  "phase 'a' connected to bus  ",  "'", a6,  "'", " .   The unachieved tolerance is variable  'epomeg' ,   which has value",/, &
       5x,      e14.3,    ' .   the fractional speed discrepancy on the final iteration was',    e16.7,      ' .   assuming',/, &
       5x, "that  'niomax'  is reasonable (say five or larger), then the effect of two remaining variables might profitably")
  write (lunit6, 7304)  deltat
7304 format (5x, "be contemplated.   First, the user should check that his time-step size  'deltat'  is not too big, so as",/, &
       5x, "to insure that the change in speed between successive time steps will be small.   This data case used  'deltat' =",/, &
       5x,    e14.3,    " .   second and more subtle is the choice of tolerance  'epomeg' ,   which should not be set smaller",/, &
       5x,  'than the roundoff limitation which applies to the iterative correction of the speed. ')
  go to 6220
6105 write (lunit6, 7105)  lstat(14)
7105 format (5x, 'The last-read data card is the first card of class-2 data for a dynamic synchronous machine (s.m.) source',/, &
       5x,  "component.   Variable  'numas'  as read from columns 1-2 of this card (using  i2  format) has value",  i5,  ' .' ,/, &
       5x, 'this is supposed to be the number of masses which make up the shaft system of the rotor of the machine.   But' ,/, &
       5x,  'this figure is non-positive .')
  go to 6220
6106 d1 = 1.0 / fltinf
  write(lunit6,7106)  bus1
7106 format (5x,  'Erroneous data discovered during the initialization of multiple s.m. connected to bus', "'", a6, "'", '. The specified', /, &
       5x, "values of parameters 'smoutp' and/or 'smoutq' for the involved s.m.'s add up to zero.", /, &
       5x, 'Rrecall that any blank field used in the above data is replaced by the near-zero EMTP tolerance 1.0/fltinf =')
  write (lunit6, 7206)  d1
7206 format (5x, e14.3, ' .   The user should never use the negative of this value for any of the four parameters in',/, &
       5x, "question.   Also, he must not punch one of the  'smoutp' fields so as to be the negative of the other.   The",/, &
       5x,  "same prohibition applies to the two  'smoutq'  values. ")
  go to 6220
6107 write(lunit6,7107)
7107 format (5x, ' Fractional mass torque parameters do not conform to rules -- see user manual',/)
  go to 6220
6108 write (lunit6, 7108)
7108 format (5x, 'The last-read data card is taken by the EMTP to be a request for an auxiliary synchronous machine (s.m.) input',/, &
       5x, 'to tacs.   Recall that such data cards (if any) complete the data input for a dynamic s.m., after being followed',/, &
       5x, "by a terminator record which bears the text  'finish'  in columns 3-8.   But the last-read data card is illegal, ")
  if ( lstat(16)  .le.  lstat(15) )   go to 7308
  write (lunit6, 7208)  lstat(15)
7208 format (5x, 'since storage for such s.m.-tacs interface variables has been overflowed.   The existing program dimension',/, &
       5x,  'is',  i4,  " ,   which is equal to the number of previously-defined interface variables in arrays  'etac  '  and",/, &
       5x,  "'ismtac' . ")
  go to 6220
7308 if( lstat( 17 ) .gt. 0 )  go to 7508
  write (lunit6, 7408)   bus6
7408 format (5x, 'since the name ', "'", a6, "'", ' of the interface variable (as read from columns 3-8 using a6 format) is invalid.', /, &
       5x, 'All such names must be valid, for they serve as names of tacs variables (for type-92 tacs sources). ')
  go to 6220
7508 write (lunit6, 7608)  lstat( 14 ),  lstat( 13 )
7608 format (5x, 'Since the data read from columns 1-2 using  i2 format and from columns 15-17 using i3 format is invalid .,', /, &
       5x, 'the read values were....', i4, i5 )
  go to 6220
6109 write( lunit6, 7109 )   kill
7109 format ( 5x, 'Unused kill code number....', i5  )
  go to 6220
6110 write (lunit6, 7110)  nenerg, bus5, bus6
7110 format (5x,  "Parameter  'nenerg'  of the integer miscellaneous data card was punched nonzero (value", i5, '  )   for this',/, &
       5x, "data case, representing a request for a  'statistics'  or a  'systematic'  simulation.   The last-read data card",/, &
       5x, 'his for an EMTP switch, and it bears one of the two just-mentioned key words in columns 55-64, which is fine.',/, &
       5x, 'But this is a dependent switch, with illegal reference names  ', "'", a6,  "'", '  and  ', "'", a6,  "'", '  punched for data ')
  write (lunit6, 7210)  lstat(15), lstat(16)
7210 format (5x, "fields  'bus5'  and  'bus6'  (columns 65-76).   The ordered pair of reference names corresponds to EMTP node",/, &
       5x, 'numbers', i5,  '  and',  i5, ' ,   respectively, with azero value meaning that the associated  a6  name is',/, &
       5x, 'illegal (unrecognizable as a legitimate EMTP electrical network node name).   If both of the just-printed',/, &
       5x, 'integers are positive, then both names are legitimate EMTP network names, but do not correspond to a previously-')
  write (lunit6, 7310)
7310 format (5x, 'inputted switch.   Remember that ordering of the pair of reference names is crucial, with it being mandatory',/, &
       5x, "that the first reference name  'bus5'  match a first switch name  'bus1' ,   etc. for  'bus6'  matching  'bus2' . ")
  go to 6220
6111 write (lunit6, 7111)   lstat(13), lstat(15), lstat(16), lstat(17)
7111 format (5x, 'One of the working vectors which are used to assemble output quantities for all dynamic synchronous machine',/, &
       5x, 'source components has overflowed.   The dimensioned limit for the number of  a6  identification names is',  i6,  ' ,  ',/, &
       5x, 'as is the maximum allowable number of output quantities. But after finishing with machine number',  i5,  ' ,',/, &
       5x, 'the two corresponding table sizes are',  i6,  '   and', i6,  ' ,   respectively.   One or both of these table',/, &
       5x, 'sizes has now been exceeded.   Execution will be stopped.   Either request fewer outputs, or redimension the tables.')
  go to 6220
6112 write (lunit6, 7112)  kswtch
7112 format (5x, 'Wow.   Double wow (wow, wow).   The EMTP has finished with the input of all switch cards for this data case.',/, &
       5x, 'of the', i5,  "  switches, over ten had the key word  'statistics'  or  'systematic'  punched in columns 55-64.",/, &
       5x, "in fact, there are over ten independent  'statistics'  or 'systematic'  switches  ----  distinguished by either",/, &
       5x, "field  'bus5'  (cols. 65-70)  punched with  'target' , or both fields  'bus5'  and  'bus6'  (cols. 65-76) ")
  write (lunit6, 7212)  lstat(14), bus1, bus2
7212 format (5x,  'left blank.   The EMTP limit is ten, however, which was exceeded by switch number', i5,  ' ,   which connects',/, &
       5x, "node  ', "'", a6,  "'", '  with node  ', "'", a6,  "'", ' .   The EMTP wonders whether the user really appreciates the",/, &
       5x, 'significance of ten independent switches, or equivalently , the vastness of a ten-dimensional vector space.',/, &
       5x, 'remember, even if one quantized each dimension into only two discrete values, ten independent variables would ')
  write (lunit6, 7312)
7312 format (5x, 'yield   2**10 = 1024   compartments or cells.   With three discrete values, the figure becomes  59049.   and     ',/, &
       5x, "with four, it jumps to the staggering figure of  1048576. The user's computer system may be fast, but not swift     ",/, &
       5x, 'enough to even begin to realistically sample a ten-dimensional vector space in a representative fashion.   Execution    ',/, &
       5x,  'will not be allowed to continue, for this data case.   ')
  go to 6220
6113 n2 = lstat(13)
  write (lunit6, 7113)  nenerg
7113 format (5x, "The data case now being processed is for a  'systematic'  study, since integer miscellaneous data parameter",/, &
       5x, " 'nenerg'  was punched as a negative number.   Using  i8 format, a value of", i6,  "   was read for  'nenerg'",/, &
       5x, 'from columns 65-72.   By definition, this is supposed to be the negative of the number of energizations which',/, &
       5x, 'are to be run.   Of course this figure is determined by multiplying together the number of steps to be taken ')
  write (lunit6, 7213)  lstat(12), kswtch
7213 format (5x, "by each independent  'systematic'  switch.   But when the EMTP multiplies together all such integers",/, &
       5x, "which were read using  i10  format from columns 35-44 of 'systematic'  switch cards, it finds the product to be",/, &
       5x, 'equal to', i6,  " .    This value is inconsistent with the aforementioned  'nenerg' .    To aid the user in",/, &
       5x,  'correcting this discrepancy, it is mentioned that this data case has', i5,  '  switches.   In order of data ')
  write (lunit6, 7313)  numref,  ( ipntv(i), i=1, numref )
7313 format (5x,  'input, the integers read from the', i4,  "  independent  'systematic'  switch cards are as follows ....",/, &
       ( 1x, 12i10 ) )
  do i=1, numref
     if ( ipntv(i)  .le.  0 )   go to 7513
7413 end do
  go to 6220
7513 write (lunit6, 7613)  i, ipntv(i)
7613 format (5x,  'Upon inspecting these figures, the EMTP immediately notices that number', i4,  '  is illegal.   A value of',/, &
       5x, i6, "   is seen, which certainly is not positive (as per the user's manual instructions).   Remember, any  'systematic'",/, &
       5x, "switch card which has either field  'bus5'  (cols. 65-70) punched with  'target' ,   or both fields  'bus5'  and",/, &
       5x, "'bus6'  (cols. 65-76)  left blank, is an independent switch.   Columns 35-44 of such a switch must then be ")
  write (lunit6, 7713)
7713 format (5x,  'punched with the number of steps (necessarily positive) which are to be taken with this switch. ')
  go to 6220
6114 write (lunit6, 7114)
7114 format (  8h unused.  )
6115 write (lunit6, 7115)  lstat(14), bus1
7115 format (5x, '"he user has been inputting 'tacs' data, specifically cards which define tacs function blocks.   Yet the",/, &
       5x, "last-read data card has an illegal integer  'n'  punched in the field of columns 1-2.   A value of", i4,  '  was',/, &
       5x, "read, for this block having (output) variable name  ', "'",  a6,  "'", ' .   since this integer does not equal one of',/, &
       5x, 'the special code numbers which are used to indicate special block types (e.g., n=99 for a zero-th order block), it ')
  write (lunit6, 7215)
7215 format (5x, 'is taken by the EMTP to be the order of the Laplace transfer function  h(s)  of the block.   But this must',/, &
       5x, 'be positive.   Also, a limit of 10-th order has been arbitrarily imposed by the EMTP.   If the user really does',/, &
       5x, 'have a higher-order  h(s)  than 10 (highly unlikely), he must simply split his original  h(s)  into two or more',/, &
       5x, 'cascaded sub-blocks, each of which is of order  10  or less. ')
  go to 6220
6116 write (lunit6, 7116)  bus1, lstat(14)
7116 format (5x, 'The EMTP data case under consideration includes a tacs representation, the function blocks of which are now',/, &
       5x, 'being inputted.   Now, the last tacs function block which was read by the EMTP had (output) name  ', "'", a6, "'",/, &
       5x,  '(punched in columns 3-8), and was purported to be a dynamic function block of order', i3, '  (punched in columns',/, &
       5x, '1-2).   The tacs function block is characterized by a Laplace transfer function  h(s) ,   which is a rational ')
  if ( lstat( 17) .eq. 1 )  go to 16116
  write (lunit6, 7216)  lstat(14)
7216 format (5x, "function (ratio of polynomials) of the complex frequency variable  's' .   By definition, the order of the",/, &
       5x, 'tacs block refers to the order of the highest polynomial involved (either numerator or denominator).   But both',/, &
       5x, "the numerator and denominator coefficients of  's'  raised to the power", i3,  '  are zero.   The highest power',/, &
       5x, "of  's'  is thus missing, completely.   The block order is inconsistent with the polynomial coefficients which",/, &
       5x,  "are supposed to define the block's  H(s) ,   so execution is being terminated. ")
  go to 6220
16116 write (lunit6, 7316)
7316 format (5x,  "Function (ratio of polynomials) of the complex frequency variable 's' .,",/,&
       5x, "here, the order-zero coefficients (of the terms without 's')  in both numerator and denominator", /, &
       5x, 'were read as  zero .',/, &
       5x, "did the user really mean this, the program wonders.   Don't. ")
  go to 6220
6117 n9 = 50
  write (lunit6, 7117)   n9, ipunch
7117 format (5x, 'The last-read data card is the first branch card for a multi-phase line which is to be modeled using Semlyen',/, &
       5x, 'or Ametani recursive convolution for the frequency-dependent representation.   Such modeling is limited to a',/, &
       5x,  'maximum of', i4, "   modes (coupled phases).   But this limit is exceeded by the user's requested number, which",/,&
       5x,  'was punched as',  i5,  '   (read from columns 77-78).   A major programming effort (involving some unanswered ')
  write (lunit6, 7217)
7217 format (5x, 'theoretical questions) would be required in order to extend this modeling limitation, so the user has no other',/, &
       5x, 'alternative than decreasing the number of coupled Ametani or Semlyen phases in his coupled branch group. ')
  go to 6220
6118 write (lunit6, 7118)
7118 format (5x, 'The blank card which terminates the input of tacs data cards which define function blocks has now just been',/, &
       5x, 'read.   But no function-block definitions preceded this blank card.   The user will not be allowed to continue',/, &
       5x, "with such a degenerate tacs representation.  If the user's present representation has any meaning at all, it must",/, &
       5x, 'surely have one or more supplemental variables.   If adding or subtracting is therein involved (as is usually ')
  write (lunit6, 7218)
7218 format (5x, 'the case), a zero-th order function block could have been employed for one of these operations, thereby',/, &
       5x, 'resolving the present difficulty.   Otherwise, as a last resort, the user can add a dummy tacs function block to',/, &
       5x,  'is tacs representation, being careful to provide it with a valid input variable. ')
  go to 6220
6119 write (lunit6, 7147)  bus6
  go to 7421
6120 write (lunit6, 7120)  bus1, lstat(14)
7120 format (5x, "The EMTP is in the process of inputting the user's tacs supplemental-variable data cards, with the last-read",/, &
       5x, 'such data card being in error.   Specifically, the user has attempted to define a supplemental variable named',/, &
       5x, "'", a6, "'", '  (data field of columns 3-8 of the card), for which the type code which has been punched in columns 1-2',/, &
       5x, 'is invalid.   A value of', i4,  '  was read from columns 1 -2, which is an unrecognized type code. ')
  go to 6220
6121 write (lunit6, 7121)  bus2, bus1
7121 format (5x, 'The tacs data which has now all been inputted is incomplete.   Specifically, there is a difficulty associated',/, &
       5x, 'with the tacs function block which was given the (output) name  ', "'", a6, "'", '  (as read from columns 3-8 of the',/, &
       5x, 'leading card which defines this function block).   One of the five possible inputs to this block was given name',/, &
       5x, "'", a6, "'", '  (as read from one of the fields of columns 12-17, 20-25, 28-33, 36-41, and 44-49, using  a6  format). ')
  write (lunit6, 7221)
7221 format (5x, 'But this input variable is undefined.   It is neither the output of another function block, nor a supplemental',/, &
       5x, 'variable, nor a tacs source of any type (1, 2, 90, 91, 92, or 93 type codes).   Did the user make a spelling error, the ',/, &
       5x, 'EMTP wonders.   The EMTP does not know what to do with the aforementioned input, so execution will be terminated. ')
7421 write (lunit6, 7521)
7521 format (/, 5x, 'Since the user is having trouble with 6-character tacs variable names, it is perhaps worth qualifying the',/, &
       5x, 'preceding error text which complains about an unidentifiable name that is associated with a certain tacs component',/, &
       5x, 'or data class.   All that is really involved here is a spelling comparison with other usages of the same variable',/, &
       5x, "name.   When for some particular tacs component a given name  'name1 '  can not be found in a table where it ")
  write (lunit6, 7621)
7621 format (5x, 'belongs, the EMTP says that the former is unrecognizable.   Yet, as used with the component explicitely',/, &
       5x, "mentioned in the message,   'name1 '   may in fact be spelled exactly as the user intended.   It may be the table",/, &
       5x, 'being searched which is in error, due to faulty spelling of the variable name on some other data card (which',/, &
       5x, 'was the usage that generated the table entry).   Hence the user should look at other data cards for mis-spelling of ')
  write (lunit6, 7721)
7721 format (5x, 'the name in question, if the spelling as printed in the above error text is actually as the user wanted it. ')
  write (lunit6, 7821)
7821 format (/, 5x, 'Then too, while talking about spelling, it might be a good idea to emphasize what is involved.   All six-',/, &
       5x, 'character variable names are  a6  fortran alphanumeric information.   When printed out within error messages, the',/, &
       5x, 'six characters in question are delineated by a leading and a trailing quotation or apostrophe mark.   Position',/, &
       5x, "of imbedded blanks is indeed crucial, then.   For example,   'raver '  and  ' raver'  are completely different, ")
  write (lunit6, 7921)
7921 format (5x, 'distinct 6-character names, as far as the EMTP is concerned.   EMTP names (including those of tacs)',/, &
       5x, "consist of an ordered string of 6 characters, with 'blank' being a character like any other one.   Two variable",/, &
       5x, 'names are equal if and only if both characters of any character position are equal for the two names, for all',/, &
       5x, 'possible character positions  1, 2, .... 6 . ')
  go to 6220
6122 write(lunit6,7122)  lstat( 17), lstat( 16)
7122 format (5x, 'The tacs representation currently being processed has overflowed the dimensioned tacs storage.   Specifically,',/, &
       5x, 'the tables which are numbered', i4,  '  (see explanatory directory of table numbers below) which have a length of',/, &
       5x, i4, '  entries are now full.   Because there is not enough room to solve the problem, execution is being stopped.',/,1x)
  write (lunit6, 7222)
7222 format (5x, 'Since tacs can be partially redimensioned at execution time, the user can simply increase the appropriate table',/, &
       5x, 'sizes on his  tacs-table-size cards , and try the data  case again.  Yet such relative size changes can only be',/, &
       5x, 'made within the overall EMTP variable-dimensioning limitation of EMTP list number 19, which constrains the sum',/, &
       5x, 'total space which is allowed for tacs arrays.   Hence overall EMTP redimensioning with a larger size for list ')
  write (lunit6, 7322)
7322 format (5x, 'number 19 may also be necessary.   See the EMTP case summary statistics for the current size of EMTP list',/, &
       5x, 'number 19.   The user is reminded that the existing list-space can be allocated either in absolute terms (using',/, &
       5x, "the  'absolute tacs dimensions'  request) or in relative terms (using the  'relative tacs dimensions'  request).  In",/, &
       5x, "the 'absolute' case, the 'present figure' for list 19 will show the total working space which is required for the",/, &
       5x, "user-requested dimensions.   for the 'relative' case, the 'present figure' will simply equal the limit. ")
  write (lunit6, 3322)
3322 format ( /, 5x, 'The aforementioned directory of tacs table numbers, indicating upon what the table lengths depend, reads as follows....',/, &
       8x,  '1.  Number of tacs dynamic function blocks, having Laplace transfer functions  H(s) . ')
  write (lunit6, 7422)
7422 format (8x,  '2.  Number of zero-th order tacs function blocks (type code 99 punched in cols. 1-2).',/, &
       8x,  '3.  Total number of input variables to tacs dynamic and zero-th order blocks.',/, &
       8x,  '4.  Number of signal sources to tacs (type codes 1, 2, 90, 91, 92, or 93).',/, &
       12x, "Remember that in addition to user-defined tacs sources, there are internally-defined ones ( 'timex'  and",/, &
       12x, "'unity' ,   as of january, 1977). ")
  write (lunit6, 7522)
7522 format (8x, '5.  Number of tacs supplemental variables and devices (type codes 99, 98, or 88).',/, &
       8x,  '6.  Total number of arguments describing the supplemental variables proper (not the devices).',/, &
       8x, '7.  Total number of numerical arguments (not alphanumeric) of supplemental variables (not devices). ')
  write (lunit6, 7622)
7622 format (8x,  '8.  Number of tacs supplemental devices proper (type codes 50, 51, ... 58).',/, &
       8x,  '9.  Total number of signed input variables to supplemental devices.',/, &
       7x, '10.  Total number of transport delay history terms, digitizer levels, and 2*points for nonlinear devices. ')
  write (lunit6, 2222)
2222 format (7x,  '11.  The sum over all tacs dynamic function blocks of the block order + one --- the sum of (n+1).',/, &
       7x, '12.  Total number of dynamic and zero-th order blocks (may be less than the sum of table numbers 1 and 2).',/, &
       7x, '13.  Number of distinct  a6  names used in the tacs data representation. ')
  write (lunit6, 2322)
2322 format (7x,  '14.  Number of nonzero factors of the triangularized matrix (steady-state or transients).',/, &
       7x, '15.  Number of tacs output variables (as requested by the user before tacs initial condition cards).',/,1x)
  if ( lstat(17)  .eq.  15 )
1 write (lunit6, 2422)
2422 format (12x, "Actually, the limit for tacs table 15 is one less than was indicated above, if the user's problem",/, &
       12x, "possesses an electric network.   The extra output-identification name  'tacs  '  is internally added to",/, &
       12x, 'the output vector by the EMTP, in this case.   More importantly, if the user asked for the output of all tacs variables,',/, &
       12x, 'he just shot himself in the foot.  This will never work, due to astronomical csp spy demands.  Use selective output. ')
  write (lunit6, 7822)   (i, i=1, 15)
7822 format (5x, 'Ok, so just as with overall EMTP variable dimensioning, the user can intelligently allocate storage among the',/, &
       5x, 'different tacs tables only if he knows the multiplicity of each.   For the tacs table numbers just delineated,',/, &
       5x, 'one has the following multiplicities ....',/, &
       10x, 'Tacs table number.',  10x,  15i5, /, &
       10x,  'alphanumeric multiplicity.',  2x,  '    5    3    1    1    1    1    0    2    1    0    0    0    2    0    1 ')
  write (lunit6, 7922)
7922 format(10x, 'floating-point multiplicity.', '    7    3    0    5    0    0    1    3    0    1    5    3    6    2    5 ',/, &
       10x, 'integer multiplicity.',  7x,   '    7    3    1    1    3    2    0    3    1    0    0    3    0    1    1 ',/, &
       10x, 'total multiplicity.   ',  9x,   '   19    9    2    7   4    3    1    8    2    1    5    6    8    3    7 ')
  write(lunit6,2122)  (i, i=1, 15),  (lstat( i), i=1, 15)
2122 format (5x,  'Dimensioned sizes of the tacs tables which were used for the present run are as follows ....',/, &
       10x, 'tacs table number.', 10x, 15i5, /, &
       10x, 'present dimension.',   10x,   15i5)
  go to 6220
6123 write (lunit6, 7123)
7123 format (' unused.')
6124 write (lunit6, 7124)
7124 format (5x, "Preceding all tacs data cards, the user inputted a special request card which was punched with the text  'tacs",/, &
       5x, "EMTP sources'  in columns 1-17.   Now, columns 21-80 of this card are to be read by the EMTP using  10a6  format,",/, &
       5x, 'in order to discover which type 1 through 10 EMTP sources the user wants to have controlled by specified tacs',/, &
       5x, "variables.   Recall that if the k-th field so-read contains the nonblank  a6  text  'name' ,   then on the ")
  write (lunit6, 7224)  lstat(14), bus1
7224 format (5x, "electrical side it will be the EMTP source of type-code  'k'  which will be given the numerical value of tacs",/, &
       5x, "variable  'name' .   Now, the user's  'tacs EMTP sources' card is in error because field number", i3,  '  was punched',/, &
       5x, 'with the name  ', "'", a6,  "'", ' ,   which does not correspond to any tacs variable.   Maybe this name was misspelled,',/, &
       5x, 'the EMTP wonders.   In any case, since tacs cannot supply a necessary interface request, execution will be stopped. ')
  go to 7421
6125 n1 = 90
  write (lunit6, 7125)  n1, bus1
7125 format (5x,  "As part of the user's tacs data which has now been completely read by the EMTP, there was a type-", i2,  ' tacs',/, &
       5x,  'source card which bore the 6-character alphanumeric name ', "'", a6, "'", '  in columns 3-8.   Now, by definition, this',/, &
       5x, "field must be punched with a node name (a6 format) of the EMTP electrical network which is a part of this 'hybrid'",/, &
       5x,  'data case. ')
  write (lunit6, 7225)
7225 format ('+',   17x,  'but no branch or switch card of the subsequently-inputted electrical network defined this  a6  node',/, &
       5x, 'name.   Since the EMTP does not know what variable of the electrical network should be used to control this type-90',/, &
       5x, 'source, execution of the data case will now be stopped. ')
  go to 7421
6126 n1 = 91
  write (lunit6, 7125)  n1, bus1
  write (lunit6, 7226)
7226 format ('+',  17x,  'aalso, this EMTP electrical-network node must have a switch connected to it, since it is the',/, &
       5x, 'current in the first (in order of data input) such adjacent switch which is to control the type-91 tacs source.',/, &
       5x, 'But no such switch can be found by the EMTP.   Since the EMTP does not know what variable of the electrical',/, &
       5x, 'network should be used to control this type-91 tacs source, execution must be stopped. ')
  go to 7421
6127 write (lunit6, 7127)  lstat(14), bus1
7127 format (5x, 'Has part of the input of tacs data which is now complete, the user has elected to manually define initial',/, &
       5x, 'conditions for one or more tacs variables.   Recall that such data follows the blank card which terminates tacs',/, &
       5x, 'output-variable specification cards, with one tacs variable name and associated initial condition on each',/, &
       5x, 'card.   Now, of such specifications, number', i4, '  in order of input is for a tacs variable which is',/,&
       5x, 'purported to have the 6-character alphanumeric name  ', "'", a6, "'", ' .   But no such tacs variable has been ')
  write (lunit6, 7227)
7227 format (5x, 'previously defined.   Rather than allow the solution to continue with initial conditions which are probably',/, &
       5x, 'incorrect, execution will now be terminated. ')
  go to 7421
6128 write (lunit6, 7128)
7128 format (5x, 'During triangularization of the real coefficient matrix  (a)  which is used by tacs either for dc initial',/, &
       5x, 'conditions or for the repeat solution of the time-step loop, an indication of singularity or near-singularity has ')
  write (lunit6, 7228)  lstat(14), bus1, flstat(14)
7228 format (5x, 'been observed.   Specifically, the trouble has arisen while eliminating to the left of the diagonal on row', i4, /,&
       5x, 'of the matrix, which corresponds to the equation that was written for the tacs block which has the variable',/, &
       5x, 'named  ', "'", a6,  "'", '  for an output.   now, the original diagonal element value was',   e14.5)
  write (lunit6, 7328)  flstat(15), epsiln
7328 format (5x,  '(sign included), while just prior to reciprocation this has diminished (in absolute value) to',  e14.5,  ' .',/,&
       5x, 'a near-zero diagonal value has thus occurred, as measured by the ratio of these two values vis-a-vis the EMTP',/, &
       5x,  "miscellaneous data parameter  'epsiln' ,   which has a value of", e13.4, ' .   most probably the user has ')
  write (lunit6, 7428)
7428 format (5x, 'made an error in a feedback loop of the tacs control circuitry which contains the aforementioned tacs block.',/, &
       5x, 'the physical meaning associated with this matrix singularity is that the control system is unstable.   Since',/, &
       5x, 'the problem as posed has no physically-meaningful solution in the steady-state, execution is being terminated here. ')
  go to 6220
6129 d1 = sqrtz ( flstat(14) )
  d2 = sqrtz ( flstat(15) )
  d3 = sqrtz ( tolmat )
  write (lunit6, 7129)
7129 format (5x, 'During triangularization of the complex coefficient matrix  (c)  which is used by tacs to find ac sinusoidal',/, &
       5x, 'steady-state initial conditions, an indication of matrix singularity (non-invertibility) or near-singularity has ')
  write (lunit6, 7228)  lstat(14), bus1, d1
  write (lunit6, 7329)  d2, d3
7329 format (5x,  '(in magnitude), while just prior to reciprocation, this has diminished in magnitude to', e14.5,  ' .',/,&
       5x, 'a near-zero diagonal value has thus occurred, as measured by the ratio of these two values vis-a-vis the EMTP',/, &
       5x,  "miscellaneous data parameter  'tolmat' ,   which has a value of", e13.4, ' .   most probably the user has ')
  write (lunit6, 7428)
  go to 6220
6130 write (lunit6, 7130)  bus2
7130 format (5x, "the EMTP is in the process of inputting the user's tacs supplemental-variable data cards, with the last-read",/, &
       5x, 'such data card being in error.   Specifically, the user has attempted to define a supplemental variable having',/, &
       5x, '6-character name  ', "'", a6,  "'", '  (read from cols. 3-8 of the card) ')
  write (lunit6, 7230)  bus1
7230 format ('+', 68x, 'for which one of the algebraic operator codes',/, &
          5x, 'is illegal according to tacs rules.   One of the  a1  fields in which specification of the algebraic operators',/, &
          5x, 'is to be punched was read as  ', "'", a1,  "'", ' ,   which is an unrecognized character for this usage.   The user',/, &
          5x, 'is advised to study the tacs rules related to the construction of supplemental-variable cards, and then look',/, &
          5x, 'closely at the last-read data card, to see precisely what the EMTP objects to. ')
  go to 6220
6131 n1 = lstat(14)
  write (lunit6, 7131)
7131 format (5x, 'The problem under consideration includes tacs data, the tacs function blocks of which have already all been',/, &
          5x, 'read by the EMTP.   Columns 69-80 of the leading data card for each function block are read using  2a6  format,',/, &
          5x, 'in order to determine which tacs variables (if any....if the fields are nonblank) are to be used as variable',/, &
          5x, 'limits for the block.   Now, on this basis the EMTP takes exception to the data card which defines the function ')
  write (lunit6, 7231)  bus1
7231 format (5x, 'block with name  ', "'", a6,  "'", '  (as read from columns 3-8).   Columns         are for the     -limit name, ')
  if ( n1  .eq.  1 )
1 write (lunit6, 7331)
7331 format ('+', 70x,  '69-74',15x, 'low')
  if ( n1  .eq.  2 ) write (lunit6, 7431)
7431 format ('+', 70x, '75-80', 14x, 'high')
  write (lunit6, 7531)  bus6
7531 format (5x, 'with  ', "'", a6, "'", '  read therefrom by the EMTP.   This limit variable is unknown to the EMTP, not being',/, &
          5x, 'recognized as any valid tacs variable name.   Did the user make a spelling error, the EMTP wonders.   Since the',/, &
          5x, 'EMTP is uncertain as to what the user wants done with this limit, execution of this data case is being terminated. ')
  go to 7421
6132 write (lunit6, 7130)  bus2
  write (lunit6, 7132)  bus1
7132 format ('+', 68x, 'for which one of the function codes is invalid.',/, &
          5x,  'one of the  a5  fields in which the functions are to be specified was read as  ', "'", a5,  "'", ' ,   which is an',/, &
          5x, 'unrecognizable name for a supplemental variable function.      Did the user make a spelling error, the EMTP wonders. ')
  go to 6220
6133 write (lunit6, 7130)   bus1
  write (lunit6, 7133)
7133 format ('+', 68x, 'for which no operator, function, or argument has',/, &
          5x, 'been defined.   The EMTP cannot calculate the output for a tacs block whose input is unknown. ')
  go to 6220
6134 write (lunit6, 7130)  bus1
  write (lunit6, 7134)  lstat(14)
7134 format ('+', 68x, 'for which the device code is invalid.   A value',/, &
          5x, 'of',  i4, '  was read from columns 9-10 of the card, which is not a legal tacs supplemental device code number. ')
  go to 6220
6135 write (lunit6, 7130)   bus1
  write (lunit6, 7236)
  go to 6220
6136 write (lunit6, 7136)   bus1
7136 format (5x, 'The user has been inputting tacs data, with the last-read data card being a request for a function block.',/, &
          5x,  'This was to be given (output) variable name  ', "'", a6, "'", ' ,   as read from columns 3-8 of the last-read data card. ')
  write (lunit6, 7236)
7236 format (5x, 'Yet none of the five available data fields which define the inputs to this block have been used (all associated',/, &
          5x, 'data fields are blank).   These are for alphanumeric names, read from columns 12-17, 20-25, 28-33, 36-41, and 44-49',/, &
          5x, 'using  a6  formats.   The EMTP can not calculate the output of a block for which the input is a mystery. ')
  go to 6220
6137 write (lunit6, 7137)  bus1, lstat(14), bus2, bus3
7137 format ( 5x, 'The EMTP has been inputting tacs function blocks, with the last-read data card representing an illegal such',/, &
          5x,  'request.   This was to have been the lead card of a function block having (output) name  ', "'", a6,  "'", '  (cols. 3-8).',/, &
          5x,  'input field number', i3,  '  to this block has been punched with tacs name  ', "'", a6,  "'", ' ,   but is not immediately',/, &
          5x, 'preceded by either a plus sign or a minus sign.   Rather, the character  ', "'", a1, "'", '  was read.   Remember, ')
  write (lunit6, 7237)
7237 format (5x, 'each non-blank tacs input-name field ( a6  information, columns 12-17, 20-25, 28-33, 36-41, 44-49)  must be',/, &
          5x, 'immediately preceded by either a plus sign or a minus sign ( a1  information, columns 11, 19, 27, 35, 43), indicating',/, &
          5x,  'the polarity to be applied to the associated input variable. ')
  go to 6220
6138 write (lunit6, 7138)  lstat(14)
7138 format (5x, 'The last-read data card has been taken by the EMTP to be a card specifying which tacs variables are to be',/, &
          5x, 'placed in the EMTP output vector (and hence will be available for printing and/or plotting purposes).   Yet the  i2',/, &
          5x, 'field of columns 1-2 contains an illegal integer value  ', "'", i2,  "'", ' .   Only values of zero (for selective output)',/, &
          5x,  'or unity (for output of all tacs variables) are allowed by the EMTP. ')
  go to 6220
6139 write (lunit6, 7130)  bus3
  n1 = lstat(14)
  go to  (7139, 7239, 7339), n1
7139 write (lunit6, 7439)  bus1
7439 format (5x,  'One of the arguments has been read as  ', "'", a6, "'",  ' ,   for which the user has failed to define an',/, &
          5x,  'algebraic or a logical operator. ')
  go to 6220
7239 write (lunit6, 7539)  flstat(16)
7539 format (5x,  'One of the arguments has been read as the floating-point number',   e14.6,   ' ,   for which the user has',/, &
          5x,  'failed to define an algebraic or a logical operator.')
  go to 6220
7339 write (lunit6, 7639)  bus2
7639 format (5x,  'One of the operators has been read as the alphanumeric text  ', "'", a1, "'", ' ,   for which the user has failed',/, &
          5x,  'to define a function and/or an argument. ')
  go to 6220
6140 write (lunit6, 7140)
7140 format (5x, 'Were it not for the sharp eye and always helpful good common sense of the EMTP, this simulation might have',/, &
          5x, 'continued.   But there are no requests for tacs variables to be placed in the output vector, nor is the EMTP',/, &
          5x, 'expecting any electrical network data to follow, as part of this data case.   Recall that one or more nonblank',/, &
          5x, "fields on either the  'tacs outputs'  or the  'tacs EMTP sources'  card is required for EMTP electrical network ")
  write (lunit6, 7240)
7240 format (5x, 'data to follow the tacs data, as part of a hybrid data case.   There would thus be no output vector, and if',/, &
          5x, 'the simulation were allowed to continue, results would go unobserved.   Possibly the user wanted this situation,',/, &
          5x, 'but the EMTP will not allow it.   For a data case to be allowed to enter the time-step loop, there must have been',/, &
          5x, 'requested one or more output variables (either a tacs variable, or an electrical-network variable). ')
  go to 6220
6141 write (lunit6, 7141)  nenerg
7141 format (5x, "The key word  'statistics'  or  'systematic'  can be punched in columns 55-64 of a switch card, as part of a",/, &
          5x, "data case which has integer miscellaneous data parameter 'nenerg'  punched nonzero.   But these must be",/, &
          5x,  'coordinated as follows .....',/, & 12x,  "positive  'nenerg'  -------  use only  'statistics'  ",/, &
          12x, "negative  'nenerg'  -------  use only  'systematic'",/, &
          12x, "    zero  'nenerg'  -------  use neither one.",/, &
          5x, 'the last-read data card is a switch card which violates this rule.   a value of', i6,  "   was read for  'nenerg' , ")
  write (lunit6, 7241)
7241 format (5x, 'the user is reminded.   Hence this data case either mixes the two key words of columns 55-64, or the sign on',/, &
          5x, "parameter  'nenerg'  does not correspond to the single key word which is being used. ")
  go to 6220
6142 write( lunit6, 7142 )  kill
7142 format( 5x, 'Unused kill code number....', i5)
  go to 6220
6143 write (lunit6, 7143)   bus2
7143 format (5x, 104hthe tacs data which has now all been inputted is i
1 ncomplete.   specifically, there is a problem which is       ,/,
2 5 x,  93hassociated with the tacs supplemental variable or device
3 which was given the (output) name  ', a6,  6h'  (as          ,/,
4 5 x, 109hread from columns 3-8 of the data card which defined this
5 variable).   the difficulty here is associated with         ,/,
6 5 x, 111hone of the arguments or inputs to this supplemental varia
7 ble or device.   this problem argument or input, which         )
  write (lunit6, 7243)  bus1
7243 format (5x,  46hwas identified by the 6-character  a6  name  ',
1 a6,  46h' ,   is undefined.   this name is neither the       ,/,
2 5 x, 110houtput of a tacs dynamic or zero-th order function block,
3   nor is it a tacs source name.   the name in question        ,/,
4 5 x, 108hdoes not identify any other supplemental variable or devi
5   ce, either, at least not any which has been defined          ,/,
6 5 x, 109hbefore the appearance of the problem supplemental variabl
7   e or device (as required by tacs rules on ordering).         )
    write (lunit6, 7343)
7343 format (5x,  53hdid the user make a spelling error, the EMTP wonde
1   rs.          )
    go to 7421
6144 n1 = 93
    write (lunit6, 7125)  n1, bus1
    write (lunit6, 7144)
7144 format ( 1h+,      17x,      91halso, this EMTP electrical-network
1   node must have a switch connected to it, since it is the      ,/,
2 5 x, 108hstatus of the first (in order of EMTP data input) such sw
3   itch which is to be controlled by this type-93 tacs          ,/,
4 5 x, 109hsource.   but no EMTP switch adjacent to the node in ques
5   tion can be found by the EMTP.   since the EMTP does         ,/,
6 5 x, 110hnot know what variable of the electrical network should b
7   e used to control this type-93 tacs source, execution        ,/,
8 5 x,  33hmust be terminated at this point.            )
    go to 7421
6145 write (lunit6, 7145)
7145 format (5x, 104hthe EMTP data case now being inputted involves one
1   or more continuously-transposed distributed-parameter       ,/,
2 5 x, 112htransmission lines, with frequency-dependent representati
3   on of resistance  r  and  inductance  l  in one or more       ,/,
4 5 x, 115hof the modes.   in fact, the last-read data card is the m
5   iscellaneous data parameter card which precedes the point-    ,/,
6 5 x, 109hby-point definition of a pair of weighting functions.   t
7   he first 16 columns of this card are read using  2i8         )
    write (lunit6, 7245)  lstat(14), lstat(15), lstat(16)
7245 format (5x, 105hformat, to find parameters  'ntime1'  and  'ntime2
1   ' .   these give the number of points which are used to      ,/,
2 5 x,  81hdefine the two weighting functions  a1(t)  and  a2(t) ,
3   and were read as values, i8, 6h   and, i8,  2h ,            ,/,
4 5 x, 101hrespectively.   the larger of these exceeds the available
5   maximum working space, which is dimensioned, i8,  9h   cells.  )
    if ( kburro .eq. 0 )
1   write (lunit6, 7445)
7445 format (
1   5 x,         98hconcerning this latter figure, the user should b
7   e aware that the   /label/   storage of EMTP lists            )
    if ( kburro .eq. 0 )
1   write (lunit6, 7345)
7345 format (5x, 109hnumber  5  and  7  only is involved.   this limite
1   d region of memory is divided into four equal-sized arrays,    ,/,
2 5 x, 112heach having the aforestated inadequate dimension.   in or
3   der to get the user's wieghting functions into the EMTP      ,/,
4 5 x, 111h(and compacted into their final storage in the array of e
5   mtp list number 14), the sizes of EMTP list numbers  5       ,/,
6 5 x, 109hand/or  7  must be increased.   use the above explanation
7   together with known list multiplicities in order to         ,/,
8 5 x,  40hdetermine the required increase in size.           )
    if ( kburro .ne. 0 )
1   write (lunit6, 7545)
7545 format ( 5x, 39hconcerning this latter figure, the user,
1             35 h should remember that he is using a,
2             41 h virtual computer, so list 23 of "vardim"
3   ,/,  5x,  38his used for storage.  such a region of,
4             39 h memory is divided into four equal-size,
5             38 h arrays, with each of these having the
6   ,/,  5x,  35haforementioned inadequate size.  in,
7             34 h order to successfully process the,
8             40 h weighting functions, list 23 must grow.   )
    go to 6220
6146 write (lunit6, 7146)  bus1, bus2
7146 format (5x, 110hswitch cards are now being inputted, for the EMTP
1   data case under consideration.   specifically, the last-read   ,/,
2 5 x, 101hdata card represents a request for a type-11 switch (i.e.
3   , a diode or a valve) which connects node  ', a6,  1h'       ,/,
4 5 x,  12hwith node  ', a6,  3h' .    )
    n1 = kswtch - 1
    write (lunit6, 7246)  n1
7246 format ( 1h+,    28x,             87hnow, columns 61-64 were punch
1   ed with the key-word  'same' ,   which is understood to be     ,/,
2 5 x, 114ha request that the physical characteristics of the presen
3   t valve or diode be identical to those of the most-recent      ,/,
4 5 x,  56hpreceding type-11 switch element.   but of the preceding,
5   i4,  45h  switch cards, none were type-11 (punched in       ,/,
6 5 x, 104hcols. 1-2).   thus the reference switch does not exist, a
7   nd the EMTP has no way of knowing what modeling              ,/,
8 5 x,  65hparameters should be used to describe the present diode o
9   r valve.       )
    go to 6220
6147 write (lunit6, 7147)  bus5
7147 format ( 1x, 100( 1h- ),  /,
1   / 5x,  39hthis switch makes reference to the non-,
2           33 hexisting control tacs variable  ',
3   a6,   1h'  , // 1x, 100(1h-)  )
    go to 7421
6148 write (lunit6, 7148)  bus1, bus2, flstat(15), flstat(16)
7148 format ( 5x, 102hthe last-read data card is for a switched-inducta
1   nce element (type-93 switch card) which connects node        ,/,
2 5 x,   2h ', a6,   15h'  with node  ', a6,  73h' .   but the value
3   for residual flux (punched in columns 15-24, and read       ,/,
4 5 x, 110husing  e10.6  format) is illegal, for it exceeds the satu
5   ration flux (punched in columns 45-54, and read using       ,/,
6 5 x,  64he10.6  format).   the numerical values for these two numb
7   ers are,     e15.4  ,  5h  and,    e15.4 , 17h ,  respectively.  )
    write (lunit6, 7248)
7248 format (5x,  56hcorrect this violation of the EMTP rules, and try
1   again.           )
    go to 6220
6149 write (lunit6, 7149)  flstat(14)
7149 format (5x, 105hthe last-read data card has been taken by the EMTP
1   to be a source card of type 14 (sinusoidal generator).       ,/,
2 5 x, 111hbut the frequency as read from columns 21-30 of this card
3   is not positive, as required by EMTP rules.   a value       ,/,
4 5 x, 2hof,     e13.4,   88h  was read.   sinusoids of other than p
5   ositive frequency must be rejected by the EMTP on           ,/,
6 5 x,  74haesthetic grounds.   the user should correct columns 21-3
70  , and try again.         )
    go to 6220
6150 d1 = flstat(14) / deltat
    write (lunit6, 7150)  deltat, bus1, bus2
7150 format (5x, "The user has picked a time-step size  'deltat'  (read from columns 1-8 of the floating-point miscellaneous      ",/, &
         5x, 'data card) which is too small for one of the distributed parameter transmission lines that is to be modeled using      ',/, &
         5x, 'Semlyen recursive convolution.   A step-size of', e14.4,  '  seconds was requested, which is too large      ',/, &
         5x, 'for the Semlyen line having phase number 1 that connects node  ', "'", a6,   "'", ' with node  ', "'", a6, "'", '.   The')
    write (lunit6, 7250)  lstat(15), flstat(14), d1
7250 format (5x, 'rule is that the travel time for all Semlyen modes must exceed  two  time steps.   but for mode number',  i4, /, &
         5x,  "of the aforementioned line, the travel time  'tau'  is only",    e14.4,    '  seconds.   The ratio of these gives     ',/, &
         5x,  'tau/deltat =',   e12.4,   " ,   which is too small (less than 2.0).   Decrease the time-step size  'deltat'          ",/, &
         5x, 'accordingly, or alter the transmission line modeling, in order to make this data case solvable using the EMTP.          ')
    go to 6220
6220 lastov = nchain
     nchain = nfrfld + 50
     if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
4568 format ( 24h "exit  module over53." )
99999 return
   end function block
   c
   c     end of file: over53.for
   c
