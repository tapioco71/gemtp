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
7091 format (5x, "The data case now in its final stages of input is for a statistical overvoltage study, since field  'nenerg'   ",/, &
       5x, 'of columns 65-72 of the 2nd miscellaneous data card was punched with a positive number.   Yet the last-read data    ',/, &
       5x, "card requests the output of all network node voltages, by means of the  '1'  punched in column 2.   But the EMTP    ",/, &
       5x, 'does not allow this complete voltage output for statistical overvoltage studies.   If the user really wants to look    ')
  write (lunit6, 7191)
7191 format (5x, 'at every node voltage in such a study, he must request each such output individually, using the selective    ',/, &
       5x,  'node voltage output option.   ')
6092 write (lunit6, 7092)
7092 format (5x, "The EMTP logic has just attempted to read another data card by means of a call to subroutine  'cimage' .   ",/, &
       5x, 'But no more data is to be found on logical unit number 5.   An end-of-file mark has been encountered upon attempting   ',/, &
       5x, 'to read from this unit.   execution must be stopped, since data which is required for continuation of the study   ',/, &
       5x,  'simply does not exist.    ')
  go to 6220
6093 write (lunit6, 7093)
7093 format (5x, 'The last-read data card is for one phase (mode) of a distributed-parameter transmission line.   But one or more ',/, &
       5x, 'of the parameter values read therefrom makes the data illegal.   The four fields of columns 27-50 are not all   ',/, &
       5x, 'blank, so this mode data is not taken by the EMTP to be a request for a copy of the parameters of the preceding mode.   ',/, &
       5x, 'Hence columns 27-32 must be punched with mode resistance per unit length, columns 33-44 must be punched for the   ')
  write (lunit6, 7193)  flstat(14), flstat(15)
7193 format (5x, 'mode inductance and capacitance (possibly disguized, in terms of surge impedance, propagation speed, or   ',/, &
       5x, 'travel time), and finally, columns 45-50 must contain the line length.   Now one or both of the two fields which  ',/, &
       5x, "equally divide columns 33-44 is non-positive, which is illegal.   Values read from the user's card for these   ",/, &
       5x, 'two fields were',    e14.4 ,   ' and',   e14.4 , ' ,   respectively.   Remember, zero inductance or   ')
  write (lunit6, 7293)
7293 format (5x, "capacitance (the case when  'iline'  is zero)  is illegal because it implies infinite velocity of propagation.  ",/, &
       5x, 'Similar absurdities result for the other two forms of disguizing inductance and capacitance, it can readily be  ',/, &
       5x,   'seen.   ')
  go to 6220
6094 write (lunit6, 7094)  flstat(14), flstat(15)
7094 format (5x, 'The last-read data card has been taken by the t.p. to be a switch card, with closing time to be determined  ',/, &
       5x, 'randomly as part of a statistical overvoltage study.   But the mean closing time of',  e13.4, '  seconds and the     ',/, &
       5x,  'standard deviation of',  e13.4, '  seconds are such that switch closing times less than zero would not be totally     ',/, &
       5x, 'unexpected.   The mean must not be less than  3.0  times the standard deviation, if the data is to be acceptable        ')
  write (lunit6, 7194)
7194 format (5x, 'to the EMTP in the case of gaussian (normal) distribution of the switch closing times.   This ensures          ',/, &
       5x, 'that there is a probability of less than  0.13  percent for any particular switch closing time to be negative         ',/, &
       5x, "for a normal distribution.   But the user's data has failed this check, for the last-read switch.   Because the   ",/, &
       5x, "EMTP has no proper way of handling negative switch closing times (they are set to time zero, actually), the user's    ",/, &
       5x,  'data case must be rejected.      ',/,1x)
  write (lunit6, 7294)
7294 format (5x, 'If the user really did desire to use the mean and standard deviation which got him into trouble here, he is advised   ',/, &
       5x, 'of the possibility of delaying closure of all switches by exactly one (or more) fundamental-frequency cycles.     ',/, &
       5x, 'Provided one is starting from the sinusoidal steady state, the added delay of an integer number of cycles simply   ',/, &
       5x, 'delays the response accordingly, leaving it otherwise unchanged.   By adding one cycle of fundamental-frequency time   ')
  write (lunit6, 7394)
7394 format ( 5x, 'to the mean of the closing time of all switches in the data case, the aforementioned problem of negative closing',/, &
       5x,  'times is usually eliminated, and the case can be run without difficulty.    ')
  write (lunit6, 7594)
7594 format (/, 5x, "For an independent switch (fields  'bus5'  and 'bus6'  of columns 65-76 of the switch card being both      ",/, &
       5x, "blank), the mean closing time  'tbar'  is read from columns  15-24 ,   and the standard deviation  'sigma'  is        ",/, &
       5x, "read from columns  25-34 .    This is simple.   But should the  'bus5'  and  'bus6'  fields be other than both        ",/, &
       5x, "blank, then  'tbar'  and  'sigma'  depend not only on the column  15-34  numbers of the switch card for the           ")
  write (lunit6, 7694)
7694 format (5x, 'switch in question, but also on all of the other corresponding column  15-34  numbers as the dependency chain    ',/, &
       5x, "is traced back through previously-inputted switches.   In this case of a dependent switch,   'tbar'  is the sum        ",/, &
       5x, "of the column  15-24  numbers in the chain, while  'sigma'  is the square root of the sum of the squares of the       ",/, &
       5x,  'column  25-36  numbers in the chain.         ')
  go to 6220
6095 write (lunit6, 7094)  flstat(14), flstat(15)
  write (lunit6, 7095)
7095 format (5x, 'The mean must not be less than the square root of three times the standard deviation.   This condition would   ',/, &
       5x, 'guarantee that switch closing times would always be positive, for the uniform distribution which the user has   ',/, &
       5x,  "requested.   But the user's data has failed this check, for the last-read switch.   Because the   ",/, &
       5x, "EMTP has no proper way of handling negative switch closing times (they are set to time zero, actually), the user's    ",/, &
       5x,  'data case must be rejected.      ',/,1x)
  write (lunit6, 7294)
  write (lunit6, 7394)
  go to 6220
6096 write (lunit6, 7096)
7096 format (5x, 'This run is being terminated due to an attempt to plot results.   The EMTP can be variably-dimensioned,   ',/, &
       5x, 'of course, with the present version simply having no available working space in which even the plot-file header   ',/, &
       5x, 'information can be stored.   If the user wants to do plotting, he must either do it interactively on the crt, or   ',/, &
       5x, 'he must work with a program version which has more labeled-common storage.     ')
  write (lunit6, 7196)
7196 format (5x, 'More specifically, the size of the working area in this particular non-solution overlay is set by variable-    ',/, &
       5x, "dimensioning program  'vardim'  according to the relation size = szlab - offset  .       In this formula, ....   ",/, &
       10x,  "'size'  =  the number of floating-point words of working space (insufficient in this case),        ",/, &
       10x,  "'szlab'  =  the number of floating-point words used for labeled common in the solution overlays,            ")
  write (lunit6, 7296)
7296 format (10x, "'offset'  =  a fixed, non-negative offset which approximately equals the number of floating-point words    ",/, &
       23x,  'by which the compiled instructions of this overlay exceeds the corresponding figure for the longest     ',/, &
       23x, 'solution overlay (one of the secondary-level modules).',/, &
       5x, "Should  'offset'  exceed  'szlab' ,   then  'size'  is set to unity, so as to avoid compilation errors (fortran     ",/, &
       5x, "generally requires positive dimensions for all arrays).   Recall that  'szlab'  is printed out for the user every     ")
  write (lunit6, 7396)   lstat(14), lstat(15)
7396 format (5x, 'time the EMTP is executed, as part of the header information that precedes the listing of the input data.    ',/, &
          5x, "For the present case in question, the EMTP finds  'size' equal to",  i6, ' ,   while a minimum of',  i7,  '  words   ',/, &
          5x, 'are needed in order for execution to continue beyond the point where it is now being stopped.   If it is not clear    ',/, &
          5x, 'as to what remedial action should be taken, the user is advised to seek experienced counsel about this matter.     ')
  write (lunit6, 7496)
7496 format(5x, 106ha brief caution about use of a marginal working are
1 a for plotting is perhaps also in order, however.   the    ,/,
2 5x, 108haforementioned minimum figure should allow for correct pl
3 otting, but will generally produce repeated buffer-       ,/,
4 5x, 107hflushing-type spillover onto logical unit number  9  duri
5 ng the plotting of each graph, with its associated     ,/,
6 5x, 113hslowdown in execution speed.   in order to avoid all such
7 temporary disk storage,   'size'  can be set no smaller    )
  write (lunit6, 7596)
7596 format (5x, 109hthan twice the number of plot points of the partic
1 ular graph multiplied by the number of curves on the graph.    ,/,
2 5x, 111hdouble-precision conversions (ibm  real*8  is the common
3 example) can generally ignore the just-stated  'twice'   ,/,
4 5x, 110hfactor, since plotting-point storage for use with softwar
5 e plotting routines like calcomp is almost invariably    ,/,
6 5x,  17hsingle-precision.    )
  go to 6220
6097 write (lunit6, 7097)  lstat(14), lstat(15)
7097 format ( 5x,  94hthe last-read data card has an =e=-field number w
1 hich is not right-adjusted in its data field. ,/, 5x,  90hin parti
2 cular, either the letter =e=, or a sign following a numeral was fo
3 und in the field ,/, 5x,  29hwhich starts in column number , i4,
421 h, while column number , i4,  45h, which is the right-most colum
5 n of the field  ,/, 5x,  70his not punched explicitely with a deci
6 mal digit (i.e., 0, 1, .... 9 ).  ,/,
5 5x, 106hnow it may well be that such usage is indeed legal fortra
5 n data-format input for the user's own particular     ,/,
6 5x, 109hcomputer system, but it offends the keenly-honed sensibil
7 ities of the EMTP (at whose mercy the user now finds    )
  write (lunit6, 7197)
7197 format ( 5x, 106hhimself, it may be noted).   too often a user wil
1 l erroneously wind up with an exponent of ten which is in    ,/,
2 5x, 112herror by a power of ten itself, in such a case.   hence s
3 uch data is not allowed by the EMTP   repunch this data    ,/,
4 5x,  30hrecord, and try again, friend.       )
  write (lunit6, 7297 )
7297 format ( 5x, 104halternatively, a card sequence error may have res
1 ulted in an attempt to read data with the wrong format.  ,/, 5x,
2  42 hcheck for missing or out-of-sequence data. )
  go to 6220
6098 write (lunit6, 7098)  lstat(14), bus1, lstat(15)
7098 format ( 5x,  99hthe last-read data card has an =i=-field number w
1 hich is not right-adjusted in its data field.   in    ,/,
2 5x,  25hparticular, column number,  i4, 27h  contains the charact
3 er  =,  a1,   17h= ,  while column,  i3, 14h, which is the   ,/,
4 5x, 114hright-most column of the field in question, is blank.   n
5 ow maybe such usage is legal fortran input for the user=s   ,/,
6 5x, 109hcomputer system, but it offends the keenly-honed sensibil
7 ities of the EMTP (at whose mercy the user now finds    )
  write (lunit6, 7198)
7198 format ( 5x, 104hhimself, it may be noted).   too often a user wil
1 l erroneously wind up with a value which is in error by  ,/,
2 5x, 103ha power of ten, in such a case.   hence such data is simp
3 ly not allowed by the EMTP   repunch this data    ,/,
4 5x,  30hrecord, and try again, friend.       )
  go to 6220
6099 write (lunit6, 7099)
7099 format ( 5x, 106hthe EMTP has finished the calculation of weightin
1 g functions  a1(t)  and  a2(t)  for the user=s frequency-   ,/,
2 5x, 111hdependent line-mode.   now the program is ready to output
3 these functions --- on punched cards, if the user has   ,/,
4 5x, 112hgiven variable  =ipunch=  (of columns 33-40 of the second
5 data card) a value of zero.   but this is not possible   ,/,
6 5x,  83hwithin the  =f=-format fields which have been provided, w
7 ithout overflow occurring.     )
  write (lunit6, 7199)  flstat(15), flstat(16)
7199 format ( 5x, 105hspecifically, the peak weighting function values
1   can not both be punched using  f10.0  format.   the peak    ,/,
2 5x,  22hvalues in question are, /, 10x, 12hmax(a1(t)) =,
3   e14.5, 10x, 12hmax(a2(t)) =, e14.5, 2h .   ,/,
4 5x, 113hin some way, the user=s data would seem to be highly unus
5   ual, and worthy of careful scrutiny.   two special cases   ,/,
6 5x, 112hwhich will always lead to numerical trouble are perhaps w
7   orthy of mention, in this regard.   first, as the line-    )
    write (lunit6, 7299)
7299 format ( 5x, 107hlength  =dist=  (columns 1-8 of the first data ca
1   rd) approaches zero,    max(a1(t))    is known to approach    ,/,
2 5x, 110hinfinity.   similar behavior will also result for any fix
3   ed-length line, as the impulse attenuation approaches   ,/,
4 5x, 112hzero (e.g., let  r  approach zero and  l  approach a cons
5   tant value, for all frequencies).   the user is advised   ,/,
6 5x,  89hto seek experienced counsel, if he can not find an obviou
7   s massive data error on his own.     )
    go to 6220
6100 write (lunit6, 7099)
    write (lunit6, 7100)  flstat(15)
7100 format ( 5x, 104hspecifically, the weighting functions extend too
1   far out in time, with duration in excess of the  100000     ,/,
2 5x, 107hmicroseconds which can be punched legally using  f10.4  f
3   ormat.   with light traveling at a rate of  300 km   ,/,
4 5x, 111h(186 miles)  per millisecond, this is absurd for a power
5   line, and must be rejected.   electrically, the user=s    ,/,
6 5x, 110hline is approximately one order of magnitude longer than
7   anything which is typical for american power systems. ,/,
8 5x,  49hthe user's weighting functions have a duration of,
9   e13.4,  15h  microseconds.   )
    write (lunit6, 7200)
7200 format ( 5x, 107hthe user is reminded that even if he has a very l
1   ong line of say  1000  miles, he is advised to break it up   ,/,
2 5x, 110hinto maybe five identical segments of  200  miles each.
3   then he derives weighting functions for one 200-mile   ,/,
4 5x, 111hsegment, and uses the reference-branch capability in sett
5   ing up the EMTP data case for the network in question,    ,/,
6 5x,   94hthereby saving  4/5  on the otherwise-required weighting
7   -function storage of list number  14 .     )
      go to 6220
6101  write (lunit6, 7101)
7101  format ( 5x, 103hthe decision to kill this run has been made withi
1     n the dummy (i.e., the original, the empty) subroutine    ,/,
2 5   x, 110h 'analyt'  which is called by subroutine  'subts3'  of ov
3     erlay 16 (overlay 18 when using ecs overlaying of the   ,/,
4 5   x, 110htime-step loop on cdc).   the user declared his intent to
5     use analytically-defined sources at the beginning of   ,/,
6 5   x, 110hthis data case by inputting the special  'analytic source
7     s'  request card.   but he has failed to have program    )
      write (lunit6, 7201)
7201  format ( 5x, 108hmaintenance replace the original, dummy module  '
1     analyt'  by the one which he should have written especially   ,/,
2 5   x, 110hfor this data case.   the user needs a module which defin
3     es his own special sources --- one or more sources of   ,/,
4 5   x,  95htype  1  through  10 ,   defined in fortran (or machine l
5     anguage) within subroutine  'analyt' .    )
      go to 6220
6102  write (lunit6, 7102)  lstat(15), bus1, bus2, flstat(14)
7102  format ( 5x,    20hlinear branch number, i4,  42h  (in order of da
1     ta input) connects bus  ', a6, 14h'  with bus  ', a6,  3h' .   ,/,
2 5   x, 112hdata values on this card are for a mode of the distribute
3     d-parameter line which is being modeled with resistance   ,/,
4 5   x, 114hlumped, one half in the middle, and one quarter at each e
5     ach end (field  'ipunch'  of columns 53-54 equal to -1 ).    ,/,
6 5   x,  38hbut the total modal line resistance of,    e15.5,   57h
7     ohms is unreasonably large when compared with the surge    )
      write (lunit6, 7202)  flstat(15)
7202  format ( 5x,  29h(characteristic) impedance of,     e15.5,   57h
1     ohms.   program logic can not handle a resistance which   ,/,
2 5   x, 108hexceeds four times the characteristic impedance.   for no
3     rmal power lines, such values are absurd anyway, so     ,/,
4 5   x,  42hthe user gets little sympathy on this one.    )
      go to 6220
6103  if(lstat(12).gt.0)write(lunit6,7103)lstat(12)
7103  format(5x, 50hpossible loss of significance in elimination step ,1
1     x,i4,1x,19hin subroutine dgelg,/)
        go to 6220
6104    write (lunit6, 7104)  iprsov(37), lstat(12)
7104    format (5x, 105hthe data case now being solved involves one or mor
1       e dynamic synchronous machine (s.m.) source components.      ,/,
2 5     x, 109hthe associated equations (park's or blondel's) are nonlin
3       ear, and must be solved by an iterative procedure at         ,/,
4 5     x,  85heach time step.   this iteration has failed to converge w
5       ithin the iteration limit of,  i5, 16h ,   for dynamic       ,/,
6 5     x,  11hs.m. number,  i5, 83h  (in order of input).   the iterati
7       on limit is variable  'niomax' ,   which can be              )
        write (lunit6, 7204)  bus1, epomeg, flstat(14)
7204    format (5x, 110hre-defined during the s.m. data input, using a  't
1       olerances'  special-request card.   the s.m. in question has   ,/,
2 5     x,  29hphase 'a' connected to bus  ',  a6,  72h' .   the unachie
3       ved tolerance is variable  'epomeg' ,   which has value      ,/,
4 5     x,      e14.3,    64h .   the fractional speed discrepancy on th
5       e final iteration was,    e16.7,      13h .   assuming       ,/,
6 5     x, 111hthat  'niomax'  is reasonable (say five or larger), then
7       the effect of two remaining variables might profitably       )
        write (lunit6, 7304)  deltat
7304    format (5x, 104hbe contemplated.   first, the user should check th
1       at his time-step size  'deltat'  is not too big, so as       ,/,
2 5     x, 113hto insure that the change in speed between successive tim
3       e steps will be small.   this data case used  'deltat' =     ,/,
4 5     x,    e14.3,    100h .   second and more subtle is the choice of
5       tolerance  'epomeg' ,   which should not be set smaller     ,/,
6 5     x,  84hthan the roundoff limitation which applies to the iterati
7       ve correction of the speed.        )
        go to 6220
6105    write (lunit6, 7105)  lstat(14)
7105    format (5x, 105hthe last-read data card is the first card of class
1       -2 data for a dynamic synchronous machine (s.m.) source      ,/,
2 5     x,  99hcomponent.   variable  'numas'  as read from columns 1-2
3       of this card (using  i2  format) has value,  i5,  2h .       ,/,
4 5     x, 109hthis is supposed to be the number of masses which make up
5       the shaft system of the rotor of the machine.   but         ,/,
6 5     x,  29hthis figure is non-positive .)
        go to 6220
6106    d1 = 1.0 / fltinf
        write(lunit6,7106)  bus1
7106    format(5x,  85herroneous data discovered during the initialization
1       of multiple s.m. connected to bus, 1h', a6, 16h'. the specified,
2       /, 5x,   86hvalues of parameters 'smoutp' and/or 'smoutq' for the
3       involved s.m.'s add up to zero., /,
4 5     x, 107hrecall that any blank field used in the above data is rep
5       laced by the near-zero EMTP tolerance 1.0/fltinf =)
        write (lunit6, 7206)  d1
7206    format (5x,   e14.3,        91h .   the user should never use the
1       negative of this value for any of the four parameters in     ,/,
2 5     x, 109hquestion.   also, he must not punch one of the  'smoutp'
3       fields so as to be the negative of the other.   the         ,/,
4 5     x,  54hsame prohibition applies to the two  'smoutq'  values.  )
        go to 6220
6107    write(lunit6,7107)
7107    format(5x,49h fractional mass torque parameters do not conform,
1 28    h to rules -- see user manual,/)
        go to 6220
6108    write (lunit6, 7108)
7108    format (5x, 110hthe last-read data card is taken by the EMTP to be
1       a request for an auxiliary synchronous machine (s.m.) input   ,/,
2 5     x, 112hto tacs.   recall that such data cards (if any) complete
3       the data input for a dynamic s.m., after being followed      ,/,
4 5     x, 112hby a terminator record which bears the text  'finish'  in
5       columns 3-8.   but the last-read data card is illegal,       )
        if ( lstat(16)  .le.  lstat(15) )   go to 7308
        write (lunit6, 7208)  lstat(15)
7208    format (5x, 106hsince storage for such s.m.-tacs interface variabl
1       es has been overflowed.   the existing program dimension      ,/,
2 5     x,  2his,  i4,  100h ,   which is equal to the number of previou
3       sly-defined interface variables in arrays  'etac  '  and     ,/,
4 5     x,  10h'ismtac' .   )
        go to 6220
7308    if( lstat( 17 ) .gt. 0 )  go to 7508
        write (lunit6, 7408)   bus6
7408    format (5x, 16hsince the name ', a6, 24h' of the interface varia,
1 58    hble (as read from columns 3-8 using a6 format) is invalid., /,
2 5     x, 56hall such names must be valid, for they serve as names of,
3 43    h tacs variables (for type-92 tacs sources). )
        go to 6220
7508    write (lunit6, 7608)  lstat( 14 ),  lstat( 13 )
7608    format (5x, 46hsince the data read from columns 1-2 using  i2,
1 59    h format and from columns 15-17 using i3 format is invalid .,
2       /, 5x, 24hthe read values were...., i4, i5 )
        go to 6220
6109    write( lunit6, 7109 )   kill
7109    format( 5x, 27hunused kill code number...., i5  )
        go to 6220
6110    write (lunit6, 7110)  nenerg, bus5, bus6
7110    format (5x,  86hparameter  'nenerg'  of the integer miscellaneous
1       data card was punched nonzero (value, i5, 14h  )   for this    ,/,
2 5     x, 112hdata case, representing a request for a  'statistics'  or
3       a  'systematic'  simulation.   the last-read data card      ,/,
4 5     x, 108his for an EMTP switch, and it bears one of the two just-m
5       entioned key words in columns 55-64, which is fine.          ,/,
6 5     x,  63hbut this is a dependent switch, with illegal reference na
7       mes  ', a6,  9h'  and  ', a6,  19h'  punched for data        )
        write (lunit6, 7210)  lstat(15), lstat(16)
7210    format (5x, 108hfields  'bus5'  and  'bus6'  (columns 65-76).   th
1       e ordered pair of reference names corresponds to EMTP node     ,/,
2 5     x,   7hnumbers, i5,  5h  and,  i5, 77h ,   respectively, with a
3       zero value meaning that the associated  a6  name is          ,/,
4 5     x, 105hillegal (unrecognizable as a legitimate EMTP electrical n
5       etwork node name).   if both of the just-printed             ,/,
6 5     x, 112hintegers are positive, then both names are legitimate emt
7       p network names, but do not correspond to a previously-        )
        write (lunit6, 7310)
7310    format (5x, 108hinputted switch.   remember that ordering of the p
1       air of reference names is crucial, with it being mandatory     ,/,
2 5     x, 113hthat the first reference name  'bus5'  match a first swit
3       ch name  'bus1' ,   etc. for  'bus6'  matching  'bus2' .      )
        go to 6220
6111    write (lunit6, 7111)   lstat(13), lstat(15), lstat(16), lstat(17)
7111    format (5x, 107hone of the working vectors which are used to assem
1       ble output quantities for all dynamic synchronous machine     ,/,
2 5     x, 104hsource components has overflowed.   the dimensioned limit
3       for the number of  a6  identification names is,  i6,  2h ,  ,/,
4 5     x,  98has is the maximum allowable number of output quantities.
5       but after finishing with machine number,  i5,  2h ,        ,/,
6 5     x,  37hthe two corresponding table sizes are,  i6,  6h   and,
7       i6,  47h ,   respectively.   one or both of these table      ,/,
8 5     x, 116hsizes has now been exceeded.   execution will be stopped.
9       either request fewer outputs, or redimension the tables.    )
        go to 6220
6112    write (lunit6, 7112)  kswtch
7112    format (5x, 108hwow.   double wow (wow, wow).   the EMTP has finis
1       hed with the input of all switch cards for this data case.     ,/,
2 5     x,   6hof the, i5,  96h  switches, over ten had the key word  's
3       tatistics'  or  'systematic'  punched in columns 55-64.      ,/,
4 5     x, 112hin fact, there are over ten independent  'statistics'  or
5       'systematic'  switches  ----  distinguished by either       ,/,
6 5     x, 107hfield  'bus5'  (cols. 65-70)  punched with  'target' ,
7       or both fields  'bus5'  and  'bus6'  (cols. 65-76)           )
        write (lunit6, 7212)  lstat(14), bus1, bus2
7212    format (5x,  81hleft blank.   the EMTP limit is ten, however, whic
1       h was exceeded by switch number, i5,  19h ,   which connects   ,/,
2 5     x,   7hnode  ', a6,  15h'  with node  ', a6,  62h' .   the EMTP
3       wonders whether the user really appreciates the              ,/,
4 5     x, 106hsignificance of ten independent switches, or equivalently
5       , the vastness of a ten-dimensional vector space.            ,/,
6 5     x, 109hremember, even if one quantized each dimension into only
7       two discrete values, ten independent variables would         )
        write (lunit6, 7312)
7312    format (5x, 108hyield   2**10 = 1024   compartments or cells.   wi
1       th three discrete values, the figure becomes  59049.   and     ,/,
2 5     x, 113hwith four, it jumps to the staggering figure of  1048576.
3       the user's computer system may be fast, but not swift     ,/,
4 5     x, 116henough to even begin to realistically sample a ten-dimens
5       ional vector space in a representative fashion.   execution    ,/,
6 5     x,  52hwill not be allowed to continue, for this data case.   )
        go to 6220
6113    n2 = lstat(13)
        write (lunit6, 7113)  nenerg
7113    format (5x, 107hthe data case now being processed is for a  'syste
1       matic'  study, since integer miscellaneous data parameter     ,/,
2 5     x,  76h 'nenerg'  was punched as a negative number.   using  i8
3       format, a value of, i6,  25h   was read for  'nenerg'       ,/,
4 5     x, 109hfrom columns 65-72.   by definition, this is supposed to
5       be the negative of the number of energizations which         ,/,
6 5     x, 108hare to be run.   of course this figure is determined by m
7       ultiplying together the number of steps to be taken          )
        write (lunit6, 7213)  lstat(12), kswtch
7213    format (5x, 100hby each independent  'systematic'  switch.   but w
1       hen the EMTP multiplies together all such integers    ,/,
2 5     x, 112hwhich were read using  i10  format from columns 35-44 of
3       'systematic'  switch cards, it finds the product to be       ,/,
4 5     x,   8hequal to, i6,  90h .    this value is inconsistent with t
5       he aforementioned  'nenerg' .    to aid the user in         ,/,
6 5     x,  68hcorrecting this discrepancy, it is mentioned that this da
7       ta case has, i5,  30h  switches.   in order of data          )
        write (lunit6, 7313)  numref,  ( ipntv(i), i=1, numref )
7313    format (5x,  33hinput, the integers read from the, i4,  61h  indep
1       endent  'systematic'  switch cards are as follows ....   ,/,
2       ( 1x, 12i10 ) )
        do 7413  i=1, numref
           if ( ipntv(i)  .le.  0 )   go to 7513
7413       continue
           go to 6220
7513       write (lunit6, 7613)  i, ipntv(i)
7613       format (5x,  71hupon inspecting these figures, the EMTP immediatel
1          y notices that number, i4,  26h  is illegal.   a value of     ,/,
2 5        x, i6, 114h   is seen, which certainly is not positive (as per t
3          he user's manual instructions).   remember, any  'systematic'  ,/,
4 5        x, 112hswitch card which has either field  'bus5'  (cols. 65-70)
5          punched with  'target' ,   or both fields  'bus5'  and      ,/,
6 5        x, 106h'bus6'  (cols. 65-76)  left blank, is an independent swit
7          ch.   columns 35-44 of such a switch must then be            )
           write (lunit6, 7713)
7713       format (5x,  95hpunched with the number of steps (necessarily posi
1          tive) which are to be taken with this switch.           )
           go to 6220
6114       write (lunit6, 7114)
7114       format (  8h unused.  )
6115       write (lunit6, 7115)  lstat(14), bus1
7115       format (5x, 104hthe user has been inputting 'tacs' data, specifica
1          lly cards which define tacs function blocks.   yet the   ,/,
2 5          x,  98hlast-read data card has an illegal integer  'n'  punched
3            in the field of columns 1-2.   a value of, i4,  5h  was    ,/,
4 5          x,  53hread, for this block having (output) variable name  ',
5            a6,  46h' .   since this integer does not equal one of   ,/,
6 5          x, 114hthe special code numbers which are used to indicate speci
7            al block types (e.g., n=99 for a zero-th order block), it    )
             write (lunit6, 7215)
7215         format (5x,106his taken by the EMTP to be the order of the laplace
1            transfer function  h(s)  of the block.   but this must   ,/,
2 5            x, 111hbe positive.   also, a limit of 10-th order has been arbi
3              trarily imposed by the EMTP.   if the user really does   ,/,
4 5            x, 111hhave a higher-order  h(s)  than 10 (highly unlikely), he
5              must simply split his original  h(s)  into two or more   ,/,
6 5            x,  60hcascaded sub-blocks, each of which is of order  10  or le
7              ss.     )
               go to 6220
6116           write (lunit6, 7116)  bus1, lstat(14)
7116           format (5x, 107hthe EMTP data case under consideration includes a
1              tacs representation, the function blocks of which are now     ,/,
2 5              x,  99hbeing inputted.   now, the last tacs function block which
3                  was read by the EMTP had (output) name  ', a6,   1h'        ,/,
4 5                x,  83h(punched in columns 3-8), and was purported to be a dynam
5                  ic function block of order, i3,  21h  (punched in columns     ,/,
6 5                  x, 109h1-2).   the tacs function block is characterized by a lap
7                      lace transfer function  h(s) ,   which is a rational         )
                         if ( lstat( 17) .eq. 1 )  go to 16116
                         write (lunit6, 7216)  lstat(14)
7216                     format (5x, 106hfunction (ratio of polynomials) of the complex fre
1                        quency variable  's' .   by definition, the order of the     ,/,
2 5                      x, 111htacs block refers to the order of the highest polynomial
3                        involved (either numerator or denominator).   but both       ,/,
4 5                      x,  71hthe numerator and denominator coefficients of  's'  raise
5                        d to the power, i3,  31h  are zero.   the highest power      ,/,
6 5                      x, 110hof  's'  is thus missing, completely.   the block order i
7                        s inconsistent with the polynomial coefficients which      ,/,
8 5                      x,  78hare supposed to define the block's  h(s) ,   so execution
9                        is being terminated.       )
                         go to 6220
16116                    write (lunit6, 7316)
7316                     format (5x,  34hfunction (ratio of polynomials) of,
1             37         h the complex frequency variable 's' .,
2                        / 5x,    33hhere, the order-zero coefficients,
3             31         h (of the terms without 's')  in,
4             31         h both numerator and denominator,
5                        / 5x,    31hwere read as  zero .           ,
6                        / 5x,    30hdid the user really mean this,,
7             30         h the program wonders.   don't. )
                         go to 6220
6117                     n9 = 50
                         write (lunit6, 7117)   n9, ipunch
7117                     format (5x, 108hthe last-read data card is the first branch card f
1                        or a multi-phase line which is to be modeled using Semlyen     ,/,
2 5                      x, 108hor ametani recursive convolution for the frequency-depend
3                        ent representation.   such modeling is limited to a          ,/,
4 5                      x,  10hmaximum of, i4, 93h   modes (coupled phases).   but this
5                        limit is exceeded by the user's requested number, which      ,/,
6 5                      x,  14hwas punched as,  i5,  85h   (read from columns 77-78).
7                        a major programming effort (involving some unanswered        )
                         write (lunit6, 7217)
7217                     format (5x, 110htheoretical questions) would be required in order
1                        to extend this modeling limitation, so the user has no other   ,/,
2 5                      x, 104halternative than decreasing the number of coupled ametani
3                        or Semlyen phases in his coupled branch group.       )
                         go to 6220
6118                     write (lunit6, 7118)
7118                     format (5x, 107hthe blank card which terminates the input of tacs
1                        data cards which define function blocks has now just been    ,/,
2 5                        x, 110hread.   but no function-block definitions preceded this b
3                          lank card.   the user will not be allowed to continue    ,/,
4 5                        x, 114hwith such a degenerate tacs representation.  if the user'
5                          s present representation has any meaning at all, it must     ,/,
6 5                        x, 109hsurely have one or more supplemental variables.   if addi
7                          ng or subtracting is therein involved (as is usually         )
                           write (lunit6, 7218)
7218                       format (5x, 103hthe case), a zero-th order function block could ha
1                            ve been employed for one of these operations, thereby        ,/,
2 5                          x, 112hresolving the present difficulty.   otherwise, as a last
3                            resort, the user can add a dummy tacs function block to     ,/,
4 5                            x,  81hhis tacs representation, being careful to provide it with
5                              a valid input variable.      )
                               go to 6220
6119                           write (lunit6, 7147)  bus6
                               go to 7421
6120                           write (lunit6, 7120)  bus1, lstat(14)
7120                           format (5x, 108hthe EMTP is in the process of inputting the user's
1                              tacs supplemental-variable data cards, with the last-read     ,/,
2 5                            x, 109hsuch data card being in error.   specifically, the user h
3                              as attempted to define a supplemental variable named     ,/,
4 5                            x, 2h ', a6, 105h'  (data field of columns 3-8 of the card), for
5                              which the type code which has been punched in columns 1-2    ,/,
6 5                            x,  24his invalid.   a value of, i4,  64h  was read from columns
7 1                            -2, which is an unrecognized type code.     )
                               go to 6220
6121                           write (lunit6, 7121)  bus2, bus1
7121                           format (5x, 109hthe tacs data which has now all been inputted is i
1                              ncomplete.   specifically, there is a difficulty associated    ,/,
2 5                            x,  65hwith the tacs function block which was given the (output)
3                                name  ', a6,  35h'  (as read from columns 3-8 of the     ,/,
4 5                              x, 111hleading card which defines this function block).   one of
5                                  the five possible inputs to this block was given name   ,/,
6 5                                x,   2h ', a6, 104h'  (as read from one of the fields of columns
7 12                               -17, 20-25, 28-33, 36-41, and 44-49, using  a6  format).    )
                                   write (lunit6, 7221)
7221                               format (5x, 110hbut this input variable is undefined.   it is neit
1                                  her the output of another function block, nor a supplemental   ,/,
2 5                                  x, 119hvariable, nor a tacs source of any type (1, 2, 90, 91, 92
3                                    , or 93 type codes).   did the user make a spelling error, the ,/,
4 5                                  x, 113hEMTP wonders.   the EMTP does not know what to do with th
5                                    e aforementioned input, so execution will be terminated.      )
7421                                 write (lunit6, 7521)
7521                                 format (/, 5x, 105hsince the user is having trouble with 6-charact
2                                    er tacs variable names, it is perhaps worth qualifying the     ,/,
3 5                                  x, 114hpreceding error text which complains about an unidentifia
4                                    ble name that is associated with a certain tacs component    ,/,
5 5                                  x, 113hor data class.   all that is really involved here is a sp
6                                    elling comparison with other usages of the same variable     ,/,
7 5                                  x, 108hname.   when for some particular tacs component a given n
8                                    ame  'name1 '  can not be found in a table where it           )
                                     write (lunit6, 7621)
7621                                 format (5x, 103hbelongs, the EMTP says that the former is unrecogn
1                                    izable.   yet, as used with the component explicitely      ,/,
2 5                                  x, 113hmentioned in the message,   'name1 '   may in fact be spe
3                                    lled exactly as the user intended.   it may be the table     ,/,
4 5                                  x, 108hbeing searched which is in error, due to faulty spelling
5                                    of the variable name on some other data card (which     ,/,
6 5                                  x, 115hwas the usage that generated the table entry).   hence th
7                                    e user should look at other data cards for mis-spelling of   )
                                     write (lunit6, 7721)
7721                                 format (5x, 107hthe name in question, if the spelling as printed i
1                                    n the above error text is actually as the user wanted it.     )
                                     write (lunit6, 7821)
7821                                 format (/, 5x, 105hthen too, while talking about spelling, it migh
1                                    t be a good idea to emphasize what is involved.   all six-    ,/,
2 5                                  x, 113hcharacter variable names are  a6  fortran alphanumeric in
3                                    formation.   when printed out within error messages, the       ,/,
4 5                                  x, 110hsix characters in question are delineated by a leading an
5                                    d a trailing quotation or apostrophe mark.   position        ,/,
6 5                                  x, 111hof imbedded blanks is indeed crucial, then.   for example
7                                    ,   'raver '  and  ' raver'  are completely different,        )
                                     write (lunit6, 7921)
7921                                 format (   5x,  99hdistinct 6-character names, as far as the EMTP
1                                    is concerned.   EMTP names (including those of tacs)     ,/,
2 5                                  x, 111hconsist of an ordered string of 6 characters, with 'blank
3                                    ' being a character like any other one.   two variable       ,/,
4 5                                  x, 109hnames are equal if and only if both characters of any cha
5                                    racter position are equal for the two names, for all    ,/,
6 5                                  x,  44hpossible character positions  1, 2, .... 6 .      )
                                     go to 6220
6122                                 write(lunit6,7122)  lstat( 17), lstat( 16)
7122                                 format (5x, 110hthe tacs representation currently being processed
1                                    has overflowed the dimensioned tacs storage.   specifically,    ,/
2 5                                  x,  29hthe tables which are numbered, i4,  75h  (see explanatory
3                                    directory of table numbers below) which have a length of      ,/,
4 5                                  x, i4, 108h  entries are now full.   because there is not enough
5                                    room to solve the problem, execution is being stopped.   ,/,1x)
                                     write (lunit6, 7222)
7222                                 format (5x,111hsince tacs can be partially redimensioned at execut
1                                    ion time, the user can simply increase the appropriate table   ,/,
2 5                                  x, 111hsizes on his  tacs-table-size cards , and try the data  c
3                                    ase again.  yet such relative size changes can only be      ,/,
4 5                                  x, 110hmade within the overall EMTP variable-dimensioning limita
5                                    tion of EMTP list number 19, which constrains the sum    ,/,
6 5                                  x, 109htotal space which is allowed for tacs arrays.   hence ove
7                                    rall EMTP redimensioning with a larger size for list      )
                                     write (lunit6, 7322)
7322                                 format (5x, 105hnumber 19 may also be necessary.   see the EMTP ca
1                                    se summary statistics for the current size of EMTP list     ,/,
2 5                                  x, 114hnumber 19.   the user is reminded that the existing list-
319                                  space can be allocated either in absolute terms (using   ,/,
4 5                                  x, 116hthe  'absolute tacs dimensions'  request) or in relative
5                                    terms (using the  'relative tacs dimensions'  request).  in  ,/,
6 5                                  x, 113hthe 'absolute' case, the 'present figure' for list 19 wil
7                                    l show the total working space which is required for the    ,/,
8 5                                  x, 103huser-requested dimensions.   for the 'relative' case, the
9                                    'present figure' will simply equal the limit.              )
                                     write (lunit6, 3322)
3322                                 format ( /,
3 5                                  x, 119hthe aforementioned directory of tacs table numbers, indic
4                                    ating upon what the table lengths depend, reads as follows.... ,/,
5 8                                  x,  85h1.  number of tacs dynamic function blocks, having laplac
6                                      e transfer functions  h(s) .              )
                                       write (lunit6, 7422)
7422                                   format (8x,  85h2.  number of zero-th order tacs function blocks (
1                                        type code 99 punched in cols. 1-2).                        ,/,
2 8                                      x,  77h3.  total number of input variables to tacs dynamic and z
3                                        ero-th order blocks.                                         ,/,
4 8                                      x,  74h4.  number of signal sources to tacs (type codes 1, 2, 90
5                                        , 91, 92, or 93).        ,/,
6 12                                     x, 104hremember that in addition to user-defined tacs sources,
7                                        there are internally-defined ones ( 'timex'  and             ,/,
8 12                                     x,  33h'unity' ,   as of january, 1977).      )
                                         write (lunit6, 7522)
7522                                     format (8x,  81h5.  number of tacs supplemental variables and devi
1                                        ces (type codes 99, 98, or 88).                              ,/,
2 8                                      x,  93h6.  total number of arguments describing the supplemental
3                                        variables proper (not the devices).                         ,/,
4 8                                      x,  99h7.  total number of numerical arguments (not alphanumeric
5                                        ) of supplemental variables (not devices).                    )
                                         write (lunit6, 7622)
7622                                     format (8x,  75h8.  number of tacs supplemental devices proper (ty
1                                        pe codes 50, 51, ... 58).                                    ,/,
2 8                                      x,  67h9.  total number of signed input variables to supplementa
3                                        l devices.                                                   ,/,
4 7                                      x, 105h10.  total number of transport delay history terms, digit
5                                        izer levels, and 2*points for nonlinear devices.             )
                                         write (lunit6, 2222)
2222                                     format (7x,  97h11.  the sum over all tacs dynamic function blocks
1                                          of the block order + one --- the sum of (n+1).              ,/,
2 7                                        x, 106h12.  total number of dynamic and zero-th order blocks (ma
3                                          y be less than the sum of table numbers 1 and 2).            ,/,
4 7                                        x,  72h13.  number of distinct  a6  names used in the tacs data
5                                          representation.                                              )
                                           write (lunit6, 2322)
2322                                       format (7x,  89h14.  number of nonzero factors of the triangulariz
1                                          ed matrix (steady-state or transients).                      ,/,
2 7                                        x, 100h15.  number of tacs output variables (as requested by the
3                                          user before tacs initial condition cards).                 ,/,1x)
                                           if ( lstat(17)  .eq.  15 )
1                                          write (lunit6, 2422)
2422                                       format ( 12x,  97hactually, the limit for tacs table 15 is one les
1                                          s than was indicated above, if the user's problem            ,/,
2 12                                       x,      103hpossesses an electric network.   the extra output-i
3                                          dentification name  'tacs  '  is internally added to         ,/,
4 12                                       x,       44hthe output vector by the EMTP, in this case.,
5            38                            h   more importantly, if the user asked,
6            38                            h for the output of all tacs variables, ,/,
7 12                                       x,       39hhe just shot himself in the foot.  this,
8            37                            h will never work, due to astronomical,
9            40                            h csp spy demands.  use selective output. )
                                           write (lunit6, 7822)   (i, i=1, 15)
7822                                       format (5x, 109hok, so just as with overall EMTP variable dimensio
1                                          ning, the user can intelligently allocate storage among the    ,/,
2 5                                        x, 110hdifferent tacs tables only if he knows the multiplicity o
3                                          f each.   for the tacs table numbers just delineated,         ,/,
4 5                                        x,  41hone has the following multiplicities ....   ,/,
5 10                                       x,   18htacs table number.,  10x,  15i5,  /,
6 10                                       x,  26halphanumeric multiplicity.,  2x,  75h    5    3    1
71    1    1    0    2    1    0    0    0    2    0    1 )
                                           write (lunit6, 7922)
7922                                       format(10x, 28hfloating-point multiplicity., 75h    7    3    0
1 5    0    0    1    3    0    1    5    3    6    2    5 ,/,
2 10                                       x,  21hinteger multiplicity.,  7x,   75h    7    3    1    1
3 3    2    0    3    1    0    0    3    0    1    1 ,/,
4 10                                       x,   19htotal multiplicity.   ,  9x,   75h   19    9    2    7
5   4    3    1    8    2    1    5    6    8    3    7 )
                                           write(lunit6,2122)  (i, i=1, 15),  (lstat( i), i=1, 15)
2122                                       format (5x,  92hdimensioned sizes of the tacs tables which were us
1                                          ed for the present run are as follows ....     ,/,
2 10                                       x,    18htacs table number.,   10x,   15i5,  /,
3 10                                       x, 18hpresent dimension.,   10x,   15i5    )
                                           go to 6220
6123                                       write (lunit6, 7123)
7123                                       format (  8h unused.  )
6124                                       write (lunit6, 7124)
7124                                       format (5x, 110hpreceding all tacs data cards, the user inputted a
1                                          special request card which was punched with the text  'tacs   ,/,
2 5                                        x, 113hEMTP sources'  in columns 1-17.   now, columns 21-80 of t
3                                          his card are to be read by the EMTP using  10a6  format,     ,/,
4 5                                        x, 109hin order to discover which type 1 through 10 EMTP sources
5                                          the user wants to have controlled by specified tacs      ,/,
6 5                                        x, 106hvariables.   recall that if the k-th field so-read contai
7                                          ns the nonblank  a6  text  'name' ,   then on the        )
                                           write (lunit6, 7224)  lstat(14), bus1
7224                                       format (5x, 109helectrical side it will be the EMTP source of type
1                                          -code  'k'  which will be given the numerical value of tacs   ,/,
2 5                                        x,  96hvariable  'name' .   now, the user's  'tacs EMTP sources'
3                                          card is in error because field number, i3,  13h  was punched ,/,
4 5                                        x,  16hwith the name  ', a6,  87h' ,   which does not correspond
5                                          to any tacs variable.   maybe this name was misspelled,      ,/,
6 5                                        x, 115hthe EMTP wonders.   in any case, since tacs cannot supply
7                                          a necessary interface request, execution will be stopped.     )
                                           go to 7421
6125                                       n1 = 90
                                           write (lunit6, 7125)  n1, bus1
7125                                       format (5x,  97has part of the user's tacs data which has now been
1                                          completely read by the EMTP, there was a type-, i2,  5h tacs  ,/,
2 5                                        x,  59hsource card which bore the 6-character alphanumeric name
3                                          ', a6,  45h'  in columns 3-8.   now, by definition, this      ,/,
4 5                                        x, 114hfield must be punched with a node name (a6 format) of the
5                                          EMTP electrical network which is a part of this 'hybrid'    ,/,
6 5                                        x,  10hdata case.     )
                                           write (lunit6, 7225)
7225                                       format (   1h+,   17x,  99hbut no branch or switch card of the sub
1                                          sequently-inputted electrical network defined this  a6  node   ,/,
2 5                                        x, 115hname.   since the EMTP does not know what variable of the
3                                          electrical network should be used to control this type-90   ,/,
4 5                                        x,  55hsource, execution of the data case will now be stopped. )
                                           go to 7421
6126                                       n1 = 91
                                           write (lunit6, 7125)  n1, bus1
                                           write (lunit6, 7226)
7226                                       format (   1h+,  17x,  91halso, this EMTP electrical-network node
1                                          must have a switch connected to it, since it is the    ,/,
2 5                                        x, 111hcurrent in the first (in order of data input) such adjace
3                                          nt switch which is to control the type-91 tacs source.   ,/,
4 5                                        x, 107hbut no such switch can be found by the EMTP.   since the
5                                          EMTP does not know what variable of the electrical       ,/,
6 5                                        x,  86hnetwork should be used to control this type-91 tacs sourc
7                                          e, execution must be stopped.    )
                                           go to 7421
6127                                       write (lunit6, 7127)  lstat(14), bus1
7127                                       format (5x, 104has part of the input of tacs data which is now com
1                                          plete, the user has elected to manually define initial   ,/,
2 5                                        x, 111hconditions for one or more tacs variables.   recall that
3                                          such data follows the blank card which terminates tacs    ,/,
4 5                                        x, 105houtput-variable specification cards, with one tacs variab
5                                          le name and associated initial condition on each             ,/,
6 5                                        x,  43hcard.   now, of such specifications, number, i4,
7 51                                       h  in order of input is for a tacs variable which is       ,/,
8 5                                        x,  54hpurported to have the 6-character alphanumeric name  ',
9                                          a6,  40h' .   but no such tacs variable has been         )
                                           write (lunit6, 7227)
7227                                       format (5x, 107hpreviously defined.   rather than allow the soluti
1                                          on to continue with initial conditions which are probably    ,/,
2 5                                        x,  44hincorrect, execution will now be terminated.    )
                                           go to 7421
6128                                       write (lunit6, 7128)
7128                                       format (5x, 105hduring triangularization of the real coefficient m
1                                          atrix  (a)  which is used by tacs either for dc initial   ,/,
2 5                                        x, 113hconditions or for the repeat solution of the time-step lo
3                                          op, an indication of singularity or near-singularity has     )
                                           write (lunit6, 7228)  lstat(14), bus1, flstat(14)
7228                                       format (5x,  106hbeen observed.   specifically, the trouble has ar
1                                          isen while eliminating to the left of the diagonal on row, i4, /,
2 5                                        x, 107hof the matrix, which corresponds to the equation that was
3                                          written for the tacs block which has the variable     ,/,
4 5                                        x,   8hnamed  ', a6,  64h'  for an output.   now, the original d
5                                          iagonal element value was,   e14.5      )
                                           write (lunit6, 7328)  flstat(15), epsiln
7328                                       format (5x,  93h(sign included), while just prior to reciprocation
1                                          this has diminished (in absolute value) to,  e14.5,  2h .    ,/,
2 5                                        x, 109ha near-zero diagonal value has thus occurred, as measured
3                                          by the ratio of these two values vis-a-vis the EMTP    ,/,
4 5                                        x,  63hmiscellaneous data parameter  'epsiln' ,   which has a va
5                                          lue of, e13.4, 31h .   most probably the user has    )
                                           write (lunit6, 7428)
7428                                       format (5x, 108hmade an error in a feedback loop of the tacs contr
1                                          ol circuitry which contains the aforementioned tacs block.    ,/,
2 5                                        x, 108hthe physical meaning associated with this matrix singular
3                                          ity is that the control system is unstable.   since    ,/,
4 5                                        x, 115hthe problem as posed has no physically-meaningful solutio
5                                          n in the steady-state, execution is being terminated here.     )
                                           go to 6220
6129                                       d1 = sqrtz ( flstat(14) )
                                           d2 = sqrtz ( flstat(15) )
                                           d3 = sqrtz ( tolmat )
                                           write (lunit6, 7129)
7129                                       format (5x, 108hduring triangularization of the complex coefficien
1                                          t matrix  (c)  which is used by tacs to find ac sinusoidal     ,/,
2 5                                        x, 112hsteady-state initial conditions, an indication of matrix
3                                          singularity (non-invertibility) or near-singularity has      )
                                           write (lunit6, 7228)  lstat(14), bus1, d1
                                           write (lunit6, 7329)  d2, d3
7329                                       format (5x,  86h(in magnitude), while just prior to reciprocation,
1                                          this has diminished in magnitude to, e14.5,  2h .    ,/,
2 5                                        x, 109ha near-zero diagonal value has thus occurred, as measured
3                                          by the ratio of these two values vis-a-vis the EMTP    ,/,
4 5                                        x,  63hmiscellaneous data parameter  'tolmat' ,   which has a va
5                                          lue of, e13.4, 31h .   most probably the user has    )
                                           write (lunit6, 7428)
                                           go to 6220
6130                                       write (lunit6, 7130)  bus2
7130                                       format (5x, 108hthe EMTP is in the process of inputting the user's
1                                          tacs supplemental-variable data cards, with the last-read     ,/,
2 5                                        x, 110hsuch data card being in error.   specifically, the user h
3                                          as attempted to define a supplemental variable having        ,/,
4 5                                        x,  19h6-character name  ', a6,  36h'  (read from cols. 3-8 of t
5                                          he card)                                                     )
                                           write (lunit6, 7230)  bus1
7230                                       format (1h+,   68x,
1      45                                  hfor which one of the algebraic operator codes         ,/,
2 5                                        x, 110his illegal according to tacs rules.   one of the  a1  fie
3                                          lds in which specification of the algebraic operators       ,/,
4        5                                 x,  31his to be punched was read as  ', a1,  67h' ,   whi
5                                          ch is an unrecognized character for this usage.   the user    ,/,
6 5                                        x, 108his advised to study the tacs rules related to the constru
7                                          ction of supplemental-variable cards, and then look          ,/,
8 5                                        x,  78hclosely at the last-read data card, to see precisely what
9                                          the EMTP objects to.                                      )
                                           go to 6220
6131                                       n1 = lstat(14)
                                           write (lunit6, 7131)
7131                                       format (5x, 107hthe problem under consideration includes tacs data
1                                          , the tacs function blocks of which have already all been     ,/,
2 5                                          x, 111hread by the EMTP.   columns 69-80 of the leading data car
3                                            d for each function block are read using  2a6  format,       ,/,
4 5                                            x, 108hin order to determine which tacs variables (if any....if
5                                              the fields are nonblank) are to be used as variable           ,/,
6 5                                            x, 111hlimits for the block.   now, on this basis the EMTP takes
7                                              exception to the data card which defines the function        )
                                               write (lunit6, 7231)  bus1
7231                                           format (5x,  18hblock with name  ', a6,  77h'  (as read from colum
1                                              ns 3-8).   columns         are for the     -limit name,       )
                                               if ( n1  .eq.  1 )
1                                              write (lunit6, 7331)
7331                                           format ( 1h+, 70x,  5h69-74,15x,  3hlow     )
                                               if ( n1  .eq.  2 )
1                                              write (lunit6, 7431)
7431                                           format ( 1h+, 70x,  5h75-80,14x,  4hhigh    )
                                               write (lunit6, 7531)  bus6
7531                                           format (5x,   7hwith  ', a6,  86h'  read therefrom by the EMTP.
1                                              this limit variable is unknown to the EMTP, not being     ,/,
2 5                                            x, 111hrecognized as any valid tacs variable name.   did the use
3                                              r make a spelling error, the EMTP wonders.   since the     ,/,
4 5                                            x, 114hEMTP is uncertain as to what the user wants done with thi
5                                              s limit, execution of this data case is being terminated.     )
                                               go to 7421
6132                                           write (lunit6, 7130)  bus2
                                               write (lunit6, 7132)  bus1
7132                                           format ( 1h+, 68x,
1  47                                          hfor which one of the function codes is invalid.         ,/,
2 5                                              x,  80hone of the  a5  fields in which the functions are to be s
3                                                pecified was read as  ', a5,  17h' ,   which is an           ,/,
4 5                                              x, 113hunrecognizable name for a supplemental variable function.
5                                                did the user make a spelling error, the EMTP wonders.      )
                                                 go to 6220
6133                                             write (lunit6, 7130)   bus1
                                                 write (lunit6, 7133)
7133                                             format ( 1h+, 68x,
1 48                                             hfor which no operator, function, or argument has         ,/,
2 5                                              x,  93hbeen defined.   the EMTP cannot calculate the output for
3                                                a tacs block whose input is unknown.           )
                                                 go to 6220
6134                                             write (lunit6, 7130)  bus1
                                                 write (lunit6, 7134)  lstat(14)
7134                                             format ( 1h+, 68x,
1 47                                             hfor which the device code is invalid.   a value          ,/,
2 5                                              x,    2hof,  i4, 100h  was read from columns 9-10 of the card, w
3                                                hich is not a legal tacs supplemental device code number.      )
                                                 go to 6220
6135                                             write (lunit6, 7130)   bus1
                                                 write (lunit6, 7236)
                                                 go to 6220
6136                                             write (lunit6, 7136)   bus1
7136                                             format (5x, 105hthe user has been inputting tacs data, with the la
1                                                st-read data card being a request for a function block.       ,/,
2 5                                                x,  46hthis was to be given (output) variable name  ', a6,
3 58                                               h' ,   as read from columns 3-8 of the last-read data card.   )
                                                   write (lunit6, 7236)
7236                                               format (
2 5                                                x, 111hyet none of the five available data fields which define t
3                                                  he inputs to this block have been used (all associated     ,/,
4 5                                                x, 115hdata fields are blank).   these are for alphanumeric name
5                                                  s, read from columns 12-17, 20-25, 28-33, 36-41, and 44-49    ,/,
6 5                                                x, 104husing  a6  formats.   the EMTP can not calculate the outp
7                                                  ut of a block for which the input is a mystery.      )
                                                   go to 6220
6137                                               write (lunit6, 7137)  bus1, lstat(14), bus2, bus3
7137                                               format ( 5x, 107hthe EMTP has been inputting tacs function blocks,
1                                                    with the last-read data card representing an illegal such     ,/,
2 5                                                  x,  90hrequest.   this was to have been the lead card of a funct
3                                                    ion block having (output) name  ', a6,  15h'  (cols. 3-8).    ,/,
4 5                                                  x,  18hinput field number, i3,  50h  to this block has been punc
5                                                    hed with tacs name  ', a6,  28h' ,   but is not immediately    ,/,
6 5                                                  x,  74hpreceded by either a plus sign or a minus sign.   rather,
7                                                    the character  ', a1,  24h'  was read.   remember,        )
                                                     write (lunit6, 7237)
7237                                                 format (5x, 107heach non-blank tacs input-name field ( a6  informa
1                                                    tion, columns 12-17, 20-25, 28-33, 36-41, 44-49)  must be     ,/,
2 5                                                  x, 117himmediately preceded by either a plus sign or a minus sig
3                                                    n ( a1  information, columns 11, 19, 27, 35, 43), indicating   ,/,
4 5                                                  x,  60hthe polarity to be applied to the associated input variab
5                                                    le.   )
                                                     go to 6220
6138                                                 write (lunit6, 7138)  lstat(14)
7138                                                 format (5x, 105hthe last-read data card has been taken by the EMTP
1                                                    to be a card specifying which tacs variables are to be      ,/,
2 5                                                  x, 115hplaced in the EMTP output vector (and hence will be avail
3                                                    able for printing and/or plotting purposes).   yet the  i2     ,/,
4 5                                                  x,  57hfield of columns 1-2 contains an illegal integer value  '
5                                                    , i2,  48h' .   only values of zero (for selective output)    ,/,
6 5                                                  x,  68hor unity (for output of all tacs variables) are allowed b
7                                                    y the EMTP.      )
                                                     go to 6220
6139                                                 write (lunit6, 7130)  bus3
                                                     n1 = lstat(14)
                                                     go to  (7139, 7239, 7339), n1
7139                                                 write (lunit6, 7439)  bus1
7439                                                 format (5x,  40hone of the arguments has been read as  ', a6,
1  48                                                h' ,   for which the user has failed to define an         ,/,
2 5                                                  x,  32halgebraic or a logical operator.       )
                                                     go to 6220
7239                                                 write (lunit6, 7539)  flstat(16)
7539                                                 format (5x,  63hone of the arguments has been read as the floating
1                                                    -point number,   e14.6,   27h ,   for which the user has      ,/,
2 5                                                  x,  52hfailed to define an algebraic or a logical operator.   )
                                                     go to 6220
7339                                                 write (lunit6, 7639)  bus2
7639                                                 format (5x,  62hone of the operators has been read as the alphanum
1                                                    eric text  ', a1,  35h' ,   for which the user has failed      ,/,
2 5                                                  x,  40hto define a function and/or an argument.    )
                                                       go to 6220
6140                                                   write (lunit6, 7140)
7140                                                   format (5x, 106hwere it not for the sharp eye and always helpful g
1                                                      ood common sense of the EMTP, this simulation might have     ,/,
2 5                                                    x, 108hcontinued.   but there are no requests for tacs variables
3                                                      to be placed in the output vector, nor is the EMTP        ,/,
4 5                                                    x, 110hexpecting any electrical network data to follow, as part
5                                                      of this data case.   recall that one or more nonblank       ,/,
6 5                                                    x, 111hfields on either the  'tacs outputs'  or the  'tacs EMTP
7                                                      sources'  card is required for EMTP electrical network         )
                                                       write (lunit6, 7240)
7240                                                   format (5x, 107hdata to follow the tacs data, as part of a hybrid
1                                                      data case.   there would thus be no output vector, and if     ,/,
2 5                                                    x, 112hthe simulation were allowed to continue, results would go
3                                                      unobserved.   possibly the user wanted this situation,      ,/,
4 5                                                    x, 113hbut the EMTP will not allow it.   for a data case to be a
5                                                      llowed to enter the time-step loop, there must have been      ,/,
6 5                                                    x,  99hrequested one or more output variables (either a tacs var
7                                                      iable, or an electrical-network variable).          )
                                                       go to 6220
6141                                                   write (lunit6, 7141)  nenerg
7141                                                   format (5x, 108hthe key word  'statistics'  or  'systematic'  can
1                                                      be punched in columns 55-64 of a switch card, as part of a     ,/,
2 5                                                    x, 104hdata case which has integer miscellaneous data parameter
3                                                      'nenerg'  punched nonzero.   but these must be             ,/,
4 5                                                    x,  28hcoordinated as follows .....                ,/,
5 12                                                   x,  51hpositive  'nenerg'  -------  use only  'statistics'  ,/,
6 12                                                   x,  51hnegative  'nenerg'  -------  use only  'systematic'  ,/,
7 12                                                   x,  45h    zero  'nenerg'  -------  use neither one.      ,/,
8 5                                                    x,  79hthe last-read data card is a switch card which violates t
9                                                      his rule.   a value of, i6,  27h   was read for  'nenerg' ,     )
                                                       write (lunit6, 7241)
7241                                                   format (5x, 108hthe user is reminded.   hence this data case eithe
1                                                      r mixes the two key words of columns 55-64, or the sign on     ,/,
2 5                                                    x,  84hparameter  'nenerg'  does not correspond to the single ke
3                                                      y word which is being used.           )
                                                       go to 6220
6142                                                   write( lunit6, 7142 )  kill
7142                                                   format( 5x, 27hunused kill code number...., i5 )
                                                       go to 6220
6143                                                   write (lunit6, 7143)   bus2
7143                                                   format (5x, 104hthe tacs data which has now all been inputted is i
1                                                      ncomplete.   specifically, there is a problem which is       ,/,
2 5                                                    x,  93hassociated with the tacs supplemental variable or device
3                                                      which was given the (output) name  ', a6,  6h'  (as          ,/,
4 5                                                    x, 109hread from columns 3-8 of the data card which defined this
5                                                      variable).   the difficulty here is associated with         ,/,
6 5                                                    x, 111hone of the arguments or inputs to this supplemental varia
7                                                      ble or device.   this problem argument or input, which         )
                                                       write (lunit6, 7243)  bus1
7243                                                   format (5x,  46hwas identified by the 6-character  a6  name  ',
1                                                      a6,  46h' ,   is undefined.   this name is neither the       ,/,
2 5                                                    x, 110houtput of a tacs dynamic or zero-th order function block,
3                                                        nor is it a tacs source name.   the name in question        ,/,
4 5                                                      x, 108hdoes not identify any other supplemental variable or devi
5                                                        ce, either, at least not any which has been defined          ,/,
6 5                                                      x, 109hbefore the appearance of the problem supplemental variabl
7                                                        e or device (as required by tacs rules on ordering).         )
                                                         write (lunit6, 7343)
7343                                                     format (5x,  53hdid the user make a spelling error, the EMTP wonde
1                                                        rs.          )
                                                         go to 7421
6144                                                     n1 = 93
                                                         write (lunit6, 7125)  n1, bus1
                                                         write (lunit6, 7144)
7144                                                     format ( 1h+,      17x,      91halso, this EMTP electrical-network
1                                                        node must have a switch connected to it, since it is the      ,/,
2 5                                                      x, 108hstatus of the first (in order of EMTP data input) such sw
3                                                        itch which is to be controlled by this type-93 tacs          ,/,
4 5                                                      x, 109hsource.   but no EMTP switch adjacent to the node in ques
5                                                        tion can be found by the EMTP.   since the EMTP does         ,/,
6 5                                                      x, 110hnot know what variable of the electrical network should b
7                                                        e used to control this type-93 tacs source, execution        ,/,
8 5                                                      x,  33hmust be terminated at this point.            )
                                                         go to 7421
6145                                                     write (lunit6, 7145)
7145                                                     format (5x, 104hthe EMTP data case now being inputted involves one
1                                                        or more continuously-transposed distributed-parameter       ,/,
2 5                                                      x, 112htransmission lines, with frequency-dependent representati
3                                                        on of resistance  r  and  inductance  l  in one or more       ,/,
4 5                                                      x, 115hof the modes.   in fact, the last-read data card is the m
5                                                        iscellaneous data parameter card which precedes the point-    ,/,
6 5                                                      x, 109hby-point definition of a pair of weighting functions.   t
7                                                        he first 16 columns of this card are read using  2i8         )
                                                         write (lunit6, 7245)  lstat(14), lstat(15), lstat(16)
7245                                                     format (5x, 105hformat, to find parameters  'ntime1'  and  'ntime2
1                                                        ' .   these give the number of points which are used to      ,/,
2 5                                                      x,  81hdefine the two weighting functions  a1(t)  and  a2(t) ,
3                                                        and were read as values, i8, 6h   and, i8,  2h ,            ,/,
4 5                                                      x, 101hrespectively.   the larger of these exceeds the available
5                                                        maximum working space, which is dimensioned, i8,  9h   cells.  )
                                                         if ( kburro .eq. 0 )
1                                                        write (lunit6, 7445)
7445                                                     format (
1   5                                                    x,         98hconcerning this latter figure, the user should b
7                                                        e aware that the   /label/   storage of EMTP lists            )
                                                         if ( kburro .eq. 0 )
1                                                        write (lunit6, 7345)
7345                                                     format (5x, 109hnumber  5  and  7  only is involved.   this limite
1                                                        d region of memory is divided into four equal-sized arrays,    ,/,
2 5                                                      x, 112heach having the aforestated inadequate dimension.   in or
3                                                        der to get the user's wieghting functions into the EMTP      ,/,
4 5                                                      x, 111h(and compacted into their final storage in the array of e
5                                                        mtp list number 14), the sizes of EMTP list numbers  5       ,/,
6 5                                                      x, 109hand/or  7  must be increased.   use the above explanation
7                                                        together with known list multiplicities in order to         ,/,
8 5                                                      x,  40hdetermine the required increase in size.           )
                                                         if ( kburro .ne. 0 )
1                                                        write (lunit6, 7545)
7545                                                     format ( 5x, 39hconcerning this latter figure, the user,
1             35                                         h should remember that he is using a,
2             41                                         h virtual computer, so list 23 of "vardim"
3                                                        ,/,  5x,  38his used for storage.  such a region of,
4             39                                         h memory is divided into four equal-size,
5             38                                         h arrays, with each of these having the
6                                                        ,/,  5x,  35haforementioned inadequate size.  in,
7             34                                         h order to successfully process the,
8             40                                         h weighting functions, list 23 must grow.   )
                                                         go to 6220
6146                                                     write (lunit6, 7146)  bus1, bus2
7146                                                     format (5x, 110hswitch cards are now being inputted, for the EMTP
1                                                        data case under consideration.   specifically, the last-read   ,/,
2 5                                                      x, 101hdata card represents a request for a type-11 switch (i.e.
3                                                        , a diode or a valve) which connects node  ', a6,  1h'       ,/,
4 5                                                      x,  12hwith node  ', a6,  3h' .    )
                                                         n1 = kswtch - 1
                                                         write (lunit6, 7246)  n1
7246                                                     format ( 1h+,    28x,             87hnow, columns 61-64 were punch
1                                                        ed with the key-word  'same' ,   which is understood to be     ,/,
2 5                                                      x, 114ha request that the physical characteristics of the presen
3                                                        t valve or diode be identical to those of the most-recent      ,/,
4 5                                                      x,  56hpreceding type-11 switch element.   but of the preceding,
5                                                        i4,  45h  switch cards, none were type-11 (punched in       ,/,
6 5                                                      x, 104hcols. 1-2).   thus the reference switch does not exist, a
7                                                        nd the EMTP has no way of knowing what modeling              ,/,
8 5                                                      x,  65hparameters should be used to describe the present diode o
9                                                        r valve.       )
                                                         go to 6220
6147                                                     write (lunit6, 7147)  bus5
7147                                                     format ( 1x, 100( 1h- ),  /,
1                                                        / 5x,  39hthis switch makes reference to the non-,
2           33                                           hexisting control tacs variable  ',
3                                                        a6,   1h'  , // 1x, 100(1h-)  )
                                                         go to 7421
6148                                                     write (lunit6, 7148)  bus1, bus2, flstat(15), flstat(16)
7148                                                     format ( 5x, 102hthe last-read data card is for a switched-inducta
1                                                        nce element (type-93 switch card) which connects node        ,/,
2 5                                                      x,   2h ', a6,   15h'  with node  ', a6,  73h' .   but the value
3                                                        for residual flux (punched in columns 15-24, and read       ,/,
4 5                                                      x, 110husing  e10.6  format) is illegal, for it exceeds the satu
5                                                        ration flux (punched in columns 45-54, and read using       ,/,
6 5                                                      x,  64he10.6  format).   the numerical values for these two numb
7                                                        ers are,     e15.4  ,  5h  and,    e15.4 , 17h ,  respectively.  )
                                                         write (lunit6, 7248)
7248                                                     format (5x,  56hcorrect this violation of the EMTP rules, and try
1                                                        again.           )
                                                         go to 6220
6149                                                     write (lunit6, 7149)  flstat(14)
7149                                                     format (5x, 105hthe last-read data card has been taken by the EMTP
1                                                        to be a source card of type 14 (sinusoidal generator).       ,/,
2 5                                                      x, 111hbut the frequency as read from columns 21-30 of this card
3                                                        is not positive, as required by EMTP rules.   a value       ,/,
4 5                                                      x, 2hof,     e13.4,   88h  was read.   sinusoids of other than p
5                                                        ositive frequency must be rejected by the EMTP on           ,/,
6 5                                                      x,  74haesthetic grounds.   the user should correct columns 21-3
70                                                       , and try again.         )
                                                         go to 6220
6150                                                     d1 = flstat(14) / deltat
                                                         write (lunit6, 7150)  deltat, bus1, bus2
7150                                                     format (5x, "The user has picked a time-step size  'deltat'  (read from columns 1-8 of the floating-point miscellaneous      ",/, &
                                                              5x, 'data card) which is too small for one of the distributed parameter transmission lines that is to be modeled using      ',/, &
                                                              5x, 'Semlyen recursive convolution.   A step-size of', e14.4,  '  seconds was requested, which is too large      ',/, &
                                                              5x, 'for the Semlyen line having phase number 1 that connects node  ', "'", a6,   "'", ' with node  ', "'", a6, "'", '.   The')
                                                         write (lunit6, 7250)  lstat(15), flstat(14), d1
7250                                                     format (5x, 'rule is that the travel time for all Semlyen modes must exceed  two  time steps.   but for mode number',  i4, /, &
                                                              5x,  "of the aforementioned line, the travel time  'tau'  is only",    e14.4,    '  seconds.   The ratio of these gives     ',/, &
                                                              5x,  'tau/deltat =',   e12.4,   " ,   which is too small (less than 2.0).   Decrease the time-step size  'deltat'          ",/, &
                                                              5x, 'accordingly, or alter the transmission line modeling, in order to make this data case solvable using the EMTP.          ')
                                                         go to 6220
6220                                                     lastov = nchain
                                                         nchain = nfrfld + 50
                                                         if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )
4568                                                     format ( 24h "exit  module over53." )
99999                                                    return
                                                       end function block
                                                       c
                                                       c     end of file: over53.for
                                                       c
