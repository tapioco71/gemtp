c-*- mode: fortran; syntax: ansi-fortran-77; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
c
c     file: over52.for
c
c
c     subroutine over52.
c
      subroutine over52
      implicit real*8 (a-h, o-z) ,
     1     integer*4 (i-n)
      include  'blkcom.ftn'
      if ( iprsup  .ge.  1 )
     1 write ( lunit6, 4567 )
 4567 format ( 24h  "begin module over52." )
      if ( nchain  .ne.  52 )   go to 99999
      n1 = kill - 50
      go to (
     5 6051, 6052, 6053, 6054, 6055, 6056, 6057, 6058, 6059, 6060,
     6 6061, 6062, 6063, 6064, 6065, 6066, 6067, 6068, 6069, 6070,
     7 6071, 6072, 6073, 6074, 6075, 6076, 6077, 6078, 6079, 6080,
     8 6081, 6082, 6083, 6084, 6085, 6086, 6087, 6088, 6089, 6090  ), n1
 6051 write(lunit6, 7051)  lstat(14), bus1, bus2
 7051 format( 5x,  74hthe user's network includes a group of mutually-co
     1upled branches of order , i2, 23h which are specified by   ,/,
     2 5x,  74hmatrices (a), (b).   the first branch of this problem gro
     3up connects bus ', a6, 12h' with bus ', a6,  8h.   when  ,/,
     4 5x, 108husing the (a), (b) option, the number of upper-triangle m
     5atrix elements ( n(n+1)/2 ) must not exceed the bus  ,/,
     6 5x, 110hdimensioning (list 1) of the program, as it has here.   i
     7f the user really wants so many conductors, he should   ,/,
     8 5x,  46hswitch to use of (r), (l) format, if possible.   )
      go to 6220
 6052 write(lunit6, 7052)  lstat(16)
 7052 format( 5x, 108hthe user has been inputting a saturable-transforme
     1r component, the data for which is not all legal.   of the  ,/,
     2 5x,  31hrules to follow, that numbered , i1,  39h has been violat
     3ed.   rules include....   ,/,
     4 10x,  81h1)  zero-sequence reluctance of air-return path of 3-pha
     5se unit must be positive.    ,/,
     6 10x, 107h2)  leakage inductance  l  for all but the first winding
     7 must be nonzero.   first winding must have nonzero  ,/,
     8 14x, 18hleakage impedance.   )
      write(lunit6, 7152)
 7152 format( 10x,  74h3)  rated winding voltages (number of winding tur
     1ns) must all be positive.      ,/,
     2 10x, 115h4)  the rated winding voltage (number of winding turns)
     3must be the same for corresponding windings of all 3 phases  ,/,
     4 14x,  25hof a 3-phase transformer.   )
      goto6220
 6053 write(lunit6,7453)
 7453 format( 5x, 109hthe user is attempting to use the steady-state cas
     1cading of pi-circuits option to solve a very large problem.   )
      write(lunit6,7053)lstat(12)
 7053 format(5x,26herror in cascaded-pi data.  ,/, 5x,
     1  107hthere are too many phases requested for the program to handl
     2e.   the number of phases 'nphcas' as read from  ,/,
     3 5x, 32hcolumns 27-32 of header card is , i6,  69h.   this is too
     4large for present program dimensions.   the cascading  ,/,
     5 5x, 109hcode has temporary working arrays which are equivalenced
     6to various components of regular (permanent) program  ,/,
     7 5x, 113htables, and it is one of the list limits for these which
     8is insufficient for the user's problem.   in particular,   )
      if( lstat(15) .ne. 1 )  go to 7253
      write(lunit6, 7153)  lstat(16), lpast
 7153 format( 5x, 11hn*(n+1)/2 = , i5,  20h  exceeds 'lpast' of ,
     1 i5,  42h   , where  n = 3*nphcas ,  and 'lpast' is   ,/,
     2 5x,  38hthe size of list number 8 (see below).   )
 7253 if( lstat(15) .ne. 2 )  go to 6220
      write(lunit6, 7353)  lstat(16), lbus
 7353 format( 5x,  21hnphcas*(nphcas+1)/2 = , i5,   19h  exceeds 'lbus'
     1of, i5,  41hwhere 'lbus' is the size of list number 1  ,/,
     2 5x,  12h(see below).   )
      goto6220
 6054 write(lunit6, 7154)
 7154 format( 5x, 113hthe user has attempted to use the steady-state cas
     1cading of pi-circuits option, but has made an input data error.  )
      write(lunit6,7054)lstat(13),lstat(12)
 7054 format( 5x, 101hthe line-position card provides for specification
     1of a transposition by means of a phase-position map   ,/,
     2 5x, 101hbeginning in column 25, and using 'i4' format for each of
     3 'nphcas' entries.   these entries must be a   ,/,
     4 5x,  67hpermutation of the integers 1, 2, 3, ..., 'nphcas', with
     5'nphcas' =, i3,  35h for the present line.   but one of  ,/,
     6 5x,  42hthe position-map entries has been read as , i4,
     7  50h, which is clearly impossible.   assuming that the   )
      write(lunit6, 7254)
 7254 format( 5x,106hdigits which were punched on the card were ok, chec
     1k for right-justification within the fields of width 4.   ,/,
     2 5x, 112hrecall that an erroneous shift to the left by one column
     3multiplies the number by 10, while a shift to the right  ,/,
     4 5x, 112hwill spill over into the following field, generally creat
     5ing a very large integer when the latter field is read.    )
      goto6220
 6055 write(lunit6, 7154)
      write(lunit6,7055)lstat(12)
 7055 format( 5x, 108h'multip' is the multiplicity parameter, read from
     1columns 9-12 of the line-position card.   as such, it must  ,/,
     2 5x, 111hbe a non-negative integer, with zero or blank given the d
     3efault value of unity.   but the emtp has read a value   ,/,  5x,
     4  3hof , i4,  67h for 'multip', which has no physical meaning, and
     5 must be rejected.  )
      goto6220
 6056 write(lunit6, 7154)
      write(lunit6,7056) lstat(13), lstat(14), lstat(12)
 7056 format( 5x,  62hthe most-recently-inputted group of r-l-c shunt br
     1anches were , i2,  36h in number, with both ends of number  ,/,
     2 5x, i2,  49h of this group connected to the same node number ,
     3 i2,  41h.   now while such a requested connection    ,/,
     4 5x, 114hpresents no problem if taken literally, the emtp's infall
     5ible intuition is inclined to feel that the user has made  ,/,
     6 5x, 110ha punching error in columns 3-14 of his data card (where
     7the two numbers in question are read in).   a network  ,/,
     8 5x,  81hsolution with such degenerate topology will not be permit
     9ted.   sorry 'bout that.    )
      goto6220
 6057 write(lunit6, 7154)
      write(lunit6,7057)lstat(12),lstat(13)
 7057 format(5x,26herror in cascaded-pi data.  ,/,
     15x,4hnode ,i4,24h short circuited to node ,i4,57h.  one of these n
     2odes should be removed from the circuit.  )
      goto6220
 6058 write(lunit6, 7154)
      write(lunit6,7058)
 7058 format(5x,103h'stop cascade' card missing in referenced cascaded p
     1i section.  this can be corrected by either putting,/,5x,48hthis c
     2ard in or removing the 'cascaded pi' card.)
      go to 6220
 6059 write (lunit6, 7059)  lstat(16), bus1
 7059 format( 5x,  52h3-phase compensation for synchronous machine numbe
     1r , i2,  30h breaks down.   terminal bus ', a6, 1h' , /,
     2 5x,  67halready has another element requiring compensation connec
     3ted to it.   )
      go to 6220
 6060 write(lunit6, 7154)
      write(lunit6,7060)lstat(12)
 7060 format(5x,32hinvalid shunt r-l-c node number  ,i3,42hwas used.  th
     1is number is out of sequence.  ,/,5x,50hcheck connections in this
     2area and renumber nodes.   )
      go to 6220
 6061 write(lunit6, 7061)  flstat(15), flstat(16)
 7061 format( 5x, 101hswitched-inductance element just read has inductan
     1ce parameters  l1  and  l2  (columns 25-44) punched  ,/,
     2 5x,  2has, e14.3, 4h and, e14.3,  83h, respectively.   one or bot
     3h of these are non-positive, which is illegal.   if the  ,/,
     4 5x,  63huser wants a zero value, he must punch a small positive n
     5umber.   )
      go to 6220
 6062 write(lunit6, 7062)  flstat(14), flstat(15)
 7062 format( 5x,  94hswitched-resistance element just read has resistan
     1ce  r  (columns 15-24) or breakpoint voltage  ,/,
     2 5x, 118h(columns 35-44) nonpositive, which is illegal.   if the u
     3ser wants a zero value, he must punch a small positve number.  ,/,
     4 5x,  48hnumbers read from the two fields in question are,
     5 e13.3, 5h and , e13.3,  15h, respectively.  )
      go to 6220
 6063 write(lunit6, 7063)
 7063 format( 5x, 102htrouble has been encountered during decoding of us
     1er's input data.   had not this very civilized error  ,/,
     2 5x, 110hmessage been specially provided, the computer operating s
     3ystem would have performed the termination in its own ,/,
     4 5x, 109hcursory, inimitably-inscrutable manner.   most probably,
     5the last data card read carries some erroneous data,   ,/,
     6 5x,  37has per the following suggestions ....   ,/,
     7 8x, 111h1) it is possible that one or more columns carry illegal,
     8 completely-uninterpretable punches, in which case the   )
      write(lunit6, 7163)
 7163 format( 11x, 104hlast 80-column card image printed out above might
     1 not be an accurate image of the data card in question.   ,/,
     2 11x, 102hthe user should be aware of this limitation on the fidel
     3ity of the printed card images, which can only   ,/,
     4 11x, 116hbe guaranteed if all characters are legally-punched.   y
     5et this case of illegal punches is rare.   continue reading.  ,/,
     6 8x, 101h2) more commonly, the user may have punched data a little
     7 to the left or to the right of where he had   ,/,
     8 11x, 104hintended, thereby placing a decimal point in an integer
     9field, or a non-numeric character in a numerical    )
      write(lunit6, 7263)
 7263 format( 11x,   6hfield.  ,/,
     1 8x, 101h3) one or more random punching errors in a numeric field
     2will generally produce trouble.   letters or   ,/,
     3 11x, 105hpunctuation are not allowed, of course (with a few very
     4special exceptions, which are highly improbable).    ,/,
     5 8x, 109h4) punching of floating-point numbers with an 'e' (power
     6of 10) can lead to trouble if not right-justified in  ,/,
     7 11x, 108hthe field, since any blank spaces to the right of the la
     8st-punched exponent digit are interpreted as zeroes,    )
      write(lunit6, 7363)
 7363 format( 11x,  99hthereby increasing the exponent by a power of ten
     1.   this may produce a result which is outside the   ,/,
     2 11x,  38hlegal range of floating-point numbers.    ,/,
     3 8x, 111h5) out-of-sequence data is a notorious offender.   for ex
     4ample, if a legitimate branch card is read as a switch  ,/,
     5 11x, 107hcard (maybe it was mixed in with the switch cards by mis
     6take), one could have 2 decimal points punched in a  ,/,
     7 11x,105 hdata field which is read under 'e'-format control, which
     8 is clearly illegal.   the user should check that   )
      write(lunit6, 7463)
 7463 format( 11x, 101hblank cards, and any other data-termination or de
     1lineation cards, are properly positioned in the data   ,/,
     2 11x, 106hstream.   only with such care will the emtp think that i
     3t is reading what the user thinks he is inputting,    ,/,
     4 11x, 22hat said point in time.    )
      go to 6220
 6064 write (lunit6, 7064)  bus1
 7064 format( 5x, 38hswitch connectivity error.  subroutine,
     1            37h switch has determined that a loop of,
     2            31h closed switches is about to be   ,/,
     3        5x, 24hcreated involving bus  ",  a6,
     4            35h".   such switch loops are illegal.  )
      go to 6220
 6065 write(lunit6, 7065)  lstat(14), flstat(15), flstat(16)
 7065 format( 5x,  69hthe last-read data card has been taken by the emtp
     1 to be card number , i3,  25h of the user's frequency-   ,/,
     2 5x, 115hdependent line-constants input.   it is illegal because t
     3he frequency, as read from columns 21-30 using 'e' format,   ,/,
     4 5x,  86his not greater than that read from the preceding card.
     5the frequency of this card is, e13.4,  12h, while that   ,/,
     6 5x,  25hof the preceding card was, e13.4,  73h.   data cards for
     7line constants must be inputted in order of increasing  ,/,
     8 5x,  44hfrequency ---- strictly monotone-increasing.  )
      go to 6220
 6066 write(lunit6, 7066)
 7066 format( 5x, 105hthe user is presently trying to input line constan
     1ts ('r', 'l', and 'f' fields) for too many frequencies.   )
      write(lunit6, 7166)  lstat(12)
 7166 format( 5x,  59hthe current program dimensioned storage for such p
     1oints is , i3,  36h.   does the user really need all of   ,/,
     2 5x, 108hthese points that he is using, the emtp wonders.   if he
     3really does, and can justify his request to program   ,/,
     4 5x, 111hmaintenance, an expansion of these tables which have just
     5 overflowed could easily be made.   otherwise, maybe a   ,/,
     6 5x, 108hre-reading of the user's manual, or a talk with someone f
     7amiliar with the weighting-function calculation, or   ,/,
     8 5x,  24hboth, might be in order.    )
      go to 6220
 6067 write(lunit6, 7067)  lstat(13)
 7067 format( 5x, 106hto find the near-end weighting function  a2(t) ,
     1recourse is made to the infinitely-long line.   for time  ,/,
     2 5x, 108hless than the infinite-frequency travel time 'tau', it is
     3 the infinite-line response which is actually used.    ,/,
     4 5x, 112hinput parameter  'npinf'  (columns 25-32 of the second mi
     5sc. data card) specifies the number of points which are   ,/,
     6 5x,  54hused to define this response, with the user requesting,
     7 i4,  1h.   )
      write(lunit6, 7166)  lstat(12)
      go to 6220
 6068 write(lunit6, 7068)  flstat(14), lstat(15), lstat(13)
 7068 format( 5x, 106hthe number of points which are used to define the
     1weighting functions is controlled (in part) by two misc.   ,/,
     2 5x,  60hdata parameters.   the first of these is  'rmax'  with va
     3lue, e12.3,  39h, and the second is  'mnum'  with value   ,/,
     4 5x, i3, 106h.   either these were user-punched numbers, or they a
     5re default values built into the emtp   anyway, these   ,/,
     6 5x, 100hparameters combine to produce a storage requirement for t
     7he finite line given by    (rmax-1)*mnum = , i4,  8h points.   )
      write(lunit6,7168) lstat(12)
 7168 format( 5x, 45hthe present dimensions of the program permit , i4,
     1  8h points. )
      if ( flstat(13) .gt. 0. ) write(lunit6,7268)
 7268 format(5x,100hsince variable time increment dt was requested by mn
     1um less than zero, the total storage requirement ,/, 5x,109hwas es
     2tablished by summation of similar expressions with adjusted values
     3 of rmax and mnum for each time span. )
      go to 6220
 6069 write(lunit6,7069)
 7069 format( 5x, 101hthe user is presently trying to input i-v points w
     1hich define his magnetic saturation characteristic.    ,/,
     2 5x,  53hbut he is using too many points to define this curve.   )
      write(lunit6, 7166)  lstat(12)
      go to 6220
 6070 write(lunit6,7070)
 7070 format( 5x, 108hthis error is most likely due to the computer syst
     1em library subroutine for the sine function giving a value   ,/,
     2 5x, 112hgreater than unity.   if there were any justice in the wo
     3rld, this trouble would not have arisen.   in any case,   ,/,
     4 5x, 115hthe difficulty is probably not the user's fault, so he sh
     5ould go complain to program maintenance about his trouble.    )
      go to 6220
 6071 write(lunit6, 7071)  lstat(12), flstat(13), flstat(14)
 7071 format( 5x, 101hthe user is presently trying to input i-v points w
     1hich define his magnetic saturation characteristic.    ,/,
     2 5x, 110hbut he has violated the rule which requires that both vol
     3tage and current be strictly monotone-increasing (for    ,/,
     4 5x,  60hthe order of data-card input).   the last-read card (numb
     5er , i3,  37h of the characteristic) violates this   ,/,
     6 5x,  91hrequirement, as shown by the following variable values wh
     7ich were punched by the user .....    ,/,
     8 20x, 16hlatest current = , e13.4, 10x, 19hpreceding current = ,
     9 e13.4   )
      write(lunit6, 7171)  flstat(15), flstat(16)
 7171 format( 20x, 16hlatest voltage = , e13.4, 10x,
     1 19hpreceding voltage = , e13.4, 1h.  )
      if( lstat(12) .gt. 1 )  go to 6220
      write(lunit6, 7271)
 7271 format( 5x, 108hof course since trouble was spotted upon an examin
     1ation of the user's very first card of the characteristic,   ,/,
     2 5x, 111hthe 'preceding card' refers to the implied point (0, 0) w
     3hich represents the origin.   in this case, the user's   ,/,
     4 5x, 113htrouble stems from his failure to punch a positive pair o
     5f values (i, v) on his first card of the characteristic.   )
      go to 6220
 6072 write(lunit6, 7072)  flstat(16)
 7072 format( 5x, 106hthe user has been inputting a type-97 (staircase)
     1time-varying resistance element, and has violated one of  ,/,
     2 5x, 103hthe data rules pertaining there to.   in particular, the
     3last-read data card has a nonpositive value of  , e13.4,  /,
     4 5x, 112hpunched in columns 9-16.   such a nonpositive resistance
     5for a portion of the time-vs.-resistance characteristic   ,/,
     6 5x,  51his interpreted as a data-punching error by the emtp   )
      go to 6220
 6073 write(lunit6, 7073)  lstat(16), bus1
 7073 format( 5x, 103hthe last-read data card has been taken by the emtp
     1 to be a source card, specifying a generator of type , i2, /,
     2 5x,  16hlocated on bus ', a6,    83h'.   now for such a source, c
     3olumns 11-20 are to be punched with the amplitude (for   ,/,
     4 5x, 113ha type-14 sinusoidal source, this is the peak value).   b
     5ut the user has punched a zero, or left the field blank.   ,/,
     6 5x, 109hwhile such a case could easily be solved, the emtp's supe
     7rbly-honed perspicacity is offended by such an idea.   ,/,
     8 5x, 109hafter all, if a voltage source is involved, zero amplitud
     9e means permanent grounding, in which case  '      '    )
      write(lunit6, 7173)
 7173 format( 5x, 105h(6 blank characters as bus name) or a switch to gr
     1ound should be used, for clarity.   if a current source   ,/,
     2 5x, 114his involved, zero amplitude means that the source doesn't
     3 exist at all.   either way, the user should correct this  ,/,
     4 5x,  86h'aesthetic difficulty', and try again.   who knows, next
     5time the job may run further.   )
      go to 6220
 6074 write(lunit6, 7074)  flstat(15), lstat(16)
 7074 format( 5x, 109hwhile solution of this problem could continue, the
     1 emtp has become suspicious of a data 'misunderstanding' on   ,/,
     2 5x, 110hthe part of the user.   note that the termination time of
     3 the study ( 'tmax' ) is nonpositive, indicating that   ,/,
     4 5x, 110honly a sinusoidal steady-state solution is desired.   the
     5 value of  'tmax'  actually read from columns 9-16 of   ,/,
     6 5x,  36hthe first miscellaneous data card is, e13.4,
     7 14h.   yet of the , i3,  27h sources which the user has  ,/,
     8 5x, 107hinputted, none is a sinusoidal source which is to be pres
     9ent in the steady-state (for time less than zero).     )
      write(lunit6, 7174)
 7174 format( 5x, 105hrecall that  'tstart'  of columns 61-70 of the sou
     1rce card must be punched with a negative number, if the  ,/,
     2 5x, 111hsource is to be present in the steady-state solution.   t
     3hus there are no sources for the phasor solution which  ,/,
     4 5x, 111hwas requested, and the solution must necessarily be ident
     5ically zero.   such a degenerate, trivial problem will   ,/,
     6 5x,  15hnot be allowed.   )
      go to 6220
 6075 write(lunit6, 7075)
 7075 format( 5x,  99hthe user has been inputting frequency-dependent li
     1ne constants to the weighting-function supporting    ,/,
     2 5x, 110hprogram, in order to calculate  a1(t)  and  a2(t)  for th
     3e zero-sequence mode of a distributed-parameter line.   )
      write(lunit6, 7175)  flstat(15)
 7175 format( 5x, 105hbut the last-read data card has gotten him in trou
     1ble.   columns 1-10 (the resistance field, r) are blank   ,/,
     2 5x,  96hor zero, while columns 11-20 (the inductance field, l) co
     3ntain the nonzero floating-point number,  e14.4,  4h   .    ,/,
     4 5x, 108hnow if this card was meant to be the blank card terminati
     5ng the line-constants input, the user should remove  ,/,
     6 5x, 107hthe punches from columns 11-20, and resubmit the job.   i
     7f on the other hand this card was intended to be a   ,/,
     8 5x, 110hline-constants card, the user had better re-think the mat
     9ter.   resistance is the parameter which provides the   )
      write(lunit6, 7275)
 7275 format( 5x, 109hdamping in the zero-sequence network which increas
     1es dramatically with frequency, and is a crucial ingredient  ,/,
     2 5x, 112hto this sophisticated distributed-line representation.
     3the emtp will not allow the user to calculate weighting  ,/,
     4 5x, 113hfunctions without resistance present.   it is not even cl
     5ear if the inverse fourier transformation logic is valid  ,/,
     6 5x, 111hfor such a case, since would not the impulse response of
     7such a distortionless line produce an impulse for  a1.    ,/,
     8 5x, 114hin this case, re-run a line-constants program to calculat
     9e legitimate line constants, and then try this job again.  )
      go to 6220
 6076 write(lunit6, 7076)  lstat(16)
 7076 format( 5x, 112hthe emtp logic has interpreted the last-read data
     1card to be part of the specification cards for plotted output. ,/,
     2 5x,  73hbut the type-code read from columns 1-2 cannot be accepte
     3d.   a value of , i2, 25h was read, which does not   ,/,
     4 5x, 114hcorrespond to any of the legitimate existing codes (0, 1,
     5 2).   if the card in question really is not a plot card,    ,/,
     6 5x, 108hthe user must discover how his data does not conform to t
     7he assumed input sequencing, and rectify the error.     ,/,
     8 5x, 116hthe correct number of, and placement of, blank delimiter
     9cards is a notorious cause of data-synchronization trouble.   )
      go to 6220
 6077 write(lunit6, 7077)  lstat(14), flstat(15), flstat(16)
 7077 format( 5x,  92hthe last-read data card has been taken by the emtp
     1 to be a branch card for conductor number , i2,  5h of a   ,/,
     2 5x, 110hdistributed-parameter transmission line.   the user is in
     3 trouble because of an inconsistent line-length which  ,/,
     4 5x, 114hhe has punched in columns 45-50 of this last-read card.
     5 the first card for this multiphase line carries a length   ,/,
     6 5x,   3hof , e13.4,  41h, while the last-read card is for length
     7 , e13.4,  41h.   get it all together, guy, and correct   ,/,
     8 5x, 110hthis contradiction.   the easiest way is to put the corre
     9ct length on the first (zero-sequence) card only, and    )
      write(lunit6, 7177)
 7177 format( 5x, 112hthen leave columns 45-50 of the other cards blank.
     1   such a blank or zero length is interpreted by the emtp as a ,/,
     2 5x,  85hdefault value, meaning the same length as on the first ca
     3rd for the distributed line.    )
      go to 6220
 6078 write(lunit6, 7078 )  flstat(16)
 7078 format( 5x, 106hthe last-read data card is for the first conductor
     1 (or mode) of a distributed-parameter transmission line.   ,/,
     2 5x,  91hthe field of columns 45-50 is supposed to be punched with
     3 the line length, with a value of , e13.4,  5h read   ,/,
     4 5x, 114hby the emtp from the user's card.   thou shalt not input
     5transmission lines having nonpositive length, good buddy.   )
      go to 6220
 6079 write(lunit6, 7079)
 7079 format( 5x,  40hsince an interactive emtp version is not,
     1             36h being used, illegal data card shown,
     2             41h before "error" heading must be corrected
     3  ,/,  5x,   38hafter the program stops, and then case,
     4             38h must be re-run.  too bad (interactive,
     5             38h correction might have been possible).  )
      call stoptp
 6080 write(lunit6, 7080)  lbus, intinf
 7080 format (5x, 111hassociated with the steady-state phasor solution i
     1s a bias applied to node numbers of sources.   the bus tables  ,/,
     2 5x,  18hare dimensioned at, i6,  87h, which is over one third of
     3the largest integer permitted for this program conversion.   ,/,
     4 5x,  99hthe latter figure is stored in variable  'intinf' ,  whic
     5h for this conversion was given a value of, i6,  7h.   the   ,/,
     6 5x, 115hemtp wonders how in the hell the user can have so many bu
     7sses, or such short integers.   go see program maintenance   ,/,
     8 5x,  37habout this problem, fella, .... fast.   )
      go to 6220
 6081 write(lunit6, 7081)  flstat(15), flstat(16), tmax, deltat
 7081 format( 5x,  96hthe user's first miscellaneous data card implies a
     1 transient simulation of (in e-field notation), e15.6,   /,
     2 5x, 115hsteps.   this exceeds the limit  'intinf'  for integer nu
     3mbers for this program conversion of (in e-field notation),
     3 /, 1x, e15.6,  41h.   the case thus can not be run, because   ,/,
     4 5x,         104hwe can not count high enough to count the final t
     5ime step.   the aforementioned number of solution steps   ,/,
     6 5x,  92his calculated by dividing the user's  'tmax'  (columns 9-
     716 of the first misc. data card) of, e13.4,  8h seconds   ,/,
     8 5x,  61hby his time-step  'deltat'  (columns 1-8 of the same card
     9) of, e13.4,  35h.   if the user does not understand    )
      write(lunit6, 7181)
 7181 format( 5x, 101hthe error of his ways, he had better pay a call to
     1 his friendly neighborhood program maintenance man.   )
      go to 6220
 6082 write(lunit6, 7082)  lstat(15)
 7082 format( 5x, 103hthe user has overflowed storage within the line-co
     1nstants supporting program.   for the current program   ,/,
     2 5x,  48hversion, one is limited to cases having not over, i4,
     3  53h conductors.   storage for the arrays in question has   ,/,
     4 5x, 115hbeen optimally (and dynamically) allocated so as to use a
     5s much of the emtp core storage as is available.   it thus   ,/,
     6 5x, 115his not the fault of the line-constants supporting program
     7 itself, but rather of the overall emtp dimensioning, that   ,/,
     8 5x, 106hthis data case must be rejected.   go procure or make ano
     9ther program version which has more total tabular    )
      write (lunit6, 7182)
 7182 format ( 5x, 106hstorage space, and try the job over again.   reme
     1mber, the line constants calculation requires the storage   ,/,
     2 5x, 103hof full matrices, so memory requirements go up approximat
     3ely as the square of the number of conductors.   )
      go to 6220
 6083 write (lunit6, 7083)  lstat(15)
 7083 format (5x, 107hfor inputting conductor data of a line-constants c
     1ase, data-field  'xtype'  of columns 17-18 can be punched   ,/,
     2 5x, 115hwith a  '4' .   if this be the case, the emtp expects a s
     3pecification for the skin-effect calculation to be punched   ,/,
     4 5x, 114hin columns 4-8 (data field  'rtype' ).   but the user has
     5 left field  'rtype'  blank (or has punched a zero, which    ,/,
     6 5x,  91his no better), indicating that no skin-effect correction
     7is to be made for conductor number,  i3,  14h of the sorted  ,/,
     8 5x, 113hinput list.   this contradictory situation is not permitt
     9ed.   reconcile the data fields  'xtype'  and  'rtype' ,  )
      write (lunit6, 7183)
 7183 format ( 5x,  14hand try again.    )
      go to 6220
 6084 write (lunit6, 7084)  lstat(15)
 7084 format (5x, 106hthe just-printed conductor has geometrical x-y pos
     1itioning (i.e., horizontal distance and height) which is   ,/,
     2 5x,  39hidentical with that of conductor number, i3,  62h of the
     3sorted input.   thou shalt not place two or more solid    ,/,
     4 5x,  50hobjects in the same space at the same time, fella.   )
      go to 6220
 6085 write (lunit6, 7085)  lstat(15)
 7085 format (5x,  59hthe user has failed to input a conductor for circu
     1it number,  i3,  39h.   remember, circuit numbers must span   ,/,
     2 5x, 111hthe full range of numbers from unity through the highest
     3conductor number, without any gaps (unused numbers) in   ,/,
     4 5x,  50hbetween.   renumber the conductors, and try again.   )
      go to 6220
 6086 write (lunit6, 7086)  lstat(15), flstat(16), lstat(13), lstat(14)
 7086 format (5x, 110hgo see program maintenance about this one, fast.
     1 the case has stopped inside subroutine "redu44" of the line   ,/,
     2 5x,  51hconstants calculation, with diagonal element of row, i3,
     3   9h equal to, e14.5,  30h, which is less than hermann's   ,/,
     4 5x,  80hbuild-in fixed tolerance of  1.0e-8 .   parameters  'n'
     5and  'm'  are equal to , 2i5,  1h.   )
      go to 6220
 6087 write (lunit6, 7087)
 7087 format (5x,  38h big trouble.   the computer operating,
     1             39h system has interrupted emtp execution.,
     2             39h   this emtp solution is hereby fatally
     3   ,/,  5x,  37h wounded.   if more information about,
     4             35h the interrupt is known, it will be,
     5             38h found immediately preceding the first
     6   ,/,  5x,  38h "error  error ...."  line above.   if,
     7             35h this computer system can skip to a,
     8             38h following data case, it will (below).  )
      go to 6220
 6088 write (lunit6, 7088)  lstat(32), nenerg
 7088 format (5x, 107hthis is a statistical-overvoltage study, in which
     1all solutions are now completed, and the emtp is ready to   ,/,
     2 5x, 112hperform the final statistical processing (calculation of
     3cumulative distribution functions, etc.).   to do this,     ,/,
     4 5x,  20hthe output vectors (,  i3,  43h cells long) for all of th
     5e energizations (,  i3, 34h in number) must be simultaneously  ,/,
     6 5x, 110hstored in core.   this is the dominant demand for memory.
     7   there also is the need to store various vectors (a         )
      write (lunit6, 7288)   kswtch,  nenerg
 7288 format (5x,  85hlinear overhead).   total memory requirements are
     1given by the following formula ....         ,/,
     2 10x,  70hstorage  =  9*kswtch  +  (nenerg +4)*nvar  +  nc  +  nto
     3t  +  2*liminc            ,/,   5x,   10hwhere  ...   ,/,
     4 10x,   9hkswtch  =,  i4,  45h  =  number of switches in the emtp
     5data case            ,/,
     6 10x,    9hnenerg  =,  i4,  28h  =  number of energizations    )
      write (lunit6, 7388)   lstat(32), nc, ntot, lstat(14)
 7388 format ( 10x,   9hnvar    =,  i4,  64h  =  number of output variab
     1les for the energization simulations                         ,/,
     2 10x,   9hnc      =,  i4,  71h  =  number of branch output variabl
     3es for the energization simulations                          ,/,
     4 10x,   9hntot    =,  i4,  53h  =  total number of network nodes (
     5including ground)                                            ,/,
     6 10x,   9hliminc  =,  i4,  71h  =  maximum number of compartments
     7for any one statistical tabulation.                          )
      write (lunit6, 7488)   lstat(15),  lstat(16)
 7488 format ( 5x,  27hbut this requirement totals,  i7,
     1  37h   cells, which exceeds the available,  i7   )
      write (lunit6, 7188)
 7188 format (5x, 103hcells of working space.   to run this case without
     1 increasing the emtp dimensions, the user must either    ,/,
     2 5x, 112hdecrease the number of energizations, or decrease the num
     3ber of output quantities, or both.   alternatively, the    ,/,
     4 5x, 111huser may be willing to increase program dimensions, in wh
     5ich case any of the independent lists can be increased   ,/,
     6 5x,  73h(all are equally good, directly contributing to the stora
     7ge in question).     )
      go to 6220
 6089 write (lunit6, 7089)  flstat(15), flstat(16)
 7089 format (5x, 106hthe last-read card is a switch-card, bearing the k
     1ey word  'statistics'  in columns 55-64.   the switch is    ,/,
     2 5x, 110hthus a circuit-breaker pole, for which the closing time i
     3s to be a random variable.   but either the specified   ,/,
     4 5x, 113hvariable mean (punched in field  'tclose' ,   columns 15-
     524) or the standard deviation (field  'topen' ,  columns   ,/,
     6 5x,  84h25-34) has been punched as negative, which is not allowed
     7.   the two values read are, e14.5,    4h and, /,
     8 5x, e14.5,   15h, respectively.    )
      if ( lstat(15) .eq. 4433 )
     1 write (lunit6, 7189)  lstat(14)
 7189 format ( 5x, 35h====  correction  ====  the problem,
     1             36h switch in question is not the last-,
     2             34hread one, but rather switch number,  i5
     3   ,/,   5x, 35h                        in order of,
     4             35h input.  in fact, all switches have,
     5             18h been read by now.         )
      go to 6220
 6090 write (lunit6, 7090)  flstat(14), flstat(15)
 7090 format (5x, 103hthe emtp has just begun reading data for the conve
     1rsion of an rms current-voltage saturation curve into   ,/,
     2 5x, 112hcurrent-flux form.   but illegal values for either  'vbas
     3e'  or  'pbase'  have been read from the last-read data  ,/,
     4 5x, 108hcard (columns 9-16  and  17-24, respectively).   both of
     5these fields must be punched with positive numbers.   ,/,
     6 5x,  31hbut the emtp has read values of, e14.4,  4h and,
     7 e14.4,  35h for these variables, respectively.  )
 6220 lastov = nchain
      nchain = nfrfld + 50
      if ( iprsup  .ge.  1 )
     1 write ( lunit6, 4568 )
 4568 format ( 24h  "exit  module over52." )
99999 return
      end
      subroutine err87 ( lstat, flstat, lunit6 )
      implicit real*8 (a-h, o-z) ,
     1      integer*4 (i-n)
      dimension lstat(1), flstat(1)
      write (lunit6, 7087)
 7087 format (5x, 107has part of the interactive crt plotting or  'replo
     1t'  features of the emtp,  plot data points are stored on   ,/,
     2 5x, 111hdisk as a  'permanent file'  (as opposed to temporary, sc
     3ratch storage, which is used for every emtp run).   it   ,/,
     4 5x, 111his the operation of internally cataloging this data file
     5as a permanent file, inside the emtp, which has gotten   ,/,
     6 5x, 114hthe emtp into trouble with the computer operating system
     7(otherwise affectionately known as 'bigger big brother').     )
      return
      end
c
c     end of file: over52.for
c
