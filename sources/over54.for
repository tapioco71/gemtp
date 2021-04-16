c-*- mode: fortran; syntax: ansi-fortran-77; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
c
c     file: over54.for
c
c
c     subroutine over54.
c
      subroutine over54
      implicit real*8 (a-h, o-z) ,
     1      integer*4 (i-n)
      include  'blkcom.ftn'
      include  'volt45.ftn'
      if ( iprsup  .ge.  1 )
     1 write ( lunit6, 4567 )
 4567 format ( 24h  "begin module over54." )
      if ( nchain  .ne.  54 )   go to 99999
      n1 = kill - 150
      go to (
     1 6151, 6152, 6153, 6154, 6155, 6156, 6157, 6158, 6159, 6160,
     2 6161, 6162, 6163, 6164, 6165, 6166, 6167, 6168, 6169, 6170,
     3 6171, 6172, 6173, 6174, 6175, 6176, 6177, 6178, 6179, 6180,
     4 6181, 6182, 6183, 6184, 6185, 6186, 6187, 6188, 6189, 6190,
     5 6191, 6192, 6193, 6194, 6195, 6196, 6197, 6198, 6199, 6200), n1
 6151 write (lunit6, 7151)
 7151 format (5x, 104hthe present data case has a positive parameter  'n
     1energ'  (read from columns 65-72 of the floating-point        ,/,
     2 5x, 107hmiscellaneous data card), indicating that random switch c
     3losing times for  'statistics'  switches are to be           ,/,
     4 5x, 111hgenerated.   since parameter  'isw'  was punched with the
     5 key request value  '4444' ,   only an analysis of the       ,/,
     6 5x, 115hswitch closing times is desired (no emtp simulations are
     7 to be performed).   but such an analysis is not possible,     )
      write (lunit6, 7251)  lstat(14), lstat(15)
 7251 format (5x,  62hdue to a shortage of available working space in me
     1mory.   only, i6,  34h  integer cells of   /label/   are      ,/,
     2 5x,  16havailable, while, i7,  82h  such cells are required in or
     3der to execute this data case.   a formula for this          ,/,
     4 5x,  46hrequired minimum integer storage space is ....      ,/,
     5     30x,  32hmin  =  ( 3 + ns*(ns-1)/2 ) * nc               ,/,
     6 5x, 103hwhere  'ns'  is the number of  'statistics'  switches,  a
     7nd  'nc'  is the number of compartments of the                )
      write (lunit6, 7351)  lstat(16), lstat(13)
 7351 format (5x,  73hcumulative distribution function tabulation.   for
     1 this data case,   ns =, i4,  17h ,   while   nc =, i4, 2h ,,   /,
     2 5x, 113hwith the latter figure equal to twice parameter  'sigmax'
     3  divided by  'aincr'  (see  'statistics'  miscellaneous      ,/,
     4 5x, 117hdata card parameters).   in order to run this test of the
     5 random number generator, either the number of  'statistics'   ,/,
     6 5x, 109hswitches or tabulation compartments must be appropriately
     7 decreased, or the emtp must be re-dimensioned so as          )
      write (lunit6, 7451)
 7451 format (5x, 107hto provide for more working space in memory.   if
     1the latter course is taken, it is recommended that all of    ,/,
     2 5x, 110hincreased space be confined to lists number  5,  7,  8,
     313,  14,  or  15,  in which case there is no waste or        ,/,
     4 5x,  47hloss (the increase is  100 per cent effective).     )
      go to 6220
 6152 write (lunit6, 7152)  lstat(14)
 7152 format (5x, 109hexecution of the line constants program can not be
     1 allowed to continue, due to a shortage of core storage for    ,/,
     2 5x, 111huse as tabular working space.   the emtp is variably-dime
     3nsioned, of course, with the present size of   /label/       ,/,
     4 5x, 113hsimply insufficient to allow for even the solution of a s
     5ingle-conductor problem.   available dimensioned working      ,/,
     6 5x,  33hspace in subroutine  'vdov44'  is, i5,  66h  cells.   fit
     7ting all matrices into this constraint requires that          )
      write (lunit6, 7252)  lstat(15)
 7252 format (5x,  20ha conductor limit of, i5,  73h  conductors be impo
     1sed.   the user must redimension the emtp in order to         ,/,
     2 5x, 107hprovide for more   /label/   storage (with the distributi
     3on of this storage among the different lists being            ,/,
     4 5x,  34himmaterial), and resubmit the job.      )
      go to 6220
 6153 write (lunit6, 7153)  bus1
 7153 format (5x, 108has part of the tacs data which has now been comple
     1tely read by the emtp, the user has employed the same six-   ,/,
     2 5x,  30hcharacter alphanumeric name  ', a6,  66h'  for two differ
     3ent tacs variables.   unfortunately for the user,            ,/,
     4 5x, 109hhomographs are to be exiled for life from the emtp world.
     5   yet should this succinct characterization be less         ,/,
     6 5x, 111hthan perfectly clear, suffice it to say that it is a viol
     7ation of emtp rules for the user to apply the same  a6        )
      write (lunit6, 7253)
 7253 format (5x,  60hname to what are supposed to be two distinct tacs
     1variables.            )
      go to 7421
 7421 write (lunit6, 7521)
 7521 format (/, 5x, 105hsince the user is having trouble with 6-charact
     2er tacs variable names, it is perhaps worth qualifying the     ,/,
     3 5x, 114hpreceding error text which complains about an unidentifia
     4ble name that is associated with a certain tacs component    ,/,
     5 5x, 113hor data class.   all that is really involved here is a sp
     6elling comparison with other usages of the same variable     ,/,
     7 5x, 108hname.   when for some particular tacs component a given n
     8ame  'name1 '  can not be found in a table where it           )
      write (lunit6, 7621)
 7621 format (5x, 103hbelongs, the emtp says that the former is unrecogn
     1izable.   yet, as used with the component explicitely      ,/,
     2 5x, 113hmentioned in the message,   'name1 '   may in fact be spe
     3lled exactly as the user intended.   it may be the table     ,/,
     4 5x, 108hbeing searched which is in error, due to faulty spelling
     5of the variable name on some other data card (which     ,/,
     6 5x, 115hwas the usage that generated the table entry).   hence th
     7e user should look at other data cards for mis-spelling of   )
      write (lunit6, 7721)
 7721 format (5x, 107hthe name in question, if the spelling as printed i
     1n the above error text is actually as the user wanted it.     )
      write (lunit6, 7821)
 7821 format (/, 5x, 105hthen too, while talking about spelling, it migh
     1t be a good idea to emphasize what is involved.   all six-    ,/,
     2 5x, 113hcharacter variable names are  a6  fortran alphanumeric in
     3formation.   when printed out within error messages, the       ,/,
     4 5x, 110hsix characters in question are delineated by a leading an
     5d a trailing quotation or apostrophe mark.   position        ,/,
     6 5x, 111hof imbedded blanks is indeed crucial, then.   for example
     7,   'raver '  and  ' raver'  are completely different,        )
      write (lunit6, 7921)
 7921 format (   5x,  99hdistinct 6-character names, as far as the emtp
     1is concerned.   emtp names (including those of tacs)     ,/,
     2 5x, 111hconsist of an ordered string of 6 characters, with 'blank
     3' being a character like any other one.   two variable       ,/,
     4 5x, 109hnames are equal if and only if both characters of any cha
     5racter position are equal for the two names, for all    ,/,
     6 5x,  44hpossible character positions  1, 2, .... 6 .      )
      go to 6220
 6154 write (lunit6, 7154)
 7154 format (5x, 101hthe emtp has been inputting data for a  'semlyen s
     1etup'  case, and has discovered an inconsistency or          ,/,
     2 5x,  82hillegality in the numerical values which were punched by
     3the user.   specifically,            )
      write (lunit6, 7254)  lstat(13), lstat(14)
 7254 format (5x, 101hdata parameters  'iotx' ,  'ioss' ,  and  'iofl'
     1are required to be octal (base-8) numbers.   digits          ,/,
     2 5x,  96h '8'  and  '9'  are thus illegal (nonexistant), and must
     3not be used.   but the field for number,  i4,   8h   among    ,/,
     4 5x,  32hthese was punched with the value,  i6,  22h .   naughty,
     5naughty.     )
      go to 6220
 6155 write (lunit6, 7155)   lstat(13), lstat(14)
 7155 format (5x, 108hwithin the computations for a  'semlyen setup'  da
     1ta case, the iterative procedure which is used for fitting    ,/,
     2 5x, 105hthe characteristic-admittance step-response has failed to
     3 converge.   this fitting is done by means of an             ,/,
     4 5x, 113halgebraic adjustment in the frequency domain, not the mor
     5e well-known fitting of exponentials in the time domain.      ,/,
     6 5x,  22hthe iteration limit of,  i5,  54h   (parameter  'nitery'
     7)  was reached for mode number,  i4,  10h   without          )
      write (lunit6, 7255)  flstat(13), flstat(14)
 7255 format (5x,  65hsatisfying tolerances  'epsyc'  and  'epsn' ,   wh
     1ich have values,    e15.3,    6h   and,    e15.3,   2h ,     ,/,
     2 5x, 106hrespectively.   the user is reminded that these just-ment
     3ioned  'semlyen setup'  parameters are under user            ,/,
     4 5x, 111hcontrol by means of a special  'tolerances'  request-word
     5 card, if this will help any.   alternatively, did the       ,/,
     6 5x, 109huser perhaps already redefined default values by means of
     7 such a card, thereby making convergence impossible,         ,/,
     8 5x,  17hthe emtp wonders.     )
      go to 6220
 6156 write (lunit6, 7156)  lstat(14)
 7156 format (5x, 103hthe present   'semlyen setup'   data case has prov
     1en to be too much for the emtp logic, for mode number,  i4, 2h .)
      if ( lstat(19) .eq. 40080)  go to 7556
      if ( lstat(19) .eq. 4545 .or. lstat(19) .eq. 4557 ) go to 7956
      write (lunit6, 7256) lstat(15)
 7256 format (5x,103h the toe of the step response has been located, bas
     1ed on the speed of the highest frequency specified . ,/,5x,103h th
     2e time step used in the fitting process is established by a techni
     3que determined by parameter 'kfit' ,/, 5x, 43h for which the user
     4has supplied a value of , i2, 2h . ,/,
     6  5x,  49hif  "0"    the time step is simply toe time / noo  ,/,
     7  5x,  93hif  "1"    the time step is sized to put 25" of the spec
     8ified "npoint" points between the toe   ,/,  16x,  40hand the time
     9 at which the value is 2/3 . ,/,  5x, 114hif  "2"    the time step
     1 is sized to put  5" of the specified "npoint" points between the
     21/3 and the 2/3  times .  )
      write (lunit6, 7356) lstat(11)
 7356 format (  5x, 124hdetermining the 1/3 point or the 2/3 point may n
     1ot be possible because of noise at small times in inverse fourier
     2routines . ,/,  5x,  44hthis run has suffered from this difficulty
     3 . ,             5x, i4, 55h iterations have failed to locate one
     4of these points . ,/,  5x,  62hthe solutions available are "  "a"
     5 different value of "kfit", ,/,  5x,  80h"b" a higher number for "
     6nfit" which controls the permissible no. of iterations,  ,/, 5x, 1
     708h"c" a higher frequency range and/or more frequency points in an
     8 attempt to improve the accuracy of the ifr . )
      write ( lunit6, 7456 ) flstat(11), flstat(16), lstat(10)
 7456 format ( 5x,  74hother parameters relevent to this non-converged i
     1teration are as follows : ,/,  5x,  42hthe initial size of the tim
     2e increment was , e15.5,  24h .  the target value was , e15.5  ,/,
     3  5x,  56hthe number of points "npoint" as requested by the user =
     4, i4,  2h .  )
      go to 6220
 7556 write (lunit6, 7656)  lstat(13)
 7656 format ( 5x, 117hthe technique for fitting of the admittance step
     1response has been determined by the first column in parameter 'kfi
     2t' ,/, 5x,  42hfor which the user has supplied a value of,i2,2h .)
      write (lunit6, 7756)  flstat(16)
 7756 format (5x, 119hif '1', the user has requested a time domain fit,
     1which requires the program to determine the time period to be fitt
     2ed.  ,/, 5x,  85hin this process the program searches a limited ti
     3me frame for a change in response of , f4.2,  23h of the initial v
     4alue . ,/, 5x,  99hthe program has been unable to locate this valu
     5e, either because the period searched was too short, ,/, 5x,  48ho
     6r because the ift routine failed to produce it. ,/, 5x,  83ha wide
     7r frequency range or more numerous frequency data may improve the
     8ift result. ,/, 5x,  94ha value of '2' will result in a preliminar
     9y frequency domain fit to determine the time period. )
      write (lunit6, 7856) flstat(11),flstat(13),lstat(10)
 7856 format ( 5x,  64hother parameters relevant to this search failure
     1are as follows: ,/, 5x,  42hthe initial size of the time increment
     2 was ,e15.5,  30h .  the time zone searched was ,e15.5 ,/, 5x,  56
     3hthe number of points 'npoint' as requested by the user = , i4)
      go to 6220
 7956 write(lunit6,8056)
 8056 format( 5x,  93hthe newton-raphson algorithm, used to fit the line
     1 response with exponentials, is in trouble. ,/, 5x, 101heither the
     2 jacobian matrix is singular, or the initial guess made by the pro
     3gram is not close enough. ,/, 5x,  84hafter carefully checking dat
     4a, a consultation with program maintenance is indicated. )
      go to 6220
 6157 write (lunit6, 7157)
 7157 format (5x, 108hthe 'semlyen setup'  data case now under considera
     1tion requires that modal step responses be computed first,     ,/,
     2 5x,  75hby inverse fourier transformation.   this procedure has n
     3ow been completed.    )
      if ( lstat(14)  .eq.  0 )   go to 7457
      write (lunit6, 7257)  lstat(14), lstat(15),  flstat(14)
 7257 format (5x,  37hbut the step response for mode number,  i4,
     1 56h   is physically invalid.   in theory, this should begin   ,/,
     2 5x, 104hat zero, and should rise monatonically to unity, as time
     3goes to infinity.   but the value for the final              ,/,
     4 5x,  12hpoint number,  i5,  5h   is,    e14.3,     66h ,   which
     5is nowhere near unity.   in fact, this final value does      )
      write (lunit6, 7357)  flstat(15)
 7357 format (5x,  54hnot even reach the threshold  'fit2z' ,   which eq
     1uals,    e14.3,     31h .    the step response is thus       ,/,
     2 5x,  55hinvalid, and execution will not be allowed to continue. )
      go to 6220
 7457 write (lunit6, 7557)  lstat(16), lstat(15), lstat(17),
     1                      flstat(14), flstat(15), flstat(16)
 7557 format (5x,  25hbut between points number,  i6,  6h   and,  i6,
     1  46h ,   the rise to unity is not characterized by          ,/,
     2 5x,  63hmonotone-decreasing slope (concave downward).   at point
     3number,  i6,  37h ,   there is trouble,   let's denote       ,/,
     4 5x,  70hthis point by  'k' .   values for points  k-1, k, % k+1
     5then are  ...,     3e14.5,    3h  .        )
      go to 6220
 6158 write (lunit6, 7158)
 7158 format (5x, 103hthe data case under study involves the conversion
     1of emtp data cards for a saturable  'transformer'  to       ,/,
     2 5x, 107h(r), (l)  format (type-51, 52, ... ).   this is a feature
     3 of supporting routine  'xformer' .   but there is          ,/,
     4 5x,  89htrouble with the user's  'transformer'  data which is now
     5 being read.   specifically, ...     )
      n1 = lstat(14)
      if ( n1  .eq.  1 )
     1 write (lunit6, 7258)  bus5
 7258 format (8x, 109hthe emtp is just beginning a new conversion case,
     1and it expects the last-read data card (since non-blank) to  ,/,
     2 8x,  88hhave key word  'transformer'  punched in columns  3-14 .
     3  but columns  3-8  contain   ',  a6,  3h' .                )
      if ( n1  .eq.  2 )
     1 write (lunit6, 7358)
 7358 format (8x, 108hthe data card for winding number one can not be lo
     1cated.   column  2  must be punched with unity, of course,   ,/,
     2 8x, 114hand column  1  must be either blank or be punched with ze
     3ro.   reread the rules governing saturable  'transformer'   ,/,
     4 8x,  44hcomponent, and correct the data accordingly.       )
      if ( n1  .eq.  3 )
     1 write (lunit6, 7458)
 7458 format (8x, 106hprogram working space allows for a maximum of  19
     1 windings,   which has been exceeded by the user's data.      )
      go to 6220
 6159 write(lunit6,7159)  kill
 7159 format(5x, 19hinvalid kill code =, i5 )
      go  to  6220
 6160 write (lunit6, 7160)  lstat(14)
 7160 format(5x, 93hthe frequency card containing the number of decades
     1 and the number of frequencies per decade,/, 5x, 58hik and ips res
     2pectively, for the inverse fourier transform,/, 5x, 81hhas request
     3ed a total number of frequencies greater than the available storag
     4e of , i5,/, 5x, 85hsurely with a geometric progression of frequen
     5cies, this many steps is not necessary. )
      go to 6220
 6161 write (lunit6, 7161)
 7161 format (5x, 108h the last-read data card bears the special request
     1 word  'transformer '   in columns  3-14, and is the first     ,/,
     2 5x, 109hof several cards used to define a saturable transformer c
     3omponent.   but the user has failed to punch columns         ,/,
     4 5x, 112h39-44  (field  'bustop' )   with a non-blank 6-character
     5(a6) alphanumeric name.   this is the node name for the       ,/,
     6 5x, 114hextra, internal node of the saturable transformer compone
     7nt, at the top of the magnetizing branch (see transformer      )
      write (lunit6, 7261)
 7261 format (5x, 105hequivalent circuit in the emtp user's manual).   t
     1he data field  'bustop'  for each saturable transformer      ,/,
     2 5x, 112hcomponent must be non-blank, and all should be unique ---
     3 different from each other, and also different from all      ,/,
     4 5x,  25hother emtp network nodes.        )
      go to 6220
 6162 write (lunit6, 7162)  ipunch
 7162 format (5x, 108hthe user is running a  'line constants'  derivatio
     1n in which the emtp automatically loops over the different     ,/,
     2 5x, 110hfrequencies of interest.   on the frequency card which re
     3quested this mode of program execution, the field for        ,/,
     4 5x, 106hemtp variable  'ipunch'  (columns  66-68 ,   read using
     5i3  format)  contains an illegal numerical value.            ,/,
     6 5x, 101honly values  0,  1,  2,  3,  88,  and  89  are recognized
     7 as being legitimate, although a value of  ', i3,  6h'  was    )
      write (lunit6, 7262)
 7262 format (5x,  73hread from the user's frequency card.   correct thi
     1s value, and try again.             )
      go to 6220
 6163 write (lunit6, 7163)  lstat(14)
 7163 format (5x,  52hthe last-read data card is for a tacs source of ty
     1pe, i5,  50h (type code as read from columns 1-2 of the source ,/,
     2 5x, 111hcard).   but no valid emtp electric-network node names we
     3re specified by the user on either a preceding   'tacs       ,/,
     4 5x, 109hemtp sources'   card,  or on a preceding   'tacs outputs'
     5   card.   hence the emtp concludes that there is no         ,/,
     6 5x, 109helectric network, that the present problem being inputted
     7 is a tacs stand-alone case.   but tacs source types         )
      write (lunit6, 7263)
 7263 format (5x, 107h 90,  91,  etc.  all come from the electric-networ
     1k side of the emtp, by definition.   in sum, the user has     ,/,
     2 5x, 108hrequested that a tacs source be determined by a non-exist
     3ent part of the problem.   this discrepancy must be          ,/,
     4 5x,  56hcorrected, before execution will be allowed to continue.)
      go to 6220
 6164 write (lunit6, 7164)
 7164 format ( 5x,  15h  unused.                         )
      go to 6220
 6165 write (lunit6, 7165)
 7165 format(5x,  71hduring calculation of eigenvalues and eigenvectors
     1by the power method, ,/, 5x,  95hthe itteration count has exceeded
     2 "nieig".  this limit and the corresponding tolerance "epseig" ,/,
     3 5x,  38hmay be changed by a "tolerances" card. )
      go to 6220
 6166 write (lunit6, 7166)  csepar, chcont
 7166 format (5x, 108hthe last-read data card is assumed by the emtp to
     1have free-format data on it, due to the presence of one or    ,/,
     2 5x,  28hmore separator characters  ',  a1,
     3  38h'  or  continuation-card characters  ',  a1,
     4  42h' .   well, emtp control was within module             ,/,
     5 5x, 114h'frefld'  of utpf overlay  (main00, -1)  when the data ra
     6n out.   that is, control was looking for more data items   )
      write (lunit6, 7266)
 7266 format (5x, 110hthan have been punched on the card.   the user sho
     1uld first verify that the data card in question was intended  ,/,
     2 5x, 106hto be free-field data format.   then, he should check all
     3 data items, counting separator characters (which           ,/,
     4 5x,  33hdefine the number of data items).                  )
      go to 6220
 6167 n1 = iabs( lstat(14) )
      write (lunit6, 7167)  branch, bus4, n1
 7167 format (5x,  82hthe last-read data card has the  'bus3'  field of
     1columns  15-20  punched with   ',  a6,  17h' ,   so that the  ,/,
     2 5x, 112h'bus4'  field of columns  21-26  is to be the name of the
     3 branch.   but the 6-character text used for this name,  ,/, 5x,
     4 1h',  a6,  99h' ,   is not new and unique as required by emtp law
     5.   90 days in jail or a $100 fine.   row number,  i6,
     6   9h   of the   )
      if ( lstat(14)  .gt.  0 )
     1 write (lunit6, 7267)
 7267 format (5x,  77hlinear branch table contains a linear branch which
     1 has already been so named.    )
      if ( lstat(14)  .lt.  0 )
     1 write (lunit6, 7367)
 7367 format (5x, 103hnonlinear branch table contains a nonlinear or pse
     1udo-nonlinear branch which has already been so named.    )
      go to 6220
 6168 write (lunit6, 7168)
 7168 format ( 8h unused.  )
 6169 write (lunit6, 7169)
 7169 format ( 8h unused.  )
 6170 write (lunit6, 7170)  lstat(14)
 7170 format (5x, 108hthe last-read data card is a  'line constants'  fr
     1equency card which belongs to the interior data of another     ,/,
     2 5x, 114hemtp supporting program (e.g.,  'semlyen setup' ).   but
     3the emtp limit on such individually-specified frequencies     ,/,
     4 5x,   2his,  i5,  98h ,   which has been overflowed by the last-r
     5ead data card.   dimensioning of the crucial arrays in       ,/,
     6 5x, 111hquestion is not under user control.   a complete emtp rec
     7ompilation would be required in order to increase this    ,/,
     8 5x,  48hlimit, should this prove to be really necessary.     )
      go to 6220
 6171 write (lunit6, 7171)  xopt, copt
 7171 format (5x, 106hthe branch card now being processed represents a r
     1equest for semlyen recursive convolution modeling.   but     ,/,
     2 5x,  88hthe user specified floating-point miscellaneous data para
     3meters  'xopt'  and  'copt'  as,    e13.4,  6h   and         ,/,
     4 5x,    e13.4,  88h ,   respectively.   this is illegal.   both pa
     5rameters must be equal, and also nonzero.     )
      go to 6220
 6172 write (lunit6, 7172)
 7172 format (5x, 110hthe emtp has been reading data within the  'cable
     1constants'  supporting program, with the last-read data card   ,/,
     2 5x, 117hbeing the miscellaneous data card of a new case.   but th
     3e numbers punched hereupon are inconsistent.   specifically,    )
      write (lunit6, 7272)  lstat(14)
 7272 format (5x,  10ha value of, i5,  83h   was read from the field  'i
     1typec'  of columns  1  through  5  of the card, using        ,/,
     2 5x, 112h i5  format.   but only the values  1  (for overhead line
     3 constants) and  2  (for cable constants) are legal, if      ,/,
     4 5x, 115hthe card in question is nonblank (recall that a blank car
     5d marks the end of such cases within  'cable constants' ).     )
      go to 6220
 6173 write (lunit6, 7172)
      write (lunit6, 7173)
 7173 format (5x, 108hfield  'isyst'  of columns  6  through  10  has be
     1en punched with the integer value  -1 ,   which means that    ,/,
     2 5x, 108han underground cable system is to be under consideration.
     3   but field  'iearth'  of columns  16  through  20          ,/,
     4 5x, 110hhas been punched with the integer value  99 ,   which mea
     5ns that the stratified-earth option is desired.   but        ,/,
     6 5x, 107hthis combination of features is illegal (the emtp is inca
     7pable of finding the parameters for an underground           ,/,
     8 5x,  53hcable when the earth is not homogeneous and uniform).   )
      go to 6220
 6174 write (lunit6, 7174)
 7174 format (5x, 108hall conductor cards of an overhead-line case withi
     1n the  'cable constants'  supporting program have now been   ,/,
     2 5x, 106hread.   but one or more of these cards has been punched w
     3ith illegal or inconsistent data.   specifically,            )
      n1 = 3 * lstat(14) - 1
      write (lunit6, 7274)  n1, flstat(15), flstat(16)
 7274 format (5x,  23hnon-comment card number, i4,  74h  after the misce
     1llaneous data card bears geometrical conductor data which     ,/,
     2 5x,  78his physically impossible.   the inner radius of the condu
     3ctor has been read as,    e16.4,        16h   meters, while   ,/,
     4 5x,  19hthe outer radius is,    e16.4,     67h .    this violates
     5 the emtp restriction that the tubular conductor             ,/,
     6 5x,  72hthickness must be positive.   zero or negative thickness
     7is not allowed.        )
      go to 6220
 6175 write (lunit6, 7175)  lstat(15), flstat(14)
 7175 format (5x, 106hsubroutine  'eigen'  is used to calculate eigenval
     1ues of the   (z)(y)   matrix product, within the  'cable     ,/,
     2 5x,  67hconstants'  supporting program.   but within the iteratio
     3n limit of,  i5,   32h ,   the iterative algorithm has       ,/,
     4 5x,  46hfailed to satisfy the convergence tolerance of,
     5 e14.2,  39h .    the resulting eigenvalues must be          ,/,
     6 5x,  83hviewed with suspicion (at best), so the solution is being
     7 terminated at this point.    )
      go to 6220
 6176 write(lunit6,7176)
 7176 format(5x,  49herror is in synchronous machine electrical data. ,
     1 /,5x,      46hdata has one or more of the following errors  ,
     1 /,10x,     28h1. xl not smallest reactance ,
     1 /,10x,     28h2. xdpp not smaller than xdp ,
     1 /,10x,     27h3. xdp  not smaller than xd  ,
     1 /,10x,     28h4. xqpp not smaller than xqp ,
     1 /,10x,     27h5. xqp  not smaller than xq  ,
     1 /,10x,     44h6. one or more reactances or ra is negative ,/)
      go to 6220
 6177 write (lunit6, 7177)  numsm
 7177 format (5x, 102hthe emtp is presently in the process of reading em
     1tp source cards, with the last-read card producing a         ,/,
     2 5x, 104hviolation of the ordering restriction which is applicable
     3 when dynamic synchronous machine (s.m.) source              ,/,
     4 5x, 111hcomponents are present.   recall that all non-s.m. source
     5 cards must precede any s.m. source cards, in order of       ,/,
     6 5x,  37hdata input.   thusfar there have been, i4,   60h   s.m. s
     7ource components inputted (with an sce dual machine          )
      write (lunit6, 7277)  lstat(14)
 7277 format (5x, 109hcounting as just one source component), before the
     1last-read data card.   but the last data card has the field    ,/,
     2 5x,  74hof source type-code  'itype'  (columns 1-2) punched with
     3the integer value, i5,  25h ,   which is not a legal         ,/,
     4 5x, 110hcharacterization for a s.m. source component.   the last-
     5read data card is thus either erroneous by itself, or        ,/,
     6 5x,  63hout of order (it must precede the first s.m. source compo
     7nent).      )
      go to 6220
 6178 write (lunit6, 7178)  lstat(14)
 7178 format (5x, 109hthe emtp is presently inputting source components,
     1 with the last-read data card serving to define the network    ,/,
     2 5x, 109hconnection for the third and final phase of a 3-phase dyn
     3amic synchronous machine (s.m.) component.   but the         ,/,
     4 5x, 108hsource type-code for this component is illegal (unrecogni
     5zable).   from the field  'itype'  (columns 1-2) of          ,/,
     6 5x, 108hthe first data card for this s.m. component --- two non-c
     7omment cards before the last-read data card --- was          ,/,
     8 5x, 19hread a type code of,  i2,    83h .    but only s.m. model
     9types  51  through  54  and  59  are presently available.     )
      write (lunit6, 7278)   numsm
 7278 format (5x, 106hfurther, the user is allowed to use either machine
     1 types  51-54 ,   or type  59  ---- but not a mixture of     ,/,
     2 5x, 108hthe two.   recall that these two classes of s.m.  are qui
     3te different, utilizing different solution methods.          ,/,
     4 5x,  62hthe just-read dynamic  s.m.  emtp  source component was n
     5umber,  i4,  21h   in order of input.                        )
      go to 6220
 6179 write (lunit6, 7179)
 7179 format (5x, 108hthe data case under consideration has too many out
     1put quantities for the present emtp table sizes.   this is    ,/,
     2 5x, 109hreally an overflow of emtp list number  11 ,   though not
     3 due to conventional emtp electric-network branch or         ,/,
     4 5x, 110hnode quantities.   rather, it is the added burden of dyna
     5mic synchronous machine (s.m.) output variables, plus        ,/,
     6 5x, 103hthe burden of tacs output variables, which has now led to
     7 the present list-11 overflow.   figures which               )
      write (lunit6, 7279) lsiz12, nc, lstat(15), lstat(16), lstat(14)
 7279 format (5x,  48hcharacterize this difficulty are as follows ....,/
     1  ,5x,  i5, 43h = present user-dimensioned size of list 11    ,/,
     2 5x,  i5,  67h = number of conventional electric-network branch-ou
     3tput quantities                                              ,/,
     4 5x,  i5,  35h = number of s.m. output quantities            ,/,
     5 5x,  i5,  35h = number of tacs output quantities            ,/,
     6 5x,  34hthe last three figures above total,  i6,  70h ,   which i
     7s therefore the minimum acceptable size of list number 11.    )
      write (lunit6, 7379)
 7379 format (5x, 103heither increase the size of list 11 to at least th
     1is size, or appropriately reduce the number of output        ,/,
     2 5x,  72hquantities, before trying once again to solve the data ca
     3se in question.              )
      kill = 1
      lstat(16) = 11
      nchain = 51
      if ( iprsup  .ge.  1 )
     1 write ( lunit6, 4568 )
 4568 format ( 24h  "exit  module over54." )
      go to 99999
 6180 write(lunit6,7180) lstat(16), lstat(17)
 7180 format( 5x, 51hthe size of array 'voltbc', defined in blkcom to be
     2, i6, 1h,, 3x, 48his not adequate for the use of that array in the
     3, /, 5x,  64hprocessing of output requests for the type 59 s.m.  a
     4 minimum of, i5, 3x,18hcells is required.)
      go  to  6220
 6181 write(lunit6,7181) lstat(15),lstat(16), lstat(16)
 7181 format( 5x, 77hinconsistency discoverd on one of the mass cards .
     2the specified mass no. was, i5, 1h., 3x, 15hfor a s.m. with, i5,
     3  8 h  masses, /, 5x,  38hallowable range is between     1   and,
     4 i5,  9h  masses.)
      go  to  6220
 6182 write (lunit6, 7154)
      write (lunit6, 7182)  lstat(14), lstat(15)
 7182 format (5x, 104hthe number of frequency cards which are present in
     1 the previously-read line-constants data is incorrect.       ,/,
     2 5x,102hbefore the one which requests an automatic looping over lo
     3garithmically-spaced frequencies, there were, i4,            /,
     4 5x,    108hfrequency cards, each of which requests the line-const
     5ants calculation at an individual, discrete frequency.       ,/,
     6 5x,  77hbut the transposition flag (miscellaneous data parameter
     7 'nss'  )  had value,  i4,   23h .   these two integers        )
      write (lunit6, 7282)
 7282 format (5x, 108hshould add up to two.   but they do not.   remembe
     1r, for an untransposed line, there are to be two discrete-    ,/,
     2 5x,  62hfrequency cards, while a transposed line requires exactly
     3 one.    )
      go to 6220
 6183 j = lstat(13)
      write (lunit6, 7154)
      write (lunit6, 7183)  volti(j), j, ci1
 7183 format (5x, 104hthe frequency cards which are present in the previ
     1ously-read line-constants data are not all consistent.       ,/,
     2 5x, 110hthe emtp requires that all such cards apply to the same m
     3odel of the transmission line.   but it will be noted        ,/,
     4 5x,  28hthat an earth resistivity of,    e17.5,  49h   ohm-meters
     5 was read from frequency card number, i4,   10h ,   while     ,/,
     6 5x,     e17.5,      94h   was read from the first such card.   th
     7ese two resistivities are unequal, which is illegal.         )
      go to 6220
 6184 write (lunit6, 7154)
      write (lunit6, 7184)   lstat(13)
 7184 format (5x, 104hthe previously-read line-constants data cards are
     1illegally-structured for usage with  'semlyen setup' .       ,/,
     2 5x, 109hfirst, there should be the line-conductor cards, terminat
     3ed by a blank card.   then come the frequency cards,         ,/,
     4 5x, 104hwith only the last of these requesting the logarithmic fr
     5equency-looping option.   finally, a blank card              ,/,
     6 5x,  95hterminates the frequency cards, and a second blank card f
     7inishes the line constants data cards., 10x, 4hn1 =, i2 )
      go to 6220
 6185 write (lunit6, 7154)
      write (lunit6, 7185)   lstat(13), lstat(14)
 7185 format (5x,  39hthe emtp has previously read a value of,  i6,
     1  49h   (decimal) for the miscellaneous data parameter       ,/,
     2 5x,  82h 'iotx'  which controls printout.   but this exceeds the
     3legal meaningful limit of,  i6,  12h  (decimal).     )
      go to 6220
 6186 write (lunit6, 7154)
      write (lunit6, 7186)  voltbc(1), voltk(icheck)
 7186 format (5x, 107hfor purposes of  'semlyen setup'  usage, the frequ
     1encies for which line constants have just been calculated     ,/,
     2 5x, 112hare inappropriate.   recall that within the line-constant
     3s data, the frequency card for logarithmic looping over      ,/,
     4 5x, 112ha range of frequencies is to be preceeded by a frequency
     5card for the steady-state frequency.   the semlyen code      ,/,
     6 5x, 112hrequires that the beginning loop-frequency exceed the ste
     7ady-state frequency.   but such is not the case for the       ,/,
     8 5x,  22huser's data (values of,   e16.4,    6h   and,    e16.4,
     9     41h   were punched for these, respectively).        )
      go to 6220
 6187 write (lunit6, 7187)  bus3, bus4, bus5
 7187 format (5x, 105hthe last-read data card belongs to a  'semlyen set
     1up'  data case, but is not what the emtp was expecting.       ,/,
     2 5x, 108hfollowing the first semlyen miscellaneous data card, the
     3user must supply a card which bears either the text          ,/,
     4 5x, 110h 'line constants'   or the text   'cable constants' ,   p
     5unched in columns one onward.   not so for the user's        ,/,
     6 5x,  56hcard, the column 1-18 contents of which were read as   ',
     7       3a6 ,     33h' .    correct this card so as to     )
      write (lunit6, 7287)
 7287 format ( 5x, 101hproperly inform the emtp as to which supporting r
     1outine program control is to be transferred to next.      )
      go to 6220
 6188 d1 = flstat(13) / twopi
      d2 = flstat(15) / twopi
      write (lunit6, 7154)
      write (lunit6, 7188)  d1, flstat(14)
 7188 format (5x, 104hafter having completed the  'line constants'  or
     1'cable constants'  calculation, program control is now       ,/,
     2 5x, 114hback in the  'semlyen setup'  overlay where matrices  (r)
     3,  (x),  (g),  and  (b)  are being read from input/output      ,/,
     4 5x, 112hunit number  3 .   but the capacitance matrix is erroneou
     5s.   it should be independent of frequency, but is not.      ,/,
     6 5x,  49hin particular, the first diagonal entry  c(1,1) =,
     7    e20.8 ,       29h   farad/length for frequency,  e18.6     )
      write (lunit6, 7288)  d2, epsiln
 7288 format (5x,  61hhz,  while the  c(1,1)  value for the preceding fr
     1equency was,     e20.8 ,    24h .   it is required that      ,/,
     2 5x,  46hthese capacitances agree within a tolerance of,   e14.2,
     3    47h   (floating-point miscellaneous data parameter       ,/,
     4 5x, 108h 'epsiln' ).   for the user's data, this check has not be
     5en met.   the capacitance matrices are judged to be          ,/,
     6 5x,  46hinvalid, and the solution is being terminated.     )
      go to 6220
 6189 write (lunit6, 7189)  lstat(15)
 7189 format (5x, 106hthe emtp data case now being processed makes use o
     1f the tacs modeling capability, all data cards for which       ,/,
     2 5x, 110hhave now been read.   as the preceding printout shows, th
     3e vector of tacs variable names has repeated entries.         ,/,
     4 5x,   9hthere are,  i4,   94h   such conflicts.   the trouble can
     5 be traced back to the definition of tacs function blocks,    ,/,
     6 5x, 113htacs summers, and tacs supplemental variables and devices
     7.   all names assigned to such outputs must be distinct.      )
      write (lunit6, 7289)
 7289 format (5x, 106hfor example, if a tacs function block is given the
     1 name  'kontrl'  (read from columns 3-8 of the data card     ,/,
     2 5x, 108hdefining the block), then this same 6-character name had
     3better not be used for a tacs supplemental variable          ,/,
     4 5x, 56h(read from columns 3-8 of the variable-definition card). )
      go to 7421
 6190 write (lunit6, 7190)  lstat(14), bus6
 7190 format (5x, 109hthe emtp has been reading data cards which define
     1a dynamic synchronous machine (s.m.) component.   the last-    ,/,
     2 5x, 110hread card follows the s.m. output-request card, and prece
     3des the  'finish'  card|   having columns 1-2 punched        ,/,
     4 5x,  7hwith  ',  i2,   98h' ,   this card represents a request th
     5at the machine be controlled by tacs.    but either the a-6    ,/,
     6 5x,   7hname  ',  a6,  95h'  which was read from columns 3-8 is n
     7ot a legal tacs-variable name for this purpose, since it      )
      write (lunit6, 7290)
 7290 format (5x, 109hwas not also punched on a   'tacs outputs'   card
     1which began the input of tacs data.   before tacs variables    ,/,
     2 5x, 113hcan be used within the electric network, they must be dec
     3lared on a   'tacs outputs'   or a   'tacs emtp sources'     ,/,
     4 5x,   5hcard.   )
      write( lunit6, 7390 )  lstat( 15 )
 7390 format( 5x, 41hor... the number punched in columns 15-17,
     1 27h has an incorrect value of,, i5 )
      go to 6220
 6191 write (lunit6, 7191)
 7191 format (5x, 110hthe emtp finds the user's data case to be obnoxiou
     1sly degenerate, and refuses to continue with the simulation.   ,/,
     2 5x, 110hthe electric network has no sources and no dynamic synchr
     3onous machines, so only a natural (unforced) solution        ,/,
     4 5x, 110his called for.   but the user has failed to input any non
     5zero initial conditions.   hence the solution will be        ,/,
     6 5x,  86hidentically zero for all time.   there is no need to cont
     7inue with the solution, then.      )
      go to 6220
 6192 write (lunit6, 7192)
 7192 format (5x, 107hduring the list-building operation which is requir
     1ed for connectivity output, the temporary working vectors     ,/,
     2 5x, 110hof list 99 which are used have overflowed.   the proper s
     3olution is to redimension the emtp, though removal of        ,/,
     4 5x, 110hthe '1'-punch in field  'idoubl'  of the integer miscella
     5neous data card will bypass the present complication.        ,/,
     6 5x, 109hbut unless the user increases dimensions, he will probabl
     7y just be stopped shortly hereafter, in renumbering.         )
      write (lunit6, 7292)
 7292 format (5x, 109hboth computations use the same arrays, and have re
     1lated storage requirements.   lists number 5 % 8 contribute    ,/,
     2 5x,  36htotally to dependent list number 99.       )
      go to 6220
 6193 write (lunit6, 7193)  fminfs
 7193 format (5x, 108hthe last-read data card bears the key word   'freq
     1uency scan'   in columns 1-14.   this is a request for the     ,/,
     2 5x, 109hautomatic looping over steady-state phasor solutions only
     3, as the source frequency is methodically increased.         ,/,
     4 5x,  24ha beginning frequency of,    e15.4,      64h   was read f
     5rom the  'fmin'  field of columns 25-32 using  e8.0          ,/,
     6 5x, 115hformat, and must be positive (to be legal data).   the fr
     7equency increment  'delf'  of columns 33-40 was read using    )
      write (lunit6, 7293)  delffs, fmaxfs, lstat(14)
 7293 format (5x,  15he8.0  format as,    e15.4,     71h .    this must
     1be positive, unless data field  'log'  of columns 49-56      ,/,
     2 5x, 103h(see below) is punched positive.   the maximum (or end) f
     3requency  'fmax'  of columns 41-48 was read as,  e15.4       ,/,
     4 5x, 109h ,   which must exceed  'fmin' .   finally, integer field
     5  'log'  of columns 49-56 was punched with the value         ,/,
     6 5x, i5, 107h ,   which must not be negative.   the user's data ca
     7rd is illegal in that it violates one or more of these       ,/,
     8 5x,  22hjust-delineated rules.   )
      go to 6220
 6194 write (lunit6, 7194)  kconst, lstat(13), lstat(14)
 7194 format (5x, 103hthe data case now being read in includes a request
     1 for the  'frequency scan'  feature.   but the source        ,/,
     2 5x, 108hdata is inconsistent with this intended usage.   to be le
     3gal, there must be at least one type-14 emtp source          ,/,
     4 5x, 112hcomponent which is present during the steady-state phasor
     5 network solutions (as requested by punching data field      ,/,
     6 5x, 57h 'tstart'  of columns 61-70 negative).   but the user has,
     7 i5,  35h   emtp source components, of which,  i5,  6h   are   ,/,
     8 5x,  30hof type 14, of which (in turn),  i5,
     9  28h   have  'tstart'  negative.          )
      go to 6220
 6195 write (lunit6, 7195)  lstat(14), lstat(15)
 7195 format (5x, 102hthe emtp is in the process of inputting branch car
     1ds for a transmission circuit which is modeled using         ,/,
     2 5x,  89hametani linear convolution.   the last-read data card con
     3tains parameters for mode number,  i5,   9h .    but         ,/,
     4 5x,  72hthe integer which was read from columns  73-74  using  i2
     5  format is   ',  i2,  28h' .    this is the number of       ,/,
     6 5x, 105hlinear segments which represent the response ---- which i
     7s presently constrained to equal five.    re-run             ,/,
     8 5x,  94h 'ametani setup'   to get a valid new set of branch cards
     9 for the circuit, and then try again.      )
      go to 6220
 6196 write (lunit6, 7196)
 7196 format (5x, 103hemtp control is now in the   'ametani setup'   sup
     1porting routine, ready to transfer to either   'cable        ,/,
     2 5x, 111hconstants'   or   'line constants' .    but the last-read
     3 data card does not bear one of these key words on it,       ,/,
     4 5x,  31hbeginning in column number  1 .        )
      go to 6220
 6197 write (lunit6, 7197)   deltat, flstat(14)
 7197 format (5x,  49hthe emtp is now inputting data for a transmission,
     1             36h circuit that is being modeled using   ,/,
     2 5x,  96hfrequency dependent representation.  but for this data, t
     3he present time-step size  'deltat'  of, e15.4, 10h sec    is  ,/,
     4 5x,  61htoo large.   the  travel time of the current mode is equa
     7l to, e15.4,  45h sec ,    which must exceed  'deltat' .   the ,/,
     8 5x,  73htime-step size of the study must be decreased to satisfy
     9this constraint.        )
      go to 6220
 6198 write (lunit6, 7198)  lstat(16), lstat(15)
 7198 format (5x, 109hthe emtp is in the middle of solving a    'semlyen
     1 setup'    data case, at which point it has been discovered    ,/,
     2 5x, 112hthat insufficient working space exists.    the   'line co
     3nstants'   or   'cable constants'   calculation has now      ,/,
     4 5x,  59hbeen successfully completed, and a minimum working space
     5of,  i7,   39h    floating-point cells is now a known        ,/,
     6 5x, 103hrequirement for the completion of   'semlyen setup'   pro
     7cessing.   but module  'vdov45'  only contains,  i7          )
      write (lunit6, 7298)
 7298 format (5x, 108hfloating-point cells of such working space.   the
     1emtp must be re-dimensioned by the user, so as to increase   ,/,
     2 5x, 113hthe size of   /label/   by the just-indicated shortfall.
     3  distribution of total storage among the different emtp     ,/,
     4 5x, 103hlists is immaterial in this case (as with the dimensionin
     5g of all primary-level non-solution overlays).      )
      go to 6220
 6199 write (lunit6, 7199)  flstat(15), lstat(14), flstat(13), lstat(13)
 7199 format (5x, 107hthe present   'semlyen setup'   data case has brok
     1en down in the middle of a matrix inversion operation for    ,/,
     2 5x,   9hfrequency,    e14.5,    67h    hertz.   the eigenvector (
     3modal transformation) matrix of order,  i5,   7h    has      ,/,
     4 5x,   9hthe value,     e13.3,    56h    for the largest possible
     5pivot element of row number,   i5,   19h .    but this does    )
      write (lunit6, 7299)  flstat(14)
 7299 format ( 5x,  64hnot exceed the near-zero tolerance  'epspv2' ,
     1which has value,    e13.3,     20h .    execution must       ,/,
     2 5x, 107hbe stopped immediately.   yet, the user is reminded that
     3he can redefine  'epspv2'  by means of an optional           ,/,
     4 5x,  52h 'tolerances'   card (read by   'semlyen setup'  ) .  )
      go to 6220
 6200 write (lunit6, 7200)
 7200 format (5x, 103hmemory-overflow problem, before entry into the ren
     1umbering overlay (for transient network renumbering).        ,/,
     2 5x, 112hrecall that space for renumbering comes from a major port
     3ion of   /label/ .    three vectors are used, with size      ,/,
     4 5x, 114hgiven by list number  99  of the case-summary statistics.
     5   this space is insufficient even for the simple storage    ,/,
     6 5x,  98hof the connectivity of  (y) --- to say nothing of the sim
     7ulation of fillin upon triangularization.                    )
      if ( lstat(14)  .gt.  0 )
     1 write (lunit6, 7300)  ibr, lstat(14)
 7300 format (5x,  6hof the,  i6,
     1  36h   entries of the branch table, only,  i6,
     2  58h   were inserted into the working storage before overflow. )
      if ( lstat(14)  .eq.  0 )
     1 write (lunit6, 7400)
 7400 format (5x, 108hall branch-table entries were successfully inserte
     1d into the working storage, but entries from the nonlinear   ,/,
     2 5x,
     3  58helement table and the switch table then produced overflow. )
      write (lunit6, 7500)
 7500 format (5x, 108has for redimensioning of the emtp, lists  5  and
     18  contribute  100  per cent to the size of dependent list   ,/,
     2 5x,  68h99.   increase one of these list sizes substantially, and
     3 try again.         )
 6220 lastov = nchain
      nchain = nfrfld + 50
      if ( iprsup  .ge.  1 )
     1 write ( lunit6, 4568 )
99999 return
      end
c
c     end of file: over54.for
c
