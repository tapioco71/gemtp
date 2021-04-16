c-*- mode: fortran; syntax: ansi-fortran-77; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
c
c     file: blockdspy.for
c
      block data blkhlp
      include 'dekspy.ftn'
      data texspy (   1 ) / 'key word no.  1:  "heading"     ----  ----  ----                                '  /
      data texspy (   2 ) / '  response will be the printing of the previously-defined heading of  "examine",'  /
      data texspy (   3 ) / '  followed by current values of all variables (as for the 1st  "examine"  use). '  /
      data texspy (   4 ) / 'key word no.  2:  "stop"        ----  ----  ----                                '  /
      data texspy (   5 ) / '  this command will terminate interactive EMP execution immediately,  by  means '  /
      data texspy (   6 ) / '  of a fortran "stop" statement.  There will be no automatic saving  of  tables,'  /
      data  texspy (   7 )   /  '  or  of plot data points before such a
     1termination  (use prior  "sleep"  and/or'  /
      data  texspy (   8 )   /  '  "lunit4"  commands, if such preservat
     1ion is desired).                         '  /
      data  texspy (   9 )   /  'key word no.  3:  "plot"        ----  -
     1---  ----                                '  /
      data  texspy (  10 )   /  '  issue this command to transfer contro
     1l to the  "outer:"  prompt of interactive'  /
      data  texspy (  11 )   /  '  emtp plotting (the former separate em
     1tp crt plotting program  "tpplot").   due'  /
      data  texspy (  12 )   /  '  to the absorbtion into spy,  several
     1changes have been made.  first,  no plot-'  /
      data  texspy (  13 )   /  '  file specification is required of the
     1user  (he should subsequently send  "go"'  /
      data  texspy (  14 )   /  '  if no other outer-level response is d
     1esired).   at the  "middle:"  level,  the'  /
      data  texspy (  15 )   /  '  "timespan"  computation is now automa
     1tically performed,  internally.   at  any'  /
      data  texspy (  16 )   /  '  level,  "stop"  no longer terminates
     1program execution, but instead it returns'  /
      data  texspy (  17 )   /  '  control to the  "spy:"  prompt.   con
     1tinuous,  automatic plotting  of  the on-'  /
      data  texspy (  18 )   /  '  going solution  (like a strip-chart)
     1is  based  on the use of either  "rollc"'  /
      data  texspy (  19 )   /  '  (for character plotting)  or  "rollv"
     1(for vector plotting)  as  commands  at'  /
      data  texspy (  20 )   /  '  the  "inner:"  level.   for  a  detai
     1led explanation of all  "plot"  commands,'  /
      data  texspy (  21 )   /  '  send  "help"  at any of the three lev
     1els within  "plot".                      '  /
      data  texspy (  22 )   /  'key word no.  4:  "help"        ----  -
     1---  ----                                '  /
      data  texspy (  23 )   /  '  education for the ignorant, such as t
     1he user is now being subjected to (joke).'  /
      data  texspy (  24 )   /  '  a  carriage return  <cr>  will  produ
     1ce text for the next key word  in  order,'  /
      data  texspy (  25 )   /  '  while  a  return  to the  "spy:"  pro
     1mpt is accomplished by  "spy", or  "end".'  /
      data  texspy (  26 )   /  '  "all"  will loop over  all  explanati
     1ons  (subject  to  user-keyed interrupt).'  /
      data  texspy (  27 )   /  '  sending  "top"  will rewind to the fi
     1rst message, while  "bot"  will give the '  /
      data  texspy (  28 )   /  '  last.   use  "back"  to back up one m
     1essage.                                  '  /
      data  texspy (  29 )   /  'key word no.  5:  "examine"     ----  -
     1---  ----                                '  /
      data  texspy (  30 )   /  '  issue this command  to  examine the c
     1ontents of any emtp common variables  (of'  /
      data  texspy (  31 )   /  '  solution overlays).   integer  scalar
     1s require 6 columns;  all other variables'  /
      data  texspy (  32 )   /  '  require 15.   subsequent prompts will
     1allow  the  user  to specify scalars and'  /
      data  texspy (  33 )   /  '  vector ranges (e.g.,  "kbus(3:8)"  fo
     1r  cells  3  through  8 of kbus).   "end"'  /
      data  texspy (  34 )   /  '  terminates the list,  resulting  in a
     1display of heading and numerical values.'  /
      data  texspy (  35 )   /  '  any later striking of the  "return"
     1key  will  then display current numerical'  /
      data  texspy (  36 )   /  '  values only.   send  "heading"  for
     1a  refresh of the variable names.  for  a'  /
      data  texspy (  37 )   /  '  rolling display,  send  "roll"  (also
     1,  see  separate  instructions  for  this'  /
      data  texspy (  38 )   /  '  command).  for  rolling, the  output
     1vector  is  re-formed at each spy chance,'  /
      data  texspy (  39 )   /  '  but is only output when one or more v
     1ariables has changed.  to  terminate  the'  /
      data  texspy (  40 )   /  '  roll-loop,  use the regular user-keye
     1d interrupt.                             '  /
      data  texspy (  41 )   /  'key word no.  6:  "deposit"     ----  -
     1---  ----                                '  /
      data  texspy (  42 )   /  '  issue this command to modify the cont
     1ents of any  emtp  common  variables  (of'  /
      data  texspy (  43 )   /  '  solution overlays).   subsequent prom
     1pts  will  permit  the  user  to  specify'  /
      data  texspy (  44 )   /  '  scalars  and vector ranges  (e.g.,  "
     1kbus(3:8)"  for  cells  3  through  8  of'  /
      data  texspy (  45 )   /  '  kbus).   "end"  terminates the list,
     1returning to the  "spy:"  prompt.  after'  /
      data  texspy (  46 )   /  '  each  variable,  there will be a prom
     1pt for the desired new value (free-format'  /
      data  texspy (  47 )   /  '  number),  if no  "="  is used.   the
     1separately-prompted input  is  rigorously'  /
      data  texspy (  48 )   /  '  free-format  (so  that any  i, f, or
     1e-field number is permissible).  but  for'  /
      data  texspy (  49 )   /  '  simple numeric values which  can  be
     1read  using  f15.0,  or  for  text (a6),'  /
      data  texspy (  50 )   /  '  follow the scalar or vector by an equ
     1al sign and then the number.  the case of'  /
      data  texspy (  51 )   /  '  alphanumeric  must  not  have imbedde
     1d blanks after the  "=",  but numbers can'  /
      data  texspy (  52 )   /  '  (within the span of 15 columns).
     1'  /
      data  texspy (  53 )   /  'key word no.  7:  "switch"      ----  -
     1---  ----                                '  /
      data  texspy (  54 )   /  '  this response to the  "spy:"  prompt
     1is  issued  for  a  display  of the emtp'  /
      data  texspy (  55 )   /  '  switch  table.   subsequently  send
     1an  additional  "extra"  to change to the'  /
      data  texspy (  56 )   /  '  next sub-table (as of february 1984,
     1there are two),  if different columns are'  /
      data  texspy (  57 )   /  '  desired.  within any one choice as to
     1sub-table,  there is a loop in which the'  /
      data  texspy (  58 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      data  texspy (  59 )   /  '  or  a  key word.  recognized key word
     1s here  are  "all"  for the entire table,'  /
      data  texspy (  60 )   /  '  "top"  for row one,  "bot"  for the
     1last  row,  "next"  or  just  a  carriage'  /
      data  texspy (  61 )   /  '  return for the following row, and of
     1course  "spy"  to  abort the display loop'  /
      data  texspy (  62 )   /  '  and return to the  "spy:"  prompt.  t
     1o refresh the heading, use either  "swit"'  /
      data  texspy (  63 )   /  '  or  "head"  (only 4 characters of "sw
     1itch" or "heading" are checked).         '  /
      data  texspy (  64 )   /  'key word no.  8:  "append"      ----  -
     1---  ----                                '  /
      data  texspy (  65 )   /  '  this command is the gateway to instal
     1lation-dependent commands which are non- '  /
      data  texspy (  66 )   /  '  standard,  and which have been instal
     1led locally.   the  utpf carries a dummy '  /
      data  texspy (  67 )   /  '  subroutine append,  by  definition.
     1one  clean  way  of adding a functional '  /
      data  texspy (  68 )   /  '  module is at translation time,  using
     1a  submod  request.                     '  /
      data  texspy (  69 )   /  'key word no.  9:  "save"        ----  -
     1---  ----                                '  /
      data  texspy (  70 )   /  '  when the emtp is in the time-step loo
     1p, this command will force an exit to    '  /
      data  texspy (  71 )   /  '  overlay 20 upon completion of present
     1time-step.   there,  "katalg"  saves    '  /
      data  texspy (  72 )   /  '  emtp tables as rapidly as possible (m
     1ay not be permanent).   the simulation   '  /
      data  texspy (  73 )   /  '  then recommences by an automatic tran
     1sfer back to the beginning of the        '  /
      data  texspy (  74 )   /  '  time-step loop ("over16").   since pl
     1ot data points are not part of the       '  /
      data  texspy (  75 )   /  '  saving, if the user wants these to be
     1preserved, too, he must be careful.     '  /
      data  texspy (  76 )   /  '  provided emtp storage for plot data p
     1oints does not fill up before the        '  /
      data  texspy (  77 )   /  '  subsequent  restore  operation (to ba
     1ck up time to the  save  point), there   '  /
      data  texspy (  78 )   /  '  is no problem.  otherwise, consider u
     1se of  "space"  and  "lunit4"  commands. '  /
      data  texspy (  79 )   /  'key word no. 10:  "restore"     ----  -
     1---  ----                                '  /
      data  texspy (  80 )   /  '  the  "restore"  command is the revers
     1e of  "save".  when within the deltat    '  /
      data  texspy (  81 )   /  '  loop, use of  "restore"  will rapidly
     1transfer to overlay 20.  there, module  '  /
      data  texspy (  82 )   /  '  "katalg" restores former tables, afte
     1r which control is transferred to spy.   '  /
      data  texspy (  83 )   /  '  possibly the  "space"  and  "lunit4"
     1would also be appropriate, to restore   '  /
      data  texspy (  84 )   /  '  any separately plot files.   also aft
     1er tables have been restored, the user   '  /
      data  texspy (  85 )   /  '  can modify any emtp variables  desire
     1d,  and exit  "deposit"  with  "end";    '  /
      data  texspy (  86 )   /  '  finally, send  "go"  in response to t
     1he  "spy:"  prompt, to transfer back to  '  /
      data  texspy (  87 )   /  '  the top of the time-step loop ("over1
     16").                                     '  /
      data  texspy (  88 )   /  'key word no. 11:  "go"          ----  -
     1---  ----                                '  /
      data  texspy (  89 )   /  '  this command is issued in response to
     1the  "spy:"  prompt to terminate        '  /
      data  texspy (  90 )   /  '  several sequences, such as one which
     1might have begun with  "restore".  in    '  /
      data  texspy (  91 )   /  '  this case, there would be the precedi
     1ng transfers of control from overlay 16  '  /
      data  texspy (  92 )   /  '  to overlay 20 ("katalg"), and then ba
     1ck to overlay 16 again.  as a second     '  /
      data  texspy (  93 )   /  '  example,  "go"  cancels the  "rest"
     1command.  third, it can be used to       '  /
      data  texspy (  94 )   /  '  begin the emtp solution following the
     1"data"  command (for data card input   '  /
      data  texspy (  95 )   /  '  at the beginning of program execution
     1).   finally,  "go"  cancels the         '  /
      data  texspy (  96 )   /  '  suspension   of execution which accom
     1panies the  "sleep"  command.            '  /
      data  texspy (  97 )   /  'key word no. 12:  "echo"        ----  -
     1---  ----                                '  /
      data  texspy (  98 )   /  '  users who are interested in keeping a
     1history of spy commands which are to    '  /
      data  texspy (  99 )   /  '  be issued can use the  "echo"  comman
     1d.   a subsequent prompt then allows     '  /
      data  texspy ( 100 )   /  '  several choices.  to begin such accum
     1ulation, send  "begin";  to end it,      '  /
      data  texspy ( 101 )   /  '  send  "file",  which will result in t
     1he dumping of all accumulation onto      '  /
      data  texspy ( 102 )   /  '  disk as a permanent file of the users
     1choice (a subsequent prompt will        '  /
      data  texspy ( 103 )   /  '  ask for the desired file name).   the
     1history consists of 80-column card      '  /
      data  texspy ( 104 )   /  '  images,  stored from the bottom of  f
     1ile6  upward  (with cell  kspsav         '  /
      data  texspy ( 105 )   /  '  storing the last, and  limcrd  storin
     1g the first).  to view the accumulation  '  /
      data  texspy ( 106 )   /  '  to date, use  "show".   when  "file"
     1is used,  not only is a copy sent to    '  /
      data  texspy ( 107 )   /  '  disk, but those in memory are erased.
     1hence, if  echoing  is to continue,   '  /
      data  texspy ( 108 )   /  '  the user must send  "begin"  again,
     1immediately after  "file"  is complete.  '  /
      data  texspy ( 109 )   /  '  in order to erase an erroneous, immed
     1iately-preceding command, use  "cancel"  '  /
      data  texspy ( 110 )   /  '  at any point.  this is intercepted by
     1the input routine  "flager",  so is     '  /
      data  texspy ( 111 )   /  '  not a spy command per se.   there wil
     1l be confirmation of the erasure.        '  /
      data  texspy ( 112 )   /  'key word no. 13:  "find"        ----  -
     1---  ----                                '  /
      data  texspy ( 113 )   /  '  the sending of  "find"  will result i
     1n the printing of a heading, followed    '  /
      data  texspy ( 114 )   /  '  by a pause, as  spy  waits for the us
     1er to supply a 6-character emtp symbol   '  /
      data  texspy ( 115 )   /  '  name.   this is a closed loop: after
     1receiving a name,  spy  will display     '  /
      data  texspy ( 116 )   /  '  the memory address, and then wait for
     1another such symbol.   exit by  "end".  '  /
      data  texspy ( 117 )   /  '  wild cards ("*") of  vax/vms  are hon
     1ored here, although the candidate        '  /
      data  texspy ( 118 )   /  '  string is limited to eight characters
     1maximum (only the first 8 are read).    '  /
      data  texspy ( 119 )   /  'key word no. 14:  "list"        ----  -
     1---  ----                                '  /
      data  texspy ( 120 )   /  '  this command will result in the print
     1ing of a heading, followed by a pause.   '  /
      data  texspy ( 121 )   /  '  at this point the user is in a loop,
     1supplying row numbers (beginning and     '  /
      data  texspy ( 122 )   /  '  ending rows as a pair of free-format
     1integers).   spy responds to each with   '  /
      data  texspy ( 123 )   /  '  a display of those rows of the spy sy
     1mbol table, and then waits for the next  '  /
      data  texspy ( 124 )   /  '  request.   the sending of  "0,0"  wil
     1l break out, returning to the  "spy:"    '  /
      data  texspy ( 125 )   /  '  prompt.   the user can interrupt any
     1excessively long display with his keyed  '  /
      data  texspy ( 126 )   /  '  interrupt (details depend upon comput
     1er).  instead of  "0,0"  to break out,   '  /
      data  texspy ( 127 )   /  '  "end"  or  "spy:"  can alternatively
     1be used.  sending nothing (just a        '  /
      data  texspy ( 128 )   /  '  carriage return <cr>) is interpreted
     1by spy as a request for "more of the     '  /
      data  texspy ( 129 )   /  '  same" ---- the same number of rows as
     1just displayed, beginning where the     '  /
      data  texspy ( 130 )   /  '  last display left off.   special tric
     1kery is required if an argument of  "@"  '  /
      data  texspy ( 131 )   /  '  usage is to respond to the  "list"  p
     1rompt,  since a comma must not be used   '  /
      data  texspy ( 132 )   /  '  as the free-format separator (due to
     1confusion with use of the same symbol    '  /
      data  texspy ( 133 )   /  '  for argument separation by "@").  ins
     1tead, a pounds sign  "#"  must be used   '  /
      data  texspy ( 134 )   /  '  rather than a blank (due to extractio
     1n of blanks by  "@"  logic).  finally,   '  /
      data  texspy ( 135 )   /  '  sending  "all"  instead of two row nu
     1mbers displays the whole table.          '  /
      data  texspy ( 136 )   /  'key word no. 15:  "spy"         ----  -
     1---  ----                                '  /
      data  texspy ( 137 )   /  '  this text, supplied almost anywhere t
     1hat the program is looking for text,     '  /
      data  texspy ( 138 )   /  '  will cause an internal interruption o
     1f whatever was happening, and a return   '  /
      data  texspy ( 139 )   /  '  to the  "spy:"  prompt.   one excepti
     1on is the  "help"  prompt which is now   '  /
      data  texspy ( 140 )   /  '  being serviced.
     1'  /
      data  texspy ( 141 )   /  'key word no. 16:  "break"       ----  -
     1---  ----                                '  /
      data  texspy ( 142 )   /  '  this response to the  "spy:"  prompt
     1is appropriate if the user wants the     '  /
      data  texspy ( 143 )   /  '  simulation to continue uninterrupted
     1until some pre-specified time, when a    '  /
      data  texspy ( 144 )   /  '  clean break at the start of "subts1"
     1will be made.  a subsequent prompt will  '  /
      data  texspy ( 145 )   /  '  allow the user to specify the future
     1break time  t-break  in seconds.  if a   '  /
      data  texspy ( 146 )   /  '  minus sign is appended, then the inpu
     1t is taken to be a step number of the    '  /
      data  texspy ( 147 )   /  '  time-step loop, and the program will
     1calculate  t-break   by multiplying by   '  /
      data  texspy ( 148 )   /  '  deltat.  oh, in case "subts1" means n
     1othing to the user, this is the first of '  /
      data  texspy ( 149 )   /  '  four pieces of overlay 16 (the time-s
     1tep loop).  in case the user sends zero  '  /
      data  texspy ( 150 )   /  '  for  t-break, an added prompt will se
     1ek clarification as to which utpf overlay'  /
      data  texspy ( 151 )   /  '  is the desired stopping point (immedi
     1ately prior to entry).  common usage     '  /
      data  texspy ( 152 )   /  '  involves overlays numbered 6  (after
     1all sources have been read),  12  (when  '  /
      data  texspy ( 153 )   /  '  the   phasor solution is complete), o
     1r  16  (the time-step loop).             '  /
      data  texspy ( 154 )   /  'key word no. 17:  "when"        ----  -
     1---  ----                                '  /
      data  texspy ( 155 )   /  '  this response to the  "spy:"  prompt
     1will redefine the emtp table-handling    '  /
      data  texspy ( 156 )   /  '  time of  "save"  and  "restore" .   t
     1hat is,  if so defined,  this overrides  '  /
      data  texspy ( 157 )   /  '  the otherwise immediate exit of the t
     1ime-step loop for table-handling.        '  /
      data  texspy ( 158 )   /  'key word no. 18:  "comment"     ----  -
     1---  ----                                '  /
      data  texspy ( 159 )   /  '  this response to the  "spy:"  prompt
     1will toggle the switch which controls the'  /
      data  texspy ( 160 )   /  '  printing of comment cards ("c ") whic
     1h may be contained within command files. '  /
      data  texspy ( 161 )   /  '  the default (starting value) is to sh
     1ow comment cards during execution ("@"). '  /
      data  texspy ( 162 )   /  'key word no. 19:  "@?"          ----  -
     1---  ----                                '  /
      data  texspy ( 163 )   /  '  this response to the  "spy:"  prompt
     1will result in the internal  opening  of '  /
      data  texspy ( 164 )   /  '  an arbitary disk file, and the connec
     1tion of this file as replacement for     '  /
      data  texspy ( 165 )   /  '  keyboard input to spy.  if the file n
     1ame consists of just a single digit      '  /
      data  texspy ( 166 )   /  '  (?=1-9),  inclspy?.dat  is the disk f
     1ile name to be used.  reading from the   '  /
      data  texspy ( 167 )   /  '  disk file continues until an end-of-f
     1ile is hit, at which point the spy input '  /
      data  texspy ( 168 )   /  '  channel is connected once again to th
     1e keyboard.  such usage can not be nested'  /
      data  texspy ( 169 )   /  '  (i.e., no such disk file can itself c
     1ontain an  "@"  statement).   emtp       '  /
      data  texspy ( 170 )   /  '  comment cards ("c ") are permitted wi
     1thin such disk files, however,  with the '  /
      data  texspy ( 171 )   /  '  "comment"  switch controlling whether
     1or not they are written to the screen   '  /
      data  texspy ( 172 )   /  '  (the default is for such writing).
     1other lines of an executed disk file are '  /
      data  texspy ( 173 )   /  '  generally not seen by the user as the
     1y are read during execution, and neither '  /
      data  texspy ( 174 )   /  '  are the repetitive  "spy:"  prompts.
     1parameters are possible, to substitute  '  /
      data  texspy ( 175 )   /  '  for 8-column   "%%%%%%%%"   fields of
     1the disk file.   blanks are ignored,    '  /
      data  texspy ( 176 )   /  '  and arguments are to be separated by
     1commas.  an opening parenthesis  "("  and'  /
      data  texspy ( 177 )   /  '  a closing parenthesis  ")"  are optio
     1nal delimiters.   each command line is   '  /
      data  texspy ( 178 )   /  '  limited to 80 columns (no continuatio
     1n), and a maximum of 10 arguments.   the '  /
      data  texspy ( 179 )   /  '  left-to-right list is applied to the
     1file %-fields from top to bottom, in     '  /
      data  texspy ( 180 )   /  '  order.  the %-fields can be built int
     1o any line of the disk file which        '  /
      data  texspy ( 181 )   /  '  is read by  spy  proper (not plotting
     1).   finally, there is the use of a pound'  /
      data  texspy ( 182 )   /  '  sign "#" for reserved blanks, which a
     1re otherwise ignored.   the classic case '  /
      data  texspy ( 183 )   /  '  where it is needed is for a response
     1to  "list"  (e.g.,  "2#4");   this is    '  /
      data  texspy ( 184 )   /  '  free-format information, with a comma
     1impossible due to the conflict with "@".'  /
      data  texspy ( 185 )   /  '  although not a part of the interactiv
     1e  spy  code per se, it should also be   '  /
      data  texspy ( 186 )   /  '  remembered that commands which could
     1be built into  "@"  files can also be    '  /
      data  texspy ( 187 )   /  '  executed as part of the regular emtp
     1data.  the key to such usage is  "$spy", '  /
      data  texspy ( 188 )   /  '  which is processed by  "cimage"  and/
     1or  "erexit".   if all spy commands are  '  /
      data  texspy ( 189 )   /  '  to be built in-line as part of the lu
     1nit5 emtp input data, then precede such  '  /
      data  texspy ( 190 )   /  '  data by a card reading  "$spy",  with
     1columns 5 onward blank.  in this        '  /
      data  texspy ( 191 )   /  '  case,  "erexit"  reads and removes su
     1ch data cards, creating special reserved '  /
      data  texspy ( 192 )   /  '  disk files named  spyfile?.dat,  wher
     1e  "?"  is a single digit between one    '  /
      data  texspy ( 193 )   /  '  and nine (allowing a maximum of nine
     1such distinct groups of spy commands).   '  /
      data  texspy ( 194 )   /  '  the last spy command of each such gro
     1up is to be followed by  "$endspy"  as a '  /
      data  texspy ( 195 )   /  '  special marker record.  on the other
     1hand, if the user does not care about    '  /
      data  texspy ( 196 )   /  '  unifying all such emtp data in a sing
     1le disk file, then a single line of emtp '  /
      data  texspy ( 197 )   /  '  data,  "$spy, filename",  is all that
     1is required to provide the connection to'  /
      data  texspy ( 198 )   /  '  spy.  in this second card,  filename
     1can be any legal, full file name of the '  /
      data  texspy ( 199 )   /  '  computer system being considered (if
     1the user supplies the file name, and     '  /
      data  texspy ( 200 )   /  '  puts commands in the disk file, then
     1there is no limit on the number of       '  /
      data  texspy ( 201 )   /  '  such usages.   the role of  "cimage"
     1is to treat  "$spy"  like   "$include".  '  /
      data  texspy ( 202 )   /  'key word no. 20:  "roll"        ----  -
     1---  ----                                '  /
      data  texspy ( 203 )   /  '  this response to the  "spy:"  prompt
     1will result in a "rolling" of previously-'  /
      data  texspy ( 204 )   /  '  defined  "examine"  request.   while
     1this happens, no other spy activity is   '  /
      data  texspy ( 205 )   /  '  permitted (any <cr> will abort the lo
     1op, and return to the  "spy:"  prompt).  '  /
      data  texspy ( 206 )   /  '  following  "roll",  the simulation wi
     1ll recommence, with spy called at each   '  /
      data  texspy ( 207 )   /  '  opportunity for a spy break (within f
     1requency  maxflg  of  "dekspy",  which   '  /
      data  texspy ( 208 )   /  '  nominally has the value one, implying
     1four checks per time step).   the very  '  /
      data  texspy ( 209 )   /  '  first check, the heading and current
     1value of the  "examine"  vector will be  '  /
      data  texspy ( 210 )   /  '  displayed;  thereafter, output will b
     1e produced if and only if a change in    '  /
      data  texspy ( 211 )   /  '  the output (compared with the previou
     1s evaluation) has occurred.              '  /
      data  texspy ( 212 )   /  '  a final thought is about other comman
     1ds which function much like the  "roll"  '  /
      data  texspy ( 213 )   /  '  commands, although in fact these are
     1not issued at the  "spy:" level.  for    '  /
      data  texspy ( 214 )   /  '  "plot"  use,  at the  "inner:"  level
     1,  "rollc"  will produce continuous      '  /
      data  texspy ( 215 )   /  '  character plotting, while  "rollv"  d
     1oes the same for vector plotting.   it   '  /
      data  texspy ( 216 )   /  '  is the  "noroll"  command of spy whic
     1h will cancel either or both of these.   '  /
      data  texspy ( 217 )   /  'key word no. 21:  "type?"       ----  -
     1---  ----                                '  /
      data  texspy ( 218 )   /  '  this response to the  "spy:"  prompt
     1will result in the listing of command    '  /
      data  texspy ( 219 )   /  '  file  inclspy?.dat  of  "@?"  usage.
     1for any specific numerical digit       '  /
      data  texspy ( 220 )   /  '  "?",  just that one file will be list
     1ed.   but if symbolic "?" is retained, or'  /
      data  texspy ( 221 )   /  '  if column 5 onward is blank, then all
     1possible  "@?"  files will be listed    '  /
      data  texspy ( 222 )   /  '  in natural order, preceded by an iden
     1tifying heading.  also, for arbitrary    '  /
      data  texspy ( 223 )   /  '  disk files of card images with names
     1of 2 or more characters, this command    '  /
      data  texspy ( 224 )   /  '  will display them.  "type filename"
     1is the form of this more general         '  /
      data  texspy ( 225 )   /  '  command, where  filename  is any lega
     1l file name of 32 or fewer characters.   '  /
      data  texspy ( 226 )   /  '  if no such file exists, there will be
     1a warning message, so this command      '  /
      data  texspy ( 227 )   /  '  can be used to check on the existence
     1of arbitary disk files of card images.  '  /
      data  texspy ( 228 )   /  'key word no. 22:  "verify"      ----  -
     1---  ----                                '  /
      data  texspy ( 229 )   /  '  this response to the  "spy:"  prompt
     1will toggle the switch that controls the '  /
      data  texspy ( 230 )   /  '  echoing of data cards within a disk f
     1ile which is read via  "@"  usage.  the  '  /
      data  texspy ( 231 )   /  '  default (beginning) setting is to hav
     1e such echoing.   a related command is   '  /
      data  texspy ( 232 )   /  '  "comment",  which can separately cont
     1rol the display of comment ("c ") cards  '  /
      data  texspy ( 233 )   /  '  as they are encountered during proces
     1sing of the  "@"  file.  if there is no  '  /
      data  texspy ( 234 )   /  '  echoing of spy data, then comment car
     1ds might likewise not be displayed;  or, '  /
      data  texspy ( 235 )   /  '  they can be used as an absolute refer
     1ence (if they are displayed), to mark the'  /
      data  texspy ( 236 )   /  '  beginning or ending of invisible oper
     1ations due to no-echoing of "verify".    '  /
      data  texspy ( 237 )   /  'key word no. 23:  "files"       ----  -
     1---  ----                                '  /
      data  texspy ( 238 )   /  '  this  response  to  the  "spy:"  prom
     1pt will result in a display of all of the'  /
      data  texspy ( 239 )   /  '  inclspy?.dat  files which exist,  bas
     1ed on the fortran "inquire" operation  at'  /
      data  texspy ( 240 )   /  '  the time program execution began.  an
     1"x"  means that the file exists (in the'  /
      data  texspy ( 241 )   /  '  display),  whereas a blank means that
     1it does not.  there are 9 columns.      '  /
      data  texspy ( 242 )   /  'key word no. 24:  "sleep"       ----  -
     1---  ----                                '  /
      data  texspy ( 243 )   /  '  this response to the  "spy:"  prompt
     1will put the emtp to sleep in such a way '  /
      data  texspy ( 244 )   /  '  that the simulation can be continued
     1at any later time.  the interactive      '  /
      data  texspy ( 245 )   /  '  "sleep"  command is comparable to the
     1batch-mode use of miscellaneous data    '  /
      data  texspy ( 246 )   /  '  parameter   memsav = 1   for the savi
     1ng of emtp tables on disk.  to service a '  /
      data  texspy ( 247 )   /  '  "sleep"  command, spy exits the time-
     1step loop and jumps to  "over20"  for    '  /
      data  texspy ( 248 )   /  '  table dumping to disk.  subsequent aw
     1akening is via  "wake",  which reads     '  /
      data  texspy ( 249 )   /  '  emtp tables back from disk into emtp
     1memory.  any plot data must be separately'  /
      data  texspy ( 250 )   /  '  and manually provided for by the user
     1(using  "space", "lunit4"),  if it, too,'  /
      data  texspy ( 251 )   /  '  is to be permanently saved.
     1'  /
      data  texspy ( 252 )   /  'key word no. 25:  "source"      ----  -
     1---  ----                                '  /
      data  texspy ( 253 )   /  '  this response to the  "spy:"  prompt
     1will allow the user to look at either    '  /
      data  texspy ( 254 )   /  '  the electric network or the tacs sour
     1ce table.   there will be a pause after  '  /
      data  texspy ( 255 )   /  '  spy receives  "source",  as it waits
     1to receive either  "tacs"  or  "elec"  as'  /
      data  texspy ( 256 )   /  '  an indication of source-table choice.
     1then  spy  waits for a pair of free-  '  /
      data  texspy ( 257 )   /  '  format integer row numbers, to define
     1the limits of the table display.   a few'  /
      data  texspy ( 258 )   /  '  key words are also accepted here:  "a
     1ll"  to display entire table,  "end"  or '  /
      data  texspy ( 259 )   /  '  "stop"  or  "spy"  to return to  "spy
     1:"  prompt,  and  "tacs"  or  "elec"  to '  /
      data  texspy ( 260 )   /  '  produce a new table heading (or switc
     1h between the two tables).   the tacs    '  /
      data  texspy ( 261 )   /  '  table displays offsets of  "sptacs"
     1which are used with  "deposit/examine",  '  /
      data  texspy ( 262 )   /  '  in case the user wants to redefine su
     1ch quantities.                           '  /
      data  texspy ( 263 )   /  'key word no. 26:  "edit"        ----  -
     1---  ----                                '  /
      data  texspy ( 264 )   /  '  this response to the  "spy:"  prompt
     1will allow the user to examine and modify'  /
      data  texspy ( 265 )   /  '  card images which are currently store
     1d in memory (see the  "data"  command).  '  /
      data  texspy ( 266 )   /  '  a  "*"  prompt will next appear, at w
     1hich point vax sos-like editing commands '  /
      data  texspy ( 267 )   /  '  can be issued.  the only bothersome c
     1hanges of notation are the use of  "#"   '  /
      data  texspy ( 268 )   /  '  in place of vax"s  "!"  (because of i
     1n-line comment problems with e/ts),  and '  /
      data  texspy ( 269 )   /  '  use of  "@"  in place of vax"s  "esc"
     1key as a character string delimiter.   '  /
      data  texspy ( 270 )   /  '  for an explanation of sos editing rul
     1es, see dec vax-11 user documentation.   '  /
      data  texspy ( 271 )   /  '  examples of printing include:  "*p^:*
     1",  "*p5",  "*p^:18",  "*p5#10",  "*p",  '  /
      data  texspy ( 272 )   /  '  and  "p.-20:."   there also is  "*f"
     1usage,  only with  "@"  replacing  <esc> '  /
      data  texspy ( 273 )   /  '  of  vax/vms  sos.  to exit the  "edit
     1"  command and return to the  "spy:"     '  /
      data  texspy ( 274 )   /  '  prompt, use  "*e"  (analogous to the
     1sos exit).  additional user commands     '  /
      data  texspy ( 275 )   /  '  include  "*d"  (for deletion of lines
     1),  "*r"  (for replacement of lines),    '  /
      data  texspy ( 276 )   /  '  "*i"  (for insertion of new lines),
     1and  "*s"  (for the substitution of one  '  /
      data  texspy ( 277 )   /  '  character string by another).  concer
     1ning  "*s",  however, once again  "@"    '  /
      data  texspy ( 278 )   /  '  is used as a delimiter rather than  <
     1esc>,  and no qualifiers (e.g., ",d" for '  /
      data  texspy ( 279 )   /  '  "decide mode") are allowed.   finally
     1, there are special emtp-designed        '  /
      data  texspy ( 280 )   /  '  commands.   the first of these is  "*
     18",  which initiates a search for the    '  /
      data  texspy ( 281 )   /  '  next card which has a non-blank colum
     1n 80.   after display of this record,    '  /
      data  texspy ( 282 )   /  '  spy awaits a user decision regarding
     1disposition:  <cr>  will leave the card  '  /
      data  texspy ( 283 )   /  '  unchanged and initiate a search for t
     1he following one;  digits 0, 1, 2, 3, 4  '  /
      data  texspy ( 284 )   /  '  will result in the punching of this v
     1alue into column 80 before searching for '  /
      data  texspy ( 285 )   /  '  the next such record (with "0" intern
     1ally changed to a blank before punching).'  /
      data  texspy ( 286 )   /  '  the  "*"  prompt will reappear automa
     1tically when the search hits the bottom  '  /
      data  texspy ( 287 )   /  '  of the file.  or,  "*"  can be reache
     1d at any point of the search-display loop'  /
      data  texspy ( 288 )   /  '  by sending  "e" .   even if not in th
     1e  "*8"  loop, column-80 deposits are    '  /
      data  texspy ( 289 )   /  '  possible by use of  "*8,?"  where  "?
     1"  is the desired col.-80 content of the '  /
      data  texspy ( 290 )   /  '  current line.   finally,  the command
     1"*col"  will produce a heading of      '  /
      data  texspy ( 291 )   /  '  column numbers, which is useful when
     1inserting new data records using  "*i" . '  /
      data  texspy ( 292 )   /  '  this ruler heading will be shifted to
     1line up with  "*p"  displays.   if an   '  /
      data  texspy ( 293 )   /  '  unshifted display is desired,  use  "
     1*col8" .                                 '  /
      data  texspy ( 294 )   /  'key word no. 27:  "wake"        ----  -
     1---  ----                                '  /
      data  texspy ( 295 )   /  '  this response to the  "spy:"  prompt
     1will awaken a hibernating solution (one  '  /
      data  texspy ( 296 )   /  '  which was put to bed with an earlier
     1"sleep"  command).   altered program    '  /
      data  texspy ( 297 )   /  '  dimensions are not allowed  (both pro
     1grams must be dimensioned identically),  '  /
      data  texspy ( 298 )   /  '  just as with the batch-mode  "start a
     1gain".  the  "wake"  command is the exact'  /
      data  texspy ( 299 )   /  '  interactive equivalent of the batch-m
     1ode request  "start again".  there is a  '  /
      data  texspy ( 300 )   /  '  related  "wake4"  command for regener
     1ation of the lunit4 plot-file header     '  /
      data  texspy ( 301 )   /  '  information, in case this is wanted b
     1y the user (it is in fact needed, if the '  /
      data  texspy ( 302 )   /  '  "plot"  command is to be used, as of
     126 feb 1984).                            '  /
      data  texspy ( 303 )   /  'key word no. 28:  "language"    ----  -
     1---  ----                                '  /
      data  texspy ( 304 )   /  '  this response to the  "spy:"  prompt
     1allows the user to either examine or     '  /
      data  texspy ( 305 )   /  '  modify "spy:"-level command words (e.
     1g., "plot", "data", etc.).  a loop will  '  /
      data  texspy ( 306 )   /  '  be entered, in which there are only 4
     1legal responses  ("single",  "entire",  '  /
      data  texspy ( 307 )   /  '  "show",  and  "spy")  to the prompt.
     1sending  "single"  will lead to an      '  /
      data  texspy ( 308 )   /  '  inner input loop in which old and new
     1symbol pairs are to be redefined one at '  /
      data  texspy ( 309 )   /  '  a time, terminated by  "end"  (to ret
     1urn to outer  "language"  loop) or  "spy"'  /
      data  texspy ( 310 )   /  '  (to return to the  "spy:"  prompt).
     1the second outer response,  "entire",   '  /
      data  texspy ( 311 )   /  '  must be followed by an unabridged dic
     1tionary of symbols using 10a8 format.    '  /
      data  texspy ( 312 )   /  '  sending the third response  "show"  w
     1ill result in an unabridged display of   '  /
      data  texspy ( 313 )   /  '  all such current commands.  finally,
     1sending  "spy"  will exit the outer loop,'  /
      data  texspy ( 314 )   /  '  and return to the  "spy:"  prompt.  t
     1he language of  "plot"  usage does not   '  /
      data  texspy ( 315 )   /  '  occur at the command level with promp
     1t  "spy:",  so it can only be redefined  '  /
      data  texspy ( 316 )   /  '  within that utility (by means of the
     1"set data"  command).                   '  /
      data  texspy ( 317 )   /  'key word no. 29:  "catalog"     ----  -
     1---  ----                                '  /
      data  texspy ( 318 )   /  '  this response to the  "spy:"  prompt
     1will create a new permanent disk file, it'  /
      data  texspy ( 319 )   /  '  will dump a copy of the data case whi
     1ch is presently contained within the     '  /
      data  texspy ( 320 )   /  '  emtp into that disk file.   the name
     1of this new disk file is user-supplied   '  /
      data  texspy ( 321 )   /  '  (in response to a subsequent prompt f
     1or such a name, which must, naturally, be'  /
      data  texspy ( 322 )   /  '  legal for the computer system being u
     1sed).   the emtp data case in question   '  /
      data  texspy ( 323 )   /  '  generally will differ from that origi
     1nally read in using the  "data"  command,'  /
      data  texspy ( 324 )   /  '  of course  (assuming  "edit"  operati
     1ons have produced alterations).          '  /
      data  texspy ( 325 )   /  'key word no. 30:  "begin"       ----  -
     1---  ----                                '  /
      data  texspy ( 326 )   /  '  this response to the  "spy:"  prompt
     1will abort the current emtp solution,    '  /
      data  texspy ( 327 )   /  '  and initiate a complete new solution.
     1it is the simulator-stored emtp data  '  /
      data  texspy ( 328 )   /  '  cards which are used as input for the
     1new solution, and these will usually    '  /
      data  texspy ( 329 )   /  '  have just been modified using  "edit"
     1operations.  the transition is         '  /
      data  texspy ( 330 )   /  '  "instantaneous" for cases which have
     1been put to bed via  "rest"  or  "sleep",'  /
      data  texspy ( 331 )   /  '  or which are cycling the time-step lo
     1op.   for execution in earlier overlays, '  /
      data  texspy ( 332 )   /  '  there may be a delay until the presen
     1t overlay is exited.                     '  /
      data  texspy ( 333 )   /  'key word no. 31:  "step"        ----  -
     1---  ----                                '  /
      data  texspy ( 334 )   /  '  this response to the  "spy:"  prompt
     1represents a request to toggle the binary'  /
      data  texspy ( 335 )   /  '  switch that  forces  a  spy  interrup
     1t at each and every possible opportunity.'  /
      data  texspy ( 336 )   /  '  either  there  are  such  ever-presen
     1t forced interrupts, or  there  are none.'  /
      data  texspy ( 337 )   /  '  interrupt opportunities exist at the
     1start of each overlay,  at  the start  of'  /
      data  texspy ( 338 )   /  '  each of the four pieces of overlay 16
     1(the time-step loop),  and  finally, as'  /
      data  texspy ( 339 )   /  '  each new data card is read by input m
     1odule  "cimage".                         '  /
      data  texspy ( 340 )   /  'key word no. 32:  "debug"       ----  -
     1---  ----                                '  /
      data  texspy ( 341 )   /  '  this response to the  "spy:"  prompt
     1is used only for debugging of interactive'  /
      data  texspy ( 342 )   /  '  emtp execution itself, in case of fau
     1lty operation.   it is not used for cases'  /
      data  texspy ( 343 )   /  '  of suspected emtp error (do not confu
     1se with  "iprsup"  of the emtp).  anyway,'  /
      data  texspy ( 344 )   /  '  there will be a subsequent prompt for
     1diagnostic control variable  iprspy.    '  /
      data  texspy ( 345 )   /  'key word no. 33:  "data"       ----  --
     1--  ----                                 '  /
      data  texspy ( 346 )   /  '  this response to the  "spy:"  prompt
     1is appropriate when the user wants to    '  /
      data  texspy ( 347 )   /  '  specify a disk file of emtp data whic
     1h is to be solved next.  the program     '  /
      data  texspy ( 348 )   /  '  will then prompt for the desired file
     1name, which will generally be computer- '  /
      data  texspy ( 349 )   /  '  dependent.  a full, legal file name f
     1or the computer being used must then be  '  /
      data  texspy ( 350 )   /  '  provided.  before giving the real fil
     1e name, however, the user could send     '  /
      data  texspy ( 351 )   /  '  "control",  should he want to input o
     1nly a portion of the disk file, or should'  /
      data  texspy ( 352 )   /  '  it be desired to have the data placed
     1in the emtp cache with an offset, or    '  /
      data  texspy ( 353 )   /  '  should it be desired to have the  lun
     1it5  usage pointer set to other than the '  /
      data  texspy ( 354 )   /  '  default value of unity (offset zero).
     1a second non-file response is  "spy",  '  /
      data  texspy ( 355 )   /  '  to abort the file-name prompt after t
     1he  "control"  definitions (in case the  '  /
      data  texspy ( 356 )   /  '  user only wants to redefine the  luni
     1t5  usage pointer, for example).   a     '  /
      data  texspy ( 357 )   /  '  final point concerns the avoidance of
     1non-existent files (which would result  '  /
      data  texspy ( 358 )   /  '  in an error stop of the emtp by the o
     1perating system).  if in doubt about a   '  /
      data  texspy ( 359 )   /  '  file"s existence, try to  type  it fi
     1rst (the  "type"  command warns of non-  '  /
      data  texspy ( 360 )   /  '  existent files, without any terminati
     1on of execution).                        '  /
      data  texspy ( 361 )   /  'key word no. 34:  "ramp"        ----  -
     1---  ----                                '  /
      data  texspy ( 362 )   /  '  this response to the  "spy:"  prompt
     1will result in prompts which allow the   '  /
      data  texspy ( 363 )   /  '  user to modify any variables in emtp
     1common blocks as linear functions of     '  /
      data  texspy ( 364 )   /  '  time.   that is, the the user can ram
     1p the values between beginning and ending'  /
      data  texspy ( 365 )   /  '  limits,  as emtp simulation time move
     1s between beginning and ending times --- '  /
      data  texspy ( 366 )   /  '  with all control parameters being use
     1r-defined.  any variable which can be    '  /
      data  texspy ( 367 )   /  '  seen via the  "examine"  command can
     1also be  ramped.  in specifying parameter'  /
      data  texspy ( 368 )   /  '  values, there are three nested loops.
     1the outer loop is for time, the middle'  /
      data  texspy ( 369 )   /  '  one is for variable values, and the i
     1nner one is for variable names.   the    '  /
      data  texspy ( 370 )   /  '  user can stay inside (middle or inner
     1loops) as long as the outer quantity is '  /
      data  texspy ( 371 )   /  '  not altered.   "end" will exit any on
     1e level,  and head outward to the next   '  /
      data  texspy ( 372 )   /  '  one.   at the outer-most level, the o
     1ptional  "show"  command will display all'  /
      data  texspy ( 373 )   /  '  ramps which the user has defined thus
     1far,  while  "end"  or  "spy"  will     '  /
      data  texspy ( 374 )   /  '  return to the  "spy:"  prompt.  actua
     1l variable redefinition is handled within'  /
      data  texspy ( 375 )   /  '  module  "analyt"  of overlay 20 of th
     1e emtp, where ramping logic has been     '  /
      data  texspy ( 376 )   /  '  built (this is automatic, as the simu
     1lation progresses).                      '  /
      data  texspy ( 377 )   /  'key word no. 35:  "time"        ----  -
     1---  ----                                '  /
      data  texspy ( 378 )   /  '  this response to the  "spy:"  prompt
     1will result in the display of  both  the'  /
      data  texspy ( 379 )   /  '  wall-clock time  (the date, too)  and
     1also  the  current emtp simulation time'  /
      data  texspy ( 380 )   /  '  parameters (t, tmax, deltat).
     1'  /
      data  texspy ( 381 )   /  'key word no. 36:  "tek"         ----  -
     1---  ----                                '  /
      data  texspy ( 382 )   /  '  this response to the  "spy:"  prompt
     1allows modification to an ongoing vector-'  /
      data  texspy ( 383 )   /  '  graphic display which is currently  r
     1olling.  yes, the user could simply      '  /
      data  texspy ( 384 )   /  '  regenerate the plot from the beginnin
     1g (by use of a  "plot"  command following'  /
      data  texspy ( 385 )   /  '  a user-keyed interrupt), but minor ch
     1anges can often be made more easily, and '  /
      data  texspy ( 386 )   /  '  with less disruption, using  "tek".
     1a subsequent prompt will then list a    '  /
      data  texspy ( 387 )   /  '  menu of alternatives which are availa
     1ble for such "on-the-fly"  rolling       '  /
      data  texspy ( 388 )   /  '  "plot"  tampering.  for completeness,
     1that menu is repeated here:             '  /
      data  texspy ( 389 )   /  '       spy:tek
     1'  /
      data  texspy ( 390 )   /  '        >< to tamper with the rolling v
     1ector plot,  send choice.                '  /
      data  texspy ( 391 )   /  '        >< option (mark, delay, inner,
     1overlap, end, help) :                    '  /
      data  texspy ( 392 )   /  '           mark  ---- for instantaneous
     1marking of curves on tek screen;        '  /
      data  texspy ( 393 )   /  '           delay  --- to control how si
     1multaneous the rolling is to be;         '  /
      data  texspy ( 394 )   /  '           inner  ---- to  call timval
     1(the "inner:" level of ploting);        '  /
      data  texspy ( 395 )   /  '           overlap  -- to modify the pe
     1rcent overlap for new-page plot;         '  /
      data  texspy ( 396 )   /  '           end   ---- for return to  "s
     1py:"  prompt.                            '  /
      data  texspy ( 397 )   /  '  unless the user requests curve identi
     1fication via  "mark",  the  rolling plot '  /
      data  texspy ( 398 )   /  '  will only have it for the regenerated
     1portion (the overlap from the previous  '  /
      data  texspy ( 399 )   /  '  page, as controlled by  "overlap").
     1the  "delay"  command allows the user   '  /
      data  texspy ( 400 )   /  '  to control the time-step multiplicity
     1between plotting sessions.  there is a  '  /
      data  texspy ( 401 )   /  '  trade off between solution efficiency
     1(maximized if plotting is infrequent)   '  /
      data  texspy ( 402 )   /  '  and simultaneous observation (maximiz
     1ed if plotting occurs on each new time   '  /
      data  texspy ( 403 )   /  '  step).   so much for the concept.  bu
     1t the details of counting are in fact a  '  /
      data  texspy ( 404 )   /  '  little different, since it is not tim
     1e steps, but rather plot points, which   '  /
      data  texspy ( 405 )   /  '  are counted.   only if every solution
     1point becomes a plot point (only if the '  /
      data  texspy ( 406 )   /  '  miscellaneous data parameter  iplot
     1is equal to unity) are these two equal,  '  /
      data  texspy ( 407 )   /  '  note.  so, if  iout = 3  and the user
     1wants to see plotting progress about    '  /
      data  texspy ( 408 )   /  '  every twentieth time-step, he would s
     1end  "7"  in response to the prompt for a'  /
      data  texspy ( 409 )   /  '  multiplicity after his  "delay"  was
     1accepted.   the result would then be     '  /
      data  texspy ( 410 )   /  '  incremental plotting every 21st step
     1(21 = 3*7).   concerning  "overlap",  it '  /
      data  texspy ( 411 )   /  '  is generally recommended that this on
     1ly be used for terminals without memory, '  /
      data  texspy ( 412 )   /  '  since any percentage greater than zer
     1o does increase the plotting burden.   in'  /
      data  texspy ( 413 )   /  '  the case of displays with near-infini
     1te storage (e.g., apollo windows),  it is'  /
      data  texspy ( 414 )   /  '  better to leave the overlap at the de
     1fault value of zero.   use of  "inner"   '  /
      data  texspy ( 415 )   /  '  both very powerful and potentially ve
     1ry tricky, since it results in a  call to'  /
      data  texspy ( 416 )   /  '  subroutine timval,  which is responsi
     1ble for the  "inner:"  level of  "plot"  '  /
      data  texspy ( 417 )   /  '  dialogue.   all  "inner:"  level defi
     1nitions are therefore available to the   '  /
      data  texspy ( 418 )   /  '  user, although effects may or may not
     1be as expected (either trial and error  '  /
      data  texspy ( 419 )   /  '  experience, or understanding of "tppl
     1ot" fortran, are required in order to    '  /
      data  texspy ( 420 )   /  '  predict the results of any given oper
     1ation).   the one big difference is that '  /
      data  texspy ( 421 )   /  '  the use of  "time"  or just  <cr>  do
     1not result in the production of a new   '  /
      data  texspy ( 422 )   /  '  plot.  to return back to the  "tek"
     1prompt from the  "inner:"  prompt,       '  /
      data  texspy ( 423 )   /  '  send  "spy".   as for return to the
     1"spy"  prompt,  either the listed  "end" '  /
      data  texspy ( 424 )   /  '  or  "spy"  will work.
     1'  /
      data  texspy ( 425 )   /  'key word no. 37:  "branch"      ----  -
     1---  ----                                '  /
      data  texspy ( 426 )   /  '  this response to the  "spy:"  prompt
     1is  issued  for  a  display  of the emtp'  /
      data  texspy ( 427 )   /  '  branch table.  subsequently send an a
     1dditional  "extra"  to change to the next'  /
      data  texspy ( 428 )   /  '  sub-table  (as of february 1984, ther
     1e  are  two),  if  different columns  are'  /
      data  texspy ( 429 )   /  '  desired.  within any one choice as to
     1sub-table,  there is a loop in which the'  /
      data  texspy ( 430 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      data  texspy ( 431 )   /  '  or  a  key word.  recognized key word
     1s here  are  "all"  for the entire table,'  /
      data  texspy ( 432 )   /  '  "top"  for row one,  "bot"  for the
     1last  row,  "next"  or  just  a  carriage'  /
      data  texspy ( 433 )   /  '  return for the following row, and of
     1course  "spy"  to  abort the display loop'  /
      data  texspy ( 434 )   /  '  and return to the  "spy:"  prompt.  t
     1o refresh the heading, use either  "bran"'  /
      data  texspy ( 435 )   /  '  or  "head"  (only 4 characters of  "b
     1ranch"  or  "heading"  are checked).     '  /
      data  texspy ( 436 )   /  'key word no. 38:  "yform"       ----  -
     1---  ----                                '  /
      data  texspy ( 437 )   /  '  this  response  to  the  "spy:"  prom
     1pt  will result in continuous (every time'  /
      data  texspy ( 438 )   /  '  step)  re-formation of  [y],  followe
     1d by re-triangularization.   it is turned'  /
      data  texspy ( 439 )   /  '  off (no more [y]-forming)  by sending
     1"noy"  after  "spy:"  prompt.  ???  ???'  /
      data  texspy ( 440 )   /  '  warning:  like  the earlier batch-mod
     1e  "modify deltat",  this looks better in'  /
      data  texspy ( 441 )   /  '  theory than  it  does in practice.  f
     1or  important elements such as frequency-'  /
      data  texspy ( 442 )   /  '  dependent transmission lines, the ori
     1ginal data which is needed to reform  [y]'  /
      data  texspy ( 443 )   /  '  has been destroyed by the solution, s
     1o the result  will  be erroneous.  unless'  /
      data  texspy ( 444 )   /  '  the user has a particularly degenerat
     1e problem,  and  he is sure that he knows'  /
      data  texspy ( 445 )   /  '  what he is doing, the general recomme
     1ndation is to avoid all such use.  ??? ??'  /
      data  texspy ( 446 )   /  'key word no. 39:  "noy"         ----  -
     1---  ----                                '  /
      data  texspy ( 447 )   /  '  this response to the  "spy:"  prompt
     1will cancel a preceding  "yform"  request'  /
      data  texspy ( 448 )   /  '  for continuous [y]-formation.
     1'  /
      data  texspy ( 449 )   /  'key word no. 40:  "factor"      ----  -
     1---  ----                                '  /
      data  texspy ( 450 )   /  '  this  response  to  the  "spy:"  prom
     1pt  will result in continuous (every time'  /
      data  texspy ( 451 )   /  '  step) triangularization.   it is turn
     1ed off  (no more automatic factoring)  by'  /
      data  texspy ( 452 )   /  '  sending  "nof"  in response to the  "
     1spy:"  prompt.                           '  /
      data  texspy ( 453 )   /  'key word no. 41:  "nof"         ----  -
     1---  ----                                '  /
      data  texspy ( 454 )   /  '  this  response  to  the  "spy:"  prom
     1pt  will  cancel  a  preceding   "factor"'  /
      data  texspy ( 455 )   /  '  request for the continuous re-triangu
     1larization of [y].                       '  /
      data  texspy ( 456 )   /  'key word no. 42:  "rlc"         ----  -
     1---  ----                                '  /
      data  texspy ( 457 )   /  '  this response to the  "spy:"  prompt
     1will produce a display of the emtp  r-l-c'  /
      data  texspy ( 458 )   /  '  tables.  subsequently  send  an  addi
     1tional  "extra"  to  change to  the  next'  /
      data  texspy ( 459 )   /  '  sub-table  (as of february 1984, ther
     1e  are  two),  if  different columns  are'  /
      data  texspy ( 460 )   /  '  desired.  within any one choice as to
     1sub-table,  there is a loop in which the'  /
      data  texspy ( 461 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      data  texspy ( 462 )   /  '  or  a  key word.  recognized key word
     1s here  are  "all"  for the entire table,'  /
      data  texspy ( 463 )   /  '  "top"  for row one,  "bot"  for the
     1last  row,  "next"  or  just  a  carriage'  /
      data  texspy ( 464 )   /  '  return for the following row, and of
     1course  "spy"  to  abort the display loop'  /
      data  texspy ( 465 )   /  '  and return to the  "spy:"  prompt.  t
     1o refresh the heading,  use either  "rlc"'  /
      data  texspy ( 466 )   /  '  or  "head"  (only 4 characters of  "h
     1eading"  are checked).                   '  /
      data  texspy ( 467 )   /  'key word no. 43:  "width"       ----  -
     1---  ----                                '  /
      data  texspy ( 468 )   /  '  this response to the  "spy:"  prompt
     1will toggle the output line length  (if  '  /
      data  texspy ( 469 )   /  '  equal to 132 at the time the command
     1is issued, it will be changed to 80,  and'  /
      data  texspy ( 470 )   /  '  vice-versa).  this is for emtp line p
     1rinter output (channel lunit6) only.     '  /
      data  texspy ( 471 )   /  'key word no. 44:  "bus"         ----  -
     1---  ----                                '  /
      data  texspy ( 472 )   /  '  this response to the  "spy:"  prompt
     1will produce a display  of  the  emtp bus'  /
      data  texspy ( 473 )   /  '  vectors.   subsequently  send  an  ad
     1ditional  "extra"  to  change to the next'  /
      data  texspy ( 474 )   /  '  next sub-table (as of february 1984,
     1there are two),  if different columns are'  /
      data  texspy ( 475 )   /  '  desired.  within any one choice as to
     1sub-table,  there is a loop in which the'  /
      data  texspy ( 476 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      data  texspy ( 477 )   /  '  or  a  key word.  recognized key word
     1s here  are  "all"  for the entire table,'  /
      data  texspy ( 478 )   /  '  "top"  for row one,  "bot"  for the
     1last  row,  "next"  or  just  a  carriage'  /
      data  texspy ( 479 )   /  '  return for the following row, and of
     1course  "spy"  to  abort the display loop'  /
      data  texspy ( 480 )   /  '  and return to the  "spy:"  prompt.  t
     1o refresh the heading,  use either  "bus"'  /
      data  texspy ( 481 )   /  '  or  "head"  (only 4 characters of "he
     1ading" are checked).                     '  /
      data  texspy ( 482 )   /  'key word no. 45:  "size"        ----  -
     1---  ----                                '  /
      data  texspy ( 483 )   /  '  this response  to  the  "spy:"  promp
     1t  will  produce a display of actual emtp'  /
      data  texspy ( 484 )   /  '  data requirements  ----  the  "presen
     1t figure"  list sizes which are  seen  at'  /
      data  texspy ( 485 )   /  '  the end of batch-mode emtp printout,
     1in the case-summary statistics.         '  /
      data  texspy ( 486 )   /  'key word no. 46:  "limit"       ----  -
     1---  ----                                '  /
      data  texspy ( 487 )   /  '  this response  to  the  "spy:"  promp
     1t  will produce a display of the limiting'  /
      data  texspy ( 488 )   /  '  emtp table sizes  ----  the  "program
     1limit"  list sizes which are seen at the'  /
      data  texspy ( 489 )   /  '  end of batch-mode emtp printout,  in
     1the case-summary statistics.             '  /
      data  texspy ( 490 )   /  'key word no. 47:  "iout"        ----  -
     1---  ----                                '  /
      data  texspy ( 491 )   /  '  this response  to  the  "spy:"  promp
     1t  will alter the emtp printout frequency'  /
      data  texspy ( 492 )   /  '  according  to  the user"s latest desi
     1re.   the response will be instantaneous,'  /
      data  texspy ( 493 )   /  '  with  a  later  minor adjustment at t
     1he first round step number  (the next one'  /
      data  texspy ( 494 )   /  '  which  is  divisible  by  iout  with
     1zero  remainder).   this command cancels'  /
      data  texspy ( 495 )   /  '  any  previous  batch-mode requests  f
     1or  modification  of the output frequency'  /
      data  texspy ( 496 )   /  '  (e.g., the special-request word  "cha
     1nge printout frequency").                '  /
      data  texspy ( 497 )   /  'key word no. 48:  "node"        ----  -
     1---  ----                                '  /
      data  texspy ( 498 )   /  '  this response  to  the  "spy:"  promp
     1t  will yield a question/answer loop with'  /
      data  texspy ( 499 )   /  '  input being a user-supplied 6-charact
     1er bus name, and output (the spy display)'  /
      data  texspy ( 500 )   /  '  being the corresponding emtp node num
     1ber.   after any one node number has been'  /
      data  texspy ( 501 )   /  '  displayed,  the  user can send  "conn
     1ect"  to obtain a list of all row numbers'  /
      data  texspy ( 502 )   /  '  of all connected branches/switches/no
     1nlinear elements.                        '  /
      data  texspy ( 503 )   /  'key word no. 49:  "nonlin"      ----  -
     1---  ----                                '  /
      data  texspy ( 504 )   /  '  this  response  to  the  "spy:"  prom
     1pt  is  issued  for a display of the emtp'  /
      data  texspy ( 505 )   /  '  nonlinear element table.  subsequentl
     1y  send an additional  "extra"  to change'  /
      data  texspy ( 506 )   /  '  to the next sub-table (as of february
     11984, there are just two),  if different'  /
      data  texspy ( 507 )   /  '  columns are desired.  within any one
     1sub-table,  there is a loop in which  the'  /
      data  texspy ( 508 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      data  texspy ( 509 )   /  '  or  a  key word.  recognized key word
     1s here  are  "all"  for the entire table,'  /
      data  texspy ( 510 )   /  '  "top"  for row one,  "bot"  for the
     1last  row,  "next"  or  just  a  carriage'  /
      data  texspy ( 511 )   /  '  return for the following row, and of
     1course  "spy"  to  abort the display loop'  /
      data  texspy ( 512 )   /  '  and return to the  "spy:"  prompt.  t
     1o refresh the heading, use either  "nonl"'  /
      data  texspy ( 513 )   /  '  or  "head"  (only 4 characters of "no
     1nlin" or "heading" are checked).         '  /
      data  texspy ( 514 )   /  'key word no. 50:  "space"       ----  -
     1---  ----                                '  /
      data  texspy ( 515 )   /  '  this response to the  "spy:"  prompt
     1begins dialogue which can rearrange      '  /
      data  texspy ( 516 )   /  '  storage of either data cards or plot
     1data points.  such use is mandatory if   '  /
      data  texspy ( 517 )   /  '  no additional room for plot data rema
     1ins (logic of "pltfil" will deny passage '  /
      data  texspy ( 518 )   /  '  until more storage has so been freed,
     1in such a case).  the management of card'  /
      data  texspy ( 519 )   /  '  images will generally be performed on
     1ly in conjunction with  "edit"  use, to  '  /
      data  texspy ( 520 )   /  '  modify emtp data.  parameters related
     1to these two storages will be displayed:'  /
      data  texspy ( 521 )   /  '     indbeg --- beginning location (ind
     1ex to /c29b01/) of plot data points;     '  /
      data  texspy ( 522 )   /  '     indbuf --- next free cell (index t
     1o /c29b01/) for storage of plot data;    '  /
      data  texspy ( 523 )   /  '     limbuf --- limit on indbuf (end of
     1/c29b01/);                              '  /
      data  texspy ( 524 )   /  '     numdcd --- last card read by emtp
     1(index to character*80 file6 storage)    '  /
      data  texspy ( 525 )   /  '     numcrd --- largest index for card
     1storage in file6 (as read by data);      '  /
      data  texspy ( 526 )   /  '     limcrd --- limiting index for file
     16 card storage (dimensioned limit).      '  /
      data  texspy ( 527 )   /  '  immediately after this display there
     1will be a choice among the possible      '  /
      data  texspy ( 528 )   /  '  responses  "cards",  "plot",  and  "s
     1py".  the third of these will abort      '  /
      data  texspy ( 529 )   /  '  the command, while the first two sele
     1ct between the two major classes of      '  /
      data  texspy ( 530 )   /  '  space management.
     1'  /
      data  texspy ( 531 )   /  '       (1) for the  "plot"  case,  the
     1user must next choose among  "write",    '  /
      data  texspy ( 532 )   /  '  "thin",  "delete",  and  "read",  bas
     1ically.   the first of these writes plot '  /
      data  texspy ( 533 )   /  '  points to disk just as a non-interact
     1ive program would, using i/o channel     '  /
      data  texspy ( 534 )   /  '  number lunit4.   the only change is t
     1hat the time span  [tbeg, tend]  to be   '  /
      data  texspy ( 535 )   /  '  dumped,  as well as the frequency of
     1the output (the effective iplot), are    '  /
      data  texspy ( 536 )   /  '  under interactive control (chosen by
     1responses to subsequent prompts).  the   '  /
      data  texspy ( 537 )   /  '  "thin"  command discards points withi
     1n the  [tbeg, tend]  window according to '  /
      data  texspy ( 538 )   /  '  a user-specified frequency.  the  "de
     1lete"  option destroys all points        '  /
      data  texspy ( 539 )   /  '  (the effective iplot is infinite) wit
     1hin the user-specified time range        '  /
      data  texspy ( 540 )   /  '  [tbeg,  tend].   the  "read"  option
     1is the reverse of  "write",  allowing the'  /
      data  texspy ( 541 )   /  '  restoration of lunit4 disk-stored poi
     1nts to the working interactive memory.   '  /
      data  texspy ( 542 )   /  '  this is done with user-specified freq
     1uency within the time range [tbeg, tend].'  /
      data  texspy ( 543 )   /  '  finally, there is a hybrid command  "
     1flush"  which combines  "write"  and     '  /
      data  texspy ( 544 )   /  '  "thin"  (assuming that the user wants
     1to use a common multiplicity for both   '  /
      data  texspy ( 545 )   /  '  operations).  after compacting operat
     1ions, there will be printout of available'  /
      data  texspy ( 546 )   /  '  free space.  the normal emtp line pri
     1nter output (i/o channel lunit6) will    '  /
      data  texspy ( 547 )   /  '  contain a record of the plot-file man
     1ipulations, to remind the user of what   '  /
      data  texspy ( 548 )   /  '  he has done.  an extra command is  "a
     1uto",  which gives an automatic, full    '  /
      data  texspy ( 549 )   /  '  "flush"  from that point onward.
     1'  /
      data  texspy ( 550 )   /  '       (2) for the  cards  branch, the
     1user is prompted to choose among  "move",'  /
      data  texspy ( 551 )   /  '  "copy",  and  "blank".   the first of
     1these is for block transfers (leaving   '  /
      data  texspy ( 552 )   /  '  blanks in vacated locations), while t
     1he second is for block reproductions     '  /
      data  texspy ( 553 )   /  '  (leaving the original locations unalt
     1ered).  the  "blank"  command is to      '  /
      data  texspy ( 554 )   /  '  erase a block of cards.  in all three
     1cases, beginning locations and total    '  /
      data  texspy ( 555 )   /  '  number of cards processed are used to
     1define the blocks involved.             '  /
      data  texspy ( 556 )   /  '       many sub-commands of the large
     1"space"  command allow the abort option  '  /
      data  texspy ( 557 )   /  '  "spy" (to return to the  "spy:"  prom
     1pt), or the option of backing up one     '  /
      data  texspy ( 558 )   /  '  level (via  "out").
     1'  /
      data  texspy ( 559 )   /  'key word no. 51:  "lunit4"      ----  -
     1---  ----                                '  /
      data  texspy ( 560 )   /  '  issue  this  command  to  connect  (f
     1ortran open), disconnect (fortran close),'  /
      data  texspy ( 561 )   /  '  position,  or  inspect  the  contents
     1of  the disk file of plot data which is'  /
      data  texspy ( 562 )   /  '  connected  to  i/o unit number  lunit
     14.   recall that a disk file can be given'  /
      data  texspy ( 563 )   /  '  solution  points  via  the  "disk"  o
     1r  "flush"  subcommands  of  the  "space"'  /
      data  texspy ( 564 )   /  '  command.  subcommand choices are as f
     1ollows: "open",  "close",  "top",  "bot",'  /
      data  texspy ( 565 )   /  '  "next",  "back",  and  "time".   in o
     1rder,  these shall now                   '  /
      data  texspy ( 566 )   /  '  be summarized:
     1'  /
      data  texspy ( 567 )   /  '       open -- subsequent prompts will
     1allow for a user-supplied file name      '  /
      data  texspy ( 568 )   /  '               (not to exceed 32 charac
     1ters), and the  "status"  choice         '  /
      data  texspy ( 569 )   /  '               between  "new"  and  "ol
     1d"  (vacuous or pre-defined files).      '  /
      data  texspy ( 570 )   /  '       close -- there are no parameters
     1; the distinction between  "keep"        '  /
      data  texspy ( 571 )   /  '                and  "delete"  is impli
     1cit (all opened files should be saved).  '  /
      data  texspy ( 572 )   /  '       top  --- to position the lunit4
     1disk file ready to read the beginning    '  /
      data  texspy ( 573 )   /  '                plot data (for smallest
     1time);                                  '  /
      data  texspy ( 574 )   /  '       bot  --- to position the lunit4
     1disk file after reading the final plot   '  /
      data  texspy ( 575 )   /  '                data (there is a read u
     1ntil an end-of-file).  certain computers '  /
      data  texspy ( 576 )   /  '                (e.g., vax) allow write
     1ing at this point, so old plot files can '  /
      data  texspy ( 577 )   /  '                be directly added to af
     1ter such an initial positioning.         '  /
      data  texspy ( 578 )   /  '       next --- to read and display the
     1plot points of the next time instant.   '  /
      data  texspy ( 579 )   /  '                this forward read is fa
     1st for all known computers.              '  /
      data  texspy ( 580 )   /  '       back --- to read and display plo
     1t points of the preceding time step.     '  /
      data  texspy ( 581 )   /  '                since this uses backspa
     1ce, be aware that for some computers     '  /
      data  texspy ( 582 )   /  '                (prime, apollo, etc.),
     1this command is internally converted to  '  /
      data  texspy ( 583 )   /  '                a  "top"  and  "time"
     1command which backs up two steps.        '  /
      data  texspy ( 584 )   /  '       time --- to position the lunit4
     1plot file immediately after having read  '  /
      data  texspy ( 585 )   /  '                points for the first st
     1ep at or beyond the user-specified time; '  /
      data  texspy ( 586 )   /  'key word no. 52:  "series"      ----  -
     1---  ----                                '  /
      data  texspy ( 587 )   /  '  issue this command in order to modify
     1the values of series r-l-c branches     '  /
      data  texspy ( 588 )   /  '  within the time-step loop.  such usag
     1e requires one unused row of both the    '  /
      data  texspy ( 589 )   /  '  branch table (list 2) and the branch-
     1parameter table (list 7) for each series '  /
      data  texspy ( 590 )   /  '  r-l-c branch of interest.  the  "seri
     1es"  command is coupled to the  "ramp"   '  /
      data  texspy ( 591 )   /  '  command in that  "series"  must be us
     1ed to select the branches of interest    '  /
      data  texspy ( 592 )   /  '  or potential interest ahead of time,
     1while it is  "ramp"  that actually varies'  /
      data  texspy ( 593 )   /  '  them when the time comes.  this would
     1be for continuous variation (ramping),  '  /
      data  texspy ( 594 )   /  '  which is the most common case of inte
     1rest.  for step changes (single-time     '  /
      data  texspy ( 595 )   /  '  occurances),  "ramp"  is not used.  i
     1n either case,  actual changes to [y]    '  /
      data  texspy ( 596 )   /  '  occur just prior to factoring of [y]
     1in  "subts1"  of overlay 16.  previously,'  /
      data  texspy ( 597 )   /  '  the user must issue a spy call for  "
     1series"  ----   at any point prior to    '  /
      data  texspy ( 598 )   /  '  overlay 12.  this is remembered, so t
     1hat the emtp will later automatically    '  /
      data  texspy ( 599 )   /  '  break in the middle of  "over12"  wit
     1h a message that this is the time to     '  /
      data  texspy ( 600 )   /  '  define the table of series r-l-c bran
     1ches which might later be tampered with. '  /
      data  texspy ( 601 )   /  '  at the automatic break in  "over12",
     1the user chooses among the commands     '  /
      data  texspy ( 602 )   /  '  "show",  "extra",  "change",  "step",
     1"rewind",  and  "spy".   "show"  will  '  /
      data  texspy ( 603 )   /  '  display the table of series r-l-c bra
     1nches which have thus far been considered'  /
      data  texspy ( 604 )   /  '  for possible later change,  with  "ex
     1tra"  yielding an extension to the       '  /
      data  texspy ( 605 )   /  '  display (a second table,  giving star
     1ting and next values).   "change"  is the'  /
      data  texspy ( 606 )   /  '  gateway to various further choices  (
     1"data",  "move",  "blank",  "use",       '  /
      data  texspy ( 607 )   /  '  "value",  "end",  and  "spy")  for th
     1e definition and manipulation of the     '  /
      data  texspy ( 608 )   /  '  table of  "show".   quickly summarizi
     1ng these,  "data"  is used to copy series'  /
      data  texspy ( 609 )   /  '  r-l-c branches into the tamper table,
     1while  "move"  will copy from one row of'  /
      data  texspy ( 610 )   /  '  the tamper table to another, and  "bl
     1ank"  will erase any such row.   "use"   '  /
      data  texspy ( 611 )   /  '  allows the user to toggle the activit
     1y status of any entry in the tamper      '  /
      data  texspy ( 612 )   /  '  table,  while  "value"  allows modifi
     1cation of r-l-c parameters in case this  '  /
      data  texspy ( 613 )   /  '  is to be done discontinuously.   "end
     1"  moves outward, back to the preceding  '  /
      data  texspy ( 614 )   /  '  prompt,  while  "spy"  aborts  "show"
     1and returns to the  "spy:"  prompt.    '  /
      data  texspy ( 615 )   /  '  this completes  "change"  usage.   ne
     1xt is  "step",  which is the command for '  /
      data  texspy ( 616 )   /  '  a manual, discontinuous change at the
     1next available opportunity (within one  '  /
      data  texspy ( 617 )   /  '  time step).   "rewind"  will erase th
     1e tamper table completely, so it is only '  /
      data  texspy ( 618 )   /  '  recommended if the existing table is
     1unsalvageable.   for details of the      '  /
      data  texspy ( 619 )   /  '  behind the ramping of series r-l-c el
     1ements, see vladimir"s article in the    '  /
      data  texspy ( 620 )   /  '  emtp newsletter, vol. 4, no. 2, novem
     1ber, 1983.                               '  /
      data  texspy ( 621 )   /  'key word no. 53:  "lock"        ----  -
     1---  ----                                '  /
      data  texspy ( 622 )   /  '  issue this command to disable time-sh
     1aring between  spy  and the ongoing emtp '  /
      data  texspy ( 623 )   /  '  simulation.  subsequent cancellation
     1of the command is by  "go".   since this '  /
      data  texspy ( 624 )   /  '  command is absolute, it can be repeat
     1ed any number of times.   besides this   '  /
      data  texspy ( 625 )   /  '  user-defined  "lock"  command,  there
     1are hidden, internal ones, such as the  '  /
      data  texspy ( 626 )   /  '  one by module  "pltfil"  when plot da
     1ta space has been exhausted.  but a word '  /
      data  texspy ( 627 )   /  '  of caution:  all of this talk about t
     1ime-sharing has meaning only for those   '  /
      data  texspy ( 628 )   /  '  computers which allow time-sharing (w
     1ith type-ahead that is not erased by the '  /
      data  texspy ( 629 )   /  '  user-keyed interrupt).
     1'  /
      data  texspy ( 630 )   /  'key word no. 54:  "[y]"         ----  -
     1---  ----                                '  /
      data  texspy ( 631 )   /  '  issue this command to display one or
     1more rows of the nodal admittance matrix '  /
      data  texspy ( 632 )   /  '  [y] of the time-step loop.   a subseq
     1uent prompt will request the desired row,'  /
      data  texspy ( 633 )   /  '  which may either be specified by bus
     1name (assumed to begin with a letter), or'  /
      data  texspy ( 634 )   /  '  by a node number.  alternate response
     1s are  "spy"  or  "end"  (to return to   '  /
      data  texspy ( 635 )   /  '  the  "spy:"  prompt),  "top"  (to dis
     1play the row for node 2),  "bot"  (to    '  /
      data  texspy ( 636 )   /  '  display the row for node  kpartb),  a
     1nd  "next"  or  nothing  (to display the '  /
      data  texspy ( 637 )   /  '  following row).   each nonzero term o
     1f the row will be displayed,  next to    '  /
      data  texspy ( 638 )   /  '  the associated column number.   a min
     1us sign applied to the column number is  '  /
      data  texspy ( 639 )   /  '  used to mark the largest column of th
     1e row.   in case separate  "examine"     '  /
      data  texspy ( 640 )   /  '  usage might be contemplated,  kks  po
     1ints to the  (km, ykm)  pairs.           '  /
      data  texspy ( 641 )   /  'key word no. 55:  "[f]"         ----  -
     1---  ----                                '  /
      data  texspy ( 642 )   /  '  issue this command to display one or
     1more rows of the table of factors (the   '  /
      data  texspy ( 643 )   /  '  triangularized [y] of the time-step l
     1oop).   display options are generally    '  /
      data  texspy ( 644 )   /  '  identical to the immediately-precedin
     1g  "[y]"  command,  except that here     '  /
      data  texspy ( 645 )   /  '  only the upper triangle exists,  a mi
     1nus sign marks the diagonal entry,  and  '  /
      data  texspy ( 646 )   /  '  kk  is the pointer (which can be obse
     1rved  via the  "bus"  command).          '  /
      data  texspy ( 647 )   /  'key word no. 55:  "noroll"      ----  -
     1---  ----                                '  /
      data  texspy ( 648 )   /  '  use this command to cancel  rolling
     1character and/or vector plots.   these   '  /
      data  texspy ( 649 )   /  '  began,  it should be recalled,  via
     1"rollc"  and/or  "rollv"  commands       '  /
      data  texspy ( 650 )   /  '  (independent of each other, and also
     1independent of the "tek" setting) at the '  /
      data  texspy ( 651 )   /  '  "inner:"  level of plotting.   the us
     1er-keyed interrupt will by itself stop   '  /
      data  texspy ( 652 )   /  '  the rolling only if control is within
     1"chrplt"  or  "tekplt"  plotting       '  /
      data  texspy ( 653 )   /  '  modules when it is spotted.  otherwis
     1e, this added manual request is needed.  '  /
      data  texspy ( 654 )   /  '  the only alternative would be to go b
     1ack into the  "inner:"  level of         '  /
      data  texspy ( 655 )   /  '  plotting to toggle the appropriate sw
     1itch (via  "rollc"  and/or  "rollv"      '  /
      data  texspy ( 656 )   /  '  requests).
     1'  /
      data  texspy ( 657 )   /  'key word no. 57:  "open"        ----  -
     1---  ----                                '  /
      data  texspy ( 658 )   /  '  issue this command to  open  a format
     1ted disk file.  a loop will be entered in'  /
      data  texspy ( 659 )   /  '  which there will be a prompt for the
     1next i/o unit number, and the disk       '  /
      data  texspy ( 660 )   /  '  file which is to be connected thereto
     1.  to exit the loop, send a blank line.  '  /
      data  texspy ( 661 )   /  'key word no. 58:  "close"       ----  -
     1---  ----                                '  /
      data  texspy ( 662 )   /  '  issue this command to  close  a disk
     1file.  a loop will be entered in         '  /
      data  texspy ( 663 )   /  '  which there will be a prompt for the
     1next i/o unit number, and the  status    '  /
      data  texspy ( 664 )   /  '  (either  keep  or  delete  are ok).
     1to exit the loop, send a blank line.     '  /
      data  texspy ( 665 )   /  'key word no. 59:  "sm"          ----  -
     1---  ----                                '  /
      data  texspy ( 666 )   /  '  issue this command to display the var
     1ious electrical, mechanical, and solution'  /
      data  texspy ( 667 )   /  '  parameters of any type-59 synchronous
     1machine (s.m.).  time-sharing by spy    '  /
      data  texspy ( 668 )   /  '  is automatically suspended during thi
     1s display, due to algorithmic limitations'  /
      data  texspy ( 669 )   /  '  of the type-59 model (examination is
     1only possible as that machine is being   '  /
      data  texspy ( 670 )   /  '  processed within subroutine update, w
     1hich is called by "subts1" (the first    '  /
      data  texspy ( 671 )   /  '  piece of the time-step loop.  the use
     1r can issue the  "sm"  request at any    '  /
      data  texspy ( 672 )   /  '  time, but in fact the emtp simulation
     1will stop within the processing loop of '  /
      data  texspy ( 673 )   /  '  "update", and allow the user to accep
     1t or reject (y or n responses) each      '  /
      data  texspy ( 674 )   /  '  machine in turn.  actually, only  "n"
     1vetos the opportunity (a simple <cr>   '  /
      data  texspy ( 675 )   /  '  will accept the next machine).  once
     1considering any one type-59 s.m., there  '  /
      data  texspy ( 676 )   /  '  are subsequent choices as to which pa
     1rameters the user wants to see:  "elec"  '  /
      data  texspy ( 677 )   /  '  for electrical parameters,  "mech"  f
     1or mechanical parameters, etc.           '  /
      data  texspy ( 678 )   /  '  ?????????   as of 26 feb 84, command
     1is not complete   ?????????????????????  '  /
      data  texspy ( 679 )   /  'key word no. 60:  "honk"        ----  -
     1---  ----                                '  /
      data  texspy ( 680 )   /  '  issue this command to ring the termin
     1al bell (i.e., honk its horn).  there    '  /
      data  texspy ( 681 )   /  '  will then be a prompt for an integer
     1severity level, which should be between  '  /
      data  texspy ( 682 )   /  '  one and ten (zero produces no respons
     1e, while 10 indicates a disaster).       '  /
      data  texspy ( 683 )   /  'key word no. 61:  "choice"      ----  -
     1---  ----                                '  /
      data  texspy ( 684 )   /  '  issue this command to see output vari
     1able names of any of the five classes    '  /
      data  texspy ( 685 )   /  '  making up the emtp output vector (1=n
     1ode voltages,  2=branch voltages,  etc.).'  /
      data  texspy ( 686 )   /  '  order of the five classes corresponds
     1to the order of concatenation in the    '  /
      data  texspy ( 687 )   /  '  emtp output vector;  and within each
     1class, the order corresponds, too.  the  '  /
      data  texspy ( 688 )   /  '  display is dynamic,  so it always ref
     1lects the current output variables       '  /
      data  texspy ( 689 )   /  '  (unlike  "choice"  at the  "middle:"
     1level of plotting,  which only displays '  /
      data  texspy ( 690 )   /  '  the original status of overlay 15).
     1'  /
      data  texspy ( 691 )   /  'key word no. 62:  "tacs"        ----  -
     1---  ----                                '  /
      data  texspy ( 692 )   /  '  issue this command as the gateway to
     1concurrent sequential processing (csp),  '  /
      data  texspy ( 693 )   /  '  which is based on special, spy-define
     1d tacs supplemental variable usage.  then'  /
      data  texspy ( 694 )   /  '  one enters a loop over csp control co
     1mmands.   if  "rewind"  is requested,    '  /
      data  texspy ( 695 )   /  '  all previous definitions are erased,
     1so definitions begin from level zero.  if'  /
      data  texspy ( 696 )   /  '  "source"  is sent,  all following inp
     1ut is assumed to be tacs source cards,   '  /
      data  texspy ( 697 )   /  '  until terminated by an  "end"  card.
     1if  "supplemental"  is sent,  then all '  /
      data  texspy ( 698 )   /  '  following data cards (spy input) is a
     1ssumed to be tacs supplemental variable/ '  /
      data  texspy ( 699 )   /  '  device data cards,  also until an  "e
     1nd"  card is encountered.  the subcommand'  /
      data  texspy ( 700 )   /  '  "patch"  allows the user to connect a
     1ny variables to his tacs sources  (to    '  /
      data  texspy ( 701 )   /  '  define the inputs),  and apply any su
     1pplemental variable results anywhere.    '  /
      data  texspy ( 702 )   /  '  the "any" comes from the use of a mem
     1ory address of the spy commands  "list"  '  /
      data  texspy ( 703 )   /  '  or  "find",  just as  "examine"  or
     1"deposit"  are quite unrestricted.  there'  /
      data  texspy ( 704 )   /  '  will be separate prompts for all such
     1tacs input and output connections.  when'  /
      data  texspy ( 705 )   /  '  all such connections are complete,  "
     1show"  can be used to display a summary  '  /
      data  texspy ( 706 )   /  '  table of all tacs csp input/output co
     1nnections.   working room for such usage '  /
      data  texspy ( 707 )   /  '  can be controlled by the user in seve
     1ral ways, if there are shortages.  first,'  /
      data  texspy ( 708 )   /  '  if a user wants dummy output channels
     1for printing/plotting, reserve dummy    '  /
      data  texspy ( 709 )   /  '  node voltage outputs with  "chan01"
     1punched in cols. 3-8, and the desired    '  /
      data  texspy ( 710 )   /  '  number punched in columns 9-16 with i
     18 format.   resultant names will be      '  /
      data  texspy ( 711 )   /  '  serialized from the aforementioned ro
     1ot, and added to the output vector at the'  /
      data  texspy ( 712 )   /  '  point of definition (if this card is
     1first, all such dummy names will come    '  /
      data  texspy ( 713 )   /  '  first, before other node voltages).
     1second, there is array dimensioning for  '  /
      data  texspy ( 714 )   /  '  all vectors of the  "choice"  table
     1---  presently fixed in deck "dekspy", at'  /
      data  texspy ( 715 )   /  '  the moment.  finally, in addition to
     1the familiar tacs table limits, there is '  /
      data  texspy ( 716 )   /  '  an automatic reservation of space for
     1all user-defined tacs csp sources.  this'  /
      data  texspy ( 717 )   /  '  is maximized automatically, within ex
     1isting tacs dimensions (as assigned by   '  /
      data  texspy ( 718 )   /  '  absolute tacs dimensions, for example
     1).  as usual,  "spy"  will break out of  '  /
      data  texspy ( 719 )   /  '  any loop, in case an abortion is desi
     1red.                                     '  /
      data  texspy ( 720 )   /  'key word no. 63:  "wait"        ----  -
     1---  ----                                '  /
      data  texspy ( 721 )   /  '  the response to this spy command will
     1be a prompt for the desired delay       '  /
      data  texspy ( 722 )   /  '  (hibernation) time in seconds.  once
     1this is accepted, the installation-      '  /
      data  texspy ( 723 )   /  '  dependent subroutine tdelay  is calle
     1d, to return control only after the      '  /
      data  texspy ( 724 )   /  '  requested nap.
     1'  /
      data  texspy ( 725 )   /  'key word no. 64:  "v-i""        ----  -
     1---  ----                                '  /
      data  texspy ( 726 )   /  '  this spy command is used to examine t
     1he list-10 characteristics of all        '  /
      data  texspy ( 727 )   /  '  nonlinear and pseudo-nonlinear elemen
     1ts.  the name  "v-i"  is symbolic        '  /
      data  texspy ( 728 )   /  '  only, since flux-current or resistanc
     1e-time characteristics are also          '  /
      data  texspy ( 729 )   /  '  included.  immediately upon entry, th
     1ere will be a display of the number      '  /
      data  texspy ( 730 )   /  '  of nonlinear elements (list 9) and da
     1ta points of the characteristics         '  /
      data  texspy ( 731 )   /  '  (list 10).  also shown will be a list
     1of options to a pair of free-format     '  /
      data  texspy ( 732 )   /  '  row numbers:
     1'  /
      data  texspy ( 733 )   /  '     next or <cr>  ---- to display the
     1characteristic of the next element       '  /
      data  texspy ( 734 )   /  '                        of the nonlinea
     1r element table.  there is wrap around   '  /
      data  texspy ( 735 )   /  '                        from beginning
     1to end.  no initialization required.     '  /
      data  texspy ( 736 )   /  '     last  ---- to display the characte
     1ristic of the preceding element of       '  /
      data  texspy ( 737 )   /  '                the nonlinear element t
     1able.  there is wrap around from the     '  /
      data  texspy ( 738 )   /  '                beginning to the end.
     1no initialization is required.           '  /
      data  texspy ( 739 )   /  '     all  --- to display all rows of th
     1e list 10 characteristics.               '  /
      data  texspy ( 740 )   /  '     spy, end, or stop ---- to abort di
     1splay loop, and return to  "spy:"        '  /
      data  texspy ( 741 )   /  '     mode  ---- to toggle the binary sw
     1itch which selects between a display     '  /
      data  texspy ( 742 )   /  '                by elements (value n8=0
     1), and a display by rows of list 10.     '  /
      data  texspy ( 743 )   /  '                initially, the display
     1by elements, only one at a time, is      '  /
      data  texspy ( 744 )   /  '                assumed.
     1'  /
      data  texspy ( 745 )   /  '  in case the use inputs a pair of numb
     1ers instead of one of these key          '  /
      data  texspy ( 746 )   /  '  words, then the response depends on
     1mode.   for the display by elements,     '  /
      data  texspy ( 747 )   /  '  the first list-9 entry with a charact
     1eristic contained within the range       '  /
      data  texspy ( 748 )   /  '  [n1, n2]  is identified, and displaye
     1d (where n1 and n2 are the ordered       '  /
      data  texspy ( 749 )   /  '  pair of free-format numbers which the
     1user has inputted.  on the other hand,  '  /
      data  texspy ( 750 )   /  '  with  mode  toggled for the absolute,
     1unlimited display of list 10 rows,      '  /
      data  texspy ( 751 )   /  '  then these rows (within limits of ava
     1ilability) are displayed.                '  /
      data  texspy ( 752 )   /  '  the following plot commands are honor
     1ed at all three levels (either  "outer:",'  /
      data  texspy ( 753 )   /  '  "middle",  or  "inner:")  of plot dia
     1logue:                                   '  /
      data  texspy ( 754 )   /  '  stop  ---  to abort  "plot"  activity
     1,  and return to the  "spy:"  prompt.    '  /
      data  texspy ( 755 )   /  '  spy   ---  to abort  "plot"  activity
     1,  and return to the  "spy:"  prompt.    '  /
      data  texspy ( 756 )   /  '  in   ----  to transfer one level inwa
     1rd  (either from  "outer"  to  "middle:" '  /
      data  texspy ( 757 )   /  '             or from  "middle"  to  "in
     1ner")  in the plot dialogue.             '  /
      data  texspy ( 758 )   /  '  help  ---  to produce the display now
     1being viewed.  if unqualified, messages '  /
      data  texspy ( 759 )   /  '             will be restricted to the
     1level of current operation.  but one     '  /
      data  texspy ( 760 )   /  '             blank and then a qualifier
     1("all", "outer", "middle", or "inner")  '  /
      data  texspy ( 761 )   /  '             provide the opportunity to
     1see messages of other levels, too.      '  /
      data  texspy ( 762 )   /  '  debug  --  to alter the level of diag
     1nostic printout.  if none is now being   '  /
      data  texspy ( 763 )   /  '             used, this command will tu
     1rn it on.  in fact, the control is       '  /
      data  texspy ( 764 )   /  '             identical to that used in
     1response to the  "spy:"  prompt (iprspy).'  /
      data  texspy ( 765 )   /  '               < < <  =======   end of
     1any-level  commands   =======  > > >     '  /
      data  texspy ( 766 )   /  '  set data -- to over-ride default data
     1values with user-established choices.   '  /
      data  texspy ( 767 )   /  '              the associated user-defin
     1ed data file must be pre-stored on disk, '  /
      data  texspy ( 768 )   /  '              and must have the file na
     1me  "tpparam.dat".                       '  /
      data  texspy ( 769 )   /  '  tek  ---- to toggle the switch which
     1chooses between character plotting and   '  /
      data  texspy ( 770 )   /  '            vector-graphic (e.g., tektr
     1onix) plotting.  the default may be      '  /
      data  texspy ( 771 )   /  '            computer dependent, or come
     1from  "set data"  usage.                '  /
      data  texspy ( 772 )   /  '  column -- to toggle the switch that c
     1hooses between 80 and 132-column widths  '  /
      data  texspy ( 773 )   /  '            for character plotting.
     1'  /
      data  texspy ( 774 )   /  '  set column -- to set character plot w
     1idths to any value.  but if either 80 or '  /
      data  texspy ( 775 )   /  '                132 is desired (common
     1choices), use the simpler  "column".     '  /
      data  texspy ( 776 )   /  '               < < <  =======   end of
     1outer-level  commands   =======  > > >   '  /
      data  texspy ( 777 )   /  '  timespan -- to be shown the time rang
     1e  [t-min, t-max]  of the plot data.     '  /
      data  texspy ( 778 )   /  '  choice -- to produce a tabulation of
     1plotable emtp variables.   but be very   '  /
      data  texspy ( 779 )   /  '            careful, since the resultan
     1t tabulation corresponds to the status   '  /
      data  texspy ( 780 )   /  '            before the time-step loop w
     1as entered.  for a dynamic tabulation,   '  /
      data  texspy ( 781 )   /  '            use  "choice"  in response
     1to the  "spy:"  prompt.                  '  /
      data  texspy ( 782 )   /  '  time units -- to specify the integer
     1which characterizes the time units used  '  /
      data  texspy ( 783 )   /  '                in the plotting.  this
     1is just as with batch-mode emtp plotting,'  /
      data  texspy ( 784 )   /  '                where:  "1"  -- for deg
     1rees based on the power frequency;       '  /
      data  texspy ( 785 )   /  '                        "2"  -- for cyc
     1les  based on the power frequency;       '  /
      data  texspy ( 786 )   /  '                        "3,4,5"  -- for
     1seconds, milliseconds, and microseconds '  /
      data  texspy ( 787 )   /  '                        "6,7" -- for  "
     1frequency scan"  data cases, using either'  /
      data  texspy ( 788 )   /  '                                 hz or
     1the log to base 10 of hz,  rather than t.'  /
      data  texspy ( 789 )   /  '  x   ----- any garbage character (incl
     1uding blank) represents a request for the'  /
      data  texspy ( 790 )   /  '            program to begin the interr
     1ogation for plot variables and labeling. '  /
      data  texspy ( 791 )   /  '            hereafter, in response to t
     1he different prompts, several special    '  /
      data  texspy ( 792 )   /  '            key words are applicable (i
     1mmediately below).  upon the completion  '  /
      data  texspy ( 793 )   /  '            of all such information, th
     1ere is an automatic transfer to the      '  /
      data  texspy ( 794 )   /  '            "inner:"  level of program
     1dialogue.  but first, options are:       '  /
      data  texspy ( 795 )   /  '    repeat >> to reuse all former plot
     1labels, send  "repeat"  when first asked '  /
      data  texspy ( 796 )   /  '              for label information (wh
     1ich begins with the super title).  for   '  /
      data  texspy ( 797 )   /  '              the first plot, "former l
     1abels" are all blank (initialization).   '  /
      data  texspy ( 798 )   /  '    back >>>> to abort input of plot va
     1riables, and return to "middle:" prompt. '  /
      data  texspy ( 799 )   /  '    end  >>>> to terminate all indeterm
     1inate inputs.  included are the three    '  /
      data  texspy ( 800 )   /  '              classes of plot variables
     1, and lines of case-title text.          '  /
      data  texspy ( 801 )   /  '    last >>>> to terminate all plot-var
     1iable input, and begin specifying labels.'  /
      data  texspy ( 802 )   /  '    blank >>> sending nothing (just a c
     1arriage return) will usually reuse the   '  /
      data  texspy ( 803 )   /  '              old datum (e.g., node nam
     1e, or line of case-title text).  old data'  /
      data  texspy ( 804 )   /  '              are usually displayed wit
     1hin parentheses for this reason.         '  /
      data  texspy ( 805 )   /  '    flush >>> to rewind the pointer whi
     1ch counts the lines of case-title text.  '  /
      data  texspy ( 806 )   /  '              the text then remains, to
     1be accepted or overwritten line by line.'  /
      data  texspy ( 807 )   /  '    playback> to display the entire pre
     1sent case title.  this command is legal  '  /
      data  texspy ( 808 )   /  '              at any point before the
     1"end"  which freezes the case title.     '  /
      data  texspy ( 809 )   /  '  label  -- to skip around the plot-var
     1iable specification (see "x" above), and '  /
      data  texspy ( 810 )   /  '            begin the input of plot lab
     1els.  plot variables remain unchanged.   '  /
      data  texspy ( 811 )   /  '               < < <  =======   end of
     1middle-level  commands   =======  > > >  '  /
      data  texspy ( 812 )   /  '  extrema -- to toggle the switch that
     1decides whether or not variable extrema  '  /
      data  texspy ( 813 )   /  '             of subsequent plots are to
     1be displayed.  such output precedes the '  /
      data  texspy ( 814 )   /  '             associated plot (as does t
     1hat of  "level"  below).  the program    '  /
      data  texspy ( 815 )   /  '             then pauses before drawing
     1the graph, waiting for the user to send '  /
      data  texspy ( 816 )   /  '             a blank.  if the user want
     1s to skip the plot and return to the     '  /
      data  texspy ( 817 )   /  '             "middle:"  level of dialog
     1ue, send  "no plot".                     '  /
      data  texspy ( 818 )   /  '  level -- to toggle the switch that de
     1cides whether or not level-triggers for  '  /
      data  texspy ( 819 )   /  '           variables are to be activate
     1d.  if such triggers are being turned on,'  /
      data  texspy ( 820 )   /  '           the program will next reques
     1t a level vector.  the response is using '  /
      data  texspy ( 821 )   /  '           free-format.  the same pause
     1exists as with  "extrema".              '  /
      data  texspy ( 822 )   /  '  smooth -- to change the tolerance whi
     1ch is used to discard plot points.       '  /
      data  texspy ( 823 )   /  '  size   -- to change the length of the
     1time axis of character plots.           '  /
      data  texspy ( 824 )   /  '  show   -- to display the current valu
     1es of many important plot parameters.    '  /
      data  texspy ( 825 )   /  '  factor -- to specify a new vector of
     1multiplicative scaling factors for plot  '  /
      data  texspy ( 826 )   /  '            variables (the "a" of  z =
     1a*y + b ).   zero is taken to mean unity,'  /
      data  texspy ( 827 )   /  '            and free-format is used.
     1'  /
      data  texspy ( 828 )   /  '  offset -- like  "factor",  only to sp
     1ecify the vector of constants  "b".      '  /
      data  texspy ( 829 )   /  '  rescale-- to return to natural scalin
     1g (i.e.,  a=1.0 ,  b=0.0 ) for all plot  '  /
      data  texspy ( 830 )   /  '            variables.  this is used to
     1erase previous  "factor"  or  "offset". '  /
      data  texspy ( 831 )   /  '  limits -- to specify new minimum and
     1maximum values for the vertical axis of  '  /
      data  texspy ( 832 )   /  '            plots.  send  "0,0"  to can
     1cel previous manually-specified limits,  '  /
      data  texspy ( 833 )   /  '            and return to the automatic
     1scaling of the computer.                '  /
      data  texspy ( 834 )   /  '  average-- to change the limit on the
     1number of consecutive oscillations after '  /
      data  texspy ( 835 )   /  '            which the averaging of succ
     1essive ordinates is to be instituted.    '  /
      data  texspy ( 836 )   /  '  time   -- to specify time-axis limits
     1t-min  and  t-max  of the plot.  this  '  /
      data  texspy ( 837 )   /  '            must be done at least once,
     1for the first plot, unless  "timespan"  '  /
      data  texspy ( 838 )   /  '            was issued at the  "middle:
     1"  level (resulting in full time range). '  /
      data  texspy ( 839 )   /  '  all time--this is a request for a plo
     1t over the full time range of the data.  '  /
      data  texspy ( 840 )   /  '            it functions correctly only
     1if  "timespan"  was previously used at  '  /
      data  texspy ( 841 )   /  '            the  "middle:"  level.
     1'  /
      data  texspy ( 842 )   /  '  blank  -- a blank (just a <cr>) is in
     1terpreted as a request for a plot using  '  /
      data  texspy ( 843 )   /  '            time-axis scaling as for th
     1e preceding plot (or the full range, if  '  /
      data  texspy ( 844 )   /  '            no previous plots existed,
     1but  "timespan"  was ordered).           '  /
      data  texspy ( 845 )   /  '  cursor -- to toggle the switch that i
     1ndicates whether or not cursor input is  '  /
      data  texspy ( 846 )   /  '            expected after the next vec
     1tor plot.  when the cursor is switched to'  /
      data  texspy ( 847 )   /  '            "on",  following the plot t
     1here will be keyboard input following the'  /
      data  texspy ( 848 )   /  '            positioning of the cursor:
     1'  /
      data  texspy ( 849 )   /  '     p  >>>>> to mark another cursor po
     1int (wherever the cursor is sitting)     '  /
      data  texspy ( 850 )   /  '     e  >>>>> to terminate such marking
     1input (end of cursor use, actually).    '  /
      data  texspy ( 851 )   /  '     show >>> to produce a tabulation o
     1f all cursor points (previous "p" usage).'  /
      data  texspy ( 852 )   /  '     slope >> to produce  dx,  dy,  f,
     1and  dy/dx  of any pair of points that  '  /
      data  texspy ( 853 )   /  '              the user is interested in
     1.  after the last such  (m,k)  pair of   '  /
      data  texspy ( 854 )   /  '              point numbers which are o
     1f interest,  send  "0,0"  to terminate.  '  /
      data  texspy ( 855 )   /  '     end  >>> to terminate all cursor d
     1isplays, and return to  "inner:"  prompt.'  /
      data  texspy ( 856 )   /  '  x-y plot--to toggle the switch which
     1chooses between regular plotting as a    '  /
      data  texspy ( 857 )   /  '            function of time (the defau
     1lt), and x-y plotting.                   '  /
      data  texspy ( 858 )   /  '  rollv  -- to toggle the switch which
     1produces a  rolling  vector plot.   it is'  /
      data  texspy ( 859 )   /  '            normal to turn on such roll
     1ing after the basic plot is in place, and'  /
      data  texspy ( 860 )   /  '            the axis scaling is judged
     1to be appropriate.  then, after  "rollv",'  /
      data  texspy ( 861 )   /  '            the user will normally send
     1"spy"  to return to the  "spy:"  prompt'  /
      data  texspy ( 862 )   /  '            (from which  "go"  will be
     1required to actually make the plot  roll,'  /
      data  texspy ( 863 )   /  '            if  "break"  or  "lock"  we
     1re in effect at that point).   should the'  /
      data  texspy ( 864 )   /  '            user wish to cancel a rolli
     1ng vector plot from spy, send  "noroll"  '  /
      data  texspy ( 865 )   /  '            (which will stop both  roll
     1ing  vector and character plots).        '  /
      data  texspy ( 866 )   /  '  rollc  -- to toggle the switch which
     1produces a  rolling  character plot.  see'  /
      data  texspy ( 867 )   /  '            preceding  "rollv"  for det
     1ails (functioning is comparable).        '  /
      data  texspy ( 868 )   /  '               < < <  =======   end of
     1inner-level  commands   =======  > > >   '  /
      data  kbegtx (  1 )   /     1   /
      data  kbegtx (  2 )   /     4   /
      data  kbegtx (  3 )   /     9   /
      data  kbegtx (  4 )   /    22   /
      data  kbegtx (  5 )   /    29   /
      data  kbegtx (  6 )   /    41   /
      data  kbegtx (  7 )   /    53   /
      data  kbegtx (  8 )   /    64   /
      data  kbegtx (  9 )   /    69   /
      data  kbegtx ( 10 )   /    79   /
      data  kbegtx ( 11 )   /    88   /
      data  kbegtx ( 12 )   /    97   /
      data  kbegtx ( 13 )   /   112   /
      data  kbegtx ( 14 )   /   119   /
      data  kbegtx ( 15 )   /   136   /
      data  kbegtx ( 16 )   /   141   /
      data  kbegtx ( 17 )   /   154   /
      data  kbegtx ( 18 )   /   158   /
      data  kbegtx ( 19 )   /   162   /
      data  kbegtx ( 20 )   /   202   /
      data  kbegtx ( 21 )   /   217   /
      data  kbegtx ( 22 )   /   228   /
      data  kbegtx ( 23 )   /   237   /
      data  kbegtx ( 24 )   /   242   /
      data  kbegtx ( 25 )   /   252   /
      data  kbegtx ( 26 )   /   263   /
      data  kbegtx ( 27 )   /   294   /
      data  kbegtx ( 28 )   /   303   /
      data  kbegtx ( 29 )   /   317   /
      data  kbegtx ( 30 )   /   325   /
      data  kbegtx ( 31 )   /   333   /
      data  kbegtx ( 32 )   /   340   /
      data  kbegtx ( 33 )   /   345   /
      data  kbegtx ( 34 )   /   361   /
      data  kbegtx ( 35 )   /   377   /
      data  kbegtx ( 36 )   /   381   /
      data  kbegtx ( 37 )   /   425   /
      data  kbegtx ( 38 )   /   436   /
      data  kbegtx ( 39 )   /   446   /
      data  kbegtx ( 40 )   /   449   /
      data  kbegtx ( 41 )   /   453   /
      data  kbegtx ( 42 )   /   456   /
      data  kbegtx ( 43 )   /   467   /
      data  kbegtx ( 44 )   /   471   /
      data  kbegtx ( 45 )   /   482   /
      data  kbegtx ( 46 )   /   486   /
      data  kbegtx ( 47 )   /   490   /
      data  kbegtx ( 48 )   /   497   /
      data  kbegtx ( 49 )   /   503   /
      data  kbegtx ( 50 )   /   514   /
      data  kbegtx ( 51 )   /   559   /
      data  kbegtx ( 52 )   /   586   /
      data  kbegtx ( 53 )   /   621   /
      data  kbegtx ( 54 )   /   630   /
      data  kbegtx ( 55 )   /   641   /
      data  kbegtx ( 56 )   /   647   /
      data  kbegtx ( 57 )   /   657   /
      data  kbegtx ( 58 )   /   661   /
      data  kbegtx ( 59 )   /   665   /
      data  kbegtx ( 60 )   /   679   /
      data  kbegtx ( 61 )   /   683   /
      data  kbegtx ( 62 )   /   691   /
      data  kbegtx ( 63 )   /   720   /
      data  kbegtx ( 64 )   /   725   /
      data  kbegtx ( 65 )   /   752   /
      data  kbegtx ( 66 )   /   766   /
      data  kbegtx ( 67 )   /   777   /
      data  kbegtx ( 68 )   /   812   /
      data  kbegtx ( 69 )   /   869   /
      end
c
c     end of file: blockdspy.for
c
