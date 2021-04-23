  !-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
  !
  !     file: main00.for
  !
program gemtp
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  include 'volt45.ftn'
  !**********************************************************************
  !                                                                     *
  !    --------------- Electromagnetic Transients Program ------------  *
  !                    methods development branch, route eogb           *
  !                    division of system engineering                   *
  !                    Bonneville Power Administration                  *
  !                    P. O. Box 3621                                   *
  !                    Portland, Oregon  97208                          *
  !                    U.S.A.     phone: (503) 230-4404                 *
  !                                                                     *
  !    The fortran comment-card text now being read represents a        *
  !    summary introduction and explanation which applies to a very     *
  !    large program development referred to by the title of            *
  !    'electromagnetic transients program'  (abbreviated  'EMTP' ,     *
  !    or   't.p.'  in the older notation).                             *
  !                                                                     *
  !    In general terms, the purpose of this work is to simulate        *
  !    transient phenomena in power system networks, taking into        *
  !    account traveling waves (electromagnetic transients) on          *
  !    transmission lines, saturation of transformers, nonlinearities   *
  !    of surge arresters, etc.   While not so restricted in theory,    *
  !    the most common program application is for the simulation of     *
  !    switching surges which propagate on power network transmission   *
  !    lines.   for a more detailed explantion of the modeling          *
  !    techniques which are used, the reader is referred to the         *
  !    manual for this program (840 pages for the version dated         *
  !    march, 1983).  ).    while older issues were titled              *
  !    "EMTP user's manual",  beginning in september of 1980            *
  !    this work is now called the  "EMTP rule book" .                  *
  !                                                                     *
  !     The utpf is a large 80-column bcd card-image file, to be used    *
  !     as input to e/t programs.   E/t programs machine translate this  *
  !     utpf code into legal fortran for most computer systems of        *
  !     interest (ibm, cdc, univac, honeywell, dec pdp-10, dec vax-11,   *
  !     prime, sel, apollo, hitachi, facom, harris, etc.).               *
  !                                                                      *
  !     In conformity with long-standing bpa policy, as well as the      *
  !    more recent (february 19, 1975) federal freedom of information   *
  !    act, dissemination of these program materials is freely made     *
  !    to any and all interested parties.   a fee to cover reproduction,*
  !    handling, and mailing costs may be assessed against the          *
  !    organization receiving the material, however.   No claim or      *
  !    warranty as to the usefulness, accuracy, fidelity, or            *
  !    completeness of these materials is (or ever has been) in any     *
  !    way expressed or implied.                                        *
  !                                                                      *
  !**********************************************************************
  character(32) arg
  data  ll34   /  34  /
  !     unit assignments of "over1" needed earlier by spy:
  options_count = command_argument_count()
  do i = 1, iargc()
     call getarg(i, arg)
     write (*, *) arg
  end do
  lunit0 = gfortran_err_unit
  lunit1 = 1
  lunit2 = 2
  lunit3 = 3
  lunit4 = 4
  lunit5 = gfortran_stdin_unit
  lunit6 = gfortran_stdout_unit
  lunit7 = 7
  lunit8 = 8
  lunit9 = 9
  lunt10 = 10
  lunt11 = 11
  lunt12 = 12
  lunt13 = 13
  lunt14 = 14
  lunt15 = 15
  llbuff = -3333
  kol132 = 132
  nchain = -1
  lastov = 0
  kill = 0
2000 if ( kill  .eq.  0 )   go to 2001
  if ( kill  .eq.  9999 )   go to 2001
  if ( kill .ne. 7733 )  go to 4372
  write (lunit6, 4367)
4367 format (' "main00" intercept of "begin" request.')
  kill = 0
  numdcd = 0
  nchain = 1
  go to 1983
4372 if ( nchain  .gt.  51 )   go to 2001
  nchain = 51
2001 n1 = nchain
  if ( n1  .gt.  30 )   n1 = n1 - 30
  if ( n1  .le.  0 )  n1 = 1
  iprsup = iprsov(n1)
  if (nchain .gt. 20)   go to 2010
  if (nchain .eq. 12 .or. nchain .eq. 2) go to 1983
  if (nchain .eq. -1) call move0(iprsov(1), ll34)
  call erexit
  nchain = 0
  if (nchain .gt. 20)   go to 2000
1983 call main10
  go to 2000
2010 if ( m4plot .eq. 1 ) call emtspy
  if ( nchain  .gt.  29 )   go to 2020
  call over29
  go to 2000
2020 if ( nchain  .gt.  31 )   go to 2024
  call over31
  go to 2000
2024 if ( nchain  .gt.  39 )   go to 2025
  call over39
  go to 2000
2025 if ( nchain  .gt.  41 )   go to 2070
  call over41
  go to 2000
2070 if ( nchain  .gt.  42 )   go to 2080
  call over42
  go to 2000
2080 if (nchain .gt. 44) go to 2100
  call over44
  go to 2000
2100 if ( nchain  .gt.  45 )   go to 2110
  call over45
  go to 2000
2110 if ( nchain  .gt.  47 )   go to 2130
  call over47
  go to 2000
2130 if ( nchain  .gt.  51 )   go to 2260
  call over51
  go to 2000
2260 if ( nchain  .gt.  52 )   go to 2270
  call over52
  go to 2000
2270 if ( nchain  .gt.  53 )   go to 2280
  call over53
  go to 2000
2280 if ( nchain  .gt.  54 )   go to 2290
  call over54
  go to 2000
2290 if ( nchain  .gt.  55 )   go to 2300
  call over55
  go to 2000
2300 write (lunit6, 9236)  nchain
9236 format (/, ' Illegal nchain in main00.', i8)
  go to 2000
  !
  !    The present module  main00  is always in memory.   It is the
  !    highest level module of a program which has two levels of
  !    overlaying.   It calls primary level overlays only (directly),
  !    based on the value of variable  'nchain' .   The following
  !     legitimate values, and the meaning of the associated overlay
  !     calls, exist .....
  !     1-20.  for overlays 1, 2, ..., 20, which are secondary-level
  !            overlays, a call must be first made to the controlling
  !            primary-level overlay.   thus for such  'nchain'  values,
  !            control is transfered first to module  main10 .   this
  !            is the only case where calls to overlays are not made
  !            directly.
  !
  !     29.  completion of statistics (monte carlo) study, where variable
  !          maxima of the different case solutions are read off the
  !          disk, and are processed statistically to produce
  !          cumulative distribution functions, etc.
  !
  !     31.  plot routine, for graphical output of transients.
  !          the program also terminates execution here, usually,
  !          after writing an end-of-information mark on the
  !          plot tape (whether or not the user has plotted anything).
  !
  !     39.  supporting routine which generates EMTP branch
  !          cards for the frequency-dependent representation of
  !          an untransposed transmission line.   this is the
  !          "marti setup"  code, named after dr. jose marti of
  !          vancouver and caracas (see 1981 ieee pica paper).
  !
  !     41.  supporting routine which calculates transformer matrices  (r)
  !          and  (l)  from short-circuit and open-circuit data.
  !
  !     42.  supporting routine which converts an rms voltage vs. current
  !          saturation characteristic into an instantaneous flux vs.
  !          current characteristic.
  !
  !     43.  supporting routine which calculates weighting functions
  !          a1(t)  and  a2(2)  for the zero-sequence mode of a
  !          distributed line which has frequency-dependent line
  !          constants  r  and  l .
  !
  !     44.  supporting routine which calculates line constants for
  !          overhead transmission lines by means of carson's formula.
  !          this is a modified version of what was originally (until
  !          january 1975) the completely-separate bpa line-constants
  !          program.
  !
  !     45.  supporting routine of  'semlyen setup'  code.   the output
  !          is a group of punched cards, as are required for the EMTP
  !          simulation of a transmission circuit using  semlyen
  !          recursive convolution modeling.
  !
  !     47.  supporting routine of  'cable constants'  code.   the
  !          primary function is to calculate  (r),  (l),  %  (c)
  !          matrices for a multi-phase system of single-core coaxial
  !          cables.
  !
  !     51.  printing of introductory paragraph of error-message
  !          termination ('you lose, fella, ... '), plus error-message
  !          texts for  'kill'  codes numbered  1  through  50 .
  !          the exiting linkage is to the last error overlay.
  !     52.  error message texts for  'kill'  codes numbered  51
  !          the exiting linkage is to the last error overlay.
  !     53.  error message texts for  'kill'  codes numbered  91
  !          through  150.  the exiting linkage is to the last
  !          error overlay.
  !     54.  error message texts for  'kill'  codes numbered  151
  !          through  200.   the exiting linkage is to the
  !          last error overlay.
  !     55.  final error overlay.  messages for  kill = 201
  !          onward are contained, as well as summary statistics
  !          --- table sizes and timing figures for the run.
  !          the exiting linkage is generally to module  over1  (to read
  !          a new data case), but may be to module  over31 (for final
  !          case termination).
end program
!
!     subroutine stoptp.
!
subroutine stoptp
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Temporary stop statements of EMTP have been converted to
  !     "call stoptp", allowing installation-dependent clean up.
  include  "blkcom.ftn"
  read (unit = abuff, fmt = 5607) texcol
5607 format (80a1)
  if (nchain .eq. 31 .and. lastov .eq. 1 .and. kill .eq. 9999) go to 9000
  write (lunit6, 5623)  nchain, lastov, texcol
5623 format (/, ' Temporary error stop in "stoptp".', '   nchain, lastov =', 2i5, 5x, 'Last-read card image abuff follows ....', /, 80a1)
9000 stop
end subroutine stoptp
!
! subroutine copyr.
!
subroutine copyr(d1, to, kk)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Routine which copies the same floating-point word  'd1'  into a
  !     contiguous region of memory ----  'kk'  words in length,
  !     beginning with word  to(1) .
  dimension  to(1)
  do i = 1, kk ! do 5431
     to(i) = d1    ! 5431
  end do
  return
end subroutine copyr
!
! subroutine copyi.
!
subroutine copyi(n1, ito, kk)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     routine which copies the same integer word  'n1'  into a
  !     contiguous region of memory ----  'kk'  words in length,
  !     beginning with word  ito(1) .
  dimension  ito(1000)
  do i = 1, kk
     ito(i) = n1
  end do
  return
end subroutine copyi
!
!    subroutine copya.
!
subroutine copya ( text1, text2, kk )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     routine which copies the same alphanumeric word  'text1'  into
  !     a contiguous region of memory ----  'kk'  words in length,
  !     beginning with word  text2(1) .
  !     real*8         text1, text2
  character*8 text1, text2
  dimension  text2(1)
  do i = 1, kk
     text2(i) = text1
  end do
  return
end subroutine copya
!
!     subroutine erexit.
!
subroutine erexit
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     VAX-11   installation-dependent EMTP module.   This is
  !     called by the top of "main00", before any emtp data input.
  include 'blkcom.ftn'
  !include 'dekspy.ftn'
  !     dimension idum(3)   !  dummy vector for ctrl-c handling
  external kwiter       ! needed for ctrl-c initialization
  common /comkwt/  kwtvax  ! magic block for vax/vms ctrl-c
  lunit6 = gfortran_stdout_unit  ! for use of "prompt" until fixed tt?? address
  muntsv(2) = 49  ! alternate munit5 unit number of spy
  kwtvax = 0    ! set flag corresponding to no ctrl-c usage
  !      call enable_ctrl_c ( kwiter, idum(1) )  ! initialize ctrl-c
  call datain    ! read data in, process $include, spy, etc.
  return
end subroutine erexit
!
!     subroutine runtym.
!
subroutine runtym (d1, d2)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !    This subroutine returns with the current job-execution time, as
  !    broken down into two categories ....
  !           d1 = central processor job time, in seconds
  !           d2 = peripheral processor (or input/output) job time,
  !                in seconds.
  !    If two such figures are not available on the user's computer,
  !    'd2'  should be set to zero so that case-summary statistics
  !    will not print out garbage values.   Such is the only use of
  !    the values gotten by calling this subroutine ---- for the case-
  !    summary printout.   Hence if one wants to convert time into
  !    dollars, or some other measure of job effort, it is easily done.
  !     Include  '[scott]commuk.for' --- share with "settym" in-line:
  common /timers/ cputime
  integer*4 cputime
  common /timer2/ l4cpu, cputime_code, cputime_adr, zero, zerofin
  real*8 now_cputime
  integer*4 zero, zerofin, time
  !     if (.not.sys$getjpi(,,,l4cpu,,,)) then
  !     write(6,*) 'error in another private place'
  !     endif
  call cpu_time(now_cputime)
  time = int(1e6 * now_cputime)
  d1 = (time - cputime) / 1e6
  d2 = 0.0
  return
end subroutine runtym
!
!     subroutine settym.
!
! subroutine settym
!   implicit real*8 (a-h, o-z), integer*4 (i-n)
!   !     VAX-11  installation-dependent EMTP module.
!   !     Called only by VAX "runtym";  destroy for other computers.
!   !     Include  '[scott]commuk.for' --- share with "runtym" in-line:
!   common /timers/ cputime
!   integer*4 cputime
!   common /timer2/ l4cpu, cputime_code, cputime_adr, zero, zerofin
!   integer*2 l4cpu, cputime_code
!   integer*8 cputime_adr
!   !     integer*4 zero,zerofin,sys$getjpi,now_cputime
!   integer*4 zero, zerofin
!   data cputime_code /1031/
!   data l4cpu /4/
!   cputime_adr=%loc(cputime)
!   !     if (.not.sys$getjpi(,,,l4cpu,,,)) then
!   !     write(6,*) 'error in a private place'
!   !     endif
!   return
! end subroutine settym
subroutine settym
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  real*8 time
  common /timers/ cputime
  integer*4 cputime
  common /timer2/ l4cpu, cputime_code, cputime_adr, zero, zerofin
  call cpu_time(time)
  if (time .eq. -1.0) then
     write (6, *) 'Error, no timer unit available!'
  else
     cputime = int(1e6 * time)
  end if
  return
end subroutine settym
!
!     subroutine time44.
!
subroutine time44(a)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !    The purpose of subroutine  time44  is to interrogate the
  !    installation clock, and return the wall-clock time through the
  !    argument of the subroutine.   Eight bcd characters are allowed,
  !    with the first (left) four characters to be placed in  a(1) ,
  !    and the final (right) four placed in  a(2) .   A statement like
  !             write (lunit6, 4041)  a
  !        4041 format ( 1x, 2a4 )
  !    thus outputs the wall-clock time as first hours, then minutes,
  !    and finally seconds, separated by periods (hh.mm.ss) .
  !    Subroutine  time44  is of course installation dependent.
  !    Installation-dependent  EMTP  module written for the  DEC
  !    VAX-11/780.    'time'  is a  DEC  system subroutine which
  !    returns the wall-clock time as an 8-byte character string.
  !    This is just what the emtp needs, except that we want periods
  !    rather than colons, and of course we require  2a4  format.
  character(10) time
  character(8) a(2)
  call date_and_time(time = time)
  write (unit = a(1), fmt = 2741) time(1:1), time(2:2), time(3:3)
2741 format (2a1, '.', a1)
  write (unit = a(2), fmt = 2754) time(4:4), time(5:5), time(6:6)
2754 format (a1, '.', 2a1)
  return
end subroutine time44
!
!     subroutine cimage.
!
subroutine cimage
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     VAX-11  installation-dependent emtp module which serves
  !     to return the next input card.  All systems will substitute.
  include 'blkcom.ftn'
  include 'labcom.ftn'
  real*8 d11
  character*8 charc, chtacs, textax, textay, text1, text2
  character*8 text4, text5
  dimension  buff10(10)
  equivalence (buff10(1), abuff(1))
  character*25 filen
  dimension textax(60), jpntr(52), textay(50), aupper(10)
  equivalence (aupper(1), texcol(1))
  dimension xopt(1), copt(1)
  !     burroughs: preserve local variable between module calls:
  data n8         / 0 /        ! remember last $-card number
  data charc      / 1hc /
  data text4      / 1h9 /
  data chtacs     / 4htacs /
  data text5      / 6hblank  /
  !              *****    request no. 1.    "$attach"      *****  *****
  data textay(1)  / 6ha      /
  data jpntr(1)   / 1 /
  data textax(1)  / 6hattach /
  !              *****    request no. 2.    "$punch"       *****  *****
  data textay(2)  / 6hp      /
  data  jpntr(2)  / 2 /
  data textax(2)  / 6hpunch  /
  !              *****    request no. 3.    "$output"      *****  *****
  data textay(3)  / 6hout    /
  data jpntr(3)   / 3 /
  data textax(3)  / 6houtput /
  !              *****    request no. 4.    "$save"        *****  *****
  data textay(4)  / 6hs      /
  data jpntr(4)   / 4 /
  data textax(4)  / 6hsave   /
  !              *****    request no. 5.    "$spydata"     *****  *****
  !                  former "$include", which is now in "erexit".
  data textay(5)  / 6hspy    /
  data jpntr(5)   / 5 /
  data textax(5)  / 6hspydat /
  data textax(6)  / 6ha      /
  !              *****    request no. 6.    "$disable"     *****  *****
  data textay(6)  / 6hd      /
  data jpntr(6)   / 7 /
  data textax(7)  / 6hdisabl /
  data textax(8)  / 6he      /
  !              *****    request no. 7.    "$enable"      *****  *****
  data textay(7)  / 6he      /
  data jpntr(7)   / 9 /
  data textax(9)  / 6henable /
  !              *****    request no. 8.    "$return"      *****  *****
  data textay(8)  / 6hr      /
  data jpntr(8)   / 10 /
  data textax(10) / 6hreturn /
  !              *****    request no. 9.    "newfile"      *****  *****
  data textay(9)  / 6hn      /
  data jpntr(9)   / 11 /
  data textax(11) / 6hnewfil /
  data textax(12) / 6he      /
  !              *****    request no. 10.   "new epsiln"   *****  *****
  data textay(10) / 6hne      /
  data jpntr(10)  / 13 /
  data textax(13) / 6hnew     /
  data textax(14) / 6hepsiln  /
  !              *****    request no. 11.   "delete"       *****  *****
  data textay(11) / 6hde      /
  data jpntr(11)  / 15 /
  data textax(15) / 6hdelete  /
  !              *****    request no. 12.   "monitor"      *****  *****
  data textay(12) / 6hm       /
  data jpntr(12)  / 16 /
  data textax(16) / 6hmonito  /
  data textax(17) / 6hr       /
  !              *****    request no. 13.   "listoff"      *****  *****
  data textay(13) / 6hlf      /
  data jpntr(13)  / 18 /
  data textax(18) / 6hlistof  /
  data textax(19) / 6hf       /
  !              *****    request no. 14.   "liston"       *****  *****
  data textay(14) / 6hln      /
  data jpntr(14)  / 20 /
  data textax(20) / 6hliston  /
  !              *****    request no. 15.   "vintage"      *****  *****
  data textay(15) / 6hv       /
  data jpntr(15)  / 21 /
  data textax(21) / 6hvintag  /
  data textax(22) / 6he       /
  !              *****    request no. 16.   "oldfile"      *****  *****
  data textay(16) / 6hold     /
  data jpntr(16)  / 23 /
  data textax(23) / 6holdfil  /
  data textax(24) / 6he       /
  !              *****    request no. 17.   "stop"         *****  *****
  data textay(17) / 6hst      /
  data jpntr(17)  / 25 /
  data textax(25) / 6hstop    /
  !              *****    request no. 18.   "watch5"       *****  *****
  data textay(18) / 6hw       /
  data jpntr(18)  / 26 /
  data textax(26) / 6hwatch5  /
  !              *****    request no. 19.   "comment"      *****  *****
  data textay(19) / 6hcom     /
  data jpntr(19)  / 27 /
  data textax(27) / 6hcommen  /
  data textax(28) / 6ht       /
  !              *****    request no. 20.   "width"        *****  *****
  data textay(20) / 6hwi      /
  data jpntr(20)  / 29 /
  data textax(29) / 6hwidth   /
  !              *****    request no. 21.   "units"        *****  *****
  data textay(21) / 6hu       /
  data jpntr(21)  / 30 /
  data textax(30) / 6hunits   /
  !              *****   bounds follow ...
  data jpntr(22)  / 31 /
  data jpntr(23)  / 0 /
  data n11        / 0 /
  data n12        / 99999 /
  data n13        / 99999 /
  n6 = 0
  if (iprsup .ge. 10) write (lunit6, 987) lunit5, lunit6, noutpr, numdcd
987 format (' Begin cimage.  lunit5, lunit6, noutpr, numdcd =', 4i5)
1000 if (m4plot .eq. 1) call emtspy ! interactive usage
  if (lunit5 .gt. 0) read (lunit5, 3000, end=4000) buff10
3000 format (10a8)
  if (lunit5 .le. 0) call nextcard
  if (kill .gt. 0) go to 4000 ! "nextcard" eof jump
  if (lunsav(5) .ne. -5) numdcd = numdcd + 1
  read (unit = abuff, fmt = 3012) text1, text2
3012 format (2a1)
  if (text1 .ne. charc) go to 3034
  if (text2 .ne. blank) go to 3034
1036 if (noutpr .ne. 0) go to 1000
  if (n11 .ne. 0) go to 1000
  if (kol132 .eq. 132) write (lunit6, 3015) buff10
3015 format (' Comment card.', 37x, '1', 10a8)
  if (kol132 .ne. 132) write (lunit6, 3016) (abuff(j), j = 1, 4)
3016 format (' Comment card.', 37x, '1', 3a8, a5)
  go to 1000
3034 if (noutpr .ne. 0) go to 3035
  if (kol132 .eq. 132) write (lunit6, 3006) buff10
3006 format(51x, '1', 10a8)
  if (kol132 .ne. 132) write (lunit6, 3007) (abuff(j), j = 1, 4)
3007 format (51x, '1', 3a8, a5)
3035 if (n13 .gt. 0) go to 3011
  print 3009, numdcd, (abuff(i), i = 1, 9)
3009 format (1x, i5, ' :', 9a8)
  n13 = n12
3011 n13 = n13 - 1
  read (unit = abuff(1), fmt = 3037) text2
3037 format (a6)
  if (text2 .ne. text5) go to 3040
  if (n8 .eq. 6) go to 3044
  !     3039 do 3038 i = 1, 10
  do i = 1, 10
     abuff (i) = blank
  end do
  go to 3233
3040 if (chcont .eq. text4) go to 3233
  read (unit = abuff(1), fmt = 3041) texcol
3041 format (80a1)
  !     Dan Goldsworthy had trouble with $listoff within $include
  !     which was within tacs supplemental variables.  wsm+thl
  !      if ( abuff(1) .ne. 8h$listoff   .and.
  !     1     abuff(1) .ne. 8h$liston  )   go to 3042
  if (abuff(1) .ne. "$listoff" .and. abuff(1) .ne. "$liston") go to 3042
  go to 3246
  !     chcont is 'tacs' if cimage called from within tacs fortran express
3042 if (chcont .eq. chtacs) go to 3233
  if (texcol(1) .eq. chcont) go to 3246
  if (n8 .ne. 6) go to 1144
3044 if (noutpr .eq. 0) write (lunit6, 3045)
3045 format ('+Comment card (implicit).')
  go to 1000
1144 do k = 1, 80
     if (texcol(k) .eq. csepar ) go to 3237
     if (texcol(k) .eq. chcont ) go to 3237
  end do
3233 kolbeg = -intinf
  go to 7014
3237 kolbeg = 1
  go to 7014
3246 kolbeg = 2
  !     3251 nright = -2
  nright = -2
  call freone ( d1 )
  if (iprsup .ge. 1) write (lunit6, 3281) nfrfld, texta6(1), texta6(2)
3281 format (/,  ' nfrfld =', i8, 5x, 'texta6 =', 2a7)
  nright = 0
  do i = 1, 51        ! was 200 but maximum is 52?
     n1 = jpntr(i)
     n2 = jpntr(i + 1) - 1
     if (n2 .lt. 0) go to 3319
     if (iprsup .ge. 35) write (lunit6, 3285) i, (textax(j), j = n1, n2)
3285 format (' Special-request word', i4, ' .', 10a6)
     if (textax(n1) .eq. blank) go to 3306
     l = 0
     n3 = n2 - n1 + 1
     if (n3 .ne. nfrfld) go to 3306
     do j = n1, n2
        l = l + 1
        if (texta6(l) .ne. textax(j)) go to 3306
3291 end do
     if (iprsup .ge. 2) write (lunit6, 3292) i, n8, texta6(1), textay(i)
3292 format (/,  ' key-word found.  i, n8 =',  2i5, 5x, 'texta6, textay =', 2a7)
3294 if (n8 .ne. 6) go to 3301
     if (i .ne. 7) go to 1036
3301 n8 = i
     go to (4100, 4200, 4300, 4400, 4500, 4600, 4700, 4800, 4900, 5000, 5100, 5200, 5300, 5400, 5500, 5600, 5700, 5800, 5900, 6000,&
          6100), n8
3306 if (texta6(1) .eq. textay(i)) go to 3294
  end do
3319 write (lunit6, 3230)
3230 format (' Illegal $-card.   Stop at s.n. 3319 of "cimage" .')
  call stoptp   ! installation-dependent program stop card
  !               *****    request no. 1.    "$attach"      *****  *****
4100 text1 = 'attach'
  go to 4506
  !               *****    request no. 2.    "$punch"       *****  *****
4200 text1 = textax(2)
4206 n2 = lunit7
4209 nfrfld = 1
  call freone ( d11 )
  n1 = int(d11)
  !     4225 if ( n1 .le. 0 ) n1 = n2
  if (n1 .le. 0) n1 = n2
  if (n8 .eq. 8) go to 4817
  if (noutpr  .eq.  0) write (lunit6, 4231) n1, text1
4231 format ('+Copy file', i4, '   to ', a6,  ' .')
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind n1
  do k = 1, intinf
     read (n1, 3000, end = 4249) aupper
     !     because dec fortran does not honor the cdc  "punch"  statement
     !     (to directly punch cards), i write to unit 7 as next best
     !     thing.  wsm.  jan, 1980.
     if (n8 .eq. 2 ) write (lunit7, 3000) aupper
     if (n8 .eq. 3 ) write (lunit6, 4238) k, aupper
4238 format (20x, 'record', i5, ' .  1', 10a8)
  end do
  !     segmented, 1, vax e/t can skip translation of rewind:
4249 rewind n1
  go to 1000
  !               *****    request no. 3.    "$output"      *****  *****
4300 text1 = textax(3)
  go to 4206
  !               *****    request no. 4.    "$save"        *****  *****
4400 text1 = 'saved'
  go to 4506
4423 close (unit = n7, status = 'delete')
  open (unit = n7, status = 'new', form = 'formatted', file = filen)
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind n6
  do k = 1, intinf
     read (n6, 3000, end = 4436) aupper
     write (n7, 3000) aupper
  end do
  !     4436 close ( unit=n7,  dispose='save' )
4436 close (unit = n7, status = 'keep')
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind n6
  go to 1000
  !               *****    request no. 5.    "$spy"         *****  *****
4500 text1 = 'spying'
  muntsv(2) = 2288  ! remember $spy for eof time in "spying"
  muntsv(1) = munit5   ! remember spy input channel till now
  munit5 = 17   ! special i/o unit used for $spy connection
  kfile5 = 1  ! flag so "spying" knows munit5 opened to disk
4506 n4 = 0
  !     encode (25, 4523, filen(1))
  write (unit = filen(1:24), fmt = 4523)
4523 format (25x)
  do 4532 k = kolbeg, 80
     if (texcol(k) .eq. blank) go to 4532
     if (texcol(k) .eq. csepar) go to 4536
     !     if ( texcol(k)  .eq.   1h(  )   go to 4536
     if (texcol(k) .eq. "(") go to 4536
     n4 = n4 + 1
     !     encode (1, 3041, filen(n4))  texcol(k)
     write (unit = filen(n4:24), fmt = 3041) texcol(k)
4532 end do
  k = 80
4536 kolbeg = k + 1
  nfrfld = 1
  call freone(d11)
  n7 = int(d11)
  if ( n8  .ne.  4 )   go to 4557
  call freone(d11)
  n6 = int(d11)
4557 if (n6 .eq. 0) n6 = lunit7
  if (n8 .eq. 5) n7 = munit5   ! $spy uses this channel
  if (n7 .gt. 0) go to 4570
  do  k = 1, 15
     if (lunsav(k) .le. 0) go to 4568
  end do
  write (lunit6, 4565)
4565 format (/, 10(' error,'))
  write (lunit6, 4566) lunsav
4566 format (5x, ' All i/o channels occupied.  Kill run at  s.n. 4566  of  "cimage" .', /,  20i5)
  call stoptp   ! installation-dependent program stop card
4568 n7 = k
4570 if (noutpr .eq. 0) write (lunit6, 4572) text1, filen, n7
4572 format ('+', a6, ' file:', 25a1, ' unit =', i3)
  if (n8 .eq. 4) go to 4423
  if (n8 .eq. 9) go to 4907
  if (n8 .eq. 11) go to 5106
  if (n8 .eq. 16) go to 5608
  close (unit = n7)
  open (unit = n7, status = 'old', form = 'formatted', file = filen)
  rewind n7
  if (n8 .eq. 5) call spying   ! process $spy command file
  go to 1000
  !               *****    request no. 6.    "$disable"     *****  *****
4600 if (noutpr .eq. 0) write (lunit6, 4612)
4612 format ('+Begin data to be ignored.')
  go to 1000
  !               *****    request no. 7.    "$enable"      *****  *****
4700 if (noutpr .eq. 0) write (lunit6, 4714)
4714 format ('+End of data to be ignored.')
  go to 1000
  !               *****    request no. 8.    "$return"      *****  *****
4800 n2 = lunit4
  go to 4209
4817 close (unit = n1)
  if (noutpr .eq. 0) write (lunit6, 4823) n1
4823 format ('+Close file on unit', i3, ' .')
  if (n1 .ne. lunit5) go to 1000
  noutpr = 1
  go to 1000
  !               *****    request no. 9.    "$newfile"     *****  *****
4900 text1 = 'newfil'
  n2 = lunit4
  go to 4506
  !     4907 open (unit=n7, type='new', form='unformatted', name=filen )
4907 open (unit = n7, status = 'new', form = 'unformatted', file = filen)
  go to 1000
  !               *****    request no. 10.   "new epsiln"   *****  *****
5000 nfrfld = 1
  d1 = epsiln
  call freone (epsiln)
  if (noutpr .eq. 0) write (lunit6, 5017)  d1, epsiln
5017 format ('+ epsiln change.  old, new =', 2e11.2)
  go to 1000
  !               *****    request no. 11.   "delete"       *****  *****
5100 text1 = 'delete'
  go to 4506
  !     5106 open ( unit=n7, type='old', name=filen )
5106 open (unit = n7, status = 'old', file = filen)
  !     close( unit=n7, dispose='delete' )
  close (unit=n7, status='delete')
  go to 1000
  !               *****    request no. 12.   "monitor"      *****  *****
5200 if ( noutpr  .ne.  0 )   go to 5219
  print 3006,  buff10
  print 5214, numdcd
5214 format ('+Crt monitor.  card number =', i5)
5219 go to 1000
  !               *****    request no. 13.   "listoff"      *****  *****
5300 if ( noutpr  .ne.  0 )   go to 5324
  write (lunit6, 5307)  numdcd
5307 format ('+Turn off input listing at card', i5)
  noutpr = 1
5324 go to 1000
  !               *****    request no. 14.   "liston"       *****  *****
5400 write (lunit6, 5404)
5404 format (51x, '1$liston')
  noutpr = 0
  write (lunit6, 5412)  numdcd
5412 format ('+Turn on input listing at card', i5)
  go to 1000
  !               *****    request no. 15.   "vintage"      *****  *****
5500 nfrfld = 1
  call freone(d11)
  moldat = int(d11)
  if ( noutpr  .eq.  0 ) write (lunit6, 5518)  moldat
5518 format ('+New moldat =', i4, 5x, '(data vintage)')
  go to 1000
  !               *****    request no. 16.   "oldfile"      *****  *****
5600 text1 = 'oldfil'
  n2 = lunit2
  go to 4506
5608 close (unit = n7)
  !     open (unit=n7, type='old', form='unformatted', name=filen )
  open (unit = n7, status = 'old', form = 'unformatted', file = filen)
  go to 1000
  !               *****    request no. 17.   "stop"         *****  *****
5700 write (lunit6, 5706)
5706 format ('+Stop execution immediately, in "cimage".')
  call stoptp   ! installation-dependent program stop card
  !               *****    request no. 18.   "watch5"       *****  *****
5800 nfrfld = 1
  call freone(d11)
  n12 = int(d11)
  n13 = n12
  if (noutpr .eq. 0) write (lunit6, 5812) n12
5812 format ('+Paint input data on screen.', i8)
  go to 1000
  !               *****    request no. 19.   "comment"      *****  *****
5900 n11 = n11 + 1
  if (n11 .ge. 2) n11 = 0
  if (noutpr .eq. 0) write (lunit6, 5917) n11
5917 format ('+Toggle comment card destruction flag.', i8)
  go to 1000
6000 call stoptp   ! installation-dependent program stop card
  !               *****    request no. 21.   "units"        *****  *****
6100 nfrfld = 1
  call frefld(xopt)
  call frefld(copt)
  if (noutpr .eq. 0) write (lunit6, 6114) xopt, copt
6114 format ('+New  xopt, copt =', 2e14.4)
  xunits = 1000.
  if(xopt(1) .gt. 0.0) xunits = twopi * xopt(1)
  go to 1000
  !     additional key-word code goes below.
4000 write (lunit6, 4006)
4006 format (/, 1x, 85('='), /, ' End of file encounted in "cimage" while attempting to read another data card.   Stop.', /, 1x, 85('='))
  call stoptp   ! installation-dependent program stop card
  !     unique exit of module, possibly after echoing card image:
7014 if (inecho .eq. 0) return
  entry cecho
  !     "statistics" over12 echos lunit5 card images of base case
  !     (read in over13, over15, maybe subts3, subr31) so each
  !     energization can re-cimage this lunt12 data. 2nd and later
  !     one skips overlay 12-15 cards with lunit5 read in over12
  if (nchain .le. 15) ipntv(11) = ipntv(11) + 1
  write (inecho, 3000) buff10
  return
  entry ibrinc
  ibr = ibr + 1
  xoptbr(ibr) = xopt(1)
  coptbr(ibr) = copt(1)
  return
end subroutine cimage
!
!     subroutine ioerr.
!
subroutine ioerr(naddr)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
 naddr = 0
 return
end subroutine ioerr
!
!     subroutine caterr.
!
subroutine caterr(naddr, koderr)
 implicit real*8 (a-h, o-z), integer*4 (i-n)
 naddr = 0
 koderr = 0
 return
end subroutine caterr
!
!     function locf.
!
function locf(array)
 implicit real*8 (a-h, o-z), integer*4 (i-n)
 integer*8 locf
 real*8 array
 dimension array(1)
 locf = %loc(array(1)) / 8
 return
end function locf
!
!     function locint.
!
function  locint(iarray)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  integer*8 locint
  integer*4 iarray
  dimension iarray(1)
  !     installation-dependent EMTP module.   This is  VAX  version.
  !     function  'LOCINT'  is designed to return the address in memory
  !     of the argument, as an  INTEGER*4  word address.   An arbitrary
  !     constant offset is allowed, since only differences will ever be
  !     used by the EMTP.   Note vector argument  "iarray"  (which
  !     is an assumption for all EMTP usage).
  locint = (%loc(iarray(1))) / 4
  return
end function locint
!
!     function rfunl1.
!
function rfunl1(x)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     this function provides for all real library functions of
  !     a single real argument.   All translations will make a
  !     substitution.
  include 'blkcom.ftn'
  rfunl1 = x
  return
  entry absz   (x)
  absz = dabs(x)
  return
  entry acosz  (x)
  acosz = dacos ( x )
  return
  entry aintz  (x)
  aintz = dint(x)
  return
  entry alogz  (x)
  alogz = dlog(x)
  return
  entry alog1z (x)
  alog1z = dlog10(x)
  return
  entry asinz  (x)
  asinz = dasin(x)
  return
  entry atanz  (x)
  atanz = datan(x)
  return
  entry cosz   (x)
  cosz = dcos(x)
  return
  entry coshz  (x)
  coshz = dcosh(x)
  return
  entry cotanz (x)
  y = dsin(x)
  if ( dabs(y)*fltinf  .gt.  1.0 )   go to 4783
  write (lunit6, 4761)  x
4761 format (/, " Stop.   Too small argument at  'cotanz'  within 'rfunl1' .", e15.5)
  call stoptp   ! installation-dependent program stop card
4783 cotanz = dcos(x) / y
  return
  entry expz   (x)
  if ( x .ge. -87 ) go to 1488
  expz = 0.0
  return
1488 expz = dexp(x)
  return
  entry sinz   (x)
  sinz = dsin(x)
  return
  entry sinhz  (x)
  sinhz = dsinh(x)
  return
  entry sqrtz  (x)
  sqrtz = dsqrt(x)
  return
  entry tanz   (x)
  tanz = dtan(x)
  return
  entry tanhz  (x)
  tanhz = dtanh(x)
  return
end function rfunl1
!
! subroutine trgwnd.
!
subroutine trgwnd ( x, d17 )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  d17 = x
  if (dabs(x) .lt. 25000.) go to 9000
  n13 = int(x / twopi)
  d17 = d17 - n13 * twopi
  if ( iprsup .ge. 1 ) write (*, 3456)  nchain, x, d17
3456 format (' Angle unwind in "trgwnd" called by "rfunl1".   nchain, x, d17 =', i5, 2e25.16)
9000 return
end subroutine trgwnd
!
!     function ifunl1.
!
function  ifunl1(d1)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     This function is to provide neutral names ending in "z"
  !     for all integer library functions of one real argument.
  ifunl1 = int(d1)
  return
  entry     intz ( d1 )
  intz = int(dint(d1))
  return
end function ifunl1
!
! function cfunl1.
function cfunl1(x)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  complex*16    cfunl1, x, cexpz, csqrtz, clogz
  cfunl1 = x
  return
  entry cexpz  (x)
  cexpz = cdexp(x)
  return
  entry csqrtz (x)
  csqrtz = cdsqrt(x)
  return
  entry clogz (x)
  clogz = cdlog(x)
  return
end function cfunl1
!
! function rfunl2.
!
function rfunl2 ( x, y )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     This function provides for all real library functions of
  !     two real arguments.  All translations will make a
  !     substitution.
  rfunl2 = x
  return
  entry atan2z (x,y)
  atan2z = 0.0
  if (x .ne. 0.0  .or. y .ne. 0.0) atan2z = datan2(x,y)
  return
  entry signz  (x,y)
  signz = dsign(x,y)
  return
  entry amodz(x,y)
  amodz = dmod (x,y)
  return
  entry amin1z(x,y)
  amin1z = dmin1(x,y)
  return
  entry amax1z(x,y)
  amax1z = dmax1(x,y)
  return
end function rfunl2
!
! function cmpxz.
!
function cmplxz ( x, y )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     This function provides for all complex library functions of
  !     two real arguments.  All translations will make a
  !     substitution.
  !       VAX module switched to complex*16 (from *8) in aug 1981
  complex*16    cmplxz
  cmplxz = dcmplx ( x, y )
  return
end function cmplxz
!
! function rfunl3.
!
function rfunl3 ( x )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     This function provides for all real library functions of
  !     a single complex argument.   All translations will make a
  !     substitution.
  !       this VAX module became complex*16 (from *8) in aug 1981
  complex*16     x
  rfunl3 = 0.0
  return
  entry aimagz (x)
  aimagz = dimag ( x )
  return
  entry realz  (x)
  realz = dreal ( x )
  return
  entry cabsz(x)
  cabsz = cdabs ( x )
  return
end function rfunl3
!
! subroutine cmultz.
!
subroutine cmultz (ar,ai,br,bi,cr,ci,ksn)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  sr=br*cr-bi*ci
  ai=bi*cr+br*ci
  ar=sr
  if (ksn.ge.0) go to 200
  ar=-ar
  ai=-ai
200 return
end subroutine cmultz
!
! subroutine cdivz.
!
subroutine cdivz (ar,ai,br,bi,cr,ci,ksn)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  fac=cr*cr+ci*ci
  fac=1./fac
  sr=br*fac*cr+bi*fac*ci
  ai=bi*fac*cr-br*fac*ci
  ar=sr
  if (ksn.ge.0) go to 200
  ar=-ar
  ai=-ai
200 return
end subroutine cdivz
!
! function iabsz.
!
function  iabsz ( n1 )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     One and only integer library function of one integer
  !     argument.    Make entry point if 2nd is used.
  iabsz = iabs ( n1 )
  return
end function iabsz
!
! function ifunl2.
!
function ifunl2 ( n1, n2 )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Provision for all integer library functions of 2 integer arguments
  ifunl2 = n1
  return
  entry isignz (n1, n2)
  isignz = isign (n1, n2)
  return
  entry modz (n1, n2)
  modz = mod (n1, n2)
  return
  entry min0z (n1, n2)
  min0z = min0 (n1, n2)
  return
  entry max0z (n1, n2)
  max0z = max0 (n1, n2)
  return
end function ifunl2
!
! subroutine dlibrf.
!
subroutine dlibrf(x, y)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  double precision x, y
  !     This module serves to provide selected double-precision
  !     library functions for several places in the program.
  !     Making this a subroutine rather than a function avoids all
  !     complications with the module name having a variable type
  !     associated with it.   It is installation-dependent because of two
  !     things --- first the use of entry points, and second the use
  !     of the double-precision declaration (by which is meant double
  !     the precision of regular floating-point variables of the EMTP).
  !     Since most byte-organized machines use  real*8  for other
  !     variables, this implies  real*16 ,  if available.
  !     Installation-dependent module coded for  dec vax-11
  return
  entry dabsz (x, y)
  y = dabs(x)
  return
  entry dcosz (x, y)
  y = dcos(x)
  return
  entry dexpz (x, y)
  y = dexp(x)
  return
  entry dsinz (x, y)
  y = dsin(x)
  return
  entry dsqrtz (x, y)
  y = dsqrt(x)
  return
  entry dlogz (x, y)
  y = dlog(x)
  return
end subroutine dlibrf
!
! subroutine dlibr2.
!
subroutine dlibr2(x, y, z)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  double precision  x, y, z
  !     Installation-dependent module for dec vax-11 computer
  !     like "dlibrf" (see comments there), only for two inputs
  return
  entry datn2z (x,y,z)
  z = datan2 ( x, y )
  return
end subroutine dlibr2
!
!     subroutine setmar.
!
subroutine setmar
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Subroutine to change page size and eject page for printer plots.
  !     Dummy module since this ontario hydro (univac) trick is unknown.
  return
  entry chrsiz(n)
  n = 0
  !     Entry point for do-nothing mimiced tektronix plot10 of "tekplt"
  return
  entry  setplt
  !     Entry point to change lines/page to maximum number allowed,
  !     to allow printer plots to be continuous over page boundries.
  !     call system dependant routine to change page size
  !     write (lunit6, 1000 )
  !     1000 format (1h1)
  return
  entry  setstd
  !     Entry point to restore page limits to standard values.
  !     call system dependant routine to change page size
  !     write (lunit6, 1000 )
  return
end subroutine setmar
!
!     subroutine interp.
!
subroutine interp
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     If  character*51 kunit6,  active module needed to flush
  !     abuff and kunit6 as 132-column interpreted data card line.
  return
end subroutine interp
!
!     subroutine mover.
!
subroutine mover(a, b, n)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !    Subroutine  mover  (with entry point mover0) is used for block
  !    transfers between contiguous cells of core storage.   'n'  is the
  !    number of words to be transfered.
  !         1.  using 'mover', the transfer is from  a(j)  to  b(j),
  !             for  j=1, n.
  !         2.  using  'mover0',  a(1)  is copied into all  n  cells
  !             of array  'b'.   for zeroing array  'b' ,  the subroutine
  !             call is made with the first argument explicitely
  !             punched as zero.
  dimension  a(1), b(1)
  do i = 1, n
     b(i) = a(i)
  end do
  return
end subroutine mover
!
!     subroutine mover0.
!
subroutine mover0 ( b, n )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  real(8) b(*)
  !    Subroutine  mover0  is a block-zeroing routine for floating-point
  !    arrays or variables.   Of the two arguments, the second,  'n' ,
  !    is the number of words of core to be zeroed.   The first argument
  !    specifies the address of the first word of the block of  'n'
  !    words of core which is to be zeroed.
  do i = 1, n
     b(i) = 0.0
  end do
  return
end subroutine mover0
!
!     subroutine move.
!
subroutine move(inta, intb, n)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !    Subroutine  move  is identical to the
  !    block-transfer routine  mover  except
  !    that  move  is for integer arrays, while  mover  was for
  !    floating-point arrays.   There is a difference, of course, on
  !    machines like ibm, where integer words may be shorter than
  !    floating-point words.
  dimension inta(1), intb(1)
  do i = 1, n
     intb(i) = inta(i)
  end do
  return
end subroutine move
!
!     subroutine move0.
!
subroutine move0(intb, n)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !    Subroutine  move0  is identical to  the block-zeroing routine
  !    mover0  except that  move0  is for integer arrays, while  mover0
  !    is for floating-point arrays.   There is a difference, on
  !    machines like IBM, where integer words may be shorter than
  !    floating-point words.
  integer(4) intb(*)
  do i = 1, n
     intb(i) = 0
  end do
  return
end subroutine move0
!
!     subroutine addmxd.
!
subroutine addmxd(a, b, c, n)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Subroutine  addmxd  forms matrix   (c) = (a) + b(u)  , where (a),
  !     and (c)  are n by n matrices,  b  is a scalar, and (u) is the
  !     identity matrix.   Array (c) may be the same as (a), if desired.
  !     See subr.  mult  for symmetric-matric storage scheme assumed.
  dimension  a(1), c(1)
  k = 1
  j = 1
  jt = n * (n+1) / 2
  do l = 1, jt
     c(l) = a(l)
     if( l .lt. k )  go to 3010
     c(l) = c(l) + b
     j = j + 1
     k = k + j
3010 end do
  return
end subroutine addmxd
!
!     subroutine multmx.
!
subroutine multmx(a, b, c, temp, n)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     Subroutine multmx  forms the matrix product   (c) = (a)(b)   where
  !     matrices  (a), (b), and (c)  are all  n by n  square arrays.
  !     Array  'temp'  is a scratch working area of not less than  2n
  !     cells.   Arrays  (b)  and  (c)  may be identical, thereby placing
  !     the product   (a)(b)   back into  (b) .    See subroutine  'mult'
  !     which is called herein, for details about the storage scheme used
  !     )    for these real, symmetric matrices.
  dimension a(1), b(1), c(1), temp(1)
  l = 0
  ll0 = 0
  ii = 0
  do j = 1, n
     do i = 1, n
        if( i .le. j )  go to 3420
        l = l + (i - 1)
        go to 3430
3420    l = ii + i
3430    temp(i) = b(l)
     end do
     m = n + 1
     call mult(a(1), temp(1), temp(m), n, ll0)
     call mover(temp(m), c(ii+1), j)
     ii = ii + j
  end do
  return
end subroutine multmx
!
!     subroutine frefld.
!
subroutine frefld(array)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'blkcom.ftn'
  character*8 text1, chtacs, texbuf, texvec
  dimension texbuf(30), array(1), texvec(1)
  equivalence (texvec(1), text1)
  data  chtacs  /  6htacs     /
  if (iprsup .ge. 5) write (lunit6, 1016) nfrfld, nright, kolbeg
1016 format (' Top "frefld".  nfrfld, nright, kolbeg =', 3i6)
  if (nright .lt. 0 ) go to 5913
  do 5851 jj = 1, nfrfld
     if (kolbeg .le. 80) go to 5600
     lstat(19) = 5600
     go to 9200
5600 n3 = 0
     go to 5805
5603 if (chcont .eq. chtacs ) go to 5614
     if (text1 .eq. blank ) go to 5802
     if (text1 .ne. csepar ) go to 5623
5609 kolbeg = kolbeg + 1
     go to 5827
5614 if (text1 .ne. csepar ) go to 5623
     if (text1 .eq. blank ) go to 5802
     go to 5609
5623 if (n3 .lt. 30 ) go to 5627
     lstat(19) = 5623
     go to 9200
5627 n3 = n3 + 1
     texbuf(n3) = text1
5802 kolbeg = kolbeg + 1
5805 text1 = texcol(kolbeg)
     if (text1 .ne. chcont ) go to 5819
     !     read input card using cimage
     call cimage
     kolbeg = 1
     if (n3 .eq. 0 ) go to 5805
     go to 5827
5819 if (kolbeg .le. 80 ) go to 5603
5827 if (n3 .gt. 0) go to 5829
     array(jj) = 0.0
     go to 5831
5829 call frenum (texbuf(1), n3, array(jj))
5831 if (iprsup .ge. 5) write (lunit6, 5837)  jj, kolbeg, n3, array(jj)
5837 format (/, ' "frefld" number.      jj  kolbeg      n3',  21x,  'array(jj)  ', /, 17x, 3i8, e30.20)
5851 end do
  go to 9900
5913 if (nright .lt. -1) go to 6036
  do 5948  jj=1, nfrfld
     texta6(jj) = blank
     ll = 0
     if (kolbeg .le. 80) go to 5920
     lstat(19) = 5920
     go to 9200
5920 text1 = texcol(kolbeg)
     kolbeg = kolbeg + 1
     if (chcont .eq. chtacs) go to 5928
     if (text1 .eq. blank ) go to 5923
     if (text1 .eq. csepar ) go to 5948
5921 if (ll .le. 6 ) go to 5922
     lstat(19) = 5922
     go to 9200
5928 if (text1 .eq. csepar ) go to 5948
     if (text1 .eq. blank ) go to 5923
     go to 5921
5922 ll = ll + 1
     call packa1(texvec(1), texta6(jj), ll)
5923 if (kolbeg .le. 80 ) go to 5920
5948 end do
  go to 9900
6036 ll = 0
  jj = 0
  go to 6054
6042 jj = jj + 1
  if ( jj  .gt.  10 )   go to 6072
  texta6(jj) = blank
  ll = 0
6048 text1 = texcol(kolbeg)
  if (chcont .eq. chtacs) go to 6051
  if (text1 .eq. blank) go to 6054
  if (text1 .eq. csepar) go to 6072
  go to 6052
6051 if (text1 .eq. csepar) go to 6072
  if (text1 .eq. blank) go to 6054
6052 if (ll .eq. 6) go to 6042
  ll = ll + 1
  call packa1 ( texvec(1), texta6(jj), ll )
  kolbeg = kolbeg + 1
  go to 6048
6054 n9 = kolbeg
  do 6059  i = kolbeg, 80
     if (texcol(i) .ne. blank) go to 6067
6059 end do
  kolbeg = 79
  go to 6072
6067 kolbeg = i
  if (kolbeg - n9 .le. 2 ) go to 6069
  if (jj .gt. 0 ) go to 6072
6069 if (texcol(kolbeg) .ne. csepar ) go to 6042
6072 nfrfld = jj
  kolbeg = kolbeg + 1
  if (iprsup .ge. 1) write (lunit6, 6083) jj, ll, kolbeg, texcol
6083 format (/, ' Keyword near "frefld" exit.      jj      ll  kolbeg   ', /, 28x, 3i8, /, (' texcol =', 30a4))
  go to 9900
9200 kill = 166
  if (iprsup .ge. 0) write (lunit6, 9207)  lstat(19), nchain, lastov, kolbeg, nfrfld, nright
9207 format (/, " Error stop within  'frefld' .", 6i8, /, 1x)
  lstat(18) = -1
9900 if (iprsup .ge. 2) write (lunit6, 9901)  kill, kolbeg, array(1)
9901 format (' Exit "frefld".  Kill, kolbeg, array(1) =', i6, e20.10)
  return
end subroutine frefld
!
!     subroutine freone
!
subroutine freone(d1)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     scalar version of  "frefld"  which enters the utpf with
  !     "m29."  vintage, to satisfy burroughs (see problem b,
  !     section ii, page ecwb-4, vol. x  emtp memo of 14 feb 1981.)
  dimension array(10)
  call frefld(array)
  d1 = array(1)
  return
end subroutine freone
!
! subroutine frenum.
!
subroutine frenum(text1, n3, d1)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     VAX-11/780  installation-dependent module called only by
  !     the free-format data module  "frefld" .  Purpose is to
  !     convert input characters  (text1(1) ... text1(n3))  into
  !     a floating point number.
  !     real*8        text1(1), blank
  character*8 text1(1), blank
  !     logical*1  texta(30), textb
  character texta(30), textb
  data  blank   /  6h          /
  data  textb   /  1h          /
  n9 = 30
  n4 = n3 + 1
  do 4718  i=1, n3
     n4 = n4 - 1
     if ( text1(n4)  .eq.  blank )   go to 4718
     if ( n9  .ge.  2 )   go to 4711
     write (6, 4706)
4706 format (/, ' Error stop in "frenum".   There are 33 or more characters in a free-format number on last data card.')
     call stoptp   ! installation-dependent program stop card
     !     4711 encode (1, 4712, texta(n9))  text1(n4)
4711 write (unit=texta(n9), fmt=4712) text1(n4)
4712 format (80a1)
     n9 = n9 - 1
4718 end do
  do i = 1, n9
     texta(i) = textb
  end do
  read (unit=texta(1), fmt=4732) d1
4732 format (e30.0)
  return
end subroutine frenum
!
!     subroutine packa1.
!
subroutine packa1(from, to, kk)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     System-dependent emtp module  'packa1'  for  VAX-11/780.
  !     Argument  'from'  contains  a1  information which is to be stored
  !     in character position  kk  of argument  'to' .
  !     For all emtp usage,  1st 2 arguments must be vectors.
  !     logical*1 from(1), to(6)
  character from(1), to(6)
  to(kk) = from(1)
  return
end subroutine packa1
!
!     subroutine packch.
!
subroutine packch(from, to, k4or6, nchbeg, nword)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !
  !     This module performs the system-dependent function of packing bcd
  !     characters from  a4  or  a6  words together so as to form a
  !     contiguous string of characters without extra blank fill.   For
  !     example, cdc has a 60-bit word which stores 10 bcd characters (6
  !     bits to a character).   With plot alphanumeric text read under  a6
  !     format control, the right-most four characters of such words will
  !     be blank-filled by the system.   IBM's eight characters in an
  !     eight-byte word pose an analogous problem.   Since the  calcomp
  !     plotting subroutine calls which appear in
  !     the calling module require contiguous character strings, output
  !     text must be compressed so as to remove the word-length-dependent
  !     blank fill.   The present subroutine is called by the plotting
  !     code to perform this function.   Meaning of the subroutine
  !     arguments is as follows ......
  !      nword ----- the number of words which are to have their bcd
  !                  characters extracted, and packed into  a character
  !                  string.   this is a positive integer.
  !      from(1)  ----- the first of  'nword'  words  whose bcd contents
  !                     are to be packed.
  !      k4or6  ----- equal to either  4  or  6 ,  whichever is the number
  !                   of characters of bcd information which is stored in
  !                   the words   (from(i), i=1, nword) .
  !      to(1)  ----- the active bcd characters of   (from(i), i=1, nword)
  !                   are to be packed as a contiguous string of
  !                   characters beginning with character position
  !                   'nchbeg'  of word  to(1) .
  !      nchbeg  -----  the character position of word  to(1)  where the
  !                     contiguous string of bcd characters is to begin.
  !                     normally  'nchbeg'  is between  1  and  10 ,
  !                     although larger positive values are allowed.
  !                     for example, a value of  27  means that the cdc
  !                     character insertion begins in position  7  of
  !                     word  to(3) .
  logical*1 from(nword), to(nword)
  i_char = nchbeg
  do i_word = 1, nword
     do k_char = 1, k4or6
        jchar = (i_word - 1) * 8 + k_char
        to(i_char) = from(j_char)
        i_char = i_char + 1
     end do
  end do
  return
end subroutine packch
!
!     function seedy.
!
function seedy(atim)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !
  !     This function is designed to take the time of day (wall-clock
  !     time) in bcd form as input, and return the number of seconds
  !     since midnight as output.   the time of day is assumed to be
  !     coded in words   atim(1)   and   atim(2) ,   as found by a call to
  !     subroutine  'time44' .   The storage here is in format   2a4 ,
  !     with the first four characters of   'hh.mm.ss'   assumed to be
  !     in  atim(1) ,  and the last four in  atim(2) .
  !
  !     real*8          atim
  character*8 atim
  dimension atim(2)
  read (unit = atim, fmt = 4286) ihr, imin10, imin1, isec
4286 format (i2, 1x, i1, 4x, i1, 1x, i2)
  imin = imin10 * 10  +  imin1
  hour = ihr * 3600
  amin = imin * 60
  sec = isec
  seedy = sec + amin + hour + 1.0
  return
end function seedy
!
!     subroutine randnm.
!
function randnm(x)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !
  !     This is a random number generating function, whose purpose is to
  !     return with a value for a random variable which is uniformly-
  !     distributed over the interval from zero to unity.
  !     A typical call is    y = randnm(x)  ,    where  'x'  is a
  !     floating-point variable which is present only because 'random'
  !     number-generating algorithms are never truly random, and generally
  !     require some sort of 'random' initialization to even get started.
  !     Since such number-generating algorithms are actually cyclic if a
  !     large enough sampling is taken, it is also desirable to
  !     re-initialize the algorithm periodically, by means of a 'random'
  !     external input.   Variable  'x'  is this 'random' input, whose
  !     purpose is to randomly re-initialize the algorithm, as follows ...
  !          1.  if  'x'  is identically zero, there is no initialization.
  !              the next random number is simply returned through the
  !              function name.
  !          2.  if  'x'  is positive, the random number generating
  !              algorithm is to be initialized.   emtp usage has  'x'
  !              as the wall-clock time in seconds since midnight, an
  !              integer (though stored in floating-point mode, note).
  !              in this case, no random number need be returned with the
  !              function name, since the emtp will not use it.
  !     if a non-cdc user has access to a random number generating
  !     function which does not require initialization, he may simply
  !     ignore  'x' ,   and return the random number through the function
  !     name every time this module is called.
  !
  !     A minus sign appended to variable  'xmaxmx'  of   /blank/  is a
  !     flag that the user wants to employ the standard table of random
  !     numbers which is built into module  'sandnm' .
  !
  !     Installation-dependent  EMTP  module written for the  dec
  !     VAX-11/780.    'ran'  is a  dec  system subroutine which
  !     returns a random number uniformly distributed over  (0, 1) .
  include  'blkcom.ftn'
  equivalence (moncar(1), knt)
  if (xmaxmx  .lt.  0.0) go to 7265
  if (x .eq.  0.0) go to 4213
  if (knt .gt. 1) go to 9800 ! skip 2nd or later seed
  n14 = int(x)
  if (n14 / 2 * 2 .eq. n14) n14 = n14 + 1
4213 randnm = ran(n14)         ! 29 dec 1987, change from  (n1, n2)
  go to 9800
7265 randnm = sandnm(x)
9800 return
end function randnm
!
!     function sandnm.
!
function sandnm(x)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     This version of  'randnm'  is used for testing of the
  !     statistical overvoltage capability of the emtp only.   It uses
  !     built-in random numbers, so as to produce identical results
  !     on all computer systems, thereby permitting comparative
  !     verification of results exactly as for conventional test cases.
  !     In order to avoid re-use of random numbers, the number of
  !     energizations  'nenerg'  times the number of switches  'kswtch'
  !     must not exceed the dimension of array  'a' .
  !     If  'statistics'  miscellaneous data parameter  'xmaxmx'
  !     is input by the user with negative value, the emtp takes
  !     this to mean that "sandnm" is to be used for random
  !     numbers rather than "randnm" .
  include  'blkcom.ftn'
  dimension  a(100)
  equivalence  ( moncar(1), knt )
  !     burroughs: preserve local variable between module calls:
  data   l  / 0 /
  !     beginning of assignment of random numbers to array  'a' .
  a(  1) =  .1445312506618
  a(  2) =  .8477795260409
  a(  3) =  .8267723125831
  a(  4) =  .6660710406131
  a(  5) =  .7152322826372
  a(  6) =  .3239128029543
  a(  7) =  .5051959208554
  a(  8) =  .3805491872180
  a(  9) =  .5609474043286
  a( 10) =  .5996361115942
  a( 11) =  .4159594349328
  a( 12) =  .6756609755246
  a( 13) =  .0995378032610
  a( 14) =  .6033780421273
  a( 15) =  .4515533431030
  a( 16) =  .0020932062778
  a( 17) =  .9161858062074
  a( 18) =  .3434229008090
  a( 19) =  .7876940045781
  a( 20) =  .2760908032985
  a( 21) =  .3665660303205
  a( 22) =  .8204300122029
  a( 23) =  .2413831265551
  a( 24) =  .1246653115746
  a( 25) =  .2802441882439
  a( 26) =  .0466535013838
  a( 27) =  .4742772736449
  a( 28) =  .9477027545777
  a( 29) =  .2260144770748
  a( 30) =  .2987460629005
  a( 31) =  .5203589181526
  a( 32) =  .8981967037721
  a( 33) =  .3873885562436
  a( 34) =  .5780913804991
  a( 35) =  .1280852320759
  a( 36) =  .3327754064471
  a( 37) =  .4043867414456
  a( 38) =  .9490362532099
  a( 39) =  .6261391859471
  a( 40) =  .3291406705415
  a( 41) =  .3366864607083
  a( 42) =  .9438413593777
  a( 43) =  .9369008057740
  a( 44) =  .0713971670442
  a( 45) =  .6500854844946
  a( 46) =  .9737952005663
  a( 47) =  .6485758973471
  a( 48) =  .7724301318424
  a( 49) =  .9676692044394
  a( 50) =  .5163953619955
  a( 51) =  .5788464270596
  a( 52) =  .7758933795560
  a( 53) =  .0910635448877
  a( 54) =  .0439510552688
  a( 55) =  .0707223001462
  a( 56) =  .9379043319315
  a( 57) =  .0052391978463
  a( 58) =  .9420572226295
  a( 59) =  .5932597508799
  a( 60) =  .6466146627873
  a( 61) =  .4395400252824
  a( 62) =  .1972895298303
  a( 63) =  .5017482047726
  a( 64) =  .1503404202877
  a( 65) =  .9624699228977
  a( 66) =  .0098276069324
  a( 67) =  .6571365402082
  a( 68) =  .4233003554891
  a( 69) =  .1203194365765
  a( 70) =  .7436871629477
  a( 71) =  .8136524161969
  a( 72) =  .7311319136405
  a( 73) =  .0594772166524
  a( 74) =  .2374863512189
  a( 75) =  .2450459940689
  a( 76) =  .4326371816340
  a( 77) =  .3562832359564
  a( 78) =  .3723442477773
  a( 79) =  .1694432139356
  a( 80) =  .3735622812899
  a( 81) =  .0610718353086
  a( 82) =  .2782657746530
  a( 83) =  .5137050416289
  a( 84) =  .4340395038268
  a( 85) =  .5766543446808
  a( 86) =  .4413641042052
  a( 87) =  .9812390285872
  a( 88) =  .2625281459037
  a( 89) =  .9554345097074
  a( 90) =  .4741647690234
  a( 91) =  .9906793757886
  a( 92) =  .7820837820369
  a( 93) =  .2206664815389
  a( 94) =  .0901816247992
  a( 95) =  .7625227133400
  a( 96) =  .4434728419824
  a( 97) =  .7905859532294
  a( 98) =  .9796097207935
  a( 99) =  .7599602497147
  a(100) =  .1154361048406
  !     end       of assignment of random numbers to array  'a' .
  if ( x  .eq.  0.0 )   go to 2614
  l = (knt - 1 ) * kswtch + 1
  go to 2632
2614 l = l + 1
2632 n1 = (l-1) / 100
  if ( iprsup  .ge.  1 ) write (lunit6, 2645)  l, knt, kswtch, n1, x
2645 format (/, " Variables in  'sandnm' ,   the random-number generator with 100 built-in numbers.       &
          l     knt  kswtch      n1 ", 14x, 'x', /, 82x, 4i8, e15.5)
  if ( n1  .gt.  0 ) l = l  -  100 * n1
  sandnm = a(l)
  return
end function sandnm
!
! subroutine mult.
!
subroutine mult(a, x, y, n, icheck)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !     subroutine  'mult'  is used to post-multiply a symmetric matrix
  !     by a vector.
  !     a=matrix,x and y=vectors.if icheck=0   then  y=a*x
  !     n=size of matrix.        if icheck=neg.then  y=a*x-y
  !                              if icheck=pos.then  y=a*x+y.
  !     matrix a is real, symmetric and stored as upper triangular matrix
  !     in one-dimensional array (1 element for first column, 2 for second
  !     column etc.). y must not be identical with x.
  dimension a(1), x(1), y(1)
  ii = 0
  k = 0
1 k = k + 1
  if(k .gt. n) return
  xx = x(k)
  yy = y(k)
  if(icheck .eq. 0) yy = 0.
  if(icheck .lt. 0) yy = -yy
  do i = 1, k
     ii = ii + 1
     y(i) = y(i) + a(ii) * xx
     yy = yy + a(ii) * x(i)
  end do
  y(k) = yy
  go to 1
end subroutine mult
!
!     end of file: main00.for
!
