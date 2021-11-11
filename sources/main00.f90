!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file main00.f90
!

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
!     The utpf is a large 80-column bcd card-image file, to be used   *
!     as input to e/t programs.   E/t programs machine translate this *
!     utpf code into legal fortran for most computer systems of       *
!     interest (ibm, cdc, univac, honeywell, dec pdp-10, dec vax-11,  *
!     prime, sel, apollo, hitachi, facom, harris, etc.).              *
!                                                                     *
!     In conformity with long-standing bpa policy, as well as the     *
!    more recent (february 19, 1975) federal freedom of information   *
!    act, dissemination of these program materials is freely made     *
!    to any and all interested parties.   a fee to cover reproduction,*
!    handling, and mailing costs may be assessed against the          *
!    organization receiving the material, however.   No claim or      *
!    warranty as to the usefulness, accuracy, fidelity, or            *
!    completeness of these materials is (or ever has been) in any     *
!    way expressed or implied.                                        *
!                                                                     *
!**********************************************************************
!
!    The present module  main00  is always in memory.   It is the
!    highest level module of a program which has two levels of
!    overlaying.   It calls primary level overlays only (directly),
!    based on the value of variable  'nchain' .   The following
!     legitimate values, and the meaning of the associated overlay
!     calls, exist .....
!     1-20.  For overlays 1, 2, ..., 20, which are secondary-level
!            overlays, a call must be first made to the controlling
!            primary-level overlay.   Thus for such  'nchain'  values,
!            control is transfered first to module  main10 .   This
!            is the only case where calls to overlays are not made
!            directly.
!
!     29.  Completion of statistics (Monte Carlo) study, where variable
!          maxima of the different case solutions are read off the
!          disk, and are processed statistically to produce
!          cumulative distribution functions, etc.
!
!     31.  Plot routine, for graphical output of transients.
!          the program also terminates execution here, usually,
!          after writing an end-of-information mark on the
!          plot tape (whether or not the user has plotted anything).
!
!     39.  Supporting routine which generates EMTP branch
!          cards for the frequency-dependent representation of
!          an untransposed transmission line.   this is the
!          "marti setup"  code, named after dr. jose marti of
!          vancouver and caracas (see 1981 ieee pica paper).
!
!     41.  Supporting routine which calculates transformer matrices  (R)
!          and  (L)  from short-circuit and open-circuit data.
!
!     42.  Supporting routine which converts an rms voltage vs. current
!          saturation characteristic into an instantaneous flux vs.
!          current characteristic.
!
!     43.  Supporting routine which calculates weighting functions
!          a1(t)  and  a2(2)  for the zero-sequence mode of a
!          distributed line which has frequency-dependent line
!          constants  r  and  l .
!
!     44.  Supporting routine which calculates line constants for
!          overhead transmission lines by means of carson's formula.
!          this is a modified version of what was originally (until
!          january 1975) the completely-separate bpa line-constants
!          program.
!
!     45.  Supporting routine of  'Semlyen setup'  code.   the output
!          is a group of punched cards, as are required for the EMTP
!          simulation of a transmission circuit using  semlyen
!          recursive convolution modeling.
!
!     47.  Supporting routine of  'cable constants'  code.   the
!          primary function is to calculate  (r),  (l),  %  (c)
!          matrices for a multi-phase system of single-core coaxial
!          cables.
!
!     51.  Printing of introductory paragraph of error-message
!          termination ('you lose, fella, ... '), plus error-message
!          texts for  'kill'  codes numbered  1  through  50 .
!          the exiting linkage is to the last error overlay.
!
!     52.  Error message texts for  'kill'  codes numbered  51
!          the exiting linkage is to the last error overlay.
!
!     53.  Error message texts for  'kill'  codes numbered  91
!          through  150.  the exiting linkage is to the last
!          error overlay.
!
!     54.  Error message texts for  'kill'  codes numbered  151
!          through  200.   the exiting linkage is to the
!          last error overlay.
!
!     55.  Final error overlay.  messages for  kill = 201
!          onward are contained, as well as summary statistics
!          --- table sizes and timing figures for the run.
!          the exiting linkage is generally to module  over1  (to read
!          a new data case), but may be to module  over31 (for final
!          case termination).
!**********************************************************************

module timers
  implicit none
  real(8) :: cputime
  save
end module timers

!
! program gemtp.
!

program gemtp
  use blkcom
  use volpri
  use iocons
  use movcop
  implicit none
  integer(4) :: ll34
  !
  data ll34 / 34 /
  !     unit assignments of "over1" needed earlier by spy:
  lunit0 = gfortran_stderr_unit
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
  do
     if (kill .eq. 0) then
        call a2001
     else if (kill .eq. 9999) then
        call a2001
     else if (kill .ne. 7733) then
        call a4372
     else
        write (unit = lunit6, fmt = 4367)
4367    format (' "main00" intercept of "begin" request.')
        kill = 0
        numdcd = 0
        nchain = 1
        call main10
     end if
  end do
  return

contains
  subroutine a2001
    integer n1
    !
    n1 = nchain
    if (n1 .gt. 30) n1 = n1 - 30
    if (n1 .le. 0) n1 = 1
    iprsup = iprsov(n1)
    if (nchain .gt. 20) then
       call a2010
    else
       if (nchain .eq. 12 .or. nchain .eq. 2) go to 1983
       if (nchain .eq. -1) call move0 (iprsov, ll34)
       call erexit
       nchain = 0
       if (nchain .gt. 20) return
1983   call main10
    end if
    return
  end subroutine a2001

  subroutine a2010
    if (m4plot .eq. 1) call emtspy
    if (nchain .gt. 29) then
       if (nchain .gt. 31) then
          if (nchain .gt. 39) then
             if (nchain .gt. 41) then
                if (nchain .gt. 42) then
                   if (nchain .gt. 44) then
                      if (nchain .gt. 45) then
                         if (nchain .gt. 47) then
                            if (nchain .gt. 51) then
                               if (nchain .gt. 52) then
                                  if (nchain .gt. 53) then
                                     if (nchain .gt. 54) then
                                        if (nchain .gt. 55) then
                                           write (unit = lunit6, fmt = 9236) nchain
9236                                       format (/, ' Illegal nchain in main00.', i8)
                                        else
#ifdef WITH_OVER55
                                           call over55
#else
                                           call dummy
#endif
                                        end if
                                     else
#ifdef WITH_OVER54
                                        call over54
#else
                                        call dummy
#endif
                                     end if
                                  else
#ifdef WITH_OVER53
                                     call over53
#else
                                     call dummy
#endif
                                  end if
                               else
#ifdef WITH_OVER52
                                  call over52
#else
                                  call dummy
#endif
                               end if
                            else
#ifdef WITH_OVER51
                               call over51
#else
                               call dummy
#endif
                            end if
                         else
#ifdef WITH_OVER47
                            call over47
#else
                            call dummy
#endif
                         end if
                      else
#ifdef WITH_OVER45
                         call over45
#else
                         call dummy
#endif
                      end if
                   else
#ifdef WITH_OVER44
                      call over44
#else
                      call dummy
#endif
                   end if
                else
#ifdef WITH_OVER42
                   call over42
#else
                   call dummy
#endif
                end if
             else
#ifdef WITH_OVER41
                call over41
#else
                call dummy
#endif
             end if
          else
#ifdef WITH_OVER39
             call over39
#else
             call dummy
#endif
          end if
       else
#ifdef WITH_OVER31
          call over31
#else
          call dummy
#endif
       end if
    else
#ifdef WITH_OVER29
       call over29
#else
       call dummy
#endif
    end if
  end subroutine a2010

  subroutine a4372
    if (nchain .le. 51) nchain = 51
    call a2001
    return
  end subroutine a4372

end program gemtp

!
! subroutine stoptp.
!

subroutine stoptp
  use blkcom
  implicit none
  !     Temporary stop statements of EMTP have been converted to
  !     "call stoptp", allowing installation-dependent clean up.
  integer(4) :: i, ios
  !
  read (unit = abuff(1), fmt = 5607, iostat = ios) (texcol(i), i = 1, 80)
5607 format (80a1)
  if (ios .ne. 0) go to 9000
  if (nchain .eq. 31 .and. lastov .eq. 1 .and. kill .eq. 9999) go to 9000
  write (unit = lunit6, fmt = 5623) nchain, lastov, (texcol(i), i = 1, 80)
5623 format (/, ' Temporary error stop in "stoptp".   nchain, lastov =', 2i5, 5x, 'last-read card image abuff follows ....', /, 80a1)
9000 stop
end subroutine stoptp

!
! subroutine erexit.
!

subroutine erexit
  use blkcom
  use iocons
  use comkwt
  implicit none
  !     VAX-11   installation-dependent EMTP module.   This is
  !     called by the top of "main00", before any emtp data input.
  !     dimension idum(3)                                   !  dummy vector for ctrl-c handling

  external kwiter                                           ! needed for ctrl-c initialization
  !
  lunit6 = gfortran_stdout_unit                             ! for use of "prompt" until fixed tt?? address
  muntsv(2) = 49                                            ! alternate munit5 unit number of spy
  kwtvax = 0                                                ! set flag corresponding to no ctrl-c usage
  !      call enable_ctrl_c ( kwiter, idum(1) )             ! initialize ctrl-c
  call datain                                               ! read data in, process $include, spy, etc.
  return
end subroutine erexit

!
! subroutine runtym.
!

subroutine runtym (d1, d2)
  use timers
  implicit none
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
  real(8), intent(out) :: d1, d2
  integer(4) :: time
  real(8) :: now_cputime
  !
  call cpu_time (now_cputime)
  time = int(1e6 * now_cputime)
  d1 = (time - cputime) / 1e6
  d2 = 0.0
  return
end subroutine runtym

!
! subroutine settym.
!

subroutine settym
  use timers
  implicit none
  real(8) :: time
  !
  call cpu_time (time)
  if (time .eq. -1.0) then
     write (unit = 6, fmt = *) 'Error, no timer unit available!'
  else
     cputime = 1e6 * time
  end if
  return
end subroutine settym

!
! subroutine cimage.
!

subroutine cimage
  use blkcom
  use labcom
  use strcom
  use freedom
  implicit none
  !     VAX-11  installation-dependent emtp module which serves
  !     to return the next input card.  All systems will substitute.
  integer(4) :: i, ios, j, jpntr(201), k, kfile5, l, munit5
  integer(4) :: n1, n2, n3, n4, n6, n7, n8, n11, n12, n13
  real(8) :: d1, d11
  character(8) :: charc, chtacs, textax(60), textay(50), text1, text2
  character(8) :: text4, text5
  character(25) :: filen
  !  dimension xopt(1), copt(1)
  !  dimension buff10(10)
  !  dimension textax(60), jpntr(201), textay(50), aupper(10)
  !
  !  equivalence (buff10(1), abuff(1))
  !  equivalence (aupper(1), texcol(1))
  !
  !     Burroughs: preserve local variable between module calls:
  data n8         / 0 /        ! remember last $-card number
  data charc      / 'c' /
  data text4      / '9' /
  data chtacs     / 'tacs' /
  data text5      / 'blank ' /
  !              *****    request no. 1.    "$attach"      *****  *****
  data textay(1)  / 'a     ' /
  data jpntr(1)   / 1 /
  data textax(1)  / 'attach' /
  !              *****    request no. 2.    "$punch"       *****  *****
  data textay(2)  / 'p     ' /
  data jpntr(2)   / 2 /
  data textax(2)  / 'punch ' /
  !              *****    request no. 3.    "$output"      *****  *****
  data textay(3)  / 'out   ' /
  data jpntr(3)   / 3 /
  data textax(3)  / 'output' /
  !              *****    request no. 4.    "$save"        *****  *****
  data textay(4)  / 's     ' /
  data jpntr(4)   / 4 /
  data textax(4)  / 'save  ' /
  !              *****    request no. 5.    "$spydata"     *****  *****
  !                  former "$include", which is now in "erexit".
  data textay(5)  / 'spy   ' /
  data jpntr(5)   / 5 /
  data textax(5)  / 'spydat' /
  data textax(6)  / 'a     ' /
  !              *****    request no. 6.    "$disable"     *****  *****
  data textay(6)  / 'd     ' /
  data jpntr(6)   / 7 /
  data textax(7)  / 'disabl' /
  data textax(8)  / 'e     ' /
  !              *****    request no. 7.    "$enable"      *****  *****
  data textay(7)  / 'e     ' /
  data jpntr(7)   / 9 /
  data textax(9)  / 'enable' /
  !              *****    request no. 8.    "$return"      *****  *****
  data textay(8)  / 'r     ' /
  data jpntr(8)   / 10 /
  data textax(10) / 'return' /
  !              *****    request no. 9.    "newfile"      *****  *****
  data textay(9)  / 'n     ' /
  data jpntr(9)   / 11 /
  data textax(11) / 'newfil' /
  data textax(12) / 'e     ' /
  !              *****    request no. 10.   "new epsiln"   *****  *****
  data textay(10) / 'ne    ' /
  data jpntr(10)  / 13 /
  data textax(13) / 'new   ' /
  data textax(14) / 'epsiln' /
  !              *****    request no. 11.   "delete"       *****  *****
  data textay(11) / 'de    ' /
  data jpntr(11)  / 15 /
  data textax(15) / 'delete' /
  !              *****    request no. 12.   "monitor"      *****  *****
  data textay(12) / 'm     ' /
  data jpntr(12)  / 16 /
  data textax(16) / 'monito' /
  data textax(17) / 'r     ' /
  !              *****    request no. 13.   "listoff"      *****  *****
  data textay(13) / 'lf    ' /
  data jpntr(13)  / 18 /
  data textax(18) / 'listof' /
  data textax(19) / 'f     ' /
  !              *****    request no. 14.   "liston"       *****  *****
  data textay(14) / 'ln    ' /
  data jpntr(14)  / 20 /
  data textax(20) / 'liston' /
  !              *****    request no. 15.   "vintage"      *****  *****
  data textay(15) / 'v     ' /
  data jpntr(15)  / 21 /
  data textax(21) / 'vintag' /
  data textax(22) / 'e     ' /
  !              *****    request no. 16.   "oldfile"      *****  *****
  data textay(16) / 'old   ' /
  data jpntr(16)  / 23 /
  data textax(23) / 'oldfil' /
  data textax(24) / 'e     ' /
  !              *****    request no. 17.   "stop"         *****  *****
  data textay(17) / 'st    ' /
  data jpntr(17)  / 25 /
  data textax(25) / 'stop  ' /
  !              *****    request no. 18.   "watch5"       *****  *****
  data textay(18) / 'w     ' /
  data jpntr(18)  / 26 /
  data textax(26) / 'watch5' /
  !              *****    request no. 19.   "comment"      *****  *****
  data textay(19) / 'com   ' /
  data jpntr(19)  / 27 /
  data textax(27) / 'commen' /
  data textax(28) / 't     ' /
  !              *****    request no. 20.   "width"        *****  *****
  data textay(20) / 'wi    ' /
  data jpntr(20)  / 29 /
  data textax(29) / 'width ' /
  !              *****    request no. 21.   "units"        *****  *****
  data textay(21) / 'u     ' /
  data jpntr(21)  / 30 /
  data textax(30) / 'units ' /
  !              *****   bounds follow ...
  data jpntr(22)  / 31 /
  data jpntr(23)  / 0 /
  data n11        / 0 /
  data n12        / 99999 /
  data n13        / 99999 /
  n6 = 0
  if (iprsup .ge. 10) write (unit = lunit6, fmt = 987) lunit5, lunit6, noutpr, numdcd
987 format (' Begin cimage.  lunit5, lunit6, noutpr, numdcd =', 4i5)
1000 if (m4plot .eq. 1) call emtspy                         ! interactive usage
  if (lunit5 .gt. 0) read (unit = lunit5, fmt = 3000, end = 4000) (buff10(i), i = 1, 10)
3000 format (10a8)
  if (lunit5 .le. 0) call nextcard
  if (kill .gt. 0) go to 4000                               ! "nextcard" eof jump
  if (lunsav(5) .ne. -5) numdcd = numdcd + 1
  read (unit = abuff, fmt = 3012, iostat = ios) text1, text2
3012 format (2a1)
  if (ios .ne. 0) then
     write (unit = lunit6, fmt = "('Could not read from abuff.  Stop.')")
     stop
  end if
  if (text1 .ne. charc) go to 3034
  if (text2 .ne. blank) go to 3034
1036 if (noutpr .ne. 0) go to 1000
  if (n11 .ne. 0) go to 1000
  if (kol132 .eq. 132) write (unit = lunit6, fmt = 3015) buff10
3015 format (' Comment card.', 37x, '|', 10a8)
  if (kol132 .ne. 132) write (unit = lunit6, fmt = 3016) (abuff(j), j = 1, 4)
  !  if (kol132 .ne. 132) write (unit = lunit6, fmt = 3016) abuff(1 : 29)
3016 format (' Comment card.', 37x, '1', 3a8, a5)
!3016 format (' Comment card.', 37x, '|', a29)
  go to 1000
3034 if (noutpr .ne. 0) go to 3035
  if (kol132 .eq. 132) write (unit = lunit6, fmt = 3006) buff10
3006 format (51x, '|', 10a8)
  if (kol132 .ne. 132) write (unit = lunit6, fmt = 3007) (abuff(j), j = 1, 4)
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
  do i = 1, 10
     abuff(i) = blank
  end do
  go to 3233
3040 if (chcont .eq. text4) go to 3233
  read (unit = abuff(1), fmt = 3041, iostat = ios) texcol
3041 format (80a1)
  !     Dan Goldsworthy had trouble with $listoff within $include
  !     which was within tacs supplemental variables.  wsm+thl
  if (to_lower (abuff(1)) .ne. '$listoff' .and. to_lower (abuff(1)) .ne. '$liston') go to 3042
  go to 3246
  !     chcont is 'tacs' if cimage called from within tacs fortran express
3042 if (chcont .eq. chtacs) go to 3233
  if (texcol(1) .eq. chcont) go to 3246
  if (n8 .ne. 6) go to 1144
3044 if (noutpr .eq. 0) write (unit = lunit6, fmt = 3045)
3045 format ('+Comment card (implicit).')
  go to 1000
1144 do k = 1, 80
     if (texcol(k) .eq. csepar) go to 3237
     if (texcol(k) .eq. chcont) go to 3237
  end do
3233 kolbeg = -intinf
  go to 7014
3237 kolbeg = 1
  go to 7014
3246 kolbeg = 2
  !     3251 nright = -2
  nright = -2
  !  call freone (d1)
  call free (d1)
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 3281) nfrfld, texta6(1), texta6(2)
3281 format (/, ' nfrfld =', i8, 5x, 'texta6 =', 2a7)
  nright = 0
  do i = 1, 200
     n1 = jpntr(i)
     n2 = jpntr(i + 1) - 1
     if (n2 .lt. 0) go to 3319
     if (iprsup .ge. 35) write (unit = lunit6, fmt = 3285) i, (textax(j), j = n1, n2)
3285 format (' Special-request word', i4, ' .', 10a6)
     if (textax(n1) .eq. blank) go to 3306
     l = 0
     n3 = n2 - n1 + 1
     if (n3 .ne. nfrfld) go to 3306
     do j = n1, n2
        l = l + 1
        if (texta6(l) .ne. textax(j)) go to 3306
     end do
     if (iprsup .ge. 2) write (unit = lunit6, fmt = 3292) i, n8, texta6(1), textay(i)
3292 format (/, ' key-word found.  i, n8 =', 2i5, 5x, 'texta6, textay =', 2a7)
3294 if (n8 .ne. 6) go to 3301
     if (i .ne. 7) go to 1036
3301 n8 = i
     select case (n8)
     case (1)
        go to 4100

     case (2)
        go to 4200

     case (3)
        go to 4300

     case (4)
        go to 4400

     case (5)
        go to 4500

     case (6)
        go to 4600

     case (7)
        go to 4700

     case (8)
        go to 4800

     case (9)
        go to 4900

     case (10)
        go to 5000

     case (11)
        go to 5100

     case (12)
        go to 5200

     case (13)
        go to 5300

     case (14)
        go to 5400

     case (15)
        go to 5500

     case (16)
        go to 5600

     case (17)
        go to 5700

     case (18)
        go to 5800

     case (19)
        go to 5900

     case (20)
        go to 6000

     case (21)
        go to 6100

     end select
3306 if (to_lower (texta6(1)) .eq. textay(i)) go to 3294
  end do
3319 write (unit = lunit6, fmt = 3230)
3230 format (' Illegal $-card.   Stop at s.n. 3319 of "cimage" .')
  call stoptp                                               ! installation-dependent program stop card
  !               *****    request no. 1.    "$attach"      *****  *****
4100 text1 = 'attach'
  go to 4506
  !               *****    request no. 2.    "$punch"       *****  *****
4200 text1 = textax(2)
4206 n2 = lunit7
4209 nfrfld = 1
  !  call freone (d11)
  call free (d11)
  n1 = int (d11)
  !     4225 if ( n1 .le. 0 ) n1 = n2
  if (n1 .le. 0) n1 = n2
  if (n8 .eq. 8) go to 4817
  if (noutpr .eq. 0) write (unit = lunit6, fmt = 4231) n1, text1
4231 format ('+Copy file', i4, '   to ', a6, ' .')
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind n1
  do k = 1, intinf
     read (unit = n1, fmt = 3000, end = 4249) aupper
     !     because DEC Fortran does not honor the CDC  "punch"  statement
     !     (to directly punch cards), i write to unit 7 as next best
     !     thing.  wsm.  Jan, 1980.
     if (n8 .eq. 2 ) write (unit = lunit7, fmt = 3000) aupper
     if (n8 .eq. 3 ) write (unit = lunit6, fmt = 4238) k, aupper
4238 format (20x, 'record', i5, ' .  1', 10a8)
  end do
  !     segmented, 1, VAX e/t can skip translation of rewind:
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
  !     segmented, 1, VAX e/t can skip translation of rewind:
  rewind n6
  do k = 1, intinf
     read (unit = n6, fmt = 3000, end = 4436) aupper
     write (unit = n7, fmt = 3000) aupper
  end do
  !     4436 close ( unit=n7,  dispose='save' )
4436 close (unit = n7, status = 'keep')
  !     segmented, 1, vax e/t can skip translation of rewind:
  rewind n6
  go to 1000
  !               *****    request no. 5.    "$spy"         *****  *****
4500 text1 = 'spying'
  muntsv(2) = 2288                                          ! remember $spy for eof time in "spying"
  muntsv(1) = munit5                                        ! remember spy input channel till now
  munit5 = 17                                               ! special i/o unit used for $spy connection
  kfile5 = 1                                                ! flag so "spying" knows munit5 opened to disk
4506 n4 = 0
  write (unit = filen(1 : 25), fmt = 4523)
4523 format (25x)
  do k = kolbeg, 80
     if (texcol(k) .eq. blank) go to 4532
     if (texcol(k) .eq. csepar) go to 4536
     if (texcol(k) .eq. '(') go to 4536
     n4 = n4 + 1
     !     encode (1, 3041, filen(n4))  texcol(k)
     write (unit = filen(n4 :), fmt = 3041) texcol(k)
  end do
4532 continue
  k = 80
4536 kolbeg = k + 1
  nfrfld = 1
  !  call freone (d11)
  call free (d11)
  n7 = int (d11)
  if (n8 .ne. 4) go to 4557
  !  call freone (d11)
  call free (d11)
  n6 = int (d11)
4557 if (n6 .eq. 0) n6 = lunit7
  if (n8 .eq. 5) n7 = munit5                                ! $spy uses this channel
  if (n7 .gt. 0) go to 4570
  do  k = 1, 15
     if (lunsav(k) .le. 0) go to 4568
  end do
  write (unit = lunit6, fmt = 4565)
4565 format (/, 10(' error,'))
  write (unit = lunit6, fmt = 4566) lunsav
4566 format (5x, ' All i/o channels occupied.  Kill run at  s.n. 4566  of  "cimage" .', /, 20i5)
  call stoptp                                               ! installation-dependent program stop card
4568 n7 = k
4570 if (noutpr .eq. 0) write (unit = lunit6, fmt = 4572) text1, filen, n7
4572 format ('+', a6, ' file:', 25a1, ' unit =', i3)
  if (n8 .eq. 4) go to 4423
  if (n8 .eq. 9) go to 4907
  if (n8 .eq. 11) go to 5106
  if (n8 .eq. 16) go to 5608
  close (unit = n7)
  open (unit = n7, status = 'old', form = 'formatted', file = filen)
  rewind n7
  if (n8 .eq. 5) call spying                                ! process $spy command file
  go to 1000
  !               *****    request no. 6.    "$disable"     *****  *****
4600 if (noutpr .eq. 0) write (unit = lunit6, fmt = 4612)
4612 format ('+Begin data to be ignored.')
  go to 1000
  !               *****    request no. 7.    "$enable"      *****  *****
4700 if (noutpr .eq. 0) write (unit = lunit6, fmt = 4714)
4714 format ('+End of data to be ignored.')
  go to 1000
  !               *****    request no. 8.    "$return"      *****  *****
4800 n2 = lunit4
  go to 4209
4817 close (unit = n1)
  if (noutpr .eq. 0) write (unit = lunit6, fmt = 4823) n1
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
  !  call freone (epsiln)
  call free (epsiln)
  if (noutpr .eq. 0) write (unit = lunit6, fmt = 5017) d1, epsiln
5017 format ('+Epsiln change.  old, new =', 2e11.2)
  go to 1000
  !               *****    request no. 11.   "delete"       *****  *****
5100 text1 = 'delete'
  go to 4506
  !     5106 open ( unit=n7, type='old', name=filen )
5106 open (unit = n7, status = 'old', file = filen)
  !     close( unit=n7, dispose='delete' )
  close (unit = n7, status = 'delete')
  go to 1000
  !               *****    request no. 12.   "monitor"      *****  *****
5200 if (noutpr .ne. 0) go to 5219
  print 3006, buff10
  print 5214, numdcd
5214 format ('+CRT monitor.  card number =', i5)
5219 go to 1000
  !               *****    request no. 13.   "listoff"      *****  *****
5300 if (noutpr .ne. 0) go to 5324
  write (unit = lunit6, fmt = 5307) numdcd
5307 format ('+Turn off input listing at card', i5)
  noutpr = 1
5324 go to 1000
  !               *****    request no. 14.   "liston"       *****  *****
5400 write (unit = lunit6, fmt = 5404)
5404 format (51x, '1$liston')
  noutpr = 0
  write (unit = lunit6, fmt = 5412) numdcd
5412 format ('+Turn on input listing at card', i5)
  go to 1000
  !               *****    request no. 15.   "vintage"      *****  *****
5500 nfrfld = 1
  !  call freone (d11)
  call free (d11)
  moldat = int (d11)
  if (noutpr .eq. 0) write (unit = lunit6, fmt = 5518) moldat
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
5700 write (unit = lunit6, fmt = 5706)
5706 format ('+Stop execution immediately in "cimage".')
  call stoptp                                               ! installation-dependent program stop card
  !               *****    request no. 18.   "watch5"       *****  *****
5800 nfrfld = 1
  !  call freone (d11)
  call free (d11)
  n12 = int (d11)
  n13 = n12
  if (noutpr .eq. 0) write (unit = lunit6, fmt = 5812) n12
5812 format ('+Paint input data on screen.', i8)
  go to 1000
  !               *****    request no. 19.   "comment"      *****  *****
5900 n11 = n11 + 1
  if (n11 .ge. 2) n11 = 0
  if (noutpr .eq. 0) write (unit = lunit6, fmt = 5917) n11
5917 format ('+Toggle comment card destruction flag.', i8)
  go to 1000
6000 call stoptp                                            ! installation-dependent program stop card
  !               *****    request no. 21.   "units"        *****  *****
6100 nfrfld = 1
  !  call frefld (xopt)
  call free (xopt)
  !  call frefld (copt)
  call free (copt)
  if (noutpr .eq. 0) write (unit = lunit6, fmt = 6114) xopt, copt
6114 format ('+New  xopt, copt =', 2e14.4)
  xunits = 1000.
  if (xopt .gt. 0.0) xunits = twopi * xopt
  go to 1000
  !     additional key-word code goes below.
4000 write (unit = lunit6, fmt = 4006)
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
  write (unit = inecho, fmt = 3000) buff10
  return
  entry ibrinc
  ibr = ibr + 1
  xoptbr(ibr) = xopt
  coptbr(ibr) = copt
  return
end subroutine cimage

!
! subroutine ioerr.
!

subroutine ioerr (naddr)
  implicit none
  integer(4), intent(out) :: naddr
  !
  naddr = 0
  return
end subroutine ioerr

!
! subroutine caterr.
!

subroutine caterr (naddr, koderr)
  implicit none
  integer(4), intent(out) :: koderr, naddr
  !
  naddr = 0
  koderr = 0
  return
end subroutine caterr

!
! subroutine trgwnd.
!

subroutine trgwnd (x, d17)
  use blkcom
  implicit none
  real(8), intent(in) :: x
  real(8), intent(out) :: d17
  integer(4) :: n13
  !
  d17 = x
  if (dabs (x) .lt. 25000.) go to 9000
  n13 = int (x / twopi)
  d17 = d17 - n13 * twopi
  if (iprsup .ge. 1) write (unit = *, fmt = 3456) nchain, x, d17
3456 format (' Angle unwind in "trgwnd" called by "rfunl1".   nchain, x, d17 =', i5, 2e25.16)
9000 return
end subroutine trgwnd

!
! subroutine cmultz.
!

subroutine cmultz (ar, ai, br, bi, cr, ci, ksn)
  implicit none
  integer(4), intent(in) :: ksn
  real(8), intent(out) :: ar, ai
  real(8), intent(in) :: br, bi, cr, ci
  real(8) :: sr
  !
  sr = br * cr - bi * ci
  ai = bi * cr + br * ci
  ar = sr
  if (ksn .ge. 0) go to 200
  ar = -ar
  ai = -ai
200 return
end subroutine cmultz

!
! subroutine cdivz.
!

subroutine cdivz (ar, ai, br, bi, cr, ci, ksn)
  implicit none
  integer(4), intent(in) :: ksn
  real(8), intent(out) :: ar, ai
  real(8), intent(in) :: br, bi, cr, ci
  real(8) :: fac, sr
  !
  fac = cr * cr + ci * ci
  fac = 1. / fac
  sr = br * fac * cr + bi * fac * ci
  ai = bi * fac * cr - br * fac * ci
  ar = sr
  if (ksn .ge. 0) go to 200
  ar = -ar
  ai = -ai
200 return
end subroutine cdivz

!
! subroutine setmar.
!

subroutine setmar (n)
  implicit none
  integer(4), intent(out), optional :: n
  !
  !     Subroutine to change page size and eject page for printer plots.
  !     Dummy module since this ontario hydro (univac) trick is unknown.
  if (present (n)) n = 0
  return
end subroutine setmar

!
! subroutine chrsiz.
!

subroutine chrsiz (n)
  implicit none
  integer(4), intent(out), optional :: n
  if (present (n)) n = 0
  !     Entry point for do-nothing mimiced tektronix plot10 of "tekplt"
  return
end subroutine chrsiz

!
! subroutine setplt.
!

subroutine setplt
  implicit none
  !     Entry point to change lines/page to maximum number allowed,
  !     to allow printer plots to be continuous over page boundries.
  !     call system dependant routine to change page size
  !     write (lunit6, 1000 )
  !     1000 format (1h1)
  return
end subroutine setplt

!
! subroutine setstd.
!

subroutine setstd
  !     Entry point to restore page limits to standard values.
  !     call system dependant routine to change page size
  !     write (lunit6, 1000 )
  return
end subroutine setstd

!
! subroutine interp.
!

subroutine interp
  implicit none
  !     If  character*51 kunit6,  active module needed to flush
  !     abuff and kunit6 as 132-column interpreted data card line.
  return
end subroutine interp

!
! subroutine addmxd.
!

subroutine addmxd (a, b, c, n)
  implicit none
  !     Subroutine  addmxd  forms matrix   (c) = (a) + b(u)  , where (a),
  !     and (c)  are n by n matrices,  b  is a scalar, and (u) is the
  !     identity matrix.   Array (c) may be the same as (a), if desired.
  !     See subr.  mult  for symmetric-matric storage scheme assumed.
  integer(4), intent(in) :: n
  real(8), intent(in) :: a(:)
  real(8), intent(out) :: c(:)
  real(8), intent(in) :: b
  integer(4) :: j, jt, k, l
  !
  k = 1
  j = 1
  jt = n * (n + 1) / 2
  do l = 1, jt
     c(l) = a(l)
     if(l .lt. k) go to 3010
     c(l) = c(l) + b
     j = j + 1
     k = k + j
  end do
3010 continue
  return
end subroutine addmxd

!
! subroutine multmx.
!

subroutine multmx (a, b, c, temp, n)
  use movcop
  implicit none
  !     Subroutine multmx  forms the matrix product   (c) = (a)(b)   where
  !     matrices  (a), (b), and (c)  are all  n by n  square arrays.
  !     Array  'temp'  is a scratch working area of not less than  2n
  !     cells.   Arrays  (b)  and  (c)  may be identical, thereby placing
  !     the product   (a)(b)   back into  (b) .    See subroutine  'mult'
  !     which is called herein, for details about the storage scheme used
  !     )    for these real, symmetric matrices.
  integer(4), intent(in) :: n
  real(8), intent(out) :: b(:), c(:), temp(:)
  real(8), intent(in) :: a(:)
  integer(4) :: i, ii, j, l, ll0, m
  !
  l = 0
  ll0 = 0
  ii = 0
  do j = 1, n
     do i = 1, n
        if(i .le. j) go to 3420
        l = l + (i - 1)
        go to 3430
3420    l = ii + i
3430    temp(i) = b(l)
     end do
     m = n + 1
     call mult (a, temp(1 :), temp(m :), n, ll0)
     !     call mover (temp(m), c(ii + 1), j)
     call move (temp(m :), c(ii + 1 :), j)
     ii = ii + j
  end do
  return
end subroutine multmx

!
! subroutine packa1.
!

subroutine packa1 (from, to, kk)
  implicit none
  !     System-dependent EMTP module  'packa1'  for  VAX-11/780.
  !     Argument  'from'  contains  a1  information which is to be stored
  !     in character position  kk  of argument  'to' .
  !     For all EMTP usage,  1st 2 arguments must be vectors.
  !     logical*1 from(1), to(6)
  integer(4), intent(in) :: kk
  character(1), intent(in) :: from
  character(*), intent(out) :: to
  !
  to(kk : kk) = from(1 : 1)
  return
end subroutine packa1

!
! subroutine packch.
!

subroutine packch (from, to, k4or6, nchbeg, nword)
  implicit none
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
  logical(1), intent(in) :: from(*)
  logical(1), intent(out) :: to(*)
  integer(4), intent(in) :: k4or6, nchbeg, nword
  integer(4) :: ichar, iword, jchar, kchar
  !
  ichar = nchbeg
  do iword = 1, nword
     do kchar = 1, k4or6
        jchar = (iword - 1) * 8 + kchar
        to(ichar) = from(jchar)
        ichar = ichar + 1
     end do
  end do
  return
end subroutine packch

!
! subroutine mult.
!

subroutine mult (a, x, y, n, icheck)
  implicit none
  !     subroutine  'mult'  is used to post-multiply a symmetric matrix
  !     by a vector.
  !     a=matrix,x and y=vectors.if icheck=0   then  y=a*x
  !     n=size of matrix.        if icheck=neg.then  y=a*x-y
  !                              if icheck=pos.then  y=a*x+y.
  !     matrix a is real, symmetric and stored as upper triangular matrix
  !     in one-dimensional array (1 element for first column, 2 for second
  !     column etc.). y must not be identical with x.
  integer(4), intent(in) :: icheck, n
  real(8), intent(in) :: a(1), x(1)
  real(8), intent(out) :: y(1)
  integer(4) :: i, ii, k
  real(8) :: xx, yy
  !
  ii = 0
  k = 0
  do
     k = k + 1
     if(k .gt. n) return
     xx = x(k)
     yy = y(k)
     if (icheck .eq. 0) yy = 0.
     if (icheck .lt. 0) yy = -yy
     do i = 1, k
        ii = ii + 1
        y(i) = y(i) + a(ii) * xx
        yy = yy + a(ii) * x(i)
     end do
     y(k) = yy
  end do
end subroutine mult

subroutine dummy
  implicit none
  write (unit = *, fmt = *) 'Dummy subroutine called.'
  call stoptp
  return
end subroutine dummy

!
! end of file main00.f90
!
