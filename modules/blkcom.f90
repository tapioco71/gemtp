!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file blkcom.f90
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

module comthl
  !  common /comthl/ angtpe, nswtpe
  integer(4) :: nswtpe
  real(8) :: angtpe
end module comthl

module comld
  !  common /comld/ newtac
  integer(4) :: newtac
end module comld

module blkcom
  implicit none
  !     Parameters.
  real(8), parameter :: fltinf = huge (0.0)
  real(8), parameter :: flzero = tiny (0.0)
  real(8), parameter :: speedl = 2.997925d8
  !
  !     flag-1.   begin class-1  /blank/  variables
  !               (alphanumeric variables, capable of storing  a6  info).
  !
  character(1) :: chcont, csepar
  character(8), target :: texcol(80)
  character(8) :: blank, bus2, bus3, bus4, bus5, bus6
  character(8), target :: bus1
  character(8) :: trash, terra, userid, branch, chcopy
  character(8) :: vstacs(24), texta6(15)
  character(80), target :: abuff
  !
  ! common abuff
  ! common bus1, bus2, bus3, bus4, bus5, bus6
  ! common trash, blank, terra, userid, branch, copy
  ! common texcol
  ! common csepar, chcont, texta6
  ! common date1, tclock, vstacs
  !
  !     flag-2.   begin class-2  /blank/  variables
  !               (floating-point numeric usage only, with scalars
  !               preceding arrays).
  !
  real(8) :: ci1, ck1
  real(8) :: deltat, delta2
  real(8) :: freqcs
  real(8) :: xunits, aincr, xmaxmx
  real(8) :: znolim(2), epstop, t, tolmat
  real(8) :: tmax
  real(8), target :: omega
  real(8) :: copt, xopt, szplt
  real(8) :: szbed, sigmax
  real(8) :: fminfs, delffs, fmaxfs, tenerg, begmax(6)
  real(8) :: unity, onehaf
  real(8) :: degmin, degmax, statfr, voltbc(50)
  real(8) :: flstat(20)
  real(8), target :: angle
  real(8) :: pu, dltinv
  real(8) :: epsuba, epomeg, epdgel, epsiln, epszno, epwarn
  real(8) :: peaknd(3)
  real(8) :: sglfir
  real(8) :: tenm3, tenm6, twopi
  !
  !     flag-3.   begin class-3  /blank/  variables
  !               (integer-numeric usage only, with arrays
  !               preceding scalars).
  !
  integer(4) :: icheck, iline, inonl, iout, ipunch, iread, istep, itype, it1
  integer(4) :: ia, ipntv(11), it2, izero
  integer(4), target :: indtv(10)
  integer(4), target :: iprsov(39)
  integer(4) :: kprchg(6)
  integer(4) :: locatn(0:20, 0:20)
  integer(4), target :: lunit(0 : 63)
  integer(4) :: lunsav(0 : 15)
  integer(4), target :: lstat(80)
  integer(4) :: max99m, multpr(5)
  integer(4), target :: moncar(90)
  integer(4) :: nbyte(6), nright, nfrfld, kolbeg
  integer(4) :: kol132, kpar
  integer(4), pointer :: kunit6 => lunit(6)
  integer(4) :: kwtspy, kxic
  integer(4) :: kcount, istead, ldata, lbrnch
  integer(4) :: lexct, lbus, lymat
  integer(4), pointer :: lswtch => moncar(61)
  integer(4) :: lnonl, lchar, m4plot
  integer(4) :: lpast, lsize7, iplot, ncomp, nv, lcomp, numsm
  integer(4) :: ifdep, ltails, lfdep, lwt, last, npower, maxpe
  integer(4) :: lsiz12, lsmout, limass, iv
  integer(4), target :: ktrlsw(8)
  integer(4) :: num99, kpartb, llbuff, kanal, nsmth
  integer(4) :: nstacs, maxbus, lastov
  integer(4) :: lhist, ifx, isubc1, inecho, noutpr
  integer(4) :: ktab, jflsos, numdcd, numum, lspcum
  integer(4) :: nphcas, ialter, i_char, ktref, memsav, lisoff
  integer(4) :: kburro, iaverg, lsiz23, lsiz26, numout, moldat
  integer(4) :: lsiz27, lsiz28, ltlabl, iwt, ifdep2, idoubl, ioutin
  integer(4) :: ipun, jst, jst1, muntsv(2), numsub, maxzno
  integer(4) :: ifsem, lfsem, iadd, lfd
  integer(4), target :: nexout(17)
  integer(4) :: ibr, icat, iftail, intinf
  integer(4), target :: iofbnd
  integer(4), target :: iofgnd
  integer(4) :: ioffd, iprsup, isplot
  integer(4) :: isprin, it, ivolt
  integer(4) :: modout, lint
  integer(4) :: ncurr, maxout
  integer(4) :: nchain
  integer(4) :: kconst, kswtch, ntot
  integer(4) :: kill, kssout
  integer(4) :: lsyn
  integer(4), target :: loopss(13)
  integer(4) :: numref, nword1, nword2, iloaep
  integer(4) :: ntot1, limstp, indstp, nc
  integer(4), target :: nenerg
  integer(4) :: numnvo
  integer(4) :: swtpe
  integer(4) :: newtac, niu, nsu, nsudv, nsup, nuk
  real(8) :: lnpin
  !
  data terra / 'terra ' /
  !     unit assignments of "over1" needed earlier by spy:
  data lunit / 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, &
       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, &
       32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, &
       48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 /

end module blkcom

!
! end of file blkcom.f90
!
