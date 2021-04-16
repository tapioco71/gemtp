!-*- mode: fortran; syntax: ansi-fortran-90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: ntacs3.for
!
!
!     subroutine ntacs3.
!
subroutine ntacs3
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'tacsto.ins'
  include 'blkcom.ftn'      ! wsm  +  thl manual modification for bpa emtp
  include 'tacsar.ftn'      ! wsm  +  thl manual modification for bpa emtp
  include 'labcom.ins'      ! wsm  +  thl manual modification for bpa emtp
!  common  / c0b002 /   ykm   (   1 )    ! wsm  +  thl manual modification for bpa emtp
!  common  / c0b014 /   sptacs(  29 )    ! wsm  +  thl manual modification for bpa emtp
!  common  / c0b099 /   tclose(   1 )    ! wsm  +  thl manual modification for bpa emtp
!  common  / c0b101 /   kpos  (   1 )    ! wsm  +  thl manual modification for bpa emtp
!  common  / c0b103 /   emtpe (   1 )    ! wsm  +  thl manual modification for bpa emtp
  if (.not. (niu .gt. 0)) go to 5000
  i5 = kud1
  do 4080 i = 1,niu
     i2 = kxtcs + nuk + i
     xtcs(i2) = flzero
     i1 = iuty(kiuty + i)
     k = int(ud1(i5 + 2))
     i6 = iabs(kpos(k))
     i3 = i1-89
     go to(4090, 4091, 4092, 4093, 4080, 4080), i3
4090 xtcs(i2) = emtpe(k)
     go to 4080
4091 if (i6 .le. 3) xtcs(i2) = tclose(k)
     go to 4080
4092 xtcs(i2) = ykm(k)
     go to 4080
4093 if (i6 .le. 3) xtcs(i2) = unity
4080 i5 = i5 + 5
5000 continue
  etime = t
  to = 9000
  call elec
!  lstat(35) = cptr   ! wsm  +  thl manual modification for bpa emtp
!  k = isto(ishenv + 7) ! wsm  +  thl manual modification for bpa emtp
!  moncar(32) = iptr + isto(k)*isto(k + 1)   ! wsm  +  thl manual modification for bpa emtp
!  k = isto(ishenv + 8)                    ! wsm  +  thl manual modification for bpa emtp
!  lstat(48) = rptr-rbase + isto(k)*isto(k + 1)   ! wsm  +  thl manual modification for bpa emtp
  if (.not. (etime + estep / two .gt. estop)) go to 5010
  close(unit08)
  close(bkfile)
5010 continue
  return
end subroutine ntacs3
!
!     end of file: ntacs3.for
!
