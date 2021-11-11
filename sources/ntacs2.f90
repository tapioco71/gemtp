!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file ntacs2.f90
!


!
! subroutine ntacs2.
!

subroutine ntacs2
  use tacsto
  use blkcom
  use tacsar
  use labcom
  implicit none
  !  common  / c0b002 /   ykm   (   1 )    ! wsm + thl manual modification for bpa emtp
  !  common  / c0b063 /   texvec(1000 )    ! wsm + thl manual modification for bpa emtp
  !  character*6  texvec                   ! wsm + thl manual modification for bpa emtp
  !  common  / c0b064 /   bus   (   1 )    ! wsm + thl manual modification for bpa emtp
  !  character*6  bus                      ! wsm + thl manual modification for bpa emtp
  !  common  / c0b094 /   kmswit(   1 )    ! wsm + thl manual modification for bpa emtp
  !  common  / c0b095 /   nextsw(   1 )    ! wsm + thl manual modification for bpa emtp
  !  common  / c0b099 /   tclose(   1 )    ! wsm + thl manual modification for bpa emtp
  !  common  / c0b100 /   adelay(   1 )    ! wsm + thl manual modification for bpa emtp
  !  common  / c0b102 /   namesw(   1 )    ! wsm + thl manual modification for bpa emtp
  !  common  / c0b103 /   emtpe (   1 )    ! wsm + thl manual modification for bpa emtp
  !  common  / c0b104 /   emtpf (   1 )    ! wsm + thl manual modification for bpa emtp
  !  equivalence    ( moncar(32), kitacs ),    ( moncar(61),  lswtch )
  character(6) :: hus1          ! wsm + thl manual modification for bpa emtp
  character(8) :: real8
  !
  if (.not. (niu .gt. 0)) goto 5020
  i5 = kud1
  do i = 1, niu
     i1 = iuty(kiuty + i)
     k = ilntab(klntab + nuk + i)
     real8 = texvec(k)
     if (i1 .ne. 90 .and. i1 .ne. 94) go to 4012
     do j = 2, ntot
        if (real8 .eq. bus(j)) go to 4018 ! wsm + thl manual modification for bpa emtp
     end do
     write (unit = lunit6, fmt = 601) hus1, hus1
601  format(' tacs2 -- as found in a TACS input', /, 'the EMTP function "v(', a6, ')" or "imssv(', a6,')"', /, '          refers to a non-existing node name', /, '          in the electrical network.')
     call stoptp
4012 if (i1 .ne. 91 .and. i1 .ne. 93 .and. i1 .ne. 95) goto 4010
     do j = 1, kswtch
!!     k = namesw(j)
!!     if (texvec(k) .eq. real8 ) goto 4018  ! wsm + thl manual modification for bpa emtp
        k = iabs (kmswit(j))
        m = iabs (kmswit(lswtch + j))
        ! wsm + thl manual modification for bpa emtp
        if (real8 .eq. bus(k) .or. real8 .eq. bus(m)) go to 4018 ! wsm + thl manual modification for bpa emtp
     end do
!4030 continue
     write (unit = lunit6, fmt = 602) hus1, hus1, hus1
602  format(' tacs2 -- as found in a TACS input', /, '          the EMTP function "i(',      a6, ')"', /, '                         or "switch(', a6, ')"', /, '                         or "imssi(',  a6, ')"', /, '          refers to a non-existing switch name or node name', /, '          in the electrical network.')
     call stoptp
4018 ud1(i5 + 2) = j
4010 i5 = i5 + 5
  end do
5020 continue
  do i = 1, ktab
     xtcs(kxtcs + i) = zero
  end do
  if (.not. (niu .gt. 0)) goto 5030
  i5 = kud1
  do i = 1, niu
     i2 = kxtcs + nuk + i
     i1 = iuty(kiuty + i)
     k = int (ud1(i5 + 2))
     i3 = i1 - 89
     !     go to (4090, 4091, 4092, 4093, 4094, 4095), i3
     select case (i3)
     case (1)
        go to 4090

     case (2)
        go to 4091

     case (3)
        go to 4092

     case (4)
        go to 4093

     case (5)
        go to 4094

     case (6)
        go to 4095
     end select
     !4090 xtcs(i2) = emtpe(k)
     4090 xtcs(i2) = e(k)
     go to 4080
4091 if (nextsw(k) .eq. 87) xtcs(i2) = tclose(k)
     go to 4080
4092 xtcs(i2) = ykm(k)
     go to 4080
4093 if (nextsw(k).eq.87) xtcs(i2) = one
     go to 4080
     !4094 xtcs(i2) = emtpf(k)
     4094 xtcs(i2) = f(k)
     go to 4080
4095 xtcs(i2) = adelay(lswtch + k)
4080 i5 = i5 + 5
  end do
5030 continue
  etime = t
  estop = tmax + deltat / two
  estep = deltat
  to = 9000
  call elec
  !  lstat(35) = cptr   ! wsm + thl manual modification for bpa emtp
  !  k = isto(ishenv+7) ! wsm + thl manual modification for bpa emtp
  !  moncar(32) = iptr+isto(k)*isto(k+1)   ! wsm + thl manual modification for bpa emtp
  !  k = isto(ishenv+8)                    ! wsm + thl manual modification for bpa emtp
  !  lstat(48) = rptr-rbase+isto(k)*isto(k+1)   ! wsm + thl manual modification for bpa emtp
  if (.not. (etime + estep / two .gt. estop)) goto 5040
  close (unit08)
  close (bkfile)
5040 continue
  return
end subroutine ntacs2

!
! end of file ntacs2.f90
!
