!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: tread.f90
!

!
!     subroutine tread.
!
subroutine tread
!  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'                     ! wsm + thl manual modification for bpa emtp
  include 'tacsto.ftn'
  character(80) dbuff
  !  equivalence ( moncas(109), noutpr )   ! wsm + thl manual modification for bpa emtp
  character(6) chvbar
  !  equivalence ( texta6(17),  chvbar )   ! wsm + thl manual modification for bpa emtp
  !  character*6 chrcom, chrori            ! wsm + thl manual modification for bpa emtp
  !  equivalence ( texta6(23),  chrcom )   ! wsm + thl manual modification for bpa emtp
  chvbar = '|'                             ! wsm + thl manual modification for bpa emtp
!     9000 continue
  continue
  ctbl(38 : 38) = char(39)
  ctbl(49 : 49) = char(47)
  ctbl(65 : 65) = char(9)
  dptr = cptr
  i0 = 0
  i1 = 0
  i3 = 1
  i4 = 0
  i5 = 0
  i6 = 0
  i7 = 0
  i8 = 0
  i9 = 0
  !c chrori=chrcom        ! wsm + thl manual modification for bpa emtp
  !c chrcom(3:4)='!!'     ! wsm + thl manual modification for bpa emtp
3001 if (.not. (i0 .eq. 0 .and. i1 .eq. 0)) go to 3000
  continue
  call cimage
  write (dbuff, 7231) (abuff(j : j), j = 1, 80) ! wsm + thl manual modification for bpa emtp
7231 format (80a1)           ! wsm + thl manual modification for bpa emtp
  if (.not. (noutpr .eq. 0)) go to 5151
  !  write(unit06,1000) chvbar(1:1),dbuff    ! wsm + thl
  !     1000   format(51x,a1,a80)
  go to 5150
5151 continue
5150 continue
  if (.not. (dbuff(1 : 7) .eq. ctbl(143 : 149))) go to 5141
  i1 = 1
  i = 0
3051 if (.not. (i .lt. 7)) go to 3050
  i = i + 1
  csto(cptr + i) = dbuff(i : i)
  go to 3051
3050 continue
  cptr = cptr + 7
  go to 5140
5141 continue
5140 continue
  if (.not.(i1.eq.0)) go to 5001
  i10 = 80
  i = 80
3041 if (.not. (i .gt. 1 .and. dbuff(i : i) .eq. csto(64))) go to 3040
  i10 = i10 - 1
  i = i - 1
  go to 3041
3040 continue
  i = 0
3011 if (.not. (i .lt. i10)) go to 3010
  i = i + 1
  j = ichar (dbuff(i : i))
  if (.not. (j .ge. 97 .and. j .le. 122)) go to 5011
  dbuff(i : i) = char(j - 32)
  go to 5010
5011 continue
5010 continue
  go to 3011
3010 continue
  i6 = 0
  i = 0
3021 if (.not. (i .lt. i10 .and. i0 .eq. 0)) go to 3020
  i = i + 1
  i2 = 0
  char1 = dbuff(i : i)
  if (.not. (dbuff(i : i + 6) .eq. ctbl(143 : 149) .and. dbuff(i + 7 : i + 7) .eq. csto(64))) go to 5021
  i0 = 1
  j = 0
3031 if (.not. (j .lt. 7)) go to 3030
  j = j + 1
  k = i - 1 + j
  csto(cptr + j) = dbuff(k : k)
  go to 3031
3030 continue
  cptr = cptr + 7
  go to 5020
5021 if (.not. (i4 .eq. 1)) go to 5022
  if (.not. (dbuff(i : i + 14) .eq. ctbl(97 : 111))) go to 5111
  i = i + 14
  i4 = 0
  go to 5110
5111 continue
5110 continue
  go to 5020
5022 if (.not. (i5 .eq. 1)) go to 5023
  if (.not. (dbuff(i : i + 9) .eq. ctbl(75 : 84))) go to 5041
  i = i + 9
  i5 = 0
  go to 5040
5041 continue
5040 continue
  go to 5020
5023 if (.not. (i6 .eq. 1)) go to 5024
  if (.not. (char1 .eq. csto(67))) go to 5051
  i6 = 0
  go to 5050
5051 continue
5050 continue
  go to 5020
5024 if (.not. (i8 .eq. 1)) go to 5025
  i2 = 1
  i3 = 0
  i7 = 0
  if (.not. (char1 .eq. csto(38))) go to 5121
  i8 = 0
  go to 5120
5121 continue
5120 continue
  go to 5020
5025 if (.not. (i9 .eq. 1)) go to 5026
  i2 = 1
  i3 = 0
  i7 = 0
  if (.not. (char1 .eq. csto(39))) go to 5131
  i9 = 0
  go to 5130
5131 continue
5130 continue
  go to 5020
5026 if (.not. (char1 .eq. csto(38))) go to 5027
  i2 = 1
  i3 = 0
  i7 = 0
  i8 = 1
  go to 5020
5027 if (.not. (char1 .eq. csto(39))) go to 5028
  i2 = 1
  i3 = 0
  i7 = 0
  i9 = 1
  go to 5020
5028 if (.not. (dbuff(i : i + 11) .eq. ctbl(85 : 96))) go to 5029
  i = i + 11
  i4 = 1
  go to 5020
5029 if (.not. (dbuff(i : i + 6) .eq. ctbl(68 : 74))) go to 5030
  i = i + 6
  i5 = 1
  go to 5020
5030 if (.not. (char1 .eq. csto(67))) go to 5031
  i6=1
  go to 5020
5031 if (.not. (char1 .eq. csto(64) .or. char1 .eq. csto(65) .or. char1 .eq. csto(62) .or. char1 .eq. csto(63))) go to 5032
  if (.not. (char1 .eq. csto(62))) go to 5091
  i7 = 1
  go to 5090
5091 continue
5090 continue
  if (.not. (i3 .eq. 0)) go to 5061
  i3 = 1
  i2 = 1
  char1 = csto(64)
  go to 5060
5061 continue
5060 continue
  go to 5020
5032 continue
  if (.not. ((char1 .eq. csto(47) .or. char1 .eq. csto(46)) .and. i7 .gt. 0)) go to 5101
  csto(cptr + 1) = csto(62)
  cptr = cptr + 1
  go to 5100
5101 continue
5100 continue
  i3 = 0
  i7 = 0
  i2 = 1
5020 continue
  if (.not. (i2 .eq. 1)) go to 5071
  cptr = cptr + 1
  csto(cptr) = char1
  go to 5070
5071 continue
5070 continue
  go to 3021
3020 continue
  if (.not. (csto(cptr) .ne. csto(64))) go to 5081
  i3 = 1
  cptr = cptr + 1
  csto(cptr) = csto(64)
  go to 5080
5081 continue
5080 continue
  go to 5000
5001 continue
5000 continue
  go to 3001
3000 continue
  cptr = cptr + 1
  csto(cptr) = csto(64)
  chrcom = chrori          ! wsm + thl manual modification for bpa emtp
  return
end subroutine tread

!
!     end of file: tread.for
!
