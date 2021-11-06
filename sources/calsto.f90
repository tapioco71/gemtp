!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file calsto.f90
!

!
! subroutine calsto.
!

subroutine calsto
  use tacsto
  implicit none
  !  include 'tacsto.ftn'
  from = 0
  go to 9000
9500 if (from .eq. 0) return
  !  go to (9501, 9502), from-9500
  select case (from - 9500)
  case (1)
     go to 9501

  case (2)
     go to 9502
  end select
  stop 'Invalid "from" reference in "calsto".'
9000 continue
  stpflg = 0
  to = 0
  from = 0
  stpt = -rinf
  ishenv = 10 + strcnt + 1
  isto(ishenv + 51) = 0
  isto(ishenv + 52) = 0
  isto(ishenv + 60) = 0
  isto(ishenv + 55) = 0
  isto(ishenv + 56) = 0
  isto(ishenv + 57) = 0
  isto(ishenv + 15) = 0
  isto(ishenv + 61) = 0
  isto(ishenv + 0) = ndx0
  iptr = 10 + strcnt + ishcnt + iempty + intcnt
  split = 6.0d0
  pgsize = (stoflt - 40) / 64
  cnt4 = 2
  cnt5 = 4
  cnt7 = 0
  sptr = int((stoflt - 40) / (two * bratio))
  rptr = int((sptr * bratio) + 2)
  to = 9204
  call putil2
  mndx = 507
  to = 9206
  call putil2
  if (.not. (mflg .gt. 0)) go to 5061
  sptr = sptr - 1
  from = 9501
  go to 9001
9501 from = isto(sptr)
  sptr = sptr + 1
  go to 5060
5061 continue
5060 continue
  cnt2 = pgsize / intlen
  cnt3 = pgsize / fltlen
  tacs_a = (stoflt - 40) * bratio / (split + bratio)
  rsize = idint (tacs_a)
  isize = idint (tacs_a * split)
  csize = stocha
  ilen = isize - cnt4 * cnt2
  rlen = stoflt - 40 - cnt5 * cnt3
  clen = stocha
  rbase = stoflt - 40 - rsize
  if (.not. (ilen .lt. 2000)) go to 5001
  stpflg = 134
  stpi2 = cnt2
  stpi3 = isize
  stpi4 = ilen
  stpi5 = stoflt
  go to 5000
5001 if (.not. (rlen - rbase .lt. 200)) go to 5002
  stpflg = 135
  stpi2 = cnt3
  stpi3 = rsize
  stpi4 = rlen
  stpi5 = stoflt
  go to 5000
5002 if (.not. (clen .lt. 1800)) go to 5003
  stpflg = 136
  stpi2 = 0
  stpi3 = csize
  stpi4 = clen
  stpi5 = stocha
  go to 5000
5003 continue
5000 continue
  if (.not. (stpflg .gt. 0)) go to 5011
  stpi1 = pgsize
  stpr1 = split
  continue
  call errstp
  go to 5010
5011 continue
5010 continue
  rptr = rbase
  sptr = ilen + 1
  rsptr = rlen + 1
  ibkptr = 0
  rbkptr = 0
  do i = iptr + 1, isize
     isto(i) = inull
  end do
  do i = rptr + 1, stoflt - 40
     rsto(i) = rnull
  end do
  isto(ishenv + 7) = iptr + 1
  env = iptr + 1
  iptr = iptr + 6
  isto(env + 0) = cnt4
  isto(env + 1) = cnt2
  isto(env + 2) = 0
  if (.not. (cnt4 .gt. 0)) go to 5021
  isto(env + 3) = 1
  sptr = sptr - 1
  isto(sptr) = env
  cnt0 = ilen
  cnt6 = 0
  i = 0
3001 if (.not. (i .lt. cnt4)) go to 3000
  i = i + 1
  env = iptr + 1
  iptr = iptr + 5
  isto(env + 0) = cnt0
  cnt0 = cnt0 + cnt2
  isto(env + 1) = cnt6
  cnt6 = cnt6 + cnt2
  isto(env + 3) = 0
  isto(env + 4) = 0
  go to 3001
3000 continue
  env = isto(sptr)
  sptr = sptr + 1
  go to 5020
5021 continue
  isto(env + 3) = 0
5020 continue
  if (.not. (cnt4 .gt. 0)) go to 5051
  cnt1 = 0
  isto(env + 4) = iptr + 1
  sptr = sptr - 1
  isto(sptr) = env
  env = env + 1
  i = 0
3031 if (.not. (i .lt. cnt4)) go to 3030
  i = i + 1
  env = env + 5
  isto(env + 2) = iptr + 1
  sptr = sptr - 1
  isto(sptr) = env
  env = iptr + 1
  iptr = iptr + 3
  isto(env + 0) = iptr + 1
  cnt1 = cnt1 + 1
  isto(env + 1) = cnt1
  cnt7 = cnt7 + 1
  isto(env + 2) = cnt7
  env = isto(sptr)
  sptr = sptr + 1
  go to 3031
3030 continue
  isto(iptr - 2) = 0
  env = isto(sptr)
  sptr = sptr + 1
  isto(env + 5) = iptr - 2
  go to 5050
5051 continue
  isto(env + 4) = 0
  isto(env + 5) = 0
5050 continue
  isto(ishenv + 8) = iptr + 1
  env = iptr + 1
  iptr = iptr + 6
  isto(env + 0) = cnt5
  isto(env + 1) = cnt3
  isto(env + 2) = 0
  if (.not. (cnt5 .gt. 0)) go to 5031
  isto(env + 3) = 1
  sptr = sptr - 1
  isto(sptr) = env
  cnt0 = rlen
  cnt6 = 0
  i = 0
3011 if (.not. (i .lt. cnt5)) go to 3010
  i = i + 1
  env = iptr + 1
  iptr = iptr + 5
  isto(env + 0) = cnt0
  cnt0 = cnt0 + cnt3
  isto(env + 1) = cnt6
  cnt6 = cnt6 + cnt3
  isto(env + 3) = 0
  isto(env + 4) = 0
  go to 3011
3010 continue
  env = isto(sptr)
  sptr = sptr + 1
  go to 5030
5031 continue
  isto(env + 3) = 0
5030 continue
  if (.not. (cnt5 .gt. 0)) go to 5041
  cnt1 = 0
  isto(env + 4) = iptr + 1
  sptr = sptr - 1
  isto(sptr) = env
  env = env + 1
  i = 0
3021 if (.not. (i .lt. cnt5)) go to 3020
  i = i + 1
  env = env + 5
  isto(env + 2) = iptr + 1
  sptr = sptr - 1
  isto(sptr) = env
  env = iptr + 1
  iptr = iptr + 3
  isto(env + 0) = iptr + 1
  cnt1 = cnt1 + 1
  isto(env + 1) = cnt1
  cnt7 = cnt7 + 1
  isto(env + 2) = cnt7
  env = isto(sptr)
  sptr = sptr + 1
  go to 3021
3020 continue
  isto(iptr - 2) = 0
  env = isto(sptr)
  sptr = sptr + 1
  isto(env + 5) = iptr - 2
  go to 5040
5041 continue
  isto(env + 4) = 0
  isto(env + 5) = 0
5040 continue
  isto(ishenv + 9) = cnt7
  go to 9500
9001 continue
  sptr = sptr - 1
  isto(sptr) = lpflg
  lpflg = 1
3041 if (.not. (lpflg .gt. 0)) go to 3040
  mndx = 509
  to = 9206
  call putil2
  if (.not. (mflg .gt. 0)) go to 5071
  lpflg = 0
  go to 5070
5071 continue
  sptr = sptr - 1
  isto(sptr) = from
  from = 9502
  go to 9002
9502 from = isto(sptr)
  sptr = sptr + 1
5070 continue
  go to 3041
3040 continue
  lpflg = isto(sptr)
  sptr = sptr + 1
  go to 9500
9002 continue
  sptr = sptr - 1
  isto(sptr) = ndx0
  ndx0 = 0
  sptr = sptr - 1
  isto(sptr) = flg1
  flg1 = 0
  mndx = 499
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5081
  mndx = 495
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5091
  mndx = 97
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5101
  mndx = 497
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5111
  mndx = 105
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5121
  ndx0 = 1
  go to 5120
5121 continue
5120 continue
  go to 5110
5111 continue
5110 continue
  go to 5100
5101 continue
5100 continue
  if (.not. (ndx0 .ne. 1)) go to 5131
  isto(ishenv + 54) = 92
  continue
  call synstp
  go to 5130
5131 continue
5130 continue
  go to 5090
5091 continue
  mndx = 105
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5141
  ndx0 = 1
  go to 5140
5141 continue
  isto(ishenv + 54) = 92
  continue
  call synstp
5140 continue
5090 continue
  go to 5080
5081 continue
5080 continue
  if (.not. (ndx0 .eq. 1)) go to 5151
  to = 9214
  call putil2
  split = rsto(rptr)
  rptr = rptr - 1
  go to 5150
5151 continue
  mndx = 501
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5161
  mndx = 97
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5171
  mndx = 503
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5181
  mndx = 105
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5191
  ndx0 = 2
  to = 9214
  call putil2
  tacs_a = rsto(rptr)
  rptr = rptr - 1
  pgsize = idnint (tacs_a)
  go to 5190
5191 continue
5190 continue
  go to 5180
5181 continue
5180 continue
  go to 5170
5171 continue
5170 continue
  if (.not. (ndx0 .ne. 2)) go to 5201
  isto(ishenv + 54) = 93
  continue
  call synstp
  go to 5200
5201 continue
5200 continue
  go to 5160
5161 continue
  mndx = 495
  to = 9206
  call putil2
  if (.not. (mflg .gt. 0)) go to 5211
  mndx = 505
  to = 9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5221
  mndx = 105
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5231
  ndx0 = 3
  to = 9214
  call putil2
  tacs_a = rsto(rptr)
  rptr = rptr - 1
  cnt4 = idnint (tacs_a)
  go to 5230
5231 continue
5230 continue
  go to 5220
5221 continue
5220 continue
  if (.not. (ndx0 .ne. 3)) go to 5241
  isto(ishenv + 54) = 94
  continue
  call synstp
  go to 5240
5241 continue
5240 continue
  go to 5210
5211 continue
  mndx = 497
  to = 9206
  call putil2
  if (.not. (mflg .gt. 0)) go to 5251
  mndx = 505
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5261
  mndx = 105
  to = 9205
  call putil2
  if (.not. (mflg .gt. 0)) go to 5271
  ndx0 = 4
  to = 9214
  call putil2
  tacs_a = rsto(rptr)
  rptr = rptr - 1
  cnt5 = idnint (tacs_a)
  go to 5270
5271 continue
5270 continue
  go to 5260
5261 continue
5260 continue
  if (.not. (ndx0 .ne. 4)) go to 5281
  isto(ishenv + 54) = 95
  continue
  call synstp
  go to 5280
5281 continue
5280 continue
  go to 5250
5251 continue
  isto(ishenv + 54) = 96
  continue
  call synstp
5250 continue
5210 continue
5160 continue
5150 continue
  flg1 = isto(sptr)
  sptr = sptr + 1
  ndx0 = isto(sptr)
  sptr = sptr + 1
  go to 9500
end subroutine calsto

!
! end of file calsto.f90
!
