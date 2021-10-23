!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file build2.f90
!

subroutine build2
  use tacsto
  implicit none
  !  include 'tacsto.ftn'
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = from
  from = 0
  !  go to (9002, 9003, 9004, 9005, 9006, 9007, 9008, 9009, 9010, 9011, 9012 , 9013, 9014, 9015, 9016, 9017, 9018, 9019, 9020, 9021, 9022 , 9023, 9024, 9025, 9026, 9027), to-9001
  select case (to - 9001)
  case (1)
     go to 9002

  case (2)
     go to 9003

  case (3)
     go to 9004

  case (4)
     go to 9005

  case (5)
     go to 9006

  case (6)
     go to 9007

  case (7)
     go to 9008

  case (8)
     go to 9009

  case (9)
     go to 9010

  case (10)
     go to 9011

  case (11)
     go to 9012

  case (12)
     go to 9013

  case (13)
     go to 9014

  case (14)
     go to 9015

  case (15)
     go to 9016

  case (16)
     go to 9017

  case (17)
     go to 9018

  case (18)
     go to 9019

  case (19)
     go to 9020

  case (20)
     go to 9021

  case (21)
     go to 9022

  case (22)
     go to 9023

  case (23)
     go to 9024

  case (24)
     go to 9025

  case (25)
     go to 9026

  case (26)
     go to 9027
  end select
  stop 'invalid "to" reference in "build".'
9500 if (.not. (from .eq. 0)) go to 0001
  from = isto(sptr)
  sptr = sptr + 1
  return
  !0001 go to (9523, 9524, 9525, 9526, 9527, 9528, 9529, 9530, 9531, 9532, 9533, 9534, 9535, 9536), from-9522
0001 select case (from - 9522)
  case (1)
     go to 9523

  case (2)
     go to 9524

  case (3)
     go to 9525

  case (4)
     go to 9526

  case (5)
     go to 9527

  case (6)
     go to 9528

  case (7)
     go to 9529

  case (8)
     go to 9530

  case (9)
     go to 9531

  case (10)
     go to 9532

  case (11)
     go to 9533

  case (12)
     go to 9534

  case (13)
     go to 9535

  case (14)
     go to 9536
  end select
  !     0002
  stop 'Invalid "from" reference in "build".'
910 stpflg = 42
  stpi1 = iptr
  stpi2 = ilen - iptr
  continue
  call errstp
9002 continue
  sptr = sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = ptr1
  ptr1 = cptr
  isto(env + 7) = ptr1 + 1
  if (.not. (isto(ishenv + 10) .gt. 0)) go to 5121
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = env
  env = isto(ishenv + 10)
  i1=isto(env+6)
  i2=isto(env+7)-1
  env=isto(sptr)
  sptr=sptr+1
  i=0
3131 if (.not.(i.lt.i1)) go to 3130
  i=i+1
  csto(cptr+i)=csto(i2+i)
  go to 3131
3130 continue
  cptr=cptr+i1+1
  mndx=113
  j=base1+mndx
  k=base2 +isto(j)
  csto(cptr)=csto(k)
  go to 5120
5121 continue
5120 continue
  i3=isto(env+3)
  i4=isto(env+4)-1
  i=0
3141 if (.not.(i.lt.i3)) go to 3140
  i=i+1
  csto(cptr+i)=csto(i4+i)
  go to 3141
3140 continue
  cptr=cptr+i3
  i5=isto(env+5)
  if (.not.(i5.lt.iinf)) go to 5131
  cptr=cptr+1
  mndx=79
  j=base1+mndx
  k=base2 +isto(j)
  csto(cptr)=csto(k)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=i5
  to=9036
  call xpr2
  ndx=isto(sptr)
  sptr=sptr+1
  cptr=cptr+1
  mndx=81
  j=base1+mndx
  k=base2 +isto(j)
  csto(cptr)=csto(k)
  go to 5130
5131 continue
5130 continue
  isto(env+6)=cptr-ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9003 continue
  i6=isto(base6+ndx5)
  if (.not.(i6.eq.-iinf)) go to 5141
  i4=ndx4
  i6=ndx4
  isto(base4+ndx5)=ndx4
  isto(base6+ndx5)=ndx4
  ndx7=1
  go to 5140
5141 continue
  i4=isto(base4+ndx5)
  ndx7=i6-i4+1
5140 continue
  i3=ndx6-i6
  if (.not.(i3.lt.0)) go to 5681
  i3=0
  go to 5680
5681 continue
5680 continue
  i2=i4-ndx4
  if (.not.(i2.lt.0)) go to 5691
  i2=0
  go to 5690
5691 continue
5690 continue
  i1=i2+i3
  cnt1=cnt1+i1
  cnt2=cnt2+i1
  rptr=rptr+i1
  if (.not.(ndx4.lt.i4)) go to 5151
  i4=ndx4
  isto(base4+ndx5)=i4
  go to 5150
5151 continue
5150 continue
  if (.not.(ndx6.gt.i6)) go to 5161
  i6=ndx6
  isto(base6+ndx5)=i6
  go to 5160
5161 continue
5160 continue
  ndx8=i6-i4+1
  if (.not.(i1.gt.0 .and. ndx5.lt.ndx0)) go to 5171
  i=cnt1+1
  k=isto(base5+ndx5+1)
3151 if (.not.(i.gt.k)) go to 3150
  i=i-1
  a=rsto(base3+i)
  rsto(base3+i+i1)=a
  go to 3151
3150 continue
  i=ndx5
3161 if (.not.(i.lt.ndx0)) go to 3160
  i=i+1
  k=isto(base5+i)
  isto(base5+i)=k+i1
  go to 3161
3160 continue
  go to 5170
5171 continue
5170 continue
  if (.not.(i1.gt.0)) go to 5181
  i0=base3+isto(base5+ndx5)-1
  i=ndx7+1
3171 if (.not.(i.gt.1)) go to 3170
  i=i-1
  a=rsto(i0+i)
  rsto(i0+i+i2)=a
  go to 3171
3170 continue
  go to 5180
5181 continue
5180 continue
  if (.not.(i3.gt.0)) go to 5191
  i0=base3+isto(base5+ndx5)-i4+i6-i3
  i=0
3181 if (.not.(i.lt.i3)) go to 3180
  i=i+1
  rsto(i0+i)=rnull
  go to 3181
3180 continue
  go to 5190
5191 continue
5190 continue
  if (.not.(i2.gt.0)) go to 5201
  i0=base3+isto(base5+ndx5)-1
  i=0
3191 if (.not.(i.lt.i2)) go to 3190
  i=i+1
  rsto(i0+i)=rnull
  go to 3191
3190 continue
  go to 5200
5201 continue
5200 continue
  go to 9500
9004 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+6)
3201 if (.not.(env.gt.0)) go to 3200
  ndx5=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  stpl1=isto(env+0)
  stpc1=isto(env+1)
  ndx1=isto(env+2)
  ndx2=isto(env+3)
  to=9020
  call xpr1
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9523
  go to 9003
9523 from=isto(sptr)
  sptr=sptr+1
  xprndx=isto(env+5)
  to=9000
  call xpr1
  to=9000
  call xpr2
  env=isto(env+0)
  go to 3201
3200 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9005 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+8)
3211 if (.not.(env.gt.0)) go to 3210
  ndx5=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  stpl1=isto(env+0)
  stpc1=isto(env+1)
  ndx1=isto(env+2)
  ndx2=isto(env+3)
  to=9020
  call xpr1
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9524
  go to 9003
9524 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3211
3210 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9006 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+10)
3221 if (.not.(env.gt.0)) go to 3220
  ndx5=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  stpl1=isto(env+0)
  stpc1=isto(env+1)
  ndx1=isto(env+2)
  ndx2=isto(env+3)
  to=9020
  call xpr1
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9525
  go to 9003
9525 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3221
3220 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9007 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+12)
3231 if (.not.(env.gt.0)) go to 3230
  ndx5=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  stpl1=isto(env+0)
  stpc1=isto(env+1)
  ndx1=isto(env+2)
  ndx2=isto(env+3)
  to=9020
  call xpr1
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9526
  go to 9003
9526 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3231
3230 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9008 continue
  stop 'no buildftndata routine.'
9009 continue
  stop 'no buildftninput routine.'
9010 continue
  stop 'no buildftnvar routine.'
9011 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+10)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+11)
3241 if (.not.(env.gt.0)) go to 3240
  if (.not.(isto(env+1).eq.7)) go to 5211
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3251 if (.not.(env.gt.0)) go to 3250
  xprndx=isto(env+2)
  to=9000
  call xpr1
  ndx3=xprcnt
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+1)
  ndx5=isto(env+2)
  ndx1=isto(env+8)
  ndx2=isto(env+9)
  env=isto(sptr)
  sptr=sptr+1
  to=9020
  call xpr1
  to=9037
  call xpr2
  xprcnt=ndx3
  to=9000
  call xpr2
  env=isto(env+0)
  go to 3251
3250 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5210
5211 continue
5210 continue
  env=isto(env+0)
  go to 3241
3240 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  go to 9500
9012 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+8)
3261 if (.not.(env.gt.0)) go to 3260
  ndx5=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  stpl1=isto(env+0)
  stpc1=isto(env+1)
  ndx1=isto(env+2)
  ndx2=isto(env+3)
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  xprndx=isto(env+5)
  if (.not.(xprndx.gt.0)) go to 5221
  to=9000
  call xpr1
  to=9001
  call xpr2
  go to 5220
5221 continue
5220 continue
  env=isto(env+0)
  go to 3261
3260 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9013 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+48)
3271 if (.not.(env.gt.0)) go to 3270
  if (.not.(isto(env+1).eq.12)) go to 5231
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3281 if (.not.(env.gt.0)) go to 3280
  xprndx=isto(env+2)
  to=9000
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  cnt4 = idnint (a)
  if (.not. (cnt4 .lt. 0 .or. cnt4 .gt. 4)) go to 5241
  stpflg=113
  stpi1=cnt4
  continue
  call errstp
  go to 5240
5241 continue
5240 continue
  k=isto(env+0)
  if (.not.(k.gt.0)) go to 5251
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=k
3291 if (.not.(env.gt.0)) go to 3290
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+1)
  ndx5=isto(env+2)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=isto(env+9)
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9527
  go to 9014
9527 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3291
3290 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 5250
5251 continue
  cnt5=cnt4
5250 continue
  env=isto(env+1)
  go to 3281
3280 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5230
5231 continue
5230 continue
  env=isto(env+0)
  go to 3271
3270 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9014 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  i1=isto(env+29)+4*(ndx5-cnt6)
  i2=isto(i1+3)
  i4=isto(base4+ndx5)
  i3=i2+5*(ndx4-i4-1)
  k=ndx6-ndx4+1
  i=0
3301 if (.not.(i.lt.k)) go to 3300
  i=i+1
  i3=i3+5
  if (.not.(cnt4.gt.isto(i3+1))) go to 5261
  isto(i3+1)=cnt4
  go to 5260
5261 continue
5260 continue
  go to 3301
3300 continue
  if (.not.(cnt4.gt.cnt8)) go to 5271
  cnt8=cnt4
  go to 5270
5271 continue
5270 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9015 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+10)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+11)
3311 if (.not.(env.gt.0)) go to 3310
  if (.not.(isto(env+1).eq.10)) go to 5281
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3321 if (.not.(env.gt.0)) go to 3320
  xprndx=isto(env+2)
  to=9000
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  cnt4 = idnint (a)
  if (.not. (cnt4 .lt. 0 .or. cnt4 .gt. 4)) go to 5291
  stpflg=114
  stpi1=cnt4
  continue
  call errstp
  go to 5290
5291 continue
5290 continue
  k=isto(env+1)
  if (.not.(k.gt.0)) go to 5301
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=k
3331 if (.not.(env.gt.0)) go to 3330
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+1)
  ndx5=isto(env+2)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=isto(env+9)
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9528
  go to 9014
9528 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3331
3330 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 5300
5301 continue
  cnt5=cnt4
5300 continue
  env=isto(env+0)
  go to 3321
3320 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5280
5281 continue
5280 continue
  env=isto(env+0)
  go to 3311
3310 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9016 continue
  i1=isto(env+32)-5
  k=isto(env+30)
  i=0
3341 if (.not.(i.lt.k)) go to 3340
  i=i+1
  i1=i1+5
  if (.not.(isto(i1+1).lt.0)) go to 5311
  isto(i1+1)=cnt5
  if (.not.(cnt5.gt.cnt8)) go to 5671
  cnt8=cnt5
  go to 5670
5671 continue
5670 continue
  go to 5310
5311 continue
5310 continue
  go to 3341
3340 continue
  go to 9500
9017 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+35)
3351 if (.not.(env.gt.0)) go to 3350
  k = isto(env + 1)
  if (.not. (k .eq. 379 .or. k .eq. 385 .or. k.eq.389 .or. k .eq. 391 )) go to 5321
  xprndx=isto(env+7)
  if (.not.(xprndx.gt.0)) go to 5331
  to=9000
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  cnt4 = idnint (a)
  cnt3=2*cnt4
  if (.not.(cnt3.eq.0)) go to 5341
  cnt3=1
  go to 5340
5341 continue
5340 continue
  if (.not. (cnt4 .lt. 0 .or. cnt4 .gt. 4)) go to 5351
  stpflg=112
  k=isto(env+1)
  stpl1=isto(base1+k+1)
  stpc1=base2+isto(base1+k)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  stpl2=isto(env+7)
  stpc2=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  stpi1=cnt4
  continue
  call errstp
  go to 5350
5351 continue
5350 continue
  go to 5330
5331 continue
  cnt4=1
  cnt3=2
5330 continue
  go to 5320
5321 if (.not. (k .eq. 387 .or. k .eq. 393)) go to 5322
  cnt3=1
  cnt4=-1
  go to 5320
5322 if (.not.(k.eq.395)) go to 5323
  cnt3=2
  cnt4=-1
  go to 5320
5323 continue
  cnt3=0
5320 continue
  if (.not.(cnt3.gt.0)) go to 5361
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  ndx5=isto(env+1)
  stpl1=isto(env+7)
  stpc1=isto(env+8)
  ndx1=isto(env+9)
  ndx2=ndx1
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9529
  go to 9018
9529 from=isto(sptr)
  sptr=sptr+1
  go to 5360
5361 continue
5360 continue
  env=isto(env+0)
  go to 3351
3350 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9018 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  i4=isto(base4+ndx5)
  if (.not.(ndx5.ge.cnt7)) go to 5371
  i1=isto(env+36)+3*(ndx5-cnt7)
  i2=isto(i1)
  i3=i2+7*(ndx4-i4)
  go to 5370
5371 continue
  i1=isto(env+29)+4*(ndx5-cnt6)
  i2=isto(i1)
  i3=i2+7*(ndx4-i4)
5370 continue
  if (.not.(cnt3.gt.isto(i3+5))) go to 5381
  isto(i3+5)=cnt3
  go to 5380
5381 continue
5380 continue
  if (.not.(cnt4.gt.isto(i3+1))) go to 5391
  isto(i3+1)=cnt4
  go to 5390
5391 continue
5390 continue
  if (.not.(cnt3.gt.cnt9)) go to 5401
  cnt9=cnt3
  go to 5400
5401 continue
5400 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9019 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+13)
3361 if (.not.(env.gt.0)) go to 3360
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx5=isto(env+2)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=ndx1
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  cnt3=isto(env+1)
  cnt4=-1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9530
  go to 9018
9530 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3361
3360 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9020 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+14)
3371 if (.not.(env.gt.0)) go to 3370
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx5=isto(env+2)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=ndx1
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  cnt3=isto(env+2)
  cnt4=-1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9531
  go to 9018
9531 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3371
3370 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9021 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+15)
3381 if (.not.(env.gt.0)) go to 3380
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+5)
  ndx5=isto(env+2)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=ndx1
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  cnt3=isto(env+1)
  cnt4=-1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9532
  go to 9018
9532 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx5=isto(env+2)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=ndx1
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  cnt3=isto(env+2)
  cnt4=-1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9533
  go to 9018
9533 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3381
3380 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9022 continue
  i1=isto(env+29)
  i2=isto(env+36)
  i=0
3391 if (.not.(i.lt.ndx10)) go to 3390
  i=i+1
  ndx9=ndx9+3
  ndx5=isto(ndx9+2)
  ndx4=isto(base4+ndx5)
  ndx6=isto(base6+ndx5)
  if (.not.(ndx5.ge.cnt7)) go to 5411
  i3=i2+3*(ndx5-cnt7)
  go to 5410
5411 continue
  i3=i1+4*(ndx5-cnt6)
5410 continue
  i4=isto(i3)-7
  isto(i3+2)=iptr+1
  k=ndx6-ndx4+1
  j=0
3401 if (.not.(j.lt.k)) go to 3400
  j=j+1
  i4=i4+7
  if (.not.(isto(i4+5).lt.1)) go to 5421
  isto(i4+5)=1
  go to 5420
5421 continue
5420 continue
  isto(iptr+1)=rptr+1
  isto(iptr+2)=0
  isto(iptr+3)=1
  isto(iptr+4)=0
  iptr=iptr+4
  rptr=rptr+2
  go to 3401
3400 continue
  go to 3391
3390 continue
  go to 9500
9023 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  i0=isto(env+43)
  i1=isto(env+42)
  env=isto(sptr)
  sptr=sptr+1
  i2=isto(env+28)
  i9=isto(env+29)-4
  i3=1
  i=0
3411 if (.not.(i.lt.i2)) go to 3410
  i=i+1
  i5=cnt6+i-1
  i9=i9+4
  i7=i0-3
  j=0
  k=0
3421 if (.not.(j.lt.i1 .and. k.eq.0)) go to 3420
  j=j+1
  i7=i7+3
  if (.not.(isto(i7+2).eq.i5)) go to 5431
  k=1
  isto(i9+1)=i3
  i4=isto(base4+i5)
  i6=isto(base6+i5)
  i8=i6-i4+1
  i3=i3+i8
  go to 5430
5431 continue
5430 continue
  go to 3421
3420 continue
  if (.not.(k.eq.0)) go to 5441
  isto(i9+1)=0
  go to 5440
5441 continue
5440 continue
  go to 3411
3410 continue
  i2=isto(env+35)
  i9=isto(env+36)-3
  i=0
3431 if (.not.(i.lt.i2)) go to 3430
  i=i+1
  i5=cnt7+i-1
  i9=i9+3
  i7=i0-3
  j=0
  k=0
3441 if (.not.(j.lt.i1 .and. k.eq.0)) go to 3440
  j=j+1
  i7=i7+3
  if (.not.(isto(i7+2).eq.i5)) go to 5451
  k=1
  isto(i9+1)=i3
  i4=isto(base4+i5)
  i6=isto(base6+i5)
  i8=i6-i4+1
  i3=i3+i8
  go to 5450
5451 continue
5450 continue
  go to 3441
3440 continue
  if (.not.(k.eq.0)) go to 5461
  isto(i9+1)=0
  go to 5460
5461 continue
5460 continue
  go to 3431
3430 continue
  isto(env+13)=i3-1
  go to 9500
9024 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+10)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+11)
3451 if (.not.(env.gt.0)) go to 3450
  if (.not.(isto(env+1).eq.8)) go to 5471
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3461 if (.not.(env.gt.0)) go to 3460
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx5=isto(env+2)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=isto(env+9)
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=isto(env+5)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=flg1
  flg1=0
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=flg2
  flg2=isto(env+3)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9534
  go to 9026
9534 from=isto(sptr)
  sptr=sptr+1
  flg2=isto(sptr)
  sptr=sptr+1
  flg1=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  env=isto(env+2)
  go to 3461
3460 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5470
5471 continue
5470 continue
  env=isto(env+0)
  go to 3451
3450 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9025 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+48)
3471 if (.not.(env.gt.0)) go to 3470
  if (.not.(isto(env+1).eq.10)) go to 5481
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3481 if (.not.(env.gt.0)) go to 3480
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx5=isto(env+2)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=isto(env+9)
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=isto(env+5)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=flg1
  flg1=1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=flg2
  flg2=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9535
  go to 9026
9535 from=isto(sptr)
  sptr=sptr+1
  flg2=isto(sptr)
  sptr=sptr+1
  flg1=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  env=isto(env+1)
  go to 3481
3480 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5480
5481 continue
5480 continue
  env=isto(env+0)
  go to 3471
3470 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9026 continue
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = env
  env = useenv
  i4 = isto(base4 + ndx5)
  k = ndx6 - ndx4 + 1
  if (.not. (ndx5 .ge. cnt7)) go to 5491
  i1 = isto(env + 36) + 3 * (ndx5 - cnt7)
  go to 5490
5491 continue
  i1 = isto(env + 29) + 4 * (ndx5 - cnt6)
5490 continue
  if (.not. (flg2 .gt. 0)) go to 5501
  i9 = isto(i1 + 2)
  i10 = i9 + 4 * (ndx4 - i4 - 1)
  i = 0
3491 if (.not. (i .lt. k)) go to 3490
  i = i + 1
  i10 = i10 + 4
  if (.not. (flg1 .eq. 0 .or. isto(i10 + 1) .eq. 0)) go to 5511
  isto(i10 + 1) = ptr1
  isto(i10 + 3) = i
  go to 5510
5511 continue
5510 continue
  go to 3491
3490 continue
  go to 5500
5501 continue
  i2 = isto(i1)
  i3 = i2 + 7 * (ndx4 - i4 - 1)
  i = 0
3501 if (.not. (i .lt. k)) go to 3500
  i = i + 1
  i3 = i3 + 7
  if (.not. (flg1 .eq. 0 .or. isto(i3 + 3) .eq. 0)) go to 5521
  isto(i3 + 3) = ptr1
  isto(i3 + 6) = i
  go to 5520
5521 continue
5520 continue
  go to 3501
3500 continue
  if (.not. (ndx5 .lt. cnt7)) go to 5531
  i7 = isto(i1 + 3)
  i8 = i7 + 5 * (ndx4 - i4 - 1)
  i = 0
3511 if (.not. (i .lt. k)) go to 3510
  i = i + 1
  i8 = i8 + 5
  if (.not. (flg1 .eq. 0 .or. isto(i8 + 3) .eq. 0)) go to 5541
  isto(i8 + 3) = ptr1
  isto(i8 + 4) = i
  go to 5540
5541 continue
5540 continue
  go to 3511
3510 continue
  go to 5530
5531 continue
5530 continue
5500 continue
  env = isto(sptr)
  sptr = sptr + 1
  go to 9500
9027 continue
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = env
  env = datenv
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = env
  env = isto(env + 22)
3521 if (.not. (env .gt. 0)) go to 3520
  if (.not. (isto(env + 2) .eq. 0)) go to 5701
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = env
  env = isto(env + 4)
  ndx5 = isto(env + 2)
  stpl1 = isto(env + 6)
  stpc1 = isto(env + 7)
  ndx1 = isto(env + 8)
  ndx2 = ndx1
  to = 9020
  call xpr1
  to = 9037
  call xpr2
  env = isto(sptr)
  sptr = sptr + 1
  cnt3 = 1
  cnt4 = -1
  ndx4 = ndx4 - 1
  k = ndx6 - ndx4
  i = 0
3531 if (.not. (i .lt. k)) go to 3530
  i = i + 1
  ndx4 = ndx4 + 1
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = from
  from = 9536
  go to 9018
9536 from = isto(sptr)
  sptr = sptr + 1
  go to 3531
3530 continue
  go to 5700
5701 continue
5700 continue
  env = isto(env+0)
  go to 3521
3520 continue
  env = isto(sptr)
  sptr = sptr + 1
  env = isto(sptr)
  sptr = sptr + 1
  go to 9500
end subroutine build2

!
! end of file build2.f90
!
