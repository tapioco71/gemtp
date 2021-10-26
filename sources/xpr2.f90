!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file xpr2.f90
!

!
! subroutine xpr2.
!

subroutine xpr2
  use tacsto
  use random
  implicit none
  !
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = from
  from = 0
  go to (9000, 9001, 9002, 9003, 9004, 9005, 9006, 9007, 9008, 9009, 9010, 9011, 9012, 9013, 9014, 9015, 9016, 9017, 9018, 9019, 9020, 9021, 9022, 9023, 9024, 9025, 9026, 9027, 9028, 9029, 9030, 9031, 9032, 9033, 9034, 9035, 9036, 9037), to-8999
  stop 'invalid "to" reference in "xpr2".'
9500 if(.not.(from.eq.0)) go to 0001
  from = isto(sptr)
  sptr = sptr + 1
  return
 0001 go to (9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, 9516, 9517, 9518, 9519, 9520, 9521, 9522, 9523, 9524, 9525, 9526, 9527, 9528, 9529), from-9500
  stop 'invalid "from" reference in "xpr2".'
910 stpflg = 42
  stpi1 = iptr
  stpi2 = ilen - iptr
  continue
  call errstp
930 stpflg = 44
  stpi1 = rptr
  stpi2 = rlen - rptr
  continue
  call errstp
9000 continue
  rptr = rptr - xprcnt
  k=ndx6-ndx4+1
  if(.not.(k.lt.1)) go to 5001
  stpflg=29
  go to 5000
5001 if(.not.(k.ne.xprcnt .and. xprcnt.gt.1)) go to 5002
  stpflg=30
  go to 5000
5002 continue
5000 continue
  if(.not.(stpflg.gt.0)) go to 5011
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  i0=isto(env+1) +2*(ndx5-1)
  env=isto(sptr)
  sptr=sptr+1
  stpl1=isto(i0)
  stpc1=isto(i0+1)
  stpi1=ndx4
  stpi2=ndx6
  stpi3=xprcnt
  continue
  call errstp
  go to 5010
5011 continue
5010 continue
  i0=base3 +isto(base5+ndx5)-isto(base4+ndx5) +ndx4 -1
  i=0
  if(.not.(xprcnt.eq.1)) go to 5021
  a=rsto(rptr+1)
3001 if(.not.(i.lt.k)) go to 3000
  i=i+1
  rsto(i0+i)=a
  go to 3001
3000 continue
  go to 5020
5021 continue
3011 if(.not.(i.lt.xprcnt)) go to 3010
  i=i+1
  rsto(i0+i)=rsto(rptr+i)
  go to 3011
3010 continue
5020 continue
  go to 9500
9001 continue
  rptr=rptr-xprcnt
  k=ndx6-ndx4+1
  if(.not.(k.lt.1 .or. (k.ne.xprcnt .and. xprcnt.gt.1))) go to 5031
  stpflg=103
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  i0=isto(env+1) +2*(ndx5-1)
  env=isto(sptr)
  sptr=sptr+1
  stpl1=isto(i0)
  stpc1=isto(i0+1)
  stpi1=ndx4
  stpi2=ndx6
  stpi3=xprcnt
  continue
  call errstp
  go to 5030
5031 continue
5030 continue
  i0=base3 +isto(base5+ndx5)-isto(base4+ndx5) +ndx4 -1
  i=0
  if(.not.(xprcnt.eq.1)) go to 5041
  a=rsto(rptr+1)
3021 if(.not.(i.lt.k)) go to 3020
  i=i+1
  if(.not.(rsto(i0+i).eq.rnull)) go to 5051
  rsto(i0+i)=a
  go to 5050
5051 continue
5050 continue
  go to 3021
3020 continue
  go to 5040
5041 continue
3031 if(.not.(i.lt.xprcnt)) go to 3030
  i=i+1
  if(.not.(rsto(i0+i).eq.rnull)) go to 5061
  rsto(i0+i)=rsto(rptr+i)
  go to 5060
5061 continue
5060 continue
  go to 3031
3030 continue
5040 continue
  go to 9500
9002 continue
  stpi1=ndx4
  stpi2=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base6
  if(.not.(isto(ishenv+17).ne.datenv)) go to 5071
  base3=base13
  base5=base15
  base4=base14
  base6=base16
  go to 5070
5071 continue
5070 continue
  j=isto(base4+ndx5)
  k=isto(base6+ndx5)
  stpi3=j
  stpi4=k
  if(.not.(k.eq.-iinf)) go to 5081
  stpflg=84
  go to 5080
5081 if(.not.(ndx6.lt.ndx4)) go to 5082
  stpflg=85
  go to 5080
5082 if(.not.(ndx4.lt.j .or. ndx4.gt.k)) go to 5083
  stpflg=86
  go to 5080
5083 if(.not.(ndx6.lt.j .or. ndx6.gt.k)) go to 5084
  stpflg=87
  go to 5080
5084 continue
  k=ndx6 -ndx4 +1
  i0=base3 +isto(base5+ndx5) -j +ndx4 -1
  i=0
3041 if(.not.(i.lt.k .and. stpflg.eq.0)) go to 3040
  i=i+1
  a=rsto(i0+i)
  if(.not.(a.eq.rnull)) go to 5091
  stpflg=88
  stpi5=i0+i
  go to 5090
5091 continue
5090 continue
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=a
  go to 3041
3040 continue
5080 continue
  base6=isto(sptr)
  sptr=sptr+1
  base4=isto(sptr)
  sptr=sptr+1
  base5=isto(sptr)
  sptr=sptr+1
  base3=isto(sptr)
  sptr=sptr+1
  go to 9500
9003 continue
  if(.not.(cnt0.eq.0)) go to 5101
  cnt0=cnt1
  go to 5100
5101 continue
  rptr=rptr-cnt1
  if(.not.(cnt0.ne.cnt1)) go to 5111
  stpflg=31
  stpi1=cnt0
  stpi2=cnt1
  stpi3=xprknd
  stpi4=xprsub
  continue
  call errstp
  go to 5110
5111 continue
  i=0
3051 if(.not.(i.lt.cnt0)) go to 3050
  i=i+1
  if(.not.(rsto(ptr0+i).le.rmargn)) go to 5121
  rsto(ptr0+i)=rsto(rptr+i)
  go to 5120
5121 continue
5120 continue
  go to 3051
3050 continue
5110 continue
5100 continue
  go to 9500
9004 continue
  if(.not.(cnt1.eq.0)) go to 5131
  cnt1=cnt2
  go to 5130
5131 continue
  rptr=rptr-cnt2
  if(.not.(cnt1.ne.cnt2)) go to 5141
  stpflg=32
  stpi1=cnt1
  stpi2=cnt2
  stpi3=xprknd
  stpi4=xprsub
  continue
  call errstp
  go to 5140
5141 continue
  i=0
3061 if(.not.(i.lt.cnt1)) go to 3060
  i=i+1
  if(.not.(rsto(ptr1+i).gt.rmargn)) go to 5151
  rsto(ptr1+i)=rsto(rptr+i)
  go to 5150
5151 continue
5150 continue
  go to 3061
3060 continue
5140 continue
5130 continue
  go to 9500
9005 continue
  p = dmax1 (rmargn, dabs (rmargn * b))
  if (.not. ((ndx .eq. 3 .and. a .lt. b - p) .or. (ndx .eq. 4 .and. a .le. b + p) .or. (ndx .eq. 5 .and. a .gt. b + p) .or. (ndx .eq. 6 .and. a .ge. b - p) .or. (ndx .eq. 1 .and. a .ge. b - p .and. a .le. b + p) .or. (ndx .eq. 2 .and. (a .lt. b - p .or. a .gt. b + p)))) go to 5161
  c = one
  go to 5160
5161 continue
5160 continue
  go to 9500
9006 continue
  if(.not.(cnt3.eq.0)) go to 5231
  cnt3=cnt4
  go to 5230
5231 continue
  rptr=rptr-cnt4
  if(.not.(cnt3.ne.cnt4)) go to 5241
  stpflg=35
  stpi1=cnt3
  stpi2=cnt4
  stpi3=xprknd
  stpi4=xprsub
  continue
  call errstp
  go to 5240
5241 continue
  i=0
3071 if(.not.(i.lt.cnt3)) go to 3070
  i=i+1
  rsto(ptr3+i)=rsto(ptr3+i) +rsto(rptr+i)
  go to 3071
3070 continue
5240 continue
5230 continue
  go to 9500
9007 continue
  if(.not.(cnt4.eq.0)) go to 5251
  cnt4=cnt5
  go to 5250
5251 continue
  rptr=rptr-cnt5
  i=0
  if(.not.(cnt4.eq.1)) go to 5261
  a=rsto(rptr)
3081 if(.not.(i.lt.cnt5)) go to 3080
  i=i+1
  rsto(ptr4+i)=rsto(rptr+i) *a
  go to 3081
3080 continue
  cnt4=cnt5
  go to 5260
5261 if(.not.(cnt5.eq.1)) go to 5262
  a=rsto(rptr+1)
3091 if(.not.(i.lt.cnt4)) go to 3090
  i=i+1
  rsto(ptr4+i)=rsto(ptr4+i) *a
  go to 3091
3090 continue
  go to 5260
5262 continue
  stpflg=79
  stpi1=cnt4
  stpi2=cnt5
  continue
  call errstp
5260 continue
  rptr=ptr4+cnt4
5250 continue
  go to 9500
9008 continue
  if (.not. (dabs(a) .lt. rmargn .or. dabs(b) .lt. divzro)) go to 5271
  c=zero
  go to 5270
5271 continue
  c = a - dint (a / b) * b
5270 continue
  go to 9500
9009 continue
  if (.not. (dabs (a) .lt. rmargn)) go to 5281
  c=zero
  go to 5280
5281 if (.not. (dabs(b) .lt. rmargn)) go to 5282
  c=one
  go to 5280
5282 if(.not.(a.lt.zero)) go to 5283
  d = dnint (b)
  e = dabs (d - b)
  if(.not.(e.gt.rmargn)) go to 5871
  stpflg=132
  stpr1=a
  stpr2=b
  continue
  call errstp
  go to 5870
5871 continue
  k = idnint (b)
  c=a**k
5870 continue
  go to 5280
5283 continue
  c=a**b
5280 continue
  go to 9500
9010 continue
  a=rsto(rptr)
  if (.not. (dabs (a) .lt. divzro)) go to 5291
  stpflg=82
  stpr1=a
  continue
  call errstp
  go to 5290
5291 continue
  rsto(rptr)=one /a
5290 continue
  go to 9500
9011 continue
  a=rsto(rptr)
  if (.not. (dabs (a) .lt. divzro)) go to 5301
  flg=1
  go to 5300
5301 continue
  flg=0
  rsto(rptr)=one /a
5300 continue
  go to 9500
9012 continue
  stpflg=121
  continue
  call errstp
  go to 9500
9013 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  r6=rsto(rptr)
  rptr=rptr-1
  p = dmax1 (rmargn, dabs (rmargn * r4))
  if(.not.(r6.lt.r4-p)) go to 5311
  ndx3=-1
  go to 5310
5311 if(.not.(r6.gt.r4+p)) go to 5312
  ndx3=1
  go to 5310
5312 continue
  ndx3=0
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r3
5310 continue
  if(.not.(ndx3.ne.0)) go to 6311
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  ndx4=isto(env+14)
  env=isto(sptr)
  sptr=sptr+1
  flg=0
3101 if(.not.(ndx4.gt.0 .and. flg.eq.0)) go to 3100
  ndx=0
  ndx6=ndx4+2
  ptr0=iptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9501
  go to 9032
9501 from=isto(sptr)
  sptr=sptr+1
  ndx1=isto(ptr0+1)
  ndx2=isto(ptr0+2)
  ndx4=isto(ptr0+3)
  ndx=0
  ndx6=ndx4+ndx12
  ptr0=rptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9502
  go to 9033
9502 from=isto(sptr)
  sptr=sptr+1
  a=rsto(ptr0+1)
  b=rsto(ptr0+1 +ndx12)
  p = dmax1 (rmargn, dabs (rmargn * r6))
  if (.not. ((ndx3 .lt. 0 .and. b .le. r6 + p) .or. (ndx3 .gt. 0 .and. b .ge. r6 - p))) go to 5321
  flg=1
  go to 5320
5321 continue
  ndx4=ndx2
5320 continue
  go to 3101
3100 continue
  p = dmax1 (rmargn, dabs (rmargn * r6))
  if (.not. (flg .eq. 0)) go to 5331
  r5 = rnull
  rptr = rptr + 1
  if (rptr .eq. rsptr) go to 930
  rsto(rptr) = r5
  go to 5330
5331 if (.not. ((ndx3 .lt. 0 .and. b .ge. r6 - p) .or. (ndx3 .gt. 0 .and. b .le. r6 + p))) go to 5332
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=a
  go to 5330
5332 continue
  ndx4=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  ptr0=rptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=1
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9019
9503 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9020
9504 from=isto(sptr)
  sptr=sptr+1
  rptr=ptr0
  ndx0=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
5330 continue
  go to 6310
6311 continue
6310 continue
  ptr0=isto(sptr)
  sptr=sptr+1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9014 continue
  r5=rsto(rptr)
  rptr=rptr-1
  p = dmax1 (rmargn, dabs (rmargn * r3))
  if (.not. (r5 .lt. r3 - p)) go to 5341
  stpflg=22
  stpr1=r5
  stpr2=r3
  continue
  call errstp
  go to 5340
5341 if(.not.(r5.le.r3+p .or. ipol.eq.0)) go to 5342
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r4
  go to 5340
5342 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  ptr0=rptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=0
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9018
9505 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9020
9506 from=isto(sptr)
  sptr=sptr+1
  if(.not.((r2-r4)*(rsto(ptr0)-r4).gt.zero)) go to 6131
  rsto(ptr0)=r4
  go to 6130
6131 continue
6130 continue
  rptr=ptr0
  ndx0=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
5340 continue
  go to 9500
9015 continue
  r6=rsto(rptr)
  rptr=rptr-1
  p = dmax1 (rmargn, dabs (rmargn * r4))
  if (.not. (dabs (r6 - r4) .le. p .or. dabs (r2 - r4) .le. p)) go to 6141
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r3
  go to 6140
6141 continue
  if(.not.(ipol.eq.0)) go to 5351
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=rnull
  go to 5350
5351 if(.not.((r2-r4)*(r6-r4).gt.zero)) go to 5352
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r3
  go to 5350
5352 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  ptr0=rptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=1
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9507
  go to 9018
9507 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9508
  go to 9020
9508 from=isto(sptr)
  sptr=sptr+1
  p = dmax1 (rmargn, dabs (rmargn * r3))
  if (.not. (rsto(ptr0) .le. r3 + p)) go to 5931
  rsto(ptr0)=r3
  go to 5930
5931 if(.not.(rsto(ptr0).gt.r3+rsto(base3+6))) go to 5932
  rsto(ptr0)=r3+rsto(base3+6)
  go to 5930
5932 continue
5930 continue
  rptr=ptr0
  ndx0=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
5350 continue
6140 continue
  go to 9500
9016 continue
  r5=rsto(rptr)
  rptr=rptr-1
  p = dmax1 (rmargn, dabs (rmargn * r5))
  if (.not. (r5 .lt. r1 - p .or. r5 .gt. r3 + p)) go to 5361
  stpflg=23
  stpr1=r1
  stpr2=r3
  stpr3=r5
  continue
  call errstp
  go to 5360
5361 if(.not.(r5.le.r1+p .or. ipol.eq.0)) go to 5362
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r2
  go to 5360
5362 if(.not.(r5.ge.r3-p)) go to 5363
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r4
  go to 5360
5363 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  ptr0=rptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=0
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9509
  go to 9018
9509 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9510
  go to 9020
9510 from=isto(sptr)
  sptr=sptr+1
  if(.not.((r2-r4)*(rsto(ptr0)-r4).lt.zero)) go to 6151
  rsto(ptr0)=r4
  go to 6150
6151 if(.not.((r2-r4)*(r2-rsto(ptr0)).lt.zero)) go to 6152
  rsto(ptr0)=r2
  go to 6150
6152 continue
6150 continue
  rptr=ptr0
  ndx0=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
5360 continue
  go to 9500
9017 continue
  r6=rsto(rptr)
  rptr=rptr-1
  p = dmax1 (rmargn, dabs(rmargn * r4))
  if (.not. (dabs (r6 - r4) .le. p .or. dabs (r2 - r4) .le. p)) go to 5951
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r3
  go to 5950
5951 if (.not. (dabs (r6 - r2) .le. dmax1 (rmargn, dabs (rmargn * r2)))) go to 5952
  if (.not. (ipol .eq. 0)) go to 6161
  a = r3 - dmax1 (rmargn, dabs (rmargn * r3))
  rptr = rptr + 1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=a
  go to 6160
6161 continue
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r1
6160 continue
  go to 5950
5952 continue
  if(.not.(ipol.eq.0)) go to 5371
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=rnull
  go to 5370
5371 if(.not.((r2-r4)*(r6-r4).lt.zero)) go to 5372
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r3
  go to 5370
5372 if(.not.((r2-r4)*(r2-r6).lt.zero)) go to 5373
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r1
  go to 5370
5373 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  ptr0=rptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=1
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=r6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9511
  go to 9018
9511 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9512
  go to 9020
9512 from=isto(sptr)
  sptr=sptr+1
  p = dmax1 (rmargn, dabs (rmargn * rsto(ptr0)))
  if (.not. (rsto(ptr0) .ge. r3 - p)) go to 5941
  rsto(ptr0) = r3
  go to 5940
5941 if (.not. (rsto(ptr0) .le. r1 + p)) go to 5942
  rsto(ptr0) = r1
  go to 5940
5942 continue
5940 continue
  rptr=ptr0
  ndx0=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
5370 continue
5950 continue
  go to 9500
9018 continue
  i1=2*ipol+1
  if(.not.(i1.eq.1)) go to 5381
  i1=2
  go to 5380
5381 continue
5380 continue
  ptr1=rptr
  ptr2=rptr+i1
  rptr=ptr2+i1
  rsto(ptr1+1)=r3
  rsto(ptr1+2)=r1
  rsto(ptr2+1)=r4
  rsto(ptr2+2)=r2
  cnt=2
  if(.not.(cnt.lt.i1)) go to 5391
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  i4=isto(env+50)
  i2=isto(env+49)
  i3=isto(env+51)
  env=isto(sptr)
  sptr=sptr+1
  k=ndx10
  j=i2
3111 if(.not.(cnt.lt.i1)) go to 3110
  cnt=cnt+1
  j=j-1
  if(.not.(j.lt.0)) go to 5401
  j=j+i4
  go to 5400
5401 continue
5400 continue
  rsto(ptr1+cnt)=rsto(i3+j)
  k=k-1
  if(.not.(k.lt.0)) go to 5411
  k=k+ndx9
  go to 5410
5411 continue
5410 continue
  rsto(ptr2+cnt)=rsto(ndx8+k)
  go to 3111
3110 continue
  go to 5390
5391 continue
5390 continue
  if(.not.(ndx0.gt.0)) go to 5421
  k=ptr1
  ptr1=ptr2
  ptr2=k
  go to 5420
5421 continue
5420 continue
  go to 9500
9019 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt1
  cnt1=ipol+1
  if(.not.(cnt1.eq.1)) go to 5431
  cnt1=2
  go to 5430
5431 continue
5430 continue
  ptr1=rptr
  ptr2=rptr+cnt1
  rptr=ptr2+cnt1
3121 if(.not.(ndx4.gt.0 .and. cnt.lt.cnt1)) go to 3120
  cnt=cnt+1
  ndx=0
  ndx6=ndx4+2
  ptr0=iptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9513
  go to 9032
9513 from=isto(sptr)
  sptr=sptr+1
  ndx1=isto(ptr0+1)
  ndx2=isto(ptr0+2)
  ndx4=isto(ptr0+3)
  ndx=0
  ndx6=ndx4+ndx12
  ptr0=rptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9514
  go to 9033
9514 from=isto(sptr)
  sptr=sptr+1
  rsto(ptr1+cnt)=rsto(ptr0+1)
  rsto(ptr2+cnt)=rsto(ptr0+1+ndx12)
  ndx4=ndx2
  go to 3121
3120 continue
  if(.not.(ndx0.gt.0)) go to 5441
  k=ptr1
  ptr1=ptr2
  ptr2=k
  go to 5440
5441 continue
5440 continue
  cnt1=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9020 continue
  k1=ptr1+1
  k2=ptr2+1
  n=cnt-1
  g=one
  if(.not.(n.gt.0)) go to 5961
  a=rsto(ptr0)
  b=rsto(k1)
  c=rsto(k1+1)
  p = dmax1 (rmargn, dabs (rmargn * b))
  if (.not. (a * c .le. zero .or. a * b .le. zero)) go to 6121
  n = 1
  go to 6120
6121 continue
  if (.not. (dabs(a) .le. p)) go to 6091
  n=1
  go to 6090
6091 continue
  g = dmax1 (g, dabs (dlog10 (dabs (a / b))))
  n = min0 (n, 2 * idint (6.0d0 / g))
  n = max0 (n, 1)
6090 continue
  if(.not.(c.gt.b)) go to 5971
  i1=1
  go to 5970
5971 if(.not.(c.lt.b)) go to 5972
  i1=-1
  go to 5970
5972 continue
5970 continue
6120 continue
  c=b
  i=0
3311 if(.not.(i.lt.n .and. n.gt.1)) go to 3310
  i=i+1
  b=c
  c=rsto(k1+i)
  p = dmax1 (rmargn, dabs (rmargn * b))
  if (.not. ((c .gt. b .and. i1 .lt. 0) .or. (c .lt. b .and. i1 .gt. 0))) go to 6021
  n = i - 1
  go to 6020
6021 continue
6020 continue
  if (.not. (dabs (c - b) .le. p)) go to 6031
  n=i-1
  go to 6030
6031 continue
  g = dmax1 (g, dabs (dlog10 (dabs ((c - b) / b))))
6030 continue
  if(.not.(b*c.le.zero)) go to 6101
  n=i-1
  go to 6100
6101 continue
6100 continue
  if (.not. (dabs(c) .le. p)) go to 6111
  n = i - 1
  go to 6110
6111 continue
  g = dmax1 (g, dabs (dlog10 (dabs (c / b))))
6110 continue
  n=min0(n,2*idint(6.0d0/g))
  n=max0(n,1)
  go to 3311
3310 continue
  go to 5960
5961 continue
5960 continue
  if(.not.(n.eq.0)) go to 6041
  f=rsto(k2)
  go to 6040
6041 continue
  if(.not.(n.eq.1)) go to 6061
  i4=1
  go to 6060
6061 continue
  i4=0
  n=2*(n/2)
  i3=n/2
6060 continue
  k3=rptr+1
  k4=k3+n+1
  i=-1
3321 if(.not.(i.lt.n)) go to 3320
  i=i+1
  c=rsto(k1+i)
  rsto(k3+i)=a-c
  rsto(k4+i)=rsto(k2+i)
  if(.not.(i4.eq.0)) go to 6071
  rsto(k4+i)=rsto(k4+i)*(c**i3)
  go to 6070
6071 continue
6070 continue
  go to 3321
3320 continue
  j=0
3331 if(.not.(j.lt.n)) go to 3330
  j=j+1
  c=rsto(k3+j-1)
  d=rsto(k4+j-1)
  i=j-1
3341 if(.not.(i.lt.n)) go to 3340
  i=i+1
  b=rsto(k3+i)
  e=b*d -c*rsto(k4+i)
  rsto(k4+i)=e/(b-c)
  go to 3341
3340 continue
  go to 3331
3330 continue
  f=rsto(k4+n)
  if(.not.(i4.eq.0)) go to 6081
  f=f/(a**i3)
  go to 6080
6081 continue
6080 continue
6040 continue
  rsto(ptr0)=f
  go to 9500
9021 continue
  if(.not.(ndx.eq.351)) go to 5461
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9515
  go to 9023
9515 from=isto(sptr)
  sptr=sptr+1
  go to 5460
5461 if(.not.(ndx.eq.353)) go to 5462
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9516
  go to 9024
9516 from=isto(sptr)
  sptr=sptr+1
  go to 5460
5462 if(.not.(ndx.eq.355)) go to 5463
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9517
  go to 9025
9517 from=isto(sptr)
  sptr=sptr+1
  go to 5460
5463 if(.not.(ndx.eq.357)) go to 5464
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9518
  go to 9026
9518 from=isto(sptr)
  sptr=sptr+1
  go to 5460
5464 if(.not.(ndx.eq.359)) go to 5465
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9519
  go to 9027
9519 from=isto(sptr)
  sptr=sptr+1
  go to 5460
5465 if(.not.(ndx.eq.361)) go to 5466
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9520
  go to 9028
9520 from=isto(sptr)
  sptr=sptr+1
  go to 5460
5466 if(.not.(ndx.eq.365)) go to 5467
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9521
  go to 9029
9521 from=isto(sptr)
  sptr=sptr+1
  go to 5460
5467 if(.not.(ndx.eq.367)) go to 5468
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9522
  go to 9030
9522 from=isto(sptr)
  sptr=sptr+1
  go to 5460
5468 if(.not.(ndx.eq.369)) go to 5469
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9523
  go to 9031
9523 from=isto(sptr)
  sptr=sptr+1
  go to 5460
5469 if(.not.(ndx.eq.371)) go to 5470
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9528
  go to 9029
9528 from=isto(sptr)
  sptr=sptr+1
  rsto(rptr)=-rsto(rptr)+one
  go to 5460
5470 if(.not.(ndx.eq.373)) go to 5471
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9529
  go to 9030
9529 from=isto(sptr)
  sptr=sptr+1
  rsto(rptr)=-rsto(rptr)+one
  go to 5460
5471 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9524
  go to 9022
9524 from=isto(sptr)
  sptr=sptr+1
5460 continue
  go to 9500
9022 continue
  if(.not.(cnt.ne.1)) go to 5861
  stpflg=17
  stpi1=cnt
  continue
  call errstp
  go to 5860
5861 continue
5860 continue
  a=rsto(ptr6+1)
  if(.not.(ndx.eq.297)) go to 5481
  c = dabs (a)
  go to 5480
5481 if(.not.(ndx.eq.299)) go to 5482
  if(.not.(a.lt.zero)) go to 5821
  stpflg=1
  go to 5820
5821 if(.not.(a.eq.zero)) go to 5822
  c=zero
  go to 5820
5822 continue
  c = dsqrt (a)
5820 continue
  go to 5480
5482 if(.not.(ndx.eq.301)) go to 5483
  c = dexp (a)
  go to 5480
5483 if(.not.(ndx.eq.303)) go to 5484
  if(.not.(a.lt.zero)) go to 5511
  stpflg=2
  go to 5510
5511 if(.not.(a.eq.zero)) go to 5512
  c=-rinf
  go to 5510
5512 continue
  c = dlog (a)
5510 continue
  go to 5480
5484 if (.not. (ndx .eq. 305)) go to 5485
  if (.not. (a .lt. zero)) go to 5521
  stpflg = 3
  go to 5520
5521 if (.not. (a .eq. zero)) go to 5522
  c = -rinf
  go to 5520
5522 continue
  c = dlog (a) / dlog (ten)
5520 continue
  go to 5480
5485 if(.not.(ndx.eq.307)) go to 5486
  if(.not.(a.lt.zero)) go to 5531
  stpflg=4
  go to 5530
5531 if(.not.(a.eq.zero)) go to 5532
  c=-rinf
  go to 5530
5532 continue
  c = dlog (a) / dlog (two)
5530 continue
  go to 5480
5486 if(.not.(ndx.eq.309)) go to 5487
  if (.not. (dabs(a) .le. divzro)) go to 5541
  stpflg = 5
  go to 5540
5541 continue
  c=one/a
5540 continue
  go to 5480
5487 if(.not.(ndx.eq.311)) go to 5488
  if(.not.(a.lt.-half)) go to 5551
  stpflg=6
  go to 5550
5551 continue
  b=one
  c=one
3161 if(.not.(a.ge.b+half)) go to 3160
  b=b+one
  c=c*b
  go to 3161
3160 continue
5550 continue
  go to 5480
5488 if(.not.(ndx.eq.313)) go to 5489
  c = dint (a)
  go to 5480
5489 if(.not.(ndx.eq.315)) go to 5490
  c = a - dint (a)
  go to 5480
5490 if(.not.(ndx.eq.317)) go to 5491
  c = dnint (a)
  go to 5480
5491 if(.not.(ndx.eq.319)) go to 5492
  if(.not.(a.lt.rmargn)) go to 5561
  c=-one
  go to 5560
5561 if(.not.(a.gt.rmargn)) go to 5562
  c=one
  go to 5560
5562 continue
  c=zero
5560 continue
  go to 5480
5492 if(.not.(ndx.eq.321)) go to 5493
  c=a /180.0 *pi
  go to 5480
5493 if(.not.(ndx.eq.323)) go to 5494
  c=a *180.0 /pi
  go to 5480
5494 if(.not.(ndx.eq.325)) go to 5495
  c = randnm (zero)    !wsm + thl
  go to 5480
5495 if(.not.(ndx.eq.327)) go to 5496
  c = dsin (a)
  go to 5480
5496 if(.not.(ndx.eq.329)) go to 5497
  c = dcos (a)
  go to 5480
5497 if(.not.(ndx.eq.331)) go to 5498
  c = dtan (a)
  go to 5480
5498 if (.not. (ndx .eq. 333)) go to 5499
  if (.not. (a .lt. -one - rmargn)) go to 5571
  stpflg = 7
  go to 5570
5571 if (.not. (a .lt. -one)) go to 5572
  a = -one
  go to 5570
5572 if (.not. (a .gt. one + rmargn)) go to 5573
  stpflg = 7
  go to 5570
5573 if (.not. (a .gt. one)) go to 5574
  a = one
  go to 5570
5574 continue
5570 continue
  if (.not. (stpflg .eq. 0)) go to 5581
  c = dasin (a)
  go to 5580
5581 continue
5580 continue
  go to 5480
5499 if (.not. (ndx .eq. 335)) go to 5500
  if (.not. (a .lt. -one - rmargn)) go to 5591
  stpflg = 8
  go to 5590
5591 if (.not. (a .lt. -one)) go to 5592
  a = -one
  go to 5590
5592 if (.not. (a .gt. one + rmargn)) go to 5593
  stpflg = 8
  go to 5590
5593 if (.not. (a .gt. one)) go to 5594
  a = one
  go to 5590
5594 continue
5590 continue
  if (.not. (stpflg .eq. 0)) go to 5601
  c = dacos (a)
  go to 5600
5601 continue
5600 continue
  go to 5480
5500 if(.not.(ndx.eq.337)) go to 5501
  c = datan (a)
  go to 5480
5501 if (.not. (ndx .eq. 339)) go to 5502
  c = dsinh (a)
  go to 5480
5502 if (.not. (ndx .eq. 341)) go to 5503
  c = dcosh (a)
  go to 5480
5503 if (.not. (ndx .eq. 343)) go to 5504
  c = dtanh (a)
  go to 5480
5504 if (.not. (ndx .eq. 345)) go to 5505
  c = rnull
  go to 5480
5505 if (.not. (ndx .eq. 347)) go to 5506
  if (.not. (a .lt. one-rmargn)) go to 5611
  stpflg = 9
  go to 5610
5611 if (.not. (a .lt. one)) go to 5612
  c = zero
  go to 5610
5612 continue
  c = rnull
5610 continue
  go to 5480
5506 if(.not.(ndx.eq.349)) go to 5507
  if(.not.(a.lt.-one-rmargn)) go to 5621
  stpflg=10
  go to 5620
5621 if(.not.(a.lt.-one)) go to 5622
  a=-one
  go to 5620
5622 if(.not.(a.gt.one+rmargn)) go to 5623
  stpflg=10
  go to 5620
5623 if(.not.(a.gt.one)) go to 5624
  a=one
  go to 5620
5624 continue
5620 continue
  if(.not.(stpflg.eq.0)) go to 5631
  c=rnull
  go to 5630
5631 continue
5630 continue
  go to 5480
5507 if(.not.(ndx.eq.363)) go to 5508
  if(.not.(a.gt.rmargn)) go to 5641
  c=one
  go to 5640
5641 continue
  c=zero
5640 continue
  go to 5480
5508 continue
5480 continue
  if(.not.(stpflg.gt.0)) go to 5651
  stpr1=rsto(ptr6+1)
  continue
  call errstp
  go to 5650
5651 continue
5650 continue
  rsto(ptr6+1)=c
  go to 9500
9023 continue
  if(.not.(cnt.ne.2)) go to 5661
  stpflg=11
  stpi1=cnt
  continue
  call errstp
  go to 5660
5661 continue
5660 continue
  a=rsto(ptr6+1)
  b=rsto(ptr6+2)
  if (.not. (dabs (a) .le. rmargn .and. dabs (a) .le. rmargn)) go to 5851
  stpflg=131
  stpr1=a
  stpr2=b
  continue
  call errstp
  go to 5850
5851 continue
5850 continue
  c = datan (a / b)
  if (.not. (b .lt. zero)) go to 5671
  if (.not. (c .lt. zero)) go to 5681
  c=c+pi
  go to 5680
5681 continue
  c=c-pi
5680 continue
  go to 5670
5671 continue
5670 continue
  rptr=ptr6+1
  rsto(rptr)=c
  go to 9500
9024 continue
  if(.not.(cnt.ne.2)) go to 5691
  stpflg=12
  stpi1=cnt
  continue
  call errstp
  go to 5690
5691 continue
5690 continue
  a = rsto(rptr)
  a = dnint (a)
  rptr=rptr-1
  b=rsto(rptr)
  b = dnint (b)
  if (.not. (a .lt. zero .or. b .le. zero .or. a .gt. b)) go to 5701
  stpflg=13
  stpr1=rsto(rptr)
  stpr2=rsto(rptr+1)
  continue
  call errstp
  go to 5700
5701 continue
5700 continue
  if (.not. (a .eq. zero .or. a .eq. b)) go to 5711
  c=one
  go to 5710
5711 continue
  d=b-a
  if(.not.(a.gt.d)) go to 5721
  e=a
  a=d
  d=e
  go to 5720
5721 continue
5720 continue
  c=one
3171 if(.not.(d.lt.b)) go to 3170
  d=d+one
  c=c*d
  go to 3171
3170 continue
3181 if(.not.(a.gt.one)) go to 3180
  c=c/a
  a=a-one
  go to 3181
3180 continue
5710 continue
  rsto(rptr)=c
  go to 9500
9025 continue
  if(.not.(cnt.ne.2)) go to 5731
  stpflg=14
  stpi1=cnt
  continue
  call errstp
  go to 5730
5731 continue
5730 continue
  a=rsto(rptr)
  a = dnint (a)
  rptr=rptr-1
  b=rsto(rptr)
  b = dnint (b)
  if (.not. (a .lt. zero .or. b .le. zero .or. a .gt. b)) go to 5741
  stpflg=15
  stpr1=rsto(rptr)
  stpr2=rsto(rptr+1)
  continue
  call errstp
  go to 5740
5741 continue
5740 continue
  d=b-a
  c=one
3191 if(.not.(d.lt.b)) go to 3190
  d=d+one
  c=c*d
  go to 3191
3190 continue
  rsto(rptr)=c
  go to 9500
9026 continue
  a=rsto(rptr)
3201 if(.not.(rptr.gt.ptr6+1)) go to 3200
  rptr=rptr-1
  b=rsto(rptr)
  if(.not.(b.lt.a)) go to 5751
  a=b
  go to 5750
5751 continue
5750 continue
  go to 3201
3200 continue
  rsto(rptr)=a
  go to 9500
9027 continue
  a=rsto(rptr)
3211 if(.not.(rptr.gt.ptr6+1)) go to 3210
  rptr=rptr-1
  b=rsto(rptr)
  if(.not.(b.gt.a)) go to 5761
  a=b
  go to 5760
5761 continue
5760 continue
  go to 3211
3210 continue
  rsto(rptr)=a
  go to 9500
9028 continue
  a=rsto(rptr)
  a=a*a
3221 if(.not.(rptr.gt.ptr6+1)) go to 3220
  rptr=rptr-1
  b=rsto(rptr)
  a=a +b*b
  go to 3221
3220 continue
  rsto(rptr) = dsqrt (a)
  go to 9500
9029 continue
  a=rsto(rptr)
3231 if(.not.(rptr.gt.ptr6+1 .and. a.gt.rmargn)) go to 3230
  rptr=rptr-1
  a=rsto(rptr)
  go to 3231
3230 continue
  if(.not.(a.gt.rmargn)) go to 5771
  a=zero
  go to 5770
5771 continue
  a=one
5770 continue
  rptr=ptr6+1
  rsto(rptr)=a
  go to 9500
9030 continue
  a=rsto(rptr)
3241 if(.not.(rptr.gt.ptr6+1 .and. a.le.rmargn)) go to 3240
  rptr=rptr-1
  a=rsto(rptr)
  go to 3241
3240 continue
  if(.not.(a.le.rmargn)) go to 5781
  a=one
  go to 5780
5781 continue
  a=zero
5780 continue
  rptr=ptr6+1
  rsto(rptr)=a
  go to 9500
9031 continue
  k=0
3251 if(.not.(rptr.ge.ptr6+1)) go to 3250
  if(.not.(rsto(rptr).gt.rmargn)) go to 5791
  k=k+1
  go to 5790
5791 continue
5790 continue
  rptr=rptr-1
  go to 3251
3250 continue
  if(.not.(k.eq.1)) go to 5801
  a=one
  go to 5800
5801 continue
  a=zero
5800 continue
  rptr=ptr6+1
  rsto(rptr)=a
  go to 9500
9032 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(ishenv+7)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx7
  ndx7=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9525
  go to 9034
9525 from=isto(sptr)
  sptr=sptr+1
  ndx7=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9033 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(ishenv+8)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx7
  ndx7=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9526
  go to 9034
9526 from=isto(sptr)
  sptr=sptr+1
  ndx7=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9034 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  cnt0=isto(env+1)
  ndx0=((ndx4-1)/cnt0)*cnt0
  ndx1=((ndx6-1)/cnt0)*cnt0
  ndx2=ndx4
  ndx3=ndx6
3351 if(.not.(ndx0.le.ndx1)) go to 3350
  if(.not.(ndx0.eq.ndx1)) go to 6171
  ndx6=ndx3
  go to 6170
6171 continue
  ndx6=ndx0 +cnt0
6170 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9527
  go to 9035
9527 from=isto(sptr)
  sptr=sptr+1
  ndx0=ndx0 +cnt0
  ndx4=ndx0 +1
  go to 3351
3350 continue
  ndx4=ndx2
  ndx6=ndx3
  ndx3=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  cnt0=isto(sptr)
  sptr=sptr+1
  go to 9500
9035 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  ptr2=isto(env+3)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr7
  ptr7=isto(env+0)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=env+6+5*(ptr2-1)
  ptr3=isto(env+1)
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx0.ne.ptr3)) go to 6181
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=env+1
  k=0
  i=0
3361 if(.not.(i.lt.ptr7 .and. k.eq.0)) go to 3360
  i=i+1
  env=env+5
  if(.not.(ndx0.eq.isto(env+1))) go to 6191
  k=1
  ptr2=i
  go to 6190
6191 continue
6190 continue
  go to 3361
3360 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(k.eq.0)) go to 6201
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=env+1
  i=0
  k=iinf
3371 if(.not.(i.lt.ptr7)) go to 3370
  i=i+1
  env=env+5
  j=isto(env+3)
  if(.not.(j.lt.k)) go to 6211
  k=j
  ptr2=i
  go to 6210
6211 continue
6210 continue
  go to 3371
3370 continue
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=env+6+5*(ptr2-1)
  k=isto(env+0)
  if(.not.(isto(env+4).gt.0)) go to 6221
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  ptr4=isto(env+2)
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx7.eq.1)) go to 6231
  write(bkfile, rec=ptr4) (isto(k+i),i=1,cnt0)
  go to 6230
6231 if(.not.(ndx7.eq.2)) go to 6232
  write(bkfile, rec=ptr4) (rsto(k+i),i=1,cnt0)
  go to 6230
6232 continue
6230 continue
  go to 6220
6221 continue
6220 continue
  env=isto(sptr)
  sptr=sptr+1
  ptr5=ndx0/cnt0 +1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  k1=0
3381 if(.not.(env.gt.0 .and. k1.eq.0)) go to 3380
  if(.not.(isto(env+1).eq.ptr5)) go to 6241
  k1=1
  ptr4=isto(env+2)
  ptr6=env
  go to 6240
6241 continue
6240 continue
  env=isto(env+0)
  go to 3381
3380 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(k1.gt.0)) go to 6251
  if(.not.(ndx7.eq.1)) go to 6261
  read(bkfile, rec=ptr4) (isto(k+i),i=1,cnt0)
  go to 6260
6261 if(.not.(ndx7.eq.2)) go to 6262
  read(bkfile, rec=ptr4) (rsto(k+i),i=1,cnt0)
  go to 6260
6262 continue
6260 continue
  go to 6250
6251 continue
  ptr4=isto(ishenv+9)+1
  isto(ishenv+9)=ptr4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+5)
  isto(env+0)=iptr+1
  env=iptr+1
  iptr=iptr+3
  isto(env+0)=0
  isto(env+1)=ptr5
  isto(env+2)=ptr4
  ptr6=env
  env=isto(sptr)
  sptr=sptr+1
  isto(env+5)=ptr6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=env+6+5*(ptr2-1)
  k=isto(env+0)
  i=0
  if(.not.(ndx7.eq.1)) go to 6301
3421 if(.not.(i.lt.cnt0)) go to 3420
  i=i+1
  isto(k+i)=inull
  go to 3421
3420 continue
  go to 6300
6301 continue
3391 if(.not.(i.lt.cnt0)) go to 3390
  i=i+1
  rsto(k+i)=rnull
  go to 3391
3390 continue
6300 continue
  env=isto(sptr)
  sptr=sptr+1
6250 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=env+6+5*(ptr2-1)
  isto(env+1)=ndx0
  isto(env+2)=ptr6
  isto(env+4)=0
  env=isto(sptr)
  sptr=sptr+1
  go to 6200
6201 continue
6200 continue
  go to 6180
6181 continue
6180 continue
  k=isto(env+2)
  k=k+1
  isto(env+2)=k
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=env+6+5*(ptr2-1)
  isto(env+3)=k
  k1=ndx6 -ndx4 +1
  k2=isto(env+0) -ndx0 +ndx4 -1
  k3=ptr0+ndx4-ndx2
  i=0
  if(.not.(ndx.eq.0)) go to 6271
3401 if(.not.(i.lt.k1)) go to 3400
  i=i+1
  if(.not.(ndx7.eq.1)) go to 6281
  isto(k3+i)=isto(k2+i)
  go to 6280
6281 if(.not.(ndx7.eq.2)) go to 6282
  rsto(k3+i)=rsto(k2+i)
  go to 6280
6282 continue
6280 continue
  go to 3401
3400 continue
  go to 6270
6271 continue
  isto(env+4)=1
3411 if(.not.(i.lt.k1)) go to 3410
  i=i+1
  if(.not.(ndx7.eq.1)) go to 6291
  isto(k2+i)=isto(k3+i)
  go to 6290
6291 if(.not.(ndx7.eq.2)) go to 6292
  rsto(k2+i)=rsto(k3+i)
  go to 6290
6292 continue
6290 continue
  go to 3411
3410 continue
6270 continue
  env=isto(sptr)
  sptr=sptr+1
  ptr7=isto(sptr)
  sptr=sptr+1
  ptr6=isto(sptr)
  sptr=sptr+1
  ptr5=isto(sptr)
  sptr=sptr+1
  ptr4=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  go to 9500
9036 continue
  write(cbuff,'(i12)') ndx
  i=0
3261 if(.not.(i.lt.12)) go to 3260
  i=i+1
  char1=cbuff(i:i)
  if(.not.(char1.ne.csto(64))) go to 5831
  cptr=cptr+1
  csto(cptr)=char1
  go to 5830
5831 continue
5830 continue
  go to 3261
3260 continue
  go to 9500
9037 continue
  i4=isto(base4+ndx5)
  i6=isto(base6+ndx5)
  if(.not.(ndx4.lt.i4 .or. ndx6.gt.i6)) go to 5841
  stpflg=129
  stpi1=ndx4
  stpi2=ndx6
  stpi3=i4
  stpi4=i6
  continue
  call errstp
  go to 5840
5841 continue
5840 continue
  go to 9500
end subroutine xpr2

!
! end of file xpr2.f90
!
