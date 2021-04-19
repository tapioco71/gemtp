!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: xpr1.for
!
!
!     subroutine xpr1.
!
subroutine xpr1
  include 'tacsto.ins'
  sptr=sptr-1
  isto(sptr)=from
  from=0
  go to (9000, 9001, 9002, 9003, 9004, 9005, 9006, 9007, 9008, 9009, 9010, 9011, 9012, 9013, 9014, 9015, 9016, 9017, 9018, 9019, &
       9020, 9021, 9022, 9023, 9024, 9025, 9026, 9027, 9028), to-8999
  stop 'invalid "to" reference in "xpr1".'
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, 9516, 9517, 9518, 9519, &
          9520, 9521, 9522, 9523, 9524, 9525, 9526, 9527, 9528, 9529, 9530, 9531, 9532, 9533, 9534, 9535, 9536, 9537, 9538, &
          9539, 9540, 9541, 9542, 9543, 9544, 9545, 9546, 9547, 9548, 9549, 9550, 9551, 9552, 9553, 9554, 9555), from-9500
  stop 'invalid "from" reference in "xpr1".'
910 stpflg=42
  stpi1=iptr
  stpi2=ilen-iptr
  continue
  call errstp
930 stpflg=44
  stpi1=rptr
  stpi2=rlen-rptr
  continue
  call errstp
9000 continue
  if(sptr-100.le.iptr) go to 910
  if(rptr+100.ge.rsptr) go to 930
  sptr=sptr-1
  isto(sptr)=env
  env=xprndx
  sptr=sptr-1
  isto(sptr)=xprknd
  xprknd=isto(env+1)
  sptr=sptr-1
  isto(sptr)=xprsub
  xprsub=isto(env+2)
  sptr=sptr-1
  isto(sptr)=cnt0
  cnt0=0
  sptr=sptr-1
  isto(sptr)=ptr0
  ptr0=rptr
  sptr=sptr-1
  isto(sptr)=cnt1
  k=isto(env+3)
  sptr=sptr-1
  isto(sptr)=env
  env=env+4
  if(.not.(k.gt.0)) go to 6041
  sptr=sptr-1
  isto(sptr)=cnt6
  sptr=sptr-1
  isto(sptr)=from
  from=9555
  go to 9008
9555 from=isto(sptr)
  sptr=sptr+1
  cnt0=cnt6
  cnt6=isto(sptr)
  sptr=sptr+1
  go to 6040
6041 continue
3001 if(.not.(env.gt.0)) go to 3000
  sptr=sptr-1
  isto(sptr)=from
  from=9501
  go to 9001
9501 from=isto(sptr)
  sptr=sptr+1
  to=9003
  call xpr2
  env=isto(env+0)
  go to 3001
3000 continue
6040 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(isto(env+0).eq.2)) go to 5001
  i=0
3011 if(.not.(i.lt.cnt0)) go to 3010
  i=i +1
  if(.not.(rsto(ptr0+i).gt.rmargn)) go to 5011
  a=one
  go to 5010
5011 continue
  a=zero
5010 continue
  rsto(ptr0+i)=a
  go to 3011
3010 continue
  go to 5000
5001 continue
5000 continue
  xprcnt=cnt0
  cnt1=isto(sptr)
  ptr0=isto(sptr+1)
  cnt0=isto(sptr+2)
  xprsub=isto(sptr+3)
  xprknd=isto(sptr+4)
  env=isto(sptr+5)
  sptr=sptr+6
  go to 9500
9001 continue
  sptr=sptr-1
  isto(sptr)=ptr1
  ptr1=rptr
  sptr=sptr-1
  isto(sptr)=cnt2
  cnt1=0
  sptr=sptr-1
  isto(sptr)=env
  env=env+1
3021 if(.not.(env.gt.0)) go to 3020
  sptr=sptr-1
  isto(sptr)=from
  from=9502
  go to 9002
9502 from=isto(sptr)
  sptr=sptr+1
  to=9004
  call xpr2
  env=isto(env+0)
  go to 3021
3020 continue
  env=isto(sptr)
  sptr=sptr+1
  cnt2=isto(sptr)
  ptr1=isto(sptr+1)
  sptr=sptr+2
  go to 9500
9002 continue
  sptr=sptr-1
  isto(sptr)=ptr2
  ptr2=rptr
  sptr=sptr-1
  isto(sptr)=cnt3
  sptr=sptr-1
  isto(sptr)=env
  env=env+3
  sptr=sptr-1
  isto(sptr)=from
  from=9503
  go to 9003
9503 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  cnt2=cnt3
  i=isto(env+1)
  if(.not.(i.gt.0)) go to 5021
  sptr=sptr-1
  isto(sptr)=ndx
  sptr=sptr-1
  isto(sptr)=env
  env=i
  sptr=sptr-1
  isto(sptr)=from
  from=9504
  go to 9003
9504 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  rptr=rptr-cnt3
  ndx=isto(env+2)
  if(.not.(cnt2.eq.1)) go to 5031
  a=rsto(ptr2+1)
  i=0
3031 if(.not.(i.lt.cnt3)) go to 3030
  i=i+1
  b=rsto(rptr+i)
  c=zero
  to=9005
  call xpr2
  rsto(ptr2+i)=c
  go to 3031
3030 continue
  cnt2=cnt3
  go to 5030
5031 if(.not.(cnt3.eq.1)) go to 5032
  b=rsto(rptr+1)
  i=0
3041 if(.not.(i.lt.cnt2)) go to 3040
  i=i+1
  a=rsto(ptr2+i)
  c=zero
  to=9005
  call xpr2
  rsto(ptr2+i)=c
  go to 3041
3040 continue
  go to 5030
5032 continue
  c=zero
  if(.not.(cnt2.ne.cnt3)) go to 5041
  if(.not.(ndx.eq.2)) go to 5051
  c=one
  go to 5050
5051 continue
5050 continue
  go to 5040
5041 continue
  i=0
  k=1
3051 if(.not.(k.gt.0 .and. i.lt.cnt2))go to 3050
  i=i+1
  a=rsto(ptr2+i)
  b=rsto(rptr+i)
  p=dmax1(rmargn,dabs(rmargn*b))
  if(.not.(a.lt.b-p .or. a.gt.b+p)) go to 5061
  k=0
  go to 5060
5061 continue
5060 continue
  go to 3051
3050 continue
  if(.not.((k.gt.0 .and. ndx.eq.1) .or. (k.eq.0 .and. ndx.eq.2))) go to 5071
  c=one
  go to 5070
5071 continue
5070 continue
5040 continue
  rsto(ptr2+1)=c
  cnt2=1
5030 continue
  rptr=ptr2+cnt2
  ndx=isto(sptr)
  sptr=sptr+1
  go to 5020
5021 continue
5020 continue
  cnt3=isto(sptr)
  ptr2=isto(sptr+1)
  sptr=sptr+2
  go to 9500
9003 continue
  sptr=sptr-1
  isto(sptr)=ptr3
  ptr3=rptr
  sptr=sptr-1
  isto(sptr)=cnt4
  cnt3=0
  sptr=sptr-1
  isto(sptr)=env
  env=env+2
3061 if(.not.(env.gt.0)) go to 3060
  sptr=sptr-1
  isto(sptr)=from
  from=9505
  go to 9006
9505 from=isto(sptr)
  sptr=sptr+1
  to=9006
  call xpr2
  env=isto(env+0)
  go to 3061
3060 continue
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  isto(sptr)=from
  from=9506
  go to 9004
9506 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  isto(sptr)=from
  from=9507
  go to 9005
9507 from=isto(sptr)
  sptr=sptr+1
  cnt4=isto(sptr)
  ptr3=isto(sptr+1)
  sptr=sptr+2
  go to 9500
9004 continue
  xprndx=isto(env+0)
  if(.not.(xprndx.gt.0)) go to 5081
  sptr=sptr-1
  isto(sptr)=from
  from=9508
  go to 9000
9508 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  i=0
  if(.not.(xprcnt.eq.1)) go to 5091
  a=rsto(rptr+1)
  p=dmax1(rmargn,dabs(rmargn*a))
3071 if(.not.(i.lt.cnt3)) go to 3070
  i=i+1
  if(.not.(rsto(ptr3+i).lt.a-p .and. rsto(ptr3+i).ne.rnull)) go to 5101
  rsto(ptr3+i)=a
  go to 5100
5101 continue
5100 continue
  go to 3071
3070 continue
  go to 5090
5091 continue
  if(.not.(xprcnt.ne.cnt3)) go to 5111
  stpflg=33
  stpi1=cnt3
  stpi2=xprcnt
  stpi3=xprknd
  stpi4=xprsub
  continue
  call errstp
  go to 5110
5111 continue
3081 if(.not.(i.lt.cnt3)) go to 3080
  i=i+1
  a=rsto(rptr+i)
  p=dmax1(rmargn,dabs(rmargn*a))
  if(.not.(rsto(ptr3+i).lt.a-p .and. rsto(ptr3+i).ne.rnull )) go to 5121
  rsto(ptr3+i)=a
  go to 5120
5121 continue
5120 continue
  go to 3081
3080 continue
5110 continue
5090 continue
  go to 5080
5081 continue
5080 continue
  go to 9500
9005 continue
  xprndx=isto(env+1)
  if(.not.(xprndx.gt.0)) go to 5131
  sptr=sptr-1
  isto(sptr)=from
  from=9509
  go to 9000
9509 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  i=0
  if(.not.(xprcnt.eq.1)) go to 5141
  a=rsto(rptr+1)
  p=dmax1(rmargn,dabs(rmargn*a))
3091 if(.not.(i.lt.cnt3)) go to 3090
  i=i+1
  if(.not.(rsto(ptr3+i).gt.a+p .and. rsto(ptr3+i).ne.rnull)) go to 5151
  rsto(ptr3+i)=a
  go to 5150
5151 continue
5150 continue
  go to 3091
3090 continue
  go to 5140
5141 continue
  if(.not.(xprcnt.ne.cnt3)) go to 5161
  stpflg=34
  stpi1=cnt3
  stpi2=xprcnt
  stpi3=xprknd
  stpi4=xprsub
  continue
  call errstp
  go to 5160
5161 continue
3101 if(.not.(i.lt.cnt3)) go to 3100
  i=i+1
  a=rsto(rptr+i)
  p=dmax1(rmargn,dabs(rmargn*a))
  if(.not.(rsto(ptr3+i).gt.a+p .and. rsto(ptr3+i).ne.rnull)) go to 5171
  rsto(ptr3+i)=a
  go to 5170
5171 continue
5170 continue
  go to 3101
3100 continue
5160 continue
5140 continue
  go to 5130
5131 continue
5130 continue
  go to 9500
9006 continue
  sptr=sptr-1
  isto(sptr)=ptr4
  ptr4=rptr
  sptr=sptr-1
  isto(sptr)=cnt5
  cnt4=0
  sptr=sptr-1
  isto(sptr)=env
  env=env+2
3111 if(.not.(env.gt.0)) go to 3110
  sptr=sptr-1
  isto(sptr)=from
  from=9510
  go to 9007
9510 from=isto(sptr)
  sptr=sptr+1
  to=9007
  call xpr2
  env=isto(env+0)
  go to 3111
3110 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(isto(env+1).eq.2)) go to 5181
  i=0
3121 if(.not.(i.lt.cnt4)) go to 3120
  i=i+1
  rsto(ptr4+i)= -rsto(ptr4+i)
  go to 3121
3120 continue
  go to 5180
5181 continue
5180 continue
  cnt5=isto(sptr)
  ptr4=isto(sptr+1)
  sptr=sptr+2
  go to 9500
9007 continue
  sptr=sptr-1
  isto(sptr)=ptr5
  ptr5=rptr
  sptr=sptr-1
  isto(sptr)=cnt6
  sptr=sptr-1
  isto(sptr)=env
  env=env+4
  sptr=sptr-1
  isto(sptr)=from
  from=9511
  go to 9008
9511 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  cnt5=cnt6
  i=isto(env+3)
  if(.not.(i.gt.0)) go to 5191
  sptr=sptr-1
  isto(sptr)=env
  env=i
  sptr=sptr-1
  isto(sptr)=from
  from=9512
  go to 9008
9512 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  isto(sptr)=ndx1
  ndx1=isto(env+2)
  rptr=rptr -cnt6
  i=0
  if(.not.(cnt5.eq.1)) go to 5201
  a=rsto(rptr)
3131 if(.not.(i.lt.cnt6)) go to 3130
  i=i+1
  b=rsto(rptr+i)
  if(.not.(ndx1.eq.3)) go to 5211
  to=9008
  call xpr2
  go to 5210
5211 continue
  to=9009
  call xpr2
5210 continue
  rsto(ptr5+i)=c
  go to 3131
3130 continue
  cnt5=cnt6
  go to 5200
5201 if(.not.(cnt6.eq.1)) go to 5202
  b=rsto(rptr+1)
3141 if(.not.(i.lt.cnt5)) go to 3140
  i=i+1
  a=rsto(ptr5+i)
  if(.not.(ndx1.eq.3)) go to 5221
  to=9008
  call xpr2
  go to 5220
5221 continue
  to=9009
  call xpr2
5220 continue
  rsto(ptr5+i)=c
  go to 3141
3140 continue
  go to 5200
5202 continue
  if(.not.(ndx1.eq.3)) go to 5231
  stpflg=80
  go to 5230
5231 continue
  stpflg=81
5230 continue
  stpi1=cnt5
  stpi2=cnt6
  continue
  call errstp
5200 continue
  rptr=ptr5+cnt5
  ndx1=isto(sptr)
  sptr=sptr+1
  go to 5190
5191 continue
5190 continue
  if(.not.(isto(env+1).eq.2)) go to 5241
  i=0
3151 if(.not.(i.lt.cnt5)) go to 3150
  i=i+1
  a=rsto(ptr5+i)
  rptr=rptr+1
  rsto(rptr)=a
  to=9010
  call xpr2
  a=rsto(rptr)
  rptr=rptr-1
  rsto(ptr5+i)=a
  go to 3151
3150 continue
  go to 5240
5241 continue
5240 continue
  cnt6=isto(sptr)
  ptr5=isto(sptr+1)
  sptr=sptr+2
  go to 9500
9008 continue
  sptr=sptr-1
  isto(sptr)=ptr6
  ptr6=rptr
  cnt6=0
  i=isto(env+0)
  sptr=sptr-1
  isto(sptr)=env
  env=env+2
  if(.not.(i.eq.1)) go to 5251
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+0)
3161 if(.not.(env.gt.0)) go to 3160
  xprndx=env+1
  sptr=sptr-1
  isto(sptr)=from
  from=9513
  go to 9000
9513 from=isto(sptr)
  sptr=sptr+1
  cnt6=cnt6 +xprcnt
  env=isto(env+0)
  go to 3161
3160 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 5250
5251 if(.not.(i.eq.2)) go to 5252
  rptr=rptr+1
  cnt6=1
  j=isto(env+2)
  rsto(rptr)=rsto(j)
  go to 5250
5252 if(.not.(i.eq.3)) go to 5253
  xprndx=isto(env+0)
  sptr=sptr-1
  isto(sptr)=from
  from=9514
  go to 9000
9514 from=isto(sptr)
  sptr=sptr+1
  cnt6=xprcnt
  go to 5250
5253 continue
  sptr=sptr-1
  isto(sptr)=from
  from=9515
  go to 9009
9515 from=isto(sptr)
  sptr=sptr+1
5250 continue
  env=isto(sptr)
  sptr=sptr+1
  k=isto(env+1)
  if(.not.(k.gt.0)) go to 5261
  i=0
3171 if(.not.(i.lt.cnt6)) go to 3170
  i=i+1
  a=rsto(ptr6+i)
  if(.not.(k.eq.1)) go to 5271
  if(.not.(a.gt.rmargn)) go to 5281
  a=zero
  go to 5280
5281 continue
  a=one
5280 continue
  go to 5270
5271 if(.not.(k.eq.2)) go to 5272
  a=-a
  go to 5270
5272 continue
5270 continue
  rsto(ptr6+i)=a
  go to 3171
3170 continue
  go to 5260
5261 continue
5260 continue
  ptr6=isto(sptr)
  sptr=sptr+1
  go to 9500
9009 continue
  sptr=sptr-1
  isto(sptr)=ndx1
  ndx1=isto(env+2)
  sptr=sptr-1
  isto(sptr)=ndx2
  ndx2=isto(env+3)
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  ndx5=isto(env+1)
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=stpl1
  stpl1=isto(env+7)
  sptr=sptr-1
  isto(sptr)=stpc1
  stpc1=isto(env+8)
  k=isto(env+13)
  cnt6=1
  if(.not.(k.eq.7)) go to 5291
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+15)
  sptr=sptr-1
  isto(sptr)=from
  from=9516
  go to 9010
9516 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5290
5291 if(.not.(ndx1.eq.8 .or. ndx1.eq.9)) go to 5292
  k=isto(base5+ndx5)
  a=rsto(base3+k)
  rptr=rptr+1
  rsto(rptr)=a
  go to 5290
5292 if(.not.(ndx5.lt.0)) go to 5293
  a=rsto(base7-ndx5)
  if(.not.(a.eq.rnull .and. ndx5.ne.-20)) go to 5301
  stpflg=83
  go to 5300
5301 continue
5300 continue
  rptr=rptr+1
  rsto(rptr)=a
  go to 5290
5293 continue
  sptr=sptr-1
  isto(sptr)=ndx1
  ndx1=isto(env+9)
  sptr=sptr-1
  isto(sptr)=ndx2
  ndx2=isto(env+10)
  sptr=sptr-1
  isto(sptr)=from
  from=9517
  go to 9020
9517 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  isto(sptr)=base4
  sptr=sptr-1
  isto(sptr)=base5
  sptr=sptr-1
  isto(sptr)=base6
  if(.not.(isto(env+5).lt.isto(env+6))) go to 6051
  base4=base14
  base5=base15
  base6=base16
  go to 6050
6051 continue
6050 continue
  to=9037
  call xpr2
  base6=isto(sptr)
  base5=isto(sptr+1)
  base4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  sptr=sptr+5
  sptr=sptr-1
  isto(sptr)=isto(ishenv+17)
  isto(ishenv+17)=isto(env+5)
  to=9002
  call xpr2
  isto(ishenv+17)=isto(sptr)
  sptr=sptr+1
  cnt6=ndx6 -ndx4 +1
5290 continue
  if(.not.(stpflg.gt.0)) go to 5311
  continue
  call errstp
  go to 5310
5311 continue
5310 continue
  stpc1=isto(sptr)
  stpl1=isto(sptr+1)
  ndx6=isto(sptr+2)
  ndx5=isto(sptr+3)
  ndx4=isto(sptr+4)
  ndx2=isto(sptr+5)
  ndx1=isto(sptr+6)
  sptr=sptr+7
  go to 9500
9010 continue
  i=isto(env+0)
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+1)
  if(.not.(i.eq.1)) go to 5321
  k=isto(env+1)
  if(.not.(k.ge.399 .and. k.le.409)) go to 5331
  to=9012
  call xpr2
  go to 5330
5331 continue
  sptr=sptr-1
  isto(sptr)=from
  from=9518
  go to 9011
9518 from=isto(sptr)
  sptr=sptr+1
5330 continue
  go to 5320
5321 if(.not.(i.eq.2)) go to 5322
  sptr=sptr-1
  isto(sptr)=from
  from=9519
  go to 9015
9519 from=isto(sptr)
  sptr=sptr+1
  go to 5320
5322 continue
5320 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9011 continue
  sptr=sptr-1
  isto(sptr)=ndx
  ndx=isto(env+1)
  sptr=sptr-1
  isto(sptr)=ipol
  sptr=sptr-1
  isto(sptr)=ndx0
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx3
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx7
  sptr=sptr-1
  isto(sptr)=ndx8
  sptr=sptr-1
  isto(sptr)=ndx9
  sptr=sptr-1
  isto(sptr)=ndx10
  sptr=sptr-1
  isto(sptr)=ndx11
  sptr=sptr-1
  isto(sptr)=ndx12
  sptr=sptr-1
  isto(sptr)=ndx13
  sptr=sptr-1
  isto(sptr)=ndx14
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+2)
  stpl2=isto(env+7)
  stpc2=isto(env+8)
  ndx5=isto(env+1)
  ndx0=isto(env+5)
  sptr=sptr-1
  isto(sptr)=base4
  sptr=sptr-1
  isto(sptr)=base6
  if(.not.(ndx0.ne.datenv)) go to 5341
  base4=base14
  base6=base16
  go to 5340
5341 continue
5340 continue
  ndx2=isto(base4+ndx5)
  ndx3=isto(base6+ndx5)
  base6=isto(sptr)
  sptr=sptr+1
  base4=isto(sptr)
  sptr=sptr+1
  xprndx=isto(env+9)
  if(.not.(xprndx.gt.0)) go to 5351
  sptr=sptr-1
  isto(sptr)=from
  from=9520
  go to 9000
9520 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5361
  stpflg=18
  stpi1=xprcnt
  continue
  call errstp
  go to 5360
5361 continue
5360 continue
  a=rsto(rptr)
  rptr=rptr-1
  ndx4=idnint(a)
  if(.not.(ndx4.lt.ndx2 .or. ndx4.gt.ndx3)) go to 5371
  stpflg=19
  stpi1=ndx4
  stpi2=ndx2
  stpi3=ndx3
  continue
  call errstp
  go to 5370
5371 continue
5370 continue
  go to 5350
5351 continue
  ndx4=1
5350 continue
  i1=isto(env+2)
  ndx1=isto(env+3)
  env=isto(sptr)
  sptr=sptr+1
  i2=isto(env+4)
  sptr=sptr-1
  isto(sptr)=env
  if(.not.(ndx0.eq.datenv)) go to 5381
  env=useenv
  go to 5380
5381 continue
  env=isto(ishenv+10)
5380 continue
  if(.not.(i1.eq.6)) go to 5391
  i3=4
  i4=isto(env+29)
  go to 5390
5391 continue
  i3=3
  i4=isto(env+36)
5390 continue
  i0=i4 -1 +i3*(ndx1-1)
  if(.not.(i2.gt.0)) go to 5401
  i3=isto(i0+3)
  i7=i3 +4*(ndx4-ndx2)
  ndx11=isto(i7+1)
  ndx14=isto(i7+3)
  env=isto(sptr)
  sptr=sptr+1
  go to 5400
5401 continue
  i3=isto(i0+2)
  ndx12=i3 +(ndx4-ndx2)
  i4=isto(i0+1)
  ndx13=i4 +7*(ndx4-ndx2)
  ndx8=isto(ndx13)
  ndx9=isto(ndx13+5)
  ndx10=isto(ndx13+2)
  ndx11=isto(ndx13+3)
  ndx14=isto(ndx13+6)
  env=isto(sptr)
  sptr=sptr+1
  ndx7=base3 +isto(base5+ndx5) -ndx2 +ndx4
  xprndx=isto(env+7)
  if(.not.(xprndx.gt.0)) go to 5411
  sptr=sptr-1
  isto(sptr)=from
  from=9521
  go to 9000
9521 from=isto(sptr)
  sptr=sptr+1
  a=rsto(rptr)
  rptr=rptr-1
  ipol=idnint(a)
  go to 5410
5411 continue
  ipol=1
5410 continue
  if(.not.(ipol.gt.ndx9/2 .and. ndx.gt.379 .and. ndx.le.391 .and. ndx.ne.387)) go to 5421
  stpflg=20
  stpi1=ipol
  stpi2=ndx9/2
  continue
  call errstp
  go to 5420
5421 continue
5420 continue
  xprndx=isto(env+6)
  if(.not.(xprndx.gt.0)) go to 5431
  sptr=sptr-1
  isto(sptr)=from
  from=9522
  go to 9000
9522 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5441
  stpflg=21
  stpi1=xprcnt
  continue
  call errstp
  go to 5440
5441 continue
5440 continue
  go to 5430
5431 continue
5430 continue
  r1=rsto(base3+7)
  r2=rsto(ndx8+ndx10)
  r3=rsto(base3+8)
  r4=rsto(ndx7)
5400 continue
  stpi1=ndx4
  if(.not.(ndx.eq.379)) go to 5451
  to=9014
  call xpr2
  go to 5450
5451 if(.not.(ndx.eq.381)) go to 5452
  sptr=sptr-1
  isto(sptr)=from
  from=9523
  go to 9012
9523 from=isto(sptr)
  sptr=sptr+1
  go to 5450
5452 if(.not.(ndx.eq.383)) go to 5453
  to=9013
  call xpr2
  go to 5450
5453 if(.not.(ndx.eq.385)) go to 5454
  to=9015
  call xpr2
  go to 5450
5454 if(.not.(ndx.eq.387)) go to 5455
  rptr=rptr+1
  rsto(rptr)=r2
  go to 5450
5455 if(.not.(ndx.eq.389)) go to 5456
  to=9016
  call xpr2
  go to 5450
5456 if(.not.(ndx.eq.391)) go to 5457
  to=9017
  call xpr2
  go to 5450
5457 if(.not.(ndx.eq.393 .or. ndx.eq.395)) go to 5458
  sptr=sptr-1
  isto(sptr)=from
  from=9524
  go to 9013
9524 from=isto(sptr)
  sptr=sptr+1
  go to 5450
5458 if(.not.(ndx.eq.411)) go to 5459
  sptr=sptr-1
  isto(sptr)=from
  from=9525
  go to 9014
9525 from=isto(sptr)
  sptr=sptr+1
  go to 5450
5459 if(.not.(ndx.eq.413)) go to 5460
  rsptr=rsptr-1
  rsto(rsptr)=rsto(base7+1)
  rsto(base7+1)=rsto(rptr)
  rptr=rptr-1
  sptr=sptr-1
  isto(sptr)=from
  from=9526
  go to 9014
9526 from=isto(sptr)
  sptr=sptr+1
  rsto(base7+1)=rsto(rsptr)
  rsptr=rsptr+1
  go to 5450
5460 continue
5450 continue
  ndx14=isto(sptr)
  ndx13=isto(sptr+1)
  ndx12=isto(sptr+2)
  ndx11=isto(sptr+3)
  sptr=sptr+4
  ndx10=isto(sptr)
  ndx9=isto(sptr+1)
  ndx8=isto(sptr+2)
  ndx7=isto(sptr+3)
  ndx5=isto(sptr+4)
  ndx4=isto(sptr+5)
  sptr=sptr+6
  ndx3=isto(sptr)
  ndx2=isto(sptr+1)
  ndx1=isto(sptr+2)
  ndx0=isto(sptr+3)
  ipol=isto(sptr+4)
  ndx=isto(sptr+5)
  sptr=sptr+6
  go to 9500
9012 continue
  sptr=sptr-1
  isto(sptr)=flg
  sptr=sptr-1
  isto(sptr)=ndx
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=ptr0
  r5=rsto(rptr)
  rptr=rptr-1
  p=dmax1(rmargn,dabs(rmargn*r3))
  if(.not.(r5.gt.r3+p)) go to 6011
  stpflg=28
  stpr1=r3
  stpr2=r5
  continue
  call errstp
  go to 6010
6011 continue
6010 continue
  if(.not.(r5.ge.r3-p)) go to 5471
  rptr=rptr+1
  rsto(rptr)=r4
  go to 5470
5471 continue
  sptr=sptr-1
  isto(sptr)=env
  env=useenv
  ndx4=isto(env+14)
  env=isto(sptr)
  sptr=sptr+1
  flg=0
3181 if(.not.(ndx4.gt.0 .and. flg.eq.0)) go to 3180
  ndx=0
  ndx6=ndx4+2
  ptr0=iptr
  to=9032
  call xpr2
  ndx1=isto(ptr0+1)
  ndx2=isto(ptr0+2)
  ndx4=isto(ptr0+3)
  ndx=0
  ndx6=ndx4+ndx12
  ptr0=rptr
  to=9033
  call xpr2
  a=rsto(ptr0+1)
  b=rsto(ptr0+1 +ndx12)
  p=dmax1(rmargn,dabs(rmargn*r5))
  if(.not.(a.le.r5+p)) go to 5481
  flg=1
  go to 5480
5481 continue
  ndx4=ndx2
5480 continue
  go to 3181
3180 continue
  p=dmax1(rmargn,dabs(rmargn*r5))
  if(.not.(flg.eq.0)) go to 5491
  rptr=rptr+1
  rsto(rptr)=rsto(base7+1)
  rsto(base7+1)=r5
  sptr=sptr-1
  isto(sptr)=from
  from=9527
  go to 9014
9527 from=isto(sptr)
  sptr=sptr+1
  a=rsto(rptr)
  rptr=rptr-1
  rsto(base7+1)=rsto(rptr)
  rptr=rptr-1
  rptr=rptr+1
  rsto(rptr)=a
  go to 5490
5491 if(.not.(a.ge.r5-p)) go to 5492
  rptr=rptr+1
  rsto(rptr)=b
  go to 5490
5492 continue
  ndx4=ndx1
  sptr=sptr-1
  isto(sptr)=ptr0
  ptr0=rptr+1
  sptr=sptr-1
  isto(sptr)=ptr1
  sptr=sptr-1
  isto(sptr)=ptr2
  sptr=sptr-1
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  isto(sptr)=ndx0
  ndx0=0
  rptr=rptr+1
  rsto(rptr)=r5
  to=9019
  call xpr2
  to=9020
  call xpr2
  rptr=ptr0
  ndx0=isto(sptr)
  cnt=isto(sptr+1)
  ptr2=isto(sptr+2)
  ptr1=isto(sptr+3)
  ptr0=isto(sptr+4)
  sptr=sptr+5
5490 continue
5470 continue
  ptr0=isto(sptr)
  ndx6=isto(sptr+1)
  ndx4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  ndx=isto(sptr+5)
  flg=isto(sptr+6)
  sptr=sptr+7
  go to 9500
9013 continue
  rptr=rptr+1
  rsto(rptr)=r1
  rptr=rptr+1
  rsto(rptr)=r2
  rptr=rptr+1
  rsto(rptr)=r3
  rptr=rptr+1
  rsto(rptr)=r4
  sptr=sptr-1
  isto(sptr)=flg
  flg=1
  sptr=sptr-1
  isto(sptr)=ndx
  if(.not.(ndx.eq.393)) go to 5501
  ndx=1
  go to 5500
5501 continue
  ndx=2
5500 continue
  sptr=sptr-1
  isto(sptr)=ptr1
  ptr1=rptr
  sptr=sptr-1
  isto(sptr)=ptr2
  ptr2=0
  sptr=sptr-1
  isto(sptr)=ptr3
  ptr3=ndx13
  sptr=sptr-1
  isto(sptr)=from
  from=9528
  go to 9024
9528 from=isto(sptr)
  sptr=sptr+1
  a=rsto(rptr)
  rptr=rptr-1
  b=rsto(rptr)
  rptr=rptr-1
  r4=rsto(rptr)
  rptr=rptr-1
  r3=rsto(rptr)
  rptr=rptr-1
  r2=rsto(rptr)
  rptr=rptr-1
  r1=rsto(rptr)
  rptr=rptr-1
  d=b*r4 +a
  rptr=rptr+1
  rsto(rptr)=d
  ptr3=isto(sptr)
  ptr2=isto(sptr+1)
  ptr1=isto(sptr+2)
  ndx=isto(sptr+3)
  flg=isto(sptr+4)
  sptr=sptr+5
  go to 9500
9014 continue
  xprndx=ndx11
  if(.not.(xprndx.gt.0)) go to 5511
  sptr=sptr-1
  isto(sptr)=from
  from=9529
  go to 9000
9529 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  if(.not.(ndx14.gt.xprcnt .and. xprcnt.gt.1)) go to 5521
  stpflg=115
  stpi2=ndx14
  stpi3=xprcnt
  continue
  call errstp
  go to 5520
5521 if(.not.(xprcnt.gt.1)) go to 5522
  rsto(rptr+1)=rsto(rptr+ndx14)
  go to 5520
5522 continue
5520 continue
  rptr=rptr+1
  go to 5510
5511 continue
  stpflg=24
  stpi2=ndx4
  continue
  call errstp
5510 continue
  go to 9500
9015 continue
  sptr=sptr-1
  isto(sptr)=ndx
  ndx=isto(env+1)
  sptr=sptr-1
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  isto(sptr)=ndx1
  ndx1=isto(env+4)
  if(.not.(ndx1.eq.1)) go to 5531
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+5)
  k=isto(env+2)
  env=isto(sptr)
  sptr=sptr+1
  i=0
3191 if(.not.(i.lt.k)) go to 3190
  i=i+1
  rsto(ptr6+i)=rnull
  go to 3191
3190 continue
  go to 5530
5531 continue
5530 continue
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+7)
3201 if(.not.(env .gt.0)) go to 3200
  cnt=cnt+1
  xprndx=isto(env+1)
  if(.not.(xprndx.eq.0)) go to 5541
  if(.not.(ndx1.eq.1)) go to 5551
  a=rnull
  rptr=rptr+1
  rsto(rptr)=a
  go to 5550
5551 continue
  stpflg=16
  stpi1=cnt
  continue
  call errstp
5550 continue
  go to 5540
5541 continue
  sptr=sptr-1
  isto(sptr)=from
  from=9530
  go to 9000
9530 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5561
  stpflg=89
  stpi1=cnt
  stpi2=xprcnt
  continue
  call errstp
  go to 5560
5561 continue
5560 continue
5540 continue
  env=isto(env+0)
  go to 3201
3200 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.eq.325)) go to 6021
  cnt=1
  a=rnull
  rptr=rptr+1
  rsto(rptr)=a
  go to 6020
6021 continue
6020 continue
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+5)
  if(.not.(ndx1.eq.0)) go to 5571
  to=9021
  call xpr2
  go to 5570
5571 if(.not.(ndx1.eq.1)) go to 5572
  sptr=sptr-1
  isto(sptr)=from
  from=9531
  go to 9016
9531 from=isto(sptr)
  sptr=sptr+1
  go to 5570
5572 if(.not.(ndx1.eq.2)) go to 5573
  sptr=sptr-1
  isto(sptr)=from
  from=9532
  go to 9017
9532 from=isto(sptr)
  sptr=sptr+1
  go to 5570
5573 if(.not.(ndx1.eq.3)) go to 5574
  sptr=sptr-1
  isto(sptr)=from
  from=9533
  go to 9019
9533 from=isto(sptr)
  sptr=sptr+1
  go to 5570
5574 continue
5570 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  cnt=isto(sptr+1)
  ndx=isto(sptr+2)
  sptr=sptr+3
  go to 9500
9016 continue
  sptr=sptr-1
  isto(sptr)=cnt1
  cnt1=0
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+3)
3211 if(.not.(env.gt.0)) go to 3210
  cnt1=cnt1+1
  if(.not.(cnt1.gt.cnt)) go to 5581
  a=rnull
  go to 5580
5581 continue
  a=rsto(ptr6+cnt1)
5580 continue
  if(.not.(a.eq.rnull)) go to 5591
  stpl2=isto(env+1)
  stpc2=isto(env+2)
  xprndx=isto(env+6)
  if(.not.(xprndx.eq.0)) go to 5601
  stpflg=94
  continue
  call errstp
  go to 5600
5601 continue
5600 continue
  sptr=sptr-1
  isto(sptr)=from
  from=9534
  go to 9000
9534 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5611
  stpflg=92
  stpi1=xprcnt
  continue
  call errstp
  go to 5610
5611 continue
5610 continue
  rsto(ptr6+cnt1)=rsto(rptr)
  rptr=rptr-1
  go to 5590
5591 continue
5590 continue
  env=isto(env+0)
  go to 3211
3210 continue
  env=isto(sptr)
  sptr=sptr+1
  k=base5+isto(env+1)+1
  k=base3+isto(k)-1
  i=0
3221 if(.not.(i.lt.cnt1)) go to 3220
  i=i+1
  rsto(k+i)=rsto(ptr6+i)
  go to 3221
3220 continue
  xprndx=isto(env+4)
  sptr=sptr-1
  isto(sptr)=from
  from=9535
  go to 9000
9535 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5621
  stpflg=93
  stpi1=xprcnt
  continue
  call errstp
  go to 5620
5621 continue
5620 continue
  rsto(ptr6+1)=rsto(rptr)
  rptr=ptr6+1
  cnt1=isto(sptr)
  sptr=sptr+1
  go to 9500
9017 continue
  sptr=sptr-1
  isto(sptr)=ipol
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=flg
  if(.not.(cnt.gt.2)) go to 5631
  stpflg=25
  stpi1=cnt
  continue
  call errstp
  go to 5630
5631 continue
5630 continue
  if(.not.(cnt.eq.2)) go to 5641
  b=rsto(ptr6+2)
  ipol=idnint(b)
  go to 5640
5641 continue
  ipol=1
5640 continue
  ndx1=isto(env+2)
  sptr=sptr-1
  isto(sptr)=env
  env=ndx1
  xprndx=isto(env+2)
  sptr=sptr-1
  isto(sptr)=from
  from=9536
  go to 9000
9536 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5651
  stpflg=117
  stpi1=xprcnt
  continue
  call errstp
  go to 5650
5651 continue
5650 continue
  b=rsto(rptr)
  rptr=rptr-1
  p=dmax1(rmargn,dabs(rmargn*b))
  a=rsto(ptr6+1)
  if(.not.(a.gt.b+p)) go to 5661
  stpflg=26
  stpr1=a
  stpr2=b
  continue
  call errstp
  go to 5660
5661 continue
5660 continue
  if(.not.(a.ge.b-p)) go to 5671
  xprndx=isto(env+3)
  sptr=sptr-1
  isto(sptr)=from
  from=9537
  go to 9000
9537 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5681
  stpflg=117
  stpi1=xprcnt
  continue
  call errstp
  go to 5680
5681 continue
5680 continue
  go to 5670
5671 continue
  flg=0
  ndx1=isto(env+1)
3231 if(.not.(ndx1.gt.0 .and. flg.eq.0)) go to 3230
  env=ndx1
  xprndx=isto(env+2)
  sptr=sptr-1
  isto(sptr)=from
  from=9538
  go to 9000
9538 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5691
  stpflg=117
  stpi1=xprcnt
  continue
  call errstp
  go to 5690
5691 continue
5690 continue
  b=rsto(rptr)
  rptr=rptr-1
  p=dmax1(rmargn,dabs(rmargn*b))
  a=rsto(ptr6+1)
  if(.not.(a.ge.b-p)) go to 5701
  flg=1
  go to 5700
5701 continue
  ndx1=isto(env+1)
5700 continue
  go to 3231
3230 continue
  if(.not.(flg.eq.0)) go to 5711
  stpflg=27
  stpr1=a
  stpr2=b
  continue
  call errstp
  go to 5710
5711 continue
5710 continue
  if(.not.(a.le.b+p .or. ipol.eq.0)) go to 5721
  xprndx=isto(env+3)
  sptr=sptr-1
  isto(sptr)=from
  from=9539
  go to 9000
9539 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5731
  stpflg=117
  stpi1=xprcnt
  continue
  call errstp
  go to 5730
5731 continue
5730 continue
  go to 5720
5721 continue
  ndx1=isto(env+0)
  sptr=sptr-1
  isto(sptr)=ptr0
  ptr0=rptr+1
  sptr=sptr-1
  isto(sptr)=ptr1
  sptr=sptr-1
  isto(sptr)=ptr2
  sptr=sptr-1
  isto(sptr)=cnt
  cnt=0
  rptr=rptr+1
  rsto(rptr)=a
  sptr=sptr-1
  isto(sptr)=from
  from=9540
  go to 9018
9540 from=isto(sptr)
  sptr=sptr+1
  to=9020
  call xpr2
  rptr=ptr6+1
  rsto(rptr)=rsto(ptr0)
  cnt=isto(sptr)
  ptr2=isto(sptr+1)
  ptr1=isto(sptr+2)
  ptr0=isto(sptr+3)
  sptr=sptr+4
5720 continue
5670 continue
  env=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  ndx1=isto(sptr+1)
  ipol=isto(sptr+2)
  sptr=sptr+3
  go to 9500
9018 continue
  i1=2*ipol+1
  if(.not.(i1.eq.1)) go to 5741
  i1=2
  go to 5740
5741 continue
5740 continue
  ptr1=rptr
  ptr2=rptr+i1
  rptr=ptr2+i1
  cnt=0
3241 if(.not.(ndx1.gt.0 .and. cnt.lt.i1)) go to 3240
  cnt=cnt+1
  env=ndx1
  xprndx=isto(env+2)
  sptr=sptr-1
  isto(sptr)=from
  from=9541
  go to 9000
9541 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5751
  stpflg=117
  stpi1=xprcnt
  continue
  call errstp
  go to 5750
5751 continue
5750 continue
  b=rsto(rptr)
  rptr=rptr-1
  rsto(ptr1+cnt)=b
  xprndx=isto(env+3)
  sptr=sptr-1
  isto(sptr)=from
  from=9542
  go to 9000
9542 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5761
  stpflg=117
  stpi1=xprcnt
  continue
  call errstp
  go to 5760
5761 continue
5760 continue
  c=rsto(rptr)
  rptr=rptr-1
  rsto(ptr2+cnt)=c
  ndx1=isto(env+1)
  go to 3241
3240 continue
  go to 9500
9019 continue
  stpflg=95
  continue
  call errstp
  go to 9500
9020 continue
  if(.not.(ndx1.gt.0)) go to 5771
  xprndx=ndx1
  sptr=sptr-1
  isto(sptr)=from
  from=9543
  go to 9000
9543 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5781
  stpflg=90
  stpi1=xprcnt
  continue
  call errstp
  go to 5780
5781 continue
5780 continue
  a=rsto(rptr)
  rptr=rptr-1
  ndx4=idnint(a)
  if(.not.(ndx2.gt.0)) go to 5791
  xprndx=ndx2
  sptr=sptr-1
  isto(sptr)=from
  from=9544
  go to 9000
9544 from=isto(sptr)
  sptr=sptr+1
  if(.not.(xprcnt.gt.1)) go to 5801
  stpflg=91
  stpi1=xprcnt
  continue
  call errstp
  go to 5800
5801 continue
5800 continue
  a=rsto(rptr)
  rptr=rptr-1
  ndx6=idnint(a)
  go to 5790
5791 continue
  ndx6=ndx4
5790 continue
  go to 5770
5771 continue
  ndx4=1
  ndx6=1
5770 continue
  go to 9500
9021 continue
  xprndx=ndx1
  if(.not.(xprndx.gt.0)) go to 5811
  sptr=sptr-1
  isto(sptr)=from
  from=9545
  go to 9000
9545 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  a=rsto(rptr+1)
  p=dmax1(rmargn,dabs(rmargn*a))
  if(.not.(rsto(rptr).lt.a-p .and. rsto(rptr).ne.rnull)) go to 5821
  rsto(rptr)=a
  go to 5820
5821 continue
5820 continue
  go to 5810
5811 continue
5810 continue
  xprndx=ndx2
  if(.not.(xprndx.gt.0)) go to 5831
  sptr=sptr-1
  isto(sptr)=from
  from=9546
  go to 9000
9546 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  a=rsto(rptr+1)
  p=dmax1(rmargn,dabs(rmargn*a))
  if(.not.(rsto(rptr).gt.a+p .and. rsto(rptr).ne.rnull)) go to 5841
  rsto(rptr)=a
  go to 5840
5841 continue
5840 continue
  go to 5830
5831 continue
5830 continue
  go to 9500
9022 continue
  sptr=sptr-1
  isto(sptr)=flg
  flg=0
  xprndx=ndx1
  if(.not.(xprndx.gt.0)) go to 5851
  sptr=sptr-1
  isto(sptr)=from
  from=9547
  go to 9000
9547 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  a=rsto(rptr+1)
  p=dmax1(rmargn,dabs(rmargn*a))
  if(.not.(rsto(rptr).lt.a-p .and. rsto(rptr).ne.rnull)) go to 5861
  flg=1
  rsto(rptr)=a
  go to 5860
5861 continue
5860 continue
  go to 5850
5851 continue
5850 continue
  xprndx=ndx2
  if(.not.(xprndx.gt.0)) go to 5871
  sptr=sptr-1
  isto(sptr)=from
  from=9548
  go to 9000
9548 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  a=rsto(rptr+1)
  p=dmax1(rmargn,dabs(rmargn*a))
  if(.not.(rsto(rptr).gt.a+p .and. rsto(rptr).ne.rnull)) go to 5881
  flg=1
  rsto(rptr)=a
  go to 5880
5881 continue
5880 continue
  go to 5870
5871 continue
5870 continue
  if(.not.(flg.gt.0)) go to 5891
  isto(ptr3+4)=1
  go to 5890
5891 continue
5890 continue
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9023 continue
  sptr=sptr-1
  isto(sptr)=flg
  flg=0
  xprndx=ndx1
  if(.not.(xprndx.gt.0)) go to 5901
  sptr=sptr-1
  isto(sptr)=from
  from=9549
  go to 9000
9549 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  a=rsto(rptr+1)
  p=dmax1(rmargn,dabs(rmargn*a))
  if(.not.(rsto(rptr).lt.a-p .and. rsto(rptr).ne.rnull)) go to 5911
  flg=1
  rsto(rptr)=a
  go to 5910
5911 continue
5910 continue
  go to 5900
5901 continue
5900 continue
  xprndx=ndx2
  if(.not.(xprndx.gt.0)) go to 5921
  sptr=sptr-1
  isto(sptr)=from
  from=9550
  go to 9000
9550 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  a=rsto(rptr+1)
  p=dmax1(rmargn,dabs(rmargn*a))
  if(.not.(rsto(rptr).gt.a+p .and. rsto(rptr).ne.rnull)) go to 5931
  flg=1
  rsto(rptr)=a
  go to 5930
5931 continue
5930 continue
  go to 5920
5921 continue
5920 continue
  if(.not.(flg.gt.0)) go to 5941
  isto(ptr2+2)=isto(ptr2+2) +2
  i2=isto(ptr2)+1
  rsto(i2)=rsto(rptr)
  go to 5940
5941 continue
5940 continue
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9024 continue
  sptr=sptr-1
  isto(sptr)=from
  from=9551
  go to 9025
9551 from=isto(sptr)
  sptr=sptr+1
  f=one
  i0=ptr1+ndx+2
  e=rsto(i0)
  j1=i0+ndx
  i2=ndx+2
  i1=rptr
  i=0
3251 if(.not.(i.lt.ndx)) go to 3250
  i=i+1
  i2=i2-1
  i1=i1+i2
  a=f/(e-rsto(i0+i))
  rsto(rptr+i)=a
  rsto(i1)=a
  go to 3251
3250 continue
  b=rsto(ptr1+2)
  r1=rsto(ptr1+1) +b*rsto(rptr+1)
  r2=-b*rsto(rptr+ndx+1)*rsto(j1+1)
  i2=ndx+2
  i1=rptr
  j=1
3261 if(.not.(j.lt.ndx)) go to 3260
  j=j+1
  f=f+one
  e=rsto(i0+j-1)
  i2=i2-1
  i1=i1+i2
  i4=i2
  i3=i1
  i=j-1
3271 if(.not.(i.lt.ndx)) go to 3270
  i=i+1
  i4=i4-1
  i3=i3+i4
  a=f/(e-rsto(i0+i))
  rsto(i3)=a*rsto(i3)
  rsto(i1-j+i+1)=a *rsto(i1)
  i6=ndx+2
  i5=rptr
  k=-1
3281 if(.not.(k.lt.j-2)) go to 3280
  k=k+1
  b=rsto(i5-k+i)
  b=a*(rsto(i5-k+j-1) -b)
  rsto(i5-k+i)=b
  i6=i6-1
  i5=i5+i6
  go to 3281
3280 continue
  go to 3271
3270 continue
  a=rsto(ptr1+j+1)
  r1=r1 +a*rsto(rptr+j)
  b=zero
  c=one
  i6=ndx+2
  i5=rptr
  k=0
3291 if(.not.(k.lt.j)) go to 3290
  k=k+1
  i6=i6-1
  i5=i5+i6
  c=-c
  b=b +c*rsto(i5-k+j)*rsto(j1+k)
  go to 3291
3290 continue
  r2=r2 +b*a
  go to 3261
3260 continue
  rptr=ptr1+2
  if(.not.(ndx.eq.0)) go to 6031
  rsto(ptr1+2)=zero
  go to 6030
6031 continue
  rsto(ptr1+1)=r1
  rsto(ptr1+2)=r2
6030 continue
  go to 9500
9025 continue
  if(.not.(flg.eq.0)) go to 5951
  sptr=sptr-1
  isto(sptr)=from
  from=9552
  go to 9026
9552 from=isto(sptr)
  sptr=sptr+1
  go to 5950
5951 continue
  sptr=sptr-1
  isto(sptr)=from
  from=9553
  go to 9027
9553 from=isto(sptr)
  sptr=sptr+1
5950 continue
  a=rsto(base3+8)
  rptr=rptr+1
  rsto(rptr)=a
  sptr=sptr-1
  isto(sptr)=env
  env=useenv
  i0=isto(env+51)
  j=isto(env+50)
  k=isto(env+49)+1
  i=0
3301 if(.not.(i.lt.ndx)) go to 3300
  i=i+1
  k=k-1
  if(.not.(k.lt.0)) go to 5961
  k=k+j
  go to 5960
5961 continue
5960 continue
  a=rsto(i0+k)
  rptr=rptr+1
  rsto(rptr)=a
  go to 3301
3300 continue
  i0=isto(ptr3)
  j=isto(ptr3+5)
  k=isto(ptr3+2) +1
  i=0
3311 if(.not.(i.lt.ndx)) go to 3310
  i=i+1
  k=k-1
  if(.not.(k.lt.0)) go to 5971
  k=k+j
  go to 5970
5971 continue
5970 continue
  a=rsto(i0+k)
  rptr=rptr+1
  rsto(rptr)=a
  go to 3311
3310 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9026 continue
  sptr=sptr-1
  isto(sptr)=ptr0
  ptr0=rptr+1
  rptr=ptr0+ndx
  i=-1
3321 if(.not.(i.lt.ndx)) go to 3320
  i=i+1
  rsto(ptr0+i)=zero
  go to 3321
3320 continue
  sptr=sptr-1
  isto(sptr)=env
  env=ptr2
3331 if(.not.(env.gt.0)) go to 3330
  xprndx=isto(env+3)
  if(.not.(xprndx.gt.0)) go to 5981
  sptr=sptr-1
  isto(sptr)=from
  from=9554
  go to 9000
9554 from=isto(sptr)
  sptr=sptr+1
  rptr=rptr-xprcnt
  a=rsto(rptr+1)
  go to 5980
5981 continue
  a=one
5980 continue
  if(.not.(isto(env+1).eq.2)) go to 5991
  a=-a
  go to 5990
5991 continue
5990 continue
  i=ptr0+isto(env+2)
  rsto(i)=rsto(i) +a
  env=isto(env+0)
  go to 3331
3330 continue
  env=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
  go to 9500
9027 continue
  a=zero
  rptr=rptr+1
  rsto(rptr)=a
  if(.not.(ndx.eq.1)) go to 6001
  a=one
  rptr=rptr+1
  rsto(rptr)=a
  go to 6000
6001 continue
  rptr=rptr+1
  rsto(rptr)=a
  a=one
  rptr=rptr+1
  rsto(rptr)=a
6000 continue
  go to 9500
9028 continue
  a=half*rsto(base3+6)
  rptr=rptr+1
  rsto(rptr)=a
  i3=isto(ptr3+2)
  i4=isto(ptr3)
  i5=isto(ptr2)
  b=rsto(i5) +rsto(i4+i3)*a
  rptr=rptr+1
  rsto(rptr)=b
  go to 9500
end subroutine xpr1
!
!     end of file: xpr1.for
!
