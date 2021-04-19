!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: putil1.for
!
!
!     subroutine putil1.
!
subroutine putil1
  include  'tacsto.ins'
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=0
  go to (9100, 9101, 9102, 9103, 9104, 9105, 9106 , 9107, 9108, 9109, 9110, 9111, 9112, 9113, 9114, 9115, 9116, 9117, 9118, 9119, &
       9120, 9121 , 9122, 9123, 9124, 9125, 9126, 9127, 9128, 9129, 9130), to-9099
  write(unit06,99)
99 format(' invalid "to" reference in putil1.')
  stop
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, 9516, 9517, 9518, 9519, 9520,&
          9521, 9522, 9523, 9524, 9525, 9526, 9527, 9528, 9529, 9530, 9531, 9532, 9533, 9534, 9535, 9536, 9537, 9538, 9539, 9540, &
          9541, 9542, 9543, 9544, 9545, 9546, 9547, 9548, 9549, 9550, 9551, 9552, 9553), from-9500
  write(unit06,98)
98 format(' invalid "from" reference in putil1.')
  stop
910 stpflg=42
  stpi1=iptr
  stpi2=ilen-iptr
  continue
  call errstp
9100 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  mndx=247
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5001
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5011
  isto(ishenv+54)=18
  continue
  call synstp
  go to 5010
5011 continue
5010 continue
  flg=1
  ndx1=1
  go to 5000
5001 continue
  ndx1=0
  flg=0
5000 continue
  if(.not.(ndx.eq.4)) go to 5021
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=397
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9501
  go to 9112
9501 from=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx1.gt.5)) go to 5031
  isto(ishenv+54)=31
  continue
  call synstp
  go to 5030
5031 continue
5030 continue
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5020
5021 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+17)
  if(.not.(ndx.eq.3)) go to 5041
  isto(ishenv+17)=isto(ishenv+12)
  go to 5040
5041 continue
5040 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9502
  go to 9120
9502 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+17)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
5020 continue
  if(.not.(flg.gt.0)) go to 5051
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5061
  isto(ishenv+54)=18
  continue
  call synstp
  go to 5060
5061 continue
5060 continue
  go to 5050
5051 continue
5050 continue
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9101 continue
  iptr=iptr+4
  isto(env+1)=isto(ishenv+50)
  isto(env+2)=isto(ishenv+51)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr12
  ptr12=isto(ishenv+56)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr13
  ptr13=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr14
  ptr14=rptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr15
  ptr15=isto(ishenv+45)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr16
  ptr16=isto(ishenv+44)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr17
  ptr17=isto(ishenv+23)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr18
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr19
  ptr19=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3001 if(.not.(lpflg.gt.0)) go to 3000
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9102
9503 from=isto(sptr)
  sptr=sptr+1
  mndx=269
  to=9207
  call putil2
  if(.not.(mflg.gt.0)) go to 5071
  mndx=269
  to=9205
  call putil2
  ndx0=2
  ptr19=0
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5070
5071 continue
  isto(env+0)=0
  lpflg=0
5070 continue
  go to 3001
3000 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  isto(env+0)=ndx0
  if(.not.(ptr19.gt.0)) go to 6041
  iptr=env+3
  isto(env+3)=iptr+1
  rptr=ptr14
  isto(ishenv+56)=ptr12
  isto(ishenv+45)=ptr15
  isto(ishenv+44)=ptr16
  isto(ishenv+23)=ptr17
  dptr=ptr18
  if(.not.(isto(ishenv).gt.0)) go to 6051
  i=0
3141 if(.not.(i.lt.ptr13)) go to 3140
  i=i+1
  backspace 12
  go to 3141
3140 continue
  go to 6050
6051 continue
6050 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9553
  go to 9107
9553 from=isto(sptr)
  sptr=sptr+1
  mndx=117
  to=9208
  call putil2
  env=isto(sptr)
  sptr=sptr+1
  go to 6040
6041 continue
  isto(env+3)=0
6040 continue
  ptr19=isto(sptr)
  sptr=sptr+1
  ptr18=isto(sptr)
  sptr=sptr+1
  ptr17=isto(sptr)
  sptr=sptr+1
  ptr16=isto(sptr)
  sptr=sptr+1
  ptr15=isto(sptr)
  sptr=sptr+1
  ptr14=isto(sptr)
  sptr=sptr+1
  k=ptr13
  ptr13=isto(sptr)
  sptr=sptr+1
  ptr13=ptr13+k
  ptr12=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  go to 9500
9102 continue
  ptr13=ptr13+1
  iptr=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3011 if(.not.(lpflg.gt.0)) go to 3010
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9103
9504 from=isto(sptr)
  sptr=sptr+1
  mndx=271
  to=9207
  call putil2
  if(.not.(mflg.gt.0)) go to 5081
  mndx=271
  to=9205
  call putil2
  ptr19=0
  ndx0=2
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5080
5081 continue
  isto(env+0)=0
  lpflg=0
5080 continue
  go to 3011
3010 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9103 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=0
  ptr13=ptr13+1
  iptr=iptr+3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9104
9505 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  mndx=107
  to=9208
  call putil2
  if(.not.(mflg.gt.0)) go to 5091
  ndx=1
  go to 5090
5091 continue
  mndx=111
  to=9208
  call putil2
  if(.not.(mflg.gt.0)) go to 5101
  mndx=109
  to=9208
  call putil2
  if(.not.(mflg.gt.0)) go to 5111
  ndx=2
  go to 5110
5111 continue
  mndx=107
  to=9208
  call putil2
  if(.not.(mflg.gt.0)) go to 5121
  ndx=4
  go to 5120
5121 continue
  ndx=3
5120 continue
5110 continue
  go to 5100
5101 continue
  mndx=109
  to=9208
  call putil2
  if(.not.(mflg.gt.0)) go to 5131
  mndx=107
  to=9208
  call putil2
  if(.not.(mflg.gt.0)) go to 5141
  ndx=6
  go to 5140
5141 continue
  ndx=5
5140 continue
  go to 5130
5131 continue
5130 continue
5100 continue
5090 continue
  isto(env+2)=ndx
  if(.not.(ndx.gt.0)) go to 5151
  to=9204
  call putil2
  ptr19=0
  ndx0=2
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9104
9506 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5150
5151 continue
  isto(env+1)=0
5150 continue
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9104 continue
  ptr13=ptr13+1
  iptr=iptr+2
  mndx=91
  to=9205
  call putil2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=1
3021 if(.not.(ndx.gt.0)) go to 3020
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9507
  go to 9105
9507 from=isto(sptr)
  sptr=sptr+1
  mndx=91
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5161
  ndx=1
  go to 5160
5161 continue
  mndx=93
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5171
  ndx=2
  go to 5170
5171 continue
  ndx=0
5170 continue
5160 continue
  if(.not.(ndx.gt.0)) go to 5181
  ptr19=0
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5180
5181 continue
  isto(env+0)=0
5180 continue
  go to 3021
3020 continue
  ndx=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr3
  ptr3=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr4
  ptr4=0
  mndx=87
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5191
  ptr19=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9508
  go to 9123
9508 from=isto(sptr)
  sptr=sptr+1
  if(.not.(isto(ishenv+55).gt.0.and. isto(ishenv+50).eq.27)) go to 5201
  if(.not.(isto(ishenv+57).gt.0)) go to 5211
  isto(ishenv+54)=72
  continue
  call synstp
  go to 5210
5211 continue
5210 continue
  isto(ishenv+57)=isto(ishenv+58)
  go to 5200
5201 continue
5200 continue
  go to 5190
5191 continue
  mndx=117
  to=9208
  call putil2
5190 continue
  isto(env+0)=ptr3
  isto(env+1)=ptr4
  ptr4=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  go to 9500
9105 continue
  ptr13=ptr13+1
  iptr=iptr+2
  isto(env+1)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=1
3031 if(.not.(ndx.gt.0)) go to 3030
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9509
  go to 9106
9509 from=isto(sptr)
  sptr=sptr+1
  mndx=95
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5221
  ndx=1
  go to 5220
5221 continue
  mndx=97
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5231
  ndx=2
  go to 5230
5231 continue
  ndx=0
5230 continue
5220 continue
  if(.not.(ndx.gt.0)) go to 5241
  ptr19=0
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5240
5241 continue
  isto(env+0)=0
5240 continue
  go to 3031
3030 continue
  ndx=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9106 continue
  ptr13=ptr13+1
  iptr=iptr+4
  isto(env+1)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9510
  go to 9107
9510 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  mndx=99
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5251
  isto(env+2)=2
  go to 5250
5251 continue
  mndx=275
  to=9207
  call putil2
  if(.not.(mflg.gt.0)) go to 5261
  mndx=275
  to=9205
  call putil2
  isto(env+2)=3
  go to 5260
5261 continue
5260 continue
5250 continue
  if(.not.(mflg.gt.0)) go to 5271
  ptr19=0
  isto(env+3)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9511
  go to 9107
9511 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5270
5271 continue
  isto(env+3)=0
  isto(env+2)=1
5270 continue
  go to 9500
9107 continue
  ptr13=ptr13+1
  ptr18=dptr
  iptr=iptr+2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  ndx1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg1
  flg1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=dptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  mndx=79
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5281
  ndx1=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9512
  go to 9108
9512 from=isto(sptr)
  sptr=sptr+1
  go to 5280
5281 continue
  to=9211
  call putil2
  if(.not.(mflg.gt.0)) go to 5291
  dptr=dptr-1
  ndx1=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9513
  go to 9110
9513 from=isto(sptr)
  sptr=sptr+1
  go to 5290
5291 continue
  mndx=91
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5301
  to=9211
  call putil2
  if(.not.(mflg.gt.0)) go to 5311
  dptr=dptr-1
  ndx1=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9514
  go to 9110
9514 from=isto(sptr)
  sptr=sptr+1
  go to 5310
5311 continue
  isto(ishenv+54)=47
  continue
  call synstp
5310 continue
  go to 5300
5301 continue
  mndx=93
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5321
  to=9211
  call putil2
  if(.not.(mflg.gt.0)) go to 5331
  dptr=dptr-1
  flg1=1
  ndx1=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9515
  go to 9110
9515 from=isto(sptr)
  sptr=sptr+1
  go to 5330
5331 continue
  ndx=2
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5341
  ndx1=3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9516
  go to 9111
9516 from=isto(sptr)
  sptr=sptr+1
  go to 5340
5341 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9517
  go to 9112
9517 from=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
5340 continue
5330 continue
  go to 5320
5321 continue
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5351
  ndx1=3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9518
  go to 9111
9518 from=isto(sptr)
  sptr=sptr+1
  go to 5350
5351 continue
  mndx=273
  to=9207
  call putil2
  if(.not.(mflg.gt.0)) go to 5361
  mndx=273
  to=9205
  call putil2
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5371
  ndx=1
  ndx1=3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9519
  go to 9111
9519 from=isto(sptr)
  sptr=sptr+1
  go to 5370
5371 continue
5370 continue
  go to 5360
5361 continue
5360 continue
  if(.not.(ndx1.eq.0)) go to 5381
  dptr=ptr1
  mndx=273
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5391
  ndx=1
  go to 5390
5391 continue
5390 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9520
  go to 9112
9520 from=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  go to 5380
5381 continue
5380 continue
5350 continue
5320 continue
5300 continue
5290 continue
5280 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+0)=ndx1
  isto(env+1)=ndx
  ptr1=isto(sptr)
  sptr=sptr+1
  flg1=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9108 continue
  ptr13=ptr13+1
  iptr=iptr+1
  isto(env+0)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3041 if(.not.(lpflg.gt.0)) go to 3040
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9521
  go to 9109
9521 from=isto(sptr)
  sptr=sptr+1
  mndx=81
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5401
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5400
5401 continue
  isto(env+0)=0
  lpflg=0
5400 continue
  go to 3041
3040 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9109 continue
  ptr13=ptr13+1
  iptr=iptr+2
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+51)
  isto(ishenv+51)=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9522
  go to 9101
9522 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+51)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9110 continue
  iptr=iptr+3
  isto(env+1)=dptr+1
  to=9214
  call putil2
  isto(env+2)=rptr
  isto(env+0)=dptr -isto(env+1) +1
  ptr13=ptr13+1
  go to 9500
9111 continue
  ptr13=ptr13+1
  iptr=iptr+1
  isto(env+0)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+51)
  isto(ishenv+51)=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9523
  go to 9101
9523 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+51)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5501
  isto(ishenv+54)=48
  continue
  call synstp
  go to 5500
5501 continue
5500 continue
  go to 9500
9112 continue
  iptr=iptr+16
  ipn=isto(ishenv+23)
  isto(ipn)=env
  isto(ishenv+23)=env
  isto(env+0)=0
  isto(env+4)=isto(ishenv+50)
  isto(env+5)=datenv
  isto(env+6)=isto(ishenv+12)
  isto(env+11)=ptr6
  isto(env+12)=ptr10
  isto(env+14)=ndx2
  isto(env+8)=dptr+1
  to=9200
  call putil2
  isto(env+7)=dptr -isto(env+8) +1
  ptr13=ptr13+1
  to=9204
  call putil2
  isto(env+15)=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  ptr2=0
  mndx=79
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5531
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9524
  go to 9121
9524 from=isto(sptr)
  sptr=sptr+1
  if(.not.(ptr2.gt.0)) go to 5541
  ndx1=6
  go to 5540
5541 continue
  ndx1=5
5540 continue
  go to 5530
5531 continue
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5551
  ndx1=7
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  ndx3=isto(env+7)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  ndx4=isto(env+8)
  isto(env+15)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9525
  go to 9113
9525 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  go to 5550
5551 continue
  ndx1=4
5550 continue
5530 continue
  isto(env+9)=ptr1
  isto(env+10)=ptr2
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(env+13)=ndx1
  go to 9500
9113 continue
  ptr13=ptr13+1
  iptr=iptr+2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9526
  go to 9114
9526 from=isto(sptr)
  sptr=sptr+1
  isto(env+1)=iptr+1
  if(.not.((ndx.ge.379 .and. ndx.le.395).or. (ndx.ge.411 .and. ndx.le.413))) go to 5581
  isto(env+0)=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9527
  go to 9116
9527 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5580
5581 continue
  isto(env+0)=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9528
  go to 9117
9528 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
5580 continue
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9114 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=1
  ndx5=isto(env+1) -2 +base1
  ndx6=isto(env+0)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9529
  go to 9115
9529 from=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.eq.0)) go to 5591
  ndx5=isto(env+3) -2 +base1
  ndx6=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9530
  go to 9115
9530 from=isto(sptr)
  sptr=sptr+1
  go to 5590
5591 continue
5590 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  go to 9500
9115 continue
  i0=0
  k=0
3081 if(.not.(i0.lt.ndx6 .and. k.eq.0)) go to 3080
  i0=i0+1
  ndx5=ndx5+2
  i1=isto(ndx5+1)
  if(.not.(i1.eq.ndx3)) go to 5601
  i=0
  i2=base2 +isto(ndx5)
3091 if(.not.(i.lt.ndx3 .and. csto(i2+i).eq.csto(ndx4+i))) go to 3090
  i=i+1
  go to 3091
3090 continue
  if(.not.(i.eq.ndx3)) go to 5611
  k=1
  go to 5610
5611 continue
5610 continue
  go to 5600
5601 continue
5600 continue
  go to 3081
3080 continue
  if(.not.(k.gt.0)) go to 5621
  ndx=ndx5-base1
  go to 5620
5621 continue
  ndx=0
5620 continue
  go to 9500
9116 continue
  ptr13=ptr13+1
  iptr=iptr+9
  isto(env+1)=ndx
  isto(env+3)=0
  isto(env+8)=0
  if(.not.(isto(ishenv+55).gt.0.and. isto(ishenv+50).eq.27)) go to 5631
  isto(ishenv+56)=isto(ishenv+56)+1
  go to 5630
5631 continue
5630 continue
  ipn=isto(ishenv+45)
  isto(ipn)=env
  isto(ishenv+45)=env
  isto(env+0)=0
  isto(env+6)=0
  isto(env+7)=0
  isto(env+4)=0
  isto(env+2)=iptr+1
  if(.not.(ndx.eq.411)) go to 5641
  if(.not.(isto(ishenv+50).ne.15 .and. isto(ishenv+50).ne.25 .and. isto(ishenv+50).ne.52)) go to 5651
  isto(ishenv+54)=64
  continue
  call synstp
  go to 5650
5651 continue
  k=1
5650 continue
  go to 5640
5641 if(.not.(ndx.eq.413)) go to 5642
  k=1
  go to 5640
5642 if(.not.(isto(ishenv+52).gt.0)) go to 5643
  isto(ishenv+54)=90
  continue
  call synstp
  go to 5640
5643 continue
  k=0
5640 continue
  if(.not.(k.gt.0)) go to 5661
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9531
  go to 9100
9531 from=isto(sptr)
  sptr=sptr+1
  isto(env+4)=ndx1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  go to 5660
5661 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9532
  go to 9112
9532 from=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx1.gt.5)) go to 5671
  isto(ishenv+54)=31
  continue
  call synstp
  go to 5670
5671 continue
5670 continue
  ndx1=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
5660 continue
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5681
  isto(env+6)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+51)
  isto(ishenv+51)=3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9533
  go to 9101
9533 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+51)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5691
  isto(env+7)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+51)
  isto(ishenv+51)=4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9534
  go to 9101
9534 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+51)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5701
  isto(ishenv+54)=51
  continue
  call synstp
  go to 5700
5701 continue
5700 continue
  go to 5690
5691 continue
5690 continue
  go to 5680
5681 continue
5680 continue
  if(.not.(isto(env+6).eq.0 .and. ((ndx.ge.379 .and. ndx.le.391.and. ndx.ne.387).or. ndx.eq.413))) go to 5711
  isto(ishenv+54)=49
  continue
  call synstp
  go to 5710
5711 if(.not.(isto(env+6).gt.0.and. (ndx.eq.387 .or. ndx.eq.411))) go to 5712
  isto(ishenv+54)=73
  continue
  call synstp
  go to 5710
5712 if(.not.(isto(env+7).gt.0.and. ndx.eq.413)) go to 5713
  isto(ishenv+54)=76
  continue
  call synstp
  go to 5710
5713 continue
5710 continue
  go to 9500
9117 continue
  iptr=iptr+8
  ipn=isto(ishenv+44)
  isto(ipn)=env
  isto(ishenv+44)=env
  isto(env+0)=0
  isto(env+1)=ndx
  isto(env+2)=ndx3
  isto(env+3)=ndx4
  ptr13=ptr13+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  ndx1=0
  if(.not.(ndx.eq.325)) go to 5721
  isto(env+7)=0
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5731
  isto(ishenv+54)=52
  continue
  call synstp
  go to 5730
5731 continue
5730 continue
  go to 5720
5721 continue
  isto(env+7)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3101 if(.not.(lpflg.gt.0)) go to 3100
  ndx1=ndx1+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9535
  go to 9118
9535 from=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5741
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5740
5741 continue
  isto(env+0)=0
  lpflg=0
5740 continue
  go to 3101
3100 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
5720 continue
  isto(env+6)=ndx1
  ndx1=isto(sptr)
  sptr=sptr+1
  go to 9500
9118 continue
  ptr13=ptr13+1
  iptr=iptr+2
  mndx=279
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5751
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+51)
  isto(ishenv+51)=5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9536
  go to 9101
9536 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+51)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5750
5751 continue
  isto(env+1)=0
5750 continue
  go to 9500
9119 continue
  iptr=iptr+4
  isto(env+1)=dptr+1
  to=9200
  call putil2
  isto(env+0)=dptr -isto(env+1) +1
  to=9204
  call putil2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  ptr2=0
  mndx=79
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5761
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9537
  go to 9121
9537 from=isto(sptr)
  sptr=sptr+1
  go to 5760
5761 continue
5760 continue
  isto(env+2)=ptr1
  isto(env+3)=ptr2
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9120 continue
  iptr=iptr+10
  ipn=isto(ishenv+22)
  isto(ipn)=env
  isto(ishenv+22)=env
  isto(env+0)=0
  isto(env+1)=ndx
  isto(env+5)=isto(ishenv+17)
  isto(env+7)=dptr+1
  to=9200
  call putil2
  isto(env+6)=dptr -isto(env+7) +1
  to=9204
  call putil2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  ptr2=0
  mndx=79
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5771
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9538
  go to 9121
9538 from=isto(sptr)
  sptr=sptr+1
  go to 5770
5771 continue
5770 continue
  if(.not.(ndx.ge.9 .and. ndx.le.15.and. ptr2.gt.0)) go to 5781
  isto(ishenv+54)=66
  continue
  call synstp
  go to 5780
5781 continue
5780 continue
  isto(env+8)=ptr1
  isto(env+9)=ptr2
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9121 continue
  ptr1=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+51)
  isto(ishenv+51)=6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9539
  go to 9101
9539 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+51)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  mndx=115
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5791
  dptr=dptr-1
  mndx=115
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5801
  dptr=dptr+1
  go to 5800
5801 continue
5800 continue
  go to 5790
5791 continue
5790 continue
  if(.not.(mflg.gt.0)) go to 5811
  ptr2=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+51)
  isto(ishenv+51)=7
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9540
  go to 9101
9540 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+51)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5810
5811 continue
5810 continue
  mndx=81
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5821
  isto(ishenv+54)=54
  continue
  call synstp
  go to 5820
5821 continue
5820 continue
  go to 9500
9122 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3111 if(.not.(lpflg.gt.0)) go to 3110
  ptr1=iptr+1
  isto(ishenv+50)=59
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9541
  go to 9127
9541 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5831
  ptr3=ptr1
  go to 5830
5831 continue
  isto(ishenv+50)=60
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9542
  go to 9128
9542 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5841
  ptr4=ptr1
  go to 5840
5841 continue
  isto(ishenv+54)=55
  continue
  call synstp
5840 continue
5830 continue
  mndx=89
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5851
  lpflg=0
  go to 5850
5851 continue
5850 continue
  go to 3111
3110 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9123 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3121 if(.not.(lpflg.gt.0)) go to 3120
  ptr1=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+51)
  isto(ishenv+51)=8
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9543
  go to 9127
9543 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+51)=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5861
  ptr3=ptr1
  go to 5860
5861 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+51)
  isto(ishenv+51)=9
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9544
  go to 9128
9544 from=isto(sptr)
  sptr=sptr+1
  isto(ishenv+51)=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5871
  ptr4=ptr1
  go to 5870
5871 continue
  isto(ishenv+54)=55
  continue
  call synstp
5870 continue
5860 continue
  mndx=89
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5881
  lpflg=0
  go to 5880
5881 continue
5880 continue
  go to 3121
3120 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9124 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3131 if(.not.(lpflg.gt.0)) go to 3130
  ptr1=iptr+1
  isto(ishenv+50)=61
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9545
  go to 9129
9545 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5891
  ptr3=ptr1
  go to 5890
5891 continue
  isto(ishenv+50)=62
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9546
  go to 9130
9546 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5901
  ptr4=ptr1
  go to 5900
5901 continue
  isto(ishenv+54)=56
  continue
  call synstp
5900 continue
5890 continue
  mndx=89
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5911
  lpflg=0
  go to 5910
5911 continue
5910 continue
  go to 3131
3130 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9125 continue
  mndx=277
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5921
  mndx=105
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5931
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9547
  go to 9101
9547 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5930
5931 continue
  isto(ishenv+54)=57
  continue
  call synstp
5930 continue
  mflg=1
  go to 5920
5921 continue
5920 continue
  go to 9500
9126 continue
  mndx=279
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5941
  mndx=105
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5951
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9548
  go to 9101
9548 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5950
5951 continue
  isto(ishenv+54)=58
  continue
  call synstp
5950 continue
  mflg=1
  go to 5940
5941 continue
5940 continue
  go to 9500
9127 continue
  mndx=357
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5961
  mndx=105
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5971
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9549
  go to 9101
9549 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5970
5971 continue
  isto(ishenv+54)=59
  continue
  call synstp
5970 continue
  mflg=1
  go to 5960
5961 continue
5960 continue
  go to 9500
9128 continue
  mndx=359
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5981
  mndx=105
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5991
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9550
  go to 9101
9550 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5990
5991 continue
  isto(ishenv+54)=60
  continue
  call synstp
5990 continue
  mflg=1
  go to 5980
5981 continue
5980 continue
  go to 9500
9129 continue
  mndx=281
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6001
  mndx=105
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6011
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9551
  go to 9101
9551 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 6010
6011 continue
  isto(ishenv+54)=61
  continue
  call synstp
6010 continue
  mflg=1
  go to 6000
6001 continue
6000 continue
  go to 9500
9130 continue
  mndx=283
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6021
  mndx=105
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6031
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9552
  go to 9101
9552 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 6030
6031 continue
  isto(ishenv+54)=62
  continue
  call synstp
6030 continue
  mflg=1
  go to 6020
6021 continue
6020 continue
  go to 9500
end subroutine putil1
!
!     file: putil1.for
!
