!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file pstmt.f90
!

!
! subroutine pstmt.
!

subroutine pstmt
  use tacsto
  implicit none
  !  include  'tacsto.ftn'
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=0
  go to 9000
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, &
       9516, 9517, 9518, 9519, 9520, 9521, 9522, 9523, 9524, 0002, 9526, 9527, 9528, 9529, 9530, 9531, &
       9532, 9533, 9534, 9535, 9536, 9537, 9538, 9539, 9540, 9541, 9542, 9543, 9544), from- 9500
0002 stop 'invalid from reference in pstmt.'
910 stpflg=42
  stpi1=iptr
  stpi2=ilen-iptr
  continue
  call errstp
9000 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9501
  go to 9001
9501 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5001
  ptr1=iptr+1
  go to 5000
5001 continue
  ptr1=0
5000 continue
3001 if(.not.(mflg.eq.0)) go to 3000
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9502
  go to 9002
9502 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9001
9503 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5011
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5010
5011 continue
  isto(env+0)=0
5010 continue
  go to 3001
3000 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9001 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=dptr
  if(.not.(isto(ishenv+53).eq.1)) go to 5021
  mndx=169
  to=9206
  call putil2
  go to 5020
5021 if(.not.(isto(ishenv+53).eq.2)) go to 5022
  mndx=173
  to=9206
  call putil2
  go to 5020
5022 if(.not.(isto(ishenv+53).eq.3)) go to 5023
  mndx=207
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) go to 5031
  mndx=205
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) go to 5041
  mndx=203
  to=9206
  call putil2
  go to 5040
5041 continue
5040 continue
  go to 5030
5031 continue
5030 continue
  go to 5020
5023 if(.not.(isto(ishenv+53).eq.4)) go to 5024
  mndx=207
  to=9206
  call putil2
  go to 5020
5024 if(.not.(isto(ishenv+53).eq.5)) go to 5025
  mndx=211
  to=9206
  call putil2
  go to 5020
5025 if(.not.(isto(ishenv+53).eq.6)) go to 5026
  mndx=215
  to=9206
  call putil2
  go to 5020
5026 if(.not.(isto(ishenv+53).eq.7)) go to 5027
  mndx=219
  to=9206
  call putil2
  go to 5020
5027 if(.not.(isto(ishenv+53).eq.8)) go to 5028
  mndx=229
  to=9206
  call putil2
  go to 5020
5028 if(.not.(isto(ishenv+53).eq.9)) go to 5029
  mndx=237
  to=9206
  call putil2
  go to 5020
5029 continue
5020 continue
  dptr=isto(sptr)
  sptr=sptr+1
  go to 9500
9002 continue
  iptr=iptr+3
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=dptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  mndx=411
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5051
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5061
  if(.not.(isto(ishenv+52).gt.0)) go to 5071
  ndx=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9003
9504 from=isto(sptr)
  sptr=sptr+1
  go to 5070
5071 continue
  isto(ishenv+54)=63
  continue
  call synstp
5070 continue
  go to 5060
5061 continue
5060 continue
  go to 5050
5051 continue
5050 continue
  if(.not.(ndx.eq.0)) go to 5081
  dptr=ptr1
  mndx=247
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5091
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5101
  ndx=15
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9004
9505 from=isto(sptr)
  sptr=sptr+1
  go to 5100
5101 continue
5100 continue
  go to 5090
5091 continue
5090 continue
  if(.not.(ndx.eq.0)) go to 5111
  dptr=ptr1
  mndx=195
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5121
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5131
  if(.not.(isto(ishenv+52).eq.0)) go to 6251
  ndx=3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9014
9506 from=isto(sptr)
  sptr=sptr+1
  go to 6250
6251 continue
  isto(ishenv+54)=87
  continue
  call synstp
6250 continue
  go to 5130
5131 continue
5130 continue
  go to 5120
5121 continue
5120 continue
  if(.not.(ndx.eq.0)) go to 5141
  dptr=ptr1
  mndx=197
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5151
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5161
  if(.not.(isto(ishenv+52).eq.0)) go to 6261
  ndx=4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9507
  go to 9015
9507 from=isto(sptr)
  sptr=sptr+1
  go to 6260
6261 continue
  isto(ishenv+54)=88
  continue
  call synstp
6260 continue
  go to 5160
5161 continue
5160 continue
  go to 5150
5151 continue
5150 continue
  if(.not.(ndx.eq.0)) go to 5171
  dptr=ptr1
  mndx=199
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5181
  ndx=5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9508
  go to 9017
9508 from=isto(sptr)
  sptr=sptr+1
  go to 5180
5181 continue
  mndx=209
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5191
  ndx=6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9509
  go to 9018
9509 from=isto(sptr)
  sptr=sptr+1
  go to 5190
5191 continue
  mndx=213
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5201
  ndx=7
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9510
  go to 9019
9510 from=isto(sptr)
  sptr=sptr+1
  go to 5200
5201 continue
  mndx=217
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5211
  ndx=8
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9511
  go to 9022
9511 from=isto(sptr)
  sptr=sptr+1
  go to 5210
5211 continue
  mndx=221
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5221
  ndx=9
  if(.not.(isto(ishenv+63).eq.0)) go to 6241
  isto(ishenv+54)=75
  continue
  call synstp
  go to 6240
6241 continue
6240 continue
  go to 5220
5221 continue
  mndx=223
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5231
  ndx=10
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9512
  go to 9023
9512 from=isto(sptr)
  sptr=sptr+1
  go to 5230
5231 continue
  mndx=227
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5241
  ndx=11
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9513
  go to 9025
9513 from=isto(sptr)
  sptr=sptr+1
  go to 5240
5241 continue
  mndx=231
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5251
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6311
  ndx=12
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9514
  go to 9026
9514 from=isto(sptr)
  sptr=sptr+1
  go to 6310
6311 continue
6310 continue
  go to 5250
5251 continue
5250 continue
  if(.not.(ndx.eq.0)) go to 6321
  dptr=ptr1
  mndx=235
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5261
  ndx=13
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9544
  go to 9028
9544 from=isto(sptr)
  sptr=sptr+1
  go to 5260
5261 continue
  mndx=239
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5271
  if(.not.(isto(ishenv+52).eq.0)) go to 6271
  ndx=14
  continue
  call puse
  go to 6270
6271 continue
  isto(ishenv+54)=89
  continue
  call synstp
6270 continue
  go to 5270
5271 continue
  ndx=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9515
  go to 9005
9515 from=isto(sptr)
  sptr=sptr+1
5270 continue
5260 continue
  go to 6320
6321 continue
6320 continue
5240 continue
5230 continue
5220 continue
5210 continue
5200 continue
5190 continue
5180 continue
  go to 5170
5171 continue
5170 continue
  go to 5140
5141 continue
5140 continue
  go to 5110
5111 continue
5110 continue
  go to 5080
5081 continue
5080 continue
  env=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(env+1)=ndx
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9003 continue
  iptr=iptr+5
  ipn=isto(ishenv+38)
  isto(ipn)=env
  isto(ishenv+38)=env
  isto(env+0)=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  isto(env+3)=iptr+1
  isto(ishenv+50)=17
  to=9100
  call putil1
  isto(env+2)=ndx1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5281
  isto(ishenv+54)=21
  continue
  call synstp
  go to 5280
5281 continue
5280 continue
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5291
  isto(ishenv+54)=3
  continue
  call synstp
  go to 5290
5291 continue
5290 continue
  isto(env+4)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=25
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9004 continue
  iptr=iptr+4
  ipn=isto(ishenv+39)
  isto(ipn)=env
  isto(ishenv+39)=env
  isto(env+0)=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=21
  isto(ishenv+50)=63
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5301
  isto(ishenv+54)=68
  continue
  call synstp
  go to 5300
5301 continue
5300 continue
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5311
  isto(ishenv+54)=67
  continue
  call synstp
  go to 5310
5311 continue
5310 continue
  isto(env+3)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=64
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9005 continue
  iptr=iptr+3
  isto(env+0)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=8
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=26
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5321
  isto(ishenv+54)=22
  continue
  call synstp
  go to 5320
5321 continue
5320 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=0
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9516
  go to 9006
9516 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  isto(env+1)=ndx
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9006 continue
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=dptr
  mndx=243
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5331
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5341
  ndx=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9517
  go to 9008
9517 from=isto(sptr)
  sptr=sptr+1
  go to 5340
5341 continue
5340 continue
  go to 5330
5331 continue
5330 continue
  if(.not.(ndx.eq.0)) go to 5351
  dptr=ptr1
  mndx=245
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5361
  mndx=249
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5371
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5381
  ndx=4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9518
  go to 9010
9518 from=isto(sptr)
  sptr=sptr+1
  go to 5380
5381 continue
5380 continue
  go to 5370
5371 continue
5370 continue
  go to 5360
5361 continue
5360 continue
  go to 5350
5351 continue
5350 continue
  if(.not.(ndx.eq.0)) go to 5391
  dptr=ptr1
  mndx=247
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5401
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5411
  ndx=5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9519
  go to 9013
9519 from=isto(sptr)
  sptr=sptr+1
  go to 5410
5411 continue
5410 continue
  go to 5400
5401 continue
5400 continue
  go to 5390
5391 continue
5390 continue
  if(.not.(ndx.eq.0)) go to 5421
  dptr=ptr1
  ndx=1
  isto(ishenv+50)=27
  to=9101
  call putil1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9520
  go to 9007
9520 from=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.eq.1 .and. isto(ishenv+55).gt.0)) go to 5431
  isto(ishenv+54)=71
  continue
  call synstp
  go to 5430
5431 continue
5430 continue
  go to 5420
5421 continue
5420 continue
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9007 continue
  if(.not.(isto(env+3).gt.0)) go to 5441
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=env+4
  if(.not.(isto(env+0).eq.7)) go to 5471
  env=env+2
  env=isto(env+15)
  if(.not.(isto(env+0).eq.1)) go to 5481
  env=env+2
  k=isto(env+1)
  if(.not.(k.eq.393)) go to 5491
  ndx=3
  go to 5490
5491 if(.not.(k.eq.395)) go to 5492
  ndx=7
  go to 5490
5492 continue
5490 continue
  go to 5480
5481 continue
5480 continue
  go to 5470
5471 continue
5470 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 5440
5441 continue
5440 continue
  go to 9500
9008 continue
  iptr=iptr+3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=1
  isto(env+0)=iptr+1
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
  from=9521
  go to 9009
9521 from=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5501
  isto(env+0)=0
  lpflg=0
  go to 5500
5501 continue
  mndx=91
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5511
  ndx=1
  go to 5510
5511 continue
  mndx=93
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5521
  ndx=2
  go to 5520
5521 continue
  isto(ishenv+54)=23
  continue
  call synstp
5520 continue
5510 continue
  isto(env+0)=iptr+1
  env=iptr+1
5500 continue
  go to 3011
3010 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
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
  if(.not.(mflg.gt.0)) go to 5531
  to=9122
  call putil1
  if(.not.(isto(ishenv+55).gt.0)) go to 5541
  if(.not.(isto(ishenv+57).gt.0)) go to 5551
  isto(ishenv+54)=72
  continue
  call synstp
  go to 5550
5551 continue
5550 continue
  isto(ishenv+57)=isto(ishenv+58)
  go to 5540
5541 continue
5540 continue
  go to 5530
5531 continue
5530 continue
  isto(env+1)=ptr3
  isto(env+2)=ptr4
  ptr4=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  go to 9500
9009 continue
  iptr=iptr+5
  isto(env+1)=ndx
  isto(env+2)=0
  mndx=101
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5561
  isto(env+3)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=28
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=101
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5571
  isto(ishenv+54)=24
  continue
  call synstp
  go to 5570
5571 continue
5570 continue
  go to 5560
5561 continue
  isto(env+3)=0
5560 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=dptr
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5581
  mndx=91
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5591
  mndx=93
  to=9205
  call putil2
  go to 5590
5591 continue
5590 continue
  go to 5580
5581 continue
5580 continue
  dptr=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5601
  if(.not.(isto(ishenv+55).gt.0)) go to 5611
  isto(ishenv+56)=isto(ishenv+56)+1
  go to 5610
5611 continue
5610 continue
  isto(env+4)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=9
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=29
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  go to 5600
5601 continue
  isto(env+4)=0
5600 continue
  go to 9500
9010 continue
  iptr=iptr+7
  ipn=isto(ishenv+35)
  isto(ipn)=env
  isto(ishenv+35)=env
  isto(env+0)=0
  isto(env+2)=0
  if(.not.(isto(ishenv+55).gt.0)) go to 5621
  isto(ishenv+56)=isto(ishenv+56)+1
  go to 5620
5621 continue
5620 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  ndx1=0
  isto(env+3)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9522
  go to 9011
9522 from=isto(sptr)
  sptr=sptr+1
  isto(env+1)=ndx1
  ndx1=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  mndx=101
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5631
  isto(ishenv+54)=25
  continue
  call synstp
  go to 5630
5631 continue
5630 continue
  isto(env+4)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=10
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=30
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
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
  if(.not.(mflg.gt.0)) go to 5641
  to=9122
  call putil1
  if(.not.(isto(ishenv+55).gt.0)) go to 5651
  if(.not.(isto(ishenv+57).gt.0)) go to 5661
  isto(ishenv+54)=72
  continue
  call synstp
  go to 5660
5661 continue
5660 continue
  isto(ishenv+57)=isto(ishenv+58)
  go to 5650
5651 continue
5650 continue
  go to 5640
5641 continue
5640 continue
  isto(env+5)=ptr3
  isto(env+6)=ptr4
  ptr4=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  go to 9500
9011 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3021 if(.not.(lpflg.gt.0)) go to 3020
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9523
  go to 9012
9523 from=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5671
  isto(env+0)=0
  lpflg=0
  go to 5670
5671 continue
  mndx=91
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5681
  ndx=1
  go to 5680
5681 continue
  mndx=93
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5691
  ndx=2
  go to 5690
5691 continue
  isto(ishenv+54)=26
  continue
  call synstp
5690 continue
5680 continue
  isto(env+0)=iptr+1
  env=iptr+1
5670 continue
  go to 3021
3020 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9012 continue
  iptr=iptr+4
  isto(env+1)=ndx
  mndx=101
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5701
  isto(env+3)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=31
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=101
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5711
  isto(ishenv+54)=24
  continue
  call synstp
  go to 5710
5711 continue
5710 continue
  go to 5700
5701 continue
  isto(env+3)=0
5700 continue
  if(.not.(flg.eq.1)) go to 5721
  mndx=7
  go to 5720
5721 if(.not.(flg.eq.2)) go to 5722
  mndx=37
  go to 5720
5722 if(.not.(flg.eq.3)) go to 5723
  mndx=17
  go to 5720
5723 continue
5720 continue
  to=9205
  call putil2
  if(.not.(flg.eq.2 .and. mflg.eq.0)) go to 6231
  mndx=31
  to=9205
  call putil2
  go to 6230
6231 continue
6230 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  if(.not.(mflg.gt.0)) go to 5731
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  to=9212
  call putil2
  if(.not.(ndx.lt.0)) go to 5741
  ndx2=1
  go to 5740
5741 continue
  ndx2=ndx
5740 continue
  ndx=isto(sptr)
  sptr=sptr+1
  to=9204
  call putil2
  go to 5730
5731 continue
  ndx2=0
5730 continue
  isto(env+2)=ndx2
  if(.not.(ndx2.gt.ndx1)) go to 5751
  ndx1=ndx2
  go to 5750
5751 continue
5750 continue
  ndx2=isto(sptr)
  sptr=sptr+1
  go to 9500
9013 continue
  iptr=iptr+9
  ipn=isto(ishenv+34)
  isto(ipn)=env
  isto(ishenv+34)=env
  isto(env+0)=0
  isto(env+4)=0
  if(.not.(isto(ishenv+55).gt.0)) go to 5761
  isto(ishenv+56)=isto(ishenv+56)+1
  go to 5760
5761 continue
5760 continue
  isto(env+3)=1
  isto(env+5)=0
  isto(env+6)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=11
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=32
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5771
  isto(ishenv+54)=27
  continue
  call synstp
  go to 5770
5771 continue
5770 continue
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
  if(.not.(mflg.gt.0)) go to 5781
  to=9124
  call putil1
  if(.not.(isto(ishenv+55).gt.0)) go to 5791
  if(.not.(isto(ishenv+57).gt.0)) go to 5801
  isto(ishenv+54)=72
  continue
  call synstp
  go to 5800
5801 continue
5800 continue
  isto(ishenv+57)=isto(ishenv+58)
  go to 5790
5791 continue
5790 continue
  go to 5780
5781 continue
5780 continue
  isto(env+7)=ptr3
  isto(env+8)=ptr4
  ptr4=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  go to 9500
9014 continue
  iptr=iptr+8
  ipn=isto(ishenv+36)
  isto(ipn)=env
  isto(ishenv+36)=env
  isto(env+0)=0
  isto(env+3)=0
  if(.not.(isto(ishenv+55).gt.0)) go to 6291
  isto(ishenv+56)=isto(ishenv+56)+1
  go to 6290
6291 continue
6290 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  ndx1=0
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9524
  go to 9011
9524 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+1)
  k=1
3081 if(.not.(env.gt.0 .and. k.ne.0)) go to 3080
  k=isto(env+2)
  env=isto(env+0)
  go to 3081
3080 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(k.ne.0)) go to 6281
  isto(ishenv+54)=91
  continue
  call synstp
  go to 6280
6281 continue
6280 continue
  isto(env+2)=ndx1
  ndx1=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  mndx=101
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5811
  isto(ishenv+54)=25
  continue
  call synstp
  go to 5810
5811 continue
5810 continue
  isto(env+4)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=13
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=34
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
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
  if(.not.(mflg.gt.0)) go to 5821
  to=9124
  call putil1
  if(.not.(isto(ishenv+55).gt.0)) go to 5831
  if(.not.(isto(ishenv+57).gt.0)) go to 5841
  isto(ishenv+54)=72
  continue
  call synstp
  go to 5840
5841 continue
5840 continue
  isto(ishenv+57)=isto(ishenv+58)
  go to 5830
5831 continue
5830 continue
  go to 5820
5821 continue
5820 continue
  isto(env+6)=ptr3
  isto(env+7)=ptr4
  ptr4=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5851
  isto(ishenv+54)=30
  continue
  call synstp
  go to 5850
5851 continue
5850 continue
  isto(env+5)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=12
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=34
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9015 continue
  iptr=iptr+10
  ipn=isto(ishenv+37)
  isto(ipn)=env
  isto(ishenv+37)=env
  isto(env+0)=0
  isto(env+3)=0
  if(.not.(isto(ishenv+55).gt.0)) go to 5861
  isto(ishenv+56)=isto(ishenv+56)+1
  go to 5860
5861 continue
5860 continue
  isto(env+4)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=14
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=35
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=97
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5871
  isto(ishenv+54)=28
  continue
  call synstp
  go to 5870
5871 continue
5870 continue
  isto(env+5)=iptr+1
  ndx=15
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=36
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5881
  isto(ishenv+54)=29
  continue
  call synstp
  go to 5880
5881 continue
5880 continue
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
  if(.not.(mflg.gt.0)) go to 5891
  to=9124
  call putil1
  if(.not.(isto(ishenv+55).gt.0)) go to 5901
  if(.not.(isto(ishenv+57).gt.0)) go to 5911
  isto(ishenv+54)=72
  continue
  call synstp
  go to 5910
5911 continue
5910 continue
  isto(ishenv+57)=isto(ishenv+58)
  go to 5900
5901 continue
5900 continue
  go to 5890
5891 continue
5890 continue
  isto(env+6)=ptr3
  isto(env+7)=ptr4
  ptr4=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5921
  isto(ishenv+54)=32
  continue
  call synstp
  go to 5920
5921 continue
5920 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  isto(env+8)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9526
  go to 9016
9526 from=isto(sptr)
  sptr=sptr+1
  isto(env+1)=ndx1
  mndx=97
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 5931
  isto(ishenv+54)=33
  continue
  call synstp
  go to 5930
5931 continue
5930 continue
  isto(env+9)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9527
  go to 9016
9527 from=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ndx1
  ndx1=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9016 continue
  ndx1=0
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 5941
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9528
  go to 9011
9528 from=isto(sptr)
  sptr=sptr+1
  go to 5940
5941 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(env+0)=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9529
  go to 9012
9529 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
5940 continue
  go to 9500
9017 continue
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
3031 if(.not.(lpflg.gt.0)) go to 3030
  iptr=iptr+3
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=37
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=201
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) go to 5951
  isto(ishenv+54)=34
  continue
  call synstp
  go to 5950
5951 continue
5950 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+53)
  isto(ishenv+53)=3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9530
  go to 9000
9530 from=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(ishenv+53)=isto(sptr)
  sptr=sptr+1
  mndx=205
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5961
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5960
5961 continue
  isto(env+0)=0
  lpflg=0
5960 continue
  go to 3031
3030 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  mndx=203
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5971
  isto(env+0)=iptr+1
  env=iptr+1
  iptr=iptr+3
  isto(env+1)=0
  isto(env+0)=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+53)
  isto(ishenv+53)=4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9531
  go to 9000
9531 from=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(ishenv+53)=isto(sptr)
  sptr=sptr+1
  go to 5970
5971 continue
5970 continue
  env=isto(sptr)
  sptr=sptr+1
  mndx=207
  to=9206
  call putil2
  go to 9500
9018 continue
  iptr=iptr+2
  isto(env+0)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=38
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=217
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) go to 5981
  isto(ishenv+54)=35
  continue
  call synstp
  go to 5980
5981 continue
5980 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+53)
  isto(ishenv+53)=5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9532
  go to 9000
9532 from=isto(sptr)
  sptr=sptr+1
  isto(env+1)=ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(ishenv+53)=isto(sptr)
  sptr=sptr+1
  mndx=211
  to=9206
  call putil2
  go to 9500
9019 continue
  iptr=iptr+5
  ipn=isto(ishenv+41)
  isto(ipn)=env
  isto(ishenv+41)=env
  isto(env+0)=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=ptr5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr6
  ptr6=env
  isto(env+3)=iptr+1
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
  from=9533
  go to 9020
9533 from=isto(sptr)
  sptr=sptr+1
  mndx=213
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 5991
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5990
5991 continue
  isto(env+0)=0
  lpflg=0
5990 continue
  go to 3041
3040 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  mndx=217
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) go to 6001
  isto(ishenv+54)=36
  continue
  call synstp
  go to 6000
6001 continue
6000 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+53)
  isto(ishenv+53)=6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9534
  go to 9000
9534 from=isto(sptr)
  sptr=sptr+1
  isto(env+4)=ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(ishenv+53)=isto(sptr)
  sptr=sptr+1
  mndx=215
  to=9206
  call putil2
  isto(env+2)=ptr5 -ndx0
  ptr6=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  go to 9500
9020 continue
  ptr5=ptr5 +1
  iptr=iptr+7
  isto(env+5)=dptr+1
  to=9200
  call putil2
  isto(env+4)=dptr -isto(env+5)+1
  to=9204
  call putil2
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 6011
  isto(ishenv+54)=37
  continue
  call synstp
  go to 6010
6011 continue
6010 continue
  isto(env+6)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3051 if(.not.(lpflg.gt.0)) go to 3050
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9535
  go to 9021
9535 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=dptr
  mndx=217
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) go to 6021
  mndx=213
  to=9206
  call putil2
  go to 6020
6021 continue
6020 continue
  dptr=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 6031
  isto(env+0)=iptr+1
  env=iptr+1
  go to 6030
6031 continue
  isto(env+0)=0
  lpflg=0
6030 continue
  go to 3051
3050 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9021 continue
  iptr=iptr+4
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=39
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  isto(env+2)=0
  isto(env+3)=0
  mndx=251
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 6041
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=40
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=253
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) go to 6051
  isto(env+3)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=41
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  go to 6050
6051 continue
6050 continue
  go to 6040
6041 continue
6040 continue
  go to 9500
9022 continue
  iptr=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+53)
  isto(ishenv+53)=7
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+63)
  isto(ishenv+63)=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9536
  go to 9000
9536 from=isto(sptr)
  sptr=sptr+1
  isto(env+0)=ptr1
  isto(ishenv+63)=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(ishenv+53)=isto(sptr)
  sptr=sptr+1
  mndx=219
  to=9206
  call putil2
  go to 9500
9023 continue
  iptr=iptr+10
  ipn=isto(ishenv+42)
  isto(ipn)=env
  isto(ishenv+42)=env
  isto(env+0)=0
  mndx=255
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) go to 6061
  isto(ishenv+54)=69
  continue
  call synstp
  go to 6060
6061 continue
6060 continue
  isto(env+2)=dptr+1
  to=9200
  call putil2
  isto(env+1)=dptr -isto(env+2)+1
  to=9204
  call putil2
  mndx=79
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6071
  isto(env+3)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=65
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=81
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 6081
  isto(ishenv+54)=70
  continue
  call synstp
  go to 6080
6081 continue
6080 continue
  go to 6070
6071 continue
  isto(env+3)=0
6070 continue
  isto(ishenv+55)=0
  isto(ishenv+56)=0
  isto(ishenv+57)=0
  isto(env+4)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(env+1)=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3061 if(.not.(lpflg.gt.0)) go to 3060
  isto(ishenv+55)=isto(ishenv+55)+1
  isto(ishenv+58)=env
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9537
  go to 9024
9537 from=isto(sptr)
  sptr=sptr+1
  mndx=225
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) go to 6091
  isto(env+0)=iptr+1
  k=env
  env=iptr+1
  isto(env+1)=k
  go to 6090
6091 continue
  isto(env+0)=0
  lpflg=0
6090 continue
  go to 3061
3060 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  i3=env
  k=0
  if(.not.(isto(ishenv+57).eq.0.or. isto(ishenv+57).eq.env)) go to 6101
  i4=i3
  go to 6100
6101 continue
  i4=isto(ishenv+57)
  isto(env+0)=isto(ishenv+57)
  env=isto(ishenv+57)
  i1=isto(env+0)
  i2=isto(env+1)
  isto(env+1)=i3
  isto(env+0)=0
  env=i1
  isto(env+1)=i2
  if(.not.(i2.gt.0)) go to 6111
  env=i2
  isto(env+0)=i1
  go to 6110
6111 continue
  k=1
6110 continue
6100 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+5)=i4
  if(.not.(k.gt.0)) go to 6121
  isto(env+4)=i1
  go to 6120
6121 continue
6120 continue
  isto(env+7)=isto(ishenv+55)
  isto(env+8)=isto(ishenv+56)
  if(.not.(isto(ishenv+57).gt.0)) go to 6131
  isto(env+6)=1
  go to 6130
6131 continue
  isto(env+6)=0
6130 continue
  isto(ishenv+55)=0
  isto(ishenv+56)=0
  isto(ishenv+57)=0
  go to 9500
9024 continue
  iptr=iptr+6
  isto(env+5)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=dptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  mndx=195
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6141
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6151
  ndx=2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9538
  go to 9014
9538 from=isto(sptr)
  sptr=sptr+1
  go to 6150
6151 continue
6150 continue
  go to 6140
6141 continue
6140 continue
  if(.not.(ndx.eq.0)) go to 6161
  dptr=ptr1
  mndx=197
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6171
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) go to 6181
  ndx=3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9539
  go to 9015
9539 from=isto(sptr)
  sptr=sptr+1
  go to 6180
6181 continue
6180 continue
  go to 6170
6171 continue
6170 continue
  if(.not.(ndx.eq.0)) go to 6191
  dptr=ptr1
  ndx=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9540
  go to 9005
9540 from=isto(sptr)
  sptr=sptr+1
  go to 6190
6191 continue
6190 continue
  go to 6160
6161 continue
6160 continue
  env=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(env+4)=ndx
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9025 continue
  iptr=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+53)
  isto(ishenv+53)=8
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9541
  go to 9000
9541 from=isto(sptr)
  sptr=sptr+1
  isto(env+0)=ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(ishenv+53)=isto(sptr)
  sptr=sptr+1
  mndx=229
  to=9206
  call putil2
  go to 9500
9026 continue
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
3071 if(.not.(lpflg.gt.0)) go to 3070
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9542
  go to 9027
9542 from=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) go to 6201
  isto(env+0)=iptr+1
  env=iptr+1
  go to 6200
6201 continue
  isto(env+0)=0
  lpflg=0
6200 continue
  go to 3071
3070 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9027 continue
  iptr=iptr+4
  isto(env+2)=dptr+2
  isto(env+1)=0
  isto(env+3)=0
  mndx=75
  to=9208
  call putil2
  if(.not.(mflg.gt.0)) go to 6211
  to=9201
  call putil2
  isto(env+1)=dptr -isto(env+2)
  to=9204
  call putil2
  go to 6210
6211 continue
  mndx=77
  to=9208
  call putil2
  if(.not.(mflg.gt.0)) go to 6221
  to=9202
  call putil2
  isto(env+1)=dptr -isto(env+2)
  to=9204
  call putil2
  go to 6220
6221 continue
  isto(env+2)=0
  isto(env+3)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=42
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
6220 continue
6210 continue
  go to 9500
9028 continue
  iptr=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+53)
  isto(ishenv+53)=9
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9543
  go to 9000
9543 from=isto(sptr)
  sptr=sptr+1
  isto(env+0)=ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(ishenv+53)=isto(sptr)
  sptr=sptr+1
  mndx=237
  to=9206
  call putil2
  go to 9500
end subroutine pstmt

!
! end of file pstmt.f90
!
