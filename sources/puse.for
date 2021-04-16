!-*- mode: fortran; syntax: ansi-fortran-90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: puse.for
!
!
!     subroutine puse.
!
subroutine puse
  include 'tacsto.ins'
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=0
  goto 9000
9500 if(.not.(from.eq.0)) goto 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 goto (0002, 9502, 9503, 9504, 9505, 9506 , 9507, 9508, 9509, 9510, 9511 , 9512, 9513), from-9500
0002 stop 'invalid from reference in puse.'
910 stpflg=42
  stpi1=iptr
  stpi2=ilen-iptr
  continue
  call errstp
9000 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr3
  ptr3=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr4
  ptr4=0
  iptr=iptr+12
  ipn=isto(ishenv+25)
  isto(ipn)=env
  isto(ishenv+25)=env
  isto(env+0)=0
  isto(env+5)=dptr+1
  to=9200
  call putil2
  isto(env+4)=dptr -isto(env+5)+1
  to=9204
  call putil2
  mndx=255
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5071
  isto(ishenv+54)=38
  continue
  call synstp
  goto 5070
5071 continue
5070 continue
  isto(env+7)=dptr+1
  to=9200
  call putil2
  isto(env+6)=dptr -isto(env+7)+1
  to=9204
  call putil2
  mndx=79
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5081
  isto(env+8)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=43
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=81
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5091
  isto(ishenv+54)=39
  continue
  call synstp
  goto 5090
5091 continue
5090 continue
  goto 5080
5081 continue
  isto(env+8)=0
5080 continue
  i3=isto(env+4)
  i4=isto(env+5)
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=isto(env+26)
3021 if(.not.(env.gt.0 .and. ndx.eq.0)) goto 3020
  i1=isto(env+2)
  if(.not.(i1.eq.i3)) goto 5101
  i=0
  i2=isto(env+3)
3031 if(.not.(i.lt.i3.and. csto(i4+i).eq.csto(i2+i))) goto 3030
  i=i+1
  goto 3031
3030 continue
  if(.not.(i.eq.i3)) goto 5111
  ndx=isto(env+4)
  ptr1=isto(env+5)
  goto 5110
5111 continue
5110 continue
  goto 5100
5101 continue
5100 continue
  env=isto(env+0)
  goto 3021
3020 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.eq.0)) goto 5121
  stpflg=98
  stpl1=i3
  stpc1=i4
  continue
  call errstp
  goto 5120
5121 continue
5120 continue
  isto(env+1)=ndx
  isto(env+3)=ptr1
  isto(ishenv+12)=ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  ptr8=ptr8+1
  isto(env+2)=ptr8
  isto(env+11)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=lpflg
  lpflg=1
3041 if(.not.(lpflg.gt.0)) goto 3040
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9502
  goto 9002
9502 from=isto(sptr)
  sptr=sptr+1
  mndx=241
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5131
  isto(env+0)=iptr+1
  env=iptr+1
  goto 5130
5131 continue
  isto(env+0)=0
  lpflg=0
5130 continue
  goto 3041
3040 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  isto(ishenv+12)=0
  isto(env+9)=ptr3
  isto(env+10)=ptr4
  ptr4=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  goto 9500
9002 continue
  iptr=iptr+3
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr1
  ptr1=dptr
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  mndx=259
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5141
  mndx=263
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5151
  ndx=2
  goto 5150
5151 continue
  ndx=1
5150 continue
  mndx=265
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5161
  mndx=255
  to=9206
  call putil2
  goto 5160
5161 continue
5160 continue
  goto 5140
5141 continue
  mndx=261
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5171
  mndx=263
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5181
  ndx=4
  goto 5180
5181 continue
  ndx=3
5180 continue
  mndx=265
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5191
  mndx=251
  to=9206
  call putil2
  goto 5190
5191 continue
5190 continue
  goto 5170
5171 continue
5170 continue
5140 continue
  if(.not.(mflg.gt.0)) goto 5201
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9503
  goto 9003
9503 from=isto(sptr)
  sptr=sptr+1
  goto 5200
5201 continue
  ndx=0
  dptr=ptr1
  mndx=181
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5211
  ndx=5
  goto 5210
5211 continue
  mndx=183
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5221
  ndx=6
  goto 5220
5221 continue
  mndx=177
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5231
  ndx=7
  goto 5230
5231 continue
  mndx=185
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5241
  ndx=8
  goto 5240
5241 continue
  mndx=187
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5251
  ndx=9
  goto 5250
5251 continue
  mndx=189
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5261
  ndx=10
  goto 5260
5261 continue
5260 continue
5250 continue
5240 continue
5230 continue
5220 continue
5210 continue
  if(.not.(ndx.eq.0)) goto 5271
  isto(ishenv+54)=40
  continue
  call synstp
  goto 5270
5271 continue
5270 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9504
  goto 9004
9504 from=isto(sptr)
  sptr=sptr+1
5200 continue
  env=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  isto(env+1)=ndx
  ndx=isto(sptr)
  sptr=sptr+1
  goto 9500
9003 continue
  iptr=iptr+7
  if(.not.(ndx.lt.3)) goto 5281
  ipn=isto(ishenv+26)
  isto(ipn)=env
  isto(ishenv+26)=env
  goto 5280
5281 continue
  ipn=isto(ishenv+27)
  isto(ipn)=env
  isto(ishenv+27)=env
5280 continue
  isto(env+0)=0
  isto(env+1)=ndx
  isto(env+3)=dptr+1
  to=9200
  call putil2
  isto(env+2)=dptr -isto(env+3)+1
  to=9204
  call putil2
  mndx=79
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5291
  isto(env+4)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=44
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=81
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5301
  isto(ishenv+54)=41
  continue
  call synstp
  goto 5300
5301 continue
5300 continue
  goto 5290
5291 continue
  isto(env+4)=0
5290 continue
  mndx=267
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5311
  isto(ishenv+54)=42
  continue
  call synstp
  goto 5310
5311 continue
5310 continue
  isto(env+5)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=45
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  goto 9500
9004 continue
  iptr=iptr+1
  isto(env+0)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=lpflg
  lpflg=1
3051 if(.not.(lpflg.gt.0)) goto 3050
  if(.not.(ndx.eq.5)) goto 5321
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9505
  goto 9006
9505 from=isto(sptr)
  sptr=sptr+1
  goto 5320
5321 if(.not.(ndx.eq.6)) goto 5322
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9506
  goto 9007
9506 from=isto(sptr)
  sptr=sptr+1
  goto 5320
5322 if(.not.(ndx.eq.7)) goto 5323
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9507
  goto 9008
9507 from=isto(sptr)
  sptr=sptr+1
  goto 5320
5323 if(.not.(ndx.eq.8)) goto 5324
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9508
  goto 9009
9508 from=isto(sptr)
  sptr=sptr+1
  goto 5320
5324 if(.not.(ndx.eq.9)) goto 5325
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9509
  goto 9010
9509 from=isto(sptr)
  sptr=sptr+1
  goto 5320
5325 if(.not.(ndx.eq.10)) goto 5326
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9510
  goto 9011
9510 from=isto(sptr)
  sptr=sptr+1
  goto 5320
5326 continue
5320 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9511
  goto 9005
9511 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) goto 5331
  if(.not.(ndx.eq.8)) goto 5591
  isto(env+2)=iptr+1
  goto 5590
5591 continue
  isto(env+0)=iptr+1
5590 continue
  env=iptr+1
  goto 5330
5331 continue
  if(.not.(ndx.eq.8)) goto 5601
  isto(env+2)=0
  goto 5600
5601 continue
  isto(env+0)=0
5600 continue
  lpflg=0
5330 continue
  goto 3051
3050 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  goto 9500
9005 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=dptr
  mndx=241
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5341
  mndx=181
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5351
  mndx=183
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5361
  mndx=177
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5371
  mndx=185
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5381
  mndx=187
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5391
  mndx=189
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5401
  mndx=259
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5411
  mndx=263
  to=9205
  call putil2
  mndx=265
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5421
  mndx=255
  to=9206
  call putil2
  goto 5420
5421 continue
5420 continue
  goto 5410
5411 continue
  mndx=261
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5431
  mndx=263
  to=9205
  call putil2
  mndx=265
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5441
  mndx=251
  to=9206
  call putil2
  goto 5440
5441 continue
5440 continue
  goto 5430
5431 continue
5430 continue
5410 continue
  goto 5400
5401 continue
5400 continue
  goto 5390
5391 continue
5390 continue
  goto 5380
5381 continue
5380 continue
  goto 5370
5371 continue
5370 continue
  goto 5360
5361 continue
5360 continue
  goto 5350
5351 continue
5350 continue
  goto 5340
5341 continue
5340 continue
  dptr=isto(sptr)
  sptr=sptr+1
  goto 9500
9006 continue
  iptr=iptr+3
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx
  ndx=16
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=46
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=isto(ishenv+17)
  isto(ishenv+17)=isto(ishenv+12)
  to=9120
  call putil1
  isto(ishenv+17)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5451
  isto(ishenv+54)=43
  continue
  call synstp
  goto 5450
5451 continue
5450 continue
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=47
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  goto 9500
9007 continue
  iptr=iptr+3
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx
  ndx=17
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=48
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5461
  isto(ishenv+54)=44
  continue
  call synstp
  goto 5460
5461 continue
5460 continue
  isto(env+2)=iptr+1
  ndx=18
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=49
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=isto(ishenv+17)
  isto(ishenv+17)=isto(ishenv+12)
  to=9120
  call putil1
  isto(ishenv+17)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  goto 9500
9008 continue
  iptr=iptr+3
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx
  ndx=19
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=50
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=isto(ishenv+17)
  isto(ishenv+17)=isto(ishenv+12)
  to=9120
  call putil1
  isto(ishenv+17)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5471
  isto(ishenv+54)=45
  continue
  call synstp
  goto 5470
5471 continue
5470 continue
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=51
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  goto 9500
9009 continue
  iptr=iptr+6
  ipn=isto(ishenv+40)
  isto(ipn)=env
  isto(ishenv+40)=env
  isto(env+0)=0
  sptr=sptr-1
  isto(sptr)=ptr1
  ptr1=dptr
  mndx=411
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5481
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5491
  isto(ishenv+54)=46
  continue
  call synstp
  goto 5490
5491 continue
5490 continue
  goto 5480
5481 continue
  dptr=ptr1
  ptr1=0
5480 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx
  ndx=3
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx1
  isto(env+4)=iptr+1
  isto(ishenv+50)=18
  to=9100
  call putil1
  isto(env+3)=ndx1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  if(.not.(ptr1.gt.0)) goto 5611
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5501
  isto(ishenv+54)=46
  continue
  call synstp
  goto 5500
5501 continue
5500 continue
  goto 5610
5611 continue
5610 continue
  mndx=103
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5511
  isto(ishenv+54)=46
  continue
  call synstp
  goto 5510
5511 continue
5510 continue
  isto(env+5)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=52
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  goto 9500
9010 continue
  iptr=iptr+3
  isto(env+2)=iptr+1
  isto(ishenv+50)=53
  to=9127
  call putil1
  if(.not.(mflg.gt.0)) goto 5521
  isto(env+1)=1
  ptr3=isto(env+2)
  goto 5520
5521 continue
  isto(ishenv+50)=54
  to=9128
  call putil1
  if(.not.(mflg.gt.0)) goto 5531
  isto(env+1)=2
  ptr4=isto(env+2)
  goto 5530
5531 continue
  isto(ishenv+54)=19
  continue
  call synstp
5530 continue
5520 continue
  goto 9500
9011 continue
  iptr=iptr+3
  mndx=191
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5541
  isto(ishenv+54)=20
  continue
  call synstp
  goto 5540
5541 continue
5540 continue
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5551
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9512
  goto 9012
9512 from=isto(sptr)
  sptr=sptr+1
  mndx=105
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5561
  isto(ishenv+54)=20
  continue
  call synstp
  goto 5560
5561 continue
5560 continue
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=55
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  goto 5550
5551 continue
  isto(env+2)=iptr+1
  isto(ishenv+50)=56
  to=9126
  call putil1
  if(.not.(mflg.eq.0)) goto 5571
  isto(ishenv+54)=20
  continue
  call synstp
  goto 5570
5571 continue
5570 continue
  isto(env+1)=0
5550 continue
  goto 9500
9012 continue
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=lpflg
  lpflg=1
3061 if(.not.(lpflg.gt.0)) goto 3060
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9513
  goto 9013
9513 from=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5581
  isto(env+0)=iptr+1
  env=iptr+1
  goto 5580
5581 continue
  isto(env+0)=0
  lpflg=0
5580 continue
  goto 3061
3060 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  goto 9500
9013 continue
  iptr=iptr+2
  isto(env+1)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx
  ndx=20
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=57
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=isto(ishenv+17)
  isto(ishenv+17)=isto(ishenv+12)
  to=9120
  call putil1
  isto(ishenv+17)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  goto 9500
end subroutine puse
!
!     end of file: puse.for
!
