!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: putil2.for
!
!
!     subroutine putil2.
!
subroutine putil2
  include  'tacsto.ftn'
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=0
  go to (9200, 9201 , 9202, 9203, 9204, 9205, 9206 , 9207, 9208, 0002, 9210, 9211 , 9212, 9213, 9214), to-9199
0002 stop 'invalid "to" reference in "putil2".'
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501, 9502, 9503, 9504, 9505, 9506, 9507, 0003, 0003, 0003, 0003, 0003, 9513, 9514, 9515, 9516, 9517, 9518, 9519,&
          9520, 9521, 9522, 9523, 0003, 0003, 0003, 0003, 0003, 0003, 0003, 9531, 9532, 9533, 9534, 9535, 9536, 9537, 9538),&
          from-9500
0003 stop 'invalid "from" reference in "putil2".'
910 stpflg=42
  stpi1=iptr
  stpi2=ilen-iptr
  continue
  call errstp
9200 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9501
  go to 9210
9501 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5001
  isto(ishenv+54)=53
  continue
  call synstp
  go to 5000
5001 continue
5000 continue
3001 if(.not.(mflg.gt.0)) go to 3000
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9502
  go to 9210
9502 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5011
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9211
9503 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5021
  mndx=73
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9208
9504 from=isto(sptr)
  sptr=sptr+1
  go to 5020
5021 continue
5020 continue
  go to 5010
5011 continue
5010 continue
  go to 3001
3000 continue
  go to 9500
9201 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3011 if(.not.(lpflg.gt.0)) go to 3010
  mndx=75
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9208
9505 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5031
  lpflg=0
  go to 5030
5031 continue
  dptr=dptr+1
5030 continue
  go to 3011
3010 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  go to 9500
9202 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3021 if(.not.(lpflg.gt.0)) go to 3020
  mndx=77
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9208
9506 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5041
  lpflg=0
  go to 5040
5041 continue
  dptr=dptr+1
5040 continue
  go to 3021
3020 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  go to 9500
9203 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=-1
  mflg=1
3031 if(.not.(mflg.gt.0)) go to 3030
  ndx=ndx+1
  mndx=121
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9507
  go to 9208
9507 from=isto(sptr)
  sptr=sptr+1
  go to 3031
3030 continue
  if(.not.(ndx.gt.0)) go to 5101
  mflg=1
  go to 5100
5101 continue
  mflg=0
5100 continue
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9204 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=mflg
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9513
  go to 9203
9513 from=isto(sptr)
  sptr=sptr+1
  mflg=isto(sptr)
  sptr=sptr+1
  go to 9500
9205 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9514
  go to 9208
9514 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5111
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9515
  go to 9204
9515 from=isto(sptr)
  sptr=sptr+1
  go to 5110
5111 continue
5110 continue
  go to 9500
9206 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=dptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9516
  go to 9208
9516 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5121
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9517
  go to 9203
9517 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5131
  dptr=ptr1
  go to 5130
5131 continue
5130 continue
  go to 5120
5121 continue
5120 continue
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9207 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=dptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9518
  go to 9208
9518 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5141
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9519
  go to 9203
9519 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5151
  mndx=83
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9520
  go to 9208
9520 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5161
  mndx=79
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9521
  go to 9208
9521 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5171
  mndx=91
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9522
  go to 9208
9522 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5181
  mndx=93
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9523
  go to 9208
9523 from=isto(sptr)
  sptr=sptr+1
  go to 5180
5181 continue
5180 continue
  go to 5170
5171 continue
5170 continue
  go to 5160
5161 continue
5160 continue
  go to 5150
5151 continue
5150 continue
  go to 5140
5141 continue
5140 continue
  dptr=isto(sptr)
  sptr=sptr+1
  go to 9500
9208 continue
  j=base1 +mndx
  k=base2 +isto(j) -1
  j=isto(j+1)
  i=1
3041 if(.not.(i.le.j .and. csto(k+i).eq.csto(dptr+i))) go to 3040
  i=i+1
  go to 3041
3040 continue
  if(.not.(i.gt.j)) go to 5191
  mflg=1
  dptr=dptr+j
  go to 5190
5191 continue
  mflg=0
5190 continue
  go to 9500
9210 continue
  k=base2 +isto(base1+1) -1
  i=0
  mflg=0
3081 if(.not.(i.lt.26 .and. mflg.eq.0)) go to 3080
  i=i+1
  if(.not.(csto(k+i).eq.csto(dptr+1))) go to 5241
  mflg=1
  go to 5240
5241 continue
5240 continue
  go to 3081
3080 continue
  if(.not.(mflg.gt.0)) go to 5251
  dptr=dptr+1
  go to 5250
5251 continue
5250 continue
  go to 9500
9211 continue
  k=base2 +isto(base1+53) -1
  i=0
  mflg=0
3091 if(.not.(i.lt.10 .and. mflg.eq.0)) go to 3090
  i=i+1
  if(.not.(csto(k+i).eq.csto(dptr+1))) go to 5261
  mflg=1
  go to 5260
5261 continue
5260 continue
  go to 3091
3090 continue
  if(.not.(mflg.gt.0)) go to 5271
  dptr=dptr+1
  go to 5270
5271 continue
5270 continue
  go to 9500
9212 continue
  k=base2 +isto(base1+53)
  ndx=-1
  i1=0
3101 if(.not.(ndx.lt.9 .and. i1.eq.0)) go to 3100
  ndx=ndx+1
  if(.not.(csto(k+ndx).eq.csto(dptr+1))) go to 5281
  i1=1
  go to 5280
5281 continue
5280 continue
  go to 3101
3100 continue
  if(.not.(i1.gt.0)) go to 5291
  dptr=dptr+1
  go to 5290
5291 continue
  ndx=-1
5290 continue
  go to 9500
9213 continue
  k=base2 +isto(base1+53)
  i=-1
  a=-one
  mflg=0
3111 if(.not.(i.lt.9 .and. mflg.eq.0)) go to 3110
  i=i+1
  a=a +one
  if(.not.(csto(k+i).eq.csto(dptr+1))) go to 5301
  mflg=1
  go to 5300
5301 continue
5300 continue
  go to 3111
3110 continue
  if(.not.(mflg.gt.0)) go to 5311
  dptr=dptr+1
  rptr=rptr+1
  rsto(rptr)=a
  go to 5310
5311 continue
5310 continue
  go to 9500
9214 continue
  rptr=rptr+4
  rsto(rptr)=zero
  rsto(rptr-1)=one
  rsto(rptr-2)=one
  rsto(rptr-3)=zero
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3121 if(.not.(lpflg.gt.0)) go to 3120
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9531
  go to 9213
9531 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5321
  a=rsto(rptr)
  rptr=rptr-1
  rsto(rptr)=10.0d0 *rsto(rptr) +a
  go to 5320
5321 continue
  lpflg=0
5320 continue
  go to 3121
3120 continue
  mndx=113
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9532
  go to 9208
9532 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5331
  lpflg=1
3131 if(.not.(lpflg.gt.0)) go to 3130
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9533
  go to 9213
9533 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5341
  a=rsto(rptr)
  rptr=rptr-1
  rsto(rptr-1)=0.1 *rsto(rptr-1)
  rsto(rptr)=rsto(rptr) +rsto(rptr-1) *a
  go to 5340
5341 continue
  lpflg=0
5340 continue
  go to 3131
3130 continue
  go to 5330
5331 continue
5330 continue
  mndx=9
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9534
  go to 9208
9534 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5351
  mndx=91
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9535
  go to 9208
9535 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) go to 5361
  mndx=93
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9536
  go to 9208
9536 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5371
  rsto(rptr-2)=-one
  go to 5370
5371 continue
5370 continue
  go to 5360
5361 continue
5360 continue
  lpflg=1
3141 if(.not.(lpflg.gt.0)) go to 3140
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9537
  go to 9213
9537 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) go to 5381
  a=rsto(rptr)
  rptr=rptr-1
  rsto(rptr-3)=10.0d0 *rsto(rptr-3) +a
  go to 5380
5381 continue
  lpflg=0
5380 continue
  go to 3141
3140 continue
  go to 5350
5351 continue
5350 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9538
  go to 9204
9538 from=isto(sptr)
  sptr=sptr+1
  b=dlog10(rinf)
  c=dlog10(rmargn)
  e=rsto(rptr-2) *rsto(rptr-3)
  if(.not.(e.ge.b)) go to 5391
  a=rinf
  go to 5390
5391 if(.not.(e.le.c)) go to 5392
  a=zero
  go to 5390
5392 continue
  a=rsto(rptr)*(10.0d0**e)
5390 continue
  if(.not.(flg1.gt.0)) go to 5401
  a=-a
  go to 5400
5401 continue
5400 continue
  rptr=rptr-3
  rsto(rptr)=a
  go to 9500
end subroutine putil2
!
!     end of file: putil2.for
!
