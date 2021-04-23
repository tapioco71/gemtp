!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: ptacs.for
!
!
!     subroutine ptacs.
!
subroutine ptacs
  include  'tacsto.ftn'
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=0
  goto 9000
9500 if(.not.(from.eq.0)) goto 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 goto (9501 , 9502, 9503, 9504, 9505, 9506 , 9507, 9508, 9509, 9510, 9511 ), from-9500
  stop 'invalid "from" reference in "ptacs".'
910 stpflg=42
  stpi1=iptr
  stpi2=ilen-iptr
  continue
  call errstp
9000 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr6
  ptr6=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr7
  ptr7=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr8
  ptr8=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr10
  ptr10=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr14
  ptr14=0
  isto(ishenv+13)=iptr+1
  env=iptr+1
  iptr=iptr+5
  isto(env+1)=0
  isto(env+3)=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=lpflg
  to=9204
  call putil2
  mndx=141
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5001
  isto(env+2)=0
  isto(env+4)=0
  lpflg=0
  goto 5000
5001 continue
  isto(ishenv+16)=0
  isto(ishenv+12)=0
  datenv=iptr+1
  isto(ishenv+17)=iptr+1
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  i=0
3001 if(.not.(i.lt.49)) goto 3000
  i=i+1
  isto(iptr+i)=0
  goto 3001
3000 continue
  iptr=iptr+49
  isto(ishenv+22)=env+2
  isto(ishenv+23)=env+3
  isto(ishenv+24)=env+26
  isto(ishenv+25)=env+28
  isto(ishenv+26)=env+39
  isto(ishenv+27)=env+40
  isto(ishenv+30)=env+10
  isto(ishenv+31)=env+36
  isto(ishenv+40)=env+21
  isto(ishenv+44)=env+31
  isto(ishenv+45)=env+35
  env=isto(sptr)
  sptr=sptr+1
  isto(ishenv+48)=env+1
  isto(env+4)=iptr+1
  lpflg=1
5000 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
3011 if(.not.(lpflg.gt.0)) goto 3010
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9501
  goto 9001
9501 from=isto(sptr)
  sptr=sptr+1
  mndx=141
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5011
  isto(env+0)=iptr+1
  env=iptr+1
  goto 5010
5011 continue
  isto(env+0)=0
  lpflg=0
5010 continue
  goto 3011
3010 continue
  env=isto(sptr)
  sptr=sptr+1
  lpflg=isto(sptr)
  sptr=sptr+1
  k=isto(env+2)
  if(.not.(k.gt.0)) goto 5021
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=isto(env+2)
  isto(env+25)=ptr7
  isto(env+27)=ptr8
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9502
  goto 9008
9502 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  goto 5020
5021 continue
5020 continue
  isto(env+0)=ptr14
  ptr14=isto(sptr)
  sptr=sptr+1
  ptr10=isto(sptr)
  sptr=sptr+1
  ptr8=isto(sptr)
  sptr=sptr+1
  ptr7=isto(sptr)
  sptr=sptr+1
  ptr6=isto(sptr)
  sptr=sptr+1
  goto 9500
9001 continue
  iptr=iptr+3
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  mndx=181
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5031
  ndx=1
  goto 5030
5031 continue
  mndx=183
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5041
  ndx=2
  goto 5040
5041 continue
  mndx=143
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5051
  ndx=3
  goto 5050
5051 continue
5050 continue
5040 continue
5030 continue
  if(.not.(ndx.gt.0)) goto 5061
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9503
  goto 9002
9503 from=isto(sptr)
  sptr=sptr+1
  goto 5060
5061 continue
  mndx=147
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5071
  ndx=4
  to=9003
  call pmodl
  goto 5070
5071 continue
  mndx=239
  to=9206
  call putil2
  if(.not.(mflg.gt.0)) goto 5081
  ndx=5
  continue
  call puse
  goto 5080
5081 continue
  isto(ishenv+54)=74
  continue
  call synstp
5080 continue
5070 continue
5060 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+1)=ndx
  ndx=isto(sptr)
  sptr=sptr+1
  goto 9500
9002 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=lpflg
  lpflg=1
3021 if(.not.(lpflg.gt.0)) goto 3020
  if(.not.(ndx.eq.1)) goto 5091
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9504
  goto 9004
9504 from=isto(sptr)
  sptr=sptr+1
  goto 5090
5091 if(.not.(ndx.eq.2)) goto 5092
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9505
  goto 9005
9505 from=isto(sptr)
  sptr=sptr+1
  goto 5090
5092 if(.not.(ndx.eq.3)) goto 5093
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9506
  goto 9006
9506 from=isto(sptr)
  sptr=sptr+1
  goto 5090
5093 continue
5090 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9507
  goto 9003
9507 from=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.gt.0)) goto 5101
  lpflg=0
  isto(env+1)=0
  goto 5100
5101 continue
  isto(env+1)=iptr+1
  env=iptr+1
5100 continue
  goto 3021
3020 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  goto 9500
9003 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=dptr
  mndx=181
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5111
  mndx=183
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5121
  mndx=143
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5131
  mndx=147
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5141
  mndx=239
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5151
  mndx=141
  to=9206
  call putil2
  goto 5150
5151 continue
5150 continue
  goto 5140
5141 continue
5140 continue
  goto 5130
5131 continue
5130 continue
  goto 5120
5121 continue
5120 continue
  goto 5110
5111 continue
5110 continue
  dptr=isto(sptr)
  sptr=sptr+1
  goto 9500
9004 continue
  iptr=iptr+7
  ipn=isto(ishenv+30)
  isto(ipn)=env
  isto(ishenv+30)=env
  isto(env+0)=0
  isto(env+4)=dptr+1
  to=9200
  call putil2
  isto(env+3)=dptr-isto(env+4)+1
  to=9204
  call putil2
  mndx=87
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5161
  isto(ishenv+54)=80
  continue
  call synstp
  goto 5160
5161 continue
5160 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx
  ndx=397
  mflg=0
3031 if(.not.(ndx.lt.409 .and. mflg.eq.0)) goto 3030
  ndx=ndx+2
  mndx=ndx
  to=9205
  call putil2
  if(.not.(ndx.eq.401 .and. mflg.gt.0)) goto 5381
  sptr=sptr-1
  isto(sptr)=dptr
  mndx=83
  to=9205
  call putil2
  dptr=isto(sptr)
  sptr=sptr+1
  if(.not.(mflg.eq.0)) goto 5391
  dptr=dptr-1
  goto 5390
5391 continue
5390 continue
  goto 5380
5381 continue
5380 continue
  goto 3031
3030 continue
  if(.not.(mflg.eq.0)) goto 5171
  isto(ishenv+54)=81
  continue
  call synstp
  goto 5170
5171 continue
5170 continue
  ndx=(ndx-397)/2
  isto(env+2)=ndx
  ndx=isto(sptr)
  sptr=sptr+1
  mndx=83
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5181
  isto(ishenv+54)=82
  continue
  call synstp
  goto 5180
5181 continue
5180 continue
  isto(env+6)=dptr+1
  to=9200
  call putil2
  isto(env+5)=dptr-isto(env+6)+1
  if(.not.(isto(env+5).gt.6)) goto 5191
  isto(ishenv+54)=85
  continue
  call synstp
  goto 5190
5191 continue
5190 continue
  to=9204
  call putil2
  mndx=85
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5201
  isto(ishenv+54)=83
  continue
  call synstp
  goto 5200
5201 continue
5200 continue
  mndx=89
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5211
  isto(ishenv+54)=84
  continue
  call synstp
  goto 5210
5211 continue
5210 continue
  goto 9500
9005 continue
  iptr=iptr+4
  ipn=isto(ishenv+31)
  isto(ipn)=env
  isto(ishenv+31)=env
  isto(env+0)=0
  isto(env+3)=dptr+1
  to=9200
  call putil2
  isto(env+2)=dptr-isto(env+3)+1
  if(.not.(isto(env+2).gt.6)) goto 5361
  isto(ishenv+54)=78
  continue
  call synstp
  goto 5360
5361 continue
5360 continue
  to=9204
  call putil2
  goto 9500
9006 continue
  iptr=iptr+7
  ipn=isto(ishenv+48)
  isto(ipn)=env
  isto(ishenv+48)=env
  isto(env+0)=0
  ptr14=ptr14+1
  isto(env+3)=iinf
  isto(env+2)=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9508
  goto 9007
9508 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  mndx=255
  to=9206
  call putil2
  if(.not.(mflg.eq.0)) goto 5221
  isto(ishenv+54)=77
  continue
  call synstp
  goto 5220
5221 continue
5220 continue
  isto(env+5)=dptr+1
  to=9200
  call putil2
  isto(env+4)=dptr -isto(env+5) +1
  if(.not.(isto(env+4).gt.6)) goto 5231
  isto(ishenv+54)=78
  continue
  call synstp
  goto 5230
5231 continue
5230 continue
  to=9204
  call putil2
  mndx=139
  to=9206
  call putil2
  goto 9500
9007 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=lpflg
  lpflg=1
3041 if(.not.(lpflg.gt.0)) goto 3040
  iptr=iptr+4
  isto(env+2)=dptr+1
  to=9200
  call putil2
  isto(env+1)=dptr -isto(env+2) +1
  to=9204
  call putil2
  mndx=79
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5241
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=flg1
  mndx=93
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5251
  flg1=1
  goto 5250
5251 continue
  flg1=0
5250 continue
  to=9214
  call putil2
  a=rsto(rptr)
  rptr=rptr-1
  isto(env+3)=idnint(a)
  flg1=isto(sptr)
  sptr=sptr+1
  mndx=81
  to=9205
  call putil2
  if(.not.(mflg.eq.0)) goto 5261
  isto(ishenv+54)=79
  continue
  call synstp
  goto 5260
5261 continue
5260 continue
  goto 5240
5241 continue
  isto(env+3)=iinf
5240 continue
  mndx=113
  to=9205
  call putil2
  if(.not.(mflg.gt.0)) goto 5271
  isto(env+0)=iptr+1
  env=iptr+1
  goto 5270
5271 continue
  isto(env+0)=0
  lpflg=0
5270 continue
  goto 3041
3040 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  goto 9500
9008 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ptr1
  ptr1=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx0
  ndx0=0
  isto(env+1)=ptr1
  i0=base1 +417
  k=8
  i=0
3051 if(.not.(i.lt.k)) goto 3050
  i=i+1
  i0=i0+2
  cnt=cnt+1
  isto(iptr+1)=isto(i0+1)
  isto(iptr+2)=isto(i0)
  iptr=iptr+2
  goto 3051
3050 continue
  isto(env+4)=k
  cnt=k
  ndx0=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=isto(env+10)
3061 if(.not.(env.gt.0)) goto 3060
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9509
  goto 9009
9509 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  goto 3061
3060 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+9)=cnt-ndx0
  ndx0=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=isto(env+36)
3071 if(.not.(env.gt.0)) goto 3070
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9510
  goto 9010
9510 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  goto 3071
3070 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+11)=cnt-ndx0
  isto(env+37)=cnt-ndx0
  isto(env+0)=cnt
  ndx0=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  to=9012
  call xref1
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=from
  from=9511
  goto 9011
9511 from=isto(sptr)
  sptr=sptr+1
  goto 9500
9009 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx3
  ndx3=isto(env+3)
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx4
  ndx4=isto(env+4)
  to=9005
  call xref1
  if(.not.(flg.eq.0)) goto 5281
  to=9006
  call xref1
  goto 5280
5281 continue
5280 continue
  if(.not.(flg.gt.0)) goto 5291
  stpflg=46
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  goto 5290
5291 continue
5290 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx2
  ndx2=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx1
  ndx1=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx0
  ndx0=ptr1-2
  to=9003
  call xref1
  ndx0=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) goto 5301
  if(.not.(ndx1.gt.ndx0)) goto 5311
  stpflg=99
  goto 5310
5311 continue
  stpflg=122
5310 continue
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  goto 5300
5301 continue
  cnt=cnt+1
  isto(iptr+1)=ndx3
  isto(iptr+2)=ndx4
  iptr=iptr+2
5300 continue
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  goto 9500
9010 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx3
  ndx3=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx4
  ndx4=isto(env+3)
  to=9005
  call xref1
  if(.not.(flg.eq.0)) goto 5321
  to=9006
  call xref1
  goto 5320
5321 continue
5320 continue
  if(.not.(flg.gt.0)) goto 5331
  stpflg=46
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  goto 5330
5331 continue
5330 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx2
  ndx2=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx1
  ndx1=0
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=ndx0
  ndx0=ptr1-2
  to=9003
  call xref1
  ndx0=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) goto 5341
  if(.not.(ndx1.gt.ndx0)) goto 5351
  stpflg=123
  goto 5350
5351 if(.not.(ndx1.gt.8)) goto 5352
  stpflg=124
  goto 5350
5352 continue
  stpflg=125
5350 continue
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  goto 5340
5341 continue
  cnt=cnt+1
  isto(iptr+1)=ndx3
  isto(iptr+2)=ndx4
  iptr=iptr+2
5340 continue
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  goto 9500
9011 continue
  sptr=sptr-1
  if(sptr.eq.iptr) goto 910
  isto(sptr)=env
  env=isto(env+31)
3081 if(.not.(env.gt.0)) goto 3080
  if(.not.(isto(env+1).gt.0)) goto 5371
  isto(env+4)=0
  isto(env+5)=0
  goto 5370
5371 continue
  stpflg=133
  stpl1=isto(env+2)
  stpc1=isto(env+3)
  continue
  call errstp
5370 continue
  env=isto(env+0)
  goto 3081
3080 continue
  env=isto(sptr)
  sptr=sptr+1
  goto 9500
end subroutine ptacs
!
!     end of file: ptacs.for
!
