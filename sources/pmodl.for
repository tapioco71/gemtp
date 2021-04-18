!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: pmodl.for
!
!
!     subroutine pmodl.
!
subroutine pmodl
  include 'tacsto.ins'
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=0
  go to (9001, 9002, 9003, 9004, 9005, 9006, 9007, 9008, 9009, 9010, 9011, 9012, 9013, 9014, 9015, 9016, 9017, 9018, 9019, &
       9020, 9021, 9022, 9023, 9024, 9025, 9026, 9027), to-9000
  stop 'invalid "to" reference in "pmodl".'
9500 if (.not. (from .eq. 0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (0002, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, 9516, 9517, 9518, 9519, &
       9520, 9521, 9522, 9523, 9524, 9525, 9526, 9527, 9528), from-9500
0002 stop 'invalid "from" reference in "pmodl".'
910 stpflg = 42
  stpi1 = iptr
  stpi2 = ilen - iptr
  continue
  call errstp
9001 continue
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr3
  ptr3=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr4
  ptr4=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr5
  ptr5=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr6
  ptr6=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr7
  ptr7=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr8
  ptr8=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr9
  ptr9=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr10
  ptr10=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr11
  ptr11=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr12
  ptr12=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr13
  ptr13=0
  isto(ishenv+16)=isto(ishenv+16)+1
  isto(ishenv+63)=0
  iptr=iptr+49
  isto(env+2)=0
  isto(ishenv+22)=env+2
  isto(env+3)=0
  isto(ishenv+23)=env+3
  isto(env+26)=0
  isto(ishenv+24)=env+26
  isto(env+28)=0
  isto(ishenv+25)=env+28
  isto(env+39)=0
  isto(ishenv+26)=env+39
  isto(env+40)=0
  isto(ishenv+27)=env+40
  isto(env+6)=0
  isto(ishenv+28)=env+6
  isto(env+8)=0
  isto(ishenv+29)=env+8
  isto(env+10)=0
  isto(ishenv+30)=env+10
  isto(env+12)=0
  isto(ishenv+33)=env+12
  isto(env+16)=0
  isto(ishenv+34)=env+16
  isto(env+13)=0
  isto(ishenv+35)=env+13
  isto(env+14)=0
  isto(ishenv+36)=env+14
  isto(env+15)=0
  isto(ishenv+37)=env+15
  isto(env+36)=0
  isto(ishenv+31)=env+36
  isto(env+22)=0
  isto(ishenv+32)=env+22
  isto(env+19)=0
  isto(ishenv+38)=env+19
  isto(env+20)=0
  isto(ishenv+39)=env+20
  isto(env+21)=0
  isto(ishenv+40)=env+21
  isto(env+24)=0
  isto(ishenv+41)=env+24
  isto(env+41)=0
  isto(ishenv+42)=env+41
  isto(env+30)=0
  isto(ishenv+43)=env+30
  isto(env+32)=0
  isto(ishenv+46)=env+32
  isto(env+33)=0
  isto(ishenv+47)=env+33
  isto(env+31)=0
  isto(ishenv+44)=env+31
  isto(env+35)=0
  isto(ishenv+45)=env+35
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=lpflg
  mndx=149
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5821
  isto(env+48)=0
  lpflg=0
  go to 5820
5821 continue
  isto(env+48)=iptr+1
  lpflg=1
5820 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
3011 if (.not.(lpflg.gt.0)) go to 3010
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9502
  go to 9002
9502 from=isto(sptr)
  sptr=sptr+1
  mndx=149
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5011
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5010
5011 continue
  isto(env+0)=0
  lpflg=0
5010 continue
  go to 3011
3010 continue
  env=isto(sptr)
  sptr=sptr+1
  lpflg=isto(sptr)
  sptr=sptr+1
  isto(env+44)=ptr3
  isto(env+45)=ptr4
  isto(env+23)=ptr5
  isto(env+25)=ptr7
  isto(env+27)=ptr8
  isto(env+29)=ptr9
  isto(env+34)=ptr11
  isto(env+46)=ptr12
  isto(env+47)=ptr13
  ptr13=isto(sptr)
  sptr=sptr+1
  ptr12=isto(sptr)
  sptr=sptr+1
  ptr11=isto(sptr)
  sptr=sptr+1
  ptr10=isto(sptr)
  sptr=sptr+1
  ptr9=isto(sptr)
  sptr=sptr+1
  ptr8=isto(sptr)
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
  to=9000
  call xref1
  to=9000
  call xref2
  isto(ishenv+16)=isto(ishenv+16)-1
  go to 9500
9002 continue
  iptr=iptr+3
  isto(env+2)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx
  ndx=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  mndx=147
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5021
  ndx=1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9003
9503 from=isto(sptr)
  sptr=sptr+1
  go to 5020
5021 continue
  mndx=159
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5031
  ndx=2
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9007
9504 from=isto(sptr)
  sptr=sptr+1
  go to 5030
5031 continue
  mndx=167
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5041
  ndx=3
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9014
9505 from=isto(sptr)
  sptr=sptr+1
  go to 5040
5041 continue
  mndx=171
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5051
  ndx=4
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9015
9506 from=isto(sptr)
  sptr=sptr+1
  go to 5050
5051 continue
5050 continue
5040 continue
5030 continue
5020 continue
  if (.not.(ndx .eq. 0)) go to 5061
  mndx=175
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5071
  ndx=5
  go to 5070
5071 continue
  mndx=177
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5081
  ndx=6
  go to 5080
5081 continue
  mndx=179
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5091
  ndx=7
  go to 5090
5091 continue
  mndx=181
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5101
  ndx=8
  go to 5100
5101 continue
  mndx=183
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5111
  ndx=9
  go to 5110
5111 continue
  mndx=185
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5121
  ndx=10
  go to 5120
5121 continue
  mndx=187
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5131
  ndx=11
  go to 5130
5131 continue
  mndx=189
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5141
  ndx=12
  go to 5140
5141 continue
5140 continue
5130 continue
5120 continue
5110 continue
5100 continue
5090 continue
5080 continue
5070 continue
  if (.not.(ndx.gt.0)) go to 5151
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9507
  go to 9016
9507 from=isto(sptr)
  sptr=sptr+1
  go to 5150
5151 continue
5150 continue
  go to 5060
5061 continue
5060 continue
  env=isto(sptr)
  sptr=sptr+1
  if (.not.(ndx .eq. 0)) go to 5161
  isto(ishenv+54)=2
  continue
  call synstp
  go to 5160
5161 continue
5160 continue
  isto(env+1)=ndx
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9003 continue
  iptr=iptr+6
  ipn=isto(ishenv+24)
  isto(ipn)=env
  isto(ishenv+24)=env
  isto(env+0)=0
  isto(env+3)=dptr+1
  to=9200
  call putil2
  isto(env+2)=dptr -isto(env+3)+1
  to=9203
  call putil2
  if (.not.(mflg .eq. 0)) go to 5171
  isto(ishenv+54)=1
  continue
  call synstp
  go to 5170
5171 continue
5170 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx3
  ndx3=isto(env+2)
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx4
  ndx4=isto(env+3)
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9508
  go to 9004
9508 from=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  ptr7=ptr7+1
  isto(env+1)=ptr7
  isto(env+5)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx
  mndx=151
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5181
  ndx=2
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9509
  go to 9005
9509 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5180
5181 continue
  ndx=1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=datenv
  datenv=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=isto(ishenv+17)
  isto(ishenv+17)=datenv
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-27
  if (sptr.le.iptr) go to 910
  i0=sptr+27
  i1=ishenv+21
  i=0
3131 if (.not.(i.lt.27)) go to 3130
  i=i+1
  isto(i0-i)=isto(i1+i)
  go to 3131
3130 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9510
  go to 9001
9510 from=isto(sptr)
  sptr=sptr+1
  i0=sptr+27
  i1=ishenv+21
  i=0
3141 if (.not.(i.lt.27)) go to 3140
  i=i+1
  isto(i1+i)=isto(i0-i)
  go to 3141
3140 continue
  sptr=sptr+27
  env=isto(sptr)
  sptr=sptr+1
  isto(ishenv+17)=isto(sptr)
  sptr=sptr+1
  datenv=isto(sptr)
  sptr=sptr+1
5180 continue
  isto(env+4)=ndx
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9004 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=isto(env+26)
  stpflg=0
3021 if (.not. (env .gt. 0 .and. env .lt. isto(ishenv + 24) .and. stpflg .eq. 0)) go to 3020
  i1=isto(env+2)
  if (.not.(i1 .eq. ndx3)) go to 5191
  i=0
  i2=isto(env+3)
3031 if (.not. (i .lt. ndx3 .and. csto(i2 + i) .eq. csto(ndx4 + i))) go to 3030
  i=i+1
  go to 3031
3030 continue
  if (.not.(i .eq. ndx3)) go to 5201
  stpflg=96
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5200
5201 continue
5200 continue
  go to 5190
5191 continue
5190 continue
  env=isto(env+0)
  go to 3021
3020 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9005 continue
  iptr=iptr+2
  isto(env+0)=dptr+1
  mndx=275
  to=9208
  call putil2
  if (.not.(mflg .eq. 0)) go to 5211
  isto(ishenv+54)=4
  continue
  call synstp
  go to 5210
5211 continue
5210 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx1
  ndx1=0
3121 if (.not.(ndx1.lt.3)) go to 3120
  ndx1=ndx1+1
  to=9211
  call putil2
  if (.not.(mflg .eq. 0)) go to 5221
  isto(ishenv+54)=4
  continue
  call synstp
  go to 5440
5221 continue
  !     5220  continue
  continue
  go to 3121
3120 continue
  ndx1=isto(sptr)
  sptr=sptr+1
  to=9204
  call putil2
  mndx=87
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5231
  isto(ishenv+54)=5
  continue
  call synstp
  go to 5230
5231 continue
5230 continue
  isto(env+1)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3041 if (.not.(lpflg.gt.0)) go to 3040
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9511
  go to 9006
9511 from=isto(sptr)
  sptr=sptr+1
  mndx=89
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5241
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5240
5241 continue
  isto(env+0)=0
  lpflg=0
5240 continue
  go to 3041
3040 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9006 continue
  iptr=iptr+3
  mndx=145
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5251
  k=1
  go to 5250
5251 continue
  mndx=153
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5261
  k=2
  go to 5260
5261 continue
  mndx=155
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5271
  k=3
  go to 5270
5271 continue
  mndx=157
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5281
  k=4
  go to 5280
5281 continue
  isto(ishenv+54)=5
  continue
  call synstp
5280 continue
5270 continue
5260 continue
5250 continue
  isto(env+1)=k
  mndx=105
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5291
  isto(ishenv+54)=6
  continue
  call synstp
  go to 5290
5291 continue
5290 continue
  isto(env+2)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=1
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9007 continue
  iptr=iptr+6
  ipn=isto(ishenv+43)
  isto(ipn)=env
  isto(ishenv+43)=env
  isto(env+0)=0
  isto(env+3)=dptr+1
  to=9200
  call putil2
  isto(env+2)=dptr -isto(env+3)+1
  to=9204
  call putil2
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx3
  ndx3=isto(env+2)
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx4
  ndx4=isto(env+3)
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9512
  go to 9008
9512 from=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  ptr9=ptr9+1
  isto(env+1)=ptr9
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx
  isto(env+5)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  mndx=83
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5301
  ndx=1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9513
  go to 9009
9513 from=isto(sptr)
  sptr=sptr+1
  go to 5300
5301 continue
  mndx=161
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5311
  ndx=2
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9514
  go to 9011
9514 from=isto(sptr)
  sptr=sptr+1
  go to 5310
5311 continue
  mndx=151
  to=9206
  call putil2
  if (.not.(mflg.gt.0)) go to 5321
  ndx=3
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9515
  go to 9013
9515 from=isto(sptr)
  sptr=sptr+1
  go to 5320
5321 continue
  isto(ishenv+54)=7
  continue
  call synstp
5320 continue
5310 continue
5300 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+4)=ndx
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9008 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=isto(env+30)
  stpflg=0
3051 if (.not.(env .gt. 0 .and. env .lt. isto(ishenv + 43) .and. stpflg .eq. 0)) go to 3050
  i1=isto(env+2)
  if (.not.(i1 .eq. ndx3)) go to 5331
  i=0
  i2=isto(env+3)
3061 if (.not. (i .lt. ndx3 .and. csto(i2 + i) .eq. csto(ndx4 + i))) go to 3060
  i=i+1
  go to 3061
3060 continue
  if (.not.(i .eq. ndx3)) go to 5341
  stpflg=97
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5340
5341 continue
5340 continue
  go to 5330
5331 continue
5330 continue
  env=isto(env+0)
  go to 3051
3050 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9009 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx0
  ndx0=ptr11
  iptr=iptr+5
  ipn=isto(ishenv+46)
  isto(ipn)=env
  isto(ishenv+46)=env
  isto(env+0)=0
  ptr10=env
  isto(env+3)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3071 if (.not.(lpflg.gt.0)) go to 3070
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9516
  go to 9010
9516 from=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5351
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5350
5351 continue
  isto(env+0)=0
  lpflg=0
5350 continue
  go to 3071
3070 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ptr11-ndx0
  mndx=103
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5361
  isto(ishenv+54)=8
  continue
  call synstp
  go to 5360
5361 continue
5360 continue
  isto(env+4)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=2
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ptr10=0
  ndx0=isto(sptr)
  sptr=sptr+1
  go to 9500
9010 continue
  ptr11=ptr11+1
  iptr=iptr+7
  isto(env+2)=dptr+1
  to=9200
  call putil2
  isto(env+1)=dptr -isto(env+2)+1
  to=9204
  call putil2
  mndx=87
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5371
  isto(env+6)=iptr+1
  isto(ishenv+50)=3
  to=9126
  call putil1
  if (.not.(mflg .eq. 0)) go to 5381
  isto(ishenv+54)=9
  continue
  call synstp
  go to 5380
5381 continue
5380 continue
  mndx=89
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5391
  isto(ishenv+54)=9
  continue
  call synstp
  go to 5390
5391 continue
5390 continue
  go to 5370
5371 continue
  isto(env+6)=0
5370 continue
  go to 9500
9011 continue
  iptr=iptr+3
  ipn=isto(ishenv+47)
  isto(ipn)=env
  isto(ishenv+47)=env
  isto(env+0)=0
  mndx=83
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5401
  isto(ishenv+54)=10
  continue
  call synstp
  go to 5400
5401 continue
5400 continue
  isto(env+1)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(env+1)=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3081 if (.not.(lpflg.gt.0)) go to 3080
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9517
  go to 9012
9517 from=isto(sptr)
  sptr=sptr+1
  mndx=83
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5411
  isto(env+0)=iptr+1
  k=env
  env=iptr+1
  isto(env+1)=k
  go to 5410
5411 continue
  isto(env+0)=0
  lpflg=0
5410 continue
  go to 3081
3080 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  k=env
  env=isto(sptr)
  sptr=sptr+1
  isto(env+2)=k
  go to 9500
9012 continue
  iptr=iptr+4
  isto(env+2)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=4
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  isto(env+3)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=5
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5421
  isto(ishenv+54)=11
  continue
  call synstp
  go to 5420
5421 continue
5420 continue
  go to 9500
9013 continue
  iptr=iptr+2
  isto(env+0)=dptr+1
  mndx=163
  to=9208
  call putil2
  if (.not.(mflg .eq. 0)) go to 5431
  isto(ishenv+54)=12
  continue
  call synstp
  go to 5430
5431 continue
5430 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx1
  ndx1=0
3111 if (.not.(ndx1.lt.3)) go to 3110
  ndx1=ndx1+1
  to=9211
  call putil2
  if (.not.(mflg .eq. 0)) go to 5441
  isto(ishenv+54)=12
  continue
  call synstp
  go to 5440
5441 continue
5440 continue
  go to 3111
3110 continue
  ndx1=isto(sptr)
  sptr=sptr+1
  to=9204
  call putil2
  mndx=87
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5451
  isto(ishenv+54)=13
  continue
  call synstp
  go to 5450
5451 continue
5450 continue
  mndx=165
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5461
  mndx=105
  to=9205
  call putil2
  go to 5460
5461 continue
5460 continue
  if (.not.(mflg .eq. 0)) go to 5471
  isto(ishenv+54)=13
  continue
  call synstp
  go to 5470
5471 continue
5470 continue
  isto(env+1)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=6
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=89
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5481
  isto(ishenv+54)=13
  continue
  call synstp
  go to 5480
5481 continue
5480 continue
  go to 9500
9014 continue
  iptr=iptr+1
  isto(ishenv+53)=1
  isto(ishenv+52)=1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr1
  continue
  call pstmt
  ptr12=ptr1
  isto(env+0)=ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  mndx=169
  to=9206
  call putil2
  isto(ishenv+52)=0
  go to 9500
9015 continue
  iptr=iptr+1
  isto(ishenv+53)=2
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ptr1
  continue
  call pstmt
  ptr13=ptr1
  isto(env+0)=ptr1
  ptr1=isto(sptr)
  sptr=sptr+1
  mndx=173
  to=9206
  call putil2
  go to 9500
9016 continue
  iptr=iptr+1
  isto(env+0)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3091 if (.not.(lpflg.gt.0)) go to 3090
  if (.not.(ndx .eq. 5)) go to 5491
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9518
  go to 9018
9518 from=isto(sptr)
  sptr=sptr+1
  go to 5490
5491 if (.not.(ndx .eq. 6)) go to 5492
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9519
  go to 9019
9519 from=isto(sptr)
  sptr=sptr+1
  go to 5490
5492 if (.not.(ndx .eq. 7)) go to 5493
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9520
  go to 9020
9520 from=isto(sptr)
  sptr=sptr+1
  go to 5490
5493 if (.not.(ndx .eq. 8)) go to 5494
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9521
  go to 9021
9521 from=isto(sptr)
  sptr=sptr+1
  go to 5490
5494 if (.not.(ndx .eq. 9)) go to 5495
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9522
  go to 9022
9522 from=isto(sptr)
  sptr=sptr+1
  go to 5490
5495 if (.not.(ndx .eq. 10)) go to 5496
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9523
  go to 9023
9523 from=isto(sptr)
  sptr=sptr+1
  go to 5490
5496 if (.not.(ndx .eq. 11)) go to 5497
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9524
  go to 9024
9524 from=isto(sptr)
  sptr=sptr+1
  go to 5490
5497 if (.not.(ndx .eq. 12)) go to 5498
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9525
  go to 9025
9525 from=isto(sptr)
  sptr=sptr+1
  go to 5490
5498 continue
5490 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9526
  go to 9017
9526 from=isto(sptr)
  sptr=sptr+1
  if (.not.(mflg .eq. 0)) go to 5501
  isto(env+1)=iptr+1
  env=iptr+1
  go to 5500
5501 continue
  isto(env+1)=0
  lpflg=0
5500 continue
  go to 3091
3090 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9017 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=dptr
  mndx=149
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5511
  mndx=147
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5521
  mndx=159
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5531
  mndx=167
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5541
  mndx=171
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5551
  mndx=175
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5561
  mndx=177
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5571
  mndx=179
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5581
  mndx=181
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5591
  mndx=183
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5601
  mndx=185
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5611
  mndx=187
  to=9206
  call putil2
  if (.not.(mflg .eq. 0)) go to 5621
  mndx=189
  to=9206
  call putil2
  go to 5620
5621 continue
5620 continue
  go to 5610
5611 continue
5610 continue
  go to 5600
5601 continue
5600 continue
  go to 5590
5591 continue
5590 continue
  go to 5580
5581 continue
5580 continue
  go to 5570
5571 continue
5570 continue
  go to 5560
5561 continue
5560 continue
  go to 5550
5551 continue
5550 continue
  go to 5540
5541 continue
5540 continue
  go to 5530
5531 continue
5530 continue
  go to 5520
5521 continue
5520 continue
  go to 5510
5511 continue
5510 continue
  dptr=isto(sptr)
  sptr=sptr+1
  go to 9500
9018 continue
  iptr=iptr+6
  ipn=isto(ishenv+28)
  isto(ipn)=env
  isto(ishenv+28)=env
  isto(env+0)=0
  isto(env+4)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=7
  to=9119
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=87
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5631
  isto(ishenv+54)=14
  continue
  call synstp
  go to 5630
5631 continue
5630 continue
  isto(env+5)=iptr+1
  isto(ishenv+50)=8
  to=9125
  call putil1
  if (.not.(mflg .eq. 0)) go to 5641
  isto(ishenv+54)=14
  continue
  call synstp
  go to 5640
5641 continue
5640 continue
  mndx=89
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5651
  isto(ishenv+54)=14
  continue
  call synstp
  go to 5650
5651 continue
5650 continue
  go to 9500
9019 continue
  iptr=iptr+6
  ipn=isto(ishenv+29)
  isto(ipn)=env
  isto(ishenv+29)=env
  isto(env+0)=0
  isto(env+4)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=9
  to=9119
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=87
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5661
  isto(env+5)=iptr+1
  isto(ishenv+50)=10
  to=9126
  call putil1
  if (.not.(mflg .eq. 0)) go to 5671
  isto(ishenv+54)=15
  continue
  call synstp
  go to 5670
5671 continue
5670 continue
  mndx=89
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5681
  isto(ishenv+54)=15
  continue
  call synstp
  go to 5680
5681 continue
5680 continue
  go to 5660
5661 continue
  isto(env+5)=0
5660 continue
  go to 9500
9020 continue
  iptr=iptr+5
  ipn=isto(ishenv+33)
  isto(ipn)=env
  isto(ishenv+33)=env
  isto(env+0)=0
  isto(env+4)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=11
  to=9119
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9021 continue
  iptr=iptr+6
  ipn=isto(ishenv+30)
  isto(ipn)=env
  isto(ishenv+30)=env
  isto(env+0)=0
  isto(env+4)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=12
  to=9119
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  mndx=87
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5691
  isto(env+5)=iptr+1
  isto(ishenv+50)=13
  to=9126
  call putil1
  if (.not.(mflg .eq. 0)) go to 5701
  isto(ishenv+54)=16
  continue
  call synstp
  go to 5700
5701 continue
5700 continue
  mndx=89
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5711
  isto(ishenv+54)=16
  continue
  call synstp
  go to 5710
5711 continue
5710 continue
  go to 5690
5691 continue
  isto(env+5)=0
5690 continue
  go to 9500
9022 continue
  iptr=iptr+4
  ipn=isto(ishenv+31)
  isto(ipn)=env
  isto(ishenv+31)=env
  isto(env+0)=0
  isto(env+3)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=14
  to=9119
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9023 continue
  iptr=iptr+6
  ipn=isto(ishenv+32)
  isto(ipn)=env
  isto(ishenv+32)=env
  isto(env+0)=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx
  ndx=1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx1
  isto(env+4)=iptr+1
  isto(ishenv+50)=16
  to=9100
  call putil1
  isto(env+2)=ndx1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  mndx=87
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5721
  isto(env+5)=iptr+1
  isto(ishenv+50)=15
  to=9126
  call putil1
  if (.not.(mflg .eq. 0)) go to 5731
  isto(ishenv+54)=17
  continue
  call synstp
  go to 5730
5731 continue
5730 continue
  mndx=89
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5741
  isto(ishenv+54)=17
  continue
  call synstp
  go to 5740
5741 continue
5740 continue
  go to 5720
5721 continue
  isto(env+5)=0
5720 continue
  go to 9500
9024 continue
  iptr=iptr+3
  isto(env+2)=iptr+1
  isto(ishenv+50)=20
  to=9127
  call putil1
  if (.not.(mflg.gt.0)) go to 5751
  isto(env+0)=1
  ptr3=isto(env+2)
  go to 5750
5751 continue
  isto(ishenv+50)=21
  to=9128
  call putil1
  if (.not.(mflg.gt.0)) go to 5761
  isto(env+0)=2
  ptr4=isto(env+2)
  go to 5760
5761 continue
  isto(ishenv+54)=19
  continue
  call synstp
5760 continue
5750 continue
  go to 9500
9025 continue
  iptr=iptr+3
  mndx=191
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5771
  isto(ishenv+54)=20
  continue
  call synstp
  go to 5770
5771 continue
5770 continue
  mndx=83
  to=9205
  call putil2
  if (.not.(mflg.gt.0)) go to 5781
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9527
  go to 9026
9527 from=isto(sptr)
  sptr=sptr+1
  mndx=105
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5791
  isto(ishenv+54)=20
  continue
  call synstp
  go to 5790
5791 continue
5790 continue
  isto(env+2)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=22
  to=9101
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  go to 5780
5781 continue
  isto(env+0)=0
  isto(env+2)=iptr+1
  isto(ishenv+50)=23
  to=9126
  call putil1
  if (.not.(mflg .eq. 0)) go to 5801
  isto(ishenv+54)=20
  continue
  call synstp
  go to 5800
5801 continue
5800 continue
5780 continue
  go to 9500
9026 continue
  isto(env+0)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3101 if (.not.(lpflg.gt.0)) go to 3100
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=from
  from=9528
  go to 9027
9528 from=isto(sptr)
  sptr=sptr+1
  mndx=85
  to=9205
  call putil2
  if (.not.(mflg .eq. 0)) go to 5811
  isto(env+0)=iptr+1
  env=iptr+1
  go to 5810
5811 continue
  isto(env+0)=0
  lpflg=0
5810 continue
  go to 3101
3100 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9027 continue
  iptr=iptr+2
  isto(env+1)=iptr+1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx
  ndx=7
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=iptr+1
  isto(ishenv+50)=24
  to=9120
  call putil1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
end subroutine pmodl
!
!     end of file: pmodl.for
!
