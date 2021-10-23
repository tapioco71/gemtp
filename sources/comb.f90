!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file comb.f90
!

!
! subroutine comb.
!

subroutine comb
  use tacsto
  implicit none
  !  include 'tacsto.ftn'
  sptr=sptr-1
  isto(sptr)=from
  from=0
  !  goto (9000, 9001, 9002, 9003, 9004, 9005, 9006, 9007, 9008, 9009, 9010, 9011, 9012, 9013, 9014, 9015, 9016, 9017, 9018, 9019, 9020, 9021, 9022), to-8999
  select case (to - 8999)
  case (1)
     go to 9000

  case (2)
     go to 9001

  case (3)
     go to 9002

  case (4)
     go to 9003

  case (5)
     go to 9004

  case (6)
     go to 9005

  case (7)
     go to 9006

  case (8)
     go to 9007

  case (9)
     go to 9008

  case (10)
     go to 9009

  case (11)
     go to 9010

  case (12)
     go to 9011

  case (13)
     go to 9012

  case (14)
     go to 9013

  case (15)
     go to 9014

  case (16)
     go to 9015

  case (17)
     go to 9016

  case (18)
     go to 9017

  case (19)
     go to 9018

  case (20)
     go to 9019

  case (21)
     go to 9020

  case (22)
     go to 9021

  case (23)
     go to 9022
  end select
  stop 'Invalid "to" reference in "comb".'
9500 if(.not.(from.eq.0)) goto 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 goto (9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, 0002, 0002, 0002, 0002, 9520,&
       9521, 9522, 9523, 9524, 9525, 9526), from- 9500
0002 stop 'invalid "from" reference in "comb".'
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
  if(sptr-50.le.iptr) goto 910
  if(rptr+20.ge.rsptr) goto 930
  sptr=sptr-1
  isto(sptr)=cnt1
  cnt1=isto(env+7)
  sptr=sptr-1
  isto(sptr)=cnt2
  sptr=sptr-1
  isto(sptr)=cnt3
  cnt3=isto(env+8)
  sptr=sptr-1
  isto(sptr)=ndx1
  ndx1=isto(env+1)
  sptr=sptr-1
  isto(sptr)=ndx2
  ndx2=isto(env+2)
  sptr=sptr-1
  isto(sptr)=ndx3
  sptr=sptr-1
  isto(sptr)=ptr0
  ptr0=env
  sptr=sptr-1
  isto(sptr)=ptr1
  sptr=sptr-1
  isto(sptr)=ptr2
  sptr=sptr-1
  isto(sptr)=ptr3
  sptr=sptr-1
  isto(sptr)=ptr4
  sptr=sptr-1
  isto(sptr)=ptr5
  sptr=sptr-1
  isto(sptr)=ndx0
  xprndx=isto(env+3)
  if(.not.(xprndx.gt.0)) goto 5001
  to=9000
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  ndx3 = idnint (a)
  goto 5000
5001 continue
  ndx3=iinf
5000 continue
  stpl1=ndx1
  stpc1=ndx2
  stpi1=ndx3
  sptr=sptr-1
  isto(sptr)=from
  from=9501
  goto 9001
9501 from=isto(sptr)
  sptr=sptr+1
  i1=ptr2-cnt2
  i=0
3001 if(.not.(i.lt.cnt1)) goto 3000
  i=i+1
  rsto(ptr1+i)=zero
  i1=i1+cnt2
  j=0
3011 if(.not.(j.lt.cnt2)) goto 3010
  j=j+1
  rsto(i1+j)=zero
  goto 3011
3010 continue
  goto 3001
3000 continue
  ndx0=0
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+4)
3021 if(.not.(env.gt.0)) goto 3020
  ndx0=ndx0+1
  k=isto(env+4)
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+5)
  if(.not.(k.eq.1)) goto 5011
  sptr=sptr-1
  isto(sptr)=from
  from=9502
  goto 9004
9502 from=isto(sptr)
  sptr=sptr+1
  goto 5010
5011 if(.not.(k.eq.2)) goto 5012
  sptr=sptr-1
  isto(sptr)=from
  from=9503
  goto 9009
9503 from=isto(sptr)
  sptr=sptr+1
  goto 5010
5012 if(.not.(k.eq.3)) goto 5013
  sptr=sptr-1
  isto(sptr)=from
  from=9504
  goto 9010
9504 from=isto(sptr)
  sptr=sptr+1
  goto 5010
5013 continue
5010 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  goto 3021
3020 continue
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  isto(sptr)=from
  from=9505
  goto 9011
9505 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) goto 5021
  stpflg=109
  stpi2=flg
  continue
  call errstp
  goto 5020
5021 continue
5020 continue
  flg=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  isto(sptr)=from
  from=9506
  goto 9012
9506 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  isto(sptr)=from
  from=9507
  goto 9013
9507 from=isto(sptr)
  sptr=sptr+1
  if(.not.(isto(env+6).gt.0)) goto 5031
  sptr=sptr-1
  isto(sptr)=from
  from=9508
  goto 9015
9508 from=isto(sptr)
  sptr=sptr+1
  goto 5030
5031 continue
5030 continue
  sptr=sptr-1
  isto(sptr)=from
  from=9509
  goto 9014
9509 from=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  ptr5=isto(sptr+1)
  ptr4=isto(sptr+2)
  ptr3=isto(sptr+3)
  ptr2=isto(sptr+4)
  ptr1=isto(sptr+5)
  ptr0=isto(sptr+6)
  sptr=sptr+7
  ndx3=isto(sptr)
  ndx2=isto(sptr+1)
  ndx1=isto(sptr+2)
  cnt3=isto(sptr+3)
  cnt2=isto(sptr+4)
  cnt1=isto(sptr+5)
  sptr=sptr+6
  goto 9500
9001 continue
  cnt2=0
  sptr=sptr-1
  isto(sptr)=env
  env=useenv
  i1=isto(env+55)
  if(.not.(i1.gt.0)) goto 5041
  sptr=sptr-1
  isto(sptr)=env
  env=i1
3031 if(.not.(env.gt.0 .and. cnt2.eq.0)) goto 3030
  if(.not. (isto(env + 1) .eq. ndx1 .and. isto(env + 2) .eq. ndx2 .and. isto(env+3) .eq. ndx3)) goto 5051
  cnt2=isto(env+9)
  ptr1=isto(env+4)-1
  ptr2=isto(env+5)-1
  ptr3=isto(env+7)-1
  ptr4=isto(env+6)-1
  ptr5=isto(env+10)-1
  goto 5050
5051 continue
  i2=env
  env=isto(env+0)
5050 continue
  goto 3031
3030 continue
  if(.not.(cnt2.eq.0)) goto 5061
  env=i2
  isto(env+0)=iptr+1
  env=iptr+1
  goto 5060
5061 continue
5060 continue
  goto 5040
5041 continue
  isto(env+55)=iptr+1
  sptr=sptr-1
  isto(sptr)=env
  env=iptr+1
5040 continue
  if(.not.(cnt2.eq.0)) goto 5071
  sptr=sptr-1
  isto(sptr)=from
  from=9510
  goto 9002
9510 from=isto(sptr)
  sptr=sptr+1
  goto 5070
5071 continue
5070 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  goto 9500
9002 continue
  if(.not.(ndx3.eq.iinf)) goto 5261
  goto 5260
5261 continue
5260 continue
  iptr=iptr+11
  isto(env+0)=0
  isto(env+1)=ndx1
  isto(env+2)=ndx2
  isto(env+3)=ndx3
  isto(env+8)=cnt1
  ptr5=iptr
  isto(env+10)=iptr+1
  iptr=iptr+cnt1
  sptr=sptr-1
  isto(sptr)=env
  env=ptr0
  i0=isto(env+6)
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(i0.gt.0)) goto 5351
  k=cnt1-1
  isto(ptr5+cnt1)=cnt1
  goto 5350
5351 continue
  k=cnt1
5350 continue
  i=0
3181 if(.not.(i.lt.k)) goto 3180
  i=i+1
  isto(ptr5+i)=k-i+1
  goto 3181
3180 continue
  ptr4=iptr
  isto(env+6)=iptr+1
  iptr=iptr+cnt3
  ptr3=iptr
  isto(env+7)=ptr3+1
  sptr=sptr-1
  isto(sptr)=from
  from=9511
  goto 9003
9511 from=isto(sptr)
  sptr=sptr+1
  iptr=iptr+cnt2
  isto(env+9)=cnt2
  ptr1=rptr
  isto(env+4)=ptr1+1
  rptr=rptr+cnt1
  ptr2=rptr
  isto(env+5)=ptr2+1
  rptr=rptr+ cnt1*cnt2
  goto 9500
9003 continue
  sptr=sptr-1
  isto(sptr)=ndx1
  ndx1=0
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx3
  ndx3=0
  sptr=sptr-1
  isto(sptr)=env
  env=ptr0
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+4)
3041 if(.not.(env.gt.0)) goto 3040
  ndx1=ndx1+1
  xprndx=isto(env+3)
  if(.not.(xprndx.gt.0)) goto 5081
  to=9000
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  k = idnint (a)
  goto 5080
5081 continue
  k=1
5080 continue
  i5=isto(env+2)
  i2=isto(base5+i5)-isto(base4+i5)+k
  j=0
3051 if(.not.(j.lt.ndx1-1)) goto 3050
  j=j+1
  if(.not.(isto(ptr3+j).eq.i2)) goto 5091
  stpflg=110
  sptr=sptr-1
  isto(sptr)=env
  env=datenv
  i3=isto(env+1) +2*(j-1)
  stpl2=isto(i3)
  stpc2=isto(i3+1)
  env=isto(sptr)
  sptr=sptr+1
  stpi2=k
  continue
  call errstp
  goto 5090
5091 continue
5090 continue
  goto 3051
3050 continue
  isto(ptr3+ndx1)=i2
  env=isto(env+0)
  goto 3041
3040 continue
  env=isto(sptr)
  sptr=sptr+1
  cnt2=cnt1
  ndx2=isto(env+9)-2
3061 if(.not.(ndx3.lt.cnt3)) goto 3060
  ndx3=ndx3+1
  ndx2=ndx2+2
  xprndx=isto(ndx2+1)
  if(.not.(xprndx.gt.0)) goto 5101
  to=9000
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  k = idnint (a)
  goto 5100
5101 continue
  k=1
5100 continue
  i5=isto(ndx2)
  i2=isto(base5+i5)-isto(base4+i5)+k
  j=1
3071 if(.not. (j .le. cnt2 .and. isto(ptr3 + j) .ne. i2)) goto 3070
  j=j+1
  goto 3071
3070 continue
  if(.not.(j.gt.cnt2)) goto 5111
  cnt2=cnt2+1
  isto(ptr3+j)=i2
  goto 5110
5111 continue
5110 continue
  isto(ptr4+ndx3)=j
  goto 3061
3060 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  ndx2=isto(sptr+1)
  ndx1=isto(sptr+2)
  sptr=sptr+3
  goto 9500
9004 continue
  sptr=sptr-1
  isto(sptr)=ndx
  ndx=isto(env+1)
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+2)
  if(.not.(ndx.eq.2)) goto 5121
  sptr=sptr-1
  isto(sptr)=from
  from=9512
  goto 9005
9512 from=isto(sptr)
  sptr=sptr+1
  goto 5120
5121 if(.not.(ndx.eq.4)) goto 5122
  sptr=sptr-1
  isto(sptr)=from
  from=9513
  goto 9006
9513 from=isto(sptr)
  sptr=sptr+1
  goto 5120
5122 if(.not.(ndx.eq.5)) goto 5123
  sptr=sptr-1
  isto(sptr)=from
  from=9514
  goto 9007
9514 from=isto(sptr)
  sptr=sptr+1
  goto 5120
5123 if(.not.(ndx.eq.3 .or. ndx.eq.7)) goto 5124
  sptr=sptr-1
  isto(sptr)=from
  from=9515
  goto 9008
9515 from=isto(sptr)
  sptr=sptr+1
  goto 5120
5124 continue
5120 continue
  env=isto(sptr)
  sptr=sptr+1
  i2=ptr2 +(ndx0-1)*cnt2
  rsto(i2+ndx0)=rsto(i2+ndx0)+one
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+0)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  to=9003
  call use2
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  goto 9500
9005 continue
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=ndx7
  ndx7=ptr2 +(ndx0-1)*cnt2
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+0)
3081 if(.not.(env.gt.0)) goto 3080
  xprndx=isto(env+3)
  if(.not.(xprndx.gt.0)) goto 5131
  to=9000
  call xpr1
  goto 5130
5131 continue
  a=one
  rptr=rptr+1
  rsto(rptr)=a
5130 continue
  if(.not.(isto(env+1).eq.2)) goto 5141
  rsto(rptr)=-rsto(rptr)
  goto 5140
5141 continue
5140 continue
  i3=isto(env+4)
  if(.not.(i3.gt.0)) goto 5151
  sptr=sptr-1
  isto(sptr)=env
  env=i3
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  i1=isto(env+2)
  i2=isto(ptr4+i1)
  a=rsto(rptr)
  rptr=rptr-1
  b=rsto(ndx7+i2)
  rsto(ndx7+i2)=b-a
  goto 5150
5151 continue
  a=rsto(rptr)
  rptr=rptr-1
  b=rsto(ptr1+ndx0)
  rsto(ptr1+ndx0)=b+a
5150 continue
  env=isto(env+0)
  goto 3081
3080 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx7=isto(sptr)
  ndx6=isto(sptr+1)
  ndx5=isto(sptr+2)
  ndx4=isto(sptr+3)
  ndx2=isto(sptr+4)
  ndx1=isto(sptr+5)
  sptr=sptr+6
  goto 9500
9006 continue
  sptr=sptr-1
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  isto(sptr)=ndx
  ndx=isto(env+1)
  sptr=sptr-1
  isto(sptr)=ptr1
  ptr1=rptr
  sptr=sptr-1
  isto(sptr)=ptr2
  ptr2=isto(env+3)
  sptr=sptr-1
  isto(sptr)=ptr3
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+4)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  to=9006
  call use2
  env=isto(sptr)
  sptr=sptr+1
  to=9024
  call xpr1
  ndx6=isto(sptr)
  ndx5=isto(sptr+1)
  ndx4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  sptr=sptr+5
  ptr3=isto(sptr)
  ptr2=isto(sptr+1)
  ptr1=isto(sptr+2)
  ndx=isto(sptr+3)
  flg=isto(sptr+4)
  sptr=sptr+5
  a=rsto(rptr)
  rptr=rptr-1
  b=rsto(rptr)
  rptr=rptr-1
  c=rsto(ptr1+ndx0) +a
  rsto(ptr1+ndx0)=c
  i2=ptr2 +(ndx0-1)*cnt2
  i1=isto(env+2)
  i3=isto(ptr4+i1)
  d=rsto(i2+i3) -b
  rsto(i2+i3)=d
  goto 9500
9007 continue
  sptr=sptr-1
  isto(sptr)=ptr1
  ptr1=rptr
  sptr=sptr-1
  isto(sptr)=ptr2
  sptr=sptr-1
  isto(sptr)=ptr3
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+6)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  to=9006
  call use2
  to=9008
  call use2
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(isto(ptr2+2).gt.0)) goto 5161
  sptr=sptr-1
  isto(sptr)=env
  env=useenv
  env=isto(sptr)
  sptr=sptr+1
  i2=isto(ptr2)
  a=rsto(i2)
  b=zero
  rptr=rptr+1
  rsto(rptr)=b
  rptr=rptr+1
  rsto(rptr)=a
  goto 5160
5161 continue
  isto(ptr2+2)=0
  to=9028
  call xpr1
5160 continue
  ndx6=isto(sptr)
  ndx5=isto(sptr+1)
  ndx4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  ptr3=isto(sptr+5)
  ptr2=isto(sptr+6)
  ptr1=isto(sptr+7)
  sptr=sptr+8
  a=rsto(rptr)
  rptr=rptr-1
  b=rsto(rptr)
  rptr=rptr-1
  c=rsto(ptr1+ndx0) +a
  rsto(ptr1+ndx0)=c
  i2=ptr2 +(ndx0-1)*cnt2
  i1=isto(env+4)
  i3=isto(ptr4+i1)
  d=rsto(i2+i3) -b
  rsto(i2+i3)=d
  goto 9500
9008 continue
  sptr=sptr-1
  isto(sptr)=env
  if(.not.(isto(env+3).gt.0)) goto 5271
  env=env+6
  goto 5270
5271 continue
  env=env+18
5270 continue
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+15)
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+1)
  sptr=sptr-1
  isto(sptr)=flg
  flg=1
  sptr=sptr-1
  isto(sptr)=ndx
  if(.not.(ndx.eq.3)) goto 5171
  ndx=1
  goto 5170
5171 continue
  ndx=2
5170 continue
  sptr=sptr-1
  isto(sptr)=ptr1
  ptr1=rptr
  sptr=sptr-1
  isto(sptr)=ptr2
  ptr2=0
  sptr=sptr-1
  isto(sptr)=ptr3
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+2)
  ndx1=isto(env+9)
  ndx2=ndx1
  if(.not.(ndx1.gt.0)) goto 5181
  xprndx=ndx1
  to=9000
  call xpr1
  if(.not.(xprcnt.gt.1)) goto 5191
  stpflg=90
  stpi1=xprcnt
  continue
  call errstp
  goto 5190
5191 continue
5190 continue
  a=rsto(rptr)
  rptr=rptr-1
  ndx4 = idnint (a)
  goto 5180
5181 continue
  ndx4=1
5180 continue
  ndx6=ndx4
  ndx5=isto(env+1)
  i1=isto(env+3)
  k=isto(env+2)
  sptr=sptr-1
  isto(sptr)=env
  env=useenv
  if(.not.(k.eq.6)) goto 5281
  i0=isto(env+29) +4*(i1-1)
  goto 5280
5281 continue
  i0=isto(env+36) +3*(i1-1)
5280 continue
  i2=isto(i0)
  if(.not.(i2.gt.0)) goto 5291
  k=ndx4-isto(base4+ndx5)
  ptr3=i2+7*k
  goto 5290
5291 continue
  ptr3=0
5290 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  to=9024
  call xpr1
  i1=isto(env+8)
  i3=isto(ptr4+i1)
  ndx6=isto(sptr)
  ndx5=isto(sptr+1)
  ndx4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  sptr=sptr+5
  ptr3=isto(sptr)
  ptr2=isto(sptr+1)
  ptr1=isto(sptr+2)
  ndx=isto(sptr+3)
  flg=isto(sptr+4)
  sptr=sptr+5
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  a=rsto(rptr)
  rptr=rptr-1
  b=rsto(rptr)
  rptr=rptr-1
  c=rsto(ptr1+ndx0) +a
  rsto(ptr1+ndx0)=c
  i2=ptr2 +(ndx0-1)*cnt2
  d=rsto(i2+i3) -b
  rsto(i2+i3)=d
  goto 9500
9009 continue
  sptr=sptr-1
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  isto(sptr)=ndx
  ndx=isto(env+2)
  sptr=sptr-1
  isto(sptr)=ptr1
  sptr=sptr-1
  isto(sptr)=ptr2
  ptr2=isto(env+1)
  sptr=sptr-1
  isto(sptr)=ptr3
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+4)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  to=9006
  call use2
  isto(ptr3+4)=0
  env=isto(sptr)
  sptr=sptr+1
  ptr1=rptr
  to=9024
  call xpr1
  ndx6=isto(sptr)
  ndx5=isto(sptr+1)
  ndx4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  sptr=sptr+5
  ptr3=isto(sptr)
  ptr2=isto(sptr+1)
  ptr1=isto(sptr+2)
  ndx=isto(sptr+3)
  flg=isto(sptr+4)
  sptr=sptr+5
  a=rsto(rptr)
  rptr=rptr-1
  b=rsto(rptr)
  rptr=rptr-1
  c=rsto(ptr1+ndx0) -a
  rsto(ptr1+ndx0)=c
  i2=ptr2 +(ndx0-1)*cnt2
  rsto(i2+ndx0)=rsto(i2+ndx0)+b
  i1=isto(env+3)
  i3=isto(ptr4+i1)
  rsto(i2+i3)=rsto(i2+i3)-1
  goto 9500
9010 continue
  sptr=sptr-1
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  isto(sptr)=ndx
  ndx=isto(env+1)
  sptr=sptr-1
  isto(sptr)=ptr1
  ptr1=rptr
  sptr=sptr-1
  isto(sptr)=ptr2
  ptr2=isto(env+8)
  sptr=sptr-1
  isto(sptr)=ptr3
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+5)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  to=9006
  call use2
  env=isto(sptr)
  sptr=sptr+1
  to=9024
  call xpr1
  ndx=isto(env+2)
  ptr1=rptr
  ptr2=isto(env+9)
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+4)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  to=9006
  call use2
  isto(ptr3+4)=0
  env=isto(sptr)
  sptr=sptr+1
  to=9024
  call xpr1
  ndx6=isto(sptr)
  ndx5=isto(sptr+1)
  ndx4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  sptr=sptr+5
  ptr3=isto(sptr)
  ptr2=isto(sptr+1)
  ptr1=isto(sptr+2)
  ndx=isto(sptr+3)
  flg=isto(sptr+4)
  sptr=sptr+5
  i2=ptr2 +(ndx0-1)*cnt2
  a=rsto(rptr)
  rptr=rptr-1
  b=rsto(rptr)
  rptr=rptr-1
  c=rsto(ptr1+ndx0) -a
  rsto(ptr1+ndx0)=c
  rsto(i2+ndx0)=b
  a=rsto(rptr)
  rptr=rptr-1
  b=rsto(rptr)
  rptr=rptr-1
  c=rsto(ptr1+ndx0) +a
  rsto(ptr1+ndx0)=c
  i1=isto(env+3)
  i3=isto(ptr4+i1)
  d=rsto(i2+i3) -b
  rsto(i2+i3)=d
  goto 9500
9011 continue
  if(iptr+cnt1+1.ge.sptr) goto 910
  if(rptr+cnt2.ge.rsptr) goto 930
  i=0
3191 if(.not.(i.lt.cnt1)) goto 3190
  i=i+1
  isto(iptr+i)=isto(ptr5+i)
  goto 3191
3190 continue
  i6=0
  i7=0
  i8=0
  i9=0
  sptr=sptr-1
  isto(sptr)=env
  env=ptr0
  i10=isto(env+6)
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  isto(sptr)=lpflg
3201 if(.not. (i7 .lt. cnt1 .and. flg .eq. 0)) go to 3200
  lpflg = 1
3211 if(.not.(lpflg.gt.0)) goto 3210
  i8=i8+1
  if(.not. (i8 .eq. cnt1 .and. cnt1 .gt. 1 .and. i7 .lt. cnt1 - 1 .and. i10 .gt. 0)) goto 5361
  i8=i8+1
  goto 5360
5361 continue
5360 continue
  if(.not.(i8.gt.cnt1)) goto 5301
  i8=1
  i9=i6
  goto 5300
5301 continue
5300 continue
  i0=isto(iptr+i8)
  if(.not.(i0.gt.0)) goto 5311
  lpflg=0
  goto 5310
5311 continue
5310 continue
  goto 3211
3210 continue
  i1=ptr2+(i0-1)*cnt2
  i=0
3221 if(.not.(i.lt.cnt2)) goto 3220
  i=i+1
  rsto(rptr+i)=rsto(i1+i)
  goto 3221
3220 continue
  k1=0
3231 if(.not.(k1.lt.i7)) goto 3230
  k1=k1+1
  k=isto(ptr5+k1)
  i2=i1+k1
  a=rsto(i2)
  if (.not. (real (dabs (a), kind (rmargn)) .gt. rmargn)) goto 5321
  i3 = ptr2 + (k - 1) * cnt2
  j=k1
3241 if(.not.(j.lt.cnt2)) goto 3240
  j=j+1
  i4=i1+j
  rsto(i4)=rsto(i4)-a*rsto(i3+j)
  goto 3241
3240 continue
  goto 5320
5321 continue
5320 continue
  goto 3231
3230 continue
  i2=i1+i7+1
  a=rsto(i2)
  if (.not. (real (dabs(a), kind (rmargn)) .gt. rmargn)) goto 5331
  a=one/a
  rsto(i2)=a
  j=i7+1
3251 if(.not.(j.lt.cnt2)) goto 3250
  j=j+1
  i3=i1+j
  rsto(i3)=rsto(i3)*a
  goto 3251
3250 continue
  i7=i7+1
  isto(ptr5+i7)=i0
  isto(iptr+i8)=0
  goto 5330
5331 continue
  i6=i0
  if(.not.(i6.eq.i9)) goto 5341
  flg=i6
  goto 5340
5341 continue
5340 continue
  i=0
3261 if(.not.(i.lt.cnt2)) goto 3260
  i=i+1
  rsto(i1+i)=rsto(rptr+i)
  goto 3261
3260 continue
5330 continue
  goto 3201
3200 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  goto 9500
9012 continue
  i7=0
3131 if(.not.(i7.lt.cnt1)) goto 3130
  i7=i7+1
  i=isto(ptr5+i7)
  i1=ptr1+i
  a=rsto(i1)
  i2=ptr2+(i-1)*cnt2
  k1=0
3141 if(.not.(k1.lt.i7-1)) goto 3140
  k1=k1+1
  k=isto(ptr5+k1)
  a=a -rsto(i2+k1)*rsto(ptr1+k)
  goto 3141
3140 continue
  rsto(i1)=a*rsto(i2+i7)
  goto 3131
3130 continue
  goto 9500
9013 continue
  i=isto(ptr5+cnt1)
  a=rsto(ptr1+i)
  i1=ptr2 +(i-1)*cnt2
  k=cnt1
3151 if(.not.(k.lt.cnt2)) goto 3150
  k=k+1
  i2=isto(ptr3+k)
  b=rsto(base3+i2)
  a=a -rsto(i1+k)*b
  goto 3151
3150 continue
  i2=isto(ptr3+cnt1)
  rsto(base3+i2)=a
  goto 9500
9014 continue
  i7=cnt1
3161 if(.not.(i7.gt.1)) goto 3160
  i7=i7-1
  i=isto(ptr5+i7)
  a=rsto(ptr1+i)
  i1=ptr2+(i-1)*cnt2
  k1=i7
3171 if(.not.(k1.lt.cnt2)) goto 3170
  k1=k1+1
  i2=isto(ptr3+k1)
  b=rsto(base3+i2)
  a=a -rsto(i1+k1)*b
  goto 3171
3170 continue
  i2=isto(ptr3+i7)
  rsto(base3+i2)=a
  goto 3161
3160 continue
  goto 9500
9015 continue
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+5)
  k=isto(env+4)
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+5)
  if(.not.(k.eq.1)) goto 5231
  sptr=sptr-1
  isto(sptr)=from
  from=9520
  goto 9016
9520 from=isto(sptr)
  sptr=sptr+1
  goto 5230
5231 if(.not.(k.eq.2)) goto 5232
  sptr=sptr-1
  isto(sptr)=from
  from=9521
  goto 9021
9521 from=isto(sptr)
  sptr=sptr+1
  goto 5230
5232 if(.not.(k.eq.3)) goto 5233
  sptr=sptr-1
  isto(sptr)=from
  from=9522
  goto 9022
9522 from=isto(sptr)
  sptr=sptr+1
  goto 5230
5233 continue
5230 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  goto 9500
9016 continue
  k=isto(env+1)
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+2)
  if(.not.(k.eq.2)) goto 5241
  sptr=sptr-1
  isto(sptr)=from
  from=9523
  goto 9017
9523 from=isto(sptr)
  sptr=sptr+1
  goto 5240
5241 if(.not.(k.eq.4)) goto 5242
  sptr=sptr-1
  isto(sptr)=from
  from=9524
  goto 9018
9524 from=isto(sptr)
  sptr=sptr+1
  goto 5240
5242 if(.not.(k.eq.5)) goto 5243
  sptr=sptr-1
  isto(sptr)=from
  from=9525
  goto 9019
9525 from=isto(sptr)
  sptr=sptr+1
  goto 5240
5243 if(.not.(k.eq.3 .or. k.eq.7)) goto 5244
  sptr=sptr-1
  isto(sptr)=from
  from=9526
  goto 9020
9526 from=isto(sptr)
  sptr=sptr+1
  goto 5240
5244 continue
5240 continue
  env=isto(sptr)
  sptr=sptr+1
  goto 9500
9017 continue
  sptr=sptr-1
  isto(sptr)=ndx0
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  ndx0=isto(ptr5+cnt1)
  ndx1=isto(env+1)
  ndx2=isto(env+2)
  i2=isto(ptr3+ndx0)
  a=rsto(base3+i2)
  rptr=rptr+1
  rsto(rptr)=a
  to=9021
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  i2=isto(ptr3+ndx0)
  rsto(base3+i2)=a
  ndx2=isto(sptr)
  ndx1=isto(sptr+1)
  ndx0=isto(sptr+2)
  sptr=sptr+3
  goto 9500
9018 continue
  sptr=sptr-1
  isto(sptr)=ndx0
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  ndx0=isto(ptr5+cnt1)
  ndx1=isto(env+5)
  ndx2=isto(env+6)
  i2=isto(ptr3+ndx0)
  a=rsto(base3+i2)
  rptr=rptr+1
  rsto(rptr)=a
  to=9021
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  i2=isto(ptr3+ndx0)
  rsto(base3+i2)=a
  ndx2=isto(sptr)
  ndx1=isto(sptr+1)
  ndx0=isto(sptr+2)
  sptr=sptr+3
  goto 9500
9019 continue
  sptr=sptr-1
  isto(sptr)=ptr2
  sptr=sptr-1
  isto(sptr)=ndx0
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+6)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  to=9008
  call use2
  env=isto(sptr)
  sptr=sptr+1
  i1=isto(ptr2+2)
  if(.not.(i1.gt.1)) goto 5251
  isto(ptr2+2)=i1-2
  goto 5250
5251 continue
5250 continue
  ndx0=isto(ptr5+cnt1)
  ndx1=isto(env+7)
  ndx2=isto(env+8)
  i2=isto(ptr3+ndx0)
  a=rsto(base3+i2)
  rptr=rptr+1
  rsto(rptr)=a
  to=9023
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  i2=isto(ptr3+ndx0)
  rsto(base3+i2)=a
  ndx6=isto(sptr)
  ndx5=isto(sptr+1)
  ndx4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  ndx0=isto(sptr+5)
  ptr2=isto(sptr+6)
  sptr=sptr+7
  goto 9500
9020 continue
  sptr=sptr-1
  isto(sptr)=ndx0
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=env
  env=env+8
  ndx0=isto(ptr5+cnt1)
  ndx1=isto(env+1)
  ndx2=isto(env+2)
  env=isto(sptr)
  sptr=sptr+1
  i2=isto(ptr3+ndx0)
  a=rsto(base3+i2)
  rptr=rptr+1
  rsto(rptr)=a
  to=9021
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  i2=isto(ptr3+ndx0)
  rsto(base3+i2)=a
  ndx2=isto(sptr)
  ndx1=isto(sptr+1)
  ndx0=isto(sptr+2)
  sptr=sptr+3
  goto 9500
9021 continue
  sptr=sptr-1
  isto(sptr)=ptr4
  ptr4=ptr3
  sptr=sptr-1
  isto(sptr)=ptr3
  sptr=sptr-1
  isto(sptr)=ndx0
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+4)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  to=9006
  call use2
  env=isto(sptr)
  sptr=sptr+1
  ndx0=isto(ptr5+cnt1)
  ndx1=isto(env+6)
  ndx2=isto(env+7)
  i2=isto(ptr4+ndx0)
  a=rsto(base3+i2)
  rptr=rptr+1
  rsto(rptr)=a
  to=9022
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  i2=isto(ptr4+ndx0)
  rsto(base3+i2)=a
  ndx6=isto(sptr)
  ndx5=isto(sptr+1)
  ndx4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  ndx0=isto(sptr+5)
  ptr3=isto(sptr+6)
  ptr4=isto(sptr+7)
  sptr=sptr+8
  goto 9500
9022 continue
  sptr=sptr-1
  isto(sptr)=ptr4
  ptr4=ptr3
  sptr=sptr-1
  isto(sptr)=ptr3
  sptr=sptr-1
  isto(sptr)=ndx0
  sptr=sptr-1
  isto(sptr)=ndx1
  sptr=sptr-1
  isto(sptr)=ndx2
  sptr=sptr-1
  isto(sptr)=ndx4
  sptr=sptr-1
  isto(sptr)=ndx5
  sptr=sptr-1
  isto(sptr)=ndx6
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+4)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  to=9006
  call use2
  env=isto(sptr)
  sptr=sptr+1
  ndx0=isto(ptr5+cnt1)
  ndx1=isto(env+6)
  ndx2=isto(env+7)
  i2=isto(ptr4+ndx0)
  a=rsto(base3+i2)
  rptr=rptr+1
  rsto(rptr)=a
  to=9022
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  i2=isto(ptr4+ndx0)
  rsto(base3+i2)=a
  ndx6=isto(sptr)
  ndx5=isto(sptr+1)
  ndx4=isto(sptr+2)
  ndx2=isto(sptr+3)
  ndx1=isto(sptr+4)
  ndx0=isto(sptr+5)
  ptr3=isto(sptr+6)
  ptr4=isto(sptr+7)
  sptr=sptr+8
  goto 9500
end subroutine comb

!
! end of file comb.f90
!
