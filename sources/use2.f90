!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file use2.f90
!

!
! subroutine use2.
!

subroutine use2
  use tacsto
  implicit none
  !
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=0
  go to (9000, 9001, 9002, 9003, 9004, 9005, 9006 , 9007, 9008, 9009, 9010, 9011), to-8999
  stop 'invalid "to" reference in "use2".'
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 0002, 0002, 0002, 9514, 9515, 9516), from- 9500
0002 stop 'Invalid "from" reference in "use2".'
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
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  ndx1=isto(env+8)
  ndx2=isto(env+9)
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9501
  go to 9008
9501 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  xprndx=isto(env+3)
  to=9000
  call xpr1
  rptr=rptr-xprcnt
  k=ndx6-ndx4+1
  if(.not.(k.lt.1)) go to 5001
  stpflg=29
  go to 5000
5001 if(.not.(k.ne.xprcnt .and. xprcnt.gt.1)) go to 5002
  stpflg=41
  go to 5000
5002 continue
5000 continue
  if(.not.(stpflg.gt.0)) go to 5011
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  i0=isto(env+1)+2*(ndx5-1)
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
  tacs_a = rsto(rptr+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  i1=ptr2-4
  i2=ndx6-ndx4+1
  i=0
3001 if(.not.(i.lt.i2)) go to 3000
  i=i+1
  i1=i1+4
  isto(i1+2)=1
  i3=isto(i1)
  if(.not.(xprcnt.eq.1)) go to 5021
  rsto(i3)= tacs_a
  go to 5020
5021 continue
  rsto(i3)=rsto(rptr+i)
5020 continue
  go to 3001
3000 continue
  env=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  go to 9500
9001 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr4
  ptr4=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+3)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  k=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+3)
  if(.not.(k.gt.0)) go to 5031
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9502
  go to 9008
9502 from=isto(sptr)
  sptr=sptr+1
  isto(ptr2+1)=ptr4
  isto(ptr2+3)=1
  go to 5030
5031 continue
  i1=isto(env+4)
  k=isto(env+3)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  if(.not.(k.eq.6)) go to 5041
  i0=isto(env+29) +4*(i1-1)
  go to 5040
5041 continue
  i0=isto(env+36) +3*(i1-1)
5040 continue
  env=isto(sptr)
  sptr=sptr+1
  i2=isto(i0)
  k=ndx6-ndx4+1
  i3=ndx4-isto(base4+ndx5)
  ptr3=i2 +7*(i3-1)
  i=0
3011 if(.not.(i.lt.k)) go to 3010
  i=i+1
  ptr3=ptr3+7
  isto(ptr3+3)=ptr4
  isto(ptr3+6)=i
  go to 3011
3010 continue
5030 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ptr4=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  go to 9500
9002 continue
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
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  i=isto(env+1)
  k=isto(env+2)
  if(.not.(i.eq.1 .or. i.eq.3 .or. i.eq.7)) go to 5051
  xprndx=k
  to=9000
  call xpr1
  ndx3=xprcnt
  go to 5050
5051 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=k
  if(.not.(i.eq.2)) go to 5061
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9004
9503 from=isto(sptr)
  sptr=sptr+1
  go to 5060
5061 if(.not.(i.eq.4)) go to 5062
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9005
9504 from=isto(sptr)
  sptr=sptr+1
  go to 5060
5062 if(.not.(i.eq.5)) go to 5063
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9007
9505 from=isto(sptr)
  sptr=sptr+1
  go to 5060
5063 continue
5060 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx3=1
5050 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
  ndx1=isto(env+8)
  ndx2=isto(env+9)
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9003
9506 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  xprcnt=ndx3
  to=9000
  call xpr2
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  go to 9500
9003 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9507
  go to 9006
9507 from=isto(sptr)
  sptr=sptr+1
  if(.not.(ptr3.gt.0)) go to 5071
  i3=ptr3-7
  k=ndx6-ndx4+1
  i=0
3021 if(.not.(i.lt.k)) go to 3020
  i=i+1
  i3=i3+7
  isto(i3+4)=0
  go to 3021
3020 continue
  go to 5070
5071 continue
5070 continue
  ptr3=isto(sptr)
  sptr=sptr+1
  go to 9500
9004 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  tacs_a = zero
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)= tacs_a
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3031 if(.not.(env.gt.0)) go to 3030
  xprndx=isto(env+3)
  if(.not.(xprndx.gt.0)) go to 5081
  to=9000
  call xpr1
  go to 5080
5081 continue
  tacs_a = one
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)= tacs_a
5080 continue
  if(.not.(isto(env+1).eq.2)) go to 5091
  rsto(rptr)=-rsto(rptr)
  go to 5090
5091 continue
5090 continue
  i1=isto(env+4)
  if(.not.(i1.gt.0)) go to 5101
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=i1
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  tacs_a = rsto(rptr)
  rptr=rptr-1
  k=base3 +isto(base5+ndx5)-isto(base4+ndx5) +ndx4
  rsto(rptr) = rsto(rptr) + tacs_a * rsto(k)
  go to 5100
5101 continue
  tacs_a = rsto(rptr)
  rptr = rptr - 1
  rsto(rptr) = rsto(rptr) + tacs_a
5100 continue
  env=isto(env+0)
  go to 3031
3030 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx1=isto(env+1)
  ndx2=isto(env+2)
  to=9021
  call xpr1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  go to 9500
9005 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=rptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  ptr2=isto(env+3)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9508
  go to 9006
9508 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  to=9024
  call xpr1
  tacs_a = rsto(rptr)
  rptr=rptr-1
  tacs_b = rsto(rptr)
  rptr=rptr-1
  i0=base3 +isto(base5+ndx5)-isto(base4+ndx5) +ndx4
  tacs_c = rsto(i0)
  tacs_d = tacs_b * tacs_c + tacs_a
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr) = tacs_d
  ndx1=isto(env+5)
  ndx2=isto(env+6)
  to=9021
  call xpr1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9006 continue
  i1=isto(env+4)
  k=isto(env+3)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  if(.not.(k.eq.6)) go to 5111
  i0=isto(env+29) +4*(i1-1)
  go to 5110
5111 continue
  i0=isto(env+36) +3*(i1-1)
5110 continue
  i2=isto(i0)
  if(.not.(i2.gt.0)) go to 5121
  k=ndx4-isto(base4+ndx5)
  ptr3=i2+7*k
  go to 5120
5121 continue
  ptr3=0
5120 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9007 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=rptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+6)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9509
  go to 9006
9509 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9510
  go to 9008
9510 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  i1=isto(ptr2+2)
  if(.not.(i1.gt.1)) go to 5131
  i1=i1-2
  isto(ptr2+2)=i1
  go to 5130
5131 continue
5130 continue
  if(.not.(i1.eq.1)) go to 5141
  i2=isto(ptr2)
  tacs_a = rsto(i2)
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)= tacs_a
  go to 5140
5141 continue
  to=9028
  call xpr1
  tacs_a = rsto(rptr)
  rptr=rptr-1
  tacs_b = rsto(rptr)
  rptr=rptr-1
  i0=base3 +isto(base5+ndx5)-isto(base4+ndx5) +ndx4
  tacs_c = rsto(i0)
  tacs_d = tacs_b * tacs_c + tacs_a
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr) = tacs_d
5140 continue
  ndx1=isto(env+7)
  ndx2=isto(env+8)
  to=9023
  call xpr1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9008 continue
  i1=isto(env+4)
  k=isto(env+3)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  if(.not.(k.eq.6)) go to 5151
  i0=isto(env+29) +4*(i1-1)
  go to 5150
5151 continue
  i0=isto(env+36) +3*(i1-1)
5150 continue
  env=isto(sptr)
  sptr=sptr+1
  i2=isto(i0+2)
  k=ndx4 -isto(base4+ndx5)
  ptr2=i2 +4*k
  go to 9500
9009 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  ptr2=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx7
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  ndx7=isto(env+1)
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx7.eq.2)) go to 5261
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+5)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  env=isto(sptr)
  sptr=sptr+1
  k=isto(base5+ndx5)-isto(base4+ndx5)+ndx4
  tacs_a = rsto(base3+k)
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)= tacs_a
  go to 5260
5261 continue
5260 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9514
  go to 9006
9514 from=isto(sptr)
  sptr=sptr+1
  isto(ptr3+4)=0
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx7.eq.2)) go to 5271
  ptr1=rptr
  to=9024
  call xpr1
  tacs_b = rsto(ptr1+1)
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)= tacs_b
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  to=9011
  call xpr2
  if(.not.(flg.gt.0)) go to 5191
  stpflg=107
  stpi1=ndx4
  continue
  call errstp
  go to 5190
5191 continue
5190 continue
  flg=isto(sptr)
  sptr=sptr+1
  tacs_c = rsto(rptr)
  rptr=rptr-1
  tacs_a = rsto(ptr1+2)
  tacs_e = rsto(ptr1)
  rptr = ptr1
  rsto(rptr) = (tacs_e - tacs_a) * tacs_c
  ndx1=isto(env+6)
  ndx2=isto(env+7)
  to=9022
  call xpr1
  go to 5270
5271 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx11
  ndx11=isto(ptr3+3)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx14
  ndx14=isto(ptr3+6)
  stpl2=stpl1
  stpc2=stpc1
  stpl1=6
  stpc1=304
  stpi1=ndx4
  to=9014
  call xpr1
  ndx14=isto(sptr)
  sptr=sptr+1
  ndx11=isto(sptr)
  sptr=sptr+1
5270 continue
  i0=base3 +isto(base5+ndx5)-isto(base4+ndx5) +ndx4
  rsto(i0)=rsto(rptr)
  rptr=rptr-1
  ndx7=isto(sptr)
  sptr=sptr+1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9010 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=rptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  ptr2=isto(env+8)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx7
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  ndx7=isto(env+1)
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx7.eq.2)) go to 5241
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+5)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9515
  go to 9006
9515 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  to=9024
  call xpr1
  tacs_a = rsto(rptr)
  rptr=rptr-1
  tacs_b = rsto(rptr)
  rptr=rptr-1
  i0=base3 +isto(base5+ndx5)-isto(base4+ndx5) +ndx4
  tacs_c = rsto(i0)
  tacs_d = tacs_b * tacs_c + tacs_a
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr) = tacs_d
  ndx=isto(env+2)
  ptr1=rptr
  ptr2=isto(env+9)
  go to 5240
5241 continue
5240 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=ndx1
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9516
  go to 9006
9516 from=isto(sptr)
  sptr=sptr+1
  isto(ptr3+4)=0
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx7.eq.2)) go to 5251
  to=9024
  call xpr1
  tacs_b = rsto(ptr1+1)
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr) = tacs_b
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  to=9011
  call xpr2
  if(.not.(flg.gt.0)) go to 5201
  stpflg=108
  stpi1=ndx4
  continue
  call errstp
  go to 5200
5201 continue
5200 continue
  flg=isto(sptr)
  sptr=sptr+1
  tacs_c = rsto(rptr)
  rptr=rptr-1
  tacs_a = rsto(ptr1+2)
  tacs_e = rsto(ptr1)
  rptr=ptr1
  rsto(rptr) = (tacs_e - tacs_a) * tacs_c
  ndx1=isto(env+6)
  ndx2=isto(env+7)
  to=9022
  call xpr1
  go to 5250
5251 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx11
  ndx11=isto(ptr3+3)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx14
  ndx14=isto(ptr3+6)
  stpl2=stpl1
  stpc2=stpc1
  stpl1=7
  stpc1=310
  stpi1=ndx4
  to=9014
  call xpr1
  ndx14=isto(sptr)
  sptr=sptr+1
  ndx11=isto(sptr)
  sptr=sptr+1
5250 continue
  i0=base3 +isto(base5+ndx5)-isto(base4+ndx5) +ndx4
  rsto(i0)=rsto(rptr)
  rptr=rptr-1
  ndx7=isto(sptr)
  sptr=sptr+1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ptr3=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9011 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt1
  cnt1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3041 if(.not.(env.gt.0)) go to 3040
  i1=isto(env+1)
  if(.not.(i1.gt.0)) go to 5211
  k=isto(env+2)-1
  i=0
3061 if(.not.(i.lt.i1)) go to 3060
  i=i+1
  cbuff(cnt1+i:cnt1+i)=csto(k+i)
  go to 3061
3060 continue
  cnt1=cnt1+i1
  go to 5210
5211 continue
  xprndx=isto(env+3)
  to=9000
  call xpr1
  rptr=rptr-xprcnt
  tacs_a = rsto(rptr+1)
  write(cbuff(cnt1+1:cnt1+15),1001)
1001 format('               ')
  write (unit = cbuff(cnt1+1:cnt1+15), fmt = 1002) tacs_a
1002 format (g15.8)
  k = cnt1
  i = 0
3071 if (.not. (i .lt. 15)) go to 3070
  i = i + 1
  if(.not.(cbuff(cnt1+i:cnt1+i).ne.csto(64))) go to 5231
  k=k+1
  cbuff(k:k)=cbuff(cnt1+i:cnt1+i)
  go to 5230
5231 continue
5230 continue
  go to 3071
3070 continue
  cnt1=k
5210 continue
  env=isto(env+0)
  go to 3041
3040 continue
  write(unit06,1000) cbuff(1:cnt1)
1000 format(1x,a)
  env=isto(sptr)
  sptr=sptr+1
  cnt1=isto(sptr)
  sptr=sptr+1
  go to 9500
end subroutine use2

!
! end of file use2.f90
!
