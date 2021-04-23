!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: xref2.for
!
!
!     subroutine xref2.
!
subroutine xref2
  include 'tacsto.ftn'
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=0
  go to (9000, 9001, 9002, 9003, 9004, 9005, 9006, 9007, 9008, 9009, 9010, 9011, 9012, 9013, 9014, 9015, 9016), to-8999
  stop 'invalid "to" reference in "xref2".'
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510,9511, 9512, 9513, 9514, 9515, 9516, 9517, 9518, &
          0002, 0002, 0002, 0002, 9523, 9524, 9525), from- 9500
0002 stop 'invalid "from" reference in "xref2".'
910 stpflg=42
  stpi1=iptr
  stpi2=ilen-iptr
  continue
  call errstp
9000 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9501
  go to 9001
9501 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9502
  go to 9003
9502 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9005
9503 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9006
9504 from=isto(sptr)
  sptr=sptr+1
  go to 9500
9001 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  ndx5=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+35)
3001 if(.not.(env.gt.0)) go to 3000
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9525
  go to 9016
9525 from=isto(sptr)
  sptr=sptr+1
  k=isto(env+1)
  if(.not.(k.eq.381 .or. k.eq.383)) go to 5001
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  ndx3=isto(env+7)
  ndx4=isto(env+8)
  ndx1=isto(env+1)
  env=isto(sptr)
  sptr=sptr+1
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9002
9505 from=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.eq.0)) go to 5011
  if(.not.(ndx5.eq.0)) go to 5221
  ndx5=1
  go to 5220
5221 continue
5220 continue
  isto(iptr+1)=ndx3
  isto(iptr+2)=ndx4
  isto(iptr+3)=ndx1
  iptr=iptr+3
  cnt=cnt+1
  isto(env+3)=cnt
  go to 5010
5011 continue
  isto(env+3)=ndx
5010 continue
  go to 5000
5001 continue
5000 continue
  env=isto(env+0)
  go to 3001
3000 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+42)=cnt
  if(.not.(cnt.gt.0)) go to 5021
  isto(env+43)=ptr1
  go to 5020
5021 continue
  isto(env+43)=0
5020 continue
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9002 continue
  k=ptr1-1
  i=1
3011 if(.not.(i.le.cnt .and. isto(k+3*i).ne.ndx1)) go to 3010
  i=i+1
  go to 3011
3010 continue
  if(.not.(i.le.cnt)) go to 5031
  ndx=i
  go to 5030
5031 continue
5030 continue
  go to 9500
9003 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  ndx5=0
  isto(env+18)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+16)
3021 if(.not.(env.gt.0)) go to 3020
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+6)
  ndx0=isto(env+2)
  ndx3=isto(env+6)
  ndx4=isto(env+7)
  env=isto(sptr)
  sptr=sptr+1
  isto(env+1)=ndx0
  ndx=0
  if(.not.(cnt.gt.0)) go to 5041
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9004
9506 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5040
5041 continue
5040 continue
  if(.not.(ndx.gt.0)) go to 5051
  isto(env+2)=ndx
  go to 5050
5051 continue
  if(.not.(ndx5.eq.0)) go to 5231
  ndx5=1
  go to 5230
5231 continue
5230 continue
  cnt=cnt+1
  isto(env+2)=cnt
  isto(iptr+1)=ndx3
  isto(iptr+2)=ndx4
  isto(iptr+3)=ndx0
  iptr=iptr+3
5050 continue
  env=isto(env+0)
  go to 3021
3020 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+17)=cnt
  if(.not.(cnt.eq.0)) go to 5061
  isto(env+18)=0
  go to 5060
5061 continue
5060 continue
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9004 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+18)
  i=1
3031 if(.not.(i.le.cnt .and. isto(env+2).ne.ndx0)) go to 3030
  i=i+1
  env=env+3
  go to 3031
3030 continue
  if(.not.(i.le.cnt)) go to 5071
  ndx=i
  go to 5070
5071 continue
5070 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9005 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+22)
3041 if(.not.(env.gt.0 .and. stpflg.eq.0)) go to 3040
  if(.not.(isto(env+2).gt.0)) go to 5081
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx0=isto(env+2)
  ndx3=isto(env+6)
  ndx4=isto(env+7)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  cnt=isto(env+17)
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9507
  go to 9004
9507 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.gt.0)) go to 5091
  isto(env+3)=ndx
  go to 5090
5091 continue
  stpflg=74
5090 continue
  go to 5080
5081 continue
  isto(env+3)=0
5080 continue
  env=isto(env+0)
  go to 3041
3040 continue
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+19)
3051 if(.not.(env.gt.0 .and. stpflg.eq.0)) go to 3050
  if(.not.(isto(env+2).gt.0)) go to 5101
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+3)
  ndx0=isto(env+2)
  ndx3=isto(env+6)
  ndx4=isto(env+7)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  cnt=isto(env+17)
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9508
  go to 9004
9508 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.gt.0)) go to 5111
  isto(env+1)=ndx
  go to 5110
5111 continue
  stpflg=76
5110 continue
  go to 5100
5101 continue
  isto(env+1)=0
5100 continue
  env=isto(env+0)
  go to 3051
3050 continue
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+21)
3061 if(.not.(env.gt.0 .and. stpflg.eq.0)) go to 3060
  if(.not.(isto(env+3).gt.0)) go to 5121
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx0=isto(env+2)
  ndx3=isto(env+6)
  ndx4=isto(env+7)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+5)
  cnt=isto(env+17)
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9509
  go to 9004
9509 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.gt.0)) go to 5131
  isto(env+1)=ndx
  go to 5130
5131 continue
  stpflg=75
5130 continue
  go to 5120
5121 continue
  isto(env+1)=0
5120 continue
  env=isto(env+0)
  go to 3061
3060 continue
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+35)
3071 if(.not.(env.gt.0 .and. stpflg.eq.0)) go to 3070
  if(.not.(isto(env+4).gt.0)) go to 5141
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  ndx0=isto(env+2)
  ndx3=isto(env+7)
  ndx4=isto(env+8)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+5)
  cnt=isto(env+17)
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9510
  go to 9004
9510 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.gt.0)) go to 5151
  isto(env+5)=ndx
  go to 5150
5151 continue
  stpflg=77
5150 continue
  go to 5140
5141 continue
  isto(env+5)=0
5140 continue
  env=isto(env+0)
  go to 3071
3070 continue
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+20)
3081 if(.not.(env.gt.0 .and. stpflg.eq.0)) go to 3080
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  ndx0=isto(env+2)
  ndx3=isto(env+6)
  ndx4=isto(env+7)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  cnt=isto(env+17)
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9511
  go to 9004
9511 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx.gt.0)) go to 5161
  isto(env+1)=ndx
  go to 5160
5161 continue
  stpflg=78
5160 continue
  env=isto(env+0)
  go to 3081
3080 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(stpflg.gt.0)) go to 5171
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5170
5171 continue
5170 continue
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9006 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+41)
3091 if(.not.(env.gt.0)) go to 3090
  j=isto(env+2)
  k=j+isto(env+1)-1
  ptr1=iptr-1
  isto(env+9)=iptr+1
  iptr=iptr +2*isto(env+8)
  isto(ishenv+56)=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
3101 if(.not.(env.gt.0)) go to 3100
  k=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+5)
  if(.not.(k.eq.1)) go to 5181
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9512
  go to 9007
9512 from=isto(sptr)
  sptr=sptr+1
  go to 5180
5181 if(.not.(k.eq.2)) go to 5182
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9513
  go to 9012
9513 from=isto(sptr)
  sptr=sptr+1
  go to 5180
5182 if(.not.(k.eq.3)) go to 5183
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9514
  go to 9013
9514 from=isto(sptr)
  sptr=sptr+1
  go to 5180
5183 continue
5180 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ndx1
  isto(env+3)=ndx2
  env=isto(env+0)
  go to 3101
3100 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3091
3090 continue
  env=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  go to 9500
9007 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
  ndx1=isto(env+2)
  ndx2=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  if(.not.(ndx.eq.2)) go to 5191
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9515
  go to 9008
9515 from=isto(sptr)
  sptr=sptr+1
  go to 5190
5191 if(.not.(ndx.eq.4)) go to 5192
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9516
  go to 9009
9516 from=isto(sptr)
  sptr=sptr+1
  go to 5190
5192 if(.not.(ndx.eq.5)) go to 5193
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9517
  go to 9010
9517 from=isto(sptr)
  sptr=sptr+1
  go to 5190
5193 if(.not.(ndx.eq.3 .or. ndx.eq.7)) go to 5194
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9518
  go to 9011
9518 from=isto(sptr)
  sptr=sptr+1
  go to 5190
5194 continue
5190 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  go to 9500
9008 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3111 if(.not.(env.gt.0)) go to 3110
  i1=isto(env+4)
  if(.not.(i1.gt.0)) go to 5201
  ptr1=ptr1+2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=i1
  isto(ptr1)=isto(env+2)
  isto(ptr1+1)=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  isto(ishenv+56)=isto(ishenv+56)+1
  isto(env+2)=isto(ishenv+56)
  go to 5200
5201 continue
5200 continue
  env=isto(env+0)
  go to 3111
3110 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9009 continue
  ptr1=ptr1+2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  isto(ptr1)=isto(env+2)
  isto(ptr1+1)=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  isto(ishenv+56)=isto(ishenv+56)+1
  isto(env+2)=isto(ishenv+56)
  go to 9500
9010 continue
  ptr1=ptr1+2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+6)
  isto(ptr1)=isto(env+2)
  isto(ptr1+1)=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  isto(ishenv+56)=isto(ishenv+56)+1
  isto(env+4)=isto(ishenv+56)
  go to 9500
9011 continue
  ptr1=ptr1+2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=env+6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+15)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  isto(ptr1)=isto(env+1)
  isto(ptr1+1)=isto(env+9)
  env=isto(sptr)
  sptr=sptr+1
  isto(ishenv+56)=isto(ishenv+56)+1
  isto(env+8)=isto(ishenv+56)
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9012 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx1=isto(env+2)
  ndx2=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  ptr1=ptr1+2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+5)
  isto(ptr1)=isto(env+2)
  isto(ptr1+1)=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  isto(ishenv+56)=isto(ishenv+56)+1
  isto(env+3)=isto(ishenv+56)
  go to 9500
9013 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  ndx1=isto(env+2)
  ndx2=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  ptr1=ptr1+2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+5)
  isto(ptr1)=isto(env+2)
  isto(ptr1+1)=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  isto(ishenv+56)=isto(ishenv+56)+1
  isto(env+3)=isto(ishenv+56)
  go to 9500
9014 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  ptr0=datenv
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9523
  go to 9015
9523 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+8)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9524
  go to 9015
9524 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
  go to 9500
9015 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+9)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
3121 if(.not.(env.gt.0)) go to 3120
  if(.not.(isto(env+8).eq.0 .and. isto(env+5).eq.ptr0)) go to 5241
  i5=isto(env+2)
  i4=isto(base4+i5)
  i6=isto(base6+i5)
  if(.not.(i4.ne.1 .or. i6.ne.1)) go to 5251
  stpflg=130
  stpi1=i4
  stpi2=i6
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  continue
  call errstp
  go to 5250
5251 continue
5250 continue
  go to 5240
5241 continue
5240 continue
  env=isto(env+0)
  go to 3121
3120 continue
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+3)
3131 if(.not.(env.gt.0)) go to 3130
  i5=isto(env+1)
  if(.not.(isto(env+9).eq.0 .and. isto(env+5).eq.ptr0 .and. isto(env+13).ne.7 .and. i5.gt.0)) go to 5261
  i4=isto(base4+i5)
  i6=isto(base6+i5)
  if(.not.(i4.ne.1 .or. i6.ne.1)) go to 5271
  stpflg=130
  stpi1=i4
  stpi2=i6
  stpl1=isto(env+7)
  stpc1=isto(env+8)
  continue
  call errstp
  go to 5270
5271 continue
5270 continue
  go to 5260
5261 continue
5260 continue
  env=isto(env+0)
  go to 3131
3130 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9016 continue
  i0=isto(env+1)
  stpl1=isto(i0+1)
  stpc1=isto(i0)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  k=isto(env+2)
  if(.not.(k.ne.6 .and. k.ne.7)) go to 5281
  stpflg=133
  stpl2=isto(env+7)
  stpc2=isto(env+8)
  continue
  call errstp
  go to 5280
5281 continue
5280 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
end subroutine xref2
!
!     end of file: xref2.for
!
