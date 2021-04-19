!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: xref1.for
!
!
!     subroutine xref1.
!
subroutine xref1
  include 'tacsto.ins'
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=0
  go to (9000, 9001, 9002, 9003, 9004, 9005, 9006, 9007, 9008, 9009, 9010, 9011, 9012, 9013, 9014, 9015, 9016, 9017, 9018, 9019, &
       9020, 9021), to-8999
  stop 'invalid "to" reference in "xref1".'
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, 9516, 9517, 9518, 9519, &
          9520, 9521, 9522, 9523, 9524, 9525, 9526, 9527, 9528, 9529, 9530, 9531, 9532, 9533, 9534, 9535, 9536, 9537, 9538, 9539, &
          9540, 9541, 9542, 9543, 9544, 9545, 9546, 9547, 9548, 9549, 9550), from-9500
  stop 'invalid "from" reference in xref1.'
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
  go to 9010
9502 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9012
9503 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9019
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
  isto(sptr)=ndx0
  ndx0=0
  isto(env+1)=ptr1
  j=isto(env-3)
  k=j+isto(env-4)-1
  i0=base1 +417
  k=8
  i=0
3001 if(.not.(i.lt.k)) go to 3000
  i=i+1
  i0=i0+2
  cnt=cnt+1
  isto(iptr+1)=isto(i0+1)
  isto(iptr+2)=isto(i0)
  iptr=iptr+2
  go to 3001
3000 continue
  isto(env+4)=k
  cnt=k
  ndx0=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+6)
3011 if(.not.(env.gt.0)) go to 3010
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9002
9505 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ndx
  isto(env+3)=ndx-ndx0
  env=isto(env+0)
  go to 3011
3010 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+5)=cnt-ndx0
  ndx0=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+8)
3021 if(.not.(env.gt.0)) go to 3020
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9002
9506 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ndx
  isto(env+3)=ndx-ndx0
  env=isto(env+0)
  go to 3021
3020 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+7)=cnt-ndx0
  ndx0=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+10)
3031 if(.not.(env.gt.0)) go to 3030
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9507
  go to 9002
9507 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ndx
  isto(env+3)=ndx-ndx0
  env=isto(env+0)
  go to 3031
3030 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+9)=cnt-ndx0
  ndx0=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+12)
3041 if(.not.(env.gt.0)) go to 3040
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9508
  go to 9002
9508 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ndx
  isto(env+3)=ndx-ndx0
  env=isto(env+0)
  go to 3041
3040 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+11)=cnt-ndx0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  ndx5=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx6
  ndx6=ndx0
  ndx0=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+24)
3051 if(.not.(env.gt.0)) go to 3050
  ndx1=cnt
  isto(env+1)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+3)
3061 if(.not.(env.gt.0)) go to 3060
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9509
  go to 9008
9509 from=isto(sptr)
  sptr=sptr+1
  isto(env+1)=ndx
  isto(env+2)=ndx-ndx0
  isto(env+3)=ndx-ndx1
  env=isto(env+0)
  go to 3061
3060 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3051
3050 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx6=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  ndx0=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+32)
3071 if(.not.(env.gt.0)) go to 3070
  ndx1=cnt
  isto(env+1)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+3)
3081 if(.not.(env.gt.0)) go to 3080
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9510
  go to 9009
9510 from=isto(sptr)
  sptr=sptr+1
  isto(env+3)=ndx
  isto(env+4)=ndx-ndx0
  isto(env+5)=ndx-ndx1
  env=isto(env+0)
  go to 3081
3080 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3071
3070 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  isto(env+0)=cnt
  ndx0=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9002 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  ndx3=isto(env+0)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  ndx4=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9511
  go to 9005
9511 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5001
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9512
  go to 9006
9512 from=isto(sptr)
  sptr=sptr+1
  go to 5000
5001 continue
5000 continue
  if(.not.(flg.gt.0)) go to 5011
  stpflg=46
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5010
5011 continue
5010 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  ndx1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=ptr1-2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9513
  go to 9003
9513 from=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5021
  if(.not.(ndx1.le.ndx0)) go to 5031
  stpflg=48
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5030
5031 continue
  ndx=ndx1
5030 continue
  go to 5020
5021 continue
  cnt=cnt+1
  ndx=cnt
  iptr=iptr+2
  isto(iptr-1)=ndx3
  isto(iptr  )=ndx4
5020 continue
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
  go to 9500
9003 continue
3091 if(.not.(ndx1.lt.ndx2 .and. flg.eq.0)) go to 3090
  ndx1=ndx1+1
  ndx0=ndx0+2
  if(.not.(isto(ndx0).eq.ndx3)) go to 5041
  j=0
  i2=isto(ndx0+1)
3101 if(.not.(j.lt.ndx3 .and. csto(i2+j).eq.csto(ndx4+j))) go to 3100
  j=j+1
  go to 3101
3100 continue
  if(.not.(j.eq.ndx3)) go to 5051
  flg=1
  go to 5050
5051 continue
5050 continue
  go to 5040
5041 continue
5040 continue
  go to 3091
3090 continue
  go to 9500
9004 continue
3111 if(.not.(ndx1.lt.ndx2 .and. flg.eq.0)) go to 3110
  ndx1=ndx1+1
  ndx0=ndx0+2
  if(.not.(isto(ndx0+1).eq.ndx3)) go to 5061
  j=0
  i2=isto(ndx0)
3121 if(.not.(j.lt.ndx3 .and. csto(i2+j).eq.csto(ndx4+j))) go to 3120
  j=j+1
  go to 3121
3120 continue
  if(.not.(j.eq.ndx3)) go to 5071
  flg=1
  go to 5070
5071 continue
5070 continue
  go to 5060
5061 continue
5060 continue
  go to 3111
3110 continue
  go to 9500
9005 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=1
  ndx0=base1 +isto(env+7) -2
  ndx2=isto(env+6)
  env=isto(sptr)
  sptr=sptr+1
  ndx1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9514
  go to 9004
9514 from=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  go to 9500
9006 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=1
  ndx0=base1 +isto(env+9) -2
  ndx2=isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  ndx1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9515
  go to 9004
9515 from=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  go to 9500
9007 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=1
  ndx0=base1 +isto(env+5) -2
  ndx2=isto(env+4)
  env=isto(sptr)
  sptr=sptr+1
  ndx1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9516
  go to 9004
9516 from=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  go to 9500
9008 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  ndx3=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  ndx4=isto(env+5)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9517
  go to 9005
9517 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5081
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9518
  go to 9006
9518 from=isto(sptr)
  sptr=sptr+1
  go to 5080
5081 continue
5080 continue
  if(.not.(flg.eq.0)) go to 5091
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9519
  go to 9007
9519 from=isto(sptr)
  sptr=sptr+1
  go to 5090
5091 continue
5090 continue
  if(.not.(flg.gt.0)) go to 5101
  stpflg=49
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5100
5101 continue
5100 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=ptr1+2*ndx5-2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  ndx1=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=ndx6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9520
  go to 9003
9520 from=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5111
  stpflg=50
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5110
5111 continue
5110 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=ptr1+2*ndx1-2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9521
  go to 9003
9521 from=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5121
  stpflg=51
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5120
5121 continue
5120 continue
  cnt=cnt+1
  ndx=cnt
  iptr=iptr+2
  isto(iptr-1)=ndx3
  isto(iptr  )=ndx4
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9009 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  ndx3=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  ndx4=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9522
  go to 9005
9522 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5131
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9523
  go to 9006
9523 from=isto(sptr)
  sptr=sptr+1
  go to 5130
5131 continue
5130 continue
  if(.not.(flg.eq.0)) go to 5141
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9524
  go to 9007
9524 from=isto(sptr)
  sptr=sptr+1
  go to 5140
5141 continue
5140 continue
  if(.not.(flg.gt.0)) go to 5151
  stpflg=47
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5150
5151 continue
5150 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=ptr1+2*ndx1-2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=cnt
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9525
  go to 9003
9525 from=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5161
  stpflg=52
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5160
5161 continue
5160 continue
  cnt=cnt+1
  ndx=cnt
  isto(iptr+1)=ndx3
  isto(iptr+2)=ndx4
  iptr=iptr+2
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9010 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  ptr2=iptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=isto(env+4) +isto(env+5)+isto(env+7) +isto(env+9)+isto(env+11)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt
  cnt=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+36)
  if(.not.(env.gt.0)) go to 5781
  go to 5780
5781 continue
5780 continue
3131 if(.not.(env.gt.0)) go to 3130
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9526
  go to 9011
9526 from=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ndx
  env=isto(env+0)
  go to 3131
3130 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+37)=cnt
  if(.not.(cnt.eq.0)) go to 5171
  isto(env+38)=0
  go to 5170
5171 continue
  isto(env+38)=ptr2
5170 continue
  ndx=isto(sptr)
  sptr=sptr+1
  cnt=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9011 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+3)
  ndx3=isto(env+0)
  ndx4=isto(env+1)
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=ptr1-2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  ndx1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9527
  go to 9003
9527 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5181
  stpflg=53
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5180
5181 continue
5180 continue
  ndx=ndx1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  i0=ptr2-1
  i=0
  flg=0
3141 if(.not.(i.lt.cnt .and. flg.eq.0)) go to 3140
  i=i+1
  i0=i0+3
  if(.not.(isto(i0).eq.ndx)) go to 5191
  flg=1
  go to 5190
5191 continue
5190 continue
  go to 3141
3140 continue
  if(.not.(flg.eq.0)) go to 5201
  cnt=cnt+1
  isto(iptr+1)=ndx3
  isto(iptr+2)=ndx4
  isto(iptr+3)=ndx
  iptr=iptr+3
  go to 5200
5201 continue
5200 continue
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9012 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt8
  cnt8=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt9
  cnt9=cnt8+isto(env+5)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt10
  cnt10=cnt9+isto(env+7)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt11
  cnt11=cnt10+isto(env+9)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt12
  cnt12=cnt11+isto(env+11)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt13
  cnt13=cnt12+isto(env+23)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  ptr2=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
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
  isto(sptr)=from
  from=9528
  go to 9013
9528 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9529
  go to 9014
9529 from=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  cnt13=isto(sptr)
  sptr=sptr+1
  cnt12=isto(sptr)
  sptr=sptr+1
  cnt11=isto(sptr)
  sptr=sptr+1
  cnt10=isto(sptr)
  sptr=sptr+1
  cnt9=isto(sptr)
  sptr=sptr+1
  cnt8=isto(sptr)
  sptr=sptr+1
  cnt6=isto(sptr)
  sptr=sptr+1
  cnt5=isto(sptr)
  sptr=sptr+1
  cnt4=isto(sptr)
  sptr=sptr+1
  cnt3=isto(sptr)
  sptr=sptr+1
  cnt2=isto(sptr)
  sptr=sptr+1
  cnt1=isto(sptr)
  sptr=sptr+1
  go to 9500
9013 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+3)
3151 if(.not.(env.gt.0)) go to 3150
  if(.not.(isto(env+13).ne.7)) go to 5211
  ndx3=isto(env+7)
  ndx4=isto(env+8)
  ndx=0
  flg=0
  ndx5=isto(env+4)
  k=isto(env+6)
  if(.not.(k.gt.0)) go to 5221
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=k
  cnt1=isto(env+4)
  cnt2=cnt1+isto(env+5)
  cnt3=cnt2+isto(env+7)
  cnt4=cnt3+isto(env+9)
  cnt5=cnt4+isto(env+11)
  ptr1=isto(env+1)
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(ndx5.eq.44 .or. ndx5.eq.45 .or. ndx5.eq.52)) go to 5231
  ndx0=ptr1-2
  ndx1=0
  ndx2=cnt1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9530
  go to 9003
9530 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5241
  ndx=ndx1
  go to 5240
5241 continue
5240 continue
  go to 5230
5231 continue
5230 continue
  if(.not.(flg.eq.0 .and. (ndx5.eq.18 .or. (ndx5.ge.44 .and. ndx5.le.57)))) go to 5251
  ndx0=ptr1+2*cnt2-2
  ndx1=cnt2
  ndx2=cnt3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9531
  go to 9003
9531 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5261
  ndx=ndx1
  go to 5260
5261 continue
5260 continue
  go to 5250
5251 continue
5250 continue
  if (.not. (flg.eq.0 .and. (ndx5.eq.44 .or. ndx5.eq.45 .or. ndx5.eq.47 .or. ndx5.eq.52 .or. ndx5.eq.53 .or. ndx5.eq.54))) &
       go to 5271
  ndx0=ptr1+2*cnt3-2
  ndx1=cnt3
  ndx2=cnt4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9532
  go to 9003
9532 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5281
  ndx=ndx1
  go to 5280
5281 continue
5280 continue
  go to 5270
5271 continue
5270 continue
  if(.not.(flg.eq.0 .and. ndx5.eq.52)) go to 5291
  ndx0=ptr1+2*cnt4-2
  ndx1=cnt4
  ndx2=cnt5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9533
  go to 9003
9533 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5301
  ndx=ndx1
  go to 5300
5301 continue
5300 continue
  go to 5290
5291 continue
5290 continue
  if(.not.(flg.gt.0)) go to 5311
  isto(env+5)=isto(env+6)
  isto(env+1)=ndx
  if(.not.(ndx.gt.cnt4)) go to 5321
  ndx=ndx-cnt4
  k=7
  go to 5320
5321 if(.not.(ndx.gt.cnt3)) go to 5322
  ndx=ndx-cnt3
  k=6
  go to 5320
5322 if(.not.(ndx.gt.cnt2)) go to 5323
  ndx=ndx-cnt2
  k=5
  go to 5320
5323 continue
  k=3
5320 continue
  isto(env+3)=ndx
  isto(env+2)=k
  go to 5310
5311 continue
5310 continue
  go to 5220
5221 continue
5220 continue
  if(.not.(flg.eq.0)) go to 5331
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=1
  ndx0=base1 +isto(env+9) -2
  ndx2=9+isto(env+8)
  env=isto(sptr)
  sptr=sptr+1
  ndx1=9
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9534
  go to 9004
9534 from=isto(sptr)
  sptr=sptr+1
  if(.not. (flg .eq. 0 .and. ((ndx5 .ge. 2 .and. ndx5 .le. 5) .or. ndx5 .eq. 13 .or. ndx5 .eq. 15 .or. ndx5 .eq. 20 .or. &
       ndx5 .eq. 21 .or. ndx5 .eq. 25 .or. (ndx5 .ge. 27 .and. ndx5 .le. 45) .or. ndx5 .eq. 47 .or. ndx5 .eq. 48 &
       .or. ndx5 .eq. 52 .or. ndx5 .eq. 53 .or. ndx5 .eq. 54 .or. (ndx5 .ge. 59 .and. ndx5 .le. 66 .and. ndx5.ne.63)))) go to 5341
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=1
  ndx0=base1 +isto(env+7) -2
  ndx2=isto(env+6)
  env=isto(sptr)
  sptr=sptr+1
  ndx1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9535
  go to 9004
9535 from=isto(sptr)
  sptr=sptr+1
  go to 5340
5341 continue
5340 continue
  if(.not.(flg.gt.0)) go to 5351
  isto(env+1)=-ndx1
  if(.not.(ndx1.gt.9)) go to 5361
  ndx1=ndx1-9
  k=2
  go to 5360
5361 continue
  k=1
5360 continue
  isto(env+3)=ndx1
  isto(env+2)=k
  go to 5350
5351 continue
5350 continue
  go to 5330
5331 continue
5330 continue
  if(.not.(flg.eq.0)) go to 5371
  cnt1=cnt8
  cnt2=cnt9
  cnt3=cnt10
  cnt4=cnt11
  cnt5=cnt12
  cnt6=cnt13
  ptr1=ptr2
  if(.not.((ndx5.eq.2 .or. ndx5.eq.3) .and. isto(env+12).gt.0)) go to 5381
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+12)
  ndx0=ptr1 +2*isto(env+1) -2
  ndx1=isto(env+1)
  ndx2=ndx1+isto(env+2)
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9536
  go to 9003
9536 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5391
  ndx=ndx1
  go to 5390
5391 continue
5390 continue
  go to 5380
5381 continue
5380 continue
  if(.not.(flg.eq.0 .and. isto(env+11).gt.0 .and. (ndx5.eq.4 .or. ndx5.eq.5 .or. (ndx5.ge.25 .and. ndx5.le.45) .or. ndx5.eq.47 &
       .or. ndx5.eq.48  .or. ndx5.eq.53 .or. ndx5.eq.54 .or. (ndx5.ge.59 .and. ndx5.le.66 .and. ndx5.ne.63)))) go to 5401
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+11)
  ndx0=ptr1 +2*isto(env+1) -2
  ndx1=isto(env+1)
  ndx2=ndx1+isto(env+2)
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9537
  go to 9003
9537 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5411
  ndx=ndx1
  go to 5410
5411 continue
5410 continue
  go to 5400
5401 continue
5400 continue
  if(.not.(flg.eq.0 .and. (ndx5.eq.4 .or. ndx5.eq.5 .or. ndx5.eq.13 .or. ndx5.eq.15 .or. ndx5.eq.20 .or. ndx5.eq.21 .or. ndx5 &
       .eq. 25 .or. (ndx5.ge.27 .and. ndx5.le.43) .or. ndx5.eq.47 .or. ndx5.eq.53 .or. ndx5.eq.54 .or. (ndx5.ge.59 .and. ndx5 &
       .le. 66 .and. ndx5.ne.63)))) go to 5421
  ndx0=ptr1-2
  ndx1=0
  ndx2=cnt1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9538
  go to 9003
9538 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5431
  ndx=ndx1
  go to 5430
5431 continue
5430 continue
  go to 5420
5421 continue
5420 continue
  if(.not.(flg.eq.0 .and. ndx5.ge.4 .and. ndx5.le.66)) go to 5441
  ndx0=ptr1+2*cnt1-2
  ndx1=cnt1
  ndx2=cnt2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9539
  go to 9003
9539 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5451
  ndx=ndx1
  go to 5450
5451 continue
5450 continue
  go to 5440
5441 continue
5440 continue
  if(.not.(flg.eq.0 .and.  ((ndx5.ge.4 .and. ndx5.le.6) .or. (ndx5.ge.9 .and. ndx5.le.66)))) go to 5461
  ndx0=ptr1+2*cnt2-2
  ndx1=cnt2
  ndx2=cnt3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9540
  go to 9003
9540 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5471
  ndx=ndx1
  go to 5470
5471 continue
5470 continue
  go to 5460
5461 continue
5460 continue
  if(.not.(flg.eq.0 .and.  (ndx5.eq.4 .or. ndx5.eq.5 .or. ndx5.eq.13 .or. ndx5.eq.15 .or. ndx5.eq.20 .or. ndx5.eq.21 &
       .or. (ndx5.ge.25 .and. ndx5.le.45)  .or. ndx5.eq.47 .or. ndx5.eq.48 .or. ndx5.eq.52 .or. ndx5.eq.53 .or. ndx5 &
       .eq.54 .or. (ndx5.ge.59 .and. ndx5 .le. 66 .and. ndx5.ne.63)))) go to 5481
  ndx0=ptr1+2*cnt3-2
  ndx1=cnt3
  ndx2=cnt4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9541
  go to 9003
9541 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5491
  ndx=ndx1
  go to 5490
5491 continue
5490 continue
  go to 5480
5481 continue
5480 continue
  if(.not.(flg.eq.0 .and. (ndx5.eq.4 .or. ndx5.eq.5 .or. (ndx5.ge.25 .and. ndx5.le.45) .or. ndx5.eq.47 .or. ndx5.eq.48 .or. ndx5 &
       .eq.52 .or. ndx5.eq.53  .or. ndx5.eq.54 .or. (ndx5.ge.59 .and. ndx5.le.66 .and. ndx5.ne.63)))) go to 5501
  ndx0=ptr1+2*cnt4-2
  ndx1=cnt4
  ndx2=cnt5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9542
  go to 9003
9542 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5511
  ndx=ndx1
  go to 5510
5511 continue
5510 continue
  go to 5500
5501 continue
5500 continue
  if(.not.(flg.gt.0)) go to 5521
  isto(env+1)=ndx1
  if(.not.(ndx.gt.cnt6)) go to 5531
  ndx=ndx-cnt6
  k=9
  go to 5530
5531 if(.not.(ndx.gt.cnt5)) go to 5532
  ndx=ndx-cnt5
  k=8
  go to 5530
5532 if(.not.(ndx.gt.cnt4)) go to 5533
  ndx=ndx-cnt4
  k=7
  go to 5530
5533 if(.not.(ndx.gt.cnt3)) go to 5534
  ndx=ndx-cnt3
  k=6
  go to 5530
5534 if(.not.(ndx.gt.cnt2)) go to 5535
  ndx=ndx-cnt2
  k=5
  go to 5530
5535 if(.not.(ndx.gt.cnt1)) go to 5536
  ndx=ndx-cnt1
  k=4
  go to 5530
5536 continue
  k=3
5530 continue
  isto(env+3)=ndx
  isto(env+2)=k
  go to 5520
5521 continue
5520 continue
  go to 5370
5371 continue
5370 continue
  if(.not.(flg.eq.0)) go to 5541
  stpflg=54
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5540
5541 continue
5540 continue
  go to 5210
5211 continue
5210 continue
  env=isto(env+0)
  go to 3151
3150 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  go to 9500
9014 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
3161 if(.not.(env.gt.0)) go to 3160
  ndx3=isto(env+6)
  ndx4=isto(env+7)
  ndx5=isto(env+1)
  ndx=0
  flg=0
  k=isto(env+5)
  if(.not.(k.ne.datenv)) go to 5551
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=k
  cnt1=isto(env+4)
  cnt2=cnt1+isto(env+5)
  cnt3=cnt2+isto(env+7)
  cnt4=cnt3+isto(env+9)
  env=isto(sptr)
  sptr=sptr+1
  go to 5550
5551 continue
  cnt1=cnt8
  cnt2=cnt9
  cnt3=cnt10
  cnt4=cnt11
5550 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=k
  if(.not.(ndx5.eq.18)) go to 5561
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9543
  go to 9015
9543 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5571
  stpflg=55
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5570
5571 continue
5570 continue
  go to 5560
5561 if(.not.(ndx5.eq.19)) go to 5562
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9544
  go to 9016
9544 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5581
  stpflg=56
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5580
5581 continue
5580 continue
  go to 5560
5562 if(.not.(ndx5.eq.7 .or. ndx5.eq.16 .or. ndx5.eq.20)) go to 5563
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9545
  go to 9017
9545 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5591
  if(.not.(ndx5.eq.16)) go to 5601
  stpflg=57
  go to 5600
5601 if(.not.(ndx5.eq.7)) go to 5602
  stpflg=58
  go to 5600
5602 if(.not.(ndx5.eq.20)) go to 5603
  stpflg=72
  go to 5600
5603 continue
5600 continue
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5590
5591 continue
5590 continue
  go to 5560
5563 if(.not.(ndx5.eq.8 .or. ndx5.eq.13 .or. ndx5.eq.14 .or. (ndx5.eq.17 .and. isto(ishenv+16).gt.0))) go to 5564
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9546
  go to 9018
9546 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5611
  if(.not.(ndx5.eq.8)) go to 5621
  stpflg=59
  go to 5620
5621 if(.not.(ndx5.eq.13)) go to 5622
  stpflg=60
  go to 5620
5622 if(.not.(ndx5.eq.14)) go to 5623
  stpflg=61
  go to 5620
5623 if(.not.(ndx5.eq.17)) go to 5624
  stpflg=62
  go to 5620
5624 continue
5620 continue
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5610
5611 continue
5610 continue
  go to 5560
5564 if(.not.(ndx5.eq.17)) go to 5565
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9550
  go to 9021
9550 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5841
  stpflg=126
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5840
5841 continue
5840 continue
  go to 5560
5565 continue
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9547
  go to 9017
9547 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.eq.0)) go to 5631
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9548
  go to 9018
9548 from=isto(sptr)
  sptr=sptr+1
  go to 5630
5631 continue
5630 continue
  if(.not.(flg.eq.0)) go to 5641
  if(.not.(ndx5.eq.1)) go to 5651
  stpflg=63
  go to 5650
5651 if(.not.(ndx5.eq.2)) go to 5652
  stpflg=64
  go to 5650
5652 if(.not.(ndx5.eq.3)) go to 5653
  stpflg=65
  go to 5650
5653 if(.not.(ndx5.eq.9)) go to 5654
  stpflg=66
  go to 5650
5654 if(.not.(ndx5.eq.10)) go to 5655
  stpflg=67
  go to 5650
5655 if(.not.(ndx5.eq.11)) go to 5656
  stpflg=68
  go to 5650
5656 if(.not.(ndx5.eq.21)) go to 5657
  stpflg=69
  go to 5650
5657 if(.not.(ndx5.eq.15)) go to 5658
  stpflg=70
  go to 5650
5658 if(.not.(ndx5.eq.12)) go to 5659
  stpflg=71
  go to 5650
5659 continue
5650 continue
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
  go to 5640
5641 continue
5640 continue
5560 continue
  env=isto(sptr)
  sptr=sptr+1
  isto(env+2)=ndx
  if(.not.(ndx.gt.cnt4)) go to 5661
  ndx=ndx-cnt4
  k=7
  go to 5660
5661 if(.not.(ndx.gt.cnt3)) go to 5662
  ndx=ndx-cnt3
  k=6
  go to 5660
5662 if(.not.(ndx.gt.cnt2)) go to 5663
  ndx=ndx-cnt2
  k=5
  go to 5660
5663 if(.not.(ndx.gt.cnt1)) go to 5664
  ndx=ndx-cnt1
  k=4
  go to 5660
5664 continue
  k=3
5660 continue
  isto(env+4)=ndx
  isto(env+3)=k
  env=isto(env+0)
  go to 3161
3160 continue
  env=isto(sptr)
  sptr=sptr+1
  ndx5=isto(sptr)
  sptr=sptr+1
  go to 9500
9015 continue
  k=isto(env+37)
  i=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+38)
3171 if(.not.(i.lt.k .and. flg.eq.0)) go to 3170
  i=i+1
  if(.not.(isto(env+0).eq.ndx3)) go to 5791
  j=0
  i2=isto(env+1)
3241 if(.not.(j.lt.ndx3 .and. csto(i2+j).eq.csto(ndx4+j))) go to 3240
  j=j+1
  go to 3241
3240 continue
  if(.not.(j.eq.ndx3)) go to 5671
  ndx=isto(env+2)
  flg=1
  go to 5670
5671 continue
5670 continue
  go to 5790
5791 continue
5790 continue
  env=env+3
  go to 3171
3170 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9016 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+8)
3181 if(.not.(env.gt.0 .and. flg.eq.0)) go to 3180
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  if(.not.(isto(env+0).eq.ndx3)) go to 5681
  j=0
  i2=isto(env+1)
3251 if(.not.(j.lt.ndx3 .and. csto(i2+j).eq.csto(ndx4+j) )) go to 3250
  j=j+1
  go to 3251
3250 continue
  if(.not.(j.eq.ndx3)) go to 5801
  flg=1
  go to 5800
5801 continue
5800 continue
  go to 5680
5681 continue
5680 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5691
  ndx=isto(env+2)
  go to 5690
5691 continue
5690 continue
  env=isto(env+0)
  go to 3181
3180 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9017 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+10)
3191 if(.not.(env.gt.0 .and. flg.eq.0)) go to 3190
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  if(.not.(isto(env+0).eq.ndx3)) go to 5701
  j=0
  i2=isto(env+1)
3261 if(.not.(j.lt.ndx3 .and. csto(i2+j).eq.csto(ndx4+j))) go to 3260
  j=j+1
  go to 3261
3260 continue
  if(.not.(j.eq.ndx3)) go to 5811
  flg=1
  go to 5810
5811 continue
5810 continue
  go to 5700
5701 continue
5700 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5711
  ndx=isto(env+2)
  go to 5710
5711 continue
5710 continue
  env=isto(env+0)
  go to 3191
3190 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9018 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+12)
3201 if(.not.(env.gt.0 .and. flg.eq.0)) go to 3200
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+4)
  if(.not.(isto(env+0).eq.ndx3)) go to 5721
  j=0
  i2=isto(env+1)
3271 if(.not.(j.lt.ndx3 .and. csto(i2+j).eq.csto(ndx4+j))) go to 3270
  j=j+1
  go to 3271
3270 continue
  if(.not.(j.eq.ndx3)) go to 5821
  flg=1
  go to 5820
5821 continue
5820 continue
  go to 5720
5721 continue
5720 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5731
  ndx=isto(env+2)
  go to 5730
5731 continue
5730 continue
  env=isto(env+0)
  go to 3201
3200 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9019 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+31)
3211 if(.not.(env.gt.0)) go to 3210
  if(.not.(isto(env+1).gt.0)) go to 5741
  isto(env+4)=0
  isto(env+5)=0
  go to 5740
5741 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  ndx3=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  ndx4=isto(env+3)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9549
  go to 9020
9549 from=isto(sptr)
  sptr=sptr+1
  if(.not.(flg.gt.0)) go to 5751
  isto(env+4)=ndx1
  isto(env+5)=ptr1
  go to 5750
5751 continue
  stpflg=73
  stpl1=ndx3
  stpc1=ndx4
  continue
  call errstp
5750 continue
  ptr1=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
5740 continue
  env=isto(env+0)
  go to 3211
3210 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9020 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+30)
3221 if(.not.(env.gt.0 .and. flg.eq.0)) go to 3220
  if(.not.(isto(env+2).eq.ndx3)) go to 5761
  i=0
  i2=isto(env+3)
3231 if(.not.(i.lt.ndx3 .and. csto(i2+i).eq.csto(ndx4+i))) go to 3230
  i=i+1
  go to 3231
3230 continue
  if(.not.(i.eq.ndx3)) go to 5771
  flg=1
  ndx1=isto(env+4)
  ptr1=isto(env+5)
  go to 5770
5771 continue
5770 continue
  go to 5760
5761 continue
5760 continue
  env=isto(env+0)
  go to 3221
3220 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9021 continue
  i0=isto(env+4)+isto(env+9)
  i1=isto(env+1)+2*(i0-1)
  i3=isto(env+11)
  i=0
3281 if(.not.(i.lt.i3)) go to 3280
  i=i+1
  i1=i1+2
  if(.not.(isto(i1).eq.ndx3)) go to 5851
  j=0
  i2=isto(i1+1)
3291 if(.not.(j.lt.ndx3 .and. csto(i2+j).eq.csto(ndx4+j))) go to 3290
  j=j+1
  go to 3291
3290 continue
  if(.not.(j.eq.ndx3)) go to 5861
  flg=1
  ndx=i0+i
  go to 5860
5861 continue
5860 continue
  go to 5850
5851 continue
5850 continue
  go to 3281
3280 continue
  go to 9500
end subroutine xref1
!
!     end of file: xref1.for
!
