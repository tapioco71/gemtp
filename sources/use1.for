!-*- mode: fortran; syntax: ansi-fortran-90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: use1.for
!
!
!     subroutine use1.
!
subroutine use1
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  include 'tacsto.ins'
  include 'blkcom.ftn'  ! wsm + thl
  include 'tacsar.ftn'  ! wsm + thl
  include 'labcom.ins'  ! wsm + thl
  !  common  / c0b014 /   sptacs(  29 )  ! wsm + thl
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=0
  go to (9000, 9001, 9002, 9003, 9004, 9005, 9006, 9007, 9008, 9009, 9010, 9011, 9012, 9013, 9014, 9015), to-8999
  stop 'invalid "to" reference in "use1".'
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501, 9502, 9503, 9504, 9505, 9506, 9507, 9508, 9509, 9510, 9511, 9512, 9513, 9514, 9515, 9516, 9517, 9518, 9519, &
          9520, 9521, 9522, 9523, 9524), from - 9500
  stop 'invalid "from" reference in "use1".'
910 stpflg=42
  stpi1=iptr
  stpi2=ilen-iptr
  continue
  call errstp
!     930 stpflg=44
  stpflg = 44
  stpi1 = rptr
  stpi2 = rlen - rptr
  continue
  call errstp
!     940  stpflg=45
  stpflg = 45
  stpi1=rptr
  stpi2=rlen-rptr
  continue
  call errstp
9000 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+11)
  isto(ishenv+11)=datenv
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+10)
  isto(ishenv+10)=useenv
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base13
  base13=base3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base15
  base15=base5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base14
  base14=base4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base16
  base16=base6
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+61)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+62)
  datenv=isto(env+3)
  to=9000
  call step
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  base3=isto(env+21)-1
  base5=isto(env+52)-1
  base4=isto(env+53)-1
  base6=isto(env+54)-1
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9501
  go to 9001
9501 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  isto(env+1)=2
  env=isto(sptr)
  sptr=sptr+1
  to=9001
  call step
  datenv=isto(ishenv+11)
  useenv=isto(ishenv+10)
  base3=base13
  base5=base15
  base4=base14
  base6=base16
  isto(ishenv+62)=isto(sptr)
  sptr=sptr+1
  isto(ishenv+61)=isto(sptr)
  sptr=sptr+1
  base16=isto(sptr)
  sptr=sptr+1
  base14=isto(sptr)
  sptr=sptr+1
  base15=isto(sptr)
  sptr=sptr+1
  base13=isto(sptr)
  sptr=sptr+1
  isto(ishenv+10)=isto(sptr)
  sptr=sptr+1
  isto(ishenv+11)=isto(sptr)
  sptr=sptr+1
  go to 9500
9001 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  to=9003
  call step
  if(.not.(flg.eq.0)) go to 5001
  to=9004
  call step
  if(.not.(flg.eq.0)) go to 5011
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9502
  go to 9002
9502 from=isto(sptr)
  sptr=sptr+1
  go to 5010
5011 continue
5010 continue
  flg=0
  to=9006
  call step
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9003
9503 from=isto(sptr)
  sptr=sptr+1
  go to 5000
5001 continue
5000 continue
  flg=0
  to=9005
  call step
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9002 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  k=isto(env+1)
  if(.not.(k.eq.0)) go to 5271
  isto(env+1)=1
  go to 5270
5271 continue
5270 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(k.eq.0)) go to 5281
  to=9000
  call init
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  to=9001
  call init
  to=9002
  call init
  k=isto(env+11)
  if(.not.(k.gt.0)) go to 5021
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=k
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9007
9504 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5020
5021 continue
5020 continue
  to=9003
  call init
  env=isto(sptr)
  sptr=sptr+1
  go to 5280
5281 continue
5280 continue
  go to 9500
9003 continue
  to=9000
  call calc
  to=9004
  call calc
  to=9005
  call calc
  b=rsto(rptr)
  rptr=rptr-1
  a=rsto(rptr)
  rptr=rptr-1
  if(.not.(a.lt.rmargn)) go to 5031
  a=zero
  go to 5030
5031 continue
5030 continue
  if(.not.(b.le.rmargn)) go to 5041
  stpflg=100
  go to 5040
5041 continue
5040 continue
  p=dmax1(rmargn,dabs(rmargn*b))
  if(.not.(a.gt.b+p)) go to 5051
  stpflg=101
  go to 5050
5051 continue
5050 continue
  if(.not.(stpflg.gt.0)) go to 5061
  stpr1=b
  stpr2=a
  continue
  call errstp
  go to 5060
5061 continue
5060 continue
  if(.not.(a.gt.half*b+p)) go to 5071
  g=half*b
  go to 5070
5071 continue
  g=a
5070 continue
  c=rsto(base13+8)
  d=rsto(base3+8)
  e=c-d
  if(.not.(e.le.rmargn)) go to 5081
  stpflg=102
  stpr1=c
  stpr2=d
  continue
  call errstp
  go to 5080
5081 continue
5080 continue
  if(.not.(e.gt.b+p)) go to 5091
  f=e/dnint(e/b+half)
  go to 5090
5091 continue
  f=e
5090 continue
  p=dmax1(rmargn,dabs(rmargn*g))
  if(.not.(f.ge.g-p)) go to 5101
  rsto(base3+2)=g
  rsto(base3+3)=b
  rsto(base3+4)=c
  rsto(base3+5)=e
  rsto(base3+6)=f
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  to=9006
  call calc
  to=9007
  call calc
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9004
9505 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5100
5101 continue
5100 continue
  go to 9500
9004 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  c=rsto(base3+4)
  d=rsto(base3+8)
  f=rsto(base3+6)
  rsptr=rsptr-1
  rsto(rsptr)=d
  ptr0=rsptr
  rsptr=rsptr-1
  rsto(rsptr)=stpt
  rsptr=rsptr-1
  rsto(rsptr)=rsto(base7+1)
3001 if(.not.(d+half*f.lt.c)) go to 3000
  rsto(base3+7)=d
  ndx=ndx+1
  d=rsto(ptr0)+ndx*f
  if(.not.(d+half*f.gt.c)) go to 5111
  d=c
  flg=-1
  go to 5110
5111 continue
5110 continue
  if(.not.(dabs(d).le.rmargn)) go to 5301
  d=zero
  go to 5300
5301 continue
5300 continue
  rsto(base3+8)=d
  rsto(base7+1)=d
  stpt=d
  k=isto(env+2)
  if(.not.(k.eq.1)) go to 5121
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9006
9506 from=isto(sptr)
  sptr=sptr+1
  go to 5120
5121 if(.not.(k.eq.2)) go to 5122
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9507
  go to 9005
9507 from=isto(sptr)
  sptr=sptr+1
  go to 5120
5122 continue
5120 continue
  c=rsto(base3+4)
  d=rsto(base3+8)
  f=rsto(base3+6)
  flg=1
  go to 3001
3000 continue
  rsto(base7+1)=rsto(rsptr)
  rsptr=rsptr+1
  stpt=rsto(rsptr)
  rsptr=rsptr+1
  d=rsto(rsptr)
  rsptr=rsptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9005 continue
  stop 'no calcftn routine.'
9006 continue
  to=9008
  call calc
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+12)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9508
  go to 9007
9508 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  to=9010
  call calc
  to=9012
  call calc
  to=9013
  call calc
  if(.not.(isto(env+13).gt.0)) go to 5131
  to=9015
  call calc
  go to 5130
5131 continue
5130 continue
  go to 9500
9007 continue
3011 if(.not.(env.gt.0)) go to 3010
  i=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  if(.not.(i.eq.1)) go to 5141
  to=9001
  call use2
  go to 5140
5141 if(.not.(i.eq.2)) go to 5142
  to=9002
  call use2
  go to 5140
5142 if(.not.(i.eq.3)) go to 5143
  to=9009
  call use2
  go to 5140
5143 if(.not.(i.eq.4)) go to 5144
  to=9010
  call use2
  go to 5140
5144 if(.not.(i.eq.5)) go to 5145
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9509
  go to 9008
9509 from=isto(sptr)
  sptr=sptr+1
  go to 5140
5145 if(.not.(i.eq.6)) go to 5146
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9510
  go to 9009
9510 from=isto(sptr)
  sptr=sptr+1
  go to 5140
5146 if(.not.(i.eq.7)) go to 5147
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9511
  go to 9010
9511 from=isto(sptr)
  sptr=sptr+1
  go to 5140
5147 if(.not.(i.eq.8)) go to 5148
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9512
  go to 9012
9512 from=isto(sptr)
  sptr=sptr+1
  go to 5140
5148 if(.not.(i.eq.9)) go to 5149
  isto(ishenv+59)=1
  go to 5140
5149 if(.not.(i.eq.10)) go to 5150
  to=9000
  call comb
  go to 5140
5150 if(.not.(i.eq.11)) go to 5151
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9513
  go to 9013
9513 from=isto(sptr)
  sptr=sptr+1
  go to 5140
5151 if(.not.(i.eq.12)) go to 5152
  to=9011
  call use2
  go to 5140
5152 if(.not.(i.eq.13)) go to 5153
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9514
  go to 9014
9514 from=isto(sptr)
  sptr=sptr+1
  go to 5140
5153 if(.not.(i.eq.14)) go to 5154
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9515
  go to 9000
9515 from=isto(sptr)
  sptr=sptr+1
  go to 5140
5154 if(.not.(i.eq.15)) go to 5155
  to=9000
  call use2
  go to 5140
5155 continue
5140 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3011
3010 continue
  go to 9500
9008 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3021 if(.not.(env.gt.0 .and. flg.eq.0)) go to 3020
  xprndx=isto(env+1)
  if(.not.(xprndx.gt.0)) go to 5251
  to=9000
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  if(.not.(a.gt.rmargn)) go to 5161
  flg=1
  go to 5160
5161 continue
5160 continue
  go to 5250
5251 continue
  flg=1
5250 continue
  if(.not.(flg.gt.0)) go to 5171
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9516
  go to 9007
9516 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5170
5171 continue
5170 continue
  env=isto(env+0)
  go to 3021
3020 continue
  env=isto(sptr)
  sptr=sptr+1
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9009 continue
  if(.not.(isto(env+1).gt.0)) go to 5181
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3031 if(.not.(lpflg.gt.0)) go to 3030
  xprndx=isto(env+0)
  to=9000
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  if(.not.(a.le.rmargn)) go to 5191
  lpflg=0
  go to 5190
5191 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+1)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9517
  go to 9007
9517 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
5190 continue
  go to 3031
3030 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  go to 5180
5181 continue
5180 continue
  go to 9500
9010 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  ptr0=isto(env+4)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=rsptr
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt1
  cnt1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt2
  cnt2=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=0
  k=isto(env+1)
  ptr2=base3+isto(base5+k+1)-1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+3)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9518
  go to 9011
9518 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  cnt2=isto(sptr)
  sptr=sptr+1
  ptr2=isto(sptr)
  sptr=sptr+1
  cnt1=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
  go to 9500
9011 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ptr1
  ptr1=ptr1-cnt1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt1
  cnt1=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  ndx1=0
  i=ndx2
3041 if(.not.(i.lt.cnt2)) go to 3040
  i=i+1
  rsto(ptr2+i)=rnull
  go to 3041
3040 continue
  ndx2=ndx2+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+6)
3051 if(.not.(env.gt.0)) go to 3050
  xprndx=isto(env+1)
  to=9000
  call xpr1
  r1=rsto(rptr)
  rptr=rptr-1
  xprndx=isto(env+2)
  if(.not.(xprndx.gt.0)) go to 5201
  to=9000
  call xpr1
  r2=rsto(rptr)
  rptr=rptr-1
  go to 5200
5201 continue
  r2=r1
5200 continue
  xprndx=isto(env+3)
  if(.not.(xprndx.gt.0)) go to 5211
  to=9000
  call xpr1
  r3=rsto(rptr)
  rptr=rptr-1
  if(.not.(dabs(r3).le.rmargn)) go to 5291
  r3=zero
  go to 5290
5291 continue
5290 continue
  go to 5210
5211 continue
  r3=one
5210 continue
  if(.not.(r2.lt.r1)) go to 5221
  a=-one
  go to 5220
5221 continue
  a=one
5220 continue
  p=dmax1(rmargn,dabs(rmargn*r2))
  if(.not. ((r1 .lt. r2 - p  .or. r1.gt.r2+p) .and. a*r3 .le. zero)) go to 5231
  stpflg=111
  env=isto(sptr)
  sptr=sptr+1
  stpl1=isto(env+4)
  stpc1=isto(env+5)
  stpr1=r1
  stpr2=r2
  stpr3=r3
  continue
  call errstp
  go to 5230
5231 continue
5230 continue
  b=r1-r3
  i=-1
3061 if(.not.(a*b.lt.a*r2-p)) go to 3060
  i=i+1
  b=r1+i*r3
  rsptr=rsptr-1
  rsto(rsptr)=b
  go to 3061
3060 continue
  cnt1=cnt1+i+1
  env=isto(env+0)
  go to 3051
3050 continue
  env=isto(sptr)
  sptr=sptr+1
3071 if(.not.(ndx1.lt.cnt1)) go to 3070
  ndx1=ndx1+1
  rsto(ptr2+ndx2)=rsto(ptr1-ndx1)
  if(.not.(ndx2.eq.cnt2)) go to 5241
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=ptr0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9519
  go to 9007
9519 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5240
5241 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9520
  go to 9011
9520 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
5240 continue
  go to 3071
3070 continue
  rsptr=ptr1
  ndx2=ndx2-1
  ndx1=isto(sptr)
  sptr=sptr+1
  cnt1=isto(sptr)
  sptr=sptr+1
  ptr1=isto(sptr)
  sptr=sptr+1
  go to 9500
9012 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+59)
  isto(ishenv+59)=1
3081 if(.not.(isto(ishenv+59).eq.1)) go to 3080
  isto(ishenv+59)=0
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9521
  go to 9007
9521 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 3081
3080 continue
  isto(ishenv+59)=isto(sptr)
  sptr=sptr+1
  go to 9500
9013 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9522
  go to 9007
9522 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9014 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9523
  go to 9007
9523 from=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  stpflg=105
  continue
  call errstp
  go to 9500
9015 continue
  if(.not.(isto(env+3).eq.0)) go to 5261
  to=9010
  call init
  to=9009
  call init
  go to 5260
5261 continue
  rsto(base7+1)=etime
  rsto(base3+4)=etime
  rsto(base3+5)=etime-rsto(base3+8)
  rsto(base3+6)=rsto(base3+5)
  rsto(base3+7)=rsto(base3+8)
  rsto(base3+8)=etime
  stpt=etime
5260 continue
  sptr=sptr-1
  isto(sptr)=env
  env=useenv
  k=isto(env+28)
  i0=kxtcs+nuk
  i1=isto(env+31)-1
  i=0
3101 if(.not.(i.lt.k)) go to 3100
  i=i+1
  rsto(i1+i)=xtcs(i0+i)
  go to 3101
3100 continue
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+28)
3091 if(.not.(env.gt.0)) go to 3090
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9524
  go to 9000
9524 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3091
3090 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
end subroutine use1
!
!     end of file: use1.for
!
