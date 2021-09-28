!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: init.for
!

!
!     subroutine init.
!

subroutine init
  include 'tacsto.ftn'
  character(8) atim(2)      ! wsm + thl manual modification for bpa emtp
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=0
  go to (9000, 9001, 9002, 9003, 9004, 9005, 9006, 9007, 9008, 9009, 9010), to-8999
  stop 'Invalid "to" reference in "init".'
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501, 9502, 9503, 9504, 9505, 9506), from - 9500
  stop 'Invalid "from" reference in "init".'
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
  to=9004
  call calc
  to=9005
  call calc
  b=rsto(rptr)
  rptr=rptr-1
  a=rsto(rptr)
  rptr=rptr-1
  if(.not.(a.lt.rmargn)) go to 5001
  a=zero
  go to 5000
5001 continue
5000 continue
  if(.not.(b.le.rmargn)) go to 5011
  stpflg=118
  go to 5010
5011 continue
5010 continue
  p = real (dmax1 (rmargn, dabs (real (rmargn * b, 16))), kind (p))
  if(.not.(a.gt.b+p)) go to 5021
  stpflg=119
  go to 5020
5021 continue
5020 continue
  if(.not.(stpflg.gt.0)) go to 5031
  stpr1=b
  stpr2=a
  continue
  call errstp
  go to 5030
5031 continue
5030 continue
  if(.not.(a.gt.half*b+p)) go to 5041
  g=half*b
  go to 5040
5041 continue
  g=a
5040 continue
  r1=rsto(base13+6)
  c=rsto(base13+8) -r1
  e=r1
  if(.not.(e.gt.b+p)) go to 5051
  f = real (e / dnint (real (e / b + half, 16)), kind (f))
  go to 5050
5051 continue
  f=e
5050 continue
  rsto(base3+2)=g
  rsto(base3+3)=b
  rsto(base3+4)=c
  rsto(base3+5)=r1
  rsto(base3+6)=f
  rsto(base3+7)=c-f
  rsto(base3+8)=c
  stpt=c
  go to 9500
9001 continue
  a=rsto(base3+8)
  b=rsto(base3+5)
  i1=isto(env+48)
  i2=isto(env+47)
  rsto(i1)=a
  i=i1+i2
3001 if(.not.(i.gt.i1+1)) go to 3000
  i=i-1
  a=a-b
  rsto(i)=a
  go to 3001
3000 continue
  go to 9500
9002 continue
  i1=isto(env+51)
  i2=isto(env+50)
  if(.not.(i2.gt.0)) go to 5251
  a=rsto(base3+8)
  b=rsto(base3+6)
  rsto(i1)=a
  i=i1+i2
3011 if(.not.(i.gt.i1+1)) go to 3010
  i=i-1
  a=a-b
  rsto(i)=a
  go to 3011
3010 continue
  go to 5250
5251 continue
5250 continue
  go to 9500
9003 continue
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
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx7
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx8
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx9
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx10
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx11
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx12
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx13
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=rsto(base7+1)
  ndx1=isto(env+28)
  ndx2 = isto(env + 20) + isto(env + 22) + isto(env + 25)
  ndx9=isto(env+29)-4
  ndx13=4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9501
  go to 9004
9501 from=isto(sptr)
  sptr=sptr+1
  ndx2=ndx2+ndx1
  ndx1=isto(env+35)
  ndx9=isto(env+36)-3
  ndx13=3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9502
  go to 9004
9502 from=isto(sptr)
  sptr=sptr+1
  rsto(base7+1)=rsto(rptr)
  rptr=rptr-1
  ndx13=isto(sptr)
  sptr=sptr+1
  ndx12=isto(sptr)
  sptr=sptr+1
  ndx11=isto(sptr)
  sptr=sptr+1
  ndx10=isto(sptr)
  sptr=sptr+1
  ndx9=isto(sptr)
  sptr=sptr+1
  ndx8=isto(sptr)
  sptr=sptr+1
  ndx7=isto(sptr)
  sptr=sptr+1
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
9004 continue
  ndx3=0
3021 if(.not.(ndx3.lt.ndx1)) go to 3020
  ndx3=ndx3+1
  ndx5=ndx2+ndx3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  i1=isto(env+1)+2*(ndx5-1)
  stpl1=isto(i1)
  stpc1=isto(i1+1)
  env=isto(sptr)
  sptr=sptr+1
  ndx4=isto(base4+ndx5)
  ndx6=isto(base6+ndx5)
  ndx7=ndx6-ndx4+1
  ndx9=ndx9+ndx13
  ndx10=isto(ndx9)
  if(.not.(ndx10.gt.0)) go to 5061
  ndx10=ndx10-7
  go to 5060
5061 continue
5060 continue
  ndx11=isto(ndx9+2)
  if(.not.(ndx11.gt.0)) go to 5071
  ndx11=ndx11-4
  go to 5070
5071 continue
5070 continue
  if(.not.(ndx13.eq.4)) go to 5081
  ndx12=isto(ndx9+3)-5
  go to 5080
5081 continue
  ndx12=0
5080 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9005
9503 from=isto(sptr)
  sptr=sptr+1
  go to 3021
3020 continue
  go to 9500
9005 continue
  ndx8=0
3031 if(.not.(ndx8.lt.ndx7)) go to 3030
  ndx8=ndx8+1
  stpi1=ndx8
  if(.not.(ndx10.gt.0)) go to 5091
  ndx10=ndx10+7
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9006
9504 from=isto(sptr)
  sptr=sptr+1
  i2=isto(ndx10)
  if (.not. (ndx13 .eq. 3 .and. i2 .gt. 0)) go to 5101
  i1=base3+isto(base5+ndx5)-1+ndx8
  if(.not.(rsto(i1).eq.rnull)) go to 5111
  rsto(i1)=rsto(i2)
  go to 5110
5111 continue
5110 continue
  go to 5100
5101 continue
5100 continue
  go to 5090
5091 continue
5090 continue
  if(.not.(ndx11.gt.0)) go to 5121
  ndx11=ndx11+4
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9007
9505 from=isto(sptr)
  sptr=sptr+1
  go to 5120
5121 continue
5120 continue
  if(.not.(ndx12.gt.0)) go to 5131
  ndx12=ndx12+5
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9008
9506 from=isto(sptr)
  sptr=sptr+1
  go to 5130
5131 continue
5130 continue
  go to 3031
3030 continue
  go to 9500
9006 continue
  if(.not.(isto(ndx10+5).gt.0)) go to 5261
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt1
  cnt1=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt3
  cnt3=0
  xprndx=isto(ndx10+3)
  if(.not.(xprndx.gt.0)) go to 5141
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=rsto(base7+1)
  cnt2=isto(ndx10+5)
  a=rsto(base3+8)+rsto(base3+6)
  rsto(base7+1)=a
3041 if(.not.(cnt3.lt.cnt2)) go to 3040
  cnt3=cnt3+1
  a=rsto(base7+1)-rsto(base3+6)
  rsto(base7+1)=a
  xprndx=isto(ndx10+3)
  to=9000
  call xpr1
  i1=isto(ndx10+6)
  if(.not. (xprcnt .lt. i1 .and. xprcnt .gt. 1)) go to 5151
  stpflg=120
  stpi2=i1
  stpi3=xprcnt
  continue
  call errstp
  go to 5150
5151 continue
5150 continue
  rptr=rptr-xprcnt
  cnt1=cnt1-1
  if(.not.(cnt1.lt.0)) go to 5161
  cnt1=cnt1+cnt2
  go to 5160
5161 continue
5160 continue
  i2=isto(ndx10)
  if(.not.(xprcnt.gt.1)) go to 5271
  a=rsto(rptr+i1)
  go to 5270
5271 continue
  a=rsto(rptr+1)
5270 continue
  rsto(i2+cnt1)=a
  go to 3041
3040 continue
  rsto(base7+1)=rsto(rptr)
  rptr=rptr-1
  go to 5140
5141 continue
5140 continue
  cnt3=isto(sptr)
  sptr=sptr+1
  cnt2=isto(sptr)
  sptr=sptr+1
  cnt1=isto(sptr)
  sptr=sptr+1
  go to 5260
5261 continue
5260 continue
  go to 9500
9007 continue
  xprndx=isto(ndx11+1)
  if(.not.(xprndx.gt.0)) go to 5171
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=rsto(base7+1)
  rsto(base7+1)=rsto(base3+8)+rsto(base3+6)
  to=9000
  call xpr1
  i1=isto(ndx11+3)
  if(.not. (xprcnt .lt. i1 .and. xprcnt .gt. 1)) go to 5181
  stpflg=120
  stpi2=i1
  stpi3=xprcnt
  continue
  call errstp
  go to 5180
5181 continue
5180 continue
  rptr=rptr-xprcnt
  i2=isto(ndx11)
  if(.not.(xprcnt.gt.1)) go to 5281
  a=rsto(rptr+i1)
  go to 5280
5281 continue
  a=rsto(rptr+1)
5280 continue
  rsto(i2)=a
  rsto(base7+1)=rsto(rptr)
  rptr=rptr-1
  go to 5170
5171 continue
5170 continue
  go to 9500
9008 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt1
  cnt1=1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt2
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=cnt3
  cnt3=0
  xprndx=isto(ndx12+3)
  if(.not.(xprndx.gt.0)) go to 5191
  rptr=rptr+1
  if(rptr.eq.rsptr) go to 930
  rsto(rptr)=rsto(base7+1)
  cnt2=2*isto(ndx12+1)+1
  if(.not.(cnt2.lt.2)) go to 5201
  cnt2=2
  go to 5200
5201 continue
5200 continue
  a=rsto(base3+8)+rsto(base3+5)
  rsto(base7+1)=a
3051 if(.not.(cnt3.lt.cnt2)) go to 3050
  cnt3=cnt3+1
  a=rsto(base7+1)-rsto(base3+5)
  rsto(base7+1)=a
  xprndx=isto(ndx12+3)
  to=9000
  call xpr1
  i1=isto(ndx12+4)
  if(.not. (xprcnt .lt. i1 .and. xprcnt .gt. 1)) go to 5211
  stpflg=120
  stpi2=i1
  stpi3=xprcnt
  continue
  call errstp
  go to 5210
5211 continue
5210 continue
  rptr=rptr-xprcnt
  cnt1=cnt1-1
  if(.not.(cnt1.lt.0)) go to 5221
  cnt1=cnt1+cnt2
  go to 5220
5221 continue
5220 continue
  i2=isto(ndx12)
  if(.not.(xprcnt.gt.1)) go to 5291
  a=rsto(rptr+i1)
  go to 5290
5291 continue
  a=rsto(rptr+1)
5290 continue
  rsto(i2+cnt1)=a
  go to 3051
3050 continue
  rsto(base7+1)=rsto(rptr)
  rptr=rptr-1
  go to 5190
5191 continue
5190 continue
  cnt3=isto(sptr)
  sptr=sptr+1
  cnt2=isto(sptr)
  sptr=sptr+1
  cnt1=isto(sptr)
  sptr=sptr+1
  go to 9500
9009 continue
  !     a=seedz(a)                                                              ! wsm + thl manual modification for bpa emtp
  call time44 (atim(1))                                                         ! wsm + thl manual modification for bpa emtp
  call runtym (d1, d2)                                                          ! wsm + thl manual modification for bpa emtp
  a = seedy (atim(1)) + 1000. * (d1 + d2)                                       ! wsm + thl manual modification for bpa emtp
  a=randnm ( a )                                                                ! wsm + thl manual modification for bpa emtp
  rsto(base7+10)=pi
  rsto(base7+11)=rinf
  rsto(base7+12)=zero
  rsto(base7+13)=one
  rsto(base7+14)=zero
  rsto(base7+15)=one
  rsto(base7+16)=zero
  rsto(base7+17)=one
  rsto(base7+18)=zero
  rsto(base7+19)=one
  rsto(base7+20)=rnull
  rsto(base7+1)=etime
  rsto(base7+2)=etime
  rsto(base7+3)=estop
  rsto(base7+4)=estep
  rsto(base3+1)=rnull
  rsto(base3+2)=zero
  rsto(base3+3)=rinf
  rsto(base3+4)=etime
  rsto(base3+5)=estep
  rsto(base3+6)=estep
  rsto(base3+7)=etime-estep
  rsto(base3+8)=etime
  stpt=etime
  go to 9500
9010 continue
  useenv=iptr+1
  isto(env+3)=useenv
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  i=8
3061 if(.not.(i.lt.57)) go to 3060
  i=i+1
  isto(iptr+i)=0
  go to 3061
3060 continue
  iptr=iptr+57
  isto(env+0)=0
  isto(env+1)=1
  isto(env+2)=1
  isto(env+3)=4
  isto(env+4)=146
  isto(env+5)=iinf
  isto(env+6)=4
  isto(env+7)=146
  isto(ishenv+61)=isto(env+6)
  isto(ishenv+62)=isto(env+7)
  isto(env+9)=datenv
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=datenv
  i4=isto(env+0)
  i5=isto(env+4)
  i8=isto(env+9)
  i9=isto(env+11)
  env=isto(sptr)
  sptr=sptr+1
  isto(env+15)=i4
  isto(env+16)=i4
  isto(env+20)=i5
  isto(env+28)=i8
  isto(env+30)=i8
  isto(env+35)=i9
  isto(env+37)=i9
  isto(env+52)=iptr+1
  i=0
3071 if(.not.(i.lt.i4)) go to 3070
  i=i+1
  isto(iptr+i)=i
  go to 3071
3070 continue
  iptr=iptr+i4
  isto(env+53)=iptr+1
  i=0
3081 if(.not.(i.lt.i4)) go to 3080
  i=i+1
  isto(iptr+i)=1
  go to 3081
3080 continue
  iptr=iptr+i4
  isto(env+54)=iptr+1
  i=0
3091 if(.not.(i.lt.i4)) go to 3090
  i=i+1
  isto(iptr+i)=1
  go to 3091
3090 continue
  iptr=iptr+i4
  base5=isto(env+52)-1
  base4=isto(env+53)-1
  base6=isto(env+54)-1
  base7=rptr
  i=0
3101 if(.not.(i.lt.24)) go to 3100
  i=i+1
  rsto(rptr+i)=rnull
  go to 3101
3100 continue
  rptr=rptr+24
  base3=rptr
  isto(env+21)=base3+1
  if(.not.(isto(env+28).gt.0)) go to 5231
  isto(env+31)=isto(env+21)+isto(env+20)
  go to 5230
5231 continue
5230 continue
  if(.not.(isto(env+35).gt.0)) go to 5241
  isto(env + 38) = isto(env + 21) + isto(env + 20) + isto(env + 28)
  go to 5240
5241 continue
5240 continue
  i = 0
3111 if(.not. (i .lt. i4)) go to 3110
  i=i+1
  rsto(rptr + i) = rnull
  go to 3111
3110 continue
  rptr=rptr+i4
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
end subroutine init

!
!     end of file: init.for
!
