!-*- mode: fortran; syntax: ansi-fortran-77; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: calc.for
!
!
!     subroutine calc.
!
      subroutine calc
      include 'tacsto.ins'
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=0
      goto (9000, 9001, 9002, 9003, 9004, 9005, 9006, 9007, 9008, 0002, 9010, 9011, 9012, 9013, 9014, 9015, 9016), to-8999
 0002 stop 'invalid "to" reference in "calc".'
 9500 if(.not.(from.eq.0)) goto 0001
      from=isto(sptr)
      sptr=sptr+1
      return
 0001 goto (9501,  9502,  9503,  9504, 9505, 9506,  9507,  9508,  9509), from-9500
      stop 'invalid "from" reference in "calc".'
 910  stpflg=42
      stpi1=iptr
      stpi2=ilen-iptr
      continue
      call errstp
 930  stpflg=44
      stpi1=rptr
      stpi2=rlen-rptr
      continue
      call errstp
 9000 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=useenv
      i0=isto(env+31)-1
      k=isto(env+30)
      env=isto(sptr)
      sptr=sptr+1
      i=0
 3001 if(.not.(i.lt.k)) goto 3000
      i=i+1
      rsto(i0+i)=rnull
      goto 3001
 3000 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=isto(env+11)
 3011 if(.not.(env.gt.0)) goto 3010
      if(.not.(isto(env+1).eq.5)) goto 5001
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=isto(env+2)
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=isto(env+0)
 3021 if(.not.(env.gt.0)) goto 3020
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=9501
      goto 9001
 9501 from=isto(sptr)
      sptr=sptr+1
      env=isto(env+0)
      goto 3021
 3020 continue
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      goto 5000
 5001 continue
 5000 continue
      env=isto(env+0)
      goto 3011
 3010 continue
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=datenv
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=isto(env+10)
 3031 if(.not.(env.gt.0)) goto 3030
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=9502
      goto 9002
 9502 from=isto(sptr)
      sptr=sptr+1
      env=isto(env+0)
      goto 3031
 3030 continue
      env=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=useenv
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=9503
      goto 9003
 9503 from=isto(sptr)
      sptr=sptr+1
      env=isto(sptr)
      sptr=sptr+1
      goto 9500
 9001 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx2
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx3
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx5
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx6
      xprndx=isto(env+2)
      to=9000
      call xpr1
      ndx3=xprcnt
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=isto(env+1)
      stpl1=isto(env+6)
      stpc1=isto(env+7)
      ndx1=isto(env+8)
      ndx2=isto(env+9)
      ndx5=isto(env+2)
      to=9020
      call xpr1
      to=9037
      call xpr2
      xprcnt=ndx3
      to=9000
      call xpr2
      env=isto(sptr)
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
      goto 9500
 9002 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx2
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx3
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx5
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx6
      xprndx=isto(env+5)
      if(.not.(xprndx.gt.0)) goto 5011
      to=9000
      call xpr1
      ndx3=xprcnt
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=isto(env+4)
      stpl1=isto(env+0)
      stpc1=isto(env+1)
      ndx1=isto(env+2)
      ndx2=isto(env+3)
      env=isto(sptr)
      sptr=sptr+1
      ndx5=isto(env+2)
      to=9020
      call xpr1
      to=9037
      call xpr2
      xprcnt=ndx3
      to=9001
      call xpr2
      goto 5010
 5011 continue
 5010 continue
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
      goto 9500
 9003 continue
      i0=isto(env+31) -1
      i5=isto(env+20)+isto(env+22)+isto(env+25)
      i2=i5+isto(env+28)
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=datenv
      i3=isto(env+1) +2*(i5-1)
      env=isto(sptr)
      sptr=sptr+1
 3041 if(.not.(i5.lt.i2)) goto 3040
      i5=i5+1
      i3=i3+2
      stpl1=isto(i3)
      stpc1=isto(i3+1)
      i4=isto(base4+i5)
      i6=isto(base6+i5)
      i1=base3 +isto(base5+i5) -1
      k=i6-i4+1
      i=0
 3051 if(.not.(i.lt.k)) goto 3050
      i=i+1
      if(.not.(rsto(i1+i).eq.rnull)) goto 5021
      stpflg=104
      stpi1=i4
      stpi2=i6
      stpi3=i4+i-1
      continue
      call errstp
      goto 5020
 5021 continue
 5020 continue
      goto 3051
 3050 continue
      goto 3041
 3040 continue
      goto 9500
 9004 continue
      xprndx=isto(env+9)
      if(.not.(xprndx.eq.0)) goto 5031
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=datenv
      xprndx=isto(env+44)
      env=isto(sptr)
      sptr=sptr+1
      goto 5030
 5031 continue
 5030 continue
      if(.not.(xprndx.gt.0)) goto 5041
      to=9000
      call xpr1
      goto 5040
 5041 continue
      a=zero
      rptr=rptr+1
      if(rptr.eq.rsptr) goto 930
      rsto(rptr)=a
 5040 continue
      goto 9500
 9005 continue
      xprndx=isto(env+10)
      if(.not.(xprndx.eq.0)) goto 5051
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=datenv
      xprndx=isto(env+45)
      env=isto(sptr)
      sptr=sptr+1
      goto 5050
 5051 continue
 5050 continue
      if(.not.(xprndx.gt.0)) goto 5061
      to=9000
      call xpr1
      goto 5060
 5061 continue
      a=rinf
      rptr=rptr+1
      if(rptr.eq.rsptr) goto 930
      rsto(rptr)=a
 5060 continue
      goto 9500
 9006 continue
      j=isto(env+47)
      k=isto(env+46)+1
      if(.not.(k.ge.j)) goto 5071
      k=0
      goto 5070
 5071 continue
 5070 continue
      isto(env+46)=k
      i0=isto(env+48)
      rsto(i0+k)=c
      goto 9500
 9007 continue
      i2=isto(env+31)-1
      i3=isto(env+30)
      i0=isto(env+32)
      i=0
 3061 if(.not.(i.lt.i3)) goto 3060
      i=i+1
      a=rsto(i2+i)
      i1=2*isto(i0+1)
      if(.not.(i1.eq.0)) goto 5081
      i1=1
      goto 5080
 5081 continue
 5080 continue
      i5=isto(i0+2)+1
      if(.not.(i5.gt.i1)) goto 5091
      i5=0
      goto 5090
 5091 continue
 5090 continue
      isto(i0+2)=i5
      i4=isto(i0)
      rsto(i4+i5)=a
      i0=i0+5
      goto 3061
 3060 continue
      goto 9500
 9008 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx
      ndx=0
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx0
      ndx0=isto(env+30)
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx2
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx3
      ndx3=isto(env+31)-1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx4
      ndx4=isto(env+34)-1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx5
      ndx5=isto(env+47)
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx6
      ndx6=isto(env+46)
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx7
      ndx7=isto(env+48)
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=isto(env+32)-5
      if(.not.(flg.lt.0)) goto 5101
 3071 if(.not.(ndx.lt.ndx0)) goto 3070
      ndx=ndx+1
      env=env+5
      i2=isto(env+0)+isto(env+2)
      rsto(ndx3+ndx)=rsto(i2)
      goto 3071
 3070 continue
      goto 5100
 5101 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr0
      ptr0=rptr+1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr1
      ptr1=ptr0
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr2
      ptr2=ptr1+ndx5
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ipol
      a=rsto(base3+8)
      rptr=rptr+1
      if(rptr.eq.rsptr) goto 930
      rsto(rptr)=a
      j=ndx6+1
      i=0
 3081 if(.not.(i.lt.ndx5)) goto 3080
      i=i+1
      j=j-1
      if(.not.(j.lt.0)) goto 5111
      j=j+ndx5
      goto 5110
 5111 continue
 5110 continue
      rsto(ptr1+i)=rsto(ndx7+j)
      goto 3081
 3080 continue
      rptr=ptr1+ndx5
 3091 if(.not.(ndx.lt.ndx0)) goto 3090
      ndx=ndx+1
      env=env+5
      ipol=isto(env+1)
      cnt=2*ipol+1
      if(.not.(cnt.eq.1)) goto 5121
      cnt=2
      goto 5120
 5121 continue
 5120 continue
      ndx2=isto(env+0)
      k=isto(env+2)+1
      i=0
 3101 if(.not.(i.lt.cnt)) goto 3100
      i=i+1
      k=k-1
      if(.not.(k.lt.0)) goto 5131
      k=k+cnt
      goto 5130
 5131 continue
 5130 continue
      a=rsto(ndx2+k)
      if(.not.(a.eq.rnull)) goto 5141
      stpflg=40
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=useenv
      i1=isto(env+31)-base3 -1
      i=1
 3111 if(.not. (isto(base5 + i) - i1 .le. ndx)) goto 3110
      i=i+1
      goto 3111
 3110 continue
      i5=i-1
      stpi1 = isto(base4 + i5) + ndx - (isto(base5 + i5) - i1)
      env=isto(sptr)
      sptr=sptr+1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=env
      env=datenv
      i2=isto(env+1)-2
      stpl1=isto(i2+2*i5)
      stpc1=isto(i2+2*i5+1)
      env=isto(sptr)
      sptr=sptr+1
      continue
      call errstp
      goto 5140
 5141 continue
      rsto(ptr2+i)=a
 5140 continue
      goto 3101
 3100 continue
      if(.not.(ipol.eq.0)) goto 5241
      rsto(ndx3+ndx)=rsto(ptr2+2)
      goto 5240
 5241 continue
      rptr=ptr2+cnt
      to=9020
      call xpr2
      rsto(ndx3+ndx)=rsto(ptr0)
 5240 continue
      goto 3091
 3090 continue
      rptr=ptr0-1
      ipol=isto(sptr)
      sptr=sptr+1
      cnt=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
 5100 continue
      env=isto(sptr)
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
      ndx0=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      goto 9500
 9010 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr0
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt2
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt5
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt6
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx1
      if(.not.(isto(env+43).gt.0)) goto 5151
      ptr1=isto(env+45)-1
      ptr0=isto(env+29)-4
      cnt1=isto(env+28)
      cnt2=isto(env+20)+isto(env+22)+isto(env+25)
      cnt4=base4+cnt2
      cnt5=base5+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3131 if(.not.(ndx1.lt.cnt1)) goto 3130
      ndx1=ndx1+1
      ptr0=ptr0+4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=9504
      goto 9011
 9504 from=isto(sptr)
      sptr=sptr+1
      goto 3131
 3130 continue
      ptr0=isto(env+36)-3
      cnt2=cnt2+cnt1
      cnt1=isto(env+35)
      cnt4=base4+cnt2
      cnt5=base5+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3141 if(.not.(ndx1.lt.cnt1)) goto 3140
      ndx1=ndx1+1
      ptr0=ptr0+3
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=9505
      goto 9011
 9505 from=isto(sptr)
      sptr=sptr+1
      goto 3141
 3140 continue
      goto 5150
 5151 continue
 5150 continue
      ndx1=isto(sptr)
      sptr=sptr+1
      cnt6=isto(sptr)
      sptr=sptr+1
      cnt5=isto(sptr)
      sptr=sptr+1
      cnt4=isto(sptr)
      sptr=sptr+1
      cnt2=isto(sptr)
      sptr=sptr+1
      cnt1=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      goto 9500
 9011 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr2
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr3
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx2
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx5
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx6
      ndx4=isto(cnt4+ndx1)
      ndx5=isto(cnt5+ndx1)
      ndx6=isto(cnt6+ndx1)
      cnt=ndx6-ndx4+1
      i=isto(ptr0+2)
      if(.not.(i.gt.0)) goto 5161
      ptr2=i-4
      ptr3=isto(ptr0)-7
      ndx2=0
 3151 if(.not.(ndx2.lt.cnt)) goto 3150
      ndx2=ndx2+1
      ptr2=ptr2+4
      ptr3=ptr3+7
      i1=isto(ptr2+2)
      if(.not.(i1.gt.1)) goto 5171
      i2=isto(ptr2)
      rsto(i2)=rsto(i2+1)
      goto 5170
 5171 if(.not.(i1.eq.0)) goto 5172
      to=9028
      call xpr1
      a=rsto(rptr)
      rptr=rptr-1
      b=rsto(rptr)
      rptr=rptr-1
      i0=base3 +ndx5 -1 +ndx2
      c=rsto(i0)
      d=b*c +a
      i2=isto(ptr2)
      rsto(i2)=d
      goto 5170
 5172 continue
 5170 continue
      isto(ptr2+2)=0
      goto 3151
 3150 continue
      goto 5160
 5161 continue
 5160 continue
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx5=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx2=isto(sptr)
      sptr=sptr+1
      ptr3=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      cnt=isto(sptr)
      sptr=sptr+1
      goto 9500
 9012 continue
      j=isto(env+50)
      k=isto(env+49)+1
      if(.not.(k.ge.j)) goto 5181
      k=0
      goto 5180
 5181 continue
 5180 continue
      isto(env+49)=k
      i=isto(env+51)
      rsto(i+k)=rsto(base3 +8)
      goto 9500
 9013 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr0
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt2
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt6
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx1
      ptr1=isto(env+42)-1
      ptr0=isto(env+29)-4
      cnt1=isto(env+28)
      cnt2=isto(env+20)+isto(env+22)+isto(env+25)
      cnt4=base4+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3161 if(.not.(ndx1.lt.cnt1)) goto 3160
      ndx1=ndx1+1
      ptr0=ptr0+4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=9506
      goto 9014
 9506 from=isto(sptr)
      sptr=sptr+1
      goto 3161
 3160 continue
      ptr0=isto(env+36)-3
      cnt2=cnt2+cnt1
      cnt1=isto(env+35)
      cnt4=base4+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3171 if(.not.(ndx1.lt.cnt1)) goto 3170
      ndx1=ndx1+1
      ptr0=ptr0+3
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=9507
      goto 9014
 9507 from=isto(sptr)
      sptr=sptr+1
      goto 3171
 3170 continue
      ndx1=isto(sptr)
      sptr=sptr+1
      cnt6=isto(sptr)
      sptr=sptr+1
      cnt4=isto(sptr)
      sptr=sptr+1
      cnt2=isto(sptr)
      sptr=sptr+1
      cnt1=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      goto 9500
 9014 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt
      i4=isto(cnt4+ndx1)
      i6=isto(cnt6+ndx1)
      cnt=i6-i4+1
      i=isto(ptr0)
      if(.not.(i.gt.0)) goto 5191
      i0=i-7
      i5=cnt2+ndx1
      i1=base3+isto(base5+i5)-1
      i=0
 3181 if(.not.(i.lt.cnt)) goto 3180
      i=i+1
      i0=i0+7
      j=isto(i0+5)
      if(.not.(j.gt.0)) goto 5231
      i2=isto(i0)
      a=rsto(i1+i)
      if(.not.(isto(i0+4).gt.0)) goto 5201
      isto(i0+4)=0
      i3=-1
 3191 if(.not.(i3.lt.j-1)) goto 3190
      i3=i3+1
      rsto(i2+i3)=a
      goto 3191
 3190 continue
      goto 5200
 5201 continue
      i3=isto(i0+2)+1
      if(.not.(i3.ge.j)) goto 5211
      i3=0
      goto 5210
 5211 continue
 5210 continue
      isto(i0+2)=i3
      rsto(i2+i3)=a
 5200 continue
      goto 5230
 5231 continue
 5230 continue
      goto 3181
 3180 continue
      goto 5190
 5191 continue
 5190 continue
      cnt=isto(sptr)
      sptr=sptr+1
      goto 9500
 9015 continue
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr0
      ptr0=iptr
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr1
      ptr1=isto(env+14)
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx6
      isto(iptr+1)=ibkptr+1
      ndx=1
      ndx4=ptr1
      ndx6=ndx4
      to=9032
      call xpr2
      isto(iptr+1)=0
      isto(iptr+2)=ptr1
      isto(iptr+3)=rbkptr+1
      ndx=1
      ndx4=ibkptr+1
      ndx6=ndx4+2
      to=9032
      call xpr2
      isto(env+14)=ndx4
      ibkptr=ndx6
      ptr0=rptr
      ptr1=rptr+1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ptr2
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt1
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt2
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=cnt6
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=ndx1
      a=rsto(base3+8)
      rptr=rptr+1
      if(rptr.eq.rsptr) goto 930
      rsto(rptr)=a
      ptr2=isto(env+29)-4
      cnt1=isto(env+28)
      cnt2=isto(env+20)+isto(env+22)+isto(env+25)
      cnt4=base4+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3201 if(.not.(ndx1.lt.cnt1)) goto 3200
      ndx1=ndx1+1
      ptr2=ptr2+4
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=9508
      goto 9016
 9508 from=isto(sptr)
      sptr=sptr+1
      goto 3201
 3200 continue
      ptr2=isto(env+36)-3
      cnt2=cnt2+cnt1
      cnt1=isto(env+35)
      cnt4=base4+cnt2
      cnt6=base6+cnt2
      ndx1=0
 3211 if(.not.(ndx1.lt.cnt1)) goto 3210
      ndx1=ndx1+1
      ptr2=ptr2+3
      sptr=sptr-1
      if(sptr.eq.iptr) goto 910
      isto(sptr)=from
      from=9509
      goto 9016
 9509 from=isto(sptr)
      sptr=sptr+1
      goto 3211
 3210 continue
      ndx1=isto(sptr)
      sptr=sptr+1
      cnt6=isto(sptr)
      sptr=sptr+1
      cnt4=isto(sptr)
      sptr=sptr+1
      cnt2=isto(sptr)
      sptr=sptr+1
      cnt1=isto(sptr)
      sptr=sptr+1
      ptr2=isto(sptr)
      sptr=sptr+1
      ndx=1
      ndx4=rbkptr+1
      ndx6=ndx4+rptr-ptr1
      to=9033
      call xpr2
      rbkptr=ndx6
      rptr=ptr1-1
      ndx6=isto(sptr)
      sptr=sptr+1
      ndx4=isto(sptr)
      sptr=sptr+1
      ndx=isto(sptr)
      sptr=sptr+1
      ptr1=isto(sptr)
      sptr=sptr+1
      ptr0=isto(sptr)
      sptr=sptr+1
      goto 9500
 9016 continue
      i4=isto(cnt4+ndx1)
      i6=isto(cnt6+ndx1)
      cnt=i6-i4+1
      i=isto(ptr2+1)
      if(.not.(i.gt.0)) goto 5221
      i5=cnt2+ndx1
      i1=base3 +isto(base5+i5) -1
      i=0
 3221 if(.not.(i.lt.cnt)) goto 3220
      i=i+1
      a=rsto(i1+i)
      rptr=rptr+1
      if(rptr.eq.rsptr) goto 930
      rsto(rptr)=a
      goto 3221
 3220 continue
      goto 5220
 5221 continue
 5220 continue
      goto 9500
      end
!
!     end of file: calc.for
!
