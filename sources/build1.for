!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file:: build1.for
!
!
!     subroutine build1
!
subroutine build1
  include 'tacsto.ftn'
  sptr = sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = from
  from = 0
  go to (9000, 9001), to-8999
  stop 'invalid "to" reference in "build".'
9500 if (.not. (from .eq. 0)) go to 0001
  from = isto(sptr)
  sptr = sptr + 1
  return
0001 go to (9501), from- 9500
  stop 'invalid "from" reference in "build".'
910 stpflg = 42
  stpi1 = iptr
  stpi2 = ilen - iptr
  continue
  call errstp
9000 continue
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = ndx4
  ndx4 = isto(env + 1)
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = ndx5
  ndx5 = isto(env + 3)
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = ptr0
  ptr0 = env
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = env
  env = useenv
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = from
  from = 9501
  go to 9001
9501 from = isto(sptr)
  sptr = sptr + 1
  env = isto(sptr)
  sptr = sptr + 1
  ptr0 = isto(sptr)
  sptr = sptr + 1
  ndx5 = isto(sptr)
  sptr = sptr + 1
  ndx4 = isto(sptr)
  sptr = sptr + 1
  go to 9500
9001 continue
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=cnt1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=cnt2
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=cnt3
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=cnt4
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=cnt5
  cnt5=1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=cnt6
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=cnt7
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=cnt8
  cnt8=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=cnt9
  cnt9=0
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx0
  iptr=iptr+57
  isto(env+0)=0
  isto(env+1)=0
  isto(env+2)=ndx4
  isto(env+3)=ndx1
  isto(env+4)=ndx2
  isto(env+5)=ndx3
  to=9002
  call build2
  j=isto(env+6)
  k=isto(env+7)
  isto(ishenv+61)=j
  isto(ishenv+62)=k
  isto(env+8)=isto(ishenv+10)
  isto(env+9)=ndx5
  isto(env+10)=ptr0
  isto(env+55)=0
  isto(env+56)=0
  if (.not.(ndx4.ne.2)) go to 5001
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=ndx5
  i1=isto(env+46)
  i2=isto(env+47)
  i4=isto(env+0)
  i5=isto(env+4)
  i6=isto(env+5)
  i7=isto(env+7)
  i8=isto(env+9)
  i9=isto(env+11)
  i10=isto(env+23)
  i11=isto(env+34)
  env=isto(sptr)
  sptr=sptr+1
  go to 5000
5001 continue
  i1=0
  i2=0
  i4=4
  i5=isto(env+20)
  i6=0
  i7=1
  i8=1
  i9=2
  i10=0
  i11=0
5000 continue
  isto(env+11)=i1
  isto(env+12)=i2
  isto(env+13)=0
  isto(env+14)=0
  isto(env+15)=i4
  isto(env+20)=i5
  isto(env+22)=i6
  isto(env+25)=i7
  isto(env+28)=i8
  isto(env+35)=i9
  isto(env+18)=i10
  isto(env+19)=i11
  isto(env+52)=iptr+1
  i=0
3001 if (.not.(i.lt.i4)) go to 3000
  i=i+1
  isto(iptr+i)=i
  go to 3001
3000 continue
  iptr=iptr+i4
  isto(env+53)=iptr+1
  i=0
3011 if (.not.(i.lt.i4)) go to 3010
  i=i+1
  isto(iptr+i)=1
  go to 3011
3010 continue
  iptr=iptr+i4
  isto(env+54)=iptr+1
  i=0
3021 if (.not.(i.lt.i5)) go to 3020
  i=i+1
  isto(iptr+i)=1
  go to 3021
3020 continue
  k=i4-i10-i11
3031 if (.not.(i.lt.k)) go to 3030
  i=i+1
  isto(iptr+i)=-iinf
  go to 3031
3030 continue
3041 if (.not.(i.lt.i4)) go to 3040
  i=i+1
  isto(iptr+i)=1
  go to 3041
3040 continue
  iptr=iptr+i4
  isto(env+21)=rptr+1
  i=0
3051 if (.not.(i.lt.i4)) go to 3050
  i=i+1
  rsto(rptr+i)=rnull
  go to 3051
3050 continue
  rptr=rptr+i4
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=base3
  base3=isto(env+21)-1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=base5
  base5=isto(env+52)-1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=base4
  base4=isto(env+53)-1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=base6
  base6=isto(env+54)-1
  cnt1=i4
  ndx0=i4
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx3
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx5
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx6
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx7
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx8
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx9
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=ndx10
  isto(env+24)=isto(env+21)+isto(env+20)
  cnt2=isto(env+22)
  if (.not. (isto(env + 2) .ne. 2 .and. cnt2 .gt. 0)) go to 5011
  to = 9004
  call build2
  go to 5010
5011 continue
  isto(env+24)=0
5010 continue
  isto(env + 23) = cnt2
  isto(env + 27) = isto(env+21) + isto(env + 20) + cnt2
  cnt2 = isto(env + 25)
  if (.not. (isto(env + 2) .ne. 2 .and. cnt2 .gt. 0)) go to 5021
  to = 9005
  call build2
  go to 5020
5021 if (.not. (isto(env+2) .eq. 2)) go to 5022
  to=9008
  call build2
  go to 5020
5022 continue
  isto(env+27)=0
5020 continue
  isto(env+26)=cnt2
  if (.not.(cnt2 .gt. 0)) go to 5671
  to=9011
  call build2
  to=9012
  call build2
  go to 5670
5671 continue
5670 continue
  isto(env + 31) = isto(env + 21) + isto(env + 20) + isto(env + 23) + cnt2
  cnt2 = isto(env + 28)
  if (.not. (isto(env + 2) .ne. 2 .and. cnt2 .gt. 0)) go to 5031
  to = 9006
  call build2
  go to 5030
5031 if (.not. (isto(env + 2) .eq. 2)) go to 5032
  to = 9009
  call build2
  go to 5030
5032 continue
  isto(env+31)=0
5030 continue
  isto(env + 30) = cnt2
  isto(env + 38) = isto(env + 21) + isto(env + 20) + isto(env + 23) + isto(env + 26) + cnt2
  cnt2 = isto(env + 35)
  if (.not. (isto(env + 2) .ne. 2 .and. cnt2 .gt. 0)) go to 5041
  to = 9007
  call build2
  go to 5040
5041 if (.not.(isto(env+2) .eq. 2)) go to 5042
  to=9010
  call build2
  go to 5040
5042 continue
  isto(env+38)=0
5040 continue
  isto(env+37)=cnt2
  isto(env+16)=cnt1
  to=9014
  call xref2
  isto(env+29)=iptr+1
  iptr=iptr+4*isto(env+28)
  isto(env+36)=iptr+1
  iptr=iptr+3*isto(env+35)
  isto(env+32)=iptr+1
  k=isto(env+30)
  iptr=iptr+5*k
  isto(env+40)=iptr+1
  iptr=iptr+7*(k+isto(env+37))
  i0=isto(env+28)
  i1=isto(env+40)
  i2=isto(env+29)-4
  i3=isto(env+32)
  i5=isto(env+20)+isto(env+22)+isto(env+25)
  i=0
3061 if (.not.(i.lt.i0)) go to 3060
  i=i+1
  i2=i2+4
  isto(i2)=i1
  isto(i2+1)=0
  isto(i2+2)=0
  isto(i2+3)=i3
  i4=isto(base4+i5+i)
  i6=isto(base6+i5+i)
  k=i6-i4+1
  i1=i1+7*k
  i3=i3+5*k
  go to 3061
3060 continue
  i0=isto(env+35)
  i2=isto(env+36)-3
  i5=i5+isto(env+28)
  i=0
3071 if (.not.(i.lt.i0)) go to 3070
  i=i+1
  i2=i2+3
  isto(i2)=i1
  isto(i2+1)=0
  isto(i2+2)=0
  i4=isto(base4+i5+i)
  i6=isto(base6+i5+i)
  k=i6-i4+1
  i1=i1+7*k
  go to 3071
3070 continue
  i0=isto(env+30)
  i2=isto(env+32)-5
  i=0
3081 if (.not.(i.lt.i0)) go to 3080
  i=i+1
  i2=i2+5
  isto(i2)=0
  isto(i2+1)=-1
  isto(i2+2)=0
  isto(i2+3)=0
  isto(i2+4)=0
  go to 3081
3080 continue
  i0=i0+isto(env+37)
  i2=isto(env+40)-7
  i=0
3091 if (.not.(i.lt.i0)) go to 3090
  i=i+1
  i2=i2+7
  isto(i2)=0
  isto(i2+1)=-1
  isto(i2+2)=0
  isto(i2+3)=0
  isto(i2+4)=0
  isto(i2+5)=0
  isto(i2+6)=0
  go to 3091
3090 continue
  isto(env + 39) = i0
  cnt6 = isto(env + 20) + isto(env + 22) + isto(env + 25) + 1
  cnt7 = cnt6 + isto(env + 28)
  if (.not. (isto(env + 2) .ne. 2)) go to 5051
  to = 9013
  call build2
  go to 5050
5051 continue
5050 continue
  to=9015
  call build2
  to=9016
  call build2
  if (.not.(isto(env+2).ne.2)) go to 5061
  to=9017
  call build2
  to=9019
  call build2
  to=9020
  call build2
  to=9021
  call build2
  to=9027
  call build2
  go to 5060
5061 continue
5060 continue
  sptr=sptr-1
  if (sptr .eq. iptr) go to 910
  isto(sptr)=env
  env=datenv
  ndx9=isto(env+18)-3
  ndx10=isto(env+17)
  env=isto(sptr)
  sptr=sptr+1
  if (.not. (isto(env + 2) .ne. 2 .and. ndx10 .gt. 0)) go to 5071
  isto(env+44)=iptr+1
  isto(env+45)=rptr+1
  to=9022
  call build2
  isto(env + 43) = (iptr + 1 - isto(env + 44)) / 4
  go to 5070
5071 continue
  isto(env+44)=0
  isto(env+45)=0
  isto(env+43)=0
5070 continue
  isto(env+34)=rptr+1
  i1=rptr+1
  i2=isto(env+32)-5
  i3=isto(env+30)
  i=0
3101 if (.not.(i.lt.i3)) go to 3100
  i=i+1
  i2=i2+5
  isto(i2)=i1
  k=2*isto(i2+1)+1
  if (.not.(k.lt.2)) go to 5081
  k=2
  go to 5080
5081 continue
5080 continue
  i1=i1+k
  go to 3101
3100 continue
  isto(env+33)=i1-isto(env+34)
  isto(env+42)=i1
  i2=isto(env+40)-7
  i3=isto(env+39)
  i=0
3111 if (.not.(i.lt.i3)) go to 3110
  i=i+1
  i2=i2+7
  k=isto(i2+5)
  if (.not.(k .gt. 0)) go to 5661
  isto(i2)=i1
  i1=i1+k
  go to 5660
5661 continue
5660 continue
  go to 3111
3110 continue
  rptr=i1-1
  isto(env+41)=i1-isto(env+42)
  isto(env+46)=0
  k=2*cnt8+1
  if (.not.(k .eq. 1)) go to 5091
  k=2
  go to 5090
5091 continue
5090 continue
  isto(env+47)=k
  isto(env+48)=rptr+1
  rptr=rptr+k
  isto(env+49)=0
  isto(env+50)=cnt9
  isto(env+51)=rptr+1
  rptr=rptr+cnt9
  if (.not.(isto(env+2).ne.2)) go to 5101
  to=9023
  call build2
  go to 5100
5101 continue
5100 continue
  to=9024
  call build2
  if (.not.(isto(env+2).ne.2)) go to 5111
  to=9025
  call build2
  go to 5110
5111 continue
5110 continue
  k = isto(env + 47) + isto(env + 50) + isto(env + 33) + isto(env + 41) + 2 * isto(env + 43)
  isto(env+17)=k
  j=isto(env+21)+isto(env+16)-1
  i=0
3121 if (.not.(i.lt.k)) go to 3120
  i=i+1
  rsto(j+i)=rnull
  go to 3121
3120 continue
  k=isto(env+24)
  if (.not.(k .gt. 0)) go to 5551
  go to 5550
5551 continue
5550 continue
  k=isto(env+27)
  if (.not.(k .gt. 0)) go to 5561
  go to 5560
5561 continue
5560 continue
  k=isto(env+31)
  if (.not.(k .gt. 0)) go to 5571
  go to 5570
5571 continue
5570 continue
  k=isto(env+38)
  if (.not.(k .gt. 0)) go to 5581
  go to 5580
5581 continue
5580 continue
  j=isto(env+21)+isto(env+16)-isto(env+19)
  if (.not.(isto(env+18) .gt. 0)) go to 5591
  go to 5590
5591 continue
5590 continue
  if (.not.(isto(env+19) .gt. 0)) go to 5601
  go to 5600
5601 continue
5600 continue
  k=isto(env+45)
  if (.not.(k .gt. 0)) go to 5611
  go to 5610
5611 continue
5610 continue
  if (.not.(isto(env+33) .gt. 0)) go to 5621
  go to 5620
5621 continue
5620 continue
  if (.not.(isto(env+41) .gt. 0)) go to 5631
  go to 5630
5631 continue
5630 continue
  if (.not.(isto(env+47) .gt. 0)) go to 5641
  go to 5640
5641 continue
5640 continue
  if (.not. (isto(env+50) .gt. 0)) go to 5651
  go to 5650
5651 continue
5650 continue
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
  base6=isto(sptr)
  sptr=sptr+1
  base4=isto(sptr)
  sptr=sptr+1
  base5=isto(sptr)
  sptr=sptr+1
  base3=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  cnt9=isto(sptr)
  sptr=sptr+1
  cnt8=isto(sptr)
  sptr=sptr+1
  cnt7=isto(sptr)
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
end subroutine build1
!
!     end of file: build1.for
!
