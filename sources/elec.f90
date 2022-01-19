!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file elec.f90
!

!
! subroutine elec.
!

subroutine elec
  use tacsto
  use blkcom
  use tacsar
  use labcom
  implicit none
  !
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = from
  from = 0
  !  go to (9000, 9001, 9002, 9003, 9004, 9005, 9006), to-8999
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
  end select
  stop 'Invalid "to" reference in "elec".'
9500 if (.not. (from .eq. 0)) go to 0001
  from = isto(sptr)
  sptr = sptr + 1
  return
  !0001 go to (9501, 9502, 9503, 9504, 9505, 9506), from-9500
0001 select case (from - 9500)
  case (1)
     go to 9501

  case (2)
     go to 9502

  case (3)
     go to 9503

  case (4)
     go to 9504

  case (5)
     go to 9505

  case (6)
     go to 9506
  end select
  stop 'Invalid "from" reference in "elec".'
910 stpflg = 42
  stpi1 = iptr
  stpi2 = ilen - iptr
  continue
  call errstp
9000 continue
  if (.not. (isto(env + 3) .eq. 0)) go to 5001
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = from
  from = 9501
  go to 9001
9501 from = isto(sptr)
  sptr = sptr + 1
  go to 5000
5001 continue
5000 continue
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  to = 9015
  call use1
  sptr = sptr - 1
  if (sptr .eq. iptr) go to 910
  isto(sptr) = from
  from = 9502
  go to 9002
9502 from=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  isto(sptr)=env
  env=useenv
  i0=isto(env+35)
  i1=isto(env+38)-1
  i=0
3081 if (.not.(i.lt.i0)) go to 3080
  i=i+1
  xtcs(kxtcs+i)=rsto(i1+i)
  go to 3081
3080 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9001 continue
  if (.not.(isto(env+0).gt.0)) go to 5011
  write(unit08,800)
800 format('+     time        ')
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+1)
3011 if (.not.(env.gt.0)) go to 3010
  i3=isto(env+4)
  i2=isto(env+5)-1
  i=0
3061 if (.not.(i.lt.i3)) go to 3060
  i=i+1
  cbuff(i:i)=csto(i2+i)
  go to 3061
3060 continue
3071 if (.not.(i.lt.6)) go to 3070
  i=i+1
  cbuff(i:i)=csto(64)
  go to 3071
3070 continue
  write(unit08,801) cbuff(1:6)
801 format('&     ',a6,'   ')
  env=isto(env+0)
  go to 3011
3010 continue
  env=isto(sptr)
  sptr=sptr+1
  write(unit08,802)
802 format(1x)
  go to 5010
5011 continue
5010 continue
  go to 9500
9002 continue
  sptr=sptr-1
  isto(sptr)=env
  env=isto(env+1)
  if (.not.(env.gt.0)) go to 5121
  write(unit08,803) etime
803 format(' ',g15.8,'  ')
  go to 5120
5121 continue
5120 continue
3021 if (.not.(env.gt.0)) go to 3020
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9503
  go to 9003
9503 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3021
3020 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 9500
9003 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  ndx0=isto(env+3)
  if (.not.(ndx0.gt.rptr)) go to 5021
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9504
  go to 9004
9504 from=isto(sptr)
  sptr=sptr+1
  isto(env+3)=ndx0
  go to 5020
5021 continue
5020 continue
  if (.not.(ndx0.le.rptr)) go to 5031
  tacs_a = rsto(ndx0)
  write (unit = unit08, fmt = 804) tacs_a
804 format ('& ',g13.6)
  go to 5030
5031 continue
  tacs_a = zero
  write (unit = unit08, fmt = 805)
805 format('&              ')
5030 continue
  k=isto(env+6)
  xtcs(kxtcs+k)= tacs_a
  ndx0=isto(sptr)
  sptr=sptr+1
  go to 9500
9004 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ptr0
  ptr0=isto(env+2)
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ndx4
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ndx
  ndx3=isto(ptr0+1)
  ndx4=isto(ptr0+2)
  ndx=isto(ptr0+3)
  if (.not. (ndx3 .eq. 4 .and. ndx .eq. iinf .and. csto(ndx4) .eq. csto(20) .and. csto(ndx4 + 1) .eq. csto(1) &
       .and. csto(ndx4 + 2) .eq. csto(3) .and. csto(ndx4 + 3) .eq. csto(19))) go to 5041
  ptr0 = isto(ptr0)
  go to 5040
5041 continue
5040 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=lpflg
  lpflg=1
3031 if (.not.(lpflg.eq.1)) go to 3030
  ndx3=isto(ptr0+1)
  ndx4=isto(ptr0+2)
  ndx=isto(ptr0+3)
  if (.not.(isto(ptr0+0).gt.0)) go to 5051
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9505
  go to 9005
9505 from=isto(sptr)
  sptr=sptr+1
  go to 5050
5051 continue
  lpflg=0
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9506
  go to 9006
9506 from=isto(sptr)
  sptr=sptr+1
5050 continue
  go to 3031
3030 continue
  lpflg=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  ndx=isto(sptr)
  sptr=sptr+1
  ndx4=isto(sptr)
  sptr=sptr+1
  ndx3=isto(sptr)
  sptr=sptr+1
  ptr0=isto(sptr)
  sptr=sptr+1
  go to 9500
9005 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  env=isto(env+56)
3041 if (.not. (env .gt. 0 .and. flg .eq. 0)) go to 3040
  i3 = isto(env + 3)
  if (.not. (i3 .eq. ndx3 .and. isto(env + 5) .eq. ndx)) go to 5061
  i2=isto(env+4)
  i=0
3051 if (.not. (i .lt. i3 .and. csto(i2 + i) .eq. csto(ndx4 + i))) go to 3050
  i=i+1
  go to 3051
3050 continue
  if (.not. (i .eq. i3)) go to 5071
  flg=1
  go to 5070
5071 continue
  env=isto(env+0)
5070 continue
  go to 5060
5061 continue
  env=isto(env+0)
5060 continue
  go to 3041
3040 continue
  if (.not.(flg.gt.0)) go to 5081
  ptr0=isto(ptr0)
  go to 5080
5081 continue
  lpflg=0
5080 continue
  flg=isto(sptr)
  sptr=sptr+1
  go to 9500
9006 continue
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ndx0
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=flg
  flg=0
  sptr=sptr-1
  if (sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+9)
  ndx0=isto(env+1)-2
  ndx1=0
  ndx2 = isto(env + 0) - isto(env + 34) - isto(env + 23)
  to=9003
  call xref1
  env=isto(sptr)
  sptr=sptr+1
  stpl1=ndx3
  stpc1=ndx4
  stpl2=isto(env+6)
  stpc2=isto(env+7)
  if (.not.(flg.eq.0)) go to 5091
  stpflg=127
  continue
  call errstp
  go to 5090
5091 continue
5090 continue
  i3=isto(env+21)-1
  i4=isto(env+53)-1
  i5=isto(env+52)-1
  i6=isto(env+54)-1
  if (.not.(ndx.lt.iinf)) go to 5101
  i1=ndx
  go to 5100
5101 continue
  if (.not. (isto(i4 + ndx1) .eq. 1 .and. isto(i6 + ndx1) .eq. 1)) go to 5111
  i1=1
  go to 5110
5111 continue
  stpflg=128
  continue
  call errstp
5110 continue
5100 continue
  k = i3 + isto(i5 + ndx1) - isto(i4 + ndx1) + i1
  flg=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  ndx0=isto(sptr)
  sptr=sptr+1
  ndx0=k
  go to 9500
end subroutine elec

!
! end of file elec.f90
!
