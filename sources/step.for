!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: step.for
!
!
!     subroutine step.
!
subroutine step
  include 'tacsto.ftn'
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=0
  go to (9000, 9001 , 9002, 9003, 9004, 9005, 9006 ), to-8999
  stop 'invalid "to" reference in "step".'
9500 if(.not.(from.eq.0)) go to 0001
  from=isto(sptr)
  sptr=sptr+1
  return
0001 go to (9501), from- 9500
  stop 'invalid "from" reference in "step".'
910 stpflg=42
  stpi1=iptr
  stpi2=ilen-iptr
  continue
  call errstp
9000 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx1
  ndx1=isto(env+6)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx2
  ndx2=isto(env+7)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=ndx3
  xprndx=isto(env+8)
  if(.not.(xprndx.gt.0)) go to 5001
  to=9000
  call xpr1
  a=rsto(rptr)
  rptr=rptr-1
  ndx3=idnint(a)
  go to 5000
5001 continue
  ndx3=iinf
5000 continue
  stpl1=ndx1
  stpc1=ndx2
  stpi1=ndx3
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(ishenv+10)
  useenv=0
  i1=isto(env+56)
  if(.not.(i1.gt.0)) go to 5011
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=i1
3001 if(.not.(env.gt.0 .and. useenv.eq.0)) go to 3000
  if(.not.(isto(env+3).eq.ndx1 .and. isto(env+4).eq.ndx2 .and. isto(env+5).eq.ndx3)) go to 5021
  useenv=env
  go to 5020
5021 continue
  i2=env
  env=isto(env+0)
5020 continue
  go to 3001
3000 continue
  if(.not.(useenv.eq.0)) go to 5031
  env=i2
  isto(env+0)=iptr+1
  go to 5030
5031 continue
5030 continue
  env=isto(sptr)
  sptr=sptr+1
  go to 5010
5011 continue
  isto(env+56)=iptr+1
5010 continue
  env=isto(sptr)
  sptr=sptr+1
  if(.not.(useenv.eq.0)) go to 5041
  useenv=iptr+1
  to=9000
  call build1
  go to 5040
5041 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=useenv
  isto(ishenv+61)=isto(env+6)
  isto(ishenv+62)=isto(env+7)
  env=isto(sptr)
  sptr=sptr+1
5040 continue
  ndx3=isto(sptr)
  sptr=sptr+1
  ndx2=isto(sptr)
  sptr=sptr+1
  ndx1=isto(sptr)
  sptr=sptr+1
  go to 9500
9001 continue
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+11)
3011 if(.not.(env.gt.0)) go to 3010
  if(.not.(isto(env+1).eq.6)) go to 5051
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+0)
3021 if(.not.(env.gt.0)) go to 3020
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=from
  from=9501
  go to 9002
9501 from=isto(sptr)
  sptr=sptr+1
  env=isto(env+0)
  go to 3021
3020 continue
  env=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  go to 5050
5051 continue
5050 continue
  env=isto(env+0)
  go to 3011
3010 continue
  env=isto(sptr)
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
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+2)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=isto(env+9)
  ndx5=isto(env+2)
  to=9020
  call xpr1
  to=9037
  call xpr2
  ndx3=ndx6-ndx4+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=isto(ishenv+17)
  isto(ishenv+17)=datenv
  to=9002
  call xpr2
  isto(ishenv+17)=isto(sptr)
  sptr=sptr+1
  env=isto(sptr)
  sptr=sptr+1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=env
  env=isto(env+1)
  stpl1=isto(env+6)
  stpc1=isto(env+7)
  ndx1=isto(env+8)
  ndx2=isto(env+9)
  ndx5=isto(env+2)
  to=9020
  call xpr1
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=datenv
  datenv=isto(ishenv+11)
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base3
  base3=base13
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base5
  base5=base15
  sptr=sptr-1
  if(sptr.eq.iptr) go to 910
  isto(sptr)=base4
  base4=base14
  to=9037
  call xpr2
  xprcnt=ndx3
  to=9000
  call xpr2
  base4=isto(sptr)
  sptr=sptr+1
  base5=isto(sptr)
  sptr=sptr+1
  base3=isto(sptr)
  sptr=sptr+1
  datenv=isto(sptr)
  sptr=sptr+1
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
  go to 9500
9003 continue
  go to 9500
9004 continue
  go to 9500
9005 continue
  go to 9500
9006 continue
  go to 9500
end subroutine step
!
!     end of file: step.for
!
