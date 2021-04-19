!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: errstp.for
!
!
!     subroutine errstp.
!
subroutine errstp
  include 'tacsto.ins'
  if (stpl1 .gt. 50) stpl1=0
  if (stpl2 .gt. 50) stpl2=0
  i1 = isto(ishenv+61)
  if (.not. (i1 .eq. 0)) go to 5001
  i1 = 1
  cbuff(1:1) = csto(64)
  go to 5000
5001 continue
  i2 = isto(ishenv + 62) - 1
  i3 = i2 + i1
  i = 0
3001 if (.not. (i .lt. i1)) go to 3000
  i = i + 1
  cbuff(i:i) = csto(i2 + i)
  go to 3001
3000 continue
5000 continue
  write(unit06, 1001) cbuff(1:i1), stpflg, stpi1, stpi2, stpi3, stpi4, stpi5, stpr1, stpr2, stpr3, &
       ctbl(stpc1 : stpc1+stpl1-1), ctbl(stpc2 : stpc2+stpl2-1)
1001 format(1x//, ' in: ',a// ' stop flag # ', i3// ' i1=',i10/ ' i2=',i10/ ' i3=',i10/ ' i4=',i10/ &
       ' i5=', i10// ' r1=', g13.6/ ' r2=',g13.6/ ' r3=',g13.6// ' name1=',a/ ' name2=',a//)
  write(unit06, 1002)
1002 format(/, ' refer to file drd5:[tacslib]stop.msg for interpretation.')
  stop 'tacs stop in errstp.'
end subroutine errstp
!
!     end of file: errstp.for
!
