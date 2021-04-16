!-*- mode: fortran; syntax: ansi-fortran-90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: synstp.for
!
!
!     subroutine synstp.
!
subroutine synstp
  include  'tacsto.ins'
  k=dptr-449
  if(k.le.1500) k=1501
  write(unit06,1000) (csto(i),i=k,dptr)
1000 format(//(1x,75a))
  write(unit06,1001) isto(ishenv+54)
1001 format(1x/ ' syntax error #',i5/)
  write(unit06,1002)
1002 format(' refer to file drd5:[tacslib]synstop.msg for interpretation.')
  stop 'tacs stop in synstp.'
end subroutine synstp
!
!     end of file: synstp.for
!
