!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file synstp.f90
!

!
! subroutine synstp.
!

subroutine synstp
  implicit none
  include  'tacsto.ftn'
  k = dptr - 449
  if (k .le. 1500) k = 1501
  write (unit = unit06, fmt = 1000) (csto(i),i = k, dptr)
1000 format (//, (1x, 75a))
  write (unit = unit06, fmt = 1001) isto(ishenv + 54)
1001 format (1x, /, ' Syntax error #', i5, /)
  write (unit = unit06, fmt = 1002)
1002 format (' Refer to file drd5:[tacslib]synstop.msg for interpretation.')
  stop 'tacs stop in synstp.'
end subroutine synstp

!
! end of file synstp.f90
!
