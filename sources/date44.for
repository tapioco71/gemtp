!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: date44.for
!
!
!     subroutine date44.
!
subroutine date44 ( a )
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !)    The purpose of subroutine  date44  is to interrogate the
  !)    installation calendar, and return the current date through the
  !)    argument of the subroutine.   eight bcd characters are allowed,
  !)    with the first (left) four characters to be placed in  a(1) ,
  !)    and the final (right) four placed in  a(2) .   a statement like
  !)             write (lunit6, 4041)  a
  !)        4041 format ( 1x, 2a4 )
  !)    thus outputs the current date as first the month, then the day,
  !)    and finally the year, separated by slashes (mm/dd/yy) .
  !)    Subroutine  date44  is of course installation dependent.
  !)    European (or generally non-united-states, perhaps) users of this
  !)    program may want to reverse the order of appearance of the month
  !)    and the day, in conformity with established european usage.
  !     installation-dependent  emtp  module written for the  dec
  !     VAX-11/780.    'idate'  is a  dec  system subroutine which
  !     returns the month, day, and year (of century) as three  integer*2
  !     numerical values.
  character*8 a(2)
  integer*2  n1, n2, n3
  call idate ( n1, n2, n3 )
  n4 = n1 / 10
  n5 = n1 - 10*n4
  n6 = n2 / 10
  n7 = n2 - 10*n6
  read (unit = a(1), fmt = 1386) n4, n5, n6
1386 format ( 2i1,  1h/,  1i1  )
  read (unit = a(2), fmt = 1394) n7, n3
1394 format ( i1,  1h/,  i2  )
  return
end subroutine date44
!
!     end of file: date44.for
!
