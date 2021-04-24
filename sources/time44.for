!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: time44.for
!
!
!     subroutine time44.
!
!
!     subroutine time44.
!
subroutine time44(a)
  implicit real*8 (a-h, o-z), integer*4 (i-n)
  !    The purpose of subroutine  time44  is to interrogate the
  !    installation clock, and return the wall-clock time through the
  !    argument of the subroutine.   Eight bcd characters are allowed,
  !    with the first (left) four characters to be placed in  a(1) ,
  !    and the final (right) four placed in  a(2) .   A statement like
  !             write (lunit6, 4041)  a
  !        4041 format ( 1x, 2a4 )
  !    thus outputs the wall-clock time as first hours, then minutes,
  !    and finally seconds, separated by periods (hh.mm.ss) .
  !    Subroutine  time44  is of course installation dependent.
  !    Installation-dependent  EMTP  module written for the  DEC
  !    VAX-11/780.    'time'  is a  DEC  system subroutine which
  !    returns the wall-clock time as an 8-byte character string.
  !    This is just what the emtp needs, except that we want periods
  !    rather than colons, and of course we require  2a4  format.
  character(10) time
  character(8) a(2)
  call date_and_time(time = time)
  write (unit = a(1), fmt = 2741) time(1:1), time(2:2), time(3:3)
2741 format (2a1, '.', a1)
  write (unit = a(2), fmt = 2754) time(4:4), time(5:5), time(6:6)
2754 format (a1, '.', 2a1)
  return
end subroutine time44
!
!     end of file: time44.for
!
