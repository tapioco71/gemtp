!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file date44.f90
!

module bcddat
  implicit none
  character(4) :: date1(2)

contains

  !
  ! subroutine date44.
  !

  subroutine date44 (a)
    implicit none
    !         The purpose of subroutine  date44  is to interrogate the
    !         installation calendar, and return the current date through the
    !         argument of the subroutine.   eight bcd characters are allowed,
    !         with the first (left) four characters to be placed in  a(1) ,
    !         and the final (right) four placed in  a(2) .   a statement like
    !                  write (lunit6, 4041)  a
    !             4041 format ( 1x, 2a4 )
    !         thus outputs the current date as first the month, then the day,
    !         and finally the year, separated by slashes (mm/dd/yy) .
    !         subroutine  date44  is of course installation dependent.
    !         european (or generally non-united-states, perhaps) users of this
    !         program may want to reverse the order of appearance of the month
    !         and the day, in conformity with established european usage.
    !     installation-dependent  EMTP  module written for the  DEC
    !     VAX-11/780.    'idate'  is a  dec  system subroutine which
    !     returns the month, day, and year (of century) as three  integer*2
    !     numerical values.
    character(4), dimension(:), intent(out) :: a
    character(8) :: date
    call date_and_time (date = date)
    write (unit = a(1), fmt = 10) date(7 : 8), date(5 : 5)
10  format (a2, '/', a1)
    write (unit = a(2), fmt = 20) date(6 : 6), date(3 : 4)
20  format (a1, '/', a2)
    return
  end subroutine date44

end module bcddat

!
! end of file date44.f90
!
