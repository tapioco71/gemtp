!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file strcom.f90
!

!
! subroutine to_upper.
!

module strcom
  implicit none

contains

  function to_upper (strIn) result(strOut)
    ! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
    ! Original author: Clive Page
    implicit none
    character(len = *), intent(in) :: strIn
    character(len = len(strIn)) :: strOut
    integer(4) :: i, j
    !
    do i = 1, len (strIn)
       j = iachar (strIn(i : i))
       if (j >= iachar ("a") .and. j <= iachar ("z")) then
          strOut(i : i) = achar (iachar (strIn(i : i)) - iachar ("a") + iachar ("A"))
       else
          strOut(i : i) = strIn(i : i)
       end if
    end do
  end function to_upper

  function to_lower (strIn) result(strOut)
    implicit none
    character(len = *), intent(in) :: strIn
    character(len = len(strIn)) :: strOut
    integer(4) :: i, j
    !
    do i = 1, len (strIn)
       j = iachar (strIn(i : i))
       if (j >= iachar ("A") .and. j <= iachar ("Z")) then
          strOut(i : i) = achar (iachar (strIn(i : i)) - iachar ("A") + iachar ("a"))
       else
          strOut(i : i) = strIn(i : i)
       end if
    end do
  end function to_lower

  !
  ! subroutine str2int.
  !

  subroutine str2int (str, int, stat)
    implicit none
    character(len = *), intent(in) :: str
    integer(4), intent(out) :: int
    integer(4), intent(out) :: stat
    !
    read (unit = str, fmt = 10, iostat = stat) int
10  format (i8)
  end subroutine str2int

end module strcom
