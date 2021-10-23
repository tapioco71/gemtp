!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file calcom.f90
!

!
! subroutine calcom.
!

subroutine calcom
  implicit none
  write (unit = *, fmt = *) ' Use of dummy calcomp in calcom.  Halt.'
  stop
end subroutine calcom

subroutine dshabs
  implicit none
end subroutine dshabs

subroutine movabs
  implicit none
end subroutine movabs

subroutine factor
  implicit none
end subroutine factor

subroutine line
  implicit none
end subroutine line

subroutine newpen
  implicit none
end subroutine newpen

subroutine number
  implicit none
end subroutine number

subroutine plot (x, y, n)
  implicit none
  integer(4), intent(in) :: n
  real(8), intent(in) :: x, y
  write (unit = *, fmt = 10) x, y, n
10 format (' Use of dummy plot in calcolm. ', 2e8.3, i4, '.  Halt.')
end subroutine plot

subroutine plots
  implicit none
end subroutine plots

subroutine scale
  implicit none
end subroutine scale

subroutine symbol
  implicit none
end subroutine symbol

!
! end of file calcom.f90
!
