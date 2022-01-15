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
  return
end subroutine dshabs

subroutine movabs
  implicit none
  return
end subroutine movabs

subroutine factor
  implicit none
  return
end subroutine factor

subroutine line
  implicit none
  return
end subroutine line

subroutine newpen
  implicit none
  return
end subroutine newpen

subroutine number
  implicit none
  return
end subroutine number

! subroutine plot (x, y, n)
!   implicit none
!   integer(4), intent(in) :: n
!   real(8), intent(in) :: x, y
!   write (unit = *, fmt = 10) x, y, n
! 10 format (' Use of dummy plot in calcolm. ', e10.3, 1x, e10.3, 1x, i4, '.  Halt.')
!   return
! end subroutine plot

subroutine plot (x, y, n)
  implicit none
  integer(4), intent (in) :: n
  real(8), intent(in) :: x, y
  return
end subroutine plot

subroutine plots
  implicit none
  return
end subroutine plots

subroutine scale
  implicit none
  return
end subroutine scale

subroutine symbol
  implicit none
  return
end subroutine symbol

!
! end of file calcom.f90
!
