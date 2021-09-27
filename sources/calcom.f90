!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
!     file: calcom.for
!

!
!     subroutine calcom.
!

subroutine calcom
  entry dshabs
  entry movabs
!  entry axis
  entry factor
  entry line
  entry newpen
  entry number

  entry plot (x, y, n)
  integer(8), intent(in) :: n
  real(8), intent(in) :: x, y
  return

  entry plots
  entry scale
  entry symbol
  write (unit = *, fmt = *) ' Use of dummy calcomp in calcom.  Halt.'
  stop
end subroutine calcom

!
!     file: calcom.for
!
