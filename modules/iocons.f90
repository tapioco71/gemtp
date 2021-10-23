!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file io.f90
!

module io
  integer(4) gfortran_stderr_unit, gfortran_stdin_unit, gfortran_stdout_unit
  common /iospace/ gfortran_stderr_unit
  common /iospace/ gfortran_stdin_unit
  common /iospace/ gfortran_stdout_unit
end module io

!
! end of file io.f90
!
