!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: ntacs1.f90
!

!
!     subroutine ntacs1a.
!

subroutine ntacs1a        ! wsm + thl manual modification for bpa emtp
  include 'tacsto.ftn'
  open (unit = unit09, file = 'tacs.tim', status = 'old')
  read (unit = unit09, fmt = 901) ndx0
901 format (i1)
  close (unit = unit09)
!  open (unit = unit08, file = 'tacs.rec', status = 'new', carriagecontrol = 'fortran')
  open (unit = unit08, file = 'tacs.rec', status = 'new')
  write (unit = unit08, fmt = 1009)
1009 format (1x)
  call tread
  call calsto
  open (unit = bkfile, access= 'direct', recl = pgsize, status = 'scratch')
  call ptacs
  return
end subroutine ntacs1a

!
!     end of file: ntacs1.f90
!
