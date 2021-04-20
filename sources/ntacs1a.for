!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: ntacs1.for
!
!
!     subroutine ntacs1a.
!
subroutine ntacs1a        ! wsm + thl manual modification for bpa emtp
  include 'tacsto.ins'
  open(unit09, file = 'tacs.tim', status = 'old')
  read(unit09,901) ndx0
901 format(i1)
  close(unit09)
  open(unit08, file = 'tacs.rec', status = 'new', carriagecontrol = 'fortran')
  write(unit08,1009)
1009 format(1x)
  call tread
  call calsto
  open(bkfile, access= 'direct', recl = pgsize, status = 'scratch')
  call ptacs
  return
end subroutine ntacs1a
!
!     end of file: ntacs1.for
!
