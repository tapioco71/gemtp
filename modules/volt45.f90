!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file volt45.f90
!

!     Deck used only by "Semlyen setup" (overlay 45) and
!     "Hauer setup" (overlays 48 and 49) to provide inter-
!     overlay communication (to "line constants" [44],
!     "cable constants" [47],  and kill=183 error stop [54]).
!     This deck was added as a quick and dirty fix when we
!     moved volti, etc. from blkcom to labcom (as part of
!     variable dimensioning).   Deck is always in memory.

module volpri
  real(8) ::  volti(50), voltk(50), vim(50)
  !  common /volpri/ volti(50), voltk(50), vim(50)
end module volpri

!
! end of file volt45.f90
!
