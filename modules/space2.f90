!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file space2.f90
!

module space2
  implicit none
  !  common /spac01/ bnd (1)
  !  common /spac01/ bnd (30000)
  real(8) :: bnd(30000)
  !  common /spac03/ index(40)
  integer(4) :: index(40)
  !  common /spac04/ diag(15)
  !  common /spac04/ diag
  real(8) :: diag(15)
  !  common /spac05/ diab(1)
  !  common /spac05/ diab(15)
  real(8) :: diab(15)
  !  common  /spac06/ solr(1)
  !  common  /spac06/ solr(15)
  real(8) :: solr(15)
  !  common /spac07/ soli(1)
  !  common /spac07/ soli(15)
  real(8) :: soli(15)
  !  common /spac08/ ich1(15)
  integer(4) :: ich1(15)
  !  common /spac10/ iloc(40)
  integer(4) :: iloc(40)
  !  common /spac11/ gnd(1)
  real(8) :: gnd(1)
  !  common /spac02/ norder(20)
  integer(4) :: norder(20)
  integer(4) :: ich2(15), irandn(3)
  integer(4) :: kolum(30000), korder(30000), kownt(15)
  integer(4) :: loc(15)
  real(8) :: frandn(3)
  equivalence (diag(1), ich2(1))
  equivalence (diab(1), loc(1))
  equivalence (solr(1), kownt(1))
  equivalence (bnd(1), korder(1))
  equivalence (gnd(1), kolum(1))
  equivalence (frandn(1), irandn(1))
  ! Equivalences friend zone:
  ! from over7
  integer(4) :: lorder(15)
  equivalence (ich2(1), lorder(1))
  integer(4), dimension(0 : 20, 0 : 20) :: locatn
end module space2

!
! end of file space2.f90
!
