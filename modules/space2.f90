!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file space2.f90
!

module space2
  common /spac02/ norder(20)
  common /spac03/ index(40)
  common /spac04/ diag(15)
  !common /spac04/ diag
  dimension ich2(15)
  equivalence (diag(1), ich2(1))
  !common /spac05/ diab(1)
  common /spac05/ diab(15)
  dimension loc(15)
  equivalence (diab(1), loc(1))
  !common  /spac06/ solr(1)
  common  /spac06/ solr(15)
  dimension kownt(15)
  equivalence (solr(1), kownt(1))
  !common /spac07/ soli(1)
  common /spac07/ soli(15)
  common /spac08/ ich1(15)
  !common /spac01/ bnd (1)
  common /spac01/ bnd (30000)
  dimension korder(30000)
  equivalence (bnd(1), korder(1))
  common /spac10/ iloc(40)
  common /spac11/ gnd(1)
  dimension kolum(30000), frandn(3), irandn(3)
  equivalence (gnd(1), kolum(1), frandn(1), irandn(1))
end module space2

!
! end of file space2.f90
!
