!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file tacsar.f90
!

!     000000            definition of table names                 000000

module tacsar
  implicit none
  integer(4) :: icolcs, ilntab, insup, isblk, iuty, ivarb
  integer(4) :: jout
  integer(4) :: kaliu, kalksu, katcs, kawkcs, kcolcs
  integer(4) :: kinsup, kisblk, kiuty, kivarb, kjout, kksus, klntab, kofsce, koncur
  integer(4) :: konsce, konsup, kontot, kprsup, krsblk, kspvar, ksus, kud1
  integer(4) :: kxar, kxtcs
  real(8) :: atcs, awkcs
  real(8) :: parsup
  real(8) :: rsblk
  real(8), private :: sptacs
  real(8) :: ud1
  real(8) :: xar, xtcs
  !
  dimension icolcs(1), ilntab(170000), insup(170000), isblk(1), jout(1)
  dimension ksus(1), iuty(170000), ivarb(170000)
  dimension rsblk(1), ud1(5000), xtcs(1), atcs(1), xar(1)
  dimension parsup(90000), awkcs(1), sptacs(25)
  !     equivalencing of scalars which are to be carried between modules.
  equivalence (konsce, sptacs(1)), (koncur, sptacs(2))
  equivalence (kontot, sptacs(3)), (kofsce, sptacs(4))
  equivalence (kcolcs, sptacs(5)), (kspvar, sptacs(6))
  equivalence (katcs, sptacs(7)), (konsup, sptacs(8))
  equivalence (kprsup, sptacs(9)), (kivarb, sptacs(10))
  equivalence (kaliu, sptacs(11)), (kjout, sptacs(12))
  equivalence (kiuty, sptacs(13)), (kud1, sptacs(14))
  equivalence (kawkcs, sptacs(15)), (kxar, sptacs(16))
  equivalence (kxtcs, sptacs(17)), (klntab, sptacs(18))
  equivalence (kisblk, sptacs(19)), (krsblk, sptacs(20))
  equivalence (kksus, sptacs(21)), (kalksu, sptacs(22))
  equivalence (kinsup, sptacs(23))
  equivalence (sptacs(1), isblk(1), ksus(1), iuty(1))
  equivalence (sptacs(1), ilntab(1), icolcs(1), jout(1))
  equivalence (sptacs(1), insup (1), ivarb(1), parsup(1))
  equivalence (sptacs(1), rsblk(1), ud1(1), xtcs(1))
  equivalence (sptacs(1), atcs(1), xar(1), awkcs(1))
end module tacsar

!
! end file tacsar.f90
!
