!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file: ntacs1b.f90
!

!
! subroutine ntacs1b.
!

subroutine ntacs1b
  use blkcom
  use tacsar
  use tacsto
  use labcom
  implicit none
  !  character*6  texvec
  !  equivalence (lstat(67), maxniu)
  !  equivalence (lstat(68), maxnuk)
  !  equivalence    ( moncar(  5),  ioutcs )
  character(6) :: hus1
  character(8) :: real8
  !
  sptr = sptr - 1
  isto(sptr) = env
  env = datenv
  nuk = isto(env + 11)
  env = isto(env + 36)
  i = 0
3001 if (.not. (env .gt. 0)) go to 3000
  i = i + 1
  i1 = isto(env + 2)
  i2 = isto(env + 3) - 1
!  bus1=blank     ! wsm + thl manual modification for bpa emtp
  hus1 = ' '                ! wsm + thl manual modification for bpa emtp
  j = 0
  do
     if (.not. (j .lt. i1)) go to 3010
     j = j + 1
     hus1(j : j) = csto(i2 + j)
  end do
3010 continue
  n = 0
  read (hus1, 7398) real8
7398 format (a6)
  call namea6 (real8, n)                                    ! wsm + thl manual modification for bpa emtp
  ilntab(klntab + i) = n
  env = isto(env)
  go to 3001
3000 continue
  env = isto(sptr)
  ioutcs = 0
  env = isto(env + 1)
  i = 0
3021 if (.not. (env .gt. 0)) go to 3020
  i1 = isto(env + 4)
  i2 = isto(env + 5) - 1
                                                            !  bus1=blank     ! wsm + thl manual modification for bpa emtp
  hus1 = ' '                                                ! wsm + thl manual modification for bpa emtp
  j = 0
3031 if (.not. (j .lt. i1)) go to 3030
  j = j + 1
  hus1(j:j) = csto(i2 + j)
  go to 3031
3030 continue
  read (hus1, 7398) real8
  n = 0
  call namea6(real8, n)                                     ! wsm + thl manual modification for bpa emtp
  if (.not. (nuk .lt. n - 1)) go to 5030
  nuk = n - 1                                               !only in bpa emtp
  ilntab(klntab + nuk) = n
5030 continue
  ioutcs = ioutcs + 1
  jout(kjout + ioutcs) = n - 1                              !only in bpa emtp
  isto(env + 6) = n - 1                                     !only in bpa emtp
  env = isto(env + 0)
  go to 3021
3020 continue
  env = isto(sptr)
  sptr = sptr + 1
  if (nuk .gt. maxnuk) stop 'kill 122 on maxnuk in ntacs1b.'
  kaliu = klntab + nuk
  niu = 0
  sptr = sptr - 1
  isto(sptr) = env
  env = isto(env + 4)
  k = 0
3041 if (.not. (env .gt. 0 .and. k .eq. 0)) go to 3040
  if (.not. (isto(env + 1) .eq. 1)) go to 5001
  k = 1
  env = isto(env + 2)
  go to 5000
5001 continue
  env = isto(env + 0)
5000 continue
  go to 3041
3040 continue
  if (.not. (k .gt. 0)) go to 5011
3051 if (.not. (env .gt. 0)) go to 3050
  niu = niu + 1
  !  bus1=blank     ! wsm + thl manual modification for bpa emtp
  hus1 = ' '                ! wsm + thl manual modification for bpa emtp
  i1 = isto(env + 5)
  i2 = isto(env + 6) - 1
  j = 0
3061 if (.not. (j .lt. i1)) go to 3060
  j = j + 1
  hus1(j:j) = csto(i2 + j)      ! wsm + thl manual modification for bpa emtp
  go to 3061
3060 continue
  n = 0
  read (hus1, 7398) real8
  call namea6(real8, n)
  ilntab(kaliu + niu) = n
  iuty(kiuty + niu) = isto(env + 2) + 89
  env = isto(env + 0)
  go to 3051
3050 continue
  go to 5010
5011 continue
5010 continue
  env = isto(sptr)
  sptr = sptr + 1
  if (niu .gt. maxniu) stop 'kill 122 on maxniu in ntacs1b.'
  ktab = nuk + niu
  lstat(39) = 30 + 3 * nuk + 8 * niu
  return
end subroutine ntacs1b

!
! end of file ntacs1b.for
!
