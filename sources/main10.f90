!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file main10.f90
!

!
!     The EMTP solution process proper consists of numerous secondary-
!     level overlays, each called from module  main10  using
!     module number  'nchain'  as the indicator of where control is to
!     pass next.   the following is a tabulation of valid  'nchain'
!     values, along with the function of the corresponding code.   With
!     the possible exception of the time-step loop,  'nchain'  is the
!     utpf  overlay number of the code to which control is to be
!     transfered next.   The overlay number of the time-step loop
!     is given by  'nchain'  corresponding to the first quarter of
!     the time-step-loop segmentation, on machines not using ecs
!     overlaying.
!
!     1.  input of miscellaneous data cards.
!     2.  input branch data.
!     3.  code associated with the cascading of pi-circuits for
!         steady-state phasor solutions only.
!     4.  input and processing of frequency-dependence data for
!         distributed-parameter line mode.
!     5.  input of switch and source data cards.
!     6.  network connectivity output.   setup of transient-network
!         renumbering tables.
!     7.  network node renumbering routine (John Walker's old
!         subroutine number).
!     8.  convert tables to new node numbers.   Find steady-state
!         phasor equivalents for distributed branches.
!     9.  set up steady-state phasor network renumbering tables.
!     10. form the steady-state admittance matrix (Y).   Solve for
!         steady-state phasor voltages.
!     11. output steady-state phasor solution (if requested).
!     12. branch-table and switch-table processing, as preparation
!         for the integration in time-step loop.
!     13. setup initial conditions on lumped elements, and past
!         history for distributed lines.
!     14. form (Y) for the transients network.   Triangularize the
!         first partition (nonswitch/source nodes).
!     15. final setup operations before time-step loop.
!     16. first quarter of time-step loop (checking for changes of
!         switches and pseudo-nonlinear elements, retriangularization
!         of  Ybb  and calculation of thevenin impedance vectors).
!     17. second quarter of time-step loop (branch-table history
!         updating, addition of branch contributions to nodal
!         injected current vector  i ).
!     18. third quarter of time-step loop (source update, repeat
!         solution of   (y)v = i   for node voltage vector  v ).
!     19. fourth quarter of time-step loop (solution of 3-phase
!         nonlinearities and compensation-based rotating
!         machinery [type-50 s.m., u.m.],  superposition to
!         give the total solution including compensation).
!     20. punch and print terminal conditions (if requested).
!         catalog plot-data points on the disk as a permanent file, if
!         misc. data parameter  'icat'  is positive.
!

module savcom
  implicit none

contains

  !
  ! subroutine tapsav.
  !

  subroutine tapsav (narray, n1, n2, n3)
    use blkcom
    use deck29
    use indcom
    implicit none
    !     Near-universal module for dumping or restoring (central memory
    !     vs. disk) of  /label/ .   This does not work for those
    !     computers like Prime and Burroughs where  common  blocks
    !     are not ordered regularly in memory.   Switch "KBURRO"
    !     selects between disk or virtual memory (/C29B01/).
    integer(4), intent(out) :: narray(:)
    integer(4), intent(in) :: n1, n2, n3
    integer(4) :: i, j, k, kvecsv, n4, n9, n13
    !
    if (iprsup .lt. 1) go to 5840
    n9 = 0
    kpen(2) = 0
    n4 = location (narray)
    write (unit = lunit6, fmt = 5831) n1, n2, n3, kburro, n4
5831 format (/, " Top of  'tapsav'., '      n1      n2      n3  kburro              n4", /, 18x, 4i8, i16)
    !     following check normally sends VAX EMTP to 6327 (disk is
    !     only wanted for table saving within a simulation for
    !     test purposes):
5840 if (kburro .eq. 1) go to 6327
5448 if (n3 .gt. 1) go to 5891
    write (unit = n1) (narray(i), i = 1, n2)
    go to 9000
5891 read (unit = n1) (narray(i), i = 1, n2)
    go to 9000
6327 if (nchain .eq. 20 .and. memsav .eq. 1) go to 5448
    if (nchain .eq. 1) go to 5448
    if (nchain .eq. 6 .or. nchain .eq. 8) go to 9000
    !     Preceding "if" branches will send  "memsav=1"  table
    !     saving of overlay 20 and  "start again"  table restoring
    !     of overlay 1 to disk, always.   This is for permanent
    !     (disk) storage.   "statistics"  and  "systematic"  data
    !     cases, on the other hand, shall dump to  /c29b01/ .
    !     First 50 cells of /c29b01/ (karray) are saved for rtm use.
    n13 = 29
    call dimens (kpen, n13, trash, trash)
    kvecsv = 2 * (it + it + ibr + ntot + ioffd) + kswtch + lhist
    n9 = ltlabl + kvecsv * nbyte(3) / nbyte(4)
    if (n9 .lt. kpen(2) + 50) go to 6342
    write (unit = lunit6, fmt = 6335) n2, kpen(2), n9, nchain
6335 format (' Error stop in "tapsav".  Overflow of /c29b01/ storage.  n2, kpen(2) =', 2i8, '     needed storage n9 =,  i8     ', /, ' memory requirement in integer words for virtual   computer implementation of tapsav.   Storage must ', /, ' provide for all of --/label/--( deck "labcom" ),    the several usages of "vecrsv" and "vecisv"(over6-11),  ', ' plus 50 extra cells.   ', /, ' nchain =', i5)
    call stoptp
6342 j = 50
    if (n3 .gt. 1) go to 6352
    do k = 1, n2
       j = j + 1
       karray(j) = narray(k)
    end do
    go to 9000
6352 do k = 1, n2
       j = j + 1
       narray(k) = karray(j)
    end do
9000 if (iprsup .ge. 1) write (unit = lunit6, fmt = 9003) n9, kpen(2)
9003 format (' Exit "tapsav".   n9, kpen(2) =', 2i8)
    return
  end subroutine tapsav

end module savcom

!
!     subroutine subr10.
!

subroutine subr10
  use blkcom
  use labcom
  use tacsar
  use smtacs
  use umcom
  implicit none
  integer(4) :: iprcbl, n24
  !
  ktab = 0
  do
     if (nchain .gt. 20) go to 9000
     if (kill .ne. 0) then
        nchain = 51
     else
        n24 = nchain
        if (n24 .lt. 1) n24 = 1
        iprsup = iprsov(n24)
        iprcbl = iprsup
        if (m4plot .eq. 1) call emtspy
        if (nchain .eq. 0) nchain = 1
        select case (nchain)

        case (1)
#ifdef WITH_OVER1
           call over1
#else
           call dummy
#endif

        case (2)
#ifdef WITH_OVER2
           call over2
#else
           call dummy
#endif

        case (3, 4)
           call stoptp

        case (5)
#ifdef WITH_OVER5
           call over5
#else
           call dummy
#endif

        case (6)
#ifdef WITH_OVER6
           call over6
#else
           call dummy
#endif

        case (7)
#ifdef WITH_OVER7
           call over7
#else
           call dummy
#endif

#ifdef WITH_OVER8
        case (8)
           call over8
#else
           call dummy
#endif

        case (9)
#ifdef WITH_OVER9
           call over9
#else
           call dummy
#endif

        case (10)
#ifdef WITH_OVER10
           call over10
#else
           call dummy
#endif

        case (11)
#ifdef WITH_OVER11
           call over11
#else
           call dummy
#endif

        case (12)
#ifdef WITH_OVER12
           call over12
#else
           call dummy
#endif

        case (13)
#ifdef WITH_OVER13
           call over13
#else
           call dummy
#endif

#ifdef WITH_OVER14
        case (14)
           call over14
#else
           call dummy
#endif

        case (15)
#ifdef WITH_OVER15
           call over15
#else
           call dummy
#endif

        case (16 : 19)
#ifdef WITH_OVER16
           call over16
#else
           call dummy
#endif

        case (20)
#ifdef WITH_OVER20
           call over20
#else
           call dummy
#endif

        case default
#ifdef WITH_OVER1
           call over1
#else
           call dummy
#endif
        end select
     end if
  end do
9000 return
end subroutine subr10

!
! subroutine dpelg.
!

subroutine dgelg (r, a, m, n, eps, ier)
  use tracom
  implicit none
  integer(4), intent(out) :: ier
  integer(4), intent(in) :: m, n
  real(8), intent(out) :: a(m * m), r(m * n)
  real(8), intent(in) :: eps
  integer(4) :: i, ii, ist, j, k, l, lend, ll, lst, mm, nm
  real(8) :: piv, pivi, tb, tol
  !
  i = 0
  !     Purpose:
  !             to solve a general system of simultaneous linear equations.
  !
  !     Usage:
  !             call dgelg(r, a, m, n, eps, ier)
  !
  !     Description of parameters:
  !             r      - double precision m by n right hand side matrix
  !                      (destroyed). On return r contains the solutions
  !                      of the equations.
  !             a      - double precision m by m coefficient matrix
  !                      (destroyed).
  !             m      - the number of equations in the system.
  !             n      - the number of right hand side vectors.
  !             eps    - single precision input constant which is used as
  !                      relative tolerance for test on loss of
  !                      significance.
  !             ier    - resulting error parameter coded as follows
  !                      ier=0  - no error,
  !                      ier=-1 - no result because of m less than 1 or
  !                               pivot element at any elimination step
  !                               equal to 0,
  !                      ier=k  - warning due to possible loss of signifi-
  !                               cance indicated at elimination step k+1,
  !                               where pivot element was less than or
  !                               equal to the internal tolerance eps times
  !                               absolutely greatest element of matrix a.
  !
  !     Remarks:
  !             Input matrices r and a are assumed to be stored columnwise
  !             in m*n resp. m*m successive storage locations. On return
  !             solution matrix r is stored columnwise too.
  !             The procedure gives results if the number of equations m is
  !             greater than 0 and pivot elements at all elimination steps
  !             are different from 0. However warning ier=k - if given -
  !             indicates possible loss of significance. In case of a well
  !             scaled matrix a and appropriate tolerance eps, ier=k may be
  !             interpreted that matrix a has the rank k. No warning is
  !             given in case m=1.
  !
  !     Method:
  !             Solution is done by means of gauss-elimination with
  !             complete pivoting.
  if (m .le. 0) go to 23
  !     search for greatest element in matrix a
  ier = 0
  piv = 0.0
  mm = m * m
  nm = n * m
  do l = 1, mm
     tb = absz (a(l))
     if (tb .le. piv) go to 3
     piv = tb
     i = l
  end do
3 continue
  tol = eps * piv
  !     a(i) is pivot element. piv contains the absolute value of a(i).
  !     start elimination loop
  lst = 1
  do k = 1, m
     !     test on singularity
     if (piv .le. 0.0) go to 23
     if (ier .ne. 0) go to 7
     if (piv .gt. tol) go to 7
     ier = k - 1
7    pivi = 1.0 / a(i)
     j = (i - 1) / m
     i = i - j * m - k
     j = j + 1 - k
     !     i+k is row-index, j+k column-index of pivot element
     !     pivot row reduction and row interchange in right hand side r
     do l = k, nm, m
        ll = l + i
        tb = pivi * r(ll)
        r(ll) = r(l)
        r(l) = tb
     end do
     !     is elimination terminated
     if (k .ge. m) go to 18
     !     column interchange in matrix a
     lend = lst + m - k
     if (j .le. 0) go to 12
     ii = j * m
     do l = lst, lend
        tb = a(l)
        ll = l + ii
        a(l) = a(ll)
        a(ll) = tb
     end do
     !     row interchange and pivot row reduction in matrix a
12   do l = lst, mm, m
        ll = l + i
        tb = pivi * a(ll)
        a(ll) = a(l)
        a(l) = tb
     end do
     !     save column interchange information
     a(lst)=j
     !     element reduction and next pivot search
     piv = 0.0
     lst = lst + 1
     j = 0
     do ii = lst, lend
        pivi = -a(ii)
        ist = ii + m
        j = j + 1
        do l = ist, mm, m
           ll = l - j
           a(l) = a(l) + pivi * a(ll)
           tb = absz (a(l))
           if (tb .le. piv) go to 15
           piv = tb
           i = l
        end do
15      continue
        do l = k, nm, m
           ll = l + j
           r(ll) = r(ll) + pivi * r(l)
        end do
     end do
     lst = lst + m
  end do
  !     end of elimination loop
  !     back substitution and back interchange
18 if (m .gt. 1) go to 19
  if (m .eq. 1) go to 22
  go to 23
19 ist = mm + m
  lst = m + 1
  do i = 2, m
     ii = lst - i
     ist = ist - lst
     l = ist - m
     l = int(a(l) + 0.5)
     do j = ii, nm, m
        tb = r(j)
        ll=j
        do k = ist, mm, m
           ll = ll + 1
           tb = tb - a(k) * r(ll)
        end do
        k = j + l
        r(j) = r(k)
        r(k) = tb
     end do
     return
  end do
22 return
  !     error return
23 ier = -1
  return
end subroutine dgelg

!
! subroutine matmul.
!

subroutine matmul (aum, bum)
  implicit none
  !     matrix algebra module used by universal machine (u.m.)
  integer(4) :: n1, n2, n3, n5
  real(8), intent(out) :: aum, bum
  real(8) :: cum
  dimension aum(3, 3), bum(3, 3), cum(3, 3)
  n5 = 3
  do n1 = 1, n5
     do n2 = 1, n5
        cum(n1, n2) = aum(n1, 1) * bum(1, n2)
        do n3 = 2, n5
           cum(n1, n2) =  cum(n1, n2) + aum(n1, n3) * bum(n3, n2)
        end do
     end do
  end do
  do n1 = 1, n5
     do n2 = 1, n5
        aum(n1, n2) = cum(n1, n2)
     end do
  end do
  return
end subroutine matmul

!
! subroutine matvec.
!

subroutine matvec (aum, yum)
  implicit none
  !     matrix algebra module used by universal machine (u.m.)
  real(8), intent(in) :: aum
  real(8), intent(out) :: yum
  real(8) :: x
  dimension aum(3, 3), yum(15), x(3)
  integer(4) :: n1, n2, n3
  n1 = 3
  x(1 : n1) = 0.0
  do n2 = 1, n1
     do n3 = 1, n1
        x(n2) = x(n2) + aum(n2, n3) * yum(n3)
     end do
  end do
  yum(1 : n1) = x(1 : n1)
  return
end subroutine matvec

!
! subroutine pltfil.
!

subroutine pltfil (k)
  use blkcom
  use labcom
  use dekspy
  implicit none
  !     Installation-dependent module which is called for
  !     output-vector dumping by  "subts3"  and  "over20"  if
  !     "m4plot" of  /blank/  is nonzero.   This
  !     is alternative to conventional in-line dumping on disk.
  !     Module should be universal for computers using fortran 77
  !     compilers and real*4 variables which give single precision.
  integer(4), intent(in) :: k
  integer(4) :: j, ll10, n7, n13, n17
  !
  save
  data n17 / 0 /                                            ! initialize rolling plot freq count
  if (m4plot .eq. 1) go to 7286                             ! simulator use
  !     We pass here with m4plot=2,  indicating disk storage,  but
  !     single-precision (real*4) numbers only:
  if (k .le. 450 ) go to 7273
  write (unit = lunit6, fmt = 7269) k
7269 format (' ^^^^^^^^^^^^^^^   Error stop in "pltfil"   ^^^^^^^^^^^^^^', /, ' ^^^^^^  too many output variables (', i3,  &
       ' )  for use real*4 plot file.   limit = 450.')
  call stoptp                                               ! installation-dependent program stop card
  !7273 do j = 1, k
  !     forbyt(j) = volti(j)
  !  end do
7273 forbyt(1 : k) = volti(1 : k)
  !     following apollo card replaces 2 preceding vax ones:
  !     7273 call vec_$dp_sp ( volti(1), forbyt(1), k )
  write (unit = lunit4) (forbyt(j), j = 1, k)
  go to 9000                                                ! exit module after possible diagnostic
7286 if (iprsup .ge. 3) write (unit = lunit6, fmt = 7301) indbuf, mflush, k, limbuf, newvec, numcrd, (volti(j), j = 1, 10)
7301 format (' Top of "pltfil".  indbuf, mflush, k, limbuf, newvec, numcrd =', 6i10, /, 1x, 10e13.4)
  n7 = 0
  if (indbuf .gt. 0) go to 7308                             ! not very 1st step
  !     plot data storage begins after "tables" usage for "labcom"
  !     plus 50 misc. cells, plus EMTP data cards.  offset is:
  mflush = 0                                                ! we have flushed pltbuf to disk zero times
  indbeg = ltlabl + 51                                      ! plot points offset by "labcom"
  !     7304 indbuf = indbeg                                ! reset plot storage at beginning
  indbuf = indbeg                                           ! reset plot storage at beginning
  newvec = indbuf + 1                                       ! plot data begins after data cards
7308 if (indbuf + k .le. limbuf) go to 7374                 ! not full yet
  write (unit = lunit6, fmt = 7311) indbuf, limbuf
  write (unit = munit6, fmt = 7311) indbuf, limbuf
7311 format ('   % % % % % %   Suspended simulation;  plot data space exhausted;  use spy.   indbuf, limbuf =', 2i8)
  call window                                               ! output of character variable munit6
  write (unit = munit6, fmt = 7312)
7312 format ('   % %  Time-sharing disabled.   Send user-keyed interrupt to silence alarm.')
  call window                                               ! output of character variable munit6
  ll10 = 10
  call honker (ll10)                                        ! disaster-level audible indication
  n13 = kbreak                                              ! remember present value, for after spy
  lockbr = 1                                                ! ensure forced stay in spy until "unlock"
  lastov = 9911                                             ! remember overflow for "flager", "spyink"
  call emtspy                                               ! transfer control to spy, to manage space
  go to 7308                                                ! b4 continue, check that user did his job
7374 do j = 1, k                                            ! transfer each cell j of output vector
     indbuf = indbuf + 1                                    ! index to next unused pltbuf cell
     pltbuf(indbuf) = volti(j)                              !  end  do 7389  loop over "j"
  end do
  !     following 2 apollo cards replace 3 preceding vax ones:
  !     7374 call vec_$dp_sp ( volti(1), pltbuf(indbuf+1), k )
  !     indbuf = indbuf + k
  n17 = n17 + 1                                             ! another step since last rolling service
  if (n17 .lt. kslowr) go to 9000                           ! not enough points yet
  n17 = 0                                                   ! reset counter to delay rolling after present
  if (monitr .eq. 0) go to 7396                             ! no rolling vector plot
  call tekplt                                               ! add to right edge of present vector plot
  if (monitr .ne. 8765) go to 7396                          ! vector addition done
  call timval                                               ! regenerate plot, with right half on left
  monitr = 1                                                ! plot regenerated, so turn off request flag
7396 if (monits .ne. 0) call chrplt                         ! rolling character plot
9000 if (iprsup .ge. 4) write (lunit6, 9004)
9004 format (' Exit "pltfil".')
  return
end subroutine pltfil

!
! subroutine pltlu2.
!

subroutine pltlu2 (d2, volti)
  use blkcom
  implicit none
  !     called by "tacs2" only, if and only if m4plot .ne. 0
  !     this module is universal for fortran 77 compilers and
  !     computers for which real*4 corresponds to single precision.
  real(8), intent(out) :: d2, volti(1)
  integer j, n12
  real(8) :: forbyt(150)
  !
  if (iofgnd .gt. 149) call stoptp
  n12 = iofgnd + 1
  read (unit = lunit2) (forbyt(j), j = 1, n12)
  d2 = forbyt(1)
  do j = 1, iofgnd
     volti(j) = forbyt(j + 1)
  end do
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 1978) d2, volti(1), volti(iofgnd)
1978 format (' Exit "pltlu2".  d2, volti(1, iofgnd) =', 3e14.5)
  return
end subroutine pltlu2


!
! subroutine namea6.
!

subroutine namea6 (text1, n24)
  use blkcom
  use labcom
  implicit none
  ! module for maintainance of alphanumeric vector texvec of
  ! "labcom".  maxbus of "blkcom" is last used cell.  n24 chooses
  ! mode of use:  0 will add text1, positive will locate it,
  ! and negative will destroy (remove) it.
  integer(4), intent(out) :: n24
  character(8), intent(in) :: text1
  integer(4) :: j, n17
  character(8) :: text2
  !
  ! Burroughs: preserve local variable between module calls:
  data n17 / 0 /
  data text2 / 'unused' /
  if (maxbus .le. 0) go to 3423
  do j = 1, maxbus
     if (text1 .eq. texvec(j)) go to 3446
  end do
3423 if (n24 .ne. 0) go to 3438
  if (n17 .eq. 0) go to 3434
  texvec(n17) = text1
  n24 = n17
  do j = 1, maxbus
     if (texvec(j) .ne. text2) go to 3428
     n17 = j
     go to 9000
3428 continue
  end do
  n17 = 0
  go to 9000
3434 maxbus = maxbus + 1
  if (maxbus .gt. lsize7) stop
  texvec(maxbus) = text1
  n24 = maxbus
  go to 9000
3438 if (iprsup .ge. 1) write (unit = lunit6, fmt = 3442) maxbus, text1, n24
3442 format ('  +++++  Search of EMTP name vector bus through cell', i5, '   in  "namea6"  shows no match for', /, '         .', '"', a6, '"', '.   Return -intinf.', i10)
  n24 = -intinf
  go to 9000
3446 if (n24 .lt. 0) go to 3455
  n24 = j
  go to 9000
3455 texvec(j) = text2
  n17 = j
9000 if (iprsup .ge. 6) write (unit = lunit6, fmt = 9004) text1, maxbus, n24, j
9004 format (' Exit "namea6".  text1, maxbus, n24, j =', 2x, a6, 3i10)
  return
end subroutine namea6

!
! subroutine tables.
!

subroutine tables
  use savcom
  use comlock
  use blkcom
  use labcom
  use smtacs
  use smach
  use umcom
  use indcom
  use movcop
  implicit none
  !     Utility which is used to both dump and restore EMTP
  !     tables (central memory vs. disk).  Usage is for both
  !     "statistics" (over12, over15, over20) and  "start again"
  !     (over1, over20).   Call to  "tapsav"  dumps  /label/ .
  !     Also used by  $restart  request of  "cimage"  (called
  !     from  "lookie"  which is called by  "subts3"  of  ov16).
  !     Also used by  "restore"  request of rtm, where table
  !     restoration is in  "katalg"  of overlay 20.
  !     Note about deck "synmac".   If EMTP s.m. modeling
  !     Brandwajn (type-59), is to be deleted,
  !     then all s.m. subroutines ( smdat, smout, smpfit,
  !     smint, uncor, premec, elec, past,
  !     update, increm ) are to be destroyed,  and
  !     deck "synmac" is to be removed from the present module.
  !     but then  "dimension z(1)"  should be added in its
  !     place,  and the  "n4 ="  calculation involving "locint"
  !     should be replaced by the simple statements  "n4 = 1" .
  !     comment card immediately preceding "synmac" -------------
  !     comment card immediately following "synmac" -------------
  integer(4) :: i, j
  integer(4) :: ll1, ll2, ll4
  integer(4) :: n1, n2, n3, n4, n5, n9, n24
  !
  !  equivalence (x(1), integx(1))
  !  dimension busone(1), idistx(1)
  !  equivalence (bus1, busone(1)), (nenerg, idistx(1))
  !  dimension kpen(1), itemp(1), jtemp(1), ktemp(1)
  !  equivalence (kpen(1), bus1), (itemp(1), busum(1))
  !  equivalence (jtemp(1), etac(1)), (ktemp(1), z(1))
  !  equivalence (moncar(2), kbase)
  !  dimension iprsav(4)
  !
  !     Burroughs: preserve local variable between module calls:
  iprsav(1 : 4) = (/ 0, 0, 0, 0 /)
  ll1 = 1
  ll2 = 2
  ll4 = 4
  nword1 = location (voltbc(1)) - location (kpen(1))
  nword2 = location (idistx(1)) - location (lunsav(15))
  n4 = location (msmout) - location (z) + 1
  n5 = location (lbstac) - location (etac) + 1
  if (kbase .eq. 0) nword1 = location (idistx) - location (busone)
  n9 = location (istart) - location (busum) + 1
  rewind lunit2
  if (nchain .eq. 1) go to 3289
  if (nchain .eq. 20) go to 3289
3289 if (iprsup .ge. 1) write (unit = lunit6, fmt = 2721) n4, n5, nword1, nword2, ltlabl, n9, nchain, lastov, lunit2, t
2721 format (/, ' Within  "tables" .      n4      n5  nword1  nword2  ltlabl      n9  nchain  lastov  lunit2 ',  14x,  't', /, 19x, 9i8, e15.6)
  if (nchain .eq. 1) go to 5342
  if (memsav .eq. 1016) go to 5342
  if (nchain .lt. lastov) go to 5342
  write (unit = lunit2) locker
  write (unit = lunit2) (kpen(i), i = 1, nword1)
  write (unit = lunit2) (iprsov(i), i = 35, nword2)
  !     store iprsov(16-19) in iprsav at 1st call to tables from over12
  call move (iprsov(16 :), iprsav(1 :), ll4)
  call tapsav (integx, lunit2, ltlabl, ll1)
  if (numsm .ne. 0) write (unit = lunit2) (ktemp(i), i = 1, n4), (jtemp(i), i = 1, n5)
  write (unit = lunit2) (itemp(i), i = 1, n9)
  go to 5359
5342 read (unit = lunit2) locker
  if (iprsup .ge. 9) write (unit = lunit6, fmt = 66) locker
66 format (' After 1st read.  locker =', 2i8)
  n3 = nchain
  n2 = iprsup
  n24 = numdcd
  read (unit = lunit2) (kpen(i), i = 1, nword1)
  read (unit = lunit2) (iprsov(i), i = 35, nword2)
  call tapsav (integx, lunit2, ltlabl, ll2)
  if (iprsup .ge. 9) write (lunit6, 69) numsm, n4, n5, n9
69 format (' After tapsav: numsm,n4,n5,n9 =', 4i5)
  if (numsm .ne. 0) read (unit = lunit2) (ktemp(i), i = 1, n4), (jtemp(i), i = 1, n5)
  read (unit = lunit2) (itemp(i), i = 1, n9)
  nchain = n3
  iprsup = n2
  numdcd = n24
  !     restore alternate time-step loop diagnostic printout
  !     request for the first energization in a statistics case
  do j = 1, 4
     if (iprsov(j + 15) .ne. iprsav(j)) go to 1482
  end do
  go to 5359
1482 do j = 1, 4
     n1 = iprsov(j + 15)
     iprsov(j + 15) = iprsav(j)
     iprsov(j + 30) = n1
  end do
5359 if (iprsup .ge. 1) write (unit = lunit6, fmt = 5364)
5364 format (' Exit "tables".')
  return
end subroutine tables

!
! subroutine csup.
!

subroutine csup (l)
  use blkcom
  use labcom
  use tacsar
  use tracom
  use random
  implicit none
  integer(4), intent(in) :: l
  integer(4) :: i, i1, i2, idiv, idn(20), ifl(20), iop(50)
  integer(4) :: j, jfl, ji
  integer(4) :: k, k1, karg, kdev2, kjsup, kksup
  integer(4) :: m, m1, m2, mj
  integer(4) :: n, n1, n2, n3, n4, n5, n6, n7, ndx1, ndx2, ndx3, ndx4, ndx5, ndx6
  integer(4) :: nj, nn, nnn, nop
  real(8) :: a, aa, acc(20), amx(20), arg(50)
  real(8) :: b, bb
  real(8) :: d, d1, d2, d4, d5, d7, d8, d9, d10, d11, div
  real(8) :: g
  real(8) :: h
  real(8) :: rdev1
  real(8) :: zfl
  !
  !     000  b = the argument and later the value of the function,
  !     000      before it is affected by the algebraic operation
  !     000      which will update 'a' .
  !     000  a = intermediate values of the supplemental
  !     000      variable or device .
  !     000  xtcs( l) = the final value of the variable or device .
  kjsup = kinsup + lstat(65)
  kksup = kjsup  + lstat(65)
  !     2001 if ( iprsup .lt. 6 )  go to 1000
  if (iprsup .lt. 6) go to 1000
  write (unit = lunit6, fmt = 1001) t, nsup, karg, kpar
1001 format ('0entering subroutine  csup  at  t=', e13.6, /, '0e nsup=', i6, '   karg=', i8, '   kpar=', i6)
  write (unit = lunit6, fmt = 1002) (i, ilntab(i + kspvar), insup(i + kjsup), insup(i + kksup), i = 1, nsup)
1002 format ('  Number  supvar    jsup    ksup ', /, (4i8))
  write (unit = lunit6, fmt = 1033) karg
1033 format ('  karg = ', i8, /, '       n  iopsup  ifnsup  irgsup    idev     kdj     kdk  ildev1  ildev2')
  do i = 1, nsup
     n1 = insup(kjsup + i)
     if (n1 .lt. 0) go to 2014
     n2 = insup(kksup + i)
     write (unit = lunit6, fmt = 2008) (n,ivarb(n + 1), ivarb(n + 2), ivarb(n + 2), n = n1, n2, 3)
2008 format (4i8)
     go to 2034
2014 n1 = -n1
     write (unit = lunit6, fmt = 2022 ) n1, ivarb(n1), ivarb(n1 + 1), ivarb(n1 + 2), ivarb(n1 + 3), ivarb(n1 + 4)
2022 format (i8, 24x, 5i8)
  end do
2034 continue
  if (kpar .ne. 0) write (unit = lunit6, fmt = 1004) (i, parsup(i + kprsup), i = 1, kpar)
1004 format ('0e', 5x, 'parsup ...', /, (' e', 5(i3, 1x, e15.6, 3x)))
1000 nnn = kxtcs + nuk + lstat(64)
  i = l
1234 a = 0.0
  n1 = insup(kjsup + i)
  n2 = insup(kksup + i)
  if (n1 .lt. 0) go to 600
  if (n2 .lt. 0) go to 5000
  !     ------  old tacs pseudo-fortran "variable"  ------
  nop = 0
  k = n1 - 3
20 k = k + 3
  if (k .gt. n2) go to 11
  m  = ivarb(k + 2)
  m1 = ivarb(k + 3)
  m2 = ivarb(k + 1)
  if (m2 .gt. 0) b = xtcs(kxtcs + m1)
  if (m2 .lt. 0) b = parsup(kprsup + m1)
  if (m .gt. 10) go to 30
  if (m .le. 0 .or. m .gt. 5)  go to 100
  select case (m)
  case (1)
     go to 101

  case (2)
     go to 102

  case (3)
     go to 103

  case (4)
     go to 104

  case (5)
     go to 105
  end select
30 if (m .gt. 20) go to 40
  m = m - 10
  select case (m)
  case (1)
     go to 111
  case (2)
     go to 112
  case (3)
     go to 113
  case (4)
     go to 114
  case (5)
     go to 115
  case (6)
     go to 116
  case (7)
     go to 117
  case (8)
     go to 118
  case (9)
     go to 119
  case (10)
     go to 120
  end select
40 if (m .gt. 30) go to 50
  m = m - 20
  select case (m)
  case (1)
     go to 121
  case (2)
     go to 122
  case (3)
     go to 123
  case (4)
     go to 124
  case (5)
     go to 125
  case (6)
     go to 126
  case (7)
     go to 127
  case (8)
     go to 128
  case (9)
     go to 129
  case (10)
     go to 130
  end select
50 if (m .gt. 40) go to 60
  m = m - 30
  select case (m)
  case (1)
     go to 131
  case (2)
     go to 132
  case (3)
     go to 133
  case (4)
     go to 134
  end select
60 continue
101 continue
  if (a .ge. 1.0 .and. b .ge. 1.0) go to 99
  go to 98
102 continue
  if (a .ge. 1.0 .or. b .ge. 1.0) go to 99
  go to 98
103 continue
  if (b .lt. 1.0) go to 99
  go to 98
104 continue
  if (a .ge. 1.0 .and. b .ge. 1.0) go to 98
  go to 99
105 continue
  if (a .lt. 1.0 .and. b .lt. 1.0) go to 99
  go to 98
99 a = 1.0
  go to 20
98 a = 0.0
  go to 20
111 b = sinz (b)
  go to 100
112 b = cosz (b)
  go to 100
113 b = tanz (b)
  go to 100
114 b = cotanz (b)
  go to 100
115 b = sinhz (b)
  go to 100
116 b = coshz (b)
  go to 100
117 b = tanhz (b)
  go to 100
118 b = asinz (b)
  go to 100
119 b = acosz (b)
  go to 100
120 b = atanz (b)
  go to 100
121 b = expz (b)
  go to 100
122 b = alogz (b)
  go to 100
123 b = alog1z (b)
  go to 100
124 b = sqrtz (b)
  go to 100
125 b = absz (b)
  go to 100
126 b = aintz (b)
  go to 100
127 b = - b
  go to 100
128 d7 = 1.0
  d8 = b
  !assign 7128 to idiv
  idiv = 7128
  go to 500
7128 b = div
  go to 100
129 b = b * twopi / 360.0
  go to 100
130 b = b / twopi * 360.0
  go to 100
131 n5 = int(b)
  n6 = n5 / 6
  if (n5 .lt. 0) n5 = n5 + 6 * (n6 + 1)
  n7 = n5 - 6 * n6
  if (n7 .eq. 0) n7 = 6
  b = n7
  go to 100
132 d7 = b
  b = 1.0
  if (d7 .lt. 0.0) b = -1.0
  go to 100
133 d7 = b
  b = 1.0
  if (d7 .gt. 10.0 * flzero) b = 0.0
  go to 100
134 d2 = 0.0
  b = randnm (d2)
100 continue
  if (nop .gt. 0) go to 6113
  m = iabs (m2)
  select case (m)
  case (1)
     go to 201

  case (2)
     go to 202

  case (3)
     go to 203

  case (4)
     go to 204

  case (5)
     go to 205
  end select
201 a = a + b
  go to 20
202 a = a - b
  go to 20
203 a = a * b
  go to 20
204 d7 = a
  d8 = b
  !assign 7204 to idiv
  idiv = 7204
  go to 500
7204 a = div
  go to 20
205 a = a ** b
  go to 20
500 if (d7 .eq. 0.0) go to 510
  if (d8 .eq. 0.0) go to 520
  n = int (alog1z (absz (d7)) - alog1z (absz (d8)))
  if (n .lt. -iuty(kiuty + 11)) go to 510
  if (n .gt. iuty(kiuty + 11)) go to 520
  div = d7 / d8
  go to 530
520 div = fltinf
  if (d7 .lt. 0.0) div = - div
  if (d8 .lt. 0.0) div = - div
  go to 530
510 div = 0.0
  !530 go to idiv, (7128, 7204, 6120)
530 select case(idiv)
  case (7128)
     go to 7128

  case (7204)
     go to 7204

  case (6120)
     go to 6120
  end select
  !     ------  free-format fortran expression  ------
5000 n2 = -n2
  !     :: load iop( nop)  and  arg( nop)
  k = 0
  do j = n1, n2, 3
     k = k + 1
     iop(k) = ivarb(j + 2)
     arg(k) = 0.0
     i1 = ivarb(j + 1) + 2
     i2 = ivarb(j + 3)
     select case (i1)
     case (1)
        go to 5010

     case (2)
        go to 5025

     case (3)
        go to 5015

     case (4)
        go to 5020
     end select
     !     :: numerical argument
5010 ndx4 = kprsup + i2
     arg(k) = parsup(ndx4)
     go to 5025
     !     :: tacs variable
5015 ndx4 = kxtcs + i2
     arg(k) = xtcs(ndx4)
     go to 5025
     !     :: fortran tacs function
5020 arg(k) = i2
  end do
5025 continue
  !     :::  calculate value of fortran expression  :::
  zfl = 10.0 * flzero
  jfl = 1
  ifl(1) = k
  nop = k
6010 idn(jfl) =  0
  idn(jfl + 1) = 0
6015 acc(jfl) =  0.0
  amx(jfl) = 1.0
6020 if (jfl .eq. 0) go to 6200
  i1 = ifl(jfl)
  if (i1 .eq. 0) go to 6111
  k1 = iop(i1)
  if (k1 .eq. 0) go to 6028
  if (k1 .gt. 7) go to 6025
  select case (k1)
  case (1)
     go to 6110

  case (2)
     go to 6121

  case (3)
     go to 6130

  case (4)
     go to 6121

  case (5)
     go to 6150

  case (6)
     go to 6121

  case (7)
     go to 6170
  end select
6025 if (k1 .lt. 14) go to 6180
  k1 = k1 - 13
  select case (k1)
  case (1)
     go to 6114

  case (2)
     go to 6115

  case (3)
     go to 6116

  case (4)
     go to 6117

  case (5)
     go to 6118
  end select
6026 amx(jfl) = 1.0
6027 iop(i1) = 0
6028 ifl(jfl) = ifl(jfl) - 1
  go to 6020
  !     ::  k1 = (  ::
6110 if (idn(jfl) .ne. -1) go to 6112
6111 jfl = jfl - 1
  go to 6020
6112 b = acc( jfl)
  m = int(arg(i1 - 1))
  if (m .ne. 0) go to 30
6113 arg(i1 - 1) = b
  jfl = jfl - 1
  iop(i1) = 0
  go to 6020
  !     ::  k1 = *  ::
6114 amx(jfl) = amx(jfl) * arg( i1)
  go to 6027
  !     ::  k1 = /  ::
6115 d7 = amx(jfl)
  d8 = arg(i1)
  !assign 6120 to idiv
  idiv = 6120
  go to 500
6120 amx(jfl) = div
  go to 6027
  !     ::  k1 = **  ::
6116 i2 = i1
6776 i2 = i2 - 1
  if (iop(i2) .eq. 0) go to 6776
  if (iop(i2) .eq. 7) go to 6119
  arg(i2) = arg(i2) ** arg(i1)
  go to 6027
6119 i1 = i2
  go to 6170
  !     ::  k1 = -  ::
6118 arg(i1) = -arg(i1)
  !     ::  k1 = +  ::
6117 acc(jfl) = acc(jfl) + arg(i1) * amx(jfl)
  go to 6026
  !     ::  k1 = not  ::
6121 d7 = arg(i1)
  arg(i1) = 1.0
  if (d7 .gt. zfl) arg( i1) = 0.0
  if (k1 .eq. 6) go to 6150
  !     ::  k1 = or  ::
6130 d7 = arg(i1)
  arg(i1) = 0.0
  if (d7 .gt. zfl .and. amx(jfl) .gt. zfl) arg(i1) = 1.0
  d7 = acc(jfl)
  acc(jfl) = 0.0
  if (d7 .gt. zfl .or. arg(i1) .gt. zfl) acc(jfl) = 1.0
  go to 6026
  !     ::  k1 = and  ::
6150 d7 = amx(jfl)
  amx(jfl) = 0.0
  if (d7 .gt. zfl .and. arg(i1) .gt. zfl) amx(jfl) = 1.0
  go to 6027
  !     :: k1 = )
6170 jfl = jfl + 1
  ifl(jfl) = i1 - 1
  iop(i1) = 0
  go to 6010
  !     :: k1 = .nn.
6180 if (idn(jfl + 1) .eq. -1) go to 6185
  jfl = jfl + 1
  ifl(jfl) = i1 - 1
  idn(jfl) = -1
  go to 6015
6185 iop(i1) = 0
  idn(jfl + 1) = 0
  d7 = acc(jfl)
  d8 = acc(jfl + 1)
  acc(jfl) = 0.0
  i2 = k1 - 7
  select case (i2)
  case (1)
     go to 6188

  case (2)
     go to 6189

  case (3)
     go to 6190

  case (4)
     go to 6191

  case (5)
     go to 6192

  case (6)
     go to 6193
  end select
6188 if (d8 .ne. d7) go to 6195
  go to 6020
6189 if (d8 .eq. d7) go to 6195
  go to 6020
6190 if (d8 .lt. d7) go to 6195
  go to 6020
6191 if (d8 .le. d7) go to 6195
  go to 6020
6192 if (d8 .ge. d7) go to 6195
  go to 6020
6193 if (d8 .gt. d7) go to 6195
  go to 6020
6195 acc(jfl) = 1.0
  go to 6020
  !     :: exit fortran expression
6200 a = acc( 1)
  go to 11
  !     ------  devices  start  here  ------
600 n1 = - n1
  nn = ivarb(n1)
  if (n2 .eq. 60 .or. n2 .eq. 61) go to 602
  if (n2 .eq. 63 .or. n2 .eq. 67) go to 602
  j = ivarb(n1 + 1)
  k = ivarb(n1 + 2)
  b = 0.0
  do n= j, k
     m = kxtcs + ksus(kalksu + n)
     b = b + xtcs(m) * ksus(kksus + n)
  end do
602 if (n2 .gt. 67) go to 10
  n2 = n2 - 49
  select case (n2)
  case (1)
     go to 650

  case (2, 3)
     go to 651

  case (4)
     go to 653

  case (5)
     go to 654

  case (6)
     go to 655

  case (7)
     go to 656

  case (8)
     go to 657

  case (9)
     go to 658

  case (10)
     go to 659

  case (11)
     go to 660

  case (12)
     go to 661

  case (13)
     go to 662

  case (14)
     go to 663

  case (15, 16)
     go to 664

  case (17)
     go to 666

  case (18)
     go to 667
  end select
  !     ---  frequency sensors  ---
650 n = 1
  if (b .lt. 0.0) n = -1
  m  = ivarb(n1 + 3)
  d9 = parsup(nn + 1)
  d = parsup(nn + 2)
  d7 = parsup(nn)
  a = d7
  if (m .ne. 0) go to 6501
  parsup(nn + 1) = -1.0
  go to 6507
6501 if (n  .eq. m) go to 6508
  if (d .eq. 0.0) go to 6507
  if (b .ne. 0.0) go to 6502
  d8 = t
  go to 6503
6502 d8 = t - deltat / (1.0 - d / b)
6503 if (d9 .lt. 0.0) go to 6506
  g = onehaf / (d8 - d9)
  if (d7 .eq. 0.0) go to 6505
  h = g / d7 - 1.0
  if (h .lt. parsup(nn + 3)) go to 6505
  if (iuty(kiuty + 3) .eq. 0) go to 6507
  iuty(kiuty + 3) = iuty(kiuty + 3) - 1
  ndx1 = ilntab(kspvar + i)
  write (lunit6, 6504) texvec(ndx1), t, d7, g
6504 format (' ', 5x, 'Warning. ---- Frequency sensor ', a6, ' has zero crossing at ', e15.6, ' sec. But new frequency ', /, 21x, 'of', e13.4, ' Hz differs by over fifty percent from the old frequency of ', e13.4, ' Hz. Reject it.')
  go to 6507
6505 parsup(nn) = g
  a = g
6506 parsup(nn + 1) = d8
6507 ivarb(n1 + 3) = n
6508 ndx1 = kdev2 - n1
  parsup(nn + 2) = b
  go to 11
  !     ---  relays and level-triggers  ---
651 if (parsup(nn) .ne. 0.0) b = b * parsup(nn)
  m = ivarb(n1 + 3)
  n = ivarb(n1 + 4)
  d9 = parsup(nn + 1)
  d8 = absz (parsup(nn + 2))
  if (m .ne. 0) d9 = d9 + xtcs(kxtcs + m)
  d = 0.0
  if (n .ne. 0) d = xtcs(kxtcs + n)
  if (n2 .eq. 2) d = absz (d)
  if (d .ge. d9 .and. d8 .gt. 1.5) a = b
  if (d .lt. d9 .and. d8 .lt. 1.5) a = b
  go to 11
  !     ---  variable transport delay  ---
653 n5 = int(parsup(nn))
  n6 = int(parsup(nn + 2))
  n7 = int(ivarb(n1 + 4))
  d7 = int(parsup(nn + 1))
  j = ivarb(n1 + 3)
  ndx6 = kxtcs + j
  if ( j .ne. 0 )  d7 = d7 + xtcs( ndx6)
  d7 = d7 / deltat
65310 d8 = d7 + flzero * 10.
  d9 = d7 - flzero * 10.
  if ( d8 .ge. 0.0 )  go to 65320
  if ( iuty(kiuty+3) .eq. 0 )  go to 65313
  iuty(kiuty+3) = iuty(kiuty+3) - 1
  ndx6 = ilntab( kspvar + i )
  write (lunit6, 65316)  texvec( ndx6), t
65316 format (5x, 'Warning.  ----  value of delay became negative for ', "'", a6, "'", ' at time =', e14.6, ' but lower limit nalue = 0.0 .', /, 21x, 'This message will not be repeated.')
65313 d7 = 0.0
  go to 65310
65320 if ( d9 .gt. 0.0 )  go to 65330
  a = b
  go to 11
65330 m1 = 0
  j = 0
65350 j = j + 1
  if (d9 .gt. j) go to 65350
  if (j .le. d8) m1 = 1
  if (j .le. n6) go to 65360
  if (iuty(kiuty+2) .eq. 0) go to 65353
  iuty(kiuty + 3) = iuty(kiuty + 3) - 1
  ndx6 = ilntab(kspvar + i)
  d4 = deltat * n6
  write (unit = lunit6, fmt = 65356) d4, texvec( ndx6), t
65356 format (5x, 'Warning.  ----  value of delay exceeded max. delay value of ', '"', e14.6, '"', ' for ', '"', a6, '"', ' at time =', e14.6, ' .', / 21x, 'This message will not be repeated.')
65353 j = n6
  m1 = 1
65360 n4 = n7 - j
  if (n4 .lt. n5) n4 = n4 + n6
  ndx6 = kprsup + n4
  a = parsup(ndx6)
  if (m1 .eq. 1) go to 11
  n3 = n4 + 1
  if (n3 .ge. n5 + n6) n3 = n3 - n6
  d5 = b
  ndx6 = kprsup + n3
  if (j .gt. 1) d5 = parsup(ndx6)
  a = a - (j - d7) * (a - d5)
  go to 11
  !     ---  pulse variable transport delay  ---
654 m = ivarb(n1 + 3)
  d9 = parsup(nn + 1)
  ndx1 = kxtcs  +  m
  if (m .ne. 0) d9 = d9 + xtcs(ndx1)
  d = parsup(nn)
  d7 = parsup(nn + 2)
  if (d7 .eq. -9999.) go to 65400
  ndx1 = ilntab(kspvar + i)
  if (b .le. 0.0d0) go to 65402
  if (iuty(kiuty + 3) .eq. 0) go to 65402
  iuty(kiuty + 3) = iuty(kiuty + 3) - 1
  write (unit = lunit6, fmt = 65401) texvec(ndx1), d9, t
65401 format (5x, 'Warning. ---- The pulse frequency at the pulse transport delay ', a6, ' is too fast for the present ', /, 21x, ' delay of ', e13.4, ' sec at simulation time ', e13.4, ' sec. Use device type 53 instead of type 54.', /, 21x, '******** the answer may be wrong later ********')
  go to 65402
65400 if (b .gt. 0.0d0 .and. d .eq. -9999.0d0) parsup(nn) = t
  if (b .le. 0.0d0 .and. d .ne. -9999.0d0) parsup(nn + 2) = t
65402 if (t .lt. d + d9 - 10.0d0 * flzero .or. d .eq. -9999.0d0) go to 11
  if (t .ge. d7 + d9 - 10.0d0 * flzero .and. d7 .ne. -9999.0d0) go to 65403
  a = 1.0d0
  go to 11
65403 parsup(nn) = -9999.0d0
  parsup(nn+2) = -9999.0d0
  go to 11
  !     ---  digitizer  ---
655 m = ivarb(n1 + 3)
  n = ivarb(n1 + 4)
  if (parsup(nn) .ne. 0.0d0) b = b * parsup(nn)
  ndx1 = kprsup +  m
  a = parsup(ndx1)
  if (m .eq. n) go to 11
  j = m + 1
  do k = j, n
     m = n - k + j
     ndx1 = kprsup +  m
     if (b .ge. parsup(ndx1)) go to 65501
  end do
  go to 11
65501 ndx1 = kprsup +  m
  a = parsup(ndx1)
  go to 11
  !     ---  point-by-point non-linearity  ---
656 m = ivarb(n1 + 3)
  n = ivarb(n1 + 4)
  if (parsup(nn) .ne. 0.0d0) b = b * parsup(nn)
  ndx1 = kprsup +  m + 1
  a = parsup(ndx1)
  if (n .le. m + 1) go to 11
  ndx1 = kprsup + m
  if (b .le. parsup(ndx1)) go to 11
  j = m + 2
  do k = j, n, 2
     ndx1 = kprsup + k
     if (b .le. parsup(ndx1)) go to 65601
  end do
  ndx1 = kprsup +  n
  a = parsup(ndx1)
  go to 11
65601 ndx1 = kprsup + k + 1
  ndx2 = kprsup + k - 1
  ndx3 = kprsup + k
  ndx4 = kprsup + k - 2
  d9 = (parsup(ndx1) - parsup(ndx2)) / (parsup(ndx3) - parsup(ndx4))
  a = parsup(ndx2) + d9 * (b - parsup(ndx4))
  go to 11
  !     ---  time - sequenced  switch  ---
657 m = ivarb(n1 + 3)
  n = ivarb(n1 + 4)
  n3 = int(parsup(nn + 1))
  n4 = int(parsup(nn + 2))
  if (n4 .eq. n) go to 65701
  if (n4 .eq. 0) n4 = m - 1
  j = n4 + 1
  ndx1 = kprsup + j
  if (t .lt. parsup(ndx1)) go to 65701
  parsup(nn + 2) = j
  n3 = n3 + 1
  if (n3 .eq. 2) n3 = 0
  parsup(nn + 1) = n3
65701 if (n3 .eq. 0) go to 11
  a = b
  go to 11
  !     ---  controlled integrator and counter  ---
658 if (ivarb(n1 + 3) .ne. -9999) go to 4658
  ndx6 = nnn + i
  a = xtcs(ndx6) + b
  go to 11
4658 n5 = ivarb(n1 + 3)
  if (n5 .eq. 0) go to 4721
  ndx6 = kxtcs + n5
  if (xtcs(ndx6) .gt. 0.0) go to 4721
  a = 0.0
  parsup(nn) = 0.0
  n6 = ivarb(n1 + 4)
  if (n6 .eq. 0) go to 11
  ndx6 = kxtcs + n6
  a = xtcs(ndx6)
  parsup(nn) = (parsup(nn + 1) - parsup(nn + 2)) / 2.0 * a
  go to 11
4721 a = (b + parsup(nn)) / parsup(nn + 1)
  parsup(nn) = b - parsup(nn + 2) * a
  go to 11
  !     ---  simple  derivative  ---
659 a = (b - parsup(nn + 1)) * parsup(nn)
  parsup(nn + 1) = b
  go to 11
  !     ---  input  if - block  ---
660 d7 = parsup(nn)
  n3 = ivarb(n1 + 3)
  n4 = ivarb(n1 + 4)
  ndx1 = kxtcs + n4
  if (n4 .ne. 0) d7 = d7 + xtcs(ndx1)
  ndx1 = kxtcs + n3
  d7 = xtcs(ndx1) - d7
  n = 2
  if (d7 .gt. -flzero) n = 1
  if (d7 .gt. +flzero) n = 0
  j = ivarb(n1 + 1) + n
  ndx2 = kalksu + j
  ndx3 = kksus + j
  ndx4 = kxtcs + ksus(ndx2)
  a = xtcs(ndx4) * ksus(ndx3)
  go to 11
  !     ---  input  signal  selector  ---
661 ndx3 = kxtcs + ivarb(n1 + 3)
  ndx4 = kxtcs + ivarb(n1 + 4)
  d1 = xtcs(ndx4)
  a = parsup(nn + 1)
  if (d1 .lt. 0.5d0) go to 11
  a = parsup(nn + 2)
  if (d1 .ge. 6.5d0) go to 11
  a = 0.0
  if (d1 .lt. 5.5) go to 66110
  if (ndx3 .eq. kxtcs) go to 11
  a = xtcs(ndx3)
  go to 11
66110 j = int(d1 - onehaf)
  j = ivarb(n1 + 2) - j
  ndx1 = kalksu + j
  m = ksus(ndx1)
  if (m .eq. 0) go to 11
  ndx1 = kxtcs + m
  ndx2 = kksus + j
  a = xtcs(ndx1) * ksus(ndx2)
  go to 11
  !     ---  track  and  sample  ---
662 a = parsup(nn + 2)
  n = ivarb(n1 + 3)
  if (n .eq. 0)  go to 66210
  ndx2 = kxtcs + n
  if (xtcs(ndx2) .gt. flzero) a = b
66210 n = ivarb(n1 + 4)
  m = 0
  if (n .eq. 0) go to 66220
  ndx3 = kxtcs + n
  if (xtcs(ndx3) .gt. flzero) m = 1
66220 if (parsup(nn) .eq. 1.0d0 .or. m .eq. 0) go to 66230
  a = b
  parsup(nn) = 1.0d0
  go to 66240
66230 if (parsup(nn) .eq. 1.0d0 .and. m .eq. 0) parsup(nn) = 0.0d0
66240 parsup(nn + 2) = a
  go to 11
  !     ---  instantaneous  min/max  ---
663 ndx1 = kalksu + ivarb(n1 + 1)
  k = 2
66310 if (ksus(ndx1) .gt. 0) go to 66320
  ndx1 = ndx1 + 1
  k = k + 1
  go to 66310
66320 ndx4 = kxtcs + ksus(ndx1)
  ndx2 = ndx1 - lstat(63)
  d11 = xtcs(ndx4) * ksus(ndx2)
  d10 = d11
  if (k .gt. 5) go to 66330
  ndx3 = ndx1
  do j = k, 5
     ndx3 = ndx3 + 1
     ndx2 = ndx2 + 1
     n = ksus(ndx3)
     if (n .eq. 0) go to 66340
     ndx6 = kxtcs + n
     d4 = xtcs(ndx6) * ksus(ndx2)
     if (d4 .lt. d10) d10 = d4
     if (d4 .gt. d11) d11 = d4
     if (iprsup .ge. 1) write (unit = lunit6, fmt = 7234) k, j, ndx2, ndx3, ndx6, ksus(ndx2), xtcs(ndx6)
7234 format (' Next input; k, j, ndx2, ndx3, ndx6, ksus(ndx2), xtcs(ndx6) =',  6i8, e13.3)
  end do
66340 continue
66330 a = d11
  if (parsup(nn + 1) .ge. 0.0) go to 11
  a = d10
  go to 11
  !     --- min/max tracking, controlled accumulator or couhter ---
664 ndx4 = kxtcs + ivarb(n1 + 3)
  if (ndx4 .eq. kxtcs) go to 6641
  if (xtcs(ndx4) .le. flzero) go to 6641
  a = parsup(nn + 2)
  go to 6643
6641 ndx5 = kxtcs + ivarb(n1 + 4)
  if (ndx5 .eq. kxtcs) go to 6642
  if (xtcs( ndx5) .le. flzero) go to 6642
  a = parsup(nn)
  go to 11
6642 if (n2 .eq. 16) go to 665
  a = parsup(nn)
  rdev1 = parsup( nn + 1 )
  if (rdev1 .eq. -1.0 .and. b .lt. a) a = b
  if (rdev1 .eq. +1.0 .and. b .gt. a) a = b
  go to 6643
665 a = parsup(nn) + b
6643 parsup(nn) = a
  go to 11
666 ivarb(n1 + 4) = ivarb(n1 + 4) + 1
  k = ivarb(n1 + 3)
  if (ivarb(n1 + 4) .gt. k) ivarb(n1 + 4) = 1
  ndx1 = nn + ivarb(n1 + 4)
  parsup(ndx1) = b * b
  do ji = 1, k
     a = a + parsup(nn + ji)
  end do
  a = sqrtz (a * parsup(nn))
  go to 11
667 j = ivarb(n1 + 1)
  k = ivarb(n1 + 2)
  b = 0.0
  do mj = j, k
     n = kksus + mj
     if (ksus(n) .eq. 9) go to 1166
     m = kalksu + mj
     bb = xtcs(kxtcs + ksus(m)) * ksus(n)
     nj = mj
1144 nj = nj - 1
     n = n - 1
     m = m - 1
     if (nj .lt. j) go to 1155
     if (ksus(n) .ne. 9) go to 1155
     bb = bb * xtcs(kxtcs + ksus(m))
     go to 1144
1155 b = b + bb
  end do
1166 continue
  a = b * parsup(nn + 3)
  aa = a * parsup(nn)
  if (aa .ge. parsup(nn + 1)) go to 6677
  a = parsup(nn + 1) / parsup(nn)
  go to 11
6677 if (aa .le. parsup(nn + 2)) go to 11
  a = parsup(nn + 2) / parsup(nn)
11 ndx1 = nnn + i
  xtcs(ndx1) = a
10 i = insup(kinsup + i)
  if (i .gt. 0) go to 1234
  return
end subroutine csup

!
! end of file main10.f90
!
