!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file veccom.f90
!

module veccom
  use blkcom
  use deck29
  implicit none
  integer(4) :: kntvec, kofvec(20)

#ifdef SAVE_TO_DISK
  interface vecsav
     module procedure vecrxx, vecixx
  end interface vecsav
#else
  interface vecsav
     module procedure vecrsv, vecisv
  end  interface vecsav
#endif

contains

#ifdef SAVE_TO_DISK
  !
  ! subroutine vecrxx.
  !

  subroutine vecrxx (array, n13, n2)
    implicit none
    !     Universal (non-virtual) form of module for binary i/o.  If
    !     extracted from UTPF for use, convert name "VECRXX" to "VECRSV"
    integer(4), intent(in) :: n2
    integer(4), intent(out) :: n13
    real(8), intent(out) :: array(2)
    integer(4) :: j, k, n6, n14
    !
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 1575) n13, n2
1575 format (' Begin "vecrsv".  n13, n2 =', 2i8)
    if (n2 .ne. 0) go to 1638
    !     zero n2 means that we want to position tape for next read:
    if (n13 .ge. 0) go to 1592
    n6 = -n13
    do j = 1, n6
       backspace lunt13
    end do
    go to 9000
1592 rewind lunt13
    if (n13 .eq. 0) go to 1612
    do j = 1, n13
       read (unit = lunt13) n14
    end do
1612 if (iprsup .ge. 1) write (unit = 6, fmt = 1613) n13
1613 format (' Position magnetic tape.  n13 =', i4)
    n13 = 3
    go to 9000
1638 if (n2 .eq. 1) go to 1671
    !     begin code to restore  (array(k), k=1, n13)  from tape:
    read (unit = lunt13) (array(k), k = 1, n13)
    go to 9000
    !     begin code to dump  (array(k), k=1, n13)  onto tape:
1671 write (unit = lunt13) (array(k), k = 1, n13)
9000 if (iprsup .ge. 1) write (unit = lunit6, fmt = 9007) array(1), array(2), array(n13)
9007 format (' Exit "vecrsv".  array(1; 2; n13) =', 3e15.6)
    return
  end subroutine vecrxx

  !
  ! subroutine vecixx.
  !

  subroutine vecixx (karr, n13, n2)
    implicit none
    !     Universal (non-virtual) form of module for binary i/o.  If
    !     extracted from UTPF for use, convert name "VECIXX" to "VECISV"
    integer(4), intent(out) :: karr(2)
    integer(4), intent(in) :: n2, n13
    integer(4) :: k
    !
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 1423) n13, n2
1423 format (' Begin "vecisv".  n13, n2 =', 2i8)
    if (n2 .eq. 1) go to 1471
    !     begin code to restore  (karr(k), k=1, n13)  from tape:
    read (unit = lunt13) (karr(k), k = 1, n13)
    go to 9000
    !     begin code to dump  (karr(k), k=1, n13)  onto tape:
1471 write (unit = lunt13) (karr(k), k = 1, n13)
9000 if (iprsup .ge. 1) write (unit = lunit6, fmt = 9007) karr(1), karr(2), karr(n13)
9007 format (' Exit "vecisv".  karr(1;2;n13) =', 3i10)
    return
  end subroutine vecixx
#else
  !
  ! subroutine vecrsv.
  !

  subroutine vecrsv (farray, n13, n2)
    implicit none
    !     Module for vector dumping/restoring of "OVER6", "OVER8", etc.
    !     This is universal for virtual computers which chose to
    !     use /C29B01/ space for this, as well as all of "LABCOM".
    !     Also needed are uncounted Hollerith.  Parallel to "VECISV".
    real(8), intent(inout), target :: farray(:)
    integer(4), intent(in) :: n2, n13
    integer(4) :: k, n4, n5, n14
    integer(4), pointer :: temp(:)
    !  common /veccom/ kntvec, kofvec(20)
    !
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 1623) n13, n2, kntvec
1623 format (' Begin "vecrsv".  n13, n2 =',  2i8, '     kntvec =', i8)
    if (n2 .ne. 0) go to 1638
    if (n13 .ge. 0) kntvec = n13
    if (n13 .lt. 0) kntvec = kntvec + n13
    if (iprsup .ge. 2) write (unit = lunit6, fmt = 1629) n13
1629 format (' Initialization of kntvec.  n13 =', i10)
    go to 9000
1638 if (n2 .eq. 1) go to 1671
    !     begin code to restore  (array(k), k=1, n13)  from tank:
    kntvec = kntvec + 1
    n4 = kofvec(kntvec)
    if (iprsup .ge. 2) write (unit = lunit6, fmt = 1640) kntvec, n4
1640 format (' Ready to restore.  kntvec, n4 =', 2i10)
    if (n13 .le. 0) go to 9000
    temp => karray(n4 :)
    if (associated (temp)) then
       do k = 1, n13
          farray(k) = temp(k)
       end do
       nullify (temp)
    else
       write (unit = lunit6, fmt = 1645)
1645   format ('Could not associate a temp pointer to karray.  Stop.')
       call stoptp
    end if
    go to 9000
    !     begin code to dump  (array(k), k=1, n13)  into tank:
1671 if (kntvec .gt. 0) go to 1674
    n14 = nbyte(3) / nbyte(4)                                 ! relative lengths  real/integer
    kofvec(1) = (ltlabl + 1) / n14 + 51                       ! begin storage
    if (iprsup .ge. 2) write (unit = lunit6, fmt = 1673) kofvec(1)
1673 format (' Initialize kofvec(1) =', i10)
1674 kntvec = kntvec + 1
    n4 = kofvec(kntvec)
    if (iprsup .ge. 2) write (unit = lunit6, fmt = 1675) kntvec, n4
1675 format (' Ready to dump.  kntvec, n4 =', 2i10)
    if (n13 .le. 0) go to 1683
    temp => karray(n4 :)
    if (associated (temp)) then
       do k = 1, n13
          temp(k) = farray(k)
          n4 = n4 + 1
       end do
       nullify (temp)
    else
       write (unit = lunit6, fmt = 1680)
1680   format ('Could not associate a temp pointer to karray.  Stop.')
       call stoptp
    end if
    ! if /veccom/ storage exceeded,
1683 if (kntvec .ge. 20) call stoptp                        ! installation-dependent program stop card
    kofvec(kntvec + 1) = n4
    if (iprsup .ge. 2) write (unit = lunit6, fmt = 1687) kofvec(kntvec + 1)
1687 format (' Define  kofvec(kntvec + 1) =', i10)
9000 if (iprsup .ge. 1) write (unit = lunit6, fmt = 9007) farray(1), farray(2), farray(n13)
9007 format (' Exit "vecrsv".  farray(1; 2; n13) =', 3e15.6)
    if (iprsup .ge. 2) write (unit = lunit6, fmt = 9011) kofvec
9011 format (' kofvec =', 20i6)
    return
  end subroutine vecrsv

  !
  ! subroutine vecisv.
  !

  subroutine vecisv (iarray, n13, n2)
    implicit none
    !     Module for vector dumping/restoring of "OVER6", "OVER8", etc.
    !     This is universal for virtual computers which chose to
    !     use /C29B01/ space for this, as well as all of "LABCOM".
    !     also needed are uncounted Hollerith.  Parallel to "VECISV".
    integer(4), intent(inout), target :: iarray(:)
    integer(4), intent(in) :: n13, n2
    !     block /VECCOM/ is shared with "VECRSV" (see for more info)
    integer(4) :: k, n4, n14
    integer(4), pointer :: temp(:)
    !  common /veccom/ kntvec, kofvec(20)
    !
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 1423) n13, n2
1423 format (' Begin "vecisv".  n13, n2 =',  2i8)
    if (n2 .eq. 1) go to 1471
    !     begin code to restore  (karr(k), k=1, n13)  from tank:
    kntvec = kntvec + 1
    n4 = kofvec(kntvec)
    if (iprsup .ge. 2) write (unit = lunit6, fmt = 1428) kntvec, n4
1428 format (' Ready to restore.  kntvec, n4 =', 2i10)
    temp => karray(n4 :)
    if (associated (temp)) then
       do k = 1, n13
          iarray(k) = temp(k)
       end do
       nullify (temp)
    else
       write (unit = lunit6, fmt = 1430)
1430   format ('Could not associate a temp pointer to karray.  Stop.')
       call stoptp
    end if
    go to 9000
    !     begin code to dump  (karr(k), k=1, n13)  into tank:
1471 if (kntvec .gt. 0) go to 1474
    n14 = nbyte(3) / nbyte(4)                                 ! relative lengths  real/integer
    kofvec(1) = (ltlabl + 1) / n14 + 51
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 1473) kofvec(1)
1473 format (' Initialize kofvec(1) =', i10)
1474 kntvec = kntvec + 1
    n4 = kofvec(kntvec)
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 1475) kntvec, n4
1475 format (' Ready to dump.  kntvec, n4 =', 2i10)
    kofvec(kntvec) = n4
    temp => karray(n4 :)
    if (associated (temp)) then
       do k = 1, n13
          temp(k) = iarray(k)
          n4 = n4 + 1
       end do
       nullify (temp)
    else
       write (unit = lunit6, fmt = 1480)
1480   format ('Could not associate a temp pointer to karray.  Stop.')
       call stoptp
    end if
    if (kntvec .ge. 20) call stoptp
    kofvec(kntvec + 1) = n4
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 1482) kofvec(kntvec + 1)
1482 format (' Define kofvec(kntvec+1) =', i10)
9000 if (iprsup .ge. 1) write (unit = lunit6, fmt = 9007) iarray(1), iarray(2), iarray(n13)
9007 format (' Exit "vecisv".  iarray(1; 2; n13) =', 3i10)
    if (iprsup .ge. 2) write (unit = lunit6, fmt = 9011) kofvec
9011 format (' kofvec =', 20i6)
    return
  end subroutine vecisv
#endif
  
end module veccom


