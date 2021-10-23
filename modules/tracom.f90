!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file trasce.f90
!

!     This function provides for all real library functions of
!     a single real argument.   All translations will make a
!     substitution.
!  include 'blkcom.ftn'

module tracom

contains
  real(8) function absz (x)
    implicit none
    real(8), intent(in) :: x
    absz = dabs (x)
  end function absz

  real(8) function acosz (x)
    real(8), intent(in) :: x
    acosz = dacos (x)
  end function acosz

  real(8) function aintz (x)
    implicit none
    real(8), intent(in) :: x
    aintz = dint (x)
    return
  end function aintz

  real(8) function alogz (x)
    implicit none
    real(8), intent(in) :: x
    alogz = dlog (x)
  end function alogz

  real(8) function alog1z (x)
    implicit none
    real(8), intent(in) :: x
    alog1z = dlog10 (x)
  end function alog1z

  real(8) function asinz (x)
    implicit none
    real(8), intent(in) :: x
    asinz = dasin (x)
  end function asinz

  real(8) function atanz (x)
    implicit none
    real(8), intent(in) :: x
    atanz = datan (x)
  end function atanz

  real(8) function cosz (x)
    implicit none
    real(8), intent(in) :: x
    cosz = dcos (x)
  end function cosz

  real(8) function coshz (x)
    implicit none
    real(8), intent(in) :: x
    coshz = dcosh (x)
  end function coshz

  real(8) function cotanz (x)
    use blkcom
    implicit none
    real(8), intent(in) :: x
    real(8) :: y
    !
    y = dsin (x)
    if (dabs (y) * fltinf .gt. 1.0) go to 4783
    write (unit = lunit6, fmt = 4761) x
4761 format (/, " Stop.   Too small argument at  'cotanz'  within 'rfunl1' .", e15.5)
    call stoptp                                               ! installation-dependent program stop card
4783 cotanz = dcos (x) / y
    return
  end function cotanz

  real(8) function expz (x)
    implicit none
    real(8), intent(in) :: x
    if (x .ge. -87) go to 1488
    expz = 0.0
    return
1488 expz = dexp (x)
  end function expz

  real(8) function sinz (x)
    implicit none
    real(8), intent(in) :: x
    sinz = dsin (x)
  end function sinz

  real(8) function sinhz (x)
    implicit none
    real(8), intent(in) :: x
    sinhz = dsinh (x)
  end function sinhz

  real(8) function sqrtz (x)
    implicit none
    real(8), intent(in) :: x
    sqrtz = dsqrt (x)
  end function sqrtz

  real(8) function tanz (x)
    implicit none
    real(8), intent(in) :: x
    tanz = dtan (x)
  end function tanz

  real(8) function tanhz (x)
    implicit none
    real(8), intent(in) :: x
    tanhz = dtanh (x)
  end function tanhz

  double precision function intz (d1)
    implicit none
    real(8), intent(in) :: d1
    intz = dint (d1)
  end function intz

  complex(16) function cexpz (x)
    implicit none
    complex(16), intent(in) :: x
    !  cexpz = cdexp (x)
    cexpz = exp (x)
  end function cexpz

  complex(16) function csqrtz (x)
    implicit none
    complex(16), intent(in) :: x
    !  csqrtz = cdsqrt (x)
    csqrtz = sqrt (x)
  end function csqrtz

  complex(16) function clogz (x)
    implicit none
    complex(16), intent(in) :: x
    !  clogz = cdlog (x)
    clogz = log (x)
  end function clogz

  real(8) function atan2z (x,y)
    implicit none
    real(8), intent(in) :: x, y
    !
    atan2z = 0.0
    if (x .ne. 0.0 .or. y .ne. 0.0) atan2z = datan2(x, y)
  end function atan2z

  real(8) function signz (x, y)
    implicit none
    real(8), intent(in) :: x, y
    !
    signz = dsign (x, y)
  end function signz

  real(8) function amodz (x, y)
    implicit none
    real(8), intent(in) :: x, y
    !
    amodz = dmod (x, y)
  end function amodz

  real(8) function amin1z (x, y)
    implicit none
    real(8), intent(in) :: x, y
    !
    amin1z = dmin1 (x, y)
  end function amin1z

  real(8) function amax1z (x, y)
    implicit none
    real(8), intent(in) :: x, y
    !
    amax1z = dmax1 (x, y)
  end function amax1z

  !
  ! function cmplxz.
  !

  complex(16) function cmplxz (x, y)
    implicit none
    !     This function provides for all complex library functions of
    !     two real arguments.  All translations will make a
    !     substitution.
    !       VAX module switched to complex*16 (from (8)) in aug 1981
    real(8), intent(in) :: x, y
    cmplxz = dcmplx (x, y)
  end function cmplxz

  real(8) function aimagz (x)
    implicit none
    complex(8), intent(in) :: x
    aimagz = real (dimag (dcmplx (x)), kind (aimagz))
  end function aimagz

  real(8) function realz (x)
    implicit none
    complex(8), intent(in) :: x
    realz = dreal (x)
  end function realz

  real(8) function cabsz (x)
    implicit none
    complex(8), intent(in) :: x
    cabsz = cdabs (x)
  end function cabsz

  !     This module serves to provide selected double-precision
  !     library functions for several places in the program.
  !     Making this a subroutine rather than a function avoids all
  !     complications with the module name having a variable type
  !     associated with it.   It is installation-dependent because of two
  !     things --- first the use of entry points, and second the use
  !     of the double-precision declaration (by which is meant double
  !     the precision of regular floating-point variables of the EMTP).
  !     Since most byte-organized machines use  real(8)  for other
  !     variables, this implies  real*16 ,  if available.
  !     Installation-dependent module coded for  dec vax-11

  subroutine dabsz (x, y)
    implicit none
    double precision, intent(out) :: y
    double precision, intent(in) :: x
    y = dabs (x)
  end subroutine dabsz

  subroutine dcosz (x, y)
    implicit none
    double precision, intent(out) :: y
    double precision, intent(in) :: x
    y = dcos (x)
  end subroutine dcosz

  subroutine dexpz (x, y)
    implicit none
    double precision, intent(out) :: y
    double precision, intent(in) :: x
    y = dexp (x)
  end subroutine dexpz

  subroutine dsinz (x, y)
    implicit none
    double precision, intent(out) :: y
    double precision, intent(in) :: x
    y = dsin (x)
  end subroutine dsinz

  subroutine dsqrtz (x, y)
    implicit none
    double precision, intent(out) :: y
    double precision, intent(in) :: x
    y = dsqrt (x)
  end subroutine dsqrtz

  subroutine dlogz (x, y)
    implicit none
    double precision, intent(out) :: y
    double precision, intent(in) :: x
    y = dlog (x)
  end subroutine dlogz

  !     Installation-dependent module for dec vax-11 computer
  !     like "dlibrf" (see comments there), only for two inputs

  subroutine datn2z (x, y, z)
    implicit none
    double precision, intent(out) :: z
    double precision, intent(in) :: x, y
    z = datan2 (x, y)
  end subroutine datn2z

  !
  ! function iabsz.
  !

  integer(4) function iabsz (n1)
    implicit none
    !     One and only integer library function of one integer
    !     argument.    Make entry point if 2nd is used.
    integer(4), intent(in) :: n1
    iabsz = iabs (n1)
  end function iabsz

  !     Provision for all integer library functions of 2 integer arguments

  integer(4) function isignz (n1, n2)
    implicit none
    integer(4), intent(in) :: n1, n2
    isignz = isign (n1, n2)
  end function isignz

  integer(4) function modz (n1, n2)
    implicit none
    integer(4), intent(in) :: n1, n2
    modz = mod (n1, n2)
  end function modz

  integer(4) function min0z (n1, n2)
    implicit none
    integer(4), intent(in) :: n1, n2
    min0z = min0 (n1, n2)
  end function min0z

  integer(4) function max0z (n1, n2)
    implicit none
    integer(4), intent(in) :: n1, n2
    max0z = max0 (n1, n2)
  end function max0z

end module tracom

!
! end of file trasce.f90
!
