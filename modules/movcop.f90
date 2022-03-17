!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file movcop.f90
!

module movcop

  implicit none

  interface copy
     module procedure copyi, copyr, copya
  end interface copy

  interface move0
     module procedure move0i, move0r
  end interface move0

  interface move
     module procedure movei, mover, moverl, movea
  end interface move

contains

  !
  ! subroutine copyi
  !

  subroutine copyi (i, iarray, n)
    implicit none
    !     Routine which copies the same integer word  'i'  into a
    !     contiguous region of memory ----  'n'  words in length,
    !     beginning with word iarray(1) .
    integer(4), intent(out) :: iarray(:)
    integer(4), intent(in) :: i
    integer(4), intent(in), optional :: n
    integer(4) :: n1
    !
    if (present (n)) then
       n1 = min (n, size (iarray))
    else
       n1 = size (iarray)
    end if
    iarray(1 : n1) = i
    return
  end subroutine copyi

  !
  ! subroutine copyr
  !

  subroutine copyr (f, farray, n)
    implicit none
    !     Routine which copies the same floating-point word  'f'  into a
    !     contiguous region of memory ----  'n'  words in length,
    !     beginning with word  farray(1) .
    real(8), intent(out) :: farray(:)
    real(8), intent(in) :: f
    integer(4), intent(in), optional :: n
    integer(4) :: n1
    !
    if (present (n)) then
       n1 = min (n, size (farray))
    else
       n1 = size (farray)
    end if
    farray(1 : n1) = f
    return
  end subroutine copyr

  !
  ! subroutine copya
  !

  subroutine copya (a, b, n)
    implicit none
    !     Routine which copies the same alphanumeric word  'a'  into
    !     a contiguous region of memory ----  'n'  words in length,
    !     beginning with word b(1) .
    character(*), intent(in) :: a
    character(*), intent(out) :: b(:)
    integer(4), intent(in), optional :: n
    integer(4) :: n1, n2
    !
    if (present (n)) then
       n2 = min (n, size (b))
    else
       n2 = size (b)
    end if
    n1 = min (len (a), len (b(1)))
    b(1 : n1)(1 : n2) = a(1 : n1)
    return
  end subroutine copya

  !
  ! subroutine movei
  !

  subroutine movei (from, to, n)
    !    Subroutine  move  is identical to the
    !    block-transfer routine  mover  except
    !    that  move  is for integer arrays, while  mover  was for
    !    floating-point arrays.   There is a difference, of course, on
    !    machines like ibm, where integer words may be shorter than
    !    floating-point words.
    implicit none
    integer(4), intent(out) :: to(:)
    integer(4), intent(in) :: from(:)
    integer(4), intent(in), optional :: n
    integer(4) :: n1
    !
    if (present (n)) then
       n1 = min (min (n, size (from)), size (to))
    else
       n1 = min (size (from), size (to))
    end if
    to(1 : n1) = from(1 : n1)
    return
  end subroutine movei

  !
  ! subroutine mover
  !

  subroutine mover (from, to, n)
    !    Subroutine  mover  (with entry point mover0) is used for block
    !    transfers between contiguous cells of core storage.   'n'  is the
    !    number of words to be transfered.
    !         1.  Using 'mover', the transfer is from  a(j)  to  b(j),
    !             for  j=1, n.
    !         2.  Using  'mover0',  a(1)  is copied into all  n  cells
    !             of array  'b'.   for zeroing array  'b' ,  the subroutine
    !             call is made with the first argument explicitely
    !             punched as zero.
    implicit none
    real(8), intent(out) :: to(:)
    real(8), intent(in) :: from(:)
    integer(4), intent(in), optional :: n
    integer(4) :: n1
    !
    if (present (n)) then
       n1 = min (min (n, size (from)), size (to))
    else
       n1 = min (size (from), size (to))
    end if
    to(1 : n1) = from(1 : n1)
    return
  end subroutine mover

  !
  ! subroutine movea
  !

  subroutine movea (from, to, n)
    implicit none
    character(*), intent(out) :: to(:)
    character(*), intent(in) :: from(:)
    integer(4), intent(in), optional :: n
    integer(4) :: n1, n2
    !
    if (present (n)) then
       n2 = min (min (n, size (from)), size (to))
    else
       n2 = min (size (from), size (to))
    end if
    n1 = min (len (from(1)), len (to(2)))
    to(1 : n1)(1 : n2) = from(1 : n1)(1 : n2)
    return
  end subroutine movea

  !
  ! subroutine moverl.
  !

  subroutine moverl (from, to, n)
    implicit none
    !     Module of interactive EMTP usage only, which services "emtspy".
    !     For non-interactive EMTP, this module can be destroyed.
    real(16), intent(out) :: to(:)
    integer(4), intent(in), optional :: n
    real(16), intent(in) :: from(:)
    integer(4) :: i, n1
    !
    if (present (n)) then
       n1 = min (size (from), min (size (to), n))
    else
       n1 = min (size (from), size (to))
    end if
    do i = 1, n1
       to(i) = from(i)
    end do
    return
  end subroutine moverl

  subroutine move0i (iarray, n)
    !    Subroutine  move0  is identical to  the block-zeroing routine
    !    mover0  except that  move0  is for integer arrays, while  mover0
    !    is for floating-point arrays.   There is a difference, on
    !    machines like IBM, where integer words may be shorter than
    !    floating-point words.
    implicit none
    integer(4), intent(out) :: iarray(:)
    integer(4), intent(in), optional :: n
    integer(4) :: n1
    !
    if (present (n)) then
       n1 = min (n, size (iarray))
    else
       n1 = size (iarray)
    end if
    call copyi (0, iarray, n1)
    return
  end subroutine move0i

  subroutine move0r (farray, n)
    implicit none
    real(8), intent(out) :: farray(:)
    integer(4), intent(in), optional :: n
    integer(4) :: n1
    !
    if (present (n)) then
       n1 = min (n, size (farray))
    else
       n1 = size (farray)
    end if
    call copyr (0.0d0, farray, n1)
    return
  end subroutine move0r

end module movcop
