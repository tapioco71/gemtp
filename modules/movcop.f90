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
     module procedure movei, mover, movea
  end interface move

contains


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

  subroutine movei (a, b, n)
    !    Subroutine  move  is identical to the
    !    block-transfer routine  mover  except
    !    that  move  is for integer arrays, while  mover  was for
    !    floating-point arrays.   There is a difference, of course, on
    !    machines like ibm, where integer words may be shorter than
    !    floating-point words.
    implicit none
    integer(4), intent(out) :: b(:)
    integer(4), intent(in) :: a(:)
    integer(4), intent(in), optional :: n
    integer(4) :: n1
    !
    if (present (n)) then
       n1 = min (min (n, size (a)), size (b))
    else
       n1 = min (size (a), size (b))
    end if
    b(1 : n1) = a(1 : n1)
    return
  end subroutine movei

  subroutine mover (a, b, n)
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
    real(8), intent(out) :: b(:)
    real(8), intent(in) :: a(:)
    integer(4), intent(in), optional :: n
    integer(4) :: n1
    !
    if (present (n)) then
       n1 = min (min (n, size (a)), size (b))
    else
       n1 = min (size (a), size (b))
    end if
    b(1 : n1) = a(1 : n1)
    return
  end subroutine mover

  subroutine movea (a, b, n)
    implicit none
    character(*), intent(in) :: a(:)
    character(*), intent(out) :: b(:)
    integer(4), intent(in), optional :: n
    integer(4) :: n1, n2
    !
    if (present (n)) then
       n2 = min (min (n, size (a)), size (b))
    else
       n2 = min (size (a), size (b))
    end if
    n1 = min (len (a(1)), len (a(2)))
    b(1 : n1)(1 : n2) = a(1 : n1)(1 : n2)
    return
  end subroutine movea

  subroutine move0i (iarray, n)
    !    Subroutine  move0  is identical to  the block-zeroing routine
    !    mover0  except that  move0  is for integer arrays, while  mover0
    !    is for floating-point arrays.   There is a difference, on
    !    machines like IBM, where integer words may be shorter than
    !    floating-point words.
    implicit none
    integer(4), intent(out) :: iarray(:)
    integer(4), intent(in) :: n
    !
    call copyi (0, iarray, n)
    return
  end subroutine move0i

  subroutine move0r (farray, n)
    implicit none
    real(8), intent(out) :: farray(:)
    integer(4), intent(in) :: n
    !
    call copyr (0.0d0, farray, n)
    return
  end subroutine move0r

end module movcop
