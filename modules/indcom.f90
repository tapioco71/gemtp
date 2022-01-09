!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file indcom.f90
!

module indcom
  implicit none

  interface location
     module procedure loci, locia, locim, locf, locfa, locfm, locchar, locchara, loclog, locloga
  end interface location

contains
  !
  ! function loci.
  !

  function loci (i) result (valueOut)
    implicit none
    integer(4), intent(in) :: i
    integer(4) :: valueOut, word
    integer(8) :: address
    !
    address = loc (i)
    word = int (ibits (address, 0, 32), kind (word))
    valueOut = ibclr (ishft (word, -2), 31)
  end function loci

  !
  ! function locia.
  !

  function locia (iarray) result (valueOut)
    implicit none
    integer(4), intent(in) :: iarray(:)
    !     Installation-dependent EMTP module.   This is  VAX  version.
    !     function  'LOCINT'  is designed to return the address in memory
    !     of the argument, as an  INTEGER(4)  word address.   An arbitrary
    !     constant offset is allowed, since only differences will ever be
    !     used by the EMTP.   Note vector argument  "iarray"  (which
    !     is an assumption for all EMTP usage).
    integer(4) :: valueOut
    !
    valueOut = loci (iarray(1))
  end function locia

  !
  ! function locia.
  !

  function locim (imatrix) result (valueOut)
    implicit none
    integer(4), intent(in) :: imatrix(:, :)
    integer(4) :: valueOut
    !
    valueOut = loci (imatrix(1, 1))
  end function locim


  !
  ! function locf.
  !

  function locf (f) result (valueOut)
    implicit none
    real(8), intent(in) :: f
    integer(4) :: valueOut, word
    integer(8) :: address
    !
    address = loc (f)
    word = int (ibits (address, 0, 32), kind (word))
    valueOut = ibclr (ishft (word, -3), 31)
  end function locf

  !
  ! function locfa.
  !

  function locfa (farray) result (valueOut)
    implicit none
    real(8), intent(in) :: farray(:)
    integer(4) :: valueOut
    !
    valueOut = locf (farray(1))
  end function locfa

  !
  ! function locfm.
  !

  function locfm (fmatrix) result (valueOut)
    implicit none
    real(8), intent(in) :: fmatrix(:, :)
    integer(4) :: valueOut
    !
    valueOut = locf (fmatrix(1, 1))
  end function locfm

  !
  ! function locchar.
  !

  function locchar (ch) result (valueOut)
    implicit none
    character(*), intent(in) :: ch
    integer(4) :: valueOut, word
    integer(8) :: address
    !
    address = loc (ch(1 : 1))
    word = int (ibits (address, 0, 32), kind (word))
    valueOut = ibclr (word, 31)
  end function locchar

  !
  ! function locchara
  !

  function locchara (cha) result (valueOut)
    implicit none
    character(*), intent(in) :: cha(:)
    integer(4) :: valueOut
    !
    valueOut = locchar (cha(1))
  end function locchara

  !
  ! function loclog
  !

  function loclog (l) result (valueOut)
    implicit none
    logical, intent(in) :: l
    integer(4) :: valueOut, word
    integer(8) :: address
    !
    address = loc (l)
    word = int (ibits (address, 0, 32), kind (word))
    valueOut = ibclr (word, 31)
  end function loclog

  !
  ! function loclog
  !

  function locloga (larray) result (valueOut)
    implicit none
    logical, intent(in) :: larray(:)
    integer(4) :: valueOut
    !
    valueOut = loclog (larray(1))
  end function locloga

end module indcom

!
! end of file indcom.f90
!
