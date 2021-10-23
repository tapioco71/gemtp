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

  integer(4) function loci (i)
    implicit none
    integer(4), intent(in) :: i
    integer(4) :: word
    integer(8) :: address
    address = loc (i)
    word = int (ibits (address, 0, 32), kind (word))
    loci = ibclr (ishft (word, -2), 31)
  end function loci

  !
  ! function locia.
  !

  integer(4) function locia (iarray)
    implicit none
    integer(4), intent(in) :: iarray(*)
    !     Installation-dependent EMTP module.   This is  VAX  version.
    !     function  'LOCINT'  is designed to return the address in memory
    !     of the argument, as an  INTEGER(4)  word address.   An arbitrary
    !     constant offset is allowed, since only differences will ever be
    !     used by the EMTP.   Note vector argument  "iarray"  (which
    !     is an assumption for all EMTP usage).
    locia = loci (iarray(1))
  end function locia

  !
  ! function locia.
  !

  integer(4) function locim (imatrix)
    implicit none
    integer(4), intent(in) :: imatrix(:, :)
    locim = loci (imatrix(1, 1))
  end function locim


  !
  ! function locf.
  !

  integer(4) function locf (f)
    implicit none
    real(8), intent(in) :: f
    integer(4) :: word
    integer(8) :: address
    !
    address = loc (f)
    word = int (ibits (address, 0, 32), kind (word))
    locf = ibclr (ishft (word, -3), 31)
  end function locf

  !
  ! function locfa.
  !

  integer(4) function locfa (farray)
    implicit none
    real(8), intent(in) :: farray(*)
    !
    locfa = locf (farray(1))
  end function locfa

  !
  ! function locfm.
  !

  integer(4) function locfm (fmatrix)
    implicit none
    real(8), intent(in) :: fmatrix(:, :)
    !
    locfm = locf (fmatrix(1, 1))
  end function locfm

  !
  ! function locchar.
  !

  integer(4) function locchar (ch)
    implicit none
    character(*), intent(in) :: ch
    integer(4) :: word
    integer(8) :: address
    !
    address = loc (ch(1 : 1))
    word = int (ibits (address, 0, 32), kind (word))
    locchar = ibclr (word, 31)
  end function locchar

  !
  ! function locchara
  !

  integer(4) function locchara (cha)
    implicit none
    character(*), intent(in) :: cha(:)
    !
    locchara = locchar (cha(1))
  end function locchara

  !
  ! function loclog
  !

  integer(4) function loclog (l)
    implicit none
    logical, intent(in) :: l
    integer(4) :: word
    integer(8) :: address
    !
    address = loc (l)
    word = int (ibits (address, 0, 32), kind (word))
    loclog = ibclr (word, 31)
  end function loclog

  !
  ! function loclog
  !

  integer(4) function locloga (larray)
    implicit none
    logical, intent(in) :: larray(:)
    !
    locloga = loclog (larray(1))
  end function locloga

end module indcom

!
! end of file indcom.f90
!
