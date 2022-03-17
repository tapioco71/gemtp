!-*- Mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file strcom.f90
!

! Copyright 2019-2021 Angelo Rossi <angelo.rossi.homelab@gmail.com>
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice,
!    this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! 3. Neither the name of the copyright holder nor the names of its contributors
!    may be used to endorse or promote products derived from this software
!    without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.

!
! subroutine toUpper.
!

module strcom
  implicit none

  interface pack
     module procedure packa1, packch
  end interface pack

contains

  !
  ! Function toUpper.
  !

  function toUpper (strIn) result(strOut)
    ! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
    ! Original author: Clive Page
    implicit none
    character(len = *), intent(in) :: strIn
    character(len = len(strIn)) :: strOut
    integer(4) :: i, j
    !
    do i = 1, len (strIn)
       j = iachar (strIn(i : i))
       if (j >= iachar ("a") .and. j <= iachar ("z")) then
          strOut(i : i) = achar (iachar (strIn(i : i)) - iachar ("a") + iachar ("A"))
       else
          strOut(i : i) = strIn(i : i)
       end if
    end do
  end function toUpper

  !
  ! Function toLower.
  !

  function toLower (strIn) result(strOut)
    implicit none
    character(len = *), intent(in) :: strIn
    character(len = len(strIn)) :: strOut
    integer(4) :: i, j
    !
    do i = 1, len (strIn)
       j = iachar (strIn(i : i))
       if (j >= iachar ("A") .and. j <= iachar ("Z")) then
          strOut(i : i) = achar (iachar (strIn(i : i)) - iachar ("A") + iachar ("a"))
       else
          strOut(i : i) = strIn(i : i)
       end if
    end do
  end function toLower

  !
  ! subroutine str2int.
  !

  subroutine str2int (str, int, stat)
    implicit none
    character(len = *), intent(in) :: str
    integer(4), intent(out) :: int
    integer(4), intent(out) :: stat
    !
    read (unit = str, fmt = 10, iostat = stat) int
10  format (i8)
  end subroutine str2int

  !
  ! subroutine strip.
  !

  subroutine strip (s, c)
    implicit none
    character(*), intent(inout) :: s
    character, intent(in) :: c
    !
    integer(4) :: i, j
    character, pointer :: temp(:)
    !
    allocate (temp(len (s)))
    if (associated (temp)) then
       j = 1
       do i = 1, len (s)
          if (s(i : i) .ne. c) then
             temp(j : j) = s(i : i)
             j = j + 1
          end if
       end do
       do i = 1, j - 1
          s(i : i) = temp(i)
       end do
       deallocate (temp)
       nullify (temp)
    end if
  end subroutine strip

  !
  ! subroutine packa1.
  !

  subroutine packa1 (from, to, kk)
    implicit none
    !     System-dependent EMTP module  'packa1'  for  VAX-11/780.
    !     Argument  'from'  contains  a1  information which is to be stored
    !     in character position  kk  of argument  'to' .
    !     For all EMTP usage,  1st 2 arguments must be vectors.
    !     logical*1 from(1), to(6)
    character(8), intent(in) :: from
    character(8), intent(out) :: to
    integer(4), intent(in) :: kk
    !
    to(kk : kk) = from(1 : 1)
    return
  end subroutine packa1

  !
  ! subroutine packch.
  !

  subroutine packch (from, to, k4or6, nchbeg, nword)
    implicit none
    !
    !     This module performs the system-dependent function of packing bcd
    !     characters from  a4  or  a6  words together so as to form a
    !     contiguous string of characters without extra blank fill.   For
    !     example, cdc has a 60-bit word which stores 10 bcd characters (6
    !     bits to a character).   With plot alphanumeric text read under  a6
    !     format control, the right-most four characters of such words will
    !     be blank-filled by the system.   IBM's eight characters in an
    !     eight-byte word pose an analogous problem.   Since the  calcomp
    !     plotting subroutine calls which appear in
    !     the calling module require contiguous character strings, output
    !     text must be compressed so as to remove the word-length-dependent
    !     blank fill.   The present subroutine is called by the plotting
    !     code to perform this function.   Meaning of the subroutine
    !     arguments is as follows ......
    !      nword ----- the number of words which are to have their bcd
    !                  characters extracted, and packed into  a character
    !                  string.   this is a positive integer.
    !      from(1)  ----- the first of  'nword'  words  whose bcd contents
    !                     are to be packed.
    !      k4or6  ----- equal to either  4  or  6 ,  whichever is the number
    !                   of characters of bcd information which is stored in
    !                   the words   (from(i), i=1, nword) .
    !      to(1)  ----- the active bcd characters of   (from(i), i=1, nword)
    !                   are to be packed as a contiguous string of
    !                   characters beginning with character position
    !                   'nchbeg'  of word  to(1) .
    !      nchbeg  -----  the character position of word  to(1)  where the
    !                     contiguous string of bcd characters is to begin.
    !                     normally  'nchbeg'  is between  1  and  10 ,
    !                     although larger positive values are allowed.
    !                     for example, a value of  27  means that the cdc
    !                     character insertion begins in position  7  of
    !                     word  to(3) .
    character(*), intent(in) :: from(:)
    character(*), intent(out) :: to(:)
    integer(4), intent(in) :: k4or6, nchbeg, nword
    integer(4) :: i, j, k, i_char, i_word
    !
    select case (k4or6)
    case (4, 6)
       j = nchbeg
       do i = 1, nword
          do k = 1, k4or6
             i_word = (j - 1) / 8 + 1
             i_char = mod (j, 8)
             if (i_char .eq. 0) i_char = 8
             to(i_word)(i_char : i_char) = from(i)(k : k)
             j = j + 1
          end do
       end do

    case default
       call stoptp
    end select
    return
  end subroutine packch

end module strcom
