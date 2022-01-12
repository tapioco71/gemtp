!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file vardim.f90
!

!
! subroutine make_comment.
!

module test
  implicit none

  !
  ! Variable type definition.
  !

  type variable
     character(32) :: name
     character(32) :: options(4)
     integer(4) :: kind
     integer(4) :: dimension
  end type variable

  type variableslist
     type (variable) :: variable
     integer(4) :: index
  end type variableslist

contains
  subroutine make_subroutine_comment (unit, n)
    implicit none
    integer(4), intent(in) :: unit
    character(*), intent(in) :: n
    write (unit = unit, fmt = 10) trim(n)
10  format (/, '!', /, '! subroutine ', a, '.', /, '!', /)
    return
  end subroutine make_subroutine_comment

  !
  ! subroutine make_implicit_statement.
  !

  subroutine make_implicit_statement (unit, mode, realdim, integerdim)
    implicit none
    integer(4), intent(in) :: unit, mode
    integer(4), intent(in), optional :: realdim, integerdim
    select case (mode)
    case (0)
       write (unit = unit, fmt = 10)

    case (1)
       write (unit = unit, fmt = 20)

    case (2)
       write (unit = unit, fmt = 30)

    case (3)
       if (present (realdim) .and. present (integerdim)) then
          write (unit = unit, fmt = 40) realdim, integerdim
       else
          stop
       end if
    end select
    return
10  format (2x, 'implicit none')
20  format (2x, 'implicit real(8) (a-h, o-z), integer(4) (i-n)')
30  format (2x, 'implicit real(16) (a-h, o-z), integer(8) (i-n)')
40  format (2x, 'implicit real(', i2, ') (a-h, o-z), integer(', i2, ') (i-n)')
  end subroutine make_implicit_statement

  !
  ! subroutine make_include_statement.
  !

  subroutine make_include_statement (unit, filename)
    implicit none
    integer(4), intent(in) :: unit
    character(*), intent(in) :: filename
    write (unit = unit, fmt = 10) filename
10  format (2x, 'include ', "'", a, "'")
    return
  end subroutine make_include_statement

  !
  ! subroutine make_use_statement.
  !

  subroutine make_use_statement (unit, modulename)
    implicit none
    integer(4), intent(in) :: unit
    character(*), intent(in) :: modulename
    write (unit = unit, fmt = 10) modulename
10  format (2x, 'use ', a)
    return
  end subroutine make_use_statement

  !
  ! function make_fortran_type_declaration.
  !

  function make_fortran_type_declaration (typename, dim) result (strOut)
    implicit none
    character(32) :: strOut
    character(*), intent(in) :: typename
    integer(4), intent(in), optional :: dim
    if (present (dim)) then
       write (unit = strOut, fmt = 10) typename, dim
10     format (a, '(', i2, ') ')
    else
       write (unit = strOut, fmt = 20) typename
20     format (a)
    end if
  end function make_fortran_type_declaration

  !
  ! subroutine make_variable_declaration.
  !

  subroutine make_variable_declaration (unit, var, types)
    implicit none
    character(*), intent(in) :: types(:)
    integer(4), intent(in) :: unit
    type (variable), intent(in) :: var
    integer(4) :: i
    !
    if (var%name .ne. '' .and. var%kind .gt. 0 .and. size(types) .gt. 0) then
       write (unit = unit, fmt = 10, advance = 'no') trim (types(var%kind))
10     format (2x, a)
       do i = 1, size (var%options)
          if (.not. (var%options(i) == '')) then
             write (unit = unit, fmt = 20, advance = 'no') trim (var%options(i))
20           format (', ', a)
          end if
       end do
       write (unit = unit, fmt = "(' :: ', a)", advance = 'no') trim (var%name)
       if (var%dimension .lt. 0) then
          write (unit = unit, fmt = "('(:)')")
       else if (var%dimension .eq. 0) then
          write (unit = unit, fmt = *)
       else
          write (unit = unit, fmt = 30) var%dimension
30        format ('(', i8, ')')
       end if
    end if
  end subroutine make_variable_declaration

  !
  ! subroutine make_equivalence_declaration.
  !

!   subroutine make_equivalence_declaration (unit, varname1, dim1, varname2, dim2)
!     implicit none
!     character(*), intent(in) :: varname1, varname2
!     integer(4), intent(in) :: unit
!     integer(4), intent(in), optional :: dim1, dim2
!     write (unit = unit, fmt = 10, advance = 'no') trim (varname1)
! 10  format (2x, 'equivalence', 1x, '(', a)
!     if (present (dim1)) then
!        write (unit = unit, fmt = 20, advance = 'no') dim1
! 20     format ('(', i2, ')')
!     end if
!     write (unit = unit, fmt = 30, advance = 'no') trim (varname2)
! 30  format (',', 1x, a)
!     if (present (dim2)) then
!        write (unit = unit, fmt = 40, advance = 'no') dim2
! 40     format ('(', i2, ')')
!     end if
!     write (unit = unit, fmt = 50)
! 50  format (')')
!   end subroutine make_equivalence_declaration

  subroutine make_equivalence_declaration (unit, varslist)
    implicit none
    integer(4), intent (in) :: unit
    type (variableslist), intent (in) :: varslist(:)
    integer(4) :: i, ios
    !
    inquire (unit = unit, iostat = ios)
    if (ios .eq. 0) then
       if (size(varslist) .ge. 2) then
          write (unit = unit, fmt = 10, advance = 'no')
10        format (2x, 'equivalence (')
          do i = 1, size(varslist)
             write (unit = unit, fmt = 12, advance = 'no') trim (varslist(i)%variable%name)
12           format (a)
             if (varslist(i)%index .gt. 0) then
                write (unit = unit, fmt = 14, advance = 'no') varslist(i)%index
14              format ('(', i2, ')')
             end if
             if (i .lt. size(varslist)) then
                write (unit = unit, fmt = "(', ')", advance = 'no')
             end if
          end do
          write (unit = unit, fmt = 20)
20        format (')')
       end if
    end if
  end subroutine make_equivalence_declaration

end module test

program vardim
  use bcdtim
  use bcddat
  use test
  implicit none
  integer(4), parameter :: modvara = 126, modvarc = 56
  !
  character :: char(6), bus2
  character(4) :: texta(2), textb(2)
  character(8) :: bus1
  character(16) :: types(6)
  character(16) :: ansi16
  character(80) :: abuff
  integer(4) :: i, ii, implmode, indexm, ios
  integer(4) :: j
  !  integer(4) :: jbltyp
  integer(4) :: kextra(29), kk
  integer(4) :: lm, lstdef(49), ltlabl, lunit(15), lstnew(99)
  integer(4) :: m, mextra, mtot, mul34, mulvar(7)
  integer(4) :: n1, n3, n4, n7, n9, n18, n28, n37, n86, n87, nd, ndate, nh, nm
  !  integer(4) :: ncbarr(300)
  integer(4) :: nonneg, nrec3, ns, ntime, numkex, numlst, ny
  real(8) :: d1, d2, d3
  !  character(8) :: cblock(300), cblser(300)
  real(8) :: lphd2, lphase
  !  dimension cblock(300), ncbarr(300), cblser(300), jbltyp(300)
  integer(4) :: integerdim, realdim
  type (variable) :: modvars(260), tempvar
  type (variableslist), target :: equivlist(10)
  !
  integerdim = 4
  realdim = 8
  !
  data types(1) / 'real(8)' /
  data types(2) / 'complex(8)' /
  data types(3) / 'character(8)' /
  data types(4) / 'integer(4)' /
  data types(5) / 'complex(16)' /
  data types(6) / 'character(1)' /
  !
  modvars(1)%name       = 'x'
  modvars(1)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(1)%kind       = 1
  modvars(1)%dimension  = 3

  modvars(2)%name       = 'ykm'
  modvars(2)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(2)%kind       = 1
  modvars(2)%dimension  = 5

  modvars(3)%name       = 'km'
  modvars(3)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(3)%kind       = 4
  modvars(3)%dimension  = 5

  modvars(4)%name       = 'xk'
  modvars(4)%options(1 : 4) = (/ 'target', '      ', '      ', '      ' /)
  modvars(4)%kind       = 1
  modvars(4)%dimension  = 72

  modvars(5)%name       = 'xm'
  modvars(5)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(5)%kind       = 1
  modvars(5)%dimension  = 72

  modvars(6)%name       = 'weight'
  modvars(6)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(6)%kind       = 1
  modvars(6)%dimension  = 14

  modvars(7)%name       = 'iwtent'
  modvars(7)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(7)%kind       = 4
  modvars(7)%dimension  = 52

  modvars(8)%name       = 'con1'
  modvars(8)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(8)%kind       = 1
  modvars(8)%dimension  = 51

  modvars(9)%name       = 'iskip'
  modvars(9)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(9)%kind       = 4
  modvars(9)%dimension  = 13

  modvars(10)%name       = 'zinf'
  modvars(10)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(10)%kind       = 1
  modvars(10)%dimension  = 13

  modvars(11)%name       = 'eta'
  modvars(11)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(11)%kind       = 1
  modvars(11)%dimension  = 13

  modvars(12)%name       = 'nhist'
  modvars(12)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(12)%kind       = 4
  modvars(12)%dimension  = 13

  modvars(13)%name       = 'stailm'
  modvars(13)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(13)%kind       = 1
  modvars(13)%dimension  = 15

  modvars(14)%name       = 'stailk'
  modvars(14)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(14)%kind       = 1
  modvars(14)%dimension  = 15

  modvars(15)%name       = 'xmax'
  modvars(15)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(15)%kind       = 1
  modvars(15)%dimension  = 58

  modvars(16)%name       = 'koutvp'
  modvars(16)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(16)%kind       = 4
  modvars(16)%dimension  = 62

  modvars(17)%name       = 'bnrg'
  modvars(17)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(17)%kind       = 1
  modvars(17)%dimension  = 18

  modvars(18)%name       = 'sconst'
  modvars(18)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(18)%kind       = 1
  modvars(18)%dimension  = 20

  modvars(19)%name       = 'cnvhst'
  modvars(19)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(19)%kind       = 1
  modvars(19)%dimension  = 73

  modvars(20)%name       = 'sfd'
  modvars(20)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(20)%kind       = 1
  modvars(20)%dimension  = 71

  modvars(21)%name       = 'qfd'
  modvars(21)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(21)%kind       = 1
  modvars(21)%dimension  = 71

  modvars(22)%name       = 'semaux'
  modvars(22)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(22)%kind       = 1
  modvars(22)%dimension  = 22

  modvars(23)%name       = 'ibsout'
  modvars(23)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(23)%kind       = 4
  modvars(23)%dimension  = 12

  modvars(24)%name       = 'bvalue'
  modvars(24)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(24)%kind       = 1
  modvars(24)%dimension  = 12

  modvars(25)%name       = 'sptacs'
  modvars(25)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(25)%kind       = 1
  modvars(25)%dimension  = 19

  modvars(26)%name       = 'kswtyp'
  modvars(26)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(26)%kind       = 4
  modvars(26)%dimension  = 6

  modvars(27)%name       = 'modswt'
  modvars(27)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(27)%kind       = 4
  modvars(27)%dimension  = 6

  modvars(28)%name       = 'kbegsw'
  modvars(28)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(28)%kind       = 4
  modvars(28)%dimension  = 6

  modvars(29)%name       = 'lastsw'
  modvars(29)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(29)%kind       = 4
  modvars(29)%dimension  = 6

  modvars(30)%name       = 'kentnb'
  modvars(30)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(30)%kind       = 4
  modvars(30)%dimension  = 6

  modvars(31)%name       = 'nbhdsw'
  modvars(31)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(31)%kind       = 4
  modvars(31)%dimension  = 63

  modvars(32)%name       = 'topen'
  modvars(32)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(32)%kind       = 1
  modvars(32)%dimension  = 60

  modvars(33)%name       = 'crit'
  modvars(33)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(33)%kind       = 1
  modvars(33)%dimension  = 6

  modvars(34)%name       = 'kdepsw'
  modvars(34)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(34)%kind       = 4
  modvars(34)%dimension  = 60

  modvars(35)%name       = 'tdns'
  modvars(35)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(35)%kind       = 1
  modvars(35)%dimension  = 6

  modvars(36)%name       = 'isourc'
  modvars(36)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(36)%kind       = 4
  modvars(36)%dimension  = 6

  modvars(37)%name       = 'energy'
  modvars(37)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(37)%kind       = 1
  modvars(37)%dimension  = 6

  modvars(38)%name       = 'iardub'
  modvars(38)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(38)%kind       = 4
  modvars(38)%dimension  = 63

  modvars(39)%name       = 'ardube'
  modvars(39)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(39)%kind       = 1
  modvars(39)%dimension  = 64

  modvars(40)%name       = 'nonlad'
  modvars(40)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(40)%kind       = 4
  modvars(40)%dimension  = 9

  modvars(41)%name       = 'nonle'
  modvars(41)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(41)%kind       = 4
  modvars(41)%dimension  = 9

  modvars(42)%name       = 'vnonl'
  modvars(42)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(42)%kind       = 1
  modvars(42)%dimension  = 9

  modvars(43)%name       = 'curr'
  modvars(43)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(43)%kind       = 1
  modvars(43)%dimension  = 9

  modvars(44)%name       = 'anonl'
  modvars(44)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(44)%kind       = 1
  modvars(44)%dimension  = 9

  modvars(45)%name       = 'vecnl1'
  modvars(45)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(45)%kind       = 1
  modvars(45)%dimension  = 9

  modvars(46)%name       = 'vecnl2'
  modvars(46)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(46)%kind       = 1
  modvars(46)%dimension  = 9

  modvars(47)%name       = 'namenl'
  modvars(47)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(47)%kind       = 4
  modvars(47)%dimension  = 9

  modvars(48)%name       = 'vzero'
  modvars(48)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(48)%kind       = 1
  modvars(48)%dimension  = 9

  modvars(49)%name       = 'ilast'
  modvars(49)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(49)%kind       = 4
  modvars(49)%dimension  = 9

  modvars(50)%name       = 'nltype'
  modvars(50)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(50)%kind       = 4
  modvars(50)%dimension  = 9

  modvars(51)%name       = 'kupl'
  modvars(51)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(51)%kind       = 4
  modvars(51)%dimension  = 9

  modvars(52)%name       = 'nlsub'
  modvars(52)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(52)%kind       = 4
  modvars(52)%dimension  = 9

  modvars(53)%name       = 'xoptbr'
  modvars(53)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(53)%kind       = 1
  modvars(53)%dimension  = 2

  modvars(54)%name       = 'coptbr'
  modvars(54)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(54)%kind       = 1
  modvars(54)%dimension  = 2

  modvars(55)%name       = 'cursub'
  modvars(55)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(55)%kind       = 1
  modvars(55)%dimension  = 53

  modvars(56)%name       = 'cchar'
  modvars(56)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(56)%kind       = 1
  modvars(56)%dimension  = 10

  modvars(57)%name       = 'vchar'
  modvars(57)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(57)%kind       = 1
  modvars(57)%dimension  = 10

  modvars(58)%name       = 'gslope'
  modvars(58)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(58)%kind       = 1
  modvars(58)%dimension  = 10

  modvars(59)%name       = 'ktrans'
  modvars(59)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(59)%kind       = 4
  modvars(59)%dimension  = 1

  modvars(60)%name       = 'kk'
  modvars(60)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(60)%kind       = 4
  modvars(60)%dimension  = 1

  modvars(61)%name       = 'c'
  modvars(61)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(61)%kind       = 1
  modvars(61)%dimension  = 3

  modvars(62)%name       = 'tr'
  modvars(62)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(62)%kind       = 1
  modvars(62)%dimension  = 5

  modvars(63)%name       = 'tx'
  modvars(63)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(63)%kind       = 1
  modvars(63)%dimension  = 5

  modvars(64)%name       = 'r'
  modvars(64)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(64)%kind       = 1
  modvars(64)%dimension  = 3

  modvars(65)%name       = 'nr'
  modvars(65)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(65)%kind       = 4
  modvars(65)%dimension  = 2

  modvars(66)%name       = 'length'
  modvars(66)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(66)%kind       = 4
  modvars(66)%dimension  = 2

  modvars(67)%name       = 'cik'
  modvars(67)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(67)%kind       = 1
  modvars(67)%dimension  = 2

  modvars(68)%name       = 'ci'
  modvars(68)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(68)%kind       = 1
  modvars(68)%dimension  = 2

  modvars(69)%name       = 'ck'
  modvars(69)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(69)%kind       = 1
  modvars(69)%dimension  = 2

  modvars(70)%name       = 'ismout'
  modvars(70)%options(1 : 4) = (/ 'target', '      ', '      ', '      ' /)
  modvars(70)%kind       = 4
  modvars(70)%dimension  = 70

  modvars(71)%name       = 'elp'
  modvars(71)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(71)%kind       = 1
  modvars(71)%dimension  = 65

  modvars(72)%name       = 'cu'
  modvars(72)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(72)%kind       = 1
  modvars(72)%dimension  = 66

  modvars(73)%name       = 'shp'
  modvars(73)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(73)%kind       = 1
  modvars(73)%dimension  = 67

  modvars(74)%name       = 'histq'
  modvars(74)%options(1 : 4) = (/ 'target', '      ', '      ', '      ' /)
  modvars(74)%kind       = 1
  modvars(74)%dimension  = 68

  modvars(75)%name       = 'ismdat'
  modvars(75)%options(1 : 4) = (/ 'target', '      ', '      ', '      ' /)
  modvars(75)%kind       = 4
  modvars(75)%dimension  = 69

  modvars(76)%name       = 'texvec'
  modvars(76)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(76)%kind       = 3
  modvars(76)%dimension  = 7

  modvars(77)%name       = 'ibrnch'
  modvars(77)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(77)%kind       = 4
  modvars(77)%dimension  = 12

  modvars(78)%name       = 'jbrnch'
  modvars(78)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(78)%kind       = 4
  modvars(78)%dimension  = 12

  modvars(79)%name       = 'tstop'
  modvars(79)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(79)%kind       = 1
  modvars(79)%dimension  = 4

  modvars(80)%name       = 'nonlk'
  modvars(80)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(80)%kind       = 4
  modvars(80)%dimension  = 9

  modvars(81)%name       = 'nonlm'
  modvars(81)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(81)%kind       = 4
  modvars(81)%dimension  = 9

  modvars(82)%name       = 'spum'
  modvars(82)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(82)%kind       = 1
  modvars(82)%dimension  = 25

  modvars(83)%name       = 'kks'
  modvars(83)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(83)%kind       = 4
  modvars(83)%dimension  = 1

  modvars(84)%name       = 'kknonl'
  modvars(84)%options(1 : 4) = (/ 'target', '      ', '      ', '      ' /)
  modvars(84)%kind       = 4
  modvars(84)%dimension  = 57

  modvars(85)%name       = 'znonl'
  modvars(85)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(85)%kind       = 1
  modvars(85)%dimension  = 57

  modvars(86)%name       = 'znonlb'
  modvars(86)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(86)%kind       = 1
  modvars(86)%dimension  = 1

  modvars(87)%name       = 'znonlc'
  modvars(87)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(87)%kind       = 1
  modvars(87)%dimension  = 1

  modvars(88)%name       = 'finit'
  modvars(88)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(88)%kind       = 1
  modvars(88)%dimension  = 1

  modvars(89)%name       = 'ksub'
  modvars(89)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(89)%kind       = 4
  modvars(89)%dimension  = 53

  modvars(90)%name       = 'msub'
  modvars(90)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(90)%kind       = 4
  modvars(90)%dimension  = 53

  modvars(91)%name       = 'isubeg'
  modvars(91)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(91)%kind       = 4
  modvars(91)%dimension  = 55

  modvars(92)%name       = 'litype'
  modvars(92)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(92)%kind       = 4
  modvars(92)%dimension  = 2

  modvars(93)%name       = 'imodel'
  modvars(93)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(93)%kind       = 4
  modvars(93)%dimension  = 2

  modvars(94)%name       = 'kbus'
  modvars(94)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(94)%kind       = 4
  modvars(94)%dimension  = 2

  modvars(95)%name       = 'mbus'
  modvars(95)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(95)%kind       = 4
  modvars(95)%dimension  = 2

  modvars(96)%name       = 'kodebr'
  modvars(96)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(96)%kind       = 4
  modvars(96)%dimension  = 2

  modvars(97)%name       = 'cki'
  modvars(97)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(97)%kind       = 1
  modvars(97)%dimension  = 2

  modvars(98)%name       = 'ckkjm'
  modvars(98)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(98)%kind       = 1
  modvars(98)%dimension  = 2

  modvars(99)%name       = 'indhst'
  modvars(99)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(99)%kind       = 4
  modvars(99)%dimension  = 2

  modvars(100)%name       = 'kodsem'
  modvars(100)%options(1 : 4) = ''
  modvars(100)%kind       = 4
  modvars(100)%dimension  = 2

  modvars(101)%name       = 'namebr'
  modvars(101)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(101)%kind       = 4
  modvars(101)%dimension  = 54

  modvars(102)%name       = 'iform'
  modvars(102)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(102)%kind       = 4
  modvars(102)%dimension  = 4

  modvars(103)%name       = 'node'
  modvars(103)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(103)%kind       = 4
  modvars(103)%dimension  = 4

  modvars(104)%name       = 'crest'
  modvars(104)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(104)%kind       = 1
  modvars(104)%dimension  = 4

  modvars(105)%name       = 'time1'
  modvars(105)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(105)%kind       = 1
  modvars(105)%dimension  = 4

  modvars(106)%name       = 'time2'
  modvars(106)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(106)%kind       = 1
  modvars(106)%dimension  = 4

  modvars(107)%name       = 'tstart'
  modvars(107)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(107)%kind       = 1
  modvars(107)%dimension  = 4

  modvars(108)%name       = 'sfreq'
  modvars(108)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(108)%kind       = 1
  modvars(108)%dimension  = 4

  modvars(109)%name       = 'kmswit'
  modvars(109)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(109)%kind       = 4
  modvars(109)%dimension  = 60

  modvars(110)%name       = 'nextsw'
  modvars(110)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(110)%kind       = 4
  modvars(110)%dimension  = 6

  modvars(111)%name       = 'rmfd'
  modvars(111)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(111)%kind       = 1
  modvars(111)%dimension  = 61

  modvars(112)%name       = 'cikfd'
  modvars(112)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(112)%kind       = 1
  modvars(112)%dimension  = 61

  modvars(113)%name       = 'imfd'
  modvars(113)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(113)%kind       = 4
  modvars(113)%dimension  = 27

  modvars(114)%name       = 'tclose'
  modvars(114)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(114)%kind       = 1
  modvars(114)%dimension  = 6

  modvars(115)%name       = 'adelay'
  modvars(115)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(115)%kind       = 1
  modvars(115)%dimension  = 60

  modvars(116)%name       = 'kpos'
  modvars(116)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(116)%kind       = 4
  modvars(116)%dimension  = 6

  modvars(117)%name       = 'namesw'
  modvars(117)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(117)%kind       = 4
  modvars(117)%dimension  = 6

  modvars(118)%name       = 'e'
  modvars(118)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(118)%kind       = 1
  modvars(118)%dimension  = 2

  modvars(119)%name       = 'f'
  modvars(119)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(119)%kind       = 1
  modvars(119)%dimension  = 1

  modvars(120)%name       = 'kssfrq'
  modvars(120)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(120)%kind       = 4
  modvars(120)%dimension  = 1

  modvars(121)%name       = 'kode'
  modvars(121)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(121)%kind       = 4
  modvars(121)%dimension  = 1

  modvars(122)%name       = 'kpsour'
  modvars(122)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(122)%kind       = 4
  modvars(122)%dimension  = 1

  modvars(123)%name       = 'volti'
  modvars(123)%options(1 : 4) = (/ 'target', '      ', '      ', '      ' /)
  modvars(123)%kind       = 1
  modvars(123)%dimension  = 59

  modvars(124)%name       = 'voltk'
  modvars(124)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(124)%kind       = 1
  modvars(124)%dimension  = 26

  modvars(125)%name       = 'volt'
  modvars(125)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(125)%kind       = 1
  modvars(125)%dimension  = 59

  modvars(126)%name       = 'bus'
  modvars(126)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(126)%kind       = 3
  modvars(126)%dimension  = 1

  modvars(127)%name       = 'karray'
  modvars(127)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(127)%kind       = 4
  modvars(127)%dimension  = 0

  modvars(128)%name       = 'tp'
  modvars(128)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(128)%kind       = 1
  modvars(128)%dimension  = 23

  modvars(129)%name       = 'norder'
  modvars(129)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(129)%kind       = 4
  modvars(129)%dimension  = 1

  modvars(130)%name       = 'index'
  modvars(130)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(130)%kind       = 4
  modvars(130)%dimension  = 1

  modvars(131)%name       = 'diag'
  modvars(131)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(131)%kind       = 1
  modvars(131)%dimension  = 1

  modvars(132)%name       = 'diab'
  modvars(132)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(132)%kind       = 1
  modvars(132)%dimension  = 1

  modvars(133)%name       = 'solr'
  modvars(133)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(133)%kind       = 1
  modvars(133)%dimension  = 1

  modvars(134)%name       = 'soli'
  modvars(134)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(134)%kind       = 1
  modvars(134)%dimension  = 1

  modvars(135)%name       = 'ich1'
  modvars(135)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(135)%kind       = 4
  modvars(135)%dimension  = 1

  modvars(136)%name       = 'bnd'
  modvars(136)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(136)%kind       = 1
  modvars(136)%dimension  = 9

  modvars(137)%name       = 'iloc'
  modvars(137)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(137)%kind       = 4
  modvars(137)%dimension  = 23

  modvars(138)%name       = 'gnd'
  modvars(138)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(138)%kind       = 1
  modvars(138)%dimension  = 23

  modvars(139)%name       = 'karray'
  modvars(139)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(139)%kind       = 4
  modvars(139)%dimension  = 9

  modvars(140)%name       = 'xdat'
  modvars(140)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(140)%kind       = 1
  modvars(140)%dimension  = 71

  modvars(141)%name       = 'ydat'
  modvars(141)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(141)%kind       = 1
  modvars(141)%dimension  = 71

  modvars(142)%name       = 'aphdat'
  modvars(142)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(142)%kind       = 1
  modvars(142)%dimension  = 71

  modvars(143)%name       = 'jndex'
  modvars(143)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(143)%kind       = 4
  modvars(143)%dimension  = 1

  modvars(144)%name       = 'diagg'
  modvars(144)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(144)%kind       = 1
  modvars(144)%dimension  = 1

  modvars(145)%name       = 'diabb'
  modvars(145)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(145)%kind       = 1
  modvars(145)%dimension  = 1

  modvars(146)%name       = 'solrsv'
  modvars(146)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(146)%kind       = 1
  modvars(146)%dimension  = 1

  modvars(147)%name       = 'solisv'
  modvars(147)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(147)%kind       = 1
  modvars(147)%dimension  = 1

  modvars(148)%name       = 'gndd'
  modvars(148)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(148)%kind       = 1
  modvars(148)%dimension  = 23

  modvars(149)%name       = 'bndd'
  modvars(149)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(149)%kind       = 1
  modvars(149)%dimension  = 23

  modvars(150)%name       = 'nekfix'
  modvars(150)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(150)%kind       = 4
  modvars(150)%dimension  = 4

  modvars(151)%name       = 'fxtem1'
  modvars(151)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(151)%kind       = 1
  modvars(151)%dimension  = 4

  modvars(152)%name       = 'fxtem2'
  modvars(152)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(152)%kind       = 1
  modvars(152)%dimension  = 4

  modvars(153)%name       = 'fxtem3'
  modvars(153)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(153)%kind       = 1
  modvars(153)%dimension  = 4

  modvars(154)%name       = 'fxtem4'
  modvars(154)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(154)%kind       = 1
  modvars(154)%dimension  = 4

  modvars(155)%name       = 'fxtem5'
  modvars(155)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(155)%kind       = 1
  modvars(155)%dimension  = 4

  modvars(156)%name       = 'fxtem6'
  modvars(156)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(156)%kind       = 1
  modvars(156)%dimension  = 4

  modvars(157)%name       = 'fixbu1'
  modvars(157)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(157)%kind       = 1
  modvars(157)%dimension  = 4

  modvars(158)%name       = 'fixbu2'
  modvars(158)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(158)%kind       = 1
  modvars(158)%dimension  = 4

  modvars(159)%name       = 'fixbu3'
  modvars(159)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(159)%kind       = 1
  modvars(159)%dimension  = 4

  modvars(160)%name       = 'fixbu4'
  modvars(160)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(160)%kind       = 1
  modvars(160)%dimension  = 4

  modvars(161)%name       = 'fixbu5'
  modvars(161)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(161)%kind       = 1
  modvars(161)%dimension  = 4

  modvars(162)%name       = 'fixbu6'
  modvars(162)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(162)%kind       = 1
  modvars(162)%dimension  = 4

  modvars(163)%name       = 'fixbu7'
  modvars(163)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(163)%kind       = 1
  modvars(163)%dimension  = 4

  modvars(164)%name       = 'fixbu8'
  modvars(164)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(164)%kind       = 1
  modvars(164)%dimension  = 4

  modvars(165)%name       = 'fixbu9'
  modvars(165)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(165)%kind       = 1
  modvars(165)%dimension  = 4

  modvars(166)%name       = 'fixb10'
  modvars(166)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(166)%kind       = 1
  modvars(166)%dimension  = 4

  modvars(167)%name       = 'fixb11'
  modvars(167)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(167)%kind       = 1
  modvars(167)%dimension  = 4

  modvars(168)%name       = 'kndex'
  modvars(168)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(168)%kind       = 4
  modvars(168)%dimension  = 4

  modvars(169)%name       = 'karray'
  modvars(169)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(169)%kind       = 4
  modvars(169)%dimension  = 9

  modvars(170)%name       = 'p'
  modvars(170)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(170)%kind       = 1
  modvars(170)%dimension  = 75

  modvars(171)%name       = 'z'
  modvars(171)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(171)%kind       = 1
  modvars(171)%dimension  = 75

  modvars(172)%name       = 'ic'
  modvars(172)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(172)%kind       = 4
  modvars(172)%dimension  = 71

  modvars(173)%name       = 'r'
  modvars(173)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(173)%kind       = 1
  modvars(173)%dimension  = 71

  modvars(174)%name       = 'd'
  modvars(174)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(174)%kind       = 1
  modvars(174)%dimension  = 71

  modvars(175)%name       = 'gmd'
  modvars(175)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(175)%kind       = 1
  modvars(175)%dimension  = 71

  modvars(176)%name       = 'x'
  modvars(176)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(176)%kind       = 1
  modvars(176)%dimension  = 71

  modvars(177)%name       = 'y'
  modvars(177)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(177)%kind       = 1
  modvars(177)%dimension  = 71

  modvars(178)%name       = 'tb2'
  modvars(178)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(178)%kind       = 1
  modvars(178)%dimension  = 71

  modvars(179)%name       = 'itb3'
  modvars(179)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(179)%kind       = 4
  modvars(179)%dimension  = 71

  modvars(180)%name       = 'workr1'
  modvars(180)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(180)%kind       = 1
  modvars(180)%dimension  = 71

  modvars(181)%name       = 'workr2'
  modvars(181)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(181)%kind       = 1
  modvars(181)%dimension  = 71

  modvars(182)%name       = 'text'
  modvars(182)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(182)%kind       = 3
  modvars(182)%dimension  = 76

  modvars(183)%name       = 'gd'
  modvars(183)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(183)%kind       = 1
  modvars(183)%dimension  = 74

  modvars(184)%name       = 'bd'
  modvars(184)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(184)%kind       = 1
  modvars(184)%dimension  = 74

  modvars(185)%name       = 'yd'
  modvars(185)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(185)%kind       = 1
  modvars(185)%dimension  = 74

  modvars(186)%name       = 'itbic'
  modvars(186)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(186)%kind       = 4
  modvars(186)%dimension  = 73

  modvars(187)%name       = 'tbr'
  modvars(187)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(187)%kind       = 1
  modvars(187)%dimension  = 73

  modvars(188)%name       = 'tbd'
  modvars(188)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(188)%kind       = 1
  modvars(188)%dimension  = 73

  modvars(189)%name       = 'tbg'
  modvars(189)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(189)%kind       = 1
  modvars(189)%dimension  = 73

  modvars(190)%name       = 'tbx'
  modvars(190)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(190)%kind       = 1
  modvars(190)%dimension  = 73

  modvars(191)%name       = 'tby'
  modvars(191)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(191)%kind       = 1
  modvars(191)%dimension  = 73

  modvars(192)%name       = 'tbtb2'
  modvars(192)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(192)%kind       = 4
  modvars(192)%dimension  = 73

  modvars(193)%name       = 'itbtb3'
  modvars(193)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(193)%kind       = 4
  modvars(193)%dimension  = 73

  modvars(194)%name       = 'tbtext'
  modvars(194)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(194)%kind       = 3
  modvars(194)%dimension  = 73

  modvars(195)%name       = 'karray'
  modvars(195)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(195)%kind       = 4
  modvars(195)%dimension  = 9

  modvars(196)%name       = 'karray'
  modvars(196)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(196)%kind       = 4
  modvars(196)%dimension  = 9

  !
  data char(1) / 'i' /
  data char(2) / 'j' /
  data char(3) / 'k' /
  data char(4) / 'l' /
  data char(5) / 'm' /
  data char(6) / 'n' /

  implmode = 0
  numlst = 28
  lunit(2 : 6) = (/ 7, 8, 9, 5, 6 /)
  mulvar(1 : 4) = (/ 2, 2, 2, 1 /)

  !

  modvars(201)%name = 'emtpf'
  modvars(201)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(201)%kind = 1
  modvars(201)%dimension = 1

  modvars(202)%name = 'infdli'
  modvars(202)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(202)%kind = 4
  modvars(202)%dimension = 1

  modvars(203)%name = 'temp_ispum'
  modvars(203)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(203)%kind = 4
  modvars(203)%dimension = 1

  modvars(204)%name = 'emtpe'
  modvars(204)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(204)%kind = 1
  modvars(204)%dimension = 1

  modvars(205)%name = 'cmr'
  modvars(205)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(205)%kind = 1
  modvars(205)%dimension = 1

  modvars(206)%name = 'trshun'
  modvars(206)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(206)%kind = 1
  modvars(206)%dimension = 1

  modvars(207)%name = 'cser'
  modvars(207)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(207)%kind = 1
  modvars(207)%dimension = 1

  modvars(208)%name = 'temp_massex'
  modvars(208)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(208)%kind = 4
  modvars(208)%dimension = 1

  modvars(209)%name = 'mapcas'
  modvars(209)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(209)%kind = 4
  modvars(209)%dimension = 1

  modvars(210)%name = 'mapinv'
  modvars(210)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(210)%kind = 4
  modvars(210)%dimension = 1

  modvars(211)%name = 'temp_txshun'
  modvars(211)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(211)%kind = 1
  modvars(211)%dimension = 1

  modvars(212)%name = 'node1'
  modvars(212)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(212)%kind = 4
  modvars(212)%dimension = 1

  modvars(213)%name = 'node2'
  modvars(213)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(213)%kind = 4
  modvars(213)%dimension = 1

  modvars(214)%name = 'icrit'
  modvars(214)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(214)%kind = 4
  modvars(214)%dimension = 1

  modvars(215)%name = 'cmi'
  modvars(215)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(215)%kind = 1
  modvars(215)%dimension = 1

  modvars(216)%name = 'temp_ipout'
  modvars(216)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(216)%kind = 4
  modvars(216)%dimension = -1

  modvars(217)%name = 'temp_n56'
  modvars(217)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(217)%kind = 4
  modvars(217)%dimension = -1

  modvars(218)%name = 'temp_ismold'
  modvars(218)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(218)%kind = 4
  modvars(218)%dimension = -1

  modvars(219)%name = 'nn10'
  modvars(219)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(219)%kind = 4
  modvars(219)%dimension = -1

  modvars(220)%name = 'temp_nn4'
  modvars(220)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(220)%kind = 4
  modvars(220)%dimension = -1

  modvars(221)%name = 'temp_nn14'
  modvars(221)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(221)%kind = 4
  modvars(221)%dimension = -1

  modvars(222)%name = 'caslnx'
  modvars(222)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(222)%kind = 1
  modvars(222)%dimension = 1

  modvars(223)%name = 'temp_volta'
  modvars(223)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(223)%kind = 1
  modvars(223)%dimension = 1

  modvars(224)%name = 'cshun'
  modvars(224)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(224)%kind = 1
  modvars(224)%dimension = 1

  modvars(225)%name = 'wk1'
  modvars(225)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(225)%kind = 1
  modvars(225)%dimension = 1

  modvars(226)%name = 'cblhst'
  modvars(226)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(226)%kind = 1
  modvars(226)%dimension = 1

  modvars(227)%name = 'ui'
  modvars(227)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(227)%kind = 1
  modvars(227)%dimension = 14 ! 40

  modvars(228)%name = 'trser'
  modvars(228)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(228)%kind = 1
  modvars(228)%dimension = 1

  modvars(229)%name = 'txser'
  modvars(229)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(229)%kind = 1
  modvars(229)%dimension = 1

  modvars(230)%name = 'temp_xx'
  modvars(230)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(230)%kind = 1
  modvars(230)%dimension = 1

  modvars(231)%name = 'r4'
  modvars(231)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(231)%kind = 1
  modvars(231)%dimension = 1

  modvars(232)%name = 'ur'
  modvars(232)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(232)%kind = 1
  modvars(232)%dimension = 14 ! 40

  modvars(233)%name = 'temp_caslnr'
  modvars(233)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(233)%kind = 1
  modvars(233)%dimension = 1

  modvars(234)%name = 'integx'
  modvars(234)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(234)%kind = 4
  modvars(234)%dimension = 1

  modvars(235)%name = 'akey'
  modvars(235)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(235)%kind = 1
  modvars(235)%dimension = 1

  modvars(236)%name = 'tstat'
  modvars(236)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(236)%kind = 1
  modvars(236)%dimension = 1

  modvars(237)%name = 'ndex'
  modvars(237)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(237)%kind = 4
  modvars(237)%dimension = 1

  modvars(238)%name = 'vim'
  modvars(238)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(238)%kind = 1
  modvars(238)%dimension = 1

  modvars(239)%name = 'jch2'
  modvars(239)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(239)%kind = 4
  modvars(239)%dimension = 20

  modvars(240)%name = 'temp_nsubkm'
  modvars(240)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(240)%kind = 4
  modvars(240)%dimension = 1

  modvars(241)%name = 'temp_vsmout'
  modvars(241)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(241)%kind = 1
  modvars(241)%dimension = 1

  modvars(242)%name = 'ksing'
  modvars(242)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(242)%kind = 4
  modvars(242)%dimension = 1

  modvars(243)%name = 'kindep'
  modvars(243)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(243)%kind = 4
  modvars(243)%dimension = 1

  modvars(244)%name = 'fold'
  modvars(244)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(244)%kind = 1
  modvars(244)%dimension = 1

  modvars(245)%name = 'w1'
  modvars(245)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(245)%kind = 1
  modvars(245)%dimension = 1

  modvars(246)%name = 'texvec1'
  modvars(246)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(246)%kind = 3
  modvars(246)%dimension = -1

  modvars(247)%name = 'buslst'
  modvars(247)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(247)%kind = 3
  modvars(247)%dimension = 2

  modvars(248)%name       = 'karray'
  modvars(248)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(248)%kind      = 4
  modvars(248)%dimension = 1

  modvars(249)%name       = 'ev'
  modvars(249)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(249)%kind      = 1
  modvars(249)%dimension = 2

  modvars(250)%name       = 'array'
  modvars(250)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(250)%kind      = 1
  modvars(250)%dimension = 1

  modvars(251)%name       = 'evdoub'
  modvars(251)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(251)%kind      = 1
  modvars(251)%dimension = 1

  modvars(252)%name       = 'itg'
  modvars(252)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(252)%kind      = 4
  modvars(252)%dimension = 1

  modvars(253)%name       = 'rtg'
  modvars(253)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(253)%kind      = 1
  modvars(253)%dimension = 1

  modvars(254)%name       = 'ctg'
  modvars(254)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(254)%kind      = 5
  modvars(254)%dimension = 1

  modvars(255)%name       = 'stg'
  modvars(255)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(255)%kind      = 1
  modvars(255)%dimension = 1

  modvars(256)%name       = 'itemp'
  modvars(256)%options(1 : 4) = (/ '', '', '', '' /)
  modvars(256)%kind      = 4
  modvars(256)%dimension = 9


  open (unit = lunit(2), iostat = ios, form = 'formatted')
  if (ios .eq. 0) then
     open (unit = lunit(3), iostat = ios, form = 'formatted')
     if (ios .eq. 0) then
        open (unit = lunit(4), iostat = ios, form = 'formatted')
        if (ios .eq. 0) then
           indexm = 0
           mextra = 0
           !     one less than the number of  'vardim'  modules.
           numkex = 7
           !     default values for extra offsets of  'vardim'
           !     modules belonging to  non-solution overlays.
           do i = 1, numkex
              kextra(i) = 0
           end do
           !     Default values for EMTP list sizes follow.
           lstdef(1)  =  250
           lstdef(2)  =  300
           lstdef(3)  =  500
           lstdef(4)  =  100
           lstdef(5)  =  2500
           lstdef(6)  =  40
           lstdef(7)  =  550
           lstdef(8)  =  1750
           lstdef(9)  =  75
           lstdef(10)  =  160
           lstdef(11)  =  50
           lstdef(12)  =  50
           lstdef(13)  =  5
           lstdef(14)  =  460
           lstdef(15)  =  50
           lstdef(16)  =  40
           lstdef(17)  =  4
           lstdef(18)  =  5
           lstdef(19)  =  1600
           lstdef(20)  =  650
           lstdef(21)  =  100
           lstdef(22)  =  150
           lstdef(23)  =  4000
           lstdef(24)  =  3
           lstdef(25)  =  400
           lstdef(26)  =  50
           lstdef(27)  =  lstdef(11)
           lstdef(28)  =  1080
           read (unit = lunit(5), fmt = 5287, iostat = ios) (lstnew(i), i = 1, numlst)
5287       format (10i8)
           write (unit = lunit(6), fmt = 5263)
5263       format (/,  " Pseudo-listing of data cards which have been read by the variable-dimensioning program  'vardim' .   Only  ",/, " if all data fields are punched with  'clean'  i8  integer information will this be a true listing.   Data cards", /, ' are in fact read in and then printed out using integer variables and  10i8  format.')
           write (unit = lunit(6), fmt = 5264) (i, i = 1, 8), (lstnew(i), i = 1, numlst)
5264       format (1x, 111('-'), /, 31x, '0', 8(9x, i1 ), /, 31x, '0', 8(9x, '0'), /, 31x, 80('-'), /, ' 1st card (lists  1-10).', 7x, '1', 10i8 ,/, ' 2nd card (lists 11-20).', 7x, '1', 10i8, /, ' 3rd card (lists 21-30).', 7x, '1', 10i8)
           if (lstnew(1) / 10000000 .ne. 9) go to 5294
           read (unit = lunit(5), fmt = 5287, iostat = ios) (kextra(i), i = 1, numkex)
           write (unit = lunit(6), fmt = 5378) (kextra(i), i = 1, numkex)
5378       format (' Supplemental offsets.', 9x, '1', 10i8)
           lstnew(1) = lstnew(1) - 90000000
5294       if (lstnew(11) / 10000000 .ne. 9) go to 5297
           n4 = lstnew(11) - 90000000
           do j = 1, numlst
              lstnew(j) = lstdef(j) * n4
           end do
5297       if (lstnew(1) .gt. 0) lstnew(1) = lstnew(1) + 2
           write (unit = lunit(6), fmt = 5381)
5381       format (1x, 111('-'))
           do i = 1, numlst
              n1 = i
              if (lstnew(i) .ge. 1000000) go to 9000
              if (lstnew(i) .le. 0) lstnew(i) = lstdef(i)
           end do
           if (lstnew(19) .le. 23) lstnew(19) = 23
           if (lstnew(26) .le. 10) lstnew(26) = 10
           n1 = lstnew(16) / 2
           if (2 * n1 .ne. lstnew(16)) lstnew(16) = lstnew(16) + 1
           n1 = lstnew(27) / 2
           if (2 * n1 .ne. lstnew(27)) lstnew(27) = lstnew(27) + 1
           !     list number 51 is a dependent list, always twice
           !     the size of list number 13.
           !     this is for frequency-dependence arrays
           !     'con1' ,  'con2' ,  and  'con3' .
           lstnew(51) = 6 * lstnew(13)
           !     list number 52 is a dependent list, always twice
           !     the size of list number 1, plus one.
           !     this is for frequency-dependence array   'iwtent' .
           lstnew(52) = lstnew(51) + 1
           !     list number 7 also serves for storage as part of
           !     list number 3.   hence list 7 must not be shorter.
           !     if (  lstnew(3)  .gt.  lstnew(7) )
           !    1 lstnew(7) = lstnew(3)
           !     list number 53 is for terminal pairs
           !     associated with compensation elements.
           lstnew(53) = lstnew(9) + 3 * lstnew(17)
           !      cable adds 5 list-2 extensions to  'namebr'
           lstnew(54) = 6 * lstnew(2)
           !     list 55 has one entry for each subnetwork.
           lstnew(55) = lstnew(9) + lstnew(17)
           !     list 56 is for vladimir's "cikfd" and "rmfd"
           !     arrays for freq-depend. generator equivalent
           lstnew(56) = lstnew(2) * lstnew(27) / 2
           !     list 57 maps #phases into znonl size.
           lstnew(57) = lstnew(1) * lstnew(24)
           !     list 58 is for extrema vector  "xmax" .
           lstnew(58) = 4 * lstnew(12)
           !     list 59 is extended working vector "volti"
           lstnew(59) = 2 * lstnew(26)
           !     list 60 provides double-length "kmswit" as
           !     replacement for earlier "klow" and "khigh"
           lstnew(60) = 3 * lstnew(6)
           !     list 61 is for ontario hydro freq-dep source
           lstnew(61) = 1
           !     list 62 is for power/energy ("koutvp" which
           !     now includes former, separate "koutie")
           lstnew(62) = 2 * lstnew(18)
           lstnew(63) = 3 * lstnew(6)
           lstnew(64) = 4 * lstnew(6)
           !     list 65 is for type-59 s.m. electrical data:
           lstnew(65) = 101 * lstnew(17)
           !     list 66 is for type-59 s.m. electrical variables:
           lstnew(66) = 24 * lstnew(17)
           !     list 67 is for type-59 s.m. mechanical data:
           lstnew(67) = 12 * lstnew(16)
           !     list 68 is for type-59 s.m. mechanical variables:
           lstnew(68) = 6 * lstnew(16)
           !     list 69 is for type-59 s.m. pointers:
           lstnew(69) = 30 * lstnew(17)
           !     list 70 is for type-59 s.m. output pointers:
           lstnew(70) = 5 * lstnew(11) + 2
           !     list 71 is to extend list 21 for ljg (18 aug 1987):
           lstnew(71) = 2 * lstnew(21)
           lstnew(72) = lstnew(8) + lstnew(28)
           !     list 73 is to extend list 22 for ljg (10 mar 1988):
           lstnew(73) = lstnew(22) + lstnew(21)
           mtot = 0
           do i = 1, modvara
              n9 = modvars(i)%dimension
              if (n9 .eq. 0 .or. n9 .eq. 98) exit
              n37 = 3
              bus1 = modvars(i)%name
              read (unit = bus1, fmt = 8104) bus2
8104          format (a1)
              do j = 1, 6
                 if (bus2 .eq. char(j)) n37 = 4
              end do
              if (modvars(i)%kind .ne. 0) n37 = modvars(i)%kind
              mtot = mtot + mulvar(n37) * lstnew(n9)
           end do
           write (unit = lunit(2), fmt = 4190)
4190       format ('!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-', //, '!', /, '! file newmods.f90', /, '!')
           write (unit = lunit(4), fmt = "('!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-', //, '!', /, '! file labcom.f90', /, '!', /)")
           write (unit = lunit(4), fmt = 7244)
7244       format ('module labcom', /, 2x, 'use blkcom')
           call make_subroutine_comment(lunit(2), 'main10')
           write (unit = lunit(2), fmt = "('#ifdef WITH_MAIN10')")
           write (unit = lunit(2), fmt = 4192)
4192       format ('subroutine main10')
           call make_use_statement (unit = lunit(2), modulename = 'labcom')
           call make_implicit_statement (unit = lunit(2), mode = 0)
           call make_implicit_statement (unit = lunit(4), mode = 0)
           do ii = 1, modvara
              i = 0 + ii
              n3 = modvars(i)%dimension
              nonneg = lstnew(n3)
              if (nonneg .le. 0) nonneg = 1
              n4 = modvars(i)%kind
              if (n4 .ne. 0) then
                 tempvar = modvars(i)
                 tempvar%dimension = nonneg
                 !                 call make_variable_declaration (lunit(2), tempvar, types)
                 if (tempvar%name .ne. '') call make_variable_declaration (lunit(4), tempvar, types)
!                 if (tempvar%name .ne. '') call make_variable_declaration (lunit(2), tempvar, types)
              end if
           end do
           !
           write (unit = lunit(2), fmt = "(2x, '!')")
           write (unit = lunit(4), fmt = "(2x, '!')")
           do ii = 1, modvarc
              n3 = modvars(200 + ii)%dimension
              if (n3 .lt. 0) then
                 nonneg = 0
              else
                 if (n3 .eq. 0) n3 = 1
                 nonneg = lstnew(n3)
                 if (nonneg .le. 0) nonneg = 1
              end if
              tempvar = modvars(200 + ii)
              tempvar%dimension = nonneg
              if (tempvar%name .ne. '') call make_variable_declaration (unit = lunit(4), var = tempvar, types = types)
           end do
           !
           write (unit = lunit(4), fmt = "(2x, '!')")
           !
           equivlist(1)%variable = modvars(113)   ! imfd(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(239)   ! jch2(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(125)   ! volt(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(238)   ! vim(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(118)   ! e(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(237)   ! ndex(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(33)    ! crit(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(236)   ! tstat(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(115)   ! adelay(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(235)   ! akey(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(33)    ! crit(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(214)   ! icrit(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(4)     ! xk(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(230)   ! xx(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(82)    ! spum(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(203)   ! ispum(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(84)    ! kknonl(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(240)   ! nsubkm(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(22)    ! semaux(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(225)   ! wk1(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(101)   ! namebr(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(202)   ! infdli(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(70)    ! ismout(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(241)   ! vsmout(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(74)    ! histq(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(208)   ! massex(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(123)   ! volti(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(223)   ! volta(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(56)    ! cchar(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(242)   ! ksing(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(58)    ! gslope(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(243)   ! kindep(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(57)    ! vchar(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(244)   ! fold(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(123)   ! volti(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(231)   ! r4(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(19)    ! cnvhst(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(226)   ! cblhst(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(1)     ! x(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(234)   ! integx(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(204)   ! emtpe(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(228)   ! trser(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(119)   ! f(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(229)   ! txser(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(123)   ! volti(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(212)   ! node1(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(124)   ! voltk(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(213)   ! node2(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(125)   ! volt(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(210)   ! mapinv(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(2)     ! ykm(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(209)   ! mapcas(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(4)     ! xk(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(233)   ! caslnr(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(5)     ! xm(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(222)   ! caslnx(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(121)   ! kode(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(207)   ! cser(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(60)    ! kk(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(206)   ! trshun(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(83)    ! kks(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(211)   ! txshun(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(84)    ! kknonl(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(224)   ! cshun(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(75)    ! ismdat(22)
           equivlist(1)%index = 22
           equivlist(2)%variable = modvars(216)   ! ipout
           equivlist(2)%index = 0
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(75)    ! ismdat(23)
           equivlist(1)%index = 23
           equivlist(2)%variable = modvars(217)   ! n56
           equivlist(2)%index = 0
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(75)    ! ismdat(24)
           equivlist(1)%index = 24
           equivlist(2)%variable = modvars(218)   ! ismold
           equivlist(2)%index = 0
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(75)    ! ismdat(25)
           equivlist(1)%index = 25
           equivlist(2)%variable = modvars(219)   ! nn10
           equivlist(2)%index = 0
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(75)    ! ismdat(26)
           equivlist(1)%index = 26
           equivlist(2)%variable = modvars(220)   ! nn4
           equivlist(2)%index = 0
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(75)    ! ismdat(27)
           equivlist(1)%index = 27
           equivlist(2)%variable = modvars(221)   ! nn14
           equivlist(2)%index = 0
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(125)   ! volt(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(233)   ! caslnr(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(123)   ! volti(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(232)   ! ur(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(124)   ! voltk(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(227)   ! ui(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(83)    ! kks(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(205)   ! cmr(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(84)    ! kknonl(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(215)   ! cmi(1)
           equivlist(2)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(2)     ! ykm(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(245)   ! w1(1)
           equivlist(2)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(76)    ! texvec(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(246)   ! texvec1
           equivlist(2)%index = 0
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(127)   ! karray(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(250)   ! array(1)
           equivlist(2)%index = 1
           equivlist(3)%variable = modvars(249)   ! ev(1)
           equivlist(3)%index = 1
           equivlist(4)%variable = modvars(247)   ! buslst(1)
           equivlist(4)%index = 1
           equivlist(5)%variable = modvars(251)   ! evdoub(1)
           equivlist(5)%index = 1
           equivlist(6)%variable = modvars(23)    ! ibsout(1)
           equivlist(6)%index = 1
           equivlist(7)%variable = modvars(77)    ! ibrnch(1)
           equivlist(7)%index = 1
           equivlist(8)%variable = modvars(78)    ! jbrnch(1)
           equivlist(8)%index = 1
           !           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 8))
           equivlist(1)%variable = modvars(127)   ! karray(1)
           equivlist(1)%index = 1
           equivlist(2)%variable = modvars(252)   ! itg(1)
           equivlist(2)%index = 1
           equivlist(3)%variable = modvars(253)   ! rtg(1)
           equivlist(3)%index = 1
           equivlist(4)%variable = modvars(254)   ! ctg(1)
           equivlist(4)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 4))
           equivlist(1)%variable = modvars(127)   ! karray(1)
           equivlist(1)%index = 1
           equivlist(4)%variable = modvars(255)   ! stg(1)
           equivlist(4)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           equivlist(1)%variable = modvars(124)   ! voltk(1)
           equivlist(1)%index = 1
           equivlist(4)%variable = modvars(256)   ! itemp(1)
           equivlist(4)%index = 1
           call make_equivalence_declaration (unit = lunit(4), varslist = equivlist(1 : 2))
           !
           write (unit = lunit(4), fmt = 7245)
7245       format ('end module labcom')
           write (unit = lunit(4), fmt = "(/, '!', /, '! end of file labcom.f90', /, '!', /)")
           write (unit = lunit(2), fmt = "(2x, 'call subr10')")
           write (unit = lunit(2), fmt = 7297)
7297       format (2x, 'return')
           write (unit = lunit(2), fmt = 7298) 'main10'
7298       format ('end subroutine ', a)
           write (unit = lunit(2), fmt = '(a)') '#endif'
           ltlabl = mtot
           if (ltlabl .gt. 9999999) go to 9000
           indexm = indexm + 1
           mul34 = mulvar(3) / mulvar(4)
           mextra = 4000
           lstnew(98) = ltlabl
           call make_subroutine_comment(lunit(3), 'dimens')
           write (unit = lunit(3), fmt = 8116)
8116       format ('subroutine dimens (ls, n, b1, b2)')
           call make_use_statement (unit = lunit(3), modulename = 'indcom')
           call make_implicit_statement (unit = lunit(3), mode = implmode)
           select case (implmode)
           case (3)
              write (unit = lunit(3), fmt = "(2x, 'integer, intent(out) :: ls(80)')")
              write (unit = lunit(3), fmt = "(2x, 'integer, intent(in) :: n')")
              write (unit = lunit(3), fmt = "(2x, 'integer n7')")

           case (0)
              write (unit = lunit(3), fmt = 8120) integerdim
8120          format (2x, 'integer(', i1, '), intent(out) :: ls(80)')
              write (unit = lunit(3), fmt = 8121) integerdim
8121          format (2x, 'integer(', i1, '), intent(in) :: n')
              write (unit = lunit(3), fmt = 8122) integerdim
8122          format (2x, 'integer(', i1, ') n7')
           end select
           write (unit = lunit(3), fmt = 8124)
8124       format (2x, 'character(8), intent(out) :: b1, b2')
           write (unit = lunit(3), fmt = 8134)
8134       format (2x, 'if (n .ge. 29) go to 2900')
           do i = 1, numlst
              write (unit = lunit(3), fmt = 8143) i, lstnew(i)
           end do
8143       format (2x, 'ls(', i2, ')  =', i8)
           write (unit = lunit(3), fmt = 8155) numlst
8155       format (2x, 'n7 =', i3, ' + 1')
           write (unit = lunit(3), fmt = 8159) ltlabl
8159       format (2x, 'ls(n7) =', i8)
           nrec3 = numlst + 7
           call time44 (texta)
           call date44 (textb)
           write (unit = ansi16, fmt = 3672) texta, textb
3672       format (4a4)
           read (unit = ansi16, fmt = 3676) nh, nm, ns, nm, nd, ny
3676       format (i2, 1x, i2, 1x, 2i2, 1x, i2, 1x, i2)
           ntime = 10000 * nh + 100 * nm + ns
           ndate = 10000 * nm + 100 * nd + ny
           write (unit = lunit(3), fmt = 3684) ntime, ndate
3684       format (2x, 'b1 =', "'", i8, "'", /, 2x, 'b2 =', "'", i8, "'")
           mtot = 0
           do i = 127, 138
              n9 = modvars(i)%dimension
              if (n9 .eq. 0 .or. n9 .eq. 98) exit
              n37 = 3
              bus1 = modvars(i)%name
              read (unit = bus1, fmt = 8104) bus2
              do j = 1, 6
                 if (bus2 .eq. char(j)) n37 = 4
              end do
              if (modvars(i)%kind .ne. 0) n37 = modvars(i)%kind
              mtot = mtot + mulvar(n37) * lstnew(n9)
           end do
           lstnew(99) = ltlabl + kextra(1)
           if (lstnew(99) .le. 0) lstnew(99) = 1
           modvars(127)%dimension = 99
           call make_subroutine_comment(lunit(2), 'over29')
           write (unit = lunit(2), fmt = '(a)') '#ifdef WITH_OVER29'
           write (unit = lunit(2), fmt = 4202)
4202       format ('subroutine over29')
           select case (implmode)
           case (0 : 2)
              call make_implicit_statement (unit = lunit(2), mode = implmode)

           case (3)
              call make_implicit_statement (unit = lunit(2), mode = implmode, realdim = realdim, integerdim = integerdim)
           end select
           write (unit = lunit(3), fmt = 8156)
8156       format (2x, 'return')
           lm = 0
           n86 = 29
           n87 = 31
           write (unit = lunit(3), fmt = 8158) n86, n86, n87
           nrec3 = nrec3 + 2
8158       format (i2.2, '00 if (n .gt.', i4, ') go to ', i2.2, '00')
           do ii = 1, 12
              i = modvara + ii
              n3 = modvars(i)%dimension
              if (ii .eq. 1) go to 4502
              n7 = ii - 1
              do kk = 1, n7
                 if (modvars(kk + modvara)%dimension .eq. n3) go to 4602
              end do
4502          lm = lm + 1
              n28 = n3
              if (n3 .eq. 99) n28 = 0
              write (lunit(3), 8143) lm, n28
              lm = lm + 1
              write (unit = lunit(3), fmt = 4769) lm, lstnew(n3)
4769          format (2x, 'ls(', i2, ') =', i7)
              nrec3 = nrec3 + 2
4602          continue
              nonneg = lstnew(n3)
              if (nonneg .le. 0) nonneg = 1
              n4 = modvars(i)%kind
              if (n4 .eq. 0) exit
              tempvar = modvars(i)
              tempvar%dimension = nonneg
              if (tempvar%name .ne. '') call make_variable_declaration (lunit(2), tempvar, types)
           end do
           n18 = 29
           write (unit = lunit(2), fmt = 7274) n18
7274       format (2x, 'call subr', i2, 63x)
           write (unit = lunit(2), fmt = 7297)
           write (unit = lunit(2), fmt = 7298) 'over29'
           write (unit = lunit(2), fmt = '(a)') '#endif'
           indexm = indexm + 1
           mextra = 5000
           call make_subroutine_comment(lunit(2), 'over31')
           write (unit = lunit(2), fmt = '(a)') '#ifdef WITH_OVER31'
           write (unit = lunit(2), fmt = 4203)
4203       format ('subroutine over31')
           select case (implmode)
           case (0 : 2)
              call make_implicit_statement (unit = lunit(2), mode = implmode)

           case (3)
              call make_implicit_statement (unit = lunit(2), mode = implmode, realdim = realdim, integerdim = integerdim)
           end select
           write (unit = lunit(3), fmt = 8156)
           lm = 0
           n86 = 31
           n87 = 39
           write (unit = lunit(3), fmt = 8158) n86, n86, n87
           nrec3 = nrec3 + 2
           do ii = 1, 1
              i = 138 + ii
              n3 = modvars(i)%dimension
              if (n3 .gt. 0) then
                 if (ii .eq. 1) go to 4503
                 n7 = ii - 1
                 do kk = 1, n7
                    if (modvars(kk + 138)%dimension .eq. n3) go to 4603
                 end do
4503             lm = lm + 1
                 n28 = n3
                 if (n3 .eq. 99) n28 = 0
                 write (unit = lunit(3), fmt = 8143) lm, n28
                 lm = lm + 1
                 write (unit = lunit(3), fmt = 4769) lm, lstnew(n3)
                 nrec3 = nrec3 + 2
4603             continue
                 nonneg = lstnew(n3)
                 if (nonneg .le. 0) nonneg = 1
                 n4 = modvars(i)%kind
                 if (n4 .eq. 0) go to 4103
                 tempvar = modvars(i)
                 tempvar%dimension = nonneg
                 if (tempvar%name .ne. '') call make_variable_declaration (lunit(2), tempvar, types)
              end if
           end do
4103       continue
           n18 = 31
           write (unit = lunit(2), fmt = 7274) n18
           write (unit = lunit(2), fmt = 7297)
           write (unit = lunit(2), fmt = 7298) 'over31'
           write (unit = lunit(2), fmt = '(a)') '#endif'
           lstnew(71) = 500
           if (lstnew(3) .gt. 500) lstnew(71) = lstnew(3)
           call make_subroutine_comment(lunit(2), 'over39')
           write (unit = lunit(2), fmt = '(a)') '#ifdef WITH_OVER39'
           write (unit = lunit(2), fmt = 4204)
4204       format ('subroutine over39')
           select case (implmode)
           case (0 : 2)
              call make_implicit_statement (unit = lunit(2), mode = implmode)

           case (3)
              call make_implicit_statement (unit = lunit(2), mode = implmode, realdim = realdim, integerdim = integerdim)
           end select
           write (unit = lunit(3), fmt = 8156)
           lm = 0
           n86 = 39
           n87 = 10
           write (unit = lunit(3), fmt = 8158) n86, n86, n87
           nrec3 = nrec3 + 2
           do ii = 1, 3
              i = 139  +  ii
              n3 = modvars(i)%dimension
              if (ii .eq. 1) go to 4504
              n7 = ii - 1
              do kk = 1, n7
                 if (modvars(kk + 139)%dimension .eq. n3) go to 4604
              end do
4504          lm = lm + 1
              n28 = n3
              if (n3 .eq. 99) n28 = 0
              write (unit = lunit(3), fmt = 8143) lm, n28
              lm = lm + 1
              write (unit = lunit(3), fmt = 4769) lm, lstnew(n3)
              nrec3 = nrec3 + 2
4604          continue
              nonneg = lstnew(n3)
              if (nonneg .le. 0) nonneg = 1
              n4 = modvars(i)%kind
              if (n4 .eq. 0) go to 4104
              tempvar = modvars(i)
              tempvar%dimension = nonneg
              if (tempvar%name .ne. '') call make_variable_declaration (lunit(2), tempvar, types)
           end do
4104       continue
           n18 = 39
           write (unit = lunit(2), fmt = 7274) n18
           write (unit = lunit(2), fmt = 7297)
           write (unit = lunit(2), fmt = 7298) 'over39'
           write (unit = lunit(2), fmt = '(a)') '#endif'
           call make_subroutine_comment(lunit(2), 'fixs10')
           write (unit = lunit(2), fmt = '(a)') '#ifdef WITH_FIXS10'
           write (unit = lunit(2), fmt = 4205)
4205       format ('subroutine fixs10')
           select case (implmode)
           case (0 : 2)
              call make_implicit_statement (unit = lunit(2), mode = implmode)

           case (3)
              call make_implicit_statement (unit = lunit(2), mode = implmode, realdim = realdim, integerdim = integerdim)
           end select
           write (unit = lunit(3), fmt = 8156)
           lm = 0
           n86 = 10
           n87 = 44
           write (unit = lunit(3), fmt = 8158) n86, n86, n87
           nrec3 = nrec3 + 2
           do ii = 1, 26
              i = 142  +  ii
              n3 = modvars(i)%dimension
              if (ii .eq. 1) go to 4505
              n7 = ii - 1
              do kk = 1, n7
                 if (modvars(kk + 142)%dimension .eq. n3) go to 4605
              end do
4505          lm = lm + 1
              n28 = n3
              if (n3 .eq. 99) n28 = 0
              write (unit = lunit(3), fmt = 8143) lm, n28
              lm = lm + 1
              write (unit = lunit(3), fmt = 4769) lm, lstnew(n3)
              nrec3 = nrec3 + 2
4605          continue
              nonneg = lstnew(n3)
              if (nonneg .le. 0) nonneg = 1
              n4 = modvars(i)%kind
              if ( n4  .eq.  0 )   go to 4105
              tempvar = modvars(i)
              tempvar%dimension = nonneg
              if (tempvar%name .ne. '') call make_variable_declaration (lunit(2), tempvar, types)
           end do
4105       continue
           n18 = 10
           write (unit = lunit(2), fmt = 7274) n18
           write (unit = lunit(2), fmt = 7297)
           write (unit = lunit(2), fmt = 7298) 'fixs10'
           write (unit = lunit(2), fmt = '(a)') '#endif'
           indexm = indexm + 1
           mextra = 7000
           d1 = (ltlabl + mextra + kextra(indexm)) / mul34
           d2 = (91. / 4. + 19. / 2. ) ** 2
           d3 = 4. * (11. / 8. + 19. / 2.) * (9. - d1)
           d1 = sqrt (d2 - d3)
           lphase = (-(91. / 4. + 19. / 2.) + d1) / (2. * (11. / 8. + 19. / 2.))
           if (lphase .le. 1) lphase = 2
           if (lphase .gt. 100) lphase = lphase * 0.7
           lphd2 = lphase / 2
           lstnew(71) = int (lphd2 * 2)
           lstnew(73) = lstnew(71) + 1
           lstnew(74) = int ((lphd2 * (lphd2 + 1)) / 2)
           lstnew(75) = (lstnew(71) * (lstnew(71) + 1)) / 2
           lstnew(76) = 2 * lstnew(71)
           call make_subroutine_comment(lunit(2), 'over44')
           write (unit = lunit(2), fmt = '(a)') '#ifdef WITH_OVER44'
           write (unit = lunit(2), fmt = 4206)
4206       format ('subroutine over44')
           select case (implmode)
           case (0 : 2)
              call make_implicit_statement (unit = lunit(2), mode = implmode)

           case (3)
              call make_implicit_statement (unit = lunit(2), mode = implmode, realdim = realdim, integerdim = integerdim)
           end select
           write (unit = lunit(3), fmt = 8156)
           lm = 0
           n86 = 44
           n87 = 45
           write (unit = lunit(3), fmt = 8158) n86, n86, n87
           nrec3 = nrec3 + 2
           do ii = 1, 26
              i = 168 + ii
              n3 = modvars(i)%dimension
              if (ii .eq. 1) go to 4506
              n7 = ii - 1
              do kk = 1, n7
                 if (modvars(kk + 168)%dimension .eq. n3) go to 4606
              end do
4506          lm = lm + 1
              n28 = n3
              if (n3 .eq. 99) n28 = 0
              write (unit = lunit(3), fmt = 8143) lm, n28
              lm = lm + 1
              write (unit = lunit(3), fmt = 4769) lm, lstnew(n3)
              nrec3 = nrec3 + 2
4606          continue
              nonneg = lstnew(n3)
              if (nonneg .le. 0) nonneg = 1
              n4 = modvars(i)%kind
              if (n4 .eq. 0) go to 4106
              tempvar = modvars(i)
              tempvar%dimension = nonneg
              if (tempvar%name .ne. '') call make_variable_declaration (lunit(2), tempvar, types)
           end do
4106       continue
           n18 = 44
           write (unit = lunit(2), fmt = 7274) n18
           write (unit = lunit(2), fmt = 7297)
           write (unit = lunit(2), fmt = 7298) 'over44'
           write (unit = lunit(2), fmt = '(a)') '#endif'
           indexm = indexm + 1
           mextra = 5000
           call make_subroutine_comment(lunit(2), 'over45')
           write (unit = lunit(2), fmt = '(a)') '#ifdef WITH_OVER45'
           write (unit = lunit(2), fmt = 4207)
4207       format ('subroutine over45')
           select case (implmode)
           case (0 : 2)
              call make_implicit_statement (unit = lunit(2), mode = implmode)

           case (3)
              call make_implicit_statement (unit = lunit(2), mode = implmode, realdim = realdim, integerdim = integerdim)
           end select
           write (unit = lunit(3), fmt = 8156)
           lm = 0
           n86 = 45
           n87 = 47
           write (unit = lunit(3), fmt = 8158) n86, n86, n87
           nrec3 = nrec3 + 2
           do ii = 1, 1
              i = 194  +  ii
              n3 = modvars(i)%dimension
              if (ii .eq. 1) go to 4507
              n7 = ii - 1
              do kk = 1, n7
                 if (modvars(kk + 194)%dimension .eq. n3) go to 4607
              end do
4507          lm = lm + 1
              n28 = n3
              if (n3 .eq. 99) n28 = 0
              write (unit = lunit(3), fmt = 8143) lm, n28
              lm = lm + 1
              write (unit = lunit(3), fmt = 4769) lm, lstnew(n3)
              nrec3 = nrec3 + 2
4607          continue
              nonneg = lstnew(n3)
              if (nonneg .le. 0) nonneg = 1
              n4 = modvars(i)%kind
              if ( n4  .eq.  0 )   go to 4107
              tempvar = modvars(i)
              tempvar%dimension = nonneg
              if (tempvar%name .ne. '') call make_variable_declaration (lunit(2), tempvar, types)
           end do
4107       continue
           n18 = 45
           write (unit = lunit(2), fmt = 7274) n18
           write (unit = lunit(2), fmt = 7297)
           write (unit = lunit(2), fmt = 7298) 'over45'
           write (unit = lunit(2), fmt = '(a)') '#endif'
           indexm = indexm + 1
           mextra = 5000
           call make_subroutine_comment(lunit(2), 'over47')
           write (unit = lunit(2), fmt = '(a)') '#ifdef WITH_OVER47'
           write (unit = lunit(2), fmt = 4208)
4208       format ('subroutine over47')
           select case (implmode)
           case (0 : 2)
              call make_implicit_statement (unit = lunit(2), mode = implmode)

           case (3)
              call make_implicit_statement (unit = lunit(2), mode = implmode, realdim = realdim, integerdim = integerdim)
           end select
           write (unit = lunit(3), fmt = 8156)
           lm = 0
           n86 = 47
           n87 = 99
           write (unit = lunit(3), fmt = 8158) n86, n86, n87
           nrec3 = nrec3 + 2
           do ii = 1, 1
              i = 195  +  ii
              n3 = modvars(i)%dimension
              if (ii .eq. 1) go to 4508
              n7 = ii - 1
              do kk = 1, n7
                 if (modvars(kk + 195)%dimension .eq. n3) go to 4608
              end do
4508          lm = lm + 1
              n28 = n3
              if (n3 .eq. 99) n28 = 0
              write (unit = lunit(3), fmt = 8143) lm, n28
              lm = lm + 1
              write (unit = lunit(3), fmt = 4769) lm, lstnew(n3)
              nrec3 = nrec3 + 2
4608          continue
              nonneg = lstnew(n3)
              if (nonneg .le. 0) nonneg = 1
              n4 = modvars(i)%kind
              if (n4 .eq. 0) go to 4108
              tempvar = modvars(i)
              tempvar%dimension = nonneg
              if (tempvar%name .ne. '') call make_variable_declaration (lunit(2), tempvar, types)
           end do
4108       continue
           n18 = 47
           write (unit = lunit(2), fmt = 7274) n18
           write (unit = lunit(2), fmt = 7297)
           write (unit = lunit(2), fmt = 7298) 'over47'
           write (unit = lunit(2), fmt = '(a)') '#endif'
           write (unit = lunit(3), fmt = 8156)
           write (unit = lunit(3), fmt = 8234)

8234       format (' 9900 ls(1) = location (b1) - location (b2)')
           write (unit = lunit(3), fmt = 8156)
           write (unit = lunit(3), fmt = 7298) 'dimens'
           nrec3 = nrec3 + 4
           rewind lunit(3)
           do m = 1, 99999
              read (unit = lunit(3), fmt = 8253, end = 8260) abuff
              write (unit = lunit(2), fmt = 8253) trim(abuff)
           end do
8253       format (a)
8260       write (unit = lunit(2), fmt = 8258)
8258       format (/, '!', /, '! end of file newmods.f90', /, '!')
           write (unit = lunit(6), fmt = 8942) ltlabl
8942       format (" Normal termination within  'vardim' .   The size of   /label/   equals", i8, '   integer words.', //, 1x)
           go to 9999
9000       write (unit = lunit(6), fmt = 9023)
9023       format (//, 1x)
           write (unit = lunit(6), fmt = 9027)
9027       format (1x, 131('-'))
           write (unit = lunit(6), fmt = 9031)
9031       format (22(' error'))
           write (unit = lunit(6), fmt = 9031)
           write (unit = lunit(6), fmt = 9027)
           write (unit = lunit(6), fmt = 9044)
9044       format (/, 5x, 'The user is in trouble, unless he has made a data-punching error.   Variable-dimensioning of the EMTP has', /, 5x, "broken down within the separate variable-dimensioning program  'vardim' .   In particular, one or more of the", /, 5x, 'user-inputted list sizes must be rejected as being illegally large.   User-supplied limits (or default limits,')
           write (unit = lunit(6), fmt = 9045) (lstnew(i), i = 1, numlst)
9045       format (5x, 'for any non-positive data fields) are as follows ....', /, (1x, 10i10))
           if (ltlabl .gt. 0) go to 9061
           write (unit = lunit(6), fmt = 9049) n1, lstnew(n1)
9049       format (5x, 'Specifically, the user-supplied value which was read for list number', i4, '  exceeds  999999 ,', /, 5x, 'which is unreal.   A value of', i10, "   was read from the user's data card for this list.", /, 1x)
9054       write (unit = lunit(6), fmt = 9027)
           write (unit = lunit(6), fmt = 9031)
           write (unit = lunit(6), fmt = 9031)
           write (unit = lunit(6), fmt = 9027)
           write (lunit(6), 9023)
           go to 9900
9061       write (unit = lunit(6), fmt = 9064) ltlabl
9064       format (5x, 'Specifically, the user-supplied dimensions have collectively produced a total labeled-common allocation', /, 5x, 'of', i12, '   words, which is unreal.   A limit of   9999999   has arbitrarily been imposed by the better judgment')
           write (unit = lunit(6), fmt = 9065)
9065       format (5x, 'of the program.   A machine this big (with this much real central memory) is not known to be in existence anywhere', /, 5x, 'in the world.', /, 1x)
           go to 9054
9900       continue
           close (unit = lunit(2), status = 'keep')
        end if
        close (unit = lunit(3), status = 'keep')
     endif
     close (unit = lunit(4), status = 'keep')
  end if
9999 stop

end program vardim

!
! end of file vardim.f90
!
