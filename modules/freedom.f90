!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file freedom.f90 .
!

module freedom
  implicit none

  interface free
     module procedure frefld, freone, frenum
  end interface free

contains

  !
  ! subroutine frefld.
  !

  subroutine frefld (array)
    use blkcom
    use labcom
    implicit none
    real(8), intent(out) :: array(:)
    integer(4) :: i, jj, n3, n9
    character(8) :: text1, chtacs, texbuf(30)
    !  dimension array(1)
    !  equivalence (texvec(1), text1)
    save
    !
    data chtacs / 'tacs  ' /
    integer(4) :: ll
    if (iprsup .ge. 5) write (unit = lunit6, fmt = 1016) nfrfld, nright, kolbeg
1016 format (' Top "frefld".  nfrfld, nright, kolbeg =', 3i6)
    if (nright .lt. 0) go to 5913
    do jj = 1, nfrfld
       if (kolbeg .le. 80) go to 5600
       lstat(19) = 5600
       go to 9200
5600   n3 = 0
       go to 5805
5603   if (chcont .eq. chtacs) go to 5614
       if (text1 .eq. blank) go to 5802
       if (text1 .ne. csepar) go to 5623
5609   kolbeg = kolbeg + 1
       go to 5827
5614   if (text1 .ne. csepar) go to 5623
       if (text1 .eq. blank) go to 5802
       go to 5609
5623   if (n3 .lt. 30) go to 5627
       lstat(19) = 5623
       go to 9200
5627   n3 = n3 + 1
       texbuf(n3) = text1
5802   kolbeg = kolbeg + 1
5805   text1 = texcol(kolbeg)
       if (text1 .ne. chcont) go to 5819
       !     read input card using cimage
       call cimage
       kolbeg = 1
       if (n3 .eq. 0) go to 5805
       go to 5827
5819   if (kolbeg .le. 80) go to 5603
5827   if (n3 .gt. 0) go to 5829
       array(jj) = 0.0
       go to 5831
5829   call frenum (texbuf(1), n3, array(jj))
5831   if (iprsup .ge. 5) write (unit = lunit6, fmt = 5837) jj, kolbeg, n3, array(jj)
5837   format (/, ' "frefld" number.      jj  kolbeg      n3', 21x,  'array(jj)', /, 17x, 3i8, e30.20)
    end do
    go to 9900
5913 if (nright .lt. -1) go to 6036
    do jj = 1, nfrfld
       texta6(jj) = blank
       ll = 0
       if (kolbeg .le. 80) go to 5920
       lstat(19) = 5920
       go to 9200
5920   text1 = texcol(kolbeg)
       kolbeg = kolbeg + 1
       if (chcont .eq. chtacs) go to 5928
       if (text1 .eq. blank) go to 5923
       if (text1 .eq. csepar) go to 5948
5921   if (ll .le. 6) go to 5922
       lstat(19) = 5922
       go to 9200
5928   if (text1 .eq. csepar) go to 5948
       if (text1 .eq. blank) go to 5923
       go to 5921
5922   ll = ll + 1
       call packa1 (texvec(1), texta6(jj), ll)
5923   if (kolbeg .le. 80) go to 5920
    end do
5948 continue
    go to 9900
6036 ll = 0
    jj = 0
    go to 6054
6042 jj = jj + 1
    if (jj .gt. 10) go to 6072
    texta6(jj) = blank
    ll = 0
    do
       text1 = texcol(kolbeg)
       if (chcont .eq. chtacs) go to 6051
       if (text1 .eq. blank) go to 6054
       if (text1 .eq. csepar) go to 6072
       go to 6052
6051   if (text1 .eq. csepar) go to 6072
       if (text1 .eq. blank) go to 6054
6052   if (ll .eq. 6) go to 6042
       ll = ll + 1
       call packa1 (texvec(1), texta6(jj), ll)
       kolbeg = kolbeg + 1
    end do
6054 n9 = kolbeg
    do i = kolbeg, 80
       if (texcol(i) .ne. blank) go to 6067
    end do
    kolbeg = 79
    go to 6072
6067 kolbeg = i
    if (kolbeg - n9 .le. 2) go to 6069
    if (jj .gt. 0) go to 6072
6069 if (texcol(kolbeg) .ne. csepar ) go to 6042
6072 nfrfld = jj
    kolbeg = kolbeg + 1
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 6083) jj, ll, kolbeg, texcol
6083 format (/, ' Keyword near "frefld" exit.      jj      ll  kolbeg', /, 28x, 3i8, /, (' texcol =', 30a4))
    go to 9900
9200 kill = 166
    if (iprsup .ge. 0) write (unit = lunit6, fmt = 9207) lstat(19), nchain, lastov, kolbeg, nfrfld, nright
9207 format (/, " Error stop within  'frefld' .", 6i8, /, 1x)
    lstat(18) = -1
9900 if (iprsup .ge. 2) write (unit = lunit6, fmt = 9901) kill, kolbeg, array(1)
9901 format (' Exit "frefld".  kill, kolbeg, array(1) =', i6, e20.10)
    return
  end subroutine frefld

  !
  ! subroutine freone
  !

  subroutine freone (d1)
    implicit none
    !     Scalar version of  "frefld"  which enters the utpf with
    !     "m29."  vintage, to satisfy burroughs (see problem b,
    !     section ii, page ecwb-4, vol. x  EMTP memo of 14 feb 1981.)
    real(8), intent(out) :: d1
    real(8) :: array(1)
    !  dimension array(1)
    !
    call frefld (array)
    d1 = array(1)
    return
  end subroutine freone

  !
  ! subroutine frenum.
  !

  subroutine frenum (text1, n3, d1)
    implicit none
    !     VAX-11/780  installation-dependent module called only by
    !     the free-format data module  "frefld" .  Purpose is to
    !     convert input characters  (text1(1) ... text1(n3))  into
    !     a floating point number.
    !     real(8)        text1(1), blank
    integer(4), intent(in) :: n3
    real(8), intent(out) :: d1
    character(8), intent(in) :: text1(*)
    integer(4) :: i, n4, n9
    character(8) :: blank
    character(1) :: texta(30), textb
    !
    data blank / '      ' /
    data textb / ' ' /
    n9 = 30
    n4 = n3 + 1
    do i = 1, n3
       n4 = n4 - 1
       if (text1(n4) .eq. blank) go to 4718
       if (n9 .ge. 2) go to 4711
       write (unit = 6, fmt = 4706)
4706   format (/, ' Error stop in "frenum".   There are 33 or more characters in a free-format number on last data card.')
       call stoptp                                            ! installation-dependent program stop card
4711   write (unit = texta(n9), fmt = 4712) text1(n4)
4712   format (80a1)
       n9 = n9 - 1
    end do
4718 continue
    do i = 1, n9
       texta(i) = textb
    end do
    read (unit = texta(1), fmt = 4732) d1
4732 format (e30.0)
    return
  end subroutine frenum

end module freedom

!
! end of file freedom.f90 .
!
