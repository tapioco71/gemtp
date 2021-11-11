!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file over44.f90
!

!
! subroutine subr44.
!

subroutine subr44
  use blkcom
  use deck44
  use com44
  use ovr44c
  implicit none
  integer(4) :: i, iofarr, iofdum, iofdur, iofei, iofer, iofthe, ioftii, ioftir, ioftix
  integer(4) :: ioftvi, ioftvr, iofwor, iofxti, iofxtr, iofxwc, iofxwy, iofyzi, iofyzr
  integer(4) :: iofzsu
  integer(4) :: j
  integer(4) :: length, lltemp(20)
  integer(4) :: n3, n5, n7, n8, ndim, nsqr, nsqr2, ntri
  real(8), allocatable :: stg(:)
  !  dimension stg(1)
  !  equivalence (stg(1), karray(1))
  !     list-zero "karray" is always 1st, and maybe "over29":
  !
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4567)
4567 format ('  "Begin module subr44."')
  length = size ((transfer (karray(1 :), stg)))
  allocate (stg (length))
  stg = transfer (karray(1 :), stg)
  if (.not. allocated(stg)) then
     write (unit = lunit6, fmt = 100)
100  format (' Could not associate stg to karray.  Stop.')
     call stoptp
  end if
  n8 = nchain
  if (kburro .eq. 1) n8 = 29
  call dimens (lltemp(1), n8, trash, trash)
  do j = 1, 9999, 2
     if (lltemp(j) .eq. 0) go to 5636
  end do
  write (unit = lunit6, fmt = 5632) lltemp(1), kburro, nchain
5632 format (' Temp error stop in "subr44".', 3i8)
  call stoptp
5636 n7 = lltemp(j + 1) * nbyte(4) / nbyte(3)
  call dimens (lltemp(1), nchain, trash, trash)
  n3 = 0
  do i = 1, 9999, 2
     if (n3 .ge. 2) go to 5655
     if (lltemp(i) .ne. 71) go to 5641
     lphase = lltemp(i + 1)
     n3 = n3 + 1
5641 if (lltemp(i) .ne. 74) go to 5654
     lgdbd = lltemp(i + 1)
     n3 = n3 + 1
  end do
5654 continue
  call stoptp
5655 lphpl1 = lphase + 1
  lphd2 = lphase / 2
  write (kunit6, 2456)  lphase
2456 format ('+Request for line-constants supporting prog.', i6)
  ndim = lphase
  ntri = ndim * (ndim + 1) / 2
  nsqr = ndim * ndim
  nsqr2 = 2 * nsqr
  iofarr = 1
  iofxwc = iofarr + ntri
  iofxwy = iofxwc + ntri
  iofyzr = iofxwy + ntri
  iofyzi = iofyzr + nsqr
  ioftii = iofyzi + nsqr
  ioftir = ioftii + nsqr
  ioftvi = ioftir + nsqr
  ioftvr = ioftvi + nsqr
  iofer  = ioftvr + nsqr
  iofei  = iofer  + ndim
  iofthe = iofei  + ndim
  iofxtr = iofthe + ndim
  iofxti = iofxtr + ndim
  iofzsu = iofxti + ndim
  iofdum = iofzsu + ndim
  iofdur = iofdum + ndim
  ioftix = iofdur + ndim
  iofwor = ioftix + nsqr2
  n5 = iofwor + nsqr2
  if (n5 .lt. n7) go to 10
  kill = 82
  lstat(19) = 10
  lstat(15) = lphase
  lstat(18) = nchain
  lastov = nchain
  nchain = 51
  go to 99999
10 call guts44 (stg(iofarr :), stg(iofxwc :), stg(iofxwy :), stg(iofyzr :), stg(iofyzi :), stg(ioftii :), stg(ioftir :), stg(ioftvi :), stg(ioftvr :), stg(iofer :), stg(iofei :), stg(iofthe :), stg(iofxtr :), stg(iofxti :), stg(iofzsu :), stg(iofdum :), stg(iofdur :), stg(ioftix :), stg(iofwor :), ndim, ntri, nsqr2)
  if (iprsup .ge. 1) write (unit = lunit6, fmt = 4568)
4568 format ('  "Exit  module subr44."')
99999 if (allocated (stg)) deallocate (stg)
  return
end subroutine subr44

!
! subroutine punpie.
!

subroutine punpie (kcirct)
  use blkcom
  use com44
  use deck44
  implicit none
  integer(4), intent(in) :: kcirct
  integer(4) :: i
  integer(4) :: j
  integer(4) :: k, kk
  !
  if (iprsup .ge. 1)  write (unit = *, fmt = *) ' Top "punpie".'
  write (unit = lunit7, fmt = 1201)
1201 format ('$vintage, 1')
  j = 1
  do i = 1, kcirct
     k = i + j - 1
     do kk = j, k
        yd(kk) = yd(kk) / tenm6
        if (kk .eq. j) write (unit = lunit7, fmt = 5201) i, brname(2*i-1), brname(2*i), p(kk), z(kk), yd(kk)
5201    format (i2, 2a6, 12x, 3e16.5)
        if (kk .ne. j) write (unit = lunit7, fmt = 6201) p(kk), z(kk), yd(kk)
6201    format (26x, 3e16.5)
     end do
     j = k + 1
  end do
  write (unit = lunit7, fmt = 2201)
2201 format ('$vintage, 0')
  if (iprsup .ge. 1)  write (unit = *, fmt = *) ' Exit "punpie".'
  return
end subroutine punpie

!
! subroutine dceign.
!

subroutine dceign (ar, ai, vi, vr, er, ei, n, nm, ierr, nv, nb, lunit6, iprsup, ndim)
  use tracom
  implicit none
  integer(4), intent(out) :: ierr
  integer(4), intent(in) :: iprsup
  integer(4), intent(in) :: lunit6
  integer(4), intent(in) :: n
  integer(4), intent(in) :: ndim
  integer(4), intent(in) :: nm
  integer(4), intent(in) :: nb
  integer(4), intent(in) :: nv
  real(8), intent(out) :: ai(ndim, ndim)
  real(8), intent(out) :: ar(ndim, ndim)
  real(8), intent(out) :: ei(ndim)
  real(8), intent(out) :: er(ndim)
  real(8), intent(out) :: vi(ndim, ndim)
  real(8), intent(out) :: vr(ndim, ndim)
  !  dimension ar(ndim,ndim),ai(ndim,ndim),er(ndim),ei(ndim),vi(ndim,ndim),vr(ndim,ndim)
  !  dimension scale(20),int(20),iord(20)
  integer(4) :: i, int(20), iord(20)
  integer(4) :: j
  integer(4) :: low
  integer(4) :: nupp
  real(8) :: scale(20)
  !
  if (iprsup .ge. 1) write (unit = *, fmt = *) ' Top of dceign.  Input matrix ar ....'
  do i = 1, n
     if (iprsup .ge. 1) write (unit = *, fmt = *) (ar(i, j), j = 1, n)
  end do
  if (iprsup .ge. 1) write (unit = *, fmt = *) '                 input matrix ai ....'
  do i = 1, n
     if (iprsup .ge. 1) write (unit = *, fmt = *) (ai(i, j), j = 1, n)
  end do
  if (n .gt. nm) go to 90
  low = 1
  nupp = n
  if (nb .eq. 0) go to 1
  !  Do balancing
  call cbal (nm, n, ar, ai, low, nupp, scale, ndim)
  !  Do transformation to upper hessenberg form
1 continue
  if (iprsup .ge. 3 ) write (unit = lunit6, fmt = 300) nm, n, low, nupp, nb, nv, ((ar(i, j), ai(i, j), j = 1, nm), i = 1, nm)
300 format (' Before call comhes, nm, n, low, nupp, nb, nv are', 6i5, /, ' ((ar(i, j), ai(i, j), j = 1, nm), i = 1, nm) are', /, (1x, 8e15.6))
  call comhes (nm, n, low, nupp, ar, ai, int, lunit6, iprsup, ndim, iord)
  if (nv .eq. 0) go to 12
  !  Calculate values and vectors
  if (iprsup .ge. 3) write (unit = lunit6, fmt = 350) ((ar(i, j), ai(i, j), j = 1, nm), i = 1, nm)
350 format (' After call comhes, ar(i, j) and ai(i, j) are', /, (1x, 8e15.6))
  call comlr2 (nm, n, low, nupp, int, ar, ai, vi, vr, er, ei, ierr, ndim, iord)
  if (iprsup .lt. 3) go to 410
  write (unit = lunit6, fmt = 400) (er(i), ei(i), i = 1, nm)
400 format (' After call comlr2 in dceign, the eigenvalues are', /, (1x, 8e15.6), /)
  write (unit = lunit6, fmt = 405) ((vr(i, j), vi(i, j), j = 1, nm), i = 1, nm)
405 format (' and eigenvectors are', /, (1x, 8e15.6))
410 if (ierr .ne. 0) go to 5
  !  Transform vectors to vectors of original matrix
  if (nb .eq. 0) return
  call cbabk2 (nm, n, low, nupp, scale, n, vi, vr, ndim)
  return
  !  Calculate values only
12 continue
  call comlr (nm, n, low, nupp, ar, ai, er, ei, ierr, ndim)
  if (ierr .ne. 0) go to 5
  return
  !  output error messages
5 write (unit = lunit6, fmt = 6) ierr
6 format (' ***Warning-dceign:  eigenvalue', i4, ' did not converge.')
  return
90 ierr = -1
  write (unit = lunit6, fmt = 91) n, nm
91 format (' **Error-dceign:  order of matrix greater than first dimension of matrix', /, 18x, 'order=', i5, ' first dimension=', i5)
  write (unit = *, fmt = *) 'output matrix ar '
  do i = 1, n
     write (unit = *, fmt = *) (ar(i, j), j = 1, n)
  end do
  write (unit = *, fmt = *) 'output matrix ai '
  do i = 1, n
     write (unit = *, fmt = *) (ai(i, j), j = 1, n)
  end do
  return
end subroutine dceign

!
! subroutine dceign.
!

subroutine cbal (nm, n, ar, ai, low, igh, scale, ndim)
  use tracom
  implicit none
  integer(4), intent(out) :: igh
  integer(4), intent(out) :: low
  integer(4), intent(in) :: n
  integer(4), intent(in) :: ndim
  integer(4), intent(in) :: nm
  real(8), intent(out) :: ai(ndim, ndim)
  real(8), intent(out) :: ar(ndim, ndim)
  real(8), intent(out) :: scale(20)
  !  dimension ar(ndim,ndim),ai(ndim,ndim),scale(20)
  integer(4) :: i, iexc
  integer(4) :: j, jj
  integer(4) :: k
  integer(4) :: l
  integer(4) :: m
  integer(4) :: noconv
  real(8) :: b2
  real(8) :: c, c1
  real(8) :: f
  real(8) :: g
  real(8) :: r, radix
  real(8) :: s
  !
  !     This subroutine is a translation of the algol procedure
  !     cbalance, which is a complex version of balance,
  !     num. math. 13, 293-304(1969) by parlett and reinsch.
  !     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
  !
  !     this subroutine balances a complex matrix and isolates
  !     eigenvalues whenever possible.
  !
  !     on input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        ar and ai contain the real and imaginary parts,
  !          respectively, of the complex matrix to be balanced.
  !
  !     on output:
  !
  !        ar and ai contain the real and imaginary parts,
  !          respectively, of the balanced matrix;
  !
  !        low and igh are two integers such that ar(i,j) and ai(i,j)
  !          are equal to zero if
  !           (1) i is greater than j and
  !           (2) j=1,...,low-1 or i=igh+1,...,n;
  !
  !        scale contains information determining the
  !           permutations and scaling factors used.
  !
  !     suppose that the principal submatrix in rows low through igh
  !     has been balanced, that p(j) denotes the index interchanged
  !     with j during the permutation step, and that the elements
  !     of the diagonal matrix used are denoted by d(i,j).  then
  !        scale(j) = p(j),    for j = 1,...,low-1
  !                 = d(j,j)       j = low,...,igh
  !                 = p(j)         j = igh+1,...,n.
  !     the order in which the interchanges are made is n to igh+1,
  !     then 1 to low-1.
  !
  !     note that 1 is returned for igh if igh is zero formally.
  !
  !     the algol procedure exc contained in cbalance appears in
  !     cbal  in line.  (note that the algol roles of identifiers
  !     k,l have been reversed.)
  !
  !     arithmetic is real throughout.
  !
  !     questions and comments should be directed to b. s. garbow,
  !     applied mathematics division, argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
  !     :::::::::: radix is a machine dependent parameter specifying
  !                the base of the machine floating point representation.
  !                radix = 16.0d0 for long form arithmetic
  !                on s360 ::::::::::
  !     data radix/z4210000000000000/
  !
  !     &&&&&&&&&&&&&&&   temporary patch for f4v vax compiler:
  radix = 16.0d0
  !     data statement just above is disabled (error for f4v). &&&
  !     &&&&&&&&&&&&&&&&&&&&&&&&&&&&  end temporary patch.  23 jan
  b2 = radix * radix
  k = 1
  l = n
  go to 100
  !     :::::::::: in-line procedure for row and
  !                column exchange ::::::::::
20 scale(m) = j
  if (j .eq. m) go to 50
  do i = 1, l
     f = ar(i,j)
     ar(i,j) = ar(i,m)
     ar(i,m) = f
     f = ai(i,j)
     ai(i,j) = ai(i,m)
     ai(i,m) = f
  end do
  do i = k, n
     f = ar(j,i)
     ar(j,i) = ar(m,i)
     ar(m,i) = f
     f = ai(j,i)
     ai(j,i) = ai(m,i)
     ai(m,i) = f
  end do
50 go to (80,130), iexc
  !     :::::::::: search for rows isolating an eigenvalue
  !                and push them down ::::::::::
80 if (l .eq. 1) go to 280
  l = l - 1
  !     :::::::::: for j=l step -1 until 1 do -- ::::::::::
100 do jj = 1, l
     j = l + 1 - jj
     do  i = 1, l
        if (i .eq. j) go to 110
        if (ar(j,i) .ne. 0.0 .or. ai(j,i) .ne. 0.0) go to 120
110  end do
     m = l
     iexc = 1
     go to 20
120 end do
  go to 140
  !     :::::::::: search for columns isolating an eigenvalue
  !                and push them left ::::::::::
130 k = k + 1
140 do j = k, l
     do i = k, l
        if (i .eq. j) go to 150
        if (ar(i,j) .ne. 0.0 .or. ai(i,j) .ne. 0.0) go to 170
150  end do
     m = k
     iexc = 2
     go to 20
170 end do
  !     :::::::::: now balance the submatrix in rows k to l ::::::::::
  do i = k, l
     scale(i) = 1.0
  end do
  !     :::::::::: iterative loop for norm reduction ::::::::::
190 noconv = 0
  do i = k, l
     c = 0.0
     r = 0.0
     do j = k, l
        if (j .eq. i) go to 200
        c = c + absz(ar(j,i)) + absz(ai(j,i))
        r = r + absz(ar(i,j)) + absz(ai(i,j))
200  end do
     !     :::::::::: guard against zero c or r due to underflow ::::::::::
     if (c .eq. 0.0 .or. r .eq. 0.0) go to 270
     g = r / radix
     f = 1.0
     s = c + r
210  if (c .ge. g) go to 220
     f = f * radix
     c = c * b2
     go to 210
220  g = r * radix
230  if (c .lt. g) go to 240
     f = f / radix
     c = c / b2
     go to 230
     !     :::::::::: now balance ::::::::::
240  c1 = 0.95
     if ((c + r) / f .ge.  c1 * s) go to 270
     g = 1.0 / f
     scale(i) = scale(i) * f
     noconv = 1
     do j = k, n
        ar(i,j) = ar(i,j) * g
        ai(i,j) = ai(i,j) * g
     end do
     do j = 1, l
        ar(j,i) = ar(j,i) * f
        ai(j,i) = ai(j,i) * f
     end do
  end do
270 continue
  if (noconv .eq. 1) go to 190
280 low = k
  igh = l
  return
end subroutine cbal

!
! subroutine cbabk2.
!

subroutine cbabk2 (nm, n, low, igh, scale, m, zr, zi, ndim)
  implicit none
  integer(4), intent(in) :: igh
  integer(4), intent(in) :: low
  integer(4), intent(in) :: m
  integer(4), intent(in) :: n
  integer(4), intent(in) :: ndim
  integer(4), intent(in) :: nm
  real(8), intent(in) :: scale(20)
  real(8), intent(out) :: zi(ndim, ndim)
  real(8), intent(out) :: zr(ndim, ndim)
  !  dimension scale(20),zr(ndim,ndim),zi(ndim,ndim)
  integer(4) :: i, ii
  integer(4) :: j
  integer(4) :: k
  real(8) :: s
  !
  !     This subroutine is a translation of the algol procedure
  !     cbabk2, which is a complex version of Balbak,
  !     num. math. 13, 293-304(1969) by Parlett and Reinsch.
  !     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
  !
  !     this subroutine forms the eigenvectors of a complex general
  !     matrix by back transforming those of the corresponding
  !     balanced matrix determined by  cbal.
  !
  !     On input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        low and igh are integers determined by  cbal;
  !
  !        scale contains information determining the permutations
  !          and scaling factors used by  cbal;
  !
  !        m is the number of eigenvectors to be back transformed;
  !
  !        zr and zi contain the real and imaginary parts,
  !          respectively, of the eigenvectors to be
  !          back transformed in their first m columns.
  !
  !     on output:
  !
  !        zr and zi contain the real and imaginary parts,
  !          respectively, of the transformed eigenvectors
  !          in their first m columns.
  !
  !     questions and comments should be directed to B. S. Garbow,
  !     applied mathematics division, Argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
  if (m .eq. 0) go to 200
  if (igh .eq. low) go to 120
  do i = low, igh
     s = scale(i)
     !     :::::::::: left hand eigenvectors are back transformed
     !                if the foregoing statement is replaced by
     !                s=1.0/scale(i). ::::::::::
     do j = 1, m
        zr(i, j) = zr(i, j) * s
        zi(i, j) = zi(i, j) * s
     end do
     !100  continue
  end do
  !110 continue
  !     :::::::::: for i=low-1 step -1 until 1,
  !                igh+1 step 1 until n do -- ::::::::::
120 do ii = 1, n
     i = ii
     if (i .ge. low .and. i .le. igh) go to 140
     if (i .lt. low) i = low - ii
     k = int (scale(i))
     if (k .eq. i) go to 140
     do j = 1, m
        s = zr(i,j)
        zr(i,j) = zr(k,j)
        zr(k,j) = s
        s = zi(i,j)
        zi(i,j) = zi(k,j)
        zi(k,j) = s
     end do
     !130  continue
  end do
140 continue
200 return
end subroutine cbabk2

!
! subroutine cmhes.
!

subroutine comhes (nm, n, low, igh, ar, ai, int, lunit6, iprsup, ndim, iord)
  implicit none
  integer(4), intent(in) :: igh
  integer(4), intent(out) :: int(20)
  integer(4), intent(out) :: iord(20)
  integer(4), intent(in) :: iprsup
  integer(4), intent(in) :: low
  integer(4), intent(in) :: lunit6
  integer(4), intent(in) :: n
  integer(4), intent(in) :: ndim
  integer(4), intent(in) :: nm
  real(8), intent(out) :: ai(ndim, ndim)
  real(8), intent(out) :: ar(ndim, ndim)
  !  dimension ar(ndim, ndim), ai(ndim, ndim), int(20), iord(20)
  integer(4) :: i, it
  integer(4) :: j
  integer(4) :: kp1
  integer(4) :: la
  integer(4) :: m, mm1, mp1
  real(8) :: d1, d2, d3, d13
  real(8) :: epscmh
  real(8) :: xi, xr
  real(8) :: yi, yr
  !
  !     This subroutine is a translation of the algol procedure comhes,
  !     num. math. 12, 349-368(1968) by Martin and Wilkinson.
  !     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
  !
  !     given a complex general matrix, this subroutine
  !     reduces a submatrix situated in rows and columns
  !     low through igh to upper hessenberg form by
  !     stabilized elementary similarity transformations.
  !
  !     On input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        low and igh are integers determined by the balancing
  !          subroutine  cbal.  if  cbal  has not been used,
  !          set low=1, igh=n;
  !
  !        ar and ai contain the real and imaginary parts,
  !          respectively, of the complex input matrix.
  !
  !     on output:
  !
  !        ar and ai contain the real and imaginary parts,
  !          respectively, of the hessenberg matrix.  the
  !          multipliers which were used in the reduction
  !          are stored in the remaining triangles under the
  !          hessenberg matrix;
  !
  !        int contains information on the rows and columns
  !          interchanged in the reduction.
  !          only elements low through igh are used.
  !
  !     arithmetic is real except for the replacement of the algol
  !     procedure cdiv by complex division using subroutine cmplxz.
  !
  !     questions and comments should be directed to B. S. Garbow,
  !     applied mathematics division, Argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
!!!!      flzero = 1.d-12       !replaced by epscmh }
  epscmh = 1.e-15
  do i = 1, 20
     iord(i) = i
  end do
  la = igh - 1
  kp1 = low + 1
  if (iprsup .ge. 3 ) write (unit = lunit6, fmt = 20) igh, low, la, kp1
20 format (' At the top of comhes, igh, low, la and kp1 are', 4i10)
  d13 = epscmh
  if (la .lt. kp1) go to 200
  do m = kp1, la
     mm1 = m - 1
     xr = 0.0d0
     xi = 0.0d0
     i = m
     do j = m, igh
!!!!            if (absz(ar(j,mm1)) + absz(ai(j,mm1))
!!!!     1         .le. absz(xr) + absz(xi)) go to 100
        if ((ar(j, mm1) ** 2) + (ai(j, mm1) ** 2) .le. (xr * xr) + (xi * xi)) go to 100
        xr = ar(j, mm1)
        xi = ai(j, mm1)
        i = j
100  end do
     int(m) = i
     if (i .eq. m) go to 130
     !     :::::::::: interchange rows and columns of ar and ai ::::::::::
     it = iord(i)
     iord(i) = iord(m)
     iord(m) = it
     do j = mm1, n
        yr = ar(i, j)
        ar(i, j) = ar(m, j)
        ar(m, j) = yr
        yi = ai(i, j)
        ai(i, j) = ai(m, j)
        ai(m, j) = yi
     end do
     do j = 1, igh
        yr = ar(j, i)
        ar(j, i) = ar(j, m)
        ar(j, m) = yr
        yi = ai(j, i)
        ai(j, i) = ai(j, m)
        ai(j, m) = yi
     end do
     !     :::::::::: end interchange ::::::::::
     !  130    if (xr .lt. d13 .and. xi .lt. d13)  go to 180
     !!  130    if (absz(xr) .lt. d13 .and. absz(xi) .lt. d13)  go to 180
130  if (xr .eq. 0.0d0 .and. xi .eq. 0.0d0)  go to 180
     mp1 = m + 1
     do i = mp1, igh
        yr = ar(i, mm1)
        yi = ai(i, mm1)
        !      if (yr .lt. d13 .and. yi .lt. d13) go to 160
        !!    if (absz(yr) .lt. d13 .and. absz(yi) .lt. d13) go to 160
        if (yr .eq. 0.0d0 .and. yi .eq. 0.0d0) go to 160
        d1 = xr * xr + xi * xi
        d2 = (xr * yr + yi * xi) / d1
        d3 = (xr * yi - xi * yr) / d1
        yr = d2
        yi = d3
        ar(i, mm1) = yr
        ai(i, mm1) = yi
        do j = m, n
           ar(i, j) = ar(i, j) - yr * ar(m, j) + yi * ai(m, j)
           ai(i, j) = ai(i, j) - yr * ai(m, j) - yi * ar(m, j)
        end do
        do j = 1, igh
           ar(j, m) = ar(j, m) + yr * ar(j, i) - yi * ai(j, i)
           ai(j, m) = ai(j, m) + yr * ai(j, i) + yi * ar(j, i)
        end do
     end do
160  continue
     if (iprsup .ge. 3) write (unit = lunit6, fmt = 170) m, igh, int(m), (ar(j, m), ai(j, m), j = 1, igh)
170  format (' m, igh and int(m) at 170 are', 3i10, /, ' (ar(j, m), ai(j, m), j = 1, igh), are', /, (1x, 8e15.6))
  end do
180 continue
200 return
end subroutine comhes

!
! subroutine comlr.
!

subroutine comlr (nm, n, low, igh, hr, hi, wr, wi, ierr, ndim)
  use blkcom
  use tracom
  implicit none
  integer(4), intent(out) :: ierr
  integer(4), intent(in) :: igh
  integer(4), intent(in) :: low
  integer(4), intent(in) :: n
  integer(4), intent(in) :: ndim
  integer(4), intent(in) :: nm
  real(8), intent(out) :: hi(ndim, ndim)
  real(8), intent(out) :: hr(ndim, ndim)
  real(8), intent(out) :: wi(ndim)
  real(8), intent(out) :: wr(ndim)
  !  dimension hr(ndim,ndim),hi(ndim,ndim),wr(ndim),wi(ndim)
  integer(4) :: i, ien, ienm1, im1, its
  integer(4) :: j
  integer(4) :: l, ll
  integer(4) :: m, mm, mp1
  real(8) :: d1, d2, d3, d4
  real(8) :: epmach
  real(8) :: si, sr
  real(8) :: ti, tr
  real(8) :: xi, xr
  real(8) :: yi, yr
  real(8) :: zzi, zzr
  !
  !     This subroutine is a translation of the algol procedure comlr,
  !     num. math. 12, 369-376(1968) by martin and wilkinson.
  !     handbook for auto. comp., vol.ii-linear algebra, 396-403(1971).
  !
  !     this subroutine finds the eigenvalues of a complex
  !     upper hessenberg matrix by the modified lr method.
  !
  !     on input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        low and igh are integers determined by the balancing
  !          subroutine  cbal.  if  cbal  has not been used,
  !          set low=1, igh=n;
  !
  !        hr and hi contain the real and imaginary parts,
  !          respectively, of the complex upper hessenberg matrix.
  !          their lower triangles below the subdiagonal contain the
  !          multipliers which were used in the reduction by  comhes,
  !          if performed.
  !
  !     on output:
  !
  !        the upper hessenberg portions of hr and hi have been
  !          destroyed.  therefore, they must be saved before
  !          calling  comlr  if subsequent calculation of
  !          eigenvectors is to be performed;
  !
  !        wr and wi contain the real and imaginary parts,
  !          respectively, of the eigenvalues.  if an error
  !          exit is made, the eigenvalues should be correct
  !          for indices ierr+1,...,n;
  !
  !        ierr is set to
  !          zero       for normal return,
  !          j          if the j-th eigenvalue has not been
  !                     determined after 30 iterations.
  !
  !     questions and comments should be directed to b. s. garbow,
  !     applied mathematics division, argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
  !     :::::::::: epmach is a machine dependent parameter specifying
  !                the relative precision of floating point arithmetic.
  !                epmach = 16.0d0**(-13) for long form arithmetic
  !                on s360 ::::::::::
  !     data epmach/z3410000000000000/
  !     epmach will be set to equal to the system dependent floating
  !     point zero variable  "flzero"
  epmach = flzero
  ierr = 0
  !     :::::::::: store roots isolated by cbal ::::::::::
  do i = 1, n
     if (i .ge. low .and. i .le. igh) go to 200
     wr(i) = hr(i,i)
     wi(i) = hi(i,i)
200 end do
  ien = igh
  tr = 0.0
  ti = 0.0
  !     :::::::::: search for next eigenvalue ::::::::::
220 if (ien .lt. low) go to 1001
  its = 0
  ienm1 = ien - 1
  !     :::::::::: look for single small sub-diagonal element
  !                for l=ien step -1 until low d0 -- ::::::::::
240 do ll = low, ien
     l = ien + low - ll
     if (l .eq. low) go to 300
     if (absz(hr(l,l-1)) + absz(hi(l,l-1)) .le. epmach * (absz(hr(l-1,l-1)) + absz(hi(l-1,l-1)) + absz(hr(l,l)) + absz(hi(l,l)))) go to 300
  end do
  !     :::::::::: form shift ::::::::::
300 if (l .eq. ien) go to 660
  if (its .eq. 30) go to 1000
  if (its .eq. 10 .or. its .eq. 20) go to 320
  sr = hr(ien,ien)
  si = hi(ien,ien)
  xr = hr(ienm1,ien) * hr(ien,ienm1) - hi(ienm1,ien) * hi(ien,ienm1)
  xi = hr(ienm1,ien) * hi(ien,ienm1) + hi(ienm1,ien) * hr(ien,ienm1)
  if (xr .eq. 0.0 .and. xi .eq. 0.0) go to 340
  yr = (hr(ienm1,ienm1) - sr) * onehaf
  yi = (hi(ienm1,ienm1) - si) * onehaf
  d1 = yr**2 - yi**2 + xr
  d2 = 2.0 * yr * yi + xi
  d3 = sqrtz( d1 ** 2 + d2 ** 2 )
  d3 = sqrtz( d3 )
  d4 = onehaf * atan2z(d2,d1)
  zzr = d3 * cosz (d4)
  zzi = d3 * sinz (d4)
  if (yr * zzr + yi * zzi .ge. 0.0) go to 310
  zzr = -zzr
  zzi = -zzi
310 d1 = (yr+zzr) **2 + (yi+zzi) **2
  d2 = ( xr*(yr+zzr) + xi*(yi+zzi) ) / d1
  d3 = ( xi*(yr+zzr) - xr*(yi+zzi) ) / d1
  sr = sr - d2
  si = si - d3
  go to 340
  !     :::::::::: form exceptional shift ::::::::::
320 sr = absz(hr(ien,ienm1)) + absz(hr(ienm1,ien-2))
  si = absz(hi(ien,ienm1)) + absz(hi(ienm1,ien-2))
340 do i = low, ien
     hr(i,i) = hr(i,i) - sr
     hi(i,i) = hi(i,i) - si
  end do
  tr = tr + sr
  ti = ti + si
  its = its + 1
  !     :::::::::: look for two consecutive small
  !                sub-diagonal elements ::::::::::
  xr = absz(hr(ienm1,ienm1)) + absz(hi(ienm1,ienm1))
  yr = absz(hr(ien,ienm1)) + absz(hi(ien,ienm1))
  zzr = absz(hr(ien,ien)) + absz(hi(ien,ien))
  !     :::::::::: for m=ien-1 step -1 until l do -- ::::::::::
  do mm = l, ienm1
     m = ienm1 + l - mm
     if (m .eq. l) go to 420
     yi = yr
     yr = absz(hr(m,m-1)) + absz(hi(m,m-1))
     xi = zzr
     zzr = xr
     xr = absz(hr(m-1,m-1)) + absz(hi(m-1,m-1))
     if (yr .le. epmach * zzr / yi * (zzr + xr + xi)) go to 420
  end do
  !     :::::::::: triangular decomposition h=l*r ::::::::::
420 mp1 = m + 1
  do i = mp1, ien
     im1 = i - 1
     xr = hr(im1,im1)
     xi = hi(im1,im1)
     yr = hr(i,im1)
     yi = hi(i,im1)
     if (absz(xr) + absz(xi) .ge. absz(yr) + absz(yi)) go to 460
     !     :::::::::: interchange rows of hr and hi ::::::::::
     do j = im1, ien
        zzr = hr(im1,j)
        hr(im1,j) = hr(i,j)
        hr(i,j) = zzr
        zzi = hi(im1,j)
        hi(im1,j) = hi(i,j)
        hi(i,j) = zzi
     end do
     d1 = yr ** 2 + yi ** 2
     d2 = ( xr*yr + xi*yi ) / d1
     d3 = ( yr*xi - xr*yi ) / d1
     wr(i) = 1.0
     go to 480
460  d1 = xr ** 2 + xi ** 2
     d2 = ( yr*xr + yi*xi ) / d1
     d3 = ( xr*yi - yr*xi ) / d1
     wr(i) = -1.0
480  zzr = d2
     zzi = d3
     hr(i,im1) = zzr
     hi(i,im1) = zzi
     do j = i, ien
        hr(i,j) = hr(i,j) - zzr * hr(im1,j) + zzi * hi(im1,j)
        hi(i,j) = hi(i,j) - zzr * hi(im1,j) - zzi * hr(im1,j)
     end do
  end do
  !     :::::::::: composition r*l=h ::::::::::
  do j = mp1, ien
     xr = hr(j,j-1)
     xi = hi(j,j-1)
     hr(j,j-1) = 0.0
     hi(j,j-1) = 0.0
     !     :::::::::: interchange columns of hr and hi,
     !                if necessary ::::::::::
     if (wr(j) .le. 0.0) go to 580
     do i = l, j
        zzr = hr(i,j-1)
        hr(i,j-1) = hr(i,j)
        hr(i,j) = zzr
        zzi = hi(i,j-1)
        hi(i,j-1) = hi(i,j)
        hi(i,j) = zzi
     end do
580  do i = l, j
        hr(i,j-1) = hr(i,j-1) + xr * hr(i,j) - xi * hi(i,j)
        hi(i,j-1) = hi(i,j-1) + xr * hi(i,j) + xi * hr(i,j)
     end do
  end do
  go to 240
  !     :::::::::: a root found ::::::::::
660 wr(ien) = hr(ien,ien) + tr
  wi(ien) = hi(ien,ien) + ti
  ien = ienm1
  go to 220
  !     :::::::::: set error -- no convergence to an
  !                eigenvalue after 30 iterations ::::::::::
1000 ierr = ien
1001 return
  !     :::::::::: last card of comlr ::::::::::
end subroutine comlr

!
! subroutine comlr2.
!

subroutine comlr2 (nm, n, low, igh, int, hr, hi, zi, zr, wr, wi, ierr, ndim, iord)
  use blkcom
  use tracom
  implicit none
  integer(4), intent(out) :: ierr
  integer(4), intent(in) :: igh
  integer(4), intent(in) :: int(20)
  integer(4), intent(out) :: iord(20)
  integer(4), intent(in) :: low
  integer(4), intent(in) :: n
  integer(4), intent(in) :: ndim
  integer(4), intent(in) :: nm
  real(8), intent(out) :: hi(ndim, ndim)
  real(8), intent(out) :: hr(ndim, ndim)
  real(8), intent(out) :: wi(ndim)
  real(8), intent(out) :: wr(ndim)
  real(8), intent(out) :: zi(ndim, ndim)
  real(8), intent(out) :: zr(ndim, ndim)
  !  dimension hr(ndim,ndim),hi(ndim,ndim),wr(ndim),wi(ndim)
  !  dimension zr(ndim,ndim),zi(ndim,ndim)
  !  dimension int(20),iord(20),lseq(20)
  !  dimension umr(20), umi(20)
  !  dimension eim(20),vtr(20),vti(20),ppr(20,20),ppi(20,20)
  integer(4) :: i, id, ien, iend, ienm1, ii, im1, ip1, its, itsmax, ittemp
  integer(4) :: j, jj
  integer(4) :: k
  integer(4) :: l, ll, lseq(20)
  integer(4) :: m, mm, mp1
  integer(4) :: nn
  real(8) :: d1, d2, d3, d4
  real(8) :: eim(20), epmach
  real(8) :: fnorm
  real(8) :: ppi(20, 20)
  real(8) :: ppr(20, 20)
  real(8) :: si, sr
  real(8) :: ti, tr
  real(8) :: umi(20)
  real(8) :: umr(20)
  real(8) :: vti(20)
  real(8) :: vtr(20)
  real(8) :: xi, xr
  real(8) :: yi, yr
  real(8) :: zzi, zzr
  !
  !     This subroutine is a translation of the algol procedure comlr2,
  !     num. math. 16, 181-204(1970) by Peters and Wilkinson.
  !     handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
  !
  !     this subroutine finds the eigenvalues and eigenvectors
  !     of a complex upper hessenberg matrix by the modified lr
  !     method.  the eigenvectors of a complex general matrix
  !     can also be found if  comhes  has been used to reduce
  !     this general matrix to hessenberg form.
  !
  !     on input:
  !
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement;
  !
  !        n is the order of the matrix;
  !
  !        low and igh are integers determined by the balancing
  !          subroutine  cbal.  if  cbal  has not been used,
  !          set low=1, igh=n;
  !
  !        int contains information on the rows and columns interchanged
  !          in the reduction by  comhes, if performed.  only elements
  !          low through igh are used.  if the eigenvectors of the hessen-
  !          berg matrix are desired, set int(j)=j for these elements;
  !
  !        hr and hi contain the real and imaginary parts,
  !          respectively, of the complex upper hessenberg matrix.
  !          their lower triangles below the subdiagonal contain the
  !          multipliers which were used in the reduction by  comhes,
  !          if performed.  if the eigenvectors of the hessenberg
  !          matrix are desired, these elements must be set to zero.
  !
  !     on output:
  !
  !        the upper hessenberg portions of hr and hi have been
  !          destroyed, but the location hr(1,1) contains the fnorm
  !          of the triangularized matrix;
  !
  !        wr and wi contain the real and imaginary parts,
  !          respectively, of the eigenvalues.  if an error
  !          exit is made, the eigenvalues should be correct
  !          for indices ierr+1,...,n;
  !
  !        zr and zi contain the real and imaginary parts,
  !          respectively, of the eigenvectors.  the eigenvectors
  !          are unnormalized.  if an error exit is made, none of
  !          the eigenvectors has been found;
  !
  !        ierr is set to
  !          zero       for normal return,
  !          j          if the j-th eigenvalue has not been
  !                     determined after 30 iterations.
  !
  !     questions and comments should be directed to b. s. garbow,
  !     applied mathematics division, argonne national laboratory
  !
  !     ------------------------------------------------------------------
  !
  !     :::::::::: epmach is a machine dependent parameter specifying
  !                the relative precision of floating point arithmetic.
  !                epmach = 16.0d0**(-13) for long form arithmetic
  !                on s360 ::::::::::
  !     data epmach/z3410000000000000/
  !     like in subroutine comlr, epmach is set to be flzero
!!!!      epmach = 1.0e-50      ! now in choice.dat }
  epmach = 1.e-32
  itsmax = 300
  ierr = 0
  !     :::::::::: initialize eigenvector matrix ::::::::::
  do i = 1, n
     do j = 1, n
        zr(i,j) = 0.0
        zi(i,j) = 0.0
        if (i .eq. j) zr(i,j) = 1.0
     end do
  end do
  !     :::::::::: form the matrix of accumulated transformations
  !                from the information left by comhes ::::::::::
  iend = igh - low - 1
  if (iend .le. 0) go to 180
  !     :::::::::: for i=igh-1 step -1 until low+1 do -- ::::::::::
  do ii = 1, iend
     i = igh - ii
     ip1 = i + 1
     do k = ip1, igh
        zr(k,i) = hr(k,i-1)
        zi(k,i) = hi(k,i-1)
     end do
     j = int(i)
     if (i .eq. j) go to 160
     do k = i, igh
        zr(i,k) = zr(j,k)
        zi(i,k) = zi(j,k)
        zr(j,k) = 0.0
        zi(j,k) = 0.0
     end do
     zr(j,i) = 1.0
160 end do
  if (iprsup .ge. 3) write (lunit6, 170) ((zr(i, j), zi(i, j), j = 1, igh), i = 1, igh)
170 format (' After do 160 loop in comlr2, ((zr(i, j), zi(i, j), j = 1, igh), i = 1, igh) are', /, (1x, 8e15.6))
  !     :::::::::: store roots isolated by cbal ::::::::::
180 do i = 1, n
     if (i .ge. low .and. i .le. igh) go to 200
     wr(i) = hr(i,i)
     wi(i) = hi(i,i)
200 end do
  ien = igh
  tr = 0.0
  ti = 0.0
  !     :::::::::: search for next eigenvalue ::::::::::
220 if (ien .lt. low) go to 680
  its = 0
  ienm1 = ien - 1
  !     :::::::::: look for single small sub-diagonal element
  !                for l=ien step -1 until low do -- ::::::::::
240 do ll = low, ien
     l = ien + low - ll
     if (l .eq. low) go to 300
     if (absz(hr(l,l-1)) + absz(hi(l,l-1)) .le. epmach * (absz(hr(l-1,l-1)) + absz(hi(l-1,l-1)) + absz(hr(l,l)) + absz(hi(l,l)))) go to 300
  end do
  !     :::::::::: form shift ::::::::::
300 if (l .eq. ien) go to 660
  if (its .eq. itsmax ) go to 1000
!!!!      if ((its/10)*10 .eq. its ) go to 320
!!!!!     if (its .eq. 10 .or. its .eq. 20) go to 320
  sr = hr(ien,ien)
  si = hi(ien,ien)
  xr = hr(ienm1,ien) * hr(ien,ienm1) - hi(ienm1,ien) * hi(ien,ienm1)
  xi = hr(ienm1,ien) * hi(ien,ienm1) + hi(ienm1,ien) * hr(ien,ienm1)
  if ( iprsup .ge. 3) write (lunit6, 305) sr, si, xr, xi
305 format (' sr, si, xr and xi at 305 are', 4e15.6)
  if (xr .eq. 0.0 .and. xi .eq. 0.0) go to 340
  yr = (hr(ienm1,ienm1) - sr) * onehaf
  yi = (hi(ienm1,ienm1) - si) * onehaf
  d1 = yr ** 2 - yi ** 2 + xr
  d2 = 2.0d0 * yr * yi + xi
  d3 = sqrtz (d1 ** 2 + d2 ** 2)
  d3 = sqrtz (d3)
  d4 = onehaf * atan2z (d2, d1)
  zzr = d3 * cosz (d4)
  zzi = d3 * sinz (d4)
  if (yr * zzr + yi * zzi .ge. 0.0) go to 310
  zzr = -zzr
  zzi = -zzi
310 d1 = (yr+zzr) **2 + (yi+zzi) **2
  d2 = ( xr*(yr+zzr) + xi*(yi+zzi) ) / d1
  d3 = ( xi*(yr+zzr) - xr*(yi+zzi) ) / d1
  if ( iprsup .ge. 3 ) write (lunit6, 315) yr, yi, zzr, zzi, d2,d3
315 format (' yr, yi, zzr, zzi and d2,d3 at 315 are', 6e15.6)
  sr = sr - d2
  si = si - d3
  go to 340
  !     :::::::::: form exceptional shift ::::::::::
!!!!  320 sr = absz(hr(ien,ienm1)) + absz(hr(ienm1,ien-2))
!!!!      si = absz(hi(ien,ienm1)) + absz(hi(ienm1,ien-2))
340 do i = low, ien
     hr(i,i) = hr(i,i) - sr
     hi(i,i) = hi(i,i) - si
     if ( iprsup .ge. 3 ) write (lunit6, 350) i, sr, si, hr(i,i),hi(i,i)
350  format (' i, sr, si, hr(i,i), and hi(i,i) at 350 are', /, 10x, i8, 4e15.6)
  end do
  tr = tr + sr
  ti = ti + si
  its = its + 1
  !     :::::::::: look for two consecutive small
  !                sub-diagonal elements ::::::::::
  xr = absz(hr(ienm1,ienm1)) + absz(hi(ienm1,ienm1))
  yr = absz(hr(ien,ienm1)) + absz(hi(ien,ienm1))
  zzr = absz(hr(ien,ien)) + absz(hi(ien,ien))
  if ( iprsup .ge. 3 ) write (lunit6, 370) xr, yr, zzr
370 format (' xr, yr, and zzr at 370 are', 3e15.6)
  !     :::::::::: for m=ien-1 step -1 until l do -- ::::::::::
  do mm = l, ienm1
     m = ienm1 + l - mm
     if (m .eq. l) go to 420
     yi = yr
     yr = absz(hr(m,m-1)) + absz(hi(m,m-1))
     xi = zzr
     zzr = xr
     xr = absz(hr(m-1,m-1)) + absz(hi(m-1,m-1))
     if ( iprsup .ge. 3 ) write (lunit6, 375) mm, yr, zzr, yi, xr, xi, epmach
375  format (' at 375, mm, yr, zzr, yi, xr, xi and epmach are', /, 10x, i8, 6e15.6)
     if (yr .le. epmach * zzr / yi * (zzr + xr + xi)) go to 420
  end do
  !     :::::::::: triangular decomposition h=l*r ::::::::::
420 mp1 = m + 1
  do i = mp1, ien
     im1 = i - 1
     xr = hr(im1,im1)
     xi = hi(im1,im1)
     yr = hr(i,im1)
     yi = hi(i,im1)
     if (absz(xr) + absz(xi) .ge. absz(yr) + absz(yi)) go to 460
     !     :::::::::: interchange rows of hr and hi ::::::::::
     do j = im1, n
        zzr = hr(im1,j)
        hr(im1,j) = hr(i,j)
        hr(i,j) = zzr
        zzi = hi(im1,j)
        hi(im1,j) = hi(i,j)
        hi(i,j) = zzi
     end do
     !     it=iord(i)
     ittemp = iord(i)
     iord(i) = iord(im1)
     !     iord(im1) = it
     iord(im1) = ittemp
     d1 = yr ** 2 + yi ** 2
     d2 = ( xr*yr + xi*yi ) / d1
     d3 = ( yr*xi - xr*yi ) / d1
     wr(i) = 1.0
     go to 480
460  d1 = xr ** 2 + xi ** 2
     d2 = ( yr*xr + yi*xi ) / d1
     d3 = ( xr*yi - yr*xi ) / d1
     wr(i) = -1.0
480  zzr = d2
     zzi = d3
     hr(i,im1) = zzr
     hi(i,im1) = zzi
     do j = i, n
        hr(i,j) = hr(i,j) - zzr * hr(im1,j) + zzi * hi(im1,j)
        hi(i,j) = hi(i,j) - zzr * hi(im1,j) - zzi * hr(im1,j)
     end do
     if ( iprsup .ge. 3 ) write (lunit6, 510) i,n, (hr(i,j), hi(i,j), j=1,n)
510  format (' At 510, (hr(i, j), hi(i, j), j = 1, n) for i =', i8, ', and n = ', i8, 2x, 'are', /, (1x, 8e15.6))
  end do
  !     :::::::::: composition r*l=h ::::::::::
  do j = mp1, ien
     xr = hr(j,j-1)
     xi = hi(j,j-1)
     hr(j,j-1) = 0.0
     hi(j,j-1) = 0.0
     !     :::::::::: interchange columns of hr, hi, zr, and zi,
     !                if necessary ::::::::::
     if (wr(j) .le. 0.0) go to 580
     do i = 1, j
        zzr = hr(i,j-1)
        hr(i,j-1) = hr(i,j)
        hr(i,j) = zzr
        zzi = hi(i,j-1)
        hi(i,j-1) = hi(i,j)
        hi(i,j) = zzi
     end do
     do i = low, igh
        zzr = zr(i,j-1)
        zr(i,j-1) = zr(i,j)
        zr(i,j) = zzr
        zzi = zi(i,j-1)
        zi(i,j-1) = zi(i,j)
        zi(i,j) = zzi
     end do
580  do i = 1, j
        hr(i,j-1) = hr(i,j-1) + xr * hr(i,j) - xi * hi(i,j)
        hi(i,j-1) = hi(i,j-1) + xr * hi(i,j) + xi * hr(i,j)
     end do
     !     :::::::::: accumulate transformations ::::::::::
     do i = low, igh
        zr(i,j-1) = zr(i,j-1) + xr * zr(i,j) - xi * zi(i,j)
        zi(i,j-1) = zi(i,j-1) + xr * zi(i,j) + xi * zr(i,j)
     end do
     if ( iprsup .ge. 3 ) write ( lunit6, 630) j, low, igh, (hr(i,j),hi(i,j),i=1,j), (zr(i,j),zi(i,j),i=low,igh)
630  format (' j, low and igh at 630 are', 3i8, ' (hr(i, j), hi(i, j), i = 1, j) and (zr(i, j), zi(i, j), i = low, igh) are', /, (1x, 8e15.6))
  end do
  go to 240
  !     :::::::::: a root found ::::::::::
660 hr(ien,ien) = hr(ien,ien) + tr
  wr(ien) = hr(ien,ien)
  hi(ien,ien) = hi(ien,ien) + ti
  wi(ien) = hi(ien,ien)
  ien = ienm1
  if ( iprsup .ge. 3 ) write (lunit6, 670) ien,hr(ien,ien),hi(ien,ien),wr(ien),wi(ien)
670 format ('     ien    hr(ien, ien)    hi(ien, ien)       wr(ien)      wi(ien)', /, i8, 4e15.6)
  go to 220
680 continue
  do id=low,igh
     j = iord(id)
     umr(j) = wr(id)
     umi(j) = wi(id)
  end do
  !     :::::::::: all roots found.  backsubstitute to find
  !                vectors of upper triangular form ::::::::::
  fnorm = 0.0
  do i = 1, n
     do j = i, n
        fnorm = fnorm + absz(hr(i,j)) + absz(hi(i,j))
     end do
  end do
  hr(1,1) = fnorm
  if (n .eq. 1 .or. fnorm .eq. 0.0) go to 1001
  !     :::::::::: for ien=n step -1 until 2 do -- ::::::::::
  do nn = 2, n
     ien = n + 2 - nn
     xr = wr(ien)
     xi = wi(ien)
     ienm1 = ien - 1
     !     :::::::::: for i=ien-1 step -1 until 1 do -- ::::::::::
     do ii = 1, ienm1
        i = ien - ii
        zzr = hr(i,ien)
        zzi = hi(i,ien)
        if (i .eq. ienm1) go to 760
        ip1 = i + 1
        do j = ip1, ienm1
           zzr = zzr + hr(i,j) * hr(j,ien) - hi(i,j) * hi(j,ien)
           zzi = zzi + hr(i,j) * hi(j,ien) + hi(i,j) * hr(j,ien)
        end do
760     yr = xr - wr(i)
        yi = xi - wi(i)
        if (yr .eq. 0.0 .and. yi .eq. 0.0) yr = epmach * fnorm
        d1 = yr ** 2 + yi ** 2
        d2 = ( zzr*yr + zzi*yi ) / d1
        d3 = ( yr*zzi - yi*zzr ) / d1
        hr(i,ien) = d2
        hi(i,ien) = d3
        if ( iprsup .ge. 3 ) write (lunit6, 770)  nn,n,ien,ii,i,ienm1,zzr,zzi,yr,yi,d2,d3, hr(i,ien),hi(i,ien)
770     format (' nn, n, ien, ii, i, ienm1, zzr, zzi, yr, yi, d2, d3, hr(i, ien) and hi(i,ien) at 770 are', /, 1x, 6i5, 8e12.5)
     end do
  end do
  !     :::::::::: end backsubstitution ::::::::::
  ienm1 = n - 1
  !     :::::::::: vectors of isolated roots ::::::::::
  do i = 1, ienm1
     if (i .ge. low .and. i .le. igh) go to 840
     ip1 = i + 1
     do j = ip1, n
        zr(i,j) = hr(i,j)
        zi(i,j) = hi(i,j)
     end do
  end do
840 continue
  !     :::::::::: multiply by transformation matrix to give
  !                vectors of original full matrix.
  !                for j=n step -1 until low+1 do -- ::::::::::
  do jj = low, ienm1
     j = n + low - jj
     !        m = min0z(j-1,igh)
     m = j - 1
     if ( m  .gt.  igh )  m = igh
     do i = low, igh
        zzr = zr(i,j)
        zzi = zi(i,j)
        do k = low, m
           zzr = zzr + zr(i,k) * hr(k,j) - zi(i,k) * hi(k,j)
           zzi = zzi + zr(i,k) * hi(k,j) + zi(i,k) * hr(k,j)
        end do
        zr(i,j) = zzr
        zi(i,j) = zzi
        if (iprsup .ge. 3 ) write (lunit6, 870) jj, j, m, zr(i,j),zi(i,j)
870     format (' At the end of do 880 loop, jj, j,  m, zr(i, j) and zi(i, j) are', 5x, 3i8, 2e15.6)
     end do
  end do
  go to 1001
  !     :::::::::: set error -- no convergence to an
  !                eigenvalue after 30 iterations ::::::::::
1000 ierr = ien
1001 continue
!!!!      do 1002 i=low, igh
!!!!      wr(i) = umr(i)
!!!!      wi(i) = umi(i)
  !!     ** sorting the eigenvalues with magnitude **
  !!        also reordering the eigenvector
  do i = 1, 20
     lseq(i) = i
  end do
  do i = 1, n
     vtr(i) = wr(i)
     vti(i) = wi(i)
     do j = 1, n
        ppr(i, j) = zr(i, j)
        ppi(i, j) = zi(i, j)
     end do
  end do
  do i=1,n
     eim(i) = wr(i)*wr(i) + wi(i)*wi(i)
  end do
  do j = 1, n
     l = n - j
     do i = 1, l
        if (eim(i) .ge. eim(i + 1)) go to 1007
        t = eim(i)
        eim(i) = eim(i + 1)
        eim(i + 1) = t
        t = lseq(i)
        lseq(i) = lseq(i + 1)
        lseq(i + 1) = int (t)
     end do
1007 continue
  end do
  do i = 1, n
     wr(i) = vtr(lseq(i))
     wi(i) = vti(lseq(i))
     do j = 1, n
        zr(i, j) = ppr(i, lseq(j))
        zi(i, j) = ppi(i, lseq(j))
     end do
  end do
  return
  !     :::::::::: last card of comlr2 ::::::::::
end subroutine comlr2

!
! subroutine symn.
!

subroutine symm (p, z, switch, kcirct, kk)
  use blkcom
!  use com44
  implicit none
  integer(4), intent(in) :: kcirct
  integer(4), intent(out) :: kk
  real(8), intent(out) :: p(9)
  real(8), intent(in) :: switch
  real(8), intent(out) :: z(9)
  !  dimension p(9), z(9)
  !  dimension ar(3,3),ai(3,3),fr(3),fi(3)
  integer(4) :: i
  integer(4) :: j
  integer(4) :: k, ki, kold
  integer(4) :: l, l2, l3
  integer(4) :: m
  real(8) :: ai(3, 3), ar(3, 3)
  real(8) :: f1, f2, fi(3), fr(3)
  real(8) :: valu7
  !
  if (kcirct .eq. 2) go to 100
  fr(1) = unity
  fi(1) = 0.0d0
  fr(2) = -onehaf
  fi(2) = valu7
  fr(3) = -onehaf
  fi(3) = -fi(2)
  kk = 0
63 ki = kk * (kk + 1) / 2
  j = ki + kk
  kold = kk
  kk = kk + 3
  if (kk .gt. kcirct) go to 79
75 l = ki + 1
  k = 0
64 k = k + 1
  if (ki .eq. j) go to 76
65 l3 = l + 2
66 l2 = l + 1
67 do i = 1, 3
     f1 = p(l) + fr(i) * (p(l2) + p(l3))
     f2 = fi(i) * (p(l2) - p(l3))
     if (switch .lt. 0.0d0) go to 68
     f1 = f1 + fi(i) * (z(l3) - z(l2))
     f2 = f2 + z(l) + fr(i) * (z(l2) + z(l3))
68   ar(i, k) = f1
     ai(i, k) = f2
  end do
  if (k .eq. 3) go to 69
  l = l + kold + k
  go to 64
69 l = ki
  k = 0
70 k = k + 1
  do i = 1, 3
     m = l + i
     if (ki .lt. j) go to 71
     if (i .gt. k) go to 73
71   p(m) = (ar(i, 1) + fr(k) * (ar(i, 2) + ar(i, 3)) + fi(k) * (ai(i, 3) - ai(i, 2))) / 3.0d0
     z(m) = (ai(i, 1) + fr(k) * (ai(i, 2) + ai(i, 3)) + fi(k) * (ar(i, 2) - ar(i, 3))) / 3.0d0
  end do
73 if (k .eq. 3) go to 74
  l = l + kold + k
  go to 70
74 ki = ki + 3
  if (ki .gt. j) go to 63
  go to 75
76 if (k .eq. 3) go to 65
  l2 = j + kk - 1
  l3 = l2 + kk
  if (k .eq. 2) go to 66
  l3 = l3 - 1
  go to 67
79 kk = kk - 3
  !                                   end of symmetrical components matrix
  return
100 f1 = (p(1) + p(3)) * onehaf
  f2 = p(2)
  p(2) = (p(1) - p(3)) * onehaf
  p(1) = f1 + f2
  p(3) = f1 - f2
  p(5) = p(3)
  kk = 2
  if (switch .lt. 0.0d0) return
  f1 = (z(1) + z(3)) * onehaf
  f2 = z(2)
  z(2) = (z(1) - z(3)) * onehaf
  z(1) = f1 + f2
  z(3) = f1 - f2
  z(5) = z(3)
  return
end subroutine symm

!
! subroutine skin.
!

subroutine skin (s, r, freq, rf, xf)
  use blkcom
  use com44
  use tracom
  implicit none
  real(8), intent(out) :: rf
  real(8), intent(out) :: xf
  real(8), intent(in) :: freq
  real(8), intent(in) :: r
  real(8), intent(in) :: s
  integer(4) :: ialt, iback
  integer(4) :: jjj
  integer(4) :: k
  real(8) :: a, aremb
  real(8) :: b, bei, beid, ber, berd, bremb
  real(8) :: e, e2f2
  real(8) :: f, fi, fr
  real(8) :: g, gei, geid, ger, gerd, gi, gr
  real(8) :: h
  real(8) :: phii, phir
  real(8) :: q2, qremb
  real(8) :: r2
  real(8) :: s2, s3
  real(8) :: thetai, thetar
  real(8) :: x, x2, xl, xremb
  real(8) :: z
  double precision :: vsmall
  !
  vsmall = 1.e-37
  call undrfl (jjj)
  s2 = s * s
  s3 = (unity - s2) * r
  r2 = freq * valu8 / s3
  rf = r
  xf = 0.0d0
  if (r2 .eq. 0.0d0) go to 9900
  qremb = 0.0d0
  if (s .lt. tenm6) go to 5
  q2 = r2 * s2
  if (s2 .lt. 0.8d0) go to 11
  if (q2 .le. 64.0d0 .and. r2 .gt. 64.0d0) write (unit = lunit6, fmt = 10) q2, r2
10 format (' Results from subroutine skin unreliable with mq**2= ', f9.4, ' and mr**2= ', f9.4)
11 if (q2 .gt. 64.0) qremb=sqrtz(q2) * sqrt2
  x = sqrtz (q2)
  x2 = x * x / 64.0d0
  iback = 2
  if (x2.le.unity) go to 100
  go to 200
4 a = - berd
  b = - beid
  aremb = gerd
  bremb = geid
  xremb = x
5 x = sqrtz(r2)
  x2=x*x/64.0
  iback = 1
  if (x2.le.unity) go to 100
  go to 200
6 g = ber
  h = bei
  e = berd
  f = beid
  if ( s  .lt.  tenm6 )   go to 7
  g = a*ger - b*gei + aremb*ber - bremb *bei
  h = a*gei + b*ger + aremb*bei + bremb*ber
  e = a*gerd - b*geid + aremb*berd - bremb*beid
  f = a*geid + b*gerd + aremb*beid + bremb*berd
7 e2f2 = e**2 + f**2
  s2 =  x * s3 * onehaf / e2f2
  if (iprsup .ge. 1) write (lunit6, 320)  e, f, e2f2, s2
320 format (' e, f,  e2f2, and s2 at 320', 4e16.6)
  rf = (-h*e+g*f)*s2
  xf = (g*e+h*f)*s2
  go to 9900
  !      calculation of kelvin-functions for argument.le.8.
100 z = x2
  ber = unity
  bei = 0.0
  berd = 0.0
  beid = onehaf
  gerd = 0.0
  geid =  valu9
  ger = - valu10
  gei = 0.0
  ialt = 1
  do k=1,14
     if (ialt.eq.1) go to 101
     ber = ber+fbe(k)*z
     beid = beid+fbed(k)*z
     if ( s  .lt.  tenm6 )   go to 102
     geid = geid+fked(k)*z
     if (iback.eq.2) go to 102
     ger = ger+fke(k)*z
     go to 102
101  bei = bei+fbe(k)*z
     berd = berd+fbed(k)*z
     if ( s  .lt.  tenm6 )   go to 102
     gerd = gerd+fked(k)*z
     if (iback.eq.2) go to 102
     gei = gei+fke(k)*z
102  z = z*x2
     ialt = -ialt
  end do
  beid = beid*x
  berd = berd*x
  if ( s  .lt.  tenm6 )   go to 104
  xl = alogz(x*onehaf )
  gerd = -xl*berd-ber/x+beid* aaa1 +x*gerd
  geid = -xl*beid-bei/x-berd* aaa1 +x*geid
  if (iback.eq.2) go to 104
  ger = -xl*ber+bei* aaa1            +ger
  gei = -xl*bei-ber* aaa1            +gei
104 go to (6 , 4 ),iback
  !      calculations of kelvin-functions for argument .gt. 8.
200 x2 = 8.0  /x
  z = x2
  ber = 0.0
  bei = - valu11
  berd = ber
  beid = bei
  ger = unity / sqrt2
  gei = ger
  gerd = ger
  geid = gei
  ialt = 1
  do k=1, 6
     thetar = fbe(k+14) * z
     thetai = fbed(k+14)*z
     phir = fke(k+14)*z
     phii = fked(k+14)*z
     ber = ber+thetar
     bei = bei+thetai
     ger = ger+phir
     gei = gei+phii
     if (ialt.eq.1) go to 201
     berd = berd+thetar
     beid = beid+thetai
     gerd = gerd+phir
     geid = geid+phii
     go to 202
201  berd = berd-thetar
     beid = beid-thetai
     gerd = gerd-phir
     geid = geid-phii
202  ialt = -ialt
     z = z*x2
  end do
  xl=x* sqrt2
  if (qremb .lt. 1.0)  go to 204
  xl = xl - qremb
204 thetar = - xl + berd
  thetai = -xl+beid
  z = sqrtz(x)
  x2 =  valu12 / z
  z = valu13 / z* expz(thetar)
  fr = z* cosz(thetai)
  fi = z* sinz(thetai)
  x2=x2* expz(ber)
  thetar=x2* cosz(bei)
  thetai=x2* sinz(bei)
  z = -fr*gerd+fi*geid
  geid = -fr*geid-fi*gerd
  gerd = z
  z = aaa2 * expz(-qremb)
  gr = z*sinz(qremb)
  gi = z*cosz(qremb)
  berd = thetar*ger - thetai*gei + gerd*gr - geid*gi
  beid = thetar*gei + thetai*ger + gerd*gi + geid*gr
  ger = fr
  gei = fi
  ber = thetar + ger*gr - gei*gi
  bei = thetai + ger*gi + gei*gr
  go to ( 6 , 4 ),iback
9900 continue
  if ( jjj .eq. 3 ) z = vsmall
  return
end subroutine skin

!
! subroutine undrfl.
!

subroutine undrfl (n1)
  implicit none
  !  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     dummy imitation of Ontario Hydro UNIVAC module.
  integer(4), intent(out) :: n1
  n1 = -7654
  return
end subroutine undrfl

!
! subroutine outspc.
!

subroutine outspc (p, z, kmax, metrik, fmipkm)
  use blkcom
  use com44
  use tracom
  implicit none
  integer(4), intent(in) :: kmax
  integer(4), intent(in) :: metrik
  real(8), intent(in) :: fmipkm
  real(8), intent(in) :: p(9)
  real(8), intent(in) :: z(9)
  !  dimension p(9), z(9)
  real(8) :: a0, a1, a2, aa
  real(8) :: b0, b1, b2, bb
  real(8) :: c0, c1, c2
  !
  if (kmax .eq. 4) go to 999
  write (unit = lunit6, fmt = 222)
222 format (/, ' Special output for mutuals not applicable to this case')
  return
999 aa = valu7 * (z(8) - z(9))
  bb = -valu7 * (p(8) - p(9))
  a2 = p(7) - onehaf * (p(8) + p(9))
  b2 = z(7) - onehaf * (z(8) + z(9))
  a1 = a2 + aa
  b1 = b2 + bb
  a2 = a2 - aa
  b2 = b2 - bb
  a0 = p(7) + p(8) + p(9)
  b0 = z(7) + z(8) + z(9)
  c0 = sqrtz (a0 ** 2 + b0 ** 2)
  c1 = sqrtz (a1 ** 2 + b1 ** 2)
  c2 = sqrtz (a2 ** 2 + b2 ** 2)
  if (metrik .eq. 1) go to 1120
  write (unit = lunit6, fmt = 1111) c1, c2, c0
1111 format (' Mutual impedance  positive= ', f8.5, ' Ohm/mile  negative= ', f8.5, ' Ohm/mile  zero= ', f8.4, ' Ohm/mile')
  return
1120 c1 = c1 * fmipkm
  c2 = c2 * fmipkm
  c0 = c0 * fmipkm
  write (unit = lunit6, fmt = 1121) c1, c2, c0
1121 format (' Mutual impedance  positive= ', f8.5, ' Ohm/km    negative= ', f8.5, ' Ohm/km    zero= ', f8.4, ' Ohm/km')
  return
end subroutine outspc

!
! end of file over44.f90
!
