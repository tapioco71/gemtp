!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file random.f90
!

module random
  use blkcom
  implicit none

contains
  !
  ! function seedy.
  !
  
  function seedy (atim) result(s)
    implicit none
    !
    !     This function is designed to take the time of day (wall-clock
    !     time) in bcd form as input, and return the number of seconds
    !     since midnight as output.   the time of day is assumed to be
    !     coded in words   atim(1)   and   atim(2) ,   as found by a call to
    !     subroutine  'time44' .   The storage here is in format   2a4 ,
    !     with the first four characters of   'hh.mm.ss'   assumed to be
    !     in  atim(1) ,  and the last four in  atim(2) .
    !
    character(8), intent(in) :: atim(2)
    integer(4) :: amin, hour, ihr, imin, imin1, imin10, isec, sec
    real(8) :: s
    !
    read (unit = atim, fmt = 4286) ihr, imin10, imin1, isec
4286 format (i2, 1x, i1, 4x, i1, 1x, i2)
    imin = imin10 * 10 + imin1
    hour = ihr * 3600
    amin = imin * 60
    sec = isec
    s = sec + amin + hour + 1.0
  end function seedy

  !
  ! function sandnm.
  !
  
  function sandnm (x) result(s)
    implicit none
    !     This version of  'randnm'  is used for testing of the
    !     statistical overvoltage capability of the emtp only.   It uses
    !     built-in random numbers, so as to produce identical results
    !     on all computer systems, thereby permitting comparative
    !     verification of results exactly as for conventional test cases.
    !     In order to avoid re-use of random numbers, the number of
    !     energizations  'nenerg'  times the number of switches  'kswtch'
    !     must not exceed the dimension of array  'a' .
    !     If  'statistics'  miscellaneous data parameter  'xmaxmx'
    !     is input by the user with negative value, the emtp takes
    !     this to mean that "sandnm" is to be used for random
    !     numbers rather than "randnm" .
    !  include 'blkcom.ftn'
    real(8), intent(in) :: x
    integer(4) :: l, n1
    real(8) :: a(100), s
    !  equivalence (moncar(1), knt)
    !
    !     Burroughs: preserve local variable between module calls:
    data l / 0 /
    !     beginning of assignment of random numbers to array  'a' .
    a(  1) =  .1445312506618
    a(  2) =  .8477795260409
    a(  3) =  .8267723125831
    a(  4) =  .6660710406131
    a(  5) =  .7152322826372
    a(  6) =  .3239128029543
    a(  7) =  .5051959208554
    a(  8) =  .3805491872180
    a(  9) =  .5609474043286
    a( 10) =  .5996361115942
    a( 11) =  .4159594349328
    a( 12) =  .6756609755246
    a( 13) =  .0995378032610
    a( 14) =  .6033780421273
    a( 15) =  .4515533431030
    a( 16) =  .0020932062778
    a( 17) =  .9161858062074
    a( 18) =  .3434229008090
    a( 19) =  .7876940045781
    a( 20) =  .2760908032985
    a( 21) =  .3665660303205
    a( 22) =  .8204300122029
    a( 23) =  .2413831265551
    a( 24) =  .1246653115746
    a( 25) =  .2802441882439
    a( 26) =  .0466535013838
    a( 27) =  .4742772736449
    a( 28) =  .9477027545777
    a( 29) =  .2260144770748
    a( 30) =  .2987460629005
    a( 31) =  .5203589181526
    a( 32) =  .8981967037721
    a( 33) =  .3873885562436
    a( 34) =  .5780913804991
    a( 35) =  .1280852320759
    a( 36) =  .3327754064471
    a( 37) =  .4043867414456
    a( 38) =  .9490362532099
    a( 39) =  .6261391859471
    a( 40) =  .3291406705415
    a( 41) =  .3366864607083
    a( 42) =  .9438413593777
    a( 43) =  .9369008057740
    a( 44) =  .0713971670442
    a( 45) =  .6500854844946
    a( 46) =  .9737952005663
    a( 47) =  .6485758973471
    a( 48) =  .7724301318424
    a( 49) =  .9676692044394
    a( 50) =  .5163953619955
    a( 51) =  .5788464270596
    a( 52) =  .7758933795560
    a( 53) =  .0910635448877
    a( 54) =  .0439510552688
    a( 55) =  .0707223001462
    a( 56) =  .9379043319315
    a( 57) =  .0052391978463
    a( 58) =  .9420572226295
    a( 59) =  .5932597508799
    a( 60) =  .6466146627873
    a( 61) =  .4395400252824
    a( 62) =  .1972895298303
    a( 63) =  .5017482047726
    a( 64) =  .1503404202877
    a( 65) =  .9624699228977
    a( 66) =  .0098276069324
    a( 67) =  .6571365402082
    a( 68) =  .4233003554891
    a( 69) =  .1203194365765
    a( 70) =  .7436871629477
    a( 71) =  .8136524161969
    a( 72) =  .7311319136405
    a( 73) =  .0594772166524
    a( 74) =  .2374863512189
    a( 75) =  .2450459940689
    a( 76) =  .4326371816340
    a( 77) =  .3562832359564
    a( 78) =  .3723442477773
    a( 79) =  .1694432139356
    a( 80) =  .3735622812899
    a( 81) =  .0610718353086
    a( 82) =  .2782657746530
    a( 83) =  .5137050416289
    a( 84) =  .4340395038268
    a( 85) =  .5766543446808
    a( 86) =  .4413641042052
    a( 87) =  .9812390285872
    a( 88) =  .2625281459037
    a( 89) =  .9554345097074
    a( 90) =  .4741647690234
    a( 91) =  .9906793757886
    a( 92) =  .7820837820369
    a( 93) =  .2206664815389
    a( 94) =  .0901816247992
    a( 95) =  .7625227133400
    a( 96) =  .4434728419824
    a( 97) =  .7905859532294
    a( 98) =  .9796097207935
    a( 99) =  .7599602497147
    a(100) =  .1154361048406
    !     end       of assignment of random numbers to array  'a' .
    if (x .eq. 0.0) go to 2614
    l = (knt - 1) * kswtch + 1
    go to 2632
2614 l = l + 1
2632 n1 = (l - 1) / 100
    if (iprsup .ge. 1) write (unit = lunit6, fmt = 2645) l, knt, kswtch, n1, x
2645 format (/, " Variables in  'sandnm' ,   the random-number generator with 100 built-in numbers.       l     knt  kswtch      n1 ", 14x, 'x', /, 82x, 4i8, e15.5)
    if (n1 .gt. 0) l = l - 100 * n1
    s = a(l)
  end function sandnm

  !
  ! function randnm.
  !

  function randnm (x) result(r)
    implicit none
    !
    !     This is a random number generating function, whose purpose is to
    !     return with a value for a random variable which is uniformly-
    !     distributed over the interval from zero to unity.
    !     A typical call is    y = randnm(x)  ,    where  'x'  is a
    !     floating-point variable which is present only because 'random'
    !     number-generating algorithms are never truly random, and generally
    !     require some sort of 'random' initialization to even get started.
    !     Since such number-generating algorithms are actually cyclic if a
    !     large enough sampling is taken, it is also desirable to
    !     re-initialize the algorithm periodically, by means of a 'random'
    !     external input.   Variable  'x'  is this 'random' input, whose
    !     purpose is to randomly re-initialize the algorithm, as follows ...
    !          1.  if  'x'  is identically zero, there is no initialization.
    !              the next random number is simply returned through the
    !              function name.
    !          2.  if  'x'  is positive, the random number generating
    !              algorithm is to be initialized.   emtp usage has  'x'
    !              as the wall-clock time in seconds since midnight, an
    !              integer (though stored in floating-point mode, note).
    !              in this case, no random number need be returned with the
    !              function name, since the emtp will not use it.
    !     if a non-cdc user has access to a random number generating
    !     function which does not require initialization, he may simply
    !     ignore  'x' ,   and return the random number through the function
    !     name every time this module is called.
    !
    !     A minus sign appended to variable  'xmaxmx'  of   /blank/  is a
    !     flag that the user wants to employ the standard table of random
    !     numbers which is built into module  'sandnm' .
    !
    !     Installation-dependent  EMTP  module written for the  dec
    !     VAX-11/780.    'ran'  is a  dec  system subroutine which
    !     returns a random number uniformly distributed over  (0, 1) .
    !  include 'blkcom.ftn'
    real(8), intent(in) :: x
    integer(4) :: n14
    real(8) :: r
    !  equivalence (moncar(1), knt)
    !
    if (xmaxmx .lt. 0.0) go to 7265
    if (x .eq. 0.0) go to 4213
    if (knt .gt. 1) go to 9800                                ! skip 2nd or later seed
    n14 = int (x, kind (n14))
    if (n14 / 2 * 2 .eq. n14) n14 = n14 + 1
4213 r = ran(n14)                                             ! 29 dec 1987, change from  (n1, n2)
    go to 9800
7265 r = sandnm (x)
9800 continue
  end function randnm

end module random
