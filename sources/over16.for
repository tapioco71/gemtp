!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: over16.for
!
!
!     subroutine over16.
!
subroutine over16
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'umdeck.ftn'
  include 'tacsar.ftn'
  dimension  xx(1)
  equivalence (  xk(1), xx(1) )
  dimension  ispum(1)
  equivalence  ( spum(1), ispum(1) )
  if ( iprsup  .ge.  1 ) write ( lunit6, 4567 )
4567 format (24h  begin module "over16". )
2001 if ( numum   .eq.  0 )   go to 2450
  if ( lastov  .eq.  1 )   go to 2450
  if ( lastov  .eq. 20 )   go to 2450
  call solvum ( spum(iureac), spum(iugpar), spum(iufpar), spum(iuhist), spum(iuumrp), &
       ispum(iunod1), ispum(iunod2), ispum(iujclt), ispum(iujclo), &
       ispum(iujtyp), ispum(iunodo), ispum(iujtmt), spum(iuhism), &
       spum(iuomgm), spum(iuomld), spum(iutham), spum(iuredu), &
       spum(iureds), spum(iuflds), spum(iufldr), spum(iurequ), &
       spum(iuflqs), spum(iuflqr), ispum(iujcds), ispum(iujcqs), &
       spum(iuflxd), spum(iuflxq), ispum(iunppa), spum(iurotm), &
       ispum(iuncld), ispum(iunclq), ispum(iujtqo), ispum(iujomo), &
       ispum(iujtho), spum(iureqs), spum(iuepso), spum(iudcoe), &
       ispum(iukcoi), spum(iuvolt), spum(iuangl), ispum(iunodf), &
       ispum(iunodm), ispum(iukumo), ispum(iujumo), spum(iuumou))
2450 if ( kill  .eq.  0 )   go to 2468
  nchain = 51
  if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )  kill
4568 format ( 32h exit module  "over16".   kill =,  i5  )
  go to 99999
2468 if ( m4plot .eq. 1 )  call emtspy
  n1 = nchain - 15
  iprsup = iprsov(nchain)
  if ( n1  .gt.  4 )   go to 99999
  if ( n1  .le.  0 )   go to 99999
  go to  (3016, 3017, 3018, 3019), n1
3016 call subts1
  go to 2450
3017 call subts2
  go to 2450
3018 call subts3
  go to 2450
3019 call subts4
  go to 2450
99999 return
end subroutine over16
!
!     subroutine subts1.
!
subroutine subts1
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'   ! wsm + thl
  include 'labcom.ftn'
  include 'tacsar.ftn'
  include 'umdeck.ftn'
  include 'dekspy.ftn'
  common /a8sw/ a8sw(400)
  dimension nsubkm(1), ispum(1), swcold(100)
  equivalence (spum(1),  ispum(1)),  (kknonl(1),  nsubkm(1))
  equivalence (moncar(1),    knt),       (moncar(2),  kbase)
  equivalence (iprsov(35), ipoint),  (iprsov(36), iupper)
  character*8 text1, text2, text3, text4, text5, text6
  character*8 text7, text8, text9
  character*8 text10, text11, text12, text13
  data  text1   / 6hvalve  /
  data  text2   / 6hdiode  /
  data  text3   / 6hopenin /
  data  text4   / 6hclosin /
  data  text5   / 6hgap    /
  data  text6   / 6hswitch /
  data  text8   / 6hclosed /
  data  text9   / 6h open  /
  data  text10   /  6hspy     /
  data  text11   /  6hpass    /
  data  text12   /  6hstop    /
  data  text13   /  6hykk     /
  !     burroughs: preserve local variable between module calls:
  data  nwarn   /   0   /
  if ( iprsup  .ge.  1 ) write (lunit6, 548)  istep, isprin, isplot, kswtch, inonl, num99, iupper, knt, nenerg
548 format ( ' begin 1st piece of time-step loop.   istep  isprin', '  isplot  kswtch   inonl   num99  iupper     knt  nenerg', &
       /, 35x, 10i8 )
  if ( numsm  .gt.  0 ) call update
  if ( kswtch .le. 0 ) kcount = nv
1000 if ( kswtch .le. 0 ) go to 1009
  if ( istep .gt. 0 )   go to 3780
  !     add code for random opening with current margin (from over5):
  ndx1 = lswtch + 1
  do j=1, kswtch
     ndx1 = ndx1 - 1
     if ( crit(ndx1) .eq. 0.0 )  go to 3756
     if ( iprsup .ge. 1 ) write (lunit6, 3752)  j, crit(j), crit(ndx1), topen(j)
3752 format ( 23h random open, set crit., 36h  j, crit(j), crit(ndx1), topen(j) =, i6, 3e15.5  )
     crit(j) = crit(ndx1)
3756 end do
3780 continue
  !                                   checking switch-positions for change
  if ( iprsup  .ge.  3 ) write (lunit6, 8253)  lastov, kbase, ntot, kpartb, ncomp, t, tenerg, deltat
8253 format ( /, 39h more scalars.  lastov,   kbase    ntot, 16h  kpartb   ncomp,  14x,  1ht, 9x, 6htenerg, &
       9x,  6hdeltat  ,/,  14x,  5i8,  3e15.6  )
  if ( iprsup  .lt.  4 )  go to 1326
  write (lunit6, 1314)
1314 format ( /, 39h switch table at beginning of 'subts1'. ,/,40h     row    kpos    bus1    bus2  kswtyp, &
       9x, 6htclose, 9x, 6hadelay, 10x, 5htopen, 11x, 4hcrit )
  do k = 1, kswtch
     ndx1 = lswtch + k
3333 write (lunit6, 4444)  k, kpos(k), kmswit(k), kmswit(ndx1), kswtyp(k), tclose(k), adelay(k), topen(k), crit(k)
  end do
4444 format  ( 5i8,  4e15.6 )
1326 if ( istep .le. 0 )  go to 800
  n19 = 0
  n18 = 0
  k = ktrlsw(4)
  if ( k .eq. 0 )  go to 1216
1214 ii = kpos(k)
  it1 = kswtyp(k)
  i=iabs(ii)
  vsl=0.
  if ( i .eq. 5) go to 1002
  if ( i .eq. 0) go to 1002
  ndx1 = lswtch + k
  n1 = kmswit(k)
  n2 = kmswit(ndx1)
  tcl=tclose(k)
  nn1=n1
  if (it1 .eq. 9976)  go to 3718
  if ( it1 .gt. 0 ) nn1 = kbus(it1)
  isss = isourc(k)
  if ( isss .gt. 0 ) nn1=-node(isss+1)
3718 if ( i  .eq.  10 )  go to 2100
  if (  i .eq. 11 )   go to 2108
  if ( i .eq. 4 )     go to 2103
  if (it1 .eq. 9976)  go to 2110
  if ( it1 .le. 0 )   go to 2110
  gus4 = (emtpe(nn1)-emtpe(n2)) * delta2
  ci1 = ck(it1)
  ck1 = ci1 + gus4
  ck(it1)=ck1+gus4
  if ( absz(ck1)  .ge.  crit(k).or. absz(ck1) .gt. absz(ci1) )  go to 1002
  gus4 = gus4 / delta2
  iit1 = -nr(it1)
  ck1 = x(iit1) * gus4
  cik(it1+1)  =  ck1 + cik(it1)  +  cik(it1+1)
  cik(it1) = -ck1
  go to 2113
2103 ck1 = emtpe(n2) - emtpe(n1)
  a = (ck1+energy(k)) / 2.0
  energy(k) = ck1
  if ( a .lt. 0.0 )  go to 1002
  i = 3
  go to 2105
  !                          part of checking switch-positions for changes
2100 if ( t.lt.tcl      ) go to 1002
  if ( it1 .le. 0 ) go to 2101
  iit1 = -nr(it1)
  emtpe(n1) = emtpe(n2)
  gus4 = emtpe(nn1) - emtpe(n2)
  cik(it1) = -x(iit1) * gus4
  gus4 = gus4 * delta2
  ck1 = ck(it1)
  a = ck1 + gus4
  ck(it1) = a + gus4
  if ( absz(a) .lt. topen(k)  .or. absz(a) .lt. absz(ck1) ) go to 1002
  d2 = x(iit1+1) / delta2
  ci1 = cik(it1+1) + d2 * gus4
  ck1 =  x(iit1) / (delta2 * d2 )
  d3 = a * ( ck1+1.0 )  -  ci1 / d2
  crit(k) = absz( d3 / ck1 )
  go to 2102
2101 ck1 = emtpe(nn1) - emtpe(n2)
  if ( absz(ck1) .lt. topen(k)) go to 1002
  adelay(k) = t + adelay(ndx1)
  if (  isss .ne. 0 )  go to 8455
  if ( n2 .eq. 1 ) emtpe(nn1) = emtpe(n2)
  if ( n2 .ne. 1 ) emtpe(n2) = emtpe(nn1)
  go to 2102
8455 continue
  d4 = absz ( crest(isss) )
  if ( ck1 .lt. 0.0 ) d4 = -d4
  ck1 = d4
  crest(isss) = -ck1
  crest(isss+1) = ck1
  tstop(isss) = fltinf
  tstop(isss+1) = fltinf
  energy(k) = 0.
2102 i = 1
2105 tclose(k) = 0.
  if ( tcl .ge. 0. ) tcl=t
  if (nenerg .ne. 0 ) go to 620
  text7 = text8
  timswt = tcl
620 n19 = n19 + 1
  modswt(n19) = k
  go to 2115
2108 if ( t .lt. tcl ) go to 1002
  i = 2
  if ( topen(k)  .eq.  fltinf )  go to 2105
  !     if ( ii.gt.0 .and. topen(k) .gt. tmax) i=0                   ! dan
  go to 2105
2110 l = n1
  if (nextsw(k) .lt. 0 )  l = n2
  a = -emtpf(l)
  it2 = kks(l)
2111 it2 = it2 - 1
  j = km(it2)
  if ( j .lt. 0 ) go to 2116
  a =  a  +  ykm(it2) * emtpe(j)
  go to 2111
2116 j = iabs ( j )
  a =  a  +  ykm(it2) * emtpe(j)
  if (  iprsup .ge. 7 ) write (lunit6, 3407 )  k, bus(n1), bus(n2), l, nextsw(k), emtpf(l), a
3407 format ('    k    bus1    bus2    l  nextsw(k)', 7x, 'emtpf(l)', 10x, 'a', /, 2x, i3, 2(2x, a6), 2x, i3, i11, 2(2x, e15.5))
  if ( iprsup  .ge.  5 ) write (lunit6, 8007)  k,  l, ii, i, isss, it1, tclose(k), t, topen(k), crit(k), adelay(k)
8007 format ( /, 1x, 48h       k       l      ii       i    isss     it1, /, 1x, &
       6i8  ,/,  7x,  9htclose(k),  14x,  1ht,  7x,  8htopen(k), &
       8x,  7hcrit(k),  6x,  9hadelay(k)  ,/,  1x,  5e15.6 )
  n4 = n1
  if ( n4 .eq. l )  n4 = n2
  emtpf(n4) = emtpf(n4) - a
  if ( nextsw(k)  .gt.  0 )  a = -a
  gus1 = a*tclose(k)
  if ( gus1 .eq. 0.0  .and. tclose(k) .ne. 0.)  gus1 = -1.0
  tclose(k) = a
  if ( ii .le. 0 ) vsl = a
  if ( i .eq.  3 )   gus1 = -a
  if ( isss .le. 0 ) go to 2112
  gus4 = (emtpe(nn1) - emtpe(n2))*a
  energy(k)=energy(k) + gus4
  if ( gus4 .lt. 0 ) go to 2113
2112 if ( i .gt. 1 .and. t.lt.topen(k)) go to 1002
  if ( absz(a) .lt. crit(k) ) gus1=-1.0
  if ( t  .lt.  adelay(k) ) gus1 = 1.0
2117 if ( gus1 .ge. 0.) go to 1002
2113 i = i + 1
  if ( i .ne. 2 )  go to 2118
  i = 10
  if ( isss .eq. 0 ) go to 2118
  tstop(isss)  = 0.
  tstop(isss+1)  = 0.
  a = energy(k) * deltat
  if ( nenerg .ne. 0 )  go to 2118
  write (lunit6, 2119)  bus(nn1), bus(n2), a
2119 format ( 41h energy absorbed in switched resistance ',  a6, 6h' to ',    a6, 1h', e15.6 )
2118 tclose(k) = 0.
  if ( i .eq. 3 )  i=5
  energy(k) = 0.0
  n19 = n19 + 1
  modswt(n19) = -k
  if (nenerg .ne. 0)  go to 2115
  text7 = text9
  timswt = t
2115 if ( ialter .eq. 0 ) ialter = 1
  if (nenerg .ne. 0 )  go to 2107
  write (lunit6, 2106)  bus(nn1), bus(n2), text7, timswt
2106 format (  4h ***,  65x,  9hswitch  ",  a6,  8h"  to  ", a6,  3h"  ,  a6,  6h after,  e14.6,  5h sec.  )
2107 j = iabs( i )
  if ( ii .lt. 0) j = -j
  ii = j
  kpos(k) = ii
1002 if ( n18 .ne. 0 )  go to 1221
  k = iabs( nextsw(k) )
  if ( k .ne. ktrlsw(4) )  go to 1214
1216 n18 = 1
  k = 0
1221 k = k + 1
  if ( k .gt. kswtch ) go to 1227
  if ( nextsw(k) .eq. 0 )  go to 1214
  go to 1221
1227 kcount = nv
  do k=1, kswtch
     if ( kpos(k)  .ge.  0 )   go to 1239
     kcount = kcount + 1
     bvalue(kcount) = tclose(k)
     if ( nextsw(k) .eq. 0 ) bvalue(kcount) = 0.0
1239 end do
  !     If u.m. imitation of s.m. modeling (loopss(1)=6644),  then
  !     go back into solvum to pass machine quantities (including
  !     just-calculated torque in tclose of switch) to tacs:
1009 if (loopss(1)  .eq.  6644 ) call solvum ( spum(iureac),  &
          spum(iugpar), spum(iufpar), spum(iuhist), spum(iuumrp), &
          ispum(iunod1), ispum(iunod2), ispum(iujclt), ispum(iujclo), &
          ispum(iujtyp), ispum(iunodo), ispum(iujtmt), spum(iuhism), &
          spum(iuomgm), spum(iuomld), spum(iutham), spum(iuredu), &
          spum(iureds), spum(iuflds), spum(iufldr), spum(iurequ), &
          spum(iuflqs), spum(iuflqr), ispum(iujcds), ispum(iujcqs), &
          spum(iuflxd), spum(iuflxq), ispum(iunppa), spum(iurotm), &
          ispum(iuncld), ispum(iunclq), ispum(iujtqo), ispum(iujomo), &
          ispum(iujtho), spum(iureqs), spum(iuepso), spum(iudcoe), &
          ispum(iukcoi), spum(iuvolt), spum(iuangl), ispum(iunodf), &
          ispum(iunodm), ispum(iukumo), ispum(iujumo), spum(iuumou))
  if ( kill  .gt.  0 )  go to 9200
!!!!  write (*,*) ' tacs?  ktab, istep, newtac =',  ktab, istep, newtac
  if ( ktab .le. 0 .or. istep .le. 0 ) go to 3865
  if ( newtac .ne. 1 ) call tacs3
  if ( newtac .eq. 1 ) call ntacs3
  !     ---------------------------------------  diode, valve, gap  ------
  !     ------------------------------  and tacs-controlled switch  ------
3865 if ( kswtch  .le.  0 ) go to 800
  n13 = 0
  do i=1, kswtch
     k1 = kdepsw( lswtch + i )
     if ( k1 .eq. 8888  .or. k1 .eq. 8891 ) go to 4718
     if ( k1 .ne. 8890 )  go to 801
     n13 = n13 + 1
     if ( n13 .le. 100 ) go to 4718
     write (6, *) ' overflow triac storage in subts1.  stop.'
     stop
4718 ndx1 = lswtch + i
     ndx2 = lswtch + ndx1
     ndx3 = lswtch + ndx2
     n1 = kmswit(i)
     n2 = kmswit(ndx1)
     n  = iardub(i)
     n5 = iardub(ndx1)
     i1 = iardub(ndx2)
     d1 = ardube(ndx3)
     m = kpos(i)
     m1 = iabs( m)
     bus1 = bus(n1)
     bus2 = bus(n2)
     bus3 = text1
     if ( n .eq. 0 )  bus3 = text2
     if ( k1 .eq. 8890 )  bus3 = text5
     if ( k1 .eq. 8891 )  bus3 = text6
     if ( iprsup  .eq. 7 ) write (lunit6, 7218)  i, n, m, n1, n2, n3, n4, i1, m1, d1, bus1, bus2
7218 format ( /,  1x,  32h       i       n       m      n1, 40h      n2      n3      n4      i1      m1,  13x, &
          2hd1,  16h    bus1    bus2   ,/,  1x,  9i8,  e15.6,  4x,  2a8 )
     if ( iprsup .eq. 7 ) write (6,*) ' n5, k1, n13, swcold(n13) =',n5, k1, n13, swcold(n13)
     !     ------  check  open/close  clamping  ------
     if ( n5 .eq. 0 )  go to 301
     ndx1 = kxtcs + n5
     if ( iprsup .eq. 7 ) write (lunit6, *) ' ndx1, xtcs(ndx1) =',  ndx1, xtcs(ndx1)
     if ( xtcs( ndx1) .lt. -flzero*10. )  go to 809
     if ( xtcs( ndx1) .le. +flzero*10. )  go to 803
     if ( m1 .eq. 2 )  go to 801
     go to 808
809  if ( m1 .eq. 2 )  go to 805
     go to 801
301  if ( k1 .eq. 8891 )  go to 801
     if ( k1 .eq. 8890 .and.  n .eq. 0 )  go to 801
803  if ( k1 .eq. 8891 )  go to 809
     if ( n .eq. 0 .and.  k1 .eq. 8890 )  go to 801
     if ( m1 .ne. 2 )  go to 804
     !                                         ------  was closed  ------
     d5 = tclose( i)
     mk = -adelay(i)
     if ( iprsup .eq. 7 ) write (6,*) ' mk, d5 =',  mk, d5
     if ( mk  .le.  0 ) go to 888
     if ( d5 .gt. 0.0 ) go to 688
     if ( a8sw(mk+6) .eq. 0.0 ) go to 788
     if ( a8sw(mk+6)*d5 .gt. 0.0 ) go to 788
     delti = a8sw(mk+6) - d5
     didt = delti / deltat
     a8sw(mk+3) = didt ** a8sw(mk+1) * a8sw(mk)
     a8sw(mk+4) = a8sw(mk+3) / ( didt * a8sw(mk+2) )
     a8sw(mk+5) = t + d5*deltat/delti + a8sw(mk+4)*a8sw(mk+2)
788  if ( t + deltat .lt. a8sw(mk+5) ) go to 871
     ardube(ndx2) = 9999.
     go to 805
688  delti = a8sw(mk+6) - d5
     if ( delti  .lt.  d5 ) go to 871
     didt = delti / deltat
     a8sw(mk+3) = didt ** a8sw(mk+1) * a8sw(mk)
     a8sw(mk+4) = a8sw(mk+3) / ( didt * a8sw(mk+2) )
     a8sw(mk+5) = t + d5*deltat/delti + a8sw(mk+4)*a8sw(mk+2)
     go to 788
871  a8sw(mk+6) = d5
     go to 801
888  if ( k1 .ne. 8890 )  go to 4761
     ndx3 = kxtcs + n
     if ( iprsup .eq. 7 ) write (6,*) ' check  xtcs(ndx3) =',  xtcs(ndx3)
     if ( xtcs(ndx3) .gt. 10.*flzero )  go to 801
     d14 = d5 * swcold(n13)
     swcold(n13) = d5
     if ( d14 .lt. 0.0 ) go to 805
     d5 = absz ( d5 )
4761 if ( d5 .ge. ardube(ndx2) )  go to 801
     !                                               ------  opening  ------
805  j = 5
     if ( m  .lt.  0 )   j = -j
     if ( d1 .ne. 0.0 )  ardube(i) = 0.0
     kpos(i) = j
     n19 = n19 + 1
     modswt(n19) = -i
     if ( ialter .eq. 0 )  ialter = 1
     if ( i1  .ne.  0 ) write (lunit6, 806)  bus3, bus1, bus2, text3, t
806  format ( 51x, a6, 2h ', a6, 6h' to ', a6, 3h'  , a6, 7hg after, e12.5, 5h sec.   )
     if ( istep .gt. 1  .or.  k1 .ne. 8888 )   go to 801
     write (lunit6, 1128)
1128 format ( /,  26x,  94( 1h= )   ,/, 10x, &
          97hWarning.  ----  the just-opened valve or diode was closed during the steady-state phasor solution                  , /, &
          26x,  94hfor initial conditions.   But the resulting current at time zero was from cathode to anode, so                       ,/, &
          26x,  92hopening occurred on time-step number one.   The user might consider removing  'closed'  from                         ,/, &
          26x,  46hcolumns  55-60  of the associated switch card.     ,/, 26x,  94( 1h= )  ,/,  1x    )
     go to 801
     !     ------  was open  ------
804  ardube(i) = ardube(i) + deltat
     if ( ardube(i) .gt. d1 )  ardube(i) = - fltinf
     d5 =  emtpe( n1) - emtpe( n2)
     ndx1 = lswtch + i
     if ( iprsup .eq. 7 ) write (6, *) ' ndx1, ardube(i), d1, d5, ardube(ndx1) =', ndx1, ardube(i), d1, d5, ardube(ndx1)
     if ( k1 .eq. 8890 )  d5 = absz( d5)
     if ( d5 .lt. ardube(ndx1) )  go to 801
     if ( k1 .eq. 8890 )  go to 807
     if ( ardube(i) .lt. 0.0 )  go to 807
     ardube(i) = - fltinf
     go to 808
807  if ( n .eq. 0 )  go to 808
     ndx1 = kxtcs + n
     if ( xtcs( ndx1) .le. flzero*10. )  go to 801
     !     ------  closing  ------
808  j = 2
     if ( m  .lt.  0 )   j = -j
     kpos(i) = j
     n19 = n19 + 1
     modswt(n19) = i
     if ( ialter .eq. 0 )  ialter = 1
     if ( i1  .ne.  0 ) write (lunit6, 806)  bus3, bus1, bus2, text4, t
801 end do
  ktrlsw(1) = n19
  !     ------------------------------------------------------------------
800 continue
  if ( inonl  .le. 0 )  go to 3991
  do i=1, inonl
     k = nonlk(i)
     m = iabs( nonlm(i) )
     vsl = emtpe(k) - emtpe(m)
     if (  iprsup .gt. 4 ) write (lunit6, 3906)  i, k, m, nltype(i), nonlad(i), curr(i), emtpe(k), emtpe(m), vnonl(i)
3906 format ( /, 9h at  3906   , 5i10, f10.0, 3e15.5  )
     if (  nltype(i) .lt. 0 )  go to 83903
     gus2 = curr(i)
     if (  nonle(i)  .gt.  0 )   go to 73960
     vnonl(i) = vnonl(i) + deltat*vsl
     go to 73960
83903 k9899 = 1
     j = 0
     if ( nltype(i)  .ne.  -96 )   go to 7642
     n6 = nonlad(i)
     n7 = cchar(n6)
     n9 = ilast(i)
     n10 = n9 + n7 - 1
     n12 = n6 + 2
     vnonl(i) = vnonl(i) + vsl*delta2
     cchar(n6+3) = vsl * gslope(n6+1) + gslope(n6)
     gus2 = cchar(n6+3)
     if ((cchar(n6+4) .lt. 0) .and. (vsl .gt. flzero) ) cchar(n6+4)=0
     if ( iprsup  .ge.  2 )  write (lunit6, 4317)  i, n6, n7,  n9, vnonl(i), gus2, vsl
4317 format ( /,  22h begin type-96 update., 32h       i      n6      n7      n9,   8hvnonl(i),  16x, &
          4hgus2,  17x,  3hvsl  ,/,  22x,  4i8,  3e20.11 )
     if ( vnonl(i)+flzero  .lt.  vchar(n6+2)     .and.  cchar(n6+1)  .eq.  1 )   go to 1100
     if ( vnonl(i)-flzero  .gt.  vchar(n6+2)     .and.  cchar(n6+1)  .eq.  -1 )   go to 1100
     !     the last point was not a reversal point of hysteresis trajectory.
     if ( absz( vnonl(i) )  .lt.  vchar(n10) )   go to 1116
     vchar(n6) = 0.0
     vchar(n6+1) = 0.0
1116 d7 = vchar(n6) * vnonl(i) + vchar(n6+1)
     if ( d7 .ge. 0.0) go to 1118
     d7 = 0.0
     vchar(n6) = 0.0
     vchar(n6+1) = 0.0
1118 if ( cchar(n6+1) .eq. 1 ) go to 1110
     !     we are on a downer with no reversal.
     if (cchar(n6+5) .eq. 1 ) go to 1127
     !     calculate the distance between the upper and
     !     lower half of the major loop at the present
     !     operating point
     d8 = vnonl(i) + d7
     do n11=n9,n10
        if (cchar(n6+3) .gt. cchar(n11) ) go to 1123
        cchar(n12) = n11
        go to 1125
1123 end do
     cchar(n12) = n10 + 1
1125 n13 = cchar(n12) + n7 + 1
     d9 = vchar(n13)*cchar(n6+3) + cchar(n13)
     d10 = d8 - d9
     !     compare available distance and normal operation distance
     if ( d7 .le. (d10+flzero) ) go to 1119
     cchar(n6+5) = 1
1127 do n11=n9,n10
        if (vnonl(i) .gt. vchar(n11) ) go to 1130
        cchar(n12) = n11
        go to 1212
1130 end do
     cchar(n12) = n10 + 1
     go to 1212
1119 do n11=n9,n10
        if (-d8 .gt. vchar(n11) ) go to 1120
        cchar(n12) = n11
        go to 1212
1120 end do
     cchar(n12) = n10 + 1
     go to 1212
     !     we are on an upper with no reversal.
1110 if (cchar(n6+5) .eq. 1 ) go to 1147
     !     calculate the distance between the upper and
     !     lower half of the major loop at the present
     !     operating point
     d8 = vnonl(i) - d7
     do n11=n9,n10
        if (-cchar(n6+3) .gt. cchar(n11) ) go to 1140
        cchar(n12) = n11
        go to 1141
1140 end do
     cchar(n12) = n10 + 1
1141 n13 = cchar(n12) + n7 + 1
     d9 = vchar(n13)*cchar(n6+3) - cchar(n13)
     d10 = d9 - d8
     !     compare available distance and normal operation distance
     if ( d7 .le. (d10+flzero) ) go to 1149
     cchar(n6+5) = 1
1147 do n11=n9,n10
        if (-vnonl(i) .gt. vchar(n11) ) go to 1148
        cchar(n12) = n11
        go to 1312
1148 end do
     cchar(n12) = n10 + 1
     go to 1312
1149 do n11=n9,n10
        if ( d8 .gt. vchar(n11) ) go to 1150
        cchar(n12) = n11
        go to 1312
1150 end do
     cchar(n12) = n10 + 1
     go to 1312
     !     the previous point was a reversal point on hysteresis trajectory.
1100 cchar(n6+1) = -cchar(n6+1)
     cchar(n6+4) = cchar(n6+4) + 1
     cchar(n6+5) = 0
     if ( cchar(n6+1) .ne. 1 ) go to 1195
     !     we now switch to an upper ( there was a reversal).
     if ( vchar(n6+2) .gt. -vchar(n10) ) go to 1580
     vchar(n6+5) = vchar(n10)
     gslope(n6+5) = cchar(n10)
     vchar(n6) = 0.0
     vchar(n6+1) = 0.0
     vchar(n6+4) = vchar(n9)
     gslope(n6+4) = cchar(n9)
     cchar(n12) = n9
     go to 1312
1580 if ( cchar(n6+4) .gt. 1 ) go to 1581
     vchar(n6+4) = vchar(n10)
     gslope(n6+4) = cchar(n10)
     d6 = 0.0
     go to 1575
1581 do n11=n9,n10
        if ( gslope(n6+4) .gt. cchar(n11) ) go to 1701
        cchar(n12) = n11
        go to 1710
1701 end do
     cchar(n12) = n10 + 1
1710 n13 = cchar(n12) + n7 + 1
     d9 = vchar(n13) * gslope(n6+4) + cchar(n13)
     d6 = vchar(n6+4) - d9
1575 do n11=n9,n10
        if ( vchar(n6+3) .gt. cchar(n11) ) go to 1750
        cchar(n12) = n11
        go to 1760
1750 end do
     cchar(n12) = n10 + 1
1760 n13 = cchar(n12) + n7 + 1
     d10 = vchar(n13) * vchar(n6+3) + cchar(n13)
     d11 = vchar(n6+2) -d10
     do n11=n9,n10
        if ( gslope(n6+5) .gt. cchar(n11) ) go to 1800
        n14 = n11
        go to 1810
1800 end do
     n14 = n10 + 1
1810 n14 = n14 + n7 + 1
     d10 = vchar(n14) * gslope(n6+5) + cchar(n14)
     d13 = vchar(n6+5) - d10
     if ( vchar(n6+2) .ge. vchar(n6+5) ) go to 1675
     d14 = 0.0
     if ( absz(d13) .gt. flzero ) d14=d11*d6/d13
     d9 = d9 + d14
     go to 1550
     !     we now switch to a downer ( there was a reversal ).
1195 if ( vchar(n6+2) .lt. vchar(n10) ) go to 1590
     vchar(n6+5) = vchar(n9)
     gslope(n6+5) = cchar(n9)
     vchar(n6) = 0.0
     vchar(n6+1) = 0.0
     vchar(n6+4) = vchar(n10)
     gslope(n6+4) = cchar(n10)
     cchar(n12) = n9
     go to 1212
1590 if ( cchar(n6+4) .gt. 1 ) go to 1591
     vchar(n6+4) = vchar(n9)
     gslope(n6+4) = cchar(n9)
     d6 = 0.0
     go to 1515
1591 do n11=n9,n10
        if ( -gslope(n6+4) .gt. cchar(n11) ) go to 1600
        cchar(n12) = n11
        go to 1610
1600 end do
     cchar(n12) = n10 + 1
1610 n13 = cchar(n12) + n7 + 1
     d9 = vchar(n13) * gslope(n6+4) - cchar(n13)
     d6 = d9 - vchar(n6+4)
1515 do n11=n9,n10
        if ( -vchar(n6+3) .gt. cchar(n11) ) go to 1650
        cchar(n12) = n11
        go to 1660
1650 end do
     cchar(n12) = n10 + 1
1660 n13 = cchar(n12) + n7 + 1
     d10 = vchar(n13) * vchar(n6+3) - cchar(n13)
     d11 = d10 - vchar(n6+2)
     do n11=n9,n10
        if ( -gslope(n6+5) .gt. cchar(n11) ) go to 1850
        n14 = n11
        go to 1860
1850 end do
     n14 = n10 + 1
1860 n14 = n14 + n7 + 1
     d10 = vchar(n14) * gslope(n6+5) - cchar(n14)
     d13 = d10 - vchar(n6+5)
     if ( vchar(n6+2) .gt. vchar(n6+5) ) go to 1680
1675 vchar(n6+5) = vchar(n6+4)
     gslope(n6+5) = gslope(n6+4)
     d13 = d6
     go to 1690
1680 d14 = 0.0
     if ( absz(d13) .gt. flzero ) d14=d11*d6/d13
     d9 = d9 - d14
1550 vchar(n6+5) = d9
     gslope(n6+5) = gslope(n6+4)
     d13 = d14
1690 vchar(n6+4) = vchar(n6+2)
     gslope(n6+4) = vchar(n6+3)
     d12 =d11
     if ( vchar(n6+4) .lt. (vchar(n6+5)-flzero)) go to 1700
     if ( vchar(n6+4) .gt. (vchar(n6+5)+flzero)) go to 1700
     if ( cchar(n6+1) .eq. 1) go to 1720
     vchar(n6+5) = vchar(n9)
     gslope(n6+5) = cchar(n9)
     d13 = 0.0
     go to 1700
1720 vchar(n6+5) = vchar(n10)
     gslope(n6+5) = cchar(n10)
     d13 = 0.0
1700 d15 = d12*(vchar(n10)-vchar(n6+5)) / (vchar(n10)-vchar(n6+4))
     if (cchar(n6+1) .eq. -1. ) d15 = d12*(-vchar(n10)-vchar(n6+5)) / (-vchar(n10)-vchar(n6+4))
     if ( d13  .le.  d15 )       go to 1705
     d13 = d15
     if ( iprsup  .ge.  1 ) write (lunit6,1706) bus(k), bus(m), t
1706 format ( /,  5x,  36hnote ---- for the type-96 hysteretic,31h inductor which connects bus  ', &
          a6,  8h'  to  ',  a6, 22h' ,   a trajectory was          ,/, &
          15x,  34hinitially created which would have, 29h caused operation outside the, &
          28h major hysteresis loop.  the    ,/, 15x,  31htrajectory has been modified to, &
          31h prevent this.  accuracy of the, 30h results should be unaffected.  ,/, &
          5h  t =,  e15.6  )
     if ( iprsup  .ge.  1 ) write (lunit6, 7706)  i, d13, d15
7706 format (    15x,  32h row number of  n.l.  element is, i4,  26h .   variables  d13, d15 =, 2e13.4 )
1705 vchar(n6) = (d12 - d13) / (vchar(n6+4)-vchar(n6+5))
     vchar(n6+1) = d12 - vchar(n6) * vchar(n6+4)
     if ( cchar(n6+1) .eq. 1 ) go to 1312
1212 n14 = cchar(n12)
     n13 = n14 + n7 + 1
     if (cchar(n6+5) .eq. 1 ) go to 1213
     d13 = gslope(n14) * (1.0 + vchar(n6) )
     d13 = 1.0 / d13
     d14 = gslope(n13) -  gslope(n14) * vchar(n6+1)
     d14 = d14 * d13
     go to 1315
1213 d13 = vchar(n13)
     d14 = cchar(n13)
     go to 1315
1312 n14 = cchar(n12)
     n13 = n14 + n7 + 1
     if (cchar(n6+5) .eq. 1 ) go to 1313
     d13 = gslope(n14) * ( 1.0 - vchar(n6) )
     d13 = 1.0 / d13
     d14 = gslope(n14) * vchar(n6+1) - gslope(n13)
     d14 = d14 * d13
     go to 1315
1313 d13 = vchar(n13)
     d14 = -cchar(n13)
1315 if ( gslope(n12)  .eq.  cchar(n6+1) .and. gslope(n6+3)  .eq.  cchar(n12) )   go to 1319
     d15 = delta2 / d13
     a = d15 - gslope(n6+1)
     gslope(n6+1) = d15
     go to 1322
1319 a = 0.0
1322 d16 = vnonl(i) - d14 + delta2 * vsl
     d16 = d16 / d13
     gus1 = d16 - gslope(n6)
     gslope(n6) = d16
     gslope(n12) = cchar(n6+1)
     gslope(n6+3) = cchar(n12)
     vchar(n6+3) = cchar(n6+3)
     vchar(n12) = vnonl(i)
     vnonl(i) = vnonl(i) + vsl * delta2
     n8 = n6 + 5
     if ( iprsup  .ge.  2 ) write (lunit6, 4372)  d14, a, gus1,  ( ip, cchar(ip), vchar(ip), gslope(ip), ip=n6, n8 )
4372 format ( /,  14h done type-96.,  17x,  3hd14,  19x,  1ha, 16x,  4hgus1,  5x,  48h(i, cchar(i), vchar(i), gslope(i), i=n6, &
          n8) ...   ,/,  14x,  3e20.11  ,/,  ( 1x,  i5,  3e20.11, i5,  3e20.11 )  )
     if ( a  .eq.  0.0 )  go to 3975
     go to 3950
7642 it1 = absz( curr(i) )
     it2 = nonlad(i) - 1
     gus2 = 0.0
     if (  it1 .ne. 0 )  go to 73910
     if (  nltype(i) .ne. -97 ) go to 3900
     if (  t .lt. anonl(i) )  go to 73960
     if (  absz(vsl)  .le.  vnonl(i) ) go to 73960
     anonl(i) = t
     curr(i) = 1.0
     it2 = it2 + 1
     a = gslope(it2)
     gus1 = 0.0
     go to 3950
3900 if (  absz(vsl)  .le.  vnonl(i) ) go to 73960
     j = 1
     nn1 = anonl(i)
     it2 = it2 + nn1
     curr(i) = nn1
     if ( vsl  .lt.  0.0 ) curr(i) = - curr(i)
     vecnl2(i) = t + vecnl1(i)
     a = gslope(it2)
     gus1 = cchar(it2)
73905 if (  vsl .lt. 0.0 ) gus1 = -gus1
     go to 3950
73910 it2 = it2 + it1
     gus2 = vsl * gslope(it2)
     if (  nltype(i)  .ne.  -98 )  go to 73907
     k9899 = 0
     gus2 = gus2 + anonl(i)
     gus4 = delta2 * vsl
     vsl = vnonl(i) + gus4
     go to 73908
73907 if (  nltype(i) .ne. -97 )  go to 43420
     if (  it2 .ge. nonle(i) )  go to 73960
     if (  t-anonl(i)   .lt.  cchar(it2+1) )  go to 73960
     curr(i) = curr(i) + 1.0
     a = gslope(it2+1)  -  gslope(it2)
     gus1 = 0.0
     go to 3950
43420 gus2 = absz(gus2) + cchar(it2)
     if (  vsl .lt. 0.0 )  gus2 = - gus2
     if ( t  .lt.  vecnl2(i) )   go to 73908
     if (  absz(vsl)   .lt.   vzero(i) )  go to 73942
73908 a = curr(i) * vsl
     if (  a .gt. 0.0 )  go to 73913
     if (  it1 .eq. 1 )  go to 73937
     write (lunit6, 73912)  i, curr(i), vsl, gus2
73912 format (  47h trouble at 73912 on type-99 or 98 elem number , i2,  e10.1, 2e15.5  )
     nn1 = it2 - 1
     if ( curr(i)  .gt.  0.0 )   go to 73935
     go to 73915
73913 if (    it2    .ge.  nonle(i) )  go to 73930
     if (  absz(vsl)  .le.  vchar(it2) )  go to 73930
     nn1 = it2 + 1
     if ( curr(i)  .lt.  0.0 )   go to 73935
73915 curr(i) = curr(i) + 1.0
73925 a = gslope(nn1)  -  gslope(it2)
     if (  k9899 .gt. 0 )  go to 73927
     gus3 = cchar(nn1)
     if (  vsl .lt. 0.0 )  gus3 = -gus3
     d1 = ( vsl + gus4  -  gus3 ) * gslope(nn1) / delta2
     if (  iprsup .ge. 1 ) write (lunit6, 73926) i, istep, d1, anonl(i), gus2, gus3, vnonl(i)
73926 format ( 23h type-98 segment change   ,2i10, /, 1x, 5e25.15 )
     gus2 = ( anonl(i) + d1 ) / 2.0
     go to 3950
73927 gus1 = cchar(nn1)  -  cchar(it2)
     go to 73905
73930 if (  it1 .gt. 1 )  go to 73932
73931 if (  k9899 .gt. 0 )  go to 73960
     go to 3973
73932 nn1 = it2 - 1
     if (  absz(vsl)  .ge.  vchar(nn1) )  go to 73931
     if ( curr(i)  .lt.  0.0 )   go to 73915
73935 curr(i) = curr(i) - 1.0
     go to 73925
73937 if (  k9899 .gt. 0 )  go to 73940
     curr(i) = - curr(i)
     go to 3973
73940 if ( t  .ge.  vecnl2(i) )   go to 73942
     curr(i) = - curr(i)
     go to 73978
73942 gus1 = -cchar(it2)
     if ( curr(i)  .lt.  0.0 )   gus1 = - gus1
     j = 1
     a = - gslope(it2)
     curr(i) = 0.0
     d1 = vchar(it2) - sglfir
     if ( absz(d1)  .lt.  flzero ) vnonl(i) = fltinf
3950 if ( k  .lt.  m )   go to 4113
     n8 = m
     n9 = k
     go to 4114
4113 n8 = k
     n9 = m
4114 if (  n8 .eq. 1 )  go to 3965
     if ( n8  .gt.  kpartb )   go to 73978
     n3 = kks(n8)
3963 n3 = n3 - 1
     n4 = iabs( km(n3) )
     if (  n4 .ne. n8 )  go to 3964
     ykm(n3) = ykm(n3) + a
     go to  3963
3964 if (  n4 .ne. n9 )  go to 3963
     ykm(n3) = ykm(n3) - a
     go to 3967
3965 if (  n9 .gt. kpartb )  go to 3974
3967 if (  n9 .gt. kpartb )  go to 3972
     n1 = kks(n9)
3970 n1 = n1 - 1
     n2 = iabs( km(n1) )
     if (  n2 .ne. n9 )  go to 3971
     ykm(n1) = ykm(n1) + a
     go to 3972
3971 if (  n2 .ne. n8 )  go to 3970
     ykm(n1) = ykm(n1) - a
     go to 3970
3972 if ( ialter .eq. 0 )  ialter = 1
3974 if (  k9899 .gt. 0 )  go to 3975
3973 gus1 = 2.0 * ( gus2 - anonl(i) )
     vnonl(i) = vsl + gus4
     anonl(i) = anonl(i) + gus1
     if (  iprsup .ge. 3 ) write (lunit6, 1974)  i, k, m, curr(i), gus2, gus4, vsl, gus1, vnonl(i), anonl(i), a
1974 format ( /, 30h type-98 elem update at  1974.    , 3i10,f10.0  ,/,  ( 1x, 5e25.15 ) )
3975 finit(k) = finit(k) - gus1
     finit(m) = finit(m) + gus1
73978 it1 = curr(i)
     if ( j  .le.  0  ) go to 73960
     max99m = max99m - 1
     if ( max99m .ge. 0  .or.  iprsup  .ge.  3 ) write (lunit6, 73953)  bus(k), bus(m), t, it1
73953 format ( 20x, 23htype-99 n.l. v-i from ', a6, 6h' to ', a6, 1h', 6h at t=, e12.4,  28h begins operation on segment, i4,  1h.  )
73960 if (  nonlm(i) .gt. 0 )  go to 3990
     kcount = kcount + 1
     bvalue(kcount) = gus2
3990 end do
3991 continue
  if ( m4plot .eq. 1 ) call yserlc
  if ( ialter .eq. 0 )  go to 2616
  ktrlsw(3) = ktrlsw(3) + 1
  if ( ktrlsw(1) .gt. 0 ) call switch
  if ( kanal  .eq.  2 ) call last14
  !       &&&&&&&&&& enter retriangularization of complete (y)
  call move0 ( kssfrq(1), ntot )
  ii = 0
  l = 1
  go to 2290
2205 l=l+1
  it2 = kode (l)
  if ( it2 .gt. l) go to 2290
  if ( it2 .eq. 1 )  go to 2290
  emtpf(1) = 0.0
  n17 = 1
  j=kks(l)
2220 j = j - 1
  n13 = km(j)
  i = iabs ( n13 )
  if ( kode(i) .eq. 0 ) go to 2227
2224 if ( kode(i) .lt. i ) go to 2222
  i = kode(i)
  go to 2224
2222 if ( kode(i) .eq. 1 )  go to 2233
2227 if ( i .eq. l )  go to 2237
  n18 = n17
2225 jj = kssfrq(n17)
  if ( jj .eq. 0 ) go to 2231
  if ( jj .eq. i ) go to 2229
  if ( jj .gt. i ) go to 2230
  n17 = jj
  go to 2225
2237 emtpf(1) = emtpf(1) + ykm(j)
  go to 2233
2229 emtpf(i) = emtpf(i) + ykm(j)
  go to 2232
2230 kssfrq( i ) = jj
2231 kssfrq(n17) = i
  emtpf(i) = ykm(j)
2232 n17 = n18
  if ( kode(i) .eq. 0 )  n17 = i
2233 if ( n13 .gt. 0 ) go to 2220
  if ( it2 .eq. 0 ) go to 2240
  if ( it2.eq.l) go to 2240
  !     addition of lower numbered row when switch is closed.
  j=kks(it2)
  it2 = kode(it2)
  n17 = 1
  go to 2220
  !                       addition of elements for closed switches
2240 n17 = 1
  acheck=absz(emtpf(1))*epsiln
  if (  acheck .ne. 0.0 .or. ktrans(l) .eq. -664422 )  go to 2260
  if (nwarn .le. 0 ) write (lunit6, 2253)  bus(l)
2253 format ( 38h zero diagonal admittance;   zero ykk., 40h  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% &
       ,/,   36h while preparing to eliminate row  ",  a6, 41h"  of the admittance matrix,  it is found &
       ,/,   36h that the diagonal is zero due to no, 41h connected elements.   add high impedance &
       ,/,   40h branch to earth (value 10*flzero),  and, 10h continue.    )
  nwarn = -1
  emtpf(1) = flzero * 10.
2260 k = kssfrq(n17)
  if (  k .gt. l  .or.  k .eq. 0 ) go to 2280
  a=emtpf(k)
  if ( a .eq. 0.) go to 2279
  n14 = k
  i = kk(k)
2265 if ( km(i) .lt. 0 )  go to 2270
  i = i - 1
  go to 2265
2270 i = i + 1
2278 if ( i .gt. kk(k) )  go to 2279
  n1=km(i)
  if ( n1 .eq. l )  go to 2272
2271 n15 = kssfrq(n14)
  if ( n15 .eq. n1 )   go to 2273
  if ( n15 .gt. n1 )  go to 2275
  if ( n15 .eq. 0 )  go to 2276
  n14 = n15
  go to 2271
2272 emtpf(1) = emtpf(1) - a * ykm(i)
  go to 2270
2273 emtpf(n1)=emtpf(n1)-a*ykm(i)
  go to 2277
2275 kssfrq(n1) = n15
2276 kssfrq(n14) = n1
  emtpf(n1) = - a * ykm(i)
2277 n14 = n1
  go to 2270
2279 kssfrq(n17) = 0
  n17 = k
  go to 2260
2280 a=emtpf(1)
1280 if (  absz(a) .ge. acheck ) go to 4312
  if ( nwarn .le. 0 ) write (lunit6, 2281) bus(l), a, acheck, epsiln
2281 format ( 43h floating subnetwork.  floating subnetwork., 40h  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% &
       ,/,   26h the elimination of row  ",  a6, 37h"  has produced a near-zero diagnonal &
       ,/,    9h of value,   e13.5,  15h  just prior to, 43h reciprocation.   the acceptable minimum is &
       ,/,    e13.5,  32h  (equal to "epsiln" times ykk).,  43h   short node to ground with  ykk = fltinf. &
       ,/,   45h for the record, miscellaneous data parameter,  19h "epsiln" has value,  e12.3,  2h .    )
  nwarn = -1
  a = fltinf
  if ( m4plot .ne. 1 )  go to 4312
  nwarn = 0
  write (munit6, 3688)
3688 format (  39h   ++++  no, ykk grounding is optional.  )
  call window
  call honker( ll10 )
3711 write (prom80, 3712)
3712 format ( ' send remedy (spy, ykk, pass, stop) :' )
  call prompt
  lockbr = 1
  call flager
  read (buff77, 3719)  bus1
3719 format ( a6 )
  if ( bus1 .ne. text10 )  go to 3726
  call spying
  go to 3711
3726 if ( bus1 .eq. text11 )  go to 4312
  if ( bus1 .eq. text12 ) call stoptp
  if ( bus1 .ne. text13 )  go to 3711
  write (prom80, 3733)
3733 format ( 44h send new ykk value at reciprocation point : )
  call prompt
  call flager
  call frefp1 ( buff77, a )
  go to 1280
4312 a = 1.0 / a
  ii=ii+1
  km(ii)=-l
  ykm(ii)=a
  go to 2285
2283 if (  emtpf(k) .eq. 0.0 )  go to 2288
  ii = ii + 1
  ykm(ii)=emtpf(k)*a
  km(ii)=k
2288 kssfrq(n17) = 0
  n17 = k
  k = kssfrq(k)
2285 if ( k .ne. 0 )  go to 2283
  kk(l) = ii
  if (  iprsup .gt. 1 ) write (lunit6, 2286)  l, ii, kk(l), kpartb, a
2286 format ( 38h done triangularizing new row of (y) ., 32h       l      ii   kk(l)  kpartb,  24x,  1ha  ,/, &
       38x,  4i8,  e25.16 )
  if ( ii  .lt.  kks(1) )   go to 2290
  kill = 1
  lstat(13) = 2
  lstat(15) = l
  lstat(16) = 5
  lstat(19) = 2290
  go to 9200
2290 if (  l .lt. kpartb )  go to 2205
  iupper = ii
  if ( nwarn .lt. 0 )  nwarn = 1
  n4 = iupper + lymat - kks(1) + 1
  if ( n4  .gt.  lstat(25) )   lstat(25) = n4
  if (  iprsup .gt. 0 ) write (lunit6, 2291)  iupper, lymat, kks(1)
2291 format ( /,  34h upper-triangular factors for (y)., 4x,  31h(i, km(i), ykm(i), i=1, iupper)   , 10x, 23hiupper, lymat, kks(1) =,  3i6  )
  if (  iprsup .gt. 2 ) write (lunit6, 2292 )  ( i, km(i), ykm(i), i=1, iupper )
2292 format ( 1x,   5( i6, i5,  e15.6 )   )
  if (  iprsup .gt. 1 ) write (lunit6, 2293)  ( kk(i), i=1, ntot )
2293 format ( 18h kk(i), i=1, ntot   ,/, ( 1x, 20i6 ) )
  if (  iprsup .gt. 3 ) write (lunit6, 2296)  num99, inonl, ncomp, istep, indstp, ntot, lpast, ipoint, kswtch
2296 format ( /,  1x,  40h   num99   inonl   ncomp   istep  indstp, 32h    ntot   lpast  ipoint  kswtch  ,/,  1x,  9i8  )
  !     start.  find differences of columns of inverse for nonlinearities*
2300 n13 = 0
  j11 = 0
  if (  ncomp .le. 0 )  go to 2616
  j11 = ntot * ncomp
  call mover0 ( znonl(1), j11 )
  if (  inonl .eq. num99 )  go to 2321
  do i=1,inonl
     if ( nltype(i) .lt. 0 )  go to 2320
     if ( nltype(i) .gt. 920 )  go to 2320
     vzero(i) = - fltinf
     n1=nonlad(i)
     ilast(i)=iabs(n1)
     if ( n1.lt.0) vzero(i)=0.
2320 end do
2321 do n2=1, numsub
     n1 = 0
     l = isubeg(n2)
     if ( l  .le.  0 )  go to 2322
2306 d8 = -1.0
     j = nsubkm(l+1)
     do n10=1, 2
        if ( kpsour(j)  .eq.  0 )  go to 2315
4377    if ( kode(j) .lt. j ) go to 4319
        j = kode(j)
        go to 4377
4319    if ( kode(j)  .eq.  1 )   go to 2315
        n5 = j + n1
        znonl(n5) = d8
2315    d8 = 1.0
4323    j = nsubkm(l+2)
     end do
     l = nsubkm(l)
     if ( l  .eq.  isubeg(n2) )   go to 2322
     n1 = n1 + ntot
     go to 2306
2322 end do
  if (  iprsup .ge. 2 ) write (lunit6, 2318)  ( znonl(i), i=1, j11 )
2318 format ( /, 40h (znonl(1:j11) before solution, at 1313.     ,/, ( 1x, 8e15.5 ) )
  ii = 1
  n4 = -kpartb
2410 if ( ii .gt. iupper ) go to 2450
  l = iabs(km(ii))
  n8 = l
  do m=1, ncomp
     voltbc(m) = znonl(n8)
     znonl(n8) = znonl(n8) * ykm(ii)
2413 n8 = n8 + ntot
  end do
  j=iabs(kk(l))
2420 ii=ii+1
  if ( ii.gt.j) go to 2410
  k=km(ii)
  if ( k .le. kpartb ) go to 2422
  ii = j + 1
  go to 2410
2422 n8 = l
  do m=1, ncomp
     znonl(k) = znonl(k)  -  voltbc(m) * ykm(ii)
     n8 = n8 + ntot
2423 k = k + ntot
  end do
  go to 2420
2450 if ( iprsup  .ge.  2 ) write (lunit6, 2341)  ( znonl(k), k=1, j11 )
2341 format ( /, 25h (znonl(k), k=1, j11) ...  ,/, ( 1x,  8e16.6 )  )
2500 if ( ii.eq.1) go to 2550
  call mover0 ( voltbc(1), ncomp )
2510 ii=ii-1
  k=km(ii)
  if ( k.lt.0) go to 2520
  n8 = k
  do m=1, ncomp
     voltbc(m) = voltbc(m)  -  znonl(n8) * ykm(ii)
2513 n8 = n8 + ntot
  end do
  go to 2510
2520 l=iabs(k)
  n8 = l
  do m=1, ncomp
     znonl(n8) = znonl(n8) + voltbc(m)
2526 n8 = n8 + ntot
  end do
  if ( kode(l) .eq. 0 ) go to 2500
  j = kode(l)
2528 n8 = j
  n9 = l
  do m=1, ncomp
     znonl(n8) = znonl(n9)
     n8 = n8 + ntot
2531 n9 = n9 + ntot
  end do
  if ( kode(j) .eq. l ) go to 2500
  j = kode(j)
  go to 2528
  !             find differences of columns of inverse for nonlinearities
2550 if (  inonl .le. num99 )  go to 2616
  do i=1, inonl
     if (  nltype(i) .lt. 0 )  go to 2600
     if (  nltype(i) .gt. 920 )  go to 2600
     n1=nonlk(i)
     n2=iabs(nonlm(i))
     a=znonl(n1)-znonl(n2)
     if ( nltype(i)  .eq.  94 )   go to 2561
     if ( nonle(i)  .gt.  0 )   go to 2561
2557 a = a * delta2
2561 anonl(i) = a
2600 end do
2616 if (  iprsup .ge. 2 ) write (lunit6, 2603)  ( znonl(i), i=1, j11 )
2603 format ( /,  26h znonl after soln, at 2318  ,/, ( 1x, 8e15.5 ) )
  call mover ( finit(1), emtpf(1), ntot )
  if ( kswtch .eq. 0 ) go to 5000
  do i=1, kswtch
     k1 = kdepsw( lswtch + i )
     if ( k1.ne.8888 .and. k1.ne.8890 .and. k1.ne.8891 ) go to 8801
     mk = - adelay(i)
     if ( mk .le. 0 ) go to 8801
     m = iabs( kpos(i) )
     if ( m .ne. 5 ) go to 8801
     ndx2 = lswtch + lswtch + i
     if ( ardube(ndx2) .ne. 9999. ) go to 8801
     n1 = kmswit(i)
     n2 = kmswit( lswtch + i )
     bi = exp( (a8sw(mk+5) - t - deltat) / a8sw(mk+4) )
     ai = a8sw(mk+3) * bi
     emtpf(n1) = ai
     emtpf(n2) = -ai
     if ( ai .le. a8sw(mk+7) ) ardube(ndx2) = 0.0
     if ( bi .le. 0.0001 .and. a8sw(mk+7) .eq. 0.) ardube(ndx2) = 0.
8801 end do
  !              call to solvum if no compensation of um power circuits :
5000 if (loopss(8) .eq. 3) call solvum ( spum(iureac), spum(iugpar), spum(iufpar), spum(iuhist), spum(iuumrp), &
       ispum(iunod1), ispum(iunod2), ispum(iujclt), ispum(iujclo), ispum(iujtyp), ispum(iunodo), ispum(iujtmt), spum(iuhism), &
       spum(iuomgm), spum(iuomld), spum(iutham), spum(iuredu), spum(iureds), spum(iuflds), spum(iufldr), spum(iurequ), &
       spum(iuflqs), spum(iuflqr), ispum(iujcds), ispum(iujcqs), spum(iuflxd), spum(iuflxq), ispum(iunppa), spum(iurotm), &
       ispum(iuncld), ispum(iunclq), ispum(iujtqo), ispum(iujomo),  ispum(iujtho), spum(iureqs), spum(iuepso), spum(iudcoe), &
       ispum(iukcoi), spum(iuvolt), spum(iuangl), ispum(iunodf),  ispum(iunodm), ispum(iukumo), ispum(iujumo), spum(iuumou))
  if ( kill  .gt.  0 )  go to 9200
  if ( iprsup .gt. 2 ) write (lunit6, 3992)  ( emtpe(i), emtpf(i), i=1, ntot )
3992 format (/, ' (emtpe(i), emtpf(i), i=1, ntot)   at the end of the first piece of the time-step loop.', /, (1x, 10e13.3))
  ipoint=ipoint+1
  if ( ipoint.eq.lpast) ipoint=0
  lastov = nchain
  nchain = 17
  go to 9900
7755 kill = 229
  lstat(19) = 7755
  lstat(15) = ncomp
  lstat(16) = lcomp
  go to 9200
7766 n15  = isubeg(knode)
  nn15 = isubeg(mnode)
  lstat(19) = 7766
7777 kill = 229
  lstat(19) = 7777
  lstat(15) = nsubkm (  n15 + 3 )
  lstat(16) = nsubkm ( nn15 + 3 )
9200 lstat(18) = 16
  lastov = nchain
  nchain = 51
9900 if ( iprsup .ge. 1 )  write (lunit6, 9903) kill, lstat(19)
9903 format (  34h exit "subts1".  kill, lstat(19) =, 2i8 )
99999 return
end subroutine subts1
!
!     subroutine yserlc.
!
subroutine yserlc
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     module of interactive emtp only, associated with "emtspy".
  !     non-interactive versions can replace by a dummy module.
  !     this module is called only by "subts1" of overlay 16.
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'dekspy.ftn'
  !     burroughs: preserve local variable between module calls:
  data  ixcopt  / 0 /
  data  xcon  / 0.0 /
  data  ccon  / 0.0 /
  if ( iprsup .ge. 4 ) write (lunit6, 8234)  lserlc, kserlc, kpartb, ialter
8234 format ( ' top of "yserlc".  lserlc, kserlc kpartb, ialter =',4i8 )
  if ( lserlc .le. 0 )  go to 9000
  if ( kserlc .le. 0 )  go to 9000
  if ( ixcopt .gt. 0 )  go  to  3000
  ccon = 1.0 / ( deltat * 500000. )
  xcon = ccon * 1000.
  if ( xopt .gt. 0.0 ) xcon = xcon * 1000.0 / ( twopi * xopt )
  if ( copt .gt. 0.0 ) ccon = ccon / ( twopi * copt )
  ixcopt = 1
3000 do j=1, lserlc
     ndx1 = ibr + j
     if ( kbus(ndx1) .eq. 0 )  go to 8276
     if ( nr(ndx1)   .eq. 0 )  go to 8276
     nr(ndx1) = 0
     ndx2 = it + j
     n1 = litype(ndx1)
     n1 = nr( n1 )
     n1 = iabs( n1 )
     d23 = 1.0 / x( n1 )
     d44 = tr( ndx2 ) + xcon * tx( ndx2 )
     gus1 = emtpc(ndx2)
     if ( gus1 .ne. 0.0 )  gus1 = 1.0 / ( gus1 * ccon )
     d33 = d44 + gus1
     if ( d33 .ge. epsiln*d23*10. )  go to 8253
     write (munit6, 8239)  j, d33, d23, epsiln
8239 format ( '  zero-divide trouble.  epsiln failure.'  ,/,'  j, d33, d23, epsiln =',  i8, 3e15.6       ,/, &
          '  send revised desired r,x,c-values :'  )
     read (munit5, 8246)  buff77
8246 format ( a80 )
     call frefp3(buff77, tr(ndx2), tx(ndx2), emtpc(ndx2) )
     d44 = tr( ndx2 ) + xcon * tx( ndx2 )
     gus1 = emtpc(ndx2)
     if ( gus1 .ne. 0.0 )   gus1 = 1.0 / ( gus1 * ccon )
     d33 = d44 + gus1
8253 d23 = 1.0/d33  -  x( n1 )
     ci(ndx1) = tr(ndx2)
     ck(ndx1) = tx(ndx2)
     cik(ndx1) = emtpc(ndx2)
     x(n1) = d44
     gus2 = 1.0 / d33
     r(n1) = gus2 * (ci( ndx1 ) * 2.0 - x(n1) + gus1)
     x(n1) = gus2
     emtpc( n1 ) = gus1
     if ( kbus(ndx1)  .lt.  mbus(ndx1) )   go to 4107
     n8 = mbus(ndx1)
     n9 = kbus(ndx1)
     go to 4114
4107 n8 = kbus(ndx1)
     n9 = mbus(ndx1)
4114 if ( iprsup .ge. 3 ) write (lunit6, 4116)  j, n8, n9, d23
4116 format ( ' next [y] change.  j, n8, n9, d23 =', 3i8, e14.5 )
     if ( n8 .eq. 1 )  go to  5231
     if ( n8 .gt. kpartb ) go to 8276
     n3 = kks(n8)
5217 n3 = n3 - 1
     n4 = iabs( km(n3) )
     if ( n4 .ne. n8 )  go to  5224
     ykm(n3) = ykm(n3) + d23
     go to  5217
5224 if ( n4 .ne. n9 )  go to  5217
     ykm(n3) = ykm(n3) - d23
     go to  5238
5231 if ( n9 .gt. kpartb )  go to  8276
5238 if ( n9 .gt. kpartb )  go to  5252
     n1 = kks(n9)
5244 n1 = n1 - 1
     n2 = iabs( km(n1) )
     if ( n2 .ne. n9 )  go to  5247
     ykm(n1) = ykm(n1) + d23
     go to  5252
5247 if ( n2 .ne. n8 )  go to  5244
     ykm(n1) = ykm(n1) - d23
     go to  5244
5252 if ( ialter .eq. 0 )  ialter = 1
8276 end do
9000 return
end subroutine yserlc
!
!     subroutine switch.
!
subroutine switch
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  equivalence ( ktrlsw(1), n20 )
  !     overlay-16 module called by "subts1" if and only if one or more
  !     switches has just changed status (if ktrlsw(1) .gt. 0).   the
  !     purpose is to update nextsw vector that gives order of switch
  !     current calculation.   general sophisticated logic uses the
  !     following storage:
  !       modswt(ll) = j  means that the ll-th switching operation of
  !                    this time step involves switch number j.   if
  !                    content (j) is positive,  a now-open switch is
  !                    to be closed;  if content is negative (-j), then
  !                    the now-closed switch j is to be opened.
  !       nextsw(j) = k  means that current is to be calculated in switch
  !                   number k after that of switch number j.  added minus
  !                   sign indicates kcl at 2nd node rather than 1st one.
  !                   pointer is circular (never-ending).
  !       lastsw(k) = j  is the backward map (like nextsw above,  only in
  !                   reverse order.   it, too, is circular.   added minus
  !                   sign marks last switch of switch group.
  !       kentnb(i) = n  means that switches (open or closed) adjacent to
  !                   switch i can be found in nbhdsw(n) onward.  the last
  !                   adjacent one is in cell  kentnb(i+1)-1 .
  !       nbhdsw(j) --- storage for all switches adjacent to any other
  !                     switches.   kentnb  locates desired entries.   if
  !                     no switch is adjacent,  there shall be no such
  !                     storage (entry point exceeds terminal point).
  !       kbegsw(j) = 0  means that switch j begins a switch group.   if
  !                   closed but not 1st of group, then nonzero stored.
  !       kode(k)  =  m   means that switch nodes k and m are shorted
  !                   together as part of a switch group,  and that within
  !                   the switch group,  m  is the next higher node after
  !                   but pointer chain is circular,  so the exception is
  !                   that the highest node points to the lowest.
  !       ktrlsw(6) --- fixed communication vector of /blank/.   cells
  !                     have the following meaning:
  !                   (1) = number of switch changes this time-step;
  !                   (2) = current number of closed switches;
  !                   (3) = current number of triangularizations of (y);
  !                   (4) = switch number of 1st switch of 1st group;
  !                   (5) = total number switch status changes of solution
  !                   (6) = 0 or 1 depending upon whether the
  !                         complicated or simple logic in "switch" is
  !                         to be used.   see "simple switch logic" of
  !                         emtp rule book (to get value of unity).
  !     for cases with few switches,  this is a case of overkill,  and
  !     a user who is short of memory might want to eliminate most of
  !     this subroutine.  such is possible (see above s.n. 3459).
  if ( iprsup  .ge.  1 ) write (lunit6, 3444)  ktrlsw
3444 format ( 34h top of "switch".  ktrlsw vector =,  10i7 )
  if ( iprsup  .ge.  2 ) write (lunit6, 3447)  ( modswt(j), j=1, n20 )
3447 format (39h (modswt(j), j=1, ktrlsw(1)) follow ...,/, (1x, 20i6))
  if ( iprsup  .lt.  3 )   go to 3459
  write (lunit6, 3451)  ( nextsw(i), i=1, kswtch )
3451 format ( 39h (nextsw(i), i=1, kswtch)  follows ....  ,/, ( 1x, 20i5 ) )
  write (lunit6, 3452)  ( lastsw(i), i=1, kswtch )
3452 format ( 39h (lastsw(i), i=1, kswtch)  follows ....  ,/, ( 1x, 20i5 ) )
  write (lunit6, 3453)  ( kbegsw(i), i=1, kswtch )
3453 format ( 39h (kbegsw(i), i=1, kswtch)  follows ....  ,/, ( 1x, 20i5 ) )
  write (lunit6, 3454)  ( kode(i), i=1, ntot )
3454 format ( 36h (kode(i), i=1, ntot)  follows  ....    ,/, ( 1x, 20i5 ) )
  !     choice of alternate switch logic is now to be made.
  !     immediately following is simple logic (much shorter code),
  !     which is fine for cases with few switches.   those short
  !     of memory could delete all but this (delete between
  !     bounding comment cards with "%%%%%%").    if this is
  !     done,  then also delete following "if"-statement:
3459 ktrlsw(5) = ktrlsw(5) + ktrlsw(1)
  if ( iprsup  .ge.  1 ) write (lunit6, 1004)  istep, ktrlsw
1004 format ( 39h top of "switch".  istep, ktrlsw(1:6) =, 10i6 )
  if ( ktrlsw(6) .eq. 0 )  go to 3605
  !     begin simple, brute-force switch logic of "simple switch logic"
  !     special-request card (see emtp rule book):
  call move0 ( kbegsw(1), kswtch )
  call move0 ( kode(1), ntot )
  j = ktrlsw(4)
  if ( j .le. 0 )  go to 3478
  !     enter loop around nextsw, as we note closed switches using kbegsw:
3473 kbegsw(j) = 1
  j = iabs ( nextsw(j) )
  if ( j .ne. ktrlsw(4) )  go to 3473
3478 call move0 ( nextsw(1), kswtch )
  n18 = ktrlsw(2)
  do ll=1, n20
     j = modswt(ll)
     if ( j .gt. 0 )  go to 3482
     j = -j
     kbegsw(j) = 0
     n18 = n18 - 1
     go to 3485
3482 kbegsw(j) = 1
     n18 = n18 + 1
3485 end do
  ktrlsw(2) = n18
  if ( n18 .gt. 0 ) go to 3487
  ktrlsw(4) = 0
  call move0( nextsw(1), kswtch )
  go to 4457
3487 n13 = 0
  n3 = 0
  !     enter loop  do 3580  over all switches,  in which each in turn
  !     is tested to see if it can be used for next switch-current
  !     calculation.   we execute this loop until n13 = n18 (until all
  !     closed switches have been properly ordered in nextsw:
3488 do i=1, kswtch
     if ( kbegsw(i) .eq. 0 )  go to 3580
     if ( nextsw(i) .ne. 0 )  go to 3580
     k = kmswit(i)
     ndx1 = lswtch + i
     m = kmswit(ndx1)
     n1 = kentnb(i)
     n2 = kentnb(i+1) - 1
     jj = 0
     mm = 0
     n9 = 0
     if ( k .ne. 1   .and. k .le. kpartb ) go to 3491
     !     known-voltage node k must be dropped from consideration:
     jj = 1
     n9 = n9 + 1
3491 if ( m .ne. 1   .and. m .le. kpartb ) go to 3493
     !     known-voltage node m must be dropped from consideration:
     mm = 1
     n9 = n9 + 1
3493 if ( n1 .gt. n2 )  go to 3506
     do ll=n1, n2
        j = nbhdsw(ll)
        if ( kbegsw(j) .eq. 0 )  go to 3503
        if ( nextsw(j) .ne. 0 )  go to 3503
        ndx2 = lswtch + j
        if ( jj .gt. 0 )  go to 3497
        if ( k .ne. kmswit(j)   .and. k .ne. kmswit(ndx2) ) go to 3497
        jj = 1
        go to 3501
3497    if ( mm .gt. 0 )  go to 3503
        if ( m .ne. kmswit(j)  .and. m .ne. kmswit(ndx2) ) go to 3503
        mm = 1
3501    n9 = n9 + 1
        if ( n9 .eq. 2 )  go to 3580
3503 end do
     !     we drop out of above  do 3503  loop only if one side of switch #i
     !     can be used for kcl calculation of switch current:
3506 if ( n13 .gt. 0 ) nextsw(n17) = i * nextsw(n17)
     n17 = i
     if ( jj .gt. 0 )  go to 3511
     nextsw(i) = 1
     go to 3518
3511 nextsw(i) = -1
3518 n13 = n13 + 1
     if ( n13 .eq. 1 )  n14 = i
     if ( kode(k) .ne. 0 )  go to 3528
     if ( kode(m) .ne. 0 )  go to 3528
     !     switch now closing is connected to no other closed switches, so
     !     each end points to the other (trivial kode change):
     kode(k) = m
     kode(m) = k
     go to 3563
3528 n6 = k
     if ( kode(k) .eq. 0 )  n6 = m
     n7 = k
     if ( n7 .eq. n6 ) n7 = m
     n19 = n7
     !     n6 was just one node of circular chain;  move till highest node:
3532 if ( kode(n6) .lt. n6 )  go to 3536
     n6 = kode(n6)
     go to 3532
3536 n15 = kode(n7)
     if ( n7 .le. n6   .and. n7 .ge. kode(n6) ) go to 3541
     !     insert node n7 as new max or new min:
     n16 = kode(n6)
     kode(n6) = n7
     kode(n7) = n16
     if ( n7 .gt. n6 )  n6 = n7
     go to 3556
     !     non-extrema insertion.  1st find insertion point:
3541 n9 = kode(n6)
3544 if ( n7 .lt. kode(n9) )  go to 3548
     n9 = kode(n9)
     go to 3544
     !     now we insert n7 after n9 but before kode(n9):
3548 n16 = kode(n9)
     kode(n9) = n7
     kode(n7) = n16
     !     we reach 3556 below after node n7 has been inserted in kode:
3556 if ( n15 .eq. 0 )  go to 3563
     if ( n15 .eq. n19 ) go to 3563
     !     n7-end was a chain (not isolated node), and we're not yet done:
     n7 = n15
     go to 3536
     !     we reach 3563 with kode correctly modified to reflect added switch
3563 if ( iprsup  .ge.  3 ) write (lunit6, 3564)  i
3564 format ( 30h complete processing of switch,  i5  )
3580 end do
  if ( iprsup  .ge.  1 ) write (lunit6, 3584)  n13, n17
3584 format ( 47h done another pass of all switches.  n13, n17 =,2i5)
  if ( n13 .gt. n3 )  go to 3591
  write (lunit6, 3587)
3587 format ( 47h temporary error stop in "switch" at s.n. 3587.  )
  call stoptp
3591 n3 = n13
  if ( n13 .lt. n18 )  go to 3488
  ktrlsw(4) = n14
  !     we are done,  having ordered all n18 of the closed switches.
  !     now only the circular end of nextsw must be patched on:
  nextsw(n17) = nextsw(n17) * n14
  go to 4457
  !     %%%%%%%%%%%%%%%%%%%  begin deletable sophisticated logic  %%%%%%%%
  !     %%%%% note: "lastsw" of deck "labcom" can be deleted, too,
  !     %%%%%       along with this block of sophisticated code.
  !     begin by opening all switches so ordered (negative modswt):
3605 do ll=1, n20
     if ( modswt(ll) .gt. 0 ) go to 4100
     n17 = -modswt(ll)
     k = kmswit(n17)
     ndx1 = lswtch + n17
     m = kmswit(ndx1)
     n3 = iabs ( lastsw(n17) )
     n4 = iabs ( nextsw(n17) )
     !     first test whether switch n17 constitutes a group all by itself:
     if ( lastsw(n17) .gt. 0  .or. kbegsw(n17) .ne. 0 ) go to 3621
     !     begin simplified logic for isolated switch n17 which now opens:
     kode(k) = 0
     kode(m) = 0
     go to 3664
     !     begin logic for group of 2 or more switches.   important special
     !     case here involves test for whether switch n17 is at the end of
     !     a radial.   look for such finger-switches first:
3621 n1 = kentnb(n17)
     n2 = kentnb(n17+1) - 1
     jj = 0
     mm = 0
     n9 = 0
     do i=n1, n2
        n7 = nbhdsw(i)
        if ( nextsw(n7)  .eq.  0 )   go to 3641
        ndx2 = lswtch + n7
        if ( jj  .gt.  0 )   go to 3627
        if ( k .ne. kmswit(n7)  .and. k .ne. kmswit(ndx2) ) go to 3627
        jj = 1
        n9 = n9 + 1
        go to 3635
3627    if ( mm .gt. 0 )  go to 3641
        if ( m .ne. kmswit(n7)  .and. m .ne. kmswit(ndx2) ) go to 3641
        mm = 1
        n9 = n9 + 1
3635    if ( n9 .eq. 2 ) go to 3750
3641 end do
     !     we fall out bottom of  do 3641  only if switch n17 is at end of
     !     radial.   this is finger-switch, with simple logic.    begin
     !     by finding node n16 which is out at end of radial:
     n16 = k
     if ( jj .gt. 0 )  n16 = m
     !     next extract node n16 (isolated end of finger-switch) from kode:
     n8 = n16
3652 n9 = kode(n8)
     if ( n9 .eq. n16 )  go to 3658
     n8 = n9
     go to 3652
3658 kode(n8) = kode(n16)
     kode(n16) = 0
     !     repair of "-" flag on lastsw that marks end of group, if needed:
3664 if ( lastsw(n17) .lt. 0 ) lastsw(n3) = -iabs( lastsw(n3) )
     !     next repair zero kbegsw that marks beginning of group, if needed:
     if ( kbegsw(n17) .eq. 0 ) kbegsw(n4) = 0
     !     next establish new forward linkage nextsw around now-open n17:
     n15 = nextsw(n3)
     nextsw(n3) = n4
     if ( n15 .lt. 0 ) nextsw(n3) = -n4
     !     next establish new backward linkage lastsw around now-open n17:
     n15 = lastsw(n4)
     lastsw(n4) = n3
     if ( n15 .lt. 0 ) lastsw(n4) = -n3
     if ( ktrlsw(4) .eq. n17 )  ktrlsw(4) = n4
     if ( ktrlsw(4) .eq. n17 )  ktrlsw(4) = 0
     go to 4095
     !     begin logic for group-splitting (switch n17 not at end of radial):
3750 mo = kode(k)
     if ( mo .eq. 0 ) go to 3744
     kode(k) = 0
     k = mo
     go to 3750
3744 m1 = iabs( lastsw(n17) )
3746 if ( kbegsw(m1) .eq. 0 ) go to 3754
     kbegsw(m1) = 2
     m1 = lastsw(m1)
     go to 3746
3754 kbegsw(m1) = 2
     l1 = lastsw(m1)
     m2 = n17
3756 kbegsw(m2) = 2
     if ( lastsw(m2) .lt. 0 ) go to 3758
     m2 = iabs( nextsw(m2) )
     go to 3756
3758 l2 = iabs( nextsw(m2) )
     m7 = 1
     m3 = m2
3760 if ( m3 .ne. n17 ) go to 3764
     m3 = iabs( lastsw(m3) )
3764 kbegsw(m3) = 0
     j1 = kmswit(m3)
     j2 = kmswit( m3 + lswtch )
     n1 = kentnb(m3)
     n2 = kentnb(m3 + 1) - 1
     do i = n1, n2
        n7 = nbhdsw(i)
        if ( nextsw(n7) .eq. 0 ) go to 3774
        if ( kbegsw(n7) .eq. 2 ) go to 3774
        k1 = kmswit(n7)
        k2 = kmswit( n7 + lswtch )
        jj = j1
        if (j1 .eq. k1 .or. j1 .eq. k2)  jj = j2
        mm = j1
        if ( mm .eq. jj )  mm = j2
        go to  3788
3774 end do
     !     processing switches which are no processed switch connected
     kode(j1) = j2
     kode(j2) = j1
     if ( m7 .ne. 1 )  go to 3776
     !     this is ist processing switch and the last switch of group 1
     if ( nextsw(m3) .lt. 0 )  m7 = -1
     nextsw(m3) = m3 * m7
     m4 = lastsw(m3)
     lastsw(m3) = -m3
     m3 = iabs(m4)
     m7 = 0
     go to 3760
3776 n7 = 6666
     !     now we break processed switch chain and put m3 in the chain
     m4 = m2
3778 if ( kbegsw(m4) .eq. 0 )  go to 3780
     m4 = iabs( lastsw(m4) )
     go to 3778
3780 if (n7 .ne. 6666) kbegsw(m4)=1
     m5 = nextsw(m3)
     nextsw(m3) = m4
     if ( m5 .lt. 0 )  nextsw(m3) = -m4
     m5 = lastsw(m4)
     lastsw(m4) = m3
     if ( m5 .lt. 0 )  lastsw(m4) = -m3
     m5 = iabs(m5)
     m4 = nextsw(m5)
     nextsw(m5) = m3
     if ( m4 .lt. 0 ) nextsw(m5) = -m3
     m6 = lastsw(m3)
     lastsw(m3) = m5
     if ( n7 .eq. 6666 )  lastsw(m3) = -m5
     if ( m3 .eq. m1 )  go to 3795
     m3 = m6
     go to 3760
     !     m3 connects to n7 and becomes the first switch in n7 switch group
3788 m4 = n7
3784 nmin = kode(mm)
     if ( nmin .lt. mm ) go to 3790
     mm = nmin
     go to 3784
     !     m3 is just at the end of a radial.  put node jj in the kode chain
3790 if ( jj .lt. mm  .and.  jj .gt. nmin )  go to 3792
     !     in the "kode" chain,  node jj is larger than mm or less than nmin
     kode(mm) = jj
     kode(jj) = nmin
     go to 3778
3792 mm = kode(nmin)
     if ( mm .gt. jj )  go to 3794
     nmin = mm
     go to 3792
3794 kode(nmin) = jj
     kode(jj) = mm
     go to 3778
3795 if ( l2 .eq. m1 )  go to 4095
     !     we found other switch group. break just processing switch vettors
     !     and form the whole swich vectors.   m1 is still 1st of the group.
     m2 = lastsw(m1)
     lastsw(m1) = l1
     if ( m2 .lt. 0 )  lastsw(m1) = -l1
     m2 = iabs(m2)
     m1 = nextsw(m2)
     nextsw(m2) = l2
     if ( m1 .lt. 0 )  nextsw(m2) = -l2
     m1 = lastsw(l2)
     lastsw(l2) = m2
     if ( m1 .lt. 0 )  lastsw(l2) = - m2
4095 nextsw(n17) = 0
     lastsw(n17) = 0
     kbegsw(n17) = 1
     ktrlsw(2) = ktrlsw(2) - 1
     if ( iprsup  .ge.  2 ) write (lunit6, 4096)  ll, n17, k, m
4096 format ( 44h done with another opening.  ll, n17, k, m =,  10i5 )
4100 end do
  !     next close all switches so ordered (positive modswt):
  do ll=1, n20
     if ( modswt(ll) .lt. 0 )  go to 4450
     n17 = modswt(ll)
     k = kmswit(n17)
     ndx1 = lswtch + n17
     m = kmswit(ndx1)
     n1 = kentnb(n17)
     n2 = kentnb(n17+1) - 1
     mm = 0
     n4 = 0
     if ( n2 .lt. n1 )  go to 4120
     do i=n1, n2
        n7 = nbhdsw(i)
        if ( nextsw(n7)  .eq.  0 )   go to 4118
        ndx2 = lswtch + n7
        if ( n4  .gt.  0 )   go to 4113
        jj = n7
        n5 = k
        if (k .eq. kmswit(jj)) n5 = m
        if ( k .eq. kmswit(ndx2) )  n5 = m
        n4 = k
        if ( n4 .eq. n5 )  n4 = m
        go to 4118
4113    if ( kmswit(n7) .ne. n5  .and. kmswit(ndx2) .ne. n5 )  go to 4118
        !     we have found group 2 switch (connected to node n5):
        mm = n7
        go to 4126
4118 end do
     if ( n4 .gt. 0 )  go to 4300
     !     no connected switches which are closed is easy special case:
4120 kode(k) = m
     kode(m) = k
     m1 = ktrlsw(4)
     ktrlsw(4) = n17
     kbegsw(n17) = 0
     if ( m1 .ne. 0 )  go to 4122
     nextsw(n17) = n17
     if ( k .gt. kpartb  .or.  k .eq. 1 )  nextsw(n17) = -n17
     lastsw(n17) = -n17
     go to 4130
     !     ready to check for illegal switch loop, and also voltage source:
4126 n6 = 0
     n2 = n4
     !     enter loop over nodes in group 1, to check for loops and sources:
4135 n2 = kode(n2)
     if ( n2 .eq. 1 )       n6 = 1
     if ( n2 .gt. kpartb )  n6 = 1
     if ( n2  .ne.  n5 )   go to 4147
     kill = 64
     lstat(19) = 4147
     bus1 = bus(n2)
     go to 4457
4147 if ( n2 .ne. n4 )   go to 4135
     !     ok, so no switch loops.  but we might have to swap groups 1 and
     !     2 --- required if group 1 had a voltage source:
     if ( n6 .eq. 0 )  go to 4300
     !     first, swap known switches in groups (jj and mm):
     n12 = mm
     mm = jj
     jj = n12
     !     next, swap nodes n4 and n5 (group 1 and group 2 connections):
     n12 = n5
     n5 = n4
     n4 = n12
4300 nmax = n4
     n3 = n5
4302 nmin = kode(nmax)
     if (nmin .lt. nmax)  go to 4304
     nmax = nmin
     go to 4302
4304 m5 = kode(n3)
     if ( n3 .lt. nmin ) go to 4310
     if ( n3 .gt. nmax ) go to 4310
     nless = nmin
4306 nover = kode(nless)
     if ( nover .gt. n3 )  go to 4308
     nless = nover
     go to 4306
4308 kode(nless) = n3
     kode(n3)= nover
     go to 4312
4310 kode(nmax) = n3
     kode(n3) = nmin
4312 if (m5 .eq. 0) go to 4314
     if ( m5 .eq. n5 )  go to  4314
     if ( n3 .lt. nmin )  nmin = n3
     if ( n3 .gt. nmax )  nmax = n3
     n3 = m5
     go to 4304
4314 m1 = jj
4316 if (kbegsw(m1) .eq. 0)  go to 4318
     m1 = iabs( lastsw(m1) )
     go to 4316
4318 if (mm .ne. 0) go to 532
     if (n5 .gt. kpartb .or. n5 .eq. 1) go to 532
     !     n17 can be 1st switch of the group now, so it is easy to process
     kbegsw(n17) = 0
     kbegsw(m1) = 1
     if (ktrlsw(4) .eq. m1) ktrlsw(4) = n17
4122 m3 = lastsw(m1)
     m2 = iabs( m3 )
     nextsw(n17) = m1
     if (n4 .eq. 0)  go to 4124
     if (n5 .eq. m) nextsw(n17) = -m1
     lastsw(n17) = m2
     go to 4128
4124 if (k .gt. kpartb .or. k .eq. 1) nextsw(n17) = -m1
     lastsw(n17) = -m2
4128 lastsw(m1) = n17
     if (m3 .lt. 0) lastsw(m1) = -n17
     if (nextsw(m2) .lt. 0)  n17 = -n17
     nextsw(m2) = n17
     go to 4130
     !     now enter general logic to process group of two or more switches
532  kbegsw(n17) = 1
     n3 = m1
4322 n8 = n3
     if (nextsw(n3) .lt. 0) n8 = lswtch + n3
     if (kmswit(n8) .eq. n4)  go to 4360
     if ( lastsw(n3) .lt. 0 )  go to  4324
     n3 = iabs( nextsw(n3) )
     go to 4322
     !     all jj switches of jj group are processed .   now processing n17
4324 lastsw(n3) = -lastsw(n3)
     m3 = nextsw(n3)
     m2 = iabs(m3)
     nextsw(n3) = n17
     if (m3 .lt. 0)  nextsw(n3) = -n17
     !     n17 is in the end of a radial.   we begin to process switch n17.
4399 if ( mm .ne. 0 ) go to 4330
     lastsw(n17) = -n3
     if (m1 .eq. m9 .and. m1 .eq. m2) m2=ktrlsw(4)
4325 nextsw(n17)=m2
     if (n4 .eq. m)  nextsw(n17) = -m2
     if (lastsw(m2) .lt. 0) n17 = -n17
     lastsw(m2) = n17
     go to 4130
     !     now processing switch n17  which connected  another switch group
4330 lastsw(n17) = n3
     l1 = mm
4332 if (kbegsw(l1) .eq. 0) go to 4334
     l1 = iabs( lastsw(l1) )
     go to 4332
4334 kbegsw(l1) = 1
     if (ktrlsw(4) .eq. l1) ktrlsw(4) = m1
     if (m2 .eq. l1) go to 4325
     !     a little trouble.  first extract 2nd group from switch chain and
     !     then connect the three switch groups carefully.
     l2 = mm
4336 if (lastsw(l2) .lt. 0) go to 4338
     l2 = iabs( nextsw(l2) )
     go to 4336
4338 l3 = iabs( lastsw(l1) )
     l4 = iabs( nextsw(l2) )
     l5 = l4
     if (nextsw(l3) .lt. 0) l5 = -l5
     nextsw(l3) = l5
     if ( lastsw(l4) .lt. 0 ) l3 = -l3
     lastsw(l4) = l3
     nextsw(n17) = l1
     if (n4 .eq. m)  nextsw(n17) = -l1
     if (lastsw(l1) .lt. 0) n17 = -n17
     lastsw(l1) = n17
     l5 = m2
     if (nextsw(l2) .lt. 0)  l5 = -l5
     nextsw(l2) = l5
     if (lastsw(m2) .lt. 0)  l2 = -l2
     lastsw(m2) = l2
     go to 4130
4360 if (lastsw(n3) .gt. 0) go to 4364
     nextsw(n3) = -nextsw(n3)
     go to 4324
     !     process switches which need changing "nextsw" order in the group
4364 nextsw(n17) = 1
     m3 = n3
     m9 = m3
     nn = 0
4368 kbegsw(n3) = 2
     nn = nn + 1
     if (lastsw(n3) .lt. 0) go to 4370
     n3 = iabs( nextsw(n3) )
     go to 4368
4370 m2 = iabs(nextsw(n3))
     nextsw(n3)=m3
     kbegsw(n17) = 2
     m4 = lastsw(m3)
     kk2 = 0
     ll2 = 1
     if ( nextsw(m4) .lt. 0 )  ll2 = -1
     if (m1 .eq. m3 .and. m1 .eq. m2) m4 = 0
     n3 = iabs(nextsw(m3))
4374 k1 = kmswit(n3)
     k2 = kmswit( lswtch + n3 )
     n1 = kentnb(n3)
     n2 = kentnb(n3+1)-1
     j1 = 0
     j2 = 0
     numb = 0
     do i = n1, n2
        k7 = nbhdsw(i)
        if ( nextsw(k7) .eq. 0 ) go to 4380
        if (kbegsw(k7) .ne. 2) go to 4380
        k3 = kmswit(k7)
        k4 = kmswit( lswtch + k7 )
        if ( j1 .eq. 1 ) go to 4372
        if (k1 .ne. k3 .and. k1 .ne. k4) go to 4372
        j1 = 1
        numb = numb+1
        go to 4376
4372    if ( j2 .eq. 1 ) go to 4380
        if (k2 .ne. k3 .and. k2 .ne. k4) go to 4380
        j2 = 1
        numb = numb+1
4376    if (numb .ne. 2) go to 4380
        m3 = n3
        go to 4384
4380 end do
     kk2 = kk2 + 1
     if (m4 .ne. 0) go to 4382
     ktrlsw(4) = n3
     lastsw(n3) = 0
     kbegsw(n3) = 0
     go to 4383
4382 kbegsw(n3) = 1
     if (m9 .eq. m1 .and. kk2 .eq. 1) kbegsw(n3) = 0
     lastsw(n3) = m4
     nextsw(m4) = n3 * ll2
     ll2 = 1
4383 if ( j1 .eq. 1 )  ll2 = -1
     if (kk2 .eq. nn) go to 4388
     m4 = n3
     nextsw(m3) = nextsw(n3)
4384 n3 = iabs( nextsw(n3) )
     go to 4374
4388 nextsw(n3) = n17*ll2
     kbegsw(n17) = 1
     go to 4399
4130 ktrlsw(2) = ktrlsw(2) + 1
     if ( iprsup  .ge.  2 ) write (lunit6, 4264)  ll, n17, k, m
4264 format ( 44h done with another closure.  ll, n17, k, m =,  10i5 )
4450 end do
  !     %%%%%%%%%%%%%%%%%%%  end   deletable sophisticated logic  %%%%%%%%
4457 if ( iprsup  .lt.  2 )   go to 4465
  write (lunit6, 3451)  ( nextsw(i), i=1, kswtch )
  write (lunit6, 3452)  ( lastsw(i), i=1, kswtch )
  write (lunit6, 3453)  ( kbegsw(i), i=1, kswtch )
  write (lunit6, 3454)  ( kode(i), i=1, ntot )
4465 if ( iprsup  .ge.  1 ) write (lunit6, 4468)
4468 format ( 15h exit "switch".  )
  return
end subroutine switch
!
!     subroutine tacs3.
!
subroutine  tacs3
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'tacsar.ftn'
  include 'syncom.ftn'
  !     intrinsic  absz, cosz
  if ( iprsup  .ge.  1 ) write ( lunit6, 4567 )
4567 format ( 23h  "begin module tacs3." )
  niunrs = iuty( kiuty + 1 )
  kjsup = kinsup + lstat(65)
  kksup = kjsup  + lstat(65)
  ma1 = iuty(kiuty+7)
  ma2 = iuty(kiuty+8)
  if ( iprsup  .lt.  2 )   go to 3219
  write (lunit6, 3213)  ioutcs, isprin, isplot, limstp, iout
3213 format (   40h  ioutcs  isprin  isplot  limstp    iout ,/,  5i8 )
  write (lunit6, 3214) ktab, niu, nuk, nenerg, iuty(kiuty+11)
3214 format ( 40h    ktab     niu     nuk  nenerg  infexp  ,/, 1x, i7, 12i8)
  write (lunit6, 3215)  t, twopi, fltinf, xopt, copt
3215 format ( 1x, 14x, 1ht,  10x, 5htwopi, 9x, 6hfltinf, 11x, 4hxopt,  11x, 4hcopt   ,/, 1x, 8e15.6  )
  !                                       $$$  update  input  sources  $$$
3219 if ( nchain  .eq.  18 )  go to 900
  ndx1 = kxtcs + nuk + 1
  xtcs( ndx1) = t
  ndx1 = ndx1 + 1
  xtcs( ndx1) = istep
  ndx1 = ndx1 + 1
  xtcs( ndx1) = deltat
  if ( konsce .lt. niunrs )  go to 505
  i = niunrs
  ndy5 = kud1
3456 n1 = iuty(kiuty+i)
  j = i + nuk
510 ndx1 = kxtcs + j
  xtcs(ndx1) = 0.0
  n13 = 0
  d1 = absz( ud1(ndy5+5) - ud1(ndy5+4) - onehaf )
  if ( d1 .lt. flzero )  n13 = 1
  if (iprsup .ge. 6)  write ( lunit6, 8637 ) istep, i, ud1(ndy5+5), ud1(ndy5+4), d1
8637 format ( /,  1x,  16h   istep       i,  17x,  3hud5,  17x, 3hud4,  18x,  2hd1  ,/,  1x,  2i8,  3e20.11  )
  if ( t .lt. ud1(ndy5+4) - flzero *10. )   go to 500
  if ( t .ge. ud1(ndy5+5) - flzero *10. )   go to 500
  if ( n1  .lt.  90 )   go to 501
  k = ud1( ndy5 + 2 )
  l = iabs( kpos( k))
  if ( n1 .gt. 93 )  go to 500
  n2 = n1 - 89
  go to (502, 503, 504, 506), n2
502 xtcs(ndx1) = emtpe( k)
  go to 500
503 if ( n13 .eq. 0 ) go to 509
  if ( l .eq. 11 ) xtcs(ndx1) = tclose(k)
  go to 500
509 if ( l .le. 3 )  xtcs(ndx1) = tclose( k)
  go to 500
504 xtcs(ndx1) = etac( k)
  go to 500
506 if ( l .le. 3 )  xtcs(ndx1) = 1.0
  go to 500
501 xtcs(ndx1) = ud1(ndy5+1)
  if ( n1  .eq.  11 )  go to 500
  if ( n1  .ne.  14 )  go to 511
  xtcs(ndx1) = xtcs(ndx1) * cosz(twopi * ud1(ndy5+3)  * t  + ud1(ndy5+2)  )
511 if ( n1 .ne. 23 )  go to 512
  if ( t .lt. ud1(ndy5+4) + ud1(ndy5+2) - flzero * 10. ) go to 500
  ud1(ndy5+4) = ud1(ndy5+4) + ud1(ndy5+3)
  go to 510
512 if ( n1 .ne. 24 )  go to 500
  if ( t .lt. ud1(ndy5+4)+ ud1(ndy5+3)- flzero*10. )  go to 513
  ud1(ndy5+4) = ud1(ndy5+4) + ud1(ndy5+3)
  go to 512
513 ndx2 = kxtcs + j
  xtcs(ndx2) = xtcs(ndx2) * ( t - ud1(ndy5+4) ) / ud1(ndy5+3)
  if ( absz( xtcs(ndx2) )  .le.  10.*flzero ) xtcs(ndx2) = 0.0
500 i = i + 1
  ndy5 = ndy5 + 5
  if ( i  .le.  konsce )  go to 3456
505 if ( iprsup  .lt.  6 )  go to 3030
  write ( lunit6, 3241 )
3241 format ( /,  21h tables in  'tacs3' .  ,/, 5x, 3hrow, 4x, 4hiuty, 6x, 2hiu, 4x, 4hkpos, 14x, 1he, 12x, 3hud1, 12x, 3hud2, 12x, 3hud3, 9x, 6htclose, 11x, 4hxtcs )
  ndx5 = kud1 - 5
  do k = 1, konsce
     ndx1 = ilntab( kaliu + k )
     if (  k  .gt.  11 )  go to 4848
     write ( lunit6, 3344 ) k, iuty(kiuty+k), texvec(ndx1), kpos(k), emtpe(k), tclose(k), xtcs(kxtcs+nuk+k)
3344 format ( 2i8, 2x, a6, i8, e15.6, 45x, 2e15.6 )
     go to 4040
4848 ndx5 = ndx5 + 5
     write ( lunit6, 3244 ) k,iuty(k+kiuty),texvec(ndx1),kpos(k),emtpe(k),ud1(ndx5+1), ud1(ndx5+2),ud1(ndx5+3),tclose(k),xtcs(kxtcs+nuk+k)
3244 format (  2i8, 2x, a6, i8, 6e15.6  )
4040 end do
3030 l = iuty( kiuty + 4 )
  if ( l .gt. 0 ) call csup( l )
  !                                                 $$$  form  rhside  $$$
  ndx1 = nuk * 4
  do i = 4, ndx1, 4
3231 rsblk(krsblk+i) = 0.0
  end do
  nuki = kisblk - 8
  do i = 1, nuk
     nuki = nuki + 8
     if ( isblk(nuki+2)  .eq.  1 )  go to 304
     k = kprsup + isblk(nuki+3) + 4
     j = krsblk + isblk(nuki+4) * 4
     rsblk(j) = rsblk(j) + parsup(k)
304 end do
  !                                          $$$  forward  on  rhside  $$$
  nuki = kisblk - 8
  nukr = krsblk - 4
  do i=1,nuk
     nuki = nuki + 8
     nukr = nukr + 4
     m = isblk(nuki+8)
301  k = icolcs( kcolcs + m )
     if (k .ge. i) go to 305
     ndx4 = krsblk + k * 4
     rsblk(nukr+4) = rsblk(nukr+4) - atcs(katcs+m)*rsblk(ndx4)
     m = m + 1
     go to 301
305  rsblk(nukr+4) = rsblk(nukr+4) * atcs(katcs+m)
  end do
  !                                           $$$  backward  on  xtcs  $$$
327 mm = 1
  nuki = kisblk + 8 * nuk
  nukr = krsblk + 4 * nuk
  do i=1,nuk
     nuki = nuki - 8
     nukr = nukr - 4
     j = nuk + 1 - i
329  ndx1 = kxtcs  + j
     xtcs(ndx1) = rsblk(nukr+4)
     m = isblk(nuki+8)
     n = ia
     if ( j .ne. nuk )  n = isblk(nuki+16) - 1
309  if ( icolcs(kcolcs+m) .eq. j )  go to 334
     m = m + 1
     go to 309
334  m = m + 1
     if ( m .gt. n )  go to 324
     k = icolcs( kcolcs + m )
     ndx4 = kxtcs  + k
     xtcs(ndx1) = xtcs(ndx1) - atcs(katcs+m) * xtcs(ndx4)
     go to 334
324  j1 = isblk(nuki+5)
     k1 = isblk(nuki+6)
     if ( j1 .gt. 0 )  rsblk(nukr+2)  = xtcs( kxtcs + j1 )
     if ( k1 .gt. 0 )  rsblk(nukr+3)  = xtcs( kxtcs + k1 )
     if (xtcs(ndx1) .lt. rsblk(nukr+2)) xtcs(ndx1) = rsblk(nukr+2)
     if (xtcs(ndx1) .gt. rsblk(nukr+3)) xtcs(ndx1) = rsblk(nukr+3)
     if ( rsblk(nukr+2) .le. rsblk(nukr+3) ) go to 308
     if ( iuty(kiuty+3)  .eq.  0 )  go to 308
     ndx1 = ilntab( klntab + j )
     write (lunit6, 198)  texvec(ndx1), t
198  format(5x, 'Warning. ---- the variable limits which apply to the tacs function block with (output) name  ', a6,  "'  have",  /, &
          19x,  83hcriss-crossed.   that is, the upper limit is now less than the lower limit, at time,   e14.5,  10h  seconds.    ,/, &
          19x, 105halthough the simulation will continue, strange results might be expected (be skeptical).   there shall be     ,/, &
          19x,  85hno warning message for any subsequent criss-crossing of the limits of any tacs block.      )
     iuty(kiuty+3) = iuty(kiuty+3) - 1
308  l = isblk( nuki + 7 )
     if ( l .gt. 0 )  call csup( l )
3088 end do
  !                                                       $$$  output  $$$
317 do i =1, nsup
     n1 = - insup( kjsup + i )
     if ( n1 .lt. 0 )  go to 1010
     if ( insup(kksup+i) .ne. 53 )  go to 1010
     j = ivarb( n1 + 1 )
     k = ivarb( n1 + 2 )
     b = 0.0
     do n= j, k
        m = ksus( kalksu + n )
601     b = b + xtcs(kxtcs+m) * ksus(kksus+n)
     end do
     nn = ivarb(n1)
     n5 = parsup(nn)
     n6 = parsup( nn + 2 )
     n7 = ivarb( n1 + 4 )
     ndx6 = kprsup + n7
     parsup( ndx6) = b
     n7 = n7 + 1
     if (n7 .eq. n5+n6) n7 = n5
     ivarb(n1+4) = n7
1010 end do
  if ( t  .gt.  tmax )  go to 9900
  !                                                  $$$  update  hst  $$$
  if ( nuk  .eq.  0 )  go to 340
  nuki = kisblk - 8
  nukr = krsblk - 4
  do i = 1, nuk
     nuki = nuki + 8
     nukr = nukr + 4
     if ( isblk(nuki+2)  .eq.  1 ) go to 3111
     l = isblk( nuki + 4 )
     prx = xtcs( kxtcs + l )
     j = iabs( isblk(nuki+1) )
     k = iabs( isblk(nuki+9) ) - 1
     if (  i  .eq.  nuk )  k = nsu
     if ( isblk(nuki+1) .gt. 0 )  go to 2345
     do i1 = ma1, ma2
        if ( ivarb(i1) .ne. -i )  go to 5656
        ma3 = i1 + 1
        do i2 = ma3, ma2
           if ( ivarb(i2) .lt. 0 ) go to 2345
4545       ma4 = i2
        end do
        go to 2345
5656 end do
2345 pru = 0.0
     do m=j,k
        n = ksus( kalksu + m )
        if ( isblk(nuki+1) .gt. 0 ) go to 5678
        n1 = n - lstat(64) - nuk
        do i3 = ma3, ma4
           if ( ivarb(i3) .ne. n1 ) go to 6789
           n = n + lstat(68)
           go to 5678
6789    end do
5678    ndx1 = kxtcs  + n
        pru = pru + xtcs(ndx1) * ksus( kksus + m )
312  end do
     jcm = kprsup + isblk( nuki + 3 )
     k = isblk( nuki + 2 ) - 1
     j = jcm + 6 * k - 2
     n1 = krsblk + l * 4 - 2
     n2 = n1 + 1
     if (prx .eq. rsblk(n1) .or. prx .eq. rsblk(n2)) go to 2810
     if ( k .eq. 1 )  go to 330
     n22 = jcm + 6
     do n = n22, j, 6
        ppp  = parsup(n)*pru - parsup(n+1) * prx - parsup(n-2)
331     parsup(n-2) = ppp + parsup(n+4)
     end do
330  parsup(j) = parsup(j+2) * pru - parsup(j+3) * prx
     go to 311
     !                               $$$  dynamic  limiter  hst  $$$
2810 n = j
     parsup(j) = parsup(j+2) * pru  -  parsup(j+3) * prx
2840 if ( k .eq. 1 )  go to 311
     k = k - 1
     n = n - 6
     parsup(n) = (parsup(n+2)*pru-parsup(n+3)*prx+parsup(n+6))/2.0
     go to 2840
311  if ( iprsup  .ge.  4 ) write (lunit6, 2020)  i, ( parsup(n+4), n = jcm, j, 6 )
2020 format( 10h  function, i6, 6h  hst , 7e14.6 )
3111 end do
  ndx1 = kxtcs + nuk + lstat(64) + 1
  ndx2 = ndx1 + lstat(68)
  call mover ( xtcs(ndx1), xtcs(ndx2), nsup )
340 return
  !                                                  $$$  termination  $$$
900 l = nuk + lstat(64) + konsup + 1
  call csup( l )
9900 if ( iprsup  .ge.  1 ) write (lunit6, 901)
901 format( /,  44h normal termination of run within  'tacs3' .   )
  return
end subroutine tacs3
!
!     subroutine subts2.
!
subroutine subts2
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  !     %include  '//c/tsu/cables.ins.ftn'
  real(8) real_bus2, real_bus4
  equivalence (real_bus2, bus2), (real_bus4, bus4)
  equivalence (h1, sk), (d2, sm)
  equivalence (iprsov(35), ipoint)
  !  wk1( koff1 + k ) << vm5dt( k ); wk1( koff2 + k ) << vk5dt( k )
  !  wk1( koff3 + k ) <<<  fm5( k ); wk1( koff4 + k ) <<<  bk5( k )
  !  wk1( koff5 + k ) <<<  vk5( k ); wk1( koff6 + k ) <<<  vm5( k )
  !  wk1( koff7 + k ) <<<  hk2( k ); wk1( koff8 + k ) <<<  hm2( k )
  !  wk1( koff9 + k ) <<<  hk3( k ); wk1( koff10 + k )<<<  hm3( k )
  !  wk1( koff13 + k ) <<< hk1( k ); wk1( koff14 + k )<<<  hm1( k )
  !  wk1( koff15 + k ) <<<  hk( k ); wk1( koff16 + k )<<<   hm( k )
  !  wk1( koff17 + k ) <<<vkdt( k ); wk1( koff18 + k ) <<<vmdt( k )
  dimension wk1(1)
  equivalence (semaux(1), wk1(1))
  dimension infdli(1)
  equivalence (namebr(1), infdli(1))
  common /fdqlcl/ koff1, koff2, koff3, koff4, koff5, koff6, koff7,koff8, koff9, koff10, koff13, koff14, koff15, koff16, koff17
  common /fdqlcl/ koff18, koff19, koff20, koff21, koff22, koff23, koff24,koff25, inoff1, inoff2, inoff3, inoff4, inoff5, nqtt, lcbl
  common /fdqlcl/ lmode, nqtw
  !     intrinsic  iabsz, absz
  if ( iprsup .ge. 1 ) write (lunit6, 3445) ( emtpf(j), j=1, ntot )
3445 format ( ' top  subts2.  emtpf(1:ntot) follows ...' ,/,( 1x, 8e16.7 ) )
  nmodal = kcount
  isecti = 400
  k=1
  llm1 = -1
  ll0 = 0
  ll1 = 1
  iftail = 0
  !                                      initialize counters for the -666
  ibf = 0
  isfd = 0
  ikf = 0
  kaa = 0
  !                                        accumulation of node currents f
  !                                                          updating line
1100 it2=length(k)
  n1=kbus(k)
  if ( iprsup .ge. 1 ) write (*,*) 'top of subts2 ,n1=', n1
  if (n1.lt.0) go to 1180
  it2=iabs(it2)
  if (it2.eq.1) go to 1150
1105 it1=k
  if ( length( k + 1 ) .eq.  -555 )  go to 3535
  do i=1,it2
     n1=kbus(it1)
     n2=mbus(it1)
     volti(i)=-emtpe(n1)
     voltk(i)=-emtpe(n2)
     volt(i)=voltk(i)-volti(i)
1110 it1=it1+1
  end do
  n3=nr(k)
  if (t.gt.tmax) go to 1190
  !     check for the presence of the -666 branches  *   *   *   *   *   *
  if ( length(k+1) .eq. -666 ) go to 3531
  call mult(emtpc(n3), volti, ci(k), it2, llm1)
  call mult(emtpc(n3), voltk, ck(k), it2, llm1)
  if (kodebr(k) .le. 0) go to 3530
  call mover( cik(k), volti(1), it2 )
  call mult( x(n3), volt(1), volti(1), it2, ll1 )
  call mult( r(n3), volti(1), cik(k), it2, llm1 )
  do i=1, 10
     if ( istep .le. 2   .and.   iprsup .ge. 3 ) write (lunit6, 73530)  i, kbus(i), mbus(i), nr(i), length(i), &
          kodebr(i), r(i), x(i), tr(i), tx(i), emtpc(i), volt(i), volti(i), cik(i)
73530 format (/, ' Update of  i(t-deltat)  at 73530.   ', 6i12, /, (1x, 5e25.15))
73529 end do
  go to 3535
3531 call fdcinj( ikf, isfd, ibf)
  call mover( volt(1), cik(k), it2 )
  go to 3535
3530 call mult( x(n3), cik(k), volt(1), it2, ll1 )
  call mult( r(n3), volt(1), cik(k), it2, llm1 )
3535 it1 = k + it2 - 1
  do i=k,it1
     n1=kbus(i)
     n2=mbus(i)
     emtpf(n1)=emtpf(n1)-cik(i)-ci(i)
1120 emtpf(n2)=emtpf(n2)+cik(i)-ck(i)
  end do
  !
  !
  !
  !
  go to 1200
1150 i=nr(k)
  msign=mbus(k)
  n2=iabs(msign)
  gus1=emtpe(n1)
  gus2=emtpe(n2)
  gus3=gus1-gus2
  a=cik(k)
  ci1=ci(k)
  ck1=ck(k)
  it1=iabs(i)
  cap = emtpc(it1)
  if ( iprsup .ge. 3 ) write (lunit6, *)  ' it1, n1, n2, k, i, t =', it1, n1, n2, k, i, t
  if ( iprsup  .ge.  3 ) write (lunit6,5432) x(it1), emtpe(n1), emtpe(n2), cik(k), r(it1)
5432 format (' x(it1), emtpe(n1), emtpe(n2), cik(k), r(it1) = ', 5e18.10, /)
  if (i.lt.0) go to 1170
  if (t.gt.tmax) go to 1105
  a=(gus3+a*x(i))*r(i)-a
  gus4=-cap *gus1-ci1
  ck1=-cap *gus2-ck1
  emtpf(n1)=emtpf(n1)-gus4
  emtpf(n2)=emtpf(n2)-ck1
1160 emtpf(n1)=emtpf(n1)-a
  emtpf(n2)=emtpf(n2)+a
  cik(k)=a
  ci(k)=gus4
  ck(k)=ck1
  go to 1200
1170 gus3=x(it1)*gus3
  gus4=gus3+a
  if (cap.eq.0.) go to 1175
  ck1=ck1+cap *(gus4+ci1)
  gus3=gus3-2.0*x(it1)*ck1
1175 a=gus3-r(it1)*gus4
  if (msign.gt.0) go to 1160
  gus3=gus4
  d1 = n1
  if ( ck1  .eq.  d1 )   gus3 = gus4 + ci(k-1)
  kcount=kcount+1
  bvalue(kcount)=gus3
  go to 1160
1180 n1 = -n1
  n2=mbus(k)
  if ( imodel(k) .ne. -4 ) go to 4340
  if ( it2 .lt. 0 ) go to 481
481 it2 = -it2
120 ky = k
  koff11 = koff25 + 288
  do i = 1, isecti
     wk1( koff8 + i ) = 0.d0
     wk1( koff7 + i ) = 0.d0
     wk1( koff6 + i ) = 0.d0
     wk1( koff5 + i ) = 0.d0
     wk1( koff9 + i )  = 0.0
     wk1( koff10 + i ) = 0.0
     ky = ky + 1
103 end do
  if ( iprsup .gt. 0 ) write ( *,*)'+++++before updating history source++++ t=', t
  kq = infdli(inoff1+k)
  kqk0 = infdli(inoff2+k)
  kh = k
  nnq1 = infdli(inoff3+k)
  !      lj = kq                                 ! 1st pole of qii for the
  !      if ( kq .gt. 1) lj = ( kq-1 ) * 2 + 1
  !      kiline = lpast                                  !next cell is fre
  !      kiline = nr(k)
  !      lj = kiline + lj
  lj = nr(k)
113 do j=1,it2
     ky = k
     do i=1,it2
        nn1 = -kbus(ky)
        nn2 = iabs( mbus(ky) )
        gkt = sconst(kqk0) * emtpe(nn1)
        gmt = sconst(kqk0) * emtpe(nn2)
        nteq = wk1(koff20+kq)
        if (  lint .eq. 1 ) go to 9824
        n1 = kodsem(ky)
        nrz = cki(ky)
        nra = ckkjm(ky)
        nk1 = n1 + 5*nrz + 5*nra + 4 + 1
        nn17 = sconst(nk1)
        nn5 = nk1 +1 +nn17
        n6 = nn5 + 1
        !      call convol(vm(i),vmdt(i),vmj5dt(iadrs),vmout,hmout
        !     1           ,vk(i),vkdt(i),vkj5dt(iadrs),vkout,hkout
        !     2           ,cjq(iadrs),djq(iadrs),ejq(iadrs),qk0(kq),esumq(kq)
        !     3           ,nteq,0)
        nq5 = nnq1 + 3 * nteq + 1
        nq6 = nq5 + 1
        hktdt = sconst(nq6) * sconst(n6)
        hmtdt = sconst(nq6) * sconst(nn5)
        if ( iprsup .gt. 0 ) write (*,4933)
4933    format (' kq nteq   lj  lj1  cjq(iadrs)     qk0(kq)   esumq(kq)       hktdt       hmtdt       vk(i)     vkdt(i)   vkj5dt(j)       vm(i)')
        lj1 = lj + nteq
        jkl = 0
        do jq=1,nteq
           nnq2 = nnq1 + jq
           nnq3 = nnq2 + nteq
           nq4 = nnq3 + nteq
           !     vkj5dt(iadrs) = qki(iadrs) * vkj5dt(iadrs)
           !    1              + ejq(iadrs) * sconst(n6)
           !    2              + qpi(iadrs) * emtpe(nn1)
           if ( abs(sconst(nnq2)) .ge. 1.e+14 ) go to 1800
           go to 1083
1800       jkl = jkl + 1
           if (jkl .eq. 2) go to 1801
           sconst(nnq2) = sconst(nnq2) / 1.e+15
1083       xm(lj1) = sconst(nnq2) * xm(lj1) + sconst(nq4)  * sconst(n6) + sconst(nnq3) * emtpe(nn1)
           xm(lj)  = sconst(nnq2) * xm(lj) + sconst(nq4)  * sconst(nn5) + sconst(nnq3) * emtpe(nn2)
           gkt = gkt + xm(lj1)
           gmt = gmt + xm(lj)
           hktdt = hktdt + sconst(nnq2) * xm(lj1)
           hmtdt = hmtdt + sconst(nnq2) * xm(lj)
           if ( jkl .eq. 1 ) go to 1802
1801       jkl = 0
1802       if ( iprsup .gt. 0 ) write (*,4333) kq,nteq,lj,lj1,sconst(nnq2),sconst(kqk0),sconst(nq6),hktdt,hmtdt,emtpe(nn1), sconst(n6),xm(lj1),emtpe(nn2)
4333       format (1x, i2, 3x, i2, 2x, i3, 2x, i3, 9e12.5)
           lj = lj + 1
           lj1 = lj1 + 1
           if ( lj .ge. lpast+lsiz28) write (*, 9835) lj, lpast+lsiz28
9835       format ('+xm()', i5, ' exceed the limit of list28', i5, /, ' if you are going to', /, &
                ' run the case agian, you should have your programer change', /, &
                ' the dimension. Message from ljg.')
1009    end do
9824    kqk0 = kqk0 + 2 * nteq + 1
        lj = lj + nteq
        wk1( koff8 + kh ) = wk1( koff8 + kh ) + hmtdt
        wk1( koff6 + kh ) = wk1( koff6 + kh ) + gmt
        wk1( koff7 + kh ) = wk1( koff7 + kh ) + hktdt
        wk1( koff5 + kh ) = wk1( koff5 + kh ) + gkt
        nnq1 = nnq1 + 3 * nteq + 2
        ky = ky + 1
        kq=kq+1
105  end do
     kh = kh + 1
104 end do
  !
  !        evaluation of hm1(t+dt) and hk1(t+dt)
  !
  ky = k
  n1 = kodsem(ky)
115 do i=1, it2
     !      call convol(vm5(i),vm5dt(i),gmjdt(iadrs),gm,hm1(i)
     !     1           ,vk5(i),vk5dt(i),gkjdt(iadrs),gk,hk1(i)
     !     2           ,cjy(iadrs),djy(iadrs),ejy(iadrs),yk0(i),esumy(i)
     !     3           ,ntey,0)
     if ( iprsup .gt. 0 ) write (*,*) 'just before convolution,vm5dt(ky),vk5dt(ky)dt=',wk1(koff1+ky),wk1(koff2+ky)
     n4 = indhst(ky)
     n1 = kodsem(ky)
     nrz = cki(ky)
     nra = ckkjm(ky)
     n8 = n1 + 5*nrz + 2*nra + 1
     n9 = n8 + 1
     gkt = sconst(n1) * wk1( koff5 + ky )
     gmt = sconst(n1) * wk1( koff6 + ky )
     wk1(koff13+ky) = sconst(n8) * wk1( koff2 + ky )
     wk1(koff14+ky) = sconst(n8) * wk1( koff1 + ky )
     if ( iprsup .gt. 0 ) write (*,4833)
4833 format (' ky  nrz   cjy(iadrs)  yk0(kq) esuy(kq) wk1(k13) wk1(k14)  vk5(ky) vk5dt(k) gkjdt(i)  vm5(ky) wk1(kf3) wk1(kf4)  scn(n7)  scn(n6) scn(nn5)      gmt      gkt')
     jkl = 0
     jql = 1
     nkyw = n4 + 20
     do j=1,nrz
        n5 = n1 + 2*nrz + 2*nra + j
        n6 = n5 + nrz
        n7 = n6 + nrz
        nk1 = n1 + 5*nra + 5*nrz + 5
        nn17 = sconst(nk1)
        nn5 = nk1 + j +4 +nn17
        nn6 = nn5 + nrz
        nky = n1 + j
        if ( sconst(nky) .gt. 1.e+13 ) go to 2000
        sconst(nn6) = sconst(n5) * sconst(nn6) + sconst(n7) * wk1( koff2 + ky ) + sconst(n6) * wk1( koff5 + ky )
        sconst(nn5) = sconst(n5) * sconst(nn5) + sconst(n7) * wk1( koff1 + ky ) + sconst(n6) * wk1( koff6 + ky )
        gkt=gkt+sconst(nn6)
        gmt=gmt+sconst(nn5)
        wk1(koff13+ky) = wk1(koff13+ky) + sconst(n5) * sconst(nn6)
        wk1(koff14+ky) = wk1(koff14+ky) + sconst(n5) * sconst(nn5)
        go to 2002
2000    jkl = jkl + 1
        if ( jkl .eq. 2) go to 2001
        nkyw = nkyw + jql
        nn10 = nkyw  + 1
        nn11 = nn10 + 1
        sconst(nn6) = cnvhst(nkyw) * sconst(nn6) + cnvhst(nn11) * wk1( koff2 + ky ) + cnvhst(nn10) * wk1( koff5 + ky )
        sconst(nn5) = cnvhst(nkyw) * sconst(nn5) + cnvhst(nn11) * wk1( koff1 + ky ) + cnvhst(nn10) * wk1( koff6 + ky )
        gkt=gkt+sconst(nn6)
        gmt=gmt+sconst(nn5)
        wk1(koff13+ky) = wk1(koff13+ky) + sconst(n5) * sconst(nn6)
        wk1(koff14+ky) = wk1(koff14+ky) + sconst(n5) * sconst(nn5)
        jql = jql + 6
2002    if ( iprsup .gt. 0 ) write (*,5333)     ky,nrz,sconst(n5), sconst(n1),sconst(n8), &
             wk1(koff13+ky),wk1(koff14+ky),wk1(koff5+ky),wk1(koff2+ky), &
             sconst(nn6), wk1(koff6+ky) ,wk1( koff3 + ky ),wk1( koff4 + ky ), &
             sconst(n7),  sconst(n6), sconst(nn5),      gmt  ,    gkt
5333    format (1x, i2, 3x, i2, 16e9.3)
        if ( jkl .eq. 1 ) go to 4009
2001    jkl = 0
4009 end do
     !        evaluate im'(t) = fm'(t) - gm(t)
     !                 ik'(t) = -bk'(t) + gk(t)
     !      aim5(ky)=fm5(ky)-gmt                          !fm5() = a1*fk' fro
     !      aik5(ky)=-bk5(ky)+gkt                         !bk5() = a1*bm' fro
     cnvhst(n4+13) = wk1( koff3 + ky ) - gmt
     cnvhst(n4+14) = - wk1( koff4 + ky ) + gkt
     if ( iprsup .gt. 0 ) &
                                !     1 write (*,3335) ky,hm1(ky),hk1(ky),aim5(ky),aik5(ky)
          write (*,3335) ky,wk1(koff14+ky),wk1(koff13+ky),cnvhst(n4+13),cnvhst(n4+14)
3335 format ('hm1(t),hk1(t),im5(ky),ik5(ky)=', x, i1, 4e14.5)
     ky = ky + 1
102 end do
  !
  !        evaluation of hm3(t+dt) and hk3(t+dt)
  !        (from the convolutions: im(t+dt) = q(t+dt) # im'(t+dt)
  !                                ik(t+dt) = q(t+dt) # ik'(t+dt) )
  !
  !                                       the calculation of im(t) = q(t)
  if ( lint .eq. 1) go to 116
  ka = k
  kqk0 = infdli(inoff2+k)
  nnq1 = infdli(inoff3+k)
  kq = infdli(inoff1+k)
  !      lj = kq                        ! 1st pole of qii for the branch k
  lj = nr(k)
  !      if ( kq .gt. 1) lj = ( kq-1 ) * 2 + 1
  !      kiline = lpast                                  !next cell is fre
  !      kiline = nr(k)
  !      lj = kiline + lj
  do j=1, it2
     n4 = indhst(ka)
     n1 = kodsem(ka)
     nrz = cki(ka)
     nra = ckkjm(ka)
     nk1 = n1 + 5*nra + 5*nrz + 4 + 1
     nn17 = sconst(nk1)
     nn5 = n1 + 5*nrz + 5*nra + 4 + 2 +1 +1 +nn17
     n6 = nn5 + 1
     ki = k
     do i=1, it2
        nteq = wk1(koff20+kq)
        !      call convol(aim5(j),aim5dt(j),aimjdt(iadrs),vmout,hmout
        !     1           ,aik5(j),aik5dt(j),aikjdt(iadrs),vkout,hkout
        !     2           ,cjq(iadrs),djq(iadrs),ejq(iadrs),qk0(k),esumq(k)
        !     3           ,nteq,0)
        gkt = sconst(kqk0) * cnvhst(n4+14)
        gmt = sconst(kqk0) * cnvhst(n4+13)
        nq5 = nnq1 + 3 * nteq + 1
        nq6 = nq5 + 1
        hkout = sconst(nq6) * sconst(n6)
        hmout = sconst(nq6) * sconst(nn5)
        if ( iprsup .gt. 0 ) write (*,4433)
4433    format (' kq nteq iadrs    cjq(iads)      qk0(kq)    esumq(kq)        hkout        hmout     aik5(ka)    aik5dt(j)   aikjdt(is)     aim5(ka)')
        lj1 = lj + nteq
        jkl = 0
        do jq=1,nteq
           nnq2 = nnq1 + jq
           nnq3 = nnq2 + nteq
           nq4 = nnq3 + nteq
           if ( abs(sconst(nnq2)) .ge. 1.e+14 ) go to 1804
           go to 1805
1804       jkl = jkl + 1
           if (jkl .eq. 2) go to 1806
           sconst(nnq2) = sconst(nnq2) / 1.e+15
1805       xk(lj1) = sconst(nnq2) * xk(lj1) + sconst(nq4)  * sconst(n6) + sconst(nnq3) * cnvhst(n4+14)
           xk(lj)  = sconst(nnq2) * xk(lj) + sconst(nq4)  * sconst(nn5) + sconst(nnq3) * cnvhst(n4+13)
           gkt = gkt + xk(lj1)
           gmt = gmt + xk(lj)
           hkout = hkout + sconst(nnq2) * xk(lj1)
           hmout = hmout + sconst(nnq2) * xk(lj)
           if (jkl .eq. 1 ) go to 1807
1806       jkl = 0
1807       if ( iprsup .gt. 0 ) write (*,4773) kq,nteq,iadrs,sconst(nnq2), sconst(kqk0),sconst(nq6),hkout,hmout, &
                cnvhst(n4+14),sconst(n6),xk(lj1),cnvhst(n4+13)
4773       format (1x, i2, 3x, i2, 3x, i3, 9e13.5)
           !      iadrs = iadrs + 1
           lj = lj + 1
           lj1 = lj1 + 1
6009    end do
        kqk0 = kqk0 + 2 * nteq + 1
        lj = lj + nteq
        if ( iprsup .gt. 0 ) write ( *,*)'hist.sour. with current  hm3(ki), hk3(ki)', wk1(koff10+ki), wk1(koff9+ki)
        wk1(koff10+ki) = wk1(koff10+ki) + hmout
        wk1(koff9+ki)  = wk1(koff9+ki)  + hkout
        if ( iprsup .gt. 0 ) write ( *,*)'hist.sour. with current  hm3(ki), hk3(ki)', wk1(koff10+ki), wk1(koff9+ki)
        ki = ki + 1
        kq=kq+1
        nnq1 = nnq1 + 3 * nteq + 2
108  end do
     ka = ka + 1
107 end do
  !                         evaluate fk'(t) and bm'(t):
  !                                                      fk'(t) = 2ik'(t)
  !                                                     bm'(t) = -2im'(t)
116 kcc = koff25 + 288
  koff11 = koff25 + 288
  do i = 1, ibr
     if (  kodsem(i) .eq. 0.0 .or. imodel(i) .ne. -4 ) go to 1097
     kktau = indhst(i)
     nterm = cnvhst(kktau) / deltat + 1
     kcc = kcc + 2 * nterm
1097 end do
  wk1(kcc+1) = 68556311.00
  ka = k
  if ( lmode .eq. 4 ) koff11  = lcbl
  do i=1, it2
     n4 = indhst(ka)
     fk5 =  2.d0 * cnvhst(n4+14) + wk1( koff4 + ka )
     bm5 = -2.d0 * cnvhst(n4+13) + wk1( koff3 + ka )
     !                                       update past history terms of fk'
     kktau = indhst(ka)
     nterm = cnvhst(kktau) / deltat + 1
     if ( iprsup .gt. 0 ) write (*,4143)
4143 format ('nterm ktau    fkh5(ktau)           fk5    bmh5(ktau)           bm5        fk5tdt        fk5tau        bm5tdt        bm5tau')
     !      call updcbl(fkh5(ktau),fk5,fk5tdt,fk5tau
     !     1           ,bmh5(ktau),bm5,bm5tdt,bm5tau
     !     1           ,nterm,nkill(ka),tau(ka),deltat)
     nkll = infdli(inoff5+ka)
     koff11 = koff11 + nkll
     koff12 = koff11 + nterm
     fk5tau = wk1(koff11)
     bm5tau = wk1(koff12)
     wk1(koff11) = fk5
     wk1(koff12) = bm5
     nk1 = infdli(inoff5+ka) - 1
     if ( nk1 .eq. 0 ) koff11 = koff11 + nterm
     if ( nk1 .eq. 0 ) koff12 = koff12 + nterm
     nk2 = nk1 - 1
     if ( nk2 .eq. 0 ) nk3 = koff11 - 2 + nterm
     if ( nk2 .eq. 0 ) nk4 = koff12 - 2 + nterm
     c2 = cnvhst(kktau) - nterm * deltat
     c1 = - c2 - deltat
     if ( nk2 .ne. 0 )    go to 7601
     yk = wk1(koff11-1) * c1 + wk1(nk3) * c2
     ym = wk1(koff12-1) * c1 + wk1(nk4) * c2
     go to 7602
7601 yk = wk1(koff11-1) * c1 + wk1(koff11-2) * c2
     ym = wk1(koff12-1) * c1 + wk1(koff12-2) * c2
7602 wk1(koff11-1) = - yk / deltat
     wk1(koff12-1) = - ym / deltat
     fk5tdt = wk1(koff11-1)
     bm5tdt = wk1(koff12-1)
     kaa = kaa + 2 * nterm
     !                                                      !get ready for ne
     kbb = koff25 + 288 + kaa  + 1
     if (wk1(kbb) .eq. 68556311.00 ) kaa = 0
     !                                                  !get ready for next t
     koff11 = koff25 + 288 + kaa
     infdli(inoff5+ka) = nk1
     if ( infdli(inoff5+ka) .eq. 0 ) infdli(inoff5+ka) = nterm
     if ( iprsup .gt. 0 ) write (*,5337) nterm, ktau,wk1(koff11),fk5,wk1(koff12), bm5,fk5tdt,fk5tau,bm5tdt,bm5tau
5337 format (3x, i2, 3x, i2, 8e14.5)
     !                                 evaluation of fm'(t+dt) = a1(t+dt) # f
     !                                               bk'(t+dt) = a1(t+dt) # b
     !      call convol(fk5tdt,fk5tau,fmj5(iadrs),fm5(i),hkout
     !     1           ,bm5tdt,bm5tau,bkj5(iadrs),bk5(i),hmout
     !     2           ,cja(iadrs),dja(iadrs),eja(iadrs),0.d0,0.d0,ntea,1)
     n1 = kodsem(ka)
     nrz = cki(ka)
     nra = ckkjm(ka)
     wk1( koff3 + ka ) = 0.d0
     wk1( koff4 + ka ) = 0.d0
     if ( iprsup .gt. 0 ) write (*,4435)
4435 format (' ka  nra cja(iadrs) dja(iadrs) eja(iadrs)     fk5tdt     fk5tau fmj5(iars)     bm5tdt     bm5tau bkj5(iars)        fm5        bk5')
     jgl = 0
     do j=1,nra
        n5 = n1 + 5*nrz + 2 *nra + 2 + j
        n6 = n5 + nra
        n7 = n6 + nra
        nkkk1 = n1 + 5*nra + 5*nrz + 4 + 1
        nn17 = sconst(nkkk1)
        nn5 = n1 + 7*nrz + 5*nra + 9 + j + nn17
        nn6 = nn5 + nra
        nk5 = n1 + 2*nrz + j
        if ( abs( sconst(nk5) ) .ge. 1.e13 ) jgl = jgl + 1
        if ( jgl .eq. 2) go to 5486
        sconst(nn5) = sconst(n5) * sconst(nn5) + sconst(n7) * fk5tau + sconst(n6) * fk5tdt
        sconst(nn6) = sconst(n5) * sconst(nn6) + sconst(n7) * bm5tau + sconst(n6) * bm5tdt
        wk1( koff3 + ka ) = wk1( koff3 + ka ) + sconst(nn5)
        wk1( koff4 + ka ) = wk1( koff4 + ka ) + sconst(nn6)
        if ( jgl .eq. 1 )  go to 4337
5486    jgl = 0
4337    if ( iprsup .gt. 0 ) write (*,5334)   ka,nra,sconst(n5),sconst(n6), sconst(n7),fk5tdt,fk5tau,sconst(nn5),bm5tdt, &
             bm5tau,sconst(nn6),wk1(koff3+ka),wk1(koff4+ka)
5334    format (1x, i2, 3x, i2, 11e11.5)
4204 end do
     ka = ka + 1
109 end do
  lcbl = koff11
  if (koff11 .gt. koff25+288) lmode = 4
  ka = k
  nq0k=infdli(inoff1+k)
  do i=1, it2
     if ( iprsup .gt. 0 ) write (*,*)'y05(ka),wk1(koff8+ka),wk1(koff7+ka),hm1(ka),hk1(ka)', wk1(koff21+ka),wk1(koff8+ka),wk1(koff7+ka), &
          wk1(koff14+ka),wk1(koff13+ka)
     !     vmdt(ka)=fm5(ka)-y05(ka)*hm2(ka)-hm1(ka)
     !     vkdt(ka)=bk5(ka)-y05(ka)*hk2(ka)-hk1(ka)
     !     wk1(koff18+ka) = wk1( koff3 + ka )               !vmdt(ka)=
     !    1         - y05(ka) * wk1( koff8 + ka )
     !    2         - wk1(koff14+ka)
     wk1(koff18+ka) = wk1( koff3 + ka ) - wk1(koff21+ka) * wk1( koff8 + ka ) - wk1(koff14+ka)
     wk1(koff17+ka) = wk1( koff4 + ka ) - wk1(koff21+ka) * wk1( koff7 + ka ) - wk1(koff13+ka)
     if ( iprsup .gt. 0 ) write (*,*)'ka,fm5(ka),hm1(ka),bk5(ka),hk1(ka),vmdt(ka),vkdt(ka)', &
          ka,wk1( koff3 + ka ),wk1(koff14+ka),wk1( koff4 + ka ), &
          wk1(koff13+ka),wk1(koff18+ka),wk1(koff17+ka)
     ka = ka + 1
110 end do
  !      call mvec(wk1(koff22+nq0k),wk1(koff18+k),
  !     1    wk1(koff16+k),wk1(koff17+k),wk1(koff15+k),it2,0)
  do im = 1, it2
     wk1(koff16+k-1+im) = 0.d0
     wk1(koff15+k-1+im) = 0.d0
1007 end do
  do jm = 1, it2
     do im = 1, it2
        wk1(koff16+k-1+im) = wk1(koff16+k-1+im) + wk1(koff22+nq0k) * wk1(koff18+k-1+jm)
        wk1(koff15+k-1+im) = wk1(koff15+k-1+im) + wk1(koff22+nq0k) * wk1(koff17+k-1+jm)
        nq0k = nq0k + 1
1027 end do
1017 end do
  if ( iprsup .gt. 0 ) write ( *,*)'##equi. voltage sour. hm(1), hk(1)', wk1(koff16+1), wk1(koff15+1)
  if ( lint .eq. 1 ) go to 119
  ka = k
  do i=1, it2
     wk1(koff16+ka) = wk1(koff16+ka) + wk1( koff10 + ka )
     wk1(koff15+ka) = wk1(koff15+ka) -  wk1( koff9 + ka )
     ka = ka + 1
111 end do
  !
  !        complete updating of remaining variables:
  !                vm5dt,vk5dt
  !                aim5dt,aik5dt
  !                vmdt,vkdt
  !
119 kst = k
  do i=1, it2
     wk1( koff1 + kst ) = wk1( koff6 + kst )
     wk1( koff2 + kst ) = wk1( koff5 + kst )
     n1 = kodsem(kst)
     nrz = cki(kst)
     nra = ckkjm(kst)
     nk1 = n1 + 5*nra + 5*nrz + 5
     nn17 = sconst(nk1)
     nn5 =  nk1 + 3 +nn17
     n6 = nn5 + 1
     n4 = indhst(kst)
     sconst(nn5) = cnvhst(n4+13)
     sconst(n6)  = cnvhst(n4+14)
     if ( iprsup .gt. 0 ) write ( *,*)'##equi. current sour. aim5dt(i), aik5dt(i)', sconst(nn5), sconst(n6)
     !      aim5dt(i)=aim5(i)
     !      aik5dt(i)=aik5(i)
     nn5 = nn5 - 2
     n6 = nn5 + 1
     !      vmdt(i)=vm(i)
     !      vkdt(i)=vk(i)
     !      sconst(nn5) = vm(kst)                                    !update
     !      sconst(n6)  = vk(kst)                                    !update
     nn1 = -kbus(kst)
     nn2 = iabs( mbus(kst) )
     sconst(nn5) = emtpe(nn2)
     sconst(n6)  = emtpe(nn1)
     if ( iprsup .gt. 0 ) write ( *,*)'##equi. voltage sour. vmdt(i), vkdt(i)', &
          sconst(nn5), sconst(n6)
     !      hmp(kst) = wk1(koff16+kst)
     !      hkp(kst) = wk1(koff15+kst)
     !      if ( iprsup .gt. 0 )
     !     1 write ( *,*)'##equi. voltage sour. hmp(i), hkp(i)',
     !     2                                   hmp(kst), hkp(kst)
     if ( iprsup .gt. 0 ) write ( *,*)'##equi. voltage sour. vm5dt(i), vk5dt(i)', &
          wk1( koff1 + kst), wk1( koff2 + kst )
     kst = kst + 1
112 end do
  !
  do i=1, it2
     n1 = -kbus(k)
     n2 = mbus(k)
     n2p = iabs ( n2 )
     emtpf(n1) = emtpf(n1) + wk1(koff15+k)
     emtpf(n2p) = emtpf(n2p) + wk1(koff16+k)
     k = k + 1
     if ( n2 .gt. 0 ) go to 483
     kcount = kcount + 1
     !      bvalue(kcount) = aik(i)
     kcount = kcount + 1
     !     bvalue(kcount) = -aim(i)
483 end do
  if ( iprsup .gt. 0 ) write (*,*) ' subts2 cable done.  k, it2 =',  k, it2
  it2 = 0
  go to 1200
  !                    end time step loop for frequency-dependent branches
  !  4340 if ( kodsem(k) .ne. 0  .and.
  !     1     imodel(k) .ne. -2 )  go to 11780    !if this is semlyen case
4340 if ( kodsem(k) .ne. 0  .and. imodel(k) .ge. 0 )  go to 11780
  if ( it2 .gt. 0 )   it2 = 1
  !              transformation to mode voltages, multiplied by number of
1185 it2 = iabsz ( it2 )
  phsinv = unity / it2
  it1=k+1
  ii=length(it1)
  if ( length(k) .gt. 0 )  ii = length(k)
  gus1=it2
  n3 = litype(k)
  !                                            ** m*vmode=m*qfd(transposed
  it1=k
  if ( iprsup .ge. 3 ) write (lunit6, 21194) gus1, n3, it1, it2
21194 format (' gus1, n3, it1 and it2 at 21194 are', e12.5, 3i10)
  do i=1,it2
     n1=-kbus(it1)
     n2=mbus(it1)
     volt(i)=emtpe(n1)
     ndx1 = lsiz26 + i
     volt(ndx1)=emtpe(n2)
     if (iprsup .ge. 1) write (lunit6,31194) n1, n2, emtpe(n1), emtpe(n2)
31194 format (' emtpe(', i3, '), and emtpe(', i3, ') at 31194 are', 2e15.7)
1195 it1=it1+1
  end do
  do i=1,it2
     ci1= 0.0
     ck1 = 0.0
     ndx1 = lsiz26
     do j=1,it2
        yx=qfd(n3)
        ci1  =  ci1  +  yx * volt(j)
        ndx1 = ndx1 + 1
        ck1  =  ck1  +  yx * volt(ndx1)
1196    n3=n3+1
     end do
     volti(i)=ci1*gus1
1197 voltk(i)=ck1*gus1
  end do
  if (iprsup .ge. 1 ) write (lunit6, 21197) (volt(i),volti(i),voltk(i),i=1,it2)
21197 format (' (volt(i),         volti(i), voltk(i), i=1,it2) are', /,  (1x, 9e14.6))
  ndx1 = lsiz26 + 1
  ndx2 = lsiz26 + it2
  if ( iprsup  .ge.  1 ) write (lunit6, 31197)  ( volt(i), i=ndx1, ndx2 )
31197 format (' volt:', 8e16.8)
  gus1=unity
  !              update mode quantities and find phase vectors i for right
  ci1=0
  if ( modout  .le.  0 )   go to 4641
  do i=1, modout
     n7 = nmodal + i
     bvalue(n7) = volti(i) * phsinv
     n8 = n7 + modout
4633 bvalue(n8) = voltk(i) * phsinv
  end do
  if ( iprsup  .ge.  1 ) write (lunit6, 4638)  nv, it2, k,  (volti(i), voltk(i), i=1, it2 )
4638 format (/, " Mode voltages in  'subts2' .  ", 3i10, /, (1x, 8e16.5))
  !              update mode quantities and find phase vectors i for right
4641 ck1=0.
  msign=k
  i = 0
41189 i = i + 1
  if (imodel(msign) .eq. -2)  go to 10125
  n3=nr(msign)
  if (n3.lt.0) go to 1187
  it1=ii+ipoint
  n4=it1+n3
  if (it1.gt.lpast)it1=it1-lpast
  if (n4.gt.lpast)n4=n4-lpast
  it21=it1+1
  n41=n4+1
  if (it21.gt.lpast)it21=1
  if (n41.gt.lpast)n41=1
  xmn4=xm(n4)
  a=cik(msign)
  d2=1.0-a
  gus2=ci(msign)
  h1=absz(gus2)
  h2 = absz(ck(msign))
  !
  !     current output for single distribute
  !
  if ( n2 .gt. 0 ) go to  3811
  n2 = - n2
  real_bus4 = 1. + h2
  if (real_bus2 .lt. 0.) real_bus4 = 2.0 * h2
  real_bus4 = h1 * emtpe(n1) / real_bus4 - xk(n4)
  kcount = kcount + 1
  bvalue(kcount) = real_bus4
  !
3811 if ( iprsup .gt. 8 ) write (lunit6,31192) i, msign, it1, it21, cik(msign), ci(msign), &
       ck(msign), xm(it1), xk(it1), xm(it21), xk(it21)
31192 format ('0in "subts2".  i, msign, it1, it21, cik(msign), ci(msign), ck(msign), xm(it1), xk(it1), xm(it21), xk(it21) follow .....', /, &
           1x, 4i5, 7e15.6)
  xm(n4)=h1*volti(i)-h2*xk(n4)
  xk(n4)=h1*voltk(i)-h2*xmn4
  if (ck(msign).lt.0.0) go to 20510
  h1=xk(it1)*a+xk(it21 )*d2
  d2=xm(it1)*a+xm(it21 )*d2
  if ( gus2 .lt. 0.0 )  go to 1192
  a = h1 + d2
  d2 = ( h1 - d2 ) * h2
  h1 = ( a + d2 ) / 2.0
  d2 = ( a - d2 ) / 2.0
1192 xk(n41 )=h1
  xm(n41)=d2
  if ( iprsup .gt. 6 ) write (lunit6,1193) i, msign, it1, n41, (xk(j),xm(j),j=it1,n41)
1193 format ('0in "subts2".  i, msign, it1, n41, and, 41h (xk(j), xm(j), j=it1, n41)  follow  ....', 5x, 4i5,/, (1x, 5e25.16))
1188 ii = ii + n3 + 2
  !j
  go to 10130
  !
  !                  calculate equivalent current sources
  !                  and update history vectors for f.d.
  !                  multi-phase lines
  !
10125 vkd=volti(i) * phsinv
  vmd=voltk(i) * phsinv
  nrz = cki(msign)
  nra = ckkjm(msign)
  n11 = indhst(msign)
  n1 = cnvhst(n11) * dltinv + 2.0
  n2 = kodsem(msign)
  nraz1 = nra + nrz
  nraz2 = nraz1 + nraz1
  nraz3 = nraz2 + nraz1
  nra3  = nra * 3
  nrz2  = nrz + nrz
  nrz3  = nrz2 + nrz
  n31 = n2 + nraz2
  n32 = nraz3 + 4
  n33 = n31 + n32
  n34 = n33 + nrz2
  n3 = n31 + nrz3 + 1
  n7 = n3 + nra3
  n10 = n3 + 1
  !                                           n10 is where first cell of c
  ind = nr(msign) + ipoint - 1
  if (ind .gt. lpast)  ind = ind - lpast
  d23 = 1.0 / sconst(n10)
  if ( t .eq. 0.0 )   go to  81000
  !                                                                     !t
  !     update history vectors
  aikd = vkd * d23 - cnvhst(n11+13)
  aimd = vmd * d23 - cnvhst(n11+14)
  do ii = 1, nrz
     nn1 = n31 + ii
     nn2 = nn1 + nrz
     nn3 = nn2 + nrz
     nn4 = nn1 + n32
     nn5 = nn4 + nrz
     !                                    eki= mi*eki(t-dt) + pi*ik(t) + qi*i
     !                                                      sconst(nn4) was e
     !                                                     cnvhst(n11+5) was
     !                                                                 aikd w
     !
     !     ek(t) = sum eki(t)
     !           = sum ! mi*eki(t-dt) + pi*ik(t) + qi*ik(t-dt) }
     !
     sconst(nn4) = sconst(nn2) * aikd + sconst(nn3) * cnvhst(n11+5) + sconst(nn1) * sconst(nn4)
     sconst(nn5) = sconst(nn2) * aimd + sconst(nn3) * cnvhst(n11+6) + sconst(nn1) * sconst(nn5)
80110 end do
  do ii = 1, nra
     nn6 = n10 + ii
     nn7 = nn6 + nra
     nn8 = nn7 + nra
     nn9 = n34 + ii
     nn10 = nn9+nra
     !
     !
     !          bk(t) = sum
     !
     !
     sconst(nn9) = sconst(nn6) * sconst(nn9) + sconst(nn7) * cnvhst(n11+8) + sconst(nn8) * cnvhst(n11+7)
     sconst(nn10) = sconst(nn6) * sconst(nn10) + sconst(nn7) * cnvhst(n11+10) + sconst(nn8) * cnvhst(n11+9)
80120 end do
  cnvhst(n11+5) = aikd
  cnvhst(n11+6) = aimd
  nrf = ind + n1 - 1
  if ( nrf .gt. lpast)  nrf = nrf - lpast
  xk(nrf) = 2.0 * vkd - cnvhst(n11+11)
  xm(nrf) = 2.0 * vmd - cnvhst(n11+12)
  if ( iprsup .eq. 0)  go to 81000
  write (lunit6,80135) t, i
80135 format (' at t = ', e12.5, ' for Marti branch', i4, /, ' history vectors')
  j1 = nn4 - nrz + 1
  j2 = nn5 - nrz + 1
  j3 = nn9 - nra + 1
  j4 = nn10 - nra + 1
  write (lunit6, 80140) j1, nn4
80140 format (' sconst from', i6, '    to', i6 )
  write (lunit6, 80145) (sconst(ii),ii=j1,nn4)
80145 format (1x, 10e12.5)
  write (lunit6, 80140)  j2, nn5
  write (lunit6, 80145) (sconst(ii),ii=j2,nn5)
  write (lunit6,80140)  j3, nn9
  write (lunit6, 80145) (sconst(ii),ii=j3,nn9)
  write (lunit6, 80140) j4, nn10
  write (lunit6, 80145) (sconst(ii),ii=j4,nn10)
  write (lunit6, 80150) ind, nrf
80150 format (/, ' Forward functions xk and xm; positions  ', i6, '  to', i6)
  write (lunit6,80145) (xk(ii),ii=ind,nrf),(xm(ii),ii=ind,nrf)
  !
  !
  !     equivalent current sources
81000 ekh = sconst(n3) * cnvhst(n11+5)
  emh = sconst(n3) * cnvhst(n11+6)
  do ii = 1, nrz
     n4 = n31 + ii
     n5 = n33 + ii
     n6 = n5 + nrz
     !                                              !ekhv(t)=sum(mi)*eki(t-dt
     !                                              !emhv(t)=sum(mi)*emi(t-dt
     ekh = ekh + sconst(n4) * sconst(n5)
     emh = emh + sconst(n4) * sconst(n6)
81040 end do
  !
  !     interpolation process for last point of forward functions
  !
  temp1 = cnvhst(n11) * dltinv
  itauo = temp1
  frac = 1.0 - ( temp1 - itauo)
  i1 = ind
  i1p1 = i1 + 1
  if ( i1p1 .gt. lpast )   i1p1 = i1p1 - lpast
  i1p2 = i1 + 2
  if ( i1p2 .gt. lpast )   i1p2 = i1p2 - lpast
  !
  !                                        xk=fk>>>>(vk+zc*ik)  coming fro
  !                                        xm=fm>>>>(vm+zc*im)  coming fro
  !
  cnvhst(n11+7) = (xm(i1p1)-xm(i1)) * frac + xm(i1)
  cnvhst(n11+8) = (xm(i1p2)-xm(i1p1)) * frac + xm(i1p1)
  cnvhst(n11+9) = (xk(i1p1)-xk(i1)) * frac + xk(i1)
  cnvhst(n11+10) =(xk(i1p2)-xk(i1p1)) * frac + xk(i1p1)
  !
  !               bk(t) = sumgi*bki(t-dt) + c*fm(t-tau) + d*fm(t-tau-dt)
  !                         *************>>will be done in the loop 81050
  !
  cnvhst(n11+11) = sconst(n7+2) * cnvhst(n11+8) + sconst(n7+3) * cnvhst(n11+7)
  cnvhst(n11+12) = sconst(n7+2) * cnvhst(n11+10) + sconst(n7+3) * cnvhst(n11+9)
  do ii = 1, nra
     n8 = n10 + ii
     n9 = n34 + ii
     !
     !                bk(t) =sub gi*bki(t-dt) + c*fm(t-tau) + d*fm(t-tau-dt)
     !                           *being done* + ^^^^^already done above^^^^^
     !
     cnvhst(n11+11) = cnvhst(n11+11) + sconst(n8) * sconst(n9)
     cnvhst(n11+12) = cnvhst(n11+12) + sconst(n8) * sconst(n9+nra)
81050 end do
  cnvhst(n11+13) = (ekh + cnvhst(n11+11)) * d23
  cnvhst(n11+14) = (emh + cnvhst(n11+12)) * d23
  h1 = cnvhst(n11+13)
  d2 = cnvhst(n11+14)
  if (iprsup .eq. 0)  go to 10130
  write (lunit6,81060) t, i
81060 format (/, ' at t = ', e12.5, ' for Marti branch', i4, '   equiv. circuit, voltages and current sources:')
  write (lunit6, 81070) ekh, emh, cnvhst(n11+11), cnvhst(n11+12),cnvhst(n11+13),cnvhst(n11+14)
81070 format (1x, 6e16.7)
10130 volti(i)= h1*gus1
  voltk(i)=d2*gus1
  msign=msign+1
  go to 1189
1187 h1=0.
  d2=0.
  go to 1188
20510 kf = msign
  jf = it1
20520 kf = cik(kf)
  ifdep2 = 2*kf - 1
  jfdep2 = 2 * lfdep + ifdep2
  kfdep2 = 4 * lfdep + ifdep2
  sk = 0.0
  sm = 0.0
  ik = n4
  im = ik + lpast
  iklim = 0
5140 l = iwtent(ifdep2)
  lbound = iwtent(ifdep2+1)
5180 ik1 = ik - 1
  im1 = im - 1
  if ( ik1 .gt. iklim )  go to 5186
  ik1 = ik1 + lpast
  im1 = im1 + lpast
5186 if ( im  .le.  ik )   go to 35186
  xxik = xk(ik)
  xxim = xm(ik)
  go to 45186
35186 xxik = xm(im)
  xxim = xk(im)
45186 sk = sk  +  weight(l) * xxim
  sm = sm  +  weight(l) * xxik
  !     preceding code replaces following two original records.
  !     it is a patch, to obey  common  boundaries for burroughs.
  !5186 sk = sk + weight(l) * xx(im)
  !     sm = sm + weight(l) * xx(ik)
  if ( iprsup .ge. 8 ) write (lunit6, 7129)  l, ik, im, ik1, im1, iklim, lbound, sk, sm, weight(l), xxik, xxim
7129 format (/, ' Partial sum.       l      ik      im     ik1     im1   iklim  lbound', /,  &
          13x, 7i8,/, 13x, 18x, 'sk', 18x, 'sm', 11x, 'weight(l)', 16x, 'xxik', 16x, 'xxim', /, &
          13x, 5e20.11)
  l = l + 1
  if ( l .ge. lbound )  go to 5250
  ik = ik1
  im = im1
  go to 5180
5250 if ( im1  .le.  ik1 )   go to 35250
  xxik1 = xk(ik1)
  xxim1 = xm(ik1)
  go to 45250
35250 xxik1 = xm(im1)
  xxim1 = xk(im1)
45250 iftail = iftail + 1
  !     preceding cards are replacement for two cards below.
  !     temporary patch to observe  common  boundaries.
  !5250 iftail = iftail + 1
  !     dk = con1(ifdep2) * xx(im)  +  con1(jfdep2) * xx(im1)  +
  dk = con1(ifdep2) * xxim    +  con1(jfdep2) * xxim1 + con1(kfdep2) * stailk(iftail)
  if ( iprsup .ge. 8 ) write (lunit6, 7131)  stailk(iftail), stailm(iftail), con1(ifdep2), &
       con1(jfdep2), con1(kfdep2), xxik, xxik1, xxim, xxim1
7131 format (/, ' stailk update.', 6x, 'stailk(iftail)', 6x, 'stailm(iftail)', 8x, 'con1(ifdep2)', &
          8x, 'con1(jfdep2)', 8x, 'con1(kfdep2)', /, 15x, 5e20.11, /, 15x, 16x, 'xxik', 15x, 'xxik1', &
          16x, 'xxim', 15x, 'xxim1', /, 15x, 5e20.11)
  stailk(iftail) = dk
  sk = sk + dk
  !     dm = con1(ifdep2) * xx(ik)  +  con2(ifdep2) * xx(ik1)  +
  dm = con1(ifdep2) * xxik    +  con1(jfdep2) * xxik1    + con1(kfdep2) * stailm(iftail)
  stailm(iftail) = dm
  sm = sm + dm
  ndelt = t / deltat
  if ( ndelt .lt. 10     .and.    iprsup .ge. 2 ) write (lunit6, 5257)  k, ifdep2, iftail,  l, ik, im, dk, dm, sk, sm
5257 format (/, ' Convolution done.', 48x, 'dk', 14x, 'dm', 14x, 'sk', 14x, 'sm' ,/, 18x, 6i8, 4e16.7)
  ifdep2 = ifdep2 + 1
  jfdep2 = jfdep2 + 1
  kfdep2 = kfdep2 + 1
  if ( ik .gt. im )  go to 5300
  im = n4  -  ( iskip(kf)-1 )
  if ( im  .le.  0 )   im = im + lpast
  ik = im + lpast
  iklim = lpast
  go to 5140
5300 if ( ndelt .lt. 10    .and.    iprsup .ge. 2 ) write (lunit6, 5310)  l,    kf, ifdep2, k, iftail, sk, sm
5310 format (/, ' Done f.d. mode.       l      kf  ifdep2       k  iftail',  18x, 'sk', 18x, 'sm', /, 16x, 5i8, 2e20.11)
  go to 1192
1189 if ( i  .lt.  it2 )   go to 41189
  j = litype(k)
  !*** iphase=ti*imode
  n3=j-1
  do j=1,it2
     h1=0.
     h2=0.
     n3=n3+1
     n4=n3
     do i=1,it2
        yx=qfd(n4)
        h1=h1+yx*volti(i)
        h2=h2+yx*voltk(i)
1199    n4=n4+it2
     end do
     n1=-kbus(k)
     n2=mbus(k)
     emtpf(n1)=emtpf(n1)+h1
     emtpf(n2)=emtpf(n2)+h2
     if (iprsup .ge. 3 ) write (lunit6, 21199) j, it2,n1, n2, h1, h2, emtpf(n1),emtpf(n2)
21199 format ('       j     it2      n1      n2              h1       h2           emtpf(n1)           emtpf(n2)', /, 4i8, 4(1x,e15.6))
1203 k=k+1
  end do
  ! *** end of conversion to iphase for untransposed option
1204 it2=0
  go to 1200
  !
  !  recursive convolution models
  !
11780 it2 = iabs(kodebr(k))
  phsinv = unity / it2
  n2 = cik(k)
  !  define the terminal voltage in semaux(0)(semaux(it2)) for kbus(mbus)
  do i=1, it2
     if (n2 .le. 0) go to 11784
     d1 = 0.0
     d2 = 0.0
     do j=1, it2
        ii = j + k - 1
        n3 = - kbus(ii)
        d1 = d1 + qfd(n2) * emtpe(n3)
        n3 = iabs(mbus(ii))
        d2 = d2 + qfd(n2) * emtpe(n3)
        n2 = n2 + 1
11782 end do
     go to 11786
11784 ii = i + k - 1
     n3 = - kbus(ii)
     d1 = emtpe(n3)
     n3 = iabsz(mbus(ii))
     d2 = emtpe(n3)
11786 n3 = it2 + i
     semaux(i) = d1
     semaux(n3) = d2
     volt(i) = 0.0
     volti(i) = 0.0
     voltk(i) = 0.0
     ndx1 = lsiz26 + i
     volt(ndx1) = 0.0
11788 end do
  if (iprsup .lt. 9) go to 23200
  n3 = it2 + it2
  write (lunit6, 23210) k, it2, cik(k), (semaux(i), i=1, n3)
23210 format (/, "  After calculation of modal/phase voltage vector in 'subts2'.   ", i10, i10, f10.0, /, (8(1x,e15.7)))
23200 ii = k
  if (kodsem(k) .gt. 0) go to 11824
  !  calculation of branch(volt) and norton(volti) i's for lumped elements
11790 n2 = absz(cki(ii)) - 1.0
  n3 = n2 / it2
  n2 = n2 - n3 * it2 + 1
  n4 = ck(ii)
  n5 = indhst(ii)
  n6 = - kodsem(ii)
  n3 = n3 + 1
  n7 = n3 + it2
  d1 = semaux(n3) - semaux(n7)
  d4 = sconst(n6 + 1) * d1
  d2 = d4 - cnvhst(n5)
  d3 = - d2 - d4
  cnvhst(n5) = - d3
  d2 = sconst(n6) * d1 + d2
  if (n4) 11804, 11814, 11792
  !  exponential representation of the transfer function.  complete convol
  !  ution to get current at t=t0, then preform partial conv. to get i-nor
11792 if (iprsup .ge. 9) write (lunit6, 23040) n2, n3, n4, n5, n6, d1, d2, d3
23040 format (//, '  at 11792.    ', 5i10, 3(1x,e15.7))
  do j=1, n4
     d4 = cnvhst(n5 + 1) + d1 * sconst(n6 + 4)
     if (iprsup .ge. 9) write (lunit6, 23030)  cnvhst(n5 + 1), d4
23030 format ('  Top of lumped element exponential convolution loop. ', 2(2x,e15.8))
     if (sconst(n6 + 2)) 11794, 11796, 11798
11794 cnvhst(n5 + 1) = - sconst(n6 + 3) * cnvhst(n5) + sconst(n6 - 1) * d4 + sconst(n6 + 5) * d1
     d4 = sconst(n6 - 1) * cnvhst(n5) + sconst(n6 + 3) * d4 + sconst(n6 + 1) * d1
     d3 = d3 + d4 + d4
     cnvhst(n5) = d4
     go to 11800
11796 d2 = d2 + d4
     d4 = d4 * sconst(n6 + 3) + d1 * sconst(n6 + 5)
     d3 = d3 + d4
     cnvhst(n5 + 1) = d4
     go to 11800
11798 d2 = d2 + d4 + d4
     cnvhst(n5 + 1) = d4
11800 n6 = n6 + 5
     n5 = n5 + 1
     if (iprsup .ge. 9) write (lunit6, 23020) j, n5, n6, cnvhst(n5), d2, d3, d4
23020 format ('  In exp. conv. loop.   ', 3i10, 4(1x,e15.7))
11802 end do
  go to 11814
  !  piecewise-linear representation of the step function.  complete conv
  !  at t=t0 to get norton i.  update conv. history go get branch i.
11804 d4 = cnvhst(n5 + 1) + d1 * sconst(n6 + 3)
  d2 = d2 + d4
  n7 = nr(ii)
  n8 = ipoint + iabs(n7) + length(ii) - 3
  if (n8 .gt. lpast) n8 = n8 - lpast
  n4 = n6 - n4 - n4 - 4
  d5 = 0.0
  if (n7 .lt. 0) go to 11808
  !  past history of convolution input is stored in 'xk'.
  xk(n8) = d1
  n8 = n8 + 1
  do j=n6, n4, 2
     d6 = sconst(j + 4) * dltinv
     n7 = d6
     d6 = d6 - n7
     n7 = n8 - n7
     if (n7 .le. 0) n7 = n7 + lpast
     n9 = n7 - 1
     if (n9 .le. 0) n9 = n9 + lpast
     n10 = n9 - 1
     if (n10 .le. 0) n10 = n10 + lpast
     d6 = xk(n7) + xk(n9) - d6 * (xk(n7) - xk(n10))
     d4 = d4 + sconst(j + 3) * (d5 - d6)
     d5 = d6
11806 end do
  if (iprsup .lt. 9) go to 11812
  n7 = iabs(nr(ii)) + ipoint - 1
  if (n7 .gt. lpast) n7 = n7 - lpast
  write (lunit6, 23010) n2, n3, n4, n5, n6, n7, n8, n9, n10, d1, d2, d3, d4, d5, d6, cnvhst(n5), cnvhst(n5+1), &
       (xk(j), j=n7, n8)
23010 format (/, '  After linear conv.  n2, n3, n4, n5, n6, n7, n8, n9, n10 =  ', 9i7, /, &
           5x, '  d1, d2, d3, d4, d5, d6 =   ', 6(1x,e15.7), /, 'cnvhst(n5), cnvhst(n5+1), (xk/xm(j) j=n7, n8)  ...', /, (8(1x,e15.7)))
  go to 11812
  !  past history of convolution input is stored in 'xm'.
11808 xm(n8) = d1
  n8 = n8 + 1
  do j=n6, n4, 2
     d6 = sconst(j + 4) * dltinv
     n7 = d6
     d6 = d6 - n7
     n7 = n8 - n7
     if (n7 .le. 0) n7 = n7 + lpast
     n9 = n7 - 1
     if (n9 .le. 0) n9 = n9 + lpast
     n10 = n9 - 1
     if (n10 .le. 0) n10 = n10 + lpast
     d6 = xm(n7) + xm(n9) - d6 * (xm(n7) - xm(n10))
     d4 = d4 + sconst(j + 3) * (d5 - d6)
     d5 = d6
11810 end do
  if (iprsup .lt. 9) go to 11812
  n7 = iabs(nr(ii)) + ipoint - 1
  if (n7 .gt. lpast) n7 = n7 - lpast
  write (lunit6, 23010) n2, n3, n4, n5, n6, n7, n8, n9, n10, d1, d2, d3, d4, d5, d6, cnvhst(n5), cnvhst(n5+1), (xm(j), j=n7, n8)
11812 d4 = d4 + sconst(n4 + 5) * d5
  cnvhst(n5 + 1) = d4
  d3 = d3 + d4
11814 volt(n2) = volt(n2) + d2
  volti(n2) = volti(n2) + d3
  ii = ii + 1
  if (cki(ii - 1) .gt. 0) go to 11790
  if (modout .le. 0) go to 11863
  do i = 1, modout
     n7 = nv + i
     bvalue(n7) = volt(i)
     n8 = n7 + modout
     bvalue(n8) = volti(i)
21863 end do
  !  increment f to reflect norton contribution of this branch.  compute
  !  branch current if requested by mbus(.)
11863 n2 = cik(k)
  do i=1, it2
     ii = i + k - 1
     n3 = - kbus(ii)
     n4 = mbus(ii)
     if (n2 .gt. 0) go to 11818
     if (n4 .gt. 0) go to 11816
     n4 = - n4
     kcount = kcount + 1
     bvalue(kcount) = volt(i)
11816 emtpf(n3) = emtpf(n3) - volti(i)
     emtpf(n4) = emtpf(n4) + volti(i)
     go to 11822
11818 if (n4 .gt. 0) go to 11820
     n4 = - n4
     d1 = 0.0
     n5 = n2
     do j=1, it2
        d1 = d1 + qfd(n5) * volt(j)
        n5 = n5 + it2
11819 end do
     kcount = kcount + 1
     bvalue(kcount) = d1
11820 n5 = n2
     d1 = 0.0
     do j=1, it2
        d1= d1 - qfd(n5) * volti(j)
        n5 = n5 + it2
11821 end do
     if (iprsup .ge. 9) write (lunit6, 23050) n2, n3, n4, kcount, d1, bvalue(kcount)
23050 format ('  Phase conversion at 11822.  n2, n3, n4, kcount, d1, b value(kcount) =   ', 4i6, 2(1x, e15.7))
     emtpf(n3) = emtpf(n3) + d1
     emtpf(n4) = emtpf(n4) - d1
     n2 = n2 + 1
11822 end do
  it2 = 1
11823 if (cki(k) .lt. 0.0) go to 1200
  k = k + 1
  go to 11823
  !  calculation of branch volt(volti) and norton voltk(vim) current
  !  for transmission line model for kbus(mbus)
11824 n2 = absz(cki(ii)) - 1.0
  n3 = n2 * phsinv + 1
  n2 = n2 - (n3 - 1) * it2 + 1
  n4 = ck(ii)
  n5 = indhst(ii)
  n6 = kodsem(ii) + 1
  n7 = nr(ii) + ipoint - 2
  n8 = ci(ii)
  !  d1(d2) = incident voltage wave at t=t0 for kbus(mbus)
  !  d3(d4) = incident voltage wave at t=t0+deltat for kbus(mbus)
  !  exponential(semlyen) representation of propagation transfer function
  d1 = 0.0
  d2 = 0.0
  d3 = 0.0
  d4 = 0.0
  if (n8 .eq. 0) go to 11847
  n7 = n7 + 2
  if (n7 .gt. lpast) n7 = n7 - lpast
  n9 = n7 - 1
  if (n9 .le. 0) n9 = n9 + lpast
  n10 = n7 + 1
  if (n10 .gt. lpast) n10 = n10 - lpast
  do j=1, n8
     if (iprsup .ge. 9) write (lunit6, 23060) n7, cnvhst(n5), cnvhst(n5+1), sconst(n6)
23060 format ('  Top of do 11838.  n7, cnvhst(n5), cnvhst(n5+1), sconst (n6) =    ', i10, 3(1x, e15.7))
     if (sconst(n6)) 11830, 11826, 11828
11826 d1 = d1 + cnvhst(n5 + 0)
     d2 = d2 + cnvhst(n5 + 1)
     cnvhst(n5 + 0) = cnvhst(n5 + 0) * sconst(n6 + 1)
     cnvhst(n5 + 1) = cnvhst(n5 + 1) * sconst(n6 + 1)
     go to 11830
11828 d5 = cnvhst(n5 + 0)
     d6 = cnvhst(n5 + 1)
     d1 = d1 + d5 + d5
     d2 = d2 + d6 + d6
     cnvhst(n5 + 0) = d5 * sconst(n6 + 1) - cnvhst(n5 + 2) * sconst(n6 + 6)
     cnvhst(n5 + 1) = d6 * sconst(n6 + 1) - cnvhst(n5 + 3) *  sconst(n6 + 6)
     cnvhst(n5 + 2) = d5 * sconst(n6 + 6) + cnvhst(n5 + 2) * sconst(n6 + 1)
     cnvhst(n5 + 3) = d6 * sconst(n6 + 6) + cnvhst(n5 + 3) *  sconst(n6 + 1)
11830 cnvhst(n5 + 0) = cnvhst(n5 + 0) + sconst(n6 + 2) * xm(n10) + sconst(n6 + 3) * xm(n7) + sconst(n6 + 4) * xm(n9)
     cnvhst(n5 + 1) = cnvhst(n5 + 1) + sconst(n6 + 2) * xk(n10) + sconst(n6 + 3) * xk(n7) + sconst(n6 + 4) * xk(n9)
     if (sconst(n6)) 11836, 11832, 11834
11832 d3 = d3 + cnvhst(n5 + 0)
     d4 = d4 + cnvhst(n5 + 1)
     go to 11836
11834 d3 = d3 + 2.0 * cnvhst(n5 + 0)
     d4 = d4 + 2.0 * cnvhst(n5 + 1)
11836 n5 = n5 + 2
     n6 = n6 + 5
     if (iprsup .ge. 9) write (lunit6, 23070) n5, n6, d3, d4, cnvhst(n5 - 2), cnvhst(n5 - 1)
23070 format ('  End of do 11838.  n5, n6, d3, d4, cnvhst(n5-2), cnvhs t(n5-1) =   ', 2i5, 4(1x,e13.6))
11838 end do
  n7 = n7 + length(ii) - 2
  if (n7 .gt. lpast) n7 = n7 - lpast
  !  update past history of outgoing voltage waves from each bus
  n9 = n3 + it2
  xk(n7) = semaux(n3) - d1
  xm(n7) = semaux(n9) - d2
  d1 = xk(n7) - d1
  d2 = xm(n7) - d2
11847 n10 = kodsem(ii)
  d5 = sconst(n10) * d1
  d6 = sconst(n10) * d2
  d3 = - d3 - d3
  d4 = - d4 - d4
  if (iprsup .lt. 9) go to 23110
  n8 = nr(ii) + ipoint - 1
  if (n8 .gt. lpast) n8 = n8 - lpast
  write (lunit6, 23100) n2, n3, n4, n5, n6, n7, n8, semaux(n3), semaux(n9), d1, d2, d3, d4, d5, d6, &
       (xk(j), xm(j), j=n8, n7)
23100 format (/, '  After voltage convolution.  n2, n3, n4, n5, n6, n7, n8  =   ', 7i10, /, &
           '   semaux(n3), semaux(n9), d1, d2, d3, d4, d5, d6, (xk(j), xm(j), j=n8, n7)  ....', /, (8(1x,e15.7)))
  !  volt(volti) = branch current at t=t0 at kbus(mbus).
  !  voltk(vim) = norton current via partial admittance convolution.
  !  exponential(semlyen) representation of admittance impulse...
23110 d7 = sconst(n10) * d3
  d8 = sconst(n10) * d4
  if (n4 .eq. 0) go to 11862
  d11 = semaux(n3)
  d12 = semaux(n9)
  do j=1, n4
     d9 = cnvhst(n5 + 0) + d11 * sconst(n6 + 2)
     d10 = cnvhst(n5 + 1) + d12 * sconst(n6 + 2)
     if (iprsup .ge. 9) write (lunit6, 23120) n5, n6, cnvhst(n5), cnvhst(n5+1), d9, d10, sconst(n6)
23120 format( 20h  top of do 11856.   ,2i10, 5(1x,e15.7))
     if (sconst(n6)) 11848, 11850, 11852
11848 cnvhst(n5 + 0) = cnvhst(n5 - 2) * sconst(n6 + 1) + d9 * sconst(n6 - 3) + d3 * sconst(n6 + 2) + d1 * sconst(n6 + 3)
     d9 = cnvhst(n5 - 2) * sconst(n6 - 3) - d9 * sconst(n6 + 1) + d3 * sconst(n6 - 2) + d1 * sconst(n6 - 1)
     cnvhst(n5 + 1) = cnvhst(n5 - 1) * sconst(n6 + 1) + d10 * sconst(n6 - 3) + d4 * sconst(n6 + 2) + d2 * sconst(n6 + 3)
     d10 = cnvhst(n5 - 1) * sconst(n6 - 3) - d10 * sconst(n6 + 1) + d4 * sconst(n6 - 2) + d2 * sconst(n6 - 1)
     cnvhst(n5 - 2) = d9
     cnvhst(n5 - 1) = d10
     d7 = d7 + d9 + d9
     d8 = d8 + d10 + d10
     go to 11854
11850 d5 = d5 + d9
     d6 = d6 + d10
     d9 = d9 * sconst(n6 + 1) + d3 * sconst(n6 + 2) + d1 * sconst(n6 + 3)
     d10 = d10 * sconst(n6 + 1) + d4 * sconst(n6 + 2) + d2 * sconst(n6 + 3)
     cnvhst(n5 + 0) = d9
     cnvhst(n5 + 1) = d10
     d7 = d7 + d9
     d8 = d8 + d10
     go to 11854
11852 d5 = d5 + d9 + d9
     d6 = d6 + d10 + d10
     cnvhst(n5 + 0) = d9
     cnvhst(n5 + 1) = d10
11854 n5 = n5 + 2
     n6 = n6 + 4
     if (iprsup .ge. 9) write (lunit6, 23130) d5, d6, d7, d8, d9, d10
23130 format( 46h  end of do 11856.  d5, d6, d7, d8, d9, d10 =   , 6(1x,e13.6))
11856 end do
11862 volt(n2) = volt(n2) + d5
  volti(n2) = volti(n2) + d6
  voltk(n2) = voltk(n2) + d7
  ndx1 = lsiz26 + n2
  volt(ndx1) = volt(ndx1) + d8
  ii = ii + 1
  if (cki(ii - 1) .gt. 0.0) go to 11824
  !  increment f to include the norton currents of this set of coupled
  !  branches.  include branch current in 'bvalue' if mbus(.) .lt. 0.
  n2 = cik(k)
  do i=1, it2
     ii = i + k - 1
     n3 = - kbus(ii)
     n4 = mbus(ii)
     if (n2 .gt. 0) go to 11866
     if (n4 .gt. 0) go to 11864
     n4 = - n4
     !  currents requested by col-80 punch is from 'kbus' to 'mbus' at 'kbus'
     !  terminal.  the corresponding 'bvalue(kcount) =  ... ' card which is a
     !  comment is for the current from 'mbus' to 'kbus' at 'mbus' terminal.
     kcount = kcount + 1
     bvalue(kcount) = volt(i)
     !     kcount = kcount + 1
     !     bvalue(kcount) = volti(i)
11864 emtpf(n3) = emtpf(n3) - voltk(i)
     ndx1 = lsiz26 + i
     emtpf(n4) = emtpf(n4) - volt(ndx1)
     go to 11874
11866 if (n4 .gt. 0) go to 11870
     n4 = - n4
     d1 = 0.0
     d2 = 0.0
     n5 = n2
     do j=1, it2
        d1 = d1 + qfd(n5) * volt(j)
        d2 = d2 + qfd(n5) * volti(j)
        n5 = n5 + it2
11868 end do
     kcount = kcount + 1
     bvalue(kcount) = d1
     !     kcount = kcount + 1
     !     bvalue(kcount) = d2
11870 n5 = n2
     d1 = 0.0
     d2 = 0.0
     ndx1 = lsiz26
     do j=1, it2
        d1 = d1 - qfd(n5) * voltk(j)
        ndx1 = ndx1 + 1
        d2 = d2 - qfd(n5) * volt(ndx1)
        n5 = n5 + it2
11872 end do
     if (iprsup .ge. 9) write (lunit6, 23170) n2, n3, n4, kcount, bvalue(kcount), d1, d2
23170 format( 50h  after phase conversion.  n2, n3, n4, kcount =   , 4i10,/, 30h   bvalue(kcount), d1, d2 =   ,3(1x,e15.7))
     emtpf(n3) = emtpf(n3) + d1
     emtpf(n4) = emtpf(n4) + d2
     n2 = n2 + 1
11874 end do
  it2 = 1
11876 if (cki(k) .lt. 0.0) go to 1200
  k = k + 1
  go to 11876
1190 if ( ipunch .eq. 0 )  go to 1200
  if ( length(k) .le. 0 )  go to 3564
  if ( kodebr(k) .gt. 0 )  go to 3564
  call redu17 ( x(n3), it2, ll0 )
3564 do i=1, it2
     volti(i)=-volti(i) * onehaf
     voltk(i)=-voltk(i) * onehaf
     if ( kodebr(k) .gt. 0 )  go to 1191
     volt(i) = -volt(i)
1191 end do
  call mult(x(n3), volt, cik(k), it2, ll1)
  call mult(emtpc(n3), volti, ci(k), it2, ll1)
  call mult(emtpc(n3), voltk, ck(k), it2, ll1)
1200 k=k+it2
  if ( iprsup  .ge.  4 ) write (lunit6, 7342)  k, ibr,  ( emtpf(i), i=1, ntot )
7342 format (/, ' k, ibr =', 2i6, 10x, '(emtpf(i), i=1, ntot )  follow ...', /, (1x, 8e16.7))
  if (k.le.ibr) go to 1100
  lastov = nchain
  nchain = 18
  if ( iprsup  .ge.  4 ) write (lunit6, 7342)  k, ibr,  ( emtpf(i), i=1, ntot )
9999 return
end subroutine subts2
!
!     subroutine fdcinj.
!
subroutine fdcinj( ikf, isfd, ibf )
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  dimension  ur(40)
  !     this routine updates the current injections for the individual
  !     branches as well as for the equivalent  branches inserted into the
  !     emtp. it also updates the capacitor and inductor voltages and
  !     calculates branch currents for the individual branches   *   *   *
  idk = 2 * ikf
  ikf = ikf + 1
  isf = isfd + 1
  isc = ibf + 1
  if ( iprsup .gt. 0 ) write ( lunit6, 1 ) ikf, isfd, ibf, imfd(idk+1), imfd(idk+2)
1 format(41h integer counters at start of fdcinj.....,7x,3hikf,6x,4hisfd,7x,3hibf,6x,4hizfd,6x,4hipfd,/,41x,5i10)
  !     convert phase voltages to modal frame of reference   *   *   *   *
  cz = it2
  cz = 1.0 / cz
  ur(1) = volt(1)
  do ka = 2, it2
     ur(1) = ur(1) + volt(ka)
2    ur(ka) = ( volt(1) - volt(ka) ) * cz
  end do
  ur(1) = ur(1) * cz
  if ( iprsup .gt. 0 ) write ( lunit6, 3 ) ikf, ( ur(ka), ka = 1, it2 )
3 format(37h modal voltages in fdcinj for set no.,i6,/, (2x,6e21.11))
  !     process first the 'zero' sequence data   *   *   *   *   *   *   *
  ar = 0.
  ist = isf
  isu = isfd + imfd(idk+1) * 5
  isv = ibf - 2
  do ka = ist, isu, 5
     isv = isv + 3
     !     calculate branch current     *   *   *   *   *   *   *   *   *   *
     azr = rmfd(ka+1) * ur(1) + cikfd(isv)
     !     c a p a c i t o r   v o l t a g e    *   *   *   *   *   *   *   *
     cikfd(isv+2) = cikfd(isv+2) + rmfd(ka+2) * ( azr + cikfd(isv+1) )
     !     i n d u c t o r   v o l t a g e  *   *   *   *   *   *   *   *   *
     azi = ur(1) - azr * rmfd(ka) - cikfd(isv+2)
     azi = -rmfd(ka+4) * azi - 2. * cikfd(isv+2) + ur(1)
     !     c u r r e n t   i n j e c t i o n    *   *   *   *   *   *   *   *
     azi = rmfd(ka+1) * azi - rmfd(ka+3) * azr
     ar = ar + azi
     cikfd(isv) = azi
4    cikfd(isv+1) = azr
  end do
  ur(1) = ar
  !     process the remaining ( it2 - 1 ) modes  *   *   *   *   *   *   *
  isfd = isu
  ibf = isv + 2
  isk = imfd(idk+2)
  ist = isfd + 1
  isu = isfd + isk * 5
  !     loop across modes            *   *   *   *   *   *   *   *   *   *
  do ka = 2, it2
     ar = 0.
     un = ur(ka)
     do  kb = ist, isu, 5
        isv = isv + 3
        !     calculate branch current     *   *   *   *   *   *   *   *   *   *
        azr =  rmfd(kb+1) * un + cikfd(isv)
        !     c a p a c i t o r   v o l t a g e    *   *   *   *   *   *   *   *
        cikfd(isv+2) = cikfd(isv+2) + rmfd(kb+2) * ( azr + cikfd(isv+1) )
        !     i n d u c t o r   v o l t a g e  *   *   *   *   *   *   *   *   *
        azi = un - azr * rmfd(kb) - cikfd(isv+2)
        azi = -azi * rmfd(kb+4) - 2. * cikfd(isv+2) + un
        azi = rmfd(kb+1) * azi - rmfd(kb+3) * azr
        ar = ar + azi
        cikfd(isv) = azi
5       cikfd(isv+1) = azr
     end do
6    ur(ka) = ar
  end do
  ibf = ibf + ( it2 - 1 ) * isk * 3
  isfd = isu
  !     convert modal current injections to phase coordinates    *   *   *
  cz = it2
  volt(1) = 0.
  do  ka = 1, it2
7    volt(1) = volt(1) + ur(ka)
  end do
  do ka = 2, it2
8    volt(ka) = volt(1) - cz * ur(ka)
  end do
  if ( iprsup .le. 0 ) go to 15
  write ( lunit6, 9 ) ( volt(ka), ka = 1, it2 )
9 format(35h phase current injections cik......,/,(2x,6e21.11))
  write ( lunit6, 10 ) isc, ibf, ( cikfd(ka), ka = isc, ibf )
10 format(31h updated modal branch data from,i6,4h  to,i6,/, (2x,6e21.11))
15 return
end subroutine fdcinj
!
!     subroutine update.
!
subroutine update
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     this module is used only by brandwajn (type-59) s.m. model
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'tacsar.ftn'
  include 'syncom.ftn'
  include 'synmac.ftn'
  include 'dekspy.ftn'
  dimension vsmout(1)
  equivalence ( ismout(1), vsmout(1) )
  dimension massex(1)
  equivalence ( histq(1), massex(1) )
  !     intrinsic  cosz, sinz, absz, sqrtz, atan2z
  !     this routine adjusts the current sources to be injected into
  !     the equivalent pi-circuits * * * * * * * * * * * * * * * * * * * *
  if ( iprsup  .ge.  1 ) write ( lunit6, 4099 )
4099 format ( 24h  "begin module update." )
  !     initialize counters     ******************************************
  ilk = 0
  ll0 = 0
  ll1 = 1
  !     counters for electrical variables ********************************
  ibr =  ibrold
  nwd = 24
  ikn = 1
  i26 = 0
  ll7 = 7
  !     counters for mechanical variables ********************************
  iu = 0
  ib = 0
  !     conters for tacs  interface and output. computational constants
  lmset = 0
  icnt = msmout
  ipout = -2
  !!!!  acb = omdt * istep
  acb = omega * t
  if ( iprsup .gt. 0 ) write ( lunit6, 4100 ) ibr, numsm, it
4100 format(17h begin  'update' ,5x, 3hibr, 5x, 5hnumsm, 8x, 2hit, /, 15x, 3( 5x,i5 ) )
  j30 = 1
  i30 = 1
  j75 = 27
  n22spy = ksmspy( 1 )
  do k = 1, numsm
     im = ismdat( j30 )
     n1 = ismdat( j30+2 )
     n2 = ismdat( j30+5 )
     v1 = emtpe( n1 ) - emtpe( n2 )
     n1 = ismdat( j30+3 )
     n2 = ismdat( j30+6 )
     v2 = emtpe( n1 ) - emtpe( n2 )
     n1 = ismdat( j30+4 )
     n2 = ismdat( j30+7 )
     v3 = emtpe( n1 ) - emtpe( n2 )
     idsat = 0
     idelta = ismdat( j30+1 )
     ap1 = 0.0
     ap2 = 0.0
     if ( iprsup  .gt.  0  ) write ( lunit6, 4101 ) v1, ismdat(j30+2), v2, ismdat(j30+3), v3, ismdat(j30+4)
4101 format ( 13x, 2hv1, 9x, 6hnodsma, 13x, 2hv2, 9x, 6hnodsmb, 13x, 2hv3, 9x, 6hnodsmc, /, 3(1x, e14.7, 10x, i5) )
800  ilk = ilk + 1
     numask = ismdat( i30+11 )
     nlocg = ismdat( i30+12 )
     nloce = ismdat( i30+13 )
     num2 = numask + numask
     num4 = num2 + num2
     num6 = num4 + num2
     n26  = iu + num4
     n27  = n26 + numask
     ikv = iu + numask
     ikw = ikv + 1
     ibu = ib + 1
     n22 = ib + num2
     kag = iu + nlocg
     kag2 = kag + num2
     ksg = kag + numask
     ksex = ikv + nloce
     ikp = ikv + numask
     !     calculate currents for time ''t-deltat''
     i75 = i26 + 27
     a1 = elp( i75 )
     a2 = elp(i75+1)
     juk = ibr + 1
     d6 =  -a1 * v1 - a2 * (v2+v3) - cik( juk )
     d7 =  -a1 * v2 - a2 * (v1+v3) - cik( juk+1 )
     d8 =  -a1 * v3 - a2 * (v1+v2) - cik( juk+2 )
     cv3 = ( v1 + v2 + v3 ) * asqrt3
     a5 = - ( cv3 - cu( ikn+2 ) ) * elp( i26+17 )
     if ( iprsup .gt. 0 ) write ( lunit6, 4102 )  d6, d7, d8
4102 format( 21h for time "t-deltat"., 18x, 2hd6, 18x, 2hd7, 18x, 2hd8, /, 21x, 3e20.11 )
     !     transform phase variables to park"s coordinates  * * * * * * * * *
     cz = elp( i26+26 )
     czt = cz * tenm6
     acde = elp( i75+2 )
     acdf = elp( i75+3 )
     ispdr = 1
     q3 = -cu( ikn+10 ) * cu( ikn+11 ) * czt
     etot = cu( ikn+19 )
     sum  = cu( ikn+20 )
     dsped = histq( ksg )
     n7 = ismdat( i30+16 )
     !     enter the iteration loop(angle,speed,current)  * * * * * * * * * *
200  etot = etot * athtw
     sum = sum * athtw
     au = etot * onehaf
     av = sum * sqrt32
     tsd = av - au
     tsc =-au - av
     a3  =  d6 * etot + d7 * tsd + d8 * tsc
     cv1 =  v1 * etot + v2 * tsd + v3 * tsc
     au = au * sqrt3
     av = sum * onehaf
     tsd = au - av
     tsc =-av - au
     a4  =  d6 * sum + d7 * tsc + d8 * tsd
     cv2 =  v1 * sum + v2 * tsc + v3 * tsd
     !     calculate rotor currents ****************************************
     c1 = elp( i75+5 ) * a3 + elp( i75+9 ) * cu( ikn+3 ) + elp( i75+10 ) * cu( ikn+4 )
     c2 = elp( i75+6 ) * a3 + elp( i75+10 ) * cu( ikn+3 ) + elp( i75+12 ) * cu( ikn+4 )
     c3 = elp( i75+7 ) * a4 + elp( i75+13 ) * cu( ikn+5 ) + elp( i75+14 ) * cu( ikn+6 )
     c4 = elp( i75+8 ) * a4 + elp( i75+14 ) * cu( ikn+5 ) + elp( i75+16 ) * cu( ikn+6 )
     if ( iprsup .lt. 1 )   go  to  922
     write ( lunit6, 4103 )    a3, a4, a5, cv1, cv2, cv3
4103 format( 40x, 17h updated currents, 40x, 17h updated voltages, /,3x, 3e18.10, 3x, 3e18.10 )
     write ( lunit6, 4104 )    c1, c2, c3, c4
4104 format( 23h updated rotor currents, /, 2x, 4( e20.12,5x ) )
     !     calculate the electromagnetic torque*****************************
922  ac2 = elp(i26+1)*a3 + elp(i26+2)*c1 + elp(i26+4)*c2
     ac1 = elp(i26+9)*a4 + elp(i26+10)*c3 + elp(i26+12)*c4
     cd = ( ac2 * acde * a4 - ac1 * acdf * a3 ) * czt
     !     resolve the mechanichal equations. note the  constant terms have
     !     been calculated in the previous time-step* * * * * * * * * * * * *
     cexc = histq( ksex )
     jt = n27
     if ( n7 .gt. 0 )   go to  220
     do ik = ikw, ikp
        jt = jt + 1
219     histq( ik ) = histq( jt ) / histq( ik )
     end do
     go to 222
220  m1 = n7
     do ik = ikw, ikp
        jt = jt + 1
        m1 = m1 - 1
        ndx1 = massex( m1 )
        if ( ndx1 .gt. 0 )   go  to  221
        q2 = 1.0
        go to 201
221     ndx1 = kxtcs + ndx1
        q2 = xtcs( ndx1 )
201     histq( ik ) = ( histq( jt ) * q2 ) / histq( ik )
     end do
222  histq( ksg ) = histq( ksg ) - cd
     if ( nloce .eq. 0 )   go  to  202
     cexc = ( q3 * c1 ) / cexc
     histq( ksex ) = histq( ksex ) - cexc
     !     calculate new estimates of mechanical speeds and rotor angle * * *
202  jt = n26
     do ik = ikw, ikp
        jt = jt + 1
        histq( jt ) = -histq( ik )
203     histq( ik ) =  histq( ik ) - histq( ik+num2 )
     end do
     call bansol( shp(ibu), histq(ikw), numask )
     spdn = histq( ksg )
     !     check the convergence of rotor speed ****************************
     if ( absz(dsped) .le. flzero )  dsped = flzero
     spdd = absz((spdn - dsped) / dsped )
     if ( spdd .gt. epdgel )  go  to  206
     !     successful iteration (convergence achieved ) * * * * * * * * * * *
     !     finish the calculation of the mechanical angles  * * * * * * * * *
204  kc = n22 + 5 * numask
     do ka = 1, numask
        jt = iu + ka
        ik = ikv + ka
        kb = n26 + ka
        kd = kc + ka
        histq( ik+num2 ) = histq( kb ) - shp( kd )
205     histq( jt ) = histq( jt+num2 ) + delta2 * histq( ik )
     end do
     !     store the calculated currents and voltages   * * * * * * * * * * *
     cu( ikn ) = a3
     cu(ikn+1) = a4
     cu(ikn+2) = a5
     cu(ikn+3) = c1
     cu(ikn+4) = c2
     cu(ikn+5) = c3
     cu(ikn+6) = c4
     d2 = spdn * cz
     q3 = cu( ikn+10 ) * cu( ikn+11 )
     dang = histq( kag ) * cz
     go   to   208
206  dsped = spdn
     ispdr = ispdr + 1
     dang = ( histq( kag2 ) + delta2 * spdn ) * cz
     etot = cosz( dang )
     sum = sinz( dang )
     if ( ispdr .le. iprsov(37) )  go to 200
     if ( spdd .gt. epomeg )  go  to  920
     write ( lunit6, 4105 )   ilk, istep, iprsov(37)
4105 format( 2x, 7hwarning,2x,  15(1h*), /,2x, 35hlack of convergence for machine no., i5, 2x, 11hon step no., i10, &
          /,  ' iteration limit  iprsov(37) =',  i10  )
     go to 204
920  lstat(19) = 206
     lstat(18) = nchain
     k1 = ismdat( j30+2 )
     bus1 = bus( k1 )
     flstat(14) = spdd
     lstat(12) = ilk
     kill = 104
     return
208  if ( iprsup .eq. 0 )  go to  50
     write ( lunit6, 4106 )   spdn, histq(ksex), cd, cexc
4106 format( 6x, 23hafter speed calculation, 5x, 5hrotor, 13x,7hexciter, 17x, 3hteg, 16x, 4htexc, /, 20x, 4e20.12 )
     !     load internal machine variables into tacs variable 'etac' *******
50   if ( n22spy .ne. -1 )  go to 51
     ksmspy( 2 ) = ilk
     call emtspy
     if ( ksmspy( 1 ) .eq. -1 )  go to 51
     if ( ksmspy( 1 ) .eq. 2 )  n22spy = 0
     if ( n22spy .eq. 0 )   go to 51
     smoutv( 1 ) = d6
     smoutv( 2 ) = d7
     smoutv( 3 ) = d8
     smoutv( 4 ) = cv1
     smoutv( 5 ) = cv2
     smoutv( 6 ) = cv3
     smoutv( 7 ) = q3
     smoutv( 8 ) = v1
     smoutv( 9 ) = v2
     smoutv( 10 ) = v3
     smoutv( 11 ) = cd
     smoutv( 12 ) = cexc
     go to 50
51   kmset =  ismdat( i30+14 )
     if ( kmset  .eq.  0 )   go to 8610
     do i = 1, kmset
        lmset = lmset + 1
        jmset = ismtac( lmset )
        if ( jmset .gt. 0 )  go  to  1696
        jmset = -jmset
        if ( jmset .gt. 7 )  go  to  1680
        etac( lmset ) = cu( ikn+jmset-1 )
        go  to  8650
1680    if ( jmset .gt. 8 )  go  to  1681
        etac( lmset ) = cv1
        go  to  8650
1681    if ( jmset .gt. 9 )  go  to  1682
        etac( lmset ) = cv2
        go  to  8650
1682    if ( jmset .gt. 10 )  go  to  1683
        etac( lmset ) = cv3
        go  to  8650
1683    if ( jmset .gt. 11 )  go  to  1684
        etac( lmset ) = q3
        go  to  8650
1684    if ( jmset .gt. 13 )  go  to  1686
        sf4 = a3 * elp( i26+21 ) + c1 + c2
        sf5 = ( a4 * elp( i26+21 ) + c3 + c4 ) * elp( i75+4 )
        if ( jmset .eq. 13 )  go  to  1685
        etac( lmset ) = sqrtz( sf4**2 + sf5**2 )
        go  to  8650
1685    etac( lmset ) = atan2z( sf5, sf4 )
        go  to  8650
1686    if ( jmset .gt. 14 )  go  to  1687
        etac( lmset ) = cd
        go  to  8650
1687    if ( jmset .gt. 15 )  go  to  1688
        etac( lmset ) = cexc
        go  to  8650
1688    if ( jmset .eq. 17 )  go  to  1689
        etac( lmset ) = ac2 * acde + elp( i26+19 ) * a3
        go  to 8650
1689    etac( lmset ) = ac1 * acdf + elp( i26+19 ) * a4
        go  to  8650
1696    if ( jmset .gt. num2 )  go  to  1697
        n9 = iu + jmset
        etac( lmset ) = histq( n9 )
        go  to  8650
1697    jmset1 = jmset - num2
        m = iu + jmset1
        n5 = m + numask
        ids = n22 +  jmset
        etac(lmset) = shp(ids+numask) * ( histq(m) - histq(m+1) ) + shp( ids ) * ( histq( n5 ) - histq( n5+1 ) )
8650 end do
8610 n9 = ismdat( i30+17 )
     if ( n9 .lt. 0 )  go  to  209
     if ( n9 .eq. 0 )  go  to  8215
     n8 =  ikn - 1
     do ka = 1, n9
        ipout = ipout + 3
        icnt =  icnt + 1
        n5 = ismout( ipout )
        go to (8201,8201,8201,8201,8201,8201,8201,8202,8203,8204,8205,8206,8206,8208,8209,8207 ), n5
8201    l = n5  + n8
        vsmout( icnt ) = cu( l )
        go to 8200
8202    vsmout( icnt ) = d6
        go to 8200
8203    vsmout( icnt ) = d7
        go to 8200
8204    vsmout( icnt ) = d8
        go to 8200
8205    vsmout( icnt ) = q3
        go to 8200
8206    sf4 = a3 * elp( i26+21 ) + c1 + c2
        sf5 = ( a4 * elp( i26+21 ) + c3 + c4 ) * elp( i75+4 )
        if ( n5 .eq. 13 ) go to 8207
        vsmout( icnt ) = sqrtz( sf4**2 + sf5**2 )
        go to 8200
8207    vsmout( icnt ) = atan2z( sf5, sf4 )
        go to 8200
8208    vsmout( icnt ) = cd
        go to 8200
8209    vsmout( icnt ) = cexc
8200 end do
8215 n9 = ismdat( i30+18 )
     if ( n9 .eq. 0 )  go  to  8225
     d9 = acb / cz
     do ka = 1, n9
        ipout =  ipout  + 3
        icnt = icnt + 1
        n5 = ismout(  ipout ) + iu
        vsmout( icnt ) = ( histq( n5 ) - d9 ) * radeg
8220 end do
8225 n9 = ismdat( i30+19 )
     if ( n9 .eq. 0 )  go  to  8235
     d9 = omega / cz
     n12 = iu + numask
     do ka = 1, n9
        ipout = ipout + 3
        icnt =  icnt + 1
        n13 = ismout( ipout ) + n12
        vsmout( icnt ) = histq( n13 ) - d9
8230 end do
8235 n9 = ismdat( i30+20 )
     if ( n9 .eq. 0 )  go  to  209
     jt = n22 + num2
     do ka = 1, n9
        icnt = icnt + 1
        ipout = ipout + 3
        n5 = ismout(  ipout )
        mp = jt + n5
        m = iu + n5
        n5 = m + numask
        vsmout( icnt ) = shp(mp+numask) * ( histq(m) - histq(m+1) ) + shp( mp ) * ( histq( n5 ) - histq( n5+1 ) )
8239 end do
     !     predict new rotor angle and speed. calculate also constant terms
     !     for the iteration loop in the next time-step   * * * * * * * * * *
209  s1 = 0.0
     iz = iu + 1
     iy = ikv
     izy = iy + num2
     ik  = n22 + num6
     ib = ik
     d11 = histq( iz )
     d12 = histq( iy+1 )
     if ( numask .eq. 1 )  go to  211
     ik1 = ikv - 1
     do ka = iz, ik1
        iy = iy + 1
        izy = izy + 1
        ik = ik + 4
        d31 = histq( ka+1 )
        d32 = histq( iy+1 )
        d21 = shp( ik-2 )
        d22 = shp(  ik  )
        histq(izy) = histq(izy) + shp(ik-3) * d11 + shp(ik-1) * d12 + d21 * d31 + d22 * d32 + s1
        s1 = d21 * d11 + d22 * d12
        d11 = d31
        d12 = d32
210  end do
     s1 = s1 + shp( ik+1 ) * d11
211  histq( izy+1 ) = histq( izy+1 ) +  shp( ik+3 ) * d12  +  s1
212  kc = n22
     ik = ikv
     izy = iz + num2
     do ka = iz, ikv
        ik = ik + 1
        histq( izy ) = histq( ka ) + delta2 * histq( ik )
        kc = kc + 1
        d6 = shp( kc )
        shp( kc ) = histq( ik )
        histq( ik ) = histq( ik ) * 2.0 - d6
213     izy = izy + 1
     end do
     if ( iprsup .lt. 1 )   go   to   214
     ij = iu + 1
     ik = iu  + num6
     write ( lunit6, 4107 )  ij, ik, ( histq( ka ), ka = ij, ik )
4107 format( 2x,  12hhistq at 213,  2i8  ,/, ( 1x, 5e22.13) )
     !     finish the prediction of speed and angle   **********************
214  alpha = 9.0 * ( cu( ikn+21 ) - dang ) + cu( ikn+22 ) + delta6 * ( d2  + cu( ikn+23 ) )
     cu( ikn+22 ) = cu( ikn+21 )
     cu( ikn+21 ) = dang
     cu( ikn+23 ) = d2
     if ( ismdat( i30+8 ) .eq. 0 )   go  to 946
     !     update the saturation status variables   *   *   *   *   *   *   *
     sf4 = a3 * elp( i26+21 ) + c1 + c2
     sf5 = ( a4 * elp( i26+21 ) + c3 + c4 ) * elp( i75+4 )
     sf4 = sqrtz( sf4**2 + sf5**2 )
     if ( elp( i26+22 ) .ge. sf4 )  go  to  938
     d9 = sf4 / elp( i26+22 ) - 0.9
     isd =  10.0 * d9
     isd = ( isd + 1 ) / 2
     elp(i75+2) = 1.0 / ( 1.0 + elp(i26+23) * ( sf4 - elp(i26+22) ))
     go to 939
938  isd = 0
     elp( i75+2 ) = 1.0
939  if ( elp( i26+24 ) .ge. sf4 )  go  to 941
     d9 = sf4 / elp( i26+24 ) - 0.9
     isq =  10.0 * d9
     isq = ( isq + 1 ) / 2
     elp(i75+3) = 1.0 / ( 1.0 + elp(i26+25) * ( sf4 - elp(i26+24) ))
     go to 942
941  isq = 0
     elp( i75+3 ) = 1.0
942  if ( isd .eq. ismdat( i30+9 ) .and. isq .eq. ismdat( i30+10 ) ) go  to  944
     ismdat( i30+9 ) = isd
     ismdat( i30+10 ) = isq
     call increm( ilk, sf4 )
     ialter = 1
     idsat = idsat + 1
944  acde = elp( i75+2 )
     acdf = elp( i75+3 )
     !     calculate stator flux linkages.  *   *   *   *   *   *   *   *   *
946  ac1  = -ac1 * acdf  -  elp( i26+19 ) * a4
     ac2  =  ac2 * acde  +  elp( i26+19 ) * a3
     !     predict for the next time-step the new stator  currents**********
     !     acur1 = 2.0 * a3 - cu( ikn+7 )    !d-axis predicted current
     !     cu( ikn+7 ) = a3
     !     acur2 = 2.0 * a4 - cu( ikn+8 )    !q-axis predicted current
     acur2 = ( 2.5 * a4 - 1.5 * cu( ikn+13 ) + cu( ikn+8 ) ) * onehaf
     cu( ikn+13 ) = cu( ikn+8 )
     cu( ikn+8 ) = a4
     !     update the field voltage ( if tacs controlled )******************
     q4 = cu( ikn+10 )
     jmset = ismdat( i30+15 )
     if ( jmset  .eq.  0  )   go  to  948
     ndx1 = kxtcs + jmset
     cu( ikn+11 ) = xtcs( ndx1 )
     q4 = q4 * cu( ikn+11 )
     !     calculate history terms for the electrical part  * * * * * * * * *
948  acd = elp(i75+17)*a3 + elp(i75+18)*c1 + elp(i75+19)*c2
     acd = acd + ( ac1 * d2 - cv1 ) * damrat
     acq = elp(i75+20)*a4 + elp(i75+21)*c3 + elp(i75+22)*c4
     acq = acq + ( ac2 * d2 - cv2 ) * damrat
     x1( 3 ) = elp( i26+18 ) * cu( ikn+2 ) - cv3 * damrat
     x1( 4 ) = elp(i75+23)*a3 + elp(i75+24)*c1 + elp(i75+25)*c2
     x1( 5 ) = elp(i75+26)*a3 + elp(i75+27)*c1 + elp(i75+28)*c2
     x1( 6 ) = elp(i75+29)*a4 + elp(i75+30)*c3 + elp(i75+31)*c4
     x1( 7 ) = elp(i75+32)*a4 + elp(i75+33)*c3 + elp(i75+34)*c4
     if ( iprsup .lt. 1 )   go   to  952
     write ( lunit6, 4108 )   ilk, acd, acq, ( x1( itq ), itq = 3, 7 )
4108 format( 2x, 29h at 16 history terms mach no., i5,/,(2x,6e19.11))
     write ( lunit6, 4109 )  acur1, acur2
4109 format( 2x, 26h predicted stator currents, 2x, 2e25.12 )
952  x1( 4 ) = x1( 4 ) - ( q3*damrat + q4 )
     !     account for the influence of  rotor circuits  *   *   *   *   *
     sf4 = - ( elp(i75+43) * x1( 4 ) + elp(i75+44) * x1( 5 ) )
     sf5 = - ( elp(i75+45) * x1( 6 ) + elp(i75+46) * x1( 7 ) )
     x1( 1 ) = acd + sf4
     x1( 2 ) = acq + sf5
     call mover( x1( ll1 ), cu( ikn ), ll7 )
     !     add correction terms( account for assymetry)     *****************
     !     sf4 = sf4 - ac( il ) * acur1       !no d-axis correction term
     sf5 = sf5 - elp( i75+52 ) * acur2
     !     perform remaining operations in the synchronous frame*************
     a1 = cu( ikn+18 ) - dang
     a2 = sinz( a1 )
     a1 = cosz( a1 )
     c1 = a1 * ac2 + a2 * ac1
     c2 = a2 * ac2 - a1 * ac1
     !     predict speed voltages*******************************************
     ac2 = ( 2.5 * c1 - 1.5 * cu( ikn+14 ) + cu( ikn+15 ) ) * om2
     ac1 = ( 2.5 * c2 - 1.5 * cu( ikn+16 ) + cu( ikn+17 ) ) * om2
     cu( ikn+14 ) = cu( ikn+15 )
     cu( ikn+15 ) = c1
     cu( ikn+16 ) = cu( ikn+17 )
     cu( ikn+17 ) = c2
     !     convert remaining variables to synchronous frame******************
     q3 = cosz( alpha )
     q4 = sinz( alpha )
     cu( ikn+18 ) = cu( ikn+18 ) + omdt
     cu( ikn+19 ) = q3
     cu( ikn+20 ) = q4
     a5 = cu( ikn+18 )
     c1 = cosz( a5 )
     c2 = sinz( a5 )
     a3 = c1 * q3 + c2 * q4
     a4 = c2 * q3 - c1 * q4
     q3 = a3 * sf4 - a4 * sf5
     q4 = a4 * sf4 + a3 * sf5
     !     convert  voltage sources to current sources  * * * * * * * * * * *
     a3 = ( a1 * acd - a2 * acq - ac1 + q3 ) * elp( i75+53 )
     a4 = ( a2 * acd + a1 * acq + ac2 + q4 ) * elp( i75+53 )
     a5 = x1( 3 )  *  elp( i75+54 )
     !     convert synchronous variables to phase coordinates  **************
     cik( juk ) = -a3 * c1 - a4 * c2 - a5
     a1 = -c1 * onehaf
     a2 = c2 * sqrt32
     etot = a1 + a2
     sum = a1 - a2
     a1 = -c2 * onehaf
     a2 = c1 * sqrt32
     c3 = a1 - a2
     c4 = a1 + a2
     cik( juk+1 ) = -etot * a3 - c3 * a4 - a5
     cik( juk+2 ) = -sum * a3 - c4 * a4 - a5
     ibr = ibr + 3
     if ( iprsup  .gt.  0  ) write ( lunit6, 4110 )   alpha, ( cik(ip), ip = juk, ibr )
4110 format( 10x, e20.12, 4x, 3e20.12)
     a1 = elp( i75 )
     a2 = elp(i75+1)
     if ( idelta .eq. 0 )   go to 897
     a11 = ( a1 - a2 ) / 3.0
     a1 = a11 + a11
     a2 = -a11
897  ap1 = ap1 + a1
     ap2 = ap2 + a2
     !     end  of  updating  procedure * * *** * * * * * * * * * * * * * * *
     ib = ib + num4
     iu = ikp + num4
     ikn = ikn + nwd
     i26 = i26 + 101
     i30 = i30 + 30
     im = im - 1
     if ( im .gt. 0 ) go to 800
     if ( idsat .eq. 0 )    go  to   899
     !     update the appropriate elements of the matrix (ykm)  *************
     ies = j75 + 54
     ifs = j30 + 20
     n1 = ismdat( ifs+1 )
     ykm( n1 ) = elp( ies+1 ) + ap1
     n1 = ismdat( ifs+4 )
     ykm( n1 ) = elp( ies+4 ) + ap2
     n1 = ismdat( ifs+7 )
     ykm( n1 ) = elp( ies+7 ) + ap2
     n1 = ismdat( ifs+2 )
     ykm( n1 ) = elp( ies+2 ) + ap2
     n1 = ismdat( ifs+5 )
     ykm( n1 ) = elp( ies+5 ) + ap1
     n1 = ismdat( ifs+8 )
     ykm( n1 ) = elp( ies+8 ) + ap2
     n1 = ismdat( ifs+3 )
     ykm( n1 ) = elp( ies+3 ) + ap2
     n1 = ismdat( ifs+6 )
     ykm( n1 ) = elp( ies+6 ) + ap2
     n1 = ismdat( ifs+9 )
     ykm( n1 ) = elp( ies+9 ) + ap1
899  j75 = j75 + 101
900  j30 = j30 + 30
  end do
  nexmod = 0
  ll3 = 3
  call move0 ( ksmspy(1), ll3 )
  if ( iprsup  .gt.  0 ) write ( lunit6, 4111 )
4111 format ( 24h  "exit  module update." )
  return
end subroutine update
!
!     subroutine increm.
!
subroutine increm(ilk, sf3)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     this module is used only by brandwajn (type-59) s.m. model
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'tacsar.ftn'
  include 'synmac.ftn'
  if ( iprsup  .ge.  1 ) write ( lunit6, 6000 )
6000 format( 24h  "begin module increm." )
  i30 = 30 * ilk - 29
  write ( lunit6, 6001 ) ilk, ismdat( i30+9 ), ismdat( i30+10 )
6001 format( 4h ***, 10x, 11hmachine no., i6, 10x, 31hbegin operation on segments no., 2x, 2i10 )
  ll36 = 36
  call mover0(z(1),ll36)
  acde = 1.0
  acdf = 1.0
  i26 = 101 * ilk - 100
  i75 = i26 + 26
  !     start  with  incremental  inductances of the direct axis**********
  asd = elp( i26+21 )
  if ( sf3 .le. asd )     go  to  205
  !     d - axis  saturated **********************************************
  sb = ismdat( i30+9 )
  sb = sb * 0.1
  sf4 = asd * ( .9 + sb )
  sf5 = asd * ( 1.1 + sb )
  sf6 = sf4 / ( 1.0 + elp( i26+22 ) * ( sf4 - asd ) )
  sf7 = sf5 / ( 1.0 + elp( i26+22 ) * ( sf5 - asd ) )
  acde = ( sf7 - sf6  ) / ( sf5 - sf4 )
  !     calculate incremental inductances for the quadrature axis ********
205 asd = elp( i26+23 )
  if ( sf3 .le. asd )   go  to  206
  !     q - axis  saturated **********************************************
  sb = ismdat( i30+10 )
  sb = sb * 0.1
  sf4 = asd * ( .9 + sb )
  sf5 = asd * ( 1.1 + sb )
  sf6 = sf4 / ( 1.0 + elp( i26+24 ) * ( sf4 - asd ) )
  sf7 = sf5 / ( 1.0 + elp( i26+24 ) * ( sf5 - asd ) )
  acdf = ( sf7 - sf6  ) / ( sf5 - sf4 )
206 z( 50 ) = ( elp(  i26  ) * acde + elp( i26+18 ) ) * factom
  z( 51 ) = ( elp( i26+8 ) * acdf + elp( i26+18 ) ) * factom
  z( 52 ) = elp( i26+2 ) + elp( i75+35 ) * acde
  z( 53 ) = elp( i26+5 ) + elp( i75+36 ) * acde
  z( 54 ) = elp( i26+10 ) + elp( i75+37 ) * acdf
  z( 55 ) = elp( i26+13 ) + elp( i75+38 ) * acdf
  !     fill in  the  the  transformer terms   of  the resistive history**
  !     matrix of  the  generator*****************************************
  z( 1 ) = z( 50 ) + elp( i26+19 )
  z( 8 ) = z( 51 ) + elp( i26+19 )
  z( 15 ) = z( 52 ) + elp( i26+6 )
  z( 22 ) = z( 53 ) + elp( i26+7 )
  z( 29 ) = z( 54 ) + elp( i26+14 )
  z( 36 ) = z( 55 ) + elp( i26+15 )
  z( 11 ) = elp( i75+41 ) * acdf
  z( 26 ) = z( 11 )
  z( 12 ) = elp( i75+42 ) * acdf
  z( 32) = z( 12 )
  z( 30 ) = elp( i26+12 ) * acdf
  z( 35 ) = z( 30 )
  z( 3 ) = elp( i75+39 ) * acde
  z( 13 ) = z( 3 )
  z( 4 ) = elp( i75+40 ) * acde
  z( 19 ) = z( 4 )
  z( 16 ) = elp( i26+4 ) * acde
  z( 21 ) = z( 16 )
  if ( iprsup .lt. 1 )    go  to  208
  do k = 1, 6
     n1 = ( k - 1 ) * 6 + 1
     n2 = n1 + 5
407  write ( lunit6, 6004 )    k, ( z(i), i = n1, n2 )
  end do
208 call mover( z(1), x1( 1 ), ll36 )
  ll7 = 6
  ll3 = 2
  call redusm( x1( 1 ), ll7, ll3 )
  if ( iprsup .le. 0 )   go  to  210
  write ( lunit6, 6003 )   ilk
6003 format ( /, 23h reduced network model., i10 )
  do k = 1, 6
     n1 = ( k - 1 ) * 6 + 1
     n2 = n1 + 5
409  write ( lunit6, 6004 )   k, ( x1(i), i = n1, n2 )
  end do
6004 format ( /, 11h new column, i6, /, ( 1x, 7e17.8 )  )
210 a = x1( 1 )
  b = x1( 8 )
  sf6 = a
  sf7 = b
  !     sum=(a+b) * onehaf
  !     a=a-sum
  !     x1(1)=a
  !     x1( 8 ) = -a
  sum = a
  x1( 8 ) = b - a
  etot = 1.0 / elp( i26+16 )
  a = 1.0 / sum
  b = 1.0 / etot
  sum = ( b + 2.0 * a ) / 3.0
  etot  =  ( b - a ) / 3.0
  elp( i75 ) = sum
  elp(i75+1) = etot
  elp(i75+53) = a * athtw
  elp(i75+54) = b * asqrt3
  if ( iprsup .le. 0 )   go  to  212
  write ( lunit6, 6005 )  elp(i75), elp(i75+1), elp(i75+53), elp(i75+54)
6005 format( 3x, 8e15.7 )
  !     correction terms for voltage and current calculations*************
212 elp( i75+5 ) = x1( 3 )
  elp( i75+6 ) = x1( 4 )
  elp( i75+7 ) = x1( 11 )
  elp( i75+8 ) = x1( 12 )
  elp( i75+9 ) = x1( 15 )
  elp( i75+10 ) = x1( 21 )
  elp( i75+11 ) = x1( 16 )
  elp( i75+12 ) = x1( 22 )
  elp( i75+13 ) = x1( 29 )
  elp( i75+14 ) = x1( 35 )
  elp( i75+15 ) = x1( 30 )
  elp( i75+16 ) = x1( 36 )
  elp( i75+43 ) = x1( 13 )
  elp( i75+44 ) = x1( 19 )
  elp( i75+45 ) = x1( 26 )
  elp( i75+46 ) = x1( 32 )
  elp( i75+47 ) = 1.0 / sf6
  elp( i75+48 ) = elp( i26+18 ) * elp( i75+39 )
  elp( i75+49 ) = sf7
  elp( i75+52 ) = x1( 8 )
  if ( iprsup .lt. 1 )   go to 214
  idp = in
  in = in + 7
  ids =  idt + 3
  write ( lunit6, 6006 )  ilk
6006 format( 5x, 20h arrays for mach no., i5, 17h  in order of ac ,1x, 4ha21,, 1x, 4ha22,, 1x )
  write ( lunit6, 6005 )  elp(i75+2), elp(i75+3), elp(i75+51), elp(i75+52)
  idt = i75 + 5
  ids = idt + 3
  write ( lunit6, 6005 )   ( elp( iu ),  iu = idt, ids )
  ids = ids + 1
  idt = ids + 7
  write ( lunit6, 6005 )   ( elp( iu ),  iu = ids, idt )
  !     calculate the resistive matrix for history calculations
  !     store  constants for future use in the time-step loop ************
214 elp( i75+17 ) = z( 50 ) - elp( i26+19 ) * damrat
  elp( i75+18 ) = z( 13 )
  elp( i75+19 ) = z( 19 )
  elp( i75+20 ) = z( 51 ) - elp( i26+19 ) * damrat
  elp( i75+21 ) = z( 26 )
  elp( i75+22 ) = z( 32 )
  elp( i75+23 ) = z(  3 )
  elp( i75+24 ) = z( 52 ) - elp( i26+6 ) * damrat
  elp( i75+25 ) = z( 21 )
  elp( i75+26 ) = z(  4 )
  elp( i75+27 ) = z( 16 )
  elp( i75+28 ) = z( 53 ) - elp( i26+7 ) * damrat
  elp( i75+29 ) = z( 11 )
  elp( i75+30 ) = z( 54 ) - elp( i26+14 ) * damrat
  elp( i75+31 ) = z( 35 )
  elp( i75+32 ) = z( 12 )
  elp( i75+33 ) = z( 30 )
  elp( i75+34 ) = z( 55 ) - elp( i26+15 ) * damrat
  if ( iprsup .lt. 1 )    go to  216
  write ( lunit6, 6007 )  ilk
6007 format( /, 33h history matrix for generator no.,  i5  )
  in = i75 + 9
  do k = 1, 5
     in = in + 7
     n2 = in + 7
     n1 = in + 1
410  write ( lunit6, 6008 )    in, ( elp( i ), i = n1, n2 )
  end do
6008 format( 1x, 12hnew  row    ,i6, /, (1x, 7e17.8) )
  write ( lunit6, 6009 )
6009 format ( 24h  "exit  module increm." )
216 return
end subroutine increm
!
! subroutine redusm.
!
subroutine redusm(x,m,n)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     this routine is a copy  of an identical routine used in over12****
  !     it uses gauss-jordan elimination process for both matrix inversion
  !     and matrix rduction( elimination of variables)********************
  dimension  x(1), a1(6)
  j = m
  ik = m**2
  nk = ik - m
  m1=m + 1
1 c=1.0 / x( ik )
  do k=1,m
2    a1(k)=x(nk+k)
  end do
  k = 1
4 mk=(k-1)*m
  b=-x(mk+j)*c
  i = 1
3 mk=mk+1
  x(mk)=x(mk)+b*a1(i)
  i = i + 1
  if (i.le.m) go to 3
  x(mk-m+j)=b
  k = k + 1
  if (k.eq.j) k=k+1
  if (k.le.m) go to 4
  do k=1,m
5    x(nk+k)=a1(k)*c
  end do
  x(ik)=c
  j=j-1
  ik = ik - m1
  nk=nk-m
  if (j.gt.n) go to 1
  return
end subroutine redusm
!
! subroutine bansol.
!
subroutine bansol(ab, x, n)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !     this routine performs forward and backward solution with
  !     with a tridiagonal symmetric matrix 'ab'. for detail of
  !     storage arrangements for 'ab' see subroutine 'bandel'.
  dimension ab( 1 ), x( 1 )
  !     initialization **************************************************
  i2 = 2
  i1 = 1
  !     forward substitution ( downwards ) ******************************
  d = x( 1 )
10 x( i1 ) = d * ab( i2-1 )
  if ( i1  .eq.  n )  go to 20
  i1 = i1 + 1
  d = x( i1 )  -  d * ab( i2 )
  i2 = i2 + 2
  go to 10
  !     back subtitution ( upwards ) ************************************
20 if ( i1  .eq.  1 )  go to 40
  i2 = i2 - 2
  i1 = i1 - 1
  x( i1 ) = x( i1 ) - x( i1+1 ) * ab( i2 )
  go to 20
40 return
end subroutine bansol
!
! subroutine redu17.
!
subroutine redu17(a, n, m)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !)    for an explanation about the parameters of this subroutine,
  !)    see comment cards in same module  'reduct'  of overlay 12 .
  dimension  a(1), b(100)
  j = n + 1
  w=1.0
  if (m.gt.0) w=-w
  ij=n*j/2
3 j=j-1
  if (j.eq.m) return
  h1=-1.0/a(ij)
  b(j)=h1
  ij=ij-j
  k=0
  ik=0
  !                                   begin k-loop
4 ik=ik+k
  i1=ik+1
  k=k+1
  if (k.gt.n) go to 3
  if (k.lt.j) go to 9
  if (w.lt.0.) go to 3
  if (k.eq.j) go to 7
  i=ik+j
5 h2=a(i)
  b(k)=h2*h1
  !                                   begin i-loop
  i2=ik+k
  l=0
  do i=i1,i2
     l=l+1
6    a(i)=a(i)+b(l)*h2
  end do
  if (k.lt.j) go to 4
  i=ik+j
  a(i)=b(k)
  go to 4
  !                                   end i-loop
7 i=ij
  do l=1,j
     i=i+1
8    a(i)=b(l)
  end do
  go to 4
  !                                   end k-loop
9 i=ij+k
  go to 5
end subroutine redu17
!
!     subroutine subts3.
!
subroutine subts3
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'umdeck.ftn'
  integer(4) n1
  dimension xx(1),  volta(1)
  dimension nsubkm(1)
  equivalence (kknonl(1), nsubkm(1))
  equivalence (h1, sk), (d2, sm)
  equivalence (xk(1), xx(1)), (volti(1), volta(1))
  equivalence (moncar(2), kbase)
  equivalence (iprsov(36), iupper)
  include 'tacsar.ftn'
  include 'syncom.ftn'
  include 'synmac.ftn'
  dimension vsmout(1)
  equivalence (ismout(1), vsmout(1))
  !     intrinsic  absz, cosz, expz
  ll2 = 2
  ll6 = 6
  ll8 = 8
  ll10 = 10
  if ( peaknd(1)  .eq.  0.0 )   go to 1003
  if ( begmax(maxout)  .gt.  t )   go to 1003
  d1 = absz( peaknd(1) )
  do j=2, ntot
     if ( absz( emtpe(j) )  .le.  d1 )   go to 1002
     peaknd(1) = emtpe(j)
     d1 = absz( emtpe(j) )
     peaknd(2) = t
     peaknd(3) = j
1002 end do
1003 if ( istep  .lt.  limstp )   go to 12
  isprin = 0
  iout = multpr(indstp)
  if (iout .ge. 0) go to 1010
  iout = -iout
  do j=1, 4
     n13 = iprsov(j+15)
     iprsov(j+15) = iprsov(j+30)
1005 iprsov(j+30) = n13
  end do
  iprsup = iprsov(nchain)
1010 indstp = indstp + 1
  limstp = kprchg(indstp)
12 n7 = 0
  if ( t .le. tmax )  go to 14
  n7 = 1
  if ( nenerg  .ne.  0 )   go to 14
  isprin = 0
  isplot = 0
14 if ( begmax(1)  .eq.  1.0 )   go to 13
  if ( koncur .eq. 1 ) go to 13
  if ( npower .gt. 0 )  go to 13
  if ( isprin .le. 0 )  go to 13
  if ( isplot .gt. 0 )  go to 1660
13 if ( nv .eq. 0 )  go to 1202
  do k=1,nv
     n1 = ibrnch(k)
     n2 = jbrnch(k)
1201 bvalue(k)=emtpe(n1)-emtpe(n2)
  end do
1202 volti(1) = t
  if ( ivolt .eq. 1 )  go to 1610
  k = 1
1600 if ( k  .gt.  numnvo )   go to 1630
  i = ibsout(k)
  k = k + 1
  volti(k) = emtpe(i)
  go to 1600
1610 k = ntot
  call mover ( emtpe(ll2), volti(ll2), ntot1 )
1630 if ( nc  .eq.  0 ) go to 1642
  nodev = k
  do i=1,nc
     k=k+1
1640 volti(k)=bvalue(i)
  end do
1642 if (iaverg .eq. 0) go to 1652
  if ( istep  .eq.  0 )   go to 1657
  do i = 2, k
     sdlat = volti(i)
     ndx1 = lsiz26 + i
     volti(i) = onehaf * ( sdlat + volta(ndx1) )
1651 volta(ndx1) = sdlat
  end do
  go to 1652
1657 do i = 2, k
     ndx1 = lsiz26 + i
1658 volti(ndx1) = volti(i)
  end do
1652 if (npower .le. 0) go to 54280
  do i=1, npower
     mpower = maxpe + i
     j = koutvp(i)
     if ( j .gt. 0 )  go to 54262
     j = -j
     n1 = ibrnch(j)
     d4 = emtpe(n1)
     go to 54265
54262 d4 = bvalue(j)
54265 l = koutvp(mpower)
     d1 = d4 * bvalue(l)
     bnrg(i) = bnrg(i) + d1*deltat
     if ( istep .eq. 0 )   bnrg(i) =  delta2  * d1
     d2 = bnrg(i) - d1 * delta2
     n5 = j + nodev
     n6 = l + nodev
     volti(n5) = d1
     volti(n6) = d2
     if ( istep .le. 5    .and.    iprsup .ge. 2 ) write (lunit6, 54270)  i, k,   j  , l, d1, d2, bnrg(i), d4, bvalue+(l)
54270 format( /,  9h at 54270   , 4i10, 5e15.4 )
54272 end do
54280 if ( nsmout  .eq.  0 )   go to 54284
  call mover ( vsmout(msmout+1), volti(k+1), nsmout )
  k = k + nsmout
54284 if ( ioutcs  .eq.  0 )   go to 1650
  do j8 = 1, ioutcs
     k = k + 1
     ndx1 = kxtcs + jout( kjout + j8 )
318  volti(k) = xtcs( ndx1 )
  end do
1650 if ( numout  .eq.  0 )   go to 1643
  call mover ( spum(iuumou), volti(k+1), numout )
  k = k + numout
1643 if ( k .gt. 1 )  go to 1647
  kill = 44
  lstat(19) = 1650
  go to 9200
1647 if ( koncur .eq. 1 ) call spytac
  if ( isplot .gt. 0 )  go to 1653
  if ( m4plot  .eq.  0 )   go to 1648
  call pltfil ( k )
  go to 1649
1648 write (lunit4) (volti(i),i=1,k)
1649 isplot = iplot
1653 if ( isprin .gt. 0 )  go to 1654
  if (kbase .ge. 2)   go to 1654
  n1 = k
  if ( n1 .gt. 10 )  n1 = 10
  if ( istep  .gt.  99999 )   istep = istep - 100000
  if ( t  .ge.  unity )   go to 48
  if ( deltat  .lt.  tenm6 )   go to 48
  write (lunit6, 37)  istep, ( volti(i), i=1, n1 )
37 format ( 1x, i5,  f9.6, 9e13.6 )
  go to 56
48 if ( istep  .le.  9999 )   go to 39
  write (lunit6, 51)  istep,  ( volti(i), i=1, n1 )
51 format ( 1x, i5, e9.3, 9e13.6 )
  go to 56
39 write (lunit6, 40)  istep,  ( volti(i), i=1, n1 )
40 format ( 1x,  i4,  e10.3,  9e13.6  )
56 if ( k  .gt.  n1 ) write (lunit6, 87437)  ( volti(i), i=11, k )
87437 format ( 15x, 9e13.6 )
  isprin = iout
1654 if ( begmax(1) .eq. 0.0 ) go to 1660
  if ( t .lt. begmax(maxout) )  go to 1660
  !     now update extrema vector  "xmax" .   there are 4
  !     partitions of size of list 12:  (xmax, tmax, xmin, tmin).
  do i=2, k
     l = i - 1
     ndx1 = 2 * lsiz12 + l
     if (volti(i)  .ge.  xmax(ndx1) )  go to 4655
     xmax(ndx1) = volti(i)
     ndx1 = ndx1 + lsiz12
     xmax(ndx1) = t
4655 if ( volti(i)  .le.  xmax(l)  )  go to 1655
     xmax(l) = volti(i)
     ndx1 = lsiz12 + l
     xmax(ndx1) = t
1655 end do
  if ( iprsup .ge. 2 ) write (lunit6, 1656)  ( xmax(i), i=1, l )
1656 format( /,  16h xmax(i) at 1656   ,/, ( 1x, 8e15.4 ) )
1660 if ( n7  .eq.  0 ) go to 1661
  if ( memsav  .eq.  0 ) go to 8000
1661 istep = istep + 1
  t = t + deltat
  if ( t .gt. begmax(maxout+1) )   maxout = maxout + 2
  isplot = isplot - 1
  isprin = isprin - 1
  if ( t .lt. sptacs(29) ) go to 1945
  sptacs(29) = fltinf
  iuty(kiuty+3) = iuty(kiuty+2)
1945 if ( iprsup  .ge.  4 ) write (lunit6, 7342)  k, ibr,  ( emtpf(i), i=1, ntot )
7342 format (/, ' k, ibr =', 2i6, 10x, '(emtpf(i), i=1, ntot )  follow ...', /, (1x, 8e16.7))
  !                                 constant voltages and currents
  i=0
  if (iread.eq.0) go to 1247
  !     read input card using cimage.
  call cimage
  if ( nchain  .eq.  16 )   go to 9999
  write (kunit6, 54169)
54169 format( 42h+another input card for type 1-10 sources.  )
  if ( kolbeg  .gt.  0 )   go to 7823
  read (unit = abuff, fmt = 10) (voltbc(j), j = 1, 10)
10 format(10e8.0)
  go to 7825
7823 nfrfld = 10
  call frefld ( voltbc(1) )
7825 if ( voltbc(1)  .ne.  9999. )   go to 11247
  write (kunit6, 2241)
2241 format ( 42h+another input card for type 1-10 sources.,7h   end.    )
  iread=0
1247 call mover0 ( voltbc(1), ll10 )
11247 call interp
  if ( nstacs  .eq.  0 ) go to 11248
  do j = 1, nstacs
     n1 = ichar (vstacs(j)(1 : 1))
     if (n1  .eq.  0) go to 21247
     ndx1 = kxtcs  + n1
     voltbc(j) = xtcs(ndx1)
21247 end do
11248 if ( kanal  .gt.  0 ) call analyt
  if ( kill  .gt.  0 )   go to 9200
1249 i=i+1
  if (i.gt.kconst) go to 1300
  if ( iprsup  .ge.  4 ) write (lunit6, 7356)  i, kconst,  ( emtpf(m), m=1, ntot )
7356 format (/, ' i, kconst =',  2i6, 10x, '(emtpf(i), i=1, ntot )  follow ...', /, (1x, 8e16.7))
  n1=node(i)
  n2=iabs(n1)
  n3=iform(i)
  if ( n3  .ne.  17 )  go to 7362
  n8 = sfreq(i)
  ndx1 = kxtcs  +  n8
  crest(i+1) = crest(i) * xtcs(ndx1)
  go to 1249
7362 if ( n3 .ne. 18 )   go to 7368
  !     following code is for ideal transformer and source:
  n14 = time1(i)
  emtpf(n14) = yx
  go to 1249
7368 if (n3.ne.16) go to 1251
  j=i+1
  n4=iabs(node(j))
  gus1=emtpe(n2)-emtpe(n4)
  n3=iform(j)
  gus2 = time1(j)
  gus3 = time2(j)
  ck1=crest(j+1)-tstop(i)*gus1
  gus4=sfreq(j)
  sfreq(j)=tstart(j)+crest(j)-ck1
  if (sfreq(j).gt.gus3) go to 1256
  if (sfreq(j).lt.gus2) go to 1257
  tstop(j)=(sfreq(j)-gus4)/delta2-tstop(j)
  iform(j)=1
1258 tstart(j)  =  sfreq(i)*ck1  +  time1(i)*sfreq(j)  +  tstart(i)*tstop(j)
  yx = crest(i)+tstart(j)
  emtpf(n4)=emtpf(n4)-yx
  crest(j+1)=yx
  i=j
  go to 1250
1256 sfreq(j)=gus3
  tstop(j)=0.
  iform(j)=2
  go to 1258
1257 sfreq(j)=gus2
  tstop(j)=0.
  iform(j)=3
  go to 1258
1251 yx=0.
  ts=t-tstart(i)
  if (ts.lt.0.) go to 1250
  if (t.ge.tstop(i)) go to 1250
  k=iabs(n3)
  if (k.eq.14) go to 1270
  if (k.eq.15) go to 1286
  if ( k  .lt.  60 )   go to 1387
  n8 = sfreq(i)
  ndx1 = kxtcs  + n8
  yx = xtcs(ndx1)
  go to 1250
1387 yx = crest(i)
  if (k.ge.12) go to 1280
  if (k.lt.11) yx=voltbc(k)
1250 if (n1.gt.0) go to 1260
  emtpf(n2)=emtpf(n2)+yx
  go to 1249
1260 if (n3.lt.0)  yx=yx+emtpe(n2)
  emtpe(n2)=yx
  go to 1249
1270 yx = crest(i) * cosz(sfreq(i) * ts + time1(i))
  if ( iprsup .lt. 3 )  go to 1250
  write (lunit6, 1273)  i, kconst, n1, n2, n3, node(i), iform(i), &
       t, tstart(i), tstop(i), crest(i), sfreq(i), ts, &
       time1(i),yx,emtpf(n2),emtpe(n2)
1273 format( /, 9h at 1273   , 7i12, //, ( 1x, 6e20.8 ) )
  go to 1250
1280 if (ts .ge. time1(i)) go to 1285
  yx = ts / time1(i) * yx
  go to 1250
1285 if (k.eq.12) go to 1250
  yx = yx + sfreq(i) * (ts - time1(i))
  go to 1250
1286 yx = crest(i) * (expz(sfreq(i) * ts) - expz(time1(i) * ts) )
  go to 1250
  !                                         elimination process on right s
  !                                         solve for v = [y] ** -1 * i
1300 call mover ( emtpf(1), emtpe(1), kpartb )
  n1 = 1
1301 emtpe(n1) = 0.0
  if ( kode(n1) .le. n1 )   go to 1302
  n1 = kode(n1)
  go to 1301
1302 do j = 2, kpartb
     if ( kode(j) .eq. 0 ) go to 1303
     k = kode(j)
     if ( k .gt. kpartb )  go to 1303
     if ( k .gt. j ) emtpe(k) = emtpe(k) + emtpe(j)
1303 end do
  ii=1
  if ( iprsup .lt. 4 )   go  to  1410
  write (lunit6, 41300)  ( emtpe(l), l=1, ntot )
41300 format(/, ' emtpe(l), l=1, ntot  at begin repeat soln.', /, (1x, 5e25.15))
  write (lunit6, 51300)  ( l, km(l), ykm(l), l=1, iupper )
51300 format( /,  41h table of factors used.  l, km(l), ykm(l)  ,/, ( 2i10, e30.20 , 5x, 2i10, e30.20 ) )
1410 if ( ii .gt. iupper ) go to 1450
  l=iabs(km(ii))
  a = emtpe(l)
  emtpe(l) = a*ykm(ii)
  j = kk(l)
1420 ii=ii+1
  if ( ii .gt. j ) go to 1410
  k=km(ii)
  if ( k .gt. kpartb )   go to  1420
  emtpe(k) = emtpe(k) - a * ykm(ii)
  go to 1420
1450 if (iprsup .ge. 5) write (lunit6, 5203) (emtpe(k), k = 1, ntot)
5203 format (' After downward,  ( emtpe(k), k = 1, ntot )  follow ...', /, (1x, 8e16.8))
  !                                               backsubstitution
  l = ntot
  go to 5211
1500 if ( ii .eq. 1 ) go to 1550
  a=0.
1510 ii=ii-1
  k=km(ii)
  if (k.lt.0) go to 1520
  a=a-emtpe(k)*ykm(ii)
  go to 1510
1520 l=iabs(k)
  emtpe(l) = emtpe(l) + a
5211 if ( kode(l) .eq. 0 ) go to 5218
  if ( kode(l) .gt. l ) go to 5218
  j = l
  n5 = kode ( j )
5214 emtpe(n5) = emtpe(j)
  j = n5
  n5 = kode(j)
  if ( n5 .ne. l ) go to 5214
5218 if ( l .le. kpartb ) go to 1500
  l = l - 1
  go to 5211
1550 if ( inonl .eq. num99 )  go to 1570
  do i=1,inonl
     if ( nltype(i) .lt. 0 )  go to 1559
     nn15 = nlsub(i)
     n15 = isubeg(nn15)
     if ( n15  .le. 0 )  go to 1559
     if ( nsubkm(n15+4) .eq. 0 ) go to 3534
     if ( nsubkm(n15+3) .ne. i ) go to 1559
     !     perform simultaneous  zno  solution in  "zincox" .
     call zincox ( nn15 )
     if ( kill  .gt.  0 )   go to 9200
     go to 1559
3534 i1=nonlad(i)
     i2=nonle(i)
     k=nonlk(i)
     l=iabs(nonlm(i))
     a2=anonl(i)
     n1=ilast(i)
     vzero2=emtpe(k)-emtpe(l)
     if ( nltype(i)  .eq.  94 )   go to 1521
     i2=iabs(i2)
     vzero2=     vzero2*delta2 +vnonl(i)
1551 vdiff=vzero2-vzero(i)
     vzero(i)=vzero2
     if ( vdiff .lt. 0.0 )  go to 3400
     if ( vdiff .eq. 0.0 )  go to 1555
     vsl = vzero2 + a2*cchar(n1)
     h1=vsl-vchar(n1)
     go to 3800
3100 n1=n1+1
     if (n1.lt.i2) go to 3200
     lstat(19) = 3100
4296 kill = 35
     flstat(13) = a2
     flstat(11) = emtpe(k)
     flstat(12) = emtpe(l)
4297 lstat(16) = i
     k = nonlk(i)
     bus1 = bus(k)
     bus2 = bus(l)
     go to 9200
3200 h1=h2
3800 vsr=vzero2+a2*cchar(n1+1)
     h2=vsr-vchar(n1+1)
     if (h1*h2.gt.0.) go to 3100
3300 delti=absz(h1)*(cchar(n1+1)-cchar(n1))/(absz(h1)+absz(h2))
     curr(i)=cchar(n1)+delti
     go to 1555
3500 n1=n1-1
     if (n1.ge.i1) go to 3475
     lstat(19) = 3500
     go to 4296
3400 vsr=vzero2+a2*cchar(n1+1)
     h2=vsr-vchar(n1+1)
     go to 3450
3475 h2=h1
3450 vsl=vzero2+a2*cchar(n1)
     h1=vsl-vchar(n1)
     if (h1*h2) 3300,3300,3500
1555 ilast(i)=n1
     go to 8259
1521 i1=iabs(i1)
     vzero(i)=vzero2
     if ( iprsup  .ge.  2 ) write (lunit6, 1524)  i, nonle(i), i1, i2, vnonl(i), vchar(i2+5), vzero2
1524 format (/,  32h aep arres. at 1524 of 'subts3'., 4i10, 3e20.6 )
     if ( vchar(i2+5)  .ne.  0.0 )   go to 1561
     if (absz(vnonl(i)).gt.absz(vzero2)) go to 8259
     tz1=t-deltat
     write (lunit6,1562) bus(k),bus(l),tz1
1562 format ( 60x, 11harrester  ', a6, 8h'  to  ', a6, 20h'  flashover at time, e13.5, 6h  sec.   )
1561 vchar(i2) = vzero2
     call arrest ( cchar(i1), vchar(i2), anonl(i), vzero2, curr(i)  )
     if (vchar(i2+5) .ne. 0.0)   go to 8259
     tz1=t-deltat
     write (lunit6,1563) bus(k),bus(l),tz1, vchar(i1+8)
1563 format ( 60x, 11harrester  ', a6, 8h'  to  ', a6, 20h'   cleared  at time, e13.5, 6h  sec.        ,/, &
          70x, 28hper unit energy dissipated =, e14.6     )
     !     next assign compensation current for non-zno  compensation
     !     elements.  this is used in  "subts4"  superposition.
8259 n8 = n15 / 5 + 1
     cursub(n8) = curr(i)
     emtpf(k) = emtpf(k) - curr(i)
     emtpf(l) = emtpf(l) + curr(i)
     if ( iprsup  .ge.  4 ) write (lunit6, 8261)  i, nltype(i), nn15, n8, k, l, curr(i), emtpf(k), emtpf(l)
8261 format (  20h n.l. elem. current.,  6i10, 3e15.6  )
1559 end do
  if ( iprsup .ge. 3 ) write (lunit6, 1571)  ( curr(i), i=1, inonl )
1571 format( /,  35h n.l. element,  curr(i), i=1, inonl   ,   6e15.5 )
1570 lastov = nchain
  nchain = 19
  go to 9999
8000 lastov = nchain
  nchain = 20
  go to 9999
9200 lstat(18) = 18
  lastov = nchain
  nchain = 51
9999 return
end subroutine subts3
!
! subroutine zincox.
!
subroutine zincox ( ns )
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'dekspy.ftn'
  dimension fold( 1 )
  dimension ksing( 1 ), kindep( 1 )
  equivalence ( ksing(1), cchar(1) )
  equivalence ( kindep(1), gslope(1) )
  equivalence ( fold(1), vchar(1) )
  dimension nsubkm( 1 )
  equivalence ( kknonl(1), nsubkm(1) )
  character*8 text1, text2, text3, text4
  !     intrinsic  absz
  data  text1   /  6hspy     /
  data  text2   /  6hsolve   /
  data  text3   /  6hstop    /
  data  text4   /  6hlook    /
  ll10 = 10
  iofznr = nonle( inonl )
  iofznr = iabsz( iofznr )
  d12 = nbyte(3)
  d12 = iofznr * d12 / nbyte(4)
  iofzni = d12 + 1.0
  if ( iprsup  .gt.  0 ) write (lunit6, 918) ns, iofznr, iofzni, lchar
918 format (  19h top of  "zincox" .,  3x, 28h ns, iofznr, iofzni, lchar =,  4i8 )
  n6 = isubeg( ns )
3415 n5 = 1
  n1 = 0
  n8 = n6
  ll1 = 1
  !     determine number of elements in the subnetwork
  n7 = 0
  do j = 1, 99999
     n7 = n7 + 1
     n10 = nsubkm( n8 )
     if ( n10 .le. n8 )   go to 20
10   n8 = n10
  end do
  !     check for table overflow****************************************
20 n18 = n8
  ndx7r = iofznr + n7
  if ( ndx7r .le. lchar )  go to 3422
  write (lunit6, 3416)  n7, iofznr
3416 format ( /,  22h overflow in "zincox".,   i5,40h coupled arresters, if added to iofznr =, i5,19h overflow list 10. )
  kill = 1
  lstat(19) = 3422
  lstat(16) = 10
  go to 4567
3422 if ( n7**2  .lt.  lsiz26 )   go to 3424
  write (lunit6, 3423)  n7
3423 format ( /,  22h overflow in "zincox".,   i5,49h coupled arresters, if squared, overflow list 26. )
  kill = 1
  lstat(19) = 3424
  lstat(16) = 26
  go to 4567
3424 if ( n7*2 .lt. lsiz26 )  go  to  3425
  write ( lunit6, 3418 ) n7
3418 format ( /,  22h overflow in "zincox".,   i5, 49h coupled arresters, if doubled, overflow list 26. )
  kill = 1
  lstat(19) = 3425
  lstat(16) = 26
  go to 4567
3425 ndx1r = iofznr + 1
  ndx1i = iofzni + 1
  ndx7i = iofzni + n7
  !     extract zthevenin from znonl, determine elements connected to
  !     known voltage nodes********************************************
  ndx2 = ndx1i
  m5 = n6
  do l = 1, n7
     n12 = 0
     ksing( ndx2 ) = 0
     !     process a column at a time**********************************
     do n = 1, n7
        k = nsubkm( m5+1 ) + n1
        m = nsubkm( m5+2 ) + n1
        volti(n5) = znonl(k) - znonl(m)
        if ( volti(n5)  .ne.  0.0 )   n12 = 1
        m5 = nsubkm( m5 )
3426    n5 = n5 + 1
     end do
     if ( n12  .eq.  0 )   ksing(ndx2) = -1
     ndx2 = ndx2 + 1
3431 n1 = n1 + ntot
  end do
  n5 = n5 - 1
  if ( iprsup  .ge.  6 ) write (lunit6, 3438) n6,ns,n7,n5, (volti(l), l=1,n5)
3438 format ( /,  34h thevenin matrix. n6, ns, n7, n5 =,  4i8 ,/,  ( 1x,  8e16.7 )  )
  !     extract and store thevenin voltages****************************
  m = 0
  do l = 1, n7
     n10 = nsubkm( m5+1 )
     n11 = nsubkm( m5+2 )
     inl = nsubkm( m5+3 )
     voltk( m+1 ) = emtpe( n10 ) - emtpe( n11 )
     voltk( m+2 ) = vzero( inl )
     vecnl2( inl ) = curr( inl )
     m5 = nsubkm( m5 )
3452 m = m + 2
  end do
  if ( iprsup  .gt.  1 ) write (lunit6, 3613)  ( ksing(l), l=ndx1i, ndx7i )
3613 format ( /,  27h 'ksing'  b4 copy checking.,  20i5  )
22 n15 = n7 -1
  if ( n15  .eq.  0 )   go to 3638
  n10 = 0
  ndx2 = iofzni
  do l=1, n15
     ndx2 = ndx2 + 1
     if ( ksing(ndx2)  .eq.  -1 )   go to 3634
     n11 = n10 + n7
     ndx3 = ndx2 + 1
     do m = ndx3, ndx7i
        if ( ksing(m)  .ne.  0 )   go to 3628
        !     check for identical rows (columns) ***************************
        do n12=1, n7
           n13 = n10 + n12
           n14 = n11 + n12
           if ( volti(n13)  .ne.  volti(n14) )   go to 3628
3621    end do
        ksing(m) = l
3628    n11 = n11 + n7
     end do
3634 n10 = n10 + n7
  end do
3638 if ( iprsup .lt. 6 )  go  to 40
  write (lunit6, 3458)
3458 format( /, 1x, 42h    row  ksing         vthev          vold)
  m2 = -1
  do l = 1, n7
     m2 = m2 + 2
     l1 = iofzni + l
     write ( lunit6, 32 ) l, ksing(l1), voltk(m2), voltk(m2+1)
32   format( (1x, 2i7, 2e14.5) )
35 end do
40 n17 = 0
  do l = ndx1i, ndx7i
     kindep(l) = 0
     if ( ksing(l)  .ne.  0 )   go to 3645
     n17 = n17 + 1
     kindep(l) = n17
3645 end do
  if ( n17  .eq.  0 )   go to 3522
  if ( n17  .eq.  n7 )   go to 3674
  !     collapse zthevenin (remove zero and dependent rows )
  n11 = 1
  n5 = 0
  do l = ndx1i, ndx7i
     do m = ndx1i, ndx7i
        if ( ksing(l)  .ne.  0 )   go to 3657
        if ( ksing(m)  .ne.  0 )   go to 3657
        n5 = n5 + 1
        volt(n5) = volti(n11)
3657    n11 = n11 + 1
     end do
3662 end do
  call mover ( volt(1), volti(1), n5 )
  if ( iprsup  .ge.  5 ) write (lunit6, 3668)  ( volt(l), l=1, n5 )
3668 format (     20h collapsed  (zthev):,  7e15.5  )
3674 continue
  !     enter newton loop for zno arresters (not user fortran):
  call mover0 ( volt(1), n5 )
  ndx17r = iofznr + n17
  ndx17i = iofzni + n17
  n10 = 1
  n12 = n17 + 1
  do l=1, n17
     volt(n10) = 1.0
3465 n10 = n10 + n12
  end do
  if ( kill  .gt.  0 )   go to 4567
  !     calculate explicit inverse of the collapsed zthev ***************
  call dgelg ( volt(1), volti(1), n17, n17, epsiln, ier )
  if ( iprsup  .gt.  5 ) write (lunit6, 3471)  ier,  ( volt(m), m=1, n5 )
3471 format ( /,  28h inverse of  (zthev).  ier =,  i2  ,/,( 1x, 8e16.7 ) )
  niter = 0
  if ( ier  .eq.  0 )   go to 3693
  lstat(19) = 3471
3681 kill = 209
3686 lstat(13) = n7
  lstat(14) = nsubkm( n18+3 )
  lstat(15) = nsubkm( n6+3 )
  lstat(16) = n17
  lstat(17) = niter
  n1 = nsubkm( n6+1 )
  n2 = nsubkm( n6+2 )
  bus1 = bus(n1)
  bus2 = bus(n2)
  flstat(14) = d3
  flstat(15) = t
  iprsup = 9
  if ( m4plot  .ne.  1 )  go  to  3415
  write (munit6, 3688)
3688 format (  37h   ? ? ?   singular jacobian.  newton,19h iteration stopped.    )
  call window
3691 write ( munit6, 3692 )
3692 format(  39h trouble in  "zincox".  kill  lstat(19),27h   ier  niter            d3  )
  call window
  write (munit6, 3696)  kill, lstat(19), ier, niter, d3
3696 format ( 22x, i6, i11, i6, i7, e14.5 )
  call window
  do il = 1, n7
     inl = nsubkm( m5+3 )
     curr( inl ) = vecnl2( inl )
     m5 = nsubkm( m5 )
3702 end do
  call honker( ll10 )
3711 write (prom80, 3712)
3712 format ( ' send remedy (spy, solve, look, stop) :' )
  call prompt
  lockbr = 1
  call flager
  read (buff77, 3719)  bus1
3719 format ( a6 )
  kill = 0
  if (bus1 .ne. text1) go to 3726
  call spying
  go to 3711
3726 if ( bus1 .eq. text2 )  go to 3415
  if ( bus1 .eq. text3 ) call stoptp
  if ( bus1 .ne. text4 ) go to 3752
  write (prom80, 3731)
3731 format ( ' send diagnostic level iprsup (i2 format) :' )
  call prompt
  read (buff77, 3734)  iprsup
3734 format ( i2 )
  go to 3711
3752 go to 3711
3693 d3 = fltinf
  call mover ( volt(1), volti(1), n5 )
3474 niter = niter + 1
  n11 = 1
  call mover0 ( fold(ndx1r), n17 )
  ndx2 = iofzni
  m2 = 0
  do l=1, n7
     ndx2 = ndx2 + 1
     m2 = m2 + 2
     if ( ksing(ndx2)  .eq.  -1 )   go to 3482
     n13 = l
     if ( ksing(ndx2)  .gt.  0 )   n13 = ksing(ndx2)
     ndx3 = n13 + iofzni
     n14 = kindep(ndx3)
     n10 = ( n14 - 1 ) * n17  +  n13
     inl = nsubkm( m5+3 )
     ityp = nsubkm( m5+4 )
     ils = ilast( inl )
     if ( ityp .eq. 1 )  go  to  41
     !     process piecewise linear and time varying resistances************
     if ( ils .gt. 0 )  go to  254
     d4 = voltk( m2 )
     if ( ityp  .eq.  3 )   d4 = t + vnonl( inl )
     d5 = d4
     if ( ityp .eq. 2  .and.  vchar( ichr ) .eq. 0.0 )   d5 = absz(d5)
     ichr = nonlad( inl )
     ils = -ils
     if ( d5 .gt. vchar( ils ) )  go to  251
     ibk = ils
     do jb = ichr, ils
        if ( d5 .lt. vchar( ibk ) ) go to 250
        ik = ibk
        go to 253
250     ibk = ibk - 1
     end do
     ik = ichr
     go to 253
251  ibk = nonle( inl )
     do jb = ils, ibk
        if ( d5 .gt. vchar( jb ) )  go to 252
        ik = jb - 1
        go to 253
252  end do
     ik = ibk
     !     segment has been determined**************************************
253  ilast( inl ) = -ik
     azm = gslope( ik )
     d11 = azm * d5 + cchar( ik )
     if ( ( d5 * d4 ) .lt. 0.0 )    d11 = -d11
     if ( ityp  .eq.  2 )  go  to  261
     if ( d11  .eq.  0.0 )  d11 = epsiln
     azm = 1.0 / d11
     d11 = azm * voltk( m2 )
261  curr( inl ) = d11
     volt( n10 ) = volt( n10 ) - azm
     go to 68
254  d11 = 0.0
     curr( inl ) = 0.0
     go to 68
     !     process zno arresters********************************************
41   d5 = absz( voltk( m2 ) / anonl( inl ) )
     if ( ils .lt. 0 )  go to 45
     !     set limits for scanning of segment boundaries
     ist = nonlad( inl )
     ind =  ils
     go to 55
45   ist = - ils + 1
     ind = nonle( inl )
     !     start scan of segment boundaries*********************************
55   ist = ist + 1
     do jb = ist, ind
        if ( vchar( jb )  .lt.  d5 )  go  to  60
        ik = jb - 1
        go to 65
60   end do
     ik = ind
     !     segment has been determined**************************************
65   azm = cchar( ik )
     if ( ik  .ge.  ist )   go  to  66
     d11 = azm * voltk( m2 )
     curr( inl ) = d11
     volt( n10 ) = volt( n10 ) - azm
     go to 68
66   azx = gslope( ik )
     d11 = azm * ( d5**azx )
     if ( voltk( m2 ) .lt. 0.0 )  d11 = -d11
     curr( inl ) = d11
     volt( n10 ) = volt( n10 ) - azx * d11 / voltk( m2 )
68   ndx4 = iofznr + n14
     if ( ksing(ndx2) .ne. 0 ) go to 75
     m22 = 0
     do m = ndx1i, ndx7i
        if ( ksing( m ) .ne. 0 ) go to 70
        fold(ndx4) = fold(ndx4) - volti(n11) * ( voltk( m22+2 ) - voltk( m22+1) )
        n11 = n11 + 1
70      m22 = m22 + 2
     end do
75   fold( ndx4 ) = fold( ndx4 ) + d11
3482 m5 = nsubkm( m5 )
  end do
  if ( d3 .le. epszno )  go to  3522
  if ( iprsup  .lt.  7 )  go to 3499
  write (lunit6, 3486)  ( fold(l), l=ndx1r,ndx17r )
3486 format ( /,  21h rhs vector  'fold' .,   7e15.6 )
  write (lunit6, 3494)  ( volt(l), l=1, n5 )
3494 format (     19h jacobian  'volt' .,   7e15.6 )
3499 call dgelg ( fold(ndx1r), volt(1), n17, ll1, epsiln, ier )
  if ( ier  .eq.  0 )   go to 3501
  lstat(19) = 3501
  go to 3681
3501 if ( iprsup  .gt.  4 ) write (lunit6, 3502)  ( fold(l), l=ndx1r,ndx17r )
3502 format (     16h dv correction :,  7e15.6 )
  d3 = 0.0
  !     check for too large voltage corrections*************************
  do jb = ndx1i, ndx7i
     if ( ksing(jb)  .ne.  0 )   go to 80
     n13 = kindep( jb ) + iofznr
     il1 = nsubkm( m5+3 )
     d4 = absz( fold( n13 ) / anonl( il1 ) )
     if ( d4 .gt. d3 )  d3 = d4
80   m5 = nsubkm( m5 )
  end do
  !     scale corrections, if too large**********************************
  if ( d3 .le. znolim(1) )  go to 90
  amult = znolim(1) / d3
  do jb = ndx1r, ndx17r
     fold( jb ) = fold( jb ) * amult
85 end do
90 if ( niter .lt. 4 )  go to 92
  !     check for oscillatory solution***********************************
  d35 = absz( d3 - d35 ) / d35
  if ( d35 .gt. ( 10.0*epsiln ) )    go to 92
  amult = 0.1
  do jb = ndx1r, ndx17r
     fold( jb ) = fold( jb ) * amult
91 end do
  !     update voltages( add scaled corrections)*************************
92 m2 = 0
  d4 = znolim( 2 )
  do l = ndx1i, ndx7i
     m2 = m2 + 2
     inl = nsubkm( m5+3 )
     if ( ksing(l)  .eq.  -1 )   go to 100
     if ( ksing(l)  .eq.  0 )   go to 95
     n12 = ksing(l) * 2
     voltk( m2 ) = voltk( n12 )
     go to 100
95   n13 = kindep( l ) + iofznr
     voltk( m2 ) = voltk( m2 ) + fold( n13 )
     if ( nsubkm( m5+4 )  .gt.  1 )  go to 100
     d6 = voltk( m2 )
     d5 = d4 * anonl( inl )
     if ( d5 .gt. absz( d6 ) )  go to 100
     voltk( m2 ) = d5
     if ( d6 .lt. 0.0 )  voltk( m2 ) = -d5
100  m5 = nsubkm( m5 )
  end do
  n4 = nsubkm( n6+3 )
  if ( iprsup  .ge.  3 ) write (lunit6, 3511)  n4, niter, d3
3511 format (24h subsystems 1st arrester, i3, 5x, 7hniter =, i3, 5x,  11hmax del-v =,  e13.4  )
  call mover ( volti(1), volt(1), n5 )
  d35 = d3
  if ( niter  .le.  maxzno )   go to 3474
  if ( d3  .le.  epwarn )   go to 3522
  write (lunit6, 3517)  ns, d3
3517 format (   12h subsystem =,  i3, 47h  iteration limit.   largest abs(dv / znvref) =, e12.3 )
  if ( d3  .le.  epstop )   go to 3522
  kill = 212
  lstat(19) = 3522
  if ( m4plot .ne. 1 )  go to 3686
  write (munit6, 3519)
3519 format (  42h   ? ? ?   non-converged newton iteration., 18h   limit  maxzno =,  i5,  10h  reached.  )
  call window
  go to 3691
3522 ndx2 = iofzni
  m2 = 0
  do l = 1, n7
     ndx2 = ndx2 + 1
     m2 = m2 + 2
     inl = nsubkm( m5+3 )
     ityp = nsubkm( m5+4 )
     ils = ilast( inl )
     znvref = 1.0 / anonl( inl )
     if ( ksing(ndx2)  .ne.  -1 )   go to 180
     d4 = voltk( m2-1 )
     if ( ityp .eq. 1 )  go to 139
     !     process piecewise linear and time varying  resistances**********
     d11 = 0.0
     if ( ils .gt. 0 )  go to 168
     ichr = nonlad( inl )
     ils = -ils
     if ( ityp  .eq.  3 )   d4 = t + vnonl( inl )
     d3 = d4
     if ( ityp .eq. 2  .and.  vchar( ichr ) .eq. 0.0 )  d3 = absz(d3)
     if ( d3 .gt. vchar( ils ) )  go to 351
     ibk = ils
     do jb = ichr, ils
        if ( d3 .lt. vchar( ibk ) )  go to 350
        ik = ibk
        go to 353
350     ibk = ibk - 1
     end do
     ik = ibk
     go to 353
351  ibk = nonle( inl )
     do jb = ils, ibk
        if ( d3 .gt. vchar( jb ) )  go to 352
        ik = jb - 1
        go to 353
352  end do
     ik = ibk
     !     segment has been determined*************************************
353  ilast( inl ) = -ik
     ils = -ik
     azm = gslope( ik )
     d11 = azm * d3 + cchar( ik )
     if ( ( d3 * d4 ) .lt. 0.0 )  d11 = -d11
     if ( ityp  .eq.  2 )  go to 168
     d4 = voltk( m2-1 )
     if ( d11  .eq.  0.0 )  d11 = epsiln
     d11 = ( 1.0 / d11 ) * d4
     go to 168
     !     process zno arresters********************************************
139  d5 = absz( d4 * znvref )
     if ( ils .lt. 0 )  go to 145
     !     set limits for scanning of segment boundaries
     ist = nonlad( inl )
     ind =  ils
     go to 155
145  ist = -ils + 1
150  ind = nonle( inl )
     !     start scan of segment boundaries*********************************
155  ist = ist + 1
     do jb = ist, ind
        if ( vchar( jb )  .lt.  d5 )  go  to  160
        ik = jb - 1
        go to 165
160  end do
     ik = ind
     !     segment has been determined**************************************
165  azm = cchar( ik )
     if ( ik  .ge.  ist )  go  to  166
     d11 = azm * d4
     go to 168
166  azx = gslope( ik )
     d11 = azm * ( d5**azx )
     if ( d4 .lt. 0.0 )  d11 = -d11
168  voltk( m2 ) = d4
     curr( inl ) = d11
     !     determine gap status for next time step**************************
180  d6 = vnonl( inl )
     if ( d6 .eq. fltinf )  go  to  196
     if ( ils .lt. 0 )  go to 190
     d5 = absz( voltk( m2 ) )
     if ( d5 .gt. d6 )  ils = -ils
     if ( ityp .eq. 3  .and. ils .lt. 0 ) vnonl( inl ) = -( t + deltat )
     go  to  195
190  if ( ityp  .eq.  3 )  go to 195
     if ( curr(inl)*vecnl2(inl) .lt. 0.0 )   ils = -ils
     if ( ityp .eq. 1  .or.  ils .lt. 0 )  go  to  195
     gap = vecnl1( inl )
     if ( gap .eq. 0.0 )  go to  195
     if ( gap  .lt.  0.0 )  ils = -ils
     if ( gap  .gt.  0.0 )  vnonl( inl ) = fltinf
195  ilast( inl ) = ils
196  vzero( inl ) = voltk( m2 )
     n13 = m5 / 5
     cursub( n13+1 ) = curr( inl )
     k1 = nsubkm( m5 + 1 )
     k2 = nsubkm( m5 + 2 )
     emtpf(k1) = emtpf(k1) - curr( inl )
     emtpf(k2) = emtpf(k2) + curr( inl )
200  m5 = nsubkm( m5 )
  end do
4567 if ( iprsup  .ge.  1 ) write ( lunit6, 4568 )  kill, lstat(19)
4568 format ( 41h exit module "zincox".  kill, lstat(19) =, 2i8 )
9999 return
end subroutine zincox
!
! subroutine analyt.
!
subroutine  analyt
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !)    this module is called by subroutine  'subts3'  of overlay 16 if
  !)    data case being solved uses one or more sources of type 1 through
  !)    10, and also has a special   'analytic sources'   request card
  !)    which precedes the miscellaneous data cards.   if this be the
  !)    case, the user must replace this dummy module with one which puts
  !)    one or more source values in vector  'voltbc'  at each time step.
  !     relevant variables to use in this definition process are .....
  !          t     ----- time in seconds for which source values must be
  !                      assigned.
  !          istep ----- the time-step number.
  !          deltat ---- the time-step size.   a constraint among these
  !                      three variables is    t = istep*deltat  .
  !          voltbc  --- vector of source values.   for the source having
  !                      type-code  'l' ,   voltbc(l)  must be assigned
  !                      to equal the desired source value at time  't' .
  !     since the present dummy module had better never be called, the
  !     following statements will kill the run with an appropriate error
  !     message.   the one exception is for utpf test case number 14,
  !     which has   ntot = 5  ,   ibr = 5  ,    and   deltat = 0.1   .
  !       preceding comments applied before the spring of 1981,
  !       when interactive control was devised.  ramping of common
  !       variables is done here (remote control from "emtspy").
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'dekspy.ftn'
  !     intrinsic  absz
  data  n16  / 0 /
1637 if ( iprsup  .ge.  1 ) write (lunit6, 1641)  numrmp, ntot, tminrp, tmaxrp
1641 format ( ' top of "analyt".  numrmp, ntot, tminrp, tmaxrp =', 2i8,  2e15.6 )
  if ( numrmp  .le.  0 )   go to 1847
  if ( t .lt. tminrp )  go to 1830
  if ( t .gt. tmaxrp + deltat )  go to 1830
  do j=1, numrmp
     if ( t .lt. tbegrp(j) )  go to 1762
     if ( t .gt. tendrp(j) + deltat )  go to 1762
     if ( kyramp(j) .ne. 1 )  go to 1656
     kserlc = 1
     n17 = indxrp(j)
     nr(n17) =1
     if ( iprsup .lt. 2 )  go to 1675
     write (munit6, 1674)  j, n17
1674 format ( ' "analyt", r-l-c ramp.  j, n17 =', 2i5 )
     call window
1675 d6 = rampcn(j) + rampsl(j) * t
     if ( t .lt. tendrp(j)-deltat ) go to 1758
     d6 = fendrp(j)
     go to 1758
1656 d6 = rampcn(j) + rampsl(j) * t
     if ( t .gt. tendrp(j) )  d6 = fendrp(j)
1758 n4 = memrmp(j)
     if ( n10rmp(j) .ne. 0 )  go to 1748
     fkar1(n4) = d6
     go to 1762
1748 fkar2(n4) = d6
     if ( iprspy  .ge.  1 ) write (lunit6, 1769) j, n4, t, d6
1769 format ( ' done next ramp in "analyt".  j, n4, t, d6 =',2i8,  2e15.6  )
1762 end do
1830 if ( iprspy  .ge.  1 ) write (lunit6, 1833)  t, tminrp, tmaxrp
1833 format ( ' exit "analyt".  t, tminrp, tmaxrp =', 3e15.6  )
  go to 9000
1847 if ( kanal  .ne.  2 )  go to 1942
  if ( n16  .eq.  5678 ) go to 9000
  write ( munit6, 1854 )
1854 format ( '   ###  warning.   [y] is being re-formed', ' each deltat, but there is no ramping.'   )
  call window
  write (prom80, 1855)
1855 format (  '                   is it ok to continue', ' (one and only chance if "y")? :'  )
  call prompt
  read ( munit5, 1863 )  buff77(1:1)
1863 format ( a1 )
  if ( buff77(1:1)  .ne.  'y' )  go to 1868
  n16 = 5678
  go to 9000
1868 lockbr = 1
  call spying
  go to 1637
1942 if (kanal .eq. 0) go to 9000
  if ( ntot  .ne. 5 )   go to 8613
  if ( ibr  .ne.  5 )   go to 8613
  d1 = (deltat - 1.0/10. ) * 100.
  if ( absz(d1)  .gt.  tenm3 )   go to 8613
  if ( voltbc(2)  .ne.  0.0 )   go to 2416
  d1 = t - 0.25
  voltbc(2)  =  100. - 100.*d1
  if ( t  .gt.  0.75 )   voltbc(2) = 0.0
2416 return
8613 kill = 101
  lstat(19) = 8613
9000 if ( iprsup  .ge.  1 ) write (lunit6, 9013)  ntot, ibr, numrmp, kill
9013 format ( ' exit "analyt".  ntot, ibr, numrmp, kill =', 4i8  )
  return
end subroutine analyt
!
! subroutine arrest.
!
subroutine  arrest(a, b, srt, svt, carst)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  !   number   variable                description
  !    a(1)       k        valve block resistance constant
  !    a(2)       b        valve block current exponent
  !    a(3)       l        valve vlock inductance
  !    a(4)
  !
  !    a(5)       k!       gap capacitance constant           t0-t1
  !    a(6)       i!       gap current limit for capacitance  t0-t1
  !    a(7)       e1       gap transition voltage  (1 to 2)   t0-t1
  !    a(8)       kr       gap                                t1-t4
  !    a(9)       k0       gap                                t1-t4
  !    a(10)      k1       gap                                t1-t4
  !    a(11)      k2       gap                                t1-t3
  !    a(12)      w2       gap transition energy  (2 to 3)    t1-t2
  !    a(13)      kd       gap                                t2-t3
  !    a(14)      w0       gap                                t2-t3
  !    a(15)      w1       gap                                t2-t3
  !    a(16)      k0p      gap                                t3-t4
  !    a(17)      k3       gap                                t3-t4
  !    a(18)      i3       gap transition current  (3 to 4)   t0-t4
  !
  !    b(1)       va       arrester voltage
  !    b(2)       vg       gap voltage
  !    b(3)       ia       arrester current
  !    b(4)       ia       arrester current (t-dt)
  !    b(5)       wgap     energy in gap from t1
  !    b(6)       xk       region code
  !    b(7)       m        1/c integral of region 1
  !    b(8)       vb       block voltage
  !    b(9)       wt       accumulated energy since flashover
  !    b(10)      fv       voltage division factor
  !    b(11)      fi       current division factor
  include 'blkcom.ftn'
  dimension  a(20), b(20)
  ll8 = 8
  if (iprsup .ge. 2) write (lunit6, 20)  (a(i), b(i), i=1, 20)
20 format( (1x, 8e16.6) )
  if (b(6) .gt.0) go to 100
  call mover0(b(2), ll8)
  b(6)=1.0
100 continue
  !  test polarity of arrester voltage
  isign=1
  b(1)=b(1)/b(10)
  b(3)=b(3)/b(11)
  if (b(1) .gt.0.0) go to 1
  b(1) = -b(1)
  if ( b(3) .lt. 0.0 )  b(3) = -b(3)
  isign=-1
1 continue
  !  construct valve block branches
  ylb = delta2 / a(3)
  cb=b(3) +ylb*b(8)
  rb=0.0
  if (b(3) .gt. epsiln) rb=a(2)*a(1)*b(3) **(a(2)-1.0)
  be=(1.0-a(2))*a(1)*b(3) **a(2)
  !  test gap region transition
  d1=b(6)
  if (d1.ge.3.5) go to 50
47 if (b(3) -b(4) .ge.0.0) go to 44
  if (b(3) .gt.a(18)) go to 44
  d1=4.0
  go to 50
44 if (d1.gt.1.5) go to 45
  if (b(2) .lt.a(7)) go to 50
  d1=2.0
  go to 46
45 if (d1.gt.2.5) go to 50
46 if (b(5) .lt.a(12)) go to 50
  d1=3.0
50 b(6)=d1
  if ( iprsup  .ge.  2 ) write (lunit6, 5641)  isign, d1, srt, svt, carst, ( a(i), b(i), i=1, 20 )
5641 format ( /,  23h in  'arrest' .   isign, 13x, 2hd1, 10x, 5hanonl, 9x, 6hvzero2, 11x, 4hcurr,  /, &
       15x, i8, 4e15.6, /,  (1x, 8e16.6)  )
  !  select region
  if (d1.lt.1.5) go to 200
  if (d1.lt.2.5) go to 220
  if (d1.lt.3.5) go to 240
  go to 260
  !  region (1) --- t0 to t1
200 continue
  f0=b(7)*b(3)
  dfdv=0.0
  dfdi=b(7)
  go to 99
  !  region (2) branches --- t1 to t2
220 continue
  f1=a(8)*(a(9)-b(2))
  f2=a(10)+a(11)*b(3)
  f0=f1/f2
  dfdv=-a(8)/f2
  dfdi=-a(11)*f1/f2**2
  go to 99
  !  region (3) branches --- t2 to t3
240 continue
  wx=a(14)+a(15)*b(5)
  f1=a(8)*(a(9)-b(2))
  f2=a(10)+a(11)*b(3)
  f0=f1/f2+wx*(a(13)-b(2))
  dfdv=-a(8)/f2-wx
  dfdi=a(11)*f1/f2**2
  go to 99
  !  region (4) branches --- t3 to t4
260 continue
  f1=a(8)*(a(16)-b(2))
  f2=a(10)+a(17)*b(3)**2
  f0=f1/f2
  dfdv=-a(8)/f2
  dfdi=-2.0*a(17)*f1*b(3)/f2**2
  go to 99
99 continue
  if (dfdi.eq.0) dfdi= epsiln
  gi=b(3) +(dfdv*b(2) -f0)/dfdi
  yg=(-dfdv+2.0/deltat)/dfdi
  cg=b(2) *(-dfdv-2.0/deltat)/dfdi-b(3) +gi
  art=1.0/ylb+rb+1.0/yg
  vblock=-cb/ylb
  vgap=-(cg+gi)/yg
  avt=vblock+be+vgap
  if (isign.lt.0) svt=-svt
  !  correct arrester internal node voltages
  carst=(svt-avt*b(10))/(art*b(10)/b(11)-srt)
  curr=carst/b(11)
  if (carst.le.0.0) go to 93
  vgap=vgap+curr/yg
  if (b(6) .gt.1.5) go to 91
  cip= (curr+b(3)) * onehaf
  if (curr.gt.a(6)) cip=a(6)
  b(7) =b(7) +a(5)*cip*deltat
  go to 92
91 if (b(6) .gt.3.5) go to 92
  b(5)=b(5)+ (vgap*curr+b(2)*b(3)) * delta2
92 b(4)=b(3)
  b(3)=carst
  b(2)=vgap
  b(1)=b(10)*(avt+art*curr)
  b(8)=vblock+curr/ylb
  if (isign.gt.0) go to 94
  b(3)=-b(3)
  b(1)=-b(1)
  carst=-carst
94 b(9) = b(9) + b(1) * b(3) * deltat
  go to 5681
93 b(6)=0.0
  b(3)=0.0
  b(1)=0.0
  carst=0.0
5681 if ( iprsup  .ge.  2 )  write (lunit6, 5684)  art, avt, carst,  ( a(i), b(i), i=1, 20 )
5684 format ( /, 19h at end  'arrest' ., 12x, 3hart, 12x, 3havt, 10x, 5hcarst, /, 19x, 3e15.6, /,  (1x, 8e16.6)  )
  return
end subroutine arrest
!
! subroutine solvnum.
!
subroutine solvum(reacl, gpar, fpar, hist,umcurp, nodvo1, nodvo2, jcltac, &
     jclout,jtype,nodom, jtmtac, histom, omegm, omold, thetam, &
     reamdu, reamds, flxds, flxdr, reamqu, flxqs, flxqr, &
     jcdsat, jcqsat, flxd, flxq, nppair, rotmom, ncld, &
     nclq, jtqout, jomout, jthout, reamqs, epsom, dcoef, &
     kcoil, voltum, anglum, nodfum, nodmum, kumout, jumout, umoutp)
  implicit real(8) (a-h, o-z), integer(4) (i-n)
  dimension  reacl(1), gpar(1), fpar(1), hist(1), umcurp(1)
  dimension  nodvo1(1), nodvo2(1), jcltac(1), jclout(1)
  dimension  jtype(1), nodom(1), jtmtac(1), histom(1)
  dimension  omegm(1), omold(1), thetam(1)
  dimension  reamdu(1), reamds(1), flxds(1), flxdr(1)
  dimension  reamqu(1), flxqs(1), flxqr(1)
  dimension  jcdsat(1), jcqsat(1), flxd(1), flxq(1)
  dimension  nppair(1), rotmom(1), ncld(1)
  dimension  nclq(1), jtqout(1), jomout(1), jthout(1)
  dimension  reamqs(1), epsom(1), dcoef(1), kcoil(1)
  dimension voltum(1), anglum(1), nodfum(1), nodmum(1)
  dimension kumout(1), jumout(1), umoutp(1)
  dimension zths3(3,3), smat(3,3), zmat(3,3)
  include 'blkcom.ftn'
  include 'labcom.ftn'
  include 'tacsar.ftn'
  include 'syncom.ftn'
  include 'umdeck.ftn'
  !   jcltac(kcl and kcl+1) are initialized in "umrenu"
  !   jcltac(kcl+2) is initialized in "uminit".
  !   at every time-step t solvum may be called three times (at
  !   time = 0.0 there is an additional call in over16 to
  !   calculate um history arrays and predict values of the
  !   um power currents if case of no compensation).
  !   the sequence to these calls at a certain time t is as :
  !   * call 1 : solvum time-step calculations (call in subts4).
  !        input condition : if no tclose reading, loopss(1)=7766
  !                          if tclose reading , loopss(1) = 6644
  !                          if no power comp,  loopss(8)=1
  !        at completion : if tclose reading, loopss(1)=6644
  !                        if no power comp,  loopss(8)=3
  !             note : at t=0.0, call to tclose reading is skipped
  !             and at completion : loopss(1)=7766, loopss(8)=3
  !   * call 2 : if tacs transfer with sm type-59 data is desired,
  !              then the switch current is to be read from tclose
  !              (call is in subts1, first call to solvum)
  !        input condition : loopss(1)=6644
  !        at completion : loopss(1)=7766
  !   * call 3 : if the power circuits of all um is requested to
  !              be non-compensated (to facilitate multi-mach).
  !              this call is to inject the f-array with the
  !              predicted um power voltages for calculations of
  !              next time-step.
  !              (call is in subts1, second call to solvum)
  !        input condition : loopss(8)=3
  !        at completion : loopss(1) unchanged
  !                        loopss(8)=1
  !   now we are ready for the next call 1.
  !     intrinsic  cosz, sinz, sqrtz
  if (iprsup .ge. 1) write (lunit6,7240) kconst,ibr
7240 format( 24h  "begin module solvum.",8x,8hkconst =,i4, 8x,5hibr =,i4)
  if ( iprsup  .ge.  1 ) write (lunit6, 7246)  istart,loopss(1),loopss(8),ncltot, numum,numout,t
7246 format ( /,  19h top of  "solvum" ., 48h  istart loopss1 loopss8  ncltot   numum  numout, &
       13x, 1ht ,/, 19x, 6i8,e14.5)
  if (iprsup .ge. 1 .and. istart .eq. 0) write (lunit6,7248) (hist(n1),n1=1,ncltot)
7248 format(/,17h hist(1:ncltot) =,(1x,6e14.5) )
  !.  mass angle and speed history for type-59 data input *******
  if (istart .gt. 0) go to 17040
  if (nsmach .eq. 0) go to 70
  if (umoutp(numout+1) .ne. -9999.0) go to 70
  n5 = umoutp(numout+3)
  if (iprsup .ge. 1) write (lunit6,7250) numout,n5
7250 format(/, 9h numout =,i4,10x,17hnumout+nsmtac+3 =,i4)
  if (iprsup .ge. 1) write (lunit6,7252) (umoutp(n1),n1=1,n5)
7252 format( 46h tacs interface requested for sm-59 data input, 47h. the um output table umoutp(numout+nsmtac+3) :, &
       /,6(6x,e14.5)/(6(6x,e14.5)) )
  d1 = umoutp(numout+2)
  if (d1 .eq. 0.0) go to 17042
  jm = 0
  !  jm is counter of machines with sm type-50 data input
  n5 = numout + 4
  !   n5 is first entry of umoutp used for tacs transfer purpose
  n6 = umoutp(numout+3) - 2
  !   umoutp(numout+3) is last entry of umoutp used for tacs transf
  n6 = n6 - 2
17000 if (n5 .gt. n6) go to 17042
  if (umoutp(n5) .le. -300.0) go to 17010
  n5 = n5 + 1
  go to 17000
17010 jm = jm + 1
  n7 = umoutp(n5+4)
  !  n7 is total nr of masses for machine jm
  n8 = umoutp(n5+3)
  !  n8 is total nr of umoutp entries needed for mass history
  !    calculations of machine jm.
  kswsta = - umoutp(n5) - 300
  d5 = thetam(jm) + twopi/(4.0*nppair(jm))
  !  history of mass nr 1 of machine jm :
  umoutp(n5+3) = 0.0
  if (n7 .eq. 1) umoutp(n5+3) = d5
  umoutp(n5+4) = emtpe(n5+1)
  if (n7 .eq. 1) go to 17034
  !  history of mass nr 2 and higher for machine jm :
  do n15 = 2,n7
     n19 = n5 + (n15 - 1)*5
     !  n19 is a pointer to umoutp for mass nr n15
     n12 = umoutp(n19+1)
     !  n12 is node nr of mass nr n15
     n18 = n5 + (n15 - 2)*5
     !  n18 is a pointer to umoutp for mass nr (n15-1)
     n11 = umoutp(n18+1)
     !  n11 is node nr of mass nr (n15-1)
     n20 = kswsta + n15 - 1
     !  n20 is switch nr of switch from mass nr (n15-1) to (n15)
     !  umoutp(n19+3) was lmut of mass nr (n15-1) and (n15)
     !  umoutp(n19+4) was lmut/rmut of mass nr (n15-1) and (n15)
     umoutp(n19+3) = umoutp(n18+3) + umoutp(n19+3)*tclose(n20)
     d1 = emtpe(n11) - emtpe(n12)
     umoutp(n19+3) = umoutp(n19+3) + umoutp(n19+4)*d1
     !  now umoutp(n19+3) has been set to history of mass angle.
     if (n12 .ne. nodom(jm)) go to 17015
     n14 = n15
     d3 = umoutp(n19+3) - d5
     !  n14 is the generator mass of machine jm :
17015 umoutp(n19+4) = emtpe(n12)
     !  now umoutp(n19+4) has been set to history of mass speed.
17020 end do
  !  shift of angles to have gen angle = d5 = thetam(jm)+twopi/nppair:
  do n15 = 1,n7
     n19 = n5 + (n15-1)*5
     umoutp(n19+3) = umoutp(n19+3) - d3
     if (n15 .eq. n14) umoutp(n19+3) = d5
17030 end do
17034 n5 = n5 + n8
  go to 17000
  !. tacs transfer in case of sm type-59 data input**************
17040 if (loopss(1) .ne. 6644) go to 70
  !  loopss(1) = 6644, means that one of the um has sm type-59
  !    data input (intelligence in mach do loop) and that solvum
  !    has to be called for the second time at each time step
  !    to process the switch currents for passing torques, field
  !    variables and mass speeds and angles to tacs
  loopss(1) = 7766
  n1 = umoutp(numout+3)
  if (iprsup .ge. 1) write (lunit6,7250) numout,n1
  if (iprsup .ge. 1) write (lunit6,7252) (umoutp(n2),n2=1,n1)
  !  n5 is first entry of umoutp used for tacs transfer request:
17042 n5 = numout + 4
  !  umoutp(numout+3) is last entry of umoutp used for tacs transf
  n6 = umoutp(numout+3) - 2
17050 if (istart .ne. 0) go to 17060
  if (n5 .gt. n6) go to 70
  go to 17070
17060 if (n5 .gt. n6) go to 21000
17070 n7 = umoutp(n5+1)
  !  n7 is the node nr or the switch nr
  n8 = umoutp(n5+2)
  !  n8 is the entry of the tacs table etac
  n10 = umoutp(n5)
  !  n10 is the code type for the variables to be transfered
  !  transfer of shaft torques :
  if (n10 .ne. -1) go to 17100
  etac(n8) = - tclose(n7)
  n5 = n5 + 3
  go to 17050
  !  transfer of mass speeds :
17100 if (n10 .ne. -2) go to 17110
  etac(n8) = emtpe(n7)
  n5 = n5 + 3
  go to 17050
  !  transfer of mass angles :
17110 if (n10 .le. -300) go to 17112
  if (n10 .ne. -3) go to 17116
17112 if (n8 .ne. 0) go to 17114
  n5 = n5 + 5
  go to 17050
17114 d1 = deltat/2.0
  etac(n8) = umoutp(n5+3) + (umoutp(n5+4) * emtpe(n7))*d1
  umoutp(n5+3) = etac(n8)
  umoutp(n5+4) = d1 * emtpe(n7)
  n5 = n5 + 5
  go to 17050
17116 if (n10 .ne. -299) go to 17120
  n5 = n5 + 5
  go to 17050
  !  transfer of exciter current
17120 if (n10 .ne. -4) go to 17130
  etac(n8) = - tclose(n7)
  n5 = n5 + 3
  go to 17050
  !  transfer of exciter voltage
17130 if (n10 .ne. -5) go to 17140
  etac(n8) = emtpe(n7)
  n5 = n5 + 3
  go to 17050
17140 write (lunit6,17142)
17142 format(/ , 35h error stop.   something wrong with, 37h sm type-59 transfer to tacs. consult,/,      17h emtp management. )
  call stoptp
  !.dynamic (start of time-step loop) ****************************
70 if (loopss(8) .eq. 3) go to 1000
  !     loopss(8) = 3, means that f-array is to be injected
  !       in subts1 in case of no compensation for power circuits.
  !. intialization of first element of cursub, i.e. first
  !  used coil of first used machine.
  nclcom = isubeg(ksubum)
  !. program stop if speed convergence failed in previous
  !. time step.
79 if (inpu .ne. 50) go to 90
  write (lunit6,80)
80 format(43h0program stop because of um nonconvergence., 24h  possible reasons are :)
  write (lunit6,82)
82 format(21h * time step too high)
  write (lunit6,83)
83 format(35h * convergence margin epsom too low)
  write (lunit6,84)
84 format(25h * error in um data input)
  call stoptp
90 seltat = deltat
  !. start of first machine do-loop:
1000 nxout = 0
  mshare = 0
  do jm = 1,numum
     !  adjusting omegrf to rotmom(jm) if mech network option used:
     omegrf = twopi * statfr
     if (jtmtac(jm) .ne. 0) go to 1004
     omegrf = rotmom(jm)
1004 tau = omegrf * t
     !  injection of predicted um power current to f array in subts1.
     if (loopss(8) .ne. 3) go to 1100
     if (iprsup .ge. 1) write (lunit6,1006) jm
1006 format(/, ' *************************************** Injection of predicted power-coil currents for um number', i4, ' :', /, &
          ' ***************************************', 2x, 'nodvo1', 5x, 'emtpf(nodvo1)', 2x, 'nodvo2', 5x, 'emtpf(nodvo2)')
     do n1 = 1,3
        n2 = 3 * (numum + jm - 1) + n1
        n3 = kcoil(jm) - 1 + n1
        n4 = nodvo1(n3)
        if (n4 .eq. nodvo2(n3)) go to 1030
        if (n4 .le. 1) go to 1010
        emtpf(n4) = emtpf(n4) + umcurp(n2)
1010    n5 = nodvo2(n3)
        if (n5 .le. 1) go to 1020
        emtpf(n5) = emtpf(n5) - umcurp(n2)
1020    d4 = emtpf(n4)
        d5 = emtpf(n5)
        if (n4 .le. 1) d4 = 0.0
        if (n5 .le. 1) d5 = 0.0
        if (iprsup .ge. 1) write (lunit6,1022) n3,n4,d4,n5,d5
1022    format(10h *********, 12h coil number,i4,2h :,14x,i6,e14.5,2x,i6,e14.5)
1030 end do
     if (jm .eq. numum) loopss(8) = 1
     if (jm .eq. numum) go to 21000
     go to 99999
     !     ncurpr is set to 1 on initiation of prediction calculation
     !     of um power voltages if power circuits are not compensated
1100 ncurpr = 0
     if (iprsup .ge. 1) write (lunit6,1110) jm
1110 format (/,40h ***************************************, 36h dynamic run of solvum for um number,i4,2h :)
     if (loopss(8) .eq. 1) flxdmh = flxd(jm)
     if (loopss(8) .eq. 1) flxqmh = flxq(jm)
     kcl = kcoil(jm)
     !  in case of sm type-59 data input
     ntyp59 = 0
     if (jtype(jm) .ne. 13) go to 1200
     if (umoutp(numout+1) .eq. -9999.0) loopss(1) = 6644
     if (istart .eq. 0) loopss(1) = 7766
     jtype(jm) = 1
     ntyp59 = 1
     inpust = inpu
     inpu = 0
     nmexc = 0
     if (fpar(kcl+3) .ge. 0.0) go to 1200
     kconex = - fpar(kcl+3)
     nmexc = fpar(kcl+4)
     nexc = fpar(kcl+5)
1200 if (inpu .ne. 1) go to 1210
     seltat = omegrf * deltat
1210 selta2 = seltat/2.0
     rppair = nppair(jm)
     if (istart .ne. 0) go to 1220
     if (jcdsat(jm) .ne. 5) go to 1220
     reamqu(jm) = reamdu(jm)
     reamqs(jm) = reamds(jm)
     flxqr(jm) = flxdr(jm)
     flxqs(jm) = flxds(jm)
1220 if (inpu .eq. 2) go to 1300
     flxdrr = 0.0
     flxqrr = 0.0
     go to 1600
     !  if remanent flux option is requested (inpu = 2) :
1300 if (istart .ne. 0) go to 1450
     n1 = jcdsat(jm) + jcqsat(jm)
     if (n1 .ne. 0) go to 1400
     flxdr(jm) = 0.0
     flxqr(jm) = 0.0
1400 if (flxdr(jm) .lt. 0.0) flxdr(jm) = - flxdr(jm)
     if (flxqr(jm) .lt. 0.0) flxqr(jm) = - flxqr(jm)
1450 if (jcdsat(jm) .ne. 5) go to 1500
     flxdrr = flxdr(jm)
     flxqrr = flxdrr
     flxda = 0.0
     flxqa = 0.0
     go to 1999
1500 flxdrr = flxdr(jm)
     flxqrr = flxqr(jm)
     !   calculation of flxda and flxqa (saturation line) if no
     !     total saturation (if jcdsat = 5 , calculation is later):
1600 if (reamdu(jm) .eq. 0.0) flxda = 0.0
     if (reamdu(jm) .eq. 0.0) go to 1610
     flxda = (reamds(jm)/reamdu(jm)) * (flxdrr-flxds(jm))
     flxda = flxda + flxds(jm)
     if (jcdsat(jm) .eq. 0) flxda = 0.0
1610 flxqa = (reamqs(jm)/reamqu(jm)) * (flxqrr-flxqs(jm))
     flxqa = flxqa + flxqs(jm)
     if (jcqsat(jm) .eq. 0) flxqa = 0.0
1999 do n1 = 1,10
2000    con(n1) = 0.0
     end do
     if ( jtype(jm)  .ge.  13 )   go to 2001
     n9 = jtype(jm)
     go to (2001,2002,2003,2004,2005,2006,2007,2008, 2009,2010,2011,2012),n9
2001 con(3) = 1.0
     con(5) = 1.0
     goto 3000
2002 con(2) = 1.0
     con(5) = 1.0
     goto 3000
2003 con(3) = 1.0
     go to 3000
2004 con(3) = 1.0
     con(6) = 1.0
     go to 3000
2005 con(2) = 1.0
     go to 3000
2006 con(1) = 1.0
     go to 3000
2007 con(10) = 1.0
     go to 3000
2008 con(4) = 1.0
     go to 3000
2009 con(4) = 1.0
     con(7) = 1.0
     go to 3000
2010 con(4) = 1.0
     con(7) = 1.0
     go to 3000
2011 con(4) = 1.0
     con(8) = 1.0
     go to 3000
2012 con(4) = 1.0
     con(8) = 1.0
     con(9) = 1.0
     !. coil markings :
3000 kcld1 = kcoil(jm) + 3
     kclde = kcoil(jm) + 2  + ncld(jm)
     kclq1 = kclde + 1
     kclqe = kclde + nclq(jm)
     kcle  = kclqe
     ncl   = 3 + ncld(jm) + nclq(jm)
     if (con(6) .lt. 1.0) go to 3010
     ncl = ncl + 1
     kcle = kcle + 1
3010 do n1 = kcl,kcle
3100    fpar(n1) = 1.0/(1.0 + gpar(n1)*reacl(n1)/selta2)
     end do
     !. zero initializtion of all thev variables :
     zthev = 0.0
     do n1 = 1,ncl
        vinp(n1) = 0.0
3500    zthevs(n1) = 0.0
     end do
     do n1 = 1,3
        do n2 = 1,3
           zthevr(n1,n2) = 0.0
3520       zths3(n1,n2) = 0.0
        end do
     end do
     !. thev voltages and impedances for um-stator(excitation):
     n15 = kcle - kcl - 2
     if (ncl .gt. 6) n15 = 3
     do n1 = 1,9
4000    ndum(n1) = 0
     end do
     do n1 = 1,n15
        n2 = kcl + 2 + n1
        ndum(n1) = nodvo1(n2)
        ndum(n1+3) = nodvo2(n2)
        ndum(n1+6) = nodvo1(n2) - nodvo2(n2)
4011 end do
     do n1 = 1,n15
        n2 = ndum(n1)
        n3 = ndum(n1+3)
        n4 = n1 + 6
        if (ndum(n4) .eq. 0) go to 4022
        vinp(n1+3) = - emtpe(n2) + emtpe(n3)
4022 end do
     if (istart .eq. 0) go to 4110
     n20 = 0
     do n1 = 1,n15
        n2 = ndum(n1) + n20 * ntot
        n3 = ndum(n1+3) + n20 * ntot
        n5 = ndum(n1)
        n6 = ndum(n1+3)
        n7 = n1 + 6
        n8 = ndum(1) + n20 * ntot
        n9 = ndum(4) + n20 * ntot
        if (ndum(n7) .eq. 0) go to 4033
        n20 = n20 + 1
        zths3(n1,n1) = - znonl(n2) + znonl(n3)
        if (n1 .ne. 1) go to 4030
        d1 = dcoef(jm) * dcoef(jm)
        if (ntyp59 .eq. 1) zths3(1,1) = zths3(1,1)*d1
        if (n15 .eq. 1) go to 4200
        go to 4033
4030    if (ndum(7) .eq. 0) go to 4033
        zths3(1,n1) = - znonl(n8) + znonl(n9)
        zths3(n1,1) = - znonl(n5) + znonl(n6)
4033 end do
     if (n15 .lt. 3) go to 4200
     n19 = 0
     n20 = 0
     n1 = ndum(8) * ndum(9)
     if (n1 .eq. 0) go to 4110
     if (ndum(7) .ne. 0) n20 = 1
     n1 = 3
4044 n2 = ndum(n1) + n20 * ntot
     n3 = ndum(n1+3) + n20 * ntot
     if (n19 .eq. 1) go to 4045
     zths3(3,2) = - znonl(n2) + znonl(n3)
     n20 = n20 + 1
     n19 = 1
     n1 = 2
     go to 4044
4045 zths3(2,3) = - znonl(n2) + znonl(n3)
4110 if (ncl .le. 6) go to 4200
     n15 = kcld1 + 3
     do n1 = n15,kcle
        n2 = nodvo1(n1)
        n3 = nodvo2(n1)
        n10 = n1 - kcl + 1
        if (nodvo1(n1) .eq. nodvo2(n1)) go to 4120
        if (istart .eq. 0) go to 4115
        zthevs(n10) = - znonl(n2) + znonl(n3)
4115    vinp(n10) = - emtpe(n2) + emtpe(n3)
4120 end do
     !. thev voltages and impedances for rotor(power)-coils :
4200 do n1 = 1,3
        n2 = n1 + kcl - 1
        ndum(n1) = nodvo1(n2)
        ndum(n1+3) = nodvo2(n2)
        ndum(n1+6) = nodvo1(n2) - nodvo2(n2)
4211 end do
     do n1 = 1,3
        n2 = ndum(n1)
        n3 = ndum(n1+3)
        n4 = n1 + 6
        if (ndum(n4) .eq. 0) go to 4222
        vinp(n1) = emtpe(n2) - emtpe(n3)
4222 end do
     if (loopss(8) .eq. 1) go to 5000
     if (istart .eq. 0) go to 5000
     n20 = 0
     do n1 = 1,3
        n2 = ndum(n1) + n20 * ntot
        n3 = ndum(n1+3) + n20 * ntot
        n5 = ndum(n1)
        n6 = ndum(n1+3)
        n7 = n1 + 6
        n8 = ndum(1) + n20 * ntot
        n9 = ndum(4) + n20 * ntot
        if (ndum(n7) .eq. 0) go to 4233
        n20 = n20 + 1
        zthevr(n1,n1) = - znonl(n2) + znonl(n3)
        if (n1 .eq. 1) go to 4233
        if (ndum(7) .eq. 0) go to 4233
        zthevr(1,n1) = - znonl(n8) + znonl(n9)
        zthevr(n1,1) = - znonl(n5) + znonl(n6)
4233 end do
     zthev = con(4) * zthevr(3,3)
     zthevr(3,3) = zthevr(3,3) * (con(2) + con(3))
     n19 = 0
     n20 = 0
     n1 = ndum(8) * ndum(9)
     if (n1 .eq. 0) go to 5000
     if (ndum(7) .ne. 0) n20 = 1
     n1 = 3
4244 n2 = ndum(n1) + n20 * ntot
     n3 = ndum(n1+3) + n20 * ntot
     if (n19 .eq. 1) go to 4245
     zthevr(3,2) = - znonl(n2) + znonl(n3)
     n20 = n20 + 1
     n19 = 1
     n1 = 2
     go to 4244
4245 zthevr(2,3) = - znonl(n2) + znonl(n3)
     !. generate the scalar parameter rd2 for special dm:
5000 rd2 = 0.0
     if (jtype(jm) .lt. 9) go to 6000
     if (jtype(jm) .gt. 11) go to 6000
     if (gpar(kcl+4) .ne. 0.0) rd2 = 1.0/gpar(kcl+4)
     !. update for special dm-types: csc + cpc = 1
6000 d1 = con(7) + con(8)
     if (d1 .eq. 0.0) go to 7000
     reacl(kcl+2) = reacl(kcl+2) + d1 * reacl(kcl+4)
     reacl(kcl+3) = reacl(kcl+3) + d1 * reacl(kcl+4)
     fpar(kcl +2) = 1.0/(1.0 + gpar(kcl+2)*reacl(kcl+2)/selta2)
     fpar(kcl+3) = 1.0/(1.0 + gpar(kcl+3)*reacl(kcl+3)/selta2)
     d2 = d1 * zthev + con(8)*(reacl(kcl+4)+rd2)/selta2
     betaq = fpar(kcl+2) * gpar(kcl+2) * d2
     betad1 = fpar(kcl+3) * gpar(kcl+3) * d2
     !. thevenin mechanical impedance and speed:
7000 n1 = nodom(jm)
     nshare = jcltac(kcl) + jcltac(kcl+1)
     if (nshare .ne. 0) mshare = 1
     zthevm = 0.0
     omthev = 0.0
     zthem2 = 0.0
     zthem3 = 0.0
     tqgen2 = 0.0
     tqgen3 = 0.0
     if (jtmtac(jm) .ne. 0) go to 8000
     omthev = emtpe(n1)
     if (istart .eq. 0) go to 8000
     zthevm = - znonl(n1)
     !  if more um's share the same mech network of this um nr. jm :
     if (nshare .eq. 0) go to 8000
     n12 = jcltac(kcl)
     tqgen2 = voltum(n12)
     n13 = jcltac(kcl+1)
     if (n13 .eq. 0) go to 7010
     tqgen3 = voltum(n13)
7010 n4 = 0
     if (jm .gt. n12 ) n4 = 1
     if (jm .gt. n13 .and. n13 .ne. 0) n4 = n4 + 1
     n5 = 0
     if (n12 .gt. jm) n5 = 1
     if (n12 .gt. n13 .and. n13 .ne. 0) n5 = n5 + 1
     !  now position the correct entry to the table znonl :
     n1 = nodom(jm) + n4*ntot
     n2 = nodom(jm) + n5*ntot
     zthevm = - znonl(n1)
     zthem2 = - znonl(n2)
     if (n13 .eq. 0) go to 8000
     n6 = 3 - (n4 + n5)
     n3 = nodom(jm) + n6*ntot
     zthem3 = - znonl(n3)
     !. tacs input for um-stator (excitation coils) :
8000 do n1 = 4,ncl
        n2 = kcl - 1 + n1
        n3 = jcltac(n2)
        if (n3 .eq. 0) go to 8002
        if (n1 .gt. 3) go to 8001
        vinp(n1) = vinp(n1) + xtcs(n3)
        go to 8002
8001    vinp(n1) = vinp(n1) - xtcs(n3)
8002 end do
     if (ntyp59 .eq. 1) vinp(4) = vinp(4)*dcoef(jm)
     !. voltage input for special dm:
     if (d1 .eq. 0.0) go to 9000
     vinp(4) = - d1 * vinp(3)
     vinp(5) = 0.0
     !.tacs input for mechanical input torque:
9000 tumtac = 0.0
     if (jtmtac(jm) .eq. 0) go to 11000
     n1 = jtmtac(jm)
     tumtac = xtcs(n1)
     !. determination of history at time = 0.0 :
11000 if (istart .ne. 0) go to 12000
     thetae = rppair * (1.0-con(1)-con(4)) * thetam(jm)
     omege = rppair * omegm(jm)
     go to 12200
     !  calculation of um power currents for output :
12000 if (loopss(8) .eq. 0) go to 12004
     n3 = 3 * (numum + jm - 1)
     curum1 = umcurp(n3+1) - gpar(kcl+2)*vinp(1)
     curum2 = umcurp(n3+2) - gpar(kcl+2)*vinp(2)
     curum3 = umcurp(n3+3) - gpar(kcl+2)*vinp(3)
     go to 12016
     !. storing zthevr and vinp(1:3) for nitrom:
12004 do n1 = 1,3
        zthevs(n1) = vinp(n1)
        do n2 = 1,3
12010      zmat(n1,n2) = zthevr(n1,n2)
        end do
12015 end do
     !. prediction for omega-iteration:
12016 omegmp = 2.0 * omegm(jm) - omold(jm)
     omold(jm) = omegm(jm)
     theold = thetam(jm)
     nitrom = -1
12020 nitrom = nitrom + 1
     if (loopss(8) .ne. 0) go to 12100
     do n1 = 1,3
        vinp(n1) = zthevs(n1)
        do n2 = 1,3
12030      zthevr(n1,n2) = zmat(n1,n2)
        end do
12035 end do
12100 thetam(jm) = theold + selta2 * (omold(jm) + omegmp)
     thetae = rppair * (1.0-con(1)-con(4))*thetam(jm)
     omege = rppair * omegmp
     !.set up park transformation:
12200 ptheta(1,1) = 1.0
     ptheta(2,2) = cosz(thetae)
     ptheta(2,3) = - sinz(thetae)
     ptheta(3,2) = - ptheta(2,3)
     ptheta(3,3) = ptheta(2,2)
     do n1 = 2,3
        ptheta(1,n1) = 0.0
12210   ptheta(n1,1) = ptheta(1,n1)
     end do
     do n1 = 1,3
12220   dummat(1,n1) = con(3)/sroot3
     end do
     dummat(2,1) = con(3) * sroot2/sroot3
     dummat(2,3) = - con(3)/(sroot2*sroot3)
     dummat(2,2) = dummat(2,3) + con(2) + con(10)
     dummat(3,1) = 0.0
     dummat(3,2) = - con(3)/sroot2
     dummat(3,3) = -dummat(3,2)+con(2)+con(1)-con(10)+con(4)
     call matmul(ptheta,dummat)
     if (ncurpr .eq. 1) go to 14609
     !. updating for special im(3-phase rotor) and smat :
     if (con(6) .ne. 1.0) go to 12300
     if (nitrom .gt. 0) go to 12300
     do n1 = 1,3
        n3 = n1 + 1
        if (n1 .eq. 3) n3 = 1
        do n2 = 1,3
12225      smat(n1,n2) = dummat(n3,n2)
        end do
12230 end do
     do n2 = 1,3
        n3 = n2 - 1
        do n1 = 1,3
           if (n2 .ne. 1) go to 12231
           dumvec(n1) = smat(n1,n2)
           go to 12232
12231      smat(n1,n3) = smat(n1,n2)
           if (n2 .ne. 3) go to 12232
           smat(n1,n2) = dumvec(n1)
12232   end do
12233 end do
     do n1 = 1,3
        n2 = n1 + 3
12240   dumvec(n1) = vinp(n2)
     end do
     call matvec(smat,dumvec)
     do n1 = 1,3
        n2 = n1 + 3
12250   vinp(n2) = dumvec(n1)
     end do
     !. transform the um-rotor(power) voltages:
12300 if (loopss(8) .ne. 0) go to 12304
     do n1 = 1,3
12301   dumvec(n1) = vinp(n1)
     end do
     call matvec(ptheta,dumvec)
     do n1 = 1,3
12302   vinp(n1) = dumvec(n1)
     end do
     !. determination of initial currents in um domain :
12304 if (istart .ne. 0) go to 12400
     if (initum .ne. 0) go to 12330
12310 do n1 = 1,3
        n2 = n1 + kcl - 1
12320   dumvec(n1) = hist(n2)
     end do
     call matvec(ptheta,dumvec)
     do n1 = 1,3
12325   umcur(n1) = dumvec(n1)
     end do
     if (con(6) .ne. 1.0) go to 12335
     dumvec(1) = - hist(kcl+5)
     dumvec(2) = - hist(kcl+3)
     dumvec(3) = - hist(kcl+4)
     call matvec(dummat,dumvec)
     umcur(4) = dumvec(2)
     umcur(5) = dumvec(3)
     umcur(6) = dumvec(1)
     go to 12350
12330 if (initum .eq. 0) go to 12350
     do n1 = 1,3
        n2 = n1 + kcl - 1
12332   umcur(n1) = + hist(n2)
     end do
     if (jtype(jm) .ne. 4) go to 12335
     do n1 = 4,6
        n2 = n1 + kcl - 1
12334   umcur(n1) = - hist(n2)
     end do
     go to 12350
12335 do n2 = kcld1,kcle
        n1 = n2 - kcl + 1
12340   umcur(n1) = - hist(n2)
     end do
     !. calculation of initial flxd(jm) and flxq(jm):
12350 d10 = umcur(2)
     d15 = umcur(3)
     if (ncld(jm) .lt. 1) go to 12360
     do n2 = kcld1,kclde
        n1 = n2 - kcl + 1
12355   d10 = d10 + umcur(n1)
     end do
     if (ncld(jm) .le. 1) go to 12360
     if (jtype(jm) .ne. 12) go to 12360
     d10 = d10 - umcur(5)
12360 if (nclq(jm) .lt. 1) go to 12370
     do n2 = kclq1,kclqe
        n1 = n2 - kcl + 1
12365   d15 = d15 + umcur(n1)
     end do
12370 curmd = d10
     curmq = d15
     if (jcdsat(jm) .ne. 5) go to 12372
     curmt = d10*d10 + d15*d15
     if (curmt .eq. 0.0) go to 12375
     curmt = sqrtz(curmt)
     d19 = d10 * flxds(jm)/curmt
     d20 = d15 * flxqs(jm)/curmt
     if (d19 .lt. 0.0) d19 = - d19
     if (d20 .lt. 0.0) d20 = - d20
     if (reamdu(jm) .eq. 0.0) go to 12375
     d18 = reamds(jm)/reamdu(jm)
     flxda = d19 + d18*(flxdrr - d19)
     flxqa = d20 + d18*(flxqrr - d20)
12372 if (jcdsat(jm) .eq. 0) go to 12375
     if (reamdu(jm) .eq. 0.0) go to 12375
     d11 = flxds(jm)/reamdu(jm)
     d12 = - d11
     if (d10 .gt. d11) go to 12377
     if (d10 .lt. d12) go to 12377
12375 flxd(jm) = reamdu(jm) * d10 + flxdrr
     if (d10 .lt. 0.0) flxd(jm) = flxd(jm)-2.*flxdrr
     if (jcdsat(jm) .ne. 5) go to 12376
     if (curmt .eq. 0.0) go to 12385
12376 go to 12380
12377 if (d10 .ge. 0.0) flxd(jm) = flxda + reamds(jm)*d10
     if (d10 .lt. 0.0) flxd(jm) = -flxda + reamds(jm)*d10
12380 if (jcqsat(jm) .eq. 0) go to 12385
     d16 = flxqs(jm)/reamqu(jm)
     d17 = - d16
     if (d15 .gt. d16) go to 12387
     if (d15 .lt. d17) go to 12387
12385 flxq(jm) = reamqu(jm) * d15 + flxqrr
     if (d15 .lt. 0.0) flxq(jm) = flxq(jm)-2.*flxqrr
     go to 14200
12387 if (d15 .ge. 0.0) flxq(jm) = flxqa + reamqs(jm)*d15
     if (d15 .lt. 0.0) flxq(jm) = -flxqa + reamqs(jm)*d15
     go to 14200
     !. transform the um-rotor(power) thevenin impedances:
12400 if (loopss(8) .eq. 1) go to 12465
     do n1 = 1,3
        do n2 = 1,3
12410      dummat(n1,n2) = ptheta(n2,n1)
12420   end do
     end do
     call matmul(zthevr,dummat)
     do n1 = 1,3
        do n2 = 1,3
12430      dummat(n1,n2) = ptheta(n1,n2)
        end do
12440 end do
     call matmul(dummat,zthevr)
     do n1 = 1,3
        do n2 = 1,3
12450      zthevr(n1,n2) = dummat(n1,n2)
        end do
12460 end do
     !. transform the um-stator(excit) in case of abc-excitation :
12465 if (con(6) .ne. 1.0) go to 12600
     if (nitrom .gt. 0) go to 12600
     do n1 = 1,3
        do n2 = 1,3
12470      dummat(n1,n2) = smat(n2,n1)
        end do
     end do
     call matmul(zths3,dummat)
     call matmul(smat,zths3)
     !. note: from now on smat is in back-transformation mode :
     !.                             (only for t > 0.0)
     do n1 = 1,3
        do n2 = 1,3
           zths3(n1,n2) = smat(n1,n2)
12480      smat(n1,n2) = dummat(n1,n2)
        end do
     end do
     !. calculation of flxd and flxq:
     !. calculation of jacobian:
12600 flxd(jm) = 0.0
     flxq(jm) = 0.0
     jacob = 1
     go to 13000
12601 fjd = umcur(2)
     fjq = umcur(3)
     if (loopss(8) .eq. 1) fjd = 0.0
     if (loopss(8) .eq. 1) fjq = 0.0
     if (kcld1 .gt. kclde) go to 12603
     do n1 = kcld1,kclde
        n2 = n1 - kcl + 1
12602   fjd = fjd + umcur(n2)
     end do
     if (jtype(jm) .ne. 12) go to 12603
     fjd = fjd - umcur(5)
12603 if (kclq1 .gt. kclqe) go to 12605
     do n1 = kclq1,kclqe
        n2 = n1 - kcl + 1
12604   fjq = fjq + umcur(n2)
     end do
12605 flxd(jm) = 1.0
     flxq(jm) = 0.0
     jacob = 2
     go to 13000
12606 fjdd = umcur(2)
     fjqd = umcur(3)
     if (loopss(8) .eq. 1) fjdd = 0.0
     if (loopss(8) .eq. 1) fjqd = 0.0
     if (kcld1 .gt. kclde) go to 12608
     do n1 = kcld1,kclde
        n2 = n1 - kcl + 1
12607   fjdd = fjdd + umcur(n2)
     end do
     if (jtype(jm) .ne. 12) go to 12608
     fjdd = fjdd - umcur(5)
12608 fjdd = fjdd - fjd
     if (kclq1 .gt. kclqe) go to 12610
     do n1 = kclq1,kclqe
        n2 = n1 - kcl + 1
12609   fjqd = fjqd + umcur(n2)
     end do
12610 fjqd = fjqd - fjq
     flxd(jm) = 0.0
     flxq(jm) = 1.0
     jacob = 3
     go to 13000
12611 fjdq = umcur(2)
     fjqq = umcur(3)
     if (loopss(8) .eq. 1) fjdq = 0.0
     if (loopss(8) .eq. 1) fjqq = 0.0
     if (kcld1 .gt. kclde) go to 12613
     do n1 = kcld1,kclde
        n2 = n1 - kcl + 1
12612   fjdq = fjdq + umcur(n2)
     end do
     if (jtype(jm) .ne. 12) go to 12613
     fjdq = fjdq - umcur(5)
12613 fjdq = fjdq - fjd
     if (kclq1 .gt. kclqe) go to 12615
     do n1 = kclq1,kclqe
        n2 = n1 - kcl + 1
12614   fjqq = fjqq + umcur(n2)
     end do
12615 fjqq = fjqq - fjq
     if (loopss(8) .eq. 1) fjd = fjd + umcur(2)
     if (loopss(8) .eq. 1) fjq = fjq + umcur(3)
     jacob = 4
     !. start flxds and flxqs check:
     !    curmd and curmq are repeatedly calculated to find if
     !       reamdu or reamds and reamqu or reamqs needs to be used.
     !    n1 = 1: first time and unsaturated to find sflxd and sflxq
     !    n1 = 5: command to repeat calculations in saturated condition
     !            if saturation option is requested
     !    n1 = 2: command to exit calculation
     !    n1 = 3: command to repeat with q-axis saturation
     !    n1 = 4: command to repeat with d-axis saturation
     !    the constant n2 is used for total saturation option purpose
12700 n1 = 1
     n2 = 1
     n5 = 1 + jcdsat(jm) + 2*jcqsat(jm)
     !  first find sflxd and sflxq with no sat. and res. magn.
     d15 = 0.0
     sflxd = 0.0
     sflxq = 0.0
     go to 12702
12701 if ( n5  .eq.  2 )   go to 12704
     if ( n5  .eq.  3 )   go to 12706
     if ( n5  .eq.  4 )   go to 12708
     if (n5 .ge. 5) go to 12708
12702 d12 = 0.0
     d13 = 0.0
     go to 12710
12704 d12 = 1.0
     d13 = 0.0
     go to 12710
12706 d12 = 0.0
     d13 = 1.0
     go to 12710
12708 d12 = 1.0
     d13 = 1.0
12710 d1 = sflxd * ((1.0-d12)*d15*flxdrr + d12*flxda)
     d2 = sflxq * ((1.0-d13)*d15*flxqrr + d13*flxqa)
     d3 = fjdd*d1 + fjdq*d2 + fjd
     d4 = fjqq*d2 + fjqd*d1 + fjq
     d5 = reamdu(jm)*(1.0-d12) + reamds(jm)*d12
     d6 = reamqu(jm)*(1.0-d13) + reamqs(jm)*d13
     d7 = 1.0 - fjdd*d5
     d8 = 1.0 - fjqq*d6
     d9 = - fjdq * d6
     d10 = - fjqd * d5
     d11 = d7*d8 - d9*d10
     curmd = (d8*d3 - d9*d4)/d11
     curmq = (d7*d4 - d10*d3)/d11
     d1 = curmd * curmd
     d1 = d1 + curmq * curmq
     curmt = sqrtz(d1)
     d19 = sflxd * curmd
     d20 = sflxq * curmq
     !  deletion of 6 lines to the m28 version on 28 dec 1980,
     !  the deleted line numbers : m28.6109 to m28.6114
12722 if ( n1  .ge.  5 )   go to 12725
     go to (12800,12900,12780,12790), n1
12725 if ( n5  .ge.  5 )   go to 12750
     go to (12900,12730,12740,12750,12750), n5
12730 if (d19 .gt. d17) go to 12900
     n1 = 2
     n2 = 0
     go to 12702
12740 if (d20 .gt. d18) go to 12900
     n1 = 2
     n2 = 0
     go to 12702
12750 if (n5 .ne. 5) go to 12755
     n2 = 0
     if (d19 .gt. d17) go to 12900
     if (d20 .gt. d18) go to 12900
     n1 = 2
     go to 12702
12755 if (d19 .le. d17 .and. d20 .le. d18) go to 12790
     if (d19 .gt. d17 .and. d20 .gt. d18) go to 12900
     if (d19 .gt. d17) go to 12770
     go to 12760
12760 n1 = 3
     n2 = 0
     go to 12706
12770 n1 = 4
     n2 = 0
     go to 12704
12780 if (d20 .gt. d18) go to 12900
     n1 = 2
     n2 = 0
     go to 12702
12790 if (d19 .gt. d17) go to 12900
     n1 = 2
     n2 = 0
     go to 12702
12800 if (n5 .eq. 1) go to 12900
     n1 = 5
     d15 = 1.0
     sflxd = 1.0
     sflxq = 1.0
     if (curmd .lt. 0.0) sflxd = - 1.0
     if (curmq .lt. 0.0) sflxq = - 1.0
     if (reamdu(jm) .eq. 0.0) go to 12802
     d17 = (flxds(jm) - flxdrr)/reamdu(jm)
     d18 = (flxqs(jm) - flxqrr)/reamqu(jm)
12802 if (jcdsat(jm) .eq. 5) go to 12804
     go to 12701
     !   calculation of flxda and flxqa if total saturation :
12804 if (curmt .ne. 0.0) go to 12806
     d1 = flxds(jm)
     d2 = d1
     go to 12808
12806 d19 = sflxd * curmd
     d20 = sflxq * curmq
     d17 = d17 * d19/curmt
     d18 = d18 * d20/curmt
     d1 = d19 * flxds(jm)/curmt
     d2 = d20 * flxds(jm)/curmt
12808 d3 = reamds(jm)/reamdu(jm)
     flxda = d1 + d3*(flxdrr - d1)
     flxqa = d2 + d3*(flxqrr - d2)
     n5 = 5
     go to 12701
12900 flxd(jm) = reamdu(jm) * (1.0-d12) * curmd
     flxq(jm) = reamqu(jm) * (1.0-d13) * curmq
     if (n5 .eq. 1) go to 13000
     flxd(jm) = flxd(jm) + reamds(jm)*d12*curmd
     flxd(jm) = flxd(jm) + sflxd*(1.0-d12)*flxdrr
     flxd(jm) = flxd(jm) + d12*sflxd*flxda
     flxq(jm) = flxq(jm) + reamqs(jm)*d13*curmq
     flxq(jm) = flxq(jm) + sflxq*(1.0-d13)*flxqrr
     flxq(jm) = flxq(jm) + d13*sflxq*flxqa
     if (n2 .ne. 0) go to 13000
     if (d12 .ne. 0.0) go to 12910
     d1 = flxd(jm) * flxd(jm)
     d2 = flxdrr * flxdrr
     if (d1 .le. d2) flxd(jm) = sflxd * flxdrr
12910 if (d13 .ne. 0.0) go to 13000
     d1 = flxq(jm) * flxq(jm)
     d2 = flxqrr * flxqrr
     if (d1 .le. d2) flxq(jm) = sflxq * flxqrr
     !. umcur-rotor(power) equations:
     !. formation of acurr
13000 if (loopss(8) .eq. 0) go to 13008
     if (jacob .gt. 1) go to 13100
     umcur(1) = curum1
     umcur(2) = curum2
     umcur(3) = curum3
     call matvec(ptheta,umcur)
     if (gpar(kcl) .eq. 0.0) umcur(1) = 0.0
     go to 13100
13008 do n1 = 1,3
        do n2 = 1,3
13010      dummat(n1,n2) = 0.0
        end do
13020 end do
     dummat(1,1) = gpar(kcl) * fpar(kcl)
     dummat(2,2) = gpar(kcl+1) * fpar(kcl+1)
     dummat(3,3) = gpar(kcl+2) * fpar(kcl+2)
     do n1 = 2,3
13025   dumvec(n1) = dummat(n1,n1)
     end do
     call matmul(dummat,zthevr)
     d1 = zthev + (con(7)+con(8)) *rd2
     dummat(3,3) = dummat(3,3)+dumvec(3)*d1+1.0
     dummat(2,2) = dummat(2,2) + 1.0
     dummat(1,1) = dummat(1,1) + 1.0
     d1 = (1.0-con(4))*omege*dumvec(2)*reacl(kcl+2)
     dummat(2,3) = dummat(2,3) + d1
     d1 = (1.0-con(4))*omege*dumvec(3)*reacl(kcl+1)
     dummat(3,2) = dummat(3,2) - d1
     !.formation of umcur-rotor(power):
     do n1 =1,3
        n2 = kcl - 1 + n1
13030   dumvec(n1) = - gpar(n2)*fpar(n2)*vinp(n1) + hist(n2)
     end do
     d2 = + gpar(kcl+1) * fpar(kcl+1)
     d3 = + gpar(kcl+2) * fpar(kcl+2)
     d5 = - d2 * (flxd(jm)/selta2 + flxq(jm)*omege)
     dumvec(2) = dumvec(2) + d5
     d5 = d3 * (flxd(jm)*omege - flxq(jm)/selta2)
     d6 = - d3 * flxd(jm) * (con(7)+con(8))/selta2
     dumvec(3) = dumvec(3) + d5 + d6
     !. final result for umcur-rotor(power):
     call lineqs(dummat,dumvec)
     do n1 = 1,3
13050   umcur(n1) = dumvec(n1)
     end do
     !.formation of umcur-stator(excit) :
13100 do n1 = kcld1,kclqe
        n2 = n1 - kcl + 1
13110   umcur(n2) = - gpar(n1)*fpar(n1)*vinp(n2) + hist(n1)
     end do
     if (ncld(jm) .eq. 0) go to 13150
     do n1 = kcld1,kclde
        n2 = n1 - kcl + 1
        d1 = - flxd(jm)*gpar(n1)*fpar(n1)/selta2
13120   umcur(n2) = umcur(n2) + d1
     end do
     d4 = gpar(kcl+3) * fpar(kcl+3)
     umcur(4) = umcur(4) + flxd(jm)*con(8)*d4/selta2
13150 if (nclq(jm) .eq. 0) go to 13200
     do n1 = kclq1,kclqe
        n2 = n1 - kcl + 1
        d1 = - flxq(jm)*gpar(n1)*fpar(n1)/selta2
13160   umcur(n2) = umcur(n2) + d1
     end do
     !. umcur-stator(excit)  for um=type 4(c3im):
13200 if (con(6) .eq. 0.0) go to 13210
     n1 =  kcle - kcl +1
     umcur(n1) = hist(kcle) - gpar(kcle)*fpar(kcle)*vinp(n1)
     !. implementing influence of thev impedances :
13210 d1 = con(7) + con(8)
     if (d1 .lt. 1.0) go to 13212
     zths3(1,1) = d1 * zthev + con(8) * rd2
     dummat(1,1) = 1.0 + gpar(kcld1)*fpar(kcld1)*zths3(1,1)
     go to 13230
13212 n15 = kcle
     if (ncl .gt. 6) n15 = kcl + 5
     do n1 = 1,3
        do n2 = 1,3
13215      dummat(n1,n2) = 0.0
        end do
     end do
     do n1 = kcld1,n15
        n2 = n1 - kcl - 2
13220   dummat(n2,n2) = gpar(n1) * fpar(n1)
     end do
     call matmul(dummat,zths3)
     do n1 = 1,3
13225   dummat(n1,n1) = dummat(n1,n1) + 1.0
     end do
     n16 = n15 - kcl - 2
     if ( n16  .eq.  2 )   go to 13235
     if ( n16  .eq.  3 )   go to 13240
13230 umcur(4) = umcur(4)/dummat(1,1)
     go to 13260
13235 d1 = dummat(1,1) * dummat(2,2)
     d1 = d1 - dummat(1,2) * dummat(2,1)
     d4 = umcur(4)
     d5 = umcur(5)
     umcur(4) = (dummat(2,2)*d4 - dummat(1,2)*d5)/d1
     umcur(5) = (dummat(1,1)*d5 - dummat(2,1)*d4)/d1
     go to 13260
13240 do n1 = 1,3
        n2 = n1 + 3
13245   dumvec(n1) = umcur(n2)
     end do
     call lineqs(dummat,dumvec)
     do n1 = 1,3
        n2 = n1 + 3
13250   umcur(n2) = dumvec(n1)
     end do
13260 if (ncl .le. 6) go to 13300
     n15 = kcld1 + 3
     do n1 = n15,kcle
        n2 = n1 - kcl + 1
        d1 = 1.0 + gpar(n1) * fpar(n1) * zthevs(n2)
13265   umcur(n2) = umcur(n2)/d1
     end do
     !. umcur update for um = special dm-types :
13300 d1 = con(7) + con(8)
     if (d1 .eq. 0.0) go to 14100
     d2 = 1.0/(1.0 - betaq*betad1)
     d3 = umcur(3)
     d4 = umcur(4)
     umcur(3) = (d3 + betaq*d4) * d2
     umcur(4) = (betad1*d3 + d4) * d2
     if (con(9) .ne. 0.0) go to 14100
     umcur(5) = d1 * umcur(3) - con(8) * umcur(4)
     !. program control for calculation of jacobian:
14100 if ( jacob  .ge.  4 )   go to 14200
     go to (12601, 12606, 12611),  jacob
     !. calculation of tqgen:
14200 d1 = 3.0*con(3)+2.0*con(2)+con(1)+2.0*con(10)+con(4)
     d2 = (reacl(kcl+1)-reacl(kcl+2))*umcur(2)*umcur(3)
     d3 = umcur(2) * flxq(jm)
     d4 = umcur(3) * flxd(jm)
     tqgen = (d2 - d3 + d4) * rppair
     if (inpu .eq. 1) tqgen = tqgen/d1
     !. solve omegm from torque equation:
14300 if (jtmtac(jm) .eq. 0) go to 14310
      tdamp = dcoef(jm) * omegm(jm)
      tcin = (tumtac - tqgen - tdamp) * selta2/rotmom(jm)
      if (istart .eq. 0) go to 14510
      omegm(jm) = histom(jm) + tcin
      go to 14400
14310 if (istart .eq. 0) go to 14510
      omegm(jm) = omthev - zthevm * tqgen
      if (nshare .eq. 0) go to 14400
      omegm(jm) = omegm(jm) - zthem2*tqgen2 - zthem3*tqgen3
      !. check criterium for omegm-iteration:
14400 d1 = omegm(jm) - omegmp
      d2 = + epsom(jm)
      d3 = - d2
      if (d1 .gt. d2) go to 14410
      if (d1 .ge. d3) go to 14500
14410 omegmp = omegm(jm)
      if (nitrom .le. 50) go to 14416
      write (lunit6,14414)
14414 format ( 42h0rotor speed iteration fails  in 50 steps.  )
      write (lunit6,14415) jm,t
14415 format(25h failing machine number :,i3,3x,6htime =,e14.5)
      inpu = 50
      go to 79
14416 go to 12020
      !. calculation of torque angle for sm:
14500 omegm(jm) = omegmp
14510 if (jtype(jm) .gt. 2) go to 14600
      tangle = thetae - (tau + pi/2.0)
      !. output vectors for 3-and 2-phase rotors(power coils):
14600 noutst = nxout + 1
      !. output vector for torque:
      if (jtqout(jm) .eq. 0) go to 14602
      nxout = nxout + 1
      umoutp(nxout) = tqgen
      if (jtqout(jm) .lt. 2) go to 14602
      nxout = nxout + 1
      umoutp(nxout) = flxd(jm)
      if (jtqout(jm) .ne. 3) go to 14602
      nxout = nxout + 1
      umoutp(nxout) = curmd
14602 if (jomout(jm) .eq. 0) go to 14603
      nxout = nxout + 1
      umoutp(nxout) = omegm(jm)
      if (jomout(jm) .lt. 2) go to 14603
      nxout = nxout + 1
      umoutp(nxout) = flxq(jm)
      if (jomout(jm) .ne. 3) go to 14603
      nxout = nxout + 1
      umoutp(nxout) = curmq
14603 if (jthout(jm) .eq. 0) go to 14606
      nxout = nxout + 1
      if (jtype(jm) .gt. 2) go to 14604
      umoutp(nxout) = tangle
      if (ntyp59 .ne. 1) go to 14606
      if (jthout(jm) .eq. 1) umoutp(nxout)=tangle*360.0/twopi+90.0
      if (jthout(jm) .eq. 3) umoutp(nxout) = - crest(kconex)
      go to 14606
14604 umoutp(nxout) = thetam(jm)
      !. output vector for um-rotor(power):
14606 if (loopss(8) .eq. 1  .and.  istart .ne. 0) go to 14616
      do n1 = 1,3
14607    dumvec(n1) = umcur(n1)
      end do
14609 do n1 = 1,3
         do n2 = 1,3
14610       dummat(n1,n2) = ptheta(n2,n1)
         end do
14615 end do
      call matvec(dummat,dumvec)
      if (ncurpr .eq. 1) go to 18100
14616 if (loopss(8) .eq. 0) go to 14619
      if (istart .eq. 0) go to 14619
14618 dumvec(1) = curum1
      dumvec(2) = curum2
      dumvec(3) = curum3
14619 if (con(4) .eq. 0.0) go to 14620
      dumvec(3) = umcur(3) - (con(7)+con(8))*umcur(4)
14620 do n1 = 1,3
         n2 = kcl - 1 + n1
         if (loopss(8) .eq. 1) go to 14622
         if (nodvo1(n2) .eq. nodvo2(n2)) go to 14622
         cursub(nclcom) = - dumvec(n1)
         nclcom = nclcom + 1
         n10 = nodvo1(n2)
         if (n10 .gt. 1) emtpf(n10) = emtpf(n10) + dumvec(n1)
         n10 = nodvo2(n2)
         if (n10 .gt. 1) emtpf(n10) = emtpf(n10) - dumvec(n1)
14622    if (jclout(n2) .eq. 0) go to 14625
         nxout = nxout + 1
         umoutp(nxout) = dumvec(n1)
14624    if (jclout(n2) .eq. 2) umoutp(nxout) = umcur(n1)
14625    if ( iprsup  .ge.  1 ) write (lunit6, 6589)  n1,n2,nxout, jclout(n2), umcur(n1), umoutp(nxout)
6589     format ( 37h n1, n2, nxout, jclout(n2), umcur(n1), 16 h umoutp(nxout) =, 4i8,2e14.5)
14626 end do
      !. output vector for stator(excit) if um = 3-phase im:
      if (con(6) .ne. 1.0) go to 14660
      if (istart .ne. 0) go to 14629
      do n1 = 1,3
         do n2 = 1,3
14628       dummat(n1,n2) = smat(n2,n1)
         end do
      end do
14629 do n1 = 1,3
14630    dumvec(n1) = umcur(n1+3)
      end do
      if (istart .ne. 0) go to 14631
      call matvec(dummat,dumvec)
      go to 14632
14631 call matvec(smat,dumvec)
14632 do n1 = 1,3
         go to (14635,14640,14645),n1
14635    n2 = 4
         n3 = 1
         go to 14650
14640    n2 = 5
         n3 = 2
         go to 14650
14645    n2 = 6
         n3 = 3
14650    n4 = kcl - 1 + n2
         if (nodvo1(n4) .eq. nodvo2(n4)) go to 14652
         cursub(nclcom) = dumvec(n3)
         nclcom = nclcom + 1
         n10 = nodvo1(n4)
         if (n10 .gt. 1) emtpf(n10) = emtpf(n10) - dumvec(n3)
         n10 = nodvo2(n4)
         if (n10 .gt. 1) emtpf(n10) = emtpf(n10) + dumvec(n3)
14652    if (jclout(n4) .eq. 0) go to 14655
         nxout = nxout + 1
         umoutp(nxout) = - dumvec(n3)
         if ( iprsup  .ge.  1 ) write (lunit6, 6589)  n1,n2,nxout, jclout(n2), umcur(n1), umoutp(nxout)
14655 end do
      go to 14700
      !. output vectors for stator(excit) in general:
14660 do n1 = 4,ncl
         d1 = 1.0
         if (n1 .eq. 4 .and. ntyp59 .eq. 1) d1 = dcoef(jm)
         n2 = kcl - 1 + n1
         if (nodvo1(n2) .eq. nodvo2(n2)) go to 14664
         cursub(nclcom) = umcur(n1) * d1
         nclcom = nclcom + 1
         n10 = nodvo1(n2)
         if (n10 .gt. 1) emtpf(n10) = emtpf(n10) - umcur(n1)*d1
         n10 = nodvo2(n2)
         if (n10 .gt. 1) emtpf(n10) = emtpf(n10) + umcur(n1)*d1
14664    if (jclout(n2) .eq. 0) go to 14665
         nxout = nxout + 1
         umoutp(nxout) = - umcur(n1)
         if (ntyp59 .eq. 1) umoutp(nxout) = + umcur(n1)*dcoef(jm)
         if ( iprsup  .ge.  1 ) write (lunit6, 6589)  n1,n2,nxout, jclout(n2), umcur(n1), umoutp(nxout)
14665 end do
      !. output vector if mech. system = electr. network :
14700 if (jtmtac(jm) .ne. 0) go to 15000
      n10 = nodom(jm)
      emtpf(n10) = emtpf(n10) - tqgen
      if (jcltac(kcl+2) .lt. 0) go to 15000
      cursub(nclcom) = tqgen
      nclcom = nclcom + 1
      !  in case this um is lowest numbered um in the set of um's
      !    sharing a common mech network :
      if (jcltac(kcl+2) .le. 0) go to 15000
      nclcom = nclcom + 1
      if (jcltac(kcl+1) .ne. 0) nclcom = nclcom+1
      !. emtpe(k) and vinp adjustment for rem magn + open power terminals:
15000 if (inpu .ne. 2) go to 15005
      if (istart .ne. 0) go to 15005
      if (jtype(jm) .eq. 6) go to 15005
      if (jtype(jm) .eq. 7) go to 15005
      d1 = flxdrr * flxdrr + flxqrr * flxqrr
      if (d1 .eq. 0.0) go to 15005
      do n1 = 1,3
         if (umcur(n1) .ne. 0.0) go to 15005
15001 end do
      dumvec(1) = 0.0
      dumvec(2) = - omege * flxq(jm)
      dumvec(3) = + omege * flxd(jm)
      if (jtype(jm) .gt. 7) dumvec(2) = 0.0
      d1 = vinp(2)*vinp(2) + vinp(3)*vinp(3)
      d2 = dumvec(2)*dumvec(2) + dumvec(3)*dumvec(3)
      if (d1 .gt. d2) go to 15005
      do n1 = 1,3
         vinp(n1) = dumvec(n1)
         do n2 = 1,3
15002       dummat(n1,n2) = ptheta(n2,n1)
         end do
15003 end do
      call matvec(dummat,dumvec)
      do n1 = 1,3
         n2 = kcl - 1 + n1
         n4 = kcl + 1
         n3 = nodvo1(n2)
         if (nodvo2(kcl) .ne. nodvo2(n4)) n3 = nodvo2(n2)
         emtpe(n3) = dumvec(n1)
         if (nodvo2(kcl) .ne. nodvo2(n4)) emtpe(n3) = - dumvec(n1)
15004 end do
      !. calculation of history vectors:
      !.determination of rotor(power)-hist:
15005 if (loopss(8) .ne. 0) go to 15100
      do n1 = 1,3
         do n2 = 1,3
15006       dummat(n1,n2) = 0.0
         end do
15010 end do
      if (loopss(8) .eq. 1) go to 15100
      d11 = gpar(kcl+1) * fpar(kcl+1)
      d12 = gpar(kcl+2) * fpar(kcl+2)
      dummat(1,1) = - gpar(kcl) * fpar(kcl)
      dummat(2,2) = - d11
      dummat(3,3) = - d12
      call matmul(dummat,zthevr)
      d1 = zthev + (con(7) + con(8)) * rd2
      dummat(3,3) = dummat(3,3) - d1 * d12
      do n1 = 1,3
         n2 = kcl - 1 + n1
         d1 = (gpar(n2)*reacl(n2)/selta2 - 1.0) * fpar(n2)
15015    dummat(n1,n1) = dummat(n1,n1) + d1
      end do
      d1 = (1.0-con(4))*omege*d11*reacl(kcl+2)
      dummat(2,3) = dummat(2,3) - d1
      d1 = (1.0-con(4))*omege*d12*reacl(kcl+1)
      dummat(3,2) = dummat(3,2) + d1
      do n1 = 1,3
15020    dumvec(n1) = umcur(n1)
      end do
      call matvec(dummat,dumvec)
      do n1 = 1,3
         n2 = kcl - 1 + n1
15025    hist(n2) = dumvec(n1) - gpar(n2)*fpar(n2)*vinp(n1)
      end do
      d5 = d11 * (flxd(jm)/selta2 - flxq(jm)*omege)
      hist(kcl+1) = hist(kcl+1) + d5
      d5 = d12 * (flxd(jm)*omege + flxq(jm)/selta2)
      hist(kcl+2) = hist(kcl+2) + d5
      d1 = (con(7)+con(8)) * d12
      hist(kcl+2) = hist(kcl+2) + flxd(jm)*d1/selta2
      !. rotor(power)-hist for special dm:
      if (con(8) .ne. 1.0) go to 15050
      d9 = rd2 - reacl(kcl+4)/selta2
      d10 = zthev + d9
      hist(kcl+2) = hist(kcl+2) + con(8)*d12*d10*umcur(5)
15050 if (con(7) .ne. 1.0) go to 15100
      hist(kcl+2) = hist(kcl+2) + con(7)*d12*zthev*umcur(5)
      !. determination of stator(excit)-hist:
15100 do n1 = kcld1,kclqe
         n2 = n1 - kcl + 1
         d1 = gpar(n1) * fpar(n1)
         d2 = d1 * reacl(n1)/selta2
         d3 = (- fpar(n1) + d2) * umcur(n2)
15105    hist(n1) = - d1*vinp(n2) + d3
      end do
      if (ncld(jm) .eq. 0) go to 15115
      d13 = gpar(kcl+3) * fpar(kcl+3)
      do n1 = kcld1,kclde
         d1 = flxd(jm) * gpar(n1) * fpar(n1)/selta2
15110    hist(n1) = hist(n1) + d1
      end do
      hist(kcl+3) = hist(kcl+3)-flxd(jm)*con(8)*d13/selta2
15115 if (nclq(jm) .eq. 0) go to 15200
      do n1 = kclq1,kclqe
         d1 = flxq(jm) * gpar(n1) * fpar(n1)/selta2
15120    hist(n1) = hist(n1) + d1
      end do
      !. stator(excit)-hist for special dm:
      if (con(8) .ne. 1.0) go to 15150
      hist(kcl+3) = hist(kcl+3) + con(8)*d13*d10*umcur(3)
      d9 = (zthev + rd2) * d13/selta2
      hist(kcl+3) = hist(kcl+3) - con(8)*d9*umcur(4)
15150 if (con(7) .ne. 1.0) go to 15200
      hist(kcl+3) = hist(kcl+3) + con(7)*d13*zthev*umcur(3)
      d9 = zthev * d13/selta2
      hist(kcl+3) = hist(kcl+3) - con(7)*d9*umcur(4)
      !. stator(excit)-hist for um = type 4 (c3im):
15200 if (con(6) .eq. 0.0) go to 15210
      n1 = kcle - kcl + 1
      d1 = gpar(kcle) * fpar(kcle)
      d2 = d1*reacl(kcle)/selta2 - fpar(kcle)
      hist(kcle) = d2*umcur(n1) - d1*vinp(n1)
      !. implementation of influence of thev impedances :
15210 n15 = kcle
      if (ncl .gt. 6) n15 = kcl + 5
      n16 = n15 - kcl - 2
      do n1 = 1,3
         if (n1 .gt. n16) go to 15225
         dumvec(n1) = umcur(n1+3)
         go to 15230
15225    dumvec(n1) = 0.0
15230 end do
      call matvec(zths3,dumvec)
      do n1 = kcld1,n15
         n2 = n1 - kcl - 2
         dumvec(n2) = gpar(n1) * fpar(n1) * dumvec(n2)
15235    hist(n1) = hist(n1) - dumvec(n2)
      end do
      if (ncl .le. 6) go to 15300
      n15 = kcld1 + 3
      do n1 = n15,kcle
         n2 = n1 - kcl + 1
         d1 = gpar(n1) * fpar(n1) * zthevs(n2)
15240    hist(n1) = hist(n1) - d1 * umcur(n2)
      end do
      !. determination of histom:
15300 histom(jm) = 0.0
      if (jtmtac(jm) .eq. 0) go to 16000
      histom(jm) = omegm(jm) + tcin
      !. output statements for um:
16000 n1 = istart + jm - 1
      if (n1 .ne. 0) go to 16008
      if ( iprsup .ge. 1 ) write (lunit6,16005)
16005 format(50h0-------------------------------------------------)
      go to 16016
16008 if (istart .ne. 0) go to 16012
16011 go to 16020
16012 itolto = itol + iout
      if (iout .eq. 0) go to 16100
16014 if (istart .ne. itolto) go to 16100
      if (jm .gt. 1) go to 16020
16016 if (iprsup .ge. 2 ) write (lunit6,16005)
      if (iprsup .ge. 2 ) write (lunit6,16017) t, ncomp
16017 format(7h0time =,e14.5,36h***************************  ncomp =,  i3  )
16020 if (jm .eq. numum) itol = istart
      !. momentary test output :
      if ( iprsup  .ge.  2 ) write (lunit6,16052)
16052 format(18h0*** test output :)
      if ( iprsup  .ge.  3 ) write (lunit6,16054) (umcur(n1),n1=1,3)
16054 format(10h0umcur(1)=,e14.5,2x,9humcur(2)=,e14.5,2x,9humcur(3)=,e14.5)
      if (con(6) .ne. 1.0) go to 16056
      if ( iprsup  .ge.  3 ) write (lunit6,16055) (umcur(n1),n1=4,6)
16055 format(10h umcur(4)=,e14.5,2x,9humcur(5)=,e14.5,2x,9humcur(6)=,e14.5)
16056 if ( iprsup  .ge.  2 ) write (lunit6,16057) flxd(jm),flxq(jm),nitrom
16057 format(10h     flxd=,e14.5,2x,9h    flxq=,e14.5,2x,9h  nitrom=,i3)
      n2 = kcl + ncl - 1
      if (istart .gt. 1) go to 16077
      if ( iprsup .le. 3 ) go to 16077
      write (lunit6,16060) (nodvo1(n1),n1=kcl,n2)
      write (lunit6,16061) (nodvo2(n1),n1=kcl,n2)
      write (lunit6,16062) nodom(jm)
16060 format(9h0nodvo1 :,3x,15i4)
16061 format(9h nodvo2 :,3x,15i4)
16062 format(9h nodom  :,3x,15i4)
16077 if (iprsup .ge. 2) write (lunit6,16078) zthevm
16078 format(10h0zthevm = ,e14.5)
      if (iprsup .ge. 2) write (lunit6,16080) ((zthevr(n1,n2),n2=1,3),n1=1,3)
16080 format(13h0zthevr(3,3):,3x,3e14.5/(16x,3e14.5))
      if (iprsup .ge. 2) write (lunit6,16082) ((zths3(n1,n2),n2=1,3),n1=1,3)
16082 format(12h0zths3(3,3):,3x,3e14.5/(15x,3e14.5))
      !. end momentary
      !  final statements of machine do-loop *************************
16100 if (nshare .ne. 0) fpar(kcl+2) = tqgen
      !. in case of sm type-59 : storing and inject new exciter torque
      if (ntyp59 .ne. 1) go to 18000
      jtype(jm) = 13
      inpu = inpust
      if (nmexc .eq. 0) go to 18000
      d1 = emtpe(nmexc)
      d2 = emtpe(nexc)
      d3 = umcur(4) * dcoef(jm)
      crest(kconex) = - d2 * d3/d1
      fpar(kcl+3) = - kconex
      fpar(kcl+4) = nmexc
      fpar(kcl+5) = nexc
      !. prediction of fluxes for internal um voltage of next time-
      !      step if no compensation for power coils.
      !      umcurp(1) to store zero current
      !      umcurp(3numum:6numum) to store vpi/rp for next time-step.
18000 if (loopss(8) .ne. 1) go to 99999
      ncurpr = 1
      d19 = gpar(kcl) * fpar(kcl)
      d20 = gpar(kcl) * reacl(kcl)
      n1 = 3 * (jm -1)
      n2 = 3 * (numum+jm-1)
      !      transformation to zero-freq domain :
      d10 = (tau - thetae) * (1.0-con(1)-con(4))
      if (inpu .eq. 1) d10 = d10 * omegrf
      d11 = cosz(d10)
      d12 = sinz(d10)
      d15 = flxd(jm)
      d16 = flxq(jm)
      flxd(jm) = d11*d15 - d12*d16
      flxq(jm) = d12*d15 + d11*d16
      d15 = umcur(2)
      d16 = umcur(3)
      umcur(2) = d11*d15 - d12*d16
      umcur(3) = d12*d15 + d11*d16
      flxdd = flxd(jm) + reacl(kcl+1)*umcur(2)
      flxqq = flxq(jm) + reacl(kcl+2)*umcur(3)
      !      initialization at time = 0.0
      if (istart .ne. 0) go to 18020
      flxdmh = flxd(jm)
      flxqmh = flxq(jm)
      umcurp(n1+1) = 0.0
      hist(kcl) = 0.0
      hist(kcl+1) = flxdd
      hist(kcl+2) = flxqq
      !      linear prediction of fluxes :
18020 flxddp = 2.0*flxdd - hist(kcl+1)
      flxqqp = 2.0*flxqq - hist(kcl+2)
      !       calculation of vpi/rp for next time-step
      !     dumvec(1) = d20 * (umcurp(n1+1) - umcur(1))/seltat
      dumvec(1) = 0.0
      !     d1 = (hist(kcl+1) - flxdd)/seltat
      d1 = (flxdmh - flxd(jm))/seltat
      dumvec(2) = (d1 - omegrf*flxqqp) * gpar(kcl+1)
      !     d1 = (hist(kcl+2) - flxqq)/seltat
      d1 = (flxqmh - flxq(jm))/seltat
      dumvec(3) = (d1 + omegrf*flxddp) * gpar(kcl+2)
      d20 = fpar(kcl) * (d20/selta2 - 1.0)
      umcurp(n1+1) = umcur(1)
      hist(kcl+1) = flxdd
      hist(kcl+2) = flxqq
18030 thetae = tau + deltat*omegrf
      thetae = thetae * (1.0-con(1)-con(4))
      if (inpu .eq. 1) thetae = omegrf * thetae
      go to 12200
18100 do n1 = 1,3
         n2 = 3 * (numum+jm-1) + n1
18120    umcurp(n2) = dumvec(n1)
      end do
      if (iprsup .ge. 1) write (lunit6,18130) jm
18130 format(/,39h **************************************, 42h predicted a,b,c power voltages/resistance, &
           32h for next time-step of um number, i4, 2h :)
      n1 = 3 * (numum + jm - 1) + 1
      n2 = n1 + 2
      if (iprsup .ge. 1) write (lunit6,18132) n1,n2,(umcurp(n3),n3=n1,n2)
18132 format(5x, 8h umcurp(,i3,1h:,i3,3h) :,3(3x,e14.5))
      if (jm .ne. numum) go to 99999
      loopss(8) = 3
      !. continuation of machine do-loop:
99999 end do
   !  start second mach do-loop in case a set of um's sharing
   !   a common mechanical network is present :
30000 if (mshare .eq. 0) go to 20000
   do jm = 1,numum
      kcl = kcoil(jm)
      nshare = jcltac(kcl) + jcltac(kcl+1)
      if (nshare .eq. 0) go to 30010
      !  predicting torques of higher numbered um's in the set :
      !    note : fpar(kcl+2) is tqgen at time t
      !    now predict tqgen for next time-step (for t+deltat)
      voltum(jm) = 2.0 * fpar(kcl+2) - anglum(jm)
      if (istart .eq. 0) voltum(jm) = fpar(kcl+2)
      anglum(jm) = fpar(kcl+2)
      !  setting  cursub for tqgen of higher numbered um's in set :
      if (jcltac(kcl+2) .ge. 0) go to 30010
      n1 = - jcltac(kcl+2)
      cursub(n1) = fpar(kcl+2)
30010 end do
20000 istart = istart + 1
21000 if ( iprsup  .ge.  1 ) write (lunit6, 7892)  istart,loopss(1),loopss(8),numum,t
7892 format ( /,  33h exit  "solvum" .  istart loopss1, 16 h loopss8   numum,13x,  1ht  ,/,  17x, 4i8,  e14.5,/)
   return
 end subroutine solvum
 !
 ! subroutine lineqs.
 !
 subroutine lineqs(aum,yum)
   implicit real(8) (a-h, o-z), integer(4) (i-n)
   dimension bum(3,4),aum(3,3),yum(15)
   n5 = 3
   n6 = n5 + 1
   ! formation of augmented matrix
   do n10 = 1,n5
      do n11 = 1,n5
10       bum(n10,n11) = aum(n10,n11)
      end do
      bum(n10,n6)  = yum(n10)
20 end do
   ! start gaussian elimination
   do n1 = 1,n5
      n2 = n1 + 1
      if (n2 .gt. n5) go to 50
      do n10 = n2,n5
         n8 = n6 - n1
         do n9 = 1,n8
            n11 = n6 + 1 - n9
30          bum(n10,n11) = bum(n10,n11)-bum(n10,n1)*bum(n1,n11)/bum(n1,n1)
         end do
         n11 = n6 - n8
         bum(n10,n11) = 0.0
40    end do
   end do
   ! calculation of solution
50 do n10 = 1,n5
      n7 = n5 + 1 - n10
      yum(n7) = bum(n7,n6)/bum(n7,n7)
      do n12 = 1,n5
         n13 = n7 + n12
         if (n13 .gt. n5) go to 70
60       yum(n7) = yum(n7)-bum(n7,n13)*yum(n13)/bum(n7,n7)
70    end do
   end do
   return
 end subroutine lineqs
 !
 ! subroutine subts4.
 !
 subroutine subts4
   implicit real(8) (a-h, o-z),  integer(4) (i-n)
   include 'blkcom.ftn'
   include 'labcom.ftn'
   include 'umdeck.ftn'
   dimension xx(1)
   dimension nsubkm(1)
   equivalence (kknonl(1), nsubkm(1))
   equivalence (xk(1), xx(1))
   dimension ispum(1)
   equivalence (spum(1), ispum(1))
   equivalence (moncar(1), knt), (moncar(2), kbase)
   equivalence (moncar(9), kloaep)
   if ( iprsup.ge.6 ) write (lunit6, 1000) kswtch, inonl, num99, ncomp, ntot
1000 format (  36h top "subts4". kswtch, inonl, num99,, 14h ncomp, ntot =,  10i8 )
1553 if ( numum  .le.  0 )   go to 1742
   call solvum ( spum(iureac), spum(iugpar), spum(iufpar), spum(iuhist), spum(iuumrp), &
        ispum(iunod1), ispum(iunod2), ispum(iujclt), ispum(iujclo), &
        ispum(iujtyp), ispum(iunodo), ispum(iujtmt), spum(iuhism), &
        spum(iuomgm), spum(iuomld), spum(iutham), spum(iuredu), &
        spum(iureds), spum(iuflds), spum(iufldr), spum(iurequ), &
        spum(iuflqs), spum(iuflqr), ispum(iujcds), ispum(iujcqs), &
        spum(iuflxd), spum(iuflxq), ispum(iunppa), spum(iurotm), &
        ispum(iuncld), ispum(iunclq), ispum(iujtqo), ispum(iujomo), &
        ispum(iujtho), spum(iureqs), spum(iuepso), spum(iudcoe), &
        ispum(iukcoi), spum(iuvolt), spum(iuangl), ispum(iunodf), &
        ispum(iunodm), ispum(iukumo), ispum(iujumo), spum(iuumou))
   if ( kill  .gt.  0 )   go to 9200
1742 if ( ncomp .le. 0 )  go to 1570
   if ( iprsup .ge. 6 ) write (lunit6, 1554)  ( k, kknonl(k), emtpe(k), znonl(k), k=2, ntot )
1554 format(  41h n.l. element comp.   k, kknonl, e, znonl   , 2i10, 2e20.8 )
   do k=2,ntot
      n1 = kpsour(k)
      if (n1.eq.0) go to 1560
      n15 = isubeg(n1)
      n4 = k
1111  m = n15 / 5 + 1
      emtpe(k) = emtpe(k)  +  cursub(m) * znonl(n4)
      n15 = nsubkm(n15)
      if ( n15  .eq. isubeg(n1) ) go to 2323
1557  n4 = n4 + ntot
      go to 1111
2323  n8 = n15 / 5 + 1
      if ( iprsup  .ge.  7 ) write (lunit6, 1558)  k, n1, n15, emtpe(k), cursub(n8), znonl(k)
1558  format (  11h row super., 24h       k      n1     n15,  14x,  1he, 9x, 6hcursub, 10x, 5hznonl ,/, 11x, 3i8, 3e15.6 )
1560 end do
1570 if ( ialter  .eq.  1 )   ialter = 0
   if ( iprsup .gt. 6 ) write (lunit6, 1571)  ( emtpe(k), k=1, ntot )
1571 format(//, ' Vector emtpe(k) at 1571, immediately after calc', /, (1x, 5e25.15))
   if ( tenerg  .lt.  0.0 )          go to 1609
   if ( knt  .gt.  1 )               go to 1609
   if ( kbase  .lt.  2 )             go to 1609
   if ( t + deltat  .lt.  tenerg )   go to 1609
   if ( iprsup  .ge.  -1 ) write (lunit6, 4624)  knt, kbase, kloaep, nenerg, t, tenerg
4624 format (/, 113h at end of  'subts4', common blocks are written on 'lunit2'  during the 1st energization of  'statistics'  case. , /, &
          32h     knt   kbase  kloaep  nenerg,  14x,  1ht,  9x,  6htenerg ,/,  4i8,  2e15.6    )
   call tables
   tenerg = -t
1609 if ( t  .le.  tmax )   go to 1611
   if ( memsav  .ne.  0 )   go to 1613
1611 lastov = nchain
   nchain = 16
   return
1613 lastov = nchain
   nchain = 20
   return
9200 lastov = nchain
   lstat(18) = nchain
   nchain = 51
99999 return
 end subroutine subts4
 !
 !     end of file: over16.for
 !
