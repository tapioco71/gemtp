!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
!
!     file: vardim.for
!
program vardim
  implicit  real(8) (a-h, o-z), integer(4) (i-n)
  logical ifnamed
  integer(4) lunit(15)
  character char, bus2
  character(8) bus1, texta(2), textb(2), cblock, cblser, type
  character(16) ansi16
  character(80) abuff
  dimension cblock(300), ncbarr(300), cblser(300), jbltyp(300)
  dimension lstnew(99), lstdef(49), type(4, 3), kextra(29)
  dimension char(6), mulvar(4)
  data  cblock(  1)  / 'x     ' /, ncbarr(  1)  /  3 /
  data  cblser(  1)  / 'c0b001' /, jbltyp(  1)  / 0 /
  data  cblock(  2)  / 'ykm   ' /, ncbarr(  2)  /  5 /
  data  cblser(  2)  / 'c0b002' /, jbltyp(  2)  / 0 /
  data  cblock(  3)  / 'km    ' /, ncbarr(  3)  /  5 /
  data  cblser(  3)  / 'c0b003' /, jbltyp(  3)  / 0 /
  data  cblock(  4)  / 'xk    ' /, ncbarr(  4)  / 72 /
  data  cblser(  4)  / 'c0b004' /, jbltyp(  4)  / 0 /
  data  cblock(  5)  / 'xm    ' /, ncbarr(  5)  / 72 /
  data  cblser(  5)  / 'c0b005' /, jbltyp(  5)  / 0 /
  data  cblock(  6)  / 'weight' /, ncbarr(  6)  / 14 /
  data  cblser(  6)  / 'c0b006' /, jbltyp(  6)  / 0 /
  data  cblock(  7)  / 'iwtent' /, ncbarr(  7)  / 52 /
  data  cblser(  7)  / 'c0b007' /, jbltyp(  7)  / 0 /
  data  cblock(  8)  / 'con1  ' /, ncbarr(  8)  / 51 /
  data  cblser(  8)  / 'c0b008' /, jbltyp(  8)  / 0 /
  data  cblock(  9)  / 'iskip ' /, ncbarr(  9)  / 13 /
  data  cblser(  9)  / 'c0b009' /, jbltyp(  9)  / 0 /
  data  cblock( 10)  / 'zinf  ' /, ncbarr( 10)  / 13 /
  data  cblser( 10)  / 'c0b010' /, jbltyp( 10)  / 0 /
  data  cblock( 11)  / 'eta   ' /, ncbarr( 11)  / 13 /
  data  cblser( 11)  / 'c0b011' /, jbltyp( 11)  / 0 /
  data  cblock( 12)  / 'nhist ' /, ncbarr( 12)  / 13 /
  data  cblser( 12)  / 'c0b012' /, jbltyp( 12)  / 0 /
  data  cblock( 13)  / 'stailm' /, ncbarr( 13)  / 15 /
  data  cblser( 13)  / 'c0b013' /, jbltyp( 13)  / 0 /
  data  cblock( 14)  / 'stailk' /, ncbarr( 14)  / 15 /
  data  cblser( 14)  / 'c0b014' /, jbltyp( 14)  / 0 /
  data  cblock( 15)  / 'xmax  ' /, ncbarr( 15)  / 58 /
  data  cblser( 15)  / 'c0b015' /, jbltyp( 15)  / 0 /
  data  cblock( 16)  / 'koutvp' /, ncbarr( 16)  / 62 /
  data  cblser( 16)  / 'c0b016' /, jbltyp( 16)  / 0 /
  data  cblock( 17)  / 'bnrg  ' /, ncbarr( 17)  / 18 /
  data  cblser( 17)  / 'c0b017' /, jbltyp( 17)  / 0 /
  data  cblock( 18)  / 'sconst' /, ncbarr( 18)  / 20 /
  data  cblser( 18)  / 'c0b018' /, jbltyp( 18)  / 0 /
  data  cblock( 19)  / 'cnvhst' /, ncbarr( 19)  / 73 /
  data  cblser( 19)  / 'c0b019' /, jbltyp( 19)  / 0 /
  data  cblock( 20)  / 'sfd   ' /, ncbarr( 20)  / 71 /
  data  cblser( 20)  / 'c0b020' /, jbltyp( 20)  / 0 /
  data  cblock( 21)  / 'qfd   ' /, ncbarr( 21)  / 71 /
  data  cblser( 21)  / 'c0b021' /, jbltyp( 21)  / 0 /
  data  cblock( 22)  / 'semaux' /, ncbarr( 22)  / 22 /
  data  cblser( 22)  / 'c0b022' /, jbltyp( 22)  / 0 /
  data  cblock( 23)  / 'ibsout' /, ncbarr( 23)  / 12 /
  data  cblser( 23)  / 'c0b023' /, jbltyp( 23)  / 0 /
  data  cblock( 24)  / 'bvalue' /, ncbarr( 24)  / 12 /
  data  cblser( 24)  / 'c0b024' /, jbltyp( 24)  / 0 /
  data  cblock( 25)  / 'sptacs' /, ncbarr( 25)  / 19 /
  data  cblser( 25)  / 'c0b025' /, jbltyp( 25)  / 0 /
  data  cblock( 26)  / 'kswtyp' /, ncbarr( 26)  /  6 /
  data  cblser( 26)  / 'c0b026' /, jbltyp( 26)  / 0 /
  data  cblock( 27)  / 'modswt' /, ncbarr( 27)  /  6 /
  data  cblser( 27)  / 'c0b027' /, jbltyp( 27)  / 0 /
  data  cblock( 28)  / 'kbegsw' /, ncbarr( 28)  /  6 /
  data  cblser( 28)  / 'c0b028' /, jbltyp( 28)  / 0 /
  data  cblock( 29)  / 'lastsw' /, ncbarr( 29)  /  6 /
  data  cblser( 29)  / 'c0b029' /, jbltyp( 29)  / 0 /
  data  cblock( 30)  / 'kentnb' /, ncbarr( 30)  /  6 /
  data  cblser( 30)  / 'c0b030' /, jbltyp( 30)  / 0 /
  data  cblock( 31)  / 'nbhdsw' /, ncbarr( 31)  / 63 /
  data  cblser( 31)  / 'c0b031' /, jbltyp( 31)  / 0 /
  data  cblock( 32)  / 'topen ' /, ncbarr( 32)  / 60 /
  data  cblser( 32)  / 'c0b032' /, jbltyp( 32)  / 0 /
  data  cblock( 33)  / 'crit  ' /, ncbarr( 33)  /  6 /
  data  cblser( 33)  / 'c0b033' /, jbltyp( 33)  / 0 /
  data  cblock( 34)  / 'kdepsw' /, ncbarr( 34)  / 60 /
  data  cblser( 34)  / 'c0b034' /, jbltyp( 34)  / 0 /
  data  cblock( 35)  / 'tdns  ' /, ncbarr( 35)  /  6 /
  data  cblser( 35)  / 'c0b035' /, jbltyp( 35)  / 0 /
  data  cblock( 36)  / 'isourc' /, ncbarr( 36)  /  6 /
  data  cblser( 36)  / 'c0b036' /, jbltyp( 36)  / 0 /
  data  cblock( 37)  / 'energy' /, ncbarr( 37)  /  6 /
  data  cblser( 37)  / 'c0b037' /, jbltyp( 37)  / 0 /
  data  cblock( 38)  / 'iardub' /, ncbarr( 38)  / 63 /
  data  cblser( 38)  / 'c0b038' /, jbltyp( 38)  / 0 /
  data  cblock( 39)  / 'ardube' /, ncbarr( 39)  / 64 /
  data  cblser( 39)  / 'c0b039' /, jbltyp( 39)  / 0 /
  data  cblock( 40)  / 'nonlad' /, ncbarr( 40)  /  9 /
  data  cblser( 40)  / 'c0b040' /, jbltyp( 40)  / 0 /
  data  cblock( 41)  / 'nonle ' /, ncbarr( 41)  /  9 /
  data  cblser( 41)  / 'c0b041' /, jbltyp( 41)  / 0 /
  data  cblock( 42)  / 'vnonl ' /, ncbarr( 42)  /  9 /
  data  cblser( 42)  / 'c0b042' /, jbltyp( 42)  / 0 /
  data  cblock( 43)  / 'curr  ' /, ncbarr( 43)  /  9 /
  data  cblser( 43)  / 'c0b043' /, jbltyp( 43)  / 0 /
  data  cblock( 44)  / 'anonl ' /, ncbarr( 44)  /  9 /
  data  cblser( 44)  / 'c0b044' /, jbltyp( 44)  / 0 /
  data  cblock( 45)  / 'vecnl1' /, ncbarr( 45)  /  9 /
  data  cblser( 45)  / 'c0b045' /, jbltyp( 45)  / 0 /
  data  cblock( 46)  / 'vecnl2' /, ncbarr( 46)  /  9 /
  data  cblser( 46)  / 'c0b046' /, jbltyp( 46)  / 0 /
  data  cblock( 47)  / 'namenl' /, ncbarr( 47)  /  9 /
  data  cblser( 47)  / 'c0b047' /, jbltyp( 47)  / 0 /
  data  cblock( 48)  / 'vzero ' /, ncbarr( 48)  /  9 /
  data  cblser( 48)  / 'c0b048' /, jbltyp( 48)  / 0 /
  data  cblock( 49)  / 'ilast ' /, ncbarr( 49)  /  9 /
  data  cblser( 49)  / 'c0b049' /, jbltyp( 49)  / 0 /
  data  cblock( 50)  / 'nltype' /, ncbarr( 50)  /  9 /
  data  cblser( 50)  / 'c0b050' /, jbltyp( 50)  / 0 /
  data  cblock( 51)  / 'kupl  ' /, ncbarr( 51)  /  9 /
  data  cblser( 51)  / 'c0b051' /, jbltyp( 51)  / 0 /
  data  cblock( 52)  / 'nlsub ' /, ncbarr( 52)  /  9 /
  data  cblser( 52)  / 'c0b052' /, jbltyp( 52)  / 0 /
  data  cblock( 53)  / 'xoptbr' /, ncbarr( 53)  /  2 /
  data  cblser( 53)  / 'c0b053' /, jbltyp( 53)  / 0 /
  data  cblock( 54)  / 'coptbr' /, ncbarr( 54)  /  2 /
  data  cblser( 54)  / 'c0b054' /, jbltyp( 54)  / 0 /
  data  cblock( 55)  / 'cursub' /, ncbarr( 55)  / 53 /
  data  cblser( 55)  / 'c0b055' /, jbltyp( 55)  / 0 /
  data  cblock( 56)  / 'cchar ' /, ncbarr( 56)  / 10 /
  data  cblser( 56)  / 'c0b056' /, jbltyp( 56)  / 0 /
  data  cblock( 57)  / 'vchar ' /, ncbarr( 57)  / 10 /
  data  cblser( 57)  / 'c0b057' /, jbltyp( 57)  / 0 /
  data  cblock( 58)  / 'gslope' /, ncbarr( 58)  / 10 /
  data  cblser( 58)  / 'c0b058' /, jbltyp( 58)  / 0 /
  data  cblock( 59)  / 'ktrans' /, ncbarr( 59)  /  1 /
  data  cblser( 59)  / 'c0b059' /, jbltyp( 59)  / 0 /
  data  cblock( 60)  / 'kk    ' /, ncbarr( 60)  /  1 /
  data  cblser( 60)  / 'c0b060' /, jbltyp( 60)  / 0 /
  data  cblock( 61)  / 'emtpc ' /, ncbarr( 61)  /  3 /
  data  cblser( 61)  / 'c0b061' /, jbltyp( 61)  / 0 /
  data  cblock( 62)  / 'tr    ' /, ncbarr( 62)  /  5 /
  data  cblser( 62)  / 'c0b062' /, jbltyp( 62)  / 0 /
  data  cblock( 63)  / 'tx    ' /, ncbarr( 63)  /  5 /
  data  cblser( 63)  / 'c0b063' /, jbltyp( 63)  / 0 /
  data  cblock( 64)  / 'r     ' /, ncbarr( 64)  /  3 /
  data  cblser( 64)  / 'c0b064' /, jbltyp( 64)  / 0 /
  data  cblock( 65)  / 'nr    ' /, ncbarr( 65)  /  2 /
  data  cblser( 65)  / 'c0b065' /, jbltyp( 65)  / 0 /
  data  cblock( 66)  / 'length' /, ncbarr( 66)  /  2 /
  data  cblser( 66)  / 'c0b066' /, jbltyp( 66)  / 0 /
  data  cblock( 67)  / 'cik   ' /, ncbarr( 67)  /  2 /
  data  cblser( 67)  / 'c0b067' /, jbltyp( 67)  / 0 /
  data  cblock( 68)  / 'ci    ' /, ncbarr( 68)  /  2 /
  data  cblser( 68)  / 'c0b068' /, jbltyp( 68)  / 0 /
  data  cblock( 69)  / 'ck    ' /, ncbarr( 69)  /  2 /
  data  cblser( 69)  / 'c0b069' /, jbltyp( 69)  / 0 /
  data  cblock( 70)  / 'ismout' /, ncbarr( 70)  / 70 /
  data  cblser( 70)  / 'c0b070' /, jbltyp( 70)  / 0 /
  data  cblock( 71)  / 'elp   ' /, ncbarr( 71)  / 65 /
  data  cblser( 71)  / 'c0b071' /, jbltyp( 71)  / 0 /
  data  cblock( 72)  / 'cu    ' /, ncbarr( 72)  / 66 /
  data  cblser( 72)  / 'c0b072' /, jbltyp( 72)  / 0 /
  data  cblock( 73)  / 'shp   ' /, ncbarr( 73)  / 67 /
  data  cblser( 73)  / 'c0b073' /, jbltyp( 73)  / 0 /
  data  cblock( 74)  / 'histq ' /, ncbarr( 74)  / 68 /
  data  cblser( 74)  / 'c0b074' /, jbltyp( 74)  / 0 /
  data  cblock( 75)  / 'ismdat' /, ncbarr( 75)  / 69 /
  data  cblser( 75)  / 'c0b075' /, jbltyp( 75)  / 0 /
  data  cblock( 76)  / 'texvec' /, ncbarr( 76)  /  7 /
  data  cblser( 76)  / 'c0b076' /, jbltyp( 76)  / 1 /
  data  cblock( 77)  / 'ibrnch' /, ncbarr( 77)  / 12 /
  data  cblser( 77)  / 'c0b077' /, jbltyp( 77)  / 0 /
  data  cblock( 78)  / 'jbrnch' /, ncbarr( 78)  / 12 /
  data  cblser( 78)  / 'c0b078' /, jbltyp( 78)  / 0 /
  data  cblock( 79)  / 'tstop ' /, ncbarr( 79)  /  4 /
  data  cblser( 79)  / 'c0b079' /, jbltyp( 79)  / 0 /
  data  cblock( 80)  / 'nonlk ' /, ncbarr( 80)  /  9 /
  data  cblser( 80)  / 'c0b080' /, jbltyp( 80)  / 0 /
  data  cblock( 81)  / 'nonlm ' /, ncbarr( 81)  /  9 /
  data  cblser( 81)  / 'c0b081' /, jbltyp( 81)  / 0 /
  data  cblock( 82)  / 'spum  ' /, ncbarr( 82)  / 25 /
  data  cblser( 82)  / 'c0b082' /, jbltyp( 82)  / 0 /
  data  cblock( 83)  / 'kks   ' /, ncbarr( 83)  /  1 /
  data  cblser( 83)  / 'c0b083' /, jbltyp( 83)  / 0 /
  data  cblock( 84)  / 'kknonl' /, ncbarr( 84)  / 57 /
  data  cblser( 84)  / 'c0b084' /, jbltyp( 84)  / 0 /
  data  cblock( 85)  / 'znonl ' /, ncbarr( 85)  / 57 /
  data  cblser( 85)  / 'c0b085' /, jbltyp( 85)  / 0 /
  data  cblock( 86)  / 'znonlb' /, ncbarr( 86)  /  1 /
  data  cblser( 86)  / 'c0b086' /, jbltyp( 86)  / 0 /
  data  cblock( 87)  / 'znonlc' /, ncbarr( 87)  /  1 /
  data  cblser( 87)  / 'c0b087' /, jbltyp( 87)  / 0 /
  data  cblock( 88)  / 'finit ' /, ncbarr( 88)  /  1 /
  data  cblser( 88)  / 'c0b088' /, jbltyp( 88)  / 0 /
  data  cblock( 89)  / 'ksub  ' /, ncbarr( 89)  / 53 /
  data  cblser( 89)  / 'c0b089' /, jbltyp( 89)  / 0 /
  data  cblock( 90)  / 'msub  ' /, ncbarr( 90)  / 53 /
  data  cblser( 90)  / 'c0b090' /, jbltyp( 90)  / 0 /
  data  cblock( 91)  / 'isubeg' /, ncbarr( 91)  / 55 /
  data  cblser( 91)  / 'c0b091' /, jbltyp( 91)  / 0 /
  data  cblock( 92)  / 'litype' /, ncbarr( 92)  /  2 /
  data  cblser( 92)  / 'c0b092' /, jbltyp( 92)  / 0 /
  data  cblock( 93)  / 'imodel' /, ncbarr( 93)  /  2 /
  data  cblser( 93)  / 'c0b093' /, jbltyp( 93)  / 0 /
  data  cblock( 94)  / 'kbus  ' /, ncbarr( 94)  /  2 /
  data  cblser( 94)  / 'c0b094' /, jbltyp( 94)  / 0 /
  data  cblock( 95)  / 'mbus  ' /, ncbarr( 95)  /  2 /
  data  cblser( 95)  / 'c0b095' /, jbltyp( 95)  / 0 /
  data  cblock( 96)  / 'kodebr' /, ncbarr( 96)  /  2 /
  data  cblser( 96)  / 'c0b096' /, jbltyp( 96)  / 0 /
  data  cblock( 97)  / 'cki   ' /, ncbarr( 97)  /  2 /
  data  cblser( 97)  / 'c0b097' /, jbltyp( 97)  / 0 /
  data  cblock( 98)  / 'ckkjm ' /, ncbarr( 98)  /  2 /
  data  cblser( 98)  / 'c0b098' /, jbltyp( 98)  / 0 /
  data  cblock( 99)  / 'indhst' /, ncbarr( 99)  /  2 /
  data  cblser( 99)  / 'c0b099' /, jbltyp( 99)  / 0 /
  data  cblock(100)  / 'kodsem' /, ncbarr(100)  /  2 /
  data  cblser(100)  / 'c0b100' /, jbltyp(100)  / 0 /
  data  cblock(101)  / 'namebr' /, ncbarr(101)  / 54 /
  data  cblser(101)  / 'c0b101' /, jbltyp(101)  / 0 /
  data  cblock(102)  / 'iform ' /, ncbarr(102)  /  4 /
  data  cblser(102)  / 'c0b102' /, jbltyp(102)  / 0 /
  data  cblock(103)  / 'node  ' /, ncbarr(103)  /  4 /
  data  cblser(103)  / 'c0b103' /, jbltyp(103)  / 0 /
  data  cblock(104)  / 'crest ' /, ncbarr(104)  /  4 /
  data  cblser(104)  / 'c0b104' /, jbltyp(104)  / 0 /
  data  cblock(105)  / 'time1 ' /, ncbarr(105)  /  4 /
  data  cblser(105)  / 'c0b105' /, jbltyp(105)  / 0 /
  data  cblock(106)  / 'time2 ' /, ncbarr(106)  /  4 /
  data  cblser(106)  / 'c0b106' /, jbltyp(106)  / 0 /
  data  cblock(107)  / 'tstart' /, ncbarr(107)  /  4 /
  data  cblser(107)  / 'c0b107' /, jbltyp(107)  / 0 /
  data  cblock(108)  / 'sfreq ' /, ncbarr(108)  /  4 /
  data  cblser(108)  / 'c0b108' /, jbltyp(108)  / 0 /
  data  cblock(109)  / 'kmswit' /, ncbarr(109)  / 60 /
  data  cblser(109)  / 'c0b109' /, jbltyp(109)  / 0 /
  data  cblock(110)  / 'nextsw' /, ncbarr(110)  /  6 /
  data  cblser(110)  / 'c0b110' /, jbltyp(110)  / 0 /
  data  cblock(111)  / 'rmfd  ' /, ncbarr(111)  / 61 /
  data  cblser(111)  / 'c0b111' /, jbltyp(111)  / 0 /
  data  cblock(112)  / 'cikfd ' /, ncbarr(112)  / 61 /
  data  cblser(112)  / 'c0b112' /, jbltyp(112)  / 0 /
  data  cblock(113)  / 'imfd  ' /, ncbarr(113)  / 27 /
  data  cblser(113)  / 'c0b113' /, jbltyp(113)  / 0 /
  data  cblock(114)  / 'tclose' /, ncbarr(114)  /  6 /
  data  cblser(114)  / 'c0b114' /, jbltyp(114)  / 0 /
  data  cblock(115)  / 'adelay' /, ncbarr(115)  / 60 /
  data  cblser(115)  / 'c0b115' /, jbltyp(115)  / 0 /
  data  cblock(116)  / 'kpos  ' /, ncbarr(116)  /  6 /
  data  cblser(116)  / 'c0b116' /, jbltyp(116)  / 0 /
  data  cblock(117)  / 'namesw' /, ncbarr(117)  /  6 /
  data  cblser(117)  / 'c0b117' /, jbltyp(117)  / 0 /
  data  cblock(118)  / 'emtpe ' /, ncbarr(118)  /  1 /
  data  cblser(118)  / 'c0b118' /, jbltyp(118)  / 0 /
  data  cblock(119)  / 'emtpf ' /, ncbarr(119)  /  1 /
  data  cblser(119)  / 'c0b119' /, jbltyp(119)  / 0 /
  data  cblock(120)  / 'kssfrq' /, ncbarr(120)  /  1 /
  data  cblser(120)  / 'c0b120' /, jbltyp(120)  / 0 /
  data  cblock(121)  / 'kode  ' /, ncbarr(121)  /  1 /
  data  cblser(121)  / 'c0b121' /, jbltyp(121)  / 0 /
  data  cblock(122)  / 'kpsour' /, ncbarr(122)  /  1 /
  data  cblser(122)  / 'c0b122' /, jbltyp(122)  / 0 /
  data  cblock(123)  / 'volti ' /, ncbarr(123)  / 59 /
  data  cblser(123)  / 'c0b123' /, jbltyp(123)  / 0 /
  data  cblock(124)  / 'voltk ' /, ncbarr(124)  / 26 /
  data  cblser(124)  / 'c0b124' /, jbltyp(124)  / 0 /
  data  cblock(125)  / 'volt  ' /, ncbarr(125)  / 59 /
  data  cblser(125)  / 'c0b125' /, jbltyp(125)  / 0 /
  data  cblock(126)  / 'bus   ' /, ncbarr(126)  /  1 /
  data  cblser(126)  / 'c0b126' /, jbltyp(126)  / 1 /
  data  cblock(127)  / 'karray' /, ncbarr(127)  /  0 /
  data  cblser(127)  / 'c29b01' /, jbltyp(127)  / 0 /
  data  cblock(128)  / 'tp    ' /, ncbarr(128)  / 23 /
  data  cblser(128)  / 'spac01' /, jbltyp(128)  / 0 /
  data  cblock(129)  / 'norder' /, ncbarr(129)  /  1 /
  data  cblser(129)  / 'spac02' /, jbltyp(129)  / 0 /
  data  cblock(130)  / 'index ' /, ncbarr(130)  /  1 /
  data  cblser(130)  / 'spac03' /, jbltyp(130)  / 0 /
  data  cblock(131)  / 'diag  ' /, ncbarr(131)  /  1 /
  data  cblser(131)  / 'spac04' /, jbltyp(131)  / 0 /
  data  cblock(132)  / 'diab  ' /, ncbarr(132)  /  1 /
  data  cblser(132)  / 'spac05' /, jbltyp(132)  / 0 /
  data  cblock(133)  / 'solr  ' /, ncbarr(133)  /  1 /
  data  cblser(133)  / 'spac06' /, jbltyp(133)  / 0 /
  data  cblock(134)  / 'soli  ' /, ncbarr(134)  /  1 /
  data  cblser(134)  / 'spac07' /, jbltyp(134)  / 0 /
  data  cblock(135)  / 'ich1  ' /, ncbarr(135)  /  1 /
  data  cblser(135)  / 'spac08' /, jbltyp(135)  / 0 /
  data  cblock(136)  / 'bnd   ' /, ncbarr(136)  /  9 /
  data  cblser(136)  / 'spac09' /, jbltyp(136)  / 0 /
  data  cblock(137)  / 'iloc  ' /, ncbarr(137)  / 23 /
  data  cblser(137)  / 'spac10' /, jbltyp(137)  / 0 /
  data  cblock(138)  / 'gnd   ' /, ncbarr(138)  / 23 /
  data  cblser(138)  / 'spac11' /, jbltyp(138)  / 0 /
  data  cblock(139)  / 'karray' /, ncbarr(139)  /  9 /
  data  cblser(139)  / 'c31b01' /, jbltyp(139)  / 0 /
  data  cblock(140)  / 'xdat  ' /, ncbarr(140)  / 71 /
  data  cblser(140)  / 'c39b01' /, jbltyp(140)  / 0 /
  data  cblock(141)  / 'ydat  ' /, ncbarr(141)  / 71 /
  data  cblser(141)  / 'c39b02' /, jbltyp(141)  / 0 /
  data  cblock(142)  / 'aphdat' /, ncbarr(142)  / 71 /
  data  cblser(142)  / 'c39b03' /, jbltyp(142)  / 0 /
  data  cblock(143)  / 'jndex ' /, ncbarr(143)  /  1 /
  data  cblser(143)  / 'c10b01' /, jbltyp(143)  / 0 /
  data  cblock(144)  / 'diagg ' /, ncbarr(144)  /  1 /
  data  cblser(144)  / 'c10b02' /, jbltyp(144)  / 0 /
  data  cblock(145)  / 'diabb ' /, ncbarr(145)  /  1 /
  data  cblser(145)  / 'c10b03' /, jbltyp(145)  / 0 /
  data  cblock(146)  / 'solrsv' /, ncbarr(146)  /  1 /
  data  cblser(146)  / 'c10b04' /, jbltyp(146)  / 0 /
  data  cblock(147)  / 'solisv' /, ncbarr(147)  /  1 /
  data  cblser(147)  / 'c10b05' /, jbltyp(147)  / 0 /
  data  cblock(148)  / 'gndd  ' /, ncbarr(148)  / 23 /
  data  cblser(148)  / 'c10b06' /, jbltyp(148)  / 0 /
  data  cblock(149)  / 'bndd  ' /, ncbarr(149)  / 23 /
  data  cblser(149)  / 'c10b07' /, jbltyp(149)  / 0 /
  data  cblock(150)  / 'nekfix' /, ncbarr(150)  /  4 /
  data  cblser(150)  / 'c10b08' /, jbltyp(150)  / 0 /
  data  cblock(151)  / 'fxtem1' /, ncbarr(151)  /  4 /
  data  cblser(151)  / 'c10b09' /, jbltyp(151)  / 0 /
  data  cblock(152)  / 'fxtem2' /, ncbarr(152)  /  4 /
  data  cblser(152)  / 'c10b10' /, jbltyp(152)  / 0 /
  data  cblock(153)  / 'fxtem3' /, ncbarr(153)  /  4 /
  data  cblser(153)  / 'c10b11' /, jbltyp(153)  / 0 /
  data  cblock(154)  / 'fxtem4' /, ncbarr(154)  /  4 /
  data  cblser(154)  / 'c10b12' /, jbltyp(154)  / 0 /
  data  cblock(155)  / 'fxtem5' /, ncbarr(155)  /  4 /
  data  cblser(155)  / 'c10b13' /, jbltyp(155)  / 0 /
  data  cblock(156)  / 'fxtem6' /, ncbarr(156)  /  4 /
  data  cblser(156)  / 'c10b14' /, jbltyp(156)  / 0 /
  data  cblock(157)  / 'fixbu1' /, ncbarr(157)  /  4 /
  data  cblser(157)  / 'c10b15' /, jbltyp(157)  / 0 /
  data  cblock(158)  / 'fixbu2' /, ncbarr(158)  /  4 /
  data  cblser(158)  / 'c10b16' /, jbltyp(158)  / 0 /
  data  cblock(159)  / 'fixbu3' /, ncbarr(159)  /  4 /
  data  cblser(159)  / 'c10b17' /, jbltyp(159)  / 0 /
  data  cblock(160)  / 'fixbu4' /, ncbarr(160)  /  4 /
  data  cblser(160)  / 'c10b18' /, jbltyp(160)  / 0 /
  data  cblock(161)  / 'fixbu5' /, ncbarr(161)  /  4 /
  data  cblser(161)  / 'c10b19' /, jbltyp(161)  / 0 /
  data  cblock(162)  / 'fixbu6' /, ncbarr(162)  /  4 /
  data  cblser(162)  / 'c10b20' /, jbltyp(162)  / 0 /
  data  cblock(163)  / 'fixbu7' /, ncbarr(163)  /  4 /
  data  cblser(163)  / 'c10b21' /, jbltyp(163)  / 0 /
  data  cblock(164)  / 'fixbu8' /, ncbarr(164)  /  4 /
  data  cblser(164)  / 'c10b22' /, jbltyp(164)  / 0 /
  data  cblock(165)  / 'fixbu9' /, ncbarr(165)  /  4 /
  data  cblser(165)  / 'c10b23' /, jbltyp(165)  / 0 /
  data  cblock(166)  / 'fixb10' /, ncbarr(166)  /  4 /
  data  cblser(166)  / 'c10b24' /, jbltyp(166)  / 0 /
  data  cblock(167)  / 'fixb11' /, ncbarr(167)  /  4 /
  data  cblser(167)  / 'c10b25' /, jbltyp(167)  / 0 /
  data  cblock(168)  / 'kndex ' /, ncbarr(168)  /  4 /
  data  cblser(168)  / 'c10b26' /, jbltyp(168)  / 0 /
  data  cblock(169)  / 'karray' /, ncbarr(169)  /  9 /
  data  cblser(169)  / 'c44b01' /, jbltyp(169)  / 0 /
  data  cblock(170)  / 'p     ' /, ncbarr(170)  / 75 /
  data  cblser(170)  / 'c44b02' /, jbltyp(170)  / 0 /
  data  cblock(171)  / 'z     ' /, ncbarr(171)  / 75 /
  data  cblser(171)  / 'c44b03' /, jbltyp(171)  / 0 /
  data  cblock(172)  / 'ic    ' /, ncbarr(172)  / 71 /
  data  cblser(172)  / 'c44b04' /, jbltyp(172)  / 0 /
  data  cblock(173)  / 'r     ' /, ncbarr(173)  / 71 /
  data  cblser(173)  / 'c44b05' /, jbltyp(173)  / 0 /
  data  cblock(174)  / 'emptd ' /, ncbarr(174)  / 71 /
  data  cblser(174)  / 'c44b06' /, jbltyp(174)  / 0 /
  data  cblock(175)  / 'gmd   ' /, ncbarr(175)  / 71 /
  data  cblser(175)  / 'c44b07' /, jbltyp(175)  / 0 /
  data  cblock(176)  / 'x     ' /, ncbarr(176)  / 71 /
  data  cblser(176)  / 'c44b08' /, jbltyp(176)  / 0 /
  data  cblock(177)  / 'y     ' /, ncbarr(177)  / 71 /
  data  cblser(177)  / 'c44b09' /, jbltyp(177)  / 0 /
  data  cblock(178)  / 'tb2   ' /, ncbarr(178)  / 71 /
  data  cblser(178)  / 'c44b10' /, jbltyp(178)  / 0 /
  data  cblock(179)  / 'itb3  ' /, ncbarr(179)  / 71 /
  data  cblser(179)  / 'c44b11' /, jbltyp(179)  / 0 /
  data  cblock(180)  / 'workr1' /, ncbarr(180)  / 71 /
  data  cblser(180)  / 'c44b12' /, jbltyp(180)  / 0 /
  data  cblock(181)  / 'workr2' /, ncbarr(181)  / 71 /
  data  cblser(181)  / 'c44b13' /, jbltyp(181)  / 0 /
  data  cblock(182)  / 'text  ' /, ncbarr(182)  / 76 /
  data  cblser(182)  / 'c44b14' /, jbltyp(182)  / 1 /
  data  cblock(183)  / 'gd    ' /, ncbarr(183)  / 74 /
  data  cblser(183)  / 'c44b15' /, jbltyp(183)  / 0 /
  data  cblock(184)  / 'bd    ' /, ncbarr(184)  / 74 /
  data  cblser(184)  / 'c44b16' /, jbltyp(184)  / 0 /
  data  cblock(185)  / 'yd    ' /, ncbarr(185)  / 74 /
  data  cblser(185)  / 'c44b17' /, jbltyp(185)  / 0 /
  data  cblock(186)  / 'itbic ' /, ncbarr(186)  / 73 /
  data  cblser(186)  / 'c44b18' /, jbltyp(186)  / 0 /
  data  cblock(187)  / 'tbr   ' /, ncbarr(187)  / 73 /
  data  cblser(187)  / 'c44b19' /, jbltyp(187)  / 0 /
  data  cblock(188)  / 'tbd   ' /, ncbarr(188)  / 73 /
  data  cblser(188)  / 'c44b20' /, jbltyp(188)  / 0 /
  data  cblock(189)  / 'tbg   ' /, ncbarr(189)  / 73 /
  data  cblser(189)  / 'c44b21' /, jbltyp(189)  / 0 /
  data  cblock(190)  / 'tbx   ' /, ncbarr(190)  / 73 /
  data  cblser(190)  / 'c44b22' /, jbltyp(190)  / 0 /
  data  cblock(191)  / 'tby   ' /, ncbarr(191)  / 73 /
  data  cblser(191)  / 'c44b23' /, jbltyp(191)  / 0 /
  data  cblock(192)  / 'tbtb2 ' /, ncbarr(192)  / 73 /
  data  cblser(192)  / 'c44b24' /, jbltyp(192)  / 0 /
  data  cblock(193)  / 'itbtb3' /, ncbarr(193)  / 73 /
  data  cblser(193)  / 'c44b25' /, jbltyp(193)  / 0 /
  data  cblock(194)  / 'tbtext' /, ncbarr(194)  / 73 /
  data  cblser(194)  / 'c44b26' /, jbltyp(194)  / 1 /
  data  cblock(195)  / 'karray' /, ncbarr(195)  /  9 /
  data  cblser(195)  / 'c45b01' /, jbltyp(195)  / 0 /
  data  cblock(196)  / 'karray' /, ncbarr(196)  /  9 /
  data  cblser(196)  / 'c47b01' /, jbltyp(196)  / 0 /
  data  char(1)   / 'i' /
  data  char(2)   / 'j' /
  data  char(3)   / 'k' /
  data  char(4)   / 'l' /
  data  char(5)   / 'm' /
  data  char(6)   / 'n' /
  data  type(1,1)  / 'real*8' /
  data  type(1,2)  / '      ' /
  data  type(1,3)  / '      ' /
  data  type(2,1)  / 'comple' /
  data  type(2,2)  / 'x* 8  ' /
  data  type(2,3)  / '      ' /
  data  type(3,1)  / 'real*8' /
  data  type(3,2)  / '      ' /
  data  type(3,3)  / '      ' /
  data  type(4,1)  / 'intege' /
  data  type(4,2)  / 'r*4   ' /
  data  type(4,3)  / '      ' /
  numlst = 28
  lunit(2) = 7
  lunit(3) = 8
  lunit(5) = 5
  lunit(6) = 6
  mulvar(1)  =  2
  mulvar(2)  =  2
  mulvar(3)  =  2
  mulvar(4)  =  1
  open (unit = lunit(2), iostat = ios, form = 'formatted')
  if (ios .eq. 0) then
     open (unit = lunit(3), iostat = ios, form = 'formatted')
     if (ios .eq. 0) then
        indexm = 0
        mextra = 0
        !     one less than the number of  'vardim'  modules.
        numkex = 7
        !     default values for extra offsets of  'vardim'
        !     modules belonging to  non-solution overlays.
        do i = 1, numkex
5245       kextra(i) = 0
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
        read (lunit(5), 5287, iostat = ios) (lstnew(i), i = 1, numlst)
5287    format (10i8)
        write (lunit(6), 5263)
5263    format (/,  " Pseudo-listing of data cards which have been read by the variable-dimensioning program  'vardim' .   Only  ",/, &
             " if all data fields are punched with  'clean'  i8  integer information will this be a true listing.   Data cards", /, &
             ' are in fact read in and then printed out using integer variables and  10i8  format.')
        write (lunit(6), 5264) (i, i = 1, 8), (lstnew(i), i = 1, numlst)
5264    format (1x, 111('-'), /, 31x, '0', 8(9x, i1 ), /, 31x, '0', 8(9x, '0'), /, 31x, 80('-'), /, ' 1st card (lists  1-10).', 7x, '1', 10i8 ,/, &
             ' 2nd card (lists 11-20).', 7x, '1', 10i8, /, ' 3rd card (lists 21-30).', 7x, '1', 10i8)
        if (lstnew(1) / 10000000 .ne. 9) go to 5294
        read (lunit(5), 5287, iostat = ios) (kextra(i), i = 1, numkex)
        write (lunit(6), 5378) (kextra(i), i = 1, numkex)
5378    format (' Supplemental offsets.', 9x, '1', 10i8)
        lstnew(1) = lstnew(1) - 90000000
5294    if (lstnew(11) / 10000000 .ne. 9) go to 5297
        n4 = lstnew(11) - 90000000
        do j = 1, numlst
5296       lstnew(j) = lstdef(j) * n4
        end do
5297    if (lstnew(1) .gt. 0) lstnew(1) = lstnew(1) + 2
        write (lunit(6), 5381)
5381    format (1x, 111('-'))
        do i = 1, numlst
           n1 = i
           if (lstnew(i) .ge. 1000000) go to 9000
           if (lstnew(i) .le. 0) lstnew(i) = lstdef(i)
5326    end do
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
        do i = 1, 126
           n9 = ncbarr(i)
           if (n9 .eq. 0 .or. n9 .eq. 98) go to 4301
           n37 = 3
           bus1 = cblock(i)
           read (unit = bus1, fmt = 8104) bus2
8104       format (a1)
           do j = 1, 6
              if (bus2 .eq. char(j)) n37 = 4
4701       end do
           if (jbltyp(i) .ne. 0 ) n37 = jbltyp(i)
           mtot = mtot + mulvar(n37) * lstnew(n9)
4301    end do
        write (lunit(2), 4190)
4190    format ('!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-', / &
             '!', /, '!     file: newmods.for', /, '!', /, '!', /, '!     subroutine main10.', /, '!')
        call make_subroutine_comment(lunit(2), 'main10')
        write (lunit(2), 4201)
4201    format ('subroutine main10', /, 2x, 'implicit real*8 (a-h, o-z), integer*4 (i-n)')
        do ii = 1, 126
           i = 0 + ii
           n3 = ncbarr(i)
           nonneg = lstnew(n3)
           if (nonneg .le. 0) nonneg = 1
           write (lunit(2), 7236) cblser(i), cblock(i), nonneg
7236       format (2x, 'common /', a6, '/   ', a6, '(', i8, ' )', 38x)
           n4 = jbltyp(i)
           if (n4 .eq. 0) go to 4101
           write (lunit(2), 7243) (type(n4,j), j = 1, 3), cblock(i)
7243       format (2x, 3a6, a6, 50x)
4101    end do
        write (lunit(2), 7286)
7286    format (2x, 'call subr10', 63x)
        write (lunit(2), 7297)
7297    format (2x, 'return', 68x)
        write (lunit(2), 7298) 'main10'
7298    format ('end subroutine ', a)
        ltlabl = mtot
        if ( ltlabl  .gt.  9999999 )   go to 9000
        indexm = indexm + 1
        mul34 = mulvar(3) / mulvar(4)
        mextra = 4000
        lstnew(98) = ltlabl
        call make_subroutine_comment(lunit(3), 'dimens')
        write (lunit(3), 8116)
8116    format ('subroutine dimens(lsize, nchain, bus1, bus2)', /, 2x, 'implicit real*8 (a-h, o-z)')
        write (lunit(3), 8120)
8120    format (2x, 'dimension lsize(62)')
        write (lunit(3), 8124)
8124    format (2x, 'real          bus1, bus2')
        write (lunit(3), 8134)
8134    format (2x, 'if (nchain .ge. 29) go to 2900')
        do i = 1, numlst
8137       write (lunit(3), 8143) i, lstnew(i)
        end do
8143    format (2x, 'lsize(', i2, ')  =', i8)
        write (lunit(3), 8155) numlst
8155    format (2x, 'n7 =', i3, ' + 1')
        write (lunit(3), 8159) ltlabl
8159    format (2x, 'lsize(n7) =', i8)
        nrec3 = numlst + 7
        call time44(texta)
        call date44(textb)
        write (ansi16, 3672) texta, textb
3672    format (4a4)
        read (ansi16, 3676) nh, nm, ns, nm, nd, ny
3676    format (i2, 1x, i2, 1x, 2i2, 1x, i2, 1x, i2)
        ntime = 10000 * nh + 100 * nm + ns
        ndate = 10000 * nm + 100 * nd + ny
        write (lunit(3), 3684) ntime, ndate
3684    format (2x, 'bus1 =', i8, /, 6x, 'bus2 =', i8)
        mtot = 0
        do i = 127, 138
           n9 = ncbarr(i)
           if (n9 .eq. 0 .or. n9 .eq. 98) go to 4302
           n37 = 3
           bus1 = cblock(i)
           read (unit = bus1, fmt = 8104) bus2
           do j = 1, 6
              if (bus2 .eq. char(j)) n37 = 4
4702       end do
           if (jbltyp(i) .ne. 0) n37 = jbltyp(i)
           mtot = mtot + mulvar(n37) * lstnew(n9)
4302    end do
        lstnew(99) = ltlabl + kextra(1)
        if (lstnew(99) .le. 0) lstnew(99) = 1
        ncbarr(127) = 99
        call make_subroutine_comment(lunit(2), 'over29')
        write (lunit(2), 4202)
4202    format ('subroutine over29', /, 2x, 'implicit real*8 (a-h, o-z),  integer*4 (i-n)')
        write (lunit(3), 8156)
8156    format (2x, 'return')
        lm = 0
        n86 = 29
        n87 = 31
        write (lunit(3), 8158) n86, n86, n87
        nrec3 = nrec3 + 2
8158    format (i2.2, '00 if (nchain .gt.', i4, ') go to ', i2.2, '00')
        do ii = 1, 12
           i = 126  +  ii
           n3 = ncbarr(i)
           if ( ii  .eq.  1 )   go to 4502
           n7 = ii - 1
           do kk = 1, n7
              if (ncbarr(kk+126) .eq. n3) go to 4602
4402       end do
4502       lm = lm + 1
           n28 = n3
           if (n3 .eq. 99) n28 = 0
           write (lunit(3), 8143) lm, n28
           lm = lm + 1
           write (lunit(3), 4769) lm, lstnew(n3)
4769       format (2x, 'lsize(', i2, ') =', i7)
           nrec3 = nrec3 + 2
4602       continue
           nonneg = lstnew(n3)
           if (nonneg .le. 0) nonneg = 1
           write (lunit(2), 7236) cblser(i), cblock(i), nonneg
           n4 = jbltyp(i)
           if (n4 .eq. 0) go to 4102
           write (lunit(2), 7243) (type(n4, j), j = 1, 3), cblock(i)
4102    end do
        n18 = 29
        write (lunit(2), 7274) n18
7274    format (2x, 'call subr', i2, 63x)
        write (lunit(2), 7297)
        write (lunit(2), 7298) 'over29'
        indexm = indexm + 1
        mextra = 5000
        call make_subroutine_comment(lunit(2), 'over31')
        write (lunit(2), 4203)
4203    format ('subroutine over31', /, 2x, 'implicit real*8 (a-h, o-z),  integer*4 (i-n)')
        write (lunit(3), 8156)
        lm = 0
        n86 = 31
        n87 = 39
        write (lunit(3), 8158) n86, n86, n87
        nrec3 = nrec3 + 2
        do ii = 1, 1
           i = 138 + ii
           n3 = ncbarr(i)
           if (ii .eq. 1) go to 4503
           n7 = ii - 1
           do kk = 1, n7
              if (ncbarr(kk+138) .eq. n3) go to 4603
4403       end do
4503       lm = lm + 1
           n28 = n3
           if (n3 .eq. 99) n28 = 0
           write (lunit(3), 8143) lm, n28
           lm = lm + 1
           write (lunit(3), 4769) lm, lstnew(n3)
           nrec3 = nrec3 + 2
4603       continue
           nonneg = lstnew(n3)
           if (nonneg .le. 0) nonneg = 1
           write (lunit(2), 7236) cblser(i), cblock(i), nonneg
           n4 = jbltyp(i)
           if (n4 .eq. 0) go to 4103
           write (lunit(2), 7243) (type(n4, j), j = 1, 3), cblock(i)
4103    end do
        n18 = 31
        write (lunit(2), 7274) n18
        write (lunit(2), 7297)
        write (lunit(2), 7298) 'over31'
        lstnew(71) = 500
        if (lstnew(3) .gt. 500) lstnew(71) = lstnew(3)
        call make_subroutine_comment(lunit(2), 'over39')
        write (lunit(2), 4204)
4204    format ('subroutine over39', /, 2x, 'implicit real*8 (a-h, o-z),  integer*4 (i-n)')
        write (lunit(3), 8156)
        lm = 0
        n86 = 39
        n87 = 10
        write (lunit(3), 8158) n86, n86, n87
        nrec3 = nrec3 + 2
        do ii = 1, 3
           i = 139  +  ii
           n3 = ncbarr(i)
           if (ii .eq. 1) go to 4504
           n7 = ii - 1
           do kk = 1, n7
              if (ncbarr(kk+139) .eq. n3) go to 4604
4404       end do
4504       lm = lm + 1
           n28 = n3
           if (n3 .eq. 99) n28 = 0
           write (lunit(3), 8143) lm, n28
           lm = lm + 1
           write (lunit(3), 4769) lm, lstnew(n3)
           nrec3 = nrec3 + 2
4604       continue
           nonneg = lstnew(n3)
           if (nonneg .le. 0) nonneg = 1
           write (lunit(2), 7236) cblser(i), cblock(i), nonneg
           n4 = jbltyp(i)
           if (n4 .eq. 0) go to 4104
           write (lunit(2), 7243) (type(n4, j), j = 1, 3), cblock(i)
4104    end do
        n18 = 39
        write (lunit(2), 7274) n18
        write (lunit(2), 7297)
        write (lunit(2), 7298) 'over39'
        call make_subroutine_comment(lunit(2), 'fixs10')
        write (lunit(2), 4205)
4205    format ('subroutine fixs10', /, 2x, 'implicit real*8 (a-h, o-z),  integer*4 (i-n)')
        write (lunit(3), 8156)
        lm = 0
        n86 = 10
        n87 = 44
        write (lunit(3), 8158) n86, n86, n87
        nrec3 = nrec3 + 2
        do ii = 1, 26
           i = 142  +  ii
           n3 = ncbarr(i)
           if (ii .eq. 1) go to 4505
           n7 = ii - 1
           do kk = 1, n7
              if (ncbarr(kk + 142) .eq. n3) go to 4605
4405       end do
4505       lm = lm + 1
           n28 = n3
           if (n3 .eq. 99) n28 = 0
           write (lunit(3), 8143) lm, n28
           lm = lm + 1
           write (lunit(3), 4769) lm, lstnew(n3)
           nrec3 = nrec3 + 2
4605       continue
           nonneg = lstnew(n3)
           if ( nonneg  .le.  0 )   nonneg = 1
           write (lunit(2), 7236) cblser(i), cblock(i), nonneg
           n4 = jbltyp(i)
           if ( n4  .eq.  0 )   go to 4105
           write (lunit(2), 7243) (type(n4, j), j= 1, 3), cblock(i)
4105    end do
        n18 = 10
        write (lunit(2), 7274) n18
        write (lunit(2), 7297)
        write (lunit(2), 7298) 'fixs10'
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
        lstnew(71) = lphd2 * 2
        lstnew(73) = lstnew(71) + 1
        lstnew(74) = (lphd2 * (lphd2 + 1)) / 2
        lstnew(75) = (lstnew(71) * (lstnew(71) + 1)) / 2
        lstnew(76) = 2 * lstnew(71)
        call make_subroutine_comment(lunit(2), 'over44')
        write (lunit(2), 4206)
4206    format ('subroutine over44', /, 2x, 'implicit real*8 (a-h, o-z),  integer*4 (i-n)')
        write (lunit(3), 8156)
        lm = 0
        n86 = 44
        n87 = 45
        write (lunit(3), 8158) n86, n86, n87
        nrec3 = nrec3 + 2
        do ii = 1, 26
           i = 168 + ii
           n3 = ncbarr(i)
           if (ii .eq. 1) go to 4506
           n7 = ii - 1
           do kk = 1, n7
              if (ncbarr(kk + 168) .eq. n3) go to 4606
4406       end do
4506       lm = lm + 1
           n28 = n3
           if (n3 .eq. 99) n28 = 0
           write (lunit(3), 8143) lm, n28
           lm = lm + 1
           write (lunit(3), 4769) lm, lstnew(n3)
           nrec3 = nrec3 + 2
4606       continue
           nonneg = lstnew(n3)
           if (nonneg .le. 0) nonneg = 1
           write (lunit(2), 7236) cblser(i), cblock(i), nonneg
           n4 = jbltyp(i)
           if (n4 .eq. 0) go to 4106
           write (lunit(2), 7243) (type(n4, j), j = 1, 3), cblock(i)
4106    end do
        n18 = 44
        write (lunit(2), 7274) n18
        write (lunit(2), 7297)
        write (lunit(2), 7298) 'over44'
        indexm = indexm + 1
        mextra = 5000
        call make_subroutine_comment(lunit(2), 'over45')
        write (lunit(2), 4207)
4207    format ('subroutine over45', /, 2x, 'implicit real*8 (a-h, o-z),  integer*4 (i-n)')
        write (lunit(3), 8156)
        lm = 0
        n86 = 45
        n87 = 47
        write (lunit(3), 8158) n86, n86, n87
        nrec3 = nrec3 + 2
        do ii = 1, 1
           i = 194  +  ii
           n3 = ncbarr(i)
           if ( ii  .eq.  1 )   go to 4507
           n7 = ii - 1
           do kk = 1, n7
              if ( ncbarr(kk+194)  .eq.  n3 )   go to 4607
4407       end do
4507       lm = lm + 1
           n28 = n3
           if ( n3  .eq.  99 )   n28 = 0
           write (lunit(3), 8143) lm, n28
           lm = lm + 1
           write (lunit(3), 4769) lm, lstnew(n3)
           nrec3 = nrec3 + 2
4607       continue
           nonneg = lstnew(n3)
           if (nonneg .le. 0) nonneg = 1
           write (lunit(2), 7236) cblser(i), cblock(i), nonneg
           n4 = jbltyp(i)
           if ( n4  .eq.  0 )   go to 4107
           write (lunit(2), 7243) (type(n4, j), j = 1, 3), cblock(i)
4107    end do
        n18 = 45
        write (lunit(2), 7274) n18
        write (lunit(2), 7297)
        write (lunit(2), 7298) 'over45'
        indexm = indexm + 1
        mextra = 5000
        call make_subroutine_comment(lunit(2), 'over47')
        write (lunit(2), 4208)
4208    format ('subroutine over47', /, 2x, 'implicit real*8 (a-h, o-z),  integer*4 (i-n)')
        write (lunit(3), 8156)
        lm = 0
        n86 = 47
        n87 = 99
        write (lunit(3), 8158) n86, n86, n87
        nrec3 = nrec3 + 2
        do ii = 1, 1
           i = 195  +  ii
           n3 = ncbarr(i)
           if ( ii  .eq.  1 )   go to 4508
           n7 = ii - 1
           do kk = 1, n7
              if ( ncbarr(kk+195)  .eq.  n3 )   go to 4608
4408       end do
4508       lm = lm + 1
           n28 = n3
           if ( n3  .eq.  99 )   n28 = 0
           write (lunit(3), 8143) lm, n28
           lm = lm + 1
           write (lunit(3), 4769) lm, lstnew(n3)
           nrec3 = nrec3 + 2
4608       continue
           nonneg = lstnew(n3)
           if ( nonneg  .le.  0 )   nonneg = 1
           write (lunit(2), 7236) cblser(i), cblock(i), nonneg
           n4 = jbltyp(i)
           if ( n4  .eq.  0 )   go to 4108
           write (lunit(2), 7243) (type(n4, j), j = 1, 3), cblock(i)
4108    end do
        n18 = 47
        write (lunit(2), 7274) n18
        write (lunit(2), 7297)
        write (lunit(2), 7298) 'over47'
        write (lunit(3), 8156)
        write (lunit(3), 8234)
8234    format (' 9900 lsize(1) = locint(bus1) - locint(bus2)')
        write (lunit(3), 8156)
        !        write (lunit(3), 8242)
        !8242    format (2x, 'end subroutine dimens')
        write (lunit(3), 7298) 'dimens'
        nrec3 = nrec3 + 4
        rewind lunit(3)
        do m = 1, 99999
           read (lunit(3), 8253, end = 8260) abuff
           write (lunit(2), 8253) trim(abuff)
        end do
8253    format (a)
8260    write (lunit(2), 8258)
8258    format ('!', /, '! end of file newmods.for', /, '!')
        write (lunit(6), 8942) ltlabl
8942    format (" Normal termination within  'vardim' .   The size of   /label/   equals", i8, '   integer words.', //, 1x)
        go to 9999
9000    write (lunit(6), 9023)
9023    format (//, 1x)
        write (lunit(6), 9027)
9027    format (1x, 131('-'))
        write (lunit(6), 9031)
9031    format (22(' error'))
        write (lunit(6), 9031)
        write (lunit(6), 9027)
        write (lunit(6), 9044)
9044    format (/, 5x, 'The user is in trouble, unless he has made a data-punching error.   Variable-dimensioning of the EMTP has', /, &
             5x, "broken down within the separate variable-dimensioning program  'vardim' .   In particular, one or more of the", /, &
             5x, 'user-inputted list sizes must be rejected as being illegally large.   User-supplied limits (or default limits,')
        write (lunit(6), 9045) (lstnew(i), i = 1, numlst)
9045    format (5x, 'for any non-positive data fields) are as follows ....', /, (1x, 10i10))
        if ( ltlabl  .gt.  0 )   go to 9061
        write (lunit(6), 9049) n1, lstnew(n1)
9049    format (5x, 'Specifically, the user-supplied value which was read for list number', i4, '  exceeds  999999 ,', /, &
             5x, 'which is unreal.   A value of', i10, "   was read from the user's data card for this list.", /, 1x)
9054    write (lunit(6), 9027)
        write (lunit(6), 9031)
        write (lunit(6), 9031)
        write (lunit(6), 9027)
        write (lunit(6), 9023)
        go to 9900
9061    write (lunit(6), 9064) ltlabl
9064    format (5x, 'Specifically, the user-supplied dimensions have collectively produced a total labeled-common allocation', /, &
             5x, 'of', i12, '   words, which is unreal.   A limit of   9999999   has arbitrarily been imposed by the better judgment')
        write (lunit(6), 9065)
9065    format (5x, 'of the program.   A machine this big (with this much real central memory) is not known to be in existence anywhere', /, &
             5x, 'in the world.', /, 1x)
        go to 9054
9900    continue
        close (unit = lunit(2), status = 'delete' )
     end if
     close (unit = lunit(3), status = 'delete' )
  endif
9999 stop
end program vardim
!
! subroutine make_comment.
!
subroutine make_subroutine_comment(unit, name)
  implicit  real(8) (a-h, o-z), integer(4) (i-n)
  integer(4) unit
  character(*) name
  write (unit, 10) trim(name)
10 format ('!', /, '! subroutine ', a, '.', /, '!')
  return
end subroutine make_subroutine_comment
!
!     end of file: vardim.for
!
