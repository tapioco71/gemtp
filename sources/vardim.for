c-*- mode: fortran; syntax: ansi-fortran-77; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
c
c     file: vardim.for
c
      implicit  real*8 (a-h, o-z),  integer*4 (i-n)
      character*8   bus1(1), texta(2), textb(2), cblock
      character*16  ansi16
      dimension  cblock(300),  ncbarr(300),  cblser(300),  jbltyp(300)
      dimension  lstnew(99),  lstdef(49),  type(4,3),  kextra(29)
      dimension  abuff(14), char(6), mulvar(4)
      data  cblock(  1)  / 6hx      /,          ncbarr(  1)  /  3 /
        data  cblser(  1)  / 6hc0b001 /,          jbltyp(  1)  / 0 /
      data  cblock(  2)  / 6hykm    /,          ncbarr(  2)  /  5 /
        data  cblser(  2)  / 6hc0b002 /,          jbltyp(  2)  / 0 /
      data  cblock(  3)  / 6hkm     /,          ncbarr(  3)  /  5 /
        data  cblser(  3)  / 6hc0b003 /,          jbltyp(  3)  / 0 /
      data  cblock(  4)  / 6hxk     /,          ncbarr(  4)  / 72 /
        data  cblser(  4)  / 6hc0b004 /,          jbltyp(  4)  / 0 /
      data  cblock(  5)  / 6hxm     /,          ncbarr(  5)  / 72 /
        data  cblser(  5)  / 6hc0b005 /,          jbltyp(  5)  / 0 /
      data  cblock(  6)  / 6hweight /,          ncbarr(  6)  / 14 /
        data  cblser(  6)  / 6hc0b006 /,          jbltyp(  6)  / 0 /
      data  cblock(  7)  / 6hiwtent /,          ncbarr(  7)  / 52 /
        data  cblser(  7)  / 6hc0b007 /,          jbltyp(  7)  / 0 /
      data  cblock(  8)  / 6hcon1   /,          ncbarr(  8)  / 51 /
        data  cblser(  8)  / 6hc0b008 /,          jbltyp(  8)  / 0 /
      data  cblock(  9)  / 6hiskip  /,          ncbarr(  9)  / 13 /
        data  cblser(  9)  / 6hc0b009 /,          jbltyp(  9)  / 0 /
      data  cblock( 10)  / 6hzinf   /,          ncbarr( 10)  / 13 /
        data  cblser( 10)  / 6hc0b010 /,          jbltyp( 10)  / 0 /
      data  cblock( 11)  / 6heta    /,          ncbarr( 11)  / 13 /
        data  cblser( 11)  / 6hc0b011 /,          jbltyp( 11)  / 0 /
      data  cblock( 12)  / 6hnhist  /,          ncbarr( 12)  / 13 /
        data  cblser( 12)  / 6hc0b012 /,          jbltyp( 12)  / 0 /
      data  cblock( 13)  / 6hstailm /,          ncbarr( 13)  / 15 /
        data  cblser( 13)  / 6hc0b013 /,          jbltyp( 13)  / 0 /
      data  cblock( 14)  / 6hstailk /,          ncbarr( 14)  / 15 /
        data  cblser( 14)  / 6hc0b014 /,          jbltyp( 14)  / 0 /
      data  cblock( 15)  / 6hxmax   /,          ncbarr( 15)  / 58 /
        data  cblser( 15)  / 6hc0b015 /,          jbltyp( 15)  / 0 /
      data  cblock( 16)  / 6hkoutvp /,          ncbarr( 16)  / 62 /
        data  cblser( 16)  / 6hc0b016 /,          jbltyp( 16)  / 0 /
      data  cblock( 17)  / 6hbnrg   /,          ncbarr( 17)  / 18 /
        data  cblser( 17)  / 6hc0b017 /,          jbltyp( 17)  / 0 /
      data  cblock( 18)  / 6hsconst /,          ncbarr( 18)  / 20 /
        data  cblser( 18)  / 6hc0b018 /,          jbltyp( 18)  / 0 /
      data  cblock( 19)  / 6hcnvhst /,          ncbarr( 19)  / 73 /
        data  cblser( 19)  / 6hc0b019 /,          jbltyp( 19)  / 0 /
      data  cblock( 20)  / 6hsfd    /,          ncbarr( 20)  / 71 /
        data  cblser( 20)  / 6hc0b020 /,          jbltyp( 20)  / 0 /
      data  cblock( 21)  / 6hqfd    /,          ncbarr( 21)  / 71 /
        data  cblser( 21)  / 6hc0b021 /,          jbltyp( 21)  / 0 /
      data  cblock( 22)  / 6hsemaux /,          ncbarr( 22)  / 22 /
        data  cblser( 22)  / 6hc0b022 /,          jbltyp( 22)  / 0 /
      data  cblock( 23)  / 6hibsout /,          ncbarr( 23)  / 12 /
        data  cblser( 23)  / 6hc0b023 /,          jbltyp( 23)  / 0 /
      data  cblock( 24)  / 6hbvalue /,          ncbarr( 24)  / 12 /
        data  cblser( 24)  / 6hc0b024 /,          jbltyp( 24)  / 0 /
      data  cblock( 25)  / 6hsptacs /,          ncbarr( 25)  / 19 /
        data  cblser( 25)  / 6hc0b025 /,          jbltyp( 25)  / 0 /
      data  cblock( 26)  / 6hkswtyp /,          ncbarr( 26)  /  6 /
        data  cblser( 26)  / 6hc0b026 /,          jbltyp( 26)  / 0 /
      data  cblock( 27)  / 6hmodswt /,          ncbarr( 27)  /  6 /
        data  cblser( 27)  / 6hc0b027 /,          jbltyp( 27)  / 0 /
      data  cblock( 28)  / 6hkbegsw /,          ncbarr( 28)  /  6 /
        data  cblser( 28)  / 6hc0b028 /,          jbltyp( 28)  / 0 /
      data  cblock( 29)  / 6hlastsw /,          ncbarr( 29)  /  6 /
        data  cblser( 29)  / 6hc0b029 /,          jbltyp( 29)  / 0 /
      data  cblock( 30)  / 6hkentnb /,          ncbarr( 30)  /  6 /
        data  cblser( 30)  / 6hc0b030 /,          jbltyp( 30)  / 0 /
      data  cblock( 31)  / 6hnbhdsw /,          ncbarr( 31)  / 63 /
        data  cblser( 31)  / 6hc0b031 /,          jbltyp( 31)  / 0 /
      data  cblock( 32)  / 6htopen  /,          ncbarr( 32)  / 60 /
        data  cblser( 32)  / 6hc0b032 /,          jbltyp( 32)  / 0 /
      data  cblock( 33)  / 6hcrit   /,          ncbarr( 33)  /  6 /
        data  cblser( 33)  / 6hc0b033 /,          jbltyp( 33)  / 0 /
      data  cblock( 34)  / 6hkdepsw /,          ncbarr( 34)  / 60 /
        data  cblser( 34)  / 6hc0b034 /,          jbltyp( 34)  / 0 /
      data  cblock( 35)  / 6htdns   /,          ncbarr( 35)  /  6 /
        data  cblser( 35)  / 6hc0b035 /,          jbltyp( 35)  / 0 /
      data  cblock( 36)  / 6hisourc /,          ncbarr( 36)  /  6 /
        data  cblser( 36)  / 6hc0b036 /,          jbltyp( 36)  / 0 /
      data  cblock( 37)  / 6henergy /,          ncbarr( 37)  /  6 /
        data  cblser( 37)  / 6hc0b037 /,          jbltyp( 37)  / 0 /
      data  cblock( 38)  / 6hiardub /,          ncbarr( 38)  / 63 /
        data  cblser( 38)  / 6hc0b038 /,          jbltyp( 38)  / 0 /
      data  cblock( 39)  / 6hardube /,          ncbarr( 39)  / 64 /
        data  cblser( 39)  / 6hc0b039 /,          jbltyp( 39)  / 0 /
      data  cblock( 40)  / 6hnonlad /,          ncbarr( 40)  /  9 /
        data  cblser( 40)  / 6hc0b040 /,          jbltyp( 40)  / 0 /
      data  cblock( 41)  / 6hnonle  /,          ncbarr( 41)  /  9 /
        data  cblser( 41)  / 6hc0b041 /,          jbltyp( 41)  / 0 /
      data  cblock( 42)  / 6hvnonl  /,          ncbarr( 42)  /  9 /
        data  cblser( 42)  / 6hc0b042 /,          jbltyp( 42)  / 0 /
      data  cblock( 43)  / 6hcurr   /,          ncbarr( 43)  /  9 /
        data  cblser( 43)  / 6hc0b043 /,          jbltyp( 43)  / 0 /
      data  cblock( 44)  / 6hanonl  /,          ncbarr( 44)  /  9 /
        data  cblser( 44)  / 6hc0b044 /,          jbltyp( 44)  / 0 /
      data  cblock( 45)  / 6hvecnl1 /,          ncbarr( 45)  /  9 /
        data  cblser( 45)  / 6hc0b045 /,          jbltyp( 45)  / 0 /
      data  cblock( 46)  / 6hvecnl2 /,          ncbarr( 46)  /  9 /
        data  cblser( 46)  / 6hc0b046 /,          jbltyp( 46)  / 0 /
      data  cblock( 47)  / 6hnamenl /,          ncbarr( 47)  /  9 /
        data  cblser( 47)  / 6hc0b047 /,          jbltyp( 47)  / 0 /
      data  cblock( 48)  / 6hvzero  /,          ncbarr( 48)  /  9 /
        data  cblser( 48)  / 6hc0b048 /,          jbltyp( 48)  / 0 /
      data  cblock( 49)  / 6hilast  /,          ncbarr( 49)  /  9 /
        data  cblser( 49)  / 6hc0b049 /,          jbltyp( 49)  / 0 /
      data  cblock( 50)  / 6hnltype /,          ncbarr( 50)  /  9 /
        data  cblser( 50)  / 6hc0b050 /,          jbltyp( 50)  / 0 /
      data  cblock( 51)  / 6hkupl   /,          ncbarr( 51)  /  9 /
        data  cblser( 51)  / 6hc0b051 /,          jbltyp( 51)  / 0 /
      data  cblock( 52)  / 6hnlsub  /,          ncbarr( 52)  /  9 /
        data  cblser( 52)  / 6hc0b052 /,          jbltyp( 52)  / 0 /
      data  cblock( 53)  / 6hxoptbr /,          ncbarr( 53)  /  2 /
        data  cblser( 53)  / 6hc0b053 /,          jbltyp( 53)  / 0 /
      data  cblock( 54)  / 6hcoptbr /,          ncbarr( 54)  /  2 /
        data  cblser( 54)  / 6hc0b054 /,          jbltyp( 54)  / 0 /
      data  cblock( 55)  / 6hcursub /,          ncbarr( 55)  / 53 /
        data  cblser( 55)  / 6hc0b055 /,          jbltyp( 55)  / 0 /
      data  cblock( 56)  / 6hcchar  /,          ncbarr( 56)  / 10 /
        data  cblser( 56)  / 6hc0b056 /,          jbltyp( 56)  / 0 /
      data  cblock( 57)  / 6hvchar  /,          ncbarr( 57)  / 10 /
        data  cblser( 57)  / 6hc0b057 /,          jbltyp( 57)  / 0 /
      data  cblock( 58)  / 6hgslope /,          ncbarr( 58)  / 10 /
        data  cblser( 58)  / 6hc0b058 /,          jbltyp( 58)  / 0 /
      data  cblock( 59)  / 6hktrans /,          ncbarr( 59)  /  1 /
        data  cblser( 59)  / 6hc0b059 /,          jbltyp( 59)  / 0 /
      data  cblock( 60)  / 6hkk     /,          ncbarr( 60)  /  1 /
        data  cblser( 60)  / 6hc0b060 /,          jbltyp( 60)  / 0 /
      data  cblock( 61)  / 6hc      /,          ncbarr( 61)  /  3 /
        data  cblser( 61)  / 6hc0b061 /,          jbltyp( 61)  / 0 /
      data  cblock( 62)  / 6htr     /,          ncbarr( 62)  /  5 /
        data  cblser( 62)  / 6hc0b062 /,          jbltyp( 62)  / 0 /
      data  cblock( 63)  / 6htx     /,          ncbarr( 63)  /  5 /
        data  cblser( 63)  / 6hc0b063 /,          jbltyp( 63)  / 0 /
      data  cblock( 64)  / 6hr      /,          ncbarr( 64)  /  3 /
        data  cblser( 64)  / 6hc0b064 /,          jbltyp( 64)  / 0 /
      data  cblock( 65)  / 6hnr     /,          ncbarr( 65)  /  2 /
        data  cblser( 65)  / 6hc0b065 /,          jbltyp( 65)  / 0 /
      data  cblock( 66)  / 6hlength /,          ncbarr( 66)  /  2 /
        data  cblser( 66)  / 6hc0b066 /,          jbltyp( 66)  / 0 /
      data  cblock( 67)  / 6hcik    /,          ncbarr( 67)  /  2 /
        data  cblser( 67)  / 6hc0b067 /,          jbltyp( 67)  / 0 /
      data  cblock( 68)  / 6hci     /,          ncbarr( 68)  /  2 /
        data  cblser( 68)  / 6hc0b068 /,          jbltyp( 68)  / 0 /
      data  cblock( 69)  / 6hck     /,          ncbarr( 69)  /  2 /
        data  cblser( 69)  / 6hc0b069 /,          jbltyp( 69)  / 0 /
      data  cblock( 70)  / 6hismout /,          ncbarr( 70)  / 70 /
        data  cblser( 70)  / 6hc0b070 /,          jbltyp( 70)  / 0 /
      data  cblock( 71)  / 6help    /,          ncbarr( 71)  / 65 /
        data  cblser( 71)  / 6hc0b071 /,          jbltyp( 71)  / 0 /
      data  cblock( 72)  / 6hcu     /,          ncbarr( 72)  / 66 /
        data  cblser( 72)  / 6hc0b072 /,          jbltyp( 72)  / 0 /
      data  cblock( 73)  / 6hshp    /,          ncbarr( 73)  / 67 /
        data  cblser( 73)  / 6hc0b073 /,          jbltyp( 73)  / 0 /
      data  cblock( 74)  / 6hhistq  /,          ncbarr( 74)  / 68 /
        data  cblser( 74)  / 6hc0b074 /,          jbltyp( 74)  / 0 /
      data  cblock( 75)  / 6hismdat /,          ncbarr( 75)  / 69 /
        data  cblser( 75)  / 6hc0b075 /,          jbltyp( 75)  / 0 /
      data  cblock( 76)  / 6htexvec /,          ncbarr( 76)  /  7 /
        data  cblser( 76)  / 6hc0b076 /,          jbltyp( 76)  / 1 /
      data  cblock( 77)  / 6hibrnch /,          ncbarr( 77)  / 12 /
        data  cblser( 77)  / 6hc0b077 /,          jbltyp( 77)  / 0 /
      data  cblock( 78)  / 6hjbrnch /,          ncbarr( 78)  / 12 /
        data  cblser( 78)  / 6hc0b078 /,          jbltyp( 78)  / 0 /
      data  cblock( 79)  / 6htstop  /,          ncbarr( 79)  /  4 /
        data  cblser( 79)  / 6hc0b079 /,          jbltyp( 79)  / 0 /
      data  cblock( 80)  / 6hnonlk  /,          ncbarr( 80)  /  9 /
        data  cblser( 80)  / 6hc0b080 /,          jbltyp( 80)  / 0 /
      data  cblock( 81)  / 6hnonlm  /,          ncbarr( 81)  /  9 /
        data  cblser( 81)  / 6hc0b081 /,          jbltyp( 81)  / 0 /
      data  cblock( 82)  / 6hspum   /,          ncbarr( 82)  / 25 /
        data  cblser( 82)  / 6hc0b082 /,          jbltyp( 82)  / 0 /
      data  cblock( 83)  / 6hkks    /,          ncbarr( 83)  /  1 /
        data  cblser( 83)  / 6hc0b083 /,          jbltyp( 83)  / 0 /
      data  cblock( 84)  / 6hkknonl /,          ncbarr( 84)  / 57 /
        data  cblser( 84)  / 6hc0b084 /,          jbltyp( 84)  / 0 /
      data  cblock( 85)  / 6hznonl  /,          ncbarr( 85)  / 57 /
        data  cblser( 85)  / 6hc0b085 /,          jbltyp( 85)  / 0 /
      data  cblock( 86)  / 6hznonlb /,          ncbarr( 86)  /  1 /
        data  cblser( 86)  / 6hc0b086 /,          jbltyp( 86)  / 0 /
      data  cblock( 87)  / 6hznonlc /,          ncbarr( 87)  /  1 /
        data  cblser( 87)  / 6hc0b087 /,          jbltyp( 87)  / 0 /
      data  cblock( 88)  / 6hfinit  /,          ncbarr( 88)  /  1 /
        data  cblser( 88)  / 6hc0b088 /,          jbltyp( 88)  / 0 /
      data  cblock( 89)  / 6hksub   /,          ncbarr( 89)  / 53 /
        data  cblser( 89)  / 6hc0b089 /,          jbltyp( 89)  / 0 /
      data  cblock( 90)  / 6hmsub   /,          ncbarr( 90)  / 53 /
        data  cblser( 90)  / 6hc0b090 /,          jbltyp( 90)  / 0 /
      data  cblock( 91)  / 6hisubeg /,          ncbarr( 91)  / 55 /
        data  cblser( 91)  / 6hc0b091 /,          jbltyp( 91)  / 0 /
      data  cblock( 92)  / 6hlitype /,          ncbarr( 92)  /  2 /
        data  cblser( 92)  / 6hc0b092 /,          jbltyp( 92)  / 0 /
      data  cblock( 93)  / 6himodel /,          ncbarr( 93)  /  2 /
        data  cblser( 93)  / 6hc0b093 /,          jbltyp( 93)  / 0 /
      data  cblock( 94)  / 6hkbus   /,          ncbarr( 94)  /  2 /
        data  cblser( 94)  / 6hc0b094 /,          jbltyp( 94)  / 0 /
      data  cblock( 95)  / 6hmbus   /,          ncbarr( 95)  /  2 /
        data  cblser( 95)  / 6hc0b095 /,          jbltyp( 95)  / 0 /
      data  cblock( 96)  / 6hkodebr /,          ncbarr( 96)  /  2 /
        data  cblser( 96)  / 6hc0b096 /,          jbltyp( 96)  / 0 /
      data  cblock( 97)  / 6hcki    /,          ncbarr( 97)  /  2 /
        data  cblser( 97)  / 6hc0b097 /,          jbltyp( 97)  / 0 /
      data  cblock( 98)  / 6hckkjm  /,          ncbarr( 98)  /  2 /
        data  cblser( 98)  / 6hc0b098 /,          jbltyp( 98)  / 0 /
      data  cblock( 99)  / 6hindhst /,          ncbarr( 99)  /  2 /
        data  cblser( 99)  / 6hc0b099 /,          jbltyp( 99)  / 0 /
      data  cblock(100)  / 6hkodsem /,          ncbarr(100)  /  2 /
        data  cblser(100)  / 6hc0b100 /,          jbltyp(100)  / 0 /
      data  cblock(101)  / 6hnamebr /,          ncbarr(101)  / 54 /
        data  cblser(101)  / 6hc0b101 /,          jbltyp(101)  / 0 /
      data  cblock(102)  / 6hiform  /,          ncbarr(102)  /  4 /
        data  cblser(102)  / 6hc0b102 /,          jbltyp(102)  / 0 /
      data  cblock(103)  / 6hnode   /,          ncbarr(103)  /  4 /
        data  cblser(103)  / 6hc0b103 /,          jbltyp(103)  / 0 /
      data  cblock(104)  / 6hcrest  /,          ncbarr(104)  /  4 /
        data  cblser(104)  / 6hc0b104 /,          jbltyp(104)  / 0 /
      data  cblock(105)  / 6htime1  /,          ncbarr(105)  /  4 /
        data  cblser(105)  / 6hc0b105 /,          jbltyp(105)  / 0 /
      data  cblock(106)  / 6htime2  /,          ncbarr(106)  /  4 /
        data  cblser(106)  / 6hc0b106 /,          jbltyp(106)  / 0 /
      data  cblock(107)  / 6htstart /,          ncbarr(107)  /  4 /
        data  cblser(107)  / 6hc0b107 /,          jbltyp(107)  / 0 /
      data  cblock(108)  / 6hsfreq  /,          ncbarr(108)  /  4 /
        data  cblser(108)  / 6hc0b108 /,          jbltyp(108)  / 0 /
      data  cblock(109)  / 6hkmswit /,          ncbarr(109)  / 60 /
        data  cblser(109)  / 6hc0b109 /,          jbltyp(109)  / 0 /
      data  cblock(110)  / 6hnextsw /,          ncbarr(110)  /  6 /
        data  cblser(110)  / 6hc0b110 /,          jbltyp(110)  / 0 /
      data  cblock(111)  / 6hrmfd   /,          ncbarr(111)  / 61 /
        data  cblser(111)  / 6hc0b111 /,          jbltyp(111)  / 0 /
      data  cblock(112)  / 6hcikfd  /,          ncbarr(112)  / 61 /
        data  cblser(112)  / 6hc0b112 /,          jbltyp(112)  / 0 /
      data  cblock(113)  / 6himfd   /,          ncbarr(113)  / 27 /
        data  cblser(113)  / 6hc0b113 /,          jbltyp(113)  / 0 /
      data  cblock(114)  / 6htclose /,          ncbarr(114)  /  6 /
        data  cblser(114)  / 6hc0b114 /,          jbltyp(114)  / 0 /
      data  cblock(115)  / 6hadelay /,          ncbarr(115)  / 60 /
        data  cblser(115)  / 6hc0b115 /,          jbltyp(115)  / 0 /
      data  cblock(116)  / 6hkpos   /,          ncbarr(116)  /  6 /
        data  cblser(116)  / 6hc0b116 /,          jbltyp(116)  / 0 /
      data  cblock(117)  / 6hnamesw /,          ncbarr(117)  /  6 /
        data  cblser(117)  / 6hc0b117 /,          jbltyp(117)  / 0 /
      data  cblock(118)  / 6he      /,          ncbarr(118)  /  1 /
        data  cblser(118)  / 6hc0b118 /,          jbltyp(118)  / 0 /
      data  cblock(119)  / 6hf      /,          ncbarr(119)  /  1 /
        data  cblser(119)  / 6hc0b119 /,          jbltyp(119)  / 0 /
      data  cblock(120)  / 6hkssfrq /,          ncbarr(120)  /  1 /
        data  cblser(120)  / 6hc0b120 /,          jbltyp(120)  / 0 /
      data  cblock(121)  / 6hkode   /,          ncbarr(121)  /  1 /
        data  cblser(121)  / 6hc0b121 /,          jbltyp(121)  / 0 /
      data  cblock(122)  / 6hkpsour /,          ncbarr(122)  /  1 /
        data  cblser(122)  / 6hc0b122 /,          jbltyp(122)  / 0 /
      data  cblock(123)  / 6hvolti  /,          ncbarr(123)  / 59 /
        data  cblser(123)  / 6hc0b123 /,          jbltyp(123)  / 0 /
      data  cblock(124)  / 6hvoltk  /,          ncbarr(124)  / 26 /
        data  cblser(124)  / 6hc0b124 /,          jbltyp(124)  / 0 /
      data  cblock(125)  / 6hvolt   /,          ncbarr(125)  / 59 /
        data  cblser(125)  / 6hc0b125 /,          jbltyp(125)  / 0 /
      data  cblock(126)  / 6hbus    /,          ncbarr(126)  /  1 /
        data  cblser(126)  / 6hc0b126 /,          jbltyp(126)  / 1 /
      data  cblock(127)  / 6hkarray /,          ncbarr(127)  /  0 /
        data  cblser(127)  / 6hc29b01 /,          jbltyp(127)  / 0 /
      data  cblock(128)  / 6htp     /,          ncbarr(128)  / 23 /
        data  cblser(128)  / 6hspac01 /,          jbltyp(128)  / 0 /
      data  cblock(129)  / 6hnorder /,          ncbarr(129)  /  1 /
        data  cblser(129)  / 6hspac02 /,          jbltyp(129)  / 0 /
      data  cblock(130)  / 6hindex  /,          ncbarr(130)  /  1 /
        data  cblser(130)  / 6hspac03 /,          jbltyp(130)  / 0 /
      data  cblock(131)  / 6hdiag   /,          ncbarr(131)  /  1 /
        data  cblser(131)  / 6hspac04 /,          jbltyp(131)  / 0 /
      data  cblock(132)  / 6hdiab   /,          ncbarr(132)  /  1 /
        data  cblser(132)  / 6hspac05 /,          jbltyp(132)  / 0 /
      data  cblock(133)  / 6hsolr   /,          ncbarr(133)  /  1 /
        data  cblser(133)  / 6hspac06 /,          jbltyp(133)  / 0 /
      data  cblock(134)  / 6hsoli   /,          ncbarr(134)  /  1 /
        data  cblser(134)  / 6hspac07 /,          jbltyp(134)  / 0 /
      data  cblock(135)  / 6hich1   /,          ncbarr(135)  /  1 /
        data  cblser(135)  / 6hspac08 /,          jbltyp(135)  / 0 /
      data  cblock(136)  / 6hbnd    /,          ncbarr(136)  /  9 /
        data  cblser(136)  / 6hspac09 /,          jbltyp(136)  / 0 /
      data  cblock(137)  / 6hiloc   /,          ncbarr(137)  / 23 /
        data  cblser(137)  / 6hspac10 /,          jbltyp(137)  / 0 /
      data  cblock(138)  / 6hgnd    /,          ncbarr(138)  / 23 /
        data  cblser(138)  / 6hspac11 /,          jbltyp(138)  / 0 /
      data  cblock(139)  / 6hkarray /,          ncbarr(139)  /  9 /
        data  cblser(139)  / 6hc31b01 /,          jbltyp(139)  / 0 /
      data  cblock(140)  / 6hxdat   /,          ncbarr(140)  / 71 /
        data  cblser(140)  / 6hc39b01 /,          jbltyp(140)  / 0 /
      data  cblock(141)  / 6hydat   /,          ncbarr(141)  / 71 /
        data  cblser(141)  / 6hc39b02 /,          jbltyp(141)  / 0 /
      data  cblock(142)  / 6haphdat /,          ncbarr(142)  / 71 /
        data  cblser(142)  / 6hc39b03 /,          jbltyp(142)  / 0 /
      data  cblock(143)  / 6hjndex  /,          ncbarr(143)  /  1 /
        data  cblser(143)  / 6hc10b01 /,          jbltyp(143)  / 0 /
      data  cblock(144)  / 6hdiagg  /,          ncbarr(144)  /  1 /
        data  cblser(144)  / 6hc10b02 /,          jbltyp(144)  / 0 /
      data  cblock(145)  / 6hdiabb  /,          ncbarr(145)  /  1 /
        data  cblser(145)  / 6hc10b03 /,          jbltyp(145)  / 0 /
      data  cblock(146)  / 6hsolrsv /,          ncbarr(146)  /  1 /
        data  cblser(146)  / 6hc10b04 /,          jbltyp(146)  / 0 /
      data  cblock(147)  / 6hsolisv /,          ncbarr(147)  /  1 /
        data  cblser(147)  / 6hc10b05 /,          jbltyp(147)  / 0 /
      data  cblock(148)  / 6hgndd   /,          ncbarr(148)  / 23 /
        data  cblser(148)  / 6hc10b06 /,          jbltyp(148)  / 0 /
      data  cblock(149)  / 6hbndd   /,          ncbarr(149)  / 23 /
        data  cblser(149)  / 6hc10b07 /,          jbltyp(149)  / 0 /
      data  cblock(150)  / 6hnekfix /,          ncbarr(150)  /  4 /
        data  cblser(150)  / 6hc10b08 /,          jbltyp(150)  / 0 /
      data  cblock(151)  / 6hfxtem1 /,          ncbarr(151)  /  4 /
        data  cblser(151)  / 6hc10b09 /,          jbltyp(151)  / 0 /
      data  cblock(152)  / 6hfxtem2 /,          ncbarr(152)  /  4 /
        data  cblser(152)  / 6hc10b10 /,          jbltyp(152)  / 0 /
      data  cblock(153)  / 6hfxtem3 /,          ncbarr(153)  /  4 /
        data  cblser(153)  / 6hc10b11 /,          jbltyp(153)  / 0 /
      data  cblock(154)  / 6hfxtem4 /,          ncbarr(154)  /  4 /
        data  cblser(154)  / 6hc10b12 /,          jbltyp(154)  / 0 /
      data  cblock(155)  / 6hfxtem5 /,          ncbarr(155)  /  4 /
        data  cblser(155)  / 6hc10b13 /,          jbltyp(155)  / 0 /
      data  cblock(156)  / 6hfxtem6 /,          ncbarr(156)  /  4 /
        data  cblser(156)  / 6hc10b14 /,          jbltyp(156)  / 0 /
      data  cblock(157)  / 6hfixbu1 /,          ncbarr(157)  /  4 /
        data  cblser(157)  / 6hc10b15 /,          jbltyp(157)  / 0 /
      data  cblock(158)  / 6hfixbu2 /,          ncbarr(158)  /  4 /
        data  cblser(158)  / 6hc10b16 /,          jbltyp(158)  / 0 /
      data  cblock(159)  / 6hfixbu3 /,          ncbarr(159)  /  4 /
        data  cblser(159)  / 6hc10b17 /,          jbltyp(159)  / 0 /
      data  cblock(160)  / 6hfixbu4 /,          ncbarr(160)  /  4 /
        data  cblser(160)  / 6hc10b18 /,          jbltyp(160)  / 0 /
      data  cblock(161)  / 6hfixbu5 /,          ncbarr(161)  /  4 /
        data  cblser(161)  / 6hc10b19 /,          jbltyp(161)  / 0 /
      data  cblock(162)  / 6hfixbu6 /,          ncbarr(162)  /  4 /
        data  cblser(162)  / 6hc10b20 /,          jbltyp(162)  / 0 /
      data  cblock(163)  / 6hfixbu7 /,          ncbarr(163)  /  4 /
        data  cblser(163)  / 6hc10b21 /,          jbltyp(163)  / 0 /
      data  cblock(164)  / 6hfixbu8 /,          ncbarr(164)  /  4 /
        data  cblser(164)  / 6hc10b22 /,          jbltyp(164)  / 0 /
      data  cblock(165)  / 6hfixbu9 /,          ncbarr(165)  /  4 /
        data  cblser(165)  / 6hc10b23 /,          jbltyp(165)  / 0 /
      data  cblock(166)  / 6hfixb10 /,          ncbarr(166)  /  4 /
        data  cblser(166)  / 6hc10b24 /,          jbltyp(166)  / 0 /
      data  cblock(167)  / 6hfixb11 /,          ncbarr(167)  /  4 /
        data  cblser(167)  / 6hc10b25 /,          jbltyp(167)  / 0 /
      data  cblock(168)  / 6hkndex  /,          ncbarr(168)  /  4 /
        data  cblser(168)  / 6hc10b26 /,          jbltyp(168)  / 0 /
      data  cblock(169)  / 6hkarray /,          ncbarr(169)  /  9 /
        data  cblser(169)  / 6hc44b01 /,          jbltyp(169)  / 0 /
      data  cblock(170)  / 6hp      /,          ncbarr(170)  / 75 /
        data  cblser(170)  / 6hc44b02 /,          jbltyp(170)  / 0 /
      data  cblock(171)  / 6hz      /,          ncbarr(171)  / 75 /
        data  cblser(171)  / 6hc44b03 /,          jbltyp(171)  / 0 /
      data  cblock(172)  / 6hic     /,          ncbarr(172)  / 71 /
        data  cblser(172)  / 6hc44b04 /,          jbltyp(172)  / 0 /
      data  cblock(173)  / 6hr      /,          ncbarr(173)  / 71 /
        data  cblser(173)  / 6hc44b05 /,          jbltyp(173)  / 0 /
      data  cblock(174)  / 6hd      /,          ncbarr(174)  / 71 /
        data  cblser(174)  / 6hc44b06 /,          jbltyp(174)  / 0 /
      data  cblock(175)  / 6hgmd    /,          ncbarr(175)  / 71 /
        data  cblser(175)  / 6hc44b07 /,          jbltyp(175)  / 0 /
      data  cblock(176)  / 6hx      /,          ncbarr(176)  / 71 /
        data  cblser(176)  / 6hc44b08 /,          jbltyp(176)  / 0 /
      data  cblock(177)  / 6hy      /,          ncbarr(177)  / 71 /
        data  cblser(177)  / 6hc44b09 /,          jbltyp(177)  / 0 /
      data  cblock(178)  / 6htb2    /,          ncbarr(178)  / 71 /
        data  cblser(178)  / 6hc44b10 /,          jbltyp(178)  / 0 /
      data  cblock(179)  / 6hitb3   /,          ncbarr(179)  / 71 /
        data  cblser(179)  / 6hc44b11 /,          jbltyp(179)  / 0 /
      data  cblock(180)  / 6hworkr1 /,          ncbarr(180)  / 71 /
        data  cblser(180)  / 6hc44b12 /,          jbltyp(180)  / 0 /
      data  cblock(181)  / 6hworkr2 /,          ncbarr(181)  / 71 /
        data  cblser(181)  / 6hc44b13 /,          jbltyp(181)  / 0 /
      data  cblock(182)  / 6htext   /,          ncbarr(182)  / 76 /
        data  cblser(182)  / 6hc44b14 /,          jbltyp(182)  / 1 /
      data  cblock(183)  / 6hgd     /,          ncbarr(183)  / 74 /
        data  cblser(183)  / 6hc44b15 /,          jbltyp(183)  / 0 /
      data  cblock(184)  / 6hbd     /,          ncbarr(184)  / 74 /
        data  cblser(184)  / 6hc44b16 /,          jbltyp(184)  / 0 /
      data  cblock(185)  / 6hyd     /,          ncbarr(185)  / 74 /
        data  cblser(185)  / 6hc44b17 /,          jbltyp(185)  / 0 /
      data  cblock(186)  / 6hitbic  /,          ncbarr(186)  / 73 /
        data  cblser(186)  / 6hc44b18 /,          jbltyp(186)  / 0 /
      data  cblock(187)  / 6htbr    /,          ncbarr(187)  / 73 /
        data  cblser(187)  / 6hc44b19 /,          jbltyp(187)  / 0 /
      data  cblock(188)  / 6htbd    /,          ncbarr(188)  / 73 /
        data  cblser(188)  / 6hc44b20 /,          jbltyp(188)  / 0 /
      data  cblock(189)  / 6htbg    /,          ncbarr(189)  / 73 /
        data  cblser(189)  / 6hc44b21 /,          jbltyp(189)  / 0 /
      data  cblock(190)  / 6htbx    /,          ncbarr(190)  / 73 /
        data  cblser(190)  / 6hc44b22 /,          jbltyp(190)  / 0 /
      data  cblock(191)  / 6htby    /,          ncbarr(191)  / 73 /
        data  cblser(191)  / 6hc44b23 /,          jbltyp(191)  / 0 /
      data  cblock(192)  / 6htbtb2  /,          ncbarr(192)  / 73 /
        data  cblser(192)  / 6hc44b24 /,          jbltyp(192)  / 0 /
      data  cblock(193)  / 6hitbtb3 /,          ncbarr(193)  / 73 /
        data  cblser(193)  / 6hc44b25 /,          jbltyp(193)  / 0 /
      data  cblock(194)  / 6htbtext /,          ncbarr(194)  / 73 /
        data  cblser(194)  / 6hc44b26 /,          jbltyp(194)  / 1 /
      data  cblock(195)  / 6hkarray /,          ncbarr(195)  /  9 /
        data  cblser(195)  / 6hc45b01 /,          jbltyp(195)  / 0 /
      data  cblock(196)  / 6hkarray /,          ncbarr(196)  /  9 /
        data  cblser(196)  / 6hc47b01 /,          jbltyp(196)  / 0 /
      data  char(1)   /  1hi  /
      data  char(2)   /  1hj  /
      data  char(3)   /  1hk  /
      data  char(4)   /  1hl  /
      data  char(5)   /  1hm  /
      data  char(6)   /  1hn  /
      data  type(1,1)  / 6hreal*8 /
      data  type(1,2)  / 6h       /
      data  type(1,3)  / 6h       /
      data  type(2,1)  / 6hcomple /
      data  type(2,2)  / 6hx* 8   /
      data  type(2,3)  / 6h       /
      data  type(3,1)  / 6hreal*8 /
      data  type(3,2)  / 6h       /
      data  type(3,3)  / 6h       /
      data  type(4,1)  / 6hintege /
      data  type(4,2)  / 6hr*4    /
      data  type(4,3)  / 6h       /
      numlst = 28
      lunit2 = 7
      lunit3 = 8
      lunit5 = 5
      lunit6 = 6
      mulvar(1)  =  2
      mulvar(2)  =  2
      mulvar(3)  =  2
      mulvar(4)  =  1
      open (unit = 7, status = 'scratch', form = 'formatted')
      open (unit = 8, status = 'scratch', form = 'formatted')
      indexm = 0
      mextra = 0
c     one less than the number of  'vardim'  modules.
      numkex = 7
c     default values for extra offsets of  'vardim'
c     modules belonging to  non-solution overlays.
      do 5245  i=1, numkex
 5245 kextra(i) = 0
c     default values for emtp list sizes follow.
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
      read (lunit5, 5287)  ( lstnew(i), i=1, numlst )
 5287 format ( 10i8 )
      write (lunit6, 5263)
 5263 format ( /,  29h pseudo-listing of data cards,
     1  38h which have been read by the variable-,
     2  39hdimensioning program  'vardim' .   only  ,/,
     3  36h if all data fields are punched with,
     4  39h  'clean'  i8  integer information will,
     5  37h this be a true listing.   data cards    ,/,
     6  41h are in fact read in and then printed out,
     7  34h using integer variables and  10i8,
     8   9h  format.                                )
      write (lunit6, 5264)   ( i,  i=1, 8 ),
     1                       ( lstnew(i), i=1, numlst )
 5264 format ( 1x,  111( 1h- )                      ,/,
     1  31x,  1h0,  8( 9x,  i1 )                    ,/,
     2  31x,  1h0,  8( 9x,  1h0 )                   ,/,
     3  31x,  80( 1h- )                             ,/,
     4  24h 1st card (lists  1-10).,  7x,  1h1,  10i8 ,/,
     5  24h 2nd card (lists 11-20).,  7x,  1h1,  10i8 ,/,
     6  24h 3rd card (lists 21-30).,  7x,  1h1,  10i8 )
      if ( lstnew(1) / 10000000  .ne.  9 )   go to 5294
      read (lunit5, 5287)  ( kextra(i), i=1, numkex )
      write (lunit6, 5378)  ( kextra(i), i=1, numkex )
 5378 format ( 22h supplemental offsets.,  9x,  1h1,
     1         10i8  )
      lstnew(1) = lstnew(1) - 90000000
 5294 if ( lstnew(11)/10000000 .ne. 9 ) go to 5297
      n4 = lstnew(11) - 90000000
      do 5296  j=1, numlst
 5296 lstnew(j) = lstdef(j) * n4
 5297 if ( lstnew(1)  .gt.  0 )
     1 lstnew(1) = lstnew(1) + 2
      write (lunit6, 5381)
 5381 format ( 1x,  111( 1h- )  )
      do 5326  i=1, numlst
      n1 = i
      if ( lstnew(i)  .ge.  1000000 )   go to 9000
      if ( lstnew(i)  .le.  0 )   lstnew(i) = lstdef(i)
 5326 continue
      if ( lstnew(19) .le. 23 )  lstnew(19) = 23
      if ( lstnew(26)  .le.  10 )  lstnew(26) = 10
      n1 = lstnew(16) / 2
      if ( 2*n1   .ne.   lstnew(16) )
     1 lstnew(16) = lstnew(16) + 1
      n1 = lstnew(27) / 2
      if ( 2*n1  .ne.  lstnew(27) )
     1 lstnew(27) = lstnew(27) + 1
c     list number 51 is a dependent list, always twice
c     the size of list number 13.
c     this is for frequency-dependence arrays
c     'con1' ,  'con2' ,  and  'con3' .
      lstnew(51)  =  6 * lstnew(13)
c     list number 52 is a dependent list, always twice
c     the size of list number 1, plus one.
c     this is for frequency-dependence array   'iwtent' .
      lstnew(52) = lstnew(51) + 1
c     list number 7 also serves for storage as part of
c     list number 3.   hence list 7 must not be shorter.
c     if (  lstnew(3)  .gt.  lstnew(7) )
c    1 lstnew(7) = lstnew(3)
c     list number 53 is for terminal pairs
c     associated with compensation elements.
      lstnew(53) = lstnew(9) + 3 * lstnew(17)
c      cable adds 5 list-2 extensions to  'namebr'
      lstnew(54) = 6 * lstnew(2)
c     list 55 has one entry for each subnetwork.
      lstnew(55)  =  lstnew(9)  +  lstnew(17)
c     list 56 is for vladimir's "cikfd" and "rmfd"
c     arrays for freq-depend. generator equivalent
      lstnew(56) = lstnew(2) * lstnew(27) / 2
c     list 57 maps #phases into znonl size.
      lstnew(57)  =  lstnew(1) * lstnew(24)
c     list 58 is for extrema vector  "xmax" .
      lstnew(58) = 4 * lstnew(12)
c     list 59 is extended working vector "volti"
      lstnew(59) = 2 * lstnew(26)
c     list 60 provides double-length "kmswit" as
c     replacement for earlier "klow" and "khigh"
      lstnew(60) = 3 * lstnew(6)
c     list 61 is for ontario hydro freq-dep source
      lstnew(61) = 1
c     list 62 is for power/energy ("koutvp" which
c     now includes former, separate "koutie")
      lstnew(62) = 2 * lstnew(18)
      lstnew(63) = 3 * lstnew(6)
      lstnew(64) = 4 * lstnew(6)
c     list 65 is for type-59 s.m. electrical data:
      lstnew(65) = 101 * lstnew(17)
c     list 66 is for type-59 s.m. electrical variables:
      lstnew(66) =  24 * lstnew(17)
c     list 67 is for type-59 s.m. mechanical data:
      lstnew(67) =  12 * lstnew(16)
c     list 68 is for type-59 s.m. mechanical variables:
      lstnew(68) =   6 * lstnew(16)
c     list 69 is for type-59 s.m. pointers:
      lstnew(69) =  30 * lstnew(17)
c     list 70 is for type-59 s.m. output pointers:
      lstnew(70) =   5 * lstnew(11) + 2
c     list 71 is to extend list 21 for ljg (18 aug 1987):
      lstnew(71) =   2 * lstnew(21)
      lstnew(72) =   lstnew(8) + lstnew(28)
c     list 73 is to extend list 22 for ljg (10 mar 1988):
      lstnew(73) =   lstnew(22) + lstnew(21)
      mtot = 0
      do 4301  i =   1, 126
      n9 = ncbarr(i)
      if ( n9  .eq.  0     .or.     n9  .eq.  98 )   go to 4301
      n37 = 3
      bus1(1) = cblock(i)
      read (unit = bus1, fmt = 8104) bus2
 8104 format ( a1 )
      do 4701  j=1, 6
      if ( bus2  .eq.  char(j) )   n37 = 4
 4701 continue
      if ( jbltyp(i)  .ne.  0 )   n37 = jbltyp(i)
      mtot = mtot + mulvar(n37)*lstnew(n9)
 4301 continue
      write (lunit2, 4201)
 4201 format ( 6x,  17hsubroutine main10,  57x  ,/,
     1  80h      implicit real*8 (a-h, o-z),  integer*4 (i-n)
     2                                                             )
      do 4101  ii=  1, 126
      i =   0  +  ii
      n3 = ncbarr(i)
      nonneg = lstnew(n3)
      if ( nonneg  .le.  0 )   nonneg = 1
      write (lunit2, 7236)  cblser(i), cblock(i), nonneg
 7236 format ( 6x,  10hcommon  / ,  a6,  5h /   ,  a6,  1h(,
     1  i8,  2h ),  38x  )
      n4 = jbltyp(i)
      if ( n4  .eq.  0 )   go to 4101
      write (lunit2, 7243)  (type(n4,j), j=1, 3),  cblock(i)
 7243 format ( 6x,  3a6,  a6,  50x )
 4101 continue
      write (lunit2, 7286)
 7286 format ( 6x,  11hcall subr10,  63x )
      write (lunit2, 7297)
 7297 format ( 6x,  6hreturn,  68x )
      write (lunit2, 7298)
 7298 format ( 6x,  3hend,  71x )
      ltlabl = mtot
      if ( ltlabl  .gt.  9999999 )   go to 9000
      indexm = indexm + 1
      mul34 = mulvar(3) / mulvar(4)
      mextra = 4000
      lstnew(98) = ltlabl
      write (lunit3, 8116)
 8116 format ( 6x,  47hsubroutine dimens ( lsize, nchain, bus1, bus2 ),
     1  27x  ,/,
     2  32h      implicit real*8 (a-h, o-z)   )
      write (lunit3, 8120)
 8120 format ( 6x,  20hdimension  lsize(62),  54x )
      write (lunit3, 8124)
 8124 format ( 6x,   24hreal          bus1, bus2,  50x )
      write (lunit3, 8134)
 8134 format ( 6x,  36hif ( nchain  .ge.  29 )   go to 2900,  38x )
      do 8137  i=1, numlst
 8137 write (lunit3, 8143)  i, lstnew(i)
 8143 format ( 6x,  6hlsize(,  i2,  4h)  =,  i8,  56x )
      write (lunit3, 8155)  numlst
 8155 format ( 6x,  4hn7 =,  i3,  4h + 1,  63x )
      write (lunit3, 8159)  ltlabl
 8159 format ( 6x,  11hlsize(n7) =,  i8,  55x )
      nrec3 = numlst + 7
      call time44 ( texta )
      call date44 ( textb )
      write (ansi16, 3672)  texta, textb
 3672 format ( 4a4 )
      read (ansi16, 3676) nh, nm, ns, nm, nd, ny
 3676 format ( i2, 1x, i2, 1x, 2i2, 1x, i2, 1x, i2 )
      ntime = 10000*nh + 100*nm + ns
      ndate = 10000*nm + 100*nd + ny
      write (lunit3, 3684)  ntime, ndate
 3684 format ( 6x, 6hbus1 =,  i8  ,/,
     1         6x, 6hbus2 =,  i8  )
      mtot = 0
      do 4302  i = 127, 138
      n9 = ncbarr(i)
      if ( n9  .eq.  0     .or.     n9  .eq.  98 )   go to 4302
      n37 = 3
      bus1(1) = cblock(i)
      read (unit = bus1, fmt = 8104) bus2
      do 4702  j=1, 6
      if ( bus2  .eq.  char(j) )   n37 = 4
 4702 continue
      if ( jbltyp(i)  .ne.  0 )   n37 = jbltyp(i)
      mtot = mtot + mulvar(n37)*lstnew(n9)
 4302 continue
      lstnew(99) = ltlabl + kextra( 1)
      if ( lstnew(99) .le. 0 )  lstnew(99) = 1
      ncbarr(127) = 99
      write (lunit2, 4202)
 4202 format ( 6x,  17hsubroutine over29,  57x  ,/,
     1  80h      implicit real*8 (a-h, o-z),  integer*4 (i-n)
     2                                                             )
      write (lunit3, 8156)
 8156 format ( 6x,  6hreturn,  68x )
      lm = 0
      n86 = 29
      n87 = 31
      write (lunit3, 8158)  n86, n86, n87
      nrec3 = nrec3 + 2
 8158 format ( i3,  20h00 if ( nchain  .gt.,  i4,
     1  10h )   go to,  i3,   2h00,  38x )
      do 4102  ii=  1,  12
      i = 126  +  ii
      n3 = ncbarr(i)
      if ( ii  .eq.  1 )   go to 4502
      n7 = ii - 1
      do 4402  kk=1, n7
      if ( ncbarr(kk+126)  .eq.  n3 )   go to 4602
 4402 continue
 4502 lm = lm + 1
      n28 = n3
      if ( n3  .eq.  99 )   n28 = 0
      write (lunit3, 8143)  lm, n28
      lm = lm + 1
      write (lunit3, 4769)  lm, lstnew(n3)
 4769 format ( 6x,  6hlsize(,  i2,  3h) =,  i7,  56x  )
      nrec3 = nrec3 + 2
 4602 continue
      nonneg = lstnew(n3)
      if ( nonneg  .le.  0 )   nonneg = 1
      write (lunit2, 7236)  cblser(i), cblock(i), nonneg
      n4 = jbltyp(i)
      if ( n4  .eq.  0 )   go to 4102
      write (lunit2, 7243)  (type(n4,j), j=1, 3),  cblock(i)
 4102 continue
      n18 = 29
      write (lunit2, 7274)  n18
 7274 format ( 6x,  9hcall subr,  i2,  63x )
      write (lunit2, 7297)
      write (lunit2, 7298)
      indexm = indexm + 1
      mextra = 5000
      write (lunit2, 4203)
 4203 format ( 6x,  17hsubroutine over31,  57x  ,/,
     1  80h      implicit real*8 (a-h, o-z),  integer*4 (i-n)
     2                                                             )
      write (lunit3, 8156)
      lm = 0
      n86 = 31
      n87 = 39
      write (lunit3, 8158)  n86, n86, n87
      nrec3 = nrec3 + 2
      do 4103  ii=  1,   1
      i = 138  +  ii
      n3 = ncbarr(i)
      if ( ii  .eq.  1 )   go to 4503
      n7 = ii - 1
      do 4403  kk=1, n7
      if ( ncbarr(kk+138)  .eq.  n3 )   go to 4603
 4403 continue
 4503 lm = lm + 1
      n28 = n3
      if ( n3  .eq.  99 )   n28 = 0
      write (lunit3, 8143)  lm, n28
      lm = lm + 1
      write (lunit3, 4769)  lm, lstnew(n3)
      nrec3 = nrec3 + 2
 4603 continue
      nonneg = lstnew(n3)
      if ( nonneg  .le.  0 )   nonneg = 1
      write (lunit2, 7236)  cblser(i), cblock(i), nonneg
      n4 = jbltyp(i)
      if ( n4  .eq.  0 )   go to 4103
      write (lunit2, 7243)  (type(n4,j), j=1, 3),  cblock(i)
 4103 continue
      n18 = 31
      write (lunit2, 7274)  n18
      write (lunit2, 7297)
      write (lunit2, 7298)
      lstnew(71) = 500
      if ( lstnew(3) .gt. 500 )
     1 lstnew(71) = lstnew(3)
      write (lunit2, 4204)
 4204 format ( 6x,  17hsubroutine over39,  57x  ,/,
     1  80h      implicit real*8 (a-h, o-z),  integer*4 (i-n)
     2                                                             )
      write (lunit3, 8156)
      lm = 0
      n86 = 39
      n87 = 10
      write (lunit3, 8158)  n86, n86, n87
      nrec3 = nrec3 + 2
      do 4104  ii=  1,   3
      i = 139  +  ii
      n3 = ncbarr(i)
      if ( ii  .eq.  1 )   go to 4504
      n7 = ii - 1
      do 4404  kk=1, n7
      if ( ncbarr(kk+139)  .eq.  n3 )   go to 4604
 4404 continue
 4504 lm = lm + 1
      n28 = n3
      if ( n3  .eq.  99 )   n28 = 0
      write (lunit3, 8143)  lm, n28
      lm = lm + 1
      write (lunit3, 4769)  lm, lstnew(n3)
      nrec3 = nrec3 + 2
 4604 continue
      nonneg = lstnew(n3)
      if ( nonneg  .le.  0 )   nonneg = 1
      write (lunit2, 7236)  cblser(i), cblock(i), nonneg
      n4 = jbltyp(i)
      if ( n4  .eq.  0 )   go to 4104
      write (lunit2, 7243)  (type(n4,j), j=1, 3),  cblock(i)
 4104 continue
      n18 = 39
      write (lunit2, 7274)  n18
      write (lunit2, 7297)
      write (lunit2, 7298)
      write (lunit2, 4205)
 4205 format ( 6x,  17hsubroutine fixs10,  57x  ,/,
     1  80h      implicit real*8 (a-h, o-z),  integer*4 (i-n)
     2                                                             )
      write (lunit3, 8156)
      lm = 0
      n86 = 10
      n87 = 44
      write (lunit3, 8158)  n86, n86, n87
      nrec3 = nrec3 + 2
      do 4105  ii=  1,  26
      i = 142  +  ii
      n3 = ncbarr(i)
      if ( ii  .eq.  1 )   go to 4505
      n7 = ii - 1
      do 4405  kk=1, n7
      if ( ncbarr(kk+142)  .eq.  n3 )   go to 4605
 4405 continue
 4505 lm = lm + 1
      n28 = n3
      if ( n3  .eq.  99 )   n28 = 0
      write (lunit3, 8143)  lm, n28
      lm = lm + 1
      write (lunit3, 4769)  lm, lstnew(n3)
      nrec3 = nrec3 + 2
 4605 continue
      nonneg = lstnew(n3)
      if ( nonneg  .le.  0 )   nonneg = 1
      write (lunit2, 7236)  cblser(i), cblock(i), nonneg
      n4 = jbltyp(i)
      if ( n4  .eq.  0 )   go to 4105
      write (lunit2, 7243)  (type(n4,j), j=1, 3),  cblock(i)
 4105 continue
      n18 = 10
      write (lunit2, 7274)  n18
      write (lunit2, 7297)
      write (lunit2, 7298)
      indexm = indexm + 1
      mextra = 7000
      d1 = ( ltlabl + mextra + kextra(indexm) ) / mul34
      d2 = ( 91./4. + 19./2. ) ** 2
      d3 = 4. * ( 11./8. + 19./2. ) * ( 9. - d1 )
      d1 = sqrt ( d2 - d3 )
      lphase = (-(91./4.+19./2.)+d1)/(2.*(11./8.+19./2.))
      if ( lphase  .le.  1 )   lphase = 2
      if ( lphase  .gt.  100 )   lphase = lphase * 0.7
      lphd2 = lphase / 2
      lstnew(71) = lphd2 * 2
      lstnew(73) = lstnew(71) + 1
      lstnew(74) = ( lphd2 * ( lphd2+1) ) / 2
      lstnew(75) = ( lstnew(71) * ( lstnew(71)+1) ) / 2
      lstnew(76) = 2 * lstnew(71)
      write (lunit2, 4206)
 4206 format ( 6x,  17hsubroutine over44,  57x  ,/,
     1  80h      implicit real*8 (a-h, o-z),  integer*4 (i-n)
     2                                                             )
      write (lunit3, 8156)
      lm = 0
      n86 = 44
      n87 = 45
      write (lunit3, 8158)  n86, n86, n87
      nrec3 = nrec3 + 2
      do 4106  ii=  1,  26
      i = 168  +  ii
      n3 = ncbarr(i)
      if ( ii  .eq.  1 )   go to 4506
      n7 = ii - 1
      do 4406  kk=1, n7
      if ( ncbarr(kk+168)  .eq.  n3 )   go to 4606
 4406 continue
 4506 lm = lm + 1
      n28 = n3
      if ( n3  .eq.  99 )   n28 = 0
      write (lunit3, 8143)  lm, n28
      lm = lm + 1
      write (lunit3, 4769)  lm, lstnew(n3)
      nrec3 = nrec3 + 2
 4606 continue
      nonneg = lstnew(n3)
      if ( nonneg  .le.  0 )   nonneg = 1
      write (lunit2, 7236)  cblser(i), cblock(i), nonneg
      n4 = jbltyp(i)
      if ( n4  .eq.  0 )   go to 4106
      write (lunit2, 7243)  (type(n4,j), j=1, 3),  cblock(i)
 4106 continue
      n18 = 44
      write (lunit2, 7274)  n18
      write (lunit2, 7297)
      write (lunit2, 7298)
      indexm = indexm + 1
      mextra = 5000
      write (lunit2, 4207)
 4207 format ( 6x,  17hsubroutine over45,  57x  ,/,
     1  80h      implicit real*8 (a-h, o-z),  integer*4 (i-n)
     2                                                             )
      write (lunit3, 8156)
      lm = 0
      n86 = 45
      n87 = 47
      write (lunit3, 8158)  n86, n86, n87
      nrec3 = nrec3 + 2
      do 4107  ii=  1,   1
      i = 194  +  ii
      n3 = ncbarr(i)
      if ( ii  .eq.  1 )   go to 4507
      n7 = ii - 1
      do 4407  kk=1, n7
      if ( ncbarr(kk+194)  .eq.  n3 )   go to 4607
 4407 continue
 4507 lm = lm + 1
      n28 = n3
      if ( n3  .eq.  99 )   n28 = 0
      write (lunit3, 8143)  lm, n28
      lm = lm + 1
      write (lunit3, 4769)  lm, lstnew(n3)
      nrec3 = nrec3 + 2
 4607 continue
      nonneg = lstnew(n3)
      if ( nonneg  .le.  0 )   nonneg = 1
      write (lunit2, 7236)  cblser(i), cblock(i), nonneg
      n4 = jbltyp(i)
      if ( n4  .eq.  0 )   go to 4107
      write (lunit2, 7243)  (type(n4,j), j=1, 3),  cblock(i)
 4107 continue
      n18 = 45
      write (lunit2, 7274)  n18
      write (lunit2, 7297)
      write (lunit2, 7298)
      indexm = indexm + 1
      mextra = 5000
      write (lunit2, 4208)
 4208 format ( 6x,  17hsubroutine over47,  57x  ,/,
     1  80h      implicit real*8 (a-h, o-z),  integer*4 (i-n)
     2                                                             )
      write (lunit3, 8156)
      lm = 0
      n86 = 47
      n87 = 99
      write (lunit3, 8158)  n86, n86, n87
      nrec3 = nrec3 + 2
      do 4108  ii=  1,   1
      i = 195  +  ii
      n3 = ncbarr(i)
      if ( ii  .eq.  1 )   go to 4508
      n7 = ii - 1
      do 4408  kk=1, n7
      if ( ncbarr(kk+195)  .eq.  n3 )   go to 4608
 4408 continue
 4508 lm = lm + 1
      n28 = n3
      if ( n3  .eq.  99 )   n28 = 0
      write (lunit3, 8143)  lm, n28
      lm = lm + 1
      write (lunit3, 4769)  lm, lstnew(n3)
      nrec3 = nrec3 + 2
 4608 continue
      nonneg = lstnew(n3)
      if ( nonneg  .le.  0 )   nonneg = 1
      write (lunit2, 7236)  cblser(i), cblock(i), nonneg
      n4 = jbltyp(i)
      if ( n4  .eq.  0 )   go to 4108
      write (lunit2, 7243)  (type(n4,j), j=1, 3),  cblock(i)
 4108 continue
      n18 = 47
      write (lunit2, 7274)  n18
      write (lunit2, 7297)
      write (lunit2, 7298)
      write (lunit3, 8156)
      write (lunit3, 8234)
 8234 format (44h 9900 lsize(1) = locint(bus1) - locint(bus2),  36x )
      write (lunit3, 8156)
      write (lunit3, 8242)
 8242 format ( 6x,  3hend,  71x )
      nrec3 = nrec3 + 4
      rewind lunit3
      do 8258  m=1, 99999
      read (lunit3, 8253, end=8259)  abuff
 8253 format ( 13a6,  a2 )
 8258 write (lunit2, 8253)  abuff
 8259 continue
      write (lunit6, 8942)  ltlabl
 8942 format ( 26h normal termination within,
     1  18h  'vardim' .   the                       ,/,
     2  27h size of   /label/   equals,  i8,
     3  17h   integer words.  ,//,  1x              )
      go to 9999
 9000 write (lunit6, 9023)
 9023 format ( //, 1x  )
      write (lunit6, 9027)
 9027 format ( 1x,  131( 1h- )  )
      write (lunit6, 9031)
 9031 format (  22( 6h error )   )
      write (lunit6, 9031)
      write (lunit6, 9027)
      write (lunit6, 9044)
 9044 format ( /, 5x,   23hthe user is in trouble,,
     1  42h unless he has made a data-punching error.,
     2  40h   variable-dimensioning of the emtp has   ,/,
     3  5x,  40hbroken down within the separate variable,
     4  38h-dimensioning program  'vardim' .   in,
     5  31h particular, one or more of the          ,/,
     6  5x,  32huser-inputted list sizes must be,
     7  43h rejected as being illegally large.   user-,
     8  35hsupplied limits (or default limits,      )
      write (lunit6, 9045)   ( lstnew(i), i=1, numlst )
 9045 format (  5x,  25hfor any non-positive data,
     1  28h fields) are as follows ....   ,/,
     2 ( 1x,  10i10 )  )
      if ( ltlabl  .gt.  0 )   go to 9061
      write (lunit6, 9049)  n1, lstnew(n1)
 9049 format ( 5x,  23hspecifically, the user-,
     1  38hsupplied value which was read for list,
     2   7h number,  i4,  19h  exceeds  999999 ,     ,/,
     3  5x,  29hwhich is unreal.   a value of,  i10,
     4  "   was read from the user's data card",
     5  15h for this list.    ,/,  1x               )
 9054 write (lunit6, 9027)
      write (lunit6, 9031)
      write (lunit6, 9031)
      write (lunit6, 9027)
      write (lunit6, 9023)
      go to 9900
 9061 write (lunit6, 9064)  ltlabl
 9064 format ( 5x, 31hspecifically, the user-supplied,
     1  40h dimensions have collectively produced a,
     2  32h total labeled-common allocation         ,/,
     3  5x,   2hof,  i12,  26h   words, which is unreal.,
     4  41h   a limit of   9999999   has arbitrarily,
     5  36h been imposed by the better judgment     )
      write (lunit6, 9065)
 9065 format ( 5x, 32hof the program.   a machine this,
     1  41h big (with this much real central memory),
     2  41h is not known to be in existence anywhere  ,/,
     3  5x,  13hin the world.              ,/,  1x  )
      go to 9054
 9900 continue
      close (unit = 7, status = 'delete' )
      close (unit = 8, status = 'delete' )
 9999 stop
      end
c
c     end of file: vardim.for
c
