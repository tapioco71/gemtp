!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

!
! file data.f90
!

block data tdata
   use tacsto
     !   include 'tacsto.ftn'
   data ctbl(   1:  26) / 'abcdefghijklmnopqrstuvwxyz' /
   data ctbl(  27:  37) / '0123456789_' /
   data ctbl(  39:  48) / '"[](){}+-*' /
   data ctbl(  50:  64) / '**\:=:=><...,; ' /
   data ctbl(  66:  66) / '_' /
   data ctbl(  67:  67) / '!' /
   data ctbl(  68:  74) / 'comment' /
   data ctbl(  75:  84) / 'endcomment' /
   data ctbl(  85:  96) / 'illustration' /
   data ctbl(  97: 111) / 'endillustration' /
   data ctbl( 112: 121) / '__________' /
   data ctbl( 122: 142) / 'endrecord____________' /
   data ctbl( 143: 149) / 'endtacs' /
   data ctbl( 150: 155) / 'record' /
   data ctbl( 156: 161) / 'ixdata' /
   data ctbl( 162: 166) / 'model' /
   data ctbl( 167: 174) / 'endmodel' /
   data ctbl( 175: 181) / 'fortran' /
   data ctbl( 182: 185) / 'ixin' /
   data ctbl( 186: 190) / 'ixout' /
   data ctbl( 191: 195) / 'ixvar' /
   data ctbl( 196: 203) / 'function' /
   data ctbl( 204: 212) / 'pointlist' /
   data ctbl( 213: 215) / 'fun' /
   data ctbl( 216: 220) / 'ixarg' /
   data ctbl( 221: 224) / 'init' /
   data ctbl( 225: 231) / 'endinit' /
   data ctbl( 232: 235) / 'exec' /
   data ctbl( 236: 242) / 'endexec' /
   data ctbl( 243: 247) / 'const' /
   data ctbl( 248: 251) / 'data' /
   data ctbl( 252: 254) / 'var' /
   data ctbl( 255: 259) / 'input' /
   data ctbl( 260: 265) / 'output' /
   data ctbl( 266: 272) / 'history' /
   data ctbl( 273: 280) / 'timestep' /
   data ctbl( 281: 293) / 'interpolation' /
   data ctbl( 294: 299) / 'degree' /
   data ctbl( 300: 303) / '____' /
   data ctbl( 304: 309) / 'diffeq' /
   data ctbl( 310: 316) / 'laplace' /
   data ctbl( 317: 318) / 'if' /
   data ctbl( 319: 322) / 'then' /
   data ctbl( 323: 326) / 'else' /
   data ctbl( 327: 331) / 'elsif' /
   data ctbl( 332: 336) / 'endif' /
   data ctbl( 337: 341) / 'while' /
   data ctbl( 342: 349) / 'endwhile' /
   data ctbl( 350: 352) / 'for' /
   data ctbl( 353: 358) / 'endfor' /
   data ctbl( 359: 360) / 'do' /
   data ctbl( 361: 365) / 'enddo' /
   data ctbl( 366: 369) / 'redo' /
   data ctbl( 370: 376) / 'combine' /
   data ctbl( 377: 386) / 'endcombine' /
   data ctbl( 387: 394) / 'sequence' /
   data ctbl( 395: 405) / 'endsequence' /
   data ctbl( 406: 410) / 'write' /
   data ctbl( 411: 418) / 'endwrite' /
   data ctbl( 419: 423) / 'error' /
   data ctbl( 424: 427) / 'stop' /
   data ctbl( 428: 430) / 'use' /
   data ctbl( 431: 436) / 'enduse' /
   data ctbl( 437: 439) / 'sum' /
   data ctbl( 440: 444) / 'deriv' /
   data ctbl( 445: 452) / 'integral' /
   data ctbl( 453: 455) / 'pol' /
   data ctbl( 456: 457) / 'to' /
   data ctbl( 458: 459) / 'by' /
   data ctbl( 460: 461) / 'as' /
   data ctbl( 462: 463) / 'is' /
   data ctbl( 464: 467) / 'save' /
   data ctbl( 468: 470) / 'set' /
   data ctbl( 471: 478) / 'previous' /
   data ctbl( 479: 483) / 'state' /
   data ctbl( 484: 487) / 'when' /
   data ctbl( 488: 489) / 'or' /
   data ctbl( 490: 492) / 'and' /
   data ctbl( 493: 495) / 'not' /
   data ctbl( 496: 498) / 'mod' /
   data ctbl( 499: 501) / 'val' /
   data ctbl( 502: 505) / 'dflt' /
   data ctbl( 506: 509) / 'dmin' /
   data ctbl( 510: 513) / 'dmax' /
   data ctbl( 514: 523) / '__________' /
   data ctbl( 524: 533) / '__________' /
   data ctbl( 534: 543) / '__________' /
   data ctbl( 544: 553) / '__________' /
   data ctbl( 554: 563) / '__________' /
   data ctbl( 564: 566) / 'abs' /
   data ctbl( 567: 570) / 'sqrt' /
   data ctbl( 571: 573) / 'exp' /
   data ctbl( 574: 575) / 'ln' /
   data ctbl( 576: 580) / 'log10' /
   data ctbl( 581: 584) / 'log2' /
   data ctbl( 585: 589) / 'recip' /
   data ctbl( 590: 598) / 'factorial' /
   data ctbl( 599: 603) / 'trunc' /
   data ctbl( 604: 608) / 'fract' /
   data ctbl( 609: 613) / 'round' /
   data ctbl( 614: 617) / 'sign' /
   data ctbl( 618: 620) / 'rad' /
   data ctbl( 621: 623) / 'deg' /
   data ctbl( 624: 629) / 'random' /
   data ctbl( 630: 632) / 'sin' /
   data ctbl( 633: 635) / 'cos' /
   data ctbl( 636: 638) / 'tan' /
   data ctbl( 639: 642) / 'asin' /
   data ctbl( 643: 646) / 'acos' /
   data ctbl( 647: 650) / 'atan' /
   data ctbl( 651: 654) / 'sinh' /
   data ctbl( 655: 658) / 'cosh' /
   data ctbl( 659: 662) / 'tanh' /
   data ctbl( 663: 667) / 'asinh' /
   data ctbl( 668: 672) / 'acosh' /
   data ctbl( 673: 677) / 'atanh' /
   data ctbl( 678: 682) / 'atan2' /
   data ctbl( 683: 687) / 'binom' /
   data ctbl( 688: 693) / 'permut' /
   data ctbl( 694: 696) / 'min' /
   data ctbl( 697: 699) / 'max' /
   data ctbl( 700: 703) / 'norm' /
   data ctbl( 704: 707) / 'bool' /
   data ctbl( 708: 711) / 'nand' /
   data ctbl( 712: 714) / 'nor' /
   data ctbl( 715: 717) / 'xor' /
   data ctbl( 718: 727) / 'and_______' /
   data ctbl( 728: 737) / 'or________' /
   data ctbl( 738: 747) / '__________' /
   data ctbl( 748: 757) / '__________' /
   data ctbl( 757: 763) / 'predval' /
   data ctbl( 764: 770) / 'pastval' /
   data ctbl( 771: 778) / 'pasttime' /
   data ctbl( 779: 786) / 'predtime' /
   data ctbl( 787: 793) / 'prevval' /
   data ctbl( 794: 800) / 'backval' /
   data ctbl( 801: 808) / 'backtime' /
   data ctbl( 809: 813) / 'deriv' /
   data ctbl( 814: 819) / 'deriv2' /
   data ctbl( 820: 823) / '____' /
   data ctbl( 824: 824) / 'v' /
   data ctbl( 825: 825) / 'i' /
   data ctbl( 826: 829) / 'elec' /
   data ctbl( 830: 835) / 'switch' /
   data ctbl( 836: 840) / 'imssv' /
   data ctbl( 841: 845) / 'imssi' /
   data ctbl( 846: 855) / 'histdef___' /
   data ctbl( 856: 865) / 'histval___' /
   data ctbl( 866: 875) / '__________' /
   data ctbl( 876: 885) / '__________' /
   data ctbl( 886: 895) / '__________' /
   data ctbl( 896: 902) / 'minstep' /
   data ctbl( 903: 909) / 'maxstep' /
   data ctbl( 910: 916) / 'endtime' /
   data ctbl( 917: 924) / 'fullstep' /
   data ctbl( 925: 932) / 'timestep' /
   data ctbl( 933: 940) / 'prevtime' /
   data ctbl( 941: 947) / 'simtime' /
   data ctbl( 948: 951) / 'path' /
   data ctbl( 952: 961) / '__________' /
   data ctbl( 962: 971) / '__________' /
   data ctbl( 972: 981) / '__________' /
   data ctbl( 982: 991) / '__________' /
   data ctbl( 992: 1001) / '__________' /
   data ctbl( 1002: 1002) / 't' /
   data ctbl( 1003: 1011) / 'starttime' /
   data ctbl( 1012: 1019) / 'stoptime' /
   data ctbl( 1020: 1028) / 'startstep' /
   data ctbl( 1029: 1038) / '__________' /
   data ctbl( 1039: 1048) / '__________' /
   data ctbl( 1049: 1058) / '__________' /
   data ctbl( 1059: 1068) / '__________' /
   data ctbl( 1069: 1078) / '__________' /
   data ctbl( 1079: 1080) / 'pi' /
   data ctbl( 1081: 1083) / 'inf' /
   data ctbl( 1084: 1088) / 'false' /
   data ctbl( 1089: 1092) / 'true' /
   data ctbl( 1093: 1094) / 'no' /
   data ctbl( 1095: 1097) / 'yes' /
   data ctbl( 1098: 1101) / 'open' /
   data ctbl( 1102: 1107) / 'closed' /
   data ctbl( 1108: 1110) / 'off' /
   data ctbl( 1111: 1112) / 'on' /
   data ctbl( 1113: 1122) / 'undefined_' /
   data ctbl( 1123: 1132) / '__________' /
   data ctbl( 1133: 1142) / '__________' /
   data ctbl( 1143: 1152) / '__________' /
   data ctbl( 1153: 1162) / '__________' /
   data ctbl( 1163: 1169) / 'integer' /
   data ctbl( 1170: 1173) / 'real' /
   data ctbl( 1174: 1178) / 'ratio' /
   data ctbl( 1179: 1183) / 'bytes' /
   data ctbl( 1184: 1187) / 'page' /
   data ctbl( 1188: 1192) / 'pages' /
   data ctbl( 1193: 1202) / 'storage___' /
   data ctbl( 1203: 1212) / 'endstorage' /
   data ctbl( 1213: 1222) / '__________' /
   data ctbl( 1223: 1232) / '__________' /
   data (isto(i),i=1,10) / 39,297,18,379,9,419,4,447,11,465 /
   data (isto(i),i=11,20) / 1,1, 2,1, 3,1, 4,1, 5,1 /
   data (isto(i),i=21,30) / 6,1, 7,1, 8,1, 9,1, 10,1 /
   data (isto(i),i=31,40) / 11,1, 12,1, 13,1, 14,1, 15,1 /
   data (isto(i),i=41,50) / 16,1, 17,1, 18,1, 19,1, 20,1 /
   data (isto(i),i=51,60) / 21,1, 22,1, 23,1, 24,1, 25,1 /
   data (isto(i),i=61,70) / 26,1, 27,1, 28,1, 29,1, 30,1 /
   data (isto(i),i=71,80) / 31,1, 32,1, 33,1, 34,1, 35,1 /
   data (isto(i),i=81,90) / 36,1, 37,1, 38,1, 39,1, 40,1 /
   data (isto(i),i=91,100) / 41,1, 42,1, 43,1, 44,1, 45,1 /
   data (isto(i),i=101,110) / 46,1, 47,1, 48,1, 49,1, 50,2 /
   data (isto(i),i=111,120) / 52,1, 53,2, 55,1, 56,1, 57,1 /
   data (isto(i),i=121,130) / 58,1, 59,1, 60,2, 62,1, 63,1 /
   data (isto(i),i=131,140) / 64,1, 65,1, 66,1, 67,1, 68,7 /
   data (isto(i),i=141,150) / 75,10, 85,12, 97,15, 112,10, 122,9 /
   data (isto(i),i=151,160) / 143,7, 150,6, 156,6, 162,5, 167,8 /
   data (isto(i),i=161,170) / 175,7, 182,4, 186,5, 191,5, 196,8 /
   data (isto(i),i=171,180) / 204,9, 213,3, 216,5, 221,4, 225,7 /
   data (isto(i),i=181,190) / 232,4, 236,7, 243,5, 248,4, 252,3 /
   data (isto(i),i=191,200) / 255,5, 260,6, 266,7, 273,8, 281,13 /
   data (isto(i),i=201,210) / 294,6, 300,4, 304,6, 310,7, 317,2 /
   data (isto(i),i=211,220) / 319,4, 323,4, 327,5, 332,5, 337,5 /
   data (isto(i),i=221,230) / 342,8, 350,3, 353,6, 359,2, 361,5 /
   data (isto(i),i=231,240) / 366,4, 370,7, 377,10, 387,8, 395,11 /
   data (isto(i),i=241,250) / 406,5, 411,8, 419,5, 424,4, 428,3 /
   data (isto(i),i=251,260) / 431,6, 437,3, 440,5, 445,8, 453,3 /
   data (isto(i),i=261,270) / 456,2, 458,2, 460,2, 462,2, 464,4 /
   data (isto(i),i=271,280) / 468,3, 471,8, 479,5, 484,4, 488,2 /
   data (isto(i),i=281,290) / 490,3, 493,3, 496,3, 499,3, 502,4 /
   data (isto(i),i=291,300) / 506,4, 510,4, 514,10, 524,10, 0,0 /
   data (isto(i),i=301,310) / 534,10, 544,10, 554,10, 564,3, 567,4 /
   data (isto(i),i=311,320) / 571,3, 574,2, 576,5, 581,4, 585,5 /
   data (isto(i),i=321,330) / 590,9, 599,5, 604,5, 609,5, 614,4 /
   data (isto(i),i=331,340) / 618,3, 621,3, 624,6, 630,3, 633,3 /
   data (isto(i),i=341,350) / 636,3, 639,4, 643,4, 647,4, 651,4 /
   data (isto(i),i=351,360) / 655,4, 659,4, 663,5, 668,5, 673,5 /
   data (isto(i),i=361,370) / 678,5, 683,5, 688,6, 694,3, 697,3 /
   data (isto(i),i=371,380) / 700,4, 704,4, 708,4, 712,3, 715,3 /
   data (isto(i),i=381,390) / 718,3, 728,2, 738,10, 748,9, 757,7 /
   data (isto(i),i=391,400) / 764,7, 771,8, 779,8, 787,7, 794,7 /
   data (isto(i),i=401,410) / 801,8, 809,5, 814,6, 820,4, 824,1 /
   data (isto(i),i=411,420) / 825,1, 826,4, 830,6, 836,5, 841,5 /
   data (isto(i),i=421,430) / 846,7, 856,7, 866,10, 876,10, 886,10/
   data (isto(i),i=431,440) / 896,7, 903,7, 910,7, 917,8, 925,8 /
   data (isto(i),i=441,450) / 933,8, 941,7, 948,4, 952,10, 962,10 /
   data (isto(i),i=451,460) / 972,10, 982,10, 992,10, 1002,1, 1003,9/
   data (isto(i),i=461,470) / 1012,8,1020,9,1029,10,1039,10,1049,10/
   data (isto(i),i=471,480) / 1059,10,1069,10,1079,2,1081,3,1084,5/
   data (isto(i),i=481,490) / 1089,4,1093,2,1095,3,1098,4,1102,6/
   data (isto(i),i=491,500) / 1108,3,1111,2,1113,9,1123,10,1133,10 /
   data (isto(i),i=501,510) / 1143,10,1153,10,1163,7,1170,4,1174,5/
   data (isto(i),i=511,520) / 1179,5,1184,4,1188,5,1193,7,1203,10/
   data (isto(i),i=521,524) / 1213,10,1223,10 /
   data (isto(i),i=525,799) / 275*888888888 /
   data unit05 /   5  /
   data unit06 /   6  /
   data unit08 /  28  /
   data unit09 /  29  /
   data bkfile /  39  /
   data bratio / 2.0 /
   data fltlen / 8 /
   data intlen / 4 /
   data ctblen /  1232 /
   data strcnt / 514 /
   data ishcnt /  64 /
   data iempty /   1 /
   data intcnt / 211 /
   data base1  /  10 /
   data base2  /   0 /
   data cptr   /1500 /
   data rempty /  10 /
   data reacnt /  30 /
   data inull  / 888888888 /
   data iinf   /2000000000 /
   data ten    /10.0d0 /
   data zero   / 0.0d0 /
   data one    / 1.0d0 /
   data two    / 2.0d0 /
   data half   / 0.5d0 /
   data rnull  / 88888.88888d0 /
   data rinf   / 1.0d18 /
   data divzro / 1.0d-18 /
   data rmargn / 1.0d-8 /
   data pi     / 3.141592653589793d0 /
   data stoflt / 9000 /      ! wsm + thl manual modification for bpa emtp
   data stocha / 9000 /      ! wsm + thl manual modification for bpa emtp
end block data

!
! end of file data.f90
!
