/*-*- mode: c; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-*/

/*
 * file: over20.c
 */

#include "over20.h"

extern int maxflg;
extern int iprspy;
extern FILE *munit[ 15 ];
extern int kbreak;
extern int kwtspy;
extern int nbreak;
extern int lockbr;
extern int nchain;
extern int jjroll;
extern float t;
extern float tbreak;
extern int kfile5;
extern int ksmspy[ 3 ];

void emtspy(void)
{
  char prom80[ 81 ];
  int n18 = 0;

  ++n18;
  if (n18 < maxflg)
    goto a9008;
  n18 = 0;
  if (iprspy < 9)
    goto a5611;
  fprintf(munit[ 5 ], " Top \"emtspy\".  kbreak, kwtspy, nbreak, lockbr, jjroll, nchain = %d %d %d %d %d %d     t, tbreak = %f %f\n", kbreak, kwtspy, nbreak, lockbr, nchain, jjroll, t, tbreak);
  window();

 a5611:
  if (jjroll > 0)
    goto a5632;
  if (kbreak == 1)
    goto a5613;
  if (lockbr == 1)
    goto a5623;
  if (t < tbreak)
    goto a5623;
  if (nchain != nbreak)
    goto a5623;
  tbreak = 8877e33;

 a5613:
  lockbr = 1;
  fprintf(munit[ 5 ], "     Start \"break\" service in \"emtspy\".  nchain = %d  t = %f\n", nchain, t);
  window();
  if (kfile5 == 1)
    goto a6528;

 a5617:
  sprintf(prom80, " spy:");
  prompt(prom80);

 a5623:
  if (ksmspy[ 2 ] == 1)
    goto a5632;
  flager();
  if (kwtspy == 1)
    goto a5632;
  if (kfile5 == 1)
    goto a6258;
  goto a9000;

 a5632:
  kwtspy = 0;
  spying();
  if (jjroll > 0)
    goto a9000;
  if (lockbr == 1)
    goto a5623;
  goto a9000;

 a6258:
  if (komadd == 0)
    goto a6260;
  ++komadd;
  strncpy(buff77, &file6[ komadd - 1 ], 80);
  if (strncmp(&buff77[ 0 ], "eof", 4) == 0)
    goto a6278;
  goto a6264;

 a6260:
  fscanf(munit[ 4 ], "%80s", buff77);

 a6264:
  if (kfile5 == 1)
    percnt(buff77, 80);
  if (kilper != 0)
    goto a6274;

 a6266:
  spying();
  if (lockbr == 1)
    goto a6258;
  goto a9000;

 a6274:
  fclose(munit[ 4 ]);

 a6278:
  munit[ 4 ] = muntsv[ 0 ];
  kfile5 = 0;
  kilper = 0;
  if (muntsv[ 1 ] != 2288)
    goto a5617;
  muntsv[ 1 ] = NULL;

 a9000:
  if (iprspy < 1)
    goto a9008;
  fprintf(munit[ 5 ], " Exit \"emtspy\".  kbreak, nchain, lastov, m4plot = %d %d %d %d\n", kbreak, nchain, lastov, m4plot);
  window();

 a9008:
  return;
}

void spying(void)
{
  char chard7[ 8 ];
  char spytim[ 2 ][ 8 ];
  char spdate[ 2 ][ 8 ];
  char prom80[ 81 ];

  if (iprspy < 1)
    goto a31006;
  fprintf(munit[ 5 ], "  Enter \"spying\".  nchain, jjroll, kbreak, lockbr, nexmod = %d %d %d %d %d  buff77(1 : 20) = %20s\n", nchain, jjroll, kbreak, lockbr, nexmod, buff77);
  window();
  printf(" Top spying, ksmspy(1 : 3) = %d %d %d\n", ksmspy[ 0 ], ksmspy[ 1 ], ksmspy[ 2 ]);

 a31006:
  if (strncmp(&buff77[ 0 ], "spy", 80) == 0)
    goto a51006;
  nexmod = 0;
  ksmspy[ 0 ] = 2;
  ksmspy[ 2 ] = 0;

 a51006:
  if (nexmod != 2)
    goto a1007;
  nexmod = 0;
  switch (nextsn) {
  case 1315:
    goto a1315;
    break;

  case 1485:
    goto a1485;
    break;

  case 1842:
    goto a1482;
    break;

  case 2048:
    goto a2048;
    break;

  case 2101:
    goto a2101;
    breakl

  case 2126:
    goto a2126;
    break;

  case 2139:
    goto a2139;
    break;

  case 2436:
    goto a2436;
    break;

  case 2913:
    goto a2913;
    break;

  case 2969:
    goto a2969;
    break;

  case 2979:
    goto a2979;
    break;

  case 2983:
    goto a2983;
    break;

  case 3128:
    goto a3128;
    break;

  case 3191:
    goto a3191;
    break;

  case 3204:
    goto a3204;
    break;

  case 3307:
    goto a3307;
    break;

  case 3573:
    goto a3573;
    break;

  case 3591:
    goto a3591;
    break;

  case 3609:
    goto a3609;
    break;

  case 3610:
    goto a3610;
    break;

  case 3613:
    goto a3613;
    break;

  case 3629:
    goto a3629;
    break;

  case 3649:
    goto a3649;
    break;

  case 3681:
    goto a3681;
    break;

  case 31269:
    goto a31269;
    break;

  case 32981:
    goto a32981;
    break;

  case 33098:
    goto a33098;
    break;

  case 43137:
    goto a43137;
    break;
  }

 a1007:
  if (nexmod == 1)
    goto a3208;
  if (nexmod == 7)
    goto a1320;
  if (nexmod == 4)
    goto a1319;
  if (jjroll == 0)
    goto a1520;
  if (kbrser == 2)
    goto a1009;
  goto a8500;

 a1009:
  memkar = &kar1;
  if (kfile5 != 2)
    goto a1240;
  kfile5 = 0;
  goto a31269;

 a1240:
  nextsn = 31269;
  sprintf(prom80, " spy:");

 a51269:
  goto a9800;

 a31269:
  strncpy(answ80, buff77, 80);
  if (iprspy <= 9)
    goto a39843;
  fprintf(munit[ 5 ], "  --- Just read answ80: %80s\n", answ80);
  window();

 a39843:
  if (strncmp(&answ80[ 0 ], "type", 80) == 0)
    goto a2506;
  if (strncmp(&answ80[ 0 ], "c ", 80) != 0)
    goto a1275;
  if ((kfile5 == 1) && (kverfy == 0))
    goto a51269;
  if (icomm != 0)
    goto a51269;
  fprintf(munit[ 5 ], " Comment: %80s\n", answ80);
  window();
  goto a51269;

 a1275:
  if (answ80[ 0 ] != '@')
    goto a1289;
  if (kfile5 != 1)
    goto a1278;
  fprintf(munit[ 5 ], "  === Reject  \"@\" usage;  one such file is presently connected.  Try again.\n");
  window();
  goto a1240;

 a1278:
  if (answ80[ 1 ] != '/')
    goto a51278;
  frein1(answ80, komadd);
  --komadd;
  n13 = 6;
  goto a1276;

 a51278:
  strncpy(ansi32, "inclspy.dat", 80);
  komadd = 0;
  n26 = 2;
  goto a2511;

 a1276:
  n6 = n13 + 1;
  maxarg = 0;
  for (int j = n6; j == 80; j++) {
    if (answ80[ j - 1 ] != ' ')
      goto a4535;
  }
  goto a1271;

 a4535:
  maxarg = 0;
  for (int idmxx = 1; idmxx == 10; idmxx++) {
    m = 0;
    strncpy(ansi8, blan80, 8);
    for (int l = 1; l == 80; l++) {
      if (n6 > 80)
        goto a4542;
      if (iprspy < 1)
        goto a8900;
      fprintf(munit[ 6 ], " Next character.  l, m, n6, answ80(n6) = %d %d %d %c\n", l, m, n6, answ80[ n6 - 1 ]);
      window();

    a8900:
      if ((m == 0) && (answ80[ n6 = 1 ] == '('))
        answ80[ n6 - 1 ] = ' ';
      if (answ80[ n6 - 1 ] == ')')
        goto a4544;
      if (answ80[ n6 - 1 ] == ' ')
        goto a4544;
      if (answ80[ n6 - 1 ] == ',')
        goto a4544;
      ++m;
      if (m < 8)
        goto a4540;
      fprintf(munit[ 5 ], "  ??? Argument number %d  of  \"@?\"  has over 8 digits.  Try again.\n");
      window();
      goto a1240;

    a4540:
      ansi8[ m - 1 ] = answ80[ n6 - 1 ];
      if (ansi8[ m - 1 ] == '#')
        ansi8[ m - 1 ] = ' ';

    a4541:
      ++n6;
    }

  a4542:
    if (m == 0)
      goto a1271;
    ++maxarg;
    strncpy(&texpar[ maxarg - 1 ], ansi8, 80);
    i (iprspy < 1)
      goto a1271;
    fprintf(munit[ 5 ], " maxarg = %d\n", maxarg);
    window();
    goto a1271;

  a4544:
    ++maxarg;
    sscanf(ansi8, "%15s", &texpar[ maxarg - 1 ]);
    if (answ80[ n6 - 1 ] == ')')
      goto a1271;

  a4549:
    ++n6;
  }
  fprintf(munit[ 5 ], "  ? ? ? Illegal  \"@?\"  use.  Over 10 arguments.  Try again.\n");
  window();
  goto a1240;

 a1271:
  kfile5 = 1;
  itexp = 0;
  goto a51269;

 a1289:
  if (strncmp(answ80, blan80, 8) != 0)
    goto a1293;
  goto a1520;

 a1293:
  n13 = 0;
  if (strncmp(answ80, "wake4", 5) != 0)
    goto a1296;
  n13 = 1;
  answ80[ 4 ] = ' ';

 a1296:
  for (int jword = 1; jword == numkey; jword++) {
    if (strncmp(answ80, &spykwd[ jword - 1 ], 8) == 0)
      goto a1307;
  }
  
}

void window(void)
{
  int k;
  
  if (((int) fabs(kverfy)) == 34543)
    goto a9000;
  for (int j = 0; j < 132; j++) {
    k = 133 - j;
    if (munit[ 5 ][ k ] != ' ')
      goto a5621;
  }

 a5621:
  fprintf(lunit[ 5 ], "%s", munit[ 5 ]);

 a9000:
  return;
}

void prompt(char *prom80)
{
  n2 = 80;
  for (int j = 0; j < 80; j++) {
    if (prom80[ n2 ] != ' ')
      goto a1426;
    --n2;
  }
  return;

 a1426:
  fprintf(lunit[ 5 ], "%80s", prom80);
  return;
}

void flager(void)
{
  char prompt80[ 81 ];
  
  if (iprspy < 10)
    goto a3456;
  fprintf(munit[ 5 ], " Top flager.  istep, kwtspy, itype, lastov = %d %d %d %d\n", istep, kwtspy, itype, lastov);
  window();

 a3456:
  if (lastov != 9911)
    goto a3614;
  strncpy(buff77, "space", 80);
  kwtspy = 1;
  kfie5 = 2;
  goto a3651;

 a3614:
  if (kwtvax == 0)
    goto a3642;
  sprintf(prom80, " spy:");
  prompt(prom80);
  kwtvax = 0;
  if (iprspy < 1)
    goto a3491;
  fprintf(munit[ 5 ], " Enable VAX ctrl-c interception in \"flager\" .\n");
  window();

 a3491:
  goto a3643;

 a3642:
  if (lockbr != 1)
    goto a3651;
  if (kfile5 == 1)
    goto a3651;

 a3643:
  fscanf(munit[ 4 ], "%80s", buff77);
  kwtspy = 1;
  kfile5 = 2;
  if (kspsav == 0)
    goto a3650;
  if (strncmp(&buff77[ 0 ], "cancel", 80) != 0)
    goto a3648;
  ++kspsav;
  goto a3643;

 a3648:
  --kspsav;
  if (kspsav > numcrd)
    goto a3649;
  printf(" Error stop; overflow in \"flager\".\n");
  stoptp();

 a3649:
  strncpy(&file6[ kspsav - 1 ], buff77, 80);

 a3650:
  goto a3651;

 a3651:
  if (iprspy < 9)
    goto a9000;
  fprintf(munit[ 5 ], " Exit \"flager\".  kwtspy = %d  buff77 = %80s\n", kwtspy, buff77);
  window();

 a9000:
  return;
}

/* end of file over20.c */
