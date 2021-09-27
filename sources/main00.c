/*-*- mode: c; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-*/

/*
 * file: main00.c
 */

/*
 * program gemtp.
 */

#include "main00.h"

/***********************************************************************/
/*                                                                     */
/*    --------------- Electromagnetic Transients Program ------------  */
/*                    methods development branch, route eogb           */
/*                    division of system engineering                   */
/*                    Bonneville Power Administration                  */
/*                    p. o. box 3621                                   */
/*                    Portland, Oregon  97208                          */
/*                    U.S.A.     phone: (503) 230-4404                 */
/*                                                                     */
/*    The fortran comment-card text now being read represents a        */
/*    summary introduction and explanation which applies to a very     */
/*    large program development referred to by the title of            */
/*    'electromagnetic transients program'  (abbreviated  'emtp' ,     */
/*    or   't.p.'  in the older notation).                             */
/*                                                                     */
/*    In general terms, the purpose of this work is to simulate        */
/*    transient phenomena in power system networks, taking into        */
/*    account traveling waves (electromagnetic transients) on          */
/*    transmission lines, saturation of transformers, nonlinearities   */
/*    of surge arresters, etc.   while not so restricted in theory,    */
/*    the most common program application is for the simulation of     */
/*    switching surges which propagate on power network transmission   */
/*    lines.   for a more detailed explantion of the modeling          */
/*    techniques which are used, the reader is referred to the         */
/*    manual for this program (840 pages for the version dated         */
/*    march, 1983).  ).    While older issues were titled              */
/*    "emtp user's manual",  beginning in september of 1980            */
/*    this work is now called the  "emtp rule book" .                  */
/*                                                                     */
/*    The utpf is a large 80-column bcd card-image file, to be used    */
/*    as input to e/t programs.   e/t programs machine translate this  */
/*    utpf code into legal fortran for most computer systems of        */
/*    interest (ibm, cdc, univac, honeywell, dec pdp-10, dec vax-11,   */
/*    prime, sel, apollo, hitachi, facom, harris, etc.).               */
/*                                                                     */
/*    In conformity with long-standing BPA policy, as well as the      */
/*    more recent (february 19, 1975) federal freedom of information   */
/*    act, dissemination of these program materials is freely made     */
/*    to any and all interested parties.   a fee to cover reproduction,*/
/*    handling, and mailing costs may be assessed against the          */
/*    organization receiving the material, however.   no claim or      */
/*    warranty as to the usefulness, accuracy, fidelity, or            */
/*    completeness of these materials is (or ever has been) in any     */
/*    way expressed or implied.                                        */
/*                                                                     */
/***********************************************************************/

extern FILE *lunit[ 16 ];
extern char buff10[ 80 ];
extern int lunsav[ 15 ];
extern int iprsov[ 39 ];
extern int kwtvax;
extern int nchain;
extern int lastov;
extern int kill;
extern int numdcd;
extern int m4plot;
extern int iprsup;
extern int muntsv[ 40 ];
extern int noutpr;
extern char abuff[ 80 ];
extern char texcol[ 80 ];
extern time_t cputime;
extern char blank;

int main(int argc, char *argv[])
{
  int n1;

  lunit[ 0 ] = stderr;
  lunit[ 4 ] = stdin;
  lunit[ 5 ] = stdout;
  nchain = -1;
  lastov = 0;
  kill = 0;
  time(&cputime);
  do {
  a2000:
    if((kill == 0) || (kill = 9999))
      goto a2001;
    else if(kill == 7733) {
      fprintf(lunit[ 5 ], "\"main00\" intercept of \"begin\" request.\n");
      kill = 0;
      numdcd = 0;
      nchain = 1;
      main10();
      goto a2000;
    } else {
      if(nchain > 51)
        goto a2001;
      nchain = 51;
    }

  a2001:
    n1 = nchain;
    if(n1 > 30)
      n1 -= 30;
    if(n1 <= 0)
      n1 = 1;
    iprsup = iprsov[ n1 - 1 ];
    if(nchain > 20) {
      if(m4plot == 1)
        emtspy();
      if(nchain > 29)
        if(nchain > 31)
          if(nchain > 39)
            if(nchain > 41)
              if(nchain > 42)
                if(nchain > 44)
                  if(nchain > 45)
                    if(nchain > 47)
                      if(nchain > 51)
                        if(nchain > 52)
                          if(nchain > 53)
                            if(nchain > 54)
                              if(nchain > 55)
                                fprintf(lunit[ 5 ], "Illegal nchain in main00. %d\n", nchain);
                              else
                                over55();
                            else
                              over54();
                          else
                            over53();
                        else
                          over52();
                      else
                        over51();
                    else
                      over47();
                  else
                    over45();
                else
                  over44();
              else
                over42();
            else
              over41();
          else
            over39();
        else
          over31();
      else
        over29();
    } else {
      if((nchain == 12) || (nchain == 2))
        main10();
      else {
        if(nchain == -1)
          move0(&iprsov[ 0 ], LL34);
        erexit();
        nchain = 0;
        if(nchain <= 20)
          main10();
      }
    }
  } while(1);

  return nchain;
}

/*    The present module  main00  is always in memory.   It is the      */
/*    highest level module of a program which has two levels of         */
/*    overlaying.   It calls primary level overlays only (directly),    */
/*    based on the value of variable  'nchain' .   The following        */
/*    legitimate values, and the meaning of the associated overlay      */
/*    calls, exist .....                                                */
/*    1-20.  For overlays 1, 2, ..., 20, which are secondary-level      */
/*           overlays, a call must be first made to the controlling     */
/*           primary-level overlay.   thus for such  'nchain'  values,  */
/*           control is transfered first to module  main10 .   This     */
/*           is the only case where calls to overlays are not made      */
/*           directly.                                                  */
/*                                                                      */
/*    29.  Completion of statistics (monte carlo) study, where variable */
/*         maxima of the different case solutions are read off the      */
/*         disk, and are processed statistically to produce             */
/*         cumulative distribution functions, etc.                      */
/*                                                                      */
/*    31.  Plot routine, for graphical output of transients.            */
/*         the program also terminates execution here, usually,         */
/*         after writing an end-of-information mark on the              */
/*         plot tape (whether or not the user has plotted anything).    */
/*                                                                      */
/*    39.  Supporting routine which generates emtp branch               */
/*         cards for the frequency-dependent representation of          */
/*         an untransposed transmission line.   This is the             */
/*         "marti setup"  code, named after Dr. Jose Marti of           */
/*         Vancouver and Caracas (see 1981 ieee Pica paper).            */
/*                                                                      */
/*    41.  Supporting routine which calculates transformer matrices  (r)*/
/*         and  (l)  from short-circuit and open-circuit data.          */
/*                                                                      */
/*    42.  Supporting routine which converts an rms voltage vs. current */
/*         saturation characteristic into an instantaneous flux vs.     */
/*         current characteristic.                                      */
/*                                                                      */
/*    43.  Supporting routine which calculates weighting functions      */
/*         a1(t)  and  a2(2)  for the zero-sequence mode of a           */
/*         distributed line which has frequency-dependent line          */
/*         constants  r  and  l .                                       */
/*                                                                      */
/*    44.  Supporting routine which calculates line constants for       */
/*         overhead transmission lines by means of Carson's formula.    */
/*         This is a modified version of what was originally (until     */
/*         january 1975) the completely-separate BPA line-constants     */
/*         program.                                                     */
/*                                                                      */
/*    45.  Supporting routine of  'semlyen setup'  code.   The output   */
/*         is a group of punched cards, as are required for the emtp    */
/*         simulation of a transmission circuit using  Semlyen          */
/*         recursive convolution modeling.                              */
/*                                                                      */
/*    47.  Supporting routine of  'cable constants'  code.   The        */
/*         primary function is to calculate  (r),  (l),  %  (c)         */
/*         matrices for a multi-phase system of single-core coaxial     */
/*         cables.                                                      */
/*                                                                      */
/*    51.  Printing of introductory paragraph of error-message          */
/*         termination ('you lose, fella, ... '), plus error-message    */
/*         texts for  'kill'  codes numbered  1  through  50 .          */
/*         The exiting linkage is to the last error overlay.            */
/*                                                                      */
/*    52.  Error message texts for  'kill'  codes numbered  51          */
/*         the exiting linkage is to the last error overlay.            */
/*                                                                      */
/*    53.  Error message texts for  'kill'  codes numbered  91          */
/*         through  150.  The exiting linkage is to the last            */
/*         error overlay.                                               */
/*                                                                      */
/*    54.  Error message texts for  'kill'  codes numbered  151         */
/*         through  200.   The exiting linkage is to the                */
/*         last error overlay.                                          */
/*                                                                      */
/*    55.  Final error overlay.  Messages for  kill = 201               */
/*         onward are contained, as well as summary statistics          */
/*         --- table sizes and timing figures for the run.              */
/*         The exiting linkage is generally to module  over1  (to read  */
/*         a new data case), but may be to module  over31 (for final    */
/*         case termination).                                           */

/*
void stoptp(void)
{
  sprintf(texcol, "%80c", abuff);
  if((nchain == 31) && (lastov == 1) && (kill == 9999)) {
    exit(-1L);
  } else {
    fprintf(stderr, "\nTemporary error stop in \"stoptp\". nchain, lastov = %d %d last-read card image abuff follows ....\n%s\n", nchain, lastov, texcol);
  }
}

bool copyr(float d1, float *to, size_t kk)
{
  bool bReturnValue = false;

  if(to) {
    for(size_t i = 1; i <= kk; i++) {
      to[ i - 1 ] = d1;
    }
    bReturnValue = true;
  }
  return bReturnValue;
}

bool copyi(int n1, int *ito, size_t kk)
{
  bool bReturnValue = false;

  if(ito) {
    for(size_t i = 1; i <= kk; i++)
      ito[ i - 1 ] = n1;
    bReturnValue = true;
  }
  return bReturnValue;
}

bool copya(char text1, char *text2, size_t kk)
{
  bool bReturnValue = false;

  if(text2) {
    if(memset((void *) text2, (unsigned char) text1, kk))
      bReturnValue = true;
  }
  return bReturnValue;
}

void erexit(void)
{
  muntsv[ 1 ] = 49;
  kwtvax = 0;
  datain();
}

void runtym(float *d1, float *d2)
{
  time_t now;

  time(&now);
  *d1 = difftime(now, cputime);
  *d2 = 0.0;
}

bool settym(void)
{
  bool bReturnValue = false;

  if(time(&cputime) == NULL)
    fprintf(lunit[ 5 ], "Error in a private place.\n");
  else
    bReturnValue = true;
  return bReturnValue;
}

void time44(char a[ 2 ][ 4 ])
{
  time_t now;
  struct tm t;

  char timestring[ 8 ];
  time(&now);
  t = *localtime(&now);
  sprintf(timestring, "%2d:%2d:%2d", t.tm_hour, t.tm_min, t.tm_sec);
  memcpy((void *) &a[ 0 ][ 0 ], (void *) &timestring[ 0 ], 4);
  memcpy((void *) &a[ 1 ][ 0 ], (void *) &timestring[ 4 ], 4);
}

void cimage(void)
{
  int n8 = 0;
  char charc = 'c';
  char chcont;
  char csepar;
  char text1;
  char text2;
  char textcol[ 81 ];
  char text4[] = "9";
  char chtacs[] = "tacs";
  char text5[] = "blank";
  char textax[] = "attachpunchoutputsavespydatadisableenablereturnnewfilenewepsilndeletemonitorlistofflistonvintageoldfilestopwatch5commentwidthunits";
  char textay[] = "apoutsspydernedemlflnvoldstwcomwiu";
  int jpntr[ 23 ] = { 0, 1, 2, 3, 4, 6, 8, 9, 10, 12, 14, 15, 17, 19, 20, 22, 24, 25, 26, 28, 29, 30 };
  int n11 = 0;
  int n12 = 99999;
  int n13 = 99999;

  if(iprsup >= 10)
    fprintf(lunit[ 5 ], " Begin cimage. lunit[ 4 ], lunit[ 5 ], noutpr, numdcd = %ld, %ld, %d, %d\n", lunit[ 4 ], lunit[ 5 ], noutpr, numdcd);

 a1000:
  do {
    if(m4plot == 1)
      emtspy();
    if(lunit[ 4 ] > 0)
      fscanf(lunit[ 4 ], "%80s", buff10);
    if(lunit[ 4 ] < 0)
      nextcard();
    if(kill > 0)
      goto a4000;
    if(lunsav[ 4 ] != -5)
      ++numdcd;
    sscanf(abuff, "%c%c", &text1, &text2);
    if(text1 != charc)
      goto a3034;
    if(text2 != blank)
      goto a3034;

  a1036:
    if(noutpr != 0)
      goto a1000;
    if(n11 != 0)
      goto a1000;
    if(KOL132 == 132)
      fprintf(lunit[ 5 ], " Comment card. %*c\b 1%29c\n", 37, ' ', abuff);
    goto a1000;

  a3034:
    if(noutpr != 0)
      goto a3035;
    if(KOL132 == 132)
      fprintf(lunit[ 5 ], "%*c\b%80c\n", 51, ' ', buff10);
    if(KOL132 != 132)
      fprintf(lunit[ 5 ], "%*c\b1%29c\n", 51, ' ', abuff);

  a3035:
    if(n13 > 0)
      goto a3011;
    printf(" %5d% :%72c", abuff);
    n13 = n12;

  a3011:
    --n13;
    fscanf(abuff, "%6c", &text2);

    if (text2 != text5)
      goto a3040;
    if (n8 == 6)
      goto a3044;
    memset(abuff, blank, 80);
    goto a3233;

  a3040:
    if (chcont == text4)
      goto a3233;
    sscanf(abuff, "%80s", textcol);
    if ((strncmp(abuff, "$listoff", 8) != 0) && (strncmp(abuff, "$liston", 8) != 0))
      goto a3042;
    goto a3246;

  a3042:
    if (chcont == chtacs)
      goto a3233;
    if (n8 != 6)
      goto a1144;

  a3044:
    if (noutpr == 0)
      fprintf(lunit[ 6 ], "+comment card (implicit).\n");
  } while (1);
  for (int k = 0; k < 80; k++) {
    if (textcol[ k ] == csepar)
      goto a3237;
    if (textcol[ k ] == chcont)
      goto a3237;
  }

 a3233:
  kolbeg = INT_MIN;
  goto a7014;

 a3237:
  kolbeg = 1;
  goto a7014;

 a3246:
  kolbeg = 2;
  nright = 0;
  for (int i = 0; i < 200; i++) {
    n1 = jpntr[ i ];
    n2 = jpntr[ i + 1 ] - 1;
    if (n2 < 0)
      goto a3319;
    if (iprsup >= 35)
      fprintf(lunit[ 5 ], " Special-request word %d . %60c\n", i, textax[ n1 ]);
    if (strncpy(&textay[ n1 ], blank, 1) == 0)
      goto a3306;
    l = 0;
    n3 = n2 = n1 + 1;
    if (n3 != nfrfld)
      goto a3306;
    for (j = n1; j < n2; j++) {
      ++l;
      if (texta6[ l ] != textax[ j ])
        goto a3306;
    }
    if (iprsup >= 2)
      fprintf(lunit[ 5 ], " keyword found. i, n8 = %5d %5d     texta6, texay = %7c %7c\n", i, n8, texta6[ 0 ], textay[ i ]);

  a3294:
    if (n8 != 6)
      goto a3301;
    if (i != 7)
      goto a1036;

  a3301:
    n8 = i;
    switch(n8) {
    case 1:
      goto a4100;
      break;

    case 2:
      goto a4200;
      break;

    case 3:
      goto a4300;
      break;

    case 4:
      goto a4400;
      break;

    case 5:
      goto a4500;
      break;

    case 6:
      goto a4600;
      break;

    case 7:
      goto a4700;
      break;

    case 8:
      goto a4800;
      break;

    case 9:
      goto a4900;
      break;

    case 10:
      goto 5000;
      break;

    case 11:
      goto a5100;
      break;

    case 12:
      goto a5200;
      break;

    case 13:
      goto a5300;
      break;

    case 14:
      goto a5400;
      break;

    case 15:
      goto a5500;
      break;

    case 16:
      goto a5600;
      break;

    case 17:
      goto a5700;
      break;

    case 18:
      goto a5800;
      break;

    case 19:
      goto a5900;
      break;

    case 20:
      goto a6000;
      break;

    case 21:
      goto a6100;
      break;
    }

  a3306:
    if (strncmp(texta6[ 0 ], textay[ i - 1 ], 80) == 0)
      goto a3294;
  }

 a3319:
  fprintf(lunit[ 5 ], " Illegal $-card.  Stop at s.n. 3319 of \"cimage\" .\n");
  stoptp();

 a4100:
  strncpy(text1, "attach", 80);
  goto a4506;

 a4200:
  strncpy(text2, textax[ 1 ], 80);

 a4206:
  n2 = lunit[ 6 ];

 a4209:
  nfrfld = 1;
  freone(&d11);
  n1 = (int) d11;
  if (n1 <= 0)
    n1 = n2;
  if (n8 == 8)
    goto a4817;
  if (noutpr == 0)
    fprintf(lunit[ 5 ], "+copy file %d  to %6c .\n", n1, text1);
  fseek(lunit[ 5 ], 0, SEEK_SET);
  do {
    fscanf(n1, "%c80", &aupper);
    if (n8 == 2)
      fprintf(lunit[ 6 ], "%80c", aupper);
    if (n8 == 3)
      fprintf(lunit[ 5 ]. "%*c\brecord %d .  1%80c\n", 20, ' ', k, aupper);
  } while (ferror(lunit[ 5 ]) == 0);
  clearerr(lunit[ 5 ]);

 a4249:
  fseek(lunit[ 5 ], 0, SEEK_SET);
  goto a1000;

 a4300:
  strncpy(text1, textax[ 2 ], 80);
  goto a4206;

 a4400:
  strncpy(text1, "saved", 80);
  goto a4506;

 a4423:
  fclose(n7);
  n7 = fopen(filen, "w+");
  fseek(n6, 0, SEEK_SET);
  do {
    fscanf(n6, "%80c", aupper);
    fprintf(n7, "%80c", aupper);
  } while(ferror(n6) == 0);
  clearerr(n6);

 a4436:
  fclose(n7);
  fseek(n6, 0, SEEK_SET);
  goto a1000;

 a4500:
  strncpy(text1, "spying", 80);
  muntsv[ 1 ] = 2288;
  muntsv[ 0 ] = munit5;
  munit5 = 17;
  kfile5 = 1;

 a4506:
  n4 = 0;
  filen[ 0 ] = 0;
  for (k = kolbeg - 1; k < 80; k++) {
    if (textcol[ k ] == blank)
      goto a4532;
    if (textcol[ k ] == csepar)
      goto a4536;
    if (textcol[ k ] == '(')
      goto a4536;
    ++n4;
    sprintf(&filen[ n4 = 1 ], "%80c", textcol);
  }

 a4532:
  k = 80;

 a4536:
  kolbeg = k + 1;
  nfrfld = 1;
  freone(&d11);
  n7 = (int) d11;
  if (n8 != 4)
    goto a4557;
  freone(&d11);
  n6 = (int) d11;

 a4557:
  if (n6 == 0)
    n6 = lunit[ 6 ];
  if (n8 == 5)
    n7 = munit[ 4 ];
  if (n7 > 0)
    goto a4570;
  for (k = 0; k < 15; k++)
    if (lunsav[ k ] <= 0)
      goto a4568;
  fprintf(lunit[ 5 ], "\n%*s", 10, "error,");
  fprintf(lunit[ 5 ], "%*c\b All i/o channels occupied.  Kill run at s.n. 4566 of \"cimage\" .\n%*d\n", 5, ' ', 20, lunsav);
  stoptp();

 a4568:
  n7 = k;

 a4570:
  if (noutpr == 0)
    fprintf(lunit[ 5 ], "+%6c file %*c unit = %d\n", text1, 25, filen, n7);
  if (n8 == 4)
    goto a4423;
  if (n8 == 9)
    goto a4907;
  if (n8 == 11)
    goto a5106;
  if (n8 == 16)
    goto a5608;
  fclose(n7);
  n7 = fopen(filen, "w+");
  fseek(n7, 0, SEEK_SET);
  if (n8 == 5)
    spying();
  goto a1000;

 a4600:
  if (noutpr == 0)
    fprintf(lunit[ 5 ], "+begin data to be ignored.\n");
  goto a1000;

 a4700:
  if (noutpr == 0)
    fprintf(lunit[ 5 ], "+end of data to be ignored.\n");
  goto a1000;

 a4800:
  n2 = lunit[ 3 ];
  goto a4209;

 a4817:
  fclose(n1);
  if (noutpr == 0)
    fprintf(lunit[ 5 ], "+close file on unit %d .\n", n1);
  if (n1 == lunit[ 4 ])
    noutpr = 1;
  goto a1000;

 a4900:
  strncpy(text1, "newfil", 80);
  n2 = lunit[ 3 ];
  goto a4506;

 a4907:
  n7 = fopen(filen, "w+");
  goto a1000;

 a5000:
  nfrfld = 1;
  d1 = epsiln;
  freone(&epsiln);
  if (noutpr == 0)
    fprintf(lunit[ 5 ], "+epsiln change.  old, new = %f %f\n", d1, epsiln);
  goto a1000;

 a5100:
  strncpy(text1, "delete", 80);
  goto a4506;

 a5106:
  n7 = fopen(filen, "w+");
  goto a1000;
  fclose(n7);
  goto a1000;

 a5200:
  if (noutpr != 0)
    goto a5219;
  printf("%80c\n", buff10);
  printf("+crt monitor.  card number = %d\n", numdcd);
  goto a1000;

 a5300:
  if (noutpr != 0)
    goto a5324;
  fprintf(lunit[ 5 ], "+turn off input listing at card %d\n", numdcd);
  noutpr = 1;
  goto a1000;

 a5400:
  fprintf(lunit[ 5 ], "%*c\b1$liston\n", 51, ' ');
  noutpr = 0;
  fprintf(lunit[ 5 ], "+turn on input listing at card %d\n", numdcd);
  goto a1000;

 a5500:
  nfrfld = 1;
  freone(&d11);
  moldat = (int) d11;
  if (noutpr == 0)
    fprintf(lunit[ 5 ], "+new moldat = %d%*c\b(data vintage)\n", moldat, 5, ' ');
  goto a1000;

 a5600:
  strncpy(text1, "oldil", 80);
  n2 = lunit[ 1 ];
  goto a4506;

 a5608:
  fclose(n7);
  n7 = fopen(filen, "w+");
  goto a1000;

 a5700:
  fprintf(lunit[ 5 ], "+stop execution immediately in \"cimage\".\n");
  stoptp();

 a5800:
  nfrfld = 1;
  freone(&d11);
  n12 = (int) d11;
  n13 = n12;
  if (noutpr == 0)
    fprintf(lunit[ 5 ], "+toggle comment card destruction flag. %d\n", n11);
  goto a1000;

 a6000:
  stoptp();

 a6100:
  nfrfld = 1;
  frefld(&xopt);
  frefld(&copt);
  if (noutpr == 0)
    fprintf(lunit[ 5 ], "+new xopt, copt = %d %d\n", xopt, copt);
  xunits = 1000;

 a4000:
  fprintf(lunit[ 5 ], "\n %*c\b\n End of file encounted in \"cimage\" while attempting to read another data card.  Stop.\n %*c\b", 85, '=', 85, '=');
  stoptp();

 a7014:
  if (inecho == 0)
    return;

  cecho {
    if (nchain <= 15)
      ++ipntv[ 10 ];
    fprintf(inecho, "%80c", buff10);
    return;
  };

  ibrinc {
    ++ibr;
    xoptbr[ ibr - 1 ] = xopt[ 0 ];
    coptbr[ ibr - 1 ] = copt[ 0 ];
    return;
  };
}

void ioerr(int naddr)
{
  naddr = 0;
}

void caterr(int naddr, int koderr)
{
  naddr = 0;
  koderr = 0;
}

float *locf(float array)
{
  return &array;
}

int *locint(int array)
{
  return &array;
}

char *locstr(char *str)
{
  return str;
}

char *locchar(char c)
{
  return &c;
}

void trgwnd(float x, float d17)
{
  d17 = x;
  if (abs(x) >= 25000.) {
    n13 = (int) (x / (2 * M_PI));
    d17 -= n13 * 2 * M_PI;
    if (iprsup >= 1)
      fprintf(stdout, " Angle unwind in \"trgwnd\" called by \"rfunl1\".  nchain, x, d17 = %d %f %f\n", nchain, x, d17);
  }
}

int ifunl1(float d1)
{
  return (int) d1;
}

float complex cfunl1(float x)
{
  return (float complex) x;
}
*/

/*
 * end of file: main00.c
 */
