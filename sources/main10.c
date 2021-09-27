/*-*- mode: c; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-*/

/*
 * file: main10.c
 */

/*
 * program gemtp.
 */

#include "main10.h"

extern int iprsup;
extern int iprsov[ 39 ];
extern int ktab;
extern int kill;
extern int nchain;

void subr10(void)
{
  int iprcbl;
  int n24;

  ktab = 0;
  do {
    if(nchain > 20)
      return;
    if(kill == 0) {
      n24 = nchain;
      if(n24 < 1)
        n24 = 1;
      iprsup = iprsov[ n24 - 1 ];
      iprcbl = iprsup;
      if(m4plot == 1)
        emtspy();
      if(nchain == 0)
        nchain = 1;
      (overlays[ nchain - 1 ])();
    } else
      nchain = 51;
  } while(1);
}
