/*-*- mode: c; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-*/

/*
 * file: main10.h
 */

#ifndef __MAIN10_H
#define __MAIN10_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

/*
 * emtp includes.
 */

#include "blkcom.h"
#include "labcom.h"
#include "tacsar.h"
#include "syncom.h"
#include "umdeck.h"

/*
 * variables.
 */

void over1(void);
void over2(void);
void over5(void);
void over6(void);
void over7(void);
void over8(void);
void over9(void);
void over10(void);
void over11(void);
void over12(void);
void over13(void);
void over14(void);
void over15(void);
void over16(void);
void over20(void);
void stoptp(void);

void (*overlays[])(void) =
  {
   over1,
   over2,
   stoptp,
   stoptp,
   over5,
   over6,
   over7,
   over8,
   over9,
   over10,
   over11,
   over12,
   over13,
   over14,
   over15,
   over16,
   over16,
   over16,
   over16,
   over20,
   NULL
  };

/*
 * Functions prototypes.
 */

void subr10(void);

#endif /* __MAIN10_H */
