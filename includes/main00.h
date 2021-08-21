/*-*- mode: c; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-*/

/*
 * file: main00.h
 */

#ifndef __MAIN00_H
#define __MAIN00_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include <limits.h>
#include <math.h>
#include <complex.h>

#include "blkcom.h"
#include "volt45.h"

#define LL34 34
#define LLBUFF -3333
#define KOL132 132

/*
 * Functions prototypes.
 */

extern void main10(void);
extern void emtspy(void);
extern void over29(void);
extern void over31(void);
extern void over39(void);
extern void over41(void);
extern void over42(void);
extern void over44(void);
extern void over45(void);
extern void over47(void);
extern void over51(void);
extern void over52(void);
extern void over53(void);
extern void over54(void);
extern void over55(void);

void stoptp(void);
bool copyr(float, float *, size_t);
bool copyi(int, int *, size_t);
bool copya(char, char *, size_t);
void erexit(void);
void runtym(float *, float *);
bool settym(void);
void time44(char[2][4]);
int main(int, char *[]);

#endif /* __MAIN00_H */
