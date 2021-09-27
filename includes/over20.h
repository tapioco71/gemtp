/*-*- mode: c; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-*/

/*
 * file: over20.h
 */

#ifndef __OVER20_H
#define __OVER20_H

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

void emtspy(void);
void spying(void);
void window(void);
void prompt(char *);
void flager(void);

#endif /* __OVER20_H */
