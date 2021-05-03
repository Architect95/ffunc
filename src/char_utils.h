/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2020  The R Core Team
 * 
 *  Modified on 02 May 2021 by Architect95
 *
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Pulic License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */


#ifndef CHAR_UTILS_H
#define CHAR_UTILS_H

#include <ctype.h>
#include "ffunc.h"
#include "ffunc_utils.h"
#include <wchar.h>


typedef struct {
 char *data;
 size_t bufsize;
 size_t defaultSize;
} StringBuffer;

extern StringBuffer strbuff;

static const unsigned char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5 };

#include "valid_utf8.h"


bool isValidUtf8(const char *string, size_t length);


typedef struct {
  const char * restrict str_ptr;
  int nbytes;
  cetype_t enc;
} CharLenCE;

unsigned char utf8clen(const char c);

size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps);

void *allocStringBuffer(size_t blen, StringBuffer *buf);

void freeStringBuffer(StringBuffer *buf);

bool isStatelessEncoding();

#endif