/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2020  The R Core Team
 * 
 *  Modified on 08 November 2020 by Architect95
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

#include "char_utils.h"


unsigned /*char*/ utf8clen(const char c)
{
    // This allows through 8-bit chars 10xxxxxx, which are invalid
    if ((c & 0xc0) != 0xc0) return 1;
    return 1 + utf8_table4[c & 0x3f];
}


// From util.c:
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps)
{
  size_t used = mbrtowc(wc, s, n, ps);  // mbrtowc() is in wchar.h
  if((int) used < 0) {
    /* let's try to print out a readable version */
    R_CheckStack2(4*strlen(s) + 10);
    char err[4*strlen(s) + 1], *q;
    const char *p;
    for(p = s, q = err; *p; ) {
      /* don't do the first to keep ps state straight */
      if(p > s) used = mbrtowc(NULL, p, n, ps);
      if(used == 0) break;
      else if((int) used > 0) {
      memcpy(q, p, used);
      p += used;
      q += used;
      n -= used;
      } else {
      sprintf(q, "<%02x>", (unsigned char) *p++);
      q += 4;
      n--;
      }
    }
    *q = '\0';
    error("Invalid multibyte string at '%s'", err);
  }
  return used;
}