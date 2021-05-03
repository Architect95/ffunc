/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2020  The R Core Team
 * 
 *  Modified on 03 May 2021 by Architect95
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

StringBuffer strbuff = {NULL, 0, 8192};

FORCE_INLINE bool isValidUtf8(const char *string, size_t length) {
  return valid_utf8(string, length) == 0;
}

unsigned char utf8clen(const char c)
{
    // This allows through 8-bit chars 10xxxxxx, which are invalid
    if ((c & 0xc0) != 0xc0) return 1;
    return 1 + utf8_table4[c & 0x3f];
}


// From util.c:
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps)
{
  size_t used = mbrtowc(wc, s, n, ps);  // mbrtowc() is in wchar.h
  
  // Throw an error if invalid character, or if n bytes were not enough to 
  // complete a valid character:
  if((int) used < 0) {
    /* let's try to print out a readable version */
    R_CheckStack2(4*strlen(s) + 10);
    char err[4*strlen(s) + 1], *q;
    const char *p;
    for(p = s, q = err; *p; ) {
      /* don't do the first to keep ps state straight */
      if(p > s) {
        used = mbrtowc(NULL, p, n, ps);
      }
      
      if(used == 0) { 
        break;
        
      } else if((int) used > 0) {
        memcpy(q, p, used);
        p += used;
        q += used;
        n -= used;
        
      } else {  // used < 0
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

void *allocStringBuffer(size_t blen, StringBuffer *buf)
{
  size_t blen1, bsize = buf->defaultSize;

  if(blen * sizeof(char) >= buf->bufsize) {
    buf->bufsize = (blen + 1) * sizeof(char);
    buf->data = (char *) realloc(buf->data, buf->bufsize);
    
    blen1 = blen = (blen + 1) * sizeof(char);
    blen = (blen / bsize) * bsize;
    if(blen < blen1) blen += bsize;

    if(buf->data == NULL) {
      buf->data = (char *) malloc(blen);
      if(buf->data) {
        buf->data[0] = '\0';
      }
    } else {
	    buf->data = (char *) realloc(buf->data, blen);
    }
    buf->bufsize = blen;
    if(!buf->data) {
      buf->bufsize = 0;
      error("Could not allocate memory (%u Mb) in C function 'allocStringBuffer'.",
            (unsigned int) blen>>20);
    }
  }
  return buf->data;
}

void freeStringBuffer(StringBuffer *buf)
{
  // if (buf->data != NULL) {  // Modified - check is not needed
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
  // }
}

// Non-exhaustive list of code page identifers for stateless encodings:
const char stateless_encs[][10] = {
  "874", "932", "936", "949", "950", "951",
  "1250", "1251", "1252", "1253", "1254", "1255", "1256", "1257", "1258", 
  "iso88591", "iso88592", "iso88593", "iso88594", "iso88595", "iso88596", 
  "iso88597", "iso88598", "iso88599", "iso885910", "iso885911", "iso885913", 
  "iso885914", "iso885915", "iso885916", 
  "cp1251", 
  "koi8r", "koi8u", "koi8ru", "koi8e", "koi8f",
  "euccn", "eucjp", "euckr", "euctw",
  "gb2312", "big5", "big5hkscs",
  "tis-620"
};  // Only Windows code pages available on Windows 10 by default were listed
static const unsigned char N_STATELESS_ENCS = 
  sizeof(stateless_encs) / sizeof(stateless_encs[0]);

/* Character set references:
 * Windows code pages: https://docs.microsoft.com/en-us/windows/win32/intl/code-page-identifiers
 * Linux charsets:     https://man7.org/linux/man-pages/man7/charsets.7.html
 */

static FORCE_INLINE const char *getLocaleCode(const char *code) {
  SEXP getlocale_call = PROTECT(lang2(install("Sys.getlocale"), 
                                      mkString(code)));
  SEXP locale = eval(getlocale_call, R_BaseEnv);
  UNPROTECT(1);
  return CHAR(asChar(locale));
}

static FORCE_INLINE char *sanitizeCodeSetString(char *c) {
  char *dest = c;
  while (*c != '\0') {
    if (*c != '_' && *c != '-') {
      *dest = tolower(*c);
      ++dest;
    }
    ++c;
  }
  *dest = '\0';
  return c;
}

static FORCE_INLINE char *getCodeSet(const char *lc_ctype, char *buffer, 
                                     unsigned char buflen) {
  
  // Locale specifier strings are operating system-specific. This function
  // assumes that the locale specifier is of the form
  // language[_territory[.codeset]][@modifier]
  
  const char *c = strchr(lc_ctype, '.');
  if (c == NULL) {
    c = lc_ctype;
  } else {
    ++c;  // Move pointer past the "."
  }
  
  char *at = strchr(c, '@');
  // If there's a [@modifier], write a temporary string without it
  unsigned char len = buflen - 1;
  if (at != NULL) {
    len = min(at - c, len);
  }
  return strncat(buffer, c, len);
}

#define CODESET_BUFLEN 40

bool isStatelessEncoding() {
  const char *lc_ctype = getLocaleCode("LC_CTYPE");
  
  char codeset[CODESET_BUFLEN] = "\0";
  getCodeSet(lc_ctype, codeset, CODESET_BUFLEN);
  sanitizeCodeSetString(codeset);
  
  // Return true if codeset is in list of known stateless encodings:
  for (unsigned char i = 0; i < N_STATELESS_ENCS; ++i) {
    if (!strcmp(codeset, stateless_encs[i])) {
      return true;
    }
  }
  return false;
}