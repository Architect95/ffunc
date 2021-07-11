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


#include "char_utils.h"

#include "R_ext/Riconv.h"


LibImport Rboolean mbcslocale;  // From line 30 of text.c
#define ASCII_MASK (64)
#define IS_ASCII(x) (LEVELS(x) & ASCII_MASK)
//#define isValidUtf8(str, len) (valid_utf8(str, len) == 0)

#define FAST_ARRAY_MEM_LIMIT 10000000  // 10m.
// This length is chosen heuristically to avoid slow memory usage patterns.


static void setSubstrSingleElt(SEXP string, int i_start, int i_stop, 
                              SEXP new_substring, 
                              bool skip_validity_check,
                              bool skip_validity_check_new_sub,
                              SEXP output,
                              int i,
                              void *stack_ptr) {

  // The speed of this function could be improved by running max(i_start, 1) 
  // outside of the function. However, although a reasonable return value could 
  // be returned for the case i_start == NA_INTEGER (e.g. returning an empty
  // string), it would not match the value returned by substr() in that case.

  // ints are used for string lengths, including in substr(). See
  // https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Serialization-Formats

  if (   string == NA_STRING || i_start == NA_INTEGER || i_stop == NA_INTEGER
      || new_substring == NA_STRING) {
    SET_STRING_ELT(output, i, NA_STRING);
    return;
  }
  // Since NA_INTEGER := INT_MIN and INT_MIN < -INT_MAX, the next line could be
  // done outside this function and thus vectorised for speed (because negative
  // starts get floored at zero, so we'd just need to exclude the case where
  // i_start == INT_MIN + 1). However, if the value of NA_INTEGER is ever 
  // changed, this would break.
  --i_start;
  
  i_start = (i_start  & -(0 < i_start));  // max(i_start, 0)
  // max(i_stop, 0) is not necessary because of the i_start > i_stop check below

  const char *str      = CHAR(string);
  size_t      nbytes   = LENGTH(string); // size_t because it might be reset to
                                        // the string length in native encoding
  cetype_t    encoding = getCharCE(string);

  if (i_start >= nbytes || i_start >= i_stop) {
    // Start is after the end of the string or substring is empty so return the 
    // original string:
    SET_STRING_ELT(output, i, mkCharLenCE(str, nbytes, encoding));
    return;
  }

  const char *new_substr = CHAR(new_substring);
  size_t new_nbytes         = LENGTH(new_substring);
  
  // If the two strings have different encodings, translate them both into the
  // CE_NATIVE encoding:
  if (getCharCE(new_substring) != encoding && !IS_ASCII(new_substring)) {
    str        = translateChar(string);  // This requires stack mgmt - see 
                                         // sysutils.c line 980
    nbytes     = strlen(str);
    encoding   = CE_NATIVE;
    new_substr = translateChar(new_substring);
    new_nbytes = strlen(new_substr);
    vmaxset(stack_ptr);  // Stack mgmt as required for translateChar
  }
  
  char *buffer = allocStringBuffer(2*(nbytes + new_nbytes)+1, &strbuff);

  if (encoding == CE_UTF8) {
    if (!skip_validity_check && !isValidUtf8(str, nbytes)) {
      error("Elt is invalid UTF8-encoded string \'%s\'", str);
    }
    if (!skip_validity_check_new_sub && !isValidUtf8(new_substr, new_nbytes)) {
      error("Replacement elt is invalid UTF8-encoded string \'%s\'", new_substr);
    }

    const char *end = str + nbytes;
    const char *const str_start = str;
    int j;
    for (j = 0; j < i_start && str < end; ++j) {
      // Comparison to end is needed because there's no check whether a given
      // char is the null terminator.
      str += utf8clen(*str);
    }
    const int start_len = str - str_start;
    memcpy(buffer, str_start, start_len);
    
    const char *const substr_start = new_substr;
    const char *const substr_end   = new_substr + new_nbytes;
    for (; j < i_stop && str < end && new_substr < substr_end; ++j) {
      str        += utf8clen(*str);
      new_substr += utf8clen(*new_substr);
    }
    memcpy(buffer + start_len, substr_start, new_substr - substr_start);
    memcpy(buffer + start_len + (int)(new_substr - substr_start), str, 
           nbytes - (int)(str - str_start));
    
    SET_STRING_ELT(output, i, 
                   mkCharLenCE(buffer, 
                               nbytes - (int)(str - str_start) + start_len
                                      + (int)(new_substr - substr_start), 
                               encoding));
    return;
      
  } else if (   IS_ASCII(string)
             || encoding == CE_LATIN1
             || encoding == CE_BYTES
             || !mbcslocale) {  // and (encoding != CE_UTF8) is already known.

    i_stop             = unguardedMin(i_stop, nbytes);
    int nbytes_to_copy = i_stop - i_start;
    nbytes_to_copy     = unguardedMin(new_nbytes, nbytes_to_copy);

    memcpy(buffer, str, nbytes);
    memcpy(buffer + i_start, new_substr, nbytes_to_copy);

    SET_STRING_ELT(output, i, mkCharLenCE(buffer, nbytes, encoding));
    return;
    
  } else {
  
    // Create and initialise a shift state for stateful multi-byte encodings:
    mbstate_t mb_st;
    memset(&mb_st, 0, sizeof(mbstate_t));

    const char *end = str + nbytes;
    int j;
    wchar_t *out = (wchar_t *)buffer;
    // Copy start of old string:
    for (j = 0; j < i_start && str < end; ++j, ++out) {
      str += Mbrtowc(out, str, MB_CUR_MAX, &mb_st);
    }

    mbstate_t substring_mb_st;  // Not thread-safe.
    memset(&substring_mb_st, 0, sizeof(mbstate_t));
    
    // Copy substring:
    const char *const substr_end = new_substr + new_nbytes;
    for (; j < i_stop && str < end && new_substr < substr_end; ++j) {
      str        += Mbrtowc(NULL, str, MB_CUR_MAX, &mb_st);
      new_substr += Mbrtowc(out,  str, MB_CUR_MAX, &substring_mb_st);
    }
    // Copy end of old string:
    for (; str < end; ++out) {
      str += Mbrtowc(out, str, MB_CUR_MAX, &mb_st);
    }
    SET_STRING_ELT(output, i, mkCharLenCE(buffer, 
                                          ((char *)out - buffer),
                                          encoding));
    return;
  }
}

SEXP fsubstrassignR(SEXP x, SEXP start, SEXP stop, SEXP replacement) {
  
  const R_xlen_t n = xlength(x);
  
  if (!isString(x)) {
    HANDLE_NON_STRING_INPUT(x, n)
  }
  
  
  // Use substr<- if it appears that all the strings are ASCII, since for ASCII
  // strings, substr<- is faster:
  const int n_tests = unguardedMin(n, 50);
  bool ascii_test = TRUE;
  const int idx = (n - n_tests) >> 1;  // This is floor((n - n_tests) / 2)
  for (int i=0; i < n_tests; ++i) {
    const SEXP elt = STRING_ELT(x, idx + i);
    const cetype_t encoding = getCharCE(elt);
    ascii_test = ascii_test && (   IS_ASCII(elt)
                                || (!mbcslocale && encoding != CE_UTF8)
                                || encoding == CE_LATIN1 
                                || encoding == CE_BYTES);
  }
  if (ascii_test) {
    SEXP substr_call = 
      PROTECT(lang5(install("substr<-"), x, start, stop, replacement));
    SEXP output = eval(substr_call, R_BaseEnv);
    UNPROTECT(1);
    SHALLOW_DUPLICATE_ATTRIB(output, x);  // This copies the class, if any.
    return output;
  }
  
  const R_xlen_t value_len = LENGTH(replacement);
  if (!isString(replacement) || value_len == 0) {
    error("Invalid replacement string");
  }

  const R_xlen_t start_len = xlength(start);
  const R_xlen_t stop_len  = xlength(stop);

  if (start_len == 0) {
    error("Start vector has zero length");
  } else if (stop_len == 0) {
    error("Stop vector has zero length");
  }

  // In substr(), coercion to INTSXP of ints stored as other numeric types
  // seems to happen before do_substrgets() is called. In fsubstr(), we perform
  // the coercion ourselves:
  unsigned char n_extra_protections = 0;
  if (!isInteger(start)) {
    start = PROTECT(coerceVector(start, INTSXP));
    ++n_extra_protections;

    // If it's still not a valid integer vector, throw error:
    if (!isInteger(start)) {
      error("Start contains non-integer values");
    }
    // Otherwise start was coerced to an integer vector:
    warning("Start contains non-integers that will be coerced");
  }
  if (!isInteger(stop)) {
    stop = PROTECT(coerceVector(stop, INTSXP));
    ++n_extra_protections;

    // If it's still not a valid integer vector, throw error:
    if (!isInteger(stop)) {
      error("Stop contains non-integer values");
    }
    // Otherwise start was coerced to an integer vector:
    warning("Stop contains non-integers that will be coerced");
  }

  SEXP output = PROTECT(allocVector(STRSXP, n));

  if (   (start_len == 1 && asInteger(start) == NA_INTEGER)
      || (stop_len  == 1 && asInteger(stop)  == NA_INTEGER)) {
    
    for (R_xlen_t i=0; i < n; ++i) {
      SET_STRING_ELT(output, i, NA_STRING);
    }
  } else {
    if ((n > start_len) && ((n % start_len) != 0)) {
      warning("Length of start is not a factor of length of vector; "
              "start will be partially recycled.");
    }
    if ((n > stop_len) && ((n % stop_len) != 0)) {
      warning("Length of stop is not a factor of length of vector; "
              "stop will be partially recycled.");
    }
    if ((n > value_len) && ((n % value_len) != 0)) {
      warning("Length of replacement substring is not a factor of length of "
              "vector; replacement substring will be partially recycled.");
    }
    
    strbuff.data = malloc(strbuff.defaultSize);
    void *const stack_ptr = vmaxget();
    
    SEXP prev_elt = NULL;
    SEXP prev_new_substr = NULL;
    
    for (R_xlen_t i = 0; i < n; ++i) {
      const SEXP restrict element = STRING_ELT(x, i);
      const SEXP restrict new_substr = STRING_ELT(replacement, i % value_len);
      
      setSubstrSingleElt(element, INTEGER(start)[i % start_len],
                         INTEGER(stop)[i % stop_len], new_substr,
                         element == prev_elt,  // Ptr comparison to prev_elt
                         new_substr == prev_new_substr,
                         output, i, stack_ptr);
                         
      prev_elt = element;
      prev_new_substr = new_substr;
    }
    
    freeStringBuffer(&strbuff);
  }
  
  SHALLOW_DUPLICATE_ATTRIB(output, x);  // This copies the class, if any.
  UNPROTECT(1 + n_extra_protections);
  return output;
}