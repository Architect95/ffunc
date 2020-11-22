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

#include <wchar.h>
#include "R_ext/Riconv.h"

#define R_NO_REMAP


unsigned utf8clen(char c);

LibImport Rboolean mbcslocale;  // From line 30 of text.c
#define ASCII_MASK (1<<6)
#define IS_ASCII(x) (LEVELS(x) & ASCII_MASK)
#define isValidUtf8(str, len) (valid_utf8(str, len) == 0)

#define FAST_ARRAY_MEM_LIMIT 10000000  // 10m.
// This length is chosen heuristically to avoid slow memory usage patterns.



static struct CharLenCE substrSingleElt(SEXP string, int i_start, int i_stop, 
                                        bool skip_validity_check) {

  // The speed of this function could be improved by 
  // running max(i_start, 1) outside of the function. However, although the
  // return value for i_start == NA_INTEGER could be made reasonable (e.g. 
  // returning an empty string), it would not match the value returned by
  // substr().

  // ints are used for string lengths, including in substr(). See
  // https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Serialization-Formats

  if (string == NA_STRING || i_start == NA_INTEGER || i_stop == NA_INTEGER) {
    struct CharLenCE substring = {NULL, -1, CE_NATIVE};
    return substring;
  }
  
  // Since NA_INTEGER := INT_MIN and INT_MIN < -INT_MAX, the next line could be
  // done outside this function, vectorised for faster speed. However, if the
  // value of NA_INTEGER is ever changed, this would break.
  i_start = unguardedMax(i_start, 1);
  // max(i_stop, 0) is not necessary because of the i_start > i_stop check below

  const int nbytes = LENGTH(string);

  if (i_start > nbytes || i_start > i_stop) {
    // Start is after the end of the string so return the empty string:
    struct CharLenCE substring = {NULL, 0, CE_NATIVE};
    return substring;
  }

  const char     *str     = CHAR(string);
  const cetype_t encoding = getCharCE(string);

  if (encoding == CE_UTF8) {
    if (!skip_validity_check && !isValidUtf8(str, nbytes)) {
      error("Elt is invalid UTF8-encoded string \'%s\'", str);
    }

    const int just_before_start = i_start - 1;
    const char *const end = str + nbytes;
    // Increment the str pointer past all the characters at the front to be
    // omitted:
    int j;
    for (j = 0; j < just_before_start && str < end; ++j) {
      // The comparison to end is needed because there's no check whether a
      // given char is the null terminator.
      str += utf8clen(*str);
    }
    const char *const start_point = str;
    for (; j < i_stop && str < end; ++j) {
      str += utf8clen(*str);
    }
    struct CharLenCE substring = {start_point, (int)(str - start_point), 
                                  encoding};
    return substring;
                                          
  } else if (   IS_ASCII(string)
             || encoding == CE_LATIN1
             || encoding == CE_BYTES
             || !mbcslocale) {  // and (encoding != CE_UTF8) is already known.
    // Single-byte char set, so we can just count bytes (i.e. use char ptr 
    // arithmetic):
    if (i_stop >= nbytes) {
      struct CharLenCE substring = {str + i_start - 1, nbytes - i_start + 1, 
                                    encoding};
      return substring;
    } else {
      struct CharLenCE substring = {str + i_start - 1, i_stop - i_start + 1, 
                                    encoding};
      return substring;
    }
  } else {  // Other encodings:
    // Create and initialise a multi-byte shift state:
    mbstate_t mb_st;
    memset(&mb_st, 0, sizeof(mbstate_t));

    const int just_before_start = i_start - 1;
    const char *const end = str + nbytes;
    int j;
    for (j = 0; j < just_before_start && str < end; ++j) {
      str += Mbrtowc(NULL, str, MB_CUR_MAX, &mb_st);
    }
    const char *const start_point = str;
    if (i_stop >= nbytes) {
      struct CharLenCE substring = {start_point, nbytes, encoding};
      return substring;
    } else {
      for (; j < i_stop && str < end; ++j) {
        str += Mbrtowc(NULL, str, MB_CUR_MAX, &mb_st);
      }
      struct CharLenCE substring = {start_point, (int)(str - start_point), 
                                    encoding};
      return substring;
    }

  }
}

inline void fillStringVector(SEXP output, R_xlen_t loop_start, 
                                    R_xlen_t loop_end, 
                                    struct CharLenCE *const substrings) {
  // Most of the runtime of fsubstr() is spent in this function.
  
  // Creating the CHARSXPs cannot be done in parallel because R's string
  // hash table is not thread-safe. SET_STRING_ELT is not thread-safe
  // either because the ref counts of the strings in the hash table are
  // updated in it, and if the same string appears in two threads then a count
  // could be modified separately in two threads at once.
  
  for (R_xlen_t i = loop_start; i < loop_end; ++i) {
    const R_xlen_t k = i - loop_start;
    if (substrings[k].len == -1) {
      SET_STRING_ELT(output, i, NA_STRING);
    } else {
      // There are redundant checks in mkCharLenCE(), but no way to skip them
      // whilst still using that function.
      SET_STRING_ELT(output, i, mkCharLenCE(substrings[k].str_ptr, 
                                            substrings[k].len, 
                                            substrings[k].enc));
    }
  }
}

SEXP fsubstrR(SEXP x, SEXP start, SEXP stop) {

  if (!isString(x)) {
    if (xlength(x)==0) {
      // Return character(0):
      SEXP output = PROTECT(allocVector(STRSXP, 0));
      SHALLOW_DUPLICATE_ATTRIB(output, x);
      UNPROTECT(1);
      return output;
    }
    if (LOGICAL(x)[0] == NA_LOGICAL) {
      // Return a character vector containing a single NA_STRING:
      SEXP output = PROTECT(allocVector(STRSXP, 1));
      SET_STRING_ELT(output, 0, NA_STRING);
      SHALLOW_DUPLICATE_ATTRIB(output, x);
      UNPROTECT(1);
      return output;
    }
    error("The vector of strings supplied is not a character object");
  }


  const R_xlen_t n = xlength(x);

  // When the string vector is small enough, call substr() as it will
  // probably be faster:
  const int small_len = 3000;  // Value of small_len was chosen heuristically.
  if (n < small_len || OMP_MAX_THREADS == 1) {
    SEXP substr_call = PROTECT(lang4(install("substr"), x, start, stop));
    SEXP output = eval(substr_call, R_BaseEnv);
    SHALLOW_DUPLICATE_ATTRIB(output, x);
    UNPROTECT(1);
    return output;
  }

  R_xlen_t start_len = xlength(start);
  R_xlen_t stop_len  = xlength(stop);

  if (start_len == 0) {
    error("Start vector has zero length");
  } else if (stop_len == 0) {
    error("Stop vector has zero length");
  }

  // In substr(), it seems that coercion to INTSXP of ints stored as other 
  // numeric types happens before do_substr() is called. In fsubstr(), we 
  // perform the coercion ourselves:
  unsigned char n_extra_protections = 0;
  if (!isInteger(start)) {
    start = PROTECT(coerceVector(start, INTSXP));
    ++n_extra_protections;

    // If it's still not a valid integer vector, throw error:
    if (!isInteger(start)) {
      UNPROTECT(n_extra_protections);
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
      UNPROTECT(n_extra_protections);
      error("Stop contains non-integer values");
    }
    // Otherwise start was coerced to an integer vector:
    warning("Stop contains non-integers that will be coerced");
  }

  SEXP output = PROTECT(allocVector(STRSXP, n));

  if (   (start_len == 1 && asInteger(start) == NA_INTEGER)
      || (stop_len  == 1 && asInteger(stop)  == NA_INTEGER)) {
    // start or stop is a singleton NA, so all the strings are NA:
    for (R_xlen_t i=0; i < n; ++i) {
      SET_STRING_ELT(output, i, NA_STRING);
    }
  } 
  else if (start_len == 1 && stop_len == 1) {
    int i_start = asInteger(start);
    int i_stop = asInteger(stop);

    const R_xlen_t batch_size = FAST_ARRAY_MEM_LIMIT;
    const R_xlen_t n_batches = 1 + ((n - 1) / batch_size); //Already know n >= 1

    const R_xlen_t array_size = ((n_batches > 1) ? batch_size : n);
    struct CharLenCE * restrict substrings = 
      (struct CharLenCE *)malloc(array_size * sizeof(struct CharLenCE));
    for (R_xlen_t j = 0; j < n_batches; ++j) {
      
      R_CheckUserInterrupt();
      
      const R_xlen_t loop_start = j * batch_size;
      const R_xlen_t loop_end = (j == n_batches-1) ? (n) : ((j+1)*batch_size);

      SEXP prev_elt = NULL;
      #pragma omp parallel for firstprivate(prev_elt)
      for (R_xlen_t i = loop_start; i < loop_end; ++i) {
        const SEXP restrict element = STRING_ELT(x,i);
        if (element == prev_elt) {
          substrings[i - loop_start] = substrings[i - 1 - loop_start];
        } else {
          substrings[i - loop_start] = 
            substrSingleElt(element, i_start, i_stop, FALSE);
        }
        prev_elt = element;
      }
      R_CheckUserInterrupt();
      fillStringVector(output, loop_start, loop_end, substrings);
    }
    free(substrings);

  } else if (start_len == 1) {
    int i_start = asInteger(start);
    if ((n > stop_len) && ((n % stop_len) != 0)) {
      warning("Length of stop is not a factor of length of vector; "
              "stop will be partially recycled.");
    }

    const R_xlen_t batch_size = FAST_ARRAY_MEM_LIMIT;
    const R_xlen_t n_batches = 1 + ((n - 1) / batch_size); //Already know n >= 1

    const R_xlen_t array_size = ((n_batches > 1) ? batch_size : n);
    struct CharLenCE * restrict substrings = 
      (struct CharLenCE *)malloc(array_size * sizeof(struct CharLenCE));
    for (R_xlen_t j = 0; j < n_batches; ++j) {
      
      R_CheckUserInterrupt();
      
      const R_xlen_t loop_start = j * batch_size;
      const R_xlen_t loop_end = (j == n_batches-1) ? (n) : ((j+1)*batch_size);

      SEXP prev_elt = NULL;
      #pragma omp parallel for firstprivate(prev_elt)
      for (R_xlen_t i = loop_start; i < loop_end; ++i) {
        const SEXP restrict element = STRING_ELT(x,i);
        substrings[i - loop_start] = 
          substrSingleElt(element, i_start, INTEGER(stop)[i % stop_len],
                          element == prev_elt);  //Ptr comparison to prev_elt
        prev_elt = element;
      }
      R_CheckUserInterrupt();
      fillStringVector(output, loop_start, loop_end, substrings);
    }
    free(substrings);

  } else if (stop_len == 1) {
    int i_stop = asInteger(stop);
    if ((n > start_len) && ((n % start_len) != 0)) {
      warning("Length of start is not a factor of length of vector; "
              "start will be partially recycled.");
    }

    const R_xlen_t batch_size = FAST_ARRAY_MEM_LIMIT;
    const R_xlen_t n_batches = 1 + ((n - 1) / batch_size); //Already know n >= 1

    const R_xlen_t array_size = ((n_batches > 1) ? batch_size : n);
    struct CharLenCE * restrict substrings = 
      (struct CharLenCE *)malloc(array_size * sizeof(struct CharLenCE));
    for (R_xlen_t j = 0; j < n_batches; ++j) {
      
      R_CheckUserInterrupt();
      
      const R_xlen_t loop_start = j * batch_size;
      const R_xlen_t loop_end = (j == n_batches-1) ? (n) : ((j+1)*batch_size);

      SEXP prev_elt = NULL;
      #pragma omp parallel for firstprivate(prev_elt)
      for (R_xlen_t i = loop_start; i < loop_end; ++i) {
        const SEXP restrict element = STRING_ELT(x,i);
        substrings[i - loop_start] = 
          substrSingleElt(element, INTEGER(start)[i % start_len], i_stop,
                          element == prev_elt);  // Ptr comparison to prev_elt
        prev_elt = element;
      }
      R_CheckUserInterrupt();
      fillStringVector(output, loop_start, loop_end, substrings);      
    }
    free(substrings);

  } else {
    if ((n > start_len) && ((n % start_len) != 0)) {
      warning("Length of start is not a factor of length of vector; "
              "start will be partially recycled.");
    }
    if ((n > stop_len) && ((n % stop_len) != 0)) {
      warning("Length of stop is not a factor of length of vector; "
              "stop will be partially recycled.");
    }

    const R_xlen_t batch_size = FAST_ARRAY_MEM_LIMIT;
    const R_xlen_t n_batches = 1 + ((n - 1) / batch_size); //Already know n >= 1

    const R_xlen_t array_size = ((n_batches > 1) ? batch_size : n);
    struct CharLenCE * restrict substrings = 
      (struct CharLenCE *)malloc(array_size * sizeof(struct CharLenCE));
    for (R_xlen_t j = 0; j < n_batches; ++j) {
      R_CheckUserInterrupt();
      
      const R_xlen_t loop_start = j * batch_size;
      const R_xlen_t loop_end = (j == n_batches-1) ? (n) : ((j+1)*batch_size);

      SEXP prev_elt = NULL;
      #pragma omp parallel for firstprivate(prev_elt)
      for (R_xlen_t i = loop_start; i < loop_end; ++i) {
        const SEXP restrict element = STRING_ELT(x,i);
        substrings[i - loop_start] = 
          substrSingleElt(element, INTEGER(start)[i % start_len], 
                                   INTEGER(stop)[i % stop_len],
                          element == prev_elt);  // Ptr comparison to prev_elt
        prev_elt = element;
      }
      R_CheckUserInterrupt();
      fillStringVector(output, loop_start, loop_end, substrings);
    }
    free(substrings);
  }

  SHALLOW_DUPLICATE_ATTRIB(output, x);  // This copies the class, if any.
  UNPROTECT(1 + n_extra_protections);

  return(output);
}