/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2020  The R Core Team
 * 
 *  Modified on 09 May 2021 by Architect95
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


#include "substr_utils.h"

#include "R_ext/Riconv.h"


static CharLenCE substrSingleElt(SEXP string, int i_start, int i_stop, 
                                 bool skip_validity_check) {

  // The speed of this function could be improved by running max(i_start, 1) 
  // outside of the function. However, although a reasonable return value could 
  // be returned for the case i_start == NA_INTEGER (e.g. returning an empty
  // string), it would not match the value returned by substr() in that case.

  // ints are used for string lengths, including in substr(). See
  // https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Serialization-Formats

  if (string == NA_STRING || i_start == NA_INTEGER || i_stop == NA_INTEGER) {
    return NA_STRING_CLC;
  }
  
  // Since NA_INTEGER := INT_MIN and INT_MIN < -INT_MAX, the next line could be
  // done outside this function, vectorised for faster speed. However, if the
  // value of NA_INTEGER is ever changed, this would break.
  i_start = unguardedMax(i_start, 1);
  // max(i_stop, 0) is not necessary because of the i_start > i_stop check below

  const int nbytes = LENGTH(string);

  if (i_start > nbytes || i_start > i_stop) {
    return EMPTY_STRING_CLC;
  }

  const char    *str      = CHAR(string);
  const cetype_t encoding = getCharCE(string);

  if (encoding == CE_UTF8) {
    
    return substrUtf8(str, nbytes, encoding, i_start, i_stop, 
                      skip_validity_check);
                                          
  } else if (   encoding == CE_LATIN1
             || encoding == CE_BYTES
             || !mbcslocale) {  // and (encoding != CE_UTF8) is already known.

    return substrSingleByteChars(str, nbytes, encoding, i_start, i_stop);
    
  } else {  // Other encodings:
    
    return substrMultiByteChars(str, nbytes, encoding, i_start, i_stop);
  }
}



SEXP fsubstrR(SEXP x, SEXP start, SEXP stop) {

  if (!isString(x)) {
    if (xlength(x) == 0) {
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

  is_stateless_enc = !mbcslocale || isStatelessEncoding();

  SEXP output = PROTECT(allocVector(STRSXP, n));

  if (   (start_len == 1 && asInteger(start) == NA_INTEGER)
      || (stop_len  == 1 && asInteger(stop)  == NA_INTEGER)) {
    // start or stop is a singleton NA, so all the strings are NA:
    for (R_xlen_t i=0; i < n; ++i) {
      SET_STRING_ELT(output, i, NA_STRING);
    }
    
  } else if (start_len == 1 && stop_len == 1) {
    
    int i_start = asInteger(start);
    int i_stop = asInteger(stop);

    FSUBSTR_LOOP(substrSingleElt, i_start, i_stop)

  } else if (start_len == 1) {
    
    int i_start = asInteger(start);
    warnIfVectorArgRecycled("stop",  stop_len,  n);

    FSUBSTR_LOOP(substrSingleElt, i_start, INTEGER(stop)[i % stop_len])

  } else if (stop_len == 1) {
    
    int i_stop = asInteger(stop);
    warnIfVectorArgRecycled("start", start_len, n);
    
    FSUBSTR_LOOP(substrSingleElt, INTEGER(start)[i % start_len], i_stop)

  } else {
    warnIfVectorArgRecycled("start", start_len, n);
    warnIfVectorArgRecycled("stop",  stop_len,  n);

    FSUBSTR_LOOP(substrSingleElt, INTEGER(start)[i % start_len], 
                                  INTEGER(stop)[i % stop_len])
  }

  SHALLOW_DUPLICATE_ATTRIB(output, x);  // This copies the class, if any.
  UNPROTECT(1 + n_extra_protections);

  return output;
}