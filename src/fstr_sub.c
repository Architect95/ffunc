/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2020  The R Core Team
 * 
 *  Modified on 11 July 2021 by Architect95
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



static FORCE_INLINE CharLenCE signedSubstrUtf8(
  const char *str, const int nbytes, const cetype_t encoding,
  const int i_start, const int i_stop, const bool skip_validity_check);
static FORCE_INLINE CharLenCE signedSubstrMultiByteChars(
  const char *str, const int nbytes, const cetype_t encoding,
  int i_start, int i_stop);


static CharLenCE signedSubstrSingleElt(SEXP string, int i_start, int i_stop, 
                                       bool skip_validity_check) {
  // The speed of this function could be improved by 
  // running max(i_start, 1) outside of the function. However, although the
  // return value for i_start == NA_INTEGER could be made reasonable (e.g. 
  // returning an empty string), it would not match the value returned by
  // substr().

  // ints are used for string lengths, including in substr(). See
  // https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Serialization-Formats

  if (string == NA_STRING || i_start == NA_INTEGER || i_stop == NA_INTEGER) {
    return NA_STRING_CLC;
  }
  
  if (i_start == 0) { i_start = 1; }
  
  const int nbytes = LENGTH(string);
  if (   i_start > nbytes                             // Start beyond str front
      || -i_stop > nbytes                             // Stop beyond str end
      || (   !oppositeSign(i_start, i_stop)
          && i_start > i_stop)) {                     // Start beyond stop
    
    // oppositeSign(i_start, i_stop) &&
    // (i_start > i_stop + (nchars + 1) * sign(i_start)) is another case, but
    // can only be detected now if nchars is known, i.e. if encoding is 
    // fixed-width. For single-byte chars, this is dealt with below as 
    // (i_start > i_stop) once i_start and i_stop have been recomputed as
    // positive ints.
    
    return EMPTY_STRING_CLC;
  }
  // When stop is -1, reset it to a positive number greater than or equal to 
  // the string length:
  if (i_stop == -1) { i_stop = nbytes; }

  const char    *str      = CHAR(string);
  const cetype_t encoding = getCharCE(string);

  if (encoding == CE_UTF8) {
    return signedSubstrUtf8(str, nbytes, encoding, i_start, i_stop, 
                              skip_validity_check);
  
  } else if (   encoding == CE_LATIN1
             || encoding == CE_BYTES
             || !mbcslocale) {  // and (encoding != CE_UTF8) is already known.

    if (i_start < 0) { i_start = unguardedMax(nbytes + i_start, 0) + 1; }
    if (i_stop  < 0) { i_stop  = (nbytes + i_stop) + 1; }
    
    if (i_start > i_stop) { return EMPTY_STRING_CLC; }

    return substrSingleByteChars(str, nbytes, encoding, i_start, i_stop);
    
  } else {
    
    return signedSubstrMultiByteChars(str, nbytes, encoding, i_start, i_stop);
  }
}

SEXP fstr_subR(SEXP x, SEXP start, SEXP stop) {

  const R_xlen_t n = xlength(x);
  
  const R_xlen_t start_len = xlength(start);
  const R_xlen_t stop_len  = xlength(stop);
  
  // If either the start or stop vector is longer than the input vector of
  // strings, the strings will be partially repeated. Therefore set n to the 
  // length of the longest of the three vectors:
  
  const R_xlen_t start_stop_len = max_xlen(start_len, stop_len);
  const R_xlen_t out_len = ((n > 0) ? max_xlen(n, start_stop_len) : 0);
  
  // The check for case n == 0 ensures that zero-length inputs fall through to 
  // FSUBSTR_LOOP with out_len == 0, so that no loop iterations are run, and
  // thus a zero-length STRSXP vector is returned.


  if (!isString(x)) {
    HANDLE_NON_STRING_INPUT(x, out_len)
  }

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



  SEXP output = PROTECT(allocVector(STRSXP, out_len));

  if (   (start_len == 1 && asInteger(start) == NA_INTEGER)
      || (stop_len  == 1 && asInteger(stop)  == NA_INTEGER)) {
    // start or stop is a singleton NA, so all the strings are NA:
    for (R_xlen_t i=0; i < out_len; ++i) {
      SET_STRING_ELT(output, i, NA_STRING);
    }
    
  } else if (start_len == 1 && stop_len == 1) {
    
    int i_start = asInteger(start);
    int i_stop = asInteger(stop);

    FSUBSTR_LOOP(signedSubstrSingleElt, STRING_ELT(x,i), out_len, i_start, 
                 i_stop)

  } else {
    
    if (out_len == n) {
      
      if (start_len == 1) {
        
        int i_start = asInteger(start);
        warnIfVectorArgRecycled("stop",  stop_len,  n);

        FSUBSTR_LOOP(signedSubstrSingleElt, STRING_ELT(x,i), out_len,
                    i_start, INTEGER(stop)[i % stop_len])

      } else if (stop_len == 1) {
        
        int i_stop = asInteger(stop);
        warnIfVectorArgRecycled("start", start_len, n);
        
        FSUBSTR_LOOP(signedSubstrSingleElt, STRING_ELT(x,i), out_len,
                    INTEGER(start)[i % start_len], i_stop)

      } else {
        warnIfVectorArgRecycled("start", start_len, n);
        warnIfVectorArgRecycled("stop",  stop_len,  n);

        FSUBSTR_LOOP(signedSubstrSingleElt, STRING_ELT(x,i), out_len,
                    INTEGER(start)[i % start_len], INTEGER(stop)[i % stop_len])
      }
    } else {  // out_len > n, so recycle input vector of strings
      
      if (start_len == 1) {
        
        int i_start = asInteger(start);
        warnIfVectorArgRecycled("stop",  stop_len,  out_len);

        FSUBSTR_LOOP(signedSubstrSingleElt, STRING_ELT(x, i % n), out_len,
                    i_start, INTEGER(stop)[i % stop_len])

      } else if (stop_len == 1) {
        
        int i_stop = asInteger(stop);
        warnIfVectorArgRecycled("start", start_len, out_len);
        
        FSUBSTR_LOOP(signedSubstrSingleElt, STRING_ELT(x, i % n), out_len,
                    INTEGER(start)[i % start_len], i_stop)

      } else {
        warnIfVectorArgRecycled("start", start_len, out_len);
        warnIfVectorArgRecycled("stop",  stop_len,  out_len);

        FSUBSTR_LOOP(signedSubstrSingleElt, STRING_ELT(x, i % n), out_len,
                    INTEGER(start)[i % start_len], INTEGER(stop)[i % stop_len])
      }
    }
  }

  SHALLOW_DUPLICATE_ATTRIB(output, x);  // This copies the class, if any.
  UNPROTECT(1 + n_extra_protections);

  return output;
}

// lookback helper functions:

static FORCE_INLINE int lookbackIndex(const int lookback_len, 
                                      const int latest_entry,
                                      const int from_end) {
  // Returns the lookback array index of the pointer to character at position
  // from_end in the string
  
  // Returns   (-from_end > latest_entry)
  //         ? lookback_len - ((-latest_entry - (from_end+1)) % lookback_len)
  //         : (latest_entry + (from_end+1));
  
  assert(from_end < 0);
  const int array_last = lookback_len - 1;
  return array_last -((array_last -latest_entry -from_end -1) % lookback_len);
}

static FORCE_INLINE int getSubNbytesFromLookback(
  const char *restrict start_of_sub, const char *restrict end,
  const int lookback_length, const int latest_entry, const int from_end, 
  const char **last_m_chars_ptrs) {
  
  // The caller must ensure that from_end (which is a negative offset) is not
  // beyond the start of the string, i.e. -from_end < nchar. Otherwise, this
  // function will return the wrong entry in the lookback.
  
  const char *end_of_sub;
  
  if (from_end == -1) {
    end_of_sub      = end;
  } else {
    const int index = lookbackIndex(lookback_length, latest_entry, from_end +1);
    end_of_sub      = last_m_chars_ptrs[index];
  }
  return end_of_sub - start_of_sub;
}

// UTF-8 string functions:

static FORCE_INLINE StrLookbackIndex
u8CountWithLookback(int count, const int i_stop, 
  const char *str, const char *restrict const end, 
  int lookback_length, const char **const restrict last_m_chars_ptrs) {
  
  int i = -1;
  for (; count < i_stop  && str < end; ++count) {
    ++i;
    i = i % lookback_length;
    last_m_chars_ptrs[i] = str;         // Ptr to start of (i+1)th character
    str += utf8clen(*str);
  }
  
  return (const StrLookbackIndex) {count, i, str};
}

static FORCE_INLINE StrIndex
u8CountToEnd(int count,
             const char *str, const char *restrict const end) {
  
  for (; str < end; ++count) {
    str += utf8clen(*str);
  }
  
  return (const StrIndex) {count, str};
}

static FORCE_INLINE StrLookbackIndex
u8CountToEndWithLookback(int count,
  const char *str, const char *restrict const end, 
  const int lookback_length, const char **last_m_chars_ptrs) {
  
  // This fn updates the values at last_m_chars_ptrs
  
  int i = -1;
  for (; str < end; ++count) {
    ++i;
    i = i % lookback_length;
    last_m_chars_ptrs[i] = str;  // Ptr to start of (i+1)th character
    str += utf8clen(*str);
  }
  return (const StrLookbackIndex) {count, i, str};
}


// Multibyte string functions:

static FORCE_INLINE StrLookbackIndex
mbCountWithLookback(int count, const int i_stop, 
  const char *str, const char *restrict const end, 
  mbstate_t *const restrict mb_st_ptr,
  int lookback_length, const char **const restrict last_m_chars_ptrs,
  mbstate_t *const restrict last_m_mbstates) {
  
  int i = -1;
  for (; count < i_stop  && str < end; ++count) {
    ++i;
    i = i % lookback_length;
    last_m_chars_ptrs[i] = str;         // Ptr to start of (i+1)th character
    last_m_mbstates[i]   = *mb_st_ptr;  // State after ith character
    str += Mbrtowc(NULL, str, MB_CUR_MAX, mb_st_ptr);
  }
  
  return (const StrLookbackIndex) {count, i, str};
}

static FORCE_INLINE StrIndex
mbCountToEnd(int count, 
             const char *str, const char *restrict const end, 
             mbstate_t *const restrict mb_st_ptr) {
  
  for (; str < end; ++count) {
    str += Mbrtowc(NULL, str, MB_CUR_MAX, mb_st_ptr);
  }

  return (const StrIndex) {count, str};
}

static FORCE_INLINE StrLookbackIndex
mbCountToEndWithLookback(int count,
  const char *str, const char *restrict const end, 
  mbstate_t *const restrict mb_st_ptr, 
  const int lookback_length, const char **last_m_chars_ptrs) {
  
  // This fn updates the value at mb_st_ptr and the values at last_m_chars_ptrs
  
  int i = -1;
  for (; str < end; ++count) {
    ++i;
    i = i % lookback_length;
    last_m_chars_ptrs[i] = str;  // Ptr to start of (i+1)th character
    str += Mbrtowc(NULL, str, MB_CUR_MAX, mb_st_ptr);
  }
  return (const StrLookbackIndex) {count, i, str};
}

static FORCE_INLINE StrLookbackIndex
mbCountToEndWithStatefulLookback(int count,
  const char *str, const char *restrict const end,
  mbstate_t *const restrict mb_st_ptr, 
  const int lookback_length, const char **last_m_chars_ptrs,
  mbstate_t *const restrict last_m_mbstates) {
  
  // This fn updates *mb_st_ptr and *last_m_chars_ptrs
  
  int i = -1;
  for (; str < end; ++count) {
    ++i;
    i = i % lookback_length;
    last_m_chars_ptrs[i] = str;         // Ptr to start of (i+1)th character
    last_m_mbstates[i]   = *mb_st_ptr;  // State after ith character
    str += Mbrtowc(NULL, str, MB_CUR_MAX, mb_st_ptr);
  }
  return (const StrLookbackIndex) {count, i, str};
}

static FORCE_INLINE StrIndex 
mbRecountFromZeroToStart(const int i_start,
  const char *restrict const start_of_str, const char *restrict const end,
  mbstate_t *const restrict mb_st_ptr) {
  
  memset(mb_st_ptr, '\0', sizeof(mbstate_t));
  const char *str             = start_of_str;
  const int just_before_start = i_start - 1;
  
  return mbRecount(0, just_before_start, str, end, mb_st_ptr);
}

static FORCE_INLINE CharLenCE
substrWithinShiftStateUnguarded(const char *start_of_sub, const cetype_t encoding, 
  mbstate_t mb_st, const char *const end, int nchars, const int i_stop) {
  // Cannot directly copy the bytes of the substring in the original
  // multibyte substring, because the state changing control characters
  // for the current multibyte state occur before start_point in the
  // original string.
  
  if (start_of_sub == end) { return EMPTY_STRING_CLC; }
  
  WCharFromMB first_substr_char = stateShiftAndFirstChar(start_of_sub, &mb_st);
  ++nchars;

  return substrAfterShiftStateIsSetUnguarded(first_substr_char.ptr_after,
    encoding, mb_st, end, nchars, i_stop, first_substr_char.wchar);
}

#define NEGATIVE_STOP_SUBSTR_IN_CUR_STATE(start_of_sub, out_nbytes)           \
                                                                              \
  const mbstate_t start_point_mb_st = mb_st;                                  \
  StrIndex end_of_str = mbCountToEnd(count, start_of_sub, end, &mb_st);       \
  int nchars = end_of_str.index;                                              \
                                                                              \
  const int i_stop_abs = (nchars + i_stop) + 1;                               \
  /*The brackets prevent overflow*/                                           \
                                                                              \
  if (i_stop_abs < i_start) { return EMPTY_STRING_CLC; }                      \
                                                                              \
  /*Count from start again:*/                                                 \
  mb_st = start_point_mb_st;                                                  \
  const char *end_of_sub = mbRecount(count, i_stop_abs, start_of_sub, end,    \
                                     &mb_st).ptr;                             \
                                                                              \
  const int out_nbytes = end_of_sub - start_of_sub;                           \

static FORCE_INLINE CharLenCE
signedSubstrAfterShiftStateIsSet(const char *str, const cetype_t encoding, 
  mbstate_t mb_st, const char *const end, const int count, const int i_start, 
  int i_stop, const wchar_t first_substr_char) {
  
  const char *const rest_of_sub = str;
  
  NEGATIVE_STOP_SUBSTR_IN_CUR_STATE(rest_of_sub, rest_nbytes)
  
  // Original string length was <= INT_MAX, so the substring length wiil be too,
  // so the substring length can be safely stored in an int.
  const int new_nbytes = 
   writeShiftStateSubstrToBuffer(first_substr_char, rest_of_sub, 
                                 rest_nbytes);
  
  return (const CharLenCE) {strbuff.data, new_nbytes, encoding};
}

#define SIGNED_SUBSTR_IN_CUR_STATE_WITH_LOOKBACK(start_of_sub, out_nbytes)    \
                                                                              \
  const char *last_m_chars_ptrs[lookback_length];                              \
                                                                              \
  StrLookbackIndex end_of_str =                                               \
    mbCountToEndWithLookback(count, start_of_sub, end, &mb_st, lookback_length,\
                             last_m_chars_ptrs);                              \
  const int nchars = end_of_str.index;                                        \
  const int i      = end_of_str.lookback_index;                               \
                                                                              \
  if ((nchars + i_stop) + 1 < i_start) { return EMPTY_STRING_CLC; }           \
                                                                              \
  const int out_nbytes = getSubNbytesFromLookback(start_of_sub, end,          \
    lookback_length, i, i_stop, last_m_chars_ptrs);                            \

static FORCE_INLINE CharLenCE
signedSubstrAfterShiftStateIsSetWithLookback(const char *rest_of_sub, 
  const cetype_t encoding, mbstate_t mb_st,
  const char *const end, int count, const int i_start, const int i_stop, 
  const int lookback_length, const wchar_t first_substr_char) {
  // Count the rest of the characters, storing ptrs to the last few characters.
  
  SIGNED_SUBSTR_IN_CUR_STATE_WITH_LOOKBACK(rest_of_sub, rest_nbytes)
  
  const int new_nbytes = 
   writeShiftStateSubstrToBuffer(first_substr_char, rest_of_sub, rest_nbytes);
  
  return (const CharLenCE) {strbuff.data, new_nbytes, encoding};
}

static FORCE_INLINE CharLenCE
signedSubstrWithinShiftState(const char *str, 
  const cetype_t encoding, mbstate_t mb_st, 
  const char *const end, int nchars, const int i_start, const int i_stop) {
  // Cannot directly copy the bytes of the substring in the original
  // multibyte substring, because the state changing control characters
  // for the current multibyte state occur before start_point in the
  // original string.
  
  if (str == end) { return EMPTY_STRING_CLC; }
  
  WCharFromMB first_substr_char = stateShiftAndFirstChar(str, &mb_st);
  ++nchars;
  
  return signedSubstrAfterShiftStateIsSet(first_substr_char.ptr_after, encoding, 
    mb_st, end, nchars, i_start, i_stop, first_substr_char.wchar);
}

static FORCE_INLINE CharLenCE
signedSubstrWithinShiftStateWithLookback(const char *str, 
  const cetype_t encoding, mbstate_t mb_st,
  const char *const end, int nchars, const int i_start, const int i_stop,
  const int lookback_length) {
  // Cannot directly copy the bytes of the substring in the original
  // multibyte substring, because the state changing control characters
  // for the current multibyte state occur before start_point in the
  // original string.
  
  if (str == end) { return EMPTY_STRING_CLC; }
  
  WCharFromMB first_substr_char = stateShiftAndFirstChar(str, &mb_st);
  ++nchars;
  
  return signedSubstrAfterShiftStateIsSetWithLookback(str, encoding, mb_st,
            end, nchars, i_start, i_stop, lookback_length, 
            first_substr_char.wchar);
}

static FORCE_INLINE CharLenCE
substrShiftStateKnownBytes(const char *start_of_sub, const char *end_of_sub,
  const cetype_t encoding, mbstate_t start_mb_st) {
  
  if (!mbsinit(&start_mb_st)) {
    
    if (start_of_sub == end_of_sub) { return EMPTY_STRING_CLC; }
    
    WCharFromMB first_substr_char = 
      stateShiftAndFirstChar(start_of_sub, &start_mb_st);
    
    const int rest_nbytes = end_of_sub - first_substr_char.ptr_after;
    const int new_nbytes = 
      writeShiftStateSubstrToBuffer(first_substr_char.wchar, 
        first_substr_char.ptr_after, rest_nbytes);
        
    return (const CharLenCE) {strbuff.data, new_nbytes, encoding};
  } else {
    
    const int new_nbytes = end_of_sub - start_of_sub;
    return (const CharLenCE) {start_of_sub, new_nbytes, encoding};
  }
}


// Functions that return substrings:


static FORCE_INLINE CharLenCE 
signedSubstrUtf8(const char *start_of_str, const int nbytes, 
                 const cetype_t encoding,
                 int i_start, int i_stop,
                 const bool skip_validity_check) {
  
  if (i_start >= 0 && i_stop >= 0) {
    return substrUtf8(start_of_str, nbytes, encoding, i_start, i_stop, 
                      skip_validity_check);
  }
  
  if (!skip_validity_check && !isValidUtf8(start_of_str, nbytes)) {
    error("Elt is invalid UTF8-encoded string \'%s\'", start_of_str);
  }
  
  const char *restrict str = start_of_str;
  const char *const end    = start_of_str + nbytes;
  
  const int lookback_length = 20;
  
  int count = 0;
  if (i_start >= 0) {  // start >= 0, stop < 0
  
    const int just_before_start = i_start - 1;
    // Increment the str pointer past all the characters at the front to be
    // omitted:
    StrIndex start_of_sub =u8Count(count, just_before_start, start_of_str, end);
    count = start_of_sub.index;
    
    if (i_stop == -1) {
      return (const CharLenCE) {start_of_sub.ptr, 
                                nbytes - (int)(start_of_sub.ptr - start_of_str), 
                                encoding};
    }
  
    if (-i_stop <= lookback_length) {  // Use lookback
    
      const char *last_m_chars_ptrs[lookback_length];
      
      StrLookbackIndex end_of_str = u8CountToEndWithLookback(count, 
        start_of_sub.ptr, end, lookback_length, last_m_chars_ptrs);
      
      const int nchars = end_of_str.index;
      const int i = end_of_str.lookback_index;
      
      if ((nchars + i_stop) + 1 < i_start) { return EMPTY_STRING_CLC; }
      
      const int new_nbytes = getSubNbytesFromLookback(start_of_sub.ptr, 
        end, lookback_length, i, i_stop, last_m_chars_ptrs);
      
      return (const CharLenCE) {start_of_sub.ptr, new_nbytes, encoding};
    
    } else {  // Lookback cannot be used because it does not go far enough back
    
      // Count to end:
      StrIndex end_of_str = u8CountToEnd(count, start_of_sub.ptr, end);
      
      const int nchars = end_of_str.index;
      const int i_stop_abs = (nchars + i_stop) + 1;
      // The brackets prevent overflow
      
      if (i_stop_abs < i_start) { return EMPTY_STRING_CLC; }
      
      //Count from start again:
      const char *end_of_sub = u8Count(count, i_stop_abs, start_of_sub.ptr,
                                       end).ptr;
      
      const int new_nbytes = end_of_sub - start_of_sub.ptr;
      return (const CharLenCE) {start_of_sub.ptr, new_nbytes, encoding};
    }
  }
  else if (i_stop >= 0) {  // start < 0, stop >= 0
  
    if (-i_start <= lookback_length) {  // Use lookback
      const char *last_m_chars_ptrs[lookback_length];
      
      // Count to stop:
      StrLookbackIndex end_of_sub = u8CountWithLookback(0, i_stop, 
        start_of_str, end, lookback_length, last_m_chars_ptrs);
      count       = end_of_sub.index;
      const int i = end_of_sub.lookback_index;
      
      // Keep counting to end of string:
      StrIndex end_of_str = u8Count(count, count-i_start, end_of_sub.ptr, end);
      
      if (end_of_str.ptr != end) { return EMPTY_STRING_CLC; }
      const int nchars = end_of_str.index;
      
      const int i_start_abs = unguardedMax(nchars + i_start, 0) + 1;
      i_stop                = unguardedMin(i_stop, nchars);
      
      if (i_stop < i_start_abs) { return EMPTY_STRING_CLC; }
      
      const int just_before_start = i_start_abs - 1;
      const char *start_of_sub;
      if (1 < i_start_abs && (i_stop - just_before_start) <= lookback_length) {
        // Refer to the lookback
        const int from_end = -(i_stop - just_before_start);
        const int index = lookbackIndex(lookback_length, i, from_end);
        start_of_sub = last_m_chars_ptrs[index];
        
      } else {
        // The lookback cannot be used because it does not go far enough back.
        // Count to start:
        start_of_sub = u8Count(0, just_before_start, start_of_str, end).ptr;
      }
      
      const int new_nbytes = end_of_sub.ptr - start_of_sub;
      return (const CharLenCE) {start_of_sub, new_nbytes, encoding};
      
    } else {  // No lookback
      
      // Count to stop:
      StrIndex end_of_sub = u8Count(count, i_stop, start_of_str, end);
      count = end_of_sub.index;
      
      // Keep counting to end, stopping early if i_stop + |i_start| is reached.
      // Note that i_stop + |i_start| = count-i_start
      StrIndex end_of_str = u8Count(count, count-i_start, end_of_sub.ptr, end);
      
      if (end_of_str.ptr != end) { return EMPTY_STRING_CLC; }
      const int nchars = end_of_str.index;
      
      const int i_start_abs = unguardedMax(nchars + i_start, 0) + 1;
      
      if (i_stop < i_start_abs) { return EMPTY_STRING_CLC; }
      
      // Count to start:
      const int just_before_start = i_start_abs - 1;
      const char *start_of_sub = u8Count(0, just_before_start, start_of_str, 
                                         end).ptr;
      
      const int new_nbytes = end_of_sub.ptr - start_of_sub;
      return (const CharLenCE) {start_of_sub, new_nbytes, encoding};
    }
    
  }
  else {  // start < 0 AND stop < 0
  
    if (-i_stop <= lookback_length) {  // Use lookback
    
      const char *last_m_chars_ptrs[lookback_length];
      
      // Count to end:
      StrLookbackIndex end_of_str = u8CountToEndWithLookback(0, str, end, 
        lookback_length, last_m_chars_ptrs);
      const int nchars = end_of_str.index;
      const int i      = end_of_str.lookback_index;
      
      if (i_stop < -nchars) { return EMPTY_STRING_CLC; }
      
      // Get start of sub:
      const char *start_of_sub;
      if (-i_start <= lookback_length) {  // Start will be also in the lookback:
        
        i_start = max(i_start, -nchars);
        
        // Get start from lookback:
        const int index = lookbackIndex(lookback_length, i, i_start);
        start_of_sub    = last_m_chars_ptrs[index];
        
      } else {
        
        const int i_start_abs = unguardedMax(nchars + i_start, 0) + 1;
        
        // Count to start:
        const int just_before_start = i_start_abs - 1;
        start_of_sub = u8Count(0, just_before_start, start_of_str, end).ptr;
      }
      
      // Get stop from lookback:
      const int new_nbytes = getSubNbytesFromLookback(start_of_sub, end, 
        lookback_length, i, i_stop, last_m_chars_ptrs);
        
      return (const CharLenCE) {start_of_sub, new_nbytes, encoding};
      
    } else {  // No lookback
    
      // Count to end:
      StrIndex end_of_str   = u8CountToEnd(0, str, end);
      
      // Calculate absolute indices:
      const int nchars            = end_of_str.index;
      const int just_before_start = nchars + i_start; //max( ,0) not needed here
      const int i_stop_abs        = (nchars + i_stop) + 1;
      // The brackets prevent overflow
      
      // Count to start:
      StrIndex start_of_sub = u8Count(0, just_before_start, start_of_str, end);
      
      // Count to stop:
      StrIndex end_of_sub   = u8Count(start_of_sub.index, i_stop_abs, 
                                      start_of_sub.ptr, end);
      
      const int new_nbytes = end_of_sub.ptr - start_of_sub.ptr;
      return (const CharLenCE) {start_of_sub.ptr, new_nbytes, encoding};
      
    }
  }
}


static FORCE_INLINE CharLenCE 
signedSubstrMultiByteChars(const char *const restrict start_of_str, 
                           const int nbytes, const cetype_t encoding,
                           int i_start, int i_stop) {
  
  if (i_start >= 0 && i_stop >= 0) {
    return substrMultiByteChars(start_of_str, nbytes, encoding, i_start, 
                                i_stop);
  }
  
  mbstate_t mb_st;
  memset(&mb_st, 0, sizeof(mbstate_t));
  
  const char *const restrict end = start_of_str + nbytes;
  
  const int lookback_length = 20;
  
  int count = 0;
  if (i_start >= 0) {  // start >= 0, stop < 0
    
    // Count to start and save it
    const int just_before_start = i_start - 1;
    StrIndex start_of_sub = mbCount(count, just_before_start, start_of_str, end,
                                    &mb_st);
    count = start_of_sub.index;
    
    if (-i_stop <= lookback_length) {  // Use lookback
    
      if (!mbsinit(&mb_st)) {
        return signedSubstrWithinShiftStateWithLookback(
          start_of_sub.ptr, encoding, mb_st, end, count, i_start, i_stop,
          lookback_length);
      } else {
        // No need to account for state changes prior to start of the substring
        SIGNED_SUBSTR_IN_CUR_STATE_WITH_LOOKBACK(start_of_sub.ptr, new_nbytes)
        
        return (const CharLenCE) {start_of_sub.ptr, new_nbytes, encoding};
      }
      
    } else {  // Lookback cannot be used because it does not go far enough back
      if (!mbsinit(&mb_st)) {
        return signedSubstrWithinShiftState(
          start_of_sub.ptr, encoding, mb_st, end, count, i_start, i_stop);
        
      } else {
        // No need to account for state changes prior to start of the substring
        NEGATIVE_STOP_SUBSTR_IN_CUR_STATE(start_of_sub.ptr, new_nbytes)

        return (const CharLenCE) {start_of_sub.ptr, new_nbytes, encoding};
      }
    }
    
  }
  else if (i_stop >= 0) {  // start < 0, stop >= 0
    
    if (-i_start <= lookback_length) {  // Use lookback
      
      const char *last_m_chars_ptrs[lookback_length];
      mbstate_t last_m_mbstates[lookback_length];
      
      StrLookbackIndex end_of_sub = mbCountWithLookback(0, i_stop, 
        start_of_str, end, &mb_st, lookback_length, last_m_chars_ptrs, 
        last_m_mbstates);
      count       = end_of_sub.index;
      const int i = end_of_sub.lookback_index;
      
      StrIndex end_of_str = mbCount(count, count-i_start, end_of_sub.ptr, end, 
                                    &mb_st);
      if (end_of_str.ptr != end) { return EMPTY_STRING_CLC; }
      const int nchars = end_of_str.index;
      
      const int i_start_abs = unguardedMax(nchars + i_start, 0) + 1;
      i_stop = min(i_stop, nchars);
      
      if (i_stop < i_start_abs) { return EMPTY_STRING_CLC; }
      
      const char *start_of_sub;
      if (1 < i_start_abs && (i_stop - (i_start_abs-1)) <= lookback_length) {
        // Refer to the lookback
        const int from_end = -(i_stop - (i_start_abs-1));
        const int index = 
          lookbackIndex(lookback_length, i, from_end);
        start_of_sub = last_m_chars_ptrs[index];
        mb_st        = last_m_mbstates[index];
        
      } else {
        // The lookback cannot be used because it does not go far enough back
        start_of_sub = 
          mbRecountFromZeroToStart(i_start_abs, start_of_str, end, &mb_st).ptr;
      }
      return substrShiftStateKnownBytes(start_of_sub, end_of_sub.ptr, 
                                        encoding, mb_st);
      
    } else {  // No lookback
      
      // Count to stop:
      StrIndex end_of_sub = mbCount(count, i_stop, start_of_str, end, &mb_st);
      count = end_of_sub.index;
      
      // Keep counting to end, stopping early if i_stop + |i_start| is reached.
      // Note that i_stop + |i_start| = count-i_start
      StrIndex end_of_str = mbCount(count, count-i_start, end_of_sub.ptr, end, 
                                    &mb_st);
      if (end_of_str.ptr != end) { return EMPTY_STRING_CLC; }
      int nchars = end_of_str.index;
      
      const int i_start_abs = (nchars + i_start) + 1;
      // max(nchars + i_start, 0) is not needed here because the case where 
      // i_start < -nchars is dealt with by mbRecountFromZeroToStart().
      
      if (i_stop < i_start_abs) { return EMPTY_STRING_CLC; }
      
      const char *start_of_sub = 
        mbRecountFromZeroToStart(i_start_abs, start_of_str, end, &mb_st).ptr;
      
      return substrShiftStateKnownBytes(start_of_sub, end_of_sub.ptr, 
                                        encoding, mb_st);
    }
    
  } else {  // start < 0 AND stop < 0
    
    if (-i_stop <= lookback_length) {  // Use lookback
    
      const char *last_m_chars_ptrs[lookback_length];
      int nchars, i;
      const char *start_of_sub;
      
      // Find start of sub:
      if (-i_start <= lookback_length) {
        
        mbstate_t last_m_mbstates[lookback_length];
        StrLookbackIndex end_of_str = 
        mbCountToEndWithStatefulLookback(0, start_of_str, end, &mb_st, 
          lookback_length, last_m_chars_ptrs, last_m_mbstates);
        nchars = end_of_str.index;
        i      = end_of_str.lookback_index;
        
        if (i_stop < -nchars) { return EMPTY_STRING_CLC; }
        
        i_start = max(i_start, -nchars); //max(nchars + i_start,0) embedded here
        
        const int index = lookbackIndex(lookback_length, i, i_start);
        start_of_sub    = last_m_chars_ptrs[index];
        mb_st           = last_m_mbstates[index];
      } else {
        
        StrLookbackIndex end_of_str = mbCountToEndWithLookback(0, start_of_str, end, 
          &mb_st, lookback_length, last_m_chars_ptrs);
        nchars = end_of_str.index;
        i      = end_of_str.lookback_index;
        
        if (i_stop < -nchars) { return EMPTY_STRING_CLC; }
        
        const int i_start_abs = (nchars + i_start) + 1;  //max( ,0) already done
        // The brackets prevent overflow
        start_of_sub = mbRecountFromZeroToStart(i_start_abs, start_of_str, end,
                                                &mb_st).ptr;
      }
      
      if (!mbsinit(&mb_st)) {
        
        if (start_of_sub == end) { return EMPTY_STRING_CLC; }
        
        WCharFromMB first_substr_char = stateShiftAndFirstChar(start_of_sub,
                                                               &mb_st);
        const char *rest_of_sub = first_substr_char.ptr_after;
        
        const int rest_nbytes = getSubNbytesFromLookback(rest_of_sub, end, 
          lookback_length, i, i_stop, last_m_chars_ptrs);
        
        const int new_nbytes = writeShiftStateSubstrToBuffer(
          first_substr_char.wchar, rest_of_sub, rest_nbytes);
  
        return (const CharLenCE) {strbuff.data, new_nbytes, encoding};
        
      } else {

        const int new_nbytes = getSubNbytesFromLookback(start_of_sub, end, 
          lookback_length, i, i_stop, last_m_chars_ptrs);
          
        return (const CharLenCE) {start_of_sub, new_nbytes, encoding};
      }
      
    } else {  // No lookback
    
      StrIndex end_of_str = mbCountToEnd(0, start_of_str, end, &mb_st);
      
      // Calculate absolute indices:
      const int nchars = end_of_str.index;
      const int i_start_abs = (nchars + i_start) + 1;
      const int i_stop_abs  = (nchars + i_stop)  + 1;
      // The brackets prevent overflow
      // max(nchars + i_start, 0) is not needed here because the case where 
      // i_start < -nchars is dealt with by mbRecountFromZeroToStart().
      
      StrIndex start_of_sub = 
        mbRecountFromZeroToStart(i_start_abs, start_of_str, end, &mb_st);
      count = start_of_sub.index;
      
      if (!mbsinit(&mb_st)) {
        
        return substrWithinShiftStateUnguarded(start_of_sub.ptr, encoding,
          mb_st, end, count, i_stop_abs);
        
      } else {
        
        StrIndex end_of_sub = mbRecount(count, i_stop_abs, start_of_sub.ptr, end,
                                      &mb_st);
        
        const int new_nbytes = end_of_sub.ptr - start_of_sub.ptr;
        
        return (const CharLenCE) {start_of_sub.ptr, new_nbytes, encoding};
      }
    }
  }
}