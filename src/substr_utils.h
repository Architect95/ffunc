/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2020  The R Core Team
 * 
 *  Modified on 08 May 2021 by Architect95
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



LibImport Rboolean mbcslocale;  // From line 30 of text.c. See it in l10n_info()
bool               is_stateless_enc;
#define ASCII_MASK (1<<6)
#define IS_ASCII(x) (LEVELS(x) & ASCII_MASK)


#define FAST_ARRAY_MEM_LIMIT 10000000  // 10m.
// This length is chosen heuristically to avoid slow memory usage patterns.

#define NA_STRING_CLC    (const CharLenCE) {NULL, -1, CE_NATIVE}
#define EMPTY_STRING_CLC (const CharLenCE) {NULL,  0, CE_NATIVE}


#define FSUBSTR_LOOP(substr_func, substr_start, substr_end) {                 \
                                                                              \
  if (!is_stateless_enc) {                                                    \
    /* Need to write to buffer, so cannot use parallel code */                \
    strbuff.data = malloc(strbuff.defaultSize);                               \
    CharLenCE substring;                                                      \
                                                                              \
    SEXP prev_elt = NULL;                                                     \
                                                                              \
    for (R_xlen_t i = 0; i < n; ++i) {                                        \
      const SEXP restrict element = STRING_ELT(x,i);                          \
      substring =                                                             \
        substr_func(element, (substr_start), (substr_end),                    \
                    element == prev_elt);  /*Ptr comparison to prev_elt*/     \
      prev_elt = element;                                                     \
      SET_STRING_ELT(output, i,                                               \
        mkCharLenCE(substring.str_ptr, substring.nbytes, substring.enc));     \
    }                                                                         \
                                                                              \
    freeStringBuffer(&strbuff);                                               \
  } else {                                                                    \
    const R_xlen_t batch_size = FAST_ARRAY_MEM_LIMIT;                         \
    const R_xlen_t n_batches  = 1 + ((n - 1) / batch_size);  /*We know n>=1 */\
    const R_xlen_t array_size = ((n_batches > 1) ? batch_size : n);           \
                                                                              \
    CharLenCE * restrict substrings = malloc(array_size * sizeof(CharLenCE)); \
                                                                              \
    for (R_xlen_t j = 0; j < n_batches; ++j) {                                \
                                                                              \
      const R_xlen_t loop_start = j * batch_size;                             \
      const R_xlen_t loop_end   = (j == n_batches-1) ? n : ((j+1)*batch_size);\
      SEXP prev_elt             = NULL;                                       \
                                                                              \
      _Pragma("omp parallel for firstprivate(prev_elt)")                      \
      for (R_xlen_t i = loop_start; i < loop_end; ++i) {                      \
        const SEXP restrict element = STRING_ELT(x,i);                        \
        substrings[i - loop_start] =                                          \
          substr_func(element, (substr_start), (substr_end),                  \
                      element == prev_elt);  /*Ptr comparison to prev_elt*/   \
        prev_elt = element;                                                   \
      }                                                                       \
                                                                              \
      R_CheckUserInterrupt();                                                 \
      fillStringVector(output, loop_start, loop_end, substrings);             \
    }                                                                         \
    free(substrings);                                                         \
  }                                                                           \
}                                                                             \

typedef struct { 
  const int index;
  const char *ptr;
} StrIndex;

typedef struct { 
  const int index;
  const int lookback_index;
  const char *ptr;
} StrLookbackIndex;

typedef struct {
  wchar_t wchar;
  const char *const ptr_after;
} WCharFromMB;


static inline void fillStringVector(SEXP output, R_xlen_t loop_start, 
                                    R_xlen_t loop_end, 
                                    CharLenCE *const substrings) {
  // Most of the runtime of fsubstr() is spent in this function.
  
  // Creating the CHARSXPs cannot be done in parallel because R's string
  // hash table is not thread-safe. SET_STRING_ELT is not thread-safe
  // either because it updates the ref counts of the strings in the hash table,
  // and if the same string appears in two threads then a count could be
  // modified separately in two threads at once.
  
  for (R_xlen_t i = loop_start; i < loop_end; ++i) {
    
    CharLenCE elt = substrings[i - loop_start];
    
    // There are checks in mkCharLenCE() that are not needed here, but there
    // is no way to skip them whilst still using that function.
    bool ptr_not_null = (elt.str_ptr != NULL);
    SEXP str = (ptr_not_null ? mkCharLenCE(elt.str_ptr, elt.nbytes, elt.enc)
                             : ((elt.nbytes == -1) ? NA_STRING : R_BlankString));
    SET_STRING_ELT(output, i, str);
  }
}

static FORCE_INLINE void 
warnIfVectorArgRecycled(const char * restrict arg_name, const R_xlen_t arg_len,
                        const R_xlen_t len_to_match) {
                          
  if ((arg_len < len_to_match) && ((len_to_match % arg_len) != 0)) {
    warning("Length of %s is not a factor of length of vector; "
            "%s will be partially recycled.", arg_name, arg_name);
  }
}


static FORCE_INLINE StrIndex
mbCount(int count, const int i_stop, 
        const char *str, const char *restrict const end, 
        mbstate_t *const restrict mb_st_ptr) {
  
  for (; count < i_stop && str < end; ++count) {
    str += Mbrtowc(NULL, str, MB_CUR_MAX, mb_st_ptr);
  }

  return (const StrIndex) {count, str};
}

static FORCE_INLINE StrIndex
mbRecount(int count, const int i_stop, 
          const char *str, const char *restrict const end, 
          mbstate_t *const restrict mb_st_ptr) {
  
  // Uses mbrtowc() without error checking. For use on strings that have already
  // been error-checked with Mbrtowc().
  
  for (; count < i_stop && str < end; ++count) {
    str += mbrtowc(NULL, str, MB_CUR_MAX, mb_st_ptr);
  }

  return (const StrIndex) {count, str};
}

static FORCE_INLINE WCharFromMB
stateShiftAndFirstChar(const char *str, mbstate_t *const mb_st_ptr) {
  // Convert first character of substring to wchar_t and back to multibyte chars
  // in order to store the appropriate state change control characters:
  
  // We know str != end here, so no need to test this.
  
  wchar_t first_substr_char;
  str += Mbrtowc(&first_substr_char, str, MB_CUR_MAX, mb_st_ptr);

  return (const WCharFromMB) { first_substr_char, str };
}

static FORCE_INLINE long
writeShiftStateSubstrToBuffer(const wchar_t first_substr_char, 
                              const char *restrict const rest_of_sub, 
                              const int rest_nbytes) {
                                
  int buffer_len = MB_CUR_MAX + rest_nbytes;
  char *const buffer = allocStringBuffer(buffer_len, &strbuff);
  
  mbstate_t sub_mb_st;
  memset(&sub_mb_st, '\0', sizeof(mbstate_t));
  
  // Write the substring to the buffer:
  char *out = buffer;
  out += wcrtomb(out, first_substr_char, &sub_mb_st);  // First char
  const char new_first_mb_char_width = out - buffer;
  memcpy(out, rest_of_sub, rest_nbytes);               // Rest of substring
  
  return new_first_mb_char_width + rest_nbytes;
}

#define SUBSTR_AFTER_SHIFT_STATE_IS_SET(count_fn)                             \
                                                                              \
  StrIndex end_of_sub = count_fn(cur_count, i_stop, rest_of_sub, end, &mb_st);\
                                                                              \
  const ptrdiff_t rest_nbytes = end_of_sub.ptr - rest_of_sub;                 \
                                                                              \
  long new_nbytes =                                                           \
    writeShiftStateSubstrToBuffer(first_substr_char, rest_of_sub,             \
                                  rest_nbytes);                               \
                                                                              \
  return (const CharLenCE) {strbuff.data, new_nbytes, encoding};              \


static FORCE_INLINE CharLenCE
substrAfterShiftStateIsSet(
  const char *rest_of_sub, const cetype_t encoding, mbstate_t mb_st, 
  const char *const end, const int cur_count, const int i_stop, 
  const wchar_t first_substr_char) {
  
  SUBSTR_AFTER_SHIFT_STATE_IS_SET(mbCount)
}

static FORCE_INLINE CharLenCE
substrAfterShiftStateIsSetUnguarded(
  const char *rest_of_sub, const cetype_t encoding, mbstate_t mb_st, 
  const char *const end, const int cur_count, const int i_stop,
  const wchar_t first_substr_char) {
  
  SUBSTR_AFTER_SHIFT_STATE_IS_SET(mbRecount)
}

static FORCE_INLINE CharLenCE
substrWithinShiftState(const char *start_of_sub, const cetype_t encoding, 
  mbstate_t mb_st, const char *const end, int nchars, const int i_stop) {
  // Cannot directly copy the bytes of the substring in the original
  // multibyte substring, because the state changing control characters
  // for the current multibyte state occure before start_point in the
  // original string.
  
  if (start_of_sub == end) { return EMPTY_STRING_CLC; }
  
  WCharFromMB first_substr_char = stateShiftAndFirstChar(start_of_sub, &mb_st);
  ++nchars;

  return substrAfterShiftStateIsSet(first_substr_char.ptr_after, encoding, 
    mb_st, end, nchars, i_stop, first_substr_char.wchar);
}




// Functions that return substrings:

static FORCE_INLINE CharLenCE 
substrUtf8(const char *str, const int nbytes, const cetype_t encoding,
           const int i_start, const int i_stop,
           const bool skip_validity_check) {
  
  if (!skip_validity_check && !isValidUtf8(str, nbytes)) {
    error("Elt is invalid UTF8-encoded string \'%s\'", str);
  }
  
  const int just_before_start = i_start - 1;
  const char *const end = str + nbytes;
  // Increment the str pointer past all the characters at the front to be
  // omitted:
  int j = 0;
  for (; j < just_before_start && str < end; ++j) {
    // The comparison to end is needed because there's no check whether a
    // given char is the null terminator.
    str += utf8clen(*str);
  }
  const char *const start_point = str;
  for (; j < i_stop && str < end; ++j) {
    str += utf8clen(*str);
  }
  return (const CharLenCE) {start_point, (int)(str - start_point), encoding};
}

static FORCE_INLINE CharLenCE 
substrSingleByteChars(const char *str, const int nbytes,
                      const cetype_t encoding,
                      const int i_start, const int i_stop) {
  
  if (i_stop >= nbytes) {
    return (const CharLenCE) {str + i_start -1, nbytes - i_start +1, encoding};
  } else {
    return (const CharLenCE) {str + i_start -1, i_stop - i_start +1, encoding};
  }
  // A branchless alternative using min(i_stop, nbytes) is possible, but it is
  // empirically slower in benchmarking examples (possibly due to regular data
  // leading to accurate branch prediction).
}


static FORCE_INLINE CharLenCE 
substrMultiByteChars(const char *const start_of_str, const int nbytes,
                     const cetype_t encoding,
                     const int i_start, const int i_stop) {
  
  mbstate_t mb_st;
  memset(&mb_st, 0, sizeof(mbstate_t));
  
  const char *restrict str = start_of_str;

  const int just_before_start = i_start - 1;
  const char *const end = str + nbytes;
  
  // Move str to the start of the substring:
  StrIndex start_of_sub = mbCount(0, just_before_start, str, end, &mb_st);
  str = start_of_sub.ptr;
  
  if (!mbsinit(&mb_st)) {  
    return substrWithinShiftState(str, encoding, mb_st, end, start_of_sub.index,
                                  i_stop);
  }
  
  if (i_stop >= nbytes) {
    return (const CharLenCE) {str, nbytes -(int)(str - start_of_str), encoding};
  } else {
    
    StrIndex end_of_sub = mbCount(start_of_sub.index, i_stop, str, end, &mb_st);
    
    int new_nbytes = end_of_sub.ptr - start_of_sub.ptr;
    
    return (const CharLenCE) {start_of_sub.ptr, new_nbytes, encoding};
  }
}
