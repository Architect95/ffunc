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


static inline void 
fillStringVector(
  SEXP output, R_xlen_t loop_start, R_xlen_t loop_end, 
  CharLenCE *const substrings);
  
static FORCE_INLINE void
warnIfVectorArgRecycled(
  const char * restrict arg_name, const R_xlen_t arg_len, 
  const R_xlen_t len_to_match);

static FORCE_INLINE StrIndex
mbCount(
  int count, const int i_stop, const char *str, 
  const char *restrict const end, mbstate_t *const restrict mb_st_ptr);
  
static FORCE_INLINE StrIndex 
mbRecount(
  int count, const int i_stop, const char *str, 
  const char *restrict const end, mbstate_t *const restrict mb_st_ptr);
  
static FORCE_INLINE WCharFromMB
stateShiftAndFirstChar(
  const char *str, mbstate_t *const mb_st_ptr);
  
static FORCE_INLINE long 
writeShiftStateSubstrToBuffer(
  const wchar_t first_substr_char, const char *restrict const rest_of_sub, 
  const int rest_nbytes);
  
static FORCE_INLINE CharLenCE 
substrAfterShiftStateIsSet(
  const char *rest_of_sub, const cetype_t encoding, mbstate_t mb_st, 
  const char *const end, const int cur_count, const int i_stop, 
  const wchar_t first_substr_char);
  
static FORCE_INLINE CharLenCE 
substrAfterShiftStateIsSetUnguarded(
  const char *rest_of_sub, const cetype_t encoding, mbstate_t mb_st, 
  const char *const end, const int cur_count, const int i_stop,
  const wchar_t first_substr_char);

static FORCE_INLINE CharLenCE 
substrWithinShiftState(
  const char *start_of_sub, const cetype_t encoding, mbstate_t mb_st, 
  const char *const end, int nchars, const int i_stop);



// Functions that return substrings:

static FORCE_INLINE CharLenCE
substrUtf8(
  const char *str, const int nbytes, const cetype_t encoding,
  const int i_start, const int i_stop, const bool skip_validity_check);
  
static FORCE_INLINE CharLenCE
substrSingleByteChars(
  const char *str, const int nbytes, const cetype_t encoding,
  const int i_start, const int i_stop);
  
static FORCE_INLINE CharLenCE
substrMultiByteChars(
  const char *str, const int nbytes, const cetype_t encoding,
  const int i_start, const int i_stop);