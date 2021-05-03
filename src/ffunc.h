/*
 * ffunc : Fast utility functions for R, implemented in C
 * Copyright (C) 2021 Architect95
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef FFUNC_H
#define FFUNC_H

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rversion.h>
#if !defined(R_VERSION) || R_VERSION < R_Version(3, 5, 0)
  #define USE_RINTERNALS
  #define DATAPTR_RO(x) ((const void *)DATAPTR(x))
#endif

#include <Rinternals.h>

#include <assert.h>
#include <stdlib.h>
// #include <stdint.h>
#include <stdbool.h>


#ifdef _OPENMP
  #include<omp.h>
  #define OMP_MAX_THREADS omp_get_num_procs()
#else
  #define OMP_MAX_THREADS 1
#endif


extern SEXP fsubstrR(SEXP x, SEXP start, SEXP stop);
extern SEXP fsubstrassignR(SEXP x, SEXP start, SEXP stop, SEXP value);
extern SEXP fstr_subR(SEXP x, SEXP start, SEXP stop);


// Forced inlining macro:
#ifdef _MSC_VER
  #define FORCE_INLINE inline __forceinline
#elif defined(__GNUC__)
  #define FORCE_INLINE inline __attribute__((always_inline))
#else
  #define FORCE_INLINE inline
#endif



#endif