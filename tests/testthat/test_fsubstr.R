# ffunc : Fast utility functions for R, implemented in C
# Copyright (C) 2021 Architect95
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.



n_elts_avoid_sso <- 3500  # Input vector must be long enough to avoid
                          # small size optimizations
test_string      <- "abcdefghij"
test_strings     <- rep(test_string, times = n_elts_avoid_sso)

len              <- nchar(test_string)
test_lengths     <- c(-2:(len+1), 2*len)
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))


test_that("Substrings using all combinations of test_lengths", {
  expect_equal(
    substr( test_strings, start, stop),
    fsubstr(test_strings, start, stop)
  )
})

test_that("Substrings using every ASCII character", {
  
  for(i in 0:255) {
    
    test_string      <- paste0("ab", rawToChar(as.raw(i)), "cd")
    
    test_strings     <- rep(test_string, times = n_elts_avoid_sso)
    len              <- nchar(test_string)
    test_lengths     <- c(-2:(len+1), 2*len)
    start            <- rep(test_lengths, times = length(test_lengths))
    stop             <- rep(test_lengths, each  = length(test_lengths))
    
    expect_equal(
      substr( test_strings, start, stop),
      fsubstr(test_strings, start, stop)
    )
  }
})

test_that("Substrings using every 1000th UTF-8 character", {
  
  for(i in c(seq(1, 60000, 100), seq(60001, 1200001, 1000))) {
    
    test_string      <- paste0("a", intToUtf8(i), "a")
    
    test_strings     <- rep(test_string, times = n_elts_avoid_sso)
    len              <- nchar(test_string)
    test_lengths     <- -1:(len+1)
    start            <- rep(test_lengths, times = length(test_lengths))
    stop             <- rep(test_lengths, each  = length(test_lengths))
    
    expect_equal(
      substr( test_strings, start, stop),
      fsubstr(test_strings, start, stop)
    )
  }
})

test_that("Length-one inputs", {
  expect_equal(
    substr( rep(c("Alpha"), times=n_elts_avoid_sso), start, stop),
    fsubstr(rep(c("Alpha"), times=n_elts_avoid_sso), start, stop)
  )
  expect_equal(
    substr( test_strings, 3, stop),
    fsubstr(test_strings, 3, stop)
  )
  expect_equal(
    substr( test_strings, start, 5),
    fsubstr(test_strings, start, 5)
  )
  expect_equal(
    substr( test_strings, 3, 5),
    fsubstr(test_strings, 3, 5)
  )
})

test_that("NA string", {
  expect_equal(
    substr( rep(c(NA, test_string), each=n_elts_avoid_sso), start, stop),
    fsubstr(rep(c(NA, test_string), each=n_elts_avoid_sso), start, stop)
  )
  expect_equal(
    substr( NA, start, stop),
    fsubstr(NA, start, stop)
  )
  expect_equal(
    substr( rep(NA, times=n_elts_avoid_sso), start, stop),
    fsubstr(rep(NA, times=n_elts_avoid_sso), start, stop)
  )
})

test_that("NA start and stop", {
  expect_equal(
    substr( test_strings, c(NA, 1), stop),
    fsubstr(test_strings, c(NA, 1), stop)
  )
  expect_equal(
    substr( test_strings, start, c(NA, 1)),
    fsubstr(test_strings, start, c(NA, 1))
  )
  expect_equal(
    substr( test_strings, c(NA, 1), c(NA, 1, NA)),
    fsubstr(test_strings, c(NA, 1), c(NA, 1, NA))
  )
  expect_equal(
    substr( test_strings, NA, stop),
    fsubstr(test_strings, NA, stop)
  )
  expect_equal(
    substr( test_strings, start, NA),
    fsubstr(test_strings, start, NA)
  )
  expect_equal(
    substr( test_strings, NA, NA),
    fsubstr(test_strings, NA, NA)
  )
})

test_that("NULL inputs", {
  expect_equal(
    substr( NULL, start, stop),
    fsubstr(NULL, start, stop)
  )
  expect_error(
    fsubstr(test_strings, NULL, stop), "Start vector has zero length"
  )
  expect_error(
    fsubstr(test_strings, start, NULL), "Stop vector has zero length"
  )
})

test_that("Zero length inputs", {
  expect_equal(
    substr( c(), start, stop),
    fsubstr(c(), start, stop)
  )
  expect_equal(
    substr( character(), start, stop),
    fsubstr(character(), start, stop)
  )
  expect_error(
    fsubstr(test_strings, c(), stop), "Start vector has zero length"
  )
  expect_error(
    fsubstr(test_strings, start, c()), "Stop vector has zero length"
  )
})

test_that("Empty strings", {
  expect_equal(
    substr( rep(c(""), times=n_elts_avoid_sso), start, stop),
    fsubstr(rep(c(""), times=n_elts_avoid_sso), start, stop)
  )
})

test_that("Non-integer start and stop", {
  
  start_nonint <- c(2.3, 1.1, 3.8, 3)
  stop_nonint  <- c(2.3, 1.1, 3.8, 3)
  
  expect_equal(
    substr( test_strings, start_nonint, stop),
    fsubstr(test_strings, start_nonint, stop)
  )
  expect_equal(
    substr( test_strings, start, stop_nonint),
    fsubstr(test_strings, start, stop_nonint)
  )
  expect_equal(
    substr( test_strings, start_nonint, stop_nonint),
    fsubstr(test_strings, start_nonint, stop_nonint)
  )
})

test_that("CE_UTF8-encoded strings", {
  
  test_string <- intToUtf8(10001:10030)
  
  # Confirm that the test string is indeed UTF-8 encoded on the system running
  # the test:
  expect_equal( Encoding(test_string), "UTF-8" )
  
  test_strings     <- rep(test_string, times = n_elts_avoid_sso)
  
  len              <- nchar(test_string)
  test_lengths     <- c(-2:(len+1), 2*len)
  start            <- rep(test_lengths, times = length(test_lengths))
  stop             <- rep(test_lengths, each  = length(test_lengths))
  
  expect_equal(
    substr( test_strings, start, stop),
    fsubstr(test_strings, start, stop)
  )
})

encodingEnum <- inline::cfunction(
  c(x = "character", i = "integer"), '
    int elt = asInteger(i);
    if (elt < 1) {
      error("i must be >= 1");
    } else if (elt > xlength(x)) {
      error("i must be <= length of the string vector");
    }
    cetype_t encoding = getCharCE(STRING_ELT(x, elt - 1));
    return ScalarInteger(encoding);
  '
)
# From Rinternals.h:
# CE_NATIVE =  0,
# CE_UTF8   =  1,
# CE_LATIN1 =  2,
# CE_BYTES  =  3,
# CE_SYMBOL =  5,
# CE_ANY    = 99

test_that("CE_NATIVE-encoded strings", {
  
  test_string <- enc2native(rawToChar(as.raw(120:255)))
  
  # Confirm that the test string is indeed in native encoding on the system
  # running the test:
  expect_equal(encodingEnum(test_string, 1), 0)
  
  test_strings     <- rep(test_string, times = n_elts_avoid_sso)
  
  expect_equal(
    substr( test_strings, start, stop),
    fsubstr(test_strings, start, stop)
  )
})

test_that("CE_LATIN1-encoded strings", {
  
  test_string <- paste0("abcde_\xc3\xc4\xc5\xc6\xc7\xc8_abcde")
  Encoding(test_string) <- "latin1"
  
  # Confirm that the test string is indeed in latin1 encoding on the system
  # running the test:
  expect_equal(Encoding(test_string), "latin1")
  
  test_strings     <- rep(test_string, times = n_elts_avoid_sso)
  
  expect_equal(
    substr( test_strings, start, stop),
    fsubstr(test_strings, start, stop)
  )
})

test_that("CE_BYTES-encoded strings", {
  
  test_string <- paste0("abcde_\xc3\xc4\xc5\xc6\xc7\xc8_abcde")
  Encoding(test_string) <- "bytes"
  
  # Confirm that the test string is indeed in bytes encoding on the system
  # running the test:
  expect_equal(Encoding(test_string), "bytes")
  
  test_strings     <- rep(test_string, times = n_elts_avoid_sso)
  
  expect_equal(
    substr( test_strings, start, stop),
    fsubstr(test_strings, start, stop)
  )
})

# CE_ANY and CE_SYMBOL appear to be internal parameter values that it is not
# necessary to test.
#
# Attempting to save a string with encoding CE_ANY or CE_SYMBOL using mkCharCE()
# produces the error 'unknown encoding mask' (unless the string already exists 
# in R's string hash table, in which case no new entry is added to the hash 
# table, and the encoding specifier of the existing entry is not modified).


test_that("SHALLOW_DUPLICATE_ATTRIB preserves the class of the input", {
  
  class(test_strings) <- "my_example_class"
  
  expect_equal(
    substr( test_strings, start, stop),
    fsubstr(test_strings, start, stop)
  )
  # More readable error message in the case where the class does not match:
  expect_equal(
    class(substr( test_strings, start, stop)),
    class(fsubstr(test_strings, start, stop))
  )
})

