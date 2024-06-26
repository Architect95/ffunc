# ffunc : Fast utility functions for R, implemented in C
# Copyright (C) 2022, 2023 Architect95
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

source(test_path("../common/fsubstr.R"))

library(stringr)  # fstr_sub is tested against stringr::str_sub

if(exists("LOCALE_TO_USE")){
  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", LOCALE_TO_USE)
}


n_elts_avoid_sso <- 3500  # Input vector must be long enough to avoid
                          # small size optimisations.
                          # Currently, there is no such small size optimization
                          # to avoid.

test_string      <- "abcdefghijklmnopqrstuvwxyz"
test_strings     <- rep(test_string, times = n_elts_avoid_sso)

len              <- nchar(test_string)
test_lengths     <- c(-2*len, -(len+5):(len+5), 2*len)
                    # Lengths chosen such that some of them go beyond the ends
                    # of the test string, in order to test such cases

start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))


# Ensure that length(start) is significantly higher than n_elts_avoid_sso:
start <- rep(start, times = ((length(test_strings) + 10) %/% length(start)) + 1)
stop  <- rep(stop,  times = ((length(test_strings) + 10) %/% length(stop )) + 1)


test_that("Combinations of start and stop values", {
  expect_equal(
    str_sub( test_string, start, stop),
    fstr_sub(test_string, start, stop)
  )
})

test_that("Combinations of start and stop values where the string is shorter
than the lookback length", {
  
  test_string  <- substr(test_string, 1, 3)
  test_strings <- rep(test_string, times = n_elts_avoid_sso)
  
  expect_equal(
    str_sub( test_strings, start, stop),
    fstr_sub(test_strings, start, stop)
  )
})

test_that("Combinations of start and stop values where the string is longer
than the lookback length", {
  
  test_len <- 10000  # This is assumed to be longer than the lookback length
  
  test_string  <- paste(rep(test_string, times= test_len %/% nchar(test_string)),
                        collapse = "")
  test_strings <- rep(test_string, times = n_elts_avoid_sso)
  
  expect_equal(
    str_sub( test_strings, start, stop),
    fstr_sub(test_strings, start, stop)
  )
})


utf8_string  <- intToUtf8(10001:10030)
utf8_strings <- rep(utf8_string, times = n_elts_avoid_sso)
test_that("Combinations of start and stop values with UTF-8 strings", {
  
  len          <- nchar(utf8_string)
  test_lengths <- c(-2*len, -(len+10):(len+10), 2*len)
  start        <- rep(test_lengths, times = length(test_lengths))
  stop         <- rep(test_lengths, each  = length(test_lengths))
  
  expect_equal(
    str_sub( utf8_strings, start, stop),
    fstr_sub(utf8_strings, start, stop)
  )
})

test_that("Combinations of start and stop values where the UTF-8 string is 
shorter than the lookback length", {
  
  utf8_string <- substr(utf8_string, 1, 3)
  
  # The existing start and stop values are used for this test.
  
  expect_equal(
    str_sub( utf8_string, start, stop),
    fstr_sub(utf8_string, start, stop)
  )
})

test_that("Combinations of start and stop values where the UTF-8 string is 
longer than the lookback length", {
  
  test_len <- 10000  # This is assumed to be longer than the lookback length
  
  utf8_string <- 
    paste(rep(utf8_string, times= test_len %/% nchar(utf8_string)),
          collapse = "")
  
  # The existing start and stop values are used for this test.
  
  expect_equal(
    str_sub( utf8_string, start, stop),
    fstr_sub(utf8_string, start, stop)
  )
})

test_that("Stop == 0 gives empty string", {
  expect_equal(
    rep("", times = n_elts_avoid_sso),
    fstr_sub(test_strings, -41:41, 0)
  )
})

test_that( "Missing arg produces an error", {
  expect_error(
    fstr_sub(does_not_exist, start, stop),
    "object 'does_not_exist' not found"
  )
  expect_error(
    fstr_sub(test_string, does_not_exist, stop), 
    "object 'does_not_exist' not found"
  )
  expect_error(
    fstr_sub(test_string, start, does_not_exist), 
    "object 'does_not_exist' not found"
  )
})

test_that("Length-one inputs", {
  expect_equal(
    str_sub( c("Alpha"), start, stop),
    fstr_sub(c("Alpha"), start, stop)
  )
  expect_equal(
    str_sub( test_strings, 3, stop),
    fstr_sub(test_strings, 3, stop)
  )
  expect_equal(
    str_sub( test_strings, start, 5),
    fstr_sub(test_strings, start, 5)
  )
  expect_equal(
    str_sub( test_strings, 3, 5),
    fstr_sub(test_strings, 3, 5)
  )
})

test_that("NA string", {
  # When given NA string inputs, the behaviour of fstr_sub() follows that of
  # str_sub(), which is different from the behaviour of substr().
  
  expect_equal(
    str_sub( rep(c(NA, test_string), each=n_elts_avoid_sso), rep(start, each=2), 
                                                             rep(stop, each=2)),
    fstr_sub(rep(c(NA, test_string), each=n_elts_avoid_sso), rep(start, each=2), 
                                                             rep(stop, each=2))
  )
  expect_equal(
    str_sub( NA, start, stop),
    fstr_sub(NA, start, stop)
  )
  expect_equal(
    str_sub( rep(NA, times=n_elts_avoid_sso), start, stop),
    fstr_sub(rep(NA, times=n_elts_avoid_sso), start, stop)
  )
})

test_that("NA start and stop", {
  expect_equal(
    str_sub( test_strings, c(NA, 1), stop),
    fstr_sub(test_strings, c(NA, 1), stop)
  )
  expect_equal(
    str_sub( test_strings, start, c(NA, 1)),
    fstr_sub(test_strings, start, c(NA, 1))
  )
  expect_equal(
    str_sub( test_strings, c(NA, 1), c(NA, 1, NA)),
    fstr_sub(test_strings, c(NA, 1), c(NA, 1, NA))
  )
  expect_equal(
    str_sub( test_strings, NA, stop),
    fstr_sub(test_strings, NA, stop)
  )
  expect_equal(
    str_sub( test_strings, start, NA),
    fstr_sub(test_strings, start, NA)
  )
  expect_equal(
    str_sub( test_strings, NA, NA),
    fstr_sub(test_strings, NA, NA)
  )
})

test_that("NULL inputs", {
  expect_equal(
    str_sub( NULL, start, stop),
    fstr_sub(NULL, start, stop)
  )
  expect_error(
    fstr_sub(test_strings, NULL, stop), "Start vector has zero length"
  )
  expect_error(
    fstr_sub(test_strings, start, NULL), "Stop vector has zero length"
  )
})

test_that("Zero length inputs", {
  expect_equal(
    substr( c(), start, stop),
    fstr_sub(c(), start, stop)
  )
  expect_equal(
    substr( character(), start, stop),
    fstr_sub(character(), start, stop)
  )
  expect_error(
    fstr_sub(test_strings, c(), stop), "Start vector has zero length"
  )
  expect_error(
    fstr_sub(test_strings, start, c()), "Stop vector has zero length"
  )
})

test_that("Empty strings", {
  blanks <- rep("", times = n_elts_avoid_sso)
  
  expect_equal(
    str_sub( blanks, start, stop),
    fstr_sub(blanks, start, stop)
  )
})

test_that("Non-integer start and stop", {
  
  start_nonint <- c(2.3, 1.1, 3.8, 3)
  stop_nonint  <- c(2.3, 1.1, 3.8, 3)
  
  expect_equal(
    str_sub( test_strings, start_nonint, stop),
    fstr_sub(test_strings, start_nonint, stop)
  )
  expect_equal(
    str_sub( test_strings, start, stop_nonint),
    fstr_sub(test_strings, start, stop_nonint)
  )
  expect_equal(
    str_sub( test_strings, start_nonint, stop_nonint),
    fstr_sub(test_strings, start_nonint, stop_nonint)
  )
})

test_that("CE_UTF8-encoded strings", {
  
  test_string <- intToUtf8(10001:10030)
  
  # Confirm that the test string is indeed UTF-8 encoded on the system running
  # the test:
  expect_equal( Encoding(test_string), "UTF-8" )
  
  test_strings     <- rep(test_string, times = n_elts_avoid_sso)
  
  len              <- nchar(test_string)
  test_lengths     <- c(-(len+1):(len+1), 2*len)
  start            <- rep(test_lengths, times = length(test_lengths))
  stop             <- rep(test_lengths, each  = length(test_lengths))
  
  expect_equal(
    str_sub( test_strings, start, stop),
    fstr_sub(test_strings, start, stop)
  )
})



# The below expression using substr() and ifelse cases is equivalent to the
# str_sub() behaviour for ASCII strings. It is used in test cases with
# encodings for which str_sub() produces a different result than the result
# that it produces for ASCII strings.
substrlen <- function(strings, start, stop, str_len) {
  substr(strings, 
         ifelse(start < 0, pmax(start + str_len + 1, 1), start), 
         ifelse(stop  < 0, pmax(stop  + str_len + 1, 0), stop)
  )
}

test_that("CE_NATIVE-encoded strings", {
  
  test_string <- validNativeString()
  
  # Confirm that the test string is indeed in native encoding on the system
  # running the test:
  expect_equal(encodingEnum(test_string, 1), 0)
  
  
  # Test against base R's native encoding handling using substr():
  
  start_stop_len   <- max(length(start), length(stop))
  test_strings     <- rep(test_string, times = start_stop_len)
  nchars           <- nchar(test_string)
  
  expect_equal(
    substrlen(test_strings, start, stop, nchars),
    fstr_sub( test_strings, start, stop)
  )
})



test_that("CE_NATIVE-encoded strings in a locale that uses a multibyte encoding 
on Windows", {
  
  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", "Japanese")
  
  test_string <- validWindows932String()
  if (nchar(test_string) <= 2) { 
    skip("Failed to find a valid test string in the native encoding")
  }
  
  # Confirm that the test string is indeed in native encoding on the system
  # running the test:
  expect_equal(encodingEnum(test_string, 1), 0)
  
  
  # Test against base R's native encoding handling using substr():
  
  start_stop_len   <- max(length(start), length(stop))
  test_strings     <- rep(test_string, times = start_stop_len)
  nchars           <- nchar(test_string)
  
  expect_equal(
    substrlen(test_strings, start, stop, nchars),
    fstr_sub( test_strings, start, stop)
  )
  
  # Restore locale
  resetLocale(saved_locale)
})

test_that("CE_NATIVE-encoded strings in a locale that uses a multibyte encoding 
on Windows, where the string is shorter than the lookback", {
  
  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", "Japanese")
  
  test_string <- validWindows932String()
  if (nchar(test_string) <= 2) { 
    skip("Failed to find a valid test string in the native encoding")
  }
  
  test_string <- substr(test_string, 1, 3)  # Test a short string
  
  # Confirm that the test string is indeed in native encoding on the system
  # running the test:
  expect_equal(encodingEnum(test_string, 1), 0)
  
  
  # Test against base R's native encoding handling using substr():
  
  start_stop_len   <- max(length(start), length(stop))
  test_strings     <- rep(test_string, times = start_stop_len)
  nchars           <- nchar(test_string)
  
  expect_equal(
    substrlen(test_strings, start, stop, nchars),
    fstr_sub( test_strings, start, stop)
  )
  
  # Restore locale
  resetLocale(saved_locale)
})

test_that("CE_NATIVE-encoded strings in a locale that uses a multibyte encoding 
on Windows, where the string is longer than the lookback", {
  
  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", "Japanese")
  
  test_string <- validWindows932String()
  if (nchar(test_string) <= 2) { 
    skip("Failed to find a valid test string in the native encoding")
  }
  
  # Test a long string:
  test_len    <- 10000  # This is assumed to be longer than the lookback length
  test_string <- paste(rep(test_string, times= test_len %/% nchar(test_string)),
                       collapse = "")
  
  # Confirm that the test string is indeed in native encoding on the system
  # running the test:
  expect_equal(encodingEnum(test_string, 1), 0)
  
  
  # Test against base R's native encoding handling using substr():
  
  start_stop_len   <- max(length(start), length(stop))
  test_strings     <- rep(test_string, times = start_stop_len)
  nchars           <- nchar(test_string)
  
  expect_equal(
    substrlen(test_strings, start, stop, nchars),
    fstr_sub( test_strings, start, stop)
  )
  
  # Restore locale
  resetLocale(saved_locale)
})

test_that("CE_LATIN1-encoded strings", {
  
  test_string <- "abcde_\xc3\xc4\xc5\xc6\xc7\xc8_abcde"
  Encoding(test_string) <- "latin1"
  
  # Confirm that the test string is indeed in Latin1 encoding on the system
  # running the test:
  expect_equal(Encoding(test_string), "latin1")
  
  test_strings     <- rep(test_string, times = n_elts_avoid_sso)
  
  expect_equal(
    str_sub( test_strings, start, stop),
    fstr_sub(test_strings, start, stop)
  )
})

test_that("CE_BYTES-encoded strings", {
  
  test_string <- "abcde_\xc3\xc4\xc5\xc6\xc7\xc8_abcde"
  n_char <- 18
  Encoding(test_string) <- "bytes"
  
  # Confirm that the test string is indeed in bytes encoding on the system
  # running the test:
  expect_equal(Encoding(test_string), "bytes")
  
  n_rep        <- max(n_elts_avoid_sso, length(start), length(stop))
  test_strings <- rep(test_string, times = n_rep)
  
  expect_equal(  # str_sub() does not support CE_BYTES encoding
    substrlen(test_strings, start, stop, n_char),
    fstr_sub( test_strings, start, stop)
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
  
  n_rep        <- max(n_elts_avoid_sso, length(start), length(stop))
  test_strings <- rep(test_string, times = n_rep)
  
  
  expect_equal(  # str_sub() does not preserve class, so comparison with output
                 # from an equivalent call to substr() is made instead:
    substrlen(test_strings, start, stop, nchar(test_strings)),
    fstr_sub( test_strings, start, stop)
  )
  
  # More readable error message in the case where the class does not match:
  expect_equal(
    class(substrlen(test_strings, start, stop, nchar(test_strings))),
    class(fstr_sub( test_strings, start, stop))
  )
})


if(exists("LOCALE_TO_USE")){
  resetLocale(saved_locale)
}
