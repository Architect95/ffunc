# ffunc : Fast utility functions for R, implemented in C
# Copyright (C) 2024 Architect95
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

source("tests/common/fsubstr.R")

benchmark_length <- 100000


# ASCII:

test_string      <- "abcdefghijklmnopqrstuvwxyz"
rep_string       <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
test_strings     <- rep(test_string, times = benchmark_length)
test_strings_2   <- test_strings

len              <- nchar(test_string)
test_lengths     <- c(-2*len, -(len+5):(len+5), 2*len)
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

# Make sure length(start) is significantly higher than a number that would
# trigger a small-size optimisation:
start <- rep(start, times = ((length(test_strings) + 10) %/% length(start)) + 1)
stop  <- rep(stop,  times = ((length(test_strings) + 10) %/% length(stop )) + 1)

microbenchmark::microbenchmark(
  substr( test_strings,   start, stop) <- rep_string,
  fsubstr(test_strings_2, start, stop) <- rep_string
)


# Unicode:

unicode_string    <- intToUtf8(10001:10030)
rep_unicode       <- intToUtf8(10085:10095)
unicode_strings   <- rep(unicode_string, times = benchmark_length)
unicode_strings_2 <- unicode_strings

len              <- nchar(rep_unicode)
test_lengths     <- c(-2*len, -(len+5):(len))
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

start <- rep(start, times = ((length(unicode_string) + 10) %/% length(start))+1)
stop  <- rep(stop,  times = ((length(unicode_string) + 10) %/% length(stop ))+1)

microbenchmark::microbenchmark(
  substr( unicode_strings,   start, stop) <- rep_unicode,
  fsubstr(unicode_strings_2, start, stop) <- rep_unicode
)


# Native multibyte:

saved_locale <- getLocale()
Sys.setlocale("LC_ALL", "Japanese")

rep_string  <- validNativeString(190:199)

# 1) Generic case:

test_string      <- substr(validWindows932String(), 1, 20)

test_strings     <- rep(test_string, times = benchmark_length)
test_strings_2   <- test_strings

len              <- nchar(test_string)
test_lengths     <- c(-2*len, -(len+5):(len+5), 2*len)
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

start <- rep(start, times = ((length(test_strings) + 10) %/% length(start)) + 1)
stop  <- rep(stop,  times = ((length(test_strings) + 10) %/% length(stop )) + 1)

microbenchmark::microbenchmark(
  substr( test_strings,   start, stop) <- rep_string,
  fsubstr(test_strings_2, start, stop) <- rep_string
)

# 2) Case where strings are shorter than the lookback:

test_string      <- substr(validWindows932String(), 1, 20)

test_string      <- substr(test_string, 1, 3)

test_strings     <- rep(test_string, times = benchmark_length)
test_strings_2   <- test_strings

len              <- nchar(test_string)
test_lengths     <- c(-2*len, -(len+5):(len+5), 2*len)
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

start <- rep(start, times = ((length(test_strings) + 10) %/% length(start)) + 1)
stop  <- rep(stop,  times = ((length(test_strings) + 10) %/% length(stop )) + 1)

microbenchmark::microbenchmark(
  substr( test_strings,   start, stop) <- rep_string,
  fsubstr(test_strings_2, start, stop) <- rep_string
)

# 3) Case where strings are longer than the lookback:

test_string      <- substr(validWindows932String(), 1, 20)

test_len         <- 1000  # This is assumed to be longer than the lookback length
test_string      <- paste(rep(test_string, times= test_len %/% nchar(test_string)),
                     collapse = "")

test_strings     <- rep(test_string, times = benchmark_length/100)
test_strings_2   <- test_strings

len              <- nchar(test_string)
test_lengths     <- c(-2*len, -(len+5):(len+5), 2*len)
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

start <- rep(start, times = ((length(test_strings) + 10) %/% length(start)) + 1)
stop  <- rep(stop,  times = ((length(test_strings) + 10) %/% length(stop )) + 1)

microbenchmark::microbenchmark(
  substr( test_strings,   start, stop) <- rep_string,
  fsubstr(test_strings_2, start, stop) <- rep_string
)

# Restore locale
resetLocale(saved_locale)