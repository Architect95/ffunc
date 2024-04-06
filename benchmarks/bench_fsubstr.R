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
test_strings     <- rep(test_string, times = benchmark_length)

len              <- nchar(test_string)
test_lengths     <- c(-2:(len+1), 2*len)
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

microbenchmark::microbenchmark(
  substr( test_strings, start, stop),
  fsubstr(test_strings, start, stop)
)

# Unicode:

unicode_string  <- intToUtf8(10001:10030)
unicode_strings <- rep(unicode_string, times = benchmark_length)

len          <- nchar(unicode_string)
test_lengths <- c(-2:(len+1), 2*len)
start        <- rep(test_lengths, times = length(test_lengths))
stop         <- rep(test_lengths, each  = length(test_lengths))

microbenchmark::microbenchmark(
  substr( unicode_strings, start, stop),
  fsubstr(unicode_strings, start, stop)
)

# Native multibyte:

saved_locale <- getLocale()
Sys.setlocale("LC_ALL", "Japanese")

# 1) Generic case:

test_string <- validWindows932String()

test_strings     <- rep(test_string, times = benchmark_length)

len              <- nchar(test_string)
test_lengths     <- c(-2:(len+1), 2*len)
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

microbenchmark::microbenchmark(
  substr( test_strings, start, stop),
  fsubstr(test_strings, start, stop)
)

# 2) Case where strings are shorter than the lookback:

test_string <- validWindows932String()

test_string <- substr(test_string, 1, 3)

test_strings     <- rep(test_string, times = benchmark_length)

len              <- nchar(test_string)
test_lengths     <- c(-2:(len+1), 2*len)
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

microbenchmark::microbenchmark(
  substr( test_strings, start, stop),
  fsubstr(test_strings, start, stop)
)

# 3) Case where strings are longer than the lookback:

test_string <- validWindows932String()

test_len    <- 1000  # This is assumed to be longer than the lookback length
test_string <- paste(rep(test_string, times= test_len %/% nchar(test_string)),
                     collapse = "")

test_strings     <- rep(test_string, times = benchmark_length)

len              <- nchar(test_string)
test_lengths     <- c(-2:(len+1), 2*len)
start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

microbenchmark::microbenchmark(
  substr( test_strings, start, stop),
  fsubstr(test_strings, start, stop)
)

# Restore locale
resetLocale(saved_locale)