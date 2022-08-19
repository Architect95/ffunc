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

library(testthat)
library(stringr)
library(ffunc)


# This test will only run in environments with sufficient memory. It allocates
# a string of maximum length.

test_that("Maximum length string", {

  int_max      <- .Machine$integer.max  # 2^31 -1
  test_string  <- paste0(strrep("abcdefgh", times=round(int_max/8,0)-1), 
                         "abcdefg")
  test_strings <- rep(test_string, times = 2)

  start <- c(10000, 100000)
  stop  <- c(12345, 109999)

  expect_equal(
    substr( test_strings, start, stop),
    fsubstr(test_strings, start, stop)
  )
  
  start <- c(10000, -20)
  stop  <- c(12345, -10)
  
  expect_equal(
    str_sub( test_strings, start, stop),
    fstr_sub(test_strings, start, stop)
  )
})