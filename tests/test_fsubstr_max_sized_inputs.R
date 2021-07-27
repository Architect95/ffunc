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



test_that("Maximum length string", {

  # Not implemented
  int_max <- 2^31
  test_string <- paste0(strrep("abcdefgh", times=(int_max/8)-1), "1234567")
  test_strings <- rep(test_string, times = 2)

  start <- c(10000, 100000)
  stop  <- c(12345, 109999)

  expect_equal(
    substr( test_strings, start, stop),
    fsubstr(test_strings, start, stop)
  )
})