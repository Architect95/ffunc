# ffunc : Fast utility functions for R, implemented in C
# Copyright (C) 2023 Architect95
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



# encodingEnum() makes the return value of getCharCE available in R:
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


validNativeString <- function(char_nums = 120:255) {
  chars         <- as.raw(char_nums)
  native_string <- enc2native(rawToChar(chars))
  
  while(!validEnc(native_string) && length(chars) > 2) {
    chars         <- chars[2:(length(chars)-1)]
    native_string <- enc2native(rawToChar(chars))
  }
  expect_equal(encodingEnum(native_string, 1), 0)
  if (length(chars) <= 2) {
    stop("Failed to find a valid string in the native encoding from the char_nums provided")
  }
  if (encodingEnum(native_string, 1) != 0) {
    stop("Failed to encode a string in native encoding on this system")
  }
  return(native_string)
}

validWindows932String <- function() {
  # String of two-byte characters in Windows-932 encoding:
  len                <- 51
  chars              <- rep(apply(as.matrix(c('0x98','0x40')), 1, strtoi), times=len)
  chars[(1:len)*2]   <- chars[(1:len)*2] + 0:(len-1)
  windows_932_string <- enc2native(rawToChar(as.raw(chars)))
  # For the codepage reference, see 
  # https://icu4c-demos.unicode.org/icu-bin/convexp?conv=ibm-943_P15A-2003&b=98&s=ALL#layout
  
  return(windows_932_string)
}


getLocale <- function() {
  locale_categories <- c("LC_COLLATE","LC_CTYPE","LC_MONETARY","LC_NUMERIC",
                         "LC_TIME")
  return(setNames(sapply(locale_categories, Sys.getlocale), locale_categories))
}
resetLocale <- function(saved_locale) {
  sapply(names(saved_locale), function(x){Sys.setlocale(x, saved_locale[[x]]) })
}

