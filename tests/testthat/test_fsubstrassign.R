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

source(test_path("../common/fsubstr.R"))

# Set this to true if you want to do a visual check of the output:
SHOW_SAMPLE_OUTPUT <- FALSE

if(exists("LOCALE_TO_USE")){
  Sys.setlocale("LC_ALL", LOCALE_TO_USE)
} else {
  Sys.setlocale("LC_ALL", "English")
}




n_elts_avoid_sso <- 3500  # Input vector must be long enough to avoid
                          # small size optimisations

test_string      <- "abcdefghij"
test_strings     <- rep(test_string, times = n_elts_avoid_sso)

len              <- nchar(test_string)
test_lengths     <- c(-2*len, -(len+5):(len+5), 2*len)
                    # Lengths chosen such that some of them go beyond the ends
                    # of the test string, in order to test such cases

start            <- rep(test_lengths, times = length(test_lengths))
stop             <- rep(test_lengths, each  = length(test_lengths))

test_strings_2   <- test_strings
rep_string       <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


# Make sure length(start) is significantly higher than n_elts_avoid_sso:
start <- rep(start, times = ((length(test_strings) + 10) %/% length(start)) + 1)
stop  <- rep(stop,  times = ((length(test_strings) + 10) %/% length(stop )) + 1)


test_that("Combinations of start and stop values", {
  
  substr( test_strings,   start, stop) <- rep_string
  fsubstr(test_strings_2, start, stop) <- rep_string
  
  expect_equal(test_strings, test_strings_2)
})

utf8_string    <- intToUtf8(10001:10030)
utf8_strings   <- rep(utf8_string, times = n_elts_avoid_sso)
utf8_strings_2 <- utf8_strings
rep_utf8       <- intToUtf8(10085:10125)


# The following function correctSubstr() replicates the behaviour of substr()<-
# on ASCII strings for other encodings. It is used:
#
#  1) as a workaround for a bug present in substr()<- in R versions < 4.3.1 that
#     allows substr()<- to overwrite the null terminator in UTF8 strings when 
#     the substring overruns the end of the original string, e.g.
#
#     string1 <- intToUtf8(c(23383, 97, 97, 97))  # "\u5B57aaa"
#     substr(string1, 4, 5) <- "cc"
#     string1  # Should be "\u5B57aac", but is actually "\u5B57aacc"
#
#  2) for native strings where replacing a substring with a string that is 
#     longer than the remaining space in the substring returns a string that is
#     longer than the initial substring, whereas for ASCII strings the
#     replacement is always truncated at the end of the original string, e.g.
#
#     Sys.setlocale("LC_ALL", "Japanese")
#     chars   <- c(152, 64, 152, 65, 152, 66)
#     string1 <- enc2native(rawToChar(as.raw(chars))) # "\u84EE\u9023\u932C"
#     substr(string1, 3, 4) <- "gg"
#     string1    # Should be "\u84EE\u9023g", but is actually "\u84EE\u9023gg"
#
correctSubstr <- function(input_strings, start, stop, rep){
  
  if(length(start)==1) { start <- c(start, start) }
  if(length(stop )==1) { stop  <- c(stop,  stop ) }
  if(length(rep  )==1) { rep   <- c(rep,   rep  ) }
  
  nchars       <- nchar(input_strings)
  len          <- length(input_strings)
  
  # Expand the arguments cyclically to the length of the longest argument:
  start <- start[( (0:(len-1)) %% (length(start)))+1]
  stop  <- stop[ ( (0:(len-1)) %% (length(stop)) )+1]
  rep   <- rep[  ( (0:(len-1)) %% (length(rep))  )+1]
  
  # To match behaviour of substr(), negative stop is treated as end of string:
  stop  <- ifelse(stop < 0, nchars, stop)
  
  start <- ifelse(start < 0, 1, start)
  stop  <- pmax(pmin(stop,  nchars), start-1)
  
  substr(input_strings, start, stop) <- rep
  
  return(input_strings)
}

test_that("The correction function for substr() used in this test script", {
  utf8_string <- intToUtf8(10001:10030)
  test_string <- intToUtf8(97:106)
  expect_equal(
    correctSubstr(utf8_string, 2, -20, test_string),
    intToUtf8(c(10001, 97:106, 10012:10030))
    )
  expect_equal(
    correctSubstr(utf8_string, 6, 6, test_string),
    intToUtf8(c(10001:10005, 97, 10007:10030))
  )
  expect_equal(
    correctSubstr(test_string, 11, -20, rep_utf8),
    test_string
  )
  temp <- test_string
  substr(temp, 0, 5) <- rep_string
  expect_equal(
    correctSubstr(test_string, 0, 5, rep_string),
    temp
  )

  
  string1 <- intToUtf8(c(23383, 97, 97, 97))  # "\u5B57aaa"
  expect_equal(
    correctSubstr(string1, 4, 5, "cc"),
    intToUtf8(c(23383, 97, 97, 99))           # "\u5B57aac"
  )
  
  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", "Japanese")
  
  intToChar <- function(ints) { rawToChar(as.raw(ints)) }
  chars   <- c(152, 64, 152, 65, 152, 66, 152, 67)  # "\u84EE\u9023\u932C\u5442"
  string1 <- enc2native(intToChar(chars))
  expect_equal(
    correctSubstr(string1, 4, 5, "gg"),
    intToChar(c(152, 64, 152, 65, 152, 66, 103))     # "\u84EE\u9023\u932Cg"
  )
  
  resetLocale(saved_locale)
})

test_that("Cases where replacement string overruns end of original string", {
  
  string1 <- intToUtf8(c(23383, 97, 97, 97))  # "\u5B57aaa"
  
  fsubstr(string1, 4, 5) <- "cc"
  expect_equal(
    string1,
    intToUtf8(c(23383, 97, 97, 99))           # "\u5B57aac"
  )
  
  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", "Japanese")
  
  intToChar <- function(ints) { rawToChar(as.raw(ints)) }
  chars   <- c(152, 64, 152, 65, 152, 66, 152, 67)  # "\u84EE\u9023\u932C\u5442"
  string1 <- enc2native(intToChar(chars))
  
  fsubstr(string1, 4, 5) <- "gg"
  expect_equal(
    string1,
    intToChar(c(152, 64, 152, 65, 152, 66, 103))     # "\u84EE\u9023\u932Cg"
  )
  
  resetLocale(saved_locale)
})


test_that("Combinations of start and stop values with UTF-8 strings", {
  
  # Confirm that the test string is indeed UTF-8 encoded on the system running
  # the test:
  expect_equal( Encoding(utf8_string), "UTF-8" )
  expect_equal( Encoding(rep_utf8), "UTF-8" )
  
  utf8_strings <- correctSubstr(utf8_strings, start, stop, rep_utf8)
  fsubstr(utf8_strings_2, start, stop) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})

test_that(paste0("Combinations of start and stop values with ", 
                 "UTF-8 string and ASCII replacement"), {
  
  utf8_strings <- correctSubstr(utf8_strings, start, stop, test_string)
  fsubstr(utf8_strings_2, start, stop) <- test_string
  
  expect_equal(utf8_strings, utf8_strings_2)
})

testTranslatedUtf8Replacement <- function(str, start, stop, rep) {
  result <- str
  fsubstr(result, start, stop) <- rep
  
  if(length(start)==1) { start <- c(start, start) }
  if(length(stop )==1) { stop  <- c(stop,  stop ) }
  if(length(rep  )==1) { rep   <- c(rep,   rep  ) }
  
  nchars       <- nchar(str)
  len          <- length(str)
  
  # Expand the arguments cyclically to the length of the longest argument:
  start <- start[( (0:(len-1)) %% (length(start)))+1]
  stop  <- stop[ ( (0:(len-1)) %% (length(stop)) )+1]
  rep   <- rep[  ( (0:(len-1)) %% (length(rep))  )+1]
  
  nchars_rep   <- nchar(rep)
  
  start <- ifelse(start <= 0, 1, start)
  stop  <- ifelse(stop < 0, nchars, stop)
  stop  <- pmax(pmin(stop,  nchars), start-1)
  
  # Check the unchanged characters still match:
  expect_equal(substr(str, 0, start-1),     substr(result, 0,start-1))
  expect_equal(substr(str, stop+1, nchars), substr(result, stop+1, nchars))
  
  # Check the replaced characters:
  expect_equal(
    substr(rep, 1, stop-start+1),                          # Portion of the replacement
    substr(result, start, pmin(stop, start+nchars_rep-1))  # Replacement in-place
  )
  if(SHOW_SAMPLE_OUTPUT) {
    print(result[100:199])
    cat('\n')
  }
}

test_that(paste0("Combinations of start and stop values with ", 
                 "ASCII string and UTF-8 replacement"), {
 
  testTranslatedUtf8Replacement(test_strings, start, stop, rep_utf8)
})


test_that("Replacement with NA", {
  
  expect_error(substr(utf8_strings, start, stop) <- NA, "invalid value")
  expect_error(fsubstr(utf8_string,  start, stop) <- NA, 
               "Invalid replacement string")
  
  utf8_strings <- correctSubstr(utf8_strings, start, stop, c(rep_utf8, NA))
  fsubstr(utf8_strings_2, start, stop) <- c(rep_utf8, NA)
  
  expect_equal(utf8_strings, utf8_strings_2)
})


test_that("Stop == 0 gives original string", {
  
  fsubstr(utf8_strings_2, -41:41, 0) <- rep_utf8
  
  expect_equal(
    rep(utf8_string, times = n_elts_avoid_sso),
    utf8_strings_2
  )
})

test_that( "Missing arg produces an error", {
  expect_error(
    fsubstr(does_not_exist, start, stop) <- rep_utf8,
    "object 'does_not_exist' not found"
  )
  expect_error(
    fsubstr(utf8_string, does_not_exist, stop) <- rep_utf8, 
    "object 'does_not_exist' not found"
  )
  expect_error(
    fsubstr(utf8_string, start, does_not_exist) <- rep_utf8, 
    "object 'does_not_exist' not found"
  )
  expect_error(
    fsubstr(utf8_string, start, stop) <- does_not_exist, 
    "object 'does_not_exist' not found"
  )
})

test_that("Length-one string input", {
  alpha <- c("Alpha")
  
  testTranslatedUtf8Replacement(alpha, start, stop, rep_utf8)
})
test_that("Length-one start value", {
  utf8_strings <- correctSubstr(utf8_strings, 3, stop, rep_utf8)
  fsubstr(utf8_strings_2, 3, stop) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("Length-one stop value", {
  utf8_strings <- correctSubstr(utf8_strings, start, 5, rep_utf8)
  fsubstr(utf8_strings_2, start, 5) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("Length-one start and stop values", {
  utf8_strings <- correctSubstr(utf8_strings, 3, 5, rep_utf8)
  fsubstr(utf8_strings_2, 3, 5) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("Length-one replacement string vector", {
  utf8_strings <- correctSubstr(utf8_strings, start, stop, c("Beta"))
  fsubstr(utf8_strings_2, start, stop) <- c("Beta")
  
  expect_equal(utf8_strings, utf8_strings_2)
})


test_that("NA string", {
  expect_error(
    fsubstr(NA, start, stop) <- rep_utf8,
    "target of assignment expands to non-language object"
  )
  expect_error(
    fsubstr(rep(NA, times=n_elts_avoid_sso), start, stop) <- rep_utf8,
    "target of assignment expands to non-language object"
  )
  

  substrings <- correctSubstr(utf8_strings, start, stop, rep_utf8)
  
  mixed_na_and_strings <- rep(NA, times = 2*length(substrings))
  n   <- length(mixed_na_and_strings)/2
  idx <- (1:n)*2
  mixed_na_and_strings[idx] <- substrings
  
  
  start_rep <- rep(start, each=2)
  stop_rep  <- rep(stop,  each=2)
  
  mixed_na_and_strings_2 <- rep(c(NA, utf8_string), times=n_elts_avoid_sso)
  fsubstr(mixed_na_and_strings_2, start_rep, stop_rep) <- rep_utf8

  expect_equal(mixed_na_and_strings, mixed_na_and_strings_2)
})


# NA inputs:

test_that("NA start and stop", {
  utf8_strings <- correctSubstr(utf8_strings, c(NA, 1), stop, rep_utf8)
  fsubstr(utf8_strings_2, c(NA, 1), stop) <- rep_utf8

  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("NA start and stop", {
  utf8_strings <- correctSubstr(utf8_strings, c(NA, NA), stop, rep_utf8)
  fsubstr(utf8_strings_2, c(NA, NA), stop) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("NA start and stop", {
  utf8_strings <- correctSubstr(utf8_strings, start, c(NA, 1), rep_utf8)
  fsubstr(utf8_strings_2, start, c(NA, 1)) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("NA start and stop", {
  utf8_strings <- correctSubstr(utf8_strings, start, c(NA, NA), rep_utf8)
  fsubstr(utf8_strings_2, start, c(NA, NA)) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("NA start and stop", {
  utf8_strings <- correctSubstr(utf8_strings, c(NA, 1), c(NA, 1, NA), rep_utf8)
  fsubstr(utf8_strings_2, c(NA, 1), c(NA, 1, NA)) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("NA start and stop", {
  utf8_strings <- correctSubstr(utf8_strings, NA, stop, rep_utf8)
  fsubstr(utf8_strings_2, NA, stop) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("NA start and stop", {
  utf8_strings <- correctSubstr(utf8_strings, start, NA, rep_utf8)
  fsubstr(utf8_strings_2, start, NA) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})
test_that("NA start and stop", {
  utf8_strings <- correctSubstr(utf8_strings, NA, NA, rep_utf8)
  fsubstr(utf8_strings_2, NA, NA) <- rep_utf8
  
  expect_equal(utf8_strings, utf8_strings_2)
})


test_that("NULL inputs", {
  expect_error(
    fsubstr(NULL, start, stop) <- rep_utf8, "left side of assignment"
  )
  null_var <- NULL
  expect_error(
    fsubstr(null_var, start, stop) <- rep_utf8,
    "Left-hand side of assignment is a non-character object"
  )
  
  expect_error(
    fsubstr(utf8_strings, NULL, stop) <- rep_utf8, 
    "Start vector has zero length"
  )
  expect_error(
    fsubstr(utf8_strings, start, NULL) <- rep_utf8, 
    "Stop vector has zero length"
  )
})


test_that("Zero length inputs", {
  empty <- c()
  expect_error(
    fsubstr(empty, start, stop) <- rep_utf8,
    "Left-hand side of assignment is a non-character object"
  )
  empty <- character()
  expect_equal(
    substr( empty, start, stop) <- rep_utf8,
    fsubstr(empty, start, stop) <- rep_utf8
  )
  expect_error(
    fsubstr(utf8_strings, c(), stop) <- rep_utf8, "Start vector has zero length"
  )
  expect_error(
    fsubstr(utf8_strings, start, c()) <- rep_utf8, "Stop vector has zero length"
  )
})


# Empty strings:

blanks   <- rep("", times = n_elts_avoid_sso)
blanks_2 <- blanks

test_that("Empty strings", {
  substr( blanks, start, stop)   <- rep_utf8
  fsubstr(blanks_2, start, stop) <- rep_utf8
  
  expect_equal(blanks, blanks_2)
})
test_that("Empty replacement strings", {
  substr( utf8_strings, start, stop) <- blanks
  fsubstr(utf8_strings, start, stop) <- blanks
  
  expect_equal(blanks, blanks_2)
})

rm(blanks, blanks_2)


# Non-integer start and stop:

start_nonint <- c(2.3, 1.1, 3.8, 3)
stop_nonint  <- c(2.3, 1.1, 3.8, 3)

if (package_version(R.Version()) >= "4.3.1") {
  test_that("Non-integer start", {
    substr( utf8_strings,   start_nonint, stop) <- rep_utf8
    fsubstr(utf8_strings_2, start_nonint, stop) <- rep_utf8
    
    expect_equal(utf8_strings, utf8_strings_2)
  })
  test_that("Non-integer stop", {
    substr( utf8_strings,   start, stop_nonint) <- rep_utf8
    fsubstr(utf8_strings_2, start, stop_nonint) <- rep_utf8
    
    expect_equal(utf8_strings, utf8_strings_2)
  })
  test_that("Non-integer start and stop", {
    substr( utf8_strings,   start_nonint, stop_nonint) <- rep_utf8
    fsubstr(utf8_strings_2, start_nonint, stop_nonint) <- rep_utf8
    
    expect_equal(utf8_strings, utf8_strings_2)
  })
} else {
  test_that("Non-integer start", {
    utf8_strings <- correctSubstr(utf8_strings, start_nonint, stop, rep_utf8)
    fsubstr(utf8_strings_2, start_nonint, stop) <- rep_utf8
    
    expect_equal(utf8_strings, utf8_strings_2)
  })
  test_that("Non-integer stop", {
    utf8_strings <- correctSubstr(utf8_strings, start, stop_nonint, rep_utf8)
    fsubstr(utf8_strings_2, start, stop_nonint) <- rep_utf8
    
    expect_equal(utf8_strings, utf8_strings_2)
  })
  test_that("Non-integer start and stop", {
    utf8_strings <- correctSubstr(utf8_strings, start_nonint, stop_nonint, rep_utf8)
    fsubstr(utf8_strings_2, start_nonint, stop_nonint) <- rep_utf8
    
    expect_equal(utf8_strings, utf8_strings_2)
  })
}


# Strings of various encodings:
# CE_NATIVE:
native_string <- validNativeString()
expect_equal(encodingEnum(native_string, 1), 0)
# CE_LATIN1:
latin1_string           <- paste0("abcde_\xc3\xc4\xc5\xc6\xc7\xc8_abcde")
Encoding(latin1_string) <- "latin1"
if (substr(getLocale()["LC_COLLATE"],1,7)=="English"){
  expect_equal(Encoding(latin1_string), "latin1")
}
# CE_BYTES:
bytes_string            <- paste0("abcde_\xc3\xc4\xc5\xc6\xc7\xc8_abcde")
Encoding(bytes_string)  <- "bytes"
expect_equal(Encoding(bytes_string), "bytes")

# From the R help entry for Encoding(): 'Strings marked as "bytes" are intended 
# to be non-ASCII strings which should be manipulated as bytes, and never 
# converted to a character encoding'

enc_tests <- data.frame(
  enc = c("ASCII",      "CE_UTF8",   "CE_NATIVE",   "CE_LATIN1",   "CE_BYTES"),
  str = c(test_string,  utf8_string, native_string, latin1_string, bytes_string)
)


testSpecificEncodings <- function(enc1, str1, enc2, str2) {
  
  if(   (enc1 == "CE_BYTES" && !(enc2 %in% c("ASCII", "CE_BYTES"))) 
     || (enc2 == "CE_BYTES" && enc1 != "CE_BYTES")) {
    
    # If the two strings have different encodings, they are both translated into
    # the native encoding, but translation is prohibited for 'bytes' strings, so
    # if only one string has 'bytes' encoding, then an error should be thrown.
    
    test_strings   <- rep(str1, times = n_elts_avoid_sso)
    
    expect_error(
      fsubstr(test_strings, start, stop) <- str2,
      "translating strings with \"bytes\" encoding is not allowed")
    
  } else if ((enc1 != enc2) && (enc2 != "ASCII") && (enc1 == "CE_UTF8" || enc2 == "CE_UTF8")) {
    
    test_strings   <- rep(str1, times = n_elts_avoid_sso)
    
    testTranslatedUtf8Replacement(test_strings, start, stop, str2)

  } else {
  
    test_that(
      paste0(enc1, "-encoded strings with ", enc2,
             "-encoded replacement strings"), {
        
        test_strings   <- rep(str1, times = n_elts_avoid_sso)
        test_strings_2 <- test_strings
        
        if (   (   package_version(R.Version()) < "4.3.1"
                && enc1 == "CE_UTF8" && enc2 == "CE_UTF8")
            || (substr(enc1,1,9) == "CE_NATIVE")) {
          # substr(,1,9) is used because in one case the charset is added to the
          # CE_NATIVE string as a suffix.

          test_strings <- correctSubstr(test_strings, start, stop, str2)
          
        } else {
          substr( test_strings, start, stop) <- str2
        }

        fsubstr(test_strings_2, start, stop) <- str2
        
        expect_equal(test_strings, test_strings_2)
        
        if (SHOW_SAMPLE_OUTPUT) {
          print(test_strings_2[100:199])
          cat('\n')
        }
      })
  }
}

perms <- expand.grid(rep(list(1:nrow(enc_tests)), 2))

testEncPerms <- function(enc_tests, perms) {
  for(i in 1:nrow(perms)) {
    x   <- enc_tests[perms[i,1], ]
    rep <- enc_tests[perms[i,2], ]
    
    testSpecificEncodings(x$enc, x$str, rep$enc, rep$str)
  }
}


testEncPerms(enc_tests, perms)


test_that("Strings are as expected after replacment", {
  
  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", "Japanese")
  if(Sys.getlocale("LC_COLLATE") != "Japanese_Japan.932") {
    
    warning(paste0("Cannot test whether Windows 932 (Japanese) strings ",
                   "are correct after replacement"))
  } else {
    
    test_string   <- rawToChar(as.raw(97:106))
    native_string <- rawToChar(as.raw(153:222))
    
    fsubstr(test_string, 2, 5) <- native_string
    expect_equal(charToRaw(test_string), as.raw(c(97, 153:160, 102:106)))
    if(SHOW_SAMPLE_OUTPUT) { print(test_string) }
    
    test_string   <- rawToChar(as.raw(97:106))
    native_string <- rawToChar(as.raw(153:222))
    
    fsubstr(native_string, 2, 5) <- test_string
    expect_equal(charToRaw(native_string), as.raw(c(153:154, 97:100, 162:222)))
    if(SHOW_SAMPLE_OUTPUT) { print(native_string) }
    
    native_string <- rawToChar(as.raw(153:222))
    utf8_string   <- intToUtf8(10001:10030)
    
    fsubstr(native_string, 2, 5) <- utf8_string
    expect_equal(utf8ToInt(native_string), c(21138, 10001:10004, 65378:65438))
    if(SHOW_SAMPLE_OUTPUT) { print(native_string) }
    
    native_string <- rawToChar(as.raw(153:222))
    utf8_string   <- intToUtf8(10001:10030)
    
    fsubstr(utf8_string, 2, 5) <- native_string
    expect_equal(utf8ToInt(utf8_string), c(10001, 21138, 23622, 25788, 27867, 10006:10030))
    if(SHOW_SAMPLE_OUTPUT) { print(utf8_string) }
  }
    
  resetLocale(saved_locale)
})


# Test that string lengths are the same after substring replacement:
for(i in 1:nrow(perms)) {
  enc1 <- enc_tests[perms[i,1], ]$enc
  enc2 <- enc_tests[perms[i,2], ]$enc
  
  if(   (enc1 == "CE_BYTES" && !(enc2 %in% c("ASCII", "CE_BYTES"))) 
        || (enc2 == "CE_BYTES" && enc1 != "CE_BYTES")) {
    # Replacement not possible - translating byte strings is not allowed
  } else {
    test_that(
      paste0(enc1, " string is the correct length after ", enc2, 
             " substring replacement"), {
      
      str <- rep(enc_tests[perms[i,1], ]$str, times = n_elts_avoid_sso)
      rep <- rep(enc_tests[perms[i,2], ]$str, times = n_elts_avoid_sso)
      
      str_is_bytes <- (enc1 == "CE_BYTES")
      
      original_len <- ifelse(str_is_bytes, nchar(str, "bytes"), nchar(str))
      fsubstr(str, start, stop) <- rep
      
      expect_equal(
        ifelse(str_is_bytes, nchar(str, "bytes"), nchar(str)), 
        original_len
      )
      
    })
  }
}


test_that("Native strings under Japanese locale", {
  
  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", "Japanese")
  
  native_string <- validNativeString()
  rep           <- validNativeString(190:199)
  
  start <- c(3, 1, -10)
  stop  <- c(5, 3,  -5)

  test_strings   <- rep(native_string, times = n_elts_avoid_sso)
  test_strings_2 <- test_strings
  
  substr( test_strings,   start, stop) <- rep
  fsubstr(test_strings_2, start, stop) <- rep
  
  expect_equal(test_strings, test_strings_2)
  
  resetLocale(saved_locale)
})

test_that("UTF-8 strings with native replacement under Japanese locale", {
  
  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", "Japanese")
  
  utf8_string   <- intToUtf8(10001:10030)
  native_string <- validNativeString()
  
  utf8_strings   <- rep(utf8_string,   times = n_elts_avoid_sso)
  native_strings <- rep(native_string, times = n_elts_avoid_sso)

  testTranslatedUtf8Replacement(utf8_strings, start, stop, native_strings)
  
  resetLocale(saved_locale)
})



test_that("CE_NATIVE-encoded strings in a locale that uses a multibyte encoding
on Windows", {

  saved_locale <- getLocale()
  Sys.setlocale("LC_ALL", "Japanese")
  
  windows_932_string <- validWindows932String()
  
  native_perms <- perms[perms[ ,2] == which(enc_tests$enc == "CE_NATIVE"), ]
  enc_tests_2 <- enc_tests
  enc_tests_2[enc_tests_2$enc == "CE_NATIVE", "str"] <- windows_932_string
  enc_tests_2[enc_tests_2$enc == "CE_NATIVE", "enc"] <- "CE_NATIVE Windows-932"
  
  testEncPerms(enc_tests_2, native_perms)
  
  resetLocale(saved_locale)
})


# CE_ANY and CE_SYMBOL appear to be internal parameter values that it is not
# necessary to test.
#
# Attempting to save a string with encoding CE_ANY or CE_SYMBOL using mkCharCE()
# produces the error 'unknown encoding mask' (unless the string already exists 
# in R's string hash table, in which case no new entry is added to the hash 
# table, and the encoding specifier of the existing entry is not modified).


test_that("Character vectors with different elements in different encodings", {
  
  mixed_strs <- c(test_string, utf8_string, native_string, latin1_string)
  mixed_encs <- c("ASCII",     "CE_UTF8",   "CE_NATIVE",   "CE_LATIN1")
  utf8_flag  <- c(FALSE,       TRUE,        FALSE,         FALSE)
  
  n <- length(start)
  m <- length(mixed_strs)
  test_strings <- rep(mixed_strs, times = n)
  rep_strings  <- rep(mixed_strs, each  = n)
  start <- rep(start, each = m)
  stop  <- rep(stop,  each = m)
  
  
  
  utf8_pairs  <- xor(rep(utf8_flag, times = n), rep(utf8_flag, each  = n))
  other_pairs <- !utf8_pairs
  
  # UTF8 cases:
  testTranslatedUtf8Replacement(test_strings[utf8_pairs], start[utf8_pairs], 
                                stop[utf8_pairs], rep_strings[utf8_pairs])
  
  # Other cases:
  test_strings_2 <- test_strings
  
  test_strings <- correctSubstr(test_strings, start, stop, rep_strings)
  fsubstr(test_strings_2, start, stop) <- rep_strings
  
  expect_equal(test_strings[!utf8_pairs], test_strings_2[!utf8_pairs])
  
})


test_that("SHALLOW_DUPLICATE_ATTRIB preserves the class of the input", {

  class(utf8_strings_2) <- "my_example_class"

  fsubstr(utf8_strings_2, start, stop) <- rep_utf8
  
  # class(utf8_strings) is changed to "character" by substr()<-, so instead we 
  # test directly against "my_example_class":
  expect_equal(class(utf8_strings_2), "my_example_class")
})
