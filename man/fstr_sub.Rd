% Copyright (C) 2022 Architect95
% Distributed under GPL 3 or later

\name{fstr_sub}
\alias{fstr_sub}
\title{Fast Substrings}

\description{
  Extract substrings from a vector of strings, in which negative start and stop arguments may be used to count backwards from the end of the input strings. Optimised for speed over long vectors.
}

\usage{
  fstr_sub(x, start, stop)
}

\arguments{
  \item{x}{character vector}
  \item{start}{integer}
  \item{stop}{integer}
}

\details{
  \code{fstr_sub} is optimised for speed over large input vectors of strings and start/stop values. On a multi-core processor, It splits the string vector into sub-vectors to be processed on separate cores.
  
  The \code{start} and \code{stop} values are indices of the string characters, where the characters are numbered from one. Both the start and stop characters are included in the substrings. Negative values count back from the end of the strings (e.g. \code{-1} refers to the last character in a string).
  
  On strings in variable-width and stateful character encodings where the string must be traversed from the start in order to determine the position of any given character, the following approach is used for negative start or stop values in order to reduce the execution time. When the string is first traversed, the position (and state if applicable) of the last few characters are stored in an array so that, if the negative start or stop positions are close to the end of the string, they can be retrieved immediately from the array without the need to traverse the string a second time in order to locate them.
  
  If the input vectors \code{x}, \code{start} and \code{stop} are not all of the same length, the elements of the shorter vectors are recycled to the length of the longest of the three vectors.
  
  A \code{start} value of zero will include all characters up to the stop character in the substring; a \code{stop} value of zero results in an empty substring.
}

\value{
  A character vector with the same attributes as \code{x} (after possible coercion), with the length of the longest argument.
}

\examples{
string <- "abcde"

fstr_sub(string, -4, -2)
# "bcd"

fstr_sub(string,  1, -4)
# "ab"

fstr_sub(string,  4,  5)
# "de"
}