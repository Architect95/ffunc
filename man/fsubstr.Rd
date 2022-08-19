% Copyright (C) 2022 Architect95
% Distributed under GPL 3 or later

\name{fsubstr}
\alias{fsubstr}
\alias{fsubstr<-}
\title{Fast Substrings}

\description{
  Extract or replace substrings from a vector of strings. Drop-in replacement for \code{substr()}, optimised for speed over long vectors.
}

\usage{
  fsubstr(x, start, stop)
  fsubstr(x, start, stop) <- value
}

\arguments{
  \item{x}{character vector}
  \item{start}{vector of first character positions in the substrings}
  \item{stop}{vector of last character positions in the substrings}
  \item{value}{character vector of replacement strings}
}

\details{
  \code{fsubstr} is optimised for speed over large input vectors of strings and start/stop values. On a multi-core processor, It splits the string vector into sub-vectors to be processed on separate cores.
  
  The \code{start} and \code{stop} values are indices of the string characters, where the characters are numbered from one. Both the start and stop characters are included in the substrings.
  
  If the \code{start} and/or \code{stop} vector is shorter than the string vector \code{x}, the elements of the \code{start/stop} vectors are recycled to reach the full length of the string vector \code{x}. In such cases, the function emits a warning.
  
  Start values less than one are treated as the start of a string; start values greater than the number of characters in a string result in an empty substring. Likewise, stop values greater than the number of characters in a string result are treated as the end of the string, while stop values less than one result in an empty substring.
  
  To use negative start and stop values as offsets from the end of the strings, use \code{\link{fstr_sub}} instead.
  
  \code{fsubstr()<-} does not use parallel processing in order to avoid exceeding memory constraints.
}

\value{
  A character vector of the same length and with the same attributes as \code{x} (after possible coercion).
}

\seealso{
  \code{\link{substr}}, \code{\link{fstr_sub}}
}

\examples{
string <- "abcdefgh"

fsubstr(string, 2, 5)
# "bcde"

fsubstr(string, 2, 5) <- "_"
string
# "a_cdefgh"

fsubstr(string, 2, 5) <- "KLMNOP"
string
# "aKLMNfgh"
}