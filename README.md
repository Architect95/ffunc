# ffunc

Fast utility functions for R, implemented in C


## Installation

```R
devtools::install_github("Architect95/ffunc", ref="main")

library(ffunc)
```

## Functions

### `fsubstr(x, start, stop)`

Drop-in replacement for `substr()`, optimised for speed. The majority of the speed gain is due to parallelisation.

As with `substr()`, the resulting substrings can be assigned to, using `fsubstr(x, start, stop) <- value`.

#### Example

```R
> string <- "abcdefghij"
> fsubstr(string, 2, 5)
[1] "bcde"
> fsubstr(string, 2, 5) <- "X"
> string
[1] "aXcdefghij"
> fsubstr(string, 2, 5) <- "XXYYZZ"
> string
[1] "aXXYYfghij"
```