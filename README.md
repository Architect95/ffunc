# ffunc

**ffunc** is an R package containing fast utility functions, implemented in C.


## Installation

Run the following commands in R:
```R
devtools::install_github("Architect95/ffunc", ref="main")

library(ffunc)
```

## Functions

### `fsubstr(x, start, stop)`

Drop-in replacement for `substr()`, optimised for speed over vectors. The majority of the speed gain is due to parallelisation.

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


### `fstr_sub(x, start, stop)`

Another fast substring function, in which negative start and stop arguments are used to count backwards from the end of the input string.

#### Example

```R
> string <- "abcde"
> fstr_sub(string, -4, -2)
[1] "bcd"
> fstr_sub(string,  1, -4)
[1] "ab"
> fstr_sub(string,  4,  5)
[1] "de"
```